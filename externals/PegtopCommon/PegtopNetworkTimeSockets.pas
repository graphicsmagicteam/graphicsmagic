unit PegtopNetworkTimeSockets;

interface

uses
  Windows, Classes, Messages, SysUtils, WinSock, PegtopMessageReceivers;

type
  TNTPTimeStamp = packed record // SNTP time stamp format (since January the 1th 1900)
    case Integer of
      0: (Def: Int64);
      1: (Fraction: Longword; // seconds in network byte order
          Seconds: Longword); // fraction in network byte order
      2: (Bytes: packed array[0..7] of Byte);
  end;

  TNTPMessage = packed record // SNTP message
    Mode: Byte;               // leap indicator, version number, mode
    Stratum: Byte;
    Poll: Byte;
    Precision: Byte;
    RootDelay: Longword;
    RootDispersion: Longint;
    RefID: packed array[0..3] of char;
    Reference: TNTPTimeStamp; // time server was synchronized
    Orginate: TNTPTimeStamp;  // originate timestamp of client
    Receive: TNTPTimeStamp;   // time server received client request
    Transmit: TNTPTimeStamp;  // time server sent reply
  end;

  EPegtopSocketError = class(Exception);

  TPegtopNetworkTimeEvent = procedure(Sender: TObject; SystemUTC, ServerUTC, RoundTrip: TDateTime) of object;
  TPegtopNetworkTimeErrorEvent = procedure(Sender: TObject; Error: EPegtopSocketError) of object;

  TPegtopNetworkTimeSocket = class(TPegtopCustomMessageReceiver)
  private
    FSocket: TSocket;
    FHost: String;
    FAddress: String;
    FAddr: TSockAddrIn;
    FTimeOut: Integer;
    FOriginateTime: TDateTime;
    FAutoSyncSystemTime: Boolean;
    FRecentHost: String;
    FRecentAddress: String;
    FRecentAnswer: TNTPMessage;
    FOnTimeReceived: TPegtopNetworkTimeEvent;
    FOnError: TPegtopNetworkTimeErrorEvent;
    procedure ReceiveTime(const NTPMessage: TNTPMessage);
    procedure DoRaise(Error: EPegtopSocketError);
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RequestTime;
    procedure CancelRequest;
    property RecentHost: String read FRecentHost;
    property RecentAddress: String read FRecentAddress;
    property RecentAnswer: TNTPMessage read FRecentAnswer;
  published
    property Host: String read FHost write FHost;
    property Address: String read FAddress write FAddress;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property AutoSyncSystemTime: Boolean read FAutoSyncSystemTime write FAutoSyncSystemTime;
    property OnTimeReceived: TPegtopNetworkTimeEvent read FOnTimeReceived write FOnTimeReceived;
    property OnError: TPegtopNetworkTimeErrorEvent read FOnError write FOnError; 
  end;

function ReverseNTPByteOrder(V: TNTPTimeStamp): TNTPTimeStamp;
function NTPTimeStampToDateTime(V: TNTPTimeStamp): TDateTime;
  
implementation

uses
  PegtopTimeUtils;

type
  TWMNetworkEvent = packed record
    Msg: Cardinal;
    Socket: TSocket;
    Event: Word;
    Error: Word;
    Result: Longint;
  end;

const
  WM_NETWORK_EVENT = WM_USER + $E;

var
  WSAData: TWSAData;

function ReverseNTPByteOrder(V: TNTPTimeStamp): TNTPTimeStamp;
var
  I: Integer;
begin
  for I := 0 to 7 do Result.Bytes[I] := V.Bytes[7 - I];
end;

function NTPTimeStampToDateTime(V: TNTPTimeStamp): TDateTime;
begin
  // convert to our personal zero 01/01/2000
  // (3155673600 seconds from 01/01/1900 to 01/01/2000):
  V.Seconds := V.Seconds - 3155673600;
  // convert from timestamp to Delphi scale
  // (from fractions of seconds to days):
  Result := V.Def * (1/$100000000/86400);
  // convert to Delphi zero 12/30/1899
  // (36526 days from 12/30/1899 to 01/01/2000):
  Result := Result + 36526.0;
end;

function LookupName(const Name: string): TInAddr;
var
  HostEnt: PHostEnt;
begin
  HostEnt := WinSock.gethostbyname(PChar(Name));
  FillChar(Result, SizeOf(Result), 0);
  if HostEnt <> nil then begin
    Result.S_un_b.s_b1 := HostEnt^.h_addr^[0];
    Result.S_un_b.s_b2 := HostEnt^.h_addr^[1];
    Result.S_un_b.s_b3 := HostEnt^.h_addr^[2];
    Result.S_un_b.s_b4 := HostEnt^.h_addr^[3];
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopNetworkTimeSocket
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopNetworkTimeSocket.Create(AOwner: TComponent);
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode = WSASYSNOTREADY then
    raise EPegtopSocketError.Create('Cannot connect time server because the network subsystem is not ready for communication.')
  else if ErrorCode = WSAVERNOTSUPPORTED then
    raise EPegtopSocketError.Create('Windows Sockets API version 1.1 needed to connect time server.')
  else if ErrorCode = WSAEINVAL then
    raise EPegtopSocketError.Create('Windows Sockets API version 1.1 needed to connect time server.')
  else if ErrorCode <> 0 then
    raise EPegtopSocketError.CreateFmt('Socket could not be initialized (WSAStartup error %d)', [ErrorCode]);
  inherited Create(AOwner);
  FAddr.sin_family := PF_INET;
  FAddr.sin_addr.s_addr := INADDR_ANY;
  FAddr.sin_port := 0;
  FSocket := INVALID_SOCKET;
end;

destructor TPegtopNetworkTimeSocket.Destroy;
begin
  CancelRequest;
  WSACleanup;
  inherited Destroy;
end;

procedure TPegtopNetworkTimeSocket.HandleMessage(var Msg: TMessage; var Handled: Boolean);
var
  NTPMessage: TNTPMessage;
  FromAddr: TSockAddrIn;
  FromSize: Integer;
  R: Integer;
begin
  if Msg.Msg = WM_NETWORK_EVENT then begin
    case TWMNetworkEvent(Msg).Event of
      FD_READ:
        begin
          FromSize := SizeOf(FromAddr);
          R := WinSock.recvfrom(FSocket, NTPMessage, SizeOf(NTPMessage), 0, FromAddr, FromSize);
          if R = SizeOf(NTPMessage) then
            ReceiveTime(NTPMessage)
          else if R = SOCKET_ERROR then
            DoRaise(EPegtopSocketError.CreateFmt('Could not receive data from time server (socket error %d).', [WSAGetLastError]))
          else
            DoRaise(EPegtopSocketError.Create('Data received from time server is corrupted.'));
          CancelRequest;
        end;
    end;
    Handled := True;
  end
  else if Msg.Msg = WM_TIMER then begin
    if FSocket <> INVALID_SOCKET then begin
      KillTimer(Handle, 0);
      CancelRequest;
      DoRaise(EPegtopSocketError.Create('Time server did not answer within time.'));
    end;
  end;
end;

procedure TPegtopNetworkTimeSocket.RequestTime;
var
  NTPMessage: TNTPMessage;
  Addr: TSockAddrIn;
begin
  // cancel recent request (if pending):
  CancelRequest;

  // create socket:
  FSocket := WinSock.socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
  if FSocket = INVALID_SOCKET then
    DoRaise(EPegtopSocketError.CreateFmt('Socket to access time server could not be created (socket error %d).', [WSAGetLastError]));
  WSAAsyncSelect(FSocket, Handle, WM_NETWORK_EVENT, FD_READ);

  // bind socket:
  if WinSock.bind(FSocket, FAddr, SizeOf(FAddr)) = SOCKET_ERROR then
    DoRaise(EPegtopSocketError.CreateFmt('Socket to communicate with time server could not be bound (socket error %d).', [WSAGetLastError]));

  // init address:
  Addr.sin_family := PF_INET;
  Addr.sin_port := WinSock.htons(123);
  if FHost <> '' then Addr.sin_addr := LookupName(FHost)
  else if FAddress <> '' then Addr.sin_addr.S_addr := inet_addr(PChar(Address))
  else DoRaise(EPegtopSocketError.Create('No time server address or host name specified.'));
  FRecentHost := FHost;
  with Addr.sin_addr.S_un_b do
    FRecentAddress := Format('%d.%d.%d.%d', [Byte(s_b1), Byte(s_b2), Byte(s_b3), Byte(s_b4)]);

  // prepare message:
  FillChar(NTPMessage, SizeOf(NTPMessage), 0);
  NTPMessage.Mode := $33; // client

  // send message:
  FOriginateTime := GetUTCDateTime;
  if WinSock.sendto(FSocket, NTPMessage, SizeOf(NTPMessage), 0, Addr, SizeOf(Addr)) = SOCKET_ERROR then
    DoRaise(EPegtopSocketError.CreateFmt('Could not send request to time server (socket error %d).', [WSAGetLastError]));

  // wait for answer (see message handler):
  if FTimeOut > 0 then SetTimer(Handle, 0, FTimeOut, NIL);
end;

procedure TPegtopNetworkTimeSocket.CancelRequest;
begin
  KillTimer(Handle, 0);
  if FSocket <> INVALID_SOCKET then begin
    WinSock.closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

procedure TPegtopNetworkTimeSocket.ReceiveTime(const NTPMessage: TNTPMessage);
var
  NowTime: TDateTime;
  CorrectedTime: TDateTime;
  ReceiveTime: TDateTime;
  TransmitTime: TDateTime;
  RoundTripTimeSpan: Double;
  ErrorTimeSpan: Double;
begin
  NowTime := GetUTCDateTime;
  KillTimer(Handle, 0);
  ReceiveTime := NTPTimeStampToDateTime(ReverseNTPByteOrder(NTPMessage.Receive));
  TransmitTime := NTPTimeStampToDateTime(ReverseNTPByteOrder(NTPMessage.Transmit));
  RoundTripTimeSpan := (NowTime - FOriginateTime) - (ReceiveTime - TransmitTime);
  ErrorTimeSpan := ((ReceiveTime - FOriginateTime) + (TransmitTime - NowTime)) * 0.5;
  CorrectedTime := NowTime + ErrorTimeSpan;
  FRecentAnswer := NTPMessage;
  if FAutoSyncSystemTime then begin
    if not SetUTCDateTime(CorrectedTime) then
      DoRaise(EPegtopSocketError.Create('Could not set system time.'));
  end;
  if Assigned(FOnTimeReceived) then FOnTimeReceived(Self, NowTime, CorrectedTime, RoundTripTimeSpan);
end;

procedure TPegtopNetworkTimeSocket.DoRaise(Error: EPegtopSocketError);
begin
  if Assigned(FOnError) then begin
    FOnError(Self, Error);
    Error.Free;
  end
  else begin
    raise Error;
  end;
end;

end.
