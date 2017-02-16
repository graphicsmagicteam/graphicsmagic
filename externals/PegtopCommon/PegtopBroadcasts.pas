////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopBroadcast
// Version:    1.00
// Date:       23 Sep 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopBroadcast can send messages to other applications (or other instances
// of the same application), that are sharing a TPegtopBroadcast component with
// the same NetworkName. Assign a GUID (as done by default) or your company
// and/or product name to make sure a unique windows message is generated.
// So all TPegtopBroadcast components with the same NetworkName are "stations"
// which send and receive messages on the same "frequency".
////////////////////////////////////////////////////////////////////////////////
// Usage:
// Simply place the component onto your form and assign a unique NetworkName
// (use the same NetworkName for other TPegtopBroadcast components that have
// to communicate with your TPegtopBroadcast component).
// * You get notified (OnStationJoin / OnStationLeave) whenever another
//   TPegtopBroadcast with the same NetworkName is created (and you receive its
//   window handle = StationID).
// * Assign an OnMessage event handler to receive messages from others (don't
//   forget to set Handled to True for all messages you handle).
// * Use PostMessageToAll to post a windows message to all other
//   TPegtopBroadcast components.
// * Use SendMessageToAll to send a windows message to all other
//   TPegtopBroadcast components and receive all results (OnReceiveResult).
// * Use SendDataToAll / SendData to send data to all / any other
//   TPegtopBroadcast component(s). To receive data use OnReceiveRawData,
//   OnReceiveString or OnReceiveStream).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopBroadcasts;

interface

uses
  Windows, Messages, Classes,
  PegtopMessageReceivers;

type
  TPegtopBroadcastEvent = procedure(Sender: TObject; StationID: THandle) of object;
  TPegtopReceiveResultEvent = procedure(Sender: TObject; StationID: THandle; MsgResult: Longint) of object;
  TPegtopReceiveRawDataEvent = procedure(Sender: TObject; StationID: THandle; var Data; Count: Longint; var MsgResult: Longint) of object;
  TPegtopReceiveStringEvent = procedure(Sender: TObject; StationID: THandle; Data: String; var MsgResult: Longint) of object;
  TPegtopReceiveStreamEvent = procedure(Sender: TObject; StationID: THandle; Data: TStream; var MsgResult: Longint) of object;

  TPegtopBroadcastContentType = (ctRawData, ctString, ctStream);

  TPegtopBroadcast = class(TPegtopMessageReceiver)
  private
    FNetworkMessageID: Cardinal;
    FNetworkName: String;
    FWaitCount: Cardinal;
    FStationList: TList;
    FOnStationJoin: TPegtopBroadcastEvent;
    FOnStationLeave: TPegtopBroadcastEvent;
    FOnStationFound: TPegtopBroadcastEvent;
    FOnReceiveResult: TPegtopReceiveResultEvent;
    FOnReceiveRawData: TPegtopReceiveRawDataEvent;
    FOnReceiveString: TPegtopReceiveStringEvent;
    FOnReceiveStream: TPegtopReceiveStreamEvent;
    function CreateUUID: String;
    procedure RegisterNetworkMessage;
    procedure Join;
    procedure Leave;
    procedure SetNetworkName(Value: String);
    function GetStationCount: Integer;
    function GetStationID(Index: Integer): THandle;
    procedure ReceiveData(var Msg: TWMCopyData);
    procedure SendDataToAll(ContentType: TPegtopBroadcastContentType; var Data; Count: Longint); overload;
    function SendData(StationID: THandle; ContentType: TPegtopBroadcastContentType; var Data; Count: Longint): Longint; overload;
  protected
    procedure Loaded; override;
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessageToAll(Msg: Cardinal; WParam, LParam: Longint);
    procedure PostMessageToAll(Msg: Cardinal; WParam, LParam: Longint);
    procedure SendDataToAll(var Data; Count: Longint); overload;
    procedure SendDataToAll(Data: String); overload;
    procedure SendDataToAll(Data: TMemoryStream); overload;
    function SendData(StationID: THandle; var Data; Count: Longint): Longint; overload;
    function SendData(StationID: THandle; Data: String): Longint; overload;
    function SendData(StationID: THandle; Data: TMemoryStream): Longint; overload;
    property StationCount: Integer read GetStationCount;
    property StationID[Index: Integer]: THandle read GetStationID;
    property NetworkMessageID: Cardinal read FNetworkMessageID;
  published
    property Handle;
    property NetworkName: String read FNetworkName write SetNetworkName;
    property OnStationJoin: TPegtopBroadcastEvent read FOnStationJoin write FOnStationJoin;
    property OnStationLeave: TPegtopBroadcastEvent read FOnStationLeave write FOnStationLeave;
    property OnStationFound: TPegtopBroadcastEvent read FOnStationFound write FOnStationFound;
    property OnReceiveResult: TPegtopReceiveResultEvent read FOnReceiveResult write FOnReceiveResult;
    property OnReceiveRawData: TPegtopReceiveRawDataEvent read FOnReceiveRawData write FOnReceiveRawData;
    property OnReceiveString: TPegtopReceiveStringEvent read FOnReceiveString write FOnReceiveString;
    property OnReceiveStream: TPegtopReceiveStreamEvent read FOnReceiveStream write FOnReceiveStream;
  end;

implementation

uses
  PegtopSysUtils;

constructor TPegtopBroadcast.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then
    FNetworkName := CreateUUID
  else
    FNetworkName := '';
  FNetworkMessageID := 0;
  FStationList := TList.Create;
  FWaitCount := 0;
end;

destructor TPegtopBroadcast.Destroy;
begin
  Leave;
  FStationList.Free;
  inherited;
end;

procedure TPegtopBroadcast.Loaded;
begin
  inherited;
  RegisterNetworkMessage;
  Join;
end;

procedure TPegtopBroadcast.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = FNetworkMessageID then begin
    case Msg.WParam of
      1: begin // "hello" from another station
        if THandle(Msg.LParam) <> Handle then begin // ignore echo from myself
          if FStationList.IndexOf(Pointer(Msg.LParam)) = -1 then
            FStationList.Add(Pointer(Msg.LParam));
          Inc(Msg.Result); // tell sender to wait for one more (our) reply
          Windows.PostMessage(Msg.LParam, FNetworkMessageID, 2, Handle); // reply
          if Assigned(FOnStationJoin) then FOnStationJoin(Self, Msg.LParam);
        end;
      end;
      2: begin // reply to my "hello"
        if FStationList.IndexOf(Pointer(Msg.LParam)) = -1 then
          FStationList.Add(Pointer(Msg.LParam));
        if FWaitCount > 0 then Dec(FWaitCount);
        if Assigned(FOnStationFound) then FOnStationFound(Self, Msg.LParam);
      end;
      3: begin // "goodbye" from another station
        FStationList.Remove(Pointer(Msg.LParam));
        if Assigned(FOnStationLeave) then FOnStationLeave(Self, Msg.LParam);
      end;
    end;
    Handled := True;
  end
  else if Msg.Msg = WM_COPYDATA then begin
    ReceiveData(TWMCopyData(Msg));
    Handled := True;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopBroadcast.ReceiveData(var Msg: TWMCopyData);
var
  DataString: String;
  DataStream: TMemoryStream;
begin
  case Msg.CopyDataStruct^.dwData of
    Ord(ctRawData): if Assigned(FOnReceiveRawData) then begin
      FOnReceiveRawData(Self, Msg.From, Msg.CopyDataStruct^.lpData^,
        Msg.CopyDataStruct^.cbData, Msg.Result);
    end;
    Ord(ctString): if Assigned(FOnReceiveString) then begin
      SetLength(DataString, Msg.CopyDataStruct^.cbData);
      Move(Msg.CopyDataStruct^.lpData^, DataString[1], Msg.CopyDataStruct^.cbData);
      FOnReceiveString(Self, Msg.From, DataString, Msg.Result);
    end;
    Ord(ctStream): if Assigned(FOnReceiveStream) then begin
      DataStream := TMemoryStream.Create;
      try
        DataStream.Write(Msg.CopyDataStruct^.lpdata^, Msg.CopyDataStruct^.cbData);
        DataStream.Position := 0;
        FOnReceiveStream(Self, Msg.From, DataStream, Msg.Result);
      finally
        DataStream.Free;
      end;
    end;
  end;
end;

procedure TPegtopBroadcast.SendMessageToAll(Msg: Cardinal; WParam, LParam: Longint);
var
  I: Integer;
  Receiver: THandle;
  MsgResult: Longint;
begin
  for I := 0 to FStationList.Count-1 do begin
    Receiver := THandle(FStationList[I]);
    MsgResult := Windows.SendMessage(Receiver, Msg, WParam, LParam);
    if Assigned(FOnReceiveResult) then FOnReceiveResult(Self, Receiver, MsgResult);
  end;
end;

procedure TPegtopBroadcast.PostMessageToAll(Msg: Cardinal; WParam, LParam: Longint);
var
  I: Integer;
  Receiver: THandle;
begin
  for I := 0 to FStationList.Count-1 do begin
    Receiver := THandle(FStationList[I]);
    Windows.PostMessage(Receiver, Msg, WParam, LParam);
  end;
end;

procedure TPegtopBroadcast.SendDataToAll(ContentType: TPegtopBroadcastContentType; var Data; Count: Longint);
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := Ord(ContentType);
  CopyDataStruct.cbData := Count;
  CopyDataStruct.lpData := @Data;
  SendMessageToAll(WM_COPYDATA, Handle, Longint(@CopyDataStruct))
end;

procedure TPegtopBroadcast.SendDataToAll(var Data; Count: Longint);
begin
  SendDataToAll(ctRawData, Data, Count);
end;

procedure TPegtopBroadcast.SendDataToAll(Data: String);
begin
  SendDataToAll(ctString, Data[1], Length(Data));
end;

procedure TPegtopBroadcast.SendDataToAll(Data: TMemoryStream);
begin
  SendDataToAll(ctStream, Data.Memory^, Data.Size);
end;

function TPegtopBroadcast.SendData(StationID: THandle; ContentType: TPegtopBroadcastContentType; var Data; Count: Longint): Longint;
var
  CopyDataStruct: TCopyDataStruct;
begin
  CopyDataStruct.dwData := Ord(ContentType);
  CopyDataStruct.cbData := Count;
  CopyDataStruct.lpData := @Data;
  Result := Windows.SendMessage(StationID, WM_COPYDATA, Handle, Longint(@CopyDataStruct))
end;

function TPegtopBroadcast.SendData(StationID: THandle; var Data; Count: Longint): Longint;
begin
  Result := SendData(StationID, ctRawData, Data, Count);
end;

function TPegtopBroadcast.SendData(StationID: THandle; Data: String): Longint;
begin
  Result := SendData(StationID, ctString, Data[1], Length(Data));
end;

function TPegtopBroadcast.SendData(StationID: THandle; Data: TMemoryStream): Longint;
begin
  Result := SendData(StationID, ctStream, Data.Memory^, Data.Size);
end;

function TPegtopBroadcast.CreateUUID: String;
begin
  Result := PegtopUUIDToString(PegtopCreateUUID);
end;

procedure TPegtopBroadcast.RegisterNetworkMessage;
begin
  if not (csDesigning in ComponentState) then begin
    if (FNetworkName <> '') and (FNetworkMessageID = 0) then begin
      FNetworkMessageID := RegisterWindowMessage(PChar(FNetworkName));
    end;
  end;
end;

procedure TPegtopBroadcast.Join;
begin
  if FNetworkMessageID <> 0 then begin
    FWaitCount := 0;
    SendMessageTimeout(HWND_BROADCAST, FNetworkMessageID, 1, Handle,
      SMTO_ABORTIFHUNG, 5000, FWaitCount);
  end;
end;

procedure TPegtopBroadcast.Leave;
begin
  if FNetworkMessageID <> 0 then begin
    PostMessageToAll(FNetworkMessageID, 3, Handle);
    FNetworkMessageID := 0;
  end;
end;

procedure TPegtopBroadcast.SetNetworkName(Value: String);
begin
  if FNetworkName <> Value then begin
    Leave;
    FNetworkName := Value;
    if not (csLoading in ComponentState) then begin
      RegisterNetworkMessage;
      Join;
    end;
  end;
end;

function TPegtopBroadcast.GetStationCount: Integer;
begin
  Result := FStationList.Count;
end;

function TPegtopBroadcast.GetStationID(Index: Integer): THandle;
begin
  Result := THandle(FStationList[Index]);
  // if index is out of bounds an EListError is raised by FStationList
end;

end.
