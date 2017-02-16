////////////////////////////////////////////////////////////////////////////////
// File:       PegtopChunkFiles.pas
// Version:    1.02
// Date:       09 May 2004 1.00
//             30 Mar 2005 1.01 (more general classes added)
//             23 Aug 2005 1.02 (text chunk reader added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Functions and classes to read and write chunk files. A chunk file consists of
// a header with an ID (4 bytes) and a size of the data following the header.
// This data again can be sub chunks, consisting of the same kind of header and
// data (recursively). Writing the file can easily be done using the functions
// WriteChunkHeader and CompleteChunkHeader. Reading is more difficult and
// described below.
////////////////////////////////////////////////////////////////////////////////
// Usage:
// Create a TPegtopContainerChunkReader if the (file) stream (only!) contains
// sub chunks (which is very common, otherwise create a TPegtopDataChunkReader
// or a TPegtopStreamChunkReader). Use DefineDataChunk to define data chunks,
// allowed within the container, DefineStreamChunk to define stream chunks,
// DefineContainerChunk to define sub containers. A data chunk holds data of
// constant size (such as a packed record), a stream chunk holds data of
// variable size (such as an image or other binary data). Data and stream chunks
// require an event handler to read the data, container chunks require an event
// handler to define sub chunks of their own. When defining sub chunks you can
// specify following options: pcoUnique (not more than one such chunk is
// allowed) and pcoRequired (at least one such chunk is required). Finally call
// TPegtopContainerChunkReader.Read to read to read the chunk.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopChunkFiles;

interface

uses
  Windows, Classes, PegtopHashes;

type
  TPegtopChunkId = packed array[0..3] of AnsiChar;

  TPegtopChunkHeader = record
    ChunkId: TPegtopChunkId;
    DataSize: Longword;
  end;

  TPegtopChunkOption = (pcoUnique, pcoRequired);
  TPegtopChunkOptions = set of TPegtopChunkOption;

  TPegtopChunkReader = class;
  TPegtopContainerChunkReader = class;

  TPegtopChunkValidateDataEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean) of object;
  TPegtopChunkReadDataEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean) of object;
  TPegtopChunkReadTextEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Text: String; var Valid: Boolean) of object;
  TPegtopChunkReadStreamEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; Stream: TStream; Size: Integer) of object;
  TPegtopChunkContainerEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader) of object;
  TPegtopChunkEvent = procedure(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ChunkReader: TPegtopChunkReader) of object;

  TPegtopChunkReader = class
  private
    FParent: TPegtopChunkReader;
    FIndex: Integer;
  protected
    function LowerId(const Id: TPegtopChunkId): TPegtopChunkId;
    function UpperId(const Id: TPegtopChunkId): TPegtopChunkId;
    function CompareIds(const Id1, Id2: TPegtopChunkId): Boolean;
    function ReadHeader(const Stream: TStream; out Header: TPegtopChunkHeader): Boolean;
    function ReadUnknown(const Stream: TStream; const Header: TPegtopChunkHeader): Boolean; virtual;
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; virtual; abstract;
  public
    constructor Create(const AParent: TPegtopChunkReader); virtual;
    function Read(const Stream: TStream; const Id: TPegtopChunkId): Boolean;
    property Parent: TPegtopChunkReader read FParent;
    property Index: Integer read FIndex write FIndex;
  end;
  // override ReadData

  TPegtopChunkReaderClass = class of TPegtopChunkReader;

  TPegtopStructChunkReader = class(TPegtopChunkReader)
  private
    FData: Pointer;
    FSize: Integer;
    FOnValidateData: TPegtopChunkValidateDataEvent;
  protected
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; override;
    function ValidateChunk(const Id: TPegtopChunkId): Boolean; virtual;
  public
    constructor CreateAndInit(const AParent: TPegtopChunkReader;
      const AData; const ASize: Integer; const AOnValidateData: TPegtopChunkValidateDataEvent);
  end;

  TPegtopDataChunkReader = class(TPegtopChunkReader)
  private
    FSize: Integer;
    FOnReadData: TPegtopChunkReadDataEvent;
  protected
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; override;
  public
    constructor CreateAndInit(const AParent: TPegtopChunkReader;
      const ASize: Integer; const AOnReadData: TPegtopChunkReadDataEvent);
  end;

  TPegtopTextChunkReader = class(TPegtopChunkReader)
  private
    FOnReadText: TPegtopChunkReadTextEvent;
  protected
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; override;
  public
    constructor CreateAndInit(const AParent: TPegtopChunkReader;
      const AOnReadText: TPegtopChunkReadTextEvent);
  end;

  TPegtopStreamChunkReader = class(TPegtopChunkReader)
  private
    FOnReadStream: TPegtopChunkReadStreamEvent;
  protected
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; override;
  public
    constructor CreateAndInit(const AParent: TPegtopChunkReader;
      const AOnReadStream: TPegtopChunkReadStreamEvent);
  end;

  TPegtopCustomContainerChunkReader = class(TPegtopChunkReader)
  protected
    function CreateSubReader(const Id: TPegtopChunkId): TPegtopChunkReader; virtual; abstract;
    function ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean; override;
    function ReadSubChunks(const Stream: TStream; const DataSize: Integer): Boolean;
    function ValidateSubChunk(SubReader: TPegtopChunkReader): Boolean; virtual;
    function ValidateChunk: Boolean; virtual;
  end;
  // override CreateSubReader [, ValidateChunk, ValidateSubChunk]

  TPegtopContainerChunkReader = class(TPegtopCustomContainerChunkReader)
  private
    FDefinitions: TPegtopIntHash;
  protected
    function CreateSubReader(const Id: TPegtopChunkId): TPegtopChunkReader; override;
  public
    constructor Create(const AParent: TPegtopChunkReader); override;
    destructor Destroy; override;
    procedure DefineStructChunk(const Id: TPegtopChunkId; Size: Integer;
      const Data; OnValidateData: TPegtopChunkValidateDataEvent; const Options: TPegtopChunkOptions = [];
      const OnInit: TPegtopChunkEvent = NIL);
    procedure DefineDataChunk(const Id: TPegtopChunkId; Size: Integer;
      OnReadData: TPegtopChunkReadDataEvent; const Options: TPegtopChunkOptions = [];
      const OnInit: TPegtopChunkEvent = NIL);
    procedure DefineTextChunk(const Id: TPegtopChunkId;
      OnReadText: TPegtopChunkReadTextEvent; const Options: TPegtopChunkOptions = [];
      const OnInit: TPegtopChunkEvent = NIL);
    procedure DefineStreamChunk(const Id: TPegtopChunkId;
      OnReadStream: TPegtopChunkReadStreamEvent; const Options: TPegtopChunkOptions = [];
      const OnInit: TPegtopChunkEvent = NIL);
    procedure DefineContainerChunk(const Id: TPegtopChunkId;
      const OnDefineChunks: TPegtopChunkContainerEvent; const Options: TPegtopChunkOptions = [];
      const OnInit: TPegtopChunkEvent = NIL);
  end;

function CompareChunkIds(const Id1, Id2: TPegtopChunkId): Boolean;
function ReadChunkHeader(const Stream: TStream; out ChunkId: TPegtopChunkId; out DataSize: Integer): Boolean;
function WriteChunkHeader(const Stream: TStream; const ChunkId: TPegtopChunkId; const DataSize: Integer): Boolean;
function ForgetChunkHeader(const Stream: TStream): Integer;
function CompleteChunkHeader(const Stream: TStream; const Position: Integer; const ChunkId: TPegtopChunkId): Boolean;
function WriteDataChunk(const Stream: TStream; const ChunkId: TPegtopChunkId; const Data; const DataSize: Integer): Boolean;
function WriteTextChunk(const Stream: TStream; const ChunkId: TPegtopChunkId; const Text: String): Boolean;

const
  UnknownChunkSize = $FFFFFFFF;

implementation

type
  TPegtopChunkDefinition = class
  private
    FOptions: TPegtopChunkOptions;
    FCount: Integer;
    FOnInit: TPegtopChunkEvent;
  public
    procedure IncCount;
    property Options: TPegtopChunkOptions read FOptions;
    property Count: Integer read FCount;
    property OnInit: TPegtopChunkEvent read FOnInit write FOnInit;
  end;

  TPegtopStructChunkDefinition = class(TPegtopChunkDefinition)
  private
    FData: Pointer;
    FSize: Integer;
    FOnValidateData: TPegtopChunkValidateDataEvent;
  public
    constructor Create(const AData; ASize: Integer; AOnValidateData: TPegtopChunkValidateDataEvent;
      AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
    property Data: Pointer read FData;
    property Size: Integer read FSize;
    property OnValidateData: TPegtopChunkValidateDataEvent read FOnValidateData;
  end;

  TPegtopDataChunkDefinition = class(TPegtopChunkDefinition)
  private
    FSize: Integer;
    FOnReadData: TPegtopChunkReadDataEvent;
  public
    constructor Create(ASize: Integer; AOnReadData: TPegtopChunkReadDataEvent;
      AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
    property Size: Integer read FSize;
    property OnReadData: TPegtopChunkReadDataEvent read FOnReadData;
  end;

  TPegtopTextChunkDefinition = class(TPegtopChunkDefinition)
  private
    FOnReadText: TPegtopChunkReadTextEvent;
  public
    constructor Create(AOnReadText: TPegtopChunkReadTextEvent;
      AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
    property OnReadText: TPegtopChunkReadTextEvent read FOnReadText;
  end;

  TPegtopStreamChunkDefinition = class(TPegtopChunkDefinition)
  private
    FOnReadStream: TPegtopChunkReadStreamEvent;
  public
    constructor Create(AOnReadStream: TPegtopChunkReadStreamEvent;
      AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
    property OnReadStream: TPegtopChunkReadStreamEvent read FOnReadStream;
  end;

  TPegtopContainerChunkDefinition = class(TPegtopChunkDefinition)
  private
    FOnDefineChunks: TPegtopChunkContainerEvent;
  public
    constructor Create(AOnDefineChunks: TPegtopChunkContainerEvent;
      AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
    property OnDefineChunks: TPegtopChunkContainerEvent read FOnDefineChunks;
  end;

function CompareChunkIds(const Id1, Id2: TPegtopChunkId): Boolean;
var
  I: Integer;
begin
  Result := True;
  I := 0;
  while Result and (I < 4) do begin
    if (Id1[I] >= 'A') and (Id2[I] <= 'Z') then
      // translate Id2[I] to lower case
      Result := Chr(Ord(Id2[I]) and (not 32)) = Id1[I]
    else if (Id1[I] >= 'a') and (Id2[I] <= 'z') then
      // translate Id2[I] to upper case
      Result := Chr(Ord(Id2[I]) or 32) = Id1[I]
    else
      Result := Id2[I] = Id1[I];
    Inc(I);
  end;
end;

function ReadChunkHeader(const Stream: TStream; out ChunkId: TPegtopChunkId; out DataSize: Integer): Boolean;
var
  Header: TPegtopChunkHeader;
begin
  Result := Stream.Read(Header, SizeOf(TPegtopChunkHeader)) = SizeOf(TPegtopChunkHeader);
  if Result then begin
    ChunkId := Header.ChunkId;
    DataSize := Header.DataSize;
  end
  else begin
    ChunkId := #0#0#0#0;
    DataSize := 0;
  end;
end;

function WriteChunkHeader(const Stream: TStream; const ChunkId: TPegtopChunkId; const DataSize: Integer): Boolean;
var
  Header: TPegtopChunkHeader;
begin
  Header.ChunkId := ChunkId;
  Header.DataSize := DataSize;
  Result := Stream.Write(Header, SizeOf(TPegtopChunkHeader)) = SizeOf(TPegtopChunkHeader);
end;

// If you don't know the data size when writing the chunk header, following
// functions can help. Use them like:
//
// P := ForgetChunkHeader(MyStream);
// WriteMyDataTo(MyStream);
// Success := CompleteChunkHeader(MyStream, P, MyChunkId);
//
// This inserts the header later, when the data size is known (calculated
// automatically from the stream position). In fact it's much simpler than
// calculating the size before writing the data, at the cost of some speed
// (jumping around in your stream).

function ForgetChunkHeader(const Stream: TStream): Integer;
begin
  Result := Stream.Position;
  Stream.Position := Result + SizeOf(TPegtopChunkHeader);
end;

function CompleteChunkHeader(const Stream: TStream; const Position: Integer; const ChunkId: TPegtopChunkId): Boolean;
var
  P: Integer;
begin
  P := Stream.Position;
  Stream.Position := Position;
  Result := WriteChunkHeader(Stream, ChunkId, P - Position - SizeOf(TPegtopChunkHeader));
  Stream.Position := P;
end;

function WriteDataChunk(const Stream: TStream; const ChunkId: TPegtopChunkId; const Data; const DataSize: Integer): Boolean;
begin
  Result := WriteChunkHeader(Stream, ChunkId, DataSize);
  if Result then Result := Stream.Write(Data, DataSize) = DataSize;
end;

function WriteTextChunk(const Stream: TStream; const ChunkId: TPegtopChunkId; const Text: String): Boolean;
var
  L: Integer;
begin
  L := Length(Text);
  Result := WriteChunkHeader(Stream, ChunkId, L);
  if Result then Result := Stream.Write(Text[1], L) = L;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopChunkReader.Create(const AParent: TPegtopChunkReader);
begin
  FParent := AParent;
end;

function TPegtopChunkReader.LowerId(const Id: TPegtopChunkId): TPegtopChunkId;
var
  I: Integer;
begin
  Result := Id;
  for I := 0 to 3 do begin
    if (Result[I] >= 'A') and (Result[I] <= 'Z') then
      Result[I] := Chr(Ord(Result[I]) xor 32);
  end;
end;

function TPegtopChunkReader.UpperId(const Id: TPegtopChunkId): TPegtopChunkId;
var
  I: Integer;
begin
  Result := Id;
  for I := 0 to 3 do begin
    if (Result[I] >= 'a') and (Result[I] <= 'z') then
      Result[I] := Chr(Ord(Result[I]) xor 32);
  end;
end;

function TPegtopChunkReader.CompareIds(const Id1, Id2: TPegtopChunkId): Boolean;
begin
  Result := CompareChunkIds(Id1, Id2);
end;

function TPegtopChunkReader.ReadHeader(const Stream: TStream; out Header: TPegtopChunkHeader): Boolean;
begin
  Result := Stream.Read(Header, SizeOf(TPegtopChunkHeader)) = SizeOf(TPegtopChunkHeader);
end;

function TPegtopChunkReader.ReadUnknown(const Stream: TStream; const Header: TPegtopChunkHeader): Boolean;
begin
  Stream.Seek(Header.DataSize, soFromCurrent);
  Result := True;
end;

function TPegtopChunkReader.Read(const Stream: TStream; const Id: TPegtopChunkId): Boolean;
var
  Header: TPegtopChunkHeader;
begin
  Result := ReadHeader(Stream, Header);
  if Result then begin
    Result := CompareIds(Header.ChunkId, Id);
    if Result then begin
      Result := ReadData(Stream, Header.ChunkId, Header.DataSize);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopStructChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopStructChunkReader.CreateAndInit(const AParent: TPegtopChunkReader;
  const AData; const ASize: Integer; const AOnValidateData: TPegtopChunkValidateDataEvent);
begin
  inherited Create(AParent);
  FData := @AData;
  FSize := ASize;
  FOnValidateData := AOnValidateData;
end;

function TPegtopStructChunkReader.ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean;
begin
  if FData = NIL then begin
    Stream.Seek(DataSize, soFromCurrent);
    Result := False;
  end
  else begin
    if FSize > DataSize then begin
      Result := Stream.Read(FData^, DataSize) = Integer(DataSize);
      if Result then FillChar(Pointer(Integer(FData) + Integer(DataSize))^, FSize - Integer(DataSize), 0);
    end
    else begin
      Result := Stream.Read(FData^, FSize) = FSize;
      if DataSize > FSize then Stream.Seek(Integer(DataSize) - FSize, soFromCurrent);
    end;
    if Result then Result := ValidateChunk(Id);
  end;
end;

function TPegtopStructChunkReader.ValidateChunk(const Id: TPegtopChunkId): Boolean;
begin
  Result := True;
  if Assigned(FOnValidateData) then FOnValidateData(Self, Id, FIndex, FData^, FSize, Result);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopDataChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopDataChunkReader.CreateAndInit(const AParent: TPegtopChunkReader;
  const ASize: Integer; const AOnReadData: TPegtopChunkReadDataEvent);
begin
  FSize := ASize;
  FOnReadData := AOnReadData;
  inherited Create(AParent);
end;

function TPegtopDataChunkReader.ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean;
var
  Data: Pointer;
begin
  if Assigned(FOnReadData) then begin
    GetMem(Data, FSize);
    if FSize > DataSize then begin
      Result := Stream.Read(Data^, DataSize) = Integer(DataSize);
      if Result then FillChar(Pointer(Integer(Data) + Integer(DataSize))^, FSize - Integer(DataSize), 0);
    end
    else begin
      Result := Stream.Read(Data^, FSize) = FSize;
      if DataSize > FSize then Stream.Seek(Integer(DataSize) - FSize, soFromCurrent);
    end;
    if Result then FOnReadData(Self, Id, FIndex, Data^, FSize, Result);
    FreeMem(Data);
  end
  else begin
    Stream.Seek(DataSize, soFromCurrent);
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopTextChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopTextChunkReader.CreateAndInit(const AParent: TPegtopChunkReader;
  const AOnReadText: TPegtopChunkReadTextEvent);
begin
  FOnReadText := AOnReadText;
  inherited Create(AParent);
end;

function TPegtopTextChunkReader.ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean;
var
  Text: String;
begin
  if Assigned(FOnReadText) then begin
    SetLength(Text, DataSize);
    Result := Stream.Read(Text[1], DataSize) = DataSize;
    if Result then FOnReadText(Self, Id, FIndex, Text, Result);
  end
  else begin
    Stream.Seek(DataSize, soFromCurrent);
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopStreamChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopStreamChunkReader.CreateAndInit(const AParent: TPegtopChunkReader;
  const AOnReadStream: TPegtopChunkReadStreamEvent);
begin
  inherited Create(AParent);
  FOnReadStream := AOnReadStream;
end;

function TPegtopStreamChunkReader.ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean;
var
  P: Integer;
begin
  if Assigned(FOnReadStream) then begin
    P := Stream.Position;
    FOnReadStream(Self, Id, FIndex, Stream, DataSize);
    Stream.Position := P + Integer(DataSize);
    Result := True;
  end
  else begin
    Stream.Seek(DataSize, soFromCurrent);
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomContainerChunkReader
////////////////////////////////////////////////////////////////////////////////

function TPegtopCustomContainerChunkReader.ReadData(const Stream: TStream; const Id: TPegtopChunkId; const DataSize: Integer): Boolean;
begin
  Result := ReadSubChunks(Stream, DataSize);
  if Result then Result := ValidateChunk;
end;

function TPegtopCustomContainerChunkReader.ReadSubChunks(const Stream: TStream; const DataSize: Integer): Boolean;
var
  Header: TPegtopChunkHeader;
  SubReader: TPegtopChunkReader;
  RemainingSize: Integer;
begin
  Result := True;
  RemainingSize := DataSize;
  while Result and (RemainingSize > 0) do begin
    if RemainingSize < SizeOf(TPegtopChunkHeader) then begin
      Stream.Seek(RemainingSize, soFromCurrent);
      Result := False;
    end
    else begin
      Result := ReadHeader(Stream, Header);
      if Result then begin
        Dec(RemainingSize, SizeOf(TPegtopChunkHeader));
        if Header.DataSize > Longword(RemainingSize) then begin
          Stream.Seek(RemainingSize, soFromCurrent);
          Result := False;
        end
        else begin
          SubReader := CreateSubReader(Header.ChunkId);
          if SubReader <> NIL then begin
            try
              Result := SubReader.ReadData(Stream, Header.ChunkId, Header.DataSize);
              if Result then Result := ValidateSubChunk(SubReader);
            finally
              SubReader.Free;
            end;
          end
          else begin
            Result := ReadUnknown(Stream, Header);
          end;
          Dec(RemainingSize, Header.DataSize);
        end;
      end;
    end;
  end;
end;

function TPegtopCustomContainerChunkReader.ValidateSubChunk(SubReader: TPegtopChunkReader): Boolean;
begin
  Result := True;
end;

function TPegtopCustomContainerChunkReader.ValidateChunk: Boolean;
begin
  Result := True;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopContainerChunkReader
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopContainerChunkReader.Create(const AParent: TPegtopChunkReader);
begin
  inherited;
  FDefinitions := TPegtopIntHash.Create;
end;

destructor TPegtopContainerChunkReader.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDefinitions.Count - 1 do begin
    TPegtopChunkDefinition(FDefinitions[FDefinitions.Key[I]]).Free;
  end;
  FDefinitions.Free;
  inherited;
end;

function TPegtopContainerChunkReader.CreateSubReader(const Id: TPegtopChunkId): TPegtopChunkReader;
var
  Definition: TPegtopChunkDefinition;
begin
  Result := NIL;
  Definition := FDefinitions[Longint(Id)];
  if Definition <> NIL then begin
    if (not (pcoUnique in Definition.Options)) or (Definition.Count = 0) then begin
      if Definition is TPegtopStructChunkDefinition then begin
        Result := TPegtopStructChunkReader.CreateAndInit(Self,
          TPegtopStructChunkDefinition(Definition).Data^,
          TPegtopStructChunkDefinition(Definition).FSize,
          TPegtopStructChunkDefinition(Definition).FOnValidateData);
        TPegtopStructChunkReader(Result).FIndex := Definition.Count;
      end
      else if Definition is TPegtopDataChunkDefinition then begin
        Result := TPegtopDataChunkReader.CreateAndInit(Self,
          TPegtopDataChunkDefinition(Definition).FSize,
          TPegtopDataChunkDefinition(Definition).FOnReadData);
        TPegtopDataChunkReader(Result).FIndex := Definition.Count;
      end
      else if Definition is TPegtopTextChunkDefinition then begin
        Result := TPegtopTextChunkReader.CreateAndInit(Self,
          TPegtopTextChunkDefinition(Definition).FOnReadText);
        TPegtopTextChunkReader(Result).FIndex := Definition.Count;
      end
      else if Definition is TPegtopStreamChunkDefinition then begin
        Result := TPegtopStreamChunkReader.CreateAndInit(Self,
          TPegtopStreamChunkDefinition(Definition).FOnReadStream);
        TPegtopStreamChunkReader(Result).FIndex := Definition.Count;
      end
      else if Definition is TPegtopContainerChunkDefinition then begin
        Result := TPegtopContainerChunkReader.Create(Self);
        TPegtopContainerChunkReader(Result).FIndex := Definition.Count;
        if Assigned(TPegtopContainerChunkDefinition(Definition).FOnDefineChunks) then
          TPegtopContainerChunkDefinition(Definition).FOnDefineChunks(Self, Id, Definition.Count,
            TPegtopContainerChunkReader(Result));
      end;
      if Assigned(Definition.FOnInit) then
        Definition.FOnInit(Self, Id, Definition.Count, Result);
    end;
    Definition.IncCount;
  end;
end;

procedure TPegtopContainerChunkReader.DefineStructChunk(const Id: TPegtopChunkId; Size: Integer;
  const Data; OnValidateData: TPegtopChunkValidateDataEvent; const Options: TPegtopChunkOptions = [];
  const OnInit: TPegtopChunkEvent = NIL);
begin
  if not FDefinitions.Contains(Longint(Id)) then begin
    FDefinitions.Add(Longint(Id), TPegtopStructChunkDefinition.Create(Data, Size, OnValidateData, Options, OnInit));
  end;
end;

procedure TPegtopContainerChunkReader.DefineDataChunk(const Id: TPegtopChunkId; Size: Integer;
  OnReadData: TPegtopChunkReadDataEvent; const Options: TPegtopChunkOptions = [];
  const OnInit: TPegtopChunkEvent = NIL);
begin
  if not FDefinitions.Contains(Longint(Id)) then begin
    FDefinitions.Add(Longint(Id), TPegtopDataChunkDefinition.Create(Size, OnReadData, Options, OnInit));
  end;
end;

procedure TPegtopContainerChunkReader.DefineTextChunk(const Id: TPegtopChunkId;
  OnReadText: TPegtopChunkReadTextEvent; const Options: TPegtopChunkOptions = [];
  const OnInit: TPegtopChunkEvent = NIL);
begin
  if not FDefinitions.Contains(Longint(Id)) then begin
    FDefinitions.Add(Longint(Id), TPegtopTextChunkDefinition.Create(OnReadText, Options, OnInit));
  end;
end;

procedure TPegtopContainerChunkReader.DefineStreamChunk(const Id: TPegtopChunkId;
  OnReadStream: TPegtopChunkReadStreamEvent; const Options: TPegtopChunkOptions = [];
  const OnInit: TPegtopChunkEvent = NIL);
begin
  if not FDefinitions.Contains(Longint(Id)) then begin
    FDefinitions.Add(Longint(Id), TPegtopStreamChunkDefinition.Create(OnReadStream, Options, OnInit));
  end;
end;

procedure TPegtopContainerChunkReader.DefineContainerChunk(const Id: TPegtopChunkId;
  const OnDefineChunks: TPegtopChunkContainerEvent; const Options: TPegtopChunkOptions = [];
  const OnInit: TPegtopChunkEvent = NIL);
begin
  if not FDefinitions.Contains(Longint(Id)) then begin
    FDefinitions.Add(Longint(Id), TPegtopContainerChunkDefinition.Create(OnDefineChunks, Options, OnInit));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopChunkDefinition
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopChunkDefinition.IncCount;
begin
  Inc(FCount);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopStructChunkDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopStructChunkDefinition.Create(const AData; ASize: Integer; AOnValidateData: TPegtopChunkValidateDataEvent;
  AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
begin
  FData := @AData;
  FSize := ASize;
  FOnValidateData := AOnValidateData;
  FOptions := AOptions;
  FOnInit := OnInit;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopDataChunkDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopDataChunkDefinition.Create(ASize: Integer; AOnReadData: TPegtopChunkReadDataEvent;
  AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
begin
  FSize := ASize;
  FOnReadData := AOnReadData;
  FOptions := AOptions;
  FOnInit := OnInit;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopTextChunkDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopTextChunkDefinition.Create(AOnReadText: TPegtopChunkReadTextEvent;
  AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
begin
  FOnReadText := AOnReadText;
  FOptions := AOptions;
  FOnInit := OnInit;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopStreamChunkDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopStreamChunkDefinition.Create(AOnReadStream: TPegtopChunkReadStreamEvent;
  AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
begin
  FOnReadStream := AOnReadStream;
  FOptions := AOptions;
  FOnInit := OnInit;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopContainerChunkDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopContainerChunkDefinition.Create(AOnDefineChunks: TPegtopChunkContainerEvent;
  AOptions: TPegtopChunkOptions; const OnInit: TPegtopChunkEvent = NIL);
begin
  FOnDefineChunks := AOnDefineChunks;
  FOptions := AOptions;
  FOnInit := OnInit;
end;

end.
