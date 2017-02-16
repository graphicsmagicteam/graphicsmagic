////////////////////////////////////////////////////////////////////////////////
// File:       PegtopHashes.pas
// Classes:    TPegtopAbstractHash, TPegtopIntHash, TPegtopStringHash,
//             TPegtopVirtualHash
// Version:    1.01
// Date:       23 Mar 2004 created 1.00
//             25 Jan 2005 modified 1.01 (major bug in GetIndex fixed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopIntHash and TPegtopStringHash are hash tables for integer / string
// keys.
// TPegtopVirtualHash was a try to implement hashes for keys of any data type.
////////////////////////////////////////////////////////////////////////////////
// Note:
// While these hash classes a pretty fast, a bit more memory is used than needed
// theoretically (three times instead of only twice the capacity).
// I really tried to make it fast, not small, for this reason I needed
// some more code, too (you will notice that the different hash classes
// implement very similar methods).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopHashes;

interface

type
  TPegtopHashItemState = (phiEmpty, phiErased, phiUsed);

  TPegtopIntHashItem = record
    Position: Integer;
    Index: Integer;
    State: TPegtopHashItemState;
    Key: Integer;
    Data: Pointer;
  end;

  TPegtopStringHashItem = record
    Position: Integer;
    Index: Integer;
    State: TPegtopHashItemState;
    Key: String;
    Data: Pointer;
  end;

  TPegtopPointerHashItem = record
    Position: Integer;
    Index: Integer;
    State: TPegtopHashItemState;
    Key: Pointer;
    Data: Pointer;
  end;

  TPegtopIntHashItemArray = array[0..255] of TPegtopIntHashItem;
  PPegtopIntHashItemArray = ^TPegtopIntHashItemArray;

  TPegtopStringHashItemArray = array[0..255] of TPegtopStringHashItem;
  PPegtopStringHashItemArray = ^TPegtopStringHashItemArray;

  TPegtopPointerHashItemArray = array[0..255] of TPegtopPointerHashItem;
  PPegtopPointerHashItemArray = ^TPegtopPointerHashItemArray;

  TPegtopHashDataEvent = procedure(Sender: TObject; Data: Pointer) of object;

  TPegtopAbstractHash = class
  protected
    FCapacity: Integer;
    FSize: Integer;
    FCount: Integer;
    class function IsPrim(Number: Integer): Boolean;
    class function GetNextPrim(Number: Integer): Integer;
    procedure SetCapacity(Value: Integer);
    procedure ChangeSize(NewSize: Integer); virtual; abstract;
    function GetItemSize: Integer; virtual; abstract;
  public
    constructor Create(InitialCapacity: Integer = 100); virtual;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Size: Integer read FSize;
  end;

  TPegtopIntHash = class(TPegtopAbstractHash)
  private
    FTab: PPegtopIntHashItemArray;
    function GetHashCode(Key: Integer): Integer;
    function GetIndex(Key: Integer): Integer;
    function GetData(Key: Integer): Pointer;
    procedure SetData(Key: Integer; V: Pointer);
    procedure AddItem(Key: Integer; Data: Pointer);
    function GetDataAt(Index: Integer): Pointer;
    function GetKey(Index: Integer): Integer;
  protected
    procedure ChangeSize(NewSize: Integer); override;
    function GetItemSize: Integer; override;
  public
    constructor Create(InitialCapacity: Integer = 100); override;
    destructor Destroy; override;
    procedure Assign(Hash: TPegtopIntHash); virtual;
    function Contains(Key: Integer): Boolean; virtual;
    function Add(Key: Integer; Data: Pointer): Boolean; virtual;
    function Remove(Key: Integer): Boolean; virtual;
    procedure ProcessData(OnProcess: TPegtopHashDataEvent);
    procedure Clear(OnRemove: TPegtopHashDataEvent);
    property Data[Key: Integer]: Pointer read GetData write SetData; default;
    property DataAt[Index: Integer]: Pointer read GetDataAt;
    property Key[Index: Integer]: Integer read GetKey;
  end;

  TPegtopStringHash = class(TPegtopAbstractHash)
  private
    FTab: PPegtopStringHashItemArray;
    FCaseSensitive: Boolean;
    function GetHashCode(Key: String): Integer;
    function GetIndex(Key: String): Integer;
    function GetData(Key: String): Pointer;
    procedure SetData(Key: String; V: Pointer);
    procedure AddItem(Key: String; Data: Pointer);
    function GetDataAt(Index: Integer): Pointer;
    function GetKey(Index: Integer): String;
    procedure SetCaseSensitive(Value: Boolean);
  protected
    procedure ChangeSize(NewSize: Integer); override;
    function GetItemSize: Integer; override;
    function TransformString(S: String): String; virtual;
  public
    constructor Create(InitialCapacity: Integer = 100); override;
    destructor Destroy; override;
    procedure Assign(Hash: TPegtopStringHash); virtual;
    function Contains(Key: String): Boolean; virtual;
    function Add(Key: String; Data: Pointer): Boolean; virtual;
    function Remove(Key: String): Boolean; virtual;
    procedure ProcessData(OnProcess: TPegtopHashDataEvent);
    procedure Clear(OnRemove: TPegtopHashDataEvent);
    property Data[Key: String]: Pointer read GetData write SetData; default;
    property DataAt[Index: Integer]: Pointer read GetDataAt;
    property Key[Index: Integer]: String read GetKey;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  end;

  TPegtopVirtualHashCodeEvent = function(Key: Pointer): Integer;
  TPegtopVirtualHashEqualEvent = function(Key1, Key2: Pointer): Boolean;

  TPegtopVirtualHash = class(TPegtopAbstractHash)
  private
    FTab: PPegtopPointerHashItemArray;
    FOnHashCode: TPegtopVirtualHashCodeEvent;
    FOnEqual: TPegtopVirtualHashEqualEvent;
    function GetHashCode(Key: Pointer): Integer;
    function GetIndex(Key: Pointer): Integer;
    function GetData(Key: Pointer): Pointer;
    procedure SetData(Key: Pointer; V: Pointer);
    procedure AddItem(Key: Pointer; Data: Pointer);
    function GetDataAt(Index: Integer): Pointer;
    function GetKey(Index: Integer): Pointer;
  protected
    procedure ChangeSize(NewSize: Integer); override;
    function GetItemSize: Integer; override;
  public
    constructor Create(OnHashCode: TPegtopVirtualHashCodeEvent;
      OnEqual: TPegtopVirtualHashEqualEvent;
      InitialCapacity: Integer = 100); reintroduce;
    destructor Destroy; override;
    procedure Assign(Hash: TPegtopVirtualHash); virtual;
    function Contains(Key: Pointer): Boolean; virtual;
    function Add(Key: Pointer; Data: Pointer): Boolean; virtual;
    function Remove(Key: Pointer): Boolean; virtual;
    procedure ProcessData(OnProcess: TPegtopHashDataEvent);
    procedure Clear(OnRemove: TPegtopHashDataEvent);
    property DataAt[Index: Integer]: Pointer read GetDataAt;
    property Data[Key: Pointer]: Pointer read GetData write SetData; default;
    property Key[Index: Integer]: Pointer read GetKey;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPegtopAbstractHash
////////////////////////////////////////////////////////////////////////////////

class function TPegtopAbstractHash.IsPrim(Number: Integer): Boolean;
var
  Max: Integer;
  I: Integer;
begin
  if Number = 2 then begin
    Result := True;
  end
  else if(Number and 1) = 0 then begin
    Result := False;
  end
  else begin
    Max := Trunc(Sqrt(Number)+0.0001);
    I := 3;
    Result := True;
    while (Result and (I <= Max)) do begin
      Result := (Number mod I) <> 0;
      Inc(I, 2);
    end;
  end;
end;

class function TPegtopAbstractHash.GetNextPrim(Number: Integer): Integer;
begin
  Result := Number or 1; // next impair
  while (not IsPrim(Result)) do Inc(Result, 2);
end;

constructor TPegtopAbstractHash.Create(InitialCapacity: Integer = 100);
begin
  if InitialCapacity < 10 then InitialCapacity := 10;
  FCapacity := InitialCapacity;
  FSize := GetNextPrim(FCapacity * 2);
  FCount := 0;
end;

procedure TPegtopAbstractHash.SetCapacity(Value: Integer);
var
  NewSize: Integer;
begin
  if Value < FCount then Value := FCount;
  if Value < 10 then Value := 10;
  if Value <> FCapacity then begin
    FCapacity := Value;
    NewSize := GetNextPrim(FCapacity * 2);
    if FSize <> NewSize then ChangeSize(NewSize);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopIntHash
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopIntHash.Create(InitialCapacity: Integer = 100);
begin
  inherited Create(InitialCapacity);
  FTab := AllocMem(SizeOf(TPegtopIntHashItem) * FSize);
end;

destructor TPegtopIntHash.Destroy;
begin
  FreeMem(FTab, SizeOf(TPegtopIntHashItem) * FSize);
  inherited;
end;

function TPegtopIntHash.GetItemSize: Integer;
begin
  Result := SizeOf(TPegtopIntHashItem);
end;

procedure TPegtopIntHash.Assign(Hash: TPegtopIntHash);
begin
  if Hash <> Self then begin
    FreeMem(FTab, SizeOf(TPegtopIntHashItem) * FSize);
    FCapacity := Hash.FCapacity;
    FSize := Hash.FSize;
    FCount := Hash.FCount;
    GetMem(FTab, SizeOf(TPegtopIntHashItem) * FSize);
    Move(Hash.FTab^, FTab^, SizeOf(TPegtopIntHashItem) * FSize);
  end;
end;

procedure TPegtopIntHash.ChangeSize(NewSize: Integer);
var
  OldTab: PPegtopIntHashItemArray;
  OldCount, OldSize: Integer;
  I, J: Integer;
begin
  OldTab := FTab;
  OldCount := FCount;
  OldSize := FSize;
  FSize := NewSize;
  FCount := 0;
  FTab := AllocMem(SizeOf(TPegtopIntHashItem) * FSize);
  for I := 0 to OldCount-1 do begin
    J := OldTab^[I].Position;
    AddItem(OldTab^[J].Key, OldTab^[J].Data);
  end;
  FreeMem(OldTab, SizeOf(TPegtopIntHashItem) * OldSize);
end;

function TPegtopIntHash.Contains(Key: Integer): Boolean;
begin
  Result := GetIndex(Key) >= 0;
end;

function TPegtopIntHash.GetData(Key: Integer): Pointer;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := NIL;
  end
  else begin
    Result := FTab^[I].Data;
  end;
end;

procedure TPegtopIntHash.SetData(Key: Integer; V: Pointer);
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    AddItem(Key, V);
  end
  else begin
    FTab^[I].Data := V;
  end;
end;

procedure TPegtopIntHash.AddItem(Key: Integer; Data: Pointer);
var
  I, A: Integer;
begin
  if FCount >= FCapacity then SetCapacity(FCapacity*2);
  I := GetHashCode(Key);
  A := 1;
  while (FTab^[I].State >= phiUsed) do begin
    I := (I + A) mod FSize;
    Inc(A, 2);
  end;
  FTab^[FCount].Position := I;
  FTab^[I].Index := FCount;
  FTab^[I].State := phiUsed;
  FTab^[I].Key := Key;
  FTab^[I].Data := Data;
  Inc(FCount);
end;

function TPegtopIntHash.Add(Key: Integer; Data: Pointer): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if (I >= 0) then begin
    Result := False;
  end
  else begin
    AddItem(Key, Data);
    Result := True;
  end;
end;

function TPegtopIntHash.Remove(Key: Integer): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := False;
  end
  else begin
    FTab^[I].State := phiErased;
    FTab^[FTab^[I].Index].Position := FTab^[FCount-1].Position;
    FTab^[FTab^[FCount-1].Position].Index := FTab^[I].Index;
    Dec(FCount);
    Result := True;
  end;
end;

procedure TPegtopIntHash.ProcessData(OnProcess: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  if Assigned(OnProcess) then begin
    for I := 0 to FCount-1 do begin
      J := FTab^[I].Position;
      OnProcess(Self, FTab^[J].Data);
    end;
  end;
end;

procedure TPegtopIntHash.Clear(OnRemove: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  ProcessData(OnRemove);
  for I := 0 to FCount-1 do begin
    J := FTab^[I].Position;
    FTab^[J].State := phiEmpty;
  end;
  FCount := 0;
end;

function TPegtopIntHash.GetIndex(Key: Integer): Integer;
var
  Start: Integer;
  I: Integer;
  A: Integer;
begin
  Result := -1;
  Start := GetHashCode(Key);
  I := Start;
  A := 1;
  while (Result = -1) and (FTab^[I].State > phiEmpty) do begin
    if (FTab^[I].State >= phiUsed) and (FTab^[I].Key = Key) then begin
      Result := I;
    end
    else begin
      I := (I + A) mod FSize;
      if I = Start then Exit;
      Inc(A, 2);
    end;
  end;
end;

function TPegtopIntHash.GetDataAt(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Data
  else
    Result := NIL;
end;

function TPegtopIntHash.GetKey(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Key
  else
    Result := 0;
end;

function TPegtopIntHash.GetHashCode(Key: Integer): Integer;
begin
  Result := Key mod FSize;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopStringHash
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopStringHash.Create(InitialCapacity: Integer = 100);
begin
  FCaseSensitive := False;
  inherited Create(InitialCapacity);
  FTab := AllocMem(SizeOf(TPegtopStringHashItem) * FSize);
end;

destructor TPegtopStringHash.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSize-1 do FTab^[I].Key := ''; // free memory
  FreeMem(FTab, SizeOf(TPegtopStringHashItem) * FSize);
  inherited;
end;

function TPegtopStringHash.GetItemSize: Integer;
begin
  Result := SizeOf(TPegtopStringHashItem);
end;

procedure TPegtopStringHash.Assign(Hash: TPegtopStringHash);
begin
  if Hash <> Self then begin
    FreeMem(FTab, SizeOf(TPegtopStringHashItem) * FSize);
    FCapacity := Hash.FCapacity;
    FSize := Hash.FSize;
    FCount := Hash.FCount;
    GetMem(FTab, SizeOf(TPegtopStringHashItem) * FSize);
    Move(Hash.FTab^, FTab^, SizeOf(TPegtopStringHashItem) * FSize);
  end;
end;

procedure TPegtopStringHash.ChangeSize(NewSize: Integer);
var
  OldTab: PPegtopStringHashItemArray;
  OldCount, OldSize: Integer;
  I, J: Integer;
begin
  OldTab := FTab;
  OldCount := FCount;
  OldSize := FSize;
  FSize := NewSize;
  FCount := 0;
  FTab := AllocMem(SizeOf(TPegtopStringHashItem) * FSize);
  for I := 0 to OldCount-1 do begin
    J := OldTab^[I].Position;
    AddItem(OldTab^[J].Key, OldTab^[J].Data);
    OldTab^[J].Key := ''; // free memory
  end;
  FreeMem(OldTab, SizeOf(TPegtopStringHashItem) * OldSize);
end;

function TPegtopStringHash.Contains(Key: String): Boolean;
begin
  Result := GetIndex(Key) >= 0;
end;

function TPegtopStringHash.GetData(Key: String): Pointer;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := NIL;
  end
  else begin
    Result := FTab^[I].Data;
  end;
end;

procedure TPegtopStringHash.SetData(Key: String; V: Pointer);
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    AddItem(Key, V);
  end
  else begin
    FTab^[I].Data := V;
  end;
end;

procedure TPegtopStringHash.AddItem(Key: String; Data: Pointer);
var
  I, A: Integer;
begin
  if FCount >= FCapacity then SetCapacity(FCapacity*2);
  I := GetHashCode(TransformString(Key));
  A := 1;
  while (FTab^[I].State >= phiUsed) do begin
    I := (I + A) mod FSize;
    Inc(A, 2);
  end;
  FTab^[FCount].Position := I;
  FTab^[I].Index := FCount;
  FTab^[I].State := phiUsed;
  FTab^[I].Key := Key;
  FTab^[I].Data := Data;
  Inc(FCount);
end;

function TPegtopStringHash.Add(Key: String; Data: Pointer): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if (I >= 0) then begin
    Result := False;
  end
  else begin
    AddItem(Key, Data);
    Result := True;
  end;
end;

function TPegtopStringHash.Remove(Key: String): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := False;
  end
  else begin
    FTab^[I].State := phiErased;
    FTab^[I].Key := ''; // free memory
    FTab^[FTab^[I].Index].Position := FTab^[FCount-1].Position;
    FTab^[FTab^[FCount-1].Position].Index := FTab^[I].Index;
    Dec(FCount);
    Result := True;
  end;
end;

procedure TPegtopStringHash.ProcessData(OnProcess: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  if Assigned(OnProcess) then begin
    for I := 0 to FCount-1 do begin
      J := FTab^[I].Position;
      OnProcess(Self, FTab^[J].Data);
    end;
  end;
end;

procedure TPegtopStringHash.Clear(OnRemove: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  ProcessData(OnRemove);
  for I := 0 to FCount-1 do begin
    J := FTab^[I].Position;
    FTab^[J].Key := ''; // free memory
    FTab^[J].State := phiEmpty;
  end;
  FCount := 0;
end;

function TPegtopStringHash.GetIndex(Key: String): Integer;
var
  Start: Integer;
  I: Integer;
  A: Integer;
  S: String;
begin
  Result := -1;
  S := TransformString(Key);
  Start := GetHashCode(S);
  I := Start;
  A := 1;
  while (Result = -1) and (FTab^[I].State > phiEmpty) do begin
    if (FTab^[I].State >= phiUsed) and (TransformString(FTab^[I].Key) = S) then begin
      Result := I;
    end
    else begin
      I := (I + A) mod FSize;
      if I = Start then Exit;
      Inc(A, 2);
    end;
  end;
end;

function TPegtopStringHash.GetDataAt(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Data
  else
    Result := NIL;
end;

function TPegtopStringHash.GetKey(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Key
  else
    Result := '';
end;

function TPegtopStringHash.GetHashCode(Key: String): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  L := Length(Key);
  for I := 1 to L do begin
    Result := (Result mod FSize) * 256 + Ord(Key[I]);
  end;
  Result := Result mod FSize;
end;

function TPegtopStringHash.TransformString(S: String): String;
begin
  if FCaseSensitive then
    Result := S
  else
    Result := AnsiLowerCase(S);
end;

procedure TPegtopStringHash.SetCaseSensitive(Value: Boolean);
var
  OldTab: PPegtopStringHashItemArray;
  OldCount: Integer;
  I, J: Integer;
begin
  if FCaseSensitive <> Value then begin
    FCaseSensitive := Value;
    if FCount > 0 then begin
      OldTab := FTab;
      OldCount := FCount;
      FCount := 0;
      FTab := AllocMem(SizeOf(TPegtopStringHashItem) * FSize);
      for I := 0 to OldCount-1 do begin
        J := OldTab^[I].Position;
        Add(OldTab^[J].Key, OldTab^[J].Data);
        OldTab^[J].Key := ''; // free memory
      end;
      FreeMem(OldTab, SizeOf(TPegtopStringHashItem) * FSize);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopVirtualHash
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopVirtualHash.Create(OnHashCode: TPegtopVirtualHashCodeEvent;
  OnEqual: TPegtopVirtualHashEqualEvent;
  InitialCapacity: Integer = 100);
begin
  FOnEqual := OnEqual;
  FOnHashCode := OnHashCode;
  inherited Create(InitialCapacity);
  FTab := AllocMem(SizeOf(TPegtopPointerHashItem) * FSize);
end;

destructor TPegtopVirtualHash.Destroy;
begin
  FreeMem(FTab, SizeOf(TPegtopPointerHashItem) * FSize);
  inherited;
end;

function TPegtopVirtualHash.GetItemSize: Integer;
begin
  Result := SizeOf(TPegtopPointerHashItem);
end;

procedure TPegtopVirtualHash.Assign(Hash: TPegtopVirtualHash);
begin
  if Hash <> Self then begin
    FreeMem(FTab, SizeOf(TPegtopPointerHashItem) * FSize);
    FCapacity := Hash.FCapacity;
    FSize := Hash.FSize;
    FCount := Hash.FCount;
    GetMem(FTab, SizeOf(TPegtopPointerHashItem) * FSize);
    Move(Hash.FTab^, FTab^, SizeOf(TPegtopPointerHashItem) * FSize);
  end;
end;

procedure TPegtopVirtualHash.ChangeSize(NewSize: Integer);
var
  OldTab: PPegtopPointerHashItemArray;
  OldCount, OldSize: Integer;
  I, J: Integer;
begin
  OldTab := FTab;
  OldCount := FCount;
  OldSize := FSize;
  FSize := NewSize;
  FCount := 0;
  FTab := AllocMem(SizeOf(TPegtopPointerHashItem) * FSize);
  for I := 0 to OldCount-1 do begin
    J := OldTab^[I].Position;
    AddItem(OldTab^[J].Key, OldTab^[J].Data);
  end;
  FreeMem(OldTab, SizeOf(TPegtopPointerHashItem) * OldSize);
end;

function TPegtopVirtualHash.Contains(Key: Pointer): Boolean;
begin
  Result := GetIndex(Key) >= 0;
end;

function TPegtopVirtualHash.GetData(Key: Pointer): Pointer;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := NIL;
  end
  else begin
    Result := FTab^[I].Data;
  end;
end;

procedure TPegtopVirtualHash.SetData(Key: Pointer; V: Pointer);
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    AddItem(Key, V);
  end
  else begin
    FTab^[I].Data := V;
  end;
end;

procedure TPegtopVirtualHash.AddItem(Key: Pointer; Data: Pointer);
var
  I, A: Integer;
begin
  if FCount >= FCapacity then SetCapacity(FCapacity*2);
  I := GetHashCode(Key);
  A := 1;
  while (FTab^[I].State >= phiUsed) do begin
    I := (I + A) mod FSize;
    Inc(A, 2);
  end;
  FTab^[FCount].Position := I;
  FTab^[I].Index := FCount;
  FTab^[I].State := phiUsed;
  FTab^[I].Key := Key;
  FTab^[I].Data := Data;
  Inc(FCount);
end;

function TPegtopVirtualHash.Add(Key: Pointer; Data: Pointer): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if (I >= 0) then begin
    Result := False;
  end
  else begin
    AddItem(Key, Data);
    Result := True;
  end;
end;

function TPegtopVirtualHash.Remove(Key: Pointer): Boolean;
var
  I: Integer;
begin
  I := GetIndex(Key);
  if I < 0 then begin
    Result := False;
  end
  else begin
    FTab^[I].State := phiErased;
    FTab^[FTab^[I].Index].Position := FTab^[FCount-1].Position;
    FTab^[FTab^[FCount-1].Position].Index := FTab^[I].Index;
    Dec(FCount);
    Result := True;
  end;
end;

procedure TPegtopVirtualHash.ProcessData(OnProcess: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  if Assigned(OnProcess) then begin
    for I := 0 to FCount-1 do begin
      J := FTab^[I].Position;
      OnProcess(Self, FTab^[J].Data);
    end;
  end;
end;

procedure TPegtopVirtualHash.Clear(OnRemove: TPegtopHashDataEvent);
var
  I, J: Integer;
begin
  ProcessData(OnRemove);
  for I := 0 to FCount-1 do begin
    J := FTab^[I].Position;
    FTab^[J].State := phiEmpty;
  end;
  FCount := 0;
end;

function TPegtopVirtualHash.GetIndex(Key: Pointer): Integer;
var
  Start: Integer;
  I: Integer;
  A: Integer;
begin
  Result := -1;
  Start := GetHashCode(Key);
  I := Start;
  A := 1;
  while (Result = -1) and (FTab^[I].State > phiEmpty) do begin
    if (FTab^[I].State >= phiUsed) and (FOnEqual(FTab^[I].Key, Key)) then begin
      Result := I;
    end
    else begin
      I := (I + A) mod FSize;
      if I = Start then Exit;
      Inc(A, 2);
    end;
  end;
end;

function TPegtopVirtualHash.GetDataAt(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Data
  else
    Result := NIL;
end;

function TPegtopVirtualHash.GetKey(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Key
  else
    Result := NIL;
end;

function TPegtopVirtualHash.GetHashCode(Key: Pointer): Integer;
begin
  Result := FOnHashCode(Key);
end;

end.

