////////////////////////////////////////////////////////////////////////////////
// File:       PegtopClassHashes.pas
// Classes:    TPegtopClassHash
// Version:    1.00
// Date:       03 May 2005 creaded 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopClassHash can be used to hash class types.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopClassHashes;

interface

uses
  PegtopHashes, SysUtils;

type
  TPegtopClassHashItem = record
    Position: Integer;
    Index: Integer;
    State: TPegtopHashItemState;
    Key: TClass;
    Data: Pointer;
  end;

  TPegtopClassHashItemArray = array[0..255] of TPegtopClassHashItem;
  PPegtopClassHashItemArray = ^TPegtopClassHashItemArray;

  TPegtopClassHash = class(TPegtopAbstractHash)
  private
    FTab: PPegtopClassHashItemArray;
    function GetHashCode(Key: TClass): Integer;
    function GetIndex(Key: TClass): Integer;
    function GetData(Key: TClass): Pointer;
    procedure SetData(Key: TClass; V: Pointer);
    procedure AddItem(Key: TClass; Data: Pointer);
    function GetKey(Index: Integer): TClass;
  protected
    procedure ChangeSize(NewSize: Integer); override;
    function GetItemSize: Integer; override;
  public
    constructor Create(InitialCapacity: Integer = 100); override;
    destructor Destroy; override;
    procedure Assign(Hash: TPegtopClassHash); virtual;
    function Contains(Key: TClass): Boolean; virtual;
    function Add(Key: TClass; Data: Pointer): Boolean; virtual;
    function Remove(Key: TClass): Boolean; virtual;
    procedure ProcessData(OnProcess: TPegtopHashDataEvent);
    procedure Clear(OnRemove: TPegtopHashDataEvent);
    property Data[Key: TClass]: Pointer read GetData write SetData; default;
    property Key[Index: Integer]: TClass read GetKey;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopClassHash
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopClassHash.Create(InitialCapacity: Integer = 100);
begin
  inherited Create(InitialCapacity);
  FTab := AllocMem(SizeOf(TPegtopClassHashItem) * FSize);
end;

destructor TPegtopClassHash.Destroy;
begin
  FreeMem(FTab, SizeOf(TPegtopClassHashItem) * FSize);
  inherited;
end;

function TPegtopClassHash.GetItemSize: Integer;
begin
  Result := SizeOf(TPegtopClassHashItem);
end;

procedure TPegtopClassHash.Assign(Hash: TPegtopClassHash);
begin
  if Hash <> Self then begin
    FreeMem(FTab, SizeOf(TPegtopClassHashItem) * FSize);
    FCapacity := Hash.FCapacity;
    FSize := Hash.FSize;
    FCount := Hash.FCount;
    GetMem(FTab, SizeOf(TPegtopClassHashItem) * FSize);
    Move(Hash.FTab^, FTab^, SizeOf(TPegtopClassHashItem) * FSize);
  end;
end;

procedure TPegtopClassHash.ChangeSize(NewSize: Integer);
var
  OldTab: PPegtopClassHashItemArray;
  OldCount, OldSize: Integer;
  I, J: Integer;
begin
  OldTab := FTab;
  OldCount := FCount;
  OldSize := FSize;
  FSize := NewSize;
  FCount := 0;
  FTab := AllocMem(SizeOf(TPegtopClassHashItem) * FSize);
  for I := 0 to OldCount-1 do begin
    J := OldTab^[I].Position;
    AddItem(OldTab^[J].Key, OldTab^[J].Data);
  end;
  FreeMem(OldTab, SizeOf(TPegtopClassHashItem) * OldSize);
end;

function TPegtopClassHash.Contains(Key: TClass): Boolean;
begin
  Result := GetIndex(Key) >= 0;
end;

function TPegtopClassHash.GetData(Key: TClass): Pointer;
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

procedure TPegtopClassHash.SetData(Key: TClass; V: Pointer);
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

procedure TPegtopClassHash.AddItem(Key: TClass; Data: Pointer);
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

function TPegtopClassHash.Add(Key: TClass; Data: Pointer): Boolean;
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

function TPegtopClassHash.Remove(Key: TClass): Boolean;
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

procedure TPegtopClassHash.ProcessData(OnProcess: TPegtopHashDataEvent);
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

procedure TPegtopClassHash.Clear(OnRemove: TPegtopHashDataEvent);
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

function TPegtopClassHash.GetIndex(Key: TClass): Integer;
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
    if (FTab^[I].State >= phiUsed)
    and (Pointer(FTab^[I].Key) = Pointer(Key)) then begin
      Result := I;
    end
    else begin
      I := (I + A) mod FSize;
      if I = Start then Exit;
      Inc(A, 2);
    end;
  end;
end;

function TPegtopClassHash.GetKey(Index: Integer): TClass;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Key
  else
    Result := NIL;
end;

function TPegtopClassHash.GetHashCode(Key: TClass): Integer;
begin
  Result := Cardinal(Key) mod Cardinal(FSize);
end;

end.
