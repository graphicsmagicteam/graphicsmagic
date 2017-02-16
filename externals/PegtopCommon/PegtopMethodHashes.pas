////////////////////////////////////////////////////////////////////////////////
// File:       PegtopMethodHashes.pas
// Classes:    TPegtopMethodHash
// Version:    1.01
// Date:       14 Apr 2004 creaded 1.00
//             25 Jan 2005 modified 1.01 (major bug in GetIndex fixed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopMethodHash can be used to hash method pointers (also known as
// procedures of object) consisting of an address for code and an address for
// data. Useful if you need a hash table for event handlers or window hooks (see
// PegtopWindowHooks.pas).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopMethodHashes;

interface

uses
  PegtopHashes, SysUtils;

type
  TPegtopMethodHashItem = record
    Position: Integer;
    Index: Integer;
    State: TPegtopHashItemState;
    Key: TMethod;
    Data: Pointer;
  end;

  TPegtopMethodHashItemArray = array[0..255] of TPegtopMethodHashItem;
  PPegtopMethodHashItemArray = ^TPegtopMethodHashItemArray;

  TPegtopMethodHash = class(TPegtopAbstractHash)
  private
    FTab: PPegtopMethodHashItemArray;
    function GetHashCode(Key: TMethod): Integer;
    function GetIndex(Key: TMethod): Integer;
    function GetData(Key: TMethod): Pointer;
    procedure SetData(Key: TMethod; V: Pointer);
    procedure AddItem(Key: TMethod; Data: Pointer);
    function GetDataAt(Index: Integer): Pointer;
    function GetKey(Index: Integer): TMethod;
  protected
    procedure ChangeSize(NewSize: Integer); override;
    function GetItemSize: Integer; override;
  public
    constructor Create(InitialCapacity: Integer = 100); override;
    destructor Destroy; override;
    procedure Assign(Hash: TPegtopMethodHash); virtual;
    function Contains(Key: TMethod): Boolean; virtual;
    function Add(Key: TMethod; Data: Pointer): Boolean; virtual;
    function Remove(Key: TMethod): Boolean; virtual;
    procedure ProcessData(OnProcess: TPegtopHashDataEvent);
    procedure Clear(OnRemove: TPegtopHashDataEvent);
    property Data[Key: TMethod]: Pointer read GetData write SetData; default;
    property DataAt[Index: Integer]: Pointer read GetDataAt;
    property Key[Index: Integer]: TMethod read GetKey;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopMethodHash
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMethodHash.Create(InitialCapacity: Integer = 100);
begin
  inherited Create(InitialCapacity);
  FTab := AllocMem(SizeOf(TPegtopMethodHashItem) * FSize);
end;

destructor TPegtopMethodHash.Destroy;
begin
  FreeMem(FTab, SizeOf(TPegtopMethodHashItem) * FSize);
  inherited;
end;

function TPegtopMethodHash.GetItemSize: Integer;
begin
  Result := SizeOf(TPegtopMethodHashItem);
end;

procedure TPegtopMethodHash.Assign(Hash: TPegtopMethodHash);
begin
  if Hash <> Self then begin
    FreeMem(FTab, SizeOf(TPegtopMethodHashItem) * FSize);
    FCapacity := Hash.FCapacity;
    FSize := Hash.FSize;
    FCount := Hash.FCount;
    GetMem(FTab, SizeOf(TPegtopMethodHashItem) * FSize);
    Move(Hash.FTab^, FTab^, SizeOf(TPegtopMethodHashItem) * FSize);
  end;
end;

procedure TPegtopMethodHash.ChangeSize(NewSize: Integer);
var
  OldTab: PPegtopMethodHashItemArray;
  OldCount, OldSize: Integer;
  I, J: Integer;
begin
  OldTab := FTab;
  OldCount := FCount;
  OldSize := FSize;
  FSize := NewSize;
  FCount := 0;
  FTab := AllocMem(SizeOf(TPegtopMethodHashItem) * FSize);
  for I := 0 to OldCount-1 do begin
    J := OldTab^[I].Position;
    AddItem(OldTab^[J].Key, OldTab^[J].Data);
  end;
  FreeMem(OldTab, SizeOf(TPegtopMethodHashItem) * OldSize);
end;

function TPegtopMethodHash.Contains(Key: TMethod): Boolean;
begin
  Result := GetIndex(Key) >= 0;
end;

function TPegtopMethodHash.GetData(Key: TMethod): Pointer;
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

procedure TPegtopMethodHash.SetData(Key: TMethod; V: Pointer);
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

procedure TPegtopMethodHash.AddItem(Key: TMethod; Data: Pointer);
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

function TPegtopMethodHash.Add(Key: TMethod; Data: Pointer): Boolean;
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

function TPegtopMethodHash.Remove(Key: TMethod): Boolean;
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

procedure TPegtopMethodHash.ProcessData(OnProcess: TPegtopHashDataEvent);
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

procedure TPegtopMethodHash.Clear(OnRemove: TPegtopHashDataEvent);
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

function TPegtopMethodHash.GetIndex(Key: TMethod): Integer;
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
    and (FTab^[I].Key.Code = Key.Code)
    and (FTab^[I].Key.Data = Key.Data) then begin
      Result := I;
    end
    else begin
      I := (I + A) mod FSize;
      if I = Start then Exit;
      Inc(A, 2);
    end;
  end;
end;

function TPegtopMethodHash.GetDataAt(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Data
  else
    Result := NIL;
end;

function TPegtopMethodHash.GetKey(Index: Integer): TMethod;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FTab^[FTab^[Index].Position].Key
  else
    with Result do begin Code := NIL; Data := NIL; end;
end;

function TPegtopMethodHash.GetHashCode(Key: TMethod): Integer;
begin
  Result := Cardinal(Integer(Key.Code) * 1664525 + Integer(Key.Data)) mod Cardinal(FSize);
  // use Cardinal (instead of Integer) to force insigned mod
end;

end.
