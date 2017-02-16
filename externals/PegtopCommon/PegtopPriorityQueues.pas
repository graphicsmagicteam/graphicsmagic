////////////////////////////////////////////////////////////////////////////////
// File:       PegtopPriorityQueues.pas
// Classes:    TPegtopAbstractPriorityQueue, TPegtopIntPriorityQueue,
//             TPegtopFloatPriorityQueue
// Version:    1.01
// Date:       16 Jun 2004 1.00
//             25 Sep 2005 1.01 (direct item data access added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopIntPriorityQueue and TPegtopFloatPriorityQueue are priority queues
// for integers or floating point variables (associated with some data pointer).
////////////////////////////////////////////////////////////////////////////////
// Usage:
// Use Push to add another item, Pop to retrieve the item with the highest
// priority (or lowest priority depending on the Order property). Peek return
// the item with the highest priority without removing it from the queue. Remove
// can be used to remove any item (with known data pointer). Repulse changes the
// priority of the first item (the one Peek returns), which basically equals
// a Pop and a Push of the same item with other priority (but is faster).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopPriorityQueues;

interface

type
  TPegtopIntPriorityItem = record
    Priority: Integer;
    Data: Pointer;
  end;

  TPegtopFloatPriorityItem = record
    Priority: Double;
    Data: Pointer;
  end;

  TPegtopPriorityOrder = (poHighestFirst, poLowestFirst);

  TPegtopPriorityQueueDataEvent = procedure(Sender: TObject; Data: Pointer) of object;

  TPegtopAbstractPriorityQueue = class
  private
    FCount: Integer;
    FCapacity: Integer;
    FOrder: TPegtopPriorityOrder;
    procedure SetCapacity(Value: Integer);
    procedure SetOrder(Value: TPegtopPriorityOrder);
  protected
    procedure BuildHeap;
    procedure SetCount(Value: Integer);
    function IndexOf(Data: Pointer): Integer; virtual; abstract;
    procedure ChangeCapacity(NewCapacity: Integer); virtual; abstract;
    procedure DownHeap(Index: Integer); virtual; abstract;
    procedure UpHeap(Index: Integer); virtual; abstract;
    function GetData(Index: Integer): Pointer; virtual; abstract;
  public
    constructor Create(InitialCapacity: Integer = 15); virtual;
    function Delete(Index: Integer): Boolean; virtual; abstract;
    function Remove(Data: Pointer): Boolean;
    procedure Clear(OnRemove: TPegtopPriorityQueueDataEvent); virtual; abstract;
    procedure ProcessData(OnProcess: TPegtopPriorityQueueDataEvent); virtual; abstract;
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Order: TPegtopPriorityOrder read FOrder write SetOrder;
    property Data[Index: Integer]: Pointer read GetData;
  end;

  TPegtopIntPriorityQueue = class(TPegtopAbstractPriorityQueue)
  private
    FTab: array of TPegtopIntPriorityItem;
    function HigherPriority(Prio1, Prio2: Integer): Boolean;
    function GetHighestPriority: Integer;
  protected
    function IndexOf(Data: Pointer): Integer; override;
    procedure ChangeCapacity(NewCapacity: Integer); override;
    procedure DownHeap(Index: Integer); override;
    procedure UpHeap(Index: Integer); override;
    function GetData(Index: Integer): Pointer; override;
  public
    procedure Push(Priority: Integer; Data: Pointer);
    function Pop: Pointer;
    function Peek: Pointer;
    function Delete(Index: Integer): Boolean; override;
    procedure Repulse(Priority: Integer);
    procedure ProcessData(OnProcess: TPegtopPriorityQueueDataEvent); override;
    procedure Clear(OnRemove: TPegtopPriorityQueueDataEvent); override;
    property HighestPriority: Integer read GetHighestPriority;
  end;

  TPegtopFloatPriorityQueue = class(TPegtopAbstractPriorityQueue)
  private
    FTab: array of TPegtopFloatPriorityItem;
    function HigherPriority(Prio1, Prio2: Double): Boolean;
    function GetHighestPriority: Double;
  protected
    function IndexOf(Data: Pointer): Integer; override;
    procedure ChangeCapacity(NewCapacity: Integer); override;
    procedure DownHeap(Index: Integer); override;
    procedure UpHeap(Index: Integer); override;
    function GetData(Index: Integer): Pointer; override;
  public
    procedure Push(Priority: Double; Data: Pointer);
    function Pop: Pointer;
    function Peek: Pointer;
    function Delete(Index: Integer): Boolean; override;
    procedure Repulse(Priority: Double);
    procedure ProcessData(OnProcess: TPegtopPriorityQueueDataEvent); override;
    procedure Clear(OnRemove: TPegtopPriorityQueueDataEvent); override;
    property HighestPriority: Double read GetHighestPriority;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopAbstractPriorityQueue
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopAbstractPriorityQueue.Create(InitialCapacity: Integer = 15);
begin
  FCapacity := 0;
  FCount := 0;
  FOrder := poHighestFirst;
  SetCapacity(InitialCapacity);
end;

function TPegtopAbstractPriorityQueue.Remove(Data: Pointer): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(Data);
  Result := Index >= 0;
  if Result then Delete(Index);
end;

procedure TPegtopAbstractPriorityQueue.SetCapacity(Value: Integer);
begin
  if (Value <> FCapacity) and (Value >= FCount) then begin
    ChangeCapacity(Value);
    FCapacity := Value;
  end;
end;

procedure TPegtopAbstractPriorityQueue.SetCount(Value: Integer);
begin
  FCount := Value;
  if FCount > FCapacity then SetCapacity(FCount*2-1);
end;

procedure TPegtopAbstractPriorityQueue.BuildHeap;
var
  Index: Integer;
begin
  Index := (FCount-1) div 2; // begin with parent of last item
  while Index >= 0 do begin
    DownHeap(Index); // transform subtree to heap
    Dec(Index);
  end;
end;

procedure TPegtopAbstractPriorityQueue.SetOrder(Value: TPegtopPriorityOrder);
begin
  if FOrder <> Value then begin
    FOrder := Value;
    BuildHeap;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopIntPriorityQueue
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopIntPriorityQueue.ChangeCapacity(NewCapacity: Integer);
begin
  SetLength(FTab, NewCapacity);
end;

function TPegtopIntPriorityQueue.HigherPriority(Prio1, Prio2: Integer): Boolean;
begin
  if Order = poHighestFirst then begin
    Result := Prio1 >= Prio2;
  end
  else begin
    Result := Prio1 <= Prio2;
  end;
end;

function TPegtopIntPriorityQueue.IndexOf(Data: Pointer): Integer;
begin
  Result := 0;
  while (Result < Count) and (FTab[Result].Data <> Data) do Inc(Result);
  if Result >= Count then Result := -1;
end;

procedure TPegtopIntPriorityQueue.DownHeap(Index: Integer);
var
  Child: Integer;
  Temp: TPegtopIntPriorityItem;
begin
  Temp := FTab[Index];
  Child := 2 * Index + 1; // left Child
  while (Child < Count) do begin
    if (Child+1 < Count) then begin // right child existing?
      if HigherPriority(FTab[Child+1].Priority, FTab[Child].Priority) then Inc(Child);
    end;
    if HigherPriority(Temp.Priority, FTab[Child].Priority) then begin
      // correct position reached
      FTab[Index] := Temp; // move item to correct position (>= old position)
      Exit;
    end;
    FTab[Index] := FTab[Child]; // move child up
    Index := Child;
    Child := 2 * Index + 1; // left Child
  end;
  FTab[Index] := Temp; // move item to correct position (>= old position)
end;

procedure TPegtopIntPriorityQueue.UpHeap(Index: Integer);
var
  Parent: Integer;
  Temp: TPegtopIntPriorityItem;
begin
  Temp := FTab[Index];
  while Index > 0 do begin
    Parent := (Index-1) div 2;
    if HigherPriority(FTab[Parent].Priority, Temp.Priority) then begin
      // correct position reached
      FTab[Index] := Temp; // move item to correct position (<= old position)
      exit;
    end;
    FTab[Index] := FTab[Parent]; // move parent down
    Index := Parent;
  end;
  FTab[Index] := Temp; // move item to correct position (<= old position)
end;

procedure TPegtopIntPriorityQueue.Push(Priority: Integer; Data: Pointer);
begin
  SetCount(Count+1);
  // move new element to end of list:
  FTab[Count-1].Priority := Priority;
  FTab[Count-1].Data := Data;
  // move it from end of list to correct position:
  UpHeap(Count-1);
end;

function TPegtopIntPriorityQueue.Pop: Pointer;
begin
  if Count > 0 then begin
    Result := FTab[0].Data;
    // move last element to top:
    FTab[0] := FTab[Count-1];
    SetCount(Count-1);
    // move it from top to correct position:
    DownHeap(0);
  end
  else begin
    Result := NIL;
  end;
end;

function TPegtopIntPriorityQueue.Peek: Pointer;
begin
  if Count > 0 then Result := FTab[0].Data
  else Result := NIL;
end;

function TPegtopIntPriorityQueue.GetHighestPriority: Integer;
begin
  if Count > 0 then Result := FTab[0].Priority
  else Result := 0;
end;

procedure TPegtopIntPriorityQueue.Repulse(Priority: Integer);
begin
  if Count > 0 then begin
    // change priority of top item:
    FTab[0].Priority := Priority;
    // move it from top to correct position:
    DownHeap(0);
  end;
end;

function TPegtopIntPriorityQueue.Delete(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
  if Result then begin
    // move last element to index:
    FTab[Index] := FTab[Count-1];
    SetCount(Count-1);
    // move it from index to correct position:
    DownHeap(Index);
  end;
end;

procedure TPegtopIntPriorityQueue.ProcessData(OnProcess: TPegtopPriorityQueueDataEvent);
var
  Index: Integer;
begin
  if Assigned(OnProcess) then begin
    for Index := 0 to Count-1 do OnProcess(Self, FTab[Index].Data);
  end;
end;

procedure TPegtopIntPriorityQueue.Clear(OnRemove: TPegtopPriorityQueueDataEvent);
begin
  ProcessData(OnRemove);
  SetCount(0);
end;

function TPegtopIntPriorityQueue.GetData(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < Count) then begin
    Result := FTab[Index].Data;
  end
  else begin
    Result := NIL;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopFloatPriorityQueue
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopFloatPriorityQueue.ChangeCapacity(NewCapacity: Integer);
begin
  SetLength(FTab, NewCapacity);
end;

function TPegtopFloatPriorityQueue.HigherPriority(Prio1, Prio2: Double): Boolean;
begin
  if Order = poHighestFirst then begin
    Result := Prio1 >= Prio2;
  end
  else begin
    Result := Prio1 <= Prio2;
  end;
end;

function TPegtopFloatPriorityQueue.IndexOf(Data: Pointer): Integer;
begin
  Result := 0;
  while (Result < Count) and (FTab[Result].Data <> Data) do Inc(Result);
  if Result >= Count then Result := -1;
end;

procedure TPegtopFloatPriorityQueue.DownHeap(Index: Integer);
var
  Child: Integer;
  Temp: TPegtopFloatPriorityItem;
begin
  Temp := FTab[Index];
  Child := 2 * Index + 1; // left Child
  while (Child < Count) do begin
    if (Child+1 < Count) then begin // right child existing?
      if HigherPriority(FTab[Child+1].Priority, FTab[Child].Priority) then Inc(Child);
    end;
    if HigherPriority(Temp.Priority, FTab[Child].Priority) then begin
      // correct position reached
      FTab[Index] := Temp; // move item to correct position (>= old position)
      Exit;
    end;
    FTab[Index] := FTab[Child]; // move child up
    Index := Child;
    Child := 2 * Index + 1; // left Child
  end;
  FTab[Index] := Temp; // move item to correct position (>= old position)
end;

procedure TPegtopFloatPriorityQueue.UpHeap(Index: Integer);
var
  Parent: Integer;
  Temp: TPegtopFloatPriorityItem;
begin
  Temp := FTab[Index];
  while Index > 0 do begin
    Parent := (Index-1) div 2;
    if HigherPriority(FTab[Parent].Priority, Temp.Priority) then begin
      // correct position reached
      FTab[Index] := Temp; // move item to correct position (<= old position)
      exit;
    end;
    FTab[Index] := FTab[Parent]; // move parent down
    Index := Parent;
  end;
  FTab[Index] := Temp; // move item to correct position (<= old position)
end;

procedure TPegtopFloatPriorityQueue.Push(Priority: Double; Data: Pointer);
begin
  SetCount(Count+1);
  // move new element to end of list:
  FTab[Count-1].Priority := Priority;
  FTab[Count-1].Data := Data;
  // move it from end of list to correct position:
  UpHeap(Count-1);
end;

function TPegtopFloatPriorityQueue.Pop: Pointer;
begin
  if Count > 0 then begin
    Result := FTab[0].Data;
    // move last element to top:
    FTab[0] := FTab[Count-1];
    SetCount(Count-1);
    // move it from top to correct position:
    DownHeap(0);
  end
  else begin
    Result := NIL;
  end;
end;

function TPegtopFloatPriorityQueue.Peek: Pointer;
begin
  if Count > 0 then Result := FTab[0].Data
  else Result := NIL;
end;

function TPegtopFloatPriorityQueue.GetHighestPriority: Double;
begin
  if Count > 0 then Result := FTab[0].Priority
  else Result := 0.0;
end;

procedure TPegtopFloatPriorityQueue.Repulse(Priority: Double);
begin
  if Count > 0 then begin
    // change priority of top item:
    FTab[0].Priority := Priority;
    // move it from top to correct position:
    DownHeap(0);
  end;
end;

function TPegtopFloatPriorityQueue.Delete(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
  if Result then begin
    // move last element to index:
    FTab[Index] := FTab[Count-1];
    SetCount(Count-1);
    // move it from index to correct position:
    DownHeap(Index);
  end;
end;

procedure TPegtopFloatPriorityQueue.ProcessData(OnProcess: TPegtopPriorityQueueDataEvent);
var
  Index: Integer;
begin
  if Assigned(OnProcess) then begin
    for Index := 0 to Count-1 do OnProcess(Self, FTab[Index].Data);
  end;
end;

procedure TPegtopFloatPriorityQueue.Clear(OnRemove: TPegtopPriorityQueueDataEvent);
begin
  ProcessData(OnRemove);
  SetCount(0);
end;

function TPegtopFloatPriorityQueue.GetData(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < Count) then begin
    Result := FTab[Index].Data;
  end
  else begin
    Result := NIL;
  end;
end;

end.
