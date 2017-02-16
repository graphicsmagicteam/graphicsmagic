////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradientLists.pas
// Components: TPegtopColorGradientList, TPegtopColorGradientLibrary
// Classes:    TPegtopColorGradientItem, TPegtopColorGradientCollection,
//             TPegtopColorGradientListItem, TPegtopColorGradientListCollection
// Version:    1.03
// History:    1.00 06 Jul 2005 created
//             1.01 16 Aug 2005 TPegtopColorGradientLibrary added
//             1.02 13 Nov 2005 refactored
//             1.03 14 Nov 2005 file support added
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradientList is a non-visual component, which holds a collection
// of color gradients.
// TPegtopColorGradientLibrary is a non-visual component, which holds a
// collection of color gradient lists. So you can store different "books" each
// one containing different color gradients (with different categories) in a
// single component.
// These components act as models (for storage) and can be used for
// TPegtopColorGradientListBox, TPegtopColorGradientLibraryBox,
// TPegtopColorGradientManager and TPegtopColorGradientDialog.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradientLists;

interface

uses
  Classes, PegtopColorGradients, PegtopChunkFiles;

type
  TPegtopColorGradientList = class;

  TPegtopColorGradientItem = class(TCollectionItem)
  private
    FGradient: TPegtopCustomColorGradient;
    procedure GradientChange(Sender: TObject);
    procedure SetGradient(Value: TPegtopCustomColorGradient);
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Gradient: TPegtopCustomColorGradient read FGradient write SetGradient;
  end;

  TPegtopColorGradientItemEvent = procedure(Sender: TObject; Item: TPegtopColorGradientItem) of object;

  TPegtopColorGradientCollection = class(TCollection)
  private
    FName: String;
    FOwner: TPersistent;
    FOnChange: TPegtopColorGradientItemEvent;
    FModified: Boolean;
    procedure ReadFromStream(Stream: TStream; Merge: Boolean; DefaultName: String);
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure DefineGradientsChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure DefineGradientChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure SetName(Value: String);
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TPegtopColorGradientItem; reintroduce;
    procedure SetItem(Index: Integer; Value: TPegtopColorGradientItem); reintroduce;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AOnChange: TPegtopColorGradientItemEvent);
    procedure Assign(Source: TPersistent); override;
    function Add: TPegtopColorGradientItem;
    function Insert(Index: Integer): TPegtopColorGradientItem;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure AddFromStream(Stream: TStream);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    procedure AddFromFile(FileName: String);
    property Name: String read FName write SetName;
    property Items[Index: Integer]: TPegtopColorGradientItem read GetItem write SetItem; default;
    property Modified: Boolean read FModified write FModified;
  end;

  TPegtopCustomColorGradientList = class(TComponent)
  private
    FItems: TPegtopColorGradientCollection;
    FListeners: TList;
    FOnChange: TPegtopColorGradientItemEvent;
    procedure ItemsChange(Owner: TObject; Item: TPegtopColorGradientItem);
    procedure NotifyListeners;
    procedure SetItems(Value: TPegtopColorGradientCollection);
  protected
    property OnChange: TPegtopColorGradientItemEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddListener(Event: TNotifyEvent);
    procedure RemoveListener(Event: TNotifyEvent);
  published
    property Items: TPegtopColorGradientCollection read FItems write SetItems;
  end;

  TPegtopColorGradientList = class(TPegtopCustomColorGradientList)
  published
    property OnChange;
  end;

  TPegtopColorGradientLibrary = class;

  TPegtopColorGradientListItem = class(TCollectionItem)
  private
    FItems: TPegtopColorGradientCollection;
    FFileName: String;
    procedure ItemsChange(Owner: TObject; Item: TPegtopColorGradientItem);
    function GetName: String;
    procedure SetName(Value: String);
    procedure SetItems(Value: TPegtopColorGradientCollection);
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FileName: String read FFileName write FFileName;
  published
    property Name: String read GetName write SetName;
    property Items: TPegtopColorGradientCollection read FItems write SetItems;
  end;

  TPegtopColorGradientListItemEvent = procedure(Sender: TObject; ListItem: TPegtopColorGradientListItem; Item: TPegtopColorGradientItem) of object;

  TPegtopColorGradientListCollection = class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TPegtopColorGradientListItemEvent;
    procedure ItemChange(Owner: TObject; ListItem: TPegtopColorGradientListItem; Item: TPegtopColorGradientItem);
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TPegtopColorGradientListItem; reintroduce;
    procedure SetItem(Index: Integer; Value: TPegtopColorGradientListItem); reintroduce;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AOnChange: TPegtopColorGradientListItemEvent);
    function Add: TPegtopColorGradientListItem;
    function Insert(Index: Integer): TPegtopColorGradientListItem;
    procedure SaveAll;
    procedure FetchFileNames(Strings: TStrings);
    procedure AddFiles(Strings: TStrings);
    function AddFromFile(FileName: String): TPegtopColorGradientListItem;
    property Items[Index: Integer]: TPegtopColorGradientListItem read GetItem write SetItem; default;
  end;

  TPegtopCustomColorGradientLibrary = class(TComponent)
  private
    FItems: TPegtopColorGradientListCollection;
    FListeners: TList;
    FOnChange: TPegtopColorGradientListItemEvent;
    procedure ItemsChange(Owner: TObject; ListItem: TPegtopColorGradientListItem; Item: TPegtopColorGradientItem);
    procedure NotifyListeners;
    procedure SetItems(Value: TPegtopColorGradientListCollection);
  protected
    property OnChange: TPegtopColorGradientListItemEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddListener(Event: TNotifyEvent);
    procedure RemoveListener(Event: TNotifyEvent);
  published
    property Items: TPegtopColorGradientListCollection read FItems write SetItems;
  end;

  TPegtopColorGradientLibrary = class(TPegtopCustomColorGradientLibrary)
  published
    property OnChange;
  end;

implementation

uses
  SysUtils, Graphics;

type
  TPegtopGradientCollectionRecord = packed record
    Name: String[31];
  end;

const
  GradientCollectionChunkId:     TPegtopChunkId = 'XGRC';
  GradientCollectionDataChunkId: TPegtopChunkId = 'COLD';
  GradientsChunkId:              TPegtopChunkId = 'GRDS';
  GradientChunkId:               TPegtopChunkId = 'XGRD';

function ExtractFileNameWithoutExt(const S: String): String;
begin
  Result := ExtractFileName(S);
  Result := Copy(Result, 1, Length(Result) - Length(ExtractFileExt(Result)));
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientItem
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientItem.Create(Collection: TCollection);
begin
  FGradient := TPegtopColorGradient.Create([clBlack, clWhite]);
  FGradient.Name := 'Untitled';
  TPegtopColorGradient(FGradient).OnChange := GradientChange;
  inherited;
end;

destructor TPegtopColorGradientItem.Destroy;
begin
  FGradient.Free;
  inherited;
end;

procedure TPegtopColorGradientItem.Assign(Source: TPersistent);
begin
  if Source is TPegtopColorGradientItem then begin
    FGradient.Assign(TPegtopColorGradientItem(Source).FGradient);
  end
  else begin
    inherited; //raises an exception
  end;
end;

function TPegtopColorGradientItem.GetDisplayName: String;
begin
  Result := FGradient.Name;
end;

procedure TPegtopColorGradientItem.GradientChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TPegtopColorGradientItem.SetGradient(Value: TPegtopCustomColorGradient);
begin
  FGradient.Assign(Value);
  Changed(False);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientCollection
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientCollection.Create(AOwner: TPersistent; AOnChange: TPegtopColorGradientItemEvent);
begin
  FOwner := AOwner;
  FOnChange := AOnChange;
  inherited Create(TPegtopColorGradientItem);
end;

procedure TPegtopColorGradientCollection.Assign(Source: TPersistent);
begin
  if Source is TPegtopColorGradientCollection then begin
    FName := TPegtopColorGradientCollection(Source).FName;
    FModified := True;
  end;
  inherited;
end;

function TPegtopColorGradientCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPegtopColorGradientCollection.GetItem(Index: Integer): TPegtopColorGradientItem;
begin
  Result := TPegtopColorGradientItem(inherited GetItem(Index));
end;

procedure TPegtopColorGradientCollection.SetItem(Index: Integer; Value: TPegtopColorGradientItem);
begin
  FModified := True;
  inherited SetItem(Index, Value);
end;

procedure TPegtopColorGradientCollection.Update(Item: TCollectionItem);
begin
  inherited;
  FModified := True;
  if Assigned(FOnChange) then FOnChange(Self, TPegtopColorGradientItem(Item));
end;

function TPegtopColorGradientCollection.Add: TPegtopColorGradientItem;
begin
  Result := TPegtopColorGradientItem(inherited Add);
  FModified := True;
end;

function TPegtopColorGradientCollection.Insert(Index: Integer): TPegtopColorGradientItem;
begin
  Result := TPegtopColorGradientItem(inherited Insert(Index));
  FModified := True;
end;

procedure TPegtopColorGradientCollection.SaveToStream(Stream: TStream);
var
  P1, P2: Integer;
  R: TPegtopGradientCollectionRecord;
  I: Integer;
begin
  P1 := ForgetChunkHeader(Stream);
  FillChar(R, SizeOf(R), 0);
  R.Name := FName;
  WriteDataChunk(Stream, GradientCollectionDataChunkId, R, SizeOf(R));
  P2 := ForgetChunkHeader(Stream);
  for I := 0 to Count - 1 do begin
    Items[I].Gradient.SaveToStream(Stream);
  end;
  CompleteChunkHeader(Stream, P2, GradientsChunkId);
  CompleteChunkHeader(Stream, P1, GradientCollectionChunkId);
  FModified := False;
end;

procedure TPegtopColorGradientCollection.ReadFromStream(Stream: TStream; Merge: Boolean; DefaultName: String);
  procedure RaiseInvalid;
  begin
    raise EStreamError.Create('The color gradient file is not valid.');
  end;
var
  Reader: TPegtopContainerChunkReader;
  Id: TPegtopChunkId;
begin
  if Stream.Read(Id, SizeOf(Id)) <> SizeOf(Id) then begin
    RaiseInvalid;
  end
  else begin
    Stream.Seek(-SizeOf(Id), soFromCurrent);
    if CompareChunkIds(Id, GradientChunkId) or CompareChunkIds(Id, 'XGR1') then begin
      BeginUpdate;
      try
        if not Merge then Clear;
        with Add do begin
          Gradient.LoadFromStream(Stream);
          if Gradient.Name = '' then Gradient.Name := DefaultName;
        end;
      finally
        EndUpdate;
      end;
    end
    else if CompareChunkIds(Id, GradientCollectionChunkId) then begin
      BeginUpdate;
      try
        if not Merge then Clear;
        Reader := TPegtopContainerChunkReader.Create(NIL);
        try
          Reader.DefineDataChunk(GradientCollectionDataChunkId, SizeOf(TPegtopGradientCollectionRecord), ChunkReadData, [pcoUnique]);
          Reader.DefineContainerChunk(GradientsChunkId, DefineGradientsChunks, [pcoUnique]);
          if not Reader.Read(Stream, GradientCollectionChunkId) then RaiseInvalid;
        finally
          Reader.Free;
        end;
      finally
        EndUpdate;
      end;
    end
    else begin
      RaiseInvalid;
    end;
  end;
end;

procedure TPegtopColorGradientCollection.LoadFromStream(Stream: TStream);
begin
  ReadFromStream(Stream, False, '');
  FModified := False;
end;

procedure TPegtopColorGradientCollection.AddFromStream(Stream: TStream);
begin
  ReadFromStream(Stream, True, '');
  FModified := True;
end;

procedure TPegtopColorGradientCollection.ChunkReadData(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  FName := TPegtopGradientCollectionRecord(Data).Name;
end;

procedure TPegtopColorGradientCollection.DefineGradientsChunks(Sender: TObject;
  Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineContainerChunk(GradientChunkId, DefineGradientChunks, []);
end;

procedure TPegtopColorGradientCollection.DefineGradientChunks(Sender: TObject;
  Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  Add.Gradient.DefineChunks(Sender, Id, Index, ContainerChunkReader);
end;

procedure TPegtopColorGradientCollection.SaveToFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    if Name = '' then Name := ExtractFileNameWithoutExt(FileName);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPegtopColorGradientCollection.LoadFromFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ReadFromStream(Stream, False, ExtractFileNameWithoutExt(FileName));
  finally
    Stream.Free;
  end;
  FModified := False;
end;

procedure TPegtopColorGradientCollection.AddFromFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ReadFromStream(Stream, True, ExtractFileNameWithoutExt(FileName));
  finally
    Stream.Free;
  end;
  FModified := True;
end;

procedure TPegtopColorGradientCollection.SetName(Value: String);
begin
  // not more than 31 characters allowed
  // (due to streaming the name doesn't allow more)
  if Length(Value) > 31 then Value := Copy(Value, 1, 31);
  if FName <> Value then begin
    FName := Value;
    Changed;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorGradientList
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorGradientList.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TPegtopColorGradientCollection.Create(Self, ItemsChange);
  FListeners := TList.Create;
end;

destructor TPegtopCustomColorGradientList.Destroy;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    Listener.Free;
  end;
  FListeners.Free;
  FItems.Free;
  inherited;
end;

procedure TPegtopCustomColorGradientList.Assign(Source: TPersistent);
begin
  if Source is TPegtopCustomColorGradientList then begin
    FItems.Assign(TPegtopCustomColorGradientList(Source).Items);
  end
  else begin
    inherited; //raises an exception
  end;
end;

procedure TPegtopCustomColorGradientList.AddListener(Event: TNotifyEvent);
var
  Listener: TPegtopNotifyListener;
begin
  Listener := TPegtopNotifyListener.Create(Event);
  FListeners.Add(Listener);
end;

procedure TPegtopCustomColorGradientList.RemoveListener(Event: TNotifyEvent);
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := FListeners.Count - 1 downto 0 do begin
    Listener := FListeners[I];
    if (TMethod(Listener.Event).Code = TMethod(Event).Code)
    and (TMethod(Listener.Event).Data = TMethod(Event).Data) then
      FListeners.Delete(I);
  end;
end;

procedure TPegtopCustomColorGradientList.NotifyListeners;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    if Assigned(Listener.Event) then Listener.Event(Self);
  end;
end;

procedure TPegtopCustomColorGradientList.ItemsChange(Owner: TObject; Item: TPegtopColorGradientItem);
begin
  if Assigned(FOnChange) then FOnChange(Self, Item);
  NotifyListeners;
end;

procedure TPegtopCustomColorGradientList.SetItems(Value: TPegtopColorGradientCollection);
begin
  FItems.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientListItem
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientListItem.Create(Collection: TCollection);
begin
  inherited;
  FItems := TPegtopColorGradientCollection.Create(Self, ItemsChange);
end;

destructor TPegtopColorGradientListItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TPegtopColorGradientListItem.Assign(Source: TPersistent);
begin
  if Source is TPegtopColorGradientListItem then begin
    FItems.Assign(TPegtopColorGradientListItem(Source).FItems);
  end
  else begin
    inherited; //raises an exception
  end;
end;

function TPegtopColorGradientListItem.GetDisplayName: String;
begin
  Result := FItems.Name;
end;

procedure TPegtopColorGradientListItem.ItemsChange(Owner: TObject; Item: TPegtopColorGradientItem);
begin
  TPegtopColorGradientListCollection(Collection).ItemChange(Owner, Self, Item);
  FItems.Modified := True;
end;

function TPegtopColorGradientListItem.GetName: String;
begin
  Result := FItems.Name;
end;

procedure TPegtopColorGradientListItem.SetName(Value: String);
begin
  FItems.Name := Value;
end;

procedure TPegtopColorGradientListItem.SetItems(Value: TPegtopColorGradientCollection);
begin
  FItems.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientListCollection
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientListCollection.Create(AOwner: TPersistent; AOnChange: TPegtopColorGradientListItemEvent);
begin
  FOwner := AOwner;
  FOnChange := AOnChange;
  inherited Create(TPegtopColorGradientListItem);
end;

function TPegtopColorGradientListCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPegtopColorGradientListCollection.GetItem(Index: Integer): TPegtopColorGradientListItem;
begin
  Result := TPegtopColorGradientListItem(inherited GetItem(Index));
end;

procedure TPegtopColorGradientListCollection.SetItem(Index: Integer; Value: TPegtopColorGradientListItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TPegtopColorGradientListCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self, TPegtopColorGradientListItem(Item), NIL);
end;

function TPegtopColorGradientListCollection.Add: TPegtopColorGradientListItem;
begin
  Result := TPegtopColorGradientListItem(inherited Add);
end;

function TPegtopColorGradientListCollection.Insert(Index: Integer): TPegtopColorGradientListItem;
begin
  Result := TPegtopColorGradientListItem(inherited Insert(Index));
end;

procedure TPegtopColorGradientListCollection.SaveAll;
var
  I: Integer;
  Item: TPegtopColorGradientListItem;
begin
  for I := 0 to Count - 1 do begin
    Item := GetItem(I);
    if (Item.FileName <> '') and Item.Items.Modified then begin
      Item.Items.SaveToFile(Item.FileName);
    end;
  end;
end;

procedure TPegtopColorGradientListCollection.FetchFileNames(Strings: TStrings);
var
  I: Integer;
  Item: TPegtopColorGradientListItem;
begin
  Strings.Clear;
  for I := 0 to Count - 1 do begin
    Item := GetItem(I);
    if Item.FileName <> '' then Strings.Add(Item.FileName);
  end;
end;

procedure TPegtopColorGradientListCollection.AddFiles(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do begin
    try
      AddFromFile(Strings[I]);
    except
      // continue with next file
    end;
  end;
end;

function TPegtopColorGradientListCollection.AddFromFile(FileName: String): TPegtopColorGradientListItem;
begin
  Result := Add;
  try
    Result.FFileName := FileName;
    Result.Items.LoadFromFile(FileName);
    if Result.Name = '' then begin
      Result.Name := ExtractFileName(FileName);
      Result.Name := Copy(Result.Name, 1, Length(Result.Name) - Length(ExtractFileExt(Result.Name)));
    end;
  except
    Delete(Count - 1);
    raise;
  end;
end;

procedure TPegtopColorGradientListCollection.ItemChange(Owner: TObject; ListItem: TPegtopColorGradientListItem; Item: TPegtopColorGradientItem);
begin
  if Assigned(FOnChange) then FOnChange(Self, ListItem, Item);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorGradientLibrary
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorGradientLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TPegtopColorGradientListCollection.Create(Self, ItemsChange);
  FListeners := TList.Create;
end;

destructor TPegtopCustomColorGradientLibrary.Destroy;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    Listener.Free;
  end;
  FListeners.Free;
  FItems.Free;
  inherited;
end;

procedure TPegtopCustomColorGradientLibrary.AddListener(Event: TNotifyEvent);
var
  Listener: TPegtopNotifyListener;
begin
  Listener := TPegtopNotifyListener.Create(Event);
  FListeners.Add(Listener);
end;

procedure TPegtopCustomColorGradientLibrary.RemoveListener(Event: TNotifyEvent);
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := FListeners.Count - 1 downto 0 do begin
    Listener := FListeners[I];
    if (TMethod(Listener.Event).Code = TMethod(Event).Code)
    and (TMethod(Listener.Event).Data = TMethod(Event).Data) then
      FListeners.Delete(I);
  end;
end;

procedure TPegtopCustomColorGradientLibrary.NotifyListeners;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    if Assigned(Listener.Event) then Listener.Event(Self);
  end;
end;

procedure TPegtopCustomColorGradientLibrary.ItemsChange(Owner: TObject; ListItem: TPegtopColorGradientListItem; Item: TPegtopColorGradientItem);
begin
  if Assigned(FOnChange) then FOnChange(Self, ListItem, Item);
  NotifyListeners;
end;

procedure TPegtopCustomColorGradientLibrary.SetItems(Value: TPegtopColorGradientListCollection);
begin
  FItems.Assign(Value);
end;

end.
