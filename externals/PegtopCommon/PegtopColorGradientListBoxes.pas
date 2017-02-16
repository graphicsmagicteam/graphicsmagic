////////////////////////////////////////////////////////////////////////////////
// Filename:   PegtopColorGradientListBoxes.pas
// Components: TPegtopColorGradientListBox
// Version:    1.01
// History:    1.00 06 Jun 2005 created
//             1.01 14 Nov 2005 popup menu added
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradientListBox is a list box to display color gradients.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradientListBoxes;

interface

uses
  Windows, Messages, Classes, Graphics, Forms, Controls, StdCtrls,
  PegtopVirtualListBoxes, PegtopColorGradients, PegtopColorGradientLists,
  PegtopColorControls, PegtopComboBoxes;

type
  TPegtopColorGradientEvent = procedure (Sender: TObject; Gradient: TPegtopCustomColorGradient) of object;

  TPegtopCustomColorGradientListBox = class(TPegtopCustomVirtualListBox)
  private
    FCollection: TPegtopColorGradientCollection;
    FLook: TPegtopColorControlLook;
    FOnApply: TNotifyEvent;
    FOnAddNew: TPegtopColorGradientEvent;
    FOnChange: TNotifyEvent;
    procedure SetCollection(Value: TPegtopColorGradientCollection);
    procedure SetLook(Value: TPegtopColorControlLook);
  protected
    procedure PopupContextMenu(const X, Y: Integer); virtual;
    procedure ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState); override;
    procedure ItemMeasure(Index: Integer; var ItemHeight: Integer); override;
    procedure ItemMoved(FromIndex, ToIndex: Integer); override;
    procedure DblClick; override;
    property Collection: TPegtopColorGradientCollection read FCollection write SetCollection;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
    property OnAddNew: TPegtopColorGradientEvent read FOnAddNew write FOnAddNew;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Look: TPegtopColorControlLook read FLook write SetLook default pclRoundedRect;
  end;

  TPegtopColorGradientListBox = class(TPegtopCustomColorGradientListBox)
  private
    FGradients: TPegtopCustomColorGradientList;
    procedure GradientsChange(Sender: TObject);
    procedure SetGradients(Value: TPegtopCustomColorGradientList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property ItemIndex;
    destructor Destroy; override;
  published
    property Gradients: TPegtopCustomColorGradientList read FGradients write SetGradients;
    property OnApply;
    property OnChange;
    property Options;
    property SeparatorSize;
    property SeparatorColor;
    property SeparatorDisabled;
    property BorderStyle;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnItemDraw;
    property OnItemMeasure;
    property OnItemMoved;
    property OnItemMouseMove;
    property OnItemMouseDown;
    property OnItemMouseUp;
    property OnItemMouseEnter;
    property OnItemMouseLeave;
    property OnItemSelected;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TPegtopColorGradientLibraryBox = class(TPegtopCustomColorGradientListBox)
  private
    FGradientLibrary: TPegtopCustomColorGradientLibrary;
    FGradientLibraryIndex: Integer;
    FOnGradientLibraryIndexChange: TNotifyEvent;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure GradientsChange(Sender: TObject);
    procedure SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
    procedure SetGradientLibraryIndex(Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopupContextMenu(const X, Y: Integer); override;
    procedure ItemMoved(FromIndex, ToIndex: Integer); override;
    function MovingAllowed: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowContextMenu;
  published
    property GradientLibrary: TPegtopCustomColorGradientLibrary read FGradientLibrary write SetGradientLibrary;
    property GradientLibraryIndex: Integer read FGradientLibraryIndex write SetGradientLibraryIndex;
    property OnGradientLibraryIndexChange: TNotifyEvent read FOnGradientLibraryIndexChange write FOnGradientLibraryIndexChange;
    property OnApply;
    property OnAddNew;
    property OnChange;
    property Options;
    property SeparatorSize;
    property SeparatorColor;
    property SeparatorDisabled;
    property BorderStyle;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnItemDraw;
    property OnItemMeasure;
    property OnItemMoved;
    property OnItemMouseMove;
    property OnItemMouseDown;
    property OnItemMouseUp;
    property OnItemMouseEnter;
    property OnItemMouseLeave;
    property OnItemSelected;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TPegtopColorGradientManager = class(TCustomControl)
  private
    FComboBox: TPegtopComboBox;
    FGradientLibraryBox: TPegtopColorGradientLibraryBox;
    FOnSelect: TNotifyEvent;
    FOnApply: TNotifyEvent;
    FOnAddNew: TPegtopColorGradientEvent;
    procedure RefreshComboBox;
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxGetItemStyle(Sender: TObject; Index: Integer;
      State: TOwnerDrawState; var Style: TPegtopComboBoxItemStyle);
    procedure GradientLibraryIndexChange(Sender: TObject);
    procedure GradientLibraryBoxItemSelected(Sender: TObject; Index: Integer);
    procedure GradientLibraryBoxApply(Sender: TObject);
    procedure GradientLibraryBoxAddNew(Sender: TObject; NewGradient: TPegtopCustomColorGradient);
    procedure GradientLibraryBoxChange(Sender: TObject);
    function GetGradientLibrary: TPegtopCustomColorGradientLibrary;
    procedure SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
    function GetGradientLibraryIndex: Integer;
    procedure SetGradientLibraryIndex(Value: Integer);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetSelected: TPegtopCustomColorGradient;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GradientLibrary: TPegtopCustomColorGradientLibrary read GetGradientLibrary write SetGradientLibrary;
    property GradientLibraryIndex: Integer read GetGradientLibraryIndex write SetGradientLibraryIndex;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Selected: TPegtopCustomColorGradient read GetSelected;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
    property OnAddNew: TPegtopColorGradientEvent read FOnAddNew write FOnAddNew;
  end;

implementation

uses
  Menus, Dialogs, SysUtils,
  PegtopColorUtils, PegtopColorServices, PegtopColorGradientFileDialogs;

type
  TPegtopLibraryBoxPopupMenu = class(TPopupMenu)
  private
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
    procedure DoCommand(LibraryBox: TPegtopColorGradientLibraryBox; Command: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TRenameForm = class(TForm)
  private
    FInfoLabel: TLabel;
    FNameEdit: TEdit;
  public
    procedure AfterConstruction; override;
    property InfoLabel: TLabel read FInfoLabel;
    property NameEdit: TEdit read FNameEdit;
  end;

const
  ListMenuItem = 7;

var
  LibraryBoxPopupMenu: TPegtopLibraryBoxPopupMenu;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorGradientListBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorGradientListBox.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FLook := pclRoundedRect;
end;

procedure TPegtopCustomColorGradientListBox.PopupContextMenu(const X, Y: Integer);
begin
end;

procedure TPegtopCustomColorGradientListBox.DblClick;
begin
  inherited;
  if Assigned(FOnApply) then FOnApply(Self);
end;

procedure TPegtopCustomColorGradientListBox.ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState);
var
  Gradient: TPegtopCustomColorGradient;
  TempBitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  TextRect: TRect;
begin
  if Assigned(FCollection) then begin
    Gradient := FCollection[Index].Gradient;
    // Canvas.Brush.Style := bsClear;
    // Canvas.Rectangle(ItemRect.Left + 1, ItemRect.Top + 1, ItemRect.Right - 1, ItemRect.Top + 23);
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.PixelFormat := pf32bit;
      TempBitmap.Width := ItemRect.Right - ItemRect.Left - 2;
      TempBitmap.Height := 22;
      Origin := TempBitmap.ScanLine[0];
      Pitch := Integer(TempBitmap.ScanLine[1]) - Integer(Origin);
      PegtopDrawSolidRect32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        PegtopColor($FFFFFF), 0, NIL, 1, 1, Rect(0, 0, TempBitmap.Width - 1, TempBitmap.Height - 1));
      Gradient.BlendDithered32(Origin, Pitch, Rect(1, 1, TempBitmap.Width - 1, TempBitmap.Height - 1),
        Point(1, 1), Point(TempBitmap.Width - 2, 1));
      PegtopDrawBounds32(Origin, Pitch, Rect(0, 0, TempBitmap.Width, TempBitmap.Height),
        Rect(0, 0, TempBitmap.Width, TempBitmap.Height), FLook, $00000000, Canvas.Brush.Color);
      Canvas.Draw(ItemRect.Left + 1, ItemRect.Top + 1, TempBitmap);
    finally
      TempBitmap.Free;
    end;
    TextRect := Rect(ItemRect.Left + 2, ItemRect.Top + 24, ItemRect.Right - 2, ItemRect.Bottom);
    DrawText(Canvas.Handle, PChar(Gradient.Name), Length(Gradient.Name),
      TextRect, DT_TOP or DT_CENTER or DT_END_ELLIPSIS or DT_NOPREFIX or DT_SINGLELINE);
  end;
  inherited;
end;

procedure TPegtopCustomColorGradientListBox.ItemMeasure(Index: Integer; var ItemHeight: Integer);
begin
  ItemHeight := 40;
  inherited;
end;

procedure TPegtopCustomColorGradientListBox.ItemMoved(FromIndex, ToIndex: Integer);
begin
  if Assigned(FCollection) then begin
    if ToIndex > FromIndex then
      FCollection[FromIndex].Index := ToIndex - 1
    else
      FCollection[FromIndex].Index := ToIndex;
  end;
end;

procedure TPegtopCustomColorGradientListBox.SetCollection(Value: TPegtopColorGradientCollection);
begin
  if FCollection <> Value then begin
    ClearItems;
    FCollection := Value;
    if FCollection <> NIL then
      AddItems(FCollection.Count);
  end;
end;

procedure TPegtopCustomColorGradientListBox.SetLook(Value: TPegtopColorControlLook);
begin
  if FLook <> Value then begin
    FLook := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientListBox
////////////////////////////////////////////////////////////////////////////////

destructor TPegtopColorGradientListBox.Destroy;
begin
  SetGradients(NIL); // remove listeners
  inherited;
end;

procedure TPegtopColorGradientListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  If (Operation = opRemove) and (AComponent = FGradients) then SetGradients(NIL);
  inherited;
end;

procedure TPegtopColorGradientListBox.GradientsChange(Sender: TObject);
begin
  Collection := NIL;
  Collection := FGradients.Items;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPegtopColorGradientListBox.SetGradients(Value: TPegtopCustomColorGradientList);
begin
  if FGradients <> Value then begin
    Collection := NIL;
    if Assigned(FGradients) then begin
      FGradients.RemoveListener(GradientsChange);
      FGradients.RemoveFreeNotification(Self);
    end;
    FGradients := Value;
    if Assigned(FGradients) then begin
      Collection := FGradients.Items;
      FGradients.FreeNotification(Self);
      FGradients.AddListener(GradientsChange);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientLibraryBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientLibraryBox.Create(AOwner: TComponent);
begin
  inherited;
  FGradientLibraryIndex := -1;
end;

destructor TPegtopColorGradientLibraryBox.Destroy;
begin
  SetGradientLibrary(NIL); // remove listeners
  inherited;
end;

procedure TPegtopColorGradientLibraryBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  If (Operation = opRemove) and (AComponent = FGradientLibrary) then SetGradientLibrary(NIL);
  inherited;
end;

procedure TPegtopColorGradientLibraryBox.ItemMoved(FromIndex, ToIndex: Integer);
begin
  if (FGradientLibrary <> NIL) and (FGradientLibraryIndex <> -1) then begin
    if ToIndex > FromIndex then
      FGradientLibrary.Items[FGradientLibraryIndex].Items[FromIndex].Index := ToIndex - 1
    else
      FGradientLibrary.Items[FGradientLibraryIndex].Items[FromIndex].Index := ToIndex;
  end;
end;

function TPegtopColorGradientLibraryBox.MovingAllowed: Boolean;
var
  LibraryItem: TPegtopColorGradientListItem;
begin
  if FGradientLibraryIndex >= 0 then LibraryItem := FGradientLibrary.Items[FGradientLibraryIndex] else LibraryItem := NIL;
  Result := Assigned(LibraryItem) and ((LibraryItem.FileName <> '') or (csDesigning in ComponentState));
end;

procedure TPegtopColorGradientLibraryBox.GradientsChange(Sender: TObject);
begin
  Collection := NIL;
  if FGradientLibrary = NIL then FGradientLibraryIndex := -1
  else if FGradientLibraryIndex > FGradientLibrary.Items.Count - 1 then FGradientLibraryIndex := FGradientLibrary.Items.Count - 1;
  if (FGradientLibraryIndex >= 0) and (FGradientLibraryIndex < FGradientLibrary.Items.Count) then
    Collection := FGradientLibrary.Items[FGradientLibraryIndex].Items;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPegtopColorGradientLibraryBox.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then
    ShowContextMenu
  else
    PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;

procedure TPegtopColorGradientLibraryBox.ShowContextMenu;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, Height));
  PopupContextMenu(P.X, P.Y);
end;

procedure TPegtopColorGradientLibraryBox.PopupContextMenu(const X, Y: Integer);
begin
  if FGradientLibrary <> NIL then begin
    if LibraryBoxPopupMenu = NIL then LibraryBoxPopupMenu := TPegtopLibraryBoxPopupMenu.Create(Application);
    LibraryBoxPopupMenu.PopupComponent := Self;
    LibraryBoxPopupMenu.Popup(X, Y);
  end;
end;

procedure TPegtopColorGradientLibraryBox.SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
begin
  if FGradientLibrary <> Value then begin
    Collection := NIL;
    if Assigned(FGradientLibrary) then begin
      FGradientLibrary.RemoveListener(GradientsChange);
      FGradientLibrary.RemoveFreeNotification(Self);
    end;
    FGradientLibrary := Value;
    if Assigned(FGradientLibrary) then begin
      if FGradientLibrary.Items.Count > 0 then FGradientLibraryIndex := 0 else FGradientLibraryIndex := -1;
      if (FGradientLibraryIndex >= 0) and (FGradientLibraryIndex < FGradientLibrary.Items.Count) then
        Collection := FGradientLibrary.Items[FGradientLibraryIndex].Items;
      FGradientLibrary.FreeNotification(Self);
      FGradientLibrary.AddListener(GradientsChange);
    end;
  end;
end;

procedure TPegtopColorGradientLibraryBox.SetGradientLibraryIndex(Value: Integer);
begin
  if Value < -1 then Value := -1
  else if FGradientLibrary = NIL then Value := -1
  else if Value > FGradientLibrary.Items.Count - 1 then Value := FGradientLibrary.Items.Count - 1;
  if FGradientLibraryIndex <> Value then begin
    Collection := NIL;
    FGradientLibraryIndex := Value;
    if (FGradientLibraryIndex >= 0) and (FGradientLibraryIndex < FGradientLibrary.Items.Count) then
      Collection := FGradientLibrary.Items[FGradientLibraryIndex].Items;
    if Assigned(FOnGradientLibraryIndexChange) then FOnGradientLibraryIndexChange(Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientManager
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientManager.Create(AOwner: TComponent);
begin
  inherited;
  FComboBox := TPegtopComboBox.Create(Self);
  FComboBox.Style := csDropDownList;
  FComboBox.Align := alTop;
  FComboBox.Enabled := False;
  FComboBox.Parent := Self;
  FComboBox.OnClick := ComboBoxClick;
  FComboBox.OnGetItemStyle := ComboBoxGetItemStyle;
  FGradientLibraryBox := TPegtopColorGradientLibraryBox.Create(Self);
  FGradientLibraryBox.Align := alClient;
  FGradientLibraryBox.BorderStyle := pbsFlat;
  FGradientLibraryBox.Parent := Self;
  FGradientLibraryBox.OnItemSelected := GradientLibraryBoxItemSelected;
  FGradientLibraryBox.OnApply := GradientLibraryBoxApply;
  FGradientLibraryBox.OnAddNew := GradientLibraryBoxAddNew;
  FGradientLibraryBox.OnChange := GradientLibraryBoxChange;
  FGradientLibraryBox.OnGradientLibraryIndexChange := GradientLibraryIndexChange;
end;

destructor TPegtopColorGradientManager.Destroy;
begin
  inherited;
end;

procedure TPegtopColorGradientManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FGradientLibraryBox.GradientLibrary) then begin
    FGradientLibraryBox.GradientLibrary := NIL;
    RefreshComboBox;
  end;
end;

procedure TPegtopColorGradientManager.RefreshComboBox;
var
  I: Integer;
  S: String;
  Item: TPegtopColorGradientListItem;
begin
  FComboBox.Items.Clear;
  FComboBox.Enabled := (FGradientLibraryBox.GradientLibrary <> NIL) and (FGradientLibraryBox.GradientLibrary.Items.Count > 0);
  if FComboBox.Enabled then begin
    for I := 0 to FGradientLibraryBox.GradientLibrary.Items.Count - 1 do begin
      Item := FGradientLibraryBox.GradientLibrary.Items[I];
      if Assigned(Item) and Assigned(Item.Items) then begin
        S := Item.Items.Name;
        if S = '' then S := 'no name';
      end
      else begin
        S := '';
      end;
      FComboBox.Items.AddObject(S, FGradientLibraryBox.GradientLibrary.Items[I]);
    end;
    FComboBox.ItemIndex := FGradientLibraryBox.GradientLibraryIndex;
  end;
end;

procedure TPegtopColorGradientManager.GradientLibraryIndexChange(Sender: TObject);
begin
  if FComboBox.ItemIndex <> FGradientLibraryBox.GradientLibraryIndex then
    FComboBox.ItemIndex := FGradientLibraryBox.GradientLibraryIndex;
end;

procedure TPegtopColorGradientManager.ComboBoxClick(Sender: TObject);
begin
  FGradientLibraryBox.GradientLibraryIndex := FComboBox.ItemIndex;
end;

procedure TPegtopColorGradientManager.ComboBoxGetItemStyle(Sender: TObject; Index: Integer;
  State: TOwnerDrawState; var Style: TPegtopComboBoxItemStyle);
var
  Item1: TPegtopColorGradientListItem;
  Item2: TPegtopColorGradientListItem;
begin
  if (Index > 0) then
    Item1 := TPegtopColorGradientListItem(FComboBox.Items.Objects[Index - 1])
  else
    Item1 := NIL;
  Item2 := TPegtopColorGradientListItem(FComboBox.Items.Objects[Index]);
  if Assigned(Item1) and Assigned(Item2)
  and ((Item1.FileName = '') <> (Item2.FileName = '')) then
    Style.Separator := True;
end;

procedure TPegtopColorGradientManager.GradientLibraryBoxItemSelected(Sender: TObject; Index: Integer);
begin
  if Assigned(FOnSelect) then FOnSelect(Self);
end;

procedure TPegtopColorGradientManager.GradientLibraryBoxApply(Sender: TObject);
begin
  if (FGradientLibraryBox.GradientLibraryIndex >= 0)
  and (FGradientLibraryBox.ItemIndex >= 0)
  and Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TPegtopColorGradientManager.GradientLibraryBoxAddNew(Sender: TObject; NewGradient: TPegtopCustomColorGradient);
begin
  if Assigned(FOnAddNew) then FOnAddNew(Self, NewGradient);
end;

procedure TPegtopColorGradientManager.GradientLibraryBoxChange(Sender: TObject);
begin
  RefreshComboBox;
end;

function TPegtopColorGradientManager.GetGradientLibrary: TPegtopCustomColorGradientLibrary;
begin
  Result := FGradientLibraryBox.GradientLibrary;
end;

procedure TPegtopColorGradientManager.SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
begin
  if FGradientLibraryBox.GradientLibrary <> Value then begin
    FGradientLibraryBox.GradientLibrary := Value;
    RefreshComboBox;
  end;
end;

function TPegtopColorGradientManager.GetGradientLibraryIndex: Integer;
begin
  Result := FGradientLibraryBox.GradientLibraryIndex;
end;

procedure TPegtopColorGradientManager.SetGradientLibraryIndex(Value: Integer);
begin
  if FGradientLibraryBox.GradientLibraryIndex <> Value then begin
    FGradientLibraryBox.GradientLibraryIndex := Value;
    FComboBox.ItemIndex := FGradientLibraryBox.GradientLibraryIndex;
  end;
end;

function TPegtopColorGradientManager.GetItemIndex: Integer;
begin
  Result := FGradientLibraryBox.ItemIndex;
end;

procedure TPegtopColorGradientManager.SetItemIndex(Value: Integer);
begin
  FGradientLibraryBox.ItemIndex := Value;
end;

function TPegtopColorGradientManager.GetSelected: TPegtopCustomColorGradient;
begin
  if (FGradientLibraryBox.GradientLibrary <> NIL)
  and (FGradientLibraryBox.GradientLibraryIndex >= 0)
  and (FGradientLibraryBox.ItemIndex >= 0) then
    Result := FGradientLibraryBox.GradientLibrary.Items[FGradientLibraryBox.GradientLibraryIndex].Items[FGradientLibraryBox.ItemIndex].Gradient
  else
    Result := NIL;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopLibraryBoxPopupMenu
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopLibraryBoxPopupMenu.Create(AOwner: TComponent);
const
  MenuCaptions: array[0..7] of String = (
    '&Apply', '&Copy', '-', 'Add &new', '&Rename', '&Delete', '-', 'Collection'
  );
  ListMenuCaptions: array[0..4] of String = (
    'Create &new...', '&Import file...', '&Export file...', '&Rename', 'Re&move'
  );
  function CreateItem(Caption: TCaption; Tag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Caption;
    Result.Tag := Tag;
    Result.Default := Tag = 0;
    if Result.Caption <> '-' then begin
      Result.OnClick := MenuItemClick;
    end;
  end;
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  for I := Low(MenuCaptions) to High(MenuCaptions) do begin
    MenuItem := CreateItem(MenuCaptions[I], I);
    Items.Add(MenuItem);
  end;
  for I := Low(ListMenuCaptions) to High(ListMenuCaptions) do begin
    MenuItem := CreateItem(ListMenuCaptions[I], 100 + I);
    Items[ListMenuItem].Add(MenuItem);
  end;
end;

procedure TPegtopLibraryBoxPopupMenu.DoPopup(Sender: TObject);
var
  Component: TComponent;
  LibraryBox: TPegtopColorGradientLibraryBox;
  LibraryIndex: Integer;
  LibraryItem: TPegtopColorGradientListItem;
  ItemIndex: Integer;
  Item: TPegtopColorGradientItem;
  UserDefined: Boolean;
begin
  Component := TPopupMenu(Sender).PopupComponent;
  if Component is TPegtopColorGradientLibraryBox then begin
    LibraryBox := TPegtopColorGradientLibraryBox(Component);
    LibraryIndex := LibraryBox.GradientLibraryIndex;
    if LibraryIndex >= 0 then LibraryItem := LibraryBox.GradientLibrary.Items[LibraryIndex] else LibraryItem := NIL;
    ItemIndex := LibraryBox.ItemIndex;
    if Assigned(LibraryItem) and (ItemIndex >= 0) then Item := LibraryItem.Items[ItemIndex]
    else Item := NIL;
    UserDefined := Assigned(LibraryItem) and ((LibraryItem.FileName <> '') or (csDesigning in LibraryBox.GradientLibrary.ComponentState));
    Items[0].Visible := Item <> NIL;
    Items[1].Visible := Item <> NIL;
    Items[3].Enabled := LibraryItem <> NIL;
    Items[4].Visible := Item <> NIL;
    Items[4].Enabled := (Item <> NIL) and UserDefined;
    Items[5].Visible := Item <> NIL;
    Items[5].Enabled := (Item <> NIL) and UserDefined;
    Items[ListMenuItem].Items[3].Visible := LibraryItem <> NIL;
    Items[ListMenuItem].Items[3].Enabled := UserDefined;
  end;
  inherited;
end;

procedure TPegtopLibraryBoxPopupMenu.MenuItemClick(Sender: TObject);
var
  Component: TComponent;
begin
  // since this event handler is not assigned to other objects,
  // we know that Sender is a TMenuItem of a TPopupMenu
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TPegtopColorGradientLibraryBox then begin
    DoCommand(TPegtopColorGradientLibraryBox(Component), TComponent(Sender).Tag);
  end;
end;

procedure TPegtopLibraryBoxPopupMenu.DoCommand(LibraryBox: TPegtopColorGradientLibraryBox; Command: Integer);
var
  LibraryIndex: Integer;
  LibraryItem: TPegtopColorGradientListItem;
  ItemIndex: Integer;
  Item: TPegtopColorGradientItem;
  Dialog: TPegtopColorGradientOpenDialog;
  RenameForm: TRenameForm;
  NewItem: TPegtopColorGradientItem;
begin
  if LibraryBox.GradientLibrary <> NIL then begin
    LibraryIndex := LibraryBox.GradientLibraryIndex;
    if LibraryIndex >= 0 then LibraryItem := LibraryBox.GradientLibrary.Items[LibraryIndex] else LibraryItem := NIL;
    ItemIndex := LibraryBox.ItemIndex;
    if Assigned(LibraryItem) and (ItemIndex >= 0) then Item := LibraryItem.Items[ItemIndex]
    else Item := NIL;
    case Command of
      0: // apply
        if Assigned(LibraryBox.FOnApply) then LibraryBox.FOnApply(LibraryBox);
      1: // copy
        if Assigned(Item) then ColorClipboard.SetColorGradient(Item.Gradient);
      3: // add new
        if Assigned(LibraryItem) then begin
          NewItem := LibraryItem.Items.Add;
          if Assigned(LibraryBox.FOnAddNew) then LibraryBox.FOnAddNew(LibraryBox, NewItem.Gradient);
        end;
      4: // rename
        if Assigned(Item) then begin
          RenameForm := TRenameForm.CreateNew(NIL);
          try
            RenameForm.Caption := 'Rename gradient';
            RenameForm.InfoLabel.Caption := 'Gradient name';
            RenameForm.NameEdit.Text := Item.Gradient.Name;
            RenameForm.NameEdit.MaxLength := 31;
            if RenameForm.ShowModal = mrOk then begin
              Item.Gradient.Name := RenameForm.NameEdit.Text;
            end;
          finally
            RenameForm.Free;
          end;
        end;
      5: // delete
        if Assigned(Item) and (Windows.MessageBox(LibraryBox.Handle, 'do you really want to delete the selected gradient from this collection?', 'Delete gradient', MB_YESNO or MB_ICONQUESTION) = ID_YES) then
          LibraryItem.Items.Delete(ItemIndex);
      100: // create new list
        if csDesigning in LibraryBox.GradientLibrary.ComponentState then begin
          RenameForm := TRenameForm.CreateNew(NIL);
          try
            RenameForm.Caption := 'Create new collection';
            RenameForm.InfoLabel.Caption := 'Collection name';
            RenameForm.NameEdit.Text := '';
            RenameForm.NameEdit.MaxLength := 31;
            if RenameForm.ShowModal = mrOk then begin
              with LibraryBox.GradientLibrary.Items.Add do begin
                Name := RenameForm.NameEdit.Text;
              end;
              LibraryBox.GradientLibraryIndex := LibraryBox.GradientLibrary.Items.Count - 1;
            end;
          finally
            RenameForm.Free;
          end;
        end
        else begin
          Dialog := TPegtopColorGradientSaveDialog.Create(NIL);
          try
            Dialog.Options := Dialog.Options
            + [ofOverwritePrompt, ofPathMustExist]
            - [ofAllowMultiSelect];
            if Dialog.Execute then begin
              with LibraryBox.GradientLibrary.Items.Add do begin
                FileName := Dialog.FileName;
                Name := ExtractFileName(Dialog.FileName);
                Name := Copy(Name, 1, Length(Name) - Length(ExtractFileExt(Name)));
              end;
              LibraryBox.GradientLibraryIndex := LibraryBox.GradientLibrary.Items.Count - 1;
            end;
          finally
            Dialog.Free;
          end;
        end;
      101: // import file
        begin
          Dialog := TPegtopColorGradientOpenDialog.Create(NIL);
          try
            Dialog.Options := Dialog.Options
            + [ofPathMustExist]
            - [ofAllowMultiSelect, ofOverwritePrompt];
            if Dialog.Execute then begin
              with LibraryBox.GradientLibrary.Items.AddFromFile(Dialog.FileName) do begin
                if csDesigning in LibraryBox.GradientLibrary.ComponentState then FileName := '';
              end;
              LibraryBox.GradientLibraryIndex := LibraryBox.GradientLibrary.Items.Count - 1;
            end;
          finally
            Dialog.Free;
          end;
        end;
      102: // export file
        if Assigned(LibraryItem) then begin
          Dialog := TPegtopColorGradientSaveDialog.Create(NIL);
          try
            Dialog.Options := Dialog.Options
            + [ofOverwritePrompt, ofPathMustExist]
            - [ofAllowMultiSelect];
            if Dialog.Execute then begin
              LibraryItem.Items.SaveToFile(Dialog.FileName);
            end;
          finally
            Dialog.Free;
          end;
        end;
      103: // rename list
        if Assigned(LibraryItem) then begin
          RenameForm := TRenameForm.CreateNew(NIL);
          try
            RenameForm.Caption := 'Rename collection';
            RenameForm.InfoLabel.Caption := 'Collection name';
            RenameForm.NameEdit.Text := LibraryItem.Items.Name;
            RenameForm.NameEdit.MaxLength := 31;
            if RenameForm.ShowModal = mrOk then begin
              LibraryItem.Items.Name := RenameForm.NameEdit.Text;
            end;
          finally
            RenameForm.Free;
          end;
        end;
      104: // remove list
        if Assigned(LibraryItem) then begin
          LibraryBox.GradientLibrary.Items.Delete(LibraryIndex);
        end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TRenameForm
////////////////////////////////////////////////////////////////////////////////

procedure TRenameForm.AfterConstruction;
var
  Button: TButton;
begin
  inherited;
  BorderIcons := [biSystemMenu];
  BorderStyle := bsSingle;
  ClientWidth := 200;
  ClientHeight := 96;
  Position := poScreenCenter;

  // label
  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.AutoSize := False;
  FInfoLabel.Transparent := True;
  FInfoLabel.Top := 8;
  FInfoLabel.Left := 8;
  FInfoLabel.Width := ClientWidth - 16;
  FInfoLabel.Parent := Self;

  // edit box
  FNameEdit := TEdit.Create(Self);
  FNameEdit.Top := 32;
  FNameEdit.Left := 8;
  FNameEdit.Width := ClientWidth - 16;
  FNameEdit.Parent := Self;

  // ok button
  Button := TButton.Create(Self);
  Button.Top := 64;
  Button.Left := ClientWidth - 88 * 2;
  Button.Width := 80;
  Button.Caption := 'OK';
  Button.Default := True;
  Button.ModalResult := mrOk;
  Button.Parent := Self;

  // cancel button
  Button := TButton.Create(Self);
  Button.Top := 64;
  Button.Left := ClientWidth - 88;
  Button.Width := 80;
  Button.Caption := 'Cancel';
  Button.Cancel := True;
  Button.ModalResult := mrCancel;
  Button.Parent := Self;
end;

initialization
  LibraryBoxPopupMenu := NIL;
end.
