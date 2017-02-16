////////////////////////////////////////////////////////////////////////////////
// File:       PegtopVirtualListBoxes.pas
// Components: TPegtopVirtualListBox
// Version:    1.00
// Date:       17 Oct 2003 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopVirtualListBox is a completely virtual list box, which means it holds
// no items at all. Instead handlers has to be provided which draw and size each
// virtual item.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopVirtualListBoxes;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms;

type
  TPegtopVirtualItemEvent = procedure(Sender: TObject; Index: Integer) of object;
  TPegtopVirtualItemDrawEvent = procedure(Sender: TObject; Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState) of object;
  TPegtopVirtualItemMeasureEvent = procedure(Sender: TOBject; Index: Integer; var ItemHeight: Integer) of object;
  TPegtopVirtualItemMouseEvent = procedure(Sender: TObject; Index: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TPegtopVirtualItemMouseMoveEvent = procedure(Sender: TObject; Index: Integer; Shift: TShiftState; X, Y: Integer) of object;
  TPegtopVirtualItemMovedEvent = procedure(Sender: TObject; FromIndex, ToIndex: Longint) of object;

  TPegtopVirtualListItem = record
    Pos: Integer;
    Height: Integer;
  end;

  TPegtopBorderStyle = (pbsNone, pbsLowered, pbsFlat);
  TPegtopVirtualListBoxOption = (pvoAutoSelect, pvoSeparateLast, pvoAllowMoving, pvoEraseBackground);
  TPegtopVirtualListBoxOptions = set of TPegtopVirtualListBoxOption;
                
  TPegtopCustomVirtualListBox = class(TCustomControl)
  private
    FItemList: array of TPegtopVirtualListItem;
    FScrollSize: Integer;
    FScrollPos: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FItemIndex: Integer;
    FOptions: TPegtopVirtualListBoxOptions;
    FSeparatorSize: Integer;
    FSeparatorColor: TColor;
    FSeparatorDisabled: TColor;
    FBorderStyle: TPegtopBorderStyle;
    FDropIndex: Integer;
    FScrollSpeed: Integer;
    FDragPos: Integer;
    FDragIndex: Integer;
    FMousePos: TPoint;
    FMouseIndex: Integer;
    FMouseDown: Boolean;
    FOnItemDraw: TPegtopVirtualItemDrawEvent;
    FOnItemMeasure: TPegtopVirtualItemMeasureEvent;
    FOnItemMoved: TPegtopVirtualItemMovedEvent;
    FOnItemMouseMove: TPegtopVirtualItemMouseMoveEvent;
    FOnItemMouseDown: TPegtopVirtualItemMouseEvent;
    FOnItemMouseUp: TPegtopVirtualItemMouseEvent;
    FOnItemMouseEnter: TPegtopVirtualItemEvent;
    FOnItemMouseLeave: TPegtopVirtualItemEvent;
    FOnItemSelected: TPegtopVirtualItemEvent;
    procedure ChangeScrollSize(NewSize: Integer);
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure CMWantSpecialKey(var Msg: TWMKey); message CM_WANTSPECIALKEY;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure SetItemIndex(Value: Integer);
    procedure SetOptions(Value: TPegtopVirtualListBoxOptions);
    procedure SetSeparatorSize(Value: Integer);
    procedure SetSeparatorColor(Value: TColor);
    procedure SetSeparatorDisabled(Value: TColor);
    procedure SetBorderStyle(Value: TPegtopBorderStyle);
    procedure SetOnItemDraw(Value: TPegtopVirtualItemDrawEvent);
    procedure SetOnItemMeasure(Value: TPegtopVirtualItemMeasureEvent);
  protected
    function GetClientRect: TRect; override;
    procedure SetCount(Value: Integer);
    procedure ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState); virtual;
    procedure ItemMeasure(Index: Integer; var ItemHeight: Integer); virtual;
    procedure ItemMoved(FromIndex, ToIndex: Integer); virtual;
    procedure ItemMouseMove(Index: Integer; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ItemMouseDown(Index: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ItemMouseUp(Index: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ItemMouseEnter(Index: Integer); virtual;
    procedure ItemMouseLeave(Index: Integer); virtual;
    procedure ItemSelected(Index: Integer); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure AddItems(const ACount: Integer = 1); virtual;
    procedure InsertItems(const Index: Integer; const ACount: Integer = 1); virtual;
    procedure DeleteItem(const Index: Integer); virtual;
    procedure DeleteItems(const FromIndex, ToIndex: Integer); virtual;
    procedure ClearItems; virtual;
    function MovingAllowed: Boolean; virtual;
    property Count: Integer read FCount;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Options: TPegtopVirtualListBoxOptions read FOptions write SetOptions;
    property SeparatorSize: Integer read FSeparatorSize write SetSeparatorSize;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor;
    property SeparatorDisabled: TColor read FSeparatorDisabled write SetSeparatorDisabled;
    property BorderStyle: TPegtopBorderStyle read FBorderStyle write SetBorderStyle default pbsNone;
    property OnItemDraw: TPegtopVirtualItemDrawEvent read FOnItemDraw write SetOnItemDraw;
    property OnItemMeasure: TPegtopVirtualItemMeasureEvent read FOnItemMeasure write SetOnItemMeasure;
    property OnItemMoved: TPegtopVirtualItemMovedEvent read FOnItemMoved write FOnItemMoved;
    property OnItemMouseMove: TPegtopVirtualItemMouseMoveEvent read FOnItemMouseMove write FOnItemMouseMove;
    property OnItemMouseDown: TPegtopVirtualItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseUp: TPegtopVirtualItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
    property OnItemMouseEnter: TPegtopVirtualItemEvent read FOnItemMouseEnter write FOnItemMouseEnter;
    property OnItemMouseLeave: TPegtopVirtualItemEvent read FOnItemMouseLeave write FOnItemMouseLeave;
    property OnItemSelected: TPegtopVirtualItemEvent read FOnItemSelected write FOnItemSelected;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    procedure Scroll(DeltaY: Integer);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function ItemAtPos(Y: Integer): Integer;
    function SeparatorAtPos(Y: Integer): Integer;
    procedure InvalidateItems(FirstIndex, LastIndex: Integer);
    procedure InvalidateAllItems;
    procedure MakeVisible(Index: Integer; PartialOK: Boolean); overload;
    procedure SelectItem(Index: Integer; MoveItem: Boolean = False);
  published
  end;

  TPegtopVirtualListBox = class(TPegtopCustomVirtualListBox)
  public
    procedure AddItems(const ACount: Integer = 1); override;
    procedure InsertItems(const Index: Integer; const ACount: Integer = 1); override;
    procedure DeleteItem(const Index: Integer); override;
    procedure DeleteItems(const FromIndex, ToIndex: Integer); override;
    procedure ClearItems; override;
    property Count;
    property ItemIndex;
  published
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

function MixColors(Color1, Color2: TColor; Percent: Integer): TColor;

implementation

uses
  SysUtils;

const
  ScrollInterval = 20;
  ScrollMaxSpeed = 200;

function MixColors(Color1, Color2: TColor; Percent: Integer): TColor;
begin
  Result := (((Color1 and $FF) * Percent + (Color2 and $FF) * (100 - Percent)) div 100)
  or (((Color1 shr 8 and $FF) * Percent + (Color2 shr 8 and $FF) * (100 - Percent)) div 100 shl 8)
  or (((Color1 shr 16 and $FF) * Percent + (Color2 shr 16 and $FF) * (100 - Percent)) div 100 shl 16);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomVirtualListBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomVirtualListBox.Create(AOwner: TComponent);
begin
  FCount := 0;
  FCapacity := 16;
  FScrollSize := 0;
  FScrollPos := 0;
  FItemIndex := -1;
  FSeparatorSize := 1;
  FSeparatorColor := clBtnFace;
  FSeparatorDisabled := clBtnShadow;
  FBorderStyle := pbsNone;
  FOptions := [pvoAutoSelect, pvoSeparateLast, pvoEraseBackground];
  FDropIndex := -1;
  FScrollSpeed := 0;
  FDragPos := -1;
  FMouseIndex := -1;
  FMouseDown := False;
  FOnItemDraw := NIL;
  FOnItemMeasure := NIL;
  FOnItemMoved := NIL;
  FOnItemMouseMove := NIL;
  FOnItemMouseDown := NIL;
  FOnItemMouseUp := NIL;
  FOnItemMouseEnter := NIL;
  FOnItemMouseLeave := NIL;
  FOnItemSelected := NIL;
  SetLength(FItemList, FCapacity);
  inherited;
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  Color := clWindow;
end;

procedure TPegtopCustomVirtualListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL;
end;

function TPegtopCustomVirtualListBox.GetClientRect: TRect;
begin
  // workaround: avoid runtime error when no handle is assigned
  if WindowHandle <> 0 then Result := inherited GetClientRect else Result := Rect(0, 0, Width, Height);
end;

procedure TPegtopCustomVirtualListBox.Paint;
var
  Index: Integer;
  State: TOwnerDrawState;
  ItemRect: TRect;
begin
  inherited;
  ItemRect.Left := ClientRect.Left;
  ItemRect.Right := ClientRect.Right;
  ItemRect.Top := Canvas.ClipRect.Top;
  ItemRect.Bottom := ItemRect.Top;
  Index := ItemAtPos(ItemRect.Top);
  if Index >= 0 then begin
    ItemRect.Top := FItemList[Index].Pos - FScrollPos;
    while (Index < FCount) and (ItemRect.Top < Canvas.ClipRect.Bottom) do begin
      ItemRect.Bottom := ItemRect.Top + FItemList[Index].Height;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style := psSolid;
      if not Enabled then begin
        State := [odGrayed];
        if Index = FItemIndex then Include(State, odSelected);
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := clBtnShadow;
        Canvas.Pen.Color := clBtnShadow;
      end
      else if Index = FItemIndex then begin
        if Focused then begin
          State := [odSelected, odFocused];
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.Pen.Color := clHighlightText;
        end
        else begin
          State := [odSelected];
          Canvas.Brush.Color := MixColors(ColorToRGB(clHighlight), ColorToRGB(Color), 50);
          Canvas.Font.Color := clWindowText;
          Canvas.Pen.Color := clWindowText;
        end;
      end
      else begin
        State := [];
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := clWindowText;
        Canvas.Pen.Color := clWindowText;
      end;
      if Index = FDropIndex then begin
        Inc(ItemRect.Top, 2);
        Inc(ItemRect.Bottom, 2);
      end;
      if pvoEraseBackground in FOptions then Canvas.FillRect(ItemRect);
      ItemDraw(Index, Canvas, ItemRect, State);
      if Index = FDropIndex then begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := FSeparatorColor;
        Canvas.FillRect(Rect(ItemRect.Left, ItemRect.Top - FSeparatorSize - 2, ItemRect.Right, ItemRect.Top));
      end;
      if (FSeparatorSize > 0) and ((Index < FCount-1) or (pvoSeparateLast in FOptions)) then begin
        ItemRect.Top := ItemRect.Bottom;
        ItemRect.Bottom := ItemRect.Top + FSeparatorSize;
        Canvas.Brush.Style := bsSolid;
        if Enabled then Canvas.Brush.Color := FSeparatorColor
        else Canvas.Brush.Color := FSeparatorDisabled;
        Canvas.FillRect(ItemRect);
      end;
      ItemRect.Top := ItemRect.Bottom;
      Inc(Index);
    end;
    if Index = FDropIndex then begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := FSeparatorColor;
      Canvas.FillRect(Rect(ItemRect.Left, ItemRect.Top - FSeparatorSize - 2, ItemRect.Right, ItemRect.Top));
    end;
  end;
  if ItemRect.Bottom < Canvas.ClipRect.Bottom then begin
    ItemRect.Top := ItemRect.Bottom;
    ItemRect.Bottom := Canvas.ClipRect.Bottom;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(ItemRect);
  end;
end;

procedure TPegtopCustomVirtualListBox.Resize;
var
  ScrollInfo: TScrollInfo;
begin
  inherited;
  if FScrollPos > FScrollSize - ClientHeight then begin
    if FScrollSize > ClientHeight then
      FScrollPos := FScrollSize - ClientHeight
    else if FScrollPos > 0 then
      FScrollPos := 0;
  end;
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_TRACKPOS or SIF_PAGE or SIF_RANGE or SIF_DISABLENOSCROLL;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FScrollSize - 1;
  ScrollInfo.nPos := FScrollPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TPegtopCustomVirtualListBox.SetEnabled(Value: Boolean);
begin
  inherited;
  if Enabled then EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH)
  else EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
  Invalidate;
end;

procedure TPegtopCustomVirtualListBox.SetFocus;
  function GetBaseParent(Control: TControl): TWinControl;
  begin
    while Control.Parent <> nil do Control := Control.Parent;
    if Control is TWinControl then Result := TWinControl(Control)
    else Result := nil;
  end;
var
  Parent: TWinControl;
begin
  Parent := GetBaseParent(Self);
  if Parent is TCustomForm then
    inherited
  // Bug in TWinControl.SetFocus (Delphi 5):
  // Control must either have a parent form or a window handle <> 0.
  // We also allow a parent (or parent of parent) with window handle <> 0
  else if Parent.ParentWindow <> 0 then
    Windows.SetFocus(Handle);
  Invalidate;
end;

procedure TPegtopCustomVirtualListBox.ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState);
begin
  if Assigned(FOnItemDraw) then FOnItemDraw(Self, Index, Canvas, ItemRect, State);
end;

procedure TPegtopCustomVirtualListBox.ItemMeasure(Index: Integer; var ItemHeight: Integer);
begin
  if Assigned(FOnItemMeasure) then FOnItemMeasure(Self, Index, ItemHeight);
end;

procedure TPegtopCustomVirtualListBox.ItemMoved(FromIndex, ToIndex: Integer);
begin
  if Assigned(FOnItemMoved) then FOnItemMoved(Self, FromIndex, ToIndex);
end;

procedure TPegtopCustomVirtualListBox.ItemMouseMove(Index: Integer; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnItemMouseMove) then FOnItemMouseMove(Self, Index, Shift, X, Y);
end;

procedure TPegtopCustomVirtualListBox.ItemMouseDown(Index: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnItemMouseDown) then FOnItemMouseDown(Self, Index, Button, Shift, X, Y);
end;

procedure TPegtopCustomVirtualListBox.ItemMouseUp(Index: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnItemMouseUp) then FOnItemMouseUp(Self, Index, Button, Shift, X, Y);
end;

procedure TPegtopCustomVirtualListBox.ItemMouseEnter(Index: Integer);
begin
  if Assigned(FOnItemMouseEnter) then FOnItemMouseEnter(Self, Index);
end;

procedure TPegtopCustomVirtualListBox.ItemMouseLeave(Index: Integer);
begin
  if Assigned(FOnItemMouseLeave) then FOnItemMouseLeave(Self, Index);
end;

procedure TPegtopCustomVirtualListBox.ItemSelected(Index: Integer);
begin
  if Assigned(FOnItemSelected) then FOnItemSelected(Self, Index);
end;

procedure TPegtopCustomVirtualListBox.WMVScroll(var Msg: TWMScroll);
var
  ScrollInfo: TScrollInfo;
  DeltaY: Integer;
begin
  if Msg.ScrollCode = SB_THUMBTRACK then begin
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_TRACKPOS or SIF_DISABLENOSCROLL;
    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    DeltaY := FScrollPos - ScrollInfo.nTrackPos;
    FScrollPos := ScrollInfo.nTrackPos;
    ScrollInfo.fMask := SIF_POS or SIF_DISABLENOSCROLL;
    ScrollInfo.nPos := FScrollPos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, False);
    ScrollWindowEx(Handle, 0, DeltaY, NIL, NIL, 0, NIL, SW_INVALIDATE);
  end
  else if Msg.ScrollCode = SB_LINEUP then begin
    Scroll(10);
  end
  else if Msg.ScrollCode = SB_LINEDOWN then begin
    Scroll(-10);
  end
  else if Msg.ScrollCode = SB_PAGEUP then begin
    Scroll(ClientHeight div 2);
  end
  else if Msg.ScrollCode = SB_PAGEDOWN then begin
    Scroll(-ClientHeight div 2);
  end;
  Msg.Result := 0;
end;

procedure TPegtopCustomVirtualListBox.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TPegtopCustomVirtualListBox.WMMouseWheel(var Msg: TMessage);
begin
  Scroll(Smallint(Msg.WParamHi) div 8);
end;

procedure TPegtopCustomVirtualListBox.CMWantSpecialKey(var Msg: TWMKey);
begin
  inherited;
  case Msg.CharCode of
    VK_UP, VK_DOWN:
      Msg.Result := 1;
  end;
end;

procedure TPegtopCustomVirtualListBox.WMTimer(var Msg: TWMTimer);
var
  NewDropIndex: Integer;
begin
  if FScrollSpeed > 0 then begin
    Scroll(FScrollSpeed div 10);
    if FScrollSpeed < ScrollMaxSpeed then Inc(FScrollSpeed, 2);
    if FScrollPos > 0 then SetTimer(Handle, 0, ScrollInterval, NIL);
  end
  else if FScrollSpeed < 0 then begin
    Scroll(FScrollSpeed div 10);
    if FScrollSpeed > -ScrollMaxSpeed then Dec(FScrollSpeed, 2);
    if FScrollPos < FScrollSize - ClientHeight then SetTimer(Handle, 0, ScrollInterval, NIL);
  end;
  if FDragPos >= 0 then begin
    NewDropIndex := SeparatorAtPos(FDragPos);
    if FDropIndex <> NewDropIndex then begin
      FDropIndex := NewDropIndex;
      Invalidate;
    end;
  end;
end;

procedure TPegtopCustomVirtualListBox.CMMouseLeave(var Msg: TMessage);
begin
  if FMouseIndex >= 0 then begin
    ItemMouseLeave(FMouseIndex);
    FMouseIndex := -1;
  end;
  inherited;
end;

procedure TPegtopCustomVirtualListBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopCustomVirtualListBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopCustomVirtualListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then begin
    SelectItem(ItemIndex - 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_DOWN then begin
    SelectItem(ItemIndex + 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_PRIOR then begin
    SelectItem(ItemIndex - 10, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_NEXT then begin
    SelectItem(ItemIndex + 10, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_HOME then begin
    SelectItem(0, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_END then begin
    SelectItem(FCount - 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end;
  inherited;
end;

procedure TPegtopCustomVirtualListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewMouseIndex: Integer;
begin
  if MovingAllowed and FMouseDown
  and ((Abs(FMousePos.X - X) > 3) or (Abs(FMousePos.Y - Y) > 3)) then BeginDrag(False, 3);
  NewMouseIndex := ItemAtPos(Y);
  if NewMouseIndex <> FMouseIndex then begin
    if FMouseIndex >= 0 then ItemMouseLeave(FMouseIndex);
    FMouseIndex := NewMouseIndex;
    if FMouseIndex >= 0 then ItemMouseEnter(FMouseIndex);
  end;
  if FMouseIndex >= 0 then ItemMouseMove(FMouseIndex, Shift, X, Y + FScrollPos - FItemList[FMouseIndex].Pos);
  inherited;
end;

procedure TPegtopCustomVirtualListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  SetFocus;
  Index := ItemAtPos(Y);
  if Index >= 0 then begin
    if Button = mbLeft then begin
      FDragIndex := Index;
      FMouseDown := True;
      FMousePos.X := X;
      FMousePos.Y := Y;
    end;
    if pvoAutoSelect in FOptions then SelectItem(Index);
    if Index >= 0 then ItemMouseDown(Index, Button, Shift, X, Y + FScrollPos - FItemList[Index].Pos);
  end;
end;

procedure TPegtopCustomVirtualListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  FMouseDown := False;
  Index := ItemAtPos(Y);
  if Index >= 0 then ItemMouseDown(Index, Button, Shift, X, Y + FScrollPos - FItemList[Index].Pos);
end;

procedure TPegtopCustomVirtualListBox.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
end;

procedure TPegtopCustomVirtualListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  FScrollSpeed := 0;
  FDragPos := -1;
  inherited;
  FDragIndex := -1;
end;

procedure TPegtopCustomVirtualListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if (Source = Self) then begin
    if FDragIndex <> -1 then ItemMoved(FDragIndex, SeparatorAtPos(Y));
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopCustomVirtualListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  NewDropIndex: Integer;
begin
  if Y < 12 then begin
    if FScrollSpeed = 0 then begin
      FScrollSpeed := 20;
      SetTimer(Handle, 0, ScrollInterval, NIL);
    end;
  end
  else if Y > ClientHeight - 12 then begin
    if FScrollSpeed = 0 then begin
      FScrollSpeed := -20;
      SetTimer(Handle, 0, ScrollInterval, NIL);
    end;
  end
  else begin
    FScrollSpeed := 0;
  end;
  if (Source = Self) then begin
    Accept := True;
    if State = dsDragLeave then begin
      NewDropIndex := -1;
      FDragPos := -1;
    end
    else begin
      NewDropIndex := SeparatorAtPos(Y);
      FDragPos := Y;
    end;
    if FDropIndex <> NewDropIndex then begin
      FDropIndex := NewDropIndex;
      Invalidate;
    end;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopCustomVirtualListBox.InvalidateItems(FirstIndex, LastIndex: Integer);
var
  I, Pos: Integer;
  ItemHeight: Integer;
  Changed: Boolean;
begin
  if FCount > 0 then begin
    if FirstIndex < 0 then FirstIndex := 0
    else if FirstIndex > FCount-1 then FirstIndex := FCount-1;
    if LastIndex < 0 then LastIndex := 0
    else if LastIndex > FCount-1 then LastIndex := FCount-1;
    if FirstIndex > 0 then begin
      Pos := FItemList[FirstIndex-1].Pos + FItemList[FirstIndex-1].Height + FSeparatorSize;
    end
    else begin
      Pos := 0;
    end;
    Changed := False;
    for I := FirstIndex to LastIndex do begin
      FItemList[I].Pos := Pos;
      ItemHeight := 20;
      ItemMeasure(I, ItemHeight);
      if FItemList[I].Height <> ItemHeight then begin
        FItemList[I].Height := ItemHeight;
        Changed := True;
      end;
      Inc(Pos, ItemHeight + FSeparatorSize);
    end;
    if Changed then begin
      for I := LastIndex + 1 to FCount-1 do begin
        FItemList[I].Pos := Pos;
        Inc(Pos, FItemList[I].Height + FSeparatorSize);
      end;
    end;
  end
  else begin
    Pos := 0;
  end;
  ChangeScrollSize(Pos - FSeparatorSize);
  Invalidate;
end;

procedure TPegtopCustomVirtualListBox.InvalidateAllItems;
begin
  InvalidateItems(0, FCount - 1);
end;

procedure TPegtopCustomVirtualListBox.MakeVisible(Index: Integer; PartialOK: Boolean);
begin
  if ((FItemList[Index].Pos < FScrollPos) and not PartialOK)
  or (FItemList[Index].Pos+FItemList[Index].Height <= FScrollPos) then begin
    Scroll(FScrollPos - FItemList[Index].Pos);
  end
  else if ((FItemList[Index].Pos+FItemList[Index].Height > FScrollPos+ClientHeight) and not PartialOK)
  or (FItemList[Index].Pos >= FScrollPos+ClientHeight) then begin
    Scroll((FScrollPos+ClientHeight) - (FItemList[Index].Pos+FItemList[Index].Height));
  end
end;

procedure TPegtopCustomVirtualListBox.SelectItem(Index: Integer; MoveItem: Boolean = False);
begin
  if FCount > 0 then begin
    if Index < 0 then Index := 0
    else if Index > FCount - 1 then Index := FCount - 1;
    if MoveItem then begin
      if Index <> ItemIndex then begin
        if Index >= ItemIndex then Inc(Index);
        ItemMoved(ItemIndex, Index);
      end;
    end
    else begin
      if ItemIndex <> Index then begin
        ItemIndex := Index;
        ItemSelected(ItemIndex);
      end;
      MakeVisible(ItemIndex, False);
    end;
  end;
end;

procedure TPegtopCustomVirtualListBox.ChangeScrollSize(NewSize: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if NewSize <> FScrollsize then begin
    FScrollSize := NewSize;
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_DISABLENOSCROLL;
    ScrollInfo.nPage := ClientHeight;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := FScrollSize - 1;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

procedure TPegtopCustomVirtualListBox.Scroll(DeltaY: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if DeltaY > FScrollPos then DeltaY := FScrollPos
  else if (DeltaY < 0) and (FScrollSize < ClientHeight) then DeltaY := 0
  else if DeltaY < FScrollPos - (FScrollSize - ClientHeight) then DeltaY := FScrollPos - (FScrollSize - ClientHeight);
  if DeltaY <> 0 then begin
    FScrollPos := FScrollPos - DeltaY;
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_POS or SIF_DISABLENOSCROLL;
    ScrollInfo.nPos := FScrollPos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    ScrollWindowEx(Handle, 0, DeltaY, NIL, NIL, 0, NIL, SW_INVALIDATE);
  end;
end;

function TPegtopCustomVirtualListBox.ItemAtPos(Y: Integer): Integer;
var
  P: Integer;
  I, Min, Max: Integer;
begin
  P := Y + FScrollPos;
  Result := -1;
  if (FCount > 0) then begin
    if P < FItemList[0].Pos then begin
    end
    else if P >= (FItemList[FCount-1].Pos + FItemList[FCount-1].Height + FSeparatorSize) then begin
    end
    else begin
      Min := 0;
      Max := FCount - 1;
      while (Result < 0) and (Min <= Max) do begin
        I := (Min + Max) div 2;
        if P < FItemList[I].Pos then begin
          Max := I - 1;
        end
        else if P >= (FItemList[I].Pos + FItemList[I].Height + FSeparatorSize) then begin
          Min := I + 1;
        end
        else begin
          Result := I;
        end;
      end;
    end;
  end;
end;

function TPegtopCustomVirtualListBox.SeparatorAtPos(Y: Integer): Integer;
var
  P: Integer;
begin
  P := Y + FScrollPos;
  Result := -1;
  if (FCount > 0) then begin
    if P < FItemList[0].Pos then begin
      Result := 0;
    end
    else if P >= (FItemList[FCount-1].Pos + FItemList[FCount-1].Height + FSeparatorSize) then begin
      Result := FCount;
    end
    else begin
      Result := ItemAtPos(Y);
      if (Result >= 0)
      and (Y + FScrollPos > FItemList[Result].Pos + FItemList[Result].Height div 2) then Inc(Result);
    end;
  end;
end;

procedure TPegtopCustomVirtualListBox.AddItems(const ACount: Integer = 1);
begin
  SetCount(FCount + ACount);
end;

procedure TPegtopCustomVirtualListBox.InsertItems(const Index: Integer; const ACount: Integer = 1);
var
  I: Integer;
begin
  if (Index >= 0) and (Index <= FCount) and (ACount > 0) then begin
    if FCount + ACount > FCapacity then begin
      FCapacity := (FCount + ACount) * 2;
      SetLength(FItemList, FCapacity);
    end;
    for I := FCount - 1 downto Index do begin
      FItemList[I + ACount].Height := FItemList[I - ACount].Height;
    end;
    Inc(FCount, ACount);
    InvalidateItems(Index, Index + ACount - 1);
    if FItemIndex >= 0 then Inc(FItemIndex, ACount);
    Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.DeleteItem(const Index: Integer);
begin
  DeleteItems(Index, Index);
end;

procedure TPegtopCustomVirtualListBox.DeleteItems(const FromIndex, ToIndex: Integer);
var
  I, P, C: Integer;
  RemovedSize: Integer;
begin
  if (ToIndex >= FromIndex) and (FromIndex >= 0) and (ToIndex < FCount) then begin
    C := ToIndex - FromIndex + 1;
    P := FItemList[FromIndex].Pos;
    RemovedSize := 0;
    for I := FromIndex to FCount - C - 1 do begin
      Inc(RemovedSize, FItemList[I].Height + FSeparatorSize);
      FItemList[I].Pos := P;
      FItemList[I].Height := FItemList[I + C].Height;
      Inc(P, FItemList[I].Height);
    end;
    Dec(FCount, C);
    if FCount * 2 < FCapacity then begin
      FCapacity := FCount;
      SetLength(FItemList, FCapacity);
    end;
    ChangeScrollSize(FScrollSize - RemovedSize);
    if FScrollSize <= ClientHeight then FScrollPos := 0
    else if FScrollPos > FScrollSize - ClientHeight then FScrollPos := FScrollSize - ClientHeight;
    if FItemIndex > FCount - 1 then FItemIndex := -1;
    Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.ClearItems;
begin
  DeleteItems(0, FCount - 1);
end;

function TPegtopCustomVirtualListBox.MovingAllowed: Boolean;
begin
  Result := pvoAllowMoving in FOptions;
end;

procedure TPegtopCustomVirtualListBox.SetItemIndex(Value: Integer);
begin
  if Value < -1 then Value := -1
  else if Value > FCount - 1 then Value := FCount - 1;
  if FItemIndex <> Value then begin
    FItemIndex := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetCount(Value: Integer);
var
  Old: Integer;
begin
  if Value < 0 then Value := 0;
  if FCount <> Value then begin
    Old := FCount;
    FCount := Value;
    if FCount > FCapacity then begin
      FCapacity := FCount * 2;
      SetLength(FItemList, FCapacity);
    end
    else if FCount * 2 < FCapacity then begin
      FCapacity := FCount;
      SetLength(FItemList, FCapacity);
    end;
    InvalidateItems(Old, FCount - 1);
    if FScrollSize <= ClientHeight then FScrollPos := 0
    else if FScrollPos > FScrollSize - ClientHeight then FScrollPos := FScrollSize - ClientHeight;
    if FItemIndex > FCount - 1 then FItemIndex := -1;
    Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetOptions(Value: TPegtopVirtualListBoxOptions);
var
  Old: TPegtopVirtualListBoxOptions;
begin
  if FOptions <> Value then begin
    Old := FOptions;
    FOptions := Value;
    if (FOptions * [pvoSeparateLast]) <> (Old * [pvoSeparateLast]) then Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetSeparatorSize(Value: Integer);
var
  I: Integer;
begin
  if Value < 0 then Value := 0
  else if Value > 5 then Value := 5;
  if FSeparatorSize <> Value then begin
    if FCount > 0 then begin
      for I := 1 to FCount-1 do begin
        FItemList[I].pos := FItemList[I].Pos + I * (Value - FSeparatorSize);
      end;
      ChangeScrollSize(FScrollSize + (FCount - 1) * (Value - FSeparatorSize));
    end;
    FSeparatorSize := Value;
    Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetSeparatorColor(Value: TColor);
begin
  if FSeparatorColor <> Value then begin
    FSeparatorColor := Value;
    if Enabled then Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetSeparatorDisabled(Value: TColor);
begin
  if FSeparatorDisabled <> Value then begin
    FSeparatorDisabled := Value;
    if not Enabled then Invalidate;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetBorderStyle(Value: TPegtopBorderStyle);
var
  ScrollInfo: TScrollInfo;
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    if FBorderStyle = pbsNone then begin
      Ctl3D := True;
      BevelKind := bkNone;
      BevelInner := bvNone;
      BevelOuter := bvNone;
    end
    else if FBorderStyle = pbsLowered then begin
      Ctl3D := True;
      BevelKind := bkTile;
      BevelInner := bvLowered;
      BevelOuter := bvLowered;
    end
    else if FBorderStyle = pbsFlat then begin
      Ctl3D := False;
      BevelKind := bkFlat;
      BevelInner := bvNone;
      BevelOuter := bvLowered;
    end;
    if Assigned(Parent) and (Handle <> 0) then begin
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_DISABLENOSCROLL;
      ScrollInfo.nPage := ClientHeight;
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := FScrollSize - 1;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    end;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetOnItemDraw(Value: TPegtopVirtualItemDrawEvent);
begin
  if (TMethod(FOnItemDraw).Code <> TMethod(Value).Code)
  or (TMethod(FOnItemDraw).Data <> TMethod(Value).Data) then begin
    FOnItemDraw := Value;
    if FCount > 0 then InvalidateAllItems;
  end;
end;

procedure TPegtopCustomVirtualListBox.SetOnItemMeasure(Value: TPegtopVirtualItemMeasureEvent);
begin
  if (TMethod(FOnItemMeasure).Code <> TMethod(Value).Code)
  or (TMethod(FOnItemMeasure).Data <> TMethod(Value).Data) then begin
    FOnItemMeasure := Value;
    if FCount > 0 then InvalidateAllItems;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopVirtualListBox
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopVirtualListBox.AddItems(const ACount: Integer = 1);
begin
  inherited;
end;

procedure TPegtopVirtualListBox.InsertItems(const Index: Integer; const ACount: Integer = 1);
begin
  inherited;
end;

procedure TPegtopVirtualListBox.DeleteItem(const Index: Integer);
begin
  inherited;
end;

procedure TPegtopVirtualListBox.DeleteItems(const FromIndex, ToIndex: Integer);
begin
  inherited;
end;

procedure TPegtopVirtualListBox.ClearItems;
begin
  inherited;
end;

end.
