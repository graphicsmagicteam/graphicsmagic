////////////////////////////////////////////////////////////////////////////////
// File:       PegtopLogListBoxes.pas
// Components: TPegtopLogListBox
// Version:    1.00
// Date:       31 Oct 2004 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopLogListBox is a list box for text items with optional images. Items
// are sized automatically according to the length of the text with word break.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopLogListBoxes;

interface

uses
  Windows, Classes, Graphics, Controls, ImgList, PegtopVirtualListBoxes;

type
  TPegtopLogListItem = class
  private
    FTitle: String;
    FText: String;
    FImageIndex: Integer;
  public
    constructor Create(const ATitle, AText: String; const AImageIndex: Integer);
    property Title: String read FTitle;
    property Text: String read FText;
    property ImageIndex: Integer read FImageIndex;
  end;

  TPegtopLogListBox = class(TPegtopCustomVirtualListBox)
  private
    FItems: TList;
    FMaxCount: Integer;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FTitleFont: TFont;
    procedure CutLog;
    procedure ImageListChange(Sender: TObject);
    procedure TitleFontChanged(Sender: TObject);
    procedure SetMaxCount(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetTitleFont(Value: TFont);
  protected
    procedure ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState); override;
    procedure ItemMeasure(Index: Integer; var ItemHeight: Integer); override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const Title, Text: String; const ImageIndex: Integer = -1);
    procedure Clear;
    property Count;
    property ItemIndex;
  published
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property Images: TCustomImageList read FImages write SetImages;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
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

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPegtopLogListItem
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopLogListItem.Create(const ATitle, AText: String; const AImageIndex: Integer);
begin
  FTitle := ATitle;
  FText := AText;
  FImageIndex := AImageIndex;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopLogListBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopLogListBox.Create(AOwner: TComponent);
begin
  FItems := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FTitleFont := TFont.Create;
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := TitleFontChanged;
  inherited;
end;

destructor TPegtopLogListBox.Destroy;
begin
  inherited;
  Clear;
  FItems.Free;
  FImageChangeLink.Free;
end;

procedure TPegtopLogListBox.Add(const Title, Text: String; const ImageIndex: Integer = -1);
begin
  FItems.Add(TPegtopLogListItem.Create(Title, Text, ImageIndex));
  AddItems(1);
  CutLog;
  MakeVisible(Count - 1, False);
end;

procedure TPegtopLogListBox.Clear;
var
  I: Integer;
  Item: TPegtopLogListItem;
begin
  ClearItems;
  for I := 0 to FItems.Count - 1 do begin
    Item := FItems[I];
    Item.Free;
  end;
  FItems.Clear;
end;

procedure TPegtopLogListBox.Resize;
begin
  inherited;
  InvalidateAllItems;
end;

procedure TPegtopLogListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then Images := NIL;
end;

procedure TPegtopLogListBox.CutLog;
var
  I: Integer;
  Item: TPegtopLogListItem;
begin
  if (FMaxCount > 0) and (FItems.Count > FMaxCount) then begin
    I := 0;
    while FItems.Count > FMaxCount do begin
      Item := FItems[0];
      FItems.Delete(0);
      Item.Free;
      Inc(I);
    end;
    DeleteItems(0, I - 1);
  end;
end;

procedure TPegtopLogListBox.ItemDraw(Index: Integer; Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState);
var
  L, T: Integer;
  S: String;
  DrawRect: TRect;
  Item: TPegtopLogListItem;
begin
  L := 4;
  T := 4;
  Item := FItems[Index];
  if FImages <> NIL then begin
    if Item.ImageIndex >= 0 then FImages.Draw(Canvas, L, ItemRect.Top + 4, Item.ImageIndex, Enabled);
    Inc(L, FImages.Width + 4);
  end;
  if Item.Title <> '' then begin
    Canvas.Font := FTitleFont;
    DrawRect := Rect(L, ItemRect.Top + T, ClientWidth - 4, ItemRect.Bottom - 4);
    Inc (T, DrawText(Canvas.Handle, PChar(Item.Title), Length(Item.Title), DrawRect, DT_EXPANDTABS or DT_LEFT or DT_NOPREFIX or DT_WORDBREAK));
  end;
  Canvas.Font := Font;
  DrawRect := Rect(L, ItemRect.Top + T, ClientWidth - 4, ItemRect.Bottom - 4);
  DrawText(Canvas.Handle, PChar(Item.Text), Length(Item.Text), DrawRect, DT_EXPANDTABS or DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  inherited;
end;

procedure TPegtopLogListBox.ItemMeasure(Index: Integer; var ItemHeight: Integer);
var
  L: Integer;
  DrawRect: TRect;
  Item: TPegtopLogListItem;
  H: Integer;
begin
  ItemHeight := 8;
  L := 4;
  if FImages <> NIL then Inc(L, FImages.Width + 4);
  Item := FItems[Index];
  if Item.Title <> '' then begin
    Canvas.Font := FTitleFont;
    DrawRect := Rect(L, 4, ClientWidth - 4, 4);
    Inc(ItemHeight, DrawText(Canvas.Handle, PChar(Item.Title), Length(Item.Title), DrawRect, DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_NOPREFIX or DT_WORDBREAK));
  end;
  Canvas.Font := Font;
  DrawRect := Rect(L, 4, ClientWidth - 4, 4);
  H := DrawText(Canvas.Handle, PChar(Item.Text), Length(Item.Text), DrawRect, DT_CALCRECT or DT_EXPANDTABS or DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  Inc(ItemHeight, H);
  if (FImages <> NIL) and (ItemHeight < FImages.Height + 8) then ItemHeight := FImages.Height + 8;
  inherited;
end;

procedure TPegtopLogListBox.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then Invalidate;
end;

procedure TPegtopLogListBox.TitleFontChanged(Sender: TObject);
begin
  Perform(CM_FONTCHANGED, 0, 0);
end;

procedure TPegtopLogListBox.SetMaxCount(Value: Integer);
begin
  if FMaxCount < 0 then FMaxCount := 0;
  if FMaxCount <> Value then begin
    FMaxCount := Value;
    CutLog;
  end;
end;

procedure TPegtopLogListBox.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then begin
    if FImages <> NIL then FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> NIL then begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    InvalidateAllItems;
  end;
end;

procedure TPegtopLogListBox.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

end.
