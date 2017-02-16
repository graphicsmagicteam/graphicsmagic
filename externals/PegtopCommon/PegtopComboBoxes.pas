unit PegtopComboBoxes;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, ImgList;

type
  TPegtopComboBoxDrawStyle = (pcdDefault, pcdStyled, pcdOwnerDrawFixed, pcdOwnerDrawVariable);

  TPegtopComboBoxItemStyle = record
    Caption: String;
    Comment: String;
    ImageIndex: Integer;
    Icon: THandle;
    BackgroundColor: TColor;
    CaptionColor: TColor;
    CaptionStyle: TFontStyles;
    CommentColor: TColor;
    CommentStyle: TFontStyles;
    Separator: Boolean;
    Indent: Integer;
    BoxColor: TColor;
  end;

  TPegtopComboBoxItemStyleEvent = procedure(Sender: TObject; Index: Integer;
    State: TOwnerDrawState; var Style: TPegtopComboBoxItemStyle) of object;

  TPegtopCustomComboBox = class(TCustomComboBox)
  private
    FImages: TCustomImageList;
    FChangeLink: TChangeLink;
    FOldFontChange: TNotifyEvent;
    FDefaultItemIndex: Integer;
    FDrawStyle: TPegtopComboBoxDrawStyle;
    FDropDownWidth: Integer;
    FIgnoreImageChange: Boolean;
    FOnSelEndOk: TNotifyEvent;
    FOnSelEndCancel: TNotifyEvent;
    FOnGetItemStyle: TPegtopComboBoxItemStyleEvent;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure FontChange(Sender: TObject);
    procedure ImagesChange(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
    procedure SetDrawStyle(Value: TPegtopComboBoxDrawStyle);
    procedure SetDefaultItemIndex(Value: Integer);
    procedure SetOnGetItemStyle(Value: TPegtopComboBoxItemStyleEvent);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DropDown; override;
    procedure SelEndOk; virtual;
    procedure SelEndCancel; virtual;
    property Images: TCustomImageList read FImages write SetImages;
    property DrawStyle: TPegtopComboBoxDrawStyle read FDrawStyle write SetDrawStyle;
    property DefaultItemIndex: Integer read FDefaultItemIndex write SetDefaultItemIndex;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth;
    property OnSelEndOk: TNotifyEvent read FOnSelEndOk write FOnSelEndOk;
    property OnSelEndCancel: TNotifyEvent read FOnSelEndCancel write FOnSelEndCancel;
    property OnGetItemStyle: TPegtopComboBoxItemStyleEvent read FOnGetItemStyle write SetOnGetItemStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPegtopComboBox = class(TPegtopCustomComboBox)
  published
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property Images;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property DrawStyle;
    property DropDownWidth;
    property OnSelEndOk;
    property OnSelEndCancel;
    property OnGetItemStyle;
    property Items; { Must be published after OnMeasureItem }
  end;

implementation

uses
  SysUtils;

constructor TPegtopCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
  FImages := NIL;
  FOldFontChange := Font.Onchange;
  Font.OnChange := FontChange;
end;

destructor TPegtopCustomComboBox.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TPegtopCustomComboBox.Loaded;
begin
  inherited;
  ItemIndex := FDefaultItemIndex;
end;

procedure TPegtopCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  case FDrawStyle of
    pcdOwnerDrawFixed: Params.Style := Params.Style or CBS_OWNERDRAWFIXED;
    pcdStyled, pcdOwnerDrawVariable: Params.Style := Params.Style or CBS_OWNERDRAWVARIABLE;
  end;
end;

procedure TPegtopCustomComboBox.FontChange(Sender: TObject);
var
  H: Integer;
begin
  if Assigned(FOldFontChange) then FOldFontChange(Sender);
  MeasureItem(0, H);
  ItemHeight := H;
end;

procedure TPegtopCustomComboBox.ImagesChange(Sender: TObject);
begin
  if not FIgnoreImageChange then begin
    Invalidate;
  end;
end;

procedure TPegtopCustomComboBox.Notification(AComponent: TComponent; Operation: TOperation);
var
  H: Integer;
begin
  if (AComponent = FImages) and (Operation = opRemove) then begin
    FImages := NIL;
    MeasureItem(0, H);
    ItemHeight := H;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopCustomComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if FDrawStyle = pcdStyled then begin
    if Font.Height < 0 then Height := -Font.Height * 3 div 2 - 1
    else Height := Font.Height + 4;
    if (FImages <> NIL) then begin
      if Index < 0 then begin // box
        if Height < FImages.Height then Height := FImages.Height;
      end
      else begin // drop down menu
        if Height < (FImages.Height + 1) then Height := FImages.Height + 1;
      end;
    end;
    if Assigned(OnMeasureItem) then OnMeasureItem(Self, Index, Height);
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopCustomComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Style: TPegtopComboBoxItemStyle;
  IconX, IconY: Integer;
  TextX, TextY: Integer;
  RectHeight: Integer;
  Box: Boolean;
  OldColor: TColor;
begin
  if FDrawStyle = pcdStyled then begin
    TControlCanvas(Canvas).UpdateTextFlags;

    // box or dropdown menu?
    Box := odComboBoxEdit in State;

    // assign default style:
    Style.Caption := Items[Index];
    Style.Comment := '';
    Style.ImageIndex := -1;
    Style.Icon := 0;
    Style.BackgroundColor := clNone;
    Style.CaptionColor := clNone;
    Style.CaptionStyle := [];
    Style.CommentColor := clNone;
    Style.CommentStyle := [];
    Style.Separator := False;
    Style.Indent := 0;
    Style.BoxColor := clNone;

    // change style:
    if Assigned(FOnGetItemStyle) then FOnGetItemStyle(Self, Index, State, Style);

    // compute horizontal start position:
    if Box then IconX := 1 else IconX := 2;
    Inc(IconX, Style.Indent);
    TextX := IconX;
    if (Style.Icon <> 0) then Inc(TextX, 16 + 2)
    else if (FImages <> NIL) then Inc(TextX, FImages.Width + 2);
    if Style.BoxColor <> clNone then Inc(TextX, 18);

    // compute vertical start position:
    RectHeight := Rect.Bottom - Rect.Top;
    if not Box then Dec(RectHeight); // reserve one pixel for separator line
    IconY := 0;
    if Style.Icon <> 0 then IconY := (RectHeight - 16) div 2
    else if FImages <> NIL then IconY := (RectHeight - FImages.Height) div 2;
    TextY := (RectHeight - (Abs(Font.Height)+3)) div 2;

    // set colors and font style:
    if Style.BackgroundColor <> clNone then
      Canvas.Brush.Color := Style.BackgroundColor;
    if Style.CaptionColor <> clNone then Canvas.Font.Color := Style.CaptionColor;
    Canvas.Font.Style := Style.CaptionStyle;

    // draw text:
    if Box then begin
      Canvas.TextRect(Rect, Rect.Left + TextX, Rect.Top + TextY + 1, Style.Caption)
    end
    else begin
      Canvas.TextRect(Rect, Rect.Left + TextX, Rect.Top + TextY + 1, Style.Caption);
    end;

    // draw image:
    if (Style.Icon <> 0) then begin
      DrawIconEx(Canvas.Handle, Rect.Left + IconX, Rect.Top + IconY, Style.Icon, 16, 16, 0, 0, DI_NORMAL);
    end
    else if (FImages <> NIL) and (Style.ImageIndex >= 0) then begin
      FIgnoreImageChange := True;
      try
        OldColor := FImages.BkColor;
        try
          // FImages.BkColor := Canvas.Brush.Color;
          FImages.BkColor := clNone;
          if Box then
            FImages.Draw(Canvas, Rect.Left + IconX, Rect.Top + IconY + 0, Style.ImageIndex, True)
          else
            FImages.Draw(Canvas, Rect.Left + IconX, Rect.Top + IconY + 1, Style.ImageIndex, True);
        finally
          FImages.BkColor := OldColor;
        end;
      finally
        FIgnoreImageChange := False;
      end;
    end;

    // draw color box
    if Style.BoxColor <> clNone then begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := Style.BoxColor;
      Canvas.Pen.Color := clBlack;
      Canvas.Rectangle(Rect.Left + TextX - 17, Rect.Top + 2, Rect.Left + TextX - 3, Rect.Bottom - 2);
    end;

    // draw comment:
    if Style.Comment <> '' then begin
      Canvas.Brush.Style := bsClear;
      if Style.CommentColor <> clNone then Canvas.Font.Color := Style.CommentColor;
      Canvas.Font.Style := Style.CaptionStyle;
      Canvas.TextRect(Rect, Rect.Right - 1 - Canvas.TextWidth(Style.Comment), Rect.Top + TextY + 1, Style.Comment);
    end;

    // draw separator:
    if (Index <> 0) and not Box then begin
      Canvas.Pen.Style := psSolid;
      if Style.Separator then
        Canvas.Pen.Color := clWindowText
      else
        Canvas.Pen.Color := clWindow;
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Top);
    end;

    if odFocused in State then begin
      // clear focus rect (works because DrawFocusRect "XORs" pixels):
      Canvas.DrawFocusRect(Rect);
      Rect.Top := Rect.Top + 1;
      // draw new rect:
      Canvas.DrawFocusRect(Rect);
    end;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopCustomComboBox.SetImages(Value: TCustomImageList);
var
  H: Integer;
begin
  if FImages <> Value then begin
    if FImages <> NIL then FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> NIL then begin
      FImages.RegisterChanges(FChangeLink);
      // FImages.FreeNotification(Self);
      FDrawStyle := pcdStyled;
    end;
    MeasureItem(0, H);
    ItemHeight := H;
    Invalidate;
  end;
end;

procedure TPegtopCustomComboBox.DropDown;
var
  I: Integer;
  MaxWidth: Integer;
  TempSize: SIZE;
  TempHDC: HDC;
  SaveFont: HFont;
begin
  if FDropDownWidth > 0 then begin
    Perform(CB_SETDROPPEDWIDTH, FDropDownWidth, 0);
  end
  else if FDropDownWidth < 0 then begin
    MaxWidth := Width + FDropDownWidth;
    if Items.Count > DropDownCount then MaxWidth := MaxWidth - GetSystemMetrics(SM_CXVSCROLL);
    TempHDC := GetDC(0);
    try
      SaveFont := SelectObject(TempHDC, Font.Handle);
      try
        for I := 0 to Items.Count-1 do begin
          Windows.GetTextExtentPoint32(TempHDC, PChar(Items[I]), Length(Items[I]), TempSize);
          if TempSize.cx > MaxWidth then MaxWidth := TempSize.cx;
        end;
      finally
        SelectObject(TempHDC, SaveFont);
      end;
    finally
      ReleaseDC(0, TempHDC);
    end;
    if Items.Count > DropDownCount then MaxWidth := MaxWidth + GetSystemMetrics(SM_CXVSCROLL);
    Perform(CB_SETDROPPEDWIDTH, MaxWidth - FDropDownWidth + 100, 0);
  end;
  inherited;
end;

procedure TPegtopCustomComboBox.CNCommand(var Msg: TWMCommand);
begin
  if Msg.NotifyCode = CBN_SELENDOK then begin
    SelEndOk;
  end
  else if Msg.NotifyCode = CBN_SELENDCANCEL then begin
    SelEndCancel;
  end
  else inherited;
end;

procedure TPegtopCustomComboBox.SelEndOk;
begin
  if Assigned(FOnSelEndOk) then FOnSelEndOk(Self);
end;

procedure TPegtopCustomComboBox.SelEndCancel;
begin
  if Assigned(FOnSelEndCancel) then FOnSelEndCancel(Self);
end;

procedure TPegtopCustomComboBox.SetDrawStyle(Value: TPegtopComboBoxDrawStyle);
begin
  if FDrawStyle <> Value then begin
    FDrawStyle := Value;
    RecreateWnd;
  end;
end;

procedure TPegtopCustomComboBox.SetDefaultItemIndex(Value: Integer);
begin
  FDefaultItemIndex := Value;
  if csDesigning in ComponentState then ItemIndex := FDefaultItemIndex;
end;

procedure TPegtopCustomComboBox.SetOnGetItemStyle(Value: TPegtopComboBoxItemStyleEvent);
begin
  if (TMethod(FOnGetItemStyle).Code <> TMethod(Value).Code)
  or (TMethod(FOnGetItemStyle).Data <> TMethod(Value).Data) then begin
    FOnGetItemStyle := Value;
    if Assigned(FOnGetItemStyle) then FDrawStyle := pcdStyled;
    Invalidate;
  end;
end;

end.
