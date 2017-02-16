////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradientControls.pas
// Components: TPegtopColorGradientBox
//             (plus TPegtopCustomColorControl used by TPegtopColorDialog etc.)
// Version:    1.01
// Date:       16 Apr 2005 1.00
//             13 Sep 2005 1.02 (disabled look changed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradientBox is a control to display and define color gradients.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradientControls;

interface

uses
  Windows, Classes, Messages, Controls,
  PegtopColorControls, PegtopColorGradients, PegtopColorGradientDialogs,
  PegtopColorGradientLists;

type
  TPegtopColorGradientBoxOption = (pgbContextGlyph);
  TPegtopColorGradientBoxOptions = set of TPegtopColorGradientBoxOption;
  TPegtopColorGradientBoxPart = (pgbpNull, pgbpCaption, pgbpContext);

  TPegtopColorGradientBox = class(TPegtopCustomColorWinControl)
  private
    FInternalGradient: TPegtopCustomColorGradient;
    FGradient: TPegtopCustomColorGradient;
    FGradientLibrary: TPegtopCustomColorGradientLibrary;
    FGradientLibraryIndex: Integer;
    FDisplayGradient: TPegtopCustomColorGradient;
    FOnChange: TNotifyEvent;
    FOnPreview: TPegtopColorGradientEvent;
    FHover: TPegtopColorGradientBoxPart;
    FDown: TPegtopColorGradientBoxPart;
    FOptions: TPegtopColorGradientBoxOptions;
    FGradientOptions: TPegtopColorGradientOptions;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure DialogPreview(Sender: TObject; ColorGradient: TPegtopCustomColorGradient);
    function GetContextGlyphRect: TRect;
    procedure GradientChange(Sender: TObject);
    procedure SetGradient(Value: TPegtopCustomColorGradient);
    procedure SetGradientHook(Value: TPegtopCustomColorGradient);
    procedure SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
    procedure SetGradientLibraryIndex(Value: Integer);
    procedure SetOptions(Value: TPegtopColorGradientBoxOptions);
    procedure SetGradientOptions(Value: TPegtopColorGradientOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintRect(ClipRect: TRect); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ModifyGradient;
    procedure ShowContextMenu;
    property GradientHook: TPegtopCustomColorGradient read FGradient write SetGradientHook;
  published
    property Gradient: TPegtopCustomColorGradient read FGradient write SetGradient;
    property GradientLibrary: TPegtopCustomColorGradientLibrary read FGradientLibrary write SetGradientLibrary;
    property GradientLibraryIndex: Integer read FGradientLibraryIndex write SetGradientLibraryIndex;
    property Options: TPegtopColorGradientBoxOptions read FOptions write SetOptions default [pgbContextGlyph];
    property GradientOptions: TPegtopColorGradientOptions read FGradientOptions write SetGradientOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPreview: TPegtopColorGradientEvent read FOnPreview write FOnPreview;
    property Caption;
    property Enabled;
    property Look;
    property TabOrder;
    property TabStop;
  end;

implementation

uses
  Forms, Graphics, Menus,
  PegtopColorUtils, PegtopColorServices;

{$R *.res}

type
  TPegtopColorGradientPopupMenu = class(TPopupMenu)
  private
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  protected
    function ChangeGradient(GradientBox: TPegtopColorGradientBox;
      Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean; virtual;
    procedure DoCommand(GradientBox: TPegtopColorGradientBox; Command: Integer); virtual;
    procedure MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean); virtual;
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  ColorGradientPopupMenu: TPegtopColorGradientPopupMenu;
  ColorContextNormalBitmap: TBitmap;
  ColorContextHoverBitmap: TBitmap;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientBox.Create(AOwner: TComponent);
begin
  inherited;
  FInternalGradient := TPegtopColorGradient.Create([clBlack, clWhite]);
  TPegtopColorGradient(FInternalGradient).OnChange := GradientChange;
  FGradient := FInternalGradient;
  FDisplayGradient := FGradient;
  Width := 75;
  Height := 25;
  FOptions := [pgbContextGlyph];
  if ColorContextNormalBitmap = NIL then begin
    ColorContextNormalBitmap := TBitmap.Create;
    ColorContextNormalBitmap.LoadFromResourceName(HInstance, 'PEGTOPCOLORCONTEXTNORMALBITMAP');
  end;
  if ColorContextHoverBitmap = NIL then begin
    ColorContextHoverBitmap := TBitmap.Create;
    ColorContextHoverBitmap.LoadFromResourceName(HInstance, 'PEGTOPCOLORCONTEXTHOVERBITMAP');
  end;
end;

destructor TPegtopColorGradientBox.Destroy;
begin
  if (FGradient <> FInternalGradient) then FGradient.RemoveListener(GradientChange);
  FInternalGradient.Free;
  inherited;
end;

procedure TPegtopColorGradientBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FGradientLibrary) then begin
    FGradientLibrary := NIL;
  end;
end;

procedure TPegtopColorGradientBox.ModifyGradient;
var
  Dialog: TPegtopColorGradientDialog;
  R: Boolean;
begin
  Dialog := TPegtopColorGradientDialog.Create(Self);
  try
    Dialog.GradientHook := FGradient;
    Dialog.GradientLibrary := FGradientLibrary;
    Dialog.GradientLibraryIndex := FGradientLibraryIndex;
    Dialog.Look := Look;
    Dialog.GradientOptions := FGradientOptions;
    Dialog.OnPreview := DialogPreview;
    try
      R := Dialog.Execute;
    finally
      FDisplayGradient := FGradient; // might have changed because of OnPreview event
    end;
    Invalidate; // might have changed even when cancelled because of OnPreview event
    if R then begin
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TPegtopColorGradientBox.ShowContextMenu;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, Height));
  PopupContextMenu(P.X, P.Y);
end;

procedure TPegtopColorGradientBox.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  TextColor: TPegtopColor;
  Extent: TSize;
  ContextGlyphRect: TRect;
begin
  if ClipRect.Left < 0 then ClipRect.Left := 0;
  if ClipRect.Top < 0 then ClipRect.Top := 0;
  if ClipRect.Right > ClientWidth then ClipRect.Right := ClientWidth;
  if ClipRect.Bottom > ClientHeight then ClipRect.Bottom := ClientHeight;
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);

      // draw background:
      if not Enabled then begin
        PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth - 1, ClientHeight - 1),
          SwapColorBytes(TPegtopColor(ColorToRGB(clBtnFace))), 256, NIL, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      end
      else
      if pgoIgnoreOpacity in FGradientOptions then begin
        FDisplayGradient.DrawDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
           Point(1 - ClipRect.Left, 1 - ClipRect.Top), Point(ClientWidth - 1 - ClipRect.Left, 1 - ClipRect.Top),
          pgsLinear, FGradientOptions);
      end
      else begin
        PegtopDrawSolidRect32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          PegtopColor($FFFFFF), 0, NIL, 1 + ClipRect.Left, 1 + ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
        FDisplayGradient.BlendDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(1 - ClipRect.Left, 1 - ClipRect.Top), Point(ClientWidth - 1  - ClipRect.Left, 1 - ClipRect.Top),
          pgsLinear, FGradientOptions);
      end;

      ContextGlyphRect := GetContextGlyphRect;
      if not Enabled then TextColor := TPegtopColor(ColorToRGB(clGrayText))
      else TextColor := GetContrastColor(PegtopColor($FFFFFF)); // TODO

      if Enabled then begin
        // draw glyph:
        if pgbContextGlyph in FOptions then begin
          if GetGrayLevel(PegtopColor($FFFFFF)) < 128 then // TODO
            Bitmap.Canvas.CopyMode := cmMergePaint
          else
            Bitmap.Canvas.CopyMode := cmSrcAnd;
          if (FHover = pgbpContext) and (FDown in [pgbpNull, pgbpContext]) then
            Bitmap.Canvas.Draw(ContextGlyphRect.Left - ClipRect.Left, ContextGlyphRect.Top - ClipRect.Top,
              ColorContextHoverBitmap)
          else
            Bitmap.Canvas.Draw(ContextGlyphRect.Left - ClipRect.Left, ContextGlyphRect.Top - ClipRect.Top,
              ColorContextNormalBitmap);
        end;
      end;

      // draw text:
      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.Font.Color := TextColor.Def;
      if (FHover = pgbpCaption) and (FDown in [pgbpNull, pgbpCaption]) then
        Bitmap.Canvas.Font.Style := [fsUnderline];
      Extent := Bitmap.Canvas.TextExtent(Caption);
      Bitmap.Canvas.TextOut((ClientWidth - Extent.CX) div 2 - ClipRect.Left,
        (ClientHeight - Extent.CY) div 2 - ClipRect.Top, Caption);

      // draw focus rect:
      if Focused then begin
        Bitmap.Canvas.Brush.Color := $FFFFFF;
        Bitmap.Canvas.DrawFocusRect(Rect(2 - ClipRect.Left, 2 - ClipRect.Top, ClientWidth - 2 - ClipRect.Left, ClientHeight - 2 - ClipRect.Top));
      end;

      // draw bounds:
      if not Enabled then
        DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, ClientWidth, ClientHeight), Rect(0, 0, Bitmap.Width, Bitmap.Height), clGrayText)
      else
        DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, ClientWidth, ClientHeight), Rect(0, 0, Bitmap.Width, Bitmap.Height), $000000);

      // copy result:
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopColorGradientBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ContextGlyphRect: TRect;
begin
  SetFocus;
  ContextGlyphRect := GetContextGlyphRect;
  if Button = mbLeft then begin
    if (pgbContextGlyph in FOptions)
    and (X >= ContextGlyphRect.Left) and (Y >= ContextGlyphRect.Top)
    and (X < ContextGlyphRect.Right) and (Y < ContextGlyphRect.Bottom) then begin
      FDown := pgbpContext;
    end
    else begin
      FDown := pgbpCaption;
    end;
  end;
  inherited;
  Invalidate;
end;

procedure TPegtopColorGradientBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if Button = mbLeft then begin
    if (FDown = pgbpCaption) and (FHover = pgbpCaption) then begin
      ModifyGradient;
    end
    else if (FDown = pgbpContext) and (FHover = pgbpContext) then begin
      P := ClientToScreen(Point(X, Y));
      PopupContextMenu(P.X, P.Y);
    end;
  end;
  FDown := pgbpNull;
  inherited;
end;

procedure TPegtopColorGradientBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ContextGlyphRect: TRect;
begin
  ContextGlyphRect := GetContextGlyphRect;
  // check hover:
  if (pgbContextGlyph in FOptions)
  and (X >= ContextGlyphRect.Left) and (Y >= ContextGlyphRect.Top)
  and (X < ContextGlyphRect.Right) and (Y < ContextGlyphRect.Bottom) then begin
    if FHover <> pgbpContext then begin
      FHover := pgbpContext;
      Invalidate;
    end;
  end
  else begin
    if FHover <> pgbpCaption then begin
      FHover := pgbpCaption;
      Invalidate;
    end;
  end;
  inherited;
end;

procedure TPegtopColorGradientBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then ModifyGradient;
  inherited;
end;

procedure TPegtopColorGradientBox.DoMouseLeave;
begin
  if FHover <> pgbpNull then begin
    FHover := pgbpNull;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopColorGradientBox.PopupContextMenu(const X, Y: Integer);
begin
  if ColorGradientPopupMenu = NIL then ColorGradientPopupMenu := TPegtopColorGradientPopupMenu.Create(Application);
  ColorGradientPopupMenu.PopupComponent := Self;
  ColorGradientPopupMenu.Popup(X, Y);
end;

procedure TPegtopColorGradientBox.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then
    ShowContextMenu
  else
    PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;

procedure TPegtopColorGradientBox.WMSetText(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorGradientBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorGradientBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorGradientBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorGradientBox.DialogPreview(Sender: TObject; ColorGradient: TPegtopCustomColorGradient);
begin
  FDisplayGradient := ColorGradient;
  Invalidate;
  if Assigned(FOnPreview) then FOnPreview(Self, FGradient);
end;

function TPegtopColorGradientBox.GetContextGlyphRect: TRect;
begin
  Result := Bounds(ClientWidth - ColorContextNormalBitmap.Width - 4,
    (ClientHeight - ColorContextNormalBitmap.Height) div 2,
    ColorContextNormalBitmap.Width, ColorContextNormalBitmap.Height);
end;

procedure TPegtopColorGradientBox.GradientChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TPegtopColorGradientBox.SetGradient(Value: TPegtopCustomColorGradient);
begin
  FGradient.Assign(Value);
end;

procedure TPegtopColorGradientBox.SetGradientHook(Value: TPegtopCustomColorGradient);
begin
  if Value = NIL then Value := FInternalGradient;
  if FGradient <> Value then begin
    if (FGradient <> FInternalGradient) then FGradient.RemoveListener(GradientChange);
    FGradient := Value;
    if (FGradient <> FInternalGradient) then FGradient.AddListener(GradientChange);
    FDisplayGradient := FGradient;
    Invalidate;
  end;
end;

procedure TPegtopColorGradientBox.SetGradientLibrary(Value: TPegtopCustomColorGradientLibrary);
begin
  if FGradientLibrary <> Value then begin
    if Assigned(FGradientLibrary) then begin
      FGradientLibrary.RemoveFreeNotification(Self);
    end;
    FGradientLibrary := Value;
    if Assigned(FGradientLibrary) then begin
      // if FGradientLibrary.Items.Count > 0 then FGradientLibraryIndex := 0 else FGradientLibraryIndex := -1;
      FGradientLibrary.FreeNotification(Self);
    end;
  end;
end;

procedure TPegtopColorGradientBox.SetGradientLibraryIndex(Value: Integer);
begin
  if Value < -1 then Value := -1;
  // else if FGradientLibrary = NIL then Value := -1
  // else if Value > FGradientLibrary.Items.Count - 1 then Value := FGradientLibrary.Items.Count - 1;
  if FGradientLibraryIndex <> Value then begin
    FGradientLibraryIndex := Value;
  end;
end;

procedure TPegtopColorGradientBox.SetOptions(Value: TPegtopColorGradientBoxOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TPegtopColorGradientBox.SetGradientOptions(Value: TPegtopColorGradientOptions);
begin
  if FGradientOptions <> Value then begin
    FGradientOptions := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientPopupMenu
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientPopupMenu.Create(AOwner: TComponent);
const
  MenuCaptions: array[0..6] of String = (
    '&Modify', '&Copy', '&Paste', '-',
    '&Blur', 'Distribute keys &evenly', '&Flip keys'
  );
  BlurMenuCaptions: array[0..3] of String = (
    '5%', '10%', '25%', '50%'
  );
  function CreateItem(Caption: TCaption; Tag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Caption;
    Result.Tag := Tag;
    Result.Default := Tag = 0;
    if Result.Caption <> '-' then begin
      Result.OnClick := MenuItemClick;
      Result.OnDrawItem := MenuItemDrawItem;
      Result.OnMeasureItem := MenuItemMeasureItem;
    end;
  end;
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  OwnerDraw := True;
  for I := Low(MenuCaptions) to High(MenuCaptions) do begin
    MenuItem := CreateItem(MenuCaptions[I], I);
    Items.Add(MenuItem);
  end;
  for I := Low(BlurMenuCaptions) to High(BlurMenuCaptions) do begin
    MenuItem := CreateItem(BlurMenuCaptions[I], 200 + I);
    Items[4].Add(MenuItem);
  end;
end;

procedure TPegtopColorGradientPopupMenu.DoPopup(Sender: TObject);
var
  Component: TComponent;
begin
  Component := TPopupMenu(Sender).PopupComponent;
  if Component is TPegtopColorGradientBox then begin
    Items[2].Enabled := ColorClipboard.HasColorGradient;
  end;
  inherited;
end;

procedure TPegtopColorGradientPopupMenu.MenuItemClick(Sender: TObject);
var
  Component: TComponent;
begin
  // since this event handler is not assigned to other objects,
  // we know that Sender is a TMenuItem of a TPopupMenu
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TPegtopColorGradientBox then begin
    DoCommand(TPegtopColorGradientBox(Component), TComponent(Sender).Tag);
  end;
end;

function TPegtopColorGradientPopupMenu.ChangeGradient(GradientBox: TPegtopColorGradientBox;
  Gradient: TPegtopCustomColorGradient; Command: Integer): Boolean;
const
  BlurStrength: array[0..3] of Integer = (13, 26, 64, 128);
var
  TempGradient: TPegtopCustomColorGradient;
begin
  Result := False;
  case Command of
    2:        if ColorClipboard.HasColorGradient then begin
                TempGradient := TPegtopColorGradient.Create([]);
                try
                  if ColorClipboard.GetColorGradient(TempGradient) then begin
                    Gradient.Color.Assign(TempGradient.Color);
                    Result := True;
                  end;
                finally
                  TempGradient.Free;
                end;
                if Assigned(GradientBox.OnChange) then GradientBox.OnChange(Self);
              end;
    5:        begin
                Gradient.Color.Keys.DistributeEvenly;
                Result := True;
                if Assigned(GradientBox.OnChange) then GradientBox.OnChange(Self);
              end;
    6:        begin
                Gradient.Color.Keys.Flip;
                Result := True;
                if Assigned(GradientBox.OnChange) then GradientBox.OnChange(Self);
              end;
    200..203: begin
                Gradient.Color.Keys.Blur(BlurStrength[Command - 200]);
                Result := True;
                if Assigned(GradientBox.OnChange) then GradientBox.OnChange(Self);
              end;
  end;
end;

procedure TPegtopColorGradientPopupMenu.DoCommand(GradientBox: TPegtopColorGradientBox; Command: Integer);
begin
  case Command of
    0: GradientBox.ModifyGradient;
    1: ColorClipboard.SetColorGradient(GradientBox.GradientHook);
  end;
  ChangeGradient(GradientBox, GradientBox.GradientHook, Command);
end;

procedure TPegtopColorGradientPopupMenu.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  CaptionRect: TRect;
  Component: TComponent;
  PreviewGradient: TPegtopCustomColorGradient;
begin
  if Sender is TMenuItem then begin
    CaptionRect := Rect(ARect.Left + ARect.Bottom - ARect.Top + 2, ARect.Top + 2, ARect.Right, ARect.Bottom);
    if TMenuItem(Sender).Default then ACanvas.Font.Style := [fsBold];
    ACanvas.FillRect(ARect);
    if not TMenuItem(Sender).Enabled then begin
      if not Selected then begin
        OffsetRect(CaptionRect, 1, 1);
        ACanvas.Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
          CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT);
        OffsetRect(CaptionRect, -1, -1);
      end;
      if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
        ACanvas.Font.Color := clBtnHighlight
      else
        ACanvas.Font.Color := clBtnShadow;
    end;
    DrawText(ACanvas.Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
      CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT);
    Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
    if Component is TPegtopColorGradientBox then begin
      PreviewGradient := TPegtopColorGradient.Create([]);
      try
        PreviewGradient.Assign(TPegtopColorGradientBox(Component).GradientHook);
        if ChangeGradient(TPegtopColorGradientBox(Component), PreviewGradient, TComponent(Sender).Tag) then begin
          PreviewGradient.Seamless := TPegtopColorGradientBox(Component).GradientHook.Seamless;
          PreviewGradient.Draw(ACanvas,
            Rect(ARect.Left + 3, ARect.Top + 3, ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3),
            Point(ARect.Left + 3, ARect.Top + 3), Point(ARect.Left + ARect.Bottom - ARect.Top - 3, ARect.Bottom - 3));
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := clBlack;
          ACanvas.Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Left + ARect.Bottom - ARect.Top - 2, ARect.Bottom - 2);
        end;
      finally
        PreviewGradient.Free;
      end;
    end;
  end;
end;

procedure TPegtopColorGradientPopupMenu.MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  CaptionRect: TRect;
begin
  if Sender is TMenuItem then begin
    DrawText(ACanvas.Handle, PChar(TMenuItem(Sender).Caption), Length(TMenuItem(Sender).Caption),
      CaptionRect, DT_SINGLELINE or DT_TOP or DT_LEFT or DT_CALCRECT);
    Width := Captionrect.Right - CaptionRect.Left + 8 + Height;
  end;
end;

initialization
  ColorGradientPopupMenu := NIL;
  ColorContextNormalBitmap := NIL;
  ColorContextHoverBitmap := NIL;
finalization
  ColorContextNormalBitmap.Free;
  ColorContextHoverBitmap.Free;
end.
