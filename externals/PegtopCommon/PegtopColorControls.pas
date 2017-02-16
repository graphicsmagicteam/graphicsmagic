////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorControls.pas
// Components: TPegtopColorBox
//             (plus TPegtopCustomColorControl used by TPegtopColorDialog etc.)
// Version:    1.02
// Date:       24 Jan 2005 1.00
//             20 May 2005 1.01 (eyedropper added)
//             13 Sep 2005 1.02 (disabled look changed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorBox is a control to display and define colors.
// Supports dithering for HighColor display mode and below.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorControls;

interface

uses
  Windows, Messages, Classes, Graphics, Controls,
  PegtopColorUtils, PegtopColorFilters;

type
  TPegtopColorControlLook = (pclRect, pclRoundedRect, pclLowered, pclRaised);
  TPegtopColorEvent = procedure(Sender: TObject; Color: TColor) of object;

  TPegtopCustomColorControl = class(TGraphicControl)
  private
    FLook: TPegtopColorControlLook;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure SetLook(Value: TPegtopColorControlLook);
  protected
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure ChangeCursor(const C: TCursor);
    procedure Paint; override;
    procedure PaintRect(ClipRect: TRect); virtual; abstract;
    procedure BlendPixel(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
      const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
    procedure DrawHLine(const Origin: Pointer; const Pitch: Integer;
      const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
    procedure DrawVLine(const Origin: Pointer; const Pitch: Integer;
      const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
    procedure PaintSolidRect(const Origin: Pointer; const Pitch: Integer;
      BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
      const OffsetX, OffsetY: Integer; const ClipRect: TRect);
    procedure DrawBounds(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect; const Color: TColor);
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLEave write FOnMouseLeave;
  protected
    property Look: TPegtopColorControlLook read FLook write SetLook default pclRoundedRect;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPegtopCustomColorWinControl = class(TCustomControl)
  private
    FLook: TPegtopColorControlLook;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure SetLook(Value: TPegtopColorControlLook);
  protected
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure ChangeCursor(const C: TCursor);
    procedure Paint; override;
    procedure PaintRect(ClipRect: TRect); virtual; abstract;
    procedure BlendPixel(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
      const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
    procedure DrawHLine(const Origin: Pointer; const Pitch: Integer;
      const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
    procedure DrawVLine(const Origin: Pointer; const Pitch: Integer;
      const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
    procedure PaintSolidRect(const Origin: Pointer; const Pitch: Integer;
      BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
      const OffsetX, OffsetY: Integer; const ClipRect: TRect);
    procedure DrawBounds(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect; const Color: TColor);
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLEave write FOnMouseLeave;
  protected
    property Look: TPegtopColorControlLook read FLook write SetLook default pclRoundedRect;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPegtopColorBoxOption = (pcbEyedropper, pcbContextGlyph);
  TPegtopColorBoxOptions = set of TPegtopColorBoxOption;
  TPegtopColorBoxPart = (pcbpNull, pcbpCaption, pcbpContext, pcbpEyedropper);

  TPegtopColorBox = class(TPegtopCustomColorWinControl)
  private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FOnPreview: TPegtopColorEvent;
    FHover: TPegtopColorBoxPart;
    FDown: TPegtopColorBoxPart;
    FEyedropperColor: TColor;
    FEyedropperCursor: TCursor;
    FOptions: TPegtopColorBoxOptions;
    procedure StartEyedropper(const Hotspot: TPoint);
    procedure StopEyedropper(const ApplyColor: Boolean);
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure DialogPreview(Sender: TObject; Color: TColor);
    function GetEyedropperRect: TRect;
    function GetEyedropperCenter: TPoint;
    function GetContextGlyphRect: TRect;
    procedure SetColor(Value: TColor);
    procedure SetOptions(Value: TPegtopColorBoxOptions);
  protected
    procedure PaintRect(ClipRect: TRect); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PopupContextMenu(const X, Y: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectColor;
    procedure ShowContextMenu;
    function GetDesktopColor(P: TPoint): TColor;
  published
    property Color: TColor read FColor write SetColor;
    property Options: TPegtopColorBoxOptions read FOptions write SetOptions default [pcbEyedropper, pcbContextGlyph];
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPreview: TPegtopColorEvent read FOnPreview write FOnPreview;
    property Caption;
    property Enabled;
    property Look;
    property TabOrder;
    property TabStop;
  end;

procedure PegtopBlendPixel32(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
  const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
procedure PegtopDrawHLine32(const Origin: Pointer; const Pitch: Integer;
  const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
procedure PegtopDrawVLine32(const Origin: Pointer; const Pitch: Integer;
  const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
procedure PegtopDrawSolidRect32(const Origin: Pointer; const Pitch: Integer;
  BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
  const OffsetX, OffsetY: Integer; const ClipRect: TRect);
procedure PegtopDrawBounds32(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect;
  Look: TPegtopColorControlLook = pclRect; Color: TColor = clBlack; Background: TColor = clBtnFace);

implementation

uses
  Forms, Menus, SysUtils,
  PegtopColorDialogs, PegtopColorServices, PegtopDesktopMagnifiers,
  PegtopCursors;

{$R *.res}

type
  TColorCrackerControl = class(TControl)
  public
    property Color;
  end;

  TPegtopColorPopupMenu = class(TPopupMenu)
  private
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

const
  EyedropperWidth = 12;
  EyedropperHeight = 12;

resourcestring
  PegtopColorSelect      = '&Select...';
  PegtopColorCopy        = '&Copy';
  PegtopColorPaste       = '&Paste';
  PegtopColorBlack       = '&Black';
  PegtopColorGray        = '50% &Gray';
  PegtopColorWhite       = '&White';
  PegtopUserDefinedColor = 'User defined &';

var
  ColorPopupMenu: TPegtopColorPopupMenu;
  ColorContextNormalBitmap: TBitmap;
  ColorContextHoverBitmap: TBitmap;

////////////////////////////////////////////////////////////////////////////////
// 32 bit drawing procedures
////////////////////////////////////////////////////////////////////////////////

procedure PegtopBlendPixel32(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
  const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
var
  P: PPegtopColor;
begin
  if (X >= ClipRect.Left) and (X < ClipRect.Right)
  and (Y >= ClipRect.Top) and (Y < ClipRect.Bottom) then begin
    P := Pointer(Integer(Origin) + Y * Pitch + X * 4);
    P^.R := P^.R + (Color.B - P^.R) * Alpha div 256;
    P^.G := P^.G + (Color.G - P^.G) * Alpha div 256;
    P^.B := P^.B + (Color.R - P^.B) * Alpha div 256;
  end;
end;

procedure PegtopDrawHLine32(const Origin: Pointer; const Pitch: Integer;
  const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
var
  X, X3, X4: Integer;
  P: PPegtopColor;
begin
  if (Y >= ClipRect.Top) and (Y < ClipRect.Bottom) then begin
    if X1 > X2 then begin
      X3 := X2;
      X4 := X1 + 1;
    end
    else begin
      X3 := X1;
      X4 := X2 + 1;
    end;
    if (X4 >= ClipRect.Left) and (X3 < ClipRect.Right) then begin
      if X3 < ClipRect.Left then X3 := ClipRect.Left;
      if X4 > ClipRect.Right then X4 := ClipRect.Right;
      P := Pointer(Integer(Origin) + Y * Pitch + X3 * 4);
      for X := X3 to X4 - 1 do begin
        P^ := Color;
        Inc(P);
      end;
    end;
  end;
end;

procedure PegtopDrawVLine32(const Origin: Pointer; const Pitch: Integer;
  const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
var
  Y, Y3, Y4: Integer;
  P: PPegtopColor;
begin
  if (X >= ClipRect.Left) and (X < ClipRect.Right) then begin
    if Y1 > Y2 then begin
      Y3 := Y2;
      Y4 := Y1 + 1;
    end
    else begin
      Y3 := Y1;
      Y4 := Y2 + 1;
    end;
    if (Y4 >= ClipRect.Top) and (Y3 < ClipRect.Bottom) then begin
      if Y3 < ClipRect.Top then Y3 := ClipRect.Top;
      if Y4 > ClipRect.Bottom then Y4 := ClipRect.Bottom;
      P := Pointer(Integer(Origin) + Y3 * Pitch + X * 4);
      for Y := Y3 to Y4 - 1 do begin
        P^ := Color;
        P := Pointer(Integer(P) + Pitch);
      end;
    end;
  end;
end;

procedure PegtopDrawSolidRect32(const Origin: Pointer; const Pitch: Integer;
  BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
  const OffsetX, OffsetY: Integer; const ClipRect: TRect);
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C2: TPegtopColor;
  Ground: Integer;
  R, G, B: Integer; // work with integers instead of bytes
begin
  if (BoundsRect.Left < ClipRect.Right) and (BoundsRect.Right > ClipRect.Left)
  and (BoundsRect.Top < ClipRect.Bottom) and (BoundsRect.Bottom > ClipRect.Top) then begin
    if BoundsRect.Left < ClipRect.Left then BoundsRect.Left := ClipRect.Left;
    if BoundsRect.Right > ClipRect.Right then BoundsRect.Right := ClipRect.Right;
    if BoundsRect.Top < ClipRect.Top then BoundsRect.Top := ClipRect.Top;
    if BoundsRect.Bottom > ClipRect.Bottom then BoundsRect.Bottom := ClipRect.Bottom;
    Q := Pointer(Integer(Origin) + BoundsRect.Top * Pitch + BoundsRect.Left * 4);
    if Alpha = 256 then begin
      if Assigned(Filter) then begin
        for Y := BoundsRect.Top to BoundsRect.Bottom - 1 do begin
          P := Q;
          for X := BoundsRect.Left to BoundsRect.Right - 1 do begin
            C2 := C;
            Filter.Apply(C2, X + OffsetX, Y + OffsetY);
            P^ := C2;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end
      else begin
        for Y := BoundsRect.Top to BoundsRect.Bottom - 1 do begin
          P := Q;
          for X := BoundsRect.Left to BoundsRect.Right - 1 do begin
            P^ := C;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    end
    else begin
      if Assigned(Filter) then begin
        R := C.R;
        G := C.G;
        B := C.B;
        for Y := BoundsRect.Top to BoundsRect.Bottom - 1 do begin
          P := Q;
          for X := BoundsRect.Left to BoundsRect.Right - 1 do begin
            Ground := 255 xor (((((X + OffsetX) shr 3) + ((Y + OffsetY) shr 3)) and 1) shl 6);
            C2.R := Ground + (R - Ground) * Alpha div 256;
            C2.G := Ground + (G - Ground) * Alpha div 256;
            C2.B := Ground + (B - Ground) * Alpha div 256;
            Filter.Apply(C2, X + OffsetX, Y + OffsetY);
            P^ := C2;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end
      else begin
        R := C.R;
        G := C.G;
        B := C.B;
        for Y := BoundsRect.Top to BoundsRect.Bottom - 1 do begin
          P := Q;
          for X := BoundsRect.Left to BoundsRect.Right - 1 do begin
            Ground := 255 xor (((((X + OffsetX) shr 3) + ((Y + OffsetY) shr 3)) and 1) shl 6);
            P^.R := Ground + (R - Ground) * Alpha div 256;
            P^.G := Ground + (G - Ground) * Alpha div 256;
            P^.B := Ground + (B - Ground) * Alpha div 256;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    end;
  end;
end;

procedure PegtopDrawBounds32(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect;
  Look: TPegtopColorControlLook = pclRect; Color: TColor = clBlack; Background: TColor = clBtnFace);
var
  C1, C2: TPegtopColor;
begin
  if Look = pclLowered then begin
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Top, TPegtopColor(ColorToRGB(clBtnShadow)), ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Top + 1, BoundsRect.Bottom - 2, TPegtopColor(ColorToRGB(clBtnShadow)), ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Top + 1, BoundsRect.Bottom - 2, TPegtopColor(ColorToRGB(clBtnHighlight)), ClipRect);
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Bottom - 1, TPegtopColor(ColorToRGB(clBtnHighlight)), ClipRect);
  end
  else if Look = pclRaised then begin
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Top, TPegtopColor(ColorToRGB(clBtnHighlight)), ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Top + 1, BoundsRect.Bottom - 2, TPegtopColor(ColorToRGB(clBtnHighlight)), ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Top + 1, BoundsRect.Bottom - 2, TPegtopColor(ColorToRGB(clBtnShadow)), ClipRect);
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Bottom - 1, TPegtopColor(ColorToRGB(clBtnShadow)), ClipRect);
  end
  else begin
    C1 := SwapColorBytes(TPegtopColor(ColorToRGB(Color)));
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Top, C1, ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Top + 1, BoundsRect.Bottom - 2, C1, ClipRect);
    PegtopDrawVLine32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Top + 1, BoundsRect.Bottom - 2, C1, ClipRect);
    PegtopDrawHLine32(Origin, Pitch, BoundsRect.Left, BoundsRect.Right - 1, BoundsRect.Bottom - 1, C1, ClipRect);
  end;
  if Look = pclRoundedRect then begin
    C1 := TPegtopColor(ColorToRGB(Color));
    C2 := TPegtopColor(ColorToRGB(Background));
    // top left:
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left, BoundsRect.Top, C2, 224, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left + 1, BoundsRect.Top, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left, BoundsRect.Top + 1, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left + 1, BoundsRect.Top + 1, C1, 96, ClipRect);
    // top right:
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Top, C2, 224, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 2, BoundsRect.Top, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Top + 1, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 2, BoundsRect.Top + 1, C1, 96, ClipRect);
    // bottom left:
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left, BoundsRect.Bottom - 1, C2, 224, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left + 1, BoundsRect.Bottom - 1, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left, BoundsRect.Bottom - 2, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Left + 1, BoundsRect.Bottom - 2, C1, 96, ClipRect);
    // bottom right:
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Bottom - 1, C2, 224, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 2, BoundsRect.Bottom - 1, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 1, BoundsRect.Bottom - 2, C2, 96, ClipRect);
    PegtopBlendPixel32(Origin, Pitch, BoundsRect.Right - 2, BoundsRect.Bottom - 2, C1, 96, ClipRect);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorControl
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FLook := pclRoundedRect;
end;

procedure TPegtopCustomColorControl.Paint;
begin
  PaintRect(Canvas.ClipRect);
end;

procedure TPegtopCustomColorControl.CMMouseEnter(var Msg: TMessage);
begin
  DoMouseEnter;
  inherited;
end;

procedure TPegtopCustomColorControl.CMMouseLeave(var Msg: TMessage);
begin
  DoMouseLeave;
  inherited;
end;

procedure TPegtopCustomColorControl.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPegtopCustomColorControl.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TPegtopCustomColorControl.ChangeCursor(const C: TCursor);
begin
  if Cursor <> C then begin
    Cursor := C;
    Parent.Perform(WM_SETCURSOR, Parent.Handle, HTCLIENT);
  end;
end;

procedure TPegtopCustomColorControl.BlendPixel(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
  const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
begin
  PegtopBlendPixel32(Origin, Pitch, X, Y, Color, Alpha, ClipRect);
end;

procedure TPegtopCustomColorControl.DrawHLine(const Origin: Pointer; const Pitch: Integer;
  const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
begin
  PegtopDrawHLine32(Origin, Pitch, X1, X2, Y, Color, ClipRect);
end;

procedure TPegtopCustomColorControl.DrawVLine(const Origin: Pointer; const Pitch: Integer;
  const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
begin
  PegtopDrawVLine32(Origin, Pitch, X, Y1, Y2, Color, ClipRect);
end;

procedure TPegtopCustomColorControl.PaintSolidRect(const Origin: Pointer; const Pitch: Integer;
  BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
  const OffsetX, OffsetY: Integer; const ClipRect: TRect);
begin
  PegtopDrawSolidRect32(Origin, Pitch, BoundsRect, C, Alpha, Filter, OffsetX, OffsetY, ClipRect);
end;

procedure TPegtopCustomColorControl.DrawBounds(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect; const Color: TColor);
var
  Background: TColor;
begin
  if Parent = NIL then Background := clBtnFace
  else Background := TColorCrackerControl(Parent).Color;
  PegtopDrawBounds32(Origin, Pitch, BoundsRect, ClipRect, FLook, Color, Background);
end;

procedure TPegtopCustomColorControl.SetLook(Value: TPegtopColorControlLook);
begin
  if FLook <> Value then begin
    FLook := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorWinControl
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorWinControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  FLook := pclRoundedRect;
end;

procedure TPegtopCustomColorWinControl.Paint;
begin
  PaintRect(Canvas.ClipRect);
end;

procedure TPegtopCustomColorWinControl.CMMouseEnter(var Msg: TMessage);
begin
  DoMouseEnter;
  inherited;
end;

procedure TPegtopCustomColorWinControl.CMMouseLeave(var Msg: TMessage);
begin
  DoMouseLeave;
  inherited;
end;

procedure TPegtopCustomColorWinControl.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPegtopCustomColorWinControl.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TPegtopCustomColorWinControl.ChangeCursor(const C: TCursor);
begin
  if Cursor <> C then begin
    Cursor := C;
    Perform(WM_SETCURSOR, Parent.Handle, HTCLIENT);
  end;
end;

procedure TPegtopCustomColorWinControl.BlendPixel(const Origin: Pointer; const Pitch: Integer; const X, Y: Integer;
  const Color: TPegtopColor; const Alpha: Integer; const ClipRect: TRect);
begin
  PegtopBlendPixel32(Origin, Pitch, X, Y, Color, Alpha, ClipRect);
end;

procedure TPegtopCustomColorWinControl.DrawHLine(const Origin: Pointer; const Pitch: Integer;
  const X1, X2, Y: Integer; const Color: TPegtopColor; const ClipRect: TRect);
begin
  PegtopDrawHLine32(Origin, Pitch, X1, X2, Y, Color, ClipRect);
end;

procedure TPegtopCustomColorWinControl.DrawVLine(const Origin: Pointer; const Pitch: Integer;
  const X, Y1, Y2: Integer; const Color: TPegtopColor; const ClipRect: TRect);
begin
  PegtopDrawVLine32(Origin, Pitch, X, Y1, Y2, Color, ClipRect);
end;

procedure TPegtopCustomColorWinControl.PaintSolidRect(const Origin: Pointer; const Pitch: Integer;
  BoundsRect: TRect; const C: TPegtopColor; const Alpha: Integer; const Filter: TPegtopColorFilter;
  const OffsetX, OffsetY: Integer; const ClipRect: TRect);
begin
  PegtopDrawSolidRect32(Origin, Pitch, BoundsRect, C, Alpha, Filter, OffsetX, OffsetY, ClipRect);
end;

procedure TPegtopCustomColorWinControl.DrawBounds(const Origin: Pointer; const Pitch: Integer; const BoundsRect, ClipRect: TRect; const Color: TColor);
var
  Background: TColor;
begin
  if Parent = NIL then Background := clBtnFace
  else Background := TColorCrackerControl(Parent).Color;
  PegtopDrawBounds32(Origin, Pitch, BoundsRect, ClipRect, FLook, Color, Background);
end;

procedure TPegtopCustomColorWinControl.SetLook(Value: TPegtopColorControlLook);
begin
  if FLook <> Value then begin
    FLook := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorBox
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorBox.Create(AOwner: TComponent);
begin
  inherited;
  Width := 75;
  Height := 25;
  FOptions := [pcbEyedropper, pcbContextGlyph];
  if ColorContextNormalBitmap = NIL then begin
    ColorContextNormalBitmap := TBitmap.Create;
    ColorContextNormalBitmap.LoadFromResourceName(HInstance, 'PEGTOPCOLORCONTEXTNORMALBITMAP');
  end;
  if ColorContextHoverBitmap = NIL then begin
    ColorContextHoverBitmap := TBitmap.Create;
    ColorContextHoverBitmap.LoadFromResourceName(HInstance, 'PEGTOPCOLORCONTEXTHOVERBITMAP');
  end;
end;

procedure TPegtopColorBox.SelectColor;
var
  Dialog: TPegtopColorDialog;
begin
  Dialog := TPegtopColorDialog.Create(Self);
  try
    Dialog.Color := Color;
    Dialog.Look := Look;
    Dialog.OnPreview := DialogPreview;
    if Dialog.Execute then begin
      SetColor(Dialog.Color); // sets color and invalidates control
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TPegtopColorBox.ShowContextMenu;
var
  P: TPoint;
begin
  P := ClientToScreen(Point(0, Height));
  PopupContextMenu(P.X, P.Y);
end;

procedure TPegtopColorBox.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  SolidColor, TextColor: TPegtopColor;
  Filter: TPegtopColorFilter;
  Extent: TSize;
  EyedropperRect: TRect;
  ContextGlyphRect: TRect;
begin
  if ClipRect.Left < 0 then ClipRect.Left := 0;
  if ClipRect.Top < 0 then ClipRect.Top := 0;
  if ClipRect.Right > ClientWidth then ClipRect.Right := ClientWidth;
  if ClipRect.Bottom > ClientHeight then ClipRect.Bottom := ClientHeight;
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    Filter := GetProperColorFilter(Canvas.Handle);
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      SolidColor := TPegtopColor(ColorToRGB(FColor));
      // if not Enabled then SolidColor := MixColors(TPegtopColor(ColorToRGB(clBtnFace)), SolidColor, 64);

      // draw background:
      if not Enabled then
        PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth - 1, ClientHeight - 1),
          SwapColorBytes(TPegtopColor(ColorToRGB(clBtnFace))), 256, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height))
      else
        PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth - 1, ClientHeight - 1),
          SwapColorBytes(SolidColor), 256, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));

      EyedropperRect := GetEyedropperRect;
      ContextGlyphRect := GetContextGlyphRect;
      if not Enabled then TextColor := TPegtopColor(ColorToRGB(clGrayText))
      else TextColor := GetContrastColor(TPegtopColor(FColor));

      // draw eydropper:
      if Enabled then begin
        if pcbEyedropper in FOptions then begin
          Bitmap.Canvas.Pen.Color := TextColor.Def;
          if  FDown = pcbpEyedropper then begin
            Bitmap.Canvas.Brush.Style := bsSolid;
            Bitmap.Canvas.Brush.Color := FEyedropperColor;
            Bitmap.Canvas.Rectangle(
              EyedropperRect.Left - ClipRect.Left,
              EyedropperRect.Top - ClipRect.Top,
              EyedropperRect.Right - ClipRect.Left,
              EyedropperRect.Bottom - ClipRect.Top
            );
          end
          else if FHover = pcbpEyedropper then begin
            Bitmap.Canvas.Brush.Style := bsClear;
            Bitmap.Canvas.Rectangle(
              EyedropperRect.Left - ClipRect.Left,
              EyedropperRect.Top - ClipRect.Top,
              EyedropperRect.Right - ClipRect.Left,
              EyedropperRect.Bottom - ClipRect.Top
            );
          end
          else begin
            Bitmap.Canvas.MoveTo(EyedropperRect.Left      - ClipRect.Left, EyedropperRect.Top    + 2 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Left      - ClipRect.Left, EyedropperRect.Top        - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Left  + 3 - ClipRect.Left, EyedropperRect.Top        - ClipRect.Top);
            Bitmap.Canvas.MoveTo(EyedropperRect.Right - 1 - ClipRect.Left, EyedropperRect.Top    + 2 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Right - 1 - ClipRect.Left, EyedropperRect.Top        - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Right - 4 - ClipRect.Left, EyedropperRect.Top        - ClipRect.Top);
            Bitmap.Canvas.MoveTo(EyedropperRect.Left      - ClipRect.Left, EyedropperRect.Bottom - 4 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Left      - ClipRect.Left, EyedropperRect.Bottom - 1 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Left  + 3 - ClipRect.Left, EyedropperRect.Bottom - 1 - ClipRect.Top);
            Bitmap.Canvas.MoveTo(EyedropperRect.Right - 1 - ClipRect.Left, EyedropperRect.Bottom - 4 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Right - 1 - ClipRect.Left, EyedropperRect.Bottom - 1 - ClipRect.Top);
            Bitmap.Canvas.LineTo(EyedropperRect.Right - 4 - ClipRect.Left, EyedropperRect.Bottom - 1 - ClipRect.Top);
          end;
        end;

        // draw glyph:
        if pcbContextGlyph in FOptions then begin
          if GetGrayLevel(SolidColor) < 128 then
            Bitmap.Canvas.CopyMode := cmMergePaint
          else
            Bitmap.Canvas.CopyMode := cmSrcAnd;
          if (FHover = pcbpContext) and (FDown in [pcbpNull, pcbpContext]) then
            Bitmap.Canvas.Draw(ContextGlyphRect.Left - ClipRect.Left, ContextGlyphRect.Top - ClipRect.Top, ColorContextHoverBitmap)
          else
            Bitmap.Canvas.Draw(ContextGlyphRect.Left - ClipRect.Left, ContextGlyphRect.Top - ClipRect.Top, ColorContextNormalBitmap);
        end;
      end;

      // draw text:
      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.Font.Color := TextColor.Def;
      if (FHover = pcbpCaption) and (FDown in [pcbpNull, pcbpCaption]) then
        Bitmap.Canvas.Font.Style := [fsUnderline];
      Extent := Bitmap.Canvas.TextExtent(Caption);
      Bitmap.Canvas.TextOut((ClientWidth - Extent.CX) div 2 - ClipRect.Left,
        (ClientHeight - Extent.CY) div 2 - ClipRect.Top, Caption);

      // draw focus rect:
      if Focused then begin
        Bitmap.Canvas.Brush.Color := SolidColor.Def;
        Bitmap.Canvas.DrawFocusRect(Rect(2 - ClipRect.Left, 2 - ClipRect.Top, ClientWidth - 2 - ClipRect.Left, ClientHeight - 2 - ClipRect.Top));
        Bitmap.Canvas.Brush.Color := TextColor.Def;
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

procedure TPegtopColorBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  EyedropperRect: TRect;
  ContextGlyphRect: TRect;
begin
  if FDown = pcbpEyedropper then begin
    StopEyedropper(False);
    FDown := pcbpNull;
  end
  else begin
    SetFocus;
    EyedropperRect := GetEyedropperRect;
    ContextGlyphRect := GetContextGlyphRect;
    if Button = mbLeft then begin
      if (pcbEyedropper in FOptions)
      and (X >= EyedropperRect.Left) and (Y >= EyedropperRect.Top)
      and (X < EyedropperRect.Right) and (Y < EyedropperRect.Bottom) then begin
        if FDown <> pcbpEyedropper then StartEyedropper(ClientToScreen(Point(X, Y)));
        FDown := pcbpEyedropper;
      end
      else if (pcbContextGlyph in FOptions)
      and (X >= ContextGlyphRect.Left) and (Y >= ContextGlyphRect.Top)
      and (X < ContextGlyphRect.Right) and (Y < ContextGlyphRect.Bottom) then begin
        FDown := pcbpContext;
      end
      else begin
        FDown := pcbpCaption;
      end;
    end;
    inherited;
  end;
  Invalidate;
end;

procedure TPegtopColorBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDown = pcbpEyedropper then StopEyedropper(True);
  if Button = mbLeft then begin
    if (FDown = pcbpCaption) and (FHover = pcbpCaption) then begin
      SelectColor;
    end
    else if (FDown = pcbpContext) and (FHover = pcbpContext) then begin
      P := ClientToScreen(Point(X, Y));
      PopupContextMenu(P.X, P.Y);
    end;
  end;
  FDown := pcbpNull;
  inherited;
end;

procedure TPegtopColorBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  EyedropperRect: TRect;
  ContextGlyphRect: TRect;
  Hotspot: TPoint;
  TargetColor: TColor;
begin
  EyedropperRect := GetEyedropperRect;
  ContextGlyphRect := GetContextGlyphRect;
  // check hover:
  if (pcbEyedropper in FOptions)
  and (X >= EyedropperRect.Left) and (Y >= EyedropperRect.Top)
  and (X < EyedropperRect.Right) and (Y < EyedropperRect.Bottom) then begin
    if FHover <> pcbpEyedropper then begin
      FHover := pcbpEyedropper;
      Invalidate;
    end;
  end
  else if (pcbContextGlyph in FOptions)
  and (X >= ContextGlyphRect.Left) and (Y >= ContextGlyphRect.Top)
  and (X < ContextGlyphRect.Right) and (Y < ContextGlyphRect.Bottom) then begin
    if FHover <> pcbpContext then begin
      FHover := pcbpContext;
      Invalidate;
    end;
  end
  else begin
    if FHover <> pcbpCaption then begin
      FHover := pcbpCaption;
      Invalidate;
    end;
  end;
  // update eyedropper:
  if FDown = pcbpEyedropper then begin
    Hotspot := ClientToScreen(Point(X, Y));
    GetDesktopMagnifierForm.AutoPosition(ClientToScreen(GetEyedropperCenter), Hotspot, False);
    GetDesktopMagnifierForm.Magnifier.Position := Hotspot;
    if (X >= EyedropperRect.Left) and (Y >= EyedropperRect.Top)
    and (X < EyedropperRect.Right) and (Y < EyedropperRect.Bottom) then begin
      if FEyedropperColor <> FColor then begin
        FEyedropperColor := FColor;
        Invalidate;
      end;
    end
    else begin
      TargetColor := GetDesktopColor(Hotspot);
      if FEyedropperColor <> TargetColor then begin
        FEyedropperColor := TargetColor;
        Invalidate;
      end;
    end;
  end;
  inherited;
end;

procedure TPegtopColorBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then SelectColor;
  inherited;
end;

procedure TPegtopColorBox.DoMouseLeave;
begin
  if FHover <> pcbpNull then begin
    FHover := pcbpNull;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopColorBox.PopupContextMenu(const X, Y: Integer);
begin
  if ColorPopupMenu = NIL then ColorPopupMenu := TPegtopColorPopupMenu.Create(Application);
  ColorPopupMenu.PopupComponent := Self;
  ColorPopupMenu.Popup(X, Y);
end;

procedure TPegtopColorBox.WMContextMenu(var Msg: TWMContextMenu);
begin
  if (Msg.XPos = -1) and (Msg.YPos = -1) then
    ShowContextMenu
  else
    PopupContextMenu(Msg.XPos, Msg.YPos);
  // inherited is not called because we already did popup the context menu
end;

procedure TPegtopColorBox.WMSetText(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPegtopColorBox.StartEyedropper(const Hotspot: TPoint);
begin
  GetDesktopMagnifierForm.Magnifier.Position := Hotspot;
  FEyedropperColor := FColor;
  FEyedropperCursor := Screen.Cursor;
  Screen.Cursor := crCircle;
  GetDesktopMagnifierForm.AutoPosition(ClientToScreen(GetEyedropperCenter), Hotspot, True);
  SetWindowPos(GetDesktopMagnifierForm.Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPegtopColorBox.StopEyedropper(const ApplyColor: Boolean);
begin
  SetWindowPos(GetDesktopMagnifierForm.Handle, 0, 0, 0, 0, 0,
    SWP_HIDEWINDOW or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  if Screen.Cursor = crCircle then Screen.Cursor := FEyedropperCursor;
  if ApplyColor and (FColor <> FEyedropperColor) then begin
    if FColor <> FEyedropperColor then begin
      SetColor(FEyedropperColor); // sets color and invalidates control
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

function TPegtopColorBox.GetDesktopColor(P: TPoint): TColor;
var
  DC: THandle;
begin
  DC := GetDC(0);
  try
    Result := GetPixel(DC, P.X, P.Y);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TPegtopColorBox.DialogPreview(Sender: TObject; Color: TColor);
begin
  if FColor <> Color then begin
    FColor := Color and $FFFFFF;
    Invalidate;
    if Assigned(FOnPreview) then FOnPreview(Self, Color);
  end;
end;

function TPegtopColorBox.GetEyedropperRect: TRect;
begin
  Result := Bounds(4, (ClientHeight - EyedropperHeight) div 2,
    EyedropperWidth, EyedropperHeight);
end;

function TPegtopColorBox.GetEyedropperCenter: TPoint;
var
  R: TRect;
begin
  R := GetEyedropperRect;
  Result.X := (R.Left + R.Right) div 2;
  Result.Y := (R.Top + R.Bottom) div 2;
end;

function TPegtopColorBox.GetContextGlyphRect: TRect;
begin
  Result := Bounds(ClientWidth - ColorContextNormalBitmap.Width - 4,
    (ClientHeight - ColorContextNormalBitmap.Height) div 2,
    ColorContextNormalBitmap.Width, ColorContextNormalBitmap.Height);
end;

procedure TPegtopColorBox.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TPegtopColorBox.SetOptions(Value: TPegtopColorBoxOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorPopupMenu
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorPopupMenu.Create(AOwner: TComponent);
const
  MenuCaptions: array[0..7] of String = (
    PegtopColorSelect, PegtopColorCopy, PegtopColorPaste, '-',
    PegtopColorBlack, PegtopColorGray, PegtopColorWhite, '-');
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  OwnerDraw := True;
  for I := Low(MenuCaptions) to High(MenuCaptions) + 5 do begin
    MenuItem := TMenuItem.Create(Self);
    if I <= High(MenuCaptions) then
      MenuItem.Caption := MenuCaptions[I]
    else
      MenuItem.Caption := PegtopUserDefinedColor + IntToStr(I - High(MenuCaptions));
    MenuItem.Tag := I;
    MenuItem.Default := I = 0;
    if MenuItem.Caption <> '-' then begin
      MenuItem.OnClick := MenuItemClick;
      MenuItem.OnDrawItem := MenuItemDrawItem;
      MenuItem.OnMeasureItem := MenuItemMeasureItem;
    end;
    Items.Add(MenuItem);
  end;
end;

procedure TPegtopColorPopupMenu.DoPopup(Sender: TObject);
begin
  Items[2].Enabled := ColorClipboard.HasColor;
  inherited;
end;

procedure TPegtopColorPopupMenu.MenuItemClick(Sender: TObject);
const
  PredefinedColors: array[4..6] of TColor = ($000000, $808080, $FFFFFF);
var
  C: TColor;
  Component: TComponent;
  ColorBox: TPegtopColorBox;
begin
  // since this event handler is not assigned to other objects,
  // we know that Sender is a TMenuItem of a TPopupMenu
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TPegtopColorBox then begin
    ColorBox := TPegtopColorBox(Component);
    case TComponent(Sender).Tag of
      0: ColorBox.SelectColor;
      1: ColorClipboard.SetSystemColor(ColorBox.Color);
      2:
        if ColorClipboard.GetSystemColor(C) then begin
          ColorBox.Color := C;
          if Assigned(ColorBox.OnChange) then ColorBox.OnChange(ColorBox);
        end;
      4..6:
        begin
          ColorBox.Color := PredefinedColors[TComponent(Sender).Tag];
          if Assigned(ColorBox.OnChange) then ColorBox.OnChange(ColorBox);
        end;
      8..12:
        begin
          ColorBox.Color := ColorDefinitionService.Colors[TComponent(Sender).Tag - 8];
          if Assigned(ColorBox.OnChange) then ColorBox.OnChange(ColorBox);
        end;
    end;
  end;
end;

procedure TPegtopColorPopupMenu.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  CaptionRect: TRect;
  C: TColor;
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
    if (TComponent(Sender).Tag >= 4)
    or ((TComponent(Sender).Tag = 2) and ColorClipboard.HasColor) then begin
      case TComponent(Sender).Tag of
        2: if ColorClipboard.GetSystemColor(C) then ACanvas.Brush.Color := C;
        4: ACanvas.Brush.Color := $000000;
        5: ACanvas.Brush.Color := $808080;
        6: ACanvas.Brush.Color := $FFFFFF;
        8..12: ACanvas.Brush.Color := ColorDefinitionService.Colors[TComponent(Sender).Tag - 8];
      end;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Pen.Color := clBlack;
      ACanvas.Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Left + ARect.Bottom - ARect.Top - 2, ARect.Bottom - 2);
    end;
  end;
end;

procedure TPegtopColorPopupMenu.MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
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
  ColorPopupMenu := NIL;
  ColorContextNormalBitmap := NIL;
  ColorContextHoverBitmap := NIL;
finalization
  ColorContextNormalBitmap.Free;
  ColorContextHoverBitmap.Free;
end.
