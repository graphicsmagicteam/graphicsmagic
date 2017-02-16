////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorDialogs.pas
// Components: TPegtopColorDialog
// Version:    1.01
// Date:       11 Jan 2005 created 1.00
//             19 Jan 2005 modified 1.01 (opacity track bar changed,
//                         color filters moved to PegtopColorFilters.pas,
//                         base control moved to PegtopColorControls.pas,
//                         user defined colors moved to PegtopColorServices.pas,
//                         "pink hue problem" fixed)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorDialog is a dialog for color selection via controls, that reflect
// a color space (RGB or HSB), and / or via direct input (RGB and HSB).
// Supports:
// * Alpha channel / opacity (optionally)
// * Dithering for HighColor display mode and below
// * Switch to web-safe colors
// * Hex code (for HTML etc.)
// * user defined colors (syncronized with other color dialogs)
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  PegtopColorControls, PegtopColorUtils;

type
  TPegtopColorDialogOption = (pcoOpacity, pcoWebSafe, pcoUserDefinedColors);
  TPegtopColorDialogOptions = set of TPegtopColorDialogOption;

  TPegtopColorDialog = class(TComponent)
  private
    FColor: TColor;
    FColorComponent: TPegtopColorComponent;
    FOpacity: Integer;
    FCaption: TCaption;
    FOptions: TPegtopColorDialogOptions;
    FLook: TPegtopColorControlLook;
    FOnPreview: TPegtopColorEvent;
    procedure FormPreview(Sender: TObject; Color: TColor);
    procedure SetOpacity(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property Color: TColor read FColor write FColor;
    property ColorComponent: TPegtopColorComponent read FColorComponent write FColorComponent default pccHue;
    property Opacity: Integer read FOpacity write SetOpacity default 100;
    property Caption: TCaption read FCaption write FCaption;
    property Options: TPegtopColorDialogOptions read FOptions write FOptions;
    property Look: TPegtopColorControlLook read FLook write FLook default pclRoundedRect;
    property OnPreview: TPegtopColorEvent read FOnPreview write FOnPreview;
  end;

implementation

uses
  Forms, StdCtrls, ExtCtrls, PegtopCursors, PegtopNumEdits,
  PegtopTrackBars, PegtopColorFilters, PegtopColorServices, PegtopCheckBoxes;

{$R *.res}

type
  TPegtopColorSpaceFunction = function (const X, Y, Z: Integer): TPegtopColor;

  TPegtopHSBChangeEvent = procedure(Sender: TObject; Hue, Sat, Bri: Integer) of object;
  TPegtopRGBChangeEvent = procedure(Sender: TObject; Red, Green, Blue: Integer) of object;

  TPegtopColorsControl = class(TPegtopCustomColorControl)
  private
    FFixComponent: TPegtopColorComponent;
    FTargeting: Boolean;
    FHue, FSat, FBri: Integer;
    FRed, FGreen, FBlue: Integer;
    FOnHSBChange: TPegtopHSBChangeEvent;
    FOnRGBChange: TPegtopRGBChangeEvent;
    FWebSafe: Boolean;
    procedure SetFixComponent(Value: TPegtopColorComponent);
    procedure SetWebSafe(Value: Boolean);
  protected
    procedure RefreshTarget; virtual; abstract;
    procedure RefreshValues; virtual; abstract;
  public
    procedure SetHSB(const Hue, Sat, Bri: Integer); virtual; abstract;
    procedure SetRGB(const Red, Green, Blue: Integer); virtual; abstract;
    property OnHSBChange: TPegtopHSBChangeEvent read FOnHSBChange write FOnHSBChange;
    property OnRGBChange: TPegtopRGBChangeEvent read FOnRGBChange write FOnRGBChange;
    property FixComponent: TPegtopColorComponent read FFixComponent write SetFixComponent;
    property WebSafe: Boolean read FWebSafe write SetWebSafe;
  end;

  TPegtopColorChart = class(TPegtopColorsControl)
  private
    FTarget: TPoint;
    procedure NotifyChange;
    procedure SetTarget(const X, Y: Integer; const Redraw: Boolean = False; const Refresh: Boolean = False);
  protected
    procedure PaintRect(ClipRect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure RefreshTarget; override;
    procedure RefreshValues; override;
  public
    procedure SetHSB(const Hue, Sat, Bri: Integer); override;
    procedure SetRGB(const Red, Green, Blue: Integer); override;
  end;

  TPegtopColorBar = class(TPegtopColorsControl)
  private
    FTarget: Integer;
    procedure NotifyChange;
    procedure SetTarget(const Z: Integer; const Redraw: Boolean = False; const Refresh: Boolean = False);
  protected
    procedure PaintRect(ClipRect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure RefreshTarget; override;
    procedure RefreshValues; override;
  public
    procedure SetHSB(const Hue, Sat, Bri: Integer); override;
    procedure SetRGB(const Red, Green, Blue: Integer); override;
  end;

  TPegtopColorPreview = class(TPegtopCustomColorControl)
  private
    FColor: TColor;
    FRecentColor: TColor;
    FOpacity: Integer;
    FRecentOpacity: Integer;
    FHover: Boolean;
    FDown: Boolean;
    FOnRevertColor: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetRecentColor(Value: TColor);
    procedure SetOpacity(Value: Integer);
    procedure SetRecentOpacity(Value: Integer);
  protected
    procedure PaintRect(ClipRect: TRect); override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
    property RecentColor: TColor read FRecentColor write SetRecentColor;
    property Opacity: Integer read FOpacity write SetOpacity;
    property RecentOpacity: Integer read FRecentOpacity write SetRecentOpacity;
    property OnRevertColor: TNotifyEvent read FOnRevertColor write FOnRevertColor;
  end;

  TPegtopColorContainerPart = (pccpNull, pccpSet, pccpUse);
  TPegtopChangeColorEvent = procedure(Sender: TObject; var VarColor: TColor) of object;

  TPegtopColorContainer = class(TPegtopCustomColorControl)
  private
    FColor: TColor;
    FHover: TPegtopColorContainerPart;
    FDown: TPegtopColorContainerPart;
    FOnSetColor: TNotifyEvent;
    FOnUseColor: TNotifyEvent;
    FOnGetColor: TPegtopChangeColorEvent;
    procedure SetColor(Value: TColor);
  protected
    procedure PaintRect(ClipRect: TRect); override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
    property OnSetColor: TNotifyEvent read FOnSetColor write FOnSetColor;
    property OnUseColor: TNotifyEvent read FOnUseColor write FOnUseColor;
    property OnGetColor: TPegtopChangeColorEvent read FOnGetColor write FOnGetColor;
  end;

  TPegtopColorForm = class(TCustomForm)
  private
    FColor: TColor;
    FRecentColor: TColor;
    FOpacity: Integer;
    FRecentOpacity: Integer;
    FColorComponent: TPegtopColorComponent;
    FColorChart: TPegtopColorChart;
    FColorBar: TPegtopColorBar;
    FValueGroupBox: TGroupBox;
    FHexGroupBox: TGroupBox;
    FRadioButtons: array[TPegtopColorComponent] of TRadioButton;
    FEdits: array[TPegtopColorComponent] of TPegtopIntEdit;
    FWebSafeCheckBox: TPegtopCheckBox;
    FHexEdit: TPegtopIntEdit;
    FPreview: TPegtopColorPreview;
    FContainers: array[0..4] of TPegtopColorContainer;
    FOpacityTrackBar: TPegtopTRackBar;
    FHasOpacity: Boolean;
    FHasUserDefinedColors: Boolean;
    FWebSafe: Boolean;
    FFilter: TPegtopColorFilter;
    FLook: TPegtopColorControlLook;
    FOnPreview: TPegtopColorEvent;
    procedure SetValueGroupBoxCaption;
    procedure ColorChartHSBChange(Sender: TObject; Hue, Sat, Bri: Integer);
    procedure ColorChartRGBChange(Sender: TObject; Red, Green, Blue: Integer);
    procedure ColorBarHSBChange(Sender: TObject; Hue, Sat, Bri: Integer);
    procedure ColorBarRGBChange(Sender: TObject; Red, Green, Blue: Integer);
    procedure RadioButtonClick(Sender: TObject);
    procedure HSBEditChange(Sender: TObject);
    procedure RGBEditChange(Sender: TObject);
    procedure HexEditChange(Sender: TObject);
    procedure HSBEditExit(Sender: TObject);
    procedure RGBEditExit(Sender: TObject);
    procedure HexEditExit(Sender: TObject);
    procedure OpacityTrackBarChange(Sender: TObject);
    procedure OpacityTrackBarDrawTrack(Sender: TObject; Canvas: TCanvas; Orientation: TPegtopSlideBarOrientation; BoundsRect: TRect; Center: TPoint);
    procedure WebSafeCheckBoxClick(Sender: TObject);
    procedure PreviewRevertColor(Sender: TObject);
    procedure ColorServiceColorChange(Sender: TObject; Index: Integer; Color: TColor);
    procedure ContainerSetColor(Sender: TObject);
    procedure ContainerUseColor(Sender: TObject);
    procedure ContainerGetColor(Sender: TObject; var VarColor: TColor);
    function FilterColor(const C: TColor): TColor;
    procedure AdoptHSB(Hue, Sat, Bri: Integer);
    procedure AdoptRGB(Red, Green, Blue: Integer);
    procedure AdoptHex(Value: Integer);
    procedure SetHSB(Hue, Sat, Bri: Integer);
    procedure SetRGB(Red, Green, Blue: Integer);
    procedure SetColorComponent(Value: TPegtopColorComponent);
  protected
    procedure AfterConstruction; override;
  public
    procedure Init;
    procedure DeInit;
    property Color: TColor read FColor write FColor;
    property RecentColor: TColor read FRecentColor write FRecentColor;
    property Opacity: Integer read FOpacity write FOpacity;
    property RecentOpacity: Integer read FRecentOpacity write FRecentOpacity;
    property ColorComponent: TPegtopColorComponent read FColorComponent write SetColorComponent;
    property HasOpacity: Boolean read FHasOpacity write FHasOpacity;
    property HasUserDefinedColors: Boolean read FHasUserDefinedColors write FHasUserDefinedColors;
    property WebSafe: Boolean read FWebSafe write FWebSafe;
    property Look: TPegtopColorControlLook read FLook write FLook;
    property OnPreview: TPegtopColorEvent read FOnPreview write FOnPreview;
  end;

resourcestring
  PegtopColorDialogOkCaption = 'OK';
  PegtopColorDialogCancelCaption = 'Cancel';
  PegtopColorDialogValuesCaption = 'Values (%s)';
  PegtopColorDialogHueCaption = 'hue';
  PegtopColorDialogSaturationCaption = 'saturation';
  PegtopColorDialogBrightnessCaption = 'brightness';
  PegtopColorDialogRedCaption = 'red';
  PegtopColorDialogGreenCaption = 'green';
  PegtopColorDialogBlueCaption = 'blue';
  PegtopColorDialogHexCaption = 'Hex code';
  PegtopColorDialogDefaultCaption = 'Select color';
  PegtopColorDialogOpacityCaption = 'Opacity: <pos> %';
  PegtopColorDialogRevert = 'Revert';
  PegtopColorDialogWebSafe = 'web-safe';
  PegtopColorDialogSet = 'set';

var
  CircleBitmap: TBitmap;
  ContainerBitmap: array[0..1] of TBitmap;

////////////////////////////////////////////////////////////////////////////////
// color space functions
////////////////////////////////////////////////////////////////////////////////

function PegtopGetColorSBH(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertHSBToColor(Z, X, Y);
end;

function PegtopGetColorHBS(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertHSBToColor(X, Z, Y);
end;

function PegtopGetColorHSB(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertHSBToColor(X, Y, Z);
end;

function PegtopGetColorGBR(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertRGBToColor(Y shr 8, X shr 8, Z shr 8);
end;

function PegtopGetColorRBG(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertRGBToColor(Y shr 8, Z shr 8, X shr 8);
end;

function PegtopGetColorRGB(const X, Y, Z: Integer): TPegtopColor;
begin
  Result := ConvertRGBToColor(Z shr 8, Y shr 8, X shr 8);
end;

var
  ColorSpaceFunctions: array[TPegtopColorComponent] of TPegtopColorSpaceFunction;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorsControl
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorsControl.SetFixComponent(Value: TPegtopColorComponent);
begin
  if FFixComponent <> Value then begin
    FFixComponent := Value;
    RefreshTarget;
    Invalidate;
  end;
end;

procedure TPegtopColorsControl.SetWebSafe(Value: Boolean);
begin
  if FWebSafe <> Value then begin
    FWebSafe := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorChart
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorChart.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  X, Y, Z: Integer;
  Origin: Pointer;
  Pitch: Integer;
  P, Q: PPegtopColor;
  C: TPegtopColor;
  R: TRect;
  F: TPegtopColorSpaceFunction;
  Filter: TPegtopColorFilter;
begin
  if ClipRect.Left < 0 then ClipRect.Left := 0;
  if ClipRect.Top < 0 then ClipRect.Top := 0;
  if ClipRect.Right > ClientWidth then ClipRect.Right := ClientWidth;
  if ClipRect.Bottom > ClientHeight then ClipRect.Bottom := ClientHeight;
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    F := ColorSpaceFunctions[FFixComponent];
    if WebSafe then
      Filter := PegtopWebSafeColorFilter
    else
      Filter := GetProperColorFilter(Canvas.Handle);
    case FFixComponent of
      pccHue:        Z := FHue;
      pccSaturation: Z := FSat;
      pccBrightness: Z := FBri;
      pccRed:        Z := FRed shl 8;
      pccGreen:      Z := FGreen shl 8;
      pccBlue:       Z := FBlue shl 8;
      else           Z := 0;
    end;
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      R := ClipRect;
      if R.Left < 1 then R.Left := 1;
      if R.Top < 1 then R.Top := 1;
      if R.Right > 257 then R.Right := 257;
      if R.Bottom > 257 then R.Bottom := 257;
      if Assigned(Filter) then begin
        Q := Pointer(Integer(Origin) + (R.Top - ClipRect.Top) * Pitch + (R.Left - ClipRect.Left) * 4);
        for Y := R.Top to R.Bottom - 1 do begin
          P := Q;
          for X := R.Left to R.Right - 1 do begin
            C := F(X shl 8 - 128, (257 - Y) shl 8 - 128, Z);
            Filter.Apply(C, X - 1, Y - 1);
            P^:= C;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end
      else begin
        Q := Pointer(Integer(Origin) + (R.Top - ClipRect.Top) * Pitch + (R.Left - ClipRect.Left) * 4);
        for Y := R.Top to R.Bottom - 1 do begin
          P := Q;
          for X := R.Left to R.Right - 1 do begin
            P^ := F(X shl 8 - 128, (257 - Y) shl 8 - 128, Z);
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
      Bitmap.Canvas.Draw(1 + FTarget.X - 8 - ClipRect.Left, 256 - FTarget.Y - 8 - ClipRect.Top, CircleBitmap);
      DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, 258, 258), Rect(0, 0, Bitmap.Width, Bitmap.Height), $000000);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopColorChart.NotifyChange;
begin
  case FFixComponent of
    pccHue, pccSaturation, pccBrightness:
      if Assigned(FOnHSBChange) then FOnHSBChange(Self, FHue, FSat, FBri);
    pccRed, pccGreen, pccBlue:
      if Assigned(FOnRGBChange) then FOnRGBChange(Self, FRed, FGreen, FBlue);
  end;
end;

procedure TPegtopColorChart.SetTarget(const X, Y: Integer; const Redraw: Boolean = False; const Refresh: Boolean = False);
var
  ClipRect: TRect;
begin
  if (FTarget.X <> X) or (FTarget.Y <> Y) then begin
    ClipRect := Bounds(1 + FTarget.X - 8, 256 - FTarget.Y - 8, 16, 16);
    FTarget.X := X;
    if FTarget.X < 0 then FTarget.X := 0 else if FTarget.X > 255 then FTarget.X := 255;
    FTarget.Y := Y;
    if FTarget.Y < 0 then FTarget.Y := 0 else if FTarget.Y > 255 then FTarget.Y := 255;
    if Redraw then begin
      PaintRect(ClipRect);
      ClipRect := Bounds(1 + FTarget.X - 8, 256 - FTarget.Y - 8, 16, 16);
      PaintRect(ClipRect);
    end;
    if Refresh then begin
      RefreshValues;
      NotifyChange;
    end;
  end;
end;

procedure TPegtopColorChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < 257) and (Y < 257) then begin
    FTargeting := True;
    SetTarget(X - 1, 256 - Y, True, True);
  end;
  inherited;
end;

procedure TPegtopColorChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FTargeting := False;
  inherited;
end;

procedure TPegtopColorChart.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTargeting or ((X >= 1) and (Y >= 1) and (X < 257) and (Y < 257)) then
    ChangeCursor(crCircle)
  else
    ChangeCursor(crDefault);
  if FTargeting then begin
    SetTarget(X - 1, 256 - Y, True, True);
  end;
  inherited;
end;

procedure TPegtopColorChart.RefreshTarget;
begin
  case FFixComponent of
    pccHue:        SetTarget(FSat shr 8, FBri shr 8);
    pccSaturation: SetTarget(FHue shr 8, FBri shr 8);
    pccBrightness: SetTarget(FHue shr 8, FSat shr 8);
    pccRed:        SetTarget(FGreen, FBlue);
    pccGreen:      SetTarget(FRed, FBlue);
    pccBlue:       SetTarget(FRed, FGreen);
  end;
end;

procedure TPegtopColorChart.RefreshValues;
begin
  case FFixComponent of
    pccHue:        begin FSat := MulDiv(FTarget.X, Sat100, 255)            ; FBri := MulDiv(FTarget.Y, Bri100, 255); end;
    pccSaturation: begin FHue := MulDiv(FTarget.X, Hue360, 255) and HueMask; FBri := MulDiv(FTarget.Y, Bri100, 255); end;
    pccBrightness: begin FHue := MulDiv(FTarget.X, Hue360, 255) and HueMask; FSat := MulDiv(FTarget.Y, Sat100, 255); end;
    pccRed:        begin FGreen := FTarget.X; FBlue := FTarget.Y;  end;
    pccGreen:      begin FRed := FTarget.X;   FBlue := FTarget.Y;  end;
    pccBlue:       begin FRed := FTarget.X;   FGreen := FTarget.Y; end;
  end;
  case FFixComponent of
    pccHue, pccSaturation, pccBrightness:
      ConvertColorToRGB(ConvertHSBToColor(FHue, FSat, FBri), FRed, FGreen, FBlue);
    pccRed, pccGreen, pccBlue:
      ConvertColorToHSB(ConvertRGBToColor(FRed, FGreen, FBlue), FHue, FSat, FBri);
  end;
end;

procedure TPegtopColorChart.SetHSB(const Hue, Sat, Bri: Integer);
begin
  if (FHue <> Hue) or (FSat <> Sat) or (FBri <> Bri) then begin
    FHue := Hue;
    FSat := Sat;
    FBri := Bri;
    ConvertColorToRGB(ConvertHSBToColor(FHue, FSat, FBri), FRed, FGreen, FBlue);
    RefreshTarget;
    Invalidate;
  end;
end;

procedure TPegtopColorChart.SetRGB(const Red, Green, Blue: Integer);
begin
  if (FRed <> Red) or (FGreen <> Green) or (FBlue <> Blue) then begin
    FRed := Red;
    FGreen := Green;
    FBlue := Blue;
    ConvertColorToHSB(ConvertRGBToColor(FRed, FGreen, FBlue), FHue, FSat, FBri);
    RefreshTarget;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorBar
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorBar.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  X, Y, Z1, Z2: Integer;
  Origin: Pointer;
  Pitch: Integer;
  P, Q: PPegtopColor;
  C1, C2: TPegtopColor;
  R: TRect;
  F: TPegtopColorSpaceFunction;
  Filter: TPegtopColorFilter;
begin
  if ClipRect.Left < 0 then ClipRect.Left := 0;
  if ClipRect.Top < 0 then ClipRect.Top := 0;
  if ClipRect.Right > ClientWidth then ClipRect.Right := ClientWidth;
  if ClipRect.Bottom > ClientHeight then ClipRect.Bottom := ClientHeight;
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    F := ColorSpaceFunctions[FFixComponent];
    if WebSafe then
      Filter := PegtopWebSafeColorFilter
    else
      Filter := GetProperColorFilter(Canvas.Handle);
    case FFixComponent of
      pccHue:        begin Z1 := Sat100; Z2 := Bri100; end;
      pccSaturation: begin Z1 := FHue;   Z2 := FBri;   end;
      pccBrightness: begin Z1 := FHue;   Z2 := FSat;   end;
      pccRed:        begin Z1 := FGreen shl 8; Z2 := FBlue shl 8;  end;
      pccGreen:      begin Z1 := FRed shl 8;   Z2 := FBlue shl 8;  end;
      pccBlue:       begin Z1 := FRed shl 8;   Z2 := FGreen shl 8; end;
      else           begin Z1 := 0; Z2 := 0; end;
    end;
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      R := ClipRect;
      if R.Left < 1 then R.Left := 1;
      if R.Top < 1 then R.Top := 1;
      if R.Right > ClientWidth - 1 then R.Right := ClientWidth - 1;
      if R.Bottom > 257 then R.Bottom := 257;
      if Assigned(Filter) then begin
        Q := Pointer(Integer(Origin) + (R.Top - ClipRect.Top) * Pitch + (R.Left - ClipRect.Left) * 4);
        for Y := R.Top to R.Bottom - 1 do begin
          P := Q;
          C1 := F(Z1, Z2, (257 - Y) shl 8 - 128);
          for X := R.Left to R.Right - 1 do begin
            C2 := C1;
            Filter.Apply(C2, X - 1, Y - 1);
            P^ := C2;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end
      else begin
        Q := Pointer(Integer(Origin) + (R.Top - ClipRect.Top) * Pitch + (R.Left - ClipRect.Left) * 4);
        for Y := R.Top to R.Bottom - 1 do begin
          P := Q;
          C1 := F(Z1, Z2, (257 - Y) shl 8 - 128);
          for X := R.Left to R.Right - 1 do begin
            P^ := C1;
            Inc(P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
      Bitmap.Canvas.Draw(ClientWidth div 2 - 8 - ClipRect.Left, 256 - FTarget - 8 - ClipRect.Top, CircleBitmap);
      DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, ClientWidth, 258), Rect(0, 0, Bitmap.Width, Bitmap.Height), $000000);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopColorBar.NotifyChange;
begin
  RefreshValues;
  case FFixComponent of
    pccHue, pccSaturation, pccBrightness:
      if Assigned(FOnHSBChange) then FOnHSBChange(Self, FHue, FSat, FBri);
    pccRed, pccGreen, pccBlue:
      if Assigned(FOnRGBChange) then FOnRGBChange(Self, FRed, FGreen, FBlue);
  end;
end;

procedure TPegtopColorBar.SetTarget(const Z: Integer; const Redraw: Boolean = False; const Refresh: Boolean = False);
var
  ClipRect: TRect;
begin
  if FTarget <> Z then begin
    ClipRect := Bounds(1, 256 - FTarget - 8, ClientWidth - 1, 16);
    FTarget := Z;
    if FTarget < 0 then FTarget := 0 else if FTarget > 255 then FTarget := 255;
    if Redraw then begin
      PaintRect(ClipRect);
      ClipRect := Bounds(1, 256 - FTarget - 8, ClientWidth - 1, 16);
      PaintRect(ClipRect);
    end;
    if Refresh then begin
      RefreshValues;
      NotifyChange;
    end;
  end;
end;

procedure TPegtopColorBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < ClientWidth - 1) and (Y < 257) then begin
    FTargeting := True;
    SetTarget(256 - Y, True, True);
  end;
  inherited;
end;

procedure TPegtopColorBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FTargeting := False;
  inherited;
end;

procedure TPegtopColorBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTargeting or ((X >= 1) and (Y >= 1) and (X < ClientWidth - 1) and (Y < 257)) then
    ChangeCursor(crCircle)
  else
    ChangeCursor(crDefault);
  if FTargeting then begin
    SetTarget(256 - Y, True, True);
  end;
  inherited;
end;

procedure TPegtopColorBar.RefreshTarget;
begin
  case FFixComponent of
    pccHue:        SetTarget(FHue shr 8);
    pccSaturation: SetTarget(FSat shr 8);
    pccBrightness: SetTarget(FBri shr 8);
    pccRed:        SetTarget(FRed);
    pccGreen:      SetTarget(FGreen);
    pccBlue:       SetTarget(FBlue);
  end;
end;

procedure TPegtopColorBar.RefreshValues;
begin
  case FFixComponent of
    pccHue:        FHue := MulDiv(FTarget, Hue360, 255) and HueMask;
    pccSaturation: FSat := MulDiv(FTarget, Sat100, 255);
    pccBrightness: FBri := MulDiv(FTarget, Bri100, 255);
    pccRed:        FRed := FTarget;
    pccGreen:      FGreen := FTarget;
    pccBlue:       FBlue := FTarget;
  end;
  case FFixComponent of
    pccHue, pccSaturation, pccBrightness:
      ConvertColorToRGB(ConvertHSBToColor(FHue, FSat, FBri), FRed, FGreen, FBlue);
    pccRed, pccGreen, pccBlue:
      ConvertColorToHSB(ConvertRGBToColor(FRed, FGreen, FBlue), FHue, FSat, FBri);
  end;
end;

procedure TPegtopColorBar.SetHSB(const Hue, Sat, Bri: Integer);
begin
  if (FHue <> Hue) or (FSat <> Sat) or (FBri <> Bri) then begin
    FHue := Hue;
    FSat := Sat;
    FBri := Bri;
    ConvertColorToRGB(ConvertHSBToColor(FHue, FSat, FBri), FRed, FGreen, FBlue);
    RefreshTarget;
    Invalidate;
  end;
end;

procedure TPegtopColorBar.SetRGB(const Red, Green, Blue: Integer);
begin
  if (FRed <> Red) or (FGreen <> Green) or (FBlue <> Blue) then begin
    FRed := Red;
    FGreen := Green;
    FBlue := Blue;
    ConvertColorToHSB(ConvertRGBToColor(FRed, FGreen, FBlue), FHue, FSat, FBri);
    RefreshTarget;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorPreview
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorPreview.Create(AOwner: TComponent);
begin
  inherited;
  FOpacity := 256;
  FRecentOpacity := 256;
end;

procedure TPegtopColorPreview.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  C: TPegtopColor;
  Filter: TPegtopColorFilter;
  Extent: TSize;
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
      C := SwapColorBytes(TPegtopColor(ColorToRGB(FRecentColor)));
      PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth - 1, ClientHeight div 4),
        C, FRecentOpacity, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      C := SwapColorBytes(TPegtopColor(ColorToRGB(FColor)));
      PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, ClientHeight div 4 - ClipRect.Top, ClientWidth - 1, ClientHeight - 1),
        C, FOpacity, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      if FHover then begin
        Bitmap.Canvas.Brush.Style := bsClear;
        Bitmap.Canvas.Font.Color := GetContrastColor(MixColors(PegtopColor($E0E0E0), PegtopColor(FRecentColor), FRecentOpacity)).Def;
        Extent := Bitmap.Canvas.TextExtent(PegtopColorDialogRevert);
        Bitmap.Canvas.TextOut((ClientWidth - Extent.CX) div 2 - ClipRect.Left,
          (ClientHeight div 4 - Extent.CY) div 2 - ClipRect.Top, PegtopColorDialogRevert);
      end;
      DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, ClientWidth, ClientHeight), Rect(0, 0, Bitmap.Width, Bitmap.Height), $000000);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopColorPreview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < ClientWidth - 1) and (Y < ClientHeight div 4) then begin
    FDown := True;
  end
  else begin
    FDown := False;
  end;
  inherited;
end;

procedure TPegtopColorPreview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDown and FHover and (Button = mbLeft) then begin
    if Assigned(FOnRevertColor) then FOnRevertColor(Self);
  end;
  FDown := False;
  inherited;
end;

procedure TPegtopColorPreview.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 1) and (Y >= 1) and (X < ClientWidth - 1) and (Y < ClientHeight div 4) then begin
    if not FHover then begin
      FHover := True;
      ChangeCursor(crCircle);
      Invalidate;
    end;
  end
  else begin
    if FHover then begin
      FHover := False;
      ChangeCursor(crDefault);
      Invalidate;
    end;
  end;
  inherited;
end;

procedure TPegtopColorPreview.DoMouseLeave;
begin
  if FHover then begin
    FHover := False;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopColorPreview.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TPegtopColorPreview.SetRecentColor(Value: TColor);
begin
  if FRecentColor <> Value then begin
    FRecentColor := Value;
    Invalidate;
  end;
end;

procedure TPegtopColorPreview.SetOpacity(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FOpacity <> Value then begin
    FOpacity := Value;
    Invalidate;
  end;
end;

procedure TPegtopColorPreview.SetRecentOpacity(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FRecentOpacity <> Value then begin
    FRecentOpacity := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorContainer
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorContainer.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TPegtopColorContainer.PaintRect(ClipRect: TRect);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
  C: TPegtopColor;
  VarColor: TColor;
  Filter: TPegtopColorFilter;
  Extent: TSize;
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
      VarColor := FColor;
      if (FHover = pccpSet) and Assigned(FOnGetColor) then FOnGetColor(Self, VarColor);
      C := SwapColorBytes(TPegtopColor(ColorToRGB(VarColor)));
      PaintSolidRect(Origin, Pitch, Bounds(1 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth div 2 - 1, ClientHeight - 1),
        C, 256, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      if (FHover = pccpSet) and (GetGrayLevel(TPegtopColor(VarColor)) < 128) then
        Bitmap.Canvas.CopyMode := cmMergePaint
      else
        Bitmap.Canvas.CopyMode := cmSrcAnd;
      Bitmap.Canvas.Draw((ClientWidth div 2 - ContainerBitmap[0].Width) div 2 - ClipRect.Left + 1,
        (ClientHeight - ContainerBitmap[0].Height) div 2 - ClipRect.Top + 1, ContainerBitmap[0]);
      C := SwapColorBytes(TPegtopColor(ColorToRGB(FColor)));
      DrawVLine(Origin, Pitch, ClientWidth div 2 - 1 - ClipRect.Left, 1 - ClipRect.Top, ClientHeight - 2 - ClipRect.Top,
        PegtopColor($000000), Rect(0, 0, Bitmap.Width, Bitmap.Height));
      PaintSolidRect(Origin, Pitch, Bounds(ClientWidth div 2 - ClipRect.Left, 1 - ClipRect.Top, ClientWidth - 1, ClientHeight - 1),
        C, 256, Filter, ClipRect.Left, ClipRect.Top, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      if FHover = pccpSet then begin
        Bitmap.Canvas.Brush.Style := bsClear;
        Bitmap.Canvas.Font.Color := GetContrastColor(PegtopColor(FColor)).Def;
        Extent := Bitmap.Canvas.TextExtent(PegtopColorDialogSet);
        Bitmap.Canvas.TextOut((ClientWidth div 2 - Extent.CX) div 2 + ClientWidth div 2 - ClipRect.Left,
          (ClientHeight - Extent.CY) div 2 - ClipRect.Top, PegtopColorDialogSet);
      end;
      DrawBounds(Origin, Pitch, Bounds(-ClipRect.Left, -ClipRect.Top, ClientWidth, ClientHeight), Rect(0, 0, Bitmap.Width, Bitmap.Height), $000000);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopColorContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (X >= 1) and (Y >= 1) and (X < ClientWidth div 2) and (Y < ClientHeight - 1) then begin
    FDown := pccpSet;
  end
  else if (Button = mbLeft) and (X >= ClientWidth div 2) and (Y >= 1) and (X < ClientWidth - 1) and (Y < ClientHeight - 1) then begin
    FDown := pccpUse;
  end
  else begin
    FDown := pccpNull;
  end;
  inherited;
end;

procedure TPegtopColorContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FDown = pccpSet) and (FHover = pccpSet) and (Button = mbLeft) then begin
    if Assigned(FOnSetColor) then FOnSetColor(Self);
  end
  else if (FDown = pccpUse) and (FHover = pccpUse) and (Button = mbLeft) then begin
    if Assigned(FOnUseColor) then FOnUseColor(Self);
  end;
  FDown := pccpNull;
  inherited;
end;

procedure TPegtopColorContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 1) and (Y >= 1) and (X < ClientWidth div 2) and (Y < ClientHeight - 1) then begin
    if FHover <> pccpSet then begin
      FHover := pccpSet;
      ChangeCursor(crDefault);
      Invalidate;
    end;
  end
  else if (X >= ClientWidth div 2) and (Y >= 1) and (X < ClientWidth - 1) and (Y < ClientHeight - 1) then begin
    if FHover <> pccpUse then begin
      FHover := pccpUse;
      ChangeCursor(crCircle);
      Invalidate;
    end;
  end
  else begin
    if FHover <> pccpNull then begin
      FHover := pccpNull;
      ChangeCursor(crDefault);
      Invalidate;
    end;
  end;
  inherited;
end;

procedure TPegtopColorContainer.DoMouseLeave;
begin
  if FHover <> pccpNull then begin
    FHover := pccpNull;
    Invalidate;
  end;
  inherited;
end;

procedure TPegtopColorContainer.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorForm
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorForm.AfterConstruction;
const
  ComponentCaptions: array[TPegtopColorComponent] of String = ('H', 'S', 'B', 'R', 'G', 'B');
  ComponentMax: array[TPegtopColorComponent] of Integer = (359, 100, 100, 255, 255, 255);
var
  C: TPegtopColorComponent;
begin
  inherited;
  ClientWidth := 463;
  ClientHeight := 303;
  Position := poScreenCenter;
  BorderStyle := bsSingle;
  BorderIcons := [biSystemMenu];
  AutoScroll := False;
  FColorChart := TPegtopColorChart.Create(Self);
  FColorChart.SetBounds(8, 8, 258, 258);
  FColorChart.Parent := Self;
  FColorChart.OnHSBChange := ColorChartHSBChange;
  FColorChart.OnRGBChange := ColorChartRGBChange;
  FColorBar := TPegtopColorBar.Create(Self);
  FColorBar.SetBounds(272, 8, 26, 258);
  FColorBar.Parent := Self;
  FColorBar.OnHSBChange := ColorBarHSBChange;
  FColorBar.OnRGBChange := ColorBarRGBChange;
  FPreview := TPegtopColorPreview.Create(Self);
  FPreview.SetBounds(304, 8, 152, 96);
  FPreview.Parent := Self;
  FPreview.OnRevertColor := PreviewRevertColor;
  FOpacityTrackBar := TPegtopTRackBar.Create(Self);
  FOpacityTrackBar.SetBounds(304, 72, 152, 32);
  FOpacityTrackBar.LabelCaption := PegtopColorDialogOpacityCaption;
  FOpacityTrackBar.Visible := False;
  FOpacityTrackBar.Parent := Self;
  FOpacityTrackBar.OnChange := OpacityTrackBarChange;
  FOpacityTrackBar.OnDrawTrack := OpacityTrackBarDrawTrack;
  FValueGroupBox := TGroupBox.Create(Self);
  FValueGroupBox.SetBounds(304, 112, 152, 96);
  FValueGroupBox.Parent := Self;
  SetValueGroupBoxCaption;
  FHexGroupBox := TGroupBox.Create(Self);
  FHexGroupBox.SetBounds(304, 216, 152, 48);
  FHexGroupBox.Parent := Self;
  FHexGRoupBox.Caption := PegtopColorDialogHexCaption;
  for C := Low(TPegtopColorComponent) to High(TPegtopColorComponent) do begin
    FRadioButtons[C] := TRadioButton.Create(Self);
    FRadioButtons[C].Tag := Integer(C);
    FRadioButtons[C].Caption := ComponentCaptions[C] + ':';
    FRadioButtons[C].SetBounds(8 + 72 * (Integer(C) div 3), 20 + 24 * (Integer(C) mod 3), 32, 17);
    FRadioButtons[C].Parent := FValueGroupBox;
    FRadioButtons[C].OnClick := RadioButtonClick;
  end;
  for C := Low(TPegtopColorComponent) to High(TPegtopColorComponent) do begin
    FEdits[C] := TPegtopIntEdit.Create(Self);
    FEdits[C].Tag := Integer(C);
    FEdits[C].SetBounds(40 + 72 * (Integer(C) div 3), 18 + 24 * (Integer(C) mod 3), 32, 17);
    FEdits[C].Parent := FValueGroupBox;
    FEdits[C].MinValue := 0;
    FEdits[C].MaxValue := ComponentMax[C];
    FEdits[C].CaptionAlignment := pcaRight;
    if C in [pccHue, pccSaturation, pccBrightness] then begin
      FEdits[C].OnChange := HSBEditChange;
      FEdits[C].OnExit := HSBEditExit;
    end
    else begin
      FEdits[C].OnChange := RGBEditChange;
      FEdits[C].OnExit := RGBEditExit;
    end;
  end;
  {FEdits[pccHue].Caption := '°';
  FEdits[pccSaturation].Caption := '%';
  FEdits[pccBrightness].Caption := '%';}
  FWebSafeCheckBox := TPegtopCheckBox.Create(Self);
  FWebSafeCheckBox.SetBounds(8, 22, 64, 13);
  FWebSafeCheckBox.Caption := PegtopColorDialogWebSafe;
  FWebSafeCheckBox.Parent := FHexGRoupBox;
  FWebSafeCheckBox.OnClick := WebSafeCheckBoxClick;
  FHexEdit := TPegtopIntEdit.Create(Self);
  FHexEdit.SetBounds(80, 18, 64, 17);
  FHexEdit.Parent := FHexGroupBox;
  FHexEdit.MinValue := 0;
  FHexEdit.MaxValue := $FFFFFF;
  FHexEdit.Base := 16;
  FHexEdit.Options := [pneFixLength];
  FHexEdit.OnChange := HexEditChange;
  FHexEdit.OnExit := HexEditExit;
  with TButton.Create(Self) do begin
    SetBounds(304, 272, 72, 25);
    Caption := PegtopColorDialogOkCaption;
    ModalResult := mrOk;
    Default := True;
    Parent := Self;
  end;
  with TButton.Create(Self) do begin
    SetBounds(384, 272, 72, 25);
    Caption := PegtopColorDialogCancelCaption;
    ModalResult := mrCancel;
    Cancel := True;
    Parent := Self;
  end;
end;

procedure TPegtopColorForm.Init;
var
  Red, Green, Blue: Integer;
  I: Integer;
begin
  if FHasUserDefinedColors then begin
    for I := Low(FContainers) to High(FContainers) do begin
      FContainers[I] := TPegtopColorContainer.Create(Self);
      FContainers[I].Tag := I;
      FContainers[I].SetBounds(I * 53 + 8, 272, 46, 23);
      FContainers[I].Parent := Self;
      FContainers[I].OnSetColor := ContainerSetColor;
      FContainers[I].OnUseColor := ContainerUseColor;
      FContainers[I].OnGetColor := ContainerGetColor;
      FContainers[I].Look := FLook;
      FContainers[I].Color := ColorDefinitionService.Colors[I];
    end;
    ColorDefinitionService.AddColorChangeEvent(ColorServiceColorChange);
  end;

  if FWebSafe then
    FFilter := PegtopWebSafeColorFilter
  else
    FFilter := NIL;
  FWebSafeCheckBox.Change(FWebSafe);
  FColor := FilterColor(FColor);

  if FHasOpacity then begin
    FPreview.SetBounds(304, 8, 152, 60);
    FOpacityTrackBar.Visible := True;
  end;

  Red := FColor and $FF;
  Green := (FColor shr 8) and $FF;
  Blue := (FColor shr 16) and $FF;
  FColorChart.FixComponent := FColorComponent;
  FColorChart.WebSafe := FWebSafe;
  FColorChart.Look := FLook;
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.FixComponent := FColorComponent;
  FColorBar.WebSafe := FWebSafe;
  FColorBar.Look := FLook;
  FColorBar.SetRGB(Red, Green, Blue);
  FOpacityTrackBar.Position := FOpacity;
  FOpacityTrackBar.DefaultPosition := FOpacity;
  FPreview.RecentColor := FRecentColor;
  FPreview.Color := FColor;
  FPreview.Opacity := FOpacity * 256 div 100;
  FPreview.RecentOpacity := FRecentOpacity * 256 div 100;
  FPreview.Look := FLook;
  FRadioButtons[FColorComponent].Checked := True;
  SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.DeInit;
begin
  if FHasUserDefinedColors then begin
    ColorDefinitionService.RemoveColorChangeEvent(ColorServiceColorChange);
  end;
end;

procedure TPegtopColorForm.SetValueGroupBoxCaption;
var
  S: String;
begin
  case FColorComponent of
    pccHue:        S := PegtopColorDialogHueCaption;
    pccSaturation: S := PegtopColorDialogSaturationCaption;
    pccBrightness: S := PegtopColorDialogBrightnessCaption;
    pccRed:        S := PegtopColorDialogRedCaption;
    pccGreen:      S := PegtopColorDialogGreenCaption;
    pccBlue:       S := PegtopColorDialogBlueCaption;
  end;
  FValueGroupBox.Caption := Format(PegtopColorDialogValuesCaption, [S]);
end;

procedure TPegtopColorForm.ColorChartHSBChange(Sender: TObject; Hue, Sat, Bri: Integer);
begin
  FColorBar.SetHSB(Hue, Sat, Bri);
  SetHSB(Hue, Sat, Bri);
end;

procedure TPegtopColorForm.ColorChartRGBChange(Sender: TObject; Red, Green, Blue: Integer);
begin
  FColorBar.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.ColorBarHSBChange(Sender: TObject; Hue, Sat, Bri: Integer);
begin
  FColorChart.SetHSB(Hue, Sat, Bri);
  SetHSB(Hue, Sat, Bri);
end;

procedure TPegtopColorForm.ColorBarRGBChange(Sender: TObject; Red, Green, Blue: Integer);
begin
  FColorChart.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.RadioButtonClick(Sender: TObject);
begin
  ColorComponent := TPegtopColorComponent(TComponent(Sender).Tag);
end;

procedure TPegtopColorForm.HSBEditChange(Sender: TObject);
var
  Hue, Sat, Bri: Integer;
begin
  Hue := MulDiv(FEdits[pccHue].Value, Hue360, 360);
  Sat := MulDiv(FEdits[pccSaturation].Value, Sat100, 100);
  Bri := MulDiv(FEdits[pccBrightness].Value, Sat100, 100);
  FColorChart.SetHSB(Hue, Sat, Bri);
  FColorBar.SetHSB(Hue, Sat, Bri);
  AdoptHSB(Hue, Sat, Bri);
end;

procedure TPegtopColorForm.RGBEditChange(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  Red := FEdits[pccRed].Value;
  Green := FEdits[pccGreen].Value;
  Blue := FEdits[pccBlue].Value;
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
  AdoptRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.HexEditChange(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  Red := (FHexEdit.Value shr 16) and $FF;
  Green := (FHexEdit.Value shr 8) and $FF;
  Blue := FHexEdit.Value and $FF;
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
  AdoptHex(FHexEdit.Value);
end;

procedure TPegtopColorForm.HSBEditExit(Sender: TObject);
var
  Hue, Sat, Bri: Integer;
begin
  Hue := MulDiv(FEdits[pccHue].Value, Hue360, 360);
  Sat := MulDiv(FEdits[pccSaturation].Value, Sat100, 100);
  Bri := MulDiv(FEdits[pccBrightness].Value, Sat100, 100);
  SetHSB(Hue, Sat, Bri);
  FColorChart.SetHSB(Hue, Sat, Bri);
  FColorBar.SetHSB(Hue, Sat, Bri);
end;

procedure TPegtopColorForm.RGBEditExit(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  Red := FEdits[pccRed].Value;
  Green := FEdits[pccGreen].Value;
  Blue := FEdits[pccBlue].Value;
  SetRGB(Red, Green, Blue);
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.HexEditExit(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  Red := (FHexEdit.Value shr 16) and $FF;
  Green := (FHexEdit.Value shr 8) and $FF;
  Blue := FHexEdit.Value and $FF;
  SetRGB(Red, Green, Blue);
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.OpacityTrackBarChange(Sender: TObject);
begin
  FOpacity := FOpacityTrackBar.Position;
  FPreview.Opacity := FOpacity * 256 div 100;
end;

procedure TPegtopColorForm.OpacityTrackBarDrawTrack(Sender: TObject; Canvas: TCanvas; Orientation: TPegtopSlideBarOrientation; BoundsRect: TRect; Center: TPoint);
const
  Shades = 32;
var
  I: Integer;
begin
  Canvas.Brush.Style := bsSolid;
  for I := 0 to Shades - 1 do begin
    Canvas.Brush.Color := TColor(MixColors(PegtopColor(ColorToRGB(clBtnFace)), PegtopColor($000000), I * 256 div (Shades - 1)));
    Canvas.FillRect(Rect(
      BoundsRect.Left + 1 + I * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y - 2,
      BoundsRect.Left + 1 + (I + 1) * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y - 1
    ));
    Canvas.FillRect(Rect(
      BoundsRect.Left + 1 + I * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y + 2,
      BoundsRect.Left + 1 + (I + 1) * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y + 3
    ));
    Canvas.Brush.Color := TColor(MixColors(PegtopColor(ColorToRGB(clBtnFace)), PegtopColor(FColor), I * 256 div (Shades - 1)));
    Canvas.FillRect(Rect(
      BoundsRect.Left + 1 + I * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y - 1,
      BoundsRect.Left + 1 + (I + 1) * (BoundsRect.Right - BoundsRect.Left - 2) div Shades,
      Center.Y + 2
    ));
  end;
  Canvas.Brush.Color := $000000;
  Canvas.FillRect(Rect(
    BoundsRect.Right - 2,
    Center.Y - 1,
    BoundsRect.Right - 1,
    Center.Y + 2
  ));
{  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(BoundsRect.Left + 1, Center.Y - 2, BoundsRect.Right - 1, Center.Y + 3);}
end;

procedure TPegtopColorForm.WebSafeCheckBoxClick(Sender: TObject);
begin
  FWebSafe := FWebSafeCheckBox.Checked;
  if FWebSafe then
    FFilter := PegtopWebSafeColorFilter
  else
    FFilter := NIL;
  FColorChart.WebSafe := FWebSafe;
  FColorBar.WebSafe := FWebSafe;
  SetRGB(FColor and $FF, (FColor shr 8) and $FF, (FColor shr 16) and $FF);
end;

procedure TPegtopColorForm.PreviewRevertColor(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  Red := FRecentColor and $FF;
  Green := (FRecentColor shr 8) and $FF;
  Blue := (FRecentColor shr 16) and $FF;
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
  FOpacity := FRecentOpacity;
  FOpacityTrackBar.Position := FOpacity;
  FPreview.Opacity := FOpacity * 256 div 100;
end;

procedure TPegtopColorForm.ColorServiceColorChange(Sender: TObject; Index: Integer; Color: TColor);
begin
  if FHasUserDefinedColors then begin
    FContainers[Index].Color := Color;
  end;
end;

procedure TPegtopColorForm.ContainerSetColor(Sender: TObject);
var
  I: Integer;
begin
  TPegtopColorContainer(Sender).Color := FColor;
  I := TComponent(Sender).Tag;
  ColorDefinitionService.Colors[I] := FColor;
end;

procedure TPegtopColorForm.ContainerUseColor(Sender: TObject);
var
  Red, Green, Blue: Integer;
begin
  FColor := TPegtopColorContainer(Sender).Color;
  Red := FColor and $FF;
  Green := (FColor shr 8) and $FF;
  Blue := (FColor shr 16) and $FF;
  FColorChart.SetRGB(Red, Green, Blue);
  FColorBar.SetRGB(Red, Green, Blue);
  SetRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.ContainerGetColor(Sender: TObject; var VarColor: TColor);
begin
  VarColor := FColor;
end;

function TPegtopColorForm.FilterColor(const C: TColor): TColor;
var
  D: TPegtopColor;
begin
  if Assigned(FFilter) then begin
    D := PegtopColor(FColor);
    FFilter.Apply(D, 0, 0);
    Result := D.Def;
  end
  else begin
    Result := C;
  end;
end;

procedure TPegtopColorForm.AdoptHSB(Hue, Sat, Bri: Integer);
var
  Red, Green, Blue: Integer;
begin
  ConvertColorToRGB(ConvertHSBToColor(Hue, Sat, Bri), Red, Green, Blue);
  FColor := (Blue shl 16) or (Green shl 8) or Red;
  if Assigned(FFilter) then begin
    FColor := FilterColor(FColor);
    Blue := (FColor shr 16) and $FF;
    Green := (FColor shr 8) and $FF;
    Red := FColor and $FF;
  end;
  FPreview.Color := FColor;
  FEdits[pccRed].Value   := Red;
  FEdits[pccGreen].Value := Green;
  FEdits[pccBlue].Value  := Blue;
  FHexEdit.Value := (Red shl 16) or (Green shl 8) or Blue;
  FOpacityTrackBar.Invalidate;
  if Assigned(FOnPreview) then FOnPreview(Self, FColor);
end;

procedure TPegtopColorForm.AdoptRGB(Red, Green, Blue: Integer);
var
  Hue, Sat, Bri: Integer;
begin
  FColor := (Blue shl 16) or (Green shl 8) or Red;
  if Assigned(FFilter) then begin
    FColor := FilterColor(FColor);
    Blue := (FColor shr 16) and $FF;
    Green := (FColor shr 8) and $FF;
    Red := FColor and $FF;
  end;
  FPreview.Color := FColor;
  ConvertColorToHSB(ConvertRGBToColor(Red, Green, Blue), Hue, Sat, Bri);
  FEdits[pccHue].Value        := MulDiv(Hue, 360, Hue360);
  FEdits[pccSaturation].Value := MulDiv(Sat, 100, Sat100);
  FEdits[pccBrightness].Value := MulDiv(Bri, 100, Sat100);
  FHexEdit.Value := (Red shl 16) or (Green shl 8) or Blue;
  FOpacityTrackBar.Invalidate;
  if Assigned(FOnPreview) then FOnPreview(Self, FColor);
end;

procedure TPegtopColorForm.AdoptHex(Value: Integer);
var
  Red, Green, Blue: Integer;
  Hue, Sat, Bri: Integer;
begin
  Red := (Value shr 16) and $FF;
  Green := (Value shr 8) and $FF;
  Blue := Value and $FF;
  FColor := (Blue shl 16) or (Green shl 8) or Red;
  if Assigned(FFilter) then begin
    FColor := FilterColor(FColor);
    Blue := (FColor shr 16) and $FF;
    Green := (FColor shr 8) and $FF;
    Red := FColor and $FF;
  end;
  FPreview.Color := FColor;
  ConvertColorToHSB(ConvertRGBToColor(Red, Green, Blue), Hue, Sat, Bri);
  FEdits[pccRed].Value   := Red;
  FEdits[pccGreen].Value := Green;
  FEdits[pccBlue].Value  := Blue;
  FEdits[pccHue].Value        := MulDiv(Hue, 360, Hue360);
  FEdits[pccSaturation].Value := MulDiv(Sat, 100, Sat100);
  FEdits[pccBrightness].Value := MulDiv(Bri, 100, Sat100);
  FOpacityTrackBar.Invalidate;
  if Assigned(FOnPreview) then FOnPreview(Self, FColor);
end;

procedure TPegtopColorForm.SetHSB(Hue, Sat, Bri: Integer);
var
  C: TPegtopColor;
begin
  if Assigned(FFilter) then begin
    C := ConvertHSBToColor(Hue, Sat, Bri);
    FFilter.Apply(C, 0, 0);
    ConvertColorToHSB(C, Hue, Sat, Bri);
  end;
  FEdits[pccHue].Value        := MulDiv(Hue, 360, Hue360);
  FEdits[pccSaturation].Value := MulDiv(Sat, 100, Sat100);
  FEdits[pccBrightness].Value := MulDiv(Bri, 100, Sat100);
  AdoptHSB(Hue, Sat, Bri);
end;

procedure TPegtopColorForm.SetRGB(Red, Green, Blue: Integer);
var
  C: TPegtopColor;
begin
  if Assigned(FFilter) then begin
    C := ConvertRGBToColor(Red, Green, Blue);
    FFilter.Apply(C, 0, 0);
    ConvertColorToRGB(C, Red, Green, Blue);
  end;
  FEdits[pccRed].Value   := Red;
  FEdits[pccGreen].Value := Green;
  FEdits[pccBlue].Value  := Blue;
  AdoptRGB(Red, Green, Blue);
end;

procedure TPegtopColorForm.SetColorComponent(Value: TPegtopColorComponent);
begin
  FColorComponent := Value;
  SetValueGroupBoxCaption;
  FColorChart.FixComponent := FColorComponent;
  FColorBar.FixComponent := FColorComponent;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorDialog.Create(AOwner: TComponent);
begin
  inherited;
  FColorComponent := pccHue;
  FOpacity := 100;
  FLook := pclRoundedRect;
  FOptions := [pcoUserDefinedColors];
end;

function TPegtopColorDialog.Execute: Boolean;
var
  Form: TPegtopColorForm;
  C: TColor;
begin
  Form := TPegtopColorForm.CreateNew(Application);
  try
    C := ColorToRGB(FColor);
    Form.Color := C;
    Form.RecentColor := C;
    Form.ColorComponent := FColorComponent;
    Form.Opacity := FOpacity;
    Form.RecentOpacity := FOpacity;
    Form.HasOpacity := pcoOpacity in FOptions;
    Form.WebSafe := pcoWebSafe in FOptions;
    Form.HasUserDefinedColors := pcoUserDefinedColors in FOptions;
    Form.Look := FLook;
    Form.OnPreview := FormPreview;
    if FCaption <> '' then
      Form.Caption := FCaption
    else
      Form.Caption := PegtopColorDialogDefaultCaption;
    Form.Init;
    try
      Result := Form.ShowModal = mrOk;
    finally
      Form.DeInit;
    end;
    if Result then begin
      FColor := Form.Color;
      FColorComponent := Form.ColorComponent;
      FOpacity := Form.Opacity;
    end
    else begin
      // send final event (restore old color)
      if Assigned(FOnPreview) then FOnPreview(Self, FColor);
    end;
    if Form.WebSafe then
      Include(FOptions, pcoWebSafe)
    else
      Exclude(FOptions, pcoWebSafe);
  finally
    Form.Free;
  end;
end;

procedure TPegtopColorDialog.FormPreview(Sender: TObject; Color: TColor);
begin
  if Assigned(FOnPreview) then FOnPreview(Self, Color);
end;

procedure TPegtopColorDialog.SetOpacity(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 100 then Value := 100;
  FOpacity := Value;
end;

initialization
  // init function pointers:
  ColorSpaceFunctions[pccHue]        := PegtopGetColorSBH;
  ColorSpaceFunctions[pccSaturation] := PegtopGetColorHBS;
  ColorSpaceFunctions[pccBrightness] := PegtopGetColorHSB;
  ColorSpaceFunctions[pccRed]        := PegtopGetColorGBR;
  ColorSpaceFunctions[pccGreen]      := PegtopGetColorRBG;
  ColorSpaceFunctions[pccBlue]       := PegtopGetColorRGB;
  // init bitmaps:
  CircleBitmap := TBitmap.Create;
  CircleBitmap.LoadFromResourceName(HInstance, 'PEGTOPCOLORDIALOGCIRCLE');
  CircleBitmap.Transparent := True;
  CircleBitmap.TransParentColor := CircleBitmap.Canvas.Pixels[0,0];
  ContainerBitmap[0] := TBitmap.Create;
  ContainerBitmap[0].LoadFromResourceName(HInstance, 'PEGTOPCOLORDIALOGSET');
  ContainerBitmap[1] := TBitmap.Create;
  ContainerBitmap[1].LoadFromResourceName(HInstance, 'PEGTOPCOLORDIALOGLOCK');
finalization
  ContainerBitmap[1].Free;
  ContainerBitmap[0].Free;
  CircleBitmap.Free;
end.
