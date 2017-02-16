{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang, Ma Xiaoming and GraphicsMagic Team.
  All rights reserved. }

unit ColorForm;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GR32_RangeBars, StdCtrls,
  gmTypes;

type
  TfrmColor = class(TForm)
    pnlSpectra: TPanel;
    imgSpectra: TImage;
    pnlColor: TPanel;
    lblRValue: TLabel;
    lblGValue: TLabel;
    lblBValue: TLabel;
    shpBackgroundColor: TShape;
    shpForegroundColor: TShape;
    lblGrayPercent: TLabel;
    edtRValue: TEdit;
    edtGValue: TEdit;
    edtBValue: TEdit;
    ggbrRValue: TGaugeBar;
    ggbrGValue: TGaugeBar;
    ggbrBValue: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure shpForegroundColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shpBackgroundColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ColorValueChange(Sender: TObject);
    procedure edtRValueChange(Sender: TObject);
    procedure edtGValueChange(Sender: TObject);
    procedure edtBValueChange(Sender: TObject);
    procedure imgSpectraMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgSpectraMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ColorValueKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
  private
    FColorMode               : TgmColorMode;      // current color mode
    FCurrColorSelector       : TgmColorSelector;  // color selector (foreground/background)
    FLastColorSelector       : TgmColorSelector;  // last color selector
    FRValue, FGValue, FBValue: Integer;
    FKValue                  : Integer;

    procedure UpdateColorPageAppearance;
    procedure SetColorMode(const AValue: TgmColorMode);
  public
    procedure ChangeColorViaTrackBar(const R, G, B: Byte);

    property ColorMode        : TgmColorMode     read FColorMode         write SetColorMode;
    property CurrColorSelector: TgmColorSelector read FCurrColorSelector write FCurrColorSelector;
    property LastColorSelector: TgmColorSelector read FLastColorSelector write FLastColorSelector;
  end;

var
  frmColor: TfrmColor;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmPaintFuncs,          // DrawHueGradientSpectra()...
{ GraphicsMagic Forms/Dialogs }
  MainForm,              // frmMain
  SwatchForm,
  GradientPickerPopFrm;  // frmGradientPicker

{$R *.dfm}

const
  MIN_FORM_WIDTH  = 236;
  MIN_FORM_HEIGHT = 160;

//-- Custom Methods ------------------------------------------------------------

procedure TfrmColor.ChangeColorViaTrackBar(const R, G, B: Byte);
begin
  ggbrRValue.Position := R;
  ggbrGValue.Position := G;
  ggbrBValue.Position := B;
end;

procedure TfrmColor.UpdateColorPageAppearance;
var
  r, g, b, LGray: Byte;
begin
  r      := 0;
  g      := 0;
  b      := 0;
  LGray  := 0;
  
  case FColorMode of
    cmRGB:
      begin
        lblRValue.Caption      := 'R:';
        lblRValue.Font.Color   := clRed;
        edtRValue.Font.Color   := clRed;
        lblGrayPercent.Visible := False;
        lblGValue.Visible      := True;
        edtGValue.Visible      := True;
        ggbrGValue.Visible     := True;
        lblBValue.Visible      := True;
        edtBValue.Visible      := True;
        ggbrBValue.Visible     := True;

        case FCurrColorSelector of
          csForeColor:
            begin
              r := GetRValue(frmMain.GlobalForeColor);
              g := GetGValue(frmMain.GlobalForeColor);
              b := GetBValue(frmMain.GlobalForeColor);
            end;

          csBackColor:
            begin
              r := GetRValue(frmMain.GlobalBackColor);
              g := GetGValue(frmMain.GlobalBackColor);
              b := GetBValue(frmMain.GlobalBackColor);
            end;
        end;

        ggbrRValue.Position := r;
        ggbrGValue.Position := g;
        ggbrBValue.Position := b;

        shpForegroundColor.Brush.Color  := frmMain.GlobalForeColor;
        shpBackgroundColor.Brush.Color  := frmMain.GlobalBackColor;
        
        frmSwatch.shpForegroundSwatch.Brush.Color := frmMain.GlobalForeColor;
        frmSwatch.shpBackgroundSwatch.Brush.Color := frmMain.GlobalBackColor;
      end;

    cmGrayscale:
      begin
        lblRValue.Caption      := 'K:';
        lblRValue.Font.Color   := clBlack;
        edtRValue.Font.Color   := clBlack;
        lblGrayPercent.Visible := True;
        lblGValue.Visible      := False;
        edtGValue.Visible      := False;
        ggbrGValue.Visible     := False;
        lblBValue.Visible      := False;
        edtBValue.Visible      := False;
        ggbrBValue.Visible     := False;

        case FCurrColorSelector of
          csForeColor:
            begin
              LGray := GetRValue(frmMain.ForeGrayColor);
            end;

          csBackColor:
            begin
              LGray := GetRValue(frmMain.BackGrayColor);
            end;
        end;

        { When FColorMode is cmGrayscale, gauge bar ggbrRValue will subtracts
          its position from 255 to get the final grayscale value. In order to
          get the correct value, assign (255 - Gray) to the position of
          ggbrRValue, via the calculations by ggbrRValue, we will get the
          correct value. }

        ggbrRValue.Position             := 255 - LGray;
        shpForegroundColor.Brush.Color  := frmMain.ForeGrayColor;
        shpBackgroundColor.Brush.Color  := frmMain.BackGrayColor;
        
        frmSwatch.shpBackgroundSwatch.Brush.Color := frmMain.BackGrayColor;
        frmSwatch.shpForegroundSwatch.Brush.Color := frmMain.ForeGrayColor;
      end;
  end;
end;

procedure TfrmColor.SetColorMode(const AValue: TgmColorMode);
begin
  if FColorMode <> AValue then
  begin
    FColorMode := AValue;

    UpdateColorPageAppearance;

    // change gradient colors in Gradient Picker
    frmGradientPicker.SetDynamicColors(shpForegroundColor.Brush.Color,
                                       shpBackgroundColor.Brush.Color);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmColor.FormCreate(Sender: TObject);
var
  LHalfHeight: Integer;
begin
{ Fields }
  FColorMode         := cmRGB;
  FCurrColorSelector := csForeColor;
  FLastColorSelector := csForeColor;
  FRValue            := 0;
  FGValue            := 0;
  FBValue            := 0;
  FKValue            := 0;

{ Components }
  shpForegroundColor.Shape       := stCircle;
  shpForegroundColor.Brush.Color := clBlack;
  shpBackgroundColor.Brush.Color := clWhite;
  
  // copy the foreground and background color to global variables
  frmMain.GlobalForeColor := shpForegroundColor.Brush.Color;
  frmMain.GlobalBackColor := shpBackgroundColor.Brush.Color;

  with imgSpectra.Picture do
  begin
    Bitmap.Width       := imgSpectra.Width;
    Bitmap.Height      := imgSpectra.Height;
    Bitmap.PixelFormat := pf24bit;
    LHalfHeight        := Bitmap.Height div 2;

    // filling the background of spectra image
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.Rectangle(0, 0, Bitmap.Width, LHalfHeight);
    
    Bitmap.Canvas.Brush.Color := clBlack;
    Bitmap.Canvas.Rectangle(0, LHalfHeight, Bitmap.Width, Bitmap.Height);
    
    // draw gradient spectra with brightness attenuation
    DrawHueGradientSpectra(Bitmap, 255, 255, 0, 0,
                           Bitmap.Width - LHalfHeight,
                           Bitmap.Height - 1, True);
  end;

  ManualDock(frmMain.pgcntrlDockSite2);
  Show;
  frmMain.pgcntrlDockSite2.ActivePageIndex := 0;
end;

procedure TfrmColor.FormResize(Sender: TObject);
begin
  if Self.Floating then
  begin
    if Width < MIN_FORM_WIDTH then
    begin
      Width := MIN_FORM_WIDTH;
      Abort;
    end;

    if Height < MIN_FORM_HEIGHT then
    begin
      Height := MIN_FORM_HEIGHT;
      Abort;
    end;
  end;
end;

procedure TfrmColor.shpForegroundColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r, g, b: Byte;
begin
  FCurrColorSelector       := csForeColor;
  FLastColorSelector       := csForeColor;
  shpForegroundColor.Shape := stCircle;
  shpBackGroundColor.Shape := stRectangle;

  frmSwatch.shpForegroundSwatch.Shape := stCircle;
  frmSwatch.shpBackGroundSwatch.Shape := stRectangle;

  case FColorMode of
    cmRGB:
      begin
        r := GetRValue(shpForegroundColor.Brush.Color);
        g := GetGValue(shpForegroundColor.Brush.Color);
        b := GetBValue(shpForegroundColor.Brush.Color);
        
        ChangeColorViaTrackBar(r, g, b);
      end;

    cmGrayscale:
      begin
        r := GetRValue(shpForegroundColor.Brush.Color);
        
        ggbrRValue.Position := 255 - r;
      end;
  end;
end;

procedure TfrmColor.shpBackgroundColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r, g, b: Byte;
begin
  FCurrColorSelector       := csBackColor;
  FLastColorSelector       := csBackColor;
  shpForegroundColor.Shape := stRectangle;
  shpBackGroundColor.Shape := stCircle;
  
  frmSwatch.shpForegroundSwatch.Shape := stRectangle;
  frmSwatch.shpBackGroundSwatch.Shape := stCircle;

  case FColorMode of
    cmRGB:
      begin
        r := GetRValue(shpBackgroundColor.Brush.Color);
        g := GetGValue(shpBackgroundColor.Brush.Color);
        b := GetBValue(shpBackgroundColor.Brush.Color);

        ChangeColorViaTrackBar(r, g, b);
      end;

    cmGrayscale:
      begin
        r := GetRValue(shpBackgroundColor.Brush.Color);
        
        ggbrRValue.Position := 255 - r;
      end;
  end;
end;

procedure TfrmColor.ColorValueChange(Sender: TObject);
var
  LGrayValue: Byte;
begin
  LGrayValue := 0;
  
  if Sender = ggbrRValue then
  begin
    case FColorMode of
      cmRGB:
        begin
          edtRValue.Text := IntToStr(ggbrRValue.Position);
          FRValue        := ggbrRValue.Position;
        end;

      cmGrayscale:
        begin
          LGrayValue     := 255 - ggbrRValue.Position;
          FKValue        := MulDiv(ggbrRValue.Position, 100, 255);
          edtRValue.Text := IntToStr(FKValue);
        end;
    end;
  end
  else
  if Sender = ggbrGValue then
  begin
    edtGValue.Text := IntToStr(ggbrGValue.Position);
    FGValue        := ggbrGValue.Position;
  end
  else
  if Sender = ggbrBValue then
  begin
    edtBValue.Text := IntToStr(ggbrBValue.Position);
    FBValue        := ggbrBValue.Position;
  end;

  case FColorMode of
    cmRGB:
      begin
        case FCurrColorSelector of
          csForeColor:
            begin
              frmMain.GlobalForeColor        := RGB(FRValue, FGValue, FBValue);
              shpForegroundColor.Brush.Color := frmMain.GlobalForeColor;

              frmSwatch.shpForegroundSwatch.Brush.Color := frmMain.GlobalForeColor;
            end;

          csBackColor:
            begin
              frmMain.GlobalBackColor        := RGB(FRValue, FGValue, FBValue);
              shpBackgroundColor.Brush.Color := frmMain.GlobalBackColor;

              frmSwatch.shpBackgroundSwatch.Brush.Color := frmMain.GlobalBackColor;
            end;
        end;
      end;

    cmGrayscale:
      begin
        case FCurrColorSelector of
          csForeColor:
            begin
              frmMain.ForeGrayColor          := RGB(LGrayValue, LGrayValue, LGrayValue);
              shpForegroundColor.Brush.Color := frmMain.ForeGrayColor;

              frmSwatch.shpForegroundSwatch.Brush.Color := frmMain.ForeGrayColor;
            end;

          csBackColor:
            begin
              frmMain.BackGrayColor          := RGB(LGrayValue, LGrayValue, LGrayValue);
              shpBackgroundColor.Brush.Color := frmMain.BackGrayColor;

              frmSwatch.shpBackgroundSwatch.Brush.Color := frmMain.BackGrayColor;
            end;
        end;
      end;
  end;

  // change gradient colors in Gradient Picker
  frmGradientPicker.SetDynamicColors(shpForegroundColor.Brush.Color,
                                     shpBackgroundColor.Brush.Color);
end;

procedure TfrmColor.edtRValueChange(Sender: TObject);
begin
  case FColorMode of
    cmRGB:
      begin
        try
          FRValue := StrToInt(edtRValue.Text);

          if (FRValue > 255) or (FRValue < 0) then
          begin
            FRValue        := Clamp(FRValue, 0, 255);
            edtRValue.Text := IntToStr(FRValue);
          end;

          ggbrRValue.Position := FRValue;
        except
          edtRValue.Text := IntToStr(FRValue);
        end;
      end;

    cmGrayscale:
      begin
        try
          FKValue := StrToInt(edtRValue.Text);

          if (FKValue > 100) or (FKValue < 0) then
          begin
            if FKValue < 0 then
            begin
              FKValue := 0;
            end
            else if FKValue > 100 then
            begin
              FKValue := 100;
            end;
            
            edtRValue.Text := IntToStr(FKValue);
          end;

          ggbrRValue.Position := MulDiv(255, FKValue, 100);
        except
          edtRValue.Text := IntToStr(FKValue);
        end;
      end;
  end;
end;

procedure TfrmColor.edtGValueChange(Sender: TObject);
begin
  try
    FGValue := StrToInt(edtGValue.Text);

    if (FGValue > 255) or (FGValue < 0) then
    begin
      FGValue        := Clamp(FGValue, 0, 255);
      edtGValue.Text := IntToStr(FGValue);
    end;

    ggbrGValue.Position := FGValue;
  except
    edtGValue.Text := IntToStr(FGValue);
  end;
end;

procedure TfrmColor.edtBValueChange(Sender: TObject);
begin
  try
    FBValue := StrToInt(edtBValue.Text);

    if (FBValue > 255) or (FBValue < 0) then
    begin
      FBValue        := Clamp(FBValue, 0, 255);
      edtBValue.Text := IntToStr(FBValue);
    end;
    
    ggbrBValue.Position := FBValue;
  except
    edtBValue.Text := IntToStr(FBValue);
  end;
end;

procedure TfrmColor.imgSpectraMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LColor    : TColor;
  r, g, b   : Byte;
  LIntensity: Byte;
begin
  if (X >= 0) and
     (Y >= 0) and
     (X < imgSpectra.Picture.Bitmap.Width) and
     (Y < imgSpectra.Picture.Bitmap.Height) then
  begin
    LColor := imgSpectra.Picture.Bitmap.Canvas.Pixels[X, Y];
    r      := GetRValue(LColor);
    g      := GetGValue(LColor);
    b      := GetBValue(LColor);

    case FColorMode of
      cmRGB:
        begin
          ChangeColorViaTrackBar(r, g, b);
        end;

      cmGrayscale:
        begin
          LIntensity          := (r + g + b) div 3;
          ggbrRValue.Position := 255 - LIntensity;
        end;
    end;
  end;
end;

procedure TfrmColor.imgSpectraMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LColor: TColor;
begin
  if (X >= 0) and
     (Y >= 0) and
     (X < imgSpectra.Picture.Bitmap.Width) and
     (Y < imgSpectra.Picture.Bitmap.Height) then
  begin
    LColor := imgSpectra.Picture.Bitmap.Canvas.Pixels[X, Y];

    frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
    frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
  end;
end;

procedure TfrmColor.ColorValueKeyPress(Sender: TObject; var Key: Char);
const
  LBackspace = #$08;
begin
  if not (Key in [LBackspace, '0'..'9']) then
  begin
    Key := #$00;
  end;
end;


end.
