unit SwatchForm;

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

// Update Date: 2017/01/20

{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin,
{ GraphicsMagic Lib }
  gmSwatches;

type
  TfrmSwatch = class(TForm)
    tlbrSwatches: TToolBar;
    ToolButton2: TToolButton;
    shpCurrentSwatch: TShape;
    edtSwatchCount: TEdit;
    ToolButton3: TToolButton;
    shpForegroundSwatch: TShape;
    ToolButton4: TToolButton;
    shpBackgroundSwatch: TShape;
    ToolButton8: TToolButton;
    tlbtnCreateSwatch: TToolButton;
    tlbtnSwatchOptions: TToolButton;
    scrlbxSwatches: TScrollBox;
    imgSwatches: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgSwatchesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgSwatchesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure shpBackgroundSwatchMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shpForegroundSwatchMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FSwatchList: TgmSwatchList;
  public
    procedure PutSwatchImageAtTopLeft;

    property SwatchList: TgmSwatchList read FSwatchList;
  end;

var
  frmSwatch: TfrmSwatch;

implementation

uses
{ GraphicsMagic Lib }
  gmIni,
  gmTypes,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  ColorForm;

{$R *.dfm}

const
  MIN_FORM_WIDTH  = 236;
  MIN_FORM_HEIGHT = 154;

var
  ImageSwatchX, ImageSwatchY: Integer;

procedure TfrmSwatch.PutSwatchImageAtTopLeft;
begin
  scrlbxSwatches.HorzScrollBar.Position := 0;
  scrlbxSwatches.VertScrollBar.Position := 0;

  imgSwatches.Left := 0;
  imgSwatches.Top  := 0;
end;

procedure TfrmSwatch.FormCreate(Sender: TObject);
var
  LSwatchesName     : string;
  LUseInternalSwatch: Integer;
begin
  FSwatchList := TgmSwatchList.Create(scrlbxSwatches.Width - 16, scrlbxSwatches.Height);
  try
    LUseInternalSwatch := StrToInt( ReadInfoFromIniFile(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '1') );
  except
    LUseInternalSwatch := 1;

    WriteInfoToIniFile( SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, IntToStr(LUseInternalSwatch) );
  end;

  if LUseInternalSwatch > 0 then
  begin
    FSwatchList.LoadInternalSwatchesToList;
  end
  else
  begin         
    LSwatchesName := ReadInfoFromIniFile(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, '');

    FSwatchList.LoadSwatchesToList(LSwatchesName);

    // if failure in loading external Swatches, then load the internal Swatches
    if FSwatchList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '1');
    end;
  end;

  FSwatchList.ShowSwatches(imgSwatches);

  edtSwatchCount.Text := IntToStr(FSwatchList.Count);

  shpForegroundSwatch.Shape       := stCircle;
  shpForegroundSwatch.Brush.Color := clBlack;
  shpBackgroundSwatch.Brush.Color := clWhite;

  ManualDock(frmMain.pgcntrlDockSite2);
  Show;
end;

procedure TfrmSwatch.FormDestroy(Sender: TObject);
begin
  FSwatchList.Free;
end;

procedure TfrmSwatch.FormShow(Sender: TObject);
begin
  PutSwatchImageAtTopLeft;
end;

procedure TfrmSwatch.FormResize(Sender: TObject);
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

procedure TfrmSwatch.imgSwatchesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LIntensity: Byte;
begin
  case Button of
    mbLeft:
      begin
        if FSwatchList.GetSwatch(X, Y) then
        begin
          case frmColor.ColorMode of
            cmRGB:
              begin
                frmColor.ggbrRValue.Position := FSwatchList.SelectedSwatch.Red;
                frmColor.ggbrGValue.Position := FSwatchList.SelectedSwatch.Green;
                frmColor.ggbrBValue.Position := FSwatchList.SelectedSwatch.Blue;
              end;

            cmGrayscale:
              begin
                LIntensity := (FSwatchList.SelectedSwatch.Red   +
                               FSwatchList.SelectedSwatch.Green +
                               FSwatchList.SelectedSwatch.Blue) div 3;

                frmColor.ggbrRValue.Position := 255 - LIntensity;
              end;
          end;
        end;
      end;

    mbRight:
      begin
        if FSwatchList.GetSwatch(X, Y) then
        begin
          dmMain.HideSwatchesPopupMenuItemsForImage();

          ImageSwatchX          := X;
          ImageSwatchY          := Y;
          imgSwatches.PopupMenu := dmMain.pmnSwatches;
        end
        else
        begin
          imgSwatches.PopupMenu := nil;
        end;
      end;
  end;
end;

procedure TfrmSwatch.imgSwatchesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LIndex : Integer;
  LSwatch: TgmSwatch;
  r, g, b: Byte;
begin
  if FSwatchList.Count > 0 then
  begin
    LIndex := FSwatchList.GetSwatchIndex(X, Y);

    if (LIndex >= 0) and (LIndex < FSwatchList.Count) then
    begin
      imgSwatches.Cursor := crHandPoint;

      LSwatch := TgmSwatch(FSwatchList.Items[LIndex]);
      r       := LSwatch.Red;
      g       := LSwatch.Green;
      b       := LSwatch.Blue;

      shpCurrentSwatch.Brush.Color := RGB(r, g, b);

      frmMain.ShowColorRGBInfoOnInfoViewer(shpCurrentSwatch.Brush.Color);
      frmMain.ShowColorCMYKInfoOnInfoViewer(shpCurrentSwatch.Brush.Color);
    end
    else
    begin
      imgSwatches.Cursor := crNo;
    end;
  end;
end;

procedure TfrmSwatch.shpBackgroundSwatchMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r, g, b: Byte;
begin
  frmColor.CurrColorSelector        := csBackColor;
  frmColor.LastColorSelector        := csBackColor;
  frmColor.shpForegroundColor.Shape := stRectangle;
  frmColor.shpBackGroundColor.Shape := stCircle;

  shpForegroundSwatch.Shape := stRectangle;
  shpBackGroundSwatch.Shape := stCircle;

  case frmColor.ColorMode of
    cmRGB:
      begin
        r := GetRValue(frmColor.shpBackgroundColor.Brush.Color);
        g := GetGValue(frmColor.shpBackgroundColor.Brush.Color);
        b := GetBValue(frmColor.shpBackgroundColor.Brush.Color);

        frmColor.ChangeColorViaTrackBar(r, g, b);
      end;

    cmGrayscale:
      begin
        r := GetRValue(frmColor.shpBackgroundColor.Brush.Color);

        frmColor.ggbrRValue.Position := 255 - r;
      end;
  end;
end;

procedure TfrmSwatch.shpForegroundSwatchMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r, g, b: Byte;
begin
  frmColor.CurrColorSelector        := csForeColor;
  frmColor.LastColorSelector        := csForeColor;
  frmColor.shpForegroundColor.Shape := stCircle;
  frmColor.shpBackGroundColor.Shape := stRectangle;

  shpForegroundSwatch.Shape := stCircle;
  shpBackGroundSwatch.Shape := stRectangle;
  
  case frmColor.ColorMode of
    cmRGB:
      begin
        r := GetRValue(frmColor.shpForegroundColor.Brush.Color);
        g := GetGValue(frmColor.shpForegroundColor.Brush.Color);
        b := GetBValue(frmColor.shpForegroundColor.Brush.Color);
        
        frmColor.ChangeColorViaTrackBar(r, g, b);
      end;

    cmGrayscale:
      begin
        r := GetRValue(frmColor.shpForegroundColor.Brush.Color);
        
        frmColor.ggbrRValue.Position := 255 - r;
      end;
  end;
end;

end.
