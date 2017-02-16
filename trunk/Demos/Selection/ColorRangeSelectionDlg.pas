{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit ColorRangeSelectionDlg;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, GR32_Image, ExtCtrls, ComCtrls, GR32, GR32_Layers,
  GR32_RangeBars, gmTypes;

type
  TfrmColorRangeSelection = class(TForm)
    grpbxColorRangeOptions: TGroupBox;
    lblFuzziness: TLabel;
    edtFuzzinessValue: TEdit;
    pnlColorRangeThumbnail: TPanel;
    imgColorRangeThumbnail: TImage32;
    rdbtnSelection: TRadioButton;
    rdbtnImage: TRadioButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    lblSampledColor: TLabel;
    shpSampledColor: TShape;
    lblCurrentColor: TLabel;
    shpCurrentColor: TShape;
    ggbrFuzziness: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeFuzziness(Sender: TObject);
    procedure imgColorRangeThumbnailMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgColorRangeThumbnailMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure rdbtnSelectionClick(Sender: TObject);
    procedure rdbtnImageClick(Sender: TObject);
  private
    FShadowBitmap     : TBitmap32;
    FScaleBitmap      : TBitmap32;
    FXActual, FYActual: Integer;
    FScaleWidth       : Integer;
    FScaleHeight      : Integer;
    FThumbnailMode    : TgmThumbnailMode;
  public
    FSourceBitmap: TBitmap32;
    FSampledColor: TColor32;
    FFuzziness   : Integer;
  end;

var
  frmColorRangeSelection: TfrmColorRangeSelection;

implementation

uses
  Main,
{ GraphicsMagic Lib }
  gmImageProcessFuncs,  // MakeColorRangeShadow32()...
  gmMath,
  gmGUIFuncs;

{$R *.DFM}

procedure TfrmColorRangeSelection.FormCreate(Sender: TObject);
begin
  FSourceBitmap               := TBitmap32.Create;
  FShadowBitmap               := TBitmap32.Create;
  FScaleBitmap                := TBitmap32.Create;
  FSampledColor               := clBlack32;
  FFuzziness                  := 10;
  FXActual                    := 0;
  FYActual                    := 0;
  shpSampledColor.Brush.Color := WinColor(FSampledColor);
  shpCurrentColor.Brush.Color := WinColor(FSampledColor);
  FScaleWidth                 := pnlColorRangeThumbnail.Width  - 16;
  FScaleHeight                := pnlColorRangeThumbnail.Height - 16;
  FThumbnailMode              := tmSelection;
end;

procedure TfrmColorRangeSelection.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
  FShadowBitmap.Free;
  FScaleBitmap.Free;
end;

procedure TfrmColorRangeSelection.FormShow(Sender: TObject);
begin
  if Assigned(frmMain.Selection) then
  begin
    frmMain.Selection.IsAnimated := False;             
    FSourceBitmap.Assign(frmMain.Selection.CutOriginal);
  end
  else
  begin
    FSourceBitmap.Assign(frmMain.CurrentLayer.Bitmap);
  end;

  case FThumbnailMode of
    tmSelection:
      begin
        MakeColorRangeShadow32(FSourceBitmap, FShadowBitmap, FSampledColor, FFuzziness);
        GetScaledBitmap(FShadowBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
        imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
      end;

    tmImage:
      begin
        GetScaledBitmap(FSourceBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
        imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
      end;
  end;
  
  CenterImageInPanel(pnlColorRangeThumbnail, imgColorRangeThumbnail);
end;

procedure TfrmColorRangeSelection.ChangeFuzziness(Sender: TObject);
begin
  FFuzziness             := ggbrFuzziness.Position;
  edtFuzzinessValue.Text := IntToStr(FFuzziness);

  if FThumbnailMode = tmSelection then
  begin
    MakeColorRangeShadow32(FSourceBitmap, FShadowBitmap, FSampledColor, FFuzziness);
    GetScaledBitmap(FShadowBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
    imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
  end;
end;

procedure TfrmColorRangeSelection.imgColorRangeThumbnailMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FXActual := MulDiv(X, FSourceBitmap.Width - 1,  imgColorRangeThumbnail.Width - 1);
  FYActual := MulDiv(Y, FSourceBitmap.Height - 1, imgColorRangeThumbnail.Height - 1);

  shpCurrentColor.Brush.Color := FSourceBitmap.Canvas.Pixels[FXActual, FYActual];
end;

procedure TfrmColorRangeSelection.imgColorRangeThumbnailMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FXActual := MulDiv(X, FSourceBitmap.Width - 1,  imgColorRangeThumbnail.Width - 1);
  FYActual := MulDiv(Y, FSourceBitmap.Height - 1, imgColorRangeThumbnail.Height - 1);

  FSampledColor               := FSourceBitmap.Pixel[FXActual, FYActual];
  shpSampledColor.Brush.Color := WinColor(FSampledColor);

  if FThumbnailMode = tmSelection then
  begin
    MakeColorRangeShadow32(FSourceBitmap, FShadowBitmap, FSampledColor, FFuzziness);
    GetScaledBitmap(FShadowBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
    imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
  end;
end;

procedure TfrmColorRangeSelection.rdbtnSelectionClick(Sender: TObject);
begin
  FThumbnailMode := tmSelection;
  MakeColorRangeShadow32(FSourceBitmap, FShadowBitmap, FSampledColor, FFuzziness);
  GetScaledBitmap(FShadowBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
  imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
end;

procedure TfrmColorRangeSelection.rdbtnImageClick(Sender: TObject);
begin
  FThumbnailMode := tmImage;
  GetScaledBitmap(FSourceBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
  imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
end; 

end.
