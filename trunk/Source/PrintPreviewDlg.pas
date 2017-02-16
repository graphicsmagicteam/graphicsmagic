unit PrintPreviewDlg;

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

// Update Date: 2015/10/18

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Preview, ExtCtrls, StdCtrls, Buttons;

type
  TfrmPrintPreview = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cmbbxZoom: TComboBox;
    spdbtnPrintImage: TSpeedButton;
    prntprvwPreview: TPrintPreview;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure spdbtnPrintImageClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    FPreviewBitmap   : TBitmap;
    FFlattenedBitmap : TBitmap;
  public
    procedure DrawBitmapOnPreview;
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmImageProcessFuncs,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PrintOptionsDlg;

{$R *.dfm}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmPrintPreview.DrawBitmapOnPreview;
var
  L, T, R, B : Integer;
  LPaintRect : TRect;
  LBmp       : TBitmap32;
begin
  if ActiveChildForm.LayerList.Count > 1 then
  begin
    FFlattenedBitmap.Assign(ActiveChildForm.LayerList.CombineResult);
  end
  else
  begin
    LBmp := TBitmap32.Create();
    try
      LBmp.Assign(ActiveChildForm.LayerList.SelectedLayer.LayerBitmap);
      LBmp.DrawMode := dmBlend;

      // convert the transparent area to white
      MergeBitmapToColoredBackground(LBmp, clWhite32);
      frmPrintPreview.FFlattenedBitmap.Assign(LBmp);
      frmPrintPreview.FFlattenedBitmap.PixelFormat := pf24bit;
    finally
      LBmp.Free();
    end;
  end;

  frmPrintOptions.PrintOptions.SetPaperSize(prntprvwPreview.PaperWidth,
                                            prntprvwPreview.PaperHeight);

  frmPrintOptions.PrintOptions.SourceBitmap.Assign(FFlattenedBitmap);

  if frmPrintOptions.PrintOptions.IsFitToMedia then
  begin
    frmPrintOptions.PrintOptions.SetFitToMedia(True);
  end;
  
  frmPrintOptions.PrintOptions.CalculateDrawingCoord();

  L := frmPrintOptions.PrintOptions.LeftPixel;
  T := frmPrintOptions.PrintOptions.TopPixel;
  R := frmPrintOptions.PrintOptions.RightPixel;
  B := frmPrintOptions.PrintOptions.BottomPixel;
  
  LPaintRect := Rect(L, T, R, B);

  prntprvwPreview.BeginDoc();
  try
    prntprvwPreview.Canvas.Brush.Color := clWhite;
    prntprvwPreview.Canvas.FillRect(prntprvwPreview.Canvas.ClipRect);

    if frmPrintOptions.PrintOptions.Scale = 100 then
    begin
      prntprvwPreview.Canvas.Draw(L, T, FFlattenedBitmap);
    end
    else
    begin
      prntprvwPreview.Canvas.StretchDraw(LPaintRect, FFlattenedBitmap);
    end;
  finally
    prntprvwPreview.EndDoc();
  end;
end;

procedure TfrmPrintPreview.FormCreate(Sender: TObject);
begin
  FFlattenedBitmap    := TBitmap.Create();
  FPreviewBitmap      := TBitmap.Create();
  cmbbxZoom.ItemIndex := 3;
end;

procedure TfrmPrintPreview.FormDestroy(Sender: TObject);
begin
  FFlattenedBitmap.Free();
  FPreviewBitmap.Free();
end;

procedure TfrmPrintPreview.FormShow(Sender: TObject);
begin
  DrawBitmapOnPreview();
end;

procedure TfrmPrintPreview.spdbtnPrintImageClick(Sender: TObject);
begin
  if prntprvwPreview.State = psReady then
  begin
    if dmMain.PrintDialog.Execute() then
    begin
      prntprvwPreview.Print();
    end;
  end;
end;

procedure TfrmPrintPreview.cmbbxZoomChange(Sender: TObject);
begin
  case cmbbxZoom.ItemIndex of
    0:
      begin
        prntprvwPreview.Zoom := 500;
      end;

    1:
      begin
        prntprvwPreview.Zoom := 200;
      end;

    2:
      begin
        prntprvwPreview.Zoom := 150;
      end;

    3:
      begin
        prntprvwPreview.Zoom := 100;
      end;

    4:
      begin
        prntprvwPreview.Zoom := 75;
      end;
      
    5:
      begin
        prntprvwPreview.Zoom := 50;
      end;

    6:
      begin
        prntprvwPreview.Zoom := 25;
      end;
      
    7:
      begin
        prntprvwPreview.Zoom := 10;
      end;

    8:
      begin
        prntprvwPreview.ZoomState := zsZoomToWidth;
      end;

    9:
      begin
        prntprvwPreview.ZoomState := zsZoomToHeight;
      end;

    10:
      begin
        prntprvwPreview.ZoomState := zsZoomToFit;
      end;
  end;
end; 

end.
