{
  Original authors:
    Ma Xiaoguang, Ma Xiaoming

  CopyRight(C) Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Last Update:
    2012-09-13

  ------------------------------------------------------------------

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit MainForm;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ExtDlgs, StdCtrls,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    imgvwWorkArea: TImgView32;
    btnLoadImage: TButton;
    grpbxOptions: TGroupBox;
    lblAmount: TLabel;
    ggbrAmount: TGaugeBar;
    lblRadius: TLabel;
    ggbrRadius: TGaugeBar;
    lblThreshold: TLabel;
    ggbrThreshold: TGaugeBar;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadImageClick(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FSourceBitmap: TBitmap32;

    procedure ExecuteUnsharpMask;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG,
{ GraphicsMagic }
  gmUnsharpMask;

{$R *.dfm}

const
  AMOUNT_MID_POS = 100;
  RADIUS_MID_POS = 10;

procedure TfrmMain.ExecuteUnsharpMask;
var
  LAmount: Double;
  LRadius: Double;
begin
  LAmount := ggbrAmount.Position / AMOUNT_MID_POS;
  LRadius := ggbrRadius.Position / RADIUS_MID_POS;

  imgvwWorkArea.Bitmap.Assign(FSourceBitmap);
  UnsharpMask(imgvwWorkArea.Bitmap, LAmount, LRadius, ggbrThreshold.Position);
end; 

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSourceBitmap := nil;

  lblAmount.Caption    := 'Amount: ' + IntToStr(ggbrAmount.Position) + '%';
  lblRadius.Caption    := Format('Radius: %.1f pixels', [ggbrRadius.Position / RADIUS_MID_POS]);
  lblThreshold.Caption := 'Threshold: ' + IntToStr(ggbrThreshold.Position) + ' levels';
end; 

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
end;

procedure TfrmMain.LoadImageClick(Sender: TObject);
var
  LJPEGImage: TJPEGImage;
  LTempBmp  : TBitmap;
begin
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FSourceBitmap) then
      begin
        FreeAndNil(FSourceBitmap);
      end;

      FSourceBitmap := TBitmap32.Create;

      LJPEGImage := TJPEGImage.Create;
      LTempBmp   := TBitmap.Create;
      try
        LJPEGImage.LoadFromFile(OpenPictureDialog.FileName);
        
        LTempBmp.Height      := LJPEGImage.Height;
        LTempBmp.Width       := LJPEGImage.Width;
        LTempBmp.PixelFormat := pf24bit;
        LTempBmp.Canvas.Draw(0, 0, LJPEGImage);

        FSourceBitmap.Assign(LTempBmp);
      finally
        LTempBmp.Free;
        LJPEGImage.Free;
      end;

      ExecuteUnsharpMask;

      grpbxOptions.Visible := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ggbrAmountChange(Sender: TObject);
begin
  lblAmount.Caption := 'Amount: ' + IntToStr(ggbrAmount.Position) + '%';
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  lblRadius.Caption := Format('Radius: %.1f pixels', [ggbrRadius.Position / RADIUS_MID_POS]);
end;

procedure TfrmMain.ggbrThresholdChange(Sender: TObject);
begin
  lblThreshold.Caption := 'Threshold: ' + IntToStr(ggbrThreshold.Position) + ' levels';
end;

procedure TfrmMain.GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    ExecuteUnsharpMask;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
