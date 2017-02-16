unit SmartBlurDlg;

{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2014 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved.

  Updated Date: 2017/01/24

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

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_RangeBars,
{ GraphicsMagicLib }
  gmTypes,
  gmSmartBlurFilter;

type
  TUpdateViewProc = procedure;

  TfrmSmartBlur = class(TForm)
    grpbxOptions: TGroupBox;
    pnlThumbnail: TPanel;
    imgThumbnail: TImage32;
    ggbrVert: TGaugeBar;
    ggbrHorz: TGaugeBar;
    lblRadius: TLabel;
    ggbrRadius: TGaugeBar;
    lblThreshold: TLabel;
    ggbrThreshold: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure ggbrThresholdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btbtnOKClick(Sender: TObject);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ggbrVertChange(Sender: TObject);
    procedure ggbrVertMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrVertMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrHorzChange(Sender: TObject);
    procedure ggbrHorzMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrHorzMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
  private
    FSmartBlur         : TgmSmartBlurFilter;
    FThumbnail         : TBitmap32;

    // for thumbnail
    FStartX, FStartY   : Integer;
    FMouseX, FMouseY   : Integer;
    FHorzPos, FVertPos : Integer;
    FDrawing           : Boolean;

    procedure ApplySmartBlur;
    procedure InitializeSmartBlurSettings;
    procedure InitializeThumbnailSettings;
    procedure ShowThumbnail(const ApplyFilter: Boolean);
    procedure UpdateView(const ApplyFilter: Boolean);
  public
    FSourceBmp      : TBitmap32;
    FProcessedBmp   : TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmSmartBlur: TfrmSmartBlur;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmImageProcessFuncs,
  gmPaintFuncs,
  gmPluginFuncs;

{$R *.dfm}

const
  SECTION_SMART_BLUR         = 'SmartBlurSettings';
  IDENT_SMART_BLUR_RADIUS    = 'Radius';
  IDENT_SMART_BLUR_THRESHOLD = 'Threshold';
  IDENT_PREVIEW              = 'Preview';

  APPLY_FILTER               = True;
  DONT_APPLY_FILTER          = FALSE;


procedure TfrmSmartBlur.ApplySmartBlur;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    FSmartBlur.Filter(FProcessedBmp);

    if csGrayscale in FChannelSet then
    begin
      Desaturate32(FProcessedBmp);
    end
    else
    begin
      LColorChannelCount := GetColorChannelCount(FChannelSet);

      if (LColorChannelCount > 0) and (LColorChannelCount < 3) then
      begin
        ReplaceRGBChannels(FSourceBmp, FProcessedBmp, FChannelSet, crsRemainDest);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      UpdateView(APPLY_FILTER);
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSmartBlur.InitializeSmartBlurSettings;
var
  LIniFile  : TIniFile;
  LDLLName  : array [0..255] of Char;
  LFileName : string;
begin
  GetModuleFileName(hInstance, LDLLName, 256);
  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    ggbrRadius.Position    := LIniFile.ReadInteger(SECTION_SMART_BLUR, IDENT_SMART_BLUR_RADIUS, 90);
    ggbrThreshold.Position := LIniFile.ReadInteger(SECTION_SMART_BLUR, IDENT_SMART_BLUR_THRESHOLD, 250);
    chckbxPreview.Checked  := LIniFile.ReadBool(SECTION_SMART_BLUR, IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FSmartBlur.Radius    := ggbrRadius.Position / 10;
  FSmartBlur.Threshold := ggbrThreshold.Position / 10;
end;

procedure TfrmSmartBlur.InitializeThumbnailSettings;
begin
  ggbrHorz.Visible := (FSourceBmp.Width  > imgThumbnail.Width);
  ggbrVert.Visible := (FSourceBmp.Height > imgThumbnail.Height);

  if ggbrHorz.Visible then
  begin
    ggbrHorz.Min      := 0;
    ggbrHorz.Max      := FSourceBmp.Width - imgThumbnail.Width;
    ggbrHorz.Position := 0;
  end;

  if ggbrVert.Visible then
  begin
    ggbrVert.Min      := 0;
    ggbrVert.Max      := FSourceBmp.Height - imgThumbnail.Height;
    ggbrVert.Position := 0;
  end;

  // Change Cursor
  if (ggbrHorz.Visible) or (ggbrVert.Visible) then
  begin
    imgThumbnail.Cursor := crSizeAll;
  end
  else
  begin
    imgThumbnail.Cursor := crDefault;
  end;
end;

procedure TfrmSmartBlur.ShowThumbnail(const ApplyFilter: Boolean);
var
  LOffsetX, LOffsetY : Integer;
begin
  if chckbxPreview.Checked and ApplyFilter then
  begin
    CopyRegion(FProcessedBmp, FThumbnail, FStartX, FStartY,
               FStartX + imgThumbnail.Width, FStartY + imgThumbnail.Height);
  end
  else
  begin
    CopyRegion(FSourceBmp, FThumbnail, FStartX, FStartY,
               FStartX + imgThumbnail.Width, FStartY + imgThumbnail.Height);
  end;

  LOffsetX := (imgThumbnail.Width  - FThumbnail.Width ) div 2;
  LOffsetY := (imgThumbnail.Height - FThumbnail.Height) div 2;

  // clear background
  imgThumbnail.Bitmap.Clear(Color32(ColorToRGB(clBtnFace)));
  DrawBitmap32WithARGB(FThumbnail, imgThumbnail.Bitmap, LOffsetX, LOffsetY);

  // change thumbnail by selected channels
  ExtractBitmapChannels(imgThumbnail.Bitmap, FChannelSet);
  imgThumbnail.Bitmap.Changed;
end;

procedure TfrmSmartBlur.UpdateView(const ApplyFilter: Boolean);
begin
  if ApplyFilter then
  begin
    CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);
  end
  else
  begin
    CopyBmpDataToPtr(FSourceBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);
  end;

  if Assigned(FUpdateViewProc) then
  begin
    FUpdateViewProc;
  end;
end;

procedure TfrmSmartBlur.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
  FThumbnail    := TBitmap32.Create;
  FSmartBlur    := TgmSmartBlurFilter.Create;

  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force imgThumbnail
  // to call the OnPaintStage event instead of performing default action.
  with imgThumbnail.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;

  imgThumbnail.Bitmap.SetSize(imgThumbnail.Width, imgThumbnail.Height);
  imgThumbnail.Bitmap.DrawMode := dmBlend;

  FStartX         := 0;
  FStartY         := 0;
  FDrawing        := False;
  FChannelSet     := [csRed, csGreen, csBlue];
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSmartBlur.FormDestroy(Sender: TObject);
begin
  FSmartBlur.Free;
  FThumbnail.Free;
  FProcessedBmp.Free;
  FSourceBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSmartBlur.FormShow(Sender: TObject);
begin
  InitializeSmartBlurSettings();
  InitializeThumbnailSettings();
  ApplySmartBlur();
  ShowThumbnail(APPLY_FILTER);
end;

procedure TfrmSmartBlur.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LIniFile.WriteBool(SECTION_SMART_BLUR, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSmartBlur.ggbrRadiusChange(Sender: TObject);
begin
  lblRadius.Caption := Format('Radius: %.1f', [ggbrRadius.Position / 10]);
end;

procedure TfrmSmartBlur.ggbrRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSmartBlur.Radius := ggbrRadius.Position / 10;

  ApplySmartBlur();
  ShowThumbnail(APPLY_FILTER);
end;

procedure TfrmSmartBlur.ggbrThresholdChange(Sender: TObject);
begin
  lblThreshold.Caption := Format('Threshold: %.1f', [ggbrThreshold.Position / 10]);
end;

procedure TfrmSmartBlur.ggbrThresholdMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSmartBlur.Threshold := ggbrThreshold.Position / 10;
  
  ApplySmartBlur();
  ShowThumbnail(APPLY_FILTER);
end;

procedure TfrmSmartBlur.btbtnOKClick(Sender: TObject);
var
  LIniFile  : TIniFile;
  LDLLName  : array [0..255] of Char;
  LFileName : string;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LIniFile.WriteInteger(SECTION_SMART_BLUR, IDENT_SMART_BLUR_RADIUS, ggbrRadius.Position);
    LIniFile.WriteInteger(SECTION_SMART_BLUR, IDENT_SMART_BLUR_THRESHOLD, ggbrThreshold.Position);
    LIniFile.WriteBool(SECTION_SMART_BLUR, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSmartBlur.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmSmartBlur.imgThumbnailMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if (ggbrHorz.Visible) or (ggbrVert.Visible) then
  begin
    FMouseX  := X;
    FMouseY  := Y;
    FHorzPos := ggbrHorz.Position;
    FVertPos := ggbrVert.Position;
    FDrawing := True;

    ShowThumbnail(DONT_APPLY_FILTER);
  end;
end;

procedure TfrmSmartBlur.imgThumbnailMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    if ggbrHorz.Visible then
    begin
      ggbrHorz.Position := FHorzPos + (X - FMouseX);
    end;

    if ggbrVert.Visible then
    begin
      ggbrVert.Position := FVertPos + (Y - FMouseY);
    end;

    ShowThumbnail(DONT_APPLY_FILTER);
  end;
end;

procedure TfrmSmartBlur.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    FDrawing := False;

    ShowThumbnail(APPLY_Filter);
  end;
end;

procedure TfrmSmartBlur.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_FILTER);
  end;
end;

procedure TfrmSmartBlur.ggbrVertMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_FILTER);
end;

procedure TfrmSmartBlur.ggbrVertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_FILTER);
end;

procedure TfrmSmartBlur.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_FILTER);
  end;
end;

procedure TfrmSmartBlur.ggbrHorzMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_FILTER);
end;

procedure TfrmSmartBlur.ggbrHorzMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_FILTER);
end;

procedure TfrmSmartBlur.chckbxPreviewClick(Sender: TObject);
begin
  UpdateView(chckbxPreview.Checked);
  ShowThumbnail(APPLY_FILTER);
end;

end.
