{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2013 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved.

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

// Updated Date: 2017/01/24

unit AddNoiseDlg;

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_Layers, GR32_RangeBars,
{ GraphicsMagic Lib }
  gmAddNoise, gmTypes, ExtCtrls, GR32_Image;

type
  TUpdateViewProc = procedure;

  TfrmAddNoise = class(TForm)
    grpbxOptions: TGroupBox;
    lblAmount: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    chckbxMonochromatic: TCheckBox;
    pnlThumbnail: TPanel;
    ggbrHorz: TGaugeBar;
    ggbrVert: TGaugeBar;
    imgThumbnail: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrAmountMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtAmountChange(Sender: TObject);
    procedure chckbxMonochromaticClick(Sender: TObject);
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
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
  private
    FThumbnail : TBitmap32;
    FNoiseMode : TgmNoiseMode;

    // for thumbnail
    FStartX, FStartY   : Integer;
    FMouseX, FMouseY   : Integer;
    FHorzPos, FVertPos : Integer;
    FDrawing           : Boolean;
    FExecuteChange     : Boolean;
    
    procedure InitializeThumbnailSettings;
    procedure InitializeAddNoiseSettings;
    procedure ShowThumbnail(const ApplyAddNoise: Boolean);
    procedure UpdateView(const ApplyAddNoise: Boolean);
    procedure ExecuteAddNoise;
  public
    FSourceBmp      : TBitmap32;
    FProcessedBmp   : TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmAddNoise: TfrmAddNoise;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs, gmPaintFuncs;

{$R *.dfm}

const
  SECTION_ADD_NOISE      = 'AddNoiseSettings';
  IDENT_ADD_NOISE_AMOUNT = 'Amount';
  IDENT_NOISE_MODE       = 'NoiseMode';
  IDENT_PREVIEW          = 'Preview';

  APPLY_ADD_NOISE      = True;
  DONT_APPLY_ADD_NOISE = FALSE;

procedure TfrmAddNoise.InitializeThumbnailSettings;
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

procedure TfrmAddNoise.InitializeAddNoiseSettings;
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
    ggbrAmount.Position   := LIniFile.ReadInteger(SECTION_ADD_NOISE, IDENT_ADD_NOISE_AMOUNT, 0);
    FNoiseMode            := TgmNoiseMode( LIniFile.ReadInteger(SECTION_ADD_NOISE, IDENT_NOISE_MODE, 0) );
    chckbxPreview.Checked := LIniFile.ReadBool(SECTION_ADD_NOISE, IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  chckbxMonochromatic.Checked := (FNoiseMode = nmMono);
end;

procedure TfrmAddNoise.ShowThumbnail(const ApplyAddNoise: Boolean);
var
  LOffsetX, LOffsetY : Integer;
begin
  if chckbxPreview.Checked and ApplyAddNoise then
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

procedure TfrmAddNoise.UpdateView(const ApplyAddNoise: Boolean);
begin
  if ApplyAddNoise then
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

procedure TfrmAddNoise.ExecuteAddNoise;
var
  LColorChannelCount : Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);

    case FNoiseMode of
      nmColor:
        begin
          AddColorNoise32(FProcessedBmp, ggbrAmount.Position);
        end;

      nmMono:
        begin
          AddMonoNoise32(FProcessedBmp, ggbrAmount.Position);
        end;
    end;

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
      UpdateView(APPLY_ADD_NOISE);
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmAddNoise.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
  FThumbnail    := TBitmap32.Create;

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
  FExecuteChange  := True;
  FChannelSet     := [csRed, csGreen, csBlue];
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmAddNoise.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FThumbnail.Free;
  
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmAddNoise.FormShow(Sender: TObject);
begin
  InitializeAddNoiseSettings;
  InitializeThumbnailSettings;

  ExecuteAddNoise;
  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.FormClose(Sender: TObject;
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
    LIniFile.WriteBool(SECTION_ADD_NOISE, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmAddNoise.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(SECTION_ADD_NOISE, IDENT_ADD_NOISE_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteInteger(SECTION_ADD_NOISE, IDENT_NOISE_MODE, Ord(FNoiseMode));
    LIniFile.WriteBool(SECTION_ADD_NOISE, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmAddNoise.ggbrAmountChange(Sender: TObject);
begin
  FExecuteChange := False;
  try
    edtAmount.Text := IntToStr(ggbrAmount.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmAddNoise.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteAddNoise;
  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.chckbxPreviewClick(Sender: TObject);
begin
  UpdateView(chckbxPreview.Checked);
  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.edtAmountChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if FExecuteChange then
  begin
    try
      LChangedValue := StrToInt(edtAmount.Text);
      ClampValue(LChangedValue, ggbrAmount.Min, ggbrAmount.Max);

      ggbrAmount.Position := LChangedValue;
      edtAmount.Text      := IntToStr(ggbrAmount.Position);

      ExecuteAddNoise;
      ShowThumbnail(APPLY_ADD_NOISE);
      
    except
      edtAmount.Text := IntToStr(ggbrAmount.Position);
    end;
  end;
end;

procedure TfrmAddNoise.chckbxMonochromaticClick(Sender: TObject);
begin
  if chckbxMonochromatic.Checked then
  begin
    FNoiseMode := nmMono;
  end
  else
  begin
    FNoiseMode := nmColor;
  end;
  
  ExecuteAddNoise;
  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.imgThumbnailMouseDown(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_ADD_NOISE);
  end;
end;

procedure TfrmAddNoise.imgThumbnailMouseMove(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_ADD_NOISE);
  end;
end;

procedure TfrmAddNoise.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;

  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmAddNoise.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_ADD_NOISE);
  end;
end;

procedure TfrmAddNoise.ggbrVertMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.ggbrVertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_ADD_NOISE);
  end;
end;

procedure TfrmAddNoise.ggbrHorzMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_ADD_NOISE);
end;

procedure TfrmAddNoise.ggbrHorzMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_ADD_NOISE);
end;

end.
