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

unit HighPassDlg;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
{ Graphics32 }
  GR32, GR32_RangeBars, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmHighPass, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmHighPass = class(TForm)
    grpbxHighPassOptions: TGroupBox;
    pnlThumbnail: TPanel;
    ggbrVert: TGaugeBar;
    ggbrHorz: TGaugeBar;
    lblRadius: TLabel;
    edtRadius: TEdit;
    lblPixels: TLabel;
    ggbrRadius: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    imgThumbnail: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure edtRadiusChange(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btbtnOKClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
  private
    FHighPassFilter : TgmHighPass;
    FThumbnail      : TBitmap32;

    // for thumbnail
    FStartX, FStartY   : Integer;
    FMouseX, FMouseY   : Integer;
    FHorzPos, FVertPos : Integer;
    FDrawing           : Boolean;
    FExecuteChange     : Boolean;

    procedure InitializeThumbnailSettings;
    procedure InitializeHighPassSettings;
    procedure ShowThumbnail(const ApplyHighPass: Boolean);
    procedure UpdateView(const ApplyHighPass: Boolean);
    procedure ExecuteHighPass;
  public
    FSourceBmp      : TBitmap32;
    FProcessedBmp   : TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmHighPass: TfrmHighPass;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs, gmPaintFuncs;

{$R *.dfm}

const
  SECTION_HIGHPASS = 'HighPassSettings';
  IDENT_RADIUS     = 'Radius';
  IDENT_PREVIEW    = 'Preview';

  APPLY_HIGHPASS      = True;
  DONT_APPLY_HIGHPASS = FALSE;

procedure TfrmHighPass.InitializeThumbnailSettings;
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

procedure TfrmHighPass.InitializeHighPassSettings;
var
  LIniFile  : TIniFile;
  LDLLName  : array [0..255] of Char;
  LFileName : string;
begin
  if Assigned(FHighPassFilter) then
  begin
    GetModuleFileName(hInstance, LDLLName, 256);
    LFileName := LDLLName;
    LFileName := ChangeFileExt(LFileName, '.ini');

    LIniFile := TIniFile.Create(LFileName);
    try
      FHighPassFilter.Radius := LIniFile.ReadFloat(SECTION_HIGHPASS, IDENT_RADIUS, 10.0);
      chckbxPreview.Checked  := LIniFile.ReadBool(SECTION_HIGHPASS, IDENT_PREVIEW, True);
    finally
      LIniFile.Free;
    end;

    ggbrRadius.Position := Round(FHighPassFilter.Radius);
  end;
end;

procedure TfrmHighPass.ShowThumbnail(const ApplyHighPass: Boolean);
var
  LOffsetX, LOffsetY : Integer;
begin
  if chckbxPreview.Checked and ApplyHighPass then
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

procedure TfrmHighPass.UpdateView(const ApplyHighPass: Boolean);
begin
  if ApplyHighPass then
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

procedure TfrmHighPass.ExecuteHighPass;
var
  LColorChannelCount : Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    FHighPassFilter.Execute(FProcessedBmp);

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
      UpdateView(APPLY_HIGHPASS);
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmHighPass.FormCreate(Sender: TObject);
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

  FHighPassFilter := TgmHighPass.Create;
  FStartX         := 0;
  FStartY         := 0;
  FDrawing        := False;
  FExecuteChange  := True;
  FChannelSet     := [csRed, csGreen, csBlue];
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmHighPass.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FThumbnail.Free;
  FHighPassFilter.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmHighPass.FormShow(Sender: TObject);
begin
  InitializeHighPassSettings;
  InitializeThumbnailSettings;

  ExecuteHighPass;
  ShowThumbnail(APPLY_HighPass);
end;

procedure TfrmHighPass.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmHighPass.imgThumbnailMouseDown(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_HIGHPASS);
  end;
end;

procedure TfrmHighPass.imgThumbnailMouseMove(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_HIGHPASS);
  end;
end;

procedure TfrmHighPass.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;

  ShowThumbnail(APPLY_HIGHPASS);
end;

procedure TfrmHighPass.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then  
  begin
    ShowThumbnail(DONT_APPLY_HIGHPASS);
  end;
end;

procedure TfrmHighPass.ggbrVertMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_HIGHPASS);
end;

procedure TfrmHighPass.ggbrVertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_HIGHPASS);
end;

procedure TfrmHighPass.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_HIGHPASS);
  end;
end;

procedure TfrmHighPass.ggbrHorzMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_HIGHPASS);
end;

procedure TfrmHighPass.ggbrHorzMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_HIGHPASS);
end;

procedure TfrmHighPass.edtRadiusChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if FExecuteChange then
  begin
    try
      LChangedValue := StrToInt(edtRadius.Text);
      ClampValue(LChangedValue, ggbrRadius.Min, ggbrRadius.Max);

      ggbrRadius.Position := LChangedValue;
      edtRadius.Text      := IntToStr(ggbrRadius.Position);

      ExecuteHighPass;
      ShowThumbnail(APPLY_HIGHPASS);

    except
      edtRadius.Text := IntToStr(ggbrRadius.Position);
    end;
  end;
end;

procedure TfrmHighPass.ggbrRadiusChange(Sender: TObject);
begin
  FHighPassFilter.Radius := ggbrRadius.Position;
  
  FExecuteChange := False;
  try
    edtRadius.Text := IntToStr(ggbrRadius.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmHighPass.ggbrRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteHighPass;
  ShowThumbnail(APPLY_HIGHPASS);
end;

procedure TfrmHighPass.btbtnOKClick(Sender: TObject);
var
  LIniFile  : TIniFile;
  LDLLName  : array [0..255] of Char;
  LFileName : string;
begin
  // save settings
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LIniFile.WriteFloat(SECTION_HIGHPASS, IDENT_RADIUS,  FHighPassFilter.Radius);
    LIniFile.WriteBool(SECTION_HIGHPASS, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmHighPass.chckbxPreviewClick(Sender: TObject);
begin
  UpdateView(chckbxPreview.Checked);
  ShowThumbnail(APPLY_HIGHPASS);
end;

end.
