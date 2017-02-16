{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2013 Ma Xiaoguang, Ma Xiaoming < gmbros@hotmail.com >
  and GraphicsMagic Team, all rights reserved.

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

unit AntialiasDlg;

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_RangeBars,
{ GraphicsMagicLib }
  gmTypes, ExtCtrls, GR32_Image, GR32_Layers;

type
  TUpdateViewProc = procedure;

  TfrmAntialias = class(TForm)
    grpbxOptions: TGroupBox;
    lblAmout: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    ggbrHorz: TGaugeBar;
    pnlThumbnail: TPanel;
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
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure ggbrHorzChange(Sender: TObject);
    procedure ggbrHorzMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrHorzMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrVertChange(Sender: TObject);
    procedure ggbrVertMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrVertMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FThumbnail : TBitmap32;

    // for thumbnail
    FStartX, FStartY   : Integer;
    FMouseX, FMouseY   : Integer;
    FHorzPos, FVertPos : Integer;
    FDrawing           : Boolean;
    FExecuteChange     : Boolean;
    
    procedure InitializeThumbnailSettings;
    procedure InitializeAntialiasSettings;
    procedure ShowThumbnail(const ApplyAntialias: Boolean);
    procedure UpdateView(const ApplyAntialias: Boolean);
    procedure ExecuteAntialias;
  public
    FSourceBmp      : TBitmap32;
    FProcessedBmp   : TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmAntialias: TfrmAntialias;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmAntialias, gmPluginFuncs, gmImageProcessFuncs, gmPaintFuncs;

{$R *.dfm}

const
  SECTION_ANTIALIAS      = 'AntialiasSettings';
  IDENT_ANTIALIAS_AMOUNT = 'Amount';
  IDENT_PREVIEW          = 'Preview';

  APPLY_ANTIALIAS      = True;
  DONT_APPLY_ANTIALIAS = FALSE;


procedure TfrmAntialias.InitializeThumbnailSettings;
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

procedure TfrmAntialias.InitializeAntialiasSettings;
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
    ggbrAmount.Position   := LIniFile.ReadInteger(SECTION_ANTIALIAS, IDENT_ANTIALIAS_AMOUNT, 1);
    chckbxPreview.Checked := LIniFile.ReadBool(SECTION_ANTIALIAS, IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmAntialias.ShowThumbnail(const ApplyAntialias: Boolean);
var
  LOffsetX, LOffsetY : Integer;
begin
  if chckbxPreview.Checked and ApplyAntialias then
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

procedure TfrmAntialias.UpdateView(const ApplyAntialias: Boolean);
begin
  if ApplyAntialias then
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

procedure TfrmAntialias.ExecuteAntialias;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    Antialias32(FProcessedBmp, ggbrAmount.Position);

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
      UpdateView(APPLY_ANTIALIAS);
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmAntialias.FormCreate(Sender: TObject);
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

procedure TfrmAntialias.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FThumbnail.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmAntialias.FormShow(Sender: TObject);
begin
  InitializeAntialiasSettings;
  InitializeThumbnailSettings;

  ExecuteAntialias;
  ShowThumbnail(APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.FormClose(Sender: TObject;
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
    LIniFile.WriteBool(SECTION_ANTIALIAS, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmAntialias.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(SECTION_ANTIALIAS, IDENT_ANTIALIAS_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteBool(SECTION_ANTIALIAS, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmAntialias.ggbrAmountChange(Sender: TObject);
begin
  FExecuteChange := False;
  try
    edtAmount.Text := IntToStr(ggbrAmount.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmAntialias.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteAntialias;
  ShowThumbnail(APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.chckbxPreviewClick(Sender: TObject);
begin
  UpdateView(chckbxPreview.Checked);
  ShowThumbnail(APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.edtAmountChange(Sender: TObject);
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

      ExecuteAntialias;
      ShowThumbnail(APPLY_ANTIALIAS);
      
    except
      edtAmount.Text := IntToStr(ggbrAmount.Position);
    end;
  end;
end; 

procedure TfrmAntialias.imgThumbnailMouseDown(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_ANTIALIAS);
  end;
end;

procedure TfrmAntialias.imgThumbnailMouseMove(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_ANTIALIAS);
  end;
end;

procedure TfrmAntialias.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;

  ShowThumbnail(APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmAntialias.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_ANTIALIAS);
  end;
end;

procedure TfrmAntialias.ggbrHorzMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.ggbrHorzMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then  
  begin
    ShowThumbnail(DONT_APPLY_ANTIALIAS);
  end;
end;

procedure TfrmAntialias.ggbrVertMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_ANTIALIAS);
end;

procedure TfrmAntialias.ggbrVertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_ANTIALIAS);
end;

end.
