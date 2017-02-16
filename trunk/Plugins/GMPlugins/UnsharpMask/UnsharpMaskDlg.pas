{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2010 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
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

unit UnsharpMaskDlg;

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_RangeBars, GR32_Image, GR32_Layers,
{ GraphicsMagicLib }
  gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmUnsharpMask = class(TForm)
    chckbxPreview: TCheckBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    lblAmount: TLabel;
    lblRadius: TLabel;
    lblThreshold: TLabel;
    ggbrAmount: TGaugeBar;
    ggbrRadius: TGaugeBar;
    ggbrThreshold: TGaugeBar;
    GroupBox1: TGroupBox;
    pnlThumbnail: TPanel;
    imgThumbnail: TImage32;
    ggbrHorz: TGaugeBar;
    ggbrVert: TGaugeBar;
    edtAmount: TEdit;
    edtRadius: TEdit;
    edtThreshold: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtAmountChange(Sender: TObject);
    procedure GaugeBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtRadiusChange(Sender: TObject);
    procedure edtThresholdChange(Sender: TObject);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure ggbrHorzChange(Sender: TObject);
    procedure ggbrVertChange(Sender: TObject);
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ThumbnailGaugeBarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ThumbnailGaugeBarMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FThumbnail        : TBitmap32;
    FStartX, FStartY  : Integer;
    FMouseX, FMouseY  : Integer;
    FHorzPos, FVertPos: Integer;
    FDrawing          : Boolean;
    FAllowChange      : Boolean;

    procedure ExecuteUnsharpMask;
    procedure InitializeThumbnialSettings;
    procedure ShowThumbnail(const ApplyEffect: Boolean);
  public
    { Public declarations }
    FSourceBitmap   : TBitmap32;
    FProcessedBitmap: TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmUnsharpMask: TfrmUnsharpMask;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs, gmPaintFuncs, gmUnsharpMask;

{$R *.dfm}

const
  AMOUNT_MID_POS = 100;
  RADIUS_MID_POS = 10;

  APPLY_EFFECT      = True;
  DONT_APPLY_EFFECT = False;

  INI_SECTION                      = 'Unsharp Mask Settings';
  INI_IDENT_UNSHARP_MASK_AMOUNT    = 'Amount';
  INI_IDENT_UNSHARP_MASK_RADIUS    = 'Radius';
  INI_IDENT_UNSHARP_MASK_THRESHOLD = 'Threshold';
  INI_IDENT_PREVIEW                = 'Preview';
  

procedure TfrmUnsharpMask.ExecuteUnsharpMask;
var
  LColorChannelCount: Integer;
  LAmount, LRadius  : Double;
begin
  Screen.Cursor := crHourGlass;
  try
    LAmount := ggbrAmount.Position / AMOUNT_MID_POS;
    LRadius := ggbrRadius.Position / RADIUS_MID_POS;

    FProcessedBitmap.Assign(FSourceBitmap);
    UnsharpMask(FProcessedBitmap, LAmount, LRadius, ggbrThreshold.Position);

    ShowThumbnail(APPLY_EFFECT);

    if csGrayscale in FChannelSet then
    begin
      Desaturate32(FProcessedBitmap);
    end
    else
    begin
      LColorChannelCount := GetColorChannelCount(FChannelSet);

      if (LColorChannelCount > 0) and (LColorChannelCount < 3) then
      begin
        ReplaceRGBChannels(FSourceBitmap, FProcessedBitmap, FChannelSet, crsRemainDest);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      CopyBmpDataToPtr(FProcessedBitmap, FDestBmpPtr, FProcessedBitmap.Width, FProcessedBitmap.Height);

      if Assigned(FUpdateViewProc) then
      begin
        FUpdateViewProc;
      end;
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmUnsharpMask.InitializeThumbnialSettings;
begin
  ggbrHorz.Visible := (FSourceBitmap.Width  > imgThumbnail.Width);
  ggbrVert.Visible := (FSourceBitmap.Height > imgThumbnail.Height);

  if ggbrHorz.Visible then
  begin
    ggbrHorz.Min      := 0;
    ggbrHorz.Max      := FSourceBitmap.Width - imgThumbnail.Width;
    ggbrHorz.Position := 0;
  end;

  if ggbrVert.Visible then
  begin
    ggbrVert.Min      := 0;
    ggbrVert.Max      := FSourceBitmap.Height - imgThumbnail.Height;
    ggbrVert.Position := 0;
  end;

  // change cursor
  if (ggbrHorz.Visible) or (ggbrVert.Visible) then
  begin
    imgThumbnail.Cursor := crSizeAll;
  end
  else
  begin
    imgThumbnail.Cursor := crDefault;
  end;
end;

procedure TfrmUnsharpMask.ShowThumbnail(const ApplyEffect: Boolean);
var
  LOffsetX, LOffsetY: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    if ApplyEffect then
    begin
      CopyRegion(FProcessedBitmap, FThumbnail, FStartX, FStartY,
                 FStartX + imgThumbnail.Width, FStartY + imgThumbnail.Height);
    end
    else
    begin
      CopyRegion(FSourceBitmap, FThumbnail, FStartX, FStartY,
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
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmUnsharpMask.FormCreate(Sender: TObject);
begin
  FSourceBitmap    := TBitmap32.Create;
  FProcessedBitmap := TBitmap32.Create;
  FThumbnail       := TBitmap32.Create;
  
  FDestBmpPtr      := nil;
  FUpdateViewProc  := nil;
  FChannelSet      := [csRed, csGreen, csBlue];
  FStartX          := 0;
  FStartY          := 0;
  FAllowChange     := True;
  FDrawing         := False;

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
end;

procedure TfrmUnsharpMask.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
  FProcessedBitmap.Free;
  FThumbnail.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmUnsharpMask.FormShow(Sender: TObject);
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
    ggbrAmount.Position    := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_AMOUNT,    50);
    ggbrRadius.Position    := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_RADIUS,    10);
    ggbrThreshold.Position := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_THRESHOLD, 0);
    chckbxPreview.Checked  := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FAllowChange := False;
  try
    edtAmount.Text    := IntToStr(ggbrAmount.Position);
    edtRadius.Text    := Format('%.1f', [ggbrRadius.Position / RADIUS_MID_POS]);
    edtThreshold.Text := IntToStr(ggbrThreshold.Position);
  finally
    FAllowChange := True;
  end;

  ExecuteUnsharpMask;

  InitializeThumbnialSettings;
  ShowThumbnail(APPLY_EFFECT);
end;

procedure TfrmUnsharpMask.FormClose(Sender: TObject;
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
    LIniFile.WriteBool(INI_SECTION, INI_IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmUnsharpMask.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_AMOUNT,    ggbrAmount.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_RADIUS,    ggbrRadius.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_UNSHARP_MASK_THRESHOLD, ggbrThreshold.Position);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmUnsharpMask.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);
end;

procedure TfrmUnsharpMask.ggbrRadiusChange(Sender: TObject);
begin
  edtRadius.Text := Format('%.1f', [ggbrRadius.Position / RADIUS_MID_POS]);
end; 

procedure TfrmUnsharpMask.ggbrThresholdChange(Sender: TObject);
begin
  edtThreshold.Text := IntToStr(ggbrThreshold.Position);
end;

procedure TfrmUnsharpMask.GaugeBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmUnsharpMask.GaugeBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := True;
  ExecuteUnsharpMask;
end;

procedure TfrmUnsharpMask.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    CopyBmpDataToPtr(FProcessedBitmap, FDestBmpPtr,
                     FProcessedBitmap.Width, FProcessedBitmap.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
  end;
end; 

procedure TfrmUnsharpMask.edtAmountChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtAmount.Text);
    ClampValue(LChangedValue, ggbrAmount.Min, ggbrAmount.Max);

    ggbrAmount.Position := LChangedValue;
    edtAmount.Text      := IntToStr(ggbrAmount.Position);

    if FAllowChange then
    begin
      ExecuteUnsharpMask;
    end;

  except
    edtAmount.Text := IntToStr(ggbrAmount.Position);
  end;
end;

procedure TfrmUnsharpMask.edtRadiusChange(Sender: TObject);
var
  LChangedValue: Double;
begin
  try
    LChangedValue := StrToFloat(edtRadius.Text);
    ClampValue(LChangedValue, 0.1, 250.0);

    ggbrRadius.Position := Round(LChangedValue * RADIUS_MID_POS);
    edtRadius.Text      := Format('%.1f', [LChangedValue]);

    if FAllowChange then
    begin
      ExecuteUnsharpMask;
    end;

  except
    edtRadius.Text := Format('%.1f', [ggbrRadius.Position / RADIUS_MID_POS]);
  end;
end; 

procedure TfrmUnsharpMask.edtThresholdChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtThreshold.Text);
    ClampValue(LChangedValue, ggbrThreshold.Min, ggbrThreshold.Max);

    ggbrThreshold.Position := LChangedValue;
    edtThreshold.Text      := IntToStr(ggbrThreshold.Position);

    if FAllowChange then
    begin
      ExecuteUnsharpMask;
    end;

  except
    edtThreshold.Text := IntToStr(ggbrThreshold.Position);
  end;
end; 

procedure TfrmUnsharpMask.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end; 

procedure TfrmUnsharpMask.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end;

procedure TfrmUnsharpMask.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end; 

procedure TfrmUnsharpMask.imgThumbnailMouseDown(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end; 

procedure TfrmUnsharpMask.imgThumbnailMouseMove(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end; 

procedure TfrmUnsharpMask.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;
  ShowThumbnail(APPLY_EFFECT);
end;

procedure TfrmUnsharpMask.ThumbnailGaugeBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_EFFECT);
end;

procedure TfrmUnsharpMask.ThumbnailGaugeBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_EFFECT);
end; 

end.
