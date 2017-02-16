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

unit EmbossDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, GR32, GR32_RangeBars, GR32_Image,
  GR32_Layers, gmGimpEmboss, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmEmboss = class(TForm)
    GroupBox1: TGroupBox;
    pnlThumbnail: TPanel;
    imgThumbnail: TImage32;
    ggbrVert: TGaugeBar;
    ggbrHorz: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    Label1: TLabel;
    cmbbxChannel: TComboBox;
    Label2: TLabel;
    rdbtnBumpmap: TRadioButton;
    rdbtnEmboss: TRadioButton;
    lblAzimuth: TLabel;
    ggbrAzimuth: TGaugeBar;
    lblElevation: TLabel;
    ggbrElevation: TGaugeBar;
    lblDepth: TLabel;
    ggbrDepth: TGaugeBar;
    edtAzimuth: TEdit;
    edtElevation: TEdit;
    edtDepth: TEdit;
    chckbxPreview: TCheckBox;
    btnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrVertChange(Sender: TObject);
    procedure ggbrHorzChange(Sender: TObject);
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmbbxChannelChange(Sender: TObject);
    procedure ChangeFumctionMode(Sender: TObject);
    procedure ggbrAzimuthChange(Sender: TObject);
    procedure ggbrElevationChange(Sender: TObject);
    procedure ggbrDepthChange(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure edtAzimuthChange(Sender: TObject);
    procedure edtElevationChange(Sender: TObject);
    procedure edtDepthChange(Sender: TObject);
    procedure GaugeBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure btnAboutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEmbossFilter : TgmEmbossFilter;
    FThumbnail    : TBitmap32;

    // for thumbnail
    FStartX, FStartY   : Integer;
    FMouseX, FMouseY   : Integer;
    FHorzPos, FVertPos : Integer;
    FDrawing           : Boolean;
    FExecuteChange     : Boolean;

    procedure InitializeThumbnailSettings;
    procedure InitializeEmbossSettings;
    procedure ShowThumbnail(const ApplyEmboss: Boolean);
    procedure UpdateView(const ApplyEmboss: Boolean);
    procedure ExecuteEmboss;
  public
    FSourceBmp      : TBitmap32;
    FProcessedBmp   : TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmEmboss: TfrmEmboss;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs, gmPaintFuncs,
{ Plugin Dialogs }
  AboutDlg;

{$R *.dfm}

const
  SECTION_EMBOSS      = 'EmbossSettings';
  IDENT_CHANNEL       = 'Channel';
  IDENT_FUNCTION_MODE = 'FunctionMode';
  IDENT_AZIMUTH       = 'Azimuth';
  IDENT_ELEVATION     = 'Elevation';
  IDENT_DEPTH         = 'Depth';
  IDENT_PREVIEW       = 'Preview';

  APPLY_EMBOSS        = True;
  DONT_APPLY_EMBOSS   = FALSE;


procedure TfrmEmboss.InitializeThumbnailSettings;
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

procedure TfrmEmboss.InitializeEmbossSettings;
var
  LIniFile  : TIniFile;
  LDLLName  : array [0..255] of Char;
  LFileName : string;
begin
  if Assigned(FEmbossFilter) then
  begin
    GetModuleFileName(hInstance, LDLLName, 256);
    LFileName := LDLLName;
    LFileName := ChangeFileExt(LFileName, '.ini');

    LIniFile := TIniFile.Create(LFileName);
    try
      with FEmbossFilter do
      begin
        Channel   := TgmColorChannel( LIniFile.ReadInteger(SECTION_EMBOSS, IDENT_CHANNEL, 0) );
        EmbossP   := TgmFunctionMode( LIniFile.ReadInteger(SECTION_EMBOSS, IDENT_FUNCTION_MODE, 1) );
        Azimuth   := LIniFile.ReadFloat(SECTION_EMBOSS, IDENT_AZIMUTH, 30.0);
        Elevation := LIniFile.ReadFloat(SECTION_EMBOSS, IDENT_ELEVATION, 45.0);
        Depth     := LIniFile.ReadInteger(SECTION_EMBOSS, IDENT_DEPTH, 20);
        IsPreview := LIniFile.ReadBool(SECTION_EMBOSS, IDENT_PREVIEW, True);
      end;
    finally
      LIniFile.Free;
    end;

    cmbbxChannel.ItemIndex := Ord(FEmbossFilter.Channel);
    rdbtnBumpmap.Checked   := (FEmbossFilter.EmbossP = fmBumpmap);
    rdbtnEmboss.Checked    := (FEmbossFilter.EmbossP = fmEmboss);
    ggbrAzimuth.Position   := Round(FEmbossFilter.Azimuth);
    ggbrElevation.Position := Round(FEmbossFilter.Elevation);
    ggbrDepth.Position     := FEmbossFilter.Depth;
    chckbxPreview.Checked  := FEmbossFilter.IsPreview;
  end;
end;

procedure TfrmEmboss.ShowThumbnail(const ApplyEmboss: Boolean);
var
  LOffsetX, LOffsetY : Integer;
begin
  if chckbxPreview.Checked and ApplyEmboss then
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

procedure TfrmEmboss.UpdateView(const ApplyEmboss: Boolean);
begin
  if ApplyEmboss then
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

procedure TfrmEmboss.ExecuteEmboss;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    FEmbossFilter.Emboss(FProcessedBmp);

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
      UpdateView(APPLY_EMBOSS);
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmEmboss.FormCreate(Sender: TObject);
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

  FEmbossFilter := TgmEmbossFilter.Create;

  FStartX         := 0;
  FStartY         := 0;
  FDrawing        := False;
  FExecuteChange  := True;
  FChannelSet     := [csRed, csGreen, csBlue];
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmEmboss.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FThumbnail.Free;
  FEmbossFilter.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmEmboss.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then  
  begin
    ShowThumbnail(DONT_APPLY_EMBOSS);
  end;
end;

procedure TfrmEmboss.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_EMBOSS);
  end;
end;

procedure TfrmEmboss.imgThumbnailMouseDown(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_EMBOSS);
  end;
end;

procedure TfrmEmboss.imgThumbnailMouseMove(Sender: TObject;
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

    ShowThumbnail(DONT_APPLY_EMBOSS);
  end;
end;

procedure TfrmEmboss.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;

  ExecuteEmboss;
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ExecuteEmboss;
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.cmbbxChannelChange(Sender: TObject);
begin
  FEmbossFilter.Channel := TgmColorChannel(cmbbxChannel.ItemIndex);

  ExecuteEmboss;
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.ChangeFumctionMode(Sender: TObject);
begin
  if Sender = rdbtnBumpmap then
  begin
    FEmbossFilter.EmbossP := fmBumpmap;
  end
  else if Sender = rdbtnEmboss then
  begin
    FEmbossFilter.EmbossP := fmEmboss;
  end;

  ExecuteEmboss;
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.ggbrAzimuthChange(Sender: TObject);
begin
  FEmbossFilter.Azimuth := ggbrAzimuth.Position;
  
  FExecuteChange := False;
  try
    edtAzimuth.Text := IntToStr(ggbrAzimuth.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmEmboss.ggbrElevationChange(Sender: TObject);
begin
  FEmbossFilter.Elevation := ggbrElevation.Position;

  FExecuteChange := False;
  try
    edtElevation.Text := IntToStr(ggbrElevation.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmEmboss.ggbrDepthChange(Sender: TObject);
begin
  FEmbossFilter.Depth := ggbrDepth.Position;

  FExecuteChange := False;
  try
    edtDepth.Text := IntToStr(ggbrDepth.Position);
  finally
    FExecuteChange := True;
  end;
end;

procedure TfrmEmboss.chckbxPreviewClick(Sender: TObject);
begin
  FEmbossFilter.IsPreview := chckbxPreview.Checked;

  UpdateView(chckbxPreview.Checked);
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.btbtnOKClick(Sender: TObject);
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
    with FEmbossFilter do
    begin
      LIniFile.WriteInteger(SECTION_EMBOSS, IDENT_CHANNEL,       Ord(Channel));
      LIniFile.WriteInteger(SECTION_EMBOSS, IDENT_FUNCTION_MODE, Ord(EmbossP));
      LIniFile.WriteFloat  (SECTION_EMBOSS, IDENT_AZIMUTH,       Azimuth);
      LIniFile.WriteFloat  (SECTION_EMBOSS, IDENT_ELEVATION,     Elevation);
      LIniFile.WriteInteger(SECTION_EMBOSS, IDENT_DEPTH,         Depth);
      LIniFile.WriteBool   (SECTION_EMBOSS, IDENT_PREVIEW,       IsPreview);
    end;
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmEmboss.edtAzimuthChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if FExecuteChange then
  begin
    try
      LChangedValue := StrToInt(edtAzimuth.Text);
      ClampValue(LChangedValue, ggbrAzimuth.Min, ggbrAzimuth.Max);

      ggbrAzimuth.Position := LChangedValue;
      edtAzimuth.Text      := IntToStr(ggbrAzimuth.Position);

      ExecuteEmboss;
      ShowThumbnail(APPLY_EMBOSS);
    except
      edtAzimuth.Text := IntToStr(ggbrAzimuth.Position);
    end;
  end;
end;

procedure TfrmEmboss.edtElevationChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if FExecuteChange then
  begin
    try
      LChangedValue := StrToInt(edtElevation.Text);
      ClampValue(LChangedValue, ggbrElevation.Min, ggbrElevation.Max);

      ggbrElevation.Position := LChangedValue;
      edtElevation.Text      := IntToStr(ggbrElevation.Position);

      ExecuteEmboss;
      ShowThumbnail(APPLY_EMBOSS);
    except
      edtElevation.Text := IntToStr(ggbrElevation.Position);
    end;
  end;
end;

procedure TfrmEmboss.edtDepthChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if FExecuteChange then
  begin
    try
      LChangedValue := StrToInt(edtDepth.Text);
      ClampValue(LChangedValue, ggbrDepth.Min, ggbrDepth.Max);

      ggbrDepth.Position := LChangedValue;
      edtDepth.Text      := IntToStr(ggbrDepth.Position);

      ExecuteEmboss;
      ShowThumbnail(APPLY_EMBOSS);
    except
      edtDepth.Text := IntToStr(ggbrDepth.Position);
    end;
  end;
end;

procedure TfrmEmboss.GaugeBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_EMBOSS);
end;

procedure TfrmEmboss.FormShow(Sender: TObject);
begin
  InitializeEmbossSettings;
  InitializeThumbnailSettings;

  ExecuteEmboss;
  ShowThumbnail(APPLY_EMBOSS);
end;

procedure TfrmEmboss.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmEmboss.btnAboutClick(Sender: TObject);
begin
  frmAboutEmboss := TfrmAboutEmboss.Create(Self);
  try
    frmAboutEmboss.ShowModal;
  finally
    FreeAndNil(frmAboutEmboss);
  end;
end; 

procedure TfrmEmboss.FormClose(Sender: TObject; var Action: TCloseAction);
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
    LIniFile.WriteBool(SECTION_EMBOSS, IDENT_PREVIEW, FEmbossFilter.IsPreview);
  finally
    LIniFile.Free;
  end;
end;

end.
