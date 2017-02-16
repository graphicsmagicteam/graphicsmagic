{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2010 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

unit PencilSketchDlg;

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

// Updated Date: 2017/01/24

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, GR32, GR32_RangeBars, GR32_Image,
  GR32_Layers, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmPencilSketch = class(TForm)
    GroupBox1: TGroupBox;
    lblRadius: TLabel;
    pnlThumbnail: TPanel;
    imgThumbnail: TImage32;
    ggbrVert: TGaugeBar;
    ggbrHorz: TGaugeBar;
    ggbrRadius: TGaugeBar;
    edtRadius: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrVertChange(Sender: TObject);
    procedure ggbrHorzChange(Sender: TObject);
    procedure imgThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgThumbnailMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure imgThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure ThumbnailGaugeBarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ThumbnailGaugeBarMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edtRadiusChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ggbrRadiusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FThumbnail        : TBitmap32;
    FStartX, FStartY  : Integer;
    FMouseX, FMouseY  : Integer;
    FHorzPos, FVertPos: Integer;
    FDrawing          : Boolean;
    FAllowChange      : Boolean;

    procedure ExecutePencilSketch;
    procedure InitializeThumbnialSettings;
    procedure InitializePencilSketchSettings;
    procedure ShowThumbnail(const ApplyEffect: Boolean);
  public
    FSourceBitmap   : TBitmap32;
    FProcessedBitmap: TBitmap32;
    FDestBmpPtr     : PColor32;
    FUpdateViewProc : TUpdateViewProc;
    FChannelSet     : TgmChannelSet;
  end;

var
  frmPencilSketch: TfrmPencilSketch;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmPencilSketch, gmPaintFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  SECTION_PENCIL_SKETCH = 'PencilSketchSettings';
  IDENT_RADIUS          = 'Radius';
  IDENT_PREVIEW         = 'Preview';

  APPLY_EFFECT          = True;
  DONT_APPLY_EFFECT     = False;

procedure TfrmPencilSketch.ExecutePencilSketch;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    PencilSketch(FSourceBitmap, FProcessedBitmap, ggbrRadius.Position);

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

procedure TfrmPencilSketch.InitializeThumbnialSettings;
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

procedure TfrmPencilSketch.InitializePencilSketchSettings;
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
    ggbrRadius.Position   := LIniFile.ReadInteger(SECTION_PENCIL_SKETCH, IDENT_RADIUS, 2);
    chckbxPreview.Checked := LIniFile.ReadBool(SECTION_PENCIL_SKETCH, IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FAllowChange := False;
  try
    edtRadius.Text := IntToStr(ggbrRadius.Position);
  finally
    FAllowChange := True;
  end;   
end;

procedure TfrmPencilSketch.ShowThumbnail(const ApplyEffect: Boolean);
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

procedure TfrmPencilSketch.FormCreate(Sender: TObject);
begin
  FSourceBitmap    := TBitmap32.Create;
  FProcessedBitmap := TBitmap32.Create;
  FThumbnail       := TBitmap32.Create;

  FDestBmpPtr      := nil;
  FUpdateViewProc  := nil;
  FChannelSet      := [csRed, csGreen, csBlue];
  FStartX          := 0;
  FStartY          := 0;
  FDrawing         := False;
  FAllowChange     := True;

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

procedure TfrmPencilSketch.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
  FProcessedBitmap.Free;
  FThumbnail.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmPencilSketch.FormShow(Sender: TObject);
begin
  InitializePencilSketchSettings;
  InitializeThumbnialSettings;
  ExecutePencilSketch;
  ShowThumbnail(APPLY_EFFECT);
end;

procedure TfrmPencilSketch.FormClose(Sender: TObject;
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
    LIniFile.WriteBool(SECTION_PENCIL_SKETCH, IDENT_PREVIEW, chckbxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmPencilSketch.imgThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer);
end;

procedure TfrmPencilSketch.ggbrHorzChange(Sender: TObject);
begin
  FStartX := ggbrHorz.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end;

procedure TfrmPencilSketch.ggbrVertChange(Sender: TObject);
begin
  FStartY := ggbrVert.Position;

  if not FDrawing then
  begin
    ShowThumbnail(DONT_APPLY_EFFECT);
  end;
end;

procedure TfrmPencilSketch.imgThumbnailMouseDown(Sender: TObject;
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

procedure TfrmPencilSketch.imgThumbnailMouseMove(Sender: TObject;
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

procedure TfrmPencilSketch.imgThumbnailMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;
  ShowThumbnail(APPLY_EFFECT);
end;

procedure TfrmPencilSketch.ggbrRadiusChange(Sender: TObject);
begin
  FAllowChange := False;
  try
    edtRadius.Text := IntToStr(ggbrRadius.Position);
  finally
    FAllowChange := True;
  end;
end;

procedure TfrmPencilSketch.ggbrRadiusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmPencilSketch.ggbrRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := True;
  ExecutePencilSketch;
end;

procedure TfrmPencilSketch.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    CopyBmpDataToPtr(FProcessedBitmap, FDestBmpPtr, FProcessedBitmap.Width, FProcessedBitmap.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
  end;
end;

procedure TfrmPencilSketch.ThumbnailGaugeBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(DONT_APPLY_EFFECT);
end;

procedure TfrmPencilSketch.ThumbnailGaugeBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowThumbnail(APPLY_EFFECT);
end;

procedure TfrmPencilSketch.edtRadiusChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtRadius.Text);
    ClampValue(LChangedValue, ggbrRadius.Min, ggbrRadius.Max);

    ggbrRadius.Position := LChangedValue;
    edtRadius.Text      := IntToStr(ggbrRadius.Position);

    if FAllowChange then
    begin
      ExecutePencilSketch;
    end;
  except
    edtRadius.Text := IntToStr(ggbrRadius.Position);
  end;
end;

procedure TfrmPencilSketch.btbtnOKClick(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  // save settings
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LIniFile.WriteInteger(SECTION_PENCIL_SKETCH, IDENT_RADIUS,  ggbrRadius.Position);
  finally
    LIniFile.Free;
  end;
end;

end.
