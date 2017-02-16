{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
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

unit FlareFXDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, GR32_Image, GR32,
  GR32_Layers, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmFlareFX = class(TForm)
    grpbxPreview: TGroupBox;
    pnlThumbnailHolder: TPanel;
    img32Thumbnail: TImage32;
    grpbxParamSettings: TGroupBox;
    lblCenterX: TLabel;
    lblCenterY: TLabel;
    edtFlareCX: TEdit;
    edtFlareCY: TEdit;
    updwnFlareCX: TUpDown;
    updwnFlareCY: TUpDown;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    StatusBar: TStatusBar;
    btnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtFlareCXChange(Sender: TObject);
    procedure edtFlareCYChange(Sender: TObject);
    procedure img32ThumbnailMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure img32ThumbnailMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure img32ThumbnailPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAboutClick(Sender: TObject);
    procedure updwnFlareCXMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnFlareCXMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnFlareCYMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnFlareCYMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FThumbnail  : TBitmap32;
    FImageW     : Integer;
    FImageH     : Integer;
    FAllowChange: Boolean;
    FFlareCenter: TPoint;

    procedure ExecuteFlareFX;
    procedure UpdateUpDownPosition(const X, Y: Integer);
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmFlareFX: TfrmFlareFX;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmFlareFX,
  gmPluginFuncs,
  gmImageProcessFuncs,
  gmGUIFuncs,
{ Plugin Dialogs }
  AboutFlareFXDlg;

{$R *.dfm}

const
  INI_SECTION       = 'FlareFXSettings';
  INI_IDENT_PREVIEW = 'Preview';

procedure TfrmFlareFX.ExecuteFlareFX;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    GimpFlareFX32(FSourceBmp, FProcessedBmp, FFlareCenter.X, FFlareCenter.Y);

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
      CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);

      if Assigned(FUpdateViewProc) then
      begin
        FUpdateViewProc;
      end;
    end;

    // update thumbnail
    GetScaledBitmap(FProcessedBmp, FThumbnail, FImageW, FImageH);

    if not (csGrayscale in FChannelSet) then
    begin
      LColorChannelCount := GetColorChannelCount(FChannelSet);

      if (LColorChannelCount > 0) and (LColorChannelCount < 3) then
      begin
        ExtractBitmapChannels(FThumbnail, FChannelSet);
      end;
    end;

    img32Thumbnail.Bitmap.Assign(FThumbnail);
    img32Thumbnail.Bitmap.DrawMode := dmBlend;
    img32Thumbnail.Bitmap.Changed;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmFlareFX.UpdateUpDownPosition(const X, Y: Integer);
begin
  FAllowChange := False;
  try
    updwnFlareCX.Position := X;
    updwnFlareCY.Position := Y;
  finally
    FAllowChange := True;
  end;
end;

procedure TfrmFlareFX.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
  FChannelSet   := [csRed, csGreen, csBlue];

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FFlareCenter    := Point(0, 0);

  // draw custom stage
  with img32Thumbnail.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;

  FThumbnail          := TBitmap32.Create;
  FThumbnail.DrawMode := dmBlend;

  FImageW := pnlThumbnailHolder.Width  - 16;
  FImageH := pnlThumbnailHolder.Height - 16;
end;

procedure TfrmFlareFX.FormDestroy(Sender: TObject);
begin
  FThumbnail.Free;
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end; 

procedure TfrmFlareFX.FormShow(Sender: TObject);
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
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FFlareCenter.X   := (FSourceBmp.Width  - 1) div 2;
  FFlareCenter.Y   := (FSourceBmp.Height - 1) div 2;
  updwnFlareCX.Max := FSourceBmp.Width  - 1;
  updwnFlareCY.Max := FSourceBmp.Height - 1;

  UpdateUpDownPosition(FFlareCenter.X, FFlareCenter.Y);
  ExecuteFlareFX;
  CenterImageInPanel(pnlThumbnailHolder, img32Thumbnail);
  ActiveControl := btbtnOK;
end;

procedure TfrmFlareFX.edtFlareCXChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtFlareCX.Text);
    ClampValue(LChangedValue, updwnFlareCX.Min, updwnFlareCX.Max);

    updwnFlareCX.Position := LChangedValue;
    FFlareCenter.X        := LChangedValue;
    edtFlareCX.Text       := IntToStr(updwnFlareCX.Position);

    if FAllowChange then
    begin
      ExecuteFlareFX;
    end;
    
  except
    edtFlareCX.Text := IntToStr(updwnFlareCX.Position);
  end;
end;

procedure TfrmFlareFX.edtFlareCYChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtFlareCY.Text);
    ClampValue(LChangedValue, updwnFlareCY.Min, updwnFlareCY.Max);

    updwnFlareCY.Position := LChangedValue;
    FFlareCenter.Y        := LChangedValue;
    edtFlareCY.Text       := IntToStr(updwnFlareCY.Position);

    if FAllowChange then
    begin
      ExecuteFlareFX;
    end;
    
  except
    edtFlareCY.Text := IntToStr(updwnFlareCY.Position);
  end;
end;

procedure TfrmFlareFX.img32ThumbnailMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FFlareCenter.X := MulDiv(X, FSourceBmp.Width,  img32Thumbnail.Width);
  FFlareCenter.Y := MulDiv(Y, FSourceBmp.Height, img32Thumbnail.Height);

  UpdateUpDownPosition(FFlareCenter.X, FFlareCenter.Y);
  ExecuteFlareFX;
end;

procedure TfrmFlareFX.img32ThumbnailMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LXActual, LYActual: Integer;
begin
  LXActual := MulDiv(X, FSourceBmp.Width,  img32Thumbnail.Width);
  LYActual := MulDiv(Y, FSourceBmp.Height, img32Thumbnail.Height);

  StatusBar.SimpleText := Format('X: %d, Y: %d', [LXActual, LYActual]);
end;

procedure TfrmFlareFX.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
  end;
end;

procedure TfrmFlareFX.img32ThumbnailPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  Buffer.FillRect(0, 0, Buffer.Width, Buffer.Height, clWhite32);
end;

procedure TfrmFlareFX.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmFlareFX.btnAboutClick(Sender: TObject);
begin
  frmAboutFlareFX := TfrmAboutFlareFX.Create(Self);
  try
    frmAboutFlareFX.ShowModal;
  finally
    frmAboutFlareFX.Free;
  end;
end;

procedure TfrmFlareFX.updwnFlareCXMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmFlareFX.updwnFlareCXMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteFlareFX;
  FAllowChange := True;
end;

procedure TfrmFlareFX.updwnFlareCYMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmFlareFX.updwnFlareCYMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteFlareFX;
  FAllowChange := True;
end;

end.
