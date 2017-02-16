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

unit GlassTileDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmGlassTile = class(TForm)
    GroupBox1: TGroupBox;
    lblTileWidth: TLabel;
    lblTileHeight: TLabel;
    edtTileWidth: TEdit;
    edtTileHeight: TEdit;
    updwnTileWidth: TUpDown;
    updwnTileHeight: TUpDown;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure updwnTileWidthMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnTileHeightMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtTileWidthChange(Sender: TObject);
    procedure edtTileHeightChange(Sender: TObject);
    procedure updwnTileWidthMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure updwnTileHeightMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnAboutClick(Sender: TObject);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteGlassTile;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FTileWidth     : Integer;
    FTileHeight    : Integer;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmGlassTile: TfrmGlassTile;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmGimpGlassTile,
  gmPluginFuncs,
  gmImageProcessFuncs,
{ Plugin Dialogs }
  AboutGlassTileDlg;

{$R *.dfm}

const
  INI_SECTION           = 'PluginSettings';
  INI_IDENT_TILE_WIDTH  = 'TileWidth';
  INI_IDENT_TILE_HEIGHT = 'TileHeight';
  INI_IDENT_PREVIEW     = 'Preview';

procedure TfrmGlassTile.ExecuteGlassTile;
var
  LColorChannelCount: Integer;
begin
  FTileWidth    := updwnTileWidth.Position;
  FTileHeight   := updwnTileHeight.Position;
  Screen.Cursor := crHourGlass;
  try
    GimpGlassTile32(FSourceBmp, FProcessedBmp, FTileWidth, FTileHeight);

    if not (csGrayscale in FChannelSet) then
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
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGlassTile.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  FTileWidth  := 0;
  FTileHeight := 0;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmGlassTile.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmGlassTile.FormShow(Sender: TObject);
var
  LIniFile   : TIniFile;
  LDLLName   : array [0..255] of Char;
  LFileName  : string;
  LTileWidth : Integer;
  LTileHeight: Integer;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LTileWidth            := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_TILE_WIDTH, 20);
    LTileHeight           := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_TILE_HEIGHT, 20);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FProcessedBmp.Width  := FSourceBmp.Width;
  FProcessedBmp.Height := FSourceBmp.Height;

  updwnTileWidth.Position  := LTileWidth;
  updwnTileHeight.Position := LTileHeight;

  ExecuteGlassTile;
end;

procedure TfrmGlassTile.updwnTileWidthMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGlassTile;
  FAllowChange := True;
end;

procedure TfrmGlassTile.updwnTileHeightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGlassTile;
  FAllowChange := True;
end;

procedure TfrmGlassTile.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmGlassTile.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_TILE_WIDTH, updwnTileWidth.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_TILE_HEIGHT, updwnTileHeight.Position);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmGlassTile.FormClose(Sender: TObject;
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

procedure TfrmGlassTile.edtTileWidthChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtTileWidth.Text);
    ClampValue(LChangedValue, updwnTileWidth.Min, updwnTileWidth.Max);

    updwnTileWidth.Position := LChangedValue;
    edtTileWidth.Text       := IntToStr(updwnTileWidth.Position);

    if FAllowChange then
    begin
      ExecuteGlassTile;
    end;

  except
    edtTileWidth.Text := IntToStr(updwnTileWidth.Position);
  end;
end;

procedure TfrmGlassTile.edtTileHeightChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtTileHeight.Text);
    ClampValue(LChangedValue, updwnTileHeight.Min, updwnTileHeight.Max);

    updwnTileHeight.Position := LChangedValue;
    edtTileHeight.Text       := IntToStr(updwnTileHeight.Position);

    if FAllowChange then
    begin
      ExecuteGlassTile;
    end;
    
  except
    edtTileHeight.Text := IntToStr(updwnTileHeight.Position);
  end;
end;

procedure TfrmGlassTile.updwnTileWidthMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmGlassTile.updwnTileHeightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmGlassTile.btnAboutClick(Sender: TObject);
begin
  frmAboutGlassTile := TfrmAboutGlassTile.Create(Self);
  try
    frmAboutGlassTile.ShowModal;
  finally
    frmAboutGlassTile.Free;
  end;
end; 

end.
