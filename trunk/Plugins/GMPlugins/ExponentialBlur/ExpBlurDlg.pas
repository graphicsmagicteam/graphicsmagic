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

unit ExpBlurDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmExpBlur = class(TForm)
    GroupBox1: TGroupBox;
    lblAmout: TLabel;
    edtRadius: TEdit;
    ggbrRadius: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrRadiusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtRadiusChange(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteExpBlur;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmExpBlur: TfrmExpBlur;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmExpBlur, gmPluginFuncs, gmImageProcessFuncs,
{ Plugin Dialogs }
  AboutExpBlurDlg;

{$R *.dfm}

const
  INI_SECTION               = 'ExpBlurSettings';
  INI_IDENT_EXP_BLUR_RADIUS = 'Radius';
  INI_IDENT_PREVIEW         = 'Preview';

procedure TfrmExpBlur.ExecuteExpBlur;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    ExpBlur(FProcessedBmp, ggbrRadius.Position);

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
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmExpBlur.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end; 

procedure TfrmExpBlur.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end; 

procedure TfrmExpBlur.FormShow(Sender: TObject);
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
    ggbrRadius.Position   := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_EXP_BLUR_RADIUS, 1);
  finally
    LIniFile.Free;
  end;

  ExecuteExpBlur;
end;

procedure TfrmExpBlur.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmExpBlur.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_EXP_BLUR_RADIUS, ggbrRadius.Position);
  finally
    LIniFile.Free;
  end;
end; 

procedure TfrmExpBlur.ggbrRadiusChange(Sender: TObject);
begin
  edtRadius.Text := IntToStr(ggbrRadius.Position);

  if FAllowChange then
  begin
    ExecuteExpBlur;
  end;
end; 

procedure TfrmExpBlur.ggbrRadiusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmExpBlur.ggbrRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteExpBlur;
  FAllowChange := True;
end;

procedure TfrmExpBlur.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr,
                     FProcessedBmp.Width, FProcessedBmp.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
  end;
end; 

procedure TfrmExpBlur.edtRadiusChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtRadius.Text);
    ClampValue(LChangedValue, ggbrRadius.Min, ggbrRadius.Max);

    ggbrRadius.Position := LChangedValue;
    edtRadius.Text      := IntToStr(ggbrRadius.Position);
  except
    edtRadius.Text := IntToStr(ggbrRadius.Position);
  end;
end;

procedure TfrmExpBlur.btnAboutClick(Sender: TObject);
begin
  frmAboutExpBlur := TfrmAboutExpBlur.Create(Self);
  try
    frmAboutExpBlur.ShowModal;
  finally
    FreeAndNil(frmAboutExpBlur);
  end;
end;

end.
