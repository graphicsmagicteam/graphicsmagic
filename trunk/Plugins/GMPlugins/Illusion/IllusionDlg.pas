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

unit IllusionDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, GR32, gmIllusion, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmIllusion = class(TForm)
    grpbxIllusion: TGroupBox;
    lblDivision: TLabel;
    edtDivision: TEdit;
    updwnDivision: TUpDown;
    rdbtnIllusionType1: TRadioButton;
    rdbtnIllusionType2: TRadioButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnAbout: TButton;
    procedure FormShow(Sender: TObject);
    procedure edtDivisionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeIllusioinType(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure updwnDivisionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnDivisionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FIllusion   : TgmGimpIllusion;
    FAllowChange: Boolean;

    procedure ExecuteIllusion;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmIllusion: TfrmIllusion;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs, AboutIllusionDlg;

{$R *.dfm}

const
  INI_SECTION                 = 'IllusionSettings';
  INI_IDENT_ILLUSION_DIVISION = 'Division';
  INI_IDENT_ILLUSION_TYPE     = 'IllusionType';
  INI_IDENT_PREVIEW           = 'Preview';
  

procedure TfrmIllusion.ExecuteIllusion;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FIllusion) then
    begin
      FIllusion.Execute(FProcessedBmp);
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

procedure TfrmIllusion.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  if FIllusion <> nil then
  begin
    FIllusion.Free;
    FIllusion := nil;
  end;

  FIllusion := TgmGimpIllusion.Create(FSourceBmp);

  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    FIllusion.IllusionDivision := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_ILLUSION_DIVISION, 8);
    FIllusion.IllusionType     := TgmIllusionTypes(LIniFile.ReadInteger(INI_SECTION, INI_IDENT_ILLUSION_TYPE, 0));
    chckbxPreview.Checked      := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  FAllowChange := False;
  try
    updwnDivision.Position := FIllusion.IllusionDivision;
  finally
    FAllowChange := True;
  end;

  rdbtnIllusionType1.Checked := (FIllusion.IllusionType = itType1);
  rdbtnIllusionType2.Checked := (FIllusion.IllusionType = itType2);

  ExecuteIllusion;
  ActiveControl := btbtnOK;
end;

procedure TfrmIllusion.edtDivisionChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtDivision.Text);

    ClampValue(LChangedValue, updwnDivision.Min, updwnDivision.Max);

    updwnDivision.Position := LChangedValue;

    if Assigned(FIllusion) then
    begin
      FIllusion.IllusionDivision := updwnDivision.Position;
    end;

    edtDivision.Text := IntToStr(updwnDivision.Position);

    if FAllowChange then
    begin
      ExecuteIllusion;
    end;
    
  except
    edtDivision.Text := IntToStr(updwnDivision.Position);
  end;
end;

procedure TfrmIllusion.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FIllusion       := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end; 

procedure TfrmIllusion.ChangeIllusioinType(Sender: TObject);
begin
  if Assigned(FIllusion) then
  begin
    if rdbtnIllusionType1.Checked then
    begin
      FIllusion.IllusionType := itType1;
    end
    else if rdbtnIllusionType2.Checked then
    begin
      FIllusion.IllusionType := itType2;
    end;

    ExecuteIllusion;
  end;
end;

procedure TfrmIllusion.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmIllusion.btbtnOKClick(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  if Assigned(FIllusion) then
  begin
    GetModuleFileName(hInstance, LDLLName, 256);

    LFileName := LDLLName;
    LFileName := ChangeFileExt(LFileName, '.ini');

    LIniFile := TIniFile.Create(LFileName);
    try
      LIniFile.WriteInteger(INI_SECTION, INI_IDENT_ILLUSION_DIVISION, updwnDivision.Position);
      LIniFile.WriteInteger(INI_SECTION, INI_IDENT_ILLUSION_TYPE, Ord(FIllusion.IllusionType));
    finally
      LIniFile.Free;
    end;
  end;
end; 

procedure TfrmIllusion.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  if FIllusion <> nil then
  begin
    FIllusion.Free;
    FIllusion := nil;
  end;

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

procedure TfrmIllusion.FormDestroy(Sender: TObject);
begin
  if FIllusion <> nil then
  begin
    FIllusion.Free;
  end;

  FSourceBmp.Free;
  FProcessedBmp.Free;
  
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end; 

procedure TfrmIllusion.btnAboutClick(Sender: TObject);
begin
  frmAboutIllusion := TfrmAboutIllusion.Create(Self);
  try
    frmAboutIllusion.ShowModal;
  finally
    frmAboutIllusion.Free;
  end;
end;

procedure TfrmIllusion.updwnDivisionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmIllusion.updwnDivisionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteIllusion;
  FAllowChange := True;
end;

end.
