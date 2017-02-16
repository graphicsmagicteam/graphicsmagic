{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit WaveDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmWave, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmWave = class(TForm)
    GroupBox1: TGroupBox;
    lblAmount: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    GroupBox2: TGroupBox;
    rdbtnStandard: TRadioButton;
    rdbtnExtra: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrAmountMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrAmountMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtAmountChange(Sender: TObject);
    procedure ChangeWaveOption(Sender: TObject);
  private
    FAllowChange: Boolean;
    FWaveOption : TgmWaveOption;
    FMaxAmount  : Integer;
    
    procedure ExecuteWave;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FBKColor       : TColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmWave: TfrmWave;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION           = 'WaveSettings';
  INI_IDENT_WAVE_AMOUNT = 'Amount';
  INI_IDENT_WAVE_OPTION = 'WaveOption';
  INI_IDENT_PREVIEW     = 'Preview';
  

procedure TfrmWave.ExecuteWave;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Wave32(FSourceBmp, FProcessedBmp, ggbrAmount.Position, FWaveOption, FBKColor);

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

procedure TfrmWave.FormCreate(Sender: TObject);
begin
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmWave.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmWave.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LAmount  : Integer;
begin
  FMaxAmount := FSourceBmp.Height div 2;

  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LAmount               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_WAVE_AMOUNT, 0);
    FWaveOption           := TgmWaveOption( LIniFile.ReadInteger(INI_SECTION, INI_IDENT_WAVE_OPTION, 0) );
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  if LAmount > FMaxAmount then
  begin
    LAmount := 0;
  end;

  ggbrAmount.Position   := LAmount;
  ggbrAmount.Max        := FMaxAmount;
  rdbtnStandard.Checked := (FWaveOption = woStandard);
  rdbtnExtra.Checked    := (FWaveOption = woExtra);

  ExecuteWave;
end;

procedure TfrmWave.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmWave.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_WAVE_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_WAVE_OPTION, Ord(FWaveOption));
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmWave.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);

  if FAllowChange then
  begin
    ExecuteWave;
  end;
end;

procedure TfrmWave.ggbrAmountMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmWave.ggbrAmountMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ExecuteWave;
  FAllowChange := True;
end;

procedure TfrmWave.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmWave.edtAmountChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtAmount.Text);
    ClampValue(LChangedValue, 0, FMaxAmount);

    ggbrAmount.Position := LChangedValue;
    edtAmount.Text      := IntToStr(ggbrAmount.Position);
  except
    edtAmount.Text := IntToStr(ggbrAmount.Position);
  end;
end;

procedure TfrmWave.ChangeWaveOption(Sender: TObject);
begin
  if rdbtnStandard.Checked then
  begin
    FWaveOption := woStandard;
  end
  else if rdbtnExtra.Checked then
  begin
    FWaveOption := woExtra;
  end;

  ExecuteWave;
end; 

end.
