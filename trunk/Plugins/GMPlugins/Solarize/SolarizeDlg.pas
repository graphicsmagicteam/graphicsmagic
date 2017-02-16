{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SolarizeDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmSolarize = class(TForm)
    GroupBox1: TGroupBox;
    lblAmount: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ggbrAmountChange(Sender: TObject);
    procedure ggbrAmountMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrAmountMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtAmountChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteSolarize;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmSolarize: TfrmSolarize;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmSolarize, gmPluginFuncs, gmImageProcessFuncs;

const
  INI_SECTION               = 'SolarizeSettings';
  INI_IDENT_SOLARIZE_AMOUNT = 'Amount';
  INI_IDENT_PREVIEW         = 'Preview';

{$R *.dfm}

procedure TfrmSolarize.ExecuteSolarize;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    if FProcessedBmp.Width <> FSourceBmp.Width then
    begin
      FProcessedBmp.Width := FSourceBmp.Width;
    end;

    if FProcessedBmp.Height <> FSourceBmp.Height then
    begin
      FProcessedBmp.Height := FSourceBmp.Height;
    end;

    Solarize32(FSourceBmp, FProcessedBmp, 255 - ggbrAmount.Position);

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

procedure TfrmSolarize.FormCreate(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;
  FAllowChange    := True;
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FChannelSet     := [csRed, csGreen, csBlue];
end; 

procedure TfrmSolarize.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp.Free;
  FProcessedBmp.Free;
end; 

procedure TfrmSolarize.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LAmount  : Integer;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LAmount               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SOLARIZE_AMOUNT, 0);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrAmount.Position := LAmount;
  edtAmount.Text      := IntToStr(ggbrAmount.Position);
  
  ExecuteSolarize;
end;

procedure TfrmSolarize.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);

  if FAllowChange then
  begin
    ExecuteSolarize;
  end;
end;

procedure TfrmSolarize.ggbrAmountMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmSolarize.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSolarize;
  FAllowChange := True;
end; 

procedure TfrmSolarize.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSolarize.edtAmountChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtAmount.Text);
    ClampValue(LChangedValue, ggbrAmount.Min, ggbrAmount.Max);

    ggbrAmount.Position := LChangedValue;
    edtAmount.Text      := IntToStr(ggbrAmount.Position);
  except
    edtAmount.Text := IntToStr(ggbrAmount.Position);
  end;
end;

procedure TfrmSolarize.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SOLARIZE_AMOUNT, ggbrAmount.Position);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSolarize.FormClose(Sender: TObject;
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

end.
