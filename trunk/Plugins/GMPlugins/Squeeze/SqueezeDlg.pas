{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SqueezeDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmSqueeze = class(TForm)
    GroupBox1: TGroupBox;
    lblAmount: TLabel;
    lblPixels: TLabel;
    lblSqueezeStyle: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    cmbbxSqueezeStyle: TComboBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
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
    procedure ChangeSqueezeStyle(Sender: TObject);
  private
    FAllowChange: Boolean;
    FMaxAmount  : Integer;
    
    procedure ExecuteSqueeze;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FBKColor       : TColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmSqueeze: TfrmSqueeze;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmSqueeze, gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION                   = 'SqueezeSettings';
  INI_IDENT_SQUEEZE_AMOUNT      = 'Amount';
  INI_IDENT_SQUEEZE_STYLE_INDEX = 'SqueezeStyleIndex';
  INI_IDENT_PREVIEW             = 'Preview';

procedure TfrmSqueeze.ExecuteSqueeze;
var
  LSqueezeStyle     : TgmSqueezeStyle;
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    LSqueezeStyle := TgmSqueezeStyle(cmbbxSqueezeStyle.ItemIndex);
    
    Squeeze32(FSourceBmp, FProcessedBmp, ggbrAmount.Position, LSqueezeStyle, FBKColor);

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

procedure TfrmSqueeze.FormCreate(Sender: TObject);
begin
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmSqueeze.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSqueeze.FormShow(Sender: TObject);
var
  LIniFile          : TIniFile;
  LDLLName          : array [0..255] of Char;
  LFileName         : string;
  LAmount           : Integer;
  LSqueezeStyleIndex: Integer;
begin
  FMaxAmount := FSourceBmp.Width div 2;

  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LAmount               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SQUEEZE_AMOUNT, 0);
    LSqueezeStyleIndex    := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SQUEEZE_STYLE_INDEX, 0);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  if LSqueezeStyleIndex < 0 then
  begin
    LSqueezeStyleIndex := 0;
  end
  else if LSqueezeStyleIndex > 6 then
  begin
    LSqueezeStyleIndex := 6;
  end;

  if LAmount > FMaxAmount then
  begin
    LAmount := 0;
  end;

  ggbrAmount.Max              := FMaxAmount;
  ggbrAmount.Position         := LAmount;
  cmbbxSqueezeStyle.Items     := GetSqueezeStyles;
  cmbbxSqueezeStyle.ItemIndex := LSqueezeStyleIndex;
  
  ExecuteSqueeze;
end;

procedure TfrmSqueeze.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmSqueeze.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SQUEEZE_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SQUEEZE_STYLE_INDEX, cmbbxSqueezeStyle.ItemIndex);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSqueeze.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);

  if FAllowChange then
  begin
    ExecuteSqueeze;
  end;
end;

procedure TfrmSqueeze.ggbrAmountMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmSqueeze.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSqueeze;
  FAllowChange := True;
end;

procedure TfrmSqueeze.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSqueeze.edtAmountChange(Sender: TObject);
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

procedure TfrmSqueeze.ChangeSqueezeStyle(Sender: TObject);
begin
  ExecuteSqueeze;
end;

end.
