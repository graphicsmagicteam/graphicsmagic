{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SplitImageDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmSplitImage, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmSplitImage = class(TForm)
    GroupBox1: TGroupBox;
    lblAmount: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    GroupBox2: TGroupBox;
    rdbtnRoundSplit: TRadioButton;
    rdbtnWasteSplit: TRadioButton;
    Label1: TLabel;
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
    procedure ChangeSplitType(Sender: TObject);
  private
    FAllowChange: Boolean;
    FSplitType  : TgmSplitImageType;
    FMaxAmount  : Integer;
    
    procedure ExecuteSplitImage;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FBKColor       : TColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmSplitImage: TfrmSplitImage;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION                  = 'SplitImageSettings';
  INI_IDENT_SPLIT_IMAGE_AMOUNT = 'Amount';
  INI_IDENT_SPLIT_TYPE         = 'SplitType';
  INI_IDENT_PREVIEW            = 'Preview';
  

procedure TfrmSplitImage.ExecuteSplitImage;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    SplitImage32(FSourceBmp, FProcessedBmp, ggbrAmount.Position, FSplitType, FBKColor);

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

procedure TfrmSplitImage.FormCreate(Sender: TObject);
begin
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmSplitImage.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSplitImage.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LAmount  : Integer;
begin
  FMaxAmount := FSourceBmp.Width div 2;

  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LAmount               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SPLIT_IMAGE_AMOUNT, 0);
    FSplitType            := TgmSplitImageType( LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SPLIT_TYPE, 0) );
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  if LAmount > FMaxAmount then
  begin
    LAmount := 0;
  end;

  ggbrAmount.Position     := LAmount;
  ggbrAmount.Max          := FMaxAmount;
  rdbtnRoundSplit.Checked := (FSplitType = sitRound);
  rdbtnWasteSplit.Checked := (FSplitType = sitWaste);

  ExecuteSplitImage;
end;

procedure TfrmSplitImage.FormClose(Sender: TObject;
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

procedure TfrmSplitImage.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SPLIT_IMAGE_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SPLIT_TYPE, Ord(FSplitType));
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSplitImage.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);

  if FAllowChange then
  begin
    ExecuteSplitImage;
  end;
end;

procedure TfrmSplitImage.ggbrAmountMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmSplitImage.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSplitImage;
  FAllowChange := True;
end;

procedure TfrmSplitImage.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSplitImage.edtAmountChange(Sender: TObject);
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

procedure TfrmSplitImage.ChangeSplitType(Sender: TObject);
begin
  if rdbtnRoundSplit.Checked then
  begin
    FSplitType := sitRound;
  end
  else if rdbtnWasteSplit.Checked then
  begin
    FSplitType := sitWaste;
  end;

  ExecuteSplitImage;
end; 

end.
