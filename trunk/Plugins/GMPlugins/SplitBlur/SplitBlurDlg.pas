{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SplitBlurDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmSplitBlur = class(TForm)
    GroupBox1: TGroupBox;
    lblRadius: TLabel;
    edtRadius: TEdit;
    ggbrRadius: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    Label1: TLabel;
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
  private
    FAllowChange: Boolean;
    
    procedure ExecuteSplitBlur;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmSplitBlur: TfrmSplitBlur;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmSplitBlur, gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION                 = 'SplitBlurSettings';
  INI_IDENT_SPLIT_BLUR_RADIUS = 'Radius';
  INI_IDENT_PREVIEW           = 'Preview';

procedure TfrmSplitBlur.ExecuteSplitBlur;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    SplitBlur32(FProcessedBmp, ggbrRadius.Position);

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

procedure TfrmSplitBlur.FormCreate(Sender: TObject);
begin
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmSplitBlur.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSplitBlur.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LRadius  : Integer;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LRadius               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SPLIT_BLUR_RADIUS, 0);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrRadius.Position := LRadius;
  
  ExecuteSplitBlur;
end;

procedure TfrmSplitBlur.FormClose(Sender: TObject;
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

procedure TfrmSplitBlur.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SPLIT_BLUR_RADIUS, ggbrRadius.Position);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmSplitBlur.ggbrRadiusChange(Sender: TObject);
begin
  edtRadius.Text := IntToStr(ggbrRadius.Position);

  if FAllowChange then
  begin
    ExecuteSplitBlur;
  end;
end;

procedure TfrmSplitBlur.ggbrRadiusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmSplitBlur.ggbrRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSplitBlur;
  FAllowChange := True;
end;

procedure TfrmSplitBlur.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSplitBlur.edtRadiusChange(Sender: TObject);
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

end.
