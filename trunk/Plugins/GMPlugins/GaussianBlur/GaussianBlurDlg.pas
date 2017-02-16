{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2009 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit GaussianBlurDlg;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_RangeBars,
{ GraphicsMagic }
  gmTypes, gmGimpGaussianBlur;

type
  TUpdateViewProc = procedure;

  TfrmGaussianBlur = class(TForm)
    GroupBox1: TGroupBox;
    lblRadius: TLabel;
    edtRadiusValue: TEdit;
    lblPixels: TLabel;
    ggbrRadiusValue: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure ggbrRadiusValueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrRadiusValueChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure edtRadiusValueChange(Sender: TObject);
    procedure ggbrRadiusValueMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FBlurFilter  : TgmGimpGaussianBlur;
    FAllowChange : Boolean;
    
    procedure ExecuteGBlur;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmGaussianBlur: TfrmGaussianBlur;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic }
  gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION                   = 'GaussianBlurSettings';
  INI_IDENT_GAUSSIANBLUR_RADIUS = 'Radius';
  INI_IDENT_PREVIEW             = 'Preview';

procedure TfrmGaussianBlur.ExecuteGBlur;
var
  LColorChannelCount : Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    
    FBlurFilter.HorizontalRadius := ggbrRadiusValue.Position;
    FBlurFilter.VerticalRadius   := ggbrRadiusValue.Position;
    FBlurFilter.Execute(FProcessedBmp);

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

procedure TfrmGaussianBlur.ggbrRadiusValueMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGBlur;
  FAllowChange := True;
end;

procedure TfrmGaussianBlur.FormShow(Sender: TObject);
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
    chckbxPreview.Checked    := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
    ggbrRadiusValue.Position := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_GAUSSIANBLUR_RADIUS, 1);
  finally
    LIniFile.Free;
  end;

  ExecuteGBlur;
end; 

procedure TfrmGaussianBlur.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmGaussianBlur.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
  FBlurFilter   := TgmGimpGaussianBlur.Create;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmGaussianBlur.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;
  FBlurFilter.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end; 

procedure TfrmGaussianBlur.ggbrRadiusValueChange(Sender: TObject);
begin
  edtRadiusValue.Text := IntToStr(ggbrRadiusValue.Position);

  if FAllowChange then
  begin
    ExecuteGBlur;
  end;
end;

procedure TfrmGaussianBlur.FormClose(Sender: TObject;
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

procedure TfrmGaussianBlur.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_GAUSSIANBLUR_RADIUS, ggbrRadiusValue.Position);
  finally
    LIniFile.Free;
  end;
end; 

procedure TfrmGaussianBlur.edtRadiusValueChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtRadiusValue.Text);
    ClampValue(LChangedValue, ggbrRadiusValue.Min, ggbrRadiusValue.Max);

    ggbrRadiusValue.Position := LChangedValue;
    edtRadiusValue.Text      := IntToStr(ggbrRadiusValue.Position);
  except
    edtRadiusValue.Text := IntToStr(ggbrRadiusValue.Position);
  end;
end; 

procedure TfrmGaussianBlur.ggbrRadiusValueMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

end.
