{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit MosaicDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32, GR32_RangeBars, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmMosaic = class(TForm)
    GroupBox1: TGroupBox;
    lblSize: TLabel;
    edtSize: TEdit;
    lblPixels: TLabel;
    ggbrSize: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ggbrSizeChange(Sender: TObject);
    procedure ggbrSizeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtSizeChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ggbrSizeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteMosaic;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmMosaic: TfrmMosaic;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmMosaic, gmPluginFuncs, gmImageProcessFuncs;

const
  INI_SECTION           = 'MosaicSettings';
  INI_IDENT_MOSAIC_SIZE = 'Size';
  INI_IDENT_PREVIEW     = 'Preview';

{$R *.dfm}

procedure TfrmMosaic.ExecuteMosaic;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    Mosaic32(FProcessedBmp, ggbrSize.Position);

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

procedure TfrmMosaic.FormCreate(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
end;

procedure TfrmMosaic.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp.Free;
  FProcessedBmp.Free;
end;

procedure TfrmMosaic.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LSize    : Integer;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LSize                 := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_MOSAIC_SIZE, 15);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrSize.Position := LSize;
  edtSize.Text      := IntToStr(ggbrSize.Position);
  
  ExecuteMosaic;
end;

procedure TfrmMosaic.ggbrSizeChange(Sender: TObject);
begin
  edtSize.Text := IntToStr(ggbrSize.Position);

  if FAllowChange then
  begin
    ExecuteMosaic;
  end;
end;

procedure TfrmMosaic.ggbrSizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ExecuteMosaic;
  FAllowChange := True;
end;

procedure TfrmMosaic.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmMosaic.edtSizeChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtSize.Text);
    ClampValue(LChangedValue, ggbrSize.Min, ggbrSize.Max);

    ggbrSize.Position := LChangedValue;
    edtSize.Text      := IntToStr(ggbrSize.Position);
  except
    edtSize.Text := IntToStr(ggbrSize.Position);
  end;
end;

procedure TfrmMosaic.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_MOSAIC_SIZE, ggbrSize.Position);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmMosaic.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmMosaic.ggbrSizeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

end.
