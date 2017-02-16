{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SplitColorDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32;

type
  TUpdateViewProc = procedure;

  TfrmSplitColor = class(TForm)
    GroupBox1: TGroupBox;
    lblLightness: TLabel;
    Label1: TLabel;
    edtLightness: TEdit;
    ggbrLightness: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    cmbbxColorChannel: TComboBox;
    lblColorChannel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrLightnessChange(Sender: TObject);
    procedure ggbrLightnessMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrLightnessMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtLightnessChange(Sender: TObject);
    procedure edtLightnessExit(Sender: TObject);
    procedure cmbbxColorChannelChange(Sender: TObject);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteSplitColor;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FUpdateViewProc: TUpdateViewProc;
  end;

var
  frmSplitColor: TfrmSplitColor;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmSplitColor, gmPluginFuncs;

{$R *.dfm}

const
  SPLIT_COLOR_MID: Integer = 100;

  INI_SECTION                     = 'SplitColorSettings';
  INI_IDENT_SPLIT_Color_LIGHTNESS = 'Lightness';
  INI_IDENT_SPLIT_CHANNEL_INDEX   = 'ChannelIndex';
  INI_IDENT_PREVIEW               = 'Preview';
  

procedure TfrmSplitColor.ExecuteSplitColor;
var
  LLightnessLevels: Integer;
  LColorChannel   : TgmSplitColorChannel;
begin
  Screen.Cursor := crHourGlass;
  try
    LLightnessLevels := ggbrLightness.Position - SPLIT_COLOR_MID;
    LColorChannel    := TgmSplitColorChannel(cmbbxColorChannel.ItemIndex);

    FProcessedBmp.Assign(FSourceBmp);
    SplitColor32(FProcessedBmp, LLightnessLevels, LColorChannel);

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

procedure TfrmSplitColor.FormCreate(Sender: TObject);
begin
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
end;

procedure TfrmSplitColor.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmSplitColor.FormShow(Sender: TObject);
var
  LIniFile     : TIniFile;
  LDLLName     : array [0..255] of Char;
  LFileName    : string;
  LLightness   : Integer;
  LColorChannel: Byte;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LLightness            := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SPLIT_COLOR_LIGHTNESS, SPLIT_COLOR_MID);
    LColorChannel         := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SPLIT_CHANNEL_INDEX, 0);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  if LColorChannel > 5 then
  begin
    LColorChannel := 5;
  end;

  ggbrLightness.Position      := LLightness;
  cmbbxColorChannel.ItemIndex := LColorChannel;
  
  ExecuteSplitColor;
end;

procedure TfrmSplitColor.FormClose(Sender: TObject;
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

procedure TfrmSplitColor.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SPLIT_COLOR_LIGHTNESS, ggbrLIGHTNESS.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SPLIT_CHANNEL_INDEX, cmbbxColorChannel.ItemIndex);
  finally
    LIniFile.Free;
  end;
end; 

procedure TfrmSplitColor.ggbrLightnessChange(Sender: TObject);
var
  LLightness: Integer;
begin
  LLightness := ggbrLightness.Position - SPLIT_COLOR_MID;

  if LLightness > 0 then
  begin
    edtLightness.Text := '+' + IntToStr(LLightness);
  end
  else
  begin
    edtLightness.Text := IntToStr(LLightness);
  end;

  if FAllowChange then
  begin
    ExecuteSplitColor;
  end;
end; 

procedure TfrmSplitColor.ggbrLightnessMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmSplitColor.ggbrLightnessMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSplitColor;
  FAllowChange := True;
end;

procedure TfrmSplitColor.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSplitColor.edtLightnessChange(Sender: TObject);
var
  LChangedValue: Integer;
  LShowValue   : Integer;
begin
  try
    LChangedValue := StrToInt(edtLightness.Text) + SPLIT_COLOR_MID;
    ClampValue(LChangedValue, ggbrLightness.Min, ggbrLightness.Max);

    ggbrLightness.Position := LChangedValue;
  except
    LShowValue := ggbrLightness.Position - SPLIT_COLOR_MID;

    if LShowValue > 0 then
    begin
      edtLightness.Text := '+' + IntToStr(LShowValue);
    end
    else
    begin
      edtLightness.Text := IntToStr(LShowValue);
    end;
  end;
end;

procedure TfrmSplitColor.edtLightnessExit(Sender: TObject);
var
  LShowValue: Integer;
begin
  LShowValue := ggbrLightness.Position - SPLIT_COLOR_MID;

  if LShowValue > 0 then
  begin
    edtLightness.Text := '+' + IntToStr(LShowValue);
  end
  else
  begin
    edtLightness.Text := IntToStr(LShowValue);
  end;
end;

procedure TfrmSplitColor.cmbbxColorChannelChange(Sender: TObject);
begin
  ExecuteSplitColor;
end; 

end.
