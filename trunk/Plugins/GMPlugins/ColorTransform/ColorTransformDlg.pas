{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit ColorTransformDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32, GR32_RangeBars;

type
  TUpdateViewProc = procedure;

  TfrmColorTransform = class(TForm)
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnReset: TButton;
    grpbxColorTransform: TGroupBox;
    lblRedToGreen: TLabel;
    Label1: TLabel;
    lblRedToBlue: TLabel;
    Label2: TLabel;
    lblGreenToRed: TLabel;
    Label3: TLabel;
    lblGreenToBlue: TLabel;
    Label4: TLabel;
    lblBlueToRed: TLabel;
    Label5: TLabel;
    lblBlueToGreen: TLabel;
    Label6: TLabel;
    edtRedToGreen: TEdit;
    edtRedToBlue: TEdit;
    edtGreenToRed: TEdit;
    edtGreenToBlue: TEdit;
    edtBlueToRed: TEdit;
    edtBlueToGreen: TEdit;
    ggbrRedToGreen: TGaugeBar;
    ggbrRedToBlue: TGaugeBar;
    ggbrGreenToRed: TGaugeBar;
    ggbrGreenToBlue: TGaugeBar;
    ggbrBlueToRed: TGaugeBar;
    ggbrBlueToGreen: TGaugeBar;
    btnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
    procedure GaugeBarChange(Sender: TObject);
    procedure GaugeBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    FAllowChange              : Boolean;
    FRToGPercent, FRToBPercent: Double;
    FGToRPercent, FGToBPercent: Double;
    FBToRPercent, FBToGPercent: Double;

    procedure ExecuteColorTransform;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FUpdateViewProc: TUpdateViewProc;
  end;

var
  frmColorTransform: TfrmColorTransform;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmColorTransform, gmPluginFuncs;

{$R *.dfm}

const
  INI_SECTION                  = 'ColorTransformSettings';
  INI_IDENT_RED2GREEN_PERCENT  = 'Red2Green';
  INI_IDENT_RED2BLUE_PERCENT   = 'Red2Blue';
  INI_IDENT_GREEN2RED_PERCENT  = 'Green2Red';
  INI_IDENT_GREEN2BLUE_PERCENT = 'Green2Blue';
  INI_IDENT_BLUE2RED_PERCENT   = 'Blue2Red';
  INI_IDENT_BLUE2GREEN_PERCENT = 'Blue2Green';
  INI_IDENT_PREVIEW            = 'Preview';

procedure TfrmColorTransform.ExecuteColorTransform;
begin
  Screen.Cursor := crHourGlass;
  try
    ColorTransform32(FSourceBmp, FProcessedBmp,
                     FRToGPercent, FRToBPercent, FGToRPercent,
                     FGToBPercent, FBToRPercent, FBToGPercent);

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

procedure TfrmColorTransform.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
end;

procedure TfrmColorTransform.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmColorTransform.FormShow(Sender: TObject);
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
    FRToGPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_RED2GREEN_PERCENT,  0.0);
    FRToBPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_RED2BLUE_PERCENT,   0.0);
    FGToRPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_GREEN2RED_PERCENT,  0.0);
    FGToBPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_GREEN2BLUE_PERCENT, 0.0);
    FBToRPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_BLUE2RED_PERCENT,   0.0);
    FBToGPercent          := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_BLUE2GREEN_PERCENT, 0.0);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrRedToGreen.Position  := Round(FRToGPercent * 100);
  ggbrRedToBlue.Position   := Round(FRToBPercent * 100);
  ggbrGreenToRed.Position  := Round(FGToRPercent * 100);
  ggbrGreenToBlue.Position := Round(FGToBPercent * 100);
  ggbrBlueToRed.Position   := Round(FBToRPercent * 100);
  ggbrBlueToGreen.Position := Round(FBToGPercent * 100);

  FProcessedBmp.SetSize(FSourceBmp.Width, FSourceBmp.Height);

  ExecuteColorTransform;
  ActiveControl := btbtnOK;
end;

procedure TfrmColorTransform.FormClose(Sender: TObject;
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

procedure TfrmColorTransform.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_RED2GREEN_PERCENT,  FRToGPercent);
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_RED2BLUE_PERCENT,   FRToBPercent);
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_GREEN2RED_PERCENT,  FGToRPercent);
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_GREEN2BLUE_PERCENT, FGToBPercent);
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_BLUE2RED_PERCENT,   FBToRPercent);
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_BLUE2GREEN_PERCENT, FBToGPercent);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmColorTransform.GaugeBarChange(Sender: TObject);
begin
  if Sender = ggbrRedToGreen then
  begin
    edtRedToGreen.Text := IntToStr(ggbrRedToGreen.Position);
    FRToGPercent       := ggbrRedToGreen.Position / 100;
  end
  else if Sender = ggbrRedToBlue then
  begin
    edtRedToBlue.Text := IntToStr(ggbrRedToBlue.Position);
    FRToBPercent      := ggbrRedToBlue.Position / 100;
  end
  else if Sender = ggbrGreenToRed then
  begin
    edtGreenToRed.Text := IntToStr(ggbrGreenToRed.Position);
    FGToRPercent       := ggbrGreenToRed.Position / 100;
  end
  else if Sender = ggbrGreenToBlue then
  begin
    edtGreenToBlue.Text := IntToStr(ggbrGreenToBlue.Position);
    FGToBPercent        := ggbrGreenToBlue.Position / 100;
  end
  else if Sender = ggbrBlueToRed then
  begin
    edtBlueToRed.Text := IntToStr(ggbrBlueToRed.Position);
    FBToRPercent      := ggbrBlueToRed.Position / 100;
  end
  else if Sender = ggbrBlueToGreen then
  begin
    edtBlueToGreen.Text := IntToStr(ggbrBlueToGreen.Position);
    FBToGPercent        := ggbrBlueToGreen.Position / 100;
  end;

  if FAllowChange then
  begin
    ExecuteColorTransform;
  end;
end;

procedure TfrmColorTransform.GaugeBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmColorTransform.GaugeBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteColorTransform;
  FAllowChange := True;
end;

procedure TfrmColorTransform.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmColorTransform.EditChange(Sender: TObject);
var
  LChangedValue: Integer;
  LTempEdit    : TEdit;
  LTempBar     : TGaugeBar;
begin
  LTempEdit := nil;
  LTempBar  := nil;

  if Sender = edtRedToGreen then
  begin
    LTempEdit := edtRedToGreen;
    LTempBar  := ggbrRedToGreen;
  end
  else if Sender = edtRedToBlue then
  begin
    LTempEdit := edtRedToBlue;
    LTempBar  := ggbrRedToBlue;
  end
  else if Sender = edtGreenToRed then
  begin
    LTempEdit := edtGreenToRed;
    LTempBar  := ggbrGreenToRed;
  end
  else if Sender = edtGreenToBlue then
  begin
    LTempEdit := edtGreenToBlue;
    LTempBar  := ggbrGreenToBlue;
  end
  else if Sender = edtBlueToRed then
  begin
    LTempEdit := edtBlueToRed;
    LTempBar  := ggbrBlueToRed;
  end
  else if Sender = edtBlueToGreen then
  begin
    LTempEdit := edtBlueToGreen;
    LTempBar  := ggbrBlueToGreen;
  end;

  if Assigned(LTempEdit) and Assigned(LTempBar) then
  begin
    try
      LChangedValue := StrToInt(LTempEdit.Text);
      ClampValue(LChangedValue, LTempBar.Min, LTempBar.Max);

      LTempBar.Position := LChangedValue;
      LTempEdit.Text    := IntToStr(LTempBar.Position);
    except
      LTempEdit.Text := IntToStr(LTempBar.Position);
    end;
  end;
end;

procedure TfrmColorTransform.btnResetClick(Sender: TObject);
begin
  FAllowChange := False;
  try
    ggbrRedToGreen.Position  := 0;
    ggbrRedToBlue.Position   := 0;
    ggbrGreenToRed.Position  := 0;
    ggbrGreenToBlue.Position := 0;
    ggbrBlueToRed.Position   := 0;
    ggbrBlueToGreen.Position := 0;

    ExecuteColorTransform;
  finally
    FAllowChange := True;
  end;
end;

procedure TfrmColorTransform.btnAboutClick(Sender: TObject);
begin
  MessageDlg('Copyright (c) Jean Yves Queinec', mtInformation, [mbOK], 0);
end; 

end.
