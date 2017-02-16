{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit SepiaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32, GR32_RangeBars;

type
  TUpdateViewProc = procedure;

  TfrmSepia = class(TForm)
    GroupBox1: TGroupBox;
    lblDepth: TLabel;
    lblLevels: TLabel;
    edtDepth: TEdit;
    ggbrDepth: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnAboutSepia: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ggbrDepthChange(Sender: TObject);
    procedure ggbrDepthMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtDepthChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ggbrDepthMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnAboutSepiaClick(Sender: TObject);
  private
    FAllowChange: Boolean;
    
    procedure ExecuteSepia;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
  end;

var
  frmSepia: TfrmSepia;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmSepia, gmPluginFuncs;

const
  INI_SECTION           = 'SepiaSettings';
  INI_IDENT_SEPIA_DEPTH = 'Depth';
  INI_IDENT_PREVIEW     = 'Preview';

{$R *.dfm}

procedure TfrmSepia.ExecuteSepia;
begin
  Screen.Cursor := crHourGlass;
  try
    FProcessedBmp.Assign(FSourceBmp);
    Sepia32(FProcessedBmp, ggbrDepth.Position);

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

procedure TfrmSepia.FormCreate(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;
  FAllowChange    := True;
  FSourceBmp      := TBitmap32.Create;
  FProcessedBmp   := TBitmap32.Create;
end;

procedure TfrmSepia.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp.Free;
  FProcessedBmp.Free;
end;

procedure TfrmSepia.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LDepth   : Byte;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LDepth                := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_SEPIA_DEPTH, 34);
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrDepth.Position := LDepth;
  edtDepth.Text      := IntToStr(ggbrDepth.Position);
  
  ExecuteSepia;
end; 

procedure TfrmSepia.ggbrDepthChange(Sender: TObject);
begin
  edtDepth.Text := IntToStr(ggbrDepth.Position);

  if FAllowChange then
  begin
    ExecuteSepia;
  end;
end; 

procedure TfrmSepia.ggbrDepthMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ExecuteSepia;
  FAllowChange := True;
end; 

procedure TfrmSepia.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmSepia.edtDepthChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtDepth.Text);
    ClampValue(LChangedValue, ggbrDepth.Min, ggbrDepth.Max);

    ggbrDepth.Position := LChangedValue;
    edtDepth.Text      := IntToStr(ggbrDepth.Position);
  except
    edtDepth.Text := IntToStr(ggbrDepth.Position);
  end;
end; 

procedure TfrmSepia.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_SEPIA_DEPTH, ggbrDepth.Position);
  finally
    LIniFile.Free;
  end;
end; 

procedure TfrmSepia.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TfrmSepia.ggbrDepthMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmSepia.btnAboutSepiaClick(Sender: TObject);
begin
  MessageDlg('Original author: Daniel Lopes' + #10#13 +
             'Accelerated by: Gerd Platl', mtInformation, [mbOK], 0);
end; 

end.
