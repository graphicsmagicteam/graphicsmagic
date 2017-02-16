{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

unit DiagonalDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GR32_RangeBars, GR32, gmDiagonal, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmDiagonal = class(TForm)
    GroupBox1: TGroupBox;
    lblAmount: TLabel;
    Label1: TLabel;
    edtAmount: TEdit;
    ggbrAmount: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    GroupBox2: TGroupBox;
    rdbtnLeftDiag: TRadioButton;
    rdbtnRightDiag: TRadioButton;
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
    procedure ChangeDiagonalDirection(Sender: TObject);
  private
    FAllowChange  : Boolean;
    FDiagDirection: TgmDiagonalDirection;

    procedure ExecuteDiagonal;
  public
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FBKColor       : TColor32;
    FUpdateViewProc: TUpdateViewProc;
    FChannelSet    : TgmChannelSet;
  end;

var
  frmDiagonal: TfrmDiagonal;

implementation

uses
{ Standard }
  IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs;

{$R *.dfm}

const
  INI_SECTION                  = 'DiagonalSettings';
  INI_IDENT_DIAGONAL_AMOUNT    = 'Amount';
  INI_IDENT_DIAGONAL_DIRECTION = 'DiagonalDirection';
  INI_IDENT_PREVIEW            = 'Preview';

procedure TfrmDiagonal.ExecuteDiagonal;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Diagonal32(FSourceBmp, FProcessedBmp, FBKColor, ggbrAmount.Position, FDiagDirection);

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

procedure TfrmDiagonal.FormCreate(Sender: TObject);
begin
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
  FAllowChange    := True;
  FChannelSet     := [csRed, csGreen, csBlue];
end;

procedure TfrmDiagonal.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FProcessedBmp.Free;

  FDestBmpPtr     := nil;
  FUpdateViewProc := nil;
end;

procedure TfrmDiagonal.FormShow(Sender: TObject);
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
    LAmount               := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_DIAGONAL_AMOUNT, 0);
    FDiagDirection        := TgmDiagonalDirection( LIniFile.ReadInteger(INI_SECTION, INI_IDENT_DIAGONAL_DIRECTION, 0) );
    chckbxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  ggbrAmount.Position    := LAmount;
  rdbtnLeftDiag.Checked  := (FDiagDirection = ddLeftDiag);
  rdbtnRightDiag.Checked := (FDiagDirection = ddRightDiag);

  ExecuteDiagonal;
end;

procedure TfrmDiagonal.FormClose(Sender: TObject;
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

procedure TfrmDiagonal.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_DIAGONAL_AMOUNT, ggbrAmount.Position);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_DIAGONAL_DIRECTION, Ord(FDiagDirection));
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmDiagonal.ggbrAmountChange(Sender: TObject);
begin
  edtAmount.Text := IntToStr(ggbrAmount.Position);

  if FAllowChange then
  begin
    ExecuteDiagonal;
  end;
end;

procedure TfrmDiagonal.ggbrAmountMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmDiagonal.ggbrAmountMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteDiagonal;
  FAllowChange := True;
end;

procedure TfrmDiagonal.chckbxPreviewClick(Sender: TObject);
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

procedure TfrmDiagonal.edtAmountChange(Sender: TObject);
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

procedure TfrmDiagonal.ChangeDiagonalDirection(Sender: TObject);
begin
  if rdbtnLeftDiag.Checked then
  begin
    FDiagDirection := ddLeftDiag;
  end
  else if rdbtnRightDiag.Checked then
  begin
    FDiagDirection := ddRightDiag;
  end;

  ExecuteDiagonal;
end; 

end.
