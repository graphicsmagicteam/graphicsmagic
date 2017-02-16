{ This is a plug-in filter specifically designed for GraphicsMagic.
  Written by: Gerd Platl

  release 2009-01-22 }

unit HueCirclingDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, IniFiles, GR32, GR32_RangeBars, GR32_Image;

type
  TUpdateViewProc = procedure;

  THueCirclingForm = class(TForm)
    GroupBox1: TGroupBox;
    lblDepth: TLabel;
    lblDegrees: TLabel;
    editDegrees: TEdit;
    gbDegrees: TGaugeBar;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    checkboxPreview: TCheckBox;
    AboutButton: TButton;
    PreviewImage: TImage32;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AboutButtonClick(Sender: TObject);
    procedure checkboxPreviewClick(Sender: TObject);
    procedure editDegreesChange(Sender: TObject);
    procedure gbDegreesChange(Sender: TObject);
    procedure gbDegreesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbDegreesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FAllowChange: Boolean;

    procedure ExecuteHueCircling;
  public
    FFileName      : string;
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FHSVBitmap     : TBitmap32;              // original hsv bitmap
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
  end;

var
  HueCirclingForm: THueCirclingForm;

implementation

uses
  gmHueCircling, gmPluginFuncs;

const
  INI_SECTION       = 'HueCirclingSettings';
  INI_IDENT_PREVIEW = 'Preview';

{$R *.dfm}

procedure THueCirclingForm.ExecuteHueCircling;
var
  LHueByte: byte;
begin
  Screen.Cursor := crHourGlass;
  try
    LHueByte           := Windows.MulDiv (gbDegrees.Position, 255, 359);
    PreviewImage.Color := WinColor (HSLtoRGB (LHueByte, 127, 127));   // convert pixel color format

    FProcessedBmp.Assign(FHSVBitmap);
    HueCircling32(FProcessedBmp, LHueByte);

    if checkboxPreview.Checked then
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

procedure THueCirclingForm.FormCreate(Sender: TObject);
var
  LDLLName : array [0..255] of Char;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  FFileName := LDLLName;
  FFileName := ChangeFileExt(FFileName, '.ini');

  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;
  FAllowChange    := True;

  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
  FHSVBitmap    := TBitmap32.Create;
end;

procedure THueCirclingForm.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp.Free;
  FProcessedBmp.Free;
  FHSVBitmap.Free;
end; 

procedure THueCirclingForm.FormShow(Sender: TObject);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(FFileName);
  try
    checkboxPreview.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_PREVIEW, True);
  finally
    LIniFile.Free;
  end;

  editDegrees.Text := IntToStr(gbDegrees.Position);

  BitmapRGBtoHSL(FSourceBmp, FHSVBitmap);   // convert picture to HSL pixel format

  ExecuteHueCircling;
end;

procedure THueCirclingForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(FFileName);
  try
    LIniFile.WriteBool(INI_SECTION, INI_IDENT_PREVIEW, checkboxPreview.Checked);
  finally
    LIniFile.Free;
  end;
end;

procedure THueCirclingForm.AboutButtonClick(Sender: TObject);
begin
  MessageDlg('This plugin changes the hue value of each pixel.'#13#10
             +'Author: Gerd Platl', mtInformation, [mbOK], 0);
end;

procedure THueCirclingForm.checkboxPreviewClick(Sender: TObject);
begin
  if checkboxPreview.Checked then
  begin
    CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
  end;
end;

procedure THueCirclingForm.editDegreesChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(editDegrees.Text);
    ClampValue(LChangedValue, gbDegrees.Min, gbDegrees.Max);

    gbDegrees.Position := LChangedValue;
    editDegrees.Text   := IntToStr(gbDegrees.Position);
  except
    editDegrees.Text := IntToStr(gbDegrees.Position);
  end;
end;

procedure THueCirclingForm.gbDegreesChange(Sender: TObject);
var
  LHueByte: Byte;
begin
  editDegrees.Text   := IntToStr(gbDegrees.Position);
  LHueByte           := Windows.MulDiv(gbDegrees.Position, 255, 359);
  PreviewImage.Color := WinColor(HSLtoRGB(LHueByte, 255, 127));

  if FAllowChange then
  begin
    ExecuteHueCircling;
  end;
end;

procedure THueCirclingForm.gbDegreesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure THueCirclingForm.gbDegreesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ExecuteHueCircling;
  FAllowChange := True;
end; 

end.
