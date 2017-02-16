unit MainForm;

// Author: Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
// Update Date: 2015/04/03

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ExtDlgs, StdCtrls,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars,
{ GraphicsMagicLib }
  gmJHSmartBlurFilter;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    lblRadius: TLabel;
    ggbrRadius: TGaugeBar;
    lblThreshold: TLabel;
    ggbrThreshold: TGaugeBar;
    pnlOriginal: TPanel;
    pnlOriginalHeader: TPanel;
    imgOriginal: TImgView32;
    Splitter1: TSplitter;
    pnlResult: TPanel;
    pnlResultHeader: TPanel;
    imgResult: TImgView32;
    btnLoadImage: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure ggbrRadiusChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrThresholdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FFilter : TgmJHSmartBlurFilter;

    procedure ApplyFilter;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG;

{$R *.dfm}

procedure TfrmMain.ApplyFilter;
var
  LStart : Cardinal;
  LEnd   : Cardinal;
begin
  if Assigned(FFilter) then
  begin
    LStart := GetTickCount();
    FFilter.Execute(imgOriginal.Bitmap, imgResult.Bitmap);
    LEnd := GetTickCount();

    imgResult.Bitmap.Changed();
    StatusBar.Panels[0].Text := Format('Process Time: %f s', [ (LEnd - LStart) / 1000 ]);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFilter := TgmJHSmartBlurFilter.Create();
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFilter.Free();
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  FFilter.Radius    := ggbrRadius.Position / 100;
  lblRadius.Caption := Format('Radius: %.1f', [FFilter.Radius]);
end;

procedure TfrmMain.ggbrThresholdChange(Sender: TObject);
begin
  FFilter.Threshold    := ggbrThreshold.Position;
  lblThreshold.Caption := 'Threshold: ' + IntToStr(FFilter.Threshold);
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'JPEG Image|*.jpg';

  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      imgOriginal.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
      imgResult.Bitmap.SetSizeFrom(imgOriginal.Bitmap);

      ApplyFilter();

      lblZoom.Enabled     := True;
      cmbbxZoom.Enabled   := True;
      cmbbxZoom.ItemIndex := 3;
      cmbbxZoomChange(nil);

      lblRadius.Enabled  := True;
      ggbrRadius.Enabled := True;
      
      lblThreshold.Enabled  := True;
      ggbrThreshold.Enabled := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgOriginal.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
  imgResult.Scale   := imgOriginal.Scale;
end;

procedure TfrmMain.ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    ApplyFilter();
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.ggbrThresholdMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    ApplyFilter();
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
