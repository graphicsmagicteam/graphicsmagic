// Author: Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
// Update Date: Feb. 16, 2015

unit MainForm;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtDlgs, ComCtrls, ExtCtrls,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic Lib }
  gmOilPaintingFilter, GR32_RangeBars;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    btnLoadImage: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    pnlOriginal: TPanel;
    imgOriginal: TImgView32;
    pnlOriginalHeader: TPanel;
    Splitter1: TSplitter;
    pnlResult: TPanel;
    pnlResultHeader: TPanel;
    imgResult: TImgView32;
    lblRadius: TLabel;
    lblLevel: TLabel;
    ggbrRadius: TGaugeBar;
    ggbrLevel: TGaugeBar;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure btnLoadImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrLevelChange(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    FFilter         : TgmOilPaintingFilter;
    FRadius         : Cardinal;
    FIntensityLevel : Byte;

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
    FFilter.Radius         := FRadius;
    FFilter.IntensityLevel := FIntensityLevel;

    LStart := GetTickCount();
    FFilter.Execute(imgResult.Bitmap);
    LEnd := GetTickCount();

    imgResult.Bitmap.Changed();
    StatusBar.Panels[0].Text := Format('Process Time: %f s', [ (LEnd - LStart) / 1000 ]);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFilter         := nil;
  FRadius         := 5;
  FIntensityLevel := 20;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FFilter) then
  begin
    FFilter.Free();
  end;
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

      if Assigned(FFilter) then
      begin
        FreeAndNil(FFilter);
      end;

      FFilter := TgmOilPaintingFilter.Create(imgOriginal.Bitmap);
      ApplyFilter();

      lblZoom.Enabled     := True;
      cmbbxZoom.Enabled   := True;
      cmbbxZoom.ItemIndex := 3;
      cmbbxZoomChange(nil);

      lblRadius.Enabled   := True;
      ggbrRadius.Enabled  := True;
      
      lblLevel.Enabled    := True;
      ggbrLevel.Enabled   := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  FRadius           := ggbrRadius.Position;
  lblRadius.Caption := Format('Radius: %d pixels', [FRadius]);
end;

procedure TfrmMain.ggbrLevelChange(Sender: TObject);
begin
  FIntensityLevel  := ggbrLevel.Position;
  lblLevel.Caption := Format('Intensity Level: %d', [FIntensityLevel]);
end;

procedure TfrmMain.GaugeBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    ApplyFilter();
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgOriginal.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
  imgResult.Scale   := imgOriginal.Scale;
end;

end.
