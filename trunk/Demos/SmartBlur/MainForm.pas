unit MainForm;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls,
{ Graphics32 }
  GR32_Image, GR32_RangeBars,
{ GraphicsMagic lib }
  gmSmartBlurFilter;  

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    lblRadius: TLabel;
    ggbrRadius: TGaugeBar;
    lblThreshold: TLabel;
    ggbrThreshold: TGaugeBar;
    btnOpenImage: TButton;
    pnlOriginal: TPanel;
    Panel3: TPanel;
    imgvwOriginal: TImgView32;
    Splitter1: TSplitter;
    pnlResult: TPanel;
    Panel2: TPanel;
    imgvwResult: TImgView32;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrThresholdChange(Sender: TObject);
    procedure ggbrThresholdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOpenImageClick(Sender: TObject);
  private
    FSmartBlur : TgmSmartBlurFilter;

    procedure ApplySmartBlur;
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

procedure TfrmMain.ApplySmartBlur;
begin
  Screen.Cursor := crHourGlass;
  try
    imgvwResult.Bitmap.BeginUpdate;
    imgvwResult.Bitmap.Assign(imgvwOriginal.Bitmap);
    FSmartBlur.Filter(imgvwResult.Bitmap);
    imgvwResult.Bitmap.EndUpdate;
    imgvwResult.Bitmap.Changed;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSmartBlur := TgmSmartBlurFilter.Create;
  ggbrRadius.Position := Round(FSmartBlur.Radius * 10);
  ggbrThreshold.Position := Round(FSmartBlur.Threshold * 10);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSmartBlur.Free;
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  lblRadius.Caption := Format('Radius: %.1f', [ggbrRadius.Position / 10]);
end;

procedure TfrmMain.ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSmartBlur.Radius := ggbrRadius.Position / 10;
  ApplySmartBlur;
end;

procedure TfrmMain.ggbrThresholdChange(Sender: TObject);
begin
  lblThreshold.Caption := Format('Threshold: %.1f', [ggbrThreshold.Position / 10]);
end;

procedure TfrmMain.ggbrThresholdMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSmartBlur.Threshold := ggbrThreshold.Position / 10;
  ApplySmartBlur;
end;

procedure TfrmMain.btnOpenImageClick(Sender: TObject);
var
  LGraphic : TGraphic;
begin
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    LGraphic := TJPEGImage.Create;
    try
      LGraphic.LoadFromFile(OpenPictureDialog.FileName);
      imgvwOriginal.Bitmap.Assign(LGraphic);
      ApplySmartBlur;

      lblRadius.Enabled     := True;
      ggbrRadius.Enabled    := True;
      lblThreshold.Enabled  := True;
      ggbrThreshold.Enabled := True;
    finally
      LGraphic.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.
