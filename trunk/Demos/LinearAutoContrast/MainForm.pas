unit MainForm;

// Author: Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
// Update Date: 2015/06/28

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ExtDlgs,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars,
{ GraphicsMagicLib }
  gmLinearAutoContrast;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    imgView: TImgView32;
    btnLoadImage: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    btnExecute: TButton;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    btnRestore: TButton;
    lblShadowsClip: TLabel;
    ggbrShadowsClip: TGaugeBar;
    lblHighlightsClip: TLabel;
    ggbrHighlightsClip: TGaugeBar;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrShadowsClipChange(Sender: TObject);
    procedure ggbrHighlightsClipChange(Sender: TObject);
  private
    { Private declarations }
    FOriginalBitmap : TBitmap32;
    FFilter         : TgmLinearAutoContrast;
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

const
  APP_CAPTION = 'Linear Auto Contrast';

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  LStart : Cardinal;
  LEnd   : Cardinal;
begin
  Screen.Cursor := crHourGlass;
  try
    imgView.Bitmap.Assign(FOriginalBitmap);

    LStart := GetTickCount();
    FFilter.Execute(imgView.Bitmap);
    LEnd := GetTickCount();

    imgView.Bitmap.Changed();
    
    StatusBar.Panels[0].Text := Format('Process Time: %d ms', [ (LEnd - LStart) ]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'JPEG Image|*.jpg';

  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FOriginalBitmap) then
      begin
        FreeAndNil(FOriginalBitmap);
      end;

      FOriginalBitmap := TBitmap32.Create();
      FOriginalBitmap.LoadFromFile(OpenPictureDialog.FileName);
      imgView.Bitmap.Assign(FOriginalBitmap);

      Caption := APP_CAPTION + ' - ' + ExtractFileName(OpenPictureDialog.FileName);

      lblZoom.Enabled     := True;
      cmbbxZoom.Enabled   := True;
      cmbbxZoom.ItemIndex := 3;
      cmbbxZoomChange(nil);

      btnRestore.Enabled := True;
      btnExecute.Enabled := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgView.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

procedure TfrmMain.btnRestoreClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    imgView.Bitmap.Assign(FOriginalBitmap);
    imgView.Bitmap.Changed();
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FOriginalBitmap := nil;
  FFilter         := TgmLinearAutoContrast.Create();

  ggbrShadowsClip.Position    := 10;
  ggbrHighlightsClip.Position := 10;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FOriginalBitmap.Free();
  FFilter.Free();
end;

procedure TfrmMain.ggbrShadowsClipChange(Sender: TObject);
begin
  FFilter.ShadowsClipPercent := ggbrShadowsClip.Position / 100;
  lblShadowsClip.Caption     := Format('Shadows Clip: %f', [FFilter.ShadowsClipPercent]) + '%';
end;

procedure TfrmMain.ggbrHighlightsClipChange(Sender: TObject);
begin
  FFilter.HighlightsClipPercent := ggbrHighlightsClip.Position / 100;
  lblHighlightsClip.Caption     := Format('Highlights Clip: %f', [FFilter.HighlightsClipPercent]) + '%';
end;

end.
