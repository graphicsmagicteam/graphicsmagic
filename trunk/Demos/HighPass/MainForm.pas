// Author: Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
// Date: Feb. 1st, 2013

unit MainForm;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtDlgs, Menus, ExtCtrls,
{ Graphics32 }
  GR32_Image, GR32, GR32_RangeBars,
{ GraphicsMagic }
  gmHighPass;

type
  TfrmMain = class(TForm)
    imgvwPreview: TImgView32;
    OpenPictureDialog: TOpenPictureDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnitmLoadImage: TMenuItem;
    N1: TMenuItem;
    mnitmExit: TMenuItem;
    Panel1: TPanel;
    chckbxHighPass: TCheckBox;
    ggbrRadius: TGaugeBar;
    cmbbxZoom: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ggbrRadiusChange(Sender: TObject);
    procedure ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnitmExitClick(Sender: TObject);
    procedure mnitmLoadImageClick(Sender: TObject);
    procedure chckbxHighPassClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    FSourceBitmap  : TBitmap32;
    FHighPassFilter: TgmHighPass;

    procedure ExecuteHighPass;
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

procedure TfrmMain.ExecuteHighPass;
begin
  if chckbxHighPass.Enabled then
  begin
    Screen.Cursor := crHourGlass;
    try
      imgvwPreview.Bitmap.Assign(FSourceBitmap);

      FHighPassFilter.Radius := ggbrRadius.Position / 10;
      FHighPassFilter.Execute(imgvwPreview.Bitmap);

      imgvwPreview.Bitmap.Changed;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSourceBitmap   := TBitmap32.Create;
  FHighPassFilter := TgmHighPass.Create;
  
  ggbrRadius.Position := 100;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FHighPassFilter.Free;
  FSourceBitmap.Free;
end;

procedure TfrmMain.ggbrRadiusChange(Sender: TObject);
begin
  chckbxHighPass.Caption := Format('Radius: %.1f', [ggbrRadius.Position / 10]);
end;

procedure TfrmMain.ggbrRadiusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if chckbxHighPass.Checked then
  begin
    ExecuteHighPass;
  end;
end;

procedure TfrmMain.mnitmExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnitmLoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FSourceBitmap.LoadFromFile(OpenPictureDialog.FileName);

      chckbxHighPass.Enabled := True;
      cmbbxZoom.Enabled      := True;
      cmbbxZoom.ItemIndex    := 3;
      cmbbxZoomChange(nil);

      if chckbxHighPass.Checked then
      begin
        ExecuteHighPass;
      end
      else
      begin
        imgvwPreview.Bitmap.Assign(FSourceBitmap);
      end;
      
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.chckbxHighPassClick(Sender: TObject);
begin
  if chckbxHighPass.Checked then
  begin
    ExecuteHighPass;
  end
  else
  begin
    imgvwPreview.Bitmap.Assign(FSourceBitmap);
  end;

  ggbrRadius.Enabled := chckbxHighPass.Checked;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgvwPreview.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

end.
