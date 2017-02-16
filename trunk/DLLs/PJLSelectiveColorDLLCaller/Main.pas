unit Main;

// Written by Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
// Last Update: February 13, 2015

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, ExtCtrls, StdCtrls, ExtDlgs, GR32, gmSelectiveColor,
  GR32_RangeBars, GR32_Layers;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    ImgView: TImgView32;
    btnLoadImage: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    Panel2: TPanel;
    lblColors: TLabel;
    cmbbxColors: TComboBox;
    lblCyan: TLabel;
    lblMagenta: TLabel;
    lblYellow: TLabel;
    lblBlack: TLabel;
    ggbrCyan: TGaugeBar;
    ggbrMagenta: TGaugeBar;
    ggbrYellow: TGaugeBar;
    ggbrBlack: TGaugeBar;
    rdbtnRelative: TRadioButton;
    rdbtnAbsolute: TRadioButton;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure ggbrCyanChange(Sender: TObject);
    procedure ggbrMagentaChange(Sender: TObject);
    procedure ggbrYellowChange(Sender: TObject);
    procedure ggbrBlackChange(Sender: TObject);
    procedure cmbbxColorsChange(Sender: TObject);
    procedure rdbtnRelativeClick(Sender: TObject);
    procedure rdbtnAbsoluteClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    { Private declarations }
    FOriginalBitmap : TBitmap32;
    FSelectiveColor : TgmSelectiveColor;
    FImageReady     : Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG,
  Math,
  GR32_LowLevel;

{$R *.dfm}

const
  GAUGE_BAR_MID_POS = 100;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FOriginalBitmap       := nil;
  FSelectiveColor       := TgmSelectiveColor.Create();
  FImageReady           := False;

  cmbbxColors.Items     := SelectiveColorChannelList();
  cmbbxColors.ItemIndex := 0;
  ggbrCyan.Position     := GAUGE_BAR_MID_POS;
  ggbrMagenta.Position  := GAUGE_BAR_MID_POS;
  ggbrYellow.Position   := GAUGE_BAR_MID_POS;
  ggbrBlack.Position    := GAUGE_BAR_MID_POS;
  rdbtnRelative.Checked := (FSelectiveColor.AdjustMode = amRelative);
  rdbtnAbsolute.Checked := (FSelectiveColor.AdjustMode = amAbsolute); 
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FOriginalBitmap.Free();
  FSelectiveColor.Free();
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  FImageReady := False;
  FSelectiveColor.Reset();
  
  OpenPictureDialog.Filter := 'Images|*jpg';

  if OpenPictureDialog.Execute() then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FOriginalBitmap) then
      begin
        FreeAndNil(FOriginalBitmap);
      end;

      FOriginalBitmap := TBitmap32.Create();
      FOriginalBitmap.LoadFromFile(OpenPictureDialog.FileName);
      FOriginalBitmap.DrawMode := dmBlend;

      imgView.Bitmap.Assign(FOriginalBitmap);

      lblColors.Enabled     := True;
      cmbbxColors.Enabled   := True;

      lblCyan.Enabled       := True;
      ggbrCyan.Enabled      := True;
      ggbrCyan.Position     := GAUGE_BAR_MID_POS;

      lblMagenta.Enabled    := True;
      ggbrMagenta.Enabled   := True;
      ggbrMagenta.Position  := GAUGE_BAR_MID_POS;

      lblYellow.Enabled     := True;
      ggbrYellow.Enabled    := True;
      ggbrYellow.Position   := GAUGE_BAR_MID_POS;
      
      lblBlack.Enabled      := True;
      ggbrBlack.Enabled     := True;
      ggbrBlack.Position    := GAUGE_BAR_MID_POS;

      rdbtnRelative.Enabled := True;
      rdbtnAbsolute.Enabled := True;

      cmbbxColors.ItemIndex := 0;
      cmbbxColorsChange(nil);

      lblZoom.Enabled     := True;
      cmbbxZoom.Enabled   := True;
      cmbbxZoom.ItemIndex := 3;

      FImageReady := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ggbrCyanChange(Sender: TObject);
var
  LValue : Integer;
begin
  LValue          := ggbrCyan.Position - GAUGE_BAR_MID_POS;
  lblCyan.Caption := Format('Cyan: %d', [LValue]);

  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.Cyan[cmbbxColors.ItemIndex] := LValue;

    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.ggbrMagentaChange(Sender: TObject);
var
  LValue : Integer;
begin
  LValue             := ggbrMagenta.Position - GAUGE_BAR_MID_POS;
  lblMagenta.Caption := Format('Magenta: %d', [LValue]);

  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.Magenta[cmbbxColors.ItemIndex] := LValue;

    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.ggbrYellowChange(Sender: TObject);
var
  LValue : Integer;
begin
  LValue            := ggbrYellow.Position - GAUGE_BAR_MID_POS;
  lblYellow.Caption := Format('Yellow: %d', [LValue]);

  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.Yellow[cmbbxColors.ItemIndex] := LValue;
    
    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.ggbrBlackChange(Sender: TObject);
var
  LValue : Integer;
begin
  LValue           := ggbrBlack.Position - GAUGE_BAR_MID_POS;
  lblBlack.Caption := Format('Black: %d', [LValue]);

  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.Black[cmbbxColors.ItemIndex] := LValue;
    
    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.cmbbxColorsChange(Sender: TObject);
begin
  if Assigned(FSelectiveColor) then
  begin
    ggbrCyan.Position    := FSelectiveColor.Cyan[cmbbxColors.ItemIndex] + GAUGE_BAR_MID_POS;
    ggbrMagenta.Position := FSelectiveColor.Magenta[cmbbxColors.ItemIndex] + GAUGE_BAR_MID_POS;
    ggbrYellow.Position  := FSelectiveColor.Yellow[cmbbxColors.ItemIndex] + GAUGE_BAR_MID_POS;
    ggbrBlack.Position   := FSelectiveColor.Black[cmbbxColors.ItemIndex] + GAUGE_BAR_MID_POS;
  end;
end;

procedure TfrmMain.rdbtnRelativeClick(Sender: TObject);
begin
  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.AdjustMode := amRelative;

    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.rdbtnAbsoluteClick(Sender: TObject);
begin
  if Assigned(FOriginalBitmap) and FImageReady then
  begin
    FSelectiveColor.AdjustMode := amAbsolute;

    imgView.Bitmap.Assign(FOriginalBitmap);
    FSelectiveColor.Execute(ImgView.Bitmap);
    ImgView.Bitmap.Changed();
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  ImgView.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

end.
