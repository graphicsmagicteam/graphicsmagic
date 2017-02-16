unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, ExtCtrls, Menus, ExtDlgs, GR32_RangeBars, StdCtrls,
  gmGammaTuner;

type
  TfrmMain = class(TForm)
    pnlGammaOptionsHolder: TPanel;
    imgWorkArea: TImgView32;
    OpenPictureDialog: TOpenPictureDialog;
    mnMain: TMainMenu;
    mnhdFile: TMenuItem;
    mnitmOpenFile: TMenuItem;
    N1: TMenuItem;
    mnitmExitProgram: TMenuItem;
    lblChannel: TLabel;
    cmbbxChannel: TComboBox;
    lblGammaValue: TLabel;
    ggbrGammaValue: TGaugeBar;
    procedure mnitmOpenFileClick(Sender: TObject);
    procedure mnitmExitProgramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbbxChannelChange(Sender: TObject);
    procedure ggbrGammaValueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrGammaValueChange(Sender: TObject);
  private
    FGammaTuner: TgmGammaTuner;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Jpeg,
  gmTypes;

{$R *.dfm}

procedure TfrmMain.mnitmOpenFileClick(Sender: TObject);
var
  LJPG    : TJPEGImage;
  LBmp    : TBitmap;
  LFileExt: string;
begin
  if OpenPictureDialog.Execute then
  begin
    LFileExt := LowerCase(ExtractFileExt(OpenPictureDialog.FileName));

    if LFileExt = '.bmp' then
    begin
      imgWorkArea.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    end
    else if LFileExt = '.jpg' then
    begin
      LJPG := TJPEGImage.Create;
      LBmp := TBitmap.Create;
      try
        LJPG.LoadFromFile(OpenPictureDialog.FileName);
        LBmp.Assign(LJPG);
        LBmp.PixelFormat := pf24bit;
        imgWorkArea.Bitmap.Assign(LBmp);
      finally
        LJPG.Free;
        LBmp.Free;
      end;
    end;

    imgWorkArea.Changed;
    pnlGammaOptionsHolder.Visible := True;

    if Assigned(FGammaTuner) then
    begin
      FGammaTuner.Free;
    end;
    
    FGammaTuner := TgmGammaTuner.Create(imgWorkArea.Bitmap);
  end;
end;

procedure TfrmMain.mnitmExitProgramClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FGammaTuner := nil;
  
  cmbbxChannel.ItemIndex  := 0;
  ggbrGammaValue.Position := 100;
  lblGammaValue.Caption   := Format('RGB Gamma Value: %.2f', [ggbrGammaValue.Position / 100]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FGammaTuner) then
  begin
    FGammaTuner.Free;
  end;
end;

procedure TfrmMain.cmbbxChannelChange(Sender: TObject);
begin
  if Assigned(FGammaTuner) then
  begin
    case cmbbxChannel.ItemIndex of
      0:
        begin
          FGammaTuner.Channel     := wctRGB;
          ggbrGammaValue.Position := Round(FGammaTuner.RGBGammaValue * 100);
          lblChannel.Caption      := 'RGB Channel: ';
          lblGammaValue.Caption   := Format('RGB Gamma Value: %.2f', [FGammaTuner.RGBGammaValue]);
        end;

      1:
        begin
          FGammaTuner.Channel     := wctRed;
          ggbrGammaValue.Position := Round(FGammaTuner.RedGammaValue * 100);
          lblChannel.Caption      := 'Red Channel:';
          lblGammaValue.Caption   := Format('Red Gamma Value: %.2f', [FGammaTuner.RedGammaValue]);
        end;

      2:
        begin
          FGammaTuner.Channel     := wctGreen;
          ggbrGammaValue.Position := Round(FGammaTuner.GreenGammaValue * 100);
          lblChannel.Caption      := 'Green Channel:';
          lblGammaValue.Caption   := Format('Green Gamma Value: %.2f', [FGammaTuner.GreenGammaValue]);
        end;

      3:
      begin
        FGammaTuner.Channel     := wctBlue;
        ggbrGammaValue.Position := Round(FGammaTuner.BlueGammaValue * 100);
        lblChannel.Caption      := 'Blue Channel:';
        lblGammaValue.Caption   := Format('Blue Gamma Value: %.2f', [FGammaTuner.BlueGammaValue]);
      end;
    end;

    FGammaTuner.Execute(imgWorkArea.Bitmap);
  end;
  
  imgWorkArea.Changed;
end;

procedure TfrmMain.ggbrGammaValueMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FGammaTuner) then
  begin
    case FGammaTuner.Channel of
      wctRGB:
        begin
          FGammaTuner.Channel       := wctRGB;
          FGammaTuner.RGBGammaValue := ggbrGammaValue.Position / 100;
        end;

      wctRed:
        begin
          FGammaTuner.Channel       := wctRed;
          FGammaTuner.RedGammaValue := ggbrGammaValue.Position / 100;
        end;

      wctGreen:
        begin
          FGammaTuner.Channel         := wctGreen;
          FGammaTuner.GreenGammaValue := ggbrGammaValue.Position / 100;
        end;

      wctBlue:
        begin
          FGammaTuner.Channel        := wctBlue;
          FGammaTuner.BlueGammaValue := ggbrGammaValue.Position / 100;
        end;
    end;

    FGammaTuner.Execute(imgWorkArea.Bitmap);
  end;

  imgWorkArea.Changed;
end;

procedure TfrmMain.ggbrGammaValueChange(Sender: TObject);
begin
  if Assigned(FGammaTuner) then
  begin
    case FGammaTuner.Channel of
      wctRGB:
        begin
          lblGammaValue.Caption := Format('RGB Gamma Value: %.2f', [ggbrGammaValue.Position / 100]);
        end;

      wctRed:
        begin
          lblGammaValue.Caption := Format('Red Gamma Value: %.2f', [ggbrGammaValue.Position / 100]);
        end;

      wctGreen:
        begin
          lblGammaValue.Caption := Format('Green Gamma Value: %.2f', [ggbrGammaValue.Position / 100]);
        end;

      wctBlue:
        begin
          lblGammaValue.Caption := Format('Blue Gamma Value: %.2f', [ggbrGammaValue.Position / 100]);
        end;
    end;
  end;
end;

end.



