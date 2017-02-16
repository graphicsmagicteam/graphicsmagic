unit MainForm;

{ Author:
    Ma Xiaoguang, Ma Xiaoming < gmbros[at]hotmail[dot]com >.

  Description:
    This tool is initially designed for generating a Mask bitmap for making
    splash form for GraphicsMagic. Please open the frmSplash form in
    project GraphicsMagic for details. }

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls,
{ Graphics32 }
  GR32, GR32_Image;

type
  TfrmMain = class(TForm)
    imgPNG: TImgView32;
    OpenPictureDialog: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    Panel1: TPanel;
    btnLoad: TButton;
    imgMask: TImgView32;
    btnSave: TButton;
    Splitter1: TSplitter;
    btnGenerateMask: TButton;
    cmbbxZoom: TComboBox;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnGenerateMaskClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
    procedure imgPNGPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ GraphicsMagicLib }
  gmIO,
  gmPaintFuncs;

const
  APP_CAPTION = 'PNG Alpha --> Bitmap Mask';

{$R *.dfm}

procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  LBmp : TBitmap32;
begin
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      LBmp := gmIO.LoadGraphicsFile(OpenPictureDialog.FileName);

      if Assigned(LBmp) then
      begin
        LBmp.DrawMode := dmBlend;
        
        imgPNG.Bitmap.Assign(LBmp);
        LBmp.Free;

        Caption := APP_CAPTION + ' [' +
          ExtractFileName(OpenPictureDialog.FileName) + ']';
          
        btnGenerateMask.Enabled := True;
        cmbbxZoom.Enabled       := True;
        cmbbxZoom.ItemIndex     := 3;

        ZoomChange(cmbbxZoom);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  LFileName : string;
  LFileExt  : string;
begin
  if SavePictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      LFileName := SavePictureDialog.FileName;
      LFileExt  := Lowercase( ExtractFileExt(LFileName) );

      if LFileExt = '' then
      begin
        LFileName := LFileName + '.bmp';
      end
      else if LFileExt <> '.bmp' then
      begin
        LFileName := ChangeFileExt(LFileName, '.bmp');
      end;

      if FileExists(LFileName) then
      begin
        if MessageDlg('The file ' + LFileName + ' is already exists.' + #10#13 +
                      'Do you want to overwrite it?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          imgMask.Bitmap.SaveToFile(LFileName);
        end;
      end
      else
      begin
        imgMask.Bitmap.SaveToFile(LFileName);
      end;

    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.btnGenerateMaskClick(Sender: TObject);
var
  i, w, h : Integer;
  a       : Byte;
  p1, p2  : PColor32;
begin
  Screen.Cursor := crHourGlass;
  try
    imgMask.Bitmap.SetSizeFrom(imgPNG.Bitmap);

    w := imgPNG.Bitmap.Width;
    h := imgPNG.Bitmap.Height;

    p1 := @imgPNG.Bitmap.Bits[0];
    p2 := @imgMask.Bitmap.Bits[0];
    for i := 1 to (w * h) do
    begin
      a   := p1^ shr 24 and $FF;
      p2^ := Gray32(a);

      Inc(p1);
      Inc(p2);
    end;

    imgMask.Bitmap.Changed;
    btnSave.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := APP_CAPTION;

  { by default, PST_CLEAR_BACKGND is executed at this stage,
    which, in turn, calls ExecClearBackgnd method of ImgView.
    Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
    to call the OnPaintStage event instead of performing default action. }
  with imgPNG.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;
end;

procedure TfrmMain.ZoomChange(Sender: TObject);
begin
  imgPNG.Scale  := (cmbbxZoom.ItemIndex + 1) * 0.25;
  imgMask.Scale := imgPNG.Scale;
end;

procedure TfrmMain.imgPNGPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
var
  LRect : TRect;
begin
  // draw background
  if (Buffer.Height > 0) and (Buffer.Width > 0) then
  begin
    Buffer.Clear($FFC0C0C0);

    // draw thin border, written by Andre Felix Miertschink
    LRect := imgPNG.GetBitmapRect;
    DrawCheckerboardPattern(Buffer, LRect);

    LRect.Left   := LRect.Left   - 1;
    LRect.Top    := LRect.Top    - 1;
    LRect.Right  := LRect.Right  + 1;
    LRect.Bottom := LRect.Bottom + 1;

    Buffer.FrameRectS(LRect, clBlack32);
  end;
end;

end.
