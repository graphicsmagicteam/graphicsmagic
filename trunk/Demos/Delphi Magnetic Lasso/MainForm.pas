unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, Menus, ExtCtrls, StdCtrls, ComCtrls, GR32_Image, GR32,
  GR32_Layers, gmMagneticLasso;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnitmOpenImage: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    imgvwWorkArea: TImgView32;
    Splitter1: TSplitter;
    imgvwMask: TImgView32;
    chckbxInteractive: TCheckBox;
    pnlStatus: TPanel;
    cmbbxZoomer: TComboBox;
    btnReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenImageClick(Sender: TObject);
    procedure imgvwWorkAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwWorkAreaMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwWorkAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure chckbxInteractiveClick(Sender: TObject);
    procedure cmbbxZoomerChange(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
    FXActual       : Integer;
    FYActual       : Integer;
    FDrawing       : Boolean;
    FSourceBitmap  : TBitmap32;
    FMagneticLasso : TgmMagneticLasso;
    FLassoLayer    : TBitmapLayer;

    procedure CalcBitmapSpaceCoords(const X, Y: Integer);
    procedure LassoLayerBlend(F: TColor32; var B: TColor32; M: TColor32);
    procedure CreateLassoLayer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JPEG;

{$R *.dfm}


// Parameter X and Y are both in control space.
// Calculating the corresponding coordinates in Bitmap space.
procedure TfrmMain.CalcBitmapSpaceCoords(const X, Y: Integer);
var
  LPoint: TPoint;
begin
  LPoint := Point(X, Y);
  LPoint := imgvwWorkArea.ControlToBitmap(LPoint);

  FXActual := LPoint.X;
  FYActual := LPoint.Y;
end;

// blending callback function for the OnPixelCombine event of the lasso layer
procedure TfrmMain.LassoLayerBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LBlendColor: TColor32;
begin
  if F <> $00000000 then
  begin
    LBlendColor := not ($FF7F7F7F xor B);
    B           := LBlendColor or $FF000000;
  end;
end;

procedure TfrmMain.CreateLassoLayer;
var
  LHalfWidth, LHalfHeight: Single;
  LCenterPoint           : TPoint;
begin
  if not Assigned(FLassoLayer) then
  begin
    FLassoLayer := TBitmapLayer.Create(imgvwWorkArea.Layers);

    FLassoLayer.Bitmap.DrawMode       := dmCustom;
    FLassoLayer.Bitmap.OnPixelCombine := LassoLayerBlend;

    FLassoLayer.Bitmap.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);
    FLassoLayer.Bitmap.Clear($00000000);

    LHalfWidth  := FLassoLayer.Bitmap.Width  / 2;
    LHalfHeight := FLassoLayer.Bitmap.Height / 2;

    // get the center point of the viewport of the TImage32/TImgView32 and
    // convert it from control space to bitmap space
    with imgvwWorkArea.GetViewportRect do
    begin
      LCenterPoint := imgvwWorkArea.ControlToBitmap(
        Point( (Right + Left) div 2, (Top + Bottom) div 2 )  );
    end;
    
    // setting the location of the layer
    FLassoLayer.Location := FloatRect(LCenterPoint.X - LHalfWidth,
                                      LCenterPoint.Y - LHalfHeight,
                                      LCenterPoint.X + LHalfWidth,
                                      LCenterPoint.Y + LHalfHeight);

    FLassoLayer.Scaled := True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSourceBitmap := nil;
  FLassoLayer   := nil;
  FDrawing      := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FSourceBitmap) then
  begin
    FSourceBitmap.Free;
  end;

  if Assigned(FMagneticLasso) then
  begin
    FMagneticLasso.Free;
  end;

  if Assigned(FLassoLayer) then
  begin
    FLassoLayer.Free;
  end;
end;

procedure TfrmMain.OpenImageClick(Sender: TObject);
var
  LJPEGImage: TJPEGImage;
  LTempBmp  : TBitmap;
begin
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FSourceBitmap) then
      begin
        FreeAndNil(FSourceBitmap);
      end;

      FSourceBitmap := TBitmap32.Create;

      // loading images...
      LJPEGImage := TJPEGImage.Create;
      LTempBmp   := TBitmap.Create;
      try
        LJPEGImage.LoadFromFile(OpenPictureDialog.FileName);
        
        LTempBmp.Height      := LJPEGImage.Height;
        LTempBmp.Width       := LJPEGImage.Width;
        LTempBmp.PixelFormat := pf24bit;
        LTempBmp.Canvas.Draw(0, 0, LJPEGImage);

        FSourceBitmap.Assign(LTempBmp);
      finally
        LTempBmp.Free;
        LJPEGImage.Free;
      end;

      imgvwWorkArea.Bitmap.Assign(FSourceBitmap);

      if Assigned(FLassoLayer) then
      begin
        FreeAndNil(FLassoLayer);
      end;
      
      CreateLassoLayer;

      // create magnetic lasso tool
      if Assigned(FMagneticLasso) then
      begin
        FreeAndNil(FMagneticLasso);
      end;

      FMagneticLasso := TgmMagneticLasso.Create(FSourceBitmap, FLassoLayer.Bitmap.Canvas);

      FMagneticLasso.IsInteractive := chckbxInteractive.Checked;

    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.imgvwWorkAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Button = mbLeft then
  begin
    if Assigned(FMagneticLasso) then
    begin
      CalcBitmapSpaceCoords(X, Y);
      FMagneticLasso.MouseDown(Button, Shift, FXActual, FYActual);

      if Assigned(FLassoLayer) then
      begin
        FLassoLayer.Changed;
      end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmMain.imgvwWorkAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  CalcBitmapSpaceCoords(X, Y);
  
  if FDrawing then
  begin
    if Assigned(FMagneticLasso) then
    begin
      FMagneticLasso.MouseMove(Shift, FXActual, FYActual);

      if Assigned(FLassoLayer) then
      begin
        FLassoLayer.Changed;
      end;
    end;
  end;
    
  pnlStatus.Caption := Format('X = %d, Y = %d', [FXActual, FYActual]);
end;

procedure TfrmMain.imgvwWorkAreaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    FDrawing := False;

    if Assigned(FMagneticLasso) then
    begin
      CalcBitmapSpaceCoords(X, Y);
      FMagneticLasso.MouseUp(Button, Shift, FXActual, FYActual);

      if Assigned(FLassoLayer) then
      begin
        FLassoLayer.Changed;
      end;

      if FMagneticLasso.IsConnected then
      begin
        imgvwMask.Bitmap.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);
        imgvwMask.Bitmap.Clear(clBlack32);

        imgvwMask.Bitmap.Canvas.Brush.Color := clWhite;

        FillRgn(imgvwMask.Bitmap.Canvas.Handle, FMagneticLasso.CurveRegion,
                imgvwMask.Bitmap.Canvas.Brush.Handle);

        imgvwMask.Bitmap.Changed;
      end;
    end;
  end;
end;

procedure TfrmMain.chckbxInteractiveClick(Sender: TObject);
begin
  if Assigned(FMagneticLasso) then
  begin
    FMagneticLasso.IsInteractive := chckbxInteractive.Checked;
  end;
end;

procedure TfrmMain.cmbbxZoomerChange(Sender: TObject);
begin
  case cmbbxZoomer.ItemIndex of
    0:
      begin
        imgvwWorkArea.Scale := 16.0;
      end;

    1:
      begin
        imgvwWorkArea.Scale := 8.0;
      end;

    2:
      begin
        imgvwWorkArea.Scale := 4.0;
      end;

    3:
      begin
        imgvwWorkArea.Scale := 2.0;
      end;

    4:
      begin
        imgvwWorkArea.Scale := 1.0;
      end;

    5:
      begin
        imgvwWorkArea.Scale := 0.5;
      end;

    6:
      begin
        imgvwWorkArea.Scale := 0.25;
      end;

    7:
      begin
        imgvwWorkArea.Scale := 0.125;
      end;

    8:
      begin
        imgvwWorkArea.Scale := 0.0625;
      end;
  end;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  if Assigned(FMagneticLasso) then
  begin
    FMagneticLasso.Reset;
  end;

  if Assigned(FLassoLayer) then
  begin
    FLassoLayer.Bitmap.Clear($00000000);
    FLassoLayer.Changed;
  end;
end; 

end.
