{
  Author: Ma Xiaoguang, Ma Xiaoming
  Date: 2010-11-23
} 

unit MainForm;

interface

uses
  { standard}
  Windows, Messages, SysUtils,
  Variants, Classes, Graphics,
  Controls, Forms, ExtCtrls,
  Dialogs, ExtDlgs, StdCtrls,
  Math,
  { GRAPHICS32 }
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_RangeBars,
  { GraphicsMagib Lib}
  gmCrop,
  gmIO,                 // LoadGraphicsFile(), SaveGraphicsFile()
  gmImageProcessFuncs;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    imgvwWorkArea: TImgView32;
    Button1: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    GroupBox: TGroupBox;
    lblShieldOpacity: TLabel;
    ggbrShieldOpacity: TGaugeBar;
    Label1: TLabel;
    shpShieldColor: TShape;
    ColorDialog: TColorDialog;
    btnCommit: TButton;
    procedure LoadImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgvwWorkAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwWorkAreaMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwWorkAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ggbrShieldOpacityChange(Sender: TObject);
    procedure shpShieldColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CommitCropClick(Sender: TObject);
    procedure imgvwWorkAreaResize(Sender: TObject);
  private
    { Private declarations }
    FBackLayer         : TBitmapLayer;
    FRBLayer           : TRubberBandLayer;
    FCrop              : TgmCrop;
    FDrawing           : Boolean;
    FStartPoint        : TPoint; // control coordinate space
    FEndPoint          : TPoint; // control coordinate space
    FRBLayerTopLeft    : TPoint; // bitmap coordinate space
    FRBLayerBottomRight: TPoint; // bitmap coordinate space

    procedure RBResizing(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);

    procedure RBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.RBResizing(Sender: TObject; const OldLocation: TFloatRect;
  var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
var
  tl, br: TPoint;
begin
  if Assigned(FCrop) then
  begin
    tl.X := Round(FRBLayer.Location.Left);
    tl.Y := Round(FRBLayer.Location.Top);
    br.X := Round(FRBLayer.Location.Right);
    br.Y := Round(FRBLayer.Location.Bottom);

    FCrop.FCropStart := imgvwWorkArea.ControlToBitmap(tl);
    FCrop.FCropEnd   := imgvwWorkArea.ControlToBitmap(br);
    FCrop.DrawShield;
  end;
end;

procedure TfrmMain.RBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tl, br: TPoint;
begin
  if Assigned(FCrop) then
  begin
    tl.X := Round(FRBLayer.Location.Left);
    tl.Y := Round(FRBLayer.Location.Top);
    br.X := Round(FRBLayer.Location.Right);
    br.Y := Round(FRBLayer.Location.Bottom);

    FCrop.FCropStart := imgvwWorkArea.ControlToBitmap(tl);
    FCrop.FCropEnd   := imgvwWorkArea.ControlToBitmap(br);
    FCrop.DrawShield;

    FRBLayerTopLeft     := FCrop.FCropStart;
    FRBLayerBottomRight := FCrop.FCropEnd;
  end;
end;

procedure TfrmMain.LoadImageClick(Sender: TObject);
var
  LBmp    : TBitmap32;
  LRect   : TRect;
  LTopLeft: TPoint;
begin
  if OpenPictureDialog.Execute then
  begin
    LBmp := LoadGraphicsFile(OpenPictureDialog.FileName);

    if not Assigned(LBmp) then
    begin
      MessageDlg('Cannot load picture ''' + ExtractFileName(OpenPictureDialog.FileName) + '''', mtError, [mbOK], 0);
      Exit;
    end;

    try
      FBackLayer.Bitmap.Assign(LBmp);
      FBackLayer.Bitmap.DrawMode := dmBlend;

      imgvwWorkArea.Bitmap.SetSize(LBmp.Width, LBmp.Height);
      imgvwWorkArea.Bitmap.Clear($FFFF0000);

      // get location of the bitmap in the TImgView32
      LRect := imgvwWorkArea.GetBitmapRect;

      // Convert the top-left point of the background bitmap of the TImage32
      // from control coordinate to bitmap coordinate. 
      LTopLeft := imgvwWorkArea.ControlToBitmap( Point(LRect.Left, LRect.Top) );

      FBackLayer.Location := FloatRect(LTopLeft.X, LTopLeft.Y,
                                       imgvwWorkArea.Bitmap.Width,
                                       imgvwWorkArea.Bitmap.Height);


      if Assigned(FRBLayer) then
      begin
        FreeAndNil(FRBLayer);
      end;
        
      if Assigned(FCrop) then
      begin
        FreeAndNil(FCrop);
      end;

      GroupBox.Visible := False;
    finally
      LBmp.Free;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCrop               := nil;
  FRBLayer            := nil;
  FDrawing            := False;
  FStartPoint         := Point(0, 0);
  FEndPoint           := Point(0, 0);
  FRBLayerTopLeft     := Point(0, 0); 
  FRBLayerBottomRight := Point(0, 0);

  FBackLayer        := TBitmapLayer.Create(imgvwWorkArea.Layers);
  FBackLayer.Scaled := True;

  imgvwWorkArea.Bitmap.DrawMode := dmBlend;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FRBLayer.Free;
  FCrop.Free;
  FBackLayer.Free;
end;

procedure TfrmMain.imgvwWorkAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if not Assigned(FCrop) then
  begin
    FStartPoint := Point(X, Y);
    FEndPoint   := Point(X, Y);

    imgvwWorkArea.Canvas.Pen.Mode    := pmNotXor;
    imgvwWorkArea.Canvas.Pen.Style   := psDot;
    imgvwWorkArea.Canvas.Brush.Style := bsClear;
    imgvwWorkArea.Canvas.Rectangle(FStartPoint.X, FStartPoint.Y, FEndPoint.X, FEndPoint.Y);

    FDrawing := True;
  end;
end;

procedure TfrmMain.imgvwWorkAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    // erasing the last figure
    imgvwWorkArea.Canvas.Rectangle(FStartPoint.X, FStartPoint.Y, FEndPoint.X, FEndPoint.Y);
    FEndPoint := Point(X, Y);
    imgvwWorkArea.Canvas.Rectangle(FStartPoint.X, FStartPoint.Y, FEndPoint.X, FEndPoint.Y);
  end;
end;

procedure TfrmMain.imgvwWorkAreaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  tl, br: TPoint;
begin
  if FDrawing then
  begin
    FDrawing := False;

    tl.X := MinIntValue([FStartPoint.X, FEndPoint.X]);
    tl.Y := MinIntValue([FStartPoint.Y, FEndPoint.Y]);
    br.X := MaxIntValue([FStartPoint.X, FEndPoint.X]);
    br.Y := MaxIntValue([FStartPoint.Y, FEndPoint.Y]);

    FCrop := TgmCrop.Create(FBackLayer.Bitmap.Width, FBackLayer.Bitmap.Height,
                            imgvwWorkArea.Layers, FBackLayer.Location);

    FCrop.ShieldOpacity := 20;
    FCrop.FCropStart    := imgvwWorkArea.ControlToBitmap(tl);
    FCrop.FCropEnd      := imgvwWorkArea.ControlToBitmap(br);

    FCrop.DrawShield;

    if Assigned(FRBLayer) then
    begin
      FreeAndNil(FRBLayer);
    end;

    FRBLayer            := TRubberBandLayer.Create(imgvwWorkArea.Layers);
    FRBLayer.Location   := FloatRect(tl.X, tl.Y, br.X, br.Y);
    FRBLayer.OnResizing := RBResizing;
    FRBLayer.OnMouseUp  := RBMouseUp;

    GroupBox.Visible           := True;
    ggbrShieldOpacity.Position := FCrop.ShieldOpacity;
    lblShieldOpacity.Caption   := 'Shield Opacity: ' + IntToStr(ggbrShieldOpacity.Position) + '%';
    shpShieldColor.Brush.Color := FCrop.ShieldWinColor;

    FRBLayerTopLeft     := FCrop.FCropStart;
    FRBLayerBottomRight := FCrop.FCropEnd;
  end;
end;

procedure TfrmMain.ggbrShieldOpacityChange(Sender: TObject);
begin
  lblShieldOpacity.Caption := 'Shield Opacity: ' + IntToStr(ggbrShieldOpacity.Position) + '%';
  FCrop.ShieldOpacity      := ggbrShieldOpacity.Position;
  FCrop.DrawShield;
end;

procedure TfrmMain.shpShieldColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := FCrop.ShieldWinColor;

  if ColorDialog.Execute then
  begin
    FCrop.ShieldColor32 := Color32(ColorDialog.Color);
    FCrop.DrawShield;
  end;
end;

procedure TfrmMain.CommitCropClick(Sender: TObject);
var
  LCropRect, ARect: TRect;
  LCropBitmap     : TBitmap32;
  LTopLeft        : TPoint;
begin
  if Assigned(FCrop) then
  begin
    Screen.Cursor := crHourGlass;
    try
      // doing crop...
      imgvwWorkArea.Bitmap.SetSize(FCrop.CropAreaWidth, FCrop.CropAreaHeight);

      LCropBitmap := TBitmap32.Create;
      try
        LCropBitmap.DrawMode := dmBlend;

        LCropRect := Rect(FCrop.FCropStart.X, FCrop.FCropStart.Y,
                          FCrop.FCropEnd.X, FCrop.FCropEnd.Y);

        CopyRect32WithARGB(LCropBitmap, FBackLayer.Bitmap, LCropRect, $00FFFFFF);

        FBackLayer.Bitmap.SetSize(LCropBitmap.Width, LCropBitmap.Height);
        CopyBitmap32(FBackLayer.Bitmap, LCropBitmap);

        // get location of the bitmap in the TImgView32
        ARect := imgvwWorkArea.GetBitmapRect;

        // Convert the top-left point of the background bitmap of the TImage32
        // from control coordinate to bitmap coordinate. 
        LTopLeft := imgvwWorkArea.ControlToBitmap( Point(ARect.Left, ARect.Top) );

        FBackLayer.Location := FloatRect(LTopLeft.X, LTopLeft.Y,
                                         imgvwWorkArea.Bitmap.Width,
                                         imgvwWorkArea.Bitmap.Height);
      finally
        LCropBitmap.Free;
      end;

      if Assigned(FRBLayer) then
      begin
        FreeAndNil(FRBLayer);
      end;

      FreeAndNil(FCrop);

      GroupBox.Visible := False;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.imgvwWorkAreaResize(Sender: TObject);
var
  tl, br: TPoint;
begin
  // keep the rubber band layer at the correct position when window size is changed
  if Assigned(FRBLayer) then
  begin
    tl := imgvwWorkArea.BitmapToControl(FRBLayerTopLeft);
    br := imgvwWorkArea.BitmapToControl(FRBLayerBottomRight);
    
    FRBLayer.Location := FloatRect(tl.X, tl.Y, br.X, br.Y);
  end;
end; 

end.
