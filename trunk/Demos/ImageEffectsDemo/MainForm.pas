unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GR32, GR32_Layers, GR32_Image, Buttons, FileCtrl,
  GR32_RangeBars, ExtCtrls;

type
  TgmFilterSelector = (gmfsNone,
                       gmfsBoxBlur,
                       gmfsGaussianBlur,
                       gmfsMotionBlur,
                       gmfsArtFragmentation,
                       gmfsSelectiveColor);

  TfrmMain = class(TForm)
    pnlImageFiles: TPanel;
    pnlFilters: TPanel;
    pnlView: TPanel;
    imgvwDisplay: TImgView32;
    pnlLeftCollapse: TPanel;
    pnlLeftArrowHolder: TPanel;
    pnlTopPad1: TPanel;
    pnlBottomPad1: TPanel;
    Bitmap32List1: TBitmap32List;
    imgLeftCollapse: TImage32;
    pnlRightCollapse: TPanel;
    pnlOptions: TPanel;
    pnlBottomCollapse: TPanel;
    pnlDriveBoxHolder: TPanel;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    rdbtnBoxBlur: TRadioButton;
    pnlTopPad2: TPanel;
    pnlRightArrowHolder: TPanel;
    pnlBottomPad2: TPanel;
    imgRightCollapse: TImage32;
    pnlLeftPad: TPanel;
    pnlBottomArrowHolder: TPanel;
    pnlRightPad: TPanel;
    imgBottomCollapse: TImage32;
    ntbkFilterOptions: TNotebook;
    scrlbxBoxBlurOptions: TScrollBox;
    ggbrBoxBlurHorzRadius: TGaugeBar;
    Panel1: TPanel;
    imgThumbnail: TImage32;
    lblBoxBlurHorzRadius: TLabel;
    lblBoxBlurVertRadius: TLabel;
    ggbrBoxBlurVertRadius: TGaugeBar;
    lblBoxBlurIterations: TLabel;
    ggbrBoxBlurIterations: TGaugeBar;
    rdbtnGaussianBlur: TRadioButton;
    lblGaussianBlurRadius: TLabel;
    ggbrGaussianBlurRadius: TGaugeBar;
    rdbtnArtFragmentation: TRadioButton;
    pnlTitle: TPanel;
    Label1: TLabel;
    shpArtFragBorderColor: TShape;
    ColorDialog: TColorDialog;
    chckbxArtFragAutoBorder: TCheckBox;
    rdbtnSelectiveColor: TRadioButton;
    ggbrCyanScale: TGaugeBar;
    lblCyanScale: TLabel;
    lblMagentaScale: TLabel;
    ggbrMagentaScale: TGaugeBar;
    lblYellowScale: TLabel;
    ggbrYellowScale: TGaugeBar;
    lblBlackScale: TLabel;
    ggbrBlackScale: TGaugeBar;
    cmbbxSelectiveColors: TComboBox;
    lblSelectiveColors: TLabel;
    Label2: TLabel;
    rdbtnSelectiveColorRelative: TRadioButton;
    rdbtnSelectiveColorAbsolute: TRadioButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel2: TPanel;
    cmbbxZoomer: TComboBox;
    rdbtnMotionBlur: TRadioButton;
    lblMotionBlurAngle: TLabel;
    lblMotionBlurDistance: TLabel;
    lblMotionBlurRotation: TLabel;
    lblMotionBlurZoom: TLabel;
    lblMotionBlurAngleValue: TLabel;
    lblMotionBlurDistanceValue: TLabel;
    lblMotionBlurRotationValue: TLabel;
    lblMotionBlurZoomValue: TLabel;
    ggbrMotionBlurAngle: TGaugeBar;
    ggbrMotionBlurDistance: TGaugeBar;
    ggbrMotionBlurRotation: TGaugeBar;
    ggbrMotionBlurZoom: TGaugeBar;
    chckbxMotionBlurWrapEdges: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgLeftCollapseMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgLeftCollapseMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure FormDestroy(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);
    procedure imgRightCollapseMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgRightCollapseMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure pnlBottomCollapseResize(Sender: TObject);
    procedure pnlLeftCollapseResize(Sender: TObject);
    procedure pnlRightCollapseResize(Sender: TObject);
    procedure imgBottomCollapseMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgBottomCollapseMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure FormResize(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure ggbrBoxBlurHorzRadiusChange(Sender: TObject);
    procedure ggbrBoxBlurVertRadiusChange(Sender: TObject);
    procedure ggbrBoxBlurIterationsChange(Sender: TObject);
    procedure ggbrBoxBlurHorzRadiusMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrBoxBlurVertRadiusMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrBoxBlurIterationsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrGaussianBlurRadiusChange(Sender: TObject);
    procedure ggbrGaussianBlurRadiusMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeFilterClick(Sender: TObject);
    procedure cmbbxZoomerChange(Sender: TObject);
    procedure shpArtFragBorderColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chckbxArtFragAutoBorderClick(Sender: TObject);
    procedure ggbrCyanScaleChange(Sender: TObject);
    procedure ggbrCyanScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrMagentaScaleChange(Sender: TObject);
    procedure ggbrYellowScaleChange(Sender: TObject);
    procedure ggbrBlackScaleChange(Sender: TObject);
    procedure ggbrMagentaScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrYellowScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrBlackScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmbbxSelectiveColorsChange(Sender: TObject);
    procedure ChangeSelectiveColorMethod(Sender: TObject);
    procedure ggbrMotionBlurAngleMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrMotionBlurDistanceMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrMotionBlurRotationMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrMotionBlurZoomMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrMotionBlurAngleChange(Sender: TObject);
    procedure ggbrMotionBlurDistanceChange(Sender: TObject);
    procedure ggbrMotionBlurRotationChange(Sender: TObject);
    procedure ggbrMotionBlurZoomChange(Sender: TObject);
    procedure chckbxMotionBlurWrapEdgesClick(Sender: TObject);
  private
    { Private declarations }
    FSourceBitmap  : TBitmap32;
    FFileName      : string;
    FFilter        : TObject;
    FFilterSelector: TgmFilterSelector;

    procedure ArrowImageBlend(F: TColor32; var B: TColor32; M: TColor32);
    procedure UpdateThumnail(const AFileName: string);

    procedure SetFilter;
    procedure SetTitles;
    function ExecuteFilter: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Standard }
  JPEG,
{ GraphicsMagic Lib }
  gmConvolveFilter,
  gmBoxBlurFilter,
  gmGaussianBlurFilter,
  gmMotionBlurFilter,
  gmArtFragmentationFilter,
  gmSelectiveColorFilter;

{$R *.dfm}

const
  LEFT_ARROW_INDEX   = 0;
  RIGHT_ARROW_INDEX  = 1;
  TOP_ARROW_INDEX    = 2;
  BOTTOM_ARROW_INDEX = 3;
  MIN_WINDOW_WIDTH   = 800;
  MIN_WINDOW_HEIGHT  = 500;
  GAUSSIAN_POS_SCALE = 10;

procedure TfrmMain.ArrowImageBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LGrayscale: Byte;
  r1, g1, b1: Byte;
  r2, g2, b2: Byte;
begin
  LGrayscale := Intensity(F);

  r1 := F shr 16 and $FF;
  g1 := F shr  8 and $FF;
  b1 := F        and $FF;

  r2 := B shr 16 and $FF;
  g2 := B shr  8 and $FF;
  b2 := B        and $FF;

  r1 := (r2 * LGrayscale + r1 * (255 - LGrayscale)) div 255;
  g1 := (g2 * LGrayscale + g1 * (255 - LGrayscale)) div 255;
  b1 := (b2 * LGrayscale + b1 * (255 - LGrayscale)) div 255;

  B  := $FF000000 or (r1 shl 16) or (g1 shl 8) or b1;
end;

procedure TfrmMain.UpdateThumnail(const AFileName: string);
var
  jpg     : TJPEGImage;
  bmp     : TBitmap;
  hs, vs  : Single;
  tw, th  : Integer;
  LExtName: string;
begin
  if AFileName = '' then
  begin
    imgThumbnail.Bitmap.SetSize(0, 0);
  end
  else
  begin
    if FileExists(AFileName) then
    begin
      LExtName := ExtractFileExt(AFileName);
      LExtName := Lowercase(LExtName);

      // open the image
      Screen.Cursor := crHourGlass;
      bmp           := TBitmap.Create;
      try
        if LExtName = '.jpg' then
        begin
          jpg := TJPEGImage.Create;
          try
            jpg.LoadFromFile(AFileName);
            bmp.Width       := jpg.Width;
            bmp.Height      := jpg.Height;
            bmp.PixelFormat := pf24bit;
            bmp.Canvas.Draw(0, 0, jpg);
          finally
            jpg.Free;
          end;
        end
        else if LExtName = '.bmp' then
        begin
          bmp.LoadFromFile(AFileName);
        end;

        imgThumbnail.Bitmap.Assign(bmp);

        tw := imgThumbnail.Width  - 20;
        th := imgThumbnail.Height - 20;

        // set viewing scale
        if (bmp.Width > tw) or (bmp.Height > th) then
        begin
          hs := tw / bmp.Width;
          vs := th / bmp.Height;

          if hs < vs then
          begin
            imgThumbnail.Scale := hs;
          end
          else
          begin
            imgThumbnail.Scale := vs;
          end;
        end;
      finally
        bmp.Free;
        Screen.Cursor := crDefault;
      end;
    end
    else
    begin
      imgThumbnail.Bitmap.SetSize(0, 0);
    end;
  end;
end;

procedure TfrmMain.SetFilter;
var
  LColorSelector: TgmColorSelector;
begin
  FFilterSelector := gmfsNone;

  if Assigned(FFilter) then
  begin
    FreeAndNil(FFilter);
  end;

  if rdbtnBoxBlur.Checked then
  begin
    FFilterSelector := gmfsBoxBlur;
    FFilter         := TgmBoxBlurFilter.Create;

    ggbrBoxBlurHorzRadius.Position := TgmBoxBlurFilter(FFilter).HorizontalRadius;
    ggbrBoxBlurVertRadius.Position := TgmBoxBlurFilter(FFilter).VerticalRadius;
    ggbrBoxBlurIterations.Position := TgmBoxBlurFilter(FFilter).Iterations;
    lblBoxBlurHorzRadius.Caption   := Format('Horizontal Radius: %d', [ggbrBoxBlurHorzRadius.Position]);
    lblBoxBlurVertRadius.Caption   := Format('Vertical Radius: %d', [ggbrBoxBlurVertRadius.Position]);
    lblBoxBlurIterations.Caption   := Format('Iterations: %d', [ggbrBoxBlurIterations.Position]);
  end
  else if rdbtnGaussianBlur.Checked then
  begin
    FFilterSelector := gmfsGaussianBlur;
    FFilter         := TgmGaussianFilter.Create;

    ggbrGaussianBlurRadius.Position := Round( TgmGaussianFilter(FFilter).Radius * GAUSSIAN_POS_SCALE );
    lblGaussianBlurRadius.Caption   := Format('Gaussian Blur Radius: %.1f', [TgmGaussianFilter(FFilter).Radius]);
  end
  else if rdbtnMotionBlur.Checked then
  begin
    FFilterSelector := gmfsMotionBlur;
    FFilter         := TgmMotionBlurFilter.Create;
  end
  else if rdbtnArtFragmentation.Checked then
  begin
    FFilterSelector := gmfsArtFragmentation;
    FFilter         := TgmArtFragmentationFilter.Create;

    TgmArtFragmentationFilter(FFilter).BorderColor := Color32(shpArtFragBorderColor.Brush.Color);
    TgmArtFragmentationFilter(FFilter).IsAutoColor := chckbxArtFragAutoBorder.Checked;
  end
  else if rdbtnSelectiveColor.Checked then
  begin
    FFilterSelector := gmfsSelectiveColor;
    FFilter         := TgmSelectiveColorFilter.Create;

    if rdbtnSelectiveColorRelative.Checked then
    begin
      TgmSelectiveColorFilter(FFilter).AdjustMode := gmscamRelative;
    end
    else
    if rdbtnSelectiveColorAbsolute.Checked then
    begin
      TgmSelectiveColorFilter(FFilter).AdjustMode := gmscamAbsolute;
    end;

    LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);
    TgmSelectiveColorFilter(FFilter).CyanScales[LColorSelector]    := (ggbrCyanScale.Position - 100) / 100;
    TgmSelectiveColorFilter(FFilter).MagentaScales[LColorSelector] := (ggbrMagentaScale.Position - 100) / 100;
    TgmSelectiveColorFilter(FFilter).YellowScales[LColorSelector]  := (ggbrYellowScale.Position - 100) / 100;
    TgmSelectiveColorFilter(FFilter).BlackScales[LColorSelector]   := (ggbrBlackScale.Position - 100) / 100;
  end;

  if FFilterSelector in [gmfsBoxBlur, gmfsGaussianBlur, gmfsMotionBlur,
                         gmfsArtFragmentation, gmfsSelectiveColor] then
  begin
    ntbkFilterOptions.PageIndex := Ord(FFilterSelector);
  end
  else
  begin
    ntbkFilterOptions.PageIndex := 0;
  end;

  SetTitles;
  ExecuteFilter;
end;

procedure TfrmMain.SetTitles;
var
  LFilterName: string;
  LFileName  : string;
begin
  LFilterName := '';
  LFileName   := ExtractFileName(FFileName);

  if rdbtnBoxBlur.Checked then
  begin
    LFilterName := 'Box Blur';
  end
  else if rdbtnGaussianBlur.Checked then
  begin
    LFilterName := 'Gaussian Blur';
  end
  else if rdbtnMotionBlur.Checked then
  begin
    LFilterName := 'Motion Blur';
  end
  else if rdbtnArtFragmentation.Checked then
  begin
    LFilterName := 'Art Fragmentation';
  end
  else if rdbtnSelectiveColor.Checked then
  begin
    LFilterName := 'Selective Color';
  end;

  pnlTitle.Caption := LFileName + ' -- ' + LFilterName;
end;

function TfrmMain.ExecuteFilter: Boolean;
begin
  Result := False;
  
  if Assigned(FSourceBitmap) then
  begin
    Screen.Cursor := crHourGlass;
    try
      imgvwDisplay.Bitmap.Assign(FSourceBitmap);

      if Assigned(FFilter) then
      begin
        case FFilterSelector of
          gmfsBoxBlur:
            begin
              TgmBoxBlurFilter(FFilter).Execute(FSourceBitmap, imgvwDisplay.Bitmap);
            end;

          gmfsGaussianBlur:
            begin
              TgmGaussianFilter(FFilter).Execute(FSourceBitmap, imgvwDisplay.Bitmap);
            end;

          gmfsMotionBlur:
            begin
              TgmMotionBlurFilter(FFilter).Execute(FSourceBitmap, imgvwDisplay.Bitmap)
            end;

          gmfsArtFragmentation:
            begin
              imgvwDisplay.Bitmap.Assign(FSourceBitmap);
              TgmArtFragmentationFilter(FFilter).Execute(imgvwDisplay.Bitmap);
            end;

          gmfsSelectiveColor:
            begin
              imgvwDisplay.Bitmap.Assign(FSourceBitmap);
              TgmSelectiveColorFilter(FFilter).Execute(imgvwDisplay.Bitmap);
            end;
        end;

        Result := True;
      end;

      imgvwDisplay.Bitmap.Changed;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  with imgLeftCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[LEFT_ARROW_INDEX]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;

  with imgRightCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[RIGHT_ARROW_INDEX]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;

  with imgBottomCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[TOP_ARROW_INDEX]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;

  FSourceBitmap   := nil;
  FFileName       := '';
  FFilter         := nil;
  FFilterSelector := gmfsNone;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FFilter) then
  begin
    FFilter.Free;
  end;
  
  if Assigned(FSourceBitmap) then
  begin
    FSourceBitmap.Free;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  pnlTopPad1.Height := (pnlLeftCollapse.Height - pnlLeftArrowHolder.Height) div 2;
  pnlTopPad2.Height := pnlTopPad1.Height;
  pnlLeftPad.Width  := (pnlBottomCollapse.Width - pnlBottomArrowHolder.Width) div 2;

  SetFilter;
end;

procedure TfrmMain.pnlBottomCollapseResize(Sender: TObject);
begin
  pnlLeftPad.Left  := 0;
  pnlLeftPad.Width := (pnlBottomCollapse.Width - pnlBottomArrowHolder.Width) div 2;

  pnlBottomArrowHolder.Left := pnlLeftPad.Width;
end;

procedure TfrmMain.pnlLeftCollapseResize(Sender: TObject);
begin
  pnlTopPad1.Top    := 0;
  pnlTopPad1.Height := (pnlLeftCollapse.Height - pnlLeftArrowHolder.Height) div 2;

  pnlLeftArrowHolder.Top := pnlTopPad1.Height;
end;

procedure TfrmMain.pnlRightCollapseResize(Sender: TObject);
begin
  pnlTopPad2.Top    := 0;
  pnlTopPad2.Height := (pnlRightCollapse.Height - pnlRightArrowHolder.Height) div 2;

  pnlRightArrowHolder.Top := pnlTopPad2.Height;
end;

procedure TfrmMain.imgLeftCollapseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  pnlLeftArrowHolder.BevelOuter := bvLowered;
end;

procedure TfrmMain.imgLeftCollapseMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LGlyphIndex: Integer;
begin
  pnlLeftArrowHolder.BevelOuter := bvRaised;

  pnlImageFiles.Visible := not pnlImageFiles.Visible;
  
  if pnlImageFiles.Visible then
  begin
    LGlyphIndex := LEFT_ARROW_INDEX;
  end
  else
  begin
    LGlyphIndex := RIGHT_ARROW_INDEX;
  end;

  with imgLeftCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[LGlyphIndex]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;
end;

procedure TfrmMain.imgRightCollapseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  pnlRightArrowHolder.BevelOuter := bvLowered;
end;

procedure TfrmMain.imgRightCollapseMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LGlyphIndex: Integer;
begin
  pnlRightArrowHolder.BevelOuter := bvRaised;

  pnlFilters.Visible := not pnlFilters.Visible;

  if pnlFilters.Visible then
  begin
    pnlFilters.Left := frmMain.Width - pnlFilters.Width;
    LGlyphIndex := RIGHT_ARROW_INDEX;
  end
  else
  begin
    LGlyphIndex := LEFT_ARROW_INDEX;
  end;

  with imgRightCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[LGlyphIndex]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;
end;

procedure TfrmMain.imgBottomCollapseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  pnlBottomArrowHolder.BevelOuter := bvLowered;
end; 

procedure TfrmMain.imgBottomCollapseMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LGlyphIndex: Integer;
begin
  pnlBottomArrowHolder.BevelOuter := bvRaised;

  pnlOptions.Visible := not pnlOptions.Visible;

  if pnlOptions.Visible then
  begin
    pnlOptions.Top := frmMain.Height - pnlOptions.Height;
    LGlyphIndex    := BOTTOM_ARROW_INDEX;
  end
  else
  begin
    LGlyphIndex := TOP_ARROW_INDEX;
  end;

  with imgBottomCollapse do
  begin
    Bitmap.Assign(Bitmap32List1.Bitmap[LGlyphIndex]);
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := ArrowImageBlend;
    Bitmap.Changed;
  end;
end;

procedure TfrmMain.FileListBox1DblClick(Sender: TObject);
var
  jpg     : TJPEGImage;
  bmp     : TBitmap;
  LExtName: string;
begin
  if FileExists(FileListBox1.FileName) then
  begin
    if FFileName <> FileListBox1.FileName then
    begin
      LExtName := ExtractFileExt(FileListBox1.FileName);
      LExtName := Lowercase(LExtName);

      Screen.Cursor := crHourGlass;
      bmp           := TBitmap.Create;
      try
        if LExtName = '.jpg' then
        begin
          jpg := TJPEGImage.Create;
          try
            jpg.LoadFromFile(FileListBox1.FileName);
            bmp.Width       := jpg.Width;
            bmp.Height      := jpg.Height;
            bmp.PixelFormat := pf24bit;
            bmp.Canvas.Draw(0, 0, jpg);
          finally
            jpg.Free;
          end;
        end
        else if LExtName = '.bmp' then
        begin
          bmp.LoadFromFile(FileListBox1.FileName);
        end;

        if Assigned(FSourceBitmap) then
        begin
          FreeAndNil(FSourceBitmap);
        end;

        FSourceBitmap := TBitmap32.Create;
        FSourceBitmap.Assign(bmp);

        ExecuteFilter;
      finally
        bmp.Free;
        Screen.Cursor := crDefault;
      end;

      FFileName := FileListBox1.FileName;
      SetTitles;
    end;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if frmMain.Width < MIN_WINDOW_WIDTH then
  begin
    frmMain.Width := MIN_WINDOW_WIDTH;
  end;

  if frmMain.Height < MIN_WINDOW_Height then
  begin
    frmMain.Height := MIN_WINDOW_Height;
  end;
end;

procedure TfrmMain.FileListBox1Change(Sender: TObject);
begin
  UpdateThumnail(FileListBox1.FileName);
end;

procedure TfrmMain.ggbrBoxBlurHorzRadiusChange(Sender: TObject);
begin
  lblBoxBlurHorzRadius.Caption := Format('Horizontal Radius: %d', [ggbrBoxBlurHorzRadius.Position]);
end;

procedure TfrmMain.ggbrBoxBlurVertRadiusChange(Sender: TObject);
begin
  lblBoxBlurVertRadius.Caption := Format('Vertical Radius: %d', [ggbrBoxBlurVertRadius.Position]);
end;

procedure TfrmMain.ggbrBoxBlurIterationsChange(Sender: TObject);
begin
  lblBoxBlurIterations.Caption := Format('Iterations: %d', [ggbrBoxBlurIterations.Position]);
end;

procedure TfrmMain.ggbrBoxBlurHorzRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsBoxBlur then
    begin
      TgmBoxBlurFilter(FFilter).HorizontalRadius := ggbrBoxBlurHorzRadius.Position;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrBoxBlurVertRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsBoxBlur then
    begin
      TgmBoxBlurFilter(FFilter).VerticalRadius := ggbrBoxBlurVertRadius.Position;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrBoxBlurIterationsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsBoxBlur then
    begin
      TgmBoxBlurFilter(FFilter).Iterations := ggbrBoxBlurIterations.Position;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrGaussianBlurRadiusChange(Sender: TObject);
var
  LValue: Single;
begin
  LValue := ggbrGaussianBlurRadius.Position / GAUSSIAN_POS_SCALE;

  lblGaussianBlurRadius.Caption := Format('Gaussian Blur Radius: %.1f', [LValue]);
end;

procedure TfrmMain.ggbrGaussianBlurRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsGaussianBlur then
    begin
      TgmGaussianFilter(FFilter).Radius := ggbrGaussianBlurRadius.Position / GAUSSIAN_POS_SCALE;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ChangeFilterClick(Sender: TObject);
begin
  SetFilter;
end;

procedure TfrmMain.cmbbxZoomerChange(Sender: TObject);
begin
  case cmbbxZoomer.ItemIndex of
    0:
      begin
        imgvwDisplay.Scale := 16.0;
      end;

    1:
      begin
        imgvwDisplay.Scale := 8.0;
      end;

    2:
      begin
        imgvwDisplay.Scale := 4.0;
      end;

    3:
      begin
        imgvwDisplay.Scale := 2.0;
      end;

    4:
      begin
        imgvwDisplay.Scale := 1.0;
      end;

    5:
      begin
        imgvwDisplay.Scale := 0.5;
      end;

    6:
      begin
        imgvwDisplay.Scale := 0.25;
      end;

    7:
      begin
        imgvwDisplay.Scale := 0.125;
      end;

    8:
      begin
        imgvwDisplay.Scale := 0.0625;
      end;
  end;
end;

procedure TfrmMain.shpArtFragBorderColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsArtFragmentation then
    begin
      ColorDialog.Color := shpArtFragBorderColor.Brush.Color;
      
      if ColorDialog.Execute then
      begin
        TgmArtFragmentationFilter(FFilter).BorderColor := Color32(ColorDialog.Color);
        shpArtFragBorderColor.Brush.Color              := ColorDialog.Color;

        if not TgmArtFragmentationFilter(FFilter).IsAutoColor then
        begin
          ExecuteFilter;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.chckbxArtFragAutoBorderClick(Sender: TObject);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsArtFragmentation then
    begin
      TgmArtFragmentationFilter(FFilter).IsAutoColor := chckbxArtFragAutoBorder.Checked;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrCyanScaleChange(Sender: TObject);
begin
  lblCyanScale.Caption := 'Cyan: ' + IntToStr(ggbrCyanScale.Position - 100) + '%';
end;

procedure TfrmMain.ggbrCyanScaleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LColorSelector: TgmColorSelector;
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);
      TgmSelectiveColorFilter(FFilter).CyanScales[LColorSelector] := (ggbrCyanScale.Position - 100) / 100;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrMagentaScaleChange(Sender: TObject);
begin
  lblMagentaScale.Caption := 'Magenta: ' + IntToStr(ggbrMagentaScale.Position - 100) + '%';
end;

procedure TfrmMain.ggbrYellowScaleChange(Sender: TObject);
begin
  lblYellowScale.Caption := 'Yellow: ' + IntToStr(ggbrYellowScale.Position - 100) + '%';
end;

procedure TfrmMain.ggbrBlackScaleChange(Sender: TObject);
begin
  lblBlackScale.Caption := 'Black: ' + IntToStr(ggbrBlackScale.Position - 100) + '%';
end;

procedure TfrmMain.ggbrMagentaScaleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LColorSelector: TgmColorSelector;
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);
      TgmSelectiveColorFilter(FFilter).MagentaScales[LColorSelector] := (ggbrMagentaScale.Position - 100) / 100;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrYellowScaleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LColorSelector: TgmColorSelector;
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);
      TgmSelectiveColorFilter(FFilter).YellowScales[LColorSelector] := (ggbrYellowScale.Position - 100) / 100;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrBlackScaleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LColorSelector: TgmColorSelector;
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);
      TgmSelectiveColorFilter(FFilter).BlackScales[LColorSelector] := (ggbrBlackScale.Position - 100) / 100;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.cmbbxSelectiveColorsChange(Sender: TObject);
var
  LColorSelector: TgmColorSelector;
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      LColorSelector := TgmColorSelector(cmbbxSelectiveColors.ItemIndex);

      ggbrCyanScale.Position    := Round(TgmSelectiveColorFilter(FFilter).CyanScales[LColorSelector] * 100 + 100);
      ggbrMagentaScale.Position := Round(TgmSelectiveColorFilter(FFilter).MagentaScales[LColorSelector] * 100 + 100);
      ggbrYellowScale.Position  := Round(TgmSelectiveColorFilter(FFilter).YellowScales[LColorSelector] * 100 + 100);
      ggbrBlackScale.Position   := Round(TgmSelectiveColorFilter(FFilter).BlackScales[LColorSelector] * 100 + 100);
    end;
  end;
end;

procedure TfrmMain.ChangeSelectiveColorMethod(Sender: TObject);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsSelectiveColor then
    begin
      if Sender = rdbtnSelectiveColorRelative then
      begin
        TgmSelectiveColorFilter(FFilter).AdjustMode := gmscamRelative;
      end
      else
      if Sender = rdbtnSelectiveColorAbsolute then
      begin
        TgmSelectiveColorFilter(FFilter).AdjustMode := gmscamAbsolute;
      end;

      ExecuteFilter;
    end;
  end;
end; 

procedure TfrmMain.ggbrMotionBlurAngleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsMotionBlur then
    begin
      TgmMotionBlurFilter(FFilter).Angle := ggbrMotionBlurAngle.Position * (PI / 180);
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrMotionBlurDistanceMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsMotionBlur then
    begin
      TgmMotionBlurFilter(FFilter).Distance := ggbrMotionBlurDistance.Position;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrMotionBlurRotationMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsMotionBlur then
    begin
      TgmMotionBlurFilter(FFilter).Rotation := (ggbrMotionBlurRotation.Position - 180) * (PI / 180);
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrMotionBlurZoomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsMotionBlur then
    begin
      TgmMotionBlurFilter(FFilter).Zoom := ggbrMotionBlurZoom.Position / 100;
      ExecuteFilter;
    end;
  end;
end;

procedure TfrmMain.ggbrMotionBlurAngleChange(Sender: TObject);
begin
  lblMotionBlurAngleValue.Caption := IntToStr(ggbrMotionBlurAngle.Position);
end;

procedure TfrmMain.ggbrMotionBlurDistanceChange(Sender: TObject);
begin
  lblMotionBlurDistanceValue.Caption := IntToStr(ggbrMotionBlurDistance.Position);
end;

procedure TfrmMain.ggbrMotionBlurRotationChange(Sender: TObject);
begin
  lblMotionBlurRotationValue.Caption := IntToStr(ggbrMotionBlurRotation.Position - 180);
end;

procedure TfrmMain.ggbrMotionBlurZoomChange(Sender: TObject);
begin
  lblMotionBlurZoomValue.Caption := IntToStr(ggbrMotionBlurZoom.Position);
end;

procedure TfrmMain.chckbxMotionBlurWrapEdgesClick(Sender: TObject);
begin
  if Assigned(FFilter) then
  begin
    if FFilterSelector = gmfsMotionBlur then
    begin
      TgmMotionBlurFilter(FFilter).IsWrapEdges := chckbxMotionBlurWrapEdges.Checked;
      ExecuteFilter;
    end;
  end;
end;

end.
