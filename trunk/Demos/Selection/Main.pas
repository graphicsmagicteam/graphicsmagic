unit Main;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 *   x2nie - Fathony Luthfillah < x2nie@yahoo.com >
 *     Given us the code for how to add an additional Paint-Stage
 *     that above all layers.
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, ComCtrls, ToolWin, Menus, ExtCtrls, ImgList, StdCtrls,
{ Graphics32 Lib }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmMagneticLasso, gmRegions, gmTypes,
{ Project Lib }
  gmSelection;

type
  TfrmMain = class(TForm)
    pnlToolBox: TPanel;
    MainMenu1: TMainMenu;
    mnhdFile: TMenuItem;
    mnitmOpenImage: TMenuItem;
    N1: TMenuItem;
    mnitmExit: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    pnlSelectionToolsBox: TPanel;
    Panel1: TPanel;
    tlbrSelectionTools: TToolBar;
    ToolButton1: TToolButton;
    tlbtnSelectionController: TToolButton;
    tlbtnSingleRowMarquee: TToolButton;
    tlbtnSingleColumnMarquee: TToolButton;
    tlbtnRectangularMarquee: TToolButton;
    tlbtnRoundRectangularMarquee: TToolButton;
    tlbtnEllipticalMarquee: TToolButton;
    tlbtnPolygonalMarquee: TToolButton;
    tlbtnRegularPolygonMarquee: TToolButton;
    tlbtnLassoMarquee: TToolButton;
    tlbtnMagicWand: TToolButton;
    ImageList1: TImageList;
    pnlSelectionOptionsBox: TPanel;
    Panel3: TPanel;
    tlbtSelectionOptions: TToolBar;
    ToolButton2: TToolButton;
    tlbtnNewSelection: TToolButton;
    tlbtnAddSelection: TToolButton;
    tlbtnSubtractSelection: TToolButton;
    tlbtnIntersectSelection: TToolButton;
    tlbtnExcludeOverlapSelection: TToolButton;
    ToolButton8: TToolButton;
    tlbtnCommitSelection: TToolButton;
    tlbtnDeselect: TToolButton;
    tlbtnDeleteSelection: TToolButton;
    pnlMultiTaskBox: TPanel;
    pnlMultiTaskTitle: TPanel;
    tlbtnPencil: TToolButton;
    lblMultiTask: TLabel;
    edtMultiTask: TEdit;
    updwnMultiTask: TUpDown;
    pnlCommonOptions: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    shpCurrentColor: TShape;
    rdgrpFillMode: TRadioGroup;
    Label2: TLabel;
    cmbbxZoom: TComboBox;
    lblFeatherRadius: TLabel;
    edtFeatherRadius: TEdit;
    updwnFeatherRadius: TUpDown;
    mnhdImage: TMenuItem;
    mnitmInvert: TMenuItem;
    mnhdSelect: TMenuItem;
    mnitmSelectAll: TMenuItem;
    mnitmInvertSelection: TMenuItem;
    mnitmColorRangeSelection: TMenuItem;
    mnhdEdit: TMenuItem;
    mnitmTransform: TMenuItem;
    mnitmScaleTransformation: TMenuItem;
    mnitmRotateTransformation: TMenuItem;
    mnitmDistortTransformation: TMenuItem;
    N2: TMenuItem;
    mnitmHorizFlip: TMenuItem;
    mnitmVertFlip: TMenuItem;
    tlbtnMagneticLasso: TToolButton;
    chckbxInteractive: TCheckBox;
    Panel2: TPanel;
    stsbrStatusInfo: TStatusBar;
    imgWorkArea: TImgView32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenImageClick(Sender: TObject);
    procedure ChangeMarqueeTools(Sender: TObject);
    procedure ChangeMarqueeMode(Sender: TObject);
    procedure tlbtnCommitSelectionClick(Sender: TObject);
    procedure tlbtnDeselectClick(Sender: TObject);
    procedure tlbtnDeleteSelectionClick(Sender: TObject);
    procedure updwnMultiTaskClick(Sender: TObject; Button: TUDBtnType);
    procedure cmbbxZoomChange(Sender: TObject);
    procedure updwnFeatherRadiusMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mnitmInvertClick(Sender: TObject);
    procedure mnitmSelectAllClick(Sender: TObject);
    procedure mnitmInvertSelectionClick(Sender: TObject);
    procedure mnitmColorRangeSelectionClick(Sender: TObject);
    procedure FlipImageClick(Sender: TObject);
    procedure ChangeTransformMode(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
    procedure chckbxInteractiveClick(Sender: TObject);
    procedure mnitmExitClick(Sender: TObject);
    procedure imgWorkAreaDblClick(Sender: TObject);
    procedure imgWorkAreaResize(Sender: TObject);
    procedure imgWorkAreaScroll(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FDrawing          : Boolean;       // if the left mouse button is pressed, to begin drawing
    FIsImageOpened    : Boolean;
    FDoubleClicked    : Boolean;
    FXActual          : Integer;       // the actual X coordinate on the layer
    FYActual          : Integer;       // the actual Y coordinate on the layer
    FMarqueeX         : Integer;       // the actual X coordinate on the selection
    FMarqueeY         : Integer;       // the actual Y coordinate on the selection
    FStartPoint       : TPoint;
    FEndPoint         : TPoint;
    FLayerTopLeft     : TPoint;
    FDrawingBasePoint : TPoint;        // used for holding the last mouse position
    FCurrentLayer     : TBitmapLayer;  // pointer to current layer

{ Selection }
    FHandlePaintStage     : PPaintStage;
    FHandleStageIndex     : Integer;

    FMarqueeTool          : TgmMarqueeTools;
    FMarqueeMode          : TgmMarqueeMode;
    FSides                : Integer;
    FCornerRadius         : Integer;
    FMagicWandTolerance   : Byte;

    FMarqueeDrawingState  : TgmDrawingState;     // state of the Selection Tool
    FMarqueeDrawingHandle : TgmDrawingHandle;    // the handle of the mouse which over it

    FSelection            : TgmSelection;
    FRegion               : TgmRegion;

{ Magnetic Lasso }
    FMagneticLasso      : TgmMagneticLasso;
    FMagneticLassoLayer : TBitmapLayer;

{ Selection Transformation }
    FSelectionTranslateTarget : TgmTranslateTarget;
    FSelectionTransformation  : TgmSelectionTransformation;
    FTransformHandle          : TgmDrawingHandle;
    FRotateRadiansInMouseDown : Extended;
    FRotateRadiansInMouseMove : Extended;
    FTransformOldVertices     : array of TPoint;
    FTransformNewVertices     : array of TPoint;
    
{ Pencil }
    FPencilbitmap    : TBitmap32;
    Felozox, Felozoy : Integer;
    Ftavolsag        : Real;
    FPenWidth        : Integer;

    // open an image and shown it on new layer
    function OpenImageWithNewLayer(const AFileName: string): Boolean;

    // calculate the coordinates of the mouse which is on the layer
    procedure CalcLayerCoord(X, Y: Integer);

    { Calculate the coordinates of the mouse which is on the selection.
      Note that, we need call the CalcLayerCoord() first for get the
      right coordinates. }
    procedure CalcSelectionCoord;

    // get the top left coordinates of the current layer
    function GetLayerTopLeft: TPoint;

    procedure ChangeImageCursorByMarqueeTools;

    procedure CommitSelection;
    procedure CancelSelection;
    procedure DeleteSelection;
    procedure MakeSelectionInverse;
    procedure MakeSelectionFeather;
    procedure CreateSelectionForAll;
    procedure CreateSelectionByColorRange;
    procedure FinishPolygonalSelection;
    procedure ShowProcessedSelection;

    procedure UpdateMarqueeOptions;  // update options of marquee tools

    // adapted from RebuildBrush() by Zoltan in gr32PaintDemo3
    procedure RebuildPencil;
    
    // adapted from BrushLine() by Zoltan in gr32PaintDemo3
    procedure PencilLine(const xStart, yStart, xEnd, yEnd, Distance: Integer;
      ToBitmap: TBitmap32);

    procedure ImagePaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);   

{ Magnetic Lasso }
    procedure CreateLassoLayer;
    procedure FinishMagneticLasso;

{ Selection Transform }
    procedure FinishTransformation;

{ Mouse Event Handlers }

    // a callback function for OnPixelCombine event
    procedure LayerNotXorBlend(F: TColor32; var B: TColor32; M: TColor32);

    { For Selection }
    procedure MarqueeToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure MarqueeToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MarqueeToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    { Transform }
    procedure TransformSelectionMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure TransformSelectionMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
      
    procedure TransformSelectionMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    { For Pencil Tool }
    procedure PencilMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure PencilMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure PencilMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    // connect mouse events to image
    procedure ConnectSelectionMouseEvents;
    procedure ConnectTransformMouseEvents;
    procedure ConnectPencilMouseEvents;
  public
    property CurrentLayer : TBitmapLayer read FCurrentLayer;
    property Selection    : TgmSelection read FSelection;
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Standard }
  Math, JPEG,
{ externals }
  GR32_Add_BlendModes,
  LineLibrary,
{ GraphicsMagic Lib }
  gmPaintFuncs,
  gmImageProcessFuncs,
  gmMath,
  gmGUIFuncs,
  gmConstants,
{ Demo Dialogs }
  ColorRangeSelectionDlg;

{$R *.dfm}

const
  PAINT_STAGE_ZERO_PARAMETER = 100;
  HANDLE_STAGE_PARAMETER     = 101;         

// open an image and show it on new layer
function TfrmMain.OpenImageWithNewLayer(const AFileName: string): Boolean;
var
  LRect   : TRect;
  LTopLeft: TPoint;
  LJPG    : TJPEGImage;
  LBmp    : TBitmap;
  LFileExt: String;
begin
  if AFileName <> '' then
  begin
    if FileExists(AFileName) then
    begin
      // create layer
      FCurrentLayer := TBitmapLayer.Create(imgWorkArea.Layers);

      LFileExt := LowerCase(ExtractFileExt(AFileName));

      if LFileExt = '.bmp' then
      begin
        FCurrentLayer.Bitmap.LoadFromFile(AFileName);
      end
      else
      begin
        if LFileExt = '.jpg' then
        begin
          LJPG := TJPEGImage.Create;
          LBmp := TBitmap.Create;
          try
            LJPG.LoadFromFile(AFileName);
            LBmp.Assign(LJPG);
            LBmp.PixelFormat := pf24bit;
            FCurrentLayer.Bitmap.Assign(LBmp);
          finally
            LJPG.Free;
            LBmp.Free;
          end;
        end;
      end;

      FCurrentLayer.Bitmap.DrawMode       := dmCustom;
      FCurrentLayer.Bitmap.OnPixelCombine := BlendMode.NormalBlend;

      imgWorkArea.Bitmap.SetSize(FCurrentLayer.Bitmap.Width,
                                 FCurrentLayer.Bitmap.Height);
                                 
      imgWorkArea.Bitmap.DrawMode := dmBlend;
      imgWorkArea.Bitmap.Clear($00000000);

      // get the location of the bitmap on the image
      LRect := imgWorkArea.GetBitmapRect;
      
      // convert the control coordinates to bitmap coordinates
      LTopLeft := imgWorkArea.ControlToBitmap( Point(LRect.Left, LRect.Top) );

      FCurrentLayer.Location := FloatRect(LTopLeft.X, LTopLeft.Y,
                                          FCurrentLayer.Bitmap.Width,
                                          FCurrentLayer.Bitmap.Height);

      FCurrentLayer.Scaled := True;
      FCurrentLayer.Bitmap.Changed;

      FLayerTopLeft := GetLayerTopLeft;
      Result        := True;
    end
    else
    begin
      MessageDlg('The file is not existed.', mtError, [mbOK], 0);
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

// calculate the coordinates of the mouse which is on the layer
procedure TfrmMain.CalcLayerCoord(X, Y: Integer);
begin
  if Assigned(FCurrentLayer) then
  begin
    FXActual  := X - Round(FCurrentLayer.GetAdjustedLocation.Left);
    FYActual  := Y - Round(FCurrentLayer.GetAdjustedLocation.Top);
    
    if imgWorkArea.Scale <> 1 then
    begin
      FXActual := MulDiv( FXActual, FCurrentLayer.Bitmap.Width,  Round(FCurrentLayer.Bitmap.Width  * imgWorkArea.Scale) );
      FYActual := MulDiv( FYActual, FCurrentLayer.Bitmap.Height, Round(FCurrentLayer.Bitmap.Height * imgWorkArea.Scale) );
    end;
  end;
end;

{ Calculate the coordinates of the mouse which is on the selection.
  Note that, we need call the CalcLayerCoord() first for get the
  right coordinates. }
procedure TfrmMain.CalcSelectionCoord;
begin
  if Assigned(FSelection) then
  begin
    { The selection X coordinate is (FXActual - FSelection.FMaskBorderStart.X),
      because of the user may resize the selection, so we need to scale the
      result X coordinates. The Y coordinate is calculated with the same manner.
     }
      
    FMarqueeX := MulDiv(FXActual - FSelection.MaskBorderStart.X,
                        FSelection.CutOriginal.Width - 1,
                        FSelection.Foreground.Width - 1 );
                        
    FMarqueeY := MulDiv(FYActual - FSelection.MaskBorderStart.Y,
                        FSelection.CutOriginal.Height - 1,
                        FSelection.Foreground.Height - 1);
  end;
end;

// get the top left coordinates of the current layer
function TfrmMain.GetLayerTopLeft: TPoint;
begin
  if FCurrentLayer <> nil then
  begin
    Result.X := Round(FCurrentLayer.GetAdjustedLocation.Left);
    Result.Y := Round(FCurrentLayer.GetAdjustedLocation.Top);
  end;
end;

procedure TfrmMain.ChangeImageCursorByMarqueeTools;
begin
  case FMarqueeMode of
    mmNew:
      begin
        if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical,
                            mtSingleRow, mtSingleColumn, mtRegularPolygon] then
        begin
          imgWorkArea.Cursor := crCross;
        end
        else
        if FMarqueeTool = mtPolygonal then
        begin
          imgWorkArea.Cursor := crPolygonSelection;
        end
        else
        if FMarqueeTool = mtLasso then
        begin
          imgWorkArea.Cursor := crLassoSelection;
        end
        else
        if FMarqueeTool = mtMagicWand then
        begin
          imgWorkArea.Cursor := crMagicWand;
        end;
      end;
      
    mmAdd:
      begin
        if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical,
                            mtSingleRow, mtSingleColumn, mtRegularPolygon] then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crCrossAdd;
          end
          else
          begin
            imgWorkArea.Cursor := crCross;
          end;
        end
        else
        if FMarqueeTool = mtPolygonal then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crPolygonAdd;
          end
          else
          begin
            imgWorkArea.Cursor := crPolygonSelection;
          end;
        end
        else
        if FMarqueeTool = mtLasso then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crLassoAdd;
          end
          else
          begin
            imgWorkArea.Cursor := crLassoSelection;
          end;
        end
        else
        if FMarqueeTool = mtMagicWand then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crMagicWandAdd;
          end
          else
          begin
            imgWorkArea.Cursor := crMagicWand;
          end;
        end;
      end;
      
    mmSubtract:
      begin
        if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical,
                            mtSingleRow, mtSingleColumn, mtRegularPolygon] then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crCrossSub;
          end
          else
          begin
            imgWorkArea.Cursor := crCross;
          end;
        end
        else
        if FMarqueeTool = mtPolygonal then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crPolygonSub;
          end
          else
          begin
            imgWorkArea.Cursor := crPolygonSelection;
          end;
        end
        else
        if FMarqueeTool = mtLasso then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crLassoSub;
          end
          else
          begin
            imgWorkArea.Cursor := crLassoSelection;
          end;
        end
        else
        if FMarqueeTool = mtMagicWand then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crMagicWandSub;
          end
          else
          begin
            imgWorkArea.Cursor := crMagicWand;
          end;
        end;
      end;
      
    mmIntersect:
      begin
        if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical,
                            mtSingleRow, mtSingleColumn, mtRegularPolygon] then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crCrossIntersect;
          end
          else
          begin
            imgWorkArea.Cursor := crCross;
          end;
        end
        else
        if FMarqueeTool = mtPolygonal then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crPolygonIntersect;
          end
          else
          begin
            imgWorkArea.Cursor := crPolygonSelection;
          end;
        end
        else
        if FMarqueeTool = mtLasso then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crLassoIntersect;
          end
          else
          begin
            imgWorkArea.Cursor := crLassoSelection;
          end;
        end
        else
        if FMarqueeTool = mtMagicWand then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crMagicWandIntersect;
          end
          else
          begin
            imgWorkArea.Cursor := crMagicWand;
          end;
        end;
      end;
      
    mmExcludeOverlap:
      begin
        if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical,
                            mtSingleRow, mtSingleColumn, mtRegularPolygon] then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crCrossInterSub;
          end
          else
          begin
            imgWorkArea.Cursor := crCross;
          end;
        end
        else
        if FMarqueeTool = mtPolygonal then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crPolygonInterSub;
          end
          else
          begin
            imgWorkArea.Cursor := crPolygonSelection;
          end;
        end
        else
        if FMarqueeTool = mtLasso then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crLassoInterSub;
          end
          else
          begin
            imgWorkArea.Cursor := crLassoSelection;
          end;
        end
        else
        if FMarqueeTool = mtMagicWand then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crMagicWandInterSub;
          end
          else
          begin
            imgWorkArea.Cursor := crMagicWand;
          end;
        end;
      end;
  end;

  if FMarqueeTool = mtMoveResize then
  begin
    imgWorkArea.Cursor := crMoveSelection;
  end
  else
  if FMarqueeTool = mtMagneticLasso then
  begin
    imgWorkArea.Cursor := crMagneticLasso;
  end
  else
  if FMarqueeTool = mtNone then
  begin
    imgWorkArea.Cursor := crCross;
  end;
end;

procedure TfrmMain.CommitSelection;
begin
  if Assigned(FSelection) then
  begin
    if FSelection.HasShadow then
    begin
      Screen.Cursor := crHourGlass;
      try
        FSelection.IsAnimated := False;  

        FCurrentLayer.Bitmap.Changed;
        FreeAndNil(FSelection);
        ChangeImageCursorByMarqueeTools;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmMain.CancelSelection;
begin
  if Assigned(FSelection) then
  begin
    if FSelection.HasShadow then
    begin
      Screen.Cursor := crHourGlass;
      try
        FSelection.IsAnimated := False;

        FCurrentLayer.Bitmap.Assign(FSelection.SourceBitmap);
        FCurrentLayer.Bitmap.Changed;

        FreeAndNil(FSelection);
        ChangeImageCursorByMarqueeTools;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmMain.DeleteSelection;
begin
  if Assigned(FSelection) then
  begin
    if FSelection.HasShadow then
    begin
      Screen.Cursor := crHourGlass;
      try
        FSelection.IsAnimated := False;                  

        FCurrentLayer.Bitmap.Assign(FSelection.Background);
        FCurrentLayer.Bitmap.Changed;

        FreeAndNil(FSelection);
        ChangeImageCursorByMarqueeTools;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmMain.MakeSelectionInverse;
begin
  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;       

    FSelection.InvertSelection;
    FSelection.GetActualMaskBorder;  // get border of the selection

    // if there is mask shadow, process the inverted selection
    if FSelection.HasShadow then
    begin
      FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
      FSelection.GetForeground;         // get foreground of the selection
      FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

      // get background of the selection
      case rdgrpFillMode.ItemIndex of
        0:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(shpCurrentColor.Brush.Color), [csRed, csGreen, csBlue] );
          end;

        1:
          begin
            FSelection.GetBackgroundWithTransparent;
          end;
      end;

      FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
      FCurrentLayer.Bitmap.Changed;

      FSelection.IsAnimated := True;         
    end
    else
    begin
      // if there is no mask shadow, delete the selection
      MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);

      FCurrentLayer.Bitmap.Assign(FSelection.SourceBitmap);
      FCurrentLayer.Bitmap.Changed;

      FreeAndNil(FSelection);
    end;
  end;
end;

procedure TfrmMain.MakeSelectionFeather;
begin
  if Assigned(FSelection) then
  begin
    // feathering selection
    FSelection.FeatherRadius := updwnFeatherRadius.Position;

    FSelection.GetActualMaskBorder;    // recalculate the cut region
    FSelection.CutRegionFromOriginal;  // cut the region
    FSelection.GetForeground;          // get the foreground of the selection
    FSelection.GetMarchingAntsLines;   // get Marching Ants lines

    FSelection.Background.Assign(FSelection.SourceBitmap);
    
    // get backgound of the selection
    case rdgrpFillMode.ItemIndex of
      0:
        begin
          FSelection.GetBackgroundWithFilledColor( Color32(shpCurrentColor.Brush.Color),
                                                   [csRed, csGreen, csBlue] );
        end;
        
      1:
        begin
          FSelection.GetBackgroundWithTransparent;
        end;
    end;

    FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
    FCurrentLayer.Bitmap.Changed;
    
    FSelection.IsFeathered := True;
  end;
end;

procedure TfrmMain.CreateSelectionForAll;
begin
  // if there is no selection, create one
  if FSelection = nil then
  begin
    FSelection := TgmSelection.Create(imgWorkArea, FCurrentLayer.Bitmap);
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;    
    
    FSelection.SelectAll;
    FSelection.GetActualMaskBorder;   // get the border of the selection
    FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
    FSelection.GetForeground;         // get foreground of the selection
    FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

    // get the background of the selection.
    case rdgrpFillMode.ItemIndex of
      0:
        begin
          FSelection.GetBackgroundWithFilledColor( Color32(shpCurrentColor.Brush.Color),
                                                   [csRed, csGreen, csBlue] );
        end;
        
      1:
        begin
          FSelection.GetBackgroundWithTransparent;
        end;
    end;

    FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
    FCurrentLayer.Bitmap.Changed;
    
    FSelection.IsAnimated        := True;    
    tlbtnCommitSelection.Enabled := True;
    tlbtnDeselect.Enabled        := True;
    tlbtnDeleteSelection.Enabled := True;
  end;
end;

procedure TfrmMain.CreateSelectionByColorRange;
begin
  // if there is no selection, create one
  if FSelection = nil then
  begin
    FSelection := TgmSelection.Create(imgWorkArea, FCurrentLayer.Bitmap);
  end;

  if Assigned(FSelection) then
  begin
    FSelection.CreateColorRangeRGN(frmColorRangeSelection.FSourceBitmap,
                                   frmColorRangeSelection.FSampledColor,
                                   frmColorRangeSelection.FFuzziness);

    FSelection.GetActualMaskBorder;  // get the border of the selection
    
    // if there is mask shadow, process the inverted selection
    if FSelection.HasShadow then
    begin
      FSelection.Background.Assign(FSelection.SourceBitmap);
      FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
      FSelection.GetForeground;         // get foreground of the selection
      FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

      // get the background of the selection
      case rdgrpFillMode.ItemIndex of
        0:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(shpCurrentColor.Brush.Color), [csRed, csGreen, csBlue] );
          end;
          
        1:
          begin
            FSelection.GetBackgroundWithTransparent;
          end;
      end;

      FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
      FCurrentLayer.Bitmap.Changed;
    end
    else
    begin
      // if there is no mask shadow, delete the selection
      MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);

      FCurrentLayer.Bitmap.Assign(FSelection.SourceBitmap);
      FCurrentLayer.Bitmap.Changed;

      FreeAndNil(FSelection);
    end;
  end;
end;

procedure TfrmMain.FinishPolygonalSelection;
var
  LPolygonalRegion: TgmPolygonalRegion;
begin
  // finish the Polygonal selection
  if Assigned(FRegion) then
  begin
    if FRegion.RegionStyle = gmrsPolygonal then
    begin
      LPolygonalRegion := TgmPolygonalRegion(FRegion);

      if not LPolygonalRegion.IsRegionDefineCompleted then
      begin
        // force to close the polygonal region
        LPolygonalRegion.ClosePolgonalRegion;
      end;

      if LPolygonalRegion.IsValidRegion then
      begin
        // if there is no selection, create one
        if FSelection = nil then
        begin
          FSelection := TgmSelection.Create(imgWorkArea, FCurrentLayer.Bitmap);

          FSelection.FeatherRadius := updwnFeatherRadius.Position;
          FSelection.IsFeathered   := (FSelection.FeatherRadius > 0);
        end;

        FSelection.CreateCustomRGN(LPolygonalRegion.Region, FMarqueeMode);

        FSelection.GetActualMaskBorder;   // get the border of the selection
        FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
        FSelection.GetForeground;         // get foreground of the selection
        FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

        // get the background of the selection
        FSelection.Background.Assign(FCurrentLayer.Bitmap);
        case rdgrpFillMode.ItemIndex of
          0:
            begin
              FSelection.GetBackgroundWithFilledColor( Color32(shpCurrentColor.Brush.Color),
                                                       [csRed, csGreen, csBlue] );
            end;

          1:
            begin
              FSelection.GetBackgroundWithTransparent;
            end;
        end;
      end;

      FreeAndNil(FRegion);
    end;

    // if there is no mask shadow, delete the selection
    if FSelection.HasShadow = False then
    begin
      FCurrentLayer.Bitmap.Assign(FSelection.SourceBitmap);
      FCurrentLayer.Bitmap.Changed;
      FreeAndNil(FSelection);

      FSelection.IsAnimated        := False;      
      tlbtnCommitSelection.Enabled := False;
      tlbtnDeselect.Enabled        := False;
      tlbtnDeleteSelection.Enabled := False;
    end
    else
    begin
      FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
      FCurrentLayer.Bitmap.Changed;

      FSelection.IsAnimated        := True;    
      tlbtnCommitSelection.Enabled := True;
      tlbtnDeselect.Enabled        := True;
      tlbtnDeleteSelection.Enabled := True;
    end;
  end;
end;

procedure TfrmMain.ShowProcessedSelection;
begin
  if Assigned(FSelection) then
  begin
    { The actual process is on the FCutOriginal of the selection, we call the
      GetForeground() in order to resize the FCutOriginal, and get the resized
      foreground of the selection. }
      
    FSelection.GetForeground;
    FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
    FCurrentLayer.Bitmap.Changed;
    imgWorkArea.Update;
    FSelection.DrawMarchingAnts;  
  end;
end;

// update options of marquee tools
procedure TfrmMain.UpdateMarqueeOptions;
var
  LHintStr: string;
begin
  case FMarqueeTool of
    mtMagicWand:
      begin
        LHintStr                  := 'Set range when sampling color';
        lblMultiTask.Caption      := 'Tolerance:';
        pnlMultiTaskTitle.Caption := 'Magic Wand Options';
        updwnMultiTask.Min        := 0;
        updwnMultiTask.Max        := 100;
        updwnMultiTask.Position   := FMagicWandTolerance;
      end;

    mtRegularPolygon:
      begin
        LHintStr                  := 'Set number of sides';
        lblMultiTask.Caption      := 'Sides:';
        pnlMultiTaskTitle.Caption := 'Regular Polygon Marquee Options';
        updwnMultiTask.Min        := 3;
        updwnMultiTask.Max        := 100;
        updwnMultiTask.Position   := FSides;
      end;
      
    mtRoundRectangular:
      begin
        LHintStr                  := 'Set radius of rounded corners';
        lblMultiTask.Caption      := 'Radius:';
        pnlMultiTaskTitle.Caption := 'Rounded-Corner Rectangular Marquee Options';
        updwnMultiTask.Min        := 0;
        updwnMultiTask.Max        := 1000;
        updwnMultiTask.Position   := FCornerRadius;
      end;
  end;

  lblMultiTask.Hint   := LHintStr;
  edtMultiTask.Hint   := LHintStr;
  updwnMultiTask.Hint := LHintStr;

  tlbtnDeselect.Enabled        := (FSelection <> nil);
  tlbtnCommitSelection.Enabled := tlbtnDeselect.Enabled;
  tlbtnDeleteSelection.Enabled := tlbtnDeselect.Enabled;

  pnlMultiTaskBox.Visible := (FMarqueeTool in [mtMagicWand, mtRegularPolygon, mtRoundRectangular]);
end;

// Adapted from RebuildBrush() by Zoltan in gr32PaintDemo3
procedure TfrmMain.RebuildPencil;
begin
  FPencilBitmap.SetSize(FPenWidth * 2, FPenWidth * 2);
  FPencilBitmap.Clear(clRed32);

  FeatheredCircleAlpha(FPencilBitmap,
                       FPencilBitmap.Width div 2,
                       FPencilBitmap.Height div 2,
                       FPenWidth div 2, 5);
end;

// Adapted from BrushLine() by Zoltan in gr32PaintDemo3.
procedure TfrmMain.PencilLine(const xStart, yStart, xEnd, yEnd, Distance: Integer;
  ToBitmap: TBitmap32);
var
  a,b        :  Integer;  // displacements in x and y
  d          :  Integer;  // decision variable
  diag_inc   :  Integer;  // d's increment for diagonal steps
  dx_diag    :  Integer;  // diagonal x step for next pixel
  dx_nondiag :  Integer;  // nondiagonal x step for next pixel
  dy_diag    :  Integer;  // diagonal y step for next pixel
  dy_nondiag :  Integer;  // nondiagonal y step for next pixel
  i          :  Integer;  // loop index
  nondiag_inc:  Integer;  // d's increment for nondiagonal steps
  swap       :  Integer;  // temporary variable for swap
  x,y        :  Integer;  // current x and y coordinates
begin {DrawLine}
  x := xStart;              // line starting point
  y := yStart;

  // Determine drawing direction and step to the next pixel.
  a := xEnd - xStart;       // difference in x dimension
  b := yEnd - yStart;       // difference in y dimension

  // Determine whether end point lies to right or left of start point.
  if a < 0 then               // drawing towards smaller x values?
  begin
    a       := -a;            // make 'a' positive
    dx_diag := -1
  end
  else
  begin
    dx_diag := 1;
  end;

  // Determine whether end point lies above or below start point.
  if b < 0 then               // drawing towards smaller y values?
  begin
    b       := -b;            // make 'b' positive
    dy_diag := -1
  end
  else
  begin
    dy_diag := 1;
  end;

  // Identify octant containing end point.
  if a < b then
  begin
    swap       := a;
    a          := b;
    b          := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end
  else
  begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;

  d           := b + b - a;  // initial value for d is 2*b - a
  nondiag_inc := b + b;      // set initial d increment values
  diag_inc    := b + b - a - a;

  for i := 0 to a do    // draw the a+1 pixels
  begin
    if Ftavolsag >= distance then
    begin
      FPencilBitmap.DrawTo(tobitmap, x - FPenWidth, y - FPenWidth);

      Ftavolsag := 0;
      Felozox   := x;
      Felozoy   := y;
    end;

    if d < 0 then              // is midpoint above the line?
    begin                      // step nondiagonally
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc   // update decision variable
    end
    else
    begin                    // midpoint is above the line; step diagonally
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc
    end;

    Ftavolsag := (  sqrt( sqr(x - Felozox) + sqr(y - Felozoy) )  );
  end;
end;

procedure TfrmMain.ImagePaintStage(Sender: TObject; Buffer: TBitmap32;   
  StageNum: Cardinal);
var
  P    : PPaintStage;
  LRect: TRect;
begin
  // draw background
  if (Buffer.Height > 0) and (Buffer.Width > 0) then
  begin
    P := imgWorkArea.PaintStages[StageNum];

    if P.Parameter = PAINT_STAGE_ZERO_PARAMETER then
    begin
      Buffer.Clear($FFC0C0C0);

      // draw thin border, written by Andre Felix Miertschink
      LRect := imgWorkArea.GetBitmapRect;
      DrawCheckerboardPattern(Buffer, LRect);

      LRect.Left   := LRect.Left   - 1;
      LRect.Top    := LRect.Top    - 1;
      LRect.Right  := LRect.Right  + 1;
      LRect.Bottom := LRect.Bottom + 1;

      Buffer.FrameRectS(LRect, clBlack32);
    end
    else
    begin
      if P.Parameter = HANDLE_STAGE_PARAMETER then
      begin
        if Assigned(FSelectionTransformation) then
        begin
          FSelectionTransformation.DrawOutline(Buffer.Canvas, pmNotXor);
        end
        else
        begin
          if Assigned(FSelection) then
          begin
            if FMarqueeTool = mtMoveResize then
            begin
              FSelection.DrawMarchingAntsBorder(Buffer.Canvas, True);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.CreateLassoLayer;
var
  LHalfWidth, LHalfHeight: Single;
  LCenterPoint           : TPoint;
begin
  if not Assigned(FMagneticLassoLayer) then
  begin
    FMagneticLassoLayer := TBitmapLayer.Create(imgWorkArea.Layers);

    FMagneticLassoLayer.Bitmap.DrawMode       := dmCustom;
    FMagneticLassoLayer.Bitmap.OnPixelCombine := LayerNotXorBlend;

    FMagneticLassoLayer.Bitmap.SetSize(imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);
    FMagneticLassoLayer.Bitmap.Clear($00000000);

    LHalfWidth  := FMagneticLassoLayer.Bitmap.Width  / 2;
    LHalfHeight := FMagneticLassoLayer.Bitmap.Height / 2;

    // get the center point of the viewport of the TImage32/TImgView32 and
    // convert it from control space to bitmap space
    with imgWorkArea.GetViewportRect do
    begin
      LCenterPoint := imgWorkArea.ControlToBitmap(
        Point( (Right + Left) div 2, (Top + Bottom) div 2 )  );
    end;
    
    // setting the location of the layer
    FMagneticLassoLayer.Location := FloatRect(LCenterPoint.X - LHalfWidth,
                                              LCenterPoint.Y - LHalfHeight,
                                              LCenterPoint.X + LHalfWidth,
                                              LCenterPoint.Y + LHalfHeight);

    FMagneticLassoLayer.Scaled := True;
  end;
end;

procedure TfrmMain.FinishMagneticLasso;
begin
  if Assigned(FMagneticLasso) then
  begin
    if FMagneticLasso.IsConnected then
    begin
      // if there is no selection, create one
      if not Assigned(FSelection) then
      begin
        FSelection := TgmSelection.Create(imgWorkArea, FCurrentLayer.Bitmap);
      
        FSelection.FeatherRadius := updwnFeatherRadius.Position;
        FSelection.IsFeathered   := (FSelection.FeatherRadius > 0);
      end;

      FSelection.CreateCustomRGN(FMagneticLasso.CurveRegion, FMarqueeMode);

      FSelection.GetActualMaskBorder;   // get the border of the selection
      FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
      FSelection.GetForeground;         // get foreground of the selection
      FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

      // get the background of the selection
      case rdgrpFillMode.ItemIndex of
        0:
          begin
            FSelection.GetBackgroundWithFilledColor( Color32(shpCurrentColor.Brush.Color),
                                                     [csRed, csGreen, csBlue] );
          end;
        
        1:
          begin
            FSelection.GetBackgroundWithTransparent;
          end;
      end;

      // if there is no mask shadow, delete the selection
      if FSelection.HasShadow = False then
      begin
        // don't know why we could not use the Assign method here.
        FCurrentLayer.Bitmap.Draw(0, 0, FSelection.SourceBitmap);
        FCurrentLayer.Bitmap.Changed;
        FreeAndNil(FSelection);
      
        FSelection.IsAnimated        := False;    
        tlbtnCommitSelection.Enabled := False;
        tlbtnDeselect.Enabled        := False;
        tlbtnDeleteSelection.Enabled := False;
      end
      else
      begin
        FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
        FCurrentLayer.Bitmap.Changed;

        FSelection.IsAnimated        := True;      
        tlbtnCommitSelection.Enabled := True;
        tlbtnDeselect.Enabled        := True;
        tlbtnDeleteSelection.Enabled := True;
      end;
    end;

    FreeAndNil(FMagneticLasso);
  end;

  if Assigned(FMagneticLassoLayer) then
  begin
    FreeAndNil(FMagneticLassoLayer);
  end;
end;

procedure TfrmMain.FinishTransformation;
var
  LMsgDlgResult: Integer;
begin
  if FSelectionTransformation <> nil then
  begin
    LMsgDlgResult := MessageDlg('Apply the transformation?', mtInformation,
                                [mbYes, mbNo, mbCancel], 0);

    case LMsgDlgResult of
      mrYes:
        begin
          FSelectionTransformation.AcceptTransform;
        end;
        
      mrNo:
        begin
          FSelectionTransformation.CancelTransform;
          ShowProcessedSelection;
        end;
    end;

    if LMsgDlgResult in [mrYes, mrNo] then
    begin
      FSelectionTransformation.Free;
      FSelectionTransformation := nil;

      if tlbtnPencil.Down then
      begin
        ConnectPencilMouseEvents;
      end
      else
      begin
        ConnectSelectionMouseEvents;
      end;
    end;
  end;
end;

// a callback function for OnPixelCombine event
procedure TfrmMain.LayerNotXorBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LBlendColor: TColor32;
begin
  if F <> $00000000 then
  begin
    LBlendColor := not ($FF7F7F7F xor B);
    B           := LBlendColor or $FF000000;
  end;
end;

{ Selection Event Handlers }
 
procedure TfrmMain.MarqueeToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LBmpRect: TRect;
begin
  // calculate the layer coordinates of the mouse
  CalcLayerCoord(X, Y);
  
{ Mouse left button down }

  if Assigned(FCurrentLayer) and (Button = mbLeft) then
  begin
    // show the coordinates info on the status bar
    stsbrStatusInfo.Panels[0].Text := Format('Original (X: %d, Y: %d)', [FXActual, FYActual]);
    stsbrStatusInfo.Panels[1].Text := Format('Current (X: %d, Y: %d)', [FXActual, FYActual]);

    // don't show the animated Marching Ants lines when processing bitmap
    if Assigned(FSelection) then
    begin
      if FSelection.IsAnimated = True then       
      begin
        FSelection.IsAnimated := False;
      end;
    end;

    with imgWorkArea.Canvas do
    begin
      Pen.Color   := RUBBER_BAND_PEN_COLOR;
      Pen.Style   := RUBBER_BAND_PEN_STYLE;
      Pen.Width   := RUBBER_BAND_PEN_WIDTH;
      Brush.Color := RUBBER_BAND_BRUSH_COLOR;
      Brush.Style := RUBBER_BAND_BRUSH_STYLE;
    end;

    case FMarqueeDrawingState of
      dsNotDrawing:
        begin
          if FMarqueeTool = mtMoveResize then
          begin
            if Assigned(FSelection) then
            begin
              // Determine whether the mouse is over any of the selection handles.
              // Because of the scale property of image control will not always be
              // 1.0, so for getting correct result, we need pass coordinate that in
              // control space to function GetHandleAtPoint().
              FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);  // added by Xiaoguang

              if FMarqueeDrawingHandle in [dhAxAy, dhBxBy, dhAxBy, dhBxAy,
                                           dhLeftHalfAyBy, dhRightHalfAyBy,
                                           dhTopHalfAxBx, dhBottomHalfAxBx] then
              begin
                Screen.Cursor        := SetCursorByHandle(FMarqueeDrawingHandle);
                imgWorkArea.Cursor   := Screen.Cursor;
                FMarqueeDrawingState := dsStretchCorner;

                imgWorkArea.Update;
                FSelection.DrawMarchingAnts; 
              end
              else
              begin
                // if the mouse is over the selection...
                if FSelection.ContainsPoint( Point(FXActual, FYActual) ) then
                begin
                  Screen.Cursor        := crDrag;
                  imgWorkArea.Cursor   := Screen.Cursor;
                  FMarqueeDrawingState := dsTranslate;
                  FDrawingBasePoint    := Point(FXActual, FYActual);

                  imgWorkArea.Update;
                  FSelection.DrawMarchingAnts; 
                end;
              end;
            end;
          end;
        end;

      dsNewFigure:
        begin
          if Assigned(FSelection) then
          begin
            imgWorkArea.Update;
            FSelection.DrawMarchingAnts;           
          end;

          case FMarqueeTool of
            mtSingleRow,
            mtSingleColumn,
            mtRectangular,
            mtRoundRectangular,
            mtElliptical,
            mtPolygonal,
            mtRegularPolygon,
            mtLasso:
              begin
                if Assigned(FRegion) then
                begin
                  if FRegion.IsValidRegion then
                  begin
                    FreeAndNil(FRegion);
                  end;
                end;

                if not Assigned(FRegion) then
                begin
                  if FMarqueeTool = mtSingleRow then
                  begin
                    FRegion := TgmSingleRowRegion.Create(imgWorkArea.Canvas);
                    TgmSingleRowRegion(FRegion).RowWidth := FCurrentLayer.Bitmap.Width;
                  end
                  else if FMarqueeTool = mtSingleColumn then
                  begin
                    FRegion := TgmSingleColumnRegion.Create(imgWorkArea.Canvas);
                    TgmSingleColumnRegion(FRegion).ColumnHeight := FCurrentLayer.Bitmap.Height;
                  end
                  else if FMarqueeTool = mtRectangular then
                  begin
                    FRegion := TgmRectangularRegion.Create(imgWorkArea.Canvas);
                  end
                  else if FMarqueeTool = mtRoundRectangular then
                  begin
                    FRegion := TgmRoundRectangularRegion.Create(imgWorkArea.Canvas);
                    TgmRoundRectangularRegion(FRegion).CornerRadius := FCornerRadius;
                  end
                  else if FMarqueeTool = mtElliptical then
                  begin
                    FRegion := TgmEllipticRegion.Create(imgWorkArea.Canvas);
                  end
                  else if FMarqueeTool = mtPolygonal then
                  begin
                    FRegion := TgmPolygonalRegion.Create(imgWorkArea.Canvas);
                  end
                  else if FMarqueeTool = mtRegularPolygon then
                  begin
                    FRegion := TgmRegularPolygonalRegion.Create(imgWorkArea.Canvas);
                    TgmRegularPolygonalRegion(FRegion).EdgeCount := FSides;
                  end
                  else if FMarqueeTool = mtLasso then
                  begin
                    FRegion := TgmLassoRegion.Create(imgWorkArea.Canvas);
                  end;
                end;

                LBmpRect        := imgWorkArea.GetBitmapRect;
                FRegion.OffsetX := LBmpRect.Left;
                FRegion.OffsetY := LBmpRect.Top;
                FRegion.Scale   := imgWorkArea.Scale;

                FRegion.MouseDown(Button, Shift, FXActual, FYActual);
              end;

            mtMagneticLasso:
              begin
                if Assigned(FSelection) then
                begin
                  if FSelection.IsTranslated or
                     FSelection.IsCornerStretched or
                     FSelection.IsHorizFlipped or
                     FSelection.IsVertFlipped then
                  begin
                    MessageDlg('The selection has been moved, resized or flipped.' + #10#13 +
                               'Cannot create new selection.', mtError, [mbOK], 0);
                               
                    FSelection.IsAnimated := True;     
                    Exit;
                  end;
                end;

                if not Assigned(FMagneticLassoLayer) then
                begin
                  CreateLassoLayer;
                end;

                if not Assigned(FMagneticLasso) then
                begin
                  FMagneticLasso := TgmMagneticLasso.Create(
                    FCurrentLayer.Bitmap, FMagneticLassoLayer.Bitmap.Canvas);

                  FMagneticLasso.IsInteractive := chckbxInteractive.Checked;
                end;

                if Assigned(FMagneticLasso) then
                begin
                  FMagneticLasso.MouseDown(Button, Shift, FXActual, FYActual);

                  if Assigned(FMagneticLassoLayer) then
                  begin
                    FMagneticLassoLayer.Changed;
                  end;
                end;

                if Assigned(FSelection) then
                begin
                  // the Image must be updated before drawing the Marching Ants lines
                  imgWorkArea.Update;
                  FSelection.DrawMarchingAnts;   
                end;
              end;
          end;
        end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmMain.MarqueeToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LNewPoint       : TPoint;
  LTranslateVector: TPoint;
begin
  // calculate the layer coordinates of the mouse
  CalcLayerCoord(X, Y);
  
{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    case FMarqueeDrawingState of
      dsNewFigure:
        begin
          case FMarqueeTool of
            mtSingleRow,
            mtSingleColumn,
            mtRectangular,
            mtRoundRectangular,
            mtElliptical,
            mtPolygonal,
            mtRegularPolygon,
            mtLasso:
              begin
                if Assigned(FRegion) then
                begin
                  FRegion.MouseMove(Shift, FXActual, FYActual);
                end;
              end;

            mtMagneticLasso:
              begin
                if Assigned(FMagneticLasso) then
                begin
                  FMagneticLasso.MouseMove(Shift, FXActual, FYActual);

                  if Assigned(FMagneticLassoLayer) then
                  begin
                    FMagneticLassoLayer.Changed;
                  end;
                end;

                if Assigned(FSelection) then
                begin
                  // the Image must be updated before drawing the Marching Ants lines
                  imgWorkArea.Update;
                  FSelection.DrawMarchingAnts;  
                end;
              end;
          end;
        end;

      dsStretchCorner:
        begin
          if Assigned(FSelection) then
          begin
            // set up the start point and end point of the selection

            FSelection.IsCornerStretched := True;
            
            case FMarqueeDrawingHandle of
              dhAxAy:
                begin
                  FSelection.MaskBorderStart := Point(FXActual + SELECTION_HANDLE_RADIUS,
                                                      FYActual + SELECTION_HANDLE_RADIUS);
                end;

              dhBxBy:
                begin
                  FSelection.MaskBorderEnd := Point(FXActual - SELECTION_HANDLE_RADIUS,
                                                    FYActual - SELECTION_HANDLE_RADIUS);
                end;

              dhAxBy:
                begin
                  FSelection.MaskBorderStart := Point(FXActual + SELECTION_HANDLE_RADIUS,
                                                      FSelection.MaskBorderStart.Y);
                                                         
                  FSelection.MaskBorderEnd := Point(FSelection.MaskBorderEnd.X,
                                                    FYActual - SELECTION_HANDLE_RADIUS);
                end;

              dhBxAy:
                begin
                  FSelection.MaskBorderStart := Point(FSelection.MaskBorderStart.X,
                                                      FYActual + SELECTION_HANDLE_RADIUS);
                                                         
                  fSelection.MaskBorderEnd := Point(FXActual - SELECTION_HANDLE_RADIUS,
                                                    FSelection.MaskBorderEnd.Y);
                end;

              dhTopHalfAxBx:
                begin
                  FSelection.MaskBorderStart := Point(FSelection.MaskBorderStart.X,
                                                      FYActual + SELECTION_HANDLE_RADIUS);
                end;

              dhBottomHalfAxBx:
                begin
                  FSelection.MaskBorderEnd := Point(FSelection.MaskBorderEnd.X,
                                                    FYActual - SELECTION_HANDLE_RADIUS);
                end;
                
              dhLeftHalfAyBy:
                begin
                  FSelection.MaskBorderStart := Point(FXActual + SELECTION_HANDLE_RADIUS,
                                                      FSelection.MaskBorderStart.Y);
                end;
                
              dhRightHalfAyBy:
                begin
                  FSelection.MaskBorderEnd := Point(FXActual - SELECTION_HANDLE_RADIUS,
                                                    FSelection.MaskBorderEnd.Y);
                end;
            end;
            
            // make the FMaskBorderStart and FMaskBorderEnd be in right order
            FSelection.StandardizeOrder;

            // resize the selection with FMaskBorderStart and FMaskBorderEnd of it
            FSelection.ResizeSelection;
            
            // searching for the Marching Ants lines on FResizeMask of the selection
            FSelection.GetMarchingAntsLines;

            // render the selection
            FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);

            // updating the view
            FCurrentLayer.Bitmap.Changed;

            // the Image must be updated before drawing the Marching Ants lines
            imgWorkArea.Update;

            // drawing the new Marching Ants lines
            FSelection.DrawMarchingAnts;   
          end;
        end;

      dsTranslate:
        begin
          if Assigned(FSelection) then
          begin
            // calculate the new postion
            LNewPoint        := Point(FXActual, FYActual);
            LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);
            
            FSelection.TranslateSelection(LTranslateVector);
            
            // render the selection
            FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
            FCurrentLayer.Bitmap.Changed;
            
            // the Image must be updated before drawing the Marching Ants lines
            imgWorkArea.Update;
            FSelection.DrawMarchingAnts;         

            // save the new point for calculating offset
            FDrawingBasePoint := LNewPoint;
          end;
        end;
    end;
  end
  else // if the FDrawing = False
  begin
    if Assigned(FSelection) then
    begin
      if FMarqueeDrawingState = dsNotDrawing then
      begin
        // Because of the scale property of image control will not always be
        // 1.0, so for getting correct result, we need pass coordinate that in
        // control space to function GetHandleAtPoint().
        FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);  // added by Xiaoguang

        if FMarqueeDrawingHandle in [dhNone, dhAxAy, dhBxBy, dhAxBy, dhBxAy,
                                     dhLeftHalfAyBy, dhRightHalfAyBy,
                                     dhTopHalfAxBx, dhBottomHalfAxBx] then
        begin
          Screen.Cursor := SetCursorByHandle(FMarqueeDrawingHandle);
        end;
      end;
    end;

    if Assigned(FCurrentLayer) then
    begin
      // show the color at the mouse position
      if (FXActual >= 0) and
         (FYActual >= 0) and
         (FXActual <= FCurrentLayer.Bitmap.Width) and
         (FYActual <= FCurrentLayer.Bitmap.Height) then
      begin
        shpCurrentColor.Brush.Color :=
          imgWorkArea.Canvas.Pixels[FXActual + FLayerTopLeft.X,
                                    FYActual + FLayerTopLeft.Y];
      end;
    end;
  end;
  
  imgWorkArea.Canvas.Pen.Mode := pmCopy;
  
  // show the coordinates info on the status bar
  stsbrStatusInfo.Panels[1].Text := Format('Current (X: %d, Y: %d)', [FXActual, FYActual]);
end;

procedure TfrmMain.MarqueeToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  // calculate the layer coordinates of the mouse
  CalcLayerCoord(X, Y);
  
{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    // the drawing is over
    FDrawing := False;
    
    imgWorkArea.Canvas.Pen.Mode := pmCopy;
    
    case FMarqueeDrawingState of
      dsNewFigure:
        begin
          // if there is no selection, create one
          if FSelection = nil then
          begin
            FSelection := TgmSelection.Create(imgWorkArea, FCurrentLayer.Bitmap);

            FSelection.FeatherRadius := updwnFeatherRadius.Position;
            FSelection.IsFeathered   := (FSelection.FeatherRadius > 0);
          end;

          if (FSelection.IsTranslated      = False) and
             (FSelection.IsCornerStretched = False) and
             (FSelection.IsHorizFlipped    = False) and
             (FSelection.IsVertFlipped     = False) then
          begin
            case FMarqueeTool of
              mtSingleRow,
              mtSingleColumn,
              mtRectangular,
              mtRoundRectangular,
              mtElliptical,
              mtPolygonal,
              mtRegularPolygon,
              mtLasso:
                begin
                  if Assigned(FRegion) then
                  begin
                    FRegion.MouseUp(Button, Shift, FXActual, FYActual);

                    // for Polygonal region...
                    if not FRegion.IsRegionDefineCompleted then
                    begin
                      if Assigned(FSelection) then
                      begin
                        FSelection.IsAnimated := True;        
                      end;

                      Exit;
                    end;

                    if FRegion.IsValidRegion then
                    begin
                      FSelection.CreateCustomRGN(FRegion.Region, FMarqueeMode);
                    end;
                    
                    FreeAndNil(FRegion);
                  end;
                end;
                
              mtMagicWand:
                begin
                  if (FXActual >= 0) and
                     (FYActual >= 0) and
                     (FXActual < FCurrentLayer.Bitmap.Width) and
                     (FYActual < FCurrentLayer.Bitmap.Height) then
                  begin
                    FSelection.MagicTolerance := FMagicWandTolerance / 100;

                    FSelection.CreateMagicWandMarqueeRGN(FCurrentLayer.Bitmap,
                      FXActual, FYActual, FCurrentLayer.Bitmap.Pixel[FXActual, FYActual],
                      FMarqueeMode);
                  end;
                end;

              mtMagneticLasso:
                begin
                  if FDoubleClicked = False then
                  begin
                    if Assigned(FMagneticLasso) then
                    begin
                      FMagneticLasso.MouseUp(Button, Shift, FXActual, FYActual);

                      if Assigned(FMagneticLassoLayer) then
                      begin
                        FMagneticLassoLayer.Changed;
                      end;
                    end;
                  end
                  else
                  begin
                    FDoubleClicked := False; // restore the mark

                    // convert lasso to selection
                    if Assigned(FMagneticLasso) and FMagneticLasso.IsConnected then
                    begin
                      FSelection.CreateCustomRGN(FMagneticLasso.CurveRegion, FMarqueeMode);

                      FreeAndNil(FMagneticLasso);

                      if Assigned(FMagneticLassoLayer) then
                      begin
                        FreeAndNil(FMagneticLassoLayer);
                      end;
                    end
                    else
                    begin
                      if Assigned(FSelection) then
                      begin
                        FSelection.IsAnimated := True;    
                      end;
                      
                      Exit;
                    end;
                  end;
                end;
            end;

            FSelection.Background.Assign(FSelection.SourceBitmap);
            FSelection.GetActualMaskBorder;   // get the actual border of the selection
            FSelection.CutRegionFromOriginal; // cut out bitmap from the FSourceBitmap and FOriginalMask of the selection
            FSelection.GetForeground;         // get foreground
            FSelection.GetMarchingAntsLines;  // get the Marching Ants lines from FResizeMask of the selection
            
            // filling the cutted region on the background
            case rdgrpFillMode.ItemIndex of
              0:
                begin
                  FSelection.GetBackgroundWithFilledColor( Color32(shpCurrentColor.Brush.Color),
                                                           [csRed, csGreen, csBlue] );
                end;
                
              1:
                begin
                  FSelection.GetBackgroundWithTransparent;
                end;
            end;
          end
          else
          begin
            MessageDlg('Could not create a new selection,' + #10#13 +
                       'because the current selection was flipped,' + #10#13 +
                       'translated or resized.', mtInformation, [mbOK], 0);

            if Assigned(FRegion) then
            begin
              FreeAndNil(FRegion);
            end;
          end;

          FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
          FCurrentLayer.Bitmap.Changed;

          { If there is no mask shadow that the selection needed,
            then delete the selection. }
          if FSelection.HasShadow = False then
          begin
            FCurrentLayer.Bitmap.Assign(FSelection.SourceBitmap);
            FCurrentLayer.Bitmap.Changed;
            FreeAndNil(FSelection);
            
            tlbtnCommitSelection.Enabled := False;
            tlbtnDeselect.Enabled        := False;
            tlbtnDeleteSelection.Enabled := False;
          end
          else
          begin
            tlbtnCommitSelection.Enabled := True;
            tlbtnDeselect.Enabled        := True;
            tlbtnDeleteSelection.Enabled := True;
          end;
          
          ChangeImageCursorByMarqueeTools;  // change cursor
        end;

      dsStretchCorner, dsTranslate:
        begin
          Screen.Cursor := crDefault;
          ChangeImageCursorByMarqueeTools;

          FMarqueeDrawingState := dsNotDrawing;
          FCurrentLayer.Bitmap.Changed;
        end;
    end;

    if Assigned(FSelection) and
       (FSelection.MarchingAntsLineList.Count > 0) then
    begin
      FCurrentLayer.Bitmap.Changed;
      imgWorkArea.Update;

      FSelection.IsAnimated := True;           // added by iaoming
    end;
  end;
end;

// Selection Transform
procedure TfrmMain.TransformSelectionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Assigned(FSelectionTransformation) then
  begin
    case FSelectionTransformation.TransformMode of
      tmDistort:
        begin
          // Because of the scale property of image control will not always be
          // 1.0, so for getting correct result, we need pass coordinate that in
          // control space to function GetHandleAtPoint().

          // changed by Xiaoguang -- using X and Y, not FXActual, FYActual as parameters
          FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

          if FTransformHandle <> dhNone then
          begin
            FDrawingBasePoint := Point(FXActual, FYActual);

            if Assigned(FSelection) then
            begin
              FSelection.IsAnimated := False;          
            end;

            FDrawing           := True;
            Screen.Cursor      := crMovePath;
            imgWorkArea.Cursor := crMovePath;
          end
          else
          begin
            Screen.Cursor      := crDefault;
            imgWorkArea.Cursor := crMoveSelection;
          end;
        end;
        
      tmRotate:
        begin
          FRotateRadiansInMouseDown := ArcTan2(
            FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
            FXActual - FSelectionTransformation.SelectionCenterCoord.X);

          if Assigned(FSelection) then
          begin
            FSelection.IsAnimated := False;      
          end;

          FDrawing := True;
        end;
        
      tmScale:
        begin
          // Because of the scale property of image control will not always be
          // 1.0, so for getting correct result, we need pass coordinate that in
          // control space to function GetHandleAtPoint().

          // changed by Xiaoguang -- using X and Y, not FXActual, FYActual as parameters
          FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

          if FTransformHandle <> dhNone then
          begin
            FDrawingBasePoint := Point(FXActual, FYActual);

            if Assigned(FSelection) then
            begin
              FSelection.IsAnimated := False;     
            end;

            FDrawing           := True;
            Screen.Cursor      := SetCursorByHandle(FTransformHandle);
            imgWorkArea.Cursor := Screen.Cursor;
          end
          else
          begin
            Screen.Cursor      := crDefault;
            imgWorkArea.Cursor := crMoveSelection;
          end;
        end;
    end;
  end;
end;

procedure TfrmMain.TransformSelectionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  NewPoint, TranslateVector: TPoint;
  RadiansIncrement         : Extended;
  Radians                  : Extended;
begin
  CalcLayerCoord(X, Y);
  
  if FDrawing then
  begin
    case FSelectionTransformation.TransformMode of
      tmDistort:
        begin
          // calculte the offset vector
          NewPoint        := Point(FXActual, FYActual);
          TranslateVector := SubtractPoints(NewPoint, FDrawingBasePoint);

          // change the vertices of transformation by the offset vector
          FSelectionTransformation.ChangeVertices_Distort(TranslateVector, FTransformHandle);
        end;
        
      tmRotate:
        begin
          FRotateRadiansInMouseMove := ArcTan2(
            FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
            FXActual - FSelectionTransformation.SelectionCenterCoord.X);

          RadiansIncrement := FRotateRadiansInMouseMove - FRotateRadiansInMouseDown;
          FSelectionTransformation.ChangeVertices_Rotate(RadiansIncrement);

          Screen.Cursor      := GetCursorByDegree( RadToDeg(FRotateRadiansInMouseMove) );
          imgWorkArea.Cursor := Screen.Cursor;
        end;
        
      tmScale:
        begin
          NewPoint        := Point(FXActual, FYActual);
          TranslateVector := SubtractPoints(NewPoint, FDrawingBasePoint);

          FSelectionTransformation.ChangeVertices_Scale(TranslateVector, FTransformHandle);
        end;
    end;
    
    FSelectionTransformation.ExecuteTransform;

    FSelectionTransformation.ShowTransformedSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);

    // showing the transform result
    FCurrentLayer.Bitmap.Changed;

    imgWorkArea.Update;

    if Assigned(FSelection) then
    begin
      FSelection.DrawMarchingAnts;  
    end;

    FDrawingBasePoint := NewPoint;
  end
  else
  begin
    if Assigned(FSelectionTransformation) then
    begin
      case FSelectionTransformation.TransformMode of
        tmDistort:
          begin
            // Because of the scale property of image control will not always be
            // 1.0, so for getting correct result, we need pass coordinate that in
            // control space to function GetHandleAtPoint().

            // changed by Xiaoguang -- using X and Y, not FXActual, FYActual as parameters
            if FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS) <> dhNone then
            begin
              Screen.Cursor      := crMovePath;
              imgWorkArea.Cursor := crMovePath;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crMoveSelection;
            end;
          end;
          
        tmRotate:
          begin
            Radians := ArcTan2(FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
                               FXActual - FSelectionTransformation.SelectionCenterCoord.X);

            Screen.Cursor      := GetCursorByDegree( RadToDeg(Radians) );
            imgWorkArea.Cursor := Screen.Cursor;
          end;
          
        tmScale:
          begin
            // Because of the scale property of image control will not always be
            // 1.0, so for getting correct result, we need pass coordinate that in
            // control space to function GetHandleAtPoint().

            // changed by Xiaoguang -- using X and Y, not FXActual, FYActual as parameters
            FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

            if FTransformHandle <> dhNone then
            begin
              Screen.Cursor      := SetCursorByHandle(FTransformHandle);
              imgWorkArea.Cursor := Screen.Cursor;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crMoveSelection;
            end;
          end;
      end;
    end;
  end;

  // show the coordinates info on the status bar
  stsbrStatusInfo.Panels[1].Text := Format('Current (X: %d, Y: %d)', [FXActual, FYActual]);
end;

procedure TfrmMain.TransformSelectionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated := True;      
    end;

    FDrawing := False;

    if Assigned(FSelectionTransformation) then
    begin
      if FSelectionTransformation.TransformMode = tmRotate then
      begin
        TgmSelectionRotate(FSelectionTransformation).UpdateRotateState;
      end;
    end;
  end;
end;

{ For Pencil Tool }
procedure TfrmMain.PencilMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  CalcLayerCoord(X, Y); // get layer space coordinates
  CalcSelectionCoord;   // get selection space coordinates

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // show the coordinates info on the status bar
    stsbrStatusInfo.Panels[0].Text := Format('Original (X: %d, Y: %d)', [FXActual, FYActual]);
    stsbrStatusInfo.Panels[1].Text := Format('Current (X: %d, Y: %d)', [FXActual, FYActual]);

    if Assigned(FSelection) then
    begin
      Felozox           := FMarqueeX;
      Felozoy           := FMarqueeY;
      FDrawingBasePoint := Point(FMarqueeX, FMarqueeY);
    end
    else
    begin
      Felozox           := FXActual;
      Felozoy           := FYActual;
      FDrawingBasePoint := Point(FXActual, FYActual);
    end;
    
    Ftavolsag := 0;
    RebuildPencil;

    if Assigned(FSelection) then
    begin
      // don't show the animated Marching Ants lines when processing bitmap
      if FSelection.IsAnimated = True then
      begin
        FSelection.IsAnimated := False;          
      end;

      FSelection.DrawMarchingAnts;            
      
      FPencilBitmap.drawto(FSelection.CutOriginal,
                           FMarqueeX - FPencilBitmap.Width div 2,
                           FMarqueeY - FPencilBitmap.Width div 2);

      ShowProcessedSelection;
    end
    else
    begin
      FPencilBitmap.DrawTo(FCurrentLayer.Bitmap,
                           FXActual - FPencilBitmap.Width div 2,
                           FYActual - FPencilBitmap.Width div 2);
    end;
  end;

  FDrawing := True;
end;

procedure TfrmMain.PencilMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  CalcLayerCoord(X, Y); // get the layer space coordinates
  CalcSelectionCoord;   // get the selection space coordinates

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    if Assigned(FSelection) then
    begin
      PencilLine(FDrawingBasePoint.X, FDrawingBasePoint.Y, FMarqueeX, FMarqueeY,
                 0, FSelection.CutOriginal);
                 
      ShowProcessedSelection;
      
      FDrawingBasePoint := Point(FMarqueeX, FMarqueeY);
    end
    else
    begin
      PencilLine(FDrawingBasePoint.X, FDrawingBasePoint.Y, FXActual, FYActual,
                 0, FCurrentLayer.Bitmap);

      FDrawingBasePoint := Point(FXActual, FYActual);
    end;
  end;

  // show the coordinates info on the status bar
  stsbrStatusInfo.Panels[1].Text := Format('Current (X: %d, Y: %d)', [FXActual, FYActual]);
end;

procedure TfrmMain.PencilMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    FDrawing := False;
    
    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated := True;        
    end;
  end;
end;

// connect mouse events to image.
procedure TfrmMain.ConnectSelectionMouseEvents;
begin
  imgWorkArea.OnMouseDown := MarqueeToolsMouseDown;
  imgWorkArea.OnMouseMove := MarqueeToolsMouseMove;
  imgWorkArea.OnMouseUp   := MarqueeToolsMouseUp;
end;

procedure TfrmMain.ConnectTransformMouseEvents;
begin
  imgWorkArea.OnMouseDown := TransformSelectionMouseDown;
  imgWorkArea.OnMouseMove := TransformSelectionMouseMove;
  imgWorkArea.OnMouseUp   := TransformSelectionMouseUp;
end;

procedure TfrmMain.ConnectPencilMouseEvents;
begin
  imgWorkArea.OnMouseDown := PencilMouseDown;
  imgWorkArea.OnMouseMove := PencilMouseMove;
  imgWorkArea.OnMouseUp   := PencilMouseUp;
end;

//------------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { by default, PST_CLEAR_BACKGND is executed at this stage,
    which, in turn, calls ExecClearBackgnd method of ImgView.
    Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
    to call the OnPaintStage event instead of performing default action. }

  with imgWorkArea.PaintStages[0]^ do
  begin
    Parameter := PAINT_STAGE_ZERO_PARAMETER;            

    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;

  FHandlePaintStage := imgWorkArea.PaintStages.Add;
  FHandleStageIndex := imgWorkArea.PaintStages.Count - 1;   

  with FHandlePaintStage^ do
  begin
    DsgnTime  := False;
    RunTime   := True;                                      
    Stage     := PST_CUSTOM;
    Parameter := HANDLE_STAGE_PARAMETER;
  end;

  FDrawing          := False;
  FIsImageOpened    := False;
  FDoubleClicked    := False;
  FCurrentLayer     := nil;
  FLayerTopLeft     := Point(0, 0);
  FXActual          := 0;
  FYActual          := 0;

  imgWorkArea.OnPaintStage := ImagePaintStage;             

  { Use the FMarqueeX and FMarqueeY to be the coordinates to draw things on
    selection, actually on the FCutOriginal of the selection. }
  FMarqueeX         := 0;
  FMarqueeY         := 0;
  FStartPoint       := Point(0, 0);
  FEndPoint         := Point(0, 0);
  FDrawingBasePoint := Point(0, 0);

{ Selection }
  FMarqueeDrawingState      := dsNotDrawing;
  FMarqueeDrawingHandle     := dhNone;
  FSelection                := nil;
  FSelectionTranslateTarget := sttNone;

  FMarqueeTool        := mtMoveResize;
  FMarqueeMode        := mmNew;
  FSides              := 3;
  FCornerRadius       := 30;
  FMagicWandTolerance := 15;

  FRegion := nil;

{ Magnetic Lasso }
  FMagneticLasso      := nil;
  FMagneticLassoLayer := nil;

{ Selection Transformation }
  FSelectionTransformation := nil;
  FTransformHandle         := dhNone;
  FTransformOldVertices    := nil;
  FTransformNewVertices    := nil;
  
{ Pencil }
  FPencilBitmap             := TBitmap32.Create();
  FPencilBitmap.DrawMode    := dmBlend;
  FPencilBitmap.CombineMode := cmMerge;
  FPenWidth                 := 10;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FCurrentLayer := nil;

  imgWorkArea.PaintStages.Delete(FHandleStageIndex);    

{ Selection }
  FSelection.Free;

  if Assigned(FRegion) then
  begin
    FRegion.Free;
  end;

{ Magnetic Lasso }
  if Assigned(FMagneticLasso) then
  begin
    FMagneticLasso.Free;
  end;

  if Assigned(FMagneticLassoLayer) then
  begin
    FMagneticLassoLayer.Free;
  end;

{ Selection Tranformation }
  FSelectionTransformation.Free;
  
{ Pencil }
  FPencilBitmap.Free;
end;

procedure TfrmMain.OpenImageClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if OpenPictureDialog.Execute then
    begin
      if Assigned(FMagneticLasso) then
      begin
        FreeAndNil(FMagneticLasso);
      end;

      if Assigned(FMagneticLassoLayer) then
      begin
        FreeAndNil(FMagneticLassoLayer);
      end;

      if FSelection <> nil then
      begin
        CancelSelection;
      end;

      if FCurrentLayer <> nil then
      begin
        FreeAndNil(FCurrentLayer);
      end;

      imgWorkArea.Layers.Clear;
      
      FIsImageOpened     := OpenImageWithNewLayer(OpenPictureDialog.FileName);
      pnlToolBox.Visible := FIsImageOpened;

      UpdateMarqueeOptions;
      
      // connect mouse events to image
      if tlbtnPencil.Down then
      begin
        ConnectPencilMouseEvents;
      end
      else
      begin
        ConnectSelectionMouseEvents;
      end;
      
      ChangeImageCursorByMarqueeTools; // change cursor
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.ChangeMarqueeTools(Sender: TObject);
begin
  FinishPolygonalSelection;
  FinishTransformation;
  FinishMagneticLasso;
  
  if Sender = tlbtnSelectionController then
  begin
    FMarqueeDrawingState := dsNotDrawing  // not drawing the selection
  end
  else
  begin
    FMarqueeDrawingState := dsNewFigure;  // drawing new marquee
  end;

  if Sender = tlbtnSelectionController then
  begin
    FMarqueeTool := mtMoveResize;
  end
  else if Sender = tlbtnSingleRowMarquee then
  begin
    FMarqueeTool := mtSingleRow;
  end
  else if Sender = tlbtnSingleColumnMarquee then
  begin
    FMarqueeTool := mtSingleColumn;
  end
  else if Sender = tlbtnRectangularMarquee then
  begin
    FMarqueeTool := mtRectangular;
  end
  else if Sender = tlbtnRoundRectangularMarquee then
  begin
    FMarqueeTool := mtRoundRectangular;
  end
  else if Sender = tlbtnEllipticalMarquee then
  begin
    FMarqueeTool := mtElliptical;
  end
  else if Sender = tlbtnPolygonalMarquee then
  begin
    FMarqueeTool := mtPolygonal;
  end
  else if Sender = tlbtnRegularPolygonMarquee then
  begin
    FMarqueeTool := mtRegularPolygon;
  end
  else if Sender = tlbtnLassoMarquee then
  begin
    FMarqueeTool := mtLasso;
  end
  else if Sender = tlbtnMagicWand then
  begin
    FMarqueeTool := mtMagicWand;
  end
  else if Sender = tlbtnMagneticLasso then
  begin
    FMarqueeTool := mtMagneticLasso;
  end
  else if Sender = tlbtnPencil then
  begin
    FMarqueeTool := mtNone;
  end;

  UpdateMarqueeOptions;

  if Assigned(FSelection) then
  begin
    if FSelectionTransformation = nil then
    begin
      FSelection.ShowSelection(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
      FCurrentLayer.Bitmap.Changed;
    end;
  end;

  ChangeImageCursorByMarqueeTools; // change cursor

  if tlbtnPencil.Down then
  begin
    ConnectPencilMouseEvents;
  end
  else
  begin
    ConnectSelectionMouseEvents;
  end;

  chckbxInteractive.Enabled := (FMarqueeTool = mtMagneticLasso);
end;

procedure TfrmMain.ChangeMarqueeMode(Sender: TObject);
begin
  if Sender = tlbtnNewSelection then
  begin
    FMarqueeMode := mmNew;
  end
  else if Sender = tlbtnAddSelection then
  begin
    FMarqueeMode := mmAdd;
  end
  else if Sender = tlbtnSubtractSelection then
  begin
    FMarqueeMode := mmSubtract;
  end
  else if Sender = tlbtnIntersectSelection then
  begin
    FMarqueeMode := mmIntersect;
  end
  else if Sender = tlbtnExcludeOverlapSelection then
  begin
    FMarqueeMode := mmExcludeOverlap;
  end;

  ChangeImageCursorByMarqueeTools; // change cursor
end;

procedure TfrmMain.tlbtnCommitSelectionClick(Sender: TObject);
begin
  if FSelectionTransformation = nil then
  begin
    if Assigned(FSelection) then
    begin
      CommitSelection;
    end;

    tlbtnCommitSelection.Enabled := False;
    tlbtnDeselect.Enabled        := False;
    tlbtnDeleteSelection.Enabled := False;
  end;
end;

procedure TfrmMain.tlbtnDeselectClick(Sender: TObject);
begin
  if FSelectionTransformation = nil then
  begin
    if Assigned(FSelection) then
    begin
      CancelSelection;
    end;

    tlbtnCommitSelection.Enabled := False;
    tlbtnDeselect.Enabled        := False;
    tlbtnDeleteSelection.Enabled := False;
  end;
end;

procedure TfrmMain.tlbtnDeleteSelectionClick(Sender: TObject);
begin
  if FSelectionTransformation = nil then
  begin
    if Assigned(FSelection) then
    begin
      DeleteSelection;
    end;

    tlbtnCommitSelection.Enabled := False;
    tlbtnDeselect.Enabled        := False;
    tlbtnDeleteSelection.Enabled := False;
  end;
end;

procedure TfrmMain.updwnMultiTaskClick(Sender: TObject;
  Button: TUDBtnType);
begin
  case FMarqueeTool of
    mtMagicWand:
      begin
        FMagicWandTolerance := updwnMultiTask.Position;
      end;
      
    mtRegularPolygon:
      begin
        FSides := updwnMultiTask.Position;
      end;
      
    mtRoundRectangular:
      begin
        FCornerRadius := updwnMultiTask.Position;
      end;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  FinishPolygonalSelection;
  
  case cmbbxZoom.ItemIndex of
    0:
      begin
        imgWorkArea.Scale := 16.0;
      end;

    1:
      begin
        imgWorkArea.Scale := 8.0;
      end;

    2:
      begin
        imgWorkArea.Scale := 4.0;
      end;

    3:
      begin
        imgWorkArea.Scale := 2.0;
      end;

    4:
      begin
        imgWorkArea.Scale := 1.0;
      end;

    5:
      begin
        imgWorkArea.Scale := 0.5;
      end;

    6:
      begin
        imgWorkArea.Scale := 0.25;
      end;

    7:
      begin
        imgWorkArea.Scale := 0.125;
      end;

    8:
      begin
        imgWorkArea.Scale := 0.0625;
      end;
  end;

  FLayerTopLeft := GetLayerTopLeft;
end;

procedure TfrmMain.updwnFeatherRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FinishPolygonalSelection;

  if (FSelection = nil) or (FSelectionTransformation <> nil) then
  begin
    Exit;
  end;

  if Assigned(FSelection) then
  begin
    if (FSelection.IsTranslated      = False) and
       (FSelection.IsCornerStretched = False) and
       (FSelection.IsHorizFlipped    = False) and
       (FSelection.IsVertFlipped     = False) then
    begin
      Screen.Cursor := crHourGlass;
      try
        MakeSelectionFeather;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;

  if Assigned(FSelection) then
  begin
    if FSelection.IsAnimated = False then
    begin
      FSelection.IsAnimated := True;           
    end;
  end;
end;

procedure TfrmMain.mnitmInvertClick(Sender: TObject);
begin
  FinishPolygonalSelection;
  FinishMagneticLasso;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;       
    
    InvertBitmap32(FSelection.CutOriginal, [csRed, csGreen, csBlue]);
    ShowProcessedSelection;

    FSelection.IsAnimated := True;    
  end
  else
  begin
    InvertBitmap32(FCurrentLayer.Bitmap, [csRed, csGreen, csBlue]);
    FCurrentLayer.Bitmap.Changed;
  end;
end;

procedure TfrmMain.mnitmSelectAllClick(Sender: TObject);
begin
  FinishMagneticLasso;
  CreateSelectionForAll;
end;

procedure TfrmMain.mnitmInvertSelectionClick(Sender: TObject);
begin
  FinishMagneticLasso;
  FinishPolygonalSelection;
  MakeSelectionInverse;
end;

procedure TfrmMain.mnitmColorRangeSelectionClick(Sender: TObject);
begin
  FinishPolygonalSelection;
  FinishMagneticLasso;
  
  if frmColorRangeSelection.ShowModal = idOK then
  begin
    Screen.Cursor := crHourGlass;
    try
      CreateSelectionByColorRange;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated        := True;     
    tlbtnCommitSelection.Enabled := True;
    tlbtnDeselect.Enabled        := True;
    tlbtnDeleteSelection.Enabled := True;
  end
  else
  begin
    FCurrentLayer.Bitmap.Changed;
    
    tlbtnCommitSelection.Enabled := False;
    tlbtnDeselect.Enabled        := False;
    tlbtnDeleteSelection.Enabled := False;
  end;
end; 

procedure TfrmMain.FlipImageClick(Sender: TObject);
var
  LFlipMode: TgmFlipMode;
begin
  FinishMagneticLasso;

  LFlipMode := fmNone;
  
  if Sender = mnitmHorizFlip then
  begin
    LFlipMode := fmHorizontal;
  end
  else
  if Sender = mnitmVertFlip then
  begin
    LFlipMode := fmVertical;
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;  
    
    FSelection.FlipSelection(LFlipMode);
    ShowProcessedSelection;
    
    FSelection.IsAnimated := True;   
  end
  else
  begin
    FlipBitmap(FCurrentLayer.Bitmap, LFlipMode);
    FCurrentLayer.Bitmap.Changed;
  end;
end;

procedure TfrmMain.ChangeTransformMode(Sender: TObject);
begin
  FinishMagneticLasso;

  if Assigned(FSelection) then
  begin
    if Sender = mnitmDistortTransformation then
    begin
      if FSelectionTransformation <> nil then
      begin
        if FSelectionTransformation.TransformMode <> tmDistort then
        begin
          FinishTransformation;
        end
        else
        begin
          Exit;
        end;
      end;

      mnitmDistortTransformation.Checked := True;
      
      if FSelectionTransformation = nil then
      begin
        FSelectionTransformation := TgmSelectionDistort.Create(FSelection);
      end;
    end
    else
    if Sender = mnitmRotateTransformation then
    begin
      if FSelectionTransformation <> nil then
      begin
        if FSelectionTransformation.TransformMode <> tmRotate then
        begin
          FinishTransformation;
        end
        else
        begin
          Exit;
        end;
      end;

      mnitmRotateTransformation.Checked := True;
      
      if FSelectionTransformation = nil then
      begin
        FSelectionTransformation := TgmSelectionRotate.Create(FSelection);
      end;
    end
    else
    if Sender = mnitmScaleTransformation then
    begin
      if FSelectionTransformation <> nil then
      begin
        if FSelectionTransformation.TransformMode <> tmScale then
        begin
          FinishTransformation;
        end
        else
        begin
          Exit;
        end;
      end;

      mnitmScaleTransformation.Checked := True;
      
      if FSelectionTransformation = nil then
      begin
        FSelectionTransformation := TgmSelectionScale.Create(FSelection);
      end;
    end;

    if Assigned(FSelectionTransformation) then
    begin
      FCurrentLayer.Bitmap.Changed;     
      ConnectTransformMouseEvents;
    end;
  end;
end;

procedure TfrmMain.UpdateMenuItemClick(Sender: TObject);
begin
  if Sender = mnhdEdit then
  begin
    mnitmTransform.Enabled          := FIsImageOpened;
    mnitmTransform.Items[0].Enabled := (FSelection <> nil);

    mnitmTransform.Items[0].Checked := (FSelectionTransformation <> nil) and
                                       (FSelectionTransformation.TransformMode = tmScale);
                                       
    mnitmTransform.Items[1].Enabled := mnitmTransform.Items[0].Enabled;
    
    mnitmTransform.Items[1].Checked := (FSelectionTransformation <> nil) and
                                       (FSelectionTransformation.TransformMode = tmRotate);
                                       
    mnitmTransform.Items[2].Enabled := mnitmTransform.Items[0].Enabled;
    
    mnitmTransform.Items[2].Checked := (FSelectionTransformation <> nil) and
                                       (FSelectionTransformation.TransformMode = tmDistort);
                                       
    mnitmTransform.Items[4].Enabled := (FSelectionTransformation = nil);
    mnitmTransform.Items[5].Enabled := mnitmTransform.Items[4].Enabled;
  end
  else
  if Sender = mnhdImage then
  begin
    mnitmInvert.Enabled := FIsImageOpened and (FSelectionTransformation = nil);
  end
  else
  if Sender = mnhdSelect then
  begin
    if FSelection <> nil then
    begin
      mnitmSelectAll.Enabled := (FSelection.IsTranslated      = False) and
                                (FSelection.IsCornerStretched = False) and
                                (FSelection.IsHorizFlipped    = False) and
                                (FSelection.IsVertFlipped     = False) and
                                (FSelection.FeatherRadius     = 0)     and
                                (FSelectionTransformation = nil);

      mnitmInvertSelection.Enabled     := mnitmSelectAll.Enabled;
      mnitmColorRangeSelection.Enabled := mnitmSelectAll.Enabled;
    end
    else
    begin
      mnitmSelectAll.Enabled           := FIsImageOpened and (FSelectionTransformation = nil);         
      mnitmInvertSelection.Enabled     := False;
      mnitmColorRangeSelection.Enabled := mnitmSelectAll.Enabled;
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

procedure TfrmMain.mnitmExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.imgWorkAreaDblClick(Sender: TObject);
begin
  if FMarqueeTool = mtPolygonal then
  begin
    if Assigned(FRegion) and (FRegion.RegionStyle = gmrsPolygonal) then
    begin
      TgmPolygonalRegion(FRegion).DblClick(Sender);
    end;
  end;

  FDoubleClicked := True;
end;

procedure TfrmMain.imgWorkAreaResize(Sender: TObject);
var
  LBmpRect: TRect;
begin
  FLayerTopLeft := GetLayerTopLeft;

  if Assigned(FRegion) then
  begin
    LBmpRect        := imgWorkArea.GetBitmapRect;
    FRegion.OffsetX := LBmpRect.Left;
    FRegion.OffsetY := LBmpRect.Top;

    imgWorkArea.Update;
    FRegion.DrawRegionOutline;
  end;
end;

procedure TfrmMain.imgWorkAreaScroll(Sender: TObject);
begin
  FLayerTopLeft := GetLayerTopLeft;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Close;
  end;
end;

end.
