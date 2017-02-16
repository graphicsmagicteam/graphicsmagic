unit ChildForm;

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
 *                                                   
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 *   x2nie - Fathony Luthfillah < x2nie@yahoo.com >
 *     Adding additional Paint-Stages that above all layers.
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

// Update Date: 2017/02/14

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, ImgList,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_Layers,
{ GraphicsMagicLib }
  gmChannels,
  gmChannelManager,    
  gmColorBalance,
  gmCrop,
  gmFigures,
  gmHistoryCommands,
  gmLayers,             
  gmLayerFigureManager,
  gmMagneticLasso,
  gmMeasure,
  gmPaths,
  gmPathCommands,             
  gmPenTools,            // Pen-Path Tools
  gmRegions,
  gmSelection,
  gmTypes;

type
  TfrmChild = class(TForm)
    pmnChangeCurveControlPoints: TPopupMenu;
    pmnitmCurveControlP1: TMenuItem;
    pmnitmCurveControlP2: TMenuItem;
    imglstChild: TImageList;
    imgWorkArea: TImgView32;
    tmrSpecialBrushes: TTimer;
    tmrSpecialErasers: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ChangeCurveControlPoints(Sender: TObject);
    procedure pmnChangeCurveControlPointsPopup(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure imgWorkAreaPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure tmrSpecialBrushesTimer(Sender: TObject);
    procedure imgWorkAreaResize(Sender: TObject);
    procedure imgWorkAreaScroll(Sender: TObject);
    procedure tmrSpecialErasersTimer(Sender: TObject);
  private
{ Common }
    FEditMode            : TgmEditMode;  // indicate which edit mode we are in
    FAccumTranslateVector: TPoint;       // accumulated translation vector
    FGlobalTopLeft       : TPoint;       // commonly used for save top-left position temporarily
    FGlobalBottomRight   : TPoint;       // commonly used for save bottom-right position temporarily
    FKeyIsDown           : Boolean;      // Mark that if we have pressed a key.
    FFileName            : string;       // the filename of the created/opened image file
    FImageProcessed      : Boolean;      // whether the image is modified
    FDrawing             : Boolean;      // whether the left button of the mouse is pressed
    FMayClick            : Boolean;      // whether we could define a polygon by single click the mouse left button
    FDoubleClicked       : Boolean;      // whether the left button of the mouse is double clicked
    FXActual             : Integer;      // actual x coordinate no matter what scale is used
    FYActual             : Integer;      // actual y coordinate no matter what scale is used
    FMarqueeX            : Integer;      // actual x coordinate on selection
    FMarqueeY            : Integer;      // actual y coordinate on selection
    FStartPoint          : TPoint;       // this field is for common use that may holding scaled/translated coordinates
    FEndPoint            : TPoint;       // this field is for common use that may holding scaled/translated coordinates
    FActualStartPoint    : TPoint;       // this field is for common use that may holding scaled but not translated coordinates
    FActualEndPoint      : TPoint;       // this field is for common use that may holding scaled but not translated coordinates
    FDrawingBasePoint    : TPoint;
    FPrevStrokePoint     : TPoint;
    FMagnification       : Integer;
    FPrevWheelDelta      : Integer;
    
    FCommandManager      : TgmCommandManager;  // Undo/Redo commands
    FHistoryBitmap       : TBitmap32;

    FLayerList           : TgmLayerList;
    FChannelManager      : TgmCustomChannelManager;
    FCheckerboardBmp     : TBitmap32;   // holding checkerboard pattern for background rendering

    FHandlePaintStage    : PPaintStage;
    FHandleStageIndex    : Integer;

{ for Standard tools}
    FOldFigure           : TgmFigureObject;        // For Undo/Redo
    FCurvePoint1         : TPoint;
    FCurvePoint2         : TPoint;
    FActualCurvePoint1   : TPoint;
    FActualCurvePoint2   : TPoint;
    FDrawCurveTime       : Integer;
    FPolygon             : array of TPoint;
    FActualPolygon       : array of TPoint;
    FMoveDrawingState    : TgmDrawingState;   // state of the Move tool
    FMoveDrawingHandle   : TgmDrawingHandle;
    FRegularBasePoint    : TPoint;
    FRegionSelectOK      : Boolean;
    FFigureManager       : TgmLayerFigureManager;  // used for managing the figures that on figure layers
    FPencilMask          : TBitmap32;

{ for Brush tools }
    Felozox, Felozoy: Integer;
    Ftavolsag       : Double;
    FAimPoint       : TPoint;

{ for Measure tool }
    FMeasureLine          : TgmMeasureLine;
    FMeasureDrawingState  : TgmDrawingState;
    FMeasurePointSelector : TgmMeasurePointSelector;  // indicating current endpoint type of the measure line
    FMeasureLayer         : TBitmapLayer;
    
{ for Marquee tools }
    FSelection                : TgmSelection;
    FSelectionCopy            : TgmSelection;      // Used for selection undo/redo command.
    FSelectionTranslateTarget : TgmTranslateTarget;
    FMarqueeDrawingState      : TgmDrawingState;
    FMarqueeDrawingHandle     : TgmDrawingHandle;  // which handle the mouse is hovered
    FRegion                   : TgmRegion;

    // Magnetic Lasso
    FMagneticLasso            : TgmMagneticLasso;
    FMagneticLassoLayer       : TBitmapLayer;

{ for Transform tools }
    FSelectionTransformation  : TgmSelectionTransformation;
    FTransformCopy            : TgmSelectionTransformation;  // Used for Undo/Redo
    FLastTransformMode        : TgmTransformMode;
    FTransformHandle          : TgmDrawingHandle;
    FRotateRadiansInMouseDown : Extended;
    FRotateRadiansInMouseMove : Extended;

{ for Crop tool }
    FCrop              : TgmCrop;
    FCropDrawingState  : TgmDrawingState;
    FCropDrawingHandle : TgmDrawingHandle;

{ for Pen Path tools }
    FPathList              : TgmPathList;       
    FPathLayer             : TBitmapLayer;
    FPathSelectHandle      : TgmPathSelectHandle;
    FMouseDownX            : Integer;
    FMouseDownY            : Integer;
    FMouseMoveX            : Integer;
    FMouseMoveY            : Integer;
    FWholePathIndex        : Integer;       // index of whole selected path
    FOriginalPairState     : TgmPairState;  // indicating whether modify the direction line 1 and 2 simultaneously
    FOppositeLineOperation : TgmOppositeLineOperation;

    // for Undo/Redo
    FOldCurvePathList      : TgmCurvePathList;
    FPathModificationMode  : TgmPathModificationMode;

{ for Shape Region tools }
    FShapeDrawingHandle : TgmDrawingHandle;
    FShapeDrawingState  : TgmDrawingState;
    FRegionPolygon      : array [0 .. 99] of TPoint;

{ for Text tool }
    FRichTextDrawingState  : TgmDrawingState;
    FRichTextDrawingHandle : TgmDrawingHandle;
    FOldTextStream         : TMemoryStream;    // for Undo/Redo

{ callback functions }

    // layer callback functions
    procedure AfterLayerCombined(ASender: TObject; const ARect: TRect);
    procedure AfterLayerMaskApplied(ASender: TObject);
    procedure AfterLayerMaskDisabled(ASender: TObject);
    procedure AfterLayerMaskEnabled(ASender: TObject);
    procedure AfterLayerMerged(AResultLayer: TgmCustomLayer);
    procedure AfterLayerOrderChanged(ASender: TObject; const AOldIndex, ANewIndex: Integer);
    procedure AfterLayerPanelChanged(ASender: TObject);
    procedure AfterLayerProcessStageChanged(ASender: TObject; const AStage: TgmLayerProcessStage);
    procedure AfterSelectedLayerPanelChanged(ASender: TObject);

    procedure BrightContrastLayerLogoThumbDblClick(ASender: TObject);
    procedure ChannelMixerLayerLogoThumbDblClick(ASender: TObject);
    procedure ColorBalanceLayerLogoThumbDblClick(ASender: TObject);
    procedure CurvesLayerLogoThumbDblClick(ASender: TObject);
    procedure GradientFillLayerLogoThumbDblClick(ASender: TObject);
    procedure GradientMapLayerLogoThumbDblClick(ASender: TObject);
    procedure HueSaturationLayerLogoThumbDblClick(ASender: TObject);
    procedure LevelsLayerLogoThumbDblClick(ASender: TObject);
    procedure PatternLayerLogoThumbDblClick(ASender: TObject);
    procedure PosterizeLayerLogoThumbDblClick(ASender: TObject);
    procedure RichTextLayerLogoThumbDblClick(ASender: TObject);
    procedure ShapeRegionLayerLogoThumbDblClick(ASender: TObject);
    procedure ShapeRegionLayerPanelDblClick(ASender: TObject);
    procedure SolidColorLayerLogoThumbDblClick(ASender: TObject);
    procedure ThresholdLayerLogoThumbDblClick(ASender: TObject);
    
    procedure OnLayerPanelDisabled(ASender: TObject);
    procedure OnLayerPanelEnabled(ASender: TObject);

    // channel callback functions
    procedure OnAlphaChannelDelete(ASender: TObject);
    procedure OnAlphaChannelOrderChanged(AList: TgmAlphaChannelList; const AOldIndex, ANewIndex: Integer);
    procedure OnChannelDblClick(AChannel: TgmCustomChannel; const AChannelType: TgmChannelType);
    procedure OnChannelThumbnailUpdate(ASender: TObject);
    procedure OnChannelVisibleChange(const AChannelType: TgmChannelType);
    procedure OnInsertAlphaChannel(AList: TgmAlphaChannelList; const AIndex: Integer);
    procedure OnLayerMaskChannelDelete(ASender: TObject);
    procedure OnQuickMaskChannelCreate(ASender: TObject);
    procedure OnQuickMaskChannelDelete(ASender: TObject);
    procedure OnSelectedChannelChanged(const ACurrentChannelType: TgmChannelType);

    // path callback functions
    procedure AfterPathDeleted(ASender: TObject);
    procedure AfterPathInserted(ASender: TObject; const AIndex: Integer);
    procedure AfterSelectedPathChanged(ASender: TObject);

    // command manager callback functions ...

    procedure AfterCommandAdded(ASender: TObject);

    procedure AfterSelectedCommandChanged(ACommandList: TgmCommandList;
      ASelectedCommand: TgmCustomCommand; const AOldIndex, ACurrIndex: Integer);

    procedure AfterSelectedSnapshotChanged(ASnapshotList: TgmSnapshotList;
      ASelectedSnapshot: TgmSnapshot; const AOldIndex, ACurrIndex: Integer);

    // other callback functions
    procedure ImageViewerScaleChange(ASender: TObject);
    
    // callback function for OnPixelCombine event of FMeasureLayer.Bitmap
    procedure MeasureLayerBlend(F: TColor32; var B: TColor32; M: TColor32);

    // callback function for OnPixelCombine event of FPathLayer.Bitmap
    procedure PathLayerBlend(F: TColor32; var B: TColor32; M: TColor32);

    procedure MagneticLassoLayerBlend(F: TColor32; var B: TColor32; M: TColor32);

{ methods for Main form }
    procedure UpdateMainFormStatusBarWhenMouseDown;

{ methods for Child form }

    procedure InitializeCanvasesForFigureTools;
    procedure BlendLayersAndChannelsToBitmap(ADestBmp: TBitmap32);

    procedure BeforeExit(Sender: TObject);  // confirm to save the last change to the image when exit the main program

    // Show/Hide assistant layer -- FHandleLayer, FPathLayer
    procedure SetAssistantLayerVisible(const IsVisible: Boolean);

{ Methods for Standard Tools }
    procedure SetPencilStipplePattern(ADestBmp: TBitmap32;
      const APenStyle: TPenStyle; const AColor1, AColor2: TColor32);

    // Adapted from RebuildBrush() which by Zoltan in gr32PaintDemo3.
    procedure BuildPencilStroke(ADest: TBitmap32);

    // Adapted from BrushLine() which by Zoltan in gr32PaintDemo3.
    procedure PencilLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
      ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);

    procedure PencilLineOnMask(
      const xStart, yStart, xEnd, yEnd, distance: Integer;
      const ChannelSet: TgmChannelSet);

    procedure PreparePencilTool;

    procedure ProcessFigureMouseUpOnLayer(const AShiftState: TShiftState);
    procedure ProcessFigureMouseUpOnSelection(const ShiftState: TShiftState);
    procedure ProcessFigureMouseUpOnSpecialChannels(const ShiftState: TShiftState);

    procedure FinishCurveOnLayer;
    procedure FinishCurveOnSelection;
    procedure FinishCurveOnSpecialChannels;
    
    procedure FinishPolygonOnLayer;
    procedure FinishPolygonOnSelection;
    procedure FinishPolygonOnSpecialChannels;

    function CanAddPointToPolygon(const APolygon: array of TPoint;
      const ATextPoint: TPoint; const AVertexDistance: Double): Boolean;

{ methods for Brush tools }

    procedure BrushLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
      ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);

    procedure AirBrushLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
      ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);

    procedure AirBrushLineOnMask(
      const xStart, yStart, xEnd, yEnd, distance: Integer;
      const ChannelSet: TgmChannelSet);

{ methods for Eraser tools }

    procedure EraserLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
      ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);

    procedure EraserLineOnMask(const xStart, yStart, xEnd, yEnd, distance: Integer;
      const ChannelSet: TgmChannelSet);

{ methods for Marquee tools }
    procedure FinishPolygonalSelection;
    procedure PauseMarchingAnts;     // pause drawing the Marching-Ants lines
    procedure CreateLassoLayer;      // Magnetic Lasso

{ methods for measure tool }

    procedure CreateMeasureLayer;

{ methods for Pen Path tools }
    function GetPenToolDefaultCursor: TCursor;

    procedure SaveOldCurvePathForUndoRedo;

{ methods for Shape Region tools }
    procedure CalcVertexForLineRegionOutline;

{ methods for Text tool }

    procedure AssociateTextEditorToRichTextLayers;

{ Methods for Undo/Redo }

    function GetCommandIconResourceNameForMarqueeTools(
      const AMarqueeTool: TgmMarqueeTools): string;

    function GetCommandNameForMarqueeTools(
      const AMarqueeTool: TgmMarqueeTools): string;

{ Mouse Events }
    // Calculate the coordinate of the mouse on the selection -- calling it after
    // the CalcLayerCoord() method has been called for getting the right coordinate. 
    procedure CalcSelectionCoord;

{ events for standard tools }

    procedure PencilMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure PencilMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure PencilMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure FigureToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure FigureToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure FigureToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for figure managing tools }

    // The following three mouse events are used for managing figures that
    // on figure layers, such as select figures and move them around,
    // lock/unlock away figures on layer, etc.

    procedure FigureManagingToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure FigureManagingToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure FigureManagingToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for brush tools }

    procedure BrushMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure BrushMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure BrushMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for marquee tools }

    // translate selection by keyboard strokes
    procedure TranslateSelectionKeyDown(var Key: Word; Shift: TShiftState);
    procedure TranslateSelectionKeyUp(var Key: Word; Shift: TShiftState);

    procedure MarqueeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure MarqueeMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MarqueeMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for gradient tools }

    procedure GradientToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure GradientToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure GradientToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for crop tool }

    procedure CropMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure CropMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure CropMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    // translate crop area by keyboard strokes
    procedure TranslateCropKeyDown(var Key: Word; Shift: TShiftState);

{ events for paint bucket tools }

    procedure PaintBucketMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure PaintBucketMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure PaintBucketMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for eraser tools }

    procedure EraserMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure EraserMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure EraserMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for pen path tools  }

    procedure PenToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure PenToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure PenToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for measure tool }

    procedure MeasureMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure MeasureMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MeasureMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    // translate Measure Line by keyboard strokes
    procedure TranslateMeasureKeyDown(var Key: Word; Shift: TShiftState);
    procedure TranslateMeasureKeyUp(var Key: Word; Shift: TShiftState);

{ events for shape region tools }

    // translate shape regions by keyboard strokes
    procedure TranslateShapeRegionKeyDown(var Key: Word; Shift: TShiftState);
    procedure TranslateShapeRegionKeyUp(var Key: Word; Shift: TShiftState);

    procedure ShapeRegionToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure ShapeRegionToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure ShapeRegionToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for Transform tools}
    procedure TransformSelectionMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure TransformSelectionMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure TransformSelectionMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for Text tool }

    procedure TextToolsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure TextToolsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure TextToolsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    // translate Text by keyboard stroke
    procedure TranslateTextKeyDown(var Key: Word; Shift: TShiftState);
    procedure TranslateTextKeyUp(var Key: Word; Shift: TShiftState);

{ events for Eyedropper tool }

    procedure EyedropperMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure EyedropperMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure EyedropperMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ events for Hand tool }

    procedure HandToolMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure HandToolMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure HandToolMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

{ property methods }
    procedure SetEditMode(const Value: TgmEditMode);
  public
{ for child form }

{ for Standard tools }
    FSelectedFigure : TgmFigureObject; // pointer to the selected figure
    
{ for Pen Path tools }
    FCurvePath : TgmCurvePath;    // point to current selected curve path

{ common }
    procedure UpdateThumbnailsBySelectedChannel;

{ methods for Child form }
    function ToolsInPendingStatus: Boolean;  // return true if any tool in its pending status

    procedure ChangeImageCursorByToolTemplets;
    procedure ConnectMouseEventsToImage;

    procedure CreateUndoRedoCommandForPixelProcessing(
      const ACommandName: string; const ACommandIconResName: string = '');  // for Undo/Redo

    procedure LoadDataFromGMDFile(const AFileName: string);   
    procedure RefreshCaption;  // refresh the caption of this form
    procedure SaveChannelPixelsBeforePixelProcessing;   // for Undo/Redo
    procedure SaveFileWithNewName;
    procedure SaveNamedFile;
    procedure SetupOnChildFormActivate;

{ layer methods }
    function CreateBrightContrastLayer(const ABrightAmount: Integer = 0;
      const AContrastAmount: Integer = 0): TgmCustomLayer;

    function CreateChannelMixerLayer: TgmCustomLayer;

    function CreateColorBalanceLayer(
      const ATransferMode: TgmTransferMode = tmMidtones;
      const ACyanRedValue: Integer = 0;
      const AMagentaGreen: Integer = 0;
      const AYellowBlue: Integer = 0;
      const APreserveLuminosity: Boolean = True): TgmCustomLayer;

    function CreateCurvesLayer: TgmCustomLayer;
    function CreateGradientFillLayer: TgmCustomLayer;
    function CreateGradientMapLayer: TgmCustomLayer;

    function CreateHueSaturationLayer(
      const AMode: TgmHueSaturationAdjustMode = hsamHSL;
      const AHue: Integer = 0; const ASaturation: Integer = 0;
      const ALightValue: Integer = 0): TgmCustomLayer;

    function CreateInvertLayer: TgmCustomLayer;
    function CreateLevelsLayer: TgmCustomLayer;

    function CreateNormalLayer(const ABackColor: TColor32 = $00000000;
      const AsBackLayer: Boolean = False): TgmCustomLayer;

    function CreatePatternLayer: TgmCustomLayer;
    function CreatePosterizeLayer(const ALevel: Byte = 2): TgmCustomLayer;
    function CreateRichTextLayer: TgmCustomLayer;
    function CreateShapeRegionLayer: TgmCustomLayer;
    function CreateSolidColorLayer(const AColor: TColor32 = clBlack32): TgmCustomLayer;
    function CreateThresholdLayer(const ALevel: Byte = 127): TgmCustomLayer;
    function CreateVectorLayer: TgmCustomLayer;

    procedure SetCallbacksForLayersInList;
    procedure DeleteCurrentLayer;

{ channel methods }
    procedure LoadChannelAsSelection;    // channel to selection
    procedure LoadQuickMaskAsSelection;  // channel methods

    // Undo/Redo
    procedure SetEditModeForUndoRedo(const AEditMode: TgmEditMode);

{ methods for Standard tools}
    procedure ChangeImageCursorByStandardTools;
    procedure FinishCurves;
    procedure FinishPolygon;

{ methods for Marquee tools }
    procedure FreeSelection;         // just free the selection object and do nothing else
    procedure FreeCopySelection;
    procedure CreateCopySelection;

    // Undo/Redo
    function GetSelectionForUndoRedo(const ACreateIfNone: Boolean): TgmSelection;
    function GetClipboardSelectionForUndoRedo(const ACreateIfNone: Boolean): TgmSelection;
    procedure DeleteSelectionForUndoRedo;
    procedure DeleteClipboardSelectionForUndoRedo;

{ methods for Marquee tools }
    function MakeSelectionFeather(const ARadius: Integer): Boolean;

    procedure CreateNewSelection;
    procedure CancelSelection;
    procedure ChangeImageCursorByMarqueeTools;
    procedure ChangeSelectionTarget;
    procedure CommitSelection;

    procedure CreateSelectionByColorRange(ASourceBmp: TBitmap32;
      const ASampledColor: TColor32; const AFuzziness: Integer);

    procedure CreateSelectionForAll;
    procedure DeleteSelection;
    procedure FinishMagneticLasso;
    procedure MakeSelectionInverse;
    procedure ShowProcessedSelection(const AUpdateDisplay: Boolean = True);
    procedure ShowSelectionAtBrushStroke(const ARect: TRect);

{ methods for Transform tools }
    procedure ConnectTransformMouseEvents;
    procedure CreateSelectionTransformation(const AMode: TgmTransformMode);
    procedure FinishTransformation;
    procedure FreeSelectionTransformation;

    // for Undo/Redo
    function EnterSelectionTransformModeForUndoRedo(
      const ATransformMode: TgmTransformMode;
      const ACreateIfNone: Boolean): TgmSelectionTransformation;

    // for Undo/Redo
    procedure ExitSelectionTransfromModeForUndoRedo;

    // for Undo/Redo
    function GetSelectionTransformationForUndoRedo: TgmSelectionTransformation;

{ methods for Crop tool }
    procedure CommitCrop;
    procedure CancelCrop;
    procedure FinishCrop;
    procedure ExecuteOptimalCrop;

{ methods for Eraser Tools }
    procedure ChangeImageCursorByEraserTools;

{ methods for Pen Path tools }
    procedure ChangeImageCursorByPenTools;
    procedure CreatePathLayer;
    procedure DrawPathOnPathLayer;
    procedure FitPathLayerToViewport;

{ methods for Pen Path tools }
    procedure DeletePathLayer;
    procedure LoadPathAsSelection;  // Path to Selection 

{ methods for Measure tool }
    procedure ShowMeasureResult;
    procedure FitMeasureLayerToViewport;
    procedure DrawMeasureLineOnMeasureLayer;
    procedure RemoveMeasureLayer;
    procedure RemoveMeasureLine;

{ methods for Shape Region tools }
    procedure ChangeImageCursorByShapeTools;
    
{ methods for Text tool }
    procedure CommitEdits;
    procedure CancelEdits;

{ methods for Undo/Redo }
    procedure CreateNewLayerCommand(ANewLayer: TgmCustomLayer; const ANewLayerIndex: Integer);

{ Properties }
    property ChannelManager          : TgmCustomChannelManager    read FChannelManager;
    property CheckerboardBmp         : TBitmap32                  read FCheckerboardBmp;
    property CommandManager          : TgmCommandManager          read FCommandManager;
    property Crop                    : TgmCrop                    read FCrop;
    property EditMode                : TgmEditMode                read FEditMode            write SetEditMode;
    property FigureManager           : TgmLayerFigureManager      read FFigureManager;
    property FileName                : string                     read FFileName            write FFileName;
    property HistoryBitmap           : TBitmap32                  read FHistoryBitmap;
    property IsMayClick              : Boolean                    read FMayClick            write FMayClick;
    property LayerList               : TgmLayerList               read FLayerList;
    property MarqueeDrawingState     : TgmDrawingState            read FMarqueeDrawingState write FMarqueeDrawingState;
    property MagneticLasso           : TgmMagneticLasso           read FMagneticLasso;
    property Magnification           : Integer                    read FMagnification       write FMagnification;
    property MeasureLayer            : TBitmapLayer               read FMeasureLayer;
    property MeasureLine             : TgmMeasureLine             read FMeasureLine;
    property OldTextStream           : TMemoryStream              read FOldTextStream;
    property PathLayer               : TBitmapLayer               read FPathLayer;
    property PathList                : TgmPathList                read FPathList;
    property Selection               : TgmSelection               read FSelection;
    property SelectionCopy           : TgmSelection               read FSelectionCopy;
    property SelectionTransformation : TgmSelectionTransformation read FSelectionTransformation;
    property ShapeDrawingState       : TgmDrawingState            read FShapeDrawingState   write FShapeDrawingState;
  end;

var
  frmChild: TfrmChild;

const
  DEFAULT_CHECKERBOARD_SIZE = 8;

implementation

uses
{ Delphi }
  Math, GIFImage,
{ Graphics32 Lib }
  GR32_Backends,
  GR32_LowLevel,
{ externals }
  GR32_Add_BlendModes,         // BlendMode for layer blending
  ColorLibrary,                // Color Conversion Routines
  LineLibrary,                 // AddPoints(), SubtractPoints()...
{ GraphicsMagic Package Lib }
  gmGradient,
  gmGradientRender,
{ GraphicsMagic Lib }
  gmAlphaFuncs,                // Functions for alpha processing
  gmBlendModes,
  gmBrightContrastLayer,
  gmBrushes,
  gmChannelCommands,
  gmChannelMixer,
  gmChannelMixerLayer,
  gmColorBalanceLayer,
  gmCommonFuncs,
  gmComplexCommands,
  gmConstants,
  gmConvolve,
  gmCurvesLayer,
  gmCurvesTool,
  gmGimpHistogram,
  gmGMDFile,                   // used to ouput work flow to disk with extension name '.gmd'
  gmGradientFillLayer,
  gmGradientMapLayer,
  gmGUIFuncs,
  gmHueSaturationLayer,
  gmImageProcessFuncs,         // FlattenImageToBitmap32()...
  gmIni,
  gmInvertLayer,
  gmIO,                        // LoadGraphicsFile(), SaveGraphicsFile()
  gmLayerCommands,
  gmLevelsLayer,
  gmLevelsTool,
  gmMath,
  gmMiscCommandIcons,
  gmPaintBucket,
  gmPaintFuncs,                // DrawCheckboardPattern()
  gmPatternLayer,
  gmPosterizeLayer,
  gmRichTextLayer,
  gmRichTextLayerCommands,
  gmRGBChannelManager,
  gmSelectionCommands,
  gmShapeRegionLayer,
  gmShapes,
  gmShapeRegionLayerCommands,
  gmSolidColorLayer,
  gmThresholdLayer,
  gmVectorLayer,
  gmVectorLayerCommands,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms }
  BrushDynamicsPopFrm,         // frmBrushDynamics
  ChannelForm,
  ColorForm,
  EraserAdvOptionsPopFrm,      // frmEraserAdvancedOptions
  GradientPickerPopFrm,        // frmGradientPicker
  HistoryForm,
  LayerForm,
  MainForm,
  PaintBucketOptionsPopFrm,    // frmPaintBucketAdvancedOptions
  PaintingBrushPopFrm,
  PathForm,
  PatternsPopFrm,
  RichTextEditorForm,          // frmRichTextEditor
{ GraphicsMagic Dialogs }
  BrightnessContrastDlg,
  ChannelMixerDlg,
  ChannelOptionsDlg,
  ColorBalanceDlg,
  CurvesDlg,
  GradientFillDlg,
  GradientMapDlg,
  HueSaturationDlg,
  IndexedColorDlg,
  LevelsToolDlg,
  PatternFillDlg,
  PosterizeDlg,
  SavePathDlg,
  ThresholdDlg;

{$R *.DFM}

const
  PAINT_STAGE_ZERO_PARAMETER = 100;
  HANDLE_STAGE_PARAMETER     = 101;  


function ImageClientRectToBitmapRect(AImageControl: TCustomImage32): TRect;
begin
  Result := AImageControl.ClientRect;

  with Result do
  begin
    TopLeft     := AImageControl.ControlToBitmap(TopLeft);
    BottomRight := AImageControl.ControlToBitmap(BottomRight);
  end;
end;

// return true if any tool in its pending status
function TfrmChild.ToolsInPendingStatus: Boolean;
begin
  Result := False;

  if Length(FPolygon) > 1 then  // polygonal vector
  begin
    Result := True;
  end
  else if FDrawCurveTime > 0 then  // curve vector
  begin
    Result := True;
  end
  else if Assigned(FRegion) then  // polygonal selection
  begin
    if FRegion.RegionStyle = gmrsPolygonal then
    begin
      Result := True;
    end;
  end
  else if Assigned(FMagneticLasso) then  // magnetic lasso
  begin
    Result := True;
  end
  else if Assigned(FCrop) then  // crop tool
  begin
    Result := True;
  end
  else if Assigned(FSelectionTransformation) then  // transform tools
  begin
    Result := True;
  end
  else if frmRichTextEditor.Visible then  // editing text
  begin
    Result := True;
  end;
end;

procedure TfrmChild.AfterCommandAdded(ASender: TObject);
begin
  frmHistory.CommandViewer.RefreshViewer();

  // mark the image has been modified 
  if not FImageProcessed then
  begin
    FImageProcessed := True;
  end;
end;

procedure TfrmChild.AfterLayerCombined(ASender: TObject; const ARect: TRect);
begin
  if Assigned(FLayerList) then
  begin
    // rendering the checkerboard pattern as background first
    imgWorkArea.Bitmap.Draw(ARect, ARect, FCheckerboardBmp);

    // then rendering the combined result of layers on the background
//    imgWorkArea.Bitmap.Draw(ARect, ARect, FLayerList.CombineResult);

    // here, we don't draw layer combined result onto the background,
    // instead, we tweak the combined result with color channel settings,
    // and then merge the new result with the background ...
    FChannelManager.BlendByColorChannelSettings(
      LayerList.CombineResult, imgWorkArea.Bitmap, ARect);

    imgWorkArea.Bitmap.Changed(ARect);
  end;
end;

procedure TfrmChild.AfterLayerMaskApplied(ASender: TObject);
begin
  FChannelManager.DeleteLayerMaskChannel;
          
  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate;
  end;
end;

procedure TfrmChild.AfterLayerMaskDisabled(ASender: TObject);
begin
  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate;
  end;

  FChannelManager.DeleteLayerMaskChannel;
          
  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate;
  end;
end;

procedure TfrmChild.AfterLayerMaskEnabled(ASender: TObject);
begin
  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate;
  end;

  FChannelManager.CreateLayerMaskChannel(
    FLayerList.SelectedLayer.MaskBitmap,
    FLayerList.SelectedLayer.LayerName + ' Mask');

  if Assigned(FChannelManager.LayerMaskChannel) then
  begin
    FChannelManager.SelectLayerMaskChannel;
  end;

  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate;
  end;
end;

procedure TfrmChild.AfterLayerMerged(AResultLayer: TgmCustomLayer);
begin
  // setting callback functions for the result layer
  if Assigned(AResultLayer) and (AResultLayer is TgmNormalLayer) then
  begin
    with TgmNormalLayer(AResultLayer) do
    begin
      OnChange              := Self.AfterLayerPanelChanged;
      OnMaskApplied         := Self.AfterLayerMaskApplied;
      OnMaskDisabled        := Self.AfterLayerMaskDisabled;
      OnMaskEnabled         := Self.AfterLayerMaskEnabled;
      OnMaskThumbDblClick   := nil;
      OnLayerDisabled       := Self.OnLayerPanelDisabled;
      OnLayerEnabled        := Self.OnLayerPanelEnabled;
      OnLayerThumbDblClick  := nil;
      OnPanelDblClick       := nil;
      OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
      OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
    end;
  end;

  FFigureManager.UpdateSelectedFiguresInfo();
  FLayerList.SelectedLayer.Changed();
  frmMain.UpdateToolsOptions();
end;

procedure TfrmChild.AfterSelectedCommandChanged(ACommandList: TgmCommandList;
  ASelectedCommand: TgmCustomCommand; const AOldIndex, ACurrIndex: Integer);
var
  LSizeChanged : Boolean;
  LRect        : TRect;
begin
  LSizeChanged := False;

  if AOldIndex <> ACurrIndex then
  begin
    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated := False;
    end;

    FCommandManager.SnapshotList.DeselectAllSnapshots();

    if AOldIndex < ACurrIndex then
    begin
      FCommandManager.RedoCommands(AOldIndex + 1, ACurrIndex);
    end
    else if AOldIndex > ACurrIndex then
    begin
      FCommandManager.UndoCommands(AOldIndex, ACurrIndex + 1);
    end;

    frmHistory.CommandViewer.RefreshViewer();

    // there were crop operations in the Undo/Redo list
    if (FLayerList.SelectedLayer.LayerBitmap.Width <> imgWorkArea.Bitmap.Width) or
       (FLayerList.SelectedLayer.LayerBitmap.Height <> imgWorkArea.Bitmap.Height) then
    begin
      // set background size before create background layer
      imgWorkArea.Bitmap.SetSize(FLayerList.SelectedLayer.LayerBitmap.Width,
                                 FLayerList.SelectedLayer.LayerBitmap.Height);
                                 
      imgWorkArea.Bitmap.Clear($00000000);

      // draw checkerboard pattern
      CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
      DrawCheckerboardPattern(CheckerboardBmp, CheckerboardBmp.ClipRect);

      // set location for channel layers ...
      LRect             := imgWorkArea.GetBitmapRect();
      LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

      FChannelManager.ChannelLayerLocation := FloatRect(LRect);

      LSizeChanged := True;
    end;

    if FLayerList.SelectedLayer.IsMaskEnabled then
    begin
      if not Assigned(FChannelManager.LayerMaskChannel) then
      begin
        FChannelManager.CreateLayerMaskChannel(
          FLayerList.SelectedLayer.MaskBitmap,
          FLayerList.SelectedLayer.LayerName + ' Mask');
      end
      else
      begin
        with FChannelManager.LayerMaskChannel do
        begin
          ChannelLayer.Bitmap.BeginUpdate();
          try
            // Don't directly using Assign() at here.
            // Must drawing on channel layer. 
            ChannelLayer.Bitmap.SetSizeFrom(FLayerList.SelectedLayer.MaskBitmap);
            ChannelLayer.Bitmap.Draw(0, 0, FLayerList.SelectedLayer.MaskBitmap);
            UpdateChannelThumbnail();
          finally
            ChannelLayer.Bitmap.EndUpdate();
          end;
        end;
      end;
    end
    else
    begin
      if Assigned(FChannelManager.LayerMaskChannel) then
      begin
        FChannelManager.DeleteLayerMaskChannel();
      end;
    end;

    // set callback functions for the layers in the layer list
    SetCallbacksForLayersInList();
    AssociateTextEditorToRichTextLayers();

    // deselect all figures on vector layers
    FFigureManager.DeselectAllFigures();

    // dealing with paths
    if Assigned(FPathList.SelectedPath) then
    begin
      FCurvePath := FPathList.SelectedPath.CurvePathList.SelectedPath;

      if not Assigned(FPathLayer) then
      begin
        CreatePathLayer();
      end
      else
      begin
        // there were crop operations in the Undo/Redo list
        if LSizeChanged then
        begin
          FitPathLayerToViewport();
        end;
      end;

      FPathLayer.Bitmap.Clear($00000000);

      FPathList.SelectedPath.CurvePathList.DrawAllPaths(FPathLayer.Bitmap.Canvas,
        pmNotXor, imgWorkArea.BitmapToControl);

      if Assigned(FCurvePath) then
      begin
        if not FCurvePath.CurveSegmentsList.IsClosed then
        begin
          FPathList.SelectedPath.CurvePathList.Status := cplsAddNextAnchorPoint;
        end;
      end;
    end
    else
    begin
      FCurvePath := nil;
      DeletePathLayer();
    end;
    
    FLayerList.SelectedLayer.Changed();

    // there were crop operations in the Undo/Redo list
    if LSizeChanged then
    begin
      // channels ...
      LRect             := imgWorkArea.GetBitmapRect();
      LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

      FChannelManager.UpdateColorChannelThumbnails(FLayerList.CombineResult);
      FChannelManager.ChannelLayerLocation := FloatRect(LRect);
    end;

    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated := True;
    end;

    frmLayers.LayerPanelManager.Invalidate();
    frmChannels.ChannelViewer.Invalidate();
    frmPaths.PathManager.Invalidate();

    // refresh the viewer for erasing unwanted things, such as a border of
    // a selection
    imgWorkArea.Update(imgWorkArea.ClientRect);
  end;
end;

procedure TfrmChild.AfterSelectedLayerPanelChanged(ASender: TObject);
begin
  if Assigned(frmLayers) then
  begin
    FChannelManager.AlphaChannelList.HideAllChannels();

    case FLayerList.SelectedLayer.LayerProcessStage of
      lpsLayer:
        begin
          FChannelManager.SelectColorChannel(0, True);
        end;

      lpsMask:
        begin
          FChannelManager.SelectLayerMaskChannel();
        end;
    end;

    frmLayers.LayerPanelManager.Invalidate();

    // switch selection target
    if Assigned(FSelection) then
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel,
        ctQuickMaskChannel,
        ctLayerMaskChannel:
          begin
            ChangeSelectionTarget();
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              ChangeSelectionTarget();
            end;
          end;
      end;
    end;

    with frmLayers do
    begin
      LockChange();
      try
        cmbbxBlendModes.ItemIndex := Ord(FLayerList.SelectedLayer.LayerBlendMode);
        ggbrLayerOpacity.Position := MulDiv(FLayerList.SelectedLayer.LayerOpacity, 100, 255);
      finally
        UnlockChange();
      end;
    end;

    if FLayerList.SelectedLayer.IsMaskEnabled then
    begin
      if Assigned(FChannelManager.LayerMaskChannel) then
      begin
        // replace the mask channel data with the mask settings on current layer

        FChannelManager.LayerMaskChannel.ChannelName :=
          FLayerList.SelectedLayer.LayerName + ' Mask';

        // Note that, at here, we should always update the bitmap of
        // layer mask channel with Draw(), other than Assign(),
        // otherwise, the DrawMode of the layer mask bitmap will be
        // changed from dmCustom to dmOpaque.
        FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(0, 0,
          FLayerList.SelectedLayer.MaskBitmap);

        FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
      end
      else
      begin
        FChannelManager.CreateLayerMaskChannel(
          FLayerList.SelectedLayer.MaskBitmap,
          FLayerList.SelectedLayer.LayerName + ' Mask');
      end;

      case FLayerList.SelectedLayer.LayerProcessStage of
        lpsLayer:
          begin
            FChannelManager.SelectColorChannel(0, True);
          end;

        lpsMask:
          begin
            FChannelManager.SelectLayerMaskChannel();
          end;
      end;
    end
    else
    begin
      if Assigned(FChannelManager.LayerMaskChannel) then
      begin
        FChannelManager.DeleteLayerMaskChannel();
      end;
    end;

    frmChannels.ChannelViewer.Invalidate();

    // shape region tools
    if (FLayerList.SelectedLayer is TgmShapeRegionLayer) and
       (frmMain.MainTool = gmtShape) then
    begin
      TgmShapeRegionLayer(FLayerList.SelectedLayer).IsDismissed := False;
      imgWorkArea.Changed();

      case TgmShapeRegionLayer(FLayerList.SelectedLayer).BrushStyle of
        bsSolid:
          begin
            dmMain.actnRegionSolidBrush.Execute();
          end;

        bsHorizontal:
          begin
            dmMain.actnRegionHorizontalBrush.Execute();
          end;

        bsVertical:
          begin
            dmMain.actnRegionVerticalBrush.Execute();
          end;

        bsFDiagonal:
          begin
            dmMain.actnRegionFDiagonalBrush.Execute();
          end;

        bsBDiagonal:
          begin
            dmMain.actnRegionBDiagonalBrush.Execute();
          end;

        bsCross:
          begin
            dmMain.actnRegionCrossBrush.Execute();
          end;

        bsDiagCross:
          begin
            dmMain.actnRegionDiagCrossBrush.Execute();
          end;
      end;
    end;

    // text tool
    if (FLayerList.SelectedLayer is TgmRichTextLayer) and
       (frmMain.MainTool = gmtTextTool) then
    begin
      TgmRichTextLayer(FLayerList.SelectedLayer).UpdateContentToAssociatedTextEditor();
      imgWorkArea.Changed();
    end;
  end;
end;

procedure TfrmChild.AfterLayerOrderChanged(ASender: TObject;
  const AOldIndex, ANewIndex: Integer);
var
  LCommand     : TgmCustomCommand;
  LCommandName : string;
begin
  if (ASender = FLayerList) and (AOldIndex <> ANewIndex) then
  begin
    case dmMain.LastLayerOrderType of
      lotNone:
        begin
          LCommandName := 'Layer Order'
        end;

      lotBringToFront:
        begin
          LCommandName := 'Bring To Front';
        end;

      lotBringForward:
        begin
          LCommandName := 'Bring Forward';
        end;

      lotSendBackward:
        begin
          LCommandName := 'Send Backward';
        end;

      lotSendToBack:
        begin
          LCommandName := 'Send To Back';
        end;
    end;
    
    LCommand := TgmLayerOrderCommand.Create(LCommandName,
                                            FChannelManager, FLayerList,
                                            AOldIndex, ANewIndex);

    FCommandManager.AddCommand(LCommand);

    frmLayers.LayerPanelManager.Invalidate();
    dmMain.LastLayerOrderType := lotNone;  // reset 
  end;
end;

procedure TfrmChild.AfterLayerPanelChanged(ASender: TObject);
begin
  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate;
  end;

  FChannelManager.UpdateColorChannelThumbnails(FLayerList.CombineResult);
end;

procedure TfrmChild.AfterLayerProcessStageChanged(ASender: TObject;
  const AStage: TgmLayerProcessStage);
begin
  case AStage of
    lpsLayer:
      begin
        FChannelManager.SelectColorChannel(0, True);
      end;

    lpsMask:
      begin
        FChannelManager.SelectLayerMaskChannel;
      end;
  end;

  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate;
  end;

  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate;
  end;
end;

procedure TfrmChild.AssociateTextEditorToRichTextLayers;
var
  i          : Integer;
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  if FLayerList.Count > 0 then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      if LLayer is TgmRichTextLayer then
      begin
        LTextLayer := TgmRichTextLayer(LLayer);
        LTextLayer.AssociateTextEditor(frmRichTextEditor.rchedtRichTextEditor);
      end;
    end;
  end;
end;

procedure TfrmChild.BrightContrastLayerLogoThumbDblClick(ASender: TObject);
var
  LOldBright   : Integer;
  LOldContrast : Integer;
  LBCLayer     : TgmBrightContrastLayer;
begin
  if ASender is TgmBrightContrastLayer then
  begin
    LBCLayer := TgmBrightContrastLayer(ASender);

    LOldBright   := LBCLayer.BrightAmount;
    LOldContrast := LBCLayer.ContrastAmount;

    frmBrightnessContrast := TfrmBrightnessContrast.Create(Application);
    try
      frmBrightnessContrast.AssociateToBrightContrastLayer(LBCLayer);

      case frmBrightnessContrast.ShowModal of
        mrOK:
          begin
            if not frmBrightnessContrast.chckbxPreview.Checked then
            begin
              LBCLayer.Changed;
            end;
          end;

        mrCancel:
          begin
            LBCLayer.BrightAmount   := LOldBright;
            LBCLayer.ContrastAmount := LOldContrast;
            LBCLayer.Changed;
          end;
      end; 

    finally
      FreeAndNil(frmBrightnessContrast);
    end;  
  end;
end;

// check to see whether or not the test point can be added to a polygon array
function TfrmChild.CanAddPointToPolygon(const APolygon: array of TPoint;
  const ATextPoint: TPoint; const AVertexDistance: Double): Boolean;
var
  LIndex           : Integer;
  LPoint1, LPoint2 : TPoint;
  LDistance        : Double;
begin
  Result := False;

  if Length(APolygon) >= 2 then
  begin
    LDistance := Abs(AVertexDistance);
    LIndex    := High(APolygon);
    LPoint1   := APolygon[LIndex - 1];
    LPoint2   := APolygon[LIndex];

    Result := ( CalcLengthByTwoPoints(ATextPoint, LPoint1) > LDistance ) and
              ( CalcLengthByTwoPoints(ATextPoint, LPoint2) > LDistance );
  end;
end;

procedure TfrmChild.ChannelMixerLayerLogoThumbDblClick(ASender: TObject);
var
  LOldChannelMixerSettings : TgmChannelMixer;
  LChannelMixerLayer       : TgmChannelMixerLayer;
begin
  if ASender is TgmChannelMixerLayer then
  begin
    LChannelMixerLayer := TgmChannelMixerLayer(ASender);

    LOldChannelMixerSettings := TgmChannelMixer.Create;
    try
      LOldChannelMixerSettings.AssignData(LChannelMixerLayer.ChannelMixer);

      frmChannelMixer := TfrmChannelMixer.Create(Application);
      try
        frmChannelMixer.AssociateToChannelMixerLayer(LChannelMixerLayer);

        case frmChannelMixer.ShowModal of
          mrOK:
            begin
              if not frmChannelMixer.chckbxPreview.Checked then
              begin
                LChannelMixerLayer.Changed;
              end;
            end;

          mrCancel:
            begin
              LChannelMixerLayer.ChannelMixer.AssignData(LOldChannelMixerSettings);
              LChannelMixerLayer.Changed;
            end;
        end;

      finally
        FreeAndNil(frmChannelMixer);
      end;  

    finally
      LOldChannelMixerSettings.Free;
    end;
  end;
end;

procedure TfrmChild.ColorBalanceLayerLogoThumbDblClick(ASender: TObject);
var
  LOldCyanRed            : Integer;
  LOldMagentaGreen       : Integer;
  LOldYellowBlue         : Integer;
  LOldPreserveLuminosity : Boolean;
  LOldTransferMode       : TgmTransferMode;
  LCBLayer               : TgmColorBalanceLayer;
begin
  if ASender is TgmColorBalanceLayer then
  begin
    LCBLayer := TgmColorBalanceLayer(ASender);
    
    with LCBLayer do
    begin
      LOldTransferMode       := ColorBalance.TransferMode;
      LOldPreserveLuminosity := ColorBalance.PreserveLuminosity;
      LOldCyanRed            := ColorBalance.CyanRed;
      LOldMagentaGreen       := ColorBalance.MagentaGreen;
      LOldYellowBlue         := ColorBalance.YellowBlue;
    end;

    frmColorBalance := TfrmColorBalance.Create(Application);
    try
      frmColorBalance.AssociateToColorBalanceLayer(LCBLayer);

      case frmColorBalance.ShowModal of
        mrOK:
          begin
            if not frmColorBalance.chckbxPreview.Checked then
            begin
              LCBLayer.Changed;
            end;
          end;

        mrCancel:
          begin
            with LCBLayer do
            begin
              ColorBalance.TransferMode       := LOldTransferMode;
              ColorBalance.PreserveLuminosity := LOldPreserveLuminosity;
              ColorBalance.CyanRed            := LOldCyanRed;
              ColorBalance.MagentaGreen       := LOldMagentaGreen;
              ColorBalance.YellowBlue         := LOldYellowBlue;
            end;

            LCBLayer.Changed;
          end;
      end;

    finally
      FreeAndNil(frmColorBalance);
    end;
  end;
end;

// for Undo/Redo
procedure TfrmChild.CreateNewLayerCommand(ANewLayer: TgmCustomLayer;
  const ANewLayerIndex: Integer);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := TgmNewLayerCommand.Create(FLayerList, ANewLayer, ANewLayerIndex);
  FCommandManager.AddCommand(LCommand);
end;

// for Undo/Redo
procedure TfrmChild.CreateUndoRedoCommandForPixelProcessing(
  const ACommandName: string; const ACommandIconResName: string = '');
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;

  if Assigned(FSelection) then
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
            ACommandName,
            FChannelManager,
            FChannelManager.AlphaChannelList.SelectedIndex,
            frmMain.FBitmapBefore,
            FSelection.CutOriginal,
            GetSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
            ACommandName,
            FChannelManager,
            frmMain.FBitmapBefore,
            FSelection.CutOriginal,
            GetSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
            ACommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            FSelection.CutOriginal,
            GetSelectionForUndoRedo);
          end;

      ctColorChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
            ACommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            FSelection.CutOriginal,
            GetSelectionForUndoRedo);
        end;
    end;
  end
  else
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmAlphaChannelProcessCommand.Create(
            ACommandName,
            frmMain.FBitmapBefore,
            FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
            FChannelManager.AlphaChannelList,
            FChannelManager.AlphaChannelList.SelectedIndex);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmQuickMaskChannelProcessCommand.Create(
            ACommandName,
            frmMain.FBitmapBefore,
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
            FChannelManager);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmLayerMaskProcessCommand.Create(
            ACommandName,
            frmMain.FBitmapBefore,
            FLayerList.SelectedLayer.MaskBitmap,
            FLayerList,
            FLayerList.SelectedIndex);
        end;

      ctColorChannel:
        begin
          LCommand := TgmLayerImageProcessCommand.Create(
            ACommandName,
            frmMain.FBitmapBefore,
            FLayerList.SelectedLayer.LayerBitmap,
            FLayerList,
            FLayerList.SelectedIndex);
        end;
    end;
  end;

  if Assigned(LCommand) then
  begin
    if ACommandIconResName <> '' then
    begin
      LCommand.ChangeCommandIconByResourceName(ACommandIconResName);
    end;

    FCommandManager.AddCommand(LCommand);
  end;
end;

procedure TfrmChild.CurvesLayerLogoThumbDblClick(ASender: TObject);
var
  LOldCurvesSettings : TgmCurvesTool;
  LCurvesLayer       : TgmCurvesLayer;
begin
  if ASender is TgmCurvesLayer then
  begin
    LCurvesLayer := TgmCurvesLayer(ASender);

    LOldCurvesSettings := TgmCurvesTool.Create(LCurvesLayer.LayerBitmap);
    try
      LOldCurvesSettings.Assign(LCurvesLayer.CurvesTool);

      frmCurves := TfrmCurves.Create(Application);
      try
        frmCurves.AssociateToCurvesLayer(LCurvesLayer);

        case frmCurves.ShowModal of
          mrOK:
            begin
              if not frmCurves.chckbxPreview.Checked then
              begin
                LCurvesLayer.Changed;
              end;
            end;

          mrCancel:
            begin
              LCurvesLayer.CurvesTool.Assign(LOldCurvesSettings);
              LCurvesLayer.Changed;
            end;
        end;

      finally
        FreeAndNil(frmCurves);
      end;  

    finally
      LOldCurvesSettings.Free;
    end;
  end;
end;

// For Undo/Redo
procedure TfrmChild.DeleteClipboardSelectionForUndoRedo;
begin
  if Assigned(frmMain.FSelectionClipboard) then
  begin
    FreeAndNil(frmMain.FSelectionClipboard);
  end;
end;

// For Undo/Redo
procedure TfrmChild.DeleteSelectionForUndoRedo;
begin
  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;
    FreeAndNil(FSelection);
  end;
end;

// for Undo/Redo
function TfrmChild.EnterSelectionTransformModeForUndoRedo(
  const ATransformMode: TgmTransformMode;
  const ACreateIfNone: Boolean): TgmSelectionTransformation;
begin
  Result := nil;

  if ACreateIfNone then
  begin
    if (ATransformMode = tmNone) or
       (ATransformMode = tmTranslate) or
       (not Assigned(FSelection)) then
    begin
      Exit;
    end;

    if Assigned(FSelectionTransformation) then
    begin
      FreeAndNil(FSelectionTransformation);
    end;

    case ATransformMode of
      tmDistort:
        begin
          FSelectionTransformation := TgmSelectionDistort.Create(FSelection);
        end;

      tmRotate:
        begin
          FSelectionTransformation := TgmSelectionRotate.Create(FSelection);
        end;

      tmScale:
        begin
          FSelectionTransformation := TgmSelectionScale.Create(FSelection);
        end;
    end;

    if Assigned(FSelectionTransformation) then
    begin
      ConnectTransformMouseEvents();
      Result := FSelectionTransformation;
    end;
  end
  else
  begin
    Result := FSelectionTransformation;
  end;
end;

// for Undo/Redo
procedure TfrmChild.ExitSelectionTransfromModeForUndoRedo;
begin
  FreeSelectionTransformation();
  ConnectMouseEventsToImage();
end;

// for Undo/Redo
function TfrmChild.GetClipboardSelectionForUndoRedo(
  const ACreateIfNone: Boolean): TgmSelection;
begin
  if ACreateIfNone then
  begin
    if not Assigned(frmMain.FSelectionClipboard) then
    begin
      frmMain.FSelectionClipboard := TgmSelection.Create(imgWorkArea);
    end;
  end;

  Result := frmMain.FSelectionClipboard;
end;

// for Undo/Redo
function TfrmChild.GetCommandIconResourceNameForMarqueeTools(
  const AMarqueeTool: TgmMarqueeTools): string;
begin
  Result := '';

  case AMarqueeTool of
    mtMoveResize:
      begin
        // TODO
      end;

    mtSingleRow:
      begin
        Result := gmSelectionCommands.SINGLE_ROW_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtSingleColumn:
      begin
        Result := gmSelectionCommands.SINGLE_COLUMN_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtRectangular:
      begin
        Result := gmSelectionCommands.RECT_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtRoundRectangular:
      begin
        Result := gmSelectionCommands.ROUND_RECT_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtElliptical:
      begin
        Result := gmSelectionCommands.ELLIPTICAL_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtPolygonal:
      begin
        Result := gmSelectionCommands.POLYGONAL_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtRegularPolygon:
      begin
        Result := gmSelectionCommands.REGULAR_POLY_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtLasso:
      begin
        Result := gmSelectionCommands.LASSO_MARQUEE_COMMAND_ICON_RES_NAME;
      end;

    mtMagicWand:
      begin
        Result := gmSelectionCommands.MAGIC_WAND_MARQUEE_ICON_RES_NAME;
      end;

    mtMagneticLasso:
      begin
        Result := gmSelectionCommands.MAGNETIC_LASSO_MARQUEE_ICON_RES_NAME;
      end;
  end;
end;

// for Undo/Redo
function TfrmChild.GetCommandNameForMarqueeTools(
  const AMarqueeTool: TgmMarqueeTools): string;
begin
  Result := '';

  case AMarqueeTool of
    mtMoveResize:
      begin
        // TODO
      end;

    mtSingleRow:
      begin
        Result := 'Single Row Marquee';
      end;

    mtSingleColumn:
      begin
        Result := 'Single Column Marquee';
      end;

    mtRectangular:
      begin
        Result := 'Rectangular Marquee';
      end;

    mtRoundRectangular:
      begin
        Result := 'Rounded-Corner Rectangular Marquee';
      end;

    mtElliptical:
      begin
        Result := 'Elliptical Marquee';
      end;

    mtPolygonal:
      begin
        Result := 'Polygonal Lasso';
      end;

    mtRegularPolygon:
      begin
        Result := 'Regular Polygonal Lasso';
      end;

    mtLasso:
      begin
        Result := 'Lasso';
      end;

    mtMagicWand:
      begin
        Result := 'Magic Wand';
      end;

    mtMagneticLasso:
      begin
        Result := 'Magnetic Lasso';
      end;
  end;
end;

// for Undo/Redo
function TfrmChild.GetSelectionForUndoRedo(
  const ACreateIfNone: Boolean): TgmSelection;
begin
  if ACreateIfNone then
  begin
    if not Assigned(FSelection) then
    begin
      FSelection := TgmSelection.Create(imgWorkArea);
    end;
  end;

  Result := FSelection;
end;

// for Undo/Redo
function TfrmChild.GetSelectionTransformationForUndoRedo: TgmSelectionTransformation;
begin
  Result := FSelectionTransformation;
end;

procedure TfrmChild.GradientFillLayerLogoThumbDblClick(ASender: TObject);
var
  LOldGradient       : TgmGradientItem;
  LOldSettings       : TgmGradientFillSettings;
  LGradientFillLayer : TgmGradientFillLayer;
begin
  if ASender is TgmGradientFillLayer then
  begin
    LGradientFillLayer := TgmGradientFillLayer(ASender);

    LOldSettings.Style          := LGradientFillLayer.Style;
    LOldSettings.Angle          := LGradientFillLayer.Angle;
    LOldSettings.Scale          := LGradientFillLayer.Scale;
    LOldSettings.TranslateX     := LGradientFillLayer.TranslateX;
    LOldSettings.TranslateY     := LGradientFillLayer.TranslateY;
    LOldSettings.Reversed       := LGradientFillLayer.IsReversed;
    LOldSettings.StartPoint     := LGradientFillLayer.StartPoint;
    LOldSettings.EndPoint       := LGradientFillLayer.EndPoint;
    LOldSettings.CenterPoint    := LGradientFillLayer.CenterPoint;
    LOldSettings.OriginalCenter := LGradientFillLayer.OriginalCenter;
    LOldSettings.OriginalStart  := LGradientFillLayer.OriginalStart;
    LOldSettings.OriginalEnd    := LGradientFillLayer.OriginalEnd;

    LOldGradient := TgmGradientItem.Create(nil);
    try
      LOldGradient.Assign(LGradientFillLayer.Gradient);

      // the Gradient Fill dialog is created automatically,
      // so we don't need to create it at here
      case frmGradientFill.ShowModal of
        mrOK:
          begin
            LGradientFillLayer.UpdateLogoThumbnail;
          end;

        mrCancel:
          begin
            LGradientFillLayer.Gradient.Assign(LOldGradient);
            LGradientFillLayer.Setup(LOldSettings);
            LGradientFillLayer.DrawGradientOnLayer;
            LGradientFillLayer.Changed;
          end;
      end;

    finally
      LOldGradient.Free;
    end;
  end;
end;

procedure TfrmChild.GradientMapLayerLogoThumbDblClick(ASender: TObject);
var
  LOldGradient      : TgmGradientItem;
  LOldReversed      : Boolean;
  LGradientMapLayer : TgmGradientMapLayer;
begin
  if ASender is TgmGradientMapLayer then
  begin
    LGradientMapLayer := TgmGradientMapLayer(ASender);

    LOldReversed := LGradientMapLayer.IsReversed;
    LOldGradient := TgmGradientItem.Create(nil);
    try
      LOldGradient.Assign(LGradientMapLayer.Gradient);

      // the Gradient Map dialog is created automatically,
      // so we don't need to create it at here
      case frmGradientMap.ShowModal of
        mrOK:
          begin
            if not frmGradientMap.chckbxPreview.Checked then
            begin
              LGradientMapLayer.Changed;
            end;
          end;

        mrCancel:
          begin
            LGradientMapLayer.Gradient.Assign(LOldGradient);
            LGradientMapLayer.IsReversed := LOldReversed;
            LGradientMapLayer.Changed;
          end;
      end;

    finally
      LOldGradient.Free;
    end;
  end;
end;

procedure TfrmChild.HueSaturationLayerLogoThumbDblClick(ASender: TObject);
var
  LOldMode : TgmHueSaturationAdjustMode;
  LOldH    : Integer;
  LOldS    : Integer;
  LOldLV   : Integer;
  LHSLayer : TgmHueSaturationLayer;
begin
  if ASender is TgmHueSaturationLayer then
  begin
    LHSLayer := TgmHueSaturationLayer(ASender);
    LOldMode := LHSLayer.AdjustMode;
    LOldH    := LHSLayer.Hue;
    LOldS    := LHSLayer.Saturation;
    LOldLV   := LHSLayer.LightValue;

    frmHueSaturation := TfrmHueSaturation.Create(Application);
    try
      frmHueSaturation.AssociateToHueSaturationLayer(LHSLayer);

      case frmHueSaturation.ShowModal of
        mrOK:
          begin
            if not frmHueSaturation.chckbxPreview.Checked then
            begin
              LHSLayer.Changed;
            end;
          end;

        mrCancel:
          begin
            LHSLayer.AdjustMode := LOldMode;
            LHSLayer.Hue        := LOldH;
            LHSLayer.Saturation := LOldS;
            LHSLayer.LightValue := LOldLV;

            LHSLayer.Changed;
          end;
      end;

    finally
      FreeAndNil(frmHueSaturation);
    end;
  end;
end;

procedure TfrmChild.LevelsLayerLogoThumbDblClick(ASender: TObject);
var
  LOldHistogramScale : TgmGimpHistogramScale;
  LOldLevelsSettings : TgmLevelsTool;
  LLevelsLayer       : TgmLevelsLayer;
begin
  if ASender is TgmLevelsLayer then
  begin
    LLevelsLayer := TgmLevelsLayer(ASender);

    LOldLevelsSettings := TgmLevelsTool.Create(LLevelsLayer.LayerBitmap);
    try
      LOldLevelsSettings.Assign(LLevelsLayer.LevelsTool);
      LOldHistogramScale := LLevelsLayer.HistogramScale;

      frmLevelsTool := TfrmLevelsTool.Create(Application);
      try
        frmLevelsTool.AssociateToLevelsLayer(LLevelsLayer);

        case frmLevelsTool.ShowModal of
          mrOK:
            begin
              if not frmLevelsTool.chckbxPreview.Checked then
              begin
                LLevelsLayer.Changed;
              end;
            end;

          mrCancel:
            begin
              LLevelsLayer.HistogramScale := LOldHistogramScale;
              
              LLevelsLayer.LevelsTool.Assign(LOldLevelsSettings);
              LLevelsLayer.Changed;
            end;
        end;

      finally
        FreeAndNil(frmLevelsTool);
      end;  

    finally
      LOldLevelsSettings.Free;
    end;
  end;
end;

procedure TfrmChild.PatternLayerLogoThumbDblClick(ASender: TObject);
var
  LOldPatternBmp : TBitmap32;
  LOldScale      : Double;
  LPatternLayer  : TgmPatternLayer;
begin
  LOldPatternBmp := nil;
  
  if ASender is TgmPatternLayer then
  begin
    LPatternLayer := TgmPatternLayer(ASender);
    LOldScale     := LPatternLayer.Scale;

    if Assigned(LPatternLayer.PatternBitmap) then
    begin
      LOldPatternBmp := TBitmap32.Create;
      LOldPatternBmp.Assign(LPatternLayer.PatternBitmap);
    end;

    // the Pattern Fill dialog is created automatically,
    // so we don't need to create it at here
    case frmPatternFill.ShowModal of
      mrOK:
        begin
          LPatternLayer.UpdateLogoThumbnail;
        end;

      mrCancel:
        begin
          LPatternLayer.Setup(LOldPatternBmp, LOldScale);
          LPatternLayer.FillPatternOnLayer;
          LPatternLayer.Changed;
        end;
    end;

    if Assigned(LOldPatternBmp) then
    begin
      LOldPatternBmp.Free;
    end;
  end;
end;

procedure TfrmChild.PosterizeLayerLogoThumbDblClick(ASender: TObject);
var
  LOldLevel       : Byte;
  LPosterizeLayer : TgmPosterizeLayer;
begin
  if ASender is TgmPosterizeLayer then
  begin
    LPosterizeLayer := TgmPosterizeLayer(ASender);
    LOldLevel       := LPosterizeLayer.Level;

    frmPosterize := TfrmPosterize.Create(Application);
    try
      frmPosterize.AssociateToPosterizeLayer(LPosterizeLayer);

      case frmPosterize.ShowModal of
        mrOK:
          begin
            if not frmPosterize.chckbxPreview.Checked then
            begin
              LPosterizeLayer.Changed;
            end;
          end;

        mrCancel:
          begin
            LPosterizeLayer.Level := LOldLevel;
            LPosterizeLayer.Changed;
          end;
      end;

    finally
      FreeAndNil(frmPosterize);
    end;
  end;
end;

procedure TfrmChild.RichTextLayerLogoThumbDblClick(ASender: TObject);
var
  LRichTextLayer : TgmRichTextLayer;
begin
  if ASender is TgmRichTextLayer then
  begin
    // change the main tool to Text tool, if necessary
    if frmMain.MainTool <> gmtTextTool then
    begin
      frmMain.spdbtnTextTool.Down := True;
      frmMain.ChangeMainToolClick(frmMain.spdbtnTextTool);
    end;

    LRichTextLayer := TgmRichTextLayer(ASender);

    // change the text layer state to Modify state
    if LRichTextLayer.TextLayerState <> tlsModify then
    begin
      LRichTextLayer.TextLayerState := tlsModify;
    end;

    LRichTextLayer.UpdateContentToAssociatedTextEditor();
    LRichTextLayer.IsTextChanged := False;

    frmRichTextEditor.rchedtRichTextEditor.Update();
    frmRichTextEditor.Show();

    frmMain.UpdateTextOptions();

    // for Undo/Redo
    if Assigned(FOldTextStream) then
    begin
      FreeAndNil(FOldTextStream);
    end;
    
    if LRichTextLayer.RichTextStream.Size > 0 then
    begin
      FOldTextStream := TMemoryStream.Create();
      
      LRichTextLayer.RichTextStream.Position := 0;
      FOldTextStream.LoadFromStream(LRichTextLayer.RichTextStream);

      LRichTextLayer.RichTextStream.Position := 0;
      FOldTextStream.Position                := 0;
    end;
  end;
end;

// for Undo/Redo
procedure TfrmChild.SaveChannelPixelsBeforePixelProcessing;
begin
  if Assigned(FSelection) then
  begin
    frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);
  end
  else
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            frmMain.FBitmapBefore.Assign(
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
          end;
        end;

      ctQuickMaskChannel:
        begin
          if Assigned(FChannelManager.QuickMaskChannel) then
          begin
            frmMain.FBitmapBefore.Assign(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;
        end;

      ctLayerMaskChannel:
        begin
          frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);
        end;

      ctColorChannel:
        begin
          frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);
        end;
    end;
  end;
end;

procedure TfrmChild.ShapeRegionLayerLogoThumbDblClick(ASender: TObject);
var
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldColor         : TColor;
  LCommand          : TgmCustomCommand;
begin
  if ASender is TgmShapeRegionLayer then
  begin
    LShapeRegionLayer      := TgmShapeRegionLayer(ASender);
    dmMain.clrdlgRGB.Color := LShapeRegionLayer.RegionColor;
    LOldColor              := LShapeRegionLayer.RegionColor; // for Undo/Redo

    if dmMain.clrdlgRGB.Execute then
    begin
      LShapeRegionLayer.RegionColor := dmMain.clrdlgRGB.Color;

      with LShapeRegionLayer do
      begin
        DrawRegionOnLayer();
        Changed();
        UpdateLogoThumbnail();
      end;

      // Undo/Redo
      LCommand := TgmModifyShapeRegionFillingColorCommand.Create(FLayerList,
        FLayerList.SelectedIndex, LOldColor, dmMain.clrdlgRGB.Color);

      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.ShapeRegionLayerPanelDblClick(ASender: TObject);
var
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  if ASender is TgmShapeRegionLayer then
  begin
    LShapeRegionLayer := TgmShapeRegionLayer(ASender);

    LShapeRegionLayer.IsDismissed := False;
    imgWorkArea.Changed();
  end;
end;

procedure TfrmChild.SolidColorLayerLogoThumbDblClick(ASender: TObject);
var
  LSolidColorLayer : TgmSolidColorLayer;
begin
  if ASender is TgmSolidColorLayer then
  begin
    LSolidColorLayer       := TgmSolidColorLayer(ASender);
    dmMain.clrdlgRGB.Color := WinColor(LSolidColorLayer.SolidColor);

    if dmMain.clrdlgRGB.Execute then
    begin
      LSolidColorLayer.SolidColor := Color32(dmMain.clrdlgRGB.Color);

      LSolidColorLayer.Changed();
      LSolidCOlorLayer.UpdateLogoThumbnail();
    end;
  end;
end;

procedure TfrmChild.ThresholdLayerLogoThumbDblClick(ASender: TObject);
var
  LOldLevel       : Byte;
  LThresholdLayer : TgmThresholdLayer;
begin
  if ASender is TgmThresholdLayer then
  begin
    LThresholdLayer := TgmThresholdLayer(ASender);
    LOldLevel       := LThresholdLayer.Level;

    frmThreshold := TfrmThreshold.Create(Application);
    try
      frmThreshold.AssociateToThresholdLayer(LThresholdLayer);

      case frmThreshold.ShowModal of
        mrOK:
          begin
            if not frmThreshold.chckbxPreview.Checked then
            begin
              LThresholdLayer.Changed();
            end;
          end;

        mrCancel:
          begin
            LThresholdLayer.Level := LOldLevel;
            LThresholdLayer.Changed();
          end;
      end;

    finally
      FreeAndNil(frmThreshold);
    end;
  end;
end;

procedure TfrmChild.OnLayerPanelDisabled(ASender: TObject);
begin
  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate();
  end;
end;

procedure TfrmChild.OnLayerPanelEnabled(ASender: TObject);
begin
  case FLayerList.SelectedLayer.LayerProcessStage of
    lpsLayer:
      begin
        FChannelManager.SelectColorChannel(0, False);
      end;

    lpsMask:
      begin
        FChannelManager.SelectLayerMaskChannel();
      end;
  end;

  if Assigned(frmLayers) then
  begin
    frmLayers.LayerPanelManager.Invalidate();
  end;
end;

procedure TfrmChild.OnAlphaChannelDelete(ASender: TObject);
begin
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TfrmChild.OnAlphaChannelOrderChanged(AList: TgmAlphaChannelList;
  const AOldIndex, ANewIndex: Integer);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo
  LCommand := TgmRearrangeChannelsCommand.Create(FChannelManager,
                                                 AOldIndex, ANewIndex);

  FCommandManager.AddCommand(LCommand);
end;

procedure TfrmChild.OnChannelDblClick(AChannel: TgmCustomChannel;
  const AChannelType: TgmChannelType);
var
  LChannelPanel   : TgmAlphaChannel;
  LChannelOptions : TgmChannelOptionsCommandData;
  LCommand        : TgmCustomCommand;
begin
  LCommand := nil;

  if Assigned(AChannel) then
  begin
    case AChannelType of
      ctAlphaChannel,
      ctLayerMaskChannel,
      ctQuickMaskChannel:
        begin
          LChannelPanel := TgmAlphaChannel(AChannel);

          // Undo/Redo
          if AChannelType = ctAlphaChannel then
          begin
            LChannelOptions.ChannelIndex   := FChannelManager.AlphaChannelList.GetAlphlaChannelIndex(LChannelPanel);
            LChannelOptions.OldChannelName := LChannelPanel.ChannelName;
          end;

          LChannelOptions.OldMaskColor      := LChannelPanel.MaskColor;
          LChannelOptions.OldOpacityPercent := LChannelPanel.MaskOpacity / 255.0;
          LChannelOptions.OldColorIndicator := LChannelPanel.MaskColorIndicator;

          frmChannelOptions := TfrmChannelOptions.Create(Application);
          try
            frmChannelOptions.FormSetup(LChannelPanel, AChannelType);

            if frmChannelOptions.ShowModal = mrOK then
            begin
              LChannelPanel.ChannelName        := frmChannelOptions.edtChannelName.Text;
              LChannelPanel.MaskColor          := Color32(frmChannelOptions.shpMaskColor.Brush.Color);
              LChannelPanel.MaskColorIndicator := TgmMaskColorIndicator(frmChannelOptions.rdgrpColorIndicator.ItemIndex);
              LChannelPanel.MaskOpacity        := Round( StrToInt(frmChannelOptions.edtMaskOpacity.Text) * 255 / 100 );

              LChannelPanel.ChannelLayer.Changed();

              FChannelManager.DefaultMaskColor := LChannelPanel.MaskColor;

              // Undo/Redo
              LChannelOptions.NewChannelName    := LChannelPanel.ChannelName;
              LChannelOptions.NewMaskColor      := LChannelPanel.MaskColor;
              LChannelOptions.NewOpacityPercent := LChannelPanel.MaskOpacity / 255.0;
              LChannelOptions.NewColorIndicator := LChannelPanel.MaskColorIndicator;

              // Undo/Redo
              if AChannelType = ctAlphaChannel then
              begin
                LCommand := TgmAlphaChannelOptionsCommand.Create(
                  FChannelManager, LChannelOptions);
              end
              else if AChannelType = ctQuickMaskChannel then
              begin
                LCommand := TgmQuickMaskChannelOptionsCommand.Create(
                  FChannelManager, LChannelOptions);
              end
              else if AChannelType = ctLayerMaskChannel then
              begin
                LCommand := TgmLayerMaskChannelOptionsCommand.Create(
                  FChannelManager, FLayerList, FLayerList.SelectedIndex,
                  LChannelOptions);
              end;

              if Assigned(LCommand) then
              begin
                FCommandManager.AddCommand(LCommand);
              end;
            end;
          finally
            FreeAndNil(frmChannelOptions);
          end;
        end;
    end;
  end;  
end;

procedure TfrmChild.OnChannelThumbnailUpdate(ASender: TObject);
begin
  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate();
  end;
end;

procedure TfrmChild.OnQuickMaskChannelCreate(ASender: TObject);
begin
  if Assigned(FChannelManager.QuickMaskChannel) then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;
end;

procedure TfrmChild.OnQuickMaskChannelDelete(ASender: TObject);
begin
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TfrmChild.OnSelectedChannelChanged(
  const ACurrentChannelType: TgmChannelType);
begin
  case ACurrentChannelType of
    ctColorChannel:
      begin
        if Assigned(FLayerList) then
        begin
          // rendering the checkerboard pattern as background first
          imgWorkArea.Bitmap.Draw(imgWorkArea.Bitmap.BoundsRect,
                                  imgWorkArea.Bitmap.BoundsRect,
                                  FCheckerboardBmp);

          // here, we don't draw layer combined result onto the background,
          // instead, we tweak the combined result with color channel settings,
          // and then merge the new result with the background ...
          FChannelManager.BlendByColorChannelSettings(
            FLayerList.CombineResult, imgWorkArea.Bitmap,
            imgWorkArea.Bitmap.BoundsRect);

          imgWorkArea.Bitmap.Changed(imgWorkArea.Bitmap.BoundsRect);

          FLayerList.SelectedLayer.LayerProcessStage := lpsLayer;
          FLayerList.SelectedLayer.IsLayerEnabled    := True;
        end;
      end;

    ctLayerMaskChannel:
      begin
        FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
        FLayerList.SelectedLayer.IsLayerEnabled    := True;
      end;

    ctAlphaChannel,
    ctQuickMaskChannel:
      begin
        FLayerList.SelectedLayer.IsLayerEnabled := False;
      end;
  end;

  if Assigned(frmChannels) then
  begin
    // If we need to scroll the selected channel panel in to the
    // viewport of the channel viewer, the functioin
    // ScrollSelectedChannelPanelInViewport() will help us refresh the
    // viewer. If no need to scroll, we have to refresh the viewer
    // by ourselves.
    if not frmChannels.ChannelViewer.ScrollSelectedChannelPanelInViewport then
    begin
      frmChannels.ChannelViewer.Invalidate;
    end;
  end;

  if Assigned(FSelection) then
  begin
    ChangeSelectionTarget();
  end;

  // change mode of color form
  if ACurrentChannelType = ctColorChannel then
  begin
    frmColor.ColorMode := cmRGB;
  end
  else
  begin
    frmColor.ColorMode := cmGrayscale;
  end;
end;

procedure TfrmChild.OnChannelVisibleChange(const AChannelType: TgmChannelType);
begin
  if Assigned(FLayerList) then
  begin
    // rendering the checkerboard pattern as background first
    imgWorkArea.Bitmap.Draw(imgWorkArea.Bitmap.BoundsRect,
                            FCheckerboardBmp.BoundsRect,
                            FCheckerboardBmp);

    // here, we don't draw layer combined result onto the background,
    // instead, we tweak the combined result with color channel settings,
    // and then merge the new result with the background ...
    FChannelManager.BlendByColorChannelSettings(
      FLayerList.CombineResult, imgWorkArea.Bitmap,
      imgWorkArea.Bitmap.BoundsRect);

    imgWorkArea.Bitmap.Changed(imgWorkArea.Bitmap.BoundsRect);
  end;

  if Assigned(frmChannels) then
  begin
    frmChannels.ChannelViewer.Invalidate;
  end;
end;

procedure TfrmChild.OnInsertAlphaChannel(AList: TgmAlphaChannelList;
  const AIndex: Integer);
begin
  FChannelManager.SelectAlphaChannel(AIndex, False);
end;

procedure TfrmChild.OnLayerMaskChannelDelete(ASender: TObject);
begin
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TfrmChild.AfterPathDeleted(ASender: TObject);
begin
  // TODO: perhaps adding Undo/Redo command at here
end;

procedure TfrmChild.AfterPathInserted(ASender: TObject; const AIndex: Integer);
begin
  frmPaths.PathManager.ScrollSelectedPanelInViewport();
end;

procedure TfrmChild.AfterSelectedPathChanged(ASender: TObject);
begin
  if Assigned(FPathList.SelectedPath) then
  begin
    if FPathLayer = nil then
    begin
      CreatePathLayer();
    end;

    FPathLayer.BringToFront();

    // update the path layer
    FPathLayer.Bitmap.Clear($00000000);
    
    // draw the paths on the path layer
    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
      FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
  end
  else
  begin
    DeletePathLayer();
  end;

  frmPaths.PathManager.Invalidate();
end;

procedure TfrmChild.AfterSelectedSnapshotChanged(ASnapshotList: TgmSnapshotList;
  ASelectedSnapshot: TgmSnapshot; const AOldIndex, ACurrIndex: Integer);
var
  LSizeChanged : Boolean;
  LRect        : TRect;
begin
  LSizeChanged := False;

  if AOldIndex = ACurrIndex then
  begin
    Exit;
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;
  end;
  
  if FCommandManager.CommandList.SelectedIndex >= 0 then
  begin
    FCommandManager.UndoCommands(FCommandManager.CommandList.SelectedIndex, 0);
    FCommandManager.CommandList.DeselectAllCommands();
  end;
  
  frmHistory.CommandViewer.RefreshViewer();

  // there were crop operations in the Undo/Redo list
  if (FLayerList.SelectedLayer.LayerBitmap.Width <> imgWorkArea.Bitmap.Width) or
     (FLayerList.SelectedLayer.LayerBitmap.Height <> imgWorkArea.Bitmap.Height) then
  begin
    // set background size before create background layer
    imgWorkArea.Bitmap.SetSize(FLayerList.SelectedLayer.LayerBitmap.Width,
                               FLayerList.SelectedLayer.LayerBitmap.Height);

    imgWorkArea.Bitmap.Clear($00000000);

    // draw checkerboard pattern
    CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
    DrawCheckerboardPattern(CheckerboardBmp, CheckerboardBmp.ClipRect);

    // set location for channel layers ...
    LRect             := imgWorkArea.GetBitmapRect();
    LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
    LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

    FChannelManager.ChannelLayerLocation := FloatRect(LRect);

    LSizeChanged := True;
  end;

  if FLayerList.SelectedLayer.IsMaskEnabled then
  begin
    if not Assigned(FChannelManager.LayerMaskChannel) then
    begin
      FChannelManager.CreateLayerMaskChannel(
        FLayerList.SelectedLayer.MaskBitmap,
        FLayerList.SelectedLayer.LayerName + ' Mask');
    end
    else
    begin
      with FChannelManager.LayerMaskChannel do
      begin
        ChannelLayer.Bitmap.BeginUpdate();
        try
          // Don't directly using Assign() at here.
          // Must drawing on channel layer.
          ChannelLayer.Bitmap.SetSizeFrom(FLayerList.SelectedLayer.MaskBitmap);
          ChannelLayer.Bitmap.Draw(0, 0, FLayerList.SelectedLayer.MaskBitmap);
          UpdateChannelThumbnail();
        finally
          ChannelLayer.Bitmap.EndUpdate();
        end;
      end;
    end;
  end
  else
  begin
    if Assigned(FChannelManager.LayerMaskChannel) then
    begin
      FChannelManager.DeleteLayerMaskChannel();
    end;
  end;

  // set callback functions for the layers in the layer list
  SetCallbacksForLayersInList();
  AssociateTextEditorToRichTextLayers();
  
  // deselect all figures on vector layers
  FFigureManager.DeselectAllFigures();

  // dealing with paths
  if Assigned(FPathList.SelectedPath) then
  begin
    FCurvePath := FPathList.SelectedPath.CurvePathList.SelectedPath;

    if not Assigned(FPathLayer) then
    begin
      CreatePathLayer();
    end
    else
    begin
      // there were crop operations in the Undo/Redo list
      if LSizeChanged then
      begin
        FitPathLayerToViewport();
      end;
    end;

    FPathLayer.Bitmap.Clear($00000000);

    FPathList.SelectedPath.CurvePathList.DrawAllPaths(FPathLayer.Bitmap.Canvas,
      pmNotXor, imgWorkArea.BitmapToControl);

    if Assigned(FCurvePath) then
    begin
      if not FCurvePath.CurveSegmentsList.IsClosed then
      begin
        FPathList.SelectedPath.CurvePathList.Status := cplsAddNextAnchorPoint;
      end;
    end;
  end
  else
  begin
    FCurvePath := nil;
    DeletePathLayer();
  end;

  FLayerList.SelectedLayer.Changed();

  // there were crop operations in the Undo/Redo list
  if LSizeChanged then
  begin
    // channels ...
    LRect             := imgWorkArea.GetBitmapRect();
    LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
    LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

    FChannelManager.UpdateColorChannelThumbnails(FLayerList.CombineResult);
    FChannelManager.ChannelLayerLocation := FloatRect(LRect);
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := True;
  end;

  frmLayers.LayerPanelManager.Invalidate();
  frmChannels.ChannelViewer.Invalidate();
  frmPaths.PathManager.Invalidate();

  // refresh the viewer for erasing unwanted things, such as a border of
  // a selection
  imgWorkArea.Update(imgWorkArea.ClientRect);
end;

procedure TfrmChild.ImageViewerScaleChange(ASender: TObject);
begin
  // Update the background checkerboard pattern first.
  // Note that, we divide the size of checkerboard pattern by the
  // scale of the image viewer. This way, no matter how the scale was,
  // the displayed checkerboard's cells on the background will be remained
  // to a fixed size.
  DrawCheckerboardPattern( FCheckerboardBmp,
    Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

  // We must invoke the Changed() of the selected layer to actually
  // refresh the view.
  FLayerList.SelectedLayer.Changed;
end;

// callback function for OnPixelCombine event of FMeasureLayer.Bitmap
procedure TfrmChild.MeasureLayerBlend(
  F: TColor32; var B: TColor32; M: TColor32);
var
  LBlendColor : TColor32;
begin
  if (F and $FFFFFF) < $FFFFFF then
  begin
    LBlendColor := not ($FF000000 xor B);
    B           := $FF000000 or LBlendColor;
  end;
end;

function TfrmChild.CreateBrightContrastLayer(const ABrightAmount: Integer = 0;
  const AContrastAmount: Integer = 0): TgmCustomLayer;
var
  LBCLayer : TgmBrightContrastLayer;
begin
  LBCLayer := TgmBrightContrastLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LBCLayer do
  begin
    BrightAmount          := ABrightAmount;
    ContrastAmount        := AContrastAmount;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.BrightContrastLayerLogoThumbDblClick;
  end;

  Result := LBCLayer;
end;

function TfrmChild.CreateChannelMixerLayer: TgmCustomLayer;
var
  LChannelMixerLayer : TgmChannelMixerLayer;
begin
  LChannelMixerLayer := TgmChannelMixerLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LChannelMixerLayer do
  begin
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.ChannelMixerLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LChannelMixerLayer;
end;

function TfrmChild.CreateColorBalanceLayer(
  const ATransferMode: TgmTransferMode = tmMidtones;
  const ACyanRedValue: Integer = 0;
  const AMagentaGreen: Integer = 0;
  const AYellowBlue: Integer = 0;
  const APreserveLuminosity: Boolean = True): TgmCustomLayer;
var
  LCBLayer : TgmColorBalanceLayer;
begin
  LCBLayer := TgmColorBalanceLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LCBLayer do
  begin
    ColorBalance.TransferMode       := ATransferMode;
    ColorBalance.CyanRed            := ACyanRedValue;
    ColorBalance.MagentaGreen       := AMagentaGreen;
    ColorBalance.YellowBlue         := AYellowBlue;
    ColorBalance.PreserveLuminosity := APreserveLuminosity;

    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.ColorBalanceLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LCBLayer;
end;

function TfrmChild.CreateCurvesLayer: TgmCustomLayer;
var
  LCurvesLayer : TgmCurvesLayer;
begin
  LCurvesLayer := TgmCurvesLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LCurvesLayer do
  begin
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.CurvesLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LCurvesLayer;
end;

function TfrmChild.CreateGradientFillLayer: TgmCustomLayer;
var
  LGradientFillLayer : TgmGradientFillLayer;
begin
  LGradientFillLayer := TgmGradientFillLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LGradientFillLayer do
  begin
    Gradient              := frmGradientPicker.GetFillLayerGradient;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.GradientFillLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LGradientFillLayer;
end;

function TfrmChild.CreateGradientMapLayer: TgmCustomLayer;
var
  LGradientMapLayer : TgmGradientMapLayer;
begin
  LGradientMapLayer := TgmGradientMapLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LGradientMapLayer do
  begin
    Gradient              := frmGradientPicker.GetMapLayerGradient;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.GradientMapLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LGradientMapLayer;
end;

function TfrmChild.CreateHueSaturationLayer(
  const AMode: TgmHueSaturationAdjustMode = hsamHSL;
  const AHue: Integer = 0; const ASaturation: Integer = 0;
  const ALightValue: Integer = 0): TgmCustomLayer;
var
  LHueSaturationLayer : TgmHueSaturationLayer;
begin
  LHueSaturationLayer := TgmHueSaturationLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LHueSaturationLayer do
  begin
    AdjustMode            := AMode;
    Hue                   := AHue;
    Saturation            := ASaturation;
    LightValue            := ALightValue;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.HueSaturationLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LHueSaturationLayer;
end;

function TfrmChild.CreateInvertLayer: TgmCustomLayer;
var
  LInvertLayer : TgmInvertLayer;
begin
  LInvertLayer := TgmInvertLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LInvertLayer do
  begin
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LInvertLayer;
end;

function TfrmChild.CreateLevelsLayer: TgmCustomLayer;
var
  LLevelsLayer : TgmLevelsLayer;
begin
  LLevelsLayer := TgmLevelsLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LLevelsLayer do
  begin
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.LevelsLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LLevelsLayer;
end;

function TfrmChild.CreateNormalLayer(
  const ABackColor: TColor32 = $00000000;
  const AsBackLayer: Boolean = False): TgmCustomLayer;
begin
  Result := TgmNormalLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height,
    ABackColor, AsBackLayer);

  with TgmNormalLayer(Result) do
  begin
    OnChange              := Self.AfterLayerPanelChanged;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
    OnMaskApplied         := Self.AfterLayerMaskApplied;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
  end;
end;

function TfrmChild.CreatePatternLayer: TgmCustomLayer;
var
  LPatternLayer : TgmPatternLayer;
begin
  LPatternLayer := TgmPatternLayer.Create(
    FLayerList, imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LPatternLayer do
  begin
    PatternBitmap         := frmPatterns.LayerPattern;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.PatternLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LPatternLayer;
end;

function TfrmChild.CreatePosterizeLayer(const ALevel: Byte = 2): TgmCustomLayer;
var
  LPosterizeLayer : TgmPosterizeLayer;
begin
  LPosterizeLayer := TgmPosterizeLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LPosterizeLayer do
  begin
    Level                 := ALevel;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.PosterizeLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LPosterizeLayer;
end;

function TfrmChild.CreateRichTextLayer: TgmCustomLayer;
var
  LRichTextLayer : TgmRichTextLayer;
begin
  LRichTextLayer := TgmRichTextLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LRichTextLayer do
  begin
    AssociateTextEditor(frmRichTextEditor.rchedtRichTextEditor);

    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.RichTextLayerLogoThumbDblClick;
  end;

  Result := LRichTextLayer;
end;

function TfrmChild.CreateShapeRegionLayer: TgmCustomLayer;
var
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  LShapeRegionLayer := TgmShapeRegionLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LShapeRegionLayer do
  begin
    BrushStyle            := frmMain.ShapeBrushStyle;
    RegionColor           := frmMain.GlobalForeColor;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.ShapeRegionLayerLogoThumbDblClick;
    OnPanelDblClick       := Self.ShapeRegionLayerPanelDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LShapeRegionLayer;
end;

function TfrmChild.CreateSolidColorLayer(
  const AColor: TColor32 = clBlack32): TgmCustomLayer;
var
  LSolidColorLayer : TgmSolidColorLayer;
begin
  LSolidColorLayer := TgmSolidColorLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LSolidColorLayer do
  begin
    SolidColor            := AColor;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.SolidColorLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LSolidColorLayer;
end;

function TfrmChild.CreateThresholdLayer(
  const ALevel: Byte = 127): TgmCustomLayer;
var
  LThresholdLayer : TgmThresholdLayer;
begin
  LThresholdLayer := TgmThresholdLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LThresholdLayer do
  begin
    Level                 := ALevel;
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnLogoThumbDblClick   := Self.ThresholdLayerLogoThumbDblClick;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LThresholdLayer;
end;

function TfrmChild.CreateVectorLayer: TgmCustomLayer;
var
  LVectorLayer : TgmVectorLayer;
begin
  LVectorLayer := TgmVectorLayer.Create(FLayerList,
    imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

  with LVectorLayer do
  begin
    OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
    OnChange              := Self.AfterLayerPanelChanged;
    OnLayerDisabled       := Self.OnLayerPanelDisabled;
    OnLayerEnabled        := Self.OnLayerPanelEnabled;
    OnMaskEnabled         := Self.AfterLayerMaskEnabled;
    OnMaskDisabled        := Self.AfterLayerMaskDisabled;
    OnThumbnailUpdate     := Self.AfterLayerPanelChanged;
  end;

  Result := LVectorLayer;
end;

procedure TfrmChild.SetCallbacksForLayersInList;
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if FLayerList.Count > 0 then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];

      with LLayer do
      begin
        OnProcessStageChanged := Self.AfterLayerProcessStageChanged;
        OnChange              := Self.AfterLayerPanelChanged;
        OnLayerDisabled       := Self.OnLayerPanelDisabled;
        OnLayerEnabled        := Self.OnLayerPanelEnabled;
        OnMaskEnabled         := Self.AfterLayerMaskEnabled;
        OnMaskDisabled        := Self.AfterLayerMaskDisabled;
        OnPanelDblClick       := nil;
        OnMaskThumbDblClick   := nil;
      
        if LLayer is TgmNormalLayer then
        begin
          OnThumbnailUpdate                    := Self.AfterLayerPanelChanged;
          TgmNormalLayer(LLayer).OnMaskApplied := Self.AfterLayerMaskApplied;
        end
        else if LLayer is TgmBrightContrastLayer then
        begin
          OnLogoThumbDblClick := Self.BrightContrastLayerLogoThumbDblClick;
        end
        else if LLayer is TgmSolidColorLayer then
        begin
          OnLogoThumbDblClick := Self.SolidColorLayerLogoThumbDblClick;
        end
        else if LLayer is TgmColorBalanceLayer then
        begin
          OnLogoThumbDblClick := Self.ColorBalanceLayerLogoThumbDblClick;
        end
        else if LLayer is TgmHueSaturationLayer then
        begin
          OnLogoThumbDblClick := Self.HueSaturationLayerLogoThumbDblClick;
        end
        else if LLayer is TgmThresholdLayer then
        begin
          OnLogoThumbDblClick := Self.ThresholdLayerLogoThumbDblClick;
        end
        else if LLayer is TgmPosterizeLayer then
        begin
          OnLogoThumbDblClick := Self.PosterizeLayerLogoThumbDblClick;
        end
        else if LLayer is TgmLevelsLayer then
        begin
          OnLogoThumbDblClick := Self.LevelsLayerLogoThumbDblClick;
        end
        else if LLayer is TgmCurvesLayer then
        begin
          OnLogoThumbDblClick := Self.CurvesLayerLogoThumbDblClick;
        end
        else if LLayer is TgmGradientMapLayer then
        begin
          OnLogoThumbDblClick := Self.GradientMapLayerLogoThumbDblClick;
        end
        else if LLayer is TgmGradientFillLayer then
        begin
          OnLogoThumbDblClick := Self.GradientFillLayerLogoThumbDblClick;
        end
        else if LLayer is TgmPatternLayer then
        begin
          OnLogoThumbDblClick := Self.PatternLayerLogoThumbDblClick;
        end
        else if LLayer is TgmShapeRegionLayer then
        begin
          OnLogoThumbDblClick := Self.ShapeRegionLayerLogoThumbDblClick;
          OnPanelDblClick     := Self.ShapeRegionLayerPanelDblClick;
        end
        else if LLayer is TgmRichTextLayer then
        begin
          OnLogoThumbDblClick := Self.RichTextLayerLogoThumbDblClick;
        end
        else if LLayer is TgmChannelMixerLayer then
        begin
          OnLogoThumbDblClick := Self.ChannelMixerLayerLogoThumbDblClick;
        end;
      end;
    end;
  end;
end;

// Undo/Redo
procedure TfrmChild.SetEditModeForUndoRedo(const AEditMode: TgmEditMode);
begin
  FEditMode                        := AEditMode;
  frmMain.spdbtnStandardMode.Down  := (FEditMode = emStandardMode);
  frmMain.spdbtnQuickMaskMode.Down := (FEditMode = emQuickMaskMode);

  case FEditMode of
    emStandardMode:
      begin
        frmColor.ColorMode := cmRGB;
      end;

    emQuickMaskMode:
      begin
        frmColor.ColorMode := cmGrayscale;
      end;
  end;
end;

procedure TfrmChild.AirBrushLineOnMask(
  const xStart, yStart, xEnd, yEnd, distance: Integer;
  const ChannelSet: TgmChannelSet);
var
  a,b           : Integer;  // displacements in x and y
  d             : Integer;  // decision variable
  diag_inc      : Integer;  // d's increment for diagonal steps
  dx_diag       : Integer;  // diagonal x step for next pixel
  dx_nondiag    : Integer;  // nondiagonal x step for next pixel
  dy_diag       : Integer;  // diagonal y step for next pixel
  dy_nondiag    : Integer;  // nondiagonal y step for next pixel
  i             : Integer;  // loop index
  nondiag_inc   : Integer;  // d's increment for nondiagonal steps
  swap          : Integer;  // temporary variable for swap
  x,y           : Integer;  // current x and y coordinates
  LTimerEnabled : Boolean;
begin {DrawLine}
  if Assigned(frmMain.AirBrush) then
  begin
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

    LTimerEnabled             := tmrSpecialBrushes.Enabled;
    tmrSpecialBrushes.Enabled := False;
    try
      for i := 0 to a do    // draw the a+1 pixels
      begin
        if Ftavolsag >= distance then
        begin
          frmMain.AirBrush.Draw(FLayerList.SelectedLayer.MaskBitmap, x, y, ChannelSet);

          // paint on layer mask channel as well
          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            frmMain.AirBrush.Draw(
              FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
              x, y, ChannelSet);
          end;
          
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

    finally
      tmrSpecialBrushes.Enabled := LTimerEnabled;
    end;
  end;
end;

procedure TfrmChild.BrushMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LBrushName      : string;
  LBrushArea      : TRect;
  LPoint          : TPoint;
  LSampleBmp      : TBitmap32;
  LCloneStamp     : TgmCloneStamp;
  LDynamicOpacity : Byte;
  LStartColor     : TColor32;
  LEndColor       : TColor32;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
{ Mouse left button down }

  if Button = mbLeft then
  begin
    // showing the coordinates of the starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

//-- check availability of various brushes begin -------------------------------
    
    if FChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(FChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
                   
        Exit;
      end;
    end;

    if FChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if not (FLayerList.SelectedLayer is TgmNormalLayer) then
      begin
        LBrushName := frmMain.GetBrushName;

        MessageDlg('Could not use the ' + LBrushName + ' because the' + #10#13 +
                   'content of the layer is not directly' + #10#13 +
                   'editable.', mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if frmMain.BrushTool in [btHistoryBrush,
                             btCloneStamp,
                             btPatternStamp] then
    begin
      if frmMain.GMBrush.BrushID = bidHistoryBrush then
      begin
        if FChannelManager.CurrentChannelType in [
             ctAlphaChannel, ctQuickMaskChannel] then
        begin
          MessageDlg('Could not use the history brush because the' + #10#13 +
                     'history state lacks a corresponding channel.', mtError, [mbOK], 0);

          Exit;
        end;

        if FChannelManager.CurrentChannelType = ctLayerMaskChannel then
        begin
          MessageDlg('Could not use the history brush because the' + #10#13 +
                     'history state does not contain a corresponding' + #10#13 +
                     'layer.', mtError, [mbOK], 0);

          Exit;
        end;

        // must be on layer
        if not Assigned(FSelection) then
        begin
          if (FLayerList.SelectedLayer.LayerBitmap.Width  <> FHistoryBitmap.Width) or
             (FLayerList.SelectedLayer.LayerBitmap.Height <> FHistoryBitmap.Height) then
          begin
            MessageDlg('Could not use the history brush because the current' + #10#13 +
                       'canvas size does not match that of the history state!', mtError, [mbOK], 0);

            Exit;
          end;
        end;
      end
      else if frmMain.GMBrush.BrushID = bidCloneStamp then
      begin
        if not (ssAlt in Shift) then
        begin
          if not TgmCloneStamp(frmMain.GMBrush).IsSamplingPointExist then
          begin
            MessageDlg('Could not use the cloning stamp because the area to' + #10#13 +
                       'clone has not been defined(Alt-click to define a' + #10#13 +
                       'source point).', mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end
      else if frmMain.GMBrush.BrushID = bidPatternStamp then
      begin
        if not Assigned(frmPatterns.StampPattern) then
        begin
          MessageDlg('The pattern has not selected.', mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end;

//-- check availability of various brushes end ---------------------------------

    if Assigned(FSelection) then
    begin
      // don't draw the Marching-Ants lines dynamically when processing image
      if FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;

      // confirm the foreground of the selection to avoid the distortion of the brush stroke
      FSelection.ConfirmForeground;
      CalcSelectionCoord;

      Felozox          := FMarqueeX;
      Felozoy          := FMarqueeY;
      FPrevStrokePoint := Point(FMarqueeX, FMarqueeY)
    end
    else
    begin
      Felozox          := FXActual;
      Felozoy          := FYActual;
      FPrevStrokePoint := Point(FXActual, FYActual);
    end;

    if Assigned(frmMain.AirBrush) then
    begin
      frmMain.AirBrush.IsLockTransparent := FLayerList.SelectedLayer.IsLockTransparency;
    end;

    if Assigned(frmMain.JetGun) then
    begin
      frmMain.JetGun.IsLockTransparent := FLayerList.SelectedLayer.IsLockTransparency;
    end;

    // Remember bitmap for create Undo/Redo commands.
    if Assigned(FSelection) then
    begin
      frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(FChannelManager.QuickMaskChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end;

    Ftavolsag := 0; // For BrushLine function

//-- brush settings begin ------------------------------------------------------

    if frmMain.BrushTool in [btPaintBrush,
                             btHistoryBrush,
                             btCloneStamp,
                             btPatternStamp,
                             btBlurSharpenBrush,
                             btSmudge,
                             btDodgeBurnBrush,
                             btLightBrush] then
    begin
      tmrSpecialBrushes.Enabled := False;
      
      if Assigned(frmMain.GMBrush) then
      begin
        with frmMain.GMBrush do
        begin
          LDynamicOpacity := frmMain.BrushOpacity;

          if FChannelManager.CurrentChannelType = ctColorChannel then
          begin
            LStartColor := Color32(frmMain.GlobalForeColor);
            LEndColor   := Color32(frmMain.GlobalBackColor);
          end
          else
          begin
            LStartColor := Color32(frmMain.ForeGrayColor);
            LEndColor   := Color32(frmMain.BackGrayColor);
          end;

          IsPreserveTransparency := FLayerList.SelectedLayer.IsLockTransparency;

          SetBlendMode(frmMain.BrushBlendMode);
          SetBrushOpacity(frmMain.BrushOpacity);
          SetBrushIntensity(frmMain.BrushIntensity);
          SetPaintingStroke(frmPaintingBrush.BrushStroke);

          if BrushID = bidPaintBrush then
          begin
            // set brush color ...
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              TgmPaintBrush(frmMain.GMBrush).SetColor( Color32(frmMain.GlobalForeColor) );
            end
            else
            begin
              TgmPaintBrush(frmMain.GMBrush).SetColor( Color32(frmMain.ForeGrayColor) );
            end;
          end
          else if BrushID = bidPatternStamp then
          begin
            if Assigned(FSelection) then
            begin
              TgmPatternStamp(frmMain.GMBrush).SetPatternBitmap(
                frmPatterns.StampPattern,
                FSelection.CutOriginal.Width,
                FSelection.CutOriginal.Height);
            end
            else
            begin
              TgmPatternStamp(frmMain.GMBrush).SetPatternBitmap(
                frmPatterns.StampPattern,
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height);
            end;
          end
          else if BrushID = bidBlurSharpen then
          begin
            with TgmBlurSharpenBrush(frmMain.GMBrush) do
            begin
              Pressure := frmMain.BlurSharpenPressure;

              // Brush Dynamics Settings ...
              SetDynamicPressure(frmMain.BlurSharpenPressure,
                                 frmBrushDynamics.OpacityDynamicsState,
                                 frmBrushDynamics.OpacitySteps);
            end;
          end
          else if BrushID = bidSmudge then
          begin
            LDynamicOpacity := frmMain.SmudgePressure;
            
            TgmSmudge(frmMain.GMBrush).SetPressure(frmMain.SmudgePressure);
          end
          else if BrushID = bidDodgeBurn then
          begin
            LDynamicOpacity := frmMain.DodgeBurnExposure;

            with TgmDodgeBurnBrush(frmMain.GMBrush) do
            begin
              SetDodgeBurnMode(frmMain.DodgeBurnMode);
              SetDodgeBurnExposure(frmMain.DodgeBurnExposure);
              MakeLUT;
            end;
          end
          else if BrushID = bidLightBrush then
          begin
            LDynamicOpacity := MulDiv(255, frmMain.BrushIntensity, 100);
          end;

          // Brush Dynamics Settings ...
          SetDynamicSize(frmPaintingBrush.BrushStroke,
                         frmBrushDynamics.SizeDynamicsState,
                         frmBrushDynamics.SizeSteps);

          SetDynamicOpacity(LDynamicOpacity,
                            frmBrushDynamics.OpacityDynamicsState,
                            frmBrushDynamics.OpacitySteps);

          SetDynamicColor(LStartColor, LEndColor,
                          frmBrushDynamics.ColorDynamicsState,
                          frmBrushDynamics.ColorSteps );
        end;
      end;
    end;

//-- brush settings end --------------------------------------------------------

    case frmMain.BrushTool of
      btPaintBrush,
      btPatternStamp,
      btBlurSharpenBrush,
      btDodgeBurnBrush,
      btLightBrush:
        begin
          if Assigned(FSelection) then
          begin
            { Assigned the CutOriginal to FSouceBMP of the Brush to let the
              cropped area as the background, and then cut area from it as
              the new cropped area, and drawing the brush in the new area
              to make the brush opacity setting takes effect. }
            frmMain.GMBrush.UpdateSourceBitmap(FSelection.CutOriginal);

            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              frmMain.GMBrush.Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                    FChannelManager.SelectedColorChannels);
            end
            else
            begin
              frmMain.GMBrush.Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                    [csGrayscale]);
            end;

            LBrushArea := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            ShowSelectionAtBrushStroke(LBrushArea);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  frmMain.GMBrush.UpdateSourceBitmap(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                  frmMain.GMBrush.Paint(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea  := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  frmMain.GMBrush.UpdateSourceBitmap(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                  frmMain.GMBrush.Paint(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  frmMain.GMBrush.UpdateSourceBitmap(FLayerList.SelectedLayer.MaskBitmap);

                  // get brush area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  frmMain.GMBrush.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                        FXActual, FYActual, [csGrayscale]);

                  // draw on layer mask channel as well
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  frmMain.GMBrush.UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);

                  frmMain.GMBrush.Paint(FLayerList.SelectedLayer.LayerBitmap,
                    FXActual, FYActual, FChannelManager.SelectedColorChannels);

                  // get refresh area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;
          end;

          if frmMain.GMBrush.BrushID = bidBlurSharpen then
          begin
            tmrSpecialBrushes.Interval := frmMain.BlurSharpenTimerInterval;

            if tmrSpecialBrushes.Enabled <> True then
            begin
              tmrSpecialBrushes.Enabled := True;
            end;
          end;
        end;

      btHistoryBrush:
        begin
          if frmMain.GMBrush.BrushID = bidHistoryBrush then
          begin
            TgmHistoryBrush(frmMain.GMBrush).LoadHistoryBitmap(FHistoryBitmap);

            if Assigned(FSelection) then
            begin
              with frmMain.GMBrush do
              begin
                // then setting the history sample offset
                SelectionOffsetX := FSelection.MaskBorderStart.X;
                SelectionOffsetY := FSelection.MaskBorderStart.Y;

                // finally, painting
                UpdateSourceBitmap(FSelection.CutOriginal);

                if FChannelManager.CurrentChannelType = ctColorChannel then
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                        FChannelManager.SelectedColorChannels);
                end
                else
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY, [csGrayscale]);
                end;
              end;

              LBrushArea := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              frmMain.GMBrush.UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);

              frmMain.GMBrush.Paint(FLayerList.SelectedLayer.LayerBitmap,
                FXActual, FYActual, FChannelManager.SelectedColorChannels);

              // get refresh area
              LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

              FLayerList.SelectedLayer.Changed(LBrushArea);
            end;
          end;
        end;

      btCloneStamp:
        begin
          if frmMain.GMBrush.BrushID = bidCloneStamp then
          begin
            // If the users click the mouse with the Alt key is pressed, then
            // we redefine the sample point of the clone stamp. And then mark
            // the sample point is already defined, and we need to do some
            // calculations related to the sample point and current point.
            // These task will be done by the single method call -- SetSamplingPoint().
            if ssAlt in Shift then
            begin
              TgmCloneStamp(frmMain.GMBrush).SetSamplingPoint(FXActual, FYActual);

              if Assigned(FSelection) then
              begin
                FSelection.IsAnimated := True;
              end;

              Exit;  // calling Exit() to avoid the OnMouseUp event to be executed.
            end;

            // If the property IsUpdateStampOffset is true, then it indicates
            // that we have already defined a sample point for the clone stamp,
            // we need to calculate the offset vector from current point to
            // sample point. We only need to do this once after the sample
            // point is redefined. 
            if TgmCloneStamp(frmMain.GMBrush).IsUpdateStampOffset then
            begin
              TgmCloneStamp(frmMain.GMBrush).SetStampPointOffset(FXActual, FYActual);
            end;

            with frmMain.GMBrush do
            begin
              // update the sampling bitmap
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    UpdateSourceBitmap(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
                  end;

                ctQuickMaskChannel:
                  begin
                    UpdateSourceBitmap(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
                  end;

                ctLayerMaskChannel:
                  begin
                    UpdateSourceBitmap(FLayerList.SelectedLayer.MaskBitmap);
                  end;

                ctColorChannel:
                  begin
                    // must be on layer
                    if frmMain.IsUseAllLayers then
                    begin
                      // get layer blending result, but without mask applied
                      LSampleBmp := FLayerList.GetLayerBlendResult(False);

                      if Assigned(LSampleBmp) then
                      begin
                        UpdateSourceBitmap(LSampleBmp);
                        LSampleBmp.Free;
                      end;
                    end
                    else
                    begin
                      UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);
                    end;
                  end;
              end;
            end;

            if Assigned(FSelection) then
            begin
              with frmMain.GMBrush do
              begin
                // then setting the history sample offset
                SelectionOffsetX := FSelection.MaskBorderStart.X;
                SelectionOffsetY := FSelection.MaskBorderStart.Y;

                // make a copy of the original bitmap
                UpdateForeground(FSelection.CutOriginal);

                // finally, painting
                if FChannelManager.CurrentChannelType = ctColorChannel then
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                        FChannelManager.SelectedColorChannels);
                end
                else
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY, [csGrayscale]);
                end;
              end;

              LBrushArea := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    frmMain.GMBrush.UpdateForeground(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                    frmMain.GMBrush.Paint(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctQuickMaskChannel:
                  begin
                    frmMain.GMBrush.UpdateForeground(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                    frmMain.GMBrush.Paint(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctLayerMaskChannel:
                  begin
                    frmMain.GMBrush.UpdateForeground(FLayerList.SelectedLayer.MaskBitmap);

                    // get brush area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    frmMain.GMBrush.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                          FXActual, FYActual, [csGrayscale]);

                    // paint on layer mask channel as well
                    if Assigned(FChannelManager.LayerMaskChannel) then
                    begin
                      FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                        LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                    end;

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;

                ctColorChannel:
                  begin
                    // must be on layer

                    frmMain.GMBrush.UpdateForeground(FLayerList.SelectedLayer.LayerBitmap);

                    frmMain.GMBrush.Paint(FLayerList.SelectedLayer.LayerBitmap,
                      FXActual, FYActual, FChannelManager.SelectedColorChannels);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;
              end;
            end;

            // Draw aim flag of the clone stamp on imgWorkArea.Canvas
            // with the pen mode of the canvas set to pmNotXor mode. 

            LCloneStamp := TgmCloneStamp(frmMain.GMBrush);

            FAimPoint := imgWorkArea.BitmapToControl(
              Point(FXActual + LCloneStamp.OffsetX,
                    FYActual + LCloneStamp.OffsetY) );
                                                            
            // this method will draw the aim flag with pen mode set to pmNotXor
            LCloneStamp.DrawStampAimFlag(imgWorkArea.Canvas, FAimPoint.X, FAimPoint.Y);
          end;
        end;

      btSmudge:
        begin
          if frmMain.GMBrush.BrushID = bidSmudge then
          begin
            if Assigned(FSelection) then
            begin
              { The CutRegionToForegroundBySize procedure using the half width
                and half height value which was from the TgmBrush class. It
                was already assigned above. }
                
              TgmSmudge(frmMain.GMBrush).CutRegionToForegroundBySize(
                FSelection.CutOriginal, FMarqueeX, FMarqueeY);

              if FChannelManager.CurrentChannelType = ctColorChannel then
              begin
                frmMain.GMBrush.Paint(FSelection.CutOriginal,
                  FMarqueeX, FMarqueeY, FChannelManager.SelectedColorChannels);
              end
              else
              begin
                frmMain.GMBrush.Paint(FSelection.CutOriginal,
                  FMarqueeX, FMarqueeY, [csGrayscale]);
              end;

              LBrushArea := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    TgmSmudge(frmMain.GMBrush).CutRegionToForegroundBySize(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual);

                    frmMain.GMBrush.Paint(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctQuickMaskChannel:
                  begin
                    TgmSmudge(frmMain.GMBrush).CutRegionToForegroundBySize(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual);

                    frmMain.GMBrush.Paint(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctLayerMaskChannel:
                  begin
                    TgmSmudge(frmMain.GMBrush).CutRegionToForegroundBySize(
                      FLayerList.SelectedLayer.MaskBitmap, FXActual, FYActual);

                    // get brush area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    frmMain.GMBrush.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                          FXActual, FYActual, [csGrayscale]);

                    // paint on layer mask channel as well
                    if Assigned(FChannelManager.LayerMaskChannel) then
                    begin
                      FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                        LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                    end;

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;

                ctColorChannel:
                  begin
                    TgmSmudge(frmMain.GMBrush).CutRegionToForegroundBySize(
                      FLayerList.SelectedLayer.LayerBitmap, FXActual, FYActual);

                    frmMain.GMBrush.Paint(FLayerList.SelectedLayer.LayerBitmap,
                      FXActual, FYActual, FChannelManager.SelectedColorChannels);

                    // get refresh area
                    LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;
              end;
            end;
          end;
        end;

      btAirBrush:
        begin
          if frmMain.AirBrush.IsAir and
             Assigned(frmPaintingBrush.BrushStroke) then
          begin
            with frmMain.AirBrush do
            begin
              // set brush color
              if FChannelManager.CurrentChannelType = ctColorChannel then
              begin
                Color := Color32(frmMain.GlobalForeColor);
              end
              else
              begin
                Color := Color32(frmMain.ForeGrayColor);
              end;

              AirIntensity := frmMain.AirPressure;
              BlendMode    := frmMain.BrushBlendMode;
              SetPaintingStroke(frmPaintingBrush.BrushStroke);

              { Brush Dynamics Settings }
              SetDynamicSize(frmPaintingBrush.BrushStroke,
                             frmBrushDynamics.SizeDynamicsState,
                             frmBrushDynamics.SizeSteps);

              SetDynamicPressure(frmMain.AirPressure,
                                 frmBrushDynamics.OpacityDynamicsState,
                                 frmBrushDynamics.OpacitySteps);

              SetDynamicColor( Color32(frmMain.GlobalForeColor),
                               Color32(frmMain.GlobalBackColor),
                               frmBrushDynamics.ColorDynamicsState,
                               frmBrushDynamics.ColorSteps );
            end;

            if Assigned(FSelection) then
            begin
              frmMain.AirBrush.UpdateSourceBitmap(FSelection.CutOriginal);

              if FChannelManager.CurrentChannelType = ctColorChannel then
              begin
                frmMain.AirBrush.Draw(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                      FChannelManager.SelectedColorChannels);
              end
              else
              begin
                frmMain.AirBrush.Draw(FSelection.CutOriginal,
                                      FMarqueeX, FMarqueeY, [csGrayscale]);
              end;

              // get brush area
              LBrushArea := frmMain.AirBrush.GetBrushArea(FMarqueeX, FMarqueeY);

              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    if Assigned(FChannelManager.SelectedAlphaChannel) then
                    begin
                      frmMain.AirBrush.UpdateSourceBitmap(
                        FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                      frmMain.AirBrush.Draw(
                        FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                        FXActual, FYActual, [csGrayscale]);

                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
                    end
                    else
                    begin
                      MessageDlg('Could not process more than one alpha channels at a time.', mtError, [mbOK], 0);
                      Exit;
                    end;
                  end;

                ctQuickMaskChannel:
                  begin
                    frmMain.AirBrush.UpdateSourceBitmap(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                    frmMain.AirBrush.Draw(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
                  end;

                ctLayerMaskChannel:
                  begin
                    frmMain.AirBrush.UpdateSourceBitmap(FLayerList.SelectedLayer.MaskBitmap);

                    frmMain.AirBrush.Draw(FLayerList.SelectedLayer.MaskBitmap,
                                          FXActual, FYActual, [csGrayscale]);

                    // paint on layer mask channel as well
                    if Assigned(FChannelManager.LayerMaskChannel) then
                    begin
                      frmMain.AirBrush.Draw(
                        FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                        FXActual, FYActual, [csGrayscale]);
                    end;

                    // get brush area
                    LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;

                ctColorChannel:
                  begin
                    frmMain.AirBrush.UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);

                    frmMain.AirBrush.Draw(FLayerList.SelectedLayer.LayerBitmap,
                      FXActual, FYActual, FChannelManager.SelectedColorChannels);
                  
                    // get refresh area
                    LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;
              end;
            end;

            tmrSpecialBrushes.Interval := frmMain.AirBrush.Interval;

            if not tmrSpecialBrushes.Enabled then
            begin
              tmrSpecialBrushes.Enabled := True;
            end;
          end;
        end;

      btJetGunBrush:
        begin
          if Assigned(frmMain.JetGun) then
          begin
            with frmMain.JetGun do
            begin
              Radius      := frmMain.updwnBrushRadius.Position;
              IsRandom    := frmMain.IsRandomColor;
              JetGunIndex := 0;
              
              SetBlendMode(frmMain.BrushBlendMode);
              SetPressure(frmMain.JetGunPressure);
            end;

            if Assigned(FSelection) then
            begin
              frmMain.JetGun.UpdateSourceBitmap(FSelection.CutOriginal);
              
              if FChannelManager.CurrentChannelType = ctColorChannel then
              begin
                frmMain.JetGun.Color := Color32(frmMain.GlobalForeColor);

                frmMain.JetGun.Jet(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                   FChannelManager.SelectedColorChannels);
              end
              else
              begin
                frmMain.JetGun.Color := Color32(frmMain.ForeGrayColor);

                frmMain.JetGun.Jet(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                   [csGrayscale]);
              end;

              // get brush area
              LBrushArea := frmMain.JetGun.GetJetArea(FMarqueeX, FMarqueeY);

              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    frmMain.JetGun.Color := Color32(frmMain.ForeGrayColor);

                    frmMain.JetGun.UpdateSourceBitmap(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                    frmMain.JetGun.Jet(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctQuickMaskChannel:
                  begin
                    frmMain.JetGun.Color := Color32(frmMain.ForeGrayColor);

                    frmMain.JetGun.UpdateSourceBitmap(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                    frmMain.JetGun.Jet(
                      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;

                ctLayerMaskChannel:
                  begin
                    frmMain.JetGun.Color := Color32(frmMain.ForeGrayColor);
                    
                    frmMain.JetGun.UpdateSourceBitmap(FLayerList.SelectedLayer.MaskBitmap);

                    LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                    frmMain.JetGun.Jet(FLayerList.SelectedLayer.MaskBitmap,
                                       FXActual, FYActual, [csGrayscale]);

                    // update the mask channel preview layer
                    if Assigned(FChannelManager.LayerMaskChannel) then
                    begin
                      FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                        LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                    end;

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;

                ctColorChannel:
                  begin
                    frmMain.JetGun.Color := Color32(frmMain.GlobalForeColor);
                    
                    // get refresh area
                    LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                    frmMain.JetGun.UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);

                    frmMain.JetGun.Jet(FLayerList.SelectedLayer.LayerBitmap,
                      FXActual, FYActual, FChannelManager.SelectedColorChannels);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;
              end;
            end;

            // dynamics settings ...
            with frmMain.JetGun do
            begin
              SetDynamicRadius(frmMain.updwnBrushRadius.Position,
                               frmBrushDynamics.SizeDynamicsState,
                               frmBrushDynamics.SizeSteps);

              SetDynamicPressure( MulDiv(255, frmMain.JetGunPressure, 100),
                                  frmBrushDynamics.OpacityDynamicsState,
                                  frmBrushDynamics.OpacitySteps );

              SetDynamicColor( Color32(frmMain.GlobalForeColor),
                               Color32(frmMain.GlobalBackColor),
                               frmBrushDynamics.ColorDynamicsState,
                               frmBrushDynamics.ColorSteps );
            end;

            if not tmrSpecialBrushes.Enabled then
            begin
              tmrSpecialBrushes.Enabled := True;
            end;
          end;
        end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.BrushMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor          : TColor;
  LCloneStamp     : TgmCloneStamp;
  LInterval       : Integer;
  LLastStrokeArea : TRect;
  LBrushArea      : TRect;
  LPoint          : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  CalcSelectionCoord;    // get selection space coordinates

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    case frmMain.BrushTool of
      btPaintBrush,
      btHistoryBrush,
      btCloneStamp,
      btPatternStamp,
      btBlurSharpenBrush,
      btSmudge,
      btDodgeBurnBrush,
      btLightBrush:
        begin
          if frmMain.BrushTool in [btBlurSharpenBrush, btSmudge] then
          begin
            LInterval := 0;
          end
          else
          begin
            LInterval := frmMain.BrushInterval;
          end;

          if Assigned(FSelection) then
          begin
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                        FMarqueeX, FMarqueeY,
                        LInterval, FSelection.CutOriginal,
                        FChannelManager.SelectedColorChannels);
            end
            else
            begin
              BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                        FMarqueeX, FMarqueeY, LInterval, FSelection.CutOriginal,
                        [csGrayscale]);
            end;

            // get brush area
            LLastStrokeArea := frmMain.GMBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
            LBrushArea      := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);
    
            ShowSelectionAtBrushStroke(LBrushArea);
            FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                            FXActual, FYActual, LInterval,
                            FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                            [csGrayscale]);

                  // get refresh area
                  LLastStrokeArea := frmMain.GMBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                            FXActual, FYActual, LInterval,
                            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                            [csGrayscale]);

                  // get refresh area
                  LLastStrokeArea := frmMain.GMBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                            FXActual, FYActual, LInterval,
                            FLayerList.SelectedLayer.MaskBitmap,
                            [csGrayscale]);

                  // get brush area
                  LLastStrokeArea := frmMain.GMBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  // paint to layer mask channel as well
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  BrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                            FXActual, FYActual, LInterval,
                            FLayerList.SelectedLayer.LayerBitmap,
                            FChannelManager.SelectedColorChannels);

                  // get refresh area
                  LLastStrokeArea := frmMain.GMBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;

            FPrevStrokePoint := Point(FXActual, FYActual);
          end;

          if frmMain.BrushTool = btCloneStamp then
          begin
            LCloneStamp := TgmCloneStamp(frmMain.GMBrush);

            { Because of aim flag is drawn on the canvas with pen mode
              set to pmNotXor, draw it again at the last position will clear
              the old flag on the canvas. }
            LCloneStamp.DrawStampAimFlag(imgWorkArea.Canvas, FAimPoint.X, FAimPoint.Y);

            FAimPoint := imgWorkArea.BitmapToControl( Point(FXActual + LCloneStamp.OffsetX,
                                                            FYActual + LCloneStamp.OffsetY) );

            // draw aim flag at new postion with pen mode set to pmNotXor
            LCloneStamp.DrawStampAimFlag(imgWorkArea.Canvas, FAimPoint.X, FAimPoint.Y);
          end;
        end;

      btAirBrush:
        begin
          if Assigned(FSelection) then
          begin
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              AirBrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                           FMarqueeX, FMarqueeY, 0, FSelection.CutOriginal,
                           FChannelManager.SelectedColorChannels);
            end
            else
            begin
              AirBrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                           FMarqueeX, FMarqueeY, 0, FSelection.CutOriginal,
                           [csGrayscale]);
            end;

            // get brush area
            LLastStrokeArea := frmMain.AirBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
            LBrushArea      := frmMain.AirBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);
    
            ShowSelectionAtBrushStroke(LBrushArea);
            
            FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  AirBrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, 0,
                               FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                               [csGrayscale]);

                  // get refresh area
                  LLastStrokeArea := frmMain.AirBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  AirBrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, 0,
                               FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                               [csGrayscale]);

                  // get refresh area
                  LLastStrokeArea := frmMain.AirBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  // AirBrushLineOnMask() will paint brush stroke both on
                  // FLayerList.SelectedPanel.MaskLayer and
                  // FChannelManagerMini.LayerMaskChannel.ChannelLayer.Bitmap.
                  AirBrushLineOnMask(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                                     FXActual, FYActual, 0, [csGrayscale]);

                  // get brush area
                  LLastStrokeArea := frmMain.AirBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  AirBrushLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, 0,
                               FLayerList.SelectedLayer.LayerBitmap,
                               FChannelManager.SelectedColorChannels);

                  // get refresh area
                  LLastStrokeArea := frmMain.AirBrush.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;

            FPrevStrokePoint := Point(FXActual, FYActual);
          end;
        end;
    end;
  end
  else // if the FDrawing = False
  begin
    if frmMain.BrushTool = btCloneStamp then
    begin
      if ssAlt in Shift then
      begin
        Screen.Cursor      := crCloneStamp;
        imgWorkArea.Cursor := crCloneStamp;
      end
      else
      begin
        Screen.Cursor      := crDefault;
        imgWorkArea.Cursor := crCross;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing the color info of the pixel that below the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;
  
  imgWorkArea.Canvas.Pen.Mode := pmCopy;
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.BrushMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LCommand            : TgmCustomCommand;
  LCommandName        : string;
  LCommandIconResName : string;
begin
  LCommand := nil;

  if FDrawing then
  begin
    FDrawing := False;

    if tmrSpecialBrushes.Enabled then
    begin
      tmrSpecialBrushes.Enabled := False;
    end;

    // for Undo/Redo
    if frmMain.BrushTool = btAirBrush then
    begin
      LCommandName        := frmMain.AirBrush.Name;
      LCommandIconResName := gmMiscCommandIcons.AIR_BRUSH_COMMAND_ICON_RES_NAME;
    end
    else if frmMain.BrushTool = btJetGunBrush then
    begin
      LCommandName        := frmMain.JetGun.Name;
      LCommandIconResName := gmMiscCommandIcons.JET_GUN_COMMAND_ICON_RES_NAME;
    end
    else
    begin
      LCommandName := frmMain.GMBrush.Name;

      case frmMain.GMBrush.BrushID of
        bidPaintBrush:
          begin
            LCommandIconResName := gmHistoryCommands.PAINT_BRUSH_COMMAND_ICON_RES_NAME;
          end;

        bidHistoryBrush:
          begin
            LCommandIconResName := gmMiscCommandIcons.HISTORY_BRUSH_COMMAND_ICON_RES_NAME;
          end;

        bidCloneStamp:
          begin
            LCommandIconResName := gmMiscCommandIcons.CLONE_STAMP_COMMAND_ICON_RES_NAME;
          end;

        bidPatternStamp:
          begin
            LCommandIconResName := gmMiscCommandIcons.PATTERN_STAMP_COMMAND_ICON_RES_NAME;
          end;

        bidSmudge:
          begin
            LCommandIconResName := gmMiscCommandIcons.SMUDGE_BRUSH_COMMAND_ICON_RES_NAME;
          end;

        bidBlurSharpen:
          begin
            case TgmBlurSharpenBrush(frmMain.GMBrush).ConvolveType of
              gmctBlur:
                begin
                  LCommandIconResName := gmMiscCommandIcons.BLUR_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              gmctSharpen:
                begin
                  LCommandIconResName := gmMiscCommandIcons.SHARPEN_BRUSH_COMMAND_ICON_RES_NAME;
                end;
            end;
          end;

        bidDodgeBurn:
          begin
            case TgmDodgeBurnBrush(frmMain.GMBrush).DodgeBurnType of
              dbtDodge:
                begin
                  LCommandIconResName := gmMiscCommandIcons.DODGE_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              dbtBurn:
                begin
                  LCommandIconResName := gmMiscCommandIcons.BURN_BRUSH_COMMAND_ICON_RES_NAME;
                end;
            end;
          end;

        bidLightBrush:
          begin
            case TgmLightBrush(frmMain.GMBrush).LightMode of
              lbmHighHue:
                begin
                  LCommandIconResName := gmMiscCommandIcons.HUE_UP_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmLowHue:
                begin
                  LCommandIconResName := gmMiscCommandIcons.HUE_DOWN_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmHighSaturation:
                begin
                  LCommandIconResName := gmMiscCommandIcons.SATURATION_UP_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmLowSaturation:
                begin
                  LCommandIconResName := gmMiscCommandIcons.SATURATION_DOWN_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmHighLuminosity:
                begin
                  LCommandIconResName := gmMiscCommandIcons.LUMINOSITY_UP_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmLowLuminosity:
                begin
                  LCommandIconResName := gmMiscCommandIcons.LUMINOSITY_DOWN_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmBrightness:
                begin
                  LCommandIconResName := gmMiscCommandIcons.BRIGHT_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmDarkness:
                begin
                  LCommandIconResName := gmMiscCommandIcons.DARK_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmHighContrast:
                begin
                  LCommandIconResName := gmMiscCommandIcons.CONTRAST_UP_BRUSH_COMMAND_ICON_RES_NAME;
                end;

              lbmLowContrast:
                begin
                  LCommandIconResName := gmMiscCommandIcons.CONTRAST_DOWN_BRUSH_COMMAND_ICON_RES_NAME;
                end;
            end;
          end;
      end;
    end;

    if Assigned(FSelection) then
    begin
      ShowProcessedSelection();
      FSelection.IsAnimated := True;

      // for Undo/Redo
      frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
              LCommandName,
              FChannelManager,
              FChannelManager.AlphaChannelList.SelectedIndex,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              GetSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
              LCommandName,
              FChannelManager,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              GetSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
              LCommandName,
              FChannelManager,
              FLayerList,
              FLayerList.SelectedIndex,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              GetSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
              LCommandName,
              FChannelManager,
              FLayerList,
              FLayerList.SelectedIndex,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              GetSelectionForUndoRedo);
          end;
      end;
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            FChannelManager.SelectedAlphaChannel.ChannelLayer.Changed();
            frmMain.FBitmapAfter.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

            LCommand := TgmAlphaChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              FChannelManager.AlphaChannelList,
              FChannelManager.AlphaChannelList.SelectedIndex);
          end;

        ctQuickMaskChannel:
          begin
            FChannelManager.QuickMaskChannel.ChannelLayer.Changed();
            frmMain.FBitmapAfter.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

            LCommand := TgmQuickMaskChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              FChannelManager);
          end;

        ctLayerMaskChannel:
          begin
            // refresh the whole display
            FLayerList.SelectedLayer.Changed();
            frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.MaskBitmap);

            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              0, 0, FLayerList.SelectedLayer.MaskBitmap);

            // Undo/Redo
            LCommand := TgmLayerMaskProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              FLayerList,
              FLayerList.SelectedIndex);
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.LayerBitmap);
            end;

            // refresh the whole display
            FLayerList.SelectedLayer.Changed();

            // Undo/Redo
            LCommand := TgmLayerImageProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              frmMain.FBitmapAfter,
              FLayerList,
              FLayerList.SelectedIndex);
          end;
      end;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(LCommandIconResName);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.MarqueeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LBmpRect : TRect;
  LPoint   : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

// Mouse left button down 

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    if Assigned(FSelection) then
    begin
      if FSelection.IsAnimated then
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
          if frmMain.MarqueeTool = mtMoveResize then
          begin
            if Assigned(FSelection) then
            begin
              FMarqueeDrawingHandle := dhNone;

              // check if the mouse is pointing on any of the selection handles
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    if Assigned(FChannelManager.SelectedAlphaChannel) then
                    begin
                      FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(
                        X, Y, SELECTION_HANDLE_RADIUS);
                    end;
                  end;

                ctLayerMaskChannel,
                ctQuickMaskChannel:
                  begin
                    FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(
                      X, Y, SELECTION_HANDLE_RADIUS);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(
                        X, Y, SELECTION_HANDLE_RADIUS);
                    end;
                  end;
              end;

              if FMarqueeDrawingHandle in [dhAxAy,
                                           dhBxBy,
                                           dhAxBy,
                                           dhBxAy,
                                           dhLeftHalfAyBy,
                                           dhRightHalfAyBy,
                                           dhTopHalfAxBx,
                                           dhBottomHalfAxBx] then
              begin
                Screen.Cursor        := SetCursorByHandle(FMarqueeDrawingHandle);
                imgWorkArea.Cursor   := Screen.Cursor;
                FMarqueeDrawingState := dsStretchCorner;
              end
              else
              begin
                // check if the mouse pointer is within the range of selection
                if FSelection.ContainsPoint( Point(FXActual, FYActual) ) then
                begin
                  Screen.Cursor        := crDrag;
                  imgWorkArea.Cursor   := Screen.Cursor;
                  FMarqueeDrawingState := dsTranslate;
                  FDrawingBasePoint    := Point(FXActual, FYActual);
                end;
              end;
            end;
          end;
        end;

      dsNewFigure:
        begin
          case frmMain.MarqueeTool of
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
                  if frmMain.MarqueeTool = mtSingleRow then
                  begin
                    FRegion := TgmSingleRowRegion.Create(imgWorkArea.Canvas);
                    
                    TgmSingleRowRegion(FRegion).RowWidth :=
                      FLayerList.SelectedLayer.LayerBitmap.Width;
                  end
                  else if frmMain.MarqueeTool = mtSingleColumn then
                  begin
                    FRegion := TgmSingleColumnRegion.Create(imgWorkArea.Canvas);
                    
                    TgmSingleColumnRegion(FRegion).ColumnHeight :=
                      FLayerList.SelectedLayer.LayerBitmap.Height;
                  end
                  else if frmMain.MarqueeTool = mtRectangular then
                  begin
                    FRegion := TgmRectangularRegion.Create(imgWorkArea.Canvas);
                  end
                  else if frmMain.MarqueeTool = mtRoundRectangular then
                  begin
                    FRegion := TgmRoundRectangularRegion.Create(imgWorkArea.Canvas);
                    TgmRoundRectangularRegion(FRegion).CornerRadius := frmMain.RRMCornerRadius;
                  end
                  else if frmMain.MarqueeTool = mtElliptical then
                  begin
                    FRegion := TgmEllipticRegion.Create(imgWorkArea.Canvas);
                  end
                  else if frmMain.MarqueeTool = mtPolygonal then
                  begin
                    FRegion := TgmPolygonalRegion.Create(imgWorkArea.Canvas);
                  end
                  else if frmMain.MarqueeTool = mtRegularPolygon then
                  begin
                    FRegion := TgmRegularPolygonalRegion.Create(imgWorkArea.Canvas);
                    TgmRegularPolygonalRegion(FRegion).EdgeCount := frmMain.RPMSides;
                  end
                  else if frmMain.MarqueeTool = mtLasso then
                  begin
                    FRegion := TgmLassoRegion.Create(imgWorkArea.Canvas);
                  end;
                end;

                LBmpRect        := imgWorkArea.GetBitmapRect;
                FRegion.OffsetX := LBmpRect.Left;
                FRegion.OffsetY := LBmpRect.Top;
                FRegion.Scale   := imgWorkArea.Scale;

                FRegion.MouseDown(Button, Shift, FXActual, FYActual);

                if ssDouble in Shift then
                begin
                  if frmMain.MarqueeTool = mtPolygonal then
                  begin
                    if Assigned(FRegion) and
                       (FRegion.RegionStyle = gmrsPolygonal) then
                    begin
                      TgmPolygonalRegion(FRegion).DblClick(Sender);
                    end;
                  end;
                end;
              end;

            mtMagneticLasso:
              begin
                if Assigned(FSelection) then
                begin
                  if FSelection.IsTranslated or
                     FSelection.IsCornerStretched or
                     FSelection.IsTransformed or
                     FSelection.IsHorizFlipped or
                     FSelection.IsVertFlipped or
                     FSelection.IsForeAlphaChanged then
                  begin
                    MessageDlg('The selection has been moved, resized, flipped or transformed.' + #10#13 +
                               'Cannot create new selection.', mtError, [mbOK], 0);

                    FSelection.IsAnimated := True;
                    Exit;
                  end;
                end;

                if not Assigned(FMagneticLassoLayer) then
                begin
                  CreateLassoLayer;
                end;

                // create Magnetic Lasso
                if not Assigned(FMagneticLasso) then
                begin
                  case FChannelManager.CurrentChannelType of
                    ctAlphaChannel:
                      begin
                        FMagneticLasso := TgmMagneticLasso.Create(
                          FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                          FMagneticLassoLayer.Bitmap.Canvas);
                      end;

                    ctQuickMaskChannel:
                      begin
                        FMagneticLasso := TgmMagneticLasso.Create(
                          FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                          FMagneticLassoLayer.Bitmap.Canvas);
                      end;

                    ctLayerMaskChannel:
                      begin
                        FMagneticLasso := TgmMagneticLasso.Create(
                          FLayerList.SelectedLayer.MaskBitmap,
                          FMagneticLassoLayer.Bitmap.Canvas);
                      end;

                    ctColorChannel:
                      begin
                        if frmMain.chckbxUseAllLayers.Checked then
                        begin
                          FMagneticLasso := TgmMagneticLasso.Create(
                            FLayerList.CombineResult,
                            FMagneticLassoLayer.Bitmap.Canvas);
                        end
                        else
                        begin
                          FMagneticLasso := TgmMagneticLasso.Create(
                            FLayerList.SelectedLayer.LayerBitmap,
                            FMagneticLassoLayer.Bitmap.Canvas);
                        end;
                      end;
                  end;

                  FMagneticLasso.IsInteractive := frmMain.chckbxMagneticLassoInteractive.Checked;
                end;

                if Assigned(FMagneticLasso) then
                begin
                  FMagneticLasso.MouseDown(Button, Shift, FXActual, FYActual);

                  if Assigned(FMagneticLassoLayer) then
                  begin
                    FMagneticLassoLayer.Changed;
                  end;

                  if ssDouble in Shift then
                  begin
                    FDoubleClicked := True;
                  end;
                end;
              end;
          end;
        end;
    end;

    // for Undo/Redo
    if Assigned(FSelection) then
    begin
      if FSelectionCopy = nil then
      begin
        FSelectionCopy := TgmSelection.Create(imgWorkArea);
      end;

      FSelectionCopy.AssignAllSelectionData(FSelection);
    end
    else
    begin
      if Assigned(FSelectionCopy) then
      begin
        FreeAndNil(FSelectionCopy);
      end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.MarqueeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LNewPoint, LPoint : TPoint;
  LTranslateVector  : TPoint;
  LColor            : TColor;
  LChangeRect       : TRect;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
// Move mouse when mouse left button down 

  if FDrawing then
  begin
    case FMarqueeDrawingState of
      dsNewFigure:
        begin
          case frmMain.MarqueeTool of
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
              end;
          end;
        end;

      dsStretchCorner:
        begin
          if Assigned(FSelection) then
          begin
            { The border of the selection is always wider than the actual selection
              by the radius of the border handle at each side. }

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

                  FSelection.MaskBorderEnd   := Point(FSelection.MaskBorderEnd.X,
                                                      FYActual - SELECTION_HANDLE_RADIUS);
                end;

              dhBxAy:
                begin
                  FSelection.MaskBorderStart := Point(FSelection.MaskBorderStart.X,
                                                      FYActual + SELECTION_HANDLE_RADIUS);

                  FSelection.MaskBorderEnd   := Point(FXActual - SELECTION_HANDLE_RADIUS,
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

            FSelection.StandardizeOrder;
            FSelection.ResizeSelection;
            FSelection.GetMarchingAntsLines;

            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(FChannelManager.SelectedAlphaChannel) then
                  begin
                    FSelection.ShowSelection(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      [csGrayscale]);

                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  FSelection.ShowSelection(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    [csGrayscale]);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
                end;

              ctLayerMaskChannel:
                begin
                  FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap,
                                           [csGrayscale]);

                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      0, 0, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  LChangeRect := ImageClientRectToBitmapRect(imgWorkArea);
                  
                  FLayerList.SelectedLayer.Changed(LChangeRect);
                end;

              ctColorChannel:
                begin 
                  FSelection.ShowSelection(FLayerList.SelectedLayer.LayerBitmap,
                                           FChannelManager.SelectedColorChannels);

                  LChangeRect := ImageClientRectToBitmapRect(imgWorkArea);

                  FLayerList.SelectedLayer.Changed(LChangeRect);
                end;
            end;
          end;
        end;

      dsTranslate:
        begin
          if Assigned(FSelection) then
          begin
            // calculate the new position
            LNewPoint        := Point(FXActual, FYActual);
            LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);
            
            FSelection.TranslateSelection(LTranslateVector);

            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(FChannelManager.SelectedAlphaChannel) then
                  begin
                    FSelection.ShowSelection(
                      FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                      [csGrayscale]);

                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
                  end
                  else
                  begin
                    LChangeRect := ImageClientRectToBitmapRect(imgWorkArea);

                    FLayerList.SelectedLayer.Changed(LChangeRect);
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  FSelection.ShowSelection(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    [csGrayscale]);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
                end;

              ctLayerMaskChannel:
                begin
                  FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap,
                                           [csGrayscale]);

                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      0, 0, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  LChangeRect := ImageClientRectToBitmapRect(imgWorkArea);
                  
                  FLayerList.SelectedLayer.Changed(LChangeRect);
                end;

              ctColorChannel:
                begin
                  // must be on layer
                  if FLayerList.SelectedLayer is TgmNormalLayer then
                  begin
                    FSelection.ShowSelection(
                      FLayerList.SelectedLayer.LayerBitmap,
                      FChannelManager.SelectedColorChannels);
                  end;

                  LChangeRect := ImageClientRectToBitmapRect(imgWorkArea);
                  
                  FLayerList.SelectedLayer.Changed(LChangeRect);
                end;
            end;

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
        if (FChannelManager.CurrentChannelType in [ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel]) or
           (FLayerList.SelectedLayer is TgmNormalLayer) then
        begin
          FMarqueeDrawingHandle := FSelection.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);
        end
        else
        begin
          FMarqueeDrawingHandle := dhNone;
        end;

        if FMarqueeDrawingHandle in [dhNone,
                                     dhAxAy,
                                     dhBxBy,
                                     dhAxBy,
                                     dhBxAy,
                                     dhLeftHalfAyBy,
                                     dhRightHalfAyBy,
                                     dhTopHalfAxBx,
                                     dhBottomHalfAxBx] then
        begin
          Screen.Cursor := SetCursorByHandle(FMarqueeDrawingHandle);
        end;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing the color info of the pixel which is below the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  imgWorkArea.Canvas.Pen.Mode := pmCopy;
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.MarqueeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint       : TPoint;
  LMergedBmp   : TBitmap32;
  LCommand     : TgmCustomCommand;
  LIconResName : string;
  LCommandName : string;
begin
  LCommand := nil;

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

// Move mouse when mouse left button down 

  if FDrawing then
  begin
    FDrawing := False;
    
    imgWorkArea.Canvas.Pen.Mode := pmCopy;

    case FMarqueeDrawingState of
      dsNewFigure:
        begin
          // if the selection has not been created, then create one
          if FSelection = nil then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(FChannelManager.SelectedAlphaChannel) then
                  begin
                    FSelection := TgmSelection.Create(imgWorkArea,
                       FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
                  end
                  else
                  begin
                    // make the source bitmap of the selection be the same size as current layer
                    FSelection := TgmSelection.Create(imgWorkArea,
                      FLayerList.SelectedLayer.LayerBitmap);
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  FSelection := TgmSelection.Create(imgWorkArea,
                     FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
                end;

              ctLayerMaskChannel:
                begin
                  FSelection := TgmSelection.Create(imgWorkArea,
                    FLayerList.SelectedLayer.MaskBitmap);
                end;

              ctColorChannel:
                begin
                  FSelection := TgmSelection.Create(imgWorkArea,
                    FLayerList.SelectedLayer.LayerBitmap);
                end;
            end;
          end;

          if (FSelection.IsTranslated      = False) and
             (FSelection.IsCornerStretched = False) and
             (FSelection.IsHorizFlipped    = False) and
             (FSelection.IsVertFlipped     = False) then
          begin
            case frmMain.MarqueeTool of
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
                      FSelection.CreateCustomRGN(FRegion.Region, frmMain.MarqueeMode);
                    end;

                    FreeAndNil(FRegion);
                  end;
                end;

              mtMagicWand:
                begin
                  if (FXActual >= 0) and
                     (FYActual >= 0) and
                     (FXActual < FLayerList.SelectedLayer.LayerBitmap.Width) and
                     (FYActual < FLayerList.SelectedLayer.LayerBitmap.Height) then
                  begin
                    FSelection.MagicTolerance := frmMain.MagicWandTolerance / 100;

                    case FChannelManager.CurrentChannelType of
                      ctAlphaChannel:
                        begin
                          if Assigned(FChannelManager.SelectedAlphaChannel) then
                          begin
                            FSelection.CreateMagicWandMarqueeRGN(
                              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                              FXActual, FYActual,
                              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Pixel[FXActual, FYActual],
                              frmMain.MarqueeMode);
                          end;
                        end;

                      ctQuickMaskChannel:
                        begin
                          FSelection.CreateMagicWandMarqueeRGN(
                            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                            FXActual, FYActual,
                            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Pixel[FXActual, FYActual],
                            frmMain.MarqueeMode);
                        end;

                      ctLayerMaskChannel:
                        begin
                          FSelection.CreateMagicWandMarqueeRGN(
                            FLayerList.SelectedLayer.MaskBitmap,
                            FXActual, FYActual,
                            FLayerList.SelectedLayer.MaskBitmap.Pixel[FXActual, FYActual],
                            frmMain.MarqueeMode);
                        end;

                      ctColorChannel:
                        begin
                          // must be on layer
                          if frmMain.chckbxUseAllLayers.Checked then
                          begin
                            FSelection.CreateMagicWandMarqueeRGN(
                              FLayerList.CombineResult,
                              FXActual, FYActual,
                              FLayerList.CombineResult.Pixel[FXActual, FYActual],
                              frmMain.MarqueeMode);
                          end
                          else
                          begin
                            FSelection.CreateMagicWandMarqueeRGN(
                              FLayerList.SelectedLayer.LayerBitmap,
                              FXActual, FYActual,
                              FLayerList.SelectedLayer.LayerBitmap.Pixel[FXActual, FYActual],
                              frmMain.MarqueeMode);
                          end;
                        end;
                    end;
                  end;
                end;

              mtMagneticLasso:
                begin
                  if Assigned(FMagneticLasso) then
                  begin
                    if FDoubleClicked = False then
                    begin
                      FMagneticLasso.MouseUp(Button, Shift, FXActual, FYActual);

                      if Assigned(FMagneticLassoLayer) then
                      begin
                        FMagneticLassoLayer.Changed;
                      end;

                      if Assigned(FSelection) then
                      begin
                        FSelection.IsAnimated := True;
                      end;

                      Exit;
                    end
                    else
                    begin
                      FDoubleClicked := False; // restore the mark

                      // convert lasso to selection
                      if FMagneticLasso.IsConnected then
                      begin
                        FSelection.CreateCustomRGN(FMagneticLasso.CurveRegion,
                                                   frmMain.MarqueeMode);

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
            end;

            FSelection.Background.Assign(FSelection.SourceBitmap);
            FSelection.GetActualMaskBorder;
            FSelection.CutRegionFromOriginal;
            FSelection.GetForeground;
            FSelection.GetMarchingAntsLines;

            // update the background of the layer/mask
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              // must be on layer
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end
                else
                begin
                  if (csRed   in FChannelManager.SelectedColorChannels) and
                     (csGreen in FChannelManager.SelectedColorChannels) and
                     (csBlue  in FChannelManager.SelectedColorChannels) then
                  begin
                    FSelection.GetBackgroundWithTransparent;
                  end
                  else
                  begin
                    FSelection.GetBackgroundWithFilledColor(
                      Color32(frmMain.GlobalBackColor),
                      FChannelManager.SelectedColorChannels );
                  end;
                end;
              end;
            end
            else
            begin
              // alpha channel, layer mask channel or quick mask channel ...

              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.BackGrayColor), [csGrayscale]);
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

            ShowProcessedSelection();
            FSelection.IsAnimated := True;

            if Assigned(FSelectionCopy) then
            begin
              FreeAndNil(FSelectionCopy);
            end;

            Exit;
          end;

          // show selection
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                if Assigned(FChannelManager.SelectedAlphaChannel) then
                begin
                  FSelection.ShowSelection(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                    [csGrayscale]);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
                end;
              end;

            ctQuickMaskChannel:
              begin
                FSelection.ShowSelection(
                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                  [csGrayscale]);

                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
              end;

            ctLayerMaskChannel:
              begin
                FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap,
                                         [csGrayscale]);

                FLayerList.SelectedLayer.Changed;
              end;

            ctColorChannel:
              begin
                // mast be on layer
                if FLayerList.SelectedLayer is TgmNormalLayer then
                begin
                  FSelection.ShowSelection(
                    FLayerList.SelectedLayer.LayerBitmap,
                    FChannelManager.SelectedColorChannels);

                  FLayerList.SelectedLayer.Changed();
                end;
              end;
          end;

          // if the selection shadow is not exists, then delete the selection
          if FSelection.HasShadow = False then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(FChannelManager.SelectedAlphaChannel) then
                  begin
                    with FChannelManager.SelectedAlphaChannel do
                    begin
                      ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                      ChannelLayer.Bitmap.Changed;
                    end;
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  with FChannelManager.QuickMaskChannel do
                  begin
                    ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                    ChannelLayer.Bitmap.Changed;
                  end;
                end;

              ctLayerMaskChannel:
                begin
                  FLayerList.SelectedLayer.MaskBitmap.Assign(FSelection.SourceBitmap);
                  FLayerList.SelectedLayer.Changed;
                end;

              ctColorChannel:
                begin
                  // must be on layer
                  if FLayerList.SelectedLayer is TgmNormalLayer then
                  begin
                    FLayerList.SelectedLayer.LayerBitmap.Assign(FSelection.SourceBitmap);
                    FLayerList.SelectedLayer.Changed();
                  end;
                end;
            end;

            FreeAndNil(FSelection);
            frmMain.spdbtnCommitSelection.Enabled := False;
            frmMain.spdbtnDeselect.Enabled        := False;
            frmMain.spdbtnDeleteSelection.Enabled := False;
          end
          else
          begin
            frmMain.spdbtnCommitSelection.Enabled := True;
            frmMain.spdbtnDeselect.Enabled        := True;
            frmMain.spdbtnDeleteSelection.Enabled := True;

            // Undo/Redo
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  // Undo/Redo
                  LCommandName := GetCommandNameForMarqueeTools(frmMain.MarqueeTool);

                  LCommand := TgmNewSelectionOnAlphaChannelCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FSelectionCopy, FSelection,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    GetSelectionForUndoRedo,
                    DeleteSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  // Undo/Redo
                  LCommandName := GetCommandNameForMarqueeTools(frmMain.MarqueeTool);

                  LCommand := TgmNewSelectionOnQuickMaskChannelCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo,
                    DeleteSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  // Undo/Redo
                  LCommandName := GetCommandNameForMarqueeTools(frmMain.MarqueeTool);

                  LCommand := TgmNewSelectionOnLayerMaskCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FLayerList,
                    FSelectionCopy, FSelection,
                    FLayerList.SelectedIndex,
                    GetSelectionForUndoRedo,
                    DeleteSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  // Undo/Redo
                  LCommandName := GetCommandNameForMarqueeTools(frmMain.MarqueeTool);

                  LCommand := TgmNewSelectionOnLayerCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FLayerList,
                    FSelectionCopy, FSelection,
                    FLayerList.SelectedIndex,
                    GetSelectionForUndoRedo,
                    DeleteSelectionForUndoRedo);
                end;
            end;
          end;

          ChangeImageCursorByMarqueeTools();

          // Undo/Redo
          if Assigned(LCommand) then
          begin
            LIconResName := GetCommandIconResourceNameForMarqueeTools(frmMain.MarqueeTool);
            if LIconResName <> '' then
            begin
              LCommand.ChangeCommandIconByResourceName(LIconResName);
            end;

            FCommandManager.AddCommand(LCommand);
          end;
        end;

      dsStretchCorner,
      dsTranslate:
        begin
          Screen.Cursor := crDefault;
          ChangeImageCursorByMarqueeTools();

          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                if Assigned(FChannelManager.SelectedAlphaChannel) then
                begin
                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
                end;
              end;

            ctQuickMaskChannel:
              begin
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
              end;

            ctLayerMaskChannel,
            ctColorChannel:
              begin
                FLayerList.SelectedLayer.Changed();
              end;
          end;

          // Undo/Redo
          if FMarqueeDrawingState = dsTranslate then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
                    satTranslate, FChannelManager,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
                    satTranslate, FChannelManager, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
                    satTranslate, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
                    satTranslate, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;
            end;

            LCommand.ChangeCommandIconByResourceName(
              gmHistoryCommands.MOVE_COMMAND_ICON_RES_NAME);
          end
          else if FMarqueeDrawingState = dsStretchCorner then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
                    satStretchCorner, FChannelManager,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
                    satStretchCorner, FChannelManager, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
                    satStretchCorner, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
                    satStretchCorner, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;
            end;
          end;

          if Assigned(LCommand) then
          begin
            FCommandManager.AddCommand(LCommand);
          end;

          // restore the stae to dsNotDrawing
          FMarqueeDrawingState := dsNotDrawing;
        end;
    end;

    if Assigned(FSelection) and
       (FSelection.MarchingAntsLineList.Count > 0) then
    begin
      FLayerList.SelectedLayer.Changed;
      imgWorkArea.Update;
      FSelection.IsAnimated := True;
    end;

    // update thumbnails
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            FChannelManager.SelectedAlphaChannel.UpdateChannelThumbnail;
          end;
        end;

      ctQuickMaskChannel:
        begin
          FChannelManager.QuickMaskChannel.UpdateChannelThumbnail;
        end;
        
      ctLayerMaskChannel:
        begin
          FLayerList.SelectedLayer.UpdateMaskThumbnail;

          // update the mask channel preview layer
          FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
            0, 0, FLayerList.SelectedLayer.MaskBitmap);
            
          FChannelManager.LayerMaskChannel.UpdateChannelThumbnail;
        end;

      ctColorChannel:
        begin
          if FLayerList.SelectedLayer is TgmNormalLayer then
          begin
            FLayerList.SelectedLayer.UpdateLayerThumbnail;
            
            // We don't have to update the color thumbnails in channel manager,
            // because those thumbnails would be updated when callback function
            // AfterLayerCombined() be invoked.
          end;
        end;
    end;

    if Assigned(FSelectionCopy) then
    begin
      FreeAndNil(FSelectionCopy);
    end;
  end;
end;

{ Custom procedures and functions }

function NormalizeRect(const ARectangle: TRect): TRect;
begin
  // This routine normalizes a rectangle by making sure that the (Left, Top)
  // coordinates are always above and to the left of the (Bottom, Right)
  // coordiantes.
  with ARectangle do
  begin
    if Left > Right then
    begin
      if Top > Bottom then
      begin
        Result := Rect(Right, Bottom, Left, Top);
      end
      else
      begin
        Result := Rect(Right, Top, Left, Bottom);
      end;
    end
    else
    begin
      if Top > Bottom then
      begin
        Result := Rect(Left, Bottom, Right, Top);
      end
      else
      begin
        Result := Rect(Left, Top, Right, Bottom);
      end;
    end;
  end
end;

procedure TfrmChild.SetEditMode(const Value: TgmEditMode);
var
  LMaskBmp : TBitmap32;
begin
  LMaskBmp := nil;

  if FEditMode <> Value then
  begin
    FEditMode := Value;

    case FEditMode of
      emStandardMode:
        begin
          LoadQuickMaskAsSelection();
          FChannelManager.DeleteQuickMaskChannel();
          ChangeSelectionTarget();
        end;

      emQuickMaskMode:
        begin
          if Assigned(FSelection) then
          begin
            FSelection.UpdateOriginalMaskWithResizedMask();

            // make a copy of the original mask of the selection for later use
            LMaskBmp := TBitmap32.Create();
            LMaskBmp.SetSizeFrom(FSelection.OriginalMask);
            CopyBitmap32(LMaskBmp, FSelection.OriginalMask);
            ReplaceAlphaChannelWithNewValue(LMaskBmp, 255);

            // Committing the selection before creating the quick mask to
            // avoid selection target be changed to the newly created
            // quick mask channel.
            CommitSelection();
          end;

          if not Assigned(FChannelManager.QuickMaskChannel) then
          begin
            FChannelManager.CreateQuickMaskChannel(
              imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);
          end;

          if Assigned(LMaskBmp) then
          begin
            with FChannelManager.QuickMaskChannel do
            begin
              CopyBitmap32(ChannelLayer.Bitmap, LMaskBmp);
              ChannelLayer.Bitmap.Changed();
              UpdateChannelThumbnail();
            end;

            LMaskBmp.Free();
          end;
        end;
    end;
  end;
end;

// callback function for OnPixelCombine event of FPathLayer.Bitmap
procedure TfrmChild.PathLayerBlend(F: TColor32; var B: TColor32;
  M: TColor32);
var
  LBlendColor : TColor32;
begin
  if F <> $00000000 then
  begin
    LBlendColor := not ($FF000000 xor B);
    B           := LBlendColor or $FF000000;
  end;
end;

procedure TfrmChild.MagneticLassoLayerBlend(F: TColor32; var B: TColor32;
  M: TColor32);
var
  LBlendColor : TColor32;
begin
  if F <> $00000000 then
  begin
    LBlendColor := not ($FF7F7F7F xor B);
    B           := LBlendColor or $FF000000;
  end;
end;

procedure TfrmChild.UpdateMainFormStatusBarWhenMouseDown;
begin
  frmMain.stsbrMain.Panels[0].Text := GetBitmapDimensionString(
    FLayerList.SelectedLayer.LayerBitmap);

  frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

// adapted from RebuildBrush() by Zoltan in gr32PaintDemo3 demo program
procedure TfrmChild.BuildPencilStroke(ADest: TBitmap32);
var
  LPenWidth            : Integer;
  LBackBmp, LStrokeBmp : TBitmap32;
begin
  LPenWidth := frmMain.GlobalPenWidth - 1;

  LBackBmp   := TBitmap32.Create;
  LStrokeBmp := TBitmap32.Create;
  try
    LBackBmp.SetSize(LPenWidth * 2 + 1, LPenWidth * 2 + 1);
    LBackBmp.Clear(clWhite32);

    LStrokeBmp.SetSize(LBackBmp.Width, LBackBmp.Height);
    LStrokeBmp.DrawMode    := dmBlend;
    LStrokeBmp.CombineMode := cmMerge;
    LStrokeBmp.Clear(clBlack32);

    FeatheredCircleAlpha(LStrokeBmp, LStrokeBmp.Width div 2,
                         LStrokeBmp.Height div 2,
                         frmMain.GlobalPenWidth div 2, 5);

    LStrokeBmp.DrawTo(LBackBmp);
    ADest.Assign(LBackBmp);
  finally
    LStrokeBmp.Free;
    LBackBmp.Free;
  end;
end;

// adapted from BrushLine() by Zoltan in gr32PaintDemo3 demo program
procedure TfrmChild.PencilLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
  ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);
var
  a,b        : Integer;  // displacements in x and y
  d          : Integer;  // decision variable
  diag_inc   : Integer;  // d's increment for diagonal steps
  dx_diag    : Integer;  // diagonal x step for next pixel
  dx_nondiag : Integer;  // nondiagonal x step for next pixel
  dy_diag    : Integer;  // diagonal y step for next pixel
  dy_nondiag : Integer;  // nondiagonal y step for next pixel
  i          : Integer;  // loop index
  nondiag_inc: Integer;  // d's increment for nondiagonal steps
  swap       : Integer;  // temporary variable for swap
  x,y        : Integer;  // current x and y coordinates
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
  else dx_diag := 1;

  // Determine whether end point lies above or below start point.
  if b < 0 then               // drawing towards smaller y values?
  begin
    b       := -b;            // make 'b' positive
    dy_diag := -1
  end
  else dy_diag := 1;

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
      frmMain.Pencil.Paint(tobitmap, x, y, ChannelSet);
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

procedure TfrmChild.PencilLineOnMask(
  const xStart, yStart, xEnd, yEnd, distance: Integer;
  const ChannelSet: TgmChannelSet);
var
  a,b        : Integer;  // displacements in x and y
  d          : Integer;  // decision variable
  diag_inc   : Integer;  // d's increment for diagonal steps
  dx_diag    : Integer;  // diagonal x step for next pixel
  dx_nondiag : Integer;  // nondiagonal x step for next pixel
  dy_diag    : Integer;  // diagonal y step for next pixel
  dy_nondiag : Integer;  // nondiagonal y step for next pixel
  i          : Integer;  // loop index
  nondiag_inc: Integer;  // d's increment for nondiagonal steps
  swap       : Integer;  // temporary variable for swap
  x,y        : Integer;  // current x and y coordinates
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
      frmMain.Pencil.Paint(FLayerList.SelectedLayer.MaskBitmap, x, y, ChannelSet);

      // paint on mask channel preview layer, too
      if Assigned(FChannelManager.LayerMaskChannel) then
      begin
        frmMain.Pencil.Paint(
          FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
          x, y, ChannelSet);
      end;

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

procedure TfrmChild.ProcessFigureMouseUpOnLayer(
  const AShiftState: TShiftState);
var
  LIndex       : Integer;
  LVectorLayer : TgmVectorLayer;
  LCommand     : TgmCustomCommand;
  LOnNewLayer  : Boolean;
begin
  case frmMain.StandardTool of
    gstStraightLine,
    gstRegularPolygon,
    gstRectangle,
    gstRoundRectangle,
    gstEllipse:
      begin
        // calculating coordinates of figures
        case frmMain.StandardTool of
          gstStraightLine,
          gstRegularPolygon:
            begin
              FActualEndPoint := Point(FXActual, FYActual);
            end;

          gstRectangle,
          gstRoundRectangle,
          gstEllipse: 
            begin
              if ssShift in AShiftState then
              begin
                FActualEndPoint := CalculateRegularFigureEndPoint(
                  FActualStartPoint, Point(FXActual, FYActual) );
              end
              else
              begin
                FActualEndPoint := Point(FXActual, FYActual);
              end;
            end;
        end;

        // create a new vector layer or switch to a vector layer,
        // and then add figure to list
        if not (FLayerList.SelectedLayer is TgmVectorLayer) then
        begin
          LIndex := FLayerList.SelectedIndex;

          if (LIndex < FLayerList.Count - 1) and
             (FLayerList.Layers[LIndex + 1] is TgmVectorLayer ) then
          begin
            FLayerList.SelectLayer(LIndex + 1);
            LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);
          end
          else
          begin
            LVectorLayer := TgmVectorLayer( CreateVectorLayer() );
            FLayerList.Insert(LIndex + 1, LVectorLayer);
          end;

          // for Undo/Redo
          LOnNewLayer := True;
        end
        else
        begin
          LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);

          // for Undo/Redo
          LOnNewLayer := False;
        end;

        // add figure to current active figure layer
        case frmMain.StandardTool of
          gstStraightLine:
            begin
              LVectorLayer.FigureList.AddStraightLineToList(
                frmMain.GlobalForeColor,
                frmMain.GlobalBackColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                frmMain.GlobalPenWidth,
                FActualStartPoint,
                FActualEndPoint);
            end;

          gstRectangle:
            begin
              PointStandardizeOrder(FActualStartPoint, FActualEndPoint);

              if ssShift in AShiftState then
              begin
                LVectorLayer.FigureList.AddRectangleToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  REGULAR_FIGURE);
              end
              else
              begin
                LVectorLayer.FigureList.AddRectangleToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  IRREGULAR_FIGURE);
              end;
            end;

          gstRoundRectangle:
            begin
              PointStandardizeOrder(FActualStartPoint, FActualEndPoint);

              if ssShift in AShiftState then
              begin
                LVectorLayer.FigureList.AddRoundRectangleToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  frmMain.StandardCornerRadius,
                  REGULAR_FIGURE);
              end
              else
              begin
                LVectorLayer.FigureList.AddRoundRectangleToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  frmMain.StandardCornerRadius,
                  IRREGULAR_FIGURE);
              end;
            end;

          gstEllipse:
            begin
              PointStandardizeOrder(FActualStartPoint, FActualEndPoint);
              
              if ssShift in AShiftState then
              begin
                LVectorLayer.FigureList.AddEllipseToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  REGULAR_FIGURE);
              end
              else
              begin
                LVectorLayer.FigureList.AddEllipseToList(
                  frmMain.GlobalForeColor,
                  frmMain.GlobalBackColor,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  frmMain.GlobalPenWidth,
                  FActualStartPoint,
                  FActualEndPoint,
                  IRREGULAR_FIGURE);
              end;
            end;
            
          gstRegularPolygon:
            begin
              LVectorLayer.FigureList.AddRegularPolygonToList(
                frmMain.GlobalForeColor,
                frmMain.GlobalBackColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                frmMain.GlobalPenWidth,
                frmMain.StandardPolygonSides,
                FActualStartPoint,
                FActualEndPoint);
            end;
        end;

        LVectorLayer.DrawAllFiguresOnLayer();
        LVectorLayer.Changed();
        LVectorLayer.UpdateLayerThumbnail();

        if frmMain.StandardTool in [gstStraightLine,
                                    gstRectangle,
                                    gstRoundRectangle,
                                    gstEllipse,
                                    gstRegularPolygon] then
        begin
          // Undo/Redo
          if LOnNewLayer then
          begin
            LCommand := TgmNewFigureOnNewVectorLayerCommand.Create(
              FChannelManager, FLayerList, LVectorLayer, FLayerList.SelectedIndex);
          end
          else
          begin
            LCommand := TgmNewFigureOnExistedVectorLayerCommand.Create(
              FChannelManager, FLayerList, FLayerList.SelectedIndex,
              LVectorLayer.FigureList.Count - 1);
          end;

          if Assigned(LCommand) then
          begin
            FCommandManager.AddCommand(LCommand);
          end;
        end;
      end;

    gstBezierCurve:
      begin
        if FDrawCurveTime = 1 then
        begin
          FDrawCurveTime := 2;
        end
        else if FDrawCurveTime = 2 then
        begin
          FDrawCurveTime := 3;
        end
        else if FDrawCurveTime = 3 then
        begin
          FDrawCurveTime := 0;
          FinishCurveOnLayer();
        end;
      end;

    gstPolygon:
      begin
        // do nothing for now
      end;
  end;
end;

procedure TfrmChild.ProcessFigureMouseUpOnSelection(
  const ShiftState: TShiftState);
var
  LProcessedMask      : TBitmap32;
  LBitmapCopy         : TBitmap32;
  LFigureFlag         : TgmFigureFlags;  // for Undo/Redo
  LCommand            : TgmCustomCommand;
  LCommandIconResName : string;
  LCommandName        : string;
begin
  LCommand    := nil;
  LFigureFlag := ffNone;

  if Assigned(FSelection) then
  begin
    case frmMain.StandardTool of
      gstStraightLine,
      gstRegularPolygon,
      gstRectangle,
      gstRoundRectangle,
      gstEllipse:
        begin
          // save bitmap for Undo/Redo commands
          frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);

          // used for tracking the processed part on a canvas
          LProcessedMask := TBitmap32.Create;
          LBitmapCopy    := TBitmap32.Create;
          try
            LBitmapCopy.SetSizeFrom(FSelection.CutOriginal);
            LBitmapCopy.Assign(FSelection.CutOriginal);

            LProcessedMask.SetSizeFrom(FSelection.CutOriginal);
            LProcessedMask.Clear(clBlack32);

            with LProcessedMask.Canvas do
            begin
              Pen.Color := clWhite;
              Pen.Width := frmMain.GlobalPenWidth;

              if frmMain.GlobalBrushStyle = bsSolid then
              begin
                Pen.Style := psSolid;
              end
              else
              begin
                Pen.Style := frmMain.GlobalPenStyle;
              end;

              Brush.Color := clWhite;
              Brush.Style := frmMain.GlobalBrushStyle;
            end;

            // Don't know why we need to set the brush style for
            // CutOriginal.Canvas again. Otherwise, the bsClear brush style
            // setting will not affect the selection.
            FSelection.CutOriginal.Canvas.Brush.Style := frmMain.GlobalBrushStyle;

            // calculating the coordinates of figures
            if frmMain.StandardTool in [gstStraightLine,
                                        gstRegularPolygon] then
            begin
              FActualEndPoint := Point(FMarqueeX, FMarqueeY);
            end
            else
            if frmMain.StandardTool in [gstRectangle,
                                        gstRoundRectangle,
                                        gstEllipse] then
            begin
              if ssShift in ShiftState then
              begin
                FActualEndPoint := CalculateRegularFigureEndPoint(
                  FActualStartPoint, Point(FMarqueeX, FMarqueeY) );
              end
              else
              begin
                FActualEndPoint := Point(FMarqueeX, FMarqueeY);
              end;
            end;

            // draw figure on selection
            if frmMain.StandardTool = gstStraightLine then
            begin
              DrawStraightLine(FSelection.CutOriginal.Canvas,
                               FActualStartPoint, FActualEndPoint, pmCopy);

              DrawStraightLine(LProcessedMask.Canvas,
                               FActualStartPoint, FActualEndPoint, pmCopy);

              // for Undo/Redo
              LFigureFlag         := ffStraightLine;
              LCommandIconResName := gmVectorLayerCommands.STRAIGHT_LINE_ICON_RES_NAME;
            end
            else if frmMain.StandardTool = gstRectangle then
            begin
              DrawRectangle(FSelection.CutOriginal.Canvas,
                            FActualStartPoint, FActualEndPoint, pmCopy);

              DrawRectangle(LProcessedMask.Canvas,
                            FActualStartPoint, FActualEndPoint, pmCopy);

              // for Undo/Redo
              if ssShift in ShiftState then
              begin
                LFigureFlag := ffSquare;
              end
              else
              begin
                LFigureFlag := ffRectangle;
              end;

              LCommandIconResName := gmVectorLayerCommands.RECTANGLE_ICON_RES_NAME;
            end
            else if frmMain.StandardTool = gstRoundRectangle then
            begin
              DrawRoundRect(FSelection.CutOriginal.Canvas,
                            FActualStartPoint, FActualEndPoint,
                            frmMain.StandardCornerRadius, pmCopy);

              DrawRoundRect(LProcessedMask.Canvas,
                            FActualStartPoint, FActualEndPoint,
                            frmMain.StandardCornerRadius, pmCopy);

              // for Undo/Redo
              if ssShift in ShiftState then
              begin
                LFigureFlag := ffRoundSquare;
              end
              else
              begin
                LFigureFlag := ffRoundRectangle;
              end;

              LCommandIconResName := gmVectorLayerCommands.ROUND_RECT_ICON_RES_NAME;
            end
            else if frmMain.StandardTool = gstEllipse then
            begin
              DrawEllipse(FSelection.CutOriginal.Canvas,
                          FActualStartPoint, FActualEndPoint, pmCopy);

              DrawEllipse(LProcessedMask.Canvas,
                          FActualStartPoint, FActualEndPoint, pmCopy);

              // for Undo/Redo
              if ssShift in ShiftState then
              begin
                LFigureFlag := ffCircle;
              end
              else
              begin
                LFigureFlag := ffEllipse;
              end;

              LCommandIconResName := gmVectorLayerCommands.ELLIPSE_ICON_RES_NAME;
            end
            else if frmMain.StandardTool = gstRegularPolygon then
            begin
              DrawRegularPolygon(FSelection.CutOriginal.Canvas,
                                 FActualStartPoint, FActualEndPoint,
                                 frmMain.StandardPolygonSides,
                                 pmCopy, FILL_INSIDE);

              DrawRegularPolygon(LProcessedMask.Canvas,
                                 FActualStartPoint, FActualEndPoint,
                                 frmMain.StandardPolygonSides,
                                 pmCopy, FILL_INSIDE);

              // for Undo/Redo
              LFigureFlag         := ffRegularPolygon;
              LCommandIconResName := gmVectorLayerCommands.REGULAR_POLYGON_ICON_RES_NAME;
            end;

            if FLayerList.SelectedLayer.IsLockTransparency then
            begin
              case FChannelManager.CurrentChannelType of
                ctColorChannel:
                  begin
                    ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
                  end;

                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    ReplaceAlphaChannelWithNewValue(FSelection.CutOriginal, 255);
                  end;
              end;
            end
            else
            begin
              if (FChannelManager.CurrentChannelType = ctColorChannel) and
                 (FChannelManager.ColorChannelList.SelectedChannelCount < 3) then
              begin
                ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
              end
              else
              begin
                MakeCanvasProcessedOpaque(FSelection.CutOriginal, LProcessedMask);
              end;
            end;

            // adjust RGB channels of layer...
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              ReplaceRGBChannels(LBitmapCopy,
                                 FSelection.CutOriginal,
                                 LProcessedMask,
                                 FChannelManager.SelectedColorChannels,
                                 crsRemainDest);
            end;

            ShowProcessedSelection();
            UpdateThumbnailsBySelectedChannel();
          finally
            LProcessedMask.Free();
            LBitmapCopy.Free();
          end;

          // Undo/Redo
          LCommandName := gmFigures.GetFigureName(LFigureFlag);
          frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                  LCommandName,
                  FChannelManager,
                  FChannelManager.AlphaChannelList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                  LCommandName,
                  FChannelManager,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                  LCommandName,
                  FChannelManager,
                  FLayerList,
                  FLayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                  LCommandName,
                  FChannelManager,
                  FLayerList,
                  FLayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;
          end;

          if Assigned(LCommand) then
          begin
            LCommand.ChangeCommandIconByResourceName(LCommandIconResName);
            FCommandManager.AddCommand(LCommand);
          end;
        end;

      gstBezierCurve:
        begin
          if FDrawCurveTime = 1 then
          begin
            FDrawCurveTime := 2;
          end
          else if FDrawCurveTime = 2 then
          begin
            FDrawCurveTime := 3;
          end
          else if FDrawCurveTime = 3 then
          begin
            FDrawCurveTime := 0;
            FinishCurveOnSelection();
          end;
        end;

      gstPolygon:
        begin
          // do nothing 
        end;
    end;
  end;
end;

procedure TfrmChild.ProcessFigureMouseUpOnSpecialChannels(
  const ShiftState: TShiftState);
var
  LDestBmp            : TBitmap32;
  LFigureFlag         : TgmFigureFlags;  // for Undo/Redo
  LCommandIconResName : string;
  LCommandName        : string;
  LCommand            : TgmCustomCommand;
begin
  LDestBmp    := nil;
  LCommand    := nil;
  LFigureFlag := ffNone;

  case frmMain.StandardTool of
    gstStraightLine,
    gstRegularPolygon,
    gstRectangle,
    gstRoundRectangle,
    gstEllipse:
      begin
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(FChannelManager.SelectedAlphaChannel) then
              begin
                frmMain.FBitmapBefore.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);  // for Undo/Redo
                LDestBmp := FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap;
              end;
            end;

          ctQuickMaskChannel:
            begin
              if Assigned(FChannelManager.QuickMaskChannel) then
              begin
                frmMain.FBitmapBefore.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);  // for Undo/Redo
                LDestBmp := FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap;
              end;
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);  // for Undo/Redo
              LDestBmp := FLayerList.SelectedLayer.MaskBitmap;
            end;
        end;

        // calculating the coordinates of figures
        if frmMain.StandardTool in [gstStraightLine,
                                    gstRegularPolygon] then
        begin
          FActualEndPoint := Point(FXActual, FYActual);
        end
        else
        if frmMain.StandardTool in [gstRectangle,
                                    gstRoundRectangle,
                                    gstEllipse] then
        begin
          if ssShift in ShiftState then
          begin
            FActualEndPoint := CalculateRegularFigureEndPoint(
              FActualStartPoint, Point(FXActual, FYActual) );
          end
          else
          begin
            FActualEndPoint := Point(FXActual, FYActual);
          end;
        end;

        if Assigned(LDestBmp) then
        begin
          if frmMain.StandardTool = gstStraightLine then
          begin
            DrawStraightLine(LDestBmp.Canvas, FActualStartPoint, FActualEndPoint, pmCopy);

            // for Undo/Redo
            LFigureFlag         := ffStraightLine;
            LCommandIconResName := gmVectorLayerCommands.STRAIGHT_LINE_ICON_RES_NAME;
          end
          else if frmMain.StandardTool = gstRectangle then
          begin
            DrawRectangle(LDestBmp.Canvas, FActualStartPoint, FActualEndPoint, pmCopy);

            // for Undo/Redo
            if ssShift in ShiftState then
            begin
              LFigureFlag := ffSquare;
            end
            else
            begin
              LFigureFlag := ffRectangle;
            end;

            LCommandIconResName := gmVectorLayerCommands.RECTANGLE_ICON_RES_NAME;
          end
          else if frmMain.StandardTool = gstRoundRectangle then
          begin
            DrawRoundRect(LDestBmp.Canvas,
                          FActualStartPoint, FActualEndPoint,
                          frmMain.StandardCornerRadius,
                          pmCopy);

            // for Undo/Redo
            if ssShift in ShiftState then
            begin
              LFigureFlag := ffRoundSquare;
            end
            else
            begin
              LFigureFlag := ffRoundRectangle;
            end;

            LCommandIconResName := gmVectorLayerCommands.ROUND_RECT_ICON_RES_NAME;
          end
          else if frmMain.StandardTool = gstEllipse then
          begin
            DrawEllipse(LDestBmp.Canvas, FActualStartPoint, FActualEndPoint, pmCopy);

            // for Undo/Redo
            if ssShift in ShiftState then
            begin
              LFigureFlag := ffCircle;
            end
            else
            begin
              LFigureFlag := ffEllipse;
            end;

            LCommandIconResName := gmVectorLayerCommands.ELLIPSE_ICON_RES_NAME;
          end
          else if frmMain.StandardTool = gstRegularPolygon then
          begin
            DrawRegularPolygon(LDestBmp.Canvas,
                               FActualStartPoint, FActualEndPoint,
                               frmMain.StandardPolygonSides,
                               pmCopy,
                               FILL_INSIDE);

            // for Undo/Redo
            LFigureFlag         := ffRegularPolygon;
            LCommandIconResName := gmVectorLayerCommands.REGULAR_POLYGON_ICON_RES_NAME;
          end;

          // Restore the alpha channels to opaque after applied canvas
          // operations on the mask. Otherwise, if you create selection
          // on mask, you will get a mask selection with transparent area, 
          // which is bad.
          ReplaceAlphaChannelWithNewValue(LDestBmp, 255);

          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;

            ctQuickMaskChannel:
              begin
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
              end;

            ctLayerMaskChannel:
              begin
                if Assigned(FChannelManager.LayerMaskChannel) then
                begin
                  FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                    0, 0, FLayerList.SelectedLayer.MaskBitmap);
                end;

                FLayerList.SelectedLayer.Changed();
              end;
          end;

          // update thumbnails
          UpdateThumbnailsBySelectedChannel();

          // Undo/Redo
          LCommandName := gmFigures.GetFigureName(LFigureFlag);
          
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                frmMain.FBitmapAfter.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                LCommand := TgmAlphaChannelProcessCommand.Create(
                  LCommandName,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  FChannelManager.AlphaChannelList,
                  FChannelManager.AlphaChannelList.SelectedIndex);
              end;

            ctQuickMaskChannel:
              begin
                frmMain.FBitmapAfter.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                LCommand := TgmQuickMaskChannelProcessCommand.Create(
                  LCommandName,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  FChannelManager);
              end;

            ctLayerMaskChannel:
              begin
                frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.MaskBitmap);

                LCommand := TgmLayerMaskProcessCommand.Create(
                  LCommandName,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  FLayerList,
                  FLayerList.SelectedIndex);
              end;
          end;

          if Assigned(LCommand) then
          begin
            LCommand.ChangeCommandIconByResourceName(LCommandIconResName);
            FCommandManager.AddCommand(LCommand);
          end;
        end;
      end;

    gstBezierCurve:
      begin
        if FDrawCurveTime = 1 then
        begin
          FDrawCurveTime := 2;
        end
        else if FDrawCurveTime = 2 then
        begin
          FDrawCurveTime := 3;
        end
        else if FDrawCurveTime = 3 then
        begin
          FDrawCurveTime := 0;
          FinishCurveOnSpecialChannels();
        end;
      end;

    gstPolygon:
      begin
        // do nothing
      end;
  end;
end;

procedure TfrmChild.FinishCurveOnLayer;
var
  LIndex       : Integer;
  LVectorLayer : TgmVectorLayer;
  LCommand     : TgmCustomCommand;
  LOnNewLayer  : Boolean;
begin
  // for Undo/Redo
  LOnNewLayer := False;

  // create a new vector layer or switch to a vector layer,
  // and then add figure to list
  if not (FLayerList.SelectedLayer is TgmVectorLayer) then
  begin
    LIndex := FLayerList.SelectedIndex;

    if (LIndex < FLayerList.Count - 1) and
       (FLayerList.Layers[LIndex + 1] is TgmVectorLayer ) then
    begin
      FLayerList.SelectLayer(LIndex + 1);
      LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);
    end
    else
    begin
      LVectorLayer := TgmVectorLayer( CreateVectorLayer() );
      FLayerList.Insert(LIndex + 1, LVectorLayer);

      // for Undo/Redo
      LOnNewLayer := True;
    end;
  end
  else
  begin
    LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);
  end;

  LVectorLayer.FigureList.AddCurveToList(
    frmMain.GlobalForeColor,
    frmMain.GlobalBackColor,
    frmMain.GlobalPenStyle,
    frmMain.GlobalBrushStyle,
    frmMain.GlobalPenWidth,
    FActualStartPoint,
    FActualCurvePoint1,
    FActualCurvePoint2,
    FActualEndPoint);

  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.Changed();
  LVectorLayer.UpdateLayerThumbnail();

  // Undo/Redo
  if LOnNewLayer then
  begin
    LCommand := TgmNewFigureOnNewVectorLayerCommand.Create(
      FChannelManager, FLayerList, LVectorLayer, FLayerList.SelectedIndex);
  end
  else
  begin
    LCommand := TgmNewFigureOnExistedVectorLayerCommand.Create(
      FChannelManager, FLayerList, FLayerList.SelectedIndex,
      LVectorLayer.FigureList.Count - 1);
  end;

  if Assigned(LCommand) then
  begin
    FCommandManager.AddCommand(LCommand);
  end;
end; 

procedure TfrmChild.FinishCurveOnSpecialChannels;
var
  LDestBmp     : TBitmap32; // a bitmap32 pointer
  LCommand     : TgmCustomCommand;
  LCommandName : string;
begin
  LDestBmp := nil;
  LCommand := nil;

  case FChannelManager.CurrentChannelType of
    ctAlphaChannel:
      begin
        if Assigned(FChannelManager.SelectedAlphaChannel) then
        begin
          frmMain.FBitmapBefore.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);  // for Undo/Redo
          LDestBmp := FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap;
        end;
      end;

    ctQuickMaskChannel:
      begin
        if Assigned(FChannelManager.QuickMaskChannel) then
        begin
          frmMain.FBitmapBefore.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);  // for Undo/Redo
          LDestBmp := FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap;
        end;
      end;

    ctLayerMaskChannel:
      begin
        frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);  // for Undo/Redo
        LDestBmp := FLayerList.SelectedLayer.MaskBitmap;
      end;
  end;

  if Assigned(LDestBmp) then
  begin
    DrawPolyBezier(LDestBmp.Canvas,
                   [ FActualStartPoint, FActualCurvePoint1,
                     FActualCurvePoint2, FActualEndPoint ],
                   pmCopy);

    // Restore the alpha channels to opaque after applied canvas operations on
    // the mask. Otherwise, if you create selection on mask, you will get a
    // mask selection with transparent area, which is bad.
    ReplaceAlphaChannelWithNewValue(LDestBmp, 255);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
        end;

      ctQuickMaskChannel:
        begin
          FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
        end;

      ctLayerMaskChannel:
        begin
          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              0, 0, FLayerList.SelectedLayer.MaskBitmap);
          end;

          FLayerList.SelectedLayer.Changed();
        end;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    LCommandName := gmFigures.GetFigureName(ffCurve);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          frmMain.FBitmapAfter.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

          LCommand := TgmAlphaChannelProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FChannelManager.AlphaChannelList,
            FChannelManager.AlphaChannelList.SelectedIndex);
        end;

      ctQuickMaskChannel:
        begin
          frmMain.FBitmapAfter.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

          LCommand := TgmQuickMaskChannelProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FChannelManager);
        end;

      ctLayerMaskChannel:
        begin
          frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.MaskBitmap);

          LCommand := TgmLayerMaskProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FLayerList,
            FLayerList.SelectedIndex);
        end;
    end;

    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(gmVectorLayerCommands.BEZIER_CURVE_ICON_RES_NAME);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.FinishCurveOnSelection;
var
  LProcessedMask : TBitmap32;
  LBitmapCopy    : TBitmap32;
  LCommand       : TgmCustomCommand;
  LCommandName   : string;
begin
  LCommand := nil;

  if Assigned(FSelection) then
  begin
    // for Undo/Redo
    frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);

    // used for tracking the processed part on a canvas
    LProcessedMask := TBitmap32.Create();
    LBitmapCopy    := TBitmap32.Create();
    try
      LBitmapCopy.Assign(FSelection.CutOriginal);

      LProcessedMask.SetSizeFrom(FSelection.CutOriginal);
      LProcessedMask.Clear(clBlack32);

      with LProcessedMask.Canvas do
      begin
        Pen.Color := clWhite;
        Pen.Width := frmMain.GlobalPenWidth;

        if frmMain.GlobalBrushStyle = bsSolid then
        begin
          Pen.Style := psSolid;
        end
        else
        begin
          Pen.Style := frmMain.GlobalPenStyle;
        end;

        Brush.Color := clWhite;
        Brush.Style := frmMain.GlobalBrushStyle;
      end;

      // Don't know why we need to set the brush style for
      // CutOriginal.Canvas again. Otherwise, the bsClear brush style
      // setting will not affect the selection.
      FSelection.CutOriginal.Canvas.Brush.Style := frmMain.GlobalBrushStyle;

      // drawing the bezier curve
      DrawPolyBezier(FSelection.CutOriginal.Canvas,
                     [ FActualStartPoint, FActualCurvePoint1,
                       FActualCurvePoint2, FActualEndPoint ],
                     pmCopy);

      DrawPolyBezier(LProcessedMask.Canvas,
                     [ FActualStartPoint, FActualCurvePoint1,
                       FActualCurvePoint2, FActualEndPoint ],
                     pmCopy);

      // restore the alpha channels
      if FLayerList.SelectedLayer.IsLockTransparency then
      begin
        case FChannelManager.CurrentChannelType of
          ctColorChannel:
            begin
              ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
            end;

          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              ReplaceAlphaChannelWithNewValue(FSelection.CutOriginal, 255);
            end;
        end;
      end
      else
      begin
        if (FChannelManager.CurrentChannelType = ctColorChannel) and
           (FChannelManager.ColorChannelList.SelectedChannelCount < 3) then
        begin
          ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
        end
        else
        begin
          MakeCanvasProcessedOpaque(FSelection.CutOriginal, LProcessedMask);
        end;
      end;

      // adjust RGB channels of layer...
      if FChannelManager.CurrentChannelType = ctColorChannel then
      begin
        ReplaceRGBChannels(LBitmapCopy,
                           FSelection.CutOriginal,
                           LProcessedMask,
                           FChannelManager.SelectedColorChannels,
                           crsRemainDest);
      end;

      ShowProcessedSelection();
      UpdateThumbnailsBySelectedChannel();
    finally
      LProcessedMask.Free();
      LBitmapCopy.Free();
    end;

    // Undo/Redo
    LCommandName := gmFigures.GetFigureName(ffCurve);
    frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
            LCommandName,
            FChannelManager,
            FChannelManager.AlphaChannelList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
            LCommandName,
            FChannelManager,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
            LCommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
            LCommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(gmVectorLayerCommands.BEZIER_CURVE_ICON_RES_NAME);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.FinishPolygonOnLayer;
var
  LIndex       : Integer;
  LVectorLayer : TgmVectorLayer;
  LCommand     : TgmCustomCommand;
  LOnNewLayer  : Boolean;
begin
  // for Undo/Redo
  LOnNewLayer := False;

  // create a new vector layer or switch to a vector layer,
  // and then add figure to list
  if not (FLayerList.SelectedLayer is TgmVectorLayer) then
  begin
    LIndex := FLayerList.SelectedIndex;

    if (LIndex < FLayerList.Count - 1) and
       (FLayerList.Layers[LIndex + 1] is TgmVectorLayer ) then
    begin
      FLayerList.SelectLayer(LIndex + 1);
      LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);
    end
    else
    begin
      LVectorLayer := TgmVectorLayer( CreateVectorLayer() );
      FLayerList.Insert(LIndex + 1, LVectorLayer);

      // for Undo/Redo
      LOnNewLayer := True;
    end;
  end
  else
  begin
    LVectorLayer := TgmVectorLayer(FLayerList.SelectedLayer);
  end;

  LVectorLayer.FigureList.AddPolygonToList(
    frmMain.GlobalForeColor,
    frmMain.GlobalBackColor,
    frmMain.GlobalPenStyle,
    frmMain.GlobalBrushStyle,
    frmMain.GlobalPenWidth,
    FActualPolygon);

  LVectorLayer.DrawAllFiguresOnLayer();
  LVectorLayer.Changed();
  LVectorLayer.UpdateLayerThumbnail();

  // Undo/Redo
  if LOnNewLayer then
  begin
    LCommand := TgmNewFigureOnNewVectorLayerCommand.Create(
      FChannelManager, FLayerList, LVectorLayer, FLayerList.SelectedIndex);
  end
  else
  begin
    LCommand := TgmNewFigureOnExistedVectorLayerCommand.Create(
      FChannelManager, FLayerList, FLayerList.SelectedIndex,
      LVectorLayer.FigureList.Count - 1);
  end;

  if Assigned(LCommand) then
  begin
    FCommandManager.AddCommand(LCommand);
  end;
end;

procedure TfrmChild.FinishPolygonOnSelection;
var
  LProcessedMask : TBitmap32;
  LBitmapCopy    : TBitmap32;
  LCommand       : TgmCustomCommand;
  LCommandName   : string;
begin
  LCommand := nil;
  
  if Assigned(FSelection) then
  begin
    // for Undo/Redo
    frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);

    // used for tracking the processed part on a canvas
    LProcessedMask := TBitmap32.Create();
    LBitmapCopy    := TBitmap32.Create();
    try
      LBitmapCopy.SetSizeFrom(FSelection.CutOriginal);

      if FChannelManager.CurrentChannelType = ctColorChannel then
      begin
        CopyBitmap32(LBitmapCopy, FSelection.CutOriginal);
      end;

      LProcessedMask.SetSizeFrom(FSelection.CutOriginal);
      LProcessedMask.Clear(clBlack32);

      with LProcessedMask.Canvas do
      begin
        Pen.Color := clWhite;
        Pen.Width := frmMain.GlobalPenWidth;

        if frmMain.GlobalBrushStyle = bsSolid then
        begin
          Pen.Style := psSolid;
        end
        else
        begin
          Pen.Style := frmMain.GlobalPenStyle;
        end;

        Brush.Color := clWhite;
        Brush.Style := frmMain.GlobalBrushStyle;
      end;

      // Don't know why we need to set the brush style for
      // CutOriginal.Canvas again. Otherwise, the bsClear brush style
      // setting will not affect the selection.
      FSelection.CutOriginal.Canvas.Brush.Style := frmMain.GlobalBrushStyle;

      // drawing the polygon
      DrawPolygon(FSelection.CutOriginal.Canvas, FActualPolygon, pmCopy);
      DrawPolygon(LProcessedMask.Canvas, FActualPolygon, pmCopy);

      // restore the alpha channels
      if FLayerList.SelectedLayer.IsLockTransparency then
      begin
        case FChannelManager.CurrentChannelType of
          ctColorChannel:
            begin
              ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
            end;

          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              ReplaceAlphaChannelWithNewValue(FSelection.CutOriginal, 255);
            end;
        end;
      end
      else
      begin
        if (FChannelManager.CurrentChannelType = ctColorChannel) and
           (FChannelManager.ColorChannelList.SelectedChannelCount < 3) then
        begin
          ReplaceAlphaChannelWithSource(FSelection.CutOriginal, LBitmapCopy);
        end
        else
        begin
          MakeCanvasProcessedOpaque(FSelection.CutOriginal, LProcessedMask);
        end;
      end;  

      // adjust RGB channels of layer...
      if FChannelManager.CurrentChannelType = ctColorChannel then
      begin
        ReplaceRGBChannels(//FLayerList.SelectedLayer.LayerBitmap,
                           LBitmapCopy,
                           FSelection.CutOriginal,
                           LProcessedMask,
                           FChannelManager.SelectedColorChannels,
                           crsRemainDest);
      end;

      ShowProcessedSelection();
      UpdateThumbnailsBySelectedChannel();
    finally
      LProcessedMask.Free();
      LBitmapCopy.Free();
    end;

    // Undo/Redo
    LCommandName := gmFigures.GetFigureName(ffPolygon);
    frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
            LCommandName,
            FChannelManager,
            FChannelManager.AlphaChannelList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
            LCommandName,
            FChannelManager,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
            LCommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
            LCommandName,
            FChannelManager,
            FLayerList,
            FLayerList.SelectedIndex,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            GetSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(gmVectorLayerCommands.POLYGON_ICON_RES_NAME);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.FinishPolygonOnSpecialChannels;
var
  LDestBmp     : TBitmap32;
  LCommand     : TgmCustomCommand;
  LCommandName : string;
begin
  LDestBmp := nil;
  LCommand := nil;

  case FChannelManager.CurrentChannelType of
    ctAlphaChannel:
      begin
        if Assigned(FChannelManager.SelectedAlphaChannel) then
        begin
          frmMain.FBitmapBefore.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);  // for Undo/Redo
          LDestBmp := FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap;
        end;
      end;

    ctQuickMaskChannel:
      begin
        if Assigned(FChannelManager.QuickMaskChannel) then
        begin
          frmMain.FBitmapBefore.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);  // for Undo/Redo
          LDestBmp := FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap;
        end;
      end;

    ctLayerMaskChannel:
      begin
        frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);  // for Undo/Redo
        LDestBmp := FLayerList.SelectedLayer.MaskBitmap;
      end;
  end;

  if Assigned(LDestBmp) then
  begin
    DrawPolygon(LDestBmp.Canvas, FActualPolygon, pmCopy);

    // Restore the alpha channels to opaque after applied canvas operations on
    // the mask. Otherwise, if you create selection on mask, you will get a
    // mask selection with transparent area, which is bad.
    ReplaceAlphaChannelWithNewValue(LDestBmp, 255);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
        end;

      ctQuickMaskChannel:
        begin
          FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
        end;

      ctLayerMaskChannel:
        begin
          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              0, 0, FLayerList.SelectedLayer.MaskBitmap);
          end;

          FLayerList.SelectedLayer.Changed();
        end;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    LCommandName := gmFigures.GetFigureName(ffPolygon);

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          frmMain.FBitmapAfter.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

          LCommand := TgmAlphaChannelProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FChannelManager.AlphaChannelList,
            FChannelManager.AlphaChannelList.SelectedIndex);
        end;

      ctQuickMaskChannel:
        begin
          frmMain.FBitmapAfter.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

          LCommand := TgmQuickMaskChannelProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FChannelManager);
        end;

      ctLayerMaskChannel:
        begin
          frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.MaskBitmap);

          LCommand := TgmLayerMaskProcessCommand.Create(
            LCommandName,
            frmMain.FBitmapBefore,
            frmMain.FBitmapAfter,
            FLayerList,
            FLayerList.SelectedIndex);
        end;
    end;

    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(gmVectorLayerCommands.POLYGON_ICON_RES_NAME);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

{ Bresenham algorithm for Brush tools to get continuous brush strokes.
  Author         : Zoltan Komaromy (zoltan@komaromy-nospam.hu)
  Website        : www.mandalapainter.com
  SourceCode From: gr32PainterDemo3 }
procedure TfrmChild.BrushLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
  ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);
var
  a,b         : Integer;  // displacements in x and y
  d           : Integer;  // decision variable
  diag_inc    : Integer;  // d's increment for diagonal steps
  dx_diag     : Integer;  // diagonal x step for next pixel
  dx_nondiag  : Integer;  // nondiagonal x step for next pixel
  dy_diag     : Integer;  // diagonal y step for next pixel
  dy_nondiag  : Integer;  // nondiagonal y step for next pixel
  i           : Integer;  // loop index
  nondiag_inc : Integer;  // d's increment for nondiagonal steps
  swap        : Integer;  // temporary variable for swap
  x,y         : Integer;  // current x and y coordinates
  TimerEnabled: Boolean;
begin {DrawLine}
  if Assigned(frmMain.GMBrush) then
  begin
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
    else dx_diag := 1;

    // Determine whether end point lies above or below start point.
    if b < 0 then               // drawing towards smaller y values?
    begin
      b       := -b;            // make 'b' positive
      dy_diag := -1
    end
    else dy_diag := 1;

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

    TimerEnabled              := tmrSpecialBrushes.Enabled;
    tmrSpecialBrushes.Enabled := False;
    try
      for i := 0 to a do    // draw the a+1 pixels
      begin
        if Ftavolsag >= distance then
        begin
          frmMain.GMBrush.Paint(ToBitmap, x, y, ChannelSet);
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
      end; //for

    finally
      tmrSpecialBrushes.Enabled := TimerEnabled;
    end;
  end;
end;

procedure TfrmChild.AirBrushLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
  ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);
var
  a,b         : Integer;  // displacements in x and y
  d           : Integer;  // decision variable
  diag_inc    : Integer;  // d's increment for diagonal steps
  dx_diag     : Integer;  // diagonal x step for next pixel
  dx_nondiag  : Integer;  // nondiagonal x step for next pixel
  dy_diag     : Integer;  // diagonal y step for next pixel
  dy_nondiag  : Integer;  // nondiagonal y step for next pixel
  i           : Integer;  // loop index
  nondiag_inc : Integer;  // d's increment for nondiagonal steps
  swap        : Integer;  // temporary variable for swap
  x,y         : Integer;  // current x and y coordinates
  TimerEnabled: Boolean;
begin {DrawLine}
  if Assigned(frmMain.AirBrush) then
  begin
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
    else dx_diag := 1;

    // Determine whether end point lies above or below start point.
    if b < 0 then               // drawing towards smaller y values?
    begin
      b       := -b;            // make 'b' positive
      dy_diag := -1
    end
    else dy_diag := 1;

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

    TimerEnabled              := tmrSpecialBrushes.Enabled;
    tmrSpecialBrushes.Enabled := False;
    try
      for i := 0 to a do    // draw the a+1 pixels
      begin
        if Ftavolsag >= distance then
        begin
          frmMain.AirBrush.Draw(ToBitmap, x, y, ChannelSet);
          
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

    finally
      tmrSpecialBrushes.Enabled := TimerEnabled;
    end;
  end;
end;

procedure TfrmChild.EraserLine(const xStart, yStart, xEnd, yEnd, distance: Integer;
  ToBitmap: TBitmap32; const ChannelSet: TgmChannelSet);
var
  a,b         : Integer;  // displacements in x and y
  d           : Integer;  // decision variable
  diag_inc    : Integer;  // d's increment for diagonal steps
  dx_diag     : Integer;  // diagonal x step for next pixel
  dx_nondiag  : Integer;  // nondiagonal x step for next pixel
  dy_diag     : Integer;  // diagonal y step for next pixel
  dy_nondiag  : Integer;  // nondiagonal y step for next pixel
  i           : Integer;  // loop index
  nondiag_inc : Integer;  // d's increment for nondiagonal steps
  swap        : Integer;  // temporary variable for swap
  x,y         : Integer;  // current x and y coordinates
begin {DrawLine}
  if Assigned(frmMain.GMEraser) then
  begin
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
        frmMain.GMEraser.Paint(ToBitmap, x, y, ChannelSet);
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
end;

procedure TfrmChild.EraserLineOnMask(
  const xStart, yStart, xEnd, yEnd, distance: Integer;
  const ChannelSet: TgmChannelSet);
var
  a,b         : Integer;  // displacements in x and y
  d           : Integer;  // decision variable
  diag_inc    : Integer;  // d's increment for diagonal steps
  dx_diag     : Integer;  // diagonal x step for next pixel
  dx_nondiag  : Integer;  // nondiagonal x step for next pixel
  dy_diag     : Integer;  // diagonal y step for next pixel
  dy_nondiag  : Integer;  // nondiagonal y step for next pixel
  i           : Integer;  // loop index
  nondiag_inc : Integer;  // d's increment for nondiagonal steps
  swap        : Integer;  // temporary variable for swap
  x,y         : Integer;  // current x and y coordinates
begin {DrawLine}
  if Assigned(frmMain.GMEraser) then
  begin
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
        frmMain.GMEraser.Paint(FLayerList.SelectedLayer.MaskBitmap,
                               x, y, [csGrayscale]);

        // paint on mask channel layer 
        if Assigned(FChannelManager.LayerMaskChannel) then
        begin
          with FChannelManager.LayerMaskChannel do
          begin
            frmMain.GMEraser.Paint(ChannelLayer.Bitmap, x, y, [csGrayscale]);
          end;
        end;

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
end;

// Calculate coordinates for selection when the mouse pointer is over it.
// Note, the CalLayerCoord() method must be called first to get the layer
// coordinates. And then this method will convert the coordnates from
// layer space to selection space. 
procedure TfrmChild.CalcSelectionCoord;
begin
  if Assigned(FSelection) then
  begin
    FMarqueeX := MulDiv(FXActual - FSelection.MaskBorderStart.X,
                        FSelection.CutOriginal.Width - 1,
                        FSelection.Foreground.Width - 1 );

    FMarqueeY := MulDiv(FYActual - FSelection.MaskBorderStart.Y,
                        FSelection.CutOriginal.Height - 1,
                        FSelection.Foreground.Height - 1);
  end;
end;

procedure TfrmChild.PencilMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint       : TPoint;
  LStrokeBmp   : TBitmap32;
  LRefreshArea : TRect;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    if FChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(FChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if FChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // can not process on special layers...
      if not (FLayerList.SelectedLayer is TgmNormalLayer) then
      begin
        MessageDlg('Could not using the Pencil tool on current layer.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    // for performance, we just need to render the processed area
    if imgWorkArea.RepaintMode <> rmOptimizer then
    begin
      imgWorkArea.RepaintMode := rmOptimizer;
    end;

    // used for track the processsed pixels by pencil tool
    if not Assigned(FPencilMask) then
    begin
      FPencilMask := TBitmap32.Create();
    end;

    // Remember bitmap for create Undo/Redo commands.
    // Or restoring alpha channel of pixels that changed by
    // pencil tool when Lock Transparency of layer is set to true. 
    if Assigned(FSelection) then
    begin
      frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            frmMain.FBitmapBefore.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
          end;

        ctQuickMaskChannel:
          begin
            frmMain.FBitmapBefore.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;

        ctLayerMaskChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end;

    PreparePencilTool();

    if Assigned(FSelection) then
    begin
      // confirm the foreground of the selection to avoid the distortion of
      // the brush stroke
      FSelection.ConfirmForeground;
      CalcSelectionCoord;

      Felozox          := FMarqueeX;
      Felozoy          := FMarqueeY;
      FPrevStrokePoint := Point(FMarqueeX, FMarqueeY)
    end
    else
    begin
      Felozox          := FXActual;
      Felozoy          := FYActual;
      FPrevStrokePoint := Point(FXActual, FYActual);
    end;

    Ftavolsag := 0;

    // if pen width greater than 1, then create pencil stroke
    if frmMain.GlobalPenWidth <> 1 then
    begin
      if Assigned(frmMain.Pencil) then
      begin
        frmMain.Pencil.IsPreserveTransparency := FLayerList.SelectedLayer.IsLockTransparency;
        frmMain.Pencil.SetBlendMode(bbmNormal32);
        frmMain.Pencil.SetBrushOpacity(100);

        LStrokeBmp := TBitmap32.Create;
        try
          BuildPencilStroke(LStrokeBmp);
          frmMain.Pencil.SetPaintingStroke(LStrokeBmp);
        finally
          LStrokeBmp.Free;
        end;

        // set color
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              frmMain.Pencil.SetColor( Color32(frmMain.ForeGrayColor) );
            end;

          ctColorChannel:
            begin
              frmMain.Pencil.SetColor( Color32(frmMain.GlobalForeColor) );
            end;
        end;
      end;
    end;

    if Assigned(FSelection) then
    begin
      // do not draw the dynamic Marching-Ants lines when processing image
      PauseMarchingAnts;

      if frmMain.GlobalPenWidth = 1 then
      begin
        FSelection.CutOriginal.MoveToF(FMarqueeX, FMarqueeY);

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          FPencilMask.SetSizeFrom(FSelection.CutOriginal);
          FPencilMask.MoveToF(FMarqueeX, FMarqueeY);
        end;

        DrawFreeLine32(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                       frmMain.GlobalPenStyle);

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          DrawFreeLine32(FPencilMask, FMarqueeX, FMarqueeY, frmMain.GlobalPenStyle);
        end;

        // preserve transparency...
        if (FLayerList.SelectedLayer.IsLockTransparency) and
           (FChannelManager.CurrentChannelType = ctColorChannel) then
        begin
          ReplaceAlphaChannelWithSource(FSelection.CutOriginal, frmMain.FBitmapBefore);
        end;

        // If any of R, G and B channel is not selected,
        // we have to restore corresponding channel to its
        // original value.
        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
           ReplaceRGBChannels(frmMain.FBitmapBefore,
                              FSelection.CutOriginal,
                              FPencilMask,
                              FChannelManager.SelectedColorChannels,
                              crsRemainDest);
        end;

        LRefreshArea := Rect(FMarqueeX - 1, FMarqueeY - 1,
                             FMarqueeX + 1, FMarqueeY + 1);
      end
      else // if pen width greater than 1...
      begin
        frmMain.Pencil.UpdateSourceBitmap(FSelection.CutOriginal);

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          frmMain.Pencil.Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                               FChannelManager.SelectedColorChannels);
        end
        else
        begin
          frmMain.Pencil.Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                               [csGrayscale]);
        end;

        LRefreshArea := frmMain.Pencil.GetBrushArea(FMarqueeX, FMarqueeY);
      end;

      ShowSelectionAtBrushStroke(LRefreshArea);
    end
    else // if not paint on selection...
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              with FChannelManager.SelectedAlphaChannel do
              begin
                ChannelLayer.Bitmap.MoveToF(FXActual, FYActual);

                DrawFreeLine32(ChannelLayer.Bitmap, FXActual, FYActual,
                               frmMain.GlobalPenStyle);
              end;

              // get refresh area
              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);
            end
            else
            begin
              with FChannelManager.SelectedAlphaChannel do
              begin
                frmMain.Pencil.UpdateSourceBitmap(ChannelLayer.Bitmap);

                frmMain.Pencil.Paint(ChannelLayer.Bitmap, FXActual, FYActual,
                                     [csGrayscale]);
              end;

              // get refresh area
              LRefreshArea := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
            end;

            with FChannelManager.SelectedAlphaChannel do
            begin
              ChannelLayer.Bitmap.Changed(LRefreshArea);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              with FChannelManager.QuickMaskChannel do
              begin
                ChannelLayer.Bitmap.MoveToF(FXActual, FYActual);

                DrawFreeLine32(ChannelLayer.Bitmap, FXActual, FYActual,
                               frmMain.GlobalPenStyle);
              end;

              // get refresh area
              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);
            end
            else
            begin
              with FChannelManager.QuickMaskChannel do
              begin
                frmMain.Pencil.UpdateSourceBitmap(ChannelLayer.Bitmap);

                frmMain.Pencil.Paint(ChannelLayer.Bitmap, FXActual, FYActual,
                                     [csGrayscale]);
              end;
              
              // get refresh area
              LRefreshArea := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
            end;

            with FChannelManager.QuickMaskChannel do
            begin
              ChannelLayer.Bitmap.Changed(LRefreshArea);
            end;
          end;

        ctLayerMaskChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);

              with FLayerList.SelectedLayer do
              begin
                MaskBitmap.MoveToF(FXActual, FYActual);

                DrawFreeLine32(MaskBitmap, FXActual, FYActual,
                               frmMain.GlobalPenStyle);

                if Assigned(FChannelManager.LayerMaskChannel) then
                begin
                  CopyBitmap32(
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                    MaskBitmap, LRefreshArea);
                end;
              end;
            end
            else
            begin
              with FLayerList.SelectedLayer do
              begin
                frmMain.Pencil.UpdateSourceBitmap(MaskBitmap);
                frmMain.Pencil.Paint(MaskBitmap, FXActual, FYActual, [csGrayscale]);

                // paint on mask channel preview layer, too
                if Assigned(FChannelManager.LayerMaskChannel) then
                begin
                  frmMain.Pencil.Paint(
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);
                end;
              end;

              // get brush area
              LRefreshArea := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
            end;

            FLayerList.SelectedLayer.Changed(LRefreshArea);
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              FPencilMask.SetSizeFrom(FLayerList.SelectedLayer.LayerBitmap);

              if frmMain.GlobalPenWidth = 1 then
              begin
                LRefreshArea := Rect(FXActual - 1, FYActual - 1,
                                     FXActual + 1, FYActual + 1);

                with FLayerList.SelectedLayer do
                begin
                  LayerBitmap.MoveToF(FXActual, FYActual);
                  FPencilMask.MoveToF(FXActual, FYActual);

                  DrawFreeLine32(LayerBitmap, FXActual, FYActual,
                                 frmMain.GlobalPenStyle);

                  DrawFreeLine32(FPencilMask, FXActual, FYActual,
                                 frmMain.GlobalPenStyle);

                  // If any of R, G and B channel is not selected,
                  // we have to restore corresponding channel to its
                  // original value.
                  if FChannelManager.ColorChannelList.SelectedChannelCount < 3 then
                  begin
                    ReplaceRGBChannels(frmMain.FBitmapBefore,
                                       LayerBitmap,
                                       FPencilMask,
                                       FChannelManager.SelectedColorChannels,
                                       crsRemainDest);
                  end;

                  // restore alpha channels
                  if (IsLockTransparency) or
                     (FChannelManager.ColorChannelList.SelectedChannelCount < 3) then
                  begin
                    ReplaceAlphaChannelWithSource(LayerBitmap, frmMain.FBitmapBefore);
                  end;
                end;
              end
              else
              begin
                with FLayerList.SelectedLayer do
                begin
                  frmMain.Pencil.UpdateSourceBitmap(LayerBitmap);

                  frmMain.Pencil.Paint(LayerBitmap, FXActual, FYActual,
                                       FChannelManager.SelectedColorChannels);
                end;

                // get refresh area
                LRefreshArea := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
              end;

              FLayerList.SelectedLayer.Changed(LRefreshArea);
            end;
          end;
      end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.PencilMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor           : TColor;
  LPoint           : TPoint;
  LLastRefreshArea : TRect;
  LRefreshArea     : TRect;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
  CalcSelectionCoord; // get selection space coordinates

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    if Assigned(FSelection) then
    begin
      if frmMain.GlobalPenWidth = 1 then
      begin
        DrawFreeLine32(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                       frmMain.GlobalPenStyle);

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          // remember the processed pixels...
          DrawFreeLine32(FPencilMask, FMarqueeX, FMarqueeY, frmMain.GlobalPenStyle);
        end;

        // preserve transparency...
        if (FLayerList.SelectedLayer.IsLockTransparency) and
           (FChannelManager.CurrentChannelType = ctColorChannel) then
        begin
          ReplaceAlphaChannelWithSource(FSelection.CutOriginal, frmMain.FBitmapBefore);
        end;

        // If any of R, G and B channel is not selected,
        // we have to restore corresponding channel to its
        // original value.
        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          ReplaceRGBChannels(frmMain.FBitmapBefore,
                             FSelection.CutOriginal,
                             FPencilMask,
                             FChannelManager.SelectedColorChannels,
                             crsRemainDest);
        end;

        // get refresh area
        LLastRefreshArea := Rect(FPrevStrokePoint.X - 2,
                                 FPrevStrokePoint.Y - 2,
                                 FPrevStrokePoint.X + 2,
                                 FPrevStrokePoint.Y + 2);

        LRefreshArea := Rect(FMarqueeX - 2, FMarqueeY - 2,
                             FMarqueeX + 2, FMarqueeY + 2);

        LRefreshArea := AddRects(LLastRefreshArea, LRefreshArea);
      end
      else
      begin
        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          PencilLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                     FMarqueeX, FMarqueeY, 0, FSelection.CutOriginal,
                     FChannelManager.SelectedColorChannels);
        end
        else
        begin
          PencilLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                     FMarqueeX, FMarqueeY, 0, FSelection.CutOriginal,
                     [csGrayscale]);
        end;

        // get brush area
        LLastRefreshArea := frmMain.Pencil.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
        LRefreshArea     := frmMain.Pencil.GetBrushArea(FMarqueeX, FMarqueeY);
        LRefreshArea     := AddRects(LLastRefreshArea, LRefreshArea);
      end;

      ShowSelectionAtBrushStroke(LRefreshArea);
      FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
    end
    else // not paint on selection...
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              // get refresh area
              LLastRefreshArea := Rect(FPrevStrokePoint.X - 2,
                                       FPrevStrokePoint.Y - 2,
                                       FPrevStrokePoint.X + 2,
                                       FPrevStrokePoint.Y + 2);

              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);
                                   
              LRefreshArea := AddRects(LLastRefreshArea, LRefreshArea);

              // The following method will calling the line rendering
              // methods of TBitmap32, so we don't have to refresh
              // screen by ourselves.
              DrawFreeLine32(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                FXActual, FYActual, frmMain.GlobalPenStyle);
            end
            else
            begin
              PencilLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                         FXActual, FYActual, 0,
                         FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                         [csGrayscale]);

              // get refresh area
              LLastRefreshArea := frmMain.Pencil.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
              LRefreshArea     := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
              LRefreshArea     := AddRects(LLastRefreshArea, LRefreshArea);

              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LRefreshArea);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              // get refresh area
              LLastRefreshArea := Rect(FPrevStrokePoint.X - 2,
                                       FPrevStrokePoint.Y - 2,
                                       FPrevStrokePoint.X + 2,
                                       FPrevStrokePoint.Y + 2);

              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);

              LRefreshArea := AddRects(LLastRefreshArea, LRefreshArea);

              // The following method will calling the line rendering
              // methods of TBitmap32, so we don't have to refresh
              // screen by ourselves.
              DrawFreeLine32(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                             FXActual, FYActual, frmMain.GlobalPenStyle);
            end
            else
            begin
              PencilLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                         FXActual, FYActual, 0,
                         FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                         [csGrayscale]);

              // get refresh area
              LLastRefreshArea := frmMain.Pencil.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
              LRefreshArea     := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
              LRefreshArea     := AddRects(LLastRefreshArea, LRefreshArea);

              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LRefreshArea);
            end;
          end;

        ctLayerMaskChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              // get refresh area
              LLastRefreshArea := Rect(FPrevStrokePoint.X - 2,
                                       FPrevStrokePoint.Y - 2,
                                       FPrevStrokePoint.X + 2,
                                       FPrevStrokePoint.Y + 2);

              LRefreshArea := Rect(FXActual - 2, FYActual - 2,
                                   FXActual + 2, FYActual + 2);

              LRefreshArea := AddRects(LLastRefreshArea, LRefreshArea);

              DrawFreeLine32(FLayerList.SelectedLayer.MaskBitmap,
                             FXActual, FYActual, frmMain.GlobalPenStyle);

              if Assigned(FChannelManager.LayerMaskChannel) then
              begin
                CopyBitmap32(FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                             FLayerList.SelectedLayer.MaskBitmap, LRefreshArea);
              end;
            end
            else
            begin
              // PencilLineOnMask() will paint pencil stroke both on
              // FLayerList.SelectedLayer.MaskBitmap and
              // FChannelManagerMini.LayerMaskChannel.ChannelLayer.Bitmap.

              PencilLineOnMask(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, 0, [csGrayscale]);

              // get brush area
              LLastRefreshArea := frmMain.Pencil.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
              LRefreshArea     := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
              LRefreshArea     := AddRects(LLastRefreshArea, LRefreshArea);
            end;

            FLayerList.SelectedLayer.Changed(LRefreshArea);
          end;

        ctColorChannel:
          begin
            if frmMain.GlobalPenWidth = 1 then
            begin
              // get refresh area
              LLastRefreshArea := Rect(FPrevStrokePoint.X - 1,
                                       FPrevStrokePoint.Y - 1,
                                       FPrevStrokePoint.X + 1,
                                       FPrevStrokePoint.Y + 1);

              LRefreshArea := Rect(FXActual - 1, FYActual - 1,
                                   FXActual + 1, FYActual + 1);
                                   
              LRefreshArea := AddRects(LLastRefreshArea, LRefreshArea);

              DrawFreeLine32(FLayerList.SelectedLayer.LayerBitmap,
                             FXActual, FYActual, frmMain.GlobalPenStyle);

              DrawFreeLine32(FPencilMask, FXActual, FYActual,
                             frmMain.GlobalPenStyle);

              // If any of R, G and B channel is not selected,
              // we have to restore corresponding channel to its
              // original value.
              if FChannelManager.ColorChannelList.SelectedChannelCount < 3 then
              begin
                ReplaceRGBChannels(frmMain.FBitmapBefore,
                                   FLayerList.SelectedLayer.LayerBitmap,
                                   FPencilMask,
                                   FChannelManager.SelectedColorChannels,
                                   crsRemainDest);
              end;

              // restore alpha channels
              if FLayerList.SelectedLayer.IsLockTransparency then
              begin
                ReplaceAlphaChannelWithSource(
                  FLayerList.SelectedLayer.LayerBitmap, frmMain.FBitmapBefore);
              end;
            end
            else // pen width is greater than 1
            begin
              // get refresh area
              LLastRefreshArea := frmMain.Pencil.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
              LRefreshArea     := frmMain.Pencil.GetBrushArea(FXActual, FYActual);
              LRefreshArea     := AddRects(LLastRefreshArea, LRefreshArea);

              PencilLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                         FXActual, FYActual, 0,
                         FLayerList.SelectedLayer.LayerBitmap,
                         FChannelManager.SelectedColorChannels);
            end;


            FLayerList.SelectedLayer.Changed(LRefreshArea);
          end;
      end;

      FPrevStrokePoint := Point(FXActual, FYActual);
    end;
  end
  else // if the FDrawing = False
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing color info
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  // showing current layer coordinates
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.PencilMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint       : TPoint;
  LCommand     : TgmCustomCommand;
  LCommandName : string;
begin
  LCommand     := nil;
  LCommandName := 'Pencil';

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button up }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing := False;

      UpdateThumbnailsBySelectedChannel();

      if Assigned(FSelection) then
      begin
        // refresh screen for correcting the view
        ShowProcessedSelection();
        FSelection.IsAnimated := True;

        // Undo/Redo
        frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                LCommandName,
                FChannelManager,
                FChannelManager.AlphaChannelList.SelectedIndex,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                GetSelectionForUndoRedo);
            end;

          ctQuickMaskChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                LCommandName,
                FChannelManager,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                GetSelectionForUndoRedo);
            end;

          ctLayerMaskChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                LCommandName,
                FChannelManager,
                FLayerList,
                FLayerList.SelectedIndex,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                GetSelectionForUndoRedo);
            end;

          ctColorChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                LCommandName,
                FChannelManager,
                FLayerList,
                FLayerList.SelectedIndex,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                GetSelectionForUndoRedo);
            end;
        end;
      end
      else
      begin
        // Undo/Redo
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              frmMain.FBitmapAfter.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

              LCommand := TgmAlphaChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                FChannelManager.AlphaChannelList,
                FChannelManager.AlphaChannelList.SelectedIndex);
            end;

          ctQuickMaskChannel:
            begin
              frmMain.FBitmapAfter.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

              LCommand := TgmQuickMaskChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                FChannelManager);
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.MaskBitmap);

              LCommand := TgmLayerMaskProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                FLayerList,
                FLayerList.SelectedIndex);
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                frmMain.FBitmapAfter.Assign(FLayerList.SelectedLayer.LayerBitmap);
              end;

              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                frmMain.FBitmapAfter,
                FLayerList,
                FLayerList.SelectedIndex);
            end;
        end;
      end;

      // Undo/Redo
      if Assigned(LCommand) then
      begin
        LCommand.ChangeCommandIconByResourceName(gmMiscCommandIcons.PENCIL_COMMAND_ICON_RES_NAME);
        FCommandManager.AddCommand(LCommand);
      end;

      if Assigned(FPencilMask) then
      begin
        FreeAndNil(FPencilMask);
      end;
    end;
  end;
end;

procedure TfrmChild.FigureToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord;

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    // Initialize several canvases for figure tools.
    InitializeCanvasesForFigureTools;

    if FChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(FChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channel at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if frmMain.StandardTool in [gstStraightLine,
                                gstRegularPolygon,
                                gstRectangle,
                                gstRoundRectangle,
                                gstEllipse] then
    begin
      // calculate coordinates
      FStartPoint := Point(X, Y);
      FEndPoint   := FStartPoint;

      if Assigned(FSelection) then
      begin
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              FActualStartPoint := Point(FMarqueeX, FMarqueeY);
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                FActualStartPoint := Point(FMarqueeX, FMarqueeY);
              end
              else
              begin
                FActualStartPoint := Point(FXActual, FYActual);
              end;
            end;
        end;
      end
      else // if not on selection
      begin
        FActualStartPoint := Point(FXActual, FYActual);
      end;

      FActualEndPoint := FActualStartPoint;
    end;

    case frmMain.StandardTool of
      gstBezierCurve:
        begin
          if FDrawCurveTime = 0 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualStartPoint := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualStartPoint := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualStartPoint := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualStartPoint := Point(FXActual, FYActual);
            end;

            FActualCurvePoint1 := FActualStartPoint;
            FActualCurvePoint2 := FActualStartPoint;
            FActualEndPoint    := FActualStartPoint;

            FStartPoint    := Point(X, Y);
            FCurvePoint1   := FStartPoint;
            FCurvePoint2   := FStartPoint;
            FEndPoint      := FStartPoint;
            FDrawCurveTime := 1;
          end
          else if FDrawCurveTime = 2 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualCurvePoint1 := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualCurvePoint1 := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualCurvePoint1 := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualCurvePoint1 := Point(FXActual, FYActual);
            end;

            FCurvePoint1 := Point(X, Y);
          end
          else if FDrawCurveTime = 3 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualCurvePoint2 := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualCurvePoint2 := Point(FXActual, FYActual);
            end;

            FCurvePoint2 := Point(X, Y);
          end;
        end;

      gstPolygon:
        begin
          // If the polygon vertices array is empty, adding first two
          // vertices to polygon, if not appending a new point to the polygon

          if Length(FPolygon) = 0 then
          begin
            SetLength(FPolygon, 2);
            FPolygon[0] := Point(X, Y);
            FPolygon[1] := Point(X, Y);

            SetLength(FActualPolygon, 2);

            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualPolygon[0] := Point(FMarqueeX, FMarqueeY);
                    FActualPolygon[1] := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualPolygon[0] := Point(FMarqueeX, FMarqueeY);
                      FActualPolygon[1] := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualPolygon[0] := Point(FXActual, FYActual);
                      FActualPolygon[1] := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // not on selection...
            begin
              FActualPolygon[0] := Point(FXActual, FYActual);
              FActualPolygon[1] := Point(FXActual, FYActual);
            end;
          end
          else
          begin
            if ssDouble in Shift then
            begin
              FDrawing := False;
              FinishPolygon();
              Exit;
            end;

            // If the current point is far enough to the last two vertices in
            // the polygon array, then we can add it to the array.
            if CanAddPointToPolygon(FPolygon, Point(X, Y), 1.0) then
            begin
              SetLength( FPolygon, Length(FPolygon) + 1 );
              FPolygon[High(FPolygon)] := Point(X, Y);

              SetLength( FActualPolygon, Length(FActualPolygon) + 1 );

              if Assigned(FSelection) then
              begin
                case FChannelManager.CurrentChannelType of
                  ctAlphaChannel,
                  ctQuickMaskChannel,
                  ctLayerMaskChannel:
                    begin
                      FActualPolygon[High(FActualPolygon)] := Point(FMarqueeX, FMarqueeY);
                    end;

                  ctColorChannel:
                    begin
                      if FLayerList.SelectedLayer is TgmNormalLayer then
                      begin
                        FActualPolygon[High(FActualPolygon)] := Point(FMarqueeX, FMarqueeY);
                      end
                    else
                    begin
                      FActualPolygon[High(FActualPolygon)] := Point(FXActual, FYActual);
                    end;
                  end;
                end;
              end
              else // not on selection...
              begin
                FActualPolygon[High(FActualPolygon)] := Point(FXActual, FYActual);
              end;
            end;
          end;
        end;
    end;

    // Refresh the screen.
    // All the temporary figure rendering is
    // in imgWorkArea.OnPaintStage() event handler.
    imgWorkArea.Changed();

    FDrawing := True;
  end;
end;

procedure TfrmChild.FigureToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint : TPoint;
  LColor : TColor;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord;

{ Move mouse when mouse left button is pressing }

  if FDrawing then
  begin
    case frmMain.StandardTool of
      gstStraightLine,
      gstRegularPolygon:
        begin
          FEndPoint := Point(X, Y);
        end;
        
      gstPolygon:
        begin
          if FPolygon <> nil then
          begin
            FPolygon[High(FPolygon)] := Point(X, Y);

            if Assigned(FSelection) then
            begin
              FActualPolygon[High(FActualPolygon)] := Point(FMarqueeX, FMarqueeY);
            end
            else
            begin
              FActualPolygon[High(FActualPolygon)] := Point(FXActual, FYActual);
            end;
          end;
        end;

      gstRectangle,
      gstRoundRectangle,
      gstEllipse:
        begin
          if ssShift in Shift then
          begin
            FEndPoint := CalculateRegularFigureEndPoint( FStartPoint, Point(X, Y) );
          end
          else
          begin
            FEndPoint := Point(X, Y);
          end;
        end;

      gstBezierCurve:
        begin
          if FDrawCurveTime = 0 then
          begin
            Exit;
          end
          else if FDrawCurveTime = 1 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualCurvePoint2 := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualCurvePoint2 := Point(FXActual, FYActual);
            end;

            FActualEndPoint := FActualCurvePoint2;
            FCurvePoint2    := Point(X, Y);
            FEndPoint       := FCurvePoint2;
          end
          else if FDrawCurveTime = 2 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualCurvePoint1 := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualCurvePoint1 := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualCurvePoint1 := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualCurvePoint1 := Point(FXActual, FYActual);
            end;

            FCurvePoint1 := Point(X, Y);
          end
          else if FDrawCurveTime = 3 then
          begin
            if Assigned(FSelection) then
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                  end;

                ctColorChannel:
                  begin
                    if FLayerList.SelectedLayer is TgmNormalLayer then
                    begin
                      FActualCurvePoint2 := Point(FMarqueeX, FMarqueeY);
                    end
                    else
                    begin
                      FActualCurvePoint2 := Point(FXActual, FYActual);
                    end;
                  end;
              end;
            end
            else // if not on selection...
            begin
              FActualCurvePoint2 := Point(FXActual, FYActual);
            end;

            FCurvePoint2 := Point(X, Y);
          end;
        end;
    end;

    // Refresh the screen.
    // All the temporary figure rendering is
    // in imgWorkArea.OnPaintStage() event handler.
    imgWorkArea.Changed;
  end
  else // if the FDrawing = False
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing color info
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  // showing current layer coordinates
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.FigureToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
  // get selection space coordinates
  CalcSelectionCoord;
  
{ Mouse left button up }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing := False;

      if Assigned(FSelection) then
      begin
        // on special layers...
        if (FChannelManager.CurrentChannelType = ctColorChannel) and
           (not (FLayerList.SelectedLayer is TgmNormalLayer)) then
        begin
          ProcessFigureMouseUpOnLayer(Shift);
        end
        else
        begin
          ProcessFigureMouseUpOnSelection(Shift);
        end;
      end
      else // if not on selection...
      begin
        if FChannelManager.CurrentChannelType in [ctAlphaChannel,
                                                  ctQuickMaskChannel,
                                                  ctLayerMaskChannel] then
        begin
          ProcessFigureMouseUpOnSpecialChannels(Shift);
        end
        else
        begin
          ProcessFigureMouseUpOnLayer(Shift);
        end;
      end;
    end;

    frmMain.UpdateStandardOptions;
    FLayerList.SelectedLayer.LayerBitmap.Canvas.Pen.Mode := pmCopy;
  end;
end;

procedure TfrmChild.FigureManagingToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint      : TPoint;
  LPopupPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button down }
  if Button = mbLeft then
  begin
    // showing coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    case frmMain.StandardTool of
      gstMoveObjects:
        begin
          PopupMenu         := nil;
          FMoveDrawingState := dsNotDrawing;

          // get handle that selected by the mouse
          FMoveDrawingHandle := FFigureManager.GetHandleOfSelectedFigures(
            X, Y, imgWorkArea.BitmapToControl);

          // if the mouse is pointing on one of the following handles...
          if FMoveDrawingHandle in [dhAXAY,
                                    dhBXBY,
                                    dhAXBY,
                                    dhBXAY,
                                    dhLeftHalfAYBY,
                                    dhRightHalfAYBY,
                                    dhTopHalfAXBX,
                                    dhBottomHalfAXBX,
                                    dhCurvePoint1,
                                    dhCurvePoint2,
                                    dhLineStart,
                                    dhLineEnd,
                                    dhPolygonPoint] then
          begin
            // Getting FSelectedFigure points to current selected figure.
            // If there are more than one figures were selected,
            // the GetOnlyOneSelectedFigure() routine will return nil.
            FSelectedFigure    := FFigureManager.GetOnlyOneSelectedFigure;
            FMoveDrawingState  := dsStretchCorner;
            Screen.Cursor      := SetCursorByHandle(FMoveDrawingHandle);
            imgWorkArea.Cursor := Screen.Cursor;

            FFigureManager.DrawUnselectedAndLockedFiguresOnSelectedVectorLayers();

            // Calling Changed() on the currently selected layer to
            // re-blend all layers and refresh the screen.
            FLayerList.SelectedLayer.Changed;

            if Assigned(FSelectedFigure) then
            begin
              if FMoveDrawingHandle = dhAXAY then
              begin
                FRegularBasePoint := FSelectedFigure.FEndPoint;
              end
              else if FMoveDrawingHandle = dhBXBY then
              begin
                FRegularBasePoint := FSelectedFigure.FStartPoint;
              end
              else if FMoveDrawingHandle = dhAXBY then
              begin
                FRegularBasePoint := Point(FSelectedFigure.FEndPoint.X,
                                           FSelectedFigure.FStartPoint.Y);
              end
              else if FMoveDrawingHandle = dhBXAY then
              begin
                FRegularBasePoint := Point(FSelectedFigure.FStartPoint.X,
                                           FSelectedFigure.FEndPoint.Y);
              end;

              // For Undo/Redo;
              FDrawingBasePoint := Point(FXActual, FYActual);
              FOldFigure        := FSelectedFigure.GetSelfBackup();
            end;
          end
          else
          begin
            // If the mouse is not pointing on any control handles,
            // checking whether the mouse is pointing on any of figure objects.
            if FFigureManager.PointOnSelectedFigure( Point(X, Y), imgWorkArea.BitmapToControl ) then
            begin
              FMoveDrawingState  := dsTranslate;
              FDrawingBasePoint  := Point(FXActual, FYActual);
              Screen.Cursor      := crDrag;
              imgWorkArea.Cursor := crDrag;

              FFigureManager.DrawUnselectedAndLockedFiguresOnSelectedVectorLayers();

              // Calling Changed() on the currently selected layer to
              // re-blend all layers and refresh the screen.
              FLayerList.SelectedLayer.Changed();

              FAccumTranslateVector := Point(0, 0); // for Undo/Redo
            end
            else
            begin
              // select figures
              if not (ssShift in Shift) then
              begin
                FFigureManager.DeselectAllFigures;
              end;

              FFigureManager.SelectFigures(Shift, FXActual, FYActual);

              if FFigureManager.SelectedFigureCount > 0 then
              begin
                FDrawingBasePoint  := Point(FXActual, FYActual);
                Screen.Cursor      := crDrag;
                imgWorkArea.Cursor := crDrag;
                FMoveDrawingState  := dsTranslate;

                FFigureManager.DrawUnselectedAndLockedFiguresOnSelectedVectorLayers;

                // Calling Changed() on the currently selected layer to
                // re-blend all layers and refresh the screen.
                FLayerList.SelectedLayer.Changed;
              end;
            end;
          end;
        end;

      gstPartiallySelect,
      gstTotallySelect:
        begin
          FStartPoint       := Point(X, Y);
          FEndPoint         := FStartPoint;
          FActualStartPoint := Point(FXActual, FYActual);
          FActualEndPoint   := FActualStartPoint;
          FRegionSelectOK   := False;
        end;
    end;

    FDrawing := True; // marking up the processing begin
  end
  else
  if Button = mbRight then  // Mouse Right Button Down
  begin
    if frmMain.StandardTool = gstMoveObjects then
    begin
      // get handle that selected by the mouse
      FMoveDrawingHandle := FFigureManager.GetHandleOfSelectedFigures(
        X, Y, imgWorkArea.BitmapToControl);

      // if one of the following curve control handle was selected ...
      if FMoveDrawingHandle in [dhCurvePoint1, dhCurvePoint2] then
      begin
        // Get FSelectedFigure points to current selected figure.
        // If there are more than one figure were selected,
        // the GetFirstSelectedFigure() routine will return nil. 
        FSelectedFigure := FFigureManager.GetOnlyOneSelectedFigure;

        // there is only one figure was selected...
        if Assigned(FSelectedFigure) then
        begin
          // If the two curve control handles are both at the same postion,
          // then showing a pop-up menu to allow the users to make decision of
          // which handle they want to choose of.
          if (FSelectedFigure.FCurvePoint1.X = FSelectedFigure.FCurvePoint2.X) and
             (FSelectedFigure.FCurvePoint1.Y = FSelectedFigure.FCurvePoint2.Y) then
          begin
            // connect the pop-up menu to the form and showing it at current position
            PopupMenu := pmnChangeCurveControlPoints;

            GetCursorPos(LPopupPoint);
            pmnChangeCurveControlPoints.Popup(LPopupPoint.X, LPopupPoint.Y);
          end
          else
          begin
            PopupMenu := nil;
          end;
        end;
      end
      else
      begin
        PopupMenu := nil; // disconnect the pop-up menu to the form
      end;
    end;
  end;
end;

procedure TfrmChild.FigureManagingToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint           : TPoint;
  LNewPoint        : TPoint;
  LTranslateVector : TPoint;
  LColor           : TColor;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
{ when mouse left button is pressed... }
  if FDrawing then
  begin
    case frmMain.StandardTool of
      gstMoveObjects:
        begin
         case FMoveDrawingState of
            dsStretchCorner:
              begin
                if Assigned(FSelectedFigure) then
                begin
                  if (FSelectedFigure.Flag = ffSquare) or
                     (FSelectedFigure.Flag = ffRoundSquare) or
                     (FSelectedFigure.Flag = ffCircle) then
                  begin
                    if FMoveDrawingHandle in [dhAXAY, dhBXBY, dhAXBY, dhBXAY] then
                    begin
                      FSelectedFigure.FStartPoint := FRegularBasePoint;

                      FSelectedFigure.FEndPoint :=
                        CalculateRegularFigureEndPoint(
                          FRegularBasePoint, Point(FXActual, FYActual) );
                    end;
                  end
                  else
                  begin
                    case FMoveDrawingHandle of
                      dhAXAY,
                      dhLineStart:
                        begin
                          FSelectedFigure.FStartPoint := Point(FXActual, FYActual);
                        end;
                        
                      dhBXBY,
                      dhLineEnd:
                        begin
                          FSelectedFigure.FEndPoint := Point(FXActual, FYActual);
                        end;

                      dhAXBY:
                        begin
                          FSelectedFigure.FStartPoint := Point(FXActual, FSelectedFigure.FStartPoint.Y);
                          FSelectedFigure.FEndPoint   := Point(FSelectedFigure.FEndPoint.X, FYActual);
                        end;

                      dhBXAY:
                        begin
                          FSelectedFigure.FStartPoint := Point(FSelectedFigure.FStartPoint.X, FYActual);
                          FSelectedFigure.FEndPoint   := Point(FXActual, FSelectedFigure.FEndPoint.Y);
                        end;

                      dhLeftHalfAYBY:
                        begin
                          FSelectedFigure.FStartPoint := Point(FXActual, FSelectedFigure.FStartPoint.Y);
                        end;
                        
                      dhRightHalfAYBY:
                        begin
                          FSelectedFigure.FEndPoint := Point(FXActual, FSelectedFigure.FEndPoint.Y);
                        end;
                        
                      dhTopHalfAXBX:
                        begin
                          FSelectedFigure.FStartPoint := Point(FSelectedFigure.FStartPoint.X, FYActual);
                        end;
                        
                      dhBottomHalfAXBX:
                        begin
                          FSelectedFigure.FEndPoint := Point(FSelectedFigure.FEndPoint.X, FYActual);
                        end;
                        
                      dhCurvePoint1 :
                        begin
                          FSelectedFigure.FCurvePoint1 := Point(FXActual, FYActual);
                        end;
                        
                      dhCurvePoint2:
                        begin
                          FSelectedFigure.FCurvePoint2 := Point(FXActual, FYActual);
                        end;

                      dhPolygonPoint:
                        begin
                          if FSelectedFigure.Flag = ffPolygon then
                          begin
                            FSelectedFigure.FPolygonPoints[
                              FSelectedFigure.PolygonCurrentPointIndex] :=
                                Point(FXActual, FYActual);
                          end
                          else
                          if FSelectedFigure.Flag = ffRegularPolygon then
                          begin
                            FSelectedFigure.FEndPoint := Point(FXActual, FYActual);

                            CalcRegularPolygonVertices(
                              FSelectedFigure.FPolygonPoints,
                              FSelectedFigure.FStartPoint,
                              Point(FXActual, FYActual),
                              frmMain.StandardPolygonSides );
                          end;
                        end;
                    end;
                  end;
                end;

                // Refreshing the screen, the translating figures are
                // rendering in imgWorkArea.OnPaintStage() event.
                imgWorkArea.Changed();
              end;

            dsTranslate:
              begin
                // calculate the translation vector
                LNewPoint        := Point(FXActual, FYActual);
                LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

                FFigureManager.TranslateSelectedFigures(LTranslateVector);

                // Refreshing the screen, the translating figures are
                // rendering in imgWorkArea.OnPaintStage() event.
                imgWorkArea.Changed();

                FDrawingBasePoint := LNewPoint;

                // for Undo/Redo 
                FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);
              end;
          end; 
        end;

      gstPartiallySelect,
      gstTotallySelect:
        begin
          FActualEndPoint := Point(FXActual, FYActual);
          FEndPoint       := Point(X, Y);

          // Refreshing the screen, the rubber-band selector is
          // rendering in imgWorkArea.OnPaintStage() event.
          imgWorkArea.Changed();
        end;
    end;
  end
  else // if the FDrawing = False
  begin
    FMoveDrawingHandle := FFigureManager.GetHandleOfSelectedFigures(
      X, Y, imgWorkArea.BitmapToControl);
    
    // change cursor according to different selected handles
    if FMoveDrawingHandle in [dhAXAY,
                              dhBXBY,
                              dhAXBY,
                              dhBXAY,
                              dhLeftHalfAYBY,
                              dhRightHalfAYBY,
                              dhTopHalfAXBX,
                              dhBottomHalfAXBX,
                              dhLineStart,
                              dhLineEnd,
                              dhCurvePoint1,
                              dhCurvePoint2,
                              dhPolygonPoint]
    then
    begin
      Screen.Cursor      := SetCursorByHandle(FMoveDrawingHandle);
      imgWorkArea.Cursor := Screen.Cursor;
    end
    else
    begin
      if FFigureManager.PointOnFigure( Point(X, Y), imgWorkArea.BitmapToControl ) then
      begin
        Screen.Cursor      := crHandPoint;
        imgWorkArea.Cursor := crHandPoint;
      end
      else
      begin
        Screen.Cursor := crDefault;

        case frmMain.StandardTool of
          gstMoveObjects:
            begin
              imgWorkArea.Cursor := crMoveSelection;
            end;

          gstPartiallySelect,
          gstTotallySelect:
          begin
            imgWorkArea.Cursor := crCross;
          end;
        end;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing color info that under the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.FigureManagingToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint   : TPoint;
  LCommand : TgmCustomCommand;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  LCommand := nil;

{ mouse left button is released }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing      := False;
      Screen.Cursor := crDefault;

      case frmMain.StandardTool of
        gstMoveObjects:
          begin
            case FMoveDrawingState of
              dsTranslate,
              dsStretchCorner:
                begin
                  imgWorkArea.Cursor := crMoveSelection;

                  if Assigned(FSelectedFigure) then
                  begin
                    if FSelectedFigure.Flag in [ffRectangle,
                                                ffSquare,
                                                ffRoundRectangle,
                                                ffRoundSquare,
                                                ffEllipse,
                                                ffCircle] then
                    begin
                      FSelectedFigure.StandardizeOrder;
                    end;

                    if FSelectedFigure.Flag in [ffRegularPolygon,
                                                ffSquare,
                                                ffRoundSquare,
                                                ffCircle] then
                    begin
                      FSelectedFigure.CalcOrigin();
                      FSelectedFigure.CalcRadius();
                    end;
                  end;

                  // Undo/Redo
                  if FMoveDrawingState = dsTranslate then
                  begin
                    if (FAccumTranslateVector.X <> 0) or
                       (FAccumTranslateVector.Y <> 0) then
                    begin
                      LCommand := TgmTranslateFiguresOnVectorLayersCommand.Create(
                        FLayerList, FFigureManager.FSelectedFigureInfoArray,
                        FAccumTranslateVector);
                    end;
                  end
                  else if FMoveDrawingState = dsStretchCorner then
                  begin
                    if (FDrawingBasePoint.X <> FXActual) or
                       (FDrawingBasePoint.Y <> FYActual) then
                    begin
                      LCommand := TgmSingleFigureAdjustmentCommand.Create(
                        FLayerList, FLayerList.SelectedIndex,
                        FFigureManager.FSelectedFigureInfoArray[0].FigureIndex,
                        FOldFigure, FSelectedFigure);

                      FreeAndNil(FOldFigure);
                    end;
                  end;

                  if Assigned(LCommand) then
                  begin
                    FCommandManager.AddCommand(LCommand);
                  end;

                  // NOTE: we have to restore the FMoveDrawingState.
                  FMoveDrawingState := dsNotDrawing;
                end;
            end;
          end;

        gstPartiallySelect,
        gstTotallySelect:
          begin
            if FFigureManager.HasFiguresOnVectorLayers then
            begin
              FActualEndPoint := Point(FXActual, FYActual);
              PointStandardizeOrder(FActualStartPoint, FActualEndPoint);

              FRegionSelectOK := ( ABS(FActualStartPoint.X - FActualEndPoint.X) > 4 ) and
                                 ( ABS(FActualStartPoint.Y - FActualEndPoint.Y) > 4 );

              if FRegionSelectOK then
              begin
                if frmMain.StandardTool = gstPartiallySelect then
                begin
                  FFigureManager.SelectFiguresByRectOnVectorLayers(
                    fsimPartiallyInclude, FActualStartPoint, FActualEndPoint);
                end
                else
                if frmMain.StandardTool = gstTotallySelect then
                begin
                  FFigureManager.SelectFiguresByRectOnVectorLayers(
                    fsimTotallyInclude, FActualStartPoint, FActualEndPoint);
                end;

                // if there areis selected figures
                // then switch to Move Figure tool
                if FFigureManager.SelectedFigureCount > 0 then
                begin
                  frmMain.spdbtnMoveObjects.Down := True;
                  frmMain.ChangeStandardTools(frmMain.spdbtnMoveObjects);
                end;
              end;
            end;
          end;
      end;

      if FFigureManager.HasFiguresOnVectorLayers then
      begin
        FFigureManager.DrawAllFiguresOnSelectedVectorLayers;
        FFigureManager.UpdateThumbnailOnSelectedVectorLayers;

        // Calling Changed() on the currently selected layer to
        // re-blend all layers and refresh the screen.
        FLayerList.SelectedLayer.Changed;

        frmMain.UpdateStandardOptions;
      end
      else
      begin
        imgWorkArea.Changed;  // refresh screen anyway...
      end;


      if FFigureManager.SelectedFigureCount = 1 then
      begin
        FSelectedFigure := FFigureManager.GetOnlyOneSelectedFigure;
      end
      else
      begin
        FSelectedFigure := nil;
      end;
    end;
  end;
end;

// translate selection by keyboard stroke
procedure TfrmChild.TranslateSelectionKeyDown(var Key: Word; Shift: TShiftState);
var
  LTranslateVector : TPoint;
  LIncrement       : Integer;
begin
  // if a selection definition has not been finished...
  if Assigned(FRegion) or Assigned(FMagneticLasso) then
  begin
    Exit;
  end;

  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if Assigned(FSelection) then
    begin
      if FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;

      if ssShift in Shift then
      begin
        LIncrement := 10;
      end
      else
      begin
        LIncrement := 1;
      end;

      case Key of
        VK_LEFT:
          begin
            LTranslateVector := Point(-LIncrement, 0);
          end;

        VK_UP:
          begin
            LTranslateVector := Point(0, -LIncrement);
          end;
          
        VK_RIGHT:
          begin
            LTranslateVector := Point(LIncrement, 0);
          end;
          
        VK_DOWN:
          begin
            LTranslateVector := Point(0, LIncrement);
          end;
      end;

      if (ssCtrl in Shift) or
         (FSelection.IsProcessed) or
         (not FSelection.IsPrimitive) then
      begin
        // For Undo/Redo
        if FKeyIsDown = False then
        begin
          if FSelectionCopy = nil then
          begin
            FSelectionCopy := TgmSelection.Create(imgWorkArea);
          end;

          FSelectionCopy.AssignAllSelectionData(FSelection);
          FKeyIsDown := True;
        end;

        // translate selection
        FSelection.TranslateSelection(LTranslateVector);

        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(FChannelManager.SelectedAlphaChannel) then
              begin
                FSelection.ShowSelection(
                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                  [csGrayscale]);
              end;
            end;

          ctQuickMaskChannel:
            begin
              FSelection.ShowSelection(
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                [csGrayscale]);
            end;

          ctLayerMaskChannel:
            begin
              FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap,
                                       [csGrayscale]);

              if Assigned(FChannelManager.LayerMaskChannel) then
              begin
                FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, FLayerList.SelectedLayer.MaskBitmap);
              end;
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                FSelection.ShowSelection(FLayerList.SelectedLayer.LayerBitmap,
                                         FChannelManager.SelectedColorChannels);
              end;
            end;
        end;

        // mark that we are translate the selection itself
        if FSelectionTranslateTarget <> sttSelection then
        begin
          FSelectionTranslateTarget := sttSelection;
        end;
      end
      else
      begin
        // For Undo/Redo
        if FKeyIsDown = False then
        begin
          if FSelectionCopy = nil then
          begin
            FSelectionCopy := TgmSelection.Create(imgWorkArea);
          end;

          FSelectionCopy.AssignAllSelectionData(FSelection);
          FKeyIsDown := True;
        end;

        // Nudge Selection Outline
        if (not FSelection.IsProcessed) and FSelection.IsPrimitive then
        begin
          FSelection.TranslateCutRegion(LTranslateVector);
        end;

        // mark that we are translate the cutted region of the selection
        if FSelectionTranslateTarget <> sttCutRegion then
        begin
          FSelectionTranslateTarget := sttCutRegion;
        end;
      end;

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
            end;
          end;

        ctQuickMaskChannel:
          begin
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
          end;

        ctLayerMaskChannel,
        ctColorChannel:
          begin
            FLayerList.SelectedLayer.Changed();
          end;
      end;

      imgWorkArea.Update();
      FSelection.DrawMarchingAnts();
    end;
  end;
end;

procedure TfrmChild.TranslateSelectionKeyUp(var Key: Word; Shift: TShiftState);
var
  LStdCutMask : TBitmap;
  LCommand    : TgmCustomCommand;
begin
  // if a selection definition has not been finished...
  if Assigned(FRegion) or Assigned(FMagneticLasso) then
  begin
    Exit;
  end;

  LCommand := nil;

  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if Assigned(FSelection) then
    begin
      case FSelectionTranslateTarget of
        sttSelection:
          begin
            // Undo/Redo
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
                    satTranslate, FChannelManager,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
                    satTranslate, FChannelManager, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
                    satTranslate, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
                    satTranslate, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;
            end;

            LCommand.ChangeCommandIconByResourceName(
              gmHistoryCommands.MOVE_COMMAND_ICON_RES_NAME);

            if Assigned(LCommand) then
            begin
              FCommandManager.AddCommand(LCommand);
            end;
          end;

        sttCutRegion:
          begin
            // If the cut region is out of the range of current layer,
            // then recalculating the selection. 
            if (FSelection.MaskBorderStart.X < 0) or
               (FSelection.MaskBorderStart.Y < 0) or
               (FSelection.MaskBorderEnd.X >= FLayerList.SelectedLayer.LayerBitmap.Width) or
               (FSelection.MaskBorderEnd.Y >= FLayerList.SelectedLayer.LayerBitmap.Height) then
            begin
              FSelection.OriginalMask.Clear(clBlack32);
              
              LStdCutMask := TBitmap.Create();
              try
                LStdCutMask.Assign(FSelection.CutMask);

                FSelection.OriginalMask.Canvas.Draw(
                  FSelection.MaskBorderStart.X,
                  FSelection.MaskBorderStart.Y,
                  LStdCutMask);
              finally
                LStdCutMask.Free();
              end;

              FSelection.GetActualMaskBorder();
              FSelection.CutRegionFromOriginal();
              FSelection.GetForeground();
              FSelection.GetMarchingAntsLines();  
            end;

            // update the background
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel,
              ctQuickMaskChannel,
              ctLayerMaskChannel:
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.BackGrayColor), [csGrayscale] );
                end;

              ctColorChannel:
                begin
                  if FLayerList.SelectedLayer is TgmNormalLayer then
                  begin
                    if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
                    begin
                      FSelection.GetBackgroundWithFilledColor(
                        Color32(frmMain.GlobalBackColor),
                        FChannelManager.SelectedColorChannels );
                    end
                    else
                    begin
                      // transparent layer
                      if FChannelManager.ColorChannelList.SelectedChannelCount >= 3 then
                      begin
                        FSelection.GetBackgroundWithTransparent();
                      end
                      else
                      begin
                        FSelection.GetBackgroundWithFilledColor(
                          Color32(frmMain.GlobalBackColor),
                          FChannelManager.SelectedColorChannels );
                      end;
                    end;
                  end;
                end;
            end;

            // Undo/Redo
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
                    satNudgeOutline, FChannelManager,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
                    satNudgeOutline, FChannelManager, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
                    satNudgeOutline, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
                    satNudgeOutline, FChannelManager, FLayerList,
                    FLayerList.SelectedIndex, FSelectionCopy, FSelection,
                    GetSelectionForUndoRedo);
                end;
            end;

            if Assigned(LCommand) then
            begin
              FCommandManager.AddCommand(LCommand);
            end;
          end;
      end;

      // update thumbnails
      UpdateThumbnailsBySelectedChannel();

      if not FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := True;
      end;
    end;
  end;
end;

procedure TfrmChild.GradientToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();
  UpdateMainFormStatusBarWhenMouseDown();

// Mouse left button down 

  if Button = mbLeft then
  begin
    if FChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if not (FLayerList.SelectedLayer is TgmNormalLayer) then
      begin
        MessageDlg('Could not use the gradient tool ' + 'because the' + #10#13 +
                   'content of the layer is not directly' + #10#13 +
                   'editable.', mtError, [mbOK], 0);
        Exit;
      end;
    end;

    // don't draw dynamic Marching-Ants lines when processing image
    PauseMarchingAnts();

    FStartPoint       := Point(X, Y);
    FEndPoint         := FStartPoint;
    FActualStartPoint := Point(FXActual, FYActual);

    FDrawing := True;
  end;
end;

procedure TfrmChild.GradientToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor : TColor;
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();

// Move mouse when mouse left button down 

  if FDrawing then
  begin
    FEndPoint := Point(X, Y);

    // The gradient indicator line is rendering by
    // OnPaintStage() event of imgWorkArea.
    imgWorkArea.Changed();
  end
  else // Move mouse when mouse left button not down 
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and 
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.GradientToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LGradientIndex  : Integer;
  LGradientRender : TgmGradientRender;
  LPoint          : TPoint;
  LCommand        : TgmCustomCommand;
  LCommandName    : string;
begin
  LCommand     := nil;
  LCommandName := 'Gradient';

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();

// Mouse left button up 

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing      := False;
      Screen.Cursor := crDefault;

      if FChannelManager.CurrentChannelType = ctAlphaChannel then
      begin
        if not Assigned(FChannelManager.SelectedAlphaChannel) then
        begin
          MessageDlg('Could not process more than one alpha channels at a time.',
                     mtError, [mbOK], 0);
          Exit;
        end;
      end;

      LGradientRender := TgmGradientRender.Create();
      try
        LGradientIndex := frmGradientPicker.FDrawingToolGradientListState.SelectedIndex;
        
        LGradientRender.ColorGradient :=
          frmGradientPicker.glDrawingToolGradients.Items[LGradientIndex];
            
        LGradientRender.BlendMode  := frmMain.GradientBlendMode;
        LGradientRender.Opacity    := frmMain.GradientBlendOpacity / 100;
        LGradientRender.IsReverse  := frmMain.chckbxReverseGradient.Checked;
        LGradientRender.RenderMode := frmMain.GradientRenderMode;

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          LGradientRender.ChannelSet := FChannelManager.SelectedColorChannels;
        end
        else
        begin
          LGradientRender.ChannelSet := [csGrayscale];
        end;

        if Assigned(FSelection) then
        begin
          // Convert the endpoints of the gradient line from Image
          // control space to selection space. Note, we just need to
          // convert the starting point of the gradient line.

          FActualStartPoint.X := MulDiv(FActualStartPoint.X - FSelection.MaskBorderStart.X,
                                        FSelection.CutOriginal.Width - 1,
                                        FSelection.Foreground.Width - 1);

          FActualStartPoint.Y := MulDiv(FActualStartPoint.Y - FSelection.MaskBorderStart.Y,
                                        FSelection.CutOriginal.Height - 1,
                                        FSelection.Foreground.Height - 1);

          FActualEndPoint := Point(FMarqueeX, FMarqueeY);

          LGradientRender.StartPoint := FActualStartPoint;
          LGradientRender.EndPoint   := FActualEndPoint;

          // for Undo/Redo
          frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);

          // do gradient rendering
          if LGradientRender.Render(FSelection.CutOriginal) then
          begin
            ShowProcessedSelection();
            UpdateThumbnailsBySelectedChannel();

            // for Undo/Redo
            frmMain.FBitmapAfter.Assign(FSelection.CutOriginal);

            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FChannelManager.AlphaChannelList.SelectedIndex,
                    frmMain.FBitmapBefore,
                    frmMain.FBitmapAfter,
                    GetSelectionForUndoRedo);
                end;

              ctQuickMaskChannel:
                begin
                  LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                    LCommandName,
                    FChannelManager,
                    frmMain.FBitmapBefore,
                    frmMain.FBitmapAfter,
                    GetSelectionForUndoRedo);
                end;

              ctLayerMaskChannel:
                begin
                  LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FLayerList,
                    FLayerList.SelectedIndex,
                    frmMain.FBitmapBefore,
                    frmMain.FBitmapAfter,
                    GetSelectionForUndoRedo);
                end;

              ctColorChannel:
                begin
                  LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                    LCommandName,
                    FChannelManager,
                    FLayerList,
                    FLayerList.SelectedIndex,
                    frmMain.FBitmapBefore,
                    frmMain.FBitmapAfter,
                    GetSelectionForUndoRedo);
                end;
            end;
          end;
        end
        else
        begin
          FActualEndPoint := Point(FXActual, FYActual);

          LGradientRender.StartPoint := FActualStartPoint;
          LGradientRender.EndPoint   := FActualEndPoint;

          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                if Assigned(FChannelManager.SelectedAlphaChannel) then
                begin
                  // for Undo/Redo
                  frmMain.FBitmapBefore.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                  // do gradient rendering
                  with FChannelManager.SelectedAlphaChannel do
                  begin
                    if LGradientRender.Render(ChannelLayer.Bitmap) then
                    begin
                      ChannelLayer.Bitmap.Changed();
                      UpdateChannelThumbnail();

                      // Undo/Redo
                      LCommand := TgmAlphaChannelProcessCommand.Create(
                        LCommandName,
                        frmMain.FBitmapBefore,
                        ChannelLayer.Bitmap,
                        FChannelManager.AlphaChannelList,
                        FChannelManager.AlphaChannelList.SelectedIndex);
                    end;
                  end;
                end;
              end;

            ctQuickMaskChannel:
              begin
                if Assigned(FChannelManager.QuickMaskChannel) then
                begin
                  // for Undo/Redo
                  frmMain.FBitmapBefore.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

                  // do gradient rendering
                  with FChannelManager.QuickMaskChannel do
                  begin
                    if LGradientRender.Render(ChannelLayer.Bitmap) then
                    begin
                      ChannelLayer.Bitmap.Changed();
                      UpdateChannelThumbnail();

                      // Undo/Redo
                      LCommand := TgmQuickMaskChannelProcessCommand.Create(
                        LCommandName,
                        frmMain.FBitmapBefore,
                        ChannelLayer.Bitmap,
                        FChannelManager);
                    end;
                  end;
                end;
              end;

            ctLayerMaskChannel:
              begin
                // for Undo/Redo
                frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);

                if LGradientRender.Render(FLayerList.SelectedLayer.MaskBitmap) then
                begin
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      0, 0, FLayerList.SelectedLayer.MaskBitmap);

                    FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
                  end;

                  FLayerList.SelectedLayer.Changed();
                  FLayerList.SelectedLayer.UpdateMaskThumbnail();

                  // Undo/Redo
                  LCommand := TgmLayerMaskProcessCommand.Create(
                    LCommandName,
                    frmMain.FBitmapBefore,
                    FLayerList.SelectedLayer.MaskBitmap,
                    FLayerList,
                    FLayerList.SelectedIndex);
                end;
              end;

            ctColorChannel:
              begin
                // for Undo/Redo
                frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);

                // do gradient rendering
                if LGradientRender.Render(FLayerList.SelectedLayer.LayerBitmap) then
                begin
                  FLayerList.SelectedLayer.Changed();
                  FLayerList.SelectedLayer.UpdateLayerThumbnail();

                  // Undo/Redo
                  LCommand := TgmLayerImageProcessCommand.Create(
                    LCommandName,
                    frmMain.FBitmapBefore,
                    FLayerList.SelectedLayer.LayerBitmap,
                    FLayerList,
                    FLayerList.SelectedIndex);
                end;
              end;
          end;
        end;

      finally
        FreeAndNil(LGradientRender);
      end;

      if Assigned(FSelection) then
      begin
        FSelection.IsAnimated := True;
      end;

      // Undo/Redo
      if Assigned(LCommand) then
      begin
        LCommand.ChangeCommandIconByResourceName(gmMiscCommandIcons.GRADIENT_COMMAND_ICON_RES_NAME);
        FCommandManager.AddCommand(LCommand);
      end;
    end;
  end;
end;

procedure TfrmChild.CropMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // show the coordinates of the starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    with imgWorkArea.Canvas do
    begin
      Pen.Color   := RUBBER_BAND_PEN_COLOR;
      Pen.Style   := RUBBER_BAND_PEN_STYLE;
      Pen.Width   := RUBBER_BAND_PEN_WIDTH;
      Brush.Color := RUBBER_BAND_BRUSH_COLOR;
      Brush.Style := RUBBER_BAND_BRUSH_STYLE;
    end;

    if Assigned(FCrop) then
    begin
      // 1.  Trying to stretch corner of selected figure?
      //     Note that, the X and Y is in control space.
      FCropDrawingHandle := FCrop.GetHandleAtPoint(X, Y, imgWorkArea.BitmapToControl);

      if FCropDrawingHandle in [dhAXAY,
                                dhBXBY,
                                dhAXBY,
                                dhBXAY,
                                dhTopHalfAXBX,
                                dhBottomHalfAXBX,
                                dhLeftHalfAYBY,
                                dhRightHalfAYBY] then
      begin
        Screen.Cursor      := SetCursorByHandle(FCropDrawingHandle);
        imgWorkArea.Cursor := Screen.Cursor;

        // change crop tool to stretch state
        FCropDrawingState := dsStretchCorner;  
      end
      else
      // 2.  Trying to translate selected figure(s)? Check first for existing set of selected figures.
      if FCrop.ContainsPoint( Point(FXActual, FYActual) ) then
      begin
        Screen.Cursor      := crDrag;
        imgWorkArea.Cursor := crDrag;

        FCropDrawingState := dsTranslate;
        FDrawingBasePoint := Point(FXActual, FYActual);
      end;
    end
    else
    begin
      Screen.Cursor      := crCrop;
      imgWorkArea.Cursor := crCrop;

      FStartPoint       := Point(X, Y);
      FEndPoint         := FStartPoint;
      FActualStartPoint := Point(FXActual, FYActual);

      DrawRectangle(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.CropMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint           : TPoint;
  LNewPoint        : TPoint;
  LTranslateVector : TPoint;
  LColor           : TColor;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    if Assigned(FCrop) then
    begin
      case FCropDrawingState of
        dsStretchCorner:
          begin
            with FCrop do
            begin
              case FCropDrawingHandle of
                dhAxAy:
                  begin
                    FCropStart := Point(FXActual, FYActual);
                  end;

                dhBxBy:
                  begin
                    FCropEnd := Point(FXActual, FYActual);
                  end;

                dhAxBy:
                  begin
                    FCropStart.X := FXActual;
                    FCropEnd.Y   := FYActual;
                  end;

                dhBxAy:
                  begin
                    FCropStart.Y := FYActual;
                    FCropEnd.X   := FXActual;
                  end;

                dhTopHalfAxBx:
                  begin
                    FCropStart.Y := FYActual;
                  end;

                dhBottomHalfAxBx:
                  begin
                    FCropEnd.Y := FYActual;
                  end;

                dhLeftHalfAyBy:
                  begin
                    FCropStart.X := FXActual;
                  end;

                dhRightHalfAyBy:
                  begin
                    FCropEnd.X := FXActual;
                  end;
              end;

              StandardizeOrder();
              DrawShield();
              imgWorkArea.Changed();
            end;

            // showing the dimension of the cropped area
            frmMain.CanChange := False;
            try
              frmMain.edtCropWidth.Text  := IntToStr(FCrop.CropAreaWidth);
              frmMain.edtCropHeight.Text := IntToStr(FCrop.CropAreaHeight);
            finally
              frmMain.CanChange := True;
            end;
          end;

        dsTranslate:
          begin
            with FCrop do
            begin
              LNewPoint        := Point(FXActual, FYActual);
              LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

              Translate(LTranslateVector);
              DrawShield();
              imgWorkArea.Changed();

              FDrawingBasePoint := LNewPoint;
            end;
          end;
      end;
    end
    else
    begin
      // clear the old figure
      DrawRectangle(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);

      // update the end point of the crop border
      FEndPoint := Point(X, Y);

      // drawing the new one
      DrawRectangle(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);

      // showing the dimension of the cropped area
      frmMain.CanChange := False;
      try
        frmMain.edtCropWidth.Text  := IntToStr(Abs(FEndPoint.X - FStartPoint.X));
        frmMain.edtCropHeight.Text := IntToStr(Abs(FEndPoint.Y - FStartPoint.Y));
      finally
        frmMain.CanChange := True;
      end;
    end;
  end
  else // if the FDrawing = False
  begin
    if Assigned(FCrop) then
    begin
      // Note that, the X and Y is in control space.
      FCropDrawingHandle := FCrop.GetHandleAtPoint(X, Y, imgWorkArea.BitmapToControl);
      Screen.Cursor      := SetCursorByHandle(FCropDrawingHandle);

      if Screen.Cursor = crDefault then
      begin
        imgWorkArea.Cursor := crCrop;
      end
      else
      begin
        imgWorkArea.Cursor := Screen.Cursor;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.CropMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
  LRect  : TRect;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    FDrawing           := False;
    Screen.Cursor      := crDefault;
    imgWorkArea.Cursor := crCrop;
    
    if Assigned(FCrop) then
    begin
      case FCropDrawingState of
        dsStretchCorner,
        dsTranslate:
          begin
            imgWorkArea.Changed();
            FCropDrawingState := dsNotDrawing;
          end;
      end;
    end
    else
    begin
      // if a selection is existed, we have to commit the selection first
      if FSelection <> nil then
      begin
        if FSelectionCopy = nil then
        begin
          FSelectionCopy := TgmSelection.Create(imgWorkArea);
        end;

        FSelectionCopy.AssignAllSelectionData(FSelection);
        CommitSelection();
      end
      else
      begin
        FActualEndPoint := Point(FXActual, FYActual);
        PointStandardizeOrder(FActualStartPoint, FActualEndPoint);

        // get layer loction
        LRect             := imgWorkArea.GetBitmapRect();
        LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
        LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

        FCrop := TgmCrop.Create( FLayerList.SelectedLayer.LayerBitmap.Width,
                                 FLayerList.SelectedLayer.LayerBitmap.Height,
                                 imgWorkArea.Layers, FloatRect(LRect) );
                                 
        with FCrop do
        begin
          FCropStart          := FActualStartPoint;
          FCropEnd            := FActualEndPoint;
          ShieldColor32       := Color32(frmMain.shpCroppedShieldColor.Brush.Color);
          ShieldOpacity       := frmMain.updwnCroppedShieldOpacity.Position;
          IsShieldCroppedArea := frmMain.chckbxShieldCroppedArea.Checked;

          DrawShield();
          imgWorkArea.Changed();
        end;

        frmMain.UpdateCropOptions();
      end;
    end;

    imgWorkArea.Canvas.Pen.Mode := pmCopy;
  end;
end;

// translate Crop by keyboard stroke
procedure TfrmChild.TranslateCropKeyDown(var Key: Word; Shift: TShiftState);
var
  LTranslateVector : TPoint;
  LIncrement       : Integer;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if Assigned(FCrop) then
    begin
      if ssShift in Shift then
      begin
        LIncrement := 10;
      end
      else
      begin
        LIncrement := 1;
      end;

      case Key of
        VK_LEFT:
          begin
            LTranslateVector := Point(-LIncrement, 0);
          end;

        VK_UP:
          begin
            LTranslateVector := Point(0, -LIncrement);
          end;

        VK_RIGHT:
          begin
            LTranslateVector := Point(LIncrement, 0);
          end;
          
        VK_DOWN:
          begin
            LTranslateVector := Point(0, LIncrement);
          end;
      end;

      with FCrop do
      begin
        Translate(LTranslateVector);
        DrawShield();
        imgWorkArea.Changed();
      end;
    end;
  end;
end;

procedure TfrmChild.PaintBucketMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();

// Mouse left button down 

  if Button = mbLeft then
  begin
    if FChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(FChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end
    else if FChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if not (FLayerList.SelectedLayer is TgmNormalLayer) then
      begin
        MessageDlg('Could not use the Paint Bucket because the' + #10#13 +
                   'content of the layer is not directly' + #10#13 +
                   'editable.', mtError, [mbOK], 0);
        Exit;
      end;
    end;

    // processing only when the mouse pointer is on the layer or selection
    if Assigned(FSelection) then
    begin
      if not FSelection.IfPointsOnSelection(FMarqueeX, FMarqueeY) then
      begin
        Exit;
      end;
    end
    else
    begin
      if (FXActual < 0) or
         (FYActual < 0) or
         (FXActual >= FLayerList.SelectedLayer.LayerBitmap.Width) or
         (FYActual >= FLayerList.SelectedLayer.LayerBitmap.Height) then
      begin
        Exit;
      end;
    end;

    // showing the coordinates of the starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    if Assigned(FSelection) then
    begin
      // not draw the dynamic Marching-Lines when process image
      if FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;
      
      FSelection.DrawMarchingAnts();
    end;

    // save bitmap for Undo/Redo
    if Assigned(FSelection) then
    begin
      frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(FChannelManager.QuickMaskChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.PaintBucketMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor : TColor;
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  if FDrawing = False then
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual < FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual < FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing the color info of the current pixel that under the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  // showing the current coordinates of the mouse
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.PaintBucketMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPaintBucket  : TgmPaintBucket;
  LSampleBmp    : TBitmap32;
  LFlattenedBmp : TBitmap32;
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
begin
  LCommand     := nil;
  LCommandName := 'Paint Bucket';

  if FDrawing then
  begin
    FDrawing := False;  // finish drawing

    if Assigned(FSelection) then
    begin
      if not FSelection.IsFeathered then
      begin
        FSelection.IsAnimated := True;
      end;
    end;

    LFlattenedBmp := TBitmap32.Create();
    LSampleBmp    := TBitmap32.Create();
    LPaintBucket  := TgmPaintBucket.Create();
    try
      with LPaintBucket do
      begin
        Tolerance            := MulDiv(255, frmMain.PaintBucketTolerance, 100);
        Opacity              := MulDiv(255, frmMain.PaintBucketOpacity, 100);
        AdjustIntensity      := frmPaintBucketAdvancedOptions.updwnFillIntensity.Position / 100;
        FillSource           := frmMain.PaintBucketFillSource;
        BlendMode            := frmMain.PaintBucketBlendMode;
        ColorMode            := TgmPaintBucketColorMode(frmPaintBucketAdvancedOptions.cmbbxFillType.ItemIndex);
        PreserveTransparency := frmLayers.chckbxLockTransparency.Checked;

        if FChannelManager.CurrentChannelType = ctColorChannel then
        begin
          ChannelSet := FChannelManager.SelectedColorChannels;
        end
        else
        begin
          ChannelSet := [csGrayscale];
        end;

        if frmMain.chckbxFillContiguous.Checked then
        begin
          FillCondition := pbfcContiguous;
        end
        else
        begin
          FillCondition := pbfcDiscontiguous;
        end;
      end;

      // setting the filling color
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel,
        ctQuickMaskChannel,
        ctLayerMaskChannel:
          begin
            case frmMain.PaintBucketFillSource of
              pbfsForeColor:
                begin
                  LPaintBucket.Color := Color32(frmMain.ForeGrayColor);
                end;
                
              pbfsBackColor:
                begin
                  LPaintBucket.Color := Color32(frmMain.BackGrayColor);
                end;
            end;
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              case frmMain.PaintBucketFillSource of
                pbfsForeColor:
                  begin
                    LPaintBucket.Color := Color32(frmMain.GlobalForeColor);
                  end;
                  
                pbfsBackColor:
                  begin
                    LPaintBucket.Color := Color32(frmMain.GlobalBackColor);
                  end;
              end;
            end;
          end;
      end;

      // set filling pattern
      if frmMain.PaintBucketFillSource = pbfsPattern then
      begin
        LPaintBucket.Pattern.Assign(frmPatterns.PaintBucketPattern);
        LPaintBucket.Pattern.PixelFormat := pf24bit;

        if FChannelManager.CurrentChannelType in [
             ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
        begin
          Desaturate(LPaintBucket.Pattern);
        end;
      end;

      // get the sample bitmap for sampling color
      if frmMain.chckbxFillAllLayers.Checked then
      begin
        LFlattenedBmp.DrawMode := dmBlend;
        LSampleBmp.DrawMode    := dmBlend;

        BlendLayersAndChannelsToBitmap(LFlattenedBmp);

        if Assigned(FSelection) then
        begin
          CopyRect32WithARGB( LSampleBmp, LFlattenedBmp,
                              Rect(FSelection.MaskBorderStart.X,
                                   FSelection.MaskBorderStart.Y,
                                   FSelection.MaskBorderEnd.X,
                                   FSelection.MaskBorderEnd.Y),
                              Color32(frmMain.GlobalForeColor) );

          SmoothResize32(LSampleBmp,
                         FSelection.CutOriginal.Width,
                         FSelection.CutOriginal.Height);
        end
        else
        begin
          LSampleBmp.Assign(LFlattenedBmp);
        end;
      end
      else
      begin
        if Assigned(FSelection) then
        begin
          LSampleBmp.Assign(FSelection.CutOriginal);
        end
        else
        begin
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LSampleBmp.Assign(
                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
              end;

            ctQuickMaskChannel:
              begin
                LSampleBmp.Assign(
                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
              end;

            ctLayerMaskChannel:
              begin
                LSampleBmp.Assign(FLayerList.SelectedLayer.MaskBitmap);
              end;

            ctColorChannel:
              begin
                LSampleBmp.Assign(FLayerList.SelectedLayer.LayerBitmap);
              end;
          end;
        end;
      end;

      // execute filling
      if Assigned(FSelection) then
      begin
        LPaintBucket.Paint(LSampleBmp, FSelection.CutOriginal, FMarqueeX, FMarqueeY);
        ShowProcessedSelection();
        UpdateThumbnailsBySelectedChannel();

        // Undo/Redo
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                LCommandName,
                FChannelManager,
                FChannelManager.AlphaChannelList.SelectedIndex,
                frmMain.FBitmapBefore,
                FSelection.CutOriginal,
                GetSelectionForUndoRedo);
            end;

          ctQuickMaskChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                LCommandName,
                FChannelManager,
                frmMain.FBitmapBefore,
                FSelection.CutOriginal,
                GetSelectionForUndoRedo);
            end;

          ctLayerMaskChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                LCommandName,
                FChannelManager,
                FLayerList,
                FLayerList.SelectedIndex,
                frmMain.FBitmapBefore,
                FSelection.CutOriginal,
                GetSelectionForUndoRedo);
            end;

          ctColorChannel:
            begin
              LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                LCommandName,
                FChannelManager,
                FLayerList,
                FLayerList.SelectedIndex,
                frmMain.FBitmapBefore,
                FSelection.CutOriginal,
                GetSelectionForUndoRedo);
            end;
        end;
      end
      else
      begin
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              with FChannelManager.SelectedAlphaChannel do
              begin
                LPaintBucket.Paint(LSampleBmp, ChannelLayer.Bitmap,
                                   FXActual, FYActual);

                ChannelLayer.Bitmap.Changed();
                UpdateChannelThumbnail();
              end;

              // Undo/Redo
              LCommand := TgmAlphaChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                FChannelManager.AlphaChannelList,
                FChannelManager.AlphaChannelList.SelectedIndex);
            end;

          ctQuickMaskChannel:
            begin
              with FChannelManager.QuickMaskChannel do
              begin
                LPaintBucket.Paint(LSampleBmp, ChannelLayer.Bitmap,
                                   FXActual, FYActual);

                ChannelLayer.Bitmap.Changed();
                UpdateChannelThumbnail();
              end;

              // Undo/Redo
              LCommand := TgmQuickMaskChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                FChannelManager);
            end;

          ctLayerMaskChannel:
            begin
              LPaintBucket.Paint(LSampleBmp,
                                 FLayerList.SelectedLayer.MaskBitmap,
                                 FXActual, FYActual);

              if Assigned(FChannelManager.LayerMaskChannel) then
              begin
                FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, FLayerList.SelectedLayer.MaskBitmap);

                FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
              end;

              FLayerList.SelectedLayer.Changed();
              FLayerList.SelectedLayer.UpdateMaskThumbnail();

              // Undo/Redo
              LCommand := TgmLayerMaskProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                FLayerList.SelectedLayer.MaskBitmap,
                FLayerList,
                FLayerList.SelectedIndex);
            end;

          ctColorChannel:
            begin
              LPaintBucket.Paint(LSampleBmp,
                FLayerList.SelectedLayer.LayerBitmap,
                FXActual, FYActual);

              FLayerList.SelectedLayer.Changed();
              FLayerList.SelectedLayer.UpdateLayerThumbnail();

              // Undo/Redo
              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                FLayerList.SelectedLayer.LayerBitmap,
                FLayerList,
                FLayerList.SelectedIndex);
            end;
        end;
      end;

      // Undo/Redo
      if Assigned(LCommand) then
      begin
        LCommand.ChangeCommandIconByResourceName(gmMiscCommandIcons.PAINT_BUCKET_COMMAND_ICON_RES_NAME);
        FCommandManager.AddCommand(LCommand);
      end;

    finally
      LPaintBucket.Free();
      LSampleBmp.Free();
      LFlattenedBmp.Free();
    end;
  end;
end;

procedure TfrmChild.EraserMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  r, g, b      : Integer;
  LFlattenBmp  : TBitmap32;
  LSampleBmp   : TBitmap32;
  LTempBmp     : TBitmap32;
  LEraser      : TgmEraser;
  LEraserName  : string;
  LBackEraser  : TgmBackgroundEraser;
  LPaintBucket : TgmPaintBucket;
  LBrushArea   : TRect;
  LPoint       : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

// Mouse left button down

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    if not Assigned(frmMain.GMEraser) then
    begin
      Exit;
    end;

    if FChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(FChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if FChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if not (FLayerList.SelectedLayer is TgmNormalLayer) then
      begin
        LEraserName := frmMain.GMEraser.Name;

        MessageDlg('Could not use the ' + LEraserName + ' because the' + #10#13 +
                   'content of the layer is not directly' + #10#13 +
                   'editable.', mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if imgWorkArea.RepaintMode <> rmOptimizer then
    begin
      imgWorkArea.RepaintMode := rmOptimizer;
    end;

    if Assigned(FSelection) then
    begin
      // don't draw dynamic Marching-Ants lines when processing image
      if FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;

      // confirm the foreground of the selection to avoid the
      // distortion of the brush stroke
      FSelection.ConfirmForeground();

      // get selection space coordinates
      CalcSelectionCoord();

      Felozox          := FMarqueeX;
      Felozoy          := FMarqueeY;
      FPrevStrokePoint := Point(FMarqueeX, FMarqueeY)
    end
    else
    begin
      Felozox          := FXActual;
      Felozoy          := FYActual;
      FPrevStrokePoint := Point(FXActual, FYActual);
    end;

    // save bitmap for Undo/Redo
    if Assigned(FSelection) then
    begin
      frmMain.FBitmapBefore.Assign(FSelection.CutOriginal);
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(FChannelManager.QuickMaskChannel) then
            begin
              frmMain.FBitmapBefore.Assign(
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            frmMain.FBitmapBefore.Assign(FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end;

    Ftavolsag := 0; // For EraserLine() function

    frmMain.GMEraser.IsPreserveTransparency :=
      FLayerList.SelectedLayer.IsLockTransparency;

    case frmMain.EraserTool of
      etEraser:
        begin
          tmrSpecialErasers.Enabled := False;

          if (frmMain.GMEraser.BrushID = bidEraser) and
             Assigned(frmPaintingBrush.EraserStroke) then
          begin
            LEraser                  := TgmEraser(frmMain.GMEraser);
            LEraser.IsEraseToHistory := frmEraserAdvancedOptions.chckbxEraserHistory.Checked;

            if frmEraserAdvancedOptions.chckbxEraserHistory.Checked then
            begin
              LEraser.SetHistoryBitmap(FHistoryBitmap);
            end;

            LEraser.SetErasingMode(frmMain.ErasingMode);
            LEraser.SetAirErasingPressure(frmMain.AirErasingPressure);

            LEraser.SetBrushOpacity(frmMain.ErasingOpacity);
            LEraser.SetPaintingStroke(frmPaintingBrush.EraserStroke);

            // set eraser color
            if FChannelManager.CurrentChannelType in [ctAlphaChannel,
                                                      ctQuickMaskChannel,
                                                      ctLayerMaskChannel] then
            begin
              LEraser.SetErasingColor( Color32(frmMain.BackGrayColor) );
            end
            else
            begin
              LEraser.SetErasingColor( Color32(frmMain.GlobalBackColor) );
            end;
            
            // Eraser Dynamics Settings ...
            LEraser.OriginalPressure := frmMain.AirErasingPressure;

            LEraser.SetDynamicSize(frmPaintingBrush.EraserStroke,
                                   frmBrushDynamics.SizeDynamicsState,
                                   frmBrushDynamics.SizeSteps);

            LEraser.SetDynamicOpacity(frmMain.ErasingOpacity,
                                      frmBrushDynamics.OpacityDynamicsState,
                                      frmBrushDynamics.OpacitySteps );

            if (FChannelManager.CurrentChannelType in [ctAlphaChannel,
                                                       ctQuickMaskChannel,
                                                       ctLayerMaskChannel]) then
            begin
              // set the preserve transparency of eraser to True to let the
              // eraser to draw color
              LEraser.IsPreserveTransparency := True;

              // do not erase to history state
              if frmEraserAdvancedOptions.chckbxEraserHistory.Checked then
              begin
                LEraser.IsEraseToHistory := False;
              end;
            end
            else
            begin
              // If currently working on layers, and the color channels are not
              // fully selected, that is, red, green and blue channel are not
              // all being selected, then we set the perserve transparency of
              // eraser to True to let it draw background color on the
              // destination, not drawing transparent pixels on the
              // destination. 

              if FChannelManager.ColorChannelList.SelectedChannelCount < 3 then
              begin
                LEraser.IsPreserveTransparency := True;
              end;
            end;

            if Assigned(FSelection) then
            begin
              with LEraser do
              begin
                if FChannelManager.CurrentChannelType in [
                     ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
                begin
                  if frmEraserAdvancedOptions.chckbxEraserHistory.Checked then
                  begin
                    IsEraseToHistory := False;
                  end;
                end;

                // setting the history sample offset
                SelectionOffsetX := FSelection.MaskBorderStart.X;
                SelectionOffsetY := FSelection.MaskBorderStart.Y;

                UpdateSourceBitmap(FSelection.CutOriginal);

                if FChannelManager.CurrentChannelType = ctColorChannel then
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                        FChannelManager.SelectedColorChannels);
                end
                else
                begin
                  Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY, [csGrayscale]);
                end;
              end;

              // Mark the alpha channel of selection's foreground is changed,
              // so the selection will use its background to refresh the view
              // before drawing foreground.
              if not FSelection.IsForeAlphaChanged then
              begin
                FSelection.IsForeAlphaChanged := True;
              end;

              LBrushArea := LEraser.GetBrushArea(FMarqueeX, FMarqueeY);
              ShowSelectionAtBrushStroke(LBrushArea);
            end
            else
            begin
              case FChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    with FChannelManager.SelectedAlphaChannel do
                    begin
                      LEraser.UpdateSourceBitmap(ChannelLayer.Bitmap);

                      LEraser.Paint(ChannelLayer.Bitmap,
                                    FXActual, FYActual,
                                    [csGrayscale]);

                      // get refresh area
                      LBrushArea := LEraser.GetBrushArea(FXActual, FYActual);

                      ChannelLayer.Bitmap.Changed(LBrushArea);
                    end;
                  end;

                ctQuickMaskChannel:
                  begin
                    with FChannelManager.QuickMaskChannel do
                    begin
                      LEraser.UpdateSourceBitmap(ChannelLayer.Bitmap);

                      LEraser.Paint(ChannelLayer.Bitmap,
                                    FXActual, FYActual,
                                    [csGrayscale]);

                      // get refresh area
                      LBrushArea := LEraser.GetBrushArea(FXActual, FYActual);

                      ChannelLayer.Bitmap.Changed(LBrushArea);
                    end;
                  end;

                ctLayerMaskChannel:
                  begin
                    LEraser.UpdateSourceBitmap(FLayerList.SelectedLayer.MaskBitmap);

                    LEraser.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                  FXActual, FYActual, [csGrayscale]);

                    // paint on mask channel layer as well
                    if Assigned(FChannelManager.LayerMaskChannel) then
                    begin
                      LEraser.Paint(FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                                    FXActual, FYActual, [csGrayscale]);
                    end;

                    // get refresh area
                    LBrushArea := LEraser.GetBrushArea(FXActual, FYActual);

                    // update the view
                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;

                ctColorChannel:
                  begin
                    // must be on layer
                    if frmEraserAdvancedOptions.chckbxEraserHistory.Checked then
                    begin
                      if (FLayerList.SelectedLayer.LayerBitmap.Width  <> FHistoryBitmap.Width) or
                         (FLayerList.SelectedLayer.LayerBitmap.Height <> FHistoryBitmap.Height) then
                      begin
                        MessageDlg('Could not to erase to the history state because the current' + #10#13 +
                                   'canvas size does not match that of the history state!',
                                   mtError, [mbOK], 0);

                        Exit;
                      end;
                    end;

                    LEraser.UpdateSourceBitmap(FLayerList.SelectedLayer.LayerBitmap);

                    // get refresh area
                    LBrushArea := LEraser.GetBrushArea(FXActual, FYActual);

                    LEraser.Paint(FLayerList.SelectedLayer.LayerBitmap,
                                  FXActual, FYActual,
                                  FChannelManager.SelectedColorChannels);

                    FLayerList.SelectedLayer.Changed(LBrushArea);
                  end;
              end;
            end;

            if frmMain.ErasingMode = emAirBrush then
            begin
              tmrSpecialErasers.Interval := frmMain.AirErasingInterval;

              if tmrSpecialErasers.Enabled <> True then
              begin
                tmrSpecialErasers.Enabled := True;
              end;
            end;
          end;
        end;

      etBackgroundEraser:
        begin
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel,
            ctQuickMaskChannel:
              begin
                MessageDlg('Could not use the background eraser because' + #10#13 +
                           'it does not work with alpha channels.',
                           mtError, [mbOK], 0);

                if Assigned(FSelection) then
                begin
                  FSelection.IsAnimated := True;
                end;

                Exit;
              end;

            ctLayerMaskChannel:
              begin
                MessageDlg('Could not use the background eraser because' + #10#13 +
                           'the target channels do not cover the' + #10#13 +
                           'composite.', mtError, [mbOK], 0);

                if Assigned(FSelection) then
                begin
                  FSelection.IsAnimated := True;
                end;

                Exit;
              end;

            ctColorChannel:
              begin
                // must be on layer

                if FChannelManager.ColorChannelList.SelectedChannelCount < 3 then
                begin
                  MessageDlg('Could not use the background eraser because' + #10#13 +
                             'the target channels do not cover the' + #10#13 +
                             'composite.', mtError, [mbOK], 0);

                  if Assigned(FSelection) then
                  begin
                    FSelection.IsAnimated := True;
                  end;

                  Exit;
                end;

                if not (FLayerList.SelectedLayer is TgmNormalLayer) then
                begin
                  MessageDlg('Could not use the background eraser because' + #10#13 +
                             'the content of the layer is not directly' + #10#13 +
                             'editable.', mtError, [mbOK], 0);

                  if Assigned(FSelection) then
                  begin
                    FSelection.IsAnimated := True;
                  end;

                  Exit;
                end;

                tmrSpecialErasers.Enabled := False;

                if Assigned(frmMain.GMEraser) and
                   (frmMain.GMEraser.BrushID = bidBackgroundEraser) then
                begin
                  if Assigned(frmPaintingBrush.EraserStroke) then
                  begin
                    LBackEraser := TgmBackgroundEraser(frmMain.GMEraser);

                    with LBackEraser do
                    begin
                      IsProtectedForeground := frmEraserAdvancedOptions.chckbxProtectForeground.Checked;
                      ProtectedColor        := Color32(frmMain.GlobalForeColor);

                      SetSamplingMode(frmMain.EraserSamplingMode);
                      SetErasingLimit(frmMain.BackgroundEraserLimit);
                      SetTolerance(frmMain.ErasingTolerance);
                    end;

                    if frmMain.EraserSamplingMode = bsmBackgroundSwatch then
                    begin
                      LBackEraser.SampledColor := Color32(frmMain.GlobalBackColor);
                    end;

                    frmMain.GMEraser.SetPaintingStroke(frmPaintingBrush.EraserStroke);

                    // Brush Dynamics Settings ...
                    LBackEraser.OriginalTolerance := frmMain.ErasingTolerance;

                    frmMain.GMEraser.SetDynamicSize(
                      frmPaintingBrush.EraserStroke,
                      frmBrushDynamics.SizeDynamicsState,
                      frmBrushDynamics.SizeSteps);

                    frmMain.GMEraser.SetDynamicOpacity(
                      frmMain.ErasingTolerance,
                      frmBrushDynamics.OpacityDynamicsState,
                      frmBrushDynamics.OpacitySteps );

                    if Assigned(FSelection) then
                    begin
                      frmMain.GMEraser.UpdateSourceBitmap(FSelection.CutOriginal);

                      if frmMain.EraserSamplingMode in [bsmContiguous, bsmOnce] then
                      begin
                        LBackEraser.SamplingColor(FMarqueeX, FMarqueeY);
                      end;

                      frmMain.GMEraser.Paint(
                        FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                        FChannelManager.SelectedColorChannels);

                      LBrushArea := frmMain.GMEraser.GetBrushArea(FMarqueeX, FMarqueeY);

                      // Mark the alpha channel of the selection foreground
                      // has changed, so the selection will use its background
                      // to refresh the view before drawing foreground.
                      if not FSelection.IsForeAlphaChanged then
                      begin
                        FSelection.IsForeAlphaChanged := True;
                      end;

                      ShowSelectionAtBrushStroke(LBrushArea);
                    end
                    else
                    begin
                      frmMain.GMEraser.UpdateSourceBitmap(
                        FLayerList.SelectedLayer.LayerBitmap);

                      if frmMain.EraserSamplingMode in [bsmContiguous, bsmOnce] then
                      begin
                        LBackEraser.SamplingColor(FXActual, FYActual);
                      end;

                      // get refresh area
                      LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

                      frmMain.GMEraser.Paint(
                        FLayerList.SelectedLayer.LayerBitmap,
                        FXActual, FYActual,
                        FChannelManager.SelectedColorChannels);

                      FLayerList.SelectedLayer.Changed(LBrushArea);
                    end;

                    if frmMain.EraserSamplingMode in [bsmContiguous, bsmOnce] then
                    begin
                      frmColor.CurrColorSelector := csBackColor;

                      r := LBackEraser.SampledColor shr 16 and $FF;
                      g := LBackEraser.SampledColor shr  8 and $FF;
                      b := LBackEraser.SampledColor        and $FF;

                      frmColor.ChangeColorViaTrackBar(r, g, b);
                    end;

                    if frmMain.BackgroundEraserLimit = belDiscontiguous then
                    begin
                      tmrSpecialErasers.Interval := frmMain.AirErasingInterval;

                      if tmrSpecialErasers.Enabled <> True then
                      begin
                        tmrSpecialErasers.Enabled := True;
                      end;
                    end;
                  end;
                end;
              end;
          end;
        end;

      etMagicEraser:
        begin
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel,
            ctQuickMaskChannel:
              begin
                MessageDlg('Could not use the background eraser because' + #10#13 +
                           'it does not work with alpha channels.',
                           mtError, [mbOK], 0);

                if Assigned(FSelection) then
                begin
                  FSelection.IsAnimated := True;
                end;

                Exit;
              end;

            ctLayerMaskChannel:
              begin
                MessageDlg('Could not use the magic eraser because the ' + #10#13 +
                           'target channels do not cover the composite.',
                           mtError, [mbOK], 0);

                if Assigned(FSelection) then
                begin
                  FSelection.IsAnimated := True;
                end;

                Exit;
              end;

            ctColorChannel:
              begin
                // must be on layer

                if FChannelManager.ColorChannelList.SelectedChannelCount < 3 then
                begin
                  MessageDlg('Could not use the magic eraser because the ' + #10#13 +
                             'target channels do not cover the composite.',
                             mtError, [mbOK], 0);

                  if Assigned(FSelection) then
                  begin
                    FSelection.IsAnimated := True;
                  end;

                  Exit;
                end;

                if not (FLayerList.SelectedLayer is TgmNormalLayer) then
                begin
                  MessageDlg('Could not use the magic eraser because' + #10#13 +
                             'the content of the layer is not directly' + #10#13 +
                             'editable.', mtError, [mbOK], 0);

                  if Assigned(FSelection) then
                  begin
                    FSelection.IsAnimated := True;
                  end;

                  Exit;
                end;

                LSampleBmp   := TBitmap32.Create();
                LFlattenBmp  := TBitmap32.Create();
                LPaintBucket := TgmPaintBucket.Create();
                try
                  LSampleBmp.DrawMode  := dmBlend;
                  LFlattenBmp.DrawMode := dmBlend;

                  with LPaintBucket do
                  begin
                    Color                := Color32(frmMain.GlobalBackColor);
                    Tolerance            := MulDiv(255, frmMain.ErasingTolerance, 100);
                    Opacity              := MulDiv(255, frmMain.ErasingOpacity, 100);
                    BlendMode            := bbmNormal32;
                    PreserveTransparency := frmLayers.chckbxLockTransparency.Checked;

                    if frmMain.chckbxFillContiguous.Checked then
                    begin
                      FillCondition := pbfcContiguous;
                    end
                    else
                    begin
                      FillCondition := pbfcDiscontiguous;
                    end;

                    if FLayerList.SelectedLayer.IsLockTransparency then
                    begin
                      FillSource := pbfsBackColor;
                    end
                    else
                    begin
                      FillSource := pbfsTransparent;
                    end;
                  end;

                  if frmEraserAdvancedOptions.chckbxUseAllLayers.Checked then
                  begin
                    FLayerList.FlattenLayersToBitmapWithoutMask(LFlattenBmp);

                    if Assigned(FSelection) then
                    begin
                      CopyRect32WithARGB( LSampleBmp, LFlattenBmp,
                                          Rect(FSelection.MaskBorderStart.X,
                                               FSelection.MaskBorderStart.Y,
                                               FSelection.MaskBorderEnd.X,
                                               fSelection.MaskBorderEnd.Y),
                                          Color32(frmMain.GlobalBackColor) );

                      SmoothResize32(LSampleBmp,
                                     FSelection.CutOriginal.Width,
                                     FSelection.CutOriginal.Height);
                    end
                    else
                    begin
                      LSampleBmp.Assign(LFlattenBmp);
                    end;
                  end
                  else
                  begin
                    if Assigned(FSelection) then
                    begin
                      LSampleBmp.Assign(FSelection.CutOriginal);
                    end
                    else
                    begin
                      LSampleBmp.Assign(FLayerList.SelectedLayer.LayerBitmap);
                    end;
                  end;

                  // execute filling
                  if Assigned(FSelection) then
                  begin
                    LPaintBucket.Paint(LSampleBmp, FSelection.CutOriginal,
                                       FMarqueeX, FMarqueeY);

                    // mark the alpha channel of selection foreground
                    // has been changed
                    if not FSelection.IsForeAlphaChanged then
                    begin
                      FSelection.IsForeAlphaChanged := True;
                    end;

                    ShowProcessedSelection();
                  end
                  else
                  begin
                    LPaintBucket.Paint(LSampleBmp,
                                       FLayerList.SelectedLayer.LayerBitmap,
                                       FXActual, FYActual);

                    FLayerList.SelectedLayer.Changed();
                  end;

                finally
                  LPaintBucket.Free();
                  LSampleBmp.Free();
                  LFlattenBmp.Free();
                end;
              end;
          end;
        end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.EraserMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint          : TPoint;
  LBrushArea      : TRect;
  LLastStrokeArea : TRect;
  LInterval       : Integer;
  LColor          : TColor;
  LBackEraser     : TgmBackgroundEraser;
  r, g, b         : Byte;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();

// Move mouse when mouse left button down 

  if FDrawing then
  begin
    case frmMain.EraserTool of
      etEraser:
        begin
          case frmMain.ErasingMode of
            emPaintBrush:
              begin
                LInterval := frmMain.ErasingInterval;
              end;

            emAirBrush:
              begin
                LInterval := 0;
              end;
              
          else
            LInterval := 0;
          end;

          if Assigned(FSelection) then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel,
              ctLayerMaskChannel,
              ctQuickMaskChannel:
                begin
                  EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                             FMarqueeX, FMarqueeY, LInterval,
                             FSelection.CutOriginal,
                             [csGrayscale]);
                end;

              ctColorChannel:
                begin
                  EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                             FMarqueeX, FMarqueeY, LInterval,
                             FSelection.CutOriginal,
                             FChannelManager.SelectedColorChannels);
                end;
            end;

            // get brush area
            LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
            LBrushArea      := frmMain.GMEraser.GetBrushArea(FMarqueeX, FMarqueeY);
            LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);
    
            ShowSelectionAtBrushStroke(LBrushArea);
            FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  with FChannelManager.SelectedAlphaChannel do
                  begin
                    EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, LInterval,
                               ChannelLayer.Bitmap, [csGrayscale]);

                    // get refresh area
                    LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                    LBrushArea      := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);
                    LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                    ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  with FChannelManager.QuickMaskChannel do
                  begin
                    EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                               FXActual, FYActual, LInterval,
                               ChannelLayer.Bitmap, [csGrayscale]);

                    // get refresh area
                    LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                    LBrushArea      := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);
                    LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                    ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;
                end;

              ctLayerMaskChannel:
                begin
                  // EraserLineOnMask() will paint eraser stroke both on
                  // FLayerList.SelectedLayer.MaskBitmap and
                  // FChannelManagerMini.LayerMaskChannel.ChannelLayer.Bitmap.

                  EraserLineOnMask(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                                   FXActual, FYActual, LInterval, [csGrayscale]);

                  // get brush area
                  LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  // refresh the view
                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  // must be on layer

                  // get refresh area
                  LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                  LBrushArea      := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);
                  LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                  EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                             FXActual, FYActual, LInterval,
                             FLayerList.SelectedLayer.LayerBitmap,
                             FChannelManager.SelectedColorChannels);

                  // refresh the view
                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;

            FPrevStrokePoint := Point(FXActual, FYActual);
          end;
        end;

      etBackgroundEraser:
        begin
          if Assigned(frmMain.GMBrush) and
             (frmMain.GMEraser.BrushID = bidBackgroundEraser) then
          begin
            if Assigned(frmPaintingBrush.EraserStroke) then
            begin
              LBackEraser := TgmBackgroundEraser(frmMain.GMEraser);

              if Assigned(FSelection) then
              begin
                // Sampling Color
                if frmMain.EraserSamplingMode = bsmContiguous then
                begin
                  LBackEraser.SamplingColor(FMarqueeX, FMarqueeY);
                end;

                case frmMain.BackgroundEraserLimit of
                  belDiscontiguous:
                    begin
                      EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                                 FMarqueeX, FMarqueeY, 0,
                                 FSelection.CutOriginal,
                                 FChannelManager.SelectedColorChannels);
                    end;

                  belContiguous:
                    begin
                      EraserLine(FPrevStrokePoint.X, FPrevStrokePoint.Y,
                                 FMarqueeX, FMarqueeY,
                                 frmMain.ErasingInterval,
                                 FSelection.CutOriginal,
                                 FChannelManager.SelectedColorChannels);
                    end;
                end;

                // get brush area
                LLastStrokeArea := LBackEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                LBrushArea      := LBackEraser.GetBrushArea(FMarqueeX, FMarqueeY);
                LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                ShowSelectionAtBrushStroke(LBrushArea);
                FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
              end
              else
              begin
                // on normal layers...

                // get refresh area
                LLastStrokeArea := frmMain.GMEraser.GetBrushArea(FPrevStrokePoint.X, FPrevStrokePoint.Y);
                LBrushArea      := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);
                LBrushArea      := AddRects(LLastStrokeArea, LBrushArea);

                // Sampling Color
                if frmMain.EraserSamplingMode = bsmContiguous then
                begin
                  LBackEraser.SamplingColor(FXActual, FYActual);
                end;

                case frmMain.BackgroundEraserLimit of
                  belDiscontiguous:
                    begin
                      EraserLine(
                        FPrevStrokePoint.X, FPrevStrokePoint.Y,
                        FXActual, FYActual, 0,
                        FLayerList.SelectedLayer.LayerBitmap,
                        FChannelManager.SelectedColorChannels);
                    end;

                  belContiguous:
                    begin
                      EraserLine(
                        FPrevStrokePoint.X, FPrevStrokePoint.Y,
                        FXActual, FYActual, frmMain.ErasingInterval,
                        FLayerList.SelectedLayer.LayerBitmap,
                        FChannelManager.SelectedColorChannels);
                    end;
                end;

                FLayerList.SelectedLayer.Changed(LBrushArea);

                FPrevStrokePoint := Point(FXActual, FYActual);
              end;

              // show sampling color
              if frmMain.EraserSamplingMode = bsmContiguous then
              begin
                r := LBackEraser.SampledColor shr 16 and $FF;
                g := LBackEraser.SampledColor shr  8 and $FF;
                b := LBackEraser.SampledColor        and $FF;

                frmColor.ChangeColorViaTrackBar(r, g, b);
              end;
            end;
          end;
        end;
    end;
  end
  else // if the FDrawing = False
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and 
       (FXActual < FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual < FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.EraserMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint              : TPoint;
  LCommand            : TgmCustomCommand;
  LCommandName        : string;
  LCommandIconResName : string;
begin
  LCommand := nil;

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  // get selection space coordinates
  CalcSelectionCoord();

// Move mouse when mouse left button down

  if FDrawing then
  begin
    FDrawing := False;

    if tmrSpecialErasers.Enabled then
    begin
      tmrSpecialErasers.Enabled := False;
    end;

    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated := True;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    if frmMain.EraserTool = etBackgroundEraser then
    begin
      frmColor.CurrColorSelector := frmColor.LastColorSelector;
    end;

    // Undo/Redo
    case frmMain.EraserTool of
      etEraser:
        begin
          LCommandName        := frmMain.GMEraser.Name;
          LCommandIconResName := gmMiscCommandIcons.ERASER_COMMAND_ICON_RES_NAME;
        end;

      etBackgroundEraser:
        begin
          LCommandName        := frmMain.GMEraser.Name;
          LCommandIconResName := gmMiscCommandIcons.BACKGROUND_ERASER_COMMAND_ICON_RES_NAME;
        end;

      etMagicEraser:
        begin
          LCommandName        := 'Magic Eraser';
          LCommandIconResName := gmMiscCommandIcons.MAGIC_ERASER_COMMAND_ICON_RES_NAME;
        end;
    end;

    if Assigned(FSelection) then
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
              LCommandName,
              FChannelManager,
              FChannelManager.AlphaChannelList.SelectedIndex,
              frmMain.FBitmapBefore,
              FSelection.CutOriginal,
              GetSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
              LCommandName,
              FChannelManager,
              frmMain.FBitmapBefore,
              FSelection.CutOriginal,
              GetSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
              LCommandName,
              FChannelManager,
              FLayerList,
              FLayerList.SelectedIndex,
              frmMain.FBitmapBefore,
              FSelection.CutOriginal,
              GetSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
              LCommandName,
              FChannelManager,
              FLayerList,
              FLayerList.SelectedIndex,
              frmMain.FBitmapBefore,
              FSelection.CutOriginal,
              GetSelectionForUndoRedo);
          end;
      end;
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmAlphaChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              FChannelManager.AlphaChannelList,
              FChannelManager.AlphaChannelList.SelectedIndex);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmQuickMaskChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              FChannelManager);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmLayerMaskProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              FLayerList.SelectedLayer.MaskBitmap,
              FLayerList,
              FLayerList.SelectedIndex);
          end;

        ctColorChannel:
          begin
            LCommand := TgmLayerImageProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              FLayerList.SelectedLayer.LayerBitmap,
              FLayerList,
              FLayerList.SelectedIndex);
          end;
      end;
    end;

    if Assigned(LCommand) then
    begin
      LCommand.ChangeCommandIconByResourceName(LCommandIconResName);
      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.PenToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint       : TPoint;
  LTempSegment : TgmCurveSegment;
  LCommand     : TgmCustomCommand;
begin
  // get bitmap space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

// Mouse left button down ...

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    // don't draw dynamic Marching-Ants lines when processing image
    if Assigned(FSelection) and FSelection.IsAnimated then
    begin
      FSelection.IsAnimated := False;
    end;

    // if there is no existed path layer then create one
    if FPathLayer = nil then
    begin
      CreatePathLayer();
    end;

    FDrawing              := True;
    FPathModificationMode := pmmNone;  // for Undo/Redo

    case frmMain.PenTool of
      ptPathComponentSelection:
        begin
          if Assigned(FPathList.SelectedPath) then
          begin
            FAccumTranslateVector := Point(0, 0); // Undo/Redo

            // Note, X and Y are in image control space.
            FWholePathIndex :=
              FPathList.SelectedPath.CurvePathList.IfPointOnPathsForWholePath(
                X, Y, imgWorkArea.BitmapToControl);

            if ssShift in Shift then
            begin
              // if the Shift key is pressed, then select the whole path with the mouse
              if FWholePathIndex > (-1) then
              begin
                FPathList.SelectedPath.CurvePathList.SelectWholePathByIndex(FWholePathIndex);
              end;
            end
            else
            begin  // if the Shift key is not pressed...
              // If the mouse is clicked on any path then select it,
              // otherwise, deselect all paths. 
              if FWholePathIndex > (-1) then
              begin
                // If the mouse is clicked on an unselected path,
                // then we deselect all paths and only select this path. 
                if not FPathList.SelectedPath.CurvePathList.IfSelectedWholePathByIndex(FWholePathIndex) then
                begin
                  FPathList.SelectedPath.CurvePathList.DeselectAllPaths();
                  FPathList.SelectedPath.CurvePathList.SelectWholePathByIndex(FWholePathIndex);
                end;
              end
              else
              begin
                FPathList.SelectedPath.CurvePathList.DeselectAllPaths();
                FCurvePath := nil;
              end;
            end;

            if FWholePathIndex > (-1) then
            begin
              Screen.Cursor      := crMovePath;
              imgWorkArea.Cursor := crMovePath;
              FDrawingBasePoint  := Point(FXActual, FYActual);

              SaveOldCurvePathForUndoRedo();  // for Undo/Redo
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crPathComponentSelection;
            end;

            // clear the old paths and draw the new paths
            FPathLayer.Bitmap.Clear($00000000);

            FPathList.SelectedPath.CurvePathList.DrawAllPaths(
              FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);

            FPathLayer.Bitmap.Changed();

            // draw the Marching-Ants lines if the select is existed
            if Assigned(FSelection) then
            begin
              imgWorkArea.Update();
              FSelection.DrawMarchingAnts();
            end;
          end;
        end;

      ptDirectSelection:
        begin
          if Assigned(FPathList.SelectedPath) then
          begin
            // for Undo/Redo
            FAccumTranslateVector := Point(0, 0);
            SaveOldCurvePathForUndoRedo();

            FPathLayer.Bitmap.Clear($00000000);  // clear the paths

            FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
              X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

            if Assigned(FCurvePath) then
            begin
              // save the original status
              FOriginalPairState := FCurvePath.CurveSegmentsList.GetSelectedSegmentsPairState();
              Screen.Cursor      := GetCurvePathCursor(FPathSelectHandle);
              imgWorkArea.Cursor := Screen.Cursor;
              FDrawingBasePoint  := Point(FXActual, FYActual);
            end;

            // draw the new paths
            FPathList.SelectedPath.CurvePathList.DrawAllPaths(
              FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);

            FPathLayer.Bitmap.Changed();

            // draw the Marching-Ants lines if the select is existed
            if Assigned(FSelection) then
            begin
              imgWorkArea.Update();
              FSelection.DrawMarchingAnts();
            end;

            // for Undo/Redo
            if FPathSelectHandle in [pshStart, pshEnd] then
            begin
              FPathModificationMode := pmmDragAnchorPoint;
            end
            else if FPathSelectHandle in [pshControl1,
                                          pshControl2,
                                          pshOpposite1,
                                          pshOpposite2] then
            begin
              FPathModificationMode := pmmDragControlPoint;
            end;
          end;
        end;

      ptPenTool:
        begin
          // If the current selected path is a closed path, and the mouse is not
          // clicked on it or on the endpoints of it, then set the state of the
          // path list to cplsAddNewPath for adding a path.
          if Assigned(FCurvePath) then
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if FPathList.SelectedPath.CurvePathList.Status = cplsAdjust then
              begin
                if ( FCurvePath.CurveSegmentsList.GetSelectedEndingHandle(
                       X, Y, imgWorkArea.BitmapToControl) = pshNone ) and
                   ( not FCurvePath.CurveSegmentsList.NilsIfPointOnBezier(
                       X, Y, imgWorkArea.BitmapToControl) ) then
                begin
                  FPathList.SelectedPath.CurvePathList.Status := cplsAddNewPath;
                end;
              end;
            end;
          end;

          // If there is no selected path panel, or the state of the path list
          // is cplsAddNewPath, then niling the FCurvePath field. 
          if (FPathList.SelectedPath = nil) or
             (FPathList.SelectedPath.CurvePathList.Status = cplsAddNewPath) then
          begin
            FCurvePath := nil;
          end;

          // if FCurvePath is nil, then create a new path
          if FCurvePath = nil then
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              FPathList.SelectedPath.CurvePathList.DeselectAllPaths();
            end;
            
            FCurvePath := TgmCurvePath.Create();
          end;

          // If the state of the path list is plsAddNewPath, then adding the
          // path FCurvePath to the list, and set the state of the list to
          // cplsAddNextAnchorPoint.
          if Assigned(FPathList.SelectedPath) and
             (FPathList.SelectedPath.CurvePathList.Status = cplsAddNewPath) then
          begin
            // for Undo/Redo
            FPathModificationMode := pmmNewPathComponent;
            SaveOldCurvePathForUndoRedo();

            FPathList.SelectedPath.CurvePathList.AddNewPathToList(FCurvePath);
          end;

          FPathLayer.Bitmap.Clear($00000000); // clear the old paths

          with FCurvePath.CurveSegmentsList do
          begin
            // if there is no any curve segment in the list...
            if Count = 0 then
            begin
              AddFirstSegment( Point(FXActual, FYActual),
                               Point(FXActual, FYActual),
                               Point(FXActual, FYActual),
                               Point(FXActual, FYActual) );

              imgWorkArea.Cursor := crMovePath;
            end
            else if Count > 0 then
            begin
              if IsFirstSegmentOK = False then
              begin
                // If the Alt key is pressed, and the mouse is pointing on the
                // starting point of the first curve segment, then mark up the
                // starting point as a Corner Point. 
                if ssAlt in Shift then
                begin
                  if PointOnFirstSegmentStart(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    SaveOldCurvePathForUndoRedo();  // for Undo/Redo

                    CurrentSegment.Opposite1         := CurrentSegment.Control1;
                    CurrentSegment.Control1          := CurrentSegment.StartPoint;
                    CurrentSegment.StartingPointType := eptCornerPoint;

                    // Undo/Redo
                    LCommand := TgmPathModificationCommand.Create(
                      FPathList, FPathList.SelectedIndex, FOldCurvePathList,
                      FPathList.SelectedPath.CurvePathList, pmmPickupPath,
                      FLayerList.SelectedLayer.LayerBitmap.Width,
                      FLayerList.SelectedLayer.LayerBitmap.Height);

                    FCommandManager.AddCommand(LCommand);

                    FDrawing := False;
                  end;
                end
                else
                begin
                  // If the mouse is pointing on the starting point of the first
                  // curve segment of the path, then switch the pen tool to
                  // Direct Selection tool. 
                  if PointOnFirstSegmentStart(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    // for Undo/Redo
                    Self.SaveOldCurvePathForUndoRedo();
                    FPathModificationMode := pmmPickupPath;

                    CurrentSegment.Control1 := CurrentSegment.StartPoint;
                    FPathSelectHandle       := pshControl1;
                    Screen.Cursor           := crMovePath;
                    imgWorkArea.Cursor      := crMovePath;
                    FDrawingBasePoint       := Point(FXActual, FYActual);
                    frmMain.PenTool         := ptDirectSelection;
                  end
                  else
                  begin
                    // for Undo/Redo
                    FPathModificationMode := pmmNewAnchorPoint;
                    SaveOldCurvePathForUndoRedo();

                    // specify that we want to modify the end point of the
                    // first curve segment
                    CurrentSegment.ActivePoint := apEnd;
                    IsFirstSegmentOK           := True;

                    // If the Alt key has not been pressed, then we modify the
                    // end point, control point 2 and opposite control point 2
                    // of the current curve segment.
                    if CurrentSegment.ActivePoint = apEnd then
                    begin
                      CurrentSegment.Control2  := Point(FXActual, FYActual);
                      CurrentSegment.Opposite2 := Point(FXActual, FYActual);
                      CurrentSegment.EndPoint  := Point(FXActual, FYActual);
                    end;

                    Screen.Cursor      := crMovePath;
                    imgWorkArea.Cursor := crMovePath;
                  end;
                end;
              end
              else
              begin
                // if the mouse is pointing on any of endpoints of the
                // curve segments...
                if GetSelectedEndingHandle(X, Y, imgWorkArea.BitmapToControl) <> pshNone then
                begin
                  // If the mouse is pointing on the starting point of the first
                  // curve segment of a path, then close the path. 
                  if PointOnFirstSegmentStart(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    if FCurvePath.CurveSegmentsList.IsClosed then
                    begin
                      // If the Alt is pressed, then convert the endpoint of the
                      // curve segment from Anchor Point to Corner Point,
                      // and then switch the pen tools to Convert Point tool. 
                      if ssAlt in Shift then
                      begin
                        // for Undo/Redo
                        Self.SaveOldCurvePathForUndoRedo();
                        FPathModificationMode := pmmChangeAnchorPoint;

                        FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
                          X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

                        FCurvePath.CurveSegmentsList.ConvertPoint(X, Y,
                          coAnchorToCorner, imgWorkArea.BitmapToControl);

                        FMouseDownX        := X;  // FMouseDownX should be in image control space
                        FMouseDownY        := Y;  // FMouseDownY should be in image control space
                        Screen.Cursor      := crMovePath;
                        imgWorkArea.Cursor := crMovePath;
                        FDrawingBasePoint  := Point(FXActual, FYActual);
                        frmMain.PenTool    := ptConvertPoint;
                      end
                      else
                      begin
                        // for Undo/Redo
                        Self.SaveOldCurvePathForUndoRedo();
                        FPathModificationMode := pmmDeleteAnchorPoint;

                        // if the Alt key is not pressed then delete the anchor point
                        DeleteAnchorPointOnSegment(X, Y, imgWorkArea.BitmapToControl);

                        Screen.Cursor      := crDefault;
                        imgWorkArea.Cursor := GetPenToolDefaultCursor();
                      end;
                    end
                    else
                    begin
                      Self.SaveOldCurvePathForUndoRedo(); // for Undo/Redo

                      ClosePath();
                      FCurvePath := nil;
                      FPathList.SelectedPath.CurvePathList.Status := cplsAddNewPath;

                      FPathLayer.Bitmap.Clear($00000000); // clear the old paths

                      if Assigned(FSelection) then
                      begin
                        FSelection.IsAnimated := True;
                      end;

                      if Assigned(FPathList.SelectedPath) then
                      begin
                        FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                          FPathLayer.Bitmap.Canvas, pmNotXor,
                          imgWorkArea.BitmapToControl);
                      end
                      else
                      begin
                        DrawCurveSegments(FPathLayer.Bitmap.Canvas, pmNotXor,
                          imgWorkArea.BitmapToControl);
                      end;

                      FPathLayer.Bitmap.Changed();
                      
                      FDrawing           := False;
                      Screen.Cursor      := crDefault;
                      imgWorkArea.Cursor := GetPenToolDefaultCursor();

                      // If there is selected curve segment then calculate the
                      // radian and length of it. 
                      if Assigned(FCurvePath) then
                      begin
                        FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
                      end;

                      FPathList.SelectedPath.UpdateThumbnail(
                        FLayerList.SelectedLayer.LayerBitmap.Width,
                        FLayerList.SelectedLayer.LayerBitmap.Height,
                        Point(0, 0));

                      // Undo/Redo
                      LCommand := TgmClosePathCommand.Create(FPathList,
                        FPathList.SelectedIndex, FOldCurvePathList,
                        FLayerList.SelectedLayer.LayerBitmap.Width,
                        FLayerList.SelectedLayer.LayerBitmap.Height);

                      FCommandManager.AddCommand(LCommand);

                      Exit;
                    end;
                  end
                  else
                  begin
                    // if the mouse is pointing on the end point of the
                    // last curve segment of the path...
                    LTempSegment := PointOnLastSegmentEndingPoint(X, Y,
                      imgWorkArea.BitmapToControl);

                    if Assigned(LTempSegment) then
                    begin
                      // if the Alt key is pressed, then change the anchor
                      // point to corner point
                      if ssAlt in Shift then
                      begin
                        // for Undo/Redo
                        Self.SaveOldCurvePathForUndoRedo();
                        FPathModificationMode := pmmPickupPath;

                        // If the mouse is clicked on the end point of the last
                        // curve segment of the path with the Alt key is
                        // pressed, and the last curve segment is not selected,
                        // then select it and activate the end point of it. 
                        if (CurrentIndex <> Count - 1) or
                           (CurrentSegment.ActivePoint <> apEnd) then
                        begin
                          FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
                            X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);
                            
                          LTempSegment := FCurvePath.CurveSegmentsList.CurrentSegment;
                        end;

                        // convert to convert point
                        LTempSegment.EndingPointType := eptCornerPoint;
                        LTempSegment.Opposite2       := LTempSegment.EndPoint;
                      end
                      else
                      begin
                        if (CurrentIndex <> Count - 1) or
                           (CurrentSegment.ActivePoint <> apEnd) then
                        begin
                          FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
                            X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl)
                        end;

                        // for Undo/Redo
                        Self.SaveOldCurvePathForUndoRedo();
                        FPathModificationMode := pmmPickupPath;

                        CurrentSegment.Opposite2 := CurrentSegment.EndPoint;
                        FPathSelectHandle        := pshOpposite2;
                        Screen.Cursor            := crMovePath;
                        imgWorkArea.Cursor       := crMovePath;
                        FDrawingBasePoint        := Point(FXActual, FYActual);
                        frmMain.PenTool          := ptDirectSelection; // switch to Direct Selection tool
                      end;
                    end
                    else 
                    begin
                      // If the mouse is not point on neither the starting point
                      // of the first curve segment nor the end point of the
                      // last curve segment of the path... 

                      // If the Alt key is pressed, then convert the anchor
                      // point to corner point and switch the pen tool to
                      // Convert Point tool. 
                      if ssAlt in Shift then
                      begin
                        Self.SaveOldCurvePathForUndoRedo(); // for Undo/Redo

                        FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
                          X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

                        FCurvePath.CurveSegmentsList.ConvertPoint(X, Y,
                          coAnchorToCorner, imgWorkArea.BitmapToControl);

                        FMouseDownX        := X; // FMouseDownX should be in image control space
                        FMouseDownY        := Y; // FMouseDownY should be in image control space
                        Screen.Cursor      := crMovePath;
                        imgWorkArea.Cursor := crMovePath;
                        FDrawingBasePoint  := Point(FXActual, FYActual);
                        frmMain.PenTool    := ptConvertPoint;

                        // for Undo/Redo
                        if FPathSelectHandle in [pshStart, pshEnd] then
                        begin
                          FPathModificationMode := pmmChangeAnchorPoint;
                        end
                        else if FPathSelectHandle in [pshControl1,
                                                      pshOpposite1,
                                                      pshControl2,
                                                      pshOpposite2] then
                        begin
                          FPathModificationMode := pmmCornerDrag;
                        end;
                      end
                      else
                      begin
                        // for Undo/Redo
                        Self.SaveOldCurvePathForUndoRedo();
                        FPathModificationMode := pmmDeleteAnchorPoint;

                        // if the Alt key is not pressed, then delete the
                        // anchor point
                        DeleteAnchorPointOnSegment(X, Y, imgWorkArea.BitmapToControl);

                        Screen.Cursor      := crDefault;
                        imgWorkArea.Cursor := GetPenToolDefaultCursor;
                      end;
                    end;
                  end;
                end
                else  // if the mouse is not pointing on any of the endpoints of a path...
                begin
                  // if the mouse is pointing on the path...
                  if NilsIfPointOnBezier(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    // for Undo/Redo
                    Self.SaveOldCurvePathForUndoRedo();
                    FPathModificationMode := pmmAddAnchorPoint;

                    // add anchor point
                    if FCurvePath.CurveSegmentsList.AddAnchorPointOnSegment(
                         X, Y,
                         imgWorkArea.BitmapToControl,
                         imgWorkArea.ControlToBitmap) then
                    begin
                      // compute the radian and length for the selected
                      // curve segment
                      FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
                      
                      // After the anchor point is added to the list, at the
                      // same time, if the user move the anchor point, then
                      // specify that we want to modify the opposite control
                      // point 2 of the anchor point in the mouse move event. 
                      FPathSelectHandle  := pshOpposite2;
                      frmMain.PenTool    := ptAddAnchorPoint;  // switch to Add Anchor Point tool
                      Screen.Cursor      := crMovePath;
                      imgWorkArea.Cursor := crMovePath;
                    end;
                  end
                  else
                  begin
                    if not IsClosed then
                    begin
                      // for Undo/Redo
                      FPathModificationMode := pmmNewAnchorPoint;
                      SaveOldCurvePathForUndoRedo();

                      AddFollowSegment( Point(FXActual, FYActual),
                                        Point(FXActual, FYActual) );

                      Screen.Cursor      := crMovePath;
                      imgWorkArea.Cursor := crMovePath;
                    end
                    else
                    begin
                      FDrawing := False;
                    end;
                  end;
                end;
              end;
            end;

            // draw curve segments of the path
            if Assigned(FPathList.SelectedPath) then
            begin
              FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
            end
            else
            begin
              if Count > 0 then
              begin
                DrawCurveDirectionLines(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                DrawCurveSegments(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                DrawCurveHandles(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
              end;
            end;

            FPathLayer.Bitmap.Changed();
          end;

          // draw the Marching-Ants lines if the selection is existed
          if Assigned(FSelection) then
          begin
            imgWorkArea.Update;
            FSelection.DrawMarchingAnts();
          end;
        end;

      ptAddAnchorPoint:
        begin
          if Assigned(FPathList.SelectedPath) then
          begin
            FPathLayer.Bitmap.Clear($00000000);  // clear path

            // select a path
            FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
              X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

            // If the mouse is pointing on a handle then move the handle; if the
            // mouse is pointing on a curve segment then add an anchor point to
            // the curve segment. 
            if FPathSelectHandle <> pshNone then
            begin
              if Assigned(FCurvePath) then
              begin
                // for Undo/Redo
                FAccumTranslateVector := Point(0, 0);
                SaveOldCurvePathForUndoRedo();

                if FPathSelectHandle in [pshStart, pshEnd] then
                begin
                  FPathModificationMode := pmmDragAnchorPoint;
                end
                else if FPathSelectHandle in [pshControl1,
                                              pshControl2,
                                              pshOpposite1,
                                              pshOpposite2] then
                begin
                  FPathModificationMode := pmmDragControlPoint;
                end;

                Screen.Cursor      := crMovePath;
                imgWorkArea.Cursor := crMovePath;
                FDrawingBasePoint  := Point(FXActual, FYActual);
                FOriginalPairState := FCurvePath.CurveSegmentsList.GetSelectedSegmentsPairState(); // save the original status
                frmMain.PenTool    := ptDirectSelection; // convert to Direct Selection tool
              end;
            end
            else
            begin
              if Assigned(FCurvePath) then
              begin
                with FCurvePath.CurveSegmentsList do
                begin
                  if Count > 0 then
                  begin
                    if IsFirstSegmentOK then
                    begin
                      // for Undo/Redo
                      SaveOldCurvePathForUndoRedo();
                      FPathModificationMode := pmmAddAnchorPoint;

                      // add anchor point
                      if FCurvePath.CurveSegmentsList.AddAnchorPointOnSegment(
                           X, Y,
                           imgWorkArea.BitmapToControl,
                           imgWorkArea.ControlToBitmap) then
                      begin
                        FPathSelectHandle := pshOpposite2;
                        FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
                      end;
                    end;
                  end;
                end;
              end;
            end;

            if FPathSelectHandle <> pshNone then
            begin
              Screen.Cursor      := GetCurvePathCursor(FPathSelectHandle);
              imgWorkArea.Cursor := Screen.Cursor;
              FDrawingBasePoint  := Point(FXActual, FYActual);
            end;

            FPathList.SelectedPath.CurvePathList.DrawAllPaths(
              FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);

            FPathLayer.Bitmap.Changed();

            if Assigned(FSelection) then
            begin
              imgWorkArea.Update();
              FSelection.DrawMarchingAnts();
            end;
          end;
        end;

      ptDeleteAnchorPoint:
        begin
          if Assigned(FPathList.SelectedPath) then
          begin
            FPathLayer.Bitmap.Clear($00000000);  // clear paths

            // select a curve path
            FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
              X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                if Count > 0 then
                begin
                  // if the mouse is pointing on any of the endpoints of
                  // the path, then delete it
                  if FPathSelectHandle in [pshStart, pshEnd] then
                  begin
                    // for Undo/Redo
                    SaveOldCurvePathForUndoRedo();
                    FPathModificationMode := pmmDeleteAnchorPoint;

                    DeleteAnchorPointOnSegment(X, Y, imgWorkArea.BitmapToControl);

                    Screen.Cursor      := crDefault;
                    imgWorkArea.Cursor := crDirectSelection;
                  end;
                end;
              end;

              if FPathSelectHandle in [pshControl1,
                                       pshOpposite1,
                                       pshControl2,
                                       pshOpposite2] then
              begin
                // for Undo/Redo
                SaveOldCurvePathForUndoRedo();
                FAccumTranslateVector := Point(0, 0);
                FPathModificationMode := pmmDragControlPoint;

                Screen.Cursor      := GetCurvePathCursor(FPathSelectHandle);
                imgWorkArea.Cursor := Screen.Cursor;
                FDrawingBasePoint  := Point(FXActual, FYActual);
                FOriginalPairState := FCurvePath.CurveSegmentsList.GetSelectedSegmentsPairState();  // save the original status
                frmMain.PenTool    := ptDirectSelection;  // convert to Direct Selection tool
              end;
            end;

            FPathList.SelectedPath.CurvePathList.DrawAllPaths(
              FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);

            FPathLayer.Bitmap.Changed();

            if Assigned(FSelection) then
            begin
              imgWorkArea.Update();
              FSelection.DrawMarchingAnts();
            end;
          end;
        end;

      ptConvertPoint:
        begin
          if Assigned(FPathList.SelectedPath) then
          begin
            FPathLayer.Bitmap.Clear($00000000);  // clear paths

            // select a path
            FCurvePath := FPathList.SelectedPath.CurvePathList.SelectPath(
              X, Y, FPathSelectHandle, imgWorkArea.BitmapToControl);

            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                if Count > 0 then
                begin
                  if FPathSelectHandle in [pshStart, pshEnd] then
                  begin
                    // for Undo/Redo
                    SaveOldCurvePathForUndoRedo();
                    FPathModificationMode := pmmChangeAnchorPoint;

                    FCurvePath.CurveSegmentsList.ConvertPoint(X, Y,
                      coAnchorToCorner, imgWorkArea.BitmapToControl);

                    FOppositeLineOperation := oloAbsoluteOpposite;
                    FMouseDownX            := X; // FMouseDownX should be in image control space
                    FMouseDownY            := Y; // FMouseDownY should be in image control space
                  end
                  else
                  begin
                    FOppositeLineOperation := oloChangeAngleOnly;
                  end;
                end;
              end;

              if FPathSelectHandle <> pshNone then
              begin
                Screen.Cursor     := GetCurvePathCursor(FPathSelectHandle);
                FDrawingBasePoint := Point(FXActual, FYActual);

                if FPathSelectHandle in [pshControl1,
                                         pshOpposite1,
                                         pshControl2,
                                         pshOpposite2] then
                begin
                  // for Undo/Redo
                  SaveOldCurvePathForUndoRedo();
                  FPathModificationMode := pmmCornerDrag;
                  FAccumTranslateVector := Point(0, 0);

                  FCurvePath.CurveSegmentsList.ChangeDirectionLinesPairState(psUnpaired, True);
                end;
              end;
            end;

            FPathList.SelectedPath.CurvePathList.DrawAllPaths(
              FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);

            FPathLayer.Bitmap.Changed();

            if Assigned(FSelection) then
            begin
              imgWorkArea.Update();
              FSelection.DrawMarchingAnts();
            end;
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.PenToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint           : TPoint;
  LTranslateVector : TPoint;
  LPairState       : TgmPairState;
  LColor           : TColor;
begin
  // get bitmap space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
// Move mouse when mouse left button down ...

  if FDrawing then
  begin
    // Change cursor
    if Screen.Cursor <> crMovePath then
    begin
      Screen.Cursor      := crMovePath;
      imgWorkArea.Cursor := crMovePath;
    end;

    if Assigned(FPathLayer) then
    begin
      case frmMain.PenTool of
        ptPathComponentSelection:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if FWholePathIndex > (-1) then
              begin
                with FPathList.SelectedPath.CurvePathList do
                begin
                  // clear the old paths
                  DrawAllPaths(FPathLayer.Bitmap.Canvas, pmNotXor,
                               imgWorkArea.BitmapToControl);

                  // translate the selected paths
                  LTranslateVector      := SubtractPoints( Point(FXActual, FYActual), FDrawingBasePoint );
                  FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);  // for Undo/Redo

                  TranslateAllSelectedPaths(LTranslateVector);

                  // draw the new paths
                  DrawAllPaths( FPathLayer.Bitmap.Canvas, pmNotXor,
                                imgWorkArea.BitmapToControl);

                  FPathLayer.Bitmap.Changed;

                  FDrawingBasePoint := Point(FXActual, FYActual);
                end;
              end;
            end;
          end;

        ptDirectSelection:
          begin
            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                if FPathSelectHandle <> pshNone then
                begin
                  if ssAlt in Shift then
                  begin
                    // If the pair state of the direction lines of the
                    // curve segment is changed, then we set the two direction
                    // lines are in paired state, otherwise in unpaired state. 
                    if IsPairStateChanged() then
                    begin
                      LPairState := psPaired;
                    end
                    else
                    begin
                      LPairState := psUnpaired;
                    end;

                    if GetSelectedSegmentsPairState() <> LPairState then
                    begin
                      ChangeDirectionLinesPairState(LPairState, False);
                    end;
                  end
                  else
                  begin
                    // If moving the mouse with the Alt key released, then
                    // change the pair state of the two direction lines back to
                    // the original state. 
                    if GetSelectedSegmentsPairState() <> FOriginalPairState then
                    begin
                      ChangeDirectionLinesPairState(FOriginalPairState, False);
                    end;
                  end;

                  // clear the old paths
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);

                  // change the curve segment handle position
                  LTranslateVector      := SubtractPoints( Point(FXActual, FYActual), FDrawingBasePoint );
                  FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);  // for Undo/Redo

                  ChangeSelectedHandlePosition(FPathSelectHandle,
                                               LTranslateVector,
                                               oloChangeAngleOnly);

                  // draw the new paths
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);

                  FPathLayer.Bitmap.Changed();

                  FDrawingBasePoint := Point(FXActual, FYActual);
                end;
              end;
            end;
          end;

        ptPenTool:
          begin
            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                if Assigned(FPathList.SelectedPath) then
                begin
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);
                end;

                if Count > 0 then
                begin
                  if FPathList.SelectedPath = nil then
                  begin
                    // clear the old direction lines
                    DrawCurveDirectionLines(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                    DrawCurveSegments(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                    DrawCurveHandles(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                  end;

                  if IsFirstSegmentOK = False then
                  begin
                    if Assigned(CurrentSegment) then
                    begin
                      CurrentSegment.Control1 := Point(FXActual, FYActual);

                      CurrentSegment.Opposite1 := GetOppositePoint(
                        CurrentSegment.StartPoint, CurrentSegment.Control1);
                    end;
                  end
                  else
                  begin
                    if Assigned(CurrentSegment) then
                    begin
                      case CurrentSegment.ActivePoint of
                        apStart:
                          begin
                            CurrentSegment.Control1  := Point(FXActual, FYActual);
                            CurrentSegment.Control2  := Point(FXActual, FYActual);
                            CurrentSegment.EndPoint  := Point(FXActual, FYActual);

                            CurrentSegment.Opposite1 := GetOppositePoint(
                              CurrentSegment.StartPoint, CurrentSegment.Control1);
                          end;

                        apEnd:
                          begin
                            CurrentSegment.Opposite2 := Point(FXActual, FYActual);

                            CurrentSegment.Control2 := GetOppositePoint(
                              CurrentSegment.EndPoint, CurrentSegment.Opposite2);
                          end;
                      end;
                    end;
                  end;

                  if Assigned(FPathList.SelectedPath) then
                  begin
                    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                      FPathLayer.Bitmap.Canvas, pmNotXor,
                      imgWorkArea.BitmapToControl);
                  end
                  else
                  begin
                    // draw the new direction lines
                    DrawCurveDirectionLines(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                    DrawCurveSegments(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                    DrawCurveHandles(FPathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                  end;
                end;
                
                FPathLayer.Bitmap.Changed();
              end;
            end;
          end;

        ptAddAnchorPoint:
          begin
            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                if FPathSelectHandle <> pshNone then
                begin
                  // clear the old paths
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);

                  // if the user add an anchor point to the path and
                  // simultaneously move the mouse...
                  if FPathSelectHandle = pshOpposite2 then
                  begin
                    if DifferentCoordinate( CurrentSegment.Opposite2,
                                            Point(FXActual, FYActual) ) then
                    begin
                      CurrentSegment.Opposite2 := Point(FXActual, FYActual);
                    end;
                  end;

                  // change curve segment handle position
                  LTranslateVector := SubtractPoints( Point(FXActual, FYActual),
                                                      FDrawingBasePoint );

                  ChangeSelectedHandlePosition(FPathSelectHandle,
                                               LTranslateVector,
                                               oloChangeAngleOnly);

                  // draw the new paths
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);

                  FPathLayer.Bitmap.Changed();

                  FDrawingBasePoint := Point(FXActual, FYActual);
                end;
              end;
            end;
          end;

        ptDeleteAnchorPoint:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if Assigned(FCurvePath) then
              begin
                if FPathSelectHandle in [pshControl1,
                                         pshOpposite1,
                                         pshControl2,
                                         pshOpposite2] then
                begin
                  with FCurvePath.CurveSegmentsList do
                  begin
                    // clear the old paths
                    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                      FPathLayer.Bitmap.Canvas, pmNotXor,
                      imgWorkArea.BitmapToControl);

                    // change handle position
                    LTranslateVector := SubtractPoints(
                      Point(FXActual, FYActual), FDrawingBasePoint );

                    ChangeSelectedHandlePosition(FPathSelectHandle,
                                                 LTranslateVector,
                                                 oloAbsoluteOpposite);

                    // draw the paths
                    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                      FPathLayer.Bitmap.Canvas, pmNotXor,
                      imgWorkArea.BitmapToControl);

                    FPathLayer.Bitmap.Changed();

                    FDrawingBasePoint := Point(FXActual, FYActual);
                  end;
                end;
              end;
            end;
          end;

        ptConvertPoint:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if Assigned(FCurvePath) then
              begin
                if FPathSelectHandle <> pshNone then
                begin
                  // change the endpoint of the curve segment from
                  // corner point to anchor point
                  if FPathSelectHandle in [pshStart, pshEnd] then
                  begin
                    FCurvePath.CurveSegmentsList.ConvertPoint(
                      FMouseDownX, FMouseDownY, coCornerToAnchor,
                      imgWorkArea.BitmapToControl);
                  end;

                  case FPathSelectHandle of
                    pshStart:
                      begin
                        if FCurvePath.CurveSegmentsList.IsClosed then
                        begin
                          FPathSelectHandle := pshOpposite2;
                        end
                        else
                        begin
                          FPathSelectHandle := pshControl1;
                        end;
                      end;

                    pshEnd:
                      begin
                        FPathSelectHandle := pshOpposite2;
                      end;
                  end;

                  with FCurvePath.CurveSegmentsList do
                  begin
                    // clear the old paths
                    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                      FPathLayer.Bitmap.Canvas, pmNotXor,
                      imgWorkArea.BitmapToControl);

                    // change curve segment handle position
                    LTranslateVector      := SubtractPoints( Point(FXActual, FYActual), FDrawingBasePoint );
                    FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);  // for Undo/Redo

                    ChangeSelectedHandlePosition(FPathSelectHandle,
                                                 LTranslateVector,
                                                 FOppositeLineOperation);

                    // draw the new paths
                    FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                      FPathLayer.Bitmap.Canvas, pmNotXor,
                      imgWorkArea.BitmapToControl);

                    FDrawingBasePoint := Point(FXActual, FYActual);
                  end;
                end;
              end;
            end;
          end;
      end;

      if Assigned(FSelection) then
      begin
        imgWorkArea.Update();
        FSelection.DrawMarchingAnts();
      end;
    end;
  end
  else // Move Mouse When Mouse Button Not Down
  begin
    // change cursor
    if Assigned(FPathLayer) then
    begin
      case frmMain.PenTool of
        ptPathComponentSelection:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              // Note, X and Y are in image control space.
              if FPathList.SelectedPath.CurvePathList.IfPointOnPathsForWholePath(
                   X, Y, imgWorkArea.BitmapToControl) > (-1) then
              begin
                Screen.Cursor      := crMovePath;
                imgWorkArea.Cursor := crMovePath;
              end
              else
              begin
                Screen.Cursor      := crDefault;
                imgWorkArea.Cursor := crPathComponentSelection;
              end;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crPathComponentSelection;
            end;
          end;

        ptDirectSelection:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              // note, X and Y are in image control space
              Screen.Cursor := FPathList.SelectedPath.CurvePathList.GetCursor(
                X, Y, frmMain.PenTool, imgWorkArea.BitmapToControl);

              imgWorkArea.Cursor := Screen.Cursor;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crDirectSelection;
            end;
          end;

        ptPenTool:
          begin
            if Assigned(FCurvePath) then
            begin
              with FCurvePath.CurveSegmentsList do
              begin
                // if the first curve segment is complete...
                if IsFirstSegmentOK then
                begin
                  // if the mouse is pointing on the endpoints of the path...
                  if GetSelectedEndingHandle(X, Y, imgWorkArea.BitmapToControl) <> pshNone then
                  begin
                    // if the path is not closed...
                    if not IsClosed then
                    begin
                      // if the mouse is on the starting point of the
                      // first curve segment...
                      if PointOnFirstSegmentStart(X, Y, imgWorkArea.BitmapToControl) then
                      begin
                        Screen.Cursor      := crClosePath;
                        imgWorkArea.Cursor := crClosePath;
                      end
                      else
                      // if the mouse is on the end point of the last curve segment...
                      if PointOnLastSegmentEndingPoint(X, Y, imgWorkArea.BitmapToControl) <> nil then
                      begin
                        if ssAlt in Shift then
                        begin
                          Screen.Cursor      := crAddCornerPoint;
                          imgWorkArea.Cursor := crAddCornerPoint;
                        end
                        else
                        begin
                          Screen.Cursor      := crPenToolLastEnd;
                          imgWorkArea.Cursor := crPenToolLastEnd;
                        end;
                      end
                      // If the mouse is on any of the endpoints of the path
                      // that neither the first endpoint nor the last endpoint... 
                      else
                      begin
                        if ssAlt in Shift then
                        begin
                          Screen.Cursor      := crConvertPoint;
                          imgWorkArea.Cursor := crConvertPoint;
                        end
                        else
                        begin
                          Screen.Cursor      := crDeleteAnchorPoint;
                          imgWorkArea.Cursor := crDeleteAnchorPoint;
                        end;
                      end;
                    end
                    else  // if the path is closed...
                    begin
                      if ssAlt in Shift then
                      begin
                        Screen.Cursor      := crConvertPoint;
                        imgWorkArea.Cursor := crConvertPoint;
                      end
                      else
                      begin
                        Screen.Cursor      := crDeleteAnchorPoint;
                        imgWorkArea.Cursor := crDeleteAnchorPoint;
                      end;
                    end;
                  end
                  else
                  // if the mouse is on the path ...
                  if NilsIfPointOnBezier(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    Screen.Cursor      := crAddAnchorPoint;
                    imgWorkArea.Cursor := crAddAnchorPoint;
                  end
                  else
                  begin
                    Screen.Cursor      := crDefault;
                    imgWorkArea.Cursor := GetPenToolDefaultCursor();
                  end;
                end
                else  // if the first curve segment is not complete...
                begin
                  // if the mouse is on the starting point of the
                  // first curve segment...
                  if PointOnFirstSegmentStart(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    if ssAlt in Shift then
                    begin
                      Screen.Cursor      := crAddCornerPoint;
                      imgWorkArea.Cursor := crAddCornerPoint;
                    end
                    else
                    begin
                      Screen.Cursor      := crPenToolLastEnd;
                      imgWorkArea.Cursor := crPenToolLastEnd;
                    end;
                  end
                  else
                  begin
                    Screen.Cursor      := crDefault;
                    imgWorkArea.Cursor := GetPenToolDefaultCursor();
                  end;
                end;
              end;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := GetPenToolDefaultCursor();
            end;
          end;

        ptAddAnchorPoint:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if FPathList.SelectedPath.CurvePathList.IfPointOnSelectedPathHandles(
                   X, Y, imgWorkArea.BitmapToControl) then
              begin
                Screen.Cursor      := crMovePath;
                imgWorkArea.Cursor := crMovePath;
              end
              else
              if FPathList.SelectedPath.CurvePathList.IfPointOnPaths(
                   X, Y, imgWorkArea.BitmapToControl) then
              begin
                Screen.Cursor      := crAddAnchorPoint;
                imgWorkArea.Cursor := crAddAnchorPoint;
              end
              else
              begin
                Screen.Cursor      := crDefault;
                imgWorkArea.Cursor := crDirectSelection;
              end;
            end;
          end;

        ptDeleteAnchorPoint:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              with FPathList.SelectedPath.CurvePathList do
              begin
                if Assigned(SelectedPath) then
                begin
                  if SelectedPath.CurveSegmentsList.GetSelectedEndingHandle(
                       X, Y, imgWorkArea.BitmapToControl) <> pshNone then
                  begin
                    Screen.Cursor      := crDeleteAnchorPoint;
                    imgWorkArea.Cursor := crDeleteAnchorPoint;
                  end
                  else
                  if SelectedPath.CurveSegmentsList.GetSelectedDirectionHandle(
                       X, Y, imgWorkArea.BitmapToControl) <> pshNone then
                  begin
                    Screen.Cursor      := crMovePath;
                    imgWorkArea.Cursor := crMovePath;
                  end
                  else
                  begin
                    if IfPointOnPaths(X, Y, imgWorkArea.BitmapToControl) then
                    begin
                      Screen.Cursor      := crHandPoint;
                      imgWorkArea.Cursor := crHandPoint;
                    end
                    else
                    begin
                      Screen.Cursor      := crDefault;
                      imgWorkArea.Cursor := crDirectSelection;
                    end;
                  end;
                end
                else
                begin
                  if IfPointOnPaths(X, Y, imgWorkArea.BitmapToControl) then
                  begin
                    Screen.Cursor      := crHandPoint;
                    imgWorkArea.Cursor := crHandPoint;
                  end
                  else
                  begin
                    Screen.Cursor      := crDefault;
                    imgWorkArea.Cursor := crDirectSelection;
                  end;
                end;
              end;
            end;
          end;

        ptConvertPoint:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              if FPathList.SelectedPath.CurvePathList.IfPointOnSelectedPathHandles(
                   X, Y, imgWorkArea.BitmapToControl) then
              begin
                Screen.Cursor      := crConvertPoint;
                imgWorkArea.Cursor := crConvertPoint;
              end
              else
              if FPathList.SelectedPath.CurvePathList.IfPointOnPaths(
                   X, Y, imgWorkArea.BitmapToControl) then
              begin
                Screen.Cursor      := crHandPoint;
                imgWorkArea.Cursor := crHandPoint;
              end
              else
              begin
                Screen.Cursor      := crDefault;
                imgWorkArea.Cursor := crDirectSelection;
              end;
            end;
          end;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual < FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual < FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.PenToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPath    : TgmPath;
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;

  if FDrawing then
  begin
    Screen.Cursor := crDefault;

    if Assigned(FPathLayer) then
    begin
      case frmMain.PenTool of
        ptPathComponentSelection:
          begin
            if Assigned(FPathList.SelectedPath) then
            begin
              FPathList.SelectedPath.UpdateThumbnail(
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height,
                Point(0, 0) );

              frmPaths.PathManager.Invalidate();
            end;

            // Undo/Redo
            if (FAccumTranslateVector.X <> 0) or
               (FAccumTranslateVector.Y <> 0) then
            begin
              LCommand := TgmPathsTranslationCommand.Create(FPathList,
                FPathList.SelectedIndex, FAccumTranslateVector,
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height);

              FCommandManager.AddCommand(LCommand);
            end;
          end;

        ptDirectSelection:
          begin
            if Assigned(FCurvePath) then
            begin
              // If release the mouse left button with the Alt key is pressed,
              // then change the pair state to psUnpaired and save the settings. 
              if ssAlt in Shift then
              begin
                FCurvePath.CurveSegmentsList.ChangeDirectionLinesPairState(psUnpaired, True);
              end
              else
              begin
                // If the Alt key is not pressed, and the pair state is change,
                // then change the pair state to psUnpaired. }
                if FCurvePath.CurveSegmentsList.IsPairStateChanged() then
                begin
                  FCurvePath.CurveSegmentsList.ChangeDirectionLinesPairState(psUnpaired, True);
                end;
              end;

              // recalculate the radian and length of the curve segment
              FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
            end;

            if Assigned(FPathList.SelectedPath) then
            begin
              FPathList.SelectedPath.UpdateThumbnail(
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height,
                Point(0, 0) );

              frmPaths.PathManager.Invalidate();
            end;

            imgWorkArea.Cursor := crDirectSelection;

            // Undo/Redo
            if (FAccumTranslateVector.X <> 0) or
               (FAccumTranslateVector.Y <> 0) then
            begin
              if FPathModificationMode <> pmmNone then
              begin
                LCommand := TgmPathModificationCommand.Create(FPathList,
                  FPathList.SelectedIndex, FOldCurvePathList,
                  FPathList.SelectedPath.CurvePathList, FPathModificationMode,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);

                FCommandManager.AddCommand(LCommand);
              end;
            end;
          end;

        ptPenTool:
          begin
            // if there is a selected path, then calculate the radian and
            // length of it
            if Assigned(FCurvePath) then
            begin
              FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
            end;

            if FPathList.SelectedPath = nil then
            begin
              if ( FPathList.Count = 0 ) or
                 ( FPathList.IsLastPathNamed() ) then
              begin
                LPath := TgmPath.Create(FPathList, ptWorkPath);

                LPath.CurvePathList.AddNewPathToList(FCurvePath);
                FPathList.Add(LPath);

                // Undo/Redo
                LCommand := TgmNewWorkPathCommand.Create(
                  FPathList, LPath, PathList.Count - 1,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);
              end
              else
              if ( FPathList.Count > 0 ) or
                 ( not FPathList.IsLastPathNamed() ) then
              begin
                FPathList.ActivateWorkPath();

                if Assigned(FPathList.SelectedPath) then
                begin
                  Self.SaveOldCurvePathForUndoRedo(); // for Undo/Redo

                  FPathList.SelectedPath.CurvePathList.DeleteAllCurvePaths();
                  FPathList.SelectedPath.CurvePathList.AddNewPathToList(FCurvePath);

                  // update the path layer
                  FPathLayer.Bitmap.Clear($00000000);
            
                  FPathList.SelectedPath.CurvePathList.DrawAllPaths(
                    FPathLayer.Bitmap.Canvas, pmNotXor,
                    imgWorkArea.BitmapToControl);

                  // Undo/Redo
                  LCommand := TgmActiveWorkPathCommand.Create(
                    FPathList, FOldCurvePathList, FCurvePath,
                    FLayerList.SelectedLayer.LayerBitmap.Width,
                    FLayerList.SelectedLayer.LayerBitmap.Height);
                end;
              end;
            end
            else
            begin
              if FPathList.SelectedPath.CurvePathList.Status = cplsAddNewPath then
              begin
                if Assigned(FCurvePath) then
                begin
                  FPathList.SelectedPath.CurvePathList.AddNewPathToList(FCurvePath);
                end;
              end;

              // Undo/Redo
              if FPathModificationMode <> pmmNone then
              begin
                LCommand := TgmPathModificationCommand.Create(
                  FPathList, FPathList.SelectedIndex, FOldCurvePathList,
                  FPathList.SelectedPath.CurvePathList, FPathModificationMode,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);
              end;
            end;

            if Assigned(FPathList.SelectedPath) then
            begin
              FPathList.SelectedPath.UpdateThumbnail(
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height,
                Point(0, 0) );

              frmPaths.PathManager.Invalidate();
            end;

            // change cursor
            if Assigned(FCurvePath) then
            begin
              if FCurvePath.CurveSegmentsList.GetSelectedEndingHandle(
                   X, Y, imgWorkArea.BitmapToControl) = pshNone then
              begin
                imgWorkArea.Cursor := GetPenToolDefaultCursor();
              end;
            end
            else
            begin
              imgWorkArea.Cursor := GetPenToolDefaultCursor();
            end;

            // Undo/Redo
            if Assigned(LCommand) then
            begin
              FCommandManager.AddCommand(LCommand);
            end;
          end;

        ptAddAnchorPoint,
        ptConvertPoint:
          begin
            FPathSelectHandle := pshNone;

            // if there is a selected path, then calculate the radian and
            // length of it
            if Assigned(FCurvePath) then
            begin
              FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();

              FPathList.SelectedPath.UpdateThumbnail(
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height,
                Point(0, 0) );

              frmPaths.PathManager.Invalidate();
            end;

            // Undo/Redo
            if FPathModificationMode = pmmCornerDrag then
            begin
              if (FAccumTranslateVector.X <> 0) or
                 (FAccumTranslateVector.Y <> 0) then
              begin
                LCommand := TgmPathModificationCommand.Create(FPathList,
                  FPathList.SelectedIndex, FOldCurvePathList,
                  FPathList.SelectedPath.CurvePathList, pmmCornerDrag,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);
              end;
            end
            else
            begin
              if FPathModificationMode <> pmmNone then
              begin
                LCommand := TgmPathModificationCommand.Create(FPathList,
                  FPathList.SelectedIndex, FOldCurvePathList,
                  FPathList.SelectedPath.CurvePathList, FPathModificationMode,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);
              end;
            end;

            if Assigned(LCommand) then
            begin
              FCommandManager.AddCommand(LCommand);
            end;
          end;

        ptDeleteAnchorPoint:
          begin
            FPathSelectHandle := pshNone;

            if Assigned(FPathList.SelectedPath) then
            begin
              // If there is no any curve segment in the selected path after
              // deleting an anchor point, then delete the whole path, and set
              // the path list to Add New Path state. 
              if Assigned(FPathList.SelectedPath.CurvePathList.SelectedPath) then
              begin
                if FPathList.SelectedPath.CurvePathList.SelectedPath.CurveSegmentsList.Count = 0 then
                begin
                  FPathList.SelectedPath.CurvePathList.Delete(
                    FPathList.SelectedPath.CurvePathList.SelectedPathIndex);

                  FPathList.SelectedPath.CurvePathList.SelectedPath      := nil;
                  FCurvePath                                             := nil;
                  FPathList.SelectedPath.CurvePathList.Status            := cplsAddNewPath;
                  FPathList.SelectedPath.CurvePathList.SelectedPathIndex := -1;
                end;
              end;

              FPathList.SelectedPath.UpdateThumbnail(
                FLayerList.SelectedLayer.LayerBitmap.Width,
                FLayerList.SelectedLayer.LayerBitmap.Height,
                Point(0, 0) );

              frmPaths.PathManager.Invalidate();

              // Undo/Redo
              if FPathModificationMode <> pmmNone then
              begin
                LCommand := TgmPathModificationCommand.Create(FPathList,
                  FPathList.SelectedIndex, FOldCurvePathList,
                  FPathList.SelectedPath.CurvePathList, FPathModificationMode,
                  FLayerList.SelectedLayer.LayerBitmap.Width,
                  FLayerList.SelectedLayer.LayerBitmap.Height);

                FCommandManager.AddCommand(LCommand);
              end;
            end;

            // if there is a selected path, then calculate the radian and
            // length of it
            if Assigned(FCurvePath) then
            begin
              FCurvePath.CurveSegmentsList.CalcRadLenForCurveSegments();
            end;
          end;
      end;

      // switch to current selected tool in the tool box
      frmMain.PenTool := frmMain.ActivePenTool;

      if Assigned(FSelection) then
      begin
        FSelection.IsAnimated := True;
      end;
    end;

    FDrawing := False;
  end;
end;

procedure TfrmChild.MeasureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get bitmap space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse Left Button Down }

  if Button = mbLeft then
  begin
    // showing the coordinates of the starting point and current point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    // not drawing dynamic Marching-Ants lines when processing image
    PauseMarchingAnts();

    if Assigned(FMeasureLine) then
    begin
      if ssAlt in Shift then
      begin
        if FMeasureLine.LineCount = 2 then
        begin
          // Note that the X and Y should be in image control coordinate space.
          FMeasurePointSelector := FMeasureLine.GetHandleAtPoint(
            X, Y, imgWorkArea.BitmapToControl);

          if ( FMeasurePointSelector in [mpsFirst, mpsSecond] ) then
          begin
            if FMeasurePointSelector = mpsFirst then
            begin
              FMeasureLine.SwapFirstAndSecondMeasurePoint;
            end;

            FMeasureLine.AddThirdMeasurePoint(FXActual, FYActual);
            FMeasureDrawingState := dsNewFigure;
            Screen.Cursor        := crMeasureMove;

            // update the view
            FMeasureLayer.Bitmap.Clear($FFFFFFFF);
            DrawMeasureLineOnMeasureLayer();
          end;
        end;
      end
      else
      begin
        // Determine wheter the mouse points on any ending points of the
        // measure line.
        // Note that the X and Y should be in image control coordinate space.
        FMeasurePointSelector := FMeasureLine.GetHandleAtPoint(
          X, Y, imgWorkArea.BitmapToControl);

        if FMeasurePointSelector <> mpsNone then
        begin
          FMeasureDrawingState := dsStretchCorner;
          Screen.Cursor        := crMeasureMove;

          // update the view
          FMeasureLayer.Bitmap.Clear($FFFFFFFF);
          DrawMeasureLineOnMeasureLayer();
        end
        else
        begin
          // determine wheter the mouse points on the body of the measure line
          if FMeasureLine.ContainsPoint(X, Y, imgWorkArea.BitmapToControl) then
          begin
            FDrawingBasePoint    := Point(FXActual, FYActual);
            Screen.Cursor        := crMeasureMove;
            FMeasureDrawingState := dsTranslate;

            // update the view
            FMeasureLayer.Bitmap.Clear($FFFFFFFF);
            DrawMeasureLineOnMeasureLayer();
          end
        end;
      end;
    end
    else
    begin
      Screen.Cursor := crMeasureMove;
      FMeasureLine  := TgmMeasureLine.Create;

      CreateMeasureLayer();

      // set two measuring points for the first measure line
      FMeasureLine.SetMeasurePoint(FXActual, FYActual, mpsFirst);
      FMeasureLine.SetMeasurePoint(FXActual, FYActual, mpsSecond);
      DrawMeasureLineOnMeasureLayer();

      FMeasureDrawingState := dsNewFigure;
    end;
    
    ShowMeasureResult;

    FDrawing := True;
  end;
end;

procedure TfrmChild.MeasureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint           : TPoint;
  LNewPoint        : TPoint;
  LTranslateVector : TPoint;
  LColor           : TColor;
begin
  // get bitmap space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;
  
{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    if Assigned(FMeasureLine) then
    begin
      DrawMeasureLineOnMeasureLayer();

      case FMeasureDrawingState of
        dsNewFigure:
          begin
            // change the measure points
            if FMeasureLine.LineCount = 2 then
            begin
              FMeasureLine.SetMeasurePoint(FXActual, FYActual, mpsSecond);
            end
            else if FMeasureLine.LineCount = 3 then
            begin
              FMeasureLine.SetMeasurePoint(FXActual, FYActual, mpsThird);
            end;

            Screen.Cursor := crMeasureMove;
          end;

        dsStretchCorner:
          begin
            // change the measure points
            FMeasureLine.SetMeasurePoint(FXActual, FYActual, FMeasurePointSelector);

            Screen.Cursor := crMeasureMove;
          end;

        dsTranslate:
          begin
            // calculate the translating amount
            LNewPoint        := Point(FXActual, FYActual);
            LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

            FMeasureLine.Translate(LTranslateVector);

            // track the coordinates
            FDrawingBasePoint := LNewPoint;
            Screen.Cursor     := crMeasureMove;
          end;
      end;

      DrawMeasureLineOnMeasureLayer();
      ShowMeasureResult;

      if Assigned(FSelection) then
      begin
        imgWorkArea.Update();
        FSelection.DrawMarchingAnts();
      end;
    end;
  end
  else // change cursor when mouse left button not down
  begin
    if Assigned(FMeasureLine) then
    begin
      // Note, we should use image control coordinates to do the test.
      FMeasurePointSelector := FMeasureLine.GetHandleAtPoint(
        X, Y, imgWorkArea.BitmapToControl);

      if FMeasurePointSelector <> mpsNone then
      begin
        if ssAlt in Shift then
        begin
          if FMeasureLine.LineCount = 2 then
          begin
            Screen.Cursor := crMeasureAngle;
          end
          else
          begin
            Screen.Cursor := crMeasureMove;
          end;
        end
        else
        begin
          Screen.Cursor := crMeasureMove;
        end;
      end
      else if FMeasureLine.ContainsPoint(X, Y, imgWorkArea.BitmapToControl) then
      begin
        Screen.Cursor := crHandPoint;
      end
      else
      begin
        Screen.Cursor := crDefault;
      end;
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing the color info of the pixel that under the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  // showing the current coordinates
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.MeasureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
// Mouse left button up 

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing             := False;   // finish the processing
      FMeasureDrawingState := dsNotDrawing;

      if Assigned(FSelection) then
      begin
        FSelection.IsAnimated := True;
      end;

      if Assigned(FMeasureLine) then
      begin
        if not frmMain.lblMeasureUnit.Enabled then
        begin
          frmMain.lblMeasureUnit.Enabled := True;
        end;

        if not frmMain.cmbbxMeasureUnit.Enabled then
        begin
          frmMain.cmbbxMeasureUnit.Enabled := True;
          frmMain.cmbbxMeasureUnit.Color   := clWindow;
        end;

        if not frmMain.btnClearMeasureInfo.Enabled then
        begin
          frmMain.btnClearMeasureInfo.Enabled := True;
        end;
      end;
    end;

    Screen.Cursor := crDefault;
  end;
end;

// translate Measure Line by keyboard stroke
procedure TfrmChild.TranslateMeasureKeyDown(var Key: Word; Shift: TShiftState);
var
  LTranslateVector : TPoint;
  LIncrement       : Integer;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtMeasure) and Assigned(FMeasureLine) then
    begin
      if ssShift in Shift then
      begin
        LIncrement := 10;
      end
      else
      begin
        LIncrement := 1;
      end;

      case Key of
        VK_LEFT:
          begin
            LTranslateVector := Point(-LIncrement, 0);
          end;
          
        VK_UP:
          begin
            LTranslateVector := Point(0, -LIncrement);
          end;
          
        VK_RIGHT:
          begin
            LTranslateVector := Point(LIncrement, 0);
          end;

        VK_DOWN:
          begin
            LTranslateVector := Point(0, LIncrement);
          end;
      end;

      if Assigned(FSelection) and FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;

      FMeasureLine.Translate(LTranslateVector);

      // update the view
      if Assigned(FMeasureLayer) then
      begin
        DrawMeasureLineOnMeasureLayer();
      end;

      if Assigned(FSelection) then
      begin
        imgWorkArea.Update();
        FSelection.DrawMarchingAnts();
      end;

      ShowMeasureResult();
    end;
  end;
end;

procedure TfrmChild.TranslateMeasureKeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtMeasure) and Assigned(FMeasureLine) then
    begin
      if Assigned(FSelection) then
      begin
        if not FSelection.IsAnimated then
        begin
          FSelection.IsAnimated := True;
        end;
      end;
    end;
  end;
end;

procedure TfrmChild.ShapeRegionToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint            : TPoint;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse Left Button Down }

  if Button = mbLeft then
  begin
    // showing the coordinates info of the starting point and current point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    case frmMain.ShapeRegionTool of
      srtMove:
        begin
          if FLayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

            if LShapeRegionLayer.ShapeOutlineList.Count > 0 then
            begin
              // Note that, the X and Y is in control space.
              FShapeDrawingHandle :=
                LShapeRegionLayer.ShapeOutlineList.GetHandlePoint(
                  X, Y, HANDLE_RADIUS, imgWorkArea);

              if FShapeDrawingHandle in [dhAxAy, dhBxBy, dhAxBy, dhBxAy,
                                         dhLeftHalfAyBy, dhRightHalfAyBy,
                                         dhTopHalfAxBx, dhBottomHalfAxBx] then
              begin
                FShapeDrawingState := dsStretchCorner;
                Screen.Cursor      := SetCursorByHandle(FShapeDrawingHandle);
                imgWorkArea.Cursor := Screen.Cursor;

                imgWorkArea.Changed();

                // for Undo/Redo
                FGlobalTopLeft     := LShapeRegionLayer.ShapeOutlineList.FBoundaryTL;
                FGlobalBottomRight := LShapeRegionLayer.ShapeOutlineList.FBoundaryBR;
              end
              else
              begin
                if LShapeRegionLayer.ShapeOutlineList.PointInBoundary(
                     FXActual, FYActual, HANDLE_RADIUS) then
                begin
                  FShapeDrawingState := dsTranslate;
                  imgWorkArea.Cursor := crDrag;
                  Screen.Cursor      := crDrag;
                  
                  imgWorkArea.Changed();

                  FDrawingBasePoint     := Point(FXActual, FYActual);
                  FAccumTranslateVector := Point(0, 0); // for Undo/Redo
                end;
              end;
            end;
          end;
        end;

      srtRectangle,
      srtRoundedRect,
      srtEllipse,
      srtPolygon,
      srtLine:
        begin
          FActualStartPoint := Point(FXActual, FYActual);
          FActualEndPoint   := Point(FXActual, FYActual);

          FStartPoint := Point(X, Y);
          FEndPoint   := Point(X, Y);

          // refresh the screen
          imgWorkArea.Changed;
        end;
    end;
    
    FDrawing := True;
  end;
end;

procedure TfrmChild.ShapeRegionToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint, LNewPoint : TPoint;
  LTranslateVector  : TPoint;
  LColor            : TColor;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    case FShapeDrawingState of
      dsNewFigure:
        begin
          case frmMain.ShapeRegionTool of
            srtRectangle,
            srtRoundedRect,
            srtEllipse:
              begin
                if ssShift in Shift then
                begin
                  FActualEndPoint := CalculateRegularFigureEndPoint(
                    FActualStartPoint, Point(FXActual, FYActual) );
                end
                else
                begin
                  FActualEndPoint := Point(FXActual, FYActual);
                end;

                FEndPoint := Point(X, Y);
              end;
              
            srtPolygon,
            srtLine:
              begin
                FActualEndPoint := Point(FXActual, FYActual);
                FEndPoint       := Point(X, Y);
              end;
          end;

          // all the outline rendering code is in the
          // imgWorkArea.OnPaintStage() event
          imgWorkArea.Changed;
        end;

      dsTranslate:
        begin
          if FLayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

            if LShapeRegionLayer.ShapeOutlineList.Count > 0 then
            begin
              LNewPoint        := Point(FXActual, FYActual);
              LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

              with LShapeRegionLayer do
              begin
                ShapeOutlineList.Translate(LTranslateVector);
                ShapeRegion.Translate(LTranslateVector);
                DrawRegionOnLayer();
                Changed();
              end;

              FDrawingBasePoint     := Point(FXActual, FYActual);
              FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);  // For Undo/Redo
            end;
          end;
        end;

      dsStretchCorner:
        begin
          if FLayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

            if LShapeRegionLayer.ShapeOutlineList.Count > 0then
            begin
              with LShapeRegionLayer do
              begin
                case FShapeDrawingHandle of
                  dhAxAy:
                    begin
                      ShapeOutlineList.FBoundaryTL := Point(FXActual, FYActual);
                    end;

                  dhBxBy:
                    begin
                      ShapeOutlineList.FBoundaryBR := Point(FXActual, FYActual);
                    end;

                  dhAxBy:
                    begin
                      ShapeOutlineList.FBoundaryTL.X := FXActual;
                      ShapeOutlineList.FBoundaryBR.Y := FYActual;
                    end;

                  dhBxAy:
                    begin
                      ShapeOutlineList.FBoundaryTL.Y := FYActual;
                      ShapeOutlineList.FBoundaryBR.X := FXActual;
                    end;

                  dhTopHalfAxBx:
                    begin
                      ShapeOutlineList.FBoundaryTL.Y := FYActual;
                    end;

                  dhBottomHalfAxBx:
                    begin
                      ShapeOutlineList.FBoundaryBR.Y := FYActual;
                    end;

                  dhLeftHalfAyBy:
                    begin
                      ShapeOutlineList.FBoundaryTL.X := FXActual;
                    end;

                  dhRightHalfAyBy:
                    begin
                      ShapeOutlineList.FBoundaryBR.X := FXActual;
                    end;
                end;

                ShapeOutlineList.BoundaryStandardizeOrder;
                ShapeOutlineList.ScaleShapesCoordinates;
                ShapeRegion.AccumRGN := LShapeRegionLayer.ShapeOutlineList.GetScaledShapesRegion;
                DrawRegionOnLayer();
                Changed();
              end;
            end;
          end;
        end;
    end;
  end
  else // do OnMouseMove event when mouse left button not down
  begin
    if frmMain.ShapeRegionTool = srtMove then
    begin
      if FLayerList.SelectedLayer is TgmShapeRegionLayer then
      begin
        LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

        if LShapeRegionLayer.ShapeOutlineList.Count > 0 then
        begin
          // Note that, the X and Y is in control space.
          FShapeDrawingHandle :=
            LShapeRegionLayer.ShapeOutlineList.GetHandlePoint(
              X, Y, HANDLE_RADIUS, imgWorkArea);

          Screen.Cursor := SetCursorByHandle(FShapeDrawingHandle);

          if Screen.Cursor = crDefault then
          begin
            if frmMain.ShapeRegionTool = srtMove then
            begin
              imgWorkArea.Cursor := crMoveSelection;
            end
            else
            begin
              case frmMain.RegionCombineMode of
                rcmAdd:
                  begin
                    imgWorkArea.Cursor := crCrossAdd;
                  end;

                rcmSubtract:
                  begin
                    imgWorkArea.Cursor := crCrossSub;
                  end;

                rcmIntersect:
                  begin
                    imgWorkArea.Cursor := crCrossIntersect;
                  end;

                rcmExcludeOverlap:
                  begin
                    imgWorkArea.Cursor := crCrossInterSub;
                  end;
              end;
            end;
          end
          else
          begin
            imgWorkArea.Cursor := Screen.Cursor;
          end;
        end;
      end;
    end;

    // showing the color info of the pixel that under the mouse pointer
    LColor := imgWorkArea.Canvas.Pixels[X, Y];
    frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
    frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
  end;

  // showing the coordinates info
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.ShapeRegionToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint            : TPoint;
  LOutline          : TgmShapeOutline;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LCreatedNewLayer  : Boolean;
  LCommand          : TgmCustomCommand;
begin
  LCommand := nil;

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button up }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing      := False; // finish processing
      Screen.Cursor := crDefault;

      if frmMain.ShapeRegionTool = srtMove then
      begin
        imgWorkArea.Cursor := crMoveSelection;
      end
      else
      begin
        case frmMain.RegionCombineMode of
          rcmAdd:
            begin
              imgWorkArea.Cursor := crCrossAdd;
            end;

          rcmSubtract:
            begin
              imgWorkArea.Cursor := crCrossSub;
            end;

          rcmIntersect:
            begin
              imgWorkArea.Cursor := crCrossIntersect;
            end;

          rcmExcludeOverlap:
            begin
              imgWorkArea.Cursor := crCrossInterSub;
            end;
        end;
      end;

      case FShapeDrawingState of
        dsNewFigure:
          begin
            LOutline         := nil;
            LCreatedNewLayer := False;  // for Undo/Redo

            if frmMain.ShapeRegionTool in [
                 srtRectangle, srtRoundedRect, srtEllipse] then
            begin
              if ssShift in Shift then
              begin
                FActualEndPoint := CalculateRegularFigureEndPoint(
                  FActualStartPoint, Point(FXActual, FYActual) );
              end
              else
              begin
                FActualEndPoint := Point(FXActual, FYActual);
              end;
            end
            else
            begin
              FActualEndPoint := Point(FXActual, FYActual);
            end;

            if (FActualStartPoint.X <> FActualEndPoint.X) or
               (FActualStartPoint.Y <> FActualEndPoint.Y) then
            begin
              // If the current layer is not a Shape Region layer,
              // or it is a Shape Region layer but it is not in edit state,
              // then we have to create a new Shape Region layer.
              if not (FLayerList.SelectedLayer is TgmShapeRegionLayer) then
              begin
                LShapeRegionLayer := TgmShapeRegionLayer( CreateShapeRegionLayer() );

                FLayerList.Insert(FLayerList.SelectedIndex + 1, LShapeRegionLayer);
                LShapeRegionLayer.UpdateLogoThumbnail;

                LCreatedNewLayer := True;  // for Undo/Redo
              end
              else
              begin
                LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

                if LShapeREgionLayer.IsDismissed then
                begin
                  LShapeRegionLayer := TgmShapeRegionLayer( CreateShapeRegionLayer() );

                  FLayerList.Insert(FLayerList.SelectedIndex + 1, LShapeRegionLayer);
                  LShapeRegionLayer.UpdateLogoThumbnail();
                end;
              end;

              // create region
              case frmMain.ShapeRegionTool of
                srtRectangle:
                  begin
                    LOutline            := TgmRectangleOutline.Create;
                    LOutline.StartPoint := FActualStartPoint;
                    LOutline.EndPoint   := FActualEndPoint;

                    LShapeRegionLayer.ShapeRegion.NewRGN :=
                      CreateRectRGN(FActualStartPoint.X, FActualStartPoint.Y,
                                    FActualEndPoint.X, FActualEndPoint.Y);
                  end;

                srtRoundedRect:
                  begin
                    LOutline            := TgmRoundRectOutline.Create;
                    LOutline.StartPoint := FActualStartPoint;
                    LOutline.EndPoint   := FActualEndPoint;

                    TgmRoundRectOutline(LOutline).CornerRadius := frmMain.ShapeCornerRadius;

                    LShapeRegionLayer.ShapeRegion.NewRGN :=
                      CreateRoundRectRGN(FActualStartPoint.X,
                                         FActualStartPoint.Y,
                                         FActualEndPoint.X,
                                         FActualEndPoint.Y,
                                         frmMain.ShapeCornerRadius,
                                         frmMain.ShapeCornerRadius);
                  end;

                srtEllipse:
                  begin
                    LOutline            := TgmEllipseOutline.Create;
                    LOutline.StartPoint := FActualStartPoint;
                    LOutline.EndPoint   := FActualEndPoint;

                    LShapeRegionLayer.ShapeRegion.NewRGN :=
                      CreateEllipticRGN(FActualStartPoint.X, FActualStartPoint.Y,
                                        FActualEndPoint.X, FActualEndPoint.Y);
                  end;

                srtPolygon:
                  begin
                    LOutline            := TgmRegularPolygonOutline.Create;
                    LOutline.StartPoint := FActualStartPoint;
                    LOutline.EndPoint   := FActualEndPoint;

                    TgmRegularPolygonOutline(LOutline).Sides := frmMain.ShapePolygonSides;
                    TgmRegularPolygonOutline(LOutline).SetVertex;

                    CalcRegularPolygonVertices(FRegionPolygon,
                                               FActualStartPoint,
                                               FActualEndPoint,
                                               frmMain.ShapePolygonSides);

                    LShapeRegionLayer.ShapeRegion.NewRGN :=
                      CreatePolygonRGN(FRegionPolygon,
                                       frmMain.ShapePolygonSides,
                                       ALTERNATE);
                  end;

                srtLine:
                  begin
                    LOutline                        := TgmLineOutline.Create;
                    LOutline.StartPoint             := FActualStartPoint;
                    LOutline.EndPoint               := FActualEndPoint;
                    TgmLineOutline(LOutline).Weight := frmMain.LineWeight;

                    TgmLineOutline(LOutline).SetVertex;
                    CalcVertexForLineRegionOutline();

                    LShapeRegionLayer.ShapeRegion.NewRGN :=
                      CreatePolygonRGN(FRegionPolygon, 4, ALTERNATE);
                  end;
              end;

              if Assigned(LOutline) then
              begin
                LOutline.CombineMode := frmMain.RegionCombineMode;

                LShapeRegionLayer.ShapeOutlineList.Add(LOutline);
                LShapeRegionLayer.ShapeOutlineList.GetShapesBoundary();
              end;

              LShapeRegionLayer.ShapeRegion.CombineRGNToAccumRGN(
                frmMain.RegionCombineMode);

              LShapeRegionLayer.DrawRegionOnLayer();
              LShapeRegionLayer.Changed();
              LShapeRegionLayer.UpdateLayerThumbnail();

              frmMain.UpdateShapeOptions();

              // Undo/Redo
              if LCreatedNewLayer then
              begin
                LCommand := TgmNewShapeOnNewShapeRegionLayerCommand.Create(
                  FChannelManager, FLayerList, LShapeRegionLayer,
                  FLayerList.SelectedIndex);
              end
              else
              begin
                LCommand := TgmNewShapeOnExistedShapeRegionLayerCommand.Create(
                  FLayerList, FLayerList.SelectedIndex, LOutline,
                  LShapeRegionLayer.ShapeOutlineList.Count - 1);
              end;
            end;
          end;

        dsTranslate,
        dsStretchCorner:
          begin
            if FLayerList.SelectedLayer is TgmShapeRegionLayer then
            begin
              LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);
                             
              if LShapeRegionLayer.ShapeOutlineList.Count > 0then
              begin
                imgWorkArea.Changed();
                LShapeRegionLayer.UpdateLayerThumbnail();
              end;

              // Undo/Redo
              if FShapeDrawingState = dsTranslate then
              begin
                if (FAccumTranslateVector.X <> 0) or
                   (FAccumTranslateVector.Y <> 0) then
                begin
                  LCommand := TgmTranslateShapeRegionCommand.Create(FLayerList,
                    FLayerList.SelectedIndex, FAccumTranslateVector);
                end;
              end
              else if FShapeDrawingState = dsStretchCorner then
              begin
                if (FGlobalTopLeft.X <> LShapeRegionLayer.ShapeOutlineList.FBoundaryTL.X) or
                   (FGlobalTopLeft.Y <> LShapeRegionLayer.ShapeOutlineList.FBoundaryTL.Y) or
                   (FGlobalBottomRight.X <> LShapeRegionLayer.ShapeOutlineList.FBoundaryBR.X) or
                   (FGlobalBottomRight.Y <> LShapeRegionLayer.ShapeOutlineList.FBoundaryBR.Y) then
                begin
                  LCommand := TgmScaleShapeRegionCommand.Create(FLayerList,
                    FLayerList.SelectedIndex, FGlobalTopLeft, FGlobalBottomRight,
                    LShapeRegionLayer.ShapeOutlineList.FBoundaryTL,
                    LShapeRegionLayer.ShapeOutlineList.FBoundaryBR);
                end;
              end;
            end;

            FShapeDrawingState := dsNotDrawing;
          end;
      end;

      // Undo/Redo
      if Assigned(LCommand) then
      begin
        FCommandManager.AddCommand(LCommand);
      end;
    end;
  end;
end;

procedure TfrmChild.TranslateShapeRegionKeyDown(
  var Key: Word; Shift: TShiftState);
var
  LTranslateVector  : TPoint;
  LIncrement        : Integer;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtShape) and
       (FLayerList.SelectedLayer is TgmShapeRegionLayer) then
    begin
      LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

      if (LShapeRegionLayer.ShapeOutlineList.Count > 0) and
         Assigned(LShapeRegionLayer.ShapeRegion) then
      begin
        if ssShift in Shift then
        begin
          LIncrement := 10;
        end
        else
        begin
          LIncrement := 1;
        end;

        case Key of
          VK_LEFT:
            begin
              LTranslateVector := Point(-LIncrement, 0);
            end;
            
          VK_UP:
            begin
              LTranslateVector := Point(0, -LIncrement);
            end;
            
          VK_RIGHT:
            begin
              LTranslateVector := Point(LIncrement, 0);
            end;
            
          VK_DOWN:
            begin
              LTranslateVector := Point(0, LIncrement);
            end;
        end;

        // For Undo/Redo
        if FKeyIsDown = False then
        begin
          FAccumTranslateVector := Point(0, 0);
          FKeyIsDown := True;
        end;

        FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);

        // translate shape regions
        if Assigned(FSelection) and FSelection.IsAnimated then
        begin
          FSelection.IsAnimated := False;
        end;

        with LShapeRegionLayer do
        begin
          ShapeOutlineList.Translate(LTranslateVector);
          ShapeRegion.Translate(LTranslateVector);
          DrawRegionOnLayer();
          Changed();
        end;

        if Assigned(FSelection) then
        begin
          imgWorkArea.Update();
          FSelection.DrawMarchingAnts();
        end;
      end;
    end;
  end;
end; 

procedure TfrmChild.TranslateShapeRegionKeyUp(var Key: Word; Shift: TShiftState);
var
  LShapeRegionLayer : TgmShapeRegionLayer;
  LCommand          : TgmCustomCommand;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtShape) and
       (FLayerList.SelectedLayer is TgmShapeRegionLayer) then
    begin
      LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);
      LShapeRegionLayer.UpdateLayerThumbnail();

      if Assigned(FSelection) then
      begin
        if not FSelection.IsAnimated then
        begin
          FSelection.IsAnimated := True;
        end;
      end;

      // Undo/Redo
      if (FAccumTranslateVector.X <> 0) or
         (FAccumTranslateVector.Y <> 0) then
      begin
        LCommand := TgmTranslateShapeRegionCommand.Create(FLayerList,
          FLayerList.SelectedIndex, FAccumTranslateVector);

        FCommandManager.AddCommand(LCommand);
      end;
    end;
  end;
end; 

procedure TfrmChild.TransformSelectionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  if Assigned(FSelectionTransformation) then
  begin
    FLastTransformMode := FSelectionTransformation.TransformMode;

    case FSelectionTransformation.TransformMode of
       tmDistort:
        begin
          // using control space corrdinate as parameters
          FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

          if FTransformHandle <> dhNone then
          begin
            FDrawingBasePoint     := Point(FXActual, FYActual);
            FSelection.IsAnimated := False;
            FDrawing              := True;
            Screen.Cursor         := crMovePath;
            imgWorkArea.Cursor    := crMovePath;
          end
          else
          if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
          begin
            FDrawingBasePoint     := Point(FXActual, FYActual);
            FSelection.IsAnimated := False;
            FDrawing              := True;
            Screen.Cursor         := crHandGrip;
            imgWorkArea.Cursor    := crHandGrip;

            FSelectionTransformation.TransformMode := tmTranslate;
          end
          else
          begin
            Screen.Cursor      := crDefault;
            imgWorkArea.Cursor := crMoveSelection;
          end;
        end;

      tmRotate:
        begin
          if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
          begin
            FDrawingBasePoint     := Point(FXActual, FYActual);
            FSelection.IsAnimated := False;
            FDrawing              := True;
            Screen.Cursor         := crHandGrip;
            imgWorkArea.Cursor    := crHandGrip;

            FSelectionTransformation.TransformMode := tmTranslate;
          end
          else
          begin
            FRotateRadiansInMouseDown := ArcTan2(FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
                                                 FXActual - FSelectionTransformation.SelectionCenterCoord.X);
                                                 
            FSelection.IsAnimated     := False;
            FDrawing                  := True;
          end;
        end;

      tmScale:
        begin
          FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

          if FTransformHandle <> dhNone then
          begin
            FDrawingBasePoint     := Point(FXActual, FYActual);
            FSelection.IsAnimated := False;
            FDrawing              := True;
            Screen.Cursor         := SetCursorByHandle(FTransformHandle);
            imgWorkArea.Cursor    := Screen.Cursor;
          end
          else
          if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
          begin
            FDrawingBasePoint     := Point(FXActual, FYActual);
            FSelection.IsAnimated := False;
            FDrawing              := True;
            Screen.Cursor         := crHandGrip;
            imgWorkArea.Cursor    := crHandGrip;

            FSelectionTransformation.TransformMode := tmTranslate;
          end
          else
          begin
            Screen.Cursor      := crDefault;
            imgWorkArea.Cursor := crMoveSelection;
          end;
        end;
    end;

    // Undo/Redo
    if FTransformCopy = nil then
    begin
      FTransformCopy := TgmSelectionTransformation.Create(FSelection);
    end;
    
    FTransformCopy.AssignTransformData(FSelectionTransformation);
  end;
end;

procedure TfrmChild.TransformSelectionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LPoint            : TPoint;
  LNewPoint         : TPoint;
  LTranslateVector  : TPoint;
  LRadiansIncrement : Extended;
  LRadians          : Extended;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  if FDrawing then
  begin
    case FSelectionTransformation.TransformMode of
      tmDistort:
        begin
          // calculte the offset vector
          LNewPoint        := Point(FXActual, FYActual);
          LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

          // change the vertices of the transformation by the offset vector
          FSelectionTransformation.ChangeVertices_Distort(LTranslateVector, FTransformHandle);
        end;

      tmRotate:
        begin
          FRotateRadiansInMouseMove :=
            ArcTan2(FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
                    FXActual - FSelectionTransformation.SelectionCenterCoord.X);

          LRadiansIncrement := FRotateRadiansInMouseMove - FRotateRadiansInMouseDown;
          FSelectionTransformation.ChangeVertices_Rotate(LRadiansIncrement);

          Screen.Cursor      := GetCursorByDegree( RadToDeg(FRotateRadiansInMouseMove) );
          imgWorkArea.Cursor := Screen.Cursor;
        end;

      tmScale:
        begin
          LNewPoint        := Point(FXActual, FYActual);
          LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

          FSelectionTransformation.ChangeVertices_Scale(LTranslateVector, FTransformHandle);
        end;

      tmTranslate:
        begin
          LNewPoint        := Point(FXActual, FYActual);
          LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

          FSelectionTransformation.TranslateVertices(LTranslateVector);
        end;
    end;

    FSelectionTransformation.ExecuteTransform;

    // indicating that we are doing the transformation now
    if not FSelectionTransformation.IsTransforming then
    begin
      FSelectionTransformation.IsTransforming := True;
    end;
    
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            FSelectionTransformation.ShowTransformedSelection(
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              [csGrayscale]);

            FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
          end;
        end;

      ctQuickMaskChannel:
        begin
          FSelectionTransformation.ShowTransformedSelection(
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
            [csGrayscale]);

          FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
        end;

      ctLayerMaskChannel:
        begin
          FSelectionTransformation.ShowTransformedSelection(
            FLayerList.SelectedLayer.MaskBitmap, [csGrayscale]);

          FLayerList.SelectedLayer.Changed;

          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              0, 0, FLayerList.SelectedLayer.MaskBitmap);
          end;
        end;

      ctColorChannel:
        begin
          FSelectionTransformation.ShowTransformedSelection(
            FLayerList.SelectedLayer.LayerBitmap,
            FChannelManager.SelectedColorChannels);

          FLayerList.SelectedLayer.Changed;
        end;
    end;

    // redraw the control border of the selection transformation
    imgWorkArea.Update(imgWorkArea.ClientRect);
    
    FDrawingBasePoint := LNewPoint;
  end
  else
  begin
    if Assigned(FSelectionTransformation) then
    begin
      case FSelectionTransformation.TransformMode of
        tmDistort:
          begin
            if FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS) <> dhNone then
            begin
              Screen.Cursor      := crMovePath;
              imgWorkArea.Cursor := crMovePath;
            end
            else
            if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
            begin
              Screen.Cursor      := crHandLoosen;
              imgWorkArea.Cursor := crHandLoosen;
            end
            else
            begin
              Screen.Cursor      := crDefault;
              imgWorkArea.Cursor := crMoveSelection;
            end;
          end;

        tmRotate:
          begin
            if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
            begin
              Screen.Cursor      := crHandLoosen;
              imgWorkArea.Cursor := crHandLoosen;
            end
            else
            begin
              LRadians := ArcTan2(FYActual - FSelectionTransformation.SelectionCenterCoord.Y,
                                  FXActual - FSelectionTransformation.SelectionCenterCoord.X);

              Screen.Cursor      := GetCursorByDegree( RadToDeg(LRadians) );
              imgWorkArea.Cursor := Screen.Cursor;
            end;
          end;

        tmScale:
          begin
            FTransformHandle := FSelectionTransformation.GetHandleAtPoint(X, Y, SELECTION_HANDLE_RADIUS);

            if FTransformHandle <> dhNone then
            begin
              Screen.Cursor      := SetCursorByHandle(FTransformHandle);
              imgWorkArea.Cursor := Screen.Cursor;
            end
            else
            if FSelectionTransformation.PointOnSelectionBody(FXActual, FYActual) then
            begin
              Screen.Cursor      := crHandLoosen;
              imgWorkArea.Cursor := crHandLoosen;
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

  // showing the coordinates info
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end; 

procedure TfrmChild.TransformSelectionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint   : TPoint;
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

  if FDrawing then
  begin
    if Assigned(FSelection) then
    begin
      if not FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := True;
      end;
    end;

    FDrawing := False;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    if Assigned(FSelectionTransformation) then
    begin
      // Undo/Redo
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionTransformOnAlphaChannelCommand.Create(
              FSelectionTransformation.TransformMode,
              FChannelManager, FChannelManager.AlphaChannelList.SelectedIndex,
              FTransformCopy, FSelectionTransformation,
              GetSelectionForUndoRedo, GetSelectionTransformationForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionTransformOnQuickMaskChannelCommand.Create(
              FSelectionTransformation.TransformMode, FChannelManager,
              FTransformCopy, FSelectionTransformation,
              GetSelectionForUndoRedo, GetSelectionTransformationForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionTransformOnLayerMaskCommand.Create(
               FSelectionTransformation.TransformMode,
               FLayerList, FLayerList.SelectedIndex,
               FTransformCopy, FSelectionTransformation,
               GetSelectionForUndoRedo, GetSelectionTransformationForUndoRedo);
          end;

        ctColorChannel:
          begin
             LCommand := TgmSelectionTransformOnLayerCommand.Create(
               FSelectionTransformation.TransformMode,
               FChannelManager, FLayerList, FLayerList.SelectedIndex,
               FTransformCopy, FSelectionTransformation,
               GetSelectionForUndoRedo, GetSelectionTransformationForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        FCommandManager.AddCommand(LCommand);
      end;

      if Assigned(FTransformCopy) then
      begin
        FreeAndNil(FTransformCopy);
      end;

      // We changed the transform mode to tmTranslate at some point,
      // so we need to restore it back to tmScale, tmDistort or tmRotate. 
      FSelectionTransformation.TransformMode := FLastTransformMode;

      if FSelectionTransformation.TransformMode = tmRotate then
      begin
        TgmSelectionRotate(FSelectionTransformation).UpdateRotateState();
      end;
    end;
  end;
end;

procedure TfrmChild.TextToolsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint               : TPoint;
  LIsRichTextLayer     : Boolean;
  LIsPointOnTextBorder : Boolean;
  LIsPointOnTextHandle : Boolean;
  LRichTextLayer       : TgmRichTextLayer;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // showing the coordinates of the starting and current point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    LIsRichTextLayer     := False;
    LIsPointOnTextBorder := False;
    LIsPointOnTextHandle := False;
    LRichTextLayer       := nil;
    
    if FLayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer         := TgmRichTextLayer(FLayerList.SelectedLayer);
      LIsRichTextLayer       := True;
      LIsPointOnTextBorder   := LRichTextLayer.ContainsPoint( Point(FXActual, FYActual) );
      FRichTextDrawingHandle := LRichTextLayer.GetHandleAtPoint(X, Y, imgWorkArea.BitmapToControl);
      LIsPointOnTextHandle   := (FRichTextDrawingHandle <> dhNone);
    end;

    if LIsRichTextLayer and (LIsPointOnTextHandle or LIsPointOnTextBorder) then
    begin
      if not frmRichTextEditor.Visible then
      begin
        LRichTextLayer.RichTextStream.Position := 0;
        frmRichTextEditor.rchedtRichTextEditor.Lines.LoadFromStream(LRichTextLayer.RichTextStream);
      end;

      if LIsPointOnTextHandle then
      begin
        // 1.  Trying to stretch corner of selected figure?
        Screen.Cursor         := SetCursorByHandle(FRichTextDrawingHandle);
        FRichTextDrawingState := dsStretchCorner;

        // for Undo/Redo
        FGlobalTopLeft     := LRichTextLayer.BorderStart;
        FGlobalBottomRight := LRichTextLayer.BorderEnd;
      end
      else
      // 2.  Trying to translate selected figure(s)? Check first for existing set of selected figures.
      if LIsPointOnTextBorder then
      begin
        Screen.Cursor         := crDrag;
        FRichTextDrawingState := dsTranslate;
        FDrawingBasePoint     := Point(FXActual, FYActual);
        FAccumTranslateVector := Point(0, 0); // for Undo/Redo
      end;
    end
    else
    begin
      Screen.Cursor         := crCross;
      FStartPoint           := Point(X, Y);
      FEndPoint             := FStartPoint;
      FActualStartPoint     := Point(FXActual, FYActual);
      FRichTextDrawingState := dsNewFigure;

      // the text border is rendering by imgWorkArea.OnPaintStage() event
      imgWorkArea.Changed;
    end;
    
    FDrawing := True;
  end;
end;

procedure TfrmChild.TextToolsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LRichTextLayer   : TgmRichTextLayer;
  LPoint           : TPoint;
  LNewPoint        : TPoint;
  LTranslateVector : TPoint;
  LColor           : TColor;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    case FRichTextDrawingState of
      dsStretchCorner:
        begin
          LRichTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);

          with LRichTextLayer do
          begin
            case FRichTextDrawingHandle of
              dhAxAy:
                begin
                  BorderStart := Point(FXActual, FYActual);
                end;

              dhBxBy:
                begin
                  BorderEnd := Point(FXActual, FYActual);
                end;

              dhAxBy:
                begin
                  BorderStart := Point(FXActual, BorderStart.Y);
                  BorderEnd   := Point(BorderEnd.X, FYActual);
                end;

              dhBxAy:
                begin
                  BorderStart := Point(BorderStart.X, FYActual);
                  BorderEnd   := Point(FXActual, BorderEnd.Y);
                end;
                
              dhTopHalfAxBx:
                begin
                  BorderStart := Point(BorderStart.X, FYActual);
                end;

              dhBottomHalfAxBx:
                begin
                  BorderEnd := Point(BorderEnd.X, FYActual);
                end;

              dhLeftHalfAyBy:
                begin
                  BorderStart := Point(FXActual, BorderStart.Y);
                end;

              dhRightHalfAyBy:
                begin
                  BorderEnd := Point(FXActual, BorderEnd.Y);
                end;
            end;
          end;

          LRichTextLayer.StandardizeBorderEndingOrder;
          LRichTextLayer.DrawTextOnLayer;
          LRichTextLayer.Changed;
        end;

      dsTranslate:
        begin
          LRichTextLayer   := TgmRichTextLayer(FLayerList.SelectedLayer);
          LNewPoint        := Point(FXActual, FYActual);
          LTranslateVector := SubtractPoints(LNewPoint, FDrawingBasePoint);

          LRichTextLayer.Translate(LTranslateVector);
          LRichTextLayer.DrawTextOnLayer();
          LRichTextLayer.Changed();

          FDrawingBasePoint := LNewPoint;

          // For Undo/Redo 
          FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);
        end;

      dsNewFigure:
        begin
          FEndPoint := Point(X, Y);

          // the text border is rendering by imgWorkArea.OnPaintStage() event
          imgWorkArea.Changed();
        end;
    end;
  end
  else // if the FDrawing = False
  begin
    if FLayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer         := TgmRichTextLayer(FLayerList.SelectedLayer);
      FRichTextDrawingHandle := LRichTextLayer.GetHandleAtPoint(X, Y, imgWorkArea.BitmapToControl);
      Screen.Cursor          := SetCursorByHandle(FRichTextDrawingHandle);
    end;

    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing the color info of the pixel that under the mouse pointer
      LColor := imgWorkArea.Canvas.Pixels[X, Y];
      
      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual); 
end;

procedure TfrmChild.TextToolsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint         : TPoint;
  LRichTextLayer : TgmRichTextLayer;
  LCommand       : TgmCustomCommand;
begin
  LCommand := nil;

  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    FDrawing := False;  // finish the processing

    if FRichTextDrawingState in [dsTranslate, dsStretchCorner] then
    begin
      Screen.Cursor := crDefault;

      // Undo/Redo
      if FRichTextDrawingState = dsTranslate then
      begin
        if (FAccumTranslateVector.X <> 0) or
           (FAccumTranslateVector.Y <> 0) then
        begin
          LCommand := TgmTranslateTextRegionCommand.Create(FLayerList,
            FLayerList.SelectedIndex, FAccumTranslateVector);
        end;
      end
      else if FRichTextDrawingState = dsStretchCorner then
      begin
        LRichTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);

        if (FGlobalTopLeft.X <> LRichTextLayer.BorderStart.X) or
           (FGlobalTopLeft.Y <> LRichTextLayer.BorderStart.Y) or
           (FGlobalBottomRight.X <> LRichTextLayer.BorderEnd.X) or
           (FGlobalBottomRight.Y <> LRichTextLayer.BorderEnd.Y) then
        begin
          LCommand := TgmScaleTextRegionCommand.Create(FLayerList,
            FLayerList.SelectedIndex, FGlobalTopLeft, FGlobalBottomRight,
            LRichTextLayer.BorderStart, LRichTextLayer.BorderEnd);
        end;
      end;

      if Assigned(LCommand) then
      begin
        FCommandManager.AddCommand(LCommand);
      end;
    end
    else if FRichTextDrawingState = dsNewFigure then
    begin
      // if we are editing a text layer now,
      // save text for the current text layer and create a new one
      if frmRichTextEditor.Visible then
      begin
        CommitEdits;
      end;

      Screen.Cursor   := crDefault;
      FActualEndPoint := Point(FXActual, FYActual);

      PointStandardizeOrder(FActualStartPoint, FActualEndPoint);

      // If the starting point and the end point are same, change the end
      // point, make the input area is same as default size of an
      // TRichEdit component.
      if SameCoordinate(FActualStartPoint, FActualEndPoint) then
      begin
        FActualEndPoint.X := FXActual + 121;
        FActualEndPoint.Y := FYActual + 21;
      end;

      LRichTextLayer := TgmRichTextLayer( CreateRichTextLayer() );

      FLayerList.Insert(FLayerList.SelectedIndex + 1, LRichTextLayer );
      LRichTextLayer.SetTextBorder(FActualStartPoint, FActualEndPoint);

      // Open Rich Text Editor
      frmRichTextEditor.Width  := LRichTextLayer.BorderWidth  + 14;
      frmRichTextEditor.Height := LRichTextLayer.BorderHeight + frmRichTextEditor.stsbrTextInfo.Height + 44;

      frmRichTextEditor.rchedtRichTextEditor.Clear();
      frmRichTextEditor.Show();

      frmMain.UpdateTextOptions();
    end;

    FRichTextDrawingState := dsNotDrawing;

    // update view
    imgWorkArea.Changed();
  end;
end;

// translate Text by keyboard stroke
procedure TfrmChild.TranslateTextKeyDown(var Key: Word; Shift: TShiftState);
var
  LTranslateVector : TPoint;
  LIncrement       : Integer;
  LTextLayer       : TgmRichTextLayer;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtTextTool) and
       (FLayerList.SelectedLayer is TgmRichTextLayer) then
    begin
      if ssShift in Shift then
      begin
        LIncrement := 10;
      end
      else
      begin
        LIncrement := 1;
      end;

      case Key of
        VK_LEFT:
          begin
            LTranslateVector := Point(-LIncrement, 0);
          end;
          
        VK_UP:
          begin
            LTranslateVector := Point(0, -LIncrement);
          end;
          
        VK_RIGHT:
          begin
            LTranslateVector := Point(LIncrement, 0);
          end;
          
        VK_DOWN:
          begin
            LTranslateVector := Point(0, LIncrement);
          end;
      end;

      // For Undo/Redo
      if FKeyIsDown = False then
      begin
        FAccumTranslateVector := Point(0, 0);
        FKeyIsDown := True;
      end;

      FAccumTranslateVector := AddPoints(FAccumTranslateVector, LTranslateVector);

      // translate text
      if Assigned(FSelection) and FSelection.IsAnimated then
      begin
        FSelection.IsAnimated := False;
      end;

      LTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);
      LTextLayer.Translate(LTranslateVector);

      if not frmRichTextEditor.Visible then
      begin
        if frmRichTextEditor.rchedtRichTextEditor.Lines.Count = 0 then
        begin
          LTextLayer.RichTextStream.Position := 0;
          frmRichTextEditor.rchedtRichTextEditor.Lines.LoadFromStream(LTextLayer.RichTextStream);
        end;
      end;

      FLayerList.SelectedLayer.LayerBitmap.Clear($00FFFFFF);

      DrawRichTextOnBitmap(LTextLayer.LayerBitmap,
                           LTextLayer.BorderRect,
                           frmRichTextEditor.rchedtRichTextEditor);

      FLayerList.SelectedLayer.Changed();

      if Assigned(FSelection) then
      begin
        imgWorkArea.Update();
        FSelection.DrawMarchingAnts();
      end;
    end;
  end;
end; 

procedure TfrmChild.TranslateTextKeyUp(var Key: Word; Shift: TShiftState);
var
  LCommand : TgmCustomCommand;
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
  begin
    if (frmMain.MainTool = gmtTextTool) and
       (FLayerList.SelectedLayer is TgmRichTextLayer) then
    begin
      // Undo/Redo
      LCommand := TgmTranslateTextRegionCommand.Create(FLayerList,
        FLayerList.SelectedIndex, FAccumTranslateVector);

      FCommandManager.AddCommand(LCommand);

      if not frmRichTextEditor.Visible then
      begin
        frmRichTextEditor.rchedtRichTextEditor.Clear();
      end;

      if Assigned(FSelection) then
      begin
        if not FSelection.IsAnimated then
        begin
          FSelection.IsAnimated := True;
        end;
      end;
    end;
  end;
end;

{ events for Eyedropper tool }

procedure TfrmChild.EyedropperMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    with FChannelManager do
    begin
      case CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(SelectedAlphaChannel) then
            begin
              frmColor.ggbrRValue.Position :=
                255 - GetRValue(SelectedAlphaChannel.ChannelLayer.Bitmap.Canvas.Pixels[FXActual, FYActual]);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(QuickMaskChannel) then
            begin
              frmColor.ggbrRValue.Position :=
                255 - GetRValue(QuickMaskChannel.ChannelLayer.Bitmap.Canvas.Pixels[FXActual, FYActual]);
            end;
          end;

        ctLayerMaskChannel:
          begin
            frmColor.ggbrRValue.Position :=
              255 - GetRValue(FLayerList.SelectedLayer.MaskBitmap.Canvas.Pixels[FXActual, FYActual]);
          end;
          
        ctColorChannel:
          begin
            if csRed in SelectedColorChannels then
            begin
              frmColor.ggbrRValue.Position := GetRValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;

            if csGreen in SelectedColorChannels then
            begin
              frmColor.ggbrGValue.Position := GetGValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;

            if csBlue in SelectedColorChannels then
            begin
              frmColor.ggbrBValue.Position := GetBValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;
          end;
      end;
    end;

    FDrawing := True;
  end;
end;

procedure TfrmChild.EyedropperMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor : TColor;
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    with FChannelManager do
    begin
      case CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(SelectedAlphaChannel) then
            begin
              frmColor.ggbrRValue.Position :=
                255 - GetRValue(SelectedAlphaChannel.ChannelLayer.Bitmap.Canvas.Pixels[FXActual, FYActual]);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(QuickMaskChannel) then
            begin
              frmColor.ggbrRValue.Position :=
                255 - GetRValue(QuickMaskChannel.ChannelLayer.Bitmap.Canvas.Pixels[FXActual, FYActual]);
            end;
          end;

        ctLayerMaskChannel:
          begin
            frmColor.ggbrRValue.Position :=
              255 - GetRValue(FLayerList.SelectedLayer.MaskBitmap.Canvas.Pixels[FXActual, FYActual]);
          end;

        ctColorChannel:
          begin
{$RANGECHECKS OFF}
            if csRed in SelectedColorChannels then
            begin
              frmColor.ggbrRValue.Position := GetRValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;

            if csGreen in SelectedColorChannels then
            begin
              frmColor.ggbrGValue.Position := GetGValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;

            if csBlue in SelectedColorChannels then
            begin
              frmColor.ggbrBValue.Position := GetBValue(imgWorkArea.Canvas.Pixels[X, Y]);
            end;
{$RANGECHECKS ON}
          end;
      end;
    end;
  end
  else
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing color info
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  // showing current layer coordinates
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.EyedropperMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
{ Mouse left button up }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing := False;
    end;
  end;
end; 

{ events for Hand tool }

procedure TfrmChild.HandToolMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button down }

  if Button = mbLeft then
  begin
    // showing the coordinates of starting point
    frmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
    frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);

    Screen.Cursor                := crHandGrip;
    FStartPoint                  := Point(X, Y);
    FEndPoint                    := Point(X, Y);
    imgWorkArea.Canvas.Pen.Width := 1;
    imgWorkArea.Canvas.Pen.Color := clBlack;
    imgWorkArea.Canvas.Pen.Style := psSolid;

    DrawStraightLine(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);

    FDrawing := True;
  end;
end;

procedure TfrmChild.HandToolMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LColor : TColor;
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Move mouse when mouse left button down }

  if FDrawing then
  begin
    Screen.Cursor := crHandGrip;
    // clear the old line
    DrawStraightLine(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);

    FEndPoint := Point(X, Y);
    DrawStraightLine(imgWorkArea.Canvas, FStartPoint, FEndPoint, pmNotXor);
  end
  else
  begin
    if (FXActual >= 0) and
       (FYActual >= 0) and
       (FXActual <= FLayerList.SelectedLayer.LayerBitmap.Width) and
       (FYActual <= FLayerList.SelectedLayer.LayerBitmap.Height) then
    begin
      // showing color info
      LColor := imgWorkArea.Canvas.Pixels[X, Y];

      frmMain.ShowColorRGBInfoOnInfoViewer(LColor);
      frmMain.ShowColorCMYKInfoOnInfoViewer(LColor);
    end;
  end;

  imgWorkArea.Canvas.Pen.Mode := pmCopy;
  
  // showing current layer coordinates
  frmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(FXActual, FYActual);
end;

procedure TfrmChild.HandToolMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  dx, dy : Integer;
  LPoint : TPoint;
begin
  // get layer space coordinates
  LPoint   := imgWorkArea.ControlToBitmap( Point(X, Y) );
  FXActual := LPoint.X;
  FYActual := LPoint.Y;

{ Mouse left button up }

  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FDrawing      := False;
      Screen.Cursor := crDefault;
      dx            := FEndPoint.X - FStartPoint.X;
      dy            := FEndPoint.Y - FStartPoint.Y;

      imgWorkArea.Scroll(dx, dy);
    end;

    imgWorkArea.Canvas.Pen.Mode := pmCopy;
  end;
end;

// udpate thumbnail depending on which channel is currently selected
procedure TfrmChild.UpdateThumbnailsBySelectedChannel;
begin
  case FChannelManager.CurrentChannelType of
    ctAlphaChannel:
      begin
        if Assigned(FChannelManager.SelectedAlphaChannel) then
        begin
          FChannelManager.SelectedAlphaChannel.UpdateChannelThumbnail;
        end;
      end;

    ctQuickMaskChannel:
      begin
        if Assigned(FChannelManager.QuickMaskChannel) then
        begin
          FChannelManager.QuickMaskChannel.UpdateChannelThumbnail;
        end;
      end;

    ctLayerMaskChannel:
      begin
        FLayerList.SelectedLayer.UpdateMaskThumbnail;

        if FLayerList.SelectedLayer is TgmNormalLayer then
        begin
          FLayerList.SelectedLayer.UpdateLayerThumbnail;
        end;
        
        frmLayers.LayerPanelManager.Invalidate;

        if Assigned(FChannelManager.LayerMaskChannel) then
        begin
          FChannelManager.LayerMaskChannel.UpdateChannelThumbnail;
        end;
      end;

    ctColorChannel:
      begin
        if FLayerList.SelectedLayer is TgmNormalLayer then
        begin
          FLayerList.SelectedLayer.UpdateLayerThumbnail;
          frmLayers.LayerPanelManager.Invalidate;
        end;
      end;
  end;
end;

procedure TfrmChild.SetupOnChildFormActivate;
var
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  if frmMain.ChildFormIsCreating then
  begin
    Exit;
  end;

  if ActiveChildForm <> nil then
  begin
    if ActiveChildForm <> Self then
    begin
      if Assigned(ActiveChildForm.FSelection) then
      begin
        ActiveChildForm.FSelection.IsAnimated := False;
      end;

      // save the text from previous child form
      if frmRichTextEditor.Visible then
      begin
        ActiveChildForm.CommitEdits();
      end;
    end;
  end;

  PrevChildForm   := ActiveChildForm;  // remember the old active child form
  ActiveChildForm := Self;             // force ActiveChildForm points to current active child form

  frmLayers.LayerPanelManager.LayerList := FLayerList;

  // link the channel manager to channel manager viewer
  frmChannels.ChannelViewer.ChannelManager := FChannelManager;

  // link the path list to a path manager
  frmPaths.PathManager.PathList := FPathList;

  // link the command manager to a command viewer
  frmHistory.CommandViewer.CommandManager := FCommandManager;

  // update the appearance of the color window
  if FLayerList.SelectedLayer <> nil then
  begin
    if FChannelManager.CurrentChannelType in [
         ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
    begin
      frmColor.ColorMode := cmGrayscale;
    end
    else
    begin
      frmColor.ColorMode := cmRGB;  // must be on layer
    end;
  end;

  // connect mouse event handle
  if Assigned(FSelectionTransformation) then
  begin
    ConnectTransformMouseEvents();
  end
  else
  begin
    ConnectMouseEventsToImage();
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := True;
  end;

  // showing the magnification of the current image
  frmMain.CanChange := False;
  try
    frmMain.ggbrZoomSlider.Position := FMagnification;
  finally
    frmMain.CanChange := True;
  end;

  // initialze the status bar of the main form when the child form is activated 
  frmMain.stsbrMain.Panels[0].Text := GetBitmapDimensionString(FLayerList.SelectedLayer.LayerBitmap);

  // if current tool is not any of the Standard tools,
  // then clear the movable figures
  if frmMain.MainTool <> gmtStandard then
  begin
    FFigureManager.DeselectAllFigures();
    FLayerList.SelectedLayer.Changed();
  end
  else
  begin
    if frmMain.StandardTool <> gstMoveObjects then
    begin
      FFigureManager.DeselectAllFigures();
      FLayerList.SelectedLayer.Changed();
    end;
  end;

  // finish Magnetic Lasso if the current tool is not Magnetic Lasso Tool
  if Assigned(FMagneticLasso) then
  begin
    if (frmMain.MainTool <> gmtMarquee) or
       (frmMain.MarqueeTool <> mtMagneticLasso) then
    begin
      FinishMagneticLasso();
    end;
  end;

  // if the current tool is Marquee tool...
  if frmMain.MainTool = gmtMarquee then
  begin
    if frmMain.MarqueeTool = mtMoveResize then
    begin
      // identify not to draw the selection
      FMarqueeDrawingState := dsNotDrawing;
    end
    else
    begin
      // identify draw new selection
      FMarqueeDrawingState := dsNewFigure;
    end;
  end;

  // if the current tool is Crop tool...
  if frmMain.MainTool = gmtCrop then
  begin
    if FCrop <> nil then
    begin
      frmMain.shpCroppedShieldColor.Brush.Color  := FCrop.ShieldWinColor;
      frmMain.updwnCroppedShieldOpacity.Position := FCrop.ShieldOpacity;
      frmMain.chckbxShieldCroppedArea.Checked    := FCrop.IsShieldCroppedArea;

      frmMain.CanChange := False;
      try
        frmMain.edtCropWidth.Text  := IntToStr(FCrop.CropAreaWidth);
        frmMain.edtCropHeight.Text := IntToStr(FCrop.CropAreaHeight);
      finally
        frmMain.CanChange := True;
      end;
    end
    else
    begin
      frmMain.edtCropWidth.Text  := '';
      frmMain.edtCropHeight.Text := '';
    end;
  end
  else
  begin
    FinishCrop();
  end;

  // if the current tool is Shape Region tool...
  if frmMain.MainTool = gmtShape then
  begin
    if frmMain.ShapeRegionTool = srtMove then
    begin
      FShapeDrawingState := dsNotDrawing;

      if FLayerList.SelectedLayer is TgmShapeRegionLayer then
      begin
        LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);
        LShapeRegionLayer.ShapeOutlineList.BackupCoordinates();
      end;
    end
    else
    begin
      FShapeDrawingState := dsNewFigure;
    end;
  end;

  // if current tool is not measure tool ...
  if frmMain.MainTool <> gmtMeasure then
  begin
    RemoveMeasureLine();
    RemoveMeasureLayer();
  end;

  ChangeImageCursorByToolTemplets();
  frmMain.UpdateToolsOptions();
end;

// confirm to save the image when exit the program
procedure TfrmChild.BeforeExit(Sender: TObject);
begin
  // if the image has been modified...
  if FImageProcessed then
  begin
    // if the image has a filename, then confirm to save the file
    if FFileName <> '' then
    begin
      case MessageDlg(Format('Save change to %s ?', [ExtractFileName(FFileName)]),
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
          begin
            SaveNamedFile();
          end;

        mrCancel:
          begin
            Self.Activate;  // make the ActiveChildForm global variable points to me again
            Abort;
          end;
      end;
    end
    else // if the image has no a filename, then confirm to save the new file
    begin
      case MessageDlg(Format('Save change to %s ?', ['Untitled']),
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
          begin
            SaveFileWithNewName();
          end;
          
        mrCancel:
          begin
            Self.Activate; // make the ActiveChildForm global variable points to me again
            Abort;
          end;
      end;
    end;
  end;
end;

procedure TfrmChild.InitializeCanvasesForFigureTools;
begin
  if Assigned(FSelection) then
  begin
    with FSelection.CutOriginal.Canvas do
    begin
      Pen.Width   := frmMain.GlobalPenWidth;
      Pen.Style   := frmMain.GlobalPenStyle;
      Brush.Style := frmMain.GlobalBrushStyle;

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel,
        ctQuickMaskChannel,
        ctLayerMaskChannel:
          begin
            Pen.Color   := frmMain.ForeGrayColor;
            Brush.Color := frmMain.BackGrayColor;
          end;

        ctColorChannel:
          begin
            Pen.Color   := frmMain.GlobalForeColor;
            Brush.Color := frmMain.GlobalBackColor;
          end;
      end;
    end;
  end
  else
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            with FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Canvas do
            begin
              Pen.Color   := frmMain.ForeGrayColor;
              Pen.Style   := frmMain.GlobalPenStyle;
              Pen.Width   := frmMain.GlobalPenWidth;
              Brush.Color := frmMain.BackGrayColor;
              Brush.Style := frmMain.GlobalBrushStyle;
            end;
          end;
        end;

      ctQuickMaskChannel:
        begin
          if Assigned(FChannelManager.QuickMaskChannel) then
          begin
            with FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Canvas do
            begin
              Pen.Color   := frmMain.ForeGrayColor;
              Pen.Style   := frmMain.GlobalPenStyle;
              Pen.Width   := frmMain.GlobalPenWidth;
              Brush.Color := frmMain.BackGrayColor;
              Brush.Style := frmMain.GlobalBrushStyle;
            end;
          end;
        end;

      ctLayerMaskChannel:
        begin
          with FLayerList.SelectedLayer.MaskBitmap.Canvas do
          begin
            Pen.Color   := frmMain.ForeGrayColor;
            Pen.Style   := frmMain.GlobalPenStyle;
            Pen.Width   := frmMain.GlobalPenWidth;
            Brush.Color := frmMain.BackGrayColor;
            Brush.Style := frmMain.GlobalBrushStyle;
          end;
        end;

      ctColorChannel:
        begin
          with FLayerList.SelectedLayer.LayerBitmap.Canvas do
          begin
            Pen.Color   := frmMain.GlobalForeColor;
            Pen.Width   := frmMain.GlobalPenWidth;
            Pen.Style   := frmMain.GlobalPenStyle;
            Brush.Color := frmMain.GlobalBackColor;
            Brush.Style := frmMain.GlobalBrushStyle;
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.BlendLayersAndChannelsToBitmap(ADestBmp: TBitmap32);
var
  i, j          : Integer;
  sp, dp        : PColor32;
  LAlphaChannel : TgmAlphaChannel;
begin
  if Assigned(ADestBmp) then
  begin
    if ADestBmp.DrawMode <> dmBlend then
    begin
      ADestBmp.DrawMode := dmBlend;
    end;

    ADestBmp.SetSize(FLayerList.SelectedLayer.LayerBitmap.Width,
                     FLayerList.SelectedLayer.LayerBitmap.Height);

    ADestBmp.Clear($00000000);

    FChannelManager.BlendByColorChannelSettings(
      FLayerList.CombineResult, ADestBmp, ADestBmp.ClipRect);

    // blend layer mask layer
    if Assigned(FChannelManager.LayerMaskChannel) and
       FChannelManager.LayerMaskChannel.IsChannelVisible then
    begin
      sp := @FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Bits[0];
      dp := @ADestBmp.Bits[0];

      for i := 1 to (ADestBmp.Width * ADestBmp.Height) do
      begin
        with FChannelManager.LayerMaskChannel do
        begin
          ChannelLayer.Bitmap.OnPixelCombine(
            sp^, dp^, ChannelLayer.Bitmap.MasterAlpha);
        end;
        
        Inc(sp);
        Inc(dp);
      end;
    end;

    // blend alpha channel layers
    if FChannelManager.AlphaChannelList.Count > 0 then
    begin
      for j := 0 to (FChannelManager.AlphaChannelList.Count - 1) do
      begin
        LAlphaChannel := TgmAlphaChannel(
          FChannelManager.AlphaChannelList.Channels[j]);

        if LAlphaChannel.IsChannelVisible then
        begin
          sp := @LAlphaChannel.ChannelLayer.Bitmap.Bits[0];
          dp := @ADestBmp.Bits[0];

          for i := 1 to (ADestBmp.Width * ADestBmp.Height) do
          begin
            with LAlphaChannel do
            begin
              ChannelLayer.Bitmap.OnPixelCombine(
                sp^, dp^, ChannelLayer.Bitmap.MasterAlpha);
            end;

            Inc(sp);
            Inc(dp);
          end;
        end;
      end;
    end;

    // blend quick mask channel
    if Assigned(FChannelManager.QuickMaskChannel) and
       FChannelManager.QuickMaskChannel.IsChannelVisible then
    begin
      sp := @FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Bits[0];
      dp := @ADestBmp.Bits[0];

      for i := 1 to (ADestBmp.Width * ADestBmp.Height) do
      begin
        with FChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.OnPixelCombine(
            sp^, dp^, ChannelLayer.Bitmap.MasterAlpha);
        end;
        
        Inc(sp);
        Inc(dp);
      end;
    end;
  end;
end;

procedure TfrmChild.PreparePencilTool;
var
  FGColor, BKColor : TColor32;
begin
  FGColor := $0;
  BKColor := $0;

  if frmMain.GlobalPenStyle <> psSolid then
  begin
    case FChannelManager.CurrentChannelType of
      ctColorChannel:
        begin
          FGColor := Color32(frmMain.GlobalForeColor);

          if frmMain.GlobalBrushStyle = bsSolid then
          begin
            BKColor := Color32(frmMain.GlobalBackColor);
          end
          else
          begin
            BKColor := $0;
          end;
        end;

      ctAlphaChannel,
      ctQuickMaskChannel,
      ctLayerMaskChannel:
        begin
          FGColor := Color32(frmMain.ForeGrayColor);

          if frmMain.GlobalBrushStyle = bsSolid then
          begin
            BKColor := Color32(frmMain.BackGrayColor);
          end
          else
          begin
            BKColor := $0;
          end;
        end;
    end;
  end;

  if Assigned(FSelection) then
  begin
    case FChannelManager.CurrentChannelType of
      ctColorChannel:
        begin
          FSelection.CutOriginal.PenColor := Color32(frmMain.GlobalForeColor);
        end;

      ctAlphaChannel,
      ctQuickMaskChannel,
      ctLayerMaskChannel:
        begin
          FSelection.CutOriginal.PenColor := Color32(frmMain.ForeGrayColor);
        end;
    end;

    if frmMain.GlobalPenStyle <> psSolid then
    begin
      SetPencilStipplePattern(FSelection.CutOriginal, frmMain.GlobalPenStyle,
                              FGColor, BKColor);

      // The following code is very weird, it should draws a white solid line,
      // not a stippled line. But we can't without this line, otherwise,
      // the channels of processed pixels could not be mixed. 
      if FChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if Assigned(FPencilMask) then
        begin
          SetPencilStipplePattern(FPencilMask, frmMain.GlobalPenStyle,
                                  clWhite32, clWhite32);
        end;
      end;
    end;

    // used to track the processed pixels by the pencil tool
    if Assigned(FPencilMask) then
    begin
      FPencilMask.SetSize(FSelection.CutOriginal.Width,
                          FSelection.CutOriginal.Height);
      
      FPencilMask.Clear(clBlack32);
      FPencilMask.PenColor := clWhite32;
    end;
  end
  else
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.PenColor := Color32(frmMain.ForeGrayColor);

          if frmMain.GlobalPenStyle <> psSolid then
          begin
            SetPencilStipplePattern(
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              frmMain.GlobalPenStyle, FGColor, BKColor);
          end;
        end;

      ctQuickMaskChannel:
        begin
          FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.PenColor := Color32(frmMain.ForeGrayColor);

          if frmMain.GlobalPenStyle <> psSolid then
          begin
            SetPencilStipplePattern(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              frmMain.GlobalPenStyle, FGColor, BKColor);
          end;
        end;

      ctLayerMaskChannel:
        begin
          FLayerList.SelectedLayer.MaskBitmap.PenColor := Color32(frmMain.ForeGrayColor);

          if frmMain.GlobalPenStyle <> psSolid then
          begin
            SetPencilStipplePattern(FLayerList.SelectedLayer.MaskBitmap,
                                    frmMain.GlobalPenStyle, FGColor, BKColor);
          end;
        end;

      ctColorChannel:
        begin
          FLayerList.SelectedLayer.LayerBitmap.PenColor := Color32(frmMain.GlobalForeColor);

          if frmMain.GlobalPenStyle <> psSolid then
          begin
            SetPencilStipplePattern(FLayerList.SelectedLayer.LayerBitmap,
                                    frmMain.GlobalPenStyle, FGColor, BKColor);

            // The following code is very weird, it should draws a white solid line,
            // not a stippled line. But we can't without this line, otherwise,
            // the channels of processed pixels could not be mixed.
            if Assigned(FPencilMask) then
            begin
              SetPencilStipplePattern(FPencilMask, frmMain.GlobalPenStyle,
                                      clWhite32, clWhite32);
            end;
          end;

          // used to track the processed pixels by the pencil tool
          if Assigned(FPencilMask) then
          begin
            FPencilMask.SetSize(FLayerList.SelectedLayer.LayerBitmap.Width,
                                FLayerList.SelectedLayer.LayerBitmap.Height);

            FPencilMask.Clear(clBlack32);
            FPencilMask.PenColor := clWhite32;
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.SetPencilStipplePattern(ADestBmp: TBitmap32;
  const APenStyle: TPenStyle; const AColor1, AColor2: TColor32);
begin
  ADestBmp.BeginUpdate;
  try
    case APenStyle of
      psDash:
        begin
          ADestBmp.StippleStep := 0.75;

          ADestBmp.SetStipple([AColor1, AColor1, AColor1, AColor1, AColor1, AColor1,
                               AColor1, AColor1, AColor1, AColor1, AColor1, AColor1,
                               AColor1, AColor1, AColor1, AColor1, AColor1, AColor1,
                               AColor2, AColor2, AColor2, AColor2, AColor2, AColor2]);
        end;

      psDot:
        begin
          ADestBmp.StippleStep := 0.5;
          ADestBmp.SetStipple([AColor1, AColor1, AColor2, AColor2]);
        end;

      psDashDot:
        begin
          ADestBmp.StippleStep := 0.5;

          ADestBmp.SetStipple([AColor1, AColor1, AColor1, AColor1, AColor1, AColor1,
                               AColor2, AColor2, AColor1, AColor1, AColor2, AColor2]);
        end;

      psDashDotDot:
        begin
          ADestBmp.StippleStep := 0.5;
          
          ADestBmp.SetStipple([AColor1, AColor1, AColor1, AColor1, AColor1, AColor1,
                               AColor2, AColor2, AColor1, AColor1, AColor2, AColor2,
                               AColor1, AColor1, AColor2, AColor2]);
        end;
    end;
  finally
    ADestBmp.EndUpdate;
  end;
end;

// connect mouse events to TImage32 component
procedure TfrmChild.ConnectMouseEventsToImage;
begin
  imgWorkArea.OnMouseDown := nil;
  imgWorkArea.OnMouseMove := nil;
  imgWorkArea.OnMouseUp   := nil;
  
  case frmMain.MainTool of
    gmtStandard:
      begin
        if frmMain.StandardTool = gstPencil then
        begin
          imgWorkArea.OnMouseDown := PencilMouseDown;
          imgWorkArea.OnMouseMove := PencilMouseMove;
          imgWorkArea.OnMouseUp   := PencilMouseUp;
        end
        else
        if frmMain.StandardTool in [gstStraightLine,
                                    gstBezierCurve,
                                    gstPolygon,
                                    gstRegularPolygon,
                                    gstRectangle,
                                    gstRoundRectangle,
                                    gstEllipse] then
        begin
          imgWorkArea.OnMouseDown := FigureToolsMouseDown;
          imgWorkArea.OnMouseMove := FigureToolsMouseMove;
          imgWorkArea.OnMouseUp   := FigureToolsMouseUp;
        end
        else
        if frmMain.StandardTool in [gstMoveObjects,
                                    gstPartiallySelect,
                                    gstTotallySelect] then
        begin
          imgWorkArea.OnMouseDown := FigureManagingToolsMouseDown;
          imgWorkArea.OnMouseMove := FigureManagingToolsMouseMove;
          imgWorkArea.OnMouseUp   := FigureManagingToolsMouseUp;
        end;
      end;

    gmtBrush:
      begin
        imgWorkArea.OnMouseDown := BrushMouseDown;
        imgWorkArea.OnMouseMove := BrushMouseMove;
        imgWorkArea.OnMouseUp   := BrushMouseUp;
      end;

    gmtMarquee:
      begin
        imgWorkArea.OnMouseDown := MarqueeMouseDown;
        imgWorkArea.OnMouseMove := MarqueeMouseMove;
        imgWorkArea.OnMouseUp   := MarqueeMouseUp;
      end;

    gmtGradient:
      begin
        imgWorkArea.OnMouseDown := GradientToolsMouseDown;
        imgWorkArea.OnMouseMove := GradientToolsMouseMove;
        imgWorkArea.OnMouseUp   := GradientToolsMouseUp;
      end;

    gmtCrop:
      begin
        imgWorkArea.OnMouseDown := CropMouseDown;
        imgWorkArea.OnMouseMove := CropMouseMove;
        imgWorkArea.OnMouseUp   := CropMouseUp;
      end;

    gmtPaintBucket:
      begin
        imgWorkArea.OnMouseDown := PaintBucketMouseDown;
        imgWorkArea.OnMouseMove := PaintBucketMouseMove;
        imgWorkArea.OnMouseUp   := PaintBucketMouseUp;
      end;

    gmtEraser:
      begin
        imgWorkArea.OnMouseDown := EraserMouseDown;
        imgWorkArea.OnMouseMove := EraserMouseMove;
        imgWorkArea.OnMouseUp   := EraserMouseUp;
      end;

    gmtPenTools:
      begin
        imgWorkArea.OnMouseDown := PenToolsMouseDown;
        imgWorkArea.OnMouseMove := PenToolsMouseMove;
        imgWorkArea.OnMouseUp   := PenToolsMouseUp;
      end;

    gmtMeasure:
      begin
        imgWorkArea.OnMouseDown := MeasureMouseDown;
        imgWorkArea.OnMouseMove := MeasureMouseMove;
        imgWorkArea.OnMouseUp   := MeasureMouseUp;
      end;

    gmtShape:
      begin
        imgWorkArea.OnMouseDown := ShapeRegionToolsMouseDown;
        imgWorkArea.OnMouseMove := ShapeRegionToolsMouseMove;
        imgWorkArea.OnMouseUp   := ShapeRegionToolsMouseUp;
      end;

    gmtTextTool:
      begin
        imgWorkArea.OnMouseDown := TextToolsMouseDown;
        imgWorkArea.OnMouseMove := TextToolsMouseMove;
        imgWorkArea.OnMouseUp   := TextToolsMouseUp;
      end;

    gmtEyedropper:
      begin
        imgWorkArea.OnMouseDown := EyedropperMouseDown;
        imgWorkArea.OnMouseMove := EyedropperMouseMove;
        imgWorkArea.OnMouseUp   := EyedropperMouseUp;
      end;

    gmtHandTool:
      begin
        imgWorkArea.OnMouseDown := HandToolMouseDown;
        imgWorkArea.OnMouseMove := HandToolMouseMove;
        imgWorkArea.OnMouseUp   := HandToolMouseUp;
      end;
  end;
end;

// change cursor according to the main tools
procedure TfrmChild.ChangeImageCursorByToolTemplets;
begin
  imgWorkArea.Cursor := crCross;  

  case frmMain.MainTool of
    gmtStandard:
      begin
        ChangeImageCursorByStandardTools();
      end;
      
    gmtEraser:
      begin
        ChangeImageCursorByEraserTools();
      end;
      
    gmtPenTools:
      begin
        ChangeImageCursorByPenTools();
      end;
      
    gmtMarquee:
      begin
        ChangeImageCursorByMarqueeTools();
      end;

    gmtCrop:
      begin
        imgWorkArea.Cursor := crCrop;
      end;

    gmtMeasure:
      begin
        imgWorkArea.Cursor := crMeasure;  
      end;
      
    gmtPaintBucket:
      begin
        imgWorkArea.Cursor := crPaintBucket;  
      end;
      
    gmtShape:
      begin
        ChangeImageCursorByShapeTools();
      end;
      
    gmtEyedropper:
      begin
        imgWorkArea.Cursor := crEyedropper;
      end;
      
    gmtHandTool:
      begin
        imgWorkArea.Cursor := crHandLoosen;
      end;
  end;
end;

procedure TfrmChild.SaveOldCurvePathForUndoRedo;
begin
  if Assigned(FOldCurvePathList) then
  begin
    FOldCurvePathList.Free();
    FOldCurvePathList := nil;
  end;

  if Assigned(FPathList.SelectedPath) then
  begin
    FOldCurvePathList := TgmCurvePathList.Create();
    FOldCurvePathList.AssignCurvePathListData(FPathList.SelectedPath.CurvePathList);
  end;
end;

// Show/Hide assistant layers -- FPathLayer 
procedure TfrmChild.SetAssistantLayerVisible(const IsVisible: Boolean);
begin
  if Assigned(FPathLayer) then
  begin
    FPathLayer.Visible := IsVisible;
  end;
end;

procedure TfrmChild.SaveNamedFile;
var
  LOutputBitmap    : TBitmap32;
  LColorReducedBmp : TBitmap;
  LTempBmp         : TBitmap;
  LExtensionName   : string;
  LGMDManager      : TgmGMDManager;
begin
  LColorReducedBmp := nil;

  if FFileName <> '' then
  begin
    Screen.Cursor := crHourGlass;
    try
      LExtensionName := Lowercase( ExtractFileExt(FFileName) );

      if LExtensionName = '.bmp' then
      begin
        frmMain.OutputGraphicsFormat := ogfBMP;
      end
      else if LExtensionName = '.jpg' then
      begin
        frmMain.OutputGraphicsFormat := ogfJPG;
      end
      else if LExtensionName = '.gif' then
      begin
        frmMain.OutputGraphicsFormat := ogfGIF;
      end
      else if LExtensionName = '.png' then
      begin
        frmMain.OutputGraphicsFormat := ogfPNG;
      end
      else if LExtensionName = '.tif' then
      begin
        frmMain.OutputGraphicsFormat := ogfTIF;
      end
      else if LExtensionName = '.gmd' then
      begin
        frmMain.OutputGraphicsFormat := ogfGMD;
      end;

      if frmMain.OutputGraphicsFormat = ogfGMD then
      begin
        LGMDManager := TgmGMDManager.Create();
        try
          // link pointers to the gmd manager
          LGMDManager.LayerList      := FLayerList;
          LGMDManager.ChannelManager := FChannelManager;
          LGMDManager.PathList       := FPathList;

          LGMDManager.SaveToFile(FFileName);
          Self.RefreshCaption();
          FImageProcessed := False;
        finally
          LGMDManager.Free();
        end;
      end
      else
      begin
        LOutputBitmap := TBitmap32.Create();
        try
          LOutputBitmap.Assign(FLayerList.CombineResult);
          LOutputBitmap.DrawMode := dmBlend;

          case frmMain.OutputGraphicsFormat of
            ogfBMP,
            ogfJPG,
            ogfTIF:
              begin
                MergeBitmapToColoredBackground(LOutputBitmap, clWhite32);
              end;

            ogfGIF:
              begin
                MergeBitmapToColoredBackground(LOutputBitmap, clWhite32);
                SetAssistantLayerVisible(False);

                frmIndexedColor := TfrmIndexedColor.Create(Application);
                LTempBmp        := TBitmap.Create();
                try
                  LTempBmp.Assign(LOutputBitmap);

                  // to open the frmIndexedColor dialog, we need to assign value to FBitmapBefore
                  frmMain.FBitmapBefore.Assign(LOutputBitmap);

                  if frmIndexedColor.ShowModal = mrOK then
                  begin
                    LColorReducedBmp := ReduceColors(LTempBmp,
                      frmIndexedColor.ColorReduction, frmIndexedColor.DitherMode,
                      GIFImageDefaultColorReductionBits, 0);

                    if Assigned(LColorReducedBmp) then
                    begin
                      LOutputBitmap.Assign(LColorReducedBmp);
                    end;
                  end;

                finally
                  FreeAndNil(frmIndexedColor);
                  LTempBmp.Free();

                  if Assigned(LColorReducedBmp) then
                  begin
                    LColorReducedBmp.Free();
                  end;
                end;
              end;
          end;

          frmMain.FBitmapBefore.Assign(LOutputBitmap);
          SaveGraphicsFile(FFileName, LOutputBitmap);
          RefreshCaption();

          if frmMain.svpctrdlgSavePictures.FilterIndex = 3 then
          begin
            SetAssistantLayerVisible(True);
          end;

          FImageProcessed := False;
        finally
          LOutputBitmap.Free();
        end;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else
  begin
    SaveFileWithNewName();
  end;
end;

procedure TfrmChild.SaveFileWithNewName;
var
  LExtensionName: string;
  LShortFileName: string;
begin
  if FFileName = '' then
  begin
    frmMain.OutputGraphicsFormat              := ogfBMP;
    frmMain.svpctrdlgSavePictures.FilterIndex := Ord(ogfBMP);
    frmMain.svpctrdlgSavePictures.FileName    := 'Untitled';
  end
  else
  begin
    LExtensionName := Lowercase( ExtractFileExt(FFileName) );
    LShortFileName := ExtractFileName(FFileName);

    if LExtensionName = '.bmp' then
    begin
      frmMain.OutputGraphicsFormat := ogfBMP;
    end
    else if LExtensionName = '.jpg' then
    begin
      frmMain.OutputGraphicsFormat := ogfJPG;
    end
    else if LExtensionName = '.gif' then
    begin
      frmMain.OutputGraphicsFormat := ogfGIF;
    end
    else if LExtensionName = '.png' then
    begin
      frmMain.OutputGraphicsFormat := ogfPNG;
    end
    else if LExtensionName = '.tif' then
    begin
      frmMain.OutputGraphicsFormat := ogfTIF;
    end
    else if LExtensionName = '.gmd' then
    begin
      frmMain.OutputGraphicsFormat := ogfGMD;
    end;

    frmMain.svpctrdlgSavePictures.FilterIndex := Ord(frmMain.OutputGraphicsFormat);

    if LExtensionName <> '' then
    begin
      frmMain.svpctrdlgSavePictures.FileName := Copy( LShortFileName, 1, Length(LShortFileName) - 4 );
    end
    else
    begin
      frmMain.svpctrdlgSavePictures.FileName := LShortFileName;
    end;
  end;

  if frmMain.svpctrdlgSavePictures.Execute then
  begin
    frmMain.OutputGraphicsFormat :=
      TgmOutputGraphicsFormat(frmMain.svpctrdlgSavePictures.FilterIndex);

    FFileName      := frmMain.svpctrdlgSavePictures.Filename;
    LExtensionName := Lowercase( ExtractFileExt(FFileName) );

    if LExtensionName = '' then
    begin
      case frmMain.OutputGraphicsFormat of
        ogfBMP: FFileName := FFileName + '.bmp';
        ogfJPG: FFileName := FFileName + '.jpg';
        ogfGIF: FFileName := FFileName + '.gif';
        ogfPNG: FFileName := FFileName + '.png';
        ogfTIF: FFileName := FFileName + '.tif';
        ogfGMD: FFileName := FFileName + '.gmd';
      end;
    end
    else
    begin
      case frmMain.OutputGraphicsFormat of
        ogfBMP:
          begin
            if LExtensionName <> '.bmp' then
            begin
              FFileName := ChangeFileExt(FFileName, '.bmp');
            end;
          end;

        ogfJPG:
          begin
            if LExtensionName <> '.jpg' then
            begin
              FFileName := ChangeFileExt(FFileName, '.jpg');
            end;
          end;

        ogfGIF:
          begin
            if LExtensionName <> '.gif' then
            begin
              FFileName := ChangeFileExt(FFileName, '.gif');
            end;
          end;

        ogfPNG:
          begin
            if LExtensionName <> '.png' then
            begin
              FFileName := ChangeFileExt(FFileName, '.png');
            end;
          end;

        ogfTIF:
          begin
            if LExtensionName <> '.tif' then
            begin
              FFileName := ChangeFileExt(FFileName, '.tif');
            end;
          end;

        ogfGMD:
          begin
            if LExtensionName <> '.gmd' then
            begin
              FFileName := ChangeFileExt(FFileName, '.gmd');
            end;
          end;
      end;
    end;

    if FileExists(FFileName) then
    begin
      if MessageDlg('File: ' + FFileName + ' is already exists.' + #10#13 +
                    'Do you want to replace it?',
                    mtConfirmation, mbOKCancel, 0) <> mrOK then
      begin
        Exit;
      end;
    end;

    SaveNamedFile;
  end;
end;

procedure TfrmChild.DeleteCurrentLayer;
var
  LLayerName : string;
  LCommand   : TgmCustomCommand;
begin
  if FLayerList.Count > 1 then
  begin
    LLayerName := '"' + FLayerList.SelectedLayer.LayerName + '"';

    if MessageDlg('Delete the layer ' + LLayerName + '?',
                  mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    begin
      FFigureManager.DeselectAllFigures();

      // create Undo/Redo first
      LCommand := TgmDeleteLayerCommand.Create(FChannelManager, FLayerList,
        FLayerList.SelectedLayer, FLayerList.SelectedIndex);

      // delete the layer
      FLayerList.DeleteSelectedLayer();

      FCommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmChild.LoadDataFromGMDFile(const AFileName: string);
var
  LGMDManager : TgmGMDManager;
  LRect       : TRect;
begin
  if not FileExists(AFileName) then
  begin
    Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  try
    // delete selection
    if Assigned(FSelection) then
    begin
      CommitSelection();
    end;

    // deselect figures
    FFigureManager.DeselectAllFigures();

    // cancel crop
    if Assigned(Crop) then
    begin
      CancelCrop();
    end;

    // delete handle layers...
    DeletePathLayer();
    RemoveMeasureLayer();
    RemoveMeasureLine();

    // delete paths...
    PathList.DeselectAllPaths();
    PathList.Clear();
    FCurvePath := nil;

    // delete channels...
    FChannelManager.SelectColorChannel(0, False);
    FChannelManager.AlphaChannelList.Clear();
    FChannelManager.DeleteLayerMaskChannel();
    FChannelManager.DeleteQuickMaskChannel();

    // delete layers...
    FLayerList.Clear();

    // read the data in...
    LGMDManager := TgmGMDManager.Create();
    try
      // link pointers to the gmd manager
      LGMDManager.LayerList      := FLayerList;
      LGMDManager.ChannelManager := FChannelManager;
      LGMDManager.PathList       := FPathList;
      LGMDManager.TextEditor     := frmRichTextEditor.rchedtRichTextEditor;

      if LGMDManager.LoadFromFile(AFileName) then
      begin
        with ActiveChildForm do
        begin
          // set background size before create background layer
          imgWorkArea.Bitmap.SetSize(HistoryBitmap.Width, HistoryBitmap.Height);
          imgWorkArea.Bitmap.Clear($00000000);

          // draw checkerboard pattern
          CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
          DrawCheckerboardPattern(CheckerboardBmp, CheckerboardBmp.ClipRect);

          SetCallbacksForLayersInList();

          // set location for channel layers ...
          LRect             := imgWorkArea.GetBitmapRect();
          LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
          LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

          ChannelManager.ChannelLayerLocation := FloatRect(LRect);
          if LayerList.SelectedLayer.LayerProcessStage = lpsMask then
          begin
            ChannelManager.SelectLayerMaskChannel();
          end;

          // update the view
          LayerList.SelectedLayer.Changed();

          // channels ...
          LRect             := imgWorkArea.GetBitmapRect();
          LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
          LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

          ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);
          ChannelManager.ChannelLayerLocation := FloatRect(LRect);
        end;

        frmMain.stsbrMain.Panels[0].Text := GetBitmapDimensionString(ActiveChildForm.FHistoryBitmap);
      end;

    finally
      LGMDManager.Free();
    end;   

  finally
    Screen.Cursor := crDefault;
  end;
end;

// refresh the caption of this form
procedure TfrmChild.RefreshCaption;
var
  s: string;
begin
  if FFileName = '' then
  begin
    s := 'Untitled';
  end
  else
  begin
    s := ExtractFileName(FFileName);
  end;

  Caption := s + ' @ ' + IntToStr( Round(imgWorkArea.Scale * 100) ) + '%';
end;

// finish current curve definition and prepare for drawing new one
procedure TfrmChild.FinishCurves;
begin
  if FDrawCurveTime > 0 then
  begin
    InitializeCanvasesForFigureTools();

    if Assigned(FSelection) then
    begin
      if (FChannelManager.CurrentChannelType = ctColorChannel) and
         (not (FLayerList.SelectedLayer is TgmNormalLayer)) then
      begin
        FinishCurveOnLayer();
      end
      else
      begin
        FinishCurveOnSelection();
      end;
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel,
        ctQuickMaskChannel,
        ctLayerMaskChannel:
          begin
            FinishCurveOnSpecialChannels();
          end;

        ctColorChannel:
          begin
            FinishCurveOnLayer();
          end;
      end;
    end;
    
    FDrawCurveTime := 0;
  end;
end;

// finish current polygon definition and prepare for drawing new one
procedure TfrmChild.FinishPolygon;
begin
  if Length(FPolygon) > 2 then
  begin
    InitializeCanvasesForFigureTools();

    if Assigned(FSelection) then
    begin
      if (FChannelManager.CurrentChannelType = ctColorChannel) and
         (not (FLayerList.SelectedLayer is TgmNormalLayer)) then
      begin
        FinishPolygonOnLayer();
      end
      else
      begin
        FinishPolygonOnSelection();
      end;
    end
    else
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel,
        ctQuickMaskChannel,
        ctLayerMaskChannel:
          begin
            FinishPolygonOnSpecialChannels();
          end;

        ctColorChannel:
          begin
            FinishPolygonOnLayer();
          end;
      end;
    end;
  end;

  FPolygon       := nil;
  FActualPolygon := nil;

  // refresh the screen
  imgWorkArea.Bitmap.Changed();
end;

procedure TfrmChild.ChangeImageCursorByStandardTools;
begin
  imgWorkArea.Cursor := crCross;
  
  case frmMain.StandardTool of
    gstMoveObjects:
      begin
        imgWorkArea.Cursor := crMoveSelection;
      end;

    gstPartiallySelect,
    gstTotallySelect:
      begin
        imgWorkArea.Cursor := crCross;
      end;
  end;
end;

procedure TfrmChild.CommitSelection;
begin
  if Assigned(FSelection) then
  begin
    if FSelection.HasShadow then
    begin
      Screen.Cursor := crHourGlass;
      try
        FSelection.IsAnimated := False;

        FreeAndNil(FSelection);
        ChangeImageCursorByMarqueeTools;

        // update the viewport to clear the Marching-Ants lines left by
        // the previous selection
        imgWorkArea.Changed;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmChild.CancelSelection;
begin
  if Assigned(FLayerList.SelectedLayer) then
  begin
    if Assigned(FSelection) then
    begin
      if FChannelManager.CurrentChannelType = ctAlphaChannel then
      begin
        if not Assigned(FChannelManager.SelectedAlphaChannel) then
        begin
          MessageDlg('Could not process more than one alpha channels at a time.', mtError, [mbOK], 0);
          Exit;
        end;
      end;

      FSelection.IsAnimated := False;

      if FSelection.HasShadow then
      begin
        Screen.Cursor := crHourGlass;
        try
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                with FChannelManager.SelectedAlphaChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                  ChannelLayer.Bitmap.Changed();
                  UpdateChannelThumbnail();
                end;
              end;

            ctQuickMaskChannel:
              begin
                with FChannelManager.QuickMaskChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                  ChannelLayer.Bitmap.Changed();
                  UpdateChannelThumbnail();
                end;
              end;

            ctLayerMaskChannel:
              begin
                with FLayerList.SelectedLayer do
                begin
                  MaskBitmap.Assign(FSelection.SourceBitmap);
                  Changed();
                  UpdateMaskThumbnail();
                end;

                if Assigned(FChannelManager.LayerMaskChannel) then
                begin
                  FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                    0, 0, FLayerList.SelectedLayer.MaskBitmap);

                  FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
                end;
              end;

            ctColorChannel:
              begin
                // must be on layer

                if FLayerList.SelectedLayer is TgmNormalLayer then
                begin
                  FLayerList.SelectedLayer.LayerBitmap.Assign(FSelection.SourceBitmap);
                  FLayerList.SelectedLayer.Changed();
                  FLayerList.SelectedLayer.UpdateLayerThumbnail();
                end
                else
                begin
                  // refresh the view
                  FLayerList.SelectedLayer.Changed();
                end;
              end;
          end;

          FreeAndNil(FSelection);

          // Clear the selection border that drawn on the buffer of the
          // image control. Check imgWorkAreaPaintStage() for details. 
          imgWorkArea.Update(imgWorkArea.ClientRect);
          
          ChangeImageCursorByMarqueeTools();
        finally
          Screen.Cursor := crDefault;
        end;
      end
      else
      begin
        // if the selection has no "shadow"
        FreeAndNil(FSelection);
        imgWorkArea.Update(imgWorkArea.ClientRect);
      end;
    end;
  end;
end;

procedure TfrmChild.DeleteSelection;
begin
  if Assigned(FLayerList.SelectedLayer) then
  begin
    if Assigned(FSelection) then
    begin
      if FSelection.HasShadow then
      begin
        if FChannelManager.CurrentChannelType = ctAlphaChannel then
        begin
          if not Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            MessageDlg('Could not process more than one alpha channels at a time.',
                       mtError, [mbOK], 0);
            Exit;
          end;
        end;

        Screen.Cursor := crHourGlass;
        try
          FSelection.IsAnimated := False;

          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                with FChannelManager.SelectedAlphaChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.Background);
                  ChannelLayer.Bitmap.Changed();
                  UpdateChannelThumbnail();
                end;
              end;

            ctQuickMaskChannel:
              begin
                with FChannelManager.QuickMaskChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.Background);
                  ChannelLayer.Bitmap.Changed();
                  UpdateChannelThumbnail();
                end;
              end;

            ctLayerMaskChannel:
              begin
                with FLayerList.SelectedLayer do
                begin
                  MaskBitmap.Assign(FSelection.Background);
                  Changed();
                  UpdateMaskThumbnail();
                end;

                // update the layer mask channel layer
                if Assigned(FChannelManager.LayerMaskChannel) then
                begin
                  FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                    0, 0, FLayerList.SelectedLayer.MaskBitmap);

                  FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
                end;
              end;

            ctColorChannel:
              begin
                // must be on layer

                if FLayerList.SelectedLayer is TgmNormalLayer then
                begin
                  with FLayerList.SelectedLayer do
                  begin
                    LayerBitmap.Assign(FSelection.Background);
                    Changed();
                    UpdateLayerThumbnail();
                  end;
                end
                else
                begin
                  // refresh the view
                  FLayerList.SelectedLayer.Changed();
                end;
              end;
          end;

          FreeAndNil(FSelection);
          imgWorkArea.Update(imgWorkArea.ClientRect);
          ChangeImageCursorByMarqueeTools();  
        finally
          Screen.Cursor := crDefault;
        end;
      end;
    end;
  end;
end;

procedure TfrmChild.FreeSelection;
begin
  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;
    FreeAndNil(FSelection);
  end;
end;

procedure TfrmChild.FreeCopySelection;
begin
  if Assigned(FSelectionCopy) then
  begin
    FreeAndNil(FSelectionCopy);
  end;
end;

procedure TfrmChild.CreateNewSelection;
begin
  if Assigned(FSelection) then
  begin
    FreeAndNil(FSelection);
  end;

  FSelection := TgmSelection.Create(imgWorkArea);
end;

procedure TfrmChild.CreateCopySelection;
begin
  if Assigned(FSelectionCopy) then
  begin
    FreeAndNil(FSelectionCopy);
  end;

  FSelectionCopy := TgmSelection.Create(imgWorkArea);
end;

procedure TfrmChild.CreateSelectionForAll;
begin
  if FChannelManager.CurrentChannelType = ctAlphaChannel then
  begin
    // if the SelectedAlphaChannel is nil, indicating that the user selected
    // more than one alpha channels
    if FChannelManager.SelectedAlphaChannel = nil then
    begin
      MessageDlg('Could not process more than one alpha channels at a time.',
                 mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if frmMain.MainTool <> gmtMarquee then
  begin
    frmMain.spdbtnMarqueeTools.Down := True;
    frmMain.ChangeMainToolClick(frmMain.spdbtnMarqueeTools);
  end;

  // if the selection has not been created, then create one
  if FSelection = nil then
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
        end;

      ctQuickMaskChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
        end;

      ctLayerMaskChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FLayerList.SelectedLayer.MaskBitmap);
        end;

      ctColorChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FLayerList.SelectedLayer.LayerBitmap);
        end;
    end;
  end;

  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;

    FSelection.SelectAll;
    FSelection.GetActualMaskBorder;
    FSelection.CutRegionFromOriginal;
    FSelection.GetForeground;
    FSelection.GetMarchingAntsLines;

    // filling the background that under the selection, and showing the selection
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          FSelection.GetBackgroundWithFilledColor(
            Color32(frmMain.BackGrayColor), [csGrayscale] );
        end;

      ctQuickMaskChannel:
        begin
          FSelection.GetBackgroundWithFilledColor(
            Color32(frmMain.BackGrayColor), [csGrayscale] );
        end;

      ctLayerMaskChannel:
        begin
          FSelection.GetBackgroundWithFilledColor(
            Color32(frmMain.BackGrayColor), [csGrayscale] );
        end;

      ctColorChannel:
        begin
          // must be on layer

          if FLayerList.SelectedLayer is TgmNormalLayer then
          begin
            if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.GlobalBackColor),
                FChannelManager.SelectedColorChannels );
            end
            else
            begin
              if (csRed   in FChannelManager.SelectedColorChannels) and
                 (csGreen in FChannelManager.SelectedColorChannels) and
                 (csBlue  in FChannelManager.SelectedColorChannels) then
              begin
                FSelection.GetBackgroundWithTransparent();
              end
              else
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end;
            end;
          end;
        end;
    end;

    FSelection.IsAnimated := True;
    imgWorkArea.Changed; // render control border of selection
  end;
end;

procedure TfrmChild.CreateSelectionByColorRange(ASourceBmp: TBitmap32;
  const ASampledColor: TColor32; const AFuzziness: Integer);
begin
  if frmMain.MainTool <> gmtMarquee then
  begin
    frmMain.spdbtnMarqueeTools.Down := True;
    frmMain.ChangeMainToolClick(frmMain.spdbtnMarqueeTools);
  end;

  // if the selection has not been created, create one.
  if FSelection = nil then
  begin
    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
          end;
        end;

      ctQuickMaskChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
        end;

      ctLayerMaskChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FLayerList.SelectedLayer.MaskBitmap);
        end;

      ctColorChannel:
        begin
          FSelection := TgmSelection.Create(imgWorkArea,
            FLayerList.SelectedLayer.LayerBitmap);
        end;
    end;
  end;

  if Assigned(FSelection) then
  begin
    FSelection.CreateColorRangeRGN(ASourceBmp, ASampledColor, AFuzziness);
    FSelection.GetActualMaskBorder;

    // if the selection has selected area...
    if FSelection.HasShadow then
    begin
      FSelection.Background.Assign(FSelection.SourceBitmap);
      FSelection.CutRegionFromOriginal;
      FSelection.GetForeground;
      FSelection.GetMarchingAntsLines;

      // filling background that beneath the selection
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctQuickMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctLayerMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end
              else
              begin
                if (csRed   in FChannelManager.SelectedColorChannels) and
                   (csGreen in FChannelManager.SelectedColorChannels) and
                   (csBlue  in FChannelManager.SelectedColorChannels) then
                begin
                  FSelection.GetBackgroundWithTransparent();
                end
                else
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end;
              end;
            end;
          end;
      end;
    end
    else
    begin
      // if the selection does not have the selected area, then delete the it
      MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              with FChannelManager.SelectedAlphaChannel do
              begin
                ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
              end;
            end;
          end;

        ctQuickMaskChannel:
          begin
            with FChannelManager.QuickMaskChannel do
            begin
              ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            with FLayerList.SelectedLayer do
            begin
              MaskBitmap.Assign(FSelection.SourceBitmap);
            end;
          end;

        ctColorChannel:
          begin
            with FLayerList.SelectedLayer do
            begin
              LayerBitmap.Assign(FSelection.SourceBitmap);
            end;
          end;
      end;

      FreeAndNil(FSelection);
    end;
  end;
end;

procedure TfrmChild.MakeSelectionInverse;
begin
  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;
    FSelection.InvertSelection;
    FSelection.GetActualMaskBorder;

    // if the selection has selected area...
    if FSelection.HasShadow then
    begin
      FSelection.CutRegionFromOriginal;
      FSelection.GetForeground;
      FSelection.GetMarchingAntsLines;

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctQuickMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctLayerMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );
          end;

        ctColorChannel:
          begin
            // must be on layer

            // filling background that beneath the selection
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end
              else
              begin
                if (csRed   in FChannelManager.SelectedColorChannels) and
                   (csGreen in FChannelManager.SelectedColorChannels) and
                   (csBlue  in FChannelManager.SelectedColorChannels) then
                begin
                  FSelection.GetBackgroundWithTransparent();
                end
                else
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end;
              end;
            end;
          end;
      end;

      FSelection.IsAnimated := True;
    end
    else
    begin
      // if the selection does not have the selected area, then delete the it
      MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);
      FreeAndNil(FSelection);
    end;
  end;

  // render or clear control border of selection
  imgWorkArea.Changed;
end;

function TfrmChild.MakeSelectionFeather(const ARadius: Integer): Boolean;
var
  LNewFeatherRadius : Integer;
  LOldFeatherRadius : Integer;
begin
  Result := False;

  if Assigned(FSelection) then
  begin
    LOldFeatherRadius := FSelection.FeatherRadius;
    LNewFeatherRadius := ARadius;

    // feathering selection
    FSelection.FeatherRadius := LNewFeatherRadius;

    // NOTE: if the FeatherRadius is set a value that is greater then zero,
    // and the GetActualMaskBorder() finds that after added the FeatherRadius,
    // the selection border will out of the image border, the function
    // will set the FeatherRadius back to zero.
    FSelection.GetActualMaskBorder;

    // set feather radius failed...
    if (LNewFeatherRadius > 0) and (FSelection.FeatherRadius = 0) then
    begin
      MessageDlg('The feather radius is out of the range.', mtError, [mbOK], 0);

      FSelection.FeatherRadius := LOldFeatherRadius;

      // get the old border
      FSelection.GetActualMaskBorder;
    end
    else
    begin
      FSelection.CutRegionFromOriginal;
      FSelection.GetForeground;
      FSelection.GetMarchingAntsLines;

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );

            FSelection.ShowSelection(
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              [csGrayscale]);

            FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
            FChannelManager.SelectedAlphaChannel.UpdateChannelThumbnail;
          end;

        ctQuickMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );

            FSelection.ShowSelection(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              [csGrayscale]);

            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
            FChannelManager.QuickMaskChannel.UpdateChannelThumbnail;
          end;

        ctLayerMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );

            FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap, [csGrayscale]);

            FLayerList.SelectedLayer.Changed;
            FLayerList.SelectedLayer.UpdateLayerThumbnail;
            FLayerList.SelectedLayer.UpdateMaskThumbnail;

            if Assigned(FChannelManager.LayerMaskChannel) then
            begin
              FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                0, 0, FLayerList.SelectedLayer.MaskBitmap);

              FChannelManager.LayerMaskChannel.UpdateChannelThumbnail;
            end;
          end;

        ctColorChannel:
          begin
            // filling background that beneath the selection
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end
              else
              begin
                if (csRed   in FChannelManager.SelectedColorChannels) and
                   (csGreen in FChannelManager.SelectedColorChannels) and
                   (csBlue  in FChannelManager.SelectedColorChannels) then
                begin
                  FSelection.GetBackgroundWithTransparent();
                end
                else
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end;
              end;

              FSelection.ShowSelection(
                FLayerList.SelectedLayer.LayerBitmap,
                FChannelManager.SelectedColorChannels);

              FLayerList.SelectedLayer.Changed;
              FLayerList.SelectedLayer.UpdateLayerThumbnail;
            end;
          end;
      end;

      FSelection.IsFeathered := True;
      Result                 := True;
    end;

    // render control border of selection
    imgWorkArea.Changed;
  end;
end;

// finish current definition of the polyonal selection and preparing for new one
procedure TfrmChild.FinishPolygonalSelection;
var
  LPolygonalRegion : TgmPolygonalRegion;
begin
  if Assigned(FLayerList) and
     Assigned(FLayerList.SelectedLayer) and
     Assigned(FRegion) then
  begin
    if FRegion.RegionStyle = gmrsPolygonal then
    begin
      // remember the old selection (if any) for Undo/Redo
      if Assigned(FSelection) and FSelection.HasShadow then
      begin
        if FSelectionCopy = nil then
        begin
          FSelectionCopy := TgmSelection.Create(imgWorkArea);
        end;

        FSelectionCopy.AssignAllSelectionData(FSelection);
      end
      else
      begin
        if Assigned(FSelectionCopy) then
        begin
          FreeAndNil(FSelectionCopy);
        end;
      end;

      LPolygonalRegion := TgmPolygonalRegion(FRegion);

      if not LPolygonalRegion.IsRegionDefineCompleted then
      begin
        // force to close the polygonal region
        LPolygonalRegion.ClosePolgonalRegion();
      end;

      if LPolygonalRegion.IsValidRegion then
      begin
        // create the polygonal selection
        if FSelection = nil then
        begin
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                if Assigned(FChannelManager.SelectedAlphaChannel) then
                begin
                  FSelection := TgmSelection.Create(imgWorkArea,
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
                end;
              end;

            ctQuickMaskChannel:
              begin
                FSelection := TgmSelection.Create(imgWorkArea,
                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
              end;

            ctLayerMaskChannel:
              begin
                FSelection := TgmSelection.Create(imgWorkArea,
                  FLayerList.SelectedLayer.MaskBitmap);
              end;

            ctColorChannel:
              begin
                // must be on a layer
                FSelection := TgmSelection.Create(imgWorkArea,
                  FLayerList.SelectedLayer.LayerBitmap);
              end;
          end;
        end;

        FSelection.CreateCustomRGN(LPolygonalRegion.Region, frmMain.MarqueeMode);

        FSelection.GetActualMaskBorder();
        FSelection.CutRegionFromOriginal();
        FSelection.GetForeground();
        FSelection.GetMarchingAntsLines();

        // filling up the backgound that under the selection
        if FChannelManager.CurrentChannelType in [
             ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
        begin
          FSelection.GetBackgroundWithFilledColor(
            Color32(frmMain.BackGrayColor), [csGrayscale] );
        end
        else // must be on a layer
        begin
          if FLayerList.SelectedLayer is TgmNormalLayer then
          begin
            if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.GlobalBackColor),
                FChannelManager.SelectedColorChannels );
            end
            else
            begin
              if FChannelManager.ColorChannelList.SelectedChannelCount > 2 then
              begin
                FSelection.GetBackgroundWithTransparent();
              end
              else
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end;
            end;
          end;
        end;

        // if the selection is created incorrectly, then delete it
        if FSelection.HasShadow = False then
        begin
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                if Assigned(FChannelManager.SelectedAlphaChannel) then
                begin
                  with FChannelManager.SelectedAlphaChannel do
                  begin
                    ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                    ChannelLayer.Bitmap.Changed();
                  end;
                end;
              end;

            ctQuickMaskChannel:
              begin
                with FChannelManager.QuickMaskChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                  ChannelLayer.Bitmap.Changed();
                end;
              end;

            ctLayerMaskChannel:
              begin
                with FLayerList.SelectedLayer do
                begin
                  MaskBitmap.Assign(FSelection.SourceBitmap);
                  Changed();
                end;
              end;

            ctColorChannel:
              begin
                // must be on a layer
                if FLayerList.SelectedLayer is TgmNormalLayer then
                begin
                  with FLayerList.SelectedLayer do
                  begin
                    LayerBitmap.Assign(FSelection.SourceBitmap);
                    Changed();
                  end;
                end;
              end;
          end;

          FSelection.IsAnimated := False;
          FreeAndNil(FSelection);
        end
        else
        begin
          ShowProcessedSelection();
          FSelection.IsAnimated := True;
        end;
      end;
    end;

    FreeAndNil(FRegion);
  end;
end;

procedure TfrmChild.ChangeSelectionTarget;
begin
  if Assigned(FLayerList.SelectedLayer) then
  begin
    if Assigned(FSelection) then
    begin
      FSelection.IsAnimated        := False;
      FSelection.IsTranslated      := False;
      FSelection.IsCornerStretched := False;
      FSelection.IsHorizFlipped    := False;
      FSelection.IsVertFlipped     := False;

      // setting the background for the selection
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              with FChannelManager.SelectedAlphaChannel do
              begin
                FSelection.SourceBitmap.Assign(ChannelLayer.Bitmap);
                FSelection.Background.Assign(ChannelLayer.Bitmap);
              end;
            end;
          end;

        ctQuickMaskChannel:
          begin
            with FChannelManager.QuickMaskChannel do
            begin
              FSelection.SourceBitmap.Assign(ChannelLayer.Bitmap);
              FSelection.Background.Assign(ChannelLayer.Bitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            FSelection.SourceBitmap.Assign(FLayerList.SelectedLayer.MaskBitmap);
            FSelection.Background.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            FSelection.SourceBitmap.Assign(FLayerList.SelectedLayer.LayerBitmap);
            FSelection.Background.Assign(FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;

      // make the original mask of the selection same as its resized mask
      FSelection.OriginalMask.SetSize(
        FLayerList.SelectedLayer.LayerBitmap.Width,
        FLayerList.SelectedLayer.LayerBitmap.Height);
                                      
      FSelection.OriginalMask.Clear(clBlack32);

      FSelection.OriginalMask.Draw(FSelection.MaskBorderStart.X,
                                   FSelection.MaskBorderStart.Y,
                                   FSelection.ResizedMask);

      FSelection.MakeRegionWithMask(FSelection.OriginalMask);
      FSelection.GetActualMaskBorder;
      FSelection.CutRegionFromOriginal;
      FSelection.GetForeground;
      FSelection.GetMarchingAntsLines;

      // filling the background that under the selection, and show the selection
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.BackGrayColor), [csGrayscale]);

              FSelection.ShowSelection(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                [csGrayscale]);

              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
            end;
          end;

        ctQuickMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );

            FSelection.ShowSelection(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              [csGrayscale]);

            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
          end;

        ctLayerMaskChannel:
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.BackGrayColor), [csGrayscale] );

            FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap, [csGrayscale]);
            FLayerList.SelectedLayer.Changed;
          end;

        ctColorChannel:
          begin
            if FLayerList.SelectedLayer is TgmNormalLayer then
            begin
              if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
              begin
                FSelection.GetBackgroundWithFilledColor(
                  Color32(frmMain.GlobalBackColor),
                  FChannelManager.SelectedColorChannels );
              end
              else
              begin
                if (csRed   in FChannelManager.SelectedColorChannels) and
                   (csGreen in FChannelManager.SelectedColorChannels) and
                   (csBlue  in FChannelManager.SelectedColorChannels) then
                begin
                  FSelection.GetBackgroundWithTransparent();
                end
                else
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end;
              end;
            end;

            FSelection.ShowSelection(
              FLayerList.SelectedLayer.LayerBitmap,
              FChannelManager.SelectedColorChannels);

            FLayerList.SelectedLayer.Changed;
          end;
      end;

      FSelection.IsTargetChanged := True;
      FSelection.IsAnimated      := True;
    end;
  end;
end;

// Paint selection on various target ...
procedure TfrmChild.ShowProcessedSelection(const AUpdateDisplay: Boolean = True);
begin
  if Assigned(FSelection) then
  begin
    // update the foregound of the selection
    FSelection.GetForeground;

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            if Assigned(FSelectionTransformation) and
               FSelectionTransformation.IsTransforming then
            begin
              FSelectionTransformation.ShowTransformedSelection(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                [csGrayscale]);
            end
            else
            begin
              FSelection.ShowSelection(
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                [csGrayscale]);
            end;

            if AUpdateDisplay then
            begin
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
            end;
          end;
        end;

      ctQuickMaskChannel:
        begin
          if Assigned(FSelectionTransformation) and
             FSelectionTransformation.IsTransforming then
          begin
            FSelectionTransformation.ShowTransformedSelection(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              [csGrayscale]);
          end
          else
          begin
            FSelection.ShowSelection(
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              [csGrayscale]);
          end;

          if AUpdateDisplay then
          begin
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
          end;
        end;

      ctLayerMaskChannel:
        begin
          if Assigned(FSelectionTransformation) and
             FSelectionTransformation.IsTransforming then
          begin
            FSelectionTransformation.ShowTransformedSelection(
              FLayerList.SelectedLayer.MaskBitmap, [csGrayscale]);
          end
          else
          begin
            FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap, [csGrayscale]);
          end;

          // update the layer mask channel 
          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              0, 0, FLayerList.SelectedLayer.MaskBitmap);
          end;

          if AUpdateDisplay then
          begin
            FLayerList.SelectedLayer.Changed;
          end;
        end;

      ctColorChannel:
        begin
          if Assigned(FSelectionTransformation) and
             FSelectionTransformation.IsTransforming then
          begin
            FSelectionTransformation.ShowTransformedSelection(
              FLayerList.SelectedLayer.LayerBitmap,
              FChannelManager.SelectedColorChannels);
          end
          else
          begin
            FSelection.ShowSelection(FLayerList.SelectedLayer.LayerBitmap,
                                     FChannelManager.SelectedColorChannels);
          end;

          if AUpdateDisplay then
          begin
            FLayerList.SelectedLayer.Changed;
          end;
        end;
    end;

    if AUpdateDisplay then
    begin
      imgWorkArea.Changed;
    end;
  end;
end;

procedure TfrmChild.ShowSelectionAtBrushStroke(const ARect: TRect);
var
  LRefreshRect : TRect;
  LLayerRect   : TRect;
begin
  if Assigned(FSelection) then
  begin
    // update the foregound of the selection
    FSelection.GetForeground;

    case FChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(FChannelManager.SelectedAlphaChannel) then
          begin
            FSelection.ShowSelection(
              FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              [csGrayscale], ARect);

            // selection space to bitmap space
            LRefreshRect.TopLeft     := FSelection.SelectionPointToBitmapPoint(ARect.TopLeft);
            LRefreshRect.BottomRight := FSelection.SelectionPointToBitmapPoint(ARect.BottomRight);
            
            // bitmap space to control space
            LRefreshRect.TopLeft     := imgWorkArea.BitmapToControl(LRefreshRect.TopLeft);
            LRefreshRect.BottomRight := imgWorkArea.BitmapToControl(LRefreshRect.BottomRight);

            FChannelManager.SelectedAlphaChannel.ChannelLayer.Changed(LRefreshRect);
          end;
        end;

      ctQuickMaskChannel:
        begin
          FSelection.ShowSelection(
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
            [csGrayscale], ARect);

          // selection space to bitmap space
          LRefreshRect.TopLeft     := FSelection.SelectionPointToBitmapPoint(ARect.TopLeft);
          LRefreshRect.BottomRight := FSelection.SelectionPointToBitmapPoint(ARect.BottomRight);

          // bitmap space to control space
          LRefreshRect.TopLeft     := imgWorkArea.BitmapToControl(LRefreshRect.TopLeft);
          LRefreshRect.BottomRight := imgWorkArea.BitmapToControl(LRefreshRect.BottomRight);

          FChannelManager.QuickMaskChannel.ChannelLayer.Changed(LRefreshRect);
        end;

      ctLayerMaskChannel:
        begin
          FSelection.ShowSelection(FLayerList.SelectedLayer.MaskBitmap,
                                   [csGrayscale], ARect);

          // update the layer mask channel as well
          if Assigned(FChannelManager.LayerMaskChannel) then
          begin
            LLayerRect.Left   := ARect.Left + FSelection.MaskBorderStart.X;
            LLayerRect.Top    := ARect.Top + FSelection.MaskBorderStart.Y;
            LLayerRect.Right  := ARect.Right + FSelection.MaskBorderStart.X;
            LLayerRect.Bottom := ARect.Bottom + FSelection.MaskBorderStart.Y;

            FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
              LLayerRect, LLayerRect, FLayerList.SelectedLayer.MaskBitmap);
          end;
        end;

      ctColorChannel:
        begin
          // selection space to bitmap space
          LRefreshRect.TopLeft     := FSelection.SelectionPointToBitmapPoint(ARect.TopLeft);
          LRefreshRect.BottomRight := FSelection.SelectionPointToBitmapPoint(ARect.BottomRight);

          // must using bitmap space rect to restore the background
          FSelection.RestoreBackground(FLayerList.SelectedLayer.LayerBitmap, LRefreshRect);

          FSelection.ShowSelection(FLayerList.SelectedLayer.LayerBitmap,
                                   FChannelManager.SelectedColorChannels, ARect);
        end;
    end;

    if FChannelManager.CurrentChannelType in [
         ctColorChannel, ctLayerMaskChannel ] then
    begin
      // selection space to bitmap space
      LRefreshRect.TopLeft     := FSelection.SelectionPointToBitmapPoint(ARect.TopLeft);
      LRefreshRect.BottomRight := FSelection.SelectionPointToBitmapPoint(ARect.BottomRight);

      FLayerList.SelectedLayer.Changed(LRefreshRect);
    end;
  end;
end;

procedure TfrmChild.PauseMarchingAnts;
begin
  if Assigned(FSelection) then
  begin
    FSelection.IsAnimated := False;
  end;
end;

procedure TfrmChild.ChangeImageCursorByMarqueeTools;
begin
  case frmMain.MarqueeMode of
    mmNew:
      begin
        if frmMain.MarqueeTool in [mtRectangular,
                                   mtRoundRectangular,
                                   mtElliptical,
                                   mtSingleRow,
                                   mtSingleColumn,
                                   mtRegularPolygon] then
        begin
          imgWorkArea.Cursor := crCross;
        end
        else
        if frmMain.MarqueeTool = mtPolygonal then
        begin
          imgWorkArea.Cursor := crPolygonSelection;
        end
        else
        if frmMain.MarqueeTool = mtLasso then
        begin
          imgWorkArea.Cursor := crLassoSelection;
        end
        else
        if frmMain.MarqueeTool = mtMagicWand then
        begin
          imgWorkArea.Cursor := crMagicWand;
        end;
      end;

    mmAdd:
      begin
        if frmMain.MarqueeTool in [mtRectangular,
                                   mtRoundRectangular,
                                   mtElliptical,
                                   mtSingleRow,
                                   mtSingleColumn,
                                   mtRegularPolygon] then
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
        if frmMain.MarqueeTool = mtPolygonal then
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
        if frmMain.MarqueeTool = mtLasso then
        begin
          if FSelection <> nil then
          begin
            imgWorkArea.Cursor := crLassoAdd;
          end
          else
          begin
            imgWorkArea.Cursor := crLassoSelection;
          end
        end
        else
        if frmMain.MarqueeTool = mtMagicWand then
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
        if frmMain.MarqueeTool in [mtRectangular,
                                   mtRoundRectangular,
                                   mtElliptical,
                                   mtSingleRow,
                                   mtSingleColumn,
                                   mtRegularPolygon] then
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
        if frmMain.MarqueeTool = mtPolygonal then
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
        if frmMain.MarqueeTool = mtLasso then
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
        if frmMain.MarqueeTool = mtMagicWand then
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
        if frmMain.MarqueeTool in [mtRectangular,
                                   mtRoundRectangular,
                                   mtElliptical,
                                   mtSingleRow,
                                   mtSingleColumn,
                                   mtRegularPolygon] then
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
        if frmMain.MarqueeTool = mtPolygonal then
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
        if frmMain.MarqueeTool = mtLasso then
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
        if frmMain.MarqueeTool = mtMagicWand then
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
        if frmMain.MarqueeTool in [mtRectangular,
                                   mtRoundRectangular,
                                   mtElliptical,
                                   mtSingleRow,
                                   mtSingleColumn,
                                   mtRegularPolygon] then
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
        if frmMain.MarqueeTool = mtPolygonal then
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
        if frmMain.MarqueeTool = mtLasso then
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
        if frmMain.MarqueeTool = mtMagicWand then
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

  if frmMain.MarqueeTool = mtMoveResize then
  begin
    imgWorkArea.Cursor := crMoveSelection;
  end
  else
  if frmMain.MarqueeTool = mtMagneticLasso then
  begin
    imgWorkArea.Cursor := crMagneticLasso;
  end
end;

// Magnetic Lasso 
procedure TfrmChild.CreateLassoLayer;
var
  LHalfWidth, LHalfHeight : Single;
  LCenterPoint            : TPoint;
begin
  if not Assigned(FMagneticLassoLayer) then
  begin
    FMagneticLassoLayer := TBitmapLayer.Create(imgWorkArea.Layers);

    FMagneticLassoLayer.Bitmap.DrawMode       := dmCustom;
    FMagneticLassoLayer.Bitmap.OnPixelCombine := MagneticLassoLayerBlend;

    FMagneticLassoLayer.Bitmap.SetSize(imgWorkArea.Bitmap.Width,
                                       imgWorkArea.Bitmap.Height);
                                       
    FMagneticLassoLayer.Bitmap.Clear($00000000);

    LHalfWidth  := FMagneticLassoLayer.Bitmap.Width  / 2;
    LHalfHeight := FMagneticLassoLayer.Bitmap.Height / 2;

    { Get the center point of the viewport of the TImage32/TImgView32 and
      convert it from control space to bitmap space. }
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

procedure TfrmChild.CreateMeasureLayer;
begin
  if Assigned(FMeasureLayer) then
  begin
    Exit;
  end;

  FMeasureLayer := TBitmapLayer.Create(imgWorkArea.Layers);

  with FMeasureLayer do
  begin
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := MeasureLayerBlend;
    Scaled                := False;
    Visible               := True;
  end;

  FitMeasureLayerToViewport();
end;

procedure TfrmChild.RemoveMeasureLayer;
begin
  if Assigned(FMeasureLayer) then
  begin
    FreeAndNil(FMeasureLayer);
  end;
end;

procedure TfrmChild.RemoveMeasureLine;
begin
  if Assigned(FMeasureLine) then
  begin
    FreeAndNil(FMeasureLine);
  end;
end;

procedure TfrmChild.FitMeasureLayerToViewport;
var
  r : TRect;
begin
  if not Assigned(FMeasureLayer) then
  begin
    Exit;
  end;

  r := imgWorkArea.GetViewportRect;

  with FMeasureLayer do
  begin
    FMeasureLayer.Location := FloatRect(r);
    FMeasureLayer.Bitmap.SetSize(r.Right - r.Left + 1, r.Bottom - r.Top + 1);
    FMeasureLayer.Bitmap.Clear($FFFFFFFF);
  end;
end;

procedure TfrmChild.DrawMeasureLineOnMeasureLayer;
begin
  if Assigned(FMeasureLine) and Assigned(FMeasureLayer) then
  begin
    FMeasureLine.Draw(FMeasureLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
    FMeasureLayer.Bitmap.Changed();
  end;
end;

procedure TfrmChild.FinishMagneticLasso;
begin
  if Assigned(FLayerList.SelectedLayer) and
     Assigned(FMagneticLasso) then
  begin
    if FMagneticLasso.IsConnected then
    begin
      // if there is no selection, create one
      if not Assigned(FSelection) then
      begin
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(FChannelManager.SelectedAlphaChannel) then
              begin
                FSelection := TgmSelection.Create(imgWorkArea,
                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
              end;
            end;

          ctQuickMaskChannel:
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;

          ctLayerMaskChannel:
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FLayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FLayerList.SelectedLayer.LayerBitmap);
            end;
        end;
      end;

      FSelection.CreateCustomRGN(FMagneticLasso.CurveRegion, frmMain.MarqueeMode);

      FSelection.GetActualMaskBorder;   // get the border of the selection
      FSelection.CutRegionFromOriginal; // cut region from FSourceBitmap and FOriginalMask of the selection
      FSelection.GetForeground;         // get foreground of the selection
      FSelection.GetMarchingAntsLines;  // get the Marching Ants lines form the FResizeMask of the selection

      // filling the backgound that under the selection
      if FChannelManager.CurrentChannelType in [
           ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
      begin
        FSelection.GetBackgroundWithFilledColor(
          Color32(frmMain.BackGrayColor), [csGrayscale] );
      end
      else
      begin
        // must be on layer

        if FLayerList.SelectedLayer is TgmNormalLayer then
        begin
          if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
          begin
            FSelection.GetBackgroundWithFilledColor(
              Color32(frmMain.GlobalBackColor),
              FChannelManager.SelectedColorChannels );
          end
          else
          begin
            if (csRed   in FChannelManager.SelectedColorChannels) and
               (csGreen in FChannelManager.SelectedColorChannels) and
               (csBlue  in FChannelManager.SelectedColorChannels) then
            begin
              FSelection.GetBackgroundWithTransparent();
            end
            else
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.GlobalBackColor),
                FChannelManager.SelectedColorChannels );
            end;
          end;
        end;
      end;

      // if there is no mask shadow, delete the selection
      if FSelection.HasShadow = False then
      begin
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(FChannelManager.SelectedAlphaChannel) then
              begin
                with FChannelManager.SelectedAlphaChannel do
                begin
                  ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                  ChannelLayer.Bitmap.Changed();
                end;
              end;
            end;

          ctQuickMaskChannel:
            begin
              with FChannelManager.QuickMaskChannel do
              begin
                ChannelLayer.Bitmap.Assign(FSelection.SourceBitmap);
                ChannelLayer.Bitmap.Changed;
              end;
            end;

          ctLayerMaskChannel:
            begin
              FLayerList.SelectedLayer.MaskBitmap.Assign(FSelection.SourceBitmap);
              FLayerList.SelectedLayer.Changed;
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                FLayerList.SelectedLayer.LayerBitmap.Assign(FSelection.SourceBitmap);
                FLayerList.SelectedLayer.Changed;
              end;
            end;
        end;

        FSelection.IsAnimated := False;
        FreeAndNil(FSelection);
      end
      else
      begin
        ShowProcessedSelection();
        FSelection.IsAnimated := True;
      end;
    end;

    FreeAndNil(FMagneticLasso);
  end;

  if Assigned(FMagneticLassoLayer) then
  begin
    FreeAndNil(FMagneticLassoLayer);
  end; 
end;

procedure TfrmChild.CreateSelectionTransformation(
  const AMode: TgmTransformMode);
begin
  if (AMode = tmNone) or
     (AMode = tmTranslate) or
     (not Assigned(FSelection)) then
  begin
    Exit;
  end;

  if Assigned(FSelectionTransformation) then
  begin
    FreeAndNil(FSelectionTransformation);
  end;

  case AMode of
    tmDistort:
      begin
        FSelectionTransformation := TgmSelectionDistort.Create(FSelection);
      end;

    tmRotate:
      begin
        FSelectionTransformation := TgmSelectionRotate.Create(FSelection);
      end;

    tmScale:
      begin
        FSelectionTransformation := TgmSelectionScale.Create(FSelection);
      end;
  end;
end;

procedure TfrmChild.FreeSelectionTransformation;
begin
  if Assigned(FSelectionTransformation) then
  begin
    FreeAndNil(FSelectionTransformation);
  end;
end;

procedure TfrmChild.ConnectTransformMouseEvents;
begin
  imgWorkArea.OnMouseDown := TransformSelectionMouseDown;
  imgWorkArea.OnMouseMove := TransformSelectionMouseMove;
  imgWorkArea.OnMouseUp   := TransformSelectionMouseUp;
end;

procedure TfrmChild.FinishTransformation;
var
  LMsgDlgResult : Integer;
  LCommand      : TgmCustomCommand;
  LAlphaChannel : TgmAlphaChannel;
begin
  LCommand := nil;

  if Assigned(FSelectionTransformation) then
  begin
    LMsgDlgResult := MessageDlg('Apply the transformation?', mtInformation,
                                [mbYes, mbNo, mbCancel], 0);

    case LMsgDlgResult of
      mrYes:
        begin
          // Undo/Redo first...
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmExitSelectionTransformFormAlphaChannelCommand.Create(
                  FChannelManager, FChannelManager.AlphaChannelList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcApplyTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmExitSelectionTransformFromQuickMaskChannelCommand.Create(
                  FChannelManager, FSelectionTransformation.TransformMode,
                  etmcApplyTransform, dmMain.SelectionCopy,
                  FSelectionTransformation, GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmExitSelectionTransformFromLayerMaskCommand.Create(
                  FChannelManager, FLayerList, FLayerList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcApplyTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmExitSelectionTransformFromLayerCommand.Create(
                  FChannelManager, FLayerList, FLayerList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcApplyTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;
          end;

          // then, apply the transformation
          FSelectionTransformation.AcceptTransform();
        end;

      mrNo:
        begin
          // Undo/Redo first...
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmExitSelectionTransformFormAlphaChannelCommand.Create(
                  FChannelManager, FChannelManager.AlphaChannelList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcCancelTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmExitSelectionTransformFromQuickMaskChannelCommand.Create(
                  FChannelManager, FSelectionTransformation.TransformMode,
                  etmcCancelTransform, dmMain.SelectionCopy,
                  FSelectionTransformation, GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmExitSelectionTransformFromLayerMaskCommand.Create(
                  FChannelManager, FLayerList, FLayerList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcCancelTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmExitSelectionTransformFromLayerCommand.Create(
                  FChannelManager, FLayerList, FLayerList.SelectedIndex,
                  FSelectionTransformation.TransformMode, etmcCancelTransform,
                  dmMain.SelectionCopy, FSelectionTransformation,
                  GetSelectionForUndoRedo,
                  EnterSelectionTransformModeForUndoRedo,
                  ExitSelectionTransfromModeForUndoRedo);
              end;
          end;
          
          // then, canceling the transformation
          FSelectionTransformation.CancelTransform();
          ShowProcessedSelection();

          // update thumbnails ...
          case FChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LAlphaChannel :=
                  TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[
                    FChannelManager.AlphaChannelList.SelectedIndex]);

                LAlphaChannel.UpdateChannelThumbnail();
              end;

            ctQuickMaskChannel:
              begin
                FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
              end;

            ctLayerMaskChannel:
              begin
                FLayerList.SelectedLayer.UpdateMaskThumbnail();
                FChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
              end;

            ctColorChannel:
              begin
                FLayerList.SelectedLayer.UpdateLayerThumbnail();
              end;
          end;
        end;
    end;

    if LMsgDlgResult in [mrYes, mrNo] then
    begin
      FreeAndNil(FSelectionTransformation);
      ConnectMouseEventsToImage();
      ChangeImageCursorByToolTemplets();

      // Undo/Redo
      if Assigned(LCommand) then
      begin
        FCommandManager.AddCommand(LCommand);
      end;
    end;

    // clear the control border for selection transformation
    imgWorkArea.Update(imgWorkArea.ClientRect);
  end;
end;

procedure TfrmChild.CommitCrop;
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LRect      : TRect;
begin
  if Assigned(FCrop) then
  begin
    Screen.Cursor := crHourGlass;
    try
      // doing crop...
      if (frmMain.chckbxResizeCrop.Checked) and
         (frmMain.edtResizeCropWidth.Text  <> '') and
         (frmMain.edtResizeCropHeight.Text <> '') then
      begin
        FCrop.ResizeW   := StrToInt(frmMain.edtResizeCropWidth.Text);
        FCrop.ResizeH   := StrToInt(frmMain.edtResizeCropHeight.Text);
        FCrop.IsResized := True;
      end
      else
      begin
        FCrop.IsResized := False;
      end;

      if FCrop.IsResized then
      begin
        LNewWidth  := FCrop.ResizeW;
        LNewHeight := FCrop.ResizeH;
      end
      else
      begin
        LNewWidth  := FCrop.CropAreaWidth;
        LNewHeight := FCrop.CropAreaHeight;
      end;

      // doing crop ...

      // resize the background pattern
      CheckerboardBmp.SetSize(LNewWidth, LNewHeight);

      DrawCheckerboardPattern( CheckerboardBmp,
        Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

      // crop layers ...
      FLayerList.CropLayers(FCrop, Color32(frmMain.GlobalBackColor));

      // Crop channels ...
      ChannelManager.CropChannels(FCrop);

      // set location for channel layers ...
      imgWorkArea.Bitmap.SetSize(LNewWidth, LNewHeight);

      LRect             := imgWorkArea.GetBitmapRect();
      LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

      ChannelManager.ChannelLayerLocation := FloatRect(LRect);

      // update display
      ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);

      // the paths ...
      if Assigned(PathList.SelectedPath) then
      begin
        FitPathLayerToViewport();
        DrawPathOnPathLayer();
      end;

      if PathList.Count > 0 then
      begin
        PathList.UpdateAllPathThumbnails( LNewWidth, LNewHeight, Point(0, 0) );
        frmPaths.PathManager.Invalidate();
      end;

      LayerList.SelectedLayer.Changed();
      imgWorkArea.Changed();

      // delete crop tool
      CancelCrop();
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmChild.CancelCrop;
begin
  if Assigned(FCrop) then
  begin
    FreeAndNil(FCrop);
    frmMain.UpdateCropOptions();
  end;
end;

procedure TfrmChild.FinishCrop;
begin
  if Assigned(FCrop) then
  begin
    case MessageDlg('Crop the image?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          CommitCrop();
        end;

      mrNo:
        begin
          CancelCrop();
        end;

      mrCancel:
        begin
          // switch to Crop tool
          if frmMain.MainTool <> gmtCrop then
          begin
            frmMain.spdbtnCropTools.Down := True;
            frmMain.ChangeMainToolClick(frmMain.spdbtnCropTools);
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.ExecuteOptimalCrop;
var
  LCroppedBmp           : TBitmap32;
  LCropArea, LRect      : TRect;
  LNewWidth, LNewHeight : Integer;
  LCommand              : TgmCustomCommand;
begin
  if Assigned(FSelection) then
  begin
    Exit;
  end;
  
  LCropArea := Rect(0, 0, 0, 0);

  if (FLayerList.Count = 1) and
     (FLayerList.SelectedLayer is TgmNormalLayer) and
     (FChannelManager.CurrentChannelType = ctColorChannel) and
     (FChannelManager.ColorChannelList.SelectedChannelCount > 2) then
  begin
    Screen.Cursor := crHourGlass;
    LCroppedBmp   := TBitmap32.Create();
    try
      // crop the layer and mask (if any)
      LCroppedBmp.Assign(FLayerList.SelectedLayer.LayerBitmap);
      LCropArea := OptimalCrop(FLayerList.SelectedLayer.LayerBitmap, LCroppedBmp);

      if (LCropArea.Right  > LCropArea.Left) and
         (LCropArea.Bottom > LCropArea.Top) then
      begin
        // Undo/Redo, first
        LCommand := TgmRectCropCommand.Create( 'Optimal Crop', FChannelManager,
          FLayerList, FPathList, frmRichTextEditor.rchedtRichTextEditor,
          LCropArea, Color32(frmMain.GlobalBackColor) );

        // doing crop ...
        FLayerList.CropLayers(LCropArea, Color32(frmMain.GlobalBackColor));

        LNewWidth  := FLayerList.SelectedLayer.LayerBitmap.Width;
        LNewHeight := FLayerList.SelectedLayer.LayerBitmap.Height;

        // resize the background pattern
        CheckerboardBmp.SetSize(LNewWidth, LNewHeight);

        DrawCheckerboardPattern( CheckerboardBmp,
          Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

        // Crop channels ...
        ChannelManager.CropChannels(LCropArea);

        // set location for channel layers ...
        imgWorkArea.Bitmap.SetSize(LNewWidth, LNewHeight);

        LRect             := imgWorkArea.GetBitmapRect();
        LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
        LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

        ChannelManager.ChannelLayerLocation := FloatRect(LRect);

        // the paths ...
        if Assigned(PathList.SelectedPath) then
        begin
          FitPathLayerToViewport();
          DrawPathOnPathLayer();
        end;

        if PathList.Count > 0 then
        begin
          PathList.UpdateAllPathThumbnails( LNewWidth, LNewHeight, Point(0, 0) );
          frmPaths.PathManager.Invalidate();
        end;

        // update display
        LayerList.SelectedLayer.Changed();
        ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);
        imgWorkArea.Changed();

        // Undo/Redo
        FCommandManager.AddCommand(LCommand);
      end;

    finally
      LCroppedBmp.Free();
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmChild.ChangeImageCursorByEraserTools;
begin
  case frmMain.EraserTool of
    etEraser,
    etBackgroundEraser:
      begin
        imgWorkArea.Cursor := crCross;
      end;

    etMagicEraser:
      begin
        imgWorkArea.Cursor := crMagicEraser;
      end;
  end;
end; 

procedure TfrmChild.CreatePathLayer;
begin
  if Assigned(FPathLayer) then
  begin
    Exit;
  end;

  // The path layer will always fit the size of the viewport.

  FPathLayer := TBitmapLayer.Create(imgWorkArea.Layers);

  with FPathLayer do
  begin
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := PathLayerBlend;
    Scaled                := False;
    Visible               := True;
  end;

  FitPathLayerToViewport();
end;

procedure TfrmChild.DeletePathLayer;
begin
  if Assigned(FPathLayer) then
  begin
    FreeAndNil(FPathLayer);
  end;
end;

procedure TfrmChild.DrawPathOnPathLayer;
begin
  if Assigned(FPathLayer) then
  begin
    with FPathLayer do
    begin
      if Assigned(FPathList.SelectedPath) then
      begin
        FPathList.SelectedPath.CurvePathList.DrawAllPaths(
          Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
      end;

      Bitmap.Changed();
    end;
  end;
end;

procedure TfrmChild.FitPathLayerToViewport;
var
  r : TRect;
begin
  if not Assigned(FPathLayer) then
  begin
    Exit;
  end;

  r := imgWorkArea.GetViewportRect();

  with FPathLayer do
  begin
    FPathLayer.Bitmap.SetSize(r.Right - r.Left + 1, r.Bottom - r.Top + 1);
    FPathLayer.Bitmap.Clear($00000000);
    FPathLayer.Location := FloatRect(r);
  end;
end;

procedure TfrmChild.ChangeImageCursorByPenTools;
begin
  case frmMain.PenTool of
    ptPathComponentSelection:
      begin
        imgWorkArea.Cursor := crPathComponentSelection;
      end;
      
    ptPenTool:
      begin
        imgWorkArea.Cursor := GetPenToolDefaultCursor();
      end;

    ptDirectSelection,
    ptAddAnchorPoint,
    ptDeleteAnchorPoint,
    ptConvertPoint:
      begin
        imgWorkArea.Cursor := crDirectSelection;
      end;
  end;
end;

function TfrmChild.GetPenToolDefaultCursor: TCursor;
begin
  if Assigned(FCurvePath) then
  begin
    Result := crPenToolSelected;
  end
  else
  begin
    Result := crPenToolDeselected;
  end;
end; 

// Path to Selection
procedure TfrmChild.LoadPathAsSelection;
begin
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FSelection) then
    begin
      CommitSelection;
    end;

    // create a selection
    if FSelection = nil then
    begin
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;

        ctLayerMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end;

    if Assigned(FSelection) then
    begin
      if Assigned(FPathList.SelectedPath) then
      begin
        if FPathList.SelectedPath.CurvePathList.UpdatePathRegion(
             FSelection.SourceBitmap.Width,
             FSelection.SourceBitmap.Height,
             Point(0, 0) ) then
        begin
          FSelection.CreateCustomRGN(
            FPathList.SelectedPath.CurvePathList.PathRegion, mmNew);
        end;
      end;

      FSelection.GetActualMaskBorder();

      // if the selection is created successfully...
      if FSelection.HasShadow then
      begin
        FSelection.CutRegionFromOriginal();
        FSelection.GetForeground();
        FSelection.GetMarchingAntsLines();

        // filling the background that is beneath the selection
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.BackGrayColor), [csGrayscale] );
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end
                else
                begin
                  if FChannelManager.ColorChannelList.SelectedChannelCount >= 3 then
                  begin
                    FSelection.GetBackgroundWithTransparent();
                  end
                  else
                  begin
                    FSelection.GetBackgroundWithFilledColor(
                      Color32(frmMain.GlobalBackColor),
                      FChannelManager.SelectedColorChannels );
                  end;
                end;
              end;
            end;
        end;
      end
      else
      begin
        MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);
        FreeAndNil(FSelection);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  if Assigned(FSelection) then
  begin
    ShowProcessedSelection();
    FSelection.IsAnimated := True;
  end;
  
  frmMain.UpdateMarqueeOptions();
end;

// channel to selection
procedure TfrmChild.LoadChannelAsSelection;
var
  LChannelBmp  : TBitmap32;
  LMarqueeMode : TgmMarqueeMode;
begin
  Screen.Cursor := crHourGlass;
  try
    // if the selection has not been created, then create one
    if FSelection = nil then
    begin
      LMarqueeMode := mmNew;

      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;

        ctLayerMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end
    else
    begin
      LMarqueeMode := frmMain.MarqueeMode;
    end;

    if Assigned(FSelection) then
    begin
      LChannelBmp := nil;
      
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              LChannelBmp := TBitmap32.Create;
              LChannelBmp.Assign(FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;
          
        ctQuickMaskChannel:
          begin
            LChannelBmp := TBitmap32.Create;
            LChannelBmp.Assign(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;
          
        ctLayerMaskChannel:
          begin
            LChannelBmp := TBitmap32.Create;
            LChannelBmp.Assign(FLayerList.SelectedLayer.MaskBitmap);
          end;
          
        ctColorChannel:
          begin
            LChannelBmp := FChannelManager.GetColorChannelGrayscaleMap(
              FLayerList.CombineResult);
          end;
      end;

      if Assigned(LChannelBmp) then
      begin
        if FSelection.LoadChannelAsSelection(LChannelBmp, LMarqueeMode) = False then
        begin
          MessageDlg(FSelection.OutputMsg, mtError, [mbOK], 0);
        end;

        LChannelBmp.Free;
      end;

      FSelection.GetActualMaskBorder;

      // if the selection is created successfully...
      if FSelection.HasShadow then
      begin
        FSelection.CutRegionFromOriginal;
        FSelection.GetForeground;
        FSelection.GetMarchingAntsLines;

        // filling the background that is under the selection
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.BackGrayColor), [csGrayscale] );
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end
                else
                begin
                  if (csRed   in FChannelManager.SelectedColorChannels) and
                     (csGreen in FChannelManager.SelectedColorChannels) and
                     (csBlue  in FChannelManager.SelectedColorChannels) then
                  begin
                    FSelection.GetBackgroundWithTransparent();
                  end
                  else
                  begin
                    FSelection.GetBackgroundWithFilledColor(
                      Color32(frmMain.GlobalBackColor),
                      FChannelManager.SelectedColorChannels );
                  end;
                end;
              end;
            end;
        end;
      end
      else
      begin
        MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);
        FreeAndNil(FSelection);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  if Assigned(FSelection) then
  begin
    ShowProcessedSelection();
    FSelection.IsAnimated := True;
  end;

  frmMain.UpdateMarqueeOptions();
end;

procedure TfrmChild.LoadQuickMaskAsSelection;
var
  LChannelBmp  : TBitmap32;
  LMarqueeMode : TgmMarqueeMode;
begin
  LChannelBmp := nil;

  Screen.Cursor := crHourGlass;
  try
    // if the selection has not been created, then create one
    if FSelection = nil then
    begin
      LMarqueeMode := mmNew;
      
      case FChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(FChannelManager.SelectedAlphaChannel) then
            begin
              FSelection := TgmSelection.Create(imgWorkArea,
                FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
          end;

        ctLayerMaskChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.MaskBitmap);
          end;

        ctColorChannel:
          begin
            FSelection := TgmSelection.Create(imgWorkArea,
              FLayerList.SelectedLayer.LayerBitmap);
          end;
      end;
    end
    else
    begin
      LMarqueeMode := frmMain.MarqueeMode;
    end;

    if Assigned(FSelection) then
    begin
      if Assigned(FChannelManager.QuickMaskChannel) then
      begin
        LChannelBmp := FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap;
      end;
      
      if FSelection.LoadChannelAsSelection(LChannelBmp, LMarqueeMode) = False then
      begin
        MessageDlg(FSelection.OutputMsg, mtError, [mbOK], 0);
      end;
      
      FSelection.GetActualMaskBorder;

      // if the selection is created successfully...
      if FSelection.HasShadow then
      begin
        FSelection.CutRegionFromOriginal;
        FSelection.GetForeground;
        FSelection.GetMarchingAntsLines;

        // filling the background that is under the selection
        case FChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              FSelection.GetBackgroundWithFilledColor(
                Color32(frmMain.BackGrayColor), [csGrayscale] );
            end;

          ctColorChannel:
            begin
              if FLayerList.SelectedLayer is TgmNormalLayer then
              begin
                if TgmNormalLayer(FLayerList.SelectedLayer).IsAsBackground then
                begin
                  FSelection.GetBackgroundWithFilledColor(
                    Color32(frmMain.GlobalBackColor),
                    FChannelManager.SelectedColorChannels );
                end
                else
                begin
                  if (csRed   in FChannelManager.SelectedColorChannels) and
                     (csGreen in FChannelManager.SelectedColorChannels) and
                     (csBlue  in FChannelManager.SelectedColorChannels) then
                  begin
                    FSelection.GetBackgroundWithTransparent;
                  end
                  else
                  begin
                    FSelection.GetBackgroundWithFilledColor(
                      Color32(frmMain.GlobalBackColor),
                      FChannelManager.SelectedColorChannels );
                  end;
                end;
              end;
            end;
        end;
      end
      else
      begin
        MessageDlg('No pixels were selected.', mtWarning, [mbOK], 0);
        FreeAndNil(FSelection);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  if Assigned(FSelection) then
  begin
    ShowProcessedSelection();
    FSelection.IsAnimated := True;
  end;
  
  frmMain.UpdateMarqueeOptions();
end;

procedure TfrmChild.ShowMeasureResult;
var
  LMeasureUnit : TgmMeasureUnit;
begin
  if Assigned(FMeasureLine) then
  begin
    LMeasureUnit := TgmMeasureUnit(frmMain.cmbbxMeasureUnit.ItemIndex);

    FMeasureLine.Calculate(LMeasureUnit, PixelsPerInch);

    case LMeasureUnit of
      muPixel:
        begin
          with frmMain do
          begin
            lblMStartXValue.Caption := Format('%d',   [FMeasureLine.OriginalIntX]);
            lblMStartYValue.Caption := Format('%d',   [FMeasureLine.OriginalIntY]);
            lblMWidthValue.Caption  := Format('%d',   [FMeasureLine.IntWidth]);
            lblMHeightValue.Caption := Format('%d',   [FMeasureLine.IntHeight]);
            lblMAngleValue.Caption  := Format('%.1f', [FMeasureLine.MeasureAngle]);
            lblMD1Value.Caption     := Format('%d',   [FMeasureLine.IntDistance1]);
            lblMD2Value.Caption     := Format('%d',   [FMeasureLine.IntDistance2]);
          end;
        end;

      muInch, muCM:
        begin
          with frmMain do
          begin
            lblMStartXValue.Caption := Format('%.2f', [FMeasureLine.OriginalFloatX]);
            lblMStartYValue.Caption := Format('%.2f', [FMeasureLine.OriginalFloatY]);
            lblMWidthValue.Caption  := Format('%.2f', [FMeasureLine.FloatWidth]);
            lblMHeightValue.Caption := Format('%.2f', [FMeasureLine.FloatHeight]);
            lblMAngleValue.Caption  := Format('%.1f', [FMeasureLine.MeasureAngle]);
            lblMD1Value.Caption     := Format('%.2f', [FMeasureLine.FloatDistance1]);
            lblMD2Value.Caption     := Format('%.2f', [FMeasureLine.FloatDistance2]);
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.CalcVertexForLineRegionOutline;
var
  LTempPolygon: array [0..4] of TPoint;
  i           : Integer;
begin
  CalcLineOutlineVertices(LTempPolygon, FActualStartPoint, FActualEndPoint,
                          frmMain.LineWeight);

  for i := 0 to 4 do
  begin
    FRegionPolygon[i] := LTempPolygon[i];
  end;
end;

procedure TfrmChild.ChangeImageCursorByShapeTools;
begin
  if frmMain.ShapeRegionTool = srtMove then
  begin
    imgWorkArea.Cursor := crMoveSelection;
  end
  else
  begin
    case frmMain.RegionCombineMode of
      rcmAdd:
        begin
          imgWorkArea.Cursor := crCrossAdd;
        end;

      rcmSubtract:
        begin
          imgWorkArea.Cursor := crCrossSub;
        end;
        
      rcmIntersect:
        begin
          imgWorkArea.Cursor := crCrossIntersect;
        end;
        
      rcmExcludeOverlap:
        begin
          imgWorkArea.Cursor := crCrossInterSub;
        end;
    end;
  end;
end;

procedure TfrmChild.CommitEdits;
var
  LRichTextLayer : TgmRichTextLayer;
begin
  if FLayerList.SelectedLayer is TgmRichTextLayer then
  begin
    LRichTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);
    
    LRichTextLayer.SaveTextToLayer(frmRichTextEditor.rchedtRichTextEditor.Lines);

    if not LRichTextLayer.IsRenamed then
    begin
      LRichTextLayer.LayerName := frmRichTextEditor.rchedtRichTextEditor.Lines[0];
      frmLayers.LayerPanelManager.Invalidate();
    end;
    
    frmRichTextEditor.Close();
    frmMain.UpdateTextOptions();
  end;
end;

procedure TfrmChild.CancelEdits;
var
  LRichTextLayer : TgmRichTextLayer;
begin
  if FLayerList.SelectedLayer is TgmRichTextLayer then
  begin
    LRichTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);

    frmRichTextEditor.Close();
    frmRichTextEditor.CanChange := True;
    try
      LRichTextLayer.RichTextStream.Position := 0;

      frmRichTextEditor.rchedtRichTextEditor.Lines.LoadFromStream(
        LRichTextLayer.RichTextStream);
    finally
      frmRichTextEditor.CanChange            := False;
      LRichTextLayer.RichTextStream.Position := 0;
    end;

    frmMain.UpdateTextOptions();
  end;
end;

procedure TfrmChild.FormCreate(Sender: TObject);
begin
{ Common }

  FEditMode := emStandardMode;  // indicate which edit mode we are in
  FFileName := '';
  RefreshCaption;

  FImageProcessed := False;
  FDrawing        := False;
  FMayClick       := True;
  FDoubleClicked  := False;
  FXActual        := 0;
  FYActual        := 0;

  // coordinates of selection space
  FMarqueeX := 0;
  FMarqueeY := 0;

  // points for common use
  FStartPoint       := Point(0, 0);
  FEndPoint         := Point(0, 0);
  FActualStartPoint := Point(0, 0);
  FActualEndPoint   := Point(0, 0);
  FDrawingBasePoint := Point(0, 0);
  FPrevStrokePoint  := Point(0, 0);

  FHistoryBitmap          := TBitmap32.Create;
  FHistoryBitmap.DrawMode := dmBlend;

{ frmChild }

  FLayerList := TgmLayerList.Create();
  with FLayerList do
  begin
    OnLayerCombined      := Self.AfterLayerCombined;
    OnSelectionChanged   := Self.AfterSelectedLayerPanelChanged;
    OnLayerOrderChanged  := Self.AfterLayerOrderChanged;
    OnMergeLayerDown     := Self.AfterLayerMerged;
    OnMergeVisibleLayers := Self.AfterLayerMerged;
    OnFlattenLayers      := Self.AfterLayerMerged;
  end;

  FChannelManager := TgmRGBChannelManager.Create();
  with FChannelManager do
  begin
    Layers                     := imgWorkArea.Layers;
    OnAlphaChannelDelete       := Self.OnAlphaChannelDelete;
    OnAlphaChannelOrderChanged := Self.OnAlphaChannelOrderChanged;
    OnChannelDblClick          := Self.OnChannelDblClick;
    OnChannelVisibleChanged    := Self.OnChannelVisibleChange;
    OnChannelThumbnailUpdate   := Self.OnChannelThumbnailUpdate;
    OnInsertAlphaChannel       := Self.OnInsertAlphaChannel;
    OnLayerMaskChannelDelete   := Self.OnLayerMaskChannelDelete;
    OnQuickMaskChannelCreate   := Self.OnQuickMaskChannelCreate;
    OnQuickMaskChannelDelete   := Self.OnQuickMaskChannelDelete;
    OnSelectedChannelChanged   := Self.OnSelectedChannelChanged;
  end;

  FPathList := TgmPathList.Create();
  with FPathList do
  begin
    OnInsertPath       := Self.AfterPathInserted;
    OnPathDeleted      := Self.AfterPathDeleted;
    OnSelectionChanged := Self.AfterSelectedPathChanged;
  end;

  // create a Command manager -- for Undo/Redo
  FCommandManager := TgmCommandManager.Create();
  with FCommandManager do
  begin
    CommandMaxCount := StrToInt( ReadInfoFromIniFile(SECTION_PREFERENCES,
                                                     IDENT_HISTORY_STATES,
                                                     '20') );

    OnCommandAdded            := Self.AfterCommandAdded;
    OnSelectedCommandChanged  := Self.AfterSelectedCommandChanged;
    OnSelectedSnapshotChanged := Self.AfterSelectedSnapshotChanged;
  end;

  FAccumTranslateVector := Point(0, 0);
  FGlobalTopLeft        := Point(0, 0);
  FGlobalBottomRight    := Point(0, 0);
  FKeyIsDown            := False;  // mark if we have pressed a key
  FMagnification        := 100;    // zoom scale of the image, 100% in default
  FPrevWheelDelta       := 0;

  FCheckerboardBmp := TBitmap32.Create();

  // used for managing the figures that on vector layers
  FFigureManager := TgmLayerFigureManager.Create(FLayerList);

  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action. 
  with imgWorkArea.PaintStages[0]^ do
  begin
    Parameter := PAINT_STAGE_ZERO_PARAMETER; 

    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;

  imgWorkArea.RepaintMode     := rmOptimizer;
  imgWorkArea.Bitmap.DrawMode := dmBlend;
  imgWorkArea.OnScaleChange   := Self.ImageViewerScaleChange;

  FHandlePaintStage := imgWorkArea.PaintStages.Add;
  FHandleStageIndex := imgWorkArea.PaintStages.Count - 1;   

  with FHandlePaintStage^ do
  begin
    DsgnTime  := False;
    RunTime   := True;                                      
    Stage     := PST_CUSTOM;
    Parameter := HANDLE_STAGE_PARAMETER;
  end;

{ Standard Page }

  FCurvePoint1       := Point(0, 0);
  FCurvePoint2       := Point(0, 0);
  FActualCurvePoint1 := Point(0, 0);
  FActualCurvePoint2 := Point(0, 0);
  FDrawCurveTime     := 0;
  FPolygon           := nil;
  FActualPolygon     := nil;

  FSelectedFigure    := nil;
  FMoveDrawingState  := dsNotDrawing;
  FMoveDrawingHandle := dhNone;
  FRegularBasePoint  := Point(0, 0);
  FRegionSelectOK    := False;
  FPencilMask        := nil;

  // for Undo/Redo
  FOldFigure := nil;

{ Marquee page }

  FMarqueeDrawingState      := dsNotDrawing;
  FMarqueeDrawingHandle     := dhNone;
  FSelection                := nil;
  FSelectionCopy            := nil;
  FSelectionTranslateTarget := sttNone;

  FRegion := nil;

  // Magnetic Lasso
  FMagneticLasso      := nil;
  FMagneticLassoLayer := nil;

  // Transformation 
  FSelectionTransformation := nil;
  FTransformCopy           := nil;  // used for Undo/Redo
  FTransformHandle         := dhNone;

{ Crop Tool }
  FCrop              := nil;
  FCropDrawingState  := dsNotDrawing;
  FCropDrawingHandle := dhNone;

{ Pen Tools }
  FPathLayer        := nil;
  FCurvePath        := nil;
  FPathSelectHandle := pshNone;

  FWholePathIndex        := -1;
  FMouseDownX            := 0;
  FMouseDownY            := 0;
  FMouseMoveX            := 0;
  FMouseMoveY            := 0;
  FOriginalPairState     := psUnknown;
  FOppositeLineOperation := oloAbsoluteOpposite;

  // for Undo/Redo
  FOldCurvePathList     := nil;
  FPathModificationMode := pmmNone;

{ Measure Tool }
  FMeasureLine          := nil;
  FMeasureLayer         := nil;
  FMeasureDrawingState  := dsNotDrawing;
  FMeasurePointSelector := mpsNone;

{ Shape Tool }
  FShapeDrawingHandle := dhNone;
  FShapeDrawingState  := dsNewFigure;
  
{ Text Tool }
  FRichTextDrawingState  := dsNotDrawing;
  FRichTextDrawingHandle := dhNone;
  FOldTextStream         := nil;
end; 

procedure TfrmChild.FormActivate(Sender: TObject);
begin
  SetupOnChildFormActivate();

  with frmMain do
  begin
    spdbtnStandardMode.Enabled  := True;
    spdbtnQuickMaskMode.Enabled := True;
    spdbtnStandardMode.Down     := (FEditMode = emStandardMode);
    spdbtnQuickMaskMode.Down    := (FEditMode = emQuickMaskMode);
  end;
end; 

procedure TfrmChild.FormDestroy(Sender: TObject);
begin
{ Common }
  // restore the content of the status bar of the main form after the child form is closed
  frmMain.stsbrMain.Panels[0].Text := 'Thank you for choose this program!';

  FHistoryBitmap.Free();
  FCommandManager.Free();

  FPolygon       := nil;
  FActualPolygon := nil;

{ Standard Tools }
  FOldFigure.Free();
  FPencilMask.Free();  

{ Marquee }
  FSelection.Free();
  FSelectionCopy.Free();

  if Assigned(FRegion) then
  begin
    FRegion.Free();
  end;

  // Magnetic Lasso
  if Assigned(FMagneticLasso) then
  begin
    FMagneticLasso.Free();
  end;

  if Assigned(FMagneticLassoLayer) then
  begin
    FMagneticLassoLayer.Free();
  end;

  // Tranformation
  FSelectionTransformation.Free();
  FTransformCopy.Free();

{ Crop Tool }
  FCrop.Free();

{ Pen Tools }
  FPathLayer.Free();

  if Assigned(FOldCurvePathList) then
  begin
    FOldCurvePathList.Free();
  end;

{ Measure Tool }
  FMeasureLine.Free();
  FMeasureLayer.Free();

{ Text Tool }
  if Assigned(FOldTextStream) then
  begin
    FOldTextStream.Free();
  end;

  ActiveChildForm := nil;
  frmMain.UpdateToolsOptions();

  FFigureManager.Free();
  FCheckerboardBmp.Free();
  FLayerList.Free();
  FChannelManager.Free();
  FPathList.Free();

  // delete the additional paint stages must be in opposite order of they
  // have been created
  imgWorkArea.PaintStages.Delete(FHandleStageIndex);
end; 

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // call the BeforeExit() procedure to confirm the user to save the image before close the child form
  BeforeExit(Sender);

  if frmRichTextEditor.Visible then
  begin
    frmRichTextEditor.Close;
  end;

  with frmMain do
  begin
    spdbtnStandardMode.Enabled  := False;
    spdbtnQuickMaskMode.Enabled := False;
    edtCropWidth.Text           := '';
    edtCropHeight.Text          := '';
  end;

  frmLayers.LayerPanelManager.LayerList    := nil;
  frmChannels.ChannelViewer.ChannelManager := nil;
  frmPaths.PathManager.PathList            := nil;
  frmHistory.CommandViewer.CommandManager  := nil;

  Action := caFree;  // close the child form
end;

procedure TfrmChild.ChangeCurveControlPoints(Sender: TObject);
begin
  // If FSelectedFigure is nil, it indicates that there is only one figure
  // was selected, otherwise, it indicates that there are more than one
  // figures were selected or there is no any figure was selected.
    
  if Assigned(FSelectedFigure) then
  begin
    if Sender = pmnitmCurveControlP1 then
    begin
      FSelectedFigure.CurveControl := ccpFirst;
    end
    else if Sender = pmnitmCurveControlP2 then
    begin
      FSelectedFigure.CurveControl := ccpSecond;
    end;

    // update view
    imgWorkArea.Changed;
  end;
end;

procedure TfrmChild.pmnChangeCurveControlPointsPopup(Sender: TObject);
begin
  pmnitmCurveControlP1.Checked := False;
  pmnitmCurveControlP2.Checked := False;

  if Assigned(FSelectedFigure) then
  begin
    case FSelectedFigure.CurveControl of
      ccpFirst:
        begin
          pmnitmCurveControlP1.Checked := True;
        end;

      ccpSecond:
        begin
          pmnitmCurveControlP2.Checked := True;
        end;
    end;
  end;
end;

procedure TfrmChild.FormDeactivate(Sender: TObject);
begin
  FinishPolygonalSelection();
  FinishCurves();
  FinishPolygon();

  PrevChildForm := Self;
end;

procedure TfrmChild.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case frmMain.MainTool of
    gmtMarquee:
      begin
        TranslateSelectionKeyDown(Key, Shift);
      end;

    gmtCrop:
      begin
        TranslateCropKeyDown(Key, Shift);
      end;
      
    gmtShape:
      begin
        TranslateShapeRegionKeyDown(Key, Shift);
      end;
      
    gmtMeasure:
      begin
        TranslateMeasureKeyDown(Key, Shift);
      end;
      
    gmtTextTool:
      begin
        TranslateTextKeyDown(Key, Shift);
      end;
  end;
end;

procedure TfrmChild.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FKeyIsDown := False;
  
  case frmMain.MainTool of
    gmtMarquee:
      begin
        TranslateSelectionKeyUp(Key, Shift);
      end;

    gmtShape:
      begin
        TranslateShapeRegionKeyUp(Key, Shift);
      end;
      
    gmtMeasure:
      begin
        TranslateMeasureKeyUp(Key, Shift);
      end;
      
    gmtTextTool:
      begin
        TranslateTextKeyUp(Key, Shift);
      end;
  end;
end;

procedure TfrmChild.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  frmMain.ggbrZoomSlider.Position := frmMain.ggbrZoomSlider.Position + 5;
end; 

procedure TfrmChild.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  frmMain.ggbrZoomSlider.Position := frmMain.ggbrZoomSlider.Position - 5;
end;

procedure TfrmChild.imgWorkAreaPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
var
  LRect             : TRect;
  LDrawHandles      : Boolean;
  LStage            : PPaintStage;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LTextLayer        : TgmRichTextLayer;
begin
  if (Buffer.Height > 0) and (Buffer.Width > 0) then
  begin
    LStage := imgWorkArea.PaintStages[StageNum];

    if LStage.Parameter = PAINT_STAGE_ZERO_PARAMETER then
    begin
      // draw background
      
      Buffer.Clear($FFC0C0C0);

      LRect := imgWorkArea.GetBitmapRect;

      LRect.Left   := LRect.Left   - 1;
      LRect.Top    := LRect.Top    - 1;
      LRect.Right  := LRect.Right  + 1;
      LRect.Bottom := LRect.Bottom + 1;

      // draw thin border, learned from Andre Felix Miertschink
      Buffer.FrameRectS(LRect, clBlack32);
    end
    else if LStage.Parameter = HANDLE_STAGE_PARAMETER then
    begin
      if Assigned(FSelectionTransformation) then
      begin
        if not FSelection.IsAnimated then
        begin
          // If the animation is stopped, we need to render the
          // marching-ants on buffer.
          FSelection.DrawMarchingAnts(Buffer);
        end;

        FSelectionTransformation.DrawOutline(Buffer.Canvas, pmNotXor);
      end
      else
      begin
        // selection tools
        if Assigned(FSelection) then
        begin
          if not FSelection.IsAnimated then
          begin
            // If the animation is stopped, we need to render the
            // marching-ants on buffer.
            FSelection.DrawMarchingAnts(Buffer);
          end;

          if (frmMain.MainTool    = gmtMarquee) and
             (frmMain.MarqueeTool = mtMoveResize) then
          begin
            LDrawHandles := False;

            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(FChannelManager.SelectedAlphaChannel) then
                  begin
                    LDrawHandles := True;
                  end;
                end;

              ctQuickMaskChannel,
              ctLayerMaskChannel:
                begin
                  LDrawHandles := True;
                end;

              ctColorChannel:
                begin
                  if FLayerList.SelectedLayer is TgmNormalLayer then
                  begin
                    LDrawHandles := True;
                  end;
                end;
            end;

            FSelection.DrawMarchingAntsBorder(Buffer.Canvas, LDrawHandles);
          end;
        end;

        // crop tool
        if Assigned(FCrop) then
        begin
          if frmMain.MainTool = gmtCrop then
          begin
            with Buffer.Canvas do
            begin
              Pen.Color   := RUBBER_BAND_PEN_COLOR;
              Pen.Style   := RUBBER_BAND_PEN_STYLE;
              Pen.Width   := RUBBER_BAND_PEN_WIDTH;
              Pen.Mode    := pmNotXor;
              Brush.Color := RUBBER_BAND_BRUSH_COLOR;
              Brush.Style := RUBBER_BAND_BRUSH_STYLE;
            end;

            FCrop.DrawCropBorder(Buffer.Canvas, imgWorkArea.BitmapToControl);

            // do not drawing handles when mouse button is pressed
            if not FDrawing then
            begin
              FCrop.DrawCropHandles(Buffer.Canvas, imgWorkArea.BitmapToControl);
            end;
          end;
        end;

        // shape region tool
        if frmMain.MainTool = gmtShape then
        begin
          if frmMain.ShapeRegionTool in [srtRectangle,
                                         srtRoundedRect,
                                         srtEllipse,
                                         srtPolygon,
                                         srtLine] then
          begin
            with Buffer.Canvas do
            begin
              Pen.Color   := clBlack;
              Pen.Style   := psSolid;
              Pen.Width   := Round(imgWorkArea.Scale);
              Pen.Mode    := pmNotXor;
              Brush.Style := bsClear;
            end;
          end;

          case frmMain.ShapeRegionTool of
            srtMove:
              begin
                if FLayerList.SelectedLayer is TgmShapeRegionLayer then
                begin
                  LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);
                    
                  LShapeRegionLayer.ShapeOutlineList.DrawShapesBoundary(
                    Buffer.Canvas, HANDLE_RADIUS, pmNotXor, imgWorkArea);

                  // drawing handles when mouse button is not pressed
                  if not FDrawing then
                  begin
                    LShapeRegionLayer.ShapeOutlineList.DrawShapesBoundaryHandles(
                      Buffer.Canvas, HANDLE_RADIUS, pmNotXor, imgWorkArea);
                  end;
                end;
              end;

            srtRectangle:
              begin
                // mouse button is pressed
                if FDrawing then
                begin
                  DrawRectangle(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
                end;
              end;

            srtRoundedRect:
              begin
                if FDrawing then
                begin
                  DrawRoundRect(Buffer.Canvas, FStartPoint, FEndPoint,
                    frmMain.ShapeCornerRadius, pmNotXor);
                end;
              end;

            srtEllipse:
              begin
                if FDrawing then
                begin
                  DrawEllipse(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
                end;
              end;

            srtPolygon:
              begin
                if FDrawing then
                begin
                  DrawRegularPolygon(Buffer.Canvas, FStartPoint, FEndPoint,
                    frmMain.ShapePolygonSides, pmNotXor, DONOT_FILL_INSIDE);
                end;
              end;

            srtLine:
              begin
                if FDrawing then
                begin
                  Buffer.Canvas.Pen.Mode := pmNotXor;
                  try
                    DrawLineOutline(Buffer.Canvas, FStartPoint, FEndPoint,
                      frmMain.LineWeight, imgWorkArea.Scale);
                  finally
                    Buffer.Canvas.Pen.Mode := pmCopy;
                  end;
                end;
              end;
          end;

          if FLayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(FLayerList.SelectedLayer);

            if not LShapeRegionLayer.IsDismissed then
            begin
              LShapeRegionLayer.ShapeOutlineList.DrawAllOutlines(
                Buffer.Canvas, pmNotXor, imgWorkArea);
            end;
          end;
        end;

        // text tool
        if frmMain.MainTool = gmtTextTool then
        begin
          with Buffer.Canvas do
          begin
            Pen.Color   := RUBBER_BAND_PEN_COLOR;
            Pen.Style   := RUBBER_BAND_PEN_STYLE;
            Pen.Width   := RUBBER_BAND_PEN_WIDTH;
            Brush.Color := RUBBER_BAND_BRUSH_COLOR;
            Brush.Style := RUBBER_BAND_BRUSH_STYLE;
          end;

          case FRichTextDrawingState of
            dsNewFigure:
              begin
                DrawRectangle(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
              end;
          end;

          if FLayerList.SelectedLayer is TgmRichTextLayer then
          begin
            LTextLayer := TgmRichTextLayer(FLayerList.SelectedLayer);

            LTextLayer.DrawTextBorder(Buffer.Canvas, imgWorkArea.BitmapToControl);

            // don't drawing handles when mouse button is pressed
            if not FDrawing then
            begin
              LTextLayer.DrawTextBorderHandles(Buffer.Canvas, imgWorkArea.BitmapToControl);
            end;
          end;
        end;

        // figure tools
        if frmMain.MainTool = gmtStandard then
        begin
          if frmMain.StandardTool in [gstStraightLine,
                                      gstBezierCurve,
                                      gstPolygon,
                                      gstRegularPolygon,
                                      gstRectangle,
                                      gstRoundRectangle,
                                      gstEllipse] then
          begin
            with Buffer.Canvas do
            begin
              // If color of pen/brush is white, when PenMode is pmNotXor,
              // we couldn't see the progress of drawing.
              Pen.Color   := clBlack;
              Pen.Width   := frmMain.GlobalPenWidth;
              Pen.Style   := frmMain.GlobalPenStyle;
              Brush.Color := clBlack;
              Brush.Style := frmMain.GlobalBrushStyle;
            end;
          end
          else if frmMain.StandardTool in [gstPartiallySelect,
                                           gstTotallySelect] then
          begin
            with imgWorkArea.Canvas do
            begin
              Pen.Color   := RUBBER_BAND_PEN_COLOR;
              Pen.Style   := RUBBER_BAND_PEN_STYLE;
              Pen.Width   := RUBBER_BAND_PEN_WIDTH;
              Brush.Color := RUBBER_BAND_BRUSH_COLOR;
              Brush.Style := RUBBER_BAND_BRUSH_STYLE;
            end;
          end;

          case frmMain.StandardTool of
            gstStraightLine:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  DrawStraightLine(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
                end;
              end;

            gstBezierCurve:
              begin
                if (FDrawCurveTime <= 3) and FDrawing then
                begin
                  DrawPolyBezier(Buffer.Canvas,
                    [FStartPoint, FCurvePoint1, FCurvePoint2, FEndPoint], pmNotXor);
                end;
              end;

            gstPolygon:
              begin
                DrawPolyLine(Buffer.Canvas, FPolygon, pmNotXor);
              end;

            gstRegularPolygon:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  DrawRegularPolygon(Buffer.Canvas, FStartPoint, FEndPoint,
                    frmMain.StandardPolygonSides, pmNotXor, FILL_INSIDE);
                end;
              end;

            gstRectangle:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  DrawRectangle(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
                end;
              end;

            gstRoundRectangle:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  DrawRoundRect(Buffer.Canvas, FStartPoint, FEndPoint,
                    frmMain.StandardCornerRadius, pmNotXor);
                end;
              end;

            gstEllipse:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  DrawEllipse(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
                end;
              end;

            gstMoveObjects:
              begin
                // left mouse button is pressed...
                if FDrawing then
                begin
                  case FMoveDrawingState of
                    dsTranslate:
                      begin
                        FFigureManager.DrawSelectedFiguresOnCanvas(
                          Buffer.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                      end;

                    dsStretchCorner:
                      begin
                        if Assigned(FSelectedFigure) then
                        begin
                          FFigureManager.DrawSelectedFiguresOnCanvas(
                            Buffer.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
                        end;
                      end;
                  end;
                end
                else
                begin
                  if FFigureManager.SelectedFigureCount > 0 then
                  begin
                    FFigureManager.DrawHandlesOfSelectedFigures(Buffer, imgWorkArea.BitmapToControl);
                  end;
                end;
              end;

            gstPartiallySelect,
            gstTotallySelect:
              begin
                DrawRectangle(Buffer.Canvas, FStartPoint, FEndPoint, pmNotXor);
              end;
          end;
        end;

        // gradient tools
        if frmMain.MainTool = gmtGradient then
        begin
          // drawing Gradient indicator line when mouse button is pressed
          if FDrawing then
          begin
            with Buffer.Canvas do
            begin
              Pen.Color := clBlack;
              Pen.Style := psSolid;
              Pen.Width := 1;
              Pen.Mode  := pmNotXor;

              MoveTo(FStartPoint.X, FStartPoint.Y);
              LineTo(FEndPoint.X, FEndPoint.Y);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmChild.tmrSpecialBrushesTimer(Sender: TObject);
var
  LBrushArea : TRect;
begin
  if frmMain.MainTool = gmtBrush then
  begin
    case frmMain.BrushTool of
      btBlurSharpenBrush:
        begin
          if Assigned(FSelection) then
          begin
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              frmMain.GMBrush.Paint(
                FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                FChannelManager.SelectedColorChannels);
            end
            else
            begin
              frmMain.GMBrush.Paint(
                FSelection.CutOriginal, FMarqueeX, FMarqueeY, [csGrayscale]);
            end;

            // get brush area
            LBrushArea := frmMain.GMBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            ShowSelectionAtBrushStroke(LBrushArea);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  frmMain.GMBrush.Paint(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  frmMain.GMBrush.Paint(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  // get brush area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  frmMain.GMBrush.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                        FXActual, FYActual, [csGrayscale]);

                  // paint on layer mask channel as well
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  frmMain.GMBrush.Paint(FLayerList.SelectedLayer.LayerBitmap,
                    FXActual, FYActual, FChannelManager.SelectedColorChannels);

                  // get refresh area
                  LBrushArea := frmMain.GMBrush.GetBrushArea(FXActual, FYActual);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;
          end;
        end;

      btAirBrush:
        begin
          if Assigned(FSelection) then
          begin
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              frmMain.AirBrush.Draw(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                    FChannelManager.SelectedColorChannels);
            end
            else
            begin
              frmMain.AirBrush.Draw(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                    [csGrayscale]);
            end;

            // get brush area
            LBrushArea := frmMain.AirBrush.GetBrushArea(FMarqueeX, FMarqueeY);

            ShowSelectionAtBrushStroke(LBrushArea);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  frmMain.AirBrush.Draw(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  frmMain.AirBrush.Draw(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  // get brush area
                  LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                  frmMain.AirBrush.Draw(FLayerList.SelectedLayer.MaskBitmap,
                                        FXActual, FYActual, [csGrayscale]);

                  // paint on layer mask channel as well
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  frmMain.AirBrush.Draw(FLayerList.SelectedLayer.LayerBitmap,
                    FXActual, FYActual, FChannelManager.SelectedColorChannels);

                  // get refresh area
                  LBrushArea := frmMain.AirBrush.GetBrushArea(FXActual, FYActual);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;
          end;
        end;

      btJetGunBrush:
        begin
          if Assigned(FSelection) then
          begin
            if FChannelManager.CurrentChannelType = ctColorChannel then
            begin
              frmMain.JetGun.Jet(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                 FChannelManager.SelectedColorChannels);
            end
            else
            begin
              frmMain.JetGun.Jet(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                                 [csGrayscale]);
            end;

            // get brush area
            LBrushArea := frmMain.JetGun.GetJetArea(FMarqueeX, FMarqueeY);
            ShowSelectionAtBrushStroke(LBrushArea);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  frmMain.JetGun.Jet(
                    FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                  FChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctQuickMaskChannel:
                begin
                  frmMain.JetGun.Jet(
                    FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                    FXActual, FYActual, [csGrayscale]);

                  // get refresh area
                  LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                  FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed(LBrushArea);
                end;

              ctLayerMaskChannel:
                begin
                  LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                  frmMain.JetGun.Jet(FLayerList.SelectedLayer.MaskBitmap,
                                     FXActual, FYActual, [csGrayscale]);

                  // paint on layer mask channel as well
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                      LBrushArea, LBrushArea, FLayerList.SelectedLayer.MaskBitmap);
                  end;

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  // get refresh area
                  LBrushArea := frmMain.JetGun.GetJetArea(FXActual, FYActual);

                  frmMain.JetGun.Jet(FLayerList.SelectedLayer.LayerBitmap,
                    FXActual, FYActual, FChannelManager.SelectedColorChannels);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TfrmChild.imgWorkAreaResize(Sender: TObject);
begin
  FitMeasureLayerToViewport();
  DrawMeasureLineOnMeasureLayer();

  FitPathLayerToViewport();
  DrawPathOnPathLayer();
end;

procedure TfrmChild.imgWorkAreaScroll(Sender: TObject);
begin
  if Assigned(FMeasureLayer) then
  begin
    FMeasureLayer.Bitmap.Clear($FFFFFFFF);
    DrawMeasureLineOnMeasureLayer();
  end;

  if Assigned(FPathLayer) then
  begin
    FPathLayer.Bitmap.Clear($00000000);
    DrawPathOnPathLayer();
  end;
end;

procedure TfrmChild.tmrSpecialErasersTimer(Sender: TObject);
var
  r, g, b     : Byte;
  LBackEraser : TgmBackgroundEraser;
  LBrushArea  : TRect;
begin
  if frmMain.MainTool = gmtEraser then
  begin
    case frmMain.EraserTool of
      etEraser:
        begin
          if Assigned(FSelection) then
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel,
              ctLayerMaskChannel,
              ctQuickMaskChannel:
                begin
                  frmMain.GMEraser.Paint(FSelection.CutOriginal,
                                         FMarqueeX, FMarqueeY,
                                         [csGrayscale]);
                end;

              ctColorChannel:
                begin
                  frmMain.GMEraser.Paint(FSelection.CutOriginal,
                                         FMarqueeX, FMarqueeY,
                                         FChannelManager.SelectedColorChannels);
                end;
            end;

            // get brush area
            LBrushArea := frmMain.AirBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            ShowSelectionAtBrushStroke(LBrushArea);

            FPrevStrokePoint := Point(FMarqueeX, FMarqueeY);
          end
          else
          begin
            case FChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  with FChannelManager.SelectedAlphaChannel do
                  begin
                    frmMain.GMEraser.Paint(ChannelLayer.Bitmap,
                                           FXActual, FYActual,
                                           [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

                    ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  with FChannelManager.QuickMaskChannel do
                  begin
                    frmMain.GMEraser.Paint(ChannelLayer.Bitmap,
                                           FXActual, FYActual,
                                           [csGrayscale]);

                    // get refresh area
                    LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

                    ChannelLayer.Bitmap.Changed(LBrushArea);
                  end;
                end;

              ctLayerMaskChannel:
                begin
                  frmMain.GMEraser.Paint(FLayerList.SelectedLayer.MaskBitmap,
                                         FXActual, FYActual,
                                         [csGrayscale]);

                  // also paint on mask channel preview layer
                  if Assigned(FChannelManager.LayerMaskChannel) then
                  begin
                    frmMain.GMEraser.Paint(
                      FChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                      FXActual, FYActual, [csGrayscale]);
                  end;

                  // get brush area
                  LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;

              ctColorChannel:
                begin
                  // must be on layer

                  LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

                  frmMain.GMEraser.Paint(FLayerList.SelectedLayer.LayerBitmap,
                                         FXActual, FYActual,
                                         FChannelManager.SelectedColorChannels);

                  FLayerList.SelectedLayer.Changed(LBrushArea);
                end;
            end;

            FPrevStrokePoint := Point(FXActual, FYActual);
          end;
        end;

      etBackgroundEraser:
        begin
          LBackEraser := TgmBackgroundEraser(frmMain.GMEraser);

          if Assigned(FSelection) then
          begin
            // sampling color
            if frmMain.EraserSamplingMode = bsmContiguous then
            begin
              LBackEraser.SamplingColor(FMarqueeX, FMarqueeY);
            end;

            LBackEraser.Paint(FSelection.CutOriginal, FMarqueeX, FMarqueeY,
                              FChannelManager.SelectedColorChannels);

            // get brush area
            LBrushArea := frmMain.AirBrush.GetBrushArea(FMarqueeX, FMarqueeY);
            ShowSelectionAtBrushStroke(LBrushArea);
          end
          else
          begin
            LBrushArea := frmMain.GMEraser.GetBrushArea(FXActual, FYActual);

            // sampling Color
            if frmMain.EraserSamplingMode = bsmContiguous then
            begin
              LBackEraser.SamplingColor(FXActual, FYActual);
            end;

            frmMain.GMEraser.Paint(FLayerList.SelectedLayer.LayerBitmap,
                                   FXActual, FYActual,
                                   FChannelManager.SelectedColorChannels);

            FLayerList.SelectedLayer.Changed(LBrushArea);
          end;

          // show the sampled color
          if frmMain.EraserSamplingMode = bsmContiguous then
          begin
            r := LBackEraser.SampledColor shr 16 and $FF;
            g := LBackEraser.SampledColor shr  8 and $FF;
            b := LBackEraser.SampledColor        and $FF;

            frmColor.ChangeColorViaTrackBar(r, g, b);
          end;
        end;
    end;
  end;
end;

end.
