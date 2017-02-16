unit MainForm;

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
 * x2nie - Fathony Luthfillah < x2nie@yahoo.com >
 *
 * Contributor(s):
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

// Update Date: 2017/01/24

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

interface

{$I ..\GraphicsMagicLib\GraphicsMagicLib.Inc}

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, ComCtrls, ToolWin, ExtCtrls, ExtDlgs, StdCtrls, Buttons,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_RangeBars,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Package Lib }
  gmGradientRender,
{ GraphicsMagic Lib }
  gmBrushes,
  gmChannels,
  gmImageProcessFuncs,
  gmPaintBucket,
  gmPenTools,
  gmPluginManager,
  gmSelection,
  gmShapes,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  ChildForm;

type
  TgmMainTool = (gmtStandard,
                 gmtBrush,
                 gmtMarquee,
                 gmtCrop,
                 gmtMeasure,
                 gmtGradient,
                 gmtPaintBucket,
                 gmtEraser,
                 gmtPenTools,
                 gmtShape,
                 gmtTextTool,
                 gmtEyedropper,
                 gmtHandTool);

  TgmStandardTool = (gstPencil,
                     gstStraightLine,
                     gstBezierCurve,
                     gstPolygon,
                     gstRegularPolygon,
                     gstRectangle,
                     gstRoundRectangle,
                     gstEllipse,
                     gstMoveObjects,
                     gstPartiallySelect,
                     gstTotallySelect);

  TgmGhostDetect = class(TThread)
  private
    FPoint    : TPoint;
    FExecProc : TNotifyEvent;
  public
    procedure Execute; override;

    property Point    : TPoint       read FPoint    write FPoint;
    property ExecProc : TNotifyEvent read FExecProc write FExecProc;
  end;

  TfrmMain = class(TForm)
    mnMain: TMainMenu;
    mnhdFile: TMenuItem;
    mnitmNewFile: TMenuItem;
    N1: TMenuItem;
    mnitmExitProgram: TMenuItem;
    mnhdLayer: TMenuItem;
    mnitmFlattenImage: TMenuItem;
    N2: TMenuItem;
    mnitmOpenFile: TMenuItem;
    N3: TMenuItem;
    mnitmSaveFile: TMenuItem;
    mnitmSaveFileAs: TMenuItem;
    svpctrdlgSavePictures: TSavePictureDialog;
    mnitmClose: TMenuItem;
    mnitmCloseAll: TMenuItem;
    mnhdWindow: TMenuItem;
    mnitmCascade: TMenuItem;
    mnitmTile: TMenuItem;
    mnitmTileHorizontally: TMenuItem;
    mnitmTileVertically: TMenuItem;
    mnitmArrangeIcons: TMenuItem;
    N4: TMenuItem;
    mnitmPathForm: TMenuItem;
    mnitmColorForm: TMenuItem;
    mnitmLayerForm: TMenuItem;
    mnitmStatusBar: TMenuItem;
    mnhdImage: TMenuItem;
    mnitmAdjustments: TMenuItem;
    mnitmBrightnessAndContrast: TMenuItem;
    mnitmColorBalance: TMenuItem;
    mnitmHLSOrHSV: TMenuItem;
    N5: TMenuItem;
    mnitmReplaceColor: TMenuItem;
    mnhdSelect: TMenuItem;
    mnitmSelectAll: TMenuItem;
    mnitmCommitSelection: TMenuItem;
    mnitmDeselect: TMenuItem;
    mnitmDeleteSelection: TMenuItem;
    mnitmInvertSelection: TMenuItem;
    N6: TMenuItem;
    mnitmFeatherRadius: TMenuItem;
    N7: TMenuItem;
    mnitmColorRangeSelection: TMenuItem;
    mnhdEdit: TMenuItem;
    mnitmTransform: TMenuItem;
    mnitmHorizFlip: TMenuItem;
    mnitmVertFlip: TMenuItem;
    N12: TMenuItem;
    mnitmOpenRecent: TMenuItem;
    N13: TMenuItem;
    mnitmFill: TMenuItem;
    N14: TMenuItem;
    mnitmDesaturate: TMenuItem;
    mnitmInvert: TMenuItem;
    mnitmThreshold: TMenuItem;
    mnitmPosterize: TMenuItem;
    N16: TMenuItem;
    mnitmDistortTransformation: TMenuItem;
    mnitmGimpCurvesTool: TMenuItem;
    mnitmGradientMap: TMenuItem;
    N17: TMenuItem;
    mnitmImageSize: TMenuItem;
    mnitmCanvasSize: TMenuItem;
    mnitmRotateCanvas: TMenuItem;
    mnitmRotateClockwise: TMenuItem;
    mnitm30dcw: TMenuItem;
    mnitm45dcw: TMenuItem;
    mnitm60dcw: TMenuItem;
    mnitm90dcw: TMenuItem;
    mnitm180dcw: TMenuItem;
    mnitmRotateCounterclockwise: TMenuItem;
    mnitm30ducw: TMenuItem;
    mnitm45ducw: TMenuItem;
    mnitm60ducw: TMenuItem;
    mnitm90ducw: TMenuItem;
    mnitm180ducw: TMenuItem;
    N18: TMenuItem;
    mnitmRotateArbitrary: TMenuItem;
    mnitmCrop: TMenuItem;
    N20: TMenuItem;
    mnitmHistogram: TMenuItem;
    N21: TMenuItem;
    mnitmCut: TMenuItem;
    mnitmCopy: TMenuItem;
    mnitmPaste: TMenuItem;
    N22: TMenuItem;
    mnitmPrintOptions: TMenuItem;
    mnitmPrintPreview: TMenuItem;
    mnitmPageSetup: TMenuItem;
    mnitmPrintImage: TMenuItem;
    mnitmSaveAll: TMenuItem;
    stsbrMain: TStatusBar;
    N23: TMenuItem;
    mnitmNewAdjustmentLayer: TMenuItem;
    mnitmGimpCurvesLayer: TMenuItem;
    mnitmColorBalanceLayer: TMenuItem;
    mnitmBrightContrastLayer: TMenuItem;
    N24: TMenuItem;
    mnitmHueSaturationLayer: TMenuItem;
    mnitmGradientMapLayer: TMenuItem;
    N25: TMenuItem;
    mnitmInvertLayer: TMenuItem;
    mnitmThresholdLayer: TMenuItem;
    mnitmPosterizeLayer: TMenuItem;
    mnitmNewFillLayer: TMenuItem;
    mnitmSolidColorLayer: TMenuItem;
    mnitmGradientFillLayer: TMenuItem;
    mnitmPatternFillLayer: TMenuItem;
    mnitmInfoForm: TMenuItem;
    N26: TMenuItem;
    mnitmArrangeLayer: TMenuItem;
    mnitmBringLayerToFront: TMenuItem;
    mnitmBringLayerForward: TMenuItem;
    mnitmSendLayerBackward: TMenuItem;
    mnitmSendLayerToBack: TMenuItem;
    mnitmMergeVisibleLayers: TMenuItem;
    mnitmMergeDown: TMenuItem;
    N31: TMenuItem;
    mnitmDuplicateLayer: TMenuItem;
    mnitmDeleteLayer: TMenuItem;
    mnhdHelp: TMenuItem;
    mnitmAbout: TMenuItem;
    N27: TMenuItem;
    mnitmLayerProperties: TMenuItem;
    mnitmRotateTransformation: TMenuItem;
    mnitmScaleTransformation: TMenuItem;
    mnitmGimpLevelsTool: TMenuItem;
    mnitmGimpAutoLevels: TMenuItem;
    mnitmGimpLevelsLayer: TMenuItem;
    N28: TMenuItem;
    mnitmUndoRedo: TMenuItem;
    mnhdFilter: TMenuItem;
    mnitmLastFilter: TMenuItem;
    N30: TMenuItem;
    mnitmStepforeward: TMenuItem;
    mnitmStepBackward: TMenuItem;
    mnitmHistoryForm: TMenuItem;
    N8: TMenuItem;
    mnitmPreferences: TMenuItem;
    mnitmGeneralPreferences: TMenuItem;
    mnitmShowSplash: TMenuItem;
    N9: TMenuItem;
    mnitmSoftwarePageCH: TMenuItem;
    mnitmFiltersUpdate: TMenuItem;
    mnitmFiltersPageCH: TMenuItem;
    mnitmFiltersPageEN: TMenuItem;
    mnitmSoftwarePageEN: TMenuItem;
    N10: TMenuItem;
    mnitmDownloadCH: TMenuItem;
    mnitmDownloadEN: TMenuItem;
    mnitmDownloadSourceCodeAtSourceForge: TMenuItem;
    mnitmOptimalCrop: TMenuItem;
    mnitmChannelMixer: TMenuItem;
    mnitmChannelMixerLayer: TMenuItem;
    N11: TMenuItem;
    mnitmApplyImage: TMenuItem;
    pnlRightDockArea: TPanel;
    pnlZoom: TPanel;
    lblZoomViewer: TLabel;
    spdbtnZoomOut: TSpeedButton;
    ggbrZoomSlider: TGaugeBar;
    spdbtnZoomIn: TSpeedButton;
    pgcntrlDockSite1: TPageControl;
    Splitter1: TSplitter;
    pgcntrlDockSite2: TPageControl;
    Splitter2: TSplitter;
    pgcntrlDockSite3: TPageControl;
    pnlToolBoxHolder: TPanel;
    spdbtnStandardTools: TSpeedButton;
    spdbtnBrushTools: TSpeedButton;
    spdbtnMarqueeTools: TSpeedButton;
    spdbtnGradientTools: TSpeedButton;
    spdbtnCropTools: TSpeedButton;
    spdbtnPaintBucketTools: TSpeedButton;
    spdbtnEraserTools: TSpeedButton;
    spdbtnHandTool: TSpeedButton;
    spdbtnEyedropper: TSpeedButton;
    spdbtnMeasureTool: TSpeedButton;
    spdbtnTextTool: TSpeedButton;
    spdbtnShapeTools: TSpeedButton;
    spdbtnPenTools: TSpeedButton;
    Bevel1: TBevel;
    spdbtnQuickMaskMode: TSpeedButton;
    spdbtnStandardMode: TSpeedButton;
    Bevel2: TBevel;
    pnlToolOptionsVisibility: TPanel;
    imgToolOptionsVisibility: TImage32;
    pnlToolOptions: TPanel;
    ntbkToolOptions: TNotebook;
    pnlFigureOpt: TPanel;
    pnlFigureOptHeader: TPanel;
    scrlbxFigureOptions: TScrollBox;
    spdbtnPencil: TSpeedButton;
    spdbtnStraightLine: TSpeedButton;
    spdbtnBezierCurve: TSpeedButton;
    spdbtnPolygon: TSpeedButton;
    spdbtnRegularPolygon: TSpeedButton;
    spdbtnRectangle: TSpeedButton;
    spdbtnRoundRectangle: TSpeedButton;
    spdbtnEllipse: TSpeedButton;
    spdbtnMoveObjects: TSpeedButton;
    spdbtnRSPartially: TSpeedButton;
    spdbtnRSTotally: TSpeedButton;
    spdbtnSelectFigures: TSpeedButton;
    spdbtnFigureProperties: TSpeedButton;
    spdbtnSelectAllFigures: TSpeedButton;
    spdbtnDeleteFigures: TSpeedButton;
    spdbtnLockFigures: TSpeedButton;
    spdbtnUnlockFigures: TSpeedButton;
    lblPenStyle: TLabel;
    lblPenWidth: TLabel;
    lblRadiusSides: TLabel;
    spdbtnSelectBrushStyle: TSpeedButton;
    lblBrushStyle: TLabel;
    cmbbxPenStyle: TComboBox;
    cmbbxPenWidth: TComboBox;
    edtRadiusSides: TEdit;
    updwnRadiusSides: TUpDown;
    pnlBrushStyleHolder: TPanel;
    imgBrushStyleViewer: TImage;
    pnlBrushOpt: TPanel;
    pnlBrushOptHeader: TPanel;
    scrlbxBrushOptions: TScrollBox;
    spdbtnPaintBrush: TSpeedButton;
    spdbtnHistoryBrush: TSpeedButton;
    spdbtnAirBrush: TSpeedButton;
    spdbtnJetGun: TSpeedButton;
    spdbtnCloneStamp: TSpeedButton;
    spdbtnPatternStamp: TSpeedButton;
    spdbtnBlurBrush: TSpeedButton;
    spdbtnSharpenBrush: TSpeedButton;
    spdbtnSmudge: TSpeedButton;
    spdbtnDodgeBrush: TSpeedButton;
    spdbtnBurnBrush: TSpeedButton;
    spdbtnHighHueBrush: TSpeedButton;
    spdbtnLowHueBrush: TSpeedButton;
    spdbtnHighSaturationBrush: TSpeedButton;
    spdbtnLowSaturationBrush: TSpeedButton;
    spdbtnHighLuminosityBrush: TSpeedButton;
    spdbtnLowLuminosityBrush: TSpeedButton;
    spdbtnBrightnessBrush: TSpeedButton;
    spdbtnDarknessBrush: TSpeedButton;
    spdbtnHighContrastBrush: TSpeedButton;
    spdbtnLowContrastBrush: TSpeedButton;
    spdbtnSelectPaintingBrush: TSpeedButton;
    lblBrushStroke: TLabel;
    spdbtnBrushOpenPatternSelector: TSpeedButton;
    lblStampPattern: TLabel;
    spdbtnBrushDynamics: TSpeedButton;
    lblBrushMode: TLabel;
    lblBrushOPEI: TLabel;
    Label1: TLabel;
    lblBrushInterval: TLabel;
    lblBrushRadius: TLabel;
    pnlPaintingBrushHolder: TPanel;
    imgPaintingBrush: TImage32;
    pnlBrushPatternHolder: TPanel;
    imgPatternForStamp: TImage32;
    cmbbxBrushMode: TComboBox;
    edtBrushOPEI: TEdit;
    updwnBrushOPEI: TUpDown;
    edtBrushInterval: TEdit;
    updwnBrushInterval: TUpDown;
    edtBrushRadius: TEdit;
    updwnBrushRadius: TUpDown;
    chckbxColorAndSample: TCheckBox;
    pnlSelectionOpt: TPanel;
    pnlSelectionOptHeader: TPanel;
    scrlbxMarqueeOptions: TScrollBox;
    spdbtnSelect: TSpeedButton;
    spdbtnSingleRowMarquee: TSpeedButton;
    spdbtnSingleColumnMarquee: TSpeedButton;
    spdbtnRectangularMarquee: TSpeedButton;
    spdbtnRoundRectangularMarquee: TSpeedButton;
    spdbtnEllipticalMarquee: TSpeedButton;
    spdbtnPolygonalMarquee: TSpeedButton;
    spdbtnRegularPolygonMarquee: TSpeedButton;
    spdbtnMagneticLasso: TSpeedButton;
    spdbtnLassoMarquee: TSpeedButton;
    spdbtnMagicWand: TSpeedButton;
    spdbtnNewSelection: TSpeedButton;
    spdbtnAddSelection: TSpeedButton;
    spdbtnSubtractSelection: TSpeedButton;
    spdbtnIntersectSelection: TSpeedButton;
    spdbtnExcludeOverlapSelection: TSpeedButton;
    spdbtnCommitSelection: TSpeedButton;
    spdbtnDeselect: TSpeedButton;
    spdbtnDeleteSelection: TSpeedButton;
    lblToleranceSides: TLabel;
    edtToleranceSides: TEdit;
    updwnToleranceSides: TUpDown;
    chckbxUseAllLayers: TCheckBox;
    chckbxMagneticLassoInteractive: TCheckBox;
    pnlGradientOpt: TPanel;
    pnlGradientOptHeader: TPanel;
    scrlbxGradientOptions: TScrollBox;
    spdbtnLinearGradient: TSpeedButton;
    spdbtnRadialGradient: TSpeedButton;
    spdbtnAngleGradient: TSpeedButton;
    spdbtnReflectedGradient: TSpeedButton;
    spdbtnDiamondGradient: TSpeedButton;
    spdbtnOpenGradientPicker: TSpeedButton;
    lblGradientBlendeMode: TLabel;
    lblGradientOpacity: TLabel;
    Label2: TLabel;
    pnlGradientSelector: TPanel;
    imgSelectedGradient: TImage32;
    cmbbxGradientBlendMode: TComboBox;
    edtGradientOpacity: TEdit;
    updwnGradientOpacity: TUpDown;
    chckbxReverseGradient: TCheckBox;
    pnlCropOpt: TPanel;
    pnlCropOptHeader: TPanel;
    scrlbxCropOptions: TScrollBox;
    pnlCropOptions: TPanel;
    lblCropWidth: TLabel;
    lblCropHeight: TLabel;
    edtCropWidth: TEdit;
    edtCropHeight: TEdit;
    btnCommitCrop: TButton;
    btnCancelCrop: TButton;
    chckbxShieldCroppedArea: TCheckBox;
    chckbxResizeCrop: TCheckBox;
    pnlCropShield: TPanel;
    lblCroppedShieldColor: TLabel;
    shpCroppedShieldColor: TShape;
    lblCroppedShieldOpacity: TLabel;
    lblShieldOpacityPercentSign: TLabel;
    pnlCropShieldHeader: TPanel;
    edtCroppedShieldOpacity: TEdit;
    updwnCroppedShieldOpacity: TUpDown;
    pnlResizeCrop: TPanel;
    lblResizeCropWidth: TLabel;
    lblResizeCropHeight: TLabel;
    pnlResizeCropHeader: TPanel;
    edtResizeCropWidth: TEdit;
    edtResizeCropHeight: TEdit;
    btnShowCropedAreaSize: TButton;
    pnlPaintBucketOpt: TPanel;
    pnlPaintBucketOptHeader: TPanel;
    scrlbxPaintBucketOptions: TScrollBox;
    lblPaintBucketFillSource: TLabel;
    lblPaintBucketFillMode: TLabel;
    lblPaintBucketOpacity: TLabel;
    lblPaintBucketTolerance: TLabel;
    lblPaintBucketPattern: TLabel;
    spdbtnOpenPatternForFill: TSpeedButton;
    spdbtnPaintBucketAdvancedOptions: TSpeedButton;
    cmbbxPaintBucketFillSource: TComboBox;
    cmbbxPaintBucketFillMode: TComboBox;
    edtPaintBucketOpacity: TEdit;
    updwnPaintBucketOpacity: TUpDown;
    edtPaintBucketTolerance: TEdit;
    udpwnPaintBucketTolerance: TUpDown;
    pnlFillPatternHolder: TPanel;
    imgPatternForPaintBucket: TImage32;
    chckbxFillContiguous: TCheckBox;
    chckbxFillAllLayers: TCheckBox;
    pnlEraserOpt: TPanel;
    pnlEraserOptHeader: TPanel;
    scrlbxEraserOptions: TScrollBox;
    spdbtnEraser: TSpeedButton;
    spdbtnBackgroundEraser: TSpeedButton;
    spdbtnMagicEraser: TSpeedButton;
    lblEraserPaintBrush: TLabel;
    spdbtnSelectEraserStroke: TSpeedButton;
    spdbtnEraserAdvancedOptions: TSpeedButton;
    spdbtnEraserDynamics: TSpeedButton;
    lblEraserModeLimit: TLabel;
    lblEraserSampling: TLabel;
    lblEraserOpacityPressure: TLabel;
    lblEraserOpacityPercentSign: TLabel;
    lblEraserInterval: TLabel;
    lblEraserTolerance: TLabel;
    lblEraserTolerancePercentSign: TLabel;
    pnlEraserBrushHolder: TPanel;
    imgEraserPaintingBrush: TImage32;
    cmbbxEraserModeLimit: TComboBox;
    cmbbxEraserSampling: TComboBox;
    edtEraserOpacityPressure: TEdit;
    updwnEraserOpacityPressure: TUpDown;
    edtEraserInterval: TEdit;
    updwnEraserInterval: TUpDown;
    edtEraserTolerance: TEdit;
    updwnEraserTolerance: TUpDown;
    pnlPenPathOpt: TPanel;
    pnlPenPathOptHeader: TPanel;
    scrlbxPenToolsOptions: TScrollBox;
    spdbtnPathComponentSelectionTool: TSpeedButton;
    spdbtnDirectSelectionTool: TSpeedButton;
    spdbtnPenTool: TSpeedButton;
    spdbtnAddAnchorPointTool: TSpeedButton;
    spdbtnDeleteAnchorPointTool: TSpeedButton;
    spdbtnConvertPointTool: TSpeedButton;
    pnlMeasureOpt: TPanel;
    pnlMeasureOptHeader: TPanel;
    scrlbxMeasureOptions: TScrollBox;
    lblMeasureUnit: TLabel;
    lblMeasureStartX: TLabel;
    lblMStartXValue: TLabel;
    lblMeasureStartY: TLabel;
    lblMStartYValue: TLabel;
    lblMeasureWidth: TLabel;
    lblMWidthValue: TLabel;
    lblMeasureHeight: TLabel;
    lblMHeightValue: TLabel;
    lblMeasureD1: TLabel;
    lblMD1Value: TLabel;
    lblMeasureD2: TLabel;
    lblMD2Value: TLabel;
    lblMeasureAngle: TLabel;
    lblMAngleValue: TLabel;
    Bevel3: TBevel;
    cmbbxMeasureUnit: TComboBox;
    btnClearMeasureInfo: TButton;
    pnlShapeToolOpt: TPanel;
    pnlShapeToolOptHeader: TPanel;
    scrlbxShapeOptions: TScrollBox;
    spdbtnShapeMove: TSpeedButton;
    spdbtnShapeRectangle: TSpeedButton;
    spdbtnShapeRoundRect: TSpeedButton;
    spdbtnShapeEllipse: TSpeedButton;
    spdbtnShapeRegularPolygon: TSpeedButton;
    spdbtnShapeLine: TSpeedButton;
    spdbtnAddShape: TSpeedButton;
    spdbtnSubtractShape: TSpeedButton;
    spdbtnIntersectShape: TSpeedButton;
    spdbtnExcludeOverlapShape: TSpeedButton;
    spdbtnDismissTargetPath: TSpeedButton;
    lblShapeToolRSW: TLabel;
    spdbtnShapeBrushStyle: TSpeedButton;
    lblShapeBrushStyle: TLabel;
    edtShapeToolRSW: TEdit;
    updwnShapeToolRSW: TUpDown;
    pnlShapeBrushHolder: TPanel;
    imgShapeBrushStyle: TImage;
    pnlTextToolOpt: TPanel;
    pnlTextToolOptHeader: TPanel;
    scrlbxTextOptions: TScrollBox;
    lblFontFamily: TLabel;
    lblFontColor: TLabel;
    shpFontColor: TShape;
    lblFontSize: TLabel;
    spdbtnLeftAlignText: TSpeedButton;
    spdbtnCenterText: TSpeedButton;
    spdbtnRightAlignText: TSpeedButton;
    spdbtnSelectAllRichText: TSpeedButton;
    spdbtnClearRichText: TSpeedButton;
    spdbtnCommitEdits: TSpeedButton;
    spdbtnCancelEdits: TSpeedButton;
    spdbtnOpenRichText: TSpeedButton;
    spdbtnSaveRichText: TSpeedButton;
    spdbtnSaveRichTextAs: TSpeedButton;
    cmbbxFontFamily: TComboBox;
    cmbbxFontSize: TComboBox;
    pnlFontStyleHolder: TPanel;
    tlbrRichTextSetup: TToolBar;
    tlbtnBold: TToolButton;
    tlbtnItalic: TToolButton;
    tlbtnUnderline: TToolButton;
    mnitmChannelForm: TMenuItem;
    mnitmSwatchForm: TMenuItem;
    spdbtnGhost: TSpeedButton;
    spdbtnUntouched: TSpeedButton;
    Bevel4: TBevel;
    N15: TMenuItem;
    mnitmGhostMode: TMenuItem;
    mnitmGhostSleepingMode: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnitmLastFilterClick(Sender: TObject);
    procedure ChangeMainToolClick(Sender: TObject);
    procedure PopupBrushStyleMenusClick(Sender: TObject);
    procedure ZoomSliderChange(Sender: TObject);
    procedure cmbbxPenStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure spdbtnZoomInClick(Sender: TObject);
    procedure spdbtnZoomOutClick(Sender: TObject);
    procedure SetChildFormEditMode(Sender: TObject);
    procedure ChangeStandardTools(Sender: TObject);
    procedure cmbbxPenStyleChange(Sender: TObject);
    procedure ChangeGlobalPenWidth(Sender: TObject);
    procedure cmbbxPenWidthDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ChangeRadiusSides(Sender: TObject);
    procedure ChangeBrushTools(Sender: TObject);
    procedure OpenPaintingBrushSelector(Sender: TObject);
    procedure OpenPatternSelectorForStamp(Sender: TObject);
    procedure OpenBrushDynamicsEditor(Sender: TObject);
    procedure ChangeBrushMode(Sender: TObject);
    procedure ChangeBrushOPEI(Sender: TObject);
    procedure ChangeBrushInterval(Sender: TObject);
    procedure ChangeBrushRadius(Sender: TObject);
    procedure chckbxColorAndSampleClick(Sender: TObject);
    procedure ChangeMarqueeTools(Sender: TObject);
    procedure MarqueeToolButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeMarqueeMode(Sender: TObject);
    procedure edtToleranceSidesChange(Sender: TObject);
    procedure ChangeGradientTools(Sender: TObject);
    procedure spdbtnOpenGradientPickerClick(Sender: TObject);
    procedure imgSelectedGradientClick(Sender: TObject);
    procedure imgSelectedGradientPaintStage(Sender: TObject;
      Buffer: TBitmap32; StageNum: Cardinal);
    procedure ChangeGradientBlendMode(Sender: TObject);
    procedure ChangeGradientOpacity(Sender: TObject);
    procedure chckbxReverseGradientClick(Sender: TObject);
    procedure edtCropWidthChange(Sender: TObject);
    procedure edtCropHeightChange(Sender: TObject);
    procedure ShieldCroppedArea(Sender: TObject);
    procedure chckbxResizeCropClick(Sender: TObject);
    procedure shpCroppedShieldColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeCroppedShieldOpacity(Sender: TObject);
    procedure edtResizeCropWidthChange(Sender: TObject);
    procedure edtResizeCropWidthExit(Sender: TObject);
    procedure edtResizeCropHeightChange(Sender: TObject);
    procedure edtResizeCropHeightExit(Sender: TObject);
    procedure btnShowCropedAreaSizeClick(Sender: TObject);
    procedure ChangePaintBucketFillSource(Sender: TObject);
    procedure ChangePaintBucketFillMode(Sender: TObject);
    procedure ChangePaintBucketOpacity(Sender: TObject);
    procedure ChangePaintBucketTolerance(Sender: TObject);
    procedure spdbtnOpenPatternForFillClick(Sender: TObject);
    procedure spdbtnPaintBucketAdvancedOptionsClick(Sender: TObject);
    procedure ChangeEraserTools(Sender: TObject);
    procedure spdbtnSelectEraserStrokeClick(Sender: TObject);
    procedure spdbtnEraserAdvancedOptionsClick(Sender: TObject);
    procedure spdbtnEraserDynamicsClick(Sender: TObject);
    procedure ChangeEraserModeLimit(Sender: TObject);
    procedure ChangeEraserSampling(Sender: TObject);
    procedure ChangeEraserOpacityPressure(Sender: TObject);
    procedure ChangeEraserInterval(Sender: TObject);
    procedure ChangeEraserTolerance(Sender: TObject);
    procedure ChangePenTools(Sender: TObject);
    procedure ChangeMeasureUnit(Sender: TObject);
    procedure ClearMeasureInfoClick(Sender: TObject);
    procedure ChangeShapeRegionTools(Sender: TObject);
    procedure ChangeRegionCombineMode(Sender: TObject);
    procedure edtShapeToolRSWChange(Sender: TObject);
    procedure cmbbxFontFamilyChange(Sender: TObject);
    procedure shpFontColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmbbxFontSizeChange(Sender: TObject);
    procedure tlbtnBoldClick(Sender: TObject);
    procedure tlbtnItalicClick(Sender: TObject);
    procedure tlbtnUnderlineClick(Sender: TObject);
    procedure spdbtnLeftAlignTextClick(Sender: TObject);
    procedure spdbtnCenterTextClick(Sender: TObject);
    procedure spdbtnRightAlignTextClick(Sender: TObject);
    procedure spdbtnSelectAllRichTextClick(Sender: TObject);
    procedure spdbtnClearRichTextClick(Sender: TObject);
    procedure MainToolsDblClick(Sender: TObject);
    procedure imgToolOptionsVisibilityClick(Sender: TObject);
    procedure SetCanExecClickMark(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnGhostClick(Sender: TObject);
    procedure spdbtnUntouchedClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
{ Common }
    FOutputGraphicsFormat: TgmOutputGraphicsFormat;
    FChildFormIsCreating : Boolean;
    FFigureUnits         : TgmAppliedUnit;  // unit for figures
    FNewBitmapWidth      : Integer;
    FNewBitmapHeight     : Integer;
    FGlobalForeColor     : TColor;
    FGlobalBackColor     : TColor;
    FForeGrayColor       : TColor;       // grayscale color of the global foreground color
    FBackGrayColor       : TColor;       // grayscale color of the global background color
    FOpenRecentPathList  : TStringList;  // path list of opened files
    FOpenRecentMenuList  : TList;        // menu list of OpenRecent

    FMainTool            : TgmMainTool;
    FCanChange           : Boolean;
    FCanExecClick        : Boolean;      // prevent from the OnClick events of Buttons be execute twice
    FGlobalPenStyle      : TPenStyle;
    FGlobalPenWidth      : Integer;
    FGlobalBrushStyle    : TBrushStyle;

{ Figure Tools Fields }
    FPencil              : TgmPaintBrush;
    FStandardTool        : TgmStandardTool;
    FLastStandardTool    : TgmStandardTool;
    FStandardCornerRadius: Integer;
    FStandardPolygonSides: Integer;

{ Brush Tools Fields }
    FGMBrush                 : TgmBrush;
    FAirBrush                : TgmAirBrush;
    FJetGun                  : TgmJetGun;
    FBrushTool               : TgmBrushTool;
    FBrushBlendMode          : TBlendMode32;
    FBrushOpacity            : Integer;
    FBrushIntensity          : Integer;
    FBrushRadius             : Integer;
    FBrushInterval           : Integer;
    FSmudgePressure          : Integer;
    FBlurSharpenPressure     : Integer;
    FBlurSharpenTimerInterval: Integer;
    FDodgeBurnExposure       : Integer;
    FDodgeBurnMode           : TgmDodgeBurnMode;
    FAirPressure             : Byte;
    FJetGunPressure          : Byte;
    FIsRandomColor           : Boolean;
    FIsUseAllLayers          : Boolean;

{ Marquee Tools Fields }
    FMarqueeTool       : TgmMarqueeTools;
    FMarqueeMode       : TgmMarqueeMode;
    FRPMSides          : Integer;  // sides of regular polygonal marquee
    FRRMCornerRadius   : Integer;  // corner radius of rounded-rectangular marquee
    FMagicWandTolerance: Byte;

{ Gradient Tools Fields }
    FGradientRenderMode  : TgmGradientRenderMode;
    FGradientBlendMode   : TBlendMode32;
    FGradientBlendOpacity: Integer;

{ Paint Bucket Tools Fields }
    FPaintBucketFillSource: TgmPaintBucketFillSource;
    FPaintBucketBlendMode : TBlendMode32;
    FPaintBucketOpacity   : Byte;
    FPaintBucketTolerance : Byte;

{ Eraser Tools Fields }
    FGMEraser             : TgmBrush;
    FEraserTool           : TgmEraserTool;
    FErasingMode          : TgmErasingMode;
    FBackgroundEraserLimit: TgmBackgroundEraserLimit;
    FEraserSamplingMode   : TgmBackgroundSamplingMode;
    FErasingOpacity       : Integer;
    FErasingInterval      : Integer;
    FAirErasingPressure   : Integer;
    FAirErasingInterval   : Integer;
    FErasingTolerance     : Byte;

{ Pen Tools Fields }
    FPenTool      : TgmPenTools;
    FActivePenTool: TgmPenTools;

{ Shape Tools Fields }
    FShapeRegionTool  : TgmShapeRegionTool;
    FRegionCombineMode: TgmRegionCombineMode;
    FShapeCornerRadius: Integer;
    FShapePolygonSides: Integer;
    FLineWeight       : Integer;
    FShapeBrushStyle  : TBrushStyle;

{ Filter }
    FGMPluginInfoList      : TgmPluginInfoList;
    FFilterCategoryMenuList: TgmCategoryMenuItemList;
    FLastFilterMenuItem    : TMenuItem;

{ Ghost }
    FGhosts          : TList;
    FGhostDetect     : TgmGhostDetect;
    FGhostModeEnabled: Boolean;

    procedure GhostsWakeUp(WakeUp:Boolean);
    procedure GhostsFly;
    procedure WMWINDOWPOSCHANGING(var Msg: TWMWINDOWPOSCHANGING); message WM_WINDOWPOSCHANGING;
    procedure DetectMousePos(ASender: TObject);

    procedure InitFilterMenuItem;
    procedure ExecuteFilters(Sender: TObject);
    
    function LoadGMDInChildForm(const AFileName: string): Boolean;

{ Open Recent }
    procedure LoadOpenRecentPathToList(AStringList: TStringList);
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure WriteOpenRecentInfoToIniFile;

{ Undo/Redo }
    procedure CreateCommandStorageFolder;  // to create a folder to save the temporary command file

{ Callbacks }
    // callbacks for GMD manager
    procedure AfterGMDFileLoaded(ASender: TObject; const AFileVersion: Integer;
      const ALayerMaskColor: TColor32; const ALayerMaskOpacity: Single;
      const ALayerMaskColorIndicator: TgmMaskColorIndicator);

{ Tools }
    function StandardToolsGetStatusInfo: string;
    function BrushToolsGetStatusInfo: string;
    function MarqueeToolsGetStatusInfo: string;
    function GradientToolsGetStatusInfo: string;
    function EraserToolsGetStatusInfo: string;
    function PenToolsGetStatusInfo: string;
    function ShapeToolsGetStatusInfo: string;

    procedure ShowStatusInfoOnStatusBar;

{ Text Tools }
    procedure GetFontNames(const AItems: TStrings);  // Get system supported fonts.
  public
    // global bitmaps used for various image adjustment dialogs, such as filters
    FBitmapBefore       : TBitmap32;
    FBitmapAfter        : TBitmap32;
    FSelectionClipboard : TgmSelection;  // save the selection as if it is in clipboard

    function OpenImageInChildForm(const AFileName: string): Boolean;

    // When trying to open an opened file, then we just show the form with the
    // opened file to top of the screen. 
    function ShowOpenedImageTop(const CheckFileName: string): Boolean;

    procedure AddFilePathToList(AStringList: TStringList; const AFilePath: string);  // for open recent
    procedure DuplicateSelection;
    procedure GhostsFade(Fade:Boolean);
    procedure ReloadFilters;
    procedure ShowColorRGBInfoOnInfoViewer(const AColor: TColor);
    procedure ShowColorCMYKInfoOnInfoViewer(const AColor: TColor);
    procedure ShowOriginalCoordInfoOnInfoViewerInPixel(const X, Y: Integer);
    procedure ShowCurrentCoordInfoOnInfoViewerInPixel(const X, Y: Integer);
    procedure UpdateStandardOptions;     // Update components of standard page.
    procedure UpdateBrushOptions;        // Update components of brush page.
    procedure UpdateMarqueeOptions;      // Update components of marquee page.
    procedure UpdateCropOptions;         // Update components of crop page.
    procedure UpdatePaintBucketOptions;  // Update components of paint bucket page.
    procedure UpdateEraserOptions;       // Update components of eraser page.
    procedure UpdateMeasureOptions;      // Update componets of measure page.
    procedure UpdateOpenRecentMenuItem;  // for open recent
    procedure UpdateShapeOptions;        // Update componets of shape page.
    procedure UpdateTextOptions;         // Update componets of text page.
    procedure UpdateToolsOptions;

    function PasteSelection: Boolean;

    { This function will loops through each child form and get all the
      file names in that form that has the same size image with the destination
      image which was in the Active Child Form. The function will return all
      the file names it found as result. It will return nil if no such image is
      matched. }
    function GetFileNamesWithSameSizeImage: TStringList;
    function GetChildFormPointerByFileName(const AFileName: string): TfrmChild;

{ Brush Tools }
    function GetBrushName: string;

{ Text Tools }
    procedure ChangeIndexByFontName(const AFontName: string);
    procedure ChangeIndexByFontSize(const AFontSize: Byte);

{ Common Properties }
    property OutputGraphicsFormat: TgmOutputGraphicsFormat read FOutputGraphicsFormat write FOutputGraphicsFormat;
    property ChildFormIsCreating : Boolean                 read FChildFormIsCreating  write FChildFormIsCreating;
    property FigureUnits         : TgmAppliedUnit          read FFigureUnits          write FFigureUnits;
    property NewBitmapWidth      : Integer                 read FNewBitmapWidth;
    property NewBitmapHeight     : Integer                 read FNewBitmapHeight;
    property GlobalForeColor     : TColor                  read FGlobalForeColor      write FGlobalForeColor;
    property GlobalBackColor     : TColor                  read FGlobalBackColor      write FGlobalBackColor;
    property ForeGrayColor       : TColor                  read FForeGrayColor        write FForeGrayColor;
    property BackGrayColor       : TColor                  read FBackGrayColor        write FBackGrayColor;
    property MainTool            : TgmMainTool             read FMainTool;
    property CanChange           : Boolean                 read FCanChange            write FCanChange;
    property GlobalPenStyle      : TPenStyle               read FGlobalPenStyle;
    property GlobalPenWidth      : Integer                 read FGlobalPenWidth;
    property GlobalBrushStyle    : TBrushStyle             read FGlobalBrushStyle     write FGlobalBrushStyle;
    property IsGhostModeEnabled  : Boolean                 read FGhostModeEnabled;
    property OpenRecentMenuList  : TList                   read FOpenRecentMenuList;
    property OpenRecentPathList  : TStringList             read FOpenRecentPathList;

{ Figure Tools Properties }
    property StandardTool        : TgmStandardTool read FStandardTool;
    property LastStandardTool    : TgmStandardTool read FLastStandardTool;
    property StandardCornerRadius: Integer         read FStandardCornerRadius;
    property StandardPolygonSides: Integer         read FStandardPolygonSides;
    property Pencil              : TgmPaintBrush   read FPencil;

{ Brush Tools Properties }
    property GMBrush                 : TgmBrush         read FGMBrush;
    property AirBrush                : TgmAirBrush      read FAirBrush;
    property JetGun                  : TgmJetGun        read FJetGun;
    property BrushTool               : TgmBrushTool     read FBrushTool;
    property BrushBlendMode          : TBlendMode32     read FBrushBlendMode;
    property BrushOpacity            : Integer          read FBrushOpacity;
    property BrushIntensity          : Integer          read FBrushIntensity;
    property BrushInterval           : Integer          read FBrushInterval;
    property SmudgePressure          : Integer          read FSmudgePressure;
    property BlurSharpenPressure     : Integer          read FBlurSharpenPressure;
    property BlurSharpenTimerInterval: Integer          read FBlurSharpenTimerInterval;
    property DodgeBurnExposure       : Integer          read FDodgeBurnExposure;
    property DodgeBurnMode           : TgmDodgeBurnMode read FDodgeBurnMode;
    property AirPressure             : Byte             read FAirPressure;
    property JetGunPressure          : Byte             read FJetGunPressure;
    property IsRandomColor           : Boolean          read FIsRandomColor;
    property IsUseAllLayers          : Boolean          read FIsUseAllLayers;

{ Marquee Tools Properties }
    property MarqueeTool       : TgmMarqueeTools read FMarqueeTool;
    property MarqueeMode       : TgmMarqueeMode  read FMarqueeMode;
    property RPMSides          : Integer         read FRPMSides;
    property RRMCornerRadius   : Integer         read FRRMCornerRadius;
    property MagicWandTolerance: Byte            read FMagicWandTolerance;

{ Gradient Tools Properties }
    property GradientRenderMode  : TgmGradientRenderMode read FGradientRenderMode;
    property GradientBlendMode   : TBlendMode32          read FGradientBlendMode;
    property GradientBlendOpacity: Integer               read FGradientBlendOpacity;

{ Paint Bucket Tools Propperties }
    property PaintBucketFillSource: TgmPaintBucketFillSource read FPaintBucketFillSource;
    property PaintBucketBlendMode : TBlendMode32             read FPaintBucketBlendMode;
    property PaintBucketOpacity   : Byte                     read FPaintBucketOpacity;
    property PaintBucketTolerance : Byte                     read FPaintBucketTolerance;

{ Eraser Tools Properties }
    property GMEraser             : TgmBrush                  read FGMEraser;
    property EraserTool           : TgmEraserTool             read FEraserTool;
    property ErasingMode          : TgmErasingMode            read FErasingMode;
    property BackgroundEraserLimit: TgmBackgroundEraserLimit  read FBackgroundEraserLimit;
    property EraserSamplingMode   : TgmBackgroundSamplingMode read FEraserSamplingMode;
    property ErasingOpacity       : Integer                   read FErasingOpacity;
    property ErasingInterval      : Integer                   read FErasingInterval;
    property AirErasingPressure   : Integer                   read FAirErasingPressure;
    property AirErasingInterval   : Integer                   read FAirErasingInterval;
    property ErasingTolerance     : Byte                      read FErasingTolerance;

{ Pen Tools properties }
    property PenTool      : TgmPenTools read FPenTool write FPenTool;
    property ActivePenTool: TgmPenTools read FActivePenTool;

{ Shape Tool properties }
    property ShapeRegionTool  : TgmShapeRegionTool   read FShapeRegionTool;
    property RegionCombineMode: TgmRegionCombineMode read FRegionCombineMode;
    property ShapeCornerRadius: Integer              read FShapeCornerRadius;
    property ShapePolygonSides: Integer              read FShapePolygonSides;
    property LineWeight       : Integer              read FLineWeight;
    property ShapeBrushStyle  : TBrushStyle          read FShapeBrushStyle write FShapeBrushStyle;
  end;

var
  frmMain        : TfrmMain;
  ActiveChildForm: TfrmChild;  // global pointer to the currently active child form
  PrevChildForm  : TfrmChild;

const
  VER : string = '2';

implementation

uses
{ Standard }
  Clipbrd,
  INIFiles,
{ Externals }
  ColorLibrary,
{ Graphics32 }
  GR32_Backends,
  GR32_Layers,
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmAlphaFuncs,
  gmBrightContrastLayer,
  gmChannelCommands,
  gmChannelManager,
  gmCommonFuncs,
  gmConstants,
  gmConvolve,
  gmFigures,
  gmGMDFile,                   // used to load GraphicsMagic work flow from disk
  gmGUIFuncs,
  gmHistoryCommands,
  gmIni,
  gmIO,                        // LoadGraphicsFile(), SaveGraphicsFile()
  gmLayers,
  gmMath,
  gmMeasure,
  gmPaintFuncs,
  gmShapeRegionLayer,
  gmVectorLayer,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms }
  BrushDynamicsPopFrm,
  ChannelForm,
  ColorForm,
  EraserAdvOptionsPopFrm,
  GhostForm,
  GradientPickerPopFrm,
  HistoryForm,
  InfoForm,                    // frmInfo
  LayerForm,                   // frmLayers
  PaintingBrushPopFrm,
  PaintBucketOptionsPopFrm,
  PathForm,
  PatternsPopFrm,
  RichTextEditorForm,          // frmRichTextEditor
  SplashForm,
  SwatchForm,
{ GraphicsMagic Dialogs }
  AboutDlg,
  GradientEditorDlg;

{$R *.DFM}

const
  STANDARD_PAGE_INDEX     = 0;
  BRUSH_PAGE_INDEX        = 1;
  MARQUEE_PAGE_INDEX      = 2;
  GRADIENT_PAGE_INDEX     = 3;
  CROP_PAGE_INDEX         = 4;
  PAINT_BUCKET_PAGE_INDEX = 5;
  ERASER_PAGE_INDEX       = 6;
  PEN_TOOLS_PAGE_INDEX    = 7;
  MEASURE_PAGE_INDEX      = 8;
  SHAPE_PAGE_INDEX        = 9;
  TEXT_PAGE_INDEX         = 10;
  BLANK_PAGE_INDEX        = 11;

  LEFT_GHOST_FORM_INDEX   = 0;
  RIGHT_GHOST_FORM_INDEX  = 1;

{ these methods for common use }

procedure TgmGhostDetect.Execute;
var
  p: TPoint;
begin
  while not Terminated do
  begin
    GetCursorPos(p);

    //dont send update when mouse is same as previous position
    if ( (p.X <> FPoint.X) or (p.Y <> FPoint.Y) ) and
       Assigned(FExecProc) then
    begin
      FPoint := p;
      FExecProc(Self);
    end;

    Sleep(10); //dont be too fast while looping
  end;
end;

// this procedure is for plug-ins for showing the processed image by themselves
procedure UpdateView;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(Selection) then
      begin
        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          if (LayerList.SelectedLayer.IsLockTransparency) or
             (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
          begin
            // preserve transparency
            ReplaceAlphaChannelWithSource(Selection.CutOriginal, frmMain.FBitmapBefore);
          end;
        end;

        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctQuickMaskChannel:
            begin
              if Assigned(ChannelManager.QuickMaskChannel) then
              begin
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctLayerMaskChannel:
            begin
              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              if (LayerList.SelectedLayer.IsLockTransparency) or
                 (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
              begin
                // preserve transparency
                ReplaceAlphaChannelWithSource(
                  LayerList.SelectedLayer.LayerBitmap, frmMain.FBitmapBefore);
              end;

              LayerList.SelectedLayer.Changed();
            end;
        end;
      end;

    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

// callbacks for GMD manager
procedure TfrmMain.AfterGMDFileLoaded(ASender: TObject;
  const AFileVersion: Integer; const ALayerMaskColor: TColor32;
  const ALayerMaskOpacity: Single;
  const ALayerMaskColorIndicator: TgmMaskColorIndicator);
var
  LLayer : TgmCustomLayer;
  LRect  : TRect;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  if ActiveChildForm.LayerList.Count > 0 then
  begin
    with ActiveChildForm do
    begin
      // set history bitmap
      LLayer := LayerList.GetBackgroundLayer();

      if Assigned(LLayer) then
      begin
        HistoryBitmap.Assign(LLayer.LayerBitmap);
      end
      else
      begin
        LLayer := LayerList.Layers[0];
        HistoryBitmap.Assign(LLayer.LayerBitmap);
      end;

      // TODO: in the future
      // create Snapshot for history command

      // set background size before create background layer
      imgWorkArea.Bitmap.SetSize(HistoryBitmap.Width, HistoryBitmap.Height);
      imgWorkArea.Bitmap.Clear($00000000);

      // draw checkerboard pattern
      CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
      DrawCheckerboardPattern(CheckerboardBmp, CheckerboardBmp.ClipRect);

      SetCallbacksForLayersInList();

      // set location for channel layers ...
      LRect             := imgWorkArea.GetBitmapRect;
      LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

      ChannelManager.ChannelLayerLocation := FloatRect(LRect);

      if (LayerList.SelectedLayer.LayerProcessStage = lpsMask) and
          LayerList.SelectedLayer.IsMaskEnabled and
          Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.SelectLayerMaskChannel();

        ChannelManager.LayerMaskChannel.MaskColor          := ALayerMaskColor;
        ChannelManager.LayerMaskChannel.MaskOpacity        := Round(ALayerMaskOpacity * 255);
        ChannelManager.LayerMaskChannel.MaskColorIndicator := ALayerMaskColorIndicator;
      end;

      // update the view
      LayerList.SelectedLayer.Changed();

      // channels ...
      LRect             := imgWorkArea.GetBitmapRect;
      LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

      ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);
      ChannelManager.ChannelLayerLocation := FloatRect(LRect);

      // if it is in QuickMask edit mode now ...
      if Assigned(ChannelManager.QuickMaskChannel) then
      begin
        EditMode := emQuickMaskMode;
        ChannelManager.SelectQuickMaskChannel();
        spdbtnQuickMaskMode.Down := True;
      end;

      // Add a snapshot (just the opened image) to the Command Manager.
      CommandManager.AddSnapshot( LayerList.CombineResult,
                                  ExtractFileName(FileName) );

      CommandManager.SelectSnapshot(0);
    end;
  end;
end;

procedure TfrmMain.InitFilterMenuItem;
var
  i, j            : Integer;
  FilterPath      : string;
  CategoryName    : string;
  PluginName      : string;
  GMPlugin        : TgmPlugin;
  PluginInfo      : TgmPluginInfo;
  FilterPathList  : TStringList;
  CategoryList    : TgmUniqueStringList;
  PluginNameList  : TgmUniqueStringList;
  CategoryMenuItem: TMenuItem;
begin
  if Assigned(FGMPluginInfoList) and Assigned(FFilterCategoryMenuList) then
  begin
    FilterPathList := TStringList.Create;
    try
      // Get sorted string list of filter files.
      FilterPath := ExtractFilePath( ParamStr(0) ) + 'Plug-Ins';

      ZarkoFileSearch(FilterPath, '*.gmp', False, FilterPathList);
      FilterPathList.Sort;

      // Get info of all filters.
      if FilterPathList.Count > 0 then
      begin
        for i := 0 to FilterPathList.Count - 1 do
        begin
          GMPlugin := TgmPlugin.Create(FilterPathList.Strings[i]);
          try
            CategoryName := GMPlugin.GetPluginCategory;
            PluginName   := GMPlugin.GetPluginName;

            PluginInfo                    := TgmPluginInfo.Create;
            PluginInfo.LibFileName        := FilterPathList.Strings[i];
            PluginInfo.CategoryName       := CategoryName;
            PluginInfo.PluginName         := PluginName;
            PluginInfo.IsAdjustable       := GMPlugin.IsAdjustablePlugin;
            PluginInfo.IsChannelSupported := GMPlugin.IsChannelSupported;

            FGMPluginInfoList.Add(PluginInfo);
          finally
            GMPlugin.Free;
          end;
        end;

        // Create filter menu items.
        CategoryList := FGMPluginInfoList.GetSortedCategoryList;

        if CategoryList <> nil then
        begin
          // Create filter's category menu items (One-Level menu).
          for i := 0 to CategoryList.Count - 1 do
          begin
            CategoryMenuItem         := TMenuItem.Create(Self);
            CategoryName             := CategoryList.Strings[i];
            CategoryMenuItem.Caption := CategoryName;
            CategoryMenuItem.Visible := True;

            mnhdFilter.Add(CategoryMenuItem);
            FFilterCategoryMenuList.Add(CategoryMenuItem);

            // Create filter menu items (Two-Level menu).
            PluginNameList := FGMPluginInfoList.GetSortedPluginNameList(CategoryName);

            if PluginNameList <> nil then
            begin
              for j := 0 to PluginNameList.Count - 1 do
              begin
                PluginName := PluginNameList.Strings[j];
                PluginInfo := FGMPluginInfoList.GetPluginInfoByName(PluginName);

                if PluginInfo <> nil then
                begin
                  PluginInfo.PluginMenu := TMenuItem.Create(Self);

                  if PluginInfo.IsAdjustable then
                  begin
                    PluginInfo.PluginMenu.Caption := PluginName + '...';
                  end
                  else
                  begin
                    PluginInfo.PluginMenu.Caption := PluginName;
                  end;

                  PluginInfo.PluginMenu.OnClick := ExecuteFilters;
                  CategoryMenuItem.Add(PluginInfo.PluginMenu);
                end;
              end;

              PluginNameList.Clear;
              FreeAndNil(PluginNameList);
            end;
          end;

          CategoryList.Clear;
          CategoryList.Free;
        end;
      end;
    finally
      FilterPathList.Clear;
      FilterPathList.Free;
    end;
  end;
end;

procedure TfrmMain.ExecuteFilters(Sender: TObject);
var
  LPluginInfo : TgmPluginInfo;
  LGMPlugin   : TgmPlugin;
  P           : PColor32;
  W, H        : Integer;
  LFillColor  : TColor32;
begin
  LFillColor := $00000000;
  P          := nil;
  W          := 0;
  H          := 0;

  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  if Assigned(FGMPluginInfoList) then
  begin
    with ActiveChildForm do
    begin
      // for Undo/Redo
      SaveChannelPixelsBeforePixelProcessing();

      // prepare the filling color
      if Assigned(Selection) then
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel:
            begin
              LFillColor := clBlack32;
            end;

          ctLayerMaskChannel:
            begin
              LFillColor := clWhite32;
            end;

          ctColorChannel:
            begin
              LFillColor := $00000000;
            end;
        end;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel:
            begin
              LFillColor := clBlack32;
            end;

          ctLayerMaskChannel:
            begin
              LFillColor := clWhite32;
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                if TgmNormalLayer(LayerList.SelectedLayer).IsAsBackground then
                begin
                  LFillColor := Color32(FGlobalBackColor);
                end
                else
                begin
                  LFillColor := $00000000;
                end;
              end;
            end;
        end;
      end;

      LPluginInfo := FGMPluginInfoList.GetPluginInfoByMenuItem(Sender);

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if (LayerList.SelectedLayer is TgmNormalLayer) and
           ( TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer() ) then
        begin
          MessageDlg('Could not complete the ' + LPluginInfo.PluginName +
                     ' command' + #10#13 +
                     'because the active layer is empty.',
                     mtError, [mbOK], 0);
          Exit;
        end;
      end;

      if LPluginInfo <> nil then
      begin
        if not FileExists(LPluginInfo.LibFileName) then
        begin
          MessageDlg('The filter you called is not exist.', mtError, [mbOK], 0);
          Exit;
        end;

        LGMPlugin := TgmPlugin.Create(LPluginInfo.LibFileName);
        try
          if Assigned(Selection) then
          begin
            P := @Selection.CutOriginal.Bits[0];
            W := Selection.CutOriginal.Width;
            H := Selection.CutOriginal.Height;
          end
          else
          begin
            case ChannelManager.CurrentChannelType of
              ctAlphaChannel:
                begin
                  if Assigned(ChannelManager.SelectedAlphaChannel) then
                  begin
                    P := @ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Bits[0];
                    W := ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Width;
                    H := ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Height;
                  end;
                end;

              ctQuickMaskChannel:
                begin
                  if Assigned(ChannelManager.QuickMaskChannel) then
                  begin
                    P := @ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Bits[0];
                    W := ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Width;
                    H := ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Height;
                  end;
                end;

              ctLayerMaskChannel:
                begin
                  P := @LayerList.SelectedLayer.MaskBitmap.Bits[0];
                  W := LayerList.SelectedLayer.MaskBitmap.Width;
                  H := LayerList.SelectedLayer.MaskBitmap.Height;
                end;

              ctColorChannel:
                begin
                  P := @LayerList.SelectedLayer.LayerBitmap.Bits[0];
                  W := LayerList.SelectedLayer.LayerBitmap.Width;
                  H := LayerList.SelectedLayer.LayerBitmap.Height;
                end;
            end;
          end;

          // setting channels
          if LPluginInfo.IsChannelSupported then
          begin
            if ChannelManager.CurrentChannelType = ctColorChannel then
            begin
              LGMPlugin.SetChannels(ChannelManager.SelectedColorChannels);
            end
            else
            begin
              LGMPlugin.SetChannels([csGrayscale]);
            end;
          end;

          // if filter runs sucessfully...
          if LGMPlugin.RunPlugin(P, W, H, @UpdateView, LFillColor) then
          begin
            UpdateView();

            // Undo/Redo
            CreateUndoRedoCommandForPixelProcessing(LPluginInfo.PluginName);

            // Update the last filter menu item.
            mnitmLastFilter.Caption := LPluginInfo.PluginName;
            FLastFilterMenuItem     := Sender as TMenuItem;
          end
          else
          begin
            UpdateView(); // The filter runs failed...
          end;

          if Assigned(Selection) then
          begin
            Selection.IsAnimated := True;
          end;

          // update thumnails
          UpdateThumbnailsBySelectedChannel();
        finally
          LGMPlugin.Free();
        end;
      end;
    end;
  end;
end;

// to create a folder to save the temporary command files
procedure TfrmMain.CreateCommandStorageFolder;
var
  LCmdDir: string;
begin
  LCmdDir := ExtractFileDir(ParamStr(0)) + '\History';

  if not DirectoryExists(LCmdDir) then
  begin
    CreateDir(LCmdDir);
  end;
end;

procedure TfrmMain.ShowStatusInfoOnStatusBar;
var
  LInfo: string;
begin
  case FMainTool of
    gmtStandard:
      begin
        LInfo := StandardToolsGetStatusInfo;
      end;

    gmtBrush:
      begin
        LInfo := BrushToolsGetStatusInfo;
      end;

    gmtMarquee:
      begin
        LInfo := MarqueeToolsGetStatusInfo;
      end;

    gmtGradient:
      begin
        LInfo := GradientToolsGetStatusInfo;
      end;

    gmtCrop:
      begin
        LInfo := '[Crop]';
      end;

    gmtPaintBucket:
      begin
        LInfo := '[Paint Bucket]';
      end;

    gmtEraser:
      begin
        LInfo := EraserToolsGetStatusInfo;
      end;

    gmtPenTools:
      begin
        LInfo := PenToolsGetStatusInfo;
      end;

    gmtMeasure:
      begin
        LInfo := '[Measure]';
      end;

    gmtShape:
      begin
        LInfo := ShapeToolsGetStatusInfo;
      end;

    gmtTextTool:
      begin
        LInfo := '[Text]';
      end;

    gmtEyedropper:
      begin
        LInfo := '[Eyedropper]';
      end;

    gmtHandTool:
      begin
        LInfo := '[Hand Tool]';
      end;
  end;

  stsbrMain.Panels[1].Text := LInfo;
end;

function TfrmMain.StandardToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Standard]';

  case FStandardTool of
    gstPencil:
      begin
        LInfo := LInfo + spdbtnPencil.Hint;
      end;

    gstStraightLine:
      begin
        LInfo := LInfo + spdbtnStraightLine.Hint;
      end;

    gstBezierCurve:
      begin
        LInfo := LInfo + spdbtnBezierCurve.Hint;
      end;

    gstPolygon:
      begin
        LInfo := LInfo + spdbtnPolygon.Hint;
      end;

    gstRegularPolygon:
      begin
        LInfo := LInfo + spdbtnRegularPolygon.Hint;
      end;

    gstRectangle:
      begin
        LInfo := LInfo + spdbtnRectangle.Hint;
      end;

    gstRoundRectangle:
      begin
        LInfo := LInfo + spdbtnRoundRectangle.Hint;
      end;

    gstEllipse:
      begin
        LInfo := LInfo + spdbtnEllipse.Hint;
      end;

    gstMoveObjects:
      begin
        LInfo := LInfo + spdbtnMoveObjects.Hint;
      end;

    gstPartiallySelect:
      begin
        LInfo := LInfo + spdbtnRSPartially.Hint;
      end;

    gstTotallySelect:
      begin
        LInfo := LInfo + spdbtnRSTotally.Hint;
      end;
  end;

  if FStandardTool in [gstRectangle, gstRoundRectangle, gstEllipse] then
  begin
    LInfo := LInfo + ' -- Hold down the Shift key when drawing to get a regular figure.';
  end
  else
  if FStandardTool = gstMoveObjects then
  begin
    LInfo := LInfo + ' -- When the two control points of a curve are overlapped, right click on them to make your choise.';
  end;

  Result := LInfo;
end;

function TfrmMain.BrushToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Brush]';

  if spdbtnPaintBrush.Down then
  begin
    LInfo := LInfo + spdbtnPaintBrush.Hint;
  end
  else if spdbtnHistoryBrush.Down then
  begin
    LInfo := LInfo + spdbtnHistoryBrush.Hint;
  end
  else if spdbtnAirBrush.Down then
  begin
    LInfo := LInfo + spdbtnAirBrush.Hint;
  end
  else if spdbtnJetGun.Down then
  begin
    LInfo := LInfo + spdbtnJetGun.Hint;
  end
  else if spdbtnCloneStamp.Down then
  begin
    LInfo := LInfo + spdbtnCloneStamp.Hint;
  end
  else if spdbtnPatternStamp.Down then
  begin
    LInfo := LInfo + spdbtnPatternStamp.Hint;
  end
  else if spdbtnBlurBrush.Down then
  begin
    LInfo := LInfo + spdbtnBlurBrush.Hint;
  end
  else if spdbtnSharpenBrush.Down then
  begin
    LInfo := LInfo + spdbtnSharpenBrush.Hint;
  end
  else if spdbtnSmudge.Down then
  begin
    LInfo := LInfo + spdbtnSmudge.Hint;
  end
  else if spdbtnDodgeBrush.Down then
  begin
    LInfo := LInfo + spdbtnDodgeBrush.Hint;
  end
  else if spdbtnBurnBrush.Down then
  begin
    LInfo := LInfo + spdbtnBurnBrush.Hint;
  end
  else if spdbtnHighHueBrush.Down then
  begin
    LInfo := LInfo + spdbtnHighHueBrush.Hint;
  end
  else if spdbtnLowHueBrush.Down then
  begin
    LInfo := LInfo + spdbtnLowHueBrush.Hint;
  end
  else if spdbtnHighSaturationBrush.Down then
  begin
    LInfo := LInfo + spdbtnHighSaturationBrush.Hint;
  end
  else if spdbtnLowSaturationBrush.Down then
  begin
    LInfo := LInfo + spdbtnLowSaturationBrush.Hint;
  end
  else if spdbtnHighLuminosityBrush.Down then
  begin
    LInfo := LInfo + spdbtnHighLuminosityBrush.Hint;
  end
  else if spdbtnLowLuminosityBrush.Down then
  begin
    LInfo := LInfo + spdbtnLowLuminosityBrush.Hint;
  end
  else if spdbtnBrightnessBrush.Down then
  begin
    LInfo := LInfo + spdbtnBrightnessBrush.Hint;
  end
  else if spdbtnDarknessBrush.Down then
  begin
    LInfo := LInfo + spdbtnDarknessBrush.Hint;
  end
  else if spdbtnHighContrastBrush.Down then
  begin
    LInfo := LInfo + spdbtnHighContrastBrush.Hint;
  end
  else if spdbtnLowContrastBrush.Down then
  begin
    LInfo := LInfo + spdbtnLowContrastBrush.Hint;
  end;

  Result := LInfo;
end;

function TfrmMain.MarqueeToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Marquee]';

  case FMarqueeTool of
    mtMoveResize:
      begin
        LInfo := LInfo + spdbtnSelect.Hint;
      end;

    mtSingleRow:
      begin
        LInfo := LInfo + spdbtnSingleRowMarquee.Hint;
      end;

    mtSingleColumn:
      begin
        LInfo := LInfo + spdbtnSingleColumnMarquee.Hint;
      end;

    mtRectangular:
      begin
        LInfo := LInfo + spdbtnRectangularMarquee.Hint;
      end;

    mtRoundRectangular:
      begin
        LInfo := LInfo + spdbtnRoundRectangularMarquee.Hint;
      end;

    mtElliptical:
      begin
        LInfo := LInfo + spdbtnEllipticalMarquee.Hint;
      end;

    mtPolygonal:
      begin
        LInfo := LInfo + spdbtnPolygonalMarquee.Hint;
      end;

    mtRegularPolygon:
      begin
        LInfo := LInfo + spdbtnRegularPolygonMarquee.Hint;
      end;

    mtLasso:
      begin
        LInfo := LInfo + spdbtnLassoMarquee.Hint;
      end;

    mtMagicWand:
      begin
        LInfo := LInfo + spdbtnMagicWand.Hint;
      end;
  end;

  if FMarqueeTool in [mtRectangular, mtRoundRectangular, mtElliptical] then
  begin
    LInfo := LInfo + ' -- Hold down the Shift key when drawing to get a regular marquee.';
  end;

  Result := LInfo;
end;

function TfrmMain.GradientToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Gradient]';

  case FGradientRenderMode of
    grmLinear:
      begin
        LInfo := LInfo + spdbtnLinearGradient.Hint;
      end;

    grmRadial:
      begin
        LInfo := LInfo + spdbtnRadialGradient.Hint;
      end;

    grmAngle:
      begin
        LInfo := LInfo + spdbtnAngleGradient.Hint;
      end;

    grmReflected:
      begin
        LInfo := LInfo + spdbtnReflectedGradient.Hint;
      end;

    grmDiamond:
      begin
        LInfo := LInfo + spdbtnDiamondGradient.Hint;
      end;
  end;

  Result := LInfo + ' -- Click the gradient viewer to edit gradient.';
end;

function TfrmMain.EraserToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Eraser]';

  case FEraserTool of
    etEraser:
      begin
        LInfo := LInfo + spdbtnEraser.Hint;
      end;

    etBackgroundEraser:
      begin
        LInfo := LInfo + spdbtnBackgroundEraser.Hint;
      end;

    etMagicEraser:
      begin
        LInfo := LInfo + spdbtnMagicEraser.Hint;
      end;
  end;

  Result := LInfo;
end;

function TfrmMain.PenToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Pen Tools]';

  case FPenTool of
    ptPathComponentSelection:
      begin
        LInfo := LInfo + spdbtnPathComponentSelectionTool.Hint;
        LInfo := LInfo + '-- Click to select and/or drag a subpath.';
      end;

    ptDirectSelection:
      begin
        LInfo := LInfo + spdbtnDirectSelectionTool.Hint;
        LInfo := LInfo + '-- Click to select and/or drag an anchor point.';
      end;

    ptPenTool:
      begin
        LInfo := LInfo + spdbtnPenTool.Hint;
        LInfo := LInfo + '-- Click to add points to the path. Click on start point to close path.';
      end;

    ptAddAnchorPoint:
      begin
        LInfo := LInfo + spdbtnAddAnchorPointTool.Hint;
        LInfo := LInfo + '-- Click on path to insert anchor points into the path. ';
      end;

    ptDeleteAnchorPoint:
      begin
        LInfo := LInfo + spdbtnDeleteAnchorPointTool.Hint;
        LInfo := LInfo + '-- Click on anchor points to delete from the path. ';
      end;

    ptConvertPoint:
      begin
        LInfo := LInfo + spdbtnConvertPointTool.Hint;
        LInfo := LInfo + '-- Click curve point to change to corner point. Click-drag converts back to curve point.';
      end;
  end;

  Result := LInfo;
end;

function TfrmMain.ShapeToolsGetStatusInfo: string;
var
  LInfo: string;
begin
  LInfo := '[Shape]';

  case FShapeRegionTool of
    srtMove:
      begin
        LInfo := LInfo + spdbtnShapeMove.Hint;
      end;

    srtRectangle:
      begin
        LInfo := LInfo + spdbtnShapeRectangle.Hint;
      end;

    srtRoundedRect:
      begin
        LInfo := LInfo + spdbtnShapeRoundRect.Hint;
      end;

    srtEllipse:
      begin
        LInfo := LInfo + spdbtnShapeEllipse.Hint;
      end;

    srtPolygon:
      begin
        LInfo := LInfo + spdbtnShapeRegularPolygon.Hint;
      end;

    srtLine:
      begin
        LInfo := LInfo + spdbtnShapeLine.Hint;
      end;
  end;

  if FShapeRegionTool in [srtRectangle, srtRoundedRect, srtEllipse] then
  begin
    LInfo := LInfo + ' -- Hold down the Shift key when drawing to get a regular shape.';
  end;

  Result := LInfo;
end;

// adding system supported fonts to a list
function EnumFontsProc(var ALogFont: TLogFont; var ATextMetric: TTextMetric;
  AFontType: Integer; AData: Pointer): Integer; stdcall;
begin
  TStrings(AData).Add(ALogFont.lfFaceName);
  Result := 1;
end;

// Get system supported fonts.
procedure TfrmMain.GetFontNames(const AItems: TStrings);
var
  DC: HDC;
begin
  DC := GetDC(0);

  EnumFonts( DC, nil, @EnumFontsProc, Pointer(AItems) );
  ReleaseDC(0, DC);
end;

procedure TfrmMain.ShowColorRGBInfoOnInfoViewer(const AColor: TColor);
begin
  with frmInfo do
  begin
    lblRed.Caption   := 'R: '   + IntToStr( GetRValue(AColor) );
    lblGreen.Caption := 'G: '   + IntToStr( GetGValue(AColor) );
    lblBlue.Caption  := 'B: '   + IntToStr( GetBValue(AColor) );
    lblHex.Caption   := 'Hex: ' + ColorToString(AColor);
  end;
end;

procedure TfrmMain.ShowColorCMYKInfoOnInfoViewer(const AColor: TColor);
var
  c, m, y, k: Integer;
  RGBTriple : TRGBTriple;
begin
  with RGBTriple do
  begin
    rgbtRed   := GetRValue(AColor);
    rgbtGreen := GetGValue(AColor);
    rgbtBlue  := GetBValue(AColor);
  end;

  RGBTripleToCMYK(RGBTriple, c, m, y, k);

  with frmInfo do
  begin
    lblCyan.Caption    := 'C: ' + IntToStr(c);
    lblMagenta.Caption := 'M: ' + IntToStr(m);
    lblYellow.Caption  := 'Y: ' + IntToStr(y);
    lblBlack.Caption   := 'K: ' + IntToStr(k);
  end;
end;

procedure TfrmMain.ShowOriginalCoordInfoOnInfoViewerInPixel(const X, Y: Integer);
begin
  frmInfo.lblOriginalX.Caption := 'X: ' + IntToStr(X);
  frmInfo.lblOriginalY.Caption := 'Y: ' + IntToStr(Y);
end;

procedure TfrmMain.ShowCurrentCoordInfoOnInfoViewerInPixel(const X, Y: Integer);
begin
  frmInfo.lblCurrentX.Caption := 'X: ' + IntToStr(X);
  frmInfo.lblCurrentY.Caption := 'Y: ' + IntToStr(Y);
end;

// Update components of standard page.
procedure TfrmMain.UpdateStandardOptions;
var
  LAvailable : Boolean;
begin
  case FStandardTool of
    gstRegularPolygon:
      begin
        lblRadiusSides.Caption    := 'Sides:';
        lblRadiusSides.Hint       := 'Set number of sides';
        edtRadiusSides.Hint       := 'Set number of sides';
        updwnRadiusSides.Hint     := 'Set number of sides';
        updwnRadiusSides.Min      := 3;
        updwnRadiusSides.Max      := 100;
        updwnRadiusSides.Position := FStandardPolygonSides;
      end;

    gstRoundRectangle:
      begin
        lblRadiusSides.Caption    := 'Radius:';
        lblRadiusSides.Hint       := 'Set radius of rounded corners';
        edtRadiusSides.Hint       := 'Set radius of rounded corners';
        updwnRadiusSides.Hint     := 'Set radius of rounded corners';
        updwnRadiusSides.Min      := 0;
        updwnRadiusSides.Max      := 1000;
        updwnRadiusSides.Position := FStandardCornerRadius;
      end;
  end;

  LAvailable             := FStandardTool in [gstRegularPolygon, gstRoundRectangle];
  lblRadiusSides.Enabled := LAvailable;
  edtRadiusSides.Enabled := LAvailable;

  if LAvailable then
  begin
    edtRadiusSides.Color := clWindow;
  end
  else
  begin
    edtRadiusSides.Color := clBtnFace;
  end;

  updwnRadiusSides.Enabled := LAvailable;
end;

// Update components of brush page.
procedure TfrmMain.UpdateBrushOptions;

  procedure UpdateMode;
  begin
    if FBrushTool in [btPaintBrush, btHistoryBrush, btAirBrush,
                      btJetGunBrush, btCloneStamp, btPatternStamp,
                      btBlurSharpenBrush, btSmudge] then
    begin
      lblBrushMode.Caption     := 'Mode:';
      cmbbxBrushMode.Hint      := 'Set blend mode for stroke';
      cmbbxBrushMode.Clear;
      cmbbxBrushMode.Items     := BlendModeList;
      cmbbxBrushMode.ItemIndex := Ord(FBrushBlendMode);
    end
    else
    if FBrushTool = btDodgeBurnBrush then
    begin
      lblBrushMode.Caption     := 'Range:';
      cmbbxBrushMode.Hint      := 'Set dodge/burn range for stroke';
      cmbbxBrushMode.Clear;
      cmbbxBrushMode.Items     := DodgeBurnRangeList;
      cmbbxBrushMode.ItemIndex := Ord(FDodgeBurnMode);
    end;
  end;

  // The OPEI represents Opacity, Pressure, Exposure and Intensity.
  procedure UpdateBrushOPEI;
  begin
    if FBrushTool in [btPaintBrush, btHistoryBrush, btCloneStamp,
                      btPatternStamp] then
    begin
      lblBrushOPEI.Caption     := 'Opacity:';
      edtBrushOPEI.Hint        := 'Set opacity for stroke';
      updwnBrushOPEI.Position  := FBrushOpacity;
    end
    else
    if FBrushTool = btAirBrush then
    begin
      lblBrushOPEI.Caption     := 'Pressure:';
      edtBrushOPEI.Hint        := 'Set pressure for stroke';
      updwnBrushOPEI.Position  := FAirPressure;
    end
    else
    if FBrushTool = btBlurSharpenBrush then
    begin
      lblBrushOPEI.Caption     := 'Pressure:';
      edtBrushOPEI.Hint        := 'Set pressure for stroke';
      updwnBrushOPEI.Position  := FBlurSharpenPressure;
    end
    else
    if FBrushTool = btJetGunBrush then
    begin
      lblBrushOPEI.Caption     := 'Pressure:';
      edtBrushOPEI.Hint        := 'Set pressure for stroke';
      updwnBrushOPEI.Position  := FJetGunPressure;
    end
    else
    if FBrushTool = btSmudge then
    begin
      lblBrushOPEI.Caption     := 'Pressure:';
      edtBrushOPEI.Hint        := 'Set pressure for stroke';
      updwnBrushOPEI.Position  := FSmudgePressure;
    end
    else
    if FBrushTool = btDodgeBurnBrush then
    begin
      lblBrushOPEI.Caption     := 'Exposure:';
      edtBrushOPEI.Hint        := 'Set exposure for stroke';
      updwnBrushOPEI.Position  := FDodgeBurnExposure;
    end
    else
    if FBrushTool = btLightBrush then
    begin
      lblBrushOPEI.Caption     := 'Intensity:';
      edtBrushOPEI.Hint        := 'Set intensity for stroke';
      updwnBrushOPEI.Position  := FBrushIntensity;
    end;

    updwnBrushOPEI.Hint := edtBrushOPEI.Hint;
  end;

var
  LAvailable: Boolean;
begin
  UpdateMode;
  UpdateBrushOPEI;
  spdbtnSelectPaintingBrush.Enabled := (FBrushTool <> btJetGunBrush);

  cmbbxBrushMode.Enabled := (FBrushTool in [btPaintBrush,
                                            btHistoryBrush,
                                            btAirBrush,
                                            btJetGunBrush,
                                            btCloneStamp,
                                            btPatternStamp,
                                            btBlurSharpenBrush,
                                            btSmudge,
                                            btDodgeBurnBrush]);

  lblBrushMode.Enabled   := cmbbxBrushMode.Enabled;
  LAvailable             := (FBrushTool = btJetGunBrush);
  edtBrushRadius.Enabled := LAvailable;

  if LAvailable then
  begin
    edtBrushRadius.Color := clWindow;
  end
  else
  begin
    edtBrushRadius.Color := clBtnFace;
  end;

  lblBrushRadius.Enabled                 := LAvailable;
  updwnBrushRadius.Enabled               := LAvailable;
  updwnBrushInterval.Enabled             := (FBrushTool <> btSmudge);
  edtBrushInterval.Enabled               := updwnBrushInterval.Enabled;
  lblBrushInterval.Enabled               := updwnBrushInterval.Enabled;
  chckbxColorAndSample.Visible           := (FBrushTool in [btCloneStamp, btJetGunBrush]);
  spdbtnBrushOpenPatternSelector.Enabled := (FBrushTool = btPatternStamp);

  if FBrushTool = btJetGunBrush then
  begin
    updwnBrushInterval.Position := FJetGun.Interval;
  end
  else if FBrushTool = btAirBrush then
  begin
    updwnBrushInterval.Position := FAirBrush.Interval;
  end
  else if FBrushTool = btBlurSharpenBrush then
  begin
    updwnBrushInterval.Position := FBlurSharpenTimerInterval;
  end
  else
  begin
    updwnBrushInterval.Position := FBrushInterval;
  end;

  if FBrushTool = btJetGunBrush then
  begin
    chckbxColorAndSample.Caption := 'Get Random Color';
    chckbxColorAndSample.Checked := FIsRandomColor;
  end
  else
  if FBrushTool = btCloneStamp then
  begin
    chckbxColorAndSample.Caption := 'Use All Layers';
    chckbxColorAndSample.Checked := FIsUseAllLayers;
  end;
end;

// Update components of marquee page.
procedure TfrmMain.UpdateMarqueeOptions;
var
  LHintStr   : string;
  LAvailable : Boolean;
begin
  case FMarqueeTool of
    mtMagicWand:
      begin
        LHintStr                     := 'Set range when sampling color';
        lblToleranceSides.Caption    := 'Tolerance:';
        updwnToleranceSides.Min      := 0;
        updwnToleranceSides.Max      := 100;
        updwnToleranceSides.Position := FMagicWandTolerance;
      end;

    mtRegularPolygon:
      begin
        LHintStr                     := 'Set number of sides';
        lblToleranceSides.Caption    := 'Sides:';
        updwnToleranceSides.Min      := 3;
        updwnToleranceSides.Max      := 100;
        updwnToleranceSides.Position := FRPMSides;
      end;

    mtRoundRectangular:
      begin
        LHintStr                     := 'Set radius of rounded corners';
        lblToleranceSides.Caption    := 'Radius:';
        updwnToleranceSides.Min      := 0;
        updwnToleranceSides.Max      := 1000;
        updwnToleranceSides.Position := FRRMCornerRadius;
      end;
  end;

  lblToleranceSides.Hint   := LHintStr;
  edtToleranceSides.Hint   := LHintStr;
  updwnToleranceSides.Hint := LHintStr;

  LAvailable := (FMarqueeTool in [mtMagicWand, mtRegularPolygon, mtRoundRectangular]);

  lblToleranceSides.Enabled := LAvailable;
  edtToleranceSides.Enabled := LAvailable;

  if LAvailable then
  begin
    edtToleranceSides.Color := clWindow;
  end
  else
  begin
    edtToleranceSides.Color := clBtnFace;
  end;

  updwnToleranceSides.Enabled            := LAvailable;
  chckbxUseAllLayers.Enabled             := (FMarqueeTool in [mtMagicWand, mtMagneticLasso]);
  chckbxMagneticLassoInteractive.Enabled := (FMarqueeTool = mtMagneticLasso);
end;

// Update components of crop page.
procedure TfrmMain.UpdateCropOptions;
var
  LCropCreated: Boolean;
begin
  LCropCreated := (ActiveChildForm <> nil) and (ActiveChildForm.Crop <> nil);

  lblCroppedShieldColor.Enabled       := LCropCreated;
  shpCroppedShieldColor.Enabled       := LCropCreated;
  lblCroppedShieldOpacity.Enabled     := LCropCreated;
  lblShieldOpacityPercentSign.Enabled := LCropCreated;
  edtCroppedShieldOpacity.Enabled     := LCropCreated;
  updwnCroppedShieldOpacity.Enabled   := LCropCreated;
  chckbxShieldCroppedArea.Enabled     := LCropCreated;
  chckbxResizeCrop.Enabled            := LCropCreated;
  lblCropWidth.Enabled                := LCropCreated;
  lblCropHeight.Enabled               := LCropCreated;
  edtCropWidth.Enabled                := LCropCreated;
  edtCropHeight.Enabled               := LCropCreated;

  if LCropCreated then
  begin
    edtCropWidth.Color  := clWindow;
    edtCropHeight.Color := clWindow;
  end
  else
  begin
    edtCropWidth.Color  := clBtnFace;
    edtCropHeight.Color := clBtnFace;
  end;

  // display the crop shield group
  if LCropCreated and chckbxShieldCroppedArea.Checked then
  begin
    pnlCropShield.Top     := pnlCropOptions.Top + pnlCropOptions.Height;
    pnlCropShield.Visible := True;
  end
  else
  begin
    pnlCropShield.Visible := False;
  end;

  // display the resize crop group
  if LCropCreated and chckbxResizeCrop.Checked then
  begin
    if pnlCropShield.Visible then
    begin
      pnlResizeCrop.Top := pnlCropShield.Top + pnlCropShield.Height;
    end
    else
    begin
      pnlResizeCrop.Top := pnlCropOptions.Top + pnlCropOptions.Height;
    end;

    pnlResizeCrop.Visible := True;
  end
  else
  begin
    pnlResizeCrop.Visible := False;
  end;
end;

// Update components of paint bucket page.
procedure TfrmMain.UpdatePaintBucketOptions;
var
  LEnabled: Boolean;
begin
  LEnabled                         := (FPaintBucketFillSource = pbfsPattern);
  lblPaintBucketPattern.Enabled    := LEnabled;
  pnlFillPatternHolder.Enabled     := LEnabled;
  imgPatternForPaintBucket.Enabled := LEnabled;
  spdbtnOpenPatternForFill.Enabled := LEnabled;
end;

// Update components of eraser page.
procedure TfrmMain.UpdateEraserOptions;
begin
  case FEraserTool of
    etEraser:
      begin
        lblEraserModeLimit.Caption     := 'Mode:';
        lblEraserModeLimit.Hint        := 'Erasing Mode';
        cmbbxEraserModeLimit.Items     := ErasingModeList;
        cmbbxEraserModeLimit.ItemIndex := GetErasingModeIndex(FErasingMode);
        cmbbxEraserModeLimit.Hint      := 'Erasing Mode';

        case FErasingMode of
          emPaintBrush:
            begin
              lblEraserOpacityPressure.Caption    := 'Opacity:';
              lblEraserOpacityPressure.Hint       := 'Set eraser opacity';
              edtEraserOpacityPressure.Hint       := 'Set eraser opacity';
              updwnEraserOpacityPressure.Hint     := 'Set eraser opacity';
              updwnEraserOpacityPressure.Position := FErasingOpacity;
              updwnEraserInterval.Position        := FErasingInterval;
            end;

          emAirBrush:
            begin
              lblEraserOpacityPressure.Caption    := 'Pressure:';
              lblEraserOpacityPressure.Hint       := 'Set eraser pressure';
              edtEraserOpacityPressure.Hint       := 'Set eraser pressure';
              updwnEraserOpacityPressure.Hint     := 'Set eraser pressure';
              updwnEraserOpacityPressure.Position := FAirErasingPressure;
              updwnEraserInterval.Position        := FAirErasingInterval;
            end;
        end;
      end;

    etBackgroundEraser:
      begin
        lblEraserModeLimit.Caption     := 'Limit:';
        lblEraserModeLimit.Hint        := 'How far to let the erasing spread';
        cmbbxEraserModeLimit.Items     := ErasingLimitList;
        cmbbxEraserModeLimit.ItemIndex := GetErasingLimitIndex(FBackgroundEraserLimit);
        cmbbxEraserModeLimit.Hint      := 'How far to let the erasing spread';

        case FBackgroundEraserLimit of
          belDiscontiguous:
            begin
              updwnEraserInterval.Position := FAirErasingInterval;
            end;

          belContiguous:
            begin
              updwnEraserInterval.Position := FErasingInterval;
            end;
        end;
      end;

    etMagicEraser:
      begin
        lblEraserOpacityPressure.Caption    := 'Opacity:';
        lblEraserOpacityPressure.Hint       := 'Set eraser opacity';
        edtEraserOpacityPressure.Hint       := 'Set eraser opacity';
        updwnEraserOpacityPressure.Hint     := 'Set eraser opacity';
        updwnEraserOpacityPressure.Position := FErasingOpacity;
      end;
  end;

  lblEraserPaintBrush.Enabled      := FEraserTool <> etMagicEraser;
  pnlEraserBrushHolder.Enabled     := lblEraserPaintBrush.Enabled;
  imgEraserPaintingBrush.Enabled   := lblEraserPaintBrush.Enabled;
  spdbtnSelectEraserStroke.Enabled := lblEraserPaintBrush.Enabled;

  lblEraserModeLimit.Enabled   := lblEraserPaintBrush.Enabled;
  cmbbxEraserModeLimit.Enabled := lblEraserPaintBrush.Enabled;

  if cmbbxEraserModeLimit.Enabled then
  begin
    cmbbxEraserModeLimit.Color := clWindow;
  end
  else
  begin
    cmbbxEraserModeLimit.Color := clBtnFace;
  end;

  lblEraserSampling.Enabled   := (FEraserTool = etBackgroundEraser);
  cmbbxEraserSampling.Enabled := lblEraserSampling.Enabled;

  if cmbbxEraserSampling.Enabled then
  begin
    cmbbxEraserSampling.Color := clWindow;
  end
  else
  begin
    cmbbxEraserSampling.Color := clBtnFace;
  end;

  lblEraserOpacityPressure.Enabled    := (FEraserTool <> etBackgroundEraser);
  lblEraserOpacityPercentSign.Enabled := lblEraserOpacityPressure.Enabled;
  updwnEraserOpacityPressure.Enabled  := lblEraserOpacityPressure.Enabled;
  edtEraserOpacityPressure.Enabled    := lblEraserOpacityPressure.Enabled;

  if edtEraserOpacityPressure.Enabled then
  begin
    edtEraserOpacityPressure.Color := clWindow;
  end
  else
  begin
    edtEraserOpacityPressure.Color := clBtnFace;
  end;

  lblEraserInterval.Enabled   := lblEraserPaintBrush.Enabled;
  updwnEraserInterval.Enabled := lblEraserPaintBrush.Enabled;
  edtEraserInterval.Enabled   := lblEraserPaintBrush.Enabled;

  if edtEraserInterval.Enabled then
  begin
    edtEraserInterval.Color := clWindow;
  end
  else
  begin
    edtEraserInterval.Color := clBtnFace;
  end;

  lblEraserTolerance.Enabled            := (FEraserTool <> etEraser);
  lblEraserTolerancePercentSign.Enabled := lblEraserTolerance.Enabled;
  edtEraserTolerance.Enabled            := lblEraserTolerance.Enabled;
  updwnEraserTolerance.Enabled          := lblEraserTolerance.Enabled;

  if edtEraserTolerance.Enabled then
  begin
    edtEraserTolerance.Color := clWindow;
  end
  else
  begin
    edtEraserTolerance.Color := clBtnFace;
  end;

  spdbtnEraserDynamics.Enabled := lblEraserInterval.Enabled;
end;

// Update components of measure page.
procedure TfrmMain.UpdateMeasureOptions;

  procedure ClearMeasureInfo;
  const
    FLOAT_DIMENSION: Extended = 0.00;
    ANGLE          : Extended = 0.0;
  var
    LMeasureUnit: TgmMeasureUnit;
  begin
    LMeasureUnit := TgmMeasureUnit(cmbbxMeasureUnit.ItemIndex);

    case LMeasureUnit of
      muPixel:
        begin
          lblMStartXValue.Caption := IntToStr(0);
          lblMStartYValue.Caption := IntToStr(0);
          lblMWidthValue.Caption  := IntToStr(0);
          lblMHeightValue.Caption := IntToStr(0);
          lblMAngleValue.Caption  := Format('%.1f', [Angle]);
          lblMD1Value.Caption     := IntToStr(0);
          lblMD2Value.Caption     := IntToStr(0);
        end;

      muInch, muCM:
        begin
          lblMStartXValue.Caption := Format('%.2f', [FLOAT_DIMENSION]);
          lblMStartYValue.Caption := Format('%.2f', [FLOAT_DIMENSION]);
          lblMWidthValue.Caption  := Format('%.2f', [FLOAT_DIMENSION]);
          lblMHeightValue.Caption := Format('%.2f', [FLOAT_DIMENSION]);
          lblMAngleValue.Caption  := Format('%.1f', [ANGLE]);
          lblMD1Value.Caption     := Format('%.2f', [FLOAT_DIMENSION]);
          lblMD2Value.Caption     := Format('%.2f', [FLOAT_DIMENSION]);
        end;
    end;

    lblMeasureUnit.Enabled      := False;
    cmbbxMeasureUnit.Enabled    := False;
    cmbbxMeasureUnit.Color      := clBtnFace;
    btnClearMeasureInfo.Enabled := False;
  end;

begin
  if ActiveChildForm <> nil then
  begin
    if Assigned(ActiveChildForm.MeasureLine) then
    begin
      ActiveChildForm.ShowMeasureResult;
      lblMeasureUnit.Enabled      := True;
      cmbbxMeasureUnit.Enabled    := True;
      cmbbxMeasureUnit.Color      := clWindow;
      btnClearMeasureInfo.Enabled := True;
    end
    else
    begin
      ClearMeasureInfo;
    end;
  end
  else
  begin
    ClearMeasureInfo;
  end;
end;

// Update componets of shape page.
procedure TfrmMain.UpdateShapeOptions;
var
  LAvailable : Boolean;
begin
  case FShapeRegionTool of
    srtRoundedRect:
      begin
        lblShapeToolRSW.Caption    := 'Radius:';
        lblShapeToolRSW.Hint       := 'Set radius of rounded corners';
        edtShapeToolRSW.Hint       := 'Set radius of rounded corners';
        updwnShapeToolRSW.Hint     := 'Set radius of rounded corners';
        updwnShapeToolRSW.Min      := 0;
        updwnShapeToolRSW.Max      := 1000;
        updwnShapeToolRSW.Position := FShapeCornerRadius;
      end;

    srtPolygon:
      begin
        lblShapeToolRSW.Caption    := 'Sides:';
        lblShapeToolRSW.Hint       := 'Set number of sides';
        edtShapeToolRSW.Hint       := 'Set number of sides';
        updwnShapeToolRSW.Hint     := 'Set number of sides';
        updwnShapeToolRSW.Min      := 3;
        updwnShapeToolRSW.Max      := 100;
        updwnShapeToolRSW.Position := FShapePolygonSides;
      end;

    srtLine:
      begin
        lblShapeToolRSW.Caption    := 'Weight:';
        lblShapeToolRSW.Hint       := 'Set line weight';
        edtShapeToolRSW.Hint       := 'Set line weight';
        updwnShapeToolRSW.Hint     := 'Set line weight';
        updwnShapeToolRSW.Min      := 1;
        updwnShapeToolRSW.Max      := 1000;
        updwnShapeToolRSW.Position := FLineWeight;
      end;
  end;

  LAvailable := FShapeRegionTool in [srtRoundedRect, srtPolygon, srtLine];

  lblShapeToolRSW.Enabled   := LAvailable;
  updwnShapeToolRSW.Enabled := LAvailable;
  edtShapeToolRSW.Enabled   := LAvailable;

  if edtShapeToolRSW.Enabled then
  begin
    edtShapeToolRSW.Color := clWindow;
  end
  else
  begin
    edtShapeToolRSW.Color := clBtnFace;
  end;
end;

// Update componets of text page.
procedure TfrmMain.UpdateTextOptions;
var
  LEnabled: Boolean;
begin
  LEnabled := frmRichTextEditor.Visible;

  lblFontFamily.Enabled   := LEnabled;
  cmbbxFontFamily.Enabled := LEnabled;

  if cmbbxFontFamily.Enabled then
  begin
    cmbbxFontFamily.Color := clWindow;
  end
  else
  begin
    cmbbxFontFamily.Color := clBtnFace;
  end;

  lblFontColor.Enabled  := LEnabled;
  shpFontColor.Enabled  := LEnabled;
  lblFontSize.Enabled   := LEnabled;
  cmbbxFontSize.Enabled := LEnabled;

  if cmbbxFontSize.Enabled then
  begin
    cmbbxFontSize.Color := clWindow;
  end
  else
  begin
    cmbbxFontSize.Color := clBtnFace;
  end;

  spdbtnLeftAlignText.Enabled     := LEnabled;
  spdbtnCenterText.Enabled        := LEnabled;
  spdbtnRightAlignText.Enabled    := LEnabled;
  tlbtnBold.Enabled               := LEnabled;
  tlbtnItalic.Enabled             := LEnabled;
  tlbtnUnderline.Enabled          := LEnabled;
  spdbtnSelectAllRichText.Enabled := LEnabled;
  spdbtnClearRichText.Enabled     := LEnabled;

  if LEnabled then
  begin
    ChangeIndexByFontName(frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Name);
    ChangeIndexByFontSize(frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Size);

    shpFontColor.Brush.Color   := frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Color;
    tlbtnBold.Down             := (fsBold in frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Style);
    tlbtnItalic.Down           := (fsItalic in frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Style);
    tlbtnUnderline.Down        := (fsUnderline in frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Style);
    spdbtnLeftAlignText.Down   := (frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment = taLeftJustify);
    spdbtnCenterText.Down      := (frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment = taCenter);
    spdbtnRightAlignText.Down  := (frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment = taRightJustify);
  end;
end;

procedure TfrmMain.UpdateToolsOptions;
begin
  case FMainTool of
    gmtStandard:
      begin
        UpdateStandardOptions;
      end;

    gmtBrush:
      begin
        UpdateBrushOptions;
      end;

    gmtMarquee:
      begin
        UpdateMarqueeOptions;
      end;

    gmtCrop:
      begin
        UpdateCropOptions;
      end;

    gmtMeasure:
      begin
        UpdateMeasureOptions;
      end;

    gmtPaintBucket:
      begin
        UpdatePaintBucketOptions;
      end;

    gmtEraser:
      begin
        UpdateEraserOptions;
      end;

    gmtShape:
      begin
        UpdateShapeOptions;
      end;

    gmtTextTool:
      begin
        UpdateTextOptions;
      end;
  end;

  if ActiveChildForm <> nil then
  begin
    spdbtnZoomOut.Enabled  := True;
    spdbtnZoomIn.Enabled   := True;
    ggbrZoomSlider.Enabled := True;
  end
  else
  begin
    spdbtnZoomOut.Enabled  := False;
    spdbtnZoomIn.Enabled   := False;
    ggbrZoomSlider.Enabled := False;
  end;
end;

// creating child form and open image within it
function TfrmMain.OpenImageInChildForm(const AFileName: string): Boolean;
var
  LOpenedBmp : TBitmap32;
  LRect      : TRect;
  LLayer     : TgmCustomLayer;
begin
  Result := False;

  if AFileName <> '' then
  begin
    if FileExists(AFileName) then
    begin
      if LowerCase(ExtractFileExt(AFileName)) = '.gmd' then
      begin
        Result := LoadGMDInChildForm(AFileName);
      end
      else
      begin
        // first, check if we could open the image
        LOpenedBmp := LoadGraphicsFile(AFileName);

        if not Assigned(LOpenedBmp) then
        begin
          MessageDlg('Cannot load picture ''' + ExtractFileName(AFileName) + '''',
                     mtError, [mbOK], 0);
        end
        else
        begin
          FChildFormIsCreating := True;
          try
            ActiveChildForm := TfrmChild.Create(Self);
          finally
            FChildFormIsCreating := False;
          end;

          ActiveChildForm.FileName := AFileName;

          ActiveChildForm.HistoryBitmap.Assign(LOpenedBmp);
          ActiveChildForm.HistoryBitmap.DrawMode := dmBlend;
          FreeAndNil(LOpenedBmp);

          with ActiveChildForm do
          begin
            // set background size before create background layer
            imgWorkArea.Bitmap.SetSize(HistoryBitmap.Width, HistoryBitmap.Height);
            imgWorkArea.Bitmap.Clear($00000000);

            // draw checkerboard pattern
            CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
            DrawCheckerboardPattern(CheckerboardBmp, CheckerboardBmp.ClipRect);

            LLayer := TgmNormalLayer.Create(LayerList,
              HistoryBitmap.Width, HistoryBitmap.Height, $00000000, True);

            LLayer.LayerBitmap.Assign(HistoryBitmap);
            LLayer.UpdateLayerThumbnail();
            LayerList.Add(LLayer);
            SetCallbacksForLayersInList();

            // update the view
            LayerList.SelectedLayer.Changed();

            // channels ...
            LRect             := imgWorkArea.GetBitmapRect;
            LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
            LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

            ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);
            ChannelManager.ChannelLayerLocation := FloatRect(LRect);

            // Add a snapshot (just the opened image) to the Command Manager.
            CommandManager.AddSnapshot( LayerList.CombineResult,
                                        ExtractFileName(FileName) );

            CommandManager.SelectSnapshot(0);
          end;

          if ActiveChildForm.WindowState = wsNormal then
          begin
            // set client size of the child form
            ActiveChildForm.ClientWidth  := LLayer.LayerBitmap.Width  + ActiveChildForm.imgWorkArea.ScrollBars.Size;
            ActiveChildForm.ClientHeight := LLayer.LayerBitmap.Height + ActiveChildForm.imgWorkArea.ScrollBars.Size;

            if FGhostModeEnabled and pnlToolOptions.Visible then
            begin
              if ActiveChildForm.Left < pnlToolOptions.Width then
              begin
                ActiveChildForm.Left := pnlToolOptions.Width;
              end;
            end;
          end;

          stsbrMain.Panels[0].Text := GetBitmapDimensionString(ActiveChildForm.HistoryBitmap);
          ActiveChildForm.RefreshCaption();
          ActiveChildForm.SetupOnChildFormActivate();

          Result := True;
        end;
      end;
    end
    else
    begin
      MessageDlg('The file is not existed.', mtError, [mbOK], 0);
    end;
  end;
end;

// creating child form and load '*.gmd' file
function TfrmMain.LoadGMDInChildForm(const AFileName: string): Boolean;
var
  LGMDManager    : TgmGMDManager;
  LLastChildForm : TfrmChild; // pointer to last active child form (if any)
begin
  Result         := False;
  LLastChildForm := nil;

  // load data from file
  LGMDManager := TgmGMDManager.Create();
  try
    // check file validity
    if LGMDManager.CheckFileValidity(AFileName) = False then
    begin
      MessageDlg(LGMDManager.OuputMsg, mtError, [mbOK], 0);
    end
    else
    begin
      if ActiveChildForm <> nil then
      begin
        LLastChildForm := ActiveChildForm;
      end;

      FChildFormIsCreating := True;
      try
        ActiveChildForm := TfrmChild.Create(Self);
      finally
        FChildFormIsCreating := False;
      end;

      ActiveChildForm.FileName := AFileName;

      // link pointers to the gmd manager
      LGMDManager.LayerList      := ActiveChildForm.LayerList;
      LGMDManager.ChannelManager := ActiveChildForm.ChannelManager;
      LGMDManager.PathList       := ActiveChildForm.PathList;
      LGMDManager.TextEditor     := frmRichTextEditor.rchedtRichTextEditor;

      // set events for the gmd manager
      LGMDManager.OnAfterFileLoaded := Self.AfterGMDFileLoaded;

      if LGMDManager.LoadFromFile(AFileName) then
      begin
        if ActiveChildForm.WindowState = wsNormal then
        begin
          // set client size of the child form
          ActiveChildForm.ClientWidth :=
            ActiveChildForm.imgWorkArea.Bitmap.Width +
            ActiveChildForm.imgWorkArea.ScrollBars.Size;

          ActiveChildForm.ClientHeight :=
            ActiveChildForm.imgWorkArea.Bitmap.Height +
            ActiveChildForm.imgWorkArea.ScrollBars.Size;

          if FGhostModeEnabled and pnlToolOptions.Visible then
          begin
            if ActiveChildForm.Left < pnlToolOptions.Width then
            begin
              ActiveChildForm.Left := pnlToolOptions.Width;
            end;
          end;
        end;

        stsbrMain.Panels[0].Text := GetBitmapDimensionString(ActiveChildForm.HistoryBitmap);
        ActiveChildForm.RefreshCaption();
        ActiveChildForm.SetupOnChildFormActivate();

        Result := True;
      end
      else // cannot load in the specified .gmd file from disk
      begin
        if Assigned(ActiveChildForm) then
        begin
          ActiveChildForm.Close();
          FreeAndNil(ActiveChildForm);
        end;

        if Assigned(LLastChildForm) then
        begin
          ActiveChildForm := LLastChildForm;
          ActiveChildForm.Show();
        end;
      end;
    end;

  finally
    LGMDManager.Free();
  end;
end;

{ When trying to open an opened file, then we just show the form with the
  opened file to top of the screen. }
function TfrmMain.ShowOpenedImageTop(const CheckFileName: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  if CheckFileName <> '' then
  begin
    if MDIChildCount > 0 then
    begin
      for i := 0 to MDIChildCount - 1 do
      begin
        if CheckFileName = TfrmChild(MDIChildren[i]).FileName then
        begin
          Result := True;
          TfrmChild(MDIChildren[i]).Show;
          Break;
        end;
      end;
    end;
  end;
end;

{ Open Recent Methods }

procedure TfrmMain.LoadOpenRecentPathToList(AStringList: TStringList);
var
  IniFile                     : TIniFile;
  OpenRecentCount, i          : Integer;
  Ident, FileName, IniFileName: string;
begin
  AStringList.Clear;
  IniFileName := ChangeFileExt(ParamStr(0),'.ini');

  if FileExists(IniFileName) then
  begin
    IniFile := TIniFile.Create(IniFileName);
    try
      // loading the number of OpenRecent file paths
      OpenRecentCount := StrToInt( IniFile.ReadString(SECTION_OPEN_RECENT, IDENT_OPEN_RECENT_COUNT, '0') );

      if OpenRecentCount > 0 then
      begin
        for i := 0 to OpenRecentCount - 1 do
        begin
          Ident    := IDENT_OPEN_RECENT_PATH + IntToStr(i);
          FileName := IniFile.ReadString(SECTION_OPEN_RECENT, Ident, '');
          AStringList.Add(FileName);
        end;
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TfrmMain.AddFilePathToList(AStringList: TStringList;
  const AFilePath: string);
var
  InList: Boolean;
  i     : Integer;
begin
  if AStringList.Count > 0 then
  begin
    InList := False;

    // check for whether the file path is already in the list
    for i := 0 to AStringList.Count - 1 do
    begin
      if AFilePath = AStringList.Strings[i] then
      begin
        InList := True;
        Break;
      end;
    end;

    // if the file path is already in the list, then move it to the first postion of the list
    if InList then
    begin
      AStringList.Move(i, 0);
    end
    else
    begin
      // MAX_OPEN_RECENT_COUNT was declared in gmConstants.pas.
      if AStringList.Count = MAX_OPEN_RECENT_COUNT then
      begin
        AStringList.Delete(AStringList.Count - 1);
      end;

      AStringList.Insert(0, AFilePath);
    end;
  end
  else
  begin
    // if the file path list is empty, add the file path to the list immediately
    AStringList.Add(AFilePath);
  end;
end;

procedure TfrmMain.UpdateOpenRecentMenuItem;
var
  MenuItem: TMenuItem;
  FileName: string;
  i       : Integer;
begin
  if FOpenRecentPathList.Count > 0 then
  begin
    { Make sure the number of items of the menu list is same as the number of
      items of file paths list. }
    if FOpenRecentMenuList.Count < FOpenRecentPathList.Count then
    begin
      for i := 1 to FOpenRecentPathList.Count - FOpenRecentMenuList.Count do
      begin
        MenuItem         := TMenuItem.Create(Self);   // create menu item
        MenuItem.OnClick := OpenRecentMenuItemClick;  // connect click event to the menu item

        // insert the new menu item under the OpenRecent menu item
        mnitmOpenRecent.Insert(0, MenuItem);
        FOpenRecentMenuList.Insert(0, MenuItem);  // insert the new menu item to the list
      end;
    end;

    // change the caption of the menus with Open Recent file names, respectively
    if FOpenRecentMenuList.Count = FOpenRecentPathList.Count then
    begin
      for i := 0 to FOpenRecentPathList.Count - 1 do
      begin
        FileName := ExtractFileName(FOpenRecentPathList.Strings[i]);
        TMenuItem(FOpenRecentMenuList.Items[i]).Caption := FileName;
      end;
    end;
  end;
end;

// connect this OnClick event to the OpenRecent menus
procedure TfrmMain.OpenRecentMenuItemClick(Sender: TObject);
var
  i         : Integer;
  LFileName : string;
begin
  if FOpenRecentMenuList.Count > 0 then
  begin
    for i := 0 to (FOpenRecentMenuList.Count - 1) do
    begin
      // If the mouse is clicked on any of the menu items, then open the
      // corresponding file.
      if Sender = TMenuItem(FOpenRecentMenuList.Items[i]) then
      begin
        LFileName := FOpenRecentPathList.Strings[i];
        try
          if not ShowOpenedImageTop(LFileName) then
          begin
            OpenImageInChildForm(LFileName);
          end;

          // bring the current opened file menu front
          FOpenRecentPathList.Move(i, 0);
          UpdateOpenRecentMenuItem();
        except
          MessageDlg('Can not open the file!', mtError, [mbOK], 0);
        end;
        Break;
      end;
    end;
  end;
end;

procedure TfrmMain.WriteOpenRecentInfoToIniFile;
var
  Ident: string;
  i    : Integer;
begin
  if FOpenRecentPathList.Count > 0 then
  begin
    WriteInfoToIniFile( SECTION_OPEN_RECENT, IDENT_OPEN_RECENT_COUNT, IntToStr(FOpenRecentPathList.Count) );

    for i := 0 to FOpenRecentPathList.Count - 1 do
    begin
      Ident := IDENT_OPEN_RECENT_PATH + IntToStr(i);
      WriteInfoToINIFile(SECTION_OPEN_RECENT, Ident, FOpenRecentPathList.Strings[i]);
    end;
  end;
end;

procedure TfrmMain.DuplicateSelection;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  if Assigned(FSelectionClipboard) then
  begin
    FreeAndNil(FSelectionClipboard);
  end;

  with ActiveChildForm do
  begin
    if Assigned(Selection) then
    begin
      FSelectionClipboard := TgmSelection.Create(imgWorkArea);
      FSelectionClipboard.AssignSelectionData(Selection);

      // we need to adjust the forground of the clipboard selection only on
      // the following situation
      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        AdjustBitmapChannels32(FSelectionClipboard.CutOriginal,
                               ChannelManager.SelectedColorChannels);

        FSelectionClipboard.GetForeground();
      end;
    end;
  end;
end;

procedure TfrmMain.ReloadFilters;
begin
  Screen.Cursor := crHourGlass;
  try
    // delete all filters info
    FreeAndNil(FGMPluginInfoList);
    FreeAndNil(FFilterCategoryMenuList);

    FLastFilterMenuItem     := nil;
    mnitmLastFilter.Caption := 'Last Filter';

    // reload filters
    FGMPluginInfoList       := TgmPluginInfoList.Create();
    FFilterCategoryMenuList := TgmCategoryMenuItemList.Create();

    InitFilterMenuItem();
  finally
    Screen.Cursor := crDefault;
  end;
end;

// the return value of the function indicates whether the selection
// is pasted on a new layer
function TfrmMain.PasteSelection: Boolean;
var
  LLayer      : TgmCustomLayer;
  LLayerIndex : Integer;
begin
  Result := False;

  if Assigned(FSelectionClipboard) then
  begin
    with ActiveChildForm do
    begin
      if (ChannelManager.CurrentChannelType = ctColorChannel) and
         (ChannelManager.ColorChannelList.SelectedChannelCount > 2) then
      begin
        // create a new layer ...
        LLayer      := CreateNormalLayer();
        LLayerIndex := LayerList.SelectedIndex + 1;

        LayerList.Insert(LLayerIndex, LLayer);

        Result := True;
      end;

      if Selection = nil then
      begin
        CreateNewSelection();
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
        Selection.AssignSelectionData(FSelectionClipboard);

        // The CenterAlignSelection() method culculates the position of the
        // selection according to the background size of the selection, so we
        // need to specify the background size beforehand.

        Selection.Background.SetSize(LayerList.SelectedLayer.LayerBitmap.Width,
                                     LayerList.SelectedLayer.LayerBitmap.Height);

        Selection.CenterAlignSelection();

        if ChannelManager.CurrentChannelType in [ctAlphaChannel,
                                                 ctQuickMaskChannel,
                                                 ctLayerMaskChannel] then
        begin
          Desaturate32(Selection.CutOriginal);
          Selection.GetForeground();
        end;

        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                Selection.SourceBitmap.Assign(
                  ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                Selection.Background.Assign(
                  ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

                Selection.ShowSelection(
                  ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                  [csGrayscale]);

                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
                ChannelManager.SelectedAlphaChannel.UpdateChannelThumbnail();
              end;
            end;

          ctQuickMaskChannel:
            begin
              Selection.SourceBitmap.Assign(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

              Selection.Background.Assign(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

              Selection.ShowSelection(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                [csGrayscale]);

              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
              ChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
            end;

          ctLayerMaskChannel:
            begin
              Selection.SourceBitmap.Assign(LayerList.SelectedLayer.MaskBitmap);
              Selection.Background.Assign(LayerList.SelectedLayer.MaskBitmap);

              Selection.ShowSelection(LayerList.SelectedLayer.MaskBitmap,
                                      [csGrayscale]);

              LayerList.SelectedLayer.UpdateMaskThumbnail();

              // update the mask channel preview layer
              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);

                ChannelManager.LayerMaskChannel.UpdateChannelThumbnail();
              end;

              // update the view
              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              Selection.SourceBitmap.Assign(LayerList.SelectedLayer.LayerBitmap);
              Selection.Background.Assign(LayerList.SelectedLayer.LayerBitmap);

              Selection.ShowSelection(LayerList.SelectedLayer.LayerBitmap,
                                      ChannelManager.SelectedColorChannels);

              // update the view
              LayerList.SelectedLayer.Changed();
              LayerList.SelectedLayer.UpdateLayerThumbnail();
            end;
        end;

        Selection.IsAnimated := True;
      end;
    end;
  end;
end;

// This function will loops through each child form and get all the
// file names in that form that has the same size image with the destination
// image which was in the Active Child Form. The function will return all
// the file names it found as result. It will return nil if no such image is
// matched.
function TfrmMain.GetFileNamesWithSameSizeImage: TStringList;
var
  i                      : Integer;
  LSrcImageW, LSrcImageH : Integer;
  LDstImageW, LDstImageH : Integer;
  LTempForm              : TfrmChild;
begin
  Result := nil;

  if Assigned(ActiveChildForm) then
  begin
    Result := TStringList.Create();
    Result.Add( ExtractFileName(ActiveChildForm.FileName) );

    LDstImageW := ActiveChildForm.imgWorkArea.Bitmap.Width;
    LDstImageH := ActiveChildForm.imgWorkArea.Bitmap.Height;

    for i := 0 to (MDIChildCount - 1) do
    begin
      LTempForm := TfrmChild(MDIChildren[i]);

      if LTempForm <> ActiveChildForm then
      begin
        LSrcImageW := LTempForm.imgWorkArea.Bitmap.Width;
        LSrcImageH := LTempForm.imgWorkArea.Bitmap.Height;

        if (LSrcImageW = LDstImageW) and (LSrcImageH = LDstImageH) then
        begin
          Result.Add( ExtractFileName(TfrmChild(MDIChildren[i]).FileName) );
        end;
      end;
    end;

    TStringList(Result).Sort();
  end;
end;

function TfrmMain.GetChildFormPointerByFileName(const AFileName: string): TfrmChild;
var
  i            : Integer;
  LTempForm    : TfrmChild;
  LSrcFileName : string;
begin
  Result := nil;

  if MDIChildCount > 0 then
  begin
    for i := 0 to (MDIChildCount - 1) do
    begin
      LTempForm    := TfrmChild(MDIChildren[i]);
      LSrcFileName := ExtractFileName(LTempForm.FileName);

      if AFileName = LSrcFileName then
      begin
        Result := LTempForm;
        Break;
      end;
    end;
  end;
end;

{ Brush Tools }

function TfrmMain.GetBrushName: string;
begin
  Result := '';

  case FBrushTool of
    btAirBrush:
      begin
        if Assigned(FAirBrush) then
        begin
          Result := FAirBrush.Name;
        end;
      end;

    btJetGunBrush:
      begin
        if Assigned(FJetGun) then
        begin
          Result := FJetGun.Name;
        end;
      end;

  else
    if Assigned(FGMBrush) then
    begin
      Result := FGMBrush.Name;
    end;
  end;
end;

{ Text Tools }
procedure TfrmMain.ChangeIndexByFontName(const AFontName: string);
begin
  with cmbbxFontFamily do
  begin
    if Items.Count > 0 then
    begin
      ItemIndex := Items.IndexOf(AFontName);
    end;
  end;
end;

procedure TfrmMain.ChangeIndexByFontSize(const AFontSize: Byte);
begin
  with cmbbxFontSize do
  begin
    case AFontSize of
       6: ItemIndex := 0;
       8: ItemIndex := 1;
       9: ItemIndex := 2;
      10: ItemIndex := 3;
      11: ItemIndex := 4;
      12: ItemIndex := 5;
      14: ItemIndex := 6;
      16: ItemIndex := 7;
      18: ItemIndex := 8;
      20: ItemIndex := 9;
      22: ItemIndex := 10;
      24: ItemIndex := 11;
      26: ItemIndex := 12;
      28: ItemIndex := 13;
      30: ItemIndex := 14;
      36: ItemIndex := 15;
      48: ItemIndex := 16;
      60: ItemIndex := 17;
      72: ItemIndex := 18;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMain.FormShow(Sender: TObject);
begin
  imgToolOptionsVisibility.Bitmap.Assign(dmMain.bmp32lstMainTools.Bitmap[IMG_LEFT_TRIPLE_ARROW_INDEX]);
  frmGradientPicker.ShowDrawingToolSelectedGradient();

  if FGhostModeEnabled then
  begin
    FGhostDetect.Resume();
  end;

  frmLayers.Show();
  frmChannels.Show();
  frmPaths.Show();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LPlugInsDir            : string;
  LGhostFadeInterval     : Integer;
  LGhostMaxOpaque        : Integer;
  LToolOptionHeaderColor : TColor;
begin
{ INI }
  InitializeINIFile;

{ Command }
  CreateCommandStorageFolder;  // create a folder to hold the temporary command files

{ Common }
  FOutputGraphicsFormat := ogfBMP;
  FChildFormIsCreating  := False;
  ActiveChildForm       := nil;
  PrevChildForm         := nil;
  FCanChange            := True;
  FCanExecClick         := True;

  // global bitmaps used for various image adjustment dialogs, such as filters
  FBitmapBefore          := TBitmap32.Create();
  FBitmapBefore.DrawMode := dmBlend;
  FBitmapAfter           := TBitmap32.Create();
  FBitmapAfter.DrawMode  := dmBlend;

  // For Open Recent menu.
  FOpenRecentPathList := TStringList.Create;
  FOpenRecentMenuList := TList.Create;

  // save the selection as if it is in clipboard
  FSelectionClipboard := nil;
  FFigureUnits        := auCentimeter;
  FNewBitmapWidth     := 500;
  FNewBitmapHeight    := 500;
  FGlobalForeColor    := clBlack;
  FGlobalBackColor    := clWhite;
  FForeGrayColor      := clBlack;
  FBackGrayColor      := clWhite;

  FMainTool         := gmtStandard;
  FCanChange        := False;
  FGlobalPenStyle   := psSolid;
  FGlobalPenWidth   := 1;
  dmMain.ChangeGlobalBrushStyle(dmMain.pmnitmSolidBrush);

  ntbkToolOptions.PageIndex                       := STANDARD_PAGE_INDEX;
  scrlbxFigureOptions.HorzScrollBar.Position      := 0;
  scrlbxFigureOptions.VertScrollBar.Position      := 0;
  scrlbxBrushOptions.HorzScrollBar.Position       := 0;
  scrlbxBrushOptions.VertScrollBar.Position       := 0;
  scrlbxMarqueeOptions.HorzScrollBar.Position     := 0;
  scrlbxMarqueeOptions.VertScrollBar.Position     := 0;
  scrlbxGradientOptions.HorzScrollBar.Position    := 0;
  scrlbxGradientOptions.VertScrollBar.Position    := 0;
  scrlbxCropOptions.HorzScrollBar.Position        := 0;
  scrlbxCropOptions.VertScrollBar.Position        := 0;
  scrlbxPaintBucketOptions.HorzScrollBar.Position := 0;
  scrlbxPaintBucketOptions.VertScrollBar.Position := 0;
  scrlbxEraserOptions.HorzScrollBar.Position      := 0;
  scrlbxEraserOptions.VertScrollBar.Position      := 0;
  scrlbxPenToolsOptions.HorzScrollBar.Position    := 0;
  scrlbxPenToolsOptions.VertScrollBar.Position    := 0;
  scrlbxMeasureOptions.HorzScrollBar.Position     := 0;
  scrlbxMeasureOptions.VertScrollBar.Position     := 0;
  scrlbxShapeOptions.HorzScrollBar.Position       := 0;
  scrlbxShapeOptions.VertScrollBar.Position       := 0;
  ggbrZoomSlider.ShowHint                         := True;

  // hope this would work on Windows 8/8.1
  LToolOptionHeaderColor        := RGB(224, 128, 144);
  pnlFigureOptHeader.Color      := LToolOptionHeaderColor;
  pnlBrushOptHeader.Color       := LToolOptionHeaderColor;
  pnlSelectionOptHeader.Color   := LToolOptionHeaderColor;
  pnlGradientOptHeader.Color    := LToolOptionHeaderColor;
  pnlCropOptHeader.Color        := LToolOptionHeaderColor;
  pnlCropShieldHeader.Color     := LToolOptionHeaderColor;
  pnlResizeCropHeader.Color     := LToolOptionHeaderColor;
  pnlPaintBucketOptHeader.Color := LToolOptionHeaderColor;
  pnlEraserOptHeader.Color      := LToolOptionHeaderColor;
  pnlPenPathOptHeader.Color     := LToolOptionHeaderColor;
  pnlMeasureOptHeader.Color     := LToolOptionHeaderColor;
  pnlShapeToolOptHeader.Color   := LToolOptionHeaderColor;
  pnlTextToolOptHeader.Color    := LToolOptionHeaderColor;

{ Figure Tools }
  FStandardTool         := gstPencil;
  FLastStandardTool     := gstPencil;
  FStandardCornerRadius := 30;
  FStandardPolygonSides := 3;
  FPencil               := TgmPaintBrush.Create;

  spdbtnStandardTools.Hint := spdbtnPencil.Hint;
  cmbbxPenStyle.ItemIndex  := 0;
  cmbbxPenWidth.ItemIndex  := 0;
  UpdateStandardOptions;

{ Brush Tools }
  FGMBrush                  := TgmPaintBrush.Create;
  FAirBrush                 := TgmAirBrush.Create;
  FJetGun                   := TgmJetGun.Create;
  FBrushTool                := btPaintBrush;
  FBrushBlendMode           := bbmNormal32;
  FBrushOpacity             := 100;
  FBrushIntensity           := 10;
  FBrushRadius              := 40;
  FBrushInterval            := 0;
  FAirPressure              := 10;
  FJetGunPressure           := 100;
  FIsRandomColor            := False;
  FIsUseAllLayers           := False;
  FSmudgePressure           := SMUDGE_DEFAULT_PRESSURE;
  FBlurSharpenPressure      := BLUR_SHARPEN_DEFAULT_PRESSURE;
  FBlurSharpenTimerInterval := BLUR_SHARPEN_DEFAULT_TIMER_INTERVAL;

  // for Dodge/Burn brush
  FDodgeBurnExposure := DODGE_BURN_DEFAULT_EXPOSURE;
  FDodgeBurnMode     := DODGE_BURN_DEFAULT_MODE;

  spdbtnBrushTools.Glyph.Assign(spdbtnPaintBrush.Glyph);
  spdbtnBrushTools.Hint        := spdbtnPaintBrush.Hint;
  updwnBrushRadius.Position    := FBrushRadius;
  updwnBrushInterval.Position  := FBrushInterval;
  chckbxColorAndSample.Visible := False;

  UpdateBrushOptions;

{ Marquee Tools }
  FMarqueeTool        := mtMoveResize;
  FMarqueeMode        := mmNew;
  FRPMSides           := 3;
  FRRMCornerRadius    := 30;
  FMagicWandTolerance := 30;

  spdbtnMarqueeTools.Glyph.Assign(spdbtnSelect.Glyph);
  spdbtnMarqueeTools.Hint := spdbtnSelect.Hint;

{ Gradient Tools }
  FGradientRenderMode   := grmLinear;
  FGradientBlendMode    := bbmNormal32;
  FGradientBlendOpacity := 100;

  spdbtnGradientTools.Hint         := spdbtnLinearGradient.Hint;
  cmbbxGradientBlendMode.Items     := BlendModeList;
  cmbbxGradientBlendMode.ItemIndex := 0;
  updwnGradientOpacity.Position    := 100;

  imgSelectedGradient.Bitmap.DrawMode := dmBlend;
  imgSelectedGradient.Bitmap.SetSize(imgSelectedGradient.Width, imgSelectedGradient.Height);

  with imgSelectedGradient.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;

{ Crop Tools }
  updwnCroppedShieldOpacity.Position := 50;

{ Paint Bucket Tools }
  FPaintBucketFillSource := pbfsForeColor;
  FPaintBucketBlendMode  := bbmNormal32;
  FPaintBucketOpacity    := 100;
  FPaintBucketTolerance  := 15;

  cmbbxPaintBucketFillSource.ItemIndex := 0;
  cmbbxPaintBucketFillMode.Items       := BlendModeList;
  cmbbxPaintBucketFillMode.ItemIndex   := Ord(FPaintBucketBlendMode);
  updwnPaintBucketOpacity.Position     := 100;

{ Eraser Tools }
  FGMEraser              := TgmEraser.Create;
  FEraserTool            := etEraser;
  FErasingMode           := emPaintBrush;
  FBackgroundEraserLimit := belDiscontiguous;
  FEraserSamplingMode    := bsmContiguous;
  FErasingOpacity        := 100;
  FErasingInterval       := 0;
  FAirErasingPressure    := 10;
  FAirErasingInterval    := 100;
  FErasingTolerance      := 15;

  spdbtnEraserTools.Hint              := spdbtnEraser.Hint;
  cmbbxEraserSampling.ItemIndex       := 0;
  updwnEraserOpacityPressure.Position := 100;
  updwnEraserInterval.Position        := 5;
  updwnEraserTolerance.Position       := FErasingTolerance;

  UpdateEraserOptions;

{ Pen Tools }
  FPenTool       := ptPenTool;
  FActivePenTool := ptPenTool;

  spdbtnPenTools.Hint := spdbtnPenTool.Hint;

{ Shape Tools }
  FShapeRegionTool   := srtRectangle;
  FRegionCombineMode := rcmAdd;
  FShapeCornerRadius := 30;
  FShapePolygonSides := 3;
  FLineWeight        := 1;
  FShapeBrushStyle   := bsSolid;

  spdbtnShapeTools.Hint := spdbtnShapeRectangle.Hint;
  dmMain.actnRegionSolidBrush.Execute;

{ Text Tools }
  GetFontNames(cmbbxFontFamily.Items);
  ChangeIndexByFontName('MS Sans Serif');
  UpdateToolsOptions;

{ All Tools }
  ShowStatusInfoOnStatusBar;

{ Filter }
  // create plug-in directory
  LPlugInsDir := ExtractFilePath( ParamStr(0) ) + 'Plug-Ins';

  if not DirectoryExists(LPlugInsDir) then
  begin
    CreateDir(LPlugInsDir);
  end;

  FGMPluginInfoList       := TgmPluginInfoList.Create;
  FFilterCategoryMenuList := TgmCategoryMenuItemList.Create;
  InitFilterMenuItem;

{ Open Recent }
  LoadOpenRecentPathToList(FOpenRecentPathList);
  UpdateOpenRecentMenuItem;

  ActiveControl := stsbrMain;
  Caption       := 'GraphicsMagic Professional '+ VER;

{Ghosts}
  FGhostModeEnabled  := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_MODE_ENABLED, '1')));
  LGhostFadeInterval := StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_FADE_INTERVAL, '50'));
  LGhostFadeInterval := Clamp(LGhostFadeInterval, MIN_GHOST_FADE_INTERVAL, MAX_GHOST_FADE_INTERVAL);
  LGhostMaxOpaque    := StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_MAX_OPAQUE, '255'));
  LGhostMaxOpaque    := Clamp(LGhostMaxOpaque, MAX_GHOST_OPAQUE_LOWER, MAX_GHOST_OPAQUE_UPPER);

  if FGhostModeEnabled then
  begin
    GhostsWakeUp(True);

    with TfrmGhost.Create(nil) do  //pnlToolOptions
    begin
      House := Self;
      Eat(pnlToolOptions, Self.FGhosts);
      timerFade.Interval := LGhostFadeInterval;
      MaxOpaque          := LGhostMaxOpaque;
      Show;
    end;

    with TfrmGhost.Create(nil) do //pnlRightDockArea
    begin
      House := Self;
      Eat(pnlRightDockArea, Self.FGhosts);
      timerFade.Interval := LGhostFadeInterval;
      MaxOpaque          := LGhostMaxOpaque;
      Show;
    end;

    FGhostDetect          := TgmGhostDetect.Create(True);
    FGhostDetect.ExecProc := Self.DetectMousePos;
  end;

  spdbtnGhost.Enabled     := FGhostModeEnabled;
  spdbtnUntouched.Enabled := FGhostModeEnabled;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if MDIChildCount < 1 then
  begin
    stsbrMain.Panels[0].Text := 'Thank you for choose this program!';
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  if MDIChildCount > 0 then
  begin
    for i := (MDIChildCount - 1) downto 0 do
    begin
      MDIChildren[i].Close;
    end;
  end;

  WriteOpenRecentInfoToIniFile;

  if FGhostModeEnabled then
  begin
    FGhostDetect.Terminate;
  end;
end;

procedure TfrmMain.UpdateMenuItemClick(Sender: TObject);
var
  i                  : Integer;
  LFilterMenuEnabled : Boolean;
  LMenuItem          : TMenuItem;
  LPluginInfo        : TgmPluginInfo;
begin
  if Sender = mnhdFilter then
  begin
    if Assigned(ActiveChildForm) then
    begin
      LFilterMenuEnabled := (not ActiveChildForm.ToolsInPendingStatus()) and
                            ( (ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel) or
                              (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) );

      // update menu items of filters
      FGMPluginInfoList.UpdatePluginMenusEnableState(ActiveChildForm.ChannelManager);

      if Assigned(FLastFilterMenuItem) then
      begin
        LPluginInfo := FGMPluginInfoList.GetPluginInfoByMenuItem(FLastFilterMenuItem);

        mnitmLastFilter.Enabled := (LPluginInfo.PluginMenu.Enabled and LFilterMenuEnabled);
      end
      else
      begin
        mnitmLastFilter.Enabled := False;
      end;

      // enable/disable category menu items of filters
      if Assigned(FFilterCategoryMenuList) then
      begin
        if FFilterCategoryMenuList.Count > 0 then
        begin
          for i := 0 to (FFilterCategoryMenuList.Count - 1) do
          begin
            LMenuItem         := TMenuItem(FFilterCategoryMenuList.Items[i]);
            LMenuItem.Enabled := LFilterMenuEnabled;
          end;
        end;
      end;
    end
    else
    begin
      mnitmLastFilter.Enabled := False;

      if Assigned(FFilterCategoryMenuList) then
      begin
        if FFilterCategoryMenuList.Count > 0 then
        begin
          for i := 0 to (FFilterCategoryMenuList.Count - 1) do
          begin
            LMenuItem         := TMenuItem(FFilterCategoryMenuList.Items[i]);
            LMenuItem.Enabled := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
{ Ghost}
  GhostsWakeUp(False);
  if Assigned(FGhostDetect) then
  begin
    FGhostDetect.Free();
  end;

  FOpenRecentPathList.Clear();
  FOpenRecentPathList.Free();
  FOpenRecentMenuList.Clear();
  FOpenRecentMenuList.Free();
  FBitmapBefore.Free();
  FBitmapAfter.Free();
  FSelectionClipboard.Free();

{ Figure Tools }
  FPencil.Free();

{ Brush Tools }
  FGMBrush.Free();
  FAirBrush.Free();
  FJetGun.Free();

{ Eraser Tools }
  FGMEraser.Free();

{ Filter }
  FGMPluginInfoList.Free();
  FFilterCategoryMenuList.Free();
  FLastFilterMenuItem := nil;
end;

procedure TfrmMain.mnitmLastFilterClick(Sender: TObject);
var
  LPluginInfo: TgmPluginInfo;
begin
  if Assigned(ActiveChildForm.SelectionTransformation) or
     Assigned(ActiveChildForm.Crop) or
     frmRichTextEditor.Visible then
  begin
    Exit;
  end;

  if ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel then
  begin
    if not (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      Exit;
    end;
  end;

  if Assigned(FLastFilterMenuItem) then
  begin
    LPluginInfo := FGMPluginInfoList.GetPluginInfoByMenuItem(FLastFilterMenuItem);
    LPluginInfo.UpdatePluginMenuEnableState(ActiveChildForm.ChannelManager.CurrentChannelType);

    if LPluginInfo.PluginMenu.Enabled then
    begin
      ExecuteFilters(FLastFilterMenuItem);
    end;
  end;
end;

procedure TfrmMain.ChangeMainToolClick(Sender: TObject);
var
  LLastMainTool     : TgmMainTool;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  LLastMainTool := FMainTool;  // remember the tool that last used

  // finish the creation of uncompleted figures ...
  if Assigned(ActiveChildForm) then
  begin
    ActiveChildForm.FinishTransformation();
    ActiveChildForm.FinishCurves();
    ActiveChildForm.FinishPolygon();
  end;

  if Sender = spdbtnStandardTools then
  begin
    ntbkToolOptions.PageIndex := STANDARD_PAGE_INDEX;
    FMainTool                 := gmtStandard;
  end
  else
  if Sender = spdbtnBrushTools then
  begin
    ntbkToolOptions.PageIndex := BRUSH_PAGE_INDEX;
    FMainTool                 := gmtBrush;
  end
  else
  if Sender = spdbtnMarqueeTools then
  begin
    ntbkToolOptions.PageIndex := MARQUEE_PAGE_INDEX;
    FMainTool                 := gmtMarquee;
  end
  else
  if Sender = spdbtnGradientTools then
  begin
    ntbkToolOptions.PageIndex := GRADIENT_PAGE_INDEX;
    FMainTool                 := gmtGradient;
  end
  else
  if Sender = spdbtnCropTools then
  begin
    ntbkToolOptions.PageIndex := CROP_PAGE_INDEX;
    FMainTool                 := gmtCrop;
  end
  else
  if Sender = spdbtnPaintBucketTools then
  begin
    ntbkToolOptions.PageIndex := PAINT_BUCKET_PAGE_INDEX;
    FMainTool                 := gmtPaintBucket;
  end
  else
  if Sender = spdbtnEraserTools then
  begin
    ntbkToolOptions.PageIndex := ERASER_PAGE_INDEX;
    FMainTool                 := gmtEraser;
  end
  else
  if Sender = spdbtnPenTools then
  begin
    ntbkToolOptions.PageIndex := PEN_TOOLS_PAGE_INDEX;
    FMainTool                 := gmtPenTools;

    if FMainTool <> LLastMainTool then
    begin
      if ActiveChildForm <> nil then
      begin
        // bring the path layer to front
        if Assigned(ActiveChildForm.PathLayer) then
        begin
          ActiveChildForm.PathLayer.BringToFront();
        end;
      end;
    end;
  end
  else
  if Sender = spdbtnMeasureTool then
  begin
    ntbkToolOptions.PageIndex := MEASURE_PAGE_INDEX;
    FMainTool                 := gmtMeasure;
  end
  else
  if Sender = spdbtnShapeTools then
  begin
    ntbkToolOptions.PageIndex := SHAPE_PAGE_INDEX;
    FMainTool                 := gmtShape;

    if Assigned(ActiveChildForm) then
    begin
      if FShapeRegionTool = srtMove then
      begin
        with ActiveChildForm do
        begin
          if LayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);

            LShapeRegionLayer.ShapeOutlineList.BackupCoordinates();
            LShapeRegionLayer.IsDismissed := False;
          end;
        end;
      end;
    end;
  end
  else
  if Sender = spdbtnTextTool then
  begin
    ntbkToolOptions.PageIndex := TEXT_PAGE_INDEX;
    FMainTool                 := gmtTextTool;
  end
  else
  if Sender = spdbtnEyedropper then
  begin
    FMainTool := gmtEyedropper;
  end
  else
  if Sender = spdbtnHandTool then
  begin
    FMainTool := gmtHandTool;
  end;

  if Assigned(ActiveChildForm) then
  begin
    if FMainTool <> gmtTextTool then
    begin
      if frmRichTextEditor.Visible then
      begin
        ActiveChildForm.CommitEdits();
      end;
    end;

    // Connect mouse events for image.
    ActiveChildForm.ConnectMouseEventsToImage();

    // Change cursor according to different main tool.
    ActiveChildForm.ChangeImageCursorByToolTemplets();

    if FMainTool <> gmtCrop then
    begin
      ActiveChildForm.FinishCrop();
    end;

    if FMainTool <> gmtMeasure then
    begin
      ActiveChildForm.RemoveMeasureLine();
      ActiveChildForm.RemoveMeasureLayer();
    end;

    if Assigned(ActiveChildForm.MagneticLasso) then
    begin
      if (FMainTool <> gmtMarquee) or
         (FMarqueeTool <> mtMagneticLasso) then
      begin
        ActiveChildForm.FinishMagneticLasso();
      end;
    end;
  end;

  UpdateToolsOptions();
  ShowStatusInfoOnStatusBar();

  if FMainTool in [gmtHandTool, gmtEyeDropper] then
  begin
    pnlToolOptions.Visible := False;
  end;

  imgToolOptionsVisibility.Visible := not (FMainTool in [gmtHandTool, gmtEyeDropper]);

  if pnlToolOptions.Visible then
  begin
    imgToolOptionsVisibility.Bitmap.Assign(dmMain.bmp32lstMainTools.Bitmap[IMG_LEFT_TRIPLE_ARROW_INDEX]);
  end
  else
  begin
    imgToolOptionsVisibility.Bitmap.Assign(dmMain.bmp32lstMainTools.Bitmap[IMG_RIGHT_TRIPLE_ARROW_INDEX]);
  end;

  // update the control for drawing control points properly
  if Assigned(ActiveChildForm) then
  begin
    ActiveChildForm.imgWorkArea.Changed();
  end;
end;

procedure TfrmMain.PopupBrushStyleMenusClick(Sender: TObject);
var
  LPoint: TPoint;
begin
  GetCursorPos(LPoint);
  dmMain.pmnBrushStyle.Popup(LPoint.X, LPoint.Y);
end;

procedure TfrmMain.ZoomSliderChange(Sender: TObject);
var
  LZoomValue : Integer;
begin
  LZoomValue            := ggbrZoomSlider.Position;
  lblZoomViewer.Caption := IntToStr(LZoomValue) + '%';

  if FCanChange then
  begin
    ActiveChildForm.Magnification := LZoomValue;

    // setting viewing scale of image viewer
    ActiveChildForm.imgWorkArea.Scale := LZoomValue / 100;

    if Assigned(ActiveChildForm.MeasureLayer) then
    begin
      ActiveChildForm.MeasureLayer.Bitmap.Clear($FFFFFFFF);
      ActiveChildForm.DrawMeasureLineOnMeasureLayer();
    end;

    if Assigned(ActiveChildForm.Selection) then
    begin
      ActiveChildForm.ShowProcessedSelection();
    end;

    if Assigned(ActiveChildForm.PathLayer) then
    begin
      ActiveChildForm.PathLayer.Bitmap.Clear($00000000);
      ActiveChildForm.DrawPathOnPathLayer();
    end;

    ActiveChildForm.RefreshCaption();
  end;
end;

procedure TfrmMain.cmbbxPenStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LBrushInfo: TLogBrush;
  i1, i2    : Integer;
  LPenStyle : DWORD;
begin
  with LBrushInfo do
  begin
    lbStyle := BS_SOLID;
    lbColor := clBlack;
    lbHatch := 0
  end;

  LPenStyle := PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_MITER;

  case Index of
    0:
      begin
        LPenStyle := LPenStyle + PS_SOLID;
      end;

    1:
      begin
        LPenStyle := LPenStyle + PS_DASH;
      end;

    2:
      begin
        LPenStyle := LPenStyle + PS_DOT;
      end;

    3:
      begin
        LPenStyle := LPenStyle + PS_DASHDOT;
      end;

    4:
      begin
        LPenStyle := LPenStyle + PS_DASHDOTDOT;
      end;
  end;

  (Control as TComboBox).Canvas.Pen.Handle :=
    ExtCreatePen(LPenStyle, 3, LBrushInfo, 0, nil);

  with (Control as TComboBox).Canvas do
  begin
    i1 := MulDiv(Rect.Left + Rect.Right, 1, 5);
    i2 := MulDiv(Rect.Left + Rect.Right, 4, 5);

    MoveTo( i1, (Rect.Top + Rect.Bottom) div 2 );
    LineTo( i2, (Rect.Top + Rect.Bottom) div 2 );
  end;
end;

procedure TfrmMain.spdbtnZoomInClick(Sender: TObject);
var
  LZoomValue: Integer;
begin
  if (ActiveChildForm.Magnification >= 1) and
     (ActiveChildForm.Magnification < 10) then
  begin
    ggbrZoomSlider.Position := ggbrZoomSlider.Position + 1;
  end
  else
  if (ActiveChildForm.Magnification >= 10) and
     (ActiveChildForm.Magnification < 25) then
  begin
    ggbrZoomSlider.Position := 25;
  end
  else
  if (ActiveChildForm.Magnification >= 25) and
     (ActiveChildForm.Magnification < 50) then
  begin
    ggbrZoomSlider.Position := 50;
  end
  else
  if (ActiveChildForm.Magnification >= 50) and
     (ActiveChildForm.Magnification < 75) then
  begin
    ggbrZoomSlider.Position := 75;
  end
  else
  if (ActiveChildForm.Magnification >= 75) and
     (ActiveChildForm.Magnification < 100) then
  begin
    ggbrZoomSlider.Position := 100;
  end
  else
  if (ActiveChildForm.Magnification >= 100) and
     (ActiveChildForm.Magnification < 800) then
  begin
    LZoomValue              := ggbrZoomSlider.Position;
    LZoomValue              := Trunc(LZoomValue / 100) * 100 + 100;
    ggbrZoomSlider.Position := LZoomValue;
  end
  else
  if (ActiveChildForm.Magnification >= 800) and
     (ActiveChildForm.Magnification < 1200) then
  begin
    ggbrZoomSlider.Position := 1200;
  end
  else
  if (ActiveChildForm.Magnification >= 1200) and
     (ActiveChildForm.Magnification < 1600) then
  begin
    ggbrZoomSlider.Position := 1600;
  end;
end;

procedure TfrmMain.spdbtnZoomOutClick(Sender: TObject);
var
  LZoomValue: Integer;
begin
  if (ActiveChildForm.Magnification > 1) and
     (ActiveChildForm.Magnification <= 10) then
  begin
    ggbrZoomSlider.Position := ggbrZoomSlider.Position - 1;
  end
  else
  if (ActiveChildForm.Magnification > 10) and
     (ActiveChildForm.Magnification <= 25) then
  begin
    ggbrZoomSlider.Position := 10;
  end
  else
  if (ActiveChildForm.Magnification > 25) and
     (ActiveChildForm.Magnification <= 50) then
  begin
    ggbrZoomSlider.Position := 25;
  end
  else
  if (ActiveChildForm.Magnification > 50) and
     (ActiveChildForm.Magnification <= 75) then
  begin
    ggbrZoomSlider.Position := 50;
  end
  else
  if (ActiveChildForm.Magnification > 75) and
     (ActiveChildForm.Magnification <= 100) then
  begin
    ggbrZoomSlider.Position := 75;
  end
  else
  if (ActiveChildForm.Magnification > 100) and
     (ActiveChildForm.Magnification <= 800) then
  begin
    LZoomValue := ggbrZoomSlider.Position;

    if (LZoomValue mod 100) > 0 then
    begin
      LZoomValue := Trunc(LZoomValue / 100) * 100;
    end
    else
    begin
      LZoomValue := LZoomValue - 100;
    end;

    ggbrZoomSlider.Position := LZoomValue;
  end
  else
  if (ActiveChildForm.Magnification > 800) and
     (ActiveChildForm.Magnification <= 1200) then
  begin
    ggbrZoomSlider.Position := 800;
  end
  else
  if (ActiveChildForm.Magnification > 1200) and
     (ActiveChildForm.Magnification <= 1600) then
  begin
    ggbrZoomSlider.Position := 1200;
  end;
end;

procedure TfrmMain.SetChildFormEditMode(Sender: TObject);
var
  LCommand            : TgmCustomCommand;
  LOldSelection       : TgmSelection;
  LNewSelection       : TgmSelection;
  LOldChannelMap      : TBitmap32;
  LMaskColor          : TColor32;
  LMaskColorIndicator : TgmMaskColorIndicator;
  LMaskOpacity        : Byte;
begin
  LCommand      := nil;
  LOldSelection := nil;
  LNewSelection := nil;
  
  if Assigned(ActiveChildForm) then
  begin
    if Sender = spdbtnStandardMode then
    begin
      if ActiveChildForm.EditMode <> emStandardMode then
      begin
        // for Undo/Redo
        LOldChannelMap := TBitmap32.Create();
        LOldChannelMap.Assign(ActiveChildForm.ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

        // for Undo/Redo
        if Assigned(ActiveChildForm.Selection) then
        begin
          LOldSelection := TgmSelection.Create();
          LOldSelection.AssignAllSelectionData(ActiveChildForm.Selection);
        end;

        // for Undo/Redo
        with ActiveChildForm.ChannelManager do
        begin
          LMaskColor          := QuickMaskChannel.MaskColor;
          LMaskColorIndicator := QuickMaskChannel.MaskColorIndicator;
          LMaskOpacity        := QuickMaskChannel.MaskOpacity;
        end;

        // then, switch to standard edit mode
        ActiveChildForm.EditMode := emStandardMode;
        frmColor.ColorMode       := cmRGB;  // update the appearance of the color form

        // for Undo/Redo
        if Assigned(ActiveChildForm.Selection) then
        begin
          LNewSelection := TgmSelection.Create();
          LNewSelection.AssignAllSelectionData(ActiveChildForm.Selection);
        end;

        // Undo/Redo
        LCommand := TgmExitFromQuickMaskChannelCommand.Create(
          ActiveChildForm.ChannelManager,
          LOldSelection, LNewSelection, LOldChannelMap,
          LMaskColor, LMaskColorIndicator, LMaskOpacity,
          ActiveChildForm.SetEditModeForUndoRedo,
          ActiveChildForm.GetSelectionForUndoRedo,
          ActiveChildForm.DeleteSelectionForUndoRedo);

        if Assigned(LOldSelection) then
        begin
          LOldSelection.Free();
        end;

        if Assigned(LNewSelection) then
        begin
          LNewSelection.Free();
        end;

        LOldChannelMap.Free();
      end;
    end
    else
    if Sender = spdbtnQuickMaskMode then
    begin
      if ActiveChildForm.EditMode <> emQuickMaskMode then
      begin
        // create Undo/Redo command, first
        LCommand := TgmEnterQuickMaskChannelCommand.Create(
          ActiveChildForm.ChannelManager,
          ActiveChildForm.LayerList,
          ActiveChildForm.LayerList.SelectedIndex,
          ActiveChildForm.Selection,
          ActiveChildForm.SetEditModeForUndoRedo,
          ActiveChildForm.GetSelectionForUndoRedo,
          ActiveChildForm.DeleteSelectionForUndoRedo);

        // then, switch to quick mask edit mode
        ActiveChildForm.EditMode := emQuickMaskMode;
        frmColor.ColorMode       := cmGrayscale;
      end;
    end;

    // Undo/Redo
    if Assigned(LCommand) then
    begin
      ActiveChildForm.CommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TfrmMain.ChangeStandardTools(Sender: TObject);
var
  LST : TgmStandardTool;
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // finish the creation of uncompleted figures ...
  
  if Sender <> spdbtnPolygon then
  begin
    if Assigned(ActiveChildForm) then
    begin
      ActiveChildForm.FinishPolygon();
    end;
  end;

  if Sender <> spdbtnBezierCurve then
  begin
    if Assigned(ActiveChildForm) then
    begin
      ActiveChildForm.FinishCurves();
    end;
  end;

  // change Figure Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnStandardTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnStandardTools.Hint := TSpeedButton(Sender).Hint;
  end;

  LST := FStandardTool;

  if Sender = spdbtnPencil then
  begin
    FStandardTool     := gstPencil;
    FLastStandardTool := gstPencil;
  end
  else
  if Sender = spdbtnStraightLine then
  begin
    FStandardTool     := gstStraightLine;
    FLastStandardTool := gstStraightLine;
  end
  else
  if Sender = spdbtnBezierCurve then
  begin
    FStandardTool     := gstBezierCurve;
    FLastStandardTool := gstBezierCurve;
  end
  else
  if Sender = spdbtnPolygon then
  begin
    FStandardTool     := gstPolygon;
    FLastStandardTool := gstPolygon;
  end
  else
  if Sender = spdbtnRegularPolygon then
  begin
    FStandardTool     := gstRegularPolygon;
    FLastStandardTool := gstRegularPolygon;
  end
  else
  if Sender = spdbtnRectangle then
  begin
    FStandardTool     := gstRectangle;
    FLastStandardTool := gstRectangle;
  end
  else
  if Sender = spdbtnRoundRectangle then
  begin
    FStandardTool     := gstRoundRectangle;
    FLastStandardTool := gstRoundRectangle;
  end
  else
  if Sender = spdbtnEllipse then
  begin
    FStandardTool     := gstEllipse;
    FLastStandardTool := gstEllipse;
  end
  else
  if Sender = spdbtnMoveObjects then
  begin
    FStandardTool := gstMoveObjects;
  end
  else
  if Sender = spdbtnRSPartially then
  begin
    FStandardTool := gstPartiallySelect;

    if FStandardTool <> LST then
    begin
      if ActiveChildForm <> nil then
      begin
        if ActiveChildForm.FigureManager.SelectedFigureCount > 0 then
        begin
          ActiveChildForm.FigureManager.DeselectAllFigures();
        end;
      end;
    end;
  end
  else
  if Sender = spdbtnRSTotally then
  begin
    FStandardTool := gstTotallySelect;

    if FStandardTool <> LST then
    begin
      if ActiveChildForm <> nil then
      begin
        if ActiveChildForm.FigureManager.SelectedFigureCount > 0 then
        begin
          ActiveChildForm.FigureManager.DeselectAllFigures();
        end;
      end;
    end;
  end;

  if ActiveChildForm <> nil then
  begin
    if FStandardTool <> gstMoveObjects then
    begin
      if ActiveChildForm.FigureManager.SelectedFigureCount() > 0 then
      begin
        ActiveChildForm.FigureManager.DeselectAllFigures();
      end;
    end;

    ActiveChildForm.ConnectMouseEventsToImage();
    ActiveChildForm.ChangeImageCursorByStandardTools(); // Change cursor
  end;

  UpdateStandardOptions();
  ShowStatusInfoOnStatusBar();
end;

procedure TfrmMain.cmbbxPenStyleChange(Sender: TObject);
begin
  // Change line style.
  FGlobalPenStyle := TPenStyle(cmbbxPenStyle.ItemIndex);
end;

procedure TfrmMain.ChangeGlobalPenWidth(Sender: TObject);
begin
  // Change line width.
  FGlobalPenWidth := cmbbxPenWidth.ItemIndex + 1;
end;

procedure TfrmMain.cmbbxPenWidthDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LBrushInfo: TLogBrush;
  i1, i2    : Integer;
begin
  with LBrushInfo do
  begin
    lbstyle := BS_Solid;
    lbColor := clBlack;
    lbHatch :=0;
  end;

  (Control as TComboBox).Canvas.Pen.Handle :=
    ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_MITER,
    Index + 1, LBrushInfo, 0, nil);

  with (Control as TComboBox).Canvas do
  begin
    i1 := MulDiv(Rect.Left + Rect.Right, 1, 5);
    i2 := MulDiv(Rect.Left + Rect.Right, 4, 5);

    MoveTo( i1, (Rect.Top + Rect.Bottom) div 2 );
    LineTo( i2, (Rect.Top + Rect.Bottom) div 2 );
  end;
end;

procedure TfrmMain.ChangeRadiusSides(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      case FStandardTool of
        gstRegularPolygon:
          begin
            updwnRadiusSides.Position := StrToInt(edtRadiusSides.Text);
            FStandardPolygonSides     := updwnRadiusSides.Position;
          end;

        gstRoundRectangle:
          begin
            updwnRadiusSides.Position := StrToInt(edtRadiusSides.Text);
            FStandardCornerRadius     := updwnRadiusSides.Position;
          end;
      end;

    except
      case FStandardTool of
        gstRegularPolygon:
          begin
            edtRadiusSides.Text := IntToStr(FStandardPolygonSides);
          end;

        gstRoundRectangle:
          begin
            edtRadiusSides.Text := IntToStr(FStandardCornerRadius);
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeBrushTools(Sender: TObject);

    procedure DeleteLastBrush;
    begin
      if Assigned(FGMBrush) then
      begin
        FreeAndNil(FGMBrush);
      end;
    end;

begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Brush Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnBrushTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnBrushTools.Hint := TSpeedButton(Sender).Hint;
  end;

  DeleteLastBrush;

  if Sender = spdbtnPaintBrush then
  begin
    FBrushTool := btPaintBrush;
    FGMBrush   := TgmPaintBrush.Create;
  end
  else
  if Sender = spdbtnHistoryBrush then
  begin
    FBrushTool := btHistoryBrush;
    FGMBrush   := TgmHistoryBrush.Create;
  end
  else
  if Sender = spdbtnAirBrush then
  begin
    FBrushTool := btAirBrush;
  end
  else
  if Sender = spdbtnJetGun then
  begin
    FBrushTool := btJetGunBrush;
  end
  else
  if Sender = spdbtnCloneStamp then
  begin
    FBrushTool := btCloneStamp;
    FGMBrush   := TgmCloneStamp.Create;
  end
  else
  if Sender = spdbtnPatternStamp then
  begin
    FBrushTool := btPatternStamp;
    FGMBrush   := TgmPatternStamp.Create;
  end
  else
  if Sender = spdbtnBlurBrush then
  begin
    FBrushTool := btBlurSharpenBrush;
    FGMBrush   := TgmBlurSharpenBrush.Create(gmctBlur);
  end
  else
  if Sender = spdbtnSharpenBrush then
  begin
    FBrushTool := btBlurSharpenBrush;
    FGMBrush   := TgmBlurSharpenBrush.Create(gmctSharpen);
  end
  else
  if Sender = spdbtnSmudge then
  begin
    FBrushTool := btSmudge;
    FGMBrush   := TgmSmudge.Create;
  end
  else
  if Sender = spdbtnDodgeBrush then
  begin
    FBrushTool := btDodgeBurnBrush;
    FGMBrush   := TgmDodgeBurnBrush.Create(dbtDodge);
  end
  else
  if Sender = spdbtnBurnBrush then
  begin
    FBrushTool := btDodgeBurnBrush;
    FGMBrush   := TgmDodgeBurnBrush.Create(dbtBurn);
  end
  else
  if Sender = spdbtnHighHueBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmHighHue);
  end
  else
  if Sender = spdbtnLowHueBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmLowHue);
  end
  else
  if Sender = spdbtnHighSaturationBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmHighSaturation);
  end
  else
  if Sender = spdbtnLowSaturationBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmLowSaturation);
  end
  else
  if Sender = spdbtnHighLuminosityBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmHighLuminosity);
  end
  else
  if Sender = spdbtnLowLuminosityBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmLowLuminosity);
  end
  else
  if Sender = spdbtnBrightnessBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmBrightness);
  end
  else
  if Sender = spdbtnDarknessBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmDarkness);
  end
  else
  if Sender = spdbtnHighContrastBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmHighContrast);
  end
  else
  if Sender = spdbtnLowContrastBrush then
  begin
    FBrushTool := btLightBrush;
    FGMBrush   := TgmLightBrush.Create(lbmLowContrast);
  end;
  
  UpdateBrushOptions;
  ShowStatusInfoOnStatusBar;
end;

procedure TfrmMain.OpenPaintingBrushSelector(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmPaintingBrush.Left           := p.X;
  frmPaintingBrush.Top            := p.Y;
  frmPaintingBrush.StrokeListUser := sluBrush;
  frmPaintingBrush.Show;
end;

procedure TfrmMain.OpenPatternSelectorForStamp(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmPatterns.Left            := p.X;
  frmPatterns.Top             := p.Y;
  frmPatterns.PatternListUser := pluStamp;
  frmPatternS.Show;
end;

procedure TfrmMain.OpenBrushDynamicsEditor(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmBrushDynamics.Left := p.X;
  frmBrushDynamics.Top  := p.Y;
  frmBrushDynamics.Show;
end;

procedure TfrmMain.ChangeBrushMode(Sender: TObject);
begin
  if FBrushTool = btDodgeBurnBrush then
  begin
    FDodgeBurnMode := TgmDodgeBurnMode(cmbbxBrushMode.ItemIndex);
  end
  else
  begin
    FBrushBlendMode := TBlendMode32(cmbbxBrushMode.ItemIndex);
  end;
end;

procedure TfrmMain.ChangeBrushOPEI(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnBrushOPEI.Position :=StrToInt(edtBrushOPEI.Text);

      if FBrushTool in [btPaintBrush, btHistoryBrush, btCloneStamp,
                        btPatternStamp] then
      begin
        FBrushOpacity := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btAirBrush then
      begin
        FAirPressure := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btJetGunBrush then
      begin
        FJetGunPressure := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btSmudge then
      begin
        FSmudgePressure := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btDodgeBurnBrush then
      begin
        FDodgeBurnExposure := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btBlurSharpenBrush then
      begin
        FBlurSharpenPressure := updwnBrushOPEI.Position;
      end
      else if FBrushTool = btLightBrush then
      begin
        FBrushIntensity := updwnBrushOPEI.Position;
      end;
      
    except
      case FBrushTool of
        btPaintBrush,
        btHistoryBrush,
        btCloneStamp,
        btPatternStamp:
          begin
            edtBrushOPEI.Text := IntToStr(FBrushOpacity);
          end;
      
        btAirBrush:
          begin
            edtBrushOPEI.Text := IntToStr(FAirPressure);
          end;
          
        btJetGunBrush:
          begin
            edtBrushOPEI.Text := IntToStr(FJetGunPressure);
          end;
          
        btSmudge :
          begin
            edtBrushOPEI.Text := IntToStr(FSmudgePressure);
          end;
          
        btDodgeBurnBrush:
          begin
            edtBrushOPEI.Text := IntToStr(FDodgeBurnExposure);
          end;
          
        btBlurSharpenBrush:
          begin
            edtBrushOPEI.Text := IntToStr(FBlurSharpenPressure);
          end;
          
        btLightBrush:
          begin
            edtBrushOPEI.Text := IntToStr(FBrushIntensity);
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeBrushInterval(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnBrushInterval.Position := StrToInt(edtBrushInterval.Text);

      if FBrushTool = btJetGunBrush then
      begin
        FJetGun.Interval := updwnBrushInterval.Position;

        if ActiveChildForm <> nil then
        begin
          ActiveChildForm.tmrSpecialBrushes.Interval := FJetGun.Interval;  
        end;
      end
      else
      if FBrushTool = btAirBrush then
      begin
        FAirBrush.Interval := updwnBrushInterval.Position;

        if ActiveChildForm <> nil then
        begin
          ActiveChildForm.tmrSpecialBrushes.Interval := FAirBrush.Interval;  
        end;
      end
      else
      if FBrushTool = btBlurSharpenBrush then
      begin
        FBlurSharpenTimerInterval := updwnBrushInterval.Position;

        if ActiveChildForm <> nil then
        begin
          ActiveChildForm.tmrSpecialBrushes.Interval := FBlurSharpenTimerInterval;  
        end;
      end
      else
      begin
        FBrushInterval := updwnBrushInterval.Position;
      end;
      
    except
      edtBrushInterval.Text := IntToStr(updwnBrushInterval.Position);
    end;
  end;
end;

procedure TfrmMain.ChangeBrushRadius(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnBrushRadius.Position := StrToInt(edtBrushRadius.Text);
      FBrushRadius              := updwnBrushRadius.Position;
    except
      edtBrushRadius.Text := IntToStr(FBrushRadius);
    end;
  end;
end;

procedure TfrmMain.chckbxColorAndSampleClick(Sender: TObject);
begin
  case FBrushTool of
    btJetGunBrush:
      begin
        FIsRandomColor := chckbxColorAndSample.Checked;
      end;

    btCloneStamp:
      begin
        FIsUseAllLayers := chckbxColorAndSample.Checked;
      end;
  end;
end;

procedure TfrmMain.ChangeMarqueeTools(Sender: TObject);
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Marquee Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnMarqueeTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnMarqueeTools.Hint := TSpeedButton(Sender).Hint;
  end;

  if ActiveChildForm <> nil then
  begin
    if Sender = spdbtnSelect then
    begin
      // don't drawing the selection
      ActiveChildForm.MarqueeDrawingState := dsNotDrawing
    end
    else
    begin
      // drawing new marquee
      ActiveChildForm.MarqueeDrawingState := dsNewFigure;
    end;
  end;

  if Sender = spdbtnSelect then
  begin
    FMarqueeTool := mtMoveResize;
  end
  else if Sender = spdbtnSingleRowMarquee then
  begin
    FMarqueeTool := mtSingleRow;
  end
  else if Sender = spdbtnSingleColumnMarquee then
  begin
    FMarqueeTool := mtSingleColumn;
  end
  else if Sender = spdbtnRectangularMarquee then
  begin
    FMarqueeTool := mtRectangular;
  end
  else if Sender = spdbtnRoundRectangularMarquee then
  begin
    FMarqueeTool := mtRoundRectangular;
  end
  else if Sender = spdbtnEllipticalMarquee then
  begin
    FMarqueeTool := mtElliptical;
  end
  else if Sender = spdbtnPolygonalMarquee then
  begin
    FMarqueeTool := mtPolygonal;
  end
  else if Sender = spdbtnRegularPolygonMarquee then
  begin
    FMarqueeTool := mtRegularPolygon;
  end
  else if Sender = spdbtnMagneticLasso then
  begin
    FMarqueeTool := mtMagneticLasso;
  end
  else if Sender = spdbtnLassoMarquee then
  begin
    FMarqueeTool := mtLasso;
  end
  else if Sender = spdbtnMagicWand then
  begin
    FMarqueeTool := mtMagicWand;
  end;

  if Assigned(ActiveChildForm) then
  begin
    if Assigned(ActiveChildForm.MagneticLasso) then
    begin
      if FMarqueeTool <> mtMagneticLasso then
      begin
        ActiveChildForm.FinishMagneticLasso;
      end;
    end;
  end;

  UpdateMarqueeOptions;
  ShowStatusInfoOnStatusBar;

  if ActiveChildForm <> nil then
  begin
    if Assigned(ActiveChildForm.Selection) then
    begin
      if ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if ActiveChildForm.SelectionTransformation = nil then
        begin
          if ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer then
          begin
            // render the selection

            ActiveChildForm.Selection.ShowSelection(
              ActiveChildForm.LayerList.SelectedLayer.LayerBitmap,
              ActiveChildForm.ChannelManager.SelectedColorChannels);

            ActiveChildForm.LayerList.SelectedLayer.Changed;
          end;
        end;
      end;

      // for render control border of selection
      ActiveChildForm.imgWorkArea.Changed;
    end;

    // change cursor
    ActiveChildForm.ChangeImageCursorByMarqueeTools;
  end;
end;

procedure TfrmMain.MarqueeToolButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ActiveChildForm <> nil then
  begin
    ActiveChildForm.FinishTransformation;
  end;
end;

procedure TfrmMain.ChangeMarqueeMode(Sender: TObject);
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  if Sender = spdbtnNewSelection then
  begin
    FMarqueeMode := mmNew;
  end
  else if Sender = spdbtnAddSelection then
  begin
    FMarqueeMode := mmAdd;
  end
  else if Sender = spdbtnSubtractSelection then
  begin
    FMarqueeMode := mmSubtract;
  end 
  else if Sender = spdbtnIntersectSelection then
  begin
    FMarqueeMode := mmIntersect;
  end
  else if Sender = spdbtnExcludeOverlapSelection then
  begin
    FMarqueeMode := mmExcludeOverlap;
  end;

  if ActiveChildForm <> nil then
  begin
    ActiveChildForm.ChangeImageCursorByMarqueeTools; // change cursor
  end;
end;

procedure TfrmMain.edtToleranceSidesChange(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnToleranceSides.Position := StrToInt(edtToleranceSides.Text);

      case FMarqueeTool of
        mtMagicWand:
          begin
            FMagicWandTolerance := updwnToleranceSides.Position;
          end;

        mtRegularPolygon:
          begin
            FRPMSides := updwnToleranceSides.Position;
          end;

        mtRoundRectangular:
          begin
            FRRMCornerRadius := updwnToleranceSides.Position;
          end;
      end;

    except
      case FMarqueeTool of
        mtMagicWand:
          begin
            edtToleranceSides.Text := IntToStr(FMagicWandTolerance);
          end;

        mtRegularPolygon:
          begin
            edtToleranceSides.Text := IntToStr(FRPMSides);
          end;
          
        mtRoundRectangular:
          begin
            edtToleranceSides.Text := IntToStr(FRRMCornerRadius);
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeGradientTools(Sender: TObject);
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Gradient Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnGradientTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnGradientTools.Hint := TSpeedButton(Sender).Hint;
  end;

  if Sender = spdbtnLinearGradient then
  begin
    FGradientRenderMode := grmLinear;
  end
  else if Sender = spdbtnRadialGradient then
  begin
    FGradientRenderMode := grmRadial;
  end
  else if Sender = spdbtnAngleGradient then
  begin
    FGradientRenderMode := grmAngle;
  end
  else if Sender = spdbtnReflectedGradient then
  begin
    FGradientRenderMode := grmReflected;
  end
  else if Sender = spdbtnDiamondGradient then
  begin
    FGradientRenderMode := grmDiamond;
  end;

  ShowStatusInfoOnStatusBar;
end;

procedure TfrmMain.spdbtnOpenGradientPickerClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmGradientPicker.Left         := p.X;
  frmGradientPicker.Top          := p.Y;
  frmGradientPicker.GradientUser := guGradientTools;
  frmGradientPicker.Show;
end;

procedure TfrmMain.imgSelectedGradientClick(Sender: TObject);
begin
  frmGradientEditor := TfrmGradientEditor.Create(nil);
  try
    frmGradientEditor.GradientUser := guGradientTools;
    frmGradientEditor.ShowModal;
  finally
    FreeAndNil(frmGradientEditor);
  end;
end;

procedure TfrmMain.imgSelectedGradientPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer, True);
end;

procedure TfrmMain.ChangeGradientBlendMode(Sender: TObject);
begin
  FGradientBlendMode := TBlendMode32(cmbbxGradientBlendMode.ItemIndex);
end;

procedure TfrmMain.ChangeGradientOpacity(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnGradientOpacity.Position := StrToInt(edtGradientOpacity.Text);
      FGradientBlendOpacity         := updwnGradientOpacity.Position;
    except
      edtGradientOpacity.Text := IntToStr(FGradientBlendOpacity);
    end;
  end;
end;

procedure TfrmMain.chckbxReverseGradientClick(Sender: TObject);
begin
  frmGradientPicker.ShowDrawingToolSelectedGradient;
end;

procedure TfrmMain.edtCropWidthChange(Sender: TObject);
var
  LValue : Integer;
begin
  if ActiveChildForm.Crop <> nil then
  begin
    if edtCropWidth.Text <> '' then
    begin
      if FCanChange then
      begin
        try
          LValue := StrToInt(edtCropWidth.Text);

          if LValue < 0 then
          begin
            LValue            := 0;
            edtCropWidth.Text := IntToStr(LValue);
          end;

          ActiveChildForm.Crop.CropAreaWidth := LValue;
        except
          edtCropWidth.Text := IntToStr(ActiveChildForm.Crop.CropAreaWidth);
        end;

        // update the crop tool display
        ActiveChildForm.Crop.DrawShield();
        ActiveChildForm.imgWorkArea.Changed;
      end;
    end;
  end;
end;

procedure TfrmMain.edtCropHeightChange(Sender: TObject);
var
  LValue : Integer;
begin
  if ActiveChildForm.Crop <> nil then
  begin
    if edtCropHeight.Text <> '' then
    begin
      if FCanChange then
      begin
        try
          LValue := StrToInt(edtCropHeight.Text);

          if LValue < 0 then
          begin
            LValue             := 0;
            edtCropHeight.Text := IntToStr(LValue);
          end;

          ActiveChildForm.Crop.CropAreaHeight := LValue;
        except
          edtCropHeight.Text := IntToStr(ActiveChildForm.Crop.CropAreaHeight);
        end;

        // update the crop tool display
        ActiveChildForm.Crop.DrawShield();
        ActiveChildForm.imgWorkArea.Changed();
      end;
    end;
  end;
end;

procedure TfrmMain.ShieldCroppedArea(Sender: TObject);
begin
  if chckbxShieldCroppedArea.Checked then
  begin
    pnlCropShield.Top     := pnlCropOptions.Top + pnlCropOptions.Height;
    pnlCropShield.Visible := True;
  end
  else
  begin
    pnlCropShield.Visible := False;
  end;

  if Assigned(ActiveChildForm.Crop) then
  begin
    ActiveChildForm.Crop.IsShieldCroppedArea := chckbxShieldCroppedArea.Checked;
    ActiveChildForm.Crop.DrawShield();
    ActiveChildForm.imgWorkArea.Changed();
  end;
end;

procedure TfrmMain.chckbxResizeCropClick(Sender: TObject);
begin
  if chckbxResizeCrop.Checked then
  begin
    if pnlCropShield.Visible then
    begin
      pnlResizeCrop.Top := pnlCropShield.Top + pnlCropShield.Height;
    end
    else
    begin
      pnlResizeCrop.Top := pnlCropOptions.Top + pnlCropOptions.Height;
    end;

    pnlResizeCrop.Visible := True;
  end
  else
  begin
    pnlResizeCrop.Visible := False;
  end;
end;

procedure TfrmMain.shpCroppedShieldColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(ActiveChildForm.Crop) then
  begin
    dmMain.clrdlgRGB.Color := ActiveChildForm.Crop.ShieldWinColor;

    if dmMain.clrdlgRGB.Execute() then
    begin
      shpCroppedShieldColor.Brush.Color  := dmMain.clrdlgRGB.Color;
      ActiveChildForm.Crop.ShieldColor32 := Color32(dmMain.clrdlgRGB.Color);

      ActiveChildForm.Crop.DrawShield();
      ActiveChildForm.imgWorkArea.Changed();
    end;
  end;
end;

procedure TfrmMain.ChangeCroppedShieldOpacity(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnCroppedShieldOpacity.Position := StrToInt(edtCroppedShieldOpacity.Text);

      if ActiveChildForm <> nil then
      begin
        if Assigned(ActiveChildForm.Crop) then
        begin
          ActiveChildForm.Crop.ShieldOpacity := updwnCroppedShieldOpacity.Position;
          ActiveChildForm.Crop.DrawShield();
          ActiveChildForm.imgWorkArea.Changed();
        end;
      end;

    except
      edtCroppedShieldOpacity.Text := IntToStr(updwnCroppedShieldOpacity.Position);
    end;
  end;
end;

procedure TfrmMain.edtResizeCropWidthChange(Sender: TObject);
var
  LMinWidth, LMaxWidth, LValue :Integer;
begin
  if Assigned(ActiveChildForm.Crop) then
  begin
    if edtResizeCropWidth.Text <> '' then
    begin
      if FCanChange then
      begin
        LMinWidth := 16;
        LMaxWidth := ActiveChildForm.LayerList.SelectedLayer.LayerBitmap.Width;
        try
          LValue := StrToInt(edtResizeCropWidth.Text);
          EnsureValueInRange(LValue, LMinWidth, LMaxWidth);
          ActiveChildForm.Crop.ResizeW := LValue;
        except
          edtResizeCropWidth.Text := IntToStr(ActiveChildForm.Crop.ResizeW);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.edtResizeCropWidthExit(Sender: TObject);
begin
  FCanChange := False;
  try
    if Assigned(ActiveChildForm.Crop) then
    begin
      if edtResizeCropWidth.Text <> '' then
      begin
        edtResizeCropWidth.Text := IntToStr(ActiveChildForm.Crop.ResizeW);
      end;
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmMain.edtResizeCropHeightChange(Sender: TObject);
var
  LMinHeight, LMaxHeight, LValue :Integer;
begin
  if ActiveChildForm.Crop <> nil then
  begin
    if edtResizeCropHeight.Text <> '' then
    begin
      if FCanChange then
      begin
        LMinHeight := 16;
        LMaxHeight := ActiveChildForm.LayerList.SelectedLayer.LayerBitmap.Height;
        try
          LValue := StrToInt(edtResizeCropHeight.Text);
          EnsureValueInRange(LValue, LMinHeight, LMaxHeight);
          ActiveChildForm.Crop.ResizeH := LValue;
        except
          edtResizeCropHeight.Text := IntToStr(ActiveChildForm.Crop.ResizeH);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.edtResizeCropHeightExit(Sender: TObject);
begin
  FCanChange := False;
  try
    if Assigned(ActiveChildForm.Crop) then
    begin
      if edtResizeCropHeight.Text <> '' then
      begin
        edtResizeCropHeight.Text := IntToStr(ActiveChildForm.Crop.ResizeH);
      end;
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmMain.btnShowCropedAreaSizeClick(Sender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    if Assigned(ActiveChildForm.Crop) then
    begin
      edtResizeCropWidth.Text  := IntToStr(ActiveChildForm.Crop.CropAreaWidth);
      edtResizeCropHeight.Text := IntToStr(ActiveChildForm.Crop.CropAreaHeight);
    end;
  end;
end;

procedure TfrmMain.ChangePaintBucketFillSource(Sender: TObject);
begin
  case cmbbxPaintBucketFillSource.ItemIndex of
    0:
      begin
        FPaintBucketFillSource := pbfsForeColor;
      end;

    1:
      begin
        FPaintBucketFillSource := pbfsBackColor;
      end;
      
    2:
      begin
        FPaintBucketFillSource := pbfsPattern;
      end;
  end;

  UpdatePaintBucketOptions;
end;

procedure TfrmMain.ChangePaintBucketFillMode(Sender: TObject);
begin
  FPaintBucketBlendMode := TBlendMode32(cmbbxPaintBucketFillMode.ItemIndex);
end;

procedure TfrmMain.ChangePaintBucketOpacity(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnPaintBucketOpacity.Position := StrToInt(edtPaintBucketOpacity.Text);
      FPaintBucketOpacity              := updwnPaintBucketOpacity.Position;
    except
      edtPaintBucketOpacity.Text := IntToStr(FPaintBucketOpacity);
    end;
  end;
end;

procedure TfrmMain.ChangePaintBucketTolerance(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      udpwnPaintBucketTolerance.Position := StrToInt(edtPaintBucketTolerance.Text);
      FPaintBucketTolerance              := udpwnPaintBucketTolerance.Position;
    except
      edtPaintBucketTolerance.Text := IntToStr(FPaintBucketTolerance);
    end;
  end;
end;

procedure TfrmMain.spdbtnOpenPatternForFillClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmPatterns.Left            := p.X;
  frmPatterns.Top             := p.Y;
  frmPatterns.PatternListUser := pluPaintBucket;
  frmPatternS.Show;
end;

procedure TfrmMain.spdbtnPaintBucketAdvancedOptionsClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmPaintBucketAdvancedOptions.Left := p.X;
  frmPaintBucketAdvancedOptions.Top  := p.Y;
  frmPaintBucketAdvancedOptions.Show;
end;

procedure TfrmMain.ChangeEraserTools(Sender: TObject);

    procedure DeleteLastEraser;
    begin
      if Assigned(FGMEraser) then
      begin
        FreeAndNil(FGMEraser);
      end;
    end;

var
  LLastEraserTool: TgmEraserTool;
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Eraser Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnEraserTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnEraserTools.Hint := TSpeedButton(Sender).Hint;
  end;

  LLastEraserTool := FEraserTool;

  if Sender = spdbtnEraser then
  begin
    FEraserTool := etEraser;

    if FEraserTool <> LLastEraserTool then
    begin
      DeleteLastEraser;
      FGMEraser := TgmEraser.Create;
    end;
  end
  else
  if Sender = spdbtnBackgroundEraser then
  begin
    FEraserTool := etBackgroundEraser;

    if FEraserTool <> LLastEraserTool then
    begin
      DeleteLastEraser;
      FGMEraser := TgmBackgroundEraser.Create;
    end;
  end
  else
  if Sender = spdbtnMagicEraser then
  begin
    FEraserTool := etMagicEraser;
  end;

  if Assigned(ActiveChildForm) then
  begin
    ActiveChildForm.ChangeImageCursorByEraserTools;
  end;
  
  UpdateEraserOptions;
  ShowStatusInfoOnStatusBar;
end;

procedure TfrmMain.spdbtnSelectEraserStrokeClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmPaintingBrush.Left           := p.X;
  frmPaintingBrush.Top            := p.Y;
  frmPaintingBrush.StrokeListUser := sluEraser;
  frmPaintingBrush.Show;
end;

procedure TfrmMain.spdbtnEraserAdvancedOptionsClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmEraserAdvancedOptions.Left := p.X;
  frmEraserAdvancedOptions.Top  := p.Y;
  frmEraserAdvancedOptions.Show;
end;

procedure TfrmMain.spdbtnEraserDynamicsClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  frmBrushDynamics.Left := p.X;
  frmBrushDynamics.Top  := p.Y;
  frmBrushDynamics.Show;
end;

procedure TfrmMain.ChangeEraserModeLimit(Sender: TObject);
begin
  case FEraserTool of
    etEraser:
      begin
        FErasingMode := GetErasingMode(cmbbxEraserModeLimit.ItemIndex);
      end;

    etBackgroundEraser:
      begin
        FBackgroundEraserLimit := GetErasingLimit(cmbbxEraserModeLimit.ItemIndex);
      end;
  end;

  UpdateEraserOptions;
end;

procedure TfrmMain.ChangeEraserSampling(Sender: TObject);
begin
  FEraserSamplingMode := TgmBackgroundSamplingMode(cmbbxEraserSampling.ItemIndex);
end;

procedure TfrmMain.ChangeEraserOpacityPressure(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnEraserOpacityPressure.Position := StrToInt(edtEraserOpacityPressure.Text);

      case FEraserTool of
        etEraser:
          begin
            case FErasingMode of
              emPaintBrush:
                begin
                  FErasingOpacity := updwnEraserOpacityPressure.Position;
                end;
                
              emAirBrush:
                begin
                  FAirErasingPressure := updwnEraserOpacityPressure.Position;
                end;
            end;
          end;

        etMagicEraser:
          begin
            FErasingOpacity := updwnEraserOpacityPressure.Position;
          end;
      end;
      
    except
      case FEraserTool of
        etEraser:
          begin
            case FErasingMode of
              emPaintBrush:
                begin
                  edtEraserOpacityPressure.Text := IntToStr(FErasingOpacity);
                end;
                
              emAirBrush:
                begin
                  edtEraserOpacityPressure.Text := IntToStr(FAirErasingPressure);
                end;
            end;
          end;
          
        etMagicEraser:
          begin
            edtEraserOpacityPressure.Text := IntToStr(FErasingOpacity);
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeEraserInterval(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnEraserInterval.Position := StrToInt(edtEraserInterval.Text);

      case FEraserTool of
        etEraser:
          begin
            case FErasingMode of
              emPaintBrush:
                begin
                  FErasingInterval := updwnEraserInterval.Position;
                end;
                
              emAirBrush:
                begin
                  FAirErasingInterval := updwnEraserInterval.Position;
                end;
            end;
          end;

        etBackgroundEraser:
          begin
            case FBackgroundEraserLimit of
              belDiscontiguous:
                begin
                  FAirErasingInterval := updwnEraserInterval.Position;
                end;
                
              belContiguous:
                begin
                  FErasingInterval := updwnEraserInterval.Position;
                end;
            end;
          end;
      end;
      
    except
      case FEraserTool of
        etEraser:
          begin
            case FErasingMode of
              emPaintBrush:
                begin
                  edtEraserInterval.Text := IntToStr(FErasingInterval);
                end;
                
              emAirBrush:
                begin
                  edtEraserInterval.Text := IntToStr(FAirErasingInterval);
                end;
            end;
          end;
          
        etBackgroundEraser:
          begin
            case FBackgroundEraserLimit of
              belDiscontiguous:
                begin
                  edtEraserInterval.Text := IntToStr(FAirErasingInterval);
                end;
                
              belContiguous:
                begin
                  edtEraserInterval.Text := IntToStr(FErasingInterval);
                end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeEraserTolerance(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnEraserTolerance.Position := StrToInt(edtEraserTolerance.Text);
      FErasingTolerance             := updwnEraserTolerance.Position;
    except
      edtEraserTolerance.Text := IntToStr(FErasingTolerance);
    end;
  end;
end;

procedure TfrmMain.ChangePenTools(Sender: TObject);
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Pen Path Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnPenTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnPenTools.Hint := TSpeedButton(Sender).Hint;
  end;

  if Sender = spdbtnPathComponentSelectionTool then
  begin
    FPenTool := ptPathComponentSelection;
  end
  else if Sender = spdbtnDirectSelectionTool then
  begin
    FPenTool := ptDirectSelection;
  end
  else if Sender = spdbtnPenTool then
  begin
    FPenTool := ptPenTool;
  end
  else if Sender = spdbtnAddAnchorPointTool then
  begin
    FPenTool := ptAddAnchorPoint;
  end
  else if Sender = spdbtnDeleteAnchorPointTool then
  begin
    FPenTool := ptDeleteAnchorPoint;
  end
  else if Sender = spdbtnConvertPointTool then
  begin
    FPenTool := ptConvertPoint;
  end;

  FActivePenTool := FPenTool;
  ShowStatusInfoOnStatusBar();

  if ActiveChildForm <> nil then
  begin
    with ActiveChildForm do
    begin
      case FPenTool of
        ptPathComponentSelection:
          begin
            if Assigned(PathList.SelectedPath) then
            begin
              if Assigned(FCurvePath) then
              begin
                if FCurvePath.CurveSegmentsList.SelectedCount > 0 then
                begin
                  FCurvePath.CurveSegmentsList.IsSelectedAll := True;
                  FCurvePath.CurveSegmentsList.IsSelected    := False;
                end;
              end;
            end;
          end;

        { When switch to the Pen Tool --

          If there is any full selected path, then deselect all paths, and switch
          the path list to Add New Path state; if there is no full selected path,
          but there is a selected path, if the path is closed, then switch the
          path list to Ajustment state, if the path is not closed, then switch the
          path list Add Next Anchor Point state. }
        ptPenTool:
          begin
            if Assigned(PathList.SelectedPath) then
            begin
              if PathList.SelectedPath.CurvePathList.GetWholeSelectedPathsCount() > 0 then
              begin
                PathList.SelectedPath.CurvePathList.DeselectAllPaths();
                PathList.SelectedPath.CurvePathList.Status := cplsAddNewPath;
              end
              else
              begin
                if Assigned(FCurvePath) then
                begin
                  if FCurvePath.CurveSegmentsList.IsClosed then
                  begin
                    PathList.SelectedPath.CurvePathList.Status := cplsAdjust;
                  end
                  else
                  begin
                    PathList.SelectedPath.CurvePathList.Status := cplsAddNextAnchorPoint;
                  end;
                end
                else
                begin
                  PathList.SelectedPath.CurvePathList.Status := cplsAddNewPath;
                end;
              end;
            end;
          end;
      end;

      if Assigned(PathList.SelectedPath) then
      begin
        // clear paths
        PathLayer.Bitmap.Clear($00000000);

        PathList.SelectedPath.CurvePathList.DrawAllPaths(
          PathLayer.Bitmap.Canvas, pmNotXor, imgWorkArea.BitmapToControl);
      end;

      ChangeImageCursorByPenTools();
    end;
  end;
end;

procedure TfrmMain.ChangeMeasureUnit(Sender: TObject);
begin
  UpdateMeasureOptions;
end;

procedure TfrmMain.ClearMeasureInfoClick(Sender: TObject);
begin
  UpdateMeasureOptions;
end;

procedure TfrmMain.ChangeShapeRegionTools(Sender: TObject);
var
  LShapeRegionLayer    : TgmShapeRegionLayer;
  LLastShapeRegionTool : TgmShapeRegionTool;
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  // change Shape Tool button on the main tool bar
  if Sender is TSpeedButton then
  begin
    spdbtnShapeTools.Glyph.Assign(TSpeedButton(Sender).Glyph);
    spdbtnShapeTools.Hint := TSpeedButton(Sender).Hint;
  end;

  LLastShapeRegionTool := FShapeRegionTool;

  if ActiveChildForm <> nil then
  begin
    if Sender = spdbtnShapeMove then
    begin
      ActiveChildForm.ShapeDrawingState := dsNotDrawing;
    end
    else
    begin
      ActiveChildForm.ShapeDrawingState := dsNewFigure;
    end;
  end;

  if Sender = spdbtnShapeMove then
  begin
    FShapeRegionTool := srtMove;
  end
  else if Sender = spdbtnShapeRectangle then
  begin
    FShapeRegionTool := srtRectangle;
  end
  else if Sender = spdbtnShapeRoundRect then
  begin
    FShapeRegionTool := srtRoundedRect;
  end
  else if Sender = spdbtnShapeEllipse then
  begin
    FShapeRegionTool := srtEllipse;
  end
  else if Sender = spdbtnShapeRegularPolygon then
  begin
    FShapeRegionTool := srtPolygon;
  end
  else if Sender = spdbtnShapeLine then
  begin
    FShapeRegionTool := srtLine;
  end;

  UpdateShapeOptions;
  ShowStatusInfoOnStatusBar;

  if ActiveChildForm <> nil then
  begin
    with ActiveChildForm do
    begin
      ChangeImageCursorByShapeTools;

      if FShapeRegionTool = srtMove then
      begin
        if LLastShapeRegionTool <> srtMove then
        begin
          if LayerList.SelectedLayer is TgmShapeRegionLayer then
          begin
            LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
            LShapeRegionLayer.ShapeOutlineList.BackupCoordinates();
           end;
        end;
      end;

      imgWorkArea.Changed();
    end;
  end;
end;

procedure TfrmMain.ChangeRegionCombineMode(Sender: TObject);
begin
  // prevent from the click event execute twice
  if not FCanExecClick then
  begin
    Exit;
  end;

  if Sender = spdbtnAddShape then
  begin
    FRegionCombineMode := rcmAdd;
  end
  else if Sender = spdbtnSubtractShape then
  begin
    FRegionCombineMode := rcmSubtract;
  end
  else if Sender = spdbtnIntersectShape then
  begin
    FRegionCombineMode := rcmIntersect;
  end
  else if Sender = spdbtnExcludeOverlapShape then
  begin
    FRegionCombineMode := rcmExcludeOverlap;
  end;
end;

procedure TfrmMain.edtShapeToolRSWChange(Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnShapeToolRSW.Position := StrToInt(edtShapeToolRSW.Text);

      case FShapeRegionTool of
        srtRoundedRect:
          begin
            FShapeCornerRadius := updwnShapeToolRSW.Position;
          end;

        srtPolygon:
          begin
            FShapePolygonSides := updwnShapeToolRSW.Position;
          end;

        srtLine:
          begin
            FLineWeight := updwnShapeToolRSW.Position;
          end;
      end;

    except
      case FShapeRegionTool of
        srtRoundedRect:
          begin
            edtShapeToolRSW.Text := IntToStr(FShapeCornerRadius);
          end;
          
        srtPolygon:
          begin
            edtShapeToolRSW.Text := IntToStr(FShapePolygonSides);
          end;
          
        srtLine:
          begin
            edtShapeToolRSW.Text := IntToStr(FLineWeight);
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.cmbbxFontFamilyChange(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Name :=
    cmbbxFontFamily.Items[cmbbxFontFamily.ItemIndex];

  frmRichTextEditor.Show;
end;

procedure TfrmMain.shpFontColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dmMain.clrdlgRGB.Color := shpFontColor.Brush.Color;

  if dmMain.clrdlgRGB.Execute then
  begin
    shpFontColor.Brush.Color := dmMain.clrdlgRGB.Color;
    frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Color := shpFontColor.Brush.Color;
    frmRichTextEditor.Show;
  end;
end;

procedure TfrmMain.cmbbxFontSizeChange(Sender: TObject);
var
  LFontSize: Byte;
begin
  case cmbbxFontSize.ItemIndex of
     0:  LFontSize := 6;
     1:  LFontSize := 8;
     2:  LFontSize := 9;
     3:  LFontSize := 10;
     4:  LFontSize := 11;
     5:  LFontSize := 12;
     6:  LFontSize := 14;
     7:  LFontSize := 16;
     8:  LFontSize := 18;
     9:  LFontSize := 20;
    10:  LFontSize := 22;
    11:  LFontSize := 24;
    12:  LFontSize := 26;
    13:  LFontSize := 28;
    14:  LFontSize := 30;
    15:  LFontSize := 36;
    16:  LFontSize := 48;
    17:  LFontSize := 60;
    18:  LFontSize := 72;
  else
    LFontSize := 0;
  end;

  frmRichTextEditor.rchedtRichTextEditor.SelAttributes.Size := LFontSize;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.tlbtnBoldClick(Sender: TObject);
begin
  with frmRichTextEditor.rchedtRichTextEditor.SelAttributes do
  begin
    if tlbtnBold.Down then
    begin
      Style := Style + [fsBold];
    end
    else
    begin
      Style := Style - [fsBold];
    end;
  end;

  frmRichTextEditor.Show;
end;

procedure TfrmMain.tlbtnItalicClick(Sender: TObject);
begin
  with frmRichTextEditor.rchedtRichTextEditor.SelAttributes do
  begin
    if tlbtnItalic.Down then
    begin
      Style := Style + [fsItalic];
    end
    else
    begin
      Style := Style - [fsItalic];
    end;
  end;

  frmRichTextEditor.Show;
end;

procedure TfrmMain.tlbtnUnderlineClick(Sender: TObject);
begin
  with frmRichTextEditor.rchedtRichTextEditor.SelAttributes do
  begin
    if tlbtnUnderline.Down then
    begin
      Style := Style + [fsUnderline];
    end
    else
    begin
      Style := Style - [fsUnderline];
    end;
  end;

  frmRichTextEditor.Show;
end;

procedure TfrmMain.spdbtnLeftAlignTextClick(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment := taLeftJustify;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.spdbtnCenterTextClick(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment := taCenter;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.spdbtnRightAlignTextClick(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.Paragraph.Alignment := taRightJustify;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.spdbtnSelectAllRichTextClick(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.SelectAll;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.spdbtnClearRichTextClick(Sender: TObject);
begin
  frmRichTextEditor.rchedtRichTextEditor.Clear;
  frmRichTextEditor.Show;
end;

procedure TfrmMain.MainToolsDblClick(Sender: TObject);
begin
  if not pnlToolOptions.Visible then
  begin
    pnlToolOptions.Visible := True;
  end;
end;

procedure TfrmMain.imgToolOptionsVisibilityClick(Sender: TObject);
begin
  pnlToolOptions.Visible := not pnlToolOptions.Visible;

  if pnlToolOptions.Visible then
  begin
    imgToolOptionsVisibility.Bitmap.Assign(dmMain.bmp32lstMainTools.Bitmap[IMG_LEFT_TRIPLE_ARROW_INDEX]);
  end
  else
  begin
    imgToolOptionsVisibility.Bitmap.Assign(dmMain.bmp32lstMainTools.Bitmap[IMG_RIGHT_TRIPLE_ARROW_INDEX]);
  end;
end;

procedure TfrmMain.SetCanExecClickMark(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TSpeedButton then
  begin
    FCanExecClick := (not TSpeedButton(Sender).Down);
  end;
end;

procedure TfrmMain.GhostsWakeUp(WakeUp: Boolean);
begin
  if WakeUp then
  begin
    FGhosts := TList.Create;
  end
  else
  begin
    FreeAndNil(FGhosts);
  end;
end;

procedure TfrmMain.GhostsFly;
var
  i : integer;
begin
  if not Assigned(FGhosts) then Exit;
  
  for i := 0 to (FGhosts.Count -1) do
  begin
    TfrmGhost(FGhosts[i]).Fly;
  end;
end;

procedure TfrmMain.GhostsFade(Fade: Boolean);
var
  i : integer;
begin
  if not Assigned(FGhosts) then
  begin
    Exit;
  end;

  for i := 0 to (FGhosts.Count -1) do
  begin
    with TfrmGhost(FGhosts[i]) do
    begin
      if not Untouchable then
      begin
        if Fade then
          Blending
        else
          Opaqueing;
      end;

      Scaring := Fade;
    end;
  end;
end;

procedure TfrmMain.WMWINDOWPOSCHANGING(var Msg: TWMWINDOWPOSCHANGING);
//http://delphi.about.com/od/formsdialogs/a/frm_dock_screen.htm
begin
  GhostsFly;
end;

procedure TfrmMain.DetectMousePos(ASender: TObject);
var
  p              : TPoint;
  LLeftGhostArea : TRect;
  LRightGhostArea: TRect;
begin
  //Now, it will only called when "mouse were really moved". not every time.
  p :=  TgmGhostDetect(ASender).Point;

  // if the mouse over the left ghost area, opaqueing the left ghost form
  with LLeftGhostArea do
  begin
    TopLeft := pnlToolBoxHolder.ClientToScreen( Point(0, 0) );

    BottomRight := pnlToolOptions.ClientToScreen( Point(pnlToolOptions.Width,
                                                        pnlToolOptions.Height) );
  end;

  with LRightGhostArea do
  begin
    TopLeft := pnlRightDockArea.ClientToScreen( Point(0, 0) );

    BottomRight := pnlRightDockArea.ClientToScreen( Point(pnlRightDockArea.Width,
                                                          pnlRightDockArea.Height) );
  end;

  if ( TfrmGhost(FGhosts[LEFT_GHOST_FORM_INDEX]).Scaring ) and
     ( not TfrmGhost(FGhosts[LEFT_GHOST_FORM_INDEX]).Untouchable ) then
  begin
    if Windows.PtInRect(LLeftGhostArea, p) then
    begin
      TfrmGhost(FGhosts[LEFT_GHOST_FORM_INDEX]).Opaqueing;
    end
    else
    begin
      TfrmGhost(FGhosts[LEFT_GHOST_FORM_INDEX]).Blending;
    end;
  end;

  if ( TfrmGhost(FGhosts[RIGHT_GHOST_FORM_INDEX]).Scaring ) and
     ( not TfrmGhost(FGhosts[RIGHT_GHOST_FORM_INDEX]).Untouchable ) then
  begin
    if Windows.PtInRect(LRightGhostArea, p) then
    begin
      TfrmGhost(FGhosts[RIGHT_GHOST_FORM_INDEX]).Opaqueing;
    end
    else
    begin
      TfrmGhost(FGhosts[RIGHT_GHOST_FORM_INDEX]).Blending;
    end;
  end;
end;

procedure TfrmMain.spdbtnGhostClick(Sender: TObject);
begin
  GhostsFade(spdbtnGhost.Down);
end;

procedure TfrmMain.spdbtnUntouchedClick(Sender: TObject);
var
  i: Integer;
begin
  if not Assigned(FGhosts) then
  begin
    Exit;
  end;

  for i := 0 to (FGhosts.Count -1) do
  begin
    with  TfrmGhost(FGhosts[i]) do
    begin
      Untouchable := spdbtnUntouched.Down;

      if Untouchable then
        Blending
      else
        Opaqueing;
    end;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  GhostsFly;
end;

end.
