unit MainDataModule;

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

interface

uses
  SysUtils, Classes, Dialogs, ImgList, Controls, Menus, ActnList,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagicLib }
  gmSelection,
  gmTypes, ExtDlgs;

type
  TgmLayerOrderType = (lotNone,
                       lotBringToFront,
                       lotBringForward,
                       lotSendBackward,
                       lotSendToBack);


  TdmMain = class(TDataModule)
    clrdlgRGB: TColorDialog;
    imglstTools: TImageList;
    svdlgSaveText: TSaveDialog;
    opndlgOpenText: TOpenDialog;
    pmnBrushStyle: TPopupMenu;
    pmnitmClearBrush: TMenuItem;
    pmnitmSolidBrush: TMenuItem;
    pmnitmHorizontalBrush: TMenuItem;
    pmnitmVerticalBrush: TMenuItem;
    pmnitmFDiagonalBrush: TMenuItem;
    pmnitmBDiagonalBrush: TMenuItem;
    pmnitmCrossBrush: TMenuItem;
    pmnitmDiagonalCrossBrush: TMenuItem;
    pmnShapeBrushStyle: TPopupMenu;
    pmnitmShapeSolidBrush: TMenuItem;
    pmnitmShapeHorizontalBrush: TMenuItem;
    pmnitmShapeVerticalBrush: TMenuItem;
    pmnitmShapeFDiagonalBrush: TMenuItem;
    pmnitmShapeBDiagonalBrush: TMenuItem;
    pmnitmShapeCrossBrush: TMenuItem;
    pmnitmShapeDCrossBrush: TMenuItem;
    imglstCommon: TImageList;
    bmp32lstMainTools: TBitmap32List;
    actnlstLayerForm: TActionList;
    actnNewLayer: TAction;
    actnDeleteLayer: TAction;
    actnAddMask: TAction;
    actnSpecialLayers: TAction;
    pmnLayerForm: TPopupMenu;
    mnitmNewBrightnessContrastLayer: TMenuItem;
    actnlstMainMenu: TActionList;
    actnNewBrightnessContrastLayer: TAction;
    pmnChannelOptions: TPopupMenu;
    mnitmDuplicateChannel: TMenuItem;
    mnitmDeleteChannel: TMenuItem;
    actnlstChannelForm: TActionList;
    actnNewAlphaChannel: TAction;
    actnDeleteChannel: TAction;
    actnLoadChannelAsSelection: TAction;
    actnSaveSelectionAsChannel: TAction;
    actnDuplicateChannel: TAction;
    actnMenuDeleteChannel: TAction;
    actnMenuCommitSelection: TAction;
    actnlstToolOptions: TActionList;
    actnToolCommitSelection: TAction;
    actnMenuDeselect: TAction;
    actnToolDeselect: TAction;
    actnMenuDeleteSelection: TAction;
    actnToolDeleteSelection: TAction;
    actnScaleTransformation: TAction;
    actnRotateTransformation: TAction;
    actnDistortTransformation: TAction;
    actnSelectAll: TAction;
    actnInverse: TAction;
    actnFeatherSelection: TAction;
    actnColorRangeSelection: TAction;
    actnFlipHorizontal: TAction;
    actnFlipVertical: TAction;
    actnBrightContrast: TAction;
    mnitmNewInvertLayer: TMenuItem;
    N4: TMenuItem;
    actnNewInvertLayer: TAction;
    mnitmNewThresholdLayer: TMenuItem;
    actnNewThresholdLayer: TAction;
    actnThreshold: TAction;
    mnitmNewPosterizeLayer: TMenuItem;
    actnNewPosterizeLayer: TAction;
    actnPosterize: TAction;
    N5: TMenuItem;
    mnitmNewHueSaturationLayer: TMenuItem;
    actnNewHueSaturationLayer: TAction;
    actnHueSaturation: TAction;
    mnitmNewColorBalanceLayer: TMenuItem;
    actnNewColorBalanceLayer: TAction;
    actnColorBalance: TAction;
    mnitmNewCurvesLayer: TMenuItem;
    actnNewCurvesLayer: TAction;
    actnCurves: TAction;
    mnitmNewLevelsLayer: TMenuItem;
    actnNewLevelsLayer: TAction;
    actnLevels: TAction;
    mnitmNewChannelMixerLayer: TMenuItem;
    actnNewChannelMixerLayer: TAction;
    actnChannelMixer: TAction;
    N6: TMenuItem;
    mnitmNewSolidColorLayer: TMenuItem;
    actnNewSolidColorLayer: TAction;
    mnitmNewGradientMapLayer: TMenuItem;
    actnNewGradientMapLayer: TAction;
    actnGradientMap: TAction;
    mnitmNewGradientFillLayer: TMenuItem;
    actnNewGradientFillLayer: TAction;
    mnitmNewPatternLayer: TMenuItem;
    actnNewPatternLayer: TAction;
    actnDismissTargetPath: TAction;
    actnShapeRegionBrushStyle: TAction;
    actnRegionSolidBrush: TAction;
    actnRegionHorizontalBrush: TAction;
    actnRegionVerticalBrush: TAction;
    actnRegionFDiagonalBrush: TAction;
    actnRegionBDiagonalBrush: TAction;
    actnRegionCrossBrush: TAction;
    actnRegionDiagCrossBrush: TAction;
    actnCommitTextEdits: TAction;
    actnCancelTextEdits: TAction;
    actnOpenRichTextFile: TAction;
    actnSaveRichText: TAction;
    actnSaveRichTextAs: TAction;
    actnLayerLockTransparency: TAction;
    actnSelectAllFigures: TAction;
    actnLockFigures: TAction;
    actnUnlockFigures: TAction;
    actnDeleteFigures: TAction;
    actnFigureProperties: TAction;
    actnSelectFigures: TAction;
    actnCut: TAction;
    actnCopy: TAction;
    actnPaste: TAction;
    actnlstPathForm: TActionList;
    actnCreateNewPath: TAction;
    actnDeleteCurrentPath: TAction;
    actnFillPath: TAction;
    actnStrokePath: TAction;
    actnLoadPathAsSelection: TAction;
    actnDuplicateLayer: TAction;
    actnLayerProperties: TAction;
    actnBringLayerToFront: TAction;
    actnNewFillLayer: TAction;
    actnNewAdjustmentLayer: TAction;
    actnArrangeLayer: TAction;
    actnBringLayerForward: TAction;
    actnSendLayerBackward: TAction;
    actnSendLayerToBack: TAction;
    actnMergeLayerDown: TAction;
    actnMergeVisibleLayers: TAction;
    actnFlattenImage: TAction;
    actnImageSize: TAction;
    actnCanvasSize: TAction;
    actnCrop: TAction;
    actnCommitCrop: TAction;
    actnCancelCrop: TAction;
    actn30dcw: TAction;
    actn45dcw: TAction;
    actn60dcw: TAction;
    actn90dcw: TAction;
    actn180dcw: TAction;
    actn30ducw: TAction;
    actn45ducw: TAction;
    actn60ducw: TAction;
    actn90ducw: TAction;
    actn180ducw: TAction;
    actnRotateArbitrary: TAction;
    actnRotateCanvas: TAction;
    actnAutoLevels: TAction;
    actnDesaturate: TAction;
    actnInvert: TAction;
    actnReplaceColor: TAction;
    actnAdjustments: TAction;
    actnApplyImage: TAction;
    actnOptimalCrop: TAction;
    actnHistogram: TAction;
    actnFill: TAction;
    actnGeneralPreferences: TAction;
    actnNewFile: TAction;
    actnCloseChildForm: TAction;
    actnCloseAllChildForms: TAction;
    actnPrintPreview: TAction;
    actnPrintOptions: TAction;
    actnPageSetup: TAction;
    PrinterSetupDialog: TPrinterSetupDialog;
    actnPrintImage: TAction;
    PrintDialog: TPrintDialog;
    actnExitProgram: TAction;
    actnSaveFile: TAction;
    actnSaveFileAs: TAction;
    actnSaveAll: TAction;
    actnUndoRedoSwitcher: TAction;
    actnStepForeward: TAction;
    actnStepBackward: TAction;
    actnlstHistoryForm: TActionList;
    actnDeleteCurrentState: TAction;
    actnOpenFile: TAction;
    OpenPictureDialog: TOpenPictureDialog;
    actnOpenRecent: TAction;
    actnFiltersUpdate: TAction;
    actnGhostMode: TAction;
    actnGhostSleepingMode: TAction;
    actnChannelForm: TAction;
    actnColorForm: TAction;
    actnHistoryForm: TAction;
    actnInfoForm: TAction;
    actnLayerForm: TAction;
    actnPathForm: TAction;
    actnStatusBar: TAction;
    actnSwatchForm: TAction;
    actnCascadeForms: TAction;
    actnTileForms: TAction;
    actnArrangeIcons: TAction;
    actnTileHorizontally: TAction;
    actnTileVertically: TAction;
    actnAboutForm: TAction;
    actnShowSplash: TAction;
    actnSoftwarePageCH: TAction;
    actnSoftwarePageEN: TAction;
    actnFiltersPageCH: TAction;
    actnFiltersPageEN: TAction;
    actnDownloadPageCH: TAction;
    actnDownloadPageEN: TAction;
    actnDownloadAtSourceForge: TAction;
    OpenSwatchesDialog: TOpenDialog;
    SaveSwatchesDialog: TSaveDialog;
    pmnSwatches: TPopupMenu;
    pmnitmResetSwatches: TMenuItem;
    pmnitmReplaceSwatches: TMenuItem;
    pmnitmSaveChanges: TMenuItem;
    pmnitmSaveSwatchesAs: TMenuItem;
    pmnitmSeparator1: TMenuItem;
    pmnitmNewSwatch: TMenuItem;
    pmnitmRenameSwatch: TMenuItem;
    pmnitmDeleteSwatch: TMenuItem;
    actnlstSwatchesForm: TActionList;
    actnCreateSwatch: TAction;
    actnSwatchOptions: TAction;
    actnResetSwatches: TAction;
    actnReplaceSwatches: TAction;
    actnSaveChangesToSwatches: TAction;
    actnSaveSwatchesAs: TAction;
    actnNewSwatch: TAction;
    actnRenameSwatch: TAction;
    actnDeleteSwatch: TAction;
    procedure ChangeGlobalBrushStyle(Sender: TObject);
    procedure actnNewLayerExecute(Sender: TObject);
    procedure actnNewLayerUpdate(Sender: TObject);
    procedure actnDeleteLayerExecute(Sender: TObject);
    procedure actnDeleteLayerUpdate(Sender: TObject);
    procedure actnAddMaskExecute(Sender: TObject);
    procedure actnAddMaskUpdate(Sender: TObject);
    procedure actnSpecialLayersExecute(Sender: TObject);
    procedure actnSpecialLayersUpdate(Sender: TObject);
    procedure actnNewBrightnessContrastLayerExecute(Sender: TObject);
    procedure actnNewAlphaChannelExecute(Sender: TObject);
    procedure actnNewAlphaChannelUpdate(Sender: TObject);
    procedure actnDeleteChannelExecute(Sender: TObject);
    procedure actnDeleteChannelUpdate(Sender: TObject);
    procedure actnLoadChannelAsSelectionExecute(Sender: TObject);
    procedure actnLoadChannelAsSelectionUpdate(Sender: TObject);
    procedure actnSaveSelectionAsChannelExecute(Sender: TObject);
    procedure actnSaveSelectionAsChannelUpdate(Sender: TObject);
    procedure actnDuplicateChannelExecute(Sender: TObject);
    procedure actnDuplicateChannelUpdate(Sender: TObject);
    procedure actnMenuDeleteChannelExecute(Sender: TObject);
    procedure actnMenuDeleteChannelUpdate(Sender: TObject);
    procedure actnMenuCommitSelectionExecute(Sender: TObject);
    procedure actnMenuCommitSelectionUpdate(Sender: TObject);
    procedure actnToolCommitSelectionExecute(Sender: TObject);
    procedure actnToolCommitSelectionUpdate(Sender: TObject);
    procedure actnMenuDeselectExecute(Sender: TObject);
    procedure actnMenuDeselectUpdate(Sender: TObject);
    procedure actnToolDeselectExecute(Sender: TObject);
    procedure actnToolDeselectUpdate(Sender: TObject);
    procedure actnMenuDeleteSelectionExecute(Sender: TObject);
    procedure actnMenuDeleteSelectionUpdate(Sender: TObject);
    procedure actnToolDeleteSelectionExecute(Sender: TObject);
    procedure actnToolDeleteSelectionUpdate(Sender: TObject);
    procedure actnScaleTransformationExecute(Sender: TObject);
    procedure actnScaleTransformationUpdate(Sender: TObject);
    procedure actnRotateTransformationExecute(Sender: TObject);
    procedure actnRotateTransformationUpdate(Sender: TObject);
    procedure actnDistortTransformationExecute(Sender: TObject);
    procedure actnDistortTransformationUpdate(Sender: TObject);
    procedure actnSelectAllExecute(Sender: TObject);
    procedure actnSelectAllUpdate(Sender: TObject);
    procedure actnInverseExecute(Sender: TObject);
    procedure actnInverseUpdate(Sender: TObject);
    procedure actnFeatherSelectionExecute(Sender: TObject);
    procedure actnFeatherSelectionUpdate(Sender: TObject);
    procedure actnColorRangeSelectionExecute(Sender: TObject);
    procedure actnColorRangeSelectionUpdate(Sender: TObject);
    procedure actnFlipHorizontalExecute(Sender: TObject);
    procedure actnFlipHorizontalUpdate(Sender: TObject);
    procedure actnFlipVerticalExecute(Sender: TObject);
    procedure actnFlipVerticalUpdate(Sender: TObject);
    procedure actnBrightContrastExecute(Sender: TObject);
    procedure actnBrightContrastUpdate(Sender: TObject);
    procedure actnNewInvertLayerExecute(Sender: TObject);
    procedure actnNewThresholdLayerExecute(Sender: TObject);
    procedure actnNewBrightnessContrastLayerUpdate(Sender: TObject);
    procedure actnNewInvertLayerUpdate(Sender: TObject);
    procedure actnNewThresholdLayerUpdate(Sender: TObject);
    procedure actnThresholdExecute(Sender: TObject);
    procedure actnThresholdUpdate(Sender: TObject);
    procedure actnNewPosterizeLayerExecute(Sender: TObject);
    procedure actnNewPosterizeLayerUpdate(Sender: TObject);
    procedure actnPosterizeExecute(Sender: TObject);
    procedure actnPosterizeUpdate(Sender: TObject);
    procedure actnNewHueSaturationLayerExecute(Sender: TObject);
    procedure actnNewHueSaturationLayerUpdate(Sender: TObject);
    procedure actnHueSaturationExecute(Sender: TObject);
    procedure actnHueSaturationUpdate(Sender: TObject);
    procedure actnNewColorBalanceLayerExecute(Sender: TObject);
    procedure actnColorBalanceExecute(Sender: TObject);
    procedure actnColorBalanceUpdate(Sender: TObject);
    procedure actnNewCurvesLayerExecute(Sender: TObject);
    procedure actnNewCurvesLayerUpdate(Sender: TObject);
    procedure actnCurvesExecute(Sender: TObject);
    procedure actnCurvesUpdate(Sender: TObject);
    procedure actnNewLevelsLayerExecute(Sender: TObject);
    procedure actnNewLevelsLayerUpdate(Sender: TObject);
    procedure actnNewColorBalanceLayerUpdate(Sender: TObject);
    procedure actnLevelsExecute(Sender: TObject);
    procedure actnLevelsUpdate(Sender: TObject);
    procedure actnNewChannelMixerLayerExecute(Sender: TObject);
    procedure actnNewChannelMixerLayerUpdate(Sender: TObject);
    procedure actnChannelMixerExecute(Sender: TObject);
    procedure actnChannelMixerUpdate(Sender: TObject);
    procedure actnNewSolidColorLayerExecute(Sender: TObject);
    procedure actnNewSolidColorLayerUpdate(Sender: TObject);
    procedure actnNewGradientMapLayerExecute(Sender: TObject);
    procedure actnNewGradientMapLayerUpdate(Sender: TObject);
    procedure actnGradientMapExecute(Sender: TObject);
    procedure actnGradientMapUpdate(Sender: TObject);
    procedure actnNewGradientFillLayerExecute(Sender: TObject);
    procedure actnNewGradientFillLayerUpdate(Sender: TObject);
    procedure actnNewPatternLayerExecute(Sender: TObject);
    procedure actnNewPatternLayerUpdate(Sender: TObject);
    procedure actnDismissTargetPathExecute(Sender: TObject);
    procedure actnDismissTargetPathUpdate(Sender: TObject);
    procedure actnShapeRegionBrushStyleExecute(Sender: TObject);
    procedure actnRegionSolidBrushExecute(Sender: TObject);
    procedure actnRegionHorizontalBrushExecute(Sender: TObject);
    procedure actnRegionVerticalBrushExecute(Sender: TObject);
    procedure actnRegionFDiagonalBrushExecute(Sender: TObject);
    procedure actnRegionBDiagonalBrushExecute(Sender: TObject);
    procedure actnRegionCrossBrushExecute(Sender: TObject);
    procedure actnRegionDiagCrossBrushExecute(Sender: TObject);
    procedure actnCommitTextEditsExecute(Sender: TObject);
    procedure actnCommitTextEditsUpdate(Sender: TObject);
    procedure actnCancelTextEditsExecute(Sender: TObject);
    procedure actnCancelTextEditsUpdate(Sender: TObject);
    procedure actnOpenRichTextFileExecute(Sender: TObject);
    procedure actnOpenRichTextFileUpdate(Sender: TObject);
    procedure actnSaveRichTextExecute(Sender: TObject);
    procedure actnSaveRichTextUpdate(Sender: TObject);
    procedure actnSaveRichTextAsExecute(Sender: TObject);
    procedure actnSaveRichTextAsUpdate(Sender: TObject);
    procedure actnLayerLockTransparencyExecute(Sender: TObject);
    procedure actnLayerLockTransparencyUpdate(Sender: TObject);
    procedure actnSelectAllFiguresExecute(Sender: TObject);
    procedure actnSelectAllFiguresUpdate(Sender: TObject);
    procedure actnLockFiguresExecute(Sender: TObject);
    procedure actnLockFiguresUpdate(Sender: TObject);
    procedure actnUnlockFiguresExecute(Sender: TObject);
    procedure actnUnlockFiguresUpdate(Sender: TObject);
    procedure actnDeleteFiguresExecute(Sender: TObject);
    procedure actnDeleteFiguresUpdate(Sender: TObject);
    procedure actnFigurePropertiesExecute(Sender: TObject);
    procedure actnFigurePropertiesUpdate(Sender: TObject);
    procedure actnSelectFiguresExecute(Sender: TObject);
    procedure actnSelectFiguresUpdate(Sender: TObject);
    procedure actnCutExecute(Sender: TObject);
    procedure actnCutUpdate(Sender: TObject);
    procedure actnCopyExecute(Sender: TObject);
    procedure actnCopyUpdate(Sender: TObject);
    procedure actnPasteExecute(Sender: TObject);
    procedure actnPasteUpdate(Sender: TObject);
    procedure actnCreateNewPathExecute(Sender: TObject);
    procedure actnCreateNewPathUpdate(Sender: TObject);
    procedure actnDeleteCurrentPathUpdate(Sender: TObject);
    procedure actnDeleteCurrentPathExecute(Sender: TObject);
    procedure actnFillPathExecute(Sender: TObject);
    procedure actnFillPathUpdate(Sender: TObject);
    procedure actnStrokePathExecute(Sender: TObject);
    procedure actnStrokePathUpdate(Sender: TObject);
    procedure actnLoadPathAsSelectionExecute(Sender: TObject);
    procedure actnLoadPathAsSelectionUpdate(Sender: TObject);
    procedure actnDuplicateLayerExecute(Sender: TObject);
    procedure actnDuplicateLayerUpdate(Sender: TObject);
    procedure actnLayerPropertiesExecute(Sender: TObject);
    procedure actnLayerPropertiesUpdate(Sender: TObject);
    procedure actnBringLayerToFrontExecute(Sender: TObject);
    procedure actnBringLayerToFrontUpdate(Sender: TObject);
    procedure actnNewFillLayerExecute(Sender: TObject);
    procedure actnNewFillLayerUpdate(Sender: TObject);
    procedure actnNewAdjustmentLayerExecute(Sender: TObject);
    procedure actnNewAdjustmentLayerUpdate(Sender: TObject);
    procedure actnArrangeLayerExecute(Sender: TObject);
    procedure actnArrangeLayerUpdate(Sender: TObject);
    procedure actnBringLayerForwardExecute(Sender: TObject);
    procedure actnBringLayerForwardUpdate(Sender: TObject);
    procedure actnSendLayerBackwardExecute(Sender: TObject);
    procedure actnSendLayerBackwardUpdate(Sender: TObject);
    procedure actnSendLayerToBackExecute(Sender: TObject);
    procedure actnSendLayerToBackUpdate(Sender: TObject);
    procedure actnMergeLayerDownExecute(Sender: TObject);
    procedure actnMergeLayerDownUpdate(Sender: TObject);
    procedure actnMergeVisibleLayersExecute(Sender: TObject);
    procedure actnMergeVisibleLayersUpdate(Sender: TObject);
    procedure actnFlattenImageExecute(Sender: TObject);
    procedure actnFlattenImageUpdate(Sender: TObject);
    procedure actnImageSizeExecute(Sender: TObject);
    procedure actnImageSizeUpdate(Sender: TObject);
    procedure actnCanvasSizeExecute(Sender: TObject);
    procedure actnCanvasSizeUpdate(Sender: TObject);
    procedure actnCropExecute(Sender: TObject);
    procedure actnCropUpdate(Sender: TObject);
    procedure actnCommitCropExecute(Sender: TObject);
    procedure actnCommitCropUpdate(Sender: TObject);
    procedure actnCancelCropExecute(Sender: TObject);
    procedure actnCancelCropUpdate(Sender: TObject);
    procedure actn30dcwExecute(Sender: TObject);
    procedure actn30dcwUpdate(Sender: TObject);
    procedure actn45dcwExecute(Sender: TObject);
    procedure actn45dcwUpdate(Sender: TObject);
    procedure actn60dcwExecute(Sender: TObject);
    procedure actn60dcwUpdate(Sender: TObject);
    procedure actn90dcwExecute(Sender: TObject);
    procedure actn90dcwUpdate(Sender: TObject);
    procedure actn180dcwExecute(Sender: TObject);
    procedure actn180dcwUpdate(Sender: TObject);
    procedure actn30ducwExecute(Sender: TObject);
    procedure actn30ducwUpdate(Sender: TObject);
    procedure actn45ducwExecute(Sender: TObject);
    procedure actn45ducwUpdate(Sender: TObject);
    procedure actn60ducwExecute(Sender: TObject);
    procedure actn60ducwUpdate(Sender: TObject);
    procedure actn90ducwExecute(Sender: TObject);
    procedure actn90ducwUpdate(Sender: TObject);
    procedure actn180ducwExecute(Sender: TObject);
    procedure actn180ducwUpdate(Sender: TObject);
    procedure actnRotateArbitraryExecute(Sender: TObject);
    procedure actnRotateArbitraryUpdate(Sender: TObject);
    procedure actnRotateCanvasExecute(Sender: TObject);
    procedure actnRotateCanvasUpdate(Sender: TObject);
    procedure actnAutoLevelsExecute(Sender: TObject);
    procedure actnAutoLevelsUpdate(Sender: TObject);
    procedure actnDesaturateExecute(Sender: TObject);
    procedure actnDesaturateUpdate(Sender: TObject);
    procedure actnInvertExecute(Sender: TObject);
    procedure actnInvertUpdate(Sender: TObject);
    procedure actnReplaceColorExecute(Sender: TObject);
    procedure actnReplaceColorUpdate(Sender: TObject);
    procedure actnAdjustmentsExecute(Sender: TObject);
    procedure actnAdjustmentsUpdate(Sender: TObject);
    procedure actnApplyImageExecute(Sender: TObject);
    procedure actnApplyImageUpdate(Sender: TObject);
    procedure actnOptimalCropExecute(Sender: TObject);
    procedure actnOptimalCropUpdate(Sender: TObject);
    procedure actnHistogramExecute(Sender: TObject);
    procedure actnHistogramUpdate(Sender: TObject);
    procedure actnFillExecute(Sender: TObject);
    procedure actnFillUpdate(Sender: TObject);
    procedure actnGeneralPreferencesExecute(Sender: TObject);
    procedure actnNewFileExecute(Sender: TObject);
    procedure actnCloseChildFormExecute(Sender: TObject);
    procedure actnCloseChildFormUpdate(Sender: TObject);
    procedure actnCloseAllChildFormsExecute(Sender: TObject);
    procedure actnCloseAllChildFormsUpdate(Sender: TObject);
    procedure actnPrintPreviewExecute(Sender: TObject);
    procedure actnPrintPreviewUpdate(Sender: TObject);
    procedure actnPrintOptionsExecute(Sender: TObject);
    procedure actnPrintOptionsUpdate(Sender: TObject);
    procedure actnPageSetupExecute(Sender: TObject);
    procedure actnPageSetupUpdate(Sender: TObject);
    procedure actnPrintImageExecute(Sender: TObject);
    procedure actnPrintImageUpdate(Sender: TObject);
    procedure actnExitProgramExecute(Sender: TObject);
    procedure actnSaveFileExecute(Sender: TObject);
    procedure actnSaveFileUpdate(Sender: TObject);
    procedure actnSaveFileAsExecute(Sender: TObject);
    procedure actnSaveFileAsUpdate(Sender: TObject);
    procedure actnSaveAllExecute(Sender: TObject);
    procedure actnSaveAllUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure actnUndoRedoSwitcherExecute(Sender: TObject);
    procedure actnUndoRedoSwitcherUpdate(Sender: TObject);
    procedure actnStepForewardExecute(Sender: TObject);
    procedure actnStepForewardUpdate(Sender: TObject);
    procedure actnStepBackwardExecute(Sender: TObject);
    procedure actnStepBackwardUpdate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actnDeleteCurrentStateExecute(Sender: TObject);
    procedure actnDeleteCurrentStateUpdate(Sender: TObject);
    procedure actnOpenFileExecute(Sender: TObject);
    procedure actnOpenRecentExecute(Sender: TObject);
    procedure actnOpenRecentUpdate(Sender: TObject);
    procedure actnFiltersUpdateExecute(Sender: TObject);
    procedure actnGhostModeExecute(Sender: TObject);
    procedure actnGhostModeUpdate(Sender: TObject);
    procedure actnGhostSleepingModeExecute(Sender: TObject);
    procedure actnGhostSleepingModeUpdate(Sender: TObject);
    procedure actnChannelFormExecute(Sender: TObject);
    procedure actnChannelFormUpdate(Sender: TObject);
    procedure actnColorFormExecute(Sender: TObject);
    procedure actnColorFormUpdate(Sender: TObject);
    procedure actnHistoryFormExecute(Sender: TObject);
    procedure actnHistoryFormUpdate(Sender: TObject);
    procedure actnInfoFormExecute(Sender: TObject);
    procedure actnInfoFormUpdate(Sender: TObject);
    procedure actnLayerFormExecute(Sender: TObject);
    procedure actnLayerFormUpdate(Sender: TObject);
    procedure actnPathFormExecute(Sender: TObject);
    procedure actnPathFormUpdate(Sender: TObject);
    procedure actnStatusBarUpdate(Sender: TObject);
    procedure actnStatusBarExecute(Sender: TObject);
    procedure actnSwatchFormExecute(Sender: TObject);
    procedure actnSwatchFormUpdate(Sender: TObject);
    procedure actnCascadeFormsExecute(Sender: TObject);
    procedure actnCascadeFormsUpdate(Sender: TObject);
    procedure actnTileFormsExecute(Sender: TObject);
    procedure actnTileFormsUpdate(Sender: TObject);
    procedure actnArrangeIconsExecute(Sender: TObject);
    procedure actnArrangeIconsUpdate(Sender: TObject);
    procedure actnTileHorizontallyExecute(Sender: TObject);
    procedure actnTileHorizontallyUpdate(Sender: TObject);
    procedure actnTileVerticallyExecute(Sender: TObject);
    procedure actnTileVerticallyUpdate(Sender: TObject);
    procedure actnAboutFormExecute(Sender: TObject);
    procedure actnShowSplashExecute(Sender: TObject);
    procedure actnSoftwarePageCHExecute(Sender: TObject);
    procedure actnSoftwarePageENExecute(Sender: TObject);
    procedure actnFiltersPageCHExecute(Sender: TObject);
    procedure actnFiltersPageENExecute(Sender: TObject);
    procedure actnDownloadPageCHExecute(Sender: TObject);
    procedure actnDownloadPageENExecute(Sender: TObject);
    procedure actnDownloadAtSourceForgeExecute(Sender: TObject);
    procedure actnCreateSwatchExecute(Sender: TObject);
    procedure actnSwatchOptionsExecute(Sender: TObject);
    procedure actnResetSwatchesExecute(Sender: TObject);
    procedure actnResetSwatchesUpdate(Sender: TObject);
    procedure actnReplaceSwatchesExecute(Sender: TObject);
    procedure actnSaveChangesToSwatchesExecute(Sender: TObject);
    procedure actnSaveChangesToSwatchesUpdate(Sender: TObject);
    procedure actnSaveSwatchesAsExecute(Sender: TObject);
    procedure actnNewSwatchExecute(Sender: TObject);
    procedure actnNewSwatchUpdate(Sender: TObject);
    procedure actnRenameSwatchExecute(Sender: TObject);
    procedure actnRenameSwatchUpdate(Sender: TObject);
    procedure actnDeleteSwatchExecute(Sender: TObject);
    procedure actnDeleteSwatchUpdate(Sender: TObject);
  private
    FLastLayerOrderType : TgmLayerOrderType;
    FSelectionCopy      : TgmSelection;        // for Undo/Redo

{ Menu Commands }
    procedure DoRotation(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
{ Text Tools }
    procedure SaveNamedTextFile;
    procedure SaveNotNamedTextFile;

  public
{ Swatch Form }  
    procedure HideSwatchesPopupMenuItemsForImage;
    procedure HideSwatchesPopupMenuItemsForToolButton;

    property LastLayerOrderType : TgmLayerOrderType read FLastLayerOrderType write FLastLayerOrderType;
    property SelectionCopy      : TgmSelection      read FSelectionCopy;
  end;

var
  dmMain: TdmMain;

const
  IMG_LEFT_TRIPLE_ARROW_INDEX  = 0;
  IMG_RIGHT_TRIPLE_ARROW_INDEX = 1;

implementation

uses
{ Standard }
  Windows,
  Forms,
  Graphics,
  Clipbrd,
  ShellAPI,
{ Externals }
  Preview,
{ GraphicsMagic Lib }
  gmAlphaFuncs,
  gmBrightContrastLayer,
  gmChannelCommands,
  gmChannelManager,
  gmChannelMixerLayer,
  gmChannels,
  gmColorBalanceLayer,
  gmComplexCommands,
  gmCurvesLayer,
  gmFigures,
  gmHistoryCommands,
  gmHueSaturationLayer,
  gmImageProcessFuncs,
  gmIni,
  gmLayers,
  gmLayerCommands,
  gmLevelsLayer,
  gmLevelsTool,
  gmMiscCommandIcons,
  gmPaintFuncs,
  gmPaths,
  gmPathCommands,
  gmPosterizeLayer,
  gmRichTextLayer,
  gmRichTextLayerCommands,
  gmSelectionCommands,
  gmShapeRegionLayer,
  gmShapeRegionLayerCommands,
  gmSolidColorLayer,
  gmSwatches,
  gmThresholdLayer,
  gmVectorLayer,
  gmVectorLayerCommands,
{ GraphicsMagic Forms }
  ChannelForm,
  ChildForm,
  ColorForm,
  HistoryForm,
  InfoForm,
  LayerForm,
  MainForm,
  PathForm,                 
  RichTextEditorForm,
  SplashForm,
  SwatchForm,
{ GraphicsMagic Dialogs }
  AboutDlg,
  ApplyImageDlg,
  BrightnessContrastDlg,
  CanvasSizeDlg,
  ChannelMixerDlg,
  ColorBalanceDlg,
  ColorRangeSelectionDlg,
  ColorSwatchNameDlg,
  CurvesDlg,
  DuplicateChannelDlg,
  DuplicateLayerDlg,
  FeatherSelectionDlg,
  FigurePropertiesDlg,
  FillDlg,
  GradientFillDlg,
  GradientMapDlg,
  HistogramDlg,
  HueSaturationDlg,
  ImageSizeDlg,
  IndexedColorDlg,
  LayerPropertiesDlg,
  LevelsToolDlg,
  NewFileDlg,                  
  PatternFillDlg,
  PosterizeDlg,
  PreferencesDlg,
  PrintOptionsDlg,      
  PrintPreviewDlg,            
  ReplaceColorDlg,
  RotateCanvasDlg,
  SelectFiguresDlg,
  ThresholdDlg;

{$R *.dfm}



procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(FSelectionCopy) then
  begin
    FSelectionCopy.Free();
  end;
end;

// rotating layers and channels
procedure TdmMain.DoRotation(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LRect      : TRect;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;
  
  with ActiveChildForm do
  begin
    // rotate layers
    LayerList.RotateCanvasOfLayers(ADegrees, ADirection, ABackgroundColor);

    // after layers are rotated, we could get the new size of them,
    // then resize the background pattern
    LNewWidth  := LayerList.SelectedLayer.LayerBitmap.Width;
    LNewHeight := LayerList.SelectedLayer.LayerBitmap.Height;

    CheckerboardBmp.SetSize(LNewWidth, LNewHeight);

    DrawCheckerboardPattern( CheckerboardBmp,
      Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

    // change the size of channels
    ChannelManager.RotateChannels(ADegrees, ADirection);

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
  end;
end;

procedure TdmMain.ChangeGlobalBrushStyle(Sender: TObject);
var
  LBmp: TBitmap;
begin
  LBmp := TBitmap.Create;
  try
    if Sender = pmnitmClearBrush then
    begin
      frmMain.GlobalBrushStyle         := bsClear;
      frmMain.imgBrushStyleViewer.Hint := pmnitmClearBrush.Hint;
      
      imglstCommon.GetBitmap(0, LBmp);
    end
    else
    if Sender = pmnitmSolidBrush then
    begin
      frmMain.GlobalBrushStyle         := bsSolid;
      frmMain.imgBrushStyleViewer.Hint := pmnitmSolidBrush.Hint;
      
      imglstCommon.GetBitmap(1, LBmp);
    end
    else
    if Sender = pmnitmHorizontalBrush then
    begin
      frmMain.GlobalBrushStyle         := bsHorizontal;
      frmMain.imgBrushStyleViewer.Hint := pmnitmHorizontalBrush.Hint;
      
      imglstCommon.GetBitmap(2, LBmp);
    end
    else
    if Sender = pmnitmVerticalBrush then
    begin
      frmMain.GlobalBrushStyle         := bsVertical;
      frmMain.imgBrushStyleViewer.Hint := pmnitmVerticalBrush.Hint;
      
      imglstCommon.GetBitmap(3, LBmp);
    end
    else
    if Sender = pmnitmFDiagonalBrush then
    begin
      frmMain.GlobalBrushStyle         := bsFDiagonal;
      frmMain.imgBrushStyleViewer.Hint := pmnitmFDiagonalBrush.Hint;
      
      imglstCommon.GetBitmap(4, LBmp);
    end
    else
    if Sender = pmnitmBDiagonalBrush then
    begin
      frmMain.GlobalBrushStyle         := bsBDiagonal;
      frmMain.imgBrushStyleViewer.Hint := pmnitmBDiagonalBrush.Hint;

      imglstCommon.GetBitmap(5, LBmp);
    end
    else
    if Sender = pmnitmCrossBrush then
    begin
      frmMain.GlobalBrushStyle         := bsCross;
      frmMain.imgBrushStyleViewer.Hint := pmnitmCrossBrush.Hint;
      
      imglstCommon.GetBitmap(6, LBmp);
    end
    else
    if Sender = pmnitmDiagonalCrossBrush then
    begin
      frmMain.GlobalBrushStyle         := bsDiagCross;
      frmMain.imgBrushStyleViewer.Hint := pmnitmDiagonalCrossBrush.Hint;

      imglstCommon.GetBitmap(7, LBmp);
    end;

    frmMain.imgBrushStyleViewer.Picture.Bitmap.Assign(LBmp);
  finally
    LBmp.Free;
  end;
end;

procedure TdmMain.HideSwatchesPopupMenuItemsForImage;
begin
  pmnitmResetSwatches.Visible   := False;
  pmnitmReplaceSwatches.Visible := False;
  pmnitmSaveChanges.Visible     := False;
  pmnitmSaveSwatchesAs.Visible  := False;
  pmnitmSeparator1.Visible      := False;
  pmnitmDeleteSwatch.Visible    := True;
  pmnitmRenameSwatch.Visible    := True;
end;

procedure TdmMain.HideSwatchesPopupMenuItemsForToolButton;
begin
  pmnitmResetSwatches.Visible   := True;
  pmnitmReplaceSwatches.Visible := True;
  pmnitmSaveChanges.Visible     := True;
  pmnitmSaveSwatchesAs.Visible  := True;
  pmnitmSeparator1.Visible      := True;
  pmnitmDeleteSwatch.Visible    := False;
  pmnitmRenameSwatch.Visible    := False;
end;

procedure TdmMain.SaveNamedTextFile;
var
  LRichTextLayer: TgmRichTextLayer;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);

      if LRichTextLayer.TextFileName <> '' then
      begin
        frmRichTextEditor.rchedtRichTextEditor.Lines.SaveToFile(LRichTextLayer.TextFileName);

        frmRichTextEditor.stsbrTextInfo.Panels[0].Text := 'File Name: ' +
          ExtractFileName(LRichTextLayer.TextFileName);
      end
      else
      begin
        SaveNotNamedTextFile();
      end;
    end;
  end;
end;

procedure TdmMain.SaveNotNamedTextFile;
var
  LRichTextLayer      : TgmRichTextLayer;
  LFileName, LExtName : string;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);
      LFileName      := LRichTextLayer.TextFileName;

      if LFileName = '' then
      begin
        svdlgSaveText.FilterIndex := 1;
        svdlgSaveText.FileName    := 'Untitled';
        svdlgSaveText.InitialDir  := ExtractFilePath( ParamStr(0) );
      end
      else
      begin
        LExtName := ExtractFileExt(LFileName);

        if LExtName = '.rtf' then
        begin
          svdlgSaveText.FilterIndex := 1;
        end
        else if LExtName = '.txt' then
        begin
          svdlgSaveText.FilterIndex := 2;
        end;

        svdlgSaveText.FileName   := ExtractFileName(LFileName);
        svdlgSaveText.InitialDir := ExtractFilePath(LFileName);
      end;

      if svdlgSaveText.Execute then
      begin
        LRichTextLayer.TextFileName := svdlgSaveText.Filename;
        LExtName                    := ExtractFileExt(LRichTextLayer.TextFileName);

        // save modified files
        if LExtName = '' then
        begin
          case svdlgSaveText.FilterIndex of
            1:
              begin
                LRichTextLayer.TextFileName := LRichTextLayer.TextFileName + '.rtf';
              end;

            2:
              begin
                LRichTextLayer.TextFileName := LRichTextLayer.TextFileName + '.txt';
              end;
          end;
        end
        else
        begin
          case svdlgSaveText.FilterIndex of
            1: begin
                 if LExtName <> '.rtf' then
                 begin
                   LRichTextLayer.TextFileName := ChangeFileExt(LRichTextLayer.TextFileName, '.rtf');
                 end;
               end;

            2: begin
                 if LExtName <> '.txt' then
                 begin
                   LRichTextLayer.TextFileName := ChangeFileExt(LRichTextLayer.TextFileName, '.txt');
                 end;
               end;
          end;
        end;

        if FileExists(LRichTextLayer.TextFileName) then
        begin
          if MessageDlg('The file is already existed. Do you want to replace it?',
                        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            SaveNamedTextFile;
          end
        end
        else
        begin
          SaveNamedTextFile;
        end;
      end;
    end;
  end;
end;

procedure TdmMain.actnNewLayerExecute(Sender: TObject);
var
  LLayer      : TgmCustomLayer;
  LLayerIndex : Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LLayer      := CreateNormalLayer();
      LLayerIndex := LayerList.SelectedIndex + 1;

      LayerList.Insert(LLayerIndex, LLayer);

      // Undo/Redo
      CreateNewLayerCommand(LLayer, LLayerIndex);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewLayerUpdate(Sender: TObject);
begin
  actnNewLayer.Enabled := Assigned(ActiveChildForm) and
                          (not ActiveChildForm.ToolsInPendingStatus());

end;

procedure TdmMain.actnDeleteLayerExecute(Sender: TObject);
var
  LModalResult : TModalResult;
  LCommand     : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    case LayerList.SelectedLayer.LayerProcessStage of
      lpsLayer:
        begin
          DeleteCurrentLayer();
        end;

      lpsMask:
        begin
          if LayerList.SelectedLayer is TgmNormalLayer then
          begin
            LModalResult := MessageDlg('Apply mask to layer before removing?',
                                        mtConfirmation,
                                        [mbYes, mbNo, mbCancel], 0);

            case LModalResult of
              mrYes:
                begin
                  // create Undo/Redo command first ...
                  LCommand := TgmApplyLayerMaskCommand.Create(ChannelManager,
                    LayerList, LayerList.SelectedIndex);

                  // then apply mask ...
                  TgmNormalLayer(LayerList.SelectedLayer).ApplyMask();

                  CommandManager.AddCommand(LCommand);
                end;

              mrNo:
                begin
                  // create Undo/Redo command first ...
                  LCommand := TgmDeleteLayerMaskCommand.Create(ChannelManager,
                    LayerList, LayerList.SelectedIndex);

                  LayerList.SelectedLayer.DiscardMask();

                  CommandManager.AddCommand(LCommand);
                end;
            end;
          end
          else
          begin
            if MessageDlg('Discard mask?', mtConfirmation,
                          [mbYes, mbNo], 0) = mrYes then
            begin
              // create Undo/Redo command first ...
              LCommand := TgmDeleteLayerMaskCommand.Create(ChannelManager,
                LayerList, LayerList.SelectedIndex);

              LayerList.SelectedLayer.DiscardMask();

              CommandManager.AddCommand(LCommand);
            end;
          end;
        end;
    end;
  end;
end;

procedure TdmMain.actnDeleteLayerUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if LayerList.Count > 0 then
      begin
        if LayerList.Count = 1 then
        begin
          LEnabled := (LayerList.SelectedLayer.LayerProcessStage = lpsMask);
        end
        else
        begin
          LEnabled := (LayerList.SelectedIndex >= 0);
        end;
      end;

      actnDeleteLayer.Enabled := LEnabled and (not ToolsInPendingStatus()); 
    end;
  end
  else
  begin
    actnDeleteLayer.Enabled := False;
  end;
end;

procedure TdmMain.actnAddMaskExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    LayerList.SelectedLayer.EnableMask();

    // Undo/Redo
    LCommand := TgmAddLayerMaskCommand.Create(ChannelManager, LayerList,
                                              LayerList.SelectedIndex);
    CommandManager.AddCommand(LCommand);
  end;  
end;

procedure TdmMain.actnAddMaskUpdate(Sender: TObject);
begin
  actnAddMask.Enabled := Assigned(ActiveChildForm) and
                         Assigned(ActiveChildForm.LayerList.SelectedLayer) and
                         (not ActiveChildForm.LayerList.SelectedLayer.IsMaskEnabled) and
                         (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnSpecialLayersExecute(Sender: TObject);
var
  LPoint : TPoint;
begin
  GetCursorPos(LPoint); // get cursor position on the screen

  // pop up the menu at current position
  pmnLayerForm.Popup(LPoint.X, LPoint.Y);
end;

procedure TdmMain.actnSpecialLayersUpdate(Sender: TObject);
begin
  actnSpecialLayers.Enabled := Assigned(ActiveChildForm) and
                               (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewBrightnessContrastLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateBrightContrastLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmBrightnessContrast := TfrmBrightnessContrast.Create(Application);
      try
        frmBrightnessContrast.AssociateToBrightContrastLayer( TgmBrightContrastLayer(LLayer) );

        case frmBrightnessContrast.ShowModal() of
          mrOK:
            begin
              if not frmBrightnessContrast.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmBrightnessContrast);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewBrightnessContrastLayerUpdate(Sender: TObject);
begin
  actnNewBrightnessContrastLayer.Enabled := Assigned(ActiveChildForm) and
                                            (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewAlphaChannelExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      ChannelManager.AddNewAlphaChannel(
        imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

      // Undo/Redo
      LCommand := TgmNewAlphaChannelCommand.Create(ChannelManager,
        imgWorkArea.Bitmap.Width, imgWorkArea.Bitmap.Height);

      CommandManager.AddCommand(LCommand);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewAlphaChannelUpdate(Sender: TObject);
begin
  actnNewAlphaChannel.Enabled := Assigned(ActiveChildForm) and
                                 (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnDeleteChannelExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            // Undo/Redo first
            LCommand := TgmDeleteAlphaChannelCommand.Create(ChannelManager,
              ChannelManager.AlphaChannelList.SelectedIndex);

            ChannelManager.DeleteSelectedAlphaChannels();
            CommandManager.AddCommand(LCommand);
          end;

        ctLayerMaskChannel:
          begin
            // Here, we delete the layer mask channel with the following
            // action. It will get the layer mask channel deleted.
            actnDeleteLayerExecute(Sender);
          end;

        ctQuickMaskChannel:
          begin
            ChannelManager.DeleteQuickMaskChannel;
          end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnDeleteChannelUpdate(Sender: TObject);
begin
  actnDeleteChannel.Enabled := Assigned(ActiveChildForm) and
                               (ActiveChildForm.ChannelManager.CurrentChannelType <> ctColorChannel) and
                               (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnLoadChannelAsSelectionExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LCommandName  := 'Load Selection';
  LCommand      := nil;
  LOldSelection := nil;

  with ActiveChildForm do
  begin
    // for Undo/Redo
    if Assigned(Selection) then
    begin
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);
    end;

    LoadChannelAsSelection();

    // Undo/Redo
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmNewSelectionOnAlphaChannelCommand.Create(
            LCommandName, ChannelManager, LOldSelection, Selection,
            ChannelManager.AlphaChannelList.SelectedIndex,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmNewSelectionOnQuickMaskChannelCommand.Create(
            LCommandName, ChannelManager, LOldSelection, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmNewSelectionOnLayerMaskCommand.Create(LCommandName,
            ChannelManager, LayerList, LOldSelection, Selection,
            LayerList.SelectedIndex, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmNewSelectionOnLayerCommand.Create(LCommandName,
            ChannelManager, LayerList, LOldSelection, Selection,
            LayerList.SelectedIndex, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    if Assigned(LOldSelection) then
    begin
      LOldSelection.Free();
    end;
  end;
end;

procedure TdmMain.actnLoadChannelAsSelectionUpdate(Sender: TObject);
begin
  actnLoadChannelAsSelection.Enabled := Assigned(ActiveChildForm) and
                                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnSaveSelectionAsChannelExecute(Sender: TObject);
var
  LAlphaChannel : TgmAlphaChannel;
  LChannelBmp   : TBitmap32;
  LCommand      : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    LChannelBmp := TBitmap32.Create();
    try
      LChannelBmp.Assign(Selection.OriginalMask);

      // The OriginalMask of selection may contain Windows Color,
      // so we need restore the pixels on it to opaque.
      ReplaceAlphaChannelWithNewValue(LChannelBmp, 255);
      
      ChannelManager.AddNewAlphaChannel(LChannelBmp);
    finally
      LChannelBmp.Free();
    end;

    // update the thumbnail of the newly created alpha channel
    with ChannelManager.AlphaChannelList do
    begin
      LAlphaChannel := TgmAlphaChannel(Channels[MaxIndex]);
      LAlphaChannel.UpdateChannelThumbnail();
    end;

    // Undo/Redo
    LCommand := TgmSaveSelectionAsChannelCommand.Create(ChannelManager,
      LAlphaChannel, ChannelManager.AlphaChannelList.MaxIndex);

    CommandManager.AddCommand(LCommand);  
  end;
end;

procedure TdmMain.actnSaveSelectionAsChannelUpdate(Sender: TObject);
begin
  actnSaveSelectionAsChannel.Enabled := Assigned(ActiveChildForm) and
                                        Assigned(ActiveChildForm.Selection) and
                                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnDuplicateChannelExecute(Sender: TObject);
var
  LChannelPanel : TgmCustomChannel;
  LChannelMap   : TBitmap32;
  LChannel      : TgmChannelSelector;
  LCommand      : TgmCustomCommand;
begin
  LChannelPanel := nil;
  LChannel      := csRed;

  with ActiveChildForm do
  begin
    case ChannelManager.CurrentChannelType of
      ctColorChannel:
        begin
          if csRed in ChannelManager.SelectedColorChannels then
          begin
            LChannelPanel := ChannelManager.ColorChannelList.Channels[1];
            LChannel      := csRed;
          end
          else if csGreen in ChannelManager.SelectedColorChannels then
          begin
            LChannelPanel := ChannelManager.ColorChannelList.Channels[2];
            LChannel      := csGreen;
          end
          else if csBlue in ChannelManager.SelectedColorChannels then
          begin
            LChannelPanel := ChannelManager.ColorChannelList.Channels[3];
            LChannel      := csBlue;
          end;
        end;

      ctAlphaChannel:
        begin
          LChannelPanel := ChannelManager.SelectedAlphaChannel;
          LChannel      := csGrayscale;
        end;

      ctLayerMaskChannel:
        begin
          LChannelPanel := ChannelManager.LayerMaskChannel;
          LChannel      := csGrayscale;
        end;

      ctQuickMaskChannel:
        begin
          LChannelPanel := ChannelManager.QuickMaskChannel;
          LChannel      := csGrayscale;
        end;
    end;

    frmDuplicateChannel := TfrmDuplicateChannel.Create(Application);
    try
      frmDuplicateChannel.FormSetup(LChannelPanel);

      if frmDuplicateChannel.ShowModal = mrOK then
      begin
        LChannelMap := nil;

        case ChannelManager.CurrentChannelType of
          ctColorChannel:
            begin
              LChannelMap := GetChannelMap(LayerList.CombineResult,
                LChannel, frmDuplicateChannel.chckbxInvertChannel.Checked);
            end;

          ctAlphaChannel:
            begin
              LChannelMap := GetChannelMap(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                LChannel, frmDuplicateChannel.chckbxInvertChannel.Checked);
            end;

          ctLayerMaskChannel:
            begin
              LChannelMap := GetChannelMap(
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap,
                LChannel, frmDuplicateChannel.chckbxInvertChannel.Checked);
            end;

          ctQuickMaskChannel:
            begin
              LChannelMap := GetChannelMap(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                LChannel, frmDuplicateChannel.chckbxInvertChannel.Checked);
            end;
        end;

        if Assigned(LChannelMap) then
        begin
          ChannelManager.AddNewAlphaChannel(imgWorkArea.Bitmap.Width,
                                            imgWorkArea.Bitmap.Height);

          with ChannelManager.SelectedAlphaChannel do
          begin
            ChannelName := frmDuplicateChannel.edtDuplicateChannelAs.Text;
            
            ChannelLayer.Bitmap.Draw(0, 0, LChannelMap);
            UpdateChannelThumbnail();
          end;

          // We have to change the selection target after
          // the new channel layer setup.
          if Assigned(Selection) then
          begin
            ChangeSelectionTarget();
          end;

          LChannelMap.Free();

          // Undo/Redo
          LCommand := TgmDuplicateChannelCommand.Create(ChannelManager,
            ChannelManager.AlphaChannelList.SelectedIndex);

          CommandManager.AddCommand(LCommand);
        end;
      end;
    finally
      FreeAndNil(frmDuplicateChannel);
    end;
  end;
end;

procedure TdmMain.actnDuplicateChannelUpdate(Sender: TObject);
begin
  // we can only duplicate single channel ...
  actnDuplicateChannel.Enabled := Assigned(ActiveChildForm) and
                                  (not ActiveChildForm.ChannelManager.ColorChannelList.Channels[0].IsSelected) and
                                  (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnMenuDeleteChannelExecute(Sender: TObject);
begin
  actnDeleteChannelExecute(Sender);
end;

procedure TdmMain.actnMenuDeleteChannelUpdate(Sender: TObject);
begin
  actnMenuDeleteChannel.Enabled := Assigned(ActiveChildForm) and
                                   (ActiveChildForm.ChannelManager.CurrentChannelType <> ctColorChannel) and
                                   (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnMenuCommitSelectionExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmCommitSelectionOnAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmCommitSelectionOnQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmCommitSelectionOnLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmCommitSelectionOnLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then commit the selection
    CommitSelection();
  end;
end;

procedure TdmMain.actnMenuCommitSelectionUpdate(Sender: TObject);
begin
  actnMenuCommitSelection.Enabled := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.Selection) and
                                     (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnToolCommitSelectionExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmCommitSelectionOnAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmCommitSelectionOnQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmCommitSelectionOnLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmCommitSelectionOnLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then commit the selection
    CommitSelection();
  end;
end;

procedure TdmMain.actnToolCommitSelectionUpdate(Sender: TObject);
begin
  actnToolCommitSelection.Enabled := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.Selection) and
                                     (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnMenuDeselectExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;

  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmCancelSelectionFromAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmCancelSelectionFromQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmCancelSelectionFromLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmCancelSelectionFromLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then, cancelling the selection
    CancelSelection();
  end;
end;

procedure TdmMain.actnMenuDeselectUpdate(Sender: TObject);
begin
  actnMenuDeselect.Enabled := Assigned(ActiveChildForm) and
                              Assigned(ActiveChildForm.Selection) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnToolDeselectExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;

  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmCancelSelectionFromAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmCancelSelectionFromQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmCancelSelectionFromLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmCancelSelectionFromLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then, cancelling the selection
    CancelSelection();
  end;
end;

procedure TdmMain.actnToolDeselectUpdate(Sender: TObject);
begin
  actnToolDeselect.Enabled := Assigned(ActiveChildForm) and
                              Assigned(ActiveChildForm.Selection) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnMenuDeleteSelectionExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmDeleteSelectionFromAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmDeleteSelectionFromQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmDeleteSelectionFromLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmDeleteSelectionFromLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then delete the selection
    DeleteSelection();
  end;
end;

procedure TdmMain.actnMenuDeleteSelectionUpdate(Sender: TObject);
begin
  actnMenuDeleteSelection.Enabled := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.Selection) and
                                     (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnToolDeleteSelectionExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmDeleteSelectionFromAlphaChannelCommand.Create(
            ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
            Selection, GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmDeleteSelectionFromQuickMaskChannelCommand.Create(
            ChannelManager, Selection, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmDeleteSelectionFromLayerMaskCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmDeleteSelectionFromLayerCommand.Create(
            ChannelManager, LayerList, LayerList.SelectedIndex, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    // then delete the selection
    DeleteSelection();
  end;
end;

procedure TdmMain.actnToolDeleteSelectionUpdate(Sender: TObject);
begin
  actnToolDeleteSelection.Enabled := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.Selection) and
                                     (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnScaleTransformationExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    if Assigned(SelectionTransformation) then
    begin
      if SelectionTransformation.TransformMode <> tmScale then
      begin
        FinishTransformation();
      end
      else
      begin
        Exit;
      end;
    end
    else
    begin
      CreateSelectionTransformation(tmScale);
      ConnectTransformMouseEvents();

      // for Undo/Redo
      // this is for Undo/Redo of Apply/Cancel transform
      if not Assigned(FSelectionCopy) then
      begin
        FSelectionCopy := TgmSelection.Create();
      end;
      FSelectionCopy.AssignAllSelectionData(Selection);

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(
              ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
              tmScale, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(
              ChannelManager, tmScale, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerMaskCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmScale,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmScale,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        CommandManager.AddCommand(LCommand);
      end;
    end;

    // update view for showing control border
    imgWorkArea.Changed();
  end;
end;

procedure TdmMain.actnScaleTransformationUpdate(Sender: TObject);
begin
  actnScaleTransformation.Enabled := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.Selection) and
                                     (ActiveChildForm.Crop = nil) and
                                     ( (ActiveChildForm.ChannelManager.CurrentChannelType <> ctColorChannel) or
                                       (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) );

  actnScaleTransformation.Checked := Assigned(ActiveChildForm) and
                                     Assigned(ActiveChildForm.SelectionTransformation) and
                                     (ActiveChildForm.SelectionTransformation.TransformMode = tmScale);
end;

procedure TdmMain.actnRotateTransformationExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    if Assigned(SelectionTransformation) then
    begin
      if SelectionTransformation.TransformMode <> tmRotate then
      begin
        FinishTransformation();
      end
      else
      begin
        Exit;
      end;
    end
    else
    begin
      CreateSelectionTransformation(tmRotate);
      ConnectTransformMouseEvents();

      // for Undo/Redo
      // this is for Undo/Redo of Apply/Cancel transform
      if not Assigned(FSelectionCopy) then
      begin
        FSelectionCopy := TgmSelection.Create();
      end;
      FSelectionCopy.AssignAllSelectionData(Selection);

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(
              ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
              tmRotate, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(
              ChannelManager, tmRotate, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerMaskCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmRotate,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmRotate,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        CommandManager.AddCommand(LCommand);
      end;
    end;

    // update view for showing control border
    imgWorkArea.Changed();
  end;
end;

procedure TdmMain.actnRotateTransformationUpdate(Sender: TObject);
begin
  actnRotateTransformation.Enabled := Assigned(ActiveChildForm) and
                                      Assigned(ActiveChildForm.Selection) and
                                      (ActiveChildForm.Crop = nil) and
                                      ( (ActiveChildForm.ChannelManager.CurrentChannelType <> ctColorChannel) or
                                        (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) );

  actnRotateTransformation.Checked := Assigned(ActiveChildForm) and
                                      Assigned(ActiveChildForm.SelectionTransformation) and
                                      (ActiveChildForm.SelectionTransformation.TransformMode = tmRotate);
end;

procedure TdmMain.actnDistortTransformationExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    if Assigned(SelectionTransformation) then
    begin
      if SelectionTransformation.TransformMode <> tmDistort then
      begin
        FinishTransformation();
      end
      else
      begin
        Exit;
      end;
    end
    else
    begin
      CreateSelectionTransformation(tmDistort);
      ConnectTransformMouseEvents();

      // for Undo/Redo
      // this is for Undo/Redo of Apply/Cancel transform
      if not Assigned(FSelectionCopy) then
      begin
        FSelectionCopy := TgmSelection.Create();
      end;
      FSelectionCopy.AssignAllSelectionData(Selection);

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(
              ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
              tmDistort, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(
              ChannelManager, tmDistort, Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerMaskCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmDistort,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmEnterSelectionTransformModeForLayerCommand.Create(
              ChannelManager, LayerList, LayerList.SelectedIndex, tmDistort,
              Selection, GetSelectionForUndoRedo,
              EnterSelectionTransformModeForUndoRedo,
              ExitSelectionTransfromModeForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        CommandManager.AddCommand(LCommand);
      end;
    end;

    // update view for showing control border
    imgWorkArea.Changed();
  end;
end;

procedure TdmMain.actnDistortTransformationUpdate(Sender: TObject);
begin
  actnDistortTransformation.Enabled := Assigned(ActiveChildForm) and
                                       Assigned(ActiveChildForm.Selection) and
                                       (ActiveChildForm.Crop = nil) and
                                       ( (ActiveChildForm.ChannelManager.CurrentChannelType <> ctColorChannel) or
                                         (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) );

  actnDistortTransformation.Checked := Assigned(ActiveChildForm) and
                                       Assigned(ActiveChildForm.SelectionTransformation) and
                                       (ActiveChildForm.SelectionTransformation.TransformMode = tmRotate);
end;

procedure TdmMain.actnSelectAllExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LCommand      := nil;
  LOldSelection := nil;

  if frmRichTextEditor.Visible then
  begin
    frmRichTextEditor.rchedtRichTextEditor.SelectAll();
    frmRichTextEditor.Show();
  end
  else
  begin
    LCommandName := 'Select Canvas';

    with ActiveChildForm do
    begin
      // for Undo/Redo
      if Assigned(Selection) then
      begin
        LOldSelection := TgmSelection.Create();
        LOldSelection.AssignAllSelectionData(Selection);
      end;

      CreateSelectionForAll();

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmNewSelectionOnAlphaChannelCommand.Create(
              LCommandName, ChannelManager, LOldSelection, Selection,
              ChannelManager.AlphaChannelList.SelectedIndex,
              GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmNewSelectionOnQuickMaskChannelCommand.Create(
              LCommandName, ChannelManager, LOldSelection, Selection,
              GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmNewSelectionOnLayerMaskCommand.Create(
              LCommandName, ChannelManager, LayerList,
              LOldSelection, Selection, LayerList.SelectedIndex,
              GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmNewSelectionOnLayerCommand.Create(
              LCommandName, ChannelManager, LayerList,
              LOldSelection, Selection, LayerList.SelectedIndex,
              GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        CommandManager.AddCommand(LCommand);
      end;

      if Assigned(LOldSelection) then
      begin
        LOldSelection.Free();
      end;
    end;
  end;
end;

procedure TdmMain.actnSelectAllUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if frmRichTextEditor.Visible then
  begin
    LEnabled := (frmRichTextEditor.rchedtRichTextEditor.Lines.Count > 0);
  end
  else
  begin
    if Assigned(ActiveChildForm) then
    begin
      if Assigned(ActiveChildForm.Selection) then
      begin
        if (not ActiveChildForm.Selection.IsTranslated) and
           (not ActiveChildForm.Selection.IsCornerStretched) and
           (not ActiveChildForm.Selection.IsHorizFlipped) and
           (not ActiveChildForm.Selection.IsVertFlipped) then
        begin
          LEnabled := True;
        end;
      end
      else
      begin
        LEnabled := True;
      end;

      LEnabled := LEnabled and (not ActiveChildForm.ToolsInPendingStatus());
    end;
  end;

  actnSelectAll.Enabled := LEnabled;
end;

procedure TdmMain.actnInverseExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LOldSelection : TgmSelection;
begin
  LCommand := nil;

  with ActiveChildForm do
  begin
    if Assigned(Selection) then
    begin
      // for Undo/Redo
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);

      MakeSelectionInverse();

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
              satInverse, ChannelManager,
              ChannelManager.AlphaChannelList.SelectedIndex,
              LOldSelection, Selection, GetSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
              satInverse, ChannelManager, LOldSelection, Selection,
              GetSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
              satInverse, ChannelManager, LayerList, LayerList.SelectedIndex,
              LOldSelection, Selection, GetSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
              satInverse, ChannelManager, LayerList, LayerList.SelectedIndex,
              LOldSelection, Selection, GetSelectionForUndoRedo);
          end;
      end;

      if Assigned(LCommand) then
      begin
        CommandManager.AddCommand(LCommand);
      end;

      LOldSelection.Free();
    end;
  end;
end;

procedure TdmMain.actnInverseUpdate(Sender: TObject);
begin
  actnInverse.Enabled := Assigned(ActiveChildForm) and
                         (not ActiveChildForm.ToolsInPendingStatus()) and 
                         Assigned(ActiveChildForm.Selection) and
                         (not ActiveChildForm.Selection.IsTranslated) and
                         (not ActiveChildForm.Selection.IsCornerStretched) and
                         (not ActiveChildForm.Selection.IsHorizFlipped) and
                         (not ActiveChildForm.Selection.IsVertFlipped);
end;

procedure TdmMain.actnFeatherSelectionExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LOldSelection : TgmSelection;
begin
  LCommand := nil;
  
  with ActiveChildForm do
  begin
    Selection.IsAnimated := False;
    
    frmFeatherSelection := TfrmFeatherSelection.Create(nil);
    try
      if frmFeatherSelection.ShowModal = mrOK then
      begin
        if Assigned(Selection) then
        begin
          if (Selection.IsTranslated      = False) and
             (Selection.IsCornerStretched = False) and
             (Selection.IsHorizFlipped    = False) and
             (Selection.IsVertFlipped     = False) then
          begin
            Screen.Cursor := crHourGlass;
            LOldSelection := TgmSelection.Create();  // for Undo/Redo
            try
              // for Undo/Redo
              LOldSelection.AssignAllSelectionData(Selection);

              // do feathering
              MakeSelectionFeather(frmFeatherSelection.FeatherRadius);

              // Undo/Redo
              case ChannelManager.CurrentChannelType of
                ctAlphaChannel:
                  begin
                    LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
                      satFeather, ChannelManager,
                      ChannelManager.AlphaChannelList.SelectedIndex,
                      LOldSelection, Selection, GetSelectionForUndoRedo);
                  end;

                ctQuickMaskChannel:
                  begin
                    LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
                      satFeather, ChannelManager, LOldSelection, Selection,
                      GetSelectionForUndoRedo);
                  end;

                ctLayerMaskChannel:
                  begin
                    LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
                      satFeather, ChannelManager, LayerList,
                      LayerList.SelectedIndex, LOldSelection, Selection,
                      GetSelectionForUndoRedo);
                  end;

                ctColorChannel:
                  begin
                    LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
                      satFeather, ChannelManager, LayerList,
                      LayerList.SelectedIndex, LOldSelection, Selection,
                      GetSelectionForUndoRedo);
                  end;
              end;

              if Assigned(LCommand) then
              begin
                CommandManager.AddCommand(LCommand);
              end;
            finally
              LOldSelection.Free();
              Screen.Cursor := crDefault;
            end;
          end;

          if not Selection.IsAnimated then
          begin
            Selection.IsAnimated := True;
          end;
        end;
      end;
    
    finally
      FreeAndNil(frmFeatherSelection);
    end;
  end;
end;

procedure TdmMain.actnFeatherSelectionUpdate(Sender: TObject);
begin
  actnFeatherSelection.Enabled := Assigned(ActiveChildForm) and
                                  (not ActiveChildForm.ToolsInPendingStatus()) and 
                                  Assigned(ActiveChildForm.Selection) and
                                  (not ActiveChildForm.Selection.IsTranslated) and
                                  (not ActiveChildForm.Selection.IsCornerStretched) and
                                  (not ActiveChildForm.Selection.IsHorizFlipped) and
                                  (not ActiveChildForm.Selection.IsVertFlipped);
end;

procedure TdmMain.actnColorRangeSelectionExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LCommand      := nil;
  LOldSelection := nil;
  LCommandName  := 'Color Range';

  with ActiveChildForm do
  begin
    // for Undo/Redo
    if Assigned(Selection) then
    begin
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);
    end;

    frmColorRangeSelection := TfrmColorRangeSelection.Create(Application);
    try
      if frmColorRangeSelection.ShowModal = mrOK then
      begin
        Screen.Cursor := crHourGlass;
        try
          if Assigned(Selection) then
          begin
            Selection.IsAnimated := False;
          end;

          CreateSelectionByColorRange(
            frmColorRangeSelection.SourceBitmap,
            frmColorRangeSelection.SampledColor,
            frmColorRangeSelection.Fuzziness);

          if Assigned(Selection) then
          begin
            Selection.IsAnimated := True;
            imgWorkArea.Changed();
          end;

          // Undo/Redo
          case ChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmNewSelectionOnAlphaChannelCommand.Create(
                  LCommandName, ChannelManager, LOldSelection, Selection,
                  ChannelManager.AlphaChannelList.SelectedIndex,
                  GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmNewSelectionOnQuickMaskChannelCommand.Create(
                  LCommandName, ChannelManager, LOldSelection, Selection,
                  GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmNewSelectionOnLayerMaskCommand.Create(
                  LCommandName, ChannelManager, LayerList,
                  LOldSelection, Selection, LayerList.SelectedIndex,
                  GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmNewSelectionOnLayerCommand.Create(
                  LCommandName, ChannelManager, LayerList,
                  LOldSelection, Selection, LayerList.SelectedIndex,
                  GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
              end;
          end;

          if Assigned(LCommand) then
          begin
            CommandManager.AddCommand(LCommand);
          end;

          if Assigned(LOldSelection) then
          begin
            LOldSelection.Free();
          end;

        finally
          Screen.Cursor := crDefault;
        end;
      end;
    finally
      FreeAndNil(frmColorRangeSelection);
    end;
  end;
end;

procedure TdmMain.actnColorRangeSelectionUpdate(Sender: TObject);
var
  LEnabled : Boolean;
  LCount   : Integer;
begin
  LEnabled := Assigned(ActiveChildForm);

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if ChannelManager.CurrentChannelType = ctAlphaChannel then
      begin
        // if there are more than one alpha channels were selected
        if ChannelManager.SelectedAlphaChannel = nil then
        begin
          LEnabled := False;
        end;
      end
      else if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        // it can only work with full color channels or a single color channel

        LCount := ChannelManager.ColorChannelList.SelectedChannelCount;

        if (LCount > 1) and (LCount < 3) then
        begin
          LEnabled := False;
        end;
      end;

      if Assigned(ActiveChildForm.Selection) then
      begin
        if ActiveChildForm.Selection.IsTranslated or
           ActiveChildForm.Selection.IsCornerStretched or
           ActiveChildForm.Selection.IsHorizFlipped or
           ActiveChildForm.Selection.IsVertFlipped then
        begin
          LEnabled := False;
        end;
      end;

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;


  actnColorRangeSelection.Enabled := LEnabled;
end;

procedure TdmMain.actnFlipHorizontalExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LCommand     := nil;
  LCommandName := 'Flip Horizontal';

  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if Assigned(Selection) then
    begin
      // for Undo/Redo
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);

      Selection.IsAnimated := False;
      Selection.FlipSelection(fmHorizontal);
      ShowProcessedSelection();
      Selection.IsAnimated := True;

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
              satFlipHorizontally,
              ChannelManager,
              ChannelManager.AlphaChannelList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
              satFlipHorizontally,
              ChannelManager,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
              satFlipHorizontally,
              ChannelManager,
              LayerList,
              LayerList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
              satFlipHorizontally,
              ChannelManager,
              LayerList,
              LayerList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;
      end;

      if Assigned(LOldSelection) then
      begin
        LOldSelection.Free();
      end;
    end
    else
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

            FlipBitmap(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap, fmHorizontal);
            ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();

            // Undo/Redo
            LCommand := TgmAlphaChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              ChannelManager.AlphaChannelList,
              ChannelManager.AlphaChannelList.SelectedIndex);
          end;

        ctQuickMaskChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

            FlipBitmap(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, fmHorizontal);
            ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();

            // for Undo/Redo
            LCommand := TgmQuickMaskChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              ChannelManager);
          end;

        ctLayerMaskChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);

            FlipBitmap(LayerList.SelectedLayer.MaskBitmap, fmHorizontal);

            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                0, 0, LayerList.SelectedLayer.MaskBitmap);
            end;

            LayerList.SelectedLayer.Changed();

            // Undo/Redo
            LCommand := TgmLayerMaskProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              LayerList.SelectedLayer.MaskBitmap,
              LayerList,
              LayerList.SelectedIndex);
          end;

        ctColorChannel:
          begin
            if LayerList.SelectedLayer is TgmNormalLayer then
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);

              FlipBitmap(LayerList.SelectedLayer.LayerBitmap, fmHorizontal);
              LayerList.SelectedLayer.Changed();

              // Undo/Redo
              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.LayerBitmap,
                LayerList,
                LayerList.SelectedIndex);
            end;
          end;
      end;
    end;

    // updating thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TdmMain.actnFlipHorizontalUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        LEnabled := (not ToolsInPendingStatus());

        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          // we cannot do flip on special layers
          LEnabled := LEnabled and (LayerList.SelectedLayer is TgmNormalLayer);
        end;
      end
      else
      begin
        LEnabled := (not ToolsInPendingStatus());
                    
        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          // we cannot do flip on special layers and can only work on full RGB channel
          LEnabled := LEnabled and
                      (LayerList.SelectedLayer is TgmNormalLayer) and
                      (ChannelManager.ColorChannelList.SelectedChannelCount > 2);
        end;
      end;
    end;
  end;

  actnFlipHorizontal.Enabled := LEnabled;
end;

procedure TdmMain.actnFlipVerticalExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LCommand     := nil;
  LCommandName := 'Flip Vertical';

  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if Assigned(Selection) then
    begin
      // for Undo/Redo
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);

      Selection.IsAnimated := False;
      Selection.FlipSelection(fmVertical);
      ShowProcessedSelection();
      Selection.IsAnimated := True;

      // Undo/Redo
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
              satFlipVertically,
              ChannelManager,
              ChannelManager.AlphaChannelList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctQuickMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
              satFlipVertically,
              ChannelManager,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctLayerMaskChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerMaskCommand.Create(
              satFlipVertically,
              ChannelManager,
              LayerList,
              LayerList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;

        ctColorChannel:
          begin
            LCommand := TgmSelectionAdjustmentOnLayerCommand.Create(
              satFlipVertically,
              ChannelManager,
              LayerList,
              LayerList.SelectedIndex,
              LOldSelection,
              Selection,
              GetSelectionForUndoRedo);
          end;
      end;

      if Assigned(LOldSelection) then
      begin
        LOldSelection.Free();
      end;
    end
    else
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

            FlipBitmap(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap, fmVertical);
            ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();

            // Undo/Redo
            LCommand := TgmAlphaChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
              ChannelManager.AlphaChannelList,
              ChannelManager.AlphaChannelList.SelectedIndex);
          end;

        ctQuickMaskChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

            FlipBitmap(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, fmVertical);
            ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();

            // for Undo/Redo
            LCommand := TgmQuickMaskChannelProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
              ChannelManager);
          end;

        ctLayerMaskChannel:
          begin
            // for Undo/Redo
            frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);

            FlipBitmap(LayerList.SelectedLayer.MaskBitmap, fmVertical);

            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                0, 0, LayerList.SelectedLayer.MaskBitmap);
            end;

            LayerList.SelectedLayer.Changed();

            // Undo/Redo
            LCommand := TgmLayerMaskProcessCommand.Create(
              LCommandName,
              frmMain.FBitmapBefore,
              LayerList.SelectedLayer.MaskBitmap,
              LayerList,
              LayerList.SelectedIndex);
          end;

        ctColorChannel:
          begin
            if LayerList.SelectedLayer is TgmNormalLayer then
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);

              FlipBitmap(LayerList.SelectedLayer.LayerBitmap, fmVertical);
              LayerList.SelectedLayer.Changed();

              // Undo/Redo
              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.LayerBitmap,
                LayerList,
                LayerList.SelectedIndex);
            end;
          end;
      end;
    end;

    // updating thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;
  end;
end;

procedure TdmMain.actnFlipVerticalUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        LEnabled := (not ToolsInPendingStatus());

        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          // we cannot do flip on special layers
          LEnabled := LEnabled and (LayerList.SelectedLayer is TgmNormalLayer);
        end;
      end
      else
      begin
        LEnabled := (not ToolsInPendingStatus());
                    
        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          // we cannot do flip on special layers and can only work on full RGB channel
          LEnabled := LEnabled and
                      (LayerList.SelectedLayer is TgmNormalLayer) and
                      (ChannelManager.ColorChannelList.SelectedChannelCount > 2);
        end;
      end;
    end;
  end;

  actnFlipVertical.Enabled := LEnabled;
end;

procedure TdmMain.actnBrightContrastExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Brightness/Contrast';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
                   
        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Brightness / Contrast command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmBrightnessContrast := TfrmBrightnessContrast.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmBrightnessContrast.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
              end;
            end;

          ctQuickMaskChannel:
            begin
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed;
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
                LayerList.SelectedLayer.Changed;
              end;
            end;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmBrightnessContrast);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnBrightContrastUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnBrightContrast.Enabled := LEnabled;
end;

procedure TdmMain.actnNewInvertLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateInvertLayer();

      LayerList.Insert(LOldSelectedIndex + 1, LLayer);
      LLayer.EnableMask();
      LLayer.Changed();

      // Undo/Redo
      CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewInvertLayerUpdate(Sender: TObject);
begin
  actnNewInvertLayer.Enabled := Assigned(ActiveChildForm) and
                                (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewThresholdLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateThresholdLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmThreshold := TfrmThreshold.Create(Application);
      try
        frmThreshold.AssociateToThresholdLayer( TgmThresholdLayer(LLayer) );

        case frmThreshold.ShowModal() of
          mrOK:
            begin
              if not frmThreshold.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmThreshold);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewThresholdLayerUpdate(Sender: TObject);
begin
  actnNewThresholdLayer.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnThresholdExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Threshold';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Threshold command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmThreshold := TfrmThreshold.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmThreshold.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctQuickMaskChannel:
            begin
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
                LayerList.SelectedLayer.Changed();
              end;
            end;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmThreshold);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnThresholdUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnThreshold.Enabled := LEnabled;
end;

procedure TdmMain.actnNewPosterizeLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreatePosterizeLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmPosterize := TfrmPosterize.Create(Application);
      try
        frmPosterize.AssociateToPosterizeLayer( TgmPosterizeLayer(LLayer) );

        case frmPosterize.ShowModal() of
          mrOK:
            begin
              if not frmPosterize.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmPosterize);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewPosterizeLayerUpdate(Sender: TObject);
begin
  actnNewPosterizeLayer.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnPosterizeExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Posterize';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Posterize command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmPosterize := TfrmPosterize.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmPosterize.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctQuickMaskChannel:
            begin
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
                LayerList.SelectedLayer.Changed();
              end;
            end;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmPosterize);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnPosterizeUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;
  
  actnPosterize.Enabled := LEnabled;
end;

procedure TdmMain.actnNewHueSaturationLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateHueSaturationLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmHueSaturation := TfrmHueSaturation.Create(Application);
      try
        frmHueSaturation.AssociateToHueSaturationLayer( TgmHueSaturationLayer(LLayer) );

        case frmHueSaturation.ShowModal() of
          mrOK:
            begin
              if not frmHueSaturation.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmHueSaturation);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewHueSaturationLayerUpdate(Sender: TObject);
begin
  actnNewHueSaturationLayer.Enabled := Assigned(ActiveChildForm) and
                                       (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnHueSaturationExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Hue/Saturation';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Hue / Saturation command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmHueSaturation := TfrmHueSaturation.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmHueSaturation.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if (LayerList.SelectedLayer is TgmNormalLayer) and
         (ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(LResultBmp);
          ShowProcessedSelection();
        end
        else
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
          LayerList.SelectedLayer.Changed;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmHueSaturation);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnHueSaturationUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnHueSaturation.Enabled := LEnabled;
end;

procedure TdmMain.actnNewColorBalanceLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateColorBalanceLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);
      
      frmColorBalance := TfrmColorBalance.Create(Application);
      try
        frmColorBalance.AssociateToColorBalanceLayer( TgmColorBalanceLayer(LLayer) );

        case frmColorBalance.ShowModal() of
          mrOK:
            begin
              if not frmColorBalance.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();
              
              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmColorBalance);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewColorBalanceLayerUpdate(Sender: TObject);
begin
  actnNewColorBalanceLayer.Enabled := Assigned(ActiveChildForm) and
                                      (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnColorBalanceExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Color Balance';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Color Balance command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmColorBalance := TfrmColorBalance.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmColorBalance.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if (LayerList.SelectedLayer is TgmNormalLayer) and
         (ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(LResultBmp);
          ShowProcessedSelection();
        end
        else
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
          LayerList.SelectedLayer.Changed;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmColorBalance);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnColorBalanceUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnColorBalance.Enabled := LEnabled;
end;

procedure TdmMain.actnNewCurvesLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateCurvesLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmCurves := TfrmCurves.Create(Application);
      try
        frmCurves.AssociateToCurvesLayer( TgmCurvesLayer(LLayer) );

        case frmCurves.ShowModal() of
          mrOK:
            begin
              if not frmCurves.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmCurves);
      end;
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewCurvesLayerUpdate(Sender: TObject);
begin
  actnNewCurvesLayer.Enabled := Assigned(ActiveChildForm) and
                                (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnCurvesExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Curves';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
                   
        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Curves command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmCurves := TfrmCurves.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmCurves.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctQuickMaskChannel:
            begin
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
                LayerList.SelectedLayer.Changed();
              end;
            end;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmCurves);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnCurvesUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnCurves.Enabled := LEnabled;
end;

procedure TdmMain.actnNewLevelsLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateLevelsLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      frmLevelsTool := TfrmLevelsTool.Create(Application);
      try
        frmLevelsTool.AssociateToLevelsLayer( TgmLevelsLayer(LLayer) );

        case frmLevelsTool.ShowModal() of
          mrOK:
            begin
              if not frmLevelsTool.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmLevelsTool);
      end;
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewLevelsLayerUpdate(Sender: TObject);
begin
  actnNewLevelsLayer.Enabled := Assigned(ActiveChildForm) and
                                (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnLevelsExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Levels';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Levels command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmLevelsTool := TfrmLevelsTool.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmLevelsTool.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed;
              end;
            end;

          ctQuickMaskChannel:
            begin
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed;
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed;
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
                LayerList.SelectedLayer.Changed;
              end;
            end;
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmLevelsTool);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnLevelsUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnLevels.Enabled := LEnabled;
end;

procedure TdmMain.actnNewChannelMixerLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateChannelMixerLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);
      
      frmChannelMixer := TfrmChannelMixer.Create(Application);
      try
        frmChannelMixer.AssociateToChannelMixerLayer( TgmChannelMixerLayer(LLayer) );

        case frmChannelMixer.ShowModal() of
          mrOK:
            begin
              if not frmChannelMixer.chckbxPreview.Checked then
              begin
                LLayer.Changed();
              end;

              LLayer.EnableMask();

              // Undo/Redo
              CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
            end;

          mrCancel:
            begin
              LayerList.CancelLayer(LOldSelectedIndex + 1);
            end;
        end;

      finally
        FreeAndNil(frmChannelMixer);
      end;
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewChannelMixerLayerUpdate(Sender: TObject);
begin
  actnNewChannelMixerLayer.Enabled := Assigned(ActiveChildForm) and
                                      (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnChannelMixerExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Channel Mixer';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Channel Mixer command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    frmChannelMixer := TfrmChannelMixer.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmChannelMixer.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if (LayerList.SelectedLayer is TgmNormalLayer) and
         (ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(LResultBmp);
          ShowProcessedSelection();
        end
        else
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
          LayerList.SelectedLayer.Changed();
        end;
      end;

      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

    finally
      FreeAndNil(frmChannelMixer);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnChannelMixerUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnChannelMixer.Enabled := LEnabled;
end;

procedure TdmMain.actnNewSolidColorLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LSolidColorLayer  : TgmSolidColorLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LSolidColorLayer := TgmSolidColorLayer( CreateSolidColorLayer() );
      LayerList.Insert(LOldSelectedIndex + 1, LSolidColorLayer);

      clrDlgRGB.Color := WinColor(LSolidColorLayer.SolidColor);

      if clrDlgRGB.Execute() then
      begin
        LSolidColorLayer.SolidColor := Color32(clrDlgRGB.Color);

        LSolidColorLayer.Changed();
        LSolidColorLayer.EnableMask();
        LSolidColorLayer.UpdateLogoThumbnail();

        // Undo/Redo
        CreateNewLayerCommand(LSolidColorLayer, LayerList.SelectedIndex);
      end
      else
      begin
        LayerList.CancelLayer(LOldSelectedIndex + 1);
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewSolidColorLayerUpdate(Sender: TObject);
begin
  actnNewSolidColorLayer.Enabled := Assigned(ActiveChildForm) and
                                            (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewGradientMapLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateGradientMapLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      // the Gradient Map dialog is created automatically,
      // so we don't need to create it first
      case frmGradientMap.ShowModal() of
        mrOK:
          begin
            if not frmGradientMap.chckbxPreview.Checked then
            begin
              LLayer.Changed();
            end;

            LLayer.EnableMask();

            // Undo/Redo
            CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
          end;

        mrCancel:
          begin
            LayerList.CancelLayer(LOldSelectedIndex + 1);
          end;
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewGradientMapLayerUpdate(Sender: TObject);
begin
  actnNewGradientMapLayer.Enabled := Assigned(ActiveChildForm) and
                                     (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnGradientMapExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Gradient Map';

  with ActiveChildForm do
  begin
    // we can not processing more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if (ChannelManager.CurrentChannelType = ctColorChannel) and
       (LayerList.SelectedLayer is TgmNormalLayer) then
    begin
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer then
      begin
        MessageDlg('Could not complete the Gradient Map command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    if Assigned(Selection) then
    begin
      Selection.IsAnimated := False;
    end;

    // the Gradient Map dialog is created automatically,
    // so we don't need to create it first

    LModalResult := frmGradientMap.ShowModal();

    case LModalResult of
      mrOK:
        begin
          LResultBmp := frmMain.FBitmapAfter;
        end;

      mrCancel:
        begin
          LResultBmp := frmMain.FBitmapBefore;
        end;
    end;

    if (LayerList.SelectedLayer is TgmNormalLayer) and
       (ChannelManager.CurrentChannelType = ctColorChannel) then
    begin
      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
      end
      else
      begin
        LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
        LayerList.SelectedLayer.Changed();
      end;
    end;

    if Assigned(Selection) then
    begin
      Selection.IsAnimated := True;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnGradientMapUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnGradientMap.Enabled := LEnabled;
end;

procedure TdmMain.actnNewGradientFillLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreateGradientFillLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);

      // the Gradient Fill dialog is created automatically,
      // so we don't need to create it first
      case frmGradientFill.ShowModal() of
        mrOK:
          begin
            LLayer.Changed();
            LLayer.EnableMask();
            LLayer.UpdateLogoThumbnail();

            // Undo/Redo
            CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
          end;

        mrCancel:
          begin
            LayerList.CancelLayer(LOldSelectedIndex + 1);
          end;
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewGradientFillLayerUpdate(Sender: TObject);
begin
  actnNewGradientFillLayer.Enabled := Assigned(ActiveChildForm) and
                                      (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewPatternLayerExecute(Sender: TObject);
var
  LOldSelectedIndex : Integer;
  LLayer            : TgmCustomLayer;
begin
  Screen.Cursor := crHourGlass;
  try
    with ActiveChildForm do
    begin
      LOldSelectedIndex := LayerList.SelectedIndex;

      LLayer := CreatePatternLayer();
      LayerList.Insert(LOldSelectedIndex + 1, LLayer);
      LLayer.UpdateLogoThumbnail;

      // the Pattern Fill dialog is created automatically,
      // so we don't need to create it at here
      case frmPatternFill.ShowModal() of
        mrOK:
          begin
            LLayer.Changed();
            LLayer.EnableMask();
            LLayer.UpdateLogoThumbnail();

            // Undo/Redo
            CreateNewLayerCommand(LLayer, LayerList.SelectedIndex);
          end;

        mrCancel:
          begin
            LayerList.CancelLayer(LOldSelectedIndex + 1);
          end;
      end;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdmMain.actnNewPatternLayerUpdate(Sender: TObject);
begin
  actnNewPatternLayer.Enabled := Assigned(ActiveChildForm) and
                                 (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnDismissTargetPathExecute(Sender: TObject);
var
  LShapeRegionLayer: TgmShapeRegionLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmShapeRegionLayer then
    begin
      LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);

      LShapeRegionLayer.IsDismissed := True;
      imgWorkArea.Changed();
    end;
  end;
end;

procedure TdmMain.actnDismissTargetPathUpdate(Sender: TObject);
var
  LEnabled          : Boolean;
  LShapeRegionLayer : TgmShapeRegionLayer;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if (LayerList.SelectedLayer is TgmShapeRegionLayer) and
         (frmMain.MainTool = gmtShape) then
      begin
        LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
        LEnabled          := not LShapeRegionLayer.IsDismissed;
      end;
    end;
  end;

  actnDismissTargetPath.Enabled := LEnabled;
end;

procedure TdmMain.actnShapeRegionBrushStyleExecute(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  pmnShapeBrushStyle.Popup(p.X, p.Y);
end;

procedure TdmMain.actnRegionSolidBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsSolid;
    frmMain.imgShapeBrushStyle.Hint := actnRegionSolidBrush.Hint;

    imglstCommon.GetBitmap(1, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free;
  end;
end;

procedure TdmMain.actnRegionHorizontalBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsHorizontal;
    frmMain.imgShapeBrushStyle.Hint := actnRegionHorizontalBrush.Hint;

    imglstCommon.GetBitmap(2, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnRegionVerticalBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsVertical;
    frmMain.imgShapeBrushStyle.Hint := actnRegionVerticalBrush.Hint;

    imglstCommon.GetBitmap(3, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnRegionFDiagonalBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsFDiagonal;
    frmMain.imgShapeBrushStyle.Hint := actnRegionFDiagonalBrush.Hint;

    imglstCommon.GetBitmap(4, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnRegionBDiagonalBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsBDiagonal;
    frmMain.imgShapeBrushStyle.Hint := actnRegionBDiagonalBrush.Hint;

    imglstCommon.GetBitmap(5, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnRegionCrossBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsCross;
    frmMain.imgShapeBrushStyle.Hint := actnRegionCrossBrush.Hint;

    imglstCommon.GetBitmap(6, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnRegionDiagCrossBrushExecute(Sender: TObject);
var
  LBmp              : TBitmap;
  LShapeRegionLayer : TgmShapeRegionLayer;
  LOldStyle         : TBrushStyle;
  LCommand          : TgmCustomCommand;
begin
  LBmp := TBitmap.Create;
  try
    frmMain.ShapeBrushStyle         := bsDiagCross;
    frmMain.imgShapeBrushStyle.Hint := actnRegionDiagCrossBrush.Hint;

    imglstCommon.GetBitmap(7, LBmp);
    frmMain.imgShapeBrushStyle.Picture.Bitmap.Assign(LBmp);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmShapeRegionLayer then
        begin
          LShapeRegionLayer := TgmShapeRegionLayer(LayerList.SelectedLayer);
          LOldStyle         := LShapeRegionLayer.BrushStyle; // for Undo/Redo

          if LShapeRegionLayer.BrushStyle <> frmMain.ShapeBrushStyle then
          begin
            LShapeRegionLayer.BrushStyle := frmMain.ShapeBrushStyle;
            LShapeRegionLayer.Changed();
            LShapeRegionLayer.UpdateLayerThumbnail();

            // Undo/Redo
            LCommand := TgmModifyShapeRegionFillingStyleCommand.Create(LayerList,
              LayerList.SelectedIndex, LOldStyle, frmMain.ShapeBrushStyle);

            CommandManager.AddCommand(LCommand);
          end;
        end;
      end;
    end;
  finally
    LBmp.Free();
  end;
end;

procedure TdmMain.actnCommitTextEditsExecute(Sender: TObject);
var
  LRichTextLayer : TgmRichTextLayer;
  LNewTextStream : TMemoryStream;
  LCommand       : TgmCustomCommand;
begin
  LCommand       := nil;
  LNewTextStream := nil;
  
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);

      if LRichTextLayer.TextLayerState in [tlsNew, tlsModify] then
      begin
        CommitEdits();

        // Undo/Redo
        if LRichTextLayer.TextLayerState = tlsNew then
        begin
          LCommand := TgmNewTextOnNewRichTextLayerCommand.Create(
            ChannelManager, LayerList, LRichTextLayer, LayerList.SelectedIndex);
        end
        else if LRichTextLayer.TextLayerState = tlsModify then
        begin
          if LRichTextLayer.RichTextStream.Size > 0 then
          begin
            LNewTextStream := TMemoryStream.Create();

            LRichTextLayer.RichTextStream.Position := 0;
            LNewTextStream.LoadFromStream(LRichTextLayer.RichTextStream);
            
            LRichTextLayer.RichTextStream.Position := 0;
            LNewTextStream.Position                := 0;
          end;

          LCommand := TgmEditTextOnExistedRichTextLayerCommand.Create(
            LayerList, LayerList.SelectedIndex, OldTextStream, LNewTextStream);

          if Assigned(LNewTextStream) then
          begin
            LNewTextStream.Free();
          end;
        end;

        if Assigned(LCommand) then
        begin
          CommandManager.AddCommand(LCommand);
        end;
      end;
    end;
  end;
end;

procedure TdmMain.actnCommitTextEditsUpdate(Sender: TObject);
var
  LEnabled       : Boolean;
  LRichTextLayer : TgmRichTextLayer;
begin
  LEnabled := False;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if LayerList.SelectedLayer is TgmRichTextLayer then
      begin
        LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);

        LEnabled := (LRichTextLayer.IsEditState and frmRichTextEditor.Visible);
      end;
    end;
  end;

  actnCommitTextEdits.Enabled := LEnabled;
end;

procedure TdmMain.actnCancelTextEditsExecute(Sender: TObject);
var
  LRichTextLayer : TgmRichTextLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);

      case LRichTextLayer.TextLayerState of
        tlsNew:
          begin
            frmRichTextEditor.Close();
            LayerList.CancelLayer(LayerList.SelectedIndex);
            frmMain.UpdateTextOptions();
          end;

        tlsModify:
          begin
            CancelEdits();
          end;
      end;
    end;
  end;
end;

procedure TdmMain.actnCancelTextEditsUpdate(Sender: TObject);
var
  LEnabled       : Boolean;
  LRichTextLayer : TgmRichTextLayer;
begin
  LEnabled := False;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if LayerList.SelectedLayer is TgmRichTextLayer then
      begin
        LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);
        LEnabled       := (LRichTextLayer.IsEditState and frmRichTextEditor.Visible);
      end;
    end;
  end;

  actnCancelTextEdits.Enabled := LEnabled;
end;

procedure TdmMain.actnOpenRichTextFileExecute(Sender: TObject);
var
  LRichTextLayer : TgmRichTextLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmRichTextLayer then
    begin
      LRichTextLayer := TgmRichTextLayer(LayerList.SelectedLayer);

      if OpndlgOpenText.Execute then
      begin
        frmRichTextEditor.rchedtRichTextEditor.Lines.LoadFromFile(OpndlgOpenText.FileName);

        LRichTextLayer.TextFileName := OpndlgOpenText.FileName;

        frmRichTextEditor.stsbrTextInfo.Panels[0].Text := 'File Name: ' +
          ExtractFileName(LRichTextLayer.TextFileName);

        frmRichTextEditor.Show;
      end;
    end;
  end;
end;

procedure TdmMain.actnOpenRichTextFileUpdate(Sender: TObject);
begin
  actnOpenRichTextFile.Enabled := Assigned(ActiveChildForm) and
                                  (frmRichTextEditor.Visible);
end;

procedure TdmMain.actnSaveRichTextExecute(Sender: TObject);
begin
  SaveNamedTextFile;
  frmRichTextEditor.Show;
end;

procedure TdmMain.actnSaveRichTextUpdate(Sender: TObject);
begin
  actnSaveRichText.Enabled := Assigned(ActiveChildForm) and
                              (frmRichTextEditor.Visible);
end;

procedure TdmMain.actnSaveRichTextAsExecute(Sender: TObject);
begin
  SaveNotNamedTextFile;
  frmRichTextEditor.Show;
end;

procedure TdmMain.actnSaveRichTextAsUpdate(Sender: TObject);
begin
  actnSaveRichTextAs.Enabled := Assigned(ActiveChildForm) and
                                (frmRichTextEditor.Visible)
end;

procedure TdmMain.actnLayerLockTransparencyExecute(Sender: TObject);
begin
  with ActiveChildForm.LayerList do
  begin
    actnLayerLockTransparency.Checked := not actnLayerLockTransparency.Checked;
    SelectedLayer.IsLockTransparency  := actnLayerLockTransparency.Checked;
  end;
end;

procedure TdmMain.actnLayerLockTransparencyUpdate(Sender: TObject);
var
  LChecked : Boolean;
  LEnabled : Boolean;
begin
  LChecked := False;
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
      begin
        LChecked := LayerList.SelectedLayer.IsLockTransparency;
        LEnabled := True;
      end;

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnLayerLockTransparency.Checked := LChecked;
  actnLayerLockTransparency.Enabled := LEnabled;
end;

procedure TdmMain.actnSelectAllFiguresExecute(Sender: TObject);
begin
  with ActiveChildForm do
  begin
    FigureManager.SelectAllFiguresOnVectorLayers;
    imgWorkArea.Changed;

    if FigureManager.SelectedFigureCount = 1 then
    begin
      FSelectedFigure := FigureManager.GetOnlyOneSelectedFigure;
    end
    else
    begin
      FSelectedFigure := nil;
    end;
  
    frmMain.spdbtnMoveObjects.Down := True;
    frmMain.ChangeStandardTools(frmMain.spdbtnMoveObjects);
    frmMain.UpdateStandardOptions;
  end;
end;

procedure TdmMain.actnSelectAllFiguresUpdate(Sender: TObject);
begin
  actnSelectAllFigures.Enabled := Assigned(ActiveChildForm) and
                                  ActiveChildForm.FigureManager.HasFiguresOnVectorLayers() and
                                  (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnLockFiguresExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    FigureManager.LockSelectedFigures();
    imgWorkArea.Changed();
    frmMain.UpdateStandardOptions();

    // Undo/Redo
    LCommand := TgmLockUnlockFiguresOnVectorLayersCommand.Create(LayerList,
      FigureManager.FSelectedFigureInfoArray, flmLock);

    LCommand.ChangeCommandIconByResourceName(gmMiscCommandIcons.LOCK_COMMAND_ICON_RES_NAME);
    CommandManager.AddCommand(LCommand);
  end;
end;

procedure TdmMain.actnLockFiguresUpdate(Sender: TObject);
begin
  actnLockFigures.Enabled := Assigned(ActiveChildForm) and
                             ActiveChildForm.FigureManager.HasSelectedUnlockedFigures() and
                             (frmMain.StandardTool = gstMoveObjects);
end;

procedure TdmMain.actnUnlockFiguresExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    FigureManager.UnlockSelectedFigures();
    imgWorkArea.Changed();
    frmMain.UpdateStandardOptions();

    // Undo/Redo
    LCommand := TgmLockUnlockFiguresOnVectorLayersCommand.Create(LayerList,
      FigureManager.FSelectedFigureInfoArray, flmUnlock);

    LCommand.ChangeCommandIconByResourceName(gmMiscCommandIcons.UNLOCK_COMMAND_ICON_RES_NAME);
    CommandManager.AddCommand(LCommand);
  end;
end;

procedure TdmMain.actnUnlockFiguresUpdate(Sender: TObject);
begin
  actnUnlockFigures.Enabled := Assigned(ActiveChildForm) and
                               ActiveChildForm.FigureManager.HasSelectedLockedFigures() and
                               (frmMain.StandardTool = gstMoveObjects);
end;

procedure TdmMain.actnDeleteFiguresExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    // Undo/Redo first
    LCommand := TgmDeleteFiguresOnVectorLayersCommand.Create(
      LayerList, FigureManager.FSelectedFigureInfoArray);

    CommandManager.AddCommand(LCommand);

    // doing deletion
    FigureManager.DeleteSelectedFigures();

    // After all the selected figures are deleted, draw the rest figures
    // on that layers which not be selected. Because of the field
    // FSelectedFigureIndexArray has not been cleared yet, so use these
    // indices for drawing the rest figures on the respective layers.
    // This will speed up the proformance. After that, clear the
    // FSelectedFigureIndexArray.
    FigureManager.DrawUnselectedAndLockedFiguresOnSelectedVectorLayers();
    FigureManager.UpdateThumbnailOnSelectedVectorLayers();
    FigureManager.ClearSelectedFiguresInfo();

    // update the view
    LayerList.SelectedLayer.Changed();
    frmMain.UpdateStandardOptions();
  end;
end;

procedure TdmMain.actnDeleteFiguresUpdate(Sender: TObject);
begin
  actnDeleteFigures.Enabled := Assigned(ActiveChildForm) and
                               (ActiveChildForm.FigureManager.SelectedFigureCount > 0) and
                               (frmMain.StandardTool = gstMoveObjects);
end;

procedure TdmMain.actnFigurePropertiesExecute(Sender: TObject);
var
  LOldProperties    : TgmFigureBasicData;
  LNewProperties    : TgmFigureBasicData;
  LCommand          : TgmCustomCommand;
  LVectorLayerIndex : Integer;
  LFigureIndex      : Integer;
begin
  with ActiveChildForm do
  begin
    if FigureManager.SelectedFigureCount = 1 then
    begin
      FSelectedFigure := FigureManager.GetOnlyOneSelectedFigure;
    end
    else
    begin
      FSelectedFigure := nil;
    end;

    if Assigned(FSelectedFigure) then
    begin
      // for Undo/Redo
      LOldProperties.Name       := FSelectedFigure.Name;
      LOldProperties.PenColor   := FSelectedFigure.PenColor;
      LOldProperties.BrushColor := FSelectedFigure.BrushColor;
      LOldProperties.PenWidth   := FSelectedFigure.PenWidth;
      LOldProperties.PenStyle   := FSelectedFigure.PenStyle;
      LOldProperties.BrushStyle := FSelectedFigure.BrushStyle;

      if FSelectedFigure.Flag in [ffRegularPolygon,
                                  ffSquare,
                                  ffRoundSquare,
                                  ffCircle] then
      begin
        LOldProperties.OriginX := FSelectedFigure.OriginPixelX;
        LOldProperties.OriginY := FSelectedFigure.OriginPixelY;
        LOldProperties.Radius  := FSelectedFigure.RadiusPixel;
      end;

      frmFigureProperties := TfrmFigureProperties.Create(Application);
      try
        case frmFigureProperties.ShowModal of
          idOK:
            begin
              with FSelectedFigure do
              begin
                Name       := frmFigureProperties.edtFigureName.Text;
                PenColor   := frmFigureProperties.shpPenColor.Brush.Color;
                BrushColor := frmFigureProperties.shpBrushColor.Brush.Color;
                PenWidth   := frmFigureProperties.updwnPenWidth.Position;
                PenStyle   := frmFigureProperties.PenStyle;
                BrushStyle := frmFigureProperties.BrushStyle;

                if Flag in [ffRegularPolygon,
                            ffSquare,
                            ffRoundSquare,
                            ffCircle] then
                begin
                  frmFigureProperties.SetFigureProperties(frmMain.FigureUnits);
                end;
              end;

              FigureManager.DrawAllFiguresOnSelectedVectorLayers();
              FigureManager.UpdateThumbnailOnSelectedVectorLayers();

              // refresh the view
              LayerList.SelectedLayer.Changed();

              // Undo/Redo
              LNewProperties.Name       := FSelectedFigure.Name;
              LNewProperties.PenColor   := FSelectedFigure.PenColor;
              LNewProperties.BrushColor := FSelectedFigure.BrushColor;
              LNewProperties.PenWidth   := FSelectedFigure.PenWidth;
              LNewProperties.PenStyle   := FSelectedFigure.PenStyle;
              LNewProperties.BrushStyle := FSelectedFigure.BrushStyle;

              if FSelectedFigure.Flag in [ffRegularPolygon,
                                          ffSquare,
                                          ffRoundSquare,
                                          ffCircle] then
              begin
                LNewProperties.OriginX := FSelectedFigure.OriginPixelX;
                LNewProperties.OriginY := FSelectedFigure.OriginPixelY;
                LNewProperties.Radius  := FSelectedFigure.RadiusPixel;
              end;

              LVectorLayerIndex := FigureManager.GetOnlyOneSelectedFigureLayerIndex();
              LFigureIndex      := FigureManager.GetOnlyOneSelectedFigureIndex();
              
              LCommand := TgmModifyFigurePropertiesOnVectorLayerCommand.Create(
                LayerList, LVectorLayerIndex, LFigureIndex,
                LOldProperties, LNewProperties);

              CommandManager.AddCommand(LCommand);
            end;
            
          idCancel:
            begin
              frmFigureProperties.RestoreSettings();
            end;
        end;
        
      finally
        FreeAndNil(frmFigureProperties);
      end;
    end;
  end;
end;

procedure TdmMain.actnFigurePropertiesUpdate(Sender: TObject);
begin
  actnFigureProperties.Enabled := Assigned(ActiveChildForm) and
                                  (ActiveChildForm.FigureManager.SelectedFigureCount = 1) and
                                  (frmMain.StandardTool = gstMoveObjects);
end;

procedure TdmMain.actnSelectFiguresExecute(Sender: TObject);
var
  LCommand : TgmModifyFiguresOnVectorLayersCommand;
begin
  with ActiveChildForm do
  begin
    // Undo/Redo
    LCommand := TgmModifyFiguresOnVectorLayersCommand.Create(LayerList);
    LCommand.SetOldFiguresData(LayerList);

    // get the indexes of vector layers
    FigureManager.UpdateVectorLayerIndexRecord();

    if FigureManager.SelectedFigureCount > 0 then
    begin
      FigureManager.DeselectAllFigures();
      imgWorkArea.Changed();
    end;

    frmMain.spdbtnMoveObjects.Down := True;
    frmMain.ChangeStandardTools(frmMain.spdbtnMoveObjects);

    frmSelectFigures := TfrmSelectFigures.Create(nil);
    try
      case frmSelectFigures.ShowModal() of
        idOK:
          begin
            frmSelectFigures.ApplyConfiguration();
            FigureManager.UpdateThumbnailOnSelectedVectorLayers();

            // Undo/Redo
            if frmSelectFigures.IsFiguresModified then
            begin
              LCommand.SetNewFiguresData(LayerList);
              CommandManager.AddCommand(LCommand);
            end;
          end;

        idCancel:
          begin
            frmSelectFigures.CancelConfiguration();
            FigureManager.DrawAllFigures();
            LayerList.SelectedLayer.Changed();

            // Undo/Redo
            FreeAndNil(LCommand);
          end;
      end;

      frmSelectFigures.ClearSelectedInfoArray();
      frmSelectFigures.ClearAllOriginalFiguresArray();
    finally
      FreeAndNil(frmSelectFigures);
    end;

    if FigureManager.SelectedFigureCount = 1 then
    begin
      FSelectedFigure := FigureManager.GetOnlyOneSelectedFigure();
    end
    else
    begin
      FSelectedFigure := nil;
    end;

    frmMain.UpdateStandardOptions();
  end;
end;

procedure TdmMain.actnSelectFiguresUpdate(Sender: TObject);
begin
  actnSelectFigures.Enabled := Assigned(ActiveChildForm) and
                               ActiveChildForm.FigureManager.HasFiguresOnVectorLayers() and
                               (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnCutExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  LCommand := nil;
  
  if frmRichTextEditor.Visible then
  begin
    if Assigned(frmMain.FSelectionClipboard) then
    begin
      FreeAndNil(frmMain.FSelectionClipboard);
    end;

    frmRichTextEditor.rchedtRichTextEditor.CutToClipboard();
    frmRichTextEditor.Show();
  end
  else
  begin
    Clipboard.Clear();

    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        // Create Undo/Redo first.
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              LCommand := TgmCutPixelsFromAlphaChannelCommand.Create(
                ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
                Selection, frmMain.FSelectionClipboard, GetSelectionForUndoRedo,
                DeleteSelectionForUndoRedo, GetClipboardSelectionForUndoRedo,
                DeleteClipboardSelectionForUndoRedo);
            end;

          ctQuickMaskChannel:
            begin
              LCommand := TgmCutPixelsFromQuickMaskChannelCommand.Create(
                ChannelManager, Selection, frmMain.FSelectionClipboard,
                GetSelectionForUndoRedo, DeleteSelectionForUndoRedo,
                GetClipboardSelectionForUndoRedo,
                DeleteClipboardSelectionForUndoRedo);
            end;

          ctLayerMaskChannel:
            begin
              LCommand := TgmCutPixelsFromLayerMaskCommand.Create(ChannelManager,
                LayerList, LayerList.SelectedIndex, Selection,
                frmMain.FSelectionClipboard, GetSelectionForUndoRedo,
                DeleteSelectionForUndoRedo, GetClipboardSelectionForUndoRedo,
                DeleteClipboardSelectionForUndoRedo);
            end;

          ctColorChannel:
            begin
              LCommand := TgmCutPixelsFromLayerCommand.Create(ChannelManager,
                LayerList, LayerList.SelectedIndex, Selection,
                frmMain.FSelectionClipboard, GetSelectionForUndoRedo,
                DeleteSelectionForUndoRedo, GetClipboardSelectionForUndoRedo,
                DeleteClipboardSelectionForUndoRedo);
            end;
        end;

        if Assigned(LCommand) then
        begin
          CommandManager.AddCommand(LCommand);
        end;

        // cutting selection off ...
        frmMain.DuplicateSelection();
        DeleteSelection();
        frmMain.UpdateMarqueeOptions();
      end;
    end;
  end;
end;

procedure TdmMain.actnCutUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if (frmRichTextEditor.Visible) and
         (frmRichTextEditor.rchedtRichTextEditor.SelLength > 0) then
      begin
        LEnabled := True;
      end
      else
      begin
        LEnabled := Assigned(Selection) and
                    (not ToolsInPendingStatus());

        // we cannot do cutting on special layers
        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          if not (LayerList.SelectedLayer is TgmNormalLayer) then
          begin
            LEnabled := False;
          end;
        end;
      end;
    end;
  end;

  actnCut.Enabled := LEnabled;
end;

procedure TdmMain.actnCopyExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  if frmRichTextEditor.Visible then
  begin
    if Assigned(frmMain.FSelectionClipboard) then
    begin
      FreeAndNil(frmMain.FSelectionClipboard);
    end;

    frmRichTextEditor.rchedtRichTextEditor.CopyToClipboard();
    frmRichTextEditor.Show();
  end
  else
  begin
    Clipboard.Clear();

    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        // Undo/Redo, first
        LCommand := TgmCopyPixelsCommand.Create(Selection,
          frmMain.FSelectionClipboard, GetSelectionForUndoRedo,
          DeleteSelectionForUndoRedo, GetClipboardSelectionForUndoRedo,
          DeleteClipboardSelectionForUndoRedo);

        CommandManager.AddCommand(LCommand);

        // then, copy the selection to the "clipboard"
        frmMain.DuplicateSelection();
      end;
    end;
  end;
end;

procedure TdmMain.actnCopyUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if (frmRichTextEditor.Visible) and
         (frmRichTextEditor.rchedtRichTextEditor.SelLength > 0) then
      begin
        LEnabled := True;
      end
      else
      begin
        LEnabled := Assigned(Selection) and
                    (not ToolsInPendingStatus());

        // we cannot do cutting on special layers
        if ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          if not (LayerList.SelectedLayer is TgmNormalLayer) then
          begin
            LEnabled := False;
          end;
        end;
      end;
    end;
  end;

  actnCopy.Enabled := LEnabled;
end;

procedure TdmMain.actnPasteExecute(Sender: TObject);
var
  LOldSelection  : TgmSelection;
  LOldChannelSet : TgmChannelSet;
  LOldLayerIndex : Integer;
  LCommand       : TgmCustomCommand;
begin
  LCommand      := nil;
  LOldSelection := nil;

  if frmRichTextEditor.Visible then
  begin
    frmRichTextEditor.rchedtRichTextEditor.PasteFromClipboard();
    frmRichTextEditor.Show();
  end
  else
  begin
    with ActiveChildForm do
    begin
      // Remember the old selection for Undo/Redo, if it is existed.
      if Assigned(Selection) then
      begin
        LOldSelection := TgmSelection.Create();
        LOldSelection.AssignAllSelectionData(Selection);
      end;

      // for Undo/Redo
      LOldChannelSet := ChannelManager.SelectedColorChannels;
      LOldLayerIndex := LayerList.SelectedIndex;

      if Assigned(frmMain.FSelectionClipboard) then
      begin
        // commit the old selection if any
        CommitSelection();

        // doing paste
        frmMain.PasteSelection();
        frmMain.UpdateMarqueeOptions();

        // Undo/Redo
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              LCommand := TgmPastePixelsOnAlphaChannelCommand.Create(
                ChannelManager, ChannelManager.AlphaChannelList.SelectedIndex,
                LOldSelection, Selection, GetSelectionForUndoRedo,
                DeleteSelectionForUndoRedo);
            end;

          ctQuickMaskChannel:
            begin
              LCommand := TgmPastePixelsOnQuickMaskChannelCommand.Create(
                ChannelManager, LOldSelection, Selection,
                GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
            end;

          ctLayerMaskChannel:
            begin
              LCommand := TgmPastePixelsOnLayerMaskCommand.Create(
                ChannelManager, LayerList, LayerList.SelectedIndex,
                LOldSelection, Selection, GetSelectionForUndoRedo,
                DeleteSelectionForUndoRedo);
            end;

          ctColorChannel:
            begin
              if LOldLayerIndex <> LayerList.SelectedIndex then
              begin
                LCommand := TgmPastePixelsOnNewLayerCommand.Create(
                  ChannelManager, LayerList, LOldSelection, Selection,
                  LayerList.SelectedLayer, LOldLayerIndex,
                  LayerList.SelectedIndex, GetSelectionForUndoRedo,
                  DeleteSelectionForUndoRedo);
              end
              else
              begin
                LCommand := TgmPastePixelsOnColorChannelsCommand.Create(
                  ChannelManager, LayerList, LayerList.SelectedIndex,
                  LOldSelection, Selection, GetSelectionForUndoRedo,
                  DeleteSelectionForUndoRedo);
              end;
            end;
        end;

        if Assigned(LCommand) then
        begin
          CommandManager.AddCommand(LCommand);
        end;
      end;
    end;
  end;
end;

procedure TdmMain.actnPasteUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if frmRichTextEditor.Visible then
      begin
        LEnabled := Clipboard.HasFormat(cf_TEXT);
      end
      else
      begin
        LEnabled := Assigned(frmMain.FSelectionClipboard) and
                    (not ToolsInPendingStatus());
      end;
    end;
  end;

  actnPaste.Enabled := LEnabled;
end;

procedure TdmMain.actnCreateNewPathExecute(Sender: TObject);
var
  LPath             : TgmPath;
  LOldSelectedIndex : Integer;
  LCommand          : TgmCustomCommand;
begin
  // create new path 
  if Assigned(ActiveChildForm.PathLayer) then
  begin
    ActiveChildForm.PathLayer.Bitmap.FillRect(0, 0,
      ActiveChildForm.PathLayer.Bitmap.Width,
      ActiveChildForm.PathLayer.Bitmap.Height, $00000000);
  end
  else
  begin
    ActiveChildForm.CreatePathLayer();
  end;

  ActiveChildForm.PathLayer.Bitmap.Changed();

  LPath := TgmPath.Create(ActiveChildForm.PathList, ptNamedPath);

  LPath.UpdateThumbnail(
    ActiveChildForm.LayerList.SelectedLayer.LayerBitmap.Width,
    ActiveChildForm.LayerList.SelectedLayer.LayerBitmap.Height,
    Point(0, 0) );

  LOldSelectedIndex := ActiveChildForm.PathList.SelectedIndex;  // for Undo/Redo

  with ActiveChildForm.PathList do
  begin
    if (Count = 0) or IsLastPathNamed() then
    begin
      Add(LPath);
    end
    else
    if (Count > 0) and ( not IsLastPathNamed() ) then
    begin
      Insert(LPath, Count - 1);
    end;
  end;

  // Undo/Redo
  LCommand := TgmNewPathCommand.Create(ActiveChildForm.PathList,
    LOldSelectedIndex, LPath, ActiveChildForm.PathList.SelectedIndex);

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actnCreateNewPathUpdate(Sender: TObject);
begin
  actnCreateNewPath.Enabled := Assigned(ActiveChildForm) and 
                               (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnDeleteCurrentPathExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  if MessageDlg('Delete the path "' +
                ActiveChildForm.PathList.SelectedPath.PathName + '"?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Undo/Redo first
    LCommand := TgmDeletePathCommand.Create(ActiveChildForm.PathList,
      ActiveChildForm.PathList.SelectedPath, ActiveChildForm.PathList.SelectedIndex);

    // delete the selected path
    ActiveChildForm.FCurvePath := nil;
    ActiveChildForm.PathList.DeleteSelectedPath();

    // Undo/Redo
    ActiveChildForm.CommandManager.AddCommand(LCommand);
  end;
end;

procedure TdmMain.actnDeleteCurrentPathUpdate(Sender: TObject);
begin
  actnDeleteCurrentPath.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus()) and
                                   Assigned(ActiveChildForm.PathList.SelectedPath);
end;

procedure TdmMain.actnFillPathExecute(Sender: TObject);
var
  LOffsetVector     : TPoint;
  LProcessedPartBmp : TBitmap32;
  LFillColor        : TColor32;
  LCommand          : TgmCustomCommand;
  LCommandName      : string;
begin
  LFillColor   := $0;
  LCommandName := 'Fill Path';
  LCommand     := nil;

  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if Assigned(PathList.SelectedPath) then
    begin
      // save bitmap for creating Undo/Redo command
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              frmMain.FBitmapBefore.Assign(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;

          ctQuickMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
            end;
        end;
      end;

      // filling the path
      if Assigned(Selection) then
      begin
        if Selection.IsCornerStretched then
        begin
          MessageDlg('Could not fill the selected paths on the selection,' + #10#13 +
                     'because the current selection was resized.', mtInformation, [mbOK], 0);
          Exit;
        end
        else
        begin
          case ChannelManager.CurrentChannelType of
            ctAlphaChannel,
            ctQuickMaskChannel,
            ctLayerMaskChannel:
              begin
                LFillColor := frmMain.ForeGrayColor;
              end;

            ctColorChannel:
              begin
                LFillColor := frmMain.GlobalForeColor;
              end;
          end;

          Selection.IsAnimated := False;

          // calculating the offset of the path layer relative to the selection
          LOffsetVector.X := (- Selection.MaskBorderStart.X);
          LOffsetVector.Y := (- Selection.MaskBorderStart.Y);

          // create mask bitmap
          LProcessedPartBmp := TBitmap32.Create();
          try
            LProcessedPartBmp.SetSize(Selection.CutOriginal.Width,
                                      Selection.CutOriginal.Height);

            LProcessedPartBmp.Clear(clBlack32);

            // fill the path on the selection
            PathList.SelectedPath.CurvePathList.FillSelectedPaths(
              Selection.CutOriginal.Canvas,
              LFillColor,
              frmMain.GlobalBrushStyle,
              LOffsetVector);

            // filling on mask
            PathList.SelectedPath.CurvePathList.FillSelectedPaths(
              LProcessedPartBmp.Canvas,
              clWhite,
              frmMain.GlobalBrushStyle,
              LOffsetVector);

            // restore the alpha channels
            if LayerList.SelectedLayer.IsLockTransparency then
            begin
              case ChannelManager.CurrentChannelType of
                ctColorChannel:
                  begin
                    ReplaceAlphaChannelWithSource(Selection.CutOriginal,
                                                  frmMain.FBitmapBefore);
                  end;

                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    ReplaceAlphaChannelWithNewValue(Selection.CutOriginal, 255);
                  end;
              end;
            end
            else
            begin
              if (ChannelManager.CurrentChannelType = ctColorChannel) and
                 (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
              begin
                ReplaceAlphaChannelWithSource(Selection.CutOriginal,
                                              frmMain.FBitmapBefore);
              end
              else
              begin
                // restore Alpha channel on processed part
                MakeCanvasProcessedOpaque(Selection.CutOriginal, LProcessedPartBmp);
              end;
            end;

            // channel replacement
            if ChannelManager.CurrentChannelType = ctColorChannel then
            begin
              ReplaceRGBChannels(frmMain.FBitmapBefore,
                                 Selection.CutOriginal,
                                 LProcessedPartBmp,
                                 ChannelManager.SelectedColorChannels,
                                 crsRemainDest);
            end;

          finally
            LProcessedPartBmp.Free();
          end;

          ShowProcessedSelection();
          Selection.IsAnimated := True;

          // Undo/Redo
          frmMain.FBitmapAfter.Assign(Selection.CutOriginal);

          case ChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                  LCommandName,
                  ChannelManager,
                  ChannelManager.AlphaChannelList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                  LCommandName,
                  ChannelManager,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                  LCommandName,
                  ChannelManager,
                  LayerList,
                  LayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                  LCommandName,
                  ChannelManager,
                  LayerList,
                  LayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;
          end;
          
          if Assigned(LCommand) then
          begin
            CommandManager.AddCommand(LCommand);
          end;
        end;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

              PathList.SelectedPath.CurvePathList.FillSelectedPaths(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Canvas,
                frmMain.ForeGrayColor,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              ReplaceAlphaChannelWithNewValue(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap, 255);

              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();

              // Undo/Redo
              LCommand := TgmAlphaChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                ChannelManager.AlphaChannelList,
                ChannelManager.AlphaChannelList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;

          ctQuickMaskChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

              PathList.SelectedPath.CurvePathList.FillSelectedPaths(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Canvas,
                frmMain.ForeGrayColor,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              ReplaceAlphaChannelWithNewValue(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, 255);

              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();

              // Undo/Redo
              LCommand := TgmQuickMaskChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                ChannelManager);

              CommandManager.AddCommand(LCommand);
            end;

          ctLayerMaskChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);

              PathList.SelectedPath.CurvePathList.FillSelectedPaths(
                LayerList.SelectedLayer.MaskBitmap.Canvas,
                frmMain.ForeGrayColor,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              ReplaceAlphaChannelWithNewValue(
                LayerList.SelectedLayer.MaskBitmap, 255);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();

              // Undo/Redo
              LCommand := TgmLayerMaskProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.MaskBitmap,
                LayerList,
                LayerList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;

          ctColorChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);

              PathList.SelectedPath.CurvePathList.FillSelectedPaths(
                LayerList.SelectedLayer.LayerBitmap.Canvas,
                frmMain.GlobalForeColor,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              LProcessedPartBmp := TBitmap32.Create();
              try
                LProcessedPartBmp.SetSize(
                  LayerList.SelectedLayer.LayerBitmap.Width,
                  LayerList.SelectedLayer.LayerBitmap.Height);

                LProcessedPartBmp.Clear(clBlack);

                PathList.SelectedPath.CurvePathList.FillSelectedPaths(
                  LProcessedPartBmp.Canvas, clWhite,
                  frmMain.GlobalBrushStyle, Point(0, 0) );

                // restore the alpha channels
                if (LayerList.SelectedLayer.IsLockTransparency) or
                   (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
                begin
                  ReplaceAlphaChannelWithSource(
                    LayerList.SelectedLayer.LayerBitmap, frmMain.FBitmapBefore);
                end
                else
                begin
                  // restore Alpha channel on processed part
                  MakeCanvasProcessedOpaque(
                    LayerList.SelectedLayer.LayerBitmap, LProcessedPartBmp);
                end;

                // channel replacement
                ReplaceRGBChannels(frmMain.FBitmapBefore,
                                   LayerList.SelectedLayer.LayerBitmap,
                                   LProcessedPartBmp,
                                   ChannelManager.SelectedColorChannels,
                                   crsRemainDest);

                LayerList.SelectedLayer.Changed();
              finally
                LProcessedPartBmp.Free();
              end;

              // Undo/Redo
              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.LayerBitmap,
                LayerList,
                LayerList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;
        end;
      end;

      // update thumbnails
      UpdateThumbnailsBySelectedChannel();
    end;
  end;
end;

procedure TdmMain.actnFillPathUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (not ToolsInPendingStatus()) and
                  Assigned(PathList.SelectedPath) and
                  Assigned(PathList.SelectedPath.CurvePathList.SelectedPath);

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if not (LayerList.SelectedLayer is TgmNormalLayer) then
        begin
          LEnabled := False;
        end;
      end;
    end;
  end;

  actnFillPath.Enabled := LEnabled;
end;

procedure TdmMain.actnStrokePathExecute(Sender: TObject);
var
  LOffsetVector          : TPoint;
  LProcessedPartBmp      : TBitmap32;
  LPenColor, LBrushColor : TColor32;
  LCommand               : TgmCustomCommand;
  LCommandName           : string;
begin
  LPenColor    := $0;
  LBrushColor  := $0;
  LCommandName := 'Stroke Path';
  LCommand     := nil;

  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if Assigned(PathList.SelectedPath) then
    begin
      // save bitmap for creating Undo/Redo command
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              frmMain.FBitmapBefore.Assign(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;

          ctQuickMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
            end;
        end;
      end;

      // stroke path
      if Assigned(Selection) then
      begin
        if Selection.IsCornerStretched then
        begin
          MessageDlg('Could not stroke the selected paths on the selection,' + #10#13 +
                     'because the current selection was resized.', mtInformation, [mbOK], 0);
          Exit;
        end
        else
        begin
          // setting the pen and brush
          case ChannelManager.CurrentChannelType of
            ctAlphaChannel,
            ctQuickMaskChannel,
            ctLayerMaskChannel:
              begin
                LPenColor   := frmMain.ForeGrayColor;
                LBrushColor := frmMain.BackGrayColor;
              end;

            ctColorChannel:
              begin
                LPenColor   := frmMain.GlobalForeColor;
                LBrushColor := frmMain.GlobalBackColor;
              end;
          end;

          Selection.IsAnimated := False;

          // calculating the offset of the path layer relative to the selection
          LOffsetVector.X := (- Selection.MaskBorderStart.X);
          LOffsetVector.Y := (- Selection.MaskBorderStart.Y);

          // create mask bitmap
          LProcessedPartBmp := TBitmap32.Create();
          try
            LProcessedPartBmp.SetSize(Selection.CutOriginal.Width,
                                      Selection.CutOriginal.Height);

            LProcessedPartBmp.Clear(clBlack32);

            // stroke the path on selection
            PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
              Selection.CutOriginal.Canvas,
              frmMain.GlobalPenWidth,
              LPenColor,
              LBrushColor,
              frmMain.GlobalPenStyle,
              frmMain.GlobalBrushStyle,
              LOffsetVector);

            // stroke on mask
            PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
              LProcessedPartBmp.Canvas,
              frmMain.GlobalPenWidth,
              clWhite,
              clWhite,
              frmMain.GlobalPenStyle,
              frmMain.GlobalBrushStyle,
              LOffsetVector);

            // restore the alpha channels
            if LayerList.SelectedLayer.IsLockTransparency then
            begin
              case ChannelManager.CurrentChannelType of
                ctColorChannel:
                  begin
                    ReplaceAlphaChannelWithSource(Selection.CutOriginal,
                                                  frmMain.FBitmapBefore);
                  end;

                ctAlphaChannel,
                ctQuickMaskChannel,
                ctLayerMaskChannel:
                  begin
                    ReplaceAlphaChannelWithNewValue(Selection.CutOriginal, 255);
                  end;
              end;
            end
            else
            begin
              if (ChannelManager.CurrentChannelType = ctColorChannel) and
                 (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
              begin
                ReplaceAlphaChannelWithSource(Selection.CutOriginal,
                                              frmMain.FBitmapBefore);
              end
              else
              begin
                // restore Alpha channel on processed part
                MakeCanvasProcessedOpaque(Selection.CutOriginal, LProcessedPartBmp);
              end;
            end;

            // channel replacement
            if ChannelManager.CurrentChannelType = ctColorChannel then
            begin
              ReplaceRGBChannels(frmMain.FBitmapBefore,
                                 Selection.CutOriginal,
                                 LProcessedPartBmp,
                                 ChannelManager.SelectedColorChannels,
                                 crsRemainDest);
            end;
            
          finally
            LProcessedPartBmp.Free();
          end;

          ShowProcessedSelection();
          Selection.IsAnimated := True;

          // Undo/Redo
          frmMain.FBitmapAfter.Assign(Selection.CutOriginal);

          case ChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
                  LCommandName,
                  ChannelManager,
                  ChannelManager.AlphaChannelList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctQuickMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
                  LCommandName,
                  ChannelManager,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctLayerMaskChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerMaskCommand.Create(
                  LCommandName,
                  ChannelManager,
                  LayerList,
                  LayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;

            ctColorChannel:
              begin
                LCommand := TgmSelectionPixelProcessOnLayerCommand.Create(
                  LCommandName,
                  ChannelManager,
                  LayerList,
                  LayerList.SelectedIndex,
                  frmMain.FBitmapBefore,
                  frmMain.FBitmapAfter,
                  GetSelectionForUndoRedo);
              end;
          end;
          
          if Assigned(LCommand) then
          begin
            CommandManager.AddCommand(LCommand);
          end;
        end;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);

              PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Canvas,
                frmMain.GlobalPenWidth,
                frmMain.ForeGrayColor,
                frmMain.BackGrayColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              ReplaceAlphaChannelWithNewValue(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap, 255);

              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();

              // Undo/Redo
              LCommand := TgmAlphaChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                ChannelManager.AlphaChannelList,
                ChannelManager.AlphaChannelList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;

          ctQuickMaskChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);

              PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Canvas,
                frmMain.GlobalPenWidth,
                frmMain.ForeGrayColor,
                frmMain.BackGrayColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                Point(0, 0) );

              ReplaceAlphaChannelWithNewValue(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, 255);

              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();

              // Undo/Redo
              LCommand := TgmQuickMaskChannelProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                ChannelManager);

              CommandManager.AddCommand(LCommand);
            end;

          ctLayerMaskChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);

              PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
                LayerList.SelectedLayer.MaskBitmap.Canvas,
                frmMain.GlobalPenWidth,
                frmMain.ForeGrayColor,
                frmMain.BackGrayColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                Point(0, 0));

              ReplaceAlphaChannelWithNewValue(
                LayerList.SelectedLayer.MaskBitmap, 255);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();

              // Undo/Redo
              LCommand := TgmLayerMaskProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.MaskBitmap,
                LayerList,
                LayerList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;

          ctColorChannel:
            begin
              // for Undo/Redo
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);

              PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
                LayerList.SelectedLayer.LayerBitmap.Canvas,
                frmMain.GlobalPenWidth,
                frmMain.GlobalForeColor,
                frmMain.GlobalBackColor,
                frmMain.GlobalPenStyle,
                frmMain.GlobalBrushStyle,
                Point(0, 0));

              LProcessedPartBmp := TBitmap32.Create();
              try
                LProcessedPartBmp.SetSize(
                  LayerList.SelectedLayer.LayerBitmap.Width,
                  LayerList.SelectedLayer.LayerBitmap.Height);

                LProcessedPartBmp.Clear(clBlack);
                
                PathList.SelectedPath.CurvePathList.StrokeSelectedPaths(
                  LProcessedPartBmp.Canvas,
                  frmMain.GlobalPenWidth,
                  clWhite,
                  clWhite,
                  frmMain.GlobalPenStyle,
                  frmMain.GlobalBrushStyle,
                  Point(0, 0) );

                // restore the alpha channels
                if (LayerList.SelectedLayer.IsLockTransparency) or
                   (ChannelManager.ColorChannelList.SelectedChannelCount < 3) then
                begin
                  ReplaceAlphaChannelWithSource(
                    LayerList.SelectedLayer.LayerBitmap, frmMain.FBitmapBefore);
                end
                else
                begin
                  // restore Alpha channel on processed part
                  MakeCanvasProcessedOpaque(
                    LayerList.SelectedLayer.LayerBitmap, LProcessedPartBmp);
                end;

                // channel replacement
                ReplaceRGBChannels(frmMain.FBitmapBefore,
                                   LayerList.SelectedLayer.LayerBitmap,
                                   LProcessedPartBmp,
                                   ChannelManager.SelectedColorChannels,
                                   crsRemainDest);

                LayerList.SelectedLayer.Changed();
              finally
                LProcessedPartBmp.Free();
              end;

              // Undo/Redo
              LCommand := TgmLayerImageProcessCommand.Create(
                LCommandName,
                frmMain.FBitmapBefore,
                LayerList.SelectedLayer.LayerBitmap,
                LayerList,
                LayerList.SelectedIndex);

              CommandManager.AddCommand(LCommand);
            end;
        end;
      end;

      // update thumbnails
      UpdateThumbnailsBySelectedChannel();
    end;
  end;
end;

procedure TdmMain.actnStrokePathUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (not ToolsInPendingStatus()) and
                  Assigned(PathList.SelectedPath) and
                  Assigned(PathList.SelectedPath.CurvePathList.SelectedPath);

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if not (LayerList.SelectedLayer is TgmNormalLayer) then
        begin
          LEnabled := False;
        end;
      end;
    end;
  end;

  actnStrokePath.Enabled := LEnabled;
end;

procedure TdmMain.actnLoadPathAsSelectionExecute(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandName  : string;
  LOldSelection : TgmSelection;
begin
  LOldSelection := nil;
  LCommand      := nil;

  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Could not process more than one alpha channels at a time.',
                   mtError, [mbOK], 0);
        Exit;
      end;
    end;

    // for Undo/Redo
    if Assigned(Selection) then
    begin
      LOldSelection := TgmSelection.Create();
      LOldSelection.AssignAllSelectionData(Selection);
    end;

    LoadPathAsSelection();

    // Undo/Redo
    LCommandName := 'Selection Change';
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          LCommand := TgmNewSelectionOnAlphaChannelCommand.Create(
            LCommandName, ChannelManager, LOldSelection, Selection,
            ChannelManager.AlphaChannelList.SelectedIndex,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctQuickMaskChannel:
        begin
          LCommand := TgmNewSelectionOnQuickMaskChannelCommand.Create(
            LCommandName, ChannelManager, LOldSelection, Selection,
            GetSelectionForUndoRedo, DeleteSelectionForUndoRedo);
        end;

      ctLayerMaskChannel:
        begin
          LCommand := TgmNewSelectionOnLayerMaskCommand.Create(LCommandName,
            ChannelManager, LayerList, LOldSelection, Selection,
            LayerList.SelectedIndex, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;

      ctColorChannel:
        begin
          LCommand := TgmNewSelectionOnLayerCommand.Create(LCommandName,
            ChannelManager, LayerList, LOldSelection, Selection,
            LayerList.SelectedIndex, GetSelectionForUndoRedo,
            DeleteSelectionForUndoRedo);
        end;
    end;

    if Assigned(LCommand) then
    begin
      CommandManager.AddCommand(LCommand);
    end;

    if Assigned(LOldSelection) then
    begin
      LOldSelection.Free();
    end;
  end;
end;

procedure TdmMain.actnLoadPathAsSelectionUpdate(Sender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      actnLoadPathAsSelection.Enabled :=
        (not ToolsInPendingStatus()) and
        Assigned(PathList.SelectedPath) and
        Assigned(PathList.SelectedPath.CurvePathList.SelectedPath);
    end;
  end
  else
  begin
    actnLoadPathAsSelection.Enabled := False;
  end;
end;

procedure TdmMain.actnDuplicateLayerExecute(Sender: TObject);
var
  LLayer   : TgmCustomLayer;
  LIndex   : Integer;
  LCommand : TgmCustomCommand;
begin
  frmDuplicateLayer := TfrmDuplicateLayer.Create(nil);
  try
    frmDuplicateLayer.lblDuplicateLayer.Caption :=
      'Duplicate:  ' + ActiveChildForm.LayerList.SelectedLayer.LayerName;
      
    frmDuplicateLayer.edtDuplicateLayerName.Text :=
      ActiveChildForm.LayerList.SelectedLayer.LayerName + '  copy';

    if frmDuplicateLayer.ShowModal = mrOK then
    begin
      Screen.Cursor := crHourGlass;
      try
        if ActiveChildForm.LayerList.SelectedLayer is TgmVectorLayer then
        begin
          if ActiveChildForm.FigureManager.SelectedFigureCount > 0 then
          begin
            ActiveChildForm.FigureManager.DeselectAllFigures();
          end;
        end;

        LIndex := ActiveChildForm.LayerList.SelectedIndex + 1;
        LLayer := ActiveChildForm.LayerList.SelectedLayer.GetCopy();

        // NOTE : set IsDuplicated to true before insert it to list will
        // cause the list would not give it an initial name.
        LLayer.IsDuplicated := True;
        
        ActiveChildForm.LayerList.Insert(LIndex, LLayer);

        with ActiveChildForm.LayerList.SelectedLayer do
        begin
          LayerName := frmDuplicateLayer.edtDuplicateLayerName.Text;

          if IsLayerThumbEnabled then
          begin
            UpdateLayerThumbnail();
          end;

          if IsMaskEnabled then
          begin
            UpdateMaskThumbnail();
          end;

          if IsLogoThumbEnabled then
          begin
            UpdateLogoThumbnail();
          end;
        end;

        // Undo/Redo
        LCommand := TgmDuplicateLayerCommand.Create(
          ActiveChildForm.ChannelManager,
          ActiveChildForm.LayerList,
          ActiveChildForm.LayerList.SelectedLayer,
          ActiveChildForm.LayerList.SelectedIndex - 1);

        ActiveChildForm.CommandManager.AddCommand(LCommand);
      finally
        Screen.Cursor := crDefault;
      end;      
    end;
    
  finally
    FreeAndNil(frmDuplicateLayer);
  end;    
end;

procedure TdmMain.actnDuplicateLayerUpdate(Sender: TObject);
begin
  actnDuplicateLayer.Enabled := Assigned(ActiveChildForm) and
                                (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnLayerPropertiesExecute(Sender: TObject);
var
  LOldName       : ShortString;
  LNewName       : ShortString;
  LRenamedBefore : Boolean;
  LCommand       : TgmCustomCommand;
begin
  frmLayerProperties := TfrmLayerProperties.Create(nil);
  try
    // Undo/Redo
    LOldName       := ActiveChildForm.LayerList.SelectedLayer.LayerName;
    LRenamedBefore := ActiveChildForm.LayerList.SelectedLayer.IsRenamed;

    with frmLayerProperties do
    begin
      edtLayerName.Text := ActiveChildForm.LayerList.SelectedLayer.LayerName;

      if ShowModal = mrOK then
      begin
        if edtLayerName.Text <> '' then
        begin
          ActiveChildForm.LayerList.SelectedLayer.LayerName := edtLayerName.Text;
          ActiveChildForm.LayerList.SelectedLayer.IsRenamed := True;
          frmLayers.LayerPanelManager.Invalidate();

          // Undo/Redo
          LNewName := ActiveChildForm.LayerList.SelectedLayer.LayerName;

          LCommand := TgmLayerPropertiesCommand.Create(
            ActiveChildForm.ChannelManager,
            ActiveChildForm.LayerList,
            ActiveChildForm.LayerList.SelectedIndex,
            LOldName, LNewName, LRenamedBefore);

          ActiveChildForm.CommandManager.AddCommand(LCommand);
        end;
      end;
    end;
  finally
    FreeAndNil(frmLayerProperties);
  end;
end;

procedure TdmMain.actnLayerPropertiesUpdate(Sender: TObject);
begin
  actnLayerProperties.Enabled := Assigned(ActiveChildForm) and
                                 (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnBringLayerToFrontExecute(Sender: TObject);
begin
  with ActiveChildForm do
  begin
    FLastLayerOrderType := lotBringToFront;  // for Undo/Redo command name

    LayerList.Move(LayerList.SelectedIndex, LayerList.MaxIndex);
    LayerList.SelectLayer(LayerList.MaxIndex);

    if frmLayers.LayerPanelManager.ScrollSelectedPanelInViewport() then
    begin
      frmLayers.LayerPanelManager.Invalidate();
    end;
  end;
end;

procedure TdmMain.actnBringLayerToFrontUpdate(Sender: TObject);
begin
  actnBringLayerToFront.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus()) and 
                                   (ActiveChildForm.LayerList.SelectedIndex < ActiveChildForm.LayerList.MaxIndex);
end;

procedure TdmMain.actnNewFillLayerExecute(Sender: TObject);
begin
  // dummy
end;

procedure TdmMain.actnNewFillLayerUpdate(Sender: TObject);
begin
  actnNewFillLayer.Enabled := Assigned(ActiveChildForm) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnNewAdjustmentLayerExecute(Sender: TObject);
begin
  // dummy
end;

procedure TdmMain.actnNewAdjustmentLayerUpdate(Sender: TObject);
begin
  actnNewAdjustmentLayer.Enabled := Assigned(ActiveChildForm) and
                                    (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnArrangeLayerExecute(Sender: TObject);
begin
  // dummy
end;

procedure TdmMain.actnArrangeLayerUpdate(Sender: TObject);
begin
  actnArrangeLayer.Enabled := Assigned(ActiveChildForm) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnBringLayerForwardExecute(Sender: TObject);
var
  LTargetIndex : Integer;
begin
  with ActiveChildForm do
  begin
    FLastLayerOrderType := lotBringForward;  // for Undo/Redo command name

    LTargetIndex := LayerList.SelectedIndex + 1;
    
    LayerList.Move(LayerList.SelectedIndex, LTargetIndex);
    LayerList.SelectLayer(LTargetIndex);

    if frmLayers.LayerPanelManager.ScrollSelectedPanelInViewport() then
    begin
      frmLayers.LayerPanelManager.Invalidate();
    end;
  end;
end;

procedure TdmMain.actnBringLayerForwardUpdate(Sender: TObject);
begin
  actnBringLayerForward.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus()) and
                                   (ActiveChildForm.LayerList.SelectedIndex < ActiveChildForm.LayerList.MaxIndex);
end;

procedure TdmMain.actnSendLayerBackwardExecute(Sender: TObject);
var
  LTargetIndex : Integer;
begin
  with ActiveChildForm do
  begin
    FLastLayerOrderType := lotSendBackward;  // for Undo/Redo command name

    LTargetIndex := LayerList.SelectedIndex - 1;

    LayerList.Move(LayerList.SelectedIndex, LTargetIndex);
    LayerList.SelectLayer(LTargetIndex);

    if frmLayers.LayerPanelManager.ScrollSelectedPanelInViewport() then
    begin
      frmLayers.LayerPanelManager.Invalidate();
    end;
  end;
end;

procedure TdmMain.actnSendLayerBackwardUpdate(Sender: TObject);
begin
  actnSendLayerBackward.Enabled := Assigned(ActiveChildForm) and
                                   (not ActiveChildForm.ToolsInPendingStatus()) and
                                   (ActiveChildForm.LayerList.SelectedIndex > 0);
end;

procedure TdmMain.actnSendLayerToBackExecute(Sender: TObject);
begin
  with ActiveChildForm do
  begin
    FLastLayerOrderType := lotSendToBack;  // for Undo/Redo command name

    LayerList.Move(LayerList.SelectedIndex, 0);
    LayerList.SelectLayer(0);

    if frmLayers.LayerPanelManager.ScrollSelectedPanelInViewport() then
    begin
      frmLayers.LayerPanelManager.Invalidate();
    end;
  end;
end;

procedure TdmMain.actnSendLayerToBackUpdate(Sender: TObject);
begin
  actnSendLayerToBack.Enabled := Assigned(ActiveChildForm) and
                                 (not ActiveChildForm.ToolsInPendingStatus()) and
                                 (ActiveChildForm.LayerList.SelectedIndex > 0);
end;

procedure TdmMain.actnMergeLayerDownExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    // deselect all movable figures before merging layers
    if FigureManager.SelectedFigureCount() > 0 then
    begin
      FigureManager.DeselectFiguresOnSelectedLayer();
    end;

    // Undo/Redo, first
    LCommand := TgmLayerMergenceCommand.Create(ChannelManager, LayerList,
      frmRichTextEditor.rchedtRichTextEditor, mmMergeDown);

    LayerList.MergeSelectedLayerDown();

    CommandManager.AddCommand(LCommand);  // Undo/Redo
  end;
end;

procedure TdmMain.actnMergeLayerDownUpdate(Sender: TObject);
begin
  actnMergeLayerDown.Enabled := Assigned(ActiveChildForm) and
                                (not ActiveChildForm.ToolsInPendingStatus()) and
                                ActiveChildForm.LayerList.CanMergeSelectedLayerDown();
end;

procedure TdmMain.actnMergeVisibleLayersExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    // deselect all movable figures before merging layers
    if FigureManager.SelectedFigureCount() > 0 then
    begin
      FigureManager.DeselectFiguresOnVisibleLayers();
    end;

    // Undo/Redo, first
    LCommand := TgmLayerMergenceCommand.Create(ChannelManager, LayerList,
      frmRichTextEditor.rchedtRichTextEditor, mmMergeVisible);

    LayerList.MergeVisibleLayers();

    CommandManager.AddCommand(LCommand);  // Undo/Redo
  end; 
end;

procedure TdmMain.actnMergeVisibleLayersUpdate(Sender: TObject);
begin
  actnMergeVisibleLayers.Enabled := Assigned(ActiveChildForm) and
                                    (not ActiveChildForm.ToolsInPendingStatus()) and
                                    ActiveChildForm.LayerList.CanMergeVisbleLayers();
end;

procedure TdmMain.actnFlattenImageExecute(Sender: TObject);
var
  LDoFlatten : Boolean;
  LCommand   : TgmCustomCommand;
begin
  LDoFlatten := False;
  
  with ActiveChildForm do
  begin
    if LayerList.GetHiddenLayerCount() > 0 then
    begin
      if MessageDlg('Discard hidden layers?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      begin
        LDoFlatten := True;
      end;
    end
    else
    begin
      LDoFlatten := True;
    end;

    if LDoFlatten then
    begin
      // deselect all movable figures before merging layers
      if FigureManager.SelectedFigureCount() > 0 then
      begin
        FigureManager.DeselectAllFigures();
      end;

      // Undo/Redo, first
      LCommand := TgmLayerMergenceCommand.Create(ChannelManager, LayerList,
        frmRichTextEditor.rchedtRichTextEditor, mmFlatten);

      LayerList.FlattenLayers();

      CommandManager.AddCommand(LCommand);  // Undo/Redo
    end;
  end;
end;

procedure TdmMain.actnFlattenImageUpdate(Sender: TObject);
begin
  actnFlattenImage.Enabled := Assigned(ActiveChildForm) and
                              (not ActiveChildForm.ToolsInPendingStatus()) and
                              ActiveChildForm.LayerList.CanFlattenLayers();
end;

procedure TdmMain.actnImageSizeExecute(Sender: TObject);
var
  LNewWidth, LNewHeight : Integer;
  LOldWidth, LOldHeight : Integer;
  LRect                 : TRect;
  LCommand              : TgmCustomCommand;
begin
  frmImageSize := TfrmImageSize.Create(nil);
  try
    if frmImageSize.ShowModal = mrOK then
    begin
      with ActiveChildForm do
      begin
        LOldWidth  := imgWorkArea.Bitmap.Width;
        LOldHeight := imgWorkArea.Bitmap.Height;
        LNewWidth  := frmImageSize.NewWidth;
        LNewHeight := frmImageSize.NewHeight;

        if (LNewWidth <> LOldWidth) or (LNewHeight <> LOldHeight) then
        begin
          // Undo/Redo, first
          LCommand := TgmImageSizeCommand.Create(ChannelManager, LayerList,
            PathList, frmRichTextEditor.rchedtRichTextEditor,
            LNewWidth, LNewHeight, frmImageSize.ResamplingOptions);

          // resize the background pattern
          CheckerboardBmp.SetSize(LNewWidth, LNewHeight);
          
          DrawCheckerboardPattern( CheckerboardBmp,
            Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

          // resize layers
          LayerList.ResizeImageOfLayers(LNewWidth, LNewHeight,
            frmImageSize.ResamplingOptions);

          // change the size of channels
          ChannelManager.ResizeChannels(LNewWidth, LNewHeight);

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

          // Undo/Redo
          CommandManager.AddCommand(LCommand);
        end;
      end;
    end;
  finally
    FreeAndNil(frmImageSize);
  end;
end;

procedure TdmMain.actnImageSizeUpdate(Sender: TObject);
begin
  actnImageSize.Enabled := Assigned(ActiveChildForm) and
                           (ActiveChildForm.Selection = nil) and
                           (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnCanvasSizeExecute(Sender: TObject);
var
  LNewWidth, LNewHeight : Integer;
  LOldWidth, LOldHeight : Integer;
  LRect                 : TRect;
  LCommand              : TgmCustomCommand;
begin
  frmCanvasSize := TfrmCanvasSize.Create(nil);
  try
    if frmCanvasSize.ShowModal = mrOK then
    begin
      with ActiveChildForm do
      begin
        LOldWidth  := imgWorkArea.Bitmap.Width;
        LOldHeight := imgWorkArea.Bitmap.Height;
        LNewWidth  := frmCanvasSize.NewWidth;
        LNewHeight := frmCanvasSize.NewHeight;

        if (LNewWidth <> LOldWidth) or (LNewHeight <> LOldHeight) then
        begin
          // Undo/Redo, first
          LCommand := TgmCanvasSizeCommand.Create(ChannelManager, LayerList,
            PathList, frmRichTextEditor.rchedtRichTextEditor,
            LNewWidth, LNewHeight, frmCanvasSize.AnchorDirection,
            Color32(frmMain.GlobalBackColor));

          // resize the background pattern
          CheckerboardBmp.SetSize(LNewWidth, LNewHeight);
          
          DrawCheckerboardPattern( CheckerboardBmp,
            Round(DEFAULT_CHECKERBOARD_SIZE / imgWorkArea.Scale) );

          // resize canvas of each layer ...
          LayerList.ResizeCanvasOfLayers(LNewWidth, LNewHeight,
            frmCanvasSize.AnchorDirection, Color32(frmMain.GlobalBackColor));

          // change the canvas size of each channel
          ChannelManager.ResizeCanvasOfChannels(
            LNewWidth, LNewHeight, frmCanvasSize.AnchorDirection);

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

          // Undo/Redo
          CommandManager.AddCommand(LCommand);
        end;
      end;
    end;
  finally
    FreeAndNil(frmCanvasSize);
  end;
end;

procedure TdmMain.actnCanvasSizeUpdate(Sender: TObject);
begin
  actnCanvasSize.Enabled := Assigned(ActiveChildForm) and
                            (ActiveChildForm.Selection = nil) and
                            (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnCropExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    LCommand := TgmCropCommand.Create(
      ChannelManager, LayerList, PathList,
      frmRichTextEditor.rchedtRichTextEditor,
      Crop, Color32(frmMain.GlobalBackColor));

    CommitCrop();

    CommandManager.AddCommand(LCommand);  // Undo/Redo
  end;

  frmMain.edtCropWidth.Text  := '';
  frmMain.edtCropHeight.Text := '';
end;

procedure TdmMain.actnCropUpdate(Sender: TObject);
begin
  actnCrop.Enabled := Assigned(ActiveChildForm) and
                      Assigned(ActiveChildForm.Crop);
end;

procedure TdmMain.actnCommitCropExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  with ActiveChildForm do
  begin
    // Undo/Redo, first
    LCommand := TgmCropCommand.Create(
      ChannelManager, LayerList, PathList,
      frmRichTextEditor.rchedtRichTextEditor,
      Crop, Color32(frmMain.GlobalBackColor));

    CommitCrop();

    CommandManager.AddCommand(LCommand);  // Undo/Redo
  end;

  frmMain.edtCropWidth.Text  := '';
  frmMain.edtCropHeight.Text := '';
end;

procedure TdmMain.actnCommitCropUpdate(Sender: TObject);
begin
  actnCommitCrop.Enabled := Assigned(ActiveChildForm) and
                            Assigned(ActiveChildForm.Crop);
end;

procedure TdmMain.actnCancelCropExecute(Sender: TObject);
begin
  ActiveChildForm.CancelCrop();

  frmMain.edtCropWidth.Text  := '';
  frmMain.edtCropHeight.Text := '';
end;

procedure TdmMain.actnCancelCropUpdate(Sender: TObject);
begin
  actnCancelCrop.Enabled := Assigned(ActiveChildForm) and
                            Assigned(ActiveChildForm.Crop);
end;

procedure TdmMain.actn30dcwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    30, rdClockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 30, rdClockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn30dcwUpdate(Sender: TObject);
begin
  actn30dcw.Enabled := Assigned(ActiveChildForm) and
                       (ActiveChildForm.Selection = nil) and
                       (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn45dcwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    45, rdClockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 45, rdClockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn45dcwUpdate(Sender: TObject);
begin
  actn45dcw.Enabled := Assigned(ActiveChildForm) and
                       (ActiveChildForm.Selection = nil) and
                       (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn60dcwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    60, rdClockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 60, rdClockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn60dcwUpdate(Sender: TObject);
begin
  actn60dcw.Enabled := Assigned(ActiveChildForm) and
                       (ActiveChildForm.Selection = nil) and
                       (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn90dcwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    90, rdClockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 90, rdClockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn90dcwUpdate(Sender: TObject);
begin
  actn90dcw.Enabled := Assigned(ActiveChildForm) and
                       (ActiveChildForm.Selection = nil) and
                       (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn180dcwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    180, rdClockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 180, rdClockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn180dcwUpdate(Sender: TObject);
begin
  actn180dcw.Enabled := Assigned(ActiveChildForm) and
                        (ActiveChildForm.Selection = nil) and
                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn30ducwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    30, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  DoRotation( 30, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn30ducwUpdate(Sender: TObject);
begin
  actn30ducw.Enabled := Assigned(ActiveChildForm) and
                        (ActiveChildForm.Selection = nil) and
                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn45ducwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    45, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 45, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn45ducwUpdate(Sender: TObject);
begin
  actn45ducw.Enabled := Assigned(ActiveChildForm) and
                        (ActiveChildForm.Selection = nil) and
                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn60ducwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    60, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 60, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn60ducwUpdate(Sender: TObject);
begin
  actn60ducw.Enabled := Assigned(ActiveChildForm) and
                        (ActiveChildForm.Selection = nil) and
                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn90ducwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    90, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 90, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn90ducwUpdate(Sender: TObject);
begin
  actn90ducw.Enabled := Assigned(ActiveChildForm) and
                        (ActiveChildForm.Selection = nil) and
                        (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actn180ducwExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  // Undo/Redo, first
  LCommand := TgmRotateCanvasCommand.Create(
    ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
    ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
    180, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  // then, rotating
  DoRotation( 180, rdCounterclockwise, Color32(frmMain.GlobalBackColor) );

  ActiveChildForm.CommandManager.AddCommand(LCommand);
end;

procedure TdmMain.actn180ducwUpdate(Sender: TObject);
begin
  actn180ducw.Enabled := Assigned(ActiveChildForm) and
                         (ActiveChildForm.Selection = nil) and
                         (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnRotateArbitraryExecute(Sender: TObject);
var
  LCommand : TgmCustomCommand;
begin
  frmRotateCanvas := TfrmRotateCanvas.Create(nil);
  try
    if frmRotateCanvas.ShowModal = mrOK then
    begin
      // Undo/Redo, first
      LCommand := TgmRotateCanvasCommand.Create(
        ActiveChildForm.ChannelManager, ActiveChildForm.LayerList,
        ActiveChildForm.PathList, frmRichTextEditor.rchedtRichTextEditor,
        frmRotateCanvas.RotateAngle, frmRotateCanvas.RotateDirection,
        Color32(frmMain.GlobalBackColor) );

      // then, rotating
      DoRotation( frmRotateCanvas.RotateAngle,
                  frmRotateCanvas.RotateDirection,
                  Color32(frmMain.GlobalBackColor) );

      ActiveChildForm.CommandManager.AddCommand(LCommand);
    end;
  finally
    FreeAndNil(frmRotateCanvas);
  end;
end;

procedure TdmMain.actnRotateArbitraryUpdate(Sender: TObject);
begin
  actnRotateArbitrary.Enabled := Assigned(ActiveChildForm) and
                                 (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnRotateCanvasExecute(Sender: TObject);
begin
  // Dummy
end;

procedure TdmMain.actnRotateCanvasUpdate(Sender: TObject);
begin
  actnRotateCanvas.Enabled := Assigned(ActiveChildForm) and
                              (ActiveChildForm.Selection = nil) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnAutoLevelsExecute(Sender: TObject);
var
  LSuccess     : Boolean;
  LCommandName : string;
begin
  LSuccess     := False;
  LCommandName := 'Auto Levels';

  with ActiveChildForm do
  begin
    // cannot process more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Can not process more than one alpha channel at a time.',
                   mtError, [mbOK], 0);

        Exit;
      end;
    end;

    // cannot process special layers
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if not (LayerList.SelectedLayer is TgmNormalLayer) then
      begin
        Exit;
      end;

      // cannot process empty images
      if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer() then
      begin
        MessageDlg('Could not complete the Auto Levels command' + #10#13 +
                   'because the active layer is empty.', mtError, [mbOK], 0);

        Exit;
      end;
    end;

    SaveChannelPixelsBeforePixelProcessing();  // for Undo/Redo

    if Assigned(Selection) then
    begin
      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        LSuccess := GimpAutoLevels(Selection.CutOriginal,
                                   ChannelManager.SelectedColorChannels);
      end
      else
      begin
        LSuccess := GimpAutoLevels(Selection.CutOriginal, [csGrayscale]);
      end;

      if ChannelManager.CurrentChannelType in [ctAlphaChannel,
                                               ctQuickMaskChannel,
                                               ctLayerMaskChannel] then
      begin
        Desaturate32(Selection.CutOriginal);
      end;

      ShowProcessedSelection();
      Selection.IsAnimated := True;

      // need this two lines to update the mask channel thumbnail
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
          0, 0, LayerList.SelectedLayer.MaskBitmap);

        imgWorkArea.Changed();
      end;
    end
    else
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              LSuccess := GimpAutoLevels(
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                [csGrayscale]);

              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              LSuccess := GimpAutoLevels(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                [csGrayscale]);

              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
            end;
          end;

        ctLayerMaskChannel:
          begin
            LSuccess := GimpAutoLevels(LayerList.SelectedLayer.MaskBitmap,
                                       [csGrayscale]);

            // update the mask channel in Channel Manager
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                0, 0, LayerList.SelectedLayer.MaskBitmap);
            end;

            LayerList.SelectedLayer.Changed();
          end;

        ctColorChannel:
          begin
            if LayerList.SelectedLayer is TgmNormalLayer then
            begin
              LSuccess := GimpAutoLevels(LayerList.SelectedLayer.LayerBitmap,
                                         ChannelManager.SelectedColorChannels);

              LayerList.SelectedLayer.Changed();
            end;
          end;
      end;
    end;

    if LSuccess then
    begin
      // update thumbnails
      UpdateThumbnailsBySelectedChannel();

      // Undo/Redo
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnAutoLevelsUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnAutoLevels.Enabled := LEnabled;
end;

procedure TdmMain.actnDesaturateExecute(Sender: TObject);
var
  LCommandName : string;
begin
  LCommandName := 'Desaturate';
  
  with ActiveChildForm do
  begin
    // could not process on empty layers
    if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer() then
    begin
      MessageDlg('Could not complete the Desaturate command' + #10#13 +
                 'because the active layer is empty.', mtError, [mbOK], 0);

      Exit;
    end;

    SaveChannelPixelsBeforePixelProcessing();  // for Undo/Redo

    if Assigned(Selection) then
    begin
      Selection.IsAnimated := False;
      Desaturate32(Selection.CutOriginal);
      ShowProcessedSelection();
      Selection.IsAnimated := True;
    end
    else
    begin
      // only need to deal with normal layers ...
      Desaturate32(LayerList.SelectedLayer.LayerBitmap);
      LayerList.SelectedLayer.Changed();
    end;
    
    LayerList.SelectedLayer.UpdateLayerThumbnail();

    // Undo/Redo
    CreateUndoRedoCommandForPixelProcessing(LCommandName);
  end;
end;

procedure TdmMain.actnDesaturateUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnDesaturate.Enabled := LEnabled;
end;

procedure TdmMain.actnInvertExecute(Sender: TObject);
var
  LCommandName : string;
begin
  LCommandName := 'Invert';

  with ActiveChildForm do
  begin
    // cannot not process more than one alpha channel at a time
    if ChannelManager.CurrentChannelType = ctAlphaChannel then
    begin
      if not Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        MessageDlg('Can not process more than one alpha channel at a time.',
                   mtError, [mbOK], 0);
                   
        Exit;
      end;
    end;

    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // cannot process on special layers
      if not (LayerList.SelectedLayer is TgmNormalLayer) then
      begin
        Exit;
      end;
    end;

    // could not process on empty layers
    if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer() then
    begin
      MessageDlg('Could not complete the Invert command' + #10#13 +
                 'because the active layer is empty.', mtError, [mbOK], 0);
                 
      Exit;
    end;

    SaveChannelPixelsBeforePixelProcessing();  // for Undo/Redo

    if Assigned(Selection) then
    begin
      Selection.IsAnimated := False;

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        InvertBitmap32(Selection.CutOriginal, ChannelManager.SelectedColorChannels);
      end
      else
      begin
        InvertBitmap32(Selection.CutOriginal, [csGrayscale]);
      end;

      ShowProcessedSelection();
      Selection.IsAnimated := True;

      // need this two lines to update the mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
          0, 0, LayerList.SelectedLayer.MaskBitmap);

        imgWorkArea.Changed();
      end;
    end
    else
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              InvertBitmap32(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap,
                             [csGrayscale]);

              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              InvertBitmap32(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
                             [csGrayscale]);

              ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
            end;
          end;

        ctLayerMaskChannel:
          begin
            InvertBitmap32(LayerList.SelectedLayer.MaskBitmap, [csGrayscale]);

            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                0, 0, LayerList.SelectedLayer.MaskBitmap);
            end;

            LayerList.SelectedLayer.Changed();
          end;

        ctColorChannel:
          begin
            InvertBitmap32(LayerList.SelectedLayer.LayerBitmap,
                           ChannelManager.SelectedColorChannels);

            LayerList.SelectedLayer.Changed();
          end;
      end;
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    CreateUndoRedoCommandForPixelProcessing(LCommandName);
  end;
end;

procedure TdmMain.actnInvertUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());
    end;
  end;

  actnInvert.Enabled := LEnabled;
end;

procedure TdmMain.actnReplaceColorExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Replace Color';

  with ActiveChildForm do
  begin
    // we could only process when full color channel selected or
    // one color channel selected
    if ChannelManager.ColorChannelList.SelectedChannelCount = 2 then
    begin
      Exit;
    end;

    if TgmNormalLayer(LayerList.SelectedLayer).IsEmptyLayer() then
    begin
      MessageDlg('Could not complete the Replace Color command' + #10#13 +
                 'because the active layer is empty.', mtError, [mbOK], 0);

      Exit;
    end;

    frmReplaceColor := TfrmReplaceColor.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmReplaceColor.ShowModal();

      case LModalResult of
        mrOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        mrCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
        Selection.IsAnimated := True;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctQuickMaskChannel:
            begin
              if Assigned(ChannelManager.QuickMaskChannel) then
              begin
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
              LayerList.SelectedLayer.Changed();
            end;
        end;
      end;

    finally
      FreeAndNil(frmReplaceColor);
    end;

    // update thumbnails
    UpdateThumbnailsBySelectedChannel();

    // Undo/Redo
    if LModalResult = mrOK then
    begin
      CreateUndoRedoCommandForPixelProcessing(LCommandName);
    end;
  end;
end;

procedure TdmMain.actnReplaceColorUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (ChannelManager.CurrentChannelType <> ctColorChannel) or
                  (LayerList.SelectedLayer is TgmNormalLayer);

      LEnabled := LEnabled and (not ToolsInPendingStatus());

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if ChannelManager.ColorChannelList.SelectedChannelCount = 2 then
        begin
          LEnabled := False;
        end;
      end;
    end;
  end;

  actnReplaceColor.Enabled := LEnabled;
end;

procedure TdmMain.actnAdjustmentsExecute(Sender: TObject);
begin
  // Dummy
end;

procedure TdmMain.actnAdjustmentsUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (not ToolsInPendingStatus()) and
                  ( (ChannelManager.CurrentChannelType in [ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel]) or
                    (LayerList.SelectedLayer is TgmNormalLayer) );
    end;
  end;
  
  actnAdjustments.Enabled := LEnabled;
end;

procedure TdmMain.actnApplyImageExecute(Sender: TObject);
var
  LResultBmp   : TBitmap32;
  LModalResult : TModalResult;
  LCommandName : string;
begin
  LResultBmp   := nil;
  LCommandName := 'Apply Image';

  with ActiveChildForm do
  begin
    frmApplyImage := TfrmApplyImage.Create(Application);
    try
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := False;
      end;

      LModalResult := frmApplyImage.ShowModal();

      case LModalResult of
        idOK:
          begin
            LResultBmp := frmMain.FBitmapAfter;
          end;

        idCancel:
          begin
            LResultBmp := frmMain.FBitmapBefore;
          end;
      end;

      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(LResultBmp);
        ShowProcessedSelection();
        Selection.IsAnimated := True;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(LResultBmp);
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
              end;
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(LResultBmp);

              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              LayerList.SelectedLayer.LayerBitmap.Assign(LResultBmp);
              LayerList.SelectedLayer.Changed();
            end;
        end;
      end;

      // update thumbnails
      UpdateThumbnailsBySelectedChannel();

      // Undo/Redo
      if LModalResult = mrOK then
      begin
        CreateUndoRedoCommandForPixelProcessing(LCommandName);
      end;

    finally
      FreeAndNil(frmApplyImage);
    end;
  end;
end;

procedure TdmMain.actnApplyImageUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (not ToolsInPendingStatus()) and 
                  (EditMode = emStandardMode) and
                  ( (LayerList.SelectedLayer is TgmNormalLayer) or
                    (ChannelManager.CurrentChannelType in
                      [ctAlphaChannel, ctLayerMaskChannel]) );

      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        LEnabled := LEnabled and
                    (ChannelManager.ColorChannelList.SelectedChannelCount <> 2);
      end;
    end;
  end;

  actnApplyImage.Enabled := LEnabled;
end;

procedure TdmMain.actnOptimalCropExecute(Sender: TObject);
begin
  ActiveChildForm.ExecuteOptimalCrop();
end;

procedure TdmMain.actnOptimalCropUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (LayerList.Count = 1) and
                  (LayerList.SelectedLayer is TgmNormalLayer) and
                  (ChannelManager.CurrentChannelType = ctColorChannel) and
                  (ChannelManager.ColorChannelList.SelectedChannelCount > 2) and
                  (Selection = nil) and
                  (not ToolsInPendingStatus());
    end;
  end;

  actnOptimalCrop.Enabled := LEnabled;
end;

procedure TdmMain.actnHistogramExecute(Sender: TObject);
begin
  frmHistogram := TfrmHistogram.Create(Application);
  try
    frmHistogram.ShowModal();
  finally
    FreeAndNil(frmHistogram);
  end;

  with ActiveChildForm do
  begin
    if Assigned(Selection) then
    begin
      if not Selection.IsAnimated then
      begin
        Selection.IsAnimated := True;
      end;
    end;
  end;
end;

procedure TdmMain.actnHistogramUpdate(Sender: TObject);
begin
  actnHistogram.Enabled := Assigned(ActiveChildForm) and
                           (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnFillExecute(Sender: TObject);
begin
  with ActiveChildForm do
  begin
    if Assigned(Selection) then
    begin
      Selection.IsAnimated := False;
    end;

    SaveChannelPixelsBeforePixelProcessing();  // for Undo/Redo

    if frmFill.ShowModal() = mrOK then
    begin
      if Assigned(Selection) then
      begin
        Selection.IsAnimated := True;
      end;

      // update thumbnails
      UpdateThumbnailsBySelectedChannel();

      // Undo/Redo
      CreateUndoRedoCommandForPixelProcessing('Fill',
        gmMiscCommandIcons.FILL_COMMAND_ICON_RES_NAME);
    end;
  end;
end;

procedure TdmMain.actnFillUpdate(Sender: TObject);
var
  LEnabled : Boolean;
begin
  LEnabled := False;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LEnabled := (not ToolsInPendingStatus()) and
                  ( (ChannelManager.CurrentChannelType <> ctColorChannel) or
                    (LayerList.SelectedLayer is TgmNormalLayer) );
    end;
  end;
  
  actnFill.Enabled := LEnabled;
end;

procedure TdmMain.actnGeneralPreferencesExecute(Sender: TObject);
begin
  frmPreferences := TfrmPreferences.Create(Application);
  try
    if frmPreferences.ShowModal() = mrOK then
    begin
      if ActiveChildForm <> nil then
      begin
        ActiveChildForm.CommandManager.CommandMaxCount := frmPreferences.HistoryStatesCount;
      end;
    end;
  finally
    FreeAndNil(frmPreferences);
  end;
end;

procedure TdmMain.actnNewFileExecute(Sender: TObject);
var
  LImageWidth  : Integer;
  LImageHeight : Integer;
  LRect        : TRect;
  LLayer       : TgmCustomLayer;  
begin
  frmCreateNewFile := TfrmCreateNewFile.Create(Application);
  try
    // open Create New File dialog for getting the new dimension for the image
    if frmCreateNewFile.ShowModal() = mrOK then
    begin
      LImageWidth  := frmCreateNewFile.BitmapWidth;
      LImageHeight := frmCreateNewFile.BitmapHeight;

      // create a new child form for holding the new created image
      frmMain.ChildFormIsCreating := True;
      try
        ActiveChildForm := TfrmChild.Create(frmMain);
      finally
        frmMain.ChildFormIsCreating := False;
      end;

      with ActiveChildForm do
      begin
        HistoryBitmap.SetSize(LImageWidth, LImageHeight);
        HistoryBitmap.FillRect(0, 0, LImageWidth, LImageHeight, clWhite32);

        // set background size before create background layer
        imgWorkArea.Bitmap.SetSize(LImageWidth, LImageHeight);
        imgWorkArea.Bitmap.Clear($00000000);

        // draw checkerboard pattern
        CheckerboardBmp.SetSizeFrom(imgWorkArea.Bitmap);
        DrawCheckerboardPattern(CheckerboardBmp, DEFAULT_CHECKERBOARD_SIZE);

        // create background layer
        LLayer := CreateNormalLayer(clWhite32, True);
        LayerList.Add(LLayer);

        // channels ...
        LRect             := imgWorkArea.GetBitmapRect;
        LRect.TopLeft     := imgWorkArea.ControlToBitmap(LRect.TopLeft);
        LRect.BottomRight := imgWorkArea.ControlToBitmap(LRect.BottomRight);

        ChannelManager.UpdateColorChannelThumbnails(LayerList.CombineResult);
        ChannelManager.ChannelLayerLocation := FloatRect(LRect);

        // Add a snapshot of the new image to the Command Manager.
        CommandManager.AddSnapshot(LayerList.CombineResult, 'Untitled');
        CommandManager.SelectSnapshot(0);

        if WindowState = wsNormal then
        begin
          // set client size of the new created child form
          ClientWidth  := LImageWidth  + imgWorkArea.ScrollBars.Size;
          ClientHeight := LImageHeight + imgWorkArea.ScrollBars.Size;

          if frmMain.IsGhostModeEnabled and frmMain.pnlToolOptions.Visible then
          begin
            if Left < frmMain.pnlToolOptions.Width then
            begin
              Left := frmMain.pnlToolOptions.Width;
            end;
          end;
        end;

        SetupOnChildFormActivate();
      end;
    end;
  finally
    FreeAndNil(frmCreateNewFile);
  end;
end;

procedure TdmMain.actnCloseChildFormExecute(Sender: TObject);
begin
  ActiveChildForm.Close();
end;

procedure TdmMain.actnCloseChildFormUpdate(Sender: TObject);
begin
  actnCloseChildForm.Enabled := Assigned(ActiveChildForm);
end;

procedure TdmMain.actnCloseAllChildFormsExecute(Sender: TObject);
var
  i: Integer;
begin
  // Close all opened forms
  if frmMain.MDIChildCount > 0 then
  begin
    for i := (frmMain.MDIChildCount - 1) downto 0 do
    begin
      frmMain.MDIChildren[i].Close();
    end;
  end;
end;

procedure TdmMain.actnCloseAllChildFormsUpdate(Sender: TObject);
begin
  actnCloseAllChildForms.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnPrintPreviewExecute(Sender: TObject);
begin
  frmPrintPreview.Left   := 0;
  frmPrintPreview.Top    := 0;
  frmPrintPreview.Width  := Screen.Width;
  frmPrintPreview.Height := Screen.Height - 30;

  frmPrintPreview.ShowModal();
end;

procedure TdmMain.actnPrintPreviewUpdate(Sender: TObject);
begin
  actnPrintPreview.Enabled := Assigned(ActiveChildForm);
end;

procedure TdmMain.actnPrintOptionsExecute(Sender: TObject);
begin
  frmPrintOptions.ShowModal();
end;

procedure TdmMain.actnPrintOptionsUpdate(Sender: TObject);
begin
  actnPrintOptions.Enabled := Assigned(ActiveChildForm) and
                              (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnPageSetupExecute(Sender: TObject);
begin
  if PrinterSetupDialog.Execute() then
  begin
    // Calling these routines must after the printer setup process is complete,
    // for making the print preview to take effect.
    frmPrintPreview.prntprvwPreview.BeginDoc;
    frmPrintPreview.prntprvwPreview.EndDoc;
  end;
end;

procedure TdmMain.actnPageSetupUpdate(Sender: TObject);
begin
  actnPageSetup.Enabled := Assigned(ActiveChildForm) and
                           (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnPrintImageExecute(Sender: TObject);
begin
  frmPrintPreview.DrawBitmapOnPreview();

  if frmPrintPreview.prntprvwPreview.State = psReady then
  begin
    if PrintDialog.Execute() then
    begin
      frmPrintPreview.prntprvwPreview.Print();
    end;
  end;
end;

procedure TdmMain.actnPrintImageUpdate(Sender: TObject);
begin
  actnPrintImage.Enabled := Assigned(ActiveChildForm) and
                            (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnExitProgramExecute(Sender: TObject);
begin
  frmMain.Close();
end;

procedure TdmMain.actnSaveFileExecute(Sender: TObject);
begin
  ActiveChildForm.SaveNamedFile();
end;

procedure TdmMain.actnSaveFileUpdate(Sender: TObject);
begin
  actnSaveFile.Enabled := Assigned(ActiveChildForm) and
                          (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnSaveFileAsExecute(Sender: TObject);
begin
  ActiveChildForm.SaveFileWithNewName();
end;

procedure TdmMain.actnSaveFileAsUpdate(Sender: TObject);
begin
  actnSaveFileAs.Enabled := Assigned(ActiveChildForm) and
                            (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnSaveAllExecute(Sender: TObject);
var
  i          : Integer;
  LChildForm : TfrmChild;
begin
  if frmMain.MDIChildCount > -1 then
  begin
    for i := (frmMain.MDIChildCount - 1) downto 0 do
    begin
      LChildForm := TfrmChild(frmMain.MDIChildren[i]);
      LChildForm.SaveNamedFile();
    end;
  end;
end;

procedure TdmMain.actnSaveAllUpdate(Sender: TObject);
begin
  actnSaveAll.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  FLastLayerOrderType := lotNone;
end;

procedure TdmMain.actnUndoRedoSwitcherExecute(Sender: TObject);
var
  LCommandIndex : Integer;
begin
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LCommandIndex := CommandManager.CommandList.SelectedIndex;

      case CommandManager.CommandTypeToggle of
        cttUndo:
          begin
            CommandManager.SelectCommand(LCommandIndex + 1);
          end;

        cttRedo:
          begin
            if LCommandIndex = 0 then
            begin
              CommandManager.SelectSnapshot(0);
            end
            else
            begin
              CommandManager.SelectCommand(LCommandIndex - 1);
            end;
          end;
      end;
    end;
  end;
end;

procedure TdmMain.actnUndoRedoSwitcherUpdate(Sender: TObject);
var
  LCommand      : TgmCustomCommand;
  LCommandIndex : Integer;
begin
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      actnUndoRedoSwitcher.Enabled := (CommandManager.CommandList.Count > 0) and
                                      (not ToolsInPendingStatus());
      
      if CommandManager.CommandList.Count > 0 then
      begin
        case CommandManager.CommandTypeToggle of
          cttUndo:
            begin
              LCommandIndex := CommandManager.CommandList.SelectedIndex + 1;
              LCommand      := CommandManager.CommandList.Commands[LCommandIndex];

              actnUndoRedoSwitcher.Caption := 'Redo ' + LCommand.CommandName;
            end;

          cttRedo:
            begin
              LCommandIndex := CommandManager.CommandList.SelectedIndex;
              LCommand      := CommandManager.CommandList.Commands[LCommandIndex];

              actnUndoRedoSwitcher.Caption := 'Undo ' + LCommand.CommandName;
            end;
        end;
      end
      else
      begin
        actnUndoRedoSwitcher.Caption := 'Undo';
      end;
    end;
  end
  else
  begin
    actnUndoRedoSwitcher.Caption := 'Undo';
    actnUndoRedoSwitcher.Enabled := False;
  end;
end;

procedure TdmMain.actnStepForewardExecute(Sender: TObject);
var
  LCommandIndex : Integer;
begin
  with ActiveChildForm do
  begin
    LCommandIndex := CommandManager.CommandList.SelectedIndex;
    CommandManager.SelectCommand(LCommandIndex + 1);
  end;
end;

procedure TdmMain.actnStepForewardUpdate(Sender: TObject);
var
  LIndex   : Integer;
  LCount   : Integer;
  LEnabled : Boolean;
begin
  if Assigned(ActiveChildForm) then
  begin
    LIndex   := ActiveChildForm.CommandManager.CommandList.SelectedIndex;
    LCount   := ActiveChildForm.CommandManager.CommandList.Count;
    LEnabled := ( LIndex < (LCount-1) ) and (not ActiveChildForm.ToolsInPendingStatus());
  end
  else
  begin
    LEnabled := False;
  end;

  actnStepForeward.Enabled := LEnabled;
end;

procedure TdmMain.actnStepBackwardExecute(Sender: TObject);
var
  LCommandIndex : Integer;
begin
  with ActiveChildForm do
  begin
    LCommandIndex := CommandManager.CommandList.SelectedIndex;

    if LCommandIndex > 0 then
    begin
      CommandManager.SelectCommand(LCommandIndex - 1);
    end
    else
    begin
      CommandManager.SelectSnapshot(0);
    end;
  end; 
end;

procedure TdmMain.actnStepBackwardUpdate(Sender: TObject);
var
  LIndex   : Integer;
  LEnabled : Boolean;
begin
  if Assigned(ActiveChildForm) then
  begin
    LIndex   := ActiveChildForm.CommandManager.CommandList.SelectedIndex;
    LEnabled := (LIndex >= 0) and (not ActiveChildForm.ToolsInPendingStatus());
  end
  else
  begin
    LEnabled := False;
  end;

  actnStepBackward.Enabled := LEnabled;
end;

procedure TdmMain.actnDeleteCurrentStateExecute(Sender: TObject);
var
  LCommandIndex : Integer;
begin
  with ActiveChildForm do
  begin
    if CommandManager.CommandList.SelectedIndex = 0 then
    begin
      CommandManager.SelectSnapshot(0);
      CommandManager.DeleteCommandsFromSpecifiedIndex(0);
    end
    else if CommandManager.CommandList.SelectedIndex > 0 then
    begin
      LCommandIndex := CommandManager.CommandList.SelectedIndex;

      CommandManager.SelectCommand(LCommandIndex - 1);
      CommandManager.DeleteCommandsFromSpecifiedIndex(LCommandIndex);
    end;

    frmHistory.CommandViewer.Invalidate();
  end;
end;

procedure TdmMain.actnDeleteCurrentStateUpdate(Sender: TObject);
begin
  actnDeleteCurrentState.Enabled := Assigned(ActiveChildForm) and
                                    (ActiveChildForm.CommandManager.CommandList.SelectedIndex >= 0) and
                                    (not ActiveChildForm.ToolsInPendingStatus());
end;

procedure TdmMain.actnOpenFileExecute(Sender: TObject);
var
  LNewPath : string;
begin
  OpenPictureDialog.InitialDir := ReadInfoFromIniFile(SECTION_SETUP,
    IDENT_OPEN_IMAGE_DIR, ExtractFilePath( ParamStr(0) ));
    
  OpenPictureDialog.FilterIndex := 1; // open files of types that all supported by this program

  if OpenPictureDialog.Execute then
  begin
    if OpenPictureDialog.FileName <> '' then
    begin
      if FileExists(OpenPictureDialog.FileName) then
      begin
        LNewPath := ExtractFilePath(OpenPictureDialog.FileName);

        if not frmMain.ShowOpenedImageTop(OpenPictureDialog.FileName) then
        begin
          // Update INI file
          WriteInfoToINIFile(SECTION_SETUP, IDENT_OPEN_IMAGE_DIR, LNewPath);

          if frmMain.OpenImageInChildForm(OpenPictureDialog.FileName) then
          begin
            frmMain.AddFilePathToList(frmMain.OpenRecentPathList, OpenPictureDialog.FileName);
            frmMain.UpdateOpenRecentMenuItem();
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmMain.actnOpenRecentExecute(Sender: TObject);
begin
  // Dummy
end;

procedure TdmMain.actnOpenRecentUpdate(Sender: TObject);
begin
  actnOpenRecent.Enabled := (frmMain.OpenRecentMenuList.Count > 0);
end;

procedure TdmMain.actnFiltersUpdateExecute(Sender: TObject);
begin
  frmMain.ReloadFilters();
end;

procedure TdmMain.actnGhostModeExecute(Sender: TObject);
begin
  if frmMain.IsGhostModeEnabled then
  begin
    frmMain.spdbtnGhost.Down := not frmMain.spdbtnGhost.Down;
    frmMain.GhostsFade(frmMain.spdbtnGhost.Down);
  end;
end;

procedure TdmMain.actnGhostModeUpdate(Sender: TObject);
begin
  actnGhostMode.Enabled := frmMain.IsGhostModeEnabled;
  actnGhostMode.Checked := frmMain.spdbtnGhost.Down and frmMain.IsGhostModeEnabled;
end;

procedure TdmMain.actnGhostSleepingModeExecute(Sender: TObject);
begin
  if frmMain.IsGhostModeEnabled then
  begin
    frmMain.spdbtnUntouched.Down := not frmMain.spdbtnUntouched.Down;
    frmMain.spdbtnUntouchedClick(nil);
  end;
end;

procedure TdmMain.actnGhostSleepingModeUpdate(Sender: TObject);
begin
  actnGhostSleepingMode.Enabled := frmMain.IsGhostModeEnabled;
  actnGhostSleepingMode.Checked := frmMain.spdbtnUntouched.Down and frmMain.IsGhostModeEnabled;
end;

procedure TdmMain.actnChannelFormExecute(Sender: TObject);
begin
  if frmChannels.Visible then
  begin
    frmChannels.Close();
  end
  else
  begin
    frmChannels.Show();
  end;
end;

procedure TdmMain.actnChannelFormUpdate(Sender: TObject);
begin
  actnChannelForm.Checked := frmChannels.Visible;
end;

procedure TdmMain.actnColorFormExecute(Sender: TObject);
begin
  if frmColor.Visible then
  begin
    frmColor.Close();
  end
  else
  begin
    frmColor.Show();
  end
end;

procedure TdmMain.actnColorFormUpdate(Sender: TObject);
begin
  actnColorForm.Checked := frmColor.Visible;
end;

procedure TdmMain.actnHistoryFormExecute(Sender: TObject);
begin
  if frmHistory.Visible then
  begin
    frmHistory.Close();
  end
  else
  begin
    frmHistory.Show();
  end;
end;

procedure TdmMain.actnHistoryFormUpdate(Sender: TObject);
begin
  actnHistoryForm.Checked := frmHistory.Visible;
end;

procedure TdmMain.actnInfoFormExecute(Sender: TObject);
begin
  if frmInfo.Visible then
  begin
    frmInfo.Close();
  end
  else
  begin
    frmInfo.Show();
  end;
end;

procedure TdmMain.actnInfoFormUpdate(Sender: TObject);
begin
  actnInfoForm.Checked := frmInfo.Visible;
end;

procedure TdmMain.actnLayerFormExecute(Sender: TObject);
begin
  if frmLayers.Visible then
  begin
    frmLayers.Close();
  end
  else
  begin
    frmLayers.Show();
  end;
end;

procedure TdmMain.actnLayerFormUpdate(Sender: TObject);
begin
  actnLayerForm.Checked := frmLayers.Visible;
end;

procedure TdmMain.actnPathFormExecute(Sender: TObject);
begin
  if frmPaths.Visible then
  begin
    frmPaths.Close();
  end
  else
  begin
    frmPaths.Show();
  end;
end;

procedure TdmMain.actnPathFormUpdate(Sender: TObject);
begin
  actnPathForm.Checked := frmPaths.Visible;
end;

procedure TdmMain.actnStatusBarExecute(Sender: TObject);
begin
  frmMain.stsbrMain.Visible := not frmMain.stsbrMain.Visible;
end;

procedure TdmMain.actnStatusBarUpdate(Sender: TObject);
begin
  actnStatusBar.Checked := frmMain.stsbrMain.Visible;
end;

procedure TdmMain.actnSwatchFormExecute(Sender: TObject);
begin
  if frmSwatch.Visible then
  begin
    frmSwatch.Close();
  end
  else
  begin
    frmSwatch.Show();
  end;
end;

procedure TdmMain.actnSwatchFormUpdate(Sender: TObject);
begin
  actnSwatchForm.Checked := frmSwatch.Visible;
end;

procedure TdmMain.actnCascadeFormsExecute(Sender: TObject);
begin
  frmMain.Cascade();
end;

procedure TdmMain.actnCascadeFormsUpdate(Sender: TObject);
begin
  actnCascadeForms.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnTileFormsExecute(Sender: TObject);
begin
  // Dummy
end;

procedure TdmMain.actnTileFormsUpdate(Sender: TObject);
begin
  actnTileForms.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnArrangeIconsExecute(Sender: TObject);
begin
  frmMain.ArrangeIcons();
end;

procedure TdmMain.actnArrangeIconsUpdate(Sender: TObject);
begin
  actnArrangeIcons.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnTileHorizontallyExecute(Sender: TObject);
begin
  frmMain.TileMode := tbHorizontal;
  frmMain.Tile();
end;

procedure TdmMain.actnTileHorizontallyUpdate(Sender: TObject);
begin
  actnTileHorizontally.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnTileVerticallyExecute(Sender: TObject);
begin
  frmMain.TileMode := tbVertical;
  frmMain.Tile();
end;

procedure TdmMain.actnTileVerticallyUpdate(Sender: TObject);
begin
  actnTileVertically.Enabled := (frmMain.MDIChildCount > 0);
end;

procedure TdmMain.actnAboutFormExecute(Sender: TObject);
begin
  AboutDlg.ShowGraphicsMagicAbout();
end;

procedure TdmMain.actnShowSplashExecute(Sender: TObject);
begin
  frmSplash := TfrmSplash.Create(nil);
  try
    frmSplash.imgSplash.Cursor := crHandPoint;
    frmSplash.ShowModal();
  finally
    FreeAndNil(frmSplash);
  end;
end;

procedure TdmMain.actnSoftwarePageCHExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/ch/software.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnSoftwarePageENExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/en/software.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnFiltersPageCHExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/ch/filters.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnFiltersPageENExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/en/filters.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnDownloadPageCHExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/ch/codecenter.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnDownloadPageENExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://www.mandrillsoft.com/en/codecenter.html'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnDownloadAtSourceForgeExecute(Sender: TObject);
begin
{$WARN UNSAFE_TYPE OFF}
  ShellExecute(0, 'open', PChar('http://sourceforge.net/projects/graphicsmagic/'), nil, nil, SW_NORMAL);
{$WARN UNSAFE_TYPE ON}
end;

procedure TdmMain.actnCreateSwatchExecute(Sender: TObject);
var
  LColor      : TColor;
  r, g, b     : Byte;
  LNewSwatch  : TgmSwatch;
  LSwatchName : string;
begin
  LColor := clBlack;

  if frmSwatch.SwatchList.Count > MAX_SWATCH_COUNT then
  begin
    MessageDlg('Can not add new swatch to list, because the' + #10#13 +
      'number of swatches is greater than ' + IntToStr(MAX_SWATCH_COUNT) + '.',
      mtError, [mbOK], 0);
  end
  else
  begin
    case frmColor.CurrColorSelector of
      csForeColor:
        begin
          LColor := frmColor.shpForegroundColor.Brush.Color;
        end;

      csBackColor:
        begin
          LColor := frmColor.shpBackgroundColor.Brush.Color;
        end;
    end;

    r := GetRValue(LColor);
    g := GetGValue(LColor);
    b := GetBValue(LColor);

    LSwatchName := 'Swatch' + IntToStr(frmSwatch.SwatchList.NewColorSwatchCount + 1);
    LNewSwatch  := TgmSwatch.Create(r, g, b, LSwatchName);

    frmSwatch.SwatchList.AddSwatch(LNewSwatch);
    frmSwatch.SwatchList.ShowSwatches(frmSwatch.imgSwatches);

    frmSwatch.edtSwatchCount.Text := IntToStr(frmSwatch.SwatchList.Count);
  end;
end;

procedure TdmMain.actnSwatchOptionsExecute(Sender: TObject);
var
  p: TPoint;
begin
  HideSwatchesPopupMenuItemsForToolButton();
  GetCursorPos(p);
  pmnSwatches.Popup(p.X, p.Y);
end;

procedure TdmMain.actnResetSwatchesExecute(Sender: TObject);
begin
  if MessageDlg('Replace current color swatches with the default colors?',
                mtConfirmation, [mbOK, mbCancel], 0) = idOK then
  begin
    if frmSwatch.SwatchList.IsModified then
    begin
      case MessageDlg('The color swatches has been changed. Do you want to save these changes?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
          begin
            Self.actnSaveChangesToSwatches.Execute;
          end;

        mrCancel:
          begin
            Exit;
          end;
      end;
    end;

    frmSwatch.SwatchList.LoadInternalSwatchesToList();
    frmSwatch.SwatchList.ShowSwatches(frmSwatch.imgSwatches);
    WriteInfoToIniFile(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '1');

    frmSwatch.edtSwatchCount.Text := IntToStr(frmSwatch.SwatchList.Count);
    frmSwatch.PutSwatchImageAtTopLeft();
  end;
end;

procedure TdmMain.actnResetSwatchesUpdate(Sender: TObject);
begin
  actnResetSwatches.Enabled := (not frmSwatch.SwatchList.IsUsingInternal) or
                               frmSwatch.SwatchList.IsModified;
end;

procedure TdmMain.actnReplaceSwatchesExecute(Sender: TObject);
var
  LFileName, LOpenDir: string;
begin
  if frmSwatch.SwatchList.IsModified then
  begin
    case MessageDlg('The color swathces has been changed. Do you want to save these changes?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          Self.actnSaveChangesToSwatches.Execute;
        end;
        
      mrCancel:
        begin
          Exit;
        end;
    end;
  end;

  LFileName := ReadInfoFromIniFile(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, '');

  if LFileName <> '' then
  begin
    LOpenDir := ExtractFilePath(LFileName);
  end
  else
  begin
    LOpenDir := ExtractFilePath( ParamStr(0) );
  end;

  OpenSwatchesDialog.InitialDir := LOpenDir;

  if OpenSwatchesDialog.Execute() then
  begin
    LFileName := OpenSwatchesDialog.FileName;

    if frmSwatch.SwatchList.LoadSwatchesToList(LFileName) = True then
    begin
      // update the ini file for next time use
      WriteInfoToIniFile(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, LFileName);
    end
    else
    begin
      MessageDlg(frmSwatch.SwatchList.OutputMsg, mtError, [mbOK], 0);
    end;

    frmSwatch.SwatchList.ShowSwatches(frmSwatch.imgSwatches);

    // if failure in loading external Swathes, then loading internal Swatches
    if frmSwatch.SwatchList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '1');
    end
    else
    begin
      WriteInfoToIniFile(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '0');
    end;
    
    frmSwatch.edtSwatchCount.Text := IntToStr(frmSwatch.SwatchList.Count);
    frmSwatch.PutSwatchImageAtTopLeft();
  end;
end;

procedure TdmMain.actnSaveChangesToSwatchesExecute(Sender: TObject);
begin
  if frmSwatch.SwatchList.Count > 0 then
  begin
    if frmSwatch.SwatchList.IsUsingInternal then
    begin
      Self.actnSaveSwatchesAs.Execute;
    end
    else
    begin
      frmSwatch.SwatchList.SaveSwatchesToFile(frmSwatch.SwatchList.FileName);
    end;
  end;
end;

procedure TdmMain.actnSaveChangesToSwatchesUpdate(Sender: TObject);
begin
  actnSaveChangesToSwatches.Enabled := frmSwatch.SwatchList.IsModified;
end;

procedure TdmMain.actnSaveSwatchesAsExecute(Sender: TObject);
var
  LFileName, LFileExt : string;
begin
  if SaveSwatchesDialog.Execute() then
  begin
    LFileName := SaveSwatchesDialog.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );

    if LFileExt = '' then
    begin
      LFileName := LFileName + '.swa';
    end
    else
    begin
      if LFileExt <> '.swa' then
      begin
        LFileName := ChangeFileExt(LFileName, '.swa');
      end;
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already existed. Do you want to replace it?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        frmSwatch.SwatchList.SaveSwatchesToFile(LFileName);
        WriteInfoToIniFile(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, LFileName);
      end;
    end
    else
    begin
      frmSwatch.SwatchList.SaveSwatchesToFile(LFileName);
      WriteInfoToIniFile(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, LFileName);
    end;
  end;
end;

procedure TdmMain.actnNewSwatchExecute(Sender: TObject);
var
  r, g, b    : Byte;
  LNewSwatch : TgmSwatch;
begin
  frmColorSwatchName := TfrmColorSwatchName.Create(nil);
  try
    case frmColor.CurrColorSelector of
      csForeColor:
        begin
          frmColorSwatchName.shpColorSwatch.Brush.Color := frmColor.shpForegroundColor.Brush.Color;
        end;

      csBackColor:
        begin
          frmColorSwatchName.shpColorSwatch.Brush.Color := frmColor.shpBackgroundColor.Brush.Color;
        end;
    end;

    frmColorSwatchName.edtColorSwatchName.Text := 'Swatch' + IntToStr(frmSwatch.SwatchList.NewColorSwatchCount + 1);

    if frmColorSwatchName.ShowModal = idOK then
    begin
      r := GetRValue(frmColorSwatchName.shpColorSwatch.Brush.Color);
      g := GetGValue(frmColorSwatchName.shpColorSwatch.Brush.Color);
      b := GetBValue(frmColorSwatchName.shpColorSwatch.Brush.Color);

      LNewSwatch := TgmSwatch.Create(r, g, b, frmColorSwatchName.edtColorSwatchName.Text);

      frmSwatch.SwatchList.AddSwatch(LNewSwatch);
      frmSwatch.SwatchList.ShowSwatches(frmSwatch.imgSwatches);

      frmSwatch.edtSwatchCount.Text := IntToStr(frmSwatch.SwatchList.Count);
    end;
  finally
    FreeAndNil(frmColorSwatchName);
  end;
end;

procedure TdmMain.actnNewSwatchUpdate(Sender: TObject);
begin
  actnNewSwatch.Enabled := (frmSwatch.SwatchList.Count < MAX_SWATCH_COUNT);
end;

procedure TdmMain.actnRenameSwatchExecute(Sender: TObject);
var
  r, g, b: Byte;
begin
  r := frmSwatch.SwatchList.SelectedSwatch.Red;
  g := frmSwatch.SwatchList.SelectedSwatch.Green;
  b := frmSwatch.SwatchList.SelectedSwatch.Blue;

  frmColorSwatchName := TfrmColorSwatchName.Create(nil);
  try
    frmColorSwatchName.shpColorSwatch.Brush.Color := RGB(r, g, b);
    frmColorSwatchName.edtColorSwatchName.Text    := frmSwatch.SwatchList.SelectedSwatch.Name;

    if frmColorSwatchName.ShowModal = idOK then
    begin
      r := GetRValue(frmColorSwatchName.shpColorSwatch.Brush.Color);
      g := GetGValue(frmColorSwatchName.shpColorSwatch.Brush.Color);
      b := GetBValue(frmColorSwatchName.shpColorSwatch.Brush.Color);

      with frmSwatch.SwatchList do
      begin
        SelectedSwatch.Red   := r;
        SelectedSwatch.Green := g;
        SelectedSwatch.Blue  := b;
        SelectedSwatch.Name  := frmColorSwatchName.edtColorSwatchName.Text;

        ShowSwatches(frmSwatch.imgSwatches);

        IsModified := True;
      end;
    end;
  finally
    FreeAndNil(frmColorSwatchName);
  end;
end;

procedure TdmMain.actnRenameSwatchUpdate(Sender: TObject);
begin
  actnRenameSwatch.Enabled := Assigned(frmSwatch.SwatchList.SelectedSwatch);
end;

procedure TdmMain.actnDeleteSwatchExecute(Sender: TObject);
begin
  frmSwatch.SwatchList.DeleteSelectedSwatch();
  frmSwatch.SwatchList.ShowSwatches(frmSwatch.imgSwatches);
  
  frmSwatch.edtSwatchCount.Text := IntToStr(frmSwatch.SwatchList.Count);
end;

procedure TdmMain.actnDeleteSwatchUpdate(Sender: TObject);
begin
  actnDeleteSwatch.Enabled := (frmSwatch.SwatchList.Count > 1) and
                              Assigned(frmSwatch.SwatchList.SelectedSwatch);
end;

end.




