{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmIni;

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

interface

uses
  SysUtils;

  procedure InitializeIniFile;
  function ReadInfoFromIniFile(const ASection, AIdent, ADefault: string): string;
  procedure WriteInfoToIniFile(const ASection, AIdent, AValue: string);

const
  // For Ini File
  SECTION_SETUP        = 'Setup';
  IDENT_OPEN_IMAGE_DIR = 'OpenImageDir';

  SECTION_OPEN_RECENT     = 'OpenRecent';         // key name of the Open Recent
  IDENT_OPEN_RECENT_COUNT = 'OpenRecentCount';    // identifier of the Open Recent menu count
  IDENT_OPEN_RECENT_PATH  = 'OpenRecentPath';     // prefix of the identifier of Open Recent path

  SECTION_SWATCH              = 'Swatch';
  IDENT_OPEN_SWATCHES_FILE    = 'OpenColorSwatchesFile';
  IDENT_USE_INTERNAL_SWATCHES = 'UseInternalSwatches';

  SECTION_BRUSH                   = 'Brush';
  IDENT_OPEN_BRUSH_FILE           = 'OpenBrushFile';
  IDENT_BRUSH_INDEX               = 'BrushIndex';
  IDENT_BRUSH_THUMBNAIL_SIZE_MODE = 'BrushThumbnailSizeMode';
  IDENT_BRUSH_USE_INTERNAL        = 'UseInternalStrokes';

  SECTION_ERASER                         = 'Eraser';
  IDENT_ERASER_OPEN_BRUSH_FILE           = 'OpenBrushFile';
  IDENT_ERASER_BRUSH_INDEX               = 'BrushIndex';
  IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE = 'BrushThumbnailSizeMode';
  IDENT_ERASER_USE_INTERNAL              = 'UseInternalStrokes';

{ Fill Dialog }
  SECTION_FILL_DIALOG                    = 'FillDialog';
  IDENT_FILL_OPEN_PATTERN_FILE           = 'FillOpenPatternFile';
  IDENT_FILL_PATTERN_INDEX               = 'FillPatternIndex';
  IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE = 'FillPatternThumbnailSizeMode';
  IDENT_FILL_PATTERN_USE_INTERNAL        = 'UseInternalPatterns';

  SECTION_STAMP_PATTERN                   = 'StampPattern';
  IDENT_STAMP_OPEN_PATTERN_FILE           = 'StampOpenPatternFile';
  IDENT_STAMP_PATTERN_INDEX               = 'StampPatternIndex';
  IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE = 'StampPatternThumbnailSizeMode';
  IDENT_STAMP_PATTERN_USE_INTERNAL        = 'UseInternalPatterns';

  SECTION_PAINT_BUCKET                           = 'PaintBucket';
  IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE           = 'PaintBucketOpenPatternFile';
  IDENT_PAINT_BUCKET_PATTERN_INDEX               = 'PaintBucketPatternIndex';
  IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE = 'PaintBucketPatternThumbnailSizeMode';
  IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL        = 'UseInternalPatterns';

  SECTION_PATTERN_LAYER                   = 'PatternLayer';
  IDENT_LAYER_OPEN_PATTERN_FILE           = 'LayerOpenPatternFile';
  IDENT_LAYER_PATTERN_INDEX               = 'LayerPatternIndex';
  IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE = 'LayerPatternThumbnailSizeMode';
  IDENT_LAYER_PATTERN_USE_INTERNAL        = 'UseInternalPatterns';

  SECTION_GRADIENT                   = 'Gradient';
  IDENT_OPEN_GRADIENTS_FILE          = 'OpenGradientsFile';
  IDENT_GRADIENTS_INDEX              = 'GradientsIndex';
  IDENT_GRADIENT_THUMBNAIL_SIZE_MODE = 'GradientThumbnailSizeMode';
  IDENT_GRADIENT_USE_INTERNAL        = 'UseInternalGradients';

  SECTION_GRADIENT_MAP                   = 'GradientMap';
  IDENT_GRADIENT_MAP_OPEN_FILE           = 'GradientMapOpenFile';
  IDENT_GRADIENT_MAP_INDEX               = 'GradientMapIndex';
  IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE = 'GradientMapThumbnailSizeMode';
  IDENT_GRADIENT_MAP_USE_INTERNAL        = 'UseInternalGradients';

  SECTION_GRADIENT_MAP_LAYER                   = 'GradientMapLayer';
  IDENT_GRADIENT_MAP_LAYER_OPEN_FILE           = 'GradientMapLayerOpenFile';
  IDENT_GRADIENT_MAP_LAYER_INDEX               = 'GradientMapLayerIndex';
  IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE = 'GradientMapLayerThumbnailSizeMode';
  IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL        = 'UseInternalGradients';

  SECTION_GRADIENT_LAYER                   = 'GradientLayer';
  IDENT_GRADIENT_LAYER_OPEN_FILE           = 'GradientLayerOpenFile';
  IDENT_GRADIENT_LAYER_INDEX               = 'GradientLayerIndex';
  IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE = 'GradientLayerThumbnailSizeMode';
  IDENT_GRADIENT_LAYER_USE_INTERNAL        = 'UseInternalGradients';

  SECTION_GRADIENT_EDITOR                   = 'GradientEditor';
  IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE = 'GradientEditorThumbnailSizeMode';

{ New File Dialog }
  SECTION_NEW_FILE_DIALOG              = 'NewFileDialog';
  IDENT_DEFAULT_IMAGE_WIDTH            = 'DefaultImageWidth';
  IDENT_DEFAULT_IMAGE_HEIGHT           = 'DefaultImageHeight';
  IDENT_NEW_FILE_WIDTH_UNIT_INDEX      = 'WidthUnitIndex';
  IDENT_NEW_FILE_HEIGHT_UNIT_INDEX     = 'HeightUnitIndex';
  IDENT_NEW_FILE_RESOLUTION_UNIT_INDEX = 'ResolutionUnitIndex';

{ Brightness/Contrast Dialog }
  SECTION_BRIGHT_CONTRAST_DIALOG = 'BrightnessContrastDialog';
  IDENT_BRIGHT_CONTRAST_PREVIEW  = 'Preview';

{ Color Balance Dialog }
  SECTION_COLOR_BALANCE_DIALOG            = 'ColorBalanceDialog';
  IDENT_COLOR_BALANCE_PRESERVE_LUMINOSITY = 'PreserveLuminosity';
  IDENT_COLOR_BALANCE_PREVIEW             = 'Preview';

{ Hue/Saturation Dialog }
  SECTION_HUE_SATURATION_DIALOG = 'HueSaturationDialog';
  IDENT_HUE_SATURATION_MODE     = 'HueSaturationMode';
  IDENT_HUE_SATURATION_PREVIEW  = 'Preview';

{ Replace Color Dialog }
  SECTION_REPLACE_COLOR_DIALOG       = 'ReplaceColorDialog';
  IDENT_REPLACE_COLOR_FUZZINESS      = 'Fuzziness';
  IDENT_REPLACE_COLOR_THUMBNAIL_MODE = 'ThumbnailMode';
  IDENT_REPLACE_COLOR_SAMPLE_COLOR   = 'SampleColor';
  IDENT_REPLACE_COLOR_SAMPLE_MODE    = 'SampleMode';
  IDENT_REPLACE_COLOR_PREVIEW        = 'Preview';

{ Channel Mixer Dialog }
  SECTION_CHANNEL_MIXER_DIALOG = 'ChannelMixerDialog';
  IDENT_CHANNEL_MIXER_PREVIEW  = 'Preview';

{ Threshold Dialog }
  SECTION_THRESHOLD_DIALOG = 'ThresholdDialog';
  IDENT_THRESHOLD_PREVIEW  = 'Preview';

{ Posterize Dialog }
  SECTION_POSTERIZE_DIALOG = 'PosterizeDialog';
  IDENT_POSTERIZE_LEVELS   = 'Levels';
  IDENT_POSTERIZE_PREVIEW  = 'Preview';

{ Curves Dialog }
  SECTION_CURVES_DIALOG  = 'CurvesDialog';
  IDENT_CURVES_CHANNEL   = 'Channel';
  IDENT_CURVES_HISTOGRAM = 'Histogram';
  IDENT_CURVES_TYPE      = 'CurveType';
  IDENT_CURVES_PREVIEW   = 'Preview';
  IDENT_CURVES_FILE_DIR  = 'CurvesFileDir';

{ Levels Dialog }
  SECTION_LEVELS_DIALOG  = 'LevelsDialog';
  IDENT_LEVELS_CHANNEL   = 'Channel';
  IDENT_LEVELS_HISTOGRAM = 'Histogram';
  IDENT_LEVELS_PREVIEW   = 'Preview';
  IDENT_LEVELS_FILE_DIR  = 'LevelsFileDir';

{ Image Size Dialog }
  SECTION_IMAGE_SIZE_DIALOG          = 'ImageSizeDialog';
  IDENT_IMAGE_SIZE_WIDTH_UNIT        = 'WidthUnit';
  IDENT_IMAGE_SIZE_HEIGHT_UNIT       = 'HeightUnit';
  IDENT_IMAGE_SIZE_CONSTRAIN         = 'ConstrainProperties';
  IDENT_IMAGE_SIZE_RESAMPLER         = 'Resampler';
  IDENT_IMAGE_SIZE_PIXEL_ACCESS_MODE = 'PixelAccessMode';
  IDENT_IMAGE_SIZE_WRAP_MODE         = 'WrapMode';
  IDENT_IMAGE_SIZE_KERNEL            = 'Kernel';
  IDENT_IMAGE_SIZE_KERNEL_MODE       = 'KernelMode';
  IDENT_IMAGE_SIZE_TABLE_SIZE        = 'TableSize';

{ Canvas Size Dialog }
  SECTION_CANVAS_SIZE_DIALOG    = 'CanvasSizeDialog';
  IDENT_CANVAS_SIZE_WIDTH_UNIT  = 'WidthUnit';
  IDENT_CANVAS_SIZE_HEIGHT_UNIT = 'HeightUnit';

{ Rotate Canvas Dialog }
  SECTION_ROTATE_CANVAS_DIALOG  = 'RotateCanvasDialog';
  IDENT_ROTATE_CANVAS_ANGLE     = 'RotateAngle';
  IDENT_ROTATE_CANVAS_DIRECTION = 'RotateDirection';

{ Indexed Color Dialog }
  SECTION_INDEXED_COLOR_DIALOG    = 'IndexedColorDialog';
  IDENT_INDEXED_COLOR_REDUCTION   = 'ColorReduction';
  IDENT_INDEXED_COLOR_DITHER_MODE = 'DitherMode';
  IDENT_INDEXED_COLOR_PREVIEW     = 'Preview';

{ Histogram Dialog }
  SECTION_HISTOGRAM_DIALOG      = 'HistogramDialog';
  IDENT_HISTOGRAM_COLOR_CHANNEL = 'ColorChannel';

{ Print Options Dialog }
  SECTION_PRINT_OPTIONS_DIALOG    = 'PrintOptionsDialog';
  IDENT_PRINT_OPTIONS_TOP_UNIT    = 'TopUnit';
  IDENT_PRINT_OPTIONS_LEFT_UNIT   = 'LeftUnit';
  IDENT_PRINT_OPTIONS_WIDTH_UNIT  = 'WidthUnit';
  IDENT_PRINT_OPTIONS_HEIGHT_UNIT = 'HeightUnit';
  
{ Color Range Selection Dialog }
  SECTION_COLOR_RANGE_SELECTION_DIALOG = 'ColorRangeSelectionDialog';
  IDENT_FUZZINESS                      = 'Fuzziness';
  IDENT_SAMPLE_COLOR                   = 'SampleColor';
  IDENT_THUMBNAIL_MODE                 = 'ThumbnailMode';

{ GradientMap Dialog }
  SECTION_GRADIENT_MAP_DIALOG       = 'GradientMapDialog';
  IDENT_GRADIENT_MAP_DIALOG_REVERSE = 'Reverse';
  IDENT_GRADIENT_MAP_DIALOG_PREVIEW = 'Preview';

{ Prefereces Settings }
  SECTION_PREFERENCES       = 'Preferences';
  IDENT_HISTORY_STATES      = 'HistoryStates';
  IDENT_GHOST_MODE_ENABLED  = 'GhostModeEnabled';
  IDENT_GHOST_FADE_INTERVAL = 'GhostFadeInterval';
  IDENT_GHOST_MAX_OPAQUE    = 'GhostMaxOpaque';

{ Selection Settings }
  SECTION_SELECTION    = 'Selection';
  IDENT_FEATHER_RADIUS = 'FeatherRadius';

implementation

uses
  INIFiles, Graphics;

procedure InitializeIniFile;
var
  IniFile      : TIniFile;
  IniFileName  : string;
  AppFolderAddr: string;   // folder of main application
begin
  // get file name of ini file
  IniFileName := ChangeFileExt( ParamStr(0),'.ini' );

  if not ( FileExists(IniFileName) ) then
  begin
    IniFile := TIniFile.Create(IniFileName);
    try
      AppFolderAddr := ExtractFilePath( ParamStr(0) );
      IniFile.WriteString(SECTION_SETUP, IDENT_OPEN_IMAGE_DIR, AppFolderAddr);
      // Open recent
      IniFile.WriteString(SECTION_OPEN_RECENT, IDENT_OPEN_RECENT_COUNT, '0');
      // Swatch
      IniFile.WriteString(SECTION_SWATCH, IDENT_OPEN_SWATCHES_FILE, AppFolderAddr + 'Presets\Color Swatches\Default.swa');
      IniFile.WriteString(SECTION_SWATCH, IDENT_USE_INTERNAL_SWATCHES, '1');
      // Brush
      IniFile.WriteString(SECTION_BRUSH, IDENT_OPEN_BRUSH_FILE, AppFolderAddr + 'Presets\Brushes\gmDefaultBrushes.gmb');
      IniFile.WriteString(SECTION_BRUSH, IDENT_BRUSH_INDEX, '0');
      IniFile.WriteString(SECTION_BRUSH, IDENT_BRUSH_THUMBNAIL_SIZE_MODE, '0');
      IniFile.WriteString(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '1');
      // Eraser
      IniFile.WriteString(SECTION_ERASER, IDENT_ERASER_OPEN_BRUSH_FILE, AppFolderAddr + 'Presets\Brushes\gmDefaultBrushes.gmb');
      IniFile.WriteString(SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX, '0');
      IniFile.WriteString(SECTION_ERASER, IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE, '0');
      IniFile.WriteString(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '1');
      // Fill Dialog
      IniFile.WriteString(SECTION_FILL_DIALOG, IDENT_FILL_OPEN_PATTERN_FILE, AppFolderAddr + 'Presets\Patterns\gmPattern1.pat');
      IniFile.WriteString(SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_INDEX, '0');
      IniFile.WriteString(SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_USE_INTERNAL, '1');
      // Stamp Pattern
      IniFile.WriteString(SECTION_STAMP_PATTERN, IDENT_STAMP_OPEN_PATTERN_FILE, AppFolderAddr + 'Presets\Patterns\gmPattern1.pat');
      IniFile.WriteString(SECTION_STAMP_PATTERN, IDENT_STAMP_PATTERN_INDEX, '0');
      IniFile.WriteString(SECTION_STAMP_PATTERN, IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_STAMP_PATTERN, IDENT_STAMP_PATTERN_USE_INTERNAL, '1');
      // PaintBucket Pattern
      IniFile.WriteString(SECTION_PAINT_BUCKET, IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE, AppFolderAddr + 'Presets\Patterns\gmPattern1.pat');
      IniFile.WriteString(SECTION_PAINT_BUCKET, IDENT_PAINT_BUCKET_PATTERN_INDEX, '0');
      IniFile.WriteString(SECTION_PAINT_BUCKET, IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_PAINT_BUCKET, IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL, '1');
      // Pattern Layer Pattern
      IniFile.WriteString(SECTION_PATTERN_LAYER, IDENT_LAYER_OPEN_PATTERN_FILE, AppFolderAddr + 'Presets\Patterns\gmPattern1.pat');
      IniFile.WriteString(SECTION_PATTERN_LAYER, IDENT_LAYER_PATTERN_INDEX, '0');
      IniFile.WriteString(SECTION_PATTERN_LAYER, IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_PATTERN_LAYER, IDENT_LAYER_PATTERN_USE_INTERNAL, '1');
      // Gradient
      IniFile.WriteString(SECTION_GRADIENT, IDENT_OPEN_GRADIENTS_FILE, AppFolderAddr + 'Presets\Gradients\Default.grd');
      IniFile.WriteString(SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, '0');
      IniFile.WriteString(SECTION_GRADIENT, IDENT_GRADIENT_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1');
      // Gradient Map
      IniFile.WriteString(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_OPEN_FILE, AppFolderAddr + 'Presets\Gradients\Default.grd');
      IniFile.WriteString(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, '0');
      IniFile.WriteString(SECTION_GRADIENT_MAP, IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1');
      // Gradient Map Layer
      IniFile.WriteString(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_OPEN_FILE, AppFolderAddr + 'Presets\Gradients\Default.grd');
      IniFile.WriteString(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_INDEX, '0');
      IniFile.WriteString(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1');
      // Gradient Layer
      IniFile.WriteString(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_OPEN_FILE, AppFolderAddr + 'Presets\Gradients\Default.grd');
      IniFile.WriteString(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, '0');
      IniFile.WriteString(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE, '1');
      IniFile.WriteString(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1');
      // Gradient Editor
      IniFile.WriteString(SECTION_GRADIENT_EDITOR, IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE, '1');
      // New File Dialog
      IniFile.WriteString(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_WIDTH,            '640');
      IniFile.WriteString(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_HEIGHT,           '480');
      IniFile.WriteString(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_WIDTH_UNIT_INDEX,      '1');
      IniFile.WriteString(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_HEIGHT_UNIT_INDEX,     '1');
      IniFile.WriteString(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_RESOLUTION_UNIT_INDEX, '0');
      // Brightness/Contrast Dialog
      IniFile.WriteString(SECTION_BRIGHT_CONTRAST_DIALOG, IDENT_BRIGHT_CONTRAST_PREVIEW, '1');
      // Color Balance Dialog
      IniFile.WriteString(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PRESERVE_LUMINOSITY, '1');
      IniFile.WriteString(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PREVIEW,             '1');
      // Hue/Saturation Dialog
      IniFile.WriteString(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_MODE,    '0');
      IniFile.WriteString(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_PREVIEW, '1');
      // Replace Color Dialog
      IniFile.WriteString(SECTION_REPLACE_COLOR_DIALOG, IDENT_REPLACE_COLOR_FUZZINESS,      '10');
      IniFile.WriteString(SECTION_REPLACE_COLOR_DIALOG, IDENT_REPLACE_COLOR_THUMBNAIL_MODE, '0');
      IniFile.WriteString(SECTION_REPLACE_COLOR_DIALOG, IDENT_REPLACE_COLOR_SAMPLE_COLOR,   IntToStr(ColorToRGB(clWhite)));
      IniFile.WriteString(SECTION_REPLACE_COLOR_DIALOG, IDENT_REPLACE_COLOR_SAMPLE_MODE,    '0');
      IniFile.WriteString(SECTION_REPLACE_COLOR_DIALOG, IDENT_REPLACE_COLOR_PREVIEW,        '1');
      // Channel Mixer Dialog
      IniFile.WriteString(SECTION_CHANNEL_MIXER_DIALOG, IDENT_CHANNEL_MIXER_PREVIEW, '1');
      // Threshold Dialog
      IniFile.WriteString(SECTION_THRESHOLD_DIALOG, IDENT_THRESHOLD_PREVIEW, '1');
      // Posterize Dialog
      IniFile.WriteString(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_LEVELS,  '2');
      IniFile.WriteString(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_PREVIEW, '1');
      // Curves Dialog
      IniFile.WriteString(SECTION_CURVES_DIALOG, IDENT_CURVES_CHANNEL,   '0');
      IniFile.WriteString(SECTION_CURVES_DIALOG, IDENT_CURVES_HISTOGRAM, '0');
      IniFile.WriteString(SECTION_CURVES_DIALOG, IDENT_CURVES_TYPE,      '0');
      IniFile.WriteString(SECTION_CURVES_DIALOG, IDENT_CURVES_PREVIEW,   '1');
      IniFile.WriteString(SECTION_CURVES_DIALOG, IDENT_CURVES_FILE_DIR,  AppFolderAddr);
      // Levels Dialog
      IniFile.WriteString(SECTION_LEVELS_DIALOG, IDENT_LEVELS_CHANNEL,   '0');
      IniFile.WriteString(SECTION_LEVELS_DIALOG, IDENT_LEVELS_HISTOGRAM, '0');
      IniFile.WriteString(SECTION_LEVELS_DIALOG, IDENT_LEVELS_PREVIEW,   '1');
      IniFile.WriteString(SECTION_LEVELS_DIALOG, IDENT_LEVELS_FILE_DIR,  AppFolderAddr);
      // Image Size Dialog
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WIDTH_UNIT,        '1');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_HEIGHT_UNIT,       '1');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_CONSTRAIN,         '1');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_RESAMPLER,         '3');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_PIXEL_ACCESS_MODE, '1');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WRAP_MODE,         '0');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL,            '4');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL_MODE,       '0');
      IniFile.WriteString(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_TABLE_SIZE,        '32');
      // Canvas Size Dialog
      IniFile.WriteString(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_WIDTH_UNIT, '1');
      IniFile.WriteString(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_WIDTH_UNIT, '1');
      // Rotate Canvas Dialog
      IniFile.WriteString(SECTION_ROTATE_CANVAS_DIALOG, IDENT_ROTATE_CANVAS_ANGLE,     '0');
      IniFile.WriteString(SECTION_ROTATE_CANVAS_DIALOG, IDENT_ROTATE_CANVAS_DIRECTION, '0');
      // Indexed Color Dialog
      IniFile.WriteString(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_REDUCTION,   '7');
      IniFile.WriteString(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_DITHER_MODE, '1');
      IniFile.WriteString(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_PREVIEW,     '1');
      // Histogram Dialog
      IniFile.WriteString(SECTION_HISTOGRAM_DIALOG, IDENT_HISTOGRAM_COLOR_CHANNEL, '0');
      { Print Options Dialog }
      IniFile.WriteString(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_TOP_UNIT,    '1');
      IniFile.WriteString(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_LEFT_UNIT,   '1');
      IniFile.WriteString(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_WIDTH_UNIT,  '1');
      IniFile.WriteString(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_HEIGHT_UNIT, '1');
      // Color Range Selection Dialog
      IniFile.WriteString(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_FUZZINESS, '10');
      IniFile.WriteString(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_SAMPLE_COLOR, IntToStr(ColorToRGB(clBlack)));
      IniFile.WriteString(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_THUMBNAIL_MODE, '0');
      // Gradient Map Dialog
      IniFile.WriteString(SECTION_GRADIENT_MAP_DIALOG, IDENT_GRADIENT_MAP_DIALOG_REVERSE, '1');
      IniFile.WriteString(SECTION_GRADIENT_MAP_DIALOG, IDENT_GRADIENT_MAP_DIALOG_PREVIEW, '1');
      // Preferences Settings
      IniFile.WriteString(SECTION_PREFERENCES, IDENT_HISTORY_STATES, '20');
      IniFile.WriteString(SECTION_PREFERENCES, IDENT_GHOST_MODE_ENABLED, '1');
      IniFile.WriteString(SECTION_PREFERENCES, IDENT_GHOST_FADE_INTERVAL, '50');
      IniFile.WriteString(SECTION_PREFERENCES, IDENT_GHOST_MAX_OPAQUE, '255');
      // Selection Settings
      IniFile.WriteString(SECTION_SELECTION, IDENT_FEATHER_RADIUS, '0');
    finally
      IniFile.Free;
    end;
  end;
end;

function ReadInfoFromIniFile(const ASection, AIdent, ADefault: string): string;
var
  IniFile    : TIniFile;
  IniFileName: string;
begin
  IniFileName := ChangeFileExt( ParamStr(0),'.ini' );
  if FileExists(IniFileName) then
  begin
    IniFile := TIniFile.Create(IniFileName);
    try
      Result := IniFile.ReadString(ASection, AIdent, ADefault);
    finally
      IniFile.Free;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

procedure WriteInfoToIniFile(const ASection, AIdent, AValue: string);
var
  IniFile    : TIniFile;
  IniFileName: string;
begin
  IniFileName := ChangeFileExt( ParamStr(0),'.ini' );
  IniFile     := TIniFile.Create(IniFileName);
  try
    IniFile.WriteString(ASection, AIdent, AValue);
  finally
    IniFile.Free;
  end;
end; 

end.
