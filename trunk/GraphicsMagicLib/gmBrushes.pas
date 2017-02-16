unit gmBrushes;

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

// Update Date: 2017/01/23

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, SysUtils, Classes, Graphics, ExtCtrls,
{ Graphics32 }
  GR32, GR32_Image,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmTypes,
  gmBlendModes,
  gmConvolve;

type
  TgmBrushTool = (btPaintBrush,
                  btHistoryBrush,
                  btAirBrush,
                  btJetGunBrush,
                  btCloneStamp,
                  btPatternStamp,
                  btBlurSharpenBrush,
                  btSmudge,
                  btDodgeBurnBrush,
                  btLightBrush);

  TgmEraserTool = (etEraser, etBackgroundEraser, etMagicEraser);

  TgmBrushID = (bidNone,
                bidPaintBrush,
                bidHistoryBrush,
                bidCloneStamp,
                bidPatternStamp,
                bidBlurSharpen,
                bidSmudge,
                bidDodgeBurn,
                bidLightBrush,
                bidEraser,
                bidBackgroundEraser);

  TgmLightBrushMode = (lbmNone,
                       lbmHighHue,
                       lbmLowHue,
                       lbmHighSaturation,
                       lbmLowSaturation,
                       lbmHighLuminosity,
                       lbmLowLuminosity,
                       lbmBrightness,
                       lbmDarkness,
                       lbmHighContrast,
                       lbmLowContrast);

  TgmErasingMode = (emPaintBrush, emAirBrush);

  TgmBackgroundEraserLimit  = (belDiscontiguous, belContiguous);
  TgmBackgroundSamplingMode = (bsmContiguous, bsmOnce, bsmBackgroundSwatch);

  TgmBrushDynamicState = (bdsOff, bdsFade);

{ Based on GIMP }

  // DodgeBurn brush
  TgmDodgeBurnType = (dbtNone, dbtDodge, dbtBurn);
  TgmDodgeBurnMode = (dbmHighlights, dbmMidtones, dbmShadows);

  TgmGimpLUTFunc = function (const AExposure: Double; const AValue: Real): Real;

//-- TgmBrush ------------------------------------------------------------------

  TgmBrush = class(TObject)
  protected
    FBrushID             : TgmBrushID;
    FSourceBitmap        : TBitmap32;
    FForeground          : TBitmap32;
    FMask                : TBitmap32;
    FHalfWidth           : Integer;
    FHalfHeight          : Integer;
    FOpacity             : Integer;
    FOpacityWeight       : Integer;
    FIntensity           : Byte;
    FBlendMode           : TBlendMode32;
    FPreserveTransparency: Boolean;
    FName                : string;    // Name of brush tool
    FSelectionOffsetX    : Integer;   // use these two fields to convert the selection coordinates to layer coordinates
    FSelectionOffsetY    : Integer;

    { Brush Dynamics part }
    FMaskCopy                  : TBitmap32;
    FSizeDynamicState          : TgmBrushDynamicState;
    FSizeDynamicCurrentSteps   : Integer;
    FSizeDynamicTotalSteps     : Integer;

    FOriginalOpacity           : Byte;
    FOpacityDynamicState       : TgmBrushDynamicState;
    FOpacityDynamicCurrentSteps: Integer;
    FOpacityDynamicTotalSteps  : Integer;

    FStartColor, FEndColor     : TColor32;
    FColorDynamicState         : TgmBrushDynamicState;
    FColorDynamicCurrentSteps  : Integer;
    FColorDynamicTotalSteps    : Integer;

    procedure SetStrokeSizeByStep;
    function GetStrokeColorByStep: TColor32;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetBrushOpacity(const Opacity: Integer);
    procedure SetBrushIntensity(const Intensity: Integer);
    procedure SetBlendMode(const BlendMode: TBlendMode32);
    procedure SetPaintingStroke(const AMask: TBitmap32);
    procedure UpdateSourceBitmap(const Source: TBitmap32);
    procedure UpdateForeground(const Source: TBitmap32);

    function GetBrushArea(const X, Y: Integer): TRect; 

    { Brush Dynamics }
    procedure SetDynamicSize(const AMask: TBitmap32;
      const ADynamicState: TgmBrushDynamicState; const ASteps: Integer);

    procedure SetDynamicOpacity(const Opacity: Byte;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    procedure SetDynamicColor(const Color1, Color2: TColor32;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); virtual; abstract;

    property BrushID               : TgmBrushID read FBrushID;
    property Name                  : string     read FName;
    property BrushIntensity        : Byte       read FIntensity            write FIntensity;
    property IsPreserveTransparency: Boolean    read FPreserveTransparency write FPreserveTransparency;
    property SelectionOffsetX      : Integer    read FSelectionOffsetX     write FSelectionOffsetX;
    property SelectionOffsetY      : Integer    read FSelectionOffsetY     write FSelectionOffsetY;
  end;

//-- TgmPaintBrush -------------------------------------------------------------

  TgmPaintBrush = class(TgmBrush)
  private
    FColor: TColor32;
  public
    constructor Create;

    procedure SetColor(const AColor: TColor32);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;
  end;

//-- TgmHistoryBrush -----------------------------------------------------------

  TgmHistoryBrush = class(TgmBrush)
  private
    FHistoryBitmap: TBitmap32;
  public
    constructor Create; 
    destructor Destroy; override;

    procedure LoadHistoryBitmap(const HistoryBmp: TBitmap32);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;
  end;

//-- TgmCloneStamp -------------------------------------------------------------

  TgmCloneStamp = class(TgmBrush)
  private
    FSamplingPoint     : TPoint;    // sampling point of clone stamp
    FSamplingPointExist: Boolean;   // whether the sampling point is defined
    FUpdateStampOffset : Boolean;   // whether we need to recalculate offset of clone stamp (after define a new sampling point)
    FXOffset, FYOffset : Integer;   // offset of sampling point to current position of clone stamp
  public
    constructor Create; 

    procedure SetSamplingPoint(const X, Y: Integer);
    procedure SetStampPointOffset(const X, Y: Integer);
    procedure DrawStampAimFlag(ACanvas: TCanvas; const X, Y: Integer);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    property IsSamplingPointExist: Boolean read FSamplingPointExist;
    property IsUpdateStampOffset : Boolean read FUpdateStampOffset;
    property OffsetX             : Integer read FXOffset;
    property OffsetY             : Integer read FYOffset;
  end;

//-- TgmPatternStamp -----------------------------------------------------------

  TgmPatternStamp = class(TgmBrush)
  private
    FPatternBitmap: TBitmap32;
  public
    constructor Create; 
    destructor Destroy; override;

    procedure SetPatternBitmap(const APatternBmp: TBitmap32;
      const AWidth, AHeight: Integer);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;
  end;

//-- TgmBlurSharpenBrush -------------------------------------------------------

  TgmBlurSharpenBrush = class(TgmBrush)
  private
    FType     : TgmConvolveType;
    FConvolver: TgmConvolver;
    FPressure : Integer;

    // dynamic settings
    FOriginalPressure           : Integer;
    FPressureDynamicCurrentSteps: Integer;
    FPressureDynamicTotalSteps  : Integer;
    FPressureDynamicState       : TgmBrushDynamicState;
  public
    constructor Create(const AType: TgmConvolveType);
    destructor Destroy; override;

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    procedure SetDynamicPressure(const APressure: Integer;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    property ConvolveType: TgmConvolveType read FType;
    property Pressure    : Integer         read FPressure write FPressure;
  end;

//-- TgmSmudge -----------------------------------------------------------------

  TgmSmudge = class (TgmBrush)
  private
    FPressure: Integer;
  public
    constructor Create;

    procedure SetPressure(const Pressure: Integer);

    procedure CutRegionToForegroundBySize(const Source: TBitmap32;
      const X, Y: Integer);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;
  end;

//-- TgmDodgeBurnBrush ---------------------------------------------------------

  TgmDodgeBurnBrush = class(TgmBrush)
  private
    FExposure     : Integer;
    FDodgeBurnMode: TgmDodgeBurnMode;
    FDodgeBurnType: TgmDodgeBurnType;
    FDodgeBurnLUT : array [0..255] of Byte;
  public
    constructor Create(const ADodgeBurnType: TgmDodgeBurnType);

    procedure SetDodgeBurnExposure(const Exposure: Integer);
    procedure SetDodgeBurnMode(const Mode: TgmDodgeBurnMode);
    procedure MakeLUT;
    
    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    property DodgeBurnType: TgmDodgeBurnType read FDodgeBurnType;
  end;

//-- TgmLightBrush -------------------------------------------------------------

  TgmLightBrush = class(TgmBrush)
  private
    FLightBrushMode: TgmLightBrushMode;
  public
    constructor Create(const ALightBrushMode: TgmLightBrushMode);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    property LightMode: TgmLightBrushMode read FLightBrushMode;
  end;

//-- TgmEraser -----------------------------------------------------------------

  TgmEraser = class(TgmBrush)
  private
    FHistoryBitmap     : TBitmap32;
    FErasingMode       : TgmErasingMode;
    FAirErasingPressure: Integer;
    FErasingColor      : TColor32;
    FEraseToHistory    : Boolean;
    { Brush Dynamics }
    FOriginalPressure  : Byte;

    procedure EraseToTransparent(Dest: TBitmap32; const X, Y: Integer);
    
    procedure EraseToColor(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet);
      
    procedure EraseToHistory(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet);
  public
    constructor Create; 
    destructor Destroy; override;

    procedure SetHistoryBitmap(const Source: TBitmap32);
    procedure SetErasingMode(const Mode: TgmErasingMode);
    procedure SetAirErasingPressure(const Amount: Integer);
    procedure SetErasingColor(const AColor: TColor32);
    
    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    property OriginalPressure: Byte    read FOriginalPressure write FOriginalPressure;
    property IsEraseToHistory: Boolean read FEraseToHistory   write FEraseToHistory;
  end;

//-- Background Eraser ---------------------------------------------------------

  TgmBackgroundEraser = class(TgmBrush)
  private
    FProtectedForeground: Boolean;
    FProtectedColor     : TColor32;
    FSampledColor       : TColor32;
    FErasingLimit       : TgmBackgroundEraserLimit;
    FSampledMode        : TgmBackgroundSamplingMode;
    FTolerance          : Real;
    { Brush Dynamics }
    FOriginalTolerance  : Byte;
  public
    constructor Create;

    procedure SetErasingLimit(const Limit: TgmBackgroundEraserLimit);
    procedure SetTolerance(const Tolerance: Byte);
    procedure SetSamplingMode(const Mode: TgmBackgroundSamplingMode);
    procedure SamplingColor(const X, Y: Integer);

    procedure Paint(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet); override;

    property ProtectedColor       : TColor32 read FProtectedColor      write FProtectedColor;
    property SampledColor         : TColor32 read FSampledColor        write FSampledColor;
    property IsProtectedForeground: Boolean  read FProtectedForeground write FProtectedForeground;
    property OriginalTolerance    : Byte     read FOriginalTolerance   write FOriginalTolerance;
  end;

//-- Air Brush -----------------------------------------------------------------

  TgmAirBrush = class(TObject)
  private
    FSourceBitmap              : TBitmap32;
    FMask                      : TBitmap32;
    FAirIntensity              : Byte;
    FHalfWidth                 : Integer;
    FHalfHeight                : Integer;
    FColor                     : TColor32;
    FInterval                  : Integer;
    FCounter                   : Cardinal;
    FBlendMode                 : TBlendMode32;
    FLockTransparent           : Boolean;
    FName                      : string;

    { Brush Dynamics part }
    FMaskCopy                   : TBitmap32;
    FSizeDynamicState           : TgmBrushDynamicState;
    FSizeDynamicCurrentSteps    : Integer;
    FSizeDynamicTotalSteps      : Integer;

    FOriginalPressure           : Byte;
    FPressureDynamicState       : TgmBrushDynamicState;
    FPressureDynamicCurrentSteps: Integer;
    FPressureDynamicTotalSteps  : Integer;

    FStartColor, FEndColor      : TColor32;
    FColorDynamicState          : TgmBrushDynamicState;
    FColorDynamicCurrentSteps   : Integer;
    FColorDynamicTotalSteps     : Integer;

    procedure SetAirIntensity(const Value: Byte);
    procedure SetInterval(const Value: Integer);
    procedure SetStrokeSizeByStep;
    function  GetAir: Boolean;
    function  GetStrokeColorByStep: TColor32;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateSourceBitmap(const Source: TBitmap32);
    procedure SetPaintingStroke(const AMask: TBitmap32);
    
    procedure Draw(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet);

    function GetBrushArea(const X, Y: Integer): TRect; 

    { Brush Dynamics }
    procedure SetDynamicSize(const AMask: TBitmap32;
      const ADynamicState: TgmBrushDynamicState; const ASteps: Integer);

    procedure SetDynamicPressure(const Pressure: Byte;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    procedure SetDynamicColor(const Color1, Color2: TColor32;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    property IsAir            : Boolean      read GetAir;
    property Color            : TColor32     read FColor        write FColor;
    property AirIntensity     : Byte         read FAirIntensity write SetAirIntensity;
    property Interval         : Integer      read FInterval     write SetInterval;
    property BlendMode        : TBlendMode32 read FBlendMode    write FBlendMode;
    property IsLockTransparent: Boolean                         write FLockTransparent;
    property Name             : string       read FName;
  end;

//-- JetGun --------------------------------------------------------------------

  // from GGCat
  TgmJetGun = class(TObject)
  private
    FSourceBitmap   : TBitmap32;
    FBlendMode      : TBlendMode32;
    FPressure       : Byte;
    FRadius         : Integer;
    FInterval       : Integer;
    FColor          : TColor32;
    FRandom         : Boolean;
    FJetGunPrepared : Boolean;
    FJetArrayIndex  : Integer;          // index of jet gun random spreading array
    FJetArray       : array of TPoint;  // jet gun random spreading array
    FLockTransparent: Boolean;
    FName           : string;

    { Dynamics Part }
    FOriginalRadius             : Integer;
    FRadiusDynamicState         : TgmBrushDynamicState;
    FRadiusDynamicCurrentSteps  : Integer;
    FRadiusDynamicTotalSteps    : Integer;

    FOriginalPressure           : Byte;
    FPressureDynamicState       : TgmBrushDynamicState;
    FPressureDynamicCurrentSteps: Integer;
    FPressureDynamicTotalSteps  : Integer;

    FStartColor, FEndColor      : TColor32;
    FColorDynamicState          : TgmBrushDynamicState;
    FColorDynamicCurrentSteps   : Integer;
    FColorDynamicTotalSteps     : Integer;

    procedure MakeJetArray;
    procedure SetRadius(const ARadius: Integer);
    function  GetStrokeColorByStep: TColor32;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetBlendMode(const BlendMode: TBlendMode32);
    procedure SetPressure(const Pressure: Byte);
    procedure UpdateSourceBitmap(const Source: TBitmap32);
    
    procedure Jet(Dest: TBitmap32; const X, Y: Integer;
      const ChannelSet: TgmChannelSet);

    function GetJetArea(const X, Y: Integer): TRect;

    { Dynamics part }
    procedure SetDynamicRadius(const ARadius: Integer;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    procedure SetDynamicPressure(const Pressure: Byte;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    procedure SetDynamicColor(const Color1, Color2: TColor32;
      const ADynamicState: TgmBrushDynamicState; const Steps: Integer);

    property Radius           : Integer  read FRadius   write SetRadius;
    property Interval         : Integer  read FInterval write FInterval;
    property Color            : TColor32 read FColor    write FColor;
    property IsRandom         : Boolean  read FRandom   write FRandom;
    property JetGunIndex      : Integer                 write FJetArrayIndex;
    property IsLockTransparent: Boolean                 write FLockTransparent;
    property Name             : string   read FName;
  end;

//-- TgmStroke -----------------------------------------------------------------

  TgmStroke = class(TObject)
  private
    FStrokeBitmap: TBitmap32;
    FHalfWidth   : Integer;  // half width of stroke.
    FHalfHeight  : Integer;  // half height of stroke.
    FRadius      : Integer;
    FSelected    : Boolean;
    FName        : string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetPaintingBrushSize;
    procedure GetPaintingBrushRadius;

    property StrokeBitmap: TBitmap32 read FStrokeBitmap;
    property Radius      : Integer   read FRadius;
    property IsSelected  : Boolean   read FSelected write FSelected;
    property Name        : string    read FName     write FName;
  end;

//-- TgmStrokeList -------------------------------------------------------------

  TgmStrokeList = class(TList)
  private
    FThumbnailSize       : Integer;
    FRowCount            : Integer;
    FColumnCount         : Integer;
    FThumbnailSizeMode   : TgmThumbnailSizeMode;
    FSelectedStroke      : TgmStroke;
    FSelectedIndex       : Integer;
    FStrokeStage         : TBitmap32;
    FFileName            : string;
    FBrushMark           : string;  // mark the .gmb file is brush file
    FOutputMsg           : string;  // output msg, such as errors, etc.
    FModified            : Boolean;
    FUsingInternalStrokes: Boolean;

    // compatible with old brush stroke files of GraphicsMagic
    function LoadOldStrokes(const AStream: TStream): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadInternalBrushesToList;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure DeleteAllBrushStrokes;
    procedure DeleteSelectedBrushStroke;
    procedure ChangeStrokeThumbnailSize(const AMode: TgmThumbnailSizeMode);
    procedure GetColumnCount;
    procedure GetRowCount;
    procedure ChangeStrokeStageHeight;
    procedure DrawStrokeStage(const AShowStage: TCustomImage32);
    procedure DrawStrokeBorder(const AShowStage: TCustomImage32);
    
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(const AStream: TStream): Boolean;
    function GetStrokeInfo(const X, Y: Integer): string;
    function SelectStrokeByIndex(const Index: Integer): Boolean;
    function GetCurrentStrokeInfo: string;
    function GetStrokeIndex(const X, Y: Integer): Integer;

    property SelectedStroke   : TgmStroke            read FSelectedStroke;
    property SelectedIndex    : Integer              read FSelectedIndex;
    property FileName         : string               read FFileName;
    property OutputMsg        : string               read FOutputMsg;
    property ThumbnailSizeMode: TgmThumbnailSizeMode read FThumbnailSizeMode write ChangeStrokeThumbnailSize;
    property IsModified       : Boolean              read FModified          write FModified;
    property IsUsingInternal  : Boolean              read FUsingInternalStrokes;
  end;

//-- Public procedures and functions -------------------------------------------

{ Based on GIMP }

  procedure GimpLUTSetup(var LUT: array of Byte; Func: TgmGimpLUTFunc; const Exposure: Double);
  // DodgeBurn brush
  function DodgeBurnHighlightsLUTFunc(const Exposure: Double; const Value: Real): Real;
  function DodgeBurnMidtonesLUTFunc  (const Exposure: Double; const Value: Real): Real;
  function DodgeBurnShadowsLUTFunc   (const Exposure: Double; const Value: Real): Real;

  procedure DodgeBurnInitializeLUT(var LUT: array of Byte);

  procedure DodgeBurnMakeLUT(const Exposure: Integer; var LUT: array of Byte;
    const DodgeBurnMode: TgmDodgeBurnMode; const DodgeBurnType: TgmDodgeBurnType);

  function DodgeBurnRangeList: TStringList;
  function GetDodgeBurnRangeIndex(Range: TgmDodgeBurnMode): Integer;
  function GetDodgeBurnModeString(const Mode: TgmDodgeBurnMode): string;

{ Eraser }

  function ErasingModeList: TStringList;
  function GetErasingModeIndex(const Mode: TgmErasingMode): Integer;
  function GetErasingMode(const Index: Integer): TgmErasingMode;
  function GetErasingModeString(const Mode: TgmErasingMode): string;

  function ErasingLimitList: TStringList;
  function GetErasingLimitIndex(const Limit: TgmBackgroundEraserLimit): Integer;
  function GetErasingLimit(const Index: Integer): TgmBackgroundEraserLimit;
  function GetErasingLimitString(const Limit: TgmBackgroundEraserLimit): string;
  function GetErasingSamplingString(const SamplingMode: TgmBackgroundSamplingMode): string;

  procedure MakeStrokeAgainstColor(const AStrokeBmp: TBitmap32;
    const AColor: TColor32);

const
  MAX_BRUSH_COUNT      : Integer = 42;
  STAMP_AIM_FLAG_RADIUS: Integer = 8;

  BLUR_SHARPEN_DEFAULT_PRESSURE             = 50;
  BLUR_SHARPEN_DEFAULT_TIMER_INTERVAL       = 100;
  SMUDGE_DEFAULT_PRESSURE                   = 50;
  DODGE_BURN_DEFAULT_EXPOSURE               = 50;  // for DodgeBurn Brush tool
  DODGE_BURN_DEFAULT_MODE: TgmDodgeBurnMode = dbmHighlights;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel,
  GR32_OrdinalMaps,
{ GraphicsMagic Lib }
  gmAlphaFuncs,         // ReplaceAlphaChannel_NewValue()...
  gmPaintFuncs,         
  gmImageProcessFuncs,
  gmGUIFuncs,
  CommonDataModule;     // Internal brush strokes

type
  TgmStrokeFileHeader = record
    FileID     : Cardinal; // must be $474D4253
    FileVersion: Cardinal; // version of the file format
    StrokeCount: Cardinal; // indicating how many strokes are in this file
  end;

  // info about each stroke in the file
  TgmStrokeInfoHeader = record
    Width : Cardinal;
    Height: Cardinal;
    Name  : ShortString;
  end;

const
  STROKE_FILE_ID      = $474D4253; // i.e. GMBS - GraphicsMagic Brush Strokes
  STROKE_FILE_VERSION = 1;         // the file version we could process so far

//-- Public procedures and functions -------------------------------------------

{ Based on GIMP }

procedure GimpLUTSetup(var LUT: array of Byte; Func: TgmGimpLUTFunc;
  const Exposure: Double);
var
  Value: Double;
  i    : Integer;
begin
  if High(LUT) = 255 then
  begin
    for i := 0 to 255 do
    begin
      Value := 255.0 * Func(Exposure, i / 255.0) + 0.5;

      if Value < 0.0 then
      begin
        LUT[i] := 0;
      end
      else if Value >= 255.0 then
      begin
        LUT[i] := 255;
      end
      else
      begin
        LUT[i] := Round(Value);
      end;
    end;
  end;
end; 

// DodgeBurn brush
function DodgeBurnHighlightsLUTFunc(const Exposure: Double; const Value: Real): Real;
var
  Factor: Real;
begin
  Factor := 1.0 + Exposure * 0.333333;
  Result := Factor * Value;
end;

function DodgeBurnMidtonesLUTFunc(const Exposure: Double; const Value: Real): Real;
var
  Factor: Real;
begin
  if Exposure < 0 then
  begin
    Factor := 1.0 - Exposure * 0.333333;
  end
  else
  begin
    Factor := 1 / (1.0 + Exposure);
  end;
    
  Result := Power(Value, Factor);
end;

function DodgeBurnShadowsLUTFunc(const Exposure: Double; const Value: Real): Real;
var
  Factor, NewValue: Real;
begin
  if Exposure >= 0 then
  begin
    Factor   := 0.333333 * Exposure;
    NewValue := Factor + Value - Factor * Value;
  end
  else  // PExposure < 0
  begin
    Factor := -0.333333 * Exposure;

    if Value < Factor then
    begin
      NewValue := 0;
    end
    else
    begin
      NewValue := (Value - Factor) / (1 - Factor);  // Factor <= Value <= 1
    end;
  end;
  
  Result := NewValue;
end;

procedure DodgeBurnInitializeLUT(var LUT: array of Byte);
var
  i: Integer;
begin
  if High(LUT) = 255 then
  begin
    for i := 0 to 255 do
    begin
      LUT[i] := 0;
    end;
  end;
end;

procedure DodgeBurnMakeLUT(const Exposure: Integer; var LUT: array of Byte;
  const DodgeBurnMode: TgmDodgeBurnMode; const DodgeBurnType: TgmDodgeBurnType);
var
  LUTFunc: TgmGimpLUTFunc;
  Exp    : Real;
begin
  Exp := Exposure / 100;

  { make the exposure negative if burn for lut }
  if DodgeBurnType = dbtBurn then
  begin
    Exp := -Exp;
  end;

  case DodgeBurnMode of
    dbmHighlights:
      begin
        LUTFunc := DodgeBurnHighlightsLUTFunc;
      end;

    dbmMidtones:
      begin
        LUTFunc := DodgeBurnMidtonesLUTFunc;
      end;

    dbmShadows:
      begin
        LUTFunc := DodgeBurnShadowsLUTFunc;
      end;

  else
    LUTFunc := nil;
  end;
  
  GimpLUTSetup(LUT, LUTFunc, Exp);
end;

function DodgeBurnRangeList: TStringList;
begin
  Result := TStringList.Create;

  with Result do
  begin
    Add('Highlights');
    Add('Midtones');
    Add('Shadows');
  end;
end; 

function GetDodgeBurnRangeIndex(Range: TgmDodgeBurnMode): Integer;
begin
  Result := -1;

  case Range of
    dbmHighlights:
      begin
        Result := 0;
      end;

    dbmMidtones:
      begin
        Result := 1;
      end;

    dbmShadows:
      begin
        Result := 2;
      end;
  end;
end; 

function GetDodgeBurnModeString(const Mode: TgmDodgeBurnMode): string;
var
  s: string;
begin
  case Mode of
    dbmHighlights:
      begin
        s := 'Highlights';
      end;

    dbmMidtones:
      begin
        s := 'Midtones';
      end;

    dbmShadows:
      begin
        s := 'Shadows';
      end;
  end;
  
  Result := s;
end; 

{ Eraser }

function ErasingModeList: TStringList;
begin
  Result := TStringList.Create;

  with Result do
  begin
    Add('Paintbrush');
    Add('Airbrush');
  end;
end;

function GetErasingModeIndex(const Mode: TgmErasingMode): Integer;
begin
  Result := -1;

  case Mode of
    emPaintBrush:
      begin
        Result := 0;
      end;

    emAirBrush:
      begin
        Result := 1;
      end;
  end;
end;

function GetErasingMode(const Index: Integer): TgmErasingMode;
begin
  if Index = 0 then
  begin
    Result := emPaintBrush;
  end
  else
  begin
    Result := emAirBrush;
  end;
end;

function GetErasingModeString(const Mode: TgmErasingMode): string;
var
  s: string;
begin
  case Mode of
    emPaintBrush:
      begin
        s := 'Paint Brush';
      end;

    emAirBrush:
      begin
        s := 'Air Brush';
      end;
  end;
  
  Result := s;
end;

function ErasingLimitList: TStringList;
begin
  Result := TStringList.Create;
  
  with Result do
  begin
    Add('Discontiguous');
    Add('Contiguous');
  end;
end;

function GetErasingLimitIndex(const Limit: TgmBackgroundEraserLimit): Integer;
begin
  Result := -1;
  
  case Limit of
    belDiscontiguous:
      begin
        Result := 0;
      end;

    belContiguous:
      begin
        Result := 1;
      end;
  end;
end;

function GetErasingLimit(const Index: Integer): TgmBackgroundEraserLimit;
begin
  if Index = 0 then
  begin
    Result := belDiscontiguous;
  end
  else
  begin
    Result := belContiguous;
  end;
end; 

function GetErasingLimitString(const Limit: TgmBackgroundEraserLimit): string;
var
  s: string;
begin
  case Limit of
    belDiscontiguous:
      begin
        s := 'Discontiguous';
      end;

    belContiguous:
      begin
        s := 'Contiguous';
      end;
  end;
  
  Result := s;
end; 

function GetErasingSamplingString(
  const SamplingMode: TgmBackgroundSamplingMode): string;
var
  s: string;
begin
  case SamplingMode of
    bsmContiguous:
      begin
        s := 'Contiguous';
      end;

    bsmOnce:
      begin
        s := 'Once';
      end;

    bsmBackgroundSwatch:
      begin
        s := 'Background Swatch';
      end;
  end;
  
  Result := s;
end; 

// used for displaying strokes properly
procedure MakeStrokeAgainstColor(const AStrokeBmp: TBitmap32;
  const AColor: TColor32);
var
  i, m      : Integer;
  br, bg, bb: Integer;
  r, g, b   : Cardinal;
  LBit      : PColor32;
begin
  br := AColor shr 16 and $FF;
  bg := AColor shr  8 and $FF;
  bb := AColor        and $FF;

  LBit := @AStrokeBmp.Bits[0];

  for i := 0 to (AStrokeBmp.Width * AStrokeBmp.Height - 1) do
  begin
    { The foreground color is assumed to black, its RGB value is zero,
      So, we don't need to do the actual combination of two colors. }
      
    m := LBit^ shr 16 and $FF;  // use the stroke color as mask
    r := br * m div 255;

    m := LBit^ shr 8 and $FF;
    g := bg * m div 255;
                                
    m := LBit^ and $FF;
    b := bb * m div 255;

    LBit^ := $FF000000 or (r shl 16) or (g shl 8) or b;

    Inc(LBit);
  end;
end; 

procedure CutRegionBySize(const Source: TBitmap32;
  const X, Y, Width, Height: Integer; var Dest: TBitmap32);
var
  i, j, HalfW, HalfH: Integer;
  SourceRow, DestRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow := nil;
  DestRow   := nil;

  Dest.Width  := Width;
  Dest.Height := Height;
  Dest.FillRect(0, 0, Dest.Width, Dest.Height, $00000000);

  HalfW := Trunc(Width  / 2);
  HalfH := Trunc(Height / 2);

  // copy pixels from Source bitmap, starting at (X - HalfW, Y - HalfH)
  for j := 0 to Dest.Height - 1 do
  begin
    if ( (Y - HalfH + j) < 0 )
    or ( (Y - HalfH + j) > (Source.Height - 1) )
    then Continue
    else
    begin
      SourceRow := Source.ScanLine[Y - HalfH + j];
      DestRow   := Dest.ScanLine[j];
    end;

    for i := 0 to Dest.Width - 1 do
    begin
      if ( (X - HalfW + i) < 0 ) or ( (X - HalfW + i) > (Source.Width - 1) ) then
        Continue
      else
        DestRow[i] := SourceRow[X - HalfW + i];
    end;
  end;

{$RANGECHECKS ON}
end; 

//-- TgmBrush ------------------------------------------------------------------

constructor TgmBrush.Create;
begin
  inherited Create;
  
  FBrushID               := bidNone;
  FSourceBitmap          := TBitmap32.Create;
  FSourceBitmap.DrawMode := dmBlend;
  FForeground            := TBitmap32.Create;
  FForeground.DrawMode   := dmBlend;
  FMask                  := TBitmap32.Create;
  FHalfWidth             := 0;
  FHalfHeight            := 0;
  FOpacity               := 100;
  FOpacityWeight         := MulDiv(255, FOpacity, 100);
  FBlendMode             := bbmNormal32;
  FPreserveTransparency  := False;
  FName                  := '';
  FSelectionOffsetX      := 0;
  FSelectionOffsetY      := 0;
  
  { Brush Dynamics }
  // Size
  FMaskCopy                := TBitmap32.Create;
  FSizeDynamicState        := bdsOff;
  FSizeDynamicCurrentSteps := 1;
  FSizeDynamicTotalSteps   := 1;
  // Opacity
  FOriginalOpacity            := 0;
  FOpacityDynamicState        := bdsOff;
  FOpacityDynamicCurrentSteps := 1;
  FOpacityDynamicTotalSteps   := 1;
  // Color
  FStartColor               := clBlack32;
  FEndColor                 := clWhite32;
  FColorDynamicState        := bdsOff;
  FColorDynamicCurrentSteps := 1;
  FColorDynamicTotalSteps   := 1;
end;

destructor TgmBrush.Destroy;
begin
  FSourceBitmap.Free;
  FForeground.Free;
  FMask.Free;
  { Brush Dynamics }
  FMaskCopy.Free;
  
  inherited Destroy;
end;

procedure TgmBrush.SetBrushOpacity(const Opacity: Integer);
begin
  FOpacity       := Opacity;
  FOpacityWeight := MulDiv(255, FOpacity, 100);
end;

procedure TgmBrush.SetBrushIntensity(const Intensity: Integer);
begin
  if (Intensity >= 0) and (Intensity <= 100) then
    FIntensity := MulDiv(255, Intensity, 100);
end;

procedure TgmBrush.SetBlendMode(const BlendMode: TBlendMode32);
begin
  if FBlendMode <> BlendMode then
    FBlendMode := BlendMode;
end;

procedure TgmBrush.SetPaintingStroke(const AMask: TBitmap32);
begin
  FMask.Assign(AMask);
  InvertBitmap32(FMask, [csGrayscale]);
  
  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;
end;

procedure TgmBrush.UpdateSourceBitmap(const Source: TBitmap32);
begin
  FSourceBitmap.Assign(Source);
  FSourceBitmap.DrawMode := dmBlend;
end;

procedure TgmBrush.UpdateForeground(const Source: TBitmap32);
begin
  FForeground.Assign(Source);
  FForeground.DrawMode := dmBlend;
end; 

function TgmBrush.GetBrushArea(const X, Y: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if (FHalfWidth > 0) and (FHalfHeight > 0) then
  begin
    Result.Left   := X - FHalfWidth;
    Result.Top    := Y - FHalfHeight;
    Result.Right  := Result.Left + FMask.Width;
    Result.Bottom := Result.Top  + FMask.Height;
  end;
end; 

{ Brush Dynamics }

procedure TgmBrush.SetDynamicSize(const AMask: TBitmap32;
  const ADynamicState: TgmBrushDynamicState; const ASteps: Integer);
begin
  if FSizeDynamicState <> ADynamicState then
  begin
    FSizeDynamicState := ADynamicState;
  end;

  if FSizeDynamicState = bdsFade then
  begin
    FMaskCopy.Assign(AMask);
    InvertBitmap32(FMaskCopy, [csGrayscale]);
    
    if FSizeDynamicCurrentSteps <> ASteps then
    begin
      FSizeDynamicCurrentSteps := ASteps;
    end;

    if FSizeDynamicTotalSteps <> ASteps then
    begin
      FSizeDynamicTotalSteps := ASteps;
    end;
  end;
end;

procedure TgmBrush.SetDynamicOpacity(const Opacity: Byte;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FOpacityDynamicState <> ADynamicState then
    FOpacityDynamicState := ADynamicState;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOriginalOpacity <> Opacity then
      FOriginalOpacity := Opacity;

    if FOpacityDynamicCurrentSteps <> Steps then
      FOpacityDynamicCurrentSteps := Steps;

    if FOpacityDynamicTotalSteps <> Steps then
      FOpacityDynamicTotalSteps := Steps;
  end;
end;

procedure TgmBrush.SetDynamicColor(const Color1, Color2: TColor32;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FColorDynamicState <> ADynamicState then
    FColorDynamicState := ADynamicState;

  if FColorDynamicState = bdsFade then
  begin
    if FStartColor <> Color1 then
      FStartColor := Color1;

    if FEndColor <> Color2 then
      FEndColor := Color2;

    if FColorDynamicCurrentSteps <> Steps then
      FColorDynamicCurrentSteps := Steps;
    
    if FColorDynamicTotalSteps <> Steps then
      FColorDynamicTotalSteps := Steps;
  end;
end;

procedure TgmBrush.SetStrokeSizeByStep;
begin
  FMask.Width  := MulDiv(FMaskCopy.Width,  FSizeDynamicCurrentSteps, FSizeDynamicTotalSteps);
  FMask.Height := MulDiv(FMaskCopy.Height, FSizeDynamicCurrentSteps, FSizeDynamicTotalSteps);

  if FMask.Width <= 0 then
    FMask.Width := 1;

  if FMask.Height <= 0 then
    FMask.Height := 1;

  FMask.Draw(FMask.Canvas.ClipRect, FMaskCopy.Canvas.ClipRect, FMaskCopy);
end;

function TgmBrush.GetStrokeColorByStep: TColor32;
var
  sr, sg, sb: Byte;
  er, eg, eb: Byte;
  gr, gg, gb: Cardinal;
  dr, dg, db: Integer;
begin
  sr     := FStartColor shr 16 and $FF;
  sg     := FStartColor shr  8 and $FF;
  sb     := FStartColor        and $FF;
  er     := FEndColor   shr 16 and $FF;
  eg     := FEndColor   shr  8 and $FF;
  eb     := FEndColor          and $FF;
  dr     := sr - er;
  dg     := sg - eg;
  db     := sb - eb;
  dr     := sr - MulDiv(dr, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  dg     := sg - MulDiv(dg, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  db     := sb - MulDiv(db, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  gr     := Clamp(dr, 0, 255);
  gg     := Clamp(dg, 0, 255);
  gb     := Clamp(db, 0, 255);
  Result := $FF000000 or (gr shl 16) or (gg shl 8) or gb;
end;

//-- TgmPaintBrush -------------------------------------------------------------

constructor TgmPaintBrush.Create;
begin
  inherited Create;
  
  FBrushID := bidPaintBrush;
  FColor   := clBlack32;
  FName    := 'Paintbrush';
end; 

procedure TgmPaintBrush.SetColor(const AColor: TColor32);
begin
  FColor := AColor;
end; 

procedure TgmPaintBrush.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount      : Integer;
  CurPaintX, CurPaintY       : Integer;
  MaskIntensity              : Byte;
  fa, fr, fg, fb             : Byte;
  ba, br, bg, bb             : Byte;
  BlendColor                 : TColor32;
  SourceRow, MaskRow, DestRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow       := nil;
  DestRow         := nil;
  MaskRow         := nil;
  RGBChannelCount := 0;

  if csRed in ChannelSet then
    Inc(RGBChannelCount);

  if csGreen in ChannelSet then
    Inc(RGBChannelCount);

  if csBlue in ChannelSet then
    Inc(RGBChannelCount);

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      FOpacityWeight := MulDiv(255, FOpacity, 100);
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FColorDynamicState = bdsFade then
  begin
    if FColorDynamicCurrentSteps > -1 then
    begin
      FColor := GetStrokeColorByStep;
      Dec(FColorDynamicCurrentSteps);
    end;
  end;

  // recalculate the half size of the brush stroke
  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      SourceRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow   := FMask.ScanLine[j];
      DestRow   := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
        Continue
      else
      begin
        MaskIntensity := MaskRow[i] and $FF;

        if MaskIntensity = 0 then
          Continue;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            BlendColor := RGBBlendByMode(FColor, SourceRow[CurPaintX], FOpacityWeight, FBlendMode);

            fb := Intensity(BlendColor);
            bb := Intensity(DestRow[CurPaintX]);
            bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (DestRow[CurPaintX] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            // blend color with blend mode
            if RGBChannelCount = 3 then
            begin
              BlendColor := ARGBBlendByMode(FColor, SourceRow[CurPaintX], FOpacityWeight, FBlendMode);

              if not FPreserveTransparency then
              begin
                // blend alpha by brush shape
                fa := BlendColor shr 24 and $FF;
                ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
              end;
            end
            else
            begin
              BlendColor := RGBBlendByMode(FColor, SourceRow[CurPaintX], FOpacityWeight, FBlendMode);
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr  8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end
        else // if paint on transparent area...
        begin
          // blend RGB by brush shape
          if not FPreserveTransparency then
          begin
            if  (csRed   in ChannelSet)
            and (csGreen in ChannelSet)
            and (csBlue  in ChannelSet) then
            begin
              fa := FOpacityWeight * MaskIntensity div 255;
              DestRow[CurPaintX] := (fa shl 24) or (FColor and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

//-- TgmHistoryBrush -----------------------------------------------------------

constructor TgmHistoryBrush.Create;
begin
  inherited Create;
  
  FBrushID       := bidHistoryBrush;
  FHistoryBitmap := TBitmap32.Create;
  FName          := 'History Brush';
end; 

destructor TgmHistoryBrush.Destroy;
begin
  FHistoryBitmap.Free;
  
  inherited Destroy;
end;

procedure TgmHistoryBrush.LoadHistoryBitmap(const HistoryBmp: TBitmap32);
begin
  FHistoryBitmap.Assign(HistoryBmp);
  FHistoryBitmap.DrawMode := dmBlend;
end;

// TODO -- by Ma Xiaoguang on March 18, 2014:
// There was a little rendering bug on restoring semi-transparent pixels
// to historical state with brush opacity set to anything that below 100%.
procedure TgmHistoryBrush.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, LRGBChannelCount : Integer;
  LCurPaintX, LCurPaintY : Integer;
  LSampleX, LSampleY     : Integer;
  LMaskIntensity         : Byte;
  fa, fr, fg, fb         : Byte;
  ba, br, bg, bb         : Byte;
  LBlendColor            : TColor32;
  LForeRow, LBackRow     : PColor32Array;
  LMaskRow, LDestRow     : PColor32Array;
begin
{$RANGECHECKS OFF}

  LForeRow         := nil;
  LBackRow         := nil;
  LMaskRow         := nil;
  LDestRow         := nil;
  LRGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if LRGBChannelCount = 0 then
  begin
    Exit;
  end;

  { Brush Dynamic }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps <= 0 then
    begin
      Exit;
    end;
    
    SetStrokeSizeByStep;
    Dec(FSizeDynamicCurrentSteps);
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps <= 0 then
    begin
      Exit;
    end;

    FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
    FOpacityWeight := MulDiv(255, FOpacity, 100);
    Dec(FOpacityDynamicCurrentSteps);
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // Blend with shaped mask
  for j := 0 to (FMask.Height - 1) do
  begin
    LCurPaintY := Y - FHalfHeight + j;
    LSampleY   := LCurPaintY + FSelectionOffsetY;

    if (LCurPaintY < 0) or (LCurPaintY >= Dest.Height) or
       (LSampleY   < 0) or (LSampleY   >= FHistoryBitmap.Height) then
    begin
      Continue;
    end
    else
    begin
      LForeRow := FHistoryBitmap.ScanLine[LSampleY];
      LBackRow := FSourceBitmap.ScanLine[LCurPaintY];
      LMaskRow := FMask.ScanLine[j];
      LDestRow := Dest.ScanLine[LCurPaintY];
    end;

    for i := 0 to (FMask.Width - 1) do
    begin
      LCurPaintX := X - FHalfWidth + i;
      LSampleX   := LCurPaintX + FSelectionOffsetX;

      if (LCurPaintX < 0) or (LCurPaintX >= Dest.Width) or
         (LSampleX   < 0) or (LSampleX   >= FHistoryBitmap.Width) then
      begin
        Continue;
      end
      else
      begin
        LMaskIntensity := LMaskRow[i] and $FF;

        if LMaskIntensity = 0 then
        begin
          Continue;
        end;

        fa := LForeRow[LSampleX] shr 24 and $FF;
        ba := LDestRow[LCurPaintX] shr 24 and $FF;

        // blending history and background with blend mode
        if LRGBChannelCount = 3 then
        begin
          if not FPreserveTransparency then
          begin
            LBlendColor := RGBBlendByMode(LForeRow[LSampleX], LBackRow[LCurPaintX],
                                         FOpacityWeight, FBlendMode);

            // blend alpha by brush shape
            ba := ba + ( (fa - ba) * FOpacityWeight div 255 ) * LMaskIntensity div 255;
          end
          else
          begin
            LBlendColor := ARGBBlendByMode(LForeRow[LSampleX], LBackRow[LCurPaintX],
                                           FOpacityWeight, FBlendMode);
          end;

          br := LDestRow[LCurPaintX] shr 16 and $FF;
          bg := LDestRow[LCurPaintX] shr  8 and $FF;
          bb := LDestRow[LCurPaintX]        and $FF;

          if csRed in ChannelSet then
          begin
            fr := LBlendColor shr 16 and $FF;
            br := ( fr * LMaskIntensity + br * (255 - LMaskIntensity) ) div 255;
          end;

          if csGreen in ChannelSet then
          begin
            fg := LBlendColor shr 8 and $FF;
            bg := ( fg * LMaskIntensity + bg * (255 - LMaskIntensity) ) div 255;
          end;

          if csBlue in ChannelSet then
          begin
            fb := LBlendColor and $FF;
            bb := ( fb * LMaskIntensity + bb * (255 - LMaskIntensity) ) div 255;
          end;

          LDestRow[LCurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

//-- TgmCloneStamp -------------------------------------------------------------

constructor TgmCloneStamp.Create;
begin
  inherited Create;
  
  FBrushID            := bidCloneStamp;
  FSamplingPoint      := Point(0, 0);
  FSamplingPointExist := False;
  FUpdateStampOffset  := True;
  FXOffset            := 0;
  FYOffset            := 0;
  FName               := 'Clone Stamp';
end; 

procedure TgmCloneStamp.SetSamplingPoint(const X, Y: Integer);
begin
  FSamplingPoint      := Point(X, Y);
  FSamplingPointExist := True;  // mark the sampling point is already exist
  FUpdateStampOffset  := True;  // mark that we need to recalculate the offsets
end;

procedure TgmCloneStamp.SetStampPointOffset(const X, Y: Integer);
begin
  FXOffset           := FSamplingPoint.X - X;
  FYOffset           := FSamplingPoint.Y - Y;
  FUpdateStampOffset := False;  // don't need recalculate the offsets.
end; 

procedure TgmCloneStamp.DrawStampAimFlag(ACanvas: TCanvas; const X, Y: Integer);
var
  StampPoint: TPoint;
begin
  StampPoint.X := X;
  StampPoint.Y := Y;
  DrawAimFlag(ACanvas, StampPoint, STAMP_AIM_FLAG_RADIUS);
end;

{ Note, we sample pixels from the source bitmap, and blend them with the original
  foreground pixels and draw them on the destination. }
procedure TgmCloneStamp.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount     : Integer;
  CurStampX, CurStampY      : Integer;
  CurPaintX, CurPaintY      : Integer;
  MaskIntensity, SampleAlpha: Byte;
  AdjustedOpacity           : Byte;
  fa, fr, fg, fb            : Byte;
  ba, br, bg, bb            : Byte;
  BlendColor                : TColor32;
  ForeRow, BackRow          : PColor32Array;
  MaskRow, DestRow          : PColor32Array;
  StampPoint                : TPoint;
begin
{$RANGECHECKS OFF}

  ForeRow := nil;
  BackRow := nil;
  MaskRow := nil;
  DestRow := nil;

  RGBChannelCount := 0;

  if csRed in ChannelSet then
    Inc(RGBChannelCount);

  if csGreen in ChannelSet then
    Inc(RGBChannelCount);

  if csBlue in ChannelSet then
    Inc(RGBChannelCount);

  { Add the selection offset to the current coordinate could convert the
    selection coordinate (if it is) to layer coordinate. }
  StampPoint.X := X + SelectionOffsetX + FXOffset;
  StampPoint.Y := Y + SelectionOffsetY + FYOffset;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      FOpacityWeight := MulDiv(255, FOpacity, 100);
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // Blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurStampY := StampPoint.Y - FHalfHeight + j;
    CurPaintY := Y - FHalfHeight + j;

    if (CurStampY < 0) or (CurStampY >= FSourceBitmap.Height)
    or (CurPaintY < 0) or (CurPaintY >= Dest.Height)
    then Continue
    else
    begin
      ForeRow := FSourceBitmap.ScanLine[CurStampY];  // sampling on this
      BackRow := FForeground.ScanLine[CurPaintY];
      MaskRow := FMask.ScanLine[j];
      DestRow := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurStampX := StampPoint.X - FHalfWidth + i;
      CurPaintX := X - FHalfWidth + i;

      if (CurStampX < 0) or (CurStampX >= FSourceBitmap.Width)
      or (CurPaintX < 0) or (CurPaintX >= Dest.Width)
      then Continue
      else
      begin
        MaskIntensity := MaskRow[i] and $FF;

        if MaskIntensity = 0 then
          Continue;

        fa := ForeRow[CurStampX] shr 24 and $FF;  // get alpha channel of the sample pixel

        // if the sample pixel is transparent, then go to the next loop
        if fa = 0 then
          Continue;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            { If the foreground has semi-transparent pixels, we need to adjust
              the blending opacity. }

            if fa < 255 then
            begin
              AdjustedOpacity := fa * FOpacityWeight div 255;
            end
            else
            begin
              AdjustedOpacity := FOpacityWeight;
            end;

            BlendColor := RGBBlendByMode(ForeRow[CurStampX], BackRow[CurPaintX], AdjustedOpacity, FBlendMode);
            fb         := Intensity(BlendColor);
            bb         := Intensity(DestRow[CurPaintX]);
            bb         := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (ba shl 24) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            // blend foreground and dest with blend mode
            if RGBChannelCount = 3 then
            begin
              BlendColor := ARGBBlendByMode(ForeRow[CurStampX], BackRow[CurPaintX], FOpacityWeight, FBlendMode);

              if not FPreserveTransparency then
              begin
                // blend alpha by brush shape
                fa := BlendColor shr 24 and $FF;
                ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
              end;
            end
            else
            begin
              { If the foreground has semi-transparent pixels, we need to
                adjust the blending opacity. }

              if fa < 255 then
              begin
                AdjustedOpacity := fa * FOpacityWeight div 255;
              end
              else
              begin
                AdjustedOpacity := FOpacityWeight;
              end;

              BlendColor := RGBBlendByMode(ForeRow[CurStampX], BackRow[CurPaintX], AdjustedOpacity, FBlendMode);
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr  8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end
        else
        begin // paint on transparent area...
          if not FPreserveTransparency then
          begin
            if  (csRed   in ChannelSet)
            and (csGreen in ChannelSet)
            and (csBlue  in ChannelSet) then
            begin
              fa := fa * FOpacityWeight div 255;
              fa := fa * MaskIntensity div 255;

              DestRow[CurPaintX] := (fa shl 24) or (ForeRow[CurPaintX] and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

//-- TgmPatternStamp -----------------------------------------------------------

constructor TgmPatternStamp.Create;
begin
  inherited Create;
  
  FBrushID                := bidPatternStamp;
  FPatternBitmap          := TBitmap32.Create;
  FPatternBitmap.DrawMode := dmOpaque;
  FName                   := 'Pattern Stamp';
end; 

destructor TgmPatternStamp.Destroy;
begin
  if Assigned(FPatternBitmap.Canvas.Brush.Bitmap) then
    FPatternBitmap.Canvas.Brush.Bitmap := nil;

  FPatternBitmap.Free;

  inherited Destroy;
end;

procedure TgmPatternStamp.SetPatternBitmap(const APatternBmp: TBitmap32;
  const AWidth, AHeight: Integer);
begin
  FPatternBitmap.DrawMode := dmBlend;
  FPatternBitmap.SetSize(AWidth, AHeight);
  FPatternBitmap.Clear($00000000);
  DrawPattern(FPatternBitmap, APatternBmp);
end;

procedure TgmPatternStamp.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount         : Integer;
  CurPaintX, CurPaintY          : Integer;
  MaskIntensity, AdjustedOpacity: Byte;
  fa, fr, fg, fb, ba, br, bg, bb: Byte;
  BlendColor                    : TColor32;
  ForeRow, BackRow              : PColor32Array;
  MaskRow, DestRow              : PColor32Array;
begin
{$RANGECHECKS OFF}

  ForeRow := nil;
  BackRow := nil;
  MaskRow := nil;
  DestRow := nil;

  RGBChannelCount := 0;

  if csRed in ChannelSet then
    Inc(RGBChannelCount);

  if csGreen in ChannelSet then
    Inc(RGBChannelCount);

  if csBlue in ChannelSet then
    Inc(RGBChannelCount);

  { Brush Dynamic }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      FOpacityWeight := MulDiv(255, FOpacity, 100);
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      ForeRow := FPatternBitmap.ScanLine[CurPaintY];
      BackRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow := FMask.ScanLine[j];
      DestRow := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
        Continue
      else
      begin
        MaskIntensity := MaskRow[i] and $FF;

        if MaskIntensity = 0 then
          Continue;

        fa := ForeRow[CurPaintX] shr 24 and $FF;

        // do nothing if the foreground is fully transparent
        if fa = 0 then
          Continue;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            { If the foreground has semi-transparent pixels, we need to
              adjust the blending opacity. }

            if fa < 255 then
            begin
              AdjustedOpacity := fa * FOpacityWeight div 255;
            end
            else
            begin
              AdjustedOpacity := FOpacityWeight;
            end;

            // blending pattern and background with blend mode
            BlendColor := RGBBlendByMode(ForeRow[CurPaintX], BackRow[CurPaintX], AdjustedOpacity, FBlendMode);

            fb := Intensity(BlendColor);
            bb := Intensity(DestRow[CurPaintX]);
            bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (ba shl 24) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            // blend foreground and dest with blend mode
            if RGBChannelCount = 3 then
            begin
              // blending pattern and background with blend mode
              BlendColor := ARGBBlendByMode(ForeRow[CurPaintX], BackRow[CurPaintX], FOpacityWeight, FBlendMode);

              if not FPreserveTransparency then
              begin
                // blend alpha by brush shape
                fa := BlendColor shr 24 and $FF;
                ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
              end;
            end
            else
            begin
              { If the foreground has semi-transparent pixels, we need to
                adjust the blending opacity. }

              if fa < 255 then
              begin
                AdjustedOpacity := fa * FOpacityWeight div 255;
              end
              else
              begin
                AdjustedOpacity := FOpacityWeight;
              end;

              // blending pattern and background with blend mode
              BlendColor := RGBBlendByMode(ForeRow[CurPaintX], BackRow[CurPaintX], AdjustedOpacity, FBlendMode);
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr  8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end
        else // paint on transparent area...
        begin
          if not FPreserveTransparency then
          begin
            if  (csRed   in ChannelSet)
            and (csGreen in ChannelSet)
            and (csBlue  in ChannelSet) then
            begin
              fa := fa * FOpacityWeight div 255;
              fa := fa * MaskIntensity div 255;

              DestRow[CurPaintX] := (fa shl 24) or (ForeRow[CurPaintX] and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

//-- TgmBlurSharpenBrush -------------------------------------------------------

constructor TgmBlurSharpenBrush.Create(const AType: TgmConvolveType);
begin
  inherited Create;

  FConvolver := TgmConvolver.Create;
  FBrushID   := bidBlurSharpen;
  FType      := AType;
  FPressure  := BLUR_SHARPEN_DEFAULT_PRESSURE;

  case FType of
    gmctBlur:
      begin
        FName := 'Blur Tool';
      end;

    gmctSharpen:
      begin
        FName := 'Sharpen Tool';
      end;
  end;

  // dynamic settings
  FOriginalPressure            := FPressure;
  FPressureDynamicState        := bdsOff;
  FPressureDynamicCurrentSteps := 1;
  FPressureDynamicTotalSteps   := 1;
end;

destructor TgmBlurSharpenBrush.Destroy;
begin
  FConvolver.Free;
  inherited Destroy;
end;

procedure TgmBlurSharpenBrush.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  TempBmp                  : TBitmap32;
  i, j, RGBChannelCount    : Integer;
  CurPaintX, CurPaintY     : Integer;
  MaskIntensity            : Byte;
  fa, fr, fg, fb           : Byte;
  ba, br, bg, bb           : Byte;
  BlendColor               : TColor32;
  ForeRow, MaskRow, DestRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  ForeRow := nil;
  MaskRow := nil;
  DestRow := nil;

  RGBChannelCount := 0;

  if csRed in ChannelSet then
    Inc(RGBChannelCount);

  if csGreen in ChannelSet then
    Inc(RGBChannelCount);

  if csBlue in ChannelSet then
    Inc(RGBChannelCount);

  { Brush Dynamic }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FPressureDynamicState = bdsFade then
  begin
    if FPressureDynamicCurrentSteps > 0 then
    begin
      FPressure := MulDiv(FOriginalPressure, FPressureDynamicCurrentSteps, FPressureDynamicTotalSteps);
      Dec(FPressureDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  FConvolver.CalculateMatrix(FType, FHalfWidth, FHalfHeight, FPressure);

  TempBmp := TBitmap32.Create;
  try
    CutRegionBySize(FSourceBitmap, X, Y, FMask.Width, FMask.Height, FForeground);

    TempBmp.DrawMode := dmBlend;
    TempBmp.Assign(FForeground);

    FConvolver.ConvolveRegion(TempBmp, FForeground, 3, True);

    // update the source bitmap with the processed area
    FSourceBitmap.Draw(X - FHalfWidth, Y - FHalfHeight, FForeground);
  finally
    TempBmp.Free;
  end;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      ForeRow := FForeground.ScanLine[j];
      MaskRow := FMask.ScanLine[j];
      DestRow := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
        Continue
      else
      begin
        MaskIntensity := MaskRow[i] and $FF;

        if MaskIntensity = 0 then
          Continue;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blending foreground and background with blend mode
          BlendColor := RGBBlendByMode(ForeRow[i], DestRow[CurPaintX], 255, FBlendMode);

          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            fb := Intensity(BlendColor);
            bb := Intensity(DestRow[CurPaintX]);
            bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (DestRow[CurPaintX] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            if RGBChannelCount = 3 then
            begin
              if not FPreserveTransparency then
              begin
                fa := ForeRow[i] shr 24 and $FF;
                ba := ( fa * MaskIntensity + ba * (255 - MaskIntensity) ) div 255;
              end;
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr  8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

procedure TgmBlurSharpenBrush.SetDynamicPressure(const APressure: Integer;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FPressureDynamicState <> ADynamicState then
    FPressureDynamicState := ADynamicState;

  if FPressureDynamicState = bdsFade then
  begin
    if FOriginalPressure <> APressure then
      FOriginalPressure := APressure;

    if FPressureDynamicTotalSteps <> Steps then
      FPressureDynamicTotalSteps := Steps;

    if FPressureDynamicCurrentSteps <> Steps then
      FPressureDynamicCurrentSteps := Steps;
  end;
end;

//-- TgmSmudge -----------------------------------------------------------------

constructor TgmSmudge.Create;
begin
  inherited Create;
  
  FBrushID  := bidSmudge;
  FPressure := 0;
  FName     := 'Smudge Tool';
end;

procedure TgmSmudge.SetPressure(const Pressure: Integer);
begin
  if (Pressure >= 0) and (Pressure <= 100) then
  begin
    if FPressure <> Pressure then
      FPressure := Pressure;
  end;
end;

procedure TgmSmudge.CutRegionToForegroundBySize(const Source: TBitmap32;
  const X, Y: Integer);
begin
  CutRegionBySize(Source, X, Y, FMask.Width, FMask.Height, FForeground);
end;

procedure TgmSmudge.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  TempForeground               : TBitmap32;
  Left, Top, Right, Bottom     : Integer;
  ForeHalfW, ForeHalfH         : Integer;
  MaskHalfW, MaskHalfH         : Integer;
  i, j, CurPaintX, CurPaintY   : Integer;
  PressureWeight, MaskIntensity: Byte;
  fa, ba, da                   : Byte;
  fr, fg, fb                   : Byte;
  br, bg, bb                   : Byte;
  dr, dg, db                   : Byte;
  BlendColor                   : TColor32;
  ForeRow, MaskRow, DestRow    : PColor32Array;
begin
{$RANGECHECKS OFF}

  ForeRow := nil;
  MaskRow := nil;
  DestRow := nil;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;

      ForeHalfW := FForeground.Width  div 2;
      ForeHalfH := FForeground.Height div 2;
      MaskHalfW := FMask.Width        div 2;
      MaskHalfH := FMask.Height       div 2;

      Left   := ForeHalfW - MaskHalfW;
      Top    := ForeHalfH - MaskHalfH;
      Right  := ForeHalfW + MaskHalfW;
      Bottom := ForeHalfH + MaskHalfH;

      TempForeground := TBitmap32.Create;
      try
        TempForeground.Assign(FForeground);
        TempForeground.DrawMode := dmBlend;
        FForeground.Width       := FMask.Width;
        FForeground.Height      := FMask.Height;
        FForeground.Draw( FForeground.Canvas.ClipRect, Rect(Left, Top, Right, Bottom), TempForeground );
      finally
        TempForeground.Free;
      end;
      Dec(FSizeDynamicCurrentSteps);
    end
    else Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FPressure := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  PressureWeight := MulDiv(255, FPressure, 100);

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      ForeRow := FForeground.ScanLine[j];
      MaskRow := FMask.ScanLine[j];
      DestRow := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
        Continue
      else
      begin
        { Get mask intensity, since the mask is grayscale bitmap, so we only
          need one component value of RGB as the intensity. }
        MaskIntensity := MaskRow[i] and $FF;

        if MaskIntensity = 0 then
          Continue;

        // Accumulate foreground with background by pressure
        fa := ForeRow[i] shr 24 and $FF;
        fr := ForeRow[i] shr 16 and $FF;
        fg := ForeRow[i] shr  8 and $FF;
        fb := ForeRow[i]        and $FF;

        ba := DestRow[CurPaintX] shr 24 and $FF;
        br := DestRow[CurPaintX] shr 16 and $FF;
        bg := DestRow[CurPaintX] shr  8 and $FF;
        bb := DestRow[CurPaintX]        and $FF;

        // Need to accumulate the alpha channel, too.
        fa := ( fa * PressureWeight + ba * (255 - PressureWeight) ) div 255;
        fr := ( fr * PressureWeight + br * (255 - PressureWeight) ) div 255;
        fg := ( fg * PressureWeight + bg * (255 - PressureWeight) ) div 255;
        fb := ( fb * PressureWeight + bb * (255 - PressureWeight) ) div 255;
        ForeRow[i] := (fa shl 24) or (fr shl 16) or (fg shl 8) or fb;

        { first of all, blend pixels of background bitmap with white color
          by background alpha channels for getting right color values }
        dr := ( ba * br + (255 - ba) * 255 ) div 255;
        dg := ( ba * bg + (255 - ba) * 255 ) div 255;
        db := ( ba * bb + (255 - ba) * 255 ) div 255;
        BlendColor := (fa shl 24) or (dr shl 16) or (dg shl 8) or db;

        // Blending foreground and background with BlendMode
        BlendColor := RGBBlendByMode(ForeRow[i], BlendColor, 255, FBlendMode);

        { Blend the alpha channel of foreground with the alpha channel of
          the destination by the Mask. Then let it to be the final alpha channel. }
        da := ( fa * MaskIntensity + ba * (255 - MaskIntensity) ) div 255;

        if csGrayscale in ChannelSet then
        begin
          fb := Intensity(BlendColor);
          bb := Intensity(DestRow[CurPaintX]);
          db := ( MaskIntensity * fb + (255 - MaskIntensity) * bb ) div 255;

          if FPreserveTransparency then
            DestRow[CurPaintX] := (ba shl 24) or (db shl 16) or (db shl 8) or db
          else
            DestRow[CurPaintX] := (da shl 24) or (db shl 16) or (db shl 8) or db;
        end
        else
        begin
          if csRed in ChannelSet then
          begin
            fr := BlendColor shr 16 and $FF;
            dr := ( MaskIntensity * fr + (255 - MaskIntensity) * br ) div 255;
          end
          else
            dr := br;

          if csGreen in ChannelSet then
          begin
            fg := BlendColor shr 8 and $FF;
            dg := ( MaskIntensity * fg + (255 - MaskIntensity) * bg ) div 255;
          end
          else
            dg := bg;

          if csBlue in ChannelSet then
          begin
            fb := BlendColor and $FF;
            db := ( MaskIntensity * fb + (255 - MaskIntensity) * bb ) div 255;
          end
          else
            db := bb;

          if FPreserveTransparency then
            DestRow[CurPaintX] := (ba shl 24) or (dr shl 16) or (dg shl 8) or db
          else
            DestRow[CurPaintX] := (da shl 24) or (dr shl 16) or (dg shl 8) or db;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

//-- TgmDodgeBurnBrush ---------------------------------------------------------

constructor TgmDodgeBurnBrush.Create(const ADodgeBurnType: TgmDodgeBurnType);
begin
  inherited Create;
  
  FBrushID       := bidDodgeBurn;
  FExposure      := 0;
  FDodgeBurnMode := dbmHighlights;
  FDodgeBurnType := ADodgeBurnType;

  DodgeBurnInitializeLUT(FDodgeBurnLUT);

  case FDodgeBurnType of
    dbtDodge: FName := 'Dodge Tool';
    dbtBurn : FName := 'Burn Tool';
  end;
end; 

procedure TgmDodgeBurnBrush.SetDodgeBurnExposure(const Exposure: Integer);
begin
  if FExposure <> Exposure then
    FExposure := Exposure;
end;

procedure TgmDodgeBurnBrush.SetDodgeBurnMode(const Mode: TgmDodgeBurnMode);
begin
  if FDodgeBurnMode <> Mode then
    FDodgeBurnMode := Mode;
end;

procedure TgmDodgeBurnBrush.MakeLUT;
begin
  DodgeBurnMakeLUT(FExposure, FDodgeBurnLUT, FDodgeBurnMode, FDodgeBurnType);
end;

procedure TgmDodgeBurnBrush.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j                          : Integer;
  CurPaintX, CurPaintY          : Integer;
  ba                            : Cardinal;
  fr, fg, fb, br, bg, bb, Weight: Byte;
  SourceRow, MaskRow, DestRow   : PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow := nil;
  MaskRow   := nil;
  DestRow   := nil;
  fr        := 0;
  fg        := 0;
  fb        := 0;

  if High(FDodgeBurnLUT) = 255 then
  begin
    { Brush Dynamics }
    if FSizeDynamicState = bdsFade then
    begin
      if FSizeDynamicCurrentSteps > 0 then
      begin
        SetStrokeSizeByStep;
        Dec(FSizeDynamicCurrentSteps);
      end
      else
        Exit;
    end;

    if FOpacityDynamicState = bdsFade then
    begin
      if FOpacityDynamicCurrentSteps > 0 then
      begin
        FExposure := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
        MakeLUT;
        Dec(FOpacityDynamicCurrentSteps);
      end
      else
        Exit;
    end;

    FHalfWidth  := FMask.Width  div 2;
    FHalfHeight := FMask.Height div 2;

    // blend with shaped mask
    for j := 0 to FMask.Height - 1 do
    begin
      CurPaintY := Y - FHalfHeight + j;

      if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
        Continue
      else
      begin
        SourceRow := FSourceBitmap.ScanLine[CurPaintY];
        MaskRow   := FMask.ScanLine[j];
        DestRow   := Dest.ScanLine[CurPaintY];
      end;

      for i := 0 to FMask.Width - 1 do
      begin
        CurPaintX := X - FHalfWidth + i;

        if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
          Continue
        else
        begin
          // blend with mask
          Weight := MaskRow[i] and $FF;

          if Weight = 0 then
            Continue;

          // dodge or burn adjustment for foreground color

          ba := SourceRow[CurPaintX] and $FF000000;

          if ba > 0 then
          begin
            if csGrayscale in ChannelSet then
            begin
              bb := Intensity(SourceRow[CurPaintX]);
              fb := FDodgeBurnLUT[bb];
              fb := ( FOpacityWeight * fb + (255 - FOpacityWeight) * bb ) div 255;

              bb := Intensity(DestRow[CurPaintX]);
              bb := ( Weight * fb + (255 - Weight) * bb ) div 255;

              DestRow[CurPaintX] := ba or (bb shl 16) or (bb shl 8) or bb;
            end
            else
            begin
              if csRed in ChannelSet then
              begin
                br := SourceRow[CurPaintX] shr 16 and $FF;
                // change the RGB values with the FDodgeBurnLUT
                fr := FDodgeBurnLUT[br];
                // blending foreground and background with opacity setting
                fr := ( FOpacityWeight * fr + (255 - FOpacityWeight) * br ) div 255;
              end;

              if csGreen in ChannelSet then
              begin
                bg := SourceRow[CurPaintX] shr 8 and $FF;
                fg := FDodgeBurnLUT[bg];
                fg := ( FOpacityWeight * fg + (255 - FOpacityWeight) * bg ) div 255;
              end;

              if csBlue in ChannelSet then
              begin
                bb := SourceRow[CurPaintX] and $FF;
                fb := FDodgeBurnLUT[bb];
                fb := ( FOpacityWeight * fb + (255 - FOpacityWeight) * bb ) div 255;
              end;

              br := DestRow[CurPaintX] shr 16 and $FF;
              bg := DestRow[CurPaintX] shr  8 and $FF;
              bb := DestRow[CurPaintX]        and $FF;

              if csRed in ChannelSet then
                br := ( Weight * fr + (255 - Weight) * br ) div 255;

              if csGreen in ChannelSet then
                bg := ( Weight * fg + (255 - Weight) * bg ) div 255;

              if csBlue in ChannelSet then
                bb := ( Weight * fb + (255 - Weight) * bb ) div 255;

              DestRow[CurPaintX] := ba or (br shl 16) or (bg shl 8) or bb;
            end;
          end
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

//-- TgmLightBrush -------------------------------------------------------------

constructor TgmLightBrush.Create(const ALightBrushMode: TgmLightBrushMode);
begin
  inherited Create;
  
  FBrushID        := bidLightBrush;
  FLightBrushMode := ALightBrushMode;

  case FLightBrushMode of
    lbmHighHue       : FName := 'High Hue Brush';
    lbmLowHue        : FName := 'Low Hue Brush';
    lbmHighSaturation: FName := 'High Saturation Brush';
    lbmLowSaturation : FName := 'Low Saturation Brush';
    lbmHighLuminosity: FName := 'High Luminosity Brush';
    lbmLowLuminosity : FName := 'Low Luminosity Brush';
    lbmBrightness    : FName := 'Brightness Brush';
    lbmDarkness      : FName := 'Darkness Brush';
    lbmHighContrast  : FName := 'High Contrast Brush';
    lbmLowContrast   : FName := 'Low Contrast Brush';
  end;
end; 

procedure TgmLightBrush.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, CurPaintX, CurPaintY    : Integer;
  ba                            : Cardinal;
  fr, fg, fb, br, bg, bb, Weight: Byte;
  ForeRow, MaskRow, DestRow     : PColor32Array;
begin
{$RANGECHECKS OFF}

  { Brush Dynamic }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FIntensity := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  CutRegionBySize(FSourceBitmap, X, Y, FMask.Width, FMask.Height, FForeground);

  case FLightBrushMode of
    lbmHighHue       : AdjustImageHLS(FForeground, FForeground, FIntensity, 0, 0);
    lbmLowHue        : AdjustImageHLS(FForeground, FForeground, 0 - FIntensity, 0, 0);
    lbmHighSaturation: AdjustImageHLS(FForeground, FForeground, 0, 0, FIntensity);
    lbmLowSaturation : AdjustImageHLS(FForeground, FForeground, 0, 0, 0 - FIntensity);
    lbmHighLuminosity: AdjustImageHLS(FForeground, FForeground, 0, FIntensity, 0);
    lbmLowLuminosity : AdjustImageHLS(FForeground, FForeground, 0, 0 - FIntensity, 0);
    lbmBrightness    : Brightness32(FForeground, FIntensity);
    lbmDarkness      : Brightness32(FForeground, 0 - FIntensity);
    lbmHighContrast  : Contrast32(FForeground, FIntensity);
    lbmLowContrast   : Contrast32(FForeground, 0 - FIntensity);
  end;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      ForeRow := FForeground.ScanLine[j];
      MaskRow := FMask.ScanLine[j];
      DestRow := Dest.ScanLine[CurPaintY];

      for i := 0 to FMask.Width - 1 do
      begin
        CurPaintX := X - FHalfWidth + i;

        if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
          Continue
        else
        begin
          // blend with mask
          Weight := MaskRow[i] and $FF;

          if Weight = 0 then
            Continue;

          // get original alpha channel
          ba := DestRow[CurPaintX] and $FF000000;

          if ba > 0 then
          begin
            if csGrayscale in ChannelSet then
            begin
              fb := Intensity(ForeRow[i]);
              bb := Intensity(DestRow[CurPaintX]);
              bb := ( Weight * fb + (255 - Weight) * bb ) div 255;

              DestRow[CurPaintX] := ba or (bb shl 16) or (bb shl 8) or bb;
            end
            else
            begin
              fr := ForeRow[i] shr 16 and $FF;
              fg := ForeRow[i] shr  8 and $FF;
              fb := ForeRow[i]        and $FF;

              br := DestRow[CurPaintX] shr 16 and $FF;
              bg := DestRow[CurPaintX] shr  8 and $FF;
              bb := DestRow[CurPaintX]        and $FF;

              if csRed in ChannelSet then
                br := ( Weight * fr + (255 - Weight) * br ) div 255;

              if csGreen in ChannelSet then
                bg := ( Weight * fg + (255 - Weight) * bg ) div 255;

              if csBlue in ChannelSet then
                bb := ( Weight * fb + (255 - Weight) * bb ) div 255;

              // blending the original alpha channel and new RGB values.
              DestRow[CurPaintX] := ba or (br shl 16) or (bg shl 8) or bb;
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

//-- TgmEraser -----------------------------------------------------------------

constructor TgmEraser.Create;
begin
  inherited Create;
  
  FBrushID              := bidEraser;
  FHistoryBitmap        := TBitmap32.Create;
  FErasingMode          := emPaintBrush;
  FAirErasingPressure   := 0;
  FPreserveTransparency := False;
  FErasingColor         := clWhite32;
  FEraseToHistory       := False;
  FName                 := 'Eraser';
  { Brush Dynamics }
  FOriginalPressure := 0;
end; 

destructor TgmEraser.Destroy;
begin
  FHistoryBitmap.Free;
  inherited Destroy;
end;

procedure TgmEraser.SetHistoryBitmap(const Source: TBitmap32);
begin
  FHistoryBitmap.Assign(Source);
end;

procedure TgmEraser.SetErasingMode(const Mode: TgmErasingMode);
begin
  if FErasingMode <> Mode then
  begin
    FErasingMode := Mode;
  end;
end;

procedure TgmEraser.SetAirErasingPressure(const Amount: Integer);
begin
  if FAirErasingPressure <> Amount then
  begin
    FAirErasingPressure := Amount;
  end;
end;

procedure TgmEraser.SetErasingColor(const AColor: TColor32);
begin
  if FErasingColor <> AColor then
    FErasingColor := AColor;
end; 

procedure TgmEraser.EraseToTransparent(Dest: TBitmap32; const X, Y: Integer);
var
  i, j                       : Integer;
  CurPaintX, CurPaintY       : Integer;
  MaskIntensity, ba, fa      : Byte;
  DestRGB                    : TColor32;
  SourceRow, MaskRow, DestRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow := nil;
  MaskRow   := nil;
  DestRow   := nil;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      case FErasingMode of
        emPaintBrush:
          begin
            FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
            FOpacityWeight := MulDiv(255, FOpacity, 100);
          end;

        emAirBrush:
          FAirErasingPressure := MulDiv(FOriginalPressure, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
      end;

      Dec(FOpacityDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // Blend with mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      SourceRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow   := FMask.Scanline[j];
      DestRow   := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
        Continue
      else
      begin
        { Get mask intensity, since the mask is grayscale bitmap, so we only
          need one component value of RGB as the intensity. }
        MaskIntensity := MaskRow[i] and $FF;

        if FErasingMode = emAirBrush then
          MaskIntensity := MulDiv(MaskIntensity, FAirErasingPressure, 100);

        if MaskIntensity > 0 then
        begin
          fa := $0;

          // blend alpha channel with opacity setting
          if FErasingMode = emPaintBrush then
          begin
            ba := SourceRow[CurPaintX] shr 24 and $FF;
            fa := ba * ($FF - FOpacityWeight) div 255;
          end;

          ba      := DestRow[CurPaintX] shr 24 and $FF; // get alpha of destination bitmap
          DestRGB := DestRow[CurPaintX] and $00FFFFFF;

          // blend alpha channel with mask
          fa := ( MaskIntensity * fa + (255 - MaskIntensity) * ba ) div 255;

          DestRow[CurPaintX] := (fa shl 24) or DestRGB;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure TgmEraser.EraseToColor(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount         : Integer;
  CurPaintX, CurPaintY          : Integer;
  MaskIntensity                 : Byte;
  fa, fr, fg, fb, ba, br, bg, bb: Byte;
  BlendColor                    : TColor32;
  SourceRow, MaskRow, DestRow   : PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow       := nil;
  MaskRow         := nil;
  DestRow         := nil;
  RGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      case FErasingMode of
        emPaintBrush:
          begin
            FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
            FOpacityWeight := MulDiv(255, FOpacity, 100);
          end;

        emAirBrush:
          begin
            FAirErasingPressure := MulDiv(FOriginalPressure, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
          end;
      end;

      Dec(FOpacityDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
    begin
      Continue;
    end
    else
    begin
      SourceRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow   := FMask.ScanLine[j];
      DestRow   := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
      begin
        Continue;
      end
      else
      begin
        { Get mask intensity, since the mask is grayscale bitmap, so we only
          need one component value of RGB as the intensity. }
        MaskIntensity := MaskRow[i] and $FF;

        // adjust mask with Air Pressure setting
        if FErasingMode = emAirBrush then
        begin
          MaskIntensity := MulDiv(MaskIntensity, FAirErasingPressure, 100);
        end;

        if MaskIntensity = 0 then
        begin
          Continue;
        end;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            BlendColor := RGBBlendByMode(FErasingColor, SourceRow[CurPaintX], FOpacityWeight, bbmNormal32);

            fb := Intensity(BlendColor);
            bb := Intensity(DestRow[CurPaintX]);
            bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (DestRow[CurPaintX] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            // blend color with blend mode
            if RGBChannelCount = 3 then
            begin
              BlendColor := ARGBBlendByMode(FErasingColor, SourceRow[CurPaintX], FOpacityWeight, bbmNormal32);

              if not FPreserveTransparency then
              begin
                // blend alpha by brush shape
                fa := BlendColor shr 24 and $FF;
                ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
              end;
            end
            else
            begin
              BlendColor := RGBBlendByMode(FErasingColor, SourceRow[CurPaintX], FOpacityWeight, bbmNormal32);
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr  8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end
        else // if paint on transparent area...
        begin
          if not FPreserveTransparency then
          begin
            if  (csRed   in ChannelSet)
            and (csGreen in ChannelSet)
            and (csBlue  in ChannelSet) then
            begin
              DestRow[CurPaintX] := (MaskIntensity shl 24) or (FErasingColor and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

procedure TgmEraser.EraseToHistory(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount         : Integer;
  CurPaintX, CurPaintY          : Integer;
  SampleX, SampleY              : Integer;
  MaskIntensity, AdjustedOpacity: Byte;
  fa, fr, fg, fb                : Byte;
  ba, br, bg, bb                : Byte;
  BlendColor                    : TColor32;
  HistoryRow, SourceRow         : PColor32Array;
  MaskRow, DestRow              : PColor32Array;
begin
{$RANGECHECKS OFF}

  HistoryRow      := nil;
  SourceRow       := nil;
  MaskRow         := nil;
  DestRow         := nil;
  RGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      case FErasingMode of
        emPaintBrush:
          begin
            FOpacity       := MulDiv(FOriginalOpacity, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
            FOpacityWeight := MulDiv(255, FOpacity, 100);
          end;
          
        emAirBrush:
          begin
            FAirErasingPressure := MulDiv(FOriginalPressure, FOpacityDynamicCurrentSteps, FOpacityDynamicTotalSteps);
          end;
      end;

      Dec(FOpacityDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;
    SampleY   := CurPaintY + FSelectionOffsetY;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height)
    or (SampleY   < 0) or (SampleY   >= FHistoryBitmap.Height) then
    begin
      Continue;
    end
    else
    begin
      HistoryRow := FHistoryBitmap.ScanLine[SampleY];
      SourceRow  := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow    := FMask.ScanLine[j];
      DestRow    := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;
      SampleX   := CurPaintX + FSelectionOffsetX;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width)
      or (SampleX   < 0) or (SampleX   >= FHistoryBitmap.Width) then
      begin
        Continue;
      end
      else
      begin
        { Get mask intensity, since the mask is grayscale bitmap, so we only
          need one component value of RGB as the intensity. }
        MaskIntensity := MaskRow[i] and $FF;

        // adjust mask with Air Pressure setting
        if FErasingMode = emAirBrush then
        begin
          MaskIntensity := MulDiv(MaskIntensity, FAirErasingPressure, 100);
        end;

        if MaskIntensity = 0 then
        begin
          Continue;
        end;

        fa := HistoryRow[SampleX] shr 24 and $FF;

        // do nothing if the foreground is fully transparent
        if fa = 0 then
        begin
          Continue;
        end;

        ba := DestRow[CurPaintX] shr 24 and $FF;

        if ba > 0 then
        begin
          // blend RGB by brush shape
          if csGrayscale in ChannelSet then
          begin
            { If the foreground has semi-transparent pixels, we need to
              adjust the blending opacity. }

            if fa < 255 then
            begin
              AdjustedOpacity := fa * FOpacityWeight div 255;
            end
            else
            begin
              AdjustedOpacity := FOpacityWeight;
            end;

            // blending history and background with blend mode
            BlendColor := RGBBlendByMode(HistoryRow[SampleX], SourceRow[CurPaintX], AdjustedOpacity, bbmNormal32);
            fb         := Intensity(BlendColor);
            bb         := Intensity(DestRow[CurPaintX]);
            bb         := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

            DestRow[CurPaintX] := (DestRow[CurPaintX] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
          end
          else
          begin
            // blend foreground and dest with blend mode
            if RGBChannelCount = 3 then
            begin
              // blending history and background with blend mode
              BlendColor := ARGBBlendByMode(HistoryRow[SampleX], SourceRow[CurPaintX], FOpacityWeight, bbmNormal32);

              if not FPreserveTransparency then
              begin
                // blend alpha by brush shape
                fa := BlendColor shr 24 and $FF;
                ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
              end;
            end
            else
            begin
              { If the foreground has semi-transparent pixels, we need to
                adjust the blending opacity. }

              if fa < 255 then
              begin
                AdjustedOpacity := fa * FOpacityWeight div 255;
              end
              else
              begin
                AdjustedOpacity := FOpacityWeight;
              end;

              BlendColor := RGBBlendByMode(HistoryRow[SampleX], SourceRow[CurPaintX], AdjustedOpacity, bbmNormal32);
            end;

            br := DestRow[CurPaintX] shr 16 and $FF;
            bg := DestRow[CurPaintX] shr  8 and $FF;
            bb := DestRow[CurPaintX]        and $FF;

            if csRed in ChannelSet then
            begin
              fr := BlendColor shr 16 and $FF;
              br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
            end;

            if csGreen in ChannelSet then
            begin
              fg := BlendColor shr 8 and $FF;
              bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
            end;

            if csBlue in ChannelSet then
            begin
              fb := BlendColor and $FF;
              bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
            end;

            DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
          end;
        end
        else // if paint on transparent area...
        begin
          if not FPreserveTransparency then
          begin
            if  (csRed   in ChannelSet)
            and (csGreen in ChannelSet)
            and (csBlue  in ChannelSet) then
            begin
              fa := fa * FOpacityWeight div 255;
              fa := fa * MaskIntensity  div 255;

              DestRow[CurPaintX] := (fa shl 24) or (HistoryRow[CurPaintX] and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

procedure TgmEraser.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
begin
  if FEraseToHistory then
  begin
    EraseToHistory(Dest, X, Y, ChannelSet);
  end
  else
  begin
    if FPreserveTransparency then
    begin
      EraseToColor(Dest, X, Y, ChannelSet);
    end
    else
    begin
      EraseToTransparent(Dest, X, Y);
    end;
  end;
end; 

//-- Background Eraser ---------------------------------------------------------

constructor TgmBackgroundEraser.Create;
begin
  inherited Create;
  
  FBrushID             := bidBackgroundEraser;
  FProtectedColor      := clBlack32;
  FSampledColor        := clWhite32;
  FErasingLimit        := belDiscontiguous;
  FSampledMode         := bsmContiguous;
  FTolerance           := 0;
  FProtectedForeground := False;
  FName                := 'Background Eraser';
  { Brush Dynamics }
  FOriginalTolerance := 0;
end;

procedure TgmBackgroundEraser.SetErasingLimit(
  const Limit: TgmBackgroundEraserLimit);
begin
  if FErasingLimit <> Limit then
    FErasingLimit := Limit;
end;

procedure TgmBackgroundEraser.SetTolerance(const Tolerance: Byte);
begin
  FTolerance := Tolerance / 100;
end; 

procedure TgmBackgroundEraser.SetSamplingMode(
  const Mode: TgmBackgroundSamplingMode);
begin
  if FSampledMode <> Mode then
    FSampledMode := Mode;
end; 

procedure TgmBackgroundEraser.SamplingColor(const X, Y: Integer);
var
  Alpha: Byte;
begin
  Alpha := FSourceBitmap.PixelS[X, Y] shr 24 and $FF;

  if Alpha <> 0 then
    FSampledColor := FSourceBitmap.PixelS[X, Y];
end; 

procedure TgmBackgroundEraser.Paint(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  da, na, ba, fa             : Byte;
  pr, pg, pb                 : Byte;
  SmpR, SmpG, SmpB           : Byte;
  PixR, PixG, PixB           : Byte;
  i, j                       : Integer;
  CurPaintX, CurPaintY       : Integer;
  DestRGB                    : TColor32;
  SourceRow, MaskRow, DestRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  SourceRow := nil;
  MaskRow   := nil;
  DestRow   := nil;
  fa        := 0;

  { Brush Dynamics }
  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  if FOpacityDynamicState = bdsFade then
  begin
    if FOpacityDynamicCurrentSteps > 0 then
    begin
      FTolerance := FOriginalTolerance * FOpacityDynamicCurrentSteps / FOpacityDynamicTotalSteps / 100;
      Dec(FOpacityDynamicCurrentSteps);
    end
    else
    begin
      Exit;
    end;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  pr := FProtectedColor shr 16 and $FF;
  pg := FProtectedColor shr  8 and $FF;
  pb := FProtectedColor        and $FF;

  // blend with mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
    begin
      Continue;
    end
    else
    begin
      SourceRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow   := FMask.Scanline[j];
      DestRow   := Dest.ScanLine[CurPaintY];
    end;

    for i := 0 to FMask.Width - 1 do
    begin
      CurPaintX := X - FHalfWidth + i;

      if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
      begin
        Continue;
      end
      else
      begin
        PixR := SourceRow[CurPaintX] shr 16 and $FF;
        PixG := SourceRow[CurPaintX] shr  8 and $FF;
        PixB := SourceRow[CurPaintX]        and $FF;

        if FProtectedForeground then
        begin
          if  ( Abs(pr - PixR) <= FTolerance * 150 )
          and ( Abs(pg - PixG) <= FTolerance * 150 )
          and ( Abs(pb - PixB) <= FTolerance * 150 ) then
          begin
            Continue;
          end;
        end;

        SmpR := FSampledColor shr 16 and $FF;
        SmpG := FSampledColor shr  8 and $FF;
        SmpB := FSampledColor        and $FF;

        if  ( Abs(SmpR - PixR) <= FTolerance * 150 )
        and ( Abs(SmpG - PixG) <= FTolerance * 150 )
        and ( Abs(SmpB - PixB) <= FTolerance * 150 ) then
        begin
          { Get mask intensity, since the mask is grayscale bitmap, so we only
            need one component value of RGB as the intensity. }
          na := MaskRow[i] and $FF;

          // get alpha of process bitmap.
          ba      := DestRow[CurPaintX] shr 24 and $FF;
          DestRGB := DestRow[CurPaintX] and $00FFFFFF;

          case FErasingLimit of
            belDiscontiguous: fa := DestRow[CurPaintX]   shr 24 and $FF;
            belContiguous   : fa := SourceRow[CurPaintX] shr 24 and $FF;
          end;

          da := fa * (255 - na) div 255;
          da := MinIntValue([ba, da]);
          DestRow[CurPaintX] := (da shl 24) or DestRGB;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

//-- Air Brush -----------------------------------------------------------------

constructor TgmAirBrush.Create;
begin
  inherited Create;

  FSourceBitmap          := TBitmap32.Create;
  FSourceBitmap.DrawMode := dmBlend;
  FMask                  := TBitmap32.Create;
  FCounter               := GetTickCount;
  FInterval              := 100;
  FAirIntensity          := 10;
  FColor                 := clBlack32;
  FBlendMode             := bbmNormal32;
  FLockTransparent       := False;
  FName                  := 'Airbrush';
  { Brush Dynamics }
  // Size
  FMaskCopy                := TBitmap32.Create;
  FSizeDynamicState        := bdsOff;
  FSizeDynamicCurrentSteps := 1;
  FSizeDynamicTotalSteps   := 1;
  // Pressure
  FOriginalPressure            := 0;
  FPressureDynamicState        := bdsOff;
  FPressureDynamicCurrentSteps := 1;
  FPressureDynamicTotalSteps   := 1;
  // Color
  FStartColor               := clBlack32;
  FEndColor                 := clWhite32;
  FColorDynamicState        := bdsOff;
  FColorDynamicCurrentSteps := 1;
  FColorDynamicTotalSteps   := 1;
end;

destructor TgmAirBrush.Destroy;
begin
  FSourceBitmap.Free;
  FMask.Free;
  inherited Destroy;
end;

procedure TgmAirBrush.SetAirIntensity(const Value: Byte);
begin
  if Value <> FAirIntensity then
  begin
    if (Value >= 1) and (Value <= 100) then
      FAirIntensity := Value;
  end;
end;

procedure TgmAirBrush.SetInterval(const Value: Integer);
begin
  if Value <> FInterval then
    FInterval := Value;
end;

function TgmAirBrush.GetAir: Boolean;
begin
  if Abs( GetTickCount - FCounter ) > FInterval then
  begin
    Result   := True;
    FCounter := GetTickCount;
  end
  else
    Result := False;
end;

procedure TgmAirBrush.UpdateSourceBitmap(const Source: TBitmap32);
begin
  FSourceBitmap.Assign(Source);
  FSourceBitmap.DrawMode := dmBlend;
end; 

procedure TgmAirBrush.SetPaintingStroke(const AMask: TBitmap32);
begin
  FMask.Assign(AMask);
  InvertBitmap32(FMask, [csGrayscale]);
  
  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;
end; 

procedure TgmAirBrush.SetStrokeSizeByStep;
begin
  FMask.Width  := MulDiv(FMaskCopy.Width,  FSizeDynamicCurrentSteps, FSizeDynamicTotalSteps);
  FMask.Height := MulDiv(FMaskCopy.Height, FSizeDynamicCurrentSteps, FSizeDynamicTotalSteps);

  if FMask.Width <= 0 then
    FMask.Width := 1;

  if FMask.Height <= 0 then
    FMask.Height := 1;

  FMask.Draw(FMask.Canvas.ClipRect, FMaskCopy.Canvas.ClipRect, FMaskCopy);
end;

function TgmAirBrush.GetStrokeColorByStep: TColor32;
var
  sr, sg, sb: Byte;
  er, eg, eb: Byte;
  gr, gg, gb: Cardinal;
  dr, dg, db: Integer;
begin
  sr     := FStartColor shr 16 and $FF;
  sg     := FStartColor shr  8 and $FF;
  sb     := FStartColor        and $FF;
  er     := FEndColor   shr 16 and $FF;
  eg     := FEndColor   shr  8 and $FF;
  eb     := FEndColor          and $FF;
  dr     := sr - er;
  dg     := sg - eg;
  db     := sb - eb;
  dr     := sr - MulDiv(dr, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  dg     := sg - MulDiv(dg, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  db     := sb - MulDiv(db, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  gr     := Clamp(dr, 0, 255);
  gg     := Clamp(dg, 0, 255);
  gb     := Clamp(db, 0, 255);
  Result := $FF000000 or (gr shl 16) or (gg shl 8) or gb;
end;

procedure TgmAirBrush.Draw(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, j, RGBChannelCount      : Integer;
  CurPaintX, CurPaintY       : Integer;
  MaskIntensity              : Byte;
  fa, fr, fg, fb             : Byte;
  ba, br, bg, bb             : Byte;
  BlendColor                 : TColor32;
  SourceRow, DestRow, MaskRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  RGBChannelCount := 0;

  if csRed in ChannelSet then
    Inc(RGBChannelCount);

  if csGreen in ChannelSet then
    Inc(RGBChannelCount);

  if csBlue in ChannelSet then
    Inc(RGBChannelCount);

  if FSizeDynamicState = bdsFade then
  begin
    if FSizeDynamicCurrentSteps > 0 then
    begin
      SetStrokeSizeByStep;
      Dec(FSizeDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FPressureDynamicState = bdsFade then
  begin
    if FPressureDynamicCurrentSteps > 0 then
    begin
      FAirIntensity := MulDiv(FOriginalPressure, FPressureDynamicCurrentSteps, FPressureDynamicTotalSteps);
      Dec(FPressureDynamicCurrentSteps);
    end
    else
      Exit;
  end;

  if FColorDynamicState = bdsFade then
  begin
    if FColorDynamicCurrentSteps > -1 then
    begin
      FColor := GetStrokeColorByStep;
      Dec(FColorDynamicCurrentSteps);
    end;
  end;

  FHalfWidth  := FMask.Width  div 2;
  FHalfHeight := FMask.Height div 2;

  // blend with shaped mask
  for j := 0 to FMask.Height - 1 do
  begin
    CurPaintY := Y - FHalfHeight + j;

    if (CurPaintY < 0) or (CurPaintY >= Dest.Height) then
      Continue
    else
    begin
      SourceRow := FSourceBitmap.ScanLine[CurPaintY];
      MaskRow   := FMask.ScanLine[j];
      DestRow   := Dest.ScanLine[CurPaintY];

      for i := 0 to FMask.Width - 1 do
      begin
        CurPaintX := X - FHalfWidth + i;

        if (CurPaintX < 0) or (CurPaintX >= Dest.Width) then
          Continue
        else
        begin
          { Get mask intensity, since the mask is grayscale bitmap, so we only
            need one component value of RGB as the intensity. }

          // adjust mask with intensity setting
          MaskIntensity := (MaskRow[i] and $FF) * FAirIntensity div 100;

          if MaskIntensity = 0 then
            Continue;

          ba := DestRow[CurPaintX] shr 24 and $FF;

          // blend the color and background with blend mode setting
          if ba > 0 then
          begin
            // blend RGB by brush shape
            if csGrayscale in ChannelSet then
            begin
              BlendColor := RGBBlendByMode(FColor, SourceRow[CurPaintX], 255, FBlendMode);
              fb         := Intensity(BlendColor);
              bb         := Intensity(DestRow[CurPaintX]);
              bb         := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;

              DestRow[CurPaintX] := (DestRow[CurPaintX] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
            end
            else
            begin
              // blend color with blend mode
              if RGBChannelCount = 3 then
              begin
                BlendColor := ARGBBlendByMode(FColor, SourceRow[CurPaintX], 255, FBlendMode);

                if not FLockTransparent then
                begin
                  // blend alpha by brush shape
                  fa := BlendColor shr 24 and $FF;
                  ba := (fa * MaskIntensity + ba * (255 - MaskIntensity)) div 255;
                end;
              end
              else
              begin
                BlendColor := RGBBlendByMode(FColor, SourceRow[CurPaintX], 255, FBlendMode);
              end;

              br := DestRow[CurPaintX] shr 16 and $FF;
              bg := DestRow[CurPaintX] shr  8 and $FF;
              bb := DestRow[CurPaintX]        and $FF;

              if csRed in ChannelSet then
              begin
                fr := BlendColor shr 16 and $FF;
                br := ( fr * MaskIntensity + br * (255 - MaskIntensity) ) div 255;
              end;

              if csGreen in ChannelSet then
              begin
                fg := BlendColor shr  8 and $FF;
                bg := ( fg * MaskIntensity + bg * (255 - MaskIntensity) ) div 255;
              end;

              if csBlue in ChannelSet then
              begin
                fb := BlendColor and $FF;
                bb := ( fb * MaskIntensity + bb * (255 - MaskIntensity) ) div 255;
              end;

              DestRow[CurPaintX] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
            end;
          end
          else // if paint on transparent area...
          begin
            if not FLockTransparent then
            begin
              if  (csRed   in ChannelSet)
              and (csGreen in ChannelSet)
              and (csBlue  in ChannelSet) then
                DestRow[CurPaintX] := (MaskIntensity shl 24) or (FColor and $FFFFFF);
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

function TgmAirBrush.GetBrushArea(const X, Y: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if (FHalfWidth > 0) and (FHalfHeight > 0) then
  begin
    Result.Left   := X - FHalfWidth;
    Result.Top    := Y - FHalfHeight;
    Result.Right  := Result.Left + FMask.Width;
    Result.Bottom := Result.Top  + FMask.Height;
  end;
end;

{ Brush Dynamics }

procedure TgmAirBrush.SetDynamicSize(const AMask: TBitmap32;
  const ADynamicState: TgmBrushDynamicState; const ASteps: Integer);
begin
  if FSizeDynamicState <> ADynamicState then
  begin
    FSizeDynamicState := ADynamicState;
  end;
  
  if FSizeDynamicState = bdsFade then
  begin
    FMaskCopy.Assign(AMask);
    InvertBitmap32(FMaskCopy, [csGrayscale]);

    if FSizeDynamicCurrentSteps <> ASteps then
    begin
      FSizeDynamicCurrentSteps := ASteps;
    end;

    if FSizeDynamicTotalSteps <> ASteps then
    begin
      FSizeDynamicTotalSteps := ASteps;
    end;
  end;
end;

procedure TgmAirBrush.SetDynamicPressure(const Pressure: Byte;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FPressureDynamicState <> ADynamicState then
    FPressureDynamicState := ADynamicState;

  if FPressureDynamicState = bdsFade then
  begin
    if FOriginalPressure <> Pressure then
      FOriginalPressure := Pressure;

    if FPressureDynamicCurrentSteps <> Steps then
      FPressureDynamicCurrentSteps := Steps;

    if FPressureDynamicTotalSteps <> Steps then
      FPressureDynamicTotalSteps := Steps;
  end;
end;

procedure TgmAirBrush.SetDynamicColor(const Color1, Color2: TColor32;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FColorDynamicState <> ADynamicState then
    FColorDynamicState := ADynamicState;

  if FColorDynamicState = bdsFade then
  begin
    if FStartColor <> Color1 then
      FStartColor := Color1;

    if FEndColor <> Color2 then
      FEndColor := Color2;

    if FColorDynamicCurrentSteps <> Steps then
      FColorDynamicCurrentSteps := Steps;
    
    if FColorDynamicTotalSteps <> Steps then
      FColorDynamicTotalSteps := Steps;
  end;
end;

//-- JetGun --------------------------------------------------------------------

{ From GGCat }
constructor TgmJetGun.Create;
begin
  inherited Create;
  
  FSourceBitmap          := TBitmap32.Create;
  FSourceBitmap.DrawMode := dmBlend;
  FBlendMode             := bbmNormal32;
  FRadius                := 0;
  FInterval              := 100;
  FColor                 := clBlack32;
  FRandom                := False;
  FJetGunPrepared        := False;
  FJetArrayIndex         := 0;
  FJetArray              := nil;
  FLockTransparent       := False;
  FName                  := 'Jet Gun';

  { Brush Dynamics }
  // Size
  FOriginalRadius            := 0;
  FRadiusDynamicState        := bdsOff;
  FRadiusDynamicCurrentSteps := 1;
  FRadiusDynamicTotalSteps   := 1;
  // Pressure
  FOriginalPressure            := 100;
  FPressureDynamicState        := bdsOff;
  FPressureDynamicCurrentSteps := 1;
  FPressureDynamicTotalSteps   := 1;
  // Color
  FStartColor               := clBlack32;
  FEndColor                 := clWhite32;
  FColorDynamicState        := bdsOff;
  FColorDynamicCurrentSteps := 1;
  FColorDynamicTotalSteps   := 1;
end;

destructor TgmJetGun.Destroy;
begin
  FSourceBitmap.Free;
  inherited Destroy;
end;

procedure TgmJetGun.MakeJetArray;
var
  x, y, i  : Integer;
  TempPoint: TPoint;
begin
  SetLength(FJetArray, 0);
  // generate points that within the boundary of specified radius
  for x := -FRadius to FRadius do
    for y := -FRadius to FRadius do
      if Round( Sqrt(x * x + y * y) ) <= FRadius then
      begin
        // Add a point to array
        SetLength( FJetArray, High(FJetArray) + 2 );
        FJetArray[High(FJetArray)] := Point(x, y);
      end;

  // randomly extract points in the array and swap each other
  for i := 0 to High(FJetArray) * 10 do
  begin
    x            := Random(High(FJetArray));
    y            := Random(High(FJetArray));
    TempPoint    := FJetArray[x];
    FJetArray[x] := FJetArray[y];
    FJetArray[y] := TempPoint;
  end;
  
  FJetGunPrepared := True;
end;

procedure TgmJetGun.SetRadius(const ARadius: Integer);
begin
  if FRadius <> ARadius then
  begin
    FRadius := ARadius;
    MakeJetArray;
  end;
end;

procedure TgmJetGun.SetBlendMode(const BlendMode: TBlendMode32);
begin
  if FBlendMode <> BlendMode then
    FBlendMode := BlendMode;
end;

procedure TgmJetGun.SetPressure(const Pressure: Byte);
begin
  FPressure := MulDiv(255, Pressure, 100);
end;

procedure TgmJetGun.UpdateSourceBitmap(const Source: TBitmap32);
begin
  FSourceBitmap.Assign(Source);
  FSourceBitmap.DrawMode := dmBlend;
end;

function TgmJetGun.GetStrokeColorByStep: TColor32;
var
  sr, sg, sb: Byte;
  er, eg, eb: Byte;
  gr, gg, gb: Cardinal;
  dr, dg, db: Integer;
begin
  sr     := FStartColor shr 16 and $FF;
  sg     := FStartColor shr  8 and $FF;
  sb     := FStartColor        and $FF;
  er     := FEndColor   shr 16 and $FF;
  eg     := FEndColor   shr  8 and $FF;
  eb     := FEndColor          and $FF;
  dr     := sr - er;
  dg     := sg - eg;
  db     := sb - eb;
  dr     := sr - MulDiv(dr, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  dg     := sg - MulDiv(dg, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  db     := sb - MulDiv(db, FColorDynamicTotalSteps - FColorDynamicCurrentSteps, FColorDynamicTotalSteps);
  gr     := Clamp(dr, 0, 255);
  gg     := Clamp(dg, 0, 255);
  gb     := Clamp(db, 0, 255);
  Result := $FF000000 or (gr shl 16) or (gg shl 8) or gb;
end;

procedure TgmJetGun.Jet(Dest: TBitmap32; const X, Y: Integer;
  const ChannelSet: TgmChannelSet);
var
  i, RGBChannelCount   : Integer;
  CurPaintX, CurPaintY : Integer;
  ba, br, bg, bb       : Byte;
  BackColor, BlendColor: TColor32;
begin
  RGBChannelCount := 0;

  if csRed in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csGreen in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if csBlue in ChannelSet then
  begin
    Inc(RGBChannelCount);
  end;

  if FJetGunPrepared then
  begin
    // Process dynamics
    if FRadiusDynamicState = bdsFade then
    begin
      if FRadiusDynamicCurrentSteps > 0 then
      begin
        FRadius := MulDiv(FOriginalRadius, FRadiusDynamicCurrentSteps, FRadiusDynamicTotalSteps);
        MakeJetArray;
        Dec(FRadiusDynamicCurrentSteps);
      end
      else
      begin
        Exit;
      end;
    end;

    if FPressureDynamicState = bdsFade then
    begin
      if FPressureDynamicCurrentSteps > 0 then
      begin
        FPressure := MulDiv(FOriginalPressure, FPressureDynamicCurrentSteps, FPressureDynamicTotalSteps);
        Dec(FPressureDynamicCurrentSteps);
      end
      else
      begin
        Exit;
      end;
    end;

    if not FRandom then
    begin
      if FColorDynamicState = bdsFade then
      begin
        if FColorDynamicCurrentSteps > -1 then
        begin
          FColor := GetStrokeColorByStep;
          Dec(FColorDynamicCurrentSteps);
        end;
      end;
    end;

    for i := 0 to FRadius * FRadius div 5 do
    begin
      if FRandom then
      begin
        FColor := $FF000000 or Cardinal(Random($00FFFFFF));
      end;

      CurPaintX := X + FJetArray[FJetArrayIndex].X;
      CurPaintY := Y + FJetArray[FJetArrayIndex].Y;
      BackColor := FSourceBitmap.PixelS[CurPaintX, CurPaintY];

      ba := Dest.PixelS[CurPaintX, CurPaintY] shr 24 and $FF;

      if ba > 0 then
      begin
        if csGrayscale in ChannelSet then
        begin
          BlendColor := RGBBlendByMode(FColor, BackColor, FPressure, FBlendMode);
          bb         := Intensity(BlendColor);

          Dest.PixelS[CurPaintX, CurPaintY] := (Dest.PixelS[CurPaintX, CurPaintY] and $FF000000) or (bb shl 16) or (bb shl 8) or bb;
        end
        else
        begin
          // blend color with blend mode
          if RGBChannelCount = 3 then
          begin
            BlendColor := ARGBBlendByMode(FColor, BackColor, FPressure, FBlendMode);

            if not FLockTransparent then
            begin
              ba := BlendColor shr 24 and $FF;
            end;
          end
          else
          begin
            BlendColor := RGBBlendByMode(FColor, BackColor, FPressure, FBlendMode);
          end;

          br := BackColor shr 16 and $FF;
          bg := BackColor shr  8 and $FF;
          bb := BackColor        and $FF;

          if csRed in ChannelSet then
          begin
            br := BlendColor shr 16 and $FF;
          end;

          if csGreen in ChannelSet then
          begin
            bg := BlendColor shr 8 and $FF;
          end;

          if csBlue in ChannelSet then
          begin
            bb := BlendColor and $FF;
          end;

          Dest.PixelS[CurPaintX, CurPaintY] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
        end;
      end
      else
      begin
        if not FLockTransparent then
        begin
          if  (csRed   in ChannelSet)
          and (csGreen in ChannelSet)
          and (csBlue  in ChannelSet) then
          begin
            Dest.PixelS[CurPaintX, CurPaintY] := (FPressure shl 24) or (FColor and $FFFFFF);
          end;
        end;
      end;

      // move the data pointer to next
      Inc(FJetArrayIndex);

      if FJetArrayIndex > High(FJetArray) then
      begin
        FJetArrayIndex := 0;
      end;
    end;
  end;
end;

function TgmJetGun.GetJetArea(const X, Y: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if FRadius > 0 then
  begin
    Result.Left   := X - FRadius;
    Result.Top    := Y - FRadius;
    Result.Right  := X + FRadius + 2;
    Result.Bottom := Y + FRadius + 2;
  end;
end;

// Dynamics process
procedure TgmJetGun.SetDynamicRadius(const ARadius: Integer;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FRadiusDynamicState <> ADynamicState then
    FRadiusDynamicState := ADynamicState;

  if FRadiusDynamicState = bdsFade then
  begin
    if FOriginalRadius <> ARadius then
      FOriginalRadius := ARadius;

    if FRadiusDynamicCurrentSteps <> Steps then
      FRadiusDynamicCurrentSteps := Steps;

    if FRadiusDynamicTotalSteps <> Steps then
      FRadiusDynamicTotalSteps := Steps;
  end;
end;

procedure TgmJetGun.SetDynamicPressure(const Pressure: Byte;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FPressureDynamicState <> ADynamicState then
    FPressureDynamicState := ADynamicState;

  if FPressureDynamicState = bdsFade then
  begin
    if FOriginalPressure <> Pressure then
      FOriginalPressure := Pressure;

    if FPressureDynamicCurrentSteps <> Steps then
      FPressureDynamicCurrentSteps := Steps;

    if FPressureDynamicTotalSteps <> Steps then
      FPressureDynamicTotalSteps := Steps;
  end;
end; 

procedure TgmJetGun.SetDynamicColor(const Color1, Color2: TColor32;
  const ADynamicState: TgmBrushDynamicState; const Steps: Integer);
begin
  if FColorDynamicState <> ADynamicState then
  begin
    FColorDynamicState := ADynamicState;
  end;

  if FColorDynamicState = bdsFade then
  begin
    if FStartColor <> Color1 then
    begin
      FStartColor := Color1;
    end;

    if FEndColor <> Color2 then
    begin
      FEndColor := Color2;
    end;

    if FColorDynamicCurrentSteps <> Steps then
    begin
      FColorDynamicCurrentSteps := Steps;
    end;
    
    if FColorDynamicTotalSteps <> Steps then
    begin
      FColorDynamicTotalSteps := Steps;
    end;
  end;
end; 

//-- TgmStroke -----------------------------------------------------------------

constructor TgmStroke.Create;
begin
  inherited Create;
  
  FStrokeBitmap := TBitmap32.Create;
  FHalfWidth    := 0;
  FHalfHeight   := 0;
  FRadius       := 0;
  FSelected     := False;
end;

destructor TgmStroke.Destroy;
begin
  FStrokeBitmap.Free;
  inherited Destroy;
end;

procedure TgmStroke.GetPaintingBrushSize;
begin
  FHalfWidth  := Trunc(FStrokeBitmap.Width / 2);
  FHalfHeight := Trunc(FStrokeBitmap.Height / 2);
end;

procedure TgmStroke.GetPaintingBrushRadius;
begin
  FRadius := MaxIntValue([FHalfWidth, FHalfHeight]);
end;

//-- TgmStrokeList -------------------------------------------------------------

constructor TgmStrokeList.Create;
begin
  inherited Create;
  
  FRowCount             := 0;
  FColumnCount          := 0;
  FSelectedStroke       := nil;
  FSelectedIndex        := -1;
  FStrokeStage          := TBitmap32.Create;
  FStrokeStage.Width    := 192;
  FThumbnailSizeMode    := tsmSmall;
  FThumbnailSize        := SMALL_THUMBNAIL_SIZE;
  FFileName             := '';
  FOutputMsg            := '';
  FBrushMark            := 'GraphicsMagicBrushes';
  FModified             := False;
  FUsingInternalStrokes := True;
end;

destructor TgmStrokeList.Destroy;
begin
  FSelectedStroke.Free;
  FStrokeStage.Free;
  DeleteAllBrushStrokes;
  inherited Destroy;
end;

procedure TgmStrokeList.LoadInternalBrushesToList;
var
  BrushStroke: TgmStroke;
  i          : Integer;
begin
  DeleteAllBrushStrokes;

  GMDataModule := TGMDataModule.Create(nil);
  try
    for i := 0 to INTERNAL_STROKE_COUNT - 1 do
    begin
      BrushStroke := TgmStroke.Create;

      // read brush name
      BrushStroke.Name := INTERNAL_STROKE_NAME[i];

      // read mask bitmap
      BrushStroke.StrokeBitmap.Assign(GMDataModule.bmp32lstBrushStrokes.Bitmap[i]);
      BrushStroke.GetPaintingBrushSize;
      BrushStroke.GetPaintingBrushRadius;
      
      // add brush to list
      Self.Add(BrushStroke);
    end;
  finally
    FreeAndNil(GMDataModule);
  end;
  
  FUsingInternalStrokes := True;
  FModified             := False;
  FFileName             := '';
end;

function TgmStrokeList.LoadFromFile(const AFileName: string): Boolean;
var
  LInputStream : TMemoryStream;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    // if the external file is not exists then loading internal strokes
    LoadInternalBrushesToList;
    Exit;
  end;

  // load data in...
  LInputStream := TMemoryStream.Create;
  try

    try
      LInputStream.LoadFromFile(AFileName);
      LInputStream.Position := 0;

      Result := LoadFromStream(LInputStream);

      if Result then
      begin
        FUsingInternalStrokes := False;
        FModified             := False;
        FFileName             := AFileName;
      end;

    except
      // if any error occurs at loading external strokes then load the internal strokes
      LoadInternalBrushesToList;
      FOutputMsg := 'Cannot open the Brush Stroke file.';
    end;
    
  finally
    LInputStream.Free;
  end;
end;

function TgmStrokeList.LoadOldStrokes(const AStream: TStream): Boolean;
type
  TOldStrokeMark = record
    Mark : string[40];
  end;

  TOldStrokeName = record
    Name : string[40];
  end;

  TOldStrokeCount = record
    Count : string[2];
  end;

const
  OLD_STROKE_MARK = 'GraphicsMagicBrushes';

var
  LMark        : TOldStrokeMark;
  LStrokeCount : TOldStrokeCount;
  LStrokeName  : TOldStrokeName;
  LCount       : Integer;
  LSize        : Integer;
  i            : Integer;
  LStroke      : TgmStroke;
  LBmpStream   : TMemoryStream;
  LTempBmp     : TBitmap;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin

    try
      // read the mark
      AStream.Read( LMark, SizeOf(TOldStrokeMark) );

      if LMark.Mark <> OLD_STROKE_MARK then
      begin
        Exit;
      end;

      // read brush stroke count
      AStream.Read( LStrokeCount, SizeOf(TOldStrokeCount) );
      LCount := StrToInt(LStrokeCount.Count);

      if LCount > MAX_BRUSH_COUNT then
      begin
        LCount := MAX_BRUSH_COUNT;
      end;

      if LCount > 0 then
      begin
        DeleteAllBrushStrokes;

        LBmpStream := TMemoryStream.Create;
        LTempBmp   := TBitmap.Create;
        try
          for i := 1 to LCount do
          begin
            LStroke := TgmStroke.Create;

            // read stroke name
            AStream.Read( LStrokeName, SizeOf(TOldStrokeName) );
            LStroke.Name := LStrokeName.Name;

            // read mask bitmap
            AStream.ReadBuffer( LSize, SizeOf(TBitmap) );

            LBmpStream.CopyFrom(AStream, LSize);
            LBmpStream.Position := 0;

            LTempBmp.LoadFromStream(LBmpStream);
            LTempBmp.PixelFormat := pf24bit;

            LStroke.FStrokeBitmap.Assign(LTempBmp);
            LStroke.GetPaintingBrushSize;
            LStroke.GetPaintingBrushRadius;

            LBmpStream.Clear;

            // add stroke to list
            Self.Add(LStroke);
          end;

        finally
          LTempBmp.Free;
          LBmpStream.Free;
        end;

        Result := True;
      end;

    except

    end;
  end;
end;

function TgmStrokeList.LoadFromStream(const AStream: TStream): Boolean;
var
  LFileHeader       : TgmStrokeFileHeader;
  LStrokeInfoHeader : TgmStrokeInfoHeader;
  LStroke           : TgmStroke;
  LStrokeCount      : Integer;
  i, j              : Integer;
  LStreamPosition   : Int64;
  LByteMap          : TByteMap;
  LByteBits         : PByte;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    LStreamPosition := AStream.Position;

    // try to load old stroke first
    Result := LoadOldStrokes(AStream);

    if not Result then
    begin
      AStream.Position := LStreamPosition;

      // read file header
      AStream.Read(LFileHeader, SizeOf(TgmStrokeFileHeader));

      // check for file ID
      if LFileHeader.FileID <> STROKE_FILE_ID then
      begin
        FOutputMsg := 'This file is not a GraphicsMaigc Brush Stroke file.';
        Exit;
      end;

      // check for file version
      if LFileHeader.FileVersion <> STROKE_FILE_VERSION then
      begin
        FOutputMsg := 'This Brush Stroke file version is not supported by GraphicsMaigc at current version.';
        Exit;
      end;

      // check for number of stroke in the file
      if LFileHeader.StrokeCount <= 0 then
      begin
        FOutputMsg := 'There is no any Brush strokes in this file.';
        Exit;
      end;

      // load in strokes...
      LStrokeCount := LFileHeader.StrokeCount;

      if LStrokeCount > MAX_BRUSH_COUNT then
      begin
        LStrokeCount := MAX_BRUSH_COUNT;
      end;

      DeleteAllBrushStrokes;

      LByteMap := TByteMap.Create;
      try
        for i := 1 to LStrokeCount do
        begin
          // read in the stroke info header
          AStream.Read(LStrokeInfoHeader, SizeOf(TgmStrokeInfoHeader));

          LStroke      := TgmStroke.Create;
          LStroke.Name := LStrokeInfoHeader.Name;

          // read in the stroke pixels
          LByteMap.SetSize(LStrokeInfoHeader.Width, LStrokeInfoHeader.Height);
          LStroke.StrokeBitmap.SetSize(LStrokeInfoHeader.Width, LStrokeInfoHeader.Height);

          LByteBits := @LByteMap.Bits[0];
          for j := 1 to LByteMap.Height do
          begin
            AStream.Read(LByteBits^, LByteMap.Width);
            Inc(LByteBits, LByteMap.Width);
          end;

          LByteMap.WriteTo(LStroke.StrokeBitmap, ctUniformRGB);

          LStroke.GetPaintingBrushSize;
          LStroke.GetPaintingBrushRadius;

          // add stroke to list
          Self.Add(LStroke);
        end;
      finally
        LByteMap.Free;
      end;

      Result := True;
    end;
  end;
end;

procedure TgmStrokeList.SaveToFile(const AFileName: string);
var
  LOutputStream : TMemoryStream;
begin
  if (AFileName <> '') and (Self.Count > 0) then
  begin
    LOutputStream := TMemoryStream.Create;
    try
      SaveToStream(LOutputStream);

      if LOutputStream.Size > 0 then
      begin
        LOutputStream.Position := 0;
        LOutputStream.SaveToFile(AFileName);

        FFileName := AFileName;  // remember the new filename
        FModified := False;
      end;
    finally
      LOutputStream.Free;
    end;
  end;
end;

procedure TgmStrokeList.SaveToStream(const AStream: TStream);
var
  LFileHeader       : TgmStrokeFileHeader;
  LStrokeInfoHeader : TgmStrokeInfoHeader;
  LStroke           : TgmStroke;
  LByteMap          : TByteMap;
  LByteBits         : PByte;
  i, j              : Integer;
begin
  if Assigned(AStream) then
  begin
    // write in file header
    LFileHeader.FileID      := STROKE_FILE_ID;
    LFileHeader.FileVersion := STROKE_FILE_VERSION;
    LFileHeader.StrokeCount := Self.Count;

    AStream.Write(LFileHeader, SizeOf(TgmStrokeFileHeader));

    LByteMap := TByteMap.Create;
    try
      // write data of each stroke to the stream
      for i := 0 to (Self.Count - 1) do
      begin
        LStroke := TgmStroke(Self.Items[i]);

        // write the info of stroke data to the stream
        LStrokeInfoHeader.Width  := LStroke.StrokeBitmap.Width;
        LStrokeInfoHeader.Height := LStroke.StrokeBitmap.Height;
        LStrokeInfoHeader.Name   := LStroke.Name;

        // write in stroke info header
        AStream.Write(LStrokeInfoHeader, SizeOf(TgmStrokeInfoHeader));

        // write in stroke pixels data
        LByteMap.SetSize(LStroke.StrokeBitmap.Width, LStroke.StrokeBitmap.Height);
        LByteMap.ReadFrom(LStroke.StrokeBitmap, ctUniformRGB);

        LByteBits := @LByteMap.Bits[0];
        for j := 1 to LByteMap.Height do
        begin
          AStream.Write(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
      end;
      
    finally
      LByteMap.Free;
    end;
  end;
end;

procedure TgmStrokeList.DeleteAllBrushStrokes;
var
  i          : Integer;
  BrushStroke: TgmStroke;
begin
  if Self.Count > 0 then
  begin
    for i := Self.Count - 1 downto 0 do
    begin
      BrushStroke := TgmStroke(Self.Items[i]);
      BrushStroke.Free;
    end;
  end;
  
  Self.Clear;
  FSelectedStroke := nil;
  FSelectedIndex  := -1;
end;

procedure TgmStrokeList.DeleteSelectedBrushStroke;
begin
  // delete current brush stroke and select the last one
  if Assigned(FSelectedStroke) then
  begin
    FreeAndNil(FSelectedStroke);
    Self.Delete(FSelectedIndex);
    FSelectedStroke            := Self.Items[Self.Count - 1];
    FSelectedStroke.IsSelected := True;
    FSelectedIndex             := Self.Count - 1;
    FModified                  := True;
  end;
end;

procedure TgmStrokeList.ChangeStrokeThumbnailSize(
  const AMode: TgmThumbnailSizeMode);
begin
  FThumbnailSizeMode := AMode;
  
  case FThumbnailSizeMode of
    tsmSmall:
      begin
        FThumbnailSize := SMALL_THUMBNAIL_SIZE;
      end;
      
    tsmLarge:
      begin
        FThumbnailSize := LARGE_THUMBNAIL_SIZE;
      end;
  end;

  GetColumnCount;
  GetRowCount;
  ChangeStrokeStageHeight;
end;

procedure TgmStrokeList.GetColumnCount;
begin
  FColumnCount := FStrokeStage.Width div FThumbnailSize;
end;

procedure TgmStrokeList.GetRowCount;
begin
  if (Self.Count mod FColumnCount) > 0 then
  begin
    FRowCount := Trunc(Self.Count / FColumnCount) + 1;
  end
  else
  begin
    FRowCount := Self.Count div FColumnCount;
  end;
end;

procedure TgmStrokeList.ChangeStrokeStageHeight;
begin
  FStrokeStage.Height := FRowCount * FThumbnailSize;
end;

procedure TgmStrokeList.DrawStrokeStage(const AShowStage: TCustomImage32);
var
  i, j, LIndex    : Integer;
  LIPixel, LJPixel: Integer;
  LCenterPoint    : TPoint;
  LStroke         : TgmStroke;
  LScaledBitmap   : TBitmap32;
begin
  ChangeStrokeThumbnailSize(FThumbnailSizeMode);
  
  LScaledBitmap := TBitmap32.Create;
  try
    FStrokeStage.Clear( Color32(clBtnFace) );

    for LIndex := 0 to (Self.Count - 1) do
    begin
      i := LIndex mod FColumnCount; // column
      j := LIndex div FColumnCount; // row

      LIPixel := i * FThumbnailSize;
      LJPixel := j * FThumbnailSize;

      // get each thumbnail's center point
      LCenterPoint := Point(LIPixel + FThumbnailSize div 2,
                            LJPixel + FThumbnailSize div 2);

      LStroke := TgmStroke(Self.Items[LIndex]);

      GetScaledBitmap(LStroke.StrokeBitmap, LScaledBitmap,
                      FThumbnailSize, FThumbnailSize);

      // delete the white parts
      MakeStrokeAgainstColor( LScaledBitmap, Color32(clBtnFace) );

      FStrokeStage.Draw(LCenterPoint.X - LScaledBitmap.Width div 2,
                        LCenterPoint.Y - LScaledBitmap.Height div 2,
                        LScaledBitmap);

      if LStroke.IsSelected then
      begin
        FStrokeStage.FrameRectS(LIPixel, LJPixel,
                                LIPixel + FThumbnailSize,
                                LJPixel + FThumbnailSize,
                                clRed32);
      end
      else
      begin
        FStrokeStage.FrameRectS(LIPixel, LJPixel,
                                LIPixel + FThumbnailSize,
                                LJPixel + FThumbnailSize,
                                clBlack32);
      end;
    end;

    AShowStage.Bitmap.Assign(FStrokeStage);
  finally
    LScaledBitmap.Free;
  end;
end; 

procedure TgmStrokeList.DrawStrokeBorder(const AShowStage: TCustomImage32);
var
  i, j, LIndex    : Integer;
  LIPixel, LJPixel: Integer;
  LStroke         : TgmStroke;
begin
  if Self.Count > 0 then
  begin
    for LIndex := 0 to (Self.Count - 1) do
    begin
      i := LIndex mod FColumnCount; // column
      j := LIndex div FColumnCount; // row

      LIPixel := i * FThumbnailSize;
      LJPixel := j * FThumbnailSize;

      LStroke := TgmStroke(Self.Items[LIndex]);

      if LStroke.IsSelected then
      begin
        FStrokeStage.FrameRectS(LIPixel, LJPixel,
                                LIPixel + FThumbnailSize,
                                LJPixel + FThumbnailSize,
                                clRed32);
      end
      else
      begin
        FStrokeStage.FrameRectS(LIPixel, LJPixel,
                                LIPixel + FThumbnailSize,
                                LJPixel + FThumbnailSize,
                                clBlack32);
      end;
    end;
  end;
  
  AShowStage.Bitmap.Assign(FStrokeStage);
end;

function TgmStrokeList.GetStrokeInfo(const X, Y: Integer): string;
var
  Index      : Integer;
  BrushStroke: TgmStroke;
begin
  Result := '';

  if Self.Count > 0 then
  begin
    Index := GetStrokeIndex(X, Y);
    
    if (Index >= 0) and (Index < Self.Count) then
    begin
      BrushStroke := Self.Items[Index];
      Result      := BrushStroke.Name + '(Radius: ' + IntToStr(BrushStroke.Radius) +')';
    end;
  end;
end;

function TgmStrokeList.SelectStrokeByIndex(const Index: Integer): Boolean;
var
  i          : Integer;
  BrushStroke: TgmStroke;
begin
  Result := False;
  
  if (Self.Count > 0) then
  begin
    for i := 0 to Self.Count - 1 do
    begin
      BrushStroke            := Self.Items[i];
      BrushStroke.IsSelected := False;
    end;

    if (Index >= 0) and (Index < Self.Count) then
    begin
      FSelectedStroke := Self.Items[Index];
      FSelectedIndex  := Index;
    end
    else
    begin
      FSelectedStroke := Self.Items[0];
      FSelectedIndex  := 0;
    end;

    FSelectedStroke.IsSelected := True;
    Result                     := True;
  end;
end;

function TgmStrokeList.GetCurrentStrokeInfo: string;
var
  BrushStroke: TgmStroke;
begin
  BrushStroke := Self.Items[FSelectedIndex];
  Result      := BrushStroke.Name + '(Radius: ' + IntToStr(BrushStroke.Radius) +')';
end;

function TgmStrokeList.GetStrokeIndex(const X, Y: Integer): Integer;
var
  Row, Column: Integer;
begin
  Row := Trunc(Y / FThumbnailSize);

  if (X mod FThumbnailSize) <> 0 then
  begin
    Column := Trunc(X / FThumbnailSize) + 1;
  end
  else
  begin
    Column := X div FThumbnailSize;
  end;

  Result := Row * FColumnCount + Column - 1;
end; 

end.
