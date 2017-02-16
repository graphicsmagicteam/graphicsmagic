unit gmLayerIO;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 *
 * The Original Code is gmLayerIO.pas.
 *
 * The Initial Developer of the Original Code and this unit is
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2017/01/23

interface

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}

uses
{ Delphi }
  Classes, ComCtrls,
{ GraphicsMagicLib }
  gmBrightContrastLayer,
  gmChannelMixerLayer,
  gmColorBalanceLayer,
  gmCurvesLayer,
  gmGradientFillLayer,
  gmGradientMapLayer,
  gmHueSaturationLayer,
  gmInvertLayer,
  gmLayers,
  gmLevelsLayer,
  gmPatternLayer,
  gmPosterizeLayer,
  gmRichTextLayer,
  gmShapeRegionLayer,
  gmSolidColorLayer,
  gmThresholdLayer,
  gmVectorLayer;


const
  LAYER_FILE_VER = 4;

type
  // Mark the layer as Background or Transparent, etc.
  // Only be used for load old versions of *gmd files.
  TgmLayerFeature = (lfNone,
                     lfBackground,
                     lfTransparent,
                     lfSolidColor,
                     lfBrightContrast,
                     lfCurves,
                     lfLevels,
                     lfColorBalance,
                     lfHLSOrHSV,
                     lfInvert,
                     lfThreshold,
                     lfPosterize,
                     lfPattern,
                     lfFigure,
                     lfGradientMap,
                     lfGradientFill,
                     lfShapeRegion,
                     lfRichText,
                     lfChannelMixer);

  // for new structure of layer system
  TgmLayerType = (ltUnknown,
                  ltNormal,
                  ltSolidColor,
                  ltBrightContrast,
                  ltCurves,
                  ltLevels,
                  ltColorBalance,
                  ltHueSaturation,
                  ltInvert,
                  ltThreshold,
                  ltPosterize,
                  ltPattern,
                  ltVector,
                  ltGradientMap,
                  ltGradientFill,
                  ltShapeRegion,
                  ltRichText,
                  ltChannelMixer);

  // used to save the layers info to '.gmd' file
  TgmLayerHeaderVer1 = record
    LayerName        : ShortString;
    LayerFeature     : Cardinal;  // the type of the layer
    BlendModeIndex   : Cardinal;
    MasterAlpha      : Byte;
    Selected         : Boolean;   // if the layer is currently selected
    Visible          : Boolean;   // if the layer is visible
    LockTransparency : Boolean;   // if protected the alpha channel of each pixel on a layer
    Duplicated       : Boolean;   // if the layer is a copy of an another
    HasMask          : Boolean;   // if the layer has a mask
    MaskLinked       : Boolean;   // if the layer is linked to it's mask
  end;

  TgmLayerHeaderVer2 = record
    LayerName        : ShortString;
    LayerType        : Cardinal;  // the type of the layer
    BlendModeIndex   : Cardinal;
    Opacity          : Byte;      // 0..255
    Selected         : Boolean;   // if the layer is currently selected
    Visible          : Boolean;   // if the layer is visible
    LockTransparency : Boolean;   // if protected the alpha channel of each pixel on a layer
    Duplicated       : Boolean;   // if the layer is a copy of an another
    MaskEnabled      : Boolean;   // if the layer's mask is enabled
    MaskLinked       : Boolean;   // if the layer is linked with it's mask
  end;


  { Layers Writer }

  TgmLayersWriter = class(TObject)
  private
    FLayerList : TgmLayerList;  // pointer to a layer list

    function GetLayerType(ALayer: TgmCustomLayer): TgmLayerType;

    procedure SaveLayerHeader(ALayer: TgmCustomLayer; AStream: TStream);
    procedure SaveLayerByType(ALayer: TgmCustomLayer; AStream: TStream);
    procedure SaveMask(ALayer: TgmCustomLayer; AStream: TStream);
    
    procedure SaveBrightContrastLayer(ALayer: TgmBrightContrastLayer; AStream: TStream);
    procedure SaveChannelMixerLayer(ALayer: TgmChannelMixerLayer; AStream: TStream);
    procedure SaveColorBalanceLayer(ALayer: TgmColorBalanceLayer; AStream: TStream);
    procedure SaveCurvesLayer(ALayer: TgmCurvesLayer; AStream: TStream);
    procedure SaveGradientFillLayer(ALayer: TgmGradientFillLayer; AStream: TStream);
    procedure SaveGradientMapLayer(ALayer: TgmGradientMapLayer; AStream: TStream);
    procedure SaveHueSaturationLayer(ALayer: TgmHueSaturationLayer; AStream: TStream);
    procedure SaveInvertLayer(ALayer: TgmInvertLayer; AStream: TStream);
    procedure SaveLevelsLayer(ALayer: TgmLevelsLayer; AStream: TStream);
    procedure SaveNormalLayer(ALayer: TgmNormalLayer; AStream: TStream);
    procedure SavePatternLayer(ALayer: TgmPatternLayer; AStream: TStream);
    procedure SavePosterizeLayer(ALayer: TgmPosterizeLayer; AStream: TStream);
    procedure SaveRichTextLayer(ALayer: TgmRichTextLayer; AStream: TStream);
    procedure SaveShapeRegionLayer(ALayer: TgmShapeRegionLayer; AStream: TStream);
    procedure SaveSolidColorLayer(ALayer: TgmSolidColorLayer; AStream: TStream);
    procedure SaveThresholdLayer(ALayer: TgmThresholdLayer; AStream: TStream);
    procedure SaveVectorLayer(ALayer: TgmVectorLayer; AStream: TStream);
  public
    constructor Create(ALayerList: TgmLayerList);

    procedure SaveToStream(AStream: TStream);
  end;

// load layers from a '*.gmd' file that is already loaded in a stream
function LoadLayersFromStream(
  AStream: TStream; ALayerList: TgmLayerList; ATextEditor: TRichEdit;
  const AFileVersion, ALayerCount, ALayerWidth, ALayerHeight: Cardinal;
  const AClearOldLayers: Boolean = True): Boolean;

implementation

uses
{ Delphi }
  Graphics,
{ Graphics32 }
  GR32,
  GR32_OrdinalMaps,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Package Lib }
  gmGradient,
  gmGradient_rwUnversioned,  // for loading old gradient data in
  gmGradient_rwVer1,         // for loading new gradient data in
  gmGradientRender,
{ GraphicsMagic Lib }
  gmAlphaFuncs,
  gmFigures,
  gmGimpHistogram,
  gmShapes,
  gmTypes;


type
  { TgmCustomLayerLoader }

  // The layer loader is used to load layers from a given .gmd file.

  TgmCustomLayerLoader = class(TObject)
  protected
    FTextEditor  : TRichEdit;     // pointer to an external text editor for Rich Text Layer
    FFileStream  : TStream;       // pointer to a stream
    FLayerList   : TgmLayerList;  // pointer to a layer list for associated the layer with the list
    FLayerWidth  : Cardinal;
    FLayerHeight : Cardinal;
  public
    constructor Create(AStream: TStream; ALayerList: TgmLayerList;
      ATextEditor: TRichEdit);
      
    destructor Destroy; override;

    function GetLayer: TgmCustomLayer; virtual; abstract;

    property LayerWidth  : Cardinal read FLayerWidth  write FLayerWidth;
    property LayerHeight : Cardinal read FLayerHeight write FLayerHeight;
  end;


  { TgmLayerLoader1 }
  
  // layer loader with version 1
  TgmLayerLoader1 = class(TgmCustomLayerLoader)
  protected
    function GetBackgroundLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetTransparentLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetSolidColorLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetColorBalanceLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetBrightContrastLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetHLSLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetInvertLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetThresholdLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetPosterizeLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetLevelsLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetCurvesLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetGradientMapLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetGradientFillLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetPatternLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetVectorLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetShapeRegionLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
    function GetTextLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
  public
    function GetLayer: TgmCustomLayer; override;
  end;


  { TgmLayerLoader2 }

  TgmLayerLoader2 = class(TgmLayerLoader1)
  protected
    function GetChannelMixerLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; virtual;
  public
    function GetLayer: TgmCustomLayer; override;
  end;


  { TgmLayerLoader3 }

  TgmLayerLoader3 = class(TgmLayerLoader2)
  protected
    function GetGradientFillLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; override;
    function GetGradientMapLayer(const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer; override;
  public
  end;
  

  { TgmLayerLoader4 }

  TgmLayerLoader4 = class(TgmCustomLayerLoader)
  protected
    function GetBrightContrastLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetChannelMixerLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetColorBalanceLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetCurvesLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetGradientFillLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetGradientMapLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetHueSaturationLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetInvertLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetLevelsLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetNormalLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetPatternLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetPosterizeLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetRichTextLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetShapeRegionLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetSolidColorLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetThresholdLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;
    function GetVectorLayer(const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer; virtual;

    procedure LoadMaskForLayer(ALayer: TgmCustomLayer);

    procedure SetLayerCommonProperties(ALayer: TgmCustomLayer;
      const ALayerHeader: TgmLayerHeaderVer2);
  public
    function GetLayer: TgmCustomLayer; override;
  end;


// load layers from a '*.gmd' file that is already loaded in a stream
function LoadLayersFromStream(
  AStream: TStream; ALayerList: TgmLayerList; ATextEditor: TRichEdit;
  const AFileVersion, ALayerCount, ALayerWidth, ALayerHeight: Cardinal;
  const AClearOldLayers: Boolean = True): Boolean;
var
  i            : Integer;
  LLayer       : TgmCustomLayer;
  LLayerLoader : TgmCustomLayerLoader;
begin
  Result       := True;
  LLayerLoader := nil;

  if ( not Assigned(AStream) ) or
     ( not Assigned(ALayerList) ) then
  begin
    Result := False;
    Exit;
  end;

  if (AFileVersion = 0) or
     (ALayerCount  = 0) or
     (ALayerWidth  = 0) or
     (ALayerHeight = 0) then
  begin
    Result := False;
    Exit;
  end;

  if AClearOldLayers then
  begin
    ALayerList.Clear();
  end;

  case AFileVersion of
    1:
      begin
        LLayerLoader := TgmLayerLoader1.Create(AStream, ALayerList, ATextEditor);
      end;
      
    2:
      begin
        LLayerLoader := TgmLayerLoader2.Create(AStream, ALayerList, ATextEditor);
      end;

    3:
      begin
        LLayerLoader := TgmLayerLoader3.Create(AStream, ALayerList, ATextEditor);
      end;

    4:
      begin
        LLayerLoader := TgmLayerLoader4.Create(AStream, ALayerList, ATextEditor);
      end;
  end;

  if Assigned(LLayerLoader) then
  begin
    try
      LLayerLoader.LayerWidth  := ALayerWidth;
      LLayerLoader.LayerHeight := ALayerHeight;

      for i := 0 to (ALayerCount - 1) do
      begin
        LLayer := LLayerLoader.GetLayer();

        // Add layer to list, note that, we just add it to layer
        // with SimpleAdd() of layer list.
        if Assigned(LLayer) then
        begin
          ALayerList.SimplyAdd(LLayer);
          
          // Activate the selected layer in the list.
          if LLayer.IsSelected then
          begin
            ALayerList.SelectLayer(ALayerList.Count - 1);
          end;
        end;
      end;

    finally
      LLayerLoader.Free();
    end;
  end
  else
  begin
    Result := False;
  end;
end;

{ TgmCusotmLayerLoader }

constructor TgmCustomLayerLoader.Create(AStream: TStream;
  ALayerList: TgmLayerList; ATextEditor: TRichEdit);
begin
  inherited Create();

  FFileStream  := AStream;
  FLayerList   := ALayerList;
  FLayerWidth  := 0;
  FLayerHeight := 0;
  FTextEditor  := ATextEditor; // pointer to an external text editor for Rich Text Layer);
end;

destructor TgmCustomLayerLoader.Destroy;
begin
  FFileStream := nil;
  FLayerList  := nil;

  inherited;
end;

{ TgmLayerLoader1 }

function TgmLayerLoader1.GetBackgroundLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmNormalLayer;
  LMask         : TBitmap32;
  i, LRowStride : Cardinal;
  LPixelBits    : PColor32;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
begin
  // create background layer
  LNewLayer := TgmNormalLayer.Create(FLayerList, FLayerWidth, FLayerHeight, 0, True);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  LPixelBits := @LNewLayer.LayerBitmap.Bits[0];
  LRowStride := FLayerWidth * SizeOf(TColor32);

  // load image data from stream
  for i := 0 to (FLayerHeight - 1) do
  begin
    FFileStream.Read(LPixelBits^, LRowStride);
    Inc(LPixelBits, FLayerWidth);
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();

      // Read in the alpha data of the layer that without mask applied.
      // With new layer structure, there is no longer need such data,
      // but it is hurtless to load those alpha data in and save them
      // to the background layer.
      LMask := TBitmap32.Create();
      try
        LMask.SetSizeFrom(LNewLayer.LayerBitmap);

        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (FLayerHeight - 1) do
        begin
          FFileStream.Read(LByteBits^, FLayerWidth);
          Inc(LByteBits, FLayerWidth);
        end;

        LByteMap.WriteTo(LMask, ctUniformRGB);
        ReplaceAlphaChannelWithMask(LNewLayer.LayerBitmap, LMask);
      finally
        LMask.Free();
      end;
      
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLayerThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader1.GetBrightContrastLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmBrightContrastLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LIntegerValue : Integer;
  LBooleanValue : Boolean;
begin
  // create Brightness/Contrast layer
  LNewLayer := TgmBrightContrastLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the bright/contrast layer, load settings from stream
  FFileStream.Read(LIntegerValue, 4);
  LNewLayer.BrightAmount := LIntegerValue;

  FFileStream.Read(LIntegerValue, 4);
  LNewLayer.ContrastAmount := LIntegerValue;

  // Read in the boolean value for preview settings,
  // which is discarded for TgmBrightContrastLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetColorBalanceLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmColorBalanceLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LBooleanValue : Boolean;
begin
  // create color balance layer
  LNewLayer := TgmColorBalanceLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the color balance layer, load settings from stream
  LNewLayer.ColorBalance.LoadFromStream(FFileStream);

  // Read in the boolean value for preview settings,
  // which is discarded for TgmColorBalanceLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetCurvesLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmCurvesLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LBooleanValue : Boolean;
begin
  // create a Curves layer
  LNewLayer := TgmCurvesLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // load the Curves data from stream
  LNewLayer.CurvesTool.LoadFromStream(FFileStream);

  // Read in the boolean value for preview settings,
  // which is discarded for TgmCurvesLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LNewLayer.LayerBitmap.Height - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetGradientFillLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer       : TgmGradientFillLayer;
  LLoadedGradient : TgmGradientItem;
  i, LIntValue    : Integer;
  LByteMap        : TByteMap;
  LByteBits       : PByte;
  LSettings       : TgmGradientFillSettings;
  LGradientReader : TgmOldReader;
begin
  // create a Gradient Fill layer
  LNewLayer := TgmGradientFillLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the GradientFill layer, load settings from stream
  LGradientReader := TgmOldReader.Create();
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient := LLoadedGradient;
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  FFileStream.Read(LIntValue, 4);
  LSettings.Style := TgmGradientRenderMode(LIntValue);

  FFileStream.Read(LSettings.Angle, 4);
  FFileStream.Read(LSettings.Scale, SizeOf(LSettings.Scale));
  FFileStream.Read(LSettings.TranslateX, 4);
  FFileStream.Read(LSettings.TranslateY, 4);
  FFileStream.Read(LSettings.Reversed, 1);

  FFileStream.Read(LSettings.StartPoint.X, 4);
  FFileStream.Read(LSettings.StartPoint.Y, 4);
  FFileStream.Read(LSettings.EndPoint.X, 4);
  FFileStream.Read(LSettings.EndPoint.Y, 4);
  FFileStream.Read(LSettings.CenterPoint.X, 4);
  FFileStream.Read(LSettings.CenterPoint.Y, 4);

  FFileStream.Read(LSettings.OriginalCenter.X, 4);
  FFileStream.Read(LSettings.OriginalCenter.Y, 4);
  FFileStream.Read(LSettings.OriginalStart.X, 4);
  FFileStream.Read(LSettings.OriginalStart.Y, 4);
  FFileStream.Read(LSettings.OriginalEnd.X, 4);
  FFileStream.Read(LSettings.OriginalEnd.Y, 4);

  LNewLayer.Setup(LSettings);
  LNewLayer.CalculateGradientCoord();
  LNewLayer.DrawGradientOnLayer();

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLogoThumbnail();
  Result := LNewLayer;
end;

function TgmLayerLoader1.GetGradientMapLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer       : TgmGradientMapLayer;
  LLoadedGradient : TgmGradientItem;
  i               : Cardinal;
  LByteMap        : TByteMap;
  LByteBits       : PByte;
  LBooleanValue   : Boolean;
  LGradientReader : TgmOldReader;
begin
  // create a Gradient Map layer
  LNewLayer := TgmGradientMapLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the GradientMap layer, load settings from stream
  LGradientReader := TgmOldReader.Create();
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient := LLoadedGradient;
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  FFileStream.Read(LBooleanValue, 1);
  LNewLayer.IsReversed := LBooleanValue;

  // Read in the boolean value for preview settings,
  // which is discarded for TgmGradientLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetHLSLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmHueSaturationLayer;
  i, LMode      : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LIntegerValue : Integer;
  LBooleanValue : Boolean;
begin
  // create a Hue/Saturation layer
  LNewLayer := TgmHueSaturationLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the Hue/Saturation layer, load settings from stream
  
  FFileStream.Read(LIntegerValue, 4);
  LNewLayer.Hue := LIntegerValue;

  FFileStream.Read(LIntegerValue, 4);
  LNewLayer.LightValue := LIntegerValue;

  FFileStream.Read(LIntegerValue, 4);
  LNewLayer.Saturation := LIntegerValue;

  FFileStream.Read(LMode, 4);
  LNewLayer.AdjustMode := TgmHueSaturationAdjustMode(LMode);

  // Read in the boolean value for preview settings,
  // which is discarded for TgmHueSaturationLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetInvertLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer : TgmInvertLayer;
  i         : Cardinal;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  // create a Invert layer
  LNewLayer := TgmInvertLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetLayer: TgmCustomLayer;
var
  LLayerHeader : TgmLayerHeaderVer1;
begin
  Result := nil;

  if Assigned(FFileStream) and Assigned(FLayerList) then
  begin
    FFileStream.Read(LLayerHeader, SizeOf(TgmLayerHeaderVer1));

    case TgmLayerFeature(LLayerHeader.LayerFeature) of
      lfBackground:
        begin
          Result := GetBackgroundLayer(LLayerHeader);
        end;
        
      lfTransparent:
        begin
          Result := GetTransparentLayer(LLayerHeader);
        end;

      lfSolidColor:
        begin
          Result := GetSolidColorLayer(LLayerHeader);
        end;

      lfBrightContrast:
        begin
          Result := GetBrightContrastLayer(LLayerHeader);
        end;

      lfCurves:
        begin
          Result := GetCurvesLayer(LLayerHeader);
        end;

      lfLevels:
        begin
          Result := GetLevelsLayer(LLayerHeader);
        end;

      lfColorBalance:
        begin
          Result := GetColorBalanceLayer(LLayerHeader);
        end;

      lfHLSOrHSV:
        begin
          Result := GetHLSLayer(LLayerHeader);
        end;

      lfInvert:
        begin
          Result := GetInvertLayer(LLayerHeader);
        end;

      lfThreshold:
        begin
          Result := GetThresholdLayer(LLayerHeader);
        end;

      lfPosterize:
        begin
          Result := GetPosterizeLayer(LLayerHeader);
        end;

      lfPattern:
        begin
          Result := GetPatternLayer(LLayerHeader);
        end;

      lfFigure:
        begin
          // the figure layer has renamed to vector layer 
          Result := GetVectorLayer(LLayerHeader);
        end;

      lfGradientMap:
        begin
          Result := GetGradientMapLayer(LLayerHeader);
        end;

      lfGradientFill:
        begin
          Result := GetGradientFillLayer(LLayerHeader);
        end;

      lfShapeRegion:
        begin
          Result := GetShapeRegionLayer(LLayerHeader);
        end;

      lfRichText:
        begin
          Result := GetTextLayer(LLayerHeader);
        end;
    end;
  end;
end;

function TgmLayerLoader1.GetLevelsLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmLevelsLayer;
  i, LScale     : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LBooleanValue : Boolean;
begin
  // create a Levels layer
  LNewLayer := TgmLevelsLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // load the Levels data from stream
  LNewLayer.LevelsTool.LoadFromStream(FFileStream);

  FFileStream.Read(LScale, 4);
  LNewLayer.HistogramScale := TgmGimpHistogramScale(LScale);

  // Read in the boolean value for preview settings,
  // which is discarded for TgmLevelsLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetPatternLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer           : TgmPatternLayer;
  i, w, h, LRowStride : Integer;
  LLoadedPattern      : TBitmap32;
  LPixelBits          : PColor32;
  LByteMap            : TByteMap;
  LByteBits           : PByte;
  LDoubleValue        : Double;
begin
  // create a Pattern layer
  LNewLayer := TgmPatternLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the Pattern layer, load settings from stream

  FFileStream.Read(LDoubleValue, SizeOf(LDoubleValue));
  LNewLayer.Scale := LDoubleValue;

  FFileStream.Read(w, 4);
  FFileStream.Read(h, 4);

  LLoadedPattern := TBitmap32.Create();
  try
    LLoadedPattern.SetSize(w, h);

    LRowStride := w * SizeOf(TColor32);
    LPixelBits := @LLoadedPattern.Bits[0];

    for i := 0 to (h - 1) do
    begin
      FFileStream.Read(LPixelBits^, LRowStride);
      Inc(LPixelBits, w);
    end;

    // Setting the PatternBitmap property of the pattern layer
    // will cause the layer to be filled up with the loaded
    // pattern. 
    LNewLayer.PatternBitmap := LLoadedPattern;
  finally
    LLoadedPattern.Free();
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLogoThumbnail();  
  
  Result := LNewLayer;
end;

function TgmLayerLoader1.GetPosterizeLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmPosterizeLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LByteValue    : Byte;
  LBooleanValue : Boolean;
begin
  // create a Posterize layer
  LNewLayer := TgmPosterizeLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the Posterize layer, load settings from stream

  FFileStream.Read(LByteValue, 1);
  LNewLayer.Level := LByteValue;

  // Read in the boolean value for preview settings,
  // which is discarded for TgmPosterizeLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetShapeRegionLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer          : TgmShapeRegionLayer;
  i, LIntValue       : Integer;
  LOutlineListReader : TgmOutlineListReader;
  LByteMap           : TByteMap;
  LByteBits          : PByte;
  LColorValue        : TColor;
  LBooleanValue      : Boolean;
begin
  // create a Shape Region layer
  LNewLayer := TgmShapeRegionLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the shape region layer, load settings from stream ...

  // read color
  FFileStream.Read(LColorValue, 4);
  LNewLayer.RegionColor             := LColorValue;
  LNewLayer.ShapeRegion.RegionColor := LNewLayer.RegionColor;

  // read brush style
  FFileStream.Read(LIntValue, 4);
  LNewLayer.BrushStyle             := TBrushStyle(LIntValue);
  LNewLayer.ShapeRegion.BrushStyle := LNewLayer.BrushStyle;

  FFileStream.Read(LBooleanValue, 1);
  LNewLayer.IsDismissed := LBooleanValue;

  // read the outlines
  LOutlineListReader := TgmOutlineListReader1.Create(LNewLayer.ShapeOutlineList);
  try
    LOutlineListReader.LoadFromStream(FFileStream);
  finally
    LOutlineListReader.Free();
  end;

  LNewLayer.ShapeRegion.AccumRGN := LNewLayer.ShapeOutlineList.GetScaledShapesRegion();

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.DrawRegionOnLayer();
  LNewLayer.UpdateLayerThumbnail();
  LNewLayer.UpdateLogoThumbnail();

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetSolidColorLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmSolidColorLayer;
  i             : Cardinal;
  LFillingColor : TColor;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
begin
  // load the filling color from stream
  FFileStream.Read(LFillingColor, SizeOf(TColor));

  // create solid color layer
  LNewLayer := TgmSolidColorLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.SolidColor         := Color32(LFillingColor);
  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLogoThumbnail();

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetTextLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmRichTextLayer;
  i, LIntValue  : Integer;
  X, Y          : Integer;
  LStreamSize   : Int64;
  LStrValue     : ShortString;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LBooleanValue : Boolean;
begin
  // create a Rich Text layer
  LNewLayer := TgmRichTextLayer.Create(FLayerList, FLayerWidth, FLayerHeight);
  LNewLayer.AssociateTextEditor(FTextEditor);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the Text layer, load settings from stream

  // read the data of a Text layer in from the stream
  FFileStream.Read(LStrValue, SizeOf(LStrValue));
  LNewLayer.TextFileName := LStrValue;

  FFileStream.Read(X, 4);
  FFileStream.Read(Y, 4);
  LNewLayer.BorderStart := Point(X, Y);

  FFileStream.Read(X, 4);
  FFileStream.Read(Y, 4);
  LNewLayer.BorderEnd := Point(X, Y);

  FFileStream.Read(LIntValue, 4);
  LNewLayer.TextLayerState := TgmTextLayerState(LIntValue);

  FFileStream.Read(LBooleanValue, 1);
  LNewLayer.IsEditState := LBooleanValue;

  FFileStream.Read(LBooleanValue, 1);
  LNewLayer.IsTextChanged := LBooleanValue;

  // read in the rich text stream
  FFileStream.Read(LStreamSize, SizeOf(Int64));
  LNewLayer.RichTextStream.Size := LStreamSize;

  if LStreamSize > 0 then
  begin
    LNewLayer.RichTextStream.Position := 0;
    FFileStream.Read(LNewLayer.RichTextStream.Memory^, LStreamSize);

    LNewLayer.RichTextStream.Position := 0;
  end;

  FTextEditor.Lines.LoadFromStream(LNewLayer.RichTextStream);
  LNewLayer.DrawTextOnLayer();

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLogoThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader1.GetThresholdLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmThresholdLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LByteValue    : Byte;
  LBooleanValue : Boolean;
begin
  // create a Threshold layer
  LNewLayer := TgmThresholdLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the Threshold layer, load settings from stream
  FFileStream.Read(LByteValue, 1);
  LNewLayer.Level := LByteValue;

  // Read in the boolean value for preview settings,
  // which is discarded for TgmThresholdLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader1.GetTransparentLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmNormalLayer;
  LMask         : TBitmap32;
  i, LRowStride : Cardinal;
  LPixelBits    : PColor32;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
begin
  // create transparent layer
  LNewLayer := TgmNormalLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  LPixelBits := @LNewLayer.LayerBitmap.Bits[0];
  LRowStride := FLayerWidth * SizeOf(TColor32);

  // load image data from stream
  for i := 0 to (FLayerHeight - 1) do
  begin
    FFileStream.Read(LPixelBits^, LRowStride);
    Inc(LPixelBits, FLayerWidth);
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();

      // Read in the alpha data of the layer that without mask applied.
      // With new layer structure, there is no longer need such data,
      // but it is hurtless to load those alpha data in and save them
      // to the normal layer.
      LMask := TBitmap32.Create();
      try
        LMask.SetSizeFrom(LNewLayer.LayerBitmap);

        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (FLayerHeight - 1) do
        begin
          FFileStream.Read(LByteBits^, FLayerWidth);
          Inc(LByteBits, FLayerWidth);
        end;

        LByteMap.WriteTo(LMask, ctUniformRGB);
        ReplaceAlphaChannelWithMask(LNewLayer.LayerBitmap, LMask);
      finally
        LMask.Free();
      end;

    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLayerThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader1.GetVectorLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer         : TgmVectorLayer;
  LFigureListReader : TgmFigureListReader;
  LMask, LDummyBmp  : TBitmap32;
  i, LRowStride     : Cardinal;
  LPixelBits        : PColor32;
  LByteMap          : TByteMap;
  LByteBits         : PByte;
begin
  // create a Vector layer
  LNewLayer := TgmVectorLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // load image data from stream
  LPixelBits := @LNewLayer.LayerBitmap.Bits[0];
  LRowStride := FLayerWidth * SizeOf(TColor32);
  
  for i := 0 to (FLayerHeight - 1) do
  begin
    FFileStream.Read(LPixelBits^, LRowStride);
    Inc(LPixelBits, FLayerWidth);
  end;

  // read in the figure list data from stream
  LFigureListReader := TgmFigureListReader1.Create(LNewLayer.FigureList);
  try
    LFigureListReader.LoadFromStream(FFileStream);
  finally
    LFigureListReader.Free();
  end;

  // read in the pixels data of bitmap with locked figures from the stream
  // we don't need those data for now, so just read them in for nothing
  if LNewLayer.FigureList.LockedFigureCount > 0 then
  begin
    LDummyBmp := TBitmap32.Create();
    try
      LDummyBmp.SetSize(FLayerWidth, FLayerHeight);

      LPixelBits := @LDummyBmp.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LPixelBits^, LRowStride);
        Inc(LPixelBits, FLayerWidth);
      end;
    finally
      LDummyBmp.Free();
    end;
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();

      // Read in the alpha data of the layer that without mask applied.
      // With new layer structure, there is no longer need such data,
      // but it is hurtless to load those alpha data in and save them
      // to the vector layer.
      LMask := TBitmap32.Create();
      try
        LMask.SetSizeFrom(LNewLayer.LayerBitmap);
        
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (FLayerHeight - 1) do
        begin
          FFileStream.Read(LByteBits^, FLayerWidth);
          Inc(LByteBits, FLayerWidth);
        end;

        LByteMap.WriteTo(LMask, ctUniformRGB);
        ReplaceAlphaChannelWithMask(LNewLayer.LayerBitmap, LMask);
      finally
        LMask.Free();
      end;
      
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.DrawAllFiguresOnLayer();
  LNewLayer.UpdateLayerThumbnail();

  Result := LNewLayer;
end;

{ TgmLayerLoader2 }

function TgmLayerLoader2.GetChannelMixerLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer     : TgmChannelMixerLayer;
  i             : Cardinal;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LBooleanValue : Boolean;
begin
  // create Channel Mixer layer
  LNewLayer := TgmChannelMixerLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the channel mixer layer, load settings from stream
  LNewLayer.ChannelMixer.LoadFromStream(FFileStream);

  // Read in the boolean value for preview settings,
  // which is discarded for TgmChannelMixerLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader2.GetLayer: TgmCustomLayer;
var
  LLayerHeader : TgmLayerHeaderVer1;
begin
  Result := nil;

  if Assigned(FFileStream) and Assigned(FLayerList) then
  begin
    FFileStream.Read(LLayerHeader, SizeOf(TgmLayerHeaderVer1));

    case TgmLayerFeature(LLayerHeader.LayerFeature) of
      lfBackground:
        begin
          Result := GetBackgroundLayer(LLayerHeader);
        end;
        
      lfTransparent:
        begin
          Result := GetTransparentLayer(LLayerHeader);
        end;
        
      lfSolidColor:
        begin
          Result := GetSolidColorLayer(LLayerHeader);
        end;
        
      lfBrightContrast:
        begin
          Result := GetBrightContrastLayer(LLayerHeader);
        end;
        
      lfCurves:
        begin
          Result := GetCurvesLayer(LLayerHeader);
        end;
        
      lfLevels:
        begin
          Result := GetLevelsLayer(LLayerHeader);
        end;
        
      lfColorBalance:
        begin
          Result := GetColorBalanceLayer(LLayerHeader);
        end;
        
      lfHLSOrHSV:
        begin
          Result := GetHLSLayer(LLayerHeader);
        end;
        
      lfInvert:
        begin
          Result := GetInvertLayer(LLayerHeader);
        end;
        
      lfThreshold:
        begin
          Result := GetThresholdLayer(LLayerHeader);
        end;
        
      lfPosterize:
        begin
          Result := GetPosterizeLayer(LLayerHeader);
        end;
        
      lfPattern:
        begin
          Result := GetPatternLayer(LLayerHeader);
        end;
        
      lfFigure:
        begin
          // the figure layer has renamed to vector layer 
          Result := GetVectorLayer(LLayerHeader);
        end;
        
      lfGradientMap:
        begin
          Result := GetGradientMapLayer(LLayerHeader);
        end;
        
      lfGradientFill:
        begin
          Result := GetGradientFillLayer(LLayerHeader);
        end;
        
      lfShapeRegion:
        begin
          Result := GetShapeRegionLayer(LLayerHeader);
        end;
        
      lfRichText:
        begin
          Result := GetTextLayer(LLayerHeader);
        end;
        
      lfChannelMixer:
        begin
          Result := GetChannelMixerLayer(LLayerHeader);
        end;
    end;
  end;
end;

{ TgmLayerLoader3 }

function TgmLayerLoader3.GetGradientFillLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer       : TgmGradientFillLayer;
  LLoadedGradient : TgmGradientItem;
  i, LIntValue    : Integer;
  LColorValue     : TColor;
  LByteMap        : TByteMap;
  LByteBits       : PByte;
  LSettings       : TgmGradientFillSettings;
  LGradientReader : TgmGrdVer1_Reader;
begin
  // create a Gradient Fill layer
  LNewLayer := TgmGradientFillLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the GradientFill layer, load settings from stream
  LGradientReader := TgmGrdVer1_Reader.Create();
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient.Assign(LLoadedGradient);
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  // read Forground/Background color for gradient
  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.ForegroundColor := LColorValue;

  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.BackgroundColor := LColorValue;

  FFileStream.Read(LIntValue, 4);
  LSettings.Style := TgmGradientRenderMode(LIntValue);

  FFileStream.Read(LSettings.Angle, 4);
  FFileStream.Read(LSettings.Scale, SizeOf(LSettings.Scale));
  FFileStream.Read(LSettings.TranslateX, 4);
  FFileStream.Read(LSettings.TranslateY, 4);
  FFileStream.Read(LSettings.Reversed, 1);

  FFileStream.Read(LSettings.StartPoint.X, 4);
  FFileStream.Read(LSettings.StartPoint.Y, 4);
  FFileStream.Read(LSettings.EndPoint.X, 4);
  FFileStream.Read(LSettings.EndPoint.Y, 4);
  FFileStream.Read(LSettings.CenterPoint.X, 4);
  FFileStream.Read(LSettings.CenterPoint.Y, 4);

  FFileStream.Read(LSettings.OriginalCenter.X, 4);
  FFileStream.Read(LSettings.OriginalCenter.Y, 4);
  FFileStream.Read(LSettings.OriginalStart.X, 4);
  FFileStream.Read(LSettings.OriginalStart.Y, 4);
  FFileStream.Read(LSettings.OriginalEnd.X, 4);
  FFileStream.Read(LSettings.OriginalEnd.Y, 4);

  LNewLayer.Setup(LSettings);
  LNewLayer.CalculateGradientCoord();
  LNewLayer.DrawGradientOnLayer();

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  LNewLayer.UpdateLogoThumbnail();
  Result := LNewLayer;
end;

function TgmLayerLoader3.GetGradientMapLayer(
  const ALayerHeader: TgmLayerHeaderVer1): TgmCustomLayer;
var
  LNewLayer       : TgmGradientMapLayer;
  LLoadedGradient : TgmGradientItem;
  i               : Cardinal;
  LByteMap        : TByteMap;
  LByteBits       : PByte;
  LBooleanValue   : Boolean;
  LColorValue     : TColor;
  LGradientReader : TgmGrdVer1_Reader;
begin
  // create a Gradient Map layer
  LNewLayer := TgmGradientMapLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  LNewLayer.LayerName          := ALayerHeader.LayerName;
  LNewLayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
  LNewLayer.LayerOpacity       := ALayerHeader.MasterAlpha;
  LNewLayer.IsSelected         := ALayerHeader.Selected;
  LNewLayer.IsLayerVisible     := ALayerHeader.Visible;
  LNewLayer.IsLockTransparency := ALayerHeader.LockTransparency;
  LNewLayer.IsDuplicated       := ALayerHeader.Duplicated;

  if ALayerHeader.HasMask then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // set up the GradientMap layer, load settings from stream
  LGradientReader := TgmGrdVer1_Reader.Create;
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient.Assign(LLoadedGradient);
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  // read Forground/Background color for gradient
  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.ForegroundColor := LColorValue;

  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.BackgroundColor := LColorValue;

  FFileStream.Read(LBooleanValue, 1);
  LNewLayer.IsReversed := LBooleanValue;

  // Read in the boolean value for preview settings,
  // which is discarded for TgmGradientMapLayer.
  // Just read it in to make the read position of
  // the file stream be correct.
  FFileStream.Read(LBooleanValue, 1);

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      LNewLayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(LNewLayer.MaskBitmap, ctUniformRGB);
      LNewLayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end;

  Result := LNewLayer;
end;

{ TgmLayerLoader4 }

function TgmLayerLoader4.GetBrightContrastLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmBrightContrastLayer;
  LIntValue : Integer;
begin
  // create the Brightness/Contrast layer
  LNewLayer := TgmBrightContrastLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // set up the Brightness/Contrast layer, load settings from stream
  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.BrightAmount := LIntValue;

  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.ContrastAmount := LIntValue;
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetChannelMixerLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmChannelMixerLayer;
begin
  // create the Channel Mixer layer
  LNewLayer := TgmChannelMixerLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the Channel Mixer data in from the stream
  LNewLayer.ChannelMixer.LoadFromStream(FFileStream);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetColorBalanceLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmColorBalanceLayer;
begin
  // create the Color Balance layer
  LNewLayer := TgmColorBalanceLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the color balance data in from the stream
  LNewLayer.ColorBalance.LoadFromStream(FFileStream);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetCurvesLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmCurvesLayer;
begin
  // create the Curves layer
  LNewLayer := TgmCurvesLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the Curves data in from the stream
  LNewLayer.CurvesTool.LoadFromStream(FFileStream);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetGradientFillLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer       : TgmGradientFillLayer;
  LLoadedGradient : TgmGradientItem;
  LGradientReader : TgmGrdVer1_Reader;
  LBooleanValue   : Boolean;
  LColorValue     : TColor;
  LIntValue       : Integer;
  LSettings       : TgmGradientFillSettings;
begin
  // create athe Gradient Fill layer
  LNewLayer := TgmGradientFillLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // read the Color Gradient data in from the stream
  LGradientReader := TgmGrdVer1_Reader.Create();
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient.Assign(LLoadedGradient);
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  // read Forground/Background color in from the stream for the gradient
  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.ForegroundColor := LColorValue;

  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.BackgroundColor := LColorValue;

  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LSettings.Style := TgmGradientRenderMode(LIntValue);

  FFileStream.Read( LSettings.Angle, SizeOf(Integer) );
  FFileStream.Read( LSettings.Scale, SizeOf(Double) );
  FFileStream.Read( LSettings.TranslateX, SizeOf(Integer) );
  FFileStream.Read( LSettings.TranslateY, SizeOf(Integer) );
  FFileStream.Read( LSettings.Reversed, SizeOf(Boolean) );

  FFileStream.Read( LSettings.StartPoint.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.StartPoint.Y, SizeOf(Integer) );
  FFileStream.Read( LSettings.EndPoint.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.EndPoint.Y, SizeOf(Integer) );
  FFileStream.Read( LSettings.CenterPoint.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.CenterPoint.Y, SizeOf(Integer) );

  FFileStream.Read( LSettings.OriginalCenter.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.OriginalCenter.Y, SizeOf(Integer) );
  FFileStream.Read( LSettings.OriginalStart.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.OriginalStart.Y, SizeOf(Integer) );
  FFileStream.Read( LSettings.OriginalEnd.X, SizeOf(Integer) );
  FFileStream.Read( LSettings.OriginalEnd.Y, SizeOf(Integer) );

  LNewLayer.Setup(LSettings);
  LNewLayer.CalculateGradientCoord();
  LNewLayer.DrawGradientOnLayer();
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.UpdateLogoThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetGradientMapLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer       : TgmGradientMapLayer;
  LLoadedGradient : TgmGradientItem;
  LGradientReader : TgmGrdVer1_Reader;
  LBooleanValue   : Boolean;
  LColorValue     : TColor;
begin
  // create a Gradient Map layer
  LNewLayer := TgmGradientMapLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // read the Color Gradient data in from the stream
  LGradientReader := TgmGrdVer1_Reader.Create();
  LLoadedGradient := TgmGradientItem.Create(nil);
  try
    LGradientReader.LoadItemFromStream(FFileStream, LLoadedGradient);
    LNewLayer.Gradient.Assign(LLoadedGradient);
  finally
    LLoadedGradient.Free();
    LGradientReader.Free();
  end;

  // read Forground/Background color in from the stream for the gradient
  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.ForegroundColor := LColorValue;

  FFileStream.Read( LColorValue, SizeOf(TColor) );
  LNewLayer.Gradient.BackgroundColor := LColorValue;

  FFileStream.Read( LBooleanValue, SizeOf(Boolean) );
  LNewLayer.IsReversed := LBooleanValue;
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetHueSaturationLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmHueSaturationLayer;
  LIntValue : Integer;
begin
  // create a Hue/Saturation layer
  LNewLayer := TgmHueSaturationLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the settings of a Hue/Saturation layer in from the stream
  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.Hue := LIntValue;

  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.Saturation := LIntValue;

  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.LightValue := LIntValue;

  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.AdjustMode := TgmHueSaturationAdjustMode(LIntValue);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetInvertLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmInvertLayer;
begin
  // create a Invert layer
  LNewLayer := TgmInvertLayer.Create(FLayerList, FLayerWidth, FLayerHeight);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetLayer: TgmCustomLayer;
var
  LLayerHeader : TgmLayerHeaderVer2;
begin
  Result := nil;

  if Assigned(FFileStream) and Assigned(FLayerList) then
  begin
    FFileStream.Read(LLayerHeader, SizeOf(TgmLayerHeaderVer2));

    case TgmLayerType(LLayerHeader.LayerType) of
      ltNormal:
        begin
          Result := GetNormalLayer(LLayerHeader);
        end;
        
      ltSolidColor:
        begin
          Result := GetSolidColorLayer(LLayerHeader);
        end;
        
      ltBrightContrast:
        begin
          Result := GetBrightContrastLayer(LLayerHeader);
        end;
        
      ltCurves:
        begin
          Result := GetCurvesLayer(LLayerHeader);
        end;
        
      ltLevels:
        begin
          Result := GetLevelsLayer(LLayerHeader);
        end;
        
      ltColorBalance:
        begin
          Result := GetColorBalanceLayer(LLayerHeader);
        end;
        
      ltHueSaturation:
        begin
          Result := GetHueSaturationLayer(LLayerHeader);
        end;

      ltInvert:
        begin
          Result := GetInvertLayer(LLayerHeader);
        end;
        
      ltThreshold:
        begin
          Result := GetThresholdLayer(LLayerHeader);
        end;
        
      ltPosterize:
        begin
          Result := GetPosterizeLayer(LLayerHeader);
        end;

      ltPattern:
        begin
          Result := GetPatternLayer(LLayerHeader);
        end;

      ltVector:
        begin
          Result := GetVectorLayer(LLayerHeader);
        end;

      ltGradientMap:
        begin
          Result := GetGradientMapLayer(LLayerHeader);
        end;

      ltGradientFill:
        begin
          Result := GetGradientFillLayer(LLayerHeader);
        end;

      ltShapeRegion:
        begin
          Result := GetShapeRegionLayer(LLayerHeader);
        end;

      ltRichText:
        begin
          Result := GetRichTextLayer(LLayerHeader);
        end;

      ltChannelMixer:
        begin
          Result := GetChannelMixerLayer(LLayerHeader);
        end;  
    end;
  end;
end;

function TgmLayerLoader4.GetLevelsLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer : TgmLevelsLayer;
  LIntValue : Integer;
begin
  // create a Levels layer
  LNewLayer := TgmLevelsLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the Levels data in from the stream
  LNewLayer.LevelsTool.LoadFromStream(FFileStream);

  // and load in the value of Hitogram Scale
  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.HistogramScale := TgmGimpHistogramScale(LIntValue);
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetNormalLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer     : TgmNormalLayer;
  i, LRowStride : Cardinal;
  LAsBackground : Boolean;
  LPixelBits    : PColor32;
begin
  // Read in the Boolean mark from stream for identifying whether or not
  // the normal layer is used as Background layer.
  FFileStream.Read( LAsBackground, SizeOf(Boolean) );
  
  // create a normal layer
  LNewLayer := TgmNormalLayer.Create(FLayerList, FLayerWidth, FLayerHeight,
                                     0, LAsBackground);

  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // load in the image data from the stream
  LPixelBits := @LNewLayer.LayerBitmap.Bits[0];
  LRowStride := FLayerWidth * SizeOf(TColor32);

  for i := 0 to (FLayerHeight - 1) do
  begin
    FFileStream.Read(LPixelBits^, LRowStride);
    Inc(LPixelBits, FLayerWidth);
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.UpdateLayerThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetPatternLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer      : TgmPatternLayer;
  LDoubleValue   : Double;
  i, LRowStride  : Integer;
  LPatternWidth  : Integer;
  LPatternHeight : Integer;
  LPatternBmp    : TBitmap32;
  LPixelBits     : PColor32;
begin
  // create a Pattern layer
  LNewLayer := TgmPatternLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // read the settings and data of the Pattern layer in from stream ...

  FFileStream.Read(LDoubleValue, SizeOf(Double));
  LNewLayer.Scale := LDoubleValue;

  FFileStream.Read( LPatternWidth, SizeOf(Integer) );
  FFileStream.Read( LPatternHeight, SizeOf(Integer) );

  LPatternBmp := TBitmap32.Create();
  try
    LPatternBmp.SetSize(LPatternWidth, LPatternHeight);

    LRowStride := LPatternWidth * SizeOf(TColor32);
    LPixelBits := @LPatternBmp.Bits[0];

    for i := 0 to (LPatternHeight - 1) do
    begin
      FFileStream.Read(LPixelBits^, LRowStride);
      Inc(LPixelBits, LPatternWidth);
    end;

    // Setting the PatternBitmap property of the pattern layer
    // will cause the layer to be filled up with the loaded
    // pattern. 
    LNewLayer.PatternBitmap := LPatternBmp;
  finally
    LPatternBmp.Free();
  end;

  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.UpdateLogoThumbnail();

  Result := LNewLayer;
end;

function TgmLayerLoader4.GetPosterizeLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer  : TgmPosterizeLayer;
  LByteValue : Byte;
begin
  // create a Posterize layer
  LNewLayer := TgmPosterizeLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  FFileStream.Read( LByteValue, SizeOf(Byte) );
  LNewLayer.Level := LByteValue;
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader4.GetRichTextLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer   : TgmRichTextLayer;
  x, y        : Integer;
  LStreamSize : Int64;
begin
  // create the Rich Text layer
  LNewLayer := TgmRichTextLayer.Create(FLayerList, FLayerWidth, FLayerHeight);
  LNewLayer.AssociateTextEditor(FTextEditor);

  // read data of a Rich Text layer in from the stream

  FFileStream.Read( x, SizeOf(Integer) );
  FFileStream.Read( y, SizeOf(Integer) );
  LNewLayer.BorderStart := Point(x, y);

  FFileStream.Read( x, SizeOf(Integer) );
  FFileStream.Read( y, SizeOf(Integer) );
  LNewLayer.BorderEnd := Point(x, y);

  // read in the rich text stream
  FFileStream.Read( LStreamSize, SizeOf(Int64) );
  LNewLayer.RichTextStream.Size := LStreamSize;

  if LStreamSize > 0 then
  begin
    LNewLayer.RichTextStream.Position := 0;
    FFileStream.Read(LNewLayer.RichTextStream.Memory^, LStreamSize);
    LNewLayer.RichTextStream.Position := 0;
  end;

  FTextEditor.Lines.LoadFromStream(LNewLayer.RichTextStream);
  LNewLayer.DrawTextOnLayer();

  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetShapeRegionLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer          : TgmShapeRegionLayer;
  LRegionColor       : TColor;
  i, LIntValue       : Integer;
  LBooleanValue      : Boolean;
  LOutlineListReader : TgmOutlineListReader;
begin
  // create a Shape Region layer
  LNewLayer := TgmShapeRegionLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the data of a Shape Region layer from stream

  FFileStream.Read( LRegionColor, SizeOf(TColor) );
  LNewLayer.RegionColor := LRegionColor;

  // read brush style in
  FFileStream.Read( LIntValue, SizeOf(Integer) );
  LNewLayer.BrushStyle := TBrushStyle(LIntValue);

  FFileStream.Read( LBooleanValue, SizeOf(Boolean) );
  LNewLayer.IsDismissed := LBooleanValue;

  // read the outlines in
  LOutlineListReader := TgmOutlineListReader1.Create(LNewLayer.ShapeOutlineList);
  try
    LOutlineListReader.LoadFromStream(FFileStream);
  finally
    LOutlineListReader.Free();
  end;

  LNewLayer.ShapeRegion.AccumRGN := LNewLayer.ShapeOutlineList.GetScaledShapesRegion();
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;

  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.DrawRegionOnLayer();
  LNewLayer.UpdateLayerThumbnail();
  LNewLayer.UpdateLogoThumbnail();    

  Result := LNewLayer;
end;

function TgmLayerLoader4.GetSolidColorLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer     : TgmSolidColorLayer;
  LFillingColor : TColor32;
begin
  // create the Solid Color layer
  LNewLayer := TgmSolidColorLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // load the filling color from stream
  FFileStream.Read( LFillingColor, SizeOf(TColor32) );

  LNewLayer.SolidColor := LFillingColor;
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.UpdateLogoThumbnail();
  
  Result := LNewLayer;
end;

function TgmLayerLoader4.GetThresholdLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer  : TgmThresholdLayer;
  LByteValue : Byte;
begin
  // create a Threshold layer
  LNewLayer := TgmThresholdLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  FFileStream.Read( LByteValue, SizeOf(Byte) );
  LNewLayer.Level := LByteValue;
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  Result := LNewLayer;
end;

function TgmLayerLoader4.GetVectorLayer(
  const ALayerHeader: TgmLayerHeaderVer2): TgmCustomLayer;
var
  LNewLayer         : TgmVectorLayer;
  LFigureListReader : TgmFigureListReader;
begin
  // create a Vector layer
  LNewLayer := TgmVectorLayer.Create(FLayerList, FLayerWidth, FLayerHeight);

  // read the data of the figure list in from stream
  LFigureListReader := TgmFigureListReader1.Create(LNewLayer.FigureList);
  try
    LFigureListReader.LoadFromStream(FFileStream);
  finally
    LFigureListReader.Free();
  end;
  
  SetLayerCommonProperties(LNewLayer, ALayerHeader);

  if ALayerHeader.MaskEnabled then
  begin
    LNewLayer.EnableMask();
    LNewLayer.IsMaskLinked := ALayerHeader.MaskLinked;
  end;
  
  // read in the mask data from the stream
  if LNewLayer.IsMaskEnabled then
  begin
    LoadMaskForLayer(LNewLayer);
  end;

  LNewLayer.DrawAllFiguresOnLayer();
  LNewLayer.UpdateLayerThumbnail();
  
  Result := LNewLayer;
end;

procedure TgmLayerLoader4.LoadMaskForLayer(ALayer: TgmCustomLayer);
var
  i         : Cardinal;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  if Assigned(ALayer) and ALayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      LByteMap.SetSize(FLayerWidth, FLayerHeight);

      // read in mask data
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (FLayerHeight - 1) do
      begin
        FFileStream.Read(LByteBits^, FLayerWidth);
        Inc(LByteBits, FLayerWidth);
      end;

      ALayer.MaskBitmap.SetSize(FLayerWidth, FLayerHeight);
      LByteMap.WriteTo(ALayer.MaskBitmap, ctUniformRGB);
      ALayer.UpdateMaskThumbnail();
    finally
      LByteMap.Free();
    end;
  end; 
end;

procedure TgmLayerLoader4.SetLayerCommonProperties(ALayer: TgmCustomLayer;
  const ALayerHeader: TgmLayerHeaderVer2);
begin
  if Assigned(ALayer) then
  begin
    ALayer.LayerName          := ALayerHeader.LayerName;
    ALayer.LayerBlendMode     := TBlendMode32(ALayerHeader.BlendModeIndex);
    ALayer.LayerOpacity       := ALayerHeader.Opacity;
    ALayer.IsSelected         := ALayerHeader.Selected;
    ALayer.IsLayerVisible     := ALayerHeader.Visible;
    ALayer.IsLockTransparency := ALayerHeader.LockTransparency;
    ALayer.IsDuplicated       := ALayerHeader.Duplicated;
  end;
end;


{ TgmLayersWriter }

constructor TgmLayersWriter.Create(ALayerList: TgmLayerList);
begin
  inherited Create();
  
  FLayerList := ALayerList;
end;

function TgmLayersWriter.GetLayerType(ALayer: TgmCustomLayer): TgmLayerType;
begin
  Result := ltUnknown;

  if Assigned(ALayer) then
  begin
    if ALayer is TgmNormalLayer then
    begin
      Result := ltNormal;
    end
    else if ALayer is TgmSolidColorLayer then
    begin
      Result := ltSolidColor;
    end
    else if ALayer is TgmBrightContrastLayer then
    begin
      Result := ltBrightContrast;
    end
    else if ALayer is TgmCurvesLayer then
    begin
      Result := ltCurves;
    end
    else if ALayer is TgmLevelsLayer then
    begin
      Result := ltLevels;
    end
    else if ALayer is TgmColorBalanceLayer then
    begin
      Result := ltColorBalance;
    end
    else if ALayer is TgmHueSaturationLayer then
    begin
      Result := ltHueSaturation;
    end
    else if ALayer is TgmInvertLayer then
    begin
      Result := ltInvert;
    end
    else if ALayer is TgmThresholdLayer then
    begin
      Result := ltThreshold;
    end
    else if ALayer is TgmPosterizeLayer then
    begin
      Result := ltPosterize;
    end
    else if ALayer is TgmPatternLayer then
    begin
      Result := ltPattern;
    end
    else if ALayer is TgmVectorLayer then
    begin
      Result := ltVector;
    end
    else if ALayer is TgmGradientMapLayer then
    begin
      Result := ltGradientMap;
    end
    else if ALayer is TgmGradientFillLayer then
    begin
      Result := ltGradientFill;
    end
    else if ALayer is TgmShapeRegionLayer then
    begin
      Result := ltShapeRegion;
    end
    else if ALayer is TgmRichTextLayer then
    begin
      Result := ltRichText;
    end
    else if ALayer is TgmChannelMixerLayer then
    begin
      Result := ltChannelMixer;
    end;
  end;
end;

procedure TgmLayersWriter.SaveBrightContrastLayer(
  ALayer: TgmBrightContrastLayer; AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the brightness/contrast data to stream
  AStream.Write( ALayer.BrightAmount, SizeOf(Integer) );
  AStream.Write( ALayer.ContrastAmount, SizeOf(Integer) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveChannelMixerLayer(ALayer: TgmChannelMixerLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the channel mixer data to stream
  ALayer.ChannelMixer.SaveToStream(AStream);

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveColorBalanceLayer(ALayer: TgmColorBalanceLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the color balance data to stream
  ALayer.ColorBalance.SaveToStream(AStream);

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveCurvesLayer(ALayer: TgmCurvesLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the Curves data to stream
  ALayer.CurvesTool.SaveToStream(AStream);

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveGradientFillLayer(ALayer: TgmGradientFillLayer;
  AStream: TStream);
var
  LColorValue     : TColor;
  LIntValue       : Integer;
  LGradientWriter : TgmGrdVer1_Writer;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the Color Gradient data to stream
  LGradientWriter := TgmGrdVer1_Writer.Create();
  try
    LGradientWriter.SaveItemToStream(AStream, ALayer.Gradient);
  finally
    LGradientWriter.Free();
  end;

  // write the forground/background color of the gradient to stream
  LColorValue := ALayer.Gradient.ForegroundColor;
  AStream.Write( LColorValue, SizeOf(TColor) );

  LColorValue := ALayer.Gradient.BackgroundColor;
  AStream.Write( LColorValue, SizeOf(TColor) );

  LIntValue := Ord(ALayer.Style);
  AStream.Write( LIntValue, SizeOf(Integer) );

  AStream.Write( ALayer.Angle, SizeOf(Integer) );
  AStream.Write( ALayer.Scale, SizeOf(Double) );
  AStream.Write( ALayer.TranslateX, SizeOf(Integer) );
  AStream.Write( ALayer.TranslateY, SizeOf(Integer) );
  AStream.Write( ALayer.IsReversed, SizeOf(Boolean) );

  AStream.Write( ALayer.StartPoint.X, SizeOf(Integer) );
  AStream.Write( ALayer.StartPoint.Y, SizeOf(Integer) );
  AStream.Write( ALayer.EndPoint.X, SizeOf(Integer) );
  AStream.Write( ALayer.EndPoint.Y, SizeOf(Integer) );
  AStream.Write( ALayer.CenterPoint.X, SizeOf(Integer) );
  AStream.Write( ALayer.CenterPoint.Y, SizeOf(Integer) );

  AStream.Write( ALayer.OriginalCenter.X, SizeOf(Integer) );
  AStream.Write( ALayer.OriginalCenter.Y, SizeOf(Integer) );
  AStream.Write( ALayer.OriginalStart.X, SizeOf(Integer) );
  AStream.Write( ALayer.OriginalStart.Y, SizeOf(Integer) );
  AStream.Write( ALayer.OriginalEnd.X, SizeOf(Integer) );
  AStream.Write( ALayer.OriginalEnd.Y, SizeOf(Integer) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveGradientMapLayer(ALayer: TgmGradientMapLayer;
  AStream: TStream);
var
  LColorValue     : TColor;
  LGradientWriter : TgmGrdVer1_Writer;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the Color Gradient data to stream
  LGradientWriter := TgmGrdVer1_Writer.Create();
  try
    LGradientWriter.SaveItemToStream(AStream, ALayer.Gradient);
  finally
    LGradientWriter.Free();
  end;

  // write forground/background color of the gradient to stream
  LColorValue := ALayer.Gradient.ForegroundColor;
  AStream.Write( LColorValue, SizeOf(TColor) );

  LColorValue := ALayer.Gradient.BackgroundColor;
  AStream.Write( LColorValue, SizeOf(TColor) );

  AStream.Write( ALayer.IsReversed, SizeOf(Boolean) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveHueSaturationLayer(ALayer: TgmHueSaturationLayer;
  AStream: TStream);
var
  LIntValue : Integer; 
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the settings of Hue/Saturation layer to stream
  AStream.Write( ALayer.Hue, SizeOf(Integer) );
  AStream.Write( ALayer.Saturation, SizeOf(Integer) );
  AStream.Write( ALayer.LightValue, SizeOf(Integer) );

  LIntValue := Ord(ALayer.AdjustMode);
  AStream.Write( LIntValue, SizeOf(Integer) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveInvertLayer(ALayer: TgmInvertLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveLayerByType(ALayer: TgmCustomLayer;
  AStream: TStream);
var
  LLayerType : TgmLayerType;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // save layer header first ...
  Self.SaveLayerHeader(ALayer, AStream);

  LLayerType := GetLayerType(ALayer);

  case LLayerType of
    ltNormal:
      begin
        SaveNormalLayer( TgmNormalLayer(ALayer), AStream );
      end;

    ltSolidColor:
      begin
        SaveSolidColorLayer( TgmSolidColorLayer(ALayer), AStream );
      end;

    ltBrightContrast:
      begin
        SaveBrightContrastLayer( TgmBrightContrastLayer(ALayer), AStream );
      end;

    ltCurves:
      begin
        SaveCurvesLayer( TgmCurvesLayer(ALayer), AStream );
      end;

    ltLevels:
      begin
        SaveLevelsLayer( TgmLevelsLayer(ALayer), AStream );
      end;

    ltColorBalance:
      begin
        SaveColorBalanceLayer( TgmColorBalanceLayer(ALayer), AStream );
      end;

    ltHueSaturation:
      begin
        SaveHueSaturationLayer( TgmHueSaturationLayer(ALayer), AStream );
      end;

    ltInvert:
      begin
        SaveInvertLayer( TgmInvertLayer(ALayer), AStream );
      end;

    ltThreshold:
      begin
        SaveThresholdLayer( TgmThresholdLayer(ALayer), AStream );
      end;
      
    ltPosterize:
      begin
        SavePosterizeLayer( TgmPosterizeLayer(ALayer), AStream );
      end;

    ltPattern:
      begin
        SavePatternLayer( TgmPatternLayer(ALayer), AStream );
      end;

    ltVector:
      begin
        SaveVectorLayer( TgmVectorLayer(ALayer), AStream );
      end;

    ltGradientMap:
      begin
        SaveGradientMapLayer( TgmGradientMapLayer(ALayer), AStream ); 
      end;

    ltGradientFill:
      begin
        SaveGradientFillLayer( TgmGradientFillLayer(ALayer), AStream );
      end;

    ltShapeRegion:
      begin
        SaveShapeRegionLayer( TgmShapeRegionLayer(ALayer), AStream );
      end;

    ltRichText:
      begin
        SaveRichTextLayer( TgmRichTextLayer(ALayer), AStream );
      end;

    ltChannelMixer:
      begin
        SaveChannelMixerLayer( TgmChannelMixerLayer(ALayer), AStream );
      end;
  end;
end;

procedure TgmLayersWriter.SaveLayerHeader(ALayer: TgmCustomLayer;
  AStream: TStream);
var
  LLayerHeader : TgmLayerHeaderVer2;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  with LLayerHeader do
  begin
    LayerName        := ALayer.LayerName;
    LayerType        := Ord( GetLayerType(ALayer) );
    BlendModeIndex   := Ord(ALayer.LayerBlendMode);
    Opacity          := ALayer.LayerOpacity;
    Selected         := ALayer.IsSelected;
    Visible          := ALayer.IsLayerVisible;
    LockTransparency := ALayer.IsLockTransparency;
    Duplicated       := ALayer.IsDuplicated;
    MaskEnabled      := ALayer.IsMaskEnabled;
    MaskLinked       := ALayer.IsMaskLinked;
  end;

  AStream.Write(LLayerHeader, SizeOf(TgmLayerHeaderVer2));
end;

procedure TgmLayersWriter.SaveLevelsLayer(ALayer: TgmLevelsLayer;
  AStream: TStream);
var
  LIntValue : Integer; 
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the Levels data to stream
  ALayer.LevelsTool.SaveToStream(AStream);

  // save the Hitogram Scale enumerated value as integer
  LIntValue := Ord(ALayer.HistogramScale);
  AStream.Write( LIntValue, SizeOf(Integer) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveMask(ALayer: TgmCustomLayer; AStream: TStream);
var
  i         : Integer;
  LByteMap  : TByteMap;
  LByteBits : PByte;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    LByteMap := TByteMap.Create();
    try
      // write the mask data to stream
      LByteMap.SetSize(ALayer.MaskBitmap.Width, ALayer.MaskBitmap.Height);
      LByteMap.ReadFrom(ALayer.MaskBitmap, ctUniformRGB);

      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (ALayer.MaskBitmap.Height - 1) do
      begin
        AStream.Write(LByteBits^, ALayer.MaskBitmap.Width);
        Inc(LByteBits, ALayer.MaskBitmap.Width);
      end;
    finally
      LByteMap.Free();
    end;
  end;
end;

procedure TgmLayersWriter.SaveNormalLayer(ALayer: TgmNormalLayer;
  AStream: TStream);
var
  i          : Integer;
  LPixelBits : PColor32;
  LRowStride : Cardinal; // in bytes
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // Write the Boolean mark to stream for identifying whether or not
  // the normal layer is used as Background layer.
  AStream.Write( ALayer.IsAsBackground, SizeOf(Boolean) );

  // write the pixels of the layer to stream
  LRowStride := ALayer.LayerBitmap.Width * SizeOf(TColor32);
  LPixelBits := @ALayer.LayerBitmap.Bits[0];
  
  for i := 0 to (ALayer.LayerBitmap.Height - 1) do
  begin
    AStream.Write(LPixelBits^, LRowStride);
    Inc(LPixelBits, ALayer.LayerBitmap.Width);
  end;

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SavePatternLayer(ALayer: TgmPatternLayer;
  AStream: TStream);
var
  i              : Integer;
  LPatternWidth  : Integer;
  LPatternHeight : Integer;
  LRowStride     : Integer;
  LPixelBits     : PColor32;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  LPatternWidth  := ALayer.PatternBitmap.Width;
  LPatternHeight := ALayer.PatternBitmap.Height;

  // write the settings of the pattern
  AStream.Write( ALayer.Scale, SizeOf(Double) );
  AStream.Write( LPatternWidth, SizeOf(Integer) );
  AStream.Write( LPatternHeight, SizeOf(Integer) );

  // write the pixel data of the pattern
  LRowStride := LPatternWidth * SizeOf(TColor32);
  LPixelBits := @ALayer.PatternBitmap.Bits[0];

  for i := 0 to (LPatternHeight - 1) do
  begin
    AStream.Write(LPixelBits^, LRowStride);
    Inc(LPixelBits, LPatternWidth);
  end;

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SavePosterizeLayer(ALayer: TgmPosterizeLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  AStream.Write( ALayer.Level, SizeOf(Byte) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveRichTextLayer(ALayer: TgmRichTextLayer;
  AStream: TStream);
var
  LStreamSize : Int64;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the data of the Rich Text layer to stream ...

  AStream.Write( ALayer.BorderStart.X, SizeOf(Integer) );
  AStream.Write( ALayer.BorderStart.Y, SizeOf(Integer) );
  AStream.Write( ALayer.BorderEnd.X, SizeOf(Integer) );
  AStream.Write( ALayer.BorderEnd.Y, SizeOf(Integer) );

  // write the rich text stream to output stream
  LStreamSize := ALayer.RichTextStream.Size;
  AStream.Write( LStreamSize, SizeOf(Int64) );

  if ALayer.RichTextStream.Size > 0 then
  begin
    ALayer.RichTextStream.Position := 0;
    ALayer.RichTextStream.SaveToStream(AStream);
    ALayer.RichTextStream.Position := 0;
  end;

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveShapeRegionLayer(ALayer: TgmShapeRegionLayer;
  AStream: TStream);
var
  LIntValue    : Integer;
  LRegionColor : TColor;
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the shape region data to stream ...
  
  LRegionColor := ALayer.RegionColor;
  AStream.Write( LRegionColor, SizeOf(TColor) );

  LIntValue := Ord(ALayer.BrushStyle);
  AStream.Write( LIntValue, SizeOf(Integer) );

  AStream.Write( ALayer.IsDismissed, SizeOf(Boolean) );

  ALayer.ShapeOutlineList.SaveToStream(AStream);

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveSolidColorLayer(ALayer: TgmSolidColorLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the filling color to stream
  AStream.Write( ALayer.SolidColor, SizeOf(TColor32) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveThresholdLayer(ALayer: TgmThresholdLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  AStream.Write( ALayer.Level, SizeOf(Byte) );

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;

procedure TgmLayersWriter.SaveToStream(AStream: TStream);
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AStream) then
  begin
    Exit;
  end;

  if Assigned(FLayerList) and (FLayerList.Count > 0) then
  begin
    for i := 0 to (FLayerList.Count - 1) do
    begin
      LLayer := FLayerList.Layers[i];
      SaveLayerByType(LLayer, AStream);
    end;
  end;
end;

procedure TgmLayersWriter.SaveVectorLayer(ALayer: TgmVectorLayer;
  AStream: TStream);
begin
  if ( not Assigned(ALayer) ) or ( not Assigned(AStream) ) then
  begin
    Exit;
  end;

  // write the data of vectors to stream
  ALayer.FigureList.SaveToStream(AStream);

  // write the mask data of the layer to stream, if any
  if ALayer.IsMaskEnabled then
  begin
    SaveMask(ALayer, AStream);
  end;
end;


end.
