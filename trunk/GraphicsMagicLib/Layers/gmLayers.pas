unit gmLayers;

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
 * The Original Code is igLayers.pas.
 *
 * This unit is adapted from Original Code for GraphicsMagic.
 *
 * The Initial Developer of the Original Code and this unit are
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/12/26

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Delphi }
  Classes, Contnrs,
{ Graphics32 }
  GR32,
{ externals\Graphics32_add_ons }
  GR32_Add_BlendModes,
{ GraphicsMagicLib }
  gmCrop,
  gmResamplers,
  gmTypes;

type
  TgmLayerPixelFeature = (lpfNone,
                          lpfNormalPixelized,   // Such layers can be editing with any tools,
                                                // example of such a layer is Normal layer.
                          lpfSpecialPixelized,  // Such layers can be editing only with its own specific tools,
                                                // example of such a layer is Gradient layer.
                          lpfNonPixelized       // Such layers have dummy pixels, and can only take
                                                // effect on the blending result of beneath layers
                          );

  // mark process stage -- on the layer or on the mask
  TgmLayerProcessStage = (lpsLayer, lpsMask);

  TgmLayerProcessStageChanged = procedure (ASender: TObject; const AStage: TgmLayerProcessStage) of object;
  TgmMaskLinkChanged = procedure (ASender: TObject; const ALinked: Boolean) of object;

  { Forward Declarations }
  TgmLayerList = class;
  TgmClassCounter = class;

  { TgmCustomLayer }

  TgmCustomLayer = class(TObject)
  protected
    FOwner                 : TgmLayerList;
    FLayerBitmap           : TBitmap32;
    FLayerThumb            : TBitmap32;
    FMaskBitmap            : TBitmap32;
    FMaskThumb             : TBitmap32;
    FLogoBitmap            : TBitmap32;
    FLogoThumb             : TBitmap32;
    FLayerBlendMode        : TBlendMode32;
    FLayerBlendEvent       : TPixelCombineEvent;
    FLayerVisible          : Boolean;
    FLayerProcessStage     : TgmLayerProcessStage;
    FPixelFeature          : TgmLayerPixelFeature;  // the pixel feature of the layer
    FSelected              : Boolean;
    FLayerEnabled          : Boolean;               // indicate whether the layer is currently editable
    FDuplicated            : Boolean;               // indicate whether this layer is duplicated from another one
    FMaskEnabled           : Boolean;               // indicate whether this layer has a mask
    FMaskLinked            : Boolean;               // indicate whether this layer is linked to a mask
    FLayerThumbEnabled     : Boolean;               // indicate whether this layer has a layer thumbnail
    FLogoThumbEnabled      : Boolean;               // indicate whether this layer has a logo thumbnail
    FLockTransparency      : Boolean;               // indicate whether the alpha channel of this layer is locked
    FRenamed               : Boolean;               // whether the layer has a custom name
    FRealThumbRect         : TRect;
    FDefaultLayerName      : string;
    FLayerName             : string;                // current layer name

    FOnChange              : TNotifyEvent;
    FOnLayerDisabled       : TNotifyEvent;
    FOnLayerEnabled        : TNotifyEvent;
    FOnMaskEnabled         : TNotifyEvent;
    FOnMaskDisabled        : TNotifyEvent;
    FOnMaskLinkChanged     : TgmMaskLinkChanged;
    FOnThumbUpdate         : TNotifyEvent;
    FOnPanelDblClick       : TNotifyEvent;
    FOnLayerThumbDblClick  : TNotifyEvent;
    FOnMaskThumbDblClick   : TNotifyEvent;
    FOnLogoThumbDblClick   : TNotifyEvent;
    FOnProcessStageChanged : TgmLayerProcessStageChanged;

    function GetLayerOpacity: Byte;
    
    function GetThumbZoomScale(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;

    function GetRealThumbRect(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer;
      const AMarginSize: Integer = 4): TRect;

    procedure CopyCommonDataTo(ADestLayer: TgmCustomLayer);
    procedure SetLayerEnabled(AValue: Boolean);
    procedure SetLayerVisible(AValue: Boolean);
    procedure SetMaskEnabled(AValue: Boolean);
    procedure SetMaskLinked(AValue: Boolean);
    procedure SetLayerBlendMode(AValue: TBlendMode32);
    procedure SetLayerOpacity(AValue: Byte);
    procedure SetLayerProcessStage(AValue: TgmLayerProcessStage);
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); virtual;
    procedure InitMask;

    procedure ResizeMaskCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection);

    procedure RotateMaskCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection);

    procedure CropMask(ACrop: TgmCrop); overload;
    procedure CropMask(const ACropArea: TRect); overload;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer;
      const AFillColor: TColor32 = $00000000);

    destructor Destroy; override;

    procedure Changed; overload;
    procedure Changed(const ARect: TRect); overload;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); virtual; abstract;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); virtual; abstract;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); virtual; abstract;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); virtual; abstract;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection;
      const ABackgroundColor: TColor32); virtual; abstract;

    procedure SimplySetLayerVisible(const AValue: Boolean);

    procedure UpdateLayerThumbnail; virtual;
    procedure UpdateMaskThumbnail;
    procedure UpdateLogoThumbnail; virtual;

    function EnableMask: Boolean;
    function DiscardMask: Boolean;
    function GetCopy: TgmCustomLayer; virtual; abstract;
    function SimplyDiscardMask: Boolean;
    function SimplyEnableMask: Boolean;

    property LayerBitmap           : TBitmap32                   read FLayerBitmap;
    property LayerThumbnail        : TBitmap32                   read FLayerThumb;
    property MaskBitmap            : TBitmap32                   read FMaskBitmap;
    property MaskThumbnail         : TBitmap32                   read FMaskThumb;
    property LogoBitmap            : TBitmap32                   read FLogoBitmap;
    property LogoThumbnail         : TBitmap32                   read FLogoThumb;
    property IsLayerEnabled        : Boolean                     read FLayerEnabled          write SetLayerEnabled;
    property IsLayerVisible        : Boolean                     read FLayerVisible          write SetLayerVisible;
    property IsDuplicated          : Boolean                     read FDuplicated            write FDuplicated;
    property IsSelected            : Boolean                     read FSelected              write FSelected;
    property IsMaskEnabled         : Boolean                     read FMaskEnabled;
    property IsMaskLinked          : Boolean                     read FMaskLinked            write SetMaskLinked;
    property IsLayerThumbEnabled   : Boolean                     read FLayerThumbEnabled;
    property IsLogoThumbEnabled    : Boolean                     read FLogoThumbEnabled;
    property IsLockTransparency    : Boolean                     read FLockTransparency      write FLockTransparency;
    property IsRenamed             : Boolean                     read FRenamed               write FRenamed;
    property LayerName             : string                      read FLayerName             write FLayerName;
    property LayerBlendMode        : TBlendMode32                read FLayerBlendMode        write SetLayerBlendMode;
    property LayerOpacity          : Byte                        read GetLayerOpacity        write SetLayerOpacity;
    property LayerProcessStage     : TgmLayerProcessStage        read FLayerProcessStage     write SetLayerProcessStage;
    property PixelFeature          : TgmLayerPixelFeature        read FPixelFeature;
    property OnChange              : TNotifyEvent                read FOnChange              write FOnChange;
    property OnThumbnailUpdate     : TNotifyEvent                read FOnThumbUpdate         write FOnThumbUpdate;
    property OnMaskEnabled         : TNotifyEvent                read FOnMaskEnabled         write FOnMaskEnabled;
    property OnMaskDisabled        : TNotifyEvent                read FOnMaskDisabled        write FOnMaskDisabled;
    property OnMaskLinkChanged     : TgmMaskLinkChanged          read FOnMaskLinkChanged     write FOnMaskLinkChanged;
    property OnPanelDblClick       : TNotifyEvent                read FOnPanelDblClick       write FOnPanelDblClick;
    property OnLayerDisabled       : TNotifyEvent                read FOnLayerDisabled       write FOnLayerDisabled;
    property OnLayerEnabled        : TNotifyEvent                read FOnLayerEnabled        write FOnLayerEnabled;
    property OnLayerThumbDblClick  : TNotifyEvent                read FOnLayerThumbDblClick  write FOnLayerThumbDblClick;
    property OnMaskThumbDblClick   : TNotifyEvent                read FOnMaskThumbDblClick   write FOnMaskThumbDblClick;
    property OnLogoThumbDblClick   : TNotifyEvent                read FOnLogoThumbDblClick   write FOnLogoThumbDblClick;
    property OnProcessStageChanged : TgmLayerProcessStageChanged read FOnProcessStageChanged write FOnProcessStageChanged;
  end;

  TgmCustomLayerClass = class of TgmCustomLayer;


  { TgmNonPixelizedLayer }

  TgmNonPixelizedLayer = class(TgmCustomLayer)
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;
  end;


  { TgmSpecialPixelizedLayer }

  TgmSpecialPixelizedLayer = class(TgmCustomLayer)
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;
  end;
  

  { TgmNormalLayer }

  TgmNormalLayer = class(TgmCustomLayer)
  private
    FAsBackground  : Boolean;      // if this layer is a background layer
    FOnMaskApplied : TNotifyEvent;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer;
      const AFillColor: TColor32 = $00000000;
      const AsBackLayer: Boolean = False);

    function ApplyMask: Boolean;
    function GetCopy: TgmCustomLayer; override;
    function IsEmptyLayer: Boolean;
    function SimplyApplyMask: Boolean;

//    procedure UpdateLayerThumbnail; override;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection;
      const ABackgroundColor: TColor32); override;

    property IsAsBackground : Boolean read FAsBackground;
    property OnMaskApplied  : TNotifyEvent read FOnMaskApplied write FOnMaskApplied;
  end;


  { TgmLayerList }

  TgmLayerCombinedEvent = procedure (ASender: TObject; const ARect: TRect) of object;
  TgmLayerOrderChangeEvent = procedure (ASender: TObject; const AOldIndex, ANewIndex: Integer) of object;
  TgmMergeLayerEvent = procedure (AResult: TgmCustomLayer) of object;

  TgmLayerList = class(TObject)
  private
    FItems                : TObjectList;
    FSelectedLayer        : TgmCustomLayer;
    FCombineResult        : TBitmap32;
    FLayerWidth           : Integer;
    FLayerHeight          : Integer;

    FOnLayerCombined      : TgmLayerCombinedEvent;
    FOnSelectionChanged   : TNotifyEvent;
    FOnLayerOrderChanged  : TgmLayerOrderChangeEvent;
    FOnMergeLayerDown     : TgmMergeLayerEvent;
    FOnMergeVisibleLayers : TgmMergeLayerEvent;
    FOnFlattenLayers      : TgmMergeLayerEvent;

    FLayerTypeCounter     : TgmClassCounter;

    function GetLayerCount: Integer;
    function GetLayerMaxIndex: Integer;
    function GetSelectedLayerIndex: Integer;
    function GetLayer(AIndex: Integer): TgmCustomLayer;
    function GetVisbileLayerCount: Integer;
    function GetVisibleNormalLayerCount: Integer;

    procedure BlendLayers; overload;
    procedure BlendLayers(const ARect: TRect); overload;
    procedure DeleteVisibleLayers;
    procedure DeselectAllLayers;
    procedure SetLayerInitialName(ALayer: TgmCustomLayer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ALayer: TgmCustomLayer);
    procedure CancelLayer(AIndex: Integer);
    procedure Clear;
    procedure DeleteLayer(AIndex: Integer);
    procedure DeleteSelectedLayer;
    procedure Insert(AIndex: Integer; ALayer: TgmCustomLayer);
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure SelectLayer(const AIndex: Integer);

    procedure SimplyAdd(ALayer: TgmCustomLayer);
    procedure SimplyDeleteLayer(AIndex: Integer);
    procedure SimplyInsert(AIndex: Integer; ALayer: TgmCustomLayer);
    procedure SimplyMove(ACurIndex, ANewIndex: Integer);
    procedure SimplySelectLayer(const AIndex: Integer);

    procedure FlattenLayersToBitmapWithoutMask(ADestBmp: TBitmap32;
      const ABackgroundColor: TColor32 = $00FFFFFF);

    procedure SetLayerCounter(ALayerClassName: ShortString; const ACount: Integer); overload;
    procedure SetLayerCounter(ALayerType: TgmCustomLayerClass; const ACount: Integer); overload;

    procedure CropLayers(ACrop: TgmCrop; const ABackColor: TColor32); overload;
    procedure CropLayers(const ACropArea: TRect; const ABackColor: TColor32); overload;

    procedure ResizeCanvasOfLayers(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32);

    procedure ResizeImageOfLayers(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions);

    procedure RotateCanvasOfLayers(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);

    function CanFlattenLayers: Boolean;
    function CanMergeSelectedLayerDown: Boolean;
    function CanMergeVisbleLayers: Boolean;
    function FlattenLayers: Boolean;
    function MergeSelectedLayerDown: Boolean;
    function MergeVisibleLayers: Boolean;
    function IsValidIndex(const AIndex: Integer): Boolean;
    function GetBackgroundLayer: TgmNormalLayer;
    function GetHiddenLayerCount: Integer;

    function GetLayerBlendResult(const AMaskAvailable: Boolean): TBitmap32; overload;

    function GetLayerBlendResult(const AIndex1, AIndex2: Integer;
      const AMaskAvailable: Boolean): TBitmap32; overload;

    function GetLayerCounter(ALayerClassName: ShortString): Integer; overload;
    function GetLayerCounter(ALayerType: TgmCustomLayerClass): Integer; overload;

    property CombineResult           : TBitmap32                read FCombineResult;
    property Count                   : Integer                  read GetLayerCount;
    property MaxIndex                : Integer                  read GetLayerMaxIndex;
    property SelectedIndex           : Integer                  read GetSelectedLayerIndex;
    property Layers[AIndex: Integer] : TgmCustomLayer           read GetLayer;
    property SelectedLayer           : TgmCustomLayer           read FSelectedLayer;
    property OnLayerCombined         : TgmLayerCombinedEvent    read FOnLayerCombined      write FOnLayerCombined;
    property OnSelectionChanged      : TNotifyEvent             read FOnSelectionChanged   write FOnSelectionChanged;
    property OnLayerOrderChanged     : TgmLayerOrderChangeEvent read FOnLayerOrderChanged  write FOnLayerOrderChanged;
    property OnMergeLayerDown        : TgmMergeLayerEvent       read FOnMergeLayerDown     write FOnMergeLayerDown;
    property OnMergeVisibleLayers    : TgmMergeLayerEvent       read FOnMergeVisibleLayers write FOnMergeVisibleLayers;
    property OnFlattenLayers         : TgmMergeLayerEvent       read FOnFlattenLayers      write FOnFlattenLayers;
  end;


  { TgmClassRec }

  TgmClassRec = class(TObject)
  private
    FName  : ShortString;
    FCount : Integer;
  public
    constructor Create(const AClassName: ShortString);

    property Name  : ShortString read FName  write FName;
    property Count : Integer     read FCount write FCount;
  end;


  { TgmClassCounter }

  TgmClassCounter = class(TObject)
  private
    FItems : TObjectList;

    function GetIndex(const AClassName: ShortString): Integer;
    function IsValidIndex(const AIndex: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Increase(const AClassName: ShortString);
    procedure Decrease(const AClassName: ShortString);
    procedure Clear;

    function GetCount(const AClassName: ShortString): Integer;
    procedure SetCount(const AClassName: ShortString; const ACount: Integer);
  end;

const
  LAYER_THUMB_SIZE = 36;
  LAYER_LOGO_SIZE  = 36;

implementation

uses
{ Delphi }
  SysUtils, Graphics, Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic lib }
  gmImageProcessFuncs,
  gmPaintFuncs;


{ TgmCustomLayer }

constructor TgmCustomLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer;
  const AFillColor: TColor32 = $00000000);
begin
  inherited Create;

  FOwner             := AOwner;
  FLayerBlendMode    := bbmNormal32;
  FLayerBlendEvent   := GetBlendMode( Ord(FLayerBlendMode) );
  FDuplicated        := False;
  FLayerVisible      := True;
  FSelected          := True;
  FLayerEnabled      := True;
  FMaskEnabled       := False;
  FMaskLinked        := False;
  FLayerThumbEnabled := False;               
  FLogoThumbEnabled  := False;
  FLockTransparency  := False;
  FRenamed           := False;
  FDefaultLayerName  := '';
  FLayerName         := '';
  FLayerProcessStage := lpsLayer;
  FPixelFeature      := lpfNone;
  
  FOnChange              := nil;
  FOnThumbUpdate         := nil;
  FOnMaskEnabled         := nil;
  FOnMaskDisabled        := nil;
  FOnMaskLinkChanged     := nil;
  FOnPanelDblClick       := nil;
  FOnLayerThumbDblClick  := nil;
  FOnMaskThumbDblClick   := nil;
  FOnLogoThumbDblClick   := nil;
  FOnProcessStageChanged := nil;

  FLayerBitmap := TBitmap32.Create;
  with FLayerBitmap do
  begin
    DrawMode    := dmBlend;
    CombineMode := cmMerge;
    
    SetSize(ALayerWidth, ALayerHeight);
    Clear(AFillColor);
  end;

  FMaskBitmap := nil;
  FMaskThumb  := nil;
  FLogoBitmap := nil;
  FLogoThumb  := nil;

  FRealThumbRect := GetRealThumbRect(ALayerWidth, ALayerHeight,
                                     LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
end;

destructor TgmCustomLayer.Destroy;
begin
  FLayerBlendEvent       := nil;
  FOwner                 := nil;
  FOnChange              := nil;
  FOnThumbUpdate         := nil;
  FOnMaskEnabled         := nil;
  FOnMaskDisabled        := nil;
  FOnMaskLinkChanged     := nil;
  FOnPanelDblClick       := nil;
  FOnLayerThumbDblClick  := nil;
  FOnMaskThumbDblClick   := nil;
  FOnLogoThumbDblClick   := nil;
  FOnProcessStageChanged := nil;
  
  FLayerBitmap.Free;
  FLayerThumb.Free;
  FMaskBitmap.Free;
  FMaskThumb.Free;
  FLogoBitmap.Free;
  FLogoThumb.Free;
  
  inherited;
end;

procedure TgmCustomLayer.Changed;
begin
  if Assigned(FOwner) then
  begin
    FOwner.BlendLayers;
  end; 

  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TgmCustomLayer.Changed(const ARect: TRect);
begin
  if Assigned(FOwner) then
  begin
    FOwner.BlendLayers(ARect);
  end;

  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TgmCustomLayer.CopyCommonDataTo(ADestLayer: TgmCustomLayer);
begin
  if Assigned(ADestLayer) then
  begin
    // layer part ...
    ADestLayer.LayerBitmap.Assign(Self.LayerBitmap);

    // mask part ...
    if Self.IsMaskEnabled then
    begin
      if not ADestLayer.IsMaskEnabled then
      begin
        ADestLayer.EnableMask();
      end;

      ADestLayer.MaskBitmap.Assign(Self.FMaskBitmap);
      ADestLayer.IsMaskLinked := Self.IsMaskLinked;
    end
    else
    begin
      if ADestLayer.IsMaskEnabled then
      begin
        ADestLayer.DiscardMask();
      end;
    end;

    // common properties ...
    ADestLayer.LayerProcessStage  := Self.LayerProcessStage;
    ADestLayer.LayerBlendMode     := Self.LayerBlendMode;
    ADestLayer.LayerOpacity       := Self.LayerOpacity;
    ADestLayer.IsLayerVisible     := Self.IsLayerVisible;
    ADestLayer.IsLockTransparency := Self.IsLockTransparency;
    ADestLayer.LayerOpacity       := Self.LayerOpacity;

    // common events ...
    ADestLayer.OnChange              := Self.OnChange;
    ADestLayer.OnLayerDisabled       := Self.OnLayerDisabled;
    ADestLayer.OnLayerEnabled        := Self.OnLayerEnabled;
    ADestLayer.OnMaskEnabled         := Self.OnMaskEnabled;
    ADestLayer.OnMaskDisabled        := Self.OnMaskDisabled;
    ADestLayer.OnMaskLinkChanged     := Self.OnMaskLinkChanged;
    ADestLayer.OnThumbnailUpdate     := Self.OnThumbnailUpdate;
    ADestLayer.OnPanelDblClick       := Self.OnPanelDblClick;
    ADestLayer.OnLayerThumbDblClick  := Self.OnLayerThumbDblClick;
    ADestLayer.OnMaskThumbDblClick   := Self.OnMaskThumbDblClick;
    ADestLayer.OnLogoThumbDblClick   := Self.OnLogoThumbDblClick;
    ADestLayer.OnProcessStageChanged := Self.OnProcessStageChanged;
  end;
end;

procedure TgmCustomLayer.CropMask(ACrop: TgmCrop);
var
  LCropRect   : TRect;
  LCropBitmap : TBitmap32;
begin
  if Assigned(ACrop) and Assigned(FMaskBitmap) then
  begin
    LCropBitmap := TBitmap32.Create();
    try
      LCropBitmap.DrawMode := dmOpaque;

      LCropRect := Rect(ACrop.FCropStart.X, ACrop.FCropStart.Y,
                        ACrop.FCropEnd.X, ACrop.FCropEnd.Y);

      CopyRect32WithARGB(LCropBitmap, FMaskBitmap, LCropRect, clWhite32);

      if ACrop.IsResized then
      begin
        if (LCropBitmap.Width  <> ACrop.ResizeW) or
           (LCropBitmap.Height <> ACrop.ResizeH) then
        begin
          SmoothResize32(LCropBitmap, ACrop.ResizeW, ACrop.ResizeH);
        end;
      end;

      FMaskBitmap.Assign(LCropBitmap);
      UpdateMaskThumbnail();
    finally
      LCropBitmap.Free();
    end;
  end;
end;

procedure TgmCustomLayer.CropMask(const ACropArea: TRect);
var
  LCropBitmap : TBitmap32;
  LCropRect   : TRect;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  if Assigned(FMaskBitmap) then
  begin
    LCropBitmap := TBitmap32.Create();
    try
      LCropBitmap.DrawMode := dmOpaque;

      CopyRect32WithARGB(LCropBitmap, FMaskBitmap, LCropRect, clWhite32);
      FMaskBitmap.Assign(LCropBitmap);
      UpdateMaskThumbnail();
    finally
      LCropBitmap.Free();
    end;
  end;
end;

// discard the mask settings, if any
function TgmCustomLayer.DiscardMask: Boolean;
begin
  Result := False;

  if FMaskEnabled then
  begin
    SetMaskEnabled(False);
    Self.Changed;

    if Assigned(FOnMaskDisabled) then
    begin
      FOnMaskDisabled(Self);
    end;

    Result := not FMaskEnabled;
  end;
end;

// enable mask, if it has not ...
function TgmCustomLayer.EnableMask: Boolean;
begin
  Result := False;

  if not FMaskEnabled then
  begin
    SetMaskEnabled(True);

    if Assigned(FOnMaskEnabled) then
    begin
      FOnMaskEnabled(Self);
    end;

    Result := FMaskEnabled;
  end;
end;

function TgmCustomLayer.GetLayerOpacity: Byte;
begin
  Result := FLayerBitmap.MasterAlpha and $FF;
end;

function TgmCustomLayer.GetRealThumbRect(
  const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer;
  const AMarginSize: Integer = 4): TRect;
var
  LThumbWidth  : Integer;
  LThumbHeight : Integer;
  LScale       : Single;
begin
  LScale := GetThumbZoomScale(ASrcWidth, ASrcHeight,
    AThumbWidth - AMarginSize, AThumbHeight - AMarginSize);

  LThumbWidth  := Round(ASrcWidth  * LScale);
  LThumbHeight := Round(ASrcHeight * LScale);

  with Result do
  begin
    Left   := (LAYER_THUMB_SIZE - LThumbWidth)  div 2;
    Top    := (LAYER_THUMB_SIZE - LThumbHeight) div 2;
    Right  := Left + LThumbWidth;
    Bottom := Top  + LThumbHeight;
  end;
end;

function TgmCustomLayer.GetThumbZoomScale(
  const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;
var
  ws, hs : Single;
begin
  if (ASrcWidth <= AThumbWidth) and (ASrcHeight <= AThumbHeight) then
  begin
    Result := 1.0;
  end
  else
  begin
    ws := AThumbWidth  / ASrcWidth;
    hs := AThumbHeight / ASrcHeight;

    if ws < hs then
    begin
      Result := ws;
    end
    else
    begin
      Result := hs;
    end;
  end;
end;

procedure TgmCustomLayer.InitMask;
begin
  if not Assigned(FMaskBitmap) then
  begin
    FMaskBitmap := TBitmap32.Create;
  end;

  with FMaskBitmap do
  begin
    SetSizeFrom(FLayerBitmap);
    Clear(clWhite32);
  end;

  if not Assigned(FMaskThumb) then
  begin
    FMaskThumb := TBitmap32.Create;
  end;

  with FMaskThumb do
  begin
    SetSize(LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
  end;

  UpdateMaskThumbnail;
end;

procedure TgmCustomLayer.LayerBlend(
  F: TColor32; var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmCustomLayer.ResizeMaskCanvas(const ANewWidth, ANewHeight: Integer;
  const AAnchor: TgmAnchorDirection);
var
  LCutBitmap : TBitmap32;
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if FMaskEnabled then
    begin
      LCutBitmap := TBitmap32.Create();
      try
        // resize layer's canvas
        CutBitmap32ByAnchorDirection(FMaskBitmap, LCutBitmap,
          ANewWidth, ANewHeight, AAnchor, $FFFFFFFF);

        FMaskBitmap.SetSize(ANewWidth, ANewHeight);
        FMaskBitmap.Clear($FFFFFFFF);
        DrawBitmap32ByAnchorDirection(LCutBitmap, FMaskBitmap, AAnchor);
        UpdateMaskThumbnail();
      finally
        LCutBitmap.Free();
      end;
    end;
  end;
end;

procedure TgmCustomLayer.RotateMaskCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection);
var
  LSrcBmp : TBitmap32;
begin
  if FMaskEnabled and Assigned(FMaskBitmap) then
  begin
    LSrcBmp := TBitmap32.Create();
    try
      LSrcBmp.Assign(FMaskBitmap);
      RotateBitmap32(LSrcBmp, FMaskBitmap, ADirection, ADegrees, 0, clWhite32);
      UpdateMaskThumbnail();
    finally
      LSrcBmp.Free();
    end;
  end;
end;

procedure TgmCustomLayer.SetLayerBlendMode(AValue: TBlendMode32);
begin
  if FLayerBlendMode <> AValue then
  begin
    FLayerBlendMode  := AValue;
    FLayerBlendEvent := GetBlendMode( Ord(FLayerBlendMode) );

    Changed;
  end;
end;

procedure TgmCustomLayer.SetLayerEnabled(AValue: Boolean);
begin
  if FLayerEnabled <> AValue then
  begin
    FLayerEnabled := AValue;

    if FLayerEnabled then
    begin
      if Assigned(FOnLayerEnabled) then
      begin
        FOnLayerEnabled(Self);
      end;
    end
    else
    begin
      FOnLayerDisabled(Self);
    end;
  end;
end;

procedure TgmCustomLayer.SetLayerOpacity(AValue: Byte);
begin
  if (FLayerBitmap.MasterAlpha and $FF) <> AValue then
  begin
    FLayerBitmap.MasterAlpha := AValue;
    Changed;
  end;
end;

procedure TgmCustomLayer.SetLayerProcessStage(
  AValue: TgmLayerProcessStage);
begin
  if FLayerProcessStage <> AValue then
  begin
    FLayerProcessStage := AValue;

    if Assigned(FOnProcessStageChanged) then
    begin
      FOnProcessStageChanged(Self, FLayerProcessStage);
    end;
  end;
end;

procedure TgmCustomLayer.SetLayerVisible(AValue: Boolean);
begin
  if FLayerVisible <> AValue then
  begin
    FLayerVisible := AValue;
    Changed;
  end;
end;

procedure TgmCustomLayer.SetMaskEnabled(AValue: Boolean);
begin
  if FMaskEnabled <> AValue then
  begin
    FMaskEnabled := AValue;

    if FMaskEnabled then
    begin
      FLayerProcessStage := lpsMask;
      FMaskLinked        := True;
      
      InitMask();
    end
    else
    begin
      FLayerProcessStage := lpsLayer;
      FMaskLinked        := False;
      
      FreeAndNil(FMaskBitmap);
      FreeAndNil(FMaskThumb);
    end;
  end;
end;

procedure TgmCustomLayer.SetMaskLinked(AValue: Boolean);
begin
  if FMaskLinked <> AValue then
  begin
    FMaskLinked := AValue;
    Changed;

    if Assigned(FOnMaskLinkChanged) then
    begin
      FOnMaskLinkChanged(Self, FMaskLinked);
    end;
  end;
end;

// throw away mask without callback function invoking
function TgmCustomLayer.SimplyDiscardMask: Boolean;
begin
  Result := False;

  if FMaskEnabled then
  begin
    SetMaskEnabled(False);
    Result := not FMaskEnabled;
  end;
end;

// enabling mask without callback function invoking
function TgmCustomLayer.SimplyEnableMask: Boolean;
begin
  Result := False;

  if not FMaskEnabled then
  begin
    SetMaskEnabled(True);
    Result := FMaskEnabled;
  end;
end;

procedure TgmCustomLayer.SimplySetLayerVisible(const AValue: Boolean);
begin
  FLayerVisible := AValue;
end;

procedure TgmCustomLayer.UpdateLayerThumbnail;
var
  LRect : TRect;
  LBmp  : TBitmap32;
begin
  LRect := FRealThumbRect;

  FLayerThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FLayerThumb, LRect, True);

  LBmp := TBitmap32.Create;
  try
    // The thumbnail should not shows the MasterAlpha settings of the layer.
    // The MasterAlpha only takes effect when layer blending.
    LBmp.Assign(FLayerBitmap);
    LBmp.MasterAlpha := 255;

    FLayerThumb.Draw(LRect, LBmp.BoundsRect, LBmp);
  finally
    LBmp.Free;
  end;

  InflateRect(LRect, 1, 1);
  FLayerThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

procedure TgmCustomLayer.UpdateLogoThumbnail;
var
  LRect : TRect;
begin
  LRect := GetRealThumbRect(FLogoBitmap.Width, FLogoBitmap.Height,
                            LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);

  FLogoThumb.Clear( Color32(clBtnFace) );
  FLogoThumb.Draw(LRect, FLogoBitmap.BoundsRect, FLogoBitmap);

  InflateRect(LRect, 1, 1);
  FLogoThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;

procedure TgmCustomLayer.UpdateMaskThumbnail;
var
  LRect : TRect;
begin
  LRect := FRealThumbRect;
  
  FMaskThumb.Clear( Color32(clBtnFace) );
  FMaskThumb.Draw(LRect, FMaskBitmap.BoundsRect, FMaskBitmap);

  InflateRect(LRect, 1, 1);
  FMaskThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end; 
end;


{ TgmNonPixelizedLayer }

constructor TgmNonPixelizedLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FPixelFeature := lpfNonPixelized;
end;

procedure TgmNonPixelizedLayer.CropLayer(ACrop: TgmCrop;
  const ABackColor: TColor32);
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if Assigned(ACrop) then
  begin
    LNewWidth  := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
    LNewHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);

    if ACrop.IsResized then
    begin
      LNewWidth  := ACrop.ResizeW;
      LNewHeight := ACrop.ResizeH;
    end;

    // resize layer
    FLayerBitmap.SetSize(LNewWidth, LNewHeight);

    FRealThumbRect := GetRealThumbRect(
      LNewWidth, LNewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

    // resize mask's canvas
    if FMaskEnabled then
    begin
      CropMask(ACrop);
    end;
  end;
end;

procedure TgmNonPixelizedLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  LCropRect  : TRect;
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  LNewWidth  := LCropRect.Right - LCropRect.Left;
  LNewHeight := LCropRect.Bottom - LCropRect.Top;

  // resize layer
  FLayerBitmap.SetSize(LNewWidth, LNewHeight);

  FRealThumbRect := GetRealThumbRect(
    LNewWidth, LNewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

  // resize mask's canvas
  if FMaskEnabled then
  begin
    CropMask(LCropRect);
  end;
end;

procedure TgmNonPixelizedLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      // resize layer
      FLayerBitmap.SetSize(ANewWidth, ANewHeight);

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
        
      // resize mask
      if FMaskEnabled then
      begin
        ResizeMaskCanvas(ANewWidth, ANewHeight, AAnchor);
      end;
    end;
  end;
end;

procedure TgmNonPixelizedLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      // resize layer
      FLayerBitmap.SetSize(ANewWidth, ANewHeight);

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
        
      // resize mask
      if FMaskEnabled then
      begin
        SmoothResize32(FMaskBitmap, ANewWidth, ANewHeight);
        UpdateMaskThumbnail();
      end;
    end;
  end;
end;

procedure TgmNonPixelizedLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LSrcBmp : TBitmap32;
begin
  LSrcBmp := TBitmap32.Create();
  try
    LSrcBmp.SetSizeFrom(FLayerBitmap);
    RotateBitmap32(LSrcBmp, FLayerBitmap, ADirection, ADegrees, 0, $00000000);

    FRealThumbRect := GetRealThumbRect(FLayerBitmap.Width, FLayerBitmap.Height,
                                       LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

    // rotate mask
    if FMaskEnabled then
    begin
      RotateMaskCanvas(ADegrees, ADirection);
    end;
  finally
    LSrcBmp.Free();
  end;
end;


{ TgmSpecialPixelizedLayer }

constructor TgmSpecialPixelizedLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FPixelFeature := lpfSpecialPixelized;
end;

procedure TgmSpecialPixelizedLayer.CropLayer(ACrop: TgmCrop;
  const ABackColor: TColor32);
var
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if Assigned(ACrop) then
  begin
    LNewWidth  := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
    LNewHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);

    if ACrop.IsResized then
    begin
      LNewWidth  := ACrop.ResizeW;
      LNewHeight := ACrop.ResizeH;
    end;
    
    if (LNewWidth > 0) and (LNewHeight > 0) then
    begin
      // resize layer
      FLayerBitmap.SetSize(LNewWidth, LNewHeight);

      FRealThumbRect := GetRealThumbRect(
        LNewWidth, LNewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

      // resize mask
      if FMaskEnabled then
      begin
        CropMask(ACrop);
      end;
    end;
  end;
end;

procedure TgmSpecialPixelizedLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  LCropRect  : TRect;
  LNewWidth  : Integer;
  LNewHeight : Integer;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  LNewWidth  := LCropRect.Right - LCropRect.Left;
  LNewHeight := LCropRect.Bottom - LCropRect.Top;

  // resize layer
  FLayerBitmap.SetSize(LNewWidth, LNewHeight);

  FRealThumbRect := GetRealThumbRect(
    LNewWidth, LNewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

  // resize mask's canvas
  if FMaskEnabled then
  begin
    CropMask(LCropRect);
  end;
end;

procedure TgmSpecialPixelizedLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      // resize layer
      FLayerBitmap.SetSize(ANewWidth, ANewHeight);

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
        
      // resize mask
      if FMaskEnabled then
      begin
        ResizeMaskCanvas(ANewWidth, ANewHeight, AAnchor);
      end;
    end;
  end;
end;

procedure TgmSpecialPixelizedLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      // resize layer
      FLayerBitmap.SetSize(ANewWidth, ANewHeight);

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
        
      // resize mask
      if FMaskEnabled then
      begin
        SmoothResize32(FMaskBitmap, ANewWidth, ANewHeight);
        UpdateMaskThumbnail();
      end;
    end;
  end;
end;

procedure TgmSpecialPixelizedLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LSrcBmp : TBitmap32;
begin
  LSrcBmp := TBitmap32.Create();
  try
    LSrcBmp.SetSizeFrom(FLayerBitmap);
    RotateBitmap32(LSrcBmp, FLayerBitmap, ADirection, ADegrees, 0, $00000000);

    FRealThumbRect := GetRealThumbRect(FLayerBitmap.Width, FLayerBitmap.Height,
                                       LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

    // rotate mask
    if FMaskEnabled then
    begin
      RotateMaskCanvas(ADegrees, ADirection);
    end;
  finally
    LSrcBmp.Free();
  end;
end;


{ TgmNormalLayer }

constructor TgmNormalLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer;
  const AFillColor: TColor32 = $00000000;
  const AsBackLayer: Boolean = False);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight, AFillColor);

  FPixelFeature      := lpfNormalPixelized;
  FAsBackground      := AsBackLayer;
  FDefaultLayerName  := 'Layer';
  FLayerThumbEnabled := True;

  if FAsBackground then
  begin
    FDefaultLayerName := 'Background';
    FLayerName        := FDefaultLayerName;
  end;

  FLayerThumb := TBitmap32.Create;
  with FLayerThumb do
  begin
    SetSize(LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
  end;

  UpdateLayerThumbnail;
end;

// applying the mask settings to the alpha channel of each pixel on the
// layer bitmap, and then disable the mask
function TgmNormalLayer.ApplyMask: Boolean;
var
  i           : Integer;
  a, m        : Cardinal;
  LLayerBits  : PColor32;
  LMaskBits   : PColor32;
  LMaskLinked : Boolean;
begin
  Result := False;

  if FMaskEnabled then
  begin
    LLayerBits := @FLayerBitmap.Bits[0];
    LMaskBits  := @FMaskBitmap.Bits[0];

    for i := 1 to (FLayerBitmap.Width * FLayerBitmap.Height) do
    begin
      m := LMaskBits^ and $FF;
      a := LLayerBits^ shr 24 and $FF;
      a := a * m div 255;

      LLayerBits^ := (a shl 24) or (LLayerBits^ and $FFFFFF);

      Inc(LLayerBits);
      Inc(LMaskBits);
    end;

    LMaskLinked := Self.FMaskLinked;  // remember the mask linked state for later use
    SetMaskEnabled(False);            // disable the mask first

    // if not link with mask, after disable the mask, we need to merge layer
    // to get new blending result, otherwise we don't need to do it, because
    // the current blending result is correct 
    if not LMaskLinked then
    begin
      if Assigned(FOwner) then
      begin
        FOwner.BlendLayers;
      end;
    end;

    UpdateLayerThumbnail;
    
    Result := not FMaskEnabled;

    if Assigned(FOnMaskApplied) then
    begin
      FOnMaskApplied(Self);
    end;
  end;
end;

procedure TgmNormalLayer.CropLayer(ACrop: TgmCrop; const ABackColor: TColor32);
var
  LBackColor  : TColor32;
  LCropBitmap : TBitmap32;
  LCropRect   : TRect;
begin
  if Assigned(ACrop) then
  begin
    if FAsBackground then
    begin
      LBackColor := ABackColor;
    end
    else
    begin
      LBackColor := $00FFFFFF;
    end;

    LCropBitmap := TBitmap32.Create();
    try
      LCropRect := Rect(ACrop.FCropStart.X, ACrop.FCropStart.Y,
                        ACrop.FCropEnd.X,   ACrop.FCropEnd.Y);

      CopyRect32WithARGB(LCropBitmap, FLayerBitmap, LCropRect, LBackColor);

      FLayerBitmap.SetSize(LCropBitmap.Width, LCropBitmap.Height);
      FLayerBitmap.Clear(LBackColor);
      CopyBitmap32(FLayerBitmap, LCropBitmap);
    finally
      LCropBitmap.Free();
    end;

    if ACrop.IsResized then
    begin
      if (ACrop.ResizeW > 0) and (ACrop.ResizeH > 0) then
      begin
        if (FLayerBitmap.Width <> ACrop.ResizeW) or
           (FLayerBitmap.Height <> ACrop.ResizeH) then
        begin
          SmoothResize32(FLayerBitmap, ACrop.ResizeW, ACrop.ResizeH);
        end;
      end;
    end;

    FRealThumbRect := GetRealThumbRect(
      FLayerBitmap.Width, FLayerBitmap.Height,
      LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

    UpdateLayerThumbnail();

    // resize mask's canvas
    if FMaskEnabled then
    begin
      CropMask(ACrop);
    end;
  end;
end;

procedure TgmNormalLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  LBackColor  : TColor32;
  LCropBitmap : TBitmap32;
  LCropRect   : TRect;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  if FAsBackground then
  begin
    LBackColor := ABackgroundColor;
  end
  else
  begin
    LBackColor := $00FFFFFF;
  end;

  LCropBitmap := TBitmap32.Create();
  try
    CopyRect32WithARGB(LCropBitmap, FLayerBitmap, LCropRect, LBackColor);

    FLayerBitmap.SetSize(LCropBitmap.Width, LCropBitmap.Height);
    FLayerBitmap.Clear(LBackColor);
    CopyBitmap32(FLayerBitmap, LCropBitmap);
  finally
    LCropBitmap.Free();
  end;

  FRealThumbRect := GetRealThumbRect(FLayerBitmap.Width, FLayerBitmap.Height,
                                     LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

  UpdateLayerThumbnail();

  // resize mask's canvas
  if FMaskEnabled then
  begin
    CropMask(LCropRect);
  end;
end;

function TgmNormalLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmNormalLayer;
begin
  LLayer := TgmNormalLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  Result := LLayer;
end;

// Determine whether the layer is an empty layer
// by checking the alpha component of each pixels on the layer.
//
//  True  = Empty (all the pixels with zero alpha value) 
//  False = Non-empty
function TgmNormalLayer.IsEmptyLayer: Boolean;
var
  i : Integer;
  a : Byte;
  p : PColor32;
begin
  Result := True;
  p      := @FLayerBitmap.Bits[0];

  for i := 1 to (FLayerBitmap.Width * FLayerBitmap.Height) do
  begin
    a := p^ shr 24 and $FF;

    if a > 0 then
    begin
      Result := False;
      Break;
    end;

    Inc(p);
  end;
end;

procedure TgmNormalLayer.ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
  const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32);
var
  LBackColor : TColor32;
  LCutBitmap : TBitmap32;
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      if FAsBackground then
      begin
        LBackColor := ABackgroundColor;
      end
      else
      begin
        LBackColor := $00FFFFFF;
      end;

      LCutBitmap := TBitmap32.Create();
      try
        LCutBitmap.DrawMode := dmBlend;
        
        // resize layer's canvas
        CutBitmap32ByAnchorDirection(FLayerBitmap, LCutBitmap,
          ANewWidth, ANewHeight, AAnchor, LBackColor);

        FLayerBitmap.SetSize(ANewWidth, ANewHeight);
        FLayerBitmap.Clear(LBackColor);
        DrawBitmap32ByAnchorDirection(LCutBitmap, FLayerBitmap, AAnchor);

      finally
        LCutBitmap.Free();
      end;

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);

      UpdateLayerThumbnail();

      // resize mask's canvas
      if FMaskEnabled then
      begin
        ResizeMaskCanvas(ANewWidth, ANewHeight, AAnchor);
      end;
    end;
  end;
end;

procedure TgmNormalLayer.ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerBitmap.Width) or
       (ANewHeight <> FLayerBitmap.Height) then
    begin
      // resize layer
      ExecuteResample(FLayerBitmap, ANewWidth, ANewHeight, ASamplingOptions);

      FRealThumbRect := GetRealThumbRect(
        ANewWidth, ANewHeight, LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
                                     
      UpdateLayerThumbnail();

      // resize mask
      if FMaskEnabled then
      begin
        SmoothResize32(FMaskBitmap, ANewWidth, ANewHeight);
        UpdateMaskThumbnail();
      end;
    end;
  end;
end;

procedure TgmNormalLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  LSrcBmp : TBitmap32;
begin
  LSrcBmp := TBitmap32.Create();
  try
    LSrcBmp.Assign(FLayerBitmap);

    if FAsBackground then
    begin
      RotateBitmap32(LSrcBmp, FLayerBitmap, ADirection, ADegrees, 0, ABackgroundColor);
    end
    else
    begin
      RotateBitmap32(LSrcBmp, FLayerBitmap, ADirection, ADegrees, 0, $00FFFFFF);
    end;

    FRealThumbRect := GetRealThumbRect(FLayerBitmap.Width, FLayerBitmap.Height,
                                       LAYER_THUMB_SIZE, LAYER_THUMB_SIZE);
                                     
    UpdateLayerThumbnail();

    // rotate mask
    if FMaskEnabled then
    begin
      RotateMaskCanvas(ADegrees, ADirection);
    end;

  finally
    LSrcBmp.Free();
  end;
end;

// no callback function calling version of ApplyMask()
function TgmNormalLayer.SimplyApplyMask: Boolean;
var
  i          : Integer;
  a, m       : Cardinal;
  LLayerBits : PColor32;
  LMaskBits  : PColor32;
begin
  Result := False;

  if FMaskEnabled then
  begin
    LLayerBits := @FLayerBitmap.Bits[0];
    LMaskBits  := @FMaskBitmap.Bits[0];

    for i := 1 to (FLayerBitmap.Width * FLayerBitmap.Height) do
    begin
      m := LMaskBits^ and $FF;
      a := LLayerBits^ shr 24 and $FF;
      a := a * m div 255;

      LLayerBits^ := (a shl 24) or (LLayerBits^ and $FFFFFF);

      Inc(LLayerBits);
      Inc(LMaskBits);
    end;

    SetMaskEnabled(False);  // disable the mask first
    UpdateLayerThumbnail();
    
    Result := not FMaskEnabled;
  end;
end;

{procedure TgmNormalLayer.UpdateLayerThumbnail;
var
  i         : Integer;
  a, m      : byte;
  LRect     : TRect;
  LBmp      : TBitmap32;
  LForeBits : PColor32;
  LMaskBits : PColor32;
begin
  LRect := FRealThumbRect;

  FLayerThumb.Clear( Color32(clBtnFace) );
  DrawCheckerboardPattern(FLayerThumb, LRect, True);

  LBmp := TBitmap32.Create;
  try
    // The thumbnail should not shows the MasterAlpha settings of the layer.
    // The MasterAlpha only takes effect when layer blending.
    LBmp.Assign(FLayerBitmap);
    LBmp.MasterAlpha := 255;

    // apply mask if it is linked to the layer
    if FMaskEnabled and FMaskLinked then
    begin
      LForeBits := @LBmp.Bits[0];
      LMaskBits := @FMaskBitmap.Bits[0];

      for i := 1 to (LBmp.Width * LBmp.Height) do
      begin
        m := LMaskBits^ and $FF;
        a := LForeBits^ shr 24 and $FF;
        a := Clamp( a - (255 - m), 0, 255);

        LForeBits^ := (a shl 24) or (LForeBits^ and $FFFFFF);

        Inc(LForeBits);
        Inc(LMaskBits);
      end;
    end;

    FLayerThumb.Draw(LRect, LBmp.BoundsRect, LBmp);
  finally
    LBmp.Free;
  end;

  InflateRect(LRect, 1, 1);
  FLayerThumb.FrameRectS(LRect, clBlack32);

  if Assigned(FOnThumbUpdate) then
  begin
    FOnThumbUpdate(Self);
  end;
end;    }

{ TgmLayerList }

constructor TgmLayerList.Create;
begin
  inherited;

  FSelectedLayer        := nil;
  FOnLayerCombined      := nil;
  FOnSelectionChanged   := nil;
  FOnLayerOrderChanged  := nil;
  FOnMergeLayerDown     := nil;
  FOnMergeVisibleLayers := nil;
  FOnFlattenLayers      := nil;

  FItems            := TObjectList.Create(True);
  FLayerTypeCounter := TgmClassCounter.Create();

  FCombineResult := TBitmap32.Create();
  with FCombineResult do
  begin
    DrawMode := dmBlend;
  end;
end;

destructor TgmLayerList.Destroy;
begin
  FItems.Clear();
  FItems.Free();
  FCombineResult.Free();
  FLayerTypeCounter.Free();
  
  inherited;
end;

procedure TgmLayerList.Add(ALayer: TgmCustomLayer);
begin
  if Assigned(ALayer) then
  begin
    FItems.Add(ALayer);

    // we don't count background layers
    if ALayer is TgmNormalLayer then
    begin
      if not TgmNormalLayer(ALayer).IsAsBackground then
      begin
        FLayerTypeCounter.Increase(ALayer.ClassName);
      end;
    end
    else
    begin
      FLayerTypeCounter.Increase(ALayer.ClassName);
    end;

    // first adding
    if FItems.Count = 1 then
    begin
      FLayerWidth  := ALayer.FLayerBitmap.Width;
      FLayerHeight := ALayer.FLayerBitmap.Height;
      
      FCombineResult.SetSize(FLayerWidth, FLayerHeight);
    end;

    BlendLayers;
    SelectLayer(FItems.Count - 1);

    if not FSelectedLayer.IsDuplicated then
    begin
      SetLayerInitialName(FSelectedLayer);
    end;
  end;
end;

procedure TgmLayerList.BlendLayers;
var
  i, j        : Integer;
  LPixelCount : Integer;
  m           : Cardinal;
  LLayer      : TgmCustomLayer;
  LForeBits   : PColor32;
  LBackBits   : PColor32;
  LMaskBits   : PColor32;
begin
  LMaskBits := nil;

  FCombineResult.BeginUpdate;
  try
    FCombineResult.Clear($00000000);

    if FItems.Count > 0 then
    begin
      LPixelCount := FLayerWidth * FLayerHeight;

      for i := 0 to (FItems.Count - 1) do
      begin
        LLayer := GetLayer(i);

        if not LLayer.IsLayerVisible then
        begin
          Continue;
        end;

        LForeBits := @LLayer.FLayerBitmap.Bits[0];
        LBackBits := @FCombineResult.Bits[0];

        if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
        begin
          LMaskBits := @LLayer.FMaskBitmap.Bits[0];
        end;

        for j := 1 to LPixelCount do
        begin
          m := LLayer.FLayerBitmap.MasterAlpha;

          if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
          begin
            // adjust the MasterAlpha with Mask setting
            m := m * (LMaskBits^ and $FF) div 255;
          end;

          LLayer.LayerBlend(LForeBits^, LBackBits^, m);

          Inc(LForeBits);
          Inc(LBackBits);
          
          if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
          begin
            Inc(LMaskBits);
          end;
        end;
      end;
    end;

  finally
    FCombineResult.EndUpdate;
  end;

  if Assigned(FOnLayerCombined) then
  begin
    FOnLayerCombined( Self, Rect(0, 0, FLayerWidth, FLayerHeight) );
  end;
end;

procedure TgmLayerList.BlendLayers(const ARect: TRect);
var
  LRect        : TRect;
  i            : Integer;
  x, y, xx, yy : Integer;
  LRectWidth   : Integer;
  LRectHeight  : Integer;
  m            : Cardinal;
  LLayer       : TgmCustomLayer;
  LResultRow   : PColor32Array;
  LLayerRow    : PColor32Array;
  LMaskRow     : PColor32Array;
begin
{$RANGECHECKS OFF}

  LMaskRow := nil;

  LRect.Left   := Math.Min(ARect.Left, ARect.Right);
  LRect.Right  := Math.Max(ARect.Left, ARect.Right);
  LRect.Top    := Math.Min(ARect.Top, ARect.Right);
  LRect.Bottom := Math.Max(ARect.Top, ARect.Bottom);
  
  if (LRect.Left = LRect.Right) or
     (LRect.Top = LRect.Bottom) or
     (LRect.Left > FLayerWidth) or
     (LRect.Top > FLayerHeight) or
     (LRect.Right <= 0) or
     (LRect.Bottom <= 0) then
  begin
    Exit;
  end;

  LRectWidth  := LRect.Right - LRect.Left + 1;
  LRectHeight := LRect.Bottom - LRect.Top + 1;

  FCombineResult.BeginUpdate;
  try
    FCombineResult.FillRectS(LRect, $00000000);

    if FItems.Count > 0 then
    begin
      for y := 0 to (LRectHeight - 1) do
      begin
        yy := y + LRect.Top;

        if (yy < 0) or (yy >= FLayerHeight) then
        begin
          Continue;
        end;

        // get entries of one line pixels on the background bitmap ...
        LResultRow := FCombineResult.ScanLine[yy];

        for i := 0 to (FItems.Count - 1) do
        begin
          LLayer := GetLayer(i);

          if not LLayer.IsLayerVisible then
          begin
            Continue;
          end;

          // get entries of one line pixels on each layer bitmap ...
          LLayerRow := LLayer.FLayerBitmap.ScanLine[yy];

          if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
          begin
            // get entries of one line pixels on each layer mask bitmap, if any ...
            LMaskRow := LLayer.FMaskBitmap.ScanLine[yy];
          end;

          for x := 0 to (LRectWidth - 1) do
          begin
            xx := x + LRect.Left;

            if (xx < 0) or (xx >= FLayerWidth) then
            begin
              Continue;
            end;

            // blending ...
            m := LLayer.FLayerBitmap.MasterAlpha;

            if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
            begin
              // adjust the MasterAlpha with Mask setting
              m := m * (LMaskRow[xx] and $FF) div 255;
            end;

            LLayer.LayerBlend(LLayerRow[xx], LResultRow[xx], m);
          end;
        end;
      end;
    end;

  finally
    FCombineResult.EndUpdate;
  end;

  if Assigned(FOnLayerCombined) then
  begin
    FOnLayerCombined(Self, LRect);
  end;
     
{$RANGECHECKS ON}
end;

function TgmLayerList.CanFlattenLayers: Boolean;
begin
  Result := False;

  if FItems.Count > 0 then
  begin
    if FItems.Count = 1 then
    begin
      if Self.SelectedLayer is TgmNormalLayer then
      begin
        // If the only layer is a Normal layer but not as background layer,
        // we could flatten it as a background layer
        Result := not TgmNormalLayer(Self.SelectedLayer).IsAsBackground;
      end;
    end
    else
    begin
      // we could flatten layers as long as the numnber of layers
      // is greater than one
      Result := True;
    end;
  end;
end;

function TgmLayerList.CanMergeSelectedLayerDown: Boolean;
var
  LPrevIndex : Integer;
  LPrevLayer : TgmCustomLayer;
begin
  Result     := False;
  LPrevIndex := Self.SelectedIndex - 1;

  if IsValidIndex(LPrevIndex) then
  begin
    LPrevLayer := Self.Layers[LPrevIndex];

    // can only merge down to a visible Normal layer
    Result := FSelectedLayer.IsLayerVisible and
              LPrevLayer.IsLayerVisible and
              (LPrevLayer.PixelFeature = lpfNormalPixelized);
  end;
end;

function TgmLayerList.CanMergeVisbleLayers: Boolean;
begin
  Result := FSelectedLayer.IsLayerVisible and
            (GetVisibleNormalLayerCount() > 0) and (GetVisbileLayerCount() > 1);
end;

// This method is similar to DeleteLayer(), but it will also
// modifys the statistics in Layer Type Counter.
procedure TgmLayerList.CancelLayer(AIndex: Integer);
var
  LLayer : TgmCustomLayer;
begin
  if (FItems.Count = 1) or ( not IsValidIndex(AIndex) ) then
  begin
    Exit;
  end;

  LLayer := Self.Layers[AIndex];
  Self.FLayerTypeCounter.Decrease(LLayer.ClassName);

  DeleteLayer(AIndex);
end;

procedure TgmLayerList.Clear;
begin
  if FItems.Count > 0 then
  begin
    FItems.Clear();
    FSelectedLayer := nil;
  end;
end;

procedure TgmLayerList.CropLayers(ACrop: TgmCrop; const ABackColor: TColor32);
var
  i          : Integer;
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LLayer     : TgmCustomLayer;
begin
  if not Assigned(ACrop) then
  begin
    Exit;
  end;

  LNewWidth  := Abs(ACrop.FCropEnd.X - ACrop.FCropStart.X);
  LNewHeight := Abs(ACrop.FCropEnd.Y - ACrop.FCropStart.Y);

  if (LNewWidth > 0) and (LNewHeight > 0) then
  begin
    if ACrop.IsResized then
    begin
      FLayerWidth  := ACrop.ResizeW;
      FLayerHeight := ACrop.ResizeH;
    end
    else
    begin
      FLayerWidth  := LNewWidth;
      FLayerHeight := LNewHeight;
    end;

    FCombineResult.SetSize(FLayerWidth, FLayerHeight);

    if FItems.Count > 0 then
    begin
      for i := 0 to (FItems.Count - 1) do
      begin
        LLayer := TgmCustomLayer(FItems.Items[i]);
        LLayer.CropLayer(ACrop, ABackColor);
      end;
    end;
  end;
end;

procedure TgmLayerList.CropLayers(const ACropArea: TRect;
  const ABackColor: TColor32);
var
  i          : Integer;
  LNewWidth  : Integer;
  LNewHeight : Integer;
  LLayer     : TgmCustomLayer;
begin
  LNewWidth  := Abs(ACropArea.Right - ACropArea.Left);
  LNewHeight := Abs(ACropArea.Bottom - ACropArea.Top);

  if (LNewWidth > 0) and (LNewHeight > 0) then
  begin
    FLayerWidth  := LNewWidth;
    FLayerHeight := LNewHeight;

    FCombineResult.SetSize(FLayerWidth, FLayerHeight);

    if FItems.Count > 0 then
    begin
      for i := 0 to (FItems.Count - 1) do
      begin
        LLayer := TgmCustomLayer(FItems.Items[i]);
        LLayer.CropLayerRect(ACropArea, ABackColor);
      end;
    end;
  end;
end;

procedure TgmLayerList.DeleteLayer(AIndex: Integer);
begin
  if (FItems.Count = 1) or ( not IsValidIndex(AIndex) ) then
  begin
    Exit;
  end;

  FSelectedLayer := nil;

  FItems.Delete(AIndex);
  BlendLayers;

  // select the previous layer ...

  AIndex := AIndex - 1;

  if AIndex < 0 then
  begin
    AIndex := 0;
  end;

  SelectLayer(AIndex);
end;

procedure TgmLayerList.DeleteSelectedLayer;
var
  LIndex : Integer;
begin
  LIndex := GetSelectedLayerIndex;
  DeleteLayer(LIndex);
end;

procedure TgmLayerList.DeleteVisibleLayers;
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if FItems.Count > 0 then
  begin
    for i := (FItems.Count - 1) downto 0 do
    begin
      LLayer := Self.Layers[i];

      if LLayer.IsLayerVisible then
      begin
        FItems.Delete(i);
      end;
    end;
  end;
end;

procedure TgmLayerList.DeselectAllLayers;
var
  i : Integer;
begin
  if FItems.Count > 0 then
  begin
    Self.FSelectedLayer := nil;

    for i := 0 to (FItems.Count - 1) do
    begin
      // NOTICE :
      //   Setting with field FSelected, not with property Selected,
      //   for avoiding the setter of property be invoked.
      GetLayer(i).FSelected := False;
    end;
  end;
end;

function TgmLayerList.FlattenLayers: Boolean;
var
  LBackLayer : TgmCustomLayer;
begin
  Result := CanFlattenLayers;
  
  if Result then
  begin
    LBackLayer := TgmNormalLayer.Create(Self, FLayerWidth, FLayerHeight, clWhite32, True);

    with LBackLayer do
    begin
      // Note that, if the background layer has special properties as the one
      // in Photoshop, we should draw the combined result onto a white
      // background. But for now, we haven't figure out how to do the same
      // thing as PS, so we just make the combined result as the background
      // layer.
      
      //LayerBitmap.Draw(0, 0, FCombineResult);
      LayerBitmap.Assign(FCombineResult);
      UpdateLayerThumbnail;
    end;

    FItems.Clear;
    FLayerTypeCounter.Clear;
    Self.Add(LBackLayer);
    Self.SelectLayer(0);

    if Assigned(FOnFlattenLayers) then
    begin
      FOnFlattenLayers(Self.FSelectedLayer);
    end;
  end;
end;

procedure TgmLayerList.FlattenLayersToBitmapWithoutMask(ADestBmp: TBitmap32;
  const ABackgroundColor: TColor32 = $00FFFFFF);
var
  i, j        : Integer;
  LPixelCount : Integer;
  m           : Cardinal;
  LLayer      : TgmCustomLayer;
  LForeBits   : PColor32;
  LBackBits   : PColor32;
begin
  if not Assigned(ADestBmp) then
  begin
    Exit;
  end;

  ADestBmp.SetSize(FLayerWidth, FLayerHeight);

  ADestBmp.BeginUpdate();
  try
    ADestBmp.Clear(ABackgroundColor);

    if FItems.Count > 0 then
    begin
      LPixelCount := FLayerWidth * FLayerHeight;

      for i := 0 to (FItems.Count - 1) do
      begin
        LLayer := GetLayer(i);

        if not LLayer.IsLayerVisible then
        begin
          Continue;
        end;

        LForeBits := @LLayer.FLayerBitmap.Bits[0];
        LBackBits := @ADestBmp.Bits[0];

        for j := 1 to LPixelCount do
        begin
          m := LLayer.FLayerBitmap.MasterAlpha;

          LLayer.LayerBlend(LForeBits^, LBackBits^, m);

          Inc(LForeBits);
          Inc(LBackBits);
        end;
      end;
    end;

  finally
    ADestBmp.EndUpdate();
  end;
end;

// find a normal layer whose AsBackground set to true
function TgmLayerList.GetBackgroundLayer: TgmNormalLayer;
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  Result := nil;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LLayer := Self.Layers[i];

      if LLayer is TgmNormalLayer then
      begin
        if TgmNormalLayer(LLayer).IsAsBackground then
        begin
          Result := TgmNormalLayer(LLayer);
          Break;
        end;
      end;
    end
  end;
end;

function TgmLayerList.GetHiddenLayerCount: Integer;
var
  i : Integer;
begin
  Result := 0;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if not Self.Layers[i].IsLayerVisible then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

function TgmLayerList.GetLayerBlendResult(
  const AMaskAvailable: Boolean): TBitmap32;
var
  i, j        : Integer;
  LPixelCount : Integer;
  m           : Cardinal;
  LLayer      : TgmCustomLayer;
  LForeBits   : PColor32;
  LBackBits   : PColor32;
  LMaskBits   : PColor32;
begin
  Result    := nil;
  LMaskBits := nil;

  if FItems.Count > 0 then
  begin
    Result             := TBitmap32.Create;
    Result.DrawMode    := dmBlend;
    Result.CombineMode := cmMerge;

    Result.BeginUpdate;
    try
      Result.SetSize(FLayerWidth, FLayerHeight);
      Result.Clear($00000000);

      LPixelCount := FLayerWidth * FLayerHeight;

      for i := 0 to (FItems.Count - 1) do
      begin
        LLayer := GetLayer(i);

        if not LLayer.IsLayerVisible then
        begin
          Continue;
        end;

        LForeBits := @LLayer.FLayerBitmap.Bits[0];
        LBackBits := @Result.Bits[0];

        if AMaskAvailable then
        begin
          if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
          begin
            LMaskBits := @LLayer.FMaskBitmap.Bits[0];
          end;
        end;

        for j := 1 to LPixelCount do
        begin
          m := LLayer.FLayerBitmap.MasterAlpha;

          if AMaskAvailable then
          begin
            if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
            begin
              // adjust the MasterAlpha with Mask setting
              m := m * (LMaskBits^ and $FF) div 255;
            end;
          end;

          LLayer.LayerBlend(LForeBits^, LBackBits^, m);

          Inc(LForeBits);
          Inc(LBackBits);

          if AMaskAvailable then
          begin
            if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
            begin
              Inc(LMaskBits);
            end;
          end;
        end;
      end;

    finally
      Result.EndUpdate;
    end;
  end;
end;

function TgmLayerList.GetLayerBlendResult(const AIndex1, AIndex2: Integer;
  const AMaskAvailable: Boolean): TBitmap32;
var
  i, j        : Integer;
  LPixelCount : Integer;
  m           : Cardinal;
  LLayer      : TgmCustomLayer;
  LForeBits   : PColor32;
  LBackBits   : PColor32;
  LMaskBits   : PColor32;
begin
  if not IsValidIndex(AIndex1) then
  begin
    raise Exception.Create('[Error]TgmLayerList.GetLayerBlendResult(): AIndex1 out of the range.' );
  end;

  if not IsValidIndex(AIndex2) then
  begin
    raise Exception.Create('[Error]TgmLayerList.GetLayerBlendResult(): AIndex2 out of the range.' );
  end;

  if AIndex1 > AIndex2 then
  begin
    raise Exception.Create('[Error]TgmLayerList.GetLayerBlendResult(): AIndex1 should not be greater than AIndex2.' );
  end;

  Result    := nil;
  LMaskBits := nil;

  if FItems.Count > 0 then
  begin
    Result             := TBitmap32.Create;
    Result.DrawMode    := dmBlend;
    Result.CombineMode := cmMerge;

    Result.BeginUpdate;
    try
      Result.SetSize(FLayerWidth, FLayerHeight);
      Result.Clear($00000000);

      LPixelCount := FLayerWidth * FLayerHeight;

      for i := AIndex1 to AIndex2 do
      begin
        LLayer := GetLayer(i);

        if not LLayer.IsLayerVisible then
        begin
          Continue;
        end;

        LForeBits := @LLayer.FLayerBitmap.Bits[0];
        LBackBits := @Result.Bits[0];

        if AMaskAvailable then
        begin
          if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
          begin
            LMaskBits := @LLayer.FMaskBitmap.Bits[0];
          end;
        end;

        for j := 1 to LPixelCount do
        begin
          m := LLayer.FLayerBitmap.MasterAlpha;

          if AMaskAvailable then
          begin
            if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
            begin
              // adjust the MasterAlpha with Mask setting
              m := m * (LMaskBits^ and $FF) div 255;
            end;
          end;

          LLayer.LayerBlend(LForeBits^, LBackBits^, m);

          Inc(LForeBits);
          Inc(LBackBits);

          if AMaskAvailable then
          begin
            if LLayer.IsMaskEnabled and LLayer.IsMaskLinked then
            begin
              Inc(LMaskBits);
            end;
          end;
        end;
      end;

    finally
      Result.EndUpdate;
    end;
  end;
end;

function TgmLayerList.GetLayer(AIndex: Integer): TgmCustomLayer;
begin
  Result := nil;

  if IsValidIndex(AIndex) then
  begin
    Result := TgmCustomLayer(FItems.Items[AIndex]);
  end;
end;

function TgmLayerList.GetLayerCount: Integer;
begin
  Result := FItems.Count;
end;

function TgmLayerList.GetLayerCounter(ALayerClassName: ShortString): Integer;
begin
  Result := FLayerTypeCounter.GetCount(ALayerClassName);
end;

// get count of layers that are of the given type in the list
function TgmLayerList.GetLayerCounter(ALayerType: TgmCustomLayerClass): Integer;
begin
  Result := FLayerTypeCounter.GetCount(ALayerType.ClassName);
end;

function TgmLayerList.GetLayerMaxIndex: Integer;
begin
  Result := FItems.Count - 1;
end;

function TgmLayerList.GetSelectedLayerIndex: Integer;
var
  i : Integer;
begin
  Result := -1;

  if (FItems.Count > 0) and Assigned(FSelectedLayer) then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if FSelectedLayer = Self.Layers[i] then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// TODO: Perhaps need to rename this function
// to 'GetVisibleNormalPixelizedLayerCount'
function TgmLayerList.GetVisibleNormalLayerCount: Integer;
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  Result := 0;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LLayer := Self.Layers[i];

      if LLayer.IsLayerVisible and
         (LLayer.PixelFeature = lpfNormalPixelized) then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

function TgmLayerList.GetVisbileLayerCount: Integer;
var
  i : Integer;
begin
  Result := 0;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if Self.Layers[i].IsLayerVisible then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

procedure TgmLayerList.Insert(AIndex: Integer; ALayer: TgmCustomLayer);
begin
  if Assigned(ALayer) then
  begin
    AIndex := Clamp(AIndex, 0, FItems.Count);
    FItems.Insert(AIndex, ALayer);
    
    // we don't count background layers
    if ALayer is TgmNormalLayer then
    begin
      if (not TgmNormalLayer(ALayer).IsAsBackground) and
         (not ALayer.IsDuplicated) then
      begin
        FLayerTypeCounter.Increase(ALayer.ClassName);
      end;
    end
    else
    begin
      // don't increase the counter if the inserted layer is an duplicated one
      if not ALayer.IsDuplicated then
      begin
        FLayerTypeCounter.Increase(ALayer.ClassName);
      end;
    end;
    
    BlendLayers;
    SelectLayer(AIndex);

    if not FSelectedLayer.IsDuplicated then
    begin
      SetLayerInitialName(FSelectedLayer);
    end;
  end;
end;

function TgmLayerList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

function TgmLayerList.MergeSelectedLayerDown: Boolean;
var
  i             : Integer;
  m             : Cardinal;
  LMaskEffected : Boolean;
  LPrevIndex    : Integer;
  LPrevLayer    : TgmCustomLayer;
  LForeBits     : PColor32;
  LBackBits     : PColor32;
  LMaskBits     : PColor32;
begin
  Result := CanMergeSelectedLayerDown;

  LMaskEffected := False;
  LMaskBits     := nil;

  if Result then
  begin
    LPrevIndex := SelectedIndex - 1;
    LPrevLayer := Self.Layers[LPrevIndex];

    LForeBits := @FSelectedLayer.FLayerBitmap.Bits[0];
    LBackBits := @LPrevLayer.FLayerBitmap.Bits[0];

    if (FSelectedLayer.IsMaskEnabled) and (FSelectedLayer.IsMaskLinked) then
    begin
      LMaskBits     := @FSelectedLayer.FMaskBitmap.Bits[0];
      LMaskEffected := True;
    end;

    for i := 1 to (FLayerWidth * FLayerHeight) do
    begin
      m := FSelectedLayer.FLayerBitmap.MasterAlpha;
      
      if LMaskEffected then
      begin
        // adjust the MasterAlpha with Mask setting
        m := m * (LMaskBits^ and $FF) div 255;
      end;

      FSelectedLayer.LayerBlend(LForeBits^, LBackBits^, m);

      Inc(LForeBits);
      Inc(LBackBits);

      if LMaskEffected then
      begin
        Inc(LMaskBits);
      end;
    end;

    LPrevLayer.UpdateLayerThumbnail();

    // this routine will make the previous layer be selected automatically
    DeleteSelectedLayer();
    BlendLayers();

    if Assigned(FOnMergeLayerDown) then
    begin
      FOnMergeLayerDown(Self.FSelectedLayer);
    end;
  end;
end;

function TgmLayerList.MergeVisibleLayers: Boolean;
var
  LMergedLayer  : TgmCustomLayer;
  LAsBackground : Boolean;
begin
  Result := Self.CanMergeVisbleLayers;

  if Result then
  begin
    LAsBackground := False;
    
    if FSelectedLayer is TgmNormalLayer then
    begin
      LAsBackground := TgmNormalLayer(FSelectedLayer).FAsBackground;
    end;

    LMergedLayer := TgmNormalLayer.Create(Self,
      FLayerWidth, FLayerHeight, $00000000, LAsBackground);

    with LMergedLayer do
    begin
      FLayerBitmap.Assign(FCombineResult);
      UpdateLayerThumbnail;

      FLayerName := FSelectedLayer.FLayerName;
    end;
    
    DeleteVisibleLayers;
    FSelectedLayer := nil;

    FItems.Insert(0, LMergedLayer);
    Self.SelectLayer(0);

    if Assigned(FOnMergeVisibleLayers) then
    begin
      FOnMergeVisibleLayers(Self.FSelectedLayer);
    end;
  end;
end;

procedure TgmLayerList.Move(ACurIndex, ANewIndex: Integer);
begin
  if IsValidIndex(ACurIndex) and
     IsValidIndex(ANewIndex) and
     (ACurIndex <> ANewIndex) then
  begin
    FItems.Move(ACurIndex, ANewIndex);
    BlendLayers;

    if Assigned(FOnLayerOrderChanged) then
    begin
      FOnLayerOrderChanged(Self, ACurIndex, ANewIndex);
    end;
  end;
end;

procedure TgmLayerList.ResizeCanvasOfLayers(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerWidth) or (ANewHeight <> FLayerHeight) then
    begin
      FLayerWidth  := ANewWidth;
      FLayerHeight := ANewHeight;

      FCombineResult.SetSize(FLayerWidth, FLayerHeight);

      if FItems.Count > 0 then
      begin
        for i := 0 to (FItems.Count - 1) do
        begin
          LLayer := TgmCustomLayer(FItems.Items[i]);

          LLayer.ResizeLayerCanvas(ANewWidth, ANewHeight,
                                   AAnchor, ABackgroundColor);
        end;
      end;
    end;
  end;
end;

procedure TgmLayerList.ResizeImageOfLayers(const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (ANewWidth <> FLayerWidth) or (ANewHeight <> FLayerHeight) then
    begin
      FLayerWidth  := ANewWidth;
      FLayerHeight := ANewHeight;

      FCombineResult.SetSize(FLayerWidth, FLayerHeight);

      if FItems.Count > 0 then
      begin
        for i := 0 to (FItems.Count - 1) do
        begin
          LLayer := TgmCustomLayer(FItems.Items[i]);
          LLayer.ResizeLayerImage(ANewWidth, ANewHeight, ASamplingOptions);
        end;
      end;
    end;
  end;
end;

procedure TgmLayerList.RotateCanvasOfLayers(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
var
  i      : Integer;
  LLayer : TgmCustomLayer;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LLayer := TgmCustomLayer(FItems.Items[i]);

      LLayer.RotateCanvas(ADegrees, ADirection, ABackgroundColor);
    end;

    // after rotating, we could get new layer width and height
    LLayer := TgmCustomLayer(FItems.Items[0]);

    if (FLayerWidth <> LLayer.LayerBitmap.Width) or
       (FLayerHeight <> LLayer.LayerBitmap.Height) then
    begin
      FLayerWidth  := LLayer.LayerBitmap.Width;
      FLayerHeight := LLayer.LayerBitmap.Height;

      FCombineResult.SetSize(FLayerWidth, FLayerHeight);
    end;
  end;
end;

procedure TgmLayerList.SelectLayer(const AIndex: Integer);
var
  LLayer : TgmCustomLayer;
begin
  LLayer := GetLayer(AIndex);

  if Assigned(LLayer) then
  begin
    if FSelectedLayer <> LLayer then
    begin
      DeselectAllLayers;

      FSelectedLayer           := LLayer;
      FSelectedLayer.FSelected := True;

      if Assigned(FOnSelectionChanged) then
      begin
        FOnSelectionChanged(Self);
      end;
    end;

    // always enable the layer when it is selected
    FSelectedLayer.IsLayerEnabled := True;
  end;
end;

procedure TgmLayerList.SetLayerCounter(ALayerClassName: ShortString;
  const ACount: Integer);
begin
  FLayerTypeCounter.SetCount(ALayerClassName, ACount);
end;

// set counter for a given layer type 
procedure TgmLayerList.SetLayerCounter(ALayerType: TgmCustomLayerClass;
  const ACount: Integer);
begin
  FLayerTypeCounter.SetCount(ALayerType.ClassName, ACount);
end;

procedure TgmLayerList.SetLayerInitialName(ALayer: TgmCustomLayer);
var
  LNumber : Integer;
begin
  if Assigned(ALayer) then
  begin
    if ALayer is TgmNormalLayer then
    begin
      if TgmNormalLayer(ALayer).IsAsBackground then
      begin
        Exit;
      end;
    end;

    LNumber          := FLayerTypeCounter.GetCount(ALayer.ClassName);
    ALayer.LayerName := ALayer.FDefaultLayerName + ' ' + IntToStr(LNumber);
  end;
end;

// This procedure does the similar thing as the Add() procedure above,
// but it won't blend layers, invoke callback functions, etc.
// It simply adds a layer to the layer list.
procedure TgmLayerList.SimplyAdd(ALayer: TgmCustomLayer);
begin
  if Assigned(ALayer) then
  begin
    FItems.Add(ALayer);
    
    // first adding
    if FItems.Count = 1 then
    begin
      FLayerWidth  := ALayer.FLayerBitmap.Width;
      FLayerHeight := ALayer.FLayerBitmap.Height;
      
      FCombineResult.SetSize(FLayerWidth, FLayerHeight);
    end;
  end;
end;

procedure TgmLayerList.SimplyDeleteLayer(AIndex: Integer);
begin
  if (FItems.Count > 1) and IsValidIndex(AIndex) then
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TgmLayerList.SimplyInsert(AIndex: Integer; ALayer: TgmCustomLayer);
begin
  if Assigned(ALayer) then
  begin
    AIndex := Clamp(AIndex, 0, FItems.Count);
    FItems.Insert(AIndex, ALayer);
  end;
end;

// no callbacks invoking version of Move()
procedure TgmLayerList.SimplyMove(ACurIndex, ANewIndex: Integer);
begin
  if IsValidIndex(ACurIndex) and
     IsValidIndex(ANewIndex) and
     (ACurIndex <> ANewIndex) then
  begin
    FItems.Move(ACurIndex, ANewIndex);
  end;
end;

procedure TgmLayerList.SimplySelectLayer(const AIndex: Integer);
var
  LLayer : TgmCustomLayer;
begin
  LLayer := GetLayer(AIndex);

  if Assigned(LLayer) then
  begin
    if FSelectedLayer <> LLayer then
    begin
      DeselectAllLayers;

      FSelectedLayer           := LLayer;
      FSelectedLayer.FSelected := True;
    end;

    // always enable the layer when it is selected
    FSelectedLayer.IsLayerEnabled := True;
  end;
end;

{ TgmClassRec }

constructor TgmClassRec.Create(const AClassName: ShortString);
begin
  inherited Create;

  FName  := AClassName;
  FCount := 1;
end;

{ TgmClassCounter }

constructor TgmClassCounter.Create;
begin
  inherited;

  FItems := TObjectList.Create();
end;

destructor TgmClassCounter.Destroy;
begin
  FItems.Clear();
  FItems.Free();

  inherited;
end;

procedure TgmClassCounter.Clear;
begin
  FItems.Clear();
end;

// This method will decrease the number of a class name in the counter.
procedure TgmClassCounter.Decrease(const AClassName: ShortString);
var
  LIndex : Integer;
  LRec   : TgmClassRec;
begin
  if AClassName = '' then
  begin
    Exit;
  end;

  LIndex := Self.GetIndex(AClassName);

  if Self.IsValidIndex(LIndex) then
  begin
    LRec       := TgmClassRec(FItems.Items[LIndex]);
    LRec.Count := LRec.Count - 1;

    if LRec.Count = 0 then
    begin
      FItems.Delete(LIndex);
    end;
  end;
end;

function TgmClassCounter.GetCount(const AClassName: ShortString): Integer;
var
  i    : Integer;
  LRec : TgmClassRec;
begin
  Result := 0;

  if AClassName = '' then
  begin
    Exit;
  end;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LRec := TgmClassRec(FItems.Items[i]);

      if AClassName = LRec.Name then
      begin
        Result := LRec.Count;
        Break;
      end;
    end;
  end;
end;

function TgmClassCounter.GetIndex(const AClassName: ShortString): Integer;
var
  i    : Integer;
  LRec : TgmClassRec;
begin
  Result := -1;

  if AClassName = '' then
  begin
    Exit;
  end;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LRec := TgmClassRec(FItems.Items[i]);

      if AClassName = LRec.Name then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// This method will increase the number of a class name in the counter.
procedure TgmClassCounter.Increase(const AClassName: ShortString);
var
  LIndex : Integer;
  LRec   : TgmClassRec;
begin
  if AClassName = '' then
  begin
    Exit;
  end;

  LIndex := Self.GetIndex(AClassName);

  if Self.IsValidIndex(LIndex) then
  begin
    LRec       := TgmClassRec(FItems.Items[LIndex]);
    LRec.Count := LRec.Count + 1;
  end
  else
  begin
    LRec := TgmClassRec.Create(AClassName);
    FItems.Add(LRec);
  end;
end;

function TgmClassCounter.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

procedure TgmClassCounter.SetCount(const AClassName: ShortString;
  const ACount: Integer);
var
  LIndex : Integer;
  LRec   : TgmClassRec;
begin
  if (AClassName = '') and (ACount >= 0) then
  begin
    Exit;
  end;

  LIndex := Self.GetIndex(AClassName);

  if Self.IsValidIndex(LIndex) then
  begin
    LRec       := TgmClassRec(FItems.Items[LIndex]);
    LRec.Count := ACount;
  end
  else
  begin
    LRec       := TgmClassRec.Create(AClassName);
    LRec.Count := ACount;
    
    FItems.Add(LRec);
  end;
end;


end.
