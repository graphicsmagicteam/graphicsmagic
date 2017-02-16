////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradients.pas
// Version:    1.04
// History:    1.00 18 Feb 2005 created
//             1.01 26 Mar 2005 restructured: color / opacity
//                              definitions / keys encapsulated,
//                              noise and other features added,
//                              better interpolation implemented,
//                              alpha channel value for drawing added,
//                              update mechanism enhanced by
//                              introducing a change type,
//                              streaming capabilities added
//             1.02 07 Jul 2005 spiral, helical and tunnel style added,
//                              inner loops optimized
//             1.03 14 Nov 2005 symmetrical noise adjusted
//             1.04 28 Feb 2006 memory leaks removed
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradient is a color gradient defined by color and opacity keys
// and/or a perlin noise function. Supports linear and cubic interpolation.
// The gradient can either be defined in RGB or HSB color space.
// There are methods to return the gradient colors (fill a color array of any
// size) and to draw a rectangular gradient to a bitmap, supporting different
// styles and optional dithering. TPegtopColorGradient also supports streaming,
// can be a property of a component, and can be save to / loaded from a file.
////////////////////////////////////////////////////////////////////////////////
// Usage:
// A gradient can be created with following constructor:
// TPegtopColorGradient.Create([clBlack, clBlue, clWhite]);
// Single color keys can be added / modified like this:
// with MyGradient.Color.Keys.Add do begin
//   Position := 500; // 0..1000
//   Color := clYellow;
// end;
// MyGradient.Opacity.Keys[0].Opacity := 128; // 0..256
// A gradient can be drawn like this (most simple case):
// MyGradient.Draw(ACanvas, ARect, Point1, Point2);
// MyGradient.Draw(ACanvas, ARect, Point1, Point2, pgsRadial, True,
//   [pgoReverse, pgoSymmetrical], 2);
// (especially for 32 bit bitmaps there are more efficient ways, e.g. Draw32)
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradients;

interface

uses
  Windows, Classes, SysUtils, Graphics,
  PegtopColorUtils, PegtopChunkFiles;

type
  TPegtopColorArray = array of TColor;
  TPegtopOpacityArray = array of Integer;

  TPegtopColor64Array = array of TPegtopColor64;
  TPegtopNoiseArray = array of Integer;

  TPegtopColorGradientStyle = (pgsLinear, pgsLinearRepetitive, pgsRadial, pgsRadialRepetitive,
    pgsPolar, pgsDiamond, pgsDiamondRepetitive, pgsStar, pgsTapered, pgsTaperedRepetitive,
    pgsSpherical, pgsSpiralClockwise, pgsSpiralAntiClockwise, pgsTunnel, pgsHelicalClockwise, pgsHelicalAntiClockwise);
  TPegtopColorGradientOption = (pgoReverse, pgoSymmetrical, pgoIgnoreOpacity, pgoIgnoreNoise);
  TPegtopColorGradientOptions = set of TPegtopColorGradientOption;
  TPegtopColorGradientMode = (pgmRGB, pgmHSB);
  TPegtopColorGradientChange = (pgcProperty, pgcDefinition, pgcKeyCount, pgcKeyPosition, pgcKeyAttribute, pgcNoise);
  TPegtopColorGradientChanges = set of TPegtopColorGradientChange;
  TPegtopColorKeyComponent = (pckHue, pckSaturation, pckBrightness, pckRed, pckGreen, pckBlue);

  TPegtopCustomColorGradient = class;
  TPegtopColorGradientDefinition = class;
  TPegtopGradientKeyList = class;

  TPegtopGradientKey = class(TPersistent)
  private
    FOwner: TPegtopGradientKeyList;
    FPosition: Integer;
    FEnabled: Boolean;
    procedure SetPosition(Value: Integer);
    procedure SetEnabled(Value: Boolean);
  public
    constructor Create(AOwner: TPegtopGradientKeyList); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Owner: TPegtopGradientKeyList read FOwner;
  published
    property Position: Integer read FPosition write SetPosition;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TPegtopColorKey = class(TPegtopGradientKey)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
  public
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property Color: TColor read FColor write SetColor;
  end;

  TPegtopOpacityKey = class(TPegtopGradientKey)
  private
    FOpacity: Integer;
    procedure SetOpacity(Value: Integer);
  public
    constructor Create(AOwner: TPegtopGradientKeyList); override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property Opacity: Integer read FOpacity write SetOpacity;
  end;

  TPegtopColorGradientNoise = class(TPersistent)
  private
    FOwner: TPegtopColorGradientDefinition;
    FStrength: Integer;
    FFrequency: Integer;
    FRoughness: Integer;
    FRandomKey: Integer;
    procedure SetStrength(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetRoughness(Value: Integer);
    procedure SetRandomKey(Value: Integer);
  protected
    procedure GetPerlinNoise(var NoiseArray: TPegtopNoiseArray; Channel, Iterations: Integer);
  public
    constructor Create(AOwner: TPegtopColorGradientDefinition); virtual;
    procedure Clear; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Owner: TPegtopColorGradientDefinition read FOwner;
  published
    property Strength: Integer read FStrength write SetStrength default 0;
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property Roughness: Integer read FRoughness write SetRoughness default 128;
    property RandomKey: Integer read FRandomKey write SetRandomKey default 0;
  end;

  TPegtopColorGradientColorNoise = class(TPegtopColorGradientNoise)
  private
    FRedMin: Integer;
    FRedMax: Integer;
    FGreenMin: Integer;
    FGreenMax: Integer;
    FBlueMin: Integer;
    FBlueMax: Integer;
    FHueAvg: Integer;
    FHueRange: Integer;
    FSaturationMin: Integer;
    FSaturationMax: Integer;
    FBrightnessMin: Integer;
    FBrightnessMax: Integer;
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure SetRedMin(Value: Integer);
    procedure SetRedMax(Value: Integer);
    procedure SetGreenMin(Value: Integer);
    procedure SetGreenMax(Value: Integer);
    procedure SetBlueMin(Value: Integer);
    procedure SetBlueMax(Value: Integer);
    procedure SetHueAvg(Value: Integer);
    procedure SetHueRange(Value: Integer);
    procedure SetSaturationMin(Value: Integer);
    procedure SetSaturationMax(Value: Integer);
    procedure SetBrightnessMin(Value: Integer);
    procedure SetBrightnessMax(Value: Integer);
  public
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ApplyRGB(var ColorArray: TPegtopColorArray;
      const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
    procedure ApplyHSB(var ColorArray: TPegtopColorArray;
      const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
    procedure ApplyRGB64(var ColorArray: TPegtopColor64Array;
      const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
    procedure ApplyHSB64(var ColorArray: TPegtopColor64Array;
      const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
  published
    property RedMin: Integer read FRedMin write SetRedMin default 0;
    property RedMax: Integer read FRedMax write SetRedMax default 255;
    property GreenMin: Integer read FGreenMin write SetGreenMin default 0;
    property GreenMax: Integer read FGreenMax write SetGreenMax default 255;
    property BlueMin: Integer read FBlueMin write SetBlueMin default 0;
    property BlueMax: Integer read FBlueMax write SetBlueMax default 255;
    property HueAvg: Integer read FHueAvg write SetHueAvg default 0;
    property HueRange: Integer read FHueRange write SetHueRange default 65536;
    property SaturationMin: Integer read FSaturationMin write SetSaturationMin default 0;
    property SaturationMax: Integer read FSaturationMax write SetSaturationMax default 65536;
    property BrightnessMin: Integer read FBrightnessMin write SetBrightnessMin default 0;
    property BrightnessMax: Integer read FBrightnessMax write SetBrightnessMax default 65536;
  end;

  TPegtopColorGradientOpacityNoise = class(TPegtopColorGradientNoise)
  private
    FOpacityMin: Integer;
    FOpacityMax: Integer;
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure SetOpacityMin(Value: Integer);
    procedure SetOpacityMax(Value: Integer);
  public
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
  published
    property OpacityMin: Integer read FOpacityMin write SetOpacityMin default 0;
    property OpacityMax: Integer read FOpacityMax write SetOpacityMax default 256;
  end;

  TPegtopColorGradientInterpolateLinearEvent = procedure (Data: Pointer; Index: Integer;
    Key1, Key2: TPegtopGradientKey; Rate, Max: Integer) of object;

  TPegtopColorGradientInterpolateCubicEvent = procedure (Data: Pointer; Index: Integer;
    Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer) of object;

  TPegtopGradientKeyList = class
  private
    FOwner: TPegtopColorGradientDefinition;
    FKeys: TList;
    FUpdateChanges: TPegtopColorGradientChanges;
    function GetCount: Integer;
  protected
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer;
      ContainerChunkReader: TPegtopContainerChunkReader); virtual; abstract;
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean); virtual; abstract;
    function AddGradientKey: TPegtopGradientKey; virtual; abstract;
    procedure BlurKey(Key, PreviousKey, NextKey: TPegtopGradientKey; Intensity: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TPegtopColorGradientDefinition); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPegtopGradientKeyList);
    procedure Merge(Source: TPegtopGradientKeyList); virtual; abstract;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure ApplyChanges;
    procedure Update(Changes: TPegtopColorGradientChanges);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    function IndexOf(Key: TPegtopGradientKey): Integer;
    procedure Delete(Index: Integer);
    function Remove(Key: TPegtopGradientKey): Integer;
    procedure DistributeEvenly;
    procedure Flip;
    procedure Invert; virtual; abstract;
    procedure Blur(Intensity: Integer);
    procedure InterpolateLinear(Data: Pointer; Size: Integer;
      Options: TPegtopColorGradientOptions; Iterations: Integer; Max: Integer;
      OnInterpolate: TPegtopColorGradientInterpolateLinearEvent);
    procedure InterpolateCubic(Data: Pointer; Size: Integer;
      Options: TPegtopColorGradientOptions; Iterations: Integer; Max: Integer;
      OnInterpolate: TPegtopColorGradientInterpolateCubicEvent);
    property Count: Integer read GetCount;
    property Changes: TPegtopColorGradientChanges read FUpdateChanges;
  end;

  TPegtopGradientColorKeyList = class(TPegtopGradientKeyList)
  private
    function GetKey(Index: Integer): TPegtopColorKey;
    procedure SetKey(Index: Integer; Value: TPegtopColorKey);
  protected
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer;
      ContainerChunkReader: TPegtopContainerChunkReader); override;
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean); override;
    function AddGradientKey: TPegtopGradientKey; override;
    procedure BlurKey(Key, PreviousKey, NextKey: TPegtopGradientKey; Intensity: Integer); override;
  public
    procedure Define(const AColors: array of TColor);
    procedure Merge(Source: TPegtopGradientKeyList); override;
    function Add: TPegtopColorKey;
    procedure UnifyColorComponents(Color: TColor; ColorComponent: TPegtopColorComponent);
    procedure Invert; override;
    property Keys[Index: Integer]: TPegtopColorKey read GetKey write SetKey; default;
  end;

  TPegtopGradientColorKeyListClass = class of TPegtopGradientColorKeyList;

  TPegtopGradientOpacityKeyList = class(TPegtopGradientKeyList)
  private
    function GetKey(Index: Integer): TPegtopOpacityKey;
    procedure SetKey(Index: Integer; Value: TPegtopOpacityKey);
  protected
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer;
      ContainerChunkReader: TPegtopContainerChunkReader); override;
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean); override;
    function AddGradientKey: TPegtopGradientKey; override;
    procedure BlurKey(Key, PreviousKey, NextKey: TPegtopGradientKey; Intensity: Integer); override;
  public
    procedure Define(const AOpacities: array of Integer);
    procedure Merge(Source: TPegtopGradientKeyList); override;
    function Add: TPegtopOpacityKey;
    procedure Invert; override;
    property Keys[Index: Integer]: TPegtopOpacityKey read GetKey write SetKey; default;
  end;

  TPegtopColorGradientDefinition = class(TPersistent)
  private
    FOwner: TPegtopCustomColorGradient;
    FKeys: TPegtopGradientKeyList;
    FFrequency: Integer;
    FSmoothness: Integer;
    procedure SetFrequency(Value: Integer);
    procedure SetSmoothness(Value: Integer);
  protected
    function CreateKeys: TPegtopGradientKeyList; virtual; abstract;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure ApplyChanges;
    procedure Update(Changes: TPegtopColorGradientChanges);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Owner: TPegtopCustomColorGradient read FOwner;
  published
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property Smoothness: Integer read FSmoothness write SetSmoothness default 256;
  end;

  TPegtopColorGradientColor = class(TPegtopColorGradientDefinition)
  private
    FMode: TPegtopColorGradientMode;
    FNoise: TPegtopColorGradientColorNoise;
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure InterpolateLinearRGB(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearBGR(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearRGB64(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearBGR64(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearHSB(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearBSH(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearHSB64(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateLinearBSH64(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicRGB(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicBGR(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicRGB64(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicBGR64(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicHSB(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicBSH(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicHSB64(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicBSH64(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure SetMode(Value: TPegtopColorGradientMode);
    procedure SetNoise(Value: TPegtopColorGradientColorNoise);
    function GetKeys: TPegtopGradientColorKeyList;
    procedure SetKeys(Value: TPegtopGradientColorKeyList);
  protected
    function CreateKeys: TPegtopGradientKeyList; override;
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure GetColors(var ColorArray: TPegtopColorArray;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
      const SwapRedBlue: Boolean = False);
    procedure GetColors64(var ColorArray: TPegtopColor64Array;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
      const SwapRedBlue: Boolean = False);
  published
    property Mode: TPegtopColorGradientMode read FMode write SetMode default pgmRGB;
    property Noise: TPegtopColorGradientColorNoise read FNoise write SetNoise;
    property Keys: TPegtopGradientColorKeyList read GetKeys write SetKeys;
  end;

  TPegtopColorGradientOpacity = class(TPegtopColorGradientDefinition)
  private
    FNoise: TPegtopColorGradientOpacityNoise;
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
      Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure InterpolateLinearOpacity(Data: Pointer; Index: Integer;
      Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
    procedure InterpolateCubicOpacity(Data: Pointer; Index: Integer;
      Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
    procedure SetNoise(Value: TPegtopColorGradientOpacityNoise);
    function GetKeys: TPegtopGradientOpacityKeyList;
    procedure SetKeys(Value: TPegtopGradientOpacityKeyList);
  protected
    function CreateKeys: TPegtopGradientKeyList; override;
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure GetOpacities(var OpacityArray: TPegtopOpacityArray;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1; Max: Integer = 256);
  published
    property Noise: TPegtopColorGradientOpacityNoise read FNoise write SetNoise;
    property Keys: TPegtopGradientOpacityKeyList read GetKeys write SetKeys;
  end;

  TPegtopCustomColorGradient = class(TPersistent)
  private
    FName: String;
    FSeamless: Boolean;
    FColor: TPegtopColorGradientColor;
    FOpacity: TPegtopColorGradientOpacity;
    FUpdateLevel: Integer;
    FUpdateChanges: TPegtopColorGradientChanges;
    FListeners: TList;
    FOnChange: TNotifyEvent;
    procedure LoadOldXFaderFormat(Stream: TStream);
    procedure RaiseInvalidStream;
    procedure ChunkReadData(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean);
    procedure SetName(Value: String);
    procedure SetSeamless(Value: Boolean);
    procedure SetColor(Value: TPegtopColorGradientColor);
    procedure SetOpacity(Value: TPegtopColorGradientOpacity);
    procedure NotifyListeners;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(const AColors: array of TColor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream);
    procedure DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
    
    procedure LoadFromStream(Stream: TStream; Index: Integer = 0);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String; Index: Integer = 0);
    procedure Update(Changes: TPegtopColorGradientChanges);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddListener(Event: TNotifyEvent);
    procedure RemoveListener(Event: TNotifyEvent);
    procedure Draw32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure DrawDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure Blend32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure BlendDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure GrayscaleOpacity32(const TransparentColor, OpaqueColor: TPegtopColor;
      const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure GrayscaleOpacityDithered32(const TransparentColor, OpaqueColor: TPegtopColor;
      const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
      const Alpha: Byte = 0);
    procedure Draw(const Canvas: TCanvas; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
      const Iterations: Integer = 1);
    procedure Blend(const Canvas: TCanvas; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
      const Iterations: Integer = 1);
    procedure GrayscaleOpacity(const TransparentColor, OpaqueColor: TColor;
      const Canvas: TCanvas; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
      const Iterations: Integer = 1);
    property Changes: TPegtopColorGradientChanges read FUpdateChanges;
  published
    property Name: String read FName write SetName;
    property Seamless: Boolean read FSeamless write SetSeamless;
    property Color: TPegtopColorGradientColor read FColor write SetColor;
    property Opacity: TPegtopColorGradientOpacity read FOpacity write SetOpacity;
  end;

  TPegtopColorGradient = class(TPegtopCustomColorGradient)
  published
    property OnChange;
  end;

  TPegtopNotifyListener = class
  private
    FEvent: TNotifyEvent;
  public
    constructor Create(AEvent: TNotifyEvent);
    property Event: TNotifyEvent read FEvent;
  end;

const
  PegtopColorGradientPositionRange = 1000;

implementation

uses
  PegtopColorGradientLists, PegtopInterpolationUtils;

type
  TPegtopGradientRecord = packed record
    Name: String[31];
    Seamless: Longbool;
  end;

  TPegtopColorRecord = packed record
    Frequency: Longint;
    Smoothness: Longint;
    Mode: Longint;
  end;

  TPegtopOpacityRecord = packed record
    Frequency: Longint;
    Smoothness: Longint;
  end;

  TPegtopColorNoiseRecord = packed record
    Strength: Longint;
    Frequency: Longint;
    Roughness: Longint;
    RandomKey: Longint;
    RedMin: Longint;
    RedMax: Longint;
    GreenMin: Longint;
    GreenMax: Longint;
    BlueMin: Longint;
    BlueMax: Longint;
    HueAvg: Longint;
    HueRange: Longint;
    SaturationMin: Longint;
    SaturationMax: Longint;
    BrightnessMin: Longint;
    BrightnessMax: Longint;
  end;

  TPegtopOpacityNoiseRecord = packed record
    Strength: Longint;
    Frequency: Longint;
    Roughness: Longint;
    RandomKey: Longint;
    OpacityMin: Longint;
    OpacityMax: Longint;
  end;

  TPegtopColorKeyRecord = packed record
    Position: Longint;
    Reserved: packed array[0..7] of Byte;
    Color: Longint;
  end;

  TPegtopOpacityKeyRecord = packed record
    Position: Longint;
    Reserved: packed array[0..7] of Byte;
    Opacity: Longint;
  end;

const
  GradientCollectionChunkId: TPegtopChunkId = 'XGRC';
  GradientChunkId:           TPegtopChunkId = 'XGRD';
  GradientDataChunkId:       TPegtopChunkId = 'GDAT';
  ColorChunkId:              TPegtopChunkId = 'CDEF';
  OpacityChunkId:            TPegtopChunkId = 'ODEF';
  ColorDataChunkId:          TPegtopChunkId = 'CDAT';
  OpacityDataChunkId:        TPegtopChunkId = 'ODAT';
  ColorNoiseChunkId:         TPegtopChunkId = 'CNOI';
  OpacityNoiseChunkId:       TPegtopChunkId = 'ONOI';
  ColorNoiseDataChunkId:     TPegtopChunkId = 'CNDT';
  OpacityNoiseDataChunkId:   TPegtopChunkId = 'ONDT';
  KeysChunkId:               TPegtopChunkId = 'KEYS';
  ColorKeyChunkId:           TPegtopChunkId = 'CKEY';
  OpacityKeyChunkId:         TPegtopChunkId = 'OKEY';

  GradientArrayBits = 12;
  GradientArraySize = (1 shl GradientArrayBits); // (for drawing gradients) must be a power of 2 !!!
  GradientArrayMask = GradientArraySize - 1;

var
  DitherTable: array[0..15, 0..15] of Byte;

////////////////////////////////////////////////////////////////////////////////
// math functions
////////////////////////////////////////////////////////////////////////////////

function PartArcTan(Y, X: Extended): Extended;
asm
  fld Y              // st(0) = Y
  fld X              // st(0) = X
  fpatan             // st(0) = ArcTan(Y, X)
  fwait
end;

function ArcSin(X: Extended): Extended; // -1 <= X <= 1
asm
  fld X               // st(0) = X
  fld st(0)           // st(1) = X
  fmul st(0), st(0)   // st(0) = Sqr(X)
  fld1                // st(0) = 1
  fsubrp st(1), st(0) // st(0) = 1 - Sqr(X)
  fsqrt               // st(0) = Sqrt(1 - Sqr(X))
  fpatan              // st(0) = ArcTan(X, Sqrt(1 - X*X))
  fwait
end;

function Ln2(X: Extended): Extended;
asm
  fld1
  fld X
  fyl2x
  fwait
end;

////////////////////////////////////////////////////////////////////////////////
// interpolation
////////////////////////////////////////////////////////////////////////////////

function InterpolateSmooth(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Smoothness, X: Integer): Integer;
var
  A1, A2: Integer;
begin
  A1 := 0 * (Y2 - Y0);
  A2 := 0 * (Y3 - Y1);
  Result := InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X);
  if Smoothness < 256 then
    Result := Result + ((Y1 + (Y2 - Y1) * X div 256) - Result) * (256 - Smoothness) div 256;
end;

function InterpolateSmoothSaturated(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Smoothness, X: Integer; Min, Max: Integer): Integer;
var
  A1, A2: Integer;
begin
  A1 := 0 * (Y2 - Y0);
  A2 := 0 * (Y3 - Y1);
  Result := InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X);
  if Smoothness < 256 then
    Result := Result + ((Y1 + (Y2 - Y1) * X div 256) - Result) * (256 - Smoothness) div 256;
  if Result < Min then Result := Min
  else if Result > Max then Result := Max;
end;

procedure GetHueDiff(const Sat1, Sat2: Integer; var Hue1, Hue2: Integer; out HueDiff: Integer);
begin
  if Sat1 = 0 then Hue1 := Hue2
  else if Sat2 = 0 then Hue2 := Hue1;
  HueDiff := (Hue2 - Hue1) and HueMask;
  if HueDiff > (HueMask shr 1) then HueDiff := -((Hue1 - Hue2) and HueMask);
end;

////////////////////////////////////////////////////////////////////////////////
// dither procedures
////////////////////////////////////////////////////////////////////////////////

procedure InitDither;
  function FindDitherX(I: Integer): Integer;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to 3 do
      Result := Result or ((((((I shr (J * 2)) and 3) + 1) and 2) shr 1) shl (3 - J));
  end;
  function FindDitherY(I: Integer): Integer;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to 3 do
      Result := Result or (((I shr (J * 2)) and 1) shl (3 - J));
  end;
var
  X, Y, I: Integer;
begin
  for Y := 0 to 15 do begin
    for X := 0 to 15 do begin
      I := Y * 16 + X;
      DitherTable[FindDitherX(I), FindDitherY(I)] := I;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// string functions
////////////////////////////////////////////////////////////////////////////////

function ExtractFileNameWithoutExt(const S: String): String;
begin
  Result := ExtractFileName(S);
  Result := Copy(Result, 1, Length(Result) - Length(ExtractFileExt(Result)));
end;

////////////////////////////////////////////////////////////////////////////////
// compare functions
////////////////////////////////////////////////////////////////////////////////

function KeySortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TPegtopGradientKey(Item1).Position - TPegtopGradientKey(Item2).Position;
end;

////////////////////////////////////////////////////////////////////////////////
// noise functions
////////////////////////////////////////////////////////////////////////////////

function RandomNoise(X: Integer): Integer; overload;
begin
  Result := X * (X * X * 15731 + 789221) + 1376312589;
end;

function RandomNoise(Key, Channel, Octave, X: Integer): Integer; overload;
begin
  Result := (RandomNoise((Key + Channel * 17 + Octave) * 11 + X) and $FFFF) - $8000;
end;

////////////////////////////////////////////////////////////////////////////////
// drawing procedures
////////////////////////////////////////////////////////////////////////////////

procedure DrawColors32(const Colors: TPegtopColorArray; const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
const
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  Offset: Integer;
  Vector: TPoint;
  Ratio: Double;
  Dist, Ang: Double;
  CosAng, SinAng: Double;
  CosY, SinY: Double;
  AlphaColor: TColor;
begin
  // calculate gradient vector:
  Vector.X := Point2.X - Point1.X;
  Vector.Y := Point2.Y - Point1.Y;
  if (Vector.X = 0) and (Vector.Y = 0) then begin
    Vector.Y := 1;
  end;
  AlphaColor := Alpha shl 24;
  for X := 0 to Length(Colors) - 1 do Colors[X] := Colors[X] or AlphaColor;
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              P^.Def := Colors[Offset];
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              P^.Def := Colors[Offset];
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsLinearRepetitive:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X)) and GradientArrayMask;
              P^.Def := Colors[Offset];
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y)) and GradientArrayMask;
              P^.Def := Colors[Offset];
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsRadialRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist) and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsPolar:
      begin
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamondRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist) and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsStar:
      begin
        Dist := 1.0 / ((Sqr(Vector.X) + Sqr(Vector.Y)) * 0.5);
        Ang := PartArcTan(Vector.Y, Vector.X) + (Pi/4);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * Abs((CosAng * Vector.X + SinY) * (SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTapered:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTaperedRepetitive:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y))) and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpherical:
      begin
        Ratio := GradientArraySize * (2/Pi);
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ang := Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist;
            if Ang < 1 then Offset := Trunc(Ratio * ArcSin(Ang))
            else Offset := GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian));
            Offset := Offset and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * ((PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian + Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist));
            Offset := Offset and GradientArrayMask;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTunnel:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio);
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) + (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure DrawColorsDithered32(const Colors: TPegtopColor64Array; const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
const
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor64;
  Offset, Dithering: Integer;
  Vector: TPoint;
  Ratio: Double;
  Dist, Ang: Double;
  CosAng, SinAng: Double;
  CosY, SinY: Double;
begin
  // calculate gradient vector:
  Vector.X := Point2.X - Point1.X;
  Vector.Y := Point2.Y - Point1.Y;
  if (Vector.X = 0) and (Vector.Y = 0) then begin
    Vector.Y := 1;
  end;
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := Alpha;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := Alpha;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsLinearRepetitive:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X)) and GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := Alpha;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y)) and GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := Alpha;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsRadialRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsPolar:
      begin
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamondRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsStar:
      begin
        Dist := 1.0 / ((Sqr(Vector.X) + Sqr(Vector.Y)) * 0.5);
        Ang := PartArcTan(Vector.Y, Vector.X) + (Pi/4);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * Abs((CosAng * Vector.X + SinY) * (SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTapered:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTaperedRepetitive:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y))) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpherical:
      begin
        Ratio := GradientArraySize * (2/Pi);
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ang := Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist;
            if Ang < 1 then Offset := Trunc(Ratio * ArcSin(Ang))
            else Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian));
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * ((PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian + Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist));
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTunnel:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio);
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) + (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := Alpha;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure BlendColors32(const Colors: TPegtopColorArray; const Opacities: TPegtopOpacityArray;
  const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
const
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor;
  O: Integer;
  Offset: Integer;
  Vector: TPoint;
  Ratio: Double;
  Dist, Ang: Double;
  CosAng, SinAng: Double;
  CosY, SinY: Double;
begin
  // calculate gradient vector:
  Vector.X := Point2.X - Point1.X;
  Vector.Y := Point2.Y - Point1.Y;
  if (Vector.X = 0) and (Vector.Y = 0) then begin
    Vector.Y := 1;
  end;
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsLinearRepetitive:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X)) and GradientArrayMask;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y)) and GradientArrayMask;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsRadialRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist) and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsPolar:
      begin
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamondRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist) and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsStar:
      begin
        Dist := 1.0 / ((Sqr(Vector.X) + Sqr(Vector.Y)) * 0.5);
        Ang := PartArcTan(Vector.Y, Vector.X) + (Pi/4);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * Abs((CosAng * Vector.X + SinY) * (SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTapered:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTaperedRepetitive:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y))) and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpherical:
      begin
        Ratio := GradientArraySize * (2/Pi);
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ang := Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist;
            if Ang < 1 then Offset := Trunc(Ratio * ArcSin(Ang))
            else Offset := GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian));
            Offset := Offset and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * ((PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian + Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist));
            Offset := Offset and GradientArrayMask;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTunnel:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio);
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) + (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure BlendColorsDithered32(const Colors: TPegtopColor64Array; const Opacities: TPegtopOpacityArray;
  const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
const
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor64;
  O: Integer;
  Offset, Dithering: Integer;
  Vector: TPoint;
  Ratio: Double;
  Dist, Ang: Double;
  CosAng, SinAng: Double;
  CosY, SinY: Double;
begin
  // calculate gradient vector:
  Vector.X := Point2.X - Point1.X;
  Vector.Y := Point2.Y - Point1.Y;
  if (Vector.X = 0) and (Vector.Y = 0) then begin
    Vector.Y := 1;
  end;
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
              if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
              else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
              if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
              else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
              if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
              else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > GradientArrayMask then Offset := GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
              if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
              else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
              if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
              else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
              if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
              else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsLinearRepetitive:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X)) and GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
              if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
              else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
              if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
              else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
              if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
              else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end
        else begin
          Ratio := -Vector.X / Vector.Y;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(GradientArraySize * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y)) and GradientArrayMask;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
              if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
              else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
              if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
              else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
              if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
              else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
              P^.A := P^.A + (Alpha - P^.A) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsRadialRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsPolar:
      begin
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamondRepetitive:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) * Dist) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsStar:
      begin
        Dist := 1.0 / ((Sqr(Vector.X) + Sqr(Vector.Y)) * 0.5);
        Ang := PartArcTan(Vector.Y, Vector.X) + (Pi/4);
        CosAng := Cos(Ang);
        SinAng := Sin(Ang);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          Vector.Y := Y - Point1.Y;
          CosY := CosAng * Vector.Y;
          SinY := SinAng * Vector.Y;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Vector.X := X - Point1.X;
            Offset := Round(GradientArraySize * Abs((CosAng * Vector.X + SinY) * (SinAng * Vector.X - CosY)) * Dist);
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTapered:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > GradientArrayMask then Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTaperedRepetitive:
      begin
        Dist := 1.0 / (Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y))) and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpherical:
      begin
        Ratio := GradientArraySize * (2/Pi);
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ang := Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist;
            if Ang < 1 then Offset := Trunc(Ratio * ArcSin(Ang))
            else Offset := GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * (Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian));
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsSpiralAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(GradientArraySize * ((PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian + Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) * Dist));
            Offset := Offset and GradientArrayMask;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsTunnel:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio);
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) + (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsHelicalAntiClockwise:
      begin
        Dist := 1.0 / Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Ang := PartArcTan(Vector.Y, Vector.X);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Ratio := Dist * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y));
            if Ratio <> 0 then Ratio := Ln2(1.0 / Ratio) - (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian;
            Offset := GradientArrayMask - (Round(GradientArraySize * Ratio) and GradientArrayMask);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            if (O and $FF) <= Dithering then O := O shr 8 else O := O shr 8 + 1;
            if (C^.R and $FF) <= Dithering then P^.R := P^.R + (C^.R shr 8 - P^.R) * O div 256
            else P^.R := P^.R + (C^.R shr 8 + 1 - P^.R) * O div 256;
            if (C^.G and $FF) <= Dithering then P^.G := P^.G + (C^.G shr 8 - P^.G) * O div 256
            else P^.G := P^.G + (C^.G shr 8 + 1 - P^.G) * O div 256;
            if (C^.B and $FF) <= Dithering then P^.B := P^.B + (C^.B shr 8 - P^.B) * O div 256
            else P^.B := P^.B + (C^.B shr 8 + 1 - P^.B) * O div 256;
            P^.A := P^.A + (Alpha - P^.A) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopNotifyListener
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopNotifyListener.Create(AEvent: TNotifyEvent);
begin
  FEvent := AEvent;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientKey
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGradientKey.Create(AOwner: TPegtopGradientKeyList);
begin
  FOwner := AOwner;
  FEnabled := True;
end;

procedure TPegtopGradientKey.Assign(Source: TPersistent);
var
  S: TPegtopGradientKey;
begin
  if Source is TPegtopGradientKey then begin
    S := TPegtopGradientKey(Source);
    FPosition := S.FPosition;
    FOwner.Update([pgcKeyPosition, pgcKeyAttribute]);
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopGradientKey.SetPosition(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > PegtopColorGradientPositionRange then
    Value := PegtopColorGradientPositionRange;
  if FPosition <> Value then begin
    FPosition := Value;
    FOwner.Update([pgcKeyPosition]);
  end;
end;

procedure TPegtopGradientKey.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    FOwner.Update([pgcKeyAttribute]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorKey
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorKey.Assign(Source: TPersistent);
var
  S: TPegtopColorKey;
begin
  if Source is TPegtopColorKey then begin
    S := TPegtopColorKey(Source);
    FColor := S.FColor;
  end;
  inherited;
end;

procedure TPegtopColorKey.SaveToStream(Stream: TStream);
var
  R: TPegtopColorKeyRecord;
begin
  R.Position := Position;
  FillChar(R.Reserved, SizeOf(R.Reserved), 0);
  R.Color := FColor;
  WriteChunkHeader(Stream, ColorKeyChunkId, SizeOf(TPegtopColorKeyRecord));
  Stream.Write(R, SizeOf(R));
end;

procedure TPegtopColorKey.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Owner.Update([pgcKeyAttribute]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopOpacityKey
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopOpacityKey.Create(AOwner: TPegtopGradientKeyList);
begin
  inherited;
  FOpacity := 256;
end;

procedure TPegtopOpacityKey.SaveToStream(Stream: TStream);
var
  R: TPegtopOpacityKeyRecord;
begin
  R.Position := Position;
  FillChar(R.Reserved, SizeOf(R.Reserved), 0);
  R.Opacity := FOpacity;
  WriteChunkHeader(Stream, OpacityKeyChunkId, SizeOf(TPegtopOpacityKeyRecord));
  Stream.Write(R, SizeOf(R));
end;

procedure TPegtopOpacityKey.Assign(Source: TPersistent);
var
  S: TPegtopOpacityKey;
begin
  if Source is TPegtopOpacityKey then begin
    S := TPegtopOpacityKey(Source);
    FOpacity := S.FOpacity;
  end;
  inherited;
end;

procedure TPegtopOpacityKey.SetOpacity(Value: Integer);
begin
  if FOpacity <> Value then begin
    FOpacity := Value;
    FOwner.Update([pgcKeyAttribute]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientNoise
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientNoise.Create(AOwner: TPegtopColorGradientDefinition);
begin
  FOwner := AOwner;
  Clear;
end;

procedure TPegtopColorGradientNoise.Clear;
begin
  FStrength := 0;
  FFrequency := 2;
  FRoughness := 128;
  FRandomKey := 0;
end;

procedure TPegtopColorGradientNoise.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientNoise;
begin
  if Source is TPegtopColorGradientNoise then begin
    S := TPegtopColorGradientNoise(Source);
    FStrength := S.FStrength;
    FFrequency := S.FFrequency;
    FRoughness := S.FRoughness;
    FRandomKey := S.FRandomKey;
    FOwner.Update([pgcNoise]);
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopColorGradientNoise.GetPerlinNoise(var NoiseArray: TPegtopNoiseArray; Channel, Iterations: Integer);
var
  I, J, Size: Integer;
  Y0, Y1, Y2, Y3, A: Integer;
  IntX, FracX, MaxX, IntP, OldP: Integer;
  Octaves: Integer;
  DeltaIntX, DeltaFracX: Integer;
  Seamless: Boolean;
begin
  Seamless := FOwner.FOwner.Seamless;
  Y0 := 0; // avoid compiler warnings
  Y1 := 0; // avoid compiler warnings
  Y2 := 0; // avoid compiler warnings
  Y3 := 0; // avoid compiler warnings
  Size := Length(NoiseArray);
  Octaves := Round(Ln(Size / FFrequency) / Ln(2)) - 1;
  DeltaIntX := ((FFrequency * Iterations) shl 8) div Size;
  DeltaFracX := ((FFrequency * Iterations) shl 8) mod Size;
  MaxX := FFrequency shl 8;
  IntX := 0;
  FracX := 0;
  OldP := -2;
  for I := 0 to Size - 1 do begin
    IntP := IntX div 256;
    if IntP <> OldP then begin
      if Seamless then begin
        if IntP = OldP + 1 then begin
          Y0 := Y1;
          Y1 := Y2;
          Y2 := Y3;
          Y3 := RandomNoise(FRandomKey, Channel, 0, (IntP + 2) mod FFrequency);
        end
        else begin
          Y0 := RandomNoise(FRandomKey, Channel, 0, (IntP + FFrequency - 1) mod FFrequency);
          Y1 := RandomNoise(FRandomKey, Channel, 0, IntP mod FFrequency);
          Y2 := RandomNoise(FRandomKey, Channel, 0, (IntP + 1) mod FFrequency);
          Y3 := RandomNoise(FRandomKey, Channel, 0, (IntP + 2) mod FFrequency);
        end;
      end
      else begin
        if IntP = OldP + 1 then begin
          Y0 := Y1;
          Y1 := Y2;
          Y2 := Y3;
          Y3 := RandomNoise(FRandomKey, Channel, 0, IntP + 2);
        end
        else begin
          Y0 := RandomNoise(FRandomKey, Channel, 0, IntP - 1);
          Y1 := RandomNoise(FRandomKey, Channel, 0, IntP);
          Y2 := RandomNoise(FRandomKey, Channel, 0, IntP + 1);
          Y3 := RandomNoise(FRandomKey, Channel, 0, IntP + 2);
        end;
      end;
    end;
    NoiseArray[I] := InterpolateCubicEquidistant256(Y0, Y1, Y2, Y3, IntX and 255);
    OldP := IntP;
    // simulate floating point arithmetics:
    Inc(IntX, DeltaIntX);
    Inc(FracX, DeltaFracX);
    if FracX >= Size then begin
      Inc(IntX);
      Dec(FracX, Size);
    end;
    if IntX >= MaxX then Dec(IntX, MaxX);
  end;
  if FRoughness > 0 then begin
    for J := 1 to Octaves - 1 do begin
      A := Round(Exp(Ln(FRoughness / 256) * J) * 256); // (FRoughness / 256) ^ J
      DeltaIntX := (((FFrequency * Iterations) shl (J + 8))) div Size;
      DeltaFracX := (((FFrequency * Iterations) shl (J + 8))) mod Size;
      MaxX := FFrequency shl (J + 8);
      IntX := 0;
      FracX := 0;
      OldP := -2;
      for I := 0 to Size - 1 do begin
        IntP := IntX div 256;
        if IntP <> OldP then begin
          if Seamless then begin
            if IntP = OldP + 1 then begin
              Y0 := Y1;
              Y1 := Y2;
              Y2 := Y3;
              Y3 := RandomNoise(FRandomKey, Channel, 0, (IntP + 2) mod (FFrequency shl J)) * A div 256;
            end
            else begin
              Y0 := RandomNoise(FRandomKey, Channel, 0, (IntP + (FFrequency shl J) - 1) mod (FFrequency shl J)) * A div 256;
              Y1 := RandomNoise(FRandomKey, Channel, 0, IntP mod (FFrequency shl J)) * A div 256;
              Y2 := RandomNoise(FRandomKey, Channel, 0, (IntP + 1) mod (FFrequency shl J)) * A div 256;
              Y3 := RandomNoise(FRandomKey, Channel, 0, (IntP + 2) mod (FFrequency shl J)) * A div 256;
            end;
          end
          else begin
            if IntP = OldP + 1 then begin
              Y0 := Y1;
              Y1 := Y2;
              Y2 := Y3;
              Y3 := RandomNoise(FRandomKey, Channel, 0, IntP + 2) * A div 256;
            end
            else begin
              Y0 := RandomNoise(FRandomKey, Channel, J, IntP - 1) * A div 256;
              Y1 := RandomNoise(FRandomKey, Channel, J, IntP) * A div 256;
              Y2 := RandomNoise(FRandomKey, Channel, J, IntP + 1) * A div 256;
              Y3 := RandomNoise(FRandomKey, Channel, J, IntP + 2) * A div 256;
            end;
          end;
        end;
        NoiseArray[I] := NoiseArray[I] + InterpolateCubicEquidistant256(Y0, Y1, Y2, Y3, IntX and 255);
        OldP := IntP;
        // simulate floating point arithmetics:
        Inc(IntX, DeltaIntX);
        Inc(FracX, DeltaFracX);
        if FracX >= Size then begin
          Inc(IntX);
          Dec(FracX, Size);
        end;
        if IntX >= MaxX then Dec(IntX, MaxX);
      end;
    end;
  end;
  for I := 0 to Size - 1 do begin
    NoiseArray[I] := NoiseArray[I] + $8000;
    if NoiseArray[I] < 0 then NoiseArray[I] := 0
    else if NoiseArray[I] > $FF00 then NoiseArray[I] := $FF00;
  end;
end;

procedure TPegtopColorGradientNoise.SetStrength(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FStrength <> Value then begin
    FStrength := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientNoise.SetFrequency(Value: Integer);
begin
  if Value < 1 then Value := 1 else if Value > 10 then Value := 10;
  if FFrequency <> Value then begin
    FFrequency := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientNoise.SetRoughness(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FRoughness <> Value then begin
    FRoughness := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientNoise.SetRandomKey(Value: Integer);
begin
  Value := Value mod 1000000;
  if FRandomKey <> Value then begin
    FRandomKey := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientColorNoise
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorGradientColorNoise.Clear;
begin
  inherited;
  FRedMin := 0;
  FRedMax := 255;
  FGreenMin := 0;
  FGreenMax := 255;
  FBlueMin := 0;
  FBlueMax := 255;
  FHueAvg := 0;
  FHueRange := 65536;
  FSaturationMin := 0;
  FSaturationMax := 65536;
  FBrightnessMin := 0;
  FBrightnessMax := 65536;
end;

procedure TPegtopColorGradientColorNoise.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientColorNoise;
begin
  if Source is TPegtopColorGradientColorNoise then begin
    S := TPegtopColorGradientColorNoise(Source);
    FRedMin := S.FRedMin;
    FRedMax := S.FRedMax;
    FGreenMin := S.FGreenMin;
    FGreenMax := S.FGreenMax;
    FBlueMin := S.FBlueMin;
    FBlueMax := S.FBlueMax;
    FHueAvg := S.FHueAvg;
    FHueRange := S.FHueRange;
    FSaturationMin := S.FSaturationMin;
    FSaturationMax := S.FSaturationMax;
    FBrightnessMin := S.FBrightnessMin;
    FBrightnessMax := S.FBrightnessMax;
  end;
  inherited;
end;

procedure TPegtopColorGradientColorNoise.SaveToStream(Stream: TStream);
var
  P: Integer;
  R: TPegtopColorNoiseRecord;
begin
  P := ForgetChunkHeader(Stream);
  WriteChunkHeader(Stream, ColorNoiseDataChunkId, SizeOf(R));
  R.Strength := FStrength;
  R.Frequency := FFrequency;
  R.Roughness := FRoughness;
  R.RandomKey := FRandomKey;
  R.RedMin := FRedMin;
  R.RedMax := FRedMax;
  R.GreenMin := FGreenMin;
  R.GreenMax := FGreenMax;
  R.BlueMin := FBlueMin;
  R.BlueMax := FBlueMax;
  R.HueAvg := FHueAvg;
  R.HueRange := FHueRange;
  R.SaturationMin := FSaturationMin;
  R.SaturationMax := FSaturationMax;
  R.BrightnessMin := FBrightnessMin;
  R.BrightnessMax := FBrightnessMax;
  Stream.Write(R, SizeOf(R));
  CompleteChunkHeader(Stream, P, ColorNoiseChunkId);
end;

procedure TPegtopColorGradientColorNoise.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineDataChunk(ColorNoiseDataChunkId, SizeOf(TPegtopColorNoiseRecord), ChunkReadData, [pcoUnique]);
end;

procedure TPegtopColorGradientColorNoise.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  FStrength := TPegtopColorNoiseRecord(Data).Strength;
  FFrequency := TPegtopColorNoiseRecord(Data).Frequency;
  FRoughness := TPegtopColorNoiseRecord(Data).Roughness;
  FRandomKey := TPegtopColorNoiseRecord(Data).RandomKey;
  FRedMin := TPegtopColorNoiseRecord(Data).RedMin;
  FRedMax := TPegtopColorNoiseRecord(Data).RedMax;
  FGreenMin := TPegtopColorNoiseRecord(Data).GreenMin;
  FGreenMax := TPegtopColorNoiseRecord(Data).GreenMax;
  FBlueMin := TPegtopColorNoiseRecord(Data).BlueMin;
  FBlueMax := TPegtopColorNoiseRecord(Data).BlueMax;
  FHueAvg := TPegtopColorNoiseRecord(Data).HueAvg;
  FHueRange := TPegtopColorNoiseRecord(Data).HueRange;
  FSaturationMin := TPegtopColorNoiseRecord(Data).SaturationMin;
  FSaturationMax := TPegtopColorNoiseRecord(Data).SaturationMax;
  FBrightnessMin := TPegtopColorNoiseRecord(Data).BrightnessMin;
  FBrightnessMax := TPegtopColorNoiseRecord(Data).BrightnessMax;
end;

procedure TPegtopColorGradientColorNoise.ApplyRGB(var ColorArray: TPegtopColorArray;
  const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
var
  I, A: Integer;
  Size: Integer;
  P: PPegtopColor;
  NoiseArrays: array[0..2] of TPegtopNoiseArray;
  A1, A2, A3: Integer;
  O1, O2, O3: Integer;
begin
  Size := Length(ColorArray);
  if pgoSymmetrical in Options then Size := (Size + 1) div 2;
  A := FStrength;
  O3 := FRedMin;
  O2 := FGreenMin;
  O1 := FBlueMin;
  A3 := (FRedMax - FRedMin) * 256 div 255;
  A2 := (FGreenMax - FGreenMin) * 256 div 255;
  A1 := (FBlueMax - FBlueMin) * 256 div 255;
  for I := 0 to 2 do begin
    SetLength(NoiseArrays[I], Size);
    GetPerlinNoise(NoiseArrays[I], I, Iterations);
  end;
  P := Addr(ColorArray[0]);
  if pgoReverse in Options then begin
    if SwapRedBlue then begin
      for I := Size - 1 downto 0 do begin
        P^.R := P^.R + (NoiseArrays[2, I] * A3 div 65536 + O3 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 65536 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[0, I] * A1 div 65536 + O1 - P^.B) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := Size - 1 downto 0 do begin
        P^.R := P^.R + (NoiseArrays[0, I] * A1 div 65536 + O1 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 65536 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[2, I] * A3 div 65536 + O3 - P^.B) * A div 256;
        Inc(P);
      end;
    end;
  end
  else begin
    if SwapRedBlue then begin
      for I := 0 to Size - 1 do begin
        P^.R := P^.R + (NoiseArrays[2, I] * A3 div 65536 + O3 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 65536 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[0, I] * A1 div 65536 + O1 - P^.B) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := 0 to Size - 1 do begin
        P^.R := P^.R + (NoiseArrays[0, I] * A1 div 65536 + O1 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 65536 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[2, I] * A3 div 65536 + O3 - P^.B) * A div 256;
        Inc(P);
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColorNoise.ApplyHSB(var ColorArray: TPegtopColorArray;
  const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
var
  I, A: Integer;
  Size: Integer;
  P: PPegtopColor;
  C: TPegtopColor;
  NoiseArrays: array[0..2] of TPegtopNoiseArray;
  A1, A2, A3: Integer;
  O1, O2, O3: Integer;
begin
  Size := Length(ColorArray);
  if pgoSymmetrical in Options then Size := (Size + 1) div 2;
  A := FStrength;
  O1 := FHueAvg - FHueRange div 2;
  O2 := FSaturationMin;
  O3 := FBrightnessMin;
  A1 := FHueRange div 2;
  A2 := (FSaturationMax - FSaturationMin) div 2;
  A3 := (FBrightnessMax - FBrightnessMin) div 2;
  for I := 0 to 2 do begin
    SetLength(NoiseArrays[I], Size);
    GetPerlinNoise(NoiseArrays[I], I, Iterations);
  end;
  P := Addr(ColorArray[0]);
  if pgoReverse in Options then begin
    if SwapRedBlue then begin
      for I := Size - 1 downto 0 do begin
        C := ConvertHSBToColor(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.B - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.R - P^.R) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := Size - 1 downto 0 do begin
        C := ConvertHSBToColor(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.R - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.B - P^.R) * A div 256;
        Inc(P);
      end;
    end;
  end
  else begin
    if SwapRedBlue then begin
      for I := 0 to Size - 1 do begin
        C := ConvertHSBToColor(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.B - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.R - P^.R) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := 0 to Size - 1 do begin
        C := ConvertHSBToColor(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.R - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.B - P^.R) * A div 256;
        Inc(P);
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColorNoise.ApplyRGB64(var ColorArray: TPegtopColor64Array;
  const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
var
  I, A: Integer;
  Size: Integer;
  P: PPegtopColor64;
  NoiseArrays: array[0..2] of TPegtopNoiseArray;
  A1, A2, A3: Integer;
  O1, O2, O3: Integer;
begin
  Size := Length(ColorArray);
  if pgoSymmetrical in Options then Size := (Size + 1) div 2;
  A := FStrength;
  O3 := FRedMin * 256;
  O2 := FGreenMin * 256;
  O1 := FBlueMin * 256;
  A3 := (FRedMax - FRedMin) * 256 div 255;
  A2 := (FGreenMax - FGreenMin) * 256 div 255;
  A1 := (FBlueMax - FBlueMin) * 256 div 255;
  for I := 0 to 2 do begin
    SetLength(NoiseArrays[I], Size);
    GetPerlinNoise(NoiseArrays[I], I, Iterations);
  end;
  P := Addr(ColorArray[0]);
  if pgoReverse in Options then begin
    if SwapRedBlue then begin
      for I := Size - 1 downto 0 do begin
        P^.R := P^.R + (NoiseArrays[2, I] * A3 div 256 + O3 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 256 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[0, I] * A1 div 256 + O1 - P^.B) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := Size - 1 downto 0 do begin
        P^.R := P^.R + (NoiseArrays[0, I] * A1 div 256 + O1 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 256 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[2, I] * A3 div 256 + O3 - P^.B) * A div 256;
        Inc(P);
      end;
    end;
  end
  else begin
    if SwapRedBlue then begin
      for I := 0 to Size - 1 do begin
        P^.R := P^.R + (NoiseArrays[2, I] * A3 div 256 + O3 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 256 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[0, I] * A1 div 256 + O1 - P^.B) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := 0 to Size - 1 do begin
        P^.R := P^.R + (NoiseArrays[0, I] * A1 div 256 + O1 - P^.R) * A div 256;
        P^.G := P^.G + (NoiseArrays[1, I] * A2 div 256 + O2 - P^.G) * A div 256;
        P^.B := P^.B + (NoiseArrays[2, I] * A3 div 256 + O3 - P^.B) * A div 256;
        Inc(P);
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColorNoise.ApplyHSB64(var ColorArray: TPegtopColor64Array;
  const Options: TPegtopColorGradientOptions; Iterations: Integer; SwapRedBlue: Boolean);
var
  I, A: Integer;
  Size: Integer;
  P: PPegtopColor64;
  C: TPegtopColor64;
  NoiseArrays: array[0..2] of TPegtopNoiseArray;
  A1, A2, A3: Integer;
  O1, O2, O3: Integer;
begin
  Size := Length(ColorArray);
  if pgoSymmetrical in Options then Size := (Size + 1) div 2;
  A := FStrength;
  O1 := FHueAvg - FHueRange div 2;
  O2 := FSaturationMin;
  O3 := FBrightnessMin;
  A1 := FHueRange div 2;
  A2 := (FSaturationMax - FSaturationMin) div 2;
  A3 := (FBrightnessMax - FBrightnessMin) div 2;
  for I := 0 to 2 do begin
    SetLength(NoiseArrays[I], Size);
    GetPerlinNoise(NoiseArrays[I], I, Iterations);
  end;
  P := Addr(ColorArray[0]);
  if pgoReverse in Options then begin
    if SwapRedBlue then begin
      for I := Size - 1 downto 0 do begin
        C := ConvertHSBToColor64(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.B - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.R - P^.R) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := Size - 1 downto 0 do begin
        C := ConvertHSBToColor64(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.R - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.B - P^.R) * A div 256;
        Inc(P);
      end;
    end;
  end
  else begin
    if SwapRedBlue then begin
      for I := 0 to Size - 1 do begin
        C := ConvertHSBToColor64(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.B - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.R - P^.R) * A div 256;
        Inc(P);
      end;
    end
    else begin
      for I := 0 to Size - 1 do begin
        C := ConvertHSBToColor64(NoiseArrays[0, I] * A1 div 32768 + O1, NoiseArrays[1, I] * A2 div 32768 + O2, NoiseArrays[2, I] * A3 div 32768 + O3);
        P^.B := P^.B + (C.R - P^.B) * A div 256;
        P^.G := P^.G + (C.G - P^.G) * A div 256;
        P^.R := P^.R + (C.B - P^.R) * A div 256;
        Inc(P);
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColorNoise.SetRedMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FRedMin <> Value then begin
    FRedMin := Value;
    if FRedMax < FRedMin then FRedMax := FRedMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetRedMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FRedMax <> Value then begin
    FRedMax := Value;
    if FRedMin > FRedMax then FRedMin := FRedMax;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetGreenMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FGreenMin <> Value then begin
    FGreenMin := Value;
    if FGreenMax < FGreenMin then FGreenMax := FGreenMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetGreenMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FGreenMax <> Value then begin
    FGreenMax := Value;
    if FGreenMin > FGreenMax then FGreenMin := FGreenMax;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetBlueMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FBlueMin <> Value then begin
    FBlueMin := Value;
    if FBlueMax < FBlueMin then FBlueMax := FBlueMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetBlueMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 255 then Value := 255;
  if FBlueMax <> Value then begin
    FBlueMax := Value;
    if FBlueMin > FBlueMax then FBlueMin := FBlueMax;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetHueAvg(Value: Integer);
begin
  Value := Value and HueMask;
  if FHueAvg <> Value then begin
    FHueAvg := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetHueRange(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > Hue360 then Value := Hue360;
  if FHueRange <> Value then begin
    FHueRange := Value;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetSaturationMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > Sat100 then Value := Sat100;
  if FSaturationMin <> Value then begin
    FSaturationMin := Value;
    if FSaturationMax < FSaturationMin then FSaturationMax := FSaturationMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetSaturationMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > Sat100 then Value := Sat100;
  if FSaturationMax <> Value then begin
    FSaturationMax := Value;
    if FSaturationMin > FSaturationMax then FSaturationMin := FSaturationMax;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetBrightnessMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > Bri100 then Value := Bri100;
  if FBrightnessMin <> Value then begin
    FBrightnessMin := Value;
    if FBrightnessMax < FBrightnessMin then FBrightnessMax := FBrightnessMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientColorNoise.SetBrightnessMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > Bri100 then Value := Bri100;
  if FBrightnessMax <> Value then begin
    FBrightnessMax := Value;
    if FBrightnessMin > FBrightnessMax then FBrightnessMin := FBrightnessMax;
    FOwner.Update([pgcNoise]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientOpacityNoise
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorGradientOpacityNoise.Clear;
begin
  inherited;
  FOpacityMin := 0;
  FOpacityMax := 256;
end;

procedure TPegtopColorGradientOpacityNoise.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientOpacityNoise;
begin
  if Source is TPegtopColorGradientOpacityNoise then begin
    S := TPegtopColorGradientOpacityNoise(Source);
    FOpacityMin := S.FOpacityMin;
    FOpacityMax := S.FOpacityMax;
  end;
  inherited;
end;

procedure TPegtopColorGradientOpacityNoise.SaveToStream(Stream: TStream);
var
  P: Integer;
  R: TPegtopOpacityNoiseRecord;
begin
  P := ForgetChunkHeader(Stream);
  WriteChunkHeader(Stream, OpacityNoiseDataChunkId, SizeOf(R));
  R.Strength := FStrength;
  R.Frequency := FFrequency;
  R.Roughness := FRoughness;
  R.RandomKey := FRandomKey;
  R.OpacityMin := FOpacityMin;
  R.OpacityMax := FOpacityMax;
  Stream.Write(R, SizeOf(R));
  CompleteChunkHeader(Stream, P, OpacityNoiseChunkId);
end;

procedure TPegtopColorGradientOpacityNoise.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineDataChunk(OpacityNoiseDataChunkId, SizeOf(TPegtopOpacityNoiseRecord), ChunkReadData, [pcoUnique]);
end;

procedure TPegtopColorGradientOpacityNoise.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  FStrength := TPegtopOpacityNoiseRecord(Data).Strength;
  FFrequency := TPegtopOpacityNoiseRecord(Data).Frequency;
  FRoughness := TPegtopOpacityNoiseRecord(Data).Roughness;
  FRandomKey := TPegtopOpacityNoiseRecord(Data).RandomKey;
  FOpacityMin := TPegtopOpacityNoiseRecord(Data).OpacityMin;
  FOpacityMax := TPegtopOpacityNoiseRecord(Data).OpacityMax;
end;

procedure TPegtopColorGradientOpacityNoise.SetOpacityMin(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FOpacityMin <> Value then begin
    FOpacityMin := Value;
    if FOpacityMax < FOpacityMin then FOpacityMax := FOpacityMin;
    FOwner.Update([pgcNoise]);
  end;
end;

procedure TPegtopColorGradientOpacityNoise.SetOpacityMax(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FOpacityMax <> Value then begin
    FOpacityMax := Value;
    if FOpacityMin > FOpacityMax then FOpacityMin := FOpacityMax;
    FOwner.Update([pgcNoise]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientKeyList
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGradientKeyList.Create(AOwner: TPegtopColorGradientDefinition);
begin
  FOwner := AOwner;
  FKeys := TList.Create;
end;

destructor TPegtopGradientKeyList.Destroy;
begin
  Clear;
  FKeys.Free;
  inherited;
end;

procedure TPegtopGradientKeyList.Assign(Source: TPegtopGradientKeyList);
begin
  BeginUpdate;
  try
    Clear;
    Merge(Source);
  finally
    EndUpdate;
  end;
end;

procedure TPegtopGradientKeyList.LoadFromStream(Stream: TStream);
var
  Reader: TPegtopContainerChunkReader;
begin
  Clear;
  Reader := TPegtopContainerChunkReader.Create(NIL);
  try
    Reader.DefineDataChunk(ColorKeyChunkId, SizeOf(TPegtopColorKeyRecord), ChunkReadData);
    Reader.DefineDataChunk(OpacityKeyChunkId, SizeOf(TPegtopOpacityKeyRecord), ChunkReadData);
    Reader.Read(Stream, KeysChunkId);
  finally
    Reader.Free;
  end;
end;

procedure TPegtopGradientKeyList.SaveToStream(Stream: TStream);
var
  I: Integer;
  P: Integer;
begin
  P := ForgetChunkHeader(Stream);
  for I := 0 to FKeys.Count - 1 do begin
    TPegtopGradientKey(FKeys[I]).SaveToStream(Stream);
  end;
  CompleteChunkHeader(Stream, P, KeysChunkId);
end;

procedure TPegtopGradientKeyList.ApplyChanges;
begin
  if pgcKeyPosition in FUpdateChanges then FKeys.Sort(KeySortCompare);
  FUpdateChanges := [];
end;

procedure TPegtopGradientKeyList.Update(Changes: TPegtopColorGradientChanges);
begin
  FUpdateChanges := FUpdateChanges + Changes;
  if FOwner <> NIL then FOwner.Update(Changes);
end;

procedure TPegtopGradientKeyList.BeginUpdate;
begin
  if FOwner <> NIL then FOwner.BeginUpdate;
end;

procedure TPegtopGradientKeyList.EndUpdate;
begin
  if FOwner <> NIL then FOwner.EndUpdate;
end;

procedure TPegtopGradientKeyList.Clear;
var
  I: Integer;
begin
  for I := 0 to FKeys.Count - 1 do begin
    TPegtopGradientKey(FKeys[I]).Free;
  end;
  FKeys.Clear;
  Update([pgcKeyCount]); // no keys to sort left
end;

function TPegtopGradientKeyList.IndexOf(Key: TPegtopGradientKey): Integer;
begin
  Result := FKeys.IndexOf(Key);
end;

procedure TPegtopGradientKeyList.Delete(Index: Integer);
begin
  FKeys.Delete(Index);
  Update([pgcKeyCount]); // order has not changed
end;

function TPegtopGradientKeyList.Remove(Key: TPegtopGradientKey): Integer;
begin
  Result := FKeys.Remove(Key);
  Update([pgcKeyCount]); // order has not changed
end;

procedure TPegtopGradientKeyList.DistributeEvenly;
var
  I: Integer;
begin
  BeginUpdate;
  try
    if FKeys.Count >= 2 then begin
      if FOwner.Owner.Seamless then begin
        for I := 0 to FKeys.Count - 1 do begin
          TPegtopGradientKey(FKeys[I]).Position := (I * 2 + 1) * PegtopColorGradientPositionRange div (2 * FKeys.Count);
        end;
      end
      else begin
        for I := 0 to FKeys.Count - 1 do begin
          TPegtopGradientKey(FKeys[I]).Position := I * PegtopColorGradientPositionRange div (FKeys.Count - 1);
        end;
      end;
    end
    else if FKeys.Count = 1 then begin
      TPegtopGradientKey(FKeys[0]).Position := PegtopColorGradientPositionRange div 2;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopGradientKeyList.Flip;
var
  I: Integer;
begin
  if FKeys.Count >= 1 then begin
    BeginUpdate;
    try
      for I := 0 to FKeys.Count - 1 do begin
        TPegtopGradientKey(FKeys[I]).Position := PegtopColorGradientPositionRange - TPegtopGradientKey(FKeys[I]).Position;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPegtopGradientKeyList.Blur(Intensity: Integer);
var
  I: Integer;
  Key, PreviousKey, NextKey: TPegtopGradientKey;
  TempKeys: TPegtopGradientKeyList;
begin
  if Count > 1 then begin
    TempKeys := TPegtopGradientColorKeyListClass(Self.ClassType).Create(NIL);
    try
      TempKeys.Assign(Self); // copy current keys
      if TempKeys.FKeys.Count <> FKeys.Count then raise Exception.Create('Internal error: color gradient cannot be blurred.');
      BeginUpdate;
      try
        for I := 0 to FKeys.Count - 1 do begin
          Key := FKeys[I];
          if I = 0 then begin
            if FOwner.Owner.Seamless then
              PreviousKey := TempKeys.FKeys[FKeys.Count - 1]
            else
              PreviousKey := TempKeys.FKeys[0];
            NextKey := TempKeys.FKeys[I + 1];
          end
          else if I = TempKeys.Count - 1 then begin
            PreviousKey := TempKeys.FKeys[I - 1];
            if FOwner.Owner.Seamless then
              NextKey := TempKeys.FKeys[0]
            else
              NextKey := TempKeys.FKeys[FKeys.Count - 1];
          end
          else begin
            PreviousKey := TempKeys.FKeys[I - 1];
            NextKey := TempKeys.FKeys[I + 1];
          end;
          BlurKey(Key, PreviousKey, NextKey, Intensity);
        end;
      finally
        EndUpdate;
      end;
    finally
      TempKeys.Free;
    end;
  end;
end;

procedure TPegtopGradientKeyList.InterpolateLinear(Data: Pointer; Size: Integer;
  Options: TPegtopColorGradientOptions; Iterations: Integer; Max: Integer;
  OnInterpolate: TPegtopColorGradientInterpolateLinearEvent);
var
  I, J, P, Q, Rate: Integer;
  CurrentIndex: Integer;
  CurrentPosition, NextPosition: Integer;
  CurrentKey, NextKey: TPegtopGradientKey;
  Step, Range: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  CurrentPosition := 0; // avoid compiler warning
  NextPosition := 0; // avoid compiler warning
  CurrentIndex := 0; // avoid compiler warning
  CurrentKey := NIL; // avoid compiler warning
  NextKey := NIL; // avoid compiler warning
  if (Size > 0) and (FKeys.Count > 1) then begin // "normal" gradient
    Step := PegtopColorGradientPositionRange * FOwner.Frequency * Iterations;
    Range := Size * PegtopColorGradientPositionRange;
    Q := Range;
    for I := 0 To Size - 1 do begin
      if pgoReverse in Options then J := Size - 1 - I else J := I;
      if Q >= Range then begin
        Dec(Q, Range);
        // (re)initialize:
        if TPegtopGradientKey(FKeys[0]).Position > 0 then begin
          CurrentIndex := -1;
          if FOwner.Owner.Seamless then begin
            // start with last key
            CurrentPosition := TPegtopGradientKey(FKeys[FKeys.Count - 1]).Position - PegtopColorGradientPositionRange;
            CurrentKey := TPegtopGradientKey(FKeys[FKeys.Count - 1]);
          end
          else begin
            // start with first key (but not with first index)
            CurrentPosition := 0;
            CurrentKey := TPegtopGradientKey(FKeys[0]);
          end;
        end
        else begin
          // start with first key
          CurrentIndex := 0;
          CurrentPosition := TPegtopGradientKey(FKeys[0]).Position;
          CurrentKey := TPegtopGradientKey(FKeys[0]);
        end;
        NextKey := TPegtopGradientKey(FKeys[CurrentIndex + 1]);
        NextPosition := NextKey.Position;
      end;
      P := Q div Size;
      while (P >= NextPosition) and (CurrentIndex < FKeys.Count - 1) do begin
        // step to the next key (loop because multiple keys might share the same position)
        Inc(CurrentIndex);
        CurrentPosition := TPegtopGradientKey(FKeys[CurrentIndex]).Position;
        CurrentKey := TPegtopGradientKey(FKeys[CurrentIndex]);
        if CurrentIndex >= FKeys.Count - 1 then begin
          // last key reached
          if FOwner.Owner.Seamless then begin
            // continue with first key
            NextPosition := TPegtopGradientKey(FKeys[0]).Position + PegtopColorGradientPositionRange + 1;
            NextKey := TPegtopGradientKey(FKeys[0]);
          end
          else begin
            // continue with last key
            NextPosition := PegtopColorGradientPositionRange + 1;
            NextKey := CurrentKey;
          end;
        end
        else begin
          // continue with next key
          NextPosition := TPegtopGradientKey(FKeys[CurrentIndex + 1]).Position;
          NextKey := TPegtopGradientKey(FKeys[CurrentIndex + 1]);
        end;
      end;
      // interpolate color:
      Rate := MulDiv((P - CurrentPosition), Max, (NextPosition - CurrentPosition));
      OnInterpolate(Data, J, CurrentKey, NextKey, Rate, Max);
      Inc (Q, Step);
    end;
  end;
end;

procedure TPegtopGradientKeyList.InterpolateCubic(Data: Pointer; Size: Integer;
  Options: TPegtopColorGradientOptions; Iterations: Integer; Max: Integer;
  OnInterpolate: TPegtopColorGradientInterpolateCubicEvent);
var
  I, J, P, Q, Rate: Integer;
  CurrentIndex: Integer;
  CurrentPosition, NextPosition: Integer;
  PreviousKey, CurrentKey, NextKey, AfterNextKey: TPegtopGradientKey;
  Step, Range: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  CurrentPosition := 0; // avoid compiler warning
  NextPosition := 0; // avoid compiler warning
  CurrentIndex := 0; // avoid compiler warning
  PreviousKey := NIL; // avoid compiler warning
  CurrentKey := NIL; // avoid compiler warning
  NextKey := NIL; // avoid compiler warning
  AfterNextKey := NIL; // avoid compiler warning
  if (Size > 0) and (FKeys.Count > 1) then begin // "normal" gradient
    Step := PegtopColorGradientPositionRange * FOwner.Frequency * Iterations;
    Range := Size * PegtopColorGradientPositionRange;
    Q := Range;
    for I := 0 To Size - 1 do begin
      if pgoReverse in Options then J := Size - 1 - I else J := I;
      if Q >= Range then begin
        Dec(Q, Range);
        // (re)initialize:
        if TPegtopGradientKey(FKeys[0]).Position > 0 then begin
          CurrentIndex := -1;
          if FOwner.Owner.Seamless then begin
            // start with last key
            CurrentPosition := TPegtopGradientKey(FKeys[FKeys.Count - 1]).Position - PegtopColorGradientPositionRange;
            CurrentKey := TPegtopGradientKey(FKeys[FKeys.Count - 1]);
            PreviousKey := TPegtopGradientKey(FKeys[(FKeys.Count + FKeys.Count - 2) mod FKeys.Count]);
          end
          else begin
            // start with first key (but not with first index)
            CurrentPosition := 0;
            CurrentKey := TPegtopGradientKey(FKeys[0]);
            PreviousKey := TPegtopGradientKey(FKeys[0])
          end;
        end
        else begin
          // start with first key
          CurrentIndex := 0;
          CurrentPosition := TPegtopGradientKey(FKeys[0]).Position;
          CurrentKey := TPegtopGradientKey(FKeys[0]);
          if FOwner.Owner.Seamless then
            PreviousKey := TPegtopGradientKey(FKeys[FKeys.Count - 1])
          else
            PreviousKey := TPegtopGradientKey(FKeys[0])
        end;
        NextKey := TPegtopGradientKey(FKeys[CurrentIndex + 1]);
        NextPosition := NextKey.Position;
        if FOwner.Owner.Seamless then
          AfterNextKey := TPegtopGradientKey(FKeys[(CurrentIndex + 2) mod FKeys.Count])
        else if CurrentIndex + 2 >= FKeys.Count then
          AfterNextKey := TPegtopGradientKey(FKeys[FKeys.Count - 1])
        else
          AfterNextKey := TPegtopGradientKey(FKeys[CurrentIndex + 2]);
      end;
      P := Q div Size;
      while (P >= NextPosition) and (CurrentIndex < FKeys.Count - 1) do begin
        // step to the next key (loop because multiple keys might share the same position)
        PreviousKey := CurrentKey;
        Inc(CurrentIndex);
        CurrentPosition := TPegtopGradientKey(FKeys[CurrentIndex]).Position;
        CurrentKey := TPegtopGradientKey(FKeys[CurrentIndex]);
        if CurrentIndex >= FKeys.Count - 1 then begin
          // last key reached
          if FOwner.Owner.Seamless then begin
            // continue with first key
            NextPosition := TPegtopGradientKey(FKeys[0]).Position + PegtopColorGradientPositionRange + 1;
            NextKey := TPegtopGradientKey(FKeys[0]);
          end
          else begin
            // continue with last key
            NextPosition := PegtopColorGradientPositionRange + 1;
            NextKey := CurrentKey;
          end;
        end
        else begin
          // continue with next key
          NextPosition := TPegtopGradientKey(FKeys[CurrentIndex + 1]).Position;
          NextKey := TPegtopGradientKey(FKeys[CurrentIndex + 1]);
        end;
        if FOwner.Owner.Seamless then
          AfterNextKey := TPegtopGradientKey(FKeys[(CurrentIndex + 2) mod FKeys.Count])
        else if CurrentIndex + 2 >= FKeys.Count then
          AfterNextKey := TPegtopGradientKey(FKeys[FKeys.Count - 1])
        else
          AfterNextKey := TPegtopGradientKey(FKeys[CurrentIndex + 2]);
      end;
      // interpolate color:
      Rate := MulDiv((P - CurrentPosition), 256, (NextPosition - CurrentPosition));
      OnInterpolate(Data, J, PreviousKey, CurrentKey, NextKey, AfterNextKey, Rate, Max);
      Inc (Q, Step);
    end;
  end;
end;

function TPegtopGradientKeyList.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientColorKeyList
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopGradientColorKeyList.Merge(Source: TPegtopGradientKeyList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Source.Count - 1 do begin
      with Add do Assign(Source.FKeys[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopGradientColorKeyList.Define(const AColors: array of TColor);
var
  I, L: Integer;
begin
  Clear;
  L := Length(AColors) - 1;
  if L > 0 then begin
    BeginUpdate;
    try
      for I := 0 to L do begin
        with Add do begin
          Position := I * 1000 div L;
          Color := AColors[I];
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else if L > -1 then begin
    with Add do Color := AColors[0];
  end;
end;

procedure TPegtopGradientColorKeyList.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  Clear;
  ContainerChunkReader.DefineDataChunk(ColorKeyChunkId, SizeOf(TPegtopColorKeyRecord), ChunkReadData);
end;

procedure TPegtopGradientColorKeyList.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  if CompareChunkIds(Id, ColorKeyChunkId) then begin
    with Add do begin
      FPosition := TPegtopColorKeyRecord(Data).Position;
      FColor := TPegtopColorKeyRecord(Data).Color;
    end;
  end;
end;

procedure TPegtopGradientColorKeyList.Invert;
var
  I: Integer;
begin
  if Count >= 1 then begin
    BeginUpdate;
    try
      for I := 0 to Count - 1 do begin
        if (Keys[I].Color and $FF000000) = 0 then Keys[I].Color := Keys[I].Color xor $FFFFFF;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPegtopGradientColorKeyList.UnifyColorComponents(Color: TColor; ColorComponent: TPegtopColorComponent);
var
  I: Integer;
  UH, US, UB: Integer;
  KH, KS, KB: Integer;
begin
  if ColorComponent in [pccHue, pccSaturation, pccBrightness] then begin
    ConvertColorToHSB(TPegtopColor(Color), UH, US, UB);
    BeginUpdate;
    try
      for I := 0 to Count - 1 do begin
        ConvertColorToHSB(TPegtopColor(Keys[I].Color), KH, KS, KB);
        case ColorComponent of
          pccHue:        KH := UH;
          pccSaturation: KS := US;
          pccBrightness: KB := UB;
        end;
        Keys[I].Color := TColor(ConvertHSBToColor(KH, KS, KB));
      end;
    finally
      EndUpdate;
    end;
  end
  else if ColorComponent in [pccRed, pccGreen, pccBlue] then begin
    BeginUpdate;
    try
      for I := 0 to Count - 1 do begin
        case ColorComponent of
          pccRed:   Keys[I].Color := (Keys[I].Color and TColor($FFFFFF00)) or (Color and TColor($000000FF));
          pccGreen: Keys[I].Color := (Keys[I].Color and TColor($FFFF00FF)) or (Color and TColor($0000FF00));
          pccBlue:  Keys[I].Color := (Keys[I].Color and TColor($FF00FFFF)) or (Color and TColor($00FF0000));
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPegtopGradientColorKeyList.BlurKey(Key, PreviousKey, NextKey: TPegtopGradientKey; Intensity: Integer);
var
  C1, C2: TPegtopColor;
begin
  // if FMode = pgmHSB then begin
  // TODO
  // end
  // else begin
  C1 := TPegtopColor(TPegtopColorKey(Key).Color);
  C2 := MixColors(TPegtopColor(TPegtopColorKey(PreviousKey).Color), TPegtopColor(TPegtopColorKey(NextKey).Color), 128);
  TPegtopColorKey(Key).Color := TColor(MixColors(C1, C2, Intensity));
  // end;
end;

function TPegtopGradientColorKeyList.AddGradientKey: TPegtopGradientKey;
begin
  Result := Add;
end;

function TPegtopGradientColorKeyList.Add: TPegtopColorKey;
begin
  Result := TPegtopColorKey.Create(Self);
  FKeys.Add(Result);
  Update([pgcKeyCount]);
end;

function TPegtopGradientColorKeyList.GetKey(Index: Integer): TPegtopColorKey;
begin
  if (Index >= 0) and (Index < FKeys.Count) then
    Result := FKeys[Index]
  else
    Result := NIL;
end;

procedure TPegtopGradientColorKeyList.SetKey(Index: Integer; Value: TPegtopColorKey);
begin
  if (Index >= 0) and (Index < FKeys.Count) then
    TPegtopColorKey(FKeys[Index]).Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGradientOpacityKeyList
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopGradientOpacityKeyList.Merge(Source: TPegtopGradientKeyList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Source.FKeys.Count - 1 do begin
      with Add do Assign(Source.FKeys[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopGradientOpacityKeyList.Define(const AOpacities: array of Integer);
var
  I, L: Integer;
begin
  Clear;
  L := Length(AOpacities) - 1;
  if L > 0 then begin
    BeginUpdate;
    try
      for I := 0 to L do begin
        with Add do begin
          Position := I * 1000 div L;
          Opacity := AOpacities[I];
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else if L > -1 then begin
    with Add do Opacity := AOpacities[0];
  end;
end;

procedure TPegtopGradientOpacityKeyList.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  Clear;
  ContainerChunkReader.DefineDataChunk(OpacityKeyChunkId, SizeOf(TPegtopOpacityKeyRecord), ChunkReadData);
end;

procedure TPegtopGradientOpacityKeyList.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  if CompareChunkIds(Id, OpacityKeyChunkId) then begin
    with Add do begin
      FPosition := TPegtopOpacityKeyRecord(Data).Position;
      FOpacity := TPegtopOpacityKeyRecord(Data).Opacity;
    end;
  end;
end;

procedure TPegtopGradientOpacityKeyList.Invert;
var
  I: Integer;
begin
  if Count >= 1 then begin
    BeginUpdate;
    try
      for I := 0 to Count - 1 do begin
        Keys[I].Opacity := Keys[I].Opacity xor $FF;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPegtopGradientOpacityKeyList.BlurKey(Key, PreviousKey, NextKey: TPegtopGradientKey; Intensity: Integer);
var
  O1, O2: Integer;
begin
  O1 := TPegtopOpacityKey(Key).Opacity;
  O2 := (TPegtopOpacityKey(PreviousKey).Opacity + TPegtopOpacityKey(NextKey).Opacity) div 2;
  TPegtopOpacityKey(Key).Opacity := O1 + (O2 - O1) * Intensity div 256;
end;

function TPegtopGradientOpacityKeyList.AddGradientKey: TPegtopGradientKey;
begin
  Result := Add;
end;

function TPegtopGradientOpacityKeyList.Add: TPegtopOpacityKey;
begin
  Result := TPegtopOpacityKey.Create(Self);
  FKeys.Add(Result);
  Update([pgcKeyCount]);
end;

function TPegtopGradientOpacityKeyList.GetKey(Index: Integer): TPegtopOpacityKey;
begin
  if (Index >= 0) and (Index < FKeys.Count) then
    Result := FKeys[Index]
  else
    Result := NIL;
end;

procedure TPegtopGradientOpacityKeyList.SetKey(Index: Integer; Value: TPegtopOpacityKey);
begin
  if (Index >= 0) and (Index < FKeys.Count) then
    TPegtopOpacityKey(FKeys[Index]).Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientDefinition
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientDefinition.Create(AOwner: TPegtopCustomColorGradient);
begin
  FOwner := AOwner;
  FKeys := CreateKeys;
  FFrequency := 1;
  FSmoothness := 256;
end;

destructor TPegtopColorGradientDefinition.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TPegtopColorGradientDefinition.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientDefinition;
begin
  if Source is TPegtopColorGradientDefinition then begin
    BeginUpdate;
    try
      S := TPegtopColorGradientDefinition(Source);
      FFrequency := S.FFrequency;
      FSmoothness := S.Smoothness;
      Update([pgcDefinition]);
      FKeys.Assign(S.FKeys);
    finally
      EndUpdate;
    end;
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopColorGradientDefinition.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('KeyData', FKeys.LoadFromStream, FKeys.SaveToStream, True);
end;

procedure TPegtopColorGradientDefinition.ApplyChanges;
begin
  FKeys.ApplyChanges;
end;

procedure TPegtopColorGradientDefinition.Update(Changes: TPegtopColorGradientChanges);
begin
  FOwner.Update(Changes);
end;

procedure TPegtopColorGradientDefinition.BeginUpdate;
begin
  FOwner.BeginUpdate;
end;

procedure TPegtopColorGradientDefinition.EndUpdate;
begin
  FOwner.EndUpdate;
end;

procedure TPegtopColorGradientDefinition.SetFrequency(Value: Integer);
begin
  if Value < 1 then Value := 1 else if Value > 10 then Value := 10;
  if FFrequency <> Value then begin
    FFrequency := Value;
    Update([pgcDefinition]);
  end;
end;

procedure TPegtopColorGradientDefinition.SetSmoothness(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FSmoothness <> Value then begin
    FSmoothness := Value;
    Update([pgcDefinition]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientColor
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientColor.Create(AOwner: TPegtopCustomColorGradient);
begin
  inherited;
  FMode := pgmRGB;
  FNoise := TPegtopColorGradientColorNoise.Create(Self);
end;

destructor TPegtopColorGradientColor.Destroy;
begin
  FNoise.Free;
  inherited;
end;

procedure TPegtopColorGradientColor.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientColor;
begin
  if Source is TPegtopColorGradientColor then begin
    BeginUpdate;
    try
      inherited;
      S := TPegtopColorGradientColor(Source);
      FMode := S.FMode;
      FNoise.Assign(S.FNoise);
    finally
      EndUpdate;
    end;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopColorGradientColor.SaveToStream(Stream: TStream);
var
  P: Integer;
  R: TPegtopColorRecord;
begin
  R.Frequency := FFrequency;
  R.Smoothness := FSmoothness;
  R.Mode := Ord(FMode);
  P := ForgetChunkHeader(Stream);
  WriteChunkHeader(Stream, ColorDataChunkId, SizeOf(R));
  Stream.Write(R, SizeOf(R));
  FKeys.SaveToStream(Stream);
  FNoise.SaveToStream(Stream);
  CompleteChunkHeader(Stream, P, ColorChunkId);
end;

function TPegtopColorGradientColor.CreateKeys: TPegtopGradientKeyList;
begin
  Result := TPegtopGradientColorKeyList.Create(Self);
end;

procedure TPegtopColorGradientColor.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineDataChunk(ColorDataChunkId, SizeOf(TPegtopColorRecord), ChunkReadData, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(KeysChunkId, FKeys.DefineChunks, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(ColorNoiseChunkId, FNoise.DefineChunks, [pcoUnique]);
end;

procedure TPegtopColorGradientColor.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  FFrequency := TPegtopColorRecord(Data).Frequency;
  FSmoothness := TPegtopColorRecord(Data).Smoothness;
  FMode := TPegtopColorGradientMode(TPegtopColorRecord(Data).Mode);
end;

procedure TPegtopColorGradientColor.InterpolateLinearRGB(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  Color1, Color2: TPegtopColor;
begin
  P := Data;
  Inc(P, Index);
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  P^.R := Color1.R + (Color2.R - Color1.R) * Rate div 256;
  P^.G := Color1.G + (Color2.G - Color1.G) * Rate div 256;
  P^.B := Color1.B + (Color2.B - Color1.B) * Rate div 256;
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateLinearBGR(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  Color1, Color2: TPegtopColor;
begin
  P := Data;
  Inc(P, Index);
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  P^.R := Color1.B + (Color2.B - Color1.B) * Rate div 256;
  P^.G := Color1.G + (Color2.G - Color1.G) * Rate div 256;
  P^.B := Color1.R + (Color2.R - Color1.R) * Rate div 256;
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateLinearRGB64(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  Color1, Color2: TPegtopColor;
begin
  P := Data;
  Inc(P, Index);
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  P^.R := Color1.R * 256 + (Color2.R - Color1.R) * Rate div 256;
  P^.G := Color1.G * 256 + (Color2.G - Color1.G) * Rate div 256;
  P^.B := Color1.B * 256 + (Color2.B - Color1.B) * Rate div 256;
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateLinearBGR64(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  Color1, Color2: TPegtopColor;
begin
  P := Data;
  Inc(P, Index);
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  P^.R := Color1.B * 256 + (Color2.B - Color1.B) * Rate div 256;
  P^.G := Color1.G * 256 + (Color2.G - Color1.G) * Rate div 256;
  P^.B := Color1.R * 256 + (Color2.R - Color1.R) * Rate div 256;
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateLinearHSB(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  HDiff: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  if S1 = 0 then H1 := H2
  else if S2 = 0 then H2 := H1;
  HDiff := (H2 - H1) and HueMask;
  if HDiff > (HueMask shr 1) then HDiff := -((H1 - H2) and HueMask);
  P^ := ConvertHSBToColor(
    (H1 + HDiff * Rate div 16384) and HueMask,
    S1 + (S2 - S1) * Rate div 16384,
    B1 + (B2 - B1) * Rate div 16384
  );
end;

procedure TPegtopColorGradientColor.InterpolateLinearBSH(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  HDiff: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  if S1 = 0 then H1 := H2
  else if S2 = 0 then H2 := H1;
  HDiff := (H2 - H1) and HueMask;
  if HDiff > (HueMask shr 1) then HDiff := -((H1 - H2) and HueMask);
  P^ := SwapColorBytes(ConvertHSBToColor(
    (H1 + HDiff * Rate div 16384) and HueMask,
    S1 + (S2 - S1) * Rate div 16384,
    B1 + (B2 - B1) * Rate div 16384
  ));
end;

procedure TPegtopColorGradientColor.InterpolateLinearHSB64(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  HDiff: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  if S1 = 0 then H1 := H2
  else if S2 = 0 then H2 := H1;
  HDiff := (H2 - H1) and HueMask;
  if HDiff > (HueMask shr 1) then HDiff := -((H1 - H2) and HueMask);
  P^ := ConvertHSBToColor64(
    (H1 + HDiff * Rate div 16384) and HueMask,
    S1 + (S2 - S1) * Rate div 16384,
    B1 + (B2 - B1) * Rate div 16384
  );
end;

procedure TPegtopColorGradientColor.InterpolateLinearBSH64(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  HDiff: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  if S1 = 0 then H1 := H2
  else if S2 = 0 then H2 := H1;
  HDiff := (H2 - H1) and HueMask;
  if HDiff > (HueMask shr 1) then HDiff := -((H1 - H2) and HueMask);
  P^ := SwapColorWords(ConvertHSBToColor64(
    (H1 + HDiff * Rate div 16384) and HueMask,
    S1 + (S2 - S1) * Rate div 16384,
    B1 + (B2 - B1) * Rate div 16384
  ));
end;

procedure TPegtopColorGradientColor.InterpolateCubicRGB(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  Color0, Color1, Color2, Color3: TPegtopColor;
  P0, P1, P2, P3: Integer;
begin
  P := Data;
  Inc(P, Index);
  Color0 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color));
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  Color3 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color));
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  P^.R := InterpolateSmoothSaturated(P0, Color0.R, P1, Color1.R, P2, Color2.R, P3, Color3.R, FSmoothness, Rate, 0, $FF);
  P^.G := InterpolateSmoothSaturated(P0, Color0.G, P1, Color1.G, P2, Color2.G, P3, Color3.G, FSmoothness, Rate, 0, $FF);
  P^.B := InterpolateSmoothSaturated(P0, Color0.B, P1, Color1.B, P2, Color2.B, P3, Color3.B, FSmoothness, Rate, 0, $FF);
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateCubicBGR(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  Color0, Color1, Color2, Color3: TPegtopColor;
  P0, P1, P2, P3: Integer;
begin
  P := Data;
  Inc(P, Index);
  Color0 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color));
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  Color3 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color));
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  P^.R := InterpolateSmoothSaturated(P0, Color0.B, P1, Color1.B, P2, Color2.B, P3, Color3.B, FSmoothness, Rate, 0, $FF);
  P^.G := InterpolateSmoothSaturated(P0, Color0.G, P1, Color1.G, P2, Color2.G, P3, Color3.G, FSmoothness, Rate, 0, $FF);
  P^.B := InterpolateSmoothSaturated(P0, Color0.R, P1, Color1.R, P2, Color2.R, P3, Color3.R, FSmoothness, Rate, 0, $FF);
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateCubicRGB64(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  Color0, Color1, Color2, Color3: TPegtopColor;
  P0, P1, P2, P3: Integer;
begin
  P := Data;
  Inc(P, Index);
  Color0 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color));
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  Color3 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color));
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  P^.R := InterpolateSmoothSaturated(P0, Color0.R * 256, P1, Color1.R * 256, P2, Color2.R * 256, P3, Color3.R * 256, FSmoothness, Rate, 0, $FF00);
  P^.G := InterpolateSmoothSaturated(P0, Color0.G * 256, P1, Color1.G * 256, P2, Color2.G * 256, P3, Color3.G * 256, FSmoothness, Rate, 0, $FF00);
  P^.B := InterpolateSmoothSaturated(P0, Color0.B * 256, P1, Color1.B * 256, P2, Color2.B * 256, P3, Color3.B * 256, FSmoothness, Rate, 0, $FF00);
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateCubicBGR64(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  Color0, Color1, Color2, Color3: TPegtopColor;
  P0, P1, P2, P3: Integer;
begin
  P := Data;
  Inc(P, Index);
  Color0 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color));
  Color1 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color));
  Color2 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color));
  Color3 := TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color));
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  P^.R := InterpolateSmoothSaturated(P0, Color0.B * 256, P1, Color1.B * 256, P2, Color2.B * 256, P3, Color3.B * 256, FSmoothness, Rate, 0, $FF00);
  P^.G := InterpolateSmoothSaturated(P0, Color0.G * 256, P1, Color1.G * 256, P2, Color2.G * 256, P3, Color3.G * 256, FSmoothness, Rate, 0, $FF00);
  P^.B := InterpolateSmoothSaturated(P0, Color0.R * 256, P1, Color1.R * 256, P2, Color2.R * 256, P3, Color3.R * 256, FSmoothness, Rate, 0, $FF00);
  P^.A := 0;
end;

procedure TPegtopColorGradientColor.InterpolateCubicHSB(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  P0, P1, P2, P3: Integer;
  H0, S0, B0: Integer;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  H3, S3, B3: Integer;
  HDiff0, HDiff1, HDiff2: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color)), H0, S0, B0);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color)), H3, S3, B3);
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  GetHueDiff(S0, S1, H0, H1, HDiff0);
  GetHueDiff(S1, S2, H1, H2, HDiff1);
  GetHueDiff(S2, S3, H2, H3, HDiff2);
  P^ := ConvertHSBToColor(
    InterpolateSmooth(P0, H1 - HDiff0, P1, H1, P2, H1 + HDiff1, P3, H1 + HDiff1 + HDiff2, FSmoothness, Rate) and HueMask,
    InterpolateSmoothSaturated(P0, S0, P1, S1, P2, S2, P3, S3, FSmoothness, Rate, 0, $FF00),
    InterpolateSmoothSaturated(P0, B0, P1, B1, P2, B2, P3, B3, FSmoothness, Rate, 0, $FF00)
  );
end;

procedure TPegtopColorGradientColor.InterpolateCubicBSH(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor;
  P0, P1, P2, P3: Integer;
  H0, S0, B0: Integer;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  H3, S3, B3: Integer;
  HDiff0, HDiff1, HDiff2: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color)), H0, S0, B0);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color)), H3, S3, B3);
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  GetHueDiff(S0, S1, H0, H1, HDiff0);
  GetHueDiff(S1, S2, H1, H2, HDiff1);
  GetHueDiff(S2, S3, H2, H3, HDiff2);
  P^ := SwapColorBytes(ConvertHSBToColor(
    InterpolateSmooth(P0, H1 - HDiff0, P1, H1, P2, H1 + HDiff1, P3, H1 + HDiff1 + HDiff2, FSmoothness, Rate) and HueMask,
    InterpolateSmoothSaturated(P0, S0, P1, S1, P2, S2, P3, S3, FSmoothness, Rate, 0, $FF00),
    InterpolateSmoothSaturated(P0, B0, P1, B1, P2, B2, P3, B3, FSmoothness, Rate, 0, $FF00)
  ));
end;

procedure TPegtopColorGradientColor.InterpolateCubicHSB64(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  P0, P1, P2, P3: Integer;
  H0, S0, B0: Integer;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  H3, S3, B3: Integer;
  HDiff0, HDiff1, HDiff2: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color)), H0, S0, B0);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color)), H3, S3, B3);
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  GetHueDiff(S0, S1, H0, H1, HDiff0);
  GetHueDiff(S1, S2, H1, H2, HDiff1);
  GetHueDiff(S2, S3, H2, H3, HDiff2);
  P^ := ConvertHSBToColor64(
    InterpolateSmooth(P0, H1 - HDiff0, P1, H1, P2, H1 + HDiff1, P3, H1 + HDiff1 + HDiff2, FSmoothness, Rate) and HueMask,
    InterpolateSmoothSaturated(P0, S0, P1, S1, P2, S2, P3, S3, FSmoothness, Rate, 0, $FF00),
    InterpolateSmoothSaturated(P0, B0, P1, B1, P2, B2, P3, B3, FSmoothness, Rate, 0, $FF00)
  );
end;

procedure TPegtopColorGradientColor.InterpolateCubicBSH64(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: PPegtopColor64;
  P0, P1, P2, P3: Integer;
  H0, S0, B0: Integer;
  H1, S1, B1: Integer;
  H2, S2, B2: Integer;
  H3, S3, B3: Integer;
  HDiff0, HDiff1, HDiff2: Integer;
begin
  P := Data;
  Inc(P, Index);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key0).Color)), H0, S0, B0);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key1).Color)), H1, S1, B1);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key2).Color)), H2, S2, B2);
  ConvertColorToHSB(TPegtopColor(ColorToRGB(TPegtopColorKey(Key3).Color)), H3, S3, B3);
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  if P0 > P1 then Dec(P0, PegtopColorGradientPositionRange);
  if P3 < P2 then Inc(P3, PegtopColorGradientPositionRange);
  GetHueDiff(S0, S1, H0, H1, HDiff0);
  GetHueDiff(S1, S2, H1, H2, HDiff1);
  GetHueDiff(S2, S3, H2, H3, HDiff2);
  P^ := SwapColorWords(ConvertHSBToColor64(
    InterpolateSmooth(P0, H1 - HDiff0, P1, H1, P2, H1 + HDiff1, P3, H1 + HDiff1 + HDiff2, FSmoothness, Rate) and HueMask,
    InterpolateSmoothSaturated(P0, S0, P1, S1, P2, S2, P3, S3, FSmoothness, Rate, 0, $FF00),
    InterpolateSmoothSaturated(P0, B0, P1, B1, P2, B2, P3, B3, FSmoothness, Rate, 0, $FF00)
  ));
end;

procedure TPegtopColorGradientColor.GetColors(var ColorArray: TPegtopColorArray;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
  const SwapRedBlue: Boolean = False);
var
  I, J: Integer;
  Size: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  Size := Length(ColorArray);
  if Size > 0 then begin
    if pgoSymmetrical in Options then Size := (Size + 1) div 2;
    // generate gradient:
    if FKeys.Count > 1 then begin // "normal" gradient
      if FSmoothness = 0 then begin
        case FMode of
          pgmRGB:
            if SwapRedBlue then
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 256, InterpolateLinearBGR)
            else
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 256, InterpolateLinearRGB);
          pgmHSB:
            if SwapRedBlue then
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateLinearBSH)
            else
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateLinearHSB);
        end;
      end
      else begin
        case FMode of
          pgmRGB:
            if SwapRedBlue then
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 256, InterpolateCubicBGR)
            else
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 256, InterpolateCubicRGB);
          pgmHSB:
            if SwapRedBlue then
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateCubicBSH)
            else
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateCubicHSB);
        end;
      end;
    end
    else if FKeys.Count > 0 then begin // single color
      for I := 0 To Size - 1 do begin
        if SwapRedBlue then
          ColorArray[I] := SwapColorBytes(TPegtopColor(ColorToRGB(Keys[0].Color))).Def
        else
          ColorArray[I] := ColorToRGB(Keys[0].Color);
      end;
    end
    else begin // no colores defined
      for I := 0 To Size - 1 do ColorArray[I] := $FFFFFF;
    end;
    // add noise:
    if not (pgoIgnoreNoise in Options) then begin
      if FNoise.FStrength > 0 then begin
        if Mode = pgmHSB then FNoise.ApplyHSB(ColorArray, Options, Iterations, SwapRedBlue)
        else FNoise.ApplyRGB(ColorArray, Options, Iterations, SwapRedBlue);
      end;
    end;
    // mirror:
    if pgoSymmetrical in Options then begin
      J := Length(ColorArray);
      for I := 0 to Size - 1 do begin
        Dec(J);
        ColorArray[J] := ColorArray[I];
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColor.GetColors64(var ColorArray: TPegtopColor64Array;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
  const SwapRedBlue: Boolean = False);
var
  I, J: Integer;
  Size: Integer;
  C: TPegtopColor64;
begin
  if Iterations < 1 then Iterations := 1;
  Size := Length(ColorArray);
  if Size > 0 then begin
    if pgoSymmetrical in Options then Size := (Size + 1) div 2;
    // generate gradient:
    if FKeys.Count > 1 then begin // "normal" gradient
      if FSmoothness = 0 then begin
        case FMode of
          pgmRGB:
            if SwapRedBlue then
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 65536, InterpolateLinearBGR64)
            else
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 65536, InterpolateLinearRGB64);
          pgmHSB:
            if SwapRedBlue then
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateLinearBSH64)
            else
              FKeys.InterpolateLinear(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateLinearHSB64);
        end;
      end
      else begin
        case FMode of
          pgmRGB:
            if SwapRedBlue then
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 65536, InterpolateCubicBGR64)
            else
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 65536, InterpolateCubicRGB64);
          pgmHSB:
            if SwapRedBlue then
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateCubicBSH64)
            else
              FKeys.InterpolateCubic(Addr(ColorArray[0]), Size, Options, Iterations, 16384, InterpolateCubicHSB64);
        end;
      end;
    end
    else if FKeys.Count > 0 then begin // single color
      if SwapRedBlue then begin
        C.R := TPegtopColor(Keys[0].Color).B * 256;
        C.G := TPegtopColor(Keys[0].Color).G * 256;
        C.B := TPegtopColor(Keys[0].Color).R * 256;
      end
      else begin
        C.R := TPegtopColor(Keys[0].Color).R * 256;
        C.G := TPegtopColor(Keys[0].Color).G * 256;
        C.B := TPegtopColor(Keys[0].Color).B * 256;
      end;
      C.A := 0;
      for I := 0 To Size - 1 do begin
        ColorArray[I] := C;
      end;
    end
    else begin // no colores defined
      for I := 0 To Size - 1 do ColorArray[I].Def := $FF00FF00FF00;
    end;
    // add noise:
    if not (pgoIgnoreNoise in Options) then begin
      if FNoise.FStrength > 0 then begin
        if Mode = pgmHSB then FNoise.ApplyHSB64(ColorArray, Options, Iterations, SwapRedBlue)
        else FNoise.ApplyRGB64(ColorArray, Options, Iterations, SwapRedBlue);
      end;
    end;
    // mirror:
    if pgoSymmetrical in Options then begin
      J := Length(ColorArray);
      for I := 0 to Size - 1 do begin
        Dec(J);
        ColorArray[J] := ColorArray[I];
      end;
    end;
  end;
end;

procedure TPegtopColorGradientColor.SetMode(Value: TPegtopColorGradientMode);
begin
  if FMode <> Value then begin
    FMode := Value;
    Update([pgcDefinition]);
  end;
end;

procedure TPegtopColorGradientColor.SetNoise(Value: TPegtopColorGradientColorNoise);
begin
  FNoise.Assign(Value);
end;

function TPegtopColorGradientColor.GetKeys: TPegtopGradientColorKeyList;
begin
  Result := TPegtopGradientColorKeyList(FKeys);
end;

procedure TPegtopColorGradientColor.SetKeys(Value: TPegtopGradientColorKeyList);
begin
  FKeys.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientOpacity
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientOpacity.Create(AOwner: TPegtopCustomColorGradient);
begin
  inherited;
  FNoise := TPegtopColorGradientOpacityNoise.Create(Self);
end;

destructor TPegtopColorGradientOpacity.Destroy;
begin
  FNoise.Free;
  inherited;
end;

procedure TPegtopColorGradientOpacity.Assign(Source: TPersistent);
var
  S: TPegtopColorGradientOpacity;
begin
  if Source is TPegtopColorGradientOpacity then begin
    BeginUpdate;
    try
      inherited;
      S := TPegtopColorGradientOpacity(Source);
      FNoise.Assign(S.FNoise);
    finally
      EndUpdate;
    end;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopColorGradientOpacity.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineDataChunk(OpacityDataChunkId, SizeOf(TPegtopOpacityRecord), ChunkReadData, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(KeysChunkId, FKeys.DefineChunks, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(OpacityNoiseChunkId, FNoise.DefineChunks, [pcoUnique]);
end;

procedure TPegtopColorGradientOpacity.ChunkReadData(Sender: TObject; Id: TPegtopChunkId;
  Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  FFrequency := TPegtopOpacityRecord(Data).Frequency;
  FSmoothness := TPegtopOpacityRecord(Data).Smoothness;
end;

function TPegtopColorGradientOpacity.CreateKeys: TPegtopGradientKeyList;
begin
  Result := TPegtopGradientOpacityKeyList.Create(Self);
end;

procedure TPegtopColorGradientOpacity.InterpolateLinearOpacity(Data: Pointer; Index: Integer;
  Key1, Key2: TPegtopGradientKey; Rate, Max: Integer);
var
  P: ^Integer;
begin
  P := Data;
  Inc(P, Index);
  P^ := (TPegtopOpacityKey(Key1).Opacity * Max
    + (TPegtopOpacityKey(Key2).Opacity - TPegtopOpacityKey(Key1).Opacity) * Rate)
    div 256;
end;

procedure TPegtopColorGradientOpacity.InterpolateCubicOpacity(Data: Pointer; Index: Integer;
  Key0, Key1, Key2, Key3: TPegtopGradientKey; Rate, Max: Integer);
var
  P: ^Integer;
  O0, O1, O2, O3: Integer;
  P0, P1, P2, P3: Integer;
  O: Integer;
begin
  P := Data;
  Inc(P, Index);
  O0 := TPegtopOpacityKey(Key0).Opacity * Max div 256;
  O1 := TPegtopOpacityKey(Key1).Opacity * Max div 256;
  O2 := TPegtopOpacityKey(Key2).Opacity * Max div 256;
  O3 := TPegtopOpacityKey(Key3).Opacity * Max div 256;
  P0 := Key0.Position;
  P1 := Key1.Position;
  P2 := Key2.Position;
  P3 := Key3.Position;
  O := InterpolateSmooth(P0, O0, P1, O1, P2, O2, P3, O3, FSmoothness, Rate);
  if O < 0 then P^ := 0 else if O >= Max then P^ := Max - 1 else P^ := O;
end;

procedure TPegtopColorGradientOpacity.GetOpacities(var OpacityArray: TPegtopOpacityArray;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1; Max: Integer = 256);
var
  I, J, A: Integer;
  Size: Integer;
  NoiseArray: TPegtopNoiseArray;
  P: ^Integer;
  A1, O1: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  Size := Length(OpacityArray);
  if Size > 0 then begin
    if pgoIgnoreOpacity in Options then begin
      for I := 0 to Size - 1 do OpacityArray[I] := Max;
    end
    else begin
      if pgoSymmetrical in Options then Size := (Size + 1) div 2;
      // generate gradient:
      if FKeys.Count > 1 then begin // "normal" gradient
        if FSmoothness = 0 then
          FKeys.InterpolateLinear(Addr(OpacityArray[0]), Size, Options, Iterations, Max, InterpolateLinearOpacity)
        else
          FKeys.InterpolateCubic(Addr(OpacityArray[0]), Size, Options, Iterations, Max, InterpolateCubicOpacity);
      end
      else if FKeys.Count > 0 then begin // single opacity
        for I := 0 To Size - 1 do OpacityArray[I] := Keys[0].Opacity * Max div 256;
      end
      else begin // no opacities defined
        for I := 0 To Size - 1 do OpacityArray[I] := Max;
      end;
      // add noise:
      if not (pgoIgnoreNoise in Options) then begin
        A := FNoise.FStrength;
        if A > 0 then begin
          O1 := FNoise.OpacityMin * $FF;
          A1 := FNoise.OpacityMax - FNoise.OpacityMin;
          SetLength(NoiseArray, Size);
          FNoise.GetPerlinNoise(NoiseArray, 4, Iterations);
          P := Addr(OpacityArray[0]);
          if pgoReverse in Options then begin
            for I := Size - 1 downto 0 do begin
              P^ := P^ + (MulDiv(NoiseArray[I] * A1 div 256 + O1, Max, $FF00) - P^) * A div 256;
              Inc(P);
            end;
          end
          else begin
            for I := 0 to Size - 1 do begin
              P^ := P^ + (MulDiv(NoiseArray[I] * A1 div 256 + O1, Max, $FF00) - P^) * A div 256;
              Inc(P);
            end;
          end;
        end;
      end;
      // mirror:
      if pgoSymmetrical in Options then begin
        J := Length(OpacityArray);
        for I := 0 to Size - 1 do begin
          Dec(J);
          OpacityArray[J] := OpacityArray[I];
        end;
      end;
    end;
  end;
end;

procedure TPegtopColorGradientOpacity.SaveToStream(Stream: TStream);
var
  P: Integer;
  R: TPegtopOpacityRecord;
begin
  R.Frequency := FFrequency;
  R.Smoothness := FSmoothness;
  P := ForgetChunkHeader(Stream);
  WriteChunkHeader(Stream, OpacityDataChunkId, SizeOf(R));
  Stream.Write(R, SizeOf(R));
  FKeys.SaveToStream(Stream);
  FNoise.SaveToStream(Stream);
  CompleteChunkHeader(Stream, P, OpacityChunkId);
end;

procedure TPegtopColorGradientOpacity.SetNoise(Value: TPegtopColorGradientOpacityNoise);
begin
  FNoise.Assign(Value);
end;

function TPegtopColorGradientOpacity.GetKeys: TPegtopGradientOpacityKeyList;
begin
  Result := TPegtopGradientOpacityKeyList(FKeys);
end;

procedure TPegtopColorGradientOpacity.SetKeys(Value: TPegtopGradientOpacityKeyList);
begin
  FKeys.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorGradient
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorGradient.Create(const AColors: array of TColor);
begin
  FColor := TPegtopColorGradientColor.Create(Self);
  FOpacity := TPegtopColorGradientOpacity.Create(Self);
  FListeners := TList.Create;
  FColor.Keys.Define(AColors);
  with Opacity.Keys.Add do Position := 0;
  with Opacity.Keys.Add do Position := PegtopColorGradientPositionRange;
end;

destructor TPegtopCustomColorGradient.Destroy;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    Listener.Free;
  end;
  FListeners.Free;
  FOpacity.Free;
  FColor.Free;
  inherited;
end;

procedure TPegtopCustomColorGradient.Assign(Source: TPersistent);
var
  S: TPegtopCustomColorGradient;
begin
  if Source is TPegtopCustomColorGradient then begin
    BeginUpdate;
    try
      S := TPegtopCustomColorGradient(Source);
      FName := S.FName;
      FSeamless := S.FSeamless;
      FColor.Assign(S.FColor);
      FOpacity.Assign(S.FOpacity);
    finally
      EndUpdate;
    end;
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopCustomColorGradient.SaveToStream(Stream: TStream);
var
  P: Integer;
  R: TPegtopGradientRecord;
begin
  P := ForgetChunkHeader(Stream);
  FillChar(R, SizeOf(R), 0);
  R.Name := FName;
  R.Seamless := FSeamless;
  WriteDataChunk(Stream, GradientDataChunkId, R, SizeOf(R));
  FColor.SaveToStream(Stream);
  FOpacity.SaveToStream(Stream);
  CompleteChunkHeader(Stream, P, GradientChunkId);
end;

procedure TPegtopCustomColorGradient.DefineChunks(Sender: TObject; Id: TPegtopChunkId; Index: Integer; ContainerChunkReader: TPegtopContainerChunkReader);
begin
  ContainerChunkReader.DefineDataChunk(GradientDataChunkId, SizeOf(TPegtopGradientRecord), ChunkReadData, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(ColorChunkId, FColor.DefineChunks, [pcoUnique]);
  ContainerChunkReader.DefineContainerChunk(OpacityChunkId, FOpacity.DefineChunks, [pcoUnique]);
end;

procedure TPegtopCustomColorGradient.LoadFromStream(Stream: TStream; Index: Integer = 0);
var
  Reader: TPegtopContainerChunkReader;
  Id: TPegtopChunkId;
  List: TPegtopColorGradientList;
  Gradient: TPegtopCustomColorGradient;
begin
  if Stream.Read(Id, SizeOf(Id)) <> SizeOf(Id) then begin
    RaiseInvalidStream;
  end
  else begin
    Stream.Seek(-SizeOf(Id), soFromCurrent);
    if CompareChunkIds(Id, GradientChunkId) then begin
      Reader := TPegtopContainerChunkReader.Create(NIL);
      try
        DefineChunks(NIL, Id, 0, Reader);
        BeginUpdate;
        try
          if not Reader.Read(Stream, GradientChunkId) then RaiseInvalidStream;
        finally
          EndUpdate;
        end;
      finally
        Reader.Free;
      end;
    end
    else if CompareChunkIds(Id, GradientCollectionChunkId) then begin
      List := TPegtopColorGradientList.Create(NIL);
      try
        List.Items.LoadFromStream(Stream);
        if (Index < 0) or (Index >= List.Items.Count) then Index := 0;
        Gradient := List.Items[Index].Gradient;
        if Gradient = NIL then raise EStreamError.CreateFmt('The gradient list does not contain a gradient with index %d.', [Index]);
        Assign(Gradient);
      finally
        List.Free;
      end;
    end
    else if CompareChunkIds(Id, 'XGR1') then begin
      LoadOldXFaderFormat(Stream); // please don't punish me ;-)
    end
    else begin
      RaiseInvalidStream;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.LoadOldXFaderFormat(Stream: TStream);
type
  TLongIntArray = array[0..32767] of LongInt;
  PLongIntArray = ^TLongIntArray;
var
  Id: TPegtopChunkId;
  C: Longint;
  I: Integer;
  PositionArray: PLongIntArray;
  ColorArray: PLongIntArray;
  OpacityValue: Integer;
  OpacityIsConstant: Boolean;
begin
  BeginUpdate;
  try
    FName := '';
    FSeamless := False;
    FColor.Keys.Clear;
    FColor.Noise.Clear;
    FColor.Mode := pgmRGB;
    FColor.Frequency := 1;
    FColor.Smoothness := 0;
    FOpacity.Keys.Clear;
    FOpacity.Noise.Clear;
    FOpacity.Frequency := 1;
    FOpacity.Smoothness := 0;
    if (Stream.Read(Id, SizeOf(Id)) <> SizeOf(Id))
    or (not CompareChunkIds(Id, 'XGR1')) then RaiseInvalidStream;
    if (Stream.Read(C, SizeOf(C)) <> SizeOf(C))
    or (C > 10000) then RaiseInvalidStream;
    GetMem(PositionArray, SizeOf(LongInt) * C);
    GetMem(ColorArray, SizeOf(LongInt) * C);
    try
      Stream.Read(PositionArray^, SizeOf(LongInt) * C);
      Stream.Read(ColorArray^, SizeOf(LongInt) * C);
      for I := 0 to C - 1 do begin
        with FColor.Keys.Add do begin
          Position := MulDiv(PositionArray^[I], PegtopColorGradientPositionRange, 4095);
          Color := ColorArray^[I] and $FFFFFF;
        end;
        with FOpacity.Keys.Add do begin
          Position := MulDiv(PositionArray^[I], PegtopColorGradientPositionRange, 4095);
          Opacity := Round((255 - (ColorArray^[I] shr 24)) * 256 / 255);
        end;
      end;
    finally
      FreeMem(PositionArray);
      FreeMem(ColorArray);
    end;
    // Optimize opacity:
    // (especially important since the old format stores opacity for every color key,
    // which often results in unneccessary opacity keys for full opaque gradients)
    if FOpacity.Keys.Count > 0 then begin
      OpacityValue := FOpacity.Keys[0].Opacity;
      OpacityIsConstant := True;
      I := 1;
      while OpacityIsConstant and (I < FOpacity.Keys.Count) do begin
        OpacityIsConstant := FOpacity.Keys[I].Opacity = OpacityValue;
        Inc(I);
      end;
      if OpacityIsConstant then begin
        FOpacity.Keys.Define([OpacityValue, OpacityValue]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopCustomColorGradient.RaiseInvalidStream;
begin
  raise EStreamError.Create('The color gradient file is not valid.');
end;

procedure TPegtopCustomColorGradient.ChunkReadData(Sender: TObject; Id: TPegtopChunkId; Index: Integer; const Data; Size: Integer; var Valid: Boolean);
begin
  if Size >= 32 then FName := TPegtopGradientRecord(Data).Name else FName := '';
  FSeamless := (Size >= 36) and TPegtopGradientRecord(Data).Seamless;
end;

procedure TPegtopCustomColorGradient.SaveToFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    if Name = '' then Name := ExtractFileNameWithoutExt(FileName); 
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPegtopCustomColorGradient.LoadFromFile(FileName: String; Index: Integer = 0);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Index);
    if Name = '' then Name := ExtractFileNameWithoutExt(FileName);
  finally
    Stream.Free;
  end;
end;

procedure TPegtopCustomColorGradient.Update(Changes: TPegtopColorGradientChanges);
begin
  FUpdateChanges := FUpdateChanges + Changes;
  if FUpdateLevel = 0 then begin
    FColor.ApplyChanges;
    FOpacity.ApplyChanges;
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyListeners;
    FUpdateChanges := [];
  end;
end;

procedure TPegtopCustomColorGradient.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TPegtopCustomColorGradient.EndUpdate;
begin
  Dec(FUpdateLevel);
  Update([]);
end;

procedure TPegtopCustomColorGradient.AddListener(Event: TNotifyEvent);
var
  Listener: TPegtopNotifyListener;
begin
  Listener := TPegtopNotifyListener.Create(Event);
  FListeners.Add(Listener);
end;

procedure TPegtopCustomColorGradient.RemoveListener(Event: TNotifyEvent);
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := FListeners.Count - 1 downto 0 do begin
    Listener := FListeners[I];
    if (TMethod(Listener.Event).Code = TMethod(Event).Code)
    and (TMethod(Listener.Event).Data = TMethod(Event).Data) then
      FListeners.Delete(I);
  end;
end;

procedure TPegtopCustomColorGradient.NotifyListeners;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    if Assigned(Listener.Event) then Listener.Event(Self);
  end;
end;

procedure TPegtopCustomColorGradient.Draw32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColorArray;
begin
  SetLength(Colors, GradientArraySize);
  Color.GetColors(Colors, Options, Iterations, True);
  DrawColors32(Colors, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.DrawDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColor64Array;
begin
  SetLength(Colors, GradientArraySize);
  Color.GetColors64(Colors, Options, Iterations, True);
  DrawColorsDithered32(Colors, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.Blend32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColorArray;
  Opacities: TPegtopOpacityArray;
begin
  SetLength(Colors, GradientArraySize);
  Color.GetColors(Colors, Options, Iterations, True);
  SetLength(Opacities, GradientArraySize);
  Opacity.GetOpacities(Opacities, Options, Iterations);
  BlendColors32(Colors, Opacities, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.BlendDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColor64Array;
  Opacities: TPegtopOpacityArray;
begin
  SetLength(Colors, GradientArraySize);
  Color.GetColors64(Colors, Options, Iterations, True);
  SetLength(Opacities, GradientArraySize);
  Opacity.GetOpacities(Opacities, Options, Iterations, $10000);
  BlendColorsDithered32(Colors, Opacities, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.GrayscaleOpacity32(const TransparentColor, OpaqueColor: TPegtopColor;
  const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColorArray;
  Opacities: TPegtopOpacityArray;
  I: Integer;
begin
  SetLength(Opacities, GradientArraySize);
  Opacity.GetOpacities(Opacities, Options, Iterations, $10000);
  SetLength(Colors, GradientArraySize);
  for I := 0 to GradientArraySize - 1 do begin
    TPegtopColor(Colors[I]).R := TransparentColor.B + (OpaqueColor.B - TransparentColor.B) * Opacities[I] div $10000;
    TPegtopColor(Colors[I]).G := TransparentColor.G + (OpaqueColor.G - TransparentColor.G) * Opacities[I] div $10000;
    TPegtopColor(Colors[I]).B := TransparentColor.R + (OpaqueColor.R - TransparentColor.R) * Opacities[I] div $10000;
    TPegtopColor(Colors[I]).A := 0;
  end;
  DrawColors32(Colors, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.GrayscaleOpacityDithered32(const TransparentColor, OpaqueColor: TPegtopColor;
  const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1;
  const Alpha: Byte = 0);
var
  Colors: TPegtopColor64Array;
  Opacities: TPegtopOpacityArray;
  I: Integer;
begin
  SetLength(Opacities, GradientArraySize);
  Opacity.GetOpacities(Opacities, Options, Iterations, $10000);
  SetLength(Colors, GradientArraySize);
  for I := 0 to GradientArraySize - 1 do begin
    Colors[I].R := TransparentColor.B * $100 + (OpaqueColor.B - TransparentColor.B) * Opacities[I] div $100;
    Colors[I].G := TransparentColor.G * $100 + (OpaqueColor.G - TransparentColor.G) * Opacities[I] div $100;
    Colors[I].B := TransparentColor.R * $100 + (OpaqueColor.R - TransparentColor.R) * Opacities[I] div $100;
    Colors[I].A := 0;
  end;
  DrawColorsDithered32(Colors, Origin, Pitch, ClipRect, Point1, Point2, Style, Options, Iterations, Alpha);
end;

procedure TPegtopCustomColorGradient.Draw(const Canvas: TCanvas; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
  const Iterations: Integer = 1);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
begin
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      if Dithered then
        DrawDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations)
      else
        Draw32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.Blend(const Canvas: TCanvas; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
  const Iterations: Integer = 1);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
begin
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Canvas, ClipRect);
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      if Dithered then
        BlendDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations)
      else
        Blend32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.GrayscaleOpacity(const TransparentColor, OpaqueColor: TColor;
  const Canvas: TCanvas; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
  const Iterations: Integer = 1);
var
  Bitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
begin
  if (ClipRect.Right > ClipRect.Left) and (ClipRect.Bottom > ClipRect.Top) then begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Width := ClipRect.Right - ClipRect.Left;
      Bitmap.Height := ClipRect.Bottom - ClipRect.Top;
      Origin := Bitmap.ScanLine[0];
      Pitch := Integer(Bitmap.ScanLine[1]) - Integer(Origin);
      if Dithered then
        GrayscaleOpacityDithered32(TPegtopColor(TransparentColor), TPegtopColor(OpaqueColor),
          Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations)
      else
        GrayscaleOpacity32(TPegtopColor(TransparentColor), TPegtopColor(OpaqueColor),
          Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height),
          Point(Point1.X - ClipRect.Left, Point1.Y - ClipRect.Top),
          Point(Point2.X - ClipRect.Left, Point2.Y - ClipRect.Top),
          Style, Options, Iterations);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.SetName(Value: String);
begin
  // not more than 31 characters allowed
  // (due to streaming the name doesn't allow more)
  if Length(Value) > 31 then Value := Copy(Value, 1, 31); 
  if FName <> Value then begin
    FName := Value;
    Update([pgcProperty]);
  end;
end;

procedure TPegtopCustomColorGradient.SetSeamless(Value: Boolean);
begin
  if FSeamless <> Value then begin
    FSeamless := Value;
    Update([pgcProperty]);
  end;
end;

procedure TPegtopCustomColorGradient.SetColor(Value: TPegtopColorGradientColor);
begin
  FColor.Assign(Value);
end;

procedure TPegtopCustomColorGradient.SetOpacity(Value: TPegtopColorGradientOpacity);
begin
  FOpacity.Assign(Value);
end;

initialization
  InitDither;
end.
