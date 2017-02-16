////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradients.pas
// Version:    1.00 beta
// Date:       18 Feb 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradient is a color gradient defined by color and opacity keys.
// There are methods to return the gradient colors (fill a color array of any
// size) and to draw a rectangular gradient to a bitmap, supporting different
// styles and optional dithering.
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
  Windows, Classes, SysUtils, Graphics;

type
  TPegtopColorArray = array of TColor;
  TPegtopOpacityArray = array of Integer;

  TPegtopColor64 = packed record
    case Integer of
      0: (Def: Int64);
      1: (R, G, B, A: Word);
      2: (Channel: packed array[0..3] of Word);
  end;

  TPegtopColor64Array = array of TPegtopColor64;

  TPegtopColorGradientStyle = (pgsLinear, pgsRadial, pgsPolar, pgsDiamond, pgsBiasRadial);
  TPegtopColorGradientOption = (pgoReverse, pgoSymmetrical);
  TPegtopColorGradientOptions = set of TPegtopColorGradientOption;
  TPegtopColorNoiseMode = (pcnRGB, pcnHSB);

  TPegtopCustomColorGradient = class;

  TPegtopGradientKey = class(TPersistent)
  private
    FOwner: TPegtopCustomColorGradient;
    FPosition: Integer;
    FEnabled: Boolean;
    procedure SetPosition(Value: Integer);
    procedure SetEnabled(Value: Boolean);
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); virtual;
    procedure Assign(Source: TPersistent); override;
    property Owner: TPegtopCustomColorGradient read FOwner;
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
  published
    property Color: TColor read FColor write SetColor;
  end;

  TPegtopOpacityKey = class(TPegtopGradientKey)
  private
    FOpacity: Integer;
    procedure SetOpacity(Value: Integer);
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Opacity: Integer read FOpacity write SetOpacity;
  end;

  TPegtopCustomGradientOptions = class(TPersistent)
  private
    FOwner: TPegtopCustomColorGradient;
    FIterations: Integer;
    FSmoothness: Integer;
    FNoiseLevel: Integer;
    FNoiseRoughness: Integer;
    FNoiseKey: Integer;
    procedure SetIterations(Value: Integer);
    procedure SetSmoothness(Value: Integer);
    procedure SetNoiseLevel(Value: Integer);
    procedure SetNoiseRoughness(Value: Integer);
    procedure SetNoiseKey(Value: Integer);
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); virtual;
    procedure Assign(Source: TPersistent); override;
    property Owner: TPegtopCustomColorGradient read FOwner;
  published
    property Iterations: Integer read FIterations write SetIterations;
    property Smoothness: Integer read FSmoothness write SetSmoothness;
    property NoiseLevel: Integer read FNoiseLevel write SetNoiseLevel;
    property NoiseRoughness: Integer read FNoiseRoughness write SetNoiseRoughness;
    property NoiseKey: Integer read FNoiseKey write SetNoiseKey;
  end;

  TPegtopColorGradientOptions = class(TPegtopCustomGradientOptions)
  private
    FNoiseMode: TPegtopColorNoiseMode;
    FNoiseRedMin: Integer;
    FNoiseRedMax: Integer;
    FNoiseGreenMin: Integer;
    FNoiseGreenMax: Integer;
    FNoiseBlueMin: Integer;
    FNoiseBlueMax: Integer;
    FNoiseHue: Integer;
    FNoiseHueRange: Integer;
    FNoiseSaturationMin: Integer;
    FNoiseSaturationMax: Integer;
    FNoiseBrightnessMin: Integer;
    FNoiseBrightnessMax: Integer;
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); override;
    procedure Assign(Source: TPersistent); override;
  end;

  TPegtopOpacityGradientOptions = class(TPegtopCustomGradientOptions)
  private
    FNoiseMin: Integer;
    FNoiseMax: Integer;
  public
    constructor Create(AOwner: TPegtopCustomColorGradient); override;
    procedure Assign(Source: TPersistent); override;
  end;

  TPegtopCustomColorGradient = class(TPersistent)
  private
    FName: String;
    FSeamless: Boolean;
    ColorOptions: TPegtopColorGradientOptions;
    OpacityOptions: TPegtopOpacityGradientOptions;
    FColorKeys: TList;
    FOpacityKeys: TList;
    FUpdateLevel: Integer;
    FListeners: TList;
    FOnChange: TNotifyEvent;
    procedure UpdateWithoutSorting;
    procedure UpdateAndSortColorKeysOnly;
    procedure UpdateAndSortOpacityKeysOnly;
    function GetColorKey(Index: Integer): TPegtopColorKey;
    procedure SetColorKey(Index: Integer; Value: TPegtopColorKey);
    function GetOpacityKey(Index: Integer): TPegtopOpacityKey;
    procedure SetOpacityKey(Index: Integer; Value: TPegtopOpacityKey);
    function GetColorKeyCount: Integer;
    function GetOpacityKeyCount: Integer;
    procedure SetName(Value: String);
    procedure SetSeamless(Value: Boolean);
    procedure SetColorOptions(Value: TPegtopColorGradientOptions);
    procedure SetOpacityOptions(Value: TPegtopOpacityGradientOptions);
    procedure NotifyListeners;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(const AColors: array of TColor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignColors(const AColors: array of TColor);
    procedure AssignOpacities(const AOpacities: array of Integer);
    procedure Update;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddListener(Event: TNotifyEvent);
    procedure RemoveListener(Event: TNotifyEvent);
    procedure ClearColorKeys;
    procedure ClearOpacityKeys;
    function AddColorKey: TPegtopColorKey;
    function AddOpacityKey: TPegtopOpacityKey;
    procedure DeleteColorKey(Index: Integer);
    procedure DeleteOpacityKey(Index: Integer);
    function RemoveColorKey(ColorKey: TPegtopColorKey): Integer;
    function RemoveOpacityKey(OpacityKey: TPegtopOpacityKey): Integer;
    function IndexOfColorKey(ColorKey: TPegtopColorKey): Integer;
    function IndexOfOpacityKey(OpacityKey: TPegtopOpacityKey): Integer;
    procedure AssignColorKeysFrom(Gradient: TPegtopCustomColorGradient);
    procedure AssignOpacityKeysFrom(Gradient: TPegtopCustomColorGradient);
    procedure GetColors(var ColorArray: TPegtopColorArray;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
      const SwapRedBlue: Boolean = False);
    procedure GetColors64(var ColorArray: TPegtopColor64Array;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
      const SwapRedBlue: Boolean = False);
    procedure GetOpacities(var OpacityArray: TPegtopOpacityArray;
      const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1; Max: Integer = 256);
    procedure Draw32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
    procedure DrawDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
    procedure Blend32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
    procedure BlendDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
    procedure Draw(const Canvas: TCanvas; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
      const Iterations: Integer = 1);
    procedure Blend(const Canvas: TCanvas; const ClipRect: TRect;
      const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
      const Dithered: Boolean = False; const Options: TPegtopColorGradientOptions = [];
      const Iterations: Integer = 1);
    property ColorKeys[Index: Integer]: TPegtopColorKey read GetColorKey write SetColorKey;
    property ColorKeyCount: Integer read GetColorKeyCount;
    property OpacityKeys[Index: Integer]: TPegtopOpacityKey read GetOpacityKey write SetOpacityKey;
    property OpacityKeyCount: Integer read GetOpacityKeyCount;
  published
    property Name: String read FName write SetName;
    property Seamless: Boolean read FSeamless write SetSeamless;
    property ColorOptions: TPegtopColorGradientOptions read FColorOptions write SetColorOptions;
    property OpacityOptions: TPegtopOpacityGradientOptions read FOpacityOptions write SetOpacityOptions;
  end;

  TPegtopColorGradient = class(TPegtopCustomColorGradient)
  published
    property OnChange;
  end;

const
  PegtopColorGradientPositionRange = 1000;

implementation

uses
  PegtopColorUtils, PegtopChunkFiles;

type
  TPegtopNotifyListener = class
  private
    FEvent: TNotifyEvent;
  public
    constructor Create(AEvent: TNotifyEvent);
    property Event: TNotifyEvent read FEvent;
  end;

var
  DitherTable: array[0..15, 0..15] of Byte;

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

function KeySortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TPegtopGradientKey(Item1).Position - TPegtopGradientKey(Item2).Position;
end;

function PartArcTan(Y, X: Extended): Extended;
asm
  FLD Y
  FLD X
  FPATAN
  FWAIT
end;

function InterpolateCubicHermiteSpline(X1, Y1, X2, Y2, A1, A2, X: Double): Double;
var
  P, Q: Double;
begin
  P := A1 + A2 + 2 * Y1 - 2 * Y2;
  Q := Y2 - P - A1 - Y1;
  Result := ((P * X + Q) * X + A1) * X + Y1;
end;

function InterpolateCubicCardinalSpline(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Tension2, X: Double): Double;
var
  A1, A2: Double;
begin
  A1 := (1.0 - Tension1) * (Y2 - Y0);
  A2 := (1.0 - Tension2) * (Y3 - Y1);
  Result := InterpolateCubicHermiteSpline(X1, Y1, X2, Y2, A1, A2, X);
end;

function InterpolateCubicKochanekBartelsSpline(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Bias1, Continuity1, Tension2, Bias2, Continuity2, X: Double): Double;
var
  A1, A2: Double;
begin
  A1 := 0.5 * ((1.0 - Tension1) * (1.0 + Bias1) * (1.0 + Continuity1) * (Y1 - Y0)
  + (1.0 - Tension1) * (1.0 - Bias1) * (1.0 - Continuity1) * (Y2 - Y1));
  A2 := 0.5 * ((1.0 - Tension2) * (1.0 + Bias2) * (1.0 - Continuity2) * (Y2 - Y1)
  + (1.0 - Tension2) * (1.0 - Bias2) * (1.0 + Continuity2) * (Y3 - Y2));
  Result := InterpolateCubicHermiteSpline(X1, Y1, X2, Y2, A1, A2, X);
end;

function PerlinNoise(Key, Layer, X, Channel: Integer): Integer;
begin
  Result := (Key * 789221 + Layer * 1664525 + X * 1376312589 + Channel * 15731 + 1013904223) and $FFFF;
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

constructor TPegtopGradientKey.Create(AOwner: TPegtopCustomColorGradient);
begin
  FOwner := AOwner;
  FEnabled := True;
end;

procedure TPegtopGradientKey.Assign(Source: TPersistent);
begin
  if Source is TPegtopGradientKey then begin
    with TPegtopGradientKey(Source) do begin
      Self.FPosition := FPosition;
    end;
    FOwner.Update;
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopGradientKey.SetPosition(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value >= PegtopColorGradientPositionRange then
    Value := PegtopColorGradientPositionRange - 1;
  if FPosition <> Value then begin
    FPosition := Value;
    FOwner.Update;
  end;
end;

procedure TPegtopGradientKey.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    FOwner.Update;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorKey
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopColorKey.Assign(Source: TPersistent);
begin
  if Source is TPegtopColorKey then begin
    with TPegtopColorKey(Source) do begin
      Self.FColor := FColor;
    end;
  end;
  inherited;
end;

procedure TPegtopColorKey.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopOpacityKey
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopOpacityKey.Create(AOwner: TPegtopCustomColorGradient);
begin
  inherited;
  FOpacity := 256;
end;

procedure TPegtopOpacityKey.Assign(Source: TPersistent);
begin
  if Source is TPegtopOpacityKey then begin
    with TPegtopOpacityKey(Source) do begin
      Self.FOpacity := FOpacity;
    end;
  end;
  inherited;
end;

procedure TPegtopOpacityKey.SetOpacity(Value: Integer);
begin
  if FOpacity <> Value then begin
    FOpacity := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomGradientOptions
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomGradientOptions.Create(AOwner: TPegtopCustomColorGradient);
begin
  FOwner := AOwner;
  FIterations := 1;
  FSmoothness := 256;
  FNoiseLevel := 0;
  FNoiseRoughness := 128;
  FNoiseKey := 0;
end;

procedure TPegtopCustomGradientOptions.Assign(Source: TPersistent);
begin
  if Source is TPegtopCustomGradientOptions then begin
    with TPegtopCustomGradientOptions(Source) do begin
      Self.FIterations := FIterations;
      Self.FSmoothness := FSmoothness;
      Self.FNoiseLevel := FNoiseLevel;
      Self.FNoiseRoughness := FNoiseRoughness;
      Self.FNoiseKey := FNoiseKey;
    end;
    FOwner.UpdateWithoutSorting;
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopCustomGradientOptions.SetIterations(Value: Integer);
begin
  if Value < 1 then Value := 1 else if Value > 100 then Value := 100;
  if FIterations <> Value then begin
    FIterations := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

procedure TPegtopCustomGradientOptions.SetSmoothness(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FSmoothness <> Value then begin
    FSmoothness := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

procedure TPegtopCustomGradientOptions.SetNoiseLevel(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FNoiseLevel <> Value then begin
    FNoiseLevel := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

procedure TPegtopCustomGradientOptions.SetNoiseRoughness(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FNoiseRoughness <> Value then begin
    FNoiseRoughness := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

procedure TPegtopCustomGradientOptions.SetNoiseKey(Value: Integer);
begin
  Value :=Value mod 1000000;
  if FNoiseKey <> Value then begin
    FNoiseKey := Value;
    Owner.UpdateWithoutSorting;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientOptions
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientOptions.Create(AOwner: TPegtopCustomColorGradient);
begin
  inherited;
  FNoiseMode := pcmRGB;
  FNoiseRedMin := 0;
  FNoiseRedMax := 255;
  FNoiseGreenMin := 0;
  FNoiseGreenMax := 255;
  FNoiseBlueMin := 0;
  FNoiseBlueMax := 255;
  FNoiseHue := 0;
  FNoiseHueRange := 65536;
  FNoiseSaturationMin := 0;
  FNoiseSaturationMax := 65536;
  FNoiseBrightnessMin := 0;
  FNoiseBrightnessMax := 65536;
end;

procedure TPegtopColorGradientOptions.Assign(Source: TPersistent);
begin
  if Source is TPegtopColorGradientOptions then begin
    with TPegtopColorGradientOptions(Source) do begin
      Self.FOpacity := FOpacity;
    end;
  end;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopOpacityGradientOptions
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopOpacityGradientOptions.Create(AOwner: TPegtopCustomColorGradient); override;
begin
  inherited;
  FNoiseMin := 0;
  FNoiseMax := 256;
end;

procedure TPegtopOpacityGradientOptions.Assign(Source: TPersistent); override;
begin
  if Source is TPegtopOpacityGradientOptions then begin
    with TPegtopOpacityGradientOptions(Source) do begin
      Self.FNoiseMin := FNoiseMin;
      Self.FNoiseMax := FNoiseMax;
    end;
  end;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomColorGradient
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomColorGradient.Create(const AColors: array of TColor);
begin
  FColorKeys := TList.Create;
  FOpacityKeys := TList.Create;
  FListeners := TList.Create;
  FColorIterations := 1;
  FOpacityIterations := 1;
  AssignColors(AColors);
  with AddOpacityKey do Position := 0;
  with AddOpacityKey do Position := PegtopColorGradientPositionRange;
end;

destructor TPegtopCustomColorGradient.Destroy;
var
  I: Integer;
  Listener: TPegtopNotifyListener;
begin
  ClearOpacityKeys;
  FOpacityKeys.Free;
  ClearColorKeys;
  FColorKeys.Free;
  for I := 0 to FListeners.Count - 1 do begin
    Listener := FListeners[I];
    Listener.Free;
  end;
  FListeners.Free;
  inherited;
end;

procedure TPegtopCustomColorGradient.Assign(Source: TPersistent);
var
  Gradient: TPegtopCustomColorGradient;
begin
  if Source is TPegtopCustomColorGradient then begin
    BeginUpdate;
    try
      Gradient := TPegtopCustomColorGradient(Source);
      FName := Gradient.Name;
      FSeamless := Gradient.Seamless;
      AssignColorKeysFrom(Gradient);
      AssignOpacityKeysFrom(Gradient);
      FColorOptions.Assign(Gradient.ColorOptions);
      FOpacityOptions.Assign(Gradient.OpacityOptions);
    finally
      EndUpdate;
    end;
  end
  else begin
    inherited; // raise exception
  end;
end;

procedure TPegtopCustomColorGradient.AssignColors(const AColors: array of TColor);
var
  I, L: Integer;
begin
  ClearColorKeys;
  L := Length(AColors) - 1;
  if L > 0 then begin
    BeginUpdate;
    try
      for I := 0 to L do begin
        with AddColorKey do begin
          Position := I * 1000 div L;
          Color := AColors[I];
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else if L > -1 then begin
    with AddColorKey do Color := AColors[0];
  end;
end;

procedure TPegtopCustomColorGradient.AssignOpacities(const AOpacities: array of Integer);
var
  I, L: Integer;
begin
  ClearOpacityKeys;
  L := Length(AOpacities) - 1;
  if L > 0 then begin
    BeginUpdate;
    try
      for I := 0 to L do begin
        with AddOpacityKey do begin
          Position := I * 1000 div L;
          Opacity := AOpacities[I];
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else if L > -1 then begin
    with AddOpacityKey do Opacity := AOpacities[0];
  end;
end;

procedure TPegtopCustomColorGradient.UpdateWithoutSorting;
begin
  if FUpdateLevel = 0 then begin
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyListeners;
  end;
end;

procedure TPegtopCustomColorGradient.UpdateAndSortColorKeysOnly;
begin
  if FUpdateLevel = 0 then begin
    FColorKeys.Sort(KeySortCompare);
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyListeners;
  end;
end;

procedure TPegtopCustomColorGradient.UpdateAndSortOpacityKeysOnly;
begin
  if FUpdateLevel = 0 then begin
    FOpacityKeys.Sort(KeySortCompare);
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyListeners;
  end;
end;

procedure TPegtopCustomColorGradient.Update;
begin
  if FUpdateLevel = 0 then begin
    FColorKeys.Sort(KeySortCompare);
    FOpacityKeys.Sort(KeySortCompare);
    if Assigned(FOnChange) then FOnChange(Self);
    NotifyListeners;
  end;
end;

procedure TPegtopCustomColorGradient.BeginUpdate;
begin
  Inc(FUpdateLevel);
end;

procedure TPegtopCustomColorGradient.EndUpdate;
begin
  Dec(FUpdateLevel);
  Update;
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

procedure TPegtopCustomColorGradient.ClearColorKeys;
var
  I: Integer;
begin
  for I := 0 to FColorKeys.Count - 1 do begin
    TPegtopColorKey(FColorKeys[I]).Free;
  end;
  FColorKeys.Clear;
  UpdateWithoutSorting; // no keys to sort left
end;

procedure TPegtopCustomColorGradient.ClearOpacityKeys;
var
  I: Integer;
begin
  for I := 0 to FOpacityKeys.Count - 1 do begin
    TPegtopOpacityKey(FOpacityKeys[I]).Free;
  end;
  FOpacityKeys.Clear;
  UpdateWithoutSorting; // no keys to sort left
end;

function TPegtopCustomColorGradient.AddColorKey: TPegtopColorKey;
begin
  Result := TPegtopColorKey.Create(Self);
  FColorKeys.Add(Result);
  UpdateAndSortColorKeysOnly;
end;

function TPegtopCustomColorGradient.AddOpacityKey: TPegtopOpacityKey;
begin
  Result := TPegtopOpacityKey.Create(Self);
  FOpacityKeys.Add(Result);
  UpdateAndSortOpacityKeysOnly;
end;

procedure TPegtopCustomColorGradient.DeleteColorKey(Index: Integer);
begin
  FColorKeys.Delete(Index);
  UpdateWithoutSorting; // order has not changed
end;

procedure TPegtopCustomColorGradient.DeleteOpacityKey(Index: Integer);
begin
  FOpacityKeys.Delete(Index);
  UpdateWithoutSorting; // order has not changed
end;

function TPegtopCustomColorGradient.RemoveColorKey(ColorKey: TPegtopColorKey): Integer;
begin
  Result := FColorKeys.Remove(ColorKey);
  UpdateWithoutSorting; // order has not changed
end;

function TPegtopCustomColorGradient.RemoveOpacityKey(OpacityKey: TPegtopOpacityKey): Integer;
begin
  Result := FOpacityKeys.Remove(OpacityKey);
  UpdateWithoutSorting; // order has not changed
end;

function TPegtopCustomColorGradient.IndexOfColorKey(ColorKey: TPegtopColorKey): Integer;
begin
  Result := FColorKeys.IndexOf(ColorKey);
end;

function TPegtopCustomColorGradient.IndexOfOpacityKey(OpacityKey: TPegtopOpacityKey): Integer;
begin
  Result := FOpacityKeys.IndexOf(OpacityKey);
end;

procedure TPegtopCustomColorGradient.AssignColorKeysFrom(Gradient: TPegtopCustomColorGradient);
var
  I: Integer;
begin
  BeginUpdate;
  try
    ClearColorKeys;
    for I := 0 to Gradient.ColorKeyCount - 1 do begin
      with AddColorKey do Assign(Gradient.ColorKeys[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopCustomColorGradient.AssignOpacityKeysFrom(Gradient: TPegtopCustomColorGradient);
var
  I: Integer;
begin
  BeginUpdate;
  try
    ClearOpacityKeys;
    for I := 0 to Gradient.OpacityKeyCount - 1 do begin
      with AddOpacityKey do Assign(Gradient.OpacityKeys[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPegtopCustomColorGradient.GetColors(var ColorArray: TPegtopColorArray;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
  const SwapRedBlue: Boolean = False);
var
  I, J, P, Q, A: Integer;
  Size: Integer;
  CurrentIndex, NextIndex: Integer;
  CurrentPosition, NextPosition: Integer;
  CurrentColor, NextColor: TPegtopColor;
  Step, Range: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  CurrentPosition := 0; // avoid compiler warning
  NextPosition := 0; // avoid compiler warning
  CurrentIndex := 0; // avoid compiler warning
  Size := Length(ColorArray);
  if Size > 0 then begin
    if FColorKeys.Count > 1 then begin // "normal" gradient
      if pgoSymmetrical in Options then Size := (Size + 1) div 2;
      Step := PegtopColorGradientPositionRange * FColorIterations * Iterations;
      Range := Size * PegtopColorGradientPositionRange;
      Q := Range;
      for I := 0 To Size - 1 do begin
        if pgoReverse in Options then J := Size - 1 - I else J := I;
        if Q >= Range then begin
          Dec(Q, Range);
          // (re)initialize:
          if ColorKeys[0].Position > 0 then begin
            CurrentIndex := -1;
            if FSeamless then begin
              // start with last key
              CurrentPosition := ColorKeys[FColorKeys.Count - 1].Position - PegtopColorGradientPositionRange;
              CurrentColor.Def := ColorToRGB(ColorKeys[FColorKeys.Count - 1].Color);
            end
            else begin
              // start with first key (but not with first index)
              CurrentPosition := 0;
              CurrentColor.Def := ColorToRGB(ColorKeys[0].Color);
            end;
          end
          else begin
            // start with first key
            CurrentIndex := 0;
            CurrentPosition := ColorKeys[0].Position;
            CurrentColor.Def := ColorToRGB(ColorKeys[0].Color);
          end;
          NextIndex := CurrentIndex + 1;
          NextPosition := ColorKeys[NextIndex].Position;
          NextColor.Def := ColorToRGB(ColorKeys[NextIndex].Color);
        end;
        P := Q div Size;
        while (P >= NextPosition) and (CurrentIndex < FColorKeys.Count - 1) do begin
          // step to the next key (loop because multiple keys might share the same position)
          Inc(CurrentIndex);
          CurrentPosition := ColorKeys[CurrentIndex].Position;
          CurrentColor.Def := ColorToRGB(ColorKeys[CurrentIndex].Color);
          if CurrentIndex >= FColorKeys.Count - 1 then begin
            // last key reached
            if FSeamless then begin
              // continue with first key
              NextPosition := ColorKeys[0].Position + PegtopColorGradientPositionRange + 1;
              NextColor.Def := ColorToRGB(ColorKeys[0].Color);
            end
            else begin
              // continue with last key
              NextPosition := PegtopColorGradientPositionRange + 1;
              NextColor := CurrentColor;
            end;
          end
          else begin
            // continue with next key
            NextPosition := ColorKeys[CurrentIndex + 1].Position;
            NextColor.Def := ColorToRGB(ColorKeys[CurrentIndex + 1].Color);
          end;
        end;
        // interpolate color:
        A := (P - CurrentPosition) * 256 div (NextPosition - CurrentPosition);
        if SwapRedBlue then begin
          TPegtopColor(ColorArray[J]).R := CurrentColor.B + (NextColor.B - CurrentColor.B) * A div 256;
          TPegtopColor(ColorArray[J]).G := CurrentColor.G + (NextColor.G - CurrentColor.G) * A div 256;
          TPegtopColor(ColorArray[J]).B := CurrentColor.R + (NextColor.R - CurrentColor.R) * A div 256;
        end
        else begin
          TPegtopColor(ColorArray[J]).R := CurrentColor.R + (NextColor.R - CurrentColor.R) * A div 256;
          TPegtopColor(ColorArray[J]).G := CurrentColor.G + (NextColor.G - CurrentColor.G) * A div 256;
          TPegtopColor(ColorArray[J]).B := CurrentColor.B + (NextColor.B - CurrentColor.B) * A div 256;
        end;
        TPegtopColor(ColorArray[J]).A := 0;
        Inc (Q, Step);
      end;
      if pgoSymmetrical in Options then begin
        J := Length(ColorArray);
        for I := 0 to Size - 1 do begin
          Dec(J);
          ColorArray[J] := ColorArray[I];
        end;
      end;
    end

    else if FColorKeys.Count > 0 then begin // single color
      for I := 0 To Size - 1 do begin
        if SwapRedBlue then
          ColorArray[I] := SwapColorBytes(TPegtopColor(ColorToRGB(ColorKeys[0].Color))).Def
        else
          ColorArray[I] := ColorToRGB(ColorKeys[0].Color);
      end;
    end

    else begin // no colores defined
      for I := 0 To Size - 1 do ColorArray[I] := $FFFFFF;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.GetColors64(var ColorArray: TPegtopColor64Array;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1;
  const SwapRedBlue: Boolean = False);
var
  I, J, P, Q, A: Integer;
  Size: Integer;
  CurrentIndex: Integer;
  CurrentPosition, NextPosition: Integer;
  CurrentColor, NextColor: TPegtopColor;
  Step, Range: Integer;
  C: TPegtopColor64;
begin
  if Iterations < 1 then Iterations := 1;
  CurrentPosition := 0; // avoid compiler warning
  NextPosition := 0; // avoid compiler warning
  CurrentIndex := 0; // avoid compiler warning
  Size := Length(ColorArray);
  if Size > 0 then begin
    if FColorKeys.Count > 1 then begin // "normal" gradient
      if pgoSymmetrical in Options then Size := (Size + 1) div 2;
      Step := PegtopColorGradientPositionRange * FColorIterations * Iterations;
      Range := Size * PegtopColorGradientPositionRange;
      Q := Range;
      for I := 0 To Size - 1 do begin
        if pgoReverse in Options then J := Size - 1 - I else J := I;
        if Q >= Range then begin
          Dec(Q, Range);
          // (re)initialize:
          if ColorKeys[0].Position > 0 then begin
            CurrentIndex := -1;
            if FSeamless then begin
              // start with last key
              CurrentPosition := ColorKeys[FColorKeys.Count - 1].Position - PegtopColorGradientPositionRange;
              CurrentColor.Def := ColorToRGB(ColorKeys[FColorKeys.Count - 1].Color);
            end
            else begin
              // start with first key (but not with first index)
              CurrentPosition := 0;
              CurrentColor.Def := ColorToRGB(ColorKeys[0].Color);
            end;
          end
          else begin
            // start with first key
            CurrentIndex := 0;
            CurrentPosition := ColorKeys[0].Position;
            CurrentColor.Def := ColorToRGB(ColorKeys[0].Color);
          end;
          NextPosition := ColorKeys[CurrentIndex + 1].Position;
          NextColor.Def := ColorToRGB(ColorKeys[CurrentIndex + 1].Color);
        end;
        P := Q div Size;
        while (P >= NextPosition) and (CurrentIndex < FColorKeys.Count - 1) do begin
          // step to the next key (loop because multiple keys might share the same position)
          Inc(CurrentIndex);
          CurrentPosition := ColorKeys[CurrentIndex].Position;
          CurrentColor.Def := ColorToRGB(ColorKeys[CurrentIndex].Color);
          if CurrentIndex >= FColorKeys.Count - 1 then begin
            // last key reached
            if FSeamless then begin
              // continue with first key
              NextPosition := ColorKeys[0].Position + PegtopColorGradientPositionRange + 1;
              NextColor.Def := ColorToRGB(ColorKeys[0].Color);
            end
            else begin
              // continue with last key
              NextPosition := PegtopColorGradientPositionRange + 1;
              NextColor := CurrentColor;
            end;
          end
          else begin
            // continue with next key
            NextPosition := ColorKeys[CurrentIndex + 1].Position;
            NextColor.Def := ColorToRGB(ColorKeys[CurrentIndex + 1].Color);
          end;
        end;
        // interpolate color:
        A := (P - CurrentPosition) * 65536 div (NextPosition - CurrentPosition);
        if SwapRedBlue then begin
          ColorArray[J].R := CurrentColor.B * 256 + (NextColor.B - CurrentColor.B) * A div 256;
          ColorArray[J].G := CurrentColor.G * 256 + (NextColor.G - CurrentColor.G) * A div 256;
          ColorArray[J].B := CurrentColor.R * 256 + (NextColor.R - CurrentColor.R) * A div 256;
        end
        else begin
          ColorArray[J].R := CurrentColor.R * 256  + (NextColor.R - CurrentColor.R) * A div 256;
          ColorArray[J].G := CurrentColor.G * 256  + (NextColor.G - CurrentColor.G) * A div 256;
          ColorArray[J].B := CurrentColor.B * 256  + (NextColor.B - CurrentColor.B) * A div 256;
        end;
        ColorArray[J].A := 0;
        Inc (Q, Step);
      end;
      if pgoSymmetrical in Options then begin
        J := Length(ColorArray);
        for I := 0 to Size - 1 do begin
          Dec(J);
          ColorArray[J] := ColorArray[I];
        end;
      end;
    end

    else if FColorKeys.Count > 0 then begin // single color

      if SwapRedBlue then begin
        C.R := TPegtopColor(ColorKeys[0].Color).B * 256;
        C.G := TPegtopColor(ColorKeys[0].Color).G * 256;
        C.B := TPegtopColor(ColorKeys[0].Color).R * 256;
      end
      else begin
        C.R := TPegtopColor(ColorKeys[0].Color).R * 256;
        C.G := TPegtopColor(ColorKeys[0].Color).G * 256;
        C.B := TPegtopColor(ColorKeys[0].Color).B * 256;
      end;
      C.A := 0;
      for I := 0 To Size - 1 do begin
        ColorArray[I] := C;
      end;
    end

    else begin // no colores defined
      for I := 0 To Size - 1 do ColorArray[I].Def := $FFFFFFFFFFFF;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.GetOpacities(var OpacityArray: TPegtopOpacityArray;
  const Options: TPegtopColorGradientOptions = []; Iterations: Integer = 1; Max: Integer = 256);
var
  I, J, P, Q, A: Integer;
  Size: Integer;
  CurrentIndex: Integer;
  CurrentPosition, NextPosition: Integer;
  CurrentOpacity, NextOpacity: Integer;
  Step, Range: Integer;
begin
  if Iterations < 1 then Iterations := 1;
  CurrentPosition := 0; // avoid compiler warning
  NextPosition := 0; // avoid compiler warning
  CurrentIndex := 0; // avoid compiler warning
  CurrentOpacity := 0; // avoid compiler warning
  NextOpacity := 0; // avoid compiler warning
  Size := Length(OpacityArray);
  if Size > 0 then begin
    if FOpacityKeys.Count > 1 then begin // "normal" gradient
      if pgoSymmetrical in Options then Size := (Size + 1) div 2;
      Step := PegtopColorGradientPositionRange * FOpacityIterations * Iterations;
      Range := Size * PegtopColorGradientPositionRange;
      Q := Range;
      for I := 0 To Size - 1 do begin
        if pgoReverse in Options then J := Size - 1 - I else J := I;
        if Q >= Range then begin
          Dec(Q, Range);
          // (re)initialize:
          if OpacityKeys[0].Position > 0 then begin
            CurrentIndex := -1;
            if FSeamless then begin
              // start with last key
              CurrentPosition := OpacityKeys[FOpacityKeys.Count - 1].Position - PegtopColorGradientPositionRange;
              CurrentOpacity := OpacityKeys[FOpacityKeys.Count - 1].Opacity;
            end
            else begin
              // start with first key (but not with first index)
              CurrentPosition := 0;
              CurrentOpacity := OpacityKeys[0].Opacity;
            end;
          end
          else begin
            // start with first key
            CurrentIndex := 0;
            CurrentPosition := OpacityKeys[0].Position;
            CurrentOpacity := OpacityKeys[0].Opacity;
          end;
          NextPosition := OpacityKeys[CurrentIndex + 1].Position;
          NextOpacity := OpacityKeys[CurrentIndex + 1].Opacity;
        end;
        P := Q div Size;
        while (P >= NextPosition) and (CurrentIndex < FOpacityKeys.Count - 1) do begin
          // step to the next key (loop because multiple keys might share the same position)
          Inc(CurrentIndex);
          CurrentPosition := OpacityKeys[CurrentIndex].Position;
          CurrentOpacity := OpacityKeys[CurrentIndex].Opacity;
          if CurrentIndex >= FOpacityKeys.Count - 1 then begin
            // last key reached
            if FSeamless then begin
              // continue with first key
              NextPosition := OpacityKeys[0].Position + PegtopColorGradientPositionRange + 1;
              NextOpacity := OpacityKeys[0].Opacity;
            end
            else begin
              // continue with last key
              NextPosition := PegtopColorGradientPositionRange + 1;
              NextOpacity := CurrentOpacity;
            end;
          end
          else begin
            // continue with next key
            NextPosition := OpacityKeys[CurrentIndex + 1].Position;
            NextOpacity := OpacityKeys[CurrentIndex + 1].Opacity;
          end;
        end;
        // interpolate Opacity:
        A := MulDiv((P - CurrentPosition), Max, (NextPosition - CurrentPosition));
        OpacityArray[J] := (CurrentOpacity * Max + (NextOpacity - CurrentOpacity) * A) div 256;
        Inc (Q, Step);
      end;
      if pgoSymmetrical in Options then begin
        J := Length(OpacityArray);
        for I := 0 to Size - 1 do begin
          Dec(J);
          OpacityArray[J] := OpacityArray[I];
        end;
      end;
    end

    else if FOpacityKeys.Count > 0 then begin // single opacity
      for I := 0 To Size - 1 do OpacityArray[I] := OpacityKeys[0].Opacity * Max div 256;
    end

    else begin // no opacities defined
      for I := 0 To Size - 1 do OpacityArray[I] := Max;
    end;
  end;
end;

procedure TPegtopCustomColorGradient.Draw32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
const
  Size = 8192; // must be a power of 2 !!!
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  Offset: Integer;
  Colors: TPegtopColorArray;
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
  SetLength(Colors, Size);
  GetColors(Colors, Options, Iterations, True);
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(Size * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
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
              Offset := Round(Size * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
              P^.Def := Colors[Offset];
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
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
            Offset := Round(Size * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and (Size - 1);
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
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
            Offset := Round(Size * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsBiasRadial:
      begin
        Dist := Sqr(Vector.X) + Sqr(Vector.Y);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size / Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > Size - 1 then Offset := Size - 1;
            P^.Def := Colors[Offset];
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure TPegtopCustomColorGradient.DrawDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
const
  Size = 8192; // must be a power of 2 !!!
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor64;
  Offset, Dithering: Integer;
  Colors: TPegtopColor64Array;
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
  SetLength(Colors, Size);
  GetColors64(Colors, Options, Iterations, True);
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(Size * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := 0;
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
              Offset := Round(Size * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
              Dithering := DitherTable[X and $F, Y and $F];
              C := Addr(Colors[Offset]);
              if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
              if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
              if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
              P^.A := 0;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := 0;
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
            Offset := Round(Size * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and (Size - 1);
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := 0;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
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
            Offset := Round(Size * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := 0;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsBiasRadial:
      begin
        Dist := Sqr(Vector.X) + Sqr(Vector.Y);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size / Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > Size - 1 then Offset := Size - 1;
            Dithering := DitherTable[X and $F, Y and $F];
            C := Addr(Colors[Offset]);
            if (C^.R and $FF) <= Dithering then P^.R := C^.R shr 8 else P^.R := C^.R shr 8 + 1;
            if (C^.G and $FF) <= Dithering then P^.G := C^.G shr 8 else P^.G := C^.G shr 8 + 1;
            if (C^.B and $FF) <= Dithering then P^.B := C^.B shr 8 else P^.B := C^.B shr 8 + 1;
            P^.A := 0;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure TPegtopCustomColorGradient.Blend32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
const
  Size = 8192; // must be a power of 2 !!!
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor;
  O: Integer;
  Offset: Integer;
  Colors: TPegtopColorArray;
  Opacities: TPegtopOpacityArray;
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
  SetLength(Colors, Size);
  GetColors(Colors, Options, Iterations, True);
  SetLength(Opacities, Size);
  GetOpacities(Opacities, Options, Iterations);
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(Size * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
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
              Offset := Round(Size * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
              C := Addr(Colors[Offset]);
              O := Opacities[Offset];
              P^.R := P^.R + (C^.R - P^.R) * O div 256;
              P^.G := P^.G + (C^.G - P^.G) * O div 256;
              P^.B := P^.B + (C^.B - P^.B) * O div 256;
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
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
            Offset := Round(Size * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and (Size - 1);
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
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
            Offset := Round(Size * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsBiasRadial:
      begin
        Dist := Sqr(Vector.X) + Sqr(Vector.Y);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size / Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > Size - 1 then Offset := Size - 1;
            C := Addr(Colors[Offset]);
            O := Opacities[Offset];
            P^.R := P^.R + (C^.R - P^.R) * O div 256;
            P^.G := P^.G + (C^.G - P^.G) * O div 256;
            P^.B := P^.B + (C^.B - P^.B) * O div 256;
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
end;

procedure TPegtopCustomColorGradient.BlendDithered32(const Origin: Pointer; const Pitch: Integer; const ClipRect: TRect;
  const Point1, Point2: TPoint; const Style: TPegtopColorGradientStyle = pgsLinear;
  const Options: TPegtopColorGradientOptions = []; const Iterations: Integer = 1);
const
  Size = 8192; // must be a power of 2 !!!
  InverseRadian = 0.5 / Pi;
var
  X, Y: Integer;
  P, Q: PPegtopColor;
  C: ^TPegtopColor64;
  O: Integer;
  Offset, Dithering: Integer;
  Colors: TPegtopColor64Array;
  Opacities: TPegtopOpacityArray;
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
  SetLength(Colors, Size);
  GetColors64(Colors, Options, Iterations, True);
  SetLength(Opacities, Size);
  GetOpacities(Opacities, Options, Iterations, $10000);
  case Style of
    pgsLinear:
      begin
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        if Abs(Vector.X) > Abs(Vector.Y) then begin
          Ratio := -Vector.Y / Vector.X;
          for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
            P := Q;
            for X := ClipRect.Left to ClipRect.Right-1 do begin
              Offset := Round(Size * (Point1.X - X - Ratio * (Point1.Y - Y)) / (Ratio * Vector.Y - Vector.X));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
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
              Offset := Round(Size * (Point1.Y - Y - Ratio * (Point1.X - X)) / (Ratio * Vector.X - Vector.Y));
              if Offset < 0 then Offset := 0 else if Offset > Size - 1 then Offset := Size - 1;
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
              Inc (P);
            end;
            Q := Pointer(Integer(Q) + Pitch);
          end;
        end;
      end;
    pgsRadial:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size * Sqrt(Sqr(X - Point1.X) + Sqr(Y - Point1.Y)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
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
            Offset := Round(Size * (PartArcTan(Y - Point1.Y, X - Point1.X) - Ang) * InverseRadian);
            Offset := Offset and (Size - 1);
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
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsDiamond:
      begin
        Dist := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
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
            Offset := Round(Size * (Abs(CosAng * Vector.X + SinY) + Abs(SinAng * Vector.X - CosY)) / Dist);
            if Offset > Size - 1 then Offset := Size - 1;
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
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
    pgsBiasRadial:
      begin
        Dist := Sqr(Vector.X) + Sqr(Vector.Y);
        Q := Pointer(Integer(Origin) + ClipRect.Top * Pitch + ClipRect.Left * 4);
        for Y := ClipRect.Top to ClipRect.Bottom-1 do begin
          P := Q;
          for X := ClipRect.Left to ClipRect.Right-1 do begin
            Offset := Round(Size / Dist * (Sqr(X - Point1.X) + Sqr(Y - Point1.Y)));
            if Offset > Size - 1 then Offset := Size - 1;
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
            Inc (P);
          end;
          Q := Pointer(Integer(Q) + Pitch);
        end;
      end;
  end;
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
        DrawDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height), Point1, Point2, Style, Options, Iterations)
      else
        Draw32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height), Point1, Point2, Style, Options, Iterations);
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
        BlendDithered32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height), Point1, Point2, Style, Options, Iterations)
      else
        Blend32(Origin, Pitch, Rect(0, 0, Bitmap.Width, Bitmap.Height), Point1, Point2, Style, Options, Iterations);
      Canvas.Draw(ClipRect.Left, ClipRect.Top, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

function TPegtopCustomColorGradient.GetColorKey(Index: Integer): TPegtopColorKey;
begin
  if (Index >= 0) and (Index < FColorKeys.Count) then
    Result := FColorKeys[Index]
  else
    Result := NIL;
end;

procedure TPegtopCustomColorGradient.SetColorKey(Index: Integer; Value: TPegtopColorKey);
begin
  if (Index >= 0) and (Index < FColorKeys.Count) then
    TPegtopColorKey(FColorKeys[Index]).Assign(Value);
end;

function TPegtopCustomColorGradient.GetOpacityKey(Index: Integer): TPegtopOpacityKey;
begin
  if (Index >= 0) and (Index < FOpacityKeys.Count) then
    Result := FOpacityKeys[Index]
  else
    Result := NIL;
end;

procedure TPegtopCustomColorGradient.SetOpacityKey(Index: Integer; Value: TPegtopOpacityKey);
begin
  if (Index >= 0) and (Index < FOpacityKeys.Count) then
    TPegtopOpacityKey(FOpacityKeys[Index]).Assign(Value);
end;

function TPegtopCustomColorGradient.GetColorKeyCount: Integer;
begin
  Result := FColorKeys.Count;
end;

function TPegtopCustomColorGradient.GetOpacityKeyCount: Integer;
begin
  Result := FOpacityKeys.Count;
end;

procedure TPegtopCustomColorGradient.SetName(Value: String);
begin
  // not more than 31 characters allowed
  // (due to streaming the name doesn't allow more)
  if Length(Value) > 31 then Value := Copy(Value, 1, 31); 
  if FName <> Value then begin
    FName := Value;
    UpdateWithoutSorting;
  end;
end;

procedure TPegtopCustomColorGradient.SetSeamless(Value: Boolean);
begin
  if FSeamless <> Value then begin
    FSeamless := Value;
    UpdateWithoutSorting;
  end;
end;

procedure SetColorOptions(Value: TPegtopColorGradientOptions);
begin
  FColorOptions.Assign(Value);
end;

procedure SetOpacityOptions(Value: TPegtopOpacityGradientOptions);
begin
  FOpacityOptions.Assign(Value);
end;

initialization
  InitDither;
end.
