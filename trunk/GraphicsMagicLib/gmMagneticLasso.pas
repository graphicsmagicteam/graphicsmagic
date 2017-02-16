{ -- LICENSE -------------------------------------------------------------------

  This unit is written by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
  at 2011-02-21.

  Based on app\tools\gimpiscissorstool.h and gimpiscissorstool.c of GIMP 2.6.0 

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  -- COMMENTS FROM GIMP --------------------------------------------------------

  This tool is based on a paper from SIGGRAPH '95:
   "Intelligent Scissors for Image Composition", Eric N. Mortensen and
    William A. Barrett, Brigham Young University.

  thanks to Professor D. Forsyth for prompting us to implement this tool.

  ----------------------

  Personal note: Dr. Barrett, one of the authors of the paper written above
  is not only one of the most brilliant people I have ever met, he is an
  incredible professor who genuinely cares about his students and wants them
  to learn as much as they can about the topic.

  I didn't even notice I was taking a class from the person who wrote the
  paper until halfway through the semester.
                                                    -- Rockwalrus
  ----------------------

  The history of this implementation is lonog and varied.  It was
  orignally done by Spencer and Peter, and worked fine in the 0.54
  (motif only) release of GIMP.  Later revisions (0.99.something
  until about 1.1.4) completely changed the algorithm used, until it
  bore little resemblance to the one described in the paper above.
  The 0.54 version of the algorithm was then forwards ported to 1.1.4
  by Austin Donnelly.

  ----------------------

  Livewire boundary implementation done by Laramie Leavitt
}

unit gmMagneticLasso;

interface

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  Windows, Graphics, Classes, Controls, GR32;

type
  // The possible states...
  TgmMagneticLassoState = (mlsNoAction,
                           mlsSeedPlacement,
                           mlsSeedAdjustment,
                           mlsWaiting);

  // The possible drawing state...
  TgmMagneticLassoDraw = (mldDrawNothing,
                          mldDrawCurrentSeed,
                          mldDrawCurve,
                          mldDrawActiveCurve,
                          mldDrawLivewire);

  TgmMagneticLassoDrawStates = set of TgmMagneticLassoDraw;

  TgmICurve = class(TObject)
  private
    FX1, FY1: Integer;          // the first vertex of the curve
    FX2, FY2: Integer;          // the second vertex of the curve
    FLength : Integer;          // the length of the point array
    FPoints : array of TPoint;  // the points between the two vertices

    function GetPoint(I: Integer): TPoint;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const APoint: TPoint): Integer; overload;
    function Add(const X, Y: Integer): Integer; overload;

    procedure Clear;

    property Points[I: Integer]: TPoint  read GetPoint;
    property Len               : Integer read FLength;
    property X1                : Integer read FX1 write FX1;
    property Y1                : Integer read FY1 write FY1;
    property X2                : Integer read FX2 write FX2;
    property Y2                : Integer read FY2 write FY2;
  end;

  TgmICurveList = class(TList)
  public
    destructor Destroy; override;
    procedure DeleteAllCurves;
  end;

  TgmMagneticLasso = class(TObject)
  private
    FOffsetX          : Integer;                     // drawing offset vector
    FOffsetY          : Integer;
    FScale            : Single;                      // drawing scale
    
    FX, FY            : Integer;                     // upper left hand coordinate
    FInitX, FInitY    : Integer;                     // initial coordinates
    FNewX, FNewY      : Integer;                     // new coordinates

    FLivewire         : TgmICurve;                   // livewire boundary curve
    FCurve1           : TgmICurve;                   // 1st curve connected to current point
    FCurve2           : TgmICurve;                   // 2nd curve connected to current point
    FCurveList        : TgmICurveList;               // the list of curves

    FLeftButtonPressed: Boolean;
    FFirstGradient    : Boolean;
    FFirstPoint       : Boolean;                     // is this the first point?
    FConnected        : Boolean;                     // is the region closed?
    FInteractive      : Boolean;

    FToolState        : TgmMagneticLassoState;       // state of magnetic lasso
    FDrawStates       : TgmMagneticLassoDrawStates;  // items to draw on a draw request

    FDPBuf            : TBitmap32;                   // used as 4-bytes dynamic programming buffer
    FSourceBitmap     : TBitmap32;                   
    FGradientMap      : TBitmap;                     // lazily filled gradient map
    FCanvas           : TCanvas;                     // pointer to a canvas for drawing curves on

    FCurveRegion      : hRGN;

    procedure Init;
    procedure CreateGradientMap;
    procedure InitGradientMap;
    procedure FindMaxGradient(var AX, AY: Integer);
    procedure CalculateCurve(const ACurve: TgmICurve);
    procedure DrawCurve(const ACurve: TgmICurve);
    procedure FindOptimalPath(const X1, Y1, X2, Y2, XS, YS: Integer);
    procedure ConvertCurvesToRegion;
    procedure Draw;

    procedure PlotPixels(const ACurve: TgmICurve;
      const X1, Y1, XS, YS, XE, YE: Integer);

    function CalculateLink(const AX, AY: Integer; APixel: Cardinal;
      const ALink: Integer): Integer;

    function GradientMapValue(const AX, AY: Integer; var AGrad, ADir: Byte): Boolean;
    function GetOffset(const APixel: Cardinal): Integer;
    function ClickedOnVertex(const AX, AY: Integer): Boolean;
    function ClickedOnCurve(const AX, AY: Integer): Boolean;
    function MouseOverVertex(const AX, AY: Integer): Integer;
    function MouseOverCurve(const AX, AY: Integer): Integer;
  public
    constructor Create(const ASourceBmp: TBitmap32; const ACanvas: TCanvas);
    destructor Destroy; override;

    procedure Reset;
    procedure UpdateDisplay;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer);

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer);

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer);

    property OffsetX      : Integer read FOffsetX     write FOffsetX;
    property OffsetY      : Integer read FOffsetY     write FOffsetY;
    property Scale        : Single  read FScale       write FScale;
    property IsInteractive: Boolean read FInteractive write FInteractive;
    property IsConnected  : Boolean read FConnected;
    property CurveRegion  : hRGN    read FCurveRegion;
  end;

implementation

uses
{ Standard }
  SysUtils, Types, Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmGTypes,
  gmGimpPaintFuncs,
  gmGimpBaseEnums,
  gmGimpDrawTool,
  gmGtkEnums;

const
  MAX_GRADIENT    = 179.606; // = Sqrt (127^2 + 127^2)
  GRADIENT_SEARCH = 32;      // how far to look when snapping to an edge
  TARGET_SIZE     = 25;
  POINT_WIDTH     = 12;      // size (in pixels) of seed handles
  EXTEND_BY       = 0.2;     // proportion to expand cost map by
  FIXED           = 5;       // additional fixed size to expand cost map
  MIN_GRADIENT    = 63;      // gradients < this are directionless

  COST_WIDTH      = 2;       // number of bytes for each pixel in cost map

  // weight to give between gradient (_G) and direction (_D)
  OMEGA_D         = 0.2;
  OMEGA_G         = 0.8;

  // sentinel to mark seed point in ?cost? map
  SEED_POINT      = 9;

  // where to move on a given link direction
  MOVE_DIRECTION: array [0..7, 0..1] of Integer =
  ( (  1,  0 ),  // move to right
    (  0,  1 ),  // move to bottom
    ( -1,  1 ),  // move to bottom-left
    (  1,  1 ),  // move to bottom-right
    ( -1,  0 ),  // move to left
    (  0, -1 ),  // move to top
    (  1, -1 ),  // move to top-right
    ( -1, -1 )   // move to top-left
  );

  { IE:
    '---+---+---`
    | 7 | 5 | 6 |
    +---+---+---+
    | 4 |   | 0 |
    +---+---+---+
    | 2 | 1 | 3 |
    `---+---+---'
  }

  HORZ_DERIV: array [0..8] of Single =
  (
     1,  0, -1,
     2,  0, -2,
     1,  0, -1
  );

  VERT_DERIV: array [0..8] of Single =
  (
     1,  2,  1,
     0,  0,  0,
    -1, -2, -1
  );

  BLUR_32: array [0..8] of Single =
  (
     1,  1,  1,
     1, 24,  1,
     1,  1,  1
  );

var
  DistanceWeights: array [0..(GRADIENT_SEARCH * GRADIENT_SEARCH - 1)] of Single;
  DiagonalWeight : array [0..255] of Integer;
  DirectionValue : array [0..255, 0..3] of Integer;

//-- Functional defines --------------------------------------------------------

function PixelCost(const X: Cardinal): Cardinal;
begin
  Result := X shr 8;
end;

function PixelDir(const X: Cardinal): Cardinal;
begin
  Result := X and $000000FF;
end;

function Pack(const X, Y: Integer): Cardinal;
begin
  Result := ( (Y and $FF) shl 8 ) or (X and $FF);  
end; 

//-- TgmICurve -----------------------------------------------------------------

constructor TgmICurve.Create;
begin
  inherited Create;

  FX1     := 0;
  FY1     := 0;
  FX2     := 0;
  FY2     := 0;
  FLength := 0;
  FPoints := nil;
end;

destructor TgmICurve.Destroy;
begin
  if Length(FPoints) > 0 then
  begin
    SetLength(FPoints, 0);
    FPoints := nil;
  end;

  inherited Destroy;
end;

function TgmICurve.GetPoint(I: Integer): TPoint;
begin
  Result := GR32.Point(0, 0);
  
  if FLength > 0 then
  begin
    if (I >= 0) and (I < FLength) then
    begin
      Result := FPoints[I];
    end;
  end;
end;

function TgmICurve.Add(const APoint: TPoint): Integer;
var
  LIndex: Integer;
begin
  Inc(FLength);
  SetLength(FPoints, FLength);

  LIndex          := FLength - 1;
  FPoints[LIndex] := APoint;
  Result          := LIndex;
end;

function TgmICurve.Add(const X, Y: Integer): Integer;
var
  LIndex: Integer;
begin
  Inc(FLength);
  SetLength(FPoints, FLength);

  LIndex          := FLength - 1;
  FPoints[LIndex] := GR32.Point(X, Y);
  Result          := LIndex;
end;

procedure TgmICurve.Clear;
begin
  if FLength > 0 then
  begin
    SetLength(FPoints, 0);
    FLength := 0;
  end;
end;

//-- TgmICurveList -------------------------------------------------------------

destructor TgmICurveList.Destroy;
begin
  DeleteAllCurves;
  inherited Destroy;
end;

procedure TgmICurveList.DeleteAllCurves;
var
  i     : Integer;
  LCurve: TgmICurve;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LCurve := TgmICurve(Self.Items[i]);

      Self.Delete(i);
      LCurve.Free;
    end;
  end;
end;

//-- TgmMagneticLasso ----------------------------------------------------------

constructor TgmMagneticLasso.Create(const ASourceBmp: TBitmap32;
  const ACanvas: TCanvas);
begin
  inherited Create;

  FSourceBitmap := nil;

  if Assigned(ASourceBmp) then
  begin
    FSourceBitmap := TBitmap32.Create;
    FSourceBitmap.Assign(ASourceBmp);
  end;

  FCanvas := ACanvas;

  Init;

  { Initialise the gradient map tile manager for this image if we
    don't already have one. }
  if not Assigned(FGradientMap) then
  begin
    CreateGradientMap;
  end;
end;

destructor TgmMagneticLasso.Destroy;
begin
  FCanvas := nil;

  if Assigned(FSourceBitmap) then
  begin
    FSourceBitmap.Free;
  end;

  if Assigned(FDPBuf) then
  begin
    FDPBuf.Free;
  end;

  if Assigned(FGradientMap) then
  begin
    FGradientMap.Free;
  end;

  DeleteObject(FCurveRegion);

  inherited Destroy;
end;

procedure TgmMagneticLasso.Init;
begin
  FCurveList         := TgmICurveList.Create;
  FDPBuf             := nil;
  FGradientMap       := nil;
  FLivewire          := nil;
  FLeftButtonPressed := False;
  FInteractive       := False;
  FFirstGradient     := True;
  FOffsetX           := 0;
  FOffsetY           := 0;
  FScale             := 1.0;

  Reset;
end;

procedure TgmMagneticLasso.Reset;
begin
  FCurve1     := nil;
  FCurve2     := nil;
  FFirstPoint := True;
  FConnected  := False;
  FDrawStates := [mldDrawNothing];
  FToolState  := mlsNoAction;

  // free and reset the curve list
  FCurveList.DeleteAllCurves;

  // reset the dp buffers
  if Assigned(FDPBuf) then
  begin
    FreeAndNil(FDPBuf);
  end;
end;

procedure TgmMagneticLasso.CreateGradientMap;
begin
  if Assigned(FSourceBitmap) then
  begin
    if Assigned(FGradientMap) then
    begin
      FreeAndNil(FGradientMap);
    end;

    FGradientMap             := TBitmap.Create;
    FGradientMap.Width       := FSourceBitmap.Width;
    FGradientMap.Height      := FSourceBitmap.Height;
    FGradientMap.PixelFormat := pf16bit;

    InitGradientMap;
  end;
end;

// called to fill in a newly created gradient map
procedure TgmMagneticLasso.InitGradientMap;
var
  x, y          : Integer;
  dw, dh        : Integer;
  sw, sh        : Integer;
  i, j          : Integer;
  b             : Integer;
  LBytes        : Integer;
  LRadius       : Integer;
  LGradient     : Single;
  LDirection    : Single;
  LGradMap      : PByteArray;
  LDataH        : PColor32Array;
  LDataV        : PColor32Array;
  LHMax, LVMax  : ShortInt;
  ha, hr, hg, hb: Byte;
  va, vr, vg, vb: Byte;
  LBlurredBmp   : TBitmap32;
  LHorzDerivBmp : TBitmap32;
  LVertDerivBmp : TBitmap32;
begin
{$RANGECHECKS OFF}

  if not Assigned(FSourceBitmap) then
  begin
    Exit;
  end;

  if not Assigned(FGradientMap) then
  begin
    Exit;
  end;

  if (FGradientMap.Width  <> FSourceBitmap.Width) or
     (FGradientMap.Height <> FSourceBitmap.Height) then
  begin
    Exit;
  end;

  if FFirstGradient then
  begin
    LRadius := GRADIENT_SEARCH shr 1;

    // compute the distance weights
    for i := 0 to (GRADIENT_SEARCH - 1) do
    begin
      for j := 0 to (GRADIENT_SEARCH - 1) do
      begin
        DistanceWeights[i * GRADIENT_SEARCH + j] :=
          1.0 / (1 + Sqrt((i - LRadius) * (i - LRadius) + (j - LRadius) * (j - LRadius)));
      end;
    end;

    FFirstGradient := False;
  end;

  LBlurredBmp   := TBitmap32.Create;
  LHorzDerivBmp := TBitmap32.Create;
  LVertDerivBmp := TBitmap32.Create;
  try
    // blur the source to get rid of noise
    LBlurredBmp.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);
    ConvolveRegion(FSourceBitmap, LBlurredBmp, BLUR_32, 3, 32, gctNormalConvolve, False);

    // get the horizontal derivative
    LHorzDerivBmp.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);
    ConvolveRegion(LBlurredBmp, LHorzDerivBmp, HORZ_DERIV, 3, 1, gctNegativeConvolve, False);

    // get the vertical derivative
    LVertDerivBmp.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);
    ConvolveRegion(LBlurredBmp, LVertDerivBmp, VERT_DERIV, 3, 1, gctNegativeConvolve, False);

    // calculate overall gradient

    for i := 0 to (LBlurredBmp.Height - 1) do
    begin
      LDataH   := LHorzDerivBmp.ScanLine[i];
      LDataV   := LVertDerivBmp.ScanLine[i];
      LGradMap := FGradientMap.ScanLine[i];

      for j := 0 to (LBlurredBmp.Width - 1) do
      begin
        ha := LDataH[j] shr 24 and $FF;
        hr := LDataH[j] shr 16 and $FF;
        hg := LDataH[j] shr  8 and $FF;
        hb := LDataH[j]        and $FF;

        va := LDataV[j] shr 24 and $FF;
        vr := LDataV[j] shr 16 and $FF;
        vg := LDataV[j] shr  8 and $FF;
        vb := LDataV[j]        and $FF;

        LHMax := hb - 128;

        if Abs(hg - 128) > Abs(LHMax) then
        begin
          LHMax := hg - 128;
        end;

        if Abs(hr - 128) > Abs(LHMax) then
        begin
          LHMax := hr - 128;
        end;

        if Abs(ha - 128) > Abs(LHMax) then
        begin
          LHMax := ha - 128;
        end;

        LVMax := vb - 128;

        if Abs(vg - 128) > Abs(LVMax) then
        begin
          LVMax := vg - 128;
        end;

        if Abs(vr - 128) > Abs(LVMax) then
        begin
          LVMax := vr - 128;
        end;

        if Abs(va - 128) > Abs(LVMax) then
        begin
          LVMax := va - 128;
        end;

        if (i = 0) or
           (j = 0) or
           (i = (LBlurredBmp.Height - 1)) or
           (j = (LBlurredBmp.Width - 1)) then
        begin
          LGradMap[j * COST_WIDTH + 0] := 0;
          LGradMap[j * COST_WIDTH + 1] := 255;
          Continue;
        end;

        // 1 byte absolute magnitude first
        LGradient := Sqrt(LHMax * LHMax + LVMax * LVMax);
        LGradMap[j * COST_WIDTH] := Trunc(LGradient * 255 / MAX_GRADIENT);

        // then 1 byte direction
        if LGradient > MIN_GRADIENT then
        begin
          if LHMax = 0 then
          begin
            if LVMax > 0 then
            begin
              LDirection := G_PI_2;
            end
            else
            begin
              LDirection := -G_PI_2;
            end;
          end
          else
          begin
            LDirection := ArcTan(LVMax / LHMax);
          end;

          { Scale the direction from between 0 and 254,
            corresponding to -PI/2, PI/2 255 is reserved for
            directionless pixels }

          LGradMap[j * COST_WIDTH + 1] := Trunc(254 * (LDirection + G_PI_2) / G_PI);
        end
        else
        begin
          LGradMap[j * COST_WIDTH + 1] := 255; // reserved for weak gradient
        end;
      end;
    end;

  finally
    LBlurredBmp.Free;
    LHorzDerivBmp.Free;
    LVertDerivBmp.Free;
  end;

{$RANGECHECKS ON}
end;

procedure TgmMagneticLasso.FindMaxGradient(var AX, AY: Integer);
var
  LRadius       : Integer;
  i, j          : Integer;
  cx, cy        : Integer;
  x1, y1, x2, y2: Integer;
  LMaxGradient  : Single;
  g             : Single;
  LGradient     : PByteArray;
begin
  { Initialise the gradient map tile manager for this image if we
    don't already have one. }
  if not Assigned(FGradientMap) then
  begin
    CreateGradientMap;
  end;

  LRadius := GRADIENT_SEARCH shr 1;

  // calculate the extent of the search
  cx := Clamp(AX, 0, FSourceBitmap.Width);
  cy := Clamp(AY, 0, FSourceBitmap.Height);
  x1 := Clamp(cx - LRadius, 0, FSourceBitmap.Width);
  y1 := Clamp(cy - LRadius, 0, FSourceBitmap.Height);
  x2 := Clamp(cx + LRadius, 0, FSourceBitmap.Width);
  y2 := Clamp(cy + LRadius, 0, FSourceBitmap.Height);

  // calculate the factor to multiply the distance from the cursor by
  LMaxGradient := 0;
  AX           := cx;
  AY           := cy;

  // find the point of max gradient
  for i := y1 to (y2 - 1) do
  begin
    LGradient := FGradientMap.ScanLine[i];

    for j := x1 to (x2 - 1) do
    begin
      g := LGradient[j * COST_WIDTH];
      g := g * DistanceWeights[(i - y1) * GRADIENT_SEARCH + (j - x1)];

      if g > LMaxGradient then
      begin
        LMaxGradient := g;
        AX           := j;
        AY           := i;
      end;
    end;
  end;
end;

procedure TgmMagneticLasso.CalculateCurve(const ACurve: TgmICurve);
var
  x, y, LDir       : Integer;
  xs, ys, xe, ye   : Integer;
  x1, y1, x2, y2   : Integer;
  LWidth, LHeight  : Integer;
  LEWidth, LEHeight: Integer;
begin
  if not Assigned(ACurve) then
  begin
    Exit;
  end;

  { Calculate the lowest cost path from one vertex to the next as specified
    by the parameter "curve".
      Here are the steps:
        1)  Calculate the appropriate working area for this operation
        2)  Allocate a temp buf for the dynamic programming array
        3)  Run the dynamic programming algorithm to find the optimal path
        4)  Translate the optimal path into pixels in the icurve data
              structure.
  }

  // get the bounding box
  xs := Clamp(ACurve.X1, 0, FSourceBitmap.Width  - 1);
  ys := Clamp(ACurve.Y1, 0, FSourceBitmap.Height - 1);
  xe := Clamp(ACurve.X2, 0, FSourceBitmap.Width  - 1);
  ye := Clamp(ACurve.Y2, 0, FSourceBitmap.Height - 1);

  x1 := Min(xs, xe);
  y1 := Min(ys, ye);
  x2 := max(xs, xe) + 1;  // +1 because if xe = 199 & xs = 0, x2 - x1, width = 200
  y2 := max(ys, ye) + 1;

  { expand the boundaries past the ending points by
    some percentage of width and height.  This serves the following purpose:
    It gives the algorithm more area to search so better solutions
    are found.  This is particularly helpful in finding "bumps" which
    fall outside the bounding box represented by the start and end
    coordinates of the "curve".
  }

  LEWidth  := Trunc( (x2 - x1) * EXTEND_BY + FIXED );
  LEHeight := Trunc( (y2 - y1) * EXTEND_BY + FIXED );

  if xe >= xs then
  begin
    x2 := x2 + Clamp(LEWidth, 0, FSourceBitmap.Width - x2);
  end
  else
  begin
    x1 := x1 - Clamp(LEWidth, 0, x1);
  end;

  if ye >= ys then
  begin
    y2 := y2 + Clamp(LEHeight, 0, FSourceBitmap.Height - y2);
  end
  else
  begin
    y1 := y1 - Clamp(LEHeight, 0, y1);
  end;

  // blow away any previous points list we might have
  if ACurve.Len > 0 then
  begin
    ACurve.Clear;
  end;

  // if the bounding box has width and height...
  if ( (x2 - x1) > 0 ) and ( (y2 - y1) > 0 ) then
  begin
    LWidth  := x2 - x1;
    LHeight := y2 - y1;

    { Initialize the gradient map tile manager for this image if we
      don't already have one. }
    if not Assigned(FGradientMap) then
    begin
      CreateGradientMap;
    end;

    // allocate the dynamic programming array
    if Assigned(FDPBuf) then
    begin
      FreeAndNil(FDPBuf);
    end;

    FDPBuf := TBitmap32.Create;
    FDPBuf.SetSize(LWidth, LHeight);

    // the following four lines are needed, added by ourselves
    xs := Clamp(xs, x1, x2 - 1);
    ys := Clamp(ys, y1, y2 - 1);
    xe := Clamp(xe, x1, x2 - 1);
    ye := Clamp(ye, y1, y2 - 1);

    // find the optimal path of pixels from (x1, y1) to (x2, y2)
    FindOptimalPath(x1, y1, x2, y2, xs, ys);

    // get a list of the pixels in the optimal path
    PlotPixels(ACurve, x1, y1, xs, ys, xe, ye);
  end
  else if ((x2 - x1) = 0) then  // if the bounding box has no width
  begin
    // plot a vertical line
    y := ys;

    if ys > ye then
    begin
      LDir := -1;
    end
    else
    begin
      LDir := 1;
    end;

    while y <> ye do
    begin
      ACurve.Add(xs, y);

      y := y + LDir;
    end;
  end
  else if ( (y2 - y1) = 0) then  // if the bounding box has no height 
  begin
    // plot a horizontal line
    x := xs;

    if xs > xe then
    begin
      LDir := -1;
    end
    else
    begin
      LDir := 1;
    end;

    while x <> xe do
    begin
      ACurve.Add(x, ys);

      x := x + LDir;
    end;
  end;
end;

procedure TgmMagneticLasso.DrawCurve(const ACurve: TgmICurve);
begin
  if Assigned(ACurve) then
  begin
    DrawLines(FCanvas, ACurve.FPoints, False, FOffsetX, FOffsetY, FScale);
  end;
end;

procedure TgmMagneticLasso.FindOptimalPath(const X1, Y1, X2, Y2, XS, YS: Integer);
var
  i, j, k, x, y: Integer;
  LLink        : Integer;
  LLinkDir     : Integer;
  LDirX, LDirY : Integer;
  LMinCost     : Integer;
  LNewCost     : Integer;
  LOffset      : Integer;
  LCumCost     : array [0..7] of Integer;
  LLinkCost    : array [0..7] of Integer;
  LPixelCost   : array [0..7] of Integer;
  LPixel       : array [0..7] of Cardinal;
  LData, d     : PColor32;
begin
{$RANGECHECKS OFF}

  if not Assigned(FDPBuf) then
  begin
    Exit;
  end;

  // initialize the dynamic programming buffer
  FDPBuf.Clear($00000000);

  // what directions are we filling the array in according to?
  if (xs - x1) = 0 then
  begin
    LDirX := 1;
  end
  else
  begin
    LDirX := -1;
  end;

  if (ys - y1) = 0 then
  begin
    LDirY := 1;
  end
  else
  begin
    LDirY := -1;
  end;

  LLinkDir := LDirX * LDirY;

  y := ys;

  for i := 0 to FDPBuf.Height - 1 do
  begin
    x := xs;

    LData := @FDPBuf.Bits[(y - y1) * FDPBuf.Width + (x - x1)];

    for j := 0 to FDPBuf.Width - 1 do
    begin
      LMinCost := MAXINT;

      { pixel[] array encodes how to get to a neigbour, if possible.
        0 means no connection (eg edge).
        Rest packed as bottom two bytes: y offset then x offset.
        Initially, we assume we can't get anywhere. }
      for k := 0 to 7 do
      begin
        LPixel[k] := 0;
      end;

      // find the valid neighboring pixels
      // the previous pixel
      if j > 0 then
      begin
        if LDirX = 1 then
        begin
          LPixel[4] := Pack(-LDirX, 0);
        end
        else
        begin
          LPixel[0] := Pack(-LDirX, 0);
        end;
      end;

      // the previous row of pixels
      if i > 0 then
      begin
        if LDirY = 1 then
        begin
          LPixel[5] := Pack(0, -LDirY);
        end
        else
        begin
          LPixel[1] := Pack(0, -LDirY);
        end;

        if LLinkDir = 1 then
        begin
          LLink := 3;
        end
        else
        begin
          LLink := 2;
        end;

        if j > 0 then
        begin
          if LDirY = 1 then
          begin
            LPixel[LLink + 4] := Pack(-LDirX, -LDirY);
          end
          else
          begin
            LPixel[LLink] := Pack(-LDirX, -LDirY);
          end;
        end;

        if LLinkDir = 1 then
        begin
          LLink := 2;
        end
        else
        begin
          LLink := 3;
        end;

        if j <> (FDPBuf.Width - 1) then
        begin
          if LDirY = 1 then
          begin
            LPixel[LLink + 4] := Pack(LDirX, -LDirY);
          end
          else
          begin
            LPixel[LLink] := Pack(LDirX, -LDirY);
          end;
        end;
      end;

      // find the minimum cost of going through each neighbor to reach the
      // seed point...

      LLink := -1;

      for k := 0 to 7 do
      begin
        if LPixel[k] <> 0 then
        begin
          if k > 3 then
          begin
            LLinkCost[k] := CalculateLink(xs + j * LDirX,
                                          ys + i * LDirY,
                                          LPixel[k], k - 4);
          end
          else
          begin
            LLinkCost[k] := CalculateLink(xs + j * LDirX,
                                          ys + i * LDirY,
                                          LPixel[k], k);
          end;

          LOffset := GetOffset(LPixel[k]);

          d := LData;
          Inc(d, LOffset);

          LPixelCost[k] := PixelCost(d^);
          LCumCost[k]   := LPixelCost[k] + LLinkCost[k];

          if LCumCost[k] < LMinCost then
          begin
            LMinCost := LCumCost[k];
            LLink    := k;
          end;
        end;
      end;

      // If anything can be done...
      if LLink >= 0 then
      begin
        // set the cumulative cost of this pixel and the new direction
        LData^ := (LCumCost[LLink] shl 8) + LLink;

        { possibly change the links from the other pixels to this pixel...
          these changes occur if a neighboring pixel will receive a lower
          cumulative cost by going through this pixel.
         }
         for k := 0 to 7 do
         begin
           if (LPixel[k] <> 0) and (k <> LLink) then
           begin
             { if the cumulative cost at the neighbor is greater than
               the cost through the link to the current pixel, change the
               neighbor's link to point to the current pixel.
             }
             LNewCost := LLinkCost[k] + LCumCost[LLink];

             if LPixelCost[k] > LNewCost then
             begin
               // reverse the link direction
               LOffset := GetOffset(LPixel[k]);

               d := LData;
               Inc(d, LOffset);

               if k > 3 then
               begin
                 d^ := (LNewCost shl 8) + (k - 4);
               end
               else
               begin
                 d^ := (LNewCost shl 8) + (k + 4);
               end;
             end;
           end;
         end;
      end
      else
      if ( (i = 0) and (j = 0) ) then
      begin
        // set the seed point
        LData^ := SEED_POINT;
      end;

      // increment the data pointer and the x counter
      Inc(LData, LDirX);
    end;

    // increment the y counter
    y := y + LDirY;
  end;

{$RANGECHECKS ON}
end;

procedure TgmMagneticLasso.ConvertCurvesToRegion;
var
  LCurve      : TgmICurve;
  LPoints     : array [0..65535] of TPoint;
  i, j        : Integer;
  LTotalPoints: Integer;
begin
  if FCurveList.Count > 0 then
  begin
    LTotalPoints := 0;

    // go over the curves in reverse order, adding the points we have
    for i := (FCurveList.Count - 1) downto 0 do
    begin
      LCurve := TgmICurve(FCurveList.Items[i]);

      for j := 0 to (LCurve.Len - 1) do
      begin
        LPoints[LTotalPoints + j] := LCurve.Points[j];
      end;

      Inc(LTotalPoints, LCurve.Len);
    end;

    DeleteObject(FCurveRegion);
    FCurveRegion := CreatePolygonRgn(LPoints, LTotalPoints, ALTERNATE);
  end;
end;

procedure TgmMagneticLasso.PlotPixels(const ACurve: TgmICurve;
  const X1, Y1, XS, YS, XE, YE: Integer);
var
  x, y  : Integer;
  LLink : Integer;
  LWidth: Integer;
  LData : PColor32;
begin
{$RANGECHECKS OFF}

  if not Assigned(FDPBuf) then
  begin
    Exit;
  end;

  if not Assigned(ACurve) then
  begin
    Exit;
  end;

  LWidth := FDPBuf.Width;

  // start the data pointer at the correct location
  LData := @FDPBuf.Bits[(YE - Y1) * LWidth + (XE - X1)];

  x := XE;
  y := YE;

  while (True) do
  begin
    ACurve.Add(x, y);

    LLink := PixelDir(LData^);

    if LLink = SEED_POINT then
    begin
      Break;
    end;

    x := x + MOVE_DIRECTION[LLink, 0];
    y := y + MOVE_DIRECTION[LLink, 1];

    Inc( LData, (MOVE_DIRECTION[LLink, 1] * LWidth + MOVE_DIRECTION[LLink, 0]) );
  end;

{$RANGECHECKS ON}
end;

function TgmMagneticLasso.CalculateLink(const AX, AY: Integer; APixel: Cardinal;
  const ALink: Integer): Integer;
var
  LValue        : Integer;
  LX, LY        : Integer;
  LGrad1, LGrad2: Byte;
  LDir1,  LDir2 : Byte;
begin
  LValue := 0;

  if not GradientMapValue(AX, AY, LGrad1, LDir1) then
  begin
    LGrad1 := 0;
    LDir1  := 255;
  end;

  { Convert the gradient into a cost: large gradients are good, and
    so have low cost. }
  LGrad1 := 255 - LGrad1;

  // calculate the contribution of the gradient magnitude
  if ALink > 1 then
  begin
    LValue := Trunc(LValue + DiagonalWeight[LGrad1] * OMEGA_G); 
  end
  else
  begin
    LValue := Trunc(LValue + LGrad1 * OMEGA_G);
  end;

  // calculate the contribution of the gradient direction
  LX := AX + ShortInt(APixel and $FF);
  LY := AY + ShortInt((APixel and $FF00) shr 8);

  if not GradientMapValue(LX, LY, LGrad2, LDir2) then
  begin
    LGrad2 := 0;
    LDir2  := 255;
  end;

  LValue := Trunc( LValue + (DirectionValue[LDir1, ALink] + DirectionValue[LDir2, ALink]) * OMEGA_D );

  Result := LValue;
end;

function TgmMagneticLasso.GradientMapValue(const AX, AY: Integer;
  var AGrad, ADir: Byte): Boolean;
var
  p    : PByteArray;
  LBits: Integer;
begin
  Result := False;
  
  if not Assigned(FGradientMap) then
  begin
    Exit;
  end;

  if ( AX <> Clamp(AX, 0, FGradientMap.Width  - 1) ) or
     ( AY <> Clamp(AY, 0, FGradientMap.Height - 1) ) then
  begin
    Exit;
  end;

  p := FGradientMap.ScanLine[AY];

  LBits := AX * COST_WIDTH;

  AGrad := P[LBits + 0];
  ADir  := P[LBits + 1];

  Result := True;
end;

function TgmMagneticLasso.GetOffset(const APixel: Cardinal): Integer;
var
  x, y: ShortInt;
begin
  x := ShortInt(APixel and $FF);
  y := ShortInt( (APixel and $FF00) shr 8 );

  Result := x + y * FDPBuf.Width;
end;

function TgmMagneticLasso.ClickedOnVertex(const AX, AY: Integer): Boolean;
var
  LCurvesFound: Integer;
begin
  LCurvesFound := MouseOverVertex(AX, AY);

  if LCurvesFound > 1 then
  begin
    // undraw the curve
    FDrawStates := [mldDrawCurve];
    Draw;

    Result := True;
    Exit;
  end;

  { if only one curve was found, the curves are unconnected, and
    the user only wants to move either the first or last point
    disallow this for now.
   }

  if LCurvesFound = 1 then
  begin
    Result := False;
    Exit;
  end;

  Result := ClickedOnCurve(AX, AY);
end;

function TgmMagneticLasso.ClickedOnCurve(const AX, AY: Integer): Boolean;
var
  LIndex   : Integer;
  LCurve   : TgmICurve;
  LNewCurve: TgmICurve;
begin
  { traverse through the list, getting back the curve segment's list
    element if the current cursor position is on a curve...
    If this occurs, replace the curve with two new curves,
    separated by a new vertex.
   }

  Result := False;
  LIndex := MouseOverCurve(AX, AY);

  if LIndex >= 0 then
  begin
    // undraw the curve
    FDrawStates := [mldDrawCurve];
    Draw;

    LCurve := TgmICurve(FCurveList.Items[LIndex]);

    // create the new curve
    LNewCurve := TgmICurve.Create;

    LNewCurve.X2 := LCurve.X2;
    LNewCurve.Y2 := LCurve.Y2;

    LNewCurve.X1 := FX;
    LNewCurve.Y1 := FX;

    LCurve.X2 := FX;
    LCurve.Y2 := FY;

    // insert the new curve to list
    FCurveList.Insert(LIndex + 1, LNewCurve);

    FCurve1 := LNewCurve;
    FCurve2 := LCurve;

    Result := True;
  end; 
end;

// XXX need some scan-conversion routines from somewhere.  maybe. ?
function TgmMagneticLasso.MouseOverVertex(const AX, AY: Integer): Integer;
var
  i           : Integer;
  LCurvesFound: Integer;
  LCurve      : TgmICurve;
begin
  { traverse through the list, returning non-zero if the current cursor
    position is on an existing curve vertex.  Set the curve1 and curve2
    variables to the two curves containing the vertex in question
   }

  LCurvesFound := 0;
  FCurve1      := nil;
  FCurve2      := nil;

  if FCurveList.Count > 0 then
  begin
    for i := 0 to (FCurveList.Count - 1) do
    begin
      LCurve := TgmICurve(FCurveList.Items[i]);

      if CoordsOnHandle(AX, AY, ghtCircle, LCurve.X1, LCurve.Y1,
                        POINT_WIDTH, POINT_WIDTH, gtkatCenter) then
      begin
        FCurve1 := LCurve;

        if LCurvesFound > 0 then
        begin
          Inc(LCurvesFound);
          Break;
        end;

        Inc(LCurvesFound);
      end
      else
      if CoordsOnHandle(AX, AY, ghtCircle, LCurve.X2, LCurve.Y2,
                        POINT_WIDTH, POINT_WIDTH, gtkatCenter) then
      begin
        FCurve2 := LCurve;

        if LCurvesFound > 0 then
        begin
          Inc(LCurvesFound);
          Break;
        end;

        Inc(LCurvesFound);
      end;
    end;
  end;

  Result := LCurvesFound;
end;

function TgmMagneticLasso.MouseOverCurve(const AX, AY: Integer): Integer;
var
  i, j  : Integer;
  tx, ty: Integer;
  LCurve: TgmICurve;
begin
  { traverse through the list, returning the curve segment's list element
    if the current cursor position is on a curve...
   }

  Result := -1;
  
  if FCurveList.Count > 0 then
  begin
    for i := 0 to (FCurveList.Count - 1) do
    begin
      LCurve := TgmICurve(FCurveList.Items[i]);

      if LCurve.Len > 0 then
      begin
        for j := 0 to (LCurve.Len - 1) do
        begin
          tx := LCurve.Points[j].X;
          ty := LCurve.Points[j].Y;
          
          // Is the specified point close enough to the curve?
          if CoordsInRadius(tx, ty, AX, AY, POINT_WIDTH div 2) then
          begin
            Result := i;
            Break;
          end;
        end;
      end; 
    end;
  end;
end;

procedure TgmMagneticLasso.Draw;
var
  i     : Integer;
  LCurve: TgmICurve;
begin
  // draw the crosshairs target if we're placing a seed
  if mldDrawCurrentSeed in FDrawStates then
  begin
    DrawHandle(FCanvas, ghtCross, FX, FY, TARGET_SIZE, TARGET_SIZE, gtkatCenter,
               FOffsetX, FOffsetY, FScale);

    // draw a line boundary
    if ( not FFirstPoint ) and
       ( not (mldDrawLivewire in FDrawStates) ) then
    begin
      DrawLine(FCanvas, FInitX, FInitY, FX, FY, FOffsetX, FOffsetY, FScale); 
    end;
  end;

  // draw the livewire boundary
  if (mldDrawLivewire in FDrawStates) and (not FFirstPoint) then
  begin
    // See if the mouse has moved. If so, create a new segment...
    if ( not Assigned(FLivewire) ) or
       ( (FInitX <> FLivewire.X1) or
         (FX     <> FLivewire.X2) or
         (FInitY <> FLivewire.Y1) or
         (FY     <> FLivewire.Y2) ) then
    begin
      if Assigned(FLivewire) then
      begin
        FreeAndNil(FLivewire);
      end;

      FLivewire := TgmICurve.Create;

      FLivewire.X1 := FInitX;
      FLivewire.Y1 := FInitY;
      FLivewire.X2 := FX;
      FLivewire.Y2 := FY;

      CalculateCurve(FLivewire);
    end;

    // plot the curve
    DrawCurve(FLivewire);
  end;

  if (mldDrawCurve in FDrawStates) and (not FFirstPoint) then
  begin
    // draw a point at the init point coordinates
    if not FConnected then
    begin
      DrawHandle(FCanvas, ghtFilledCircle, FInitX, FInitY,
                 POINT_WIDTH, POINT_WIDTH, gtkatCenter, FOffsetX, FOffsetY,
                 FScale);
    end;

    // Go through the list of icurves, and render each one...
    if FCurveList.Count > 0 then
    begin
      for i := 0 to (FCurveList.Count - 1) do
      begin
        LCurve := TgmICurve(FCurveList.Items[i]);

        if mldDrawActiveCurve in FDrawStates then
        begin
          // don't draw curve1 at all
          if Assigned(FCurve1) and (LCurve = FCurve1) then
          begin
            Continue;
          end;
        end;

        DrawHandle(FCanvas, ghtFilledCircle, LCurve.X1, LCurve.Y1,
                   POINT_WIDTH, POINT_WIDTH, gtkatCenter, FOffsetX, FOffsetY,
                   FScale);

        if mldDrawActiveCurve in FDrawStates then
        begin
          // draw only the start handle of curve2
          if Assigned(FCurve2) and (LCurve = FCurve2) then
          begin
            Continue;
          end;
        end;

        // plot the curve
        DrawCurve(LCurve);
      end;
    end;
  end;

  if mldDrawActiveCurve in FDrawStates then
  begin
    // plot both curves, and the control point between them
    if Assigned(FCurve1) then
    begin
      DrawLine(FCanvas, FCurve1.X2, FCurve1.Y2, FNewX, FNewY,
               FOffsetX, FOffsetY, FScale);
    end;

    if Assigned(FCurve2) then
    begin
      DrawLine(FCanvas, FCurve2.X1, FCurve2.Y1, FNewX, FNewY,
               FOffsetX, FOffsetY, FScale);
    end;

    DrawHandle(FCanvas, ghtFilledCircle, FNewX, FNewY,
               POINT_WIDTH, POINT_WIDTH, gtkatCenter, FOffsetX, FOffsetY,
               FScale);
  end;    
end;

procedure TgmMagneticLasso.UpdateDisplay;
begin
  if FCurveList.Count > 0 then
  begin
    FDrawStates := [mldDrawCurve];
    Draw;
  end;
end;

procedure TgmMagneticLasso.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FX := AX;
    FY := AY;

    case FToolState of
      mlsNoAction:
        begin
          FToolState  := mlsSeedPlacement;
          FDrawStates := [mldDrawCurrentSeed];

          if not (ssShift in AShift) then
          begin
            FindMaxGradient(FX, FY);
          end;

          FX := Clamp(FX, 0, FSourceBitmap.Width  - 1);
          FY := Clamp(FY, 0, FSourceBitmap.Height - 1);

          FInitX := FX;
          FInitY := FY;

          Draw;
        end;

    else
      // check if the mouse click occurred on a vertex or the curve itself
      if ClickedOnVertex(AX, AY) then
      begin
        FNewX := FX;
        FNewY := FY;

        FToolState  := mlsSeedAdjustment;
        FDrawStates := [mldDrawCurve, mldDrawActiveCurve];

        Draw;
      end
      else
      if not FConnected then
      begin
        // if we're not connected, we're adding a new point

        // pause the tool, but undraw nothing
        FDrawStates := [mldDrawNothing];
        Draw;

        FToolState  := mlsSeedPlacement;
        FDrawStates := [mldDrawCurrentSeed];

        if FInteractive then
        begin
          FDrawStates := FDrawStates + [mldDrawLivewire];
        end;

        Draw;
      end;
    end;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmMagneticLasso.MouseMove(AShift: TShiftState;
  AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    if FToolState = mlsNoAction then
    begin
      Exit;
    end;

    if FToolState = mlsSeedPlacement then
    begin
      FDrawStates := [mldDrawCurrentSeed];

      if FInteractive then
      begin
        FDrawStates := FDrawStates + [mldDrawLivewire];
      end;
    end
    else if FToolState = mlsSeedAdjustment then
    begin
      FDrawStates := [mldDrawActiveCurve];
    end;

    Draw;

    FX := AX;
    FY := AY;

    case FToolState of
      mlsSeedPlacement:
        begin
          // hold the shift key down to disable the auto-edge snap feature
          if not (ssShift in AShift) then
          begin
            FindMaxGradient(FX, FY);
          end;

          FX := Clamp(FX, 0, FSourceBitmap.Width  - 1);
          FY := Clamp(FY, 0, FSourceBitmap.Height - 1);

          if FFirstPoint then
          begin
            FInitX := FX;
            FInitY := FY;
          end;
        end;

      mlsSeedAdjustment:
        begin
          // move the current seed to the location of the cursor
          if not (ssShift in AShift) then
          begin
            FindMaxGradient(FX, FY);
          end;

          FX := Clamp(FX, 0, FSourceBitmap.Width  - 1);
          FY := Clamp(FY, 0, FSourceBitmap.Height - 1);

          FNewX := FX;
          FNewY := FY;
        end;
    end;

    Draw;
  end;
end;

procedure TgmMagneticLasso.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LCurve: TgmICurve;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed := False;

    { Make sure X didn't skip the button release event -- as it's known
      to do
     }
    if FToolState = mlsWaiting then
    begin
      Exit;
    end;

    // Undraw everything
    case FToolState of
      mlsSeedPlacement:
        begin
          FDrawStates := [mldDrawCurve, mldDrawCurrentSeed];

          if FInteractive then
          begin
            FDrawStates := FDrawStates + [mldDrawLivewire];
          end;
        end;

      mlsSeedAdjustment:
        begin
          FDrawStates := [mldDrawCurve, mldDrawActiveCurve];
        end;
    end;

    Draw;

    // progress to the next stage of intelligent selection
    case FToolState of
      mlsSeedPlacement:
        begin
          // add a new icurve
          if not FFirstPoint then
          begin
            // determine if we're connecting to the first point
            if FCurveList.Count > 0 then
            begin
              LCurve := TgmICurve(FCurveList.Items[0]);

              if CoordsOnHandle(FX, FY, ghtCircle, LCurve.X1, LCurve.Y1,
                                POINT_WIDTH, POINT_WIDTH, gtkatCenter) then
              begin
                FX         := LCurve.X1;
                FY         := LCurve.Y1;
                FConnected := True;
              end;
            end;

            // create the new curve segment
            if (FInitX <> FX) or (FInitY <> FY) then
            begin
              LCurve := TgmICurve.Create;

              LCurve.X1 := FInitX;
              LCurve.Y1 := FInitY;
              LCurve.X2 := FX;
              LCurve.Y2 := FY;

              FInitX := FX;
              FInitY := FY;

              FCurveList.Add(LCurve);
              CalculateCurve(LCurve);
            end;
          end
          else // this was our first point
          begin
            FFirstPoint := False;
          end;
        end;

      mlsSeedAdjustment:
        begin
          // recalculate both curves
          if Assigned(FCurve1) then
          begin
            FCurve1.X1 := FNewX;
            FCurve1.Y1 := FNewY;

            CalculateCurve(FCurve1);
          end;

          if Assigned(FCurve2) then
          begin
            FCurve2.X2 := FNewX;
            FCurve2.Y2 := FNewY;

            CalculateCurve(FCurve2);
          end;
        end;
    end;

    FToolState := mlsWaiting;

    // draw only the boundary
    FDrawStates := [mldDrawCurve];
    Draw;

    // convert the curves into a region
    if FConnected then
    begin
      ConvertCurvesToRegion;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure InitGlobalArrays;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    //  The diagonal weight array
    DiagonalWeight[i] := Round(i * G_SQRT2);

    //  The direction value array
    DirectionValue[i, 0] := ( 127 - Abs(127 - i) ) * 2;
    DirectionValue[i, 1] := Abs(127 - i) * 2;
    DirectionValue[i, 2] := Abs(191 - i) * 2;
    DirectionValue[i, 3] := Abs(63 - i)  * 2;
  end;

  //  set the 256th index of the direction_values to the hightest cost
  DirectionValue[255, 0] := 255;
  DirectionValue[255, 1] := 255;
  DirectionValue[255, 2] := 255;
  DirectionValue[255, 3] := 255;
end; 

initialization

  InitGlobalArrays;

end.
