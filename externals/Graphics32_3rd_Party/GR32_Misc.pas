unit GR32_Misc;

(* BEGIN LICENSE BLOCK *********************************************************
 * Version: MPL 1.1
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
 * The Original Code is GR32_Misc.
 * The Initial Developer of the Original Code is Angus Johnson and is
 * Copyright (C) 2009-2010 the Initial Developer. All Rights Reserved.
 *
 * Version 3.92 (Last updated 10-Nov-2010)
 *
 * END LICENSE BLOCK **********************************************************)


(* ***** UPDATE *****
 *
 * Date: 2017-02-12
 * By: Ma Xiaoguang
 *
 * Description:
 *
 *     Reference to GR32_PolygonsOld.pas instead of GR32_Polygons.pas,
 *   because the code in GR32_PolygonsOld.pas is actually the code this
 *   unit originally referenced to. The code in GR32_Polygons.pas which
 *   was included in the upcoming Graphics32 version 2.0 is completely
 *   different than the code in the old version.
 *
 * *****************)

interface

{$I GR32.inc}
{$IFDEF COMPILER7}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

{.$DEFINE GR32_PolygonsEx}

uses
{$IFDEF CLX}
  Qt, Types,
  {$IFDEF LINUX}Libc, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$ELSE}
  Windows, Types,
{$ENDIF}
  Classes, SysUtils, Math, GR32, GR32_LowLevel, GR32_Blend, GR32_Transforms,
{$IFDEF GR32_PolygonsEx}
  GR32_PolygonsEx, GR32_VPR,
{$ENDIF}
  GR32_Math, GR32_PolygonsOld;

type
  TBalloonPos = (bpNone, bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight);
  TCorner     = (cTopLeft, cTopRight, cBottomLeft, cBottomRight);
  TArrayOfArrayOfArrayOfFixedPoint = array of TArrayOfArrayOfFixedPoint;

procedure OffsetPoint(var pt: TFloatPoint; dx, dy: Single); overload;
procedure OffsetPoint(var pt: TFixedPoint; dx, dy: Single); overload;
procedure OffsetPoints(var pts: TArrayOfFixedPoint; dx, dy: Single); overload;
procedure OffsetPoints(var pts: TArrayOfFloatPoint; dx, dy: Single); overload;
procedure OffsetPolyPoints(var polyPts: TArrayOfArrayOfFixedPoint; dx, dy: Single);
procedure OffsetPolyPolyPoints(var polyPolyPts: TArrayOfArrayOfArrayOfFixedPoint; dx, dy: Single);

function CopyPolyPoints(const polyPts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFixedPoint;
function ReversePoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint; overload;
function ReversePoints(const pts: TArrayOfFloatPoint): TArrayOfFloatPoint; overload;
procedure ConcatenatePoints(var pts: TArrayOfFixedPoint;
  const extras: TArrayOfFixedPoint); overload;
procedure ConcatenatePoints(var pts: TArrayOfFixedPoint;
  const extras: array Of TFixedPoint); overload;
procedure ConcatenatePolyPoints(var polyPts: TArrayOfArrayOfFixedPoint;
  const extras: TArrayOfArrayOfFixedPoint);

function AFixedToAFloat(pts: TArrayOfFixedPoint): TArrayOfFloatPoint;
function AAFixedToAAFloat(ppts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint;
function AFloatToAFixed(pts: TArrayOfFloatPoint): TArrayOfFixedPoint;
function AAFloatToAAFixed(ppts: TArrayOfArrayOfFloatPoint):
  TArrayOfArrayOfFixedPoint;

function MakeArrayOfFixedPoints(const a: array of Single): TArrayOfFixedPoint; overload;
function MakeArrayOfFixedPoints(const rec: TRect): TArrayOfFixedPoint; overload;
function MakeArrayOfFixedPoints(const rec: TFixedRect): TArrayOfFixedPoint; overload;
function MakeArrayOfFixedPoints(const rec: TFloatRect): TArrayOfFixedPoint; overload;
function MakeArrayOfFixedPoints(const a: TArrayOfFloatPoint): TArrayOfFixedPoint; overload;

function MakeArrayOfFloatPoints(const a: array of Single): TArrayOfFloatPoint; overload;
function MakeArrayOfFloatPoints(const a: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;

function MakeArrayOfArrayOfFloatPoints(const a: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint;
function MakeArrayOfFloat(const a: array of TFloat): TArrayOfFloat;

procedure OffsetFixedRect(var rec: TFixedRect; dx, dy: Single);
procedure OffsetFloatRect(var rec: TFloatRect; dx, dy: Single);

function IsDuplicatePoint(const p1,p2: TFixedPoint): Boolean; overload;
function IsDuplicatePoint(const p1,p2: TFloatPoint): Boolean; overload;
function IsDuplicateRect(const rec1, rec2: TFloatRect): Boolean;

function StripDuplicatePoints(const pts: array of TFixedPoint): TArrayOfFixedPoint; overload;
function StripDuplicatePoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint; overload;

function GetPointAtAngleFromPoint(const pt: TFixedPoint;
  const dist, Radians: Single): TFixedPoint;

function SquaredDistBetweenPoints(const pt1, pt2: TFixedPoint): Single; overload;
function SquaredDistBetweenPoints(const pt1, pt2: TFloatPoint): Single; overload;
function DistBetweenPoints(const pt1, pt2: TFixedPoint): Single; overload;
function DistBetweenPoints(const pt1, pt2: TFloatPoint): Single; overload;

function ClosestPointOnLine(const pt, lnA, lnB: TFloatPoint;
  ForceResultBetweenLinePts: Boolean): TFloatPoint;
function DistOfPointFromLine(const pt, lnA, lnB: TFloatPoint;
  ForceResultBetweenLinePts: Boolean): TFloat; overload;
function DistOfPointFromLine(const pt, lnA, lnB: TFixedPoint;
  ForceResultBetweenLinePts: Boolean): TFloat; overload;
function IntersectionPoint(const ln1A, ln1B, ln2A, ln2B: TFixedPoint;
  out IntersectPoint: TFixedPoint): Boolean;

function RotatePoint(pt, origin: TFixedPoint; const Radians: Single): TFixedPoint;
function RotatePoints(const pts: array of TFixedPoint;
  origin: TFixedPoint; Radians: Single): TArrayOfFixedPoint; overload;
function RotatePoints(const pts: array of TFixedPoint;
  Radians: Single): TArrayOfFixedPoint; overload;
procedure RotatePolyPoints(var pts: TArrayOfArrayOfFixedPoint;
  origin: TFixedPoint; Radians: Single);

function MidPoint(pt1, pt2: TFixedPoint): TFixedPoint;

function GetAngleOfPt2FromPt1(pt1, pt2: TFixedPoint): Single; overload;
function GetAngleOfPt2FromPt1(pt1, pt2: TFloatPoint): Single; overload;

function PtInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean; overload;
function PtInPolygon(const pt: TFloatPoint; const Pts: TArrayOfFloatPoint): Boolean; overload;
function FixedPtInRect(const R: TFixedRect; const P: TFixedPoint): Boolean;

function MakePoint(const pt: TFixedPoint): TPoint; overload;
function MakePoint(const pt: TFloatPoint): TPoint; overload;

function FloatPoints(const pts: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;
function FixedPoints(const pts: TArrayOfFloatPoint): TArrayOfFixedPoint; overload;
function FloatPoints(const ppts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint; overload;
function FixedPoints(const ppts: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFixedPoint; overload;

function CopyPoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint;

function GetBoundsRect(pts: array of TFixedPoint): TRect;
function GetBoundsFixedRect(pts: array of TFixedPoint): TFixedRect; overload;
function GetBoundsFixedRect(polyPts: TArrayOfArrayOfFixedPoint): TFixedRect; overload;
function GetBoundsFloatRect(pts: array of TFixedPoint): TFloatRect; overload;
function GetBoundsFloatRect(pts: array of TFloatPoint): TFloatRect; overload;

function GetRectUnion(const rec1, rec2: TFixedRect): TFixedRect;

function GetConvexHull(const polygon: TArrayOfFixedPoint): TArrayOfFixedPoint;

//CreateMaskFromPolygon: creates a white polygon on a black background ...
function CreateMaskFromPolygon(bitmap: TBitmap32;
  const polygon: TArrayOfFixedPoint): TBitmap32; overload;
function CreateMaskFromPolygon(bitmap: TBitmap32;
  const polygons: TArrayOfArrayOfFixedPoint): TBitmap32; overload;
//ApplyMaskToAlpha: black in the mask sets the bitmap alpha channel to 0 ...
//nb: ApplyMaskToAlpha is useless with opaque bitmaps.
procedure ApplyMaskToAlpha(bitmap, mask: TBitmap32; invertMask: Boolean = False);
//ApplyMask: black in mask -> use original; white in mask -> use modified.
procedure ApplyMask(modifiedBmp, originalBmp, maskBmp: TBitmap32; invertMask: Boolean = False);
procedure InvertMask(maskBmp: TBitmap32);

function MakeArrayOfColor32(colors: array of TColor32): TArrayOfColor32;

//SmoothChart: returns an array of points representing a smoothing of ChartPts.
//It assumes that chartPts progress in a positive direction along the X-axis.
//The returned polyline will pass through all ChartPts' points.
//(xStep is the usual interval between points on the x-axis.)
function SmoothChart(const chartPts: array of TFixedPoint; xStep: Integer): TArrayOfFixedPoint;

//Cubic Beziers (CBezier, CSpline):
//GetCBezierPoints: the last (D) control_point of a precenting segment
//becomes the first (A) control_point of a following segment.
function GetCBezierPoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
//GetCSplinePoints: 'Smooth' CBeziers where the first and last control_points
//(A & D) are derived from the midpoints of adjacent 'B' & 'C' control_points.
function GetCSplinePoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;

//Quadratic Beziers (QBezier, QSpline):
//GetQBezierPoints: the last (C) control_point of a precenting segment
//becomes the first (A) control_point of a following segment.
function GetQBezierPoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
//GetQSplinePoints: 'Smooth' QBeziers where the first and last control_points
//(A & C) are derived from the midpoints of adjacent 'B' control_points.
function GetQSplinePoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;

function GetEllipsePoints(const ellipseRect: TFloatRect): TArrayOfFixedPoint;

function GetPtOnEllipseFromAngleEccentric(const ellipseRect: TFloatRect;
  eccentric_angle_radians: Single): TFloatPoint;

function GetPointsAroundEllipse(const ellipseRect:
  TFloatRect; PointCount: Integer): TArrayOfFixedPoint;

function GetArcPoints(const ellipseRect: TFloatRect;
  startPt, endPt: TFloatPoint): TArrayOfFixedPoint; overload;

function GetArcPoints(const ellipseRect: TFloatRect;
  start_degrees, end_degrees: Single): TArrayOfFixedPoint; overload;

function GetArcPointsEccentric(const ellipseRect: TFloatRect;
  start_eccentric, end_eccentric: Single): TArrayOfFixedPoint;

function GetPiePoints(const ellipseRect: TFloatRect;
  startPt, endPt: TFloatPoint): TArrayOfFixedPoint; overload;

function GetPiePoints(const ellipseRect: TFloatRect;
  start_degrees, end_degrees: Single): TArrayOfFixedPoint; overload;

function GetPiePointsEccentric(const ellipseRect: TFloatRect;
  start_eccentric, end_eccentric: Single): TArrayOfFixedPoint;

function GetRoundedRectanglePoints(const rect: TFloatRect;
  roundingPercent: cardinal): TArrayOfFixedPoint;

function GetBalloonedEllipsePoints(const ellipseRect: TFloatRect;
  balloonPos: TBalloonPos): TArrayOfFixedPoint;

function GetPipeCornerPoints(rec: TFloatRect;
  pipeWidth: Single; corner: TCorner): TArrayOfFixedPoint;

function GetStarPoints(const Center: TFixedPoint;
  PointCount: Integer; radius1, radius2: Single): TArrayOfFixedPoint;

function GradientColor(color1, color2: TColor32; frac: Single): TColor32;
function GetColor(const colors: array of TColor32; fraction: Single): TColor32;

//IsClockwise: determines the direction the points take to construct a polygon.
function IsClockwise(const pts: TArrayOfFixedPoint): Boolean; overload;
function IsClockwise(const pts: TArrayOfFloatPoint): Boolean; overload;

function BuildDashedLine(const Points: TArrayOfFixedPoint;
  const DashArray: array of TFloat; DashOffset: TFloat = 0): TArrayOfArrayOfFixedPoint;

function GetUnitVector(const pt1, pt2: TFixedPoint): TFloatPoint;
function GetUnitNormal(const pt1, pt2: TFixedPoint): TFloatPoint; overload;
function GetUnitNormal(const pt1, pt2: TFloatPoint): TFloatPoint; overload;

function NearlyMatch(const s1, s2, tolerance: Single): Boolean;

//quick and easy (anti-aliased) lines of 1px line width ...
procedure SimpleLine(bitmap: TBitmap32;
  const pts: array of TFixedPoint; color: TColor32; closed: Boolean = False); overload;
procedure SimpleLine(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFixedPoint;
  color: TColor32; closed: Boolean); overload;

//fill a set of closed points with a color (with 1px width edge) ...
procedure SimpleFill(bitmap: TBitmap32; pts: array of TFixedPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding); overload;
procedure SimpleFill(bitmap: TBitmap32; const pts: array of TFloatPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding); overload;
procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding); overload;

//fill a set of closed points with a bitmap32 pattern (with 1px width edge) ...
procedure SimpleFill(bitmap: TBitmap32; const pts: array of TFixedPoint;
  edgeColor: TColor32; pattern: TBitmap32; aFillMode: TPolyFillMode = pfWinding); overload;

procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  edgeColor: TColor32; pattern: TBitmap32; aFillMode: TPolyFillMode = pfWinding); overload;

procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFloatPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding); overload;

//fill a set of closed points with a color gradient at the specified angle ...
procedure SimpleGradientFill(
  bitmap: TBitmap32; pts: array of TFixedPoint; edgeColor: TColor32;
  const colors: array Of TColor32; angle_degrees: Integer); overload;
procedure SimpleGradientFill(
  bitmap: TBitmap32; pts: TArrayOfArrayOfFixedPoint; edgeColor: TColor32;
  const colors: array Of TColor32; angle_degrees: Integer); overload;

procedure SimpleStippleFill(bitmap: TBitmap32; pts: array of TFixedPoint;
  const colors: array Of TColor32; step: Single; angle_degrees: Integer);

procedure SimpleRadialFill(bitmap: TBitmap32; const pt: TFixedPoint;
  Radius: Single; const colors: array of TColor32); overload;
procedure SimpleRadialFill(bitmap: TBitmap32; rec: TFloatRect;
  const colors: array of TColor32); overload;

procedure SimpleRadialFill(bitmap: TBitmap32;
  const pts: array of TFixedPoint; const
  colors: array of TColor32; angle_degrees: Integer = 0); overload;
procedure SimpleRadialFill(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFixedPoint;
  const colors: array of TColor32; angle_degrees: Integer = 0); overload;

//SimpleShadow: fadeRate parameter ...
//  0  = MAXIMUM_SHADOW_FADE (exponential) => palest shadows
//  1 .. 9 => mixture of exponential and linear shadow fades
//  5  = MEDIUM_SHADOW_FADE
//  10 = MINIMUM_SHADOW_FADE (linear)     => darkest shadows that still fade
// >10 = NO_SHADOW_FADE                   => uniform shadow color
procedure SimpleShadow(bitmap: TBitmap32; const pts: TArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; shadowColor: TColor32;
  closed: Boolean = False; NoInteriorBleed: Boolean = False); overload;
procedure SimpleShadow(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; shadowColor: TColor32;
  closed: Boolean = False; NoInteriorBleed: Boolean = False); overload;

procedure Simple3D(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFloatPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32); overload;
procedure Simple3D(bitmap: TBitmap32; const pts: TArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32); overload;
procedure Simple3D(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32); overload;

//nb: Using semiopaque colors, SimpleBevel can be an easy way to add watermarks
procedure SimpleBevel(bitmap: TBitmap32; ppts: TArrayOfArrayOfFixedPoint;
  strokeWidth: Integer; lightClr, darkClr: TColor32; angle_degrees: Integer);

const
  MAXIMUM_SHADOW_FADE = 0;
  MEDIUM_SHADOW_FADE  = 5;
  MINIMUM_SHADOW_FADE = 10;
  NO_SHADOW_FADE      = 11; //anything > 10

////////////////////////////////////////////////////////////////////////////////

{$IFDEF GR32_PolygonsEx}
type
  //TPolygon32Ex - is a wrapper class that redirects polygon rasterisation
  //to Mattias Andersson's GR32_PolygonsEx unit by emulating a small subset
  //of TPolygon32's methods and properties ...
  TPolygon32Ex = class
  private
    fPPts: TArrayOfArrayOfFixedPoint;
    fClosed: Boolean;               //ignored (always true)
    fAntialiased: Boolean;          //ignored (always true)
    fAntialiasMode: TAntialiasMode; //ignored
    fFillMode: TPolyFillMode;
  public
    constructor Create;
    procedure Clear;
    procedure Newline;
    procedure AddPoints(var First: TFixedPoint; Count: Integer);
    procedure Draw(Bitmap: TBitmap32; OutlineColor, FillColor: TColor32); overload;
    procedure Draw(Bitmap: TBitmap32; OutlineColor: TColor32; Filler: TCustomPolygonFiller); overload;
    procedure DrawFill(Bitmap: TBitmap32; Color: TColor32); overload;
    procedure DrawFill(Bitmap: TBitmap32; Filler: TCustomPolygonFiller); overload;
    procedure DrawEdge(Bitmap: TBitmap32; Color: TColor32);
    property  Closed: Boolean read FClosed write FClosed;
    property  Antialiased: Boolean read fAntialiased write fAntialiased;
    property  AntialiasMode: TAntialiasMode read FAntialiasMode write FAntialiasMode;
    property  FillMode: TPolyFillMode read FFillMode write FFillMode;
  end;
{$ENDIF}
////////////////////////////////////////////////////////////////////////////////

const
  rad01 = pi / 180;
  rad30 = pi / 6;
  rad45 = pi / 4;
  rad60 = pi / 3;
  rad90 = pi / 2;
  rad180 = pi;
  rad270 = rad90 * 3;
  rad360 = rad180 * 2;
  DegToRad = pi/180;
  RadToDeg = 180/pi;
  SingleLineLimit = 1.5;
  SqrtTwo = 1.41421356;
  half = 0.5;
  cbezier_tolerance = 0.5;
  qbezier_tolerance = 0.5;

implementation

{$IFDEF GR32_PolygonsEx}

//------------------------------------------------------------------------------
// TPolygon32Ex methods ...
//------------------------------------------------------------------------------

constructor TPolygon32Ex.Create;
begin
  inherited;
  fClosed := true;
  Newline;
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.Clear;
begin
  fPPts := nil;
  NewLine;
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.Newline;
begin
  SetLength(fPPts, Length(fPPts) + 1);
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.AddPoints(var First: TFixedPoint; Count: Integer);
var
  H, L, I: Integer;
begin
  H := High(fPPts);
  if H < 0 then
  begin
    SetLength(fPPts, 1);
    H := 0;
  end;
  L := Length(fPPts[H]);
  SetLength(fPPts[H], L + Count);
  {$R-}
  for I := 0 to Count - 1 do
    fPPts[H, L + I] := PFixedPointArray(@First)[I];
  {$R+}
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.Draw(Bitmap: TBitmap32; OutlineColor, FillColor: TColor32);
var
  Tmp: TArrayOfArrayOfFloatPoint;
begin
  Tmp := AAFixedToAAFloat(fPPts);
  PolyPolygonFS(Bitmap, Tmp, FillColor, fFillMode);
  if (OutlineColor and $FF000000) <> 0 then
    PolyPolylineXS(Bitmap, fPPts, OutlineColor, true);
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.Draw(Bitmap: TBitmap32; OutlineColor: TColor32;
  Filler: TCustomPolygonFiller);
var
  Tmp: TArrayOfArrayOfFloatPoint;
begin
  Tmp := AAFixedToAAFloat(fPPts);
  PolyPolygonFS(Bitmap, Tmp, Filler, fFillMode);
  if (OutlineColor and $FF000000) <> 0 then
    PolyPolylineXS(Bitmap, fPPts, OutlineColor, true);
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.DrawFill(Bitmap: TBitmap32; Color: TColor32);
var
  Tmp: TArrayOfArrayOfFloatPoint;
begin
  Tmp := AAFixedToAAFloat(fPPts);
  PolyPolygonFS(Bitmap, Tmp, Color, fFillMode);
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.DrawFill(Bitmap: TBitmap32; Filler: TCustomPolygonFiller);
var
  Tmp: TArrayOfArrayOfFloatPoint;
begin
  Tmp := AAFixedToAAFloat(fPPts);
  Bitmap.BeginUpdate;
  PolyPolygonFS(Bitmap, Tmp, Filler, fFillMode);
  Bitmap.EndUpdate;
  Bitmap.Changed;
end;
//------------------------------------------------------------------------------

procedure TPolygon32Ex.DrawEdge(Bitmap: TBitmap32; Color: TColor32);
begin
  Bitmap.BeginUpdate;
  PolyPolylineXS(Bitmap, fPPts, Color, true);
  Bitmap.EndUpdate;
  Bitmap.Changed;
end;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure OffsetPoint(var pt: TFloatPoint; dx, dy: Single);
begin
  pt.X := pt.X + dx;
  pt.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

procedure OffsetPoint(var pt: TFixedPoint; dx, dy: Single);
begin
  with pt do
  begin
    X := X + Fixed(dx);
    Y := Y + Fixed(dy);
  end;
end;
//------------------------------------------------------------------------------

procedure OffsetPoints(var pts: TArrayOfFixedPoint; dx, dy: Single);
var
  i: Integer;
  dxFixed, dyFixed: TFixed;
begin
  dxFixed := Fixed(dx);
  dyFixed := Fixed(dy);
  for i := 0 to high(pts) do
    with pts[i] do
    begin
      X := X + dxFixed;
      Y := Y + dyFixed;
    end;
end;
//------------------------------------------------------------------------------

procedure OffsetPoints(var pts: TArrayOfFloatPoint; dx, dy: Single);
var
  i: Integer;
begin
  for i := 0 to high(pts) do
    with pts[i] do
    begin
      X := X + dx;
      Y := Y + dy;
    end;
end;
//------------------------------------------------------------------------------

procedure OffsetPolyPoints(var polyPts: TArrayOfArrayOfFixedPoint; dx, dy: Single);
var
  i,j: Integer;
  dxFixed, dyFixed: TFixed;
begin
  dxFixed := Fixed(dx);
  dyFixed := Fixed(dy);
  for i := 0 to high(polyPts) do
    for j := 0 to high(polyPts[i]) do
      with polyPts[i][j] do
      begin
        X := X + dxFixed;
        Y := Y + dyFixed;
      end;
end;
//------------------------------------------------------------------------------

procedure OffsetPolyPolyPoints(var polyPolyPts: TArrayOfArrayOfArrayOfFixedPoint;
  dx, dy: Single);
var
  i,j,k: Integer;
  dxFixed, dyFixed: TFixed;
begin
  dxFixed := Fixed(dx);
  dyFixed := Fixed(dy);
  for i := 0 to high(polyPolyPts) do
    for j := 0 to high(polyPolyPts[i]) do
      for k := 0 to high(polyPolyPts[i][j]) do
        with polyPolyPts[i][j][k] do
        begin
          X := X + dxFixed;
          Y := Y + dyFixed;
        end;
end;
//------------------------------------------------------------------------------

function CopyPolyPoints(const polyPts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFixedPoint;
var
  i: Integer;
begin
  SetLength(Result, Length(polyPts));
  for i := 0 to high(polyPts) do
    Result[i] := Copy(polyPts[i], 0, Length(polyPts[i]));
end;
//------------------------------------------------------------------------------

function ReversePoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint;
var
  i, highPts: Integer;
begin
  highPts := high(pts);
  SetLength(Result, highPts +1);
  for i := 0 to highPts do Result[i] := pts[highPts - i];
end;
//------------------------------------------------------------------------------

function ReversePoints(const pts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  i, highPts: Integer;
begin
  highPts := high(pts);
  SetLength(Result, highPts +1);
  for i := 0 to highPts do Result[i] := pts[highPts - i];
end;
//------------------------------------------------------------------------------

procedure ConcatenatePoints(var pts: TArrayOfFixedPoint;
  const extras: TArrayOfFixedPoint);
var
  i, ptsCnt, extrasCnt: Integer;
begin
  ptsCnt := Length(pts);
  extrasCnt := Length(extras);
  if extrasCnt = 0 then exit;
  SetLength(pts, ptsCnt + extrasCnt);
  for i := 0 to extrasCnt -1 do
    pts[ptsCnt + i] := extras[i];
end;
//------------------------------------------------------------------------------

procedure ConcatenatePoints(var pts: TArrayOfFixedPoint;
  const extras: array Of TFixedPoint);
var
  i, ptsCnt, extrasCnt: Integer;
begin
  ptsCnt := Length(pts);
  extrasCnt := Length(extras);
  if extrasCnt = 0 then exit;
  SetLength(pts, ptsCnt + extrasCnt);
  for i := 0 to extrasCnt -1 do
    pts[ptsCnt + i] := extras[i];
end;
//------------------------------------------------------------------------------

procedure ConcatenatePolyPoints(var polyPts: TArrayOfArrayOfFixedPoint;
  const extras: TArrayOfArrayOfFixedPoint);
var
  i, polyPtsCnt, ExtrasCnt: Integer;
begin
  polyPtsCnt := Length(polyPts);
  ExtrasCnt := Length(extras);
  if ExtrasCnt = 0 then exit;
  SetLength(polyPts, polyPtsCnt + ExtrasCnt);
  for i := 0 to ExtrasCnt -1 do
    polyPts[polyPtsCnt + i] := copy(extras[i], 0, Length(extras[i]));
end;
//------------------------------------------------------------------------------

function AFixedToAFloat(pts: TArrayOfFixedPoint): TArrayOfFloatPoint;
var
  i, Len: Integer;
begin
  Len := Length(pts);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := FloatPoint(pts[i]);
end;
//------------------------------------------------------------------------------

function AAFixedToAAFloat(ppts: TArrayOfArrayOfFixedPoint):
  TArrayOfArrayOfFloatPoint;
var
  i,j, Len, len2: Integer;
begin
  Len := Length(ppts);
  SetLength(Result, Len);
  for i := 0 to Len -1 do
  begin
    len2 := Length(ppts[i]);
    SetLength(Result[i], len2);
    for j := 0 to len2 -1 do
      Result[i][j] := FloatPoint(ppts[i][j]);
  end;
end;
//------------------------------------------------------------------------------

function AFloatToAFixed(pts: TArrayOfFloatPoint): TArrayOfFixedPoint;
var
  i, Len: Integer;
begin
  Len := Length(pts);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := FixedPoint(pts[i]);
end;
//------------------------------------------------------------------------------

function AAFloatToAAFixed(ppts: TArrayOfArrayOfFloatPoint):
  TArrayOfArrayOfFixedPoint;
var
  i,j, Len, len2: Integer;
begin
  Len := Length(ppts);
  SetLength(Result, Len);
  for i := 0 to Len -1 do
  begin
    len2 := Length(ppts[i]);
    SetLength(Result[i], len2);
    for j := 0 to len2 -1 do
      Result[i][j] := FixedPoint(ppts[i][j]);
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFixedPoints(const a: array of Single): TArrayOfFixedPoint;
var
  i, Len: Integer;
begin
  Len := Length(a) div 2;
  SetLength(Result, Len);
  if Len = 0 then exit;
  for i := 0 to Len -1 do
  begin
    Result[i].X := round(a[i*2] * FixedOne);
    Result[i].Y := round(a[i*2 +1] * FixedOne);
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFixedPoints(const rec: TRect): TArrayOfFixedPoint;
begin
  SetLength(Result, 4);
  with FixedRect(rec) do
  begin
    Result[0].X := Left;
    Result[0].Y := Top;
    Result[1].X := Right;
    Result[1].Y := Top;
    Result[2].X := Right;
    Result[2].Y := Bottom;
    Result[3].X := Left;
    Result[3].Y := Bottom;
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFixedPoints(const rec: TFloatRect): TArrayOfFixedPoint;
begin
  SetLength(Result, 4);
  with FixedRect(rec) do
  begin
    Result[0].X := Left;
    Result[0].Y := Top;
    Result[1].X := Right;
    Result[1].Y := Top;
    Result[2].X := Right;
    Result[2].Y := Bottom;
    Result[3].X := Left;
    Result[3].Y := Bottom;
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFixedPoints(const rec: TFixedRect): TArrayOfFixedPoint;
begin
  SetLength(Result, 4);
  with rec do
  begin
    Result[0].X := Left;
    Result[0].Y := Top;
    Result[1].X := Right;
    Result[1].Y := Top;
    Result[2].X := Right;
    Result[2].Y := Bottom;
    Result[3].X := Left;
    Result[3].Y := Bottom;
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFixedPoints(const a: TArrayOfFloatPoint): TArrayOfFixedPoint;
var
  i, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := FixedPoint(a[i]);
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: array of Single): TArrayOfFloatPoint;
var
  i, Len: Integer;
begin
  Len := Length(a) div 2;
  SetLength(Result, Len);
  if Len = 0 then exit;
  for i := 0 to Len -1 do
  begin
    Result[i].X := a[i*2];
    Result[i].Y := a[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: TArrayOfFixedPoint): TArrayOfFloatPoint;
var
  i, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := FloatPoint(a[i]);
end;
//------------------------------------------------------------------------------

function MakeArrayOfArrayOfFloatPoints(const a: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint;
var
  i, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := MakeArrayOfFloatPoints(a[i]);
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloat(const a: array of TFloat): TArrayOfFloat;
var
  i, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := a[i];
end;
//------------------------------------------------------------------------------

procedure OffsetFixedRect(var rec: TFixedRect; dx, dy: Single);
begin
  with rec do
  begin
    Left := Left + Fixed(dx);
    Top := Top + Fixed(dy);
    Right := Right + Fixed(dx);
    Bottom := Bottom + Fixed(dy);
  end;
end;
//------------------------------------------------------------------------------

procedure OffsetFloatRect(var rec: TFloatRect; dx, dy: Single);
begin
  with rec do
  begin
    Left := Left + dx;
    Top := Top + dy;
    Right := Right + dx;
    Bottom := Bottom + dy;
  end;
end;
//------------------------------------------------------------------------------

function IsDuplicatePoint(const p1,p2: TFixedPoint): Boolean;
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function IsDuplicatePoint(const p1,p2: TFloatPoint): Boolean;
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function IsDuplicateRect(const rec1,rec2: TFloatRect): Boolean;
const
  tolerance = 0.001;
begin
  Result :=
    (abs(rec1.Left - rec2.Left) < tolerance) and
    (abs(rec1.Top - rec2.Top) < tolerance) and
    (abs(rec1.Right - rec2.Right) < tolerance) and
    (abs(rec1.Bottom - rec2.Bottom) < tolerance);
end;
//------------------------------------------------------------------------------

function StripDuplicatePoints(const pts: array of TFixedPoint): TArrayOfFixedPoint;
var
  i, j, Len: Integer;
begin
  Len := Length(pts);
  SetLength(Result,Len);
  if Len = 0 then exit;
  Result[0] := pts[0];
  j := 1;
  for i := 1 to Len -1 do
  begin
    if IsDuplicatePoint(Result[j-1],pts[i]) then continue;
    Result[j] := pts[i];
    inc(j);
  end;
  if j < Len then SetLength(Result,j);
end;
//------------------------------------------------------------------------------

function StripDuplicatePoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint;
var
  i, j, Len: Integer;
begin
  Len := Length(pts);
  SetLength(Result,Len);
  if Len = 0 then exit;
  Result[0] := pts[0];
  j := 1;
  for i := 1 to Len -1 do
  begin
    if IsDuplicatePoint(Result[j-1],pts[i]) then continue;
    Result[j] := pts[i];
    inc(j);
  end;
  if j < Len then SetLength(Result,j);
end;
//------------------------------------------------------------------------------

function GetPointAtAngleFromPoint(const pt: TFixedPoint;
  const dist, Radians: Single): TFixedPoint;
var
  SinAng, CosAng: Single;
begin
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  Result.X := round(dist * CosAng*FixedOne) + pt.X;
  Result.Y := -round(dist * SinAng *FixedOne) + pt.Y; //nb: Y axis is +ve down
end;
//------------------------------------------------------------------------------

function SquaredDistBetweenPoints(const pt1, pt2: TFixedPoint): Single;
var
  X, Y: Single;
begin
  X := (pt2.X - pt1.X) * FixedToFloat;
  Y := (pt2.Y - pt1.Y) * FixedToFloat;
  Result := X*X + Y*Y;
end;
//------------------------------------------------------------------------------

function SquaredDistBetweenPoints(const pt1, pt2: TFloatPoint): Single;
var
  X, Y: Single;
begin
  X := (pt2.X-pt1.X);
  Y := (pt2.Y-pt1.Y);
  Result := X*X + Y*Y;
end;
//------------------------------------------------------------------------------

function DistBetweenPoints(const pt1, pt2: TFixedPoint): Single;
begin
  Result := Hypot((pt2.X - pt1.X)*FixedToFloat,(pt2.Y - pt1.Y)*FixedToFloat);
end;
//------------------------------------------------------------------------------

function DistBetweenPoints(const pt1, pt2: TFloatPoint): Single;
begin
  Result := Hypot((pt2.X - pt1.X),(pt2.Y - pt1.Y));
end;
//------------------------------------------------------------------------------

function ClosestPointOnLine(const pt, lnA, lnB: TFloatPoint;
  ForceResultBetweenLinePts: Boolean): TFloatPoint;
var
  q: Single;
begin
  if (lnA.X = lnB.X) and (lnA.Y = lnB.Y) then
    Result := lnA
  else
  begin
    q := ((pt.X-lnA.X)*(lnB.X-lnA.X) + (pt.Y-lnA.Y)*(lnB.Y-lnA.Y)) /
      (sqr(lnB.X-lnA.X) + sqr(lnB.Y-lnA.Y));
    if ForceResultBetweenLinePts then constrain(q,0,1);
    Result.X := (1-q)*lnA.X + q*lnB.X;
    Result.Y := (1-q)*lnA.Y + q*lnB.Y;
  end;
end;
//------------------------------------------------------------------------------

function DistOfPointFromLine(const pt, lnA, lnB: TFloatPoint;
  ForceResultBetweenLinePts: Boolean): TFloat;
var
  cpol: TFloatPoint;
begin
  cpol := ClosestPointOnLine(pt, lnA, lnB, ForceResultBetweenLinePts);
  Result := Hypot(pt.X - cpol.X, pt.Y- cpol.Y);
end;
//------------------------------------------------------------------------------

function DistOfPointFromLine(const pt, lnA, lnB: TFixedPoint;
  ForceResultBetweenLinePts: Boolean): TFloat;
begin
  Result := DistOfPointFromLine(FloatPoint(pt),
    FloatPoint(lnA), FloatPoint(lnB), ForceResultBetweenLinePts);
end;
//------------------------------------------------------------------------------

//IntersectionPoint: returns false whenever the lines are parallel
function IntersectionPoint(const ln1A, ln1B, ln2A, ln2B: TFixedPoint;
  out IntersectPoint: TFixedPoint): Boolean;
var
  m1, b1, m2, b2: Single;
begin
  Result := False;
  (*
  nb: testing for segment 
      denominator := (ln2B.Y-ln2A.Y)*(ln1B.X-ln1A.X) -
        (ln2B.X-ln2A.X)*(ln1B.Y-ln1A.Y);
      if denominator = 0 then parallel lines
      ua = ((ln2B.X-ln2A.X)*(ln1A.Y-ln2A.Y)-(ln2B.Y-ln2A.Y)*(ln1A.X-ln2A.X)) / denominator;
      ub = ((ln1B.X-ln1A.X)*(ln1A.Y-ln2A.Y)-(ln1B.Y-ln1A.Y)*(ln1A.X-ln2A.X)) / denominator;
      if 0 <= ua <= 1 and 0 <= ub <= 1 then segments intersect
      IntersectPoint.X = ln1A.X + ua*(ln2A.X-ln1A.X);
      IntersectPoint.Y = ln1A.Y + ua*(ln2A.Y-ln1A.Y);
      see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  *)

  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; //parallel lines
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    IntersectPoint.X := ln1A.X;
    IntersectPoint.Y := round(m2*ln1A.X + b2);
  end
  else if (ln2B.X = ln2A.X) then
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    IntersectPoint.X := ln2A.X;
    IntersectPoint.Y := round(m1*ln2A.X + b1);
  end else
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    if m1 = m2 then exit; //parallel lines
    IntersectPoint.X := round((b2 - b1)/(m1 - m2));
    IntersectPoint.Y := round(m1 * IntersectPoint.X + b1);
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function RotatePoint(pt, origin: TFixedPoint; const Radians: Single): TFixedPoint;
var
  CosAng, SinAng: Single;
begin
  if Radians = 0 then
  begin
    Result := pt;
    exit;
  end;
  //rotates in an anticlockwise direction if Radians > 0;
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  pt.X := pt.X - origin.X;
  pt.Y := pt.Y - origin.Y;
  Result.X := round((pt.X * CosAng) + (pt.Y * SinAng) + origin.X);
  Result.Y := round((pt.Y * CosAng) - (pt.X * SinAng) + origin.Y);
end;
//------------------------------------------------------------------------------

function RotatePoints(const pts: array of TFixedPoint;
  origin: TFixedPoint; Radians: Single): TArrayOfFixedPoint;
var
  i: Integer;
  Tmp: TFixedPoint;
  CosAng, SinAng: Single;
begin
  SetLength(Result,Length(pts));
  if Radians = 0 then
  begin
    for i := 0 to high(pts) do Result[i] := pts[i];
    exit;
  end;
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  for i := 0 to high(pts) do
  begin
    Tmp.X := pts[i].X - origin.X;
    Tmp.Y := pts[i].Y - origin.Y;
    Result[i].X := round((Tmp.X * CosAng) + (Tmp.Y * SinAng) + origin.X);
    Result[i].Y := round((Tmp.Y * CosAng) - (Tmp.X * SinAng) + origin.Y);
  end;
end;
//------------------------------------------------------------------------------

function RotatePoints(const pts: array of TFixedPoint;
  Radians: Single): TArrayOfFixedPoint;
var
  i: Integer;
  CosAng, SinAng: Single;
begin
  SetLength(Result,Length(pts));
  if Radians = 0 then
  begin
    for i := 0 to high(pts) do Result[i] := pts[i];
    exit;
  end;
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  for i := 0 to high(pts) do
  begin
    Result[i].X := round((pts[i].X * CosAng) + (pts[i].Y * SinAng));
    Result[i].Y := round((pts[i].Y * CosAng) - (pts[i].X * SinAng));
  end;
end;
//------------------------------------------------------------------------------

procedure RotatePolyPoints(var pts: TArrayOfArrayOfFixedPoint;
  origin: TFixedPoint; Radians: Single);
var
  i,j: Integer;
  Tmp: TFixedPoint;
  CosAng, SinAng: Single;
begin
  if Radians = 0 then exit;
  GR32_Math.SinCos(Radians, SinAng, CosAng);
  for i := 0 to high(pts) do
    for j := 0 to high(pts[i]) do
      with pts[i][j] do
      begin
        Tmp.X := X - origin.X;
        Tmp.Y := Y - origin.Y;
        X := round((Tmp.X * CosAng) + (Tmp.Y * SinAng) + origin.X);
        Y := round((Tmp.Y * CosAng) - (Tmp.X * SinAng) + origin.Y);
      end;
end;
//------------------------------------------------------------------------------

function MidPoint(pt1, pt2: TFixedPoint): TFixedPoint;
begin
  Result.X := (pt1.X + pt2.X) div 2;
  Result.Y := (pt1.Y + pt2.Y) div 2;
end;
//------------------------------------------------------------------------------

function GetAngleOfPt2FromPt1(pt1, pt2: TFixedPoint): Single;
begin
  with pt2 do
  begin
    X := X - pt1.X; Y := Y - pt1.Y;
    if X = 0 then
    begin
     if Y > 0 then Result := rad270 else Result := rad90;
    end else
    begin
      Result := arctan2(-Y,X);
      if Result < 0 then Result := Result + rad360;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetAngleOfPt2FromPt1(pt1, pt2: TFloatPoint): Single;
begin
  with pt2 do
  begin
    X := X - pt1.X; Y := Y - pt1.Y;
    if X = 0 then
    begin
      if Y > 0 then Result := rad270 else Result := rad90;
    end else
    begin
      Result := arctan2(-Y,X);
      if Result < 0 then Result := Result + rad360;
    end;
  end;
end;
//------------------------------------------------------------------------------

function PtInPolygon(const Pt: TFixedPoint; const Pts: array of TFixedPoint): Boolean;
var
  I: Integer;
  iPt, jPt: PFixedPoint;
begin
  Result := False;
  iPt := @Pts[0];
  jPt := @Pts[High(Pts)];
  for I := 0 to High(Pts) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      (Pt.X - iPt.X < MulDiv(jPt.X - iPt.X, Pt.Y - iPt.Y, jPt.Y - iPt.Y)));
    jPt := iPt;
    Inc(iPt);
  end;
end;
//------------------------------------------------------------------------------


//see: "The Point in Polygon Problem for Arbitrary Polygons" by Kai Hormann
//     http://www2.in.tu-clausthal.de/~hormann/papers/Hormann.2001.TPI.pdf
function PtInPolygon(const pt: TFloatPoint; const Pts: TArrayOfFloatPoint): Boolean;
var
  i, highI, windingNum: Integer;
  ptOnPolygonFlag: Boolean;

  function det(i,j: Integer): Single;
  begin
    Result := (Pts[i].X-pt.X)*(Pts[j].Y-pt.Y)-(Pts[j].X-pt.X)*(Pts[i].Y-pt.Y);
    if Result = 0 then ptOnPolygonFlag := true;
  end;

  procedure process(i,j: Integer);
  begin
    if (Pts[j].Y = pt.Y) then
      if (Pts[j].X = pt.X) then
      begin
        ptOnPolygonFlag := true;
        exit;
      end else if (Pts[i].Y = pt.Y) and
        ((Pts[j].X > pt.X) = (Pts[i].X < pt.X)) then
      begin
        ptOnPolygonFlag := true;
        exit;
      end;
    if (Pts[i].Y < pt.Y) <> (Pts[j].Y < pt.Y) then
      if (Pts[i].X >= pt.X) then
      begin
        if (Pts[j].X >= pt.X) then
        begin
          if Pts[j].Y > Pts[i].Y then inc(windingNum) else dec(windingNum);
        end else if (det(i,j) > 0) = (Pts[j].Y > Pts[i].Y) then
        begin
          if Pts[j].Y > Pts[i].Y then inc(windingNum) else dec(windingNum);
        end;
      end
      else if (Pts[j].X >= pt.X) and
        ((det(i,j) > 0) = (Pts[j].Y > Pts[i].Y)) then
      begin
          if Pts[j].Y > Pts[i].Y then inc(windingNum) else dec(windingNum);
      end;
  end;

begin
  Result := False;
  highI := high(Pts);
  if (highI < 2) or (pt.X = Pts[0].X) and (pt.Y = Pts[0].Y) then exit;

  ptOnPolygonFlag := False;
  windingNum := 0;
  for i := 0 to highI -1 do
  begin
    process(i, i+1);
    if ptOnPolygonFlag then exit;
  end;
  process(highI, 0);
  if not ptOnPolygonFlag then
    Result := odd(windingNum);
end;
//------------------------------------------------------------------------------

function FixedPtInRect(const R: TFixedRect; const P: TFixedPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;
//------------------------------------------------------------------------------

function MakePoint(const pt: TFixedPoint): TPoint;
begin
  Result := Point(FixedRound(pt.X),FixedRound(pt.Y));
end;
//------------------------------------------------------------------------------

function MakePoint(const pt: TFloatPoint): TPoint;
begin
  Result := Point(round(pt.X),round(pt.Y));
end;
//------------------------------------------------------------------------------

function FloatPoints(const pts: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;
var
  i: Integer;
begin
  SetLength(Result, Length(pts));
  for i := 0 to high(pts) do Result[i] := FloatPoint(pts[i]);
end;
//------------------------------------------------------------------------------

function FixedPoints(const pts: TArrayOfFloatPoint): TArrayOfFixedPoint; overload;
var
  i: Integer;
begin
  SetLength(Result, Length(pts));
  for i := 0 to high(pts) do Result[i] := FixedPoint(pts[i]);
end;
//------------------------------------------------------------------------------

function FloatPoints(const ppts: TArrayOfArrayOfFixedPoint): TArrayOfArrayOfFloatPoint;
var
  i,j: Integer;
begin
  SetLength(Result, Length(ppts));
  for i := 0 to high(ppts) do
  begin
    SetLength(Result[i], Length(ppts[i]));
    for j := 0 to high(ppts[i]) do
      Result[i][j] := FloatPoint(ppts[i][j]);
  end;
end;
//------------------------------------------------------------------------------

function FixedPoints(const ppts: TArrayOfArrayOfFloatPoint): TArrayOfArrayOfFixedPoint;
var
  i,j: Integer;
begin
  SetLength(Result, Length(ppts));
  for i := 0 to high(ppts) do
  begin
    SetLength(Result[i], Length(ppts[i]));
    for j := 0 to high(ppts[i]) do
      Result[i][j] := FixedPoint(ppts[i][j]);
  end;
end;
//------------------------------------------------------------------------------

function CopyPoints(const pts: TArrayOfFixedPoint): TArrayOfFixedPoint;
begin
  Result := copy(pts,0,Length(pts));
end;
//------------------------------------------------------------------------------

function GetBoundsRect(pts: array of TFixedPoint): TRect;
begin
  Result := MakeRect(GetBoundsFixedRect(pts), rrOutside);
end;
//------------------------------------------------------------------------------

function GetBoundsFixedRect(pts: array of TFixedPoint): TFixedRect;
var
  i: Integer;
begin
  if Length(pts) = 0 then
  begin
    Result := FixedRect(0,0,0,0);
    exit;
  end;
  with pts[0], Result do
  begin
    left := X ; top := Y; right := X; bottom := Y;
  end;
  for i := 1 to high(pts) do
    with pts[i], Result do
    begin
      if X < left then left := X else if X > right then right := X;
      if Y < top then top := Y else if Y > bottom then bottom := Y;
    end;
end;
//------------------------------------------------------------------------------

function GetBoundsFixedRect(polyPts: TArrayOfArrayOfFixedPoint): TFixedRect;
var
  i,j: Integer;
  firstPointPending: Boolean;
begin
  firstPointPending := true;
  for i := 0 to high(polyPts) do
    for j := 0 to high(polyPts[i]) do
      with polyPts[i][j], Result do
        if firstPointPending then
        begin
          left := X ; top := Y; right := X; bottom := Y;
          firstPointPending := False;
        end else
        begin
          if X < left then left := X else if X > right then right := X;
          if Y < top then top := Y else if Y > bottom then bottom := Y;
        end;
  if firstPointPending then Result := FixedRect(0,0,0,0);
end;
//------------------------------------------------------------------------------

function GetBoundsFloatRect(pts: array of TFixedPoint): TFloatRect;
begin
  Result := FloatRect(GetBoundsFixedRect(pts));
end;
//------------------------------------------------------------------------------

function GetBoundsFloatRect(pts: array of TFloatPoint): TFloatRect;
var
  i: Integer;
begin
  if Length(pts) = 0 then
  begin
    Result := FloatRect(0,0,0,0);
    exit;
  end;
  with pts[0], Result do
  begin
    left := X ; top := Y; right := X; bottom := Y;
  end;
  for i := 1 to high(pts) do
    with pts[i], Result do
    begin
      if X < left then left := X else if X > right then right := X;
      if Y < top then top := Y else if Y > bottom then bottom := Y;
    end;
end;
//------------------------------------------------------------------------------

function GetRectUnion(const rec1, rec2: TFixedRect): TFixedRect;
begin
  Result := rec1;
  if rec2.Left < Result.Left then Result.Left := rec2.Left;
  if rec2.Right > Result.Right then Result.Right := rec2.Right;
  if rec2.Top < Result.Top then Result.Top := rec2.Top;
  if rec2.Bottom > Result.Bottom then Result.Bottom := rec2.Bottom;
end;
//------------------------------------------------------------------------------

type
  TSortRec = record
    pt: TFloatPoint;
    cotangent_value: Single;
  end;

  PSortRecList = ^TSortRecList;
  TSortRecList = array [0 .. MaxListSize -1] of TSortRec;
  TArrayOfSortRec = array of TSortRec;

function GetConvexHull(const polygon: TArrayOfFixedPoint): TArrayOfFixedPoint;
var
  i, m, Len: Integer;
  sortArray: TArrayOfSortRec;

  procedure swap(var pt1, pt2: TFloatPoint);
  var
    Tmp: TFloatPoint;
  begin
    Tmp := pt1; pt1 := pt2; pt2 := Tmp;
  end;

  function IsClockwise(const pt1, pt2, pt3: TFloatPoint): Boolean;
  begin
    Result := (pt2.x-pt1.x)*(pt3.y-pt1.y) - (pt2.y-pt1.y)*(pt3.x-pt1.x) <= 0;
  end;

  procedure QSort(list: PSortRecList; l, r: Integer);
  var
    i,j: Integer;
    P, T: TSortRec;
  begin
    repeat
      i := l;
      j := r;
      P := list^[(l + r) shr 1];
      repeat
        while list^[i].cotangent_value > P.cotangent_value do Inc(i);
        while list^[j].cotangent_value < P.cotangent_value do Dec(j);
        if i <= j then
        begin
          T := list^[i];
          list^[i] := list^[j];
          list^[j] := T;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if l < j then QSort(list, l, j);
      l := i;
    until i >= r;
  end;

begin
  //see Wikipedia article on Graham's Convex Hull algorithm ...
  //http://en.wikipedia.org/wiki/Graham_scan
  Len := Length(polygon);
  if Len < 4 then
  begin
    Result := copyPoints(polygon);
    exit;
  end;
  //find the lowest point (and left-most point if equal lowest) ...
  m := 0;
  for i := 1 to Len -1 do
    if polygon[i].Y < polygon[m].Y then continue
    else if (polygon[i].Y > polygon[m].Y) or
      (polygon[i].X < polygon[m].X)  then m := i;
  //sort all the points relative to this lowest point so that the points arc
  //from the rightmost point to leftmost point relative to this bottom point ...
  SetLength(sortArray,Len+1);
  sortArray[1].pt := FloatPoint(polygon[m]);
  inc(m);
  for i := 2 to Len do
  begin
    if i = m then
      sortArray[i].pt := FloatPoint(polygon[0]) else
      sortArray[i].pt := FloatPoint(polygon[i-1]);
    if abs(sortArray[i].pt.Y - sortArray[1].pt.Y) < 0.0001 then
      sortArray[i].cotangent_value := -MaxSingle else
      sortArray[i].cotangent_value := (sortArray[i].pt.X - sortArray[1].pt.X)/
        (sortArray[i].pt.Y - sortArray[1].pt.Y);
  end;
  QSort(PSortRecList(@sortArray[0]),2,Len);
  sortArray[0].pt := sortArray[Len].pt;
  m := 2;
  for i := 3 to Len do
  begin
    while IsClockwise(sortArray[m-1].pt,sortArray[m].pt,sortArray[i].pt) do dec(m);
    inc(m);
    swap(sortArray[m].pt,sortArray[i].pt);
  end;
  SetLength(Result,m);
  for i := 1 to m do
    Result[i-1] := FixedPoint(sortArray[i].pt);
end;
//------------------------------------------------------------------------------

function CreateMaskFromPolygon(bitmap: TBitmap32; const polygon: TArrayOfFixedPoint): TBitmap32;
var
  ppts: TArrayOfArrayOfFixedPoint;
begin
  SetLength(ppts,1);
  ppts[0] := polygon;
  Result := CreateMaskFromPolygon(bitmap, ppts);
end;
//------------------------------------------------------------------------------

function CreateMaskFromPolygon(bitmap: TBitmap32;
  const polygons: TArrayOfArrayOfFixedPoint): TBitmap32;
var
  i, highI: Integer;
begin
  Result := TBitmap32.create;
  with bitmap do Result.SetSize(width,height);
  highI := high(polygons);
  if highI < 0 then exit;
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := pfAlternate;
    AddPoints(polygons[0][0], Length(polygons[0]));
    for i := 1 to highI do
    begin
      NewLine;
      AddPoints(polygons[i][0], Length(polygons[i]));
    end;
    Draw(Result, clBlack32, clWhite32);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure ApplyMaskToAlpha(bitmap, mask: TBitmap32; invertMask: Boolean = False);
var
  i: Integer;
  bitmapColor, maskColor: PColor32Entry;
begin
  //nb: this only works when bitmap.DrawMode = Blend ...
  if (bitmap.DrawMode = dmOpaque) or
    (bitmap.Width <> mask.Width) or (bitmap.Height <> mask.Height) then exit;

  bitmapColor := @bitmap.Bits[0];
  maskColor := @mask.Bits[0];
  if invertMask then
    for i := 1 to bitmap.Width * bitmap.Height do
    begin
      //ie masked area (white) becomes transparent, background is opaque
      bitmapColor.A := (bitmapColor.A * (255-maskColor.A)) shr 8;
      inc(bitmapColor);
      inc(maskColor);
    end
  else
    for i := 1 to bitmap.Width * bitmap.Height do
    begin
      //ie masked area (white) becomes opaque, background is transparent
      bitmapColor.A := (bitmapColor.A * maskColor.A) shr 8;
      inc(bitmapColor);
      inc(maskColor);
    end;
end;
//------------------------------------------------------------------------------

procedure ApplyMask(modifiedBmp, originalBmp, maskBmp: TBitmap32; invertMask: Boolean = False);
var
  i: Integer;
  origClr, modClr, mskClr: PColor32Entry;
begin
  if not assigned(originalBmp) or not assigned(maskBmp) or
    (originalBmp.Width <> modifiedBmp.Width) or
    (originalBmp.Height <> modifiedBmp.Height) or
    (originalBmp.Height <> maskBmp.Height) or
    (originalBmp.Height <> maskBmp.Height) then exit;

  origClr := @originalBmp.Bits[0];
  modClr := @modifiedBmp.Bits[0];
  mskClr := @maskBmp.Bits[0];
  for i := 1 to originalBmp.Width * originalBmp.Height do
  begin
    //black pixel in mask -> replace modified color with original color
    //white pixel in mask -> keep modified color
    if invertMask then
      MergeMemEx(origClr.ARGB, modClr.ARGB, 255- mskClr.B) else
      MergeMemEx(origClr.ARGB, modClr.ARGB, mskClr.B);
    inc(origClr);
    inc(modClr);
    inc(mskClr);
  end;
  EMMS;
end;
//------------------------------------------------------------------------------

procedure InvertMask(maskBmp: TBitmap32);
var
  i: Integer;
  mskClr: PColor32Entry;
begin
  if not assigned(maskBmp) then exit;
  mskClr := @maskBmp.Bits[0];
  for i := 1 to maskBmp.Width * maskBmp.Height do
  begin
    mskClr.B := 255- mskClr.B;
    inc(mskClr);
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfColor32(colors: array of TColor32): TArrayOfColor32;
var
  i,Len: Integer;
begin
  Len := Length(colors);
  SetLength(Result, Len);
  for i := 0 to Len -1 do Result[i] := colors[i];
end;
//------------------------------------------------------------------------------

function SmoothChart(const chartPts: array of TFixedPoint; xStep: Integer): TArrayOfFixedPoint;
var
  i, ptCnt: Integer;
  dx{, angle1, angle2}: Single;
begin
  Result := nil;
  ptCnt := Length(chartPts);
  if ptCnt < 2 then exit;

  dx := abs(xStep)/2;

  SetLength(Result, ptCnt + (ptCnt - 1)*2);
  Result[0] := chartPts[0];
  with FloatPoint(chartPts[0]) do Result[1] := FixedPoint(X +dx, Y);
  for i := 1 to high(chartPts) -1 do
//    if ((chartPts[i-1].Y <= chartPts[i].Y) and (chartPts[i+1].Y <= chartPts[i].Y)) or
//      ((chartPts[i-1].Y >= chartPts[i].Y) and (chartPts[i+1].Y >= chartPts[i].Y)) then
    begin
      with FloatPoint(chartPts[i]) do Result[i*3-1] := FixedPoint(X -dx, Y);
      Result[i*3] := chartPts[i];
      with FloatPoint(chartPts[i]) do Result[i*3+1] := FixedPoint(X +dx, Y);
//    end else
//    begin
//      //not a peak or trough so get the average slope ...
//      angle1 := GetAngleOfPt2FromPt1(chartPts[i-1],chartPts[i]);
//      angle2 := GetAngleOfPt2FromPt1(chartPts[i],chartPts[i+1]);
//      angle1 := (angle1 + angle2)/2;
//      Result[i*3-1] := GetPointAtAngleFromPoint(chartPts[i], dx, angle1 - pi);
//      Result[i*3] := chartPts[i];
//      Result[i*3+1] := GetPointAtAngleFromPoint(chartPts[i], dx, angle1);
    end;
  with FloatPoint(chartPts[ptCnt -1]) do Result[high(Result)-1] := FixedPoint(X -dx, Y);
  Result[high(Result)] := chartPts[ptCnt -1];
  Result := GetCBezierPoints(Result);
end;
//------------------------------------------------------------------------------

function GetCBezierPoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
var
  i, j, arrayLen, resultCnt: Integer;
  ctrlPts: array [ 0..3] of TFloatPoint;

  procedure RecursiveCBezier(const p1, p2, p3, p4: TFloatPoint);
  var
    p12, p23, p34, p123, p234, p1234: TFloatPoint;
  begin
    //assess flatness of curve ...
    //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
    if abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y) < cbezier_tolerance then
    begin
      if resultCnt = Length(Result) then
        SetLength(Result, Length(Result) +128);
      Result[resultCnt] := FixedPoint(p4);
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) *half;
      p12.Y := (p1.Y + p2.Y) *half;
      p23.X := (p2.X + p3.X) *half;
      p23.Y := (p2.Y + p3.Y) *half;
      p34.X := (p3.X + p4.X) *half;
      p34.Y := (p3.Y + p4.Y) *half;
      p123.X := (p12.X + p23.X) *half;
      p123.Y := (p12.Y + p23.Y) *half;
      p234.X := (p23.X + p34.X) *half;
      p234.Y := (p23.Y + p34.Y) *half;
      p1234.X := (p123.X + p234.X) *half;
      p1234.Y := (p123.Y + p234.Y) *half;
      RecursiveCBezier(p1, p12, p123, p1234);
      RecursiveCBezier(p1234, p234, p34, p4);
    end;
  end;

begin
  //first check that the 'control_points' count is valid ...
  arrayLen := Length(control_points);
  if (arrayLen < 4) or ((arrayLen -1) mod 3 <> 0) then exit;

  SetLength(Result, 128);
  Result[0] := control_points[0];
  resultCnt := 1;
  for i := 0 to (arrayLen div 3)-1 do
  begin
    for j := 0 to 3 do
      ctrlPts[j] := FloatPoint(control_points[i*3 +j]);
    RecursiveCBezier(ctrlPts[0], ctrlPts[1], ctrlPts[2], ctrlPts[3]);
  end;
  SetLength(Result,resultCnt);
end;
//------------------------------------------------------------------------------

function GetCSplinePoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
var
  i, arrayLen, resultCnt, resultSize, arrayDiv2Min2: Integer;
  pts1, pts2, pts3, pts4: TFloatPoint;

  procedure RecursiveCBezier(const p1, p2, p3, p4: TFloatPoint);
  var
    p12, p23, p34, p123, p234, p1234: TFloatPoint;
  begin
    //assess flatness of curve ...
    //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
    if abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y) < cbezier_tolerance then
    begin
      if resultCnt = Length(Result) then
        SetLength(Result, Length(Result) +128);
      Result[resultCnt] := FixedPoint(p4);
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) *half;
      p12.Y := (p1.Y + p2.Y) *half;
      p23.X := (p2.X + p3.X) *half;
      p23.Y := (p2.Y + p3.Y) *half;
      p34.X := (p3.X + p4.X) *half;
      p34.Y := (p3.Y + p4.Y) *half;
      p123.X := (p12.X + p23.X) *half;
      p123.Y := (p12.Y + p23.Y) *half;
      p234.X := (p23.X + p34.X) *half;
      p234.Y := (p23.Y + p34.Y) *half;
      p1234.X := (p123.X + p234.X) *half;
      p1234.Y := (p123.Y + p234.Y) *half;
      RecursiveCBezier(p1, p12, p123, p1234);
      RecursiveCBezier(p1234, p234, p34, p4);
    end;
  end;

begin
  //CSplines are series of 'smooth' CBeziers where the shared point of each
  //adjacent bezier is calculated from the midpoint of adjoining control points.
  //(c.f. SVG's 'S' command - http://www.w3.org/TR/SVG11/paths.html ).

  //first check that the 'control_points' count is valid ...
  arrayLen := Length(control_points);
  if (arrayLen < 4) or odd(arrayLen) then exit;
  arrayDiv2Min2 := arrayLen div 2 -2;
  resultSize := 128;
  SetLength(Result, resultSize);
  Result[0] := control_points[0];
  resultCnt := 1;

  pts1 := FloatPoint(control_points[0]);
  for i := 0 to arrayDiv2Min2 do
  begin
    pts2 := FloatPoint(control_points[i*2+1]);
    pts3 := FloatPoint(control_points[i*2+2]);
    if i < arrayDiv2Min2 then //if not last bezier then ...
      pts4 := FloatPoint(MidPoint(control_points[i*2+2], control_points[i*2+3]))
    else
      pts4 := FloatPoint(control_points[i*2+3]);
    RecursiveCBezier(pts1, pts2, pts3, pts4);

    //prepare for the next segment ...
    pts1 := pts4;
  end;
  SetLength(Result, resultCnt);
end;
//------------------------------------------------------------------------------

function GetQBezierPoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
var
  i, j, arrayLen, resultCnt, resultSize: Integer;
  pts: array [0..2] of TFloatPoint;

  procedure RecursiveQBezier(const p1, p2, p3: TFloatPoint);
  var
    p12, p23, p123: TFloatPoint;
  begin
    //assess flatness of curve ...
    if abs(p1.x + p3.x - 2*p2.x) + abs(p1.y + p3.y - 2*p2.y) < qbezier_tolerance then
    begin
      if resultCnt = Length(Result) then SetLength(Result, Length(Result) +128);
      Result[resultCnt] := FixedPoint(p3);
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) *half;
      p12.Y := (p1.Y + p2.Y) *half;
      p23.X := (p2.X + p3.X) *half;
      p23.Y := (p2.Y + p3.Y) *half;
      p123.X := (p12.X + p23.X) *half;
      p123.Y := (p12.Y + p23.Y) *half;
      RecursiveQBezier(p1, p12, p123);
      RecursiveQBezier(p123, p23, p3);
    end;
  end;

begin
  //first check that the 'control_points' count is valid ...
  arrayLen := Length(control_points);
  if (arrayLen < 3) or ((arrayLen -1) mod 2 <> 0) then exit;

  resultSize := 128;
  SetLength(Result, resultSize);
  Result[0] := control_points[0];
  resultCnt := 1;
  for j := 0 to (arrayLen div 2)-1 do
  begin
    for i := 0 to 2 do
    begin
      pts[i].X := control_points[i+ j*2].X*FixedToFloat;
      pts[i].Y := control_points[i+ j*2].Y*FixedToFloat;
    end;
    RecursiveQBezier(pts[0], pts[1], pts[2]);
  end;
  SetLength(Result,resultCnt);
end;
//------------------------------------------------------------------------------

function GetQSplinePoints(const control_points: array of TFixedPoint): TArrayOfFixedPoint;
var
  i, arrayLen, resultCnt, resultSize, arrayLenMin3: Integer;
  pts1, pts2, pts3: TFloatPoint;

  procedure RecursiveQSpline(const p1, p2, p3: TFloatPoint);
  var
    p12, p23, p123: TFloatPoint;
  begin
    //assess flatness of curve ...
    if abs(p1.x + p3.x - 2*p2.x) + abs(p1.y + p3.y - 2*p2.y) < qbezier_tolerance then
    begin
      if resultCnt = Length(Result) then SetLength(Result, Length(Result) +128);
      Result[resultCnt] := FixedPoint(p3);
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) *half;
      p12.Y := (p1.Y + p2.Y) *half;
      p23.X := (p2.X + p3.X) *half;
      p23.Y := (p2.Y + p3.Y) *half;
      p123.X := (p12.X + p23.X) *half;
      p123.Y := (p12.Y + p23.Y) *half;
      RecursiveQSpline(p1, p12, p123);
      RecursiveQSpline(p123, p23, p3);
    end;
  end;

begin
  //QSplines are series of 'smooth' QBeziers where each shared point is also
  //the midpoint of the control points of the adjoining QBeziers. This function
  //is typically used together with the Windows' GetGlyphOutline() API function.
  //(c.f. SVG's 'T' command - http://www.w3.org/TR/SVG11/paths.html )

  //first check that the 'control_points' count is valid ...
  arrayLen := Length(control_points);
  if (arrayLen < 3) then exit;
  arrayLenMin3 := arrayLen -3;
  resultSize := 128;
  SetLength(Result, resultSize);
  Result[0] := control_points[0];
  resultCnt := 1;

  pts1 := FloatPoint(control_points[0]);
  for i := 0 to arrayLenMin3 do
  begin
    pts2 := FloatPoint(control_points[i+1]);
    if i < arrayLenMin3 then //if not last bezier then ...
      pts3 := FloatPoint(MidPoint(control_points[i+1], control_points[i+2]))
    else
      pts3 := FloatPoint(control_points[i+2]);
    RecursiveQSpline(pts1, pts2, pts3);

    //prepare for the next segment ...
    pts1 := pts3;
  end;
  SetLength(Result, resultCnt);
end;
//------------------------------------------------------------------------------

function GetEllipsePoints(const ellipseRect: TFloatRect): TArrayOfFixedPoint;
const
  //Magic constant = 2/3*(1-cos(90deg/2))/sin(90deg/2) = 2/3*(sqrt(2)-1) = 0.27614
  offset: Single = 0.276142375;
var
  midx, midy, offx, offy: Single;
  pts: array [0..12] of TFixedPoint;
begin
  with ellipseRect do
  begin
    if (abs(Left - Right) <= 0.5) and (abs(Top - Bottom) <= 0.5) then
    begin
      SetLength(Result,1);
      Result[0] := FixedPoint(Left,Top);
      exit;
    end;

    midx := (right + left)/2;
    midy := (bottom + top)/2;
    offx := (right - left) * offset;
    offy := (bottom - top) * offset;
    //draws an ellipse starting at angle 0 and moving anti-clockwise ...
    pts[0]  := FixedPoint(right, midy);
    pts[1]  := FixedPoint(right, midy - offy);
    pts[2]  := FixedPoint(midx + offx, top);
    pts[3]  := FixedPoint(midx, top);
    pts[4]  := FixedPoint(midx - offx, top);
    pts[5]  := FixedPoint(left, midy - offy);
    pts[6]  := FixedPoint(left, midy);
    pts[7]  := FixedPoint(left, midy + offy);
    pts[8]  := FixedPoint(midx - offx, bottom);
    pts[9]  := FixedPoint(midx, bottom);
    pts[10] := FixedPoint(midx + offx, bottom);
    pts[11] := FixedPoint(right, midy + offy);
    pts[12] := pts[0];
  end;
  Result := GetCBezierPoints(pts);
end;
//------------------------------------------------------------------------------

function GetPtOnEllipseFromAngleEccentric(const ellipseRect: TFloatRect;
  eccentric_angle_radians: Single): TFloatPoint;
var
  SinAng, CosAng: Single;
  Center: TFloatPoint;
begin
  with ellipseRect do
  begin
    Center.X := (right + left) * 0.5;
    Center.Y := (bottom + top) * 0.5;
    GR32_Math.SinCos(eccentric_angle_radians, SinAng, CosAng);
    Result.X := Center.X + (right-left)/2*CosAng;
    //negative offset since Y is positive downwards ...
    Result.Y := Center.Y - (bottom-top)/2*SinAng;
  end;
end;
//------------------------------------------------------------------------------

function GetPointsAroundEllipse(const ellipseRect:
  TFloatRect; PointCount: Integer): TArrayOfFixedPoint;
var
  i: Integer;
  a: Single;
begin
  a := 0;
  SetLength(Result, PointCount);
  for i := 0 to PointCount-1 do
  begin
    Result[i] := FixedPoint(GetPtOnEllipseFromAngleEccentric(ellipseRect,a));
    a := a + rad360/PointCount;
  end;
end;
//------------------------------------------------------------------------------

function AngleToEccentricAngle(const ellipseRect: TFloatRect;
  Radians: Single): Single;
var
  a,b: Single;
  quadrant: Integer;
begin
  quadrant := trunc(Radians/rad90) mod 4;
  if Radians < 0 then inc(quadrant,4) else inc(quadrant);

  //given a point (x,y) on an ellipse with x Radius = a; y Radius = b
  //x = a * cos? y = b * sin? y/x = tan?
  //a = x / cos? b = y / sin?
  //if a / b = r then a = r * b and x / cos?= r * y / sin?
  //x * sin?/ cos?= r * y
  //x * tan?= r * y
  //tan?= r * y/x
  //?= arctan(a/b * y/x);
  //?= arctan(a/b * tan?;
  with ellipseRect do
  begin
    a := (right-left)/2;
    b := (bottom-top)/2;
  end;
  Result := 0;
  if (a = 0) or (b = 0) then exit;
  Result := abs(arctan(tan(Radians) * a/b));
  case quadrant of
    2 : Result := rad180 - Result;
    3 : Result := rad180 + Result;
    4 : Result := rad360 - Result;
  end;
end;
//------------------------------------------------------------------------------

function GetArcPoints(const ellipseRect: TFloatRect;
  startPt, endPt: TFloatPoint): TArrayOfFixedPoint;
var
  c: TFloatPoint;
  start_rad, end_rad: Single;
begin
  with ellipseRect do
  begin
    c.X := (right+left)/2;
    c.Y := (bottom+top)/2;
  end;
  start_rad := AngleToEccentricAngle(ellipseRect,GetAngleOfPt2FromPt1(c,startPt));
  end_rad := AngleToEccentricAngle(ellipseRect,GetAngleOfPt2FromPt1(c,endPt));
  Result := GetArcPointsEccentric(ellipseRect,start_rad,end_rad);
end;
//------------------------------------------------------------------------------

function GetArcPoints(const ellipseRect: TFloatRect;
  start_degrees, end_degrees: Single): TArrayOfFixedPoint;
var
  start_radians, end_radians: Single;
begin
  start_radians := AngleToEccentricAngle(ellipseRect,start_degrees*DegToRad);
  end_radians := AngleToEccentricAngle(ellipseRect,end_degrees*DegToRad);
  Result := GetArcPointsEccentric(ellipseRect,start_radians, end_radians);
end;
//------------------------------------------------------------------------------

function GetArcPointsEccentric(const ellipseRect: TFloatRect;
  start_eccentric, end_eccentric: Single): TArrayOfFixedPoint;
var
  w, h, offset, angleDiff: Single;
  tmpLen, resultLen: Integer;
  pts: array [0..3] of TFixedPoint;
  Tmp: TArrayOfFixedPoint;
const
  //offset90 = 2/3*(1-cos(90deg/2))/sin(90deg/2) = 2/3*(sqrt(2)-1) = 0.276142375
  offset90 = 0.276142375;
begin
  if start_eccentric = end_eccentric then
    start_eccentric := start_eccentric - rad360;
  angleDiff := end_eccentric - start_eccentric;
  if angleDiff < 0 then angleDiff := angleDiff + rad360;
  with ellipseRect do
  begin
    w := (right - left);
    h := (bottom - top);
  end;

  resultLen := 0;
  while angleDiff > rad90 do
  begin
    pts[0]  := FixedPoint(GetPtOnEllipseFromAngleEccentric(ellipseRect,start_eccentric));
    pts[3]  := FixedPoint(GetPtOnEllipseFromAngleEccentric(ellipseRect,start_eccentric+rad90));
    pts[1].X  := Fixed((pts[0].X*fixedToFloat) - (sin(start_eccentric)*offset90*w));
    pts[1].Y  := Fixed((pts[0].Y*fixedToFloat) - (cos(start_eccentric)*offset90*h));
    pts[2].X  := Fixed((pts[3].X*fixedToFloat) + (sin(start_eccentric+rad90)*offset90*w));
    pts[2].Y  := Fixed((pts[3].Y*fixedToFloat) + (cos(start_eccentric+rad90)*offset90*h));
    Tmp := GetCBezierPoints(pts);
    tmpLen := Length(Tmp);
    SetLength(Result, resultLen+tmpLen);
    move(Tmp[0], Result[resultLen], tmpLen * sizeof(TFixedPoint));
    inc(resultLen, tmpLen);
    start_eccentric := start_eccentric + rad90;
    angleDiff := angleDiff - rad90;
  end;
  offset := 2/3*(1-cos(angleDiff/2))/sin(angleDiff/2);
  pts[0]  := FixedPoint(GetPtOnEllipseFromAngleEccentric(ellipseRect,start_eccentric));
  pts[3]  := FixedPoint(GetPtOnEllipseFromAngleEccentric(ellipseRect,end_eccentric));
  pts[1].X  := Fixed((pts[0].X*fixedToFloat) - (sin(start_eccentric)*offset*w));
  pts[1].Y  := Fixed((pts[0].Y*fixedToFloat) - (cos(start_eccentric)*offset*h));
  pts[2].X  := Fixed((pts[3].X*fixedToFloat) + (sin(end_eccentric)*offset*w));
  pts[2].Y  := Fixed((pts[3].Y*fixedToFloat) + (cos(end_eccentric)*offset*h));
  Tmp := GetCBezierPoints(pts);
  tmpLen := Length(Tmp);
  SetLength(Result, resultLen+tmpLen);
  move(Tmp[0], Result[resultLen], tmpLen * sizeof(TFixedPoint));
end;
//------------------------------------------------------------------------------

function GetPiePoints(const ellipseRect: TFloatRect;
  startPt, endPt: TFloatPoint): TArrayOfFixedPoint;
var
  Len: Integer;
begin
  Result := GetArcPoints(ellipseRect,startPt,endPt);
  Len := Length(Result);
  SetLength(Result, Len+2);
  with ellipseRect do
    Result[Len] := FixedPoint((left+right)/2,(top+bottom)/2);
  Result[Len+1] := Result[0];
end;
//------------------------------------------------------------------------------

function GetPiePoints(const ellipseRect: TFloatRect;
  start_degrees, end_degrees: Single): TArrayOfFixedPoint;
var
  start_radians, end_radians: Single;
begin
  start_radians := AngleToEccentricAngle(ellipseRect,start_degrees*DegToRad);
  end_radians := AngleToEccentricAngle(ellipseRect,end_degrees*DegToRad);
  Result := GetPiePointsEccentric(ellipseRect,start_radians, end_radians);
end;
//------------------------------------------------------------------------------

function GetPiePointsEccentric(const ellipseRect: TFloatRect;
  start_eccentric, end_eccentric: Single): TArrayOfFixedPoint;
var
  Len: Integer;
begin
  Result := GetArcPointsEccentric(ellipseRect, start_eccentric, end_eccentric);
  Len := Length(Result);
  SetLength(Result, Len+2);
  with ellipseRect do
    Result[Len] := FixedPoint((left+right)/2,(top+bottom)/2);
  Result[Len+1] := Result[0];
end;
//------------------------------------------------------------------------------

function GetRoundedRectanglePoints(const rect: TFloatRect;
  roundingPercent: cardinal): TArrayOfFixedPoint;
var
  roundingFrac: Single;
  i,j,k,arcLen: Integer;
  dx,dy: Single;
  arcs: array [0 .. 3] of TArrayOfFixedPoint;
begin
  //nb: it's simpler to construct the rounded rect in an anti-clockwise
  //direction because that's the direction in which the arc points are returned.

  if roundingPercent < 5 then roundingPercent := 5
  else if roundingPercent > 90 then roundingPercent := 90;
  roundingFrac := roundingPercent/200; //ie rounds up 90deg of half the line
  with rect do
  begin
    dx := (Right-Left)*roundingFrac;
    dy := (Bottom-Top)*roundingFrac;
    if (dx < dy) then
    begin
      if dy < (Right-Left)*0.45 then dx := dy
      else dx := (Right-Left)*0.45;
    end;
    if (dy < dx) then
    begin
      if dx < (Bottom-Top)*0.45 then dy := dx
      else dy := (Bottom-Top)*0.45;
    end;
    arcs[0] := GetArcPointsEccentric(
      FloatRect(Left, Bottom -dy*2, Left+dx*2, Bottom), rad180, rad270);
    arcs[1] := GetArcPointsEccentric(
      FloatRect(Right-dx*2, Bottom -dy*2, Right, Bottom), rad270, 0);
    arcs[2] := GetArcPointsEccentric(
      FloatRect(Right - dx*2, Top, Right, Top + dy*2), 0, rad90);
    arcs[3] := GetArcPointsEccentric(
      FloatRect(Left, top, Left+dx*2, Top+dy*2), rad90, rad180);
  end;

  //calculate the final number of points to return
  j := 0;
  for i := 0 to 3 do inc(j, Length(arcs[i]));
  SetLength(Result, j);

  j := 0;
  for i := 0 to 3 do
  begin
    arcLen := Length(arcs[i]);
    for k := 0 to arcLen -1 do Result[j+k] := arcs[i][k];
    inc(j,arcLen);
  end;
end;
//------------------------------------------------------------------------------

function GetBalloonedEllipsePoints(const ellipseRect: TFloatRect;
  balloonPos: TBalloonPos): TArrayOfFixedPoint;
const
  DiamondSize = 15;
var
  Len: Integer;
begin
  case balloonPos of
    bpNone:
      Result := GetEllipsePoints(ellipseRect);
    bpTopLeft:
      with ellipseRect do
      begin
        Result := GetArcPointsEccentric(
          FloatRect(left,top,right,bottom),
          (135 + DiamondSize)*DegToRad, (135-diamondSize)*DegToRad);
        Len := Length(Result);
        SetLength(Result, Len+1);
        Result[Len] := FixedPoint(left,top);
      end;
    bpTopRight:
      with ellipseRect do
      begin
        Result := GetArcPointsEccentric(
          FloatRect(left,top,right,bottom),
          (45+diamondSize)*DegToRad, (45-diamondSize)*DegToRad);
        Len := Length(Result);
        SetLength(Result, Len+1);
        Result[Len] := FixedPoint(right,top);
      end;
    bpBottomRight:
      with ellipseRect do
      begin
        Result := GetArcPointsEccentric(
          FloatRect(left,top,right,bottom),
          (315+diamondSize)*DegToRad, (315-diamondSize)*DegToRad);
        Len := Length(Result);
        SetLength(Result, Len+1);
        Result[Len] := FixedPoint(right,bottom);
      end;
    bpBottomLeft:
      with ellipseRect do
      begin
        Result := GetArcPointsEccentric(
          FloatRect(left,top,right,bottom),
          (225 + DiamondSize) * DegToRad, (225-diamondSize)*DegToRad);
        Len := Length(Result);
        SetLength(Result, Len+1);
        Result[Len] := FixedPoint(left,bottom);
      end;

  end;
end;
//------------------------------------------------------------------------------

function GetPipeCornerPoints(rec: TFloatRect;
  pipeWidth: Single; corner: TCorner): TArrayOfFixedPoint;
var
  Len: Integer;
  Inner: TArrayOfFixedPoint;
  OuterRect, InnerRect: TFloatRect;
  Radius, Delta: Single;
begin
  Result := nil;
  with rec do
  begin
    Radius := min(Right - Left, Bottom - Top);
    if (Radius < 5) or (pipeWidth < 1) then exit
    else if pipeWidth > Radius then pipeWidth := Radius;
    Delta := (Radius - pipeWidth)/2;

    case corner of
      cTopLeft:
        begin
          OuterRect := FloatRect(Left+Delta, Top+Delta,
            Left + Delta +(Radius-delta)*2, Top + Delta +(Radius-delta)*2);
          InnerRect := FloatRect(Left+Radius-delta, Top+Radius-delta,
            Left+Radius-delta + Delta*2, Top+Radius-delta + Delta*2);
          Result := GetArcPoints(OuterRect,90,180);
          Inner := GetArcPoints(InnerRect,90,180);
          Inner := ReversePoints(Inner);
          Len := Length(Result);
          Result[0] := FixedPoint(Right, Top+Delta);
          Result[Len-1] := FixedPoint(Left+Delta, Bottom);
          Len := Length(Inner);
          Inner[0] := FixedPoint(Left+Radius-delta, Bottom);
          Inner[Len-1] := FixedPoint(Right, Top+Radius-delta);
        end;
      cTopRight:
        begin
          OuterRect := FloatRect(Right-delta -(Radius-delta)*2, Top+Delta,
            Right - Delta, Top + Delta +(Radius-delta)*2);
          InnerRect := FloatRect(Right-Radius-delta, Top+Radius-delta,
            Right-Radius-delta + Delta*2, Top+Radius-delta + Delta*2);
          Result := GetArcPoints(OuterRect,0,90);
          Inner := GetArcPoints(InnerRect,0,90);
          Inner := ReversePoints(Inner);
          Len := Length(Result);
          Result[0] := FixedPoint(Right-delta, Bottom);
          Result[Len-1] := FixedPoint(Left, Top+Delta);
          Len := Length(Inner);
          Inner[0] := FixedPoint(Left, Top+Radius-delta);
          Inner[Len-1] := FixedPoint(Right-Radius+Delta, Bottom);
        end;
      cBottomLeft:
        begin
          OuterRect := FloatRect(Left+Delta, Bottom-delta-(Radius-delta)*2,
            Left + Delta +(Radius-delta)*2, Bottom-delta);
          InnerRect := FloatRect(Left+Radius-delta, Bottom-Radius-delta,
            Left+Radius-delta + Delta*2, Bottom-Radius-delta + Delta*2);
          Result := GetArcPoints(OuterRect,180,270);
          Inner := GetArcPoints(InnerRect,180,270);
          Inner := ReversePoints(Inner);
          Len := Length(Result);
          Result[0] := FixedPoint(Left+Delta, Top);
          Result[Len-1] := FixedPoint(Right, Bottom-delta);
          Len := Length(Inner);
          Inner[0] := FixedPoint(Right, Bottom-Radius+Delta);
          Inner[Len-1] := FixedPoint(Left+Radius-delta, Top);
        end;
      cBottomRight:
        begin
          OuterRect := FloatRect(Right-delta -(Radius-delta)*2,
            Bottom-delta-(Radius-delta)*2, Right - Delta, Bottom-delta);
          InnerRect := FloatRect(Right-Radius-delta, Bottom-Radius-delta,
            Right-Radius-delta + Delta*2, Bottom-Radius-delta + Delta*2);
          Result := GetArcPoints(OuterRect,270,0);
          Inner := GetArcPoints(InnerRect,270,0);
          Inner := ReversePoints(Inner);
          Len := Length(Result);
          Result[0] := FixedPoint(Left, Bottom-delta);
          Result[Len-1] := FixedPoint(Right-delta, Top);
          Len := Length(Inner);
          Inner[0] := FixedPoint(Right-Radius+Delta, Top);
          Inner[Len-1] := FixedPoint(Left, Bottom-Radius+Delta);
        end;
    end;
    ConcatenatePoints(Result, Inner);
  end;
end;
//------------------------------------------------------------------------------

function GetStarPoints(const Center: TFixedPoint;
  PointCount: Integer; radius1, radius2: Single): TArrayOfFixedPoint;
var
  CosA, SinA, r, AngleDelta: Single;
  i: Integer;
begin
  Result := nil;
  if PointCount < 5 then exit
  else if PointCount > 90 then PointCount := 90;
  SetLength(Result, PointCount*2);
  AngleDelta := rad360 / (PointCount*2);
  for i := 0 to PointCount*2 -1 do
  begin
    GR32_Math.SinCos(AngleDelta *i, SinA, CosA);
    if odd(i) then r := radius1 else r := radius2;
    Result[i].X := Center.X + Fixed( r * CosA);
    Result[i].Y := Center.Y + Fixed(-r * SinA); //nb: Y axis is +ve down
  end;
end;
//------------------------------------------------------------------------------

function GradientColor(color1, color2: TColor32; frac: Single): TColor32;
var
  a1,a2,b1,g1,r1,b2,g2,r2: byte;
begin
  if frac >= 1 then Result := color2
  else if frac <= 0 then Result := color1
  else
  begin
    Color32ToRGBA(color1,r1,g1,b1,a1);
    Color32ToRGBA(color2,r2,g2,b2,a2);

    r1 := trunc(r1*(1-frac) + r2*frac);
    g1 := trunc(g1*(1-frac) + g2*frac);
    b1 := trunc(b1*(1-frac) + b2*frac);
    a1 := trunc(a1*(1-frac) + a2*frac);
    Result := (a1 shl 24) or (r1 shl 16) or (g1 shl 8) or b1;
  end;
end;
//------------------------------------------------------------------------------

function GetColor(const colors: array of TColor32; fraction: Single): TColor32;
var
  i,colorLen: Integer;
  c1,c2: TColor32;
begin
  colorLen := Length(colors);
  if (fraction >= 1) then
    Result := colors[colorLen -1]
  else if (fraction <= 0) then
    Result := colors[0]
  else
  begin
    fraction := fraction * (colorLen-1);
    i := trunc(fraction);
    c1 := colors[i];
    c2 := colors[i+1];
    Result := GradientColor(c1, c2, frac(fraction));
  end;
end;
//------------------------------------------------------------------------------

function IsClockwise(const pts: TArrayOfFixedPoint): Boolean;
var
  i, highI, bottomIdx, iPrior, iNext: Integer;
  N1, N2: TFloatPoint;

  function GetPrior(i: Integer): Integer;
  begin
    if i = 0 then Result := highI else Result := i -1;
  end;

  function GetNext(i: Integer): Integer;
  begin
    if i = highI then Result := 0 else Result := i +1;
  end;

begin
  highI := high(pts);
  Result := highI > 1;
  if not Result then exit;
  bottomIdx := 0;
  for i := 1 to highI do
    if pts[i].Y < pts[bottomIdx].Y then continue
    else if pts[i].Y > pts[bottomIdx].Y then bottomIdx := i
    else if pts[i].X > pts[bottomIdx].X then bottomIdx := i;
  //bottomIdx now references the bottom right-most point in the polygon
  iPrior := GetPrior(bottomIdx);
  while (iPrior <> bottomIdx) and IsDuplicatePoint(pts[bottomIdx], pts[iPrior]) do
    iPrior := GetPrior(iPrior);
  iNext := GetNext(bottomIdx);
  while (iNext <> bottomIdx) and IsDuplicatePoint(pts[bottomIdx], pts[iNext]) do
    iNext := GetNext(iNext);
  if iNext <> iPrior then
  begin
    N1 := GetUnitNormal(pts[iPrior], pts[bottomIdx]);
    N2 := GetUnitNormal(pts[bottomIdx], pts[iNext]);
    //(N1.X * N2.Y - N2.X * N1.Y) == unit normal "cross product" == sin(angle)
    Result := (N1.X * N2.Y - N2.X * N1.Y) > 0; //ie angle > 180deg.
  end else
    Result := False;
end;
//------------------------------------------------------------------------------

function IsClockwise(const pts: TArrayOfFloatPoint): Boolean;
var
  i, highI, bottomIdx, iPrior, iNext: Integer;
  N1, N2: TFloatPoint;

  function GetPrior(i: Integer): Integer;
  begin
    if i = 0 then Result := highI else Result := i -1;
  end;

  function GetNext(i: Integer): Integer;
  begin
    if i = highI then Result := 0 else Result := i +1;
  end;

begin
  highI := high(pts);
  Result := highI > 1;
  if not Result then exit;
  bottomIdx := 0;
  for i := 1 to highI do
    if pts[i].Y < pts[bottomIdx].Y then continue
    else if pts[i].Y > pts[bottomIdx].Y then bottomIdx := i
    else if pts[i].X > pts[bottomIdx].X then bottomIdx := i;
  //bottomIdx now references the bottom right-most point in the polygon
  iPrior := GetPrior(bottomIdx);
  while (iPrior <> bottomIdx) and IsDuplicatePoint(pts[bottomIdx], pts[iPrior]) do
    iPrior := GetPrior(iPrior);
  iNext := GetNext(bottomIdx);
  while (iNext <> bottomIdx) and IsDuplicatePoint(pts[bottomIdx], pts[iNext]) do
    iNext := GetNext(iNext);
  if iNext <> iPrior then
  begin
    N1 := GetUnitNormal(pts[iPrior], pts[bottomIdx]);
    N2 := GetUnitNormal(pts[bottomIdx], pts[iNext]);
    //(N1.X * N2.Y - N2.X * N1.Y) == unit normal "cross product" == sin(angle)
    Result := (N1.X * N2.Y - N2.X * N1.Y) > 0; //ie angle > 180deg.
  end else
    Result := False;
end;
//------------------------------------------------------------------------------

function BuildDashedLine(const Points: TArrayOfFixedPoint;
  const DashArray: array of TFloat; DashOffset: TFloat = 0): TArrayOfArrayOfFixedPoint;
var
  I, J, DashIndex: Integer;
  Offset, d, v: TFloat;
  dx, dy: TFixed;

  procedure AddPoint(X, Y: TFixed);
  var
    K: Integer;
  begin
    K := Length(Result[J]);
    SetLength(Result[J], K + 1);
    Result[J][K].X := X;
    Result[J][K].Y := Y;
  end;

begin
  DashIndex := 0;
  Offset := 0;
  DashOffset := DashArray[0] - DashOffset;

  v := 0;
  for I := 0 to High(DashArray) do v := v + DashArray[I];
  while DashOffset < 0 do DashOffset := DashOffset + v;
  while DashOffset >= v do DashOffset := DashOffset - v;

  while DashOffset - DashArray[DashIndex] > 0 do
  begin
    DashOffset := DashOffset - DashArray[DashIndex];
    Inc(DashIndex);
  end;

  J := 0;
  SetLength(Result, 1);
  if not Odd(DashIndex) then
    AddPoint(Points[0].X, Points[0].Y);
  for I := 1 to High(Points) do
  begin
    dx := Points[I].X - Points[I - 1].X;
    dy := Points[I].Y - Points[I - 1].Y;
    d := Hypot(dx*FixedToFloat, dy*FixedToFloat);
    if d = 0 then  Continue;
    dx := round(dx / d);
    dy := round(dy / d);
    Offset := Offset + d;
    while Offset > DashOffset do
    begin
      v := Offset - DashOffset;
      AddPoint(Points[I].X - round(v * dx), Points[I].Y - round(v * dy));
      DashIndex := (DashIndex + 1) mod Length(DashArray);
      DashOffset := DashOffset + DashArray[DashIndex];
      if Odd(DashIndex) then
      begin
        Inc(J);
        SetLength(Result, J + 1);
      end;
    end;
    if not Odd(DashIndex) then
      AddPoint(Points[I].X, Points[I].Y);
  end;
  if Length(Result[J]) = 0 then SetLength(Result, high(Result));
end;
//------------------------------------------------------------------------------

function GetUnitVector(const pt1, pt2: TFixedPoint): TFloatPoint;
var
  dx, dy, f: Single;
begin
  dx := (pt2.X - pt1.X)*FixedToFloat;
  dy := (pt2.Y - pt1.Y)*FixedToFloat;
  if (dx = 0) and (dy = 0) then
  begin
    Result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    Result.X := dx * f;
    Result.Y := dy * f;
  end;
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TFixedPoint): TFloatPoint;
var
  dx, dy, f: Single;
begin
  dx := (pt2.X - pt1.X)*FixedToFloat;
  dy := (pt2.Y - pt1.Y)*FixedToFloat;

  if (dx = 0) and (dy = 0) then
  begin
    Result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;  //ie perpendicular to
  Result.Y := -dx; //the unit vector
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TFloatPoint): TFloatPoint;
var
  dx, dy, f: Single;
begin
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);

  if (dx = 0) and (dy = 0) then
  begin
    Result := FloatPoint(0,0);
  end else
  begin
    f := 1 / GR32_Math.Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;  //ie perpendicular to
  Result.Y := -dx; //the unit vector
end;
//------------------------------------------------------------------------------

procedure SimpleLine(bitmap: TBitmap32;
  const pts: array of TFixedPoint; color: TColor32; closed: Boolean = False);
var
  i, j: Integer;
begin
  j := high(pts);
  if j < 1 then exit;
  for i := 1 to j do
    bitmap.LineXS(pts[i-1].X, pts[i-1].Y,pts[i].X, pts[i].Y, color);
  if closed then bitmap.LineXS(pts[j].X, pts[j].Y,pts[0].X, pts[0].Y, color);
end;
//------------------------------------------------------------------------------

procedure SimpleLine(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFixedPoint;
  color: TColor32; closed: Boolean);
var
  i: Integer;
begin
  for i := 0 to Length(ppts) -1 do SimpleLine(bitmap,  ppts[i], color, closed);
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; pts: array of TFixedPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding);
begin
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := aFillMode;
    AddPoints(pts[0], Length(pts));
    Draw(Bitmap, edgeColor, fillColor);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding);
var
  i, Len: Integer;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := aFillMode;
    AddPoints(pts[0][0],Length(pts[0]));
    for i := 1 to Len -1 do
    begin
      newline;
      AddPoints(pts[i][0],Length(pts[i]));
    end;
    Draw(bitmap, edgeColor, fillColor);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; const pts: array of TFloatPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding);
var
  i,highI: Integer;
  pts2: TArrayOfFixedPoint;
begin
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := aFillMode;
    highI := high(pts);
    SetLength(pts2, highI +1);
    for i := 0 to highI do pts2[i] := FixedPoint(pts[i]);
    AddPoints(pts2[0], Length(pts2));
    Draw(Bitmap, edgeColor, fillColor);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFloatPoint;
  edgeColor, fillColor: TColor32; aFillMode: TPolyFillMode = pfWinding);
var
  i, Len: Integer;
  pts2: TArrayOfFixedPoint;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := aFillMode;
    pts2 := MakeArrayOfFixedPoints(pts[0]);
    AddPoints(pts2[0],Length(pts2));
    for i := 1 to Len -1 do
    begin
      newline;
      pts2 := MakeArrayOfFixedPoints(pts[i]);
      AddPoints(pts2[0],Length(pts2));
    end;
    Draw(bitmap, edgeColor, fillColor);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; const pts: array of TFixedPoint;
  edgeColor: TColor32; pattern: TBitmap32; aFillMode: TPolyFillMode = pfWinding);
var
  i,Len: Integer;
  polyPts: TArrayOfArrayOfFixedPoint;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  SetLength(polyPts,1);
  SetLength(polyPts[0],Len);
  for i := 0 to Len -1 do polyPts[0][i] := pts[i];
  SimpleFill(bitmap, polyPts, edgeColor, pattern, aFillMode);
end;
//------------------------------------------------------------------------------

procedure SimpleFill(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  edgeColor: TColor32; pattern: TBitmap32; aFillMode: TPolyFillMode = pfWinding);
var
  i, Len: Integer;
  filler: TBitmapPolygonFiller;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  {$IFDEF GR32_PolygonsEx}
  with TPolygon32Ex.Create do
  {$ELSE}
  with TPolygon32.Create do
  {$ENDIF}
  try
    Closed := true;
    Antialiased := true;
    AntialiasMode := am16times;
    FillMode := aFillMode;
    filler := TBitmapPolygonFiller.Create;
    try
      filler.Pattern := pattern;
      AddPoints(pts[0][0], Length(pts[0]));
      for i := 1 to Len -1 do
      begin
        newline;
        AddPoints(pts[i][0],Length(pts[i]));
      end;
      DrawFill(bitmap, filler);
    finally
      filler.Free;
    end;

    if AlphaComponent(edgeColor) <> $0 then
      SimpleLine(bitmap, pts, edgeColor, true);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleGradientFillHorz(bitmap: TBitmap32; pts: TArrayOfArrayOfFixedPoint;
  edgeColor: TColor32; const colors: array Of TColor32);
var
  i, j: Integer;
  bmp: TBitmap32;
  rec: TFixedRect;
  r: TRect;
  dx: Single;
  p: TArrayOfColor32;
  p2: PColor32Array;
begin
  rec := GetBoundsFixedRect(pts);
  r := MakeRect(rec,rrOutside);
  with r do if (right = left) or (top = bottom) then exit;
  bmp := TBitmap32.Create;
  try
    bmp.Width := bitmap.Width;
    bmp.Height := bitmap.Height;
    bmp.DrawMode := dmBlend;
    bmp.CombineMode := cmMerge;

    //fill bmp with the gradient colors ...
    SetLength(p, bmp.Width);
    {$R-}
    dx := 1/(r.Right-r.Left);
    for i := max(r.Left,0) to min(r.Right,bmp.Width) -1 do
      p[i] := GetColor(colors, (i-r.Left)*dx);
    for j := max(r.Top,0) to min(r.Bottom,bmp.Height) -1 do
    begin
      p2 := bmp.ScanLine[j];
      for i := max(r.Left,0) to min(r.Right,bmp.Width) -1 do
        p2[i] := p[i];
    end;
    {$R+}

    SimpleFill(bitmap, pts, edgeColor, bmp);

  finally
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleGradientFillVert(bitmap: TBitmap32; pts: TArrayOfArrayOfFixedPoint;
  edgeColor: TColor32; const colors: array Of TColor32);
var
  i, j: Integer;
  bmp: TBitmap32;
  rec: TFixedRect;
  r: TRect;
  dy: Single;
  c: TColor32;
  p2: PColor32Array;
begin
  rec := GetBoundsFixedRect(pts);
  r := MakeRect(rec, rrOutside);
  with r do if (right = left) or (top = bottom) then exit;
  bmp := TBitmap32.Create;
  try
    bmp.Width := bitmap.Width;
    bmp.Height := bitmap.Height;
    bmp.DrawMode := dmBlend;
    bmp.CombineMode := cmMerge;

    //fill bmp with the gradient colors ...
    {$R-}
    dy := 1/(r.Bottom-r.top);
    for j := max(r.Top,0) to min(r.Bottom, bmp.Height) -1 do
    begin
      p2 := bmp.ScanLine[j];
      c := GetColor(colors, (j-r.Top)*dy);
      for i := max(r.Left,0) to min(r.Right, bmp.Width) -1 do p2[i] := c;
    end;
    {$R+}

    SimpleFill(bitmap, pts, edgeColor, bmp);
  finally
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleGradientFill(bitmap: TBitmap32; pts: array of TFixedPoint;
  edgeColor: TColor32; const colors: array Of TColor32; angle_degrees: Integer);
var
  i,Len: Integer;
  polyPts: TArrayOfArrayOfFixedPoint;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  SetLength(polyPts,1);
  SetLength(polyPts[0],Len);
  for i := 0 to Len -1 do polyPts[0][i] := pts[i];
  SimpleGradientFill(bitmap, polyPts, edgeColor, colors, angle_degrees);
end;
//------------------------------------------------------------------------------

function NearlyMatch(const s1, s2, tolerance: Single): Boolean;
begin
  Result := abs(s1 - s2) <= tolerance;
end;
//------------------------------------------------------------------------------

procedure SimpleGradientFill(
  bitmap: TBitmap32; pts: TArrayOfArrayOfFixedPoint; edgeColor: TColor32;
  const colors: array of TColor32; angle_degrees: Integer); overload;
var
  i, j, Len: Integer;
  bmp, bmp2: TBitmap32;
  tmpRec, tmpRec2: TFixedRect;
  rec, rec2, rec3: TRect;
  rec3_offset, rec3_diff: TPoint;
  AT: TAffineTransformation;
  rotatedPts: TArrayOfArrayOfFixedPoint;
  rotPoint: TFloatPoint;
  angle_radians, dx: Single;
  reverseColors: array of TColor32;
  src,dst: PColor32;
begin
  if angle_degrees < 0 then angle_degrees := angle_degrees + 360;
  angle_radians := angle_degrees*DegToRad;
  Len := Length(colors);
  if Len = 0 then exit;

  if NearlyMatch(angle_radians, 0, 5*rad01) then
  begin
    SimpleGradientFillHorz(bitmap, pts, edgeColor, colors);
    exit;
  end
  else if NearlyMatch(angle_radians, rad180, 5*rad01) then
  begin
    SetLength(reverseColors, Len);
    for i := 0 to Len -1 do reverseColors[i] := colors[Len-1-i];
    SimpleGradientFillHorz(bitmap, pts, edgeColor, reverseColors);
    exit;
  end
  else if NearlyMatch(angle_radians, rad90, 5*rad01) then
  begin
    SetLength(reverseColors, Len);
    for i := 0 to Len -1 do reverseColors[i] := colors[Len-1-i];
    SimpleGradientFillVert(bitmap, pts, edgeColor, reverseColors);
    exit;
  end
  else if NearlyMatch(angle_radians, rad270, 5*rad01) then
  begin
    SimpleGradientFillVert(bitmap, pts, edgeColor, colors);
    exit;
  end;

  i := 0;
  Len := Length(pts);
  while (i < Len) and (Length(pts[i]) = 0) do inc(i);
  if i = Len then exit;
  for j := i to Len-1 do
  begin
    if Length(pts[j]) = 0 then continue;
    if j = i then
      tmpRec := GetBoundsFixedRect(pts[j])
    else
    begin
      tmpRec2 := GetBoundsFixedRect(pts[j]);
      tmpRec := GetRectUnion(tmpRec2,tmpRec);
    end;
  end;
  rec := MakeRect(tmpRec,rrOutside);
  if (rec.Right = rec.Left) or (rec.Bottom = rec.Top) then exit;
  with rec do
    rotPoint := FloatPoint((left+right)/2,(top+bottom)/2);

  SetLength(rotatedPts, Length(pts));
  for i := 0 to high(pts) do
    rotatedPts[i] := rotatePoints(pts[i], FixedPoint(rotPoint), -angle_radians);

  for i := 0 to Len-1 do
  begin
    if Length(rotatedPts[i]) = 0 then continue;
    if i = 0 then
      tmpRec := GetBoundsFixedRect(rotatedPts[i])
    else
    begin
      tmpRec2 := GetBoundsFixedRect(rotatedPts[i]);
      tmpRec := GetRectUnion(tmpRec2,tmpRec);
    end;
  end;
  rec2 := MakeRect(tmpRec,rrOutside);
  Windows.InflateRect(rec2, 1, 1);
  rec3 := rec2;
  if rec3.Left > rec.Left then rec3.Left := rec.Left;
  if rec3.Top > rec.Top then rec3.Top := rec.Top;
  if rec3.Right < rec.Right then rec3.Right := rec.Right;
  if rec3.Bottom < rec.Bottom then rec3.Bottom := rec.Bottom;

  rec3_offset := Point(rec3.Left, rec3.Top);
  rec3_diff := Point(rec2.Left - rec3.Left, rec2.Top - rec3.Top);
  Windows.offsetRect(rec3, -rec3.Left, -rec3.Top);
  offsetPoint(rotPoint, -rec2.Left, -rec2.Top);
  Windows.offsetRect(rec2, -rec2.Left, -rec2.Top);

  bmp := TBitmap32.Create;
  bmp2 := TBitmap32.Create;
  try
    bmp.DrawMode := dmBlend;
    bmp.CombineMode := cmMerge;
    bmp.SetSize(rec2.right,rec2.bottom);

    bmp2.DrawMode := dmOpaque;
    bmp2.SetSize(rec3.right,rec3.bottom);

    {$R-}
    dx := 1/bmp.Width;
    src := @bmp.bits[0];
    for i := 0 to bmp.Width -1 do
    begin
      src^ := GetColor(colors, i*dx);
      //src^ := GradientColor(fillColor2, fillColor1, i*dx);
      inc(src);
    end;

    for i := 1 to bmp.Height -1 do
    begin
      src := @bmp.bits[0];
      dst := @bmp.bits[i*bmp.Width];
      for j := 0 to bmp.Width -1 do
      begin
        dst^ := src^;
        inc(src);
        inc(dst);
      end;
    end;
    {$R+}

    AT := TAffineTransformation.Create;
    try
      AT.SrcRect := FloatRect(rec2);
      AT.Rotate(rotPoint.X, rotPoint.Y, angle_degrees);
      AT.Translate(rec3_diff.X, rec3_diff.Y);
      GR32_Transforms.Transform(bmp2, bmp, AT);
    finally
      AT.free;
    end;

    bmp.SetSize(bitmap.Width, bitmap.Height);
    bmp2.DrawTo(bmp,rec3_offset.X,rec3_offset.Y);
    SimpleFill(bitmap, pts, edgeColor, bmp);
  finally
    bmp.Free; bmp2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleStippleFill(bitmap: TBitmap32; pts: array of TFixedPoint;
  const colors: array Of TColor32; step: Single; angle_degrees: Integer);
var
  bmp, bmp2: TBitmap32;
  rec, rec2, rec3: TRect;
  rec3_offset, rec3_diff: TPoint;
  AT: TAffineTransformation;
  rotatedPts: TArrayOfFixedPoint;
  rotPoint: TFloatPoint;
  i, j: Integer;
  angle_radians: Single;
  src,dst: PColor32;
begin
  angle_radians := angle_degrees*DegToRad;
  rec := GetBoundsRect(pts);
  if (rec.Right = rec.Left) or (rec.Bottom = rec.Top) then exit;

  if angle_degrees = 0 then
  begin
    //this avoids a lot of the complexity below ...
    bmp := TBitmap32.Create;
    bmp2 := TBitmap32.Create;
    try
      bmp.DrawMode := dmBlend;
      bmp.CombineMode := cmMerge;
      with rec do
        bmp.SetSize(right-left,bottom-top);
      bmp.SetStipple(colors);
      bmp.StippleStep := step;

      bmp2.DrawMode := dmBlend;
      bmp2.SetSize(rec.right,rec.bottom);

      bmp.LineFSP(0, 0, bmp.Width, 0);
      for i := 1 to bmp.Height -1 do
      begin
        {$R-}
        src := @bmp.bits[0];
        dst := @bmp.bits[i*bmp.Width];
        {$R+}
        for j := 0 to bmp.Width -1 do
        begin
          dst^ := src^;
          inc(src);
          inc(dst);
        end;
      end;

      bmp.DrawTo(bmp2,rec.Left,rec.Top);
      SimpleFill(bitmap, pts, $00000000, bmp2);
    finally
      bmp.Free;
      bmp2.Free;
    end;
    exit;
  end;

  with rec do
    rotPoint := FloatPoint((left+right)/2,(top+bottom)/2);
  rotatedPts := rotatePoints(pts, FixedPoint(rotPoint), -angle_radians);
  rec2 := GetBoundsRect(rotatedPts);
  rec3 := rec2;
  if rec3.Left > rec.Left then rec3.Left := rec.Left;
  if rec3.Top > rec.Top then rec3.Top := rec.Top;
  if rec3.Right < rec.Right then rec3.Right := rec.Right;
  if rec3.Bottom < rec.Bottom then rec3.Bottom := rec.Bottom;

  rec3_offset := Point(rec3.Left, rec3.Top);
  rec3_diff := Point(rec2.Left - rec3.Left, rec2.Top - rec3.Top);
  Windows.offsetRect(rec3, -rec3.Left, -rec3.Top);
  offsetPoint(rotPoint, -rec2.Left, -rec2.Top);
  Windows.offsetRect(rec2, -rec2.Left, -rec2.Top);

  bmp := TBitmap32.Create;
  bmp2 := TBitmap32.Create;
  try
    bmp.DrawMode := dmBlend;
    bmp.CombineMode := cmMerge;
    bmp.SetSize(rec2.right,rec2.bottom);
    bmp.SetStipple(colors);
    bmp.StippleStep := step;

    bmp2.DrawMode := dmOpaque;
    bmp2.SetSize(rec3.right,rec3.bottom);

    bmp.LineFSP(0, 0, bmp.Width, 0);
    for i := 1 to bmp.Height -1 do
    begin
      {$R-}
      src := @bmp.bits[0];
      dst := @bmp.bits[i*bmp.Width];
      {$R+}
      for j := 0 to bmp.Width -1 do
      begin
        dst^ := src^;
        inc(src);
        inc(dst);
      end;
    end;

    AT := TAffineTransformation.Create;
    try
      AT.SrcRect := FloatRect(rec2);
      AT.Rotate(rotPoint.X, rotPoint.Y, angle_degrees);
      AT.Translate(rec3_diff.X, rec3_diff.Y);
      GR32_Transforms.Transform(bmp2, bmp, AT);
    finally
      AT.free;
    end;

    bmp.SetSize(rec3_offset.X+rec3.Right, rec3_offset.Y+rec3.Bottom);
    bmp2.DrawTo(bmp,rec3_offset.X,rec3_offset.Y);

    //finally fill the polygon ...
    SimpleFill(bitmap, pts, $00000000, bmp);
  finally
    bmp.Free;
    bmp2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleRadialFill(bitmap: TBitmap32; const pt: TFixedPoint;
  Radius: Single; const colors: array of TColor32);
var
  i,j, colorLen: Integer;
  dx,dy, dist, radiusPlus, radiusMinus: Single;
  p: TFloatPoint;
  pts: TArrayOfFixedPoint;
  rec, unclippedRec: TRect;
  outerColor, c, M: TColor32;
  dst, dstQ1,dstQ2,dstQ3,dstQ4: PColor32;
begin
  colorLen := Length(colors);
  if (Radius < 1) or (colorLen = 0) then exit;
  if colorLen = 1 then
  begin
    //draw monochrome circle of specified Radius then exit
    with FloatPoint(pt) do
      pts := GetEllipsePoints(FloatRect(X-Radius,Y-Radius,X+Radius,Y+Radius));
    SimpleFill(bitmap,pts,$0,colors[0]);
    exit;
  end;

  radiusPlus := Radius + 0.5;
  radiusMinus := Radius - 0.5;
  outerColor := colors[colorLen -1];

  p := FloatPoint(pt);
  with p do
  begin
    rec.Left := floor(X - radiusPlus);
    rec.Top := floor(Y - radiusPlus);
    rec.Right := ceil(X + radiusPlus);
    rec.Bottom := ceil(Y + radiusPlus);
  end;
  unclippedRec := rec;
  if rec.Left < 0 then rec.Left := 0;
  if rec.Right >= bitmap.Width then rec.Right := bitmap.Width -1;
  if rec.Top < 0 then rec.Top := 0;
  if rec.Bottom >= bitmap.Height then rec.Bottom := bitmap.Height -1;
  if (rec.Left >= rec.Right) or (rec.Top >= rec.Bottom) then exit;

  {$R-}
  if compareMem(@rec, @unclippedRec, sizeof(TRect)) then
  begin
    //If the bounding rectangle hasn't been clipped then we can speed things up
    //by calculating the color in only one quadrant ...
    for i := rec.Top to (rec.Top + rec.Bottom) div 2 do
    begin
      dstQ1 := @bitmap.bits[rec.Left + i*bitmap.width];
      dstQ2 := @bitmap.bits[rec.Right + i*bitmap.width];
      dstQ3 := @bitmap.bits[rec.Left + (rec.Bottom + rec.Top - i)*bitmap.width];
      dstQ4 := @bitmap.bits[rec.Right + (rec.Bottom + rec.Top - i)*bitmap.width];

      for j := rec.Left to (rec.Left + rec.Right) div 2 do
      begin
        dy := p.Y - i;
        dx := p.X - j;
        dist := GR32_Math.Hypot(dx, dy);
        if dist > radiusPlus then
          //do nothing
        else if dist > radiusMinus then
        begin
          M := round(255- (dist - radiusMinus)*255);
          BlendMemEx(outerColor, dstQ1^, M);
          if dstQ1 <> dstQ2 then BlendMemEx(outerColor, dstQ2^, M);
          if dstQ1 <> dstQ3 then BlendMemEx(outerColor, dstQ3^, M);
          if (dstQ3 <> dstQ4) and (dstQ2 <> dstQ4) then
            BlendMemEx(outerColor, dstQ4^, M);
          EMMS;
        end
        else
        begin
          c := GetColor(colors, dist/Radius);
          MergeMem(c, dstQ1^);
          if dstQ1 <> dstQ2 then MergeMem(c, dstQ2^);
          if dstQ1 <> dstQ3 then MergeMem(c, dstQ3^);
          if (dstQ3 <> dstQ4) and (dstQ2 <> dstQ4) then MergeMem(c, dstQ4^);
          EMMS;
        end;
        inc(dstQ1); dec(dstQ2); inc(dstQ3); dec(dstQ4);
      end;
    end;
  end else
  begin
    for i := rec.Top to rec.Bottom do
    begin
      dst := @bitmap.bits[rec.Left + i*bitmap.width];
      for j := rec.Left to rec.Right do
      begin
        dy := p.Y - i;
        dx := p.X - j;
        dist := GR32_Math.Hypot(dx, dy);
        if dist > radiusPlus then
          //do nothing
        else if dist > radiusMinus then
        begin
          BlendMemEx(outerColor, dst^, round(255- (dist - radiusMinus)*255));
          EMMS;
        end
        else
        begin
          c := GetColor(colors, dist/Radius);
          MergeMem(c, dst^);
          EMMS;
        end;
        inc(dst);
      end;
    end;
  end;
  {$R+}
end;
//------------------------------------------------------------------------------

procedure SimpleRadialFill(bitmap: TBitmap32; rec: TFloatRect;
  const colors: array of TColor32);
var
  i,j,colorLen: Integer;
  a, b, a2, r, rPlus, rMinus, theta: Single;
  mp: TFloatPoint;
  pts: TArrayOfFixedPoint;
  rec2, unclippedRec: TRect;
  outerColor, c, M: TColor32;
  dst, dstQ1,dstQ2,dstQ3,dstQ4: PColor32;
begin
  colorLen := Length(colors);
  if colorLen = 0 then exit;
  if colorLen = 1 then
  begin
    pts := GetEllipsePoints(rec);
    SimpleFill(bitmap, pts, $0, colors[0]);
    exit;
  end;

  with rec do
  begin
    if Left < 0 then Left := 0;
    if Top < 0 then Top := 0;
    if Right >= bitmap.Width then Right := bitmap.Width -1;
    if Bottom >= bitmap.Height then Bottom := bitmap.Height -1;
    if (right-left < 1) or (bottom-top < 1) then exit;
    mp := FloatPoint((right+left)/2, (top+bottom)/2);
    a := mp.X - left;
    b := mp.Y - top;
    if (a < 1) or (b < 1) then exit;
  end;
  rPlus := (a+1)/a;
  rMinus := (a-1)/a;

  rec2 := MakeRect(rec, rrOutside);
  unclippedRec := rec2;
  with rec2 do
  begin
    if (right-left = top-bottom) then
    begin
      //circular, so use the faster algorithm ...
      SimpleRadialFill(bitmap, FixedPoint(mp), a, colors);
      exit;
    end;
    if (Left >= Right) or (Top >= Bottom) then exit;

    outerColor := colors[colorLen -1];
    {$R-}
    //If the bounding rectangle hasn't been clipped then we can speed things up
    //by calculating the color in only one quadrant ...
    if compareMem(@rec2, @unclippedRec, sizeof(TRect)) then
    begin
      for i := Top to (Top + Bottom) div 2 do
      begin
        dstQ1 := @bitmap.bits[Left + i*bitmap.width];
        dstQ2 := @bitmap.bits[Right + i*bitmap.width];
        dstQ3 := @bitmap.bits[Left + (Bottom + Top - i)*bitmap.width];
        dstQ4 := @bitmap.bits[Right + (Bottom + Top - i)*bitmap.width];
        for j := Left to (Left + Right) div 2 do
        begin
          if j = mp.X then
          begin
            r := (mp.Y -i)/b;
          end else
          begin
            //?= arctan2(a*y, b*x); a = x / cos?
            theta := arctan2(a*(mp.Y-i), b*(mp.X -j));
            a2 := (mp.X -j) / cos(theta);
            r := a2/a;
          end;

          if r > rPlus then
            //do nothing
          else if r > rMinus then //ie anti-alias the edge
          begin
            M := round(255- (r-RMinus)/(rPlus-rMinus)*255);
            BlendMemEx(outerColor, dstQ1^, M);
            if dstQ1 <> dstQ2 then BlendMemEx(outerColor, dstQ2^, M);
            if dstQ1 <> dstQ3 then BlendMemEx(outerColor, dstQ3^, M);
            if (dstQ3 <> dstQ4) and (dstQ2 <> dstQ4) then
              BlendMemEx(outerColor, dstQ4^, M);
            EMMS;
          end
          else
          begin
            c := GetColor(colors, r);
            if c and $FF000000 <> 0 then
            begin
              MergeMem(c, dstQ1^);
              if dstQ1 <> dstQ2 then MergeMem(c, dstQ2^);
              if dstQ1 <> dstQ3 then MergeMem(c, dstQ3^);
              if (dstQ3 <> dstQ4) and (dstQ2 <> dstQ4) then MergeMem(c, dstQ4^);
              EMMS;
            end;
          end;
          inc(dstQ1); dec(dstQ2); inc(dstQ3); dec(dstQ4);
        end;
      end;
    end else
    begin
      for i := Top to Bottom do
      begin
        dst := @bitmap.bits[Left + i*bitmap.width];
        for j := Left to Right do
        begin
          if j = mp.X then continue;
          //?= arctan2(a*y, b*x); a = x / cos?
          theta := arctan2(a*(mp.Y-i), b*(mp.X -j));
          a2 := (mp.X -j) / cos(theta) /a;
          if a2 > rPlus then
            //do nothing
          else if a2 > rMinus then //ie anti-alias the edge
          begin
            M := round(255- (a2-RMinus)/(rPlus-rMinus)*255);
            BlendMemEx(outerColor, dst^, M);
            EMMS;
          end else
          begin
            c := GetColor(colors, a2);
            MergeMem(c, dst^);
            EMMS;
          end;
          inc(dst);
        end;
      end;
    end;
    {$R+}
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleRadialFill(bitmap: TBitmap32;
  const pts: array of TFixedPoint;
  const colors: array of TColor32; angle_degrees: Integer = 0);
var
  i,Len: Integer;
  polyPts: TArrayOfArrayOfFixedPoint;
begin
  Len := Length(pts);
  if Len = 0 then exit;
  SetLength(polyPts,1);
  SetLength(polyPts[0],Len);
  for i := 0 to Len -1 do polyPts[0][i] := pts[i];
  SimpleRadialFill(bitmap, polyPts, colors, angle_degrees);
end;
//------------------------------------------------------------------------------

procedure SimpleRadialFill(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFixedPoint;
  const colors: array of TColor32; angle_degrees: Integer = 0);
var
  i,j,Len: Integer;
  mask, rotFillBmp, unrotFillBmp: TBitmap32;
  rec: TRect;
  mp: TFixedPoint;
  ppts2: TArrayOfArrayOfFixedPoint;
  a, newA, tmpA, b, theta: Single;
  AT: TAffineTransformation;
begin
  if Length(colors) = 0 then exit;
  Len := Length(ppts);
  rec := MakeRect(GetBoundsFixedRect(ppts), rrOutside);
  with rec do
    mp := FixedPoint((Left+Right)/2,(top+bottom)/2);

  ppts2 := CopyPolyPoints(ppts);
  if angle_degrees <> 0 then
  begin
    //unrotate points to get the unrotated boundsrect ...
    RotatePolyPoints(ppts2, mp, -angle_degrees*DegToRad);
    rec := MakeRect(GetBoundsFixedRect(ppts2), rrOutside);
    with rec do
      mp := FixedPoint((Left+Right)/2,(top+bottom)/2);
  end;

  with rec do
  begin
    a := (Right-Left)/2;
    b := (Bottom-Top)/2;
  end;

  //now see if rec needs enlarging ...
  newA := a;
  for i := 0 to Len-1 do
    for j := 0 to high(ppts2[i]) do
    begin
      //?= arctan2(a*y, b*x); a = x / cos?
      if (mp.X = ppts2[i][j].X) or (mp.Y = ppts2[i][j].Y) then continue;
      theta := arctan2(a*(mp.Y-ppts2[i][j].Y) , b*(mp.X -ppts2[i][j].X));
      tmpA := abs((ppts2[i][j].X - mp.X)*FixedToFloat / cos(theta));
      if tmpA > newA then newA := tmpA;
    end;
  if round(newA) > a then
  begin
    b := b * newA/a; //scales 'b' relative to newA
    a := newA;
  end;
  rec := MakeRect(0,0,ceil(a)*2,ceil(b)*2);

  //now ....
  //1. create a mask using ppts
  //2. create and fill unrotFillBmp bitmap with the radial colors
  //3. create rotFillBmp and copy bitmap to it
  //4. rotate and transform unrotFillBmp onto rotFillBmp
  //5. finally apply rotFillBmp using the mask onto bitmap
  unrotFillBmp := TBitmap32.Create;
  rotFillBmp := TBitmap32.Create;
  mask := CreateMaskFromPolygon(bitmap, ppts);
  try
    with rec do
      unrotFillBmp.SetSize(right-left,bottom-top);
    SimpleRadialFill(unrotFillBmp, FloatRect(rec), colors);
    rotFillBmp.Assign(bitmap);

    AT := TAffineTransformation.Create;
    try
      AT.SrcRect := FloatRect(rec);
      if angle_degrees <> 0 then
        AT.Rotate(rec.Right div 2, rec.Bottom div 2, angle_degrees);
      with FloatPoint(mp) do
        AT.Translate(X - rec.Right div 2, Y - rec.Bottom div 2);
      GR32_Transforms.Transform(rotFillBmp, unrotFillBmp, AT);
    finally
      AT.free;
    end;
    ApplyMask(bitmap, rotFillBmp, mask);
  finally
    unrotFillBmp.Free;
    rotFillBmp.Free;
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SimpleShadow(bitmap: TBitmap32; const pts: TArrayOfFixedPoint;
  dx, dy, fadeRate: Integer; shadowColor: TColor32;
  closed: Boolean = False; NoInteriorBleed: Boolean = False);
var
  ppts: TArrayOfArrayOfFixedPoint;
begin
  SetLength(ppts, 1);
  ppts[0] := pts;
  SimpleShadow(bitmap,ppts,dx,dy,fadeRate,shadowColor,closed,NoInteriorBleed);
end;
//------------------------------------------------------------------------------

procedure SimpleShadow(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  dx, dy, fadeRate: Integer; shadowColor: TColor32;
  closed: Boolean = False; NoInteriorBleed: Boolean = False);
var
  i, j, maxD: Integer;
  sx,sy, a, alpha, alphaLinear, alphaExp, dRate: Single;
  p: TArrayOfFixedPoint;
  originalBitmap, maskBitmap: TBitmap32;
  sc: TColor32;
begin
  if ((dx = 0) and (dy = 0)) or (Length(pts) = 0) then exit;

  if abs(dy) > abs(dx) then
  begin
    maxD := abs(dy);
    sy := sign(dy);
    sx := dx/maxD;
  end else
  begin
    maxD := abs(dx);
    sx := sign(dx);
    sy := dy/maxD;
  end;

  if fadeRate <= MAXIMUM_SHADOW_FADE then dRate := 0.05
  else if fadeRate >= MINIMUM_SHADOW_FADE then dRate := 0.95
  else dRate := fadeRate/10;
  alpha := AlphaComponent(shadowColor);
  alphaLinear := alpha*dRate/maxD;
  alphaExp := exp(ln(dRate)/maxD);

  NoInteriorBleed := NoInteriorBleed and closed;
  if NoInteriorBleed then
  begin
    originalBitmap := TBitmap32.Create;
    originalBitmap.Assign(bitmap);
    maskBitmap := CreateMaskFromPolygon(bitmap,pts);
  end else
  begin
    originalBitmap := nil;
    maskBitmap := nil;
  end;

  try
    a := alpha;
    sc := shadowColor;
    for j := 0 to high(pts) do
    begin
      alpha := a;
      shadowColor := sc;
      p := copy(pts[j], 0, Length(pts[j]));
      for i := 1 to maxD do
      begin
        SimpleLine(bitmap, p, shadowColor, closed);
        alpha := alpha * alphaExp;
        if fadeRate < NO_SHADOW_FADE then
          shadowColor := SetAlpha(shadowColor, round(alpha - i*alphaLinear));
        OffsetPoints(p, sx, sy);
      end;
    end;
    if assigned(originalBitmap) then
      ApplyMask(bitmap, originalBitmap, maskBitmap);
  finally
    FreeAndNil(originalBitmap);
    FreeAndNil(maskBitmap);
  end;
end;
//------------------------------------------------------------------------------

procedure Simple3D(bitmap: TBitmap32; const ppts: TArrayOfArrayOfFloatPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32);
var
  i,j: Integer;
  ppts2: TArrayOfArrayOfFixedPoint;
begin
  SetLength(ppts2, Length(ppts));
  for i := 0 to high(ppts) do
  begin
    SetLength(ppts2[i], Length(ppts[i]));
    for j := 0 to high(ppts[i]) do
      ppts2[i][j] := FixedPoint(ppts[i][j]);
  end;
  Simple3D(bitmap, ppts2, dx, dy, fadeRate, topLeftColor, bottomRightColor);
end;
//------------------------------------------------------------------------------

procedure Simple3D(bitmap: TBitmap32; const pts: TArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32);
var
  ppts: TArrayOfArrayOfFixedPoint;
begin
  SetLength(ppts, 1);
  ppts[0] := pts;
  Simple3D(bitmap, ppts, dx, dy, fadeRate, topLeftColor, bottomRightColor);
end;
//------------------------------------------------------------------------------

procedure Simple3D(bitmap: TBitmap32; const pts: TArrayOfArrayOfFixedPoint;
  dx,dy,fadeRate: Integer; topLeftColor, bottomRightColor: TColor32);
var
  mask, orig: TBitmap32;
begin
  orig := TBitmap32.Create;
  mask := CreateMaskFromPolygon(bitmap,pts);
  try
    orig.Assign(bitmap);
    SimpleShadow(bitmap, pts, -dx, -dy, fadeRate, bottomRightColor, true);
    SimpleShadow(bitmap, pts, dx, dy, fadeRate, topLeftColor, true);
    ApplyMask(bitmap, orig, mask, true);
  finally
    orig.Free;
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

type
  TArrayOfArrayOfColor = array of array of TColor32;

procedure SimpleBevel(bitmap: TBitmap32; ppts: TArrayOfArrayOfFixedPoint;
  strokeWidth: Integer; lightClr, darkClr: TColor32; angle_degrees: Integer);
var
  i,j,k,m,n,alpha: Integer;
  ccls: TArrayOfArrayOfColor;
  b: TBitmap32;

  function ColorFromAngle(pt1,pt2: TFixedPoint): TColor32;
  var
    a: Integer;
  begin
    a := trunc(GetAngleOfPt2FromPt1(pt1,pt2)*RadToDeg);
    //angle_degrees: angle that the light color deviates from the vertical
    inc(a,angle_degrees-135); //-135 so defaults to dark at bottom-right
    while a > 360 do dec(a,360);
    while a < 0 do inc(a,360);
    if a < 180 then
      Result := lightClr else
      Result := GradientColor(darkClr, lightClr, abs(270-a)/90 -0.02);
  end;

begin
  alpha := (AlphaComponent(lightClr)+AlphaComponent(darkClr)) div 2;
  if (alpha < 254) and (strokeWidth > 1) then
  begin
    b := TBitmap32.Create;
    with bitmap do b.SetSize(width,height);
    b.Clear(lightClr and $00FFFFFF);
    b.DrawMode := dmBlend;
    b.MasterAlpha := alpha;
    lightClr := lightClr or $FF000000;
    darkClr := darkClr or $FF000000;
  end else
    b := bitmap;

  try
    //get the colors ...
    SetLength(ccls, Length(ppts));
    for i := 0 to high(ppts) do
    begin
      k := Length(ppts[i]);
      if k < 2 then continue;
      SetLength(ccls[i], k);
      ccls[i][0] := ColorFromAngle(ppts[i][0],ppts[i][k-1]);
      for j := 1 to k-1 do
        ccls[i][j] := ColorFromAngle(ppts[i][j],ppts[i][j-1]);
    end;


    if strokeWidth > 1 then
      OffsetPolyPoints(ppts,-strokeWidth/2 +0.5,-strokeWidth/2 +0.5);

    for m := 1 to strokeWidth do
    begin
      for n := 1 to strokeWidth do
      begin
        for i := 0 to high(ppts) do
        begin
          k := Length(ppts[i]);
          if k < 2 then continue;
          for j := 1 to k-1 do
            b.LineXS(ppts[i][j-1].X,ppts[i][j-1].Y,
              ppts[i][j].X,ppts[i][j].Y, ccls[i][j]);
          b.LineXS(ppts[i][k-1].X,ppts[i][k-1].Y,
            ppts[i][0].X,ppts[i][0].Y,ccls[i][0]);
        end;
        if strokeWidth = 1 then exit;
        if Odd(m) then
          OffsetPolyPoints(ppts, 0, 1) else
          OffsetPolyPoints(ppts, 0, -1);
      end;
      OffsetPolyPoints(ppts,1,0);
    end;
  finally
    if b <> bitmap then
    begin
      bitmap.Draw(0,0,b);
      b.Free;
    end;
  end;
end;
//------------------------------------------------------------------------------

end.
