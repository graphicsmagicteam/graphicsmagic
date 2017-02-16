unit gmMath;

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

// Update Date: 2016/10/27

{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Windows, Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmTypes;

type
  // Quadrants (current postion relative to the source position)
  TgmCoordQuadrant = (cqNone, cqFirst, cqSecond, cqThird, cqFourth);

  { TLineOrientation is used as part of the heuristic algorithm that decides
    if a line is selected and if lines intersect }
  TgmLineOrientation = (loPoint, loHorizontal, loVertical);

{ Points }

// get the quadrant of current point ralative to the source point
function GetQuadrant(const ASourcePoint, ATestPoint: TPoint): TgmCoordQuadrant;

// calculate the new position in the new region with the same position scale as original
function ScalePoint(const APoint: TPoint;
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer): TPoint;

// calculating end-point for regular figures (square and circle)
function CalculateRegularFigureEndPoint(
  const AStartPoint, ACurrentPoint: TPoint): TPoint;

// calculate vertices for regular polygon
procedure CalcRegularPolygonVertices(var APointArray: array of TPoint;
  const ACenterPoint, ACurrentPoint: TPoint; const ASides: Integer);

// calculate the line outline vertices
procedure CalcLineOutlineVertices(var APolygon: array of TPoint;
  const ALineStart, ALineEnd: TPoint; const ALineWeight: Integer);

// determine whether the two coordinates are same
function SameCoordinate(const APoint1, APoint2: TPoint): Boolean;

// determine whether the two coordinates are different
function DifferentCoordinate(const APoint1, APoint2: TPoint): Boolean;

function GetOppositePoint(const ACenterPoint, ACurrentPoint: TPoint): TPoint;
function CalcRadianByTwoPoints(const ACenterPoint, ACurrentPoint: TPoint): Double;
function CalcLengthByTwoPoints(const APoint1, APoint2: TPoint): Double;

function CalcOffsetPointByRadian(const ACenterPoint: TPoint;
  const ARadian, ALength: Double): TPoint;

// get bounding rectangle coordinates of a polygon
function GetPolygonRect(APointArray: array of TPoint; out TL, BR: TPoint): Boolean;

function GetMidPointAtLine(const AStartPoint, AEndPoint: TPoint): TPoint;

// calculate offset after the canvas size is changed
function CalcOffsetCoordinateByAnchorDirection(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer;
  const AAnchor: TgmAnchorDirection): TPoint;

function GetStarPoints(const ACenterPoint, AEndingPoint: TPoint;
  const AVertexCount: Integer = 5): TArrayOfPoint;

{ Lines }

{ The following routines was written by Xiong Wei.
  Copyright (C) ? Xiong Wei. (from Nanchang, China)

  Many thanks for Xiong Wei for permitting us distribute these routines with
  GraphicsMagic. }

function IfInLine(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;

function PointOnCurve(const FX1, FY1, FX2, FY2: Integer;
  const FX3, FY3, FX4, FY4: Integer; const pt: TPoint): Boolean;

// calculate bounding rectangle coordinates of a curve
procedure CalcCurvePoint(const FX1, FY1, FX2, FY2: Integer;
  const FX3, FY3, FX4, FY4: Integer; var FP1, FP2: TPoint);

function IfInRectangle(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;
function IfInEllipse(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;

{ Angle }

// get an acute angle from -90 to 90 degrees
procedure GetAcuteAngle(const ACenterPoint, ACurrentPoint: TPoint;
  var AAngle: Integer);

{ Rectangle }

function AddRects(const ARect1, ARect2: TRect): TRect;
function GetRectSize(const ARect: TRect): TSize;

{ Range Calculation }

procedure EnsureValueInRange(var ACheckValue: Integer;
  const AMinimumValue, AMaximumValue: Integer); overload;

procedure EnsureValueInRange(var ACheckValue: Double;
  const AMinimumValue, AMaximumValue: Double);  overload;

{ Used for PrintOptions in order to get the bitmap size to fit the
  specified size by scale as possible. }
function GetScaleToFitMedia(const AOldWidth, AOldHeight, AMediaWidth, AMediaHeight: Integer): Double;

{ Arithmic }

{ The following routines were found at:
  http://delphi.about.com/od/mathematics/a/baseconvert.htm }
function HexToInt(AHexNum: string): LongInt;
function IntToBin(AValue: LongInt; ADigits: Integer): string;
function BinToInt(AValue: string): LongInt;

// adapted from x2nie's code
procedure QuickSort(AIntArray: TArrayOfInteger; L, R: Integer);

const
  // The PAEz_COS_TABLE and PAEz_SIN_TABLE are introduced by PAEz.

  PAEz_COS_TABLE: array[0..180] of Double =
  (
   -2.71050543121376E-20,
    0.01745240643728,
    0.0348994967025,
    0.05233595624294,
    0.06975647374412,
    0.08715574274765,
    0.10452846326765,
    0.12186934340514,
    0.13917310096006,
    0.15643446504023,
    0.17364817766693,
    0.19080899537654,
    0.20791169081775,
    0.22495105434386,
    0.24192189559966,
    0.25881904510252,
    0.27563735581699,
    0.29237170472273,
    0.30901699437494,
    0.32556815445715,
    0.34202014332566,
    0.3583679495453,
    0.37460659341591,
    0.39073112848927,
    0.4067366430758,
    0.42261826174069,
    0.43837114678907,
    0.45399049973954,
    0.46947156278589,
    0.48480962024633,
    0.5,
    0.51503807491005,
    0.5299192642332,
    0.54463903501502,
    0.55919290347074,
    0.57357643635104,
    0.58778525229247,
    0.60181502315204,
    0.61566147532565,
    0.62932039104983,
    0.64278760968653,
    0.6560590289905,
    0.66913060635885,
    0.68199836006249,
    0.69465837045899,
    0.70710678118654,
    0.71933980033865,
    0.73135370161917,
    0.74314482547739,
    0.75470958022277,
    0.76604444311897,
    0.77714596145697,
    0.78801075360672,
    0.79863551004729,
    0.80901699437494,
    0.81915204428899,
    0.82903757255504,
    0.83867056794542,
    0.84804809615642,
    0.85716730070211,
    0.86602540378443,
    0.87461970713939,
    0.88294759285892,
    0.89100652418836,
    0.89879404629916,
    0.90630778703665,
    0.9135454576426,
    0.92050485345244,
    0.92718385456678,
    0.9335804264972,
    0.9396926207859,
    0.94551857559931,
    0.95105651629515,
    0.95630475596303,
    0.96126169593831,
    0.96592582628906,
    0.97029572627599,
    0.97437006478523,
    0.9781476007338,
    0.98162718344766,
    0.9848077530122,
    0.98768834059513,
    0.99026806874157,
    0.99254615164132,
    0.99452189536827,
    0.99619469809174,
    0.99756405025982,
    0.99862953475457,
    0.99939082701909,
    0.99984769515639,
    1,
    0.99984769515639,
    0.99939082701909,
    0.99862953475457,
    0.99756405025982,
    0.99619469809174,
    0.99452189536827,
    0.99254615164132,
    0.99026806874157,
    0.98768834059513,
    0.9848077530122,
    0.98162718344766,
    0.9781476007338,
    0.97437006478523,
    0.97029572627599,
    0.96592582628906,
    0.96126169593831,
    0.95630475596303,
    0.95105651629515,
    0.94551857559931,
    0.9396926207859,
    0.9335804264972,
    0.92718385456678,
    0.92050485345244,
    0.9135454576426,
    0.90630778703665,
    0.89879404629916,
    0.89100652418836,
    0.88294759285892,
    0.87461970713939,
    0.86602540378443,
    0.85716730070211,
    0.84804809615642,
    0.83867056794542,
    0.82903757255504,
    0.81915204428899,
    0.80901699437494,
    0.79863551004729,
    0.78801075360672,
    0.77714596145697,
    0.76604444311897,
    0.75470958022277,
    0.74314482547739,
    0.73135370161917,
    0.71933980033865,
    0.70710678118654,
    0.69465837045899,
    0.68199836006249,
    0.66913060635885,
    0.6560590289905,
    0.64278760968653,
    0.62932039104983,
    0.61566147532565,
    0.60181502315204,
    0.58778525229247,
    0.57357643635104,
    0.55919290347074,
    0.54463903501502,
    0.5299192642332,
    0.51503807491005,
    0.5,
    0.48480962024633,
    0.46947156278589,
    0.45399049973954,
    0.43837114678907,
    0.42261826174069,
    0.4067366430758,
    0.39073112848927,
    0.37460659341591,
    0.3583679495453,
    0.34202014332566,
    0.32556815445715,
    0.30901699437494,
    0.29237170472273,
    0.27563735581699,
    0.25881904510252,
    0.24192189559966,
    0.22495105434386,
    0.20791169081775,
    0.19080899537654,
    0.17364817766693,
    0.15643446504023,
    0.13917310096006,
    0.12186934340514,
    0.10452846326765,
    0.08715574274765,
    0.06975647374412,
    0.05233595624294,
    0.0348994967025,
    0.01745240643728,
   -2.71050543121376E-20
  );

  PAEz_SIN_TABLE: array[0..180] of Double =
  (
   1,
   0.99984769515639,
   0.99939082701909,
   0.99862953475457,
   0.99756405025982,
   0.99619469809174,
   0.99452189536827,
   0.99254615164132,
   0.99026806874157,
   0.98768834059513,
   0.9848077530122,
   0.98162718344766,
   0.9781476007338,
   0.97437006478523,
   0.97029572627599,
   0.96592582628906,
   0.96126169593831,
   0.95630475596303,
   0.95105651629515,
   0.94551857559931,
   0.9396926207859,
   0.9335804264972,
   0.92718385456678,
   0.92050485345244,
   0.9135454576426,
   0.90630778703665,
   0.89879404629916,
   0.89100652418836,
   0.88294759285892,
   0.87461970713939,
   0.86602540378443,
   0.85716730070211,
   0.84804809615642,
   0.83867056794542,
   0.82903757255504,
   0.81915204428899,
   0.80901699437494,
   0.79863551004729,
   0.78801075360672,
   0.77714596145697,
   0.76604444311897,
   0.75470958022277,
   0.74314482547739,
   0.73135370161917,
   0.71933980033865,
   0.70710678118654,
   0.69465837045899,
   0.68199836006249,
   0.66913060635885,
   0.6560590289905,
   0.64278760968653,
   0.62932039104983,
   0.61566147532565,
   0.60181502315204,
   0.58778525229247,
   0.57357643635104,
   0.55919290347074,
   0.54463903501502,
   0.5299192642332,
   0.51503807491005,
   0.5,
   0.48480962024633,
   0.46947156278589,
   0.45399049973954,
   0.43837114678907,
   0.42261826174069,
   0.4067366430758,
   0.39073112848927,
   0.37460659341591,
   0.3583679495453,
   0.34202014332566,
   0.32556815445715,
   0.30901699437494,
   0.29237170472273,
   0.27563735581699,
   0.25881904510252,
   0.24192189559966,
   0.22495105434386,
   0.20791169081775,
   0.19080899537654,
   0.17364817766693,
   0.15643446504023,
   0.13917310096006,
   0.12186934340514,
   0.10452846326765,
   0.08715574274765,
   0.06975647374412,
   0.05233595624294,
   0.0348994967025,
   0.01745240643728,
   0,
   0.01745240643728,
   0.0348994967025,
   0.05233595624294,
   0.06975647374412,
   0.08715574274765,
   0.10452846326765,
   0.12186934340514,
   0.13917310096006,
   0.15643446504023,
   0.17364817766693,
   0.19080899537654,
   0.20791169081775,
   0.22495105434386,
   0.24192189559966,
   0.25881904510252,
   0.27563735581699,
   0.29237170472273,
   0.30901699437494,
   0.32556815445715,
   0.34202014332566,
   0.3583679495453,
   0.37460659341591,
   0.39073112848927,
   0.4067366430758,
   0.42261826174069,
   0.43837114678907,
   0.45399049973954,
   0.46947156278589,
   0.48480962024633,
   0.5,
   0.51503807491005,
   0.5299192642332,
   0.54463903501502,
   0.55919290347074,
   0.57357643635104,
   0.58778525229247,
   0.60181502315204,
   0.61566147532565,
   0.62932039104983,
   0.64278760968653,
   0.6560590289905,
   0.66913060635885,
   0.68199836006249,
   0.69465837045899,
   0.70710678118654,
   0.71933980033865,
   0.73135370161917,
   0.74314482547739,
   0.75470958022277,
   0.76604444311897,
   0.77714596145697,
   0.78801075360672,
   0.79863551004729,
   0.80901699437494,
   0.81915204428899,
   0.82903757255504,
   0.83867056794542,
   0.84804809615642,
   0.85716730070211,
   0.86602540378443,
   0.87461970713939,
   0.88294759285892,
   0.89100652418836,
   0.89879404629916,
   0.90630778703665,
   0.9135454576426,
   0.92050485345244,
   0.92718385456678,
   0.9335804264972,
   0.9396926207859,
   0.94551857559931,
   0.95105651629515,
   0.95630475596303,
   0.96126169593831,
   0.96592582628906,
   0.97029572627599,
   0.97437006478523,
   0.9781476007338,
   0.98162718344766,
   0.9848077530122,
   0.98768834059513,
   0.99026806874157,
   0.99254615164132,
   0.99452189536827,
   0.99619469809174,
   0.99756405025982,
   0.99862953475457,
   0.99939082701909,
   0.99984769515639,
   1
  );

implementation

uses
{ Delphi }
  Math, SysUtils,
{ Graphics32 }
  GR32_LowLevel;

{ Points }

// get the quadrant of current point ralative to the source point
function GetQuadrant(const ASourcePoint, ATestPoint: TPoint): TgmCoordQuadrant;
begin
  Result := cqNone;

  if (ATestPoint.X > ASourcePoint.X) and
     (ATestPoint.Y < ASourcePoint.Y) then
  begin
    Result := cqFirst;
  end
  else
  if (ATestPoint.X < ASourcePoint.X) and
     (ATestPoint.Y < ASourcePoint.Y) then
  begin
    Result := cqSecond;
  end
  else
  if (ATestPoint.X < ASourcePoint.X) and
     (ATestPoint.Y > ASourcePoint.Y) then
  begin
    Result := cqThird;
  end
  else
  if (ATestPoint.X > ASourcePoint.X) and
     (ATestPoint.Y > ASourcePoint.Y) then
  begin
    Result := cqFourth;
  end;
end;

// calculate the new position in the new region with the same position scale as original
function ScalePoint(const APoint: TPoint;
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer): TPoint;
var
  LXScale, LYScale: Double;
begin
  LXScale  := APoint.X / AOldWidth;
  LYScale  := APoint.Y / AOldHeight;
  Result.X := Round(ANewWidth  * LXScale);
  Result.Y := Round(ANewHeight * LYScale);
end;

// calculating end-point for regular figures (square and circle)
function CalculateRegularFigureEndPoint(
  const AStartPoint, ACurrentPoint: TPoint): TPoint;
var
  LQuadrant   : TgmCoordQuadrant;
  LSubX, LSubY: Integer;
  LMinAbsValue: Integer;
begin
  Result := Point(0, 0);
  
  LQuadrant    := GetQuadrant(AStartPoint, ACurrentPoint);
  LSubX        := Abs(ACurrentPoint.X - AStartPoint.X);
  LSubY        := Abs(ACurrentPoint.Y - AStartPoint.Y);
  LMinAbsValue := MinIntValue([LSubX, LSubY]);

  case LQuadrant of
    cqFirst:
      begin
        Result.X := AStartPoint.X + LMinAbsValue;
        Result.Y := AStartPoint.Y - LMinAbsValue;
      end;

    cqSecond:
      begin
        Result.X := AStartPoint.X - LMinAbsValue;
        Result.Y := AStartPoint.Y - LMinAbsValue;
      end;

    cqThird:
      begin
        Result.X := AStartPoint.X - LMinAbsValue;
        Result.Y := AStartPoint.Y + LMinAbsValue;
      end;

    cqFourth:
      begin
        Result.X := AStartPoint.X + LMinAbsValue;
        Result.Y := AStartPoint.Y + LMinAbsValue;
      end;
  end;
end;

// calculate vertices for regular polygon
procedure CalcRegularPolygonVertices(var APointArray: array of TPoint;
  const ACenterPoint, ACurrentPoint: TPoint; const ASides: Integer);
var
  LRadius       : Extended;   // circumcircle radius of the regular polygon
  LInternalAngle: Extended;   // radians of an internal angle of the regular polygon
  LCurrentAngle : Extended;   // radians of current point relative to center point
  LAccumRadians : Extended;   // accumulated radians
  i, x, y       : Integer;
  LXDistance    : Integer;
  LYDistance    : Integer;
begin
  if High(APointArray) >= (ASides - 1) then
  begin
    // the number of sides of the polygon should be 3..100
    if ASides in [3..100] then
    begin
      LYDistance := ACurrentPoint.Y - ACenterPoint.Y;
      LXDistance := ACurrentPoint.X - ACenterPoint.X;

      // radians of one internal angle of the polygon
      LInternalAngle := 2 * PI / ASides;
      
      // radians of current point relative to center point
      LCurrentAngle := ArcTan2(LYDistance, LXDistance);

      // circumcircle radius of the regular polygon
      LRadius := Sqrt(LXDistance * LXDistance + LYDistance * LYDistance);

      for i := 0 to (ASides - 1) do
      begin
        // accumulated radians
        LAccumRadians := LCurrentAngle + LInternalAngle * i;

        // calculate the vertex
        x := ACenterPoint.X + Round( LRadius * Cos(LAccumRadians) );
        y := ACenterPoint.Y + Round( LRadius * Sin(LAccumRadians) );

        // save the vertex
        APointArray[i] := Point(x, y);
      end;

      // close the polygon
      APointArray[ASides] := APointArray[0];
    end;
  end;
end;

// calculate the line outline vertices
procedure CalcLineOutlineVertices(var APolygon: array of TPoint;
  const ALineStart, ALineEnd: TPoint; const ALineWeight: Integer);
var
  LXDistance, LYDistance        : Integer;
  LAngle, LAccumRadians, LHalfPI: Extended;  // radians
  LDeltaX, LDeltaY              : Extended;                    
begin
  if High(APolygon) = 4 then
  begin
    LXDistance := ALineEnd.X - ALineStart.X;
    LYDistance := ALineEnd.Y - ALineStart.Y;
    LAngle     := ArcTan2(LYDistance, LXDistance);
    LHalfPI    := PI / 2;

    LAccumRadians := LAngle + LHalfPI;
    LDeltaX       := ALineWeight * Cos(LAccumRadians);
    LDeltaY       := ALineWeight * Sin(LAccumRadians);
    APolygon[0].X := Round(ALineStart.X + LDeltaX);
    APolygon[0].Y := Round(ALineStart.Y + LDeltaY);
    APolygon[1].X := Round(ALineEnd.X   + LDeltaX);
    APolygon[1].Y := Round(ALineEnd.Y   + LDeltaY);

    LAccumRadians := LAngle - LHalfPI;
    LDeltaX       := ALineWeight * Cos(LAccumRadians);
    LDeltaY       := ALineWeight * Sin(LAccumRadians);
    APolygon[2].X := Round(ALineEnd.X   + LDeltaX );
    APolygon[2].Y := Round(ALineEnd.Y   + LDeltaY );
    APolygon[3].X := Round(ALineStart.X + LDeltaX );
    APolygon[3].Y := Round(ALineStart.Y + LDeltaY );

    // Connect from start to end.
    APolygon[4] := APolygon[0];
  end;
end;

// determine whether the two coordinates are same
function SameCoordinate(const APoint1, APoint2: TPoint): Boolean;
begin
  Result := (APoint1.X = APoint2.X) and (APoint1.Y = APoint2.Y);
end;

// determine whether the two coordinates are different
function DifferentCoordinate(const APoint1, APoint2: TPoint): Boolean;
begin
  Result := (APoint1.X <> APoint2.X) or (APoint1.Y <> APoint2.Y);
end;

function GetOppositePoint(const ACenterPoint, ACurrentPoint: TPoint): TPoint;
var
  LXDistance, LYDistance: Integer;
  LRadius               : Double;
  LRadian1, LRadian2    : Double;
begin
  LXDistance := ACurrentPoint.X - ACenterPoint.X;
  LYDistance := ACurrentPoint.Y - ACenterPoint.Y;
  LRadius    := Sqrt(LXDistance * LXDistance + LYDistance * LYDistance);
  LRadian1   := ArcTan2(LYDistance, LXDistance);
  LRadian2   := LRadian1 + PI;
  Result.X   := Round( ACenterPoint.X + LRadius * Cos(LRadian2) );
  Result.Y   := Round( ACenterPoint.Y + LRadius * Sin(LRadian2) );
end;

function CalcRadianByTwoPoints(const ACenterPoint, ACurrentPoint: TPoint): Double;
var
  DX, DY: Integer;
begin
  DX     := ACurrentPoint.X - ACenterPoint.X;
  DY     := ACurrentPoint.Y - ACenterPoint.Y;
  Result := ArcTan2(DY, DX);
end;

function CalcLengthByTwoPoints(const APoint1, APoint2: TPoint): Double;
var
  DX, DY: Integer;
begin
  DX     := APoint1.X - APoint2.X;
  DY     := APoint1.Y - APoint2.Y;
  Result := Sqrt(DX * DX + DY * DY);
end;

function CalcOffsetPointByRadian(const ACenterPoint: TPoint;
  const ARadian, ALength: Double): TPoint;
begin
  Result.X := ACenterPoint.X + Round( ALength * Cos(ARadian) );
  Result.Y := ACenterPoint.Y + Round( ALength * Sin(ARadian) );
end;

// get bounding rectangle coordinates of a polygon
function GetPolygonRect(APointArray: array of TPoint; out TL, BR: TPoint): Boolean;
var
  i: Integer;
begin
  Result := False;
  TL.Y   := 10000;
  TL.X   := 10000;
  BR.Y   := -10000;
  BR.X   := -10000;

  if High(APointArray) > 1 then
  begin
    for i := Low(APointArray) to High(APointArray) do
    begin
      TL.Y := MinIntValue([ TL.Y, APointArray[i].Y ]);
      TL.X := MinIntValue([ TL.X, APointArray[i].X ]);
      BR.Y := MaxIntValue([ BR.Y, APointArray[i].Y ]);
      BR.X := MaxIntValue([ BR.X, APointArray[i].X ]);
    end;
    
    Result := True;
  end;
end;

function GetMidPointAtLine(const AStartPoint, AEndPoint: TPoint): TPoint;
begin
  Result.X := (AStartPoint.X + AEndPoint.X) div 2;
  Result.Y := (AStartPoint.Y + AEndPoint.Y) div 2;
end; 

// calculate offset after the canvas size is changed
function CalcOffsetCoordinateByAnchorDirection(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer;
  const AAnchor: TgmAnchorDirection): TPoint;
var
  LIncX, LIncY: Integer; 
begin
  LIncX := ANewWidth  - AOldWidth;
  LIncY := ANewHeight - AOldHeight;

  case AAnchor of
    adTopLeft:
      begin
        Result.X := 0;
        Result.Y := 0;
      end;

    adTop:
      begin
        Result.X := LIncX div 2;
        Result.Y := 0;
      end;

    adTopRight:
      begin
        Result.X := LIncX;
        Result.Y := 0;
      end;

    adLeft:
      begin
        Result.X := 0;
        Result.Y := LIncY div 2;
      end;

    adCenter:
      begin
        Result.X := LIncX div 2;
        Result.Y := LIncY div 2;
      end;

    adRight:
      begin
        Result.X := LIncX;
        Result.Y := LIncY div 2;
      end;

    adBottomLeft:
      begin
        Result.X := 0;
        Result.Y := LIncY;
      end;

    adBottom:
      begin
        Result.X := LIncX div 2;
        Result.Y := LIncY;
      end;
      
    adBottomRight:
      begin
        Result.X := LIncX;
        Result.Y := LIncY;
      end;
  end;
end;

// Enlightened by how to draw a pentagram :
// http://en.wikipedia.org/wiki/Pentagram
function GetStarPoints(const ACenterPoint, AEndingPoint: TPoint;
  const AVertexCount: Integer = 5): TArrayOfPoint;
const
  // For drawing a Pentagram, we actually need to get every vertex of a
  // decagon. But to get these vertices, we are not using a single radius,
  // but with two radii alternatively, one is longer, and the another is
  // shorter.
  //
  // The longer radius is easy to calculate, that is:
  //
  //   Long Radius = Sqrt( Sqr(EndingPoint.X - CenterPoint.X) +
  //                       Sqr(EndingPoint.Y - CenterPoint.Y)
  //
  // The question is how to calculate for the shorter radius.
  // According to the explanation about Pentagram from Wikipedia.com,
  // We aware of that to do this, we need to get the length of the magenta
  // line of the Pentagram. And for getting its length, we should get the
  // length of the red line for the Pentagram, first. That is:
  //
  //   Red Line = 2 * Sin(2 * Inner Radians) * Longer Radius
  //
  // The lines of a Pentagram complys with the Golden Ratio.
  // The Golden Radio Phi equals to 2 * Cos36, and the relationship
  // among these lines are:
  //
  //   Red/Green = Green/Blue = Blue/Magenta = Phi
  //
  // So:
  //
  //   Magenta Line = Red Line / Phi^3 = Red Line * (1 / Phi^3)
  //
  // where Phi^3 means to raise Phi to power 3.
  //
  // and 1/(Phi^3) equals to 0.236 approximately, so we got the following
  // constant.
  //
  // After we got the length of the Magenta Line, then, we could get the
  // the length of the shorter radius:
  //
  // Short Radius := Magenta Line * 0.5 / Sin(Inner Radians);

  INNER_LINE_SCALE      = 0.236;
  MIN_STAR_VERTEX_COUNT = 3;
  MAX_STAR_VERTEX_COUNT = 100;
var
  LPoints      : TArrayOfPoint;
  LInnerRad    : Double;
  LCurRad      : Double;
  LAccumRad    : Double;
  LLargeRadius : Double;
  LSmallRadius : Double;
  LDiagonal    : Double;
  LInnerLine   : Double;
  i, dx, dy    : Integer;
  LVertexCount : Integer;
begin
  if AVertexCount < 3 then
  begin
    Exit;
  end;

  LVertexCount := AVertexCount * 2;
  
  SetLength(LPoints, LVertexCount + 1);

  dx           := AEndingPoint.X - ACenterPoint.X;
  dy           := AEndingPoint.Y - ACenterPoint.Y;
  LLargeRadius := Sqrt( (dx * dx) + (dy * dy) );
  LInnerRad    := 2 * PI / LVertexCount;

  if dx = 0 then
  begin
    if dy >= 0 then
    begin
      LCurRad := PI / 2;
    end
    else
    begin
      LCurRad := -PI / 2;
    end;
  end
  else
  begin
    LCurRad := ArcTan2(dy, dx);
  end;

  LDiagonal    := 2 * Sin(2 * LInnerRad) * LLargeRadius;  // the red line
  LInnerLine   := LDiagonal * INNER_LINE_SCALE;           // the magenta line 
  LSmallRadius := LInnerLine * 0.5 / Sin(LInnerRad);

  LAccumRad  := LCurRad + LInnerRad;
  LPoints[0] := AEndingPoint;
  for i := 1 to (LVertexCount - 1) do
  begin
    if (i mod 2) = 0 then
    begin
      LPoints[i].X := Round( LLargeRadius * Cos(LAccumRad) + ACenterPoint.X );
      LPoints[i].Y := Round( LLargeRadius * Sin(LAccumRad) + ACenterPoint.Y ); 
    end
    else
    begin
      LPoints[i].X := Round( LSmallRadius * Cos(LAccumRad) + ACenterPoint.X );
      LPoints[i].Y := Round( LSmallRadius * Sin(LAccumRad) + ACenterPoint.Y );
    end;

    LAccumRad := LAccumRad + LInnerRad;
  end;

  LPoints[LVertexCount] := LPoints[0];

  Result := LPoints;
end;

{ Lines }

function IfInLine(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;
const
  FUZZINESS = 2;
var
  K, B              : Double;
  fx1, fy1, fx2, fy2: Integer;
begin
  if x1 < x2 then
  begin
    fx1 := x1;
    fx2 := x2;
  end
  else
  begin
    fx1 := x2;
    fx2 := x1;
  end;

  if y1 < y2 then
  begin
    fy1 := y1;
    fy2 := y2;
  end
  else
  begin
    fy1 := y2;
    fy2 := y1;
  end;

  try
    if x1 = x2 then
    begin
      if (X >= (X1 - 1)) and (X <= (X1 + 1)) and (Y >= Y1) and (Y <= Y2) then
      begin
        Result := True;
      end
      else
      begin
        Result := False;
      end;

      Exit;
    end
    else
    begin
      K := (Y1 - Y2) / (X1 - X2);
    end;
    
  except
    Result := False;
    Exit;
  end;

  B := Y1 - K * X1;
  
  if Abs((K * X + B) - Y) <= FUZZINESS then
  begin
    if (X >= fx1) and (X <= fx2) and (Y >= fy1) and (Y <= fy2) then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function PointOnCurve(const FX1, FY1, FX2, FY2: Integer;
  const FX3, FY3, FX4, FY4: Integer; const pt: TPoint): Boolean;
var
  ps, pc         : TPoint;
  i, j, k, LCount: Integer;
  t              : Double;
  cf             : array [1..4] of TPoint;
begin
  Result := False;
  LCount := 50;  // change this value could change precision

  // calculate the curve points with Casteljau algorithm
  cf[1].x := FX1;
  cf[1].y := FY1;
  cf[2].x := FX2;
  cf[2].y := FY2;
  cf[3].x := FX3;
  cf[3].y := FY3;
  cf[4].x := FX4;
  cf[4].y := FY4;

  for i := 1 to LCount do
  begin
    t := (i - 1) / LCount;

    for j := 1 to 3 do
    begin
      for k := 1 to (4 - j) do
      begin
        cf[k].x := Round((1 - t) * cf[k].x + t * cf[k + 1].x);
        cf[k].y := Round((1 - t) * cf[k].y + t * cf[k + 1].y);
      end;
    end;

    pc.x := Round(cf[1].x);
    pc.y := Round(cf[1].y);

    if i = 1 then
    begin
      ps := Point(pc.x, pc.y);
    end;

    if IfInLine(ps.x, ps.y, pc.x, pc.y, pt.x, pt.y) then
    begin
      Result := True;
      Exit;
    end;

    ps := Point(pc.x, pc.y);
  end;
end;

// calculate bounding rectangle coordinates of a curve
procedure CalcCurvePoint(const FX1, FY1, FX2, FY2: Integer;
  const FX3, FY3, FX4, FY4: Integer; var FP1, FP2: TPoint);
var
  ps, pc         : TPoint;
  i, j, k, LCount: Integer;
  t              : Double;
  cf             : array [1..4] of TPoint;
begin
  LCount:= 5;  // change this value could change precision

  // calculate the curve points with Casteljau algorithm
  cf[1].x := FX1;
  cf[1].y := FY1;
  cf[2].x := FX2;
  cf[2].y := FY2;
  cf[3].x := FX3;
  cf[3].y := FY3;
  cf[4].x := FX4;
  cf[4].y := FY4;
  Fp1     := Point(9999, 9999);
  Fp2     := Point(0, 0);

  for i := 1 to LCount do
  begin
    t := (i - 1) / LCount;

    for j := 1 to 3 do
    begin
      for k := 1 to (4 - j) do
      begin
        cf[k].x := Round((1 - t) * cf[k].x + t * cf[k + 1].x);
        cf[k].y := Round((1 - t) * cf[k].y + t * cf[k + 1].y);
      end;
    end;

    pc.x := Round(cf[1].x);
    pc.y := Round(cf[1].y);

    if i = 1 then
    begin
      ps := Point(pc.x, pc.y);
    end;

    if pc.x < fp1.x then
    begin
      fp1.x := pc.x;
    end;

    if ps.x < fp1.x then
    begin
      fp1.x := ps.x;
    end;

    if pc.y < fp1.y then
    begin
      fp1.y := pc.y;
    end;

    if ps.y < fp1.y then
    begin
      fp1.y := ps.y;
    end;

    if pc.x > fp2.x then
    begin
      fp2.x := pc.x;
    end;

    if ps.x > fp2.x then
    begin
      fp2.x := ps.x;
    end;

    if pc.y > fp2.y then
    begin
      fp2.y := pc.y;
    end;

    if ps.y > fp2.y then
    begin
      fp2.y := ps.y;
    end;

    ps := Point(pc.x, pc.y);
  end;
end;

function IfInRectangle(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;
var
  fx1, fy1, fx2, fy2: Integer;
begin
  if x1 < x2 then
  begin
    fx1 := x1;
    fx2 := x2;
  end
  else
  begin
    fx1 := x2;
    fx2 := x1;
  end;

  if y1 < y2 then
  begin
    fy1 := y1;
    fy2 := y2;
  end
  else
  begin
    fy1 := y2;
    fy2 := y1;
  end;
  
  if (Y = FY1) or (Y = FY2) then
  begin
    if (X >= fx1) and (X <= fx2) then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  if (X = FX1) or (X = FX2) then
  begin
    if (Y >= fy1) and (Y <= fy2) then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function IfInEllipse(const X1, Y1, X2, Y2, X, Y: Integer): Boolean;
const
  FUZZINESS = 0.1;
  
var
  TX1, TY1, TX2, TY2: Integer;
  A, B, C           : Double;
begin
  if x1 < x2 then
  begin
    TX1 := X1;
    TY1 := -Y1;
    TX2 := X2;
    TY2 := -Y2;
  end
  else
  begin
    TX1 := X2;
    TY1 := -Y2;
    TX2 := X1;
    TY2 := -Y1;
  end;

  A := (TX2 - TX1) / 2;
  B := (TY2 - TY1) / 2;
  try
    C := ( (X  - A - TX1) * (X  - A - TX1) ) / (A * A) +
         ( (-Y - B - TY1) * (-Y - B - TY1) ) / (B * B);
  except
    Result := False;
    Exit;
  end;
  
  Result := ( (Frac(C) <= FUZZINESS ) and ( Int(C) = 1) );
end;

{ Angle }

// Get an acute angle from -90 to 90 degrees
procedure GetAcuteAngle(const ACenterPoint, ACurrentPoint: TPoint;
  var AAngle: Integer);
var
  X, Y, LTempAngle    : Integer;
  LHypotenuse, LRadian: Double;
  LQuadrant           : TgmCoordQuadrant;
begin
  X := Abs(ACurrentPoint.X - ACenterPoint.X);
  Y := Abs(ACurrentPoint.Y - ACenterPoint.Y);

  if (X <> 0) or (Y <> 0) then
  begin
    LHypotenuse := Sqrt(X * X + Y * Y);

    if X >= Y then
    begin
      LRadian := ArcCos(X / LHypotenuse);
    end
    else
    begin
      LRadian := ArcSin(Y / LHypotenuse);
    end;

    LTempAngle := Round( RadToDeg(LRadian) );

    if (LTempAngle > 0) and (LTempAngle < 90) then
    begin
      LQuadrant := GetQuadrant(ACenterPoint, ACurrentPoint);

      case LQuadrant of
        cqFirst, cqThird:
          begin
            AAngle := LTempAngle;
          end;

        cqSecond, cqFourth:
          begin
            AAngle := -LTempAngle;
          end;
      end;
    end
    else if LTempAngle = 90 then
    begin
      if ACurrentPoint.Y < ACenterPoint.Y then
      begin
        AAngle := LTempAngle;
      end
      else
      begin
        AAngle := -LTempAngle;
      end;
    end
    else
    begin
      AAngle := LTempAngle;
    end;
  end;
end;

{ Rectangle }

function AddRects(const ARect1, ARect2: TRect): TRect;
begin
  Result.Left   := Min(ARect1.Left,   ARect2.Left);
  Result.Top    := Min(ARect1.Top,    ARect2.Top);
  Result.Right  := Max(ARect1.Right,  ARect2.Right);
  Result.Bottom := Max(ARect1.Bottom, ARect2.Bottom);
end;

function GetRectSize(const ARect: TRect): TSize;
begin
  Result.cx := ARect.Right - ARect.Left + 1;
  Result.cy := ARect.Bottom - ARect.Top + 1;
end;


{ Range Calculation }

procedure EnsureValueInRange(var ACheckValue: Integer;
  const AMinimumValue, AMaximumValue: Integer);
begin
  if ACheckValue < AMinimumValue then
  begin
    ACheckValue := AMinimumValue;
  end
  else if ACheckValue > AMaximumValue then
  begin
    ACheckValue := AMaximumValue;
  end;
end;

procedure EnsureValueInRange(var ACheckValue: Double;
  const AMinimumValue, AMaximumValue: Double);
begin
  if ACheckValue < AMinimumValue then
  begin
    ACheckValue := AMinimumValue;
  end
  else if ACheckValue > AMaximumValue then
  begin
    ACheckValue := AMaximumValue;
  end;
end;

{ Used for PrintOptions in order to get the bitmap size to fit the
  specified size by scale as possible. }
function GetScaleToFitMedia(
  const AOldWidth, AOldHeight, AMediaWidth, AMediaHeight: Integer): Double;
var
  LAspectRatio, LWidthRatio, LHeightRatio: Double;
  LNewWidth, LNewHeight                  : Integer;
begin
  LNewWidth    := 0;
  LNewHeight   := 0;
  LAspectRatio := AOldWidth / AOldHeight;

  if ( (AOldWidth < AMediaWidth) and (AOldHeight < AMediaHeight) ) or
     ( (AOldWidth > AMediaWidth) and (AOldHeight > AMediaHeight) ) then
  begin
    LWidthRatio  := AOldWidth  / AMediaWidth;
    LHeightRatio := AOldHeight / AMediaHeight;

    if LWidthRatio < LHeightRatio then
    begin
      LNewHeight := AMediaHeight;
      LNewWidth  := Trunc(LNewHeight * LAspectRatio);
    end
    else
    begin
      LNewWidth  := AMediaWidth;
      LNewHeight := Trunc(LNewWidth / LAspectRatio);
    end;
  end
  else
  if (AOldWidth  >= AMediaWidth) and
     (AOldHeight <= AMediaHeight) then
  begin
    LNewWidth  := AMediaWidth;
    LNewHeight := Trunc(LNewWidth / LAspectRatio);
  end
  else
  if (AOldWidth  <= AMediaWidth) and
     (AOldHeight >= AMediaHeight) then
  begin
    LNewHeight := AMediaHeight;
    LNewWidth  := Trunc(LNewHeight * LAspectRatio);
  end;

  Result := (LNewWidth / AOldWidth + LNewHeight / AOldHeight) / 2;
end;

{ Arithmic }

{ The following routines were found at:
  http://delphi.about.com/od/mathematics/a/baseconvert.htm }

function HexToInt(AHexNum: string): LongInt;
begin
  Result := StrToInt('$' + AHexNum);
end;

function IntToBin(AValue: LongInt; ADigits: Integer): string;
begin
  Result := StringOfChar('0', ADigits);

  while AValue > 0 do
  begin
    if (AValue and 1) = 1 then
    begin
      Result[ADigits] := '1';
    end;

    Dec(ADigits);
    AValue := AValue shr 1;
  end;
end;

function BinToInt(AValue: string): LongInt;
var
  i: Integer;
begin
  Result := 0;

  // remove leading zeroes
  while Copy(AValue, 1, 1) = '0' do
  begin
    AValue := Copy( AValue, 2, Length(AValue) - 1 );
  end;

  // do the conversion
  for i := Length(AValue) downto 1 do
  begin
    if Copy(AValue, i, 1) = '1' then
    begin
      Result := Result + ( 1 shl (Length(AValue) - i) );
    end;
  end;
end;

// adapted from x2nie's code
procedure QuickSort(AIntArray: TArrayOfInteger; L, R: Integer);
var
  I, J, M, T : Integer;
begin
  if Length(AIntArray) <= 0 then
  begin
    Exit;
  end;

  repeat
    I := L;
    J := R;
    M := AIntArray[(L + R) shr 1];
    
    repeat
      while AIntArray[I] < M do
      begin
        Inc(I);
      end;
      
      while AIntArray[J] > M do
      begin
        Dec(J);
      end;

      if I <= J then
      begin
        // exchange I & J
        T            := AIntArray[I];
        AIntArray[I] := AIntArray[J];
        AIntArray[J] := T;

        Inc(I);
        Dec(J);
      end;
      
    until I > J;
    
    if L < J then
    begin
      QuickSort(AIntArray, L, J);
    end;

    L := I;
  until I >= R;
end;

end.
