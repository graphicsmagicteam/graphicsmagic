unit gmMeasure;

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
 * Last Update: January 19th, 2015
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
{ Delphi }
  Windows, Classes, Graphics,
{ GraphicsMagic Lib }
  gmTypes;

type
  // indicating which point of the measure line you want to modify
  TgmMeasurePointSelector = (mpsNone, mpsFirst, mpsSecond, mpsThird);

  // Measure Unit
  TgmMeasureUnit = (muPixel, muInch, muCM);

  TgmMeasureLine = class(TObject)
  private
    FMeasurePoints    : array of TPoint; // measure points for measure line

    { Calculation }
    FOriginalIntX     : Integer;
    FOriginalIntY     : Integer;
    FIntWidth         : Integer;
    FIntHeight        : Integer;
    FIntDistance1     : Integer;
    FIntDistance2     : Integer;

    FOriginalFloatX   : Extended;
    FOriginalFloatY   : Extended;
    FFloatWidth       : Extended;
    FFloatHeight      : Extended;
    FFloatDistance1   : Extended;
    FFloatDistance2   : Extended;

    FMeasureAngle     : Extended;
    
    FLine1Color       : TColor;
    FLine2Color       : TColor;
    FHandleLineColor1 : TColor;
    FHandleLineColor2 : TColor;
    FConnectorColor   : TColor;

    function GetMeasurePointCount: Integer;

    procedure DrawHandles(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
  public
    constructor Create;
    destructor Destroy; override;

    // change coordnate for the selected Measure point
    procedure SetMeasurePoint(const AX, AY: Integer;
      const AMeasurePointSelector: TgmMeasurePointSelector);

    // add third Measure point for measure line
    procedure AddThirdMeasurePoint(const AX, AY: Integer);
    procedure Translate(const ATranslateVector: TPoint);
    procedure SwapFirstAndSecondMeasurePoint;

    procedure Draw(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil);

    // calculate the measure result
    procedure Calculate(const AUnit: TgmMeasureUnit; const APixelsPerInch: Integer);

    // check whether the mouse is over on one of the Measure points
    function GetHandleAtPoint(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmMeasurePointSelector;

    // check whether the mouse is over on the Measure line
    function ContainsPoint(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean; overload;

    property LineCount        : Integer  read GetMeasurePointCount;
    property OriginalIntX     : Integer  read FOriginalIntX;
    property OriginalIntY     : Integer  read FOriginalIntY;
    property IntWidth         : Integer  read FIntWidth;
    property IntHeight        : Integer  read FIntWidth;
    property IntDistance1     : Integer  read FIntDistance1;
    property IntDistance2     : Integer  read FIntDistance2;
    property OriginalFloatX   : Extended read FOriginalFloatX;
    property OriginalFloatY   : Extended read FOriginalFloatY;
    property FloatWidth       : Extended read FFloatWidth;
    property FloatHeight      : Extended read FFloatWidth;
    property FloatDistance1   : Extended read FFloatDistance1;
    property FloatDistance2   : Extended read FFloatDistance2;
    property MeasureAngle     : Extended read FMeasureAngle;
    property Line1Color       : TColor   read FLine1Color       write FLine1Color;
    property Line2Color       : TColor   read FLine2Color       write FLine2Color;
    property HandleLineColor1 : TColor   read FHandleLineColor1 write FHandleLineColor1;
    property HandleLineColor2 : TColor   read FHandleLineColor2 write FHandleLineColor2;
    property ConnectorColor   : TColor   read FConnectorColor   write FConnectorColor;
  end;

implementation

uses
{ Standard }
  Math,
{ externals }
  LineLibrary,
{ GraphicsMagic Lib }
  gmConstants,
  gmCommonFuncs;


{ Measuring }

procedure CalculateMeasureByCM(const AMeasurePolygon: array of TPoint;
  const PixelsPerInch: Integer;
  var OriginalX, OriginalY, AWidth, AHeight, D1, D2, Angle: Extended);
var
  LTempWidth  : Extended;
  LTempHeight : Extended;
  LRadians1   : Extended;
  LRadians2   : Extended;
  LAngle1     : Extended;
  LAngle2     : Extended;
  LMaxAngle   : Extended;
  LMinAngle   : Extended;
begin
  if High(AMeasurePolygon) > 0 then
  begin
    OriginalX := AMeasurePolygon[0].X / PixelsPerInch * 2.54;
    OriginalY := AMeasurePolygon[0].Y / PixelsPerInch * 2.54;

    if High(AMeasurePolygon) = 1 then
    begin
      AWidth  := (AMeasurePolygon[1].X - AMeasurePolygon[0].X) / PixelsPerInch * 2.54;
      AHeight := (AMeasurePolygon[1].Y - AMeasurePolygon[0].Y) / PixelsPerInch * 2.54;
      D1      := Sqrt(AWidth * AWidth + AHeight * AHeight);

      // The second point is in the first quadrant.
      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the second quadrant.
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 180 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the third quadrant.
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 0 - ( 180 - RadToDeg(LRadians1) );
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the forth quadrant.
      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 0 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end;
    end
    else
    if High(AMeasurePolygon) = 2 then
    begin
      AWidth      := (AMeasurePolygon[1].X - AMeasurePolygon[0].X) / PixelsPerInch * 2.54;
      AHeight     := (AMeasurePolygon[1].Y - AMeasurePolygon[0].Y) / PixelsPerInch * 2.54;
      D1          := Sqrt(AWidth * AWidth + AHeight * AHeight);
      LTempWidth  := (AMeasurePolygon[2].X - AMeasurePolygon[1].X) / PixelsPerInch * 2.54;
      LTempHeight := (AMeasurePolygon[2].Y - AMeasurePolygon[1].Y) / PixelsPerInch * 2.54;
      D2          := Sqrt(LTempWidth * LTempWidth + LTempHeight * LTempHeight);

      if D1 <> 0.0 then
      begin
        LRadians1 := ArcSin( Abs(AHeight) / D1 );
        LAngle1   := RadToDeg(LRadians1);
      end
      else
      begin
        LAngle1 := 0.0;
      end;

      if D2 <> 0.0 then
      begin
        LRadians2 := ArcSin( Abs(LTempHeight) / D2 );
        LAngle2   := RadToDeg(LRadians2);
      end
      else
      begin
        LAngle2 := 0.0;
      end;
      
      // The two points are both in the first, second, third or forth quadrant.
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := Abs(LAngle1 - LAngle2);
      end
      else
      { One point is in the first quadrant, and another is in the second quadrant,
        or, one point is in the third quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := 180 - LAngle1 - LAngle2;
      end
      else
      { One point is in the second quadrant, and another is in the third quadrant,
        or, one point is in the first quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        Angle := LAngle2 + LAngle1;
      end
      else
      { One point is in the first quadrant, and another is in the third quadrant,
        or, one point is in the second quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        LMaxAngle := Max(LAngle1, LAngle2);
        LMinAngle := Min(LAngle1, LAngle2);
        Angle     := 180 - LMaxAngle + LMinAngle;
      end;
    end;
  end;
end;

procedure CalculateMeasureByInches(const AMeasurePolygon: array of TPoint;
  const PixelsPerInch: Integer;
  var OriginalX, OriginalY, AWidth, AHeight, D1, D2, Angle: Extended);
var
  LTempWidth  : Extended;
  LTempHeight : Extended;
  LRadians1   : Extended;
  LRadians2   : Extended;
  LAngle1     : Extended;
  LAngle2     : Extended;
  LMaxAngle   : Extended;
  LMinAngle   : Extended;
begin
  if High(AMeasurePolygon) > 0 then
  begin
    OriginalX := AMeasurePolygon[0].X / PixelsPerInch;
    OriginalY := AMeasurePolygon[0].Y / PixelsPerInch;

    if High(AMeasurePolygon) = 1 then
    begin
      AWidth  := (AMeasurePolygon[1].X - AMeasurePolygon[0].X) / PixelsPerInch;
      AHeight := (AMeasurePolygon[1].Y - AMeasurePolygon[0].Y) / PixelsPerInch;
      D1      := Sqrt(AWidth * AWidth + AHeight * AHeight);

      // The second point is in the first quadrant.
      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the second quadrant.
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 180 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the third quadrant.
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 0 - ( 180 - RadToDeg(LRadians1) );
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // The second point is in the forth quadrant.
      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if D1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / D1 );
          Angle     := 0 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end;
    end
    else
    if High(AMeasurePolygon) = 2 then
    begin
      AWidth      := (AMeasurePolygon[1].X - AMeasurePolygon[0].X) / PixelsPerInch;
      AHeight     := (AMeasurePolygon[1].Y - AMeasurePolygon[0].Y) / PixelsPerInch;
      D1          := Sqrt(AWidth * AWidth + AHeight * AHeight);
      LTempWidth  := (AMeasurePolygon[2].X - AMeasurePolygon[1].X) / PixelsPerInch;
      LTempHeight := (AMeasurePolygon[2].Y - AMeasurePolygon[1].Y) / PixelsPerInch;
      D2          := Sqrt(LTempWidth * LTempWidth + LTempHeight * LTempHeight);

      if D1 <> 0.0 then
      begin
        LRadians1 := ArcSin( Abs(AHeight) / D1 );
        LAngle1   := RadToDeg(LRadians1);
      end
      else
      begin
        LAngle1 := 0.0;
      end;

      if D2 <> 0.0 then
      begin
        LRadians2 := ArcSin( Abs(LTempHeight) / D2 );
        LAngle2   := RadToDeg(LRadians2);
      end
      else
      begin
        LAngle2 := 0.0;
      end;
      
      // The two points are both in the first, second, third or forth quadrant.
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := Abs(LAngle1 - LAngle2);
      end
      else
      { One point is in the first quadrant, and another is in the second quadrant,
        or, one point is in the third quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := 180 - LAngle1 - LAngle2;
      end
      else
      { One point is in the second quadrant, and another is in the third quadrant,
        or, one point is in the first quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        Angle := LAngle2 + LAngle1;
      end
      else
      { One point is in the first quadrant, and another is in the third quadrant,
        or, one point is in the second quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        LMaxAngle := Max(LAngle1, LAngle2);
        LMinAngle := Min(LAngle1, LAngle2);
        Angle     := 180 - LMaxAngle + LMinAngle;
      end;
    end;
  end;
end; 

procedure CalculateMeasureByPixels(const AMeasurePolygon: array of TPoint;
  var OriginalX, OriginalY, AWidth, AHeight, D1, D2: Integer;
  var Angle: Extended);
var
  LTempWidth   : Integer;
  LTempHeight  : Integer;
  LRadians1    : Extended;
  LRadians2    : Extended;
  LHypotenuse1 : Extended;
  LHypotenuse2 : Extended;
  LAngle1      : Extended;
  LAngle2      : Extended;
  LMaxAngle    : Extended;
  LMinAngle    : Extended;
begin
  if High(AMeasurePolygon) > 0 then
  begin
    OriginalX := AMeasurePolygon[0].X;
    OriginalY := AMeasurePolygon[0].Y;

    if High(AMeasurePolygon) = 1 then
    begin
      AWidth       := AMeasurePolygon[1].X - AMeasurePolygon[0].X;
      AHeight      := AMeasurePolygon[1].Y - AMeasurePolygon[0].Y;
      D1           := Round( Sqrt( AWidth * AWidth + AHeight * AHeight) );
      LHypotenuse1 := Sqrt( AWidth * AWidth + AHeight * AHeight);

      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        // the second point is in the first quadrant
        if LHypotenuse1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / LHypotenuse1 );
          Angle     := RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // the second point is in the second quadrant
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y <= AMeasurePolygon[0].Y) then
      begin
        if LHypotenuse1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / LHypotenuse1 );
          Angle     := 180 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // the second point is in the third quadrant
      if (AMeasurePolygon[1].X <= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if LHypotenuse1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / LHypotenuse1 );
          Angle     := 0 - ( 180 - RadToDeg(LRadians1) );
        end
        else
        begin
          Angle := 0.0;
        end;
      end
      else
      // the second point is in the forth quadrant
      if (AMeasurePolygon[1].X >= AMeasurePolygon[0].X) and
         (AMeasurePolygon[1].Y >= AMeasurePolygon[0].Y) then
      begin
        if LHypotenuse1 <> 0.0 then
        begin
          LRadians1 := ArcSin( Abs(AHeight) / LHypotenuse1 );
          Angle     := 0 - RadToDeg(LRadians1);
        end
        else
        begin
          Angle := 0.0;
        end;
      end;
    end
    else
    if High(AMeasurePolygon) = 2 then
    begin
      AWidth       := AMeasurePolygon[1].X - AMeasurePolygon[0].X;
      AHeight      := AMeasurePolygon[1].Y - AMeasurePolygon[0].Y;
      D1           := Round( Sqrt( AWidth * AWidth + AHeight * AHeight) );
      LHypotenuse1 := Sqrt( AWidth * AWidth + AHeight * AHeight);
      LTempWidth   := AMeasurePolygon[2].X - AMeasurePolygon[1].X;
      LTempHeight  := AMeasurePolygon[2].Y - AMeasurePolygon[1].Y;
      D2           := Round( Sqrt( LTempWidth * LTempWidth + LTempHeight * LTempHeight) );
      LHypotenuse2 := Sqrt( LTempWidth * LTempWidth + LTempHeight * LTempHeight);

      if LHypotenuse1 <> 0.0 then
      begin
        LRadians1 := ArcSin( Abs(AHeight) / LHypotenuse1 );
        LAngle1   := RadToDeg(LRadians1);
      end
      else
      begin
        LAngle1 := 0.0;
      end;

      if LHypotenuse2 <> 0.0 then
      begin
        LRadians2 := ArcSin( Abs(LTempHeight) / LHypotenuse2 );
        LAngle2   := RadToDeg(LRadians2);
      end
      else
      begin
        LAngle2 := 0.0;
      end;
      
      // The two points are both in the first, second, third or forth quadrant.
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := Abs(LAngle1 - LAngle2);
      end
      else
      { One point is in the first quadrant, and another is in the second quadrant,
        or, one point is in the third quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) ) then
      begin
        Angle := 180 - LAngle1 - LAngle2;
      end
      else
      { One point is in the second quadrant, and another is in the third quadrant,
        or, one point is in the first quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        Angle := LAngle2 + LAngle1;
      end
      else
      { One point is in the first quadrant, and another is in the third quadrant,
        or, one point is in the second quadrant and another is in the forth quadrant. }
      if ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y <= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y >= AMeasurePolygon[1].Y) )
      or ( (AMeasurePolygon[0].X >= AMeasurePolygon[1].X) and
           (AMeasurePolygon[0].Y >= AMeasurePolygon[1].Y) and
           (AMeasurePolygon[2].X <= AMeasurePolygon[1].X) and
           (AMeasurePolygon[2].Y <= AMeasurePolygon[1].Y) ) then
      begin
        LMaxAngle := Max(LAngle1, LAngle2);
        LMinAngle := Min(LAngle1, LAngle2);
        Angle     := 180 - LMaxAngle + LMinAngle;
      end;
    end;
  end;
end;

{ Drawing }

procedure DrawSpider(const ACanvas: TCanvas; const APoint: TPoint;
  const ARadius: Integer; const AColor1, AColor2: TColor;
  const APenMode: TPenMode);
var
  LTempPenColor : TColor;
  LTempPenMode  : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenColor := Pen.Color;
    LTempPenMode  := Pen.Mode;

    Pen.Mode  := APenMode;

    Pen.Color := AColor1;
    MoveTo(APoint.X - ARadius + 1, APoint.Y);
    LineTo(APoint.X + ARadius, APoint.Y);

    Pen.Color := AColor2;
    MoveTo(APoint.X, APoint.Y - ARadius + 1);
    LineTo(APoint.X, APoint.Y + ARadius);

    Pen.Color := LTempPenColor;
    Pen.Mode  := LTempPenMode;
  end;
end;

{ TgmMeasureLine }

constructor TgmMeasureLine.Create;
begin
  inherited Create;

  // Create two measure points in initialization.
  SetLength(FMeasurePoints, 2);

{ Calculation }
  FOriginalIntX     := 0;
  FOriginalIntY     := 0;
  FIntWidth         := 0;
  FIntHeight        := 0;
  FIntDistance1     := 0;
  FIntDistance2     := 0;

  FOriginalFloatX   := 0;
  FOriginalFloatY   := 0;
  FFloatWidth       := 0;
  FFloatHeight      := 0;
  FFloatDistance1   := 0;
  FFloatDistance2   := 0;

  FMeasureAngle     := 0;

  FLine1Color       := clRed;
  FLine2Color       := clGreen;
  FHandleLineColor1 := clBlue;
  FHandleLineColor2 := clYellow;
  FConnectorColor   := clAqua;
end; 

destructor TgmMeasureLine.Destroy;
begin
  SetLength(FMeasurePoints, 0);
  inherited Destroy;
end;

procedure TgmMeasureLine.AddThirdMeasurePoint(const AX, AY: Integer);
begin
  if High(FMeasurePoints) = 1 then
  begin
    SetLength(FMeasurePoints, 3);
    FMeasurePoints[2] := Point(AX, AY);
  end;
end;

// Calculate the measure result.
procedure TgmMeasureLine.Calculate(const AUnit: TgmMeasureUnit;
  const APixelsPerInch: Integer);
begin
  case AUnit of
    muPixel:
      begin
        CalculateMeasureByPixels(FMeasurePoints, FOriginalIntX, FOriginalIntY,
                                 FIntWidth, FIntHeight, FIntDistance1,
                                 FIntDistance2, FMeasureAngle);
      end;

    muInch:
      begin
        CalculateMeasureByInches(FMeasurePoints, APixelsPerInch,
                                 FOriginalFloatX, FOriginalFloatY, FFloatWidth,
                                 FFloatHeight, FFloatDistance1, FFloatDistance2,
                                 FMeasureAngle);
      end;
                                      
    muCM:
      begin
        CalculateMeasureByCM(FMeasurePoints, APixelsPerInch,
                             FOriginalFloatX, FOriginalFloatY, FFloatWidth,
                             FFloatHeight, FFloatDistance1, FFloatDistance2,
                             FMeasureAngle);
      end;
  end;
end; 

// Checking for whether or not the mouse is over the Measure line.
function TgmMeasureLine.ContainsPoint(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  i           : Integer;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  Result := False;

  if Length(FMeasurePoints) > 0 then
  begin
    for i := 0 to High(FMeasurePoints) - 1 do
    begin
      if Assigned(ACoordConvertFunc) then
      begin
        LStartPoint := ACoordConvertFunc(FMeasurePoints[i]);
        LEndPoint   := ACoordConvertFunc(FMeasurePoints[i + 1]);
      end
      else
      begin
        LStartPoint := FMeasurePoints[i];
        LEndPoint   := FMeasurePoints[i + 1];
      end;

      if NearLine( Point(AX, AY), LStartPoint, LEndPoint ) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TgmMeasureLine.Draw(ACanvas: TCanvas; const APenMode: TPenMode;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LTempPenColor          : TColor;
  LTempPenWidth          : Integer;
  LTempPenStyle          : TPenStyle;
  LTempPenMode           : TPenMode;
  LLineStartP, LLineEndP : TPoint;
  LMeasurePointCount     : Integer;
begin
  with ACanvas do
  begin
    // Copy the original properties of the Canvas.Pen .
    LTempPenColor := Pen.Color;
    LTempPenWidth := Pen.Width;
    LTempPenStyle := Pen.Style;
    LTempPenMode  := Pen.Mode;

    // Change the propertis of the pen of the Canvas.
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Mode  := APenMode;

    LMeasurePointCount := Length(FMeasurePoints);

    // Draw measure lines.
    if LMeasurePointCount >= 2 then
    begin
      // Draw first measure line.

      Pen.Color := FLine1Color;

      if Assigned(ACoordConvertFunc) then
      begin
        LLineStartP := ACoordConvertFunc(FMeasurePoints[0]);
        LLineEndP   := ACoordConvertFunc(FMeasurePoints[1]);
      end
      else
      begin
        LLineStartP := FMeasurePoints[0];
        LLineEndP   := FMeasurePoints[1];
      end;

      MoveTo(LLineStartP.X, LLineStartP.Y);
      LineTo(LLineEndP.X, LLineEndP.Y);

      // Draw second measure line.

      Pen.Color := FLine2Color;

      if LMeasurePointCount = 3 then
      begin
        if Assigned(ACoordConvertFunc) then
        begin
          LLineStartP := ACoordConvertFunc(FMeasurePoints[1]);
          LLineEndP   := ACoordConvertFunc(FMeasurePoints[2]);
        end
        else
        begin
          LLineStartP := FMeasurePoints[1];
          LLineEndP   := FMeasurePoints[2];
        end;

        MoveTo(LLineStartP.X, LLineStartP.Y);
        LineTo(LLineEndP.X,   LLineEndP.Y);
      end;
    end;

    // Restore the properties of the Canvas.Pen .
    Pen.Color := LTempPenColor;
    Pen.Width := LTempPenWidth;
    Pen.Style := LTempPenStyle;
    Pen.Mode  := LTempPenMode;
  end;
  
  // Draw handles
  DrawHandles(ACanvas, APenMode, ACoordConvertFunc);
end;

procedure TgmMeasureLine.DrawHandles(ACanvas: TCanvas; const APenMode: TPenMode;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LTempPenColor      : TColor;
  LTempBrushColor    : TColor;
  LTempPenWidth      : Integer;
  LTempPenStyle      : TPenStyle;
  LTempPenMode       : TPenMode;
  LTempBrushStyle    : TBrushStyle;
  LSpiderPt1         : TPoint;
  LSpiderPt2         : TPoint;
  LEllipseStart      : TPoint;
  LEllipseEnd        : TPoint;
  LMeasurePointCount : Integer;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                        LTempPenStyle, LTempPenMode, LTempBrushStyle);

    Pen.Style   := psSolid;
    Pen.Width   := 1;
    Pen.Mode    := APenMode;
    Brush.Style := bsClear;

    LMeasurePointCount := Length(FMeasurePoints);
    
    if LMeasurePointCount = 2 then
    begin
      if Assigned(ACoordConvertFunc) then
      begin
        LSpiderPt1 := ACoordConvertFunc(FMeasurePoints[0]);
        LSpiderPt2 := ACoordConvertFunc(FMeasurePoints[1]);
      end
      else
      begin
        LSpiderPt1 := FMeasurePoints[0];
        LSpiderPt2 := FMeasurePoints[1];
      end;
      
      DrawSpider(ACanvas, LSpiderPt1, HANDLE_RADIUS,
                 FHandleLineColor1, FHandleLineColor2,
                 APenMode);  // draw first handle
                 
      DrawSpider(ACanvas, LSpiderPt2, HANDLE_RADIUS,
                 FHandleLineColor1, FHandleLineColor2,
                 APenMode);  // draw second handle
    end
    else
    if LMeasurePointCount = 3 then
    begin
      if Assigned(ACoordConvertFunc) then
      begin
        LSpiderPt1    := ACoordConvertFunc(FMeasurePoints[0]);
        LSpiderPt2    := ACoordConvertFunc(FMeasurePoints[2]);
        LEllipseStart := ACoordConvertFunc(FMeasurePoints[1]);
      end
      else
      begin
        LSpiderPt1    := FMeasurePoints[0];
        LSpiderPt2    := FMeasurePoints[2];
        LEllipseStart := FMeasurePoints[1];
      end;

      LEllipseEnd     := LEllipseStart;
      LEllipseStart.X := LEllipseStart.X - HANDLE_RADIUS + 1;
      LEllipseStart.Y := LEllipseStart.Y - HANDLE_RADIUS + 1;
      LEllipseEnd.X   := LEllipseEnd.X   + HANDLE_RADIUS;
      LEllipseEnd.Y   := LEllipseEnd.Y   + HANDLE_RADIUS;

      // draw first handle
      DrawSpider(ACanvas, LSpiderPt1, HANDLE_RADIUS,
                 FHandleLineColor1, FHandleLineColor2,
                 APenMode);

      // draw second handle -- the connector
      Pen.Color := FConnectorColor;
      Ellipse(LEllipseStart.X, LEllipseStart.Y, LEllipseEnd.X, LEllipseEnd.Y);

      // draw third handle
      DrawSpider(ACanvas, LSpiderPt2, HANDLE_RADIUS,
                 FHandleLineColor1, FHandleLineColor2,
                 APenMode);
    end;

    SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                        LTempPenStyle, LTempPenMode, LTempBrushStyle);
  end;
end;

// Checking for whether or not the coordinate is over one of the Measure points.
function TgmMeasureLine.GetHandleAtPoint(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmMeasurePointSelector;
var
  LMeasurePointCount : Integer;
  p0, p1, p2         : TPoint;
begin
  Result             := mpsNone;
  LMeasurePointCount := Length(FMeasurePoints);

  if LMeasurePointCount >= 2 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      p0 := ACoordConvertFunc(FMeasurePoints[0]);
      p1 := ACoordConvertFunc(FMeasurePoints[1]);

      if LMeasurePointCount = 3 then
      begin
        p2 := ACoordConvertFunc(FMeasurePoints[2]);
      end;
    end
    else
    begin
      p0 := FMeasurePoints[0];
      p1 := FMeasurePoints[1];

      if LMeasurePointCount = 3 then
      begin
        p2 := FMeasurePoints[2];
      end;
    end;

    if SquareContainsPoint( p0, HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := mpsFirst;
    end
    else
    if SquareContainsPoint( p1, HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := mpsSecond;
    end
    else
    begin
      if LMeasurePointCount = 3 then
      begin
        if SquareContainsPoint( p2, HANDLE_RADIUS, Point(AX, AY) ) then
        begin
          Result := mpsThird;
        end;
      end;
    end;
  end;
end;

function TgmMeasureLine.GetMeasurePointCount: Integer;
begin
  Result := High(FMeasurePoints) + 1;
end;

procedure TgmMeasureLine.SetMeasurePoint(const AX, AY: Integer;
  const AMeasurePointSelector: TgmMeasurePointSelector);
begin
  case AMeasurePointSelector of
    mpsFirst:
      begin
        if High(FMeasurePoints) > -1 then
        begin
          FMeasurePoints[0] := Point(AX, AY);
        end;
      end;

    mpsSecond:
      begin
        if High(FMeasurePoints) > 0 then
        begin
          FMeasurePoints[1] := Point(AX, AY);
        end;
      end;
      
    mpsThird:
      begin
        if High(FMeasurePoints) = 2 then
        begin
          FMeasurePoints[2] := Point(AX, AY);
        end;
      end;
  end;
end;

procedure TgmMeasureLine.SwapFirstAndSecondMeasurePoint;
var
  LSwapPoint : TPoint;
begin
  if High(FMeasurePoints) = 1 then
  begin
    LSwapPoint        := FMeasurePoints[0];
    FMeasurePoints[0] := FMeasurePoints[1];
    FMeasurePoints[1] := LSwapPoint;
  end;
end;

procedure TgmMeasureLine.Translate(const ATranslateVector: TPoint);
var
  i : Integer;
begin
  if High(FMeasurePoints) >= 1 then
  begin
    for i := Low(FMeasurePoints) to High(FMeasurePoints) do
    begin
      FMeasurePoints[i] := AddPoints(FMeasurePoints[i], ATranslateVector);
    end;
  end;
end;


end.
