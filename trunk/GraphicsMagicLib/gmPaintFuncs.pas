unit gmPaintFuncs;

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
 * Update Date: 20th, Feb, 2014 by Ma Xiaoguang
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

// Update Date: 2017-02-14

interface

uses
{ Delphi }
  Windows,
  Graphics,
{ Graphics32 }
  GR32;

  
procedure DrawAimFlag(ACanvas: TCanvas; const ASourcePoint: TPoint;
  const ARadius: Integer);

procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
  const ARect: TRect; const ASmallPattern: Boolean = False); overload;

procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
  ASize: Integer); overload;

procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
  const ASmallPattern: Boolean = False); overload;

procedure DrawDiamondHandle(ACanvas: TCanvas; const APoint: TPoint;
  const AHandleRadius: Integer; const ABrushStyle: TBrushStyle;
  const APenMode: TPenMode);

procedure DrawDirectionCircle(ACanvas: TCanvas; const ACenterPoint: TPoint;
  const ARadius, ARadians: Double; const ADrawDiameter: Boolean);

procedure DrawDistanceCircle(ACanvas: TCanvas; const ACenterPoint: TPoint;
  const ARadius, AAngle, AEdgePenWidth, ADistancePenWidth, ACenterPointRadius: Integer;
  const APenColor, ABrushColor, AFillBackground: TColor;
  const APenStyle: TPenStyle; const APenMode: TPenMode;
  const ABrushStyle: TBrushStyle;
  var ADistanceStart, ADistanceEnd: TPoint);

procedure DrawEllipse(ACanvas: TCanvas; const AStartPoint, AEndPoint: TPoint;
  const APenMode: TPenMode);

procedure DrawFreeLine(ACanvas: TCanvas; const X, Y: Integer;
  const APenMode: TPenMode);

procedure DrawFreeLine32(ADestBmp: TBitmap32; const X, Y: Integer;
  const APenStyle: TPenStyle);

procedure DrawHandle(const ACanvas: TCanvas; const APoint: TPoint;
  const APenColor, ABrushColor: TColor; const ABrushStyle: TBrushStyle;
  const APenMode: TPenMode; const ARadius: Integer);

procedure DrawHandleOnBitmap32(ADestBmp: TBitmap32;
  const APoint, AOffsetVector: TPoint; const ARadius: Integer;
  const AHandleColor: TColor32);

procedure DrawHorizontalLine(ACanvas: TCanvas;
  const AStartX, Y, AEndX: Integer; const APenMode: TPenMode);

procedure DrawHueGradientSpectra(ABitmap: TBitmap;
  const ASaturation, AValue: Integer;
  const AStartX, AStartY, AEndX, AEndY: Integer;
  const AValueAttenuation: Boolean);

procedure DrawLineOutline(ACanvas: TCanvas;
  const ACenterPoint, ACurrentPoint: TPoint; const AWeight: Integer;
  const AScale: Double);
  
procedure DrawPattern(const ADestBmp, APatternBmp: TBitmap32);

procedure DrawPolygon(const ACanvas: TCanvas;
  const APointArray: array of TPoint; const APenMode: TPenMode);

procedure DrawPolyBezier(ACanvas: TCanvas; const APointArray: array of TPoint;
  const APenMode: TPenMode);

procedure DrawPolyLine(const ACanvas: TCanvas;
  const APointArray: array of TPoint; const APenMode: TPenMode);

procedure DrawRectangle(ACanvas: TCanvas;
  const AStartPoint, AEndPoint: TPoint; const APenMode: TPenMode);

procedure DrawRectHandle(const ACanvas: TCanvas; const APoint: TPoint;
  const AHandleRadius: Integer; const ABrushStyle: TBrushStyle;
  const ABrushColor: TColor; const APenMode: TPenMode);

procedure DrawRegularPolygon(ACanvas: TCanvas;
  const ACenterPoint, ACurrentPoint: TPoint; const ASides: Integer;
  const APenMode: TPenMode; const AFillInside: Boolean);

procedure DrawRoundRect(ACanvas: TCanvas; const AStartPoint, AEndPoint: TPoint;
  const ACornerRadius: Integer; const APenMode: TPenMode);

procedure DrawStraightLine(ACanvas: TCanvas;
  const AStartPoint, AEndPoint: TPoint; const APenMode: TPenMode);

procedure DrawVectorLine(const ABitmap: TBitmap32;
  const AX1, AY1, AX2, AY2, AArrowEdgeLength: Integer); overload;

procedure DrawVectorLine(const ABitmap: TBitmap32;
  const AX1, AY1, AX2, AY2, AArrowEdgeLength: Integer;
  const AFillingColor, AEdgeColor: TColor32); overload;

procedure DrawVectorLine(const ACanvas: TCanvas;
  const AX1, AY1, AX2, AY2, AArrowEdgeLength: Integer); overload;

procedure DrawVerticalLine(ACanvas: TCanvas; const X, AStartY, AEndY: Integer;
  const APenMode: TPenMode);

procedure FeatheredCircleAlpha(ABitmap: TBitmap32;
  const ACenterX, ACenterY, ARadius, AFeather: Single);

procedure FNMDrawVectorLine(ABitmap: TBitmap32; const X1, Y1, X2, Y2: Integer;
  const AArrowEdgeLength: Integer);
  

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel,    // MoveLongWord
{ externals\Graphics32_3rd_Party }
  GR32_PolygonsOld,
{ externals\efg2 }
  ColorLibrary,     // HSVToRGBTriple()
{ GraphicsMagic Lib }
  gmTypes,
  gmMath,
  gmCommonFuncs;


procedure DrawAimFlag(ACanvas: TCanvas; const ASourcePoint: TPoint;
  const ARadius: Integer);
var
  LTempPenColor   : TColor;
  LTempBrushColor : TColor;
  LTempPenWidth   : Integer;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Style   := psSolid;
    Pen.Width   := 1;
    Pen.Mode    := pmNotXor;
    Brush.Color := clBlack;
    Brush.Style := bsClear;

    MoveTo(ASourcePoint.X - ARadius + 1, ASourcePoint.Y);
    LineTo(ASourcePoint.X + ARadius,     ASourcePoint.Y);

    MoveTo(ASourcePoint.X, ASourcePoint.Y - ARadius + 1);
    LineTo(ASourcePoint.X, ASourcePoint.Y + ARadius);

    Ellipse(ASourcePoint.X - ARadius + 1, ASourcePoint.Y - ARadius + 1,
            ASourcePoint.X + ARadius,     ASourcePoint.Y + ARadius);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32; const ARect: TRect;
  const ASmallPattern: Boolean = False);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  w, h, i, j     : Integer;
  x, y, LParity  : Integer;
  LLine1, LLine2 : TArrayOfColor32; // a buffer for a couple of scanlines
  LDestRow       : PColor32Array;
  LRect          : TRect;
begin
{$RANGECHECKS OFF}

  if not Assigned(ADestBmp) then
  begin
    Exit;
  end;

  LRect.Left   := Clamp(ARect.Left,   0, ADestBmp.Width);
  LRect.Right  := Clamp(ARect.Right,  0, ADestBmp.Width);
  LRect.Top    := Clamp(ARect.Top,    0, ADestBmp.Height);
  LRect.Bottom := Clamp(ARect.Bottom, 0, ADestBmp.Height);

  if (LRect.Left >= LRect.Right) or
     (LRect.Top  >= LRect.Bottom) then
  begin
    Exit;
  end;

  w := LRect.Right  - LRect.Left;
  h := LRect.Bottom - LRect.Top;

  SetLength(LLine1, w);
  SetLength(LLine2, w);

  for i := 0 to (w - 1) do
  begin
    if ASmallPattern then
    begin
      LParity := i shr 2 and $1;
    end
    else
    begin
      LParity := i shr 3 and $1;
    end;

    LLine1[i] := Colors[LParity];
    LLine2[i] := Colors[1 - LParity];
  end;
    
  for j := 0 to (h - 1) do
  begin
    y        := j + LRect.Top;
    LDestRow := ADestBmp.Scanline[y];

    if ASmallPattern then
    begin
      LParity := j shr 2 and $1;
    end
    else
    begin
      LParity := j shr 3 and $1;
    end;

    for i := 0 to (w - 1) do
    begin
      x := i + LRect.Left;

      if Boolean(LParity) then
      begin
        LDestRow[x] := LLine1[i];
      end
      else
      begin
        LDestRow[x] := LLine2[i];
      end;
    end;
  end;

{$RANGECHECKS ON}
end;
  
procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32; ASize: Integer);
const
  Colors : array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  i, LParity     : Integer;
  LMaxSize       : Integer;
  LLine1, LLine2 : TArrayOfColor32; // a buffer for a couple of scanlines
begin
  LMaxSize := MinIntValue([ADestBmp.Width, ADestBmp.Height]);
  ASize    := Clamp(ASize, 1, LMaxSize);

  with ADestBmp do
  begin
    SetLength(LLine1, Width);
    SetLength(LLine2, Width);
    
    for i := 0 to (Width - 1) do
    begin
      LParity   := i div ASize and $1;
      LLine1[i] := Colors[LParity];
      LLine2[i] := Colors[1 - LParity];
    end;
    
    for i := 0 to (Height - 1) do
    begin
      LParity := i div ASize and $1;
      
      if Boolean(LParity) then
      begin
        MoveLongword(LLine1[0], ScanLine[i]^, Width);
      end
      else
      begin
        MoveLongword(LLine2[0], ScanLine[i]^, Width);
      end;
    end;
  end;
end;

procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
  const ASmallPattern: Boolean = False);
const
  Colors : array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  i, LParity     : Integer;
  LLine1, LLine2 : TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with ADestBmp do
  begin
    SetLength(LLine1, Width);
    SetLength(LLine2, Width);
    
    for i := 0 to (Width - 1) do
    begin
      if ASmallPattern then
      begin
        LParity := i shr 2 and $1;
      end
      else
      begin
        LParity := i shr 3 and $1;
      end;

      LLine1[i] := Colors[LParity];
      LLine2[i] := Colors[1 - LParity];
    end;
    
    for i := 0 to (Height - 1) do
    begin
      if ASmallPattern then
      begin
        LParity := i shr 2 and $1;
      end
      else
      begin
        LParity := i shr 3 and $1;
      end;
      
      if Boolean(LParity) then
      begin
        MoveLongword(LLine1[0], ScanLine[i]^, Width);
      end
      else
      begin
        MoveLongword(LLine2[0], ScanLine[i]^, Width);
      end;
    end;
  end;
end;

// Pen Path Tool use only, yet
procedure DrawDiamondHandle(ACanvas: TCanvas; const APoint: TPoint;
  const AHandleRadius: Integer; const ABrushStyle: TBrushStyle;
  const APenMode: TPenMode);
var
  LTempPenWidth   : Integer;
  LTempPenColor   : TColor;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
  LTempBrushColor : TColor;
  LPoints         : array [0 .. 3] of TPoint;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Width   := 1;
    Pen.Color   := clGray;
    Pen.Style   := psSolid;
    Pen.Mode    := pmNotXor;
    Brush.Style := ABrushStyle;
    Brush.Color := clGray;

    LPoints[0].X := APoint.X;
    LPoints[0].Y := APoint.Y - AHandleRadius;
    LPoints[1].X := APoint.X - AHandleRadius;
    LPoints[1].Y := APoint.Y;
    LPoints[2].X := APoint.X;
    LPoints[2].Y := APoint.Y + AHandleRadius;
    LPoints[3].X := APoint.X + AHandleRadius;
    LPoints[3].Y := APoint.Y;

    Polygon(LPoints);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure DrawDirectionCircle(ACanvas: TCanvas; const ACenterPoint: TPoint;
  const ARadius, ARadians: Double; const ADrawDiameter: Boolean);
var
  LPenColor   : TColor;
  LBrushColor : TColor;
  LPenStyle   : TPenStyle;
  LBrushStyle : TBrushStyle;
  LPenMode    : TPenMode;
  LEndPoint   : TPoint;
  LIntRadius  : Integer;
begin
  with ACanvas do
  begin
    LPenColor   := Pen.Color;
    LBrushColor := Brush.Color;
    LPenStyle   := Pen.Style;
    LBrushStyle := Brush.Style;
    LPenMode    := Pen.Mode;

    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := clBlack;

    // filling background
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;

    FillRect(ClipRect);

    LIntRadius := Round(ARadius);

    // draw lines
    Pen.Width := 1;
    LEndPoint := CalcOffsetPointByRadian(ACenterPoint, ARadians, LIntRadius);

    MoveTo(ACenterPoint.X, ACenterPoint.Y);
    LineTo(LEndPoint.X, LEndPoint.Y);

    if ADrawDiameter then
    begin
      LEndPoint := CalcOffsetPointByRadian( ACenterPoint, (ARadians + PI), LIntRadius );
      
      MoveTo(ACenterPoint.X, ACenterPoint.Y);
      LineTo(LEndPoint.X, LEndPoint.Y);
    end;

    // draw circle
    Pen.Width   := 2;
    Brush.Style := bsClear;
    
    Ellipse(ACenterPoint.X - LIntRadius, ACenterPoint.Y - LIntRadius,
            ACenterPoint.X + LIntRadius, ACenterPoint.Y + LIntRadius);

    // draw center point
    Brush.Style := bsSolid;
    Brush.Color := clBlack;

    Ellipse(ACenterPoint.X - 1, ACenterPoint.Y - 1,
            ACenterPoint.X + 2, ACenterPoint.Y + 2);

    Pen.Color   := LPenColor;
    Brush.Color := LBrushColor;
    Pen.Style   := LPenStyle;
    Brush.Style := LBrushStyle;
    Pen.Mode    := LPenMode;
  end;
end;

// for Motion Blur
procedure DrawDistanceCircle(ACanvas: TCanvas; const ACenterPoint: TPoint;
  const ARadius, AAngle, AEdgePenWidth, ADistancePenWidth, ACenterPointRadius: Integer;
  const APenColor, ABrushColor, AFillBackground: TColor;
  const APenStyle: TPenStyle; const APenMode: TPenMode;
  const ABrushStyle: TBrushStyle;
  var ADistanceStart, ADistanceEnd: TPoint);
var
  LCircleStart, LCircleEnd : TPoint;
  LDeltaX, LDeltaY         : Integer;
begin
  if ( AAngle >= (-90) ) and (AAngle <= 90) then
  begin
    LCircleStart := Point(ACenterPoint.X - ARadius, ACenterPoint.Y - ARadius);
    LCircleEnd   := Point(ACenterPoint.X + ARadius, ACenterPoint.Y + ARadius);
    
    LDeltaX := Round(  Cos( Abs(AAngle) * PI / 180 ) * ARadius  );
    LDeltaY := Round(  Sin( Abs(AAngle) * PI / 180 ) * ARadius  );

    if AAngle >= 0 then
    begin
      ADistanceStart.X := ACenterPoint.X + LDeltaX;
      ADistanceStart.Y := ACenterPoint.Y - LDeltaY;
      ADistanceEnd.X   := ACenterPoint.X - LDeltaX;
      ADistanceEnd.Y   := ACenterPoint.Y + LDeltaY;
    end
    else
    begin
      ADistanceStart.X := ACenterPoint.X - LDeltaX;
      ADistanceStart.Y := ACenterPoint.Y - LDeltaY;
      ADistanceEnd.X   := ACenterPoint.X + LDeltaX;
      ADistanceEnd.Y   := ACenterPoint.Y + LDeltaY;
    end;

    with ACanvas do
    begin
      // Fill background for erasing the last process
      Brush.Style := bsSolid;
      Brush.Color := AFillBackground;

      FillRect(ClipRect);

      // Draw circle
      Pen.Width   := AEdgePenWidth;
      Pen.Color   := APenColor;
      Pen.Mode    := APenMode;
      Pen.Style   := APenStyle;
      Brush.Color := ABrushColor;
      Brush.Style := ABrushStyle;
      
      Ellipse(LCircleStart.X, LCircleStart.Y, LCircleEnd.X, LCircleEnd.Y);

      // Draw distance
      Pen.Width := ADistancePenWidth;
      
      MoveTo(ADistanceStart.X, ADistanceStart.Y);
      LineTo(ADistanceEnd.X,   ADistanceEnd.Y);

      // Draw center point
      Pen.Width   := 1;
      Brush.Style := bsSolid;
      Brush.Color := APenColor;

      Ellipse(ACenterPoint.X - ACenterPointRadius,
              ACenterPoint.Y - ACenterPointRadius,
              ACenterPoint.X + ACenterPointRadius,
              ACenterPoint.Y + ACenterPointRadius);
    end;
  end;
end;

procedure DrawEllipse(ACanvas: TCanvas; const AStartPoint, AEndPoint: TPoint;
  const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    Ellipse(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);
    
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawFreeLine(ACanvas: TCanvas; const X, Y: Integer;
  const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;
    
    LineTo(X, Y);

    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawFreeLine32(ADestBmp: TBitmap32; const X, Y: Integer;
  const APenStyle: TPenStyle);
begin
  if APenStyle = psSolid then
  begin
    ADestBmp.LineToFS(X, Y);
  end
  else
  begin
    ADestBmp.LineToFSP(X, Y);
  end;
end;

procedure DrawHandle(const ACanvas: TCanvas; const APoint: TPoint;
  const APenColor, ABrushColor: TColor; const ABrushStyle: TBrushStyle;
  const APenMode: TPenMode; const ARadius: Integer);
var
  LTempPenColor   : TColor;
  LTempBrushColor : TColor;
  LTempPenWidth   : Integer;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := APenColor;
    Pen.Style   := psSolid;
    Pen.Width   := 1;           // Hardwire for now
    Pen.Mode    := APenMode;
    Brush.Color := ABrushColor;
    Brush.Style := ABrushStyle;

    Rectangle(APoint.X - ARadius, APoint.Y - ARadius,
              APoint.X + ARadius, APoint.Y + ARadius);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure DrawHandleOnBitmap32(ADestBmp: TBitmap32;
  const APoint, AOffsetVector: TPoint; const ARadius: Integer;
  const AHandleColor: TColor32);
var
  LTop, LLeft, LBottom, LRight : Integer;
begin
  LLeft   := APoint.X - ARadius + AOffsetVector.X;
  LTop    := APoint.Y - ARadius + AOffsetVector.Y;
  LRight  := APoint.X + ARadius + AOffsetVector.X;
  LBottom := APoint.Y + ARadius + AOffsetVector.Y;

  ADestBmp.FillRectS (LLeft, LTop, LRight, LBottom, AHandleColor);
  ADestBmp.FrameRectS(LLeft, LTop, LRight, LBottom, clBlack32);
end;

procedure DrawHorizontalLine(ACanvas: TCanvas;
  const AStartX, Y, AEndX: Integer; const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    MoveTo(AStartX, Y);
    LineTo(AEndX,   Y);
    
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawHueGradientSpectra(ABitmap: TBitmap;
  const ASaturation, AValue: Integer;
  const AStartX, AStartY, AEndX, AEndY: Integer;
  const AValueAttenuation: Boolean);
var
  i, j, v, w, h : Integer;
  LValueStep    : Double;
  LRow          : PRGBTripleArray;
begin
  LValueStep := 0.0;

  if (AStartX <  0) or
     (AEndX   <  0) or
     (AStartX >  ABitmap.Width) or
     (AEndX   >  ABitmap.Width) or
     (AStartX >= AEndX) then
  begin
    Exit;
  end;

  if (AStartY <  0) or
     (AEndY   <  0) or
     (AStartY >  ABitmap.Height) or
     (AEndY   >  ABitmap.Height) or
     (AStartY >= AEndY) then
  begin
    Exit;
  end;

  w := AEndX - AStartX;
  h := AEndY - AStartY;

  if AValueAttenuation then
  begin
    LValueStep := AValue / h; // the steps of lightness attenuation
  end;

  if ABitmap.PixelFormat <> pf24bit then
  begin
    ABitmap.PixelFormat := pf24bit;
  end;

  for j := AStartY to AEndY do
  begin
    // lightness attenuation
    if AValueAttenuation then
    begin
      v := Round( LValueStep * (AEndY - j) );
    end
    else
    begin
      v := AValue;
    end;

    if v > 255 then
    begin
      v := 255;
    end;

    LRow := ABitmap.ScanLine[j];
    
    for i := AStartX to AEndX do
    begin
      LRow[i] := HSVToRGBTriple( MulDiv(360, i, w { origin is 255}), ASaturation, v);
    end;
  end;
end;

procedure DrawLineOutline(ACanvas: TCanvas;
  const ACenterPoint, ACurrentPoint: TPoint; const AWeight: Integer;
  const AScale: Double);
var
  LLineWeight : Integer;
  LPolygon    : array [0 .. 4] of TPoint;
begin
  LLineWeight := Round(AWeight * AScale);

  // get vertices for the outline
  CalcLineOutlineVertices(LPolygon, ACenterPoint, ACurrentPoint, LLineWeight);

  // draw the polygon
  ACanvas.Polyline(LPolygon);
end;

procedure DrawPattern(const ADestBmp, APatternBmp: TBitmap32);
var
  X, Y : Integer;
begin
  if (not Assigned(ADestBmp)) or
     (not Assigned(APatternBmp)) or
     (ADestBmp.Width <= 0) or
     (ADestBmp.Height <= 0) or
     (APatternBmp.Width <= 0) or
     (APatternBmp.Height <= 0) then
  begin
    Exit;
  end;

  Y := 0;
  repeat

    X := 0;
    repeat
      ADestBmp.Draw(X, Y, APatternBmp);
      X := X + APatternBmp.Width;
    until X >= ADestBmp.Width;

    Y := Y + APatternBmp.Height;
  until Y >= ADestBmp.Height;
end;

procedure DrawPolygon(const ACanvas: TCanvas;
  const APointArray: array of TPoint; const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    Polygon(APointArray);
    
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawPolyBezier(ACanvas: TCanvas; const APointArray: array of TPoint;
  const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    PolyBezier(APointArray);
    
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawPolyLine(const ACanvas: TCanvas;
  const APointArray: array of TPoint; const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    PolyLine(APointArray);
    
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawRectangle(ACanvas: TCanvas; const AStartPoint, AEndPoint: TPoint;
  const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    Rectangle(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);
    
    Pen.Mode := LTempPenMode;
  end;
end;

// Pen Path Tool use only, yet
procedure DrawRectHandle(const ACanvas: TCanvas; const APoint: TPoint;
  const AHandleRadius: Integer; const ABrushStyle: TBrushStyle;
  const ABrushColor: TColor; const APenMode: TPenMode);
var
  LTempPenWidth   : Integer;
  LTempPenColor   : TColor;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
  LTempBrushColor : TColor;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Width   := 1;
    Pen.Color   := clBlack;
    Pen.Style   := psSolid;
    Pen.Mode    := APenMode;
    Brush.Color := ABrushColor;
    Brush.Style := ABrushStyle;

    Rectangle(APoint.X - AHandleRadius, APoint.Y - AHandleRadius,
              APoint.X + AHandleRadius, APoint.Y + AHandleRadius);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure DrawRegularPolygon(ACanvas: TCanvas;
  const ACenterPoint, ACurrentPoint: TPoint;
  const ASides: Integer; const APenMode: TPenMode;
  const AFillInside: Boolean);
var
  LVertices    : array of TPoint;     // vertices array
  LTempPenMode : TPenMode;
begin
  // the number of sides of the polygon should be 3..100
  if (ASides >= 3) and (ASides <= 100) then
  begin
    // setup the vertices array for holding all vertices of the polygon
    SetLength(LVertices, ASides + 1);

    // get vertices for the regular polygon
    CalcRegularPolygonVertices(LVertices, ACenterPoint, ACurrentPoint, ASides);

    with ACanvas do
    begin
      LTempPenMode := Pen.Mode;
      Pen.Mode     := APenMode;

      // draw the polygon
      if AFillInside then
      begin
        Polygon(LVertices);
      end
      else
      begin
        Polyline(LVertices);
      end;

      Pen.Mode := LTempPenMode;
    end;
  end;
end;

procedure DrawRoundRect(ACanvas: TCanvas; const AStartPoint, AEndPoint: TPoint;
  const ACornerRadius: Integer; const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    RoundRect(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y,
              ACornerRadius, ACornerRadius);
              
    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawStraightLine(ACanvas: TCanvas;
  const AStartPoint, AEndPoint: TPoint; const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    MoveTo(AStartPoint.X, AStartPoint.Y);
    LineTo(AEndPoint.X,   AEndPoint.Y);

    Pen.Mode := LTempPenMode;
  end;
end;

procedure DrawVectorLine(const ABitmap: TBitmap32;
  const AX1, AY1, AX2, AY2: Integer; const AArrowEdgeLength: Integer);
var
  LDeltaX, LDeltaY, i  : Integer;
  LTheta, LAccumRadian : Extended;  // radian
  LLineLength          : Extended;
  LArrowHeight         : Extended;
  LHalfEdgeLength      : Extended;
  LTailLength          : Extended;
  LBasePoint           : TPoint;
  LArrowPoints         : array [0..2] of TPoint;
  LPolygon             : TPolygon32;
begin
  if (AX1 = AX2) and (AY1 = AY2) then
  begin
    Exit;
  end;

  if AArrowEdgeLength < 1 then
  begin
    Exit;
  end;

  LDeltaX := AX2 - AX1;
  LDeltaY := AY2 - AY1;
  LTheta  := ArcTan2(LDeltaY, LDeltaX);

  LLineLength := Sqrt(LDeltaX * LDeltaX + LDeltaY * LDeltaY);

  LHalfEdgeLength := AArrowEdgeLength / 2;
  LArrowHeight    := Sqrt(AArrowEdgeLength * AArrowEdgeLength - LHalfEdgeLength * LHalfEdgeLength);
  LTailLength     := LLineLength - LArrowHeight;

  LBasePoint.X := Round( AX1 + LTailLength * Cos(LTheta) );
  LBasePoint.Y := Round( AY1 + LTailLength * Sin(LTheta) );

  LAccumRadian      := LTheta + PI / 2;
  LArrowPoints[0].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPoints[0].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LAccumRadian      := LTheta - PI / 2;
  LArrowPoints[1].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPoints[1].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LArrowPoints[2].X := AX2;
  LArrowPoints[2].Y := AY2;

  LPolygon := TPolygon32.Create;
  try
    for i := 0 to 2 do
    begin
      LPolygon.Add( FixedPoint(LArrowPoints[i]) );
    end;

    ABitmap.PenColor := clBlack32;

    ABitmap.MoveTo(AX1, AY1);
    ABitmap.LineToS(AX2, AY2);

    LPolygon.DrawFill(ABitmap, clBlack32);
    LPolygon.DrawEdge(ABitmap, clBlack32);
  finally
    LPolygon.Free;
  end;
end;

procedure DrawVectorLine(const ABitmap: TBitmap32;
  const AX1, AY1, AX2, AY2, AArrowEdgeLength: Integer;
  const AFillingColor, AEdgeColor: TColor32);
var
  LDeltaX, LDeltaY, i  : Integer;
  LTheta, LAccumRadian : Extended;  // radian
  LLineLength          : Extended;
  LArrowHeight         : Extended;
  LHalfEdgeLength      : Extended;
  LTailLength          : Extended;
  LBasePoint           : TPoint;
  LArrowPoints         : array [0..2] of TPoint;
  LPolygon             : TPolygon32;
begin
  if (AX1 = AX2) and (AY1 = AY2) then
  begin
    Exit;
  end;

  if AArrowEdgeLength < 1 then
  begin
    Exit;
  end;

  LDeltaX := AX2 - AX1;
  LDeltaY := AY2 - AY1;
  LTheta  := ArcTan2(LDeltaY, LDeltaX);

  LLineLength := Sqrt(LDeltaX * LDeltaX + LDeltaY * LDeltaY);

  LHalfEdgeLength := AArrowEdgeLength / 2;
  LArrowHeight    := Sqrt(AArrowEdgeLength * AArrowEdgeLength - LHalfEdgeLength * LHalfEdgeLength);
  LTailLength     := LLineLength - LArrowHeight;

  LBasePoint.X := Round( AX1 + LTailLength * Cos(LTheta) );
  LBasePoint.Y := Round( AY1 + LTailLength * Sin(LTheta) );

  LAccumRadian      := LTheta + PI / 2;
  LArrowPoints[0].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPoints[0].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LAccumRadian      := LTheta - PI / 2;
  LArrowPoints[1].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPoints[1].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LArrowPoints[2].X := AX2;
  LArrowPoints[2].Y := AY2;

  LPolygon := TPolygon32.Create;
  try
    for i := 0 to 2 do
    begin
      LPolygon.Add( FixedPoint(LArrowPoints[i]) );
    end;

    ABitmap.PenColor := clBlack32;

    ABitmap.MoveTo(AX1, AY1);
    ABitmap.LineToS(AX2, AY2);

    LPolygon.DrawFill(ABitmap, AFillingColor);
    LPolygon.DrawEdge(ABitmap, AEdgeColor);
  finally
    LPolygon.Free;
  end;
end;

procedure DrawVectorLine(const ACanvas: TCanvas;
  const AX1, AY1, AX2, AY2, AArrowEdgeLength: Integer);
var
  LDeltaX, LDeltaY     : Integer;
  LTheta, LAccumRadian : Extended;  // radian
  LLineLength          : Extended;
  LArrowHeight         : Extended;
  LHalfEdgeLength      : Extended;
  LTailLength          : Extended;
  LBasePoint           : TPoint;
  LArrowPolygon        : array [0..2] of TPoint;
begin
  if (AX1 = AX2) and (AY1 = AY2) then
  begin
    Exit;
  end;

  if AArrowEdgeLength < 1 then
  begin
    Exit;
  end;

  LDeltaX := AX2 - AX1;
  LDeltaY := AY2 - AY1;
  LTheta  := ArcTan2(LDeltaY, LDeltaX);

  LLineLength := Sqrt(LDeltaX * LDeltaX + LDeltaY * LDeltaY);

  LHalfEdgeLength := AArrowEdgeLength / 2;
  LArrowHeight    := Sqrt(AArrowEdgeLength * AArrowEdgeLength - LHalfEdgeLength * LHalfEdgeLength);
  LTailLength     := LLineLength - LArrowHeight;

  LBasePoint.X := Round( AX1 + LTailLength * Cos(LTheta) );
  LBasePoint.Y := Round( AY1 + LTailLength * Sin(LTheta) );

  LAccumRadian       := LTheta + PI / 2;
  LArrowPolygon[0].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPolygon[0].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LAccumRadian       := LTheta - PI / 2;
  LArrowPolygon[1].X := Round( LBasePoint.X + LHalfEdgeLength * Cos(LAccumRadian) );
  LArrowPolygon[1].Y := Round( LBasePoint.Y + LHalfEdgeLength * Sin(LAccumRadian) );

  LArrowPolygon[2].X := AX2;
  LArrowPolygon[2].Y := AY2;

  with ACanvas do
  begin
    MoveTo(AX1, AY1);
    LineTo(AX2, AY2);
    Polygon(LArrowPolygon);
  end;
end;

procedure DrawVerticalLine(ACanvas: TCanvas; const X, AStartY, AEndY: Integer;
  const APenMode: TPenMode);
var
  LTempPenMode : TPenMode;
begin
  with ACanvas do
  begin
    LTempPenMode := Pen.Mode;
    Pen.Mode     := APenMode;

    MoveTo(X, AStartY);
    LineTo(X, AEndY);

    Pen.Mode := LTempPenMode;
  end;
end;

procedure FeatheredCircleAlpha(ABitmap: TBitmap32;
  const ACenterX, ACenterY, ARadius, AFeather: Single);

{----------------------------------------
  modified by zoltan!

  Draw a disk on Bitmap. Bitmap must be a 256 color (pf8bit) palette bitmap,
  and parts outside the disk will get palette index 0, parts inside will get
  palette index 255, and in the antialiased area (feather), the pixels will
  get values inbetween.

  ***Parameters***
  Bitmap:
    The bitmap to draw on

  CenterX, CenterY:
    The center of the disk (float precision). Note that [0, 0] would be the
    center of the first pixel. To draw in the exact middle of a 100x100 bitmap,
    use CenterX = 49.5 and CenterY = 49.5

  Radius:
    The radius of the drawn disk in pixels (float precision)

  Feather:
    The feather area. Use 1 pixel for a 1-pixel antialiased area. Pixel centers
    outside 'Radius + Feather / 2' become 0, pixel centers inside 'Radius - Feather/2'
    become 255. Using a value of 0 will yield a bilevel image.
    
  Copyright (c) 2003 Nils Haeck M.Sc. www.simdesign.nl
  
  ------------------------------------------------------}

var
  x, y           : Integer;
  LX, RX, LY, RY : Integer;
  Fact           : Integer;
  RPF2, RMF2     : Single;
//  P              : PByteArray;  //zoltan
  P              : PColor32Array;
  SqY, SqDist    : Single;
  sqX            : array of Single;
  b              : Byte;
begin
{$RANGECHECKS OFF}

  // Determine some helpful values (singles)
  RPF2 := sqr(ARadius + AFeather/2);
  RMF2 := sqr(ARadius - AFeather/2);

  // Determine bounds:
  LX := Max(floor(ACenterX - RPF2), 0);
  RX := Min(ceil (ACenterX + RPF2), ABitmap.Width - 1);
  LY := Max(floor(ACenterY - RPF2), 0);
  RY := Min(ceil (ACenterY + RPF2), ABitmap.Height - 1);

  // Optimization run: find squares of X first
  SetLength(SqX, RX - LX + 1);
  for x := LX to RX do
  begin
    SqX[x - LX] := sqr(x - ACenterX);
  end;

  // Loop through Y values
  for y := LY to RY do
  begin
    P := ABitmap.Scanline[y];    //zoltan!!!
//       p :=  bitmap32.pixelptr[x,y];
    SqY := Sqr(y - ACenterY);
    
    // Loop through X values
    for x := LX to RX do
    begin
      // determine squared distance from center for this pixel
      SqDist := SqY + SqX[x - LX];

      // inside inner circle? Most often..
      if sqdist < RMF2 then
      begin
        // inside the inner circle.. just give the scanline the new color
//        P[x] := 255;                       //orig
//        P[x] := color32(255,255,255,255); //works ok!
        P[x]:=SetAlpha(P[x], 255);
      end
      else
      begin
        // inside outer circle?
        if sqdist < RPF2 then
        begin
          // We are inbetween the inner and outer bound, now mix the color
          Fact := round(((ARadius - sqrt(sqdist)) * 2 / AFeather) * 127.5 + 127.5);
//          P[x] := Max(0, Min(Fact, 255)); // just in case limit to [0, 255]   //orig
          b:=Max(0, Min(Fact, 255)); // just in case limit to [0, 255]
//           P[x] := color32(b,b,b,255);  //works ok!
          P[x]:=SetAlpha(P[x], b);
        end
        else
        begin
          //P[x] := 0;
          //P[x] := color32(0,0,0,255);   //works ok!
          P[x]:=SetAlpha(P[x], 0);
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

// by Fernando Nadal Martinez < fnm@uma.es >, from GR32 newsgroup
procedure FNMDrawVectorLine(ABitmap: TBitmap32; const X1, Y1, X2, Y2: Integer;
  const AArrowEdgeLength: Integer);
const
  AspectRatio = 0.5 / 2.0; // Added by FNM
var
  LDeltaX, LDeltaY, i  : Integer;
  LTheta               : Extended;  // radian
  LCosTheta, LSinTheta : Extended;	 // Added by FNM
  LArrowLength         : Extended;
  LHalfEdgeLength      : Extended;
  LBasePoint           : TPoint;
  LArrowPoints         : array [0..2] of TPoint;
  LPolygon             : TPolygon32;
begin
  if (X1 = X2) and (Y1 = Y2) then
  begin
    Exit;
  end;

  if AArrowEdgeLength < 1 then
  begin
    Exit;
  end;

  LDeltaX := X2 - X1;
  LDeltaY := Y2 - Y1;
  LTheta  := ArcTan2(LDeltaY, LDeltaX);

  LHalfEdgeLength := AspectRatio * AArrowEdgeLength; // Changed by FNM
  LArrowLength    := 0.75 * AArrowEdgeLength;        // Added by FNM

  LCosTheta := Cos(LTheta); // Added by FNM
  LSinTheta := Sin(LTheta); // Added by FNM

  LBasePoint.X := Round( X2 - LArrowLength * LCosTheta ); // Changed by FNM
  LBasePoint.Y := Round( Y2 - LArrowLength * LSinTheta ); // Changed by FNM

  LCosTheta := LHalfEdgeLength * LCosTheta;
  LSinTheta := LHalfEdgeLength * LSinTheta;

  LArrowPoints[0].X := Round( LBasePoint.X - ( - LSinTheta ) ); // Changed by FNM
  LArrowPoints[0].Y := Round( LBasePoint.Y - ( + LCosTheta ) ); // Changed by FNM

  LArrowPoints[1].X := Round( LBasePoint.X - ( + LSinTheta ) ); // Changed by FNM
  LArrowPoints[1].Y := Round( LBasePoint.Y - ( - LCosTheta ) ); // Changed by FNM

  LArrowPoints[2].X := X2;
  LArrowPoints[2].Y := Y2;

  LPolygon := TPolygon32.Create;
  try
    for i := 0 to 2 do
    begin
      LPolygon.Add( FixedPoint(LArrowPoints[i]) );
    end;

    ABitmap.PenColor := clBlack32;

    ABitmap.MoveTo(X1, Y1);
    ABitmap.LineToS(X2, Y2);
    
    LPolygon.DrawFill(ABitmap, clBlack32);
    LPolygon.DrawEdge(ABitmap, clBlack32);
  finally
    LPolygon.Free;
  end;
end;


end.
