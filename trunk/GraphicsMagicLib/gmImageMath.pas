{ Based on the code in ImageMath.java.

  Original author: Jerry Huxtable
  Pascal code: Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Last update: 2011-10-24

  -- Quote the words of the original author -------------------------------

  Another Disclaimer

  There's source code in Java for pretty well everything I talk about here.
  I make no claims that these are optimised in any way - I've opted for
  simplicity over speed everywhere and you'll probably be able to make most of
  these thing go faster with a bit of effort. You can use the source code for
  anything you want, including commercial purposes, but there's no liability.
  If your nuclear power station or missile system fails because of an improper
  blur, it's not my fault.

                                                  -- by Jerry Huxtable
}

unit gmImageMath;

interface

uses
  GR32;

type
  // A class containing math methods useful for image processing.
  TgmImageMath = class
    function Bias(a, b: Single): Single;
    function Gain(a, b: Single): Single;
    function Step(a, x: Single): Single;
    function Pulse(a, b, x: Single): Single;
    function SmoothPulse(a1, a2, b1, b2, x: Single): Single;
    function SmoothStep(a, b, x: Single): Single;
    function CircleUp(x: Single): Single;
    function CircleDown(x: Single): Single;
    function Clamp(x, a, b: Single): Single; overload;
    function Clamp(x, a, b: Integer): Integer; overload;
    function Modul(a, b: Double): Double; overload;
    function Modul(a, b: Single): Single; overload;
    function Modul(a, b: Integer): Integer; overload;
    function Triangle(x: Single): Single;
    function Lerp(t, a, b: Single): Single; overload;
    function Lerp(t: Single; a, b: Integer): Integer; overload;
    function MixColors(t: Single; rgb1, rgb2: TColor32): TColor32;
    function BilinearInterpolation(x, y: Single; p: array of TColor32): TColor32;
    function BrightnessNTSC(rgb: TColor32): Integer;
    function Spline(x: Single; NumKnots: Integer; Knots: array of Single): Single; overload;
    function Spline(x: Single; NumKnots: Integer; xKnots, yKnots: array of Integer): Single; overload;
    function ColorSpline(x: Single; NumKnots: Integer; Knots: array of Integer): Integer; overload;
    function ColorSpline(x, NumKnots: Integer; xKnots, yKnots: array of Integer): Integer; overload;
    procedure Resample(ASource, ADest: TArrayOfColor32; ALength, AOffset, AStride: Integer; AOutput: array of Single);
  end;

var
  ImageMath: TgmImageMath;

const
  HALF_PI    = PI / 2;
  QUARTER_PI = PI / 4;
  TWO_PI     = PI * 2;

implementation

uses
  SysUtils, Math;

const 
  // Catmull-Rom splines
  m00: Single = -0.5;
	m01: Single =  1.5;
	m02: Single = -1.5;
	m03: Single =  0.5;
	m10: Single =  1.0;
	m11: Single = -2.5;
	m12: Single =  2.0;
	m13: Single = -0.5;
	m20: Single = -0.5;
	m21: Single =  0.0;
	m22: Single =  0.5;
	m23: Single =  0.0;
	m30: Single =  0.0;
	m31: Single =  1.0;
	m32: Single =  0.0;
	m33: Single =  0.0;

//  | m00  m01  m02  m03 |       | -0.5   1.5  -1.5   0.5 |
//  | m10  m11  m12  m13 |  ---- |  1.0  -2.5   2.0  -0.5 |
//  | m20  m21  m22  m23 |  ---- | -0.5   0.0   0.5   0.0 |
//  | m30  m31  m32  m33 |       |  0.0   1.0   0.0   0.0 |


{ Apply a bias to a number in the unit interval, moving numbers towards 0 or 1
	according to the bias parameter.
	@param a the number to bias
	@param b the bias parameter. 0.5 means no change, smaller values bias towards 0, larger towards 1.
	@return the output value }
function TgmImageMath.Bias(a, b: Single): Single;
begin
//  Result := Power( a, Ln(b) / Ln(0.5) );
  Result := a / ((1.0/b - 2) * (1.0 - a) + 1);
end;


{ A variant of the gamma function.
	@param a the number to apply gain to
	@param b the gain parameter. 0.5 means no change, smaller values reduce gain, larger values increase gain.
	@return the output value }
function TgmImageMath.Gain(a, b: Single): Single;
var
//  p: Single;
  c: Single;
begin
{  if a < 0.001 then
  begin
    Result := 0.0;
  end
  else if a > 0.999 then
  begin
    Result := 1.0;
  end
  else
  begin
    p := Ln(1.0 - b) / Ln(0.5);

    if a < 0.5 then
    begin
      Result := Power(2 * a, p) / 2;
    end
    else
    begin
      Result := 1.0 - Power(2 * (1.0 - a), p) / 2;
    end;
  end; }

  c := (1.0/b - 2.0) * (1.0 - 2.0 * a);

  if a < 0.5 then
  begin
    Result := a / (c + 1.0);
  end
  else
  begin
    Result := (c - a) / (c - 1.0);
  end;
end;


{ The step function. Returns 0 below a threshold, 1 above.
	@param a the threshold position
	@param x the input parameter
	@return the output value - 0 or 1 }
function TgmImageMath.Step(a, x: Single): Single;
begin
  if x < a then
  begin
    Result := 0.0;
  end
  else
  begin
    Result := 1.0;
  end;
end;


{ A smoothed pulse function. A cubic function is used to smooth the step between two thresholds.
	@param a1 the lower threshold position for the start of the pulse
	@param a2 the upper threshold position for the start of the pulse
	@param b1 the lower threshold position for the end of the pulse
	@param b2 the upper threshold position for the end of the pulse
	@param x the input parameter
	@return the output value }
function TgmImageMath.SmoothPulse(a1, a2, b1, b2, x: Single): Single;
begin
  if (x < a1) or (x >= b2) then
  begin
    Result := 0;
  end
  else if x >= a2 then
  begin
    if x < b1 then
    begin
      Result := 1.0;
    end
    else
    begin
      x      := (x - b1) / (b2 - b1);
      Result := 1.0 - (x*x * (3.0 - 2.0 * x));
    end;
  end
  else
  begin
    x      := (x - a1) / (a2 - a1);
    Result := x*x * (3.0 - 2.0 * x);
  end;
end;


{ The pulse function. Returns 1 between two thresholds, 0 outside.
	@param a the lower threshold position
	@param b the upper threshold position
	@param x the input parameter
	@return the output value - 0 or 1 }
function TgmImageMath.Pulse(a, b, x: Single): Single;
begin
  if (x < a) or (x >= b) then
  begin
    Result := 0.0;
  end
  else
  begin
    Result := 1.0;
  end;
end;


{ A smoothed step function. A cubic function is used to smooth the step between two thresholds.
	@param a the lower threshold position
	@param b the upper threshold position
	@param x the input parameter
	@return the output value }
function TgmImageMath.SmoothStep(a, b, x: Single): Single;
begin
  if x < a then
  begin
    Result := 0;
  end
  else
  if x >= b then
  begin
    Result := 1;
  end
  else
  begin
    x      := (x - a) / (b - a);
    Result := x*x * (3 - 2 * x);
  end;
end;


{ A "circle up" function. Returns y on a unit circle given 1-x. Useful for forming bevels.
	@param x the input parameter in the range 0..1
	@return the output value }
function TgmImageMath.CircleUp(x: Single): Single;
begin
  x      := 1 - x;
  Result := Sqrt(1 - x * x);
end;


{ A "circle down" function. Returns 1-y on a unit circle given x. Useful for forming bevels.
	@param x the input parameter in the range 0..1
	@return the output value }
function TgmImageMath.CircleDown(x: Single): Single;
begin
  Result := 1.0 - Sqrt(1 - x * x);
end;


{ Clamp a value to an interval.
	@param a the lower clamp threshold
	@param b the upper clamp threshold
	@param x the input parameter
	@return the clamped value }
function TgmImageMath.Clamp(x, a, b: Single): Single;
begin
  if x < a then
  begin
    Result := a;
  end
  else if x > b then
  begin
    Result := b;
  end
  else
  begin
    Result := x;
  end;
end;


{ Clamp a value to an interval.
	@param a the lower clamp threshold
	@param b the upper clamp threshold
	@param x the input parameter
	@return the clamped value }
function TgmImageMath.Clamp(x, a, b: Integer): Integer;
begin
  if x < a then
  begin
    Result := a;
  end
  else if x > b then
  begin
    Result := b;
  end
  else
  begin
    Result := x;
  end;
end;


{ Return a mod b. This differs from the % operator with respect to negative numbers.
	@param a the dividend
	@param b the divisor
	@return a mod b }
function TgmImageMath.Modul(a, b: Double): Double;
var
  n: Integer;
begin
  n := Trunc(a / b);
  a := a - n*b;

  if a < 0 then
  begin
    Result := a + b;
  end
  else
  begin
    Result := a;
  end;
end;


{ Return a mod b. This differs from the % operator with respect to negative numbers.
	@param a the dividend
	@param b the divisor
	@return a mod b }
function TgmImageMath.Modul(a, b: Single): Single;
var
  n: Integer;
begin
  n := Trunc(a / b);
  a := a - n*b;

  if a < 0 then
  begin
    Result := a + b;
  end
  else
  begin
    Result := a;
  end;
end;


{ Return a mod b. This differs from the % operator with respect to negative numbers.
  @param a the dividend
  @param b the divisor
  @return a mod b }
function TgmImageMath.Modul(a, b: Integer): Integer;
var
  n: Integer;
begin
  n := a div b;
  a := a - n*b;

  if a < 0 then
  begin
    Result := a + b;
  end
  else
  begin
    Result := a;
  end;
end;


{ The triangle function. Returns a repeating triangle shape in the range 0..1 with wavelength 1.0
	@param x the input parameter
	@return the output value }
function TgmImageMath.Triangle(x: Single): Single;
var
  r: Single;
begin
  r := Modul(x, 1.0);

  if r < 0.5 then
  begin
    Result := 2.0 * r;
  end
  else
  begin
    Result := 2.0 * (1 - r);
  end;
end;


{ Linear interpolation.
	@param t the interpolation parameter
	@param a the lower interpolation range
	@param b the upper interpolation range
	@return the interpolated value }
function TgmImageMath.Lerp(t, a, b: Single): Single;
begin
  Result := a + t * (b - a);
end;


{ Linear interpolation.
	@param t the interpolation parameter
	@param a the lower interpolation range
	@param b the upper interpolation range
	@return the interpolated value }
function TgmImageMath.Lerp(t: Single; a, b: Integer): Integer;
begin
  Result := Trunc( a + t * (b - a) );
end;


{ Linear interpolation of ARGB values.
	@param t the interpolation parameter
	@param rgb1 the lower interpolation range
	@param rgb2 the upper interpolation range
	@return the interpolated value }
function TgmImageMath.MixColors(t: Single; rgb1, rgb2: TColor32): TColor32;
var
  a1, r1, g1, b1: Integer;
  a2, r2, g2, b2: Integer;
begin
  a1 := rgb1 shr 24 and $FF;
  r1 := rgb1 shr 16 and $FF;
  g1 := rgb1 shr 8  and $FF;
  b1 := rgb1        and $FF;

  a2 := rgb2 shr 24 and $FF;
  r2 := rgb2 shr 16 and $FF;
  g2 := rgb2 shr 8  and $FF;
  b2 := rgb2        and $FF;

  a1 := Lerp(t, a1, a2);
  r1 := Lerp(t, r1, r2);
  g1 := Lerp(t, g1, g2);
  b1 := Lerp(t, b1, b2);

  Result := (a1 shl 24) or (r1 shl 16) or (g1 shl 8) or b1;
end;


{ Bilinear interpolation of ARGB values.
	@param x the X interpolation parameter 0..1
	@param y the y interpolation parameter 0..1
	@param rgb array of four ARGB values in the order NW, NE, SW, SE
	@return the interpolated value }
function TgmImageMath.BilinearInterpolation(x, y: Single;
  p: array of TColor32): TColor32;
var
  m0, m1, cx, cy: Single;
  a, r, g, b    : Integer;
  a0, r0, g0, b0: Integer;
  a1, r1, g1, b1: Integer;
  a2, r2, g2, b2: Integer;
  a3, r3, g3, b3: Integer;
begin
  a0 := p[0] shr 24 and $FF;
  r0 := p[0] shr 16 and $FF;
  g0 := p[0] shr 8  and $FF;
  b0 := p[0]        and $FF;

  a1 := p[1] shr 24 and $FF;
  r1 := p[1] shr 16 and $FF;
  g1 := p[1] shr 8  and $FF;
  b1 := p[1]        and $FF;

  a2 := p[2] shr 24 and $FF;
  r2 := p[2] shr 16 and $FF;
  g2 := p[2] shr 8  and $FF;
  b2 := p[2]        and $FF;

  a3 := p[3] shr 24 and $FF;
  r3 := p[3] shr 16 and $FF;
  g3 := p[3] shr 8  and $FF;
  b3 := p[3]        and $FF;

  cx := 1.0 - x;
  cy := 1.0 - y;

  m0 := cx * a0 + x * a1;
  m1 := cx * a2 + x * a3;
  a  := Trunc(cy * m0 + y * m1);

  m0 := cx * r0 + x * r1;
  m1 := cx * r2 + x * r3;
  r  := Trunc(cy * m0 + y * m1);

  m0 := cx * g0 + x * g1;
  m1 := cx * g2 + x * g3;
  g  := Trunc(cy * m0 + y + m1);

  m0 := cx * b0 + x * b1;
  m1 := cx * b2 + x * b3;
  b  := Trunc(cy * m0 + y + m1);

  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;


{ Return the NTSC gray level of an RGB value.
	@param rgb1 the input pixel
	@return the gray level (0-255) }
function TgmImageMath.BrightnessNTSC(rgb: TColor32): Integer;
var
  r, g, b: Integer;
begin
  r := rgb shr 16 and $FF;
  g := rgb shr 8  and $FF;
  b := rgb        and $FF;

  Result := Trunc(r * 0.299 + g * 0.587 + b * 0.114);
end;


{ Compute a Catmull-Rom spline.
	@param x the input parameter
	@param numKnots the number of knots in the spline
	@param knots the array of knots
	@return the spline value }
function TgmImageMath.Spline(x: Single; NumKnots: Integer;
  Knots: array of Single): Single;
var
  Span, NumSpans: Integer;
  k0, k1, k2, k3: Single;
  c0, c1, c2, c3: Single;
begin
  NumSpans := NumKnots - 3;

  if NumSpans < 1 then
  begin
    raise Exception.Create('Too few knots in spline.');
  end;

  x    := Clamp(x, 0, 1) * NumSpans;
  Span := Trunc(x);

  if Span > (NumKnots - 4) then
  begin
    Span := NumKnots - 4;
  end;

  x := x - span;

  k0 := Knots[Span];
  k1 := Knots[Span + 1];
  k2 := Knots[Span + 2];
  k3 := Knots[Span + 3];

  c3 := m00*k0 + m01*k1 + m02*k2 + m03*k3;
  c2 := m10*k0 + m11*k1 + m12*k2 + m13*k3;
	c1 := m20*k0 + m21*k1 + m22*k2 + m23*k3;
	c0 := m30*k0 + m31*k1 + m32*k2 + m33*k3;

  Result := ((c3*x + c2)*x + c1)*x + c0;
end;


{ Compute a Catmull-Rom spline, but with variable knot spacing.
	@param x the input parameter
	@param numKnots the number of knots in the spline
	@param xknots the array of knot x values
	@param yknots the array of knot y values
	@return the spline value }
function TgmImageMath.Spline(x: Single; NumKnots: Integer;
  xKnots, yKnots: array of Integer): Single;
var
  Span, NumSpans: Integer;
  k0, k1, k2, k3: Single;
  c0, c1, c2, c3: Single;
  t             : Single;
begin
  NumSpans := NumKnots - 3;

  if NumSpans < 1 then
  begin
    raise Exception.Create('Too few knots in spline.');
  end;

  for Span := 0 to (NumSpans - 1) do
  begin
    if xKnots[Span + 1] > x then
    begin
      Break;
    end;
  end;

  if Span > (NumKnots - 3) then
  begin
    Span := NumKnots - 3;
  end;

  t := (x - xKnots[Span]) / (xKnots[Span + 1] - xKnots[Span]);

  Dec(Span);
  if Span < 0 then
  begin
    Span := 0;
    t    := 0;
  end;

  k0 := yKnots[Span];
  k1 := yKnots[Span + 1];
	k2 := yKnots[Span + 2];
	k3 := yKnots[Span + 3];
		
	c3 := m00*k0 + m01*k1 + m02*k2 + m03*k3;
	c2 := m10*k0 + m11*k1 + m12*k2 + m13*k3;
	c1 := m20*k0 + m21*k1 + m22*k2 + m23*k3;
	c0 := m30*k0 + m31*k1 + m32*k2 + m33*k3;
		
	Result := ((c3*t + c2)*t + c1)*t + c0;
end;


{ Compute a Catmull-Rom spline for RGB values.
	@param x the input parameter
	@param numKnots the number of knots in the spline
	@param knots the array of knots
	@return the spline value }
function TgmImageMath.ColorSpline(x: Single; NumKnots: Integer;
  Knots: array of Integer): Integer;
var
  Span, NumSpans: Integer;
  v, i, Shift, n: Integer;
  k0, k1, k2, k3: Single;
  c0, c1, c2, c3: Single;
begin
  NumSpans := NumKnots - 3;

  if NumSpans < 1 then
  begin
    raise Exception.Create('Too few knots in spline.');
  end;

  x := Clamp(x, 0, 1) * NumSpans;

  Span := Trunc(x);
  if Span > (NumKnots - 4) then
  begin
    Span := NumKnots - 4;
  end;

  x := x - Span;
  v := 0;

  for i := 0 to 3 do
  begin
    Shift := i * 8;

    k0 := Knots[Span]     shr Shift and $FF;
    k1 := Knots[Span + 1] shr Shift and $FF;
    k2 := Knots[Span + 2] shr Shift and $FF;
    k3 := Knots[Span + 3] shr Shift and $FF;

    c3 := m00*k0 + m01*k1 + m02*k2 + m03*k3;
    c2 := m10*k0 + m11*k1 + m12*k2 + m13*k3;
		c1 := m20*k0 + m21*k1 + m22*k2 + m23*k3;
		c0 := m30*k0 + m31*k1 + m32*k2 + m33*k3;

    n := Trunc( ((c3*x + c2)*x + c1)*x + c0 );

    if n < 0 then
    begin
      n := 0;
    end
    else if n > 255 then
    begin
      n := 255;
    end;

    v := v or (n shl Shift);
  end;

  Result := v;
end;


{ Compute a Catmull-Rom spline for RGB values, but with variable knot spacing.
	@param x the input parameter
	@param numKnots the number of knots in the spline
	@param xknots the array of knot x values
	@param yknots the array of knot y values
	@return the spline value }
function TgmImageMath.ColorSpline(x, NumKnots: Integer;
  xKnots, yKnots: array of Integer): Integer;
var
  Span, NumSpans: Integer;
  v, i, Shift, n: Integer;
  k0, k1, k2, k3: Single;
  c0, c1, c2, c3: Single;
  t             : Single;
begin
  NumSpans := NumKnots - 3;

  if NumSpans < 1 then
  begin
    raise Exception.Create('Too few knots in spline.');
  end;

  for Span := 0 to (NumSpans - 1) do
  begin
    if xKnots[Span + 1] > x then
    begin
      Break;
    end;
  end;

  if Span > (NumKnots - 3) then
  begin
    Span := NumKnots - 3;
  end;

  t := (x - xKnots[Span]) / (xKnots[Span + 1] - xKnots[Span]);

  Dec(Span);
  if Span < 0 then
  begin
    Span := 0;
    t    := 0;
  end;

  v := 0;
  for i := 0 to 3 do
  begin
    Shift := i * 8;

    k0 := yKnots[Span]     shr Shift and $FF;
    k1 := yKnots[Span + 1] shr Shift and $FF;
    k2 := yKnots[Span + 2] shr Shift and $FF;
    k3 := yKnots[Span + 3] shr Shift and $FF;

    c3 := m00*k0 + m01*k1 + m02*k2 + m03*k3;
    c2 := m10*k0 + m11*k1 + m12*k2 + m13*k3;
    c1 := m20*k0 + m21*k1 + m22*k2 + m23*k3;
    c0 := m30*k0 + m31*k1 + m32*k2 + m33*k3;

    n := Trunc( ((c3*t + c2)*t + c1)*t + c0 );

    if n < 0 then
    begin
      n := 0;
    end
    else if n > 255 then
    begin
      n := 255;
    end;

    v := v or (n shl Shift);
  end;

  Result := v;
end;


{ An implementation of Fant's resampling algorithm.
	@param source the source pixels
	@param dest the destination pixels
	@param length the length of the scanline to resample
	@param offset the start offset into the arrays
	@param stride the offset between pixels in consecutive rows
	@param output an array of output positions for each pixel }
procedure TgmImageMath.Resample(ASource, ADest: TArrayOfColor32;
  ALength, AOffset, AStride: Integer; AOutput: array of Single);
var
  Intensity, SizFac             : Single;
  InSegment, OutSegment         : Single;
  aSum, rSum, gSum, bSum        : Single;
  aIntensity, rIntensity        : Single;
  gIntensity, bIntensity        : Single;
  Input                         : array of Single;
  i, j, a, r, g, b              : Integer;
  NextA, NextR, NextG, NextB    : Integer;
  SrcIndex, DestIndex, LastIndex: Integer;
  rgb                           : TColor32;
begin
  SrcIndex  := AOffset;
  DestIndex := AOffset;
  LastIndex := Length(ASource);

  SetLength(Input, ALength + 1);

  i := 0;
  for j := 0 to (ALength - 1) do
  begin
    while AOutput[i + 1] < j do
    begin
      Inc(i);
    end;

    Input[j] := i + (j - AOutput[i]) / (AOutput[i + 1] - AOutput[i]);
  end;

  Input[ALength] := ALength;

  InSegment  := 1.0;
  OutSegment := Input[1];
  SizFac     := OutSegment;

  aSum := 0.0;
  rSum := 0.0;
  gSum := 0.0;
  bSum := 0.0;

  rgb := ASource[SrcIndex];
  a   := rgb shr 24 and $FF;
  r   := rgb shr 16 and $FF;
  g   := rgb shr 8  and $FF;
  b   := rgb        and $FF;

  Inc(SrcIndex, AStride);

  rgb   := ASource[SrcIndex];
  NextA := rgb shr 24 and $FF;
  NextR := rgb shr 16 and $FF;
  NextG := rgb shr 8  and $FF;
  NextB := rgb        and $FF;
  
  Inc(SrcIndex, AStride);

  i := 1;

  while i < ALength do
  begin
    aIntensity := InSegment * a + (1.0 - InSegment) * NextA;
    rIntensity := InSegment * r + (1.0 - InSegment) * NextR;
		gIntensity := InSegment * g + (1.0 - InSegment) * NextG;
		bIntensity := InSegment * b + (1.0 - InSegment) * NextB;

    if InSegment < OutSegment then
    begin
      aSum := aSum + (aIntensity * InSegment);
      rSum := rSum + (rIntensity * InSegment);
      gSum := gSum + (gIntensity * InSegment);
      bSum := bSum + (bIntensity * InSegment);

      OutSegment := OutSegment - InSegment;
      InSegment  := 1.0;

      a := NextA;
      r := NextR;
      g := NextG;
      b := NextB;

      if SrcIndex < LastIndex then
      begin
        rgb := ASource[SrcIndex];
      end;

      NextA := rgb shr 24 and $FF;
      NextR := rgb shr 16 and $FF;
      NextG := rgb shr 8  and $FF;
      NextB := rgb        and $FF;

      Inc(SrcIndex, AStride);
    end
    else
    begin
      aSum := aSum + (aIntensity * OutSegment);
      rSum := rSum + (rIntensity * OutSegment);
      gSum := gSum + (gIntensity * OutSegment);
      bSum := bSum + (bIntensity * OutSegment);

      ADest[DestIndex] := (Trunc(Min(aSum / SizFac, 255)) shl 24) or
                          (Trunc(Min(rSum / SizFac, 255)) shl 16) or
                          (Trunc(Min(gSum / SizFac, 255)) shl 8)  or
                          Trunc(Min(bSum / SizFac, 255));

      Inc(DestIndex, AStride);

      rSum := 0.0;
      gSum := 0.0;
      bSum := 0.0;

      InSegment  := InSegment - OutSegment;
      OutSegment := Input[i + 1] - Input[i];
      SizFac     := OutSegment;

      Inc(i);
    end;
  end;
end; 

end.
