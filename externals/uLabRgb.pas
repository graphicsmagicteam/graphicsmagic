unit uLabRgb;

{* *****************************************************************************
 * The original author of this unit is
 * Steve Schafer (TeamB) < pandeng[at]telepath[dot]com >. 
 *
 * Found from newsgroup: borland.public.delphi.graphics
 * Date: 12 Dec 1998
 *
 * -- Quote --
 *
 * On Fri, 11 Dec 1998 21:34:12 GMT,
 * terryhaan[at]palsoft[dot]bc[dot]ca (Terry Haan)
 * wrote:
 *
 *  >I am trying to write a routine that paints a box on the screen, but
 *  >the edges of the box should be either lighter or darker then the color
 *  >of the box. I want the routine to take a color, say clBlue, then paint
 *  >the box in blue, and the top area in light blue and the bottom in dark
 *  >blue.  ie the same of clYellow, etc..  The brightness would change say
 *  >20%-40% each way
 *  >
 *  >Using the Win32 Standard Color dialog, it is possible to set a custom
 *  >color to say blue, then manipulate a scroll bar to see all the
 *  >different brightnesses (luminosity) of that color.
 *  >
 *  >Is it possible to write a routine in Delphi that takes two parameters,
 *  >a color parameter, and a brightness % change, and return a new color?
 *
 *  Delphi is Turing-complete. It's possible to write a routine to do
 *  _anything_ computable in Delphi.
 *
 *  The HSL system that Windows uses is imperfect in that different colors
 *  having the same L value aren't all of the same perceived brightness.
 *  This makes it difficult to do brightness matching.  I've put together
 *  some code that uses the CIE's L*,a*,b* system to provide a color
 *  coordinate system that does a much better job of matching the response
 *  of the human visual system.
 *
 *  To use the code, take your original RGB value, then divide each value
 *  by 255 so that the resulting numbers range from 0.0 to 1.0. Now feed
 *  these values to the RgbToLab function. This converts the RGB
 *  coordinates to LAB coordinates, where the first coordinate (L) is
 *  scaled from 0 to 100. So now you can modify that L value to change the
 *  brightness of the color, then feed the new LAB values to LabToRgb to
 *  convert back to RGB. Finally, multiply each of the final result values
 *  by 255 and round to the nearest integer.
 *
 ******************************************************************************}

interface

type
  TVector3 = array[1..3] of Double;

function LabToRgb(Lab: TVector3): TVector3;
function RgbToLab(Rgb: TVector3): TVector3;

implementation

type
  TMatrix3 = array[1..3, 1..3] of Double;

var //const
  // Note: In the original code, the following two are of contants.
  // They cannot be initialized by the code in the Initialization part
  // of this unit at the end of the unit. So we changed the both to be
  // variables.
  //                                   -- Ma Xiaoguang and Ma Xiaoming
  RgbXyz: TMatrix3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  XyzRgb: TMatrix3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1));

  { CCIR recommended values }
  PhosphorX: TVector3 = (0.64, 0.30, 0.15);
  PhosphorY: TVector3 = (0.33, 0.60, 0.06);
  WhitePoint: TVector3 = (0.95, 1.0000, 1.09);
  Gamma: Double = 1 / 0.45;

function MultiplyMatrix3ByVector3(const M: TMatrix3;
  const V: TVector3): TVector3;
var
  I: Integer;
  J: Integer;
begin
  for I := 1 to 3 do
  begin
    Result[I] := 0.0;
    for J := 1 to 3 do
      Result[I] := Result[I] + M[I, J] * V[J]
  end
end;

function MultiplyMatrix3ByMatrix3(const M1, M2: TMatrix3): TMatrix3;
var
  I: Integer;
  J: Integer;
  K: Integer;
begin
  for I := 1 to 3 do
    for J := 1 to 3 do
    begin
      Result[I, J] := 0.0;
      for K := 1 to 3 do
        Result[I, J] := Result[I, J] + M1[I, K] * M2[K, J]
    end
end;

function InvertMatrix3(const M: TMatrix3): TMatrix3;
var
  I: Integer;
  J: Integer;
  D: Double;

  function Next(I: Integer): Integer;
  begin
    Result := I + 1;
    if Result > 3 then
      Result := Result - 3
  end;

  function Prev(I: Integer): Integer;
  begin
    Result := I - 1;
    if Result < 1 then
      Result := Result + 3
  end;

begin
  D := 0;

  for I := 1 to 3 do
    D := D + M[1, I] * (M[2, Next(I)] * M[3, Prev(I)] -
      M[2, Prev(I)] * M[3, Next(I)]);

  FillChar(Result, SizeOf(Result), 0);
  for I := 1 to 3 do
    for J := 1 to 3 do
      Result[J, I] := (M[Next(I), Next(J)] * M[Prev(I), Prev(J)] -
        M[Next(I), Prev(J)] * M[Prev(I), Next(J)]) / D
end;

function LabToXyz(const Lab: TVector3): TVector3;
var
  LL: Double;

  function Cube(X: Double): Double;
  begin
    if X >= (6 / 29) then
      Result := X * X * X
    else
      Result := (108 / 841) * (X - (4 / 29))
  end;

begin
  LL := (Lab[1] + 16) / 116;
  Result[1] := WhitePoint[1] * Cube(LL + Lab[2] / 500);
  Result[2] := WhitePoint[2] * Cube(LL);
  Result[3] := WhitePoint[3] * Cube(LL - Lab[3] / 200)
end;

function XyzToRgb(const Xyz: TVector3): TVector3;
var
  I: Integer;
begin
  Result := MultiplyMatrix3ByVector3(XyzRgb, Xyz);
  for I := 1 to 3 do
    if Result[I] <= 0.0 then
      Result[I] := 0
    else
      Result[I] := Exp(Ln(Result[I]) / Gamma)
end;

function LabToRgb(Lab: TVector3): TVector3;
begin
  Result := XyzToRgb(LabToXyz(Lab))
end;

function RgbToXyz(const Rgb: TVector3): TVector3;
var
  I: Integer;
begin
  Result := Rgb;
  for I := 1 to 3 do
    if Result[I] <= 0.0 then
      Result[I] := 0
    else
      Result[I] := Exp(Ln(Result[I]) * Gamma);

  Result := MultiplyMatrix3ByVector3(RgbXyz, Result)
end;

function XyzToLab(const Xyz: TVector3): TVector3;
var
  YY: Double;

  function CubeRoot(X: Double): Double;
  begin
    if X >= (216 / 24389) then
      Result := Exp(Ln(X) / 3)
    else
      Result := (841 / 108) * X + (4 / 29)
  end;

begin
  YY := CubeRoot(Xyz[2] / WhitePoint[2]);
  Result[1] := 116 * YY - 16;
  Result[2] := 500 * (CubeRoot(Xyz[1] / WhitePoint[1]) - YY);
  Result[3] := 200 * (YY - CubeRoot(Xyz[3] / WhitePoint[3]));
end;

function RgbToLab(Rgb: TVector3): TVector3;
begin
  Result := XyzToLab(RgbToXyz(Rgb))
end;

procedure InitTransformationMatrices;
var
  I: Integer;
  J: Integer;
  PhosphorZ: TVector3;
  C: TVector3;
  CToXyz: TMatrix3;
  XyzToC: TMatrix3;
begin
  for I := 1 to 3 do
  begin
    CToXyz[1, I] := PhosphorX[I];
    CToXyz[2, I] := PhosphorY[I];
    CToXyz[3, I] := 1 - PhosphorX[I] - PhosphorY[I]
  end;
  
  XyzToC := InvertMatrix3(CToXyz);
  C := MultiplyMatrix3ByVector3(XyzToC, WhitePoint);
  
  for I := 1 to 3 do
    for J := 1 to 3 do
      RgbXyz[I, J] := CToXyz[I, J] * C[J];

  XyzRgb := InvertMatrix3(RgbXyz)
end;

initialization
  InitTransformationMatrices;

end.
