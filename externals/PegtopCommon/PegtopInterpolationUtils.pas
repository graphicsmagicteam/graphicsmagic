////////////////////////////////////////////////////////////////////////////////
// File:       PegtopInterpolationUtils.pas
// Version:    1.00
// Date:       04 Jul 2005 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Functions to perform cubic interpolation. Interpolation is done between
// (X1, Y1) and (X2, Y2) for any value of X in [0, 1] (or [0, 256] for integer
// math versions).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopInterpolationUtils;

interface

// floating point math
// 0.0 <= X <= 1.0
function InterpolateCubicHermiteSpline(X1, Y1, X2, Y2, A1, A2, X: Double): Double;
function InterpolateCubicCardinalSpline(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Tension2, X: Double): Double;
function InterpolateCubicKochanekBartelsSpline(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Bias1, Continuity1, Tension2, Bias2, Continuity2, X: Double): Double;
function InterpolateCubicEquidistant(Y0, Y1, Y2, Y3, X: Double): Double;

// integer math
// 0 <= X <= 256
// all other parameters are scaled by 256 as well
function InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X: Integer): Integer;
function InterpolateCubicCardinalSpline256(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Tension2, X: Integer): Integer;
function InterpolateCubicKochanekBartelsSpline256(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Bias1, Continuity1, Tension2, Bias2, Continuity2, X: Integer): Integer;
function InterpolateCubicEquidistant256(Y0, Y1, Y2, Y3, X: Integer): Integer;

implementation

////////////////////////////////////////////////////////////////////////////////
// floating point math
////////////////////////////////////////////////////////////////////////////////

function InterpolateCubicHermiteSpline(X1, Y1, X2, Y2, A1, A2, X: Double): Double;
var
  P, Q: Double;
begin
  P := A1 + A2 + 2 * (Y1 - Y2);
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

function InterpolateCubicEquidistant(Y0, Y1, Y2, Y3, X: Double): Double;
var
  P, Q: Double;
begin
  P := (Y3 - Y2) - (Y0 - Y1);
  Q := (Y0 - Y1) - P;
  Result := ((P * X + Q) * X + (Y2 - Y0)) * X + Y1;
end;

////////////////////////////////////////////////////////////////////////////////
// integer math
////////////////////////////////////////////////////////////////////////////////

function InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X: Integer): Integer;
var
  P, Q: Integer;
begin
  P := (A1 + A2) div 256 + 2 * (Y1 - Y2);
  Q := Y2 - P - A1 div 256 - Y1;
  Result := (((P * X div 256 + Q) * X + A1) div 256) * X div 256 + Y1;
end;

function InterpolateCubicCardinalSpline256(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Tension2, X: Integer): Integer;
var
  A1, A2: Integer;
begin
  A1 := (256 - Tension1) * (Y2 - Y0);
  A2 := (256 - Tension2) * (Y3 - Y1);
  Result := InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X);
end;

function InterpolateCubicKochanekBartelsSpline256(X0, Y0, X1, Y1, X2, Y2, X3, Y3,
  Tension1, Bias1, Continuity1, Tension2, Bias2, Continuity2, X: Integer): Integer;
var
  A1, A2: Integer;
begin
  A1 := (((256 - Tension1) * (256 + Bias1) * (256 + Continuity1) div 65536) * (Y1 - Y0)
  + ((256 - Tension1) * (256 - Bias1) * (256 - Continuity1) div 65536) * (Y2 - Y1)) div 2;
  A2 := (((256 - Tension2) * (256 + Bias2) * (256 - Continuity2) div 65536) * (Y2 - Y1)
  + ((256 - Tension2) * (256 - Bias2) * (256 + Continuity2) div 65536) * (Y3 - Y2)) div 2;
  Result := InterpolateCubicHermiteSpline256(X1, Y1, X2, Y2, A1, A2, X);
end;

function InterpolateCubicEquidistant256(Y0, Y1, Y2, Y3, X: Integer): Integer;
var
  P, Q: Integer;
begin
  P := (Y3 - Y2) - (Y0 - Y1);
  Q := (Y0 - Y1) - P;
  Result := ((P * X div 256 + Q) * X div 256 + (Y2 - Y0)) * X div 256 + Y1;
end;

end.
