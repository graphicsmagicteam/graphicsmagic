{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "base\curves.h" and "base\curves.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis,
  for giving us the opportunity to know how to achieve Curves Tool.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }

unit gmGimpBaseCurves;

interface

uses
  gmGimpBaseEnums;

const
  CURVES_NUM_POINTS = 17;

type
  TCRMatrix = array [0..3, 0..3] of Double;

  TgmCurves = class(TObject)
  private
    FCurveType: array [0..4] of Integer;
    FPoints   : array [0..4, 0..CURVES_NUM_POINTS - 1, 0..1] of Integer;

    procedure Init;
    procedure PlotCurve(const AChannel, p1, p2, p3, p4: Integer);

    function GetCurveType(AIndex: Integer): Integer;
    function GetPoints(AIndex1, AIndex2, AIndex3: Integer): Integer;

    procedure SetCurveType(AIndex: Integer; const AValue: Integer);
    procedure SetPoints(AIndex1, AIndex2, AIndex3: Integer; const AValue: Integer);
  public
    FCurve: array [0..4, 0.. 255] of Byte;

    constructor Create;

    procedure ChannelReset(const AChannel: Integer);
    procedure CalculateCurve(const AChannel: Integer);
    function LUTFunc(const NChannels, AChannel: Integer; const AValue: Double): Double;

    property CurveType[index: Integer]           : Integer read GetCurveType write SetCurveType;
    property Points[index, index, index: Integer]: Integer read GetPoints    write SetPoints;
  end;

implementation

uses
  Math, gmGimpCommonFuncs;

const
  {  private constants  }

  CR_BASIS: TCRMatrix = ( (-0.5,  1.5, -1.5,  0.5),
                          ( 1.0, -2.5,  2.0, -0.5),
                          (-0.5,  0.0,  0.5,  0.0),
                          ( 0.0,  1.0,  0.0,  0.0) );

//-- private functions ---------------------------------------------------------

procedure CurvesCRCompose(const a, b: TCRMatrix; var ab: TCRMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
    begin
      ab[i, j] := (a[i, 0] * b[0, j] +
                   a[i, 1] * b[1, j] +
                   a[i, 2] * b[2, j] +
                   a[i, 3] * b[3, j]);
    end;
  end;
end; 

//-- TgmCurves -----------------------------------------------------------------

constructor TgmCurves.Create;
begin
  inherited Create;
  Init;
end;

procedure TgmCurves.Init;
var
  LChannel: Integer;
begin
  for LChannel := GIMP_HISTOGRAM_VALUE to GIMP_HISTOGRAM_ALPHA do
  begin
    FCurveType[LChannel] := GIMP_CURVE_SMOOTH;
    ChannelReset(LChannel);
  end;
end;

procedure TgmCurves.ChannelReset(const AChannel: Integer);
var
  j: Integer;
begin
  for j := 0 to 255 do
  begin
    FCurve[AChannel, j] := j;
  end;

  for j := 0 to CURVES_NUM_POINTS - 1 do
  begin
    FPoints[AChannel, j, 0] := -1;
    FPoints[AChannel, j, 1] := -1;
  end;

  FPoints[AChannel, 0, 0] := 0;
  FPoints[AChannel, 0, 1] := 0;
  FPoints[AChannel, CURVES_NUM_POINTS - 1, 0] := 255;
  FPoints[AChannel, CURVES_NUM_POINTS - 1, 1] := 255;
end;

procedure TgmCurves.CalculateCurve(const AChannel: Integer);
var
  i, x, y       : Integer;
  LPoints       : array [0..CURVES_NUM_POINTS - 1] of Integer;
  LNumPts       : Integer;
  p1, p2, p3, p4: Integer;
begin
  if FCurveType[AChannel] = GIMP_CURVE_SMOOTH then
  begin
    {  cycle through the curves  }
    LNumPts := 0;
    for i := 0 to (CURVES_NUM_POINTS - 1) do
    begin
      if FPoints[AChannel, i, 0] <> -1 then
      begin
        LPoints[LNumPts] := i;
        Inc(LNumPts);
      end;
    end;

    {  Initialize boundary curve points }
    if LNumPts <> 0 then
    begin
      for i := 0 to (FPoints[ AChannel, LPoints[0], 0 ] - 1) do
      begin
        FCurve[AChannel, i] := FPoints[ AChannel, LPoints[0], 1 ];
      end;
        
      for i := FPoints[ AChannel, LPoints[LNumPts - 1], 0] to 255 do
      begin
	      FCurve[AChannel, i] := FPoints[ AChannel, LPoints[LNumPts - 1], 1 ];
      end;
    end;

    for i := 0 to (LNumPts - 2) do
    begin
      if i = 0 then
      begin
        p1 := LPoints[i];
      end
      else
      begin
        p1 := LPoints[i - 1];
      end;

      p2 := LPoints[i];
      p3 := LPoints[i + 1];
      
      if i = (LNumPts - 2) then
      begin
        p4 := LPoints[LNumPts - 1];
      end
      else
      begin
        p4 := LPoints[i + 2];
      end;

      PlotCurve(AChannel, p1, p2, p3, p4);
    end;

    { ensure that the control points are used exactly }
    for i := 0 to (LNumPts - 1) do
    begin
      x := FPoints[ AChannel, LPoints[i], 0 ];
      y := FPoints[ AChannel, LPoints[i], 1 ];

      FCurve[AChannel, x] := y;
    end;
  end;
end;

procedure TgmCurves.PlotCurve(const AChannel, p1, p2, p3, p4: Integer);
var
  LGeometry      : TCRMatrix;
  LTmp1, LTmp2   : TCRMatrix;
  LDeltas        : TCRMatrix;
  x, dx, dx2, dx3: Double;
  y, dy, dy2, dy3: Double;
  d, d2, d3      : Double;
  LLastX, LLastY : Integer;
  LNewX, LNewY   : Integer;
  i              : Integer;
begin
  { construct the geometry matrix from the segment }
  for i := 0 to 3 do
  begin
    LGeometry[i, 2] := 0;
    LGeometry[i, 3] := 0;
  end;

  for i := 0 to 1 do
  begin
    LGeometry[0, i] := FPoints[AChannel, p1, i];
    LGeometry[1, i] := FPoints[AChannel, p2, i];
    LGeometry[2, i] := FPoints[AChannel, p3, i];
    LGeometry[3, i] := FPoints[AChannel, p4, i];
  end;

  { subdivide the curve 1000 times }
  { n can be adjusted to give a finer or coarser curve }
  d  := 1.0 / 1000;
  d2 := d * d;
  d3 := d * d * d;

  { construct a temporary matrix for determining the forward differencing deltas }
  LTmp2[0, 0] := 0;
  LTmp2[0, 1] := 0;
  LTmp2[0, 2] := 0;
  LTmp2[0, 3] := 1;

  LTmp2[1, 0] := d3;
  LTmp2[1, 1] := d2;
  LTmp2[1, 2] := d;
  LTmp2[1, 3] := 0;

  LTmp2[2, 0] := 6 * d3;
  LTmp2[2, 1] := 2 * d2;
  LTmp2[2, 2] := 0;
  LTmp2[2, 3] := 0;

  LTmp2[3, 0] := 6 * d3;
  LTmp2[3, 1] := 0;
  LTmp2[3, 2] := 0;
  LTmp2[3, 3] := 0;

  { compose the basis and geometry matrices }
  CurvesCRCompose(CR_BASIS, LGeometry, LTmp1);

  { compose the above results to get the deltas matrix }
  CurvesCRCompose(LTmp2, LTmp1, LDeltas);

  { extract the x deltas }
  x   := LDeltas[0, 0];
  dx  := LDeltas[1, 0];
  dx2 := LDeltas[2, 0];
  dx3 := LDeltas[3, 0];

  { extract the y deltas }
  y   := LDeltas[0, 1];
  dy  := LDeltas[1, 1];
  dy2 := LDeltas[2, 1];
  dy3 := LDeltas[3, 1];

  LLastX := CLAMP( Round(x), 0, 255 );
  LLastY := CLAMP( Round(y), 0, 255 );

  FCurve[AChannel, LLastX] := LLastY;

  { loop over the curve }
  for i := 0 to 999 do
  begin
    { increment the x values }
    x   := x   + dx;
    dx  := dx  + dx2;
    dx2 := dx2 + dx3;

    { increment the y values }
    y   := y   + dy;
    dy  := dy  + dy2;
    dy2 := dy2 + dy3;

    LNewX := CLAMP0255( Round(x) );
    LNewY := CLAMP0255( Round(y) );

    { if this point is different than the last one...then draw it }
    if (LLastX <> LNewX) or (LLastY <> LNewY) then
    begin
      FCurve[AChannel, LNewX] := LNewY;
    end;

    LLastX := LNewX;
    LLastY := LNewY;
  end;
end; 

function TgmCurves.LUTFunc(const NChannels, AChannel: Integer;
  const AValue: Double): Double;
var
  f, LInten: Double;
  j, LIndex: Integer;
begin
  if NChannels <= 2 then
  begin
    j := AChannel;
  end
  else
  begin
    j := AChannel + 1;
  end;

  LInten := AValue;

  { For RGB and RGBA images this runs through the loop with j = channel + 1
    the first time and j = 0 the second time

    For GRAY images this runs through the loop with j = 0 the first and
    only time }
  while (j >= 0) do
  begin
    { don't apply the overall curve to the alpha channel }
    if (j = 0) and
       ( (NChannels = 2) or (NChannels = 4) ) and
       (AChannel = NChannels - 1) then
    begin
      Break;
    end;

    if LInten < 0.0 then
    begin
      LInten := FCurve[j, 0] / 255.0;
    end
    else if LInten >= 1.0 then
    begin
      LInten := FCurve[j, 255] / 255.0;
    end
    else { interpolate the curve }
    begin
      LIndex := Floor(LInten * 255.0);
      f      := LInten * 255.0 - LIndex;
      LInten := ( (1.0 - f) * FCurve[j, LIndex] + f * FCurve[j, LIndex + 1] ) / 255.0;
    end;

    j := j - (AChannel + 1);
  end;

  Result := LInten;
end; 

function TgmCurves.GetCurveType(AIndex: Integer): Integer;
begin
  if AIndex in [0..4] then
  begin
    Result := FCurveType[AIndex];
  end
  else
  begin
    Result := FCurveType[0];
  end;
end; 

function TgmCurves.GetPoints(AIndex1, AIndex2, AIndex3: Integer): Integer;
begin
  if (AIndex1 in [0..4]) and
     (AIndex2 in [0..CURVES_NUM_POINTS - 1]) and
     (AIndex3 in [0..1]) then
  begin
    Result := FPoints[AIndex1, AIndex2, AIndex3];
  end
  else
  begin
    Result := FPoints[0, 0, 0];
  end;
end;

procedure TgmCurves.SetCurveType(AIndex: Integer; const AValue: Integer);
begin
  if AIndex in [0..4] then
  begin
    if FCurveType[AIndex] <> AValue then
    begin
      FCurveType[AIndex] := AValue;
    end;
  end;
end;

procedure TgmCurves.SetPoints(AIndex1, AIndex2, AIndex3: Integer;
  const AValue: Integer);
begin
  if (AIndex1 in [0..4]) and
     (AIndex2 in [0..CURVES_NUM_POINTS - 1]) and
     (AIndex3 in [0..1]) then
  begin
    if FPoints[AIndex1, AIndex2, AIndex3] <> AValue then
    begin
      FPoints[AIndex1, AIndex2, AIndex3] := AValue;
    end;
  end;
end; 

end.
