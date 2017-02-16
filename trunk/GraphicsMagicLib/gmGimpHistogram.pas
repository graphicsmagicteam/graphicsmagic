{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on:
  "base\gimphistogram.h",
  "base\gimphistogram.c",
  "app\widgets\gimphistogramview.h",
  "app\widgets\gimphistogramview.c",
  "base\widgets\widgets-enums.h"
  from GIMP 2.2.10 . The original source can be found at www.gimp.org.

  gimphistogram module Copyright (C) 1999 Jay Cox <jaycox@earthlink.net>

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
  Boston, MA 02111-1307, USA.}

unit gmGimpHistogram;

interface

uses
  GR32;

type
  TgmGimpHistogramScale = (GIMP_HISTOGRAM_SCALE_LINEAR,      { < desc="Linear"      > }
                           GIMP_HISTOGRAM_SCALE_LOGARITHMIC  { < desc="Logarithmic" > });

  TgmGimpHistogram = class(TObject)
  private
    FHistogramMap   : TBitmap32;
    FLineColor      : TColor32;
    FBackgroundColor: TColor32;
    FScale          : TgmGimpHistogramScale;
    FValues         : array of array of Double;
    FNChannels      : Integer;

    procedure gimp_histogram_alloc_values(const ABytes: Integer);
    procedure gimp_histogram_free_values;
    procedure gimp_histogram_calculate_sub_region(ARegion: TBitmap32);
    procedure gimp_histogram_view_draw_spike(AChannel, x, i, j, AMax, AHeight: Integer);

    function gimp_histogram_get_maximum(AChannel: Integer): Double;
    function gimp_histogram_view_get_maximum(AChannel: Integer): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure gimp_histogram_calculate(ARegion: TBitmap32);
    procedure gimp_histogram_view_expose(const AChannel: Integer);

    function gimp_histogram_get_count(AChannel, AStart, AEnd: Integer): Double;
    function gimp_histogram_get_value(AChannel, ABin: Integer): Double;

    property HistogramMap   : TBitmap32             read FHistogramMap;
    property LineColor      : TColor32              read FLineColor       write FLineColor;
    property BackgroundColor: TColor32              read FBackgroundColor write FBackgroundColor;
    property Scale          : TgmGimpHistogramScale read FScale           write FScale;
  end;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic Lib }
  gmGimpBaseEnums, gmGimpCommonFuncs;


procedure TgmGimpHistogram.gimp_histogram_alloc_values(const ABytes: Integer);
var
  i: Integer;
begin
  if (ABytes + 1) <> FNChannels then
  begin
    gimp_histogram_free_values;

    FNChannels := ABytes + 1;
    SetLength(FValues, FNChannels);

    for i := 0 to (FNChannels - 1) do
    begin
      SetLength(FValues[i], 256);
    end;
  end;
end; 

procedure TgmGimpHistogram.gimp_histogram_free_values;
var
  i: Integer;
begin
  if High(FValues) > (-1) then
  begin
    for i := 0 to (FNChannels - 1) do
    begin
      SetLength(FValues[i], 0);
      FValues[i] := nil;
    end;
    
    SetLength(FValues, 0);
    
    FValues    := nil;
    FNChannels := 0;
  end;
end;

procedure TgmGimpHistogram.gimp_histogram_calculate_sub_region(
  ARegion: TBitmap32);
var
  LSrc      : PColor32Array;
  r, g, b   : Byte;
  h, w, LMax: Integer;
begin
{$RANGECHECKS OFF}

  { no mask }
  for h := 0 to (ARegion.Height - 1) do
  begin
    LSrc := ARegion.ScanLine[h];
    
    { calculate separate value values }
    for w := 0 to (ARegion.Width - 1) do
    begin
      r             := LSrc[w] shr 16 and $FF;
      g             := LSrc[w] shr  8 and $FF;
      b             := LSrc[w]        and $FF;
      FValues[1, r] := FValues[1, r] + 1.0;
      FValues[2, g] := FValues[2, g] + 1.0;
      FValues[3, b] := FValues[3, b] + 1.0;

      if r > g then
      begin
        LMax := r;
      end
      else
      begin
        LMax := g;
      end;

      if b > LMax then
      begin
        FValues[0, b] := FValues[0, b] + 1.0
      end
      else
      begin
        FValues[0, LMax] := FValues[0, LMax] + 1.0;
      end;
    end;
  end;

{$RANGECHECKS ON}  
end;  

procedure TgmGimpHistogram.gimp_histogram_view_draw_spike(
  AChannel, x, i, j, AMax, AHeight: Integer);
const
  e = 2.71828;  // the base of the logarithm
var
  LValue: Double;
  y     : Integer;
  v     : Double;
begin
  LValue := 0.0;

  repeat
    v := gimp_histogram_get_value(AChannel, i);
    Inc(i);

    if v > LValue then
    begin
      LValue := v;
    end;
  until (i >= j);

  if LValue <= 0.0 then
  begin
    Exit;
  end;

  case FScale of
    GIMP_HISTOGRAM_SCALE_LINEAR:
      begin
        y := Round( (AHeight - 1) * LValue / AMax );
      end;
      
    GIMP_HISTOGRAM_SCALE_LOGARITHMIC:
      begin
        y := Round( (AHeight - 1) * LogN(e, LValue) / AMax );
      end;
      
  else
    y := 0;
  end;

  FHistogramMap.LineS(x, AHeight - 1, x, AHeight - y - 1, FLineColor);
end; 

function TgmGimpHistogram.gimp_histogram_get_value(
  AChannel, ABin: Integer): Double;
begin
  Result := FValues[AChannel, ABin];
end;

function TgmGimpHistogram.gimp_histogram_get_maximum(AChannel: Integer): Double;
var
  LMax: Double;
  x   : Integer;
begin
  LMax := 0.0;

  {  the gray alpha channel is in slot 1  }
  if (FNChannels = 3) and (AChannel = GIMP_HISTOGRAM_ALPHA) then
  begin
    AChannel := 1;
  end;

  if ( High(FValues) < 0 ) or
     ( (AChannel <> GIMP_HISTOGRAM_RGB) and (AChannel >= FNChannels) ) then
  begin
    Result := 0.0;
    Exit;
  end;

  for x := 0 to 255 do
  begin
    if FValues[AChannel, x] > LMax then
    begin
      LMax := FValues[AChannel, x];
    end;
  end;

  Result := LMax;
end; 

function TgmGimpHistogram.gimp_histogram_view_get_maximum(
  AChannel: Integer): Double;
const
  e = 2.71828;  // the base of the logarithm
var
  LMax: Double;
begin
  LMax := gimp_histogram_get_maximum(AChannel);

  if FScale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC then
  begin
    if LMax > 0.0 then
    begin
      LMax := LogN(e, LMax);
    end
    else
    begin
      LMax := 1.0;
    end;
  end;

  Result := LMax;
end;

constructor TgmGimpHistogram.Create;
begin
  inherited Create;

  FHistogramMap        := TBitmap32.Create;
  FHistogramMap.Width  := GRAPH_SIZE;
  FHistogramMap.Height := GRAPH_SIZE;

  FLineColor       := clLightGray32;
  FBackgroundColor := clWhite32;

  FValues    := nil;
  FNChannels := 0;
  FScale     := GIMP_HISTOGRAM_SCALE_LINEAR;
end; 

destructor TgmGimpHistogram.Destroy;
begin
  FHistogramMap.Free;
  gimp_histogram_free_values;
  
  inherited Destroy;
end;

procedure TgmGimpHistogram.gimp_histogram_calculate(ARegion: TBitmap32);
var
  i, j: Integer;
begin
  gimp_histogram_alloc_values(3);

  for i := 0 to FNChannels - 1 do
  begin
    for j := 0 to 255 do
    begin
      FValues[i, j] := 0.0;
    end;
  end;

  gimp_histogram_calculate_sub_region(ARegion);
end; 

procedure TgmGimpHistogram.gimp_histogram_view_expose(const AChannel: Integer);
var
  x, i, j        : Integer;
  LWidth, LHeight: Integer;
  LMax           : Double;
begin
  { The fill code is added by ourselves -- clearing the last map. }
  FHistogramMap.FillRectS(FHistogramMap.Canvas.ClipRect, FBackgroundColor);

  LWidth  := FHistogramMap.Width;
  LHeight := FHistogramMap.Height;

  LMax := gimp_histogram_view_get_maximum(AChannel);

  for x := 0 to (LWidth - 1) do
  begin
    i := (x * 256) div LWidth;
    j := ( (x + 1) * 256 ) div LWidth;

    gimp_histogram_view_draw_spike(AChannel, x, i, j, Round(LMax), LHeight);
  end;
end;

function TgmGimpHistogram.gimp_histogram_get_count(
  AChannel, AStart, AEnd: Integer): Double;
var
  i     : Integer;
  LCount: Double;
begin
  LCount := 0.0;

  //  the gray alpha channel is in slot 1
  if (FNChannels = 3) and (AChannel = GIMP_HISTOGRAM_ALPHA) then
  begin
    AChannel := 1;
  end;

  if AChannel = GIMP_HISTOGRAM_RGB then
  begin
    Result := gimp_histogram_get_count(GIMP_HISTOGRAM_RED, AStart, AEnd) +
	            gimp_histogram_get_count(GIMP_HISTOGRAM_GREEN, AStart, AEnd) +
	            gimp_histogram_get_count(GIMP_HISTOGRAM_BLUE,  AStart, AEnd);

    Exit;
  end;

  if (FValues = nil) or (AStart > AEnd) or (AChannel >= FNChannels) then
  begin
    Result := 0.0;
    Exit;
  end;

  AStart := CLAMP(AStart, 0, 255);
  AEnd   := CLAMP(AEnd, 0, 255);

  for i := AStart to AEnd do
  begin
    LCount := LCount + FValues[AChannel, i];
  end;

  Result := LCount;
end; 

end.
 