unit gmOilPaintingFilter;

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

//
// Update Date: February 12, 2015
//   Coding out the algorithm of Oil Painting.
//                            -- Ma Xiaoguang
//
// Update Date: February 17, 2015
//   Speed up the algorithm with FillChar() and a lookup table.
//                            -- Ma Xiaoguang 
//
// TODO:
//   Optimize the algorithm for better performance.
//                            -- Ma Xiaoguang
// 

interface

{$DEFINE OIL_PAINTING_OPTIMIZATION}

uses
  GR32;

type
  TgmOilPaintingFilter = class
  private
    FSourceBitmap   : TBitmap32;
    FRadius         : Cardinal;
    FIntensityLevel : Byte;

    procedure SetRadius(ARadius: Cardinal);
  public
    constructor Create(ASourceImage: TBitmap32);
    destructor Destroy; override;

    procedure Execute(AImage: TBitmap32);

    property Radius         : Cardinal read FRadius         write SetRadius;
    property IntensityLevel : Byte     read FIntensityLevel write FIntensityLevel;
  end;


procedure OilPainting(ASrcImage, ADstImage: TBitmap32; ARadius: Cardinal = 5;
  AIntensityLevel: Byte = 20);

implementation

uses
  GR32_LowLevel;

const
  MIN_RADIUS = 1;
  MAX_RADIUS = 100;

// Based on the algorithm at here:
// http://supercomputingblog.com/graphics/oil-painting-algorithm/
//
// Quote to the original paper:
// --------------------------------------------------------------
//
// ***************************
// * The Supercomputing Blog *
// ***************************
//
// Oil Painting Algorithm
//
//    Taking an image and making it look like an oil painting is not only
//    visually impressive, but also easy, from an algorithmic point of view.
//    This page will show you how to write code to achieve the oil painting
//    effect.
//
// The parameters
//    There are two important parameters. The radius and the number of
//    levels of intensity. Like 2d convolution, for each pixel, a number of
//    pixels around that pixel are taken into account. The radius simply
//    defines how many pixels in each direction to look for. A radius of 5,
//    for example, should be good for rough oil painting pictures.
//
// Levels of intensity
//
//    For this algorithm, each pixel will be put into an intensity 'bin'.
//    The true intensity of a pixel is defined as (r+b+g)/3, and can range
//    anywhere from 0 to 256. However, oil paintings have a much more blocky
//    effect, so each pixel will have its intensity binned. For a fairly
//    blocky oil painting, 20 is a good reference number.
//
// The algorithm
//
//    Step 1
// 
//    For each pixel, all pixels within the radius will have to be examined.
//    Pixels within the radius of the current pixel will be referred to as
//    sub-pixels. For each sub-pixel, calculate the intensity, and determine
//    which intensity bin that intensity number falls into. Maintain a
//    counter for each intensity bin, which will count the number of
//    sub-pixels which fall into each intensity bin. Also maintain the total
//    red, green, and blue values for each bin, later; these may be used to
//    determine the final value of the pixel.
//
//    for (each pixel)
//    {
//        for (each pixel, within radius r of pixel)
//        {
//            int curIntensity = (int)((double)((r+g+b)/3)*intensityLevels)/255.0f;
//            intensityCount[curIntensity]++;
//            averageR[curIntensity] += r;
//            averageG[curIntensity] += g;
//            averageB[curIntensity] += b;
//        }
//    }
//
//    Step 2
//
//    For each pixel, determine which intensity bin has the most number of
//    pixels in it. Yes, this can be rolled into step 1, but for the
//    purposes of this tutorial, this way is simpler to understand the code.
//
//    //  Step 2, find the maximum level of intensity
//    // Yes, this can be rolled into step one and be slightly faster
//    // But for the purposes of this tutorial, it's simpler this way.
//    for (each level of intensity)
//    {
//        if (intensityCount[i] > curMax)
//        {
//            curMax = intensityCount[i];
//            maxIndex = i;
//        }
//    }
//
//    Step 3
// 
//    After we determine which intensity the pixel represents, as determined
//    by the intensity bin with the most pixels, we can then determine the
//    final color of the pixel by taking the total red, green, and blue values
//    in that specific bin, and dividing that by the total number of pixels in
//    that specific intensity bin.
//
//    // Step 3, calculate the final value
//    finalR = averageR[maxIndex] / curMax;
//    finalG = averageG[maxIndex] / curMax;
//    finalB = averageB[maxIndex] / curMax;
//

{$IFDEF OIL_PAINTING_OPTIMIZATION }

var
  gIntensityLUT : array [0..255] of Byte;

// optimized version
procedure OilPainting(ASrcImage, ADstImage: TBitmap32; ARadius: Cardinal = 5;
  AIntensityLevel: Byte = 20);

  procedure SetIntensityLUT;
  var
    i : Integer;
  begin
    for i := 0 to 255 do
    begin
      gIntensityLUT[i] := i * AIntensityLevel div 255;
    end;
  end;

var
  LIntensityCount : array [0..255] of Cardinal;
  LAverageR       : array [0..255] of Cardinal;
  LAverageG       : array [0..255] of Cardinal;
  LAverageB       : array [0..255] of Cardinal;
  x, y, xx, yy    : Integer;
  i, j, w, h      : Integer;
  a               : Cardinal;
  r, g, b         : Cardinal;
  LCurIntensity   : Cardinal;
  LCurMax         : Cardinal;
  LMaxIndex       : Integer;
  LColor          : TColor32;
  LRadius         : Integer;
begin
  if Assigned(ASrcImage) and
     Assigned(ADstImage) and 
     (ASrcImage.Width > 0) and
     (ASrcImage.Height > 0) and
     (ADstImage.Width = ASrcImage.Width) and
     (ADstImage.Height = ASrcImage.Height) then
  begin
    LRadius := Clamp(ARadius, MIN_RADIUS, MAX_RADIUS);

    w := ADstImage.Width;
    h := ADstImage.Height;

    SetIntensityLUT();

    for y := 0 to (h - 1) do
    begin
      for x := 0 to (w - 1) do
      begin
        FillChar(LIntensityCount, SizeOf(LIntensityCount), 0);
        FillChar(LAverageR, SizeOf(LAverageR), 0);
        FillChar(LAverageG, SizeOf(LAverageG), 0);
        FillChar(LAverageB, SizeOf(LAverageB), 0);
      
        for j := (-LRadius) to LRadius do
        begin
          yy := y + j;
          if (yy < 0) or (yy >= h) then
          begin
            Continue;
          end;
          
          for i := (-LRadius) to LRadius do
          begin
            xx := x + i;
            if (xx < 0) or (xx >= w) then
            begin
              Continue;
            end;

            LColor := ASrcImage.PixelS[xx, yy];
            r      := LColor shr 16 and $FF;
            g      := LColor shr  8 and $FF;
            b      := LColor        and $FF;

            LCurIntensity := gIntensityLUT[ ((r + g + b) div 3) ];

            LIntensityCount[LCurIntensity] := LIntensityCount[LCurIntensity] + 1;
            LAverageR[LCurIntensity]       := LAverageR[LCurIntensity] + r;
            LAverageG[LCurIntensity]       := LAverageG[LCurIntensity] + g;
            LAverageB[LCurIntensity]       := LAverageB[LCurIntensity] + b;
          end;
        end;

        LCurMax   := 0;
        LMaxIndex := 0;
        for i := 0 to 255 do
        begin
          if LIntensityCount[i] > LCurMax then
          begin
            LCurMax   := LIntensityCount[i];
            LMaxIndex := i; 
          end;
        end;

        a := ASrcImage.PixelS[x, y] and $FF000000;
        r := LAverageR[LMaxIndex] div LCurMax;
        g := LAverageG[LMaxIndex] div LCurMax;
        b := LAverageB[LMaxIndex] div LCurMax;

        ADstImage.PixelS[x, y] := a or (r shl 16) or (g shl 8) or b;
      end;
    end;
  end;
end;

{$ELSE}

// original version
procedure OilPainting(ASrcImage, ADstImage: TBitmap32; ARadius: Cardinal = 5;
  AIntensityLevel: Byte = 20);
var
  LIntensityCount : array [0..255] of Cardinal;
  LAverageR       : array [0..255] of Cardinal;
  LAverageG       : array [0..255] of Cardinal;
  LAverageB       : array [0..255] of Cardinal;
  x, y, xx, yy    : Integer;
  i, j, w, h      : Integer;
  a               : Cardinal;
  r, g, b         : Cardinal;
  LCurIntensity   : Cardinal;
  LCurMax         : Cardinal;
  LMaxIndex       : Integer;
  LColor          : TColor32;
  LRadius         : Integer;
begin
  if Assigned(ASrcImage) and
     Assigned(ADstImage) and 
     (ASrcImage.Width > 0) and
     (ASrcImage.Height > 0) and
     (ADstImage.Width = ASrcImage.Width) and
     (ADstImage.Height = ASrcImage.Height) then
  begin
    LRadius := Clamp(ARadius, MIN_RADIUS, MAX_RADIUS);

    w := ADstImage.Width;
    h := ADstImage.Height;

    for y := 0 to (h - 1) do
    begin
      for x := 0 to (w - 1) do
      begin
        for i := 0 to 255 do
        begin
          LIntensityCount[i] := 0;
          LAverageR[i]       := 0;
          LAverageG[i]       := 0;
          LAverageB[i]       := 0;
        end;

        for j := (-LRadius) to LRadius do
        begin
          yy := y + j;
          if (yy < 0) or (yy >= h) then
          begin
            Continue;
          end;
          
          for i := (-LRadius) to LRadius do
          begin
            xx := x + i;
            if (xx < 0) or (xx >= w) then
            begin
              Continue;
            end;

            LColor := ASrcImage.PixelS[xx, yy];
            r      := LColor shr 16 and $FF;
            g      := LColor shr  8 and $FF;
            b      := LColor        and $FF;

            LCurIntensity := ((r + g + b) div 3) * AIntensityLevel div 255;

            LIntensityCount[LCurIntensity] := LIntensityCount[LCurIntensity] + 1;
            LAverageR[LCurIntensity]       := LAverageR[LCurIntensity] + r;
            LAverageG[LCurIntensity]       := LAverageG[LCurIntensity] + g;
            LAverageB[LCurIntensity]       := LAverageB[LCurIntensity] + b;
          end;
        end;

        LCurMax   := 0;
        LMaxIndex := 0;
        for i := 0 to 255 do
        begin
          if LIntensityCount[i] > LCurMax then
          begin
            LCurMax   := LIntensityCount[i];
            LMaxIndex := i; 
          end;
        end;

        a := ASrcImage.PixelS[x, y] and $FF000000;
        r := LAverageR[LMaxIndex] div LCurMax;
        g := LAverageG[LMaxIndex] div LCurMax;
        b := LAverageB[LMaxIndex] div LCurMax;

        ADstImage.PixelS[x, y] := a or (r shl 16) or (g shl 8) or b;
      end;
    end;
  end;
end;

{$ENDIF}


{ TgmOilPaintingFilter }

constructor TgmOilPaintingFilter.Create(ASourceImage: TBitmap32);
begin
  inherited Create();

  FSourceBitmap := nil;

  if Assigned(ASourceImage) then
  begin
    FSourceBitmap := TBitmap32.Create();
    FSourceBitmap.Assign(ASourceImage);
  end;

  FRadius         := 5;
  FIntensityLevel := 20;
end;

destructor TgmOilPaintingFilter.Destroy;
begin
  FSourceBitmap.Free();

  inherited;
end;

procedure TgmOilPaintingFilter.Execute(AImage: TBitmap32);
begin
  if Assigned(FSourceBitmap) then
  begin
    OilPainting(FSourceBitmap, AImage, FRadius, FIntensityLevel);
  end;
end;

procedure TgmOilPaintingFilter.SetRadius(ARadius: Cardinal);
begin
  FRadius := Clamp(ARadius, MIN_RADIUS, MAX_RADIUS);
end;

end.
