unit gmGimpGaussianBlur;

{ The original code is blur-gauss.c of GIMP 2.6.0
  Copyright (C) 1995 Spencer Kimball and Peter Mattis

  This unit is based on the original C/C++ code in blur-gauss.c,
  Copyright (C) Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com > .

  Created Date: Aug. 27th, 2012
  Last update: Sep. 13th, 2012

  Contributors:

    Tommi Prami < tommi.prami@gmail.com >

  ***** BEGIN LICENSE BLOCK *****

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
  Boston, MA 02111-1307, USA.
  
  ***** END LICENSE BLOCK *****
 }

interface

uses
{ Standard }
  SysUtils, Classes,
{ Graphics32 }
  GR32;

type
  TgmBlurMethod = (gbmBlurIIR, gbmBlurRLE);

  { Quote:

    Applies a gaussian blur to the drawable, with
    specified radius of affect.  The standard deviation
    of the normal distribution used to modify pixel
    values is calculated based on the supplied radius.
    Horizontal and vertical blurring can be
    independently invoked by specifying only one to
    run.  The IIR gaussian blurring works best for
    large radius values and for images which are not
    computer-generated.

    Spencer Kimball & Peter Mattis,
    1995-1996,
   }
   
  TgmGimpGaussianBlur = class(TPersistent)
  private
    FHorizontal: Double;
    FVertical  : Double;
    FBlurMethod: TgmBlurMethod;

    procedure GaussIIR(AImage: TBitmap32);
    procedure GaussRLE(AImage: TBitmap32);
  public
    constructor Create;

    procedure Execute(AImage: TBitmap32);
    function BlurMethodString: string;

    property HorizontalRadius : Double        read FHorizontal write FHorizontal;
    property VerticalRadius   : Double        read FVertical   write FVertical;
    property BlurMethod       : TgmBlurMethod read FBlurMethod write FBlurMethod;
  end;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel;

type
  TArrayOfArrayOfDouble = array of array of Double;

{ Gaussian blur helper functions }

procedure TransferPixels(const ASrc1, ASrc2: TArrayOfArrayOfDouble;
  ADest: PColor32; const AJump, ABytes, AWidth: Integer);
var
  i, b, c: Integer;
  LSum   : array [0..3] of Byte;
begin
  for i := 0 to (AWidth - 1) do
  begin
    for b := 0 to (ABytes - 1) do
    begin
      c := Round(ASrc1[i, b] + ASrc2[i, b]);

      if c > 255 then
      begin
        c := 255
      end
      else if c < 0 then
      begin
        c := 0;
      end;

      LSum[b] := c;
    end;

    ADest^ := (LSum[0] shl 24) or (LSum[1] shl 16) or (LSum[2] shl 8) or LSum[3];

    Inc(ADest, AJump);
  end;
end;

procedure FindIIRConstants(var n_p, n_m, d_p, d_m, bd_p, bd_m: array of Double;
  const AStdDev: Double);
var
  LDiv                   : Double;
  x0, x1, x2, x3         : Double;
  x4, x5, x6, x7         : Double;
  sum_n_p, sum_n_m, sum_d: Double;
  a, b                   : Double;
  i                      : Integer;
begin
  // The constants used in the implemenation of a casual sequence
  // using a 4th order approximation of the gaussian operator

  LDiv := Sqrt(2 * PI) * AStdDev;
  x0   := -1.783  / AStdDev;
  x1   := -1.723  / AStdDev;
  x2   :=  0.6318 / AStdDev;
  x3   :=  1.997  / AStdDev;
  x4   :=  1.6803 / LDiv;
  x5   :=  3.735  / LDiv;
  x6   := -0.6803 / LDiv;
  x7   := -0.2598 / LDiv;

  n_p[0] := x4 + x6;

  n_p[1] := (Exp(x1) * (x7 * Sin(x3) - (x6 + 2 * x4) * Cos(x3)) +
             Exp(x0) * (x5 * Sin(x2) - (2 * x6 + x4) * Cos(x2)));

  n_p[2] := (2 * Exp(x0 + x1) *
              ((x4 + x6) * Cos(x3) * Cos(x2) - x5 * Cos(x3) * Sin(x2) -
               x7 * Cos(x2) * Sin(x3)) +
              x6 * Exp(2 * x0) + x4 * Exp(2 * x1));

  n_p[3] := (Exp(x1 + 2 * x0) * (x7 * Sin(x3) - x6 * Cos(x3)) +
             Exp(x0 + 2 * x1) * (x5 * Sin(x2) - x4 * Cos(x2)));

  n_p[4] := 0.0;

  d_p[0] := 0.0;
  d_P[1] := -2 * Exp(x1) * Cos(x3) - 2 * Exp(x0) * Cos(x2);
  d_p[2] := 4 * Cos(x3) * Cos(x2) * Exp(x0 + x1) + Exp(2 * x1) + Exp(2 * x0);
  d_p[3] := -2 * Cos(x2) * Exp(x0 + 2 * x1) - 2 * Cos(x3) * Exp(x1 + 2 * x0);
  d_p[4] := Exp(2 * x0 + 2 * x1);

  for i := 0 to 4 do
  begin
    d_m[i] := d_p[i];
  end;

  n_m[0] := 0.0;

  for i := 1 to 4 do
  begin
    n_m[i] := n_p[i] - d_p[i] * n_p[0];
  end;

  sum_n_p := 0.0;
  sum_n_m := 0.0;
  sum_d   := 0.0;

  for i := 0 to 4 do
  begin
    sum_n_p := sum_n_p + n_p[i];
    sum_n_m := sum_n_m + n_m[i];
    sum_d   := sum_d + d_p[i];
  end;

  a := sum_n_p / (1.0 + sum_d);
  b := sum_n_m / (1.0 + sum_d);

  for i := 0 to 4 do
  begin
    bd_p[i] := d_p[i] * a;
    bd_m[i] := d_m[i] * b;
  end;
end;

// Convert from separated to premultiplied alpha, on a single scan line. 
procedure MultiplyAlpha(AImage: TBitmap32);
const
  ONE_ABOVE_255 = 1.0 / 255.0;
var
  i         : Integer;
  fa        : Double;
  a, r, g, b: Byte;
  p         : PColor32;
begin
  p := @AImage.Bits[0];
  
  for i := 0 to (AImage.Width * AImage.Height - 1) do
  begin
    a := p^ shr 24 and $FF;
    r := p^ shr 16 and $FF;
    g := p^ shr  8 and $FF;
    b := p^        and $FF;

    fa := a * ONE_ABOVE_255;
    r  := Round(r * fa);
    g  := Round(g * fa);
    b  := Round(b * fa);

    p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;

    Inc(p);
  end;
end;

// Convert from premultiplied to separated alpha, on a single scan line.
procedure SeparateAlpha(AImage: TBitmap32);
var
  i, nr, ng, nb: Integer;
  a, r, g, b   : Byte;
  LRecipeAlpha : Double;
  p            : PColor32;
begin
  p := @AImage.Bits[0];

  for i := 0 to (AImage.Width * AImage.Height - 1) do
  begin
    a := p^ shr 24 and $FF;

    if (a > 0) and (a < 255) then
    begin
      LRecipeAlpha := 255.0 / a;

      r  := p^ shr 16 and $FF;
      g  := p^ shr  8 and $FF;
      b  := p^        and $FF;

      nr := Round(r * LRecipeAlpha);
      ng := Round(g * LRecipeAlpha);
      nb := Round(b * LRecipeAlpha);

      r  := Clamp(nr, 255);
      g  := Clamp(ng, 255);
      b  := Clamp(nb, 255);

      p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;
    end;

    Inc(p);
  end;
end;

{
  make_rle_curve(sigma, &curve, &length, &sum, &total)


  Fill the Gauss curve.

                g(r) = exp (- r^2 / (2 * sigma^2))
                   r = sqrt (x^2 + y ^2)

  o length is filled with the length the curve (in both directions)
  o curve[-length .. length] is allocated and filled with the
    (scaled) gauss curve.
  o sum[-length .. length]   is allocated and filled with the 'summed' curve.
  o total is filled with the sum of all elements in the curve (for
    normalization).
    
 }
procedure MakeRLECurve(const ASigma: Double;
  var ACurve: TArrayOfInteger; var ALength: Integer;
  var ASum: TArrayOfInteger; var ATotal, ACenter: Integer);
var
  LSigma2, L     : Double;
  LTemp, LLength : Integer;
  i, n           : Integer;
  LSum, LCurve   : TArrayOfInteger;
begin
  LSigma2      := 2 * ASigma * ASigma;
  L            := Sqrt( -LSigma2 * Ln(1.0 / 255.0) );
  n            := Ceil(L) * 2;
  LLength      := n div 2;
  ACenter      := LLength;  

  if (n mod 2) = 0 then
  begin
    n := n + 1;
  end;

  SetLength(LCurve, n);

  LCurve[ACenter] := 255;
  for i := 1 to LLength do
  begin
    LTemp := Trunc( Exp(-(i * i) / LSigma2) * 255 );

    LCurve[-i + ACenter] := LTemp;
    LCurve[ i + ACenter] := LTemp;
  end;

  SetLength(LSum, 2 * LLength + 1);

  LSum[0] := 0;
  for i := 1 to (LLength * 2) do
  begin
    LSum[i] := LCurve[i - LLength - 1 + ACenter] + LSum[i - 1];
  end;

  ATotal  := LSum[LLength + ACenter] - LSum[-LLength + ACenter];
  ACurve  := LCurve;
  ASum    := LSum;
  ALength := LLength;
end;

{
  run_length_encode (src, rle, pix, dist, width, border, pack);

  Copy 'width' 8bit pixels from 'src' to 'pix' and extend both sides
  by 'border' pixels so 'pix[]' is filled from '-border' to 'width+border-1'.

  'dist' is the distance between the pixels in 'src'.

  If 'pack' is TRUE, then 'rle' is filled with a run-length encoding
  of the pixels. In plain english, that means that rle[i] gives the
  number of times the same pixel is found pix[i], pix[i+1], ...  A
  standalone pixel has a rle value of 1.

  The function returns the number of times 2 identical consecutive pixels
  were found.

  Note: The function must be inlined to insure that all tests on
        'pack' are efficiently resolved by the compiler (they are in
        the critical loop).  As a consequence, the function should
        only be called with known constant value for 'pack'.  In the
        current implementation, 'pack' is always TRUE but it might be
        more efficient to have an 'adaptive' algorithm that switches
        to FALSE when the run-length is innefficient.
 }
function RunLengthEncode(ASrc: PByte; ARLE, APix: TArrayOfInteger;
  const ADist, AWidth, ABorder, ACenter: Integer;
  const APack: Boolean): Integer;
var
  LLast, LCount, LSame : Integer;
  i, c                 : Integer;
  LRLEIndex, LPixIndex : Integer;
begin
  LSame     := 0;
  LRLEIndex := ACenter;
  LPixIndex := ACenter;
  
  Inc( ASrc, ADist * (AWidth - 1) );

  if APack then
  begin
    LRLEIndex := LRLEIndex + AWidth + ABorder - 1;
  end;

  LPixIndex := LPixIndex + AWidth + ABorder - 1;

  LLast  := ASrc^;
  LCount := 0;

  // the 'end' border
  for i := 0 to (ABorder - 1) do
  begin
    Inc(LCount);

    APix[LPixIndex] := LLast;
    LPixIndex       := LPixIndex - 1;;

    if APack then
    begin
      ARLE[LRLEIndex] := LCount;
      LRLEIndex       := LRLEIndex - 1;
    end;
  end;

  // the real pixels
  for i := 0 to (AWidth - 1) do
  begin
    c := ASrc^;
    Dec(ASrc, ADist);

    if APack and (c = LLast) then
    begin
      LCount := LCount + 1;
      APix[LPixIndex] := LLast;
      ARLE[LRLEIndex] := LCount;

      LPixIndex := LPixIndex - 1;
      LRLEIndex := LRLEIndex - 1;
      LSame     := LSame + 1;
    end
    else
    begin
      LCount          := 1;
      LLast           := c;
      APix[LPixIndex] := LLast;
      LPixIndex       := LPixIndex - 1;

      if APack then
      begin
        ARLE[LRLEIndex] := LCount;
        LRLEIndex       := LRLEIndex - 1;
      end;
    end;
  end;

  // the start pixels
  for i := 0 to (ABorder - 1) do
  begin
    LCount          := LCount + 1;
    APix[LPixIndex] := LLast;
    LPixIndex       := LPixIndex - 1;

    if APack then
    begin
      ARLE[LRLEIndex] := LCount;
      LRLEIndex       := LRLEIndex - 1;
    end;
  end;

  Result := LSame;
end;

procedure DoEncodedLRE(AEnc, ASrc: TArrayOfInteger; ADest: PByte;
  const AWidth, ALength, ADist: Integer;
  ACurve: TArrayOfInteger; const ACTotal: Integer;
  ACSum: TArrayOfInteger; const ACenter: Integer);
var
  LCol, LStart, LVal   : Integer;
  i, nb, s1, s2        : Integer;
  LEncIndex, LSrcIndex : Integer;
begin
  LStart := -ALength;
  LVal   := ACTotal div 2;
  
  for LCol := 0 to (AWidth - 1) do
  begin
    LEncIndex := LCol + LStart + ACenter;
    LSrcIndex := LEncIndex;

    s1 := ACSum[LStart + ACenter];
    nb := AEnc[LEncIndex];
    i  := LStart + nb;

    while (i <= ALength) do
    begin
      s2   := ACSum[i + ACenter];
      Lval := LVal + ASrc[LSrcIndex] * (s2 - s1);
      s1   := s2;

      LEncIndex := LEncIndex + nb;
      LSrcIndex := LSrcIndex + nb;

      nb := AEnc[LEncIndex];
      i  := i + nb;
    end;

    LVal := LVal + ASrc[LSrcIndex] * (ACSum[ALength + ACenter] - s1);
    LVal := LVal div ACTotal;

    ADest^ := Min(LVal, 255);

    Inc(ADest, ADist);
  end;
end;

procedure DoFullLRE(ASrc: TArrayOfInteger; ADest: PByte;
  const AWidth, ALength, ADist: Integer; ACurve: TArrayOfInteger;
  const ACTotal, ACenter: Integer);
var
  i, LCol, LVal   : Integer;
  x1_i, x2_i, c_i : Integer;
  x1, x2, c       : TArrayOfInteger;
begin
  c    := ACurve;
  x1   := ASrc;
  x2   := x1;

  for LCol := 0 to (AWidth - 1) do
  begin
    // The central point is a special case since it should only be
    // processed ONCE
    LVal := ACTotal div 2;

    c_i  := ACenter;
    x1_i := LCol + ACenter;
    x2_i := x1_i;

    LVal := LVal + x1[0 + x1_i] * c[0 + c_i];

    c_i  := c_i + 1;
    x1_i := x1_i + 1;
    x2_i := x2_i - 1;
    i    := ALength;

    // Processing multiple points in a single iteration should be
    // faster but is not strictly required.
    // Some precise benchmarking will be needed to figure out
    // if this is really interesting.
    while (i >= 8) do
    begin
      LVal := LVal + (x1[0 + x1_i] + x2[-0 + x2_i]) * c[0 + c_i];
      LVal := LVal + (x1[1 + x1_i] + x2[-1 + x2_i]) * c[1 + c_i];
      LVal := LVal + (x1[2 + x1_i] + x2[-2 + x2_i]) * c[2 + c_i];
      LVal := LVal + (x1[3 + x1_i] + x2[-3 + x2_i]) * c[3 + c_i];
      LVal := LVal + (x1[4 + x1_i] + x2[-4 + x2_i]) * c[4 + c_i];
      LVal := LVal + (x1[5 + x1_i] + x2[-5 + x2_i]) * c[5 + c_i];
      LVal := LVal + (x1[6 + x1_i] + x2[-6 + x2_i]) * c[6 + c_i];
      LVal := LVal + (x1[7 + x1_i] + x2[-7 + x2_i]) * c[7 + c_i];

      c_i  := c_i + 8;
      x1_i := x1_i + 8;
      x2_i := x2_i - 8;
      i    := i - 8;
    end;

    while (i >= 4) do
    begin
      LVal := LVal + (x1[0 + x1_i] + x2[-0 + x2_i]) * c[0 + c_i];
      LVal := LVal + (x1[1 + x1_i] + x2[-1 + x2_i]) * c[1 + c_i];
      LVal := LVal + (x1[2 + x1_i] + x2[-2 + x2_i]) * c[2 + c_i];
      LVal := LVal + (x1[3 + x1_i] + x2[-3 + x2_i]) * c[3 + c_i];

      c_i  := c_i + 4;
      x1_i := x1_i + 4;
      x2_i := x2_i - 4;
      i    := i - 4;
    end;

    // Only that final loop is strictly required

    while (i >= 1) do
    begin
      // process the pixels at the distance i before and after the
      // central point. They must have the same coefficient
      LVal := LVal + (x1[0 + x1_i] + x2[-0 + x2_i]) * c[0 + c_i];

      c_i  := c_i + 1;
      x1_i := x1_i + 1;
      x2_i := x2_i - 1;
      i    := i - 1;
    end;

    LVal   := LVal div ACTotal;
    ADest^ := Min(LVal, 255);

    Inc(ADest, ADist);
  end;       
end;

{ TgmGimpGaussianBlur }

constructor TgmGimpGaussianBlur.Create;
begin
  inherited Create;

  FHorizontal := 5.0;  // x radius
  FVertical   := 5.0;  // y radius
  FBlurMethod := gbmBlurIIR;
end;

procedure TgmGimpGaussianBlur.GaussIIR(AImage: TBitmap32);
var
  i, j, LLength, LOffset : Integer;
  LRow, LCol, k          : Integer;
  LTerms, LLastIndex     : Integer;
  LHorz, LVert           : Double;
  LStdDev                : Double;
  n_p, n_m               : array [0..4] of Double;
  d_p, d_m               : array [0..4] of Double;
  bd_p, bd_m             : array [0..4] of Double;
  initial_p, initial_m   : array [0..3] of Integer;
  val_p, val_m           : TArrayOfArrayOfDouble;
  LARGBData              : array of array of Byte;
  LImageBit              : PColor32;
begin
  if not Assigned(AImage) then
  begin
    Exit;
  end;

  LHorz   := FHorizontal;
  LVert   := FVertical;
  LLength := Max(AImage.Width, AImage.Height);

  SetLength(val_p, LLength);
  SetLength(val_m, LLength);

  for i := 0 to (LLength - 1) do
  begin
    // for alpha, red, green and blue components
    SetLength(val_p[i], 4);
    SetLength(val_m[i], 4);
  end;

  try
    // Fist the vertical pass
    
    SetLength(LARGBData, AImage.Height);
    try
      LLastIndex := AImage.Height - 1;
      for i := 0 to LLastIndex do
      begin
        SetLength(LARGBData[i], 4);
      end;

      if LVert > 0.0 then
      begin
        LVert   := Abs(LVert) + 1.0;
        LStdDev := Sqrt( -(LVert * LVert) / (2 * Ln(1.0 / 255.0)) );
      end;

      // derive the constants for calculating the gaussian
      // from the std dev
      FindIIRConstants(n_p, n_m, d_p, d_m, bd_p, bd_m, LStdDev);

      MultiplyAlpha(AImage);

      for LCol := 0 to (AImage.Width - 1) do
      begin
        for i := 0 to LLastIndex do
        begin
          for j := 0 to 3 do
          begin
            val_p[i, j] := 0.0;
            val_m[i, j] := 0.0;
          end;
        end;

        // get each color channel of pixels of a column
        for i := 0 to LLastIndex do
        begin
          LARGBData[i, 0] := AImage.PixelS[LCol, i] shr 24 and $FF;
          LARGBData[i, 1] := AImage.PixelS[LCol, i] shr 16 and $FF;
          LARGBData[i, 2] := AImage.PixelS[LCol, i] shr  8 and $FF;
          LARGBData[i, 3] := AImage.PixelS[LCol, i]        and $FF;
        end;

        // Set up the first vals
        for i := 0 to 3 do
        begin
          initial_p[i] := LARGBData[0, i];
          initial_m[i] := LARGBData[LLastIndex, i];
        end;

        for LRow := 0 to LLastIndex do
        begin
          if LRow < 4 then
          begin
            LTerms := LRow;
          end
          else
          begin
            LTerms := 4;
          end;

          for k := 0 to 3 do
          begin
            for i := 0 to LTerms do
            begin
              val_p[LRow, k] := val_p[LRow, k] + n_p[i] * LARGBData[-i + LRow, k] -
                d_p[i] * val_p[-i + LRow, k];

              val_m[LLastIndex - LRow, k] := val_m[LLastIndex - LRow, k] +
                n_m[i] * LARGBData[i + LLastIndex - LRow, k] -
                  d_m[i] * val_m[i + LLastIndex - LRow, k];
            end;

            for j := i to 4 do
            begin
              val_p[LRow, k] := val_p[LRow, k] + (n_p[j] - bd_p[j]) * initial_p[k];

              val_m[LLastIndex - LRow, k] := val_m[LLastIndex - LRow, k] +
                (n_m[j] - bd_m[j]) * initial_m[k];
            end;
          end;
        end;

        // get pointer to first pixel of each column
        LImageBit := @AImage.Bits[0];
        Inc(LImageBit, LCol);
        
        TransferPixels(val_p, val_m, LImageBit, AImage.Width, 4, AImage.Height);
      end;

      SeparateAlpha(AImage);

    finally
      for i := 0 to High(LARGBData) do
      begin
        SetLength(LARGBData[i], 0);
        LARGBData[i] := nil;
      end;

      SetLength(LARGBData, 0);
      LARGBData := nil;
    end;


    // Now the horizontal pass
    SetLength(LARGBData, AImage.Width);
    try
      LLastIndex := AImage.Width - 1;
      for i := 0 to LLastIndex do
      begin
        SetLength(LARGBData[i], 4);
      end;

      if LHorz > 0.0 then
      begin
        LHorz := Abs(LHorz) + 1.0;

        if (LHorz <> LVert) then
        begin
          LStdDev := Sqrt( -(LHorz * LHorz) / (2 * Ln(1.0 / 255.0)) );

          // derive the constants for calculating the gaussian
          // from the std dev
          FindIIRConstants(n_p, n_m, d_p, d_m, bd_p, bd_m, LStdDev);
        end;

        MultiplyAlpha(AImage);

        LOffset := 0;

        for LRow := 0 to (AImage.Height - 1) do
        begin
          for i := 0 to LLastIndex do
          begin
            for j := 0 to 3 do
            begin
              val_p[i, j] := 0.0;
              val_m[i, j] := 0.0;
            end;
          end;

          // get each color channel of pixels of a column
          for i := 0 to LLastIndex do
          begin
            LARGBData[i, 0] := AImage.PixelS[i, LRow] shr 24 and $FF;
            LARGBData[i, 1] := AImage.PixelS[i, LRow] shr 16 and $FF;
            LARGBData[i, 2] := AImage.PixelS[i, LRow] shr  8 and $FF;
            LARGBData[i, 3] := AImage.PixelS[i, LRow]        and $FF;
          end;

          // Set up the first vals
          for i := 0 to 3 do
          begin
            initial_p[i] := LARGBData[0, i];
            initial_m[i] := LARGBData[LLastIndex, i];
          end;

          for LCol := 0 to LLastIndex do
          begin
            if LCol < 4 then
            begin
              LTerms := LCol;
            end
            else
            begin
              LTerms := 4;
            end;

            for k := 0 to 3 do
            begin
              for i := 0 to LTerms do
              begin
                val_p[LCol, k] := val_p[LCol, k] + n_p[i] * LARGBData[-i + LCol, k] -
                  d_p[i] * val_p[-i + LCol, k];

                val_m[LLastIndex - LCol, k] := val_m[LLastIndex - LCol, k] +
                  n_m[i] * LARGBData[i + LLastIndex - LCol, k] -
                    d_m[i] * val_m[i + LLastIndex - LCol, k];
              end;

              for j := i to 4 do
              begin
                val_p[LCol, k] := val_p[LCol, k] + (n_p[j] - bd_p[j]) * initial_p[k];

                val_m[LLastIndex - LCol, k] := val_m[LLastIndex - LCol, k] +
                  (n_m[j] - bd_m[j]) * initial_m[k];
              end;
            end;
          end;

          // get pointer to first pixel of each row
          LImageBit := @AImage.Bits[0];
          Inc(LImageBit, LOffset);
          
          TransferPixels(val_p, val_m, LImageBit, 1, 4, AImage.Width);

          Inc(LOffset, AImage.Width);
        end;

        SeparateAlpha(AImage);
      end;

    finally
      for i := 0 to High(LARGBData) do
      begin
        SetLength(LARGBData[i], 0);
        LARGBData[i] := nil;
      end;

      SetLength(LARGBData, 0);
      LARGBData := nil;
    end;

  finally
    for i := 0 to (LLength - 1) do
    begin
      SetLength(val_p[i], 0);
      SetLength(val_m[i], 0);

      val_p[i] := nil;
      val_m[i] := nil;
    end;

    SetLength(val_p, 0);
    SetLength(val_m, 0);

    val_p := nil;
    val_m := nil;
  end;
end;

procedure TgmGimpGaussianBlur.GaussRLE(AImage: TBitmap32);
var
  LVert, LHorz, LStdDev : Double;
  i, b, LCenter         : Integer;
  LCol, LRow, LSame     : Integer;
  LTotal, LLength       : Integer;
  LCurve, LSum          : TArrayOfInteger;
  rle, pix              : TArrayOfInteger;
  LColorArray           : TArrayOfColor32;
  LSrc, p               : PByte;
begin
{$RANGECHECKS OFF}

  LVert := FVertical;
  LHorz := FHorizontal;

  try

    // First the vertical pass
    if LVert > 0.0 then
    begin
      LVert   := Abs(LVert) + 1.0;
      LStdDev := Sqrt( -(LVert * LVert) / (2 * Ln(1.0 / 255)) );

      MakeRLECurve(LStdDev, LCurve, LLength, LSum, LTotal, LCenter);

      SetLength(rle, AImage.Height + 2 * LLength);
      SetLength(pix, AImage.Height + 2 * LLength);

      SetLength(LColorArray, AImage.Height);
      try
        MultiplyAlpha(AImage);

        for LCol := 0 to (AImage.Width - 1) do
        begin
          // get pixel color of each column...
          for i := 0 to (AImage.Height - 1) do
          begin
            LColorArray[i] := AImage.PixelS[LCol, i];
          end;

          LSrc := @LColorArray[0];

          for b := 0 to 3 do
          begin
            p := LSrc;
            Inc(p, b);

            LSame := RunLengthEncode(p, rle, pix, 4, AImage.Height, LLength, LCenter, True);

            if LSame > (3 * AImage.Height div 4) then
            begin
              // encoded_rle is only fastest if there are a lot of
		          // repeating pixels
		          DoEncodedLRE(rle, pix, p, AImage.Height, LLength, 4,
                           LCurve, LTotal, LSum, LCenter);
            end
            else
            begin
              // else a full but more simple algorithm is better
              DoFullLRE(pix, p, AImage.Height, LLength, 4, LCurve, LTotal, LCenter);
            end;
          end;

          // set pixel color of each column...
          for i := 0 to (AImage.Height - 1) do
          begin
            AImage.PixelS[LCol, i] := LColorArray[i];
          end;
        end;

        SeparateAlpha(AImage);

      finally
        SetLength(LColorArray, 0);
        LColorArray := nil;
      end;
    end;

    // Now the horizontal pass 
    if LHorz > 0.0 then
    begin
      LHorz := Abs(LHorz) + 1.0;

      // reuse the same curve if possible else recompute a new one
      if LHorz <> LVert then
      begin
        LStdDev := Sqrt( -(LHorz * LHorz) / (2 * Ln(1.0 / 255)) );

        SetLength(LCurve, 0);
        SetLength(LSum, 0);
        LCurve := nil;
        LSum   := nil;

        MakeRLECurve(LStdDev, LCurve, LLength, LSum, LTotal, LCenter);
      end;

      SetLength(rle, AImage.Width + 2 * LLength);
      SetLength(pix, AImage.Width + 2 * LLength);

      MultiplyAlpha(AImage);

      for LRow := 0 to (AImage.Height - 1) do
      begin
        LSrc := PByte(AImage.ScanLine[LRow]);

        for b := 0 to 3 do
        begin
          p := LSrc;
          Inc(p, b);

          LSame := RunLengthEncode(p, rle, pix, 4, AImage.Width, LLength, LCenter, True);

          if LSame > (3 * AImage.Width div 4) then
          begin
            // encoded_rle is only fastest if there are a lot of
		        // repeating pixels
		        DoEncodedLRE(rle, pix, p, AImage.Width, LLength, 4,
                         LCurve, LTotal, LSum, LCenter);
          end
          else
          begin
            // else a full but more simple algorithm is better
            DoFullLRE(pix, p, AImage.Width, LLength, 4, LCurve, LTotal, LCenter);
          end;
        end;
      end;

      SeparateAlpha(AImage);
    end;

  finally
    SetLength(LCurve, 0);
    SetLength(LSum, 0);
    SetLength(rle, 0);
    SetLength(pix, 0);

    LCurve := nil;
    LSum   := nil;
    rle    := nil;
    pix    := nil;
  end;

{$RANGECHECKS ON}
end;

procedure TgmGimpGaussianBlur.Execute(AImage: TBitmap32);
var
  LMethod: TgmBlurMethod;
begin
  LMethod := FBlurMethod;

  // IIR goes wrong if the blur radius is less than 1, so we silently
  // switch to RLE in this case.
  if (FHorizontal <= 1.0) or (FVertical <= 1.0) then
  begin
    LMethod := gbmBlurRLE;
  end;

  case LMethod of
    gbmBlurIIR:
      begin
        GaussIIR(AImage);
      end;

    gbmBlurRLE:
      begin
        GaussRLE(AImage);
      end;
  end;
end;

function TgmGimpGaussianBlur.BlurMethodString: string;
begin
  case FBlurMethod of
    gbmBlurIIR : Result := 'IIR';
    gbmBlurRLE : Result := 'RLE';
    else
      raise Exception.Create('Unknown or Unsupported Blurmethod');
  end;
end;

end.
