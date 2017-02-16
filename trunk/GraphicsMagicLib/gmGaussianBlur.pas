{ This library is based on GBlur2.pas found in
  http://www.esanu.name/delphi/Multimedia/Graphics/Gaussian%20Blur%20in%20Delphi.html

  We adapted the source code for supporting TBitmap32.

********************************************************************************

{ Original answer from original author of this algorithm:

  Problem/Question/Abstract:

  Gaussian Blur in Delphi

  Answer:

  The gaussian kernel exp(-(x^2 + y^2)) is of the form f(x)*g(y), which means
  that you can perform a two-dimensional convolution by doing a sequence of
  one-dimensional convolutions - first you convolve each row and then each
  column. This is much faster (an N^2 becomes an N*2). Any convolution requires
  some temporary storage - below the BlurRow routine allocates and frees the
  memory, meaning that it gets allocated and freed once for each row. Probably
  changing this would speed it up some, it's not entirely clear how much.

  The kernel "size" is limited to 200 entries. In fact if you use radius
  anything like that large it will take forever - you want to try this with a
  radius = 3 or 5 or something. For a kernel with that many entries a straight
  convolution is the thing to do, while when the kernel gets much larger
  Fourier transform techniques will be better (I couldn't say what the actual
  cutoff is.)

  One comment that needs to be made is that a gaussian blur has the magical
  property that you can blur each row one by one and then blur each column -
  this is much faster than an actual 2-d convolution.

  Anyway, you can do this:
}
 
unit gmGaussianBlur;

interface

uses
  SysUtils, GR32;

  procedure GBlur32(const ADestBmp: TBitmap32; const ARadius: Double);

implementation

const
  MAX_KERNEL_SIZE = 100;

type
  TKernelSize32 = 1..MAX_KERNEL_SIZE;

  { The idea is that when using a TKernel you ignore the Weights except
    for Weights in the range -Size..Size. }
  TKernel32 = record
    Size   : TKernelSize32;
    Weights: array [-MAX_KERNEL_SIZE..MAX_KERNEL_SIZE] of Single;
  end;

{ makes K into a gaussian kernel with standard deviation = radius. For the
  current application you set MaxData = 255 and DataGranularity = 1. Now the
  procedure sets the value of K.Size so that when we use K we will ignore the
  Weights that are so small they can't possibly matter. (Small Size is good
  because the execution time is going to be propertional to K.Size.) }

procedure MakeGaussianKernel32(var K: TKernel32; const ARadius: Double;
  const AMaxData, ADataGranularity: Double);
var
  j                    : Integer;
  LLowIndex, LHighIndex: Integer;
  LTemp, LDelta        : Double;
  LKernelSize32        : TKernelSize32;
begin
  LLowIndex  := Low(K.Weights);
  LHighIndex := High(K.Weights);

  for j := LLowIndex to LHighIndex do
  begin
    LTemp        := j / ARadius;
    K.Weights[j] := Exp(-LTemp * LTemp / 2);
  end;

  { now divide by constant so sum(Weights) = 1: }
  LTemp := 0;
  for j := LLowIndex to LHighIndex do
  begin
    LTemp := LTemp + K.Weights[j];
  end;

  for j := LLowIndex to LHighIndex do
  begin
    K.Weights[j] := K.Weights[j] / LTemp;
  end;

  { now discard (or rather mark as ignorable by setting Size) the entries that
    are too small to matter. This is important, otherwise a blur with a small
    radius will take as long as with a large radius... }
  LKernelSize32 := MAX_KERNEL_SIZE;
  LDelta        := ADataGranularity / (2 * AMaxData);
  LTemp         := 0;

  while (LTemp < LDelta) and (LKernelSize32 > 1) do
  begin
    LTemp := LTemp + 2 * K.Weights[LKernelSize32];
    Dec(LKernelSize32);
  end;

  K.Size := LKernelSize32;
  
  { now just to be correct go back and jiggle again so the sum of the entries
    we'll be using is exactly 1 }
  LTemp := 0;

  for j := -K.Size to K.Size do
  begin
    LTemp := LTemp + K.Weights[j];
  end;

  for j := -K.Size to K.Size do
  begin
    K.Weights[j] := K.Weights[j] / LTemp;
  end;
end; 


function TrimInt32(const Lower, Upper, theInteger: Integer): Integer;
begin
  {
    Improved the operations for edge pixels by PAEz .

    Quote PAEz's words:
    -------------------
    I made the edges a little better by changing the trim routine to not
    repeat edge pixels but to mirror back in the row....
  }

  //-- PAEz's implimentation --

  Result := theInteger;

  while (Result < Lower) or (Result > Upper) do
  begin
    if Result > Upper then
    begin
      Result := Upper - (Result - Upper);
    end
    else if Result < 0 then
    begin
      Result := Lower - (Result - Lower);
    end;
  end;

  //-- original implementation --

  { The result of the original code seems has some problems at edge pixels
    operation, but Photoshop has the same problem. }
{  if (theInteger <= Upper) and (theInteger >= Lower) then
  begin
    Result := theInteger;
  end
  else if theInteger > Upper then
  begin
    Result := Upper;
  end
  else
  begin
    Result := Lower;
  end;
}
end;


function TrimReal32(const Lower, Upper: Integer; X: Double): Integer;
begin
  if (X < Upper) and (X >= Lower) then
  begin
    Result := Trunc(X);
  end
  else if X > Upper then
  begin
    Result := Upper;
  end
  else
  begin
    Result := Lower;
  end;
end; 


function TrimDouble(const n: Double; const AMin, AMax: Integer): Integer;
var
  rn: Integer;
begin
  rn := Round(n);

  if rn < AMin then
  begin
    Result := AMin;
  end
  else if rn > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := rn;
  end;
end;


procedure BlurRow32(var theRow: array of TColor32; const K: TKernel32);
var
  j, n          : Integer;
  ta, tr, tg, tb: Double;   {tempRed, etc}
  w             : Double;
  Index         : Integer;
  a, r, g, b    : Cardinal;
  p             : array of TColor32;
begin
  SetLength(p, High(theRow) + 1);

  for j := 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;
    ta := 0;

    for n := -K.Size to K.Size do
    begin
      w := K.Weights[n];
      { the TrimInt32 keeps us from running off the edge of the row... }

      Index := TrimInt32( 0, High(theRow), j - n );
      b     := theRow[Index]        and $FF;
      g     := theRow[Index] shr  8 and $FF;
      r     := theRow[Index] shr 16 and $FF;
      a     := theRow[Index] shr 24 and $FF;
      tb    := tb + w * b;
      tg    := tg + w * g;
      tr    := tr + w * r;
      ta    := ta + w * a;  // blend the alpha channel
    end;

    b := TrimDouble(tb, 0, 255);
    g := TrimDouble(tg, 0, 255);
    r := TrimDouble(tr, 0, 255);
    a := TrimDouble(ta, 0, 255);

    P[j] := (a shl 24) or (r shl 16) or (g shl 8) or b;
  end;

  Move(  P[0], theRow[0], ( High(theRow) + 1 ) * SizeOf(TColor32)  );
end;

procedure GBlur32(const ADestBmp: TBitmap32; const ARadius: Double);
var
  LCol, LRow: Integer;
  w, h      : Integer;
  LTheRows  : array of PColor32Array;
  LPixels   : array of TColor32;
  K         : TKernel32;
begin
{$RANGECHECKS OFF}

  MakeGaussianKernel32(K, ARadius, 255, 1);

  SetLength(LTheRows, ADestBmp.Height);

  w := ADestBmp.Width  - 1;
  h := ADestBmp.Height - 1;

  // record the location of the bitmap data...
  for LRow := 0 to h do
  begin
    LTheRows[LRow] := ADestBmp.ScanLine[LRow];
  end;

  // blur each row...
  SetLength(LPixels, ADestBmp.Width);

  for LRow := 0 to h do
  begin
    // read in one row pixels to the Pixels[]
    for LCol := 0 to w do
    begin
      LPixels[LCol] := LTheRows[LRow, LCol];
    end;

    // blur the pixels of a row
    BlurRow32(LPixels, K);

    // write the blurred pixels back to the corresponding row of the bitmap
    for LCol := 0 to w do
    begin
      LTheRows[LRow, LCol] := LPixels[LCol];
    end;
  end;

  // now blur each column...
  SetLength(LPixels, ADestBmp.Height);
  
  for LCol := 0 to w do
  begin
    // first read in one column to the Pixels[]
    for LRow := 0 to h do
    begin
      LPixels[LRow] := LTheRows[LRow, LCol];
    end;

    // blur the pixels of a column
    BlurRow32(LPixels, K);

    // write the blurred pixels back to the corresponding column of the bitmap
    for LRow := 0 to h do
    begin
      LTheRows[LRow, LCol] := LPixels[LRow];
    end;
  end;

  SetLength(LTheRows, 0);
  LTheRows := nil;

{$RANGECHECKS ON}
end;

end.
