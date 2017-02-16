{
  Based on the Java code in BoxBlurFilter.java.

  The original code could be found at here:
  http://www.jhlabs.com/ip/blurring.html

  Copyright 2005 Huxtable.com. All rights reserved.

  Original author: Jerry Huxtable
  Pascal code: Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Last update: 2011-05-29

  -- Quote ------------------------------------------------------------------

  Another Disclaimer

  There's source code in Java for pretty well everything I talk about here.
  I make no claims that these are optimised in any way - I've opted for
  simplicity over speed everywhere and you'll probably be able to make most of
  these thing go faster with a bit of effort. You can use the source code for
  anything you want, including commercial purposes, but there's no liability.
  If your nuclear power station or missile system fails because of an improper
  blur, it's not my fault.

  Box Blur

  We've solved the edge pixel problem, but our blur is still going really
  slowly, and things are only going to get worse. The problem is that the
  number of multiplications in the convolution is going up as the square of
  the kernel radius. With a 100x100 kernel, we're going to be doing 10000
  multiplies and adds per pixel (approx). How can we get round this? It turns
  out that there are more ways to go about this than I've possibly got time to
  write about, or even bother to look at. One way I will mention quickly before
  sweeping it under the rug is this: You can do a box blur by shrinking down
  your image, blurring it and scaling it up again. This may be fine for your
  purposes, and you should bear it in mind. One problem is that it doesn't
  animate very well, but may not be a concern to you.

  Let's look at the box blur again: It turns out that there's a couple of
  really easy ways to speed this up. Firstly, it turns out that the box blur is
  separable. This means that we can do a 2D blur by doing two 1D blurs, once in
  the horizontal direction and once in the vertical direction. This is much
  faster than doing the 2D blur because the time taken goes up in proportion to
  the kernel size, not as its square. Secondly, Think about the window that
  we're sliding across the image. As we move it from left to right, pixels
  come in at the right edge and are added to the total and at the same time
  pixels leave the left edge and are subtracted from the total. All we need to
  do is just do the add and subtract for the entering and leaving pixels at
  each step instead of adding together all the pixels in the window. We only
  need to store a set of running totals which are the width or height of the
  kernel. This gives a massive speed improvement at the cost of having to write
  some code. Luckily, I've written the code for you, so you win all round.
  We need two passes, once to blur horizontally and once vertically. The code
  for these is, of course, quite different. But wait! There's a trick we can do
  which allows us just to write the code once. If we write a blurring function
  which does the horizontal blur but writes its output image transposed,
  then we can just call it twice. The first pass blurs horizontally and
  transposes, the second pass does the same, but as the image is now
  transposed, it's really doing a vertical blur. The second transposition
  makes the image the right way up again and voila! - a very fast box blur.

  You may have noticed that we have only used an integer radius so far which
  makes it easy to work out the array indices for the blurring. We can extend
  the technique to do sub-pixel blurring (i.e. a non-integral radius) simply
  by linear interpolation between the array values. My source code doesn't do
  this, but it's easy to add.

                                                -- by Jerry Huxtable
}

unit gmBoxBlurFilter;

interface

uses
  GR32;

type
  TgmBoxBlurFilter = class(TObject)
  private
    FHorzRadius: Integer;
    FVertRadius: Integer;
    FIterations: Integer;

    procedure SetRadius(const AValue: Integer);

    procedure Blur(const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight, ARadius: Integer);
  public
    constructor Create;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Radius          : Integer read FHorzRadius write SetRadius;
    property HorizontalRadius: Integer read FHorzRadius write FHorzRadius;
    property VerticalRadius  : Integer read FVertRadius write FVertRadius;
    property Iterations      : Integer read FIterations write FIterations;
  end;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmImageProcessFuncs;

constructor TgmBoxBlurFilter.Create;
begin
  inherited Create;
  FHorzRadius := 0;
  FVertRadius := 0;
  FIterations := 1;
end;

procedure TgmBoxBlurFilter.SetRadius(const AValue: Integer);
begin
  FHorzRadius := AValue;
  FVertRadius := AValue;
end;

procedure TgmBoxBlurFilter.Blur(const AInPixels, AOutPixels: TArrayOfColor32;
  const AWidth, AHeight, ARadius: Integer);
var
  i, x, y, i1, i2: Integer;
  LWidthMinusOne : Integer;
  LTableSize     : Integer;
  LItemCount     : Integer;
  LInIndex       : Integer;
  LOutIndex      : Integer;
  ta, tr, tg, tb : Integer;
  a1, r1, g1, b1 : Byte;
  a2, r2, g2, b2 : Byte;
  LInColor       : TColor32;
  rgb1, rgb2     : TColor32;
  LDivideArray   : array of Integer;
begin
{$RANGECHECKS OFF}
  LWidthMinusOne := AWidth - 1;
  LTableSize     := 2 * ARadius + 1;
  LItemCount     := 256 * LTableSize;

  SetLength(LDivideArray, LItemCount);
  try
    for i := 0 to (LItemCount - 1) do
    begin
      LDivideArray[i] := Trunc(i / LTableSize);
    end;

    LInIndex := 0;

    for y := 0 to (AHeight - 1) do
    begin
      LOutIndex := y;
      ta        := 0;
      tr        := 0;
      tg        := 0;
      tb        := 0;

      for i := -ARadius to ARadius do
      begin
        LInColor := AInPixels[LInIndex + Clamp(i, 0, LWidthMinusOne)];

        a1 := LInColor shr 24 and $FF;
        r1 := LInColor shr 16 and $FF;
        g1 := LInColor shr  8 and $FF;
        b1 := LInColor        and $FF;

        ta := ta + a1;
        tr := tr + r1;
        tg := tg + g1;
        tb := tb + b1;
      end;

      for x := 0 to LWidthMinusOne do
      begin
        a1 := LDivideArray[ta];
        r1 := LDivideArray[tr];
        g1 := LDivideArray[tg];
        b1 := LDivideArray[tb];

        AOutPixels[LOutIndex] := (a1 shl 24) or (r1 shl 16) or (g1 shl  8) or b1;

        i1 := x + ARadius + 1;

        if i1 > LWidthMinusOne then
        begin
          i1 := LWidthMinusOne;
        end;

        i2 := x - ARadius;

        if i2 < 0 then
        begin
          i2 := 0;
        end;

        rgb1 := AInPixels[LInIndex + i1];
        rgb2 := AInPixels[LInIndex + i2];

        a1 := rgb1 shr 24 and $FF;
        r1 := rgb1 shr 16 and $FF;
        g1 := rgb1 shr  8 and $FF;
        b1 := rgb1        and $FF;

        a2 := rgb2 shr 24 and $FF;
        r2 := rgb2 shr 16 and $FF;
        g2 := rgb2 shr  8 and $FF;
        b2 := rgb2        and $FF;

        ta := ta + (a1 - a2);
        tr := tr + (r1 - r2);
        tg := tg + (g1 - g2);
        tb := tb + (b1 - b2);

        LOutIndex := LOutIndex + AHeight;
      end;

      LInIndex := LInIndex + AWidth;
    end;
  finally
    SetLength(LDivideArray, 0);
    LDivideArray := nil;
  end;
{$RANGECHECKS ON}
end;

function TgmBoxBlurFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth, LHeight: Integer;
  LPixelCount, i : Integer;
  LInPixels      : TArrayOfColor32;
  LOutPixels     : TArrayOfColor32;
begin
  Result := False;
  
  if (not Assigned(ASourceBmp)) or
     (not Assigned(ADestBmp)) or
     (ASourceBmp.Width <= 0) or
     (ASourceBmp.Height <= 0) then
  begin
    Exit;
  end;

  LWidth      := ASourceBmp.Width;
  LHeight     := ASourceBmp.Height;
  LPixelCount := LWidth * LHeight;

  SetLength(LInPixels, LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ASourceBmp, LInPixels);

    for i := 0 to (FIterations - 1) do
    begin
      Blur(LInPixels, LOutPixels, LWidth, LHeight, FHorzRadius);
      Blur(LOutPixels, LInPixels, LHeight, LWidth, FVertRadius);
    end;

    Result := SetRGB(ADestBmp, LWidth, LHeight, LInPixels);
  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end; 

function TgmBoxBlurFilter.ToString: string;
begin
  Result := 'Blur/Box Blur...';
end;

end.
