{
  Based on the Java code in ConvolveFilter.java.

  The original code could be found at here:
  http://www.jhlabs.com/ip/blurring.html

  Copyright 2005 Huxtable.com. All rights reserved.

  Original author: Jerry Huxtable
  Pascal code: Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Last update: 2015/04/03

  -- Quote the words of the original author -------------------------------

  A Disclaimer

  Whenever blurring is mentioned, there's always somebody who says "Hey!
  That's not a real motion blur!", or writes angry letters in green ink
  complaining that the mathematics is dubious or that there's a much faster
  way to do this using the sponglerizer registers on the HAL-9000. Ignore
  these people. This is a big subject, and this article is just for
  beginners (of which I can proudly say I am one). What matters is you get
  the results that you're aiming for, and if the results you're aiming for
  require dubious mathematics, then so be it. If the results you're aiming
  for look horrible to me, then that's fine, as long as they look good to
  you.
  

  Another Disclaimer

  There's source code in Java for pretty well everything I talk about here.
  I make no claims that these are optimised in any way - I've opted for
  simplicity over speed everywhere and you'll probably be able to make most of
  these thing go faster with a bit of effort. You can use the source code for
  anything you want, including commercial purposes, but there's no liability.
  If your nuclear power station or missile system fails because of an improper
  blur, it's not my fault.

  
  Gaussian Blur

  Now it's time to address the speed and square-looking blur issues at the same
  time. To get rid of the square look to the blur, we need a circular-shaped
  kernel. Unfortunately, the trick we used for box blurs doesn't work with a
  circle but there's a loophole: If the kernel has the right profile - the
  Gaussian profile - then we can do a 2D blur by performing two 1D blurs, just
  like we did with the box blur. It's not so fast because the sliding window
  trick doesn't work, but it's still a lot faster than doing the 2D convolution.
  The profile we need is the familiar bell-shaped, or Gaussian curve that you've
  heard of.

  Here's some code to create a 1D Gaussian kernel for a given radius. All we
  need to do is to apply this twice, once horizontally and once vertically. As
  a bonus, I've wrapped it up in a GaussianFilter to make it easy to use.

  This is why the Gaussian blur is found in every graphics package - it's much
  faster than other types of blur. The only problem is that it's not very
  realistic when it comes to simulating camera lenses, but more on that later.
  If you want to do things like simulating shadows, then the Gaussian blur, or
  even the box blur may be just fine. There's a place for all these
  effects - just because they aren't realistic doesn't mean they're not useful.

  The Gaussian blur is much faster, but it's nowhere near as fast as our box
  blur we did earlier on. If only there was some way to combine the two. I
  imagine you've guessed by now that there might be one, so I'll not hold the
  suspense any longer: If you do a lot of box blurs, the result looks more and
  more like a Gaussian blur. In fact, you can prove it mathematically if you've
  a spare moment (but don't tell me how - I'm not interested). In practice, 3
  to 5 box blurs look pretty good. Don't just take my word for it: The box blur
  has an "Iterations" options so you can try it out for yourself.

                                                  -- by Jerry Huxtable
}

unit gmGaussianBlurFilter;

interface

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmKernel, gmTypes;

type
  { A filter which applies Gaussian blur to an image. The filter simply creates
    a kernel with a Gaussian distribution for blurring.
    
    @original author Jerry Huxtable }
    
  TgmGaussianFilter = class(TObject)
  private
    FAlpha  : Boolean;
    FKernel : TgmKernel;

    function GetRadius: Single;
    procedure SetRadius(const ARadius: Single);
  protected
    FRadius : Single;

    procedure ConvolveAndTranspose(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction);
  public
    constructor Create; overload;
    constructor Create(const ARadius: Single); overload;

    destructor Destroy; override;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Radius  : Single  read GetRadius write SetRadius;
    property IsAlpha : Boolean read FAlpha    write FAlpha;
  end;

function MakeKernel(const ARadius: Single): TgmKernel;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmImageProcessFuncs;


// Make a Gaussian blur kernel.
function MakeKernel(const ARadius: Single): TgmKernel;
var
  i, r, LIndex  : Integer;
  LRow, LRows   : Integer;
  LSigma        : Single;
  LSigma22      : Single;
  LSigmaPi2     : Single;
  LSqrtSigmaPi2 : Single;
  LRadius2      : Single;
  LTotal        : Single;
  LDistance     : Single;
  LMatrix       : array of Single;
begin
  r             := Ceil(ARadius);
  LRows         := r * 2 + 1;
  LSigma        := ARadius / 3;
  LSigma22      := 2 * LSigma * LSigma;
  LSigmaPi2     := 2 * PI * LSigma;
  LSqrtSigmaPi2 := Sqrt(LSigmaPi2);
  LRadius2      := ARadius * ARadius;
  LTotal        := 0.0;
  LIndex        := 0;
  
  SetLength(LMatrix, LRows);
  try
    for LRow := -r to r do
    begin
      LDistance := LRow * LRow;

      if LDistance > LRadius2 then
      begin
        LMatrix[LIndex] := 0.0;
      end
      else
      begin
        LMatrix[LIndex] := Exp(-LDistance / LSigma22) / LSqrtSigmaPi2;
      end;

      LTotal := LTotal + LMatrix[LIndex];

      Inc(LIndex);
    end;

    for i := 0 to (LRows - 1) do
    begin
      LMatrix[i] := LMatrix[i] / LTotal;
    end;

    Result := TgmKernel.Create(LRows, 1, LMatrix);
  finally
    SetLength(LMatrix, 0);
    LMatrix := nil;
  end;
end;


{ TgmGaussianFilter }

// construct a Gaussian filter
constructor TgmGaussianFilter.Create;
begin
  inherited Create;

  FAlpha := True;
  SetRadius(2.0);
end;

{ construct a Gaussian filter
  @param ARadius blur radius in pixels }
constructor TgmGaussianFilter.Create(const ARadius: Single);
begin
  inherited Create;
  
  FAlpha := True;
  SetRadius(ARadius);
end;

destructor TgmGaussianFilter.Destroy;
begin
  FKernel.Free;
  inherited Destroy;
end;

{ Get the radius of the kernel.
	@return the radius }
function TgmGaussianFilter.GetRadius: Single;
begin
  Result := FRadius;
end;

{ Set the radius of the kernel, and hence the amount of blur. The bigger the
  radius, the longer this filter will take.
  @param radius the radius of the blur in pixels. }
procedure TgmGaussianFilter.SetRadius(const ARadius: Single);
begin
  FRadius := ARadius;
  FKernel := MakeKernel(FRadius);
end;

procedure TgmGaussianFilter.ConvolveAndTranspose(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32;
  const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
  const AEdgeAction: TgmEdgeAction);
var
  x, y, ix, LIndex    : Integer;
  LIOffset, LMOffset  : Integer;
  LCol, LCols, LCols2 : Integer;
  a, r, g, b, f       : Single;
  ia, ir, ig, ib      : Byte;
  LRGB                : TColor32;
begin
{$RANGECHECKS OFF}
  LCols  := AKernel.Width;
  LCols2 := LCols div 2;

  for y := 0 to (AHeight - 1) do
  begin
    LIndex   := y;
    LIOffset := y * AWidth;

    for x := 0 to (AWidth - 1) do
    begin
      a := 0.0;
      r := 0.0;
      g := 0.0;
      b := 0.0;

      LMOffset := LCols2;

      for LCol := -LCols2 to LCols2 do
      begin
        f := AKernel.Data[LMOffset + LCol];

        if f <> 0.0 then
        begin
          ix := x + LCol;

          if ix < 0 then
          begin
            if AEdgeAction = eaClampEdges then
            begin
              ix := 0;
            end
            else if AEdgeAction = eaWrapEdges then
            begin
              //ix := (x + AWidth) mod AWidth;  // the original code

              // Enlightened by PAEz: Result := Lower - (Result - Lower);
              ix := -ix;
            end;
          end
          else if ix >= AWidth then
          begin
            if AEdgeAction = eaClampEdges then
            begin
              ix := AWidth - 1;
            end
            else if AEdgeAction = eaWrapEdges then
            begin
              //ix := (x + AWidth) mod AWidth;  // the original code

              // Enlightened by PAEz: Result := Upper - (Result - Upper);
              ix := (AWidth - 1) - (ix - AWidth + 1);
            end;
          end;

          LRGB := AInPixels[LIOffset + ix];
          ia   := LRGB shr 24 and $FF;
          ir   := LRGB shr 16 and $FF;
          ig   := LRGB shr  8 and $FF;
          ib   := LRGB        and $FF;

          a := a + f * ia;
          r := r + f * ir;
          g := g + f * ig;
          b := b + f * ib;
        end;
      end;

      if AIsAlpha then
      begin
        ia := Clamp( Round(a), 0, 255 );
      end
      else
      begin
        ia := 255;
      end;

      ir := Clamp( Round(r), 0, 255 );
      ig := Clamp( Round(g), 0, 255 );
      ib := Clamp( Round(b), 0, 255 );

      AOutPixels[LIndex] := (ia shl 24) or (ir shl 16) or (ig shl 8) or ib;

      LIndex := LIndex + AHeight;
    end;
  end;
{$RANGECHECKS ON}
end;

function TgmGaussianFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth      : Integer;
  LHeight     : Integer;
  LPixelCount : Integer;
  LInPixels   : TArrayOfColor32;
  LOutPixels  : TArrayOfColor32;
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

  SetLength(LInPixels,  LPixelCount);
  SetLength(LOutPixels, LPixelCount);
  try
    GetRGB(ASourceBmp, LInPixels);

    ConvolveAndTranspose(FKernel, LInPixels, LOutPixels, LWidth, LHeight, FAlpha, eaWrapEdges);
    ConvolveAndTranspose(FKernel, LOutPixels, LInPixels, LHeight, LWidth, FAlpha, eaWrapEdges);

    Result := SetRGB(ADestBmp, LWidth, LHeight, LInPixels);
  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end;

function TgmGaussianFilter.ToString: string;
begin
  Result := 'Blur/Gaussian Blur...';
end;

end.
