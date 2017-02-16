{
  Based on the Java code in ConvolveFilter.java.

  The original code could be found at here:
  http://www.jhlabs.com/ip/blurring.html

  Copyright 2005 Huxtable.com. All rights reserved.

  Original author: Jerry Huxtable
  Pascal code: Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Last update: 2012-02-17

  -- Quote -------------------------------------------------------------------

  Another Disclaimer

  There's source code in Java for pretty well everything I talk about here.
  I make no claims that these are optimised in any way - I've opted for
  simplicity over speed everywhere and you'll probably be able to make most of
  these thing go faster with a bit of effort. You can use the source code for
  anything you want, including commercial purposes, but there's no liability.
  If your nuclear power station or missile system fails because of an improper
  blur, it's not my fault.

  What is Blurring?

  We all know what blurring is, don't we? It's that thing that happens when
  your camera is out of focus or the dog steals your glasses. What happens is
  that what should be seen as a sharp point gets smeared out, usually into a
  disc shape. In image terms this means that each pixel in the source image
  gets spread over and mixed into surrounding pixels. Another way to look at
  this is that each pixel in the destination image is made up out of a mixture
  of surrounding pixels from the source image.
  
  The operation we need for this is called convolution. This sounds complicated
  but thats only because mathematicians like to make things sound complicated
  in order to maintain that air of magic and keep the funding rolling in. Well,
  I'm onto them and I can reveal that convolution is not that complicated (at
  my level anyway). The way it works is this: we imagine sliding a rectangular
  array of numbers over our image. This array is called the convolution kernel.
  For every pixel in the image, we take the corresponding numbers from the
  kernel and the pixels they are over, multiply them together and add all the
  results together to make the new pixel. For example, imagine we want to do a
  really simple blur where we just average together each pixel and its eight
  immediate neighbours. The kernel we need is:

	  1/9 1/9 1/9
	  1/9 1/9 1/9
	  1/9 1/9 1/9

  Notice that these all add up to 1, which means that our resulting image will
  be just as bright as the original.

  Now the squareness problem: The reason everything looks square is because what
  we're doing here is called a box blur - our kernel is shaped like a square,
  as if we're using a camera which has a square aperture. Incidentally, don't
  let anyone tell you that box blurs are useless - in fact if you're simulating
  the shadow cast by a square light, it's exactly what you want. Anyway, they'll
  come in useful further on. Another thing: don't get confused - I'm using the
  term box blur to refer to the shape of the kernel, not its profile, which I'm
  going to call a box filter. More on this later on. To get a more realistic
  blur, what we should have done is used a circle-shaped kernel. This simulates
  much better what a real camera does.

                                                      -- by Jerry Huxtable
}

unit gmConvolveFilter;

interface

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmKernel, gmTypes;

type

  { A filter which applies a convolution kernel to an image.
    @original author Jerry Huxtable }
    
  TgmConvolveFilter = class(TObject)
  protected
    FAlpha     : Boolean;
    FEdgeAction: TgmEdgeAction;
    FKernel    : TgmKernel;

    procedure Convolve(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AEdgeAction: TgmEdgeAction); overload;

    procedure Convolve(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction); overload;

    // convolve with a 2D kernel
    procedure ConvolveHV(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction);

    // convolve with a kernel consisting of one row
    procedure ConvolveH(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction);

    // convolve with a kernel consisting of one column
    procedure ConvolveV(const AKernel: TgmKernel;
      const AInPixels, AOutPixels: TArrayOfColor32;
      const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
      const AEdgeAction: TgmEdgeAction);
  public
    constructor Create; overload;
    constructor Create(const AMatrix: array of Single); overload;
    constructor Create(const ARows, ACols: Integer; const AMatrix: array of Single); overload;
    constructor Create(const AKernel: TgmKernel); overload;

    destructor Destroy; override;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Kernel    : TgmKernel     read FKernel     write FKernel;
    property EdgeAction: TgmEdgeAction read FEdgeAction write FEdgeAction;
    property IsAlpha   : Boolean       read FAlpha      write FAlpha;
  end;

implementation

uses
{ Standard }
  SysUtils,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmImageProcessFuncs;

{ Construct a filter with a null kernel. This is only useful if you're
  going to change the kernel later on. }
constructor TgmConvolveFilter.Create;
var
  LData: array of Single;
  i    : Integer;
begin
  inherited Create;

  FAlpha      := True;
  FEdgeAction := eaClampEdges;

  SetLength(LData, 9);
  try
    for i := 0 to 8 do
    begin
      LData[i] := 0.0;
    end;

    FKernel := TgmKernel.Create(3, 3, LData);
  finally
    SetLength(LData, 0);
    LData := nil;
  end;
end;

{ Construct a filter with the given 3x3 kernel.
	@param AMatrix  an array of 9 floats containing the kernel }
constructor TgmConvolveFilter.Create(const AMatrix: array of Single);
begin
  inherited Create;

  FAlpha      := True;
  FEdgeAction := eaClampEdges;

  if Length(AMatrix) <> 9 then
  begin
    raise Exception.Create('The passed parameter is not a 3x3 matrix.');
  end;

  FKernel := TgmKernel.Create(3, 3, AMatrix);
end;

{ Construct a filter with the given kernel.
	@param ARows	  the number of rows in the kernel
	@param ACols	  the number of columns in the kernel
  @param AMatrix	an array of rows*cols floats containing the kernel }
constructor TgmConvolveFilter.Create(const ARows, ACols: Integer;
  const AMatrix: array of Single);
begin
  inherited Create;

  FAlpha      := True;
  FEdgeAction := eaClampEdges;
  FKernel     := TgmKernel.Create(ACols, ARows, AMatrix);
end;

{ Construct a filter with the given kernel.
	@param AKernel  a kernel with customed settings }
constructor TgmConvolveFilter.Create(const AKernel: TgmKernel);
begin
  inherited Create;

  FAlpha      := True;
  FEdgeAction := eaClampEdges;
  FKernel     := AKernel;
end;

destructor TgmConvolveFilter.Destroy;
begin
  FKernel.Free;
  inherited Destroy;
end;

procedure TgmConvolveFilter.Convolve(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32; const AWidth, AHeight: Integer;
  const AEdgeAction: TgmEdgeAction);
begin
  Convolve(AKernel, AInPixels, AOutPixels, AWidth, AHeight, True, AEdgeAction);
end; 

procedure TgmConvolveFilter.Convolve(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32;
  const AWidth, AHeight: Integer; const AIsAlpha: Boolean;
  const AEdgeAction: TgmEdgeAction);
begin
  if AKernel.Height = 1 then
  begin
    ConvolveH(AKernel, AInPixels, AOutPixels, AWidth, AHeight, AIsAlpha, AEdgeAction);
  end
  else if AKernel.Width = 1 then
  begin
    ConvolveV(AKernel, AInPixels, AOutPixels, AWidth, AHeight, AIsAlpha, AEdgeAction);
  end
  else
  begin
    ConvolveHV(AKernel, AInPixels, AOutPixels, AWidth, AHeight, AIsAlpha, AEdgeAction);
  end;
end; 

// convolve with a 2D kernel
procedure TgmConvolveFilter.ConvolveHV(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32; const AWidth, AHeight: Integer;
  const AIsAlpha: Boolean; const AEdgeAction: TgmEdgeAction);
var
  x, y, iy, ix      : Integer;
  LIOffset, LMOffset: Integer;
  a, r, g, b, f     : Single;
  ia, ir, ig, ib    : Byte;
  LRow, LCol, LIndex: Integer;
  LRows, LRows2     : Integer;
  LCols, LCols2     : Integer;
  LRGB              : TColor32;
begin
{$RANGECHECKS OFF}
  LIndex   := 0;
  LIOffset := 0;
  LRows    := AKernel.Height;
  LCols    := AKernel.Width;
  LRows2   := LRows div 2;
  LCols2   := LCols div 2;

  for y := 0 to (AHeight - 1) do
  begin
    for x := 0 to (AWidth - 1) do
    begin
      a := 0.0;
      r := 0.0;
      g := 0.0;
      b := 0.0;

      for LRow := -LRows2 to LRows2 do
      begin
        iy := y + LRow;

        if (iy >= 0) and (iy < AHeight) then
        begin
          LIOffset := iy * AWidth;
        end
        else if (AEdgeAction = eaClampEdges) then
        begin
          LIOffset := y * AWidth;
        end
        else if (AEdgeAction = eaWrapEdges) then
        begin
          LIOffset := ( (iy + AHeight) mod AHeight ) * AWidth;
        end
        else
        begin
          Continue;
        end;

        LMOffset := LCols * (LRow + LRows2) + LCols2;

        for LCol := -LCols2 to LCols2 do
        begin
          f := AKernel.Data[LMOffset + LCol];

          if f <> 0.0 then
          begin
            ix := x + LCol;

            if not ( (ix >= 0) and (ix < AWidth) ) then
            begin
              if AEdgeAction = eaClampEdges then
              begin
                ix := x;
              end
              else if AEdgeAction = eaWrapEdges then
              begin
                ix := (x + AWidth) mod AWidth;
              end
              else
              begin
                Continue;
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

      Inc(LIndex);
    end;
  end;
{$RANGECHECKS ON}
end;

// convolve with a kernel consisting of one row
procedure TgmConvolveFilter.ConvolveH(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32; const AWidth, AHeight: Integer;
  const AIsAlpha: Boolean; const AEdgeAction: TgmEdgeAction);
var
  x, y, ix, LIndex   : Integer;
  LCol, LCols, LCols2: Integer;
  LIOffset, LMOffset : Integer;
  a, r, g, b, f      : Single;
  ia, ir, ig, ib     : Byte;
  LRGB               : TColor32;
begin
{$RANGECHECKS OFF}
  LIndex := 0;
  LCols  := AKernel.Width;
  LCols2 := LCols div 2;

  for y := 0 to (AHeight - 1) do
  begin
    LIOffset := y * AWidth;

    for x := 0 to (AWidth - 1) do
    begin
      a        := 0.0;
      r        := 0.0;
      g        := 0.0;
      b        := 0.0;
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
              ix := (x + AWidth) mod AWidth;
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
              ix := (x + AWidth) mod AWidth;
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

      Inc(LIndex);
    end;
  end;
{$RANGECHECKS ON}
end; 

// convolve with a kernel consisting of one column
procedure TgmConvolveFilter.ConvolveV(const AKernel: TgmKernel;
  const AInPixels, AOutPixels: TArrayOfColor32; const AWidth, AHeight: Integer;
  const AIsAlpha: Boolean; const AEdgeAction: TgmEdgeAction);
var
  x, y, iy, LIndex   : Integer;
  LRow, LRows, LRows2: Integer;
  LIOffset           : Integer;
  a, r, g, b, f      : Single;
  ia, ir, ig, ib     : Byte;
  LRGB               : TColor32;
begin
{$RANGECHECKS OFF}
  LIndex := 0;
  LRows  := AKernel.Height;
  LRows2 := LRows div 2;

  for y := 0 to (AHeight - 1) do
  begin
    for x := 0 to (AWidth - 1) do
    begin
      a := 0.0;
      r := 0.0;
      g := 0.0;
      b := 0.0;

      for LRow := -LRows2 to LRows2 do
      begin
        iy := y + LRow;

        if iy < 0 then
        begin
          if AEdgeAction = eaClampEdges then
          begin
            LIOffset := 0;
          end
          else if AEdgeAction = eaWrapEdges then
          begin
            LIOffset := ( (y + AHeight) mod AHeight ) * AWidth;
          end
          else
          begin
            LIOffset := iy * AWidth;
          end;
        end
        else if iy >= AHeight then
        begin
          if AEdgeAction = eaClampEdges then
          begin
            LIOffset := (AHeight - 1) * AWidth;
          end
          else if AEdgeAction = eaWrapEdges then
          begin
            LIOffset := ( (y + AHeight) mod AHeight ) * AWidth;
          end
          else
          begin
            LIOffset := iy * AWidth;
          end;
        end
        else
        begin
          LIOffset := iy * AWidth;
        end;

        f := AKernel.Data[LRow + LRows2];

        if f <> 0.0 then
        begin
          LRGB := AInPixels[LIOffset + x];
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

      Inc(LIndex);
    end;
  end;
{$RANGECHECKS ON}
end; 

function TgmConvolveFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth     : Integer;
  LHeight    : Integer;
  LPixelCount: Integer;
  LInPixels  : TArrayOfColor32;
  LOutPixels : TArrayOfColor32;
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
    Convolve(FKernel, LInPixels, LOutPixels, LWidth, LHeight, FAlpha, FEdgeAction);

    Result := SetRGB(ADestBmp, LWidth, LHeight, LOutPixels);
  finally
    SetLength(LInPixels,  0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;
  end;
end; 

function TgmConvolveFilter.ToString: string;
begin
  Result := 'Blur/Convolve...';
end; 

end.
