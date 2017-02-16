{
  Based on the Java code in MotionBlurFilter.java.

  The original code could be found at here:
  http://www.jhlabs.com/ip/blurring.html

  Copyright 2005 Huxtable.com. All rights reserved.

  Original author: Jerry Huxtable
  Pascal code: Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Last update: 2012-02-17

  -- Quote the words of the original author -------------------------------

  Another Disclaimer

  There's source code in Java for pretty well everything I talk about here.
  I make no claims that these are optimised in any way - I've opted for
  simplicity over speed everywhere and you'll probably be able to make most of
  these thing go faster with a bit of effort. You can use the source code for
  anything you want, including commercial purposes, but there's no liability.
  If your nuclear power station or missile system fails because of an improper
  blur, it's not my fault.


  Motion Blur

  Time for a change of direction. So far we've only talked about uniform blurs,
  but there are other types. Motion blur is the blur you get when an object
  (or the camera) moves during the exposure. The image gets blurred along the
  apparent path of the object. Here we're just going to be talking about
  simulating motion blur on an existing still image - doing motion blur in
  animations is a whole different area. We're also only going to be blurring
  the whole image - we're not going to try and blur an object in the image.

  The good news is that we've already done simple motion blur. Go back to the
  box blur applet above and set the horizontal radius to, say 10, and the
  vertical radius to zero. This gives you a nice horizontal motion blur.
  For some purposes, this may be all you need. For example, one way to produce
  a brushed metal texture is to take an image consisting of random noise and
  apply a motion blur.

  If we want to blur in a direction other than horizontal or vertical, then
  things get more complicated. One technique might be to rotate the image,
  blur and then rotate back. What we'll do here though is to do it the hard
  and slow way. What we need to do is loop over the image, and for every pixel,
  add up all the pixels along the motion path. For a straight motion blur,
  this just means following a straight line from the pixel, but you could
  follow a wiggly path if you wanted to simulate long-exposure camera shake,
  say.

  Spin and Zoom Blur

  Once we've got the code for motion blur in place, it's a simple matter to
  modify it to do zoom and spin blurs, or even a combination of all three.
  It's just a matter of following the right path for each pixel. For radial
  blurs, just follow a path going from the blur center. For a spin blur,
  follow a tangential path. 

                                                  -- by Jerry Huxtable
}

unit gmMotionBlurFilter;

interface

uses
{ Graphics32 }
  GR32;

type
  TgmMotionBlurFilter = class(TObject)
  private
    FAngle    : Single;   // in radians
    FFalloff  : Single;
    FDistance : Single;
    FZoom     : Single;
    FRotation : Single;   // in radians
    FWrapEdges: Boolean;
  public
    constructor Create;

    function Execute(const ASourceBmp, ADestBmp: TBitmap32): Boolean;
    function ToString: string;

    property Angle      : Single  read FAngle     write FAngle;
    property Distance   : Single  read FDistance  write FDistance;
    property Rotation   : Single  read FRotation  write FRotation;
    property Zoom       : Single  read FZoom      write FZoom;
    property IsWrapEdges: Boolean read FWrapEdges write FWrapEdges;
  end;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_Transforms, GR32_LowLevel,
{ own }
  gmImageMath, gmImageProcessFuncs;

constructor TgmMotionBlurFilter.Create;
begin
  inherited Create;

  FAngle     := 0.0;
  FFalloff   := 1.0;
  FDistance  := 1.0;
  FZoom      := 0.0;
  FRotation  := 0.0;
  FWrapEdges := False;
end;

function TgmMotionBlurFilter.Execute(
  const ASourceBmp, ADestBmp: TBitmap32): Boolean;
var
  LWidth         : Integer;
  LHeight        : Integer;
  LPixelCount    : Integer;
  i, x, y        : Integer;
  cx, cy         : Integer;
  LNewX, LNewY   : Integer;
  LCount         : Integer;
  LIndex         : Integer;
  LRepetitions   : Integer;
  a, r, g, b     : Integer;
  aa, rr, gg, bb : Byte;
  LSinAngle      : Single;
  LCosAngle      : Single;
  LImageRadius   : Single;
  LTranslateX    : Single;
  LTranslateY    : Single;
  LMaxDistance   : Single;
  f, s           : Single;
  tx, ty         : Single;
  AT             : TAffineTransformation;
  p              : TFloatPoint;
  LColor         : TColor32;
  LInPixels      : TArrayOfColor32;
  LOutPixels     : TArrayOfColor32;
begin
{$RANGECHECKS OFF}
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
  AT := TAffineTransformation.Create;
  try
    GetRGB(ASourceBmp, LInPixels);

    LSinAngle := Sin(FAngle);
    LCosAngle := Cos(FAngle);

    cx     := LWidth div 2;
    cy     := LHeight div 2;
    LIndex := 0;

    LImageRadius := Sqrt(cx * cx + cy * cy);
    LTranslateX  := FDistance * LCosAngle;
    LTranslateY  := FDistance * (-LSinAngle);
    LMaxDistance := FDistance + Abs(FRotation * LImageRadius) + (FZoom * LImageRadius);
    LRepetitions := Trunc(LMaxDistance);

    for y := 0 to (LHeight - 1) do
    begin
      for x := 0 to (LWidth - 1) do
      begin
        a := 0;
        r := 0;
        g := 0;
        b := 0;

        LCount := 0;

        for i := 0 to (LRepetitions - 1) do
        begin
          f := i / LRepetitions;

          p.X := x;
          p.Y := y;

          AT.Clear;  // set to identity matrix

          { NOTE:

              In the original Java code, the transform matrix concatenation
              is Translation->Scaling->Rotation, but in our translated code,
              the matrix concatenation is in opposite order. We think that
              this is due to the order of matrix concatenation in Java is
              different from GR32.
           }

          // Step 1 -- Rotation
          if FRotation <> 0 then
          begin
            AT.Rotate(cx, cy, -RadToDeg(FRotation * f));
          end;

          // Step 2 -- Scale
          s := 1 - FZoom * f;

          { Special notice, the order of the following two line in the
            original Java code is:

              AT.Scale(s, s);
              AT.Translate(-cx, -cy);

            We think that this is because the matrix concatenation in Java is
            different from GR32.
            }
          AT.Translate(-cx, -cy);
          AT.Scale(s, s);

          // Step 3 -- Tranlation
          tx := cx + f * LTranslateX;
          ty := cy + f * LTranslateY;
          AT.Translate(tx, ty);

          p := AT.Transform(p);

          LNewX := Trunc(p.X);
          LNewY := Trunc(p.Y);

          if (LNewX < 0) or (LNewX >= LWidth) then
          begin
            if FWrapEdges then
            begin
              LNewX := ImageMath.Modul(LNewX, LWidth);
            end
            else
            begin
              Break;
            end;
          end;

          if (LNewY < 0) or (LNewY >= LHeight) then
          begin
            if FWrapEdges then
            begin
              LNewY := ImageMath.Modul(LNewY, LHeight);
            end
            else
            begin
              Break;
            end;
          end;

          Inc(LCount);

          LColor := LInPixels[LNewY * LWidth + LNewX];

          Inc(a, LColor shr 24 and $FF);
          Inc(r, LColor shr 16 and $FF);
          Inc(g, LColor shr 8 and $FF);
          Inc(b, LColor and $FF);
        end;

        if LCount = 0 then
        begin
          LOutPixels[LIndex] := LInPixels[LIndex];
        end
        else
        begin
          aa := Clamp(a div LCount);
          rr := Clamp(r div LCount);
          gg := Clamp(g div LCount);
          bb := Clamp(b div LCount);

          LOutPixels[LIndex] := (aa shl 24) or (rr shl 16) or (gg shl 8) or bb;
        end;

        Inc(LIndex);
      end;
    end;

    SetRGB(ADestBmp, LWidth, LHeight, LOutPixels);

  finally
    SetLength(LInPixels, 0);
    SetLength(LOutPixels, 0);

    LInPixels  := nil;
    LOutPixels := nil;

    AT.Free;
  end;
{$RANGECHECKS ON}
end;

function TgmMotionBlurFilter.ToString: string;
begin
  Result := 'Blur/Motion Blur...';
end; 

end.
