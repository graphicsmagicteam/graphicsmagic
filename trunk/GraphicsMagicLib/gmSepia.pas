{ This is a plug-in filters specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

unit gmSepia;

{$WARN UNSAFE_CODE OFF}

interface

uses
  GR32;

  procedure Sepia32(const ABmp: TBitmap32; const ADepth: Byte);

implementation

//*********************************************************
// Colorize given bitmap with sepia colors.
// Original author is Daniel Lopes from Graphics32 newsgroup.
// About 20% accelerated by Gerd Platl
// input:   bmp32  source and destination bitmap
//          depth  expedient values 10 .. 60
//                 default = 34
//---------------------------------------------------------
procedure Sepia32(const ABmp: TBitmap32; const ADepth: Byte);
var
  LDepth2, i: Integer;
  LPixel    : PColor32Entry;
begin
{$RANGECHECKS OFF}

  LDepth2 := ADepth * 2;
  LPixel  := @ABmp.Bits[0];

  for i := 0 to (ABmp.Width * ABmp.Height - 1) do
  begin
    // blue component = gray scaled color
    LPixel.B := (LPixel.R + LPixel.G + LPixel.B) div 3;

    // set red component of sepia color
    LPixel.R := LPixel.B + LDepth2;

    if LPixel.R < LDepth2 then
    begin
      LPixel.R := 255;
    end;

    // set green component of sepia color
    LPixel.G := LPixel.B + ADepth;

    if LPixel.G < ADepth then
    begin
      LPixel.G := 255;
    end;

    Inc(LPixel);
  end;

{$RANGECHECKS ON}
end; 

end.
