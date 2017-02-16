{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmMosaic;

interface

uses
  GR32;

  procedure Mosaic32(const ADestBmp: TBitmap32; const ASize: Integer);

implementation

procedure Mosaic32(const ADestBmp: TBitmap32; const ASize: Integer);
var
  x, y, i, j    : Integer;
  a1, r1, g1, b1: Cardinal;
  a2, r2, g2, b2: Cardinal;
  LRow1, LRow2  : PColor32Array;
begin
{$RANGECHECKS OFF}

  y := 0;
  repeat
    LRow1 := ADestBmp.ScanLine[y];

    repeat
      j := 1;

      repeat
        LRow2 := ADestBmp.ScanLine[y];
        x     := 0;

        repeat

          a1 := LRow1[x] and $FF000000;
          r1 := LRow1[x] and $FF0000;
          g1 := LRow1[x] and $FF00;
          b1 := LRow1[x] and $FF;
          i  := 1;

          repeat
            a2 := a1;
            r2 := r1;
            g2 := g1;
            b2 := b1;

            LRow2[x] := a2 or r2 or g2 or b2;

            Inc(x);
            Inc(i);
          until (x >= ADestBmp.Width) or (i > ASize);

        until x >= ADestBmp.Width;

        Inc(j);
        Inc(y);
        
      until (y >= ADestBmp.Height) or (j > ASize);
    until (y >= ADestBmp.Height) or (x >= ADestBmp.Width);
  until y >= ADestBmp.Height;

{$RANGECHECKS ON} 
end;

end.
