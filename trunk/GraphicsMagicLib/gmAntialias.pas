{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmAntialias;

interface

uses
  Math, GR32;

  procedure Antialias32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

procedure AntiAliasRect32(const ADestBmp: TBitmap32;
  AXOrigin, AYOrigin, AXFinal, AYFinal: Integer);
var
  LMemo, x, y           : Integer;
  a, r, g, b            : Cardinal;
  a0, A1Prev, A1Next, a2: Integer;
  r0, R1Prev, R1Next, r2: Integer;
  g0, G1Prev, G1Next, g2: Integer;
  b0, B1Prev, B1Next, b2: Integer;
  p0, p1, p2            : PColor32Array;
begin
{$RANGECHECKS OFF}

  if AXFinal < AXOrigin then
  begin
    LMemo    := AXOrigin;
    AXOrigin := AXFinal;
    AXFinal  := LMemo;
  end;

  if AYFinal < AYOrigin then
  begin
    LMemo    := AYOrigin;
    AYOrigin := AYFinal;
    AYFinal  := LMemo;
  end;

  AXOrigin := Max(1, AXOrigin);
  AYOrigin := Max(1, AYOrigin);
  AXFinal  := Min(ADestBmp.Width  - 2, AXFinal);
  AYFinal  := Min(ADestBmp.Height - 2, AYFinal);

  for y := AYOrigin to AYFinal do
  begin
    p0 := ADestBmp.ScanLine[y - 1];
    p1 := ADestBmp.scanline[y];
    p2 := ADestBmp.ScanLine[y + 1];

    for x := AXOrigin to AXFinal do
    begin
      // alpha component
      a0     := p0[x]     shr 24 and $FF;
      A1Prev := p1[x - 1] shr 24 and $FF;
      A1Next := p1[x + 1] shr 24 and $FF;
      a2     := p2[x]     shr 24 and $FF;

      // red component
      r0     := p0[x]     shr 16 and $FF;
      R1Prev := p1[x - 1] shr 16 and $FF;
      R1Next := p1[x + 1] shr 16 and $FF;
      r2     := p2[x]     shr 16 and $FF;

      // green component
      g0     := p0[x]     shr 8 and $FF;
      G1Prev := p1[x - 1] shr 8 and $FF;
      G1Next := p1[x + 1] shr 8 and $FF;
      g2     := p2[x]     shr 8 and $FF;

      // blue component
      b0     := p0[x]     and $FF;
      B1Prev := p1[x - 1] and $FF;
      B1Next := p1[x + 1] and $FF;
      b2     := p2[x]     and $FF;

      // composition
      a     := (a0 + a2 + A1Prev + A1Next) div 4;
      r     := (r0 + r2 + R1Prev + R1Next) div 4;
      g     := (g0 + g2 + G1Prev + G1Next) div 4;
      b     := (b0 + b2 + B1Prev + B1Next) div 4;

      p1[x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;
  end;
  
{$RANGECHECKS ON}
end;

procedure Antialias32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i: Integer;
begin
  if AAmount >= 1 then
  begin
    for i := 1 to AAmount do
    begin
      AntiAliasRect32(ADestBmp, 0, 0, ADestBmp.Width, ADestBmp.Height);
    end;
  end;
end;

end.
