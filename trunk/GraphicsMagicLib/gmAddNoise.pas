{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmAddNoise;

{$WARN UNSAFE_CODE OFF}

interface

uses
  GR32;

type
  TgmNoiseMode = (nmColor, nmMono);

  procedure AddColorNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
  procedure AddMonoNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

uses
  GR32_LowLevel;

procedure AddColorNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, rr, gg, bb, LAmount: Integer;
  a, r, g, b            : Cardinal;
  p                     : PColor32;
begin
  LAmount := AAmount;

  if LAmount < 0 then
  begin
    LAmount := 0;
  end;

  p := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := p^ and $FF000000;

    if a > 0 then
    begin
      rr  := p^ shr 16 and $FF;
      gg  := p^ shr  8 and $FF;
      bb  := p^        and $FF;

      rr := rr + ( Random(LAmount) - (LAmount shr 1) );
      gg := gg + ( Random(LAmount) - (LAmount shr 1) );
      bb := bb + ( Random(LAmount) - (LAmount shr 1) );
      
      r  := Clamp(rr, 255);
      g  := Clamp(gg, 255);
      b  := Clamp(bb, 255);
      p^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(p);
  end;
end; 

procedure AddMonoNoise32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, LDelta, LAmount: Integer;
  rr, gg, bb        : Integer;
  a, r, g, b        : Cardinal;
  p                 : PColor32;
begin
  LAmount := AAmount;

  if LAmount < 0 then
  begin
    LAmount := 0;
  end;

  p := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := p^ and $FF000000;

    if a > 0 then
    begin
      LDelta := Random(LAmount) - (LAmount shr 1);

      rr := p^ shr 16 and $FF;
      gg := p^ shr  8 and $FF;
      bb := p^        and $FF;

      rr := rr + LDelta;
      gg := gg + LDelta;
      bb := bb + LDelta;

      r  := Clamp(rr, 255);
      g  := Clamp(gg, 255);
      b  := Clamp(bb, 255);
      p^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(p);
  end;
end; 

end.
