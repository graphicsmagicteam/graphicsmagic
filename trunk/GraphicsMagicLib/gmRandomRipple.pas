{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmRandomRipple;

interface

uses
  Math, GR32;

  procedure RandomRipple32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

procedure RandomRipple32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, j,  LMinAmount        : Integer;
  LHalfHeight, LRandomValue: Integer;
  LRow1, LRow2             : PColor32Array;
begin
{$RANGECHECKS OFF}

  LHalfHeight := (ADestBmp.Height - 1) div 2;
  LMinAmount  := MinIntValue([LHalfHeight, AAmount]);

  Randomize;

  for j := (ADestBmp.Height - 1 - AAmount) downto 0 do
  begin
    LRow1        := ADestBmp.ScanLine[j];
    LRandomValue := 0;
    
    for i := 0 to (ADestBmp.Width - 1) do
    begin
      LRow2        := ADestBmp.ScanLine[j + LRandomValue];
      LRow2[i]     := LRow1[i];
      LRandomValue := Random(LMinAmount);
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
