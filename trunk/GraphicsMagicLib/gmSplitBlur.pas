{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmSplitBlur;

interface

uses
  GR32;

  procedure SplitBlur32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

procedure SplitBlur32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  i, j, LDelta       : Integer;
  LRow1, LRow2, LRow3: PColor32Array;
  LBuffer            : array [0 .. 3, 0 .. 3] of Byte;
  a, r, g, b         : Cardinal;
begin
{$RANGECHECKS OFF}

  if AAmount <> 0 then
  begin
    for j := 0 to (ADestBmp.Height - 1) do
    begin
      LRow1 := ADestBmp.ScanLine[j];

      if (j - AAmount) < 0 then
      begin
        LRow2 := ADestBmp.ScanLine[j];
      end
      else
      begin
        LRow2 := ADestBmp.ScanLine[j - AAmount];
      end;

      if (j + AAmount) < ADestBmp.Height then
      begin
        LRow3 := ADestBmp.ScanLine[j + AAmount];
      end
      else
      begin
        { original code is:
          LRow3 := ADestBmp.ScanLine[Source.Height - j]; }
        
        LRow3 := ADestBmp.ScanLine[j];
      end;

      for i := 0 to (ADestBmp.Width - 1) do
      begin
        if (i - AAmount) < 0 then
        begin
          LDelta := i;
        end
        else
        begin
          LDelta := i - AAmount;
        end;

        LBuffer[0, 0] := LRow2[LDelta] shr 24 and $FF; // alpha
        LBuffer[0, 1] := LRow2[LDelta] shr 16 and $FF; // red
        LBuffer[0, 2] := LRow2[LDelta] shr  8 and $FF; // green
        LBuffer[0, 3] := LRow2[LDelta]        and $FF; // blue

        LBuffer[1, 0] := LRow3[LDelta] shr 24 and $FF; // alpha
        LBuffer[1, 1] := LRow3[LDelta] shr 16 and $FF; // red
        LBuffer[1, 2] := LRow3[LDelta] shr  8 and $FF; // green
        LBuffer[1, 3] := LRow3[LDelta]        and $FF; // blue

        if (i + AAmount) < ADestBmp.Width then
        begin
          LDelta := i + AAmount;
        end
        else
        begin
          { original code is:
            LDelta := ADestBmp.Width - i; }

          LDelta := i;
        end;

        LBuffer[2, 0] := LRow2[LDelta] shr 24 and $FF; // alpha
        LBuffer[2, 1] := LRow2[LDelta] shr 16 and $FF; // red
        LBuffer[2, 2] := LRow2[LDelta] shr  8 and $FF; // green
        LBuffer[2, 3] := LRow2[LDelta]        and $FF; // blue

        LBuffer[3, 0] := LRow3[LDelta] shr 24 and $FF; // alpha
        LBuffer[3, 1] := LRow3[LDelta] shr 16 and $FF; // red
        LBuffer[3, 2] := LRow3[LDelta] shr  8 and $FF; // green
        LBuffer[3, 3] := LRow3[LDelta]        and $FF; // blue

        // composition
        a := (LBuffer[0, 0] + LBuffer[1, 0] + LBuffer[2, 0] + LBuffer[3, 0]) shr 2;
        r := (LBuffer[0, 1] + LBuffer[1, 1] + LBuffer[2, 1] + LBuffer[3, 1]) shr 2;
        g := (LBuffer[0, 2] + LBuffer[1, 2] + LBuffer[2, 2] + LBuffer[3, 2]) shr 2;
        b := (LBuffer[0, 3] + LBuffer[1, 3] + LBuffer[2, 3] + LBuffer[3, 3]) shr 2;

        LRow1[i] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
