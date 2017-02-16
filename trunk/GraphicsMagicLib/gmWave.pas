{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmWave;

interface

uses
  GR32;

type
  TgmWaveOption = (woStandard, woExtra);

  procedure Wave32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer; const AWaveOption: TgmWaveOption;
    const ABKColor: TColor32);

implementation

procedure Wave32(const ASourceBmp, ADestBmp: TBitmap32; const AAmount: Integer;
  const AWaveOption: TgmWaveOption; const ABKColor: TColor32);
var
  LAngle          : Double;
  i, j, LDelta    : Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  if AAmount <> 0 then
  begin
    if ADestBmp.Width <> ASourceBmp.Width then
    begin
      ADestBmp.Width := ASourceBmp.Width;
    end;

    if ADestBmp.Height <> ASourceBmp.Height then
    begin
      ADestBmp.Height := ASourceBmp.Height;
    end;

    ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABKColor);
    LAngle := PI / 2 / AAmount;

    { original code is:
      for j := (SourceBmp.Height - 1) - (Amount * 2) downto AAmount do }
      
    for j := ((ASourceBmp.Height - 1) - (AAmount * 2)) downto 0 do
    begin
      LSrcRow := ASourceBmp.ScanLine[j];
      LDelta  := 0;

      for i := 0 to (ASourceBmp.Width - 1) do
      begin
        LDstRow    := ADestBmp.ScanLine[j + AAmount + LDelta];
        LDstRow[i] := LSrcRow[i];

        case AWaveOption of
          woStandard:
            begin
              LDelta := AAmount * Variant( Sin(LAngle * i) );
            end;

          woExtra:
            begin
              LDelta := AAmount * Variant( Sin(LAngle * i) * Cos(LAngle * i) );
            end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
 