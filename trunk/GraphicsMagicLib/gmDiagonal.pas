{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmDiagonal;

interface

uses
  GR32;

type
  TgmDiagonalDirection = (ddLeftDiag, ddRightDiag);

  procedure Diagonal32(const ASourceBmp, ADestBmp: TBitmap32;
    const ABackColor: TColor32; const AAmount: Integer;
    const ADiagDirection: TgmDiagonalDirection);

implementation

procedure Diagonal32(const ASourceBmp, ADestBmp: TBitmap32;
  const ABackColor: TColor32; const AAmount: Integer;
  const ADiagDirection: TgmDiagonalDirection);
var
  LSourceRow, LDestRow         : PColor32Array;
  i, j, LDelta, LHeight, LWidth: Integer;
  LSourceColumn, LDestColumn   : Integer;
  LFactor                      : Extended;
begin
{$RANGECHECKS OFF}

  LSourceColumn := 0;
  LDestColumn   := 0;

  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LHeight := ASourceBmp.Height;
    LWidth  := ASourceBmp.Width;

    ADestBmp.Width  := LWidth;
    ADestBmp.Height := LHeight;

    ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABackColor);

    LFactor := LWidth / ( LWidth + (AAmount / 100) * LHeight );

    for j := 0 to (LHeight - 1) do
    begin
      LSourceRow := ASourceBmp.ScanLine[j];
      LDestRow   := ADestBmp.Scanline[j];
      LDelta     := Round(AAmount / 100 * j);

      for i := 0 to (LWidth - 1) do
      begin
        case ADiagDirection of
          ddLeftDiag:
            begin
              LSourceColumn := i;
              LDestColumn   := Round( LFactor * (i + LDelta) );
            end;

          ddRightDiag:
            begin
              LSourceColumn := (LWidth - 1 - i);
              LDestColumn   := Round( LWidth - 1 - LFactor * (i + LDelta) );
            end;
        end;
        
        LDestRow[LDestColumn] := LSourceRow[LSourceColumn];
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
