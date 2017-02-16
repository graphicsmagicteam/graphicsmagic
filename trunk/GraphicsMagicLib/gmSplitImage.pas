{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmSplitImage;

interface

uses
  GR32;

type
  TgmSplitImageType = (sitRound, sitWaste);

  procedure SplitImage32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer; const ASplitType: TgmSplitImageType;
    const ABKColor: TColor32);

implementation

uses
  Classes;

procedure SplitImage32(const ASourceBmp, ADestBmp: TBitmap32;
  const AAmount: Integer; const ASplitType: TgmSplitImageType;
  const ABKColor: TColor32);
var
  i, j, LCenterX, LDelta: Integer;
  LCorrectAmount        : Integer;
  LRect1, LRect2        : TRect;
  LBitmap1, LBitmap2    : TBitmap32;
  LRow1, LRow2, LSrcRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  LDelta         := 0;
  LCorrectAmount := AAmount;

  if LCorrectAmount = 0 then
  begin
    ADestBmp.Assign(ASourceBmp);
    Exit;
  end;

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;
  
  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  ADestBmp.FillRect(0, 0, ADestBmp.Width, ADestBmp.Height, ABKColor);

  LCenterX := ASourceBmp.Width div 2;

  if LCorrectAmount > LCenterX then
  begin
    LCorrectAmount := LCenterX;
  end;

  LBitmap1 := TBitmap32.Create;
  LBitmap2 := TBitmap32.Create;
  try
    LBitmap1.Height := 1;
    LBitmap1.Width  := LCenterX;
    LBitmap2.Height := 1;
    LBitmap2.Width  := LCenterX;
    LRow1           := LBitmap1.ScanLine[0];
    LRow2           := LBitmap2.ScanLine[0];

    for j := 0 to (ASourceBmp.Height - 1) do
    begin
      LSrcRow := ASourceBmp.ScanLine[j];

      for i := 0 to (LCenterX - 1) do
      begin
        LRow1[i] := LSrcRow[i];
        LRow2[i] := LSrcRow[i + LCenterX];
      end;

      case ASplitType of
        sitRound:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Sin( j / (ASourceBmp.Height - 1) * Pi )  )   );
          end;

        sitWaste:
          begin
            LDelta := Round(   LCorrectAmount * Abs(  Cos( j / (ASourceBmp.Height - 1) * Pi )  )   );
          end;
      end;

      LRect1 := Rect(0, j, LDelta, j + 1);
      ADestBmp.Draw(LRect1, LBitmap1.Canvas.ClipRect, LBitmap1);
      
      LRect2 := Rect(ASourceBmp.Width - 1 - LDelta, j, ASourceBmp.Width - 1, j + 1);
      ADestBmp.Draw(LRect2, LBitmap2.Canvas.ClipRect, LBitmap2);
    end;
    
  finally
    LBitmap1.Free;
    LBitmap2.Free;
  end;

{$RANGECHECKS ON}
end;

end.
