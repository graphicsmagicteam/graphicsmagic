{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmImageTile;

interface

uses
  Windows, GR32;

type
  TgmImageTileMode = (itmTile, itmOverlap);

  procedure OverlapTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
  procedure TileTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);

implementation

uses
  gmImageProcessFuncs;

procedure OverlapTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  X, Y                : Integer;
  LOverlapX, LOverlapY: Integer;
  LWidth, LHeight     : Integer;
  LBitmap             : TBitmap32;
begin
  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LWidth    := MulDiv(ADestBmp.Width,  AAmount, 100);
    LHeight   := MulDiv(ADestBmp.Height, AAmount, 100);
    LOverlapX := MulDiv(LWidth,  2, 3);
    LOverlapY := MulDiv(LHeight, 2, 3);

    LBitmap := TBitmap32.Create;
    try
      LBitmap.Assign(ADestBmp);
      SmoothResize32(LBitmap, LWidth, LHeight);

      Y := 0;
      repeat
        X := 0;

        repeat
          ADestBmp.Draw(X, Y, LBitmap);
          X := X + LOverlapX;
        until X >= ADestBmp.Width;

        Y := Y + LOverlapY;
      until Y >= ADestBmp.Height;

    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TileTexturize32(const ADestBmp: TBitmap32; const AAmount: Integer);
var
  X, Y, LWidth, LHeight: Integer;
  LBitmap              : TBitmap32;
begin
  if (AAmount >= 0) and (AAmount <= 100) then
  begin
    LWidth  := MulDiv(ADestBmp.Width,  AAmount, 100);
    LHeight := MulDiv(ADestBmp.Height, AAmount, 100);
    
    LBitmap := TBitmap32.Create;
    try
      LBitmap.Assign(ADestBmp);
      SmoothResize32(LBitmap, LWidth, LHeight);

      Y := 0;
      repeat

        X := 0;
        repeat
          ADestBmp.Draw(X, Y, LBitmap);
          X := X + LWidth;
        until X >= ADestBmp.Width;

        Y := Y + LHeight;
      until Y >= ADestBmp.Height;
      
    finally
      LBitmap.Free;
    end;
  end;
end; 

end.
