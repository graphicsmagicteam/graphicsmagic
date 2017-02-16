{ The original author of this algorithms is
     Babak Sateli <babak_sateli@yahoo.com>.

  You could download the original code at:
     http://delphi.icm.edu.pl/ftp/d20free/ProEffectImage.zip

  We adapted the original code to support GR32. This library is free and can be
  used in any software product. }

unit gmTwist;

interface

uses
  GR32;

  procedure Twist32(const ASourceBmp, ADestBmp: TBitmap32;
    const AAmount: Integer);

implementation

procedure Twist32(const ASourceBmp, ADestBmp: TBitmap32;
  const AAmount: Integer);
var
  fxmid, fymid, txmid, tymid                 : Single;
  fx, fy, tx2, ty2, dx, dy                   : Single;
  r, Theta, Offset, Weight                   : Single;
  TotalAlpha, TotalRed, TotalGreen, TotalBlue: Single;
  ifx, ify, ty, tx, ix, iy, LColumn          : Integer;
  NewAlpha, NewRed, NewGreen, NewBlue        : Integer;
  WeightX, WeightY                           : array[0..1] of Single;
  sli, slo                                   : PColor32Array;
  Alpha, Red, Green, Blue                    : Cardinal;

  function ArcTan2(xt, yt: Single): Single;
  begin
    if xt = 0 then
    begin
      if yt > 0 then
      begin
        Result := Pi / 2;
      end
      else
      begin
        Result := -(Pi / 2);
      end;
    end
    else
    begin
      Result := ArcTan(yt / xt);

      if xt < 0 then
      begin
        Result := Pi + ArcTan(yt / xt);
      end;
    end;
  end; 

begin
{$RANGECHECKS OFF}

  if AAmount <> 0 then
  begin
    Offset := -(PI / 2);
    dx     := ASourceBmp.Width - 1;
    dy     := ASourceBmp.Height - 1;
    r      := Sqrt(dx * dx + dy * dy);
    tx2    := r;
    ty2    := r;
    txmid  := dx / 2;   // adjust these to move center of rotation
    tymid  := dy / 2;   // adjust these to move ......
    fxmid  := dx / 2;
    fymid  := dy / 2;

    if tx2 >= ASourceBmp.Width then
    begin
      tx2 := dx;
    end;

    if ty2 >= ASourceBmp.Height then
    begin
      ty2 := dy;
    end;

    for ty := 0 to Round(ty2) do
    begin
      for tx := 0 to Round(tx2) do
      begin
        dx := tx - txmid;
        dy := ty - tymid;
        r  := Sqrt(dx * dx + dy * dy);

        if r = 0 then
        begin
          fx := 0;
          fy := 0;
        end
        else
        begin
          Theta := ArcTan2(dx, dy) - r / AAmount - Offset;
          fx    := r * Cos(Theta);
          fy    := r * Sin(Theta);
        end;

        fx := fx + fxmid;
        fy := fy + fymid;

        ify := Trunc(fy);
        ifx := Trunc(fx);

        // Calculate the weights.
        if fy >= 0 then
        begin
          WeightY[1] := fy - ify;
          WeightY[0] := 1 - WeightY[1];
        end
        else
        begin
          WeightY[0] := -(fy - ify);
          WeightY[1] := 1 - WeightY[0];
        end;

        if fx >= 0 then
        begin
          WeightX[1] := fx - ifx;
          WeightX[0] := 1 - WeightX[1];
        end
        else
        begin
          WeightX[0] := -(fx - ifx);
          WeightX[1] := 1 - WeightX[0];
        end;

        if ifx < 0 then
        begin
          ifx := (ASourceBmp.Width - 1) - (-ifx mod ASourceBmp.Width);
        end
        else
        if ifx > (ASourceBmp.Width - 1) then
        begin
          ifx := ifx mod ASourceBmp.Width;
        end;

        if ify < 0 then
        begin
          ify := (ASourceBmp.Height - 1) - (-ify mod ASourceBmp.Height);
        end
        else
        if ify > (ASourceBmp.Height - 1) then
        begin
          ify := ify mod ASourceBmp.Height;
        end;

        TotalAlpha := 0.0;
        TotalRed   := 0.0;
        TotalGreen := 0.0;
        TotalBlue  := 0.0;

        for ix := 0 to 1 do
        begin
          for iy := 0 to 1 do
          begin
            if (ify + iy) < ASourceBmp.Height then
            begin
              sli := ASourceBmp.Scanline[ify + iy];
            end
            else
            begin
              sli := ASourceBmp.Scanline[ASourceBmp.Height - ify - iy];
            end;

            if (ifx + ix) < ASourceBmp.Width then
            begin
              LColumn  := ifx + ix;
              NewAlpha := sli[LColumn] shr 24 and $FF;
              NewRed   := sli[LColumn] shr 16 and $FF;
              NewGreen := sli[LColumn] shr  8 and $FF;
              NewBlue  := sli[LColumn]        and $FF;
            end
            else
            begin
              LColumn  := ASourceBmp.Width - ifx - ix;
              NewAlpha := sli[LColumn] shr 24 and $FF;
              NewRed   := sli[LColumn] shr 16 and $FF;
              NewGreen := sli[LColumn] shr  8 and $FF;
              NewBlue  := sli[LColumn]        and $FF;
            end;

            Weight     := WeightX[ix] * WeightY[iy];
            TotalAlpha := TotalAlpha + NewAlpha * Weight;
            TotalRed   := TotalRed   + NewRed   * Weight;
            TotalGreen := TotalGreen + NewGreen * Weight;
            TotalBlue  := TotalBlue  + NewBlue  * Weight;
          end;
        end;
        
        slo     := ADestBmp.Scanline[ty];
        Alpha   := Round(TotalAlpha);
        Red     := Round(TotalRed);
        Green   := Round(TotalGreen);
        Blue    := Round(TotalBlue);
        slo[tx] := (Alpha shl 24) or (Red shl 16) or (Green shl 8) or Blue;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
