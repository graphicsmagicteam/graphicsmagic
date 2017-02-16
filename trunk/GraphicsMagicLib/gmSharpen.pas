{ These routines was adapted from Image processing Unit Version 2 of EStampe
  demo written by Jean Yves Queinec - j.y.q@wanadoo.fr

  We adapted the original code to support Graphics32.}

unit gmSharpen;

interface

uses
  GR32;

type
  TgmSharpenMode = (smSharpen, smSharpenMore);

  procedure SharpenBitmap(const ADestBmp: TBitmap32; const AMode: TgmSharpenMode);

implementation

uses
  GR32_LowLevel;

procedure SharpenBitmap(const ADestBmp: TBitmap32; const AMode: TgmSharpenMode);
const
  MASK_MATRIX: array [0..24] of Integer = (1,1,1,1,1,
                                           1,0,0,0,1,
                                           1,0,0,0,1,
                                           1,0,0,0,1,
                                           1,1,1,1,1);

  SHARPEN_MATRIX_1   : array [0 .. 9] of Integer = (-1,-2,-1, -2,28,-2, -1,-2,-1, 16);
  SHARPEN_MATRIX_2   : array [0 .. 9] of Integer = (-2,-1,-2, -1,28,-1, -2,-1,-2, 16);
  SHARPEN_MORE_MATRIX: array [0 .. 9] of Integer = (0,-1,0, -1,6,-1, 0,-1,0, 2);

var
  LBmpCopy           : TBitmap32;
  LSharpenTime       : Integer;
  LLastMatrixNumber  : Integer;
  i, x, y, ix, iy, dx: Integer;
  LDiagonal          : Integer;
  LDiagonalX         : Integer;
  LDiagonalY         : Integer;
  aa, rr, gg, bb     : Integer;
  a, r, g, b         : Byte;
  LMatrix            : array [0..24] of Integer;
  LOriginalRow       : array of PColor32Array;
  LDestRow           : array of PColor32Array;

  procedure LoadSharpenMatrix(AMatrix: array of Integer);
  var
    i, j: Integer;
  begin
    for j := 0 to 24 do
    begin
      LMatrix[j] := 0;
    end;

    i := 0;

    for j := 6 to 8 do
    begin
      LMatrix[j] := AMatrix[i];
      Inc(i);
    end;

    for j := 11 to 13 do
    begin
      LMatrix[j] := AMatrix[i];
      Inc(i);
    end;

    for j := 16 to 18 do
    begin
      LMatrix[j] := AMatrix[i];
      Inc(i);
    end;

    LLastMatrixNumber := AMatrix[9];
  end;

begin
{$RANGECHECKS OFF} 

  LSharpenTime := 0;

  case AMode of
    smSharpen:
      begin
        Inc(LSharpenTime);

        if (LSharpenTime mod 2) = 0 then
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_1);
        end
        else
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_2);
        end;
      end;

    smSharpenMore:
      begin
        LoadSharpenMatrix(SHARPEN_MORE_MATRIX);
      end;
  end;


  { scanlines arrays 3 octets (24 bits) optimization bitmaps Maximum 2048
    lines get the access port of the dest and the original bitmap }
  SetLength(LOriginalRow, ADestBmp.Height);
  SetLength(LDestRow, ADestBmp.Height);
  
  LBmpCopy := TBitmap32.Create;
  try
    LBmpCopy.Assign(ADestBmp);
    
    for i := 0 to (ADestBmp.Height - 1) do
    begin
      LOriginalRow[i] := LBmpCopy.ScanLine[i];
      LDestRow[i]     := ADestBmp.ScanLine[i];
    end;

    if LLastMatrixNumber = 0 then
    begin
      LLastMatrixNumber := 1;
    end;

    dx := 0;
    for i := 0 to 24 do
    begin
      if (LMatrix[i] and MASK_MATRIX[i]) <> 0 then
      begin
        Inc(dx);
      end;
    end;

    if dx = 0 then
    begin
      LDiagonal := 1;
    end
    else
    begin
      LDiagonal := 2;
    end;

    for y := 0 to (ADestBmp.Height - 1) do
    begin
      for x := 0 to (ADestBmp.Width - 1) do
      begin
        aa := 0;
        rr := 0;
        gg := 0;
        bb := 0;

        for LDiagonalY := -LDiagonal to LDiagonal do
        begin
          for LDiagonalX := -LDiagonal to LDiagonal do
          begin
            iy := y + LDiagonalY;
            ix := x + LDiagonalX;

            { The original routines in the following checking code was
              if  (iy >= 1) ...
              and (ix >= 1) ...  }
            if (iy >= 0) and
               (ix >= 0) and
               ( iy <= (ADestBmp.Height - 1) ) and
               ( ix <= (ADestBmp.Width  - 1) ) then
            begin
              a := LOriginalRow[iy, ix] shr 24 and $FF;
              r := LOriginalRow[iy, ix] shr 16 and $FF;
              g := LOriginalRow[iy, ix] shr  8 and $FF;
              b := LOriginalRow[iy, ix]        and $FF;
            end
            else
            begin
              a := LOriginalRow[y, x] shr 24 and $FF;
              r := LOriginalRow[y, x] shr 16 and $FF;
              g := LOriginalRow[y, x] shr  8 and $FF;
              b := LOriginalRow[y, x]        and $FF;
            end;

            i  := 12 + LDiagonalY * 5 + LDiagonalX;
            aa := aa + a * LMatrix[i];
            rr := rr + r * LMatrix[i];
            gg := gg + g * LMatrix[i];
            bb := bb + b * LMatrix[i];
          end;
        end;

        aa := aa div LLastMatrixNumber;
        rr := rr div LLastMatrixNumber;
        gg := gg div LLastMatrixNumber;
        bb := bb div LLastMatrixNumber;

        a := Clamp(aa, 0, 255);
        r := Clamp(rr, 0, 255);
        g := Clamp(gg, 0, 255);
        b := Clamp(bb, 0, 255);

        LDestRow[y, x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
      end;
    end;
  finally
    LBmpCopy.Free;
    SetLength(LDestRow, 0);
    SetLength(LOriginalRow, 0);
  end;

{$RANGECHECKS ON}
end; 

end.
