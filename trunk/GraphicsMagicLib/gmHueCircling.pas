{ This is a plug-in filter specifically designed for GraphicsMagic.
  Written by: Gerd Platl

  release 2009-01-22 }

unit gmHueCircling;

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  Windows, GR32;

  procedure BitmapRGBtoHSL(Src, Dest: TBitmap32);
  procedure HueCircling32(const Bmp32: TBitmap32; const HueValue: Byte);

implementation

//*********************************************************
// convert bitmap from RGBA to HSLA format
// input:      src   rgba bitmap: Red-Green-Blue-Alpha-Format
//             dst   hsva bitmap: Hue-Sat-Val-Alpha-Format
//---------------------------------------------------------
procedure BitmapRGBtoHSL(Src, Dest: TBitmap32);
var
  ni: Integer;
  sp: PColor32;
  dp: PColor32Entry;
begin
  Dest.Assign(Src);          // create hsva bitmap
  
  sp := @Src.Bits[0];
  dp := @Dest.Bits[0];
  
  for ni := 0 to (Src.Height * Src.Width) - 1 do
  begin
    RGBtoHSL(sp^, dp.r, dp.g, dp.b);   // convert pixel color format
    Inc(sp);
    Inc(dp);
  end;

  Dest.Changed;
end;

//*********************************************************
// convert HSLA Color to TColor32 (RGBA-Color)
// (extents the original gr32.HSLtoRGB with alpha handling)
// input:      H,S,L,A    Hue, Saturation, Lightness, Alpha (0..255)
// result:     TColor32   RGBA color
//---------------------------------------------------------
function HSLAtoRGBA(H, S, L, A: Integer): TColor32;
var
  V, M, M1, M2, VSF: Integer;
begin
  if L <= $7F then      // L < 127
  begin
    V := L * (256 + S) shr 8;
  end
  else
  begin
    V := L + S - L * S div 255;
  end;
  
  if V <= 0 then
  begin
    Result := Color32(0, 0, 0, A);
  end
  else
  begin
    M   := L * 2 - V;
    H   := H * 6;
    VSF := (V - M) * (H and $FF) shr 8;
    M1  := M + VSF;
    M2  := V - VSF;
    
    case H shr 8 of
      0:
        begin
          Result := Color32(V, M1, M, A);
        end;

      1:
        begin
          Result := Color32(M2, V, M, A);
        end;

      2:
        begin
          Result := Color32(M, V, M1, A);
        end;

      3:
        begin
          Result := Color32(M, M2, V, A);
        end;

      4:
        begin
          Result := Color32(M1, M, V, A);
        end;

      5:
        begin
          Result := Color32(V, M, M2, A);
        end;

    else
      Result := 0;
    end;
  end;
end;

//*********************************************************
// change hue value of any image pixel
// Original author is Gerd Platl
// input:   bmp32     source and destination bitmap
//          hueValue  hue value 0..255
//---------------------------------------------------------
procedure HueCircling32(const Bmp32: TBitmap32; const HueValue: Byte);

var
  ni: Integer;
  sp: PColor32Entry;
  dp: PColor32;
begin
  sp := @Bmp32.Bits[0];
  dp := @Bmp32.Bits[0];

  for ni := 0 to (Bmp32.Height * Bmp32.Width) - 1 do
  begin
    dp^ := HSLAtoRGBA((sp.R + HueValue) and $FF, sp.G, sp.B, sp.A);

    Inc(sp);
    Inc(dp);
  end;
  
  Bmp32.Changed;
end; 

end.
