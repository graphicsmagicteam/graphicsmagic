//  Color Library
//  (Also see PaletteLibrary for Palette-related routines)
//
//  Earl F. Glynn, April 1998.  Updated September 1998.


UNIT ColorLibrary;

INTERFACE

  USES
    Windows,     // TRGBTRiple
    Graphics;    // TBitmap, TPixelFormat

  CONST
    // 16 of the 20 System Palette Colors are defined in Graphics.PAS.
    // The additional 4 colors that NEVER dither, even in 256-color mode,
    // are as follows:  (See Microsoft Systems Journal, Sept. 91,
    // page 119, for Windows 3.0 Default Palette.  Interestingly,
    // some of the "standard" colors weren't always the same!)
    clMoneyGreen = TColor($C0DCC0);   // Color   "8"  RGB:  192 220 192
    clSkyBlue    = TColor($F0CAA6);   // Color   "9"  RGB:  166 202 240
    clCream      = TColor($F0FBFF);   // Color "246"  RGB:  255 251 240
    clMediumGray = TColor($A4A0A0);   // Color "247"  RGB:  160 160 164

    NonDitherColors:  ARRAY[0..19] OF TColor =
      (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clSilver,
       clMoneyGreen, clSkyblue, clCream, clMediumGray,
       clGray, clRed, clGreen, clYellow, clBlue, clFuchsia, clAqua, clWhite);

    // Windows 3.0 Default Palette is used with 8-bit bitmasks.  These 20 values
    // are from the "Microsoft Systems Journal," Sept. 1991, p. 119.  Use these
    // values when looking at the histogram for an 8-bit image.
    //
    // Only the first 8 colors display/print the same in 256 color, 16-bit
    // and 24-bit color modes.
    MaskBlack       =  0;   {RGB =   0   0   0}
    MaskDarkRed     =  1;         {128   0   0}
    MaskDarkGreen   =  2;         {  0 128   0}
    MaskPeaGreen    =  3;         {128 128   0}
    MaskDarkBlue    =  4;         {  0   0 128}
    MaskLavender    =  5;         {128   0 128}
    MaskSlate       =  6;         {  0 128 128}
    MaskLightGray   =  7;         {192 192 192}

  TYPE
    TColorSpace = (csRGB, csHSV, csHLS, csCMYK);
    TColorPlane = (cpRGB,     {really a composite of csRed, csGreen, csBlue}
                   cpRed, cpGreen, cpBlue,
                   cpHueHSV, cpSaturationHSV, cpValue,       // HSV
                   cpHueHLS, cpLightness, cpSaturationHLS,   // HLS
                   cpCyan,   cpMagenta,   cpYellow, cpBlack, // CMYK
                   cpIntensity,
                   cpY);         // Y in YIQ coordinates

  // Miscellaneous
  FUNCTION GetColorPlaneString(CONST ColorPlane:  TColorPlane):  STRING;

  FUNCTION ExtractImagePlane (CONST ColorPlane:  TColorPlane;
                              CONST ColorOutput:  BOOLEAN;
                              CONST Invert:  BOOLEAN;                              
                              CONST OriginalBitmap:  TBitmap):  TBitmap;

  // Color Conversions

  // HLS
  FUNCTION  HLSToRGBTriple (CONST H,L,S:  INTEGER): TRGBTriple;
  PROCEDURE RGBTripleToHLS (CONST RGBTriple:  TRGBTriple;
                            VAR   H,L,S:  INTEGER);

  // HSV
  FUNCTION  HSVToRGBTriple (CONST H,S,V:  INTEGER):  TRGBTriple;
  PROCEDURE RGBTripleToHSV (CONST RGBTriple:  TRGBTriple;
                            VAR   H,S,V:  INTEGER);

  // CMY
  FUNCTION  CMYToRGBTriple (CONST C,M,Y:  INTEGER):  TRGBTriple;
  PROCEDURE RGBTripleToCMY(CONST RGB:  TRGBTriple;    // r, g and b IN [0..255]
                           VAR   C,M,Y:  INTEGER);    // c, m and y IN [0..255]


  // CMYK
  FUNCTION  CMYKToRGBTriple (CONST C,M,Y,K:  INTEGER):  TRGBTriple;
  PROCEDURE RGBTripleToCMYK(CONST RGB:  TRGBTriple;
                            VAR  C,M,Y,K:  INTEGER);

IMPLEMENTATION

  USES
    Math,                        // MaxIntValue, MaxValue
    IEEE754,                     // NAN (not a number)
    ImageProcessingPrimitives,   // pRGBTripleArray
    RealColorLibrary,            // HLStoRGB
    SysUtils;                    // Exception

  TYPE
    EColorError = CLASS(Exception);


  // ==  Miscellaneous  =================================================

  FUNCTION GetColorPlaneString(CONST ColorPlane:  TColorPlane):  STRING;
  BEGIN
    CASE ColorPlane OF
      cpRGB:           RESULT := 'RGB Composite';

      cpRed:           RESULT := 'Red';
      cpGreen:         RESULT := 'Green';
      cpBlue:          RESULT := 'Blue';

      cpHueHSV:        RESULT := 'Hue (HSV)';
      cpSaturationHSV: RESULT := 'Saturation (HSV)';
      cpValue:         RESULT := 'Value (HSV)';

      cpHueHLS:        RESULT := 'Hue (HLS)';
      cpLightness:     RESULT := 'Lightness';
      cpSaturationHLS: RESULT := 'Saturation (HLS)';

      cpIntensity:     RESULT := 'Intensity';
    END

  END {GetColorPlaneString};


  FUNCTION ExtractImagePlane (CONST ColorPlane:  TColorPlane;
                              CONST ColorOutput:  BOOLEAN;
                              CONST Invert:  BOOLEAN;
                              CONST OriginalBitmap:  TBitmap):  TBitmap;
  VAR
    C,M,Y,K     :  INTEGER;
    H,S,V       :  INTEGER;      // color coordinates
    i           :  INTEGER;
    Intensity   :  INTEGER;
    j           :  INTEGER;
    L           :  INTEGER;
    RowOriginal :  pRGBTripleArray;
    RowProcessed:  pRGBTripleArray;
  BEGIN
    IF   OriginalBitmap.PixelFormat <> pf24bit
    THEN RAISE EColorError.Create('GetImageSpace:  ' +
               'Bitmap must be 24-bit color.');

    RESULT := TBitmap.Create;
    RESULT.Width       := OriginalBitmap.Width;
    RESULT.Height      := OriginalBitmap.Height;
    RESULT.PixelFormat := OriginalBitmap.PixelFormat;

    // Step through each row of image.
    FOR j := OriginalBitmap.Height-1 DOWNTO 0 DO
    BEGIN
      RowOriginal  := OriginalBitmap.Scanline[j];
      RowProcessed := RESULT.Scanline[j];

      FOR i := OriginalBitmap.Width-1 DOWNTO 0 DO
      BEGIN
        CASE ColorPlane OF
          // ===============================================================
          cpRGB:
            IF   ColorOutput
            THEN RowProcessed[i] := RowOriginal[i]
            ELSE BEGIN
              Intensity := RGBTripleToIntensity(RowOriginal[i]);
              RowProcessed[i] :=
                   RGBtoRGBTriple(Intensity, Intensity, Intensity)
            END;

          cpRed:
            IF   ColorOutput
            THEN RowProcessed[i] :=
                      RGBtoRGBTriple(RowOriginal[i].rgbtRed, 0, 0)
            ELSE BEGIN
              Intensity := RowOriginal[i].rgbtRed;
              RowProcessed[i] :=
                   RGBtoRGBTriple(Intensity, Intensity, Intensity)
            END;

          cpGreen:
            IF   ColorOutput
            THEN RowProcessed[i] :=
                      RGBtoRGBTriple(0, RowOriginal[i].rgbtGreen, 0)
            ELSE BEGIN
              Intensity := RowOriginal[i].rgbtGreen;
              RowProcessed[i] :=
                   RGBtoRGBTriple(Intensity, Intensity, Intensity)
            END;

          cpBlue:
            IF   ColorOutput
            THEN RowProcessed[i] :=
                      RGBtoRGBTriple(0, 0, RowOriginal[i].rgbtBlue)
            ELSE BEGIN
              Intensity := RowOriginal[i].rgbtBlue;
              RowProcessed[i] :=
                   RGBtoRGBTriple(Intensity, Intensity, Intensity)
            END;

          // ===============================================================
          cpHueHSV:
            BEGIN
              RGBTripleToHSV(RowOriginal[i], H,S,V);
               // "Shades" of Hue with full saturation and value.
              RowProcessed[i] :=  HSVtoRGBTriple(H, 255, 255);

              IF   NOT ColorOutput
              THEN BEGIN
                Intensity := RGBTripleToIntensity(RowProcessed[i]);
                RowProcessed[i] :=
                     RGBtoRGBTriple(Intensity, Intensity, Intensity)
              END
            END;

          cpSaturationHSV:
            BEGIN
              RGBTripletoHSV(RowOriginal[i], H,S,V);
              // "Shades" of Saturation
              RowProcessed[i] := RGBtoRGBTriple(S,S,S)
            END;

          cpValue:
            BEGIN
              RGBTripleToHSV(RowOriginal[i], H,S,V);
              // "Shades" of Value
              RowProcessed[i] := RGBtoRGBTriple(V,V,V)
            END;

          // ===============================================================
          cpHueHLS:
            BEGIN
              RGBTripleToHLS(RowOriginal[i], H,L,S);
              // "Shades" of Hue with half lightness and full saturation.
              RowProcessed[i] := HLStoRGBTriple(H, 128,255);

              IF   NOT ColorOutput
              THEN BEGIN
                Intensity := RGBTripleToIntensity(RowProcessed[i]);
                RowProcessed[i] :=
                     RGBtoRGBTriple(Intensity, Intensity, Intensity)
              END
            END;

          cpLightness:
            BEGIN
              RGBTripleToHLS(RowOriginal[i], H,L,S);
              // "Shades" of Lightness
              RowProcessed[i] := RGBtoRGBTriple(L,L,L)
            END;

          cpSaturationHLS:
            BEGIN
              RGBTripleToHLS(RowOriginal[i], H,L,S);
              // Shades of Saturation
              RowProcessed[i] := RGBtoRGBTriple(S,S,S)
            END;
          // ===============================================================
          cpCyan:
            BEGIN
              RGBTripleToCMYK(RowOriginal[i], C,M,Y,K);
              // Shades of Cyan
              RowProcessed[i] := RGBtoRGBTriple(C,C,C)
            END;

          cpMagenta:
            BEGIN
              RGBTripleToCMYK(RowOriginal[i], C,M,Y,K);
              // Shades of Magenta
              RowProcessed[i] := RGBtoRGBTriple(M,M,M)
            END;

          cpYellow:
            BEGIN
              RGBTripleToCMYK(RowOriginal[i], C,M,Y,K);
              // Shades of Yellow
              RowProcessed[i] := RGBtoRGBTriple(Y,Y,Y)
            END;

          cpBlack:
            BEGIN
              RGBTripleToCMYK(RowOriginal[i], C,M,Y,K);
              // Shades of "Black"
              RowProcessed[i] := RGBtoRGBTriple(K,K,K)
            END;

          // ===============================================================
          cpIntensity:
            BEGIN
              Intensity := RGBTripleToIntensity(RowOriginal[i]);
              // Shades of Intensity
              RowProcessed[i] :=
                   RGBtoRGBTriple(Intensity, Intensity, Intensity)
            END

        END;  {Case}

        IF  Invert
        THEN RowProcessed[i] := RGBTripleInvert( RowProcessed[i] )

      END;

    END
  END {ExtractImagePlane};


  // == HLS / RGB =======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 596.

  FUNCTION HLStoRGBTriple (CONST H,L,S:  INTEGER):  TRGBTriple;
    VAR
      R,G,B:  TReal;
  BEGIN
    HLStoRGB(H, L/255, S/255, R,G,B);
    RESULT := ColorToRGBTriple( RGB(ROUND(255*R), ROUND(255*G), ROUND(255*B)) )
  END {HLStoRGBTriple};


  // RGB to HLS
  // H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  // L = 0 (shade of gray) to 255 (pure color)
  // S = 0 (black) to 255 {white)
  //
  // R, G, B each in [0, 255]

  PROCEDURE RGBTripleToHLS (CONST RGBTriple:  TRGBTriple;
                            VAR   H,L,S:  INTEGER);
    VAR
      Hue       :  TReal;
      Lightness :  TReal;
      Saturation:  TReal;
  BEGIN
    WITH RGBTriple DO
      RGBToHLS(rgbtRed/255, rgbtGreen/255, rgbtBlue/255,
               Hue, Lightness, Saturation);

    IF   IsNan(Hue)
    THEN H := 0
    ELSE H := ROUND(Hue);         // 0..360
    L := ROUND(255*Lightness);    // 0..255
    S := ROUND(255*Saturation);   // 0..255
  END {RGBTripleToHLS};
 

  // Floating point fractions, 0..1, replaced with integer values, 0..255.
  // Use integer conversion ONLY for one-way, or a single final conversions.
  // Use floating-point for converting reversibly (see HSVtoRGB above).
  //
  //  H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  //      0 (undefined) for S = 0
  //  S = 0 (shade of gray) to 255 (pure color)
  //  V = 0 (black) to 255 (white)

  FUNCTION HSVtoRGBTriple (CONST H,S,V:  INTEGER):  TRGBTriple;
    CONST
      divisor:  INTEGER = 255*60;
    VAR
      f    :  INTEGER;
      hTemp:  INTEGER;
      p,q,t:  INTEGER;
      VS   :  INTEGER;
  BEGIN
    IF   S = 0
    THEN RESULT := RGBtoRGBTriple(V, V, V)  // achromatic:  shades of gray
    ELSE BEGIN                              // chromatic color
      IF   H = 360
      THEN hTemp := 0
      ELSE hTemp := H;

      f     := hTemp MOD 60;     // f is IN [0, 59]
      hTemp := hTemp DIV 60;     // h is now IN [0,6)

      VS := V*S;
      p := V - VS DIV 255;                 // p = v * (1 - s)
      q := V - (VS*f) DIV divisor;         // q = v * (1 - s*f)
      t := V - (VS*(60 - f)) DIV divisor;  // t = v * (1 - s * (1 - f))

      CASE hTemp OF
        0:   RESULT := RGBtoRGBTriple(V, t, p);
        1:   RESULT := RGBtoRGBTriple(q, V, p);
        2:   RESULT := RGBtoRGBTriple(p, V, t);
        3:   RESULT := RGBtoRGBTriple(p, q, V);
        4:   RESULT := RGBtoRGBTriple(t, p, V);
        5:   RESULT := RGBtoRGBTriple(V, p, q);
        ELSE RESULT := RGBtoRGBTriple(0,0,0)  // should never happen;
                                              // avoid compiler warning
      END
    END
  END {HSVtoRGBTriple};


   // RGB, each 0 to 255, to HSV.
  //   H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  //   S = 0 (shade of gray) to 255 (pure color)
  //   V = 0 (black) to 255 {white)

  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
  // integer values, 0..255.

  PROCEDURE RGBTripleToHSV (CONST RGBTriple: TRGBTriple;  {r, g and b IN [0..255]}
                            VAR   H,S,V:  INTEGER);    {h IN 0..359; s,v IN 0..255}
    VAR
      Delta:  INTEGER;
      Min  :  INTEGER;
  BEGIN
    WITH RGBTriple DO
    BEGIN
      Min := MinIntValue( [rgbtRed, rgbtGreen, rgbtBlue] );
      V   := MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] )
    END;

    Delta := V - Min;

    // Calculate saturation:  saturation is 0 if r, g and b are all 0
    IF   V =  0
    THEN S := 0
    ELSE S := MulDiv(Delta, 255, V);

    IF   S  = 0
    THEN H := 0   // Achromatic:  When s = 0, h is undefined but assigned the value 0
    ELSE BEGIN    // Chromatic

      WITH RGBTriple DO
      BEGIN
        IF   rgbtRed = V
        THEN  // degrees -- between yellow and magenta
              H := MulDiv(rgbtGreen - rgbtBlue, 60, Delta)
        ELSE
          IF   rgbtGreen = V
          THEN // between cyan and yellow
               H := 120 + MulDiv(rgbtBlue-rgbtRed, 60, Delta)
          ELSE
            IF  rgbtBlue = V
            THEN // between magenta and cyan
                 H := 240 + MulDiv(rgbtRed-rgbtGreen, 60, Delta);
      END;

      IF   H < 0
      THEN H := H + 360;

    END
  END {RGBTripleToHSV};


  // == CMY / RGB =======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 588


  // R, G, B, C, M, Y each IN [0..255]
  FUNCTION  CMYtoRGBTriple (CONST C,M,Y:  INTEGER):  TRGBTriple;
  BEGIN
    WITH RESULT DO
    BEGIN
      rgbtRed   := 255 - C;
      rgbtGreen := 255 - M;
      rgbtBlue  := 255 - Y
    END
  END {CMYtoRGBTriple};


  // R, G, B, C, M, Y each IN [0..255]
  PROCEDURE RGBTripleToCMY(CONST RGB:  TRGBTriple;
                           VAR   C,M,Y:  INTEGER);
  BEGIN
    WITH RGB DO
    BEGIN
      C := 255 - rgbtRed;
      M := 255 - rgbtGreen;
      Y := 255 - rgbtBlue
    END
  END {RGBtoCMY};


  // == CMYK / RGB ======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 589


  // R, G, B, C, M, Y,K each IN [0..255]
  FUNCTION  CMYKtoRGBTriple (CONST C,M,Y,K:  INTEGER):  TRGBTriple;
  BEGIN
    WITH RESULT DO
    BEGIN
      rgbtRed   := 255 - (C + K);
      rgbtGreen := 255 - (M + K);
      rgbtBlue  := 255 - (Y + K)
    END
  END {CMYtoRGBTriple};


  // R, G, B, C, M, Y each IN [0..255]
  PROCEDURE RGBTripleToCMYK(CONST RGB:  TRGBTriple;
                            VAR  C,M,Y,K:  INTEGER);
  BEGIN
    RGBTripleToCMY(RGB, C,M,Y);
    K := MinIntValue([C, M, Y]);
    C := C - K;
    M := M - K;
    Y := Y - K
  END {RGBtoCMYK};


END.
