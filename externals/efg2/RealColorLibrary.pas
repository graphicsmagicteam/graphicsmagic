//  Real (as in "Float") Color Library
//
//  efg, September 1998
//  September 2001, HLStoRGB error corrected.

UNIT RealColorLibrary;

INTERFACE

  USES
    Windows,                      // TRGBQuad
    Graphics,                     // TBitmap
    ImageProcessingPrimitives;    // TReal, pRGBQuadArray


  TYPE
    TRealToRGBConversion = PROCEDURE (CONST x:  TReal; VAR R,G,B: BYTE);

    TRealColorMatrix =   // pf32bit Bitmap, or matrix of single float values
    CLASS(TBitmap)
      PRIVATE
        FUNCTION  GetReal(i,j:  Integer):  TReal;
        PROCEDURE SetReal(i,j:  Integer; value:  TReal);

      PUBLIC
        CONSTRUCTOR Create;  Override;
        PROCEDURE   ConvertRealMatrixToRGBImage(ConversionFunction:  TRealToRGBConversion);
        PROPERTY    Element[i,j:  Integer]:  TReal  READ GetReal Write SetReal;  DEFAULT;

    END;


  // Color Conversions

  // HLS
  PROCEDURE HLStoRGB(CONST H,L,S:  TReal; VAR R,G,B:  TReal);
  PROCEDURE RGBToHLS(CONST R,G,B:  TReal; VAR H,L,S:  TReal);

  // HSV
  PROCEDURE HSVtoRGB(CONST H,S,V:  TReal; VAR R,G,B:  TReal);
  PROCEDURE RGBToHSV(CONST R,G,B:  TReal; VAR H,S,V:  TReal);

  // CMY
  PROCEDURE CMYtoRGB(CONST C,M,Y:  TReal; VAR R,G,B:  TReal);
  PROCEDURE RGBToCMY(CONST R,G,B:  TReal; VAR C,M,Y:  TReal);

  // CMYK
  PROCEDURE CMYKtoRGB(CONST C,M,Y,K:  TReal; VAR R,G,B  :  TReal);
  PROCEDURE RGBToCMYK(CONST R,G,B  :  TReal; VAR C,M,Y,K:  TReal);


  // Color Matrices:  All Bitmap parameters are pf24bit bitmaps
  PROCEDURE BitmapToRGBMatrices(CONST Bitmap:  TBitmap;
                                VAR Red,Green,Blue:  TRealColorMatrix);
  FUNCTION  RGBMatricesToBitmap(CONST Red,Green,Blue:  TRealColorMatrix):  TBitmap;

  PROCEDURE RGBMatricesToHSVMatrices(CONST Red,Green,Blue: TRealColorMatrix;
                                     VAR   Hue,Saturation,Value:  TRealColorMatrix);
  PROCEDURE HSVMatricesToRGBMatrices(CONST Hue,Saturation,Value:  TRealColorMatrix;
                                     VAR   Red,Green,Blue: TRealColorMatrix);


IMPLEMENTATION

  USES
    Math,                        // MaxValue
    IEEE754,                     // NAN (not a number)
    SysUtils;                    // Exception

  TYPE
    EColorError = CLASS(Exception);

  // == TColorMatrix ====================================================
  //


  CONSTRUCTOR TRealColorMatrix.Create;
  BEGIN
    ASSERT (SizeOf(TRGBQuad) = SizeOf(TReal));  // to be sure

    Inherited Create;
    PixelFormat := pf32bit;   // Each "pixel" is a "single"
  END {Create};


  PROCEDURE TRealColorMatrix.ConvertRealMatrixToRGBImage(ConversionFunction:  TRealToRGBConversion);
    VAR
      FloatRow:  pSingleArray;
      i       :  INTEGER;
      j       :  INTEGER;
      PixelRow:  pRGBQuadArray;
      R,G,B   :  BYTE;
  BEGIN
    FOR j := 0 TO Height-1 DO
    BEGIN
      PixelRow := Scanline[j];
      FloatRow := Scanline[j];
      FOR i := 0 TO Width-1 DO
      BEGIN
        ConversionFunction(FloatRow[i], R,G,B);
        WITH   PixelRow[i] DO
        BEGIN
          rgbRed      := R;
          rgbGreen    := G;
          rgbBlue     := B;
          rgbReserved := 0
        END
      END
    END
  END {ConvertSingleToRGB};


  FUNCTION TRealColorMatrix.GetReal(i,j:  Integer):  TReal;
  BEGIN
    RESULT := pSingleArray(Scanline[j])[i]
  END {GetSingle};


  PROCEDURE TRealColorMatrix.SetReal(i,j:  Integer; value:  TReal);
  BEGIN
    pSingleArray(Scanline[j])[i] := value
  END {SetSingle};


  // == HLS / RGB =======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 596.

  PROCEDURE HLStoRGB(CONST H,L,S:  TReal; VAR R,G,B:  TReal);
    VAR
      m1:  TReal;
      m2:  TReal;

    FUNCTION Value(CONST n1,n2:  TReal;  hue:  TReal):  TReal;
    BEGIN
      IF   hue > 360.0
      THEN hue := hue - 360.0
      ELSE
        IF   hue < 0.0
        THEN hue := hue + 360.0;

      IF   hue < 60.0
      THEN RESULT := n1 + (n2 - n1)*hue / 60.0
      ELSE
        IF   hue < 180
        THEN RESULT := n2
        ELSE
          IF   hue < 240.0
          THEN RESULT := n1 + (n2-n1)*(240.0 - hue) / 60.0
          ELSE RESULT := n1
    END {Value};

  BEGIN

    // There is an error in Computer Graphics Principles and Practice,
    // Foley, et al, 1996, pp. 592-596.  The formula uses a lower case
    // "el", "l", and defines in C:
    //
    //      m2 = (l < 0.5) ? (l * (l+s)):(l+s-l*s)
    //
    // This is a perfect example of why "l" (a lower case "el") should
    // NEVER be used as a variable name, and why a programming convention --
    // to use lower case letters in variable names -- should not override
    // the problem definition, which defines the color space using an "L".
    // The 1982 version of the book, in Pascal, shows the correct formula
    // (but alas, also used a lower case "l"):
    //
    //      if   l <= 0.5
    //      then m2 := l*(1+s)    //  NOTE the one, in "1+s", here
    //      else m2 := l + s - l*s 
    //
    // [Thanks to Gary Freestone, IBM Global Services Australia, for
    // bringing this to my attention.  efg, Sept. 2001]

    IF   L <= 0.5
    THEN m2 := L * (1 + S)
    ELSE m2 := L + S - L*S;

    m1 := 2.0 * L - m2;

    IF   S = 0.0
    THEN BEGIN      // achromatic -- no hue
      IF   IsNAN(H)
      THEN BEGIN
        R := L;
        G := L;
        B := L
      END
      ELSE RAISE EColorError.Create('HLStoRGB:  S = 0 and H has a value');
    END
    ELSE BEGIN
      // Chromatic case -- there is a hue
      R := Value(m1, m2, H + 120.0);
      G := Value(m1, m2, H);
      B := Value(m1, m2, H - 120.0)
    END
  END {HLStoRGB};


  // H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
  // L = 0.0 (shade of gray) to 1.0 (pure color)
  // S = 0.0 (black)         to 1.0 {white)
  //
  // R, G, B each in [0,1]
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 595.

  PROCEDURE RGBToHLS (CONST R,G,B:  TReal; VAR   H,L,S:  TReal);
    VAR
      Delta:  TReal;
      Max  :  TReal;
      Min  :  TReal;
  BEGIN
    Max := MaxValue( [R, G, B] );
    Min := MinValue( [R, G, B] );

    L := (Max + Min) / 2.0;   // Lightness

    IF   Max = Min            // Achromatic case since r = g = b
    THEN BEGIN
      S := 0.0;
      H := NAN;               // Undefined
    END
    ELSE BEGIN
      Delta := Max - Min;

      IF   L <= 0.5
      THEN S := Delta / (Max + Min)
      ELSE S := Delta / (2.0 - (Max + Min));

      IF   R = Max
      THEN // degrees between yellow and magenta
           H := (60.0*(G - B)) / Delta
      ELSE
        IF   G = Max
        THEN // degrees between cyan and yellow
             H := 120.0 + (60.0*(B - R)) / Delta
        ELSE
          IF  B = Max
          THEN // degrees between magenta and cyan
               H := 240.0 + (60.0*(R - G)) / Delta;

      IF   H < 0
      THEN H := H + 360.0;  // Keep in interval [0, 360)

    END
  END {RGBtoHLS};


  // == HSV / RGB =======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 593.
  //
  //  H = 0.0 to 360.0 (corresponding to 0..360 degrees around hexcone)
  //      NaN (undefined) for S = 0
  //  S = 0.0 (shade of gray) to 1.0 (pure color)
  //  V = 0.0 (black)         to 1.0 (white)

  PROCEDURE  HSVtoRGB (CONST H,S,V:  TReal; VAR R,G,B:  TReal);
    VAR
      f    :  TReal;
      i    :  INTEGER;
      hTemp:  TReal;              // since H is CONST parameter
      p,q,t:  TReal;
  BEGIN
    IF   S = 0.0                  // color is on black-and-white center line
    THEN BEGIN
      IF   IsNaN(H)
      THEN BEGIN
        R := V;                   // achromatic:  shades of gray
        G := V;
        B := V
      END
      ELSE RAISE EColorError.Create('HSVtoRGB:  S = 0 and H has a value');
    END

    ELSE BEGIN                    // chromatic color
      IF   H = 360.0              // 360 degrees same as 0 degrees
      THEN hTemp := 0.0
      ELSE hTemp := H;

      hTemp := hTemp / 60;        // h is now IN [0,6)
      i := TRUNC(hTemp);          // largest integer <= h
      f := hTemp - i;             // fractional part of h

      p := V * (1.0 - S);
      q := V * (1.0 - (S * f));
      t := V * (1.0 - (S * (1.0 - f)));

      CASE i OF
        0:  BEGIN R := V;  G := t;  B := p  END;
        1:  BEGIN R := q;  G := V;  B := p  END;
        2:  BEGIN R := p;  G := V;  B := t  END;
        3:  BEGIN R := p;  G := q;  B := V  END;
        4:  BEGIN R := t;  G := p;  B := V  END;
        5:  BEGIN R := V;  G := p;  B := q  END
      END
    END
  END {HSVtoRGB};


  // RGB, each 0 to 255, to HSV.
  //   H = 0.0 to 360.0 (corresponding to 0..360.0 degrees around hexcone)
  //   S = 0.0 (shade of gray) to 1.0 (pure color)
  //   V = 0.0 (black) to 1.0 {white)

  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
  // integer values, 0..255.

  PROCEDURE RGBToHSV (CONST R,G,B:  TReal; VAR H,S,V:  TReal);
    VAR
      Delta:  TReal;
      Min  :  TReal;
  BEGIN
    Min := MinValue( [R, G, B] );
    V   := MaxValue( [R, G, B] );

    Delta := V - Min;

    // Calculate saturation:  saturation is 0 if r, g and b are all 0
    IF   V =  0.0
    THEN S := 0
    ELSE S := Delta / V;

    IF   S  = 0.0
    THEN H := NAN // Achromatic:  When s = 0, h is undefined
    ELSE BEGIN    // Chromatic
      IF   R = V
      THEN  // between yellow and magenta [degrees]
            H := 60.0 * (G - B) / Delta
      ELSE
        IF   G = V
        THEN // between cyan and yellow
             H := 120.0 + 60.0 * (B - R) / Delta
        ELSE
          IF  B = V
          THEN // between magenta and cyan
               H := 240.0 + 60.0 * (R - G) / Delta;

      IF   H < 0.0
      THEN H := H + 360.0
    END
  END {RGBtoHSV};


  // == CMY / RGB =======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 588

  // R, G, B, C, M, Y each IN [0.0 .. 1.0]
  PROCEDURE CMYtoRGB(CONST C,M,Y:  TReal; VAR   R,G,B:  TReal);
  BEGIN
    R := 1.0 - C;
    G := 1.0 - M;
    B := 1.0 - Y
  END {CMYtoRGB};


  // R, G, B, C, M, Y each IN [0.0 .. 1.0]
  PROCEDURE RGBtoCMY(CONST R,G,B:  TReal;  VAR C,M,Y:  TReal);
  BEGIN
    C := 1.0 - R;
    M := 1.0 - G;
    Y := 1.0 - B
  END {RGBtoCMY};


  // == CMYK / RGB ======================================================
  //
  // Based on C Code in "Computer Graphics -- Principles and Practice,"
  // Foley et al, 1996, p. 589

  // R, G, B, C, M, Y, K each IN [0.0 .. 1.0]
  PROCEDURE CMYKtoRGB(CONST C,M,Y,K:  TReal; VAR   R,G,B:  TReal);
  BEGIN
    R := 1.0 - (C + K);
    G := 1.0 - (M + K);
    B := 1.0 - (Y + K)
  END {CMYtoRGB};


  // R, G, B, C, M, Y each IN [0.0 .. 1.0]
  PROCEDURE RGBToCMYK(CONST R,G,B:  TReal;  VAR C,M,Y,K:  TReal);
  BEGIN
    RGBtoCMY(R,G,B, C,M,Y);
    K := MinValue([C, M, Y]);
    C := C - K;
    M := M - K;
    Y := Y - K
  END {RGBtoCMYK};


  // == Color Matrices  =================================================
  //

  PROCEDURE BitmapToRGBMatrices(CONST Bitmap:  TBitmap;
                                    VAR Red,Green,Blue:  TRealColorMatrix);
    VAR
      i,j:  INTEGER;
      row:  pRGBTripleArray;
  BEGIN
    ASSERT (Bitmap.PixelFormat = pf24bit);

    Red        := TRealColorMatrix.Create;
    Red.Width  := Bitmap.Width;
    Red.Height := Bitmap.Height;

    Green := TRealColorMatrix.Create;
    Green.Width  := Bitmap.Width;
    Green.Height := Bitmap.Height;

    Blue  := TRealColorMatrix.Create;
    Blue.Width  := Bitmap.Width;
    Blue.Height := Bitmap.Height;

    FOR j := 0 TO Bitmap.Height-1 DO
    BEGIN
      row := Bitmap.Scanline[j];
      FOR i := 0 TO Bitmap.Width-1 DO
      BEGIN
        Red[i,j]   := row[i].rgbtRed   / 255;   // 0.0 to 1.0
        Green[i,j] := row[i].rgbtGreen / 255;
        Blue[i,j]  := row[i].rgbtBlue  / 255;
      END
    END

  END {BitmapToRGBMatrices};




  FUNCTION  RGBMatricesToBitmap(CONST Red,Green,Blue:  TRealColorMatrix):  TBitmap;
    VAR
      i,j:  INTEGER;
      row:  pRGBTripleArray;
  BEGIN
    ASSERT( (Red.Width  = Green.Width)  AND
            (Red.Width  = Blue.Width)   AND
            (Red.Height = Green.Height) AND
            (Red.Height = Blue.Height) );

    RESULT             := TBitmap.Create;
    RESULT.Width       := Red.Width;
    RESULT.Height      := Red.Height;
    RESULT.PixelFormat := pf24bit;

    FOR j := 0 TO Red.Height-1 DO
    BEGIN
      row := RESULT.Scanline[j];
      FOR i := 0 TO Red.Width-1 DO
      BEGIN
        WITH row[i] DO
        BEGIN
          rgbtRed   := TRUNC(Red[i,j]   * 255 + 0.5);
          rgbtGreen := TRUNC(Green[i,j] * 255 + 0.5);
          rgbtBlue  := TRUNC(Blue[i,j]  * 255 + 0.5);
        END
      END
    END

  END {RGBMatricesToBitmap};


  PROCEDURE RGBMatricesToHSVMatrices(CONST Red,Green,Blue: TRealColorMatrix;
                                     VAR   Hue,Saturation,Value:  TRealColorMatrix);
    VAR
      i,j  :  INTEGER;
      H,S,V:  TReal;
  BEGIN
    ASSERT( (Red.Width  = Green.Width)  AND
            (Red.Width  = Blue.Width)   AND
            (Red.Height = Green.Height) AND
            (Red.Height = Blue.Height)  AND

            (Hue.Width  = Saturation.Width)  AND
            (Hue.Width  = Value.Width)       AND
            (Hue.Height = Saturation.Height) AND
            (Hue.Height = Value.Height) );

    FOR j := 0 TO Red.Height-1 DO
    BEGIN
      FOR i := 0 TO Red.Width-1 DO
      BEGIN
        RGBToHSV(Red[i,j], Green[i,j], Blue[i,j], H, S, V);
        Hue[i,j]        := H;
        Saturation[i,j] := S;
        Value[i,j]      := V
      END
    END
  END {RGBMatricesToHSVMatrices};


  PROCEDURE HSVMatricesToRGBMatrices(CONST Hue,Saturation,Value:  TRealColorMatrix;
                                     VAR   Red,Green,Blue: TRealColorMatrix);
   VAR
      i,j  :  INTEGER;
      R,G,B:  TReal;
  BEGIN
    ASSERT( (Red.Width  = Green.Width)  AND
            (Red.Width  = Blue.Width)   AND
            (Red.Height = Green.Height) AND
            (Red.Height = Blue.Height)  AND

            (Hue.Width  = Saturation.Width)  AND
            (Hue.Width  = Value.Width)       AND
            (Hue.Height = Saturation.Height) AND
            (Hue.Height = Value.Height) );

    FOR j := 0 TO Red.Height-1 DO
    BEGIN
      FOR i := 0 TO Red.Width-1 DO
      BEGIN
        HSVToRGB(Hue[i,j],
                 Saturation[i,j],
                 Value[i,j],
                 R, G, B);
        Red[i,j]   := R;
        Green[i,j] := G;
        Blue[i,j]  := B
      END
    END

  END {HSVMatriceesToRGBMatrices};

END.
