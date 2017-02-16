//  Image Processing Primitives Library
//  (See ImageProcessingLibrary for High-Level Library)
//
//  Earl F. Glynn, April 1998.  Updated September 1998.

UNIT ImageProcessingPrimitives;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

INTERFACE

  USES
    Windows,   // TRGBTriple
    Graphics;  // TPixelFormat, TCanvas

  CONST
    MaxPixelCount = 65536;

  TYPE
    TReal       = Single;

    // use SysUtils.pByteArray for pf8bit Scanlines

    // For pf24bit Scanlines
    pRGBTripleArray = ^TRGBTripleArray;
    TRGBTripleArray = ARRAY[0..MaxPixelCount-1] OF TRGBTriple;

    // for pf32bit Scanlines
    pRGBQuadArray = ^TRGBQuadArray;
    TRGBQuadArray = ARRAY[0..MaxPixelCount-1] OF TRGBQuad;
    pSingleArray  = ^TSingleArray;
    TSingleArray  = ARRAY[0..MaxPixelCount-1] OF TReal;


  // General info
  FUNCTION GetBitmapDimensionsString(CONST Bitmap:  TBitmap):  STRING;
  FUNCTION GetPixelFormatString(CONST PixelFormat:  TPixelFormat):  STRING;

  // Bitmap manipulations
  PROCEDURE PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);

  // TRGBTriple
  FUNCTION AreRGBTriplesEqual(CONST Triple1, Triple2:  TRGBTriple):  BOOLEAN;
  FUNCTION ColorToRGBTriple(CONST Color:  TColor):  TRGBTriple;
  FUNCTION RGBtoRGBTriple(CONST red, green, blue:  BYTE):  TRGBTriple;
  FUNCTION RGBTripleToColor(CONST RGBTriple:  TRGBTriple):  TColor;
  FUNCTION RGBTripleInvert (CONST RGBTriple:  TRGBTriple):  TRGBTriple;

  // RGBTriple Array manipulations
  FUNCTION RGBTripleAverage  (RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
  FUNCTION RGBTripleBrightest(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
  FUNCTION RGBTripleMaximum  (RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
  FUNCTION RGBTripleMedian   (RGB:  ARRAY OF TRGBTriple):  TRGBTRiple;
  FUNCTION RGBTripleMinimum  (RGB:  ARRAY OF TRGBTriple):  TRGBTriple;

  // TRGBTriple Brightness:  Intensity, Lightness, Value, Y; Saturation
  FUNCTION RGBTripleToIntensity (CONST RGB:  TRGBTriple):  INTEGER;
  FUNCTION RGBTripleToLightness (CONST RGB:  TRGBTriple):  INTEGER;
  FUNCTION RGBTripleToSaturation(CONST RGB:  TRGBTriple):  INTEGER;
  FUNCTION RGBTripleToValue     (CONST RGB:  TRGBTriple):  INTEGER;
  FUNCTION RGBTripleToY         (CONST RGB:  TRGBTriple):  INTEGER;

  FUNCTION CountColors(CONST Bitmap:  TBitmap):  INTEGER;

IMPLEMENTATION

  USES
    Classes,    // TBits
    Math,       // MinIntValue, MaxIntValue
    SysUtils;   // IntToStr


  //==  General Info  ======================================================

  FUNCTION GetBitmapDimensionsString(CONST Bitmap:  TBitmap):  STRING;
  BEGIN
    RESULT := IntToStr(Bitmap.Width)  + ' by ' +
              IntToStr(Bitmap.Height) + ' pixels by ' +
              GetPixelFormatString(Bitmap.PixelFormat) + ' color';
    IF   Bitmap.HandleType = bmDDB
    THEN RESULT := RESULT + '(DDB)';
  END {GetBitmapDimensionsString};


  FUNCTION GetPixelFormatString(CONST PixelFormat:  TPixelFormat):  STRING;
  BEGIN
    CASE PixelFormat OF
      pfDevice:  RESULT := 'Device';
      pf1bit:    RESULT := '1 bit';
      pf4bit:    RESULT := '4 bit';
      pf8bit:    RESULT := '8 bit';
      pf15bit:   RESULT := '15 bit';
      pf16bit:   RESULT := '16 bit';
      pf24bit:   RESULT := '24 bit';
      pf32bit:   RESULT := '32 bit'
      ELSE       RESULT := 'Unknown'
    END
  END {GetPixelFormatString};


  //==  Bitmap Manipulations  ==============================================

  // Based on posting to borland.public.delphi.winapi by Rodney E Geraghty,
  // 8/8/97.  Used to print bitmap on any Windows printer.
  PROCEDURE PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
    VAR
      BitmapHeader:  pBitmapInfo;
      BitmapImage :  POINTER;
      HeaderSize  :  DWORD;    // Use DWORD for compatibility with D3-D5
      ImageSize   :  DWORD;
  BEGIN
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage,  ImageSize);
    TRY
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     // Destination Origin
                    DestRect.Right  - DestRect.Left, // Destination Width
                    DestRect.Bottom - DestRect.Top,  // Destination Height
                    0, 0,                            // Source Origin
                    Bitmap.Width, Bitmap.Height,     // Source Width & Height
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY)
    FINALLY
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    END
  END {PrintBitmap};

  //==  TRGBTriple   =====================================================


  FUNCTION AreRGBTriplesEqual(CONST Triple1, Triple2:  TRGBTriple):  BOOLEAN;
  BEGIN
    RESULT := (Triple1.rgbtRed   = Triple2.rgbtRed  )  AND
              (Triple1.rgbtGreen = Triple2.rgbtGreen)  AND
              (Triple1.rgbtBlue  = Triple2.rgbtBlue)
  END {AreRGBTriplesEqual};


  FUNCTION ColorToRGBTriple(CONST Color:  TColor):  TRGBTriple;
  BEGIN
     WITH RESULT DO
     BEGIN
       rgbtRed   := GetRValue(Color);
       rgbtGreen := GetGValue(Color);
       rgbtBlue  := GetBValue(Color)
     END
  END {ColorToRGBTriple};


  FUNCTION RGBtoRGBTriple(CONST red, green, blue:  BYTE):  TRGBTriple;
  BEGIN
    WITH RESULT DO
    BEGIN
      rgbtRed   := red;
      rgbtGreen := green;
      rgbtBlue  := blue
    END
  END {RGBTriple};


  FUNCTION RGBTripleToColor(CONST RGBTriple:  TRGBTriple):  TColor;
  BEGIN
    WITH RGBTriple DO
      RESULT := RGB(rgbtRed, rgbtGreen, rgbtBlue)
  END {RGBTripleToColor};


  FUNCTION RGBTripleInvert (CONST RGBTriple:  TRGBTriple):  TRGBTriple;
  BEGIN
    WITH RESULT DO
    BEGIN
      rgbtRed   := 255 - RGBTriple.rgbtRed;
      rgbtGreen := 255 - RGBTriple.rgbtGreen;
      rgbtBlue  := 255 - RGBTriple.rgbtBlue
    END
  END {RGBTripleInvert};


  //==  RGBTriple manipulations  =========================================

  // Calculate "average" RGBTriple given array of RGBTriples
  FUNCTION RGBTripleAverage(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
    VAR
      Count   :  INTEGER;
      i       :  INTEGER;
      SumRed  :  INTEGER;
      SumGreen:  INTEGER;
      SumBlue :  INTEGER;
  BEGIN
    SumRed   := 0;
    SumGreen := 0;
    SumBlue  := 0;
    FOR i := Low(RGB) to High(RGB) DO
    BEGIN
      INC (SumRed,   RGB[i].rgbtRed);
      INC (SumGreen, RGB[i].rgbtGreen);
      INC (SumBlue,  RGB[i].rgbtBlue)
    END;

    Count := High(RGB) - Low(RGB) + 1;

    RESULT := RGBtoRGBTriple(sumRed   DIV Count,
                             sumGreen DIV Count,
                             sumBlue  DIV Count)
  END {RGBTripleAverage};


  // Find brightest RGBTriple in an array
  FUNCTION RGBTripleBrightest(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
   VAR
     MaxIndex    :  INTEGER;
     MaxIntensity:  INTEGER;
     i           :  INTEGER;
     Intensity   :  INTEGER;
  BEGIN
    MaxIndex := Low(RGB);
    MaxIntensity := RGBTripletoIntensity(RGB[Low(RGB)]);

    FOR i := Low(RGB)+1 to High(RGB) DO
    BEGIN
      Intensity := RGBTripletoIntensity(RGB[i]);
      IF   Intensity > MaxIntensity
      THEN BEGIN
        MaxIntensity := Intensity;
        MaxIndex := i
      END
    END;

    RESULT := RGB[MaxIndex]
  END {RGBTripleBrightest};


  // Returned TRGBTriple has the max of R, G, and B components for input array
  FUNCTION RGBTripleMaximum(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
    VAR
      i       :  INTEGER;
      MaxRed  :  BYTE;
      MaxGreen:  BYTE;
      MaxBlue :  BYTE;
  BEGIN
    MaxRed   := RGB[0].rgbtRed;
    MaxGreen := RGB[0].rgbtGreen;
    MaxBlue  := RGB[0].rgbtBlue;

    FOR i := 1 to High(RGB) DO
    BEGIN
      IF   RGB[i].rgbtRed > MaxRed
      THEN MaxRed := RGB[i].rgbtRed;

      IF   RGB[i].rgbtGreen > MaxGreen
      THEN MaxGreen := RGB[i].rgbtRed;

      IF   RGB[i].rgbtBlue > MaxBlue
      THEN MaxBlue := RGB[i].rgbtBlue
    END;

    RESULT := RGBtoRGBTriple(MaxRed, MaxGreen, MaxBlue)
  END {RGBTripleMaximum};


  // Use copy of MedianInteger here instead of using StatisticsLibrary, so
  // "low-level" image processing primitives aren't dependent on
  // "high-level" statistics library.
  FUNCTION MedianInteger (x:  ARRAY OF INTEGER):  INTEGER;
    VAR
      i        :  INTEGER;
      j        :  INTEGER;
      Middle   :  INTEGER;
      Temporary:  INTEGER;
  BEGIN
    // Use truncated selection sort to find median

    Middle := (High(x)+1) DIV 2;

    FOR i := 0 TO Middle DO
    BEGIN
      FOR j := 1 TO High(x)-i DO
      BEGIN
        IF   x[j] > x[j-1]
        THEN BEGIN
          Temporary := x[j];
          x[j] := x[j-1];
          x[j-1] := Temporary
        END
      END

    END;

    IF   Odd(High(x))
    THEN BEGIN
      // When High(x) is Odd, there are an even number of elements in array.
      // Define median as average of two middle values.
      RESULT := (x[middle] + x[middle-1]) DIV 2
    END
    ELSE BEGIN
      // When High(x) is Even, there are an odd number of elements in array.
      // Median is the middle value.
      RESULT := x[middle]
    END
  END {MedianInteger};


  FUNCTION RGBTripleMedian(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
    TYPE
      pIntegerArray = ^TIntegerArray;
      TIntegerArray = ARRAY[0..MaxPixelCount-1] OF INTEGER;
    VAR
     Count      :  INTEGER;
     i          :  INTEGER;
     MedianIndex:  INTEGER;
     MedianValue:  INTEGER;
     pIntensity :  pIntegerArray;
  BEGIN
    Count := High(RGB) - Low(RGB) + 1;

    // Allocate temporary integer array for intensity values
    // Use this technique until D4 dynamic arrays can be used.
    GetMem(pIntensity, Count*SizeOf(Integer));

    TRY
      FOR i := Low(RGB) to High(RGB) DO
      BEGIN
        pIntensity^[i] := RGBTripletoIntensity(RGB[i]);
      END;

      MedianValue := MedianInteger( Slice(pIntensity^, Count) );

      MedianIndex := Low(RGB);
      WHILE (MedianIndex < High(RGB)) AND
            (pIntensity^[MedianIndex] <> MedianValue) DO
      BEGIN
        INC (MedianIndex)
      END

    FINALLY
      FreeMem(pIntensity)
    END;

    RESULT := RGB[MedianIndex]
  END {RGBTripleMedian};


  // Returned TRGBTriple has the min of R, G, and B components for input array
  FUNCTION RGBTripleMinimum(RGB:  ARRAY OF TRGBTriple):  TRGBTriple;
    VAR
      i       :  INTEGER;
      MinRed  :  BYTE;
      MinGreen:  BYTE;
      MinBlue :  BYTE;
  BEGIN
    MinRed   := RGB[0].rgbtRed;
    MinGreen := RGB[0].rgbtGreen;
    MinBlue  := RGB[0].rgbtBlue;

    FOR i := 1 to High(RGB) DO
    BEGIN
      IF   RGB[i].rgbtRed < MinRed
      THEN MinRed := RGB[i].rgbtRed;

      IF   RGB[i].rgbtGreen < MinGreen
      THEN MinGreen := RGB[i].rgbtGreen;

      IF   RGB[i].rgbtBlue < MinBlue
      THEN MinBlue := RGB[i].rgbtBlue
    END;

    RESULT := RGBtoRGBTriple(MinRed, MinGreen, MinBlue)
  END {RGBTripleMinimum};


  //======================================================================

  // See [Russ95, p. 41]
  FUNCTION RGBTripleToIntensity(CONST RGB:  TRGBTriple):  INTEGER;
  BEGIN
    WITH RGB DO
      RESULT := (rgbtRed + rgbtGreen + rgbtBlue) DIV 3
  END {RGBTripleToIntensity};


  // See [Foley96, p. 595]
  FUNCTION RGBTripleToLightness(CONST RGB:  TRGBTriple):  INTEGER;
  BEGIN
    // Use DIV here since histogram looks "odd" when IEEE rounding is used.
    WITH RGB DO
      RESULT := (MinIntValue( [rgbtRed, rgbtGreen, rgbtBlue] ) +
                 MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] ) ) DIV 2
  END {RGBTripleToLightness};


  // See [Foley96, p. 592]
  FUNCTION RGBTripleToSaturation(CONST RGB:  TRGBTriple):  INTEGER;
    VAR
      MaxValue:  INTEGER;
      MinValue:  INTEGER;
  BEGIN
     WITH RGB DO
     BEGIN
       MinValue := MinIntValue( [rgbtRed, rgbtGReen, rgbtBlue] );
       MaxValue := MaxIntValue( [rgbtRed, rgbtGReen, rgbtBlue] )
     END;

    // Calculate saturation:  saturation is 0 if r, g and b are all 0
    IF   MaxValue = 0
    THEN RESULT  := 0
    ELSE RESULT  := MulDiv(MaxValue - MinValue, 255, MaxValue)
  END {RGBTripleToSaturation};


  // See [Foley96, p. 592]
  FUNCTION RGBTripleToValue(CONST RGB:  TRGBTriple):  INTEGER;
  BEGIN
    WITH RGB DO
      RESULT := MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] )
  END {RGBTripleToValue};


  // Y (in YIQ) = 0.299R + 0.587G + 0.114B, which can be performed in
  // integer arithmetic as Y = (77R + 150G + 29B) DIV 256
  // See [Foley96, pp. 589-590]
  FUNCTION  RGBTripleToY (CONST RGB:  TRGBTriple):  INTEGER;
  BEGIN
    WITH RGB DO
      RESULT := INTEGER(77*rgbtRed + 150*rgbtGreen + 29*rgbtBlue) SHR 8
  END {RGBtoY};


  //==  CountColors  =====================================================

  // Count number of unique R-G-B triples in a pf24bit Bitmap.
  //
  // Use 2D array of TBits objects -- when (R,G) combination occurs
  // for the first time, create 256-bit array of bits in blue dimension.
  // So, overall this is a fairly sparse matrix for most pictures.
  // Tested with pictures created with a known number of colors, including
  // a specially constructed image with 1024*1024 = 1,048,576 colors.
  //
  // efg, October 1998.
  FUNCTION CountColors(CONST Bitmap:  TBitmap):  INTEGER;
    VAR
      Flags:  ARRAY[BYTE, BYTE] OF TBits;
      i    :  INTEGER;
      j    :  INTEGER;
      k    :  INTEGER;
      rowIn:  pRGBTripleArray;
  BEGIN
    // Be sure bitmap is 24-bits/pixel
    ASSERT (Bitmap.PixelFormat = pf24Bit);

    // Clear 2D array of TBits objects
    FOR j := 0 TO 255 DO
      FOR i := 0 TO 255 DO
        Flags[i,j] := NIL;

    // Step through each scanline of image
    FOR j := 0 TO Bitmap.Height-1 DO
    BEGIN
      rowIn  := Bitmap.Scanline[j];
      FOR i := 0 TO Bitmap.Width-1 DO
      BEGIN
        WITH rowIn[i] DO
        BEGIN

          IF   NOT Assigned(Flags[rgbtRed, rgbtGreen])
          THEN BEGIN
            // Create 3D column when needed
            Flags[rgbtRed, rgbtGreen] := TBits.Create;
            Flags[rgbtRed, rgbtGreen].Size := 256;
          END;

          // Mark this R-G-B triple
          Flags[rgbtRed,rgbtGreen].Bits[rgbtBlue] := TRUE
        END
      END
    END;

    RESULT := 0;
    // Count and Free TBits objects
    FOR j := 0 TO 255 DO
    BEGIN
      FOR i := 0 TO 255 DO
      BEGIN

        IF   Assigned(Flags[i,j])
        THEN BEGIN
          FOR k := 0 TO 255 DO
            IF   Flags[i,j].Bits[k]
            THEN INC(RESULT);
          Flags[i,j].Free;
        END

      END
    END

  END {CountColors};


END.
