//  IEEE-754 Support for NANs and INFs.
//  Copyright (C) 1997-1998, Earl F. Glynn, Overland Park, KS  USA.
//  All Rights Reserved.
//
//  Information about IEEE Arithmetic was taken from
//  "Numerical Computation Guide," Sun Microsystems, 1990.

UNIT IEEE754;

INTERFACE

{$J-}  // Do not allow constants to be changed

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}


{$IFDEF VER120}   // Delphi 4
  CONST
    NANSignalingBits:  Int64 = $7FF0000000000001;
  VAR
    // With $J- directive above and Absolute reference below, this
    // value cannot be changed
    NANSignaling    :  DOUBLE ABSOLUTE NANSignalingBits;
{$ELSE}           // Delphi 1-3
  CONST
    NANSignalingBits:  ARRAY[1..2] OF LongInt = ($00000001, $7FF00000);
  VAR
    NANSignaling    :  DOUBLE ABSOLUTE NANSignalingBits;
{$ENDIF}

  FUNCTION NAN:  DOUBLE;                // The "Quiet" Nan

  FUNCTION PositiveInfinity:  DOUBLE;   //  INF
  FUNCTION NegativeInfinity:  DOUBLE;   // -INF

  // "Is" functions
  FUNCTION IsNAN(CONST d:  DOUBLE):  BOOLEAN;
  FUNCTION IsInfinity(CONST d:  DOUBLE): BOOLEAN;

  // Hex String Conversions
  FUNCTION DoubleToHex(CONST d:  DOUBLE):  STRING;
  FUNCTION HexToDouble(CONST hex:  STRING):  DOUBLE;


IMPLEMENTATION

  USES
    Windows,     // DWORD
    SysUtils;    // IntToHex

  TYPE
    EIEEEMath = class(Exception);

{$IFDEF VER120}   // Delphi 4
  // With Int64s, the logical order of the floating point values isn't
  // obfuscated by the "little endian" physical order
  CONST
    NANQuietBits         :  Int64 = $7FFFFFFFFFFFFFFF;
    PositiveInfinityBits :  Int64 = $7FF0000000000000;
    NegativeInfinityBits :  Int64 = $FFF0000000000000;

  VAR
    dNANQuiet             :  DOUBLE ABSOLUTE NANQuietBits;
    dPositiveInfinity     :  DOUBLE ABSOLUTE PositiveInfinityBits;
    dNegativeInfinity     :  DOUBLE ABSOLUTE NegativeInfinityBits;

  // Since a NAN is not a single, unique value, a special function is needed
  // for this test
  FUNCTION IsNAN(CONST d:  DOUBLE):  BOOLEAN;
    VAR
      Overlay:  Int64 ABSOLUTE d;
  BEGIN
    RESULT := ((Overlay AND $7FF0000000000000) =  $7FF0000000000000) AND
              ((Overlay AND $000FFFFFFFFFFFFF) <> $0000000000000000)
  END {IsNAN};


  FUNCTION IsInfinity(CONST d:  DOUBLE):  BOOLEAN;
    VAR
      Overlay:  Int64 ABSOLUTE d;
  BEGIN
    RESULT := (Overlay AND $7FF0000000000000) = $7FF0000000000000
  END {IsInfinity};

//  The following is a tempting "improvement" in Delphi 4, BUT
//  there no longer is zero padding on the left.  So stick with old
//  way for now.
//  FUNCTION DoubleToHex(CONST d:  DOUBLE):  STRING;
//    VAR
//      Overlay:  Int64 ABSOLUTE d;
//  BEGIN
//    RESULT := IntToHex(Overlay, 16)
//  END {DoubleToHex};


//  The following is a tempting "improvement" in Delphi 4, BUT
//  the constant for -0, $8000000000000000 generates an error.
//
//  FUNCTION HexToDouble(CONST hex:  STRING):  DOUBLE;
//    VAR
//      i:  Int64;
//      d:  Double absolute i;
//  BEGIN
//    i := StrToInt('$' + hex);
//    RESULT := d;
//  END {HexToDouble};

{$ELSE}           // Delphi 1-3

  // The order of integers is "reversed" because of "Little Endian" order.
  // With "Little Endian," the 2nd array element in physical order is the
  // 1st array element in "logical" order.
  CONST    // Use DWORD here for D3-D5 compatibility
    NANQuietBits         :  ARRAY[1..2] OF DWORD = ($FFFFFFFF, $7FFFFFFF);
    PositiveInfinityBits :  ARRAY[1..2] OF DWORD = ($00000000, $7FF00000);
    NegativeInfinityBits :  ARRAY[1..2] OF DWORD = ($00000000, $FFF00000);

  VAR
    // NEVER MODIFY THESE VARIABLES
    // "Absolute" cannot be used in CONST defintions.

    dNANQuiet             :  DOUBLE ABSOLUTE NANQuietBits;
    dPositiveInfinity     :  DOUBLE ABSOLUTE PositiveInfinityBits;
    dNegativeInfinity     :  DOUBLE ABSOLUTE NegativeInfinityBits;


  // Since a NAN is not a single, unique value, a special function is needed
  // for this test
  FUNCTION IsNAN(CONST d:  DOUBLE):  BOOLEAN;
    VAR
      Overlay:  ARRAY[1..4] OF WORD ABSOLUTE d;
  BEGIN
    RESULT := ((Overlay[4] AND $7FF0) = $7FF0) AND
              ((Overlay[1] <> 0) OR (Overlay[2] <> 0) OR (Overlay[3] <> 0))
  END {IsNAN};


  FUNCTION IsInfinity(CONST d:  DOUBLE):  BOOLEAN;
    VAR
      Overlay:  ARRAY[1..2] OF LongInt ABSOLUTE d;
  BEGIN
    RESULT := ( (Overlay[2] AND $7FF00000) = $7FF00000) AND
                (Overlay[1] = 0 )
  END {IsInfinity};

{$ENDIF}

  FUNCTION DoubleToHex(CONST d:  DOUBLE):  STRING;
    VAR
      Overlay:  ARRAY[1..2] OF LongInt ABSOLUTE d;
  BEGIN
    // Look at element 2 before element 1 because of "Little Endian" order.
    RESULT := IntToHex(Overlay[2],8) + IntToHex(Overlay[1],8);
  END {DoubleToHex};


  FUNCTION HexToDouble(CONST hex:  STRING):  DOUBLE;
    VAR
      d      :  DOUBLE;
      Overlay:  ARRAY[1..2] OF LongInt ABSOLUTE d;
  BEGIN
    IF   LENGTH(hex) <> 16
    THEN raise EIEEEMath.Create('Invalid hex string for HexToDouble');

    Overlay[1] := StrToInt('$' + COPY(hex, 9, 8));
    Overlay[2] := StrToInt('$' + Copy(hex, 1, 8));

    RESULT := d
  END {HexToDouble};


  // Use functions to make sure values can never be changed.
  FUNCTION NAN:  DOUBLE;
  BEGIN
    RESULT := dNANQuiet
  END {NAN};


  FUNCTION PositiveInfinity:  DOUBLE;
  BEGIN
    RESULT := dPositiveInfinity
  END {NAN};


  FUNCTION NegativeInfinity:  DOUBLE;
  BEGIN
    RESULT := dNegativeInfinity
  END {NAN};


END.
