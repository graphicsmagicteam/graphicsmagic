{ Histogram and Image Statistics Library
  Assumes shades-of-gray images but stored in pf24bit bitmap to avoid
  working with palettes.

  Earl F. Glynn, April 1998.  Modified November 1998.
}

{ We adapted some routines for make them supporting GR32.

                       -- Ma Xiaoguang and Ma Xiaoming }

unit HistogramLibrary;

{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows,    // TRGBTriple
  Graphics,   // TCanvas
  GR32;

type
  THistoArray  = array [Byte] of Integer;

  // RGB color space V from HSV color space Intensity "plane"
  TColorPlane = (cpRed,
                 cpGreen,
                 cpBlue,
                 cpValue,
                 cpIntensity,
                 cpCyan,
                 cpMagenta,
                 cpYellow);

  TRGBHistoArray = record
    Red      : THistoArray;
    Green    : THistoArray;
    Blue     : THistoArray;
    Intensity: THistoArray;
  end;

  THistogram = class(TObject)
  private
    FHistogram: THistoArray;

    function GetCount: Integer;
  public
    constructor Create;

    procedure Clear;
    procedure Draw(const Canvas: TCanvas);
    procedure Increment(const Index: Byte);
    function GetPercentileLevel(const Percentile: Double): Byte;

    procedure GetStatistics(var n: Integer; var Minimum, Maximum: Byte;
      var Mode, Median: Byte; var Mean, StandardDeviation: Double;
      var Skewness, Kurtosis: Double);

    property Frequency: THistoArray read FHistogram write FHistogram;
    property Count    : Integer     read GetCount;
  end;

  procedure GetHistogram(const ColorPlane: TColorPlane;
    const Bitmap: TBitmap32; var Histogram: THistogram);

implementation

uses
  Math,                       // MaxIntValue, IntPwr
  SysUtils;                   // pByteArray, Exception

const
  MaxPixelCount = 65536;
  NaN           = 0.0/0.0;   // Tricky way to define NaN

type
  EHistogramError  = class(Exception);
  EStatisticsError = class(Exception);

  // For pf24bit Scanlines
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..MaxPixelCount - 1] of TRGBTriple;


{ THistogram }

// This Histogram class is specialized for image processing applications.
// The frequency distribution is assumed to be for values ONLY in the
// range 0..255.

function THistogram.GetCount: Integer;
var
  i: Byte;
begin
  Result := 0;

  for i := Low(THistoArray) to High(THistoArray) do
  begin
    Inc(Result, FHistogram[i]);
  end;
end;

constructor THistogram.Create;
begin
  Clear;
end;

procedure THistogram.Clear;
var
  i: Byte;
begin
  for i := Low(THistoArray) to High(THistoArray) do
  begin
    FHistogram[i] := 0;
  end;
end;

function THistogram.GetPercentileLevel(const Percentile: Double): Byte;
var
  Continue                  : Boolean;
  CumulativeCount, n        : Integer;
  CumulativeFraction, Target: Double;
  Level                     : Byte;
begin
  Target          := Percentile / 100.0;  // Target is fraction, not percentage
  n               := GetCount;
  Level           := Low(THistoArray);
  CumulativeCount := 0;

  Continue := True;

  while Continue do
  begin
    CumulativeCount    := CumulativeCount + FHistogram[Level];
    CumulativeFraction := CumulativeCount / n;
    Continue           := (CumulativeFraction < Target) and ( Level < High(THistoArray) - 1 );

    if Continue then
    begin
      Inc(Level);
    end;
  end;

  Result := Level
end;

procedure THistogram.GetStatistics(var n: Integer; var Minimum, Maximum: Byte;
  var Mode, Median: Byte; var Mean, StandardDeviation: Double;
  var Skewness, Kurtosis: Double);
var
  i                       : Byte;
  Cumulative, MaxFrequency: Integer;
  m2, m3, m3Sum, m4, m4Sum: Extended;
  x, xSum, xSqrSum        : Extended;  // Use floats to avoid integer overflow
begin
  n       := 0;
  Minimum := 0;

  while (FHistogram[Minimum] = 0) and (Minimum < 255) do
  begin
    Inc(Minimum);
  end;

  Maximum := 255;

  while (FHistogram[Maximum] = 0) and (Maximum > 0) do
  begin
    Dec(Maximum);
  end;

  // Mode is value with highest frequency.
  // For now, don't worry about a "tie".
  Mode         := Minimum;
  MaxFrequency := FHistogram[Minimum];

  for i := Minimum {+1} to Maximum do    // Not clear why "+1" here causes
  begin                                  // runtime problem
    if FHistogram[i] > MaxFrequency then
    begin
      Mode         := i;
      MaxFrequency := FHistogram[i];
    end;
  end;

  // Calculate Mean and Standard Deviation
  xSum    := 0.0;
  xSqrSum := 0.0;
  
  for i := Minimum to Maximum do
  begin
    Inc(n, FHistogram[i]);
    
    x       := i;
    xSum    := xSum    + FHistogram[i] * x;
    xSqrSum := xSqrSum + FHistogram[i] * Sqr(x);
  end;

  if n = 0 then
  begin
    Mean := NaN;
  end
  else
  begin
    Mean := xSum / n;
  end;

  if n < 2 then
  begin
    StandardDeviation := NaN;
    Skewness          := 0.0;
    Kurtosis          := 0.0;
  end
  else
  begin
    StandardDeviation := Sqrt(  ( xSqrSum - n * Sqr(Mean) ) / (n - 1)  );

    // Standard Deviation is related to moment M2
    m2 := Sqr(StandardDeviation) * (n - 1) / n;
    
    // Calculate third and fourth moments
    m3Sum := 0.0;
    m4Sum := 0.0;
    
    for i := Minimum to Maximum do
    begin
      x     := i;
      m3Sum := m3Sum + FHistogram[i] * IntPower(x - Mean, 3);
      m4Sum := m4Sum + FHistogram[i] * IntPower(x - Mean, 4);
    end;

    m3 := m3Sum / n;
    m4 := m4Sum / n;

    if m2 = 0.0 then
    begin
      Skewness := NaN;
      Kurtosis := 0.0
    end
    else
    begin
      Skewness := m3 / Power(m2, 1.5);
      Kurtosis := m4 / Sqr(m2)
    end;
  end;

  // Median is value with half of values above and below.
  Cumulative := 0;
  i          := Minimum;
  
  while (Cumulative < n div 2) and (i < 255) do
  begin
    Inc(Cumulative, FHistogram[i]);

    if Cumulative < n div 2 then    // fix for when all 0s
    begin
      Inc(i);
    end;
  end;
  
  Median := i;
end;

procedure THistogram.Increment(const Index: Byte);
begin
  Inc(FHistogram[Index])
end;

// Draw Histogram on specified Canvas, assumed to have
// a width that is a multiple of 256 (for best results).
procedure THistogram.Draw(const Canvas: TCanvas);
const
  MajorTickSize = 8;
var
  BarLength, Delta, i, j, Index: Integer;
  Width, Height, MaxValue      : Integer;
  Color                        : TColor;
  Factor                       : Double;
begin
{$RANGECHECKS OFF}
  Height   := Canvas.ClipRect.Bottom;
  Width    := Canvas.ClipRect.Right;
  MaxValue := MaxIntValue(Self.Frequency);
  Factor   := Width / 256;

  // For now only paint on a canvas exactly 256 pixels wide.  If
  // MaxValue is zero, array was not filled in correctly and is ignored.
  if MaxValue > 0 then
  begin
    for i := 0 to Width - 1 do
    begin
      // In general, Width should be multiple of 255 + 1
      Index            := Round(i / Factor);
      Index            := MinIntValue( [255, Index] );
      Color            := RGB(Index, Index, Index);
      Canvas.Pen.Color := Color;
      BarLength        := Round(Height * Self.Frequency[Index] / MaxValue);

      Canvas.MoveTo(i, Height - 1);
      Canvas.LineTo(i, Height - 1 - BarLength)
    end;

    Canvas.Pen.Color := clRed;
    
    // Vertical Lines for visual estimation
    for i := 0 to 25 do
    begin
      Canvas.MoveTo( Round(10 * i * Factor), Height - 1 );

      if i mod 5 = 0 then
      begin
        Delta := MajorTickSize;
      end
      else
      begin
        Delta := MajorTickSize div 2;
      end;

      Canvas.LineTo( Round(10 * i * Factor), Height - 1 - Delta);
    end;
    
    // Horizontal Lines
    for j := 1 to 4 do
    begin
      Canvas.MoveTo(        0, j * Height div 5);
      Canvas.LineTo(Width - 1, j * Height div 5);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color  := clRed;
    
    Canvas.TextOut( 2, 2, 'Max = ' + IntToStr(MaxValue) );
    Canvas.TextOut( 2, Height - Canvas.TextHeight('X') - MajorTickSize, '0' );
    
    Canvas.TextOut( Width - Canvas.TextWidth('250 '),
                    Height - Canvas.TextHeight('X') - MajorTickSize, '250' );
  end;
{$RANGECHECKS ON}
end; 

procedure GetHistogram(const ColorPlane: TColorPlane; const Bitmap: TBitmap32;
  var Histogram: THistogram);
var
  i, Index  : Integer;
  a, r, g, b: Byte;
  p         : PColor32;
begin
  Histogram.Clear;

  p := @Bitmap.Bits[0];
  
  for i := 0 to Bitmap.Width * Bitmap.Height - 1 do
  begin
    a := p^ shr 24 and $FF;

    if a > 0 then
    begin
      r := p^ shr 16 and $FF;
      g := p^ shr  8 and $FF;
      b := p^        and $FF;

      case ColorPlane of
        // RGB Color Space
        cpRed:
          begin
            Index := r;
          end;

        cpGreen:
          begin
            Index := g;
          end;

        cpBlue:
          begin
            Index := b;
          end;

        // CMY Color Space
        cpCyan:
          begin
            Index := 255 - r;
          end;

        cpMagenta:
          begin
            Index := 255 - g;
          end;

        cpYellow:
          begin
            Index := 255 - b;
          end;

        // V from HSV Color Space
        cpValue:
          begin
            Index := MaxIntValue([r, g, b]);
          end;

        cpIntensity:
          begin
            Index := (r + g + b) div 3;
          end;

      else
        Index := 0;  // should never happen; avoid compiler warning
      end;

      Histogram.Increment(Index)
    end;

    Inc(p);
  end;
end;

end.
