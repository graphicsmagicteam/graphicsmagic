{ This library created in 07/09/2006,
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "levels.h" and "levels.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis,
  for giving us the opportunity to know how to achieve Levels Tool.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }

unit gmLevels;

interface

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmGimpBaseEnums, gmGimpHistogram;

type
  TgmLevelsActivePicker = (lapNone, lapBlack, lapGray, lapWhite);

  TgmLevels = class(TObject)
  private
    FGamma     : array [0..4] of Double;
    FLowInput  : array [0..4] of Integer;
    FHighInput : array [0..4] of Integer;
    FLowOutput : array [0..4] of Integer;
    FHighOutput: array [0..4] of Integer;

    procedure ChannelStretch(const AHist: TgmGimpHistogram; AChannel: Integer);
    function InputFromColor(const AChannel: Integer; const AColor: TColor32): Integer;

    function GetGamma(AIndex: Integer): Double;
    function GetLowInput(AIndex: Integer): Integer;
    function GetHighInput(AIndex: Integer): Integer;
    function GetLowOutput(AIndex: Integer): Integer;
    function GetHighOutput(AIndex: Integer): Integer;

    procedure SetGamma(AIndex: Integer; AValue: Double);
    procedure SetLowInput(AIndex, AValue: Integer);
    procedure SetHighInput(AIndex, AValue: Integer);
    procedure SetLowOutput(AIndex, AValue: Integer);
    procedure SetHighOutput(AIndex, AValue: Integer);
  public
    FInput: array [0..4, 0..255] of Byte;  // this is used only by gui

    constructor Create;
    
    procedure Init;
    procedure ChannelReset(const AChannel: Integer);
    procedure Stretch(const AHist: TgmGimpHistogram; const AIsColor: Boolean);

    procedure AdjustByColors(const AChannel: Integer;
      const AActivePicker: TgmLevelsActivePicker; const AColor: TColor32);
      
    procedure CalculateTransfers;
    function LUTFunc(const NChannels, AChannel: Integer; const AValue: Extended): Extended;

    property Gamma     [index: Integer]: Double  read GetGamma      write SetGamma;
    property LowInput  [index: Integer]: Integer read GetLowInput   write SetLowInput;
    property HighInput [index: Integer]: Integer read GetHighInput  write SetHighInput;
    property LowOutput [index: Integer]: Integer read GetLowOutput  write SetLowOutput;
    property HighOutput[index: Integer]: Integer read GetHighOutput write SetHighOutput;
  end;

implementation

uses
  Math, gmGimpCommonFuncs, gmGimpRGB;

constructor TgmLevels.Create;
begin
  inherited Create;
  Init;
end;

function TgmLevels.GetGamma(AIndex: Integer): Double;
begin
  if AIndex in [0..4] then
  begin
    Result := FGamma[AIndex];
  end
  else
  begin
    Result := 1.0;
  end;
end;

function TgmLevels.GetLowInput(AIndex: Integer): Integer;
begin
  if AIndex in [0..4] then
  begin
    Result := FLowInput[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmLevels.GetHighInput(AIndex: Integer): Integer;
begin
  if AIndex in [0..4] then
  begin
    Result := FHighInput[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmLevels.GetLowOutput(AIndex: Integer): Integer;
begin
  if AIndex in [0..4] then
  begin
    Result := FLowOutput[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end;

function TgmLevels.GetHighOutput(AIndex: Integer): Integer;
begin
  if AIndex in [0..4] then
  begin
    Result := FHighOutput[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

procedure TgmLevels.SetGamma(AIndex: Integer; AValue: Double);
begin
  if AIndex in [0..4] then
  begin
    if FGamma[AIndex] <> AValue then
    begin
      FGamma[AIndex] := AValue;
    end;
  end;
end; 

procedure TgmLevels.SetLowInput(AIndex, AValue: Integer);
begin
  if AIndex in [0..4] then
  begin
    if FLowInput[AIndex] <> AValue then
    begin
      FLowInput[AIndex] := AValue;
    end;
  end;
end; 

procedure TgmLevels.SetHighInput(AIndex, AValue: Integer);
begin
  if AIndex in [0..4] then
  begin
    if FHighInput[AIndex] <> AValue then
    begin
      FHighInput[AIndex] := AValue;
    end;
  end;
end;

procedure TgmLevels.SetLowOutput(AIndex, AValue: Integer);
begin
  if AIndex in [0..4] then
  begin
    if FLowOutput[AIndex] <> AValue then
    begin
      FLowOutput[AIndex] := AValue;
    end;
  end;
end;

procedure TgmLevels.SetHighOutput(AIndex, AValue: Integer);
begin
  if AIndex in [0..4] then
  begin
    if FHighOutput[AIndex] <> AValue then
    begin
      FHighOutput[AIndex] := AValue;
    end;
  end;
end; 

procedure TgmLevels.Init;
var
  LChannel: Integer;
begin
  for LChannel := GIMP_HISTOGRAM_VALUE to GIMP_HISTOGRAM_ALPHA do
  begin
    ChannelReset(LChannel);
  end;
end;

procedure TgmLevels.ChannelReset(const AChannel: Integer);
begin
  FGamma[AChannel]      := 1.0;
  FLowInput[AChannel]   := 0;
  FHighInput[AChannel]  := 255;
  FLowOutput[AChannel]  := 0;
  FHighOutput[AChannel] := 255;
end;

procedure TgmLevels.Stretch(const AHist: TgmGimpHistogram;
  const AIsColor: Boolean);
var
  LChannel: Integer;
begin
  if AHist = nil then
  begin
    Exit;
  end;

  if AIsColor then
  begin
    // Set the overall value to defaults
    ChannelReset(GIMP_HISTOGRAM_VALUE);

    for LChannel := GIMP_HISTOGRAM_RED to GIMP_HISTOGRAM_BLUE do
    begin
      ChannelStretch(AHist, LChannel);
    end;
  end
  else
  begin
    ChannelStretch(AHist, GIMP_HISTOGRAM_VALUE);
  end;
end;

procedure TgmLevels.ChannelStretch(const AHist: TgmGimpHistogram;
  AChannel: Integer);
var
  i                           : Integer;
  LCount, LNewCount           : Double;
  LPercentage, LNextPercentage: Double;
begin
  if AHist = nil then
  begin
    Exit;
  end;

  FGamma[AChannel]      := 1.0;
  FLowOutput[AChannel]  := 0;
  FHighOutput[AChannel] := 255;

  LCount := AHist.gimp_histogram_get_count(AChannel, 0, 255);

  if LCount = 0.0 then
  begin
    FLowInput[AChannel]  := 0;
    FHighInput[AChannel] := 0;
  end
  else
  begin
    // Set the low input
    LNewCount := 0.0;
    
    for i := 0 to 254 do
    begin
      LNewCount       := LNewCount + AHist.gimp_histogram_get_value(AChannel, i);
      LPercentage     := LNewCount / LCount;
      LNextPercentage := ( LNewCount + AHist.gimp_histogram_get_value(AChannel, i + 1) ) / LCount;

      if Abs(LPercentage - 0.006) < Abs(LNextPercentage - 0.006) then
      begin
	      FLowInput[AChannel] := i + 1;
        Break;
      end;
    end;

    // Set the high input
    LNewCount := 0.0;
    
    for i := 255 downto 1 do
    begin
      LNewCount       := LNewCount + AHist.gimp_histogram_get_value(AChannel, i);
      LPercentage     := LNewCount / LCount;
      LNextPercentage := ( LNewCount + AHist.gimp_histogram_get_value(AChannel, i - 1) ) / LCount;

      if Abs(LPercentage - 0.006) < Abs(LNextPercentage - 0.006) then
      begin
       	FHighInput[AChannel] := i - 1;
        Break;
      end;
    end;
  end;
end;

function TgmLevels.InputFromColor(const AChannel: Integer;
  const AColor: TColor32): Integer;
var
  a, r, g, b: Integer;
begin
  Result := 0;
  
  a := AColor shr 24 and $FF;
  r := AColor shr 16 and $FF;
  g := AColor shr  8 and $FF;
  b := AColor        and $FF;

  case AChannel of
    GIMP_HISTOGRAM_VALUE:
      begin
        Result := Max( Max(r, g), b );
      end;
      
    GIMP_HISTOGRAM_RED:
      begin
        Result := r;
      end;

    GIMP_HISTOGRAM_GREEN:
      begin
        Result := g;
      end;
      
    GIMP_HISTOGRAM_BLUE:
      begin
        Result := b;
      end;
      
    GIMP_HISTOGRAM_ALPHA:
      begin
        Result := a;
      end;

    GIMP_HISTOGRAM_RGB:
      begin
        Result := Min( Min(r, g), b );
      end;
  end;
end;

procedure TgmLevels.AdjustByColors(const AChannel: Integer;
  const AActivePicker: TgmLevelsActivePicker; const AColor: TColor32);
var
  LInput, LRange     : Integer;
  LInten, LOutLight  : Double;
  LLightness, r, g, b: Byte;
begin
  case AActivePicker of
    lapBlack:
      begin
        FLowInput[AChannel] := InputFromColor(AChannel, AColor);
      end;

    lapWhite:
      begin
        FHighInput[AChannel] := InputFromColor(AChannel, AColor);
      end;
      
    lapGray:
      begin
        r := AColor shr 16 and $FF;
        g := AColor shr  8 and $FF;
        b := AColor        and $FF;

        // calculate lightness value
        LLightness := CLAMP( Gimp_RGB_Intensity(r, g, b), 0, 255 );

        LInput := InputFromColor(AChannel, AColor);

        LRange := FHighInput[AChannel] - FLowInput[AChannel];

        if LRange <= 0 then
        begin
          Exit;
        end;

        LInput := LInput - FLowInput[AChannel];

        if LInput < 0 then
        begin
          Exit;
        end;

        // normalize input and lightness
        LInten    := LInput / LRange;
        LOutLight := LLightness / LRange;

        if LOutLight <= 0 then
        begin
          Exit;
        end;
        
        if LInten <= 0 then
        begin
          Exit;
        end;

        if Ln(LOutLight) = 0 then
        begin
          Exit;
        end;

        // map selected color to corresponding lightness
        FGamma[AChannel] := Ln(LInten) / Ln(LOutLight);
      end;
  end;
end;

procedure TgmLevels.CalculateTransfers;
var
  LInten, LTmp: Extended;
  i, j        : Integer;
begin
  //  Recalculate the levels arrays
  for j := 0 to 4 do
  begin
    for i := 0 to 255 do
    begin
      // determine input intensity
      if FHighInput[j] <> FLowInput[j] then
      begin
        LInten := (i - FLowInput[j]) / (FHighInput[j] - FLowInput[j]);
      end
      else
      begin
        LInten := i - FLowInput[j];
      end;

      LInten := CLAMP(LInten, 0.0, 1.0);

      if LInten > 0.0 then
      begin
        if FGamma[j] <> 0.0 then
        begin
          LInten := Power(LInten, 1.0 / FGamma[j]);
        end;
      end;

      LTmp := LInten * 255.0  + 0.5;

      if LTmp < 0.0 then
      begin
        LTmp := 0.0;
      end
      else if LTmp > 255.0 then
      begin
        LTmp := 255.0;
      end;

      FInput[j, i] := Round(LTmp);
    end;
  end;
end;

function TgmLevels.LUTFunc(const NChannels, AChannel: Integer;
  const AValue: Extended): Extended;
var
  LInten: Extended;
  j     : Integer;
begin
  if NChannels <= 2 then
  begin
    j := AChannel;
  end
  else
  begin
    j := AChannel + 1;
  end;

  LInten := AValue;

  { For RGB and RGBA images this runs through the loop with j = channel + 1
    the first time and j = 0 the second time

    For GRAY images this runs through the loop with j = 0 the first and
    only time
   }
   
  while j >= 0 do
  begin
    // don't apply the overall curve to the alpha channel
    if (j = 0) and
       ( (NChannels = 2) or (NChannels = 4) ) and
       (AChannel = NChannels - 1) then
    begin
      Result := LInten;
      Exit;
    end;

    // determine input intensity
    if FHighInput[j] <> FLowInput[j] then
    begin
      LInten := (255.0 * LInten - FLowInput[j]) / (FHighInput[j] - FLowInput[j]);
    end
    else
    begin
      LInten := 255.0 * LInten - FLowInput[j];
    end;

    if FGamma[j] <> 0.0 then
    begin
      if LInten > 0.0 then
      begin
        LInten := Power(LInten, 1.0 / FGamma[j]);
      end
      else if LInten < 0.0 then
      begin
        LInten := -Power(-LInten, 1.0 / FGamma[j]);
      end;
    end;

    // determine the output intensity
    if FHighOutput[j] >= FLowOutput[j] then
    begin
      LInten := LInten * (FHighOutput[j] - FLowOutput[j]) + FLowOutput[j];
    end
    else
    if FHighOutput[j] < FLowOutput[j] then
    begin
      LInten := FLowOutput[j] - LInten * (FLowOutput[j] - FHighOutput[j]);
    end;

    LInten := LInten / 255.0;
    j      := j - (AChannel + 1);
  end;

  Result := LInten;
end; 

end.
