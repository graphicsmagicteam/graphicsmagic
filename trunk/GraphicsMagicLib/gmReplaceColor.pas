{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmReplaceColor;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
  GR32, gmTypes, gmColorRange;

type
  TgmReplaceColorTool = class(TObject)
  private
    { Pointer to an external channel bitmap which could be full RGB channel
      bitmap or single channel grayscale bitmap. Use the channel bitmap to
      calculate the mask bitmap. }
    FChannelBitmap: TBitmap32;
    FMaskBitmap   : TBitmap32; // the mask based on color range calculation
    FSelectedColor: TColor32;
    FColorMode    : TgmSelectedColorMode;
    FFuzziness    : Integer;
    FSingleChannel: Boolean;   // whether used for single channel

    { color change amount }
    FHueChangeAmount       : Integer;
    FSaturationChangeAmount: Integer;
    FLuminosityChangeAmount: Integer;
    FValueChangeAmount     : Integer;
    FRedChangeAmount       : Integer;
    FGreenChangeAmount     : Integer;
    FBlueChangeAmount      : Integer;

    { color range }
    FMinHue       : Word;
    FMaxHue       : Word;
    FMinSaturation: Byte;
    FMaxSaturation: Byte;
    FMinLuminosity: Byte;
    FMaxLuminosity: Byte;
    FMinValue     : Byte;
    FMaxValue     : Byte;
    FMinRed       : Byte;
    FMaxRed       : Byte;
    FMinGreen     : Byte;
    FMaxGreen     : Byte;
    FMinBlue      : Byte;
    FMaxBlue      : Byte;

    procedure SetChannelBitmap(const ASourceBmp: TBitmap32);
    procedure GetLightMask;
    procedure GetMaskByColorMode;

    procedure ChangeLightness(const ASourceBmp, ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet);

    procedure ChangeHLS(const ASourceBmp, ADestBmp: TBitmap32);
    procedure ChangeHSV(const ASourceBmp, ADestBmp: TBitmap32);
    procedure ChangeRGB(const ASourceBmp, ADestBmp: TBitmap32);
  public
    constructor Create;
    destructor Destroy; override;

    // calculate all necessary data in one call
    procedure Calc;  

    procedure Execute(const ASourceBmp, ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet);

    property ChannelBitmap         : TBitmap32            read FChannelBitmap          write SetChannelBitmap;
    property MaskBitmap            : TBitmap32            read FMaskBitmap;
    property SelectedColor         : TColor32             read FSelectedColor          write FSelectedColor;
    property ColorMode             : TgmSelectedColorMode read FColorMode              write FColorMode;
    property Fuzziness             : Integer              read FFuzziness              write FFuzziness;
    property IsSingleChannel       : Boolean              read FSingleChannel          write FSingleChannel;
    property HueChangeAmount       : Integer              read FHueChangeAmount        write FHueChangeAmount;
    property SaturationChangeAmount: Integer              read FSaturationChangeAmount write FSaturationChangeAmount;
    property LuminosityChangeAmount: Integer              read FLuminosityChangeAmount write FLuminosityChangeAmount;
    property ValueChangeAmount     : Integer              read FValueChangeAmount      write FValueChangeAmount;
    property RedChangeAmount       : Integer              read FRedChangeAmount        write FRedChangeAmount;
    property GreenChangeAmount     : Integer              read FGreenChangeAmount      write FGreenChangeAmount;
    property BlueChangeAmount      : Integer              read FBlueChangeAmount       write FBlueChangeAmount;

    property MinHue                : Word                 read FMinHue;
    property MaxHue                : Word                 read FMaxHue;
    property MinSaturation         : Byte                 read FMinSaturation;
    property MaxSaturation         : Byte                 read FMaxSaturation;
    property MinLuminosity         : Byte                 read FMinLuminosity;
    property MaxLuminosity         : Byte                 read FMaxLuminosity;
    property MinValue              : Byte                 read FMinValue;
    property MaxValue              : Byte                 read FMaxValue;
    property MinRed                : Byte                 read FMinRed;
    property MaxRed                : Byte                 read FMaxRed;
    property MinGreen              : Byte                 read FMinGreen;
    property MaxGreen              : Byte                 read FMaxGreen;
    property MinBlue               : Byte                 read FMinBlue;
    property MaxBlue               : Byte                 read FMaxBlue;
  end;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmColorSpace;

constructor TgmReplaceColorTool.Create;
begin
  inherited Create;

  FChannelBitmap := nil;
  FMaskBitmap    := TBitmap32.Create;
  FSelectedColor := clBlack32;
  FColorMode     := scmHLS;
  FFuzziness     := 0;
  FSingleChannel := False;

  FHueChangeAmount        := 0;
  FSaturationChangeAmount := 0;
  FLuminosityChangeAmount := 0;
  FValueChangeAmount      := 0;
  FRedChangeAmount        := 0;
  FGreenChangeAmount      := 0;
  FBlueChangeAmount       := 0;
end;

destructor TgmReplaceColorTool.Destroy;
begin
  FChannelBitmap := nil;
  FMaskBitmap.Free;

  inherited Destroy;
end; 

procedure TgmReplaceColorTool.SetChannelBitmap(const ASourceBmp: TBitmap32);
begin
  if Assigned(ASourceBmp) then
  begin
    FChannelBitmap := ASourceBmp;
  end;
end; 

procedure TgmReplaceColorTool.GetLightMask;
var
  i, LLightValue, LGrayValue      : Integer;
  LLowLight, LMidLight, LHighLight: Integer;
  LSrcBits, LMaskBits             : PColor32;
begin
  if not Assigned(FChannelBitmap) then
  begin
    Exit;
  end;

  FMaskBitmap.SetSize(FChannelBitmap.Width, FChannelBitmap.Height);
  FMaskBitmap.Clear(clBlack32);

  if FFuzziness <= 0 then
  begin
    Exit;
  end;

  LLowLight  := FMinLuminosity;
  LHighLight := FMaxLuminosity;
  LMidLight  := (LLowLight + LHighLight) div 2;

  LSrcBits  := @FChannelBitmap.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  for i := 0 to (FMaskBitmap.Width * FMaskBitmap.Height - 1) do
  begin
    LLightValue := RGBToLightness32(LSrcBits^);

    if (LLightValue >= LLowLight) and
       (LLightValue <= LHighLight) then
    begin
      LGrayValue := 255 - Round( Abs(LLightValue - LMidLight) / FFuzziness * 255 );
      LMaskBits^ := Gray32(LGrayValue);
    end;

    Inc(LSrcBits);
    Inc(LMaskBits);
  end;
end;

procedure TgmReplaceColorTool.GetMaskByColorMode;
var
  i, LIntens, LFuzzIntens, LGrayValue: Integer;
  LMidIntens, LHighIntens            : Integer;
  r, g, b, H, S, V, L                : Integer;
  LRGBColor                          : TColor32;
  LSrcBits, LMaskBits                : PColor32;
begin
  if not Assigned(FChannelBitmap) then
  begin
    Exit;
  end;

  FMaskBitmap.SetSize(FChannelBitmap.Width, FChannelBitmap.Height);
  FMaskBitmap.Clear(clBlack32);

  LSrcBits  := @FChannelBitmap.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  case FColorMode of
    scmHLS:
      begin
        LMidIntens  := Intensity(FSelectedColor);
        LRGBColor   := HLSToRGB32(255, FMaxHue, FMaxLuminosity, FMaxSaturation);
        LHighIntens := Intensity(LRGBColor);
        LFuzzIntens := LHighIntens - LMidIntens;

        if LFuzzIntens <= 0 then
        begin
          Exit;
        end;

        for i := 0 to (FMaskBitmap.Width *  FMaskBitmap.Height - 1) do
        begin
          RGBToHLS32(LSrcBits^, H, L, S);

          S := Clamp(S, 1, 255);

          if (H >= FMinHue) and
             (H <= FMaxHue) and
             (L >= FMinLuminosity) and
             (L <= FMaxLuminosity) and
             (S >= FMinSaturation) and
             (S <= FMaxSaturation) then
          begin
            LIntens    := Intensity(LSrcBits^);
            LGrayValue := 255 - Round( Abs(LIntens - LMidIntens) / LFuzzIntens * 255 );
            LGrayValue := Clamp(LGrayValue, 0, 255);
            LMaskBits^ := Gray32(LGrayValue);
          end;

          Inc(LSrcBits);
          Inc(LMaskBits);
        end;
      end;

    scmHSV:
      begin
        LMidIntens  := Intensity(FSelectedColor);
        LRGBColor   := HSVToRGB32(255, FMaxHue, FMaxSaturation, FMaxValue);
        LHighIntens := Intensity(LRGBColor);
        LFuzzIntens := LHighIntens - LMidIntens;

        if LFuzzIntens <= 0 then
        begin
          Exit;
        end;

        for i := 0 to (FMaskBitmap.Width *  FMaskBitmap.Height - 1) do
        begin
          RGBToHSV32(LSrcBits^, H, S, V);

          if (H >= FMinHue) and
             (H <= FMaxHue) and
             (S >= FMinSaturation) and
             (S <= FMaxSaturation) and
             (V >= FMinValue) and
             (V <= FMaxValue) then
          begin
            LIntens    := Intensity(LSrcBits^);
            LGrayValue := 255 - Round( Abs(LIntens - LMidIntens) / LFuzzIntens * 255 );
            LGrayValue := Clamp(LGrayValue, 0, 255);
            LMaskBits^ := Gray32(LGrayValue);
          end;

          Inc(LSrcBits);
          Inc(LMaskBits);
        end;
      end;

    scmRGB:
      begin
        LMidIntens  := Intensity(FSelectedColor);
        LRGBColor   := Color32(FMaxRed, FMaxGreen, FMaxBlue);
        LHighIntens := Intensity(LRGBColor);
        LFuzzIntens := LHighIntens - LMidIntens;

        if LFuzzIntens <= 0 then
        begin
          Exit;
        end;

        for i := 0 to (FMaskBitmap.Width * FMaskBitmap.Height - 1) do
        begin
          r := LSrcBits^ shr 16 and $FF;
          g := LSrcBits^ shr  8 and $FF;
          b := LSrcBits^        and $FF;

          if (r >= FMinRed) and
             (r <= FMaxRed) and
             (g >= FMinGreen) and
             (g <= FMaxGreen) and
             (b >= FMinBlue) and
             (b <= FMaxBlue) then
          begin
            LIntens    := Intensity(LSrcBits^);
            LGrayValue := 255 - Round( Abs(LIntens - LMidIntens) / LFuzzIntens * 255 );
            LGrayValue := Clamp(LGrayValue, 0, 255);
            LMaskBits^ := Gray32(LGrayValue);
          end;

          Inc(LSrcBits);
          Inc(LMaskBits);
        end;
      end;
  end;
end; 

procedure TgmReplaceColorTool.ChangeLightness(
  const ASourceBmp, ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);
var
  r, g, b, LIntens: Byte;
  rr, gg, bb      : Byte;
  i               : Integer;
  LSrcBits        : PColor32;
  LDestBits       : PColor32;
  LMaskBits       : PColor32;
begin
  ADestBmp.Assign(ASourceBmp);

  LSrcBits  := @ASourceBmp.Bits[0];
  LDestBits := @ADestBmp.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  for i := 0 to (ASourceBmp.Width * ASourceBmp.Height - 1) do
  begin
    r := LSrcBits^ shr 16 and $FF;
    g := LSrcBits^ shr 8  and $FF;
    b := LSrcBits^        and $FF;

    if csGrayscale in AChannelSet then
    begin
      if (b >= FMinLuminosity) and
         (b <= FMaxLuminosity) then
      begin
        LIntens := LMaskBits^ and $FF;
        bb      := Clamp(b + FLuminosityChangeAmount, 0, 255);
        b       := ( bb * LIntens + b * (255 - LIntens) ) div 255;

        LDestBits^ := (LSrcBits^ and $FF000000) or (b shl 16) or (b shl 8) or b;
      end;
    end
    else
    begin
      if csRed in AChannelSet then
      begin
        if (r >= FMinLuminosity) and
           (r <= FMaxLuminosity) then
        begin
          LIntens := LMaskBits^ and $FF;
          rr      := Clamp(r + FLuminosityChangeAmount, 0, 255);
          r       := ( rr * LIntens + r * (255 - LIntens) ) div 255;
        end;
      end;

      if csGreen in AChannelSet then
      begin
        if (g >= FMinLuminosity) and
           (g <= FMaxLuminosity) then
        begin
          LIntens := LMaskBits^ and $FF;
          gg      := Clamp(g + FLuminosityChangeAmount, 0, 255);
          g       := ( gg * LIntens + g * (255 - LIntens) ) div 255;
        end;
      end;

      if csBlue in AChannelSet then
      begin
        if (b >= FMinLuminosity) and
           (b <= FMaxLuminosity) then
        begin
          LIntens := LMaskBits^ and $FF;
          bb      := Clamp(b + FLuminosityChangeAmount, 0, 255);
          b       := ( bb * LIntens + b * (255 - LIntens) ) div 255;
        end;
      end;

      LDestBits^ := (LSrcBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBits);
    Inc(LDestBits);
    Inc(LMaskBits);
  end;
end;

procedure TgmReplaceColorTool.ChangeHLS(const ASourceBmp, ADestBmp: TBitmap32);
var
  LRGBColor       : TColor32;
  a               : Cardinal;
  i, H, L, S      : Integer;
  r, g, b, LIntens: Byte;
  nr, ng, nb      : Byte;
  LSrcBits        : PColor32;
  LDestBits       : PColor32;
  LMaskBits       : PColor32;
begin
  ADestBmp.Assign(ASourceBmp);

  LSrcBits  := @ASourceBmp.Bits[0];
  LDestBits := @ADestBmp.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LSrcBits^ shr 24 and $FF;

    RGBToHLS32(LSrcBits^, H, L, S);
    S := Clamp(S, 1, 255);

    if (H >= FMinHue) and
       (H <= FMaxHue) and
       (L >= FMinLuminosity) and
       (L <= FMaxLuminosity) and
       (S >= FMinSaturation) and
       (S <= FMaxSaturation) then
    begin
      r := LSrcBits^ shr 16 and $FF;
      g := LSrcBits^ shr  8 and $FF;
      b := LSrcBits^        and $FF;

      LIntens := LMaskBits^ and $FF;

      LRGBColor := HLSToRGB32(a, Clamp(H + FHueChangeAmount, 0, 360),
                                 Clamp(L + FLuminosityChangeAmount, 0, 255),
                                 Clamp(S + FSaturationChangeAmount, 1, 255) );

      nr := LRGBColor shr 16 and $FF;
      ng := LRGBColor shr  8 and $FF;
      nb := LRGBColor        and $FF;

      r := ( nr * LIntens + r * (255 - LIntens) ) div 255;
      g := ( ng * LIntens + g * (255 - LIntens) ) div 255;
      b := ( nb * LIntens + b * (255 - LIntens) ) div 255;

      LDestBits^ := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBits);
    Inc(LDestBits);
    Inc(LMaskBits);
  end;
end;

procedure TgmReplaceColorTool.ChangeHSV(const ASourceBmp, ADestBmp: TBitmap32);
var
  LRGBColor : TColor32;
  a, r, g, b: Cardinal;
  nr, ng, nb: Cardinal;
  LIntens   : Cardinal;
  i, H, S, V: Integer;
  LSrcBits  : PColor32;
  LDestBits : PColor32;
  LMaskBits : PColor32;
begin
  ADestBmp.Assign(ASourceBmp);

  LSrcBits  := @ASourceBmp.Bits[0];
  LDestBits := @ADestBmp.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LSrcBits^ shr 24 and $FF;
    
    RGBToHSV32(LSrcBits^, H, S, V);

    if (H >= FMinHue) and
       (H <= FMaxHue) and
       (S >= FMinSaturation) and
       (S <= FMaxSaturation) and
       (V >= FMinValue) and
       (V <= FMaxValue) then
    begin
      r := LSrcBits^ shr 16 and $FF;
      g := LSrcBits^ shr  8 and $FF;
      b := LSrcBits^        and $FF;

      LIntens := LMaskBits^ and $FF;

      LRGBColor := HSVToRGB32( a, Clamp(H + FHueChangeAmount, 0, 360),
                                  Clamp(S + FSaturationChangeAmount, 0, 255),
                                  Clamp(V + FValueChangeAmount, 0, 255) );

      nr := LRGBColor shr 16 and $FF;
      ng := LRGBColor shr  8 and $FF;
      nb := LRGBColor        and $FF;

      r := ( nr * LIntens + r * (255 - LIntens) ) div 255;
      g := ( ng * LIntens + g * (255 - LIntens) ) div 255;
      b := ( nb * LIntens + b * (255 - LIntens) ) div 255;

      LDestBits^ := (a shl 24) or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBits);
    Inc(LDestBits);
    Inc(LMaskBits);
  end;
end;

procedure TgmReplaceColorTool.ChangeRGB(const ASourceBmp, ADestBmp: TBitmap32);
var
  r, g, b, LIntens: Byte;
  nr, ng, nb      : Byte;
  i               : Integer;
  LSrcBits        : PColor32;
  LDestBits       : PColor32;
  LMaskBits       : PColor32;
begin
  ADestBmp.Assign(ASourceBmp);

  LSrcBits  := @ASourceBmp.Bits[0];
  LDestBits := @ADestBmp.Bits[0];
  LMaskBits := @FMaskBitmap.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    r := LSrcBits^ shr 16 and $FF;
    g := LSrcBits^ shr 8  and $FF;
    b := LSrcBits^        and $FF;

    if (r >= FMinRed) and
       (r <= FMaxRed) and
       (G >= FMinGreen) and
       (G <= FMaxGreen) and
       (B >= FMinBlue) and
       (B <= FMaxBlue) then
    begin
      LIntens := LMaskBits^ and $FF;

      nr := Clamp(r + FRedChangeAmount,   0, 255);
      ng := Clamp(g + FGreenChangeAmount, 0, 255);
      nb := Clamp(b + FBlueChangeAmount,  0, 255);

      r := ( nr * LIntens + r * (255 - LIntens) ) div 255;
      g := ( ng * LIntens + g * (255 - LIntens) ) div 255;
      b := ( nb * LIntens + b * (255 - LIntens) ) div 255;

      LDestBits^ := (LDestBits^ and $FF000000) or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBits);
    Inc(LDestBits);
    Inc(LMaskBits);
  end;
end;

// calculate all necessary data in one call
procedure TgmReplaceColorTool.Calc;
begin
  if FSingleChannel then
  begin
    LightnessRange(FSelectedColor, FFuzziness, FMinLuminosity, FMaxLuminosity);
    GetLightMask;
  end
  else
  begin
    case FColorMode of
      scmHLS:
        begin
          HLSColorRange(FSelectedColor, FFuzziness, FMaxHue, FMinHue,
                        FMaxLuminosity, FMinLuminosity,
                        FMaxSaturation, FMinSaturation);
        end;

      scmHSV:
        begin
          HSVColorRange(FSelectedColor, FFuzziness, FMaxHue, FMinHue,
                        FMaxSaturation, FMinSaturation, FMaxValue, FMinValue);
        end;

      scmRGB:
        begin
          RGBColorRange32(FSelectedColor, FFuzziness, FMaxRed, FMinRed,
                          FMaxGreen, FMinGreen, FMaxBlue, FMinBlue);
        end;
    end;

    GetMaskByColorMode;
  end;
end;

procedure TgmReplaceColorTool.Execute(const ASourceBmp, ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
begin
  if (FMaskBitmap.Width  <> ASourceBmp.Width) or
     (FMaskBitmap.Height <> ASourceBmp.Height) then
  begin
    Exit;
  end;

  if FSingleChannel then
  begin
    ChangeLightness(ASourceBmp, ADestBmp, AChannelSet);
  end
  else
  begin
    case FColorMode of
      scmHLS:
        begin
          ChangeHLS(ASourceBmp, ADestBmp);
        end;
        
      scmHSV:
        begin
          ChangeHSV(ASourceBmp, ADestBmp);
        end;
        
      scmRGB:
        begin
          ChangeRGB(ASourceBmp, ADestBmp);
        end;
    end;
  end;
end; 

end.
