{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmFill;

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

{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Windows, Graphics,
{ Graphics32 }
  GR32,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmTypes, gmSelection;

type
  TgmFillOptions = (gmfoNone = -1,
                    gmfoForeground,
                    gmfoBackground,
                    gmfoPattern,
                    gmfoHistory,
                    gmfoBlack,
                    gmfoSemiGray,
                    gmfoWhite);

  TgmFillMode = (gmfmNone = -1,
                 gmfmColorFill,
                 gmfmBitmapFill);

  TgmFill = class(TObject)
  private
    FForeground          : TBitmap32;
    FFillColor           : TColor32;
    FOpacity             : Integer;
    FFillMode            : TgmFillMode;
    FFillOptions         : TgmFillOptions;
    FBlendMode           : TBlendMode32;
    FSelectedChannelSet  : TgmChannelSet;
    FPreserveTransparency: Boolean;

    procedure FillWithColor32(const Dest: TBitmap32);
    procedure FillWithBitmap32(const Dest: TBitmap32);
    procedure SetFillMode32(const FillMode: TgmFillMode);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetForegroundWithPattern(const Width, Height: Integer; const APatternBmp: TBitmap32);
    procedure SetForegroundWithHistory(const Source: TBitmap32);
    procedure DoFill32(const Dest: TBitmap32);

    property ForegroundBmp         : TBitmap32      read FForeground;
    property FillColor             : TColor32       read FFillColor            write FFillColor;
    property Opacity               : Integer        read FOpacity              write FOpacity;
    property FillMode              : TgmFillMode    read FFillMode             write SetFillMode32;
    property FillOptions           : TgmFillOptions read FFillOptions          write FFillOptions;
    property BlendMode             : TBlendMode32   read FBlendMode            write FBlendMode;
    property SelectedChannelSet    : TgmChannelSet  read FSelectedChannelSet   write FSelectedChannelSet;
    property IsPreserveTransparency: Boolean        read FPreserveTransparency write FPreserveTransparency;
  end;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic Lib }
  gmAlphaFuncs, gmBlendModes, gmImageProcessFuncs, gmPaintFuncs;

constructor TgmFill.Create;
begin
  inherited Create;

  FForeground           := TBitmap32.Create;
  FFillColor            := clBlack32;
  FOpacity              := 100;
  FFillMode             := gmfmColorFill;
  FFillOptions          := gmfoForeground;
  FBlendMode            := bbmNormal32;
  FSelectedChannelSet   := [];
  FPreserveTransparency := False;
end;

destructor TgmFill.Destroy;
begin
  FForeground.Canvas.Brush.Bitmap := nil;
  FForeground.Free;
  
  inherited Destroy;
end;

procedure TgmFill.SetForegroundWithPattern(const Width, Height: Integer;
  const APatternBmp: TBitmap32);
begin
  FForeground.SetSize(Width, Height);
  FForeground.Clear($00000000);
  DrawPattern(FForeground, APatternBmp);
end;

procedure TgmFill.SetForegroundWithHistory(const Source: TBitmap32);
begin
  FForeground.Assign(Source);
end;

procedure TgmFill.SetFillMode32(const FillMode: TgmFillMode);
begin
  if FFillMode <> FillMode then
  begin
    FFillMode := FillMode;
  end;
end;

procedure TgmFill.FillWithColor32(const Dest: TBitmap32);
var
  i, LRGBChannelCount: Integer;
  LWeight            : Byte;
  a, r, g, b         : Cardinal;
  LMixedColor        : TColor32;
  LDestBit           : PColor32;
begin
  LRGBChannelCount := 0;

  if csRed in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  LWeight  := MulDiv(255, FOpacity, 100);
  LDestBit := @Dest.Bits[0];

  for i := 0 to (Dest.Width * Dest.Height - 1) do
  begin
    a := LDestBit^ and $FF000000;

    if a > 0 then
    begin
      // combine
      if csGrayscale in FSelectedChannelSet then
      begin
        LMixedColor := RGBBlendByMode(FFillColor, LDestBit^, LWeight, FBlendMode);
        b           := Intensity(LMixedColor);
        LDestBit^   := (LDestBit^ and $FF000000) or (b shl 16) or (b shl 8) or b;
      end
      else
      begin
        // blend color with blend mode
        if LRGBChannelCount = 3 then
        begin
          LMixedColor := ARGBBlendByMode(FFillColor, LDestBit^, LWeight, FBlendMode);
        end
        else
        begin
          LMixedColor := RGBBlendByMode(FFillColor, LDestBit^, LWeight, FBlendMode);
        end;
        
        r := LDestBit^ and $FF0000;
        g := LDestBit^ and $FF00;
        b := LDestBit^ and $FF;

        if csRed in FSelectedChannelSet then
        begin
          r := LMixedColor and $FF0000;
        end;

        if csGreen in FSelectedChannelSet then
        begin
          g := LMixedColor and $FF00;
        end;

        if csBlue in FSelectedChannelSet then
        begin
          b := LMixedColor and $FF;
        end;

        if (FPreserveTransparency) or (LRGBChannelCount < 3) then
        begin
          LDestBit^ := a or r or g or b;
        end
        else
        begin
          LDestBit^ := (LMixedColor and $FF000000) or r or g or b;
        end;
      end;
    end
    else
    begin
      if not FPreserveTransparency then
      begin
        if (csRed   in FSelectedChannelSet) and
           (csGreen in FSelectedChannelSet) and
           (csBlue  in FSelectedChannelSet) then
        begin
          LDestBit^ := (LWeight shl 24) or (FFillColor and $FFFFFF);
        end;
      end;
    end;

    Inc(LDestBit);
  end;
end;

procedure TgmFill.FillWithBitmap32(const Dest: TBitmap32);
var
  i, LRGBChannelCount          : Integer;
  LWeight, fa, LAdjustedOpacity: Byte;
  LMixedColor                  : TColor32;
  a, r, g, b                   : Cardinal;
  LForeBit, LDestBit           : PColor32;
begin
  if (Dest.Width  <> FForeground.Width) or
     (Dest.Height <> FForeground.Height) then
  begin
    Exit;
  end;

  LRGBChannelCount := 0;

  if csRed in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csGreen in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  if csBlue in FSelectedChannelSet then
  begin
    Inc(LRGBChannelCount);
  end;

  LWeight  := MulDiv(255, FOpacity, 100);
  LForeBit := @FForeground.Bits[0];
  LDestBit := @Dest.Bits[0];

  for i := 0 to (Dest.Width * Dest.Height - 1) do
  begin
    fa := LForeBit^ shr 24 and $FF;

    // do nothing if the foreground is fully transparent
    if fa > 0 then
    begin
      a := LDestBit^ and $FF000000;

      if a > 0 then
      begin
        // combine
        if csGrayscale in FSelectedChannelSet then
        begin
          { If the foreground has semi-transparent pixels, we need to adjust the
            blending opacity. }

          if fa < 255 then
          begin
            LAdjustedOpacity := fa * LWeight div 255;
          end
          else
          begin
            LAdjustedOpacity := LWeight;
          end;

          LMixedColor := RGBBlendByMode(LForeBit^, LDestBit^, LAdjustedOpacity, FBlendMode);
          b           := Intensity(LMixedColor);
          LDestBit^   := a or (b shl 16) or (b shl 8) or b;
        end
        else
        begin
          // blend foreground and dest with blend mode
          if LRGBChannelCount = 3 then
          begin
            LMixedColor := ARGBBlendByMode(LForeBit^, LDestBit^, LWeight, FBlendMode);
          end
          else
          begin
            { If the foreground has semi-transparent pixels, we need to adjust the
              blending opacity. }

            if fa < 255 then
            begin
              LAdjustedOpacity := fa * LWeight div 255;
            end
            else
            begin
              LAdjustedOpacity := LWeight;
            end;

            LMixedColor := RGBBlendByMode(LForeBit^, LDestBit^, LAdjustedOpacity, FBlendMode);
          end;

          r := LDestBit^ and $FF0000;
          g := LDestBit^ and $FF00;
          b := LDestBit^ and $FF;

          if csRed in FSelectedChannelSet then
          begin
            r := LMixedColor and $FF0000;
          end;

          if csGreen in FSelectedChannelSet then
          begin
            g := LMixedColor and $FF00;
          end;

          if csBlue in FSelectedChannelSet then
          begin
            b := LMixedColor and $FF;
          end;

          if (FPreserveTransparency) or (LRGBChannelCount < 3) then
          begin
            LDestBit^ := a or r or g or b;
          end
          else
          begin
            LDestBit^ := (LMixedColor and $FF000000) or r or g or b;
          end;
        end;
      end
      else
      begin
        if not FPreserveTransparency then
        begin
          if (csRed   in FSelectedChannelSet) and
             (csGreen in FSelectedChannelSet) and
             (csBlue  in FSelectedChannelSet) then
          begin
            a := fa * LWeight div 255;

            LDestBit^ := (a shl 24) or (LForeBit^ and $FFFFFF);
          end;
        end;
      end;
    end;

    Inc(LForeBit);
    Inc(LDestBit);
  end;
end;

procedure TgmFill.DoFill32(const Dest: TBitmap32);
begin
  case FFillMode of
    gmfmColorFill:
      begin
        FillWithColor32(Dest);
      end;

    gmfmBitmapFill:
      begin
        FillWithBitmap32(Dest);
      end;
  end;
end; 

end.
 