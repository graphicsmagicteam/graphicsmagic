{ This filter is written by Ma Xiaoguang and Ma Xiaoming ( gmbros@hotmail.com )
  Copyright (c) Ma Xiaoguang and Ma Xiaoming. All rights reserved.

  Last update: 2011-06-05

  This filter is based on the Photoshop tutorial --
  'Art Fragmentation of the Photo'.

  You could found it at here:
  http://www.photoshopstar.com/photo-effects/art-fragmentation-of-the-photo/

  Many thanks to the author of this tutorial.
 }

unit gmArtFragmentationFilter;

interface

uses
  SysUtils, GR32;

type
  TgmArtFragmentationFilter = class(TObject)
  private
    FBorderColor: TColor32;
    FAutoColor  : Boolean;
    
    function CalcBorderSize(const ABmp: TBitmap32): Integer;
  public
    constructor Create;

    procedure Execute(const ABmp: TBitmap32);

    property BorderColor: TColor32 read FBorderColor write FBorderColor;
    property IsAutoColor: Boolean  read FAutoColor   write FAutoColor;
  end;

implementation

uses
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmTypes,
  gmLevelsTool,
  gmGimpBaseEnums,
  gmGaussianBlurFilter;

procedure LevelsAdjustment(const ADstBmp: TBitmap32);
var
  LLevelsTool: TgmLevelsTool;
begin
  if (ADstBmp.Width <= 0) or (ADstBmp.Height <= 0) then
  begin
    Exit;
  end;

  LLevelsTool := TgmLevelsTool.Create(ADstBmp);
  try
    // 1st, adjust the RGB channels
    LLevelsTool.Channel     := GIMP_HISTOGRAM_VALUE;
    LLevelsTool.LevelsGamma := 2.0;
    LLevelsTool.Map(ADstBmp, [csRed, csGreen, csBlue]);

    // 2nd, adjust red channel
    LLevelsTool.Channel     := GIMP_HISTOGRAM_RED;
    LLevelsTool.LevelsGamma := 0.7;
    LLevelsTool.Map(ADstBmp, [csRed, csGreen, csBlue]);

    // 3rd, adjust blue channel
    LLevelsTool.Channel     := GIMP_HISTOGRAM_BLUE;
    LLevelsTool.LevelsGamma := 0.8;
    LLevelsTool.Map(ADstBmp, [csRed, csGreen, csBlue]);
  finally
    LLevelsTool.Free;
  end;
end;

constructor TgmArtFragmentationFilter.Create;
begin
  inherited Create;
  FBorderColor := clWhite32;
  FAutoColor   := False;
end;

function TgmArtFragmentationFilter.CalcBorderSize(const ABmp: TBitmap32): Integer;
const
  BORDER_SCALE = 0.06;
var
  LLength: Integer;
begin
  if ABmp.Width < ABmp.Height then
  begin
    LLength := ABmp.Width;
  end
  else
  begin
    LLength := ABmp.Height;
  end;

  Result := Round(LLength * BORDER_SCALE);
end;

procedure TgmArtFragmentationFilter.Execute(const ABmp: TBitmap32);
var
  i, j, LBorderSize           : Integer;
  LLeft, LTop, LRight, LBottom: Integer;
  LBits, LSrcBits             : PColor32;
  LRows                       : PColor32Array;
  LSrcColor                   : TColor32;
  LResultColor                : TColor32;
  LForegroundBmp              : TBitmap32;
  LFilter                     : TgmGaussianFilter;
begin
{$RANGECHECKS OFF}

  if not Assigned(ABmp) then
  begin
    raise Exception.Create('The parameter is nil.');
    Exit;
  end;

  if (ABmp.Width <= 0) or (ABmp.Height <= 0) then
  begin
    raise Exception.Create('The bitmap is empty.');
    Exit;
  end;

  LBorderSize := CalcBorderSize(ABmp);

{ Background Process }

  // 1. increase contrast by Overlay blend and Lightness by Screen blend

  LBits := @ABmp.Bits[0];

  for i := 0 to (ABmp.Width * ABmp.Height - 1) do
  begin
    LSrcColor    := LBits^;
    LResultColor := LSrcColor;

    // increase contrast
    BlendMode.OverlayBlend(LSrcColor, LResultColor, 255);

    // increase lightness
    BlendMode.ScreenBlend(LSrcColor, LResultColor, 255);

    LBits^ := LResultColor;
    Inc(LBits);
  end;

  // 2. levels ajustments

  LevelsAdjustment(ABmp);

  // 3. duplicate the last result and blur it

  LForegroundBmp := TBitmap32.Create;
  try
    LFilter := TgmGaussianFilter.Create(80);
    LFilter.Execute(ABmp, LForegroundBmp);

  // 4. blending the blurred bitmap with the last result with Multiply and SoftLight blend

    LSrcBits := @LForegroundBmp.Bits[0];
    LBits    := @ABmp.Bits[0];

    for i := 0 to (ABmp.Width * ABmp.Height - 1) do
    begin
      LSrcColor    := LSrcBits^;
      LResultColor := LBits^;

      BlendMode.MultiplyBlend(LSrcColor, LResultColor, 255);
      BlendMode.SoftLightBlend(LSrcColor, LResultColor, 255);

      LBits^ := LResultColor;

      Inc(LSrcBits);
      Inc(LBits);
    end;

{ Process Border }

  // 5. create foreground border

    LLeft   := LBorderSize;
    LTop    := LBorderSize;
    LRight  := LForegroundBmp.Width  - LBorderSize - 1;
    LBottom := LForegroundBmp.Height - LBorderSize - 1;

    for j := LTop to LBottom do
    begin
      LRows := LForegroundBmp.ScanLine[j];

      for i := LLeft to LRight do
      begin

        if ( j > (LTop + 1) )  and ( j < (LBottom - 1) ) and
           ( i > (LLeft + 1) ) and ( i < (LRight - 1) ) then
        begin
          // delete internal pixels -- make their alpha to zero
          LRows[i] := LRows[i] and $00FFFFFF;
        end
        else
        begin
          // draw border pixels
          if FAutoColor then
          begin
            LRows[i] := LRows[i] xor $FFFFFF;
          end
          else
          begin
            LRows[i] := FBorderColor;
          end;
        end;
      end;
    end;

  // 6. blend to get the result
  
    LSrcBits := @LForegroundBmp.Bits[0];
    LBits    := @ABmp.Bits[0];

    for i := 0 to (ABmp.Width * ABmp.Height - 1) do
    begin
      LSrcColor    := LSrcBits^;
      LResultColor := LBits^;

      BlendMode.NormalBlend(LSrcColor, LResultColor, 255);

      LBits^ := LResultColor;

      Inc(LSrcBits);
      Inc(LBits);
    end;

  finally
    LForegroundBmp.Free;
  end; 

{$RANGECHECKS ON}
end;

end.
