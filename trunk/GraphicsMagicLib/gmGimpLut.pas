{ This library created in 01/27/2006,
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "base\gimplut.h" and "base\gimplut.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

  gimplut.h: Copyright (C) 1999 Jay Cox <jaycox@earthlink.net>

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

unit gmGimpLut;

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  GR32, gmTypes;

type
  TgmGimpLut = record
    LUTs     : array of array of Byte;
    NChannels: Integer;
  end;

  function gimp_lut_new: TgmGimpLut;

  procedure gimp_lut_process(ALUT: TgmGimpLut; ASourceBmp, ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet);

implementation

function gimp_lut_new: TgmGimpLut;
begin
  Result.LUTs      := nil;
  Result.NChannels := 0;
end; 

procedure gimp_lut_process(ALUT: TgmGimpLut; ASourceBmp, ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i                : Integer;
  LSrcBit, LDestBit: PColor32;
  a, r, g, b       : Cardinal;
begin
  ADestBmp.SetSize(ASourceBmp.Width, ASourceBmp.Height);

  LSrcBit  := @ASourceBmp.Bits[0];
  LDestBit := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a := LSrcBit^ and $FF000000;

    if csGrayscale in AChannelSet then
    begin
      b         := LSrcBit^ and $FF;
      b         := ALUT.LUTs[2, b];
      LDestBit^ := a or (b shl 16) or (b shl 8) or b;
    end
    else
    begin
      r := LSrcBit^ shr 16 and $FF;
      g := LSrcBit^ shr  8 and $FF;
      b := LSrcBit^        and $FF;

      if csRed in AChannelSet then
      begin
        r := ALUT.LUTs[0, r];
      end;

      if csGreen in AChannelSet then
      begin
        g := ALUT.LUTs[1, g];
      end;

      if csBlue in AChannelSet then
      begin
        b := ALUT.LUTs[2, b];
      end;

      LDestBit^ := a or (r shl 16) or (g shl 8) or b;
    end;

    Inc(LSrcBit);
    Inc(LDestBit);
  end;
end; 

end.
