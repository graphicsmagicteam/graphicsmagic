{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "libgimpcolor\gimprgb.h" and "libgimpcolor\gimprgb.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

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

unit gmGimpRGB;

interface

uses
  gmGimpColorTypes;

  {  RGB functions  }

  procedure gimp_rgb_set(var rgb: TGimpRGB; const Red, Green, Blue: Double);

  {  Map RGB to intensity  }

  function Gimp_RGB_Intensity(const r, g, b: Byte): Double;

const
  GIMP_RGB_INTENSITY_RED   = 0.30;
  GIMP_RGB_INTENSITY_GREEN = 0.59;
  GIMP_RGB_INTENSITY_BLUE  = 0.11;

implementation

{  RGB functions  }

{ gimp_rgb_set:
  a_rgb: a TGimpRGB record type
  a_red:
  a_green:
  a_blue:

  Sets the red, green and blue components of @rgb and leaves the
  alpha component unchanged. The color values should be between 0.0
  and 1.0 but there is no check to enforce this and the values are
  set exactly as they are passed in. }
procedure gimp_rgb_set(var rgb: TGimpRGB; const Red, Green, Blue: Double);
begin
  with rgb do
  begin
    r := Red;
    g := Green;
    b := Blue;
  end;
end;

{  Map RGB to intensity  }
function Gimp_RGB_Intensity(const r, g, b: Byte): Double;
begin
  Result := r * GIMP_RGB_INTENSITY_RED   +
            g * GIMP_RGB_INTENSITY_GREEN +
	          b * GIMP_RGB_INTENSITY_BLUE;
end; 

end.
 