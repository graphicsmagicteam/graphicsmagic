{ This unit is written by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
  in 2011-03-12.

  Based on "base\base-enums.h" and "base\base-enums.c" of GIMP 2.2.10,
  and app\base\base-enums.h of GIMP 2.6.0
  
  The original source can be found at www.gimp.org.

  -- LICENSE -------------------------------------------------------------------
  
  GIMP - The GNU Image Manipulation Program
  Copyright (C) 1995 Spencer Kimball and Peter Mattis

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 }

unit gmGimpBaseEnums;

interface

type
  TgmGimpConvolutionType = (gctNormalConvolve,     // negative numbers truncated
                            gctAbsoluteConvolve,   // absolute value
                            gctNegativeConvolve);  // add 127 to values

const
  GIMP_CURVE_SMOOTH    = 0;  { < desc="Smooth"   > }
  GIMP_CURVE_FREE      = 1;  { < desc="Freehand" > }

  GIMP_HISTOGRAM_VALUE = 0;  { < desc="Value" > }
  GIMP_HISTOGRAM_RED   = 1;  { < desc="Red"   > }
  GIMP_HISTOGRAM_GREEN = 2;  { < desc="Green" > }
  GIMP_HISTOGRAM_BLUE  = 3;  { < desc="Blue"  > }
  GIMP_HISTOGRAM_ALPHA = 4;  { < desc="Alpha" > }
  GIMP_HISTOGRAM_RGB   = 5;  { < desc="RGB", pdb-skip > }

  GRAPH_SIZE = 256;

implementation

end.
