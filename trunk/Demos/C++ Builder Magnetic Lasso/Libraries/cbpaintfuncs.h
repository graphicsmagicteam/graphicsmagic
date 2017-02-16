/* Extracted from app\paint-funcs\paint-funcs.h of GIMP 2.6.0
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef CB_PAINT_FUNCS_H
#define CB_PAINT_FUNCS_H

#include "cbbaseenums.h"

void  convolve_region(Graphics::TBitmap  *srcR,
                      Graphics::TBitmap  *destR,
                      const float        *matrix,
                      int                 size,
                      double              divisor,
                      GimpConvolutionType mode,
                      bool                alpha_weighting);

#endif /*  CB_PAINT_FUNC_H  */
