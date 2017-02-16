/* Extracted from app\paint-funcs\paint-funcs.c of GIMP 2.6.0
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

#include <vcl.h>
#include <Dialogs.hpp>
#pragma hdrstop

#include "cbpaintfuncs.h"
#include "cbgimpmath.h"

//------------------------------------------------------------------------------

void convolve_region(Graphics::TBitmap  *srcR,
                     Graphics::TBitmap  *destR,
                     const float        *matrix,
                     int                 size,
                     double              divisor,
                     GimpConvolutionType mode,
                     bool                alpha_weighting)
{
  /*  Convolve the src image using the convolution matrix, writing to dest  */
  /*  Convolve is not tile-enabled--use accordingly  */
  unsigned char *src;
  unsigned char *dest;
  int            bytes;
  int            a_byte;
  int            rowstride;
  const int      margin = size / 2;
  int            x1;
  int            y1;
  int            x2;
  int            y2;
  int            x, y;
  int            offset;

  unsigned char *d;

  switch (srcR->PixelFormat)
  {
  case pf8bit:
    bytes = 1;
    break;

  case pf16bit:
    bytes = 2;
    break;

  case pf24bit:
    bytes = 3;
    break;

  case pf32bit:
    bytes = 4;
    break;

  default:
    bytes = 0;
    break;
  }

  if (bytes == 0)
    return;

  a_byte = bytes - 1;
  x1     = 0;
  y1     = 0;
  x2     = srcR->Width  - 1;
  y2     = srcR->Height - 1;

  /*  If the mode is NEGATIVE_CONVOL, the offset should be 128  */
  if (mode == GIMP_NEGATIVE_CONVOL)
  {
    offset = 128;
    mode   = GIMP_NORMAL_CONVOL;
  }
  else
  {
    offset = 0;
  }

  for (y = 0; y < destR->Height; y++)
  {
    unsigned char *d;

    d = (unsigned char *)destR->ScanLine[y];

    if (alpha_weighting)
    {
      for (x = 0; x < destR->Width; x++)
      {
        const float *m                = matrix;
        double       total[4]         = { 0.0, 0.0, 0.0, 0.0 };
        double       weighted_divisor = 0.0;
        int          i, j, b;

        for (j = y - margin; j <= y + margin; j++)
        {
          for (i = x - margin; i <= x + margin; i++, m++)
          {
            int            xx = CLAMP(i, x1, x2);
            int            yy = CLAMP(j, y1, y2);
            unsigned char *s;
            unsigned char  a;

            s = (unsigned char *)srcR->ScanLine[yy];
            a = s[xx * bytes + a_byte];

            if (a)
            {
              double mult_alpha = *m * a;

              weighted_divisor += mult_alpha;

              for (b = 0; b < a_byte; b++)
                total[b] += mult_alpha * s[xx * bytes + b];

              total[a_byte] += mult_alpha;
            }
          }
        }

        if (weighted_divisor == 0.0)
          weighted_divisor = divisor;

        for (b = 0; b < a_byte; b++)
          total[b] /= weighted_divisor;

        total[a_byte] /= divisor;

        for (b = 0; b < bytes; b++)
        {
          total[b] += offset;

          if ( (mode != GIMP_NORMAL_CONVOL) && (total[b] < 0.0) )
            total[b] = - total[b];

          //if (total[b] < 0.0)
          //  d[x * bytes + b] = 0;
          //else
          //  d[x * bytes + b] = (total[b] > 255.0) ? 255 : (unsigned char)ceil(total[b]);

          if (total[b] < 0.0)
            *d++ = 0;
          else
            *d++ = (total[b] > 255.0) ? 255 : (unsigned char)ceil(total[b]);
        }
      }
    }
    else /*  alpha_weighting == FALSE  */
    {
      for (x = 0; x < destR->Width; x++)
      {
        const float *m        = matrix;
        double total[4] = { 0.0, 0.0, 0.0, 0.0 };
        int    i, j, b;

        for (j = y - margin; j <= y + margin; j++)
        {
          for (i = x - margin; i <= x + margin; i++, m++)
          {
            int            xx = CLAMP(i, x1, x2);
            int            yy = CLAMP(j, y1, y2);
            unsigned char *s;

            s = (unsigned char *)srcR->ScanLine[yy];
            s += xx * bytes;

            for (b = 0; b < bytes; b++)
              total[b] += *m * s[b];
          }
        }

        for (b = 0; b < bytes; b++)
        {
          unsigned char *s;

          total[b] = total[b] / divisor + offset;

          if ( (mode != GIMP_NORMAL_CONVOL) && (total[b] < 0.0) )
            total[b] = - total[b];

          //if (total[b] < 0.0)
          //  d[x * bytes + b] = 0.0;
          //else
          //  d[x * bytes + b] = (total[b] > 255.0) ? 255 : (unsigned char)ceil(total[b]);

          if (total[b] < 0.0)
            *d++ = 0.0;
          else
            *d++ = (total[b] > 255.0) ? 255 : (unsigned char)ceil(total[b]);
        }
      }
    }
  }
}
//------------------------------------------------------------------------------
