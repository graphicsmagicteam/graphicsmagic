/* Extracted from app\base\temp-buf.c of GIMP 2.6.0
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
#pragma hdrstop

#include <stdlib.h>

#include "cbtempbuf.h"


//------------------------------------------------------------------------------

TempBuf * temp_buf_new(int width, int height, int bytes, int x, int y,
  const unsigned char *color)
{
  TempBuf *temp;

  if ( (width <= 0) | (height <= 0) | (bytes <= 0) )
  {
    return NULL;
  }

  temp = (TempBuf *)malloc(sizeof(TempBuf));

  temp->bytes  = bytes;
  temp->width  = width;
  temp->height = height;
  temp->x      = x;
  temp->y      = y;

  temp->data = (unsigned char *)malloc(width * height * bytes);

  /*  initialize the data  */
  if (color)
  {
    long i;

    /* First check if we can save a lot of work */
    for (i = 1; i < bytes; i++)
    {
      if (color[0] != color[i])
        break;
    }

    if (i == bytes)
    {
      memset(temp->data, *color, width * height * bytes);
    }
    else /* No, we cannot */
    {
      unsigned char *dptr = temp->data;

      /* Fill the first row */
      for (i = width - 1; i >= 0; --i)
      {
        const unsigned char *c = color;
        int                  j = bytes;

        while (j--)
          *dptr++ = *c++;
      }

      /* Now copy from it (we set bytes to bytes-per-row now) */
      bytes *= width;

      while (--height)
      {
        memcpy(dptr, temp->data, bytes);
        dptr += bytes;
      }
    }
  }

  return temp;
}
//------------------------------------------------------------------------------

TempBuf * temp_buf_resize(TempBuf *buf, int bytes, int x, int y,
  int width, int height)
{
  if ( (width <= 0) || (height <= 0) )
    return NULL;

  if (! buf)
  {
    buf = temp_buf_new(width, height, bytes, x, y, NULL);
  }
  else
  {
    unsigned int size = width * height * bytes;

    if ( size != (buf->width * buf->height * buf->bytes) )
    {
      buf->data = (unsigned char *)realloc(buf->data, size);
    }

    buf->x      = x;
    buf->y      = y;
    buf->width  = width;
    buf->height = height;
    buf->bytes  = bytes;
  }

  return buf;
}
//------------------------------------------------------------------------------

void temp_buf_free(TempBuf *buf)
{
  if (buf == NULL)
    return;

  if (buf->data)
    free(buf->data);

  free(buf);
}
//------------------------------------------------------------------------------

unsigned char * temp_buf_data(TempBuf *buf)
{
  return buf->data;
}
//------------------------------------------------------------------------------

unsigned char * temp_buf_data_clear(TempBuf *buf)
{
  memset(buf->data, 0, buf->height * buf->width * buf->bytes);

  return buf->data;
}
//------------------------------------------------------------------------------
