/* Extracted from app\base\temp-buf.h of GIMP 2.6.0
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

#ifndef CB_TEMP_BUF_H
#define CB_TEMP_BUF_H

#include "cbbasetypes.h"

struct _TempBuf
{
  int bytes;              /*  number of bytes per pixel (1,2,3 or 4)         */
  int width;
  int height;
  int x, y;               /*  origin of data source                          */
  unsigned char *data;    /*  The data buffer. Do never access this field
                              directly, use temp_buf_data() instead !!       */
};


/*  The temp buffer functions  */

TempBuf * temp_buf_new(int width, int height, int bytes, int x, int y,
  const unsigned char *color);

TempBuf * temp_buf_resize(TempBuf *buf, int bytes, int x, int y,
  int width, int height);

void temp_buf_free(TempBuf *buf);

unsigned char * temp_buf_data(TempBuf *buf);
unsigned char * temp_buf_data_clear(TempBuf *buf);

#endif /*  CB_TEMP_BUF_H  */
