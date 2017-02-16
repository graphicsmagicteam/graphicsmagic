/* Extracted from glib\garray.c of GLib 2.12.1
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/. 
 */

/* 
 * MT safe
 */

#include <vcl.h>
#pragma hdrstop

#include "cbgarray.h"

#define MIN_ARRAY_SIZE  16

//------------------------------------------------------------------------------

static int g_nearest_pow(int num)
{
  int n = 1;

  while (n < num)
    n <<= 1;

  return n;
}
//------------------------------------------------------------------------------

static void g_ptr_array_maybe_expand(GPtrArray *array, int len)
{
  if ( (array->len + len) > array->alloc )
  {
    unsigned int old_alloc = array->alloc;

    array->alloc = g_nearest_pow(array->len + len);
    array->alloc = max(array->alloc, (unsigned int)MIN_ARRAY_SIZE);
    array->pdata = (void **)realloc(array->pdata, sizeof(void *) * array->alloc);

    for ( ; old_alloc < array->alloc; old_alloc++)
      array->pdata[old_alloc] = NULL;
  }
}
//------------------------------------------------------------------------------

GPtrArray* g_ptr_array_new(void)
{
  return g_ptr_array_sized_new(0);
}
//------------------------------------------------------------------------------

GPtrArray* g_ptr_array_sized_new(unsigned int reserved_size)
{
  GPtrArray *array = (GPtrArray *)malloc( sizeof(GPtrArray) );

  array->pdata = NULL;
  array->len   = 0;
  array->alloc = 0;

  if (reserved_size != 0)
    g_ptr_array_maybe_expand(array, reserved_size);

  return (GPtrArray *)array;
}
//------------------------------------------------------------------------------

void** g_ptr_array_free(GPtrArray *array, bool free_segment)
{
  void** segment;

  if (array == NULL)
    return NULL;

  if (free_segment)
    {
      free(array->pdata);
      segment = NULL;
    }
  else
    segment = array->pdata;

  free(array);

  return segment;
}
//------------------------------------------------------------------------------

void g_ptr_array_add(GPtrArray *array, void *data)
{
  if (array == NULL)
    return;

  g_ptr_array_maybe_expand(array, 1);

  array->pdata[array->len++] = data;
}
//------------------------------------------------------------------------------


