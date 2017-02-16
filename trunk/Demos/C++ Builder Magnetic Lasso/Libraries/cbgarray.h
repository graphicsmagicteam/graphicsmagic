/* Extracted from glib\garray.h of GLib 2.12.1
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
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

#ifndef CB_G_ARRAY_H
#define CB_G_ARRAY_H

typedef struct _GPtrArray  GPtrArray;

struct _GPtrArray
{
  void       **pdata;
  unsigned int len;
  unsigned int alloc;
};

/* Resizable pointer array.  This interface is much less complicated
 * than the above.  Add appends a pointer.  Remove fills any cleared 
 * spot and shortens the array. remove_fast will again distort order.  
 */
GPtrArray* g_ptr_array_new       (void);
GPtrArray* g_ptr_array_sized_new (unsigned int reserved_size);
void**     g_ptr_array_free      (GPtrArray *array, bool free_seg);
void       g_ptr_array_add       (GPtrArray *array, void *data);

#endif /* CB_G_ARRAY_H */
