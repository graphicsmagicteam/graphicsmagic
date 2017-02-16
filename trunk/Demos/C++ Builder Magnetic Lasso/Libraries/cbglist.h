/* Extracted from glib\glist.h of GLib 2.12.1
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

#ifndef CB_G_LIST_H
#define CB_G_LIST_H

typedef struct _GList GList;

struct _GList
{
  void  *data;
  GList *next;
  GList *prev;
};

/* Doubly linked lists */
GList* g_list_alloc(void);
void   g_list_free(GList *list);
GList* g_list_append(GList *list, void *data);

GList* g_list_insert_before(GList *list, GList *sibling, void *data);

GList* g_list_last(GList *list);

#define g_list_previous(list)  ((list) ? (((GList *)(list))->prev) : NULL)
#define g_list_next(list)      ((list) ? (((GList *)(list))->next) : NULL)

#endif /*  CB_G_LIST_H  */
