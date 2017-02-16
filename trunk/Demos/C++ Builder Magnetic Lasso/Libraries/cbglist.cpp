/* Extracted from glib\glist.c of GLib 2.12.1
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

/* 
 * MT safe
 */

#include <vcl.h>
#pragma hdrstop

#include <stdlib.h>

#include "cbglist.h"

//------------------------------------------------------------------------------

GList* g_list_alloc(void)
{
  void *mem = malloc( sizeof(GList) );

  if (mem)
    memset( mem, 0, sizeof(GList) );

  return (GList*)mem;
}
//------------------------------------------------------------------------------

void g_list_free(GList *list)
{
  GList *temp;

  while (list)
  {
    if (list->data)
      free(list->data);

    temp = list;
    list = list->next;
    free(temp);
  }
}
//------------------------------------------------------------------------------

GList* g_list_append(GList *list, void *data)
{
  GList *new_list;
  GList *last;

  new_list       = (GList *)malloc(sizeof(GList));
  new_list->data = data;
  new_list->next = NULL;

  if (list)
  {
    last = g_list_last(list);
    /* g_assert (last != NULL); */
    last->next     = new_list;
    new_list->prev = last;

    return list;
  }
  else
  {
    new_list->prev = NULL;
    return new_list;
  }
}
//------------------------------------------------------------------------------

GList* g_list_insert_before(GList *list, GList *sibling, void *data)
{
  if (!list)
  {
    list = (GList *)malloc( sizeof(GList) );

    list->data = data;

    if (sibling != NULL)
      return list;

    return list;
  }
  else if (sibling)
  {
    GList *node;

    node = (GList *)malloc( sizeof(GList) );

    node->data    = data;
    node->prev    = sibling->prev;
    node->next    = sibling;
    sibling->prev = node;

    if (node->prev)
    {
      node->prev->next = node;
      return list;
    }
    else
    {
      if (sibling != list)
        return node;

      return node;
    }
  }
  else
  {
    GList *last;

    last = list;
    while (last->next)
      last = last->next;

    last->next = (GList *)malloc( sizeof(GList) );

    last->next->data = data;
    last->next->prev = last;
    last->next->next = NULL;

    return list;
  }
}
//------------------------------------------------------------------------------

GList* g_list_last(GList *list)
{
  if (list)
  {
    while (list->next)
      list = list->next;
  }

  return list;
}
//------------------------------------------------------------------------------
