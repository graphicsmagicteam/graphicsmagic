/* Extracted from gtk\gtkenums.h of gtk+ 2.8.9
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef CB_GTK_ENUMS_H
#define CB_GTK_ENUMS_H


/* Anchor types */
typedef enum
{
  GTK_ANCHOR_CENTER,
  GTK_ANCHOR_NORTH,
  GTK_ANCHOR_NORTH_WEST,
  GTK_ANCHOR_NORTH_EAST,
  GTK_ANCHOR_SOUTH,
  GTK_ANCHOR_SOUTH_WEST,
  GTK_ANCHOR_SOUTH_EAST,
  GTK_ANCHOR_WEST,
  GTK_ANCHOR_EAST,
  GTK_ANCHOR_N		= GTK_ANCHOR_NORTH,
  GTK_ANCHOR_NW		= GTK_ANCHOR_NORTH_WEST,
  GTK_ANCHOR_NE		= GTK_ANCHOR_NORTH_EAST,
  GTK_ANCHOR_S		= GTK_ANCHOR_SOUTH,
  GTK_ANCHOR_SW		= GTK_ANCHOR_SOUTH_WEST,
  GTK_ANCHOR_SE		= GTK_ANCHOR_SOUTH_EAST,
  GTK_ANCHOR_W		= GTK_ANCHOR_WEST,
  GTK_ANCHOR_E		= GTK_ANCHOR_EAST
} GtkAnchorType;



#endif /*  CB_GTK_ENUMS_H  */
