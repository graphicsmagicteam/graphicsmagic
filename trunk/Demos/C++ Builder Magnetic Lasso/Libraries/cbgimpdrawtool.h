/* Extracted from app\tools\gimpdrawtool.h of GIMP 2.6.0
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995-2001 Spencer Kimball, Peter Mattis, and others.
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

#ifndef CB_GIMP_DRAW_TOOL_H
#define CB_GIMP_DRAW_TOOL_H

#include <Graphics.hpp>

#include "cbgtkenums.h"
#include "cbgimpmathtypes.h"


typedef enum
{
  GIMP_HANDLE_SQUARE,
  GIMP_HANDLE_FILLED_SQUARE,
  GIMP_HANDLE_CIRCLE,
  GIMP_HANDLE_FILLED_CIRCLE,
  GIMP_HANDLE_CROSS
} GimpHandleType;


double gimp_draw_tool_calc_distance_square (double x1,
                                            double y1,
                                            double x2,
                                            double y2);

bool gimp_draw_tool_in_radius (double x1,
                               double y1,
                               double x2,
                               double y2,
                               int    radius);

void gimp_draw_tool_draw_line (TCanvas *canvas,
                               double   x1,
                               double   y1,
                               double   x2,
                               double   y2,
                               bool     use_offsets);

void gimp_draw_tool_draw_rectangle_by_anchor (TCanvas      *canvas,
                                              bool          filled,
                                              double        x,
                                              double        y,
                                              int           width,
                                              int           height,
                                              GtkAnchorType anchor,
                                              bool          use_offsets);

                                              
void gimp_draw_tool_draw_handle (TCanvas       *canvas,
                                 GimpHandleType type,
                                 double         x,
                                 double         y,
                                 int            width,
                                 int            height,
                                 GtkAnchorType  anchor,
                                 bool           use_offsets);


bool gimp_draw_tool_on_handle (double         x,
                               double         y,
                               GimpHandleType type,
                               double         handle_x,
                               double         handle_y,
                               int            width,
                               int            height,
                               GtkAnchorType  anchor,
                               bool           use_offsets);

void gimp_draw_tool_draw_lines (TCanvas           *canvas,
                                const GimpVector2 *points,
                                int                n_points,
                                bool               filled,
                                bool               use_offsets);

#endif /*  CB_GIMP_DRAW_TOOL_H  */
