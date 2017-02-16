/* Extracted from app\tools\gimpdrawtool.c of GIMP 2.6.0
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995-2001 Spencer Kimball, Peter Mattis, and others
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

#include "cbgimpdrawtool.h"
#include "cbgimpmath.h"
#include "cbgtypes.h"

//------------------------------------------------------------------------------

/*  private functions  */

static inline void
gimp_draw_tool_shift_to_north_west(double        x,
                                   double        y,
                                   int           handle_width,
                                   int           handle_height,
                                   GtkAnchorType anchor,
                                   double       *shifted_x,
                                   double       *shifted_y)
{
  switch (anchor)
  {
  case GTK_ANCHOR_CENTER:
    x -= (handle_width  >> 1);
    y -= (handle_height >> 1);
    break;

  case GTK_ANCHOR_NORTH:
    x -= (handle_width >> 1);
    break;

  case GTK_ANCHOR_NORTH_WEST:
    /*  nothing, this is the default  */
    break;

  case GTK_ANCHOR_NORTH_EAST:
    x -= handle_width;
    break;

  case GTK_ANCHOR_SOUTH:
    x -= (handle_width >> 1);
    y -= handle_height;
    break;

  case GTK_ANCHOR_SOUTH_WEST:
    y -= handle_height;
    break;

  case GTK_ANCHOR_SOUTH_EAST:
    x -= handle_width;
    y -= handle_height;
    break;

  case GTK_ANCHOR_WEST:
    y -= (handle_height >> 1);
    break;

  case GTK_ANCHOR_EAST:
    x -= handle_width;
    y -= (handle_height >> 1);
    break;

  default:
    break;
  }

  if (shifted_x)
    *shifted_x = x;

  if (shifted_y)
    *shifted_y = y;
}
//------------------------------------------------------------------------------

static inline void
gimp_draw_tool_shift_to_center(double        x,
                               double        y,
                               int           handle_width,
                               int           handle_height,
                               GtkAnchorType anchor,
                               double       *shifted_x,
                               double       *shifted_y)
{
  switch (anchor)
    {
    case GTK_ANCHOR_CENTER:
      /*  nothing, this is the default  */
      break;

    case GTK_ANCHOR_NORTH:
      y += (handle_height >> 1);
      break;

    case GTK_ANCHOR_NORTH_WEST:
      x += (handle_width >> 1);
      y += (handle_height >> 1);
      break;

    case GTK_ANCHOR_NORTH_EAST:
      x -= (handle_width >> 1);
      y += (handle_height >> 1);
      break;

    case GTK_ANCHOR_SOUTH:
      y -= (handle_height >> 1);
      break;

    case GTK_ANCHOR_SOUTH_WEST:
      x += (handle_width >> 1);
      y -= (handle_height >> 1);
      break;

    case GTK_ANCHOR_SOUTH_EAST:
      x -= (handle_width >> 1);
      y -= (handle_height >> 1);
      break;

    case GTK_ANCHOR_WEST:
      x += (handle_width >> 1);
      break;

    case GTK_ANCHOR_EAST:
      x -= (handle_width >> 1);
      break;

    default:
      break;
    }

  if (shifted_x)
    *shifted_x = x;

  if (shifted_y)
    *shifted_y = y;
}
//------------------------------------------------------------------------------

/**
 * gimp_draw_tool_calc_distance_square:
 * @x1: start point X in image coordinates
 * @y1: start point Y in image coordinates
 * @x2: end point X in image coordinates
 * @y2: end point Y in image coordinates
 *
 * This function is more effective than gimp_draw_tool_calc_distance()
 * as it doesn't perform a sqrt(). Use this if you just need to compare
 * distances.
 *
 * Returns: the square of the distance between the given points in
            display coordinates
 **/
double gimp_draw_tool_calc_distance_square (double x1,
                                            double y1,
                                            double x2,
                                            double y2)
{
  return ( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
}
//------------------------------------------------------------------------------

/**
 * gimp_draw_tool_in_radius:
 * @x1:     start point X in image coordinates
 * @y1:     start point Y in image coordinates
 * @x2:     end point X in image coordinates
 * @y2:     end point Y in image coordinates
 * @radius: distance in screen coordinates, not image coordinates
 *
 * The points are in image space coordinates.
 *
 * Returns: %TRUE if the points are within radius of each other,
 *          %FALSE otherwise
 **/
bool gimp_draw_tool_in_radius (double x1,
                               double y1,
                               double x2,
                               double y2,
                               int    radius)
{
  return ( gimp_draw_tool_calc_distance_square (x1, y1, x2, y2) < (radius * radius) );
}
//------------------------------------------------------------------------------

void gimp_draw_tool_draw_line (TCanvas *canvas,
                               double   x1,
                               double   y1,
                               double   x2,
                               double   y2,
                               bool     use_offsets)
{
  double   tx1, ty1, tx2, ty2;
  int      lw;
  TPenMode pm;
  TColor   pc;

  lw = canvas->Pen->Width;
  pm = canvas->Pen->Mode;
  pc = canvas->Pen->Color;

  canvas->Pen->Width = 1;
  canvas->Pen->Mode  = pmNotXor;
  canvas->Pen->Color = clBlack;

  tx1 = (int)RINT(x1);
  ty1 = (int)RINT(y1);
  tx2 = (int)RINT(x2);
  ty2 = (int)RINT(y2);

  canvas->MoveTo(tx1, ty1);
  canvas->LineTo(tx2, ty2);

  canvas->Pen->Width = lw;
  canvas->Pen->Mode  = pm;
  canvas->Pen->Color = pc;
}
//------------------------------------------------------------------------------

void gimp_draw_tool_draw_rectangle_by_anchor (TCanvas      *canvas,
                                              bool          filled,
                                              double        x,
                                              double        y,
                                              int           width,
                                              int           height,
                                              GtkAnchorType anchor,
                                              bool          use_offsets)
{
  double      tx, ty;
  TPenMode    pm;
  TBrushStyle bs;
  TColor      bsc;
  TColor      pc;

  gimp_draw_tool_shift_to_north_west(x, y, width, height, anchor, &tx, &ty);

  pm  = canvas->Pen->Mode;
  pc  = canvas->Pen->Color;
  bs  = canvas->Brush->Style;
  bsc = canvas->Brush->Color;

  canvas->Pen->Mode    = pmNotXor;
  canvas->Brush->Color = clBlack;

  if (! filled)
  {
    width  -= 1;
    height -= 1;
    
    canvas->Brush->Style = bsClear;
  }
  else
  {
    canvas->Brush->Style = bsSolid;
  }

  tx = (int)RINT(tx);
  ty = (int)RINT(ty);

  canvas->Rectangle(tx, ty, tx + width, ty + height);

  canvas->Pen->Mode    = pm;
  canvas->Pen->Color   = pc;
  canvas->Brush->Style = bs;
  canvas->Brush->Color = bsc;
}

//------------------------------------------------------------------------------

void gimp_draw_tool_draw_arc_by_anchor (TCanvas      *canvas,
                                        bool          filled,
                                        double        x,
                                        double        y,
                                        int           width,
                                        int           height,
                                        int           angle1,
                                        int           angle2,
                                        GtkAnchorType anchor,
                                        bool          use_offsets)
{
  double      tx, ty;
  int         nXStartArc, nYStartArc, nXEndArc, nYEndArc;
  TPenMode    pm;
  TBrushStyle bs;
  TColor      bsc;
  TColor      pc;


  gimp_draw_tool_shift_to_north_west(x, y, width, height, anchor, &tx, &ty);

  pm  = canvas->Pen->Mode;
  pc  = canvas->Pen->Color;
  bs  = canvas->Brush->Style;
  bsc = canvas->Brush->Color;

  canvas->Pen->Mode    = pmNotXor;
  canvas->Brush->Style = bsSolid;
  canvas->Brush->Color = clBlack;

  if (! filled)
  {
    width  -= 1;
    height -= 1;
  }

  tx = (int)RINT(tx);
  ty = (int)RINT(ty);

  if (angle2 >= 360*64)
    {
      nXStartArc = nYStartArc = nXEndArc = nYEndArc = 0;
    }
  else if (angle2 > 0)
    {
      nXStartArc = tx + width/2  + width   * cos(angle1/64.*2.*G_PI/360.);
      nYStartArc = ty + height/2 + -height * sin(angle1/64.*2.*G_PI/360.);
      nXEndArc   = tx + width/2  + width   * cos((angle1+angle2)/64.*2.*G_PI/360.);
      nYEndArc   = ty + height/2 + -height * sin((angle1+angle2)/64.*2.*G_PI/360.);
    }
  else
    {
      nXEndArc   = tx + width/2  + width   * cos(angle1/64.*2.*G_PI/360.);
      nYEndArc   = ty + height/2 + -height * sin(angle1/64.*2.*G_PI/360.);
      nXStartArc = tx + width/2  + width   * cos((angle1+angle2)/64.*2.*G_PI/360.);
      nYStartArc = ty + height/2 + -height * sin((angle1+angle2)/64.*2.*G_PI/360.);
    }
  
  if (filled)
    {
      canvas->Pie(tx, ty, tx + width, ty + height,
                  nXStartArc, nYStartArc, nXEndArc, nYEndArc);
    }
  else
    {
      canvas->Arc(tx, ty, tx + width, ty + height,
                  nXStartArc, nYStartArc, nXEndArc, nYEndArc);
    }

  canvas->Pen->Mode    = pm;
  canvas->Pen->Color   = pc;
  canvas->Brush->Style = bs;
  canvas->Brush->Color = bsc;
}
//------------------------------------------------------------------------------

void gimp_draw_tool_draw_cross_by_anchor (TCanvas      *canvas,
                                          double        x,
                                          double        y,
                                          int           width,
                                          int           height,
                                          GtkAnchorType anchor,
                                          bool          use_offsets)
{
  double   tx, ty;
  TPenMode pm;
  TColor   pc;

  pm = canvas->Pen->Mode;
  pc = canvas->Pen->Color;
  
  canvas->Pen->Mode = pmNotXor;

  gimp_draw_tool_shift_to_center(x, y, width, height, anchor, &tx, &ty);

  tx = (int)RINT(tx);
  ty = (int)RINT(ty);

  canvas->MoveTo( tx, ty - (height >> 1) );
  canvas->LineTo( tx, ty + ((height + 1) >> 1) );

  canvas->MoveTo( tx - (width >> 1), ty );
  canvas->LineTo( tx + ((width + 1) >> 1), ty );

  canvas->Pen->Mode  = pm;
  canvas->Pen->Color = pc;
}
//------------------------------------------------------------------------------

void gimp_draw_tool_draw_handle (TCanvas       *canvas,
                                 GimpHandleType type,
                                 double         x,
                                 double         y,
                                 int            width,
                                 int            height,
                                 GtkAnchorType  anchor,
                                 bool           use_offsets)
{
  if (canvas == NULL)
    return;

  switch (type)
  {
  case GIMP_HANDLE_SQUARE:
    gimp_draw_tool_draw_rectangle_by_anchor(canvas,
                                            FALSE,
                                            x, y,
                                            width,
                                            height,
                                            anchor,
                                            use_offsets);
    break;

  case GIMP_HANDLE_FILLED_SQUARE:
    gimp_draw_tool_draw_rectangle_by_anchor (canvas,
                                             TRUE,
                                             x, y,
                                             width,
                                             height,
                                             anchor,
                                             use_offsets);
    break;

  case GIMP_HANDLE_CIRCLE:
    gimp_draw_tool_draw_arc_by_anchor (canvas,
                                       FALSE,
                                       x, y,
                                       width,
                                       height,
                                       0, 360 * 64,
                                       anchor,
                                       use_offsets);
    break;

  case GIMP_HANDLE_FILLED_CIRCLE:
    gimp_draw_tool_draw_arc_by_anchor (canvas,
                                       TRUE,
                                       x, y,
                                       width,
                                       height,
                                       0, 360 * 64,
                                       anchor,
                                       use_offsets);
    break;

  case GIMP_HANDLE_CROSS:
    gimp_draw_tool_draw_cross_by_anchor (canvas,
                                         x, y,
                                         width,
                                         height,
                                         anchor,
                                         use_offsets);
    break;
  }
}
//------------------------------------------------------------------------------

bool gimp_draw_tool_on_handle (double         x,
                               double         y,
                               GimpHandleType type,
                               double         handle_x,
                               double         handle_y,
                               int            width,
                               int            height,
                               GtkAnchorType  anchor,
                               bool           use_offsets)
{
  double handle_tx, handle_ty;
  
  switch (type)
  {
  case GIMP_HANDLE_SQUARE:
  case GIMP_HANDLE_FILLED_SQUARE:
  case GIMP_HANDLE_CROSS:
    gimp_draw_tool_shift_to_north_west(handle_x, handle_y,
                                       width, height,
                                       anchor,
                                       &handle_tx, &handle_ty);

    return (x == CLAMP(x, handle_tx, handle_tx + width) &&
            y == CLAMP(y, handle_ty, handle_ty + height));

  case GIMP_HANDLE_CIRCLE:
  case GIMP_HANDLE_FILLED_CIRCLE:
    gimp_draw_tool_shift_to_center(handle_x, handle_y,
                                   width, height,
                                   anchor,
                                   &handle_tx, &handle_ty);

    /* FIXME */
    if (width != height)
      width = (width + height) / 2;

    width /= 2;

    return ( ((handle_tx - x) * (handle_tx - x) +
              (handle_ty - y) * (handle_ty - y)) < (width * width) );

  default:
    //g_warning ("%s: invalid handle type %d", G_STRFUNC, type);
    break;
  }

  return FALSE;
}
//------------------------------------------------------------------------------

void gimp_draw_tool_draw_lines (TCanvas           *canvas,
                                const GimpVector2 *points,
                                int                n_points,
                                bool               filled,
                                bool               use_offsets)
{
  int i;

  if (canvas == NULL)
    return;

  if (points == NULL || n_points == 0)
    return;

  if (filled)
    {
      //gimp_canvas_draw_polygon (GIMP_CANVAS (shell->canvas),
      //                          GIMP_CANVAS_STYLE_XOR,
      //                          TRUE, coords, n_points);
    }
  else
    {
      for (i = 0; i < n_points - 1; i++)
      {
        double x1, y1, x2, y2;

        x1 = points[i].x;
        y1 = points[i].y;
        x2 = points[i+1].x;
        y2 = points[i+1].y;

        gimp_draw_tool_draw_line(canvas, x1, y1, x2, y2, FALSE);
      }
    }
}
//------------------------------------------------------------------------------
