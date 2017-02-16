/* Extracted from app\tools\gimpiscissorstool.h of GIMP 2.6.0
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

#ifndef CB_ISCISSORS_TOOL_H
#define CB_ISCISSORS_TOOL_H

#include <Graphics.hpp>

#include "cbbasetypes.h"
#include "cbgqueue.h"
#include "cbgarray.h"

/*  The possible states...  */
typedef enum
{
  NO_ACTION,
  SEED_PLACEMENT,
  SEED_ADJUSTMENT,
  WAITING
} IscissorsState;

/* The possible drawing state...  */
typedef enum
{
  DRAW_NOTHING = 0x0,
  DRAW_CURRENT_SEED = 0x1,
  DRAW_CURVE        = 0x2,
  DRAW_ACTIVE_CURVE = 0x4,
  DRAW_LIVEWIRE     = 0x8,
} IscissorsDraw;

/*  For oper_update & cursor_update  */
typedef enum
{
  ISCISSORS_OP_NONE,
  ISCISSORS_OP_SELECT,
  ISCISSORS_OP_MOVE_POINT,
  ISCISSORS_OP_ADD_POINT,
  ISCISSORS_OP_CONNECT,
  ISCISSORS_OP_IMPOSSIBLE
} IscissorsOps;

typedef struct _ICurve ICurve;

class TGimpIscissorsTool : public TObject
{
private:
       TCanvas           *FCanvas;

       IscissorsOps       op;

       int                x, y;         /*  upper left hand coordinate            */
       int                ix, iy;       /*  initial coordinates                   */
       int                nx, ny;       /*  new coordinates                       */

       TempBuf           *dp_buf;       /*  dynamic programming buffer            */

       ICurve            *livewire;     /*  livewire boundary curve               */

       ICurve            *curve1;       /*  1st curve connected to current point  */
       ICurve            *curve2;       /*  2nd curve connected to current point  */

       GQueue            *curves;       /*  the list of curves                    */

       bool               first_point;  /*  is this the first point?              */
       bool               connected;    /*  is the region closed?                 */
       bool               interactive;

       IscissorsState     state;        /*  state of iscissors                    */
       IscissorsDraw      draw;         /*  items to draw on a draw request       */

       Graphics::TBitmap *SourceBitmap;
       Graphics::TBitmap *gradient_map; /*  lazily filled gradient map            */
       Graphics::TBitmap *mask;      

       void __fastcall init(void);
       void __fastcall reset(void);
       
       void __fastcall find_max_gradient(Graphics::TBitmap *image,
         int *X, int *Y);

       void __fastcall calculate_curve(ICurve *curve);
       void __fastcall draw_curve(ICurve *curve);

       void __fastcall find_optimal_path(int x1, int y1, int x2, int y2,
         int xs, int ys);

       int __fastcall calculate_link (int X, int Y, unsigned int pixel, int link);

       bool __fastcall gradient_map_value(int X, int Y,
         unsigned char *grad, unsigned char *dir);

       GPtrArray * __fastcall plot_pixels(int x1, int y1, int xs, int ys,
         int xe, int ye);

       void __fastcall gimp_iscissors_tool_draw(void);

       bool __fastcall clicked_on_vertex(double X, double Y);
       bool __fastcall clicked_on_curve(double X, double Y);

       int __fastcall mouse_over_vertex(double X, double Y);

       GList * __fastcall mouse_over_curve(double X, double Y);

       void __fastcall iscissors_convert(void);

       
public:
       __fastcall TGimpIscissorsTool(Graphics::TBitmap *image,
         TCanvas *canvas);

       __fastcall ~TGimpIscissorsTool(void);

       void __fastcall button_press(TObject *Sender, TMouseButton Button,
         TShiftState Shift, int X, int Y);

       void __fastcall motion(TObject *Sender, TShiftState Shift, int X, int Y);

       void __fastcall button_release(TObject *Sender, TMouseButton Button,
         TShiftState Shift, int X, int Y);

       __property Graphics::TBitmap *Mask          = {read = mask};
       __property bool               IsConnected   = {read = connected};
       __property bool               IsInteractive = {read = interactive, write = interactive};
};

#endif /* CB_ISCISSORS_TOOL_H */


