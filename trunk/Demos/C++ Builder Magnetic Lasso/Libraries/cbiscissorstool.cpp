/* Extracted from app\tools\gimpiscissorstool.c of GIMP 2.6.0
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

/* This tool is based on a paper from SIGGRAPH '95:
 *  "Intelligent Scissors for Image Composition", Eric N. Mortensen and
 *   William A. Barrett, Brigham Young University.
 *
 * thanks to Professor D. Forsyth for prompting us to implement this tool. */

/* Personal note: Dr. Barrett, one of the authors of the paper written above
 * is not only one of the most brilliant people I have ever met, he is an
 * incredible professor who genuinely cares about his students and wants them
 * to learn as much as they can about the topic.
 *
 * I didn't even notice I was taking a class from the person who wrote the
 * paper until halfway through the semester.
 *                                                   -- Rockwalrus
 */

/* The history of this implementation is lonog and varied.  It was
 * orignally done by Spencer and Peter, and worked fine in the 0.54
 * (motif only) release of GIMP.  Later revisions (0.99.something
 * until about 1.1.4) completely changed the algorithm used, until it
 * bore little resemblance to the one described in the paper above.
 * The 0.54 version of the algorithm was then forwards ported to 1.1.4
 * by Austin Donnelly.
 */

/* Livewire boundary implementation done by Laramie Leavitt */

#include <vcl.h>
#pragma hdrstop

#include <values.h>

#include "cbiscissorstool.h"
#include "cbtile.h"
#include "cbtempbuf.h"
#include "cbpaintfuncs.h"
#include "cbgimpmath.h"
#include "cbgimpdrawtool.h"
#include "cbgimpmathtypes.h"

#include "cbgtypes.h"
#include "cbgqueue.h"
#include "cbgmacros.h"


/*  defines  */
#define  MAX_GRADIENT     179.606  /* == sqrt (127^2 + 127^2) */
#define  GRADIENT_SEARCH  32       /* how far to look when snapping to an edge */
#define  TARGET_SIZE      25
#define  POINT_WIDTH      12       /* size (in pixels) of seed handles */
#define  EXTEND_BY        0.2      /* proportion to expand cost map by */
#define  FIXED            5        /* additional fixed size to expand cost map */
#define  MIN_GRADIENT     63       /* gradients < this are directionless */

#define  COST_WIDTH       2        /* number of bytes for each pixel in cost map  */

/* weight to give between gradient (_G) and direction (_D) */
#define  OMEGA_D          0.2
#define  OMEGA_G          0.8

/* sentinel to mark seed point in ?cost? map */
#define  SEED_POINT       9

/*  Functional defines  */
#define  PIXEL_COST(x)    ((x) >> 8)
#define  PIXEL_DIR(x)     ((x) & 0x000000ff)


struct _ICurve
{
  int        x1, y1;
  int        x2, y2;
  GPtrArray *points;
};

/*  static variables  */

/*  where to move on a given link direction  */
static const int move[8][2] =
{
  { 1,   0 },  /*  move to right  */
  { 0,   1 },  /*  move to bottom  */
  { -1,  1 },  /*  move to bottom-left  */
  {  1,  1 },  /*  move to bottom-right  */
  { -1,  0 },  /*  move to left  */
  {  0, -1 },  /*  move to top  */
  {  1, -1 },  /*  move to top-right  */
  { -1, -1 },  /*  move to top-left  */
};

/* IE:
 * '---+---+---`
 * | 7 | 5 | 6 |
 * +---+---+---+
 * | 4 |   | 0 |
 * +---+---+---+
 * | 2 | 1 | 3 |
 * `---+---+---'
 */

static const float horz_deriv[9] =
{
   1,  0, -1,
   2,  0, -2,
   1,  0, -1,
};

static const float vert_deriv[9] =
{
   1,  2,  1,
   0,  0,  0,
  -1, -2, -1,
};

static const float blur_32[9] =
{
   1,  1,  1,
   1, 24,  1,
   1,  1,  1,
};

static float  distance_weights[GRADIENT_SEARCH * GRADIENT_SEARCH];

static int    diagonal_weight[256];
static int    direction_value[256][4];

//------------------------------------------------------------------------------

static void iscissors_free_icurves(GQueue *curves)
{
  while (! g_queue_is_empty(curves))
    {
      ICurve *curve = (ICurve*)g_queue_pop_head(curves);

      if (curve->points)
        g_ptr_array_free (curve->points, TRUE);

      free(curve);
    }
}

//------------------------------------------------------------------------------

/* Called to fill in a newly referenced tile in the gradient map */
static void gradmap_tile_validate(Graphics::TBitmap *tm,
  Graphics::TBitmap *image)
{
  static bool first_gradient = TRUE;

  int x, y;
  int dw, dh;
  int sw, sh;
  int i, j;
  int b;
  int bytes;
  float gradient;
  unsigned char *gradmap;

  Graphics::TBitmap* BlurredBmp;
  Graphics::TBitmap* HorzDerivBmp;
  Graphics::TBitmap* VertDerivBmp;

  if (first_gradient)
  {
    int radius = GRADIENT_SEARCH >> 1;

    /*  compute the distance weights  */
    for (i = 0; i < GRADIENT_SEARCH; i++)
    {
      for (j = 0; j < GRADIENT_SEARCH; j++)
      {
        distance_weights[i * GRADIENT_SEARCH + j] =
          1.0 / (1 + sqrt((i-radius) * (i-radius) + (j-radius) * (j-radius)));
      }
    }

    first_gradient = FALSE;
  }

  switch (image->PixelFormat)
  {
  case pf8bit:
    bytes = 1;
    break;

  case pf16bit:
    bytes = 2;
    break;

  case pf24bit:
    bytes = 3;
    break;

  case pf32bit:
    bytes = 4;
    break;

  default:
    bytes = 0;
    break;
  }

  /*  Blur the source to get rid of noise  */
  BlurredBmp              = new Graphics::TBitmap();
  BlurredBmp->Width       = image->Width;
  BlurredBmp->Height      = image->Height;
  BlurredBmp->PixelFormat = image->PixelFormat;

  convolve_region(image, BlurredBmp, blur_32, 3, 32, GIMP_NORMAL_CONVOL, FALSE);

  /*  Get the horizontal derivative  */
  HorzDerivBmp              = new Graphics::TBitmap();
  HorzDerivBmp->Width       = image->Width;
  HorzDerivBmp->Height      = image->Height;
  HorzDerivBmp->PixelFormat = image->PixelFormat;

  convolve_region(BlurredBmp, HorzDerivBmp, horz_deriv, 3, 1,
                  GIMP_NEGATIVE_CONVOL, FALSE);

  /*  Get the vertical derivative  */

  VertDerivBmp              = new Graphics::TBitmap();
  VertDerivBmp->Width       = image->Width;
  VertDerivBmp->Height      = image->Height;
  VertDerivBmp->PixelFormat = image->PixelFormat;

  convolve_region(BlurredBmp, VertDerivBmp, vert_deriv, 3, 1,
                  GIMP_NEGATIVE_CONVOL, FALSE);

  /* calculate overall gradient */

  for (i = 0; i < BlurredBmp->Height; i++)
  {
    unsigned char *datah;
    unsigned char *datav;

    datah = (unsigned char *)HorzDerivBmp->ScanLine[i];
    datav = (unsigned char *)VertDerivBmp->ScanLine[i];

    gradmap = (unsigned char *)tm->ScanLine[i];

    for (j = 0; j < BlurredBmp->Width; j++)
    {
      signed char hmax = datah[0] - 128;
      signed char vmax = datav[0] - 128;

      for (b = 1; b < bytes; b++)
      {
        if ( abs(datah[b] - 128) > abs(hmax) )
          hmax = datah[b] - 128;

        if ( abs(datav[b] - 128) > abs(vmax) )
          vmax = datav[b] - 128;
      }

      if ( (i == 0) ||
           (j == 0) ||
           (i == (BlurredBmp->Height - 1)) ||
           (j == (BlurredBmp->Width -1)) )
      {
        gradmap[j * COST_WIDTH + 0] = 0;
        gradmap[j * COST_WIDTH + 1] = 255;
        goto contin;
      }

      /* 1 byte absolute magnitude first */
      gradient = sqrt(hmax * hmax + vmax * vmax);
      gradmap[j * COST_WIDTH] = gradient * 255 / MAX_GRADIENT;

      /* then 1 byte direction */
      if (gradient > MIN_GRADIENT)
      {
        float direction;

        if (!hmax)
          direction = (vmax > 0) ? G_PI_2 : -G_PI_2;
        else
          direction = atan( (double) vmax / (double) hmax );

        /* Scale the direction from between 0 and 254,
         *  corresponding to -PI/2, PI/2 255 is reserved for
         *  directionless pixels */

        gradmap[j * COST_WIDTH + 1] =
          (unsigned char)(254 * (direction + G_PI_2) / G_PI); 
      }
      else
      {
        gradmap[j * COST_WIDTH + 1] = 255; /* reserved for weak gradient */
      }
      

    contin:
      datah += bytes;
      datav += bytes;
    }
  }

  delete BlurredBmp;
  delete HorzDerivBmp;
  delete VertDerivBmp;
}

static Graphics::TBitmap * gradient_map_new (Graphics::TBitmap *image)
{
  Graphics::TBitmap *tm;

  tm              = new Graphics::TBitmap();
  tm->Width       = image->Width;
  tm->Height      = image->Height;
  tm->PixelFormat = pf16bit;

  gradmap_tile_validate(tm, image);

  return tm;
}

//------------------------------------------------------------------------------

__fastcall TGimpIscissorsTool::TGimpIscissorsTool(
  Graphics::TBitmap *image, TCanvas *canvas)
        : TObject()
{
  int i;

  for (i = 0; i < 256; i++)
  {
    /*  The diagonal weight array  */
    diagonal_weight[i] = (int) (i * G_SQRT2);

    /*  The direction value array  */
    direction_value[i][0] = (127 - abs(127 - i)) * 2;
    direction_value[i][1] = abs(127 - i) * 2;
    direction_value[i][2] = abs(191 - i) * 2;
    direction_value[i][3] = abs(63 - i) * 2;
  }

  /*  set the 256th index of the direction_values to the hightest cost  */
  direction_value[255][0] = 255;
  direction_value[255][1] = 255;
  direction_value[255][2] = 255;
  direction_value[255][3] = 255;

  if (image != NULL)
  {
    this->SourceBitmap         = new Graphics::TBitmap();
    this->SourceBitmap->Width  = image->Width;
    this->SourceBitmap->Height = image->Height;
    
    this->SourceBitmap->Assign(image);
    this->SourceBitmap->PixelFormat = image->PixelFormat;
  }
  else
  {
    this->SourceBitmap = NULL;
  }

  FCanvas = canvas;
  init();
}
//------------------------------------------------------------------------------

__fastcall TGimpIscissorsTool::~TGimpIscissorsTool(void)
{
  if (this->SourceBitmap)
    this->SourceBitmap->Free();

  if (this->gradient_map)
    this->gradient_map->Free();

  if (this->mask)
    this->mask->Free();

  if (this->curves)
    g_queue_free(this->curves);
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::init(void)
{
  op           = ISCISSORS_OP_NONE;
  dp_buf       = NULL;
  curves       = g_queue_new();
  draw         = DRAW_NOTHING;
  state        = NO_ACTION;
  gradient_map = NULL;
  mask         = NULL;
  livewire     = NULL;
  interactive  = FALSE;

  reset();
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::reset(void)
{
  /*  Free and reset the curve list  */
  iscissors_free_icurves(curves);

  curve1      = NULL;
  curve2      = NULL;
  first_point = TRUE;
  connected   = FALSE;
  draw        = DRAW_NOTHING;
  state       = NO_ACTION;

  /*  Reset the dp buffers  */
  if (dp_buf)
  {
    temp_buf_free(dp_buf);
    dp_buf = NULL;
  }
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::button_press(TObject *Sender,
  TMouseButton Button, TShiftState Shift, int X, int Y)
{
  this->x = RINT(X);
  this->y = RINT(Y);

  switch (this->state)
  {
  case NO_ACTION:
    this->state = SEED_PLACEMENT;
    this->draw  = DRAW_CURRENT_SEED;

    if ( ! Shift.Contains(ssShift) )
    {
      find_max_gradient(this->SourceBitmap, &this->x, &this->y);
    }

    this->x = CLAMP(this->x, 0, this->SourceBitmap->Width - 1);
    this->y = CLAMP(this->y, 0, this->SourceBitmap->Height - 1);

    this->ix = this->x;
    this->iy = this->y;

    gimp_iscissors_tool_draw();
    break;

  default:
    /*  Check if the mouse click occurred on a vertex or the curve itself  */
    if ( clicked_on_vertex(X, Y) )
    {
      this->nx    = this->x;
      this->ny    = this->y;
      this->state = SEED_ADJUSTMENT;

      this->draw = DRAW_CURVE | DRAW_ACTIVE_CURVE;
      
      gimp_iscissors_tool_draw();
    }
    else if (! this->connected)
    {
      /*  if we're not connected, we're adding a new point  */

      /*  pause the tool, but undraw nothing  */
      this->draw = DRAW_NOTHING;
      gimp_iscissors_tool_draw();

      this->state = SEED_PLACEMENT;
      this->draw  = DRAW_CURRENT_SEED;

      if (this->interactive)
        this->draw |= DRAW_LIVEWIRE;

      gimp_iscissors_tool_draw();
    }
    break;
  }
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::motion(TObject *Sender, TShiftState Shift,
  int X, int Y)
{
  if (this->state == NO_ACTION)
    return;

  if (this->state == SEED_PLACEMENT)
  {
    this->draw = DRAW_CURRENT_SEED;

    if (this->interactive)
      this->draw |= DRAW_LIVEWIRE;
  }
  else if (this->state == SEED_ADJUSTMENT)
  {
    this->draw = DRAW_ACTIVE_CURVE;
  }

  this->gimp_iscissors_tool_draw();

  this->x = RINT(X);
  this->y = RINT(Y);

  switch (this->state)
  {
  case SEED_PLACEMENT:
    /*  Hold the shift key down to disable the auto-edge snap feature  */
    if (! Shift.Contains(ssShift))
    {
      find_max_gradient(this->SourceBitmap, &this->x, &this->y);
    }

    this->x = CLAMP(this->x, 0, this->SourceBitmap->Width - 1);
    this->y = CLAMP(this->y, 0, this->SourceBitmap->Height - 1);

    if (this->first_point)
    {
      this->ix = this->x;
      this->iy = this->y;
    }
    break;

  case SEED_ADJUSTMENT:
    /*  Move the current seed to the location of the cursor  */
    if (! Shift.Contains(ssShift))
    {
      find_max_gradient(this->SourceBitmap, &this->x, &this->y);
    }

    this->x = CLAMP(this->x, 0, this->SourceBitmap->Width - 1);
    this->y = CLAMP(this->y, 0, this->SourceBitmap->Height - 1);

    this->nx = this->x;
    this->ny = this->y;
    break;

  default:
    break;
  }

  this->gimp_iscissors_tool_draw();
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::button_release(TObject *Sender,
  TMouseButton Button, TShiftState Shift, int X, int Y)
{
  /* Make sure X didn't skip the button release event -- as it's known
   * to do
   */
  if (this->state == WAITING)
    return;

  /*  Undraw everything  */
  switch (this->state)
  {
  case SEED_PLACEMENT:
    this->draw = DRAW_CURVE | DRAW_CURRENT_SEED;

    if (this->interactive)
      this->draw |= DRAW_LIVEWIRE;
      
    break;

  case SEED_ADJUSTMENT:
    this->draw = DRAW_CURVE | DRAW_ACTIVE_CURVE;
    break;

  default:
    break;
  }

  gimp_iscissors_tool_draw();

  /*  Progress to the next stage of intelligent selection  */
  switch (this->state)
  {
  case SEED_PLACEMENT:
    /*  Add a new icurve  */
    if (! this->first_point)
    {
      /*  Determine if we're connecting to the first point  */
      if (! g_queue_is_empty(this->curves))
      {
        ICurve *curve = (ICurve *)g_queue_peek_head(this->curves);

        if (gimp_draw_tool_on_handle(this->x, this->y,
                                     GIMP_HANDLE_CIRCLE,
                                     curve->x1, curve->y1,
                                     POINT_WIDTH, POINT_WIDTH,
                                     GTK_ANCHOR_CENTER,
                                     FALSE))
        {
          this->x         = curve->x1;
          this->y         = curve->y1;
          this->connected = TRUE;
        }
      }

      /*  Create the new curve segment  */
      if ( (this->ix != this->x) || (this->iy != this->y) )
      {
        ICurve *curve = (ICurve *)malloc(sizeof(ICurve));

        curve->x1 = this->ix;
        curve->y1 = this->iy;
        
        this->ix = curve->x2 = this->x;
        this->iy = curve->y2 = this->y;

        curve->points = NULL;

        g_queue_push_tail(this->curves, curve);

        calculate_curve(curve);
      }
    }
    else /* this was our first point */
    {
      this->first_point = FALSE;
    }
    
    break;

  case SEED_ADJUSTMENT:
    /*  recalculate both curves  */
    if (this->curve1)
    {
      this->curve1->x1 = this->nx;
      this->curve1->y1 = this->ny;
      
      calculate_curve(this->curve1);
    }

    if (this->curve2)
    {
      this->curve2->x2 = this->nx;
      this->curve2->y2 = this->ny;

      calculate_curve(this->curve2);
    }

    break;

  default:
    break;
  }

  this->state = WAITING;

  /*  Draw only the boundary  */
  this->draw = DRAW_CURVE;
  
  gimp_iscissors_tool_draw();

  /*  convert the curves into a region  */
  if (this->connected)
    iscissors_convert();  
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::find_max_gradient(Graphics::TBitmap *image,
  int *X, int *Y)
{
  int   radius;
  int   i, j;
  int   endx, endy;
  int   cx, cy;
  int   x1, y1, x2, y2;
  void *pr;
  float max_gradient;

  /* Initialise the gradient map tile manager for this image if we
   * don't already have one. */
  if (! this->gradient_map)
  {
    this->gradient_map = gradient_map_new(image);
  }

  radius = GRADIENT_SEARCH >> 1;

  /*  calculate the extent of the search  */
  cx = (int)CLAMP(*X, 0, image->Width);
  cy = (int)CLAMP(*Y, 0, image->Height);
  x1 = (int)CLAMP(cx - radius, 0, image->Width);
  y1 = (int)CLAMP(cy - radius, 0, image->Height);
  x2 = (int)CLAMP(cx + radius, 0, image->Width);
  y2 = (int)CLAMP(cy + radius, 0, image->Height);

  /*  calculate the factor to multiply the distance from the cursor by  */
  max_gradient = 0;
  *X = cx;
  *Y = cy;

  /*  Find the point of max gradient  */
  for (i = y1; i < y2; i++)
  {
    unsigned char *gradient;

    gradient = (unsigned char *)this->gradient_map->ScanLine[i];

    for (j = x1; j < x2; j++)
    {
      float g = gradient[j * COST_WIDTH];

      g *= distance_weights[(i-y1) * GRADIENT_SEARCH + (j-x1)];

      if (g > max_gradient)
      {
        max_gradient = g;

        *X = j;
        *Y = i;
      }
    }
  }
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::calculate_curve(ICurve *curve)
{
  int X, Y, dir;
  int xs, ys, xe, ye;
  int x1, y1, x2, y2;
  int width, height;
  int ewidth, eheight;


  /*  Calculate the lowest cost path from one vertex to the next as specified
   *  by the parameter "curve".
   *    Here are the steps:
   *      1)  Calculate the appropriate working area for this operation
   *      2)  Allocate a temp buf for the dynamic programming array
   *      3)  Run the dynamic programming algorithm to find the optimal path
   *      4)  Translate the optimal path into pixels in the icurve data
   *            structure.
   */

  /*  Get the bounding box  */
  xs = CLAMP(curve->x1, 0, this->SourceBitmap->Width  - 1);
  ys = CLAMP(curve->y1, 0, this->SourceBitmap->Height - 1);
  xe = CLAMP(curve->x2, 0, this->SourceBitmap->Width  - 1);
  ye = CLAMP(curve->y2, 0, this->SourceBitmap->Height - 1);

  x1 = min(xs, xe);
  y1 = min(ys, ye);
  x2 = max(xs, xe) + 1;  /*  +1 because if xe = 199 & xs = 0, x2 - x1, width = 200  */
  y2 = max(ys, ye) + 1;

  /*  expand the boundaries past the ending points by
   *  some percentage of width and height.  This serves the following purpose:
   *  It gives the algorithm more area to search so better solutions
   *  are found.  This is particularly helpful in finding "bumps" which
   *  fall outside the bounding box represented by the start and end
   *  coordinates of the "curve".
   */

  ewidth  = (x2 - x1) * EXTEND_BY + FIXED;
  eheight = (y2 - y1) * EXTEND_BY + FIXED;

  if (xe >= xs)
    x2 += CLAMP(ewidth, 0, this->SourceBitmap->Width - x2);
  else
    x1 -= CLAMP(ewidth, 0, x1);

  if (ye >= ys)
    y2 += CLAMP(eheight, 0, this->SourceBitmap->Height - y2);
  else
    y1 -= CLAMP(eheight, 0, y1);

  /* blow away any previous points list we might have */
  if (curve->points)
  {
    g_ptr_array_free(curve->points, TRUE);
    curve->points = NULL;
  }

  /*  If the bounding box has width and height...  */
  if ( (x2 - x1) && (y2 - y1) )
  {
    width  = (x2 - x1);
    height = (y2 - y1);

    /* Initialise the gradient map tile manager for this image if we
     * don't already have one. */
    if (! this->gradient_map)
      this->gradient_map = gradient_map_new(this->SourceBitmap);

    /*  allocate the dynamic programming array  */
    this->dp_buf = temp_buf_resize(this->dp_buf, 4, x1, y1, width, height);

    /* the following four lines are needed, added by ourselves */
    xs = CLAMP(xs, x1, x2 - 1);
    ys = CLAMP(ys, y1, y2 - 1);
    xe = CLAMP(xe, x1, x2 - 1);
    ye = CLAMP(ye, y1, y2 - 1);

    /*  find the optimal path of pixels from (x1, y1) to (x2, y2)  */
    find_optimal_path(x1, y1, x2, y2, xs, ys);

    /*  get a list of the pixels in the optimal path  */
    curve->points = plot_pixels(x1, y1, xs, ys, xe, ye);
  }
  else if ( (x2 - x1) == 0 ) /*  If the bounding box has no width  */
  {
    /*  plot a vertical line  */
    Y   = ys;
    dir = (ys > ye) ? -1 : 1;

    curve->points = g_ptr_array_new();

    while (Y != ye)
    {
      int *p = (int*)malloc(sizeof(int));

      *p = (Y << 16) + xs;

      g_ptr_array_add( curve->points, GINT_TO_POINTER((Y << 16) + xs) );
      //g_ptr_array_add(curve->points, p);
      Y += dir;
    }
  }
  else if ( (y2 - y1) == 0 ) /*  If the bounding box has no height  */
  {
    /*  plot a horizontal line  */
    X   = xs;
    dir = (xs > xe) ? -1 : 1;

    curve->points = g_ptr_array_new();

    while (X != xe)
    {
      int *p = (int*)malloc(sizeof(int));

      *p = (ys << 16) + X;
      //g_ptr_array_add(curve->points, GINT_TO_POINTER((ys << 16) + X) );
      g_ptr_array_add( curve->points, p );
      X += dir;
    }
  }
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::draw_curve(ICurve *curve)
{
  GimpVector2  *points;
  void        **point;
  int           i, len;

  if (! curve->points)
    return;

  len = curve->points->len;

  points = (GimpVector2 *)malloc( sizeof(GimpVector2) * len );

  for (i = 0, point = curve->points->pdata; i < len; i++, point++)
  {
    unsigned int *coords = (unsigned int *)*point;

    points[i].x = (*coords) & 0x0000ffff;
    points[i].y = (*coords) >> 16;
  }

  gimp_draw_tool_draw_lines(this->FCanvas, points, len, FALSE, FALSE);

  free(points); 
}
//------------------------------------------------------------------------------

#define  PACK(x, y) ((((y) & 0xff) << 8) | ((x) & 0xff))

#define OFFSET(pixel) ((signed char)((pixel) & 0xff) + \
  ((signed char)(((pixel) & 0xff00) >> 8)) * dp_buf->width)

void __fastcall TGimpIscissorsTool::find_optimal_path(int x1, int y1,
  int x2, int y2, int xs, int ys)
{
  int           i, j, k;
  int           X, Y;
  int           link;
  int           linkdir;
  int           dirx, diry;
  int           min_cost;
  int           new_cost;
  int           offset;
  int           cum_cost[8];
  int           link_cost[8];
  int           pixel_cost[8];
  unsigned int  pixel[8];
  unsigned int *data;
  unsigned int *d;



  /*  initialize the dynamic programming buffer  */
  data = (unsigned int *)temp_buf_data_clear(this->dp_buf);

  /*  what directions are we filling the array in according to?  */
  dirx    = (xs - x1 == 0) ? 1 : -1;
  diry    = (ys - y1 == 0) ? 1 : -1;
  linkdir = (dirx * diry);

  Y = ys;

  for (i = 0; i < dp_buf->height; i++)
  {
    X = xs;

    d = data + (Y - y1) * dp_buf->width + (X - x1);

    for (j = 0; j < dp_buf->width; j++)
    {
      min_cost = MAXINT;

      /* pixel[] array encodes how to get to a neigbour, if possible.
       * 0 means no connection (eg edge).
       * Rest packed as bottom two bytes: y offset then x offset.
       * Initially, we assume we can't get anywhere. */
      for (k = 0; k < 8; k++)
        pixel[k] = 0;

      /*  Find the valid neighboring pixels  */
      /*  the previous pixel  */
      if (j)
        pixel[((dirx == 1) ? 4 : 0)] = PACK(-dirx, 0);

      /*  the previous row of pixels  */
      if (i)
      {
        pixel[((diry == 1) ? 5 : 1)] = PACK(0, -diry);

        link = (linkdir == 1) ? 3 : 2;
        if (j)
          pixel[((diry == 1) ? (link + 4) : link)] = PACK(-dirx, -diry);

        link = (linkdir == 1) ? 2 : 3;
        if (j != dp_buf->width - 1)
          pixel[((diry == 1) ? (link + 4) : link)] = PACK(dirx, -diry);
      }

      /*  find the minimum cost of going through each neighbor to reach the
       *  seed point...
       */
      link = -1;
      for (k = 0; k < 8; k++)
      {
        if (pixel[k])
        {
          link_cost[k] = calculate_link( xs + j*dirx, ys + i*diry,
                                         pixel[k],
                                         ((k > 3) ? k - 4 : k) );

          offset        = OFFSET(pixel[k]);
          pixel_cost[k] = PIXEL_COST(d[offset]);
          cum_cost[k]   = pixel_cost[k] + link_cost[k];

          if (cum_cost[k] < min_cost)
          {
            min_cost = cum_cost[k];
            link     = k;
          }
        }
      }

      /*  If anything can be done...  */
      if (link >= 0)
      {
        /*  set the cumulative cost of this pixel and the new direction  */
        *d = (cum_cost[link] << 8) + link;

        /*  possibly change the links from the other pixels to this pixel...
         *  these changes occur if a neighboring pixel will receive a lower
         *  cumulative cost by going through this pixel.
         */
        for (k = 0; k < 8; k++)
        {
          if ( pixel[k] && (k != link) )
          {
            /*  if the cumulative cost at the neighbor is greater than
             *  the cost through the link to the current pixel, change the
             *  neighbor's link to point to the current pixel.
             */
            new_cost = link_cost[k] + cum_cost[link];
            if (pixel_cost[k] > new_cost)
            {
              /*  reverse the link direction   /-----------------------\ */
              offset    = OFFSET(pixel[k]);
              d[offset] = (new_cost << 8) + ((k > 3) ? k - 4 : k + 4);
            }
          }
        }
      }
      else if (!i && !j)
      {
        /*  Set the seed point  */
        *d = SEED_POINT;
      }

      /*  increment the data pointer and the x counter  */
      d += dirx;
      X += dirx;
    }

    /*  increment the y counter  */
    Y += diry;
  }
}
//------------------------------------------------------------------------------

int __fastcall TGimpIscissorsTool::calculate_link (int X, int Y,
  unsigned int pixel, int link)
{
  int value = 0;
  unsigned char grad1, dir1, grad2, dir2;

  if (! gradient_map_value(X, Y, &grad1, &dir1))
  {
    grad1 = 0;
    dir1  = 255;
  }

  /* Convert the gradient into a cost: large gradients are good, and
   * so have low cost. */
  grad1 = 255 - grad1;

  /*  calculate the contribution of the gradient magnitude  */
  if (link > 1)
    value += diagonal_weight[grad1] * OMEGA_G;
  else
    value += grad1 * OMEGA_G;

  /*  calculate the contribution of the gradient direction  */
  X += (signed char)(pixel & 0xff);
  Y += (signed char)((pixel & 0xff00) >> 8);

  if (! gradient_map_value(X, Y, &grad2, &dir2))
  {
    grad2 = 0;
    dir2  = 255;
  }

  value += (direction_value[dir1][link] + direction_value[dir2][link]) * OMEGA_D;

  return value;
}
//------------------------------------------------------------------------------

bool __fastcall TGimpIscissorsTool::gradient_map_value(int X, int Y,
  unsigned char *grad, unsigned char *dir)
{
  unsigned char *p;

  if ( (X != CLAMP(X, 0, this->gradient_map->Width  - 1)) ||
       (Y != CLAMP(Y, 0, this->gradient_map->Height - 1)) )
    return FALSE;

  p = (unsigned char *)this->gradient_map->ScanLine[Y];
  p += X * COST_WIDTH;

  *grad = p[0];
  *dir  = p[1];

  return TRUE;
}
//------------------------------------------------------------------------------

GPtrArray * __fastcall TGimpIscissorsTool::plot_pixels(int x1, int y1,
  int xs, int ys, int xe, int ye)
{
  int X, Y;
  unsigned int *coords;
  int           link;
  int           width;
  unsigned int *data;
  GPtrArray    *list;

  width = this->dp_buf->width;

  /*  Start the data pointer at the correct location  */
  data = (unsigned int *)temp_buf_data(this->dp_buf) + (ye - y1) * width + (xe - x1);

  X = xe;
  Y = ye;

  list = g_ptr_array_new();

  while (TRUE)
  {
    coords = (unsigned int *)malloc( sizeof(unsigned int) );
    *coords = (Y << 16) + X;

    g_ptr_array_add(list, coords);

    link = PIXEL_DIR(*data);
    if (link == SEED_POINT)
      return list;

    X    += move[link][0];
    Y    += move[link][1];
    data += move[link][1] * width + move[link][0];
  }

  /*  won't get here  */
  return NULL; 
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::gimp_iscissors_tool_draw(void)
{
  /*  Draw the crosshairs target if we're placing a seed  */
  if (this->draw & DRAW_CURRENT_SEED)
  {
    gimp_draw_tool_draw_handle(this->FCanvas,
                               GIMP_HANDLE_CROSS,
                               this->x, this->y,
                               TARGET_SIZE, TARGET_SIZE,
                               GTK_ANCHOR_CENTER,
                               FALSE);

    /* Draw a line boundary */
    if ( (! this->first_point) &&
         (! (this->draw & DRAW_LIVEWIRE)) )
    {
      gimp_draw_tool_draw_line(this->FCanvas,
                               this->ix, this->iy,
                               this->x, this->y,
                               FALSE);
    }
  }

  /* Draw the livewire boundary */
  if ( (this->draw & DRAW_LIVEWIRE) && (! this->first_point) )
  {
    /* See if the mouse has moved.  If so, create a new segment... */
    if ( (! this->livewire) ||
         ( (this->livewire) &&
           ((this->ix != this->livewire->x1) ||
            (this->x  != this->livewire->x2) ||
            (this->iy != this->livewire->y1) ||
            (this->y  != this->livewire->y2)) ) )
    {
      ICurve *curve = (ICurve *)malloc( sizeof(ICurve) );

      curve->x1     = this->ix;
      curve->y1     = this->iy;
      curve->x2     = this->x;
      curve->y2     = this->y;
      curve->points = NULL;

      if (this->livewire)
      {
        if (this->livewire->points)
          g_ptr_array_free(this->livewire->points, TRUE);

        free(this->livewire);

        this->livewire = NULL;
      }

      this->livewire = curve;
      calculate_curve(curve);
    }

    /*  plot the curve  */
    draw_curve(this->livewire);
  }

  if ( (this->draw & DRAW_CURVE) && (! this->first_point) )
  {
    GList *list;


    /*  Draw a point at the init point coordinates  */
    if (! this->connected)
    {
      gimp_draw_tool_draw_handle (this->FCanvas,
                                  GIMP_HANDLE_FILLED_CIRCLE,
                                  this->ix,
                                  this->iy,
                                  POINT_WIDTH,
                                  POINT_WIDTH,
                                  GTK_ANCHOR_CENTER,
                                  FALSE);
    }

    /*  Go through the list of icurves, and render each one...  */
    for (list = g_queue_peek_head_link(this->curves);
         list;
         list = g_list_next(list))
    {
      ICurve *curve = (ICurve *)list->data;

      if (this->draw & DRAW_ACTIVE_CURVE)
      {
        /*  don't draw curve1 at all  */
        if (curve == this->curve1)
          continue;
      }

      gimp_draw_tool_draw_handle (this->FCanvas,
                                  GIMP_HANDLE_FILLED_CIRCLE,
                                  curve->x1,
                                  curve->y1,
                                  POINT_WIDTH,
                                  POINT_WIDTH,
                                  GTK_ANCHOR_CENTER,
                                  FALSE);

      if (this->draw & DRAW_ACTIVE_CURVE)
      {
        /*  draw only the start handle of curve2  */
        if (curve == this->curve2)
          continue;
      }

      /*  plot the curve  */
      draw_curve(curve);
    }
  }

  if (this->draw & DRAW_ACTIVE_CURVE)
  {
    /*  plot both curves, and the control point between them  */
    if (this->curve1)
    {
      gimp_draw_tool_draw_line(this->FCanvas,
                               this->curve1->x2,
                               this->curve1->y2,
                               this->nx,
                               this->ny,
                               FALSE);
    }

    if (this->curve2)
    {
      gimp_draw_tool_draw_line(this->FCanvas,
                               this->curve2->x1,
                               this->curve2->y1,
                               this->nx,
                               this->ny,
                               FALSE);
    }

    gimp_draw_tool_draw_handle(this->FCanvas,
                               GIMP_HANDLE_FILLED_CIRCLE,
                               this->nx,
                               this->ny,
                               POINT_WIDTH,
                               POINT_WIDTH,
                               GTK_ANCHOR_CENTER,
                               FALSE);
  } 
}
//------------------------------------------------------------------------------

bool __fastcall TGimpIscissorsTool::clicked_on_vertex(double X, double Y)
{
  int curves_found = 0;

  curves_found = mouse_over_vertex(X, Y);

  if (curves_found > 1)
  {
    /*  undraw the curve  */
    this->draw = DRAW_CURVE;
    gimp_iscissors_tool_draw();

    return TRUE;
  }

  /*  if only one curve was found, the curves are unconnected, and
   *  the user only wants to move either the first or last point
   *  disallow this for now.
   */

  if (curves_found == 1)
    return FALSE;

  return clicked_on_curve(X, Y);
}
//------------------------------------------------------------------------------

bool __fastcall TGimpIscissorsTool::clicked_on_curve(double X, double Y)
{
  GList *list = mouse_over_curve(X, Y);

  /*  traverse through the list, getting back the curve segment's list
   *  element if the current cursor position is on a curve...
   *  If this occurs, replace the curve with two new curves,
   *  separated by a new vertex.
   */

  if (list)
  {
    ICurve *curve = (ICurve *)list->data;
    ICurve *new_curve;

    /*  undraw the curve  */
    this->draw = DRAW_CURVE;
    gimp_iscissors_tool_draw();

    /*  Create the new curve  */
    new_curve = (ICurve *)malloc( sizeof(ICurve) );

    new_curve->x2 = curve->x2;
    new_curve->y2 = curve->y2;

    new_curve->x1 = curve->x2 = this->x;
    new_curve->y1 = curve->y2 = this->y;

    new_curve->points = NULL;

    /*  Create the new link and supply the new curve as data  */
    g_queue_insert_after(this->curves, list, new_curve);

    this->curve1 = new_curve;
    this->curve2 = curve;

    return TRUE;
  }

  return FALSE;
}
//------------------------------------------------------------------------------

/* XXX need some scan-conversion routines from somewhere.  maybe. ? */

int __fastcall TGimpIscissorsTool::mouse_over_vertex(double X, double Y)
{
  GList *list;
  int    curves_found = 0;

  /*  traverse through the list, returning non-zero if the current cursor
   *  position is on an existing curve vertex.  Set the curve1 and curve2
   *  variables to the two curves containing the vertex in question
   */

  this->curve1 = this->curve2 = NULL;

  for (list = g_queue_peek_head_link(this->curves);
       list;
       list = g_list_next(list))
  {
    ICurve *curve = (ICurve *)list->data;

    if ( gimp_draw_tool_on_handle(X, Y,
                                  GIMP_HANDLE_CIRCLE,
                                  curve->x1, curve->y1,
                                  POINT_WIDTH, POINT_WIDTH,
                                  GTK_ANCHOR_CENTER,
                                  FALSE) )
    {
      this->curve1 = curve;

      if (curves_found++)
        return curves_found;
    }
    else if ( gimp_draw_tool_on_handle(X, Y,
                                       GIMP_HANDLE_CIRCLE,
                                       curve->x2, curve->y2,
                                       POINT_WIDTH, POINT_WIDTH,
                                       GTK_ANCHOR_CENTER,
                                       FALSE) )
    {
      this->curve2 = curve;

      if (curves_found++)
        return curves_found;
    }
  }

  return curves_found;
}
//------------------------------------------------------------------------------

GList * __fastcall TGimpIscissorsTool::mouse_over_curve(double X, double Y)
{
  GList *list;

  /*  traverse through the list, returning the curve segment's list element
   *  if the current cursor position is on a curve...
   */
  for (list = g_queue_peek_head_link(this->curves);
       list;
       list = g_list_next(list))
  {
    ICurve *curve = (ICurve *)list->data;
    void  **pt;
    int     len;

    pt  = curve->points->pdata;
    len = curve->points->len;

    while (len--)
    {
      unsigned int *coords = (unsigned int *)(*pt);
      int           tx, ty;

      pt++;
      tx = (*coords) & 0x0000ffff;
      ty = (*coords) >> 16;

      /*  Is the specified point close enough to the curve?  */
      if ( gimp_draw_tool_in_radius(tx, ty, X, Y, POINT_WIDTH / 2) )
      {
        return list;
      }
    }
  }

  return NULL;
}
//------------------------------------------------------------------------------

void __fastcall TGimpIscissorsTool::iscissors_convert(void)
{
  GList       *list;
  TPoint      *points = NULL;
  unsigned int n_total_points = 0;

  if (this->mask == NULL)
  {
    this->mask              = new Graphics::TBitmap();
    this->mask->Width       = this->SourceBitmap->Width;
    this->mask->Height      = this->SourceBitmap->Height;
    this->mask->PixelFormat = pf24bit;
  }

  this->mask->Canvas->Brush->Color = clBlack;
  this->mask->Canvas->FillRect(this->mask->Canvas->ClipRect);

  for (list = g_queue_peek_tail_link(this->curves);
       list;
       list = g_list_previous(list))
  {
    ICurve *icurve = (ICurve *)list->data;

    n_total_points += icurve->points->len;
  }

  points = (TPoint *)malloc( sizeof(TPoint) * n_total_points );
  n_total_points = 0;

  /* go over the curves in reverse order, adding the points we have */
  for (list = g_queue_peek_tail_link(this->curves);
       list;
       list = g_list_previous(list))
  {
    ICurve      *icurve = (ICurve *)list->data;
    void       **pt;
    unsigned int i, n_points;

    n_points = icurve->points->len;

    pt = icurve->points->pdata;
    
    for (i = 0; i < n_points; i++)
    {

      unsigned int *coords;

      coords = (unsigned int *)(*pt);

      points[n_total_points + i].x = (int)((*coords) & 0x0000ffff);
      points[n_total_points + i].y = (int)((*coords) >> 16);

      pt++;
    }

    n_total_points += n_points;
  }  

  HRGN aRgn = CreatePolygonRgn(points, n_total_points, ALTERNATE);

  this->mask->Canvas->Brush->Color = clWhite;
  FillRgn(this->mask->Canvas->Handle, aRgn, this->mask->Canvas->Brush->Handle);

  DeleteObject(aRgn);
}
//------------------------------------------------------------------------------

