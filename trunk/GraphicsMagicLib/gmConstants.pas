{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmConstants;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Graphics;

const
  MAX_PIXEL_COUNT                      = 65536;
  RUBBER_BAND_PEN_COLOR  : TColor      = clBlack;
  RUBBER_BAND_PEN_STYLE  : TPenStyle   = psDot;
  RUBBER_BAND_PEN_WIDTH  : Integer     = 1;
  RUBBER_BAND_BRUSH_COLOR: TColor      = clBlack;
  RUBBER_BAND_BRUSH_STYLE: TBrushStyle = bsClear;
  IMAGE_BORDER_PIXEL     : Integer     = 1;
  MAX_OPEN_RECENT_COUNT  : Integer     = 10;  // maximum count of OpenRecent menu items
  HANDLE_RADIUS          : Integer     = 3;
  FIGURE_HANDLE_RADIUS   : Integer     = 2;   // radius of handle for selected figures
  FILL_INSIDE            : Boolean     = True;
  DONOT_FILL_INSIDE      : Boolean     = False;

  THUMBNAIL_SIZE_LARGE = 64;
  THUMBNAIL_SIZE_SMALL = 32;

  MAX_GHOST_FADE_INTERVAL = 50;
  MIN_GHOST_FADE_INTERVAL = 10;
  MAX_GHOST_OPAQUE_UPPER  = 255;
  MAX_GHOST_OPAQUE_LOWER  = 230;

implementation

end.
