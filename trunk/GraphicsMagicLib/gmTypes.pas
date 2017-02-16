unit gmTypes;

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
 * Update Date: August 29th, 2014
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

// Update Date: 2016/07/28

interface

uses
  Windows, Graphics, gmConstants;

type
{ Enumeration Types}

  TgmEdgeAction = (eaNone = -1,
                   eaZeroEdges,
                   eaClampEdges,
                   eaWrapEdges);

  TgmHueSaturationAdjustMode = (hsamHSL, hsamHSV);

  TgmOutputGraphicsFormat = (ogfNone,
                             ogfBMP,
                             ogfJPG,
                             ogfGIF,
                             ogfPNG,
                             ogfTIF,
                             ogfGMD);

  TgmColorMode         = (cmRGB, cmGrayscale);
  TgmColorSelector     = (csForeColor, csBackColor);
  TgmEditMode          = (emStandardMode, emQuickMaskMode);
  TgmFlipMode          = (fmNone, fmHorizontal, fmVertical);
  TgmThumbnailMode     = (tmSelection, tmImage); // for Color Selection and Replace Color commands
  TgmThumbnailSizeMode = (tsmSmall, tsmLarge);

  TgmRotateDirection   = (rdClockwise, rdCounterclockwise);
  TgmAppliedUnit       = (auInch, auCentimeter, auPoint, auPixel);
  TgmResolutionUnit    = (ruPixelsPerInch, ruPixelsPerCM);

  // channel types
  TgmChannelSelector   = (csRed, csGreen, csBlue, csGrayscale, csAlpha);
  TgmChannelSet        = set of TgmChannelSelector;

  TgmWorkingChannelType = (wctRGB,
                           wctRed,
                           wctGreen,
                           wctBlue,
                           wctAlpha,
                           wctQuickMask,
                           wctLayerMask);

  // the working state for selections, figures or shapes
  TgmDrawingState = (dsNotDrawing,
                     dsNewFigure,
                     dsStretchCorner,
                     dsTranslate);

  // type of end point "handles" for border of selected selection and figures
  TgmDrawingHandle = (dhNone,
                      dhAxAy,
                      dhBxBy,
                      dhAxBy,
                      dhBxAy,
                      dhLeftHalfAyBy,
                      dhRightHalfAyBy,
                      dhTopHalfAxBx,
                      dhBottomHalfAxBx,
                      dhLineStart,
                      dhLineEnd,
                      dhCurvePoint1,
                      dhCurvePoint2,
                      dhPolygonPoint);

  // used for Canvas Size command
  TgmAnchorDirection = (adTopLeft,
                        adTop,
                        adTopRight,
                        adLeft,
                        adCenter,
                        adRight,
                        adBottomLeft,
                        adBottom,
                        adBottomRight);
  
{ Pointer Types }

  // This is a function type for pointer to a routine to do something
  // when color mode is changed. }
  TgmColorModeChangedFunc = procedure (const AColorMode: TgmColorMode) of object;

  TgmPointCoordConvertFunc = function (const APoint: TPoint): TPoint of object;

  TgmSetEditModeProc = procedure (const AValue: TgmEditMode) of object;

  // Use SysUtils.pByteArray for 8-bit color
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0 .. MAX_PIXEL_COUNT - 1] of TRGBTriple;

  PColorArray = ^TColorArray;
  TColorArray = array of TColor;

  PArrayOfColor = ^TArrayOfColor;
  TArrayOfColor = array of TColor;

implementation

end.
