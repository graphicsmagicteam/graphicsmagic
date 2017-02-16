{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit CommonDataModule;

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
  SysUtils, Classes, GR32_Image;

type
  TGMDataModule = class(TDataModule)
    bmp32lstInternalPatterns: TBitmap32List;
    bmp32lstBrushStrokes: TBitmap32List;
    bmp32lstLayers: TBitmap32List;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GMDataModule: TGMDataModule;

const

//-- Internal Patterns ---------------------------------------------------------

  INTERNAL_PATTERN_COUNT = 12;

  INTERNAL_PATTERN_NAME: array [0..11] of string =
    ('Bubbles',
     'Wrinkles',
     'Woven',
     'Wood',
     'Tie Dye',
     'Satin',
     'Optical Checkerboard',
     'Nebula',
     'Molecular',
     'Metal Landscape',
     'Herringbone 2',
     'Clouds');

//-- Internal Brush Strokes ----------------------------------------------------

  INTERNAL_STROKE_COUNT = 36;

  INTERNAL_STROKE_NAME: array [0..35] of String =
    ('HardRound1Pixels',
     'HardRound3Pixels',
     'HardRound5Pixels',
     'HardRound9Pixels',
     'HardRound13Pixels',
     'HardRound19Pixels',
     'SoftRound5Pixels',
     'SoftRound9Pixels',
     'SoftRound13Pixels',
     'SoftRound17Pixels',
     'SoftRound21Pixels',
     'SoftRound27Pixels',
     'SoftRound35Pixels',
     'SoftRound45Pixels',
     'SoftRound65Pixels',
     'SoftRound100Pixels',
     'SoftRound200Pixels',
     'SoftRound300Pixels',
     'Spatter12Pixels',
     'Spatter25Pixels',
     'Spatter28Pixels',
     'Spatter39Pixels',
     'Spatter46Pixels',
     'Spatter59Pixels',
     'Chalk10Pixels',
     'Chalk17Pixels',
     'Chalk23Pixels',
     'Chalk36Pixels',
     'Chalk44Pixels',
     'Chalk60Pixels',
     'Star15Pixels',
     'Star26Pixels',
     'Star33Pixels',
     'Star42Pixels',
     'Star55Pixels',
     'Star70Pixels');

//-- For Layer and Channels ----------------------------------------------------

  COLOR_BALANCE_LAYER_ICON_INDEX   = 2;
  BRIGHT_CONTRAST_LAYER_ICON_INDEX = 3;
  HLS_LAYER_ICON_INDEX             = 4;
  INVERT_LAYER_ICON_INDEX          = 5;
  THRESHOLD_LAYER_ICON_INDEX       = 6;
  POSTERIZE_LAYER_ICON_INDEX       = 7;
  CURVES_LAYER_ICON_INDEX          = 8;
  GRADIENT_MAP_LAYER_ICON_INDEX    = 9;
  TEXT_LAYER_INCON_INDEX           = 10;
  LEVELS_LAYER_ICON_INDEX          = 12;
  LAYER_MASK_CHAIN_BMP_INDEX       = 13;
  CHANNEL_MIXER_LAYER_ICON_INDEX   = 14;

implementation

{$R *.dfm}

end.
