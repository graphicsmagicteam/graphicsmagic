unit gmCrop;

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

// Update Date: 2016/12/27

{$WARN UNSAFE_TYPE OFF}

interface

uses
{ Standard }
  Windows, Graphics,
{ Graphics32 }
  GR32, GR32_Layers,
{ GraphicsMagic Lib }
  gmTypes;

type
  TgmCrop = class(TObject)
  private
    FResizeW           : Integer;
    FResizeH           : Integer;
    FShieldColor       : TColor32;
    FShieldLayer       : TBitmapLayer;
    FShieldCroppedArea : Boolean;
    FResized           : Boolean;
    FShieldOpacity     : Byte;

    function GetSheildWinColor: TColor;
    function GetCropAreaWidth: Integer;
    function GetCropAreaHeight: Integer;

    procedure SetCropAreaWidth(const AValue: Integer);
    procedure SetCropAreaHeight(const AValue: Integer);
  public
    FCropStart : TPoint;
    FCropEnd   : TPoint;

    constructor Create; overload;
    
    constructor Create(const AImageWidth, AImageHeight: Integer;
      ALayerCollection: TLayerCollection; const ALayerLocation: TFloatRect); overload;

    destructor Destroy; override;

    // which handle of the crop control boundary the mouse is over
    function GetHandleAtPoint(const AX, AY: Integer): TgmDrawingHandle; overload;
    function GetHandleAtPoint(const AX, AY: Integer; const ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle; overload;

    // determine whether the mouse pointer is in the cropped area
    function ContainsPoint(const ATestPoint: TPoint):  Boolean;

    procedure DrawShield;
    procedure DrawCropBorder(ACanvas: TCanvas; const AOffsetVector: TPoint); overload;
    procedure DrawCropBorder(ACanvas: TCanvas; const ACoordConvertFunc: TgmPointCoordConvertFunc = nil); overload;
    procedure DrawCropHandles(ACanvas: TCanvas; const AOffsetVector: TPoint); overload;
    procedure DrawCropHandles(ACanvas: TCanvas; const ACoordConvertFunc: TgmPointCoordConvertFunc = nil); overload;

    // make sure the starting crop point is always at upper left
    procedure StandardizeOrder;
    procedure Translate(const ATranslateVector: TPoint);
    procedure GetCropAreaSize(var AWidth, AHeight: Integer);

    property CropAreaWidth       : Integer  read GetCropAreaWidth   write SetCropAreaWidth;
    property CropAreaHeight      : Integer  read GetCropAreaHeight  write SetCropAreaHeight;
    property IsShieldCroppedArea : Boolean  read FShieldCroppedArea write FShieldCroppedArea;
    property IsResized           : Boolean  read FResized           write FResized;
    property ResizeW             : Integer  read FResizeW           write FResizeW;
    property ResizeH             : Integer  read FResizeH           write FResizeH;
    property ShieldColor32       : TColor32 read FShieldColor       write FShieldColor;
    property ShieldOpacity       : Byte     read FShieldOpacity     write FShieldOpacity;
    property ShieldWinColor      : TColor   read GetSheildWinColor;
  end;

// written by Gerd Platl
function OptimalCrop(src, dest: TBitmap32): TRect;

implementation

uses
{ Standard }
  Classes, Math,
{ externals }
  LineLibrary,
{ GraphicsMagic Lib }
  gmAlphaFuncs,
  gmPaintFuncs,
  gmConstants,
  gmCommonFuncs;

//*********************************************************
// cut bitmap to minimum size by Gerd Platl
// frame lines with same color as pixel[0,0] are cutted away
// input:    src: tBitmap32;   source bitmap
//           dest: tBitmap32;  destination bitmap
// output: the rect of cropped area 
//---------------------------------------------------------
function OptimalCrop(src, dest: TBitmap32): TRect;
var
  srcRect                : TRect;
  w,h, xi,yi, minX, maxX : Integer;
  differentPixel         : Boolean;
  linePixels             : PColor32Array;
  firstPixel             : TColor32;
begin
{$RANGECHECKS OFF}

  srcRect := src.BoundsRect;
  w       := src.Width;
  h       := src.Height;

  // pixel[0,0] is background pixel
  firstPixel := src.Pixel[0,0];
  minX       := w-1;
  maxX       := 0;

  // cut away bottom lines
  differentPixel := false;
  for yi := h-1 downto 1 do
  begin
    linePixels := pointer(src.ScanLine[yi]);
    for xi := 0 to w-1 do
    begin
      if linePixels[xi] <> firstPixel then
      begin
        differentPixel := true;
        minX := xi;
        maxX := xi;
        break;                           //-->
      end;
    end;

    if differentPixel then
    begin
      h := yi;                         // save last
      break;                           //-->
    end;
  end;

  // cut away top lines
  differentPixel := false;
  for yi := 0 to h-1 do
  begin
    linePixels := pointer(src.ScanLine[yi]);
    for xi := w-1 downto 0 do
    begin
      if linePixels[xi] <> firstPixel then
      begin
        differentPixel := true;

        if xi < minX then
        begin
          minX := xi;
        end;

        if xi > maxX then
        begin
          maxX := xi;
        end;
        
        break;                           //-->
      end;
    end;

    if differentPixel then
    begin
      break;                       //-->
    end;
  end;

  srcRect.Top    := yi;
  srcRect.Bottom := h+1;

  if minX > maxX then                  // no differences found
  begin
    dest.SetSize(1,1);                 // crop to 1*1 picture
    dest.pixel[0,0] := firstPixel;
    exit;                              //===>
  end;

  // cut away left and right lines
  for yi := srcRect.Top to srcRect.Bottom-1 do
  begin
    linePixels := pointer(src.ScanLine[yi]);

    // cut left lines
    for xi := 0 to minX - 1 do
    begin
      if linePixels[xi] <> firstPixel then
      begin
        minX := xi;                      // save left limit
        break;                           //-->
      end;
    end;

    // cut right side
    for xi := w - 1 downto maxX + 1 do
    begin
      if linePixels[xi] <> firstPixel then
      begin
        maxX := xi;                      // save right limit
        break;                           //--> check next line
      end;
    end;
  end;

  srcRect.Left  := minX;
  srcRect.Right := maxX+1;

  with srcRect do
  begin
    dest.SetSize (Right-Left, Bottom-Top);
  end;

  dest.Draw(0,0, srcRect, src);

  Result := srcRect;
  
{$RANGECHECKS ON}
end;

//-- TgmCrop -------------------------------------------------------------------

constructor TgmCrop.Create;
begin
  inherited;

  FCropStart         := Point(0, 0);
  FCropEnd           := Point(0, 0);
  FResizeW           := 0;
  FResizeH           := 0;
  FShieldColor       := clRed32;
  FShieldCroppedArea := True;
  FShieldOpacity     := 100;
  FResized           := False;
end;

constructor TgmCrop.Create(const AImageWidth, AImageHeight: Integer;
  ALayerCollection: TLayerCollection; const ALayerLocation: TFloatRect);
begin
  inherited Create;

  FCropStart         := Point(0, 0);
  FCropEnd           := Point(0, 0);
  FResizeW           := 0;
  FResizeH           := 0;
  FShieldColor       := clRed32;
  FShieldCroppedArea := True;
  FShieldOpacity     := 100;
  FResized           := False;

  FShieldLayer := TBitmapLayer.Create(ALayerCollection);

  with FShieldLayer do
  begin
    Bitmap.DrawMode    := dmBlend;
    Bitmap.MasterAlpha := 127;
    Bitmap.Width       := AImageWidth;
    Bitmap.Height      := AImageHeight;
    Scaled             := True;
    Location           := ALayerLocation;
  end;
end;

destructor TgmCrop.Destroy;
begin
  FShieldLayer.Free();
  inherited Destroy;
end;

function TgmCrop.ContainsPoint(const ATestPoint: TPoint): Boolean;
begin
  Result := Windows.PtInRect( Rect(FCropStart.X, FCropStart.Y, FCropEnd.X, FCropEnd.Y), ATestPoint );
end;

// The version of this procedure could drawing border in bitmap space.
procedure TgmCrop.DrawCropBorder(ACanvas: TCanvas; const AOffsetVector: TPoint);
var
  LPenColor, LBrushColor : TColor;
  LPenWidth              : Integer;
  LPenStyle              : TPenStyle;
  LPenMode               : TPenMode;
  LBrushStyle            : TBrushStyle;
  LStartPt, LEndPt       : TPoint;
begin
  GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                      LPenStyle, LPenMode, LBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Style   := psDot;
    Pen.Width   := 1;
    Pen.Mode    := pmNotXor;
    Brush.Color := clWhite;
    Brush.Style := bsClear;

    LStartPt := AddPoints(FCropStart, AOffsetVector);
    LEndPt   := AddPoints(FCropEnd,   AOffsetVector);

    Rectangle(LStartPt.X, LStartPt.Y, LEndPt.X, LEndPt.Y);
  end;

  SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                      LPenStyle, LPenMode, LBrushStyle);
end;

procedure TgmCrop.DrawCropBorder(ACanvas: TCanvas;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor, LBrushColor : TColor;
  LPenWidth              : Integer;
  LPenStyle              : TPenStyle;
  LPenMode               : TPenMode;
  LBrushStyle            : TBrushStyle;
  LStartPt, LEndPt       : TPoint;
begin
  GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                      LPenStyle, LPenMode, LBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Style   := psDot;
    Pen.Width   := 1;
    Pen.Mode    := pmNotXor;
    Brush.Color := clWhite;
    Brush.Style := bsClear;

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPt := ACoordConvertFunc(FCropStart);
      LEndPt   := ACoordConvertFunc(FCropEnd);
    end
    else
    begin
      LStartPt := FCropStart;
      LEndPt   := FCropEnd;
    end;

    Rectangle(LStartPt.X, LStartPt.Y, LEndPt.X, LEndPt.Y);
  end;

  SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                      LPenStyle, LPenMode, LBrushStyle);
end;

// The version of this procedure could drawing handles in bitmap space.
procedure TgmCrop.DrawCropHandles(ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  i, LCenterX, LCenterY          : Integer;
  LStartPoint, LEndPoint, LPoint : TPoint;
begin
  LStartPoint := AddPoints(FCropStart, AOffsetVector);
  LEndPoint   := AddPoints(FCropEnd,   AOffsetVector);
  LCenterX    := (LEndPoint.X + LStartPoint.X) div 2;
  LCenterY    := (LEndPoint.Y + LStartPoint.Y) div 2;

  for i := 0 to 7 do
  begin
    if i = 0 then
    begin
      LPoint := LStartPoint;
    end
    else if i = 1 then
    begin
      LPoint := LEndPoint;
    end
    else if i = 2 then
    begin
      LPoint := Point(LStartPoint.X, LEndPoint.Y);
    end
    else if i = 3 then
    begin
      LPoint := Point(LEndPoint.X, LStartPoint.Y);
    end
    else if i = 4 then
    begin
      LPoint := Point(LStartPoint.X, LCenterY);
    end
    else if i = 5 then
    begin
      LPoint := Point(LEndPoint.X, LCenterY);
    end
    else if i = 6 then
    begin
      LPoint := Point(LCenterX, LStartPoint.Y);
    end
    else if i = 7 then
    begin
      LPoint := Point(LCenterX, LEndPoint.Y);
    end;

    DrawHandle(ACanvas, LPoint, clBlack, clWhite, bsClear, pmNotXor, HANDLE_RADIUS);
  end;
end;

procedure TgmCrop.DrawCropHandles(ACanvas: TCanvas;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i, LCenterX, LCenterY          : Integer;
  LStartPoint, LEndPoint, LPoint : TPoint;
begin
  if Assigned(ACoordConvertFunc) then
  begin
    LStartPoint := ACoordConvertFunc(FCropStart);
    LEndPoint   := ACoordConvertFunc(FCropEnd);
  end
  else
  begin
    LStartPoint := FCropStart;
    LEndPoint   := FCropEnd;
  end;
  
  LCenterX := (LEndPoint.X + LStartPoint.X) div 2;
  LCenterY := (LEndPoint.Y + LStartPoint.Y) div 2;

  for i := 0 to 7 do
  begin
    if i = 0 then
    begin
      LPoint := LStartPoint;
    end
    else if i = 1 then
    begin
      LPoint := LEndPoint;
    end
    else if i = 2 then
    begin
      LPoint := Point(LStartPoint.X, LEndPoint.Y);
    end
    else if i = 3 then
    begin
      LPoint := Point(LEndPoint.X, LStartPoint.Y);
    end
    else if i = 4 then
    begin
      LPoint := Point(LStartPoint.X, LCenterY);
    end
    else if i = 5 then
    begin
      LPoint := Point(LEndPoint.X, LCenterY);
    end
    else if i = 6 then
    begin
      LPoint := Point(LCenterX, LStartPoint.Y);
    end
    else if i = 7 then
    begin
      LPoint := Point(LCenterX, LEndPoint.Y);
    end;

    DrawHandle(ACanvas, LPoint, clBlack, clWhite, bsClear, pmNotXor, HANDLE_RADIUS);
  end;
end;

procedure TgmCrop.DrawShield;
begin
  if FShieldCroppedArea then
  begin
    FShieldLayer.Bitmap.MasterAlpha := MulDiv(255, FShieldOpacity, 100);
    FShieldLayer.Bitmap.Clear(FShieldColor);

    ReplaceAlphaChannelWithNewValueRect( FShieldLayer.Bitmap, 0,
      Rect(FCropStart.X, FCropStart.Y, FCropEnd.X, FCropEnd.Y) );
  end
  else
  begin
    FShieldLayer.Bitmap.MasterAlpha := 0;
  end;
end;

function TgmCrop.GetCropAreaHeight: Integer;
begin
  Result := Abs(FCropEnd.Y - FCropStart.Y);
end;

procedure TgmCrop.GetCropAreaSize(var AWidth, AHeight: Integer);
begin
  AWidth  := Abs(FCropEnd.X - FCropStart.X);
  AHeight := Abs(FCropEnd.Y - FCropStart.Y);
end; 

function TgmCrop.GetCropAreaWidth: Integer;
begin
  Result := Abs(FCropEnd.X - FCropStart.X);
end;

// which handle of the crop control boundary the mouse is over
// parameter AX and AY should be in bitmap space
function TgmCrop.GetHandleAtPoint(const AX, AY: Integer): TgmDrawingHandle;
var
  LCenterX, LCenterY : Integer;
begin
  Result := dhNone;

  if (FCropStart.X = FCropEnd.X) and
     (FCropStart.Y = FCropEnd.Y) then
  begin
    Exit;
  end;

  LCenterX := (FCropStart.X + FCropEnd.X) div 2;
  LCenterY := (FCropStart.Y + FCropEnd.Y) div 2;

  if SquareContainsPoint( FCropStart, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxAy;
  end
  else
  if SquareContainsPoint( FCropEnd, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxBy;
  end
  else
  if SquareContainsPoint( Point(FCropStart.X, FCropEnd.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxBy;
  end
  else
  if SquareContainsPoint( Point(FCropEnd.X, FCropStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxAy;
  end
  else
  if SquareContainsPoint( Point(FCropStart.X, LCenterY), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhLeftHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(FCropEnd.X, LCenterY), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhRightHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(LCenterX, FCropStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAXBX;
  end
  else
  if SquareContainsPoint( Point(LCenterX, FCropEnd.Y ), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAXBX;
  end;
end;

function TgmCrop.GetHandleAtPoint(const AX, AY: Integer;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;
var
  LCropStart, LCropEnd : TPoint;
  LCenterX, LCenterY   : Integer;
begin
  Result := dhNone;

  if (FCropStart.X = FCropEnd.X) and
     (FCropStart.Y = FCropEnd.Y) then
  begin
    Exit;
  end;

  if Assigned(ACoordConvertFunc) then
  begin
    LCropStart := ACoordConvertFunc(FCropStart);
    LCropEnd   := ACoordConvertFunc(FCropEnd);
  end
  else
  begin
    LCropStart := FCropStart;
    LCropEnd   := FCropEnd;
  end;

  LCenterX := (LCropStart.X + LCropEnd.X) div 2;
  LCenterY := (LCropStart.Y + LCropEnd.Y) div 2;

  if SquareContainsPoint( LCropStart, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxAy;
  end
  else
  if SquareContainsPoint( LCropEnd, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxBy;
  end
  else
  if SquareContainsPoint( Point(LCropStart.X, LCropEnd.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxBy;
  end
  else
  if SquareContainsPoint( Point(LCropEnd.X, LCropStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxAy;
  end
  else
  if SquareContainsPoint( Point(LCropStart.X, LCenterY), HANDLE_RADIUS, Point(AX, AY)  ) then
  begin
    Result := dhLeftHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(LCropEnd.X, LCenterY), HANDLE_RADIUS, Point(AX, AY)  ) then
  begin
    Result := dhRightHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(LCenterX, LCropStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAXBX;
  end
  else
  if SquareContainsPoint( Point(LCenterX, LCropEnd.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAXBX;
  end;
end;

function TgmCrop.GetSheildWinColor: TColor;  
begin
  Result := WinColor(FShieldColor);
end;

procedure TgmCrop.SetCropAreaHeight(const AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FCropEnd.Y := FCropStart.Y + AValue;
  end;
end;

procedure TgmCrop.SetCropAreaWidth(const AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FCropEnd.X := FCropStart.X + AValue;
  end;
end;

procedure TgmCrop.StandardizeOrder;
var
  LTempA, LTempB : TPoint;
begin
  LTempA := FCropStart;
  LTempB := FCropEnd;

  // FCropStart is at the upper left.
  FCropStart.X := MinIntValue([LTempA.X, LTempB.X]);
  FCropStart.Y := MinIntValue([LTempA.Y, LTempB.Y]);
  
  // FCropEnd is at the lower right.
  FCropEnd.X := MaxIntValue([LTempA.X, LTempB.X]);
  FCropEnd.Y := MaxIntValue([LTempA.Y, LTempB.Y]);
end;

procedure TgmCrop.Translate(const ATranslateVector: TPoint);
begin
  FCropStart := AddPoints(FCropStart, ATranslateVector);
  FCropEnd   := AddPoints(FCropEnd,   ATranslateVector);
end;


end.
