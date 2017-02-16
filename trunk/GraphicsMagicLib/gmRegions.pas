{ Author: Ma Xiaoguang, Ma Xiaoming
  Date: 2011-04-03

  CopyRight(C) Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  This unit is used for creating hRGN. }

unit gmRegions;

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

{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows, Classes, Graphics, Controls;

type
  TgmRegionStyle = (gmrsNone,
                    gmrsSingleRow,
                    gmrsSingleColumn,
                    gmrsRectangular,
                    gmrsRoundRectangular,
                    gmrsElliptic,
                    gmrsPolygonal,
                    gmrsRegularPolygonal,
                    gmrsLasso);

  TgmRegion = class(TPersistent)
  private
    FRegion               : hRGN;
    FRegionStyle          : TgmRegionStyle;
    FScale                : Single;   // drawing scale
    FOffsetX              : Integer;
    FOffsetY              : Integer;
    FRegionDefineCompleted: Boolean;  // whether the region defintion process is completed
    FValidRegion          : Boolean;  // whether it is a valid region
    FLeftButtonPressed    : Boolean;

    FOldPenColor          : TColor;
    FOldPenStyle          : TPenStyle;
    FOldPenMode           : TPenMode;
    FOldPenWidth          : Integer;
    FOldBrushColor        : TColor;
    FOldBrushStyle        : TBrushStyle;

    FCanvas               : TCanvas;  // pointer to a canvas

    procedure SaveCanvasSettings;
    procedure SetCanvasForRegionOutline;
    procedure RestoreCanvasSettings;
  public
    constructor Create(const ACanvas: TCanvas);
    destructor Destroy; override;

    procedure DrawRegionOutline; virtual; abstract;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); virtual; abstract;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); virtual; abstract;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); virtual; abstract;

    property Region                 : hRGN           read FRegion;
    property RegionStyle            : TgmRegionStyle read FRegionStyle;
    property IsRegionDefineCompleted: Boolean        read FRegionDefineCompleted;
    property IsValidRegion          : Boolean        read FValidRegion;
    property Scale                  : Single         read FScale   write FScale;
    property OffsetX                : Integer        read FOffsetX write FOffsetX;
    property OffsetY                : Integer        read FOffsetY write FOffsetY;
  end;

  TgmSingleRowRegion = class(TgmRegion)
  private
    FRowWidth  : Integer;
    FStartPoint: TPoint;
    FEndPoint  : TPoint;
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    property RowWidth: Integer read FRowWidth write FRowWidth;
  end;

  TgmSingleColumnRegion = class(TgmRegion)
  private
    FColumnHeight: Integer;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    property ColumnHeight: Integer read FColumnHeight write FColumnHeight;
  end;

  TgmRectangularRegion = class(TgmRegion)
  private
    FStartPoint: TPoint;
    FEndPoint  : TPoint;
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
  end;

  TgmRoundRectangularRegion = class(TgmRegion)
  private
    FCornerRadius: Integer;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    property CornerRadius: Integer read FCornerRadius write FCornerRadius;
  end;

  TgmEllipticRegion = class(TgmRegion)
  private
    FStartPoint: TPoint;
    FEndPoint  : TPoint;
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
  end;

  TgmPolygonalRegion = class(TgmRegion)
  private
    FVertexArray  : array of TPoint;
    FVertexCount  : Integer;
    FPolygonClosed: Boolean;
  published
    procedure AddVertex(const AX, AY: Integer);
    function IsValidPolygon: Boolean;
  public
    constructor Create(const ACanvas: TCanvas);
    destructor Destroy; override;

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure DblClick(Sender: TObject);
    procedure ClosePolgonalRegion;

    property VertexCount: Integer read FVertexCount;
  end;

  TgmLassoRegion = class(TgmPolygonalRegion)
  public
    constructor Create(const ACanvas: TCanvas);

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
  end;

  TgmRegularPolygonalRegion = class(TgmRegion)
  private
    FStartPoint: TPoint;
    FEndPoint  : TPoint;
    FEdgeCount : Integer;

    procedure SetEdgeCount(const AValue: Integer);
  public
    constructor Create(const ACanvas: TCanvas);

    procedure DrawRegionOutline; override;

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;

    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;

    property EdgeCount: Integer read FEdgeCount write SetEdgeCount;
  end;

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmMath,
  gmPaintFuncs,
  gmConstants,
  gmCommonFuncs;

const
  REGION_OUTLINE_PEN_COLOR  : TColor      = clBlack;
  REGION_OUTLINE_PEN_STYLE  : TPenStyle   = psDot;
  REGION_OUTLINE_PEN_MODE   : TPenMode    = pmNotXor;
  REGION_OUTLINE_PEN_WIDTH  : Integer     = 1;
  REGION_OUTLINE_BRUSH_COLOR: TColor      = clBlack;
  REGION_OUTLINE_BRUSH_STYLE: TBrushStyle = bsClear;

//-- TgmRegion -----------------------------------------------------------------

constructor TgmRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create;

  FCanvas := ACanvas;

  FRegion                := 0;
  FRegionStyle           := gmrsNone;
  FScale                 := 1.0;
  FOffsetX               := 0;
  FOffsetY               := 0;
  FRegionDefineCompleted := False;
  FValidRegion           := False;
  FLeftButtonPressed     := False;

  FOldPenColor   := clBlack;
  FOldPenStyle   := psSolid;
  FOldPenMode    := pmCopy;
  FOldPenWidth   := 1;
  FOldBrushColor := clBlack;
  FOldBrushStyle := bsSolid;
end;

destructor TgmRegion.Destroy;
begin
  FCanvas := nil;

  DeleteObject(FRegion);

  inherited Destroy;
end;

procedure TgmRegion.SaveCanvasSettings;
begin
  if Assigned(FCanvas) then
  begin
    FOldPenColor   := FCanvas.Pen.Color;
    FOldPenStyle   := FCanvas.Pen.Style;
    FOldPenMode    := FCanvas.Pen.Mode;
    FOldPenWidth   := FCanvas.Pen.Width;
    FOldBrushColor := FCanvas.Brush.Color;
    FOldBrushStyle := FCanvas.Brush.Style;
  end;
end;

procedure TgmRegion.RestoreCanvasSettings;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Pen.Color   := FOldPenColor;
    FCanvas.Pen.Style   := FOldPenStyle;
    FCanvas.Pen.Mode    := FOldPenMode;
    FCanvas.Pen.Width   := FOldPenWidth;
    FCanvas.Brush.Color := FOldBrushColor;
    FCanvas.Brush.Style := FOldBrushStyle;
  end;
end;

procedure TgmRegion.SetCanvasForRegionOutline;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Pen.Color   := REGION_OUTLINE_PEN_COLOR;
    FCanvas.Pen.Style   := REGION_OUTLINE_PEN_STYLE;
    FCanvas.Pen.Mode    := REGION_OUTLINE_PEN_MODE;
    FCanvas.Pen.Width   := REGION_OUTLINE_PEN_WIDTH;
    FCanvas.Brush.Color := REGION_OUTLINE_BRUSH_COLOR;
    FCanvas.Brush.Style := REGION_OUTLINE_BRUSH_STYLE;
  end;
end;

//-- TgmSingleRowRegion --------------------------------------------------------

constructor TgmSingleRowRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle := gmrsSingleRow;
  FRowWidth    := 0;
  FStartPoint  := Point(0, 0);
  FEndPoint    := Point(0, 0);
end;

procedure TgmSingleRowRegion.DrawRegionOutline;
var
  LStartX, LStartY: Integer;
  LEndX, LEndY    : Integer;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartX := Round(FStartPoint.X * FScale + FOffsetX);
      LStartY := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndX   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndY   := Round(FEndPoint.Y   * FScale + FOffsetY);

      FCanvas.MoveTo(LStartX, LStartY);
      FCanvas.LineTo(LEndX, LEndY);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmSingleRowRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(0, AY);
    FEndPoint    := Point(FRowWidth - 1, AY);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmSingleRowRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FStartPoint := Point(0, AY);
    FEndPoint   := Point(FRowWidth - 1, AY);

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmSingleRowRegion.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
var
  LStaticPolygon: array [0 .. 3] of TPoint;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if FRowWidth <= 0 then
    begin
      Exit;
    end;
    
    LStaticPolygon[0] := Point(FStartPoint.X,   FStartPoint.Y);
    LStaticPolygon[1] := Point(FEndPoint.X + 1, FStartPoint.Y);
    LStaticPolygon[2] := Point(FEndPoint.X + 1, FStartPoint.Y + 1);
    LStaticPolygon[3] := Point(FStartPoint.X,   FStartPoint.Y + 1);

    DeleteObject(FRegion);
    
    FRegion := CreatePolygonRGN(LStaticPolygon, 4, ALTERNATE);

    FValidRegion := True;
  end;
end;

//-- TgmSingleColumnRegion -----------------------------------------------------

constructor TgmSingleColumnRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle  := gmrsSingleColumn;
  FColumnHeight := 0;
  FStartPoint   := Point(0, 0);
  FEndPoint     := Point(0, 0);
end;

procedure TgmSingleColumnRegion.DrawRegionOutline;
var
  LStartX, LStartY: Integer;
  LEndX, LEndY    : Integer;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartX := Round(FStartPoint.X * FScale + FOffsetX);
      LStartY := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndX   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndY   := Round(FEndPoint.Y   * FScale + FOffsetY);

      FCanvas.MoveTo(LStartX, LStartY);
      FCanvas.LineTo(LEndX, LEndY);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmSingleColumnRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(AX, 0);
    FEndPoint    := Point(AX, FColumnHeight - 1);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmSingleColumnRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FStartPoint := Point(AX, 0);
    FEndPoint   := Point(AX, FColumnHeight - 1);

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmSingleColumnRegion.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LStaticPolygon: array [0 .. 3] of TPoint;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if FColumnHeight <= 0 then
    begin
      Exit;
    end;
    
    LStaticPolygon[0] := Point(FStartPoint.X,     FStartPoint.Y);
    LStaticPolygon[1] := Point(FStartPoint.X + 1, FStartPoint.Y);
    LStaticPolygon[2] := Point(FStartPoint.X + 1, FEndPoint.Y + 1);
    LStaticPolygon[3] := Point(FStartPoint.X,     FEndPoint.Y + 1);

    DeleteObject(FRegion);
    
    FRegion := CreatePolygonRGN(LStaticPolygon, 4, ALTERNATE);

    FValidRegion := True;
  end;
end;

//-- TgmRectangularRegion ------------------------------------------------------

constructor TgmRectangularRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle := gmrsRectangular;
  FStartPoint  := Point(0, 0);
  FEndPoint    := Point(0, 0);
end;

procedure TgmRectangularRegion.DrawRegionOutline;
var
  LStartX, LStartY: Integer;
  LEndX, LEndY    : Integer;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartX := Round(FStartPoint.X * FScale + FOffsetX);
      LStartY := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndX   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndY   := Round(FEndPoint.Y   * FScale + FOffsetY);

      FCanvas.Rectangle(LStartX, LStartY, LEndX, LEndY);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmRectangularRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(AX, AY);
    FEndPoint    := Point(AX, AY);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmRectangularRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FEndPoint := Point(AX, AY);

    if ssShift in AShift then
    begin
      FEndPoint := CalculateRegularFigureEndPoint(FStartPoint, FEndPoint);
    end;

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmRectangularRegion.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if (FStartPoint.X = FEndPoint.X) or
       (FStartPoint.Y = FEndPoint.Y) then
    begin
      Exit;
    end;

    PointStandardizeOrder(FStartPoint, FEndPoint);

    DeleteObject(FRegion);

    FRegion := CreateRectRgn(FStartPoint.X, FStartPoint.Y,
                             FEndPoint.X, FEndPoint.Y);

    FValidRegion := True;
  end;
end;

//-- TgmRoundRectangularRegion -------------------------------------------------

constructor TgmRoundRectangularRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle  := gmrsRoundRectangular;
  FCornerRadius := 30;
  FStartPoint   := Point(0, 0);
  FEndPoint     := Point(0, 0);
end;

procedure TgmRoundRectangularRegion.DrawRegionOutline;
var
  LStartX, LStartY: Integer;
  LEndX, LEndY    : Integer;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartX := Round(FStartPoint.X * FScale + FOffsetX);
      LStartY := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndX   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndY   := Round(FEndPoint.Y   * FScale + FOffsetY);

      FCanvas.RoundRect(LStartX, LStartY, LEndX, LEndY, FCornerRadius, FCornerRadius);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmRoundRectangularRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(AX, AY);
    FEndPoint    := Point(AX, AY);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmRoundRectangularRegion.MouseMove(AShift: TShiftState;
  AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FEndPoint := Point(AX, AY);

    if ssShift in AShift then
    begin
      FEndPoint := CalculateRegularFigureEndPoint(FStartPoint, FEndPoint);
    end;

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmRoundRectangularRegion.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if (FStartPoint.X = FEndPoint.X) or
       (FStartPoint.Y = FEndPoint.Y) then
    begin
      Exit;
    end;

    PointStandardizeOrder(FStartPoint, FEndPoint);

    DeleteObject(FRegion);

    FRegion := CreateRoundRectRgn(FStartPoint.X, FStartPoint.Y,
                                  FEndPoint.X, FEndPoint.Y,
                                  FCornerRadius, FCornerRadius);

    FValidRegion := True;
  end;
end;

//-- TgmEllipticRegion ---------------------------------------------------------

constructor TgmEllipticRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle := gmrsElliptic;
  FStartPoint  := Point(0, 0);
  FEndPoint    := Point(0, 0);
end; 

procedure TgmEllipticRegion.DrawRegionOutline;
var
  LStartX, LStartY: Integer;
  LEndX, LEndY    : Integer;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartX := Round(FStartPoint.X * FScale + FOffsetX);
      LStartY := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndX   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndY   := Round(FEndPoint.Y   * FScale + FOffsetY);

      FCanvas.Ellipse(LStartX, LStartY, LEndX, LEndY);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmEllipticRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(AX, AY);
    FEndPoint    := Point(AX, AY);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmEllipticRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FEndPoint := Point(AX, AY);

    if ssShift in AShift then
    begin
      FEndPoint := CalculateRegularFigureEndPoint(FStartPoint, FEndPoint);
    end;

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmEllipticRegion.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if (FStartPoint.X = FEndPoint.X) or
       (FStartPoint.Y = FEndPoint.Y) then
    begin
      Exit;
    end;

    PointStandardizeOrder(FStartPoint, FEndPoint);

    DeleteObject(FRegion);

    FRegion := CreateEllipticRgn(FStartPoint.X, FStartPoint.Y,
                                 FEndPoint.X, FEndPoint.Y);

    FValidRegion := True;
  end;
end;

//-- TgmPolygonalRegion --------------------------------------------------------

constructor TgmPolygonalRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle   := gmrsPolygonal;
  FVertexArray   := nil;
  FVertexCount   := 0;
  FPolygonClosed := False;
end;

destructor TgmPolygonalRegion.Destroy;
begin
  if Length(FVertexArray) > 0 then
  begin
    SetLength(FVertexArray, 0);
    FVertexArray := nil;
  end;

  inherited Destroy;
end;

procedure TgmPolygonalRegion.AddVertex(const AX, AY: Integer);
var
  LIndex: Integer;
begin
  LIndex := FVertexCount;
  
  Inc(FVertexCount);
  SetLength(FVertexArray, FVertexCount);

  FVertexArray[LIndex] := Point(AX, AY);
end;

function TgmPolygonalRegion.IsValidPolygon: Boolean;
var
  i, LLastIndex: Integer;
  LMinX, LMinY : Integer;
  LMaxX, LMaxY : Integer;
  LTempPoint   : TPoint;
  LTempBmp     : TBitmap32;
  LVertexArray : array of TPoint;
  LBits        : PColor32;
begin
  Result := False;

  if FPolygonClosed then
  begin
    LMinX :=  1000000;
    LMinY :=  1000000;
    LMaxX := -1000000;
    LMaxY := -1000000;

    LLastIndex := FVertexCount - 1;

    // get bounding box of the polygon
    for i := 0 to LLastIndex do
    begin
      LTempPoint := FVertexArray[i];

      LMinX := Min(LMinX, LTempPoint.X);
      LMaxX := Max(LMaxX, LTempPoint.X);
      LMinY := Min(LMinY, LTempPoint.Y);
      LMaxY := Max(LMaxY, LTempPoint.Y);
    end;

    LTempBmp := TBitmap32.Create;
    try
      LTempBmp.SetSize(LMaxX - LMinX + 1, LMaxY - LMinY + 1);
      LTempBmp.Clear(clBlack32);

      SetLength(LVertexArray, FVertexCount);
      try
        // translate the polygon to top-left of the bounding box
        for i := 0 to LLastIndex do
        begin
          LVertexArray[i].X := FVertexArray[i].X - LMinX;
          LVertexArray[i].Y := FVertexArray[i].Y - LMinY;
        end;

        LTempBmp.Canvas.Brush.Color := clWhite;
        LTempBmp.Canvas.Brush.Style := bsSolid;

        LTempBmp.Canvas.Polygon(LVertexArray);
      finally
        SetLength(LVertexArray, 0);
        LVertexArray := nil;
      end;

      // test for whither a region is existed
      LBits := @LTempBmp.Bits[0];

      for i := 0 to (LTempBmp.Width * LTempBmp.Height - 1) do
      begin
        // only need to check for the blue channel of the pixel
        if (LBits^ and $FF) <> $0 then
        begin
          Result := True;
          Break;
        end;

        Inc(LBits);
      end;
      
    finally
      LTempBmp.Free;
    end;
  end;
end;

procedure TgmPolygonalRegion.DrawRegionOutline;
var
  i: Integer;
  p: TPoint;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      if FVertexCount > 1 then
      begin
        p.X := Round(FVertexArray[0].X * FScale + FOffsetX);
        p.Y := Round(FVertexArray[0].Y * FScale + FOffsetY);

        FCanvas.MoveTo(p.X, p.Y);

        for i := 1 to (FVertexCount - 1) do
        begin
          p.X := Round(FVertexArray[i].X * FScale + FOffsetX);
          p.Y := Round(FVertexArray[i].Y * FScale + FOffsetY);

          FCanvas.LineTo(p.X, p.Y);
        end;
      end;
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmPolygonalRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;

    if FVertexCount = 0 then
    begin
      AddVertex(AX, AY);
      AddVertex(AX, AY);
    end
    else
    begin
      DrawRegionOutline;  // erase the old outline
      AddVertex(AX, AY);
    end;

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmPolygonalRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
var
  LIndex: Integer;
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    LIndex               := FVertexCount - 1;
    FVertexArray[LIndex] := Point(AX, AY);
    
    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmPolygonalRegion.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
var
  LLastIndex, i : Integer;
  LStaticPolygon: array [0..100000] of TPoint;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed := False;

    if FPolygonClosed then
    begin
      FRegionDefineCompleted := True;

      if IsValidPolygon then
      begin
        LLastIndex := FVertexCount - 1;

        // convert the passed dynamic array to static array
        for i := 0 to LLastIndex do
        begin
          LStaticPolygon[i] := FVertexArray[i];
        end;

        FRegion := CreatePolygonRGN(LStaticPolygon, FVertexCount, ALTERNATE);

        FValidRegion := True;
      end;
    end;
  end;
end;

procedure TgmPolygonalRegion.DblClick(Sender: TObject);
var
  LLastIndex: Integer;
begin
  if FVertexCount > 2 then
  begin
    LLastIndex               := FVertexCount - 1;
    FVertexArray[LLastIndex] := FVertexArray[0];
    FPolygonClosed           := True;
  end;
end;

procedure TgmPolygonalRegion.ClosePolgonalRegion;
var
  LLastIndex, i : Integer;
  LStaticPolygon: array [0..100000] of TPoint;
begin
  if not FRegionDefineCompleted then
  begin
    FRegionDefineCompleted := True;

    if FVertexCount > 2 then
    begin
      LLastIndex := FVertexCount;

      Inc(FVertexCount);
      SetLength(FVertexArray, FVertexCount);

      FVertexArray[LLastIndex] := FVertexArray[0];
      FPolygonClosed           := True;

      if IsValidPolygon then
      begin
        // convert the passed dynamic array to static array
        for i := 0 to LLastIndex do
        begin
          LStaticPolygon[i] := FVertexArray[i];
        end;

        FRegion := CreatePolygonRGN(LStaticPolygon, FVertexCount, ALTERNATE);

        FValidRegion := True;
      end;
    end;
  end;
end;

//-- TgmLassoRegion ------------------------------------------------------------

constructor TgmLassoRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle := gmrsLasso;
end;

procedure TgmLassoRegion.MouseDown(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;

    AddVertex(AX, AY);
    DrawRegionOutline;  // draw outline

    FLeftButtonPressed := True;
  end;
end;

procedure TgmLassoRegion.MouseMove(AShift: TShiftState; AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    DrawRegionOutline;  // erase the last outline
    AddVertex(AX, AY);
    DrawRegionOutline;  // draw outline
  end;
end;

procedure TgmLassoRegion.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
var
  LLastIndex, i : Integer;
  LStaticPolygon: array [0..100000] of TPoint;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    // close the lasso
    if FVertexCount > 0 then
    begin
      AddVertex(FVertexArray[0].X, FVertexArray[0].Y);
      
      FPolygonClosed := True;
      
      if IsValidPolygon then
      begin
        LLastIndex := FVertexCount - 1;

        // convert the passed dynamic array to static array
        for i := 0 to LLastIndex do
        begin
          LStaticPolygon[i] := FVertexArray[i];
        end;

        FRegion := CreatePolygonRGN(LStaticPolygon, FVertexCount, ALTERNATE);

        FValidRegion := True;
      end;
    end;
  end;
end; 

//-- TgmRegularPolygonalRegion -------------------------------------------------

constructor TgmRegularPolygonalRegion.Create(const ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FRegionStyle := gmrsRegularPolygonal;
  FStartPoint  := Point(0, 0);
  FEndPoint    := Point(0, 0);
  FEdgeCount   := 3;  // the default most less count
end;

procedure TgmRegularPolygonalRegion.SetEdgeCount(const AValue: Integer);
begin
  if AValue in [3..100] then
  begin
    FEdgeCount := AValue;
  end;
end;

procedure TgmRegularPolygonalRegion.DrawRegionOutline;
var
  LStartPoint: TPoint;
  LEndPoint  : TPoint;
begin
  if Assigned(FCanvas) then
  begin
    SaveCanvasSettings;
    try
      SetCanvasForRegionOutline;

      LStartPoint.X := Round(FStartPoint.X * FScale + FOffsetX);
      LStartPoint.Y := Round(FStartPoint.Y * FScale + FOffsetY);
      LEndPoint.X   := Round(FEndPoint.X   * FScale + FOffsetX);
      LEndPoint.Y   := Round(FEndPoint.Y   * FScale + FOffsetY);

      DrawRegularPolygon(FCanvas, LStartPoint, LEndPoint,
                         FEdgeCount, pmNotXor, DONOT_FILL_INSIDE);
    finally
      RestoreCanvasSettings;
    end;
  end;
end;

procedure TgmRegularPolygonalRegion.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
  begin
    FValidRegion := False;
    FStartPoint  := Point(AX, AY);
    FEndPoint    := Point(AX, AY);

    // draw outline
    DrawRegionOutline;

    FLeftButtonPressed := True;
  end;
end;

procedure TgmRegularPolygonalRegion.MouseMove(AShift: TShiftState;
  AX, AY: Integer);
begin
  if FLeftButtonPressed then
  begin
    // erase the last outline
    DrawRegionOutline;

    FEndPoint := Point(AX, AY);

    // draw outline
    DrawRegionOutline;
  end;
end;

procedure TgmRegularPolygonalRegion.MouseUp(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  LVertices     : array of TPoint;
  LStaticPolygon: array [0..100000] of TPoint;
  i, LHighIndex : Integer;
begin
  if FLeftButtonPressed then
  begin
    FLeftButtonPressed     := False;
    FRegionDefineCompleted := True;

    if (FStartPoint.X = FEndPoint.X) and
       (FStartPoint.Y = FEndPoint.Y) then
    begin
      Exit;
    end;

    if FEdgeCount in [3..100] then
    begin
      DeleteObject(FRegion);

      SetLength(LVertices, FEdgeCount + 1);
      try
        CalcRegularPolygonVertices(LVertices, FStartPoint, FEndPoint, FEdgeCount);

        LHighIndex := High(LVertices);
        if LHighIndex > 0 then
        begin
          // convert the passed dynamic array to static array
          for i := 0 to LHighIndex do
          begin
            LStaticPolygon[i] := LVertices[i];
          end;

          FRegion := CreatePolygonRGN(LStaticPolygon, LHighIndex, ALTERNATE);

          FValidRegion := True;
        end;
      finally
        SetLength(LVertices, 0);
        LVertices := nil;
      end;
    end;
  end;
end; 

end.
 