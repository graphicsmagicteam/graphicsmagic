{ -- LICENSE -------------------------------------------------------------------

  This unit is written by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
  at 2011-02-23.

  Based on app\tools\gimpdrawtool.h and app\tools\gimpdrawtool.c of GIMP 2.6.0

  GIMP - The GNU Image Manipulation Program
  Copyright (C) 1995-2001 Spencer Kimball, Peter Mattis, and others.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit gmGimpDrawTool;

interface

uses
  Windows, Graphics, gmGtkEnums;

type
  TgmGimpHandleType = (ghtSquare,
                       ghtFilledSquare,
                       ghtCircle,
                       ghtFilledCircle,
                       ghtCross);

  function CalcDistanceSquare(const X1, Y1, X2, Y2: Integer): Integer;
  function CoordsInRadius(const X1, Y1, X2, Y2, ARadius: Integer): Boolean;

  function CoordsOnHandle(const AX, AY: Integer; const AType: TgmGimpHandleType;
    const AHandleX, AHandleY, AWidth, AHeight: Integer;
    const AAnchor: TGtkAnchorType): Boolean;

  procedure DrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer;
    const AOffsetX, AOffsetY: Integer; const AScale: Single);

  procedure DrawLines(const ACanvas: TCanvas; const APointArray: array of TPoint;
    const IsFilled: Boolean; const AOffsetX, AOffsetY: Integer;
    const AScale: Single); 

  procedure DrawRectangleByAnchor(const ACanvas: TCanvas;
    const IsFilled: Boolean; const AX, AY, AWidth, AHeight: Integer;
    const AAnchor: TGtkAnchorType; const AOffsetX, AOffsetY: Integer;
    const AScale: Single);

  procedure DrawArcByAnchor(const ACanvas: TCanvas; const IsFilled: Boolean;
    const AX, AY, AWidth, AHeight, AAngle1, AAngle2: Integer;
    const AAnchor: TGtkAnchorType; const AOffsetX, AOffsetY: Integer;
    const AScale: Single);

  procedure DrawCrossByAnchor(const ACanvas: TCanvas;
    const AX, AY, AWidth, AHeight: Integer; const AAnchor: TGtkAnchorType;
    const AOffsetX, AOffsetY: Integer; const AScale: Single);

  procedure DrawHandle(const ACanvas: TCanvas; const AType: TgmGimpHandleType;
    const AX, AY, AWidth, AHeight: Integer; const AAnchor: TGtkAnchorType;
    const AOffsetX, AOffsetY: Integer; const AScale: Single);

implementation

uses
{ Standard }
  Math,
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmGTypes;

{ private functions }

procedure ShiftToNorthWest(const X, Y: Integer;
  const AHandleWidth, AHandleHeight: Integer; const AAnchor: TGtkAnchorType;
  var ShiftedX, ShiftedY: Integer);
begin
  case AAnchor of
    gtkatCenter:
      begin
        ShiftedX := X - (AHandleWidth  shr 1);
        ShiftedY := Y - (AHandleHeight shr 1);
      end;

    gtkatNorth:
      begin
        ShiftedX := X - (AHandleWidth shr 1);
        ShiftedY := Y;
      end;

    gtkatNorthWest:
      begin
        ShiftedX := X;
        ShiftedY := Y;
      end;

    gtkatNorthEast:
      begin
        ShiftedX := X - AHandleWidth;
        ShiftedY := Y;
      end;

    gtkatSouth:
      begin
        ShiftedX := X - (AHandleWidth shr 1);
        ShiftedY := Y - AHandleHeight;
      end;

    gtkatSouthWest:
      begin
        ShiftedX := X;
        ShiftedY := Y - AHandleHeight;
      end;

    gtkatSouthEast:
      begin
        ShiftedX := X - AHandleWidth;
        ShiftedY := Y - AHandleHeight;
      end;

    gtkatWest:
      begin
        ShiftedX := X;
        ShiftedY := Y - (AHandleHeight shr 1);
      end;

    gtkatEast:
      begin
        ShiftedX := X - AHandleWidth;
        ShiftedY := Y - (AHandleHeight shr 1);
      end;
  end;
end;

procedure ShiftToCenter(const X, Y: Integer;
  const AHandleWidth, AHandleHeight: Integer; const AAnchor: TGtkAnchorType;
  var ShiftedX, ShiftedY: Integer);
begin
  case AAnchor of
    gtkatCenter:
      begin
        ShiftedX := X;
        ShiftedY := Y;
      end;

    gtkatNorth:
      begin
        ShiftedX := X;
        ShiftedY := Y + (AHandleHeight shr 1);
      end;

    gtkatNorthWest:
      begin
        ShiftedX := X + (AHandleWidth  shr 1);
        ShiftedY := Y + (AHandleHeight shr 1);
      end;

    gtkatNorthEast:
      begin
        ShiftedX := X - (AHandleWidth  shr 1);
        ShiftedY := Y + (AHandleHeight shr 1);
      end;

    gtkatSouth:
      begin
        ShiftedX := X;
        ShiftedY := Y - (AHandleHeight shr 1);
      end;

    gtkatSouthWest:
      begin
        ShiftedX := X + (AHandleWidth  shr 1);
        ShiftedY := Y - (AHandleHeight shr 1);
      end;

    gtkatSouthEast:
      begin
        ShiftedX := X - (AHandleWidth  shr 1);
        ShiftedY := Y - (AHandleHeight shr 1);
      end;

    gtkatWest:
      begin
        ShiftedX := X + (AHandleWidth shr 1);
        ShiftedY := Y;
      end;

    gtkatEast:
      begin
        ShiftedX := X - (AHandleWidth shr 1);
        ShiftedY := Y;
      end;
  end;
end; 

{ This function is more effective than CalcDistance()
  as it doesn't perform a sqrt(). Use this if you just need to compare
  distances. }
function CalcDistanceSquare(const X1, Y1, X2, Y2: Integer): Integer;
var
  dx, dy: Integer;
begin
  dx := X2 - X1;
  dy := Y2 - Y1;

  Result := dx * dx + dy * dy;
end;

function CoordsInRadius(const X1, Y1, X2, Y2, ARadius: Integer): Boolean;
begin
  Result := CalcDistanceSquare(X1, Y1, X2, Y2) < (ARadius * ARadius);
end;

function CoordsOnHandle(const AX, AY: Integer; const AType: TgmGimpHandleType;
  const AHandleX, AHandleY, AWidth, AHeight: Integer;
  const AAnchor: TGtkAnchorType): Boolean;
var
  LHandleTX, LHandleTY: Integer;
  LWidth              : Integer;
  dx, dy              : Integer;
begin
  Result := False;
  
  case AType of
    ghtSquare,
    ghtFilledSquare,
    ghtCross:
      begin
        ShiftToNorthWest(AHandleX, AHandleY, AWidth, AHeight, AAnchor, LHandleTX, LHandleTY);

        Result := (AX = Clamp(AX, LHandleTX, LHandleTX + AWidth)) and
                  (AY = Clamp(AY, LHandleTY, LHandleTY + AHeight));
      end;

    ghtCircle,
    ghtFilledCircle:
      begin
        ShiftToCenter(AHandleX, AHandleY, AWidth, AHeight, AAnchor, LHandleTX, LHandleTY);

        LWidth := AWidth;

        // FIXME
        if AWidth <> AHeight then
        begin
          LWidth := (AWidth + AHeight) div 2;
        end;

        LWidth := LWidth div 2;

        dx := LHandleTX - AX;
        dy := LHandleTY - AY;

        Result := ( (dx * dx) + (dy * dy) ) < (LWidth * LWidth);
      end;
  end;
end;

procedure DrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer;
  const AOffsetX, AOffsetY: Integer; const AScale: Single);
var
  tx1, ty1, tx2, ty2: Integer;
  LLineWidth        : Integer;
  LPenMode          : TPenMode;
  LPenColor         : TColor;
begin
  LLineWidth := ACanvas.Pen.Width;
  LPenMode   := ACanvas.Pen.Mode;
  LPenColor  := ACanvas.Pen.Color;

  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Mode  := pmNotXor;
  ACanvas.Pen.Color := clBlack;

  tx1 := Round(X1 * AScale + AOffsetX);
  ty1 := Round(Y1 * AScale + AOffsetY);
  tx2 := Round(X2 * AScale + AOffsetX);
  ty2 := Round(Y2 * AScale + AOffsetY);

  ACanvas.MoveTo(tx1, ty1);
  ACanvas.LineTo(tx2, ty2);

  ACanvas.Pen.Width := LLineWidth;
  ACanvas.Pen.Mode  := LPenMode;
  ACanvas.Pen.Color := LPenColor;
end;

procedure DrawLines(const ACanvas: TCanvas; const APointArray: array of TPoint;
  const IsFilled: Boolean; const AOffsetX, AOffsetY: Integer;
  const AScale: Single);
var
  i, LCount     : Integer;
  x1, y1, x2, y2: Integer;
begin
  if not Assigned(ACanvas) then
  begin
    Exit;
  end;

  LCount := Length(APointArray);

  if LCount <= 0 then
  begin
    Exit;
  end;
  
  if IsFilled then
  begin
    // DrawPolygon()
  end
  else
  begin
    for i := 0 to (LCount - 2) do
    begin
      x1 := APointArray[i].X;
      y1 := APointArray[i].Y;
      x2 := APointArray[i + 1].X;
      y2 := APointArray[i + 1].Y;

      DrawLine(ACanvas, x1, y1, x2, y2, AOffsetX, AOffsetY, AScale);
    end;
  end;
end;

procedure DrawRectangleByAnchor(const ACanvas: TCanvas; const IsFilled: Boolean;
  const AX, AY, AWidth, AHeight: Integer; const AAnchor: TGtkAnchorType;
  const AOffsetX, AOffsetY: Integer; const AScale: Single);
var
  tx, ty         : Integer;
  LWidth, LHeight: Integer;
  LPenMode       : TPenMode;
  LBrushStyle    : TBrushStyle;
  LBrushColor    : TColor;
  LPenColor      : TColor;
begin
  ShiftToNorthWest(AX, AY, AWidth, AHeight, AAnchor, tx, ty);

  LPenMode    := ACanvas.Pen.Mode;
  LPenColor   := ACanvas.Pen.Color;
  LBrushStyle := ACanvas.Brush.Style;
  LBrushColor := ACanvas.Brush.Color;

  ACanvas.Pen.Mode    := pmNOtXor;
  ACanvas.Brush.Color := clBlack;

  LWidth  := AWidth;
  LHeight := AHeight;

  if IsFilled then
  begin
    ACanvas.Brush.Style := bsSolid;
  end
  else
  begin
    LWidth  := LWidth  - 1;
    LHeight := LHeight - 1;

    ACanvas.Brush.Style := bsClear;
  end;

  tx      := Round(tx * AScale + AOffsetX);
  ty      := Round(ty * AScale + AOffsetY);
  LWidth  := Round(LWidth  * AScale);
  LHeight := Round(LHeight * AScale);

  ACanvas.Rectangle(tx, ty, tx + LWidth, ty + LHeight);

  ACanvas.Pen.Mode    := LPenMode;
  ACanvas.Pen.Color   := LPenColor;
  ACanvas.Brush.Style := LBrushStyle;
  ACanvas.Brush.Color := LBrushColor;
end;

procedure DrawArcByAnchor(const ACanvas: TCanvas; const IsFilled: Boolean;
  const AX, AY, AWidth, AHeight, AAngle1, AAngle2: Integer;
  const AAnchor: TGtkAnchorType; const AOffsetX, AOffsetY: Integer;
  const AScale: Single);
var
  tx, ty     : Integer;
  LXStartArc : Integer;
  LYStartArc : Integer;
  LXEndArc   : Integer;
  LYEndArc   : Integer;
  LWidth     : Integer;
  LHeight    : Integer;
  LPenMode   : TPenMode;
  LBrushStyle: TBrushStyle;
  LBrushColor: TColor;
  LPenColor  : TColor;
begin
  ShiftToNorthWest(AX, AY, AWidth, AHeight, AAnchor, tx, ty);

  LPenMode    := ACanvas.Pen.Mode;
  LPenColor   := ACanvas.Pen.Color;
  LBrushStyle := ACanvas.Brush.Style;
  LBrushColor := ACanvas.Brush.Color;

  ACanvas.Pen.Mode    := pmNotXor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clBlack;

  LWidth  := AWidth;
  LHeight := AHeight;

  if not IsFilled then
  begin
    LWidth  := LWidth  - 1;
    LHeight := LHeight - 1;
  end;

  tx      := Round(tx * AScale + AOffsetX);
  ty      := Round(ty * AScale + AOffsetY);
  LWidth  := Round(LWidth  * AScale);
  LHeight := Round(LHeight * AScale);

  if AAngle2 >= (360 * 64) then
  begin
    LXStartArc := 0;
    LYStartArc := 0;
    LXEndArc   := 0;
    LYEndArc   := 0;
  end
  else if AAngle2 > 0 then
  begin
    LXStartArc := Round( tx + (LWidth  / 2) + LWidth     * Cos(AAngle1 / 64 * 2 * G_PI / 360) );
    LYStartArc := Round( ty + (LHeight / 2) + (-LHeight) * Sin(AAngle1 / 64 * 2 * G_PI / 360) );
    LXEndArc   := Round( tx + (LWidth  / 2) + LWidth     * Cos( (AAngle1 + AAngle2) / 64 * 2 * G_PI /360 ));
    LYEndArc   := Round( ty + (LHeight / 2) + (-LHeight) * Sin( (AAngle1 + AAngle2) / 64 * 2 * G_PI /360 ));
  end
  else
  begin
    LXEndArc   := Round( tx + (LWidth  / 2) + LWidth     * Cos(AAngle1 / 64 * 2 * G_PI / 360) );
    LYEndArc   := Round( ty + (LHeight / 2) + (-LHeight) * Sin(AAngle1 / 64 * 2 * G_PI / 360) );
    LXStartArc := Round( tx + (LWidth  / 2) + LWidth     * Cos( (AAngle1 + AAngle2) / 64 * 2 * G_PI / 360 ));
    LYStartArc := Round( ty + (LHeight / 2) + (-LHeight) * Sin( (AAngle1 + AAngle2) / 64 * 2 * G_PI / 360 ));
  end;

  if IsFilled then
  begin
    ACanvas.Pie(tx, ty, tx + LWidth, ty + LHeight,
                LXStartArc, LYStartArc, LXEndArc, LYEndArc);
  end
  else
  begin
    ACanvas.Arc(tx, ty, tx + LWidth, ty + LHeight,
                LXStartArc, LYStartArc, LXEndArc, LYEndArc);
  end;

  ACanvas.Pen.Mode    := LPenMode;
  ACanvas.Pen.Color   := LPenColor;
  ACanvas.Brush.Style := LBrushStyle;
  ACanvas.Brush.Color := LBrushColor;
end;

procedure DrawCrossByAnchor(const ACanvas: TCanvas;
  const AX, AY, AWidth, AHeight: Integer; const AAnchor: TGtkAnchorType;
  const AOffsetX, AOffsetY: Integer; const AScale: Single);
var
  tx, ty         : Integer;
  LWidth, LHeight: Integer;
  LPenMode       : TPenMode;
  LPenColor      : TColor;
begin
  LPenMode  := ACanvas.Pen.Mode;
  LPenColor := ACanvas.Pen.Color;

  ACanvas.Pen.Mode := pmNotXor;

  ShiftToCenter(AX, AY, AWidth, AHeight, AAnchor, tx, ty);

  tx      := Round(tx * AScale + AOffsetX);
  ty      := Round(ty * AScale + AOffsetY);
  LWidth  := Round(AWidth  * AScale);
  LHeight := Round(AHeight * AScale);

  ACanvas.MoveTo( tx, ty - (LHeight shr 1) );
  ACanvas.LineTo( tx, ty + ((LHeight + 1) shr 1) );

  ACanvas.MoveTo( tx - (LWidth shr 1), ty );
  ACanvas.LineTo(tx + ((LWidth + 1) shr 1), ty );

  ACanvas.Pen.Mode  := LPenMode;
  ACanvas.Pen.Color := LPenColor;
end;

procedure DrawHandle(const ACanvas: TCanvas; const AType: TgmGimpHandleType;
  const AX, AY, AWidth, AHeight: Integer; const AAnchor: TGtkAnchorType;
  const AOffsetX, AOffsetY: Integer; const AScale: Single);
begin
  if not Assigned(ACanvas) then
  begin
    Exit;
  end;

  case AType of
    ghtSquare:
      begin
        DrawRectangleByAnchor(ACanvas, False, AX, AY, AWidth, AHeight, AAnchor,
                              AOffsetX, AOffsetY, AScale);
      end;

    ghtFilledSquare:
      begin
        DrawRectangleByAnchor(ACanvas, True, AX, AY, AWidth, AHeight, AAnchor,
                              AOffsetX, AOffsetY, AScale);
      end;

    ghtCircle:
      begin
        DrawArcByAnchor(ACanvas, False, AX, AY, AWidth, AHeight, 0, 360 * 64,
                        AAnchor, AOffsetX, AOffsetY, AScale);
      end;

    ghtFilledCircle:
      begin
        DrawArcByAnchor(ACanvas, True, AX, AY, AWidth, AHeight, 0, 360 * 64,
                        AAnchor, AOffsetX, AOffsetY, AScale);
      end;

    ghtCross:
      begin
        DrawCrossByAnchor(ACanvas, AX, AY, AWidth, AHeight, AAnchor,
                          AOffsetX, AOffsetY, AScale); 
      end;
  end;
end; 

end.
