unit gmGradientRender;

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

// Update Date: 2017-02-14

interface

{$WARN UNSAFE_CODE OFF}

uses
{ Standard }
  Windows,
{ Graphics32 }
  GR32,
{ externals\Graphics32_3rd_Party }
  GR32_Add_BlendModes,
{ GraphicsMagicLib }
  gmGradient,
  gmTypes;

type
  TgmGradientRenderMode = (grmLinear,
                           grmRadial,
                           grmAngle,
                           grmReflected,
                           grmDiamond);

  { TgmGradientRender }

  TgmGradientRender = class(TObject)
  private
    FColorGradient        : TgmGradientItem;
    FRenderMode           : TgmGradientRenderMode;
    FChannelSet           : TgmChannelSet;
    FBlendMode            : TBlendMode32;
    FReverse              : Boolean;
    FPreserveTransparency : Boolean;
    FOpacity              : Single;
    FStartPoint           : TPoint;
    FEndPoint             : TPoint;

    function GetStartPoint: TPoint;
    function GetEndPoint: TPoint;

    procedure SetColorGradient(const AValue: TgmGradientItem);
    procedure SetStartPoint(const AValue: TPoint);
    procedure SetEndPoint(const AValue: TPoint);
  public
    constructor Create;
    destructor Destroy; override;

    function Render(const ADestBmp: TBitmap32): Boolean;

    property ColorGradient          : TgmGradientItem       read FColorGradient        write SetColorGradient;
    property RenderMode             : TgmGradientRenderMode read FRenderMode           write FRenderMode;
    property ChannelSet             : TgmChannelSet         read FChannelSet           write FChannelSet;
    property BlendMode              : TBlendMode32          read FBlendMode            write FBlendMode;
    property IsReverse              : Boolean               read FReverse              write FReverse;
    property IsPreserveTransparency : Boolean               read FPreserveTransparency write FPreserveTransparency;
    property Opacity                : Single                read FOpacity              write FOpacity;
    property StartPoint             : TPoint                read GetStartPoint         write SetStartPoint;
    property EndPoint               : TPoint                read GetEndPoint           write SetEndPoint;
  end;

  { Color Gradient Rendering Methods }

  // Linear Gradient
  procedure DrawLinearGradient(const ABitmap: TBitmap32;
    const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
    const AReverse: Boolean);

  // Radial Gradient
  procedure DrawRadialGradient(const ABitmap: TBitmap32;
    const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
    const AReverse: Boolean);

  // Angle Gradient
  procedure DrawAngleGradient(const ABitmap: TBitmap32;
    const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
    const AReverse: Boolean);

  // Diamond Gradient
  procedure DrawDiamondGradient(const ABitmap: TBitmap32;
    const AStartPoint, AEndPoint: TPoint; AColorGradient: TgmGradientItem;
    const AReverse: Boolean);

  // Reflected Gradient
  procedure DrawReflectedGradient(const ABitmap: TBitmap32;
    const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
    const AReverse: Boolean);

implementation

uses
{ Standard }
  SysUtils, Classes, Math,
{ Graphics32 }
  GR32_PolygonsOld,
{ GR32_Lines Package by Angus Johnson }
  GR32_Misc,
{ GraphicsMagic Lib }
  gmBlendModes;

constructor TgmGradientRender.Create;
begin
  inherited Create;

  FColorGradient        := nil;
  FRenderMode           := grmLinear;
  FChannelSet           := [];
  FBlendMode            := bbmNormal32;
  FReverse              := False;
  FPreserveTransparency := False;
  FOpacity              := 1.0;
  FStartPoint           := Point(0, 0);
  FEndPoint             := Point(0, 0);
end;

destructor TgmGradientRender.Destroy;
begin
  FColorGradient := nil;
  
  inherited Destroy;
end;

function TgmGradientRender.GetStartPoint: TPoint;
begin
  Result := FStartPoint;
end;

function TgmGradientRender.GetEndPoint: TPoint;
begin
  Result := FEndPoint;
end;

procedure TgmGradientRender.SetColorGradient(const AValue: TgmGradientItem);
begin
  FColorGradient := AValue;
end;

procedure TgmGradientRender.SetStartPoint(const AValue: TPoint);
begin
  FStartPoint := AValue;
end;

procedure TgmGradientRender.SetEndPoint(const AValue: TPoint);
begin
  FEndPoint := AValue;
end; 

function TgmGradientRender.Render(const ADestBmp: TBitmap32): Boolean;
var
  i, LRGBChannelCount           : Integer;
  LGradientBitmap               : TBitmap32;
  LBlendColor                   : TColor32;
  LGradientBits, LDestBits      : PColor32;
  a, r, g, b                    : Cardinal;
  LWeight, fa, LAdjustedOpacity : Byte;
begin
  Result := False;

  if not Assigned(FColorGradient) then
  begin
    raise Exception.Create('Render Error -- has not specified a color gradient.');
  end;

  if FChannelSet = [] then
  begin
    raise Exception.Create('Render Error -- channel set is empty.');
  end;

  if (FStartPoint.X <> FEndPoint.X) or
     (FStartPoint.Y <> FEndPoint.Y) then
  begin
    LGradientBitmap := TBitmap32.Create;
    try
      LRGBChannelCount := 0;

      if csRed in FChannelSet then
      begin
        Inc(LRGBChannelCount);
      end;

      if csGreen in FChannelSet then
      begin
        Inc(LRGBChannelCount);
      end;

      if csBlue in FChannelSet then
      begin
        Inc(LRGBChannelCount);
      end;

      LGradientBitmap.SetSize(ADestBmp.Width, ADestBmp.Height);
      LGradientBitmap.DrawMode := dmBlend;

      // render color gradient
      case FRenderMode of
        grmLinear:
          begin
            DrawLinearGradient(LGradientBitmap, FStartPoint, FEndPoint,
                               FColorGradient, FReverse);
          end;
          
        grmRadial:
          begin
            DrawRadialGradient(LGradientBitmap, FStartPoint, FEndPoint,
                               FColorGradient, FReverse);
          end;
          
        grmAngle:
          begin
            DrawAngleGradient(LGradientBitmap, FStartPoint, FEndPoint,
                              FColorGradient, FReverse);
          end;
          
        grmReflected:
          begin
            DrawReflectedGradient(LGradientBitmap, FStartPoint, FEndPoint,
                                  FColorGradient, FReverse);
          end;
          
        grmDiamond:
          begin
            DrawDiamondGradient(LGradientBitmap, FStartPoint, FEndPoint,
                                FColorGradient, FReverse);
          end;
      end;

      LWeight       := Round(255 * FOpacity);
      LGradientBits := @LGradientBitmap.Bits[0];
      LDestBits     := @ADestBmp.Bits[0];

      // opacity blend
      for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
      begin
        fa := LGradientBits^ shr 24 and $FF;

        // do nothing if the foreground is fully transparent
        if fa > 0 then
        begin
          a := LDestBits^ and $FF000000;

          if a > 0 then
          begin
            // combine
            if csGrayscale in FChannelSet then
            begin
              // If the foreground has semi-transparent pixels, we need to
              // adjust the blending opacity. 

              if fa < 255 then
              begin
                LAdjustedOpacity := fa * LWeight div 255;
              end
              else
              begin
                LAdjustedOpacity := LWeight;
              end;

              LBlendColor := RGBBlendByMode(LGradientBits^, LDestBits^,
                                            LAdjustedOpacity, FBlendMode);
                                            
              b           := Intensity(LBlendColor);
              LDestBits^  := a or (b shl 16) or (b shl 8) or b;
            end
            else
            begin
              // blend foreground and dest with blend mode
              if LRGBChannelCount = 3 then
              begin
                LBlendColor := ARGBBlendByMode(LGradientBits^, LDestBits^,
                                               LWeight, FBlendMode);
              end
              else
              begin
                { If the foreground has semi-transparent pixels, we need to
                  adjust the blending opacity. }

                if fa < 255 then
                begin
                  LAdjustedOpacity := fa * LWeight div 255;
                end
                else
                begin
                  LAdjustedOpacity := LWeight;
                end;

                LBlendColor := RGBBlendByMode(LGradientBits^, LDestBits^,
                                              LAdjustedOpacity, FBlendMode);
              end;

              r := LDestBits^ and $FF0000;
              g := LDestBits^ and $FF00;
              b := LDestBits^ and $FF;

              if csRed in FChannelSet then
              begin
                r := LBlendColor and $FF0000;
              end;

              if csGreen in FChannelSet then
              begin
                g := LBlendColor and $FF00;
              end;

              if csBlue in FChannelSet then
              begin
                b := LBlendColor and $FF;
              end;

              if (FPreserveTransparency) or (LRGBChannelCount < 3) then
              begin
                LDestBits^ := a or r or g or b;
              end
              else
              begin
                LDestBits^ := (LBlendColor and $FF000000) or r or g or b;
              end;
            end;
          end
          else
          begin
            if not FPreserveTransparency then
            begin
              if (csRed   in FChannelSet) and
                 (csGreen in FChannelSet) and
                 (csBlue  in FChannelSet) then
              begin
                a := fa * LWeight div 255;

                LDestBits^ := (a shl 24) or (LGradientBits^ and $FFFFFF);
              end;
            end;
          end;
        end;

        // move pointer to next pixel
        Inc(LGradientBits);
        Inc(LDestBits);
      end;

      Result := True;

    finally
      LGradientBitmap.Free;
    end;
  end; 
end;

{ Color Gradient Rendering Methods }

// horizontal linear gradient
procedure HorizLinearGradientFill(const ABitmap: TBitmap32;
  const AX1, AX2: Integer; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i                  : Integer;
  LWidth, LHeight    : Integer;
  LLeftX, LRightX    : Integer;
  LLength, LMaxIndex : Integer;
  LColorLUT          : array of TColor32;
  LDrawingColor      : TColor32;
  LStartColor        : TColor32;
  LEndColor          : TColor32;
begin
  ABitmap.BeginUpdate;
  try
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    // get the left and right point
    if AX1 < AX2 then
    begin
      LLeftX  := AX1;
      LRightX := AX2;
    end
    else
    begin
      LLeftX  := AX2;
      LRightX := AX1;
    end;

    // get gradient colors
    LLength   := LRightX - LLeftX + 1;
    LMaxIndex := LLength - 1;

    AColorGradient.GradientLength := LLength;
    AColorGradient.RefreshColorArray;

    SetLength(LColorLUT, LLength);

    if AReverse then
    begin
      LStartColor := AColorGradient.AbsolutEndColor;
      LEndColor   := AColorGradient.AbsolutStartColor;

      for i := 0 to LMaxIndex do
      begin
        LColorLUT[i] := AColorGradient.OutputColors[LMaxIndex - i];
      end;
    end
    else
    begin
      LStartColor := AColorGradient.AbsolutStartColor;
      LEndColor   := AColorGradient.AbsolutEndColor;

      for i := 0 to LMaxIndex do
      begin
        LColorLUT[i] := AColorGradient.OutputColors[i];
      end;
    end;

    // If the start point is at left, and the end point is at right, draw the
    // left side by the starting color, and draw the right side by the ending
    // color.
    if AX1 < AX2 then
    begin
      ABitmap.FillRectS( Rect(0, 0, LLeftX, LHeight), LStartColor );
      ABitmap.FillRectS( Rect(LRightX, 0, LWidth, LHeight), LEndColor );
    end
    else
    if AX1 > AX2 then
    begin
      // If the start point is at right, and the end point is at left, draw the
      // left side by the ending color, and draw the right side by the
      // starting color.

      ABitmap.FillRectS( Rect(0, 0, LLeftX, LHeight), LEndColor );
      ABitmap.FillRectS( Rect(LRightX, 0, LWidth, LHeight), LStartColor );
    end;

    // Draw gradient vertical line ...
    for i := 0 to LMaxIndex do
    begin
      // If the start point is at left, and the end point is at right,
      // draw the gradient part by the color from start color to end color.
      if AX1 < AX2 then
      begin
        LDrawingColor := LColorLUT[i];
      end
      else
      begin
        // If the start point is at right, and the end point is at left,
        // draw the gradient part by the color from end color to start color.
        LDrawingColor := LColorLUT[LMaxIndex - i];
      end;

      // draw gradient vertical lines
      ABitmap.VertLineS(LLeftX + i, 0, LHeight, LDrawingColor);
    end;

  finally
    ABitmap.EndUpdate;
  end;
end;

// vertical linear gradient
procedure VertLinearGradientFill(const ABitmap: TBitmap32;
  const AY1, AY2: Integer; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i                  : Integer;
  LWidth, LHeight    : Integer;
  LTopY, LBottomY    : Integer;
  LLength, LMaxIndex : Integer;
  LColorLUT          : array of TColor32;
  LDrawingColor      : TColor32;
  LStartColor        : TColor32;
  LEndColor          : TColor32;
begin
  LDrawingColor := $00000000;

  ABitmap.BeginUpdate;
  try
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    // get the top and bottom point
    if AY1 < AY2 then
    begin
      LTopY    := AY1;
      LBottomY := AY2;
    end
    else
    begin
      LTopY    := AY2;
      LBottomY := AY1;
    end;

    // get gradient colors
    LLength   := LBottomY - LTopY + 1;
    LMaxIndex := LLength - 1;

    AColorGradient.GradientLength := LLength;
    AColorGradient.RefreshColorArray;

    SetLength(LColorLUT, LLength);

    if AReverse then
    begin
      LStartColor := AColorGradient.AbsolutEndColor;
      LEndColor   := AColorGradient.AbsolutStartColor;
      
      for i := 0 to LMaxIndex do
      begin
        LColorLUT[i] := AColorGradient.OutputColors[LMaxIndex - i];
      end;
    end
    else
    begin
      LStartColor := AColorGradient.AbsolutStartColor;
      LEndColor   := AColorGradient.AbsolutEndColor;

      for i := 0 to LMaxIndex do
      begin
        LColorLUT[i] := AColorGradient.OutputColors[i];
      end;
    end;

    // If the start point is at top, and the end point is at bottom, draw the
    // top side by start color, and draw the bottom side by end color.
    if AY1 < AY2 then
    begin
      ABitmap.FillRectS( Rect(0, 0, LWidth, LTopY ), LStartColor );
      ABitmap.FillRectS( Rect(0, LBottomY, LWidth, LHeight), LEndColor );
    end
    else
    if AY1 > AY2 then
    begin
      // If the start point is at bottom, and the end point is at top, draw the
      // top side by end color, and draw the bottom side by start color
      ABitmap.FillRectS( Rect(0, 0, LWidth, LTopY ), LEndColor );
      ABitmap.FillRectS( Rect(0, LBottomY, LWidth, LHeight), LStartColor );
    end;

    // Draw gradient horizontal lines ...
    for i := 0 to LMaxIndex do
    begin
      // If the start point is at top, and the end point is at bottom,
      // draw the gradient part by the color from start color to end color.
      if AY1 < AY2 then
      begin
        LDrawingColor := LColorLUT[i];
      end
      else
      if AY1 > AY2 then
      begin
        // If the start point is at bottom, and the end point is at top,
        // draw the gradient part by the color from end color to start color.
        LDrawingColor := LColorLUT[LMaxIndex - i];
      end;

      // Draw gradient horizontal lines
      ABitmap.HorzLineS(0, LTopY + i, LWidth, LDrawingColor);
    end;
  
  finally
    ABitmap.EndUpdate;
  end;
end; 

// linear gradient with slop fill
procedure SlopLinearGradientFill(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AGradientItem: TgmGradientItem;
  const AReverse: Boolean);
var
  i                  : Integer;
  LWidth             : Integer;
  LHeight            : Integer;
  LLength            : Integer;
  LMaxIndex          : Integer;
  LLeftSlopeTopX     : Integer;
  LLeftSlopeBottomX  : Integer;
  LRightSlopeTopX    : Integer;
  LRightSlopeBottomX : Integer;
  LSlope             : Single;
  LLeftPoint         : TPoint;
  LRightPoint        : TPoint;
  LColorLUT          : array of TColor32;
  LDrawingColor      : TColor32;
  LStartColor        : TColor32;
  LEndColor          : TColor32;
begin
  LDrawingColor := $00000000;

  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then
  begin
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    ABitmap.BeginUpdate;
    try
      LSlope:= Abs(AEndPoint.Y - AStartPoint.Y) / Abs(AEndPoint.X - AStartPoint.X);

      // get the left and right point
      if AStartPoint.X < AEndPoint.X then
      begin
        LLeftPoint  := AStartPoint;
        LRightPoint := AEndPoint;
      end
      else
      begin
        LLeftPoint  := AEndPoint;
        LRightPoint := AStartPoint;
      end;

      // slop line that down to left...
      if LLeftPoint.Y < LRightPoint.Y then
      begin
        LLeftSlopeTopX     := LLeftPoint.X  + Round(LLeftPoint.Y * LSlope);
        LLeftSlopeBottomX  := LLeftPoint.X  - Round( (LHeight - LLeftPoint.Y) * LSlope );

        LRightSlopeTopX    := LRightPoint.X + Round(LRightPoint.Y * LSlope);
        LRightSlopeBottomX := LRightPoint.X - Round( (LHeight - LRightPoint.Y) * LSlope );

        // get gradient colors
        LLength   := LRightSlopeTopX - LLeftSlopeTopX + 1;
        LMaxIndex := LLength - 1;

        AGradientItem.GradientLength := LLength;
        AGradientItem.RefreshColorArray;

        SetLength(LColorLUT, LLength);

        if AReverse then
        begin
          LStartColor := AGradientItem.AbsolutEndColor;
          LEndColor   := AGradientItem.AbsolutStartColor;

          for i := 0 to LMaxIndex do
          begin
            LColorLUT[i] := AGradientItem.OutputColors[LMaxIndex - i];
          end;
        end
        else
        begin
          LStartColor := AGradientItem.AbsolutStartColor;
          LEndColor   := AGradientItem.AbsolutEndColor;

          for i := 0 to LMaxIndex do
          begin
            LColorLUT[i] := AGradientItem.OutputColors[i];
          end;
        end;

        // If the start point is at top left, and the end point is at
        // bottom right, draw the top left side by the start color.
        if AStartPoint.X < AEndPoint.X then
        begin
          LDrawingColor := LStartColor;
        end
        else
        if AStartPoint.X > AEndPoint.X then
        begin
          // If the start point is at bottom right, and the end point is at
          // top left, draw the top left side by the end color. 
          LDrawingColor := LEndColor;
        end;

        for i := 0 to LLeftSlopeTopX do
        begin
          // Draw the right slope lines at the top-left side. Must be drawing
          // from top to bottom, Otherwise, a white line will be remained.
          ABitmap.LineS(LLeftSlopeTopX - i, 0,
                        LLeftSlopeBottomX - i, LHeight, LDrawingColor);
        end;

        // If the start point is at top left, and the end point is at
        // bottom right, draw the bottom right side by the end color.
        if AStartPoint.X < AEndPoint.X then
        begin
          LDrawingColor := LEndColor;
        end
        else
        if AStartPoint.X > AEndPoint.X then
        begin
          // If the start point is at bottom right, and the end point is at
          // top left, draw the bottom right side by the start color.
          LDrawingColor := LStartColor;
        end;

        for i := 0 to (LWidth - LRightSlopeBottomX) do
        begin
          // Draw the right slope lines at the bottom-right side. Must be drawing
          // from top to bottom, Otherwise, a white line will be remained.
          ABitmap.LineS(LRightSlopeTopX + i, 0,
                        LRightSlopeBottomX + i, LHeight, LDrawingColor);
        end;

        // Draw gradient line ...
        for i := 0 to LMaxIndex do
        begin
          // If the start point is at top left, and the end point is at
          // bottom right, draw the gradient part by the color from start color
          // to end color.
          if AStartPoint.X < AEndPoint.X then
          begin
            LDrawingColor := LColorLUT[i];
          end
          else
          if AStartPoint.X > AEndPoint.X then
          begin
            // If the start point is at bottom right, and the end point is at
            // top left, draw the gradient part by the color from end color
            // to start color.
            LDrawingColor := LColorLUT[LMaxIndex - i];
          end;

          // draw gradient right slope lines ...
          ABitmap.LineS(LLeftSlopeTopX + i, 0,
                        LLeftSlopeBottomX + i, LHeight, LDrawingColor);
        end; 
      end
      else
      if LLeftPoint.Y > LRightPoint.Y then // slop line that down to right...
      begin
        LLeftSlopeTopX     := LLeftPoint.X   - Round(LLeftPoint.Y * LSlope);
        LLeftSlopeBottomX  := LLeftPoint.X   + Round( (LHeight - LLeftPoint.Y) * LSlope );

        LRightSlopeTopX    := LRightPoint.X  - Round(LRightPoint.Y * LSlope);
        LRightSlopeBottomX := LRightPoint.X  + Round( (LHeight - LRightPoint.Y) * LSlope );

        // get gradient colors
        LLength   := LRightSlopeTopX - LLeftSlopeTopX + 1;
        LMaxIndex := LLength - 1;

        AGradientItem.GradientLength := LLength;
        AGradientItem.RefreshColorArray;

        SetLength(LColorLUT, LLength);

        if AReverse then
        begin
          LStartColor := AGradientItem.AbsolutEndColor;
          LEndColor   := AGradientItem.AbsolutStartColor;

          for i := 0 to LMaxIndex do
          begin
            LColorLUT[i] := AGradientItem.OutputColors[LMaxIndex - i];
          end;
        end
        else
        begin
          LStartColor := AGradientItem.AbsolutStartColor;
          LEndColor   := AGradientItem.AbsolutEndColor;

          for i := 0 to LMaxIndex do
          begin
            LColorLUT[i] := AGradientItem.OutputColors[i];
          end;
        end;

        // If the start point is at bottom left, and the end point is at
        // top right, draw the bottom left side by the start color. 
        if AStartPoint.X < AEndPoint.X then
        begin
          LDrawingColor := LStartColor;
        end
        else
        if AStartPoint.X > AEndPoint.X then
        begin
          // If the start point is at top right, and the end point is at
          // bottom left, draw the bottom left side by the end color. 
          LDrawingColor := LEndColor;
        end;

        for i := 0 to LLeftSlopeBottomX do
        begin
          // Draw the left slope lines at the bottom left side. Must be drawing
          // from top to bottom, Otherwise, a white line will be remained.
          ABitmap.LineS(LLeftSlopeTopX - i, 0,
                        LLeftSlopeBottomX - i, LHeight, LDrawingColor);
        end;

        // If the start point is at bottom left, and the end point is at
        // top right, draw the the top right side by end color.
        if AStartPoint.X < AEndPoint.X then
        begin
          LDrawingColor := LEndColor;
        end
        else
        if AStartPoint.X > AEndPoint.X then
        begin
          // If the start point is at top right, and the end point is at
          // bottom left, draw the top right side by start color. 
          LDrawingColor := LStartColor;
        end;

        for i := 0 to (LWidth - LRightSlopeTopX) do
        begin
          // Draw the left slope lines at top right side. Must be drawing
          // from top to bottom, Otherwise, a white line will be remained.
          ABitmap.LineS(LRightSlopeTopX + i, 0,
                        LRightSlopeBottomX + i, LHeight, LDrawingColor);
        end;

        // Draw gradient lines ...
        for i := 0 to LMaxIndex do
        begin
          // If the start point is at bottom left, and the end point is at
          // top right, draw the gradient part by the color from StartColor
          // to EndColor.
          if AStartPoint.X < AEndPoint.X then
          begin
            LDrawingColor := LColorLUT[i];
          end
          else
          if AStartPoint.X > AEndPoint.X then
          begin
            // If the start point is at top right, and the end point is at
            // bottom left, draw the gradient part by the color from end color
            // to start color.
            LDrawingColor := LColorLUT[LMaxIndex - i];
          end;

          // Draw gradient left slope lines.
          ABitmap.LineS(LLeftSlopeTopX + i, 0,
                        LLeftSlopeBottomX + i, LHeight, LDrawingColor);
        end; 
      end;

    finally
      ABitmap.EndUpdate;
    end;
  end;
end;

// Linear Gradient
procedure DrawLinearGradient(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
begin
  if ( not Assigned(AColorGradient) ) or
     ( not Assigned(ABitmap) )  then
  begin
    Exit;
  end;

  // do nothing if the start point equals to end point
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    Exit;
  end;

  // horiontal gradient 
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    HorizLinearGradientFill(ABitmap, AStartPoint.X, AEndPoint.X,
                            AColorGradient, AReverse);
  end
  else // vertical gradient 
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then
  begin
    VertLinearGradientFill(ABitmap, AStartPoint.Y, AEndPoint.Y,
                           AColorGradient, AReverse);
  end
  else
  begin
    // slope gradient
    SlopLinearGradientFill(ABitmap, AStartPoint, AEndPoint, AColorGradient,
                           AReverse);
  end;
end; 

// Radial Gradient
procedure DrawRadialGradient(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  LPolygon      : TPolygon32;
  LRadius, i    : Integer;
  LMaxIndex     : Integer;
  LColorLUT     : array of TColor32;
  LDrawingColor : TColor32;
  LCircleRect   : TFloatRect;
  LPoints       : TArrayOfFixedPoint;
begin
  LRadius := 0;

  if ( not Assigned(AColorGradient) ) or
     ( not Assigned(ABitmap) )  then
  begin
    Exit;
  end;

  // do nothing if the start point equals to end point
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    Exit;
  end;

  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y =  AEndPoint.Y) then  // Horizontal line
  begin
    LRadius := Abs(AStartPoint.X - AEndPoint.X) + 1;
  end
  else
  if (AStartPoint.X =  AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then // Vertical line
  begin
    LRadius := Abs(AStartPoint.Y - AEndPoint.Y) + 1;
  end
  else
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then  // Slope line
  begin
    LRadius := Round(  Sqrt( Sqr(AStartPoint.X - AEndPoint.X + 1) +
                             Sqr(AStartPoint.Y - AEndPoint.Y + 1) )  );
  end;

  if LRadius = 0 then
  begin
    Exit
  end;

  LMaxIndex := LRadius - 1;

  AColorGradient.GradientLength := LRadius;
  AColorGradient.RefreshColorArray;

  SetLength(LColorLUT, LRadius);

  if AReverse then
  begin
    LDrawingColor := AColorGradient.AbsolutStartColor;
    
    for i := 0 to LMaxIndex do
    begin
      LColorLUT[i] := AColorGradient.OutputColors[LMaxIndex - i];
    end;
  end
  else
  begin
    LDrawingColor := AColorGradient.AbsolutEndColor;
    
    for i := 0 to LMaxIndex do
    begin
      LColorLUT[i] := AColorGradient.OutputColors[i];
    end;
  end;

  LPolygon := TPolygon32.Create;
  ABitmap.BeginUpdate;
  try
    ABitmap.Clear(LDrawingColor);

    // Draw gradient circles from the start point
    for i := 0 to LMaxIndex do
    begin
      LDrawingColor := LColorLUT[LMaxIndex - i];

      // Draw gradient circles from outer to inner by the start point.

      LCircleRect := FloatRect(AStartPoint.X - LRadius + i,
                               AStartPoint.Y - LRadius + i,
                               AStartPoint.X + LRadius - i,
                               AStartPoint.Y + LRadius - i);

      SetLength(LPoints, 0);
      LPoints := GetEllipsePoints(LCircleRect);

      LPolygon.Clear;
      LPolygon.AddPoints( LPoints[0], Length(LPoints) );

      LPolygon.DrawFill(ABitmap, LDrawingColor);
      LPolygon.DrawEdge(ABitmap, LDrawingColor);
    end;

  finally
    ABitmap.EndUpdate;
    LPolygon.Free;
    LPoints := nil;
  end;
end; 

// Angle Gradient
//
// We could optimize this algorithm by cancel Degree <=> Radian operations,
// only use of radian as unit. But for illustrate the ideas of this alogrithm,
// we haven't do the optimizations. 
procedure DrawAngleGradient(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);

  // calculate the vertices for outline of a radial line
  procedure CalcVerticesForRadialLineOutline(var APolygon: array of TPoint;
    const ALineStart, ALineEnd: TPoint; const ALineWeight: Integer);
  var
    LXDistance, LYDistance         : Integer;
    LAngle, LAccumRadians, LHalfPI : Extended;  // radians
    LDeltaX, LDeltaY               : Extended;
  begin
    if Length(APolygon) = 4 then
    begin
      LXDistance := ALineEnd.X - ALineStart.X;
      LYDistance := ALineEnd.Y - ALineStart.Y;
      LAngle     := ArcTan2(LYDistance, LXDistance);
      LHalfPI    := PI / 2;

      APolygon[0] := ALineStart;

      LAccumRadians := LAngle + LHalfPI;
      LDeltaX       := ALineWeight * Cos(LAccumRadians);
      LDeltaY       := ALineWeight * Sin(LAccumRadians);
      APolygon[1].X := Round(ALineEnd.X   + LDeltaX);
      APolygon[1].Y := Round(ALineEnd.Y   + LDeltaY);

      LAccumRadians := LAngle - LHalfPI;
      LDeltaX       := ALineWeight * Cos(LAccumRadians);
      LDeltaY       := ALineWeight * Sin(LAccumRadians);
      APolygon[2].X := Round(ALineEnd.X   + LDeltaX );
      APolygon[2].Y := Round(ALineEnd.Y   + LDeltaY );

      // Connect from start to end.
      APolygon[3] := APolygon[0];
    end;
  end; 

var
  i, j          : Integer;
  LMaxVal       : Integer;
  LAngle        : Single;
  LRadians      : Single;
  LHypotenuse   : Single;
  LRadius       : Single;
  LDrawingColor : TColor32;
  LEndPoint     : TPoint;
  LOutline      : array of TPoint;
  LPolygon      : TPolygon32;
begin
  if ( not Assigned(AColorGradient) ) or
     ( not Assigned(ABitmap) )  then
  begin
    Exit;
  end;

  // do nothing if the start point equals to end point
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    Exit;
  end;

  LAngle      := 0.0;
//  LMaxVal     := 360 * 20;  // original code
  LMaxVal     := 360 * 50;
  LRadius     := Sqrt( Sqr(ABitmap.Width) + Sqr(ABitmap.Height) );
  LHypotenuse := Sqrt( Sqr(AEndPoint.X - AStartPoint.X) + Sqr(AEndPoint.Y - AStartPoint.Y) );

  AColorGradient.GradientLength := LMaxVal + 1;
  AColorGradient.RefreshColorArray;

  if (AEndPoint.X >= AStartPoint.X) and
     (AEndPoint.Y <= AStartPoint.Y) then // endpoint is at first quadrant relative to startpoint
  begin
    LRadians := ArcSin( Abs(AEndPoint.Y - AStartPoint.Y) / LHypotenuse );
    LAngle   := LRadians * 180 / PI;
  end
  else
  if (AEndPoint.X <= AStartPoint.X) and
     (AEndPoint.Y <= AStartPoint.Y) then // endpoint is at second quadrant relative to startpoint
  begin
    LRadians := ArcSin( Abs(AEndPoint.Y - AStartPoint.Y) / LHypotenuse );
    LAngle   := 180 - LRadians * 180 / PI ;
  end
  else
  if (AEndPoint.X <= AStartPoint.X) and
     (AEndPoint.Y >= AStartPoint.Y) then // endpoint is at third quadrant relative to startpoint
  begin
    LRadians := ArcSin( Abs(AEndPoint.Y - AStartPoint.Y) / LHypotenuse );
    LAngle   := 180 + LRadians * 180 / PI;
  end
  else
  if (AEndPoint.X >= AStartPoint.X) and
     (AEndPoint.Y >= AStartPoint.Y) then // endpoint is at forth quadrant relative to startpoint
  begin
    LRadians := ArcSin( Abs(AEndPoint.Y - AStartPoint.Y) / LHypotenuse );
    LAngle   := 360 - LRadians * 180 / PI;
  end;

  SetLength(LOutline, 4);
  ABitmap.BeginUpdate;
  LPolygon := TPolygon32.Create;
  try
    for i := 0 to LMaxVal do
    begin
      if AReverse then
      begin
        LDrawingColor := AColorGradient.OutputColors[LMaxVal - i];
      end
      else
      begin
        LDrawingColor := AColorGradient.OutputColors[i];
      end;

      LEndPoint.X := AStartPoint.X + Round( LRadius * Cos(LAngle * Pi / 180) );
      LEndPoint.Y := AStartPoint.Y - Round( LRadius * Sin(LAngle * Pi / 180) );

      ABitmap.LineS(AStartPoint.X, AStartPoint.Y, LEndPoint.X, LEndPoint.Y,
                    LDrawingColor);

      // draw radial triangle for filling out gaps ...
      CalcVerticesForRadialLineOutline(LOutline, AStartPoint, LEndPoint, 1);

      LPolygon.Clear;

      for j := 0 to 3 do
      begin
        LPolygon.Add( FixedPoint(LOutline[j]) );
      end;

      LPolygon.DrawFill(ABitmap, LDrawingColor);

      // accumulate angle
      // each step, we rotate 0.02 degree, so there are 360 * 50 steps in total
//      LAngle := LAngle + 0.05; // original code
      LAngle := LAngle + 0.02;

      if LAngle > 360.0 then
      begin
//        LAngle := 0.0;  // original code
        LAngle := LAngle - 360.0;
      end;
    end;

  finally
    ABitmap.EndUpdate;
    
    SetLength(LOutline, 0);
    LOutline := nil;

    LPolygon.Free;
  end;
end; 

// Diamond Gradient
procedure DrawDiamondGradient(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i, LLoop          : Integer;
  LWidth, LHeight   : Integer;
  LHypotenuse       : Integer;
  LXValue, LYValue  : Integer;
  LDeltaX, LDeltaY  : Single;
  LLeftPoint        : TFixedPoint;
  LRightPoint       : TFixedPoint;
  LTopPoint         : TFixedPoint;
  LBottomPoint      : TFixedPoint;
  LTopLeftPoint     : TFixedPoint;
  LBottomLeftPoint  : TFixedPoint;
  LTopRightPoint    : TFixedPoint;
  LBottomRightPoint : TFixedPoint;
  LColorLUT         : array of TColor32;
  LDrawingColor     : TColor32;
  LPolygon          : TPolygon32;
begin
  if ( not Assigned(AColorGradient) ) or
     ( not Assigned(ABitmap) )  then
  begin
    Exit;
  end;

  // do nothing if the start point equals to end point
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    Exit;
  end;

  LWidth  := Abs(AStartPoint.X - AEndPoint.X);
  LHeight := Abs(AStartPoint.Y - AEndPoint.Y);

  // fill the whole bitmap by end color
  if AReverse then
  begin
    ABitmap.Clear(AColorGradient.AbsolutStartColor);
  end
  else
  begin
    ABitmap.Clear(AColorGradient.AbsolutEndColor);
  end;

  // Horizontal or Vertical
  if ( (AStartPoint.X <> AEndPoint.X) and (AStartPoint.Y = AEndPoint.Y) ) or
     ( (AStartPoint.X = AEndPoint.X) and (AStartPoint.Y <> AEndPoint.Y) ) then
  begin
    // If it is horizontal the Loop is actually " Width + 0 ", because
    // the Height is 0; if it is vertical the Loop is actually
    // " 0 + Height ", because the Width is 0. 
    LLoop := LWidth + LHeight;

    // add one more color gradient step for filling the gap caused by
    // calculation error
    AColorGradient.GradientLength := LLoop + 1;
    AColorGradient.RefreshColorArray;

    ABitmap.BeginUpdate;
    SetLength(LColorLUT, LLoop + 1);
    LPolygon := TPolygon32.Create;
    try
      if AReverse then
      begin
        for i := 0 to LLoop do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[LLoop - i];
        end;
      end
      else
      begin
        for i := 0 to LLoop do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[i];
        end;
      end;

      for i := LLoop downto 0 do
      begin
        // calculate point
        LTopPoint    := FixedPoint(AStartPoint.X, AStartPoint.Y - i);
        LLeftPoint   := FixedPoint(AStartPoint.X - i, AStartPoint.Y);
        LBottomPoint := FixedPoint(AStartPoint.X, AStartPoint.Y + i);
        LRightPoint  := FixedPoint(AStartPoint.X + i, AStartPoint.Y);

        // gradual color -- from start color to end color
        LDrawingColor := LColorLUT[i];

        // draw lines --
        // the order is top - left - bottom - right - top
        with LPolygon do
        begin
          Clear;

          Add(LTopPoint);
          Add(LLeftPoint);
          Add(LBottomPoint);
          Add(LRightPoint);
          Add(LTopPoint);

          DrawFill(ABitmap, LDrawingColor);
        end;
      end;
    finally
      ABitmap.EndUpdate;
      SetLength(LColorLUT, 0);
      LColorLUT := nil;
      
      LPolygon.Free;
    end;
  end
  else
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then // right or left slope
  begin
    LHypotenuse := Round(  Sqrt( Sqr(LWidth) + Sqr(LHeight) )  );

    // get the scale of width/height to hypotenuse
    LDeltaX := LWidth  / LHypotenuse;
    LDeltaY := LHeight / LHypotenuse;

    // add one more gradient step for filling the gap caused by
    // calculation error
    AColorGradient.GradientLength := LHypotenuse + 1;
    AColorGradient.RefreshColorArray;

    ABitmap.BeginUpdate;
    SetLength(LColorLUT, LHypotenuse + 1);
    LPolygon := TPolygon32.Create;
    try
      if AReverse then
      begin
        for i := 0 to LHypotenuse do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[LHypotenuse - i];
        end;
      end
      else
      begin
        for i := 0 to LHypotenuse do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[i];
        end;
      end;

      for i := LHypotenuse downto 0 do
      begin
        // get delta of x/y as the hypotenuse decreasing
        LXValue := Round(i * LDeltaX);
        LYValue := Round(i * LDeltaY);

        // Right Slope
        if ( (AStartPoint.X < AEndPoint.X) and (AStartPoint.Y > AEndPoint.Y) ) or
           ( (AStartPoint.X > AEndPoint.X) and (AStartPoint.Y < AEndPoint.Y) ) then
        begin
          { Since the hypotenuse is slope to right...

            the point at first quadrant is
              (StartPoint.X + XValue, StartPoint.Y - YValue);
              
            the point at third quadrant is
              (StartPoint.X - XValue, StartPoint.Y + YValue);

            since the delta X and delta Y which are in the second and
            forth quadrant are equal to the delta Y and delta X which
            are in the first and third quadrant, respectively,
            so the points are the follows:

            the point at second quadrant is
              (StartPoint.X - YValue, StartPoint.Y - XValue);

            the point at forth quadrant is
              (StartPoint.X + YValue, StartPoint.Y + XValue);
          }

          // at first quadrant
          LTopRightPoint := FixedPoint(AStartPoint.X + LXValue,
                                       AStartPoint.Y - LYValue);

          // at third quadrant
          LBottomLeftPoint := FixedPoint(AStartPoint.X - LXValue,
                                         AStartPoint.Y + LYValue);

          // at second quadrant
          LTopLeftPoint := FixedPoint(AStartPoint.X - LYValue,
                                      AStartPoint.Y - LXValue);

          // at forth quadrant
          LBottomRightPoint := FixedPoint(AStartPoint.X + LYValue,
                                          AStartPoint.Y + LXValue);
        end
        else
        // Left Slope
        if ( (AStartPoint.X > AEndPoint.X) and (AStartPoint.Y > AEndPoint.Y) ) or
           ( (AStartPoint.X < AEndPoint.X) and (AStartPoint.Y < AEndPoint.Y) ) then
        begin
          { Since the hypotenuse is slope to left...

            the point at second quadrant is
              (StartPoint.X - XValue, StartPoint.Y - YValue);

            the point at forth quadrant is
              (StartPoint.X + XValue, StartPoint.Y + YValue);

            since the delta X and delta Y which are in the first and
            third quadrant are equal to the delta Y and delta X which
            are in the second and forth quadrant, respectively,
            so the points are the follows:

            the point at first quadrant is
              (StartPoint.X + YValue, StartPoint.Y - XValue);

            the point at third quadrant is
              (StartPoint.X - YValue, StartPoint.Y + XValue);
          }

          // at first quadrant
          LTopRightPoint := FixedPoint(AStartPoint.X + LYValue,
                                       AStartPoint.Y - LXValue);

          // at third quadrant
          LBottomLeftPoint := FixedPoint(AStartPoint.X - LYValue,
                                         AStartPoint.Y + LXValue);

          // at second quadrant
          LTopLeftPoint := FixedPoint(AStartPoint.X - LXValue,
                                      AStartPoint.Y - LYValue);

          // at forth quadrant
          LBottomRightPoint := FixedPoint(AStartPoint.X + LXValue,
                                          AStartPoint.Y + LYValue);
        end;

        // gradual color -- from start color to end color
        LDrawingColor := LColorLUT[i];

        // draw lines --
        // the order is top right - top left - bottom left - bottom right - top right
        with LPolygon do
        begin
          Clear;

          Add(LTopRightPoint);
          Add(LTopLeftPoint);
          Add(LBottomLeftPoint);
          Add(LBottomRightPoint);
          Add(LTopRightPoint);

          DrawFill(ABitmap, LDrawingColor);
        end;
      end;

    finally
      ABitmap.EndUpdate;
      SetLength(LColorLUT, 0);
      LColorLUT := nil;
      
      LPolygon.Free;
    end;
  end;
end;

// horizontal reflected gradient
procedure HorizReflectedGradientFill(const ABitmap: TBitmap32;
  const AX1, AX2: Integer; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i               : Integer;
  LLeftX, LRightX : Integer;
  LWidth, LHeight : Integer;
  LDotDistance    : Integer;
  LColorLUT       : array of TColor32;
  LDrawingColor   : TColor32;
begin
  ABitmap.BeginUpdate;
  try
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    // get the left and right point
    if AX1 < AX2 then
    begin
      LLeftX  := AX1;
      LRightX := AX2;
    end
    else
    begin
      LLeftX  := AX2;
      LRightX := AX1;
    end;

    LDotDistance := Abs(LRightX - LLeftX) div 2;

    // add one more gradient step for filling the gap
    // caused by calculation error
    AColorGradient.GradientLength := LDotDistance + 1;
    AColorGradient.RefreshColorArray;

    SetLength(LColorLUT, LDotDistance + 1);
    try
      if AReverse then
      begin
        // fill the left side and the right side by start color
        LDrawingColor := AColorGradient.AbsolutStartColor;

        ABitmap.FillRectS( Rect(0, 0, LLeftX, LHeight), LDrawingColor );
        ABitmap.FillRectS( Rect(LRightX, 0, LWidth, LHeight), LDrawingColor );

        for i := 0 to LDotDistance do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[LDotDistance - i];
        end;
      end
      else
      begin
        // fill the left side and the right side by end color
        LDrawingColor := AColorGradient.AbsolutEndColor;

        ABitmap.FillRectS( Rect(0, 0, LLeftX, LHeight), LDrawingColor );
        ABitmap.FillRectS( Rect(LRightX, 0, LWidth, LHeight), LDrawingColor );

        for i := 0 to LDotDistance do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[i];
        end;
      end;

      // draw gradient vertical line
      for i := 0 to LDotDistance do
      begin
        { The drawing direction is two-way, if the DotDistance is odd,
          draw the gradient part by the color from StartColor to EndColor,
          the drawing direction is from the inner to the outer until
          the two edges of the gradient part. if the DotDistance is Even,
          draw the gradient part by the color from EndColor to StartColor,
          the drawing direction is from the outer to the inner until
          the center of the gradient part. }

        if Odd(LDotDistance) then
        begin
          LDrawingColor := LColorLUT[i];

          // draw gradient vertical lines
          ABitmap.LineS(LLeftX + LDotdistance - i, 0,
                        LLeftX + LDotDistance - i, LHeight,
                        LDrawingColor);

          ABitmap.LineS(LLeftX + LDotDistance + i, 0,
                        LLeftX + LDotDistance + i, LHeight,
                        LDrawingColor);
        end
        else
        begin
          LDrawingColor := LColorLUT[LDotDistance - i];

          // draw gradient vertical lines
          ABitmap.LineS(LLeftX + i, 0, LLeftX + i, LHeight, LDrawingColor);
          ABitmap.LineS(LRightX - i, 0, LRightX - i, LHeight, LDrawingColor);
        end;
      end;
      
    finally
      SetLength(LColorLUT, 0);
      LColorLUT := nil;
    end;

  finally
    ABitmap.EndUpdate;
  end;
end; 

// vertical reflected gradient
procedure VertReflectedGradientFill(const ABitmap: TBitmap32;
  const AY1, AY2: Integer; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i               : Integer;
  LTopY, LBottomY : Integer;
  LWidth, LHeight : Integer;
  LDotDistance    : Integer;
  LColorLUT       : array of TColor32;
  LDrawingColor   : TColor32;
begin
  ABitmap.BeginUpdate;
  try
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    // get the top and bottom point
    if AY1 < AY2 then
    begin
      LTopY    := AY1;
      LBottomY := AY2;
    end
    else
    begin
      LTopY    := AY2;
      LBottomY := AY1;
    end;

    LDotDistance := Abs(LBottomY - LTopY) div 2;

    // add one more gradient step for filling the gap
    // caused by calculation error
    AColorGradient.GradientLength := LDotDistance + 1;
    AColorGradient.RefreshColorArray;

    SetLength(LColorLUT, LDotDistance + 1);
    try
      if AReverse then
      begin
        // fill the left side and the right side by start color
        LDrawingColor := AColorGradient.AbsolutStartColor;

        ABitmap.FillRectS( Rect(0, 0, LWidth, LTopY ), LDrawingColor );
        ABitmap.FillRectS( Rect(0, LBottomY, LWidth, LHeight), LDrawingColor );

        for i := 0 to LDotDistance do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[LDotDistance - i];
        end;
      end
      else
      begin
        // fill the left side and the right side by end color
        LDrawingColor := AColorGradient.AbsolutEndColor;

        ABitmap.FillRectS( Rect(0, 0, LWidth, LTopY ), LDrawingColor );
        ABitmap.FillRectS( Rect(0, LBottomY, LWidth, LHeight), LDrawingColor );

        for i := 0 to LDotDistance do
        begin
          LColorLUT[i] := AColorGradient.OutputColors[i];
        end;
      end;
      
      // draw gradient horizontal line
      for i := 0 to LDotDistance do
      begin
        { The drawing direction is two-way, if the DotDistance is odd,
          draw the gradient part by the color from StartColor to EndColor,
          the drawing direction is from the inner to the outer until
          the two edges of the gradient part. if the DotDistance is Even,
          draw the gradient part by the color from EndColor to StartColor,
          the drawing direction is from the outer to the inner until
          the center of the gradient part. }
          
        if Odd(LDotDistance) then
        begin
          LDrawingColor := LColorLUT[i];

          // draw gradient horizontal lines
          ABitmap.LineS(0, LTopY + LDotDistance - i,
                        LWidth, LTopY + LDotDistance - i,
                        LDrawingColor);

          ABitmap.LineS(0, LTopY + LDotDistance + i,
                        LWidth, LTopY + LDotDistance + i,
                        LDrawingColor);
        end
        else
        begin
          LDrawingColor := LColorLUT[LDotDistance - i];

          // draw gradient horizontal lines
          ABitmap.LineS(0, LTopY + i, LWidth, LTopY + i, LDrawingColor);
          ABitmap.LineS(0, LBottomY - i, LWidth, LBottomY - i, LDrawingColor);
        end;
      end;

    finally
      SetLength(LColorLUT, 0);
      LColorLUT := nil;
    end;
    
  finally
    ABitmap.EndUpdate;
  end;
end; 

// reflected gradient with slop fill
procedure SlopReflectedGradientFill(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
var
  i                  : Integer;
  LWidth, LHeight    : Integer;
  LLeftSlopeTopX     : Integer;
  LLeftSlopeBottomX  : Integer;
  LRightSlopeTopX    : Integer;
  LRightSlopeBottomX : Integer;
  LDotDistance       : Integer;
  LSlope             : Single;
  LLeftPoint         : TPoint;
  LRightPoint        : TPoint;
  LColorLUT          : array of TColor32;
  LDrawingColor      : TColor32;
begin
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then
  begin
    LWidth  := ABitmap.Width;
    LHeight := ABitmap.Height;

    ABitmap.BeginUpdate;
    try
      LSlope := Abs(AEndPoint.Y - AStartPoint.Y) / Abs(AEndPoint.X - AStartPoint.X);

      // get the left and right point
      if AStartPoint.X < AEndPoint.X then
      begin
        LLeftPoint  := AStartPoint;
        LRightPoint := AEndPoint;
      end
      else
      begin
        LLeftPoint  := AEndPoint;
        LRightPoint := AStartPoint;
      end;

      // slop line that down to left...
      if LLeftPoint.Y < LRightPoint.Y then 
      begin
        LLeftSlopeTopX     := LLeftPoint.X  + Round(LLeftPoint.Y * LSlope);
        LLeftSlopeBottomX  := LLeftPoint.X  - Round( (LHeight - LLeftPoint.Y) * LSlope );

        LRightSlopeTopX    := LRightPoint.X + Round(LRightPoint.Y * LSlope);
        LRightSlopeBottomX := LRightPoint.X - Round( (LHeight - LRightPoint.Y) * LSlope );

        { No matter the StartPoint is at top left, and the EndPoint
          is at bottom right; or the StartPoint is at bottom right,
          and the EndPoint is at top left, draw the top left side and
          the Bottom Right side by the EndColor. }

        if AReverse then
        begin
          LDrawingColor := AColorGradient.AbsolutStartColor;
        end
        else
        begin
          LDrawingColor := AColorGradient.AbsolutEndColor;
        end;

        for i := 0 to LLeftSlopeTopX do
        begin
          // draw the right slope lines at the top left side
          ABitmap.LineS(LLeftSlopeTopX - i, 0,
                        LLeftSlopeBottomX - i, LHeight,
                        LDrawingColor);
        end;

        for i := 0 to (LWidth - LRightSlopeBottomX) do
        begin
          // draw the right slope lines at the bottom right side
          ABitmap.LineS(LRightSlopeTopX + i, 0,
                        LRightSlopeBottomX + i, LHeight,
                        LDrawingColor);
        end;

        LDotDistance := (LRightSlopeTopX - LLeftSlopeTopX) div 2;

        // add one more gradient step for filling the gap
        // caused by calculation error
        AColorGradient.GradientLength := LDotDistance + 1;
        AColorGradient.RefreshColorArray;

        SetLength(LColorLUT, LDotDistance + 1);
        try
          if AReverse then
          begin
            for i := 0 to LDotDistance do
            begin
              LColorLUT[i] := AColorGradient.OutputColors[LDotDistance - i];
            end;
          end
          else
          begin
            for i := 0 to LDotDistance do
            begin
              LColorLUT[i] := AColorGradient.OutputColors[i];
            end;
          end;

          // draw gradient line
          for i := 0 to LDotDistance do
          begin
            { The drawing direction is two-way, if the DotDistance is odd,
              draw the gradient part by the color from StartColor to EndColor,
              the drawing direction is from the inner to the outer until
              the two edges of the gradient part. if the DotDistance is Even,
              draw the gradient part by the color from EndColor to StartColor,
              the drawing direction is from the outer to the inner until
              the center of the gradient part. }

            if Odd(LDotDistance) then
            begin
              LDrawingColor := LColorLUT[i];

              // draw gradient right slope lines
              ABitmap.LineS(LLeftSlopeTopX + LDotDistance - i, 0,
                            LLeftSlopeBottomX + LDotDistance - i, LHeight,
                            LDrawingColor);

              ABitmap.LineS(LLeftSlopeTopX + LDotDistance + i, 0,
                            LLeftSlopeBottomX + LDotDistance + i, LHeight,
                            LDrawingColor);
            end
            else
            begin
              LDrawingColor := LColorLUT[LDotDistance - i];

              // draw gradient right slope lines
              ABitmap.LineS(LLeftSlopeTopX + i, 0,
                            LLeftSlopeBottomX + i, LHeight,
                            LDrawingColor);

              ABitmap.LineS(LRightSlopeTopX - i, 0,
                            LRightSlopeBottomX - i, LHeight,
                            LDrawingColor);
            end;
          end;

        finally
          SetLength(LColorLUT, 0);
          LColorLUT := nil;
        end;
      end
      else
      if LLeftPoint.Y > LRightPoint.Y then  // slope to left
      begin
        LLeftSlopeTopX     := LLeftPoint.X   - Round(LLeftPoint.Y * LSlope);
        LLeftSlopeBottomX  := LLeftPoint.X   + Round( (LHeight - LLeftPoint.Y) * LSlope );

        LRightSlopeTopX    := LRightPoint.X  - Round(LRightPoint.Y * LSlope);
        LRightSlopeBottomX := LRightPoint.X  + Round( (LHeight - LRightPoint.Y) * LSlope );

        { No matter the StartPoint is at bottom left, and the EndPoint
          is at top right; or the StartPoint is at top right, and the
          EndPoint is at bottom left, draw the bottom left side and
          the bottom left side by the EndColor. }

        if AReverse then
        begin
          LDrawingColor := AColorGradient.AbsolutStartColor;
        end
        else
        begin
          LDrawingColor := AColorGradient.AbsolutEndColor;
        end;

        for i := 0 to LLeftSlopeBottomX do
        begin
          // draw the left slope lines at the bottom_left side
          ABitmap.LineS(LLeftSlopeTopX - i, 0,
                        LLeftSlopeBottomX - i, LHeight,
                        LDrawingColor);
        end;

        for i := 0 to (LWidth - LRightSlopeTopX) do
        begin
          // draw the left slope lines at top_right side
          ABitmap.LineS(LRightSlopeTopX + i, 0,
                        LRightSlopeBottomX + i, LHeight,
                        LDrawingColor);
        end;

        LDotDistance := (LRightSlopeTopX - LLeftSlopeTopX) div 2;

        // add one more gradient step for filling the gap
        // caused by calculation error
        AColorGradient.GradientLength := LDotDistance + 1;
        AColorGradient.RefreshColorArray;

        SetLength(LColorLUT, LDotDistance + 1);
        try
          if AReverse then
          begin
            for i := 0 to LDotDistance do
            begin
              LColorLUT[i] := AColorGradient.OutputColors[LDotDistance - i];
            end;
          end
          else
          begin
            for i := 0 to LDotDistance do
            begin
              LColorLUT[i] := AColorGradient.OutputColors[i];
            end;
          end;

          // draw gradient line
          for i := 0 to LDotDistance do
          begin
            { The drawing direction is two-way, if the DotDistance is odd,
              draw the gradient part by the color from StartColor to EndColor,
              the drawing direction is from the inner to the outer until
              the two edges of the gradient part. if the DotDistance is Even,
              draw the gradient part by the color from EndColor to StartColor,
              the drawing direction is from the outer to the inner until
              the center of the gradient part. }

            if Odd(LDotDistance) then
            begin
              LDrawingColor := LColorLUT[i];

              // draw gradient left slope lines
              ABitmap.LineS(LLeftSlopeTopX + LDotDistance - i, 0,
                            LLeftSlopeBottomX + LDotDistance - i, LHeight,
                            LDrawingColor);

              ABitmap.LineS(LLeftSlopeTopX + LDotDistance + i, 0,
                            LLeftSlopeBottomX + LDotDistance + i, LHeight,
                            LDrawingColor);
            end
            else
            begin
              LDrawingColor := LColorLUT[LDotDistance - i];

              // draw gradient left slope lines
              ABitmap.LineS(LLeftSlopeTopX + i, 0,
                            LLeftSlopeBottomX + i, LHeight,
                            LDrawingColor);

              ABitmap.LineS(LRightSlopeTopX - i, 0,
                            LRightSlopeBottomX - i, LHeight,
                            LDrawingColor);
            end;
          end;
          
        finally
          SetLength(LColorLUT, 0);
          LColorLUT := nil;
        end;
      end;

    finally
      ABitmap.EndUpdate;
    end;
  end;
end;

// Reflected Gradient
procedure DrawReflectedGradient(const ABitmap: TBitmap32;
  const AStartPoint, AEndPoint: TPoint; const AColorGradient: TgmGradientItem;
  const AReverse: Boolean);
begin
  if ( not Assigned(AColorGradient) ) or
     ( not Assigned(ABitmap) )  then
  begin
    Exit;
  end;

  // do nothing if the start point equals to end point
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    Exit;
  end;

  // horizontal gradient
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y = AEndPoint.Y) then
  begin
    HorizReflectedGradientFill(ABitmap, AStartPoint.X, AEndPoint.X,
                               AColorGradient, AReverse);
  end
  else
  if (AStartPoint.X = AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then // vertical gradient
  begin
    VertReflectedGradientFill(ABitmap, AStartPoint.Y, AEndPoint.Y,
                              AColorGradient, AReverse);
  end
  else
  if (AStartPoint.X <> AEndPoint.X) and
     (AStartPoint.Y <> AEndPoint.Y) then // slope gradient
  begin
    SlopReflectedGradientFill(ABitmap, AStartPoint, AEndPoint, AColorGradient,
                              AReverse);
  end; 
end;


end.
