unit gmRichTextLayer;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is gmRichTextLayer.pas.
 *
 * The Initial Developer of the Original Code are
 * Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/11/22

interface

uses
{ Standard }
  Classes,
  ComCtrls,        // TRichEdit
  Graphics,
{ Graphics32 }
  GR32,
{ GraphicsMagic lib }
  gmCrop,
  gmLayers,
  gmResamplers,
  gmTypes;

type
  // indicate the text layer state
  TgmTextLayerState = (tlsNew, tlsModify);

  { TgmRichTextLayer }

  TgmRichTextLayer = class(TgmSpecialPixelizedLayer)
  private
    FAssociatedTextEditor : TRichEdit;
    FTextFileName         : string;
    FEditState            : Boolean; // indicate whether the layer is on edit state
    FTextChanged          : Boolean;
    FTextLayerState       : TgmTextLayerState;
    FRichTextStream       : TMemoryStream;
    FBorderStart          : TPoint;
    FBorderEnd            : TPoint;

    function GetBorderWidth: Integer;
    function GetBorderHeight: Integer;
    function GetBorderRect: TRect;
  protected
    procedure LayerBlend(F: TColor32; var B: TColor32; M: TColor32); override;
  public
    constructor Create(AOwner: TgmLayerList;
      const ALayerWidth, ALayerHeight: Integer);

    destructor Destroy; override;

    procedure AssociateTextEditor(AEditor: TRichEdit);
    procedure DrawTextBorder(ACanvas: TCanvas; const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
    procedure DrawTextBorderHandles(ACanvas: TCanvas; const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
    procedure DrawTextOnLayer;
    procedure SetTextBorder(const AStartPoint, AEndPoint: TPoint);
    procedure StandardizeBorderEndingOrder;
    procedure Translate(const ATranslateVector: TPoint);

    procedure SaveTextToLayer(AStrings: TStrings);
    procedure UpdateContentToAssociatedTextEditor;

    procedure CropLayer(ACrop: TgmCrop; const ABackColor: TColor32); override;
    procedure CropLayerRect(const ACropArea: TRect; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerCanvas(const ANewWidth, ANewHeight: Integer;
      const AAnchor: TgmAnchorDirection; const ABackgroundColor: TColor32); override;

    procedure ResizeLayerImage(const ANewWidth, ANewHeight: Integer;
      const ASamplingOptions: TgmResamplingOptions); override;

    procedure RotateCanvas(const ADegrees: Integer;
      const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32); override;

    // determine whether the mouse pointer is within the text border
    function ContainsPoint(const ATestPoint: TPoint;
      const ACoordConvertFunc: TgmPointCoordConvertFunc = nil):  Boolean;

    // determine whether the mouse pointer is over a handle of the text border
    function GetHandleAtPoint(const AX, AY: Integer;
      const ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;

    function GetCopy: TgmCustomLayer; override;

    property AssociatedTextEditor : TRichEdit         read FAssociatedTextEditor;
    property BorderEnd            : TPoint            read FBorderEnd      write FBorderEnd;
    property BorderHeight         : Integer           read GetBorderHeight;
    property BorderRect           : TRect             read GetBorderRect;
    property BorderStart          : TPoint            read FBorderStart    write FBorderStart;
    property BorderWidth          : Integer           read GetBorderWidth;
    property IsEditState          : Boolean           read FEditState      write FEditState;
    property IsTextChanged        : Boolean           read FTextChanged    write FTextChanged;
    property RichTextStream       : TMemoryStream     read FRichTextStream;
    property TextFileName         : string            read FTextFileName   write FTextFileName;
    property TextLayerState       : TgmTextLayerState read FTextLayerState write FTextLayerState;
  end;

implementation

uses
{ Standard }
  Math,
  Windows,
{ externals }
  LineLibrary,
{ GraphicsMagic Lib }
  gmCommonFuncs,
  gmConstants,
  gmGUIFuncs,
  gmMath,
  gmPaintFuncs;

{$R gmRichTextLayerIcons.res}

{ TgmRichTextLayer }

constructor TgmRichTextLayer.Create(AOwner: TgmLayerList;
  const ALayerWidth, ALayerHeight: Integer);
begin
  inherited Create(AOwner, ALayerWidth, ALayerHeight);

  FDefaultLayerName := 'Text';
  FLogoThumbEnabled := True;

  FAssociatedTextEditor := nil;
  FRichTextStream       := TMemoryStream.Create();
  FBorderStart          := Point(0, 0);
  FBorderEnd            := Point(0, 0);
  FTextLayerState       := tlsNew;
  FTextFileName         := '';
  FTextChanged          := False;
  FEditState            := False;

  FLogoBitmap := TBitmap32.Create();
  FLogoBitmap.LoadFromResourceName(HInstance, 'RICHTEXTLAYERLOGO');

  FLogoThumb := TBitmap32.Create();
  with FLogoThumb do
  begin
    SetSize(LAYER_LOGO_SIZE, LAYER_LOGO_SIZE);
  end;

  UpdateLogoThumbnail();
end;

destructor TgmRichTextLayer.Destroy;
begin
  FRichTextStream.Free();
  
  inherited;
end;

procedure TgmRichTextLayer.AssociateTextEditor(AEditor: TRichEdit);
begin
  if Assigned(AEditor) then
  begin
    FAssociatedTextEditor := AEditor;
  end;
end;

// determine whether the mouse pointer is within the text border
function TgmRichTextLayer.ContainsPoint(const ATestPoint: TPoint;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  LRect       : TRect;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  if Assigned(ACoordConvertFunc) then
  begin
    LStartPoint := ACoordConvertFunc(FBorderStart);
    LEndPoint   := ACoordConvertFunc(FBorderEnd);
  end
  else
  begin
    LStartPoint := FBorderStart;
    LEndPoint   := FBorderEnd;
  end;
  
  LRect  := Rect(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y);
  Result := Windows.PtInRect(LRect, ATestPoint);
end;

procedure TgmRichTextLayer.CropLayer(ACrop: TgmCrop;
  const ABackColor: TColor32);
var
  LOffsetVector : TPoint;
begin
  if Assigned(ACrop) then
  begin
    inherited;
    
    LOffsetVector := SubtractPoints( Point(0, 0), ACrop.FCropStart );

    Self.Translate(LOffsetVector);
    DrawTextOnLayer();
  end;
end;

procedure TgmRichTextLayer.CropLayerRect(const ACropArea: TRect;
  const ABackgroundColor: TColor32);
var
  LCropRect     : TRect;
  LOffsetVector : TPoint;
begin
  if (ACropArea.Left = ACropArea.Right) or
     (ACropArea.Top = ACropArea.Bottom) then
  begin
    Exit;
  end;

  LCropRect.Left   := Min(ACropArea.Left, ACropArea.Right);
  LCropRect.Top    := Min(ACropArea.Top, ACropArea.Bottom);
  LCropRect.Right  := Max(ACropArea.Left, ACropArea.Right);
  LCropRect.Bottom := Max(ACropArea.Top, ACropArea.Bottom);

  inherited;
    
  LOffsetVector := SubtractPoints( Point(0, 0), LCropRect.TopLeft );

  Self.Translate(LOffsetVector);
  DrawTextOnLayer();
end;

procedure TgmRichTextLayer.DrawTextBorder(ACanvas: TCanvas;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LTempPenColor    : TColor;
  LTempBrushColor  : TColor;
  LTempPenWidth    : Integer;
  LTempPenStyle    : TPenStyle;
  LTempPenMode     : TPenMode;
  LTempBrushStyle  : TBrushStyle;
  LStartPt, LEndPt : TPoint;
begin
  if not Assigned(ACanvas) then
  begin
    Exit;
  end;

  if Assigned(ACoordConvertFunc) then
  begin
    LStartPt := ACoordConvertFunc(FBorderStart);
    LEndPt   := ACoordConvertFunc(FBorderEnd);
  end
  else
  begin
    LStartPt := FBorderStart;
    LEndPt   := FBorderEnd;
  end;

  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Style   := psDot;
    Pen.Width   := 1;
    Pen.Mode    := pmNotXor;
    Brush.Color := clWhite;
    Brush.Style := bsClear;
    
    Rectangle(LStartPt.X, LStartPt.Y, LEndPt.X, LEndPt.Y);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure TgmRichTextLayer.DrawTextBorderHandles(ACanvas: TCanvas;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i           : Integer;
  LCenterX    : Integer;
  LCenterY    : Integer;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
  LPoint      : TPoint;
begin
  if Assigned(ACoordConvertFunc) then
  begin
    LStartPoint := ACoordConvertFunc(FBorderStart);
    LEndPoint   := ACoordConvertFunc(FBorderEnd);
  end
  else
  begin
    LStartPoint := FBorderStart;
    LEndPoint   := FBorderEnd;
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

procedure TgmRichTextLayer.DrawTextOnLayer;
begin
  if Assigned(FAssociatedTextEditor) then
  begin
    FLayerBitmap.Clear($00FFFFFF);
    DrawRichTextOnBitmap(FLayerBitmap, Self.BorderRect, FAssociatedTextEditor);
  end;
end;

function TgmRichTextLayer.GetBorderHeight: Integer;
begin
  Result := Abs(FBorderEnd.Y - FBorderStart.Y);
end;

function TgmRichTextLayer.GetBorderRect: TRect;
begin
  Result := Rect(FBorderStart.X, FBorderStart.Y, FBorderEnd.X, FBorderEnd.Y);
end;

function TgmRichTextLayer.GetBorderWidth: Integer;
begin
  Result := Abs(FBorderEnd.X - FBorderStart.X);
end;

function TgmRichTextLayer.GetCopy: TgmCustomLayer;
var
  LLayer : TgmRichTextLayer;
begin
  LLayer := TgmRichTextLayer.Create(Self.FOwner,
    Self.FLayerBitmap.Width, Self.FLayerBitmap.Height);

  Self.CopyCommonDataTo(LLayer);

  LLayer.AssociateTextEditor(Self.FAssociatedTextEditor);
  LLayer.TextFileName  := Self.FTextFileName;
  LLayer.FBorderStart  := Self.FBorderStart;
  LLayer.FBorderEnd    := Self.FBorderEnd;
  LLayer.IsTextChanged := Self.FTextChanged;

  Self.FRichTextStream.Position := 0;
  try
    LLayer.FRichTextStream.LoadFromStream(Self.FRichTextStream);
  finally
    Self.FRichTextStream.Position   := 0;
    LLayer.FRichTextStream.Position := 0;
  end;
  
  Result := LLayer;
end;

// determine whether the mouse pointer is over a handle of the text border
function TgmRichTextLayer.GetHandleAtPoint(const AX, AY: Integer;
  const ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;
var
  LBorderStart : TPoint;
  LBorderEnd   : TPoint;
  LCenterX     : Integer;
  LCenterY     : Integer;
begin
  Result := dhNone;

  if (FBorderStart.X = FBorderEnd.X) and
     (FBorderStart.Y = FBorderEnd.Y) then
  begin
    Exit;
  end;

  if Assigned(ACoordConvertFunc) then
  begin
    LBorderStart := ACoordConvertFunc(FBorderStart);
    LBorderEnd   := ACoordConvertFunc(FBorderEnd);
  end
  else
  begin
    LBorderStart := FBorderStart;
    LBorderEnd   := FBorderEnd;
  end;

  LCenterX := (LBorderStart.X + LBorderEnd.X) div 2;
  LCenterY := (LBorderStart.Y + LBorderEnd.Y) div 2;

  if SquareContainsPoint( LBorderStart, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxAy;
  end
  else
  if SquareContainsPoint( LBorderEnd, HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxBy;
  end
  else
  if SquareContainsPoint( Point(LBorderStart.X, LBorderEnd.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhAxBy;
  end
  else
  if SquareContainsPoint( Point(LBorderEnd.X, LBorderStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBxAy;
  end
  else
  if SquareContainsPoint( Point(LBorderStart.X, LCenterY), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhLeftHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(LBorderEnd.X, LCenterY), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhRightHalfAYBY;
  end
  else
  if SquareContainsPoint( Point(LCenterX, LBorderStart.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAXBX;
  end
  else
  if SquareContainsPoint( Point(LCenterX, LBorderEnd.Y), HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAXBX;
  end;
end;

procedure TgmRichTextLayer.LayerBlend(F: TColor32;
  var B: TColor32; M: TColor32);
begin
  FLayerBlendEvent(F, B, M);
end;

procedure TgmRichTextLayer.ResizeLayerCanvas(
  const ANewWidth, ANewHeight: Integer; const AAnchor: TgmAnchorDirection;
  const ABackgroundColor: TColor32);
var
  LOffsetVector         : TPoint;
  LOldWidth, LOldHeight : Integer;
begin
  LOldWidth  := FLayerBitmap.Width;
  LOldHeight := FLayerBitmap.Height;

  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (LOldWidth <> ANewWidth) or (LOldHeight <> ANewHeight) then
    begin
      inherited;

      LOffsetVector := CalcOffsetCoordinateByAnchorDirection(
        LOldWidth, LOldHeight, ANewWidth, ANewHeight, AAnchor);

      Self.Translate(LOffsetVector);
      DrawTextOnLayer();
    end;
  end;
end;

procedure TgmRichTextLayer.ResizeLayerImage(
  const ANewWidth, ANewHeight: Integer;
  const ASamplingOptions: TgmResamplingOptions);
begin
  if (ANewWidth > 0) and (ANewHeight > 0) then
  begin
    if (FLayerBitmap.Width <> ANewWidth) or
       (FLayerBitmap.Height <> ANewHeight) then
    begin
      inherited;
      DrawTextOnLayer();
    end;
  end;
end;

procedure TgmRichTextLayer.RotateCanvas(const ADegrees: Integer;
  const ADirection: TgmRotateDirection; const ABackgroundColor: TColor32);
begin
  inherited;
  DrawTextOnLayer();
end;

procedure TgmRichTextLayer.SaveTextToLayer(AStrings: TStrings);
begin
  if Assigned(AStrings) then
  begin
    FRichTextStream.Clear;
    AStrings.SaveToStream(FRichTextStream);

    FRichTextStream.Position := 0;
    FTextChanged             := False;
  end;
end;

procedure TgmRichTextLayer.SetTextBorder(const AStartPoint, AEndPoint: TPoint);
begin
  FBorderStart := AStartPoint;
  FBorderEnd   := AEndPoint;
end;

// make sure the starting point of the text border is always at top left
procedure TgmRichTextLayer.StandardizeBorderEndingOrder;
var
  LTempA, LTempB : TPoint;
begin
  LTempA := FBorderStart;
  LTempB := FBorderEnd;

  // FBorderStart is at the upper left.
  FBorderStart.X := MinIntValue([LTempA.X, LTempB.X]);
  FBorderStart.Y := MinIntValue([LTempA.Y, LTempB.Y]);
  
  // FBorderEnd is at the lower right.
  FBorderEnd.X := MaxIntValue([LTempA.X, LTempB.X]);
  FBorderEnd.Y := MaxIntValue([LTempA.Y, LTempB.Y]);
end;

procedure TgmRichTextLayer.Translate(const ATranslateVector: TPoint);
begin
  FBorderStart := AddPoints(FBorderStart, ATranslateVector);
  FBorderEnd   := AddPoints(FBorderEnd,   ATranslateVector);
end;

procedure TgmRichTextLayer.UpdateContentToAssociatedTextEditor;
begin
  if Assigned(FAssociatedTextEditor) then
  begin
    FAssociatedTextEditor.Clear;

    if FRichTextStream.Size > 0 then
    begin
      FRichTextStream.Position := 0;
      try
        FAssociatedTextEditor.Lines.LoadFromStream(FRichTextStream);
      finally
        FRichTextStream.Position := 0;
      end;
    end;
  end;
end;


end.
