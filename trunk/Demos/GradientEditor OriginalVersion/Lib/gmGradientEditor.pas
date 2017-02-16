unit gmGradientEditor;

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
 * x2nie - Fathony Luthfillah  <x2nie@yahoo.com>
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
{ Standard }
  Controls, Classes, SysUtils,            
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmGradient, gmStopObject, gmGeometricObjects2D;

type
  TgmGradientEditorState = (gesIdle,
                            gesPrimaryColorSelected,
                            gesColorMidPointSelected,
                            gesPrimaryAlphaSelected,
                            gesAlphaMidPointSelected);

  TgmEditorActionType = (eatDoNothing,
                         eatSelectingPrimaryColor,
                         eatSelectingColorMidPoint,
                         eatDragColorOut,
                         eatSelectingPrimaryAlpha,
                         eatSelectingAlphaMidPoint,
                         eatDragAlphaOut);

  TgmEditorCursorPosition = (ecpOutOfEnable,
                             ecpOnStop,
                             ecpOnMidPoint,
                             ecpOnAddStopArea);

  TgmEditorCursorPosChangedEvent = procedure (const ACursorPosition: TgmEditorCursorPosition) of object;
  TgmEditorStateChangedEvent     = procedure (const AState: TgmGradientEditorState) of object;

  TgmGradientEditor = class(TObject)
  private
    FOwner                : TCustomImage32;
    FState                : TgmGradientEditorState;
    FActionType           : TgmEditorActionType;
    FPreviewMap           : TBitmap32;
    FGradientMap          : TBitmap32;
    FGradient             : TgmColorGradient;
    FAlphaStopList        : TgmStopObjectList;
    FColorStopList        : TgmStopObjectList;
    FSpectrumWidth        : Integer;
    FSpectrumHeight       : Integer;
    FSpectrumRect         : TRect;
    FAlphaStopProcessArea : TRect;
    FColorStopProcessArea : TRect;
    FStopOffset           : Integer;
    FStopHeight           : Integer;
    FSelectedAlphaIndex   : Integer;
    FSelectedColorIndex   : Integer;
    FLastHorizMove        : Integer;
    FBackColor            : TColor32;
    FDefaultNewColor      : TColor32;
    FDefaultNewColorType  : TgmGradientColorType;
    FDefaultNewAlpha      : Byte;
    FMouseButtonDown      : Boolean;

    // used to select a mid point
    FLeftMidPointMark     : TgmSimpleDiamond2D;
    FRightMidPointMark    : TgmSimpleDiamond2D;
    FSelectedMidPointMark : TgmSimpleDiamond2D;    // pointer to left/right mid point mark

    FLeftRGBGradient      : TgmSimpleRGBGradient;  // pointer to a simple RGB gradient that at left of the selected color
    FRightRGBGradient     : TgmSimpleRGBGradient;  // pointer to a simple RGB gradient that at right of the selected color
    FSelectedRGBGradient  : TgmSimpleRGBGradient;  // pointer to left/right simple RGB gradient

    FLeftAlphaGradient    : TgmSimpleAlphaGradient;
    FRightAlphaGradient   : TgmSimpleAlphaGradient;
    FSelectedAlphaGradient: TgmSimpleAlphaGradient;

    // used to drag color in/out to/from the gradient
    FDragOutColor         : TColor32;
    FDragOutAlpha         : Byte;
    FDrawOutColorType     : TgmGradientColorType;
    FLeftDragOutMidScale  : Single;
    FRightDragOutMidScale : Single;

    // call back functions
    FOnCursorPosChanged   : TgmEditorCursorPosChangedEvent; // a callback for change cursor from external
    FOnStateChanged       : TgmEditorStateChangedEvent;

    function GetStopOffset: Integer;
    function GetSpectrumRect: TRect;
    function GetAlphaStopProcessArea: TRect;
    function GetColorStopProcessArea: TRect;
    function GetCursorPostionType(const AX, AY: Integer): TgmEditorCursorPosition;
    function PointOnColorMidPoint(const AColorIndex, AX, AY: Integer): TgmSimpleDiamond2D;
    function PointOnAlphaMidPoint(const AAlphaIndex, AX, AY: Integer): TgmSimpleDiamond2D;
    function GetAlphaValueCount: Integer;
    function GetColorCount: Integer;

    procedure UpdateAlphaStopList;
    procedure UpdateColorStopList;
    procedure UpdateMidPointMarks;
    procedure DrawMidPointMarks;
    procedure SetColorGradient(const AValue: TgmColorGradient);
    procedure SetSpectrumWidth(const AValue: Integer);
    procedure SetSpectrumHeight(const AValue: Integer);
    procedure CancelSelection;

    procedure ProcessColorGradientWhenMouseDown(const AX, AY: Integer);
    procedure ProcessAlphaGradientWhenMouseDown(const AX, AY: Integer);
  public
    constructor Create(const AOwner: TCustomImage32);
    destructor Destroy; override;

    function ChangeSelectedPrimaryColor(const AColor: TColor32): Boolean;
    function ChangeSelectedPrimaryColorLocationScale(const AScale: Single): Boolean;
    function ChangeSelectedColorMidPointScale(const AScale: Single): Boolean;
    function ChangeSelectedAlphaValue(const AValue: Byte): Boolean;
    function ChangeSelectedAlphaLocationScale(const AScale: Single): Boolean;
    function ChangeSelectedAlphaMidPointScale(const AScale: Single): Boolean;

    function GetSelectedPrimaryColorLocationScale: Single;
    function GetSelectedColorMidPointScale: Single;
    function GetSelectedAlphaLocationScale: Single;
    function GetSelectedAlphaMidPointScale: Single;

    function DeleteSelectedPrimaryColor: Boolean;
    function DeleteSelectedAlphaValue: Boolean;

    procedure UpdatePreview;
    procedure AverageColorLocationScales;
    procedure AverageAlphaLocationScales;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer; Layer: TCustomLayer);

    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    property IsMouseButtonDown : Boolean                        read FMouseButtonDown;
    property State             : TgmGradientEditorState         read FState;
    property AlphaValueCount   : Integer                        read GetAlphaValueCount;
    property ColorCount        : Integer                        read GetColorCount;
    property ColorGradient     : TgmColorGradient               read FGradient           write SetColorGradient;
    property SpectrumWidth     : Integer                        read FSpectrumWidth      write SetSpectrumWidth;
    property SpectrumHeight    : Integer                        read FSpectrumHeight     write SetSpectrumHeight;
    property DefaultNewColor   : TColor32                       read FDefaultNewColor    write FDefaultNewColor;
    property DefaultNewAlpha   : Byte                           read FDefaultNewAlpha    write FDefaultNewAlpha;
    property OnCursorPosChanged: TgmEditorCursorPosChangedEvent read FOnCursorPosChanged write FOnCursorPosChanged;
    property OnStateChanged    : TgmEditorStateChangedEvent     read FOnStateChanged     write FOnStateChanged;
  end;

  
implementation

uses
{ Standard }
  Graphics,
{ GraphicsMagic Lib }
  gmMiscFuncs;

const
  STOP_HEAD_HEIGHT        = 10;
  DEFAULT_SPECTRUM_HEIGHT = 26;
  DEFAULT_SPECTRUM_WIDTH  = 384;
  MIN_SPECTRUM_HEIGHT     = 10;
  MIN_SPECTRUM_WIDTH      = 100;
  PREVIEW_BORDER_WIDTH    = 1;
  MID_POINT_MARK_RADIUS   = 4;

constructor TgmGradientEditor.Create(const AOwner: TCustomImage32);
begin
  inherited Create;

  if not Assigned(AOwner) then
  begin
    raise Exception.Create('TgmGradientEditor.Create() -- AOwner is nil.');
  end;

  FOwner               := AOwner;
  FState               := gesIdle;
  FActionType          := eatDoNothing;
  FAlphaStopList       := TgmStopObjectList.Create;
  FColorStopList       := TgmStopObjectList.Create;
  FPreviewMap          := TBitmap32.Create;
  FGradientMap         := TBitmap32.Create;
  FStopOffset          := GetStopOffset;
  FStopHeight          := FStopOffset * 2 + STOP_HEAD_HEIGHT;
  FSelectedAlphaIndex  := -1;
  FSelectedColorIndex  := -1;
  FBackColor           := Color32(clBtnFace);
  FDefaultNewColor     := clBlack32;
  FDefaultNewAlpha     := 255;
  FDefaultNewColorType := gctStaticColor;
  FLastHorizMove       := 0;
  FMouseButtonDown     := False;

  // default gradient...
  FGradient := TgmColorGradient.Create;
  FGradient.RGBGradient.InsertColor(0.25, $FFFF0000);
  FGradient.RGBGradient.InsertColor(0.5, $FF00FF00);
  FGradient.RGBGradient.InsertColor(0.75, $FF0000FF);
  FGradient.Name := 'Custom';

  // used to select a mid point
  FLeftMidPointMark      := TgmSimpleDiamond2D.Create(MID_POINT_MARK_RADIUS, MID_POINT_MARK_RADIUS);
  FRightMidPointMark     := TgmSimpleDiamond2D.Create(MID_POINT_MARK_RADIUS, MID_POINT_MARK_RADIUS);
  FSelectedMidPointMark  := nil;

  FLeftRGBGradient       := nil; // pointer to a simple RGB gradient that at left of the selected color
  FRightRGBGradient      := nil; // pointer to a simple RGB gradient that at right of the selected color
  FSelectedRGBGradient   := nil; // pointer to left/right simple RGB gradient

  FLeftAlphaGradient     := nil;
  FRightAlphaGradient    := nil;
  FSelectedAlphaGradient := nil;

  // set up with properties
  SpectrumWidth  := FOwner.Width - 2 * FStopOffset;
  SpectrumHeight := FOwner.Height - 2 * FStopHeight -1;

  FSpectrumRect         := GetSpectrumRect;
  FAlphaStopProcessArea := GetAlphaStopProcessArea;
  FColorStopProcessArea := GetColorStopProcessArea;

  // used to drag color in/out to/from the gradient
  FDragOutColor         := clBlack32;
  FDragOutAlpha         := 0;
  FDrawOutColorType     := gctNone;
  FLeftDragOutMidScale  := 0.0;
  FRightDragOutMidScale := 0.0;

  // callback functions
  FOnCursorPosChanged := nil;
  FOnStateChanged     := nil;

  UpdateAlphaStopList;
  UpdateColorStopList;
end; { Create }

destructor TgmGradientEditor.Destroy;
begin
  FSelectedMidPointMark := nil;

  FLeftRGBGradient     := nil;
  FRightRGBGradient    := nil;
  FSelectedRGBGradient := nil;

  FLeftAlphaGradient     := nil;
  FRightAlphaGradient    := nil;
  FSelectedAlphaGradient := nil;

  FOnCursorPosChanged := nil;
  FOnStateChanged     := nil;

  FGradient.Free;
  FLeftMidPointMark.Free;
  FRightMidPointMark.Free;
  FAlphaStopList.Free;
  FColorStopList.Free;
  FGradientMap.Free;
  FPreviewMap.Free;

  inherited Destroy;
end; { Destroy }

function TgmGradientEditor.GetStopOffset: Integer;
var
  LTempTriangle: TgmEquilateralTriangle2D;
begin
  LTempTriangle := TgmEquilateralTriangle2D.Create(Point(0, 0), Point(0, STOP_HEAD_HEIGHT));
  try
    Result := Round(LTempTriangle.EdgeLength / 2) + PREVIEW_BORDER_WIDTH;
  finally
    LTempTriangle.Free;
  end;
end; { GetStopOffset }

function TgmGradientEditor.GetSpectrumRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if (FSpectrumWidth > 0) and (FSpectrumHeight > 0) then
  begin
    Result := Rect(FStopOffset, FStopHeight,
                   FStopOffset + FSpectrumWidth + 1,
                   FStopHeight + FSpectrumHeight + 1);
  end;
end; { GetSpectrumRect }

function TgmGradientEditor.GetAlphaStopProcessArea: TRect;
begin
  Result := Rect(FSpectrumRect.Left, 0,
                 FSpectrumRect.Right, FSpectrumRect.Top);
end; { GetAlphaStopProcessArea }

function TgmGradientEditor.GetColorStopProcessArea: TRect;
begin
  Result := Rect(FSpectrumRect.Left, FSpectrumRect.Bottom,
                 FSpectrumRect.Right, FPreviewMap.Height);
end; { GetColorStopProcessArea }

function TgmGradientEditor.GetCursorPostionType(
  const AX, AY: Integer): TgmEditorCursorPosition;
var
  LPointOnControl: Boolean;
begin
  Result := ecpOutOfEnable;

  // if mouse cursor is in Alpha stop process area...
  if (AY >= FAlphaStopProcessArea.Top) and
     (AY <= FAlphaStopProcessArea.Bottom) then
  begin
    // check for whether the cursor is moved on any of alpha stops ...

    // if point on a alpha stop ...
    LPointOnControl := (FAlphaStopList.PointOnStops(AX, AY) >= 0);
    
    if LPointOnControl then
    begin
      Result := ecpOnStop;
    end
    else
    begin
      // if not point on a Alpha stop, check for whether point on a alpha mid point
      if FSelectedAlphaIndex >= 0 then
      begin
        LPointOnControl := ( PointOnAlphaMidPoint(FSelectedAlphaIndex, AX, AY) <> nil );
        Result          := ecpOnMidPoint;
      end;
    end;

    // if not point on a alpha stop or a alpha mid point...
    if not LPointOnControl then
    begin
      // if the cursor within the alpha stop process area,
      // indicating we could add a new primary alpha to the gradient
      if PtInRect( FAlphaStopProcessArea, Point(AX, AY) ) then
      begin
        Result := ecpOnAddStopArea;
      end;
    end;
  end
  else
  // if mouse cursor is in color stop process area...
  if (AY >= FColorStopProcessArea.Top) and
     (AY <= FColorStopProcessArea.Bottom) then
  begin
    // if point on a color stop
    LPointOnControl := ( FColorStopList.PointOnStops(AX, AY) >= 0 );

    if LPointOnControl then
    begin
      Result := ecpOnStop;
    end
    else
    begin
      // if not point on a color stop, check for whether point on a color mid point
      if FSelectedColorIndex >= 0 then
      begin
        LPointOnControl := ( PointOnColorMidPoint(FSelectedColorIndex, AX, AY) <> nil );
        Result          := ecpOnMidPoint;
      end;
    end;

    // if not point on a color stop or a color mid point...
    if not LPointOnControl then
    begin
      // if the cursor within the color stop process area,
      // indicating we could add a new primary color to the gradient
      if PtInRect( FColorStopProcessArea, Point(AX, AY) ) then
      begin
        Result := ecpOnAddStopArea;
      end;
    end;
  end;
end; { GetCursorPostionType }

procedure TgmGradientEditor.UpdateAlphaStopList;
var
  i            : Integer;
  LInverseAlpha: Byte;
  LStopObj     : TgmStopObject;
  LTipPos      : TPoint;
begin
  // update alpha stops
  FAlphaStopList.DeleteAllStopObjects;

  for i := 0 to (FGradient.AlphaGradient.AlphaValueCount - 1) do
  begin
    LTipPos.X := Round(FGradient.AlphaGradient.PrimaryScales[i] * FSpectrumWidth);
    LTipPos.Y := FStopHeight - PREVIEW_BORDER_WIDTH;

    LStopObj       := TgmStopObject.Create(LTipPos, STOP_HEAD_HEIGHT, sodSouth);
    LInverseAlpha  := 255 - FGradient.AlphaGradient.PrimaryAlphaValues[i];
    LStopObj.Color := Gray32(LInverseAlpha);

    FAlphaStopList.Add(LStopObj);
  end;
end; { UpdateAlphaStopList }

procedure TgmGradientEditor.UpdateColorStopList;
var
  i       : Integer;
  LStopObj: TgmStopObject;
  LTipPos : TPoint;
begin
  // update RGB stops
  FColorStopList.DeleteAllStopObjects;

  for i := 0 to (FGradient.RGBGradient.ColorCount - 1) do
  begin
    LTipPos.X := Round(FGradient.RGBGradient.PrimaryScales[i] * FSpectrumWidth);
    LTipPos.Y := FPreviewMap.Height - FStopHeight - PREVIEW_BORDER_WIDTH;

    LStopObj          := TgmStopObject.Create(LTipPos, STOP_HEAD_HEIGHT, sodNorth);
    LStopObj.Color    := FGradient.RGBGradient.PrimaryColors[i];
    LStopObj.StopType := TgmStopObjectType(FGradient.RGBGradient.PrimaryColorTypes[i]);

    FColorStopList.Add(LStopObj);
  end;
end; { UpdateColorStopList }

procedure TgmGradientEditor.UpdateMidPointMarks;
var
  LCenterX, LCenterY: Integer;
begin
  FLeftRGBGradient  := nil;
  FRightRGBGradient := nil;

  // when a primary color is selected...
  if FSelectedColorIndex >= 0 then
  begin
    LCenterY := FSpectrumRect.Bottom + MID_POINT_MARK_RADIUS + 1;

    if FSelectedColorIndex = 0 then
    begin
      LCenterX := FGradient.RGBGradient.MidPointLocation[FSelectedColorIndex];

      FRightMidPointMark.Center := Point(LCenterX, LCenterY);

      FRightRGBGradient := FGradient.RGBGradient.GetSimpleColorGradient(FSelectedColorIndex);
    end
    else
    if FSelectedColorIndex = (FGradient.RGBGradient.ColorCount - 1) then
    begin
      LCenterX := FGradient.RGBGradient.MidPointLocation[FSelectedColorIndex];

      FLeftMidPointMark.Center := Point(LCenterX, LCenterY);

      FLeftRGBGradient := FGradient.RGBGradient.GetSimpleColorGradient(FSelectedColorIndex);
    end
    else
    begin
      LCenterX := FGradient.RGBGradient.MidPointLocation[FSelectedColorIndex - 1];
      FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
      FLeftRGBGradient := FGradient.RGBGradient.GetSimpleColorGradient(FSelectedColorIndex - 1);

      LCenterX := FGradient.RGBGradient.MidPointLocation[FSelectedColorIndex];
      FRightMidPointMark.Center := Point(LCenterX, LCenterY);
      FRightRGBGradient := FGradient.RGBGradient.GetSimpleColorGradient(FSelectedColorIndex);
    end
  end
  else
  if FSelectedAlphaIndex >= 0 then // when a primary Alpha is selected...
  begin
    LCenterY := FSpectrumRect.Top - MID_POINT_MARK_RADIUS - 2;

    if FSelectedAlphaIndex = 0 then
    begin
      LCenterX := FGradient.AlphaGradient.MidPointLocation[FSelectedAlphaIndex];

      FRightMidPointMark.Center := Point(LCenterX, LCenterY);

      FRightAlphaGradient := FGradient.AlphaGradient.GetSimpleAlphaGradient(FSelectedAlphaIndex);
    end
    else
    if FSelectedAlphaIndex = (FGradient.AlphaGradient.AlphaValueCount - 1) then
    begin
      LCenterX := FGradient.AlphaGradient.MidPointLocation[FSelectedAlphaIndex];

      FLeftMidPointMark.Center := Point(LCenterX, LCenterY);

      FLeftAlphaGradient := FGradient.AlphaGradient.GetSimpleAlphaGradient(FSelectedAlphaIndex);
    end
    else
    begin
      LCenterX := FGradient.AlphaGradient.MidPointLocation[FSelectedAlphaIndex - 1];
      FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
      FLeftAlphaGradient := FGradient.AlphaGradient.GetSimpleAlphaGradient(FSelectedAlphaIndex - 1);

      LCenterX := FGradient.AlphaGradient.MidPointLocation[FSelectedAlphaIndex];
      FRightMidPointMark.Center := Point(LCenterX, LCenterY);
      FRightAlphaGradient := FGradient.AlphaGradient.GetSimpleAlphaGradient(FSelectedAlphaIndex);
    end
  end;
end; { UpdateMidPointMarks }

procedure TgmGradientEditor.DrawMidPointMarks;
var
  LInverseAlpha: Byte;
begin
  // draw mid points between color stops ...
  if FSelectedColorIndex >= 0 then
  begin
    // a mid point is selected ...
    if Assigned(FSelectedMidPointMark) then
    begin
      if Assigned(FSelectedRGBGradient) then
      begin
        FSelectedMidPointMark.FillColor := FSelectedRGBGradient.EndColor;
      end;

      FSelectedMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
    end
    else
    begin
      if FSelectedColorIndex = 0 then
      begin
        FRightMidPointMark.FillColor := $00000000;
        FRightMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end
      else
      if FSelectedColorIndex = (FGradient.RGBGradient.ColorCount - 1) then
      begin
        FLeftMidPointMark.FillColor := $00000000;
        FLeftMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end
      else
      begin
        FLeftMidPointMark.FillColor  := $00000000;
        FRightMidPointMark.FillColor := $00000000;

        FLeftMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
        FRightMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end;
    end;
  end
  else
  // draw mid points between Alpha stops ...
  if FSelectedAlphaIndex >= 0 then
  begin
    // a mid point is selected ...
    if Assigned(FSelectedMidPointMark) then
    begin
      if Assigned(FSelectedAlphaGradient) then
      begin
        LInverseAlpha := 255 - FSelectedAlphaGradient.EndAlpha;
        FSelectedMidPointMark.FillColor := Gray32(LInverseAlpha);
      end;

      FSelectedMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
    end
    else
    begin
      if FSelectedAlphaIndex = 0 then
      begin
        FRightMidPointMark.FillColor := $00000000;
        FRightMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end
      else
      if FSelectedAlphaIndex = (FGradient.AlphaGradient.AlphaValueCount - 1) then
      begin
        FLeftMidPointMark.FillColor := $00000000;
        FLeftMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end
      else
      begin
        FLeftMidPointMark.FillColor  := $00000000;
        FRightMidPointMark.FillColor := $00000000;

        FLeftMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
        FRightMidPointMark.Draw(FPreviewMap, FStopOffset, 0);
      end;
    end;
  end;
end; { DrawMidPointMarks }

procedure TgmGradientEditor.SetColorGradient(const AValue: TgmColorGradient);
begin
  if Assigned(AValue) and (FGradient <> AValue) then
  begin
    FGradient.Assign(AValue);
    CancelSelection;
    UpdateAlphaStopList;
    UpdateColorStopList;
    UpdatePreview;
  end;
end; { SetColorGradient }

procedure TgmGradientEditor.SetSpectrumWidth(const AValue: Integer);
begin
  if AValue <> FSpectrumWidth then
  begin
    if AValue > MIN_SPECTRUM_WIDTH then
    begin
      FSpectrumWidth := AValue;
    end
    else
    begin
      FSpectrumWidth := MIN_SPECTRUM_WIDTH;
    end;

    FPreviewMap.Width     := (FStopOffset * 2) + FSpectrumWidth;
    FSpectrumRect         := GetSpectrumRect;
    FAlphaStopProcessArea := GetAlphaStopProcessArea;
    FColorStopProcessArea := GetColorStopProcessArea;
  end;
end; { SetSpectrumWidth }

procedure TgmGradientEditor.SetSpectrumHeight(const AValue: Integer);
begin
  if AValue <> FSpectrumHeight then
  begin
    if AValue > MIN_SPECTRUM_HEIGHT then
    begin
      FSpectrumHeight := AValue;
    end
    else
    begin
      FSpectrumHeight := MIN_SPECTRUM_HEIGHT;
    end;

    FPreviewMap.Height    := (FStopHeight * 2) + FSpectrumHeight + (PREVIEW_BORDER_WIDTH * 2);
    FSpectrumRect         := GetSpectrumRect;
    FAlphaStopProcessArea := GetAlphaStopProcessArea;
    FColorStopProcessArea := GetColorStopProcessArea;
  end;
end; { SetSpectrumHeight }

procedure TgmGradientEditor.ProcessColorGradientWhenMouseDown(
  const AX, AY: Integer);
var
  LColorIndex        : Integer;
  LColorLocationScale: Single;
begin
  LColorIndex := FColorStopList.PointOnStops(AX, AY);

  // if point on a primary color stop...
  if LColorIndex >= 0 then
  begin
    FSelectedMidPointMark := nil;
    FSelectedRGBGradient  := nil;
    FActionType           := eatSelectingPrimaryColor;
    FState                := gesPrimaryColorSelected;

    FSelectedColorIndex   := LColorIndex;
    FDefaultNewColor      := FGradient.RGBGradient.PrimaryColors[FSelectedColorIndex];
    FDefaultNewColorType  := FGradient.RGBGradient.PrimaryColorTypes[FSelectedColorIndex];

    FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

    if FSelectedColorIndex >= 0 then
    begin
      FSelectedAlphaIndex := -1;
      FAlphaStopList.DeactivateAllStops;
    end;
  end
  else
  begin
    // if not point on any primary color stop, then to check if we
    // pointed on any mid points
    FSelectedMidPointMark := PointOnColorMidPoint(FSelectedColorIndex, AX, AY);

    if Assigned(FSelectedMidPointMark) then
    begin
      FActionType := eatSelectingColorMidPoint;
      FState      := gesColorMidPointSelected;

      FColorStopList.DeactivateAllStops;
      FAlphaStopList.DeactivateAllStops;
      FSelectedAlphaIndex := -1;

      // selected a simple color gradient that corresponding to the selected mid point
      if FSelectedMidPointMark = FLeftMidPointMark then
      begin
        FSelectedRGBGradient := FLeftRGBGradient;
      end
      else
      if FSelectedMidPointMark = FRightMidPointMark then
      begin
        FSelectedRGBGradient := FRightRGBGradient;
      end;
    end
    else
    begin
      FSelectedRGBGradient := nil;

      // if none of any stops or mid points are selected,
      // then to see whether we could add a primary color
      if PtInRect( FColorStopProcessArea, Point(AX, AY) ) then
      begin
        LColorLocationScale := AX / FSpectrumWidth;
        FSelectedColorIndex := FGradient.RGBGradient.InsertColor(LColorLocationScale, FDefaultNewColor);
        FActionType         := eatSelectingPrimaryColor;
        FState              := gesPrimaryColorSelected;

        FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex, FDefaultNewColorType);

        UpdateColorStopList;
        FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

        FSelectedAlphaIndex := -1;
        FAlphaStopList.DeactivateAllStops;
      end;
    end;
  end;
end; { ProcessColorGradientWhenMouseDown }

procedure TgmGradientEditor.ProcessAlphaGradientWhenMouseDown(
  const AX, AY: Integer);
var
  LAlphaIndex        : Integer;
  LAlphaLocationScale: Single;
begin
  LAlphaIndex := FAlphaStopList.PointOnStops(AX, AY);

  // if point on a primary Alpha stop...
  if LAlphaIndex >= 0 then
  begin
    FSelectedMidPointMark := nil;
    FSelectedRGBGradient  := nil;
    FActionType           := eatSelectingPrimaryAlpha;
    FState                := gesPrimaryAlphaSelected;

    FSelectedAlphaIndex   := LAlphaIndex;
    FDefaultNewAlpha      := FGradient.AlphaGradient.PrimaryAlphaValues[FSelectedAlphaIndex];

    FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

    if FSelectedAlphaIndex >= 0 then
    begin
      FSelectedColorIndex := -1;
      FColorStopList.DeactivateAllStops;
    end;
  end
  else
  begin
    // if not point on any primary Alpha stop, then to check if we
    // pointed on any mid points
    FSelectedMidPointMark := PointOnAlphaMidPoint(FSelectedAlphaIndex, AX, AY);

    if Assigned(FSelectedMidPointMark) then
    begin
      FActionType := eatSelectingAlphaMidPoint;
      FState      := gesAlphaMidPointSelected;

      FAlphaStopList.DeactivateAllStops;
      FColorStopList.DeactivateAllStops;
      FSelectedColorIndex := -1;

      // selected a simple Alpha gradient that corresponding to the selected mid point
      if FSelectedMidPointMark = FLeftMidPointMark then
      begin
        FSelectedAlphaGradient := FLeftAlphaGradient;
      end
      else
      if FSelectedMidPointMark = FRightMidPointMark then
      begin
        FSelectedAlphaGradient := FRightAlphaGradient;
      end;
    end
    else
    begin
      FSelectedAlphaGradient := nil;

      // if none of any stops or mid points are selected,
      // then to see whether we could add a primary Alpha
      if PtInRect( FAlphaStopProcessArea, Point(AX, AY) ) then
      begin
        LAlphaLocationScale := AX / FSpectrumWidth;
        FSelectedAlphaIndex := FGradient.AlphaGradient.InsertAlpha(LAlphaLocationScale, FDefaultNewAlpha);
        FActionType         := eatSelectingPrimaryAlpha;
        FState              := gesPrimaryAlphaSelected;

        UpdateAlphaStopList;
        FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

        FSelectedColorIndex := -1;
        FColorStopList.DeactivateAllStops;
      end;
    end;
  end;
end; { ProcessAlphaGradientWhenMouseDown }

function TgmGradientEditor.PointOnColorMidPoint(
  const AColorIndex, AX, AY: Integer): TgmSimpleDiamond2D;
begin
  Result := nil;

  if AColorIndex >= 0 then
  begin
    if AColorIndex = 0 then
    begin
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end
    else
    if AColorIndex = (FGradient.RGBGradient.ColorCount - 1) then
    begin
      if FLeftMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FLeftMidPointMark;
      end;
    end
    else
    begin
      if FLeftMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FLeftMidPointMark;
      end
      else
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end;
  end;
end; { PointOnColorMidPoint }

function TgmGradientEditor.PointOnAlphaMidPoint(
  const AAlphaIndex, AX, AY: Integer): TgmSimpleDiamond2D;
begin
  Result := nil;

  if AAlphaIndex >= 0 then
  begin
    if AAlphaIndex = 0 then
    begin
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end
    else
    if AAlphaIndex = (FGradient.AlphaGradient.AlphaValueCount - 1) then
    begin
      if FLeftMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FLeftMidPointMark;
      end;
    end
    else
    begin
      if FLeftMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FLeftMidPointMark;
      end
      else
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end;
  end;
end; { PointOnAlphaMidPoint }

function TgmGradientEditor.GetAlphaValueCount: Integer;
begin
  Result := FGradient.AlphaGradient.AlphaValueCount;
end; { GetAlphaValueCount }

function TgmGradientEditor.GetColorCount: Integer;
begin
  Result := FGradient.RGBGradient.ColorCount;
end; { GetColorCount }

function TgmGradientEditor.DeleteSelectedPrimaryColor: Boolean;
begin
  Result := False;

  if (FState = gesPrimaryColorSelected) and
     (FSelectedColorIndex >= 0) and
     (FSelectedColorIndex < FGradient.RGBGradient.ColorCount) and
     (FGradient.RGBGradient.ColorCount > 2) then
  begin
    FGradient.RGBGradient.DeleteColor(FSelectedColorIndex);

    FSelectedColorIndex := -1;
    FState              := gesIdle;

    UpdateColorStopList;
    UpdatePreview;

    if Assigned(FOnStateChanged) then
    begin
      FOnStateChanged(FState);
    end;

    Result := True;
  end; 
end; { DeleteSelectedPrimaryColor }

function TgmGradientEditor.DeleteSelectedAlphaValue: Boolean;
begin
  Result := False;

  if (FState = gesPrimaryAlphaSelected) and
     (FSelectedAlphaIndex >= 0) and
     (FSelectedAlphaIndex < FGradient.AlphaGradient.AlphaValueCount) and
     (FGradient.AlphaGradient.AlphaValueCount > 2) then
  begin
    FGradient.AlphaGradient.DeleteAlpha(FSelectedAlphaIndex);

    FSelectedAlphaIndex := -1;
    FState              := gesIdle;

    UpdateAlphaStopList;
    UpdatePreview;

    if Assigned(FOnStateChanged) then
    begin
      FOnStateChanged(FState);
    end;

    Result := True;
  end; 
end; { DeleteSelectedAlphaValue }

procedure TgmGradientEditor.AverageColorLocationScales;
begin
  FGradient.RGBGradient.AverageColorLocationScales;
  UpdateColorStopList;
  UpdatePreview;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(FState);
  end;
end; { AverageColorLocationScales }

procedure TgmGradientEditor.AverageAlphaLocationScales;
begin
  FGradient.AlphaGradient.AverageAlphaLocationScales;
  UpdateAlphaStopList;
  UpdatePreview;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(FState);
  end;
end; { AverageAlphaLocationScales }

procedure TgmGradientEditor.CancelSelection;
begin
  FSelectedColorIndex := -1;
  FSelectedAlphaIndex := -1;
  FState              := gesIdle;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(FState);
  end;
end; { CancelSelection }

procedure TgmGradientEditor.UpdatePreview;
begin
  FGradient.GradientLength := FSpectrumWidth;
  FGradient.RefreshColorArray;

  FGradientMap.SetSize(FSpectrumWidth, FSpectrumHeight);
  FGradient.DrawColorGradients(FGradientMap);
  FGradientMap.DrawMode := dmBlend;

  // draw gradient spectrum
  FPreviewMap.Clear(FBackColor);
  DrawCheckerboardPattern(FPreviewMap, FSpectrumRect, True);
  FPreviewMap.Draw(FStopOffset, FStopHeight, FGradientMap);

  // draw mid point marks
  UpdateMidPointMarks;
  DrawMidPointMarks;

  // draw alpha and color stops
  FAlphaStopList.DrawAllStopObjects(FPreviewMap, FStopOffset, 0);
  FColorStopList.DrawAllStopObjects(FPreviewMap, FStopOffset, 0);

  FPreviewMap.FrameRectS(FStopOffset, FStopHeight,
                         FStopOffset + FSpectrumWidth + 1,
                         FStopHeight + FSpectrumHeight + 1,
                         clBlack32);

  FOwner.Bitmap.Assign(FPreviewMap);
end; { UpdatePreview }

function TgmGradientEditor.ChangeSelectedPrimaryColor(
  const AColor: TColor32): Boolean;
begin
  Result := False;

  if (FState = gesPrimaryColorSelected) and
     (FSelectedColorIndex >= 0) and
     (FSelectedColorIndex < FGradient.RGBGradient.ColorCount) then
  begin
    // change color type first, otherwise the color may not be changed 
    FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex, gctStaticColor);
    FGradient.RGBGradient.ChangeColor(FSelectedColorIndex, AColor);
    FColorStopList.ChangeStopColor(FSelectedColorIndex, AColor);
    FColorStopList.ChangeStopType(FSelectedColorIndex, sotStaticColor);
    UpdatePreview;

    Result := True;
  end;
end; { ChangeSelectedPrimaryColor }

function TgmGradientEditor.ChangeSelectedPrimaryColorLocationScale(
  const AScale: Single): Boolean;
var
  LColorIndex: Integer;
  LDeltaX    : Integer;
  LOldScale  : Single;
begin
  Result := False;

  if (FState = gesPrimaryColorSelected) and
     (FSelectedColorIndex >= 0) and
     (FSelectedColorIndex < FGradient.RGBGradient.ColorCount) then
  begin
    LOldScale   := FGradient.RGBGradient.PrimaryScales[FSelectedColorIndex];
    LColorIndex := FGradient.RGBGradient.ChangeColorLocationScale(FSelectedColorIndex, AScale);

    { The index of primary color could be changed, so the index of
      color stops should also be changed. }
    if LColorIndex <> FSelectedColorIndex then
    begin
      FColorStopList.ChangeStopIndex(FSelectedColorIndex, LColorIndex);
      FSelectedColorIndex := LColorIndex;
    end;

    // change position of color stop
    LDeltaX := Round( (AScale - LOldScale) * FGradient.GradientLength );

    FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex, LDeltaX,
                                             0, FSpectrumWidth - 1);

    UpdatePreview;

    Result := True;
  end;
end; { ChangeSelectedPrimaryColorLocationScale }

function TgmGradientEditor.ChangeSelectedColorMidPointScale(
  const AScale: Single): Boolean;
begin
  Result := False;

  if Assigned(FSelectedRGBGradient) and
     (FState = gesColorMidPointSelected) then
  begin
    FSelectedRGBGradient.MidLocationScale := AScale;
    UpdatePreview;
    
    Result := True;
  end;
end; { ChangeSelectedColorMidPointScale }

function TgmGradientEditor.ChangeSelectedAlphaValue(
  const AValue: Byte): Boolean;
var
  LInverseAlpha: Byte;
  LColor       : TColor32;
begin
  Result := False;

  if (FState = gesPrimaryAlphaSelected) and
     (FSelectedAlphaIndex >= 0) and
     (FSelectedAlphaIndex < FGradient.AlphaGradient.AlphaValueCount) then
  begin
    LInverseAlpha := 255 - AValue;
    LColor        := Gray32(LInverseAlpha);

    FGradient.AlphaGradient.ChangeAlphaValue(FSelectedAlphaIndex, AValue);
    FAlphaStopList.ChangeStopColor(FSelectedAlphaIndex, LColor);
    UpdatePreview;
  end;
end; { ChangeSelectedAlphaValue }

function TgmGradientEditor.ChangeSelectedAlphaLocationScale(
  const AScale: Single): Boolean;
var
  LAlphaIndex: Integer;
  LDeltaX    : Integer;
  LOldScale  : Single;
begin
  Result := False;

  if (FState = gesPrimaryAlphaSelected) and
     (FSelectedAlphaIndex >= 0) and
     (FSelectedAlphaIndex < FGradient.AlphaGradient.AlphaValueCount) then
  begin
    LOldScale   := FGradient.AlphaGradient.PrimaryScales[FSelectedAlphaIndex];
    LAlphaIndex := FGradient.AlphaGradient.ChangeAlphaLocationScale(FSelectedAlphaIndex, AScale);

    { The index of alpha value may be changed, so the index of
      alpha stops should also be changed. }
    if LAlphaIndex <> FSelectedAlphaIndex then
    begin
      FAlphaStopList.ChangeStopIndex(FSelectedAlphaIndex, LAlphaIndex);
      FSelectedAlphaIndex := LAlphaIndex;
    end;

    // change position of alphs stop
    LDeltaX := Round( (AScale - LOldScale) * FGradient.GradientLength );

    FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex, LDeltaX,
                                             0, FSpectrumWidth - 1);

    UpdatePreview;

    Result := True;
  end;
end; { ChangeSelectedAlphaLocationScale }

function TgmGradientEditor.ChangeSelectedAlphaMidPointScale(
  const AScale: Single): Boolean;
begin
  Result := False;

  if Assigned(FSelectedAlphaGradient) and
     (FState = gesAlphaMidPointSelected) then
  begin
    FSelectedAlphaGradient.MidLocationScale := AScale;
    UpdatePreview;
    
    Result := True;
  end;
end; { ChangeSelectedAlphaMidPointScale }

function TgmGradientEditor.GetSelectedPrimaryColorLocationScale: Single;
begin
  Result := -1.0;

  if (FState = gesPrimaryColorSelected) and
     (FSelectedColorIndex >= 0) then
  begin
    Result := FGradient.RGBGradient.PrimaryScales[FSelectedColorIndex];
  end;
end; { GetSelectedPrimaryColorLocationScale }

function TgmGradientEditor.GetSelectedColorMidPointScale: Single;
begin
  Result := -1.0;

  if Assigned(FSelectedRGBGradient) and
     (FState = gesColorMidPointSelected) then
  begin
    Result := FSelectedRGBGradient.MidLocationScale;
  end;
end; { GetSelectedColorMidPointScale }

function TgmGradientEditor.GetSelectedAlphaLocationScale: Single;
begin
  Result := -1.0;

  if (FState = gesPrimaryAlphaSelected) and
     (FSelectedAlphaIndex >= 0) then
  begin
    Result := FGradient.AlphaGradient.PrimaryScales[FSelectedAlphaIndex];
  end;
end; { GetSelectedAlphaLocationScale }

function TgmGradientEditor.GetSelectedAlphaMidPointScale: Single;
begin
  Result := -1.0;

  if Assigned(FSelectedAlphaGradient) and
     (FState = gesAlphaMidPointSelected) then
  begin
    Result := FSelectedAlphaGradient.MidLocationScale;
  end;
end; { GetSelectedAlphaMidPointScale }

procedure TgmGradientEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LBmpCoord         : TPoint;
  LActualX, LActualY: Integer;
  LColorIndex       : Integer;
  LColorType        : TgmGradientColorType;
begin
  LBmpCoord := FOwner.ControlToBitmap( Point(X, Y) );
  LActualX  := LBmpCoord.X - FStopOffset;
  LActualY  := LBmpCoord.Y;

  if Button = mbLeft then
  begin
    if (LBmpCoord.Y >= FColorStopProcessArea.Top) and
       (LBmpCoord.Y <= FColorStopProcessArea.Bottom) then
    begin
      // when mouse is at color stop process area ...
      ProcessColorGradientWhenMouseDown(LActualX, LActualY);
    end
    else
    if (LBmpCoord.Y >= FAlphaStopProcessArea.Top) and
       (LBmpCoord.Y <= FAlphaStopProcessArea.Bottom) then
    begin
      // when mouse is at alpha stop process area ...
      ProcessAlphaGradientWhenMouseDown(LActualX, LActualY);
    end;
  end
  else
  if Button = mbRight then
  begin
    // if right mouse button is down, try to select a color stop,
    // and switch the stop types
    if (LBmpCoord.Y >= FColorStopProcessArea.Top) and
       (LBmpCoord.Y <= FColorStopProcessArea.Bottom) then
    begin
      LColorIndex := FColorStopList.PointOnStops(LActualX, LActualY);

      // if point on a primary color stop...
      if LColorIndex >= 0 then
      begin
        FSelectedMidPointMark := nil;
        FSelectedRGBGradient  := nil;
        FActionType           := eatSelectingPrimaryColor;
        FState                := gesPrimaryColorSelected;

        FSelectedColorIndex   := LColorIndex;
        FDefaultNewColor      := FGradient.RGBGradient.PrimaryColors[FSelectedColorIndex];
        FDefaultNewColorType  := FGradient.RGBGradient.PrimaryColorTypes[FSelectedColorIndex];

        if FSelectedColorIndex >= 0 then
        begin
          FSelectedAlphaIndex := -1;
          FAlphaStopList.DeactivateAllStops;
        end;

        // switch color stop types...
        LColorType := FGradient.RGBGradient.PrimaryColorTypes[FSelectedColorIndex];

        case LColorType of
          gctStaticColor:
            begin
              FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex,
                                                    gctDynamicForegroundColor);

              FDefaultNewColor := FGradient.RGBGradient.ForegroundColor;
            end;

          gctDynamicForegroundColor:
            begin
              FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex,
                                                    gctDynamicBackgroundColor);

              FDefaultNewColor := FGradient.RGBGradient.BackgroundColor;
            end;

          gctDynamicBackgroundColor:
            begin
              FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex,
                                                    gctStaticColor);
            end;
        end;

        UpdateColorStopList;
        FColorStopList.ActivateStopByIndex(FSelectedColorIndex);
      end;
    end
  end;

  Self.UpdatePreview;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(FState);
  end;

  if Button = mbLeft then
  begin
    FLastHorizMove   := LBmpCoord.X;
    FMouseButtonDown := True;
  end;
end; { MouseDown }

procedure TgmGradientEditor.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  LBmpCoord     : TPoint;
  LIndex        : Integer;
  LTestX, LTestY: Integer;
  LLeftLimit    : Integer;
  LRightLimit   : Integer;
  LLocationScale: Single;
  LCursorPosType: TgmEditorCursorPosition;
begin
  LBmpCoord := FOwner.ControlToBitmap( Point(X, Y) );

  if FMouseButtonDown then
  begin
    case FActionType of
//-- Color Stop process --------------------------------------------------------
      eatSelectingPrimaryColor:
        begin
          if FSelectedColorIndex >= 0 then
          begin
            // if we drag an color out of the gradient
            if ( (LBmpCoord.Y < FColorStopProcessArea.Top) or
                 (LBmpCoord.Y > FColorStopProcessArea.Bottom) ) and
               ( FGradient.RGBGradient.ColorCount > 2 ) then
            begin
              FActionType := eatDragColorOut;

              // remeber the info of dragged out color
              FDragOutColor     := FGradient.RGBGradient.PrimaryColors[FSelectedColorIndex];
              FDrawOutColorType := FGradient.RGBGradient.PrimaryColorTypes[FSelectedColorIndex];

              if FSelectedColorIndex = 0 then
              begin
                FRightDragOutMidScale := FGradient.RGBGradient.MidPointScale[FSelectedColorIndex];
              end
              else
              if FSelectedColorIndex = (FGradient.RGBGradient.ColorCount - 1) then
              begin
                FLeftDragOutMidScale := FGradient.RGBGradient.MidPointScale[FSelectedColorIndex];
              end
              else
              begin
                FLeftDragOutMidScale  := FGradient.RGBGradient.MidPointScale[FSelectedColorIndex - 1];
                FRightDragOutMidScale := FGradient.RGBGradient.MidPointScale[FSelectedColorIndex];
              end;

              FGradient.RGBGradient.DeleteColor(FSelectedColorIndex);

              FSelectedColorIndex := -1;
              FState              := gesIdle;

              UpdateColorStopList;
            end
            else
            begin
              // try to change the position of a selected primary color...

              if LBmpCoord.X < FStopOffset then
              begin
                // when mouse move out to the left of gradient map...
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  LBmpCoord.X - FStopOffset, 0, FSpectrumWidth - 1);
              end
              else
              if LBmpCoord.X > (FPreviewMap.Width - FStopOffset * 2) then
              begin
                // when mouse move out to the right of gradient map...
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  LBmpCoord.X - (FPreviewMap.Width - FStopOffset * 2),
                  0, FSpectrumWidth - 1);
              end
              else
              begin
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  LBmpCoord.X - FLastHorizMove, 0, FSpectrumWidth - 1);
              end;

              LLocationScale := FColorStopList.GetStopHeadTipX(FSelectedColorIndex) / FSpectrumWidth;
              LIndex         := FGradient.RGBGradient.ChangeColorLocationScale(FSelectedColorIndex, LLocationScale);

              { The index of primary color could be changed, so the index of
                color stops should also be changed. }
              if LIndex <> FSelectedColorIndex then
              begin
                FColorStopList.ChangeStopIndex(FSelectedColorIndex, LIndex);
                FSelectedColorIndex := LIndex;
              end;

              FLastHorizMove := LBmpCoord.X;
            end;
          end;
        end;

      eatSelectingColorMidPoint:
        begin
          if Assigned(FSelectedRGBGradient) and
             Assigned(FSelectedMidPointMark) then
          begin
            LLeftLimit  := FSelectedRGBGradient.StartLocAtTotalSteps;
            LRightLimit := FSelectedRGBGradient.EndLocAtTotalSteps;

            LTestX := LBmpCoord.X - FStopOffset;

            if (LTestX > LLeftLimit) and (LTestX < LRightLimit) then
            begin
              FSelectedRGBGradient.MidLocationScale :=
                (LTestX  - LLeftLimit) / FSelectedRGBGradient.GradientSteps;
            end;
          end;
        end;

      eatDragColorOut:
        begin
          // try to drag the color in....

          if PtInRect(FColorStopProcessArea, LBmpCoord) then
          begin
            LLocationScale      := (LBmpCoord.X - FStopOffset) / FSpectrumWidth;
            FSelectedColorIndex := FGradient.RGBGradient.InsertColor(LLocationScale, FDragOutColor);

            FGradient.RGBGradient.ChangeColorType(FSelectedColorIndex, FDrawOutColorType);

            if FSelectedColorIndex = 0 then
            begin
              FGradient.RGBGradient.ChangeMidPointScale(FSelectedColorIndex,
                FSelectedColorIndex + 1, FRightDragOutMidScale);
            end
            else
            if FSelectedColorIndex = (FGradient.RGBGradient.ColorCount - 1) then
            begin
              FGradient.RGBGradient.ChangeMidPointScale(FSelectedColorIndex - 1,
                FSelectedColorIndex, FLeftDragOutMidScale);
            end
            else
            begin
              FGradient.RGBGradient.ChangeMidPointScale(FSelectedColorIndex - 1,
                FSelectedColorIndex, FLeftDragOutMidScale);

              FGradient.RGBGradient.ChangeMidPointScale(FSelectedColorIndex,
                FSelectedColorIndex + 1, FRightDragOutMidScale);
            end;

            UpdateColorStopList;
            FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

            FActionType    := eatSelectingPrimaryColor;
            FState         := gesPrimaryColorSelected;
            FLastHorizMove := LBmpCoord.X;
          end;
        end;

//-- Alpha Stop Process --------------------------------------------------------

      eatSelectingPrimaryAlpha:
        begin
          if FSelectedAlphaIndex >= 0 then
          begin
            // if we drag an alpha out of the gradient
            if ( (LBmpCoord.Y < FAlphaStopProcessArea.Top) or
                 (LBmpCoord.Y > FAlphaStopProcessArea.Bottom) ) and
               ( FGradient.AlphaGradient.AlphaValueCount > 2 ) then
            begin
              FActionType := eatDragAlphaOut;

              // remeber the info of dragged out alpha
              FDragOutAlpha := FGradient.AlphaGradient.PrimaryAlphaValues[FSelectedAlphaIndex];

              if FSelectedAlphaIndex = 0 then
              begin
                FRightDragOutMidScale := FGradient.AlphaGradient.MidPointScale[FSelectedAlphaIndex];
              end
              else
              if FSelectedAlphaIndex = (FGradient.AlphaGradient.AlphaValueCount - 1) then
              begin
                FLeftDragOutMidScale := FGradient.AlphaGradient.MidPointScale[FSelectedAlphaIndex];
              end
              else
              begin
                FLeftDragOutMidScale  := FGradient.AlphaGradient.MidPointScale[FSelectedAlphaIndex - 1];
                FRightDragOutMidScale := FGradient.AlphaGradient.MidPointScale[FSelectedAlphaIndex];
              end;

              FGradient.AlphaGradient.DeleteAlpha(FSelectedAlphaIndex);

              FSelectedAlphaIndex := -1;
              FState              := gesIdle;

              UpdateAlphaStopList;
            end
            else
            begin
              // try to change the position of a selected primary Alpha ...

              if LBmpCoord.X < FStopOffset then
              begin
                // when mouse move out to the left of gradient map...
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  LBmpCoord.X - FStopOffset, 0, FSpectrumWidth - 1);
              end
              else
              if LBmpCoord.X > (FPreviewMap.Width - FStopOffset * 2) then
              begin
                // when mouse move out to the right of gradient map...
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  LBmpCoord.X - (FPreviewMap.Width - FStopOffset * 2),
                  0, FSpectrumWidth - 1);
              end
              else
              begin
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  LBmpCoord.X - FLastHorizMove, 0, FSpectrumWidth - 1);
              end;

              LLocationScale := FAlphaStopList.GetStopHeadTipX(FSelectedAlphaIndex) / FSpectrumWidth;
              LIndex         := FGradient.AlphaGradient.ChangeAlphaLocationScale(FSelectedAlphaIndex, LLocationScale);

              { The index of primary Alpha could be changed, so the index of
                Alpha stops should also be changed. }
              if LIndex <> FSelectedAlphaIndex then
              begin
                FAlphaStopList.ChangeStopIndex(FSelectedAlphaIndex, LIndex);
                FSelectedAlphaIndex := LIndex;
              end;

              FLastHorizMove := LBmpCoord.X;
            end;
          end;
        end;

      eatSelectingAlphaMidPoint:
        begin
          if Assigned(FSelectedAlphaGradient) and
             Assigned(FSelectedMidPointMark) then
          begin
            LLeftLimit  := FSelectedAlphaGradient.StartLocAtTotalSteps;
            LRightLimit := FSelectedAlphaGradient.EndLocAtTotalSteps;

            LTestX := LBmpCoord.X - FStopOffset;

            if (LTestX > LLeftLimit) and (LTestX < LRightLimit) then
            begin
              FSelectedAlphaGradient.MidLocationScale :=
                (LTestX  - LLeftLimit) / FSelectedAlphaGradient.GradientSteps;
            end;
          end;
        end;

      eatDragAlphaOut:
        begin
          // try to drag the Alpha in....

          if PtInRect(FAlphaStopProcessArea, LBmpCoord) then
          begin
            LLocationScale      := (LBmpCoord.X - FStopOffset) / FSpectrumWidth;
            FSelectedAlphaIndex := FGradient.AlphaGradient.InsertAlpha(LLocationScale, FDragOutAlpha);

            if FSelectedAlphaIndex = 0 then
            begin
              FGradient.AlphaGradient.ChangeMidPointScale(FSelectedAlphaIndex,
                FSelectedAlphaIndex + 1, FRightDragOutMidScale);
            end
            else
            if FSelectedAlphaIndex = (FGradient.AlphaGradient.AlphaValueCount - 1) then
            begin
              FGradient.AlphaGradient.ChangeMidPointScale(FSelectedAlphaIndex - 1,
                FSelectedAlphaIndex, FLeftDragOutMidScale);
            end
            else
            begin
              FGradient.AlphaGradient.ChangeMidPointScale(FSelectedAlphaIndex - 1,
                FSelectedAlphaIndex, FLeftDragOutMidScale);

              FGradient.AlphaGradient.ChangeMidPointScale(FSelectedAlphaIndex,
                FSelectedAlphaIndex + 1, FRightDragOutMidScale);
            end;

            UpdateAlphaStopList;
            FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

            FActionType    := eatSelectingPrimaryAlpha;
            FState         := gesPrimaryAlphaSelected;
            FLastHorizMove := LBmpCoord.X;
          end;
        end;
    end;

    Self.UpdatePreview;

    if Assigned(FOnStateChanged) then
    begin
      FOnStateChanged(FState);
    end;
  end
  else
  begin
    LTestX := LBmpCoord.X - FStopOffset;
    LTestY := LBmpCoord.Y;

    LCursorPosType := GetCursorPostionType(LTestX, LTestY);

    if Assigned(FOnCursorPosChanged) then
    begin
      FOnCursorPosChanged(LCursorPosType);
    end;
  end;
end; { MouseMove }

procedure TgmGradientEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FMouseButtonDown := False;
  FActionType      := eatDoNothing;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(FState);
  end;
end; { MouseUp }

end.
