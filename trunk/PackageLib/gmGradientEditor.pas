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
  Classes, Controls, Graphics,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic }
  gmGradient,
  gmGradient_List,
  gmGridBased_List,
  gmStopObject,
  gmGeometricObjects2D;

type
  TgmGradientEditorState = (gesIdle,
                            gesColorStopSelected,
                            gesColorMidPointSelected,
                            gesAlphaStopSelected,
                            gesAlphaMidPointSelected);

  TgmEditorActionType = (eatDoNothing,
                         eatSelectingColorStop,
                         eatSelectingColorMidPoint,
                         eatDragColorStopOut,
                         eatSelectingAlphaStop,
                         eatSelectingAlphaMidPoint,
                         eatDragAlphaStopOut);

  TgmEditorCursorPosition = (ecpOutOfEnable,
                             ecpOnStop,
                             ecpOnMidPoint,
                             ecpOnAddStopArea);
                             
  TgmEditorEditingMode = (eemDirectEdit, eemCopyEdit);

  TgmEditorCursorPosChangedEvent =
    procedure (ASender: TObject; const ACursorPosition: TgmEditorCursorPosition) of object;
    
  TgmEditorStateChangedEvent =
    procedure (ASender: TObject; const AState: TgmGradientEditorState) of object;

  TgmGradientEditor = class(TCustomPaintBox32, IgmGradientListSupport)
  private
    FGradients             : TgmGradientList;
    FGradientIndex         : TgmGradientIndex;
    FChangeLink            : TgmGridBasedChangeLink;

    FGradient              : TgmGradientItem;
    FState                 : TgmGradientEditorState;
    FActionType            : TgmEditorActionType;
    FEditMode              : TgmEditorEditingMode;

    FAlphaStopList         : TgmStopObjectList;
    FColorStopList         : TgmStopObjectList;
    FDefaultNewAlpha       : Byte;
    FDefaultNewColor       : TColor;
    FSpectrumHeight        : Integer;
    FSpectrumWidth         : Integer;
    FStopOffset            : Integer;
    FStopHeight            : Integer;
    FPreviewMap            : TBitmap32;
    FGradientMap           : TBitmap32;
    FSpectrumRect          : TRect;
    FAlphaStopProcessArea  : TRect;
    FColorStopProcessArea  : TRect;

    FSelectedAlphaIndex    : Integer;
    FSelectedColorIndex    : Integer;
    FSelectedMidPointIndex : Integer;  // used to hold the corresponding color/alpha index when a color/alpha midpoint is selected
    FLastHorizMove         : Integer;
    FMouseButtonDown       : Boolean;

    // used to select a mid point
    FLeftMidPointMark      : TgmSimpleDiamond2D;
    FRightMidPointMark     : TgmSimpleDiamond2D;
    FSelectedMidPointMark  : TgmSimpleDiamond2D;    // pointer to left/right mid point mark

    FLeftStop              : TgmGradientStopItem;   // pointer to a stop that at left of the selected color/alpha stop
    FRightStop             : TgmGradientStopItem;   // pointer to a stop that at right of the selected color/alpha stop
    FSelectedStop          : TgmGradientStopItem;   // pointer to left/right stop

    // used to drag color in/out to/from the gradient
    FDragOutColor          : TColor;
    FDragOutAlpha          : Byte;
    FLeftDragOutMidScale   : Single;
    FRightDragOutMidScale  : Single;

    FOnStateChanged        : TgmEditorStateChangedEvent;
    FOnCursorPosChanged    : TgmEditorCursorPosChangedEvent;

    // internal use for tracking change of component size
    FOldBoxWidth           : Integer;
    FOldBoxHeight          : Integer;

    procedure SetupInternalGradient;

    procedure GradientListChanged(ASender: TObject);
    procedure UpdateView(ASender: TObject); // double purpose, for internal used & called by external.
    procedure UpdateAlphaStopList;
    procedure UpdateColorStopList;
    procedure SetSpectrumHeight(const AValue: Integer);
    procedure SetSpectrumWidth(const AValue: Integer);

    procedure ProcessAlphaGradientWhenMouseDown(const AX, AY: Integer);
    procedure ProcessColorGradientWhenMouseDown(const AX, AY: Integer);

    procedure DrawMidPointMarks;
    procedure UpdateMidPointMarks;
    procedure SetGradients(const AValue: TgmGradientList);
    procedure SetGradientIndex(const AValue: TgmGradientIndex);
    procedure SetEditMode(const AValue: TgmEditorEditingMode);
    procedure StateReset;

    function GetSpectrumRect: TRect;
    function GetAlphaStopProcessArea: TRect;
    function GetColorStopProcessArea: TRect;
    function GetStopOffset: Integer;
    function GetGradient: TgmGradientItem;
    function GetGradients: TgmGradientList; // implement methods in IgmGradientListSupport
    function GetCursorPostionType(const AX, AY: Integer): TgmEditorCursorPosition;

    function PointOnAlphaMidPoint(const AAlphaIndex, AX, AY: Integer): TgmSimpleDiamond2D;
    function PointOnColorMidPoint(const AColorIndex, AX, AY: Integer): TgmSimpleDiamond2D;
  protected
    procedure DoPaintBuffer; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;

    property SpectrumWidth  : Integer read FSpectrumWidth  write SetSpectrumWidth;
    property SpectrumHeight : Integer read FSpectrumHeight write SetSpectrumHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;
    procedure AlphaStopDistributeAverage;
    procedure ColorStopDistributeAverage;

    function ChangeSelectedAlphaValue(const AValue: Byte): Boolean;
    function ChangeSelectedPrimaryColor(const AColor: TColor): Boolean;

    function ChangeSelectedAlphaLocationScale(const AValue: Double): Boolean;
    function ChangeSelectedColorLocationScale(const AValue: Double): Boolean;
    function ChangeSelectedMidPointScale(const AValue: Double): Boolean;

    function DeleteSelectedAlphaStop: Boolean;
    function DeleteSelectedColorStop: Boolean;

    property SelectedAlphaIndex    : Integer                read FSelectedAlphaIndex;
    property SelectedColorIndex    : Integer                read FSelectedColorIndex;
    property SelectedMidPointIndex : Integer                read FSelectedMidPointIndex;
    property State                 : TgmGradientEditorState read FState;
    property Gradient              : TgmGradientItem        read GetGradient;
  published
    property Align;
    property Anchors;
    property Color;
    property ParentColor;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property EditMode           : TgmEditorEditingMode           read FEditMode           write SetEditMode;
    property Gradients          : TgmGradientList                read GetGradients        write SetGradients;
    property GradientIndex      : TgmGradientIndex               read FGradientIndex      write SetGradientIndex;
    property DefaultNewColor    : TColor                         read FDefaultNewColor    write FDefaultNewColor;
    property DefaultNewAlpha    : Byte                           read FDefaultNewAlpha    write FDefaultNewAlpha;
    property OnStateChanged     : TgmEditorStateChangedEvent     read FOnStateChanged     write FOnStateChanged;
    property OnCursorPosChanged : TgmEditorCursorPosChangedEvent read FOnCursorPosChanged write FOnCursorPosChanged;
  end;
  
implementation

uses
  gmMiscFuncs;

const
  STOP_HEAD_HEIGHT        = 10;
  DEFAULT_SPECTRUM_HEIGHT = 26;
  DEFAULT_SPECTRUM_WIDTH  = 384;
  MIN_SPECTRUM_HEIGHT     = 10;
  MIN_SPECTRUM_WIDTH      = 100;
  PREVIEW_BORDER_WIDTH    = 1;
  MID_POINT_MARK_RADIUS   = 4;


{ TgmGradientEditor }

constructor TgmGradientEditor.Create(AOwner: TComponent);
begin
  inherited;

  SetupInternalGradient();
  
  FChangeLink          := TgmGridBasedChangeLink.Create;
  FChangeLink.OnChange := GradientListChanged;

  FState               := gesIdle;
  FActionType          := eatDoNothing;
  FAlphaStopList       := TgmStopObjectList.Create;
  FColorStopList       := TgmStopObjectList.Create;
  FPreviewMap          := TBitmap32.Create;
  FGradientMap         := TBitmap32.Create;
  FStopOffset          := GetStopOffset;
  FStopHeight          := FStopOffset * 2 + STOP_HEAD_HEIGHT;
  FGradientIndex       := -1;
  FSelectedAlphaIndex  := -1;
  FSelectedColorIndex  := -1;
  FDefaultNewColor     := clBlack;
  FDefaultNewAlpha     := 255;
  FLastHorizMove       := 0;
  FMouseButtonDown     := False;

  // used to select a mid point
  FLeftMidPointMark     := TgmSimpleDiamond2D.Create(MID_POINT_MARK_RADIUS, MID_POINT_MARK_RADIUS);
  FRightMidPointMark    := TgmSimpleDiamond2D.Create(MID_POINT_MARK_RADIUS, MID_POINT_MARK_RADIUS);
  FSelectedMidPointMark := nil;

  FLeftStop     := nil; // pointer to a stop that at left of the selected color/alpha stop
  FRightStop    := nil; // pointer to a stop that at right of the selected color/alpha stop
  FSelectedStop := nil; // pointer to left/right stop

  // set up with properties
  SpectrumWidth  := DEFAULT_SPECTRUM_WIDTH;
  SpectrumHeight := DEFAULT_SPECTRUM_HEIGHT;

  FSpectrumRect         := GetSpectrumRect;
  FAlphaStopProcessArea := GetAlphaStopProcessArea;
  FColorStopProcessArea := GetColorStopProcessArea;

  // used to drag color in/out to/from the gradient
  FDragOutColor         := clBlack;
  FDragOutAlpha         := 0;
  FLeftDragOutMidScale  := 0.0;
  FRightDragOutMidScale := 0.0;

  // callback functions
  FOnCursorPosChanged := nil;
  FOnStateChanged     := nil;

  UpdateAlphaStopList;
  UpdateColorStopList;

  Height        := 74;
  FOldBoxWidth  := Width;
  FOldBoxHeight := Height;
end; 

destructor TgmGradientEditor.Destroy;
begin
  FSelectedMidPointMark := nil;
  FLeftStop             := nil;
  FRightStop            := nil;
  FSelectedStop         := nil;
  FOnCursorPosChanged   := nil;
  FOnStateChanged       := nil;

  FGradient.Free;
  FChangeLink.Free;

  FLeftMidPointMark.Free;
  FRightMidPointMark.Free;

  FAlphaStopList.Free;
  FColorStopList.Free;

  FGradientMap.Free;
  FPreviewMap.Free;

  inherited;
end;

procedure TgmGradientEditor.SetupInternalGradient;
begin
  if Assigned(FGradient) then
  begin
    FGradient.Free;
  end;

  FGradient := TgmGradientItem.Create(nil);
  FGradient.RGBGradient.Insert32(0.25, $FFFF0000);
  FGradient.RGBGradient.Insert32(0.5, $FF00FF00);
  FGradient.RGBGradient.Insert32(0.75, $FF0000FF);
end;

procedure TgmGradientEditor.GradientListChanged(ASender: TObject);
begin
  if Assigned(FGradients) then
  begin
    FGradient.BackgroundColor := FGradients.BackgroundColor;
    FGradient.ForegroundColor := FGradients.ForegroundColor;
  end;

  if ASender = Gradients then
  begin
    UpdateView(ASender);
  end;
end;

procedure TgmGradientEditor.UpdateView(ASender: TObject);
begin
  UpdateAlphaStopList;
  UpdateColorStopList;
  Invalidate;
end;

procedure TgmGradientEditor.UpdateAlphaStopList;
var
  i             : Integer;
  LInverseAlpha : Byte;
  LStopObj      : TgmStopObject;
  LTipPos       : TPoint;
begin
  if Assigned(Gradient) then
  begin
    // update alpha stops
    FAlphaStopList.DeleteAllStopObjects;

    LTipPos.Y := FStopHeight - PREVIEW_BORDER_WIDTH;
    
    for i := 0 to (Gradient.AlphaGradient.Count - 1) do
    begin
      LTipPos.X      := Round(Gradient.AlphaGradient[i].LocationScale * FSpectrumWidth);
      LStopObj       := TgmStopObject.Create(LTipPos, STOP_HEAD_HEIGHT, sodSouth);
      LInverseAlpha  := 255 - (Gradient.AlphaGradient[i].Value and $000000FF);
      LStopObj.Color := Gray32(LInverseAlpha);

      FAlphaStopList.Add(LStopObj);
    end;

    if FState = gesAlphaStopSelected then
    begin
      if (FSelectedAlphaIndex >= 0) and
         (FSelectedAlphaIndex < FAlphaStopList.Count) then
      begin
        FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);
      end;
    end;
  end;
end;

procedure TgmGradientEditor.UpdateColorStopList;
var
  i        : Integer;
  LStopObj : TgmStopObject;
  LTipPos  : TPoint;
begin
  if Assigned(Gradient) then
  begin
    // update RGB stops
    FColorStopList.DeleteAllStopObjects;

    LTipPos.Y := FPreviewMap.Height - FStopHeight - PREVIEW_BORDER_WIDTH;

    for i := 0 to (Gradient.RGBGradient.Count - 1) do
    begin
      LTipPos.X      := Round(Gradient.RGBGradient[i].LocationScale * FSpectrumWidth);
      LStopObj       := TgmStopObject.Create(LTipPos, STOP_HEAD_HEIGHT, sodNorth);
      LStopObj.Color := Color32(Gradient.RGBGradient[i].ValidValue);
      
      case Gradient.RGBGradient[i].Value of
        clDefault:
          begin
            LStopObj.StopType := sotDynamicForegroundColor;
          end;

        clBackground:
          begin
            LStopObj.StopType := sotDynamicBackgroundColor;
          end;
      end;

      FColorStopList.Add(LStopObj);
    end;

    if FState = gesColorStopSelected then
    begin
      if (FSelectedColorIndex >= 0) and
         (FSelectedColorIndex < FColorStopList.Count) then
      begin
        FColorStopList.ActivateStopByIndex(FSelectedColorIndex);
      end;
    end;
  end;
end;

procedure TgmGradientEditor.SetSpectrumHeight(const AValue: Integer);
begin
  if (AValue >= MIN_SPECTRUM_HEIGHT) and (AValue <> FSpectrumHeight) then
  begin
    FSpectrumHeight       := AValue;
    FPreviewMap.Height    := (FStopHeight * 2) + FSpectrumHeight + (PREVIEW_BORDER_WIDTH * 2);
    FSpectrumRect         := GetSpectrumRect;
    FAlphaStopProcessArea := GetAlphaStopProcessArea;
    FColorStopProcessArea := GetColorStopProcessArea;
  end;
end;

procedure TgmGradientEditor.SetSpectrumWidth(const AValue: Integer);
begin
  if (AValue >= MIN_SPECTRUM_WIDTH) and (AValue <> FSpectrumWidth) then
  begin
    FSpectrumWidth        := AValue;
    FPreviewMap.Width     := (FStopOffset * 2) + FSpectrumWidth;
    FSpectrumRect         := GetSpectrumRect;
    FAlphaStopProcessArea := GetAlphaStopProcessArea;
    FColorStopProcessArea := GetColorStopProcessArea;
  end;
end;

procedure TgmGradientEditor.ProcessAlphaGradientWhenMouseDown(
  const AX, AY: Integer);
var
  LAlphaIndex         : Integer;
  LAlphaLocationScale : Single;
begin
  LAlphaIndex := FAlphaStopList.PointOnStops(AX, AY);

  // if point on a primary Alpha stop...
  if LAlphaIndex >= 0 then
  begin
    FSelectedMidPointMark := nil;
    FSelectedStop         := nil;
    FActionType           := eatSelectingAlphaStop;
    FState                := gesAlphaStopSelected;

    FSelectedAlphaIndex   := LAlphaIndex;
    FDefaultNewAlpha      := Gradient.AlphaGradient[FSelectedAlphaIndex].Value and $000000FF;

    FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

    // deselect color stop
    FSelectedColorIndex := -1;
    FColorStopList.DeactivateAllStops;
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
        FSelectedStop          := FLeftStop;
        FSelectedMidPointIndex := FSelectedAlphaIndex - 1;
      end
      else
      if FSelectedMidPointMark = FRightMidPointMark then
      begin
        FSelectedStop          := FRightStop;
        FSelectedMidPointIndex := FSelectedAlphaIndex;
      end;
    end
    else
    begin
      FSelectedStop := nil;

      // if none of any stops or mid points are selected,
      // then to see whether we could add a primary Alpha
      if PtInRect( FAlphaStopProcessArea, Point(AX, AY) ) then
      begin
        LAlphaLocationScale := AX / FSpectrumWidth;
        FSelectedAlphaIndex := Gradient.AlphaGradient.Insert(LAlphaLocationScale, FDefaultNewAlpha);
        FActionType         := eatSelectingAlphaStop;
        FState              := gesAlphaStopSelected;

        UpdateAlphaStopList;
        FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

        // deselect color stops
        FSelectedColorIndex := -1;
        FColorStopList.DeactivateAllStops;
      end;
    end;
  end;
end;

procedure TgmGradientEditor.ProcessColorGradientWhenMouseDown(
  const AX, AY: Integer);
var
  LColorIndex         : Integer;
  LColorLocationScale : Single;
begin
  LColorIndex := FColorStopList.PointOnStops(AX, AY);

  // if point on a primary color stop...
  if LColorIndex >= 0 then
  begin
    FSelectedMidPointMark := nil;
    FSelectedStop         := nil;
    FActionType           := eatSelectingColorStop;
    FState                := gesColorStopSelected;

    FSelectedColorIndex   := LColorIndex;
    FDefaultNewColor      := (Gradient.RGBGradient[FSelectedColorIndex].Value);

    FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

    // deselect alpha stops
    FSelectedAlphaIndex := -1;
    FAlphaStopList.DeactivateAllStops;
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
        FSelectedStop          := FLeftStop;
        FSelectedMidPointIndex := FSelectedColorIndex - 1;
      end
      else
      if FSelectedMidPointMark = FRightMidPointMark then
      begin
        FSelectedStop          := FRightStop;
        FSelectedMidPointIndex := FSelectedColorIndex;
      end;
    end
    else
    begin
      FSelectedStop := nil;

      // if none of any stops or mid points are selected,
      // then to see whether we could add a primary color
      if PtInRect( FColorStopProcessArea, Point(AX, AY) ) then
      begin
        LColorLocationScale := AX / FSpectrumWidth;
        FSelectedColorIndex := Gradient.RGBGradient.Insert(LColorLocationScale, FDefaultNewColor);
        FActionType         := eatSelectingColorStop;
        FState              := gesColorStopSelected;

        UpdateColorStopList;
        FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

        // deselect alpha stops
        FSelectedAlphaIndex := -1;
        FAlphaStopList.DeactivateAllStops;
      end;
    end;
  end;
end;

procedure TgmGradientEditor.DrawMidPointMarks;
var
  LInverseAlpha : Byte;
begin
  if Assigned(Gradient) then
  begin
    // draw mid points between color stops ...
    if FState in [gesColorStopSelected, gesColorMidPointSelected] then
    begin
      // a mid point is selected ...
      if Assigned(FSelectedMidPointMark) then
      begin
        if Assigned(FSelectedStop) then
        begin
          FSelectedMidPointMark.FillColor := FSelectedStop.EndColor;
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
        if FSelectedColorIndex = (Gradient.RGBGradient.Count - 1) then
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
    if FState in [gesAlphaStopSelected, gesAlphaMidPointSelected] then
    begin
      // a mid point is selected ...
      if Assigned(FSelectedMidPointMark) then
      begin
        if Assigned(FSelectedStop) then
        begin
          LInverseAlpha := 255 - (FSelectedStop.EndColor and $000000FF);
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
        if FSelectedAlphaIndex = (Gradient.AlphaGradient.Count - 1) then
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
  end;
end;

procedure TgmGradientEditor.UpdateMidPointMarks;
var
  LCenterX, LCenterY : Integer;
begin
  FLeftStop  := nil;
  FRightStop := nil;

  if Assigned(Gradient) then
  begin
    Gradient.GradientLength := FSpectrumWidth;

    // when a primary color is selected...
    if (FSelectedColorIndex >= 0) and
       (FSelectedColorIndex < Gradient.RGBGradient.Count) then
    begin
      LCenterY := FSpectrumRect.Bottom + MID_POINT_MARK_RADIUS + 1;

      if FSelectedColorIndex = 0 then
      begin
        LCenterX := Gradient.RGBGradient[FSelectedColorIndex].MidPointLocation;

        FRightMidPointMark.Center := Point(LCenterX, LCenterY);
        FRightStop                := Gradient.RGBGradient[FSelectedColorIndex];
      end
      else
      if FSelectedColorIndex = (Gradient.RGBGradient.Count - 1) then
      begin
        if Gradient.RGBGradient.Count > 1 then
        begin
          LCenterX := Gradient.RGBGradient[FSelectedColorIndex - 1].MidPointLocation;

          FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
          FLeftStop                := Gradient.RGBGradient[FSelectedColorIndex - 1];
        end;
      end
      else
      begin
        LCenterX := Gradient.RGBGradient[FSelectedColorIndex - 1].MidPointLocation;

        FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
        FLeftStop                := Gradient.RGBGradient[FSelectedColorIndex - 1];

        LCenterX := Gradient.RGBGradient[FSelectedColorIndex].MidPointLocation;

        FRightMidPointMark.Center := Point(LCenterX, LCenterY);
        FRightStop                := Gradient.RGBGradient[FSelectedColorIndex];
      end
    end
    else
    if (FSelectedAlphaIndex >= 0) and
       (FSelectedAlphaIndex < Gradient.AlphaGradient.Count) then // when a primary Alpha is selected...
    begin
      LCenterY := FSpectrumRect.Top - MID_POINT_MARK_RADIUS - 2;

      if FSelectedAlphaIndex = 0 then
      begin
        LCenterX := Gradient.AlphaGradient[FSelectedAlphaIndex].MidPointLocation;

        FRightMidPointMark.Center := Point(LCenterX, LCenterY);
        FRightStop                := Gradient.AlphaGradient[FSelectedAlphaIndex];
      end
      else
      if FSelectedAlphaIndex = (Gradient.AlphaGradient.Count - 1) then
      begin
        if Gradient.AlphaGradient.Count > 1 then
        begin
          LCenterX := Gradient.AlphaGradient[FSelectedAlphaIndex - 1].MidPointLocation;

          FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
          FLeftStop                := Gradient.AlphaGradient[FSelectedAlphaIndex - 1];
        end;
      end
      else
      begin
        LCenterX := Gradient.AlphaGradient[FSelectedAlphaIndex - 1].MidPointLocation;

        FLeftMidPointMark.Center := Point(LCenterX, LCenterY);
        FLeftStop                := Gradient.AlphaGradient[FSelectedAlphaIndex - 1];

        LCenterX := Gradient.AlphaGradient[FSelectedAlphaIndex].MidPointLocation;

        FRightMidPointMark.Center := Point(LCenterX, LCenterY);
        FRightStop                := Gradient.AlphaGradient[FSelectedAlphaIndex];
      end
    end;
  end;      
end;

procedure TgmGradientEditor.SetGradients(const AValue: TgmGradientList);
begin
  if FGradients <> nil then
  begin
    FGradients.UnregisterChanges(FChangeLink);
    FGradients.RemoveFreeNotification(Self);
  end;

  FGradients := AValue;
  
  if FGradients <> nil then
  begin
    FGradients.RegisterChanges(FChangeLink);
    FGradients.FreeNotification(Self);

    FGradient.BackgroundColor := AValue.BackgroundColor;
    FGradient.ForegroundColor := AValue.ForegroundColor;
  end;

  SetGradientIndex(FGradientIndex); // let GradientIndex to be validated
  StateReset;
end;

procedure TgmGradientEditor.SetGradientIndex(const AValue: TgmGradientIndex);
begin
  // always allow reselect, because user may need to reset the edited gradient editor
  FGradientIndex := AValue;

  if AValue >= 0 then
  begin
    if Assigned(FGradients) and
       FGradients.IsValidIndex(FGradientIndex) then
    begin
      FGradient.Assign(FGradients[FGradientIndex]);
      StateReset;
    end
    else
    begin
      SetupInternalGradient;
      StateReset;
    end;
  end
  else
  begin
    SetupInternalGradient;
    StateReset;
  end;

  UpdateView(Self);
end;

procedure TgmGradientEditor.SetEditMode(const AValue: TgmEditorEditingMode);
begin
  FEditMode := AValue;
end;

procedure TgmGradientEditor.StateReset;
begin
  FState              := gesIdle;
  FActionType         := eatDoNothing;
  FSelectedColorIndex := -1;
  FSelectedAlphaIndex := -1;

  FSelectedMidPointMark := nil;
  FLeftStop             := nil;
  FRightStop            := nil;
  FSelectedStop         := nil;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(Self, FState);
  end;
end;

function TgmGradientEditor.GetSpectrumRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if (FSpectrumWidth > 0) and (FSpectrumHeight > 0) then
  begin
    Result := Rect(FStopOffset, FStopHeight,
                   FStopOffset + FSpectrumWidth,
                   FStopHeight + FSpectrumHeight);
  end;
end;

function TgmGradientEditor.GetAlphaStopProcessArea: TRect;
begin
  Result := Rect(FSpectrumRect.Left, 0,
                 FSpectrumRect.Right, FSpectrumRect.Top);
end;

function TgmGradientEditor.GetColorStopProcessArea: TRect;
begin
  Result := Rect(FSpectrumRect.Left, FSpectrumRect.Bottom,
                 FSpectrumRect.Right, FPreviewMap.Height);
end;

function TgmGradientEditor.GetStopOffset: Integer;
var
  LTempTriangle : TgmEquilateralTriangle2D;
begin
  LTempTriangle := TgmEquilateralTriangle2D.Create(Point(0, 0), Point(0, STOP_HEAD_HEIGHT));
  try
    Result := Round(LTempTriangle.EdgeLength / 2) + PREVIEW_BORDER_WIDTH;
  finally
    LTempTriangle.Free;
  end;
end;

function TgmGradientEditor.GetGradient: TgmGradientItem;
begin
  if FGradientIndex < 0 then
  begin
    Result := FGradient;
  end
  else
  begin
    if Assigned(FGradients) and
       FGradients.IsValidIndex(FGradientIndex) then
    begin
      if (EditMode = eemCopyEdit) then
      begin
        Result := FGradient;
      end
      else
      begin
        Result := FGradients[FGradientIndex];
      end
    end
    else
    begin
      Result := FGradient;
    end;
  end;
end;

function TgmGradientEditor.GetGradients: TgmGradientList;
begin
  Result := FGradients;
end;

function TgmGradientEditor.GetCursorPostionType(
  const AX, AY: Integer): TgmEditorCursorPosition;
var
  LPointOnControl : Boolean;
begin
  Result := ecpOutOfEnable;

  // if mouse cursor is in Alpha stop process area...
  if (AY >= FAlphaStopProcessArea.Top) and
     (AY <= FAlphaStopProcessArea.Bottom) then
  begin
    // check for whether the cursor is moved on any of alpha stops ...

    // if point on an alpha stop ...
    LPointOnControl := (FAlphaStopList.PointOnStops(AX, AY) >= 0);
    
    if LPointOnControl then
    begin
      Result := ecpOnStop;
    end
    else
    begin
      // if not point on an alpha stop, check for whether point on an alpha mid point
      if FSelectedAlphaIndex >= 0 then
      begin
        LPointOnControl := ( PointOnAlphaMidPoint(FSelectedAlphaIndex, AX, AY) <> nil );
        Result          := ecpOnMidPoint;
      end;
    end;

    // if neither point on an alpha stop nor an alpha mid point...
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

    // if neither point on a color stop nor a color mid point...
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
end;

function TgmGradientEditor.PointOnAlphaMidPoint(
  const AAlphaIndex, AX, AY: Integer): TgmSimpleDiamond2D;
begin
  Result := nil;

  if (AAlphaIndex >= 0) and Assigned(Gradient) then
  begin
    if AAlphaIndex = 0 then
    begin
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end
    else
    if AAlphaIndex = (Gradient.AlphaGradient.Count - 1) then
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
end; 

function TgmGradientEditor.PointOnColorMidPoint(
  const AColorIndex, AX, AY: Integer): TgmSimpleDiamond2D;
begin
  Result := nil;

  if (AColorIndex >= 0) and Assigned(Gradient) then
  begin
    if AColorIndex = 0 then
    begin
      if FRightMidPointMark.PointInDiamond(AX, AY) then
      begin
        Result := FRightMidPointMark;
      end;
    end
    else
    if AColorIndex = (Gradient.RGBGradient.Count - 1) then
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
end;

procedure TgmGradientEditor.DoPaintBuffer;
var
  LNeedUpdateStops : Boolean;
begin
  LNeedUpdateStops := False;

  if Width <> FOldBoxWidth then
  begin
    SpectrumWidth    := Width - 2 * FStopOffset;
    FOldBoxWidth     := Width;
    LNeedUpdateStops := True;
  end;

  if Height <> FOldBoxHeight then
  begin
    SpectrumHeight   := Height - 2 * FStopHeight - 1;
    FOldBoxHeight    := Height;
    LNeedUpdateStops := True;
  end;
  
  FPreviewMap.Clear( Color32(Self.Color) );
  DrawCheckerboard(FPreviewMap, FSpectrumRect);

  if Assigned(Gradient) then
  begin
    Gradient.GradientLength := FSpectrumWidth;
    Gradient.RefreshColorArray;

    FGradientMap.SetSize(FSpectrumWidth, FSpectrumHeight);
    Gradient.DrawColorGradients(FGradientMap);
    FGradientMap.DrawMode := dmBlend;

    // draw gradient spectrum
    FPreviewMap.Draw(FStopOffset, FStopHeight, FGradientMap);

    // draw mid point marks
    UpdateMidPointMarks;
    DrawMidPointMarks;

    if LNeedUpdateStops then
    begin
      UpdateAlphaStopList;
      UpdateColorStopList;
    end;

    // draw alpha and color stops
    FAlphaStopList.DrawAllStopObjects(FPreviewMap, FStopOffset, 0);
    FColorStopList.DrawAllStopObjects(FPreviewMap, FStopOffset, 0);
  end;

  FPreviewMap.FrameRectS(FStopOffset, FStopHeight,
                         FStopOffset + FSpectrumWidth + 1,
                         FStopHeight + FSpectrumHeight + 1,
                         clBlack32);

  Buffer.Draw( (Width - FPreviewMap.Width) div 2,
               (Height - FPreviewMap.Height) div 2, FPreviewMap );

  BufferValid := True;
end;

procedure TgmGradientEditor.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGradients) then
  begin
    FGradients := nil;

    SetupInternalGradient(); //Okay, we can reset this gradient!
    UpdateView(Self);
  end;
end;

procedure TgmGradientEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LColorIndex        : Integer;
  LActualX, LActualY : Integer;
  LColor             : TColor;
  LSelectedColor     : TColor;
begin
  LActualX := X - FStopOffset;
  LActualY := Y;

  if Button = mbLeft then
  begin
    if (Y >= FColorStopProcessArea.Top) and
       (Y <= FColorStopProcessArea.Bottom) then
    begin
      // when mouse is at color stop process area ...
      ProcessColorGradientWhenMouseDown(LActualX, LActualY);
    end
    else
    if (Y >= FAlphaStopProcessArea.Top) and
       (Y <= FAlphaStopProcessArea.Bottom) then
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
    if (Y >= FColorStopProcessArea.Top) and
       (Y <= FColorStopProcessArea.Bottom) then
    begin
      LColorIndex := FColorStopList.PointOnStops(LActualX, LActualY);

      // if point on a primary color stop...
      if LColorIndex >= 0 then
      begin
        FSelectedMidPointMark := nil;
        FSelectedStop         := nil;
        FActionType           := eatSelectingColorStop;
        FState                := gesColorStopSelected;

        FSelectedColorIndex   := LColorIndex;
        FDefaultNewColor      := Gradient.RGBGradient[FSelectedColorIndex].Value;

        // deselect alpha stops
        FSelectedAlphaIndex := -1;
        FAlphaStopList.DeactivateAllStops;

        // switch color stop types...
        LSelectedColor := Gradient.RGBGradient[FSelectedColorIndex].Value;

        case LSelectedColor of
          clDefault:
            begin
              Gradient.RGBGradient[FSelectedColorIndex].Value := clBackground;
              FDefaultNewColor := Gradient.RGBGradient[FSelectedColorIndex].Value;
            end;

          clBackground:
            begin
              LColor := Gradient.RGBGradient[FSelectedColorIndex].SavedColor;
              Gradient.RGBGradient[FSelectedColorIndex].Value := LColor;
              FDefaultNewColor := Gradient.RGBGradient[FSelectedColorIndex].Value;
            end;

        else
          Gradient.RGBGradient[FSelectedColorIndex].SavedColor := Gradient.RGBGradient[FSelectedColorIndex].Value;
          Gradient.RGBGradient[FSelectedColorIndex].Value      := clDefault;
          FDefaultNewColor := Gradient.RGBGradient[FSelectedColorIndex].Value;
        end;

        UpdateColorStopList;
        FColorStopList.ActivateStopByIndex(FSelectedColorIndex);
      end;
    end
  end;

  Invalidate;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(Self, FState);
  end;

  if Assigned(OnMouseDown) then
  begin
    OnMouseDown(Self, Button, Shift, X, Y);
  end;

  if Button = mbLeft then
  begin
    FLastHorizMove   := X;
    FMouseButtonDown := True;
  end;
end;

procedure TgmGradientEditor.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  LIndex         : Integer;
  LTestX, LTestY : Integer;
  LLeftLimit     : Integer;
  LRightLimit    : Integer;
  LLocationScale : Single;
  LCursorPosType : TgmEditorCursorPosition;
begin
  if FMouseButtonDown then
  begin
    case FActionType of
//-- Color Stop process --------------------------------------------------------
      eatSelectingColorStop:
        begin
          if FSelectedColorIndex >= 0 then
          begin
            // if we drag an color out of the gradient
            if ( (Y < FColorStopProcessArea.Top) or
                 (Y > FColorStopProcessArea.Bottom) ) and
               ( Gradient.RGBGradient.Count > 2 ) then
            begin
              FActionType := eatDragColorStopOut;

              // remeber the info of dragged out color
              FDragOutColor := Gradient.RGBGradient[FSelectedColorIndex].Value;

              if (FSelectedColorIndex = 0) or
                 ( FSelectedColorIndex = (Gradient.RGBGradient.Count - 1) ) then
              begin
                FRightDragOutMidScale := Gradient.RGBGradient[FSelectedColorIndex].MidPoint;
              end
              else
              begin
                FLeftDragOutMidScale  := Gradient.RGBGradient[FSelectedColorIndex - 1].MidPoint;
                FRightDragOutMidScale := Gradient.RGBGradient[FSelectedColorIndex].MidPoint;
              end;

              Gradient.RGBGradient.Delete(FSelectedColorIndex);

              FSelectedColorIndex := -1;
              FState              := gesIdle;

              UpdateColorStopList;
            end
            else
            begin
              // try to change the position of a selected primary color...

              if X < FStopOffset then
              begin
                // when mouse move out to the left of gradient map...
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  X - FStopOffset, 0, FSpectrumWidth - 1);
              end
              else
              if X > (FPreviewMap.Width - FStopOffset * 2) then
              begin
                // when mouse move out to the right of gradient map...
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  X - (FPreviewMap.Width - FStopOffset * 2),
                  0, FSpectrumWidth - 1);
              end
              else
              begin
                FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex,
                  X - FLastHorizMove, 0, FSpectrumWidth - 1);
              end;

              if Assigned(Gradient) then
              begin
                LLocationScale := FColorStopList.GetStopHeadTipX(FSelectedColorIndex) / FSpectrumWidth;
                LIndex         := Gradient.RGBGradient.ChangeLocationScale(FSelectedColorIndex, LLocationScale);

                { The index of primary color could be changed, so the index of
                  color stops should also be changed. }
                if LIndex <> FSelectedColorIndex then
                begin
                  FColorStopList.ChangeStopIndex(FSelectedColorIndex, LIndex);
                  FSelectedColorIndex := LIndex;
                end;
              end;

              FLastHorizMove := X;
            end;
          end;
        end;

      eatSelectingColorMidPoint:
        begin
          if Assigned(FSelectedStop) and
             Assigned(FSelectedMidPointMark) then
          begin
            LLeftLimit  := Round(FSelectedStop.LocationScale * SpectrumWidth);
            LRightLimit := Round(FSelectedStop.NextLocationScale * SpectrumWidth);
            LTestX      := X - FStopOffset;

            if (LTestX > LLeftLimit) and (LTestX < LRightLimit) then
            begin
              LLocationScale := (LTestX  - LLeftLimit) /
                ((FSelectedStop.NextLocationScale - FSelectedStop.LocationScale) * SpectrumWidth);

              if LLocationScale > 0.95 then
              begin
                LLocationScale := 0.95;
              end
              else if LLocationScale < 0.05 then
              begin
                LLocationScale := 0.05;
              end;
              
              FSelectedStop.MidPoint := LLocationScale;
            end;
          end;
        end;

      eatDragColorStopOut:
        begin
          // try to drag the color in....

          if PtInRect( FColorStopProcessArea, Point(X, Y) ) then
          begin
            LLocationScale      := (X - FStopOffset) / FSpectrumWidth;
            FSelectedColorIndex := Gradient.RGBGradient.Insert(LLocationScale, FDragOutColor);

            if (FSelectedColorIndex = 0) or
               ( FSelectedColorIndex = (Gradient.RGBGradient.Count - 1) ) then
            begin
              Gradient.RGBGradient[FSelectedColorIndex].MidPoint := FRightDragOutMidScale;
            end
            else
            begin
              Gradient.RGBGradient[FSelectedColorIndex - 1].MidPoint := FLeftDragOutMidScale;
              Gradient.RGBGradient[FSelectedColorIndex].MidPoint     := FRightDragOutMidScale;
            end;

            UpdateColorStopList;
            FColorStopList.ActivateStopByIndex(FSelectedColorIndex);

            FActionType    := eatSelectingColorStop;
            FState         := gesColorStopSelected;
            FLastHorizMove := X;
          end;
        end;

//-- Alpha Stop Process --------------------------------------------------------

      eatSelectingAlphaStop:
        begin
          if FSelectedAlphaIndex >= 0 then
          begin
            // if we drag an alpha out of the gradient
            if ( (Y < FAlphaStopProcessArea.Top) or
                 (Y > FAlphaStopProcessArea.Bottom) ) and
               ( Gradient.AlphaGradient.Count > 2 ) then
            begin
              FActionType := eatDragAlphaStopOut;

              // remeber the info of dragged out alpha
              FDragOutAlpha := Gradient.AlphaGradient[FSelectedAlphaIndex].ByteValue;

              if (FSelectedAlphaIndex = 0) or
                 ( FSelectedAlphaIndex = (Gradient.AlphaGradient.Count - 1) ) then
              begin
                FRightDragOutMidScale := Gradient.AlphaGradient[FSelectedAlphaIndex].MidPoint;
              end
              else
              begin
                FLeftDragOutMidScale  := Gradient.AlphaGradient[FSelectedAlphaIndex - 1].MidPoint;
                FRightDragOutMidScale := Gradient.AlphaGradient[FSelectedAlphaIndex].MidPoint;
              end;

              Gradient.AlphaGradient.Delete(FSelectedAlphaIndex);

              FSelectedAlphaIndex := -1;
              FState              := gesIdle;

              UpdateAlphaStopList;
            end
            else
            begin
              // try to change the position of a selected primary Alpha ...

              if X < FStopOffset then
              begin
                // when mouse move out to the left of gradient map...
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  X - FStopOffset, 0, FSpectrumWidth - 1);
              end
              else
              if X > (FPreviewMap.Width - FStopOffset * 2) then
              begin
                // when mouse move out to the right of gradient map...
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  X - (FPreviewMap.Width - FStopOffset * 2),
                  0, FSpectrumWidth - 1);
              end
              else
              begin
                FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex,
                  X - FLastHorizMove, 0, FSpectrumWidth - 1);
              end;

              if Assigned(Gradient) then
              begin
                LLocationScale := FAlphaStopList.GetStopHeadTipX(FSelectedAlphaIndex) / FSpectrumWidth;
                LIndex         := Gradient.AlphaGradient.ChangeLocationScale(FSelectedAlphaIndex, LLocationScale);

                { The index of primary Alpha could be changed, so the index of
                  Alpha stops should also be changed. }
                if LIndex <> FSelectedAlphaIndex then
                begin
                  FAlphaStopList.ChangeStopIndex(FSelectedAlphaIndex, LIndex);
                  FSelectedAlphaIndex := LIndex;
                end;
              end;

              FLastHorizMove := X;
            end;
          end;
        end;

      eatSelectingAlphaMidPoint:
        begin
          if Assigned(FSelectedStop) and
             Assigned(FSelectedMidPointMark) then
          begin
            LLeftLimit  := Round(FSelectedStop.LocationScale * SpectrumWidth);
            LRightLimit := Round(FSelectedStop.NextLocationScale * SpectrumWidth);
            LTestX      := X - FStopOffset;

            if (LTestX > LLeftLimit) and (LTestX < LRightLimit) then
            begin
              LLocationScale := (LTestX  - LLeftLimit) /
                ((FSelectedStop.NextLocationScale - FSelectedStop.LocationScale) * SpectrumWidth);

              if LLocationScale > 0.95 then
              begin
                LLocationScale := 0.95;
              end
              else if LLocationScale < 0.05 then
              begin
                LLocationScale := 0.05;
              end;

              FSelectedStop.MidPoint := LLocationScale;
            end;
          end;
        end;

      eatDragAlphaStopOut:
        begin
          // try to drag the Alpha in....

          if PtInRect(FAlphaStopProcessArea, Point(X, Y) ) then
          begin
            LLocationScale      := (X - FStopOffset) / FSpectrumWidth;
            FSelectedAlphaIndex := Gradient.AlphaGradient.Insert(LLocationScale, FDragOutAlpha);

            if (FSelectedAlphaIndex = 0) or
               ( FSelectedAlphaIndex = (Gradient.AlphaGradient.Count - 1) ) then
            begin
              Gradient.AlphaGradient[FSelectedAlphaIndex].MidPoint := FRightDragOutMidScale;
            end
            else
            begin
              Gradient.AlphaGradient[FSelectedAlphaIndex - 1].MidPoint := FLeftDragOutMidScale;
              Gradient.AlphaGradient[FSelectedAlphaIndex].MidPoint     := FRightDragOutMidScale;
            end;

            UpdateAlphaStopList;
            FAlphaStopList.ActivateStopByIndex(FSelectedAlphaIndex);

            FActionType    := eatSelectingAlphaStop;
            FState         := gesAlphaStopSelected;
            FLastHorizMove := X;
          end;
        end;
    end;

    Invalidate;
  end
  else
  begin
    LTestX := X - FStopOffset;
    LTestY := Y;

    LCursorPosType := GetCursorPostionType(LTestX, LTestY);

    if Assigned(FOnCursorPosChanged) then
    begin
      FOnCursorPosChanged(Self, LCursorPosType);
    end;
  end;

  if Assigned(OnMouseMove) then
  begin
    OnMouseMove(Self, Shift, X, Y);
  end;
end;

procedure TgmGradientEditor.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonDown := False;
  FActionType      := eatDoNothing;

  if Assigned(FOnStateChanged) then
  begin
    FOnStateChanged(Self, FState);
  end;

  if Assigned(OnMouseUp) then
  begin
    OnMouseUp(Self, Button, Shift, X, Y);
  end;
end; 

procedure TgmGradientEditor.Resize;
begin
  SpectrumWidth  := Width - 2 * FStopOffset;
  SpectrumHeight := Height - 2 * FStopHeight - 1;

  UpdateAlphaStopList;
  UpdateColorStopList;
  
  inherited;
end;

procedure TgmGradientEditor.AlphaStopDistributeAverage;
begin
  Gradient.AlphaGradient.DistributeAverage;
  UpdateAlphaStopList;
  Invalidate;
end;

procedure TgmGradientEditor.ColorStopDistributeAverage;
begin
  Gradient.RGBGradient.DistributeAverage;
  UpdateColorStopList;
  Invalidate;
end;

function TgmGradientEditor.ChangeSelectedAlphaValue(
  const AValue: Byte): Boolean;
var
  LInversedAlpha : Byte;
  LColor         : TColor32;
begin
  Result := False;

  if Assigned(Gradient) and
     (FState = gesAlphaStopSelected) and
     (FSelectedAlphaIndex >= 0) and
     (FSelectedAlphaIndex < Gradient.AlphaGradient.Count) then
  begin
    LInversedAlpha := 255 - AValue;
    LColor         := Gray32(LInversedAlpha);

    Gradient.AlphaGradient[FSelectedAlphaIndex].Value := AValue;
    FAlphaStopList.ChangeStopColor(FSelectedAlphaIndex, LColor);
    Invalidate;

    Result := True;
  end;
end;

function TgmGradientEditor.ChangeSelectedPrimaryColor(
  const AColor: TColor): Boolean;
begin
  Result := False;

  if Assigned(Gradient) and
     (FState = gesColorStopSelected) and
     (FSelectedColorIndex >= 0) and
     (FSelectedColorIndex < Gradient.RGBGradient.Count) then
  begin
    Gradient.RGBGradient[FSelectedColorIndex].Value := AColor;
    FColorStopList.ChangeStopColor( FSelectedColorIndex, Color32(AColor) );

    case AColor of
      clDefault :
        begin
          FColorStopList.ChangeStopType(FSelectedColorIndex, sotDynamicForegroundColor);
        end;

      clBackground :
        begin
          FColorStopList.ChangeStopType(FSelectedColorIndex, sotDynamicBackgroundColor);
        end;
        
    else
      FColorStopList.ChangeStopType(FSelectedColorIndex, sotStaticColor);
    end;

    Invalidate;

    Result := True;
  end;
end;

function TgmGradientEditor.ChangeSelectedAlphaLocationScale(
  const AValue: Double): Boolean;
var
  LNewIndex : Integer;
  LDeltaX   : Integer;
  LOldScale : Double;
begin
  Result := False;

  if FState = gesAlphaStopSelected then
  begin
    if Assigned(Gradient) and
       (FSelectedAlphaIndex >= 0) and
       (FSelectedAlphaIndex < FAlphaStopList.Count) then
    begin
      LOldScale := Gradient.AlphaGradient.Items[FSelectedAlphaIndex].LocationScale;
      LNewIndex := Gradient.AlphaGradient.ChangeLocationScale(FSelectedAlphaIndex, AValue);

      { The index of primary Alpha could be changed, so the index of
        Alpha stops should also be changed. }
      if LNewIndex <> FSelectedAlphaIndex then
      begin
        FAlphaStopList.ChangeStopIndex(FSelectedAlphaIndex, LNewIndex);
        FSelectedAlphaIndex := LNewIndex;
      end;

      // change position of alphas stop
      LDeltaX := Round( (AValue - LOldScale) * Gradient.GradientLength );

      FAlphaStopList.HorizTranslateStopByIndex(FSelectedAlphaIndex, LDeltaX,
                                               0, FSpectrumWidth - 1);


      Invalidate;
      Result := True;
    end;
  end;
end;

function TgmGradientEditor.ChangeSelectedColorLocationScale(
  const AValue: Double): Boolean;
var
  LNewIndex : Integer;
  LDeltaX   : Integer;
  LOldScale : Double;
begin
  Result := False;

  if FState = gesColorStopSelected then
  begin
    if Assigned(Gradient) and
       (FSelectedColorIndex >= 0) and
       (FSelectedColorIndex < FColorStopList.Count) then
    begin
      LOldScale := Gradient.RGBGradient.Items[FSelectedColorIndex].LocationScale;
      LNewIndex := Gradient.RGBGradient.ChangeLocationScale(FSelectedColorIndex, AValue);

      { The index of primary Color could be changed, so the index of
        Color stops should also be changed. }
      if LNewIndex <> FSelectedColorIndex then
      begin
        FColorStopList.ChangeStopIndex(FSelectedColorIndex, LNewIndex);
        FSelectedColorIndex := LNewIndex;
      end;

      // change position of alphas stop
      LDeltaX := Round( (AValue - LOldScale) * Gradient.GradientLength );

      FColorStopList.HorizTranslateStopByIndex(FSelectedColorIndex, LDeltaX,
                                               0, FSpectrumWidth - 1);


      Invalidate;
      Result := True;
    end;
  end;
end;

function TgmGradientEditor.ChangeSelectedMidPointScale(
  const AValue: Double): Boolean;
begin
  Result := False;

  if FState in [gesAlphaMidPointSelected, gesColorMidPointSelected] then
  begin
    if Assigned(Gradient) and
       Assigned(FSelectedStop) then
    begin
      FSelectedStop.MidPoint := AValue;
      Invalidate;
      Result := True;
    end;
  end;
end;

function TgmGradientEditor.DeleteSelectedAlphaStop: Boolean;
begin
  Result := False;

  if FState = gesAlphaStopSelected then
  begin
    if Assigned(Gradient) and
       (FSelectedAlphaIndex >= 0) and
       (FSelectedAlphaIndex < FAlphaStopList.Count) then
    begin
      Gradient.AlphaGradient.Delete(FSelectedAlphaIndex);
      StateReset;
      UpdateAlphaStopList;
      Invalidate;
      
      Result := True;
    end;
  end;
end;

function TgmGradientEditor.DeleteSelectedColorStop: Boolean;
begin
  Result := False;

  if FState = gesColorStopSelected then
  begin
    if Assigned(Gradient) and
       (FSelectedColorIndex >= 0) and
       (FSelectedColorIndex < FColorStopList.Count) then
    begin
      Gradient.RGBGradient.Delete(FSelectedColorIndex);
      StateReset;
      UpdateColorStopList;
      Invalidate;
      
      Result := True;
    end;
  end;
end;


end.
