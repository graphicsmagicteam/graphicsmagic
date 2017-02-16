unit gmStopObject;

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
{ Standard }
  Classes,
{ Graphics32 }
  GR32, GR32_LowLevel,
{ GraphicsMagic Lib }
  gmGeometricObjects2D;

type
  TgmStopObjectDirection = (sodNorth, sodSouth);

  TgmStopObjectType = (sotStaticColor,
                       sotDynamicForegroundColor,
                       sotDynamicBackgroundColor);


//-- TgmStopObject -------------------------------------------------------------

  TgmStopObject = class(TObject)
  private
    FHead         : TgmEquilateralTriangle2D;
    FBody         : TRect;
    FDirection    : TgmStopObjectDirection;
    FTipPosition  : TPoint;
    FBasePosition : TPoint;
    FHeadHeight   : Integer;
    FActive       : Boolean; // if this object is active
    FColor        : TColor32;
    FStopType     : TgmStopObjectType;

    function GetBasePosition: TPoint;
    function GetBodyRect: TRect;
    function GetHeight: Integer;
    function GetHeadTipX: Integer;
  public
    constructor Create(const ATipPosition: TPoint; const AHeadHeight: Integer;
      const ADirection: TgmStopObjectDirection);
      
    destructor Destroy; override;

    function Draw(const ABmp: TBitmap32; const AOffsetX, AOffsetY: Integer): Boolean;
    function PointOnStop(const AX, AY: Integer): Boolean;

    procedure Translate(const ADeltaX, ADeltaY: Integer);
    procedure HorizTranslate(const ADeltaX, ARangeMin, ARangeMax: Integer);

    property IsActive: Boolean           read FActive   write FActive;
    property Color   : TColor32          read FColor    write FColor;
    property StopType: TgmStopObjectType read FStopType write FStopType;
    property Height  : Integer           read GetHeight;
    property HeadTipX: Integer           read GetHeadTipX;
  end;

//-- TgmStopObjectList ---------------------------------------------------------

  TgmStopObjectList = class(TList)
  private

  public
    destructor Destroy; override;

    procedure DeleteAllStopObjects;
    procedure DeactivateAllStops;

    function ActivateStopByIndex(const AIndex: Integer): Boolean;
    function ChangeStopIndex(const AOldIndex, ANewIndex: Integer): Boolean;
    function ChangeStopColor(const AIndex: Integer; const AColor: TColor32): Boolean;
    function ChangeStopType(const AIndex: Integer; const AType: TgmStopObjectType): Boolean;
    function TranslateStopByIndex(const AIndex, ADeltaX, ADeltaY: Integer): Boolean;

    function HorizTranslateStopByIndex(
      const AIndex, ADeltaX, ARangeMin, ARangeMax: Integer): Boolean;

    function DrawAllStopObjects(const ABmp: TBitmap32;
      const AOffsetX, AOffsetY: Integer): Boolean;

    function PointOnStops(const AX, AY: Integer): Integer;
    function GetStopHeadTipX(const AIndex: Integer): Integer;
  end;

implementation

uses
{ GR32_Lines lib by Angus Johnson }
  GR32_Misc,
{ GraphicsMagic Lib }
  gmMiscFuncs;

//-- TgmStopObject -------------------------------------------------------------

constructor TgmStopObject.Create(const ATipPosition: TPoint;
  const AHeadHeight: Integer; const ADirection: TgmStopObjectDirection);
begin
  inherited Create;

  FActive       := False;
  FColor        := $FFFF0000;
  FStopType     := sotStaticColor;
  FDirection    := ADirection;
  FTipPosition  := ATipPosition;
  FHeadHeight   := AHeadHeight;
  FBasePosition := GetBasePosition;

  FHead := TgmEquilateralTriangle2D.Create(FTipPosition, FBasePosition);
  FBody := GetBodyRect;
end; { Create }

destructor TgmStopObject.Destroy;
begin
  FHead.Free;

  inherited Destroy;
end; { Destroy }

function TgmStopObject.GetBasePosition: TPoint;
begin
  case FDirection of
    sodNorth:
      begin
        Result.X := FTipPosition.X;
        Result.Y := FTipPosition.Y + FHeadHeight;
      end;

    sodSouth:
      begin
        Result.X := FTipPosition.X;
        Result.Y := FTipPosition.Y - FHeadHeight;
      end;
  end;
end; { GetBasePosition }

function TgmStopObject.GetBodyRect: TRect;
var
  LHeadEdgeLength : Integer;
  LTemp           : Integer;
  LPoint1, LPoint2: TPoint;
begin
  LPoint1 := FHead.Vertics[1];
  LPoint2 := FHead.Vertics[2];

  if LPoint1.X > LPoint2.X then
  begin
    LTemp     := LPoint1.X;
    LPoint1.X := LPoint2.X;
    LPoint2.X := LTemp;
  end;

  Inc(LPoint2.X, 1);
  Inc(LPoint2.Y, 1);

  LHeadEdgeLength := LPoint2.X - LPoint1.X;

  case FDirection of
    sodNorth:
      begin
        Result := Rect(LPoint1.X, LPoint1.Y, LPoint2.X, LPoint2.Y + LHeadEdgeLength);
      end;

    sodSouth:
      begin
        Result := Rect(LPoint1.X, LPoint1.Y - LHeadEdgeLength, LPoint2.X, LPoint2.Y);
      end;
  end;
end; { GetBodyRect }

function TgmStopObject.GetHeight: Integer;
begin
  Result := 0;
  
  case FDirection of
    sodNorth:
      begin
        Result := FBody.Bottom - FHead.Vertics[0].Y + 1;
      end;

    sodSouth:
      begin
        Result := FHead.Vertics[0].Y - FBody.Top + 1;
      end;
  end;
end; { GetHeight }

function TgmStopObject.GetHeadTipX: Integer;
begin
  Result := FTipPosition.X;
end; { GetHeadTipX }

function TgmStopObject.Draw(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  LFrameRect     : TRect;
  LFillRect      : TRect;
  LRect          : TRect;
  LRectCenterX   : Integer;
  LRectCenterY   : Integer;
begin
  Result := False;

  if Assigned(ABmp) then
  begin
    if FActive then
    begin
      if FStopType = sotStaticColor then
      begin
        FHead.FillColor := FColor;
      end
      else
      begin
        FHead.FillColor := clBlack32;
      end;
    end
    else
    begin
      FHead.FillColor := $00000000;
    end;
    
    FHead.Draw(ABmp, AOffsetX, AOffsetY);

    LFrameRect := FBody;
    LFillRect  := FBody;
    InflateRect(LFillRect, -2, -2);

    OffsetRect(LFillRect, AOffsetX, AOffsetY);
    OffsetRect(LFrameRect, AOffsetX, AOffsetY);

    case FStopType of
      sotStaticColor:
        begin
          ABmp.FillRectS(LFillRect, FColor);
        end;

      sotDynamicForegroundColor:
        begin
          ABmp.FillRectS(LFillRect, clWhite32);

          LRectCenterX := (LFillRect.Left + LFillRect.Right) div 2;
          LRectCenterY := (LFillRect.Top + LFillRect.Bottom) div 2;

          LRect := Rect(LRectCenterX, LRectCenterY,
                        LFillRect.Right, LFillRect.Bottom);

          ABmp.FillRectS(LRect, clGray32);

          LRect := Rect(LFillRect.Left, LFillRect.Top,
                        LRectCenterX, LRectCenterY);

          ABmp.FillRectS(LRect, clBlack32);
        end;

      sotDynamicBackgroundColor:
        begin
          ABmp.FillRectS(LFillRect, clWhite32);

          LRectCenterX := (LFillRect.Left + LFillRect.Right) div 2;
          LRectCenterY := (LFillRect.Top + LFillRect.Bottom) div 2;

          LRect := Rect(LFillRect.Left, LFillRect.Top,
                        LRectCenterX, LRectCenterY);

          ABmp.FillRectS(LRect, clGray32);

          LRect := Rect(LRectCenterX, LRectCenterY,
                        LFillRect.Right, LFillRect.Bottom);

          ABmp.FillRectS(LRect, clBlack32);
        end;
    end;

    ABmp.FrameRectS(LFrameRect, clBlack32);

    Result := True;
  end;
end; { Draw }

function TgmStopObject.PointOnStop(const AX, AY: Integer): Boolean;
begin
  Result := FHead.PointInTriangle(AX, AY) or PtInRect(FBody, Point(AX, AY)); 
end; { PointOnStop }

procedure TgmStopObject.Translate(const ADeltaX, ADeltaY: Integer);
begin
  FHead.Translate(ADeltaX, ADeltaY);

  FBody.Left   := FBody.Left + ADeltaX;
  FBody.Top    := FBody.Top + ADeltaY;
  FBody.Right  := FBody.Right + ADeltaX;
  FBody.Bottom := FBody.Bottom + ADeltaY;

  FTipPosition  := FHead.TipVertex;
  FBasePosition := FHead.BaseVertex;
end; { Translate }

procedure TgmStopObject.HorizTranslate(
  const ADeltaX, ARangeMin, ARangeMax: Integer);
var
  LActualDeltaX: Integer;
begin
  if ARangeMin >= ARangeMax then
  begin
    Exit;
  end;

  LActualDeltaX := FHead.HorizTranslate(ADeltaX, ARangeMin, ARangeMax);

  FBody.Left    := FBody.Left  + LActualDeltaX;
  FBody.Right   := FBody.Right + LActualDeltaX;

  FTipPosition  := FHead.TipVertex;
  FBasePosition := FHead.BaseVertex;
end; { HorizTranslate }

//-- TgmStopObjectList ---------------------------------------------------------

destructor TgmStopObjectList.Destroy;
begin
  Self.DeleteAllStopObjects;
  
  inherited Destroy;
end; { Destroy }

procedure TgmStopObjectList.DeleteAllStopObjects;
var
  i       : Integer;
  LStopObj: TgmStopObject;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LStopObj := TgmStopObject(Self.Items[i]);
      Self.Delete(i);

      if Assigned(LStopObj) then
      begin
        LStopObj.Free;
      end;
    end;
  end;
end; { DeleteAllStopObjects }

procedure TgmStopObjectList.DeactivateAllStops;
var
  i       : Integer;
  LStopObj: TgmStopObject;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LStopObj          := TgmStopObject(Self.Items[i]);
      LStopObj.IsActive := False;
    end;
  end;
end; { DeactivateAllStops }

function TgmStopObjectList.ActivateStopByIndex(const AIndex: Integer): Boolean;
var
  LStopObj: TgmStopObject;
begin
  Result := False;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    Self.DeactivateAllStops;

    LStopObj          := TgmStopObject(Self.Items[AIndex]);
    LStopObj.IsActive := True;
    Result            := True;
  end;
end; { ActivateStopByIndex }

function TgmStopObjectList.ChangeStopIndex(
  const AOldIndex, ANewIndex: Integer): Boolean;
var
  LNewIndex: Integer;
begin
  Result := False;

  if (Self.Count > 0) and
     (AOldIndex >= 0) and
     (AOldIndex < Self.Count) then
  begin
    LNewIndex := Clamp(ANewIndex, 0, Self.Count - 1);

    if AOldIndex <> LNewIndex then
    begin
      Self.Move(AOldIndex, ANewIndex);
      Result := True;
    end;
  end;
end; { ChangeStopIndex }

function TgmStopObjectList.ChangeStopColor(
  const AIndex: Integer; const AColor: TColor32): Boolean;
var
  LStopObj: TgmStopObject;
begin
  Result := False;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    LStopObj       := TgmStopObject(Self.Items[AIndex]);
    LStopObj.Color := AColor;
    Result         := True;
  end;
end; { ChangeStopColor }

function TgmStopObjectList.ChangeStopType(const AIndex: Integer;
  const AType: TgmStopObjectType): Boolean;
var
  LStopObj: TgmStopObject;
begin
  Result := False;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    LStopObj          := TgmStopObject(Self.Items[AIndex]);
    LStopObj.StopType := AType;
    Result            := True;
  end;
end; { ChangeStopType }

function TgmStopObjectList.TranslateStopByIndex(
  const AIndex, ADeltaX, ADeltaY: Integer): Boolean;
var
  LStopObj: TgmStopObject;
begin
  Result := False;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    LStopObj := TgmStopObject(Self.Items[AIndex]);
    LStopObj.Translate(ADeltaX, ADeltaY);
    
    Result := True;
  end;
end; { TranslateStopByIndex }

function TgmStopObjectList.HorizTranslateStopByIndex(
  const AIndex, ADeltaX, ARangeMin, ARangeMax: Integer): Boolean;
var
  LStopObj: TgmStopObject;
begin
  Result := False;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    LStopObj := TgmStopObject(Self.Items[AIndex]);
    LStopObj.HorizTranslate(ADeltaX, ARangeMin, ARangeMax);

    Result := True;
  end;
end; { HorizTranslateStopByIndex }

function TgmStopObjectList.DrawAllStopObjects(const ABmp: TBitmap32;
  const AOffsetX, AOffsetY: Integer): Boolean;
var
  i       : Integer;
  LStopObj: TgmStopObject;
begin
  Result := False;

  if Assigned(ABmp) and (Self.Count > 0) then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LStopObj := TgmStopObject(Self.Items[i]);
      LStopObj.Draw(ABmp, AOffsetX, AOffsetY);
    end;

    Result := True;
  end;
end; { DrawAllStopObjects }

function TgmStopObjectList.PointOnStops(const AX, AY: Integer): Integer;
var
  i       : Integer;
  LStopObj: TgmStopObject;
begin
  Result := -1;

  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LStopObj := TgmStopObject(Self.Items[i]);

      if LStopObj.PointOnStop(AX, AY) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end; { PointOnStops }

function TgmStopObjectList.GetStopHeadTipX(const AIndex: Integer): Integer;
var
  LStopObj: TgmStopObject;
begin
  Result := -1;

  if (Self.Count > 0) and
     (AIndex >= 0) and
     (AIndex < Self.Count) then
  begin
    LStopObj := TgmStopObject(Self.Items[AIndex]);
    Result   := LStopObj.HeadTipX;
  end;
end; { GetStopHeadTipX }

end.
