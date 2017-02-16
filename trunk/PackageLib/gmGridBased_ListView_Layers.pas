unit gmGridBased_ListView_Layers;

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
  Classes, Controls,
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic }
  gmGridBased;

type

  TgmCustomGridBasedViewLayer = class(TCustomLayer)
  private
    FPosition   : TFloatPoint;
    FScaled     : Boolean;
    FIsDragging : Boolean;

    procedure SetPosition(const AValue: TFloatPoint);
    procedure SetScaled(const AValue: Boolean);
  protected
    function GetAdjustedPosition: TFloatRect; 
    function DoHitTest(AX, AY: Integer): Boolean; override;
  public
    property Position : TFloatPoint read FPosition write SetPosition; //LeftTop
    property Scaled   : Boolean     read FScaled   write SetScaled;
  end;

  TgmGridBasedLayer = class(TgmCustomGridBasedViewLayer)
  private
    FCropped : Boolean;

    procedure SetCropped(const AValue: Boolean);
    function GetBitmap: TBitmap32;
  protected
    procedure Paint(ABuffer: TBitmap32); override;
    procedure ForcePaint(const ABuffer: TBitmap32);
    
    property Bitmap : TBitmap32 read GetBitmap;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    
    property Cropped : Boolean read FCropped write SetCropped;
  end;

  TgmSelectedLayer = class(TgmCustomGridBasedViewLayer)
  private
    FChildLayer  : TgmCustomGridBasedViewLayer;
    FOldPosition : TFloatPoint;
    FDragPos     : TPoint;
    FDragItem    : TgmGridBasedItem;

    procedure SetChildLayer(const AValue: TgmCustomGridBasedViewLayer);
  protected
    procedure Paint(ABuffer: TBitmap32); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property ChildLayer : TgmCustomGridBasedViewLayer read FChildLayer write SetChildLayer;
  end;


implementation

uses
{ Standard }
  SysUtils, Math,
{ Graphics32 }
  GR32_Resamplers, GR32_RepaintOpt,
{ GraphicsMagic }
  gmGridBased_ListView, gmGridBased_List, gmMiscFuncs;

type
  TImage32Access             = class(TCustomImage32);
  TgmGridBasedListViewAccess = class(TgmGridBasedListView);
  TgmGridBasedListAccess     = class(TgmGridBasedList);


function GetGridBasedView(const ALayer: TgmCustomGridBasedViewLayer): TgmGridBasedListViewAccess;
begin
  Result := nil;
  
  if Assigned(ALayer) then
  begin
    with ALayer do
    begin
      if (LayerCollection.Owner is TgmGridBasedListView) then
      begin
        Result := TgmGridBasedListViewAccess(LayerCollection.Owner);
      end;
    end;
  end;
end;

function GetGridBasedItem(const ALayer: TgmCustomGridBasedViewLayer): TgmGridBasedItem;
var
  LGV   : TgmGridBasedListViewAccess;
  LList : TgmGridBasedListAccess;
begin
  Result := nil;
  LGV    := GetGridBasedView(ALayer);

  if Assigned(LGV) then
  begin
    LList := TgmGridBasedListAccess(LGV.ItemList);
    
    if LList.IsValidIndex(ALayer.Index) then
    begin
      Result := LList.Collection.Items[ALayer.Index] as TgmGridBasedItem;
    end;
  end;
end;

{ TgmCustomGridBasedViewLayer }

procedure TgmCustomGridBasedViewLayer.SetPosition(const AValue: TFloatPoint);
begin
  if (FPosition.X <> AValue.X) or (FPosition.Y <> AValue.Y) then
  begin
    Changing;
    FPosition := AValue;
    Changed;
  end;
end;

procedure TgmCustomGridBasedViewLayer.SetScaled(const AValue: Boolean);
begin
  if AValue <> FScaled then
  begin
    Changing;
    FScaled := AValue;
    Changed;
  end;
end;

function TgmCustomGridBasedViewLayer.GetAdjustedPosition: TFloatRect;
var
  LScaleX, LScaleY : TFloat;
  LShiftX, LShiftY : TFloat;
  LGV              : TgmGridBasedListViewAccess;
begin
  LGV := GetGridBasedView(Self);
  
  if Assigned(LGV) then
  begin
    if Scaled then
    begin
      LayerCollection.GetViewportShift(LShiftX, LShiftY);
      LayerCollection.GetViewportScale(LScaleX, LScaleY);

      with Result, FPosition do
      begin
        Left   := X * LScaleX + LShiftX;
        Top    := Y * LScaleY + LShiftY;
        Right  := (X + LGV.ThumbWidth -1) * LScaleX + LShiftX;
        Bottom := (Y + LGV.ThumbHeight -1) * LScaleY + LShiftY;
      end;
    end
    else
    begin
      with FPosition do
      begin
        Result := FloatRect(X, Y, X + LGV.ThumbWidth, Y + LGV.ThumbHeight);
      end;
    end;
  end
  else
  begin
    with FPosition do
    begin
      // x2nie has no better idea if layer has no imgView
      Result := FloatRect(X, Y, X+6, Y+6);
    end;
  end;
end;

function TgmCustomGridBasedViewLayer.DoHitTest(AX, AY: Integer): Boolean;
begin
  with GetAdjustedPosition do
    Result := (AX >= Left) and (AX < Right) and (AY >= Top) and (AY < Bottom);
end;

{ TgmGridBasedLayer }

constructor TgmGridBasedLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
end;

procedure TgmGridBasedLayer.SetCropped(const AValue: Boolean);
begin
  if AValue <> FCropped then
  begin
    FCropped := AValue;
    Changed;
  end;
end;

function TgmGridBasedLayer.GetBitmap: TBitmap32;
var
  LGV   : TgmGridBasedListViewAccess;
  LItem : TgmGridBasedItem;
begin
  Result := nil;
  LItem  := GetGridBasedItem(Self);

  if Assigned(LItem) then
  begin
    LGV    := GetGridBasedView(Self);
    Result := LItem.CachedBitmap(LGV.CellWidth, LGV.CellHeight);
  end;
end;

procedure TgmGridBasedLayer.Paint(ABuffer: TBitmap32);
begin
  if not FIsDragging then
  begin
    FPosition := GetGridBasedView(Self).MatrixPosition(Self.Index);
    ForcePaint(ABuffer);
  end;
end;

procedure TgmGridBasedLayer.ForcePaint(const ABuffer: TBitmap32);
var
  LSrcRect, LDstRect        : TRect;
  LClipRect, LTempRect      : TRect;
  LBorderRect, LImageRect   : TRect;
  LLayerWidth, LLayerHeight : TFloat;
  LGV                       : TgmGridBasedListViewAccess;
begin
  if Bitmap.Empty then
  begin
    Exit;
  end;
  
  LDstRect    := MakeRect(GetAdjustedPosition);
  LBorderRect := LDstRect;
  LClipRect   := ABuffer.ClipRect;

  IntersectRect(LTempRect, LClipRect, LDstRect);
  if IsRectEmpty(LTempRect) then
  begin
    Exit;
  end;

  LGV      := GetGridBasedView(Self);
  LDstRect := LGV.CellRect(LDstRect);
  LSrcRect := MakeRect(0, 0, Bitmap.Width, Bitmap.Height);
  
  if Cropped and (LayerCollection.Owner is TCustomImage32) and
     ( not (TImage32Access(LayerCollection.Owner).PaintToMode) ) then
  begin
    with LDstRect do
    begin
      LLayerWidth  := Right - Left;
      LLayerHeight := Bottom - Top;
    end;
    
    if (LLayerWidth < 0.5) or (LLayerHeight < 0.5) then
    begin
      Exit;
    end;
    
    LImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
    IntersectRect(LClipRect, LClipRect, LImageRect);
  end;

  StretchTransfer(ABuffer, LDstRect, LClipRect, Bitmap, LSrcRect,
                  Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);

  FrameRectS(ABuffer, LBorderRect, clBlack32);
  
  if LGV.CellBorderStyle = borSwatch then
  begin
    //is selected?
    if (goSelection in LGV.GridOptions) and (Self = LGV.Selection) then
      FrameRectS(ABuffer, LBorderRect, Color32(LGV.SelectedColor))
  end
  else if LGV.CellBorderStyle = borContrastGrid then
  begin
    InflateRect(LBorderRect, -1, -1);
    
    //is selected?
    if (goSelection in LGV.GridOptions) and (Self = LGV.Selection) then
      FrameRectS(ABuffer, LBorderRect, Color32(LGV.SelectedColor))
    else
      FrameRectS(ABuffer, LBorderRect, Color32(LGV.FrameColor))
  end;
end;

{ TgmSelectedLayer }

constructor TgmSelectedLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  
  LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
end;

procedure TgmSelectedLayer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LGV : TgmGridBasedListViewAccess;
begin
  LGV := GetGridBasedView(Self);
  
  //update the DragState first.
  FIsDragging             := goDragable in LGV.GridOptions;
  FChildLayer.FIsDragging := Self.FIsDragging;

  FDragItem := GetGridBasedItem(Self.FChildLayer);
  if FIsDragging then
  begin
    FOldPosition := FPosition;
    FDragPos     := Point(X, Y); 
  end;
  
  inherited;
end;

procedure TgmSelectedLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LGV    : TgmGridBasedListViewAccess;
  LIndex : integer;
begin
  if FIsDragging then
  begin
    LGV    := GetGridBasedView(Self);
    LIndex := LGV.MatrixIndex(X, Y);

    if (LIndex <> FChildLayer.Index) and LGV.ItemList.IsValidIndex(LIndex) then
    begin
      FDragItem.Collection.BeginUpdate;
      FDragItem.Index := LIndex;
      FDragItem.Collection.EndUpdate;

      FChildLayer.Index := LIndex;
      LGV.ForceFullInvalidate;
    end;
    
    Position := FloatPoint(FOldPosition.X + X - FDragPos.X,
                           FOldPosition.Y + Y - FDragPos.Y);
                           
    ChildLayer.Position := Position;
  end;
  
  inherited;
end;

procedure TgmSelectedLayer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LGV : TgmGridBasedListView;
begin
  inherited;
  
  if FIsDragging then
  begin
    FIsDragging             := False;
    FChildLayer.FIsDragging := False;
    FDragItem               := nil;
    LGV                     := GetGridBasedView(Self);
    FPosition               := LGV.MatrixPosition(FChildLayer.Index);
    ChildLayer.Position     := FPosition;

    LGV.ForceFullInvalidate;
  end;
end;

procedure TgmSelectedLayer.Paint(ABuffer: TBitmap32);
var
  LDstRect : TRect;
begin
  LDstRect := MakeRect(GetAdjustedPosition);

  FrameRectS(ABuffer, LDstRect, clWhite32);
  TgmGridBasedLayer(ChildLayer).ForcePaint(ABuffer);
end;

procedure TgmSelectedLayer.SetChildLayer(
  const AValue: TgmCustomGridBasedViewLayer);
begin
  if Assigned(FChildLayer) then
  begin
    RemoveNotification(FChildLayer);
  end;
    
  FChildLayer := AValue;
  if Assigned(AValue) then
  begin
    Position := AValue.Position;
    Scaled   := AValue.Scaled;
    
    AddNotification(FChildLayer);
  end;
end;

end.
