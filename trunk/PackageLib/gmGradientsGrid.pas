unit gmGradientsGrid;

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
 * The Original Code is Gradient Editor.
 *
 * The Initial Developer of the Original Code are
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
  SysUtils, Classes, Controls, Graphics,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic }
  gmGradient, gmGradient_List, gmGridBased_List;

type
  TgmGrowFlow = (ZWidth2Bottom,
                 NHeight2Right,
                 OSquaredGrow,
                 XStretchInnerGrow);

  TgmGradientsGrid = class(TCustomPaintBox32, IgmGradientListSupport)
  private
    FGrowFlow      : TgmGrowFlow;
    FThumbSize     : Integer;
    FGradientIndex : TgmGradientIndex;
    FGradients     : TgmGradientList;
    FChangeLink    : TgmGridBasedChangeLink;
    FFrameColor    : TColor;
    FSelectedColor : TColor;

    procedure InvalidateSize;
    procedure GradientListChanged(ASender: TObject);
    procedure SetGrowFlow(const AValue: TgmGrowFlow);
    procedure SetThumbSize(const AValue: Integer);
    procedure SetFrameColor(const AValue: TColor);
    procedure SetSelectedColor(const AValue: TColor);
    procedure SetGradientIndex(const AValue: TgmGradientIndex);
    procedure SetGradients(const AValue: TgmGradientList);

    { IgmGradientListSupport }
    function GetGradients: TgmGradientList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoPaintBuffer; override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer); override;

    function CanAutoSize(var ANewWidth, ANewHeight: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SelectedGradient: TgmGradientItem;
    function GetGradientIndex(const AX, AY: Integer): Integer;
  published
    property Align;
    property AutoSize;
    property Anchors;
    property Color;
    property ParentColor;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property FrameColor    : TColor           read FFrameColor    write SetFrameColor    default clWhite;
    property SelectedColor : TColor           read FSelectedColor write SetSelectedColor default clBlack;
    property ThumbSize     : Integer          read FThumbSize     write SetThumbSize     default 64;
    property GrowFlow      : TgmGrowFlow      read FGrowFlow      write SetGrowFlow      default ZWidth2Bottom;
    property Gradients     : TgmGradientList  read GetGradients   write SetGradients;
    property GradientIndex : TgmGradientIndex read FGradientIndex write SetGradientIndex;
  end;


implementation

uses
{ Standard }
  Math,
{ GraphicsMagic }
  gmGradientRender, gmMiscFuncs;


{ TgmGradientsGrid }

constructor TgmGradientsGrid.Create(AOwner: TComponent);
begin
  inherited;

  FThumbSize     := 64;
  FGrowFlow      := ZWidth2Bottom;
  FFrameColor    := clWhite;
  FSelectedColor := clBlack;
  FGradientIndex := -1;

  FChangeLink          := TgmGridBasedChangeLink.Create;
  FChangeLink.OnChange := GradientListChanged;
end;

destructor TgmGradientsGrid.Destroy;
begin
  FChangeLink.Destroy;
  inherited;
end;

function TgmGradientsGrid.SelectedGradient: TgmGradientItem;
begin
  Result := nil;
  
  if (GradientIndex >= 0) and (GradientIndex < Gradients.Count) then
  begin
    Result := Gradients[GradientIndex];
  end;
end;

function TgmGradientsGrid.GetGradientIndex(const AX, AY: Integer): Integer;
var
  w, h, i, t, l : Integer;
begin
  Result := -1;

  if ( not Assigned(Gradients) ) or (Gradients.Count = 0) then
  begin
    Exit;
  end;

  i := 0;

  w := Width  div ThumbSize;
  h := Height div ThumbSize;

  l := Ceil(AX / ThumbSize) -1;
  t := Ceil(AY / ThumbSize) -1;

  case FGrowFlow of
    OSquaredGrow,
    XStretchInnerGrow,
    ZWidth2Bottom :
      begin
        i := t * w + l;
      end;

    NHeight2Right :
      begin
        i := l * h + t;
      end;
  end;

  if i < Gradients.Count then
  begin
    Result := i;
  end;
end;

procedure TgmGradientsGrid.InvalidateSize;
begin
  AdjustSize;
  Invalidate;
end;

procedure TgmGradientsGrid.GradientListChanged(ASender: TObject);
begin
  if ASender = Gradients then
  begin
    if FGradientIndex >= Gradients.Count then
    begin
      FGradientIndex := -1;
    end;
    
    InvalidateSize;
  end;
end;

procedure TgmGradientsGrid.SetGrowFlow(const AValue: TgmGrowFlow);
begin
  FGrowFlow := AValue;
  InvalidateSize;
end;

procedure TgmGradientsGrid.SetThumbSize(const AValue: Integer);
begin
  FThumbSize := AValue;
  InvalidateSize;
end;

procedure TgmGradientsGrid.SetFrameColor(const AValue: TColor);
begin
  FFrameColor := AValue;
  Invalidate;
end;

procedure TgmGradientsGrid.SetSelectedColor(const AValue: TColor);
begin
  FSelectedColor := AValue;
  Invalidate;
end;

procedure TgmGradientsGrid.SetGradientIndex(const AValue: TgmGradientIndex);
begin
  // always allow reselect. because user may need to reset
  // the edited gradient editor
  FGradientIndex := AValue;
  Invalidate;
end;

procedure TgmGradientsGrid.SetGradients(const AValue: TgmGradientList);
begin
  if FGradients <> nil then
  begin
    FGradients.UnRegisterChanges(FChangeLink);
    FGradients.RemoveFreeNotification(Self);
  end;

  FGradients := AValue;
  if FGradients <> nil then
  begin
    FGradients.RegisterChanges(FChangeLink);
    FGradients.FreeNotification(Self);

    FGradientIndex := -1;
  end;
  
  InvalidateSize;
end;

function TgmGradientsGrid.GetGradients: TgmGradientList;
begin
  Result := FGradients;
end;

procedure TgmGradientsGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FGradients) then
  begin
    FGradients     := nil;
    FGradientIndex := -1;
    Invalidate;
  end;
end; 

procedure TgmGradientsGrid.DoPaintBuffer;
var
  w, h, x, y, i              : Integer;
  LOffset,LEdge              : Integer;
  LThumbWidth, LThumbHeight  : Integer;
  LStartPoint, LEndPoint     : TPoint;
  LBmp                       : TBitmap32;
  LBorderRect, LGradientRect : TRect;
begin
  i := 0;

  if Assigned(FGradients) then
  begin
    //assume this.bound has been properly calculated.
    if ThumbSize < 10 then
    begin
      FThumbSize := 10;
    end;

    LThumbHeight := ThumbSize;
    LThumbWidth  := ThumbSize;
    LOffset      := 2;

    LBmp := TBitmap32.Create;
    try
      LBmp.SetSize(LThumbWidth - 2*LOffset+1, LThumbHeight - 2*LOffset+1);
      LBmp.DrawMode := dmBlend;

      //diagonal edge +> about 5% of diagonal
      LEdge       := Round( Sqrt(LBmp.Width * LBmp.Height) * 0.05 );
      LStartPoint := Point(LEdge, LEdge); //because zero based, start from 0
      LEndPoint   := Point(LBmp.Width - LEdge-1, LBmp.Height - LEdge-1);

      w := Width  div LThumbWidth;
      h := Height div LThumbHeight;

      Buffer.Clear( Color32(Self.Color) );

      for y := 0 to (h - 1) do
        for x := 0 to (w - 1) do
        begin
          case FGrowFlow of
            OSquaredGrow,
            XStretchInnerGrow,
            ZWidth2Bottom:
              begin
                i := y * w + x;
              end;

            NHeight2Right :
              begin
                i := x * h + y;
              end;
          end;

          if (i < Gradients.Count) and
             (x * LThumbWidth < Width) and
             (y * LThumbHeight < Height) then
          begin
            DrawLinearGradient(LBmp, LStartPoint, LEndPoint, Gradients[i], False);

            with LBorderRect do
            begin
              TopLeft     := Point(x * LThumbWidth, y * LThumbHeight);
              BottomRight := Point(Left + LThumbWidth , Top + LThumbHeight);
            end;

            LGradientRect := LBorderRect;
            InflateRect(LGradientRect, -LOffset, -LOffset);

            DrawCheckerboard(Buffer, LGradientRect, 3);
            FrameRectS(Buffer, LBorderRect, clBlack32);
            InflateRect(LBorderRect, -1, -1);

            //is selected?
            if i = GradientIndex then
            begin
              FrameRectS(Buffer, LBorderRect, Color32(FSelectedColor));
            end
            else
            begin
              FrameRectS(Buffer, LBorderRect, Color32(FFrameColor));
            end;

            Buffer.Draw(LGradientRect.Left, LGradientRect.Top, LBmp);
          end;
        end;

    finally
      LBmp.Free;
    end;
  end
  else
  begin
    Buffer.Clear(clBlack32);
  end;

  BufferValid := True;
end;

procedure TgmGradientsGrid.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
var
  w, h, i, t, l : Integer;
begin
  inherited;

  i := -1;

  w := Width  div ThumbSize;
  h := Height div ThumbSize;

  l := Ceil(AX / ThumbSize) -1;
  t := Ceil(AY / ThumbSize) -1;

  case FGrowFlow of
    OSquaredGrow,
    XStretchInnerGrow,
    ZWidth2Bottom :
      begin
        i := t * w + l;
      end;

    NHeight2Right :
      begin
        i := l * h + t;
      end;
  end;

  if i < Gradients.Count then
    GradientIndex := i;
end;

function TgmGradientsGrid.CanAutoSize(var ANewWidth,
  ANewHeight: Integer): Boolean;
var
  W, H: Integer;
begin
  Result := True;
  W      := 0;
  H      := 0;

  if ( not Assigned(Gradients) ) then
  begin
    Exit;
  end;

  if Gradients.Count <= 0 then
  begin
    Exit;
  end;
  
  case FGrowFlow of
    ZWidth2Bottom :
      begin
        W := ANewWidth div ThumbSize;
        H := Ceil(Gradients.Count / W);
      end;
      
    NHeight2Right :
      begin
        H := ANewHeight div ThumbSize;
        W := Ceil(Gradients.Count / H);
      end;
      
    OSquaredGrow :
      begin
        W := Ceil( Sqrt(Gradients.Count) );
        H := W;
      end;
  end;
  
  W := Max(W, 1) * ThumbSize +1;
  H := Max(H, 1) * ThumbSize +1;
  
  if not (csDesigning in ComponentState) or (W > 0) and (H > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
    begin
      ANewWidth := W;
    end;
    
    if Align in [alNone, alTop, alBottom] then
    begin
      ANewHeight := H;
    end;
  end;  
end;

end.
