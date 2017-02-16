{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmShapes;

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

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, Graphics, Classes,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic Lib }
  gmTypes;

type
  // Shape Tool
  TgmShapeRegionTool = (srtNone,
                        srtMove,
                        srtRectangle,
                        srtRoundedRect,
                        srtEllipse,
                        srtPolygon,
                        srtLine,
                        srtCustom);
  // Combine Mode
  TgmRegionCombineMode = (rcmNone,
                          rcmAdd,
                          rcmSubtract,
                          rcmIntersect,
                          rcmExcludeOverlap);

//-- TgmShapeRegion ------------------------------------------------------------

  TgmShapeRegion = class(TObject)
  private
    FNewRGN     : hRGN;
    FCombinedRGN: hRGN;  // combined region by shapes
    FAccumRGN   : hRGN;
    FRegionColor: TColor;
    FBrushStyle : TBrushStyle;
  public
    constructor Create;
    destructor Destroy; override;

    // combine the new hRGN to the accumulate hRGN
    procedure CombineRGNToAccumRGN(const AMode: TgmRegionCombineMode);
    
    // show the Region
    procedure ShowRegion(const ADestBmp: TBitmap32);
    
    // offset the Region
    procedure Translate(const ATranslateVector: TPoint);

    property NewRGN     : hRGN        read FNewRGN      write FNewRGN;
    property AccumRGN   : hRGN        read FAccumRGN    write FAccumRGN;
    property RegionColor: TColor      read FRegionColor write FRegionColor;
    property BrushStyle : TBrushStyle read FBrushStyle  write FBrushStyle;
  end;

//-- TgmShapeOutline -----------------------------------------------------------

  TgmShapeOutline = class(TObject)
  private
    FStartPoint      : TPoint;
    FEndPoint        : TPoint;
    FStartPointBackup: TPoint;
    FEndPointBackup  : TPoint;
    FShapeRegionTool : TgmShapeRegionTool;
    FCombineMode     : TgmRegionCombineMode;
  public
    FPolygon      : array of TPoint;
    FPolygonBackup: array of TPoint;

    constructor Create; virtual;

    procedure AssignOutlineBasicData(const AOutline: TgmShapeOutline);
    procedure BackupCoordinates;
    procedure RestoreCoordinates;
    
    procedure ScaleCoordinates(const AOriginalRange, ACurrentRange: TRect;
      const AOriginalOffset, ACurrentOffset: TPoint);

    procedure Translate(const ATranslateVector: TPoint);
    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); virtual; abstract;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); virtual; abstract;
    procedure SaveToStream(const AStream: TStream); virtual; abstract;

    property StartPoint      : TPoint               read FStartPoint       write FStartPoint;
    property EndPoint        : TPoint               read FEndPoint         write FEndPoint;
    property StartPointBackup: TPoint               read FStartPointBackup write FStartPointBackup;
    property EndPointBackup  : TPoint               read FEndPointBackUp   write FEndPointBackup;
    property ShapeRegionTool : TgmShapeRegionTool   read FShapeRegionTool;
    property CombineMode     : TgmRegionCombineMode read FCombineMode      write FCombineMode;
  end;

//-- TgmRectangleOutline -------------------------------------------------------

  TgmRectangleOutline = class(TgmShapeOutline)
  public
    constructor Create; override;

    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); override;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmRoundRectOutline -------------------------------------------------------

  TgmRoundRectOutline = class(TgmShapeOutline)
  private
    FCornerRadius: Integer;
  public
    constructor Create; override;

    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); override;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); override;
    procedure SaveToStream(const AStream: TStream); override;

    property CornerRadius: Integer read FCornerRadius write FCornerRadius;
  end;

//-- TgmEllipseOutline ---------------------------------------------------------

  TgmEllipseOutline = class(TgmShapeOutline)
  public
    constructor Create; override;

    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); override;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmRegularPolygonOutline --------------------------------------------------

  TgmRegularPolygonOutline = class(TgmShapeOutline)
  private
    FSides: Integer;
  public
    constructor Create; override;

    procedure SetVertex;
    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); override;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); override;
    procedure SaveToStream(const AStream: TStream); override;

    property Sides: Integer read FSides write FSides;
  end;

//-- TgmLineOutline ------------------------------------------------------------

  TgmLineOutline = class(TgmShapeOutline)
  private
    FWeight: Integer;
  public
    constructor Create; override;

    procedure SetVertex;
    procedure DrawOutline(const ACanvas: TCanvas; const AOffsetVector: TPoint); override;
    procedure DrawOutlineInControlSpace(ACanvas: TCanvas; AImageControl: TCustomImage32); override;
    procedure SaveToStream(const AStream: TStream); override;

    property Weight: Integer read FWeight write FWeight;
  end;

//-- TgmOutlineList ------------------------------------------------------------

  TgmOutlineList = class(TList)
  private
    FTLBackup : TPoint;
    FBRBackup : TPoint;
    FStretched: Boolean;

    procedure DeleteAllOutlines;
  public
    FBoundaryTL: TPoint;
    FBoundaryBR: TPoint;

    constructor Create;
    destructor Destroy; override;

    procedure DuplicateOutlineList(const AList: TgmOutlineList);
    procedure DeleteOutlineByIndex(const AIndex: Integer);
    procedure BoundaryStandardizeOrder;
    procedure GetShapesBoundary;

    procedure DrawAllOutlines(const ACanvas: TCanvas;
      const AOffsetVector: TPoint; const APenMode: TPenMode); overload;

    procedure DrawAllOutlines(ACanvas: TCanvas; const APenMode: TPenMode;
      AImageControl: TCustomImage32); overload;

    procedure DrawShapesBoundary(const ACanvas: TCanvas;
      const AHandleRadius: Integer; const AOffsetVector: TPoint;
      const APenMode: TPenMode); overload;

    procedure DrawShapesBoundary(ACanvas: TCanvas; const AHandleRadius: Integer;
      const APenMode: TPenMode; AImageControl: TCustomImage32); overload;

    procedure DrawShapesBoundaryHandles(const ACanvas: TCanvas;
      const AHandleRadius: Integer; const AOffsetVector: TPoint;
      const APenMode: TPenMode); overload;

    procedure DrawShapesBoundaryHandles(ACanvas: TCanvas;
      const AHandleRadius: Integer; const APenMode: TPenMode;
      AImageControl: TCustomImage32); overload;

    function GetHandlePoint(const AX, AY, AHandleRadius: Integer): TgmDrawingHandle; overload;

    function GetHandlePoint(const AX, AY, AHandleRadius: Integer;
      AImageControl: TCustomImage32): TgmDrawingHandle; overload;

    function PointInBoundary(const AX, AY, AHandleRadius: Integer): Boolean;
    function GetScaledShapesRegion: hRGN;
    
    procedure Translate(const ATranslateVector: TPoint);
    procedure BackupCoordinates;
    procedure RestoreCoordinates;
    procedure ScaleShapesCoordinates;
    procedure SaveToStream(const AStream: TStream);

    property TLBackup   : TPoint  read FTLBackup  write FTLBackup;
    property BRBackup   : TPoint  read FBRBackup  write FBRBackup;
    property IsStretched: Boolean read FStretched write FStretched;
  end;

//-- TgmOutlineListReader ------------------------------------------------------

  TgmOutlineListReader = class(TObject)
  protected
    FOutlineList: TgmOutlineList;  // pointer to an outline list
  public
    constructor Create(const AOutlineList: TgmOutlineList);
    destructor Destroy; override;

    function LoadFromStream(const AStream: TStream): Boolean; virtual; abstract;
  end;

//-- TgmOutlineListReader1 -----------------------------------------------------

  TgmOutlineListReader1 = class(TgmOutlineListReader)
  private
    function GetEllipseOutline(const AStream: TStream): TgmShapeOutline;
    function GetLineOutline(const AStream: TStream): TgmShapeOutline;
    function GetRectangleOutline(const AStream: TStream): TgmShapeOutline;
    function GetRegularPolygonOutline(const AStream: TStream): TgmShapeOutline;
    function GetRoundRectOutline(const AStream: TStream): TgmShapeOutline;
  public
    function LoadFromStream(const AStream: TStream): Boolean; override;
  end;

  function GetRegionCombineModeString(const AMode: TgmRegionCombineMode): string;
  function CopyShapeOutline(const AOutline: TgmShapeOutline): TgmShapeOutline;

implementation

uses
{ Standard }
  Math,
{ externals }
  LineLibrary,
{ GraphicsMagic Lib }
  gmPaintFuncs, gmMath, gmCommonFuncs;

function GetRegionCombineModeString(const AMode: TgmRegionCombineMode): string;
begin
  case AMode of
    rcmNone:
      begin
        Result := '';
      end;
      
    rcmAdd:
      begin
        Result := 'Add';
      end;
      
    rcmSubtract:
      begin
        Result := 'Subtract';
      end;
      
    rcmIntersect:
      begin
        Result := 'Intersect';
      end;

    rcmExcludeOverlap:
      begin
        Result := 'Exclude Overlap';
      end;
  end;
end; 

function CopyShapeOutline(const AOutline: TgmShapeOutline): TgmShapeOutline;
var
  LOutline: TgmShapeOutline;
begin
  LOutline := nil;
  
  if Assigned(AOutline) then
  begin
    case AOutline.ShapeRegionTool of
      srtRectangle:
        begin
          LOutline := TgmRectangleOutline.Create;
        end;

      srtRoundedRect:
        begin
          LOutline := TgmRoundRectOutline.Create;
          TgmRoundRectOutline(LOutline).CornerRadius := TgmRoundRectOutline(AOutline).CornerRadius;
        end;

      srtEllipse:
        begin
          LOutline := TgmEllipseOutline.Create;
        end;

      srtPolygon:
        begin
          LOutline := TgmRegularPolygonOutline.Create;
          TgmRegularPolygonOutline(LOutline).Sides := TgmRegularPolygonOutline(AOutline).Sides;
        end;

      srtLine:
        begin
          LOutline := TgmLineOutline.Create;
          TgmLineOutline(LOutline).Weight := TgmLineOutline(AOutline).Weight;
        end;
    end;
    
    LOutline.AssignOutlineBasicData(AOutline);
  end;
  
  Result := LOutline;
end; 

//-- TgmShapeRegion ------------------------------------------------------------

constructor TgmShapeRegion.Create;
begin
  inherited Create;
  
  FRegionColor := clBlack;
  FBrushStyle  := bsSolid;
end;

destructor TgmShapeRegion.Destroy;
begin
  DeleteObject(FNewRGN);
  DeleteObject(FCombinedRGN);
  DeleteObject(FAccumRGN);
  
  inherited Destroy;
end;

// combine the new hRGN to the accumulate hRGN
procedure TgmShapeRegion.CombineRGNToAccumRGN(
  const AMode: TgmRegionCombineMode);
begin
  case AMode of
    rcmAdd:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_OR);
      end;

    rcmSubtract:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_DIFF);
      end;

    rcmIntersect:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_AND);
      end;

    rcmExcludeOverlap:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_XOR);
      end;
  end;
  
  FAccumRGN := FCombinedRGN;
end;

// show the Region
procedure TgmShapeRegion.ShowRegion(const ADestBmp: TBitmap32);
var
  LMaskBmp          : TBitmap32;
  i, r, g, b        : Integer;
  LColor            : TColor32;
  LDestBit, LMaskBit: PColor32;
begin
  ADestBmp.Clear($00000000);
  
  LColor   := Color32(FRegionColor);
  LMaskBmp := TBitmap32.Create;
  try
    LMaskBmp.SetSize(ADestBmp.Width, ADestBmp.Height);
    LMaskBmp.Clear(clWhite32);

    LMaskBmp.Canvas.Brush.Style := FBrushStyle;
    LMaskBmp.Canvas.Brush.Color := clBlack;
    
    PaintRGN(LMaskBmp.Canvas.Handle, FAccumRGN);

    LMaskBit := @LMaskBmp.Bits[0];
    LDestBit := @ADestBmp.Bits[0];
    
    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      r := LMaskBit^ shr 16 and $FF;
      g := LMaskBit^ shr  8 and $FF;
      b := LMaskBit^        and $FF;

      if (r = 0) and (g = 0) and (b = 0) then
      begin
        LDestBit^ := LColor;
      end;

      Inc(LMaskBit);
      Inc(LDestBit);
    end;
  finally
    LMaskBmp.Free;
  end;
end;

// offset the Region
procedure TgmShapeRegion.Translate(const ATranslateVector: TPoint);
begin
  OffsetRGN(FAccumRGN, ATranslateVector.X, ATranslateVector.Y);
end; 

//-- TgmShapeOutline -----------------------------------------------------------

constructor TgmShapeOutline.Create;
begin
  inherited Create;
  
  FStartPoint      := Point(0, 0);
  FEndPoint        := Point(0, 0);
  FShapeRegionTool := srtNone;
  FCombineMode     := rcmNone;
  
  SetLength(FPolygon, 0);

  FStartPointBackup := Point(0, 0);
  FEndPointBackup   := Point(0, 0);

  SetLength(FPolygonBackup, 0);
end;

procedure TgmShapeOutLine.AssignOutlineBasicData(
  const AOutline: TgmShapeOutline);
var
  i, LLowIndex, LHighIndex: Integer;
begin
  if Assigned(AOutline) then
  begin
    FStartPoint       := AOutline.StartPoint;
    FEndPoint         := AOutline.EndPoint;
    FStartPointBackup := AOutline.StartPointBackup;
    FEndPointBackup   := AOutline.EndPointBackup;
    FCombineMode      := AOutline.CombineMode;

    if High(AOutline.FPolygon) >= 0 then
    begin
      LLowIndex  := Low(AOutline.FPolygon);
      LHighIndex := High(AOutline.FPolygon);

      SetLength(FPolygon, LHighIndex + 1);

      for i := LLowIndex to LHighIndex do
      begin
        FPolygon[i] := AOutline.FPolygon[i];
      end;
    end
    else
    begin
      SetLength(FPolygon, 0);
      FPolygon := nil;
    end;

    if High(AOutline.FPolygonBackup) >= 0 then
    begin
      LLowIndex  := Low(AOutline.FPolygonBackup);
      LHighIndex := High(AOutline.FPolygonBackup);

      SetLength(FPolygonBackup, LHighIndex + 1);

      for i := LLowIndex to LHighIndex do
      begin
        FPolygonBackup[i] := AOutline.FPolygonBackup[i];
      end;
    end
    else
    begin
      SetLength(FPolygonBackup, 0);
      FPolygonBackup := nil;
    end;
  end;
end;

procedure TgmShapeOutLine.BackupCoordinates;
var
  i: Integer;
begin
  case FShapeRegionTool of
    srtRectangle,
    srtRoundedRect,
    srtEllipse:
      begin
        FStartPointBackup := FStartPoint;
        FEndPointBackup   := FEndPoint;
      end;

    srtPolygon,
    srtLine:
      begin
        if High(FPolygon) > (-1) then
        begin
          SetLength( FPolygonBackup, High(FPolygon) + 1 );
          
          for i := Low(FPolygon) to High(FPolygon) do
          begin
            FPolygonBackup[i] := FPolygon[i];
          end;
        end;
      end;
  end;
end;

procedure TgmShapeOutLine.RestoreCoordinates;
var
  i: Integer;
begin
  case FShapeRegionTool of
    srtRectangle,
    srtRoundedRect,
    srtEllipse:
      begin
        FStartPoint := FStartPointBackup;
        FEndPoint   := FEndPointBackup;
      end;
      
    srtPolygon,
    srtLine:
      begin
        if ( High(FPolygonBackup) > (-1) ) and
           ( High(FPolygonBackup) = High(FPolygon) )then
        begin
          for i := Low(FPolygonBackup) to High(FPolygonBackup) do
          begin
            FPolygon[i] := FPolygonBackup[i];
          end;
        end;
      end;
  end;
end; 

procedure TgmShapeOutline.ScaleCoordinates(
  const AOriginalRange, ACurrentRange: TRect;
  const AOriginalOffset, ACurrentOffset: TPoint);
var
  LOriginalW, LOriginalH: Integer;
  LCurrentW, LCurrentH  : Integer;
  LXScale, LYScale      : Double;
  i                     : Integer;
  LPoint                : TPoint;
begin
  LOriginalW := Abs(AOriginalRange.Right  - AOriginalRange.Left) + 1;
  LOriginalH := Abs(AOriginalRange.Bottom - AOriginalRange.Top)  + 1;
  LCurrentW  := Abs(ACurrentRange.Right   - ACurrentRange.Left)  + 1;
  LCurrentH  := Abs(ACurrentRange.Bottom  - ACurrentRange.Top)   + 1;

  case FShapeRegionTool of
    srtRectangle,
    srtRoundedRect,
    srtEllipse:
      begin
        LXScale       := (FStartPointBackup.X - AOriginalOffset.X) / LOriginalW;
        LYScale       := (FStartPointBackup.Y - AOriginalOffset.Y) / LOriginalH;

        FStartPoint.X := Round(LXScale * LCurrentW) + ACurrentOffset.X;
        FStartPoint.Y := Round(LYScale * LCurrentH) + ACurrentOffset.Y;

        LXScale       := (FEndPointBackup.X - AOriginalOffset.X) / LOriginalW;
        LYScale       := (FEndPointBackup.Y - AOriginalOffset.Y) / LOriginalH;

        FEndPoint.X   := Round(LXScale * LCurrentW) + ACurrentOffset.X;
        FEndPoint.Y   := Round(LYScale * LCurrentH) + ACurrentOffset.Y;
      end;
      
    srtPolygon,
    srtLine:
      begin
        if High(FPolygon) > (-1) then
        begin
          for i := Low(FPolygonBackup) to High(FPolygonBackup) do
          begin
            LPoint        := FPolygonBackup[i];
            LXScale       := (LPoint.X - AOriginalOffset.X) / LOriginalW;
            LYScale       := (LPoint.Y - AOriginalOffset.Y) / LOriginalH;
            FPolygon[i].X := Round(LXScale * LCurrentW + ACurrentOffset.X);
            FPolygon[i].Y := Round(LYScale * LCurrentH + ACurrentOffset.Y);
          end;
        end;
      end;
  end;
end;

procedure TgmShapeOutline.Translate(const ATranslateVector: TPoint);
var
  i: Integer;
begin
  case FShapeRegionTool of
    srtRectangle,
    srtRoundedRect,
    srtEllipse:
      begin
        FStartPoint       := AddPoints(FStartPoint,       ATranslateVector);
        FEndPoint         := AddPoints(FEndPoint,         ATranslateVector);
        FStartPointBackup := AddPoints(FStartPointBackup, ATranslateVector);
        FEndPointBackup   := AddPoints(FEndPointBackup,   ATranslateVector);
      end;
      
    srtPolygon, srtLine:
      begin
        if High(FPolygon) > (-1) then
        begin
          for i := Low(FPolygon) to High(FPolygon) do
          begin
            FPolygon[i]       := AddPoints(FPolygon[i],       ATranslateVector);
            FPolygonBackup[i] := AddPoints(FPolygonBackup[i], ATranslateVector);
          end;
        end;
      end;
  end;
end; 

//-- TgmRectangleOutline -------------------------------------------------------

constructor TgmRectangleOutline.Create;
begin
  inherited Create;
  FShapeRegionTool := srtRectangle;
end;

procedure TgmRectangleOutline.DrawOutline(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  LOffset: TPoint;
begin
  LOffset.X := Abs(AOffsetVector.X);
  LOffset.Y := Abs(AOffsetVector.Y);
  
  ACanvas.Rectangle(FStartPoint.X + LOffset.X, FStartPoint.Y + LOffset.Y,
                    FEndPoint.X   + LOffset.X, FEndPoint.Y   + LOffset.Y);
end;

procedure TgmRectangleOutline.DrawOutlineInControlSpace(ACanvas: TCanvas;
  AImageControl: TCustomImage32);
var
  tl, br: TPoint;
begin
  if Assigned(AImageControl) then
  begin
    tl := AImageControl.BitmapToControl(FStartPoint);
    br := AImageControl.BitmapToControl(FEndPoint);
  end
  else
  begin
    tl := FStartPoint;
    br := FEndPoint;
  end;
  
  ACanvas.Rectangle(tl.X, tl.Y, br.X, br.Y);
end;

procedure TgmRectangleOutline.SaveToStream(const AStream: TStream);
var
  LIntValue: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FShapeRegionTool);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FCombineMode);
    AStream.Write(LIntValue, 4);

    AStream.Write(FStartPoint.X,       4);
    AStream.Write(FStartPoint.Y,       4);
    AStream.Write(FEndPoint.X,         4);
    AStream.Write(FEndPoint.Y,         4);
    AStream.Write(FStartPointBackup.X, 4);
    AStream.Write(FStartPointBackup.Y, 4);
    AStream.Write(FEndPointBackup.X,   4);
    AStream.Write(FEndPointBackup.Y,   4);
  end;
end;

//-- TgmRoundRectOutline -------------------------------------------------------

constructor TgmRoundRectOutline.Create;
begin
  inherited Create;
  FShapeRegionTool := srtRoundedRect;
  FCornerRadius    := 30;
end;

procedure TgmRoundRectOutline.DrawOutline(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  LOffset: TPoint;
begin
  LOffset.X := Abs(AOffsetVector.X);
  LOffset.Y := Abs(AOffsetVector.Y);
  
  ACanvas.RoundRect(FStartPoint.X + LOffset.X, FStartPoint.Y + LOffset.Y,
                    FEndPoint.X   + LOffset.X, FEndPoint.Y   + LOffset.Y,
                    FCornerRadius, FCornerRadius);
end;

procedure TgmRoundRectOutline.DrawOutlineInControlSpace(ACanvas: TCanvas;
  AImageControl: TCustomImage32);
var
  tl, br: TPoint;
begin
  if Assigned(AImageControl) then
  begin
    tl := AImageControl.BitmapToControl(FStartPoint);
    br := AImageControl.BitmapToControl(FEndPoint);
  end
  else
  begin
    tl := FStartPoint;
    br := FEndPoint;
  end;
  
  ACanvas.RoundRect(tl.X , tl.Y, br.X, br.Y, FCornerRadius, FCornerRadius);
end;

procedure TgmRoundRectOutline.SaveToStream(const AStream: TStream);
var
  LIntValue: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FShapeRegionTool);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FCombineMode);
    AStream.Write(LIntValue, 4);

    AStream.Write(FStartPoint.X,       4);
    AStream.Write(FStartPoint.Y,       4);
    AStream.Write(FEndPoint.X,         4);
    AStream.Write(FEndPoint.Y,         4);
    AStream.Write(FStartPointBackup.X, 4);
    AStream.Write(FStartPointBackup.Y, 4);
    AStream.Write(FEndPointBackup.X,   4);
    AStream.Write(FEndPointBackup.Y,   4);

    AStream.Write(FCornerRadius, 4);
  end;
end;

//-- TgmEllipseOutline ---------------------------------------------------------

constructor TgmEllipseOutline.Create;
begin
  inherited Create;
  FShapeRegionTool := srtEllipse;
end;

procedure TgmEllipseOutline.DrawOutline(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  LOffset: TPoint;
begin
  LOffset.X := Abs(AOffsetVector.X);
  LOffset.Y := Abs(AOffsetVector.Y);
  
  ACanvas.Ellipse(FStartPoint.X + LOffset.X, FStartPoint.Y + LOffset.Y,
                  FEndPoint.X   + LOffset.X, FEndPoint.Y   + LOffset.Y);
end;

procedure TgmEllipseOutline.DrawOutlineInControlSpace(ACanvas: TCanvas;
  AImageControl: TCustomImage32);
var
  tl, br: TPoint;
begin
  if Assigned(AImageControl) then
  begin
    tl := AImageControl.BitmapToControl(FStartPoint);
    br := AImageControl.BitmapToControl(FEndPoint);
  end
  else
  begin
    tl := FStartPoint;
    br := FEndPoint;
  end;
  
  ACanvas.Ellipse(tl.X, tl.Y, br.X, br.Y);
end;

procedure TgmEllipseOutline.SaveToStream(const AStream: TStream);
var
  LIntValue: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FShapeRegionTool);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FCombineMode);
    AStream.Write(LIntValue, 4);

    AStream.Write(FStartPoint.X,       4);
    AStream.Write(FStartPoint.Y,       4);
    AStream.Write(FEndPoint.X,         4);
    AStream.Write(FEndPoint.Y,         4);
    AStream.Write(FStartPointBackup.X, 4);
    AStream.Write(FStartPointBackup.Y, 4);
    AStream.Write(FEndPointBackup.X,   4);
    AStream.Write(FEndPointBackup.Y,   4);
  end;
end;

//-- TgmRegularPolygonOutline --------------------------------------------------

constructor TgmRegularPolygonOutline.Create;
begin
  inherited Create;
  FShapeRegionTool := srtPolygon;
  FSides           := 0;
end;

procedure TgmRegularPolygonOutline.SetVertex;
begin
  SetLength(FPolygon, FSides + 1);
  CalcRegularPolygonVertices(FPolygon, FStartPoint, FEndPoint, FSides);
end; 

procedure TgmRegularPolygonOutline.DrawOutline(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  LPolygon: array of TPoint;
  i       : Integer;
  LOffset : TPoint;
begin
  LOffset.X := Abs(AOffsetVector.X);
  LOffset.Y := Abs(AOffsetVector.Y);
  
  SetLength( LPolygon, Length(FPolygon) );

  for i := Low(FPolygon) to High(FPolygon) do
  begin
    LPolygon[i] := AddPoints(FPolygon[i], LOffset);
  end;
  
  ACanvas.Polyline(LPolygon);
end;

procedure TgmRegularPolygonOutline.DrawOutlineInControlSpace(ACanvas: TCanvas;
  AImageControl: TCustomImage32);
var
  LPolygon: array of TPoint;
  i       : Integer;
begin
  if Assigned(AImageControl) then
  begin
    SetLength( LPolygon, Length(FPolygon) );
    try
      for i := Low(FPolygon) to High(FPolygon) do
      begin
        LPolygon[i] := AImageControl.BitmapToControl(FPolygon[i]);
      end;

      ACanvas.Polyline(LPolygon);
    finally
      SetLength(LPolygon, 0);
      LPolygon := nil;
    end;
  end
  else
  begin
    ACanvas.Polyline(FPolygon);
  end;
end;

procedure TgmRegularPolygonOutline.SaveToStream(const AStream: TStream);
var
  LIntValue, i: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FShapeRegionTool);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FCombineMode);
    AStream.Write(LIntValue, 4);

    // write the vertex count
    LIntValue := High(FPolygon) + 1;
    AStream.Write(LIntValue, 4);

    if LIntValue > 0 then
    begin
      for i := Low(FPolygon) to High(FPolygon) do
      begin
        AStream.Write(FPolygon[i].X, 4);
        AStream.Write(FPolygon[i].Y, 4);
      end;
    end;

    // write the number of backup vertices
    LIntValue := High(FPolygonBackup) + 1;
    AStream.Write(LIntValue, 4);

    if LIntValue > 0 then
    begin
      for i := Low(FPolygonBackup) to High(FPolygonBackup) do
      begin
        AStream.Write(FPolygonBackup[i].X, 4);
        AStream.Write(FPolygonBackup[i].Y, 4);
      end;
    end;
  end;
end;

//-- TgmLineOutline ------------------------------------------------------------

constructor TgmLineOutline.Create;
begin
  inherited Create;
  FShapeRegionTool := srtLine;
  FWeight          := 1;
end; 

procedure TgmLineOutline.SetVertex;
begin
  Setlength(FPolygon, 5);
  CalcLineOutlineVertices(FPolygon, FStartPoint, FEndPoint, FWeight);
end;

procedure TgmLineOutline.DrawOutline(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  LPolygon: array of TPoint;
  i       : Integer;
  LOffset : TPoint;
begin
  LOffset.X := Abs(AOffsetVector.X);
  LOffset.Y := Abs(AOffsetVector.Y);
  
  SetLength( LPolygon, Length(FPolygon) );

  for i := Low(FPolygon) to High(FPolygon) do
  begin
    LPolygon[i] := AddPoints(FPolygon[i], LOffset);
  end;
  
  ACanvas.Polyline(LPolygon);
end;

procedure TgmLineOutline.DrawOutlineInControlSpace(ACanvas: TCanvas;
  AImageControl: TCustomImage32);
var
  LPolygon: array of TPoint;
  i       : Integer;
begin
  if Assigned(AImageControl) then
  begin
    SetLength( LPolygon, Length(FPolygon) );
    try
      for i := Low(FPolygon) to High(FPolygon) do
      begin
        LPolygon[i] := AImageControl.BitmapToControl(FPolygon[i]);
      end;

      ACanvas.Polyline(LPolygon);
    finally
      SetLength(LPolygon, 0);
      LPolygon := nil;
    end;
  end
  else
  begin
    ACanvas.Polyline(FPolygon);
  end;
end;

procedure TgmLineOutline.SaveToStream(const AStream: TStream);
var
  LIntValue, i: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FShapeRegionTool);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FCombineMode);
    AStream.Write(LIntValue, 4);

    // write the vertex count
    LIntValue := High(FPolygon) + 1;
    AStream.Write(LIntValue, 4);

    if LIntValue > 0 then
    begin
      for i := Low(FPolygon) to High(FPolygon) do
      begin
        AStream.Write(FPolygon[i].X, 4);
        AStream.Write(FPolygon[i].Y, 4);
      end;
    end;

    // write the number of backup vertices
    LIntValue := High(FPolygonBackup) + 1;
    AStream.Write(LIntValue, 4);

    if LIntValue > 0 then
    begin
      for i := Low(FPolygonBackup) to High(FPolygonBackup) do
      begin
        AStream.Write(FPolygonBackup[i].X, 4);
        AStream.Write(FPolygonBackup[i].Y, 4);
      end;
    end;
  end;
end; 

//-- TgmOutlineList ------------------------------------------------------------

constructor TgmOutlineList.Create;
begin
  inherited Create;
  
  FBoundaryTL := Point(0, 0);
  FBoundaryBR := Point(0, 0);
  FTLBackup   := Point(0, 0);
  FBRBackup   := Point(0, 0);
  FStretched  := False;
end;

destructor TgmOutlineList.Destroy;
begin
  DeleteAllOutlines;
  
  inherited Destroy;
end;

procedure TgmOutlineList.DeleteAllOutlines;
var
  i       : Integer;
  LOutline: TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LOutline := TgmShapeOutline(Self.Items[i]);
      LOutline.Free;
    end;
    
    Self.Clear;
  end;
end;

procedure TgmOutlineList.DuplicateOutlineList(const AList: TgmOutlineList);
var
  i             : Integer;
  LTargetOutline: TgmShapeOutline;
  LNewOutline   : TgmShapeOutline;
begin
  if Assigned(AList) then
  begin
    FBoundaryTL := AList.FBoundaryTL;
    FBoundaryBR := AList.FBoundaryBR;
    FTLBackup   := AList.TLBackup;
    FBRBackup   := AList.BRBackup;
    FStretched  := AList.IsStretched;

    if AList.Count > 0 then
    begin
      DeleteAllOutlines;
      
      for i := 0 to (AList.Count - 1) do
      begin
        LTargetOutline := AList.Items[i];
        LNewOutline    := CopyShapeOutline(LTargetOutline);

        Self.Add(LNewOutline);
      end;
    end;
  end;
end;

procedure TgmOutlineList.DeleteOutlineByIndex(const AIndex: Integer);
var
  LOutline: TgmShapeOutLine;
begin
  if Self.Count > 0 then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      LOutline := TgmShapeOutline(Self.Items[AIndex]);

      LOutline.Free;
      Self.Delete(AIndex);
    end;
  end;
end;

procedure TgmOutlineList.BoundaryStandardizeOrder;
begin
  PointStandardizeOrder(FBoundaryTL, FBoundaryBR);
end;

procedure TgmOutLineList.GetShapesBoundary;
var
  i, j    : Integer;
  LOutline: TgmShapeOutLine;
begin
  if Self.Count > 0 then
  begin
    FBoundaryTL := Point(10000, 10000);
    FBoundaryBR := Point(-10000, -10000);

    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := TgmShapeOutline(Self.Items[i]);

      case LOutline.ShapeRegionTool of
        srtRectangle,
        srtRoundedRect,
        srtEllipse:
          begin
            FBoundaryTL.X := MinIntValue([FBoundaryTL.X, LOutline.StartPoint.X, LOutline.EndPoint.X]);
            FBoundaryTL.Y := MinIntValue([FBoundaryTL.Y, LOutline.StartPoint.Y, LOutline.EndPoint.Y]);
            FBoundaryBR.X := MaxIntValue([FBoundaryBR.X, LOutline.StartPoint.X, LOutline.EndPoint.X]);
            FBoundaryBR.Y := MaxIntValue([FBoundaryBR.Y, LOutline.StartPoint.Y, LOutline.EndPoint.Y]);
          end;
          
        srtPolygon,
        srtLine:
          begin
            if High(LOutline.FPolygon) > -1 then
            begin
              for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
              begin
                FBoundaryTL.X := MinIntValue([FBoundaryTL.X, LOutline.FPolygon[j].X]);
                FBoundaryTL.Y := MinIntValue([FBoundaryTL.Y, LOutline.FPolygon[j].Y]);
                FBoundaryBR.X := MaxIntValue([FBoundaryBR.X, LOutline.FPolygon[j].X]);
                FBoundaryBR.Y := MaxIntValue([FBoundaryBR.Y, LOutline.FPolygon[j].Y]);
              end;
            end;
          end;
      end;
    end;
  end;
end;

// draw outlines in bitmap space
procedure TgmOutlineList.DrawAllOutlines(const ACanvas: TCanvas;
  const AOffsetVector: TPoint; const APenMode: TPenMode);
var
  i              : Integer;
  LOutline       : TgmShapeOutline;
  LTempPenColor  : TColor;
  LTempPenWidth  : Integer;
  LTempPenStyle  : TPenStyle;
  LTempPenMode   : TPenMode;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psSolid;
    Pen.Mode    := APenMode;
    Brush.Color := clWhite;
    Brush.Style := bsClear;
  end;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := TgmShapeOutline(Self.Items[i]);
      LOutline.DrawOutline(ACanvas, AOffsetVector);
    end;
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

// draw outlines in image control space
procedure TgmOutlineList.DrawAllOutlines(ACanvas: TCanvas;
  const APenMode: TPenMode; AImageControl: TCustomImage32);
var
  i              : Integer;
  LOutline       : TgmShapeOutline;
  LTempPenColor  : TColor;
  LTempPenWidth  : Integer;
  LTempPenStyle  : TPenStyle;
  LTempPenMode   : TPenMode;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psSolid;
    Pen.Mode    := APenMode;
    Brush.Color := clWhite;
    Brush.Style := bsClear;
  end;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := TgmShapeOutline(Self.Items[i]);
      LOutline.DrawOutlineInControlSpace(ACanvas, AImageControl);
    end;
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

// The version of this procedure could drawing border bitmap space.
procedure TgmOutlineList.DrawShapesBoundary(const ACanvas: TCanvas;
  const AHandleRadius: Integer; const AOffsetVector: TPoint;
  const APenMode: TPenMode);
var
  LTempPenColor  : TColor;
  LTempPenWidth  : Integer;
  LTempPenStyle  : TPenStyle;
  LTempPenMode   : TPenMode;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
  LStartP, LEndP : TPoint;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
                      
  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psDot;
    Pen.Mode    := APenMode;
    Brush.Style := bsClear;
    LStartP.X   := FBoundaryTL.X - AHandleRadius + AOffsetVector.X;
    LStartP.Y   := FBoundaryTL.Y - AHandleRadius + AOffsetVector.Y;
    LEndP.X     := FBoundaryBR.X + AHandleRadius + AOffsetVector.X;
    LEndP.Y     := FBoundaryBR.Y + AHandleRadius + AOffsetVector.Y;
    
    Rectangle(LStartP.X, LStartP.Y, LEndP.X, LEndP.Y);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

// The version of this procedure could drawing border in image control space.
procedure TgmOutlineList.DrawShapesBoundary(ACanvas: TCanvas;
  const AHandleRadius: Integer; const APenMode: TPenMode;
  AImageControl: TCustomImage32);
var
  LTempPenColor  : TColor;
  LTempPenWidth  : Integer;
  LTempPenStyle  : TPenStyle;
  LTempPenMode   : TPenMode;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
  LStartP, LEndP : TPoint;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
                      
  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psDot;
    Pen.Mode    := APenMode;
    Brush.Style := bsClear;

    if Assigned(AImageControl) then
    begin
      LStartP := AImageControl.BitmapToControl(FBoundaryTL);
      LEndP   := AImageControl.BitmapToControl(FBoundaryBR);
    end
    else
    begin
      LStartP := FBoundaryTL;
      LEndP   := FBoundaryBR;
    end;

    LStartP.X := LStartP.X - AHandleRadius;
    LStartP.Y := LStartP.Y - AHandleRadius;
    LEndP.X   := LEndP.X   + AHandleRadius;
    LEndP.Y   := LEndP.Y   + AHandleRadius;

    Rectangle(LStartP.X, LStartP.Y, LEndP.X, LEndP.Y);
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

// The version of this procedure could drawing handles in bitmap space.
procedure TgmOutlineList.DrawShapesBoundaryHandles(const ACanvas: TCanvas;
  const AHandleRadius: Integer; const AOffsetVector: TPoint;
  const APenMode: TPenMode);
var
  TL, BR, TR, BL: TPoint;
  MT, MB, ML, MR: TPoint;
  HP            : TPoint;
  i             : Integer;
begin
  TL := Point(FBoundaryTL.X - AHandleRadius + AOffsetVector.X,
              FBoundaryTL.Y - AHandleRadius + AOffsetVector.Y);

  BR := Point(FBoundaryBR.X + AHandleRadius + AOffsetVector.X,
              FBoundaryBR.Y + AHandleRadius + AOffsetVector.Y);

  TR := Point(BR.X, TL.Y);
  BL := Point(TL.X, BR.Y);
  MT := Point( (TL.X + TR.X) div 2, TL.Y );
  MB := Point(MT.X, BR.Y);
  ML := Point( TL.X, (TL.Y + BL.Y) div 2 );
  MR := Point(BR.X, ML.Y);

  for i := 0 to 7 do
  begin
    case i of
      0:
        begin
          HP := TL;
        end;
        
      1:
        begin
          HP := BR;
        end;
        
      2:
        begin
          HP := TR;
        end;

      3:
        begin
          HP := BL;
        end;
        
      4:
        begin
          HP := MT;
        end;
        
      5:
        begin
          HP := MB;
        end;
        
      6:
        begin
          HP := ML;
        end;

      7:
        begin
          HP := MR;
        end;
    end;

    DrawHandle(ACanvas, HP, clBlack, clBlack, bsClear, APenMode, AHandleRadius);
  end;
end;

// The version of this procedure could drawing handles in image control space.
procedure TgmOutlineList.DrawShapesBoundaryHandles(ACanvas: TCanvas;
  const AHandleRadius: Integer; const APenMode: TPenMode;
  AImageControl: TCustomImage32);
var
  TL, BR, TR, BL: TPoint;
  MT, MB, ML, MR: TPoint;
  HP            : TPoint;
  i             : Integer;
begin
  if Assigned(AImageControl) then
  begin
    TL := AImageControl.BitmapToControl(FBoundaryTL);
    BR := AImageControl.BitmapToControl(FBoundaryBR);
  end
  else
  begin
    TL := FBoundaryTL;
    BR := FBoundaryBR;
  end;
  
  TL := Point(TL.X - AHandleRadius, TL.Y - AHandleRadius);
  BR := Point(BR.X + AHandleRadius, BR.Y + AHandleRadius);

  TR := Point(BR.X, TL.Y);
  BL := Point(TL.X, BR.Y);
  MT := Point( (TL.X + TR.X) div 2, TL.Y );
  MB := Point(MT.X, BR.Y);
  ML := Point( TL.X, (TL.Y + BL.Y) div 2 );
  MR := Point(BR.X, ML.Y);

  for i := 0 to 7 do
  begin
    case i of
      0:
        begin
          HP := TL;
        end;
        
      1:
        begin
          HP := BR;
        end;
        
      2:
        begin
          HP := TR;
        end;

      3:
        begin
          HP := BL;
        end;
        
      4:
        begin
          HP := MT;
        end;
        
      5:
        begin
          HP := MB;
        end;
        
      6:
        begin
          HP := ML;
        end;

      7:
        begin
          HP := MR;
        end;
    end;

    DrawHandle(ACanvas, HP, clBlack, clBlack, bsClear, APenMode, AHandleRadius);
  end;
end;

// which handle of the Shape Region control boundary the mouse is over
// parameter AX and AY should be in bitmap space
function TgmOutlineList.GetHandlePoint(
  const AX, AY, AHandleRadius: Integer): TgmDrawingHandle;
var
  TL, BR, TR, BL: TPoint;
  MT, MB, ML, MR: TPoint;
begin
  Result := dhNone;

  TL := Point(FBoundaryTL.X - AHandleRadius, FBoundaryTL.Y - AHandleRadius);
  BR := Point(FBoundaryBR.X + AHandleRadius, FBoundaryBR.Y + AHandleRadius);
  TR := Point(BR.X, TL.Y);
  BL := Point(TL.X, BR.Y);
  MT := Point( (TL.X + TR.X) div 2, TL.Y );
  MB := Point(MT.X, BR.Y);
  ML := Point( TL.X, (TL.Y + BL.Y) div 2 );
  MR := Point(BR.X, ML.Y);

  if SquareContainsPoint( TL, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhAXAY;
  end
  else
  if SquareContainsPoint( BR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBXBY;
  end
  else
  if SquareContainsPoint( TR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBXAY;
  end
  else
  if SquareContainsPoint( BL, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhAXBY;
  end
  else
  if SquareContainsPoint( MT, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAxBx;
  end
  else
  if SquareContainsPoint( MB, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAxBx;
  end
  else
  if SquareContainsPoint( ML, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhLeftHalfAyBy;
  end
  else
  if SquareContainsPoint( MR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhRightHalfAyBy;
  end;
end;

// which handle of the Shape Region control boundary the mouse is over
// parameter AX and AY should be in control space
function TgmOutlineList.GetHandlePoint(const AX, AY, AHandleRadius: Integer;
  AImageControl: TCustomImage32): TgmDrawingHandle;
var
  TL, BR, TR, BL: TPoint;
  MT, MB, ML, MR: TPoint;
begin
  Result := dhNone;

  if Assigned(AImageControl) then
  begin
    TL := AImageControl.BitmapToControl(FBoundaryTL);
    BR := AImageControl.BitmapToControl(FBoundaryBR);
  end
  else
  begin
    TL := FBoundaryTL;
    BR := FBoundaryBR;
  end;

  TL := Point(TL.X - AHandleRadius, TL.Y - AHandleRadius);
  BR := Point(BR.X + AHandleRadius, BR.Y + AHandleRadius);
  TR := Point(BR.X, TL.Y);
  BL := Point(TL.X, BR.Y);
  MT := Point( (TL.X + TR.X) div 2, TL.Y );
  MB := Point(MT.X, BR.Y);
  ML := Point( TL.X, (TL.Y + BL.Y) div 2 );
  MR := Point(BR.X, ML.Y);

  if SquareContainsPoint( TL, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhAXAY;
  end
  else
  if SquareContainsPoint( BR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBXBY;
  end
  else
  if SquareContainsPoint( TR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBXAY;
  end
  else
  if SquareContainsPoint( BL, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhAXBY;
  end
  else
  if SquareContainsPoint( MT, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAxBx;
  end
  else
  if SquareContainsPoint( MB, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAxBx;
  end
  else
  if SquareContainsPoint( ML, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhLeftHalfAyBy;
  end
  else
  if SquareContainsPoint( MR, AHandleRadius, Point(AX, AY) ) then
  begin
    Result := dhRightHalfAyBy;
  end;
end;

function TgmOutlineList.PointInBoundary(
  const AX, AY, AHandleRadius: Integer): Boolean;
var
  LRect: TRect;
begin
  LRect := Rect(FBoundaryTL.X - AHandleRadius, FBoundaryTL.Y - AHandleRadius,
                FBoundaryBR.X + AHandleRadius, FBoundaryBR.Y + AHandleRadius);
                
  Result := Windows.PtInRect( LRect, Point(AX, AY) );
end;

procedure TgmOutlineList.Translate(const ATranslateVector: TPoint);
var
  i       : Integer;
  LOutline: TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    FBoundaryTL := AddPoints(FBoundaryTL, ATranslateVector);
    FBoundaryBR := AddPoints(FBoundaryBR, ATranslateVector);
    FTLBackup   := AddPoints(FTLBackup,   ATranslateVector);
    FBRBackup   := AddPoints(FBRBackup,   ATranslateVector);

    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := Self.Items[i];
      LOutline.Translate(ATranslateVector);
    end;
  end;
end;

procedure TgmOutlineList.BackupCoordinates;
var
  i       : Integer;
  LOutline: TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    FTLBackup := FBoundaryTL;
    FBRBackup := FBoundaryBR;
    
    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := Self.Items[i];
      LOutline.BackupCoordinates;
    end;
  end;
end;

procedure TgmOutlineList.RestoreCoordinates;
var
  i       : Integer;
  LOutline: TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    FBoundaryTL := FTLBackup;
    FBoundaryBR := FBRBackup;
    
    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := Self.Items[i];
      LOutline.RestoreCoordinates;
    end;
  end;
end;

procedure TgmOutlineList.ScaleShapesCoordinates;
var
  LOriginalRect: TRect;
  LCurrentRect : TRect;
  i            : Integer;
  LOutline     : TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    FStretched    := True;
    LOriginalRect := Rect(FTLBackup.X,   FTLBackup.Y,   FBRBackup.X,   FBRBackup.Y);
    LCurrentRect  := Rect(FBoundaryTL.X, FBoundaryTL.Y, FBoundaryBR.X, FBoundaryBR.Y);

    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := Self.Items[i];
      LOutline.ScaleCoordinates(LOriginalRect, LCurrentRect, FTLBackup, FBoundaryTL);
    end;
  end;
end;

function TgmOutlineList.GetScaledShapesRegion: hRGN;
var
  i, j        : Integer;
  LOutline    : TgmShapeOutline;
  LPolygon    : array [0 .. 32767] of TPoint;
  LNewRGN     : hRGN;
  LCombinedRGN: hRGN;
  LAccumRGN   : hRGN;
begin
  LNewRGN      := 0;
  LCombinedRGN := 0;
  LAccumRGN    := 0;
  
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LOutline := Self.Items[i];

      case LOutline.ShapeRegionTool of
        srtRectangle:
          begin
            LNewRGN := CreateRectRGN(LOutline.StartPoint.X, LOutline.StartPoint.Y,
                                     LOutline.EndPoint.X, LOutline.EndPoint.Y);
          end;
          
        srtRoundedRect:
          begin
            LNewRGN := CreateRoundRectRGN(LOutline.StartPoint.X, LOutline.StartPoint.Y,
                                          LOutline.EndPoint.X, LOutline.EndPoint.Y,
                                          TgmRoundRectOutline(LOutline).CornerRadius,
                                          TgmRoundRectOutline(LOutline).CornerRadius);
          end;
          
        srtEllipse:
          begin
            LNewRGN := CreateEllipticRGN(LOutline.StartPoint.X, LOutline.StartPoint.Y,
                                         LOutline.EndPoint.X, LOutline.EndPoint.Y);
          end;
          
        srtPolygon, srtLine:
          begin
            if High(LOutline.FPolygon) > (-1) then
            begin
              for j := Low(LOutline.FPolygon) to High(LOutline.FPolygon) do
              begin
                LPolygon[j] := LOutline.FPolygon[j];
              end;

              LNewRGN := CreatePolygonRGN(LPolygon, High(LOutline.FPolygon) + 1, ALTERNATE);
            end;
          end;
      end;

      case LOutline.CombineMode of
        rcmAdd:
          begin
            LCombinedRGN := LNewRGN;
            CombineRGN(LCombinedRGN, LAccumRGN, LNewRGN, RGN_OR);
          end;

        rcmSubtract:
          begin
            LCombinedRGN := LNewRGN;
            CombineRGN(LCombinedRGN, LAccumRGN, LNewRGN, RGN_DIFF);
          end;

        rcmIntersect:
          begin
            LCombinedRGN := LNewRGN;
            CombineRGN(LCombinedRGN, LAccumRGN, LNewRGN, RGN_AND);
          end;

        rcmExcludeOverlap:
          begin
            LCombinedRGN := LNewRGN;
            CombineRGN(LCombinedRGN, LAccumRGN, LNewRGN, RGN_XOR);
          end;
      end;

      LAccumRGN := LCombinedRGN;
    end;
  end;
  
  Result := LAccumRGN;
end;

procedure TgmOutlineList.SaveToStream(const AStream: TStream);
var
  i       : Integer;
  LOutline: TgmShapeOutline;
begin
  if Self.Count > 0 then
  begin
    if Assigned(AStream) then
    begin
      // write number of outlines to stream
      AStream.Write(Self.Count, 4);

      AStream.Write(FBoundaryTL.X, 4);
      AStream.Write(FBoundaryTL.Y, 4);
      AStream.Write(FBoundaryBR.X, 4);
      AStream.Write(FBoundaryBR.Y, 4);
      AStream.Write(FTLBackup.X,   4);
      AStream.Write(FTLBackup.Y,   4);
      AStream.Write(FBRBackup.X,   4);
      AStream.Write(FBRBackup.Y,   4);
      AStream.Write(FStretched,    1);

      for i := 0 to (Self.Count - 1) do
      begin
        LOutline := TgmShapeOutline(Self.Items[i]);
        LOutline.SaveToStream(AStream);
      end;
    end;
  end;
end; 

//-- TgmOutlineListReader ------------------------------------------------------

constructor TgmOutlineListReader.Create(const AOutlineList: TgmOutlineList);
begin
  inherited Create;
  FOutlineList := AOutlineList;
end;

destructor TgmOutlineListReader.Destroy;
begin
  FOutlineList := nil;
  inherited Destroy;
end;

//-- TgmOutlineListReader1 -----------------------------------------------------

function TgmOutlineListReader1.GetEllipseOutline(
  const AStream: TStream): TgmShapeOutline;
var
  LIntValue: Integer;
begin
  Result := TgmEllipseOutline.Create;

  AStream.Read(LIntValue, 4);
  Result.FCombineMode := TgmRegionCombineMode(LIntValue);

  AStream.Read(Result.FStartPoint.X,       4);
  AStream.Read(Result.FStartPoint.Y,       4);
  AStream.Read(Result.FEndPoint.X,         4);
  AStream.Read(Result.FEndPoint.Y,         4);
  AStream.Read(Result.FStartPointBackup.X, 4);
  AStream.Read(Result.FStartPointBackup.Y, 4);
  AStream.Read(Result.FEndPointBackup.X,   4);
  AStream.Read(Result.FEndPointBackup.Y,   4);
end;

function TgmOutlineListReader1.GetLineOutline(
  const AStream: TStream): TgmShapeOutline;
var
  LIntValue, i: Integer;
begin
  Result := TgmLineOutline.Create;

  AStream.Read(LIntValue, 4);
  Result.FCombineMode := TgmRegionCombineMode(LIntValue);

  // read the vertex count
  AStream.Read(LIntValue, 4);

  if LIntValue > 0 then
  begin
    SetLength(Result.FPolygon, LIntValue);

    for i := Low(Result.FPolygon) to High(Result.FPolygon) do
    begin
      AStream.Read(Result.FPolygon[i].X, 4);
      AStream.Read(Result.FPolygon[i].Y, 4);
    end;
  end;

  // read the backup count of vertices
  AStream.Read(LIntValue, 4);

  if LIntValue > 0 then
  begin
    SetLength(Result.FPolygonBackup, LIntValue);

    for i := Low(Result.FPolygonBackup) to High(Result.FPolygonBackup) do
    begin
      AStream.Read(Result.FPolygonBackup[i].X, 4);
      AStream.Read(Result.FPolygonBackup[i].Y, 4);
    end;
  end;
end;

function TgmOutlineListReader1.GetRectangleOutline(
  const AStream: TStream): TgmShapeOutline;
var
  LIntValue: Integer;
begin
  Result := TgmRectangleOutline.Create;

  AStream.Read(LIntValue, 4);
  Result.FCombineMode := TgmRegionCombineMode(LIntValue);

  AStream.Read(Result.FStartPoint.X,       4);
  AStream.Read(Result.FStartPoint.Y,       4);
  AStream.Read(Result.FEndPoint.X,         4);
  AStream.Read(Result.FEndPoint.Y,         4);
  AStream.Read(Result.FStartPointBackup.X, 4);
  AStream.Read(Result.FStartPointBackup.Y, 4);
  AStream.Read(Result.FEndPointBackup.X,   4);
  AStream.Read(Result.FEndPointBackup.Y,   4);
end;

function TgmOutlineListReader1.GetRegularPolygonOutline(
  const AStream: TStream): TgmShapeOutline;
var
  LIntValue, i          : Integer;
  LRegularPolygonOutline: TgmRegularPolygonOutline;
begin
  Result := TgmRegularPolygonOutline.Create;

  AStream.Read(LIntValue, 4);
  Result.FCombineMode := TgmRegionCombineMode(LIntValue);

  // read the vertex count
  AStream.Read(LIntValue, 4);

  LRegularPolygonOutline       := TgmRegularPolygonOutline(Result);
  LRegularPolygonOutline.Sides := LIntValue - 1;

  if LIntValue > 0 then
  begin
    SetLength(Result.FPolygon, LIntValue);

    for i := Low(Result.FPolygon) to High(Result.FPolygon) do
    begin
      AStream.Read(Result.FPolygon[i].X, 4);
      AStream.Read(Result.FPolygon[i].Y, 4);
    end;
  end;

  // read the backup count of vertices
  AStream.Read(LIntValue, 4);

  if LIntValue > 0 then
  begin
    SetLength(Result.FPolygonBackup, LIntValue);

    for i := Low(Result.FPolygonBackup) to High(Result.FPolygonBackup) do
    begin
      AStream.Read(Result.FPolygonBackup[i].X, 4);
      AStream.Read(Result.FPolygonBackup[i].Y, 4);
    end;
  end;
end;

function TgmOutlineListReader1.GetRoundRectOutline(
  const AStream: TStream): TgmShapeOutline;
var
  LIntValue        : Integer;
  LRoundRectOutline: TgmRoundRectOutline;
begin
  Result := TgmRoundRectOutline.Create;

  AStream.Read(LIntValue, 4);
  Result.FCombineMode := TgmRegionCombineMode(LIntValue);

  AStream.Read(Result.FStartPoint.X,       4);
  AStream.Read(Result.FStartPoint.Y,       4);
  AStream.Read(Result.FEndPoint.X,         4);
  AStream.Read(Result.FEndPoint.Y,         4);
  AStream.Read(Result.FStartPointBackup.X, 4);
  AStream.Read(Result.FStartPointBackup.Y, 4);
  AStream.Read(Result.FEndPointBackup.X,   4);
  AStream.Read(Result.FEndPointBackup.Y,   4);

  // read corner radius
  LRoundRectOutline := TgmRoundRectOutline(Result);
  AStream.Read(LRoundRectOutline.FCornerRadius, 4);
end;

function TgmOutlineListReader1.LoadFromStream(const AStream: TStream): Boolean;
var
  LOutlineCount: Integer;
  LIntValue, i : Integer;
  LRegionTool  : TgmShapeRegionTool;
  LOutline     : TgmShapeOutline;
begin
  Result := False;

  LOutlineCount := 0;
  LIntValue     := -1;
  LOutline      := nil;

  if Assigned(AStream) and Assigned(FOutlineList) then
  begin
    FOutlineList.DeleteAllOutlines;

    AStream.Read(LOutlineCount, 4);
    AStream.Read(FOutlineList.FBoundaryTL.X, 4);
    AStream.Read(FOutlineList.FBoundaryTL.Y, 4);
    AStream.Read(FOutlineList.FBoundaryBR.X, 4);
    AStream.Read(FOutlineList.FBoundaryBR.Y, 4);
    AStream.Read(FOutlineList.FTLBackup.X,   4);
    AStream.Read(FOutlineList.FTLBackup.Y,   4);
    AStream.Read(FOutlineList.FBRBackup.X,   4);
    AStream.Read(FOutlineList.FBRBackup.Y,   4);
    AStream.Read(FOutlineList.FStretched,    1);

    if LOutlineCount > 0 then
    begin
      for i := 0 to (LOutlineCount - 1) do
      begin
        // read the type of the outline
        AStream.Read(LIntValue, 4);
        LRegionTool := TgmShapeRegionTool(LIntValue);

        case LRegionTool of
          srtRectangle:
            begin
              LOutline := GetRectangleOutline(AStream);
            end;

          srtRoundedRect:
            begin
              LOutline := GetRoundRectOutline(AStream);
            end;

          srtEllipse:
            begin
              LOutline := GetEllipseOutline(AStream);
            end;

          srtPolygon:
            begin
              LOutline := GetRegularPolygonOutline(AStream);
            end;

          srtLine:
            begin
              LOutline := GetLineOutline(AStream);
            end;
        end;

        if Assigned(LOutline) then
        begin
          FOutlineList.Add(LOutline);
        end;
      end;

      if FOutlineList.Count > 0 then
      begin
        Result := True;
      end;
    end;
  end;
end; 

end.
