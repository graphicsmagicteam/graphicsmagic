{ This unit is created at April Second 2007.

  Copyright (C) 2007 Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  This unit is translated from the resynth.cc . The original copyright info is
  in the following:

  ----------------------------------------------------------------------------

  The Resynthesizer - A GIMP plug-in for resynthesizing textures
  Copyright (C) 2000  Paul Francis Harrison
  Copyright (C) 2002  Laurent Despeyroux
  Copyright (C) 2002  David Rodríguez García

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  Update gimp image while preview by David Rodríguez García

  --------------------------------------------------------------------------

  This unit is also under the GNU license.

  Many thanks to Paul Francis Harrison for his helps. Without your help, we
  couldn't translated the C++ code of the plugin to Pascal. We deeply appreciate
  your helps. }

unit gmResynth;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows, SysUtils, Types, Math, Graphics, Classes, GR32;

const
  MAX_NEIGHBORS      = 1000;
  MAX_TRYS_PER_PIXEL = 10000;

type

//-- TgmCoordinates ------------------------------------------------------------

  TgmCoordinates = class(TObject)
  private
    FX: Integer;
    FY: Integer;

    function GetPoint: TPoint;
    procedure SetPoint(const APoint: TPoint);
  public
    constructor Create;

    property X  : Integer read FX       write FX;
    property Y  : Integer read FY       write FY;
    property Pnt: TPoint  read GetPoint write SetPoint;
  end;

//-- TgmCoordList --------------------------------------------------------------

  TgmCoordList = class(TList)
  private
    procedure DeleteCoordinates;
  public
    destructor Destroy; override;
  end;

//-- TgmStatus -----------------------------------------------------------------

  TgmStatusRec = record
    HasValue : Boolean;
    HasSource: Boolean;
    Source   : TPoint;
  end;

  TgmStatus = class(TObject)
  private
    FWidth      : Integer;
    FHeight     : Integer;
    FStatusArray: array of array of TgmStatusRec;

    procedure ClearArray;
    procedure SetSize(const w, h: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Width : Integer read FWidth;
    property Height: Integer read FHeight;
  end;

//-- TgmResynthesizer ----------------------------------------------------------

  TgmResynthesizer = class(TObject)
  private
    FDiffTable       : array [0..511] of Integer;
    FMapDiffTable    : array [0..511] of Integer;

    FInputBytes      : Integer;
    FMapBytes        : Integer;
    FMapPos          : Integer;
    FBytes           : Integer;

    FData            : TBitmap32;
    FDataMask        : array of array of Byte;
    FCorpus          : TBitmap32;
    FCorpusMask      : array of array of Byte;
    FStatus          : TgmStatus;
    FDataPoints      : TgmCoordList;
    FCorpusPoints    : TgmCoordList;
    FSortedOffsets   : TgmCoordList;

    FNeighbors       : array [0..MAX_NEIGHBORS - 1] of TPoint;
    FNeighborValues  : array [0..MAX_NEIGHBORS - 1] of TColor32;
    FNeighborStatuses: array [0..MAX_NEIGHBORS - 1] of TgmStatusRec;
    FNNeighbors      : Integer;
    FNeighborsCount  : Integer;
    FTrys            : Integer;

    FBest            : Integer;
    FBestPoint       : TPoint;

    FUseBorder       : Boolean;
    FHTile           : Boolean;
    FVTile           : Boolean;
    FMapWeight       : Double;
    FAutism          : Double;

    FOutputMsg       : string;  // output message, such as error, ect.

    procedure MakeOffsetList;
    procedure TryPoint(const APoint: TPoint);
    procedure FetchDataMask(const ADefaultMaskValue: Byte);
    procedure FetchCorpusMask(const ADefaultMaskValue: Byte);
    procedure ClearDataMask;
    procedure ClearCorpusMask;
    
    function WrapOrClip(const ABitmap: TBitmap32; var APoint: TPoint): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Run(const ADestBmp: TBitmap32): Boolean; // This is the main function.

    property Data       : TBitmap32 read FData;
    property Corpus     : TBitmap32 read FCorpus;
    property IsHorizTile: Boolean   read FHTile     write FHTile;
    property IsVertTile : Boolean   read FVTile     write FVTile;
    property IsUseBorder: Boolean   read FUseBorder write FUseBorder;
    property Autism     : Double    read FAutism    write FAutism;
    property OutputMsg  : string    read FOutputMsg;
  end;

implementation

uses
{ externals }
  LineLibrary;  // AddPoints(), SubtractPoints()

function NegLogCauchy(const X: Double): Double;
begin
  Result := Ln(X * X + 1.0);
end; 

function ComparePoints(Item1, Item2: Pointer): Integer;
var
  LPoint1: TPoint;
  LPoint2: TPoint;
  LValue1: Extended;
  LValue2: Extended;
begin
  LPoint1 := TgmCoordinates(Item1).Pnt;
  LPoint2 := TgmCoordinates(Item2).Pnt;
  LValue1 := LPoint1.Y * LPoint1.Y + LPoint1.X * LPoint1.X;
  LValue2 := LPoint2.Y * LPoint2.Y + LPoint2.X * LPoint2.X;

  if LValue1 < LValue2 then
  begin
    Result := -1;
  end
  else if LValue1 > LValue2 then
  begin
    Result := 1;
  end
  else
  begin
    Result := 0; // Value1 = Value2 
  end;
end;

//-- TgmCoordinates ------------------------------------------------------------

constructor TgmCoordinates.Create;
begin
  FX := 0;
  FY := 0;
end; 

function TgmCoordinates.GetPoint: TPoint;
begin
  Result.X := FX;
  Result.Y := FY;
end;

procedure TgmCoordinates.SetPoint(const APoint: TPoint);
begin
  if (FX <> APoint.X) or (FY <> APoint.Y) then
  begin
    FX := APoint.X;
    FY := APoint.Y;
  end;
end; 

//-- TgmCoordList ----------------------------------------------------------------

destructor TgmCoordList.Destroy;
begin
  DeleteCoordinates;
  inherited Destroy;
end; 

procedure TgmCoordList.DeleteCoordinates;
var
  i     : Integer;
  LCoord: TgmCoordinates;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LCoord := TgmCoordinates(Self.Items[i]);
      Self.Delete(i);

      if Assigned(LCoord) then
      begin
        FreeAndNil(LCoord);
      end;
    end;
  end;
end; 

//-- TgmStatus -----------------------------------------------------------------

constructor TgmStatus.Create;
begin
  inherited Create;
  
  FWidth       := 0;
  FHeight      := 0;
  FStatusArray := nil;
end;

destructor TgmStatus.Destroy;
begin
  ClearArray;
  
  inherited Destroy;
end;

procedure TgmStatus.ClearArray;
var
  i: Integer;
begin
  if High(FStatusArray) > 0 then
  begin
    for i := Low(FStatusArray) to High(FStatusArray) do
    begin
      if High(FStatusArray[i]) > 0 then
      begin
        SetLength(FStatusArray[i], 0);
        FStatusArray[i] := nil;
      end;
    end;

    SetLength(FStatusArray, 0);
    FStatusArray := nil;
  end;
end;

procedure TgmStatus.SetSize(const w, h: Integer);
var
  x, y: Integer;
begin
  FWidth  := w;
  FHeight := h;

  ClearArray;
  SetLength(FStatusArray, FHeight);

  for y := 0 to (h - 1) do
  begin
    SetLength(FStatusArray[y], FWidth);
  end;

  for y := 0 to (FHeight - 1) do
  begin
    for x := 0 to (FWidth - 1) do
    begin
      FStatusArray[y, x].HasValue  := False;
      FStatusArray[y, x].HasSource := False;
      FStatusArray[y, x].Source    := Point(-1, -1);
    end;
  end;
end; 

//-- TgmResynthesizer ----------------------------------------------------------

constructor TgmResynthesizer.Create;
begin
  inherited Create;

  FData          := TBitmap32.Create;
  FCorpus        := TBitmap32.Create;
  FStatus        := TgmStatus.Create;
  FDataPoints    := TgmCoordList.Create;
  FCorpusPoints  := TgmCoordList.Create;
  FSortedOffsets := TgmCoordList.Create;

  FUseBorder      := True;
  FHTile          := True;
  FVTile          := True;
  FMapWeight      := 0.5;
  FAutism         := 0.117; // 30/256
  FNeighborsCount := 30;
  FTrys           := 200;

  FOutputMsg := '';
end;

destructor TgmResynthesizer.Destroy;
begin
  FData.Free;
  FCorpus.Free;
  FStatus.Free;
  FDataPoints.Free;
  FCorpusPoints.Free;
  FSortedOffsets.Free;
  ClearDataMask;
  ClearCorpusMask;

  inherited Destroy;
end;

procedure TgmResynthesizer.MakeOffsetList;
var
  LWidth : Integer;
  LHeight: Integer;
  x, y   : Integer;
  LCoord : TgmCoordinates;
begin
  if FCorpus.Width < FData.Width then
  begin
    LWidth := FCorpus.Width;
  end
  else
  begin
    LWidth := FData.Width;
  end;

  if FCorpus.Height < FData.Height then
  begin
    LHeight := FCorpus.Height;
  end
  else
  begin
    LHeight := FData.Height;
  end;

  FSortedOffsets.DeleteCoordinates;

  for y := (-LHeight + 1) to (LHeight - 1) do
  begin
    for x := (-LWidth + 1) to (LWidth - 1) do
    begin
      LCoord   := TgmCoordinates.Create;
      LCoord.X := x;
      LCoord.Y := y;
      
      FSortedOffsets.Add(LCoord);
    end;
  end;

  FSortedOffsets.Sort(@ComparePoints);
end;

procedure TgmResynthesizer.TryPoint(const APoint: TPoint);
var
  i, LSum     : Integer;
  LOffPoint   : TPoint;
  LCorpusPixel: TColor32;
  LDataPixel  : TColor32;
begin
  LSum := 0;

  for i := 0 to (FNNeighbors - 1) do
  begin
    LOffPoint := AddPoints(APoint, FNeighbors[i]);

    if (LOffPoint.X < 0) or
       (LOffPoint.y < 0) or
       (LOffPoint.X >= FCorpus.Width) or
       (LOffPoint.Y >= FCorpus.Height) or
       (FCorpusMask[LOffPoint.Y, LOffPoint.X] = 0) then
    begin
      LSum := LSum + FDiffTable[0] * FInputBytes + FMapDiffTable[0] * FMapBytes;
    end
    else
    begin
      LCorpusPixel := FCorpus.PixelS[LOffPoint.X, LOffPoint.Y];
      LDataPixel   := FNeighborValues[i];

      if i > 0 then
      begin
        LSum := LSum + FDiffTable[256 + RedComponent(LDataPixel)   - RedComponent(LCorpusPixel)];
        LSum := LSum + FDiffTable[256 + GreenComponent(LDataPixel) - GreenComponent(LCorpusPixel)];
        LSum := LSum + FDiffTable[256 + BlueComponent(LDataPixel)  - BlueComponent(LCorpusPixel)];
        LSum := LSum + FDiffTable[256 + AlphaComponent(LDataPixel) - AlphaComponent(LCorpusPixel)];
      end;
    end;

    if LSum >= FBest then
    begin
      Exit;
    end;
  end;

  FBest      := LSum;
  FBestPoint := APoint;
end;

procedure TgmResynthesizer.FetchDataMask(const ADefaultMaskValue: Byte);
var
  x, y: Integer;
begin
  ClearDataMask;
  SetLength(FDataMask, FData.Height);

  for y := 0 to (FData.Height - 1) do
  begin
    SetLength(FDataMask[y], FData.Width);
  end;

  for y := 0 to (FData.Height - 1) do
  begin
    for x := 0 to (FData.Width - 1) do
    begin
      FDataMask[y, x] := ADefaultMaskValue;
    end;
  end;
end;

procedure TgmResynthesizer.FetchCorpusMask(const ADefaultMaskValue: Byte);
var
  x, y: Integer;
begin
  ClearDataMask;
  SetLength(FCorpusMask, FCorpus.Height);

  for y := 0 to (FCorpus.Height - 1) do
  begin
    SetLength(FCorpusMask[y], FCorpus.Width);
  end;

  for y := 0 to (FCorpus.Height - 1) do
  begin
    for x := 0 to (FCorpus.Width - 1) do
    begin
      FCorpusMask[y, x] := ADefaultMaskValue;
    end;
  end;
end;

procedure TgmResynthesizer.ClearDataMask;
var
  i: Integer;
begin
  if High(FDataMask) >= 0 then
  begin
    for i := High(FDataMask) downto Low(FDataMask) do
    begin
      SetLength(FDataMask[i], 0);
    end;
  end;

  SetLength(FDataMask, 0);
end; 

procedure TgmResynthesizer.ClearCorpusMask;
var
  i: Integer;
begin
  if High(FCorpusMask) >= 0 then
  begin
    for i := High(FCorpusMask) downto Low(FCorpusMask) do
    begin
      SetLength(FCorpusMask[i], 0);
    end;
  end;
  
  SetLength(FCorpusMask, 0);
end;

function TgmResynthesizer.WrapOrClip(const ABitmap: TBitmap32;
  var APoint: TPoint): Boolean;
begin
  while APoint.X < 0 do
  begin
    if FHTile then
    begin
      APoint.X := APoint.X + ABitmap.Width;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  while APoint.X >= ABitmap.Width do
  begin
    if FHTile then
    begin
      APoint.X := APoint.X - ABitmap.Width;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  while APoint.Y < 0 do
  begin
    if FVTile then
    begin
      APoint.Y := APoint.Y + ABitmap.Height;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  while APoint.Y >= ABitmap.Height do
  begin
    if FVTile then
    begin
      APoint.Y := APoint.Y - ABitmap.Height;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

// This is the main function.
function TgmResynthesizer.Run(const ADestBmp: TBitmap32): Boolean;
var
  LValue            : Double;
  x, y, i, j, n     : Integer;
  LCoord, C1, C2    : TgmCoordinates;
  LPosition, p      : TPoint;
  LSortedOffsetsSize: Integer;
  LIndex            : Integer;
  LTried            : array of array of Integer;
begin
  //---------------------------------------------
  // Data layout (actual image and map may vary)
  //
  //   Actual corpus  R
  //                  G
  //		      B
  //		      A
  //   Map            R
  //                  G
  //		      B
  //		      A
  //----------------------------------------------

  Result := False;

  FInputBytes  := 4;
  FMapBytes    := 0; //(with_map ? map_in_drawable->bpp : 0);
  FMapPos      := FInputBytes;
  FBytes       := FMapPos + FMapBytes;

  FetchDataMask(255);

  FStatus.SetSize(FData.Width, FData.Height);

  FDataPoints.DeleteCoordinates;

  for y := 0 to (FStatus.Height - 1) do
  begin
    for x := 0 to (FStatus.Width - 1) do
    begin
      FStatus.FStatusArray[y, x].HasSource := False;
      FStatus.FStatusArray[y, x].HasValue  := False;

      if FUseBorder and (FDataMask[y, x] = 0) then
      begin
        FStatus.FStatusArray[y, x].HasValue := True;
      end;

      if FDataMask[y, x] <> 0 then
      begin
        LCoord   := TgmCoordinates.Create;
        LCoord.X := x;
        LCoord.Y := y;

        FDataPoints.Add(LCoord);
      end;
    end;
  end;

  // Fetch the corpus

  FetchCorpusMask(0);

  FCorpusPoints.DeleteCoordinates;

  for y := 0 to (FCorpus.Height - 1) do
  begin
    for x := 0 to (FCorpus.Width - 1) do
    begin
      FCorpusMask[y, x] := 255 - FCorpusMask[y, x];
      
      if FCorpusMask[y, x] <> 0 then
      begin
        LCoord   := TgmCoordinates.Create;
        LCoord.X := x;
        LCoord.Y := y;

        FCorpusPoints.Add(LCoord);
      end;
    end;
  end;

  // Sanity check

  if (FCorpusPoints.Count = 0) or (FDataPoints.Count = 0) then
  begin
    if FCorpusPoints.Count = 0 then
    begin
      FOutputMsg := 'The input texture is too small.';
    end
    else
    begin
      FOutputMsg := 'The output image is too small.';
    end;

    Exit;
  end;

  // Setup 
  
  MakeOffsetList;
  
  for i := -256 to 255 do
  begin
    LValue                 := NegLogCauchy(i/256.0/FAutism) / NegLogCauchy(1.0/FAutism) * 65536.0;
    FDiffTable[256 + i]    := Trunc(LValue);
    FMapDiffTable[256 + i] := Trunc(i*i*FMapWeight*4.0);
  end;

  Randomize;
  for i := 0 to (FDataPoints.Count - 1) do
  begin
    j := Random(32767) mod FDataPoints.Count;

    C1 := TgmCoordinates(FDataPoints.Items[i]);
    C2 := TgmCoordinates(FDataPoints.Items[j]);

    P.X := C1.X;
    P.Y := C1.Y;

    C1.X := C2.X;
    C1.Y := C2.Y;
    C2.X := P.X;
    C2.Y := P.Y;
  end;

  n := FDataPoints.Count;
  while n >= 1 do
  begin
    n := Trunc(n*3/4); // <- note magic number... the more repetition, the higher the quality, maybe
    for i := 0 to (n - 1) do
    begin
      C1 := TgmCoordinates(FDataPoints.Items[i]);

      LCoord   := TgmCoordinates.Create;
      LCoord.X := C1.X;
      LCoord.Y := C1.Y;
      
      FDataPoints.Add(LCoord);
    end;
  end;

  // Do it
  SetLength(LTried, FCorpus.Height);

  for y := 0 to (FCorpus.Height - 1) do
  begin
    SetLength(LTried[y], FCorpus.Width);
  end;

  for y := 0 to (FCorpus.Height - 1) do
  begin
    for x := 0 to (FCorpus.Width - 1) do
    begin
      LTried[y, x] := -1;
    end;
  end;

  for i := (FDataPoints.Count - 1) downto 0 do
  begin
    LPosition := TgmCoordinates(FDataPoints.Items[i]).Pnt;

    FStatus.FStatusArray[LPosition.Y, LPosition.X].HasValue := True;

    FNNeighbors        := 0;
    LSortedOffsetsSize := FSortedOffsets.Count;

    for j := 0 to (LSortedOffsetsSize - 1) do
    begin
      P := AddPoints( LPosition, TgmCoordinates(FSortedOffsets.Items[j]).Pnt );

      if WrapOrClip(FData, P) and FStatus.FStatusArray[P.Y, P.X].HasValue then
      begin
        FNeighbors[FNNeighbors] := TgmCoordinates(FSortedOffsets.Items[j]).Pnt;

        FNeighborStatuses[FNNeighbors].HasValue  := FStatus.FStatusArray[P.Y, P.X].HasValue;
        FNeighborStatuses[FNNeighbors].HasSource := FStatus.FStatusArray[P.Y, P.X].HasSource;
        FNeighborStatuses[FNNeighbors].Source    := FStatus.FStatusArray[P.Y, P.X].Source;

        FNeighborValues[FNNeighbors] := FData.PixelS[P.X, P.Y];

        Inc(FNNeighbors);

        if FNNeighbors >= FNeighborsCount then
        begin
          Break;
        end;
      end;
    end;

    FBest := 1 shl 30;

    for j := 0 to (FNNeighbors - 1) do
    begin
      if FBest = 0 then
      begin
        Break;
      end;

      if FNeighborStatuses[j].HasSource then
      begin
        P := SubtractPoints(FNeighborStatuses[j].Source, FNeighbors[j]);

        if (P.X < 0) or
           (P.Y < 0) or
           (P.X >= FCorpus.Width) or
           (P.Y >= FCorpus.Height) then
        begin
          Continue;
        end;

        if (FCorpusMask[P.Y, P.X] = 0) or (LTried[P.Y, P.X] = i) then
        begin
          Continue;
        end;

        TryPoint(P);
        LTried[P.Y, P.X] := i;
      end;
    end;

    Randomize;
    for j := 0 to (FTrys - 1) do
    begin
      if FBest = 0 then
      begin
        Break;
      end;

      LIndex := Random(32767) mod FCorpusPoints.Count;
      P      := TgmCoordinates(FCorpusPoints.Items[LIndex]).Pnt;

      TryPoint(P);
    end;

    FData.PixelS[LPosition.X, LPosition.Y] := FCorpus.PixelS[FBestPoint.X, FBestPoint.Y];

    FStatus.FStatusArray[LPosition.Y, LPosition.X].HasSource := True;
    FStatus.FStatusArray[LPosition.Y, LPosition.X].Source    := FBestPoint;
  end;

  // ADestBmp.Assign(FData);
  ADestBmp.SetSize(FData.Width, FData.Height);
  ADestBmp.Draw(0, 0, FData);

  for y := 0 to (FCorpus.Height - 1) do
  begin
    SetLength(LTried[y], 0);
  end;

  SetLength(LTried, 0);

  Result := True;   
end; 

end.
