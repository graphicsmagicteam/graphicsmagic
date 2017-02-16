unit gmPenTools;

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

// Update Date: 2016/12/07

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Delphi }
  Windows, Classes, Graphics, Controls,
{ GraphicsMagic Lib }
  gmTypes;

const
  PATH_HANDLE_RADIUS: Integer = 3;

type
  TgmActivePoint = (apNone, apStart, apEnd);

  TgmPenTools = (ptPathComponentSelection,
                 ptDirectSelection,
                 ptPenTool,
                 ptAddAnchorPoint,
                 ptDeleteAnchorPoint,
                 ptConvertPoint);

  TgmPathSelectHandle = (pshNone,
                         pshStart,
                         pshControl1,
                         pshOpposite1,
                         pshControl2,
                         pshOpposite2,
                         pshEnd);

  // indicating the end-point of the curve is Anchor-Point or Corner-Point
  TgmEndingPointType = (eptNone, eptAnchorPoint, eptCornerPoint);

  TgmCurvePathListStatus = (cplsAddNewPath, cplsAddNextAnchorPoint, cplsAdjust);

  TgmConvertOperation = (coAnchorToCorner, coCornerToAnchor);

  TgmOppositeLineOperation = (oloChangeAngleOnly, oloAbsoluteOpposite);

  // indicating how to change the control handles -- paired or unpaired
  TgmPairState = (psUnknown, psPaired, psUnpaired);

  { TgmCurveSegment

    The whole curve path is consists of several curve segment.
    Class TgmCurveSegment is defined for one curve segment. }

  TgmCurveSegment = class(TObject)
  private
    FStartPoint          : TPoint;              // start-point of the curve
    FEndPoint            : TPoint;              // end-point of the curve
    FControl1            : TPoint;              // first control point of the curve
    FControl2            : TPoint;              // second control point of the curve
    FOpposite1           : TPoint;              // control point of the opposite extended line of the first control point
    FOpposite2           : TPoint;              // control point of the opposite extended line of the second control point

    FActive              : Boolean;             // indicating whether the curve is active
    FActivePoint         : TgmActivePoint;      // indicating the point that is currently processed
    FStartingPointType   : TgmEndingPointType;
    FEndingPointType     : TgmEndingPointType;  // type of ending-point

    FControl1Radian      : Double;              // radians of direction line 1
    FOpposite1Radian     : Double;              // radians of opposite direction line 1
    FControl2Radian      : Double;              // radians of direction line 2
    FOpposite2Radian     : Double;              // radians of opposite direction line 2
    FRadianDifference1   : Double;              // radians difference of direction line 1 and opposite direction line 1
    FRadianDifference2   : Double;              // radians difference of direction line 2 and opposite direction line 2
    FControl1Length      : Double;              // length of direction line 1
    FOpposite1Length     : Double;              // length of opposite direction line 1
    FControl2Length      : Double;              // length of direction line 2
    FOpposite2Length     : Double;              // length of opposite direction line 2
    FControl1PairState   : TgmPairState;        // indicating whether process both the direction line 1 and opposite direction line 1
    FControl2PairState   : TgmPairState;        // indicating whether process both the direction line 2 and opposite direction line 2
    FControl1PairChanged : Boolean;             // indicating whether the paired state of the direction line 1 and opposite direction line 1 is changed
    FControl2PairChanged : Boolean;             // indicating whether the paired state of the direction line 2 and opposite direction line 2 is changed
  public
    constructor Create;

    function GetCurveSegmentSelfBackup: TgmCurveSegment;  // duplicate itself

    function GetHandle(const AX, AY: Integer): TgmPathSelectHandle; overload;

    function GetHandle(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle; overload;

    procedure DrawCurveSegment(const ACanvas: TCanvas;
      const AOffsetVector: TPoint); overload;

    procedure DrawCurveSegment(ACanvas: TCanvas;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure Translate(const ATranslateVector: TPoint);
    procedure CopyCurveSegment(const ASource: TgmCurveSegment);

    procedure ScaleCurveSegment(
      const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);

    procedure SaveToStream(const AMemoryStream: TMemoryStream);

    property StartPoint           : TPoint             read FStartPoint          write FStartPoint;
    property EndPoint             : TPoint             read FEndPoint            write FEndPoint;
    property Control1             : TPoint             read FControl1            write FControl1;
    property Control2             : TPoint             read FControl2            write FControl2;
    property Opposite1            : TPoint             read FOpposite1           write FOpposite1;
    property Opposite2            : TPoint             read FOpposite2           write FOpposite2;
    property IsActive             : Boolean            read FActive              write FActive;
    property ActivePoint          : TgmActivePoint     read FActivePoint         write FActivePoint;
    property StartingPointType    : TgmEndingPointType read FStartingPointType   write FStartingPointType;
    property EndingPointType      : TgmEndingPointType read FEndingPointType     write FEndingPointType;
    property Control1Radian       : Double             read FControl1Radian      write FControl1Radian;
    property Opposite1Radian      : Double             read FOpposite1Radian     write FOpposite1Radian;
    property Control2Radian       : Double             read FControl2Radian      write FControl2Radian;
    property Opposite2Radian      : Double             read FOpposite2Radian     write FOpposite2Radian;
    property RadianDifference1    : Double             read FRadianDifference1   write FRadianDifference1;
    property RadianDifference2    : Double             read FRadianDifference2   write FRadianDifference2;
    property Control1Length       : Double             read FControl1Length      write FControl1Length;
    property Opposite1Length      : Double             read FOpposite1Length     write FOpposite1Length;
    property Control2Length       : Double             read FControl2Length      write FControl2Length;
    property Opposite2Length      : Double             read FOpposite2Length     write FOpposite2Length;
    property Control1PairState    : TgmPairState       read FControl1PairState   write FControl1PairState;
    property Control2PairState    : TgmPairState       read FControl2PairState   write FControl2PairState;
    property IsControl1PairChanged: Boolean            read FControl1PairChanged write FControl1PairChanged;
    property IsControl2PairChanged: Boolean            read FControl2PairChanged write FControl2PairChanged;
  end;

  { TgmCurveSegmentList

    Class TgmCurveSegmentsList is for holding curve segments, use these
    curve segments to build a whole curve path. }

  TgmCurveSegmentsList = class(TList)
  private
    FCurrentSegment : TgmCurveSegment;
    FNextSegment    : TgmCurveSegment;
    FCurrentIndex   : Integer;
    FSelectedCount  : Integer;
    FClosed         : Boolean;
    FSelected       : Boolean;        // if there is any selected curve segment
    FSelectedAll    : Boolean;        // if all the curve segments are selected
    FFirstSegmentOK : Boolean;

    procedure DrawCurveEndingHandles(const ACanvas: TCanvas;
      const APenMode: TPenMode); overload;

    procedure DrawCurveEndingHandles(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure DrawCurveDirectionHandles(const ACanvas: TCanvas;
      const APenMode: TPenMode); overload;

    procedure DrawCurveDirectionHandles(ACanvas: TCanvas;
      const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure DeleteAllCurveSegments;
    procedure DeleteCurveSegmentByIndex(const AIndex: Integer);

    function SelectAtSingleSegment(const AX, AY: Integer): Boolean; overload;
    
    function SelectAtSingleSegment(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;
    
    // function SelectAtWholePath(const AX, AY: Integer): Boolean;
    
    function SelectAtHandles(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle): Boolean; overload;

    function SelectAtHandles(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;

    function SelectAtEndPoints(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle): Boolean; overload;
      
    function SelectAtEndPoints(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;

    // Assign data from another one.
    procedure AssignCurveSegmentListData(const ASegmentList: TgmCurveSegmentsList);

    procedure AddFirstSegment(const P1, P2, P3, P4: TPoint);
    procedure AddFollowSegment(const P1, P2: TPoint);
    procedure DeactivateAllSegments;

    function AddAnchorPointOnSegment(const AX, AY: Integer): Boolean; overload;

    function AddAnchorPointOnSegment(const AX, AY: Integer;
      ACoordConvertFuncForTesting: TgmPointCoordConvertFunc;
      ACoordConvertFuncForAdding: TgmPointCoordConvertFunc): Boolean; overload;

    function DeleteAnchorPointOnSegment(const AX, AY: Integer): Boolean; overload;

    function DeleteAnchorPointOnSegment(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;

    procedure DrawCurveSegments(const ACanvas: TCanvas; const APenMode: TPenMode;
      const AOffsetVector: TPoint); overload;

    procedure DrawCurveSegments(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure DrawCurveDirectionLines(const ACanvas: TCanvas;
      const APenMode: TPenMode); overload;

    procedure DrawCurveDirectionLines(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure DrawCurveHandles(const ACanvas: TCanvas;
      const APenMode: TPenMode); overload;
      
    procedure DrawCurveHandles(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    function NilsIfPointOnBezier(const AX, AY: Integer): Boolean; overload;
    
    function NilsIfPointOnBezier(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;

    // determine whether the mouse is point on the start point of the first curve segment
    function PointOnFirstSegmentStart(const AX, AY: Integer): Boolean; overload;
    
    function PointOnFirstSegmentStart(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;

    // Determine whether the mouse is point on the end point of the last curve
    // segment, is so, return the last curve segment, otherwise return nil. 
    function PointOnLastSegmentEndingPoint(const AX, AY: Integer): TgmCurveSegment; overload;

    function PointOnLastSegmentEndingPoint(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmCurveSegment; overload;
    
    // determine whether the mouse is point on a handle of direction line of a selected curve segment
    function GetSelectedDirectionHandle(const AX, AY: Integer): TgmPathSelectHandle; overload;

    function GetSelectedDirectionHandle(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle; overload;

    // determine whether the mouse is point on a end point of a curve segment, not a control point
    function GetSelectedEndingHandle(const AX, AY: Integer): TgmPathSelectHandle; overload;

    function GetSelectedEndingHandle(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle; overload;

    // determine whether the mouse is point on any handles
    function GetSelectHandle(const AX, AY: Integer): TgmPathSelectHandle; overload;
    
    function GetSelectHandle(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle; overload;

    function GetEndingPointType(const AX, AY: Integer): TgmEndingPointType;
    function GetSelectedSegmentsPairState: TgmPairState;
    function GetCurrentSegmentIndexInList: Integer;
    function GetNextSegmentIndexInList: Integer;

    procedure ChangeSelectedHandlePosition(const AHandle: TgmPathSelectHandle;
      const ATranslateVector: TPoint; const AOperation: TgmOppositeLineOperation);

    procedure ChangeDirectionLinesPairState(const APairState: TgmPairState;
      const AIsSaveSetting: Boolean);

    // switch between Anchor Point and Corner Point
    procedure ConvertPoint(const AX, AY: Integer;
      const AOperation: TgmConvertOperation); overload;

    procedure ConvertPoint(const AX, AY: Integer;
      const AOperation: TgmConvertOperation;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure ClosePath;
    procedure TranslateAllSegments(const ATranslateVector: TPoint);
    procedure ScaleAllSegments(const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);
    procedure MakeRegion(const ACanvas: TCanvas; const AOffsetVector: TPoint);

    procedure StrokePathWithForegroundColor(const ACanvas: TCanvas;
      const APenWidth: Integer; const APenColor, ABrushColor: TColor;
      const APenStyle: TPenStyle; const ABrushStyle: TBrushStyle;
      const AOffsetVector: TPoint);

    procedure FillPathWithForegroundColor(const ACanvas: TCanvas;
      const AColor: TColor; const ABrushStyle: TBrushStyle;
      const AOffsetVector: TPoint);

    // calculate the radians and length according to the curve segments selected state
    procedure CalcRadLenForCurveSegments;

    // whether the pair state is changed
    function IsPairStateChanged: Boolean;

    function LoadCurveSegmentsFromStream(const AMemoryStream: TMemoryStream;
      const AVersionNum: Integer): Boolean;

    procedure SaveCurveSegmentsToStream(const AMemoryStream: TMemoryStream);

    property CurrentSegment   : TgmCurveSegment read FCurrentSegment;
    property CurrentIndex     : Integer         read FCurrentIndex;
    property SelectedCount    : Integer         read FSelectedCount;
    property IsClosed         : Boolean         read FClosed;
    property IsSelected       : Boolean         read FSelected       write FSelected;
    property IsSelectedAll    : Boolean         read FSelectedAll    write FSelectedAll;
    property IsFirstSegmentOK : Boolean         read FFirstSegmentOK write FFirstSegmentOK;
  end;

  { TgmCurvePath

    Class TgmCurvePath includes a curve segment list, encapsulate these
    curve segments as a curve path object. }

  TgmCurvePath = class(TObject)
  private
    FCurveSegmentsList : TgmCurveSegmentsList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSelfBackup: TgmCurvePath;

    property CurveSegmentsList : TgmCurveSegmentsList read FCurveSegmentsList;
  end;

  { TgmCurvePathList
  
    Class TgmCurvePathList is used to hold TgmCurvePath objects,
    each TgmCurvePath object is a whole curve path. }

  TgmCurvePathList = class(TList)
  private
    FSelectedPath      : TgmCurvePath;
    FSelectedPathIndex : Integer;
    FStatus            : TgmCurvePathListStatus;
    FPathRegion        : HRGN;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignCurvePathListData(const ACurvePathList: TgmCurvePathList);
    procedure AddNewPathToList(const APath: TgmCurvePath);
    procedure DeleteAllCurvePaths;
    procedure DeselectAllPaths;

    procedure DrawAllPathRegionsForThumbnail(const ACanvas: TCanvas;
      const AOffsetVector: TPoint);

    procedure DrawAllPathsForThumbnail(const ACanvas: TCanvas;
      const AOffsetVector: TPoint);

    procedure DrawAllPaths(const ACanvas: TCanvas; const APenMode: TPenMode;
      const AOffsetVector: TPoint); overload;

    procedure DrawAllPaths(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc); overload;

    procedure TranslateAllSelectedPaths(const ATranslateVector: TPoint);
    procedure TranslateAllPaths(const ATranslateVector: TPoint);
    
    procedure ScaleAllPaths(const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);

    procedure SelectWholePathByIndex(const AIndex: Integer);
    procedure SelectPathByIndex(const AIndex: Integer);

    procedure StrokeSelectedPaths(const ACanvas: TCanvas; const APenWidth: Integer;
      const APenColor, ABrushColor: TColor; const APenStyle: TPenStyle;
      const ABrushStyle: TBrushStyle; const AOffsetVector: TPoint);

    procedure FillSelectedPaths(const ACanvas: TCanvas; const AColor: TColor;
      const ABrushStyle: TBrushStyle; const AOffsetVector: TPoint);

    // get a cursor according to a selected path and path tools
    function GetCursor(const AX, AY: Integer; const APenTool: TgmPenTools): TCursor; overload;
    
    function GetCursor(const AX, AY: Integer; APenTool: TgmPenTools;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TCursor; overload;

    function GetWholeSelectedPathsCount: Integer;

    // get the number of selected path, including both the partially and fully selected paths
    function GetSelectedPathsCount: Integer;

    // Determine whether the mouse is point on a path, used to select a whole
    // path. The return value is the index of selected path in the list. 
    function IfPointOnPathsForWholePath(const AX, AY: Integer): Integer; overload;

    function IfPointOnPathsForWholePath(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Integer; overload;

    function IfPointOnPaths(const AX, AY: Integer): Boolean; overload;

    function IfPointOnPaths(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;

    function IfPointOnSelectedPathHandles(const AX, AY: Integer): Boolean; overload;

    function IfPointOnSelectedPathHandles(const AX, AY: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean; overload;
      
    function IfSelectedPathByIndex(const AIndex: Integer): Boolean;
    function IfSelectedWholePathByIndex(const AIndex: Integer): Boolean;
    
    function SelectPath(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle): TgmCurvePath; overload;

    function SelectPath(const AX, AY: Integer;
      var ASelectedHandle: TgmPathSelectHandle;
      ACoordConvertFunc: TgmPointCoordConvertFunc): TgmCurvePath; overload;

    function LoadCurvePathsFromStream(const AMemoryStream: TMemoryStream;
      const AVersionNum: Integer): Boolean;

    procedure SaveCurvePathsToStream(const AMemoryStream: TMemoryStream);

    function UpdatePathRegion(const ACanvasWidth, ACanvasHeight: Integer;
      const AOffsetVector: TPoint): Boolean;

    property PathRegion        : HRGN                   read FPathRegion;
    property SelectedPath      : TgmCurvePath           read FSelectedPath      write FSelectedPath;
    property SelectedPathIndex : Integer                read FSelectedPathIndex write FSelectedPathIndex;
    property Status            : TgmCurvePathListStatus read FStatus            write FStatus;
  end;


{ Common Methods }

function GetCurvePathCursor(const ASelectHandle: TgmPathSelectHandle): TCursor;


implementation

uses
{ externals }
  LineLibrary,         // efg2's AddPoints(), SubtractPoints()
{ GraphicsMagic Lib }
  NilsHaeckBeziers,
  gmGUIFuncs,          // Custom Cursors
  gmMath,
  gmPaintFuncs,        // DrawHandle()...
  gmCommonFuncs;

type

 { TgmCurveSegmentReader }

  TgmCurveSegmentReader = class(TObject)
  public
    function GetCurveSegmentFromStream(
      const AMemoryStream: TMemoryStream): TgmCurveSegment; virtual; abstract;
  end;

  { TgmCurveSegmentReader1 }

  TgmCurveSegmentReader1 = class(TgmCurveSegmentReader)
  public
    function GetCurveSegmentFromStream(
      const AMemoryStream: TMemoryStream): TgmCurveSegment; override;
  end;

const
  TOLERANCE_OVER_BEZIER: Double = 1;

{ Common Methods }

function GetCurvePathCursor(const ASelectHandle: TgmPathSelectHandle): TCursor;
begin
  if ASelectHandle <> pshNone then
  begin
    Result := crMovePath;
  end
  else
  begin
    Result := crDefault;
  end;
end; 

{ TgmCurveSegment }
  
constructor TgmCurveSegment.Create;
begin
  inherited Create;
  
  FStartPoint          := Point(0, 0);
  FEndPoint            := Point(0, 0);
  FControl1            := Point(0, 0);
  FControl2            := Point(0, 0);
  FOpposite1           := Point(0, 0);
  FOpposite2           := Point(0, 0);
  FActive              := False;
  FActivePoint         := apStart;
  FEndingPointType     := eptAnchorPoint;
  FStartingPointType   := eptAnchorPoint;
  FControl1Radian      := 0;
  FOpposite1Radian     := 0;
  FControl2Radian      := 0;
  FOpposite2Radian     := 0;
  FRadianDifference1   := 0;
  FRadianDifference2   := 0;
  FControl1Length      := 0;
  FOpposite1Length     := 0;
  FControl2Length      := 0;
  FOpposite2Length     := 0;
  FControl1PairState   := psPaired;
  FControl2PairState   := psPaired;
  FControl1PairChanged := False;
  FControl2PairChanged := False;
end;

// duplicate itself
function TgmCurveSegment.GetCurveSegmentSelfBackup: TgmCurveSegment;
begin
  Result                       := TgmCurveSegment.Create;
  Result.StartPoint            := FStartPoint;
  Result.EndPoint              := FEndPoint;
  Result.Control1              := FControl1;
  Result.Control2              := FControl2;
  Result.Opposite1             := FOpposite1;
  Result.Opposite2             := FOpposite2;
  Result.IsActive              := FActive;
  Result.ActivePoint           := FActivePoint;
  Result.StartingPointType     := FStartingPointType;
  Result.EndingPointType       := FEndingPointType;
  Result.Control1Radian        := FControl1Radian;
  Result.Opposite1Radian       := FOpposite1Radian;
  Result.Control2Radian        := FControl2Radian;
  Result.Opposite2Radian       := FOpposite2Radian;
  Result.RadianDifference1     := FRadianDifference1;
  Result.RadianDifference2     := FRadianDifference2;
  Result.Control1Length        := FControl1Length;
  Result.Opposite1Length       := FOpposite1Length;
  Result.Control2Length        := FControl2Length;
  Result.Opposite2Length       := FOpposite2Length;
  Result.Control1PairState     := FControl1PairState;
  Result.Control2PairState     := FControl2PairState;
  Result.IsControl1PairChanged := FControl1PairChanged;
  Result.IsControl2PairChanged := FControl2PairChanged;
end;

procedure TgmCurveSegment.DrawCurveSegment(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  p1, p2, p3, p4 : TPoint;
begin
  p1 := AddPoints(FStartPoint, AOffsetVector);
  p2 := AddPoints(FControl1,   AOffsetVector);
  p3 := AddPoints(FControl2,   AOffsetVector);
  p4 := AddPoints(FEndPoint,   AOffsetVector);
  
  ACanvas.PolyBezier([p1, p2, p3, p4]);
end;

procedure TgmCurveSegment.DrawCurveSegment(ACanvas: TCanvas;
  ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  p1, p2, p3, p4 : TPoint;
begin
  if Assigned(ACoordConvertFunc) then
  begin
    p1 := ACoordConvertFunc(FStartPoint);
    p2 := ACoordConvertFunc(FControl1);
    p3 := ACoordConvertFunc(FControl2);
    p4 := ACoordConvertFunc(FEndPoint);
  end
  else
  begin
    p1 := FStartPoint;
    p2 := FControl1;
    p3 := FControl2;
    p4 := FEndPoint;
  end;

  ACanvas.PolyBezier([p1, p2, p3, p4]);
end;

procedure TgmCurveSegment.Translate(const ATranslateVector: TPoint);
begin
  FStartPoint := AddPoints(FStartPoint, ATranslateVector);
  FControl1   := AddPoints(FControl1,   ATranslateVector);
  FOpposite1  := AddPoints(FOpposite1,  ATranslateVector);
  FControl2   := AddPoints(FControl2,   ATranslateVector);
  FOpposite2  := AddPoints(FOpposite2,  ATranslateVector);
  FEndPoint   := AddPoints(FEndPoint,   ATranslateVector);
end;

function TgmCurveSegment.GetHandle(const AX, AY: Integer): TgmPathSelectHandle;
begin
  if SquareContainsPoint( FStartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshStart;
  end
  else
  if SquareContainsPoint( FEndPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshEnd;
  end
  else
  if SquareContainsPoint( FControl1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshControl1;
  end
  else
  if SquareContainsPoint( FOpposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshOpposite1;
  end
  else
  if SquareContainsPoint( FControl2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshControl2;
  end
  else
  if SquareContainsPoint( FOpposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshOpposite2;
  end
  else
  begin
    Result := pshNone;
  end;
end;

function TgmCurveSegment.GetHandle(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle;
var
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
  LControl1   : TPoint;
  LControl2   : TPoint;
  LOpposite1  : TPoint;
  LOpposite2  : TPoint;
begin
  if Assigned(ACoordConvertFunc) then
  begin
    LStartPoint := ACoordConvertFunc(FStartPoint);
    LEndPoint   := ACoordConvertFunc(FEndPoint);
    LControl1   := ACoordConvertFunc(FControl1);
    LControl2   := ACoordConvertFunc(FControl2);
    LOpposite1  := ACoordConvertFunc(FOpposite1);
    LOpposite2  := ACoordConvertFunc(FOpposite2);
  end
  else
  begin
    LStartPoint := FStartPoint;
    LEndPoint   := FEndPoint;
    LControl1   := FControl1;
    LControl2   := FControl2;
    LOpposite1  := FOpposite1;
    LOpposite2  := FOpposite2;
  end;

  if SquareContainsPoint( LStartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshStart;
  end
  else
  if SquareContainsPoint( LEndPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshEnd;
  end
  else
  if SquareContainsPoint( LControl1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshControl1;
  end
  else
  if SquareContainsPoint( LOpposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshOpposite1;
  end
  else
  if SquareContainsPoint( LControl2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshControl2;
  end
  else
  if SquareContainsPoint( LOpposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
  begin
    Result := pshOpposite2;
  end
  else
  begin
    Result := pshNone;
  end;
end;

procedure TgmCurveSegment.CopyCurveSegment(const ASource: TgmCurveSegment);
begin
  FStartPoint := ASource.StartPoint;
  FControl1   := ASource.Control1;
  FOpposite1  := ASource.Opposite1;
  FControl2   := ASource.Control2;
  FOpposite2  := ASource.Opposite2;
  FEndPoint   := ASource.EndPoint;
end; 

procedure TgmCurveSegment.ScaleCurveSegment(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);
begin
  FStartPoint := ScalePoint(FStartPoint, AOldWidth, AOldHeight, ANewWidth, ANewHeight);
  FEndPoint   := ScalePoint(FEndPoint,   AOldWidth, AOldHeight, ANewWidth, ANewHeight);
  FControl1   := ScalePoint(FControl1,   AOldWidth, AOldHeight, ANewWidth, ANewHeight);
  FControl2   := ScalePoint(FControl2,   AOldWidth, AOldHeight, ANewWidth, ANewHeight);
  FOpposite1  := ScalePoint(FOpposite1,  AOldWidth, AOldHeight, ANewWidth, ANewHeight);
  FOpposite2  := ScalePoint(FOpposite2,  AOldWidth, AOldHeight, ANewWidth, ANewHeight);
end;

procedure TgmCurveSegment.SaveToStream(const AMemoryStream: TMemoryStream);
var
  LIntValue: Integer;
begin
  AMemoryStream.Write(FStartPoint.X, 4);
  AMemoryStream.Write(FStartPoint.Y, 4);
  AMemoryStream.Write(FEndPoint.X,   4);
  AMemoryStream.Write(FEndPoint.Y,   4);
  AMemoryStream.Write(FControl1.X,   4);
  AMemoryStream.Write(FControl1.Y,   4);
  AMemoryStream.Write(FControl2.X,   4);
  AMemoryStream.Write(FControl2.Y,   4);
  AMemoryStream.Write(FOpposite1.X,  4);
  AMemoryStream.Write(FOpposite1.Y,  4);
  AMemoryStream.Write(FOpposite2.X,  4);
  AMemoryStream.Write(FOpposite2.Y,  4);
  AMemoryStream.Write(FActive,       1);

  LIntValue := Ord(FActivePoint);
  AMemoryStream.Write(LIntValue, 4);

  LIntValue := Ord(FStartingPointType);
  AMemoryStream.Write(LIntValue, 4);

  LIntValue := Ord(FEndingPointType);
  AMemoryStream.Write(LIntValue, 4);

  AMemoryStream.Write(FControl1Radian,    SizeOf(Double));
  AMemoryStream.Write(FOpposite1Radian,   SizeOf(Double));
  AMemoryStream.Write(FControl2Radian,    SizeOf(Double));
  AMemoryStream.Write(FOpposite2Radian,   SizeOf(Double));
  AMemoryStream.Write(FRadianDifference1, SizeOf(Double));
  AMemoryStream.Write(FRadianDifference2, SizeOf(Double));
  AMemoryStream.Write(FControl1Length,    SizeOf(Double));
  AMemoryStream.Write(FOpposite1Length,   SizeOf(Double));
  AMemoryStream.Write(FControl2Length,    SizeOf(Double));
  AMemoryStream.Write(FOpposite2Length,   SizeOf(Double));

  LIntValue := Ord(FControl1PairState);
  AMemoryStream.Write(LIntValue, 4);

  LIntValue := Ord(FControl2PairState);
  AMemoryStream.Write(LIntValue, 4);

  AMemoryStream.Write(FControl1PairChanged, 1);
  AMemoryStream.Write(FControl2PairChanged, 1);
end; 

{ TgmCurveSegmentsList }

constructor TgmCurveSegmentsList.Create;
begin
  inherited Create;

  FCurrentSegment := nil;
  FNextSegment    := nil;
  FCurrentIndex   := 0;
  FSelectedCount  := 0;
  FClosed         := False;
  FSelected       := True;
  FSelectedAll    := False;
  FFirstSegmentOK := False;
end;

destructor TgmCurveSegmentsList.Destroy;
begin
  DeleteAllCurveSegments;
  inherited Destroy;
end;

// Assign data from another one.
procedure TgmCurveSegmentsList.AssignCurveSegmentListData(
  const ASegmentList: TgmCurveSegmentsList);
var
  i, LIndex     : Integer;
  LSegment      : TgmCurveSegment;
  LSegmentBackup: TgmCurveSegment;
begin
  if Assigned(ASegmentList) then
  begin
    DeleteAllCurveSegments;

    if ASegmentList.Count > 0 then
    begin
      for i := 0 to (ASegmentList.Count - 1) do
      begin
        LSegment       := TgmCurveSegment(ASegmentList.Items[i]);
        LSegmentBackup := LSegment.GetCurveSegmentSelfBackup;

        Self.Add(LSegmentBackup);
      end;

      LIndex := ASegmentList.GetCurrentSegmentIndexInList;

      if LIndex >= 0 then
      begin
        FCurrentSegment := TgmCurveSegment(Self.Items[LIndex]);
      end
      else
      begin
        FCurrentSegment := nil;
      end;

      LIndex := ASegmentList.GetNextSegmentIndexInList;
      
      if LIndex >= 0 then
      begin
        FNextSegment := TgmCurveSegment(Self.Items[LIndex]);
      end
      else
      begin
        FNextSegment := nil;
      end;

      FCurrentIndex   := ASegmentList.CurrentIndex;
      FSelectedCount  := ASegmentList.SelectedCount;
      FClosed         := ASegmentList.IsClosed;
      FSelected       := ASegmentList.IsSelected;
      FSelectedAll    := ASegmentList.IsSelectedAll;
      FFirstSegmentOK := ASegmentList.IsFirstSegmentOK;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveEndingHandles(const ACanvas: TCanvas;
  const APenMode: TPenMode);
var
  LTempPenWidth      : Integer;
  LTempPenColor      : TColor;
  LTempPenStyle      : TPenStyle;
  LTempPenMode       : TPenMode;
  i                  : Integer;
  LSegment1          : TgmCurveSegment;
  LSegment2          : TgmCurveSegment;
  LHandleColor       : TColor;
  LFillingBrushStyle : TBrushStyle;
  LStartPoint        : TPoint;
  LEndPoint          : TPoint;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clBlack;
      Pen.Style := psSolid;
      Pen.Mode  := pmNotXor;

      LHandleColor := clBlack;

      for i := 0 to (Self.Count - 1) do
      begin
        LSegment1   := Self.Items[i];
        LStartPoint := LSegment1.StartPoint;
        LEndPoint   := LSegment1.EndPoint;

        // if the first curve segment has not drawing complete...
        if FFirstSegmentOK = False then
        begin
          LFillingBrushStyle := bsSolid;
          
          DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                         LFillingBrushStyle, LHandleColor, pmNotXor);
        end
        else
        begin
          // if the first curve segment is drawing complete and the path is not closed...
          if FClosed = False then
          begin
            if i = 0 then
            begin
              if not FSelectedAll then
              begin
                if LSegment1.IsActive and (LSegment1.ActivePoint = apStart) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              { If the start point and the end point of the curve segment are at
                different position, then drawing the start point handle. Otherwise,
                if the first curve segment is selected, then drawing it's
                end point handle. }
              if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
              begin
                DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, pmNotXor);
              end
              else
              begin
                if LSegment1.IsActive then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;

                DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, pmNotXor)
              end;
            end;
          end;

          // if current curve segment is not the last one...
          if i < (Self.Count - 1) then
          begin
            if not FSelectedAll then
            begin
              LSegment2 := Self.Items[i + 1];

              if ( LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) ) or
                 ( LSegment2.IsActive and (LSegment2.ActivePoint = apStart) ) then
              begin
                LFillingBrushStyle := bsSolid;
              end
              else
              begin
                LFillingBrushStyle := bsClear;
              end;
            end
            else
            begin
              LFillingBrushStyle := bsSolid;
            end;

            if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
            begin
              DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                             LFillingBrushStyle, LHandleColor, pmNotXor);
            end;
          end;

          // if current curve segment is the last one...
          if i = (Self.Count - 1) then
          begin
            // if the path is closed...
            if FClosed then
            begin
              if not FSelectedAll then
              begin
                LSegment2 := Self.Items[0];

                if ( LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) ) or
                   ( LSegment2.IsActive and (LSegment2.ActivePoint = apStart) ) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              if Self.Count = 1 then
              begin
                DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, pmNotXor);
              end
              else
              begin
                if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
                begin
                  DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                                 LFillingBrushStyle, LHandleColor, pmNotXor);
                end;
              end;
            end
            else  // if the path is not closed...
            begin
              if not FSelectedAll then
              begin
                if LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
              begin
                DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, pmNotXor);
              end;
            end;
          end;
        end;
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveEndingHandles(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  LTempPenWidth      : Integer;
  LTempPenColor      : TColor;
  LTempPenStyle      : TPenStyle;
  LTempPenMode       : TPenMode;
  i                  : Integer;
  LSegment1          : TgmCurveSegment;
  LSegment2          : TgmCurveSegment;
  LHandleColor       : TColor;
  LFillingBrushStyle : TBrushStyle;
  LStartPoint        : TPoint;
  LEndPoint          : TPoint;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clBlack;
      Pen.Style := psSolid;
      Pen.Mode  := APenMode;

      LHandleColor := clBlack;

      for i := 0 to (Self.Count - 1) do
      begin
        LSegment1 := Self.Items[i];

        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint := ACoordConvertFunc(LSegment1.StartPoint);
          LEndPoint   := ACoordConvertFunc(LSegment1.EndPoint);
        end
        else
        begin
          LStartPoint := LSegment1.StartPoint;
          LEndPoint   := LSegment1.EndPoint;
        end;

        // if the first curve segment has not drawing complete...
        if FFirstSegmentOK = False then
        begin
          LFillingBrushStyle := bsSolid;
          
          DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                         LFillingBrushStyle, LHandleColor, APenMode);
        end
        else
        begin
          // if the first curve segment is drawing complete and the path is not closed...
          if FClosed = False then
          begin
            if i = 0 then
            begin
              if not FSelectedAll then
              begin
                if LSegment1.IsActive and (LSegment1.ActivePoint = apStart) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              // If the start point and the end point of the curve segment are at
              // different position, then drawing the start point handle. Otherwise,
              // if the first curve segment is selected, then drawing it's
              // end point handle. 
              if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
              begin
                DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, APenMode);
              end
              else
              begin
                if LSegment1.IsActive then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;

                DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, APenMode)
              end;
            end;
          end;

          // if current curve segment is not the last one...
          if i < (Self.Count - 1) then
          begin
            if not FSelectedAll then
            begin
              LSegment2 := Self.Items[i + 1];

              if ( LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) ) or
                 ( LSegment2.IsActive and (LSegment2.ActivePoint = apStart) ) then
              begin
                LFillingBrushStyle := bsSolid;
              end
              else
              begin
                LFillingBrushStyle := bsClear;
              end;
            end
            else
            begin
              LFillingBrushStyle := bsSolid;
            end;

            if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
            begin
              DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                             LFillingBrushStyle, LHandleColor, APenMode);
            end;
          end;

          // if current curve segment is the last one...
          if i = (Self.Count - 1) then
          begin
            // if the path is closed...
            if FClosed then
            begin
              if not FSelectedAll then
              begin
                LSegment2 := Self.Items[0];

                if ( LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) ) or
                   ( LSegment2.IsActive and (LSegment2.ActivePoint = apStart) ) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              if Self.Count = 1 then
              begin
                DrawRectHandle(ACanvas, LStartPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, APenMode);
              end
              else
              begin
                if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
                begin
                  DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                                 LFillingBrushStyle, LHandleColor, APenMode);
                end;
              end;
            end
            else  // if the path is not closed...
            begin
              if not FSelectedAll then
              begin
                if LSegment1.IsActive and (LSegment1.ActivePoint = apEnd) then
                begin
                  LFillingBrushStyle := bsSolid;
                end
                else
                begin
                  LFillingBrushStyle := bsClear;
                end;
              end
              else
              begin
                LFillingBrushStyle := bsSolid;
              end;

              if DifferentCoordinate(LSegment1.StartPoint, LSegment1.EndPoint) then
              begin
                DrawRectHandle(ACanvas, LEndPoint, PATH_HANDLE_RADIUS,
                               LFillingBrushStyle, LHandleColor, APenMode);
              end;
            end;
          end;
        end;
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveDirectionHandles(
  const ACanvas: TCanvas; const APenMode: TPenMode);
var
  LControl1, LOpposite1: TPoint;
  LControl2, LOpposite2: TPoint;
begin
  if FSelectedCount = 1 then
  begin
    LControl1  := FCurrentSegment.Control1;
    LControl2  := FCurrentSegment.Control2;
    LOpposite1 := FCurrentSegment.Opposite1;
    LOpposite2 := FCurrentSegment.Opposite2;

    if FClosed then
    begin
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
      begin
        DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(CurrentSegment.Control2, CurrentSegment.Opposite1) then
      begin
        DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.Opposite2, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LOpposite2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
    end
    else
    begin
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Control2) then
      begin
        DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      case FCurrentSegment.ActivePoint of
        apStart:
          begin
            if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
            begin
              DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
            end;
          end;

        apEnd:
          begin
            if FCurrentSegment.StartingPointType = eptCornerPoint then
            begin
              if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
              begin
                DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
              end;
            end;

            if FCurrentSegment.EndingPointType = eptAnchorPoint then
            begin
              if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2) then
              begin
                DrawDiamondHandle(ACanvas, LOpposite2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
              end;
            end;
        end;
      end;
    end;
  end;

  if FSelectedCount = 2 then
  begin
    LControl1 := FCurrentSegment.Control1;
    LControl2 := FCurrentSegment.Control2;

    if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
    begin
      DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
    
    if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Control2) then
    begin
      DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
    
    LControl1 := FNextSegment.Control1;
    LControl2 := FNextSegment.Control2;

    if DifferentCoordinate(FNextSegment.StartPoint, FNextSegment.Control1) then
    begin
      DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
    
    if DifferentCoordinate(FNextSegment.EndPoint, FNextSegment.Control2) then
    begin
      DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveDirectionHandles(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  LControl1, LOpposite1 : TPoint;
  LControl2, LOpposite2 : TPoint;
begin
  if FSelectedCount = 1 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LControl1  := ACoordConvertFunc(FCurrentSegment.Control1);
      LControl2  := ACoordConvertFunc(FCurrentSegment.Control2);
      LOpposite1 := ACoordConvertFunc(FCurrentSegment.Opposite1);
      LOpposite2 := ACoordConvertFunc(FCurrentSegment.Opposite2);
    end
    else
    begin
      LControl1  := FCurrentSegment.Control1;
      LControl2  := FCurrentSegment.Control2;
      LOpposite1 := FCurrentSegment.Opposite1;
      LOpposite2 := FCurrentSegment.Opposite2;
    end;

    if FClosed then
    begin
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
      begin
        DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(CurrentSegment.Control2, CurrentSegment.Opposite1) then
      begin
        DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.Opposite2, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LOpposite2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
    end
    else
    begin
      if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
      begin
        DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Control2) then
      begin
        DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
      end;
      
      case FCurrentSegment.ActivePoint of
        apStart:
          begin
            if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
            begin
              DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
            end;
          end;

        apEnd:
          begin
            if FCurrentSegment.StartingPointType = eptCornerPoint then
            begin
              if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Opposite1) then
              begin
                DrawDiamondHandle(ACanvas, LOpposite1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
              end;
            end;

            if FCurrentSegment.EndingPointType = eptAnchorPoint then
            begin
              if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2) then
              begin
                DrawDiamondHandle(ACanvas, LOpposite2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
              end;
            end;
        end;
      end;
    end;
  end;

  if FSelectedCount = 2 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LControl1 := ACoordConvertFunc(FCurrentSegment.Control1);
      LControl2 := ACoordConvertFunc(FCurrentSegment.Control2);
    end
    else
    begin
      LControl1 := FCurrentSegment.Control1;
      LControl2 := FCurrentSegment.Control2;
    end;

    if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.Control1) then
    begin
      DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
    
    if DifferentCoordinate(FCurrentSegment.EndPoint, FCurrentSegment.Control2) then
    begin
      DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;

    if Assigned(ACoordConvertFunc) then
    begin
      LControl1 := ACoordConvertFunc(FNextSegment.Control1);
      LControl2 := ACoordConvertFunc(FNextSegment.Control2);
    end
    else
    begin
      LControl1 := FNextSegment.Control1;
      LControl2 := FNextSegment.Control2;
    end;

    if DifferentCoordinate(FNextSegment.StartPoint, FNextSegment.Control1) then
    begin
      DrawDiamondHandle(ACanvas, LControl1, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
    
    if DifferentCoordinate(FNextSegment.EndPoint, FNextSegment.Control2) then
    begin
      DrawDiamondHandle(ACanvas, LControl2, PATH_HANDLE_RADIUS, bsSolid, APenMode);
    end;
  end;
end;

procedure TgmCurveSegmentsList.DeleteAllCurveSegments;
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);
      LSegment.Free;
    end;

    Self.Clear;
    FCurrentSegment := nil;
    FNextSegment    := nil;
  end;
end;

procedure TgmCurveSegmentsList.DeleteCurveSegmentByIndex(const AIndex: Integer);
var
  LSegment: TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    if (AIndex >=0) and (AIndex < Self.Count) then
    begin
      LSegment := TgmCurveSegment(Self.Items[AIndex]);

      LSegment.Free;
      Self.Delete(AIndex);
    end;
  end;
end;

function TgmCurveSegmentsList.SelectAtSingleSegment(
  const AX, AY: Integer): Boolean;
var
  i, LPobSteps : Integer;
  LBezier      : TsdBezier;
  LPoint       : TsdPoint;
  LSegment     : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments;

  FSelectedCount := 0;
  LPoint.X       := AX;
  LPoint.Y       := AY;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment       := TgmCurveSegment(Self.Items[i]);
      LBezier.P[0].X := LSegment.StartPoint.X;
      LBezier.P[0].Y := LSegment.StartPoint.Y;
      LBezier.P[1].X := LSegment.Control1.X;
      LBezier.P[1].Y := LSegment.Control1.Y;
      LBezier.P[2].X := LSegment.Control2.X;
      LBezier.P[2].Y := LSegment.Control2.Y;
      LBezier.P[3].X := LSegment.EndPoint.X;
      LBezier.P[3].Y := LSegment.EndPoint.Y;

      if NilsHaeckIsPointOnBezier(LBezier, LPoint, LPobSteps, TOLERANCE_OVER_BEZIER) then
      begin
        FCurrentIndex := i;
        FSelected     := True;
        
        { If the last curve segment is selected and the path is closed, then
          select the last curve segment and the first curve segment. Otherwise,
          select current curve segment and the next one of it (if there is a next). }
        if i = (Self.Count - 1) then
        begin
          if FClosed then
          begin
            FCurrentSegment          := LSegment;
            FCurrentSegment.IsActive := True;

            if Self.Count > 1 then
            begin
              FCurrentSegment.ActivePoint := apEnd;
              FNextSegment                := TgmCurveSegment(Self.Items[0]);
              FNextSegment.IsActive       := True;
              FNextSegment.ActivePoint    := apStart;
              FSelectedCount              := 2;
            end
            else
            begin
              FCurrentSegment.ActivePoint := apStart;
              FSelectedCount              := 1;
            end;
            
            Break;
          end;
        end;

        FCurrentSegment             := LSegment;
        FCurrentSegment.IsActive    := True;
        FCurrentSegment.ActivePoint := apEnd;
        FSelectedCount              := 1;

        if i < (Self.Count - 1) then
        begin
          FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
          FNextSegment.IsActive    := True;
          FNextSegment.ActivePoint := apStart;
          FSelectedCount           := 2;
        end;

        Break;
      end;
    end;
  end;
  
  Result := FSelected;
end;

function TgmCurveSegmentsList.SelectAtSingleSegment(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i, LPobSteps : Integer;
  LBezier      : TsdBezier;
  LPoint       : TsdPoint;
  LTempPoint   : TPoint;
  LSegment     : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments();

  FSelectedCount := 0;
  LPoint.X       := AX;
  LPoint.Y       := AY;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if Assigned(ACoordConvertFunc) then
      begin
        LTempPoint     := ACoordConvertFunc(LSegment.StartPoint);
        LBezier.P[0].X := LTempPoint.X;
        LBezier.P[0].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.Control1);
        LBezier.P[1].X := LTempPoint.X;
        LBezier.P[1].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.Control2);
        LBezier.P[2].X := LTempPoint.X;
        LBezier.P[2].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.EndPoint);
        LBezier.P[3].X := LTempPoint.X;
        LBezier.P[3].Y := LTempPoint.Y;
      end
      else
      begin
        LBezier.P[0].X := LSegment.StartPoint.X;
        LBezier.P[0].Y := LSegment.StartPoint.Y;
        LBezier.P[1].X := LSegment.Control1.X;
        LBezier.P[1].Y := LSegment.Control1.Y;
        LBezier.P[2].X := LSegment.Control2.X;
        LBezier.P[2].Y := LSegment.Control2.Y;
        LBezier.P[3].X := LSegment.EndPoint.X;
        LBezier.P[3].Y := LSegment.EndPoint.Y;
      end;

      if NilsHaeckIsPointOnBezier(LBezier, LPoint, LPobSteps, TOLERANCE_OVER_BEZIER) then
      begin
        FCurrentIndex := i;
        FSelected     := True;
        
        // If the last curve segment is selected and the path is closed, then
        // select the last curve segment and the first curve segment. Otherwise,
        // select current curve segment and the next one of it (if there is a next). 
        if i = (Self.Count - 1) then
        begin
          if FClosed then
          begin
            FCurrentSegment          := LSegment;
            FCurrentSegment.IsActive := True;

            if Self.Count > 1 then
            begin
              FCurrentSegment.ActivePoint := apEnd;
              FNextSegment                := TgmCurveSegment(Self.Items[0]);
              FNextSegment.IsActive       := True;
              FNextSegment.ActivePoint    := apStart;
              FSelectedCount              := 2;
            end
            else
            begin
              FCurrentSegment.ActivePoint := apStart;
              FSelectedCount              := 1;
            end;
            
            Break;
          end;
        end;

        FCurrentSegment             := LSegment;
        FCurrentSegment.IsActive    := True;
        FCurrentSegment.ActivePoint := apEnd;
        FSelectedCount              := 1;

        if i < (Self.Count - 1) then
        begin
          FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
          FNextSegment.IsActive    := True;
          FNextSegment.ActivePoint := apStart;
          FSelectedCount           := 2;
        end;

        Break;
      end;
    end;
  end;
  
  Result := FSelected;
end;

(*
function TgmCurveSegmentsList.SelectAtWholePath(const AX, AY: Integer): Boolean;
var
  i, LPobSteps: Integer;
  LBezier     : TsdBezier;
  LPoint      : TsdPoint;
  LSegment    : TCurveSegment;
begin
  FSelectedAll := False;
  LPoint.X     := AX;
  LPoint.Y     := AY;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment       := TCurveSegment(Self.Items[i]);
      LBezier.P[0].X := LSegment.StartPoint.X;
      LBezier.P[0].Y := LSegment.StartPoint.Y;
      LBezier.P[1].X := LSegment.Control1.X;
      LBezier.P[1].Y := LSegment.Control1.Y;
      LBezier.P[2].X := LSegment.Control2.X;
      LBezier.P[2].Y := LSegment.Control2.Y;
      LBezier.P[3].X := LSegment.EndPoint.X;
      LBezier.P[3].Y := LSegment.EndPoint.Y;

      if NilsHaeckIsPointOnBezier(LBezier, LPoint, LPobSteps, TOLERANCE_OVER_BEZIER) then
      begin
        FSelectedAll := True;
        Break;
      end;
    end;
  end;
  
  Result := FSelectedAll;
end; 
*)

function TgmCurveSegmentsList.SelectAtHandles(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle): Boolean;
var
  i        : Integer;
  LSegment : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments;
  FSelectedCount := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment        := TgmCurveSegment(Self.Items[i]);
      ASelectedHandle := LSegment.GetHandle(AX, AY);

      if ASelectedHandle <> pshNone then
      begin
        FSelected := True; // mark the whole path is selected

        if FClosed then
        begin
          { If the path is closed, and we get the following handles, it indicates
            that the mouse is point on the mutual handle of the first curve
            segment and the last curve segment. If there is only one curve
            segment, then save it in FCurrentSegment; if there are more than one
            curve segment, then save the last one in FCurrentSegment and save
            the first one in FNextSegment. }
          if ASelectedHandle in [pshStart, pshControl1, pshOpposite1] then
          begin
            if Self.Count = 1 then
            begin
              FCurrentSegment             := LSegment;
              FCurrentSegment.IsActive    := True;
              FCurrentSegment.ActivePoint := apStart;
              FSelectedCount              := 1;
              FCurrentIndex               := 0;
            end
            else
            begin
              FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
              FCurrentSegment.IsActive    := True;
              FCurrentSegment.ActivePoint := apEnd;
              FCurrentIndex               := Self.Count - 1;
              FNextSegment                := TgmCurveSegment(Self.Items[0]);
              FNextSegment.IsActive       := True;
              FNextSegment.ActivePoint    := apStart;
              FSelectedCount              := 2;
            end;
          end
          else
          { If the path is closed and we get the following handles, it indicates
            that there are more than two curve segments in the list at least.
            We need select two adjacent curve segments, save the current curve
            segment in FCurrentSegment, and save the next curve segment of it
            in FNextSegment. }
          if ASelectedHandle in [pshEnd, pshControl2, pshOpposite2] then
          begin
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apEnd;
            FCurrentIndex               := i;
            FSelectedCount              := 1;

            if Self.Count > 1 then
            begin
              FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
              FNextSegment.IsActive    := True;
              FNextSegment.ActivePoint := apStart;
              FSelectedCount           := 2;  // we definitely selected two curve segments in this case 
            end;
          end;
        end
        else
        begin
          { If the path is not closed and we get the following handles, it
            indicates that we selected the first curve segment. }
          if ASelectedHandle in [pshStart, pshControl1, pshOpposite1] then
          begin
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apStart;
            FSelectedCount              := 1;
            FCurrentIndex               := 0;
          end
          else
          { If the path is not close and we get the following handles, then
            we need to select two adjacent curve segments or select the last
            curve segment (if there is only one curve segment in the list,
            the last is just the first). }
          if ASelectedHandle in [pshEnd, pshControl2, pshOpposite2] then
          begin
            { If there are more than one curve segments in the list, then save
              the current curve segment in FCurrentSegment. If there is a curve
              segment that follows by the current one, then save it in FNextSegment. }
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apEnd;
            FCurrentIndex               := i;

            // if current curve segment is not the last one, then select the next one
            if i < (Self.Count - 1) then
            begin
              FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
              FNextSegment.IsActive    := True;
              FNextSegment.ActivePoint := apStart;
              FSelectedCount           := 2;
            end
            else
            begin
              FSelectedCount := 1; // the current curve segment is the last one
            end;
          end;
        end;

        Break;  // once the curve segments are selected then break the loop immediately
      end;
    end;
  end;

  Result := FSelected; // return the selected mark
end;

function TgmCurveSegmentsList.SelectAtHandles(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i        : Integer;
  LSegment : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments();
  FSelectedCount := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment        := TgmCurveSegment(Self.Items[i]);
      ASelectedHandle := LSegment.GetHandle(AX, AY, ACoordConvertFunc);

      if ASelectedHandle <> pshNone then
      begin
        FSelected := True; // mark the whole path is selected

        if FClosed then
        begin
          // If the path is closed, and we get the following handles, it indicates
          // that the mouse is point on the mutual handle of the first curve
          // segment and the last curve segment. If there is only one curve
          // segment, then save it in FCurrentSegment; if there are more than one
          // curve segment, then save the last one in FCurrentSegment and save
          // the first one in FNextSegment. 
          if ASelectedHandle in [pshStart, pshControl1, pshOpposite1] then
          begin
            if Self.Count = 1 then
            begin
              FCurrentSegment             := LSegment;
              FCurrentSegment.IsActive    := True;
              FCurrentSegment.ActivePoint := apStart;
              FSelectedCount              := 1;
              FCurrentIndex               := 0;
            end
            else
            begin
              FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
              FCurrentSegment.IsActive    := True;
              FCurrentSegment.ActivePoint := apEnd;
              FCurrentIndex               := Self.Count - 1;
              FNextSegment                := TgmCurveSegment(Self.Items[0]);
              FNextSegment.IsActive       := True;
              FNextSegment.ActivePoint    := apStart;
              FSelectedCount              := 2;
            end;
          end
          else
          // If the path is closed and we get the following handles, it indicates
          // that there are more than two curve segments in the list at least.
          // We need select two adjacent curve segments, save the current curve
          // segment in FCurrentSegment, and save the next curve segment of it
          // in FNextSegment. 
          if ASelectedHandle in [pshEnd, pshControl2, pshOpposite2] then
          begin
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apEnd;
            FCurrentIndex               := i;
            FSelectedCount              := 1;

            if Self.Count > 1 then
            begin
              FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
              FNextSegment.IsActive    := True;
              FNextSegment.ActivePoint := apStart;
              FSelectedCount           := 2;  // we definitely selected two curve segments in this case 
            end;
          end;
        end
        else
        begin
          // If the path is not closed and we get the following handles, it
          // indicates that we selected the first curve segment.
          if ASelectedHandle in [pshStart, pshControl1, pshOpposite1] then
          begin
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apStart;
            FSelectedCount              := 1;
            FCurrentIndex               := 0;
          end
          else
          // If the path is not close and we get the following handles, then
          // we need to select two adjacent curve segments or select the last
          // curve segment (if there is only one curve segment in the list,
          // the last is just the first).
          if ASelectedHandle in [pshEnd, pshControl2, pshOpposite2] then
          begin
            // If there are more than one curve segments in the list, then save
            // the current curve segment in FCurrentSegment. If there is a curve
            // segment that follows by the current one, then save it in FNextSegment. 
            FCurrentSegment             := LSegment;
            FCurrentSegment.IsActive    := True;
            FCurrentSegment.ActivePoint := apEnd;
            FCurrentIndex               := i;

            // if current curve segment is not the last one, then select the next one
            if i < (Self.Count - 1) then
            begin
              FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
              FNextSegment.IsActive    := True;
              FNextSegment.ActivePoint := apStart;
              FSelectedCount           := 2;
            end
            else
            begin
              FSelectedCount := 1; // the current curve segment is the last one
            end;
          end;
        end;

        Break;  // once the curve segments are selected then break the loop immediately
      end;
    end;
  end;

  Result := FSelected; // return the selected mark
end;

// only check for the ending points not the control points
function TgmCurveSegmentsList.SelectAtEndPoints(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle): Boolean;
var
  i        : Integer;
  LSegment : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments;
  FSelectedCount := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment        := TgmCurveSegment(Self.Items[i]);
      ASelectedHandle := LSegment.GetHandle(AX, AY);

      if ASelectedHandle in [pshStart, pshEnd] then
      begin
        FSelected                := True;
        FCurrentSegment          := LSegment;
        FCurrentSegment.IsActive := True;
        FCurrentIndex            := i;

        case ASelectedHandle of
          pshStart:
            begin
              if FClosed then
              begin
                if Self.Count > 1 then
                begin
                  FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
                  FCurrentSegment.ActivePoint := apEnd;
                  FCurrentSegment.IsActive    := True;
                  FNextSegment                := TgmCurveSegment(Self.Items[0]);
                  FNextSegment.ActivePoint    := apStart;
                  FNextSegment.IsActive       := True;
                  FSelectedCount              := 2;
                  FCurrentIndex               := Self.Count - 1;
                end
                else
                begin
                  FSelectedCount := 1;
                end;
              end
              else
              begin
                FCurrentSegment.ActivePoint := apStart;
                FSelectedCount              := 1;
              end;
            end;

          pshEnd:
            begin
              FCurrentSegment.ActivePoint := apEnd;
              
              if i < Self.Count - 1 then
              begin
                FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
                FNextSegment.IsActive    := True;
                FNextSegment.ActivePoint := apStart;
                FSelectedCount           := 2;
              end
              else
              begin
                FSelectedCount := 1;
              end;
            end;
        end;

        Break;
      end;
    end;
  end;
  
  Result := FSelected;
end;

function TgmCurveSegmentsList.SelectAtEndPoints(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i        : Integer;
  LSegment : TgmCurveSegment;
begin
  FSelectedAll := False;
  FSelected    := False;

  DeactivateAllSegments();
  FSelectedCount := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment        := TgmCurveSegment(Self.Items[i]);
      ASelectedHandle := LSegment.GetHandle(AX, AY, ACoordConvertFunc);

      if ASelectedHandle in [pshStart, pshEnd] then
      begin
        FSelected                := True;
        FCurrentSegment          := LSegment;
        FCurrentSegment.IsActive := True;
        FCurrentIndex            := i;

        case ASelectedHandle of
          pshStart:
            begin
              if FClosed then
              begin
                if Self.Count > 1 then
                begin
                  FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
                  FCurrentSegment.ActivePoint := apEnd;
                  FCurrentSegment.IsActive    := True;
                  FNextSegment                := TgmCurveSegment(Self.Items[0]);
                  FNextSegment.ActivePoint    := apStart;
                  FNextSegment.IsActive       := True;
                  FSelectedCount              := 2;
                  FCurrentIndex               := Self.Count - 1;
                end
                else
                begin
                  FSelectedCount := 1;
                end;
              end
              else
              begin
                FCurrentSegment.ActivePoint := apStart;
                FSelectedCount              := 1;
              end;
            end;

          pshEnd:
            begin
              FCurrentSegment.ActivePoint := apEnd;
              
              if i < Self.Count - 1 then
              begin
                FNextSegment             := TgmCurveSegment(Self.Items[i + 1]);
                FNextSegment.IsActive    := True;
                FNextSegment.ActivePoint := apStart;
                FSelectedCount           := 2;
              end
              else
              begin
                FSelectedCount := 1;
              end;
            end;
        end;

        Break;
      end;
    end;
  end;
  
  Result := FSelected;
end;

procedure TgmCurveSegmentsList.AddFirstSegment(const P1, P2, P3, P4: TPoint);
begin
  FCurrentSegment := nil;
  FCurrentSegment := TgmCurveSegment.Create;

  with FCurrentSegment do
  begin
    StartPoint  := P1;
    Control1    := P2;
    Control2    := P3;
    EndPoint    := P4;
    Opposite1   := GetOppositePoint(StartPoint, Control1);
    Opposite2   := GetOppositePoint(EndPoint, Control2);
    IsActive    := True;
    ActivePoint := apStart;
  end;

  Self.Add(FCurrentSegment);
  FSelectedCount := 1;
end;

procedure TgmCurveSegmentsList.AddFollowSegment(const P1, P2: TPoint);
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  FSelectedAll := False;
  LSegment     := nil;
  
  if Self.Count > 0 then
  begin
    FSelectedCount := 1;

    for i := 0 to (Self.Count - 1) do
    begin
      LSegment             := TgmCurveSegment(Self.Items[i]);
      LSegment.IsActive    := False;
      LSegment.ActivePoint := apNone;
    end;

    FCurrentSegment := nil;
    FCurrentSegment := TgmCurveSegment.Create;

    with FCurrentSegment do
    begin
      StartPoint := LSegment.EndPoint;

      case LSegment.EndingPointType of
        eptAnchorPoint:
          begin
            Control1 := LSegment.Opposite2;
          end;

        eptCornerPoint:
          begin
            Control1 := LSegment.EndPoint;
          end;
      end;

      Control2    := P1;
      EndPoint    := P2;
      Opposite1   := GetOppositePoint(StartPoint, Control1);
      Opposite2   := GetOppositePoint(EndPoint, Control2);
      IsActive    := True;
      ActivePoint := apEnd;
    end;
    
    Self.Add(FCurrentSegment);
  end;
end;

function TgmCurveSegmentsList.AddAnchorPointOnSegment(
  const AX, AY: Integer): Boolean;
var
  LSegC    : TgmCurveSegment;
  LTempSeg : TgmCurveSegment;
  LBezier  : TsdBezier;
  B1, B2   : TsdBezier;
  LPoint   : TsdPoint;
  LClosest : TsdPoint;
  A, LDist : Double;
  LSteps   : Integer;
  P        : array[0..3] of TPoint;
  LHandle  : TgmPathSelectHandle;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    DeactivateAllSegments;

    // if the mouse is not on any of the contrl handles...
    if not SelectAtHandles(AX, AY, LHandle) then
    begin
      // if the mouse points on the curve segment...
      if SelectAtSingleSegment(AX, AY) then
      begin
        Result := True;

        { Create a new curve segment used to remember current curve segment
          that before adding anchor point to the current curve segment. }
        LSegC := TgmCurveSegment.Create;
        try
          // copy data of current curve segment to SegC
          LSegC.CopyCurveSegment( TgmCurveSegment(Self.Items[FCurrentIndex]) );

          { Convert data of current curve segment to TsdBezier used for
            Nils Haeck's split bezier algorithm. }
          LBezier.P[0].X := LSegC.StartPoint.X;
          LBezier.P[0].Y := LSegC.StartPoint.Y;
          LBezier.P[1].X := LSegC.Control1.X;
          LBezier.P[1].Y := LSegC.Control1.Y;
          LBezier.P[2].X := LSegC.Control2.X;
          LBezier.P[2].Y := LSegC.Control2.Y;
          LBezier.P[3].X := LSegC.EndPoint.X;
          LBezier.P[3].Y := LSegC.EndPoint.Y;
          
          LPoint.X := AX;
          LPoint.Y := AY;

          // Find closest point on the bezier to point LPoint
          NilsHaeckClosestPointOnBezier(LBezier, LPoint, TOLERANCE_OVER_BEZIER,
                                        A, LSteps, LDist, LClosest);

          // Split beziers using factor A
          NilsHaeckSplitBezierWithFactor(LBezier, A, B1, B2);

          // convert the coordinates of first splitted curve segment to standard format
          NilsHaeckBezierToWindowsFormat(B1, P);

          // make the current curve segment is the first splitted curve segment
          FCurrentSegment.StartPoint := P[0];
          FCurrentSegment.Control1   := P[1];
          FCurrentSegment.Control2   := P[2];
          FCurrentSegment.EndPoint   := P[3];

          // create a new curve segment for FNextSegment
          FNextSegment := nil;
          FNextSegment := TgmCurveSegment.Create;

          // convert the coordinates of second splitted curve segment to standard format
          NilsHaeckBezierToWindowsFormat(B2, P);

          // make FNextSegment is the second splitted curve segment
          FNextSegment.StartPoint := P[0];
          FNextSegment.Control1   := P[1];
          FNextSegment.Control2   := P[2];
          FNextSegment.EndPoint   := P[3];
          
          // the relationship of control handles of two splitted curve segments
          FCurrentSegment.Opposite2 := FNextSegment.Control1;

          FNextSegment.Control1PairState     := psPaired;
          FNextSegment.IsControl1PairChanged := False;
          FNextSegment.Control2PairState     := FCurrentSegment.Control2PairState;
          FNextSegment.IsControl2PairChanged := FCurrentSegment.IsControl2PairChanged;

          FCurrentSegment.Control2PairState     := psPaired;
          FCurrentSegment.IsControl2PairChanged := False;

          FNextSegment.Opposite1 := FCurrentSegment.Control2;
          FNextSegment.Opposite2 := LSegC.Opposite2;

          { Insert the second splitted curve segment into list at the position
            which followed by the first splitted curve segment. }
          Self.Insert(FCurrentIndex + 1, FNextSegment);

          // adjust the type of ending points of the two splitted curve segments
          if FCurrentSegment.EndingPointType = eptCornerPoint then
          begin
            FCurrentSegment.EndingPointType := eptAnchorPoint;
            FNextSegment.EndingPointType    := eptCornerPoint;
          end;

          // if the first curve segment is splitted and the path is closed...
          if FCurrentIndex = 0 then
          begin
            if FClosed then
            begin
              LTempSeg           := TgmCurveSegment(Self.Items[Self.Count - 1]);
              LTempSeg.Opposite2 := FCurrentSegment.Control1;
            end;
          end;

          // if the last curve segment is splitted and the path is closed...
          if FCurrentIndex = (Self.Count - 2) then
          begin
            if FClosed then
            begin
              LTempSeg               := TgmCurveSegment(Self.Items[0]);
              LTempSeg.Opposite1     := FNextSegment.Control2;
              FNextSegment.Opposite2 := LTempSeg.Control1;
            end;
          end;

          { If the current curve segment has predecessor in the list, then
            adjust the handle relationships of the current and the predecessor. }
          if FCurrentIndex > 0 then
          begin
            LTempSeg           := TgmCurveSegment(Self.Items[FCurrentIndex - 1]);
            LTempSeg.Opposite2 := FCurrentSegment.Control1;
          end;

          { If the second splitted curve segment has successor in the list,
            adjust the handle relationships of the second splitted curve and
            the successor. }
          if FCurrentIndex < (Self.Count - 2) then
          begin
            LTempSeg                       := TgmCurveSegment(Self.Items[FCurrentIndex + 2]);
            LTempSeg.Opposite1             := FNextSegment.Control2;
            LTempSeg.ActivePoint           := apNone;
            LTempSeg.Control1PairState     := FCurrentSegment.Control2PairState;
            LTempSeg.IsControl1PairChanged := FCurrentSegment.IsControl2PairChanged;
          end;

          // select the two splitted curve segments
          FSelectedCount              := 2;
          FCurrentSegment.IsActive    := True;
          FCurrentSegment.ActivePoint := apEnd;
          FNextSegment.IsActive       := True;
          FNextSegment.ActivePoint    := apStart;

        finally
          LSegC.Free;
        end;
      end;
    end;
  end;
end;

// Note:
//
// The function passed by parameter ACoordConvertFuncForTesting should
// convert path coordinates to be as identical space as (AX, AY) for
// testing whether (AX, AY) is pointing on any of a curve segment.
//
// If so...
//
// The function passed by parameter ACoordConvertFuncForAdding should
// convert (AX, AY) to bitmap space for adding on the curve segment.
//
function TgmCurveSegmentsList.AddAnchorPointOnSegment(const AX, AY: Integer;
  ACoordConvertFuncForTesting: TgmPointCoordConvertFunc;
  ACoordConvertFuncForAdding: TgmPointCoordConvertFunc): Boolean;
var
  LSegC      : TgmCurveSegment;
  LTempSeg   : TgmCurveSegment;
  LBezier    : TsdBezier;
  B1, B2     : TsdBezier;
  LPoint     : TsdPoint;
  LClosest   : TsdPoint;
  LTempPoint : TPoint;
  A, LDist   : Double;
  LSteps     : Integer;
  P          : array[0..3] of TPoint;
  LHandle    : TgmPathSelectHandle;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    DeactivateAllSegments();

    // if the mouse is not on any of the contrl handles...
    if not SelectAtHandles(AX, AY, LHandle, ACoordConvertFuncForTesting) then
    begin
      // if the mouse points on the curve segment...
      if SelectAtSingleSegment(AX, AY, ACoordConvertFuncForTesting) then
      begin
        Result := True;

        // Create a new curve segment used to remember current curve segment
        // that before adding anchor point to the current curve segment. 
        LSegC := TgmCurveSegment.Create();
        try
          // copy data of current curve segment to SegC
          LSegC.CopyCurveSegment( TgmCurveSegment(Self.Items[FCurrentIndex]) );

          // Convert data of current curve segment to TsdBezier used for
          // Nils Haeck's split bezier algorithm. 
          LBezier.P[0].X := LSegC.StartPoint.X;
          LBezier.P[0].Y := LSegC.StartPoint.Y;
          LBezier.P[1].X := LSegC.Control1.X;
          LBezier.P[1].Y := LSegC.Control1.Y;
          LBezier.P[2].X := LSegC.Control2.X;
          LBezier.P[2].Y := LSegC.Control2.Y;
          LBezier.P[3].X := LSegC.EndPoint.X;
          LBezier.P[3].Y := LSegC.EndPoint.Y;

          // AX, AY may need to be convert to bitmap space for testing,
          // because all the Bezier curve points should be in bitmap space.
          if Assigned(ACoordConvertFuncForAdding) then
          begin
            LTempPoint := ACoordConvertFuncForAdding( Point(AX, AY) );
          end
          else
          begin
            LTempPoint := Point(AX, AY);
          end;
          
          LPoint.X := LTempPoint.X;
          LPoint.Y := LTempPoint.Y;

          // Find closest point on the bezier to point LPoint
          NilsHaeckClosestPointOnBezier(LBezier, LPoint, TOLERANCE_OVER_BEZIER,
                                        A, LSteps, LDist, LClosest);

          // Split beziers using factor A
          NilsHaeckSplitBezierWithFactor(LBezier, A, B1, B2);

          // convert the coordinates of first splitted curve segment to standard format
          NilsHaeckBezierToWindowsFormat(B1, P);

          // make the current curve segment is the first splitted curve segment
          FCurrentSegment.StartPoint := P[0];
          FCurrentSegment.Control1   := P[1];
          FCurrentSegment.Control2   := P[2];
          FCurrentSegment.EndPoint   := P[3];

          // create a new curve segment for FNextSegment
          FNextSegment := nil;
          FNextSegment := TgmCurveSegment.Create();

          // convert the coordinates of second splitted curve segment to standard format
          NilsHaeckBezierToWindowsFormat(B2, P);

          // make FNextSegment is the second splitted curve segment
          FNextSegment.StartPoint := P[0];
          FNextSegment.Control1   := P[1];
          FNextSegment.Control2   := P[2];
          FNextSegment.EndPoint   := P[3];
          
          // the relationship of control handles of two splitted curve segments
          FCurrentSegment.Opposite2 := FNextSegment.Control1;

          FNextSegment.Control1PairState     := psPaired;
          FNextSegment.IsControl1PairChanged := False;
          FNextSegment.Control2PairState     := FCurrentSegment.Control2PairState;
          FNextSegment.IsControl2PairChanged := FCurrentSegment.IsControl2PairChanged;

          FCurrentSegment.Control2PairState     := psPaired;
          FCurrentSegment.IsControl2PairChanged := False;

          FNextSegment.Opposite1 := FCurrentSegment.Control2;
          FNextSegment.Opposite2 := LSegC.Opposite2;

          // Insert the second splitted curve segment into list at the position
          // which followed by the first splitted curve segment.
          Self.Insert(FCurrentIndex + 1, FNextSegment);

          // adjust the type of ending points of the two splitted curve segments
          if FCurrentSegment.EndingPointType = eptCornerPoint then
          begin
            FCurrentSegment.EndingPointType := eptAnchorPoint;
            FNextSegment.EndingPointType    := eptCornerPoint;
          end;

          // if the first curve segment is splitted and the path is closed...
          if FCurrentIndex = 0 then
          begin
            if FClosed then
            begin
              LTempSeg           := TgmCurveSegment(Self.Items[Self.Count - 1]);
              LTempSeg.Opposite2 := FCurrentSegment.Control1;
            end;
          end;

          // if the last curve segment is splitted and the path is closed...
          if FCurrentIndex = (Self.Count - 2) then
          begin
            if FClosed then
            begin
              LTempSeg               := TgmCurveSegment(Self.Items[0]);
              LTempSeg.Opposite1     := FNextSegment.Control2;
              FNextSegment.Opposite2 := LTempSeg.Control1;
            end;
          end;

          // If the current curve segment has predecessor in the list, then
          // adjust the handle relationships of the current and the predecessor. 
          if FCurrentIndex > 0 then
          begin
            LTempSeg           := TgmCurveSegment(Self.Items[FCurrentIndex - 1]);
            LTempSeg.Opposite2 := FCurrentSegment.Control1;
          end;

          // If the second splitted curve segment has successor in the list,
          // adjust the handle relationships of the second splitted curve and
          // the successor. 
          if FCurrentIndex < (Self.Count - 2) then
          begin
            LTempSeg                       := TgmCurveSegment(Self.Items[FCurrentIndex + 2]);
            LTempSeg.Opposite1             := FNextSegment.Control2;
            LTempSeg.ActivePoint           := apNone;
            LTempSeg.Control1PairState     := FCurrentSegment.Control2PairState;
            LTempSeg.IsControl1PairChanged := FCurrentSegment.IsControl2PairChanged;
          end;

          // select the two splitted curve segments
          FSelectedCount              := 2;
          FCurrentSegment.IsActive    := True;
          FCurrentSegment.ActivePoint := apEnd;
          FNextSegment.IsActive       := True;
          FNextSegment.ActivePoint    := apStart;

        finally
          LSegC.Free;
        end;
      end;
    end;
  end;
end;

function TgmCurveSegmentsList.DeleteAnchorPointOnSegment(
  const AX, AY: Integer): Boolean;
var
  LSelectedHandle : TgmPathSelectHandle;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    if FSelected then
    begin
      { If the mouse is on the end-point (last point of the path), it indicates
        that maybe the path is not closed, and the mouse is not on the start-point
        of the first curve segment. If the path is closed, it indicates that
        the mouse is not on the closed point, because if the mouse is on the
        closed point, the program will determine the mouse is on the start-point
        of the first curve segment. }
      if SelectAtEndPoints(AX, AY, LSelectedHandle) then
      begin
        Result := True;

        // if there is one curve segment is selected...
        if FSelectedCount = 1 then
        begin
          // if there is only one curve segment in the list...
          if Self.Count = 1 then
          begin
            // if the path is closed...
            if FClosed then
            begin
              if LSelectedHandle in [pshStart, pshEnd] then
              begin
                DeleteAllCurveSegments;
                
                FFirstSegmentOK := False;
                FSelected       := False;
                FClosed         := False;
              end;
            end
            else  // if the path is not closed...
            begin
              { if the start point and end point of the current curve segment
                are overlapped... }
              if DifferentCoordinate(FCurrentSegment.StartPoint, FCurrentSegment.EndPoint) = False then
              begin
                if LSelectedHandle in [pshStart, pshEnd] then
                begin
                  DeleteAllCurveSegments;
                  
                  FFirstSegmentOK := False;
                  FSelected       := False;
                  FClosed         := False;
                end;
              end
              else
              begin
                { Restore the state to that there is one curve segment in the
                  list, but it is not processed complete. }
                if LSelectedHandle in [pshStart, pshEnd] then
                begin
                  FFirstSegmentOK             := False;
                  FCurrentSegment.ActivePoint := apStart;
                end;

                case LSelectedHandle of
                  pshStart:  // if point to the start point, merge it with end point
                    begin
                      with FCurrentSegment do
                      begin
                        StartPoint := EndPoint;
                        Control1   := Opposite2;
                        Opposite1  := Control2;
                        Control2   := EndPoint;
                        Opposite2  := EndPoint;
                      end;
                    end;
                    
                  pshEnd:  // if point to the end point, merge it with start point
                    begin
                      with FCurrentSegment do
                      begin
                        EndPoint  := StartPoint;
                        Control2  := StartPoint;
                        Opposite2 := StartPoint;
                      end;
                    end;
                end;
              end;
            end;
          end
          else
          // if there are more than one curve segments in the list...
          if Self.Count > 1 then
          begin
            case LSelectedHandle of
              pshStart: // the path is not closed and the mouse is point to the start point of the first curve segment
                begin
                  DeleteCurveSegmentByIndex(0);
                  
                  FCurrentSegment             := nil;
                  FCurrentSegment             := TgmCurveSegment(Self.Items[0]);
                  FCurrentSegment.IsActive    := True;
                  FCurrentSegment.ActivePoint := apStart;
                  FCurrentIndex               := 0;
                  FSelectedCount              := 1;
                end;

              pshEnd: // the path is not closed and the mouse is point to the end point of the last curve segment
                begin
                  DeleteCurveSegmentByIndex(Self.Count - 1);
                  
                  FCurrentSegment             := nil;
                  FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
                  FCurrentSegment.IsActive    := True;
                  FCurrentSegment.ActivePoint := apEnd;
                  FCurrentIndex               := Self.Count - 1;
                  FSelectedCount              := 1;
                end;
            end;
          end;
        end;

        // if there are two curve segments are selected...
        if FSelectedCount = 2 then
        begin
          case LSelectedHandle of
            pshStart: // the path is closed and the mouse is point to the start point of the first curve segment
              begin
                DeleteCurveSegmentByIndex(0);
                Dec(FCurrentIndex);

                if Self.Count = 1 then
                begin
                  with FCurrentSegment do
                  begin
                    EndPoint              := StartPoint;
                    Control2              := Opposite1;
                    Opposite2             := Control1;
                    ActivePoint           := apStart;
                    Control2PairState     := Control1PairState;
                    IsControl2PairChanged := IsControl1PairChanged;
                  end;

                  FSelectedCount := 1;
                end
                else
                if Self.Count > 1 then
                begin
                  FNextSegment                          := nil;
                  FNextSegment                          := TgmCurveSegment(Self.Items[0]);
                  FNextSegment.IsActive                 := True;
                  FNextSegment.ActivePoint              := apStart;
                  FCurrentSegment.EndPoint              := FNextSegment.StartPoint;
                  FCurrentSegment.Opposite2             := FNextSegment.Control1;
                  FCurrentSegment.Control2              := FNextSegment.Opposite1;
                  FCurrentSegment.EndingPointType       := FNextSegment.StartingPointType;
                  FCurrentSegment.Control2PairState     := FNextSegment.Control1PairState;
                  FCurrentSegment.IsControl2PairChanged := FNextSegment.IsControl1PairChanged;
                  FSelectedCount                        := 2;
                end;
              end;

            { Merge the end point of the first selected curve segment with the
              end point of the following one, and delete the second selected
              curve segment. After deletion, if there is successor of the current
              curve segment, then select it as the second selected curve segment. }
            pshEnd:
              begin
                FCurrentSegment.EndPoint              := FNextSegment.EndPoint;
                FCurrentSegment.Control2              := FNextSegment.Control2;
                FCurrentSegment.Opposite2             := FNextSegment.Opposite2;
                FCurrentSegment.EndingPointType       := FNextSegment.EndingPointType;
                FCurrentSegment.Control2PairState     := FNextSegment.Control2PairState;
                FCurrentSegment.IsControl2PairChanged := FNextSegment.IsControl2PairChanged;

                DeleteCurveSegmentByIndex(FCurrentIndex + 1);

                if Self.Count = 1 then
                begin
                  if FClosed then
                  begin
                    FCurrentSegment.ActivePoint := apStart;
                  end;

                  FSelectedCount := 1;
                end
                else
                if Self.Count > 1 then
                begin
                  // if there is successor and the path is not closed, then select it
                  if FCurrentIndex < (Self.Count - 1) then
                  begin
                    FNextSegment := nil;
                    FNextSegment := TgmCurveSegment(Self.Items[FCurrentIndex + 1]);
                  end
                  else
                  { After deletion, if there are more than one curve segments
                    in the list, and the current selected one is the last curve
                    segment, then select the first curve segment as the second
                    selected curve. }
                  if FCurrentIndex = (Self.Count - 1) then
                  begin
                    FNextSegment := nil;
                    FNextSegment := TgmCurveSegment(Self.Items[0]);
                  end;

                  FNextSegment.IsActive    := True;
                  FNextSegment.ActivePoint := apStart;
                  FSelectedCount           := 2;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

function TgmCurveSegmentsList.DeleteAnchorPointOnSegment(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  LSelectedHandle : TgmPathSelectHandle;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    if FSelected then
    begin
      // If the mouse is on the end-point (last point of the path), it indicates
      // that maybe the path is not closed, and the mouse is not on the start-point
      // of the first curve segment. If the path is closed, it indicates that
      // the mouse is not on the closed point, because if the mouse is on the
      // closed point, the program will determine the mouse is on the start-point
      // of the first curve segment. 
      if SelectAtEndPoints(AX, AY, LSelectedHandle, ACoordConvertFunc) then
      begin
        Result := True;

        // if there is one curve segment is selected...
        if FSelectedCount = 1 then
        begin
          // if there is only one curve segment in the list...
          if Self.Count = 1 then
          begin
            // if the path is closed...
            if FClosed then
            begin
              if LSelectedHandle in [pshStart, pshEnd] then
              begin
                DeleteAllCurveSegments();
                
                FFirstSegmentOK := False;
                FSelected       := False;
                FClosed         := False;
              end;
            end
            else  // if the path is not closed...
            begin
              // if the start point and end point of the current curve segment
              // are overlapped... 
              if DifferentCoordinate(FCurrentSegment.StartPoint,
                                     FCurrentSegment.EndPoint) = False then
              begin
                if LSelectedHandle in [pshStart, pshEnd] then
                begin
                  DeleteAllCurveSegments();
                  
                  FFirstSegmentOK := False;
                  FSelected       := False;
                  FClosed         := False;
                end;
              end
              else
              begin
                // Restore the state to that there is one curve segment in the
                // list, but it is not processed complete. 
                if LSelectedHandle in [pshStart, pshEnd] then
                begin
                  FFirstSegmentOK             := False;
                  FCurrentSegment.ActivePoint := apStart;
                end;

                case LSelectedHandle of
                  pshStart:  // if point to the start point, merge it with end point
                    begin
                      with FCurrentSegment do
                      begin
                        StartPoint := EndPoint;
                        Control1   := Opposite2;
                        Opposite1  := Control2;
                        Control2   := EndPoint;
                        Opposite2  := EndPoint;
                      end;
                    end;
                    
                  pshEnd:  // if point to the end point, merge it with start point
                    begin
                      with FCurrentSegment do
                      begin
                        EndPoint  := StartPoint;
                        Control2  := StartPoint;
                        Opposite2 := StartPoint;
                      end;
                    end;
                end;
              end;
            end;
          end
          else
          // if there are more than one curve segments in the list...
          if Self.Count > 1 then
          begin
            case LSelectedHandle of
              pshStart: // the path is not closed and the mouse is point to the start point of the first curve segment
                begin
                  DeleteCurveSegmentByIndex(0);
                  
                  FCurrentSegment             := nil;
                  FCurrentSegment             := TgmCurveSegment(Self.Items[0]);
                  FCurrentSegment.IsActive    := True;
                  FCurrentSegment.ActivePoint := apStart;
                  FCurrentIndex               := 0;
                  FSelectedCount              := 1;
                end;

              pshEnd: // the path is not closed and the mouse is point to the end point of the last curve segment
                begin
                  DeleteCurveSegmentByIndex(Self.Count - 1);
                  
                  FCurrentSegment             := nil;
                  FCurrentSegment             := TgmCurveSegment(Self.Items[Self.Count - 1]);
                  FCurrentSegment.IsActive    := True;
                  FCurrentSegment.ActivePoint := apEnd;
                  FCurrentIndex               := Self.Count - 1;
                  FSelectedCount              := 1;
                end;
            end;
          end;
        end;

        // if there are two curve segments are selected...
        if FSelectedCount = 2 then
        begin
          case LSelectedHandle of
            pshStart: // the path is closed and the mouse is point to the start point of the first curve segment
              begin
                DeleteCurveSegmentByIndex(0);
                Dec(FCurrentIndex);

                if Self.Count = 1 then
                begin
                  with FCurrentSegment do
                  begin
                    EndPoint              := StartPoint;
                    Control2              := Opposite1;
                    Opposite2             := Control1;
                    ActivePoint           := apStart;
                    Control2PairState     := Control1PairState;
                    IsControl2PairChanged := IsControl1PairChanged;
                  end;

                  FSelectedCount := 1;
                end
                else
                if Self.Count > 1 then
                begin
                  FNextSegment                          := nil;
                  FNextSegment                          := TgmCurveSegment(Self.Items[0]);
                  FNextSegment.IsActive                 := True;
                  FNextSegment.ActivePoint              := apStart;
                  FCurrentSegment.EndPoint              := FNextSegment.StartPoint;
                  FCurrentSegment.Opposite2             := FNextSegment.Control1;
                  FCurrentSegment.Control2              := FNextSegment.Opposite1;
                  FCurrentSegment.EndingPointType       := FNextSegment.StartingPointType;
                  FCurrentSegment.Control2PairState     := FNextSegment.Control1PairState;
                  FCurrentSegment.IsControl2PairChanged := FNextSegment.IsControl1PairChanged;
                  FSelectedCount                        := 2;
                end;
              end;

            // Merge the end point of the first selected curve segment with the
            // end point of the following one, and delete the second selected
            // curve segment. After deletion, if there is successor of the current
            // curve segment, then select it as the second selected curve segment. 
            pshEnd:
              begin
                FCurrentSegment.EndPoint              := FNextSegment.EndPoint;
                FCurrentSegment.Control2              := FNextSegment.Control2;
                FCurrentSegment.Opposite2             := FNextSegment.Opposite2;
                FCurrentSegment.EndingPointType       := FNextSegment.EndingPointType;
                FCurrentSegment.Control2PairState     := FNextSegment.Control2PairState;
                FCurrentSegment.IsControl2PairChanged := FNextSegment.IsControl2PairChanged;

                DeleteCurveSegmentByIndex(FCurrentIndex + 1);

                if Self.Count = 1 then
                begin
                  if FClosed then
                  begin
                    FCurrentSegment.ActivePoint := apStart;
                  end;

                  FSelectedCount := 1;
                end
                else
                if Self.Count > 1 then
                begin
                  // if there is successor and the path is not closed, then select it
                  if FCurrentIndex < (Self.Count - 1) then
                  begin
                    FNextSegment := nil;
                    FNextSegment := TgmCurveSegment(Self.Items[FCurrentIndex + 1]);
                  end
                  else
                  // After deletion, if there are more than one curve segments
                  // in the list, and the current selected one is the last curve
                  // segment, then select the first curve segment as the second
                  // selected curve. 
                  if FCurrentIndex = (Self.Count - 1) then
                  begin
                    FNextSegment := nil;
                    FNextSegment := TgmCurveSegment(Self.Items[0]);
                  end;

                  FNextSegment.IsActive    := True;
                  FNextSegment.ActivePoint := apStart;
                  FSelectedCount           := 2;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DeactivateAllSegments;
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment             := TgmCurveSegment(Self.Items[i]);
      LSegment.IsActive    := False;
      LSegment.ActivePoint := apNone;
      FCurrentSegment      := nil;
      FNextSegment         := nil;
      FCurrentIndex        := -1;
      FSelectedCount       := 0;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveSegments(const ACanvas: TCanvas;
  const APenMode: TPenMode; const AOffsetVector: TPoint);
var
  LTempPenWidth: Integer;
  LTempPenColor: TColor;
  LTempPenStyle: TPenStyle;
  LTempPenMode : TPenMode;
  i            : Integer;
  LSegment     : TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clBlack;
      Pen.Style := psSolid;
      Pen.Mode  := pmNotXor;

      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);
        LSegment.DrawCurveSegment(ACanvas, AOffsetVector);
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveSegments(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  LTempPenWidth : Integer;
  LTempPenColor : TColor;
  LTempPenStyle : TPenStyle;
  LTempPenMode  : TPenMode;
  i             : Integer;
  LSegment      : TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clBlack;
      Pen.Style := psSolid;
      Pen.Mode  := APenMode;

      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);
        LSegment.DrawCurveSegment(ACanvas, ACoordConvertFunc);
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveDirectionLines(const ACanvas: TCanvas;
  const APenMode: TPenMode);
var
  LTempPenWidth : Integer;
  LTempPenColor : TColor;
  LTempPenStyle : TPenStyle;
  LTempPenMode  : TPenMode;
  LStartPoint   : TPoint;
  LEndPoint     : TPoint;
  LControl1     : TPoint;
  LControl2     : TPoint;
  LOpposite1    : TPoint;
  LOpposite2    : TPoint;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clGray;
      Pen.Style := psSolid;
      Pen.Mode  := pmNotXor;

      if FSelectedCount = 1 then
      begin
        LStartPoint := FCurrentSegment.StartPoint;
        LEndPoint   := FCurrentSegment.EndPoint;
        LControl1   := FCurrentSegment.Control1;
        LControl2   := FCurrentSegment.Control2;
        LOpposite1  := FCurrentSegment.Opposite1;
        LOpposite2  := FCurrentSegment.Opposite2;

        if FClosed then
        begin
          MoveTo(LStartPoint.X, LStartPoint.Y);
          LineTo(LControl1.X, LControl1.Y);

          MoveTo(LStartPoint.X, LStartPoint.Y);
          LineTo(LOpposite1.X, LOpposite1.Y);

          if DifferentCoordinate(FCurrentSegment.Control2, FCurrentSegment.Opposite1) then
          begin
            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LControl2.X, LControl2.Y);
          end;

          if DifferentCoordinate(FCurrentSegment.Opposite2, FCurrentSegment.Control1) then
          begin
            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LOpposite2.X, LOpposite2.Y);
          end;
        end
        else
        begin
          if FFirstSegmentOK = False then
          begin
            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LControl1.X, LControl1.Y);

            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LOpposite1.X, LOpposite1.Y);
          end
          else
          begin
            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LControl1.X, LControl1.Y);

            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LControl2.X, LControl2.Y);

            case FCurrentSegment.ActivePoint of
              apStart:
                begin
                  MoveTo(LStartPoint.X, LStartPoint.Y);
                  LineTo(LOpposite1.X, LOpposite1.Y);
                end;
                
              apEnd:
                begin
                  if FCurrentSegment.StartingPointType = eptCornerPoint then
                  begin
                    MoveTo(LStartPoint.X, LStartPoint.Y);
                    LineTo(LOpposite1.X, LOpposite1.Y);
                  end;

                  if FCurrentSegment.EndingPointType = eptAnchorPoint then
                  begin
                    MoveTo(LEndPoint.X, LEndPoint.Y);
                    LineTo(LOpposite2.X, LOpposite2.Y);
                  end;
                end;
            end;
          end;
        end;
      end;

      if FSelectedCount = 2 then
      begin
        LStartPoint := FCurrentSegment.StartPoint;
        LEndPoint   := FCurrentSegment.EndPoint;
        LControl1   := FCurrentSegment.Control1;
        LControl2   := FCurrentSegment.Control2;

        MoveTo(LStartPoint.X, LStartPoint.Y);
        LineTo(LControl1.X, LControl1.Y);

        MoveTo(LEndPoint.X, LEndPoint.Y);
        LineTo(LControl2.X, LControl2.Y);

        LStartPoint := FNextSegment.StartPoint;
        LEndPoint   := FNextSegment.EndPoint;
        LControl1   := FNextSegment.Control1;
        LControl2   := FNextSegment.Control2;

        MoveTo(LStartPoint.X, LStartPoint.Y);
        LineTo(LControl1.X, LControl1.Y);

        MoveTo(LEndPoint.X, LEndPoint.Y);
        LineTo(LControl2.X, LControl2.Y);
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveDirectionLines(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  LTempPenWidth : Integer;
  LTempPenColor : TColor;
  LTempPenStyle : TPenStyle;
  LTempPenMode  : TPenMode;
  LStartPoint   : TPoint;
  LEndPoint     : TPoint;
  LControl1     : TPoint;
  LControl2     : TPoint;
  LOpposite1    : TPoint;
  LOpposite2    : TPoint;
begin
  if Self.Count > 0 then
  begin
    with ACanvas do
    begin
      LTempPenWidth := Pen.Width;
      LTempPenColor := Pen.Color;
      LTempPenStyle := Pen.Style;
      LTempPenMode  := Pen.Mode;

      Pen.Width := 1;
      Pen.Color := clGray;
      Pen.Style := psSolid;
      Pen.Mode  := APenMode;

      if FSelectedCount = 1 then
      begin
        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint := ACoordConvertFunc(FCurrentSegment.StartPoint);
          LEndPoint   := ACoordConvertFunc(FCurrentSegment.EndPoint);
          LControl1   := ACoordConvertFunc(FCurrentSegment.Control1);
          LControl2   := ACoordConvertFunc(FCurrentSegment.Control2);
          LOpposite1  := ACoordConvertFunc(FCurrentSegment.Opposite1);
          LOpposite2  := ACoordConvertFunc(FCurrentSegment.Opposite2);
        end
        else
        begin
          LStartPoint := FCurrentSegment.StartPoint;
          LEndPoint   := FCurrentSegment.EndPoint;
          LControl1   := FCurrentSegment.Control1;
          LControl2   := FCurrentSegment.Control2;
          LOpposite1  := FCurrentSegment.Opposite1;
          LOpposite2  := FCurrentSegment.Opposite2;
        end;

        if FClosed then
        begin
          MoveTo(LStartPoint.X, LStartPoint.Y);
          LineTo(LControl1.X, LControl1.Y);

          MoveTo(LStartPoint.X, LStartPoint.Y);
          LineTo(LOpposite1.X, LOpposite1.Y);

          if DifferentCoordinate(FCurrentSegment.Control2, FCurrentSegment.Opposite1) then
          begin
            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LControl2.X, LControl2.Y);
          end;

          if DifferentCoordinate(FCurrentSegment.Opposite2, FCurrentSegment.Control1) then
          begin
            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LOpposite2.X, LOpposite2.Y);
          end;
        end
        else
        begin
          if FFirstSegmentOK = False then
          begin
            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LControl1.X, LControl1.Y);

            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LOpposite1.X, LOpposite1.Y);
          end
          else
          begin
            MoveTo(LStartPoint.X, LStartPoint.Y);
            LineTo(LControl1.X, LControl1.Y);

            MoveTo(LEndPoint.X, LEndPoint.Y);
            LineTo(LControl2.X, LControl2.Y);

            case FCurrentSegment.ActivePoint of
              apStart:
                begin
                  MoveTo(LStartPoint.X, LStartPoint.Y);
                  LineTo(LOpposite1.X, LOpposite1.Y);
                end;
                
              apEnd:
                begin
                  if FCurrentSegment.StartingPointType = eptCornerPoint then
                  begin
                    MoveTo(LStartPoint.X, LStartPoint.Y);
                    LineTo(LOpposite1.X, LOpposite1.Y);
                  end;

                  if FCurrentSegment.EndingPointType = eptAnchorPoint then
                  begin
                    MoveTo(LEndPoint.X, LEndPoint.Y);
                    LineTo(LOpposite2.X, LOpposite2.Y);
                  end;
                end;
            end;
          end;
        end;
      end;

      if FSelectedCount = 2 then
      begin
        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint := ACoordConvertFunc(FCurrentSegment.StartPoint);
          LEndPoint   := ACoordConvertFunc(FCurrentSegment.EndPoint);
          LControl1   := ACoordConvertFunc(FCurrentSegment.Control1);
          LControl2   := ACoordConvertFunc(FCurrentSegment.Control2);
        end
        else
        begin
          LStartPoint := FCurrentSegment.StartPoint;
          LEndPoint   := FCurrentSegment.EndPoint;
          LControl1   := FCurrentSegment.Control1;
          LControl2   := FCurrentSegment.Control2;
        end;

        MoveTo(LStartPoint.X, LStartPoint.Y);
        LineTo(LControl1.X, LControl1.Y);

        MoveTo(LEndPoint.X, LEndPoint.Y);
        LineTo(LControl2.X, LControl2.Y);

        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint := ACoordConvertFunc(FNextSegment.StartPoint);
          LEndPoint   := ACoordConvertFunc(FNextSegment.EndPoint);
          LControl1   := ACoordConvertFunc(FNextSegment.Control1);
          LControl2   := ACoordConvertFunc(FNextSegment.Control2);
        end
        else
        begin
          LStartPoint := FNextSegment.StartPoint;
          LEndPoint   := FNextSegment.EndPoint;
          LControl1   := FNextSegment.Control1;
          LControl2   := FNextSegment.Control2;
        end;

        MoveTo(LStartPoint.X, LStartPoint.Y);
        LineTo(LControl1.X, LControl1.Y);

        MoveTo(LEndPoint.X, LEndPoint.Y);
        LineTo(LControl2.X, LControl2.Y);
      end;

      Pen.Width := LTempPenWidth;
      Pen.Color := LTempPenColor;
      Pen.Style := LTempPenStyle;
      Pen.Mode  := LTempPenMode;
    end;
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveHandles(const ACanvas: TCanvas;
  const APenMode: TPenMode);
begin
  DrawCurveEndingHandles(ACanvas, APenMode);
  
  if not FSelectedAll then
  begin
    DrawCurveDirectionHandles(ACanvas, APenMode);
  end;
end;

procedure TgmCurveSegmentsList.DrawCurveHandles(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
begin
  DrawCurveEndingHandles(ACanvas, APenMode, ACoordConvertFunc);
  
  if not FSelectedAll then
  begin
    DrawCurveDirectionHandles(ACanvas, APenMode, ACoordConvertFunc);
  end;
end;

function TgmCurveSegmentsList.NilsIfPointOnBezier(
  const AX, AY: Integer): Boolean;
var
  i, LPobSteps : Integer;
  LBezier      : TsdBezier;
  LPoint       : TsdPoint;
  LSegment     : TgmCurveSegment;
begin
  Result   := False;
  LPoint.X := AX;
  LPoint.Y := AY;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment       := TgmCurveSegment(Self.Items[i]);
      LBezier.P[0].X := LSegment.StartPoint.X;
      LBezier.P[0].Y := LSegment.StartPoint.Y;
      LBezier.P[1].X := LSegment.Control1.X;
      LBezier.P[1].Y := LSegment.Control1.Y;
      LBezier.P[2].X := LSegment.Control2.X;
      LBezier.P[2].Y := LSegment.Control2.Y;
      LBezier.P[3].X := LSegment.EndPoint.X;
      LBezier.P[3].Y := LSegment.EndPoint.Y;
      
      if NilsHaeckIsPointOnBezier(LBezier, LPoint, LPobSteps, TOLERANCE_OVER_BEZIER) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TgmCurveSegmentsList.NilsIfPointOnBezier(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i, LPobSteps : Integer;
  LBezier      : TsdBezier;
  LPoint       : TsdPoint;
  LTempPoint   : TPoint;
  LSegment     : TgmCurveSegment;
begin
  Result   := False;
  LPoint.X := AX;
  LPoint.Y := AY;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if Assigned(ACoordConvertFunc) then
      begin
        LTempPoint     := ACoordConvertFunc(LSegment.StartPoint);
        LBezier.P[0].X := LTempPoint.X;
        LBezier.P[0].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.Control1);
        LBezier.P[1].X := LTempPoint.X;
        LBezier.P[1].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.Control2);
        LBezier.P[2].X := LTempPoint.X;
        LBezier.P[2].Y := LTempPoint.Y;

        LTempPoint     := ACoordConvertFunc(LSegment.EndPoint);
        LBezier.P[3].X := LTempPoint.X;
        LBezier.P[3].Y := LTempPoint.Y;
      end
      else
      begin
        LBezier.P[0].X := LSegment.StartPoint.X;
        LBezier.P[0].Y := LSegment.StartPoint.Y;
        LBezier.P[1].X := LSegment.Control1.X;
        LBezier.P[1].Y := LSegment.Control1.Y;
        LBezier.P[2].X := LSegment.Control2.X;
        LBezier.P[2].Y := LSegment.Control2.Y;
        LBezier.P[3].X := LSegment.EndPoint.X;
        LBezier.P[3].Y := LSegment.EndPoint.Y;
      end;
      
      if NilsHaeckIsPointOnBezier(LBezier, LPoint, LPobSteps, TOLERANCE_OVER_BEZIER) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

// determine whether the mouse is point on the start point of the first curve segment
function TgmCurveSegmentsList.PointOnFirstSegmentStart(
  const AX, AY: Integer): Boolean;
var
  LSegment : TgmCurveSegment;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    LSegment := TgmCurveSegment(Self.Items[0]);
    
    Result := SquareContainsPoint( LSegment.StartPoint,
                                   PATH_HANDLE_RADIUS,
                                   Point(AX, AY) );
  end;
end;

function TgmCurveSegmentsList.PointOnFirstSegmentStart(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  LSegment    : TgmCurveSegment;
  LStartPoint : TPoint;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    LSegment := TgmCurveSegment(Self.Items[0]);

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(LSegment.StartPoint);
    end
    else
    begin
      LStartPoint := LSegment.StartPoint;
    end;
    
    Result := SquareContainsPoint( LStartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) );
  end;
end;

// Determine whether the mouse is point on the end point of the last curve
// segment, is so, return the last curve segment, otherwise return nil. 
function TgmCurveSegmentsList.PointOnLastSegmentEndingPoint(
  const AX, AY: Integer): TgmCurveSegment;
var
  LSegment : TgmCurveSegment;
begin
  Result := nil;

  if Self.Count > 0 then
  begin
    LSegment := TgmCurveSegment(Self.Items[Self.Count - 1]);

    if SquareContainsPoint(LSegment.EndPoint, PATH_HANDLE_RADIUS, Point(AX, AY)) then
    begin
      Result := LSegment;
    end;
  end;
end;

function TgmCurveSegmentsList.PointOnLastSegmentEndingPoint(
  const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmCurveSegment;
var
  LSegment  : TgmCurveSegment;
  LEndPoint : TPoint;
begin
  Result := nil;

  if Self.Count > 0 then
  begin
    LSegment := TgmCurveSegment(Self.Items[Self.Count - 1]);

    if Assigned(ACoordConvertFunc) then
    begin
      LEndPoint := ACoordConvertFunc(LSegment.EndPoint);
    end
    else
    begin
      LEndPoint := LSegment.EndPoint;
    end;

    if SquareContainsPoint(LEndPoint, PATH_HANDLE_RADIUS, Point(AX, AY)) then
    begin
      Result := LSegment;
    end;
  end;
end;

// determine whether the mouse is point on a handle of direction line of a
// selected curve segment
function TgmCurveSegmentsList.GetSelectedDirectionHandle(
  const AX, AY: Integer): TgmPathSelectHandle;
begin
  Result := pshNone;
  
  if FSelectedCount > 0 then
  begin
    if SquareContainsPoint( FCurrentSegment.Control1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl1;
      Exit;
    end;

    if SquareContainsPoint( FCurrentSegment.Control2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl2;
      Exit;
    end;
  end;

  if FSelectedCount = 1 then
  begin
    case FCurrentSegment.ActivePoint of
      apStart:
        begin
          if SquareContainsPoint( FCurrentSegment.Opposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
          begin
            Result := pshOpposite1;
          end;
        end;

      apEnd:
        begin
          if SquareContainsPoint( FCurrentSegment.Opposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
          begin
            Result := pshOpposite2;
          end;
        end;
    end;

    if FClosed then
    begin
      if SquareContainsPoint( FCurrentSegment.Opposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := pshOpposite1;
        Exit;
      end;

      if SquareContainsPoint( FCurrentSegment.Opposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := pshOpposite2;
        Exit;
      end;
    end;
  end;

  if FSelectedCount = 2 then
  begin
    if SquareContainsPoint( FNextSegment.Control1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl1;
      Exit;
    end;

    if SquareContainsPoint( FNextSegment.Control2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl2;
    end;
  end;
end;

function TgmCurveSegmentsList.GetSelectedDirectionHandle(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle;
var
  LControl1  : TPoint;
  LControl2  : TPoint;
  LOpposite1 : TPoint;
  LOpposite2 : TPoint;
begin
  Result := pshNone;
  
  if FSelectedCount > 0 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LControl1 := ACoordConvertFunc(FCurrentSegment.Control1);
      LControl2 := ACoordConvertFunc(FCurrentSegment.Control2);
    end
    else
    begin
      LControl1 := FCurrentSegment.Control1;
      LControl2 := FCurrentSegment.Control2;
    end;

    if SquareContainsPoint( LControl1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl1;
      Exit;
    end;

    if SquareContainsPoint( LControl2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl2;
      Exit;
    end;
  end;

  if FSelectedCount = 1 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LOpposite1 := ACoordConvertFunc(FCurrentSegment.Opposite1);
      LOpposite2 := ACoordConvertFunc(FCurrentSegment.Opposite2);
    end
    else
    begin
      LOpposite1 := FCurrentSegment.Opposite1;
      LOpposite2 := FCurrentSegment.Opposite2;
    end;

    case FCurrentSegment.ActivePoint of
      apStart:
        begin
          if SquareContainsPoint( LOpposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
          begin
            Result := pshOpposite1;
          end;
        end;

      apEnd:
        begin
          if SquareContainsPoint( LOpposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
          begin
            Result := pshOpposite2;
          end;
        end;
    end;

    if FClosed then
    begin
      if SquareContainsPoint( LOpposite1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := pshOpposite1;
        Exit;
      end;

      if SquareContainsPoint( LOpposite2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := pshOpposite2;
        Exit;
      end;
    end;
  end;

  if FSelectedCount = 2 then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LControl1 := ACoordConvertFunc(FNextSegment.Control1);
      LControl2 := ACoordConvertFunc(FNextSegment.Control2);
    end
    else
    begin
      LControl1 := FNextSegment.Control1;
      LControl2 := FNextSegment.Control2;
    end;

    if SquareContainsPoint( LControl1, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl1;
      Exit;
    end;

    if SquareContainsPoint( LControl2, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
    begin
      Result := pshControl2;
    end;
  end;
end;

// determine whether the mouse is point on a end point of a curve segment, not a control point
function TgmCurveSegmentsList.GetSelectedEndingHandle(
  const AX, AY: Integer): TgmPathSelectHandle;
var
  i       : Integer;
  LHandle : TgmPathSelectHandle;
  LSegment: TgmCurveSegment;
begin
  LHandle := pshNone;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if SquareContainsPoint(LSegment.StartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        LHandle := pshStart;
      end
      else
      if SquareContainsPoint(LSegment.EndPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        LHandle := pshEnd;
      end;

      if LHandle <> pshNone then
      begin
        Break;
      end;
    end;
  end;
  
  Result := LHandle;
end;

// determine whether the mouse is point on a end point of a curve segment,
// not a control point
function TgmCurveSegmentsList.GetSelectedEndingHandle(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle;
var
  i           : Integer;
  LHandle     : TgmPathSelectHandle;
  LSegment    : TgmCurveSegment;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  LHandle := pshNone;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if Assigned(ACoordConvertFunc) then
      begin
        LStartPoint := ACoordConvertFunc(LSegment.StartPoint);
        LEndPoint   := ACoordConvertFunc(LSegment.EndPoint);
      end
      else
      begin
        LStartPoint := LSegment.StartPoint;
        LEndPoint   := LSegment.EndPoint;
      end;

      if SquareContainsPoint(LStartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        LHandle := pshStart;
      end
      else
      if SquareContainsPoint(LEndPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        LHandle := pshEnd;
      end;

      if LHandle <> pshNone then
      begin
        Break;
      end;
    end;
  end;
  
  Result := LHandle;
end;

// determine whether the mouse is point on any handles
function TgmCurveSegmentsList.GetSelectHandle(
  const AX, AY: Integer): TgmPathSelectHandle;
var
  LHandle : TgmPathSelectHandle;
begin
  LHandle := pshNone;

  if Self.Count > 0 then
  begin
    if FSelected or FSelectedAll then
    begin
      LHandle := GetSelectedEndingHandle(AX, AY);
    end;

    if FSelected then
    begin
      if LHandle = pshNone then
      begin
        if FSelectedCount > 0 then
        begin
          LHandle := GetSelectedDirectionHandle(AX, AY);
        end;
      end;
    end;
  end;
  
  Result := LHandle;
end;

function TgmCurveSegmentsList.GetSelectHandle(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmPathSelectHandle;
var
  LHandle : TgmPathSelectHandle;
begin
  LHandle := pshNone;

  if Self.Count > 0 then
  begin
    if FSelected or FSelectedAll then
    begin
      LHandle := GetSelectedEndingHandle(AX, AY, ACoordConvertFunc);
    end;

    if FSelected then
    begin
      if LHandle = pshNone then
      begin
        if FSelectedCount > 0 then
        begin
          LHandle := GetSelectedDirectionHandle(AX, AY, ACoordConvertFunc);
        end;
      end;
    end;
  end;
  
  Result := LHandle;
end;

function TgmCurveSegmentsList.GetEndingPointType(
  const AX, AY: Integer): TgmEndingPointType;
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  Result := eptNone;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if SquareContainsPoint( LSegment.StartPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := LSegment.StartingPointType;
        Break;
      end
      else
      if SquareContainsPoint( LSegment.EndPoint, PATH_HANDLE_RADIUS, Point(AX, AY) ) then
      begin
        Result := LSegment.EndingPointType;
        Break;
      end;
    end;
  end;
end;

function TgmCurveSegmentsList.GetSelectedSegmentsPairState: TgmPairState;
begin
  Result := psUnknown;
  
  if Self.Count > 0 then
  begin
    if FSelectedCount = 1 then
    begin
      with FCurrentSegment do
      begin
        case ActivePoint of
          apStart:
            begin
              Result := Control1PairState;
            end;

          apEnd:
            begin
              Result := Control2PairState;
            end;
        end;
      end;
    end
    else
    if FSelectedCount = 2 then
    begin
      Result := FCurrentSegment.Control2PairState;
    end;
  end;
end;

function TgmCurveSegmentsList.GetCurrentSegmentIndexInList: Integer;
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  Result := -1;

  if Self.Count > 0 then
  begin
    if Assigned(FCurrentSegment) then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);
        
        if FCurrentSegment = LSegment then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TgmCurveSegmentsList.GetNextSegmentIndexInList: Integer;
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  Result := -1;

  if Self.Count > 0 then
  begin
    if Assigned(FNextSegment) then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);
        
        if FNextSegment = LSegment then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TgmCurveSegmentsList.ChangeSelectedHandlePosition(
  const AHandle: TgmPathSelectHandle; const ATranslateVector: TPoint;
  const AOperation: TgmOppositeLineOperation);
var
  LRadian: Double;
begin
  if FSelectedCount > 0 then
  begin
    if FSelectedCount = 1 then
    begin
      if FClosed then
      begin
        case AHandle of
          pshStart:
            begin
              with FCurrentSegment do
              begin
                StartPoint := AddPoints(StartPoint, ATranslateVector);
                Control1   := AddPoints(Control1,   ATranslateVector);
                Opposite1  := AddPoints(Opposite1,  ATranslateVector);
                EndPoint   := AddPoints(EndPoint,   ATranslateVector);
                Control2   := AddPoints(Control2,   ATranslateVector);
                Opposite2  := AddPoints(Opposite2,  ATranslateVector);
              end;
            end;

          pshControl1:
            begin
              with FCurrentSegment do
              begin
                Control1 := AddPoints(Control1, ATranslateVector);

                case AOperation of
                  oloChangeAngleOnly:
                    begin
                      if Control1PairState = psPaired then
                      begin
                        Control1Radian := CalcRadianByTwoPoints(StartPoint, Control1);
                        LRadian        := Control1Radian - RadianDifference1;
                        Opposite1      := CalcOffsetPointByRadian(StartPoint, LRadian, Opposite1Length);
                      end;
                    end;

                  oloAbsoluteOpposite:
                    begin
                      Opposite1 := GetOppositePoint(StartPoint, Control1);
                    end;
                end;
              end;
            end;

          pshOpposite1:
            begin
              with FCurrentSegment do
              begin
                Opposite1 := AddPoints(Opposite1, ATranslateVector);

                if Control1PairState = psPaired then
                begin
                  Opposite1Radian := CalcRadianByTwoPoints(StartPoint, Opposite1);
                  LRadian         := Opposite1Radian + RadianDifference1;
                  Control1        := CalcOffsetPointByRadian(StartPoint, LRadian, Control1Length);
                end;
              end;
            end;

          pshControl2:
            begin
              with FCurrentSegment do
              begin
                Control2 := AddPoints(Control2, ATranslateVector);

                if Control2PairState = psPaired then
                begin
                  Control2Radian := CalcRadianByTwoPoints(EndPoint, Control2);
                  LRadian        := Control2Radian - RadianDifference2;
                  Opposite2      := CalcOffsetPointByRadian(EndPoint, LRadian, Opposite2Length);
                end;
              end;
            end;

          pshOpposite2:
            begin
              with FCurrentSegment do
              begin
                Opposite2 := AddPoints(Opposite2, ATranslateVector);

                case AOperation of
                  oloChangeAngleOnly:
                    begin
                      if Control2PairState = psPaired then
                      begin
                        Opposite2Radian := CalcRadianByTwoPoints(EndPoint, Opposite2);
                        LRadian         := Opposite2Radian + RadianDifference2;
                        Control2        := CalcOffsetPointByRadian(EndPoint, LRadian, Control2Length);
                      end;
                    end;

                  oloAbsoluteOpposite:
                    begin
                      Control2 := GetOppositePoint(EndPoint, Opposite2);
                    end;
                end;
              end;
            end;
        end;
      end
      else
      begin
        if FFirstSegmentOK = False then
        begin
          case AHandle of
            pshStart:
              begin
                with FCurrentSegment do
                begin
                  StartPoint := AddPoints(StartPoint, ATranslateVector);
                  Control1   := AddPoints(Control1,   ATranslateVector);
                  Opposite1  := AddPoints(Opposite1,  ATranslateVector);
                  EndPoint   := AddPoints(EndPoint,   ATranslateVector);
                  Control2   := AddPoints(Control2,   ATranslateVector);
                  Opposite2  := AddPoints(Opposite2,  ATranslateVector);
                end;
              end;

            pshControl1:
              begin
                with FCurrentSegment do
                begin
                  Control1 := AddPoints(Control1, ATranslateVector);

                  case AOperation of
                    oloChangeAngleOnly:
                      begin
                        if Control1PairState = psPaired then
                        begin
                          Control1Radian := CalcRadianByTwoPoints(StartPoint, Control1);
                          LRadian        := Control1Radian - RadianDifference1;
                          Opposite1      := CalcOffsetPointByRadian(StartPoint, LRadian, Opposite1Length);
                        end;
                      end;

                    oloAbsoluteOpposite:
                      begin
                        Opposite1 := GetOppositePoint(StartPoint, Control1);
                      end;
                  end;
                end;
              end;

            pshOpposite1:
              begin
                with FCurrentSegment do
                begin
                  Opposite1 := AddPoints(Opposite1, ATranslateVector);

                  if Control1PairState = psPaired then
                  begin
                    Opposite1Radian := CalcRadianByTwoPoints(StartPoint, Opposite1);
                    LRadian         := Opposite1Radian + RadianDifference1;
                    Control1        := CalcOffsetPointByRadian(StartPoint, LRadian, Control1Length);
                  end;
                end;
              end;
          end;
        end
        else
        begin
          case AHandle of
            pshStart:
              begin
                with FCurrentSegment do
                begin
                  StartPoint := AddPoints(StartPoint, ATranslateVector);
                  Control1   := AddPoints(Control1,   ATranslateVector);
                  Opposite1  := AddPoints(Opposite1,  ATranslateVector);
                end;
              end;

            pshControl1:
              begin
                with FCurrentSegment do
                begin
                  Control1 := AddPoints(Control1, ATranslateVector);

                  case AOperation of
                    oloChangeAngleOnly:
                      begin
                        if Control1PairState = psPaired then
                        begin
                          Control1Radian := CalcRadianByTwoPoints(StartPoint, Control1);
                          LRadian        := Control1Radian - RadianDifference1;
                          Opposite1      := CalcOffsetPointByRadian(StartPoint, LRadian, Opposite1Length);
                        end;
                      end;

                    oloAbsoluteOpposite:
                      begin
                        Opposite1 := GetOppositePoint(StartPoint, Control1);
                      end;
                  end;
                end;
              end;

            pshOpposite1:
              begin
                with FCurrentSegment do
                begin
                  Opposite1 := AddPoints(Opposite1, ATranslateVector);

                  if Control1PairState = psPaired then
                  begin
                    Opposite1Radian := CalcRadianByTwoPoints(StartPoint, Opposite1);
                    LRadian         := Opposite1Radian + RadianDifference1;
                    Control1        := CalcOffsetPointByRadian(StartPoint, LRadian, Control1Length);
                  end;
                end;
              end;

            pshEnd:
              begin
                with FCurrentSegment do
                begin
                  EndPoint  := AddPoints(EndPoint,  ATranslateVector);
                  Control2  := AddPoints(Control2,  ATranslateVector);
                  Opposite2 := AddPoints(Opposite2, ATranslateVector);
                end;
              end;

            pshControl2:
              begin
                with FCurrentSegment do
                begin
                  Control2 := AddPoints(Control2, ATranslateVector);

                  if Control2PairState = psPaired then
                  begin
                    Control2Radian := CalcRadianByTwoPoints(EndPoint, Control2);
                    LRadian        := Control2Radian - RadianDifference2;
                    Opposite2      := CalcOffsetPointByRadian(EndPoint, LRadian, Opposite2Length);
                  end;
                end;
              end;

            pshOpposite2:
              begin
                with FCurrentSegment do
                begin
                  Opposite2 := AddPoints(Opposite2, ATranslateVector);

                  case AOperation of
                    oloChangeAngleOnly:
                      begin
                        if Control2PairState = psPaired then
                        begin
                          Opposite2Radian := CalcRadianByTwoPoints(EndPoint, Opposite2);
                          LRadian         := Opposite2Radian + RadianDifference2;
                          Control2        := CalcOffsetPointByRadian(EndPoint, LRadian, Control2Length);
                        end;
                      end;
                      
                    oloAbsoluteOpposite:
                      begin
                        Control2 := GetOppositePoint(EndPoint, Opposite2);
                      end;
                  end;
                end;
              end;
          end;
        end;
      end;
    end;

    if FSelectedCount = 2 then
    begin
      if FClosed then
      begin
        case AHandle of
          pshStart, pshEnd:
            begin
              with FCurrentSegment do
              begin
                EndPoint  := AddPoints(EndPoint,  ATranslateVector);
                Control2  := AddPoints(Control2,  ATranslateVector);
                Opposite2 := AddPoints(Opposite2, ATranslateVector);
              end;

              with FNextSegment do
              begin
                StartPoint := AddPoints(StartPoint, ATranslateVector);
                Control1   := AddPoints(Control1,   ATranslateVector);
                Opposite1  := AddPoints(Opposite1,  ATranslateVector);
              end;
            end;
            
          pshControl1:
            begin
              { After the path is closed, if the returned handle is shControl1,
                it indicates that the FControl1 of the FNextSegment is selected,
                which relative to the FOpposite2 of the FCurrentSegment, because
                at this point, the FCurrentSegment is the last curve and the
                FNextSegment is the first curve. }

              FCurrentSegment.Opposite2 := AddPoints(FCurrentSegment.Opposite2, ATranslateVector);

              if FCurrentSegment.Control2PairState = psPaired then
              begin
                FCurrentSegment.Opposite2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2);
                LRadian                         := FCurrentSegment.Opposite2Radian + FCurrentSegment.RadianDifference2;
                FCurrentSegment.Control2        := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Control2Length);
              end;

              if FCurrentSegment.EndingPointType = eptAnchorPoint then
              begin
                FNextSegment.Control1  := FCurrentSegment.Opposite2;
                FNextSegment.Opposite1 := FCurrentSegment.Control2;
              end;
            end;
            
          pshOpposite1:
            begin
              { After the path is closed, if the returned handle is shOpposite1,
                it indicates that the FOpposite1 of the FNextSegment is selected,
                which relative to the FControl2 of the FCurrentSegment, because
                at this point, the FCurrentSegment is the last curve and the
                FNextSegment is the first curve. }

              FCurrentSegment.Control2 := AddPoints(FCurrentSegment.Control2, ATranslateVector);

              if FCurrentSegment.Control2PairState = psPaired then
              begin
                FCurrentSegment.Control2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Control2);
                LRadian                        := FCurrentSegment.Control2Radian - FCurrentSegment.RadianDifference2;
                FCurrentSegment.Opposite2      := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Opposite2Length);
              end;

              case FCurrentSegment.EndingPointType of
                eptAnchorPoint:
                  begin
                    FNextSegment.Control1  := FCurrentSegment.Opposite2;
                    FNextSegment.Opposite1 := FCurrentSegment.Control2;
                  end;

                eptCornerPoint:
                  begin
                    FNextSegment.Opposite1 := FCurrentSegment.Control2;
                  end;
              end;
            end;

          pshControl2:
            begin
              FCurrentSegment.Control2 := AddPoints(FCurrentSegment.Control2, ATranslateVector);

              if FCurrentSegment.Control2PairState = psPaired then
              begin
                FCurrentSegment.Control2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Control2);
                LRadian                        := FCurrentSegment.Control2Radian - FCurrentSegment.RadianDifference2;
                FCurrentSegment.Opposite2      := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Opposite2Length);
              end;

              if FCurrentSegment.EndingPointType = eptAnchorPoint then
              begin
                FNextSegment.Control1  := FCurrentSegment.Opposite2;
                FNextSegment.Opposite1 := FCurrentSegment.Control2;
              end;
            end;

          pshOpposite2:
            begin
              FCurrentSegment.Opposite2 := AddPoints(FCurrentSegment.Opposite2, ATranslateVector);

              case AOperation of
                oloChangeAngleOnly:
                  begin
                    if FCurrentSegment.Control2PairState = psPaired then
                    begin
                      FCurrentSegment.Opposite2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2);
                      LRadian                         := FCurrentSegment.Opposite2Radian + FCurrentSegment.RadianDifference2;
                      FCurrentSegment.Control2        := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Control2Length);
                    end;
                  end;
                  
                oloAbsoluteOpposite:
                  begin
                    FCurrentSegment.Control2 := GetOppositePoint(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2);
                  end;
              end;

              if FCurrentSegment.EndingPointType = eptAnchorPoint then
              begin
                FNextSegment.Control1  := FCurrentSegment.Opposite2;
                FNextSegment.Opposite1 := FCurrentSegment.Control2;
              end;
            end;
        end;
      end
      else
      begin
        { If the path is not closed and there are two curve segments are selected,
          then we could only get shEnd, shControl2 and shOpposite2 control handles.}
        case AHandle of
          pshEnd:
            begin
              with FCurrentSegment do
              begin
                EndPoint  := AddPoints(EndPoint,  ATranslateVector);
                Control2  := AddPoints(Control2,  ATranslateVector);
                Opposite2 := AddPoints(Opposite2, ATranslateVector);
              end;

              with FNextSegment do
              begin
                StartPoint := AddPoints(StartPoint, ATranslateVector);
                Control1   := AddPoints(Control1,   ATranslateVector);
                Opposite1  := AddPoints(Opposite1,  ATranslateVector);
              end;
            end;

          pshControl2:
            begin
              FCurrentSegment.Control2 := AddPoints(FCurrentSegment.Control2, ATranslateVector);

              if FCurrentSegment.Control2PairState = psPaired then
              begin
                FCurrentSegment.Control2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Control2);
                LRadian                        := FCurrentSegment.Control2Radian - FCurrentSegment.RadianDifference2;
                FCurrentSegment.Opposite2      := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Opposite2Length);
              end;

              if FCurrentSegment.EndingPointType = eptAnchorPoint then
              begin
                FNextSegment.Control1  := FCurrentSegment.Opposite2;
                FNextSegment.Opposite1 := FCurrentSegment.Control2;
              end;
            end;

          pshOpposite2:
            begin
              FCurrentSegment.Opposite2 := AddPoints(FCurrentSegment.Opposite2, ATranslateVector);

              case AOperation of
                oloChangeAngleOnly:
                  begin
                    if FCurrentSegment.Control2PairState = psPaired then
                    begin
                      FCurrentSegment.Opposite2Radian := CalcRadianByTwoPoints(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2);
                      LRadian                         := FCurrentSegment.Opposite2Radian + FCurrentSegment.RadianDifference2;
                      FCurrentSegment.Control2        := CalcOffsetPointByRadian(FCurrentSegment.EndPoint, LRadian, FCurrentSegment.Control2Length);
                    end;
                  end;
                  
                oloAbsoluteOpposite:
                  begin
                    FCurrentSegment.Control2 := GetOppositePoint(FCurrentSegment.EndPoint, FCurrentSegment.Opposite2);
                  end;
              end;

              if FCurrentSegment.EndingPointType = eptAnchorPoint then
              begin
                FNextSegment.Control1  := FCurrentSegment.Opposite2;
                FNextSegment.Opposite1 := FCurrentSegment.Control2;
              end;
            end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurveSegmentsList.ChangeDirectionLinesPairState(
  const APairState: TgmPairState; const AIsSaveSetting: Boolean);
begin
  if Self.Count > 0 then
  begin
    if FSelectedCount = 1 then
    begin
      with FCurrentSegment do
      begin
        case ActivePoint of
          apStart:
            begin
              Control1PairState := APairState;

              if AIsSaveSetting then
              begin
                IsControl1PairChanged := True;
              end;
            end;

          apEnd:
            begin
              Control2PairState := APairState;

              if AIsSaveSetting then
              begin
                IsControl2PairChanged := True;
              end;
            end;
        end;
      end;
    end
    else
    if FSelectedCount = 2 then
    begin
      FCurrentSegment.Control2PairState := APairState;
      FNextSegment.Control1PairState    := APairState;

      if AIsSaveSetting then
      begin
        FCurrentSegment.IsControl2PairChanged := True;
        FNextSegment.IsControl1PairChanged    := True;
      end;
    end;
  end;
end;

// switch between Anchor Point and Corner Point
procedure TgmCurveSegmentsList.ConvertPoint(const AX, AY: Integer;
  const AOperation: TgmConvertOperation);
var
  LEndPointType : TgmEndingPointType;
begin
  LEndPointType := eptNone;
  
  if Self.Count > 0 then
  begin
    if GetSelectedEndingHandle(AX, AY) <> pshNone then
    begin
      case AOperation of
        coAnchorToCorner:
          begin
            LEndPointType := eptCornerPoint;
          end;

        coCornerToAnchor:
          begin
            LEndPointType := eptAnchorPoint;
          end;
      end;

      if FSelectedCount = 1 then
      begin
        if FClosed then
        begin
          with FCurrentSegment do
          begin
            StartingPointType     := LEndPointType;
            Control1              := StartPoint;
            Opposite1             := StartPoint;
            EndingPointType       := LEndPointType;
            Control2              := EndPoint;
            Opposite2             := EndPoint;
            Control1PairState     := psPaired;
            Control2PairState     := psPaired;
            IsControl1PairChanged := False;
            IsControl2PairChanged := False;
          end;
        end
        else
        begin
          case FCurrentSegment.ActivePoint of
            apStart:
              begin
                with FCurrentSegment do
                begin
                  StartingPointType     := LEndPointType;
                  Control1              := StartPoint;
                  Opposite1             := StartPoint;
                  Control1PairState     := psPaired;
                  IsControl1PairChanged := False;
                end;
              end;
              
            apEnd:
              begin
                with FCurrentSegment do
                begin
                  EndingPointType       := LEndPointType;
                  Control2              := EndPoint;
                  Opposite2             := EndPoint;
                  Control2PairState     := psPaired;
                  IsControl2PairChanged := False;
                end;
              end;
          end;
        end;
      end;

      if FSelectedCount = 2 then
      begin
        FCurrentSegment.EndingPointType := LEndPointType;
        FNextSegment.StartingPointType  := LEndPointType;

        if AOperation = coAnchorToCorner then
        begin
          with FCurrentSegment do
          begin
            Control2              := EndPoint;
            Opposite2             := EndPoint;
            Control2PairState     := psPaired;
            IsControl2PairChanged := False;
          end;

          with FNextSegment do
          begin
            Control1              := StartPoint;
            Opposite1             := StartPoint;
            Control1PairState     := psPaired;
            IsControl1PairChanged := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurveSegmentsList.ConvertPoint(const AX, AY: Integer;
  const AOperation: TgmConvertOperation;
  ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  LEndPointType : TgmEndingPointType;
begin
  LEndPointType := eptNone;
  
  if Self.Count > 0 then
  begin
    if GetSelectedEndingHandle(AX, AY, ACoordConvertFunc) <> pshNone then
    begin
      case AOperation of
        coAnchorToCorner:
          begin
            LEndPointType := eptCornerPoint;
          end;

        coCornerToAnchor:
          begin
            LEndPointType := eptAnchorPoint;
          end;
      end;

      if FSelectedCount = 1 then
      begin
        if FClosed then
        begin
          with FCurrentSegment do
          begin
            StartingPointType     := LEndPointType;
            Control1              := StartPoint;
            Opposite1             := StartPoint;
            EndingPointType       := LEndPointType;
            Control2              := EndPoint;
            Opposite2             := EndPoint;
            Control1PairState     := psPaired;
            Control2PairState     := psPaired;
            IsControl1PairChanged := False;
            IsControl2PairChanged := False;
          end;
        end
        else
        begin
          case FCurrentSegment.ActivePoint of
            apStart:
              begin
                with FCurrentSegment do
                begin
                  StartingPointType     := LEndPointType;
                  Control1              := StartPoint;
                  Opposite1             := StartPoint;
                  Control1PairState     := psPaired;
                  IsControl1PairChanged := False;
                end;
              end;
              
            apEnd:
              begin
                with FCurrentSegment do
                begin
                  EndingPointType       := LEndPointType;
                  Control2              := EndPoint;
                  Opposite2             := EndPoint;
                  Control2PairState     := psPaired;
                  IsControl2PairChanged := False;
                end;
              end;
          end;
        end;
      end;

      if FSelectedCount = 2 then
      begin
        FCurrentSegment.EndingPointType := LEndPointType;
        FNextSegment.StartingPointType  := LEndPointType;

        if AOperation = coAnchorToCorner then
        begin
          with FCurrentSegment do
          begin
            Control2              := EndPoint;
            Opposite2             := EndPoint;
            Control2PairState     := psPaired;
            IsControl2PairChanged := False;
          end;

          with FNextSegment do
          begin
            Control1              := StartPoint;
            Opposite1             := StartPoint;
            Control1PairState     := psPaired;
            IsControl1PairChanged := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurveSegmentsList.ClosePath;
var
  LCurveSegment: TgmCurveSegment;
  LFirstSegment: TgmCurveSegment;
  LLastSegment : TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    LFirstSegment := TgmCurveSegment(Self.Items[0]);
    LLastSegment  := TgmCurveSegment(Self.Items[Self.Count - 1]);
    LCurveSegment := TgmCurveSegment.Create;

    LCurveSegment.StartPoint := LLastSegment.EndPoint;

    case LLastSegment.EndingPointType of
      eptAnchorPoint:
        begin
          LCurveSegment.Control1  := LLastSegment.Opposite2;
          LCurveSegment.Opposite1 := LLastSegment.Control2;
        end;

      eptCornerPoint:
        begin
          LCurveSegment.Control1  := LLastSegment.EndPoint;
          LCurveSegment.Opposite1 := LLastSegment.EndPoint;
        end;
    end;

    case LFirstSegment.StartingPointType of
      eptAnchorPoint:
        begin
          LCurveSegment.Control2  := LFirstSegment.Opposite1;
          LCurveSegment.Opposite2 := LFirstSegment.Control1;
        end;

      eptCornerPoint:
        begin
          LCurveSegment.Control2        := LFirstSegment.Opposite1;
          LCurveSegment.Opposite2       := LFirstSegment.StartPoint;
          LCurveSegment.EndingPointType := eptCornerPoint;
        end;
    end;

    LCurveSegment.EndPoint := LFirstSegment.StartPoint;

    Self.Add(LCurveSegment);
    
    FClosed   := True;
    FSelected := False;
  end;
end;

procedure TgmCurveSegmentsList.TranslateAllSegments(
  const ATranslateVector: TPoint);
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);
      
      LSegment.Translate(ATranslateVector);
    end;
  end;
end;

procedure TgmCurveSegmentsList.ScaleAllSegments(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);
      
      LSegment.ScaleCurveSegment(AOldWidth, AOldHeight, ANewWidth, ANewHeight);
    end;
  end;
end;

procedure TgmCurveSegmentsList.MakeRegion(const ACanvas: TCanvas;
  const AOffsetVector: TPoint);
var
  i              : Integer;
  LSegment       : TgmCurveSegment;
  LCurvePoints   : array of TPoint;
  LRGN           : HRGN;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
begin
  SetLength(LCurvePoints, 0);

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LSegment := TgmCurveSegment(Self.Items[i]);

      if i = 0 then
      begin
        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.StartPoint, AOffsetVector);
      end;

      SetLength( LCurvePoints, High(LCurvePoints) + 2 );
      LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.Control1, AOffsetVector);

      SetLength( LCurvePoints, High(LCurvePoints) + 2 );
      LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.Control2, AOffsetVector);

      SetLength( LCurvePoints, High(LCurvePoints) + 2 );
      LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.EndPoint, AOffsetVector);
    end;

    BeginPath(ACanvas.Handle);
    ACanvas.PolyBezier(LCurvePoints);
    EndPath(ACanvas.Handle);
    
    LRGN := PathToRegion(ACanvas.Handle);

    // save the original brush settings
    LTempBrushColor := ACanvas.Brush.Color;
    LTempBrushStyle := ACanvas.Brush.Style;

    // change brush settings
    ACanvas.Brush.Color := clWhite;
    ACanvas.Brush.Style := bsSolid;

    // fill region
    FillRGN(ACanvas.Handle, LRGN, ACanvas.Brush.Handle);
    PaintRGN(ACanvas.Handle, LRGN);

    // restore brush settings
    ACanvas.Brush.Color := LTempBrushColor;
    ACanvas.Brush.Style := LTempBrushStyle;
  end;
end;

procedure TgmCurveSegmentsList.StrokePathWithForegroundColor(
  const ACanvas: TCanvas; const APenWidth: Integer;
  const APenColor, ABrushColor: TColor; const APenStyle: TPenStyle;
  const ABrushStyle: TBrushStyle; const AOffsetVector: TPoint);
var
  i, j           : Integer;
  LSegment       : TgmCurveSegment;
  LCurvePoints   : array of TPoint;
  LTempPenWidth  : Integer;
  LTempPenColor  : TColor;
  LTempPenStyle  : TPenStyle;
  LTempPenMode   : TPenMode;
  LTempBrushColor: TColor;
  LTempBrushStyle: TBrushStyle;
begin
  SetLength(LCurvePoints, 0);

  if FSelected or FSelectedAll then
  begin
    if Self.Count > 0 then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);

        if i = 0 then
        begin
          SetLength( LCurvePoints, High(LCurvePoints) + 2 );
          LCurvePoints[High(LCurvePoints)] := LSegment.StartPoint;
        end;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.Control1;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.Control2;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.EndPoint;
      end;

      // get coordinates with offset
      for j := Low(LCurvePoints) to High(LCurvePoints) do
      begin
        LCurvePoints[j] := AddPoints(LCurvePoints[j], AOffsetVector);
      end;

      BeginPath(ACanvas.Handle);
      ACanvas.PolyBezier(LCurvePoints);
      EndPath(ACanvas.Handle);

      // save the original canvas settings
      GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                          LTempPenStyle, LTempPenMode, LTempBrushStyle);

      // change canvas settings
      SetCanvasProperties(ACanvas, APenColor, ABrushColor, APenWidth, APenStyle,
                          pmCopy, ABrushStyle);

      StrokePath(ACanvas.Handle);

      // restore canvas settings
      SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                          LTempPenStyle, LTempPenMode, LTempBrushStyle);
    end;
  end;
end; 

procedure TgmCurveSegmentsList.FillPathWithForegroundColor(
  const ACanvas: TCanvas; const AColor: TColor; const ABrushStyle: TBrushStyle;
  const AOffsetVector: TPoint);
var
  i, j        : Integer;
  LSegment    : TgmCurveSegment;
  LCurvePoints: array of TPoint;
  LTempColor  : TColor;
  LTempStyle  : TBrushStyle;
begin
  SetLength(LCurvePoints, 0);

  if FSelected or FSelectedAll then
  begin
    if Self.Count > 0 then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);

        if i = 0 then
        begin
          SetLength( LCurvePoints, High(LCurvePoints) + 2 );
          LCurvePoints[High(LCurvePoints)] := LSegment.StartPoint;
        end;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.Control1;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.Control2;

        SetLength( LCurvePoints, High(LCurvePoints) + 2 );
        LCurvePoints[High(LCurvePoints)] := LSegment.EndPoint;
      end;

      // get coordinates with offset
      for j := Low(LCurvePoints) to High(LCurvePoints) do
      begin
        LCurvePoints[j] := AddPoints(LCurvePoints[j], AOffsetVector);
      end;

      BeginPath(ACanvas.Handle);
      ACanvas.PolyBezier(LCurvePoints);
      EndPath(ACanvas.Handle);

      LTempColor := ACanvas.Brush.Color;
      LTempStyle := ACanvas.Brush.Style;

      ACanvas.Brush.Color := AColor;
      ACanvas.Brush.Style := ABrushStyle;

      FillPath(ACanvas.Handle);

      ACanvas.Brush.Color := LTempColor;
      ACanvas.Brush.Style := LTempStyle;
    end;
  end;
end;

// calculate the radians and length according to the curve segments selected state
procedure TgmCurveSegmentsList.CalcRadLenForCurveSegments;
begin
  if Self.Count > 0 then
  begin
    if FSelectedCount = 1 then
    begin
      with FCurrentSegment do
      begin
        case ActivePoint of
          apStart:
            begin
              Control1Radian    := CalcRadianByTwoPoints(StartPoint, Control1);
              Opposite1Radian   := CalcRadianByTwoPoints(StartPoint, Opposite1);
              RadianDifference1 := Control1Radian - Opposite1Radian;
              Control1Length    := CalcLengthByTwoPoints(StartPoint, Control1);
              Opposite1Length   := CalcLengthByTwoPoints(StartPoint, Opposite1);
            end;
            
          apEnd:
            begin
              Control2Radian    := CalcRadianByTwoPoints(EndPoint, Control2);
              Opposite2Radian   := CalcRadianByTwoPoints(EndPoint, Opposite2);
              RadianDifference2 := Control2Radian - Opposite2Radian;
              Control2Length    := CalcLengthByTwoPoints(EndPoint, Control2);
              Opposite2Length   := CalcLengthByTwoPoints(EndPoint, Opposite2);
            end;
        end;
      end;
    end
    else
    if FSelectedCount = 2 then
    begin
      with FCurrentSegment do
      begin
        Control2Radian    := CalcRadianByTwoPoints(EndPoint, Control2);
        Opposite2Radian   := CalcRadianByTwoPoints(EndPoint, Opposite2);
        RadianDifference2 := Control2Radian - Opposite2Radian;
        Control2Length    := CalcLengthByTwoPoints(EndPoint, Control2);
        Opposite2Length   := CalcLengthByTwoPoints(EndPoint, Opposite2);
      end;

      with FNextSegment do
      begin
        Control1Radian    := FCurrentSegment.Opposite2Radian;
        Opposite1Radian   := FCurrentSegment.Control2Radian;
        RadianDifference1 := Control1Radian - Opposite1Radian;
        Control1Length    := FCurrentSegment.Opposite2Length;
        Opposite1Length   := FCurrentSegment.Control2Length;
      end;
    end;
  end;
end;

// whether the pair state is changed
function TgmCurveSegmentsList.IsPairStateChanged: Boolean;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    if FSelectedCount = 1 then
    begin
      with FCurrentSegment do
      begin
        case ActivePoint of
          apStart:
            begin
              Result := IsControl1PairChanged;
            end;

          apEnd:
            begin
              Result := IsControl2PairChanged;
            end;
        end;
      end;
    end
    else
    if FSelectedCount = 2 then
    begin
      Result := FCurrentSegment.IsControl2PairChanged;
    end;
  end;
end;

function TgmCurveSegmentsList.LoadCurveSegmentsFromStream(
  const AMemoryStream: TMemoryStream; const AVersionNum: Integer): Boolean;
var
  LSegmentReader: TgmCurveSegmentReader1;
  LCurveSegment : TgmCurveSegment;
  i, LLoadCount : Integer;
  LBooleanValue : Boolean;
begin
  Result     := False;
  LLoadCount := 0;

  if not Assigned(AMemoryStream) then
  begin
    Exit;
  end;

  case AVersionNum of
    1:
      begin
        Self.DeleteAllCurveSegments;

        AMemoryStream.Read(LBooleanValue, 1);
        Self.FClosed := LBooleanValue;

        AMemoryStream.Read(LBooleanValue, 1);
        Self.FFirstSegmentOK := LBooleanValue;

        AMemoryStream.Read(LLoadCount, 4);

        if LLoadCount > 0 then
        begin
          LSegmentReader := TgmCurveSegmentReader1.Create;
          try
            for i := 0 to (LLoadCount - 1) do
            begin
              LCurveSegment := LSegmentReader.GetCurveSegmentFromStream(AMemoryStream);

              if Assigned(LCurveSegment) then
              begin
                Self.Add(LCurveSegment);
              end;
            end;
          finally
            LSegmentReader.Free;
          end;
        end;

        Result := True;
      end;
  end;
end; 

procedure TgmCurveSegmentsList.SaveCurveSegmentsToStream(
  const AMemoryStream: TMemoryStream);
var
  i       : Integer;
  LSegment: TgmCurveSegment;
begin
  if Assigned(AMemoryStream) then
  begin
    AMemoryStream.Write(FClosed,         1);
    AMemoryStream.Write(FFirstSegmentOK, 1);
    AMemoryStream.Write(Self.Count,      4);

    if Self.Count > 0 then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LSegment := TgmCurveSegment(Self.Items[i]);
        
        LSegment.SaveToStream(AMemoryStream);
      end;
    end;
  end;
end; 

{ TgmCurvePath }

constructor TgmCurvePath.Create;
begin
  inherited Create();
  FCurveSegmentsList := TgmCurveSegmentsList.Create();
end;

destructor TgmCurvePath.Destroy;
begin
  FCurveSegmentsList.Free();
  inherited Destroy;
end;

function TgmCurvePath.GetSelfBackup: TgmCurvePath;
begin
  Result := TgmCurvePath.Create();

  if FCurveSegmentsList.Count > 0 then
  begin
    Result.CurveSegmentsList.AssignCurveSegmentListData(FCurveSegmentsList);
  end;
end; 

{ TgmCurvePathList }

constructor TgmCurvePathList.Create;
begin
  inherited Create();
  
  FSelectedPath      := nil;
  FSelectedPathIndex := -1;
  FStatus            := cplsAddNewPath;
  FPathRegion        := 0;
end;

destructor TgmCurvePathList.Destroy;
begin
  DeleteAllCurvePaths();
  DeleteObject(FPathRegion);
  
  inherited Destroy;
end;

procedure TgmCurvePathList.DeleteAllCurvePaths;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      LCurvePath.Free();
    end;

    Self.Clear;
    FSelectedPath      := nil;
    FSelectedPathIndex := -1;
  end;
end;

procedure TgmCurvePathList.AssignCurvePathListData(
  const ACurvePathList: TgmCurvePathList);
var
  i                : Integer;
  LCurvePath       : TgmCurvePath;
  LCurvePathBackup : TgmCurvePath;
begin
  if Assigned(ACurvePathList) then
  begin
    DeleteAllCurvePaths();
    
    if ACurvePathList.Count > 0 then
    begin
      for i := 0 to (ACurvePathList.Count - 1) do
      begin
        LCurvePath       := TgmCurvePath(ACurvePathList.Items[i]);
        LCurvePathBackup := LCurvePath.GetSelfBackup();

        Self.Add(LCurvePathBackup);
      end;
    end;

    FStatus            := ACurvePathList.Status;
    FSelectedPathIndex := ACurvePathList.SelectedPathIndex;

    if FSelectedPathIndex >= 0 then
    begin
      FSelectedPath := TgmCurvePath(Self.Items[FSelectedPathIndex]);
    end
    else
    begin
      FSelectedPath := nil;
    end;
  end;
end;

procedure TgmCurvePathList.AddNewPathToList(const APath: TgmCurvePath);
begin
  Self.Add(APath);
  
  FSelectedPath      := APath;
  FSelectedPathIndex := Self.Count - 1;
  FStatus            := cplsAddNextAnchorPoint;
end;

procedure TgmCurvePathList.DeselectAllPaths;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  FSelectedPath      := nil;
  FSelectedPathIndex := -1;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath                                 := TgmCurvePath(Self.Items[i]);
      LCurvePath.CurveSegmentsList.IsSelected    := False;
      LCurvePath.CurveSegmentsList.IsSelectedAll := False;
      FSelectedPath                              := nil;
      FSelectedPathIndex                         := -1;
      FStatus                                    := cplsAddNewPath;
    end;
  end;
end; 

procedure TgmCurvePathList.DrawAllPathRegionsForThumbnail(
  const ACanvas: TCanvas; const AOffsetVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      LCurvePath.CurveSegmentsList.MakeRegion(ACanvas, AOffsetVector);
    end;
  end;
end;

procedure TgmCurvePathList.DrawAllPathsForThumbnail(
  const ACanvas: TCanvas; const AOffsetVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      LCurvePath.CurveSegmentsList.DrawCurveSegments(ACanvas, pmCopy, AOffsetVector);
    end;
  end;
end; 

procedure TgmCurvePathList.DrawAllPaths(const ACanvas: TCanvas;
  const APenMode: TPenMode; const AOffsetVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if IsSelectedAll then
        begin
          DrawCurveSegments(ACanvas, APenMode, AOffsetVector);
          DrawCurveHandles(ACanvas, APenMode);
        end
        else
        begin
          if IsSelected then
          begin
            DrawCurveDirectionLines(ACanvas, APenMode);
            DrawCurveSegments(ACanvas, APenMode, AOffsetVector);
            DrawCurveHandles(ACanvas, APenMode);
          end
          else
          begin
            DrawCurveSegments(ACanvas, APenMode, AOffsetVector);
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurvePathList.DrawAllPaths(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if IsSelectedAll then
        begin
          DrawCurveSegments(ACanvas, APenMode, ACoordConvertFunc);
          DrawCurveHandles(ACanvas, APenMode, ACoordConvertFunc);
        end
        else
        begin
          if IsSelected then
          begin
            DrawCurveDirectionLines(ACanvas, APenMode, ACoordConvertFunc);
            DrawCurveSegments(ACanvas, APenMode, ACoordConvertFunc);
            DrawCurveHandles(ACanvas, APenMode, ACoordConvertFunc);
          end
          else
          begin
            DrawCurveSegments(ACanvas, APenMode, ACoordConvertFunc);
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmCurvePathList.TranslateAllSelectedPaths(
  const ATranslateVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      if LCurvePath.CurveSegmentsList.IsSelectedAll then
      begin
        LCurvePath.CurveSegmentsList.TranslateAllSegments(ATranslateVector);
      end;
    end;
  end;
end;

procedure TgmCurvePathList.TranslateAllPaths(const ATranslateVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      LCurvePath.CurveSegmentsList.TranslateAllSegments(ATranslateVector);
    end;
  end;
end;

procedure TgmCurvePathList.ScaleAllPaths(
  const AOldWidth, AOldHeight, ANewWidth, ANewHeight: Integer);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      LCurvePath.CurveSegmentsList.ScaleAllSegments(
        AOldWidth, AOldHeight, ANewWidth, ANewHeight);
    end;
  end;
end;

procedure TgmCurvePathList.SelectWholePathByIndex(const AIndex: Integer);
var
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      LCurvePath := TgmCurvePath(Self.Items[AIndex]);
      
      LCurvePath.CurveSegmentsList.IsSelectedAll := True;
      LCurvePath.CurveSegmentsList.IsSelected    := False;
    end;
  end;
end;

procedure TgmCurvePathList.SelectPathByIndex(const AIndex: Integer);
begin
  if Self.Count > 0 then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      DeselectAllPaths;
      
      FSelectedPath := TgmCurvePath(Self.Items[AIndex]);
      
      FSelectedPath.CurveSegmentsList.IsSelectedAll := False;
      FSelectedPath.CurveSegmentsList.IsSelected    := True;
      FSelectedPathIndex                            := AIndex;
    end;
  end;
end;

function TgmCurvePathList.GetWholeSelectedPathsCount: Integer;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      if LCurvePath.CurveSegmentsList.IsSelectedAll then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

function TgmCurvePathList.GetSelectedPathsCount: Integer;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      if LCurvePath.CurveSegmentsList.IsSelectedAll or
         LCurvePath.CurveSegmentsList.IsSelected then
      begin
        Inc(Result);
      end;
    end;
  end;
end;

procedure TgmCurvePathList.StrokeSelectedPaths(const ACanvas: TCanvas;
  const APenWidth: Integer; const APenColor, ABrushColor: TColor;
  const APenStyle: TPenStyle; const ABrushStyle: TBrushStyle;
  const AOffsetVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      LCurvePath.CurveSegmentsList.StrokePathWithForegroundColor(ACanvas,
        APenWidth, APenColor, ABrushColor, APenStyle, ABrushStyle, AOffsetVector);
    end;
  end;
end; 

procedure TgmCurvePathList.FillSelectedPaths(const ACanvas: TCanvas;
  const AColor: TColor; const ABrushStyle: TBrushStyle;
  const AOffsetVector: TPoint);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      LCurvePath.CurveSegmentsList.FillPathWithForegroundColor(
        ACanvas, AColor, ABrushStyle, AOffsetVector);
    end;
  end;
end; 

// get a cursor according to a selected curve path and a pen tool
function TgmCurvePathList.GetCursor(const AX, AY: Integer;
  const APenTool: TgmPenTools): TCursor;
var
  i               : Integer;
  LSelectedHandle : TgmPathSelectHandle;
  LCurvePath      : TgmCurvePath;
begin
  Result := crDefault;
  
  case APenTool of
    ptPathComponentSelection:
      begin
        Result := crPathComponentSelection;
      end;

    ptPenTool:
      begin
        Result := crPenToolSelected;
      end;

    ptDirectSelection,
    ptAddAnchorPoint,
    ptConvertPoint:
      begin
        Result := crDirectSelection;
      end;
  end;

  if Self.Count > 0 then
  begin
    // if there is selected path, determine whether the mouse is point on
    // its control handles first
    if Assigned(FSelectedPath) then
    begin
      LSelectedHandle := FSelectedPath.CurveSegmentsList.GetSelectHandle(AX, AY);

      if LSelectedHandle <> pshNone then
      begin
        Result := crMovePath;
        Exit;
      end;
    end;
    
    { If the mouse is not points on any control handles of the selected path,
      or there is no selected path, then determine whether the mouse is point
      on any of the paths in the list.  }
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      if LCurvePath.CurveSegmentsList.NilsIfPointOnBezier(AX, AY) then
      begin
        case APenTool of
          ptPathComponentSelection:
            begin
              Result := crMovePath;
            end;
            
          ptDirectSelection:
            begin
              Result := crHandPoint;
            end;
            
          ptPenTool:
            begin
              Result := crAddAnchorPoint;
            end;
            
          ptAddAnchorPoint:
            begin
              Result := crAddAnchorPoint;
            end;
            
          ptDeleteAnchorPoint:
            begin
              Result := crHandPoint;
            end;
            
          ptConvertPoint:
            begin
              Result := crHandPoint;
            end;
        end;
        
        Break;
      end;
    end;
  end;
end;

// get a cursor according to a selected curve path and a pen tool
function TgmCurvePathList.GetCursor(const AX, AY: Integer; APenTool: TgmPenTools;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TCursor;
var
  i               : Integer;
  LSelectedHandle : TgmPathSelectHandle;
  LCurvePath      : TgmCurvePath;
begin
  Result := crDefault;
  
  case APenTool of
    ptPathComponentSelection:
      begin
        Result := crPathComponentSelection;
      end;

    ptPenTool:
      begin
        Result := crPenToolSelected;
      end;

    ptDirectSelection,
    ptAddAnchorPoint,
    ptConvertPoint:
      begin
        Result := crDirectSelection;
      end;
  end;

  if Self.Count > 0 then
  begin
    // if there is selected path, determine whether the mouse is point on
    // its control handles first
    if Assigned(FSelectedPath) then
    begin
      LSelectedHandle := FSelectedPath.CurveSegmentsList.GetSelectHandle(
        AX, AY, ACoordConvertFunc);

      if LSelectedHandle <> pshNone then
      begin
        Result := crMovePath;
        Exit;
      end;
    end;
    
    // If the mouse is not points on any control handles of the selected path,
    // or there is no selected path, then determine whether the mouse is point
    // on any of the paths in the list. 
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      if LCurvePath.CurveSegmentsList.NilsIfPointOnBezier(AX, AY, ACoordConvertFunc) then
      begin
        case APenTool of
          ptPathComponentSelection:
            begin
              Result := crMovePath;
            end;
            
          ptDirectSelection:
            begin
              Result := crHandPoint;
            end;
            
          ptPenTool:
            begin
              Result := crAddAnchorPoint;
            end;
            
          ptAddAnchorPoint:
            begin
              Result := crAddAnchorPoint;
            end;
            
          ptDeleteAnchorPoint:
            begin
              Result := crHandPoint;
            end;
            
          ptConvertPoint:
            begin
              Result := crHandPoint;
            end;
        end;
        
        Break;
      end;
    end;
  end;
end;

// Determine whether the mouse is point on a path, used to select a whole
// path. The return value is the index of selected path in the list. 
function TgmCurvePathList.IfPointOnPathsForWholePath(
  const AX, AY: Integer): Integer;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := -1;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if ( GetSelectedEndingHandle(AX, AY) <> pshNone ) or
           NilsIfPointOnBezier(AX, AY) then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

// Determine whether the mouse is point on a path, used to select a whole
// path. The return value is the index of selected path in the list.
function TgmCurvePathList.IfPointOnPathsForWholePath(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Integer;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := -1;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if ( GetSelectedEndingHandle(AX, AY, ACoordConvertFunc) <> pshNone ) or
           NilsIfPointOnBezier(AX, AY, ACoordConvertFunc) then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TgmCurvePathList.IfPointOnPaths(const AX, AY: Integer): Boolean;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if NilsIfPointOnBezier(AX, AY) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TgmCurvePathList.IfPointOnPaths(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      with LCurvePath.CurveSegmentsList do
      begin
        if NilsIfPointOnBezier(AX, AY, ACoordConvertFunc) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function TgmCurvePathList.IfPointOnSelectedPathHandles(
  const AX, AY: Integer): Boolean;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      if LCurvePath.CurveSegmentsList.GetSelectHandle(AX, AY) <> pshNone then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TgmCurvePathList.IfPointOnSelectedPathHandles(const AX, AY: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc): Boolean;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);
      
      if LCurvePath.CurveSegmentsList.GetSelectHandle(
           AX, AY, ACoordConvertFunc) <> pshNone then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TgmCurvePathList.IfSelectedPathByIndex(const AIndex: Integer): Boolean;
var
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      LCurvePath := TgmCurvePath(Self.Items[AIndex]);
      Result     := LCurvePath.CurveSegmentsList.IsSelected;
    end;
  end;
end;

function TgmCurvePathList.IfSelectedWholePathByIndex(
  const AIndex: Integer): Boolean;
var
  LCurvePath : TgmCurvePath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      LCurvePath := TgmCurvePath(Self.Items[AIndex]);
      Result     := LCurvePath.CurveSegmentsList.IsSelectedAll;
    end;
  end;
end;

function TgmCurvePathList.SelectPath(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle): TgmCurvePath;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  ASelectedHandle := pshNone;
  Result          := nil;

  if Self.Count > 0 then
  begin
    // If there is a selected path, then determine whether the mouse is point on
    // any of the control handles of the path. If so, according to the control
    // handle select the appropriate sub curve segment in the path, and then
    // return the selected path and control handle.
    if Assigned(FSelectedPath) then
    begin
      if FSelectedPath.CurveSegmentsList.SelectAtHandles(AX, AY, ASelectedHandle) then
      begin
        Result := FSelectedPath;
        Exit;
      end;
    end;

    // If the mouse is not point on any control handles of the selected path,
    // then deselect all pahts, and then determine whether the mouse is point on
    // a path, if so, select it, but the returned control handle is shNone. 

    DeselectAllPaths;

    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      if LCurvePath.CurveSegmentsList.SelectAtSingleSegment(AX, AY) then
      begin
        // if point on the start point of the first curve segment, then select it only
        if LCurvePath.CurveSegmentsList.PointOnFirstSegmentStart(AX, AY) then
        begin
          LCurvePath.CurveSegmentsList.SelectAtHandles(AX, AY, ASelectedHandle);
        end;

        FSelectedPath      := LCurvePath;
        FSelectedPathIndex := i;
        Result             := FSelectedPath;
        
        Break;
      end;
    end;

    FStatus := cplsAddNewPath;

    if Assigned(FSelectedPath) then
    begin
      ASelectedHandle := FSelectedPath.CurveSegmentsList.GetSelectHandle(AX, AY);

      if not FSelectedPath.CurveSegmentsList.IsClosed then
      begin
        FStatus := cplsAddNextAnchorPoint;
      end;
    end;
  end;
end;

function TgmCurvePathList.SelectPath(const AX, AY: Integer;
  var ASelectedHandle: TgmPathSelectHandle;
  ACoordConvertFunc: TgmPointCoordConvertFunc): TgmCurvePath;
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  ASelectedHandle := pshNone;
  Result          := nil;

  if Self.Count > 0 then
  begin
    // If there is a selected path, then determine whether the mouse is point on
    // any of the control handles of the path. If so, according to the control
    // handle select the appropriate sub curve segment in the path, and then
    // return the selected path and control handle.
    if Assigned(FSelectedPath) then
    begin
      if FSelectedPath.CurveSegmentsList.SelectAtHandles(
           AX, AY, ASelectedHandle, ACoordConvertFunc) then
      begin
        Result := FSelectedPath;
        Exit;
      end;
    end;

    // If the mouse is not point on any control handles of the selected path,
    // then deselect all pahts, and then determine whether the mouse is point on
    // a path, if so, select it, but the returned control handle is shNone. 

    DeselectAllPaths();

    for i := 0 to (Self.Count - 1) do
    begin
      LCurvePath := TgmCurvePath(Self.Items[i]);

      if LCurvePath.CurveSegmentsList.SelectAtSingleSegment(
           AX, AY, ACoordConvertFunc) then
      begin
        // if point on the start point of the first curve segment, then select it only
        if LCurvePath.CurveSegmentsList.PointOnFirstSegmentStart(
             AX, AY, ACoordConvertFunc) then
        begin
          LCurvePath.CurveSegmentsList.SelectAtHandles(
            AX, AY, ASelectedHandle, ACoordConvertFunc);
        end;

        FSelectedPath      := LCurvePath;
        FSelectedPathIndex := i;
        Result             := FSelectedPath;
        
        Break;
      end;
    end;

    FStatus := cplsAddNewPath;

    if Assigned(FSelectedPath) then
    begin
      ASelectedHandle := FSelectedPath.CurveSegmentsList.GetSelectHandle(
        AX, AY, ACoordConvertFunc);

      if not FSelectedPath.CurveSegmentsList.IsClosed then
      begin
        FStatus := cplsAddNextAnchorPoint;
      end;
    end;
  end;
end;

function TgmCurvePathList.LoadCurvePathsFromStream(
  const AMemoryStream: TMemoryStream; const AVersionNum: Integer): Boolean;
var
  i, LLoadCount : Integer;
  LCurvePath    : TgmCurvePath;
begin
  Result := False;

  if not Assigned(AMemoryStream) then
  begin
    Exit;
  end;

  // read in the path count
  AMemoryStream.Read(LLoadCount, 4);

  if LLoadCount > 0 then
  begin
    Self.DeleteAllCurvePaths();
    
    for i := 0 to (LLoadCount - 1) do
    begin
      LCurvePath := TgmCurvePath.Create;

      LCurvePath.CurveSegmentsList.LoadCurveSegmentsFromStream(AMemoryStream, AVersionNum);
      Self.Add(LCurvePath);
    end;
  end;

  Result := True; 
end;

procedure TgmCurvePathList.SaveCurvePathsToStream(
  const AMemoryStream: TMemoryStream);
var
  i          : Integer;
  LCurvePath : TgmCurvePath;
begin
  if Assigned(AMemoryStream) then
  begin
    AMemoryStream.Write(Self.Count, 4);

    if Self.Count > 0  then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LCurvePath := TgmCurvePath(Self.Items[i]);

        LCurvePath.CurveSegmentsList.SaveCurveSegmentsToStream(AMemoryStream);
      end;
    end;
  end;
end;

// this function will create a region with the path
function TgmCurvePathList.UpdatePathRegion(
  const ACanvasWidth, ACanvasHeight: Integer;
  const AOffsetVector: TPoint): Boolean;
var
  i, j         : Integer;
  LCurvePath   : TgmCurvePath;
  LSegment     : TgmCurveSegment;
  LCurvePoints : array of TPoint;
  LTempBmp     : TBitmap;
begin
  Result := False;

  if (ACanvasWidth <= 0) or (ACanvasHeight <= 0) then
  begin
    Exit;
  end;

  SetLength(LCurvePoints, 0);

  if Self.Count > 0 then
  begin
    LTempBmp := TBitmap.Create;
    try
      LTempBmp.Width              := ACanvasWidth;
      LTempBmp.Height             := ACanvasHeight;
      LTempBmp.Canvas.Brush.Style := bsSolid;
      LTempBmp.Canvas.Brush.Color := clWhite;

      BeginPath(LTempBmp.Canvas.Handle); // begin to record the path

      for i := 0 to (Self.Count - 1) do
      begin
        // get a whole path
        LCurvePath := TgmCurvePath(Self.Items[i]);

        // if the path is in selected state...
        if LCurvePath.CurveSegmentsList.IsSelected or
           LCurvePath.CurveSegmentsList.IsSelectedAll then
        begin
          // remove the old data, prepare to store new data
          SetLength(LCurvePoints, 0);

          if LCurvePath.CurveSegmentsList.Count > 0 then
          begin
            for j := 0 to (LCurvePath.CurveSegmentsList.Count - 1) do
            begin
              // get each curve of the path
              LSegment := TgmCurveSegment(LCurvePath.CurveSegmentsList.Items[j]);

              // extract the coordinates of the curve
              if j = 0 then
              begin
                SetLength( LCurvePoints, Length(LCurvePoints) + 1 );
                LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.StartPoint, AOffsetVector);
              end;

              SetLength( LCurvePoints, Length(LCurvePoints) + 1 );
              LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.Control1, AOffsetVector);

              SetLength( LCurvePoints, Length(LCurvePoints) + 1 );
              LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.Control2, AOffsetVector);

              SetLength( LCurvePoints, Length(LCurvePoints) + 1 );
              LCurvePoints[High(LCurvePoints)] := AddPoints(LSegment.EndPoint, AOffsetVector);
            end;

            { Drawing the curve with the extracted coordinates in the
              BeginPath state for getting the path. }
            LTempBmp.Canvas.PolyBezier(LCurvePoints);
          end;
        end;
      end;

      // after record all of the paths, stop recording
      EndPath(LTempBmp.Canvas.Handle);

      // convert the path to region
      DeleteObject(FPathRegion);
      FPathRegion := PathToRegion(LTempBmp.Canvas.Handle);

      if FPathRegion <> 0 then
      begin
        Result := True;
      end;
    finally
      LTempBmp.Free;
    end;
  end;
end; 

{ TgmCurveSegmentReader1 }

function TgmCurveSegmentReader1.GetCurveSegmentFromStream(
  const AMemoryStream: TMemoryStream): TgmCurveSegment;
var
  LIntValue    : Integer;
  LBooleanValue: Boolean;
begin
  Result := nil;

  if Assigned(AMemoryStream) then
  begin
    Result := TgmCurveSegment.Create;

    AMemoryStream.Read(Result.FStartPoint.X, 4);
    AMemoryStream.Read(Result.FStartPoint.Y, 4);
    AMemoryStream.Read(Result.FEndPoint.X,   4);
    AMemoryStream.Read(Result.FEndPoint.Y,   4);
    AMemoryStream.Read(Result.FControl1.X,   4);
    AMemoryStream.Read(Result.FControl1.Y,   4);
    AMemoryStream.Read(Result.FControl2.X,   4);
    AMemoryStream.Read(Result.FControl2.Y,   4);
    AMemoryStream.Read(Result.FOpposite1.X,  4);
    AMemoryStream.Read(Result.FOpposite1.Y,  4);
    AMemoryStream.Read(Result.FOpposite2.X,  4);
    AMemoryStream.Read(Result.FOpposite2.Y,  4);
    AMemoryStream.Read(Result.FActive,       1);

    AMemoryStream.Read(LIntValue, 4);
    Result.FActivePoint := TgmActivePoint(LIntValue);

    AMemoryStream.Read(LIntValue, 4);
    Result.FStartingPointType := TgmEndingPointType(LIntValue);

    AMemoryStream.Read(LIntValue, 4);
    Result.FEndingPointType := TgmEndingPointType(LIntValue);

    AMemoryStream.Read(Result.FControl1Radian,    SizeOf(Double));
    AMemoryStream.Read(Result.FOpposite1Radian,   SizeOf(Double));
    AMemoryStream.Read(Result.FControl2Radian,    SizeOf(Double));
    AMemoryStream.Read(Result.FOpposite2Radian,   SizeOf(Double));
    AMemoryStream.Read(Result.FRadianDifference1, SizeOf(Double));
    AMemoryStream.Read(Result.FRadianDifference2, SizeOf(Double));
    AMemoryStream.Read(Result.FControl1Length,    SizeOf(Double));
    AMemoryStream.Read(Result.FOpposite1Length,   SizeOf(Double));
    AMemoryStream.Read(Result.FControl2Length,    SizeOf(Double));
    AMemoryStream.Read(Result.FOpposite2Length,   SizeOf(Double));

    AMemoryStream.Read(LIntValue, 4);
    Result.FControl1PairState := TgmPairState(LIntValue);

    AMemoryStream.Read(LIntValue, 4);
    Result.FControl2PairState := TgmPairState(LIntValue);

    AMemoryStream.Read(LBooleanValue, 1);
    Result.FControl1PairChanged := LBooleanValue;

    AMemoryStream.Read(LBooleanValue, 1);
    Result.FControl2PairChanged := LBooleanValue;
  end;
end; 

end.
