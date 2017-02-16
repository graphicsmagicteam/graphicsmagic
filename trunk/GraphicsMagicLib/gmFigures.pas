unit gmFigures;

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
 * Update Date: September 20th, 2014
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

// Update Date: 2017/01/23

interface

{$WARN UNSAFE_CAST OFF}

uses
{ Standard }
  Windows,          // TPoint
  SysUtils,
  Graphics,         // TCanvas
  Classes,
{ GraphicsMagic Lib }
  gmTypes;          // TDrawingHandle

const
  REGULAR_FIGURE  : Boolean = True;
  IRREGULAR_FIGURE: Boolean = False;

type
  TgmFigureSelectIncludeMode = (fsimPartiallyInclude, fsimTotallyInclude);

  { Could use BOOLEAN instead of the following type, but the following are
    more descriptive }
  TgmHighlightSelectedFigure = (hsfHighlight, hsfDoNotHighlight);

  TgmFigureFlags = (ffNone,
                    ffStraightLine,
                    ffCurve,
                    ffPolygon,
                    ffRegularPolygon,
                    ffRectangle,
                    ffSquare,
                    ffRoundRectangle,
                    ffRoundSquare,
                    ffEllipse,
                    ffCircle,
                    ffPencil);

  TgmCurveControlPoint = (ccpFirst, ccpSecond);

  TgmFigureDrawMode = (fdmRGB, fdmMask);

  TgmFigureBasicData = record
     Name      : string;
     PenColor  : TColor;
     BrushColor: TColor;
     PenWidth  : Integer;
     PenStyle  : TPenStyle;
     BrushStyle: TBrushStyle;
     OriginX   : Integer;     // X-coordinate of origin for regular figures
     OriginY   : Integer;     // Y-coordinate of origin for regular figures
     Radius    : Integer;     // radius of regular figures
  end;

//-- TgmFigureObject -----------------------------------------------------------

  TgmFigureObject = class(TObject)
  private
    FOriginX: Integer;     // X-coordinate of origin for regular figures
    FOriginY: Integer;     // Y-coordinate of origin for regular figures
    FRadius : Integer;     // radius of regular figures

    FPenWidth         : Integer;
    FPenStyle         : TPenStyle;
    FPenColor         : TColor;
    FBrushStyle       : TBrushStyle;
    FBrushColor       : TColor;
    FRoundCornerRadius: Integer;

    { Normally, the selected item is flagged TRUE.  When multiple
      selections are allowed, more than one figure can be flagged as
      selected, however. }
    FSelected: Boolean;
    FLocked  : Boolean;
    FFlag    : TgmFigureFlags;
    FName    : string;

    { If both the control points of a curve are at same position. this field
      indicating which point you want to move first. }
    FCurveControl            : TgmCurveControlPoint;
    FPolygonCurrentPointIndex: Integer;
    FRegular                 : Boolean;   // indicating whether the figure is a regular figure
    FSides                   : Integer;   // for regular polygon

    function GetOriginCMX: Double;
    function GetOriginCMY: Double;
    function GetRadiusCM: Double;
    function GetOriginInchX: Double;
    function GetOriginInchY: Double;
    function GetRadiusInch: Double;
    function GetOriginPointX: Double;
    function GetOriginPointY: Double;
    function GetRadiusPoint: Double;

    procedure SetOriginXInPixel(const X: Integer);
    procedure SetOriginYInPixel(const Y: Integer);
    procedure SetRadiusInPixel(const Radius: Integer);
    procedure SetOriginXInCM(const X: Double);
    procedure SetOriginYInCM(const Y: Double);
    procedure SetRadiusInCM(const Radius: Double);
    procedure SetOriginXInInch(const X: Double);
    procedure SetOriginYInInch(const Y: Double);
    procedure SetRadiusInInch(const Radius: Double);
    procedure SetOriginXInPoint(const X: Double);
    procedure SetOriginYInPoint(const Y: Double);
    procedure SetRadiusInPoint(const Radius: Double);
  protected
    procedure DuplicateBasicData(const AFigureObj: TgmFigureObject);
  public
    FStartPoint   : TPoint;
    FEndPoint     : TPoint;
    FCurvePoint1  : TPoint;
    FCurvePoint2  : TPoint;
    FPolygonPoints: array of TPoint;

    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer);

    procedure CalcOrigin;
    procedure CalcRadius;

    procedure StandardizeOrder;
    procedure Translate(const TranslateVector: TPoint);

    procedure DrawHandles(const ACanvas: TCanvas;
      const PenMode: TPenMode; const BrushStyle: TBrushStyle;
      const BrushColor: TColor; const Radius: Integer);

    function GetHandleAtPoint(const AX, AY, ARadius: Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;

    function ContainsPoint(const ATestPoint: TPoint;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil):  Boolean;

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); overload; virtual; abstract; 

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); overload; virtual; abstract;

    function GetSelfBackup: TgmFigureObject; virtual; abstract;
    procedure AssignData(const AFigureObj: TgmFigureObject); virtual; abstract;

    procedure SaveToStream(const AStream: TStream); virtual; abstract;

    property PenWidth                : Integer              read FPenWidth                 write FPenWidth;
    property PenStyle                : TPenStyle            read FPenStyle                 write FPenStyle;
    property PenColor                : TColor               read FPenColor                 write FPenColor;
    property BrushStyle              : TBrushStyle          read FBrushStyle               write FBrushStyle;
    property BrushColor              : TColor               read FBrushColor               write FBrushColor;
    property RoundCornerRadius       : Integer              read FRoundCornerRadius        write FRoundCornerRadius;
    property IsSelected              : Boolean              read FSelected                 write FSelected;
    property IsLocked                : Boolean              read FLocked                   write FLocked;
    property Flag                    : TgmFigureFlags       read FFlag;
    property Name                    : string               read FName                     write FName;
    property CurveControl            : TgmCurveControlPoint read FCurveControl             write FCurveControl;
    property PolygonCurrentPointIndex: Integer              read FPolygonCurrentPointIndex write FPolygonCurrentPointIndex;
    property IsRegular               : Boolean              read FRegular                  write FRegular;
    property Sides                   : Integer              read FSides                    write FSides;
    property OriginX                 : Integer              read FOriginX                  write FOriginX;
    property OriginY                 : Integer              read FOriginY                  write FOriginY;
    property OriginPixelX            : Integer              read FOriginX                  write SetOriginXInPixel;
    property OriginPixelY            : Integer              read FOriginY                  write SetOriginYInPixel;
    property Radius                  : Integer              read FRadius                   write FRadius;
    property RadiusPixel             : Integer              read FRadius                   write SetRadiusInPixel;
    property OriginCMX               : Double               read GetOriginCMX              write SetOriginXInCM;
    property OriginCMY               : Double               read GetOriginCMY              write SetOriginYInCM;
    property RadiusCM                : Double               read GetRadiusCM               write SetRadiusInCM;
    property OriginInchX             : Double               read GetOriginInchX            write SetOriginXInInch;
    property OriginInchY             : Double               read GetOriginInchY            write SetOriginYInInch;
    property RadiusInch              : Double               read GetRadiusInch             write SetRadiusInInch;
    property OriginPointX            : Double               read GetOriginPointX           write SetOriginXInPoint;
    property OriginPointY            : Double               read GetOriginPointY           write SetOriginYInPoint;
    property RadiusPoint             : Double               read GetRadiusPoint            write SetRadiusInPoint;
  end;

//-- TgmLineObject -------------------------------------------------------------

  TgmLineObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmCurveObject ------------------------------------------------------------

  TgmCurveObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const P1, P2, P3, P4: TPoint);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmPolygonObject ----------------------------------------------------------

  TgmPolygonObject = class(TgmFigureObject)
  private
    FFillPolygon: Boolean;
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const APolygon: array of TPoint);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;

    property IsFillPolygon: Boolean read FFillPolygon write FFillPolygon; 
  end;

//-- TgmRegularPolygonObject ---------------------------------------------------

  TgmRegularPolygonObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth, Sides: Integer; const CenterPoint, CurrentPoint: TPoint);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmRectangleObject --------------------------------------------------------

  TgmRectangleObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const Regular: Boolean);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmRoundRectangleObject ---------------------------------------------------

  TgmRoundRectangleObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const CornerRadius: Integer; const Regular: Boolean);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmEllipseObject ----------------------------------------------------------

  TgmEllipseObject = class(TgmFigureObject)
  public
    constructor Create(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const Regular: Boolean);

    procedure DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
      const Offset: TPoint; const Factor: Integer;
      const FigureDrawMode: TgmFigureDrawMode); override;

    procedure DrawFigure(ACanvas: TCanvas; const APenMode: TPenMode;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil); override;

    function GetSelfBackup: TgmFigureObject; override;
    procedure AssignData(const AFigureObj: TgmFigureObject); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

//-- TgmFigureList -------------------------------------------------------------

  // use TList to store figures
  TgmFigureList = class(TList)
  private
    // index of node in TList when only single figure is selected
    FIndexOfSelected      : Integer;
    FLineNumber           : Integer;
    FCurveNumber          : Integer;
    FPolygonNumber        : Integer;
    FRegularPolygonNumber : Integer;
    FRectangleNumber      : Integer;
    FSquareNumber         : Integer;
    FRoundRectangleNumber : Integer;
    FRoundSquareNumber    : Integer;
    FEllipseNumber        : Integer;
    FCircleNumber         : Integer;

    function InSelect(const X, Y, X1, Y1, X2, Y2: Integer): Boolean;

    function InRect(const AFigureObj: TgmFigureObject; const X, Y: Integer;
      const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): Boolean;

    function AllInRect(const AFigureObj: TgmFigureObject;
      const X1, Y1, X2, Y2, X3, Y3, X4, Y4, StartX, StartY, EndX, EndY: Integer): Boolean;

    function GetLockedFigureCount: Integer;
  public
    constructor Create;
    destructor Destroy;  override;

    procedure DuplicateFigureList(const AList: TgmFigureList);

    // Group of routines that manages "selected" flags for the figures.
    procedure SelectedFigureIncrementIndex(const Increment: Integer);
    function SelectedFigureCount: Integer;

    function SelectedContainsPoint(const ATargetPoint: TPoint;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;

    procedure SelectAllFigures;

    procedure DeleteSelectedFigures;
    procedure DeleteAllFigures;
    procedure DeleteFigureByIndex(const Index: Integer);
    procedure DeselectAllFigures;

    procedure DrawAllFigures(ACanvas: TCanvas; const PenMode: TPenMode;
      const FigureDrawMode: TgmFigureDrawMode);

    procedure DrawSelectedFigures(ACanvas: TCanvas; const PenMode: TPenMode;
      const FigureDrawMode: TgmFigureDrawMode);

    function GetSelectedFigure: TgmFigureObject;
    procedure TranslateSelectedFigures(const TranslateVector: TPoint);
    procedure SelectFigures(const Shift: TShiftState; const X, Y: Integer);
    function GetSelectedHandleAtPoint(const AX, AY: Integer): TgmDrawingHandle;

    // this routine is written by Xiong Wei
    procedure SelectRect(const IncludeMode: TgmFigureSelectIncludeMode;
      const RegionOrigin, RegionMovePt: TPoint);

    procedure SetSelectedFlags(const State: Boolean);
    procedure SetSelectedIndex(const Index: Integer);
    function GetTopLeftFromAllFigures: TPoint;

    procedure AddStraightLineToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint);

    procedure AddCurveToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const P1, P2, P3, P4: TPoint);

    procedure AddPolygonToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const APolygon: array of TPoint);

    procedure AddRegularPolygonToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth, Sides: Integer; const CenterPoint, CurrentPoint: TPoint);

    procedure AddRectangleToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const Regular: Boolean);

    procedure AddRoundRectangleToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const RoundCornerRadius: Integer; const Regular: Boolean);
   
    procedure AddEllipseToList(const PenColor, BrushColor: TColor;
      const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
      const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
      const Regular: Boolean);

    function IfPointOnFigure(const AX, AY:Integer;
      ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
      
    function ValidIndex(const AIndex: Integer): Boolean;

    // Save data of figure list to stream. Loading these data by specific loader.
    procedure SaveToStream(const AStream: TStream);

    property LockedFigureCount   : Integer read GetLockedFigureCount;
    property SelectedIndex       : Integer read FIndexOfSelected      write SetSelectedIndex;
    property LineNumber          : Integer read FLineNumber           write FLineNumber;
    property CurveNumber         : Integer read FCurveNumber          write FCurveNumber;
    property PolygonNumber       : Integer read FPolygonNumber        write FPolygonNumber;
    property RegularPolygonNumber: Integer read FRegularPolygonNumber write FRegularPolygonNumber;
    property RectangleNumber     : Integer read FRectangleNumber      write FRectangleNumber;
    property SquareNumber        : Integer read FSquareNumber         write FSquareNumber;
    property RoundRectangleNumber: Integer read FRoundRectangleNumber write FRoundRectangleNumber;
    property RoundSquareNumber   : Integer read FRoundSquareNumber    write FRoundSquareNumber;
    property EllipseNumber       : Integer read FEllipseNumber        write FEllipseNumber;
    property CircleNumber        : Integer read FCircleNumber         write FCircleNumber;
  end;

//-- TgmFigureListReader -------------------------------------------------------

  TgmFigureListReader = class(TObject)
  protected
    FFigureList: TgmFigureList;  // pointer to a figure list
  public
    constructor Create(const AFigureList: TgmFigureList);
    destructor Destroy; override;

    function LoadFromStream(const AStream: TStream): Boolean; virtual; abstract;
  end;

//-- TgmFigureListReader1 ------------------------------------------------------

  TgmFigureListReader1 = class(TgmFigureListReader)
  private
    function GetRectangle(const AStream: TStream; const IfRegular: Boolean): TgmFigureObject;
    function GetRoundedCornerRectangle(const AStream: TStream; const IfRegular: Boolean): TgmFigureObject;
    function GetEllipse(const AStream: TStream; const IfRegular: Boolean): TgmFigureObject;
    function GetCurve(const AStream: TStream): TgmFigureObject;
    function GetStraightLine(const AStream: TStream): TgmFigureObject;
    function GetPolygon(const AStream: TStream): TgmFigureObject;
    function GetRegularPolygon(const AStream: TStream): TgmFigureObject;
  public
    function LoadFromStream(const AStream: TStream): Boolean; override;
  end;

//-- public methods ------------------------------------------------------------

  function GetFigureName(AFlag: TgmFigureFlags): string;

implementation

uses
{ Standard }
  Math,
{ externals }
  Preview,
  LineLibrary,        // NearLine()
{ GraphicsMagicLib }
  gmPaintFuncs,       // DrawHandle()
  gmGUIFuncs,
  gmMath,
  gmConstants,
  gmCommonFuncs;

type
  TFigureListHeaderVer1 = record
    FigureCount         : Cardinal;
    LockedFigureCount   : Cardinal;
    LineNumber          : Cardinal;
    CurveNumber         : Cardinal;
    PolygonNumber       : Cardinal;
    RegularPolygonNumber: Cardinal;
    RectangleNumber     : Cardinal;
    SquareNumber        : Cardinal;
    RoundRectangleNumber: Cardinal;
    RoundSquareNumber   : Cardinal;
    EllipseNumber       : Cardinal;
    CircleNumber        : Cardinal;
  end;

const
  // No figure selected constant
  FIGURE_NOT_SELECTED: Integer = -1;

//-- public methods ------------------------------------------------------------

function GetFigureName(AFlag: TgmFigureFlags): string;
begin
  case AFlag of
    ffNone:
      begin
        Result := 'Unknow';
      end;

    ffStraightLine:
      begin
        Result := 'Straight Line';
      end;
      
    ffCurve:
      begin
        Result := 'Bezier Curve';
      end;
      
    ffPolygon:
      begin
        Result := 'Polygon';
      end;
      
    ffRegularPolygon:
      begin
        Result := 'Regular Polygon';
      end;
      
    ffRectangle:
      begin
        Result := 'Rectangle';
      end;
      
    ffSquare:
      begin
        Result := 'Square';
      end;

    ffRoundRectangle:
      begin
        Result := 'Rounded-Corner Rectangle';
      end;

    ffRoundSquare:
      begin
        Result := 'Rounded-Corner Square';
      end;

    ffEllipse:
      begin
        Result := 'Ellipse';
      end;

    ffCircle:
      begin
        Result := 'Circle';
      end;
      
    ffPencil:
      begin
        Result := 'Pencil';
      end;
  end;
end;

//-- private methods -----------------------------------------------------------

function Sign(const i: Integer): Integer;
begin
  if i = 0 then
  begin
    Result := 0;
  end
  else if i > 0 then
  begin
    Result := +1;
  end
  else
  begin
    Result := -1;
  end;
end;

function UnitDelta(const PointA, PointB: TPoint): TPoint;
begin
  Result := Point( Sign(PointB.X - PointA.X), Sign(PointB.Y - PointA.Y) );
end;

//-- TgmFigureObject -----------------------------------------------------------

constructor TgmFigureObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer);
begin
  inherited Create;

  FOriginX := 0;
  FOriginY := 0;
  FRadius  := 0;

  // assign values for new figure
  FPenColor          := PenColor;
  FPenStyle          := PenStyle;
  FPenWidth          := PenWidth;
  FBrushColor        := BrushColor;
  FBrushStyle        := BrushStyle;
  FRoundCornerRadius := 0;

  // Do not automatically select each figure as it is created.
  FSelected    := False;
  FLocked      := False;
  FFlag        := ffNone;
  FName        := '';
  FStartPoint  := Point(0, 0);
  FEndPoint    := Point(0, 0);
  FCurvePoint1 := Point(0, 0);
  FCurvePoint2 := Point(0, 0);
  SetLength(FPolygonPoints, 0);
  
  FCurveControl             := ccpSecond;
  FPolygonCurrentPointIndex := 0;
  FRegular                  := False;

{ Regular Polygon }
  FSides := 0;
end;

function TgmFigureObject.GetOriginCMX: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FOriginX, LDPI, mmPixel, mmLoMetric) / 100;
end; 

function TgmFigureObject.GetOriginCMY: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FOriginY, LDPI, mmPixel, mmLoMetric) / 100;
end;

function TgmFigureObject.GetRadiusCM: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FRadius, LDPI, mmPixel, mmLoMetric) / 100;
end;

function TgmFigureObject.GetOriginInchX: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FOriginX, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmFigureObject.GetOriginInchY: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FOriginY, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmFigureObject.GetRadiusInch: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FRadius, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmFigureObject.GetOriginPointX: Double;
var
  Inches: Double;
  LDPI  : Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Inches := ConvertUnits(FOriginX, LDPI, mmPixel, mmLoEnglish) / 100;
  Result := Inches * 72;
end;

function TgmFigureObject.GetOriginPointY: Double;
var
  Inches: Double;
  LDPI  : Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Inches := ConvertUnits(FOriginY, LDPI, mmPixel, mmLoEnglish) / 100;
  Result := Inches * 72;
end;

function TgmFigureObject.GetRadiusPoint: Double;
var
  Inches: Double;
  LDPI  : Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Inches := ConvertUnits(FRadius, LDPI, mmPixel, mmLoEnglish) / 100;
  Result := Inches * 72;
end;

procedure TgmFigureObject.SetOriginXInPixel(const X: Integer);
var
  Offset: Integer;
begin
  if X <> FOriginX then
  begin
    if FFlag in [ffRegularPolygon, ffSquare, ffRoundSquare, ffCircle] then
    begin
      Offset        := X - FOriginX;
      FStartPoint.X := FStartPoint.X + Offset;
      FEndPoint.X   := FEndPoint.X   + Offset;
      FOriginX      := X;

      if FFlag = ffRegularPolygon then
      begin
        CalcRegularPolygonVertices(FPolygonPoints, FStartPoint, FEndPoint, FSides);
      end;
    end;
  end;
end;

procedure TgmFigureObject.SetOriginYInPixel(const Y: Integer);
var
  Offset: Integer;
begin
  if Y <> FOriginY then
  begin
    if FFlag in [ffRegularPolygon, ffSquare, ffRoundSquare, ffCircle] then
    begin
      Offset        := Y - FOriginY;
      FStartPoint.Y := FStartPoint.Y + Offset;
      FEndPoint.Y   := FEndPoint.Y   + Offset;
      FOriginY      := Y;

      if FFlag = ffRegularPolygon then
      begin
        CalcRegularPolygonVertices(FPolygonPoints, FStartPoint, FEndPoint, FSides);
      end;
    end;
  end;
end;

procedure TgmFigureObject.SetRadiusInPixel(const Radius: Integer);
var
  Offset    : Integer;
  Radians   : Extended;
  IncX, IncY: Integer;
begin
  if Radius <> FRadius then
  begin
    if FFlag in [ffRegularPolygon, ffSquare, ffRoundSquare, ffCircle] then
    begin
      FRadius := Radius;
      Offset  := Round(  Sqrt( Sqr(FRadius) / 2 )  );

      case FFlag of
        ffSquare, ffRoundSquare:
          begin
            FStartPoint.X := FOriginX - Offset;
            FStartPoint.Y := FOriginY - Offset;
            FEndPoint.X   := FOriginX + Offset;
            FEndPoint.Y   := FOriginY + Offset;
          end;

        ffCircle:
          begin
            FStartPoint.X := FOriginX - FRadius;
            FStartPoint.Y := FOriginY - FRadius;
            FEndPoint.X   := FOriginX + FRadius;
            FEndPoint.Y   := FOriginY + FRadius;
          end;

        ffRegularPolygon:
          begin
            Radians     := ArcTan2(FEndPoint.Y - FStartPoint.Y, FEndPoint.X - FStartPoint.X);
            IncX        := Round( Radius * Cos(Radians) );
            IncY        := Round( Radius * Sin(Radians) );
            FEndPoint.X := FOriginX + IncX;
            FEndPoint.Y := FOriginY + IncY;

            CalcRegularPolygonVertices(FPolygonPoints, FStartPoint, FEndPoint, FSides);
          end;
      end;
    end;
  end;
end;

procedure TgmFigureObject.SetOriginXInCM(const X: Double);
var
  LoMetricValue: Integer;
  LIntX, LDPI  : Integer;
begin
  LDPI          := GetScreenPixelsPerInch;
  LoMetricValue := Round(X * 100);
  LIntX         := ConvertUnits(LoMetricValue, LDPI, mmLoMetric, mmPixel);

  SetOriginXInPixel(LIntX);
end;

procedure TgmFigureObject.SetOriginYInCM(const Y: Double);
var
  LoMetricValue: Integer;
  LIntY, LDPI  : Integer;
begin
  LDPI          := GetScreenPixelsPerInch;
  LoMetricValue := Round(Y * 100);
  LIntY         := ConvertUnits(LoMetricValue, LDPI, mmLoMetric, mmPixel);

  SetOriginYInPixel(LIntY);
end;

procedure TgmFigureObject.SetRadiusInCM(const Radius: Double);
var
  LoMetricValue: Integer;
  LIntRadius   : Integer;
  LDPI         : Integer;
begin
  LDPI          := GetScreenPixelsPerInch;
  LoMetricValue := Round(Radius * 100);
  LIntRadius    := ConvertUnits(LoMetricValue, LDPI, mmLoMetric, mmPixel);

  SetRadiusInPixel(LIntRadius);
end;

procedure TgmFigureObject.SetOriginXInInch(const X: Double);
var
  LoEnglishValue: Integer;
  LIntX, LDPI   : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LoEnglishValue := Round(X * 100);
  LIntX          := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetOriginXInPixel(LIntX);
end;

procedure TgmFigureObject.SetOriginYInInch(const Y: Double);
var
  LoEnglishValue: Integer;
  LIntY, LDPI   : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LoEnglishValue := Round(Y * 100);
  LIntY          := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetOriginYInPixel(LIntY);
end;

procedure TgmFigureObject.SetRadiusInInch(const Radius: Double);
var
  LoEnglishValue: Integer;
  LIntRadius    : Integer;
  LDPI          : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LoEnglishValue := Round(Radius * 100);
  LIntRadius     := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetRadiusInPixel(LIntRadius);
end;

procedure TgmFigureObject.SetOriginXInPoint(const X: Double);
var
  LInches       : Double;
  LoEnglishValue: Integer;
  LIntX, LDPI   : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LInches        := X / 72;
  LoEnglishValue := Round(LInches * 100);
  LIntX          := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetOriginXInPixel(LIntX);
end;

procedure TgmFigureObject.SetOriginYInPoint(const Y: Double);
var
  LInches       : Double;
  LoEnglishValue: Integer;
  LIntY, LDPI   : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LInches        := Y / 72;
  LoEnglishValue := Round(LInches * 100);
  LIntY          := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetOriginYInPixel(LIntY);
end;

procedure TgmFigureObject.SetRadiusInPoint(const Radius: Double);
var
  LInches       : Double;
  LoEnglishValue: Integer;
  LIntRadius    : Integer;
  LDPI          : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LInches        := Radius / 72;
  LoEnglishValue := Round(LInches * 100);
  LIntRadius     := ConvertUnits(LoEnglishValue, LDPI, mmLoEnglish, mmPixel);

  SetRadiusInPixel(LIntRadius);
end;

procedure TgmFigureObject.DuplicateBasicData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    FPenColor   := AFigureObj.PenColor;
    FBrushColor := AFigureObj.BrushColor;
    FBrushStyle := AFigureObj.BrushStyle;
    FPenWidth   := AFigureObj.PenWidth;
    FStartPoint := AFigureObj.FStartPoint;
    FEndPoint   := AFigureObj.FEndPoint;
    FSelected   := AFigureObj.IsSelected;
    FLocked     := AFigureObj.IsLocked;
    FName       := AFigureObj.Name;
    FRegular    := AFigureObj.IsRegular
  end;
end;

procedure TgmFigureObject.CalcOrigin;
begin
  if FFlag in [ffSquare, ffRoundSquare, ffCircle] then
  begin
    FOriginX := (FStartPoint.X + FEndPoint.X) div 2;
    FOriginY := (FStartPoint.Y + FEndPoint.Y) div 2;
  end
  else if FFlag = ffRegularPolygon then
  begin
    FOriginX := FStartPoint.X;
    FOriginY := FStartPoint.Y;
  end;
end;

procedure TgmFigureObject.CalcRadius;
begin
  if FFlag in [ffRegularPolygon, ffSquare, ffRoundSquare] then
  begin
    FRadius := Round(  Sqrt( Sqr(FEndPoint.X - FOriginX) + Sqr(FEndPoint.Y - FOriginY) )  );
  end
  else if FFlag = ffCircle then
  begin
    FRadius := FEndPoint.X - FOriginX;
  end;
end;

{ Make sure FStartPoint is at the upper left and FEndPoint is at the lower right. }
procedure TgmFigureObject.StandardizeOrder;
begin
  PointStandardizeOrder(FStartPoint, FEndPoint);
end;

procedure TgmFigureObject.Translate(const TranslateVector: TPoint);
var
  i: Integer;
begin
  if FLocked then
  begin
    Exit;
  end;
  
  FStartPoint := AddPoints(FStartPoint, TranslateVector);
  FEndPoint   := AddPoints(FEndPoint, TranslateVector);

  if FFlag = ffCurve then
  begin
    FCurvePoint1 := AddPoints(FCurvePoint1, TranslateVector);
    FCurvePoint2 := AddPoints(FCurvePoint2, TranslateVector);
  end;

  if FFlag in [ffPolygon, ffRegularPolygon] then
  begin
    for i := Low(FPolygonPoints) to High(FPolygonPoints) do
    begin
      FPolygonPoints[i] := AddPoints(FPolygonPoints[i], TranslateVector);
    end;
  end;
end;

procedure TgmFigureObject.DrawHandles(const ACanvas: TCanvas;
  const PenMode: TPenMode; const BrushStyle: TBrushStyle;
  const BrushColor: TColor; const Radius: Integer);
var
  i: Integer;
begin
  case FFlag of
    ffStraightLine:
      begin
        DrawHandle(ACanvas, FStartPoint, clBlack, BrushColor, BrushStyle, PenMode, Radius);
        DrawHandle(ACanvas, FEndPoint,   clBlack, BrushColor, BrushStyle, PenMode, Radius);
      end;

    ffCurve:
      begin
        DrawHandle(ACanvas, FStartPoint, clBlack, BrushColor, BrushStyle, PenMode, Radius);
        DrawHandle(ACanvas, FEndPoint,   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        if (FCurvePoint1.X = FCurvePoint2.X) and (FCurvePoint1.Y = FCurvePoint2.Y) then
        begin
          case FCurveControl of
            ccpFirst:
              begin
                DrawHandle(ACanvas, FCurvePoint1, clBlack, clYellow, BrushStyle, PenMode, Radius);
              end;

            ccpSecond:
              begin
                DrawHandle(ACanvas, FCurvePoint2, clBlack, clBlue,   BrushStyle, PenMode, Radius);
              end;
          end;
        end
        else
        begin
          DrawHandle(ACanvas, FCurvePoint1, clBlack, clYellow, BrushStyle, PenMode, Radius);
          DrawHandle(ACanvas, FCurvePoint2, clBlack, clBlue,   BrushStyle, PenMode, Radius);
        end;
      end;

    ffPolygon, ffRegularPolygon:
      begin
        if High(FPolygonPoints) > (-1) then
        begin
          for i := Low(FPolygonPoints) to High(FPolygonPoints) do
          begin
            DrawHandle(ACanvas, FPolygonPoints[i], clBlack, BrushColor, BrushStyle, PenMode, Radius);
          end;
        end;
      end;

    ffRectangle, ffRoundRectangle, ffEllipse:
      begin
        DrawHandle(ACanvas, FStartPoint, clBlack, BrushColor, BrushStyle, PenMode, Radius);
        DrawHandle(ACanvas, FEndPoint,   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas, Point(FStartPoint.X, FEndPoint.Y), clBlack, BrushColor,
                   BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas, Point(FEndPoint.X, FStartPoint.Y), clBlack, BrushColor,
                   BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas,
                   Point(FStartPoint.X, (FEndPoint.Y - FStartPoint.Y) div 2 + FStartPoint.Y),
                   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas,
                   Point(FEndPoint.X, (FEndPoint.Y - FStartPoint.Y) div 2 + FStartPoint.Y),
                   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas,
                   Point((FEndPoint.X - FStartPoint.X) div 2 + FStartPoint.X, FStartPoint.Y),
                   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas,
                   Point((FEndPoint.X - FStartPoint.X) div 2 + FStartPoint.X, FEndPoint.Y),
                   clBlack, BrushColor, BrushStyle, PenMode, Radius);
      end;

    ffSquare, ffRoundsquare, ffCircle:
      begin
        DrawHandle(ACanvas, FStartPoint, clBlack, BrushColor, BrushStyle, PenMode, Radius);
        DrawHandle(ACanvas, FEndPoint,   clBlack, BrushColor, BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas, Point(FStartPoint.X, FEndPoint.Y), clBlack, BrushColor,
                   BrushStyle, PenMode, Radius);

        DrawHandle(ACanvas, Point(FEndPoint.X, FStartPoint.Y), clBlack, BrushColor,
                   BrushStyle, PenMode, Radius);
      end;
  end;
end;

function TgmFigureObject.GetHandleAtPoint(const AX, AY, ARadius: Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): TgmDrawingHandle;
var
  i              : Integer;
  LStartPoint    : TPoint;
  LEndPoint      : TPoint;
  LCurvePoint1   : TPoint;
  LCurvePoint2   : TPoint;
  LPoint         : TPoint;
  p1, p2, p3, p4 : TPoint;
  p5, p6, p7, p8 : TPoint;
begin
  Result := dhNone;

  if FLocked then
  begin
    Exit;
  end;
  
  case FFlag of
    ffStraightLine:
      begin
        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint := ACoordConvertFunc(FStartPoint);
          LEndPoint   := ACoordConvertFunc(FEndPoint);
        end
        else
        begin
          LStartPoint := FStartPoint;
          LEndPoint   := FEndPoint;
        end;

        if SquareContainsPoint( LStartPoint, ARadius, Point(AX, AY) ) then
        begin
          Result := dhLineStart;
        end
        else
        if SquareContainsPoint( LEndPoint, ARadius, Point(AX, AY) ) then
        begin
          Result := dhLineEnd;
        end;
      end;

    ffCurve:
      begin
        if Assigned(ACoordConvertFunc) then
        begin
          LStartPoint  := ACoordConvertFunc(FStartPoint);
          LCurvePoint1 := ACoordConvertFunc(FCurvePoint1);
          LCurvePoint2 := ACoordConvertFunc(FCurvePoint2);
          LEndPoint    := ACoordConvertFunc(FEndPoint);
        end
        else
        begin
          LStartPoint  := FStartPoint;
          LCurvePoint1 := FCurvePoint1;
          LCurvePoint2 := FCurvePoint2;
          LEndPoint    := FEndPoint;
        end;

        if SquareContainsPoint( LStartPoint, ARadius, Point(AX, AY) ) then
        begin
          Result := dhLineStart;
        end
        else
        if SquareContainsPoint( LEndPoint, ARadius, Point(AX, AY) ) then
        begin
          Result := dhLineEnd;
        end
        else
        begin
          if (FCurvePoint1.X = FCurvePoint2.X) and
             (FCurvePoint1.Y = FCurvePoint2.Y) then
          begin
            case FCurveControl of
              ccpFirst:
                begin
                  if SquareContainsPoint( LCurvePoint1, ARadius, Point(AX, AY) ) then
                  begin
                    Result := dhCurvePoint1;
                  end;
                end;

              ccpSecond:
                begin
                  if SquareContainsPoint( LCurvePoint2, ARadius, Point(AX, AY) ) then
                  begin
                    Result := dhCurvePoint2;
                  end;
                end;
            end;
          end
          else
          begin
            if SquareContainsPoint( LCurvePoint1, ARadius, Point(AX, AY) ) then
            begin
              Result := dhCurvePoint1;
            end
            else
            if SquareContainsPoint( LCurvePoint2, ARadius, Point(AX, AY) ) then
            begin
              Result := dhCurvePoint2;
            end;
          end;
        end;
      end;

    ffPolygon,
    ffRegularPolygon:
      begin
        // Check every point of the polygon and determine whether there is one
        // is selected, if so, then exit the loop, and save the index of the
        // selected point to FPolygonCurrentPoints field, and change the point
        // of the polygon that with this index in MouseMove event.
          
        for i := Low(FPolygonPoints) to High(FPolygonPoints) do
        begin
          if Assigned(ACoordConvertFunc) then
          begin
            LPoint := ACoordConvertFunc(FPolygonPoints[i]);
          end
          else
          begin
            LPoint := FPolygonPoints[i];
          end;

          if SquareContainsPoint( LPoint, ARadius, Point(AX, AY) ) then
          begin
            FPolygonCurrentPointIndex := i;
            Result                    := dhPolygonPoint;
            Break;
          end;
        end;
      end;

    ffRectangle,
    ffRoundRectangle,
    ffEllipse:
      begin
        p1 := FStartPoint;
        p2 := FEndPoint;
        p3 := Point(FStartPoint.X, FEndPoint.Y);
        p4 := Point(FEndPoint.X, FStartPoint.Y);
        p5 := Point(FStartPoint.X, (FEndPoint.Y - FStartPoint.Y) div 2 + FStartPoint.Y);
        p6 := Point(FEndPoint.X, (FEndPoint.Y - FStartPoint.Y) div 2 + FStartPoint.Y);
        p7 := Point((FEndPoint.X - FStartPoint.X) div 2 + FStartPoint.X, FStartPoint.Y);
        p8 := Point((FEndPoint.X - FStartPoint.X) div 2 + FStartPoint.X, FEndPoint.Y);

        if Assigned(ACoordConvertFunc) then
        begin
          p1 := ACoordConvertFunc(p1);
          p2 := ACoordConvertFunc(p2);
          p3 := ACoordConvertFunc(p3);
          p4 := ACoordConvertFunc(p4);
          p5 := ACoordConvertFunc(p5);
          p6 := ACoordConvertFunc(p6);
          p7 := ACoordConvertFunc(p7);
          p8 := ACoordConvertFunc(p8);
        end;

        if SquareContainsPoint( p1, ARadius, Point(AX, AY) ) then
        begin
          Result := dhAxAy;
        end
        else
        if SquareContainsPoint( p2, ARadius, Point(AX, AY) ) then
        begin
          Result := dhBxBy;
        end
        else
        if SquareContainsPoint( p3, ARadius, Point(AX, AY) ) then
        begin
          Result := dhAxBy;
        end
        else
        if SquareContainsPoint( p4, ARadius, Point(AX, AY) ) then
        begin
          Result := dhBxAy;
        end
        else
        if SquareContainsPoint( p5, ARadius, Point(AX, AY) ) then
        begin
          Result := dhLeftHalfAYBY;
        end
        else
        if SquareContainsPoint( p6, ARadius, Point(AX, AY) ) then
        begin
          Result := dhRightHalfAYBY;
        end
        else
        if SquareContainsPoint( p7, ARadius, Point(AX, AY) ) then
        begin
          Result := dhTopHalfAXBX;
        end
        else
        if SquareContainsPoint( p8, ARadius, Point(AX, AY) ) then
        begin
          Result := dhBottomHalfAXBX;
        end;
      end;

    ffSquare,
    ffRoundSquare,
    ffCircle:
      begin
        p1 := FStartPoint;
        p2 := FEndPoint;
        p3 := Point(FStartPoint.X, FEndPoint.Y);
        p4 := Point(FEndPoint.X, FStartPoint.Y);

        if Assigned(ACoordConvertFunc) then
        begin
          p1 := ACoordConvertFunc(p1);
          p2 := ACoordConvertFunc(p2);
          p3 := ACoordConvertFunc(p3);
          p4 := ACoordConvertFunc(p4);
        end;

        if SquareContainsPoint( p1, ARadius, Point(AX, AY) ) then
        begin
          Result := dhAxAy;
        end
        else
        if SquareContainsPoint( p2, ARadius, Point(AX, AY) ) then
        begin
          Result := dhBxBy;
        end
        else
        if SquareContainsPoint( p3, ARadius, Point(AX, AY) ) then
        begin
          Result := dhAxBy;
        end
        else
        if SquareContainsPoint( p4, ARadius, Point(AX, AY) ) then
        begin
          Result := dhBxAy;
        end;
      end;
  end;
end;

function TgmFigureObject.ContainsPoint(const ATestPoint: TPoint;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  i            : Integer;
  LStartPoint  : TPoint;
  LEndPoint    : TPoint;
  LCurvePoint1 : TPoint;
  LCurvePoint2 : TPoint;
begin
  Result := False;

  if FFlag = ffStraightLine then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;
    
    Result := NearLine(ATestPoint, LStartPoint, LEndPoint);
  end
  else
  if FFlag = ffCurve then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint  := ACoordConvertFunc(FStartPoint);
      LCurvePoint1 := ACoordConvertFunc(FCurvePoint1);
      LCurvePoint2 := ACoordConvertFunc(FCurvePoint2);
      LEndPoint    := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint  := FStartPoint;
      LCurvePoint1 := FCurvePoint1;
      LCurvePoint2 := FCurvePoint2;
      LEndPoint    := FEndPoint;
    end;

    Result := PointOnCurve(LStartPoint.X, LStartPoint.Y,
                           LCurvePoint1.X, LCurvePoint1.Y,
                           LCurvePoint2.X, LCurvePoint2.Y,
                           LEndPoint.X, LEndPoint.Y,
                           ATestPoint);
  end
  else
  if FFlag in [ffPolygon, ffRegularPolygon] then
  begin
    for i := Low(FPolygonPoints) to High(FPolygonPoints) - 1 do
    begin
      if Assigned(ACoordConvertFunc) then
      begin
        LStartPoint := ACoordConvertFunc(FPolygonPoints[i]);
        LEndPoint   := ACoordConvertFunc(FPolygonPoints[i + 1]);
      end
      else
      begin
        LStartPoint := FPolygonPoints[i];
        LEndPoint   := FPolygonPoints[i + 1];
      end;

      if NearLine(ATestPoint, LStartPoint, LEndPoint) then
      begin
        Result := True;
        Break;
      end;
    end;

    if Result = False then
    begin
      i := High(FPolygonPoints);

      if Assigned(ACoordConvertFunc) then
      begin
        LStartPoint := ACoordConvertFunc(FPolygonPoints[i]);
        LEndPoint   := ACoordConvertFunc(FPolygonPoints[0]);
      end
      else
      begin
        LStartPoint := FPolygonPoints[i];
        LEndPoint   := FPolygonPoints[0];
      end;


      Result := NearLine(ATestPoint, LStartPoint, LEndPoint);
    end;
  end
  else
  if (FFlag = ffRectangle) or
     (FFlag = ffRoundRectangle) or
     (FFlag = ffEllipse)or
     (FFlag = ffSquare) or
     (FFlag = ffRoundSquare) or
     (FFlag = ffCircle) then
  begin
    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;

    Result := Windows.PtInRect( Rect(LStartPoint.X, LStartPoint.Y,
                                     LEndPoint.X, LEndPoint.Y),
                                ATestPoint );
  end;
end;

//-- TgmLineObject -------------------------------------------------------------

constructor TgmLineObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);
  
  FFlag       := ffStraightLine;
  FStartPoint := StartPoint;
  FEndPoint   := EndPoint;
end; 

procedure TgmLineObject.DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
  const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth             : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LStartPoint, LEndPoint: TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;

    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    LStartPoint.X := Offset.X + MulDiv(FStartPoint.X, Factor, 100);
    LStartPoint.Y := Offset.Y + MulDiv(FStartPoint.Y, Factor, 100);
    LEndPoint.X   := Offset.X + MulDiv(FEndPoint.X,   Factor, 100);
    LEndPoint.Y   := Offset.Y + MulDiv(FEndPoint.Y,   Factor, 100);
    
    MoveTo(LStartPoint.X, LStartPoint.Y);
    LineTo(LEndPoint.X,   LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmLineObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor   : TColor;
  LBrushColor : TColor;
  LPenWidth   : Integer;
  LPenStyle   : TPenStyle;
  LPenMode    : TPenMode;
  LBrushStyle : TBrushStyle;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;
    
    MoveTo(LStartPoint.X, LStartPoint.Y);
    LineTo(LEndPoint.X, LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmLineObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmLineObject.Create(FPenColor, FBrushColor, FPenStyle, FBrushStyle,
                                 FPenWidth, FStartPoint, FEndPoint);

  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmLineObject.AssignData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);
  end;
end;

procedure TgmLineObject.SaveToStream(const AStream: TStream);
var
  LIntValue : Integer;
  LStrValue : ShortString; 
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FEndPoint.X, 4);
    AStream.Write(FEndPoint.Y, 4);

    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmCurveObject ------------------------------------------------------------

constructor TgmCurveObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const P1, P2, P3, P4: TPoint);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FFlag        := ffCurve;
  FStartPoint  := P1;
  FCurvePoint1 := P2;
  FCurvePoint2 := P3;
  FEndPoint    := P4;

  { If both the control points are at the same position, in the default,
    the second control point is selected. }
  if (FCurvePoint1.X = FCurvePoint2.X) and
     (FCurvePoint1.Y = FCurvePoint2.Y) then
  begin
    FCurveControl := ccpSecond;
  end;
end;

procedure TgmCurveObject.DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
  const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth             : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  P1, P2, P3, P4        : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;
    
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    P1.X := Offset.X + MulDiv(FStartPoint.X,  Factor, 100);
    P1.Y := Offset.Y + MulDiv(FStartPoint.Y,  Factor, 100);
    P2.X := Offset.X + MulDiv(FCurvePoint1.X, Factor, 100);
    P2.Y := Offset.Y + MulDiv(FCurvePoint1.Y, Factor, 100);
    P3.X := Offset.X + MulDiv(FCurvePoint2.X, Factor, 100);
    P3.Y := Offset.Y + MulDiv(FCurvePoint2.Y, Factor, 100);
    P4.X := Offset.X + MulDiv(FEndPoint.X,    Factor, 100);
    P4.Y := Offset.Y + MulDiv(FEndPoint.Y,    Factor, 100);
    PolyBezier([P1, P2, P3, P4]);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmCurveObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor      : TColor;
  LBrushColor    : TColor;
  LPenWidth      : Integer;
  LPenStyle      : TPenStyle;
  LPenMode       : TPenMode;
  LBrushStyle    : TBrushStyle;
  P1, P2, P3, P4 : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);


    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Assigned(ACoordConvertFunc) then
    begin
      P1 := ACoordConvertFunc(FStartPoint);
      P2 := ACoordConvertFunc(FCurvePoint1);
      P3 := ACoordConvertFunc(FCurvePoint2);
      P4 := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      P1 := FStartPoint;
      P2 := FCurvePoint1;
      P3 := FCurvePoint2;
      P4 := FEndPoint;
    end;
    
    PolyBezier([P1, P2, P3, P4]);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmCurveObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmCurveObject.Create(FPenColor, FBrushColor, FPenStyle, FBrushStyle,
                                  FPenWidth, FStartPoint, FCurvePoint1,
                                  FCurvePoint2, FEndPoint);

  Result.IsSelected   := Self.FSelected;
  Result.IsLocked     := Self.FLocked;
  Result.Name         := Self.FName;
  Result.CurveControl := Self.FCurveControl;
end;

procedure TgmCurveObject.AssignData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);

    FCurvePoint1  := AFigureObj.FCurvePoint1;
    FCurvePoint2  := AFigureObj.FCurvePoint2;
    FCurveControl := AFigureObj.CurveControl;
  end;
end;

procedure TgmCurveObject.SaveToStream(const AStream: TStream);
var
  LIntValue : Integer;
  LStrValue : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));
    
    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);
    
    AStream.Write(FPenWidth, 4);

    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FCurvePoint1.X, 4);
    AStream.Write(FCurvePoint1.Y, 4);
    AStream.Write(FCurvePoint2.X, 4);
    AStream.Write(FCurvePoint2.Y, 4);
    AStream.Write(FEndPoint.X, 4);
    AStream.Write(FEndPoint.Y, 4);

    LIntValue := Ord(FCurveControl);
    AStream.Write(LIntValue, 4);
    
    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmPolygonObject ----------------------------------------------------------

constructor TgmPolygonObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const APolygon: array of TPoint);
var
  i: Integer;
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FFlag        := ffPolygon;
  FFillPolygon := True;

  SetLength(FPolygonPoints, 0);

  if High(APolygon) > -1 then
  begin
    for i := Low(APolygon) to High(APolygon) do
    begin
      SetLength(FPolygonPoints, High(FPolygonPoints) + 2);
      FPolygonPoints[High(FPolygonPoints)] := APolygon[i];
    end;
  end;
end;

procedure TgmPolygonObject.DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
  const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth, i          : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LOffsetPolygon        : array of TPoint;
begin
  SetLength(LOffsetPolygon, 0);

  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;
    
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    if High(FPolygonPoints) > -1 then
    begin
      for i := Low(FPolygonPoints) to High(FPolygonPoints) do
      begin
        SetLength( LOffsetPolygon, High(LOffsetPolygon) + 2 );
        
        LOffsetPolygon[High(LOffsetPolygon)].X := Offset.X + MulDiv(FPolygonPoints[i].X, Factor, 100);
        LOffsetPolygon[High(LOffsetPolygon)].Y := Offset.Y + MulDiv(FPolygonPoints[i].Y, Factor, 100);
      end;

      if Self.FFillPolygon then
      begin
        Polygon(LOffsetPolygon);
      end
      else
      begin
        PolyLine(LOffsetPolygon);
      end;
    end;

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmPolygonObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i, LIndex      : Integer;
  LPenColor      : TColor;
  LBrushColor    : TColor;
  LPenWidth      : Integer;
  LPenStyle      : TPenStyle;
  LPenMode       : TPenMode;
  LBrushStyle    : TBrushStyle;
  LOffsetPolygon : array of TPoint;
begin
  SetLength(LOffsetPolygon, 0);

  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Length(FPolygonPoints) > 0 then
    begin
      for i := 0 to High(FPolygonPoints) do
      begin
        SetLength( LOffsetPolygon, Length(LOffsetPolygon) + 1 );
        LIndex := High(LOffsetPolygon);

        if Assigned(ACoordConvertFunc) then
        begin
          LOffsetPolygon[LIndex] := ACoordConvertFunc(FPolygonPoints[i]);
        end
        else
        begin
          LOffsetPolygon[LIndex] := FPolygonPoints[i];
        end;
      end;

      if Self.FFillPolygon then
      begin
        Polygon(LOffsetPolygon);
      end
      else
      begin
        PolyLine(LOffsetPolygon);
      end;
    end;

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmPolygonObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmPolygonObject.Create(FPenColor, FBrushColor, FPenStyle,
                                    FBrushStyle, FPenWidth, FPolygonPoints);

  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmPolygonObject.AssignData(const AFigureObj: TgmFigureObject);
var
  i: Integer;
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);

    FPolygonCurrentPointIndex := AFigureObj.PolygonCurrentPointIndex;
    
    SetLength( FPolygonPoints, High(AFigureObj.FPolygonPoints) + 1 );

    for i := 0 to High(FPolygonPoints) do
    begin
      FPolygonPoints[i] := AFigureObj.FPolygonPoints[i];
    end;
  end;
end;

procedure TgmPolygonObject.SaveToStream(const AStream: TStream);
var
  LIntValue, i : Integer;
  LStrValue    : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    // vertex count
    LIntValue := High(FPolygonPoints) + 1;
    AStream.Write(LIntValue, 4);

    if LIntValue > 0 then
    begin
      for i := Low(FPolygonPoints) to High(FPolygonPoints) do
      begin
        AStream.Write(FPolygonPoints[i].X, 4);
        AStream.Write(FPolygonPoints[i].Y, 4);
      end;
    end;

    AStream.Write(FFillPolygon, 1);
    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmRegularPolygonObject ---------------------------------------------------

constructor TgmRegularPolygonObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth, Sides: Integer; const CenterPoint, CurrentPoint: TPoint);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FFlag  := ffRegularPolygon;
  FSides := Sides;

  SetLength(FPolygonPoints, FSides + 1);

  FStartPoint := CenterPoint;
  FEndPoint   := CurrentPoint;
  
  CalcRegularPolygonVertices(FPolygonPoints, CenterPoint, CurrentPoint, FSides);
  CalcOrigin;
  CalcRadius;
end;

procedure TgmRegularPolygonObject.DrawFigure(ACanvas: TCanvas;
  const PenMode: TPenMode; const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  i, LPenWidth          : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LOffsetPolygon        : array of TPoint;
begin
  SetLength(LOffsetPolygon, 0);
  
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;

    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    if High(FPolygonPoints) > (-1) then
    begin
      for i := Low(FPolygonPoints) to High(FPolygonPoints) do
      begin
        SetLength( LOffsetPolygon, High(LOffsetPolygon) + 2 );

        LOffsetPolygon[High(LOffsetPolygon)].X := Offset.X + MulDiv(FPolygonPoints[i].X, Factor, 100);
        LOffsetPolygon[High(LOffsetPolygon)].Y := Offset.Y + MulDiv(FPolygonPoints[i].Y, Factor, 100);
      end;

      Polygon(LOffsetPolygon);
    end;

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmRegularPolygonObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  i, LIndex      : Integer;
  LPenColor      : TColor;
  LBrushColor    : TColor;
  LPenWidth      : Integer;
  LPenStyle      : TPenStyle;
  LPenMode       : TPenMode;
  LBrushStyle    : TBrushStyle;
  LOffsetPolygon : array of TPoint;
begin
  SetLength(LOffsetPolygon, 0);
  
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Length(FPolygonPoints) > 0 then
    begin
      for i := 0 to High(FPolygonPoints) do
      begin
        SetLength( LOffsetPolygon, Length(LOffsetPolygon) + 1 );
        LIndex := High(LOffsetPolygon);

        if Assigned(ACoordConvertFunc) then
        begin
          LOffsetPolygon[LIndex] := ACoordConvertFunc(FPolygonPoints[i]);
        end
        else
        begin
          LOffsetPolygon[LIndex] := FPolygonPoints[i];
        end;
      end;

      Polygon(LOffsetPolygon);
    end;

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmRegularPolygonObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmRegularPolygonObject.Create(FPenColor, FBrushColor, FPenStyle,
                                           FBrushStyle, FPenWidth, FSides,
                                           FStartPoint, FEndPoint);

  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmRegularPolygonObject.AssignData(const AFigureObj: TgmFigureObject);
var
  i: Integer;
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);

    FSides   := AFigureObj.Sides;
    FOriginX := AFigureObj.OriginPixelX;
    FOriginY := AFigureObj.OriginPixelY;
    FRadius  := AFigureObj.RadiusPixel;

    SetLength( FPolygonPoints, High(AFigureObj.FPolygonPoints) + 1 );
    
    for i := 0 to High(FPolygonPoints) do
    begin
      FPolygonPoints[i] := AFigureObj.FPolygonPoints[i];
    end;
  end;
end;

procedure TgmRegularPolygonObject.SaveToStream(const AStream: TStream);
var
  LIntValue, i : Integer;
  LStrValue    : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FEndPoint.X,   4);
    AStream.Write(FEndPoint.Y,   4);

    AStream.Write(FOriginX, 4);
    AStream.Write(FOriginY, 4);
    AStream.Write(FRadius,  4);

    // side count
    AStream.Write(FSides, 4);

    if FSides > 2 then
    begin
      for i := Low(FPolygonPoints) to High(FPolygonPoints) do
      begin
        AStream.Write(FPolygonPoints[i].X, 4);
        AStream.Write(FPolygonPoints[i].Y, 4);
      end;
    end;

    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmRectangleObject --------------------------------------------------------

constructor TgmRectangleObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
  const Regular: Boolean);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FStartPoint := StartPoint;
  FEndPoint   := EndPoint;
  
  if Regular then
  begin
    FRegular := True;
    FFlag    := ffSquare;
    
    CalcOrigin;
    CalcRadius;
  end
  else
  begin
    FFlag := ffRectangle;
  end;
end;

procedure TgmRectangleObject.DrawFigure(ACanvas: TCanvas;
  const PenMode: TPenMode; const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth             : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LStartPoint, LEndPoint: TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;

    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    LStartPoint.X := Offset.X + MulDiv(FStartPoint.X, Factor, 100);
    LStartPoint.Y := Offset.Y + MulDiv(FStartPoint.Y, Factor, 100);
    LEndPoint.X   := Offset.X + MulDiv(FEndPoint.X,   Factor, 100);
    LEndPoint.Y   := Offset.Y + MulDiv(FEndPoint.Y,   Factor, 100);
    
    Rectangle(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmRectangleObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor   : TColor;
  LBrushColor : TColor;
  LPenWidth   : Integer;
  LPenStyle   : TPenStyle;
  LPenMode    : TPenMode;
  LBrushStyle : TBrushStyle;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;
    
    Rectangle(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmRectangleObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmRectangleObject.Create(FPenColor, FBrushColor, FPenStyle,
                                      FBrushStyle, FPenWidth, FStartPoint,
                                      FEndPoint, FRegular);

  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmRectangleObject.AssignData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);

    if FFlag = ffSquare then
    begin
      FOriginX := AFigureObj.OriginPixelX;
      FOriginY := AFigureObj.OriginPixelY;
      FRadius  := AFigureObj.RadiusPixel;
    end;
  end;
end; 

procedure TgmRectangleObject.SaveToStream(const AStream: TStream);
var
  LIntValue : Integer;
  LStrValue : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FEndPoint.X, 4);
    AStream.Write(FEndPoint.Y, 4);

    AStream.Write(FOriginX, 4);
    AStream.Write(FOriginY, 4);
    AStream.Write(FRadius, 4);
    AStream.Write(FRegular, 1);
    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmRoundRectangleObject ---------------------------------------------------

constructor TgmRoundRectangleObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
  const CornerRadius: Integer; const Regular: Boolean);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FStartPoint        := StartPoint;
  FEndPoint          := EndPoint;
  FRoundCornerRadius := CornerRadius;
  
  if Regular then
  begin
    FRegular := True;
    FFlag    := ffRoundSquare;

    CalcOrigin;
    CalcRadius;
  end
  else
  begin
    FFlag := ffRoundRectangle;
  end;
end;

procedure TgmRoundRectangleObject.DrawFigure(ACanvas: TCanvas;
  const PenMode: TPenMode; const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth             : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LStartPoint, LEndPoint: TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;
    
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    LStartPoint.X := Offset.X + MulDiv(FStartPoint.X, Factor, 100);
    LStartPoint.Y := Offset.Y + MulDiv(FStartPoint.Y, Factor, 100);
    LEndPoint.X   := Offset.X + MulDiv(FEndPoint.X,   Factor, 100);
    LEndPoint.Y   := Offset.Y + MulDiv(FEndPoint.Y,   Factor, 100);
    
    RoundRect(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y,
              FRoundCornerRadius, FRoundCornerRadius);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmRoundRectangleObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor   : TColor;
  LBrushColor : TColor;
  LPenWidth   : Integer;
  LPenStyle   : TPenStyle;
  LPenMode    : TPenMode;
  LBrushStyle : TBrushStyle;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;

    RoundRect(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y,
              FRoundCornerRadius, FRoundCornerRadius);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmRoundRectangleObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmRoundRectangleObject.Create(FPenColor, FBrushColor, FPenStyle,
                                           FBrushStyle, FPenWidth, FStartPoint,
                                           FEndPoint, FRoundCornerRadius,
                                           FRegular);
                                       
  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmRoundRectangleObject.AssignData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);
    FRoundCornerRadius := AFigureObj.RoundCornerRadius;
    
    if FFlag = ffRoundSquare then
    begin
      FOriginX := AFigureObj.OriginPixelX;
      FOriginY := AFigureObj.OriginPixelY;
      FRadius  := AFigureObj.RadiusPixel;
    end;
  end;
end;

procedure TgmRoundRectangleObject.SaveToStream(const AStream: TStream);
var
  LIntValue : Integer;
  LStrValue : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    AStream.Write(FRoundCornerRadius, 4);
    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FEndPoint.X, 4);
    AStream.Write(FEndPoint.Y, 4);

    AStream.Write(FOriginX, 4);
    AStream.Write(FOriginY, 4);
    AStream.Write(FRadius, 4);
    AStream.Write(FRegular, 1);
    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmEllipseObject ----------------------------------------------------------

constructor TgmEllipseObject.Create(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
  const Regular: Boolean);
begin
  inherited Create(PenColor, BrushColor, PenStyle, BrushStyle, PenWidth);

  FStartPoint := StartPoint;
  FEndPoint   := EndPoint;

  if Regular then
  begin
    FRegular := True;
    FFlag    := ffCircle;
    
    CalcOrigin;
    CalcRadius;
  end
  else
  begin
    FFlag := ffEllipse;
  end;
end;

procedure TgmEllipseObject.DrawFigure(ACanvas: TCanvas; const PenMode: TPenMode;
  const Offset: TPoint; const Factor: Integer;
  const FigureDrawMode: TgmFigureDrawMode);
var
  LPenColor, LBrushColor: TColor;
  LPenWidth             : Integer;
  LPenStyle             : TPenStyle;
  LPenMode              : TPenMode;
  LBrushStyle           : TBrushStyle;
  LStartPoint, LEndPoint: TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    case FigureDrawMode of
      fdmRGB:
        begin
          Pen.Color   := FPenColor;
          Brush.Color := FBrushColor;
        end;

      fdmMask:
        begin
          Pen.Color   := clWhite;
          Brush.Color := clWhite;
        end;
    end;

    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := PenMode;
    Brush.Style := FBrushStyle;

    LStartPoint.X := Offset.X + MulDiv(FStartPoint.X, Factor, 100);
    LStartPoint.Y := Offset.Y + MulDiv(FStartPoint.Y, Factor, 100);
    LEndPoint.X   := Offset.X + MulDiv(FEndPoint.X,   Factor, 100);
    LEndPoint.Y   := Offset.Y + MulDiv(FEndPoint.Y,   Factor, 100);
    
    Ellipse(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

procedure TgmEllipseObject.DrawFigure(ACanvas: TCanvas;
  const APenMode: TPenMode; ACoordConvertFunc: TgmPointCoordConvertFunc = nil);
var
  LPenColor   : TColor;
  LBrushColor : TColor;
  LPenWidth   : Integer;
  LPenStyle   : TPenStyle;
  LPenMode    : TPenMode;
  LBrushStyle : TBrushStyle;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
begin
  with ACanvas do
  begin
    GetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);

    Pen.Color   := FPenColor;
    Brush.Color := FBrushColor;
    Pen.Style   := FPenStyle;
    Pen.Width   := FPenWidth;
    Pen.Mode    := APenMode;
    Brush.Style := FBrushStyle;

    if Assigned(ACoordConvertFunc) then
    begin
      LStartPoint := ACoordConvertFunc(FStartPoint);
      LEndPoint   := ACoordConvertFunc(FEndPoint);
    end
    else
    begin
      LStartPoint := FStartPoint;
      LEndPoint   := FEndPoint;
    end;
    
    Ellipse(LStartPoint.X, LStartPoint.Y, LEndPoint.X, LEndPoint.Y);

    SetCanvasProperties(ACanvas, LPenColor, LBrushColor, LPenWidth,
                        LPenStyle, LPenMode, LBrushStyle);
  end;
end;

function TgmEllipseObject.GetSelfBackup: TgmFigureObject;
begin
  Result := TgmEllipseObject.Create(FPenColor, FBrushColor, FPenStyle,
                                    FBrushStyle, FPenWidth, FStartPoint,
                                    FEndPoint, FRegular);
                                
  Result.IsSelected := Self.FSelected;
  Result.IsLocked   := Self.FLocked;
  Result.Name       := Self.FName;
end;

procedure TgmEllipseObject.AssignData(const AFigureObj: TgmFigureObject);
begin
  if Assigned(AFigureObj) then
  begin
    DuplicateBasicData(AFigureObj);
    
    if FFlag = ffCircle then
    begin
      FOriginX := AFigureObj.OriginPixelX;
      FOriginY := AFigureObj.OriginPixelY;
      FRadius  := AFigureObj.RadiusPixel;
    end;
  end;
end; 

procedure TgmEllipseObject.SaveToStream(const AStream: TStream);
var
  LIntValue : Integer;
  LStrValue : ShortString;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FFlag);
    AStream.Write(LIntValue, 4);

    LStrValue := FName;
    AStream.Write(LStrValue, SizeOf(LStrValue));

    AStream.Write(FPenColor, SizeOf(TColor));
    AStream.Write(FBrushColor, SizeOf(TColor));

    LIntValue := Ord(FPenStyle);
    AStream.Write(LIntValue, 4);

    LIntValue := Ord(FBrushStyle);
    AStream.Write(LIntValue, 4);

    AStream.Write(FPenWidth, 4);

    AStream.Write(FStartPoint.X, 4);
    AStream.Write(FStartPoint.Y, 4);
    AStream.Write(FEndPoint.X, 4);
    AStream.Write(FEndPoint.Y, 4);

    AStream.Write(FOriginX, 4);
    AStream.Write(FOriginY, 4);
    AStream.Write(FRadius, 4);
    AStream.Write(FRegular, 1);
    AStream.Write(FLocked, 1);
  end;
end;

//-- TgmFigureList -------------------------------------------------------------

constructor TgmFigureList.Create;
begin
  inherited Create;

  FIndexOfSelected      := FIGURE_NOT_SELECTED;
  FLineNumber           := 0;
  FCurveNumber          := 0;
  FPolygonNumber        := 0;
  FRegularPolygonNumber := 0;
  FRectangleNumber      := 0;
  FSquareNumber         := 0;
  FRoundRectangleNumber := 0;
  FRoundSquareNumber    := 0;
  FEllipseNumber        := 0;
  FCircleNumber         := 0;
end;

destructor TgmFigureList.Destroy;
begin
  // Free items in List
  DeleteAllFigures;
  inherited Destroy;
end;

procedure TgmFigureList.DeselectAllFigures;
var
  i          : Integer;
  LFigureObj : TgmFigureObject;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LFigureObj := TgmFigureObject(Self.Items[i]);

      if LFigureObj.IsSelected then
      begin
        LFigureObj.IsSelected := False;
      end;
    end;
  end;
end;

procedure TgmFigureList.DuplicateFigureList(const AList: TgmFigureList);
var
  i            : Integer;
  LFigureObj   : TgmFigureObject;
  LFigureBackup: TgmFigureObject;
begin
  if Assigned(AList) then
  begin
    if AList.Count > 0 then
    begin
      Self.DeleteAllFigures;

      for i := 0 to AList.Count - 1 do
      begin
        LFigureObj    := TgmFigureObject(AList.Items[i]);
        LFigureBackup := LFigureObj.GetSelfBackup;

        Self.Add(LFigureBackup);
      end;

      FIndexOfSelected      := AList.SelectedIndex;
      FLineNumber           := AList.LineNumber;
      FCurveNumber          := AList.CurveNumber;
      FPolygonNumber        := AList.PolygonNumber;
      FRegularPolygonNumber := AList.RegularPolygonNumber;
      FRectangleNumber      := AList.RectangleNumber;
      FSquareNumber         := AList.SquareNumber;
      FRoundRectangleNumber := AList.RoundRectangleNumber;
      FRoundSquareNumber    := AList.RoundSquareNumber;
      FEllipseNumber        := AList.EllipseNumber;
      FCircleNumber         := AList.CircleNumber;
    end;
  end;
end;

function TgmFigureList.InSelect(const X, Y, X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := ( (X >= X1) and (X <= X2) and (Y >= Y1) and (Y <= Y2) );
end;

function TgmFigureList.InRect(const AFigureObj: TgmFigureObject;
  const X, Y: Integer; const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  if AFigureObj.Flag = ffStraightLine then
  begin
    Result := IfInLine(X1, Y1, X2, Y2, X, Y);
  end
  else
  if (AFigureObj.Flag = ffRectangle) or
     (AFigureObj.Flag = ffSquare) then
  begin
    Result := IfInRectangle(X1, Y1, X2, Y2, X, Y);
  end
  else
  if (AFigureObj.Flag = ffRoundRectangle) or
     (AFigureObj.Flag = ffRoundSquare) then
  begin
    Result := IfInRectangle(X1, Y1, X2, Y2, X, Y);
  end
  else
  if (AFigureObj.Flag = ffEllipse) or
     (AFigureObj.Flag = ffCircle) then
  begin
    Result := IfInEllipse(X1, Y1, X2, Y2, X, Y);
  end
  else
  if AFigureObj.Flag = ffCurve then
  begin
    Result := PointOnCurve( X1, Y1, X3, Y3, X4, Y4, X2, Y2, Point(X, Y) );
  end
  else
  if AFigureObj.Flag in [ffPolygon, ffRegularPolygon] then
  begin
    for i := Low(AFigureObj.FPolygonPoints) to High(AFigureObj.FPolygonPoints) - 1 do
    begin
      if IfInLine(AFigureObj.FPolygonPoints[i].X,
                  AFigureObj.FPolygonPoints[i].Y,
                  AFigureObj.FPolygonPoints[i + 1].X,
                  AFigureObj.FPolygonPoints[i + 1].Y,
                  X, Y) then
      begin
        AFigureObj.IsSelected := True;
        Result                := True;
        Break;
      end;
    end;
  end;
end; 

function TgmFigureList.AllInRect(const AFigureObj: TgmFigureObject;
  const X1, Y1, X2, Y2, X3, Y3, X4, Y4, StartX, StartY, EndX, EndY: Integer): Boolean;
var
  P1, P2: TPoint;
  LRect : TRect;
  i     : Integer;
begin
  Result := False;

  if AFigureObj.Flag in [ffStraightLine, ffRectangle, ffSquare, ffRoundRectangle,
                         ffRoundSquare, ffEllipse, ffCircle] then
  begin
    Result := ( InSelect(X1, Y1, StartX, StartY, EndX, EndY) and
                InSelect(X2, Y2, StartX, StartY, EndX, EndY) );
  end
  else
  if AFigureObj.Flag = ffCurve then
  begin
    CalcCurvePoint(X1, Y1, X3, Y3, X4, Y4, X2, Y2, P1, P2);

    Result := ( InSelect(P1.X, P1.Y, StartX, StartY, EndX, EndY) and
                InSelect(P2.X, P2.Y, StartX, StartY, EndX, EndY) );
  end
  else
  if AFigureObj.Flag in [ffPolygon, ffRegularPolygon] then
  begin
    LRect := Rect(1000000, 1000000, -1000000, -1000000);

    for i := Low(AFigureObj.FPolygonPoints) to High(AFigureObj.FPolygonPoints) do
    begin
      if AFigureObj.FPolygonPoints[i].X < LRect.Left then
      begin
        LRect.Left := AFigureObj.FPolygonPoints[i].X;
      end;

      if AFigureObj.FPolygonPoints[i].X > LRect.Right then
      begin
        LRect.Right := AFigureObj.FPolygonPoints[i].X;
      end;

      if AFigureObj.FPolygonPoints[i].Y < LRect.Top then
      begin
        LRect.Top := AFigureObj.FPolygonPoints[i].Y;
      end;

      if AFigureObj.FPolygonPoints[i].Y > LRect.Bottom then
      begin
        LRect.Bottom := AFigureObj.FPolygonPoints[i].Y;
      end;
    end;
    
    Result := ( InSelect(LRect.Left,  LRect.Top,    StartX, StartY, EndX, EndY) and
                InSelect(LRect.Right, LRect.Bottom, StartX, StartY, EndX, EndY) )
  end;
end;

function TgmFigureList.GetLockedFigureCount: Integer;
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  Result := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LFigureObj := TgmFigureObject(Self.Items[i]);

      if LFigureObj.IsLocked then
      begin
        Inc(Result);
      end;
    end;
  end;
end; 

// Increment is intended to be +1 or -1 to select the next or last figure.
procedure TgmFigureList.SelectedFigureIncrementIndex(const Increment: Integer);
var
  LCurrentlySelectedIndex: Integer;
begin
  if Self.Count > 0 then  // Do nothing if there are no figures to select
  begin
    if FIndexOfSelected = FIGURE_NOT_SELECTED then
    begin
      if Increment > 0 then
      begin
        FIndexOfSelected := 0;               // first in list
      end
      else
      begin
        FIndexOfSelected := Self.Count - 1;  // last in list
      end;

      TgmFigureObject(Self.Items[FIndexOfSelected]).IsSelected := True;
    end
    else
    begin
      { Save this index since SetSelectedFlags (next) as a side-effect
        alters value. }
      LCurrentlySelectedIndex := FIndexOfSelected;

      // Clear all flags in case multiple figures are currently selected
      SetSelectedFlags(False);
      FIndexOfSelected := LCurrentlySelectedIndex + Increment;

      if FIndexOfSelected = Self.Count then
      begin
        FIndexOfSelected := 0;                  // wrap around
      end
      else if FIndexOfSelected < 0 then
      begin
        FIndexOfSelected := Self.Count - 1;     // wrap around
      end;
      
      TgmFigureObject(Self.Items[FIndexOfSelected]).IsSelected := True;
    end;
  end;
end;

function TgmFigureList.SelectedFigureCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      if TgmFigureObject(Self.Items[i]).IsSelected then
      begin
        Inc(Result);
      end;
    end;
  end
end;

function TgmFigureList.SelectedContainsPoint(const ATargetPoint: TPoint;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  LFound     : Boolean;
  i          : Integer;
  LFigureObj : TgmFigureObject;
begin
  LFound := False;
  i      := 0;

  while (not LFound) and (i < Self.Count) do
  begin
    LFigureObj := TgmFigureObject(Self.Items[i]);
    
    if LFigureObj.IsSelected then
    begin
      LFound := LFigureObj.ContainsPoint(ATargetPoint, ACoordConvertFunc);
    end;
    
    Inc(i);
  end;

  Result := LFound;
end; 

procedure TgmFigureList.SelectAllFigures;
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to Self.Count - 1 do
    begin
      LFigureObj            := TgmFigureObject(Self.Items[i]);
      LFigureObj.IsSelected := True;
    end;
  end;
end; 

procedure TgmFigureList.DeleteSelectedFigures;
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  // Don't do anything unless at least one figure is selected
  if SelectedFigureCount = 0 then
  begin
    FIndexOfSelected := FIGURE_NOT_SELECTED;
  end
  else
  begin
    // Go in reverse order so indices are not affected by deletions.
    for i := (Self.Count - 1) downto 0 do
    begin
      LFigureObj := TgmFigureObject(Self.Items[i]);

      if LFigureObj.IsSelected then
      begin
        Self.Delete(i);
        FreeAndNil(LFigureObj);
      end;
    end;

    // If any items are left, make the last one selected.
    if Self.Count = 0 then
    begin
      FIndexOfSelected := FIGURE_NOT_SELECTED;
    end
    else
    begin
      FIndexOfSelected := Self.Count - 1;
      TgmFigureObject(Self.Items[FIndexOfSelected]).IsSelected := True;
    end;
  end;
end;

procedure TgmFigureList.DeleteAllFigures;
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  for i := (Self.Count - 1) downto 0 do
  begin
    LFigureObj := TgmFigureObject(Self.Items[i]);
    LFigureObj.Free;
  end;
  
  Self.Clear;
end;

procedure TgmFigureList.DeleteFigureByIndex(const Index: Integer);
var
  LFigureObj: TgmFigureObject;
begin
  if Self.Count > 0 then
  begin
    if (Index >= 0) and (Index < Self.Count) then
    begin
      LFigureObj := TgmFigureObject(Self.Items[Index]);

      Self.Delete(Index);
      LFigureObj.Free;
    end;
  end;
end; 

procedure TgmFigureList.DrawAllFigures(ACanvas: TCanvas;
  const PenMode: TPenMode; const FigureDrawMode: TgmFigureDrawMode);
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  for i := 0 to (Self.Count - 1) do
  begin
    LFigureObj := TgmFigureObject(Self.Items[i]);
    
    // Each figure "knows" how to draw itself
    LFigureObj.DrawFigure( ACanvas, PenMode, Point(0, 0), 100, FigureDrawMode);
  end;
end; 

// Just redraw selected figures so this can be used for XORing.
procedure TgmFigureList.DrawSelectedFigures(ACanvas: TCanvas;
  const PenMode: TPenMode; const FigureDrawMode: TgmFigureDrawMode);
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  for i := (Self.Count - 1) downto 0 do
  begin
    LFigureObj := TgmFigureObject(Self.Items[i]);
    
    if LFigureObj.IsSelected then
    begin
      LFigureObj.DrawFigure( ACanvas, PenMode, Point(0, 0), 100, FigureDrawMode );
    end;
  end;
end; 

function TgmFigureList.GetSelectedFigure: TgmFigureObject;
begin
  if FIndexOfSelected = FIGURE_NOT_SELECTED then
  begin
    Result := nil;
  end
  else
  begin
    Result := TgmFigureObject(Self.Items[FIndexOfSelected]);
  end;
end;

procedure TgmFigureList.TranslateSelectedFigures(const TranslateVector: TPoint);
var
  i         : Integer;
  LFigureObj: TgmFigureObject;
begin
  for i := (Self.Count - 1) downto 0 do
  begin
    LFigureObj := TgmFigureObject(Self.Items[i]);

    if LFigureObj.IsSelected then
    begin
      LFigureObj.Translate(TranslateVector);
    end;
  end;
end;

procedure TgmFigureList.SelectFigures(const Shift: TShiftState;
  const X, Y: Integer);
var
  i          : Integer;
  LNotFound  : Boolean;
  LRectFigure: TgmFigureObject;
  LTestPoint : TPoint;
begin
  LTestPoint := Point(X, Y);
  
  if not (ssShift in Shift) then
  begin
    Self.SetSelectedFlags(False);
  end;

  { Go through "z" order and see if figure selected.  (Future:
    Use multiple WHILE loops for various types of objects.  For
    example, look for line objects before rectangular objects.) }
    
  i         := 0;
  LNotFound := True;

  while LNotFound and (i < Self.Count) do
  begin
    LRectFigure := TgmFigureObject(Self.Items[i]);

    { Don't check item if it's already selected.  This will allow
      selection of items on top of each other. }
    if (LRectFigure is TgmFigureObject) and (not LRectFigure.IsSelected) then
    begin
      if LRectFigure.ContainsPoint(LTestPoint) then
      begin
        Self.SetSelectedIndex(i);
        LNotFound := False;
      end;
    end;
    
    Inc(i)
  end;
end; 

function TgmFigureList.GetSelectedHandleAtPoint(
  const AX, AY: Integer): TgmDrawingHandle;
var
  LFigureObj: TgmFigureObject;
begin
  Result := dhNone;
  
  if (FIndexOfSelected <> FIGURE_NOT_SELECTED) and
     (SelectedFigureCount = 1) then
  begin
    LFigureObj := TgmFigureObject(Self.Items[FIndexOfSelected]);
    Result     := LFigureObj.GetHandleAtPoint(AX, AY, HANDLE_RADIUS);
  end;
end; 

// this routine written by Xiong Wei
procedure TgmFigureList.SelectRect(
  const IncludeMode: TgmFigureSelectIncludeMode;
  const RegionOrigin, RegionMovePt: TPoint);
var
  X1, X2, Y1, Y2, I, L, TX, TY          : Integer;
  FX1, FX2, FY1, FY2, FX3, FX4, FY3, FY4: Integer;
  LIfExit                               : Boolean;
  LFigureObj                            : TgmFigureObject;
begin
  if RegionOrigin.x < RegionMovePt.x then
  begin
    X1 := RegionOrigin.X;
    X2 := RegionMovePt.X;
  end
  else
  begin
    X1 := RegionMovePt.X;
    X2 := RegionOrigin.X;
  end;
  
  if RegionOrigin.y < RegionMovePt.y then
  begin
    Y1 := RegionOrigin.Y;
    Y2 := RegionMovePt.Y;
  end
  else
  begin
    Y1 := RegionMovePt.Y;
    Y2 := RegionOrigin.Y;
  end;

  L := Self.Count - 1;

  for I := 0 to L do
  begin
    LFigureObj := TgmFigureObject(Self.Items[I]);  // normally, FX1 and FY1 are for start-point, FX2 and FY2 are for end-point
    FX1        := LFigureObj.FStartPoint.X;        // if the figure is a curve, FX1 is x coordinate of the start-point
    FY1        := LFigureObj.FStartPoint.Y;        // if the figure is a curve, FY1 is y coordinate of the start-point
    FX2        := LFigureObj.FEndPoint.X;          // if the figure is a curve, FX2 is x coordinate of the end-point
    FY2        := LFigureObj.FEndPoint.Y;          // if the figure is a curve, FY2 is y coordinate of the end-point
    FX3        := LFigureObj.FCurvePoint1.X;       // if the figure is a curve, FX3 is x coordinate of the first control point
    FY3        := LFigureObj.FCurvePoint1.Y;       // if the figure is a curve, FY3 is y coordinate of the first control point
    FX4        := LFigureObj.FCurvePoint2.X;       // if the figure is a curve, FX4 is x coordinate of the second control point
    FY4        := LFigureObj.FCurvePoint2.Y;       // if the figure is a curve, FY4 is y coordinate of the second control point

    case IncludeMode of
      fsimTotallyInclude:
        begin
          if AllInRect(LFigureObj, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4, X1, Y1, X2, Y2) then
          begin
            LFigureObj.IsSelected := True;
          end;
        end;

      fsimPartiallyInclude:
        begin
          LIfExit := False;
          
          for TX := X1 to X2 do
          begin
            if InRect(LFigureObj, TX, Y1, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4) then
            begin
              LFigureObj.IsSelected := True;
              LIfExit               := True;
              Break;
            end
            else
            if InRect(LFigureObj, TX, Y2, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4) then
            begin
              LFigureObj.IsSelected := True;
              LIfExit               := True;
              Break;
            end;
          end;

          if LIfExit then
          begin
            Continue;
          end;

          for TY := Y1 to Y2 do
          begin
            if InRect(LFigureObj, X1, TY, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4) then
            begin
              LFigureObj.IsSelected := True;
              LIfExit               := True;
              Break;
            end
            else
            if InRect(LFigureObj, X2, TY, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4) then
            begin
              LFigureObj.IsSelected := True;
              LIfExit               := True;
              Break;
            end;
          end;

          if LIfExit then
          begin
            Continue;
          end;

          if AllInRect(LFigureObj, FX1, FY1, FX2, FY2, FX3, FY3, FX4, FY4, X1, Y1, X2, Y2) then
          begin
            LFigureObj.IsSelected := True;
          end;
        end;
    end;
  end;
end; 

// The main purpose of this method is to turn all flags off.
procedure TgmFigureList.SetSelectedFlags(const State: Boolean);
var
  i: Integer;
begin
  for i := 0 to (Self.Count - 1) do
  begin
    TgmFigureObject(Self.Items[i]).IsSelected := State;
  end;

  { If no figures are selected, or if they are all selected, then
    say there is no one figure selected. }
  FIndexOfSelected := FIGURE_NOT_SELECTED;
end;

procedure TgmFigureList.SetSelectedIndex(const Index: Integer);
begin
  if Self.Count > 0 then
  begin
    FIndexOfSelected := Index;

    // Index < 0 for FIGURE_NOT_SELECTED
    if Index >= 0 then
    begin
      TgmFigureObject(Self.Items[FIndexOfSelected]).IsSelected := True;
    end;
  end;
end; 

function TgmFigureList.GetTopLeftFromAllFigures: TPoint;
var
  i, j      : Integer;
  LFigureObj: TgmFigureObject;
  LTopLeft  : TPoint;
begin
  LTopLeft := Point(10000, 10000);

  if Self.Count > 0 then
  begin
    for i := 0 to Self.Count - 1 do
    begin
      LFigureObj := TgmFigureObject(Self.Items[i]);

      case LFigureObj.Flag of
        ffStraightLine,
        ffRectangle,
        ffSquare,
        ffRoundRectangle,
        ffRoundSquare,
        ffEllipse,
        ffCircle:
          begin
            LTopLeft.X := MinIntValue([LTopLeft.X, LFigureObj.FStartPoint.X, LFigureObj.FEndPoint.X]);
            LTopLeft.Y := MinIntValue([LTopLeft.Y, LFigureObj.FStartPoint.Y, LFigureObj.FEndPoint.Y]);
          end;

        ffCurve:
          begin
            LTopLeft.X := MinIntValue([LTopLeft.X, LFigureObj.FStartPoint.X, LFigureObj.FEndPoint.X,
                                       LFigureObj.FCurvePoint1.X, LFigureObj.FCurvePoint2.X]);

            LTopLeft.Y := MinIntValue([LTopLeft.Y, LFigureObj.FStartPoint.Y, LFigureObj.FEndPoint.Y,
                                       LFigureObj.FCurvePoint1.Y, LFigureObj.FCurvePoint2.Y]);
          end;

        ffPolygon, ffRegularPolygon:
          begin
            if High(LFigureObj.FPolygonPoints) > -1 then
            begin
              for j := Low(LFigureObj.FPolygonPoints) to High(LFigureObj.FPolygonPoints) do
              begin
                LTopLeft.X := MinIntValue([LTopLeft.X, LFigureObj.FPolygonPoints[j].X]);
                LTopLeft.Y := MinIntValue([LTopLeft.Y, LFigureObj.FPolygonPoints[j].Y]);
              end;
            end;
          end;
      end;
    end;
  end;
  
  Result := LTopLeft;
end; 

procedure TgmFigureList.AddStraightLineToList(
  const PenColor, BrushColor: TColor; const PenStyle: TPenStyle;
  const BrushStyle: TBrushStyle; const PenWidth: Integer;
  const StartPoint, EndPoint: TPoint);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmLineObject.Create(PenColor, BrushColor, PenStyle, BrushStyle,
                                     PenWidth, StartPoint, EndPoint);

  Inc(FLineNumber);
  
  LFigureObj.Name := 'Straight Line' + IntToStr(FLineNumber);
  
  Self.Add(LFigureObj);
end; 

procedure TgmFigureList.AddCurveToList(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const P1, P2, P3, P4: TPoint);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmCurveObject.Create(PenColor, BrushColor, PenStyle,
                                      BrushStyle, PenWidth, P1, P2, P3, P4);

  Inc(FCurveNumber);

  LFigureObj.Name := 'Curve' + IntToStr(FCurveNumber);
  
  Self.Add(LFigureObj);
end;

procedure TgmFigureList.AddPolygonToList(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const APolygon: array of TPoint);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmPolygonObject.Create(PenColor, BrushColor, PenStyle,
                                        BrushStyle, PenWidth, APolygon);

  Inc(FPolygonNumber);

  LFigureObj.Name := 'Polygon' + IntToStr(FPolygonNumber);
  
  Self.Add(LFigureObj);
end; 

procedure TgmFigureList.AddRegularPolygonToList(
  const PenColor, BrushColor: TColor; const PenStyle: TPenStyle;
  const BrushStyle: TBrushStyle; const PenWidth, Sides: Integer;
  const CenterPoint, CurrentPoint: TPoint);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmRegularPolygonObject.Create(PenColor, BrushColor, PenStyle,
                                               BrushStyle, PenWidth, Sides,
                                               CenterPoint, CurrentPoint);

  Inc(FRegularPolygonNumber);

  LFigureObj.Name := 'Regular Polygon' + IntToStr(FRegularPolygonNumber);

  Self.Add(LFigureObj);
end; 

procedure TgmFigureList.AddRectangleToList(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
  const Regular: Boolean);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmRectangleObject.Create(PenColor, BrushColor, PenStyle,
                                          BrushStyle, PenWidth, StartPoint,
                                          EndPoint, Regular);

  if Regular then
  begin
    Inc(FSquareNumber);
    LFigureObj.Name := 'Square' + IntToStr(FSquareNumber);
  end
  else
  begin
    Inc(FRectangleNumber);
    LFigureObj.Name := 'Rectangle' + IntToStr(FRectangleNumber);
  end;

  Self.Add(LFigureObj);
end; 

procedure TgmFigureList.AddRoundRectangleToList(
  const PenColor, BrushColor: TColor; const PenStyle: TPenStyle;
  const BrushStyle: TBrushStyle; const PenWidth: Integer;
  const StartPoint, EndPoint: TPoint; const RoundCornerRadius: Integer;
  const Regular: Boolean);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmRoundRectangleObject.Create(PenColor, BrushColor, PenStyle,
                                               BrushStyle, PenWidth, StartPoint,
                                               EndPoint, RoundCornerRadius,
                                               Regular);

  if Regular then
  begin
    Inc(FRoundSquareNumber);
    LFigureObj.Name := 'Rounded-Corner Square' + IntToStr(FRoundSquareNumber);
  end
  else
  begin
    Inc(FRoundRectangleNumber);
    LFigureObj.Name := 'Rounded-Corner Rectangle' + IntToStr(FRoundRectangleNumber);
  end;

  Self.Add(LFigureObj);
end; 

procedure TgmFigureList.AddEllipseToList(const PenColor, BrushColor: TColor;
  const PenStyle: TPenStyle; const BrushStyle: TBrushStyle;
  const PenWidth: Integer; const StartPoint, EndPoint: TPoint;
  const Regular: Boolean);
var
  LFigureObj: TgmFigureObject;
begin
  LFigureObj := TgmEllipseObject.Create(PenColor, BrushColor, PenStyle,
                                        BrushStyle, PenWidth, StartPoint,
                                        EndPoint, Regular);

  if Regular then
  begin
    Inc(FCircleNumber);
    LFigureObj.Name := 'Circle' + IntToStr(FCircleNumber);
  end
  else
  begin
    Inc(FEllipseNumber);
    LFigureObj.Name := 'Ellipse' + IntToStr(FEllipseNumber);
  end;

  Self.Add(LFigureObj);
end; 

function TgmFigureList.IfPointOnFigure(const AX, AY:Integer;
  ACoordConvertFunc: TgmPointCoordConvertFunc = nil): Boolean;
var
  i          : Integer;
  LFigureObj : TgmFigureObject;
begin
  Result := False;
  
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LFigureObj := TgmFigureObject(Self.Items[i]);
      
      if LFigureObj.ContainsPoint( Point(AX, AY), ACoordConvertFunc ) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TgmFigureList.SaveToStream(const AStream: TStream);
var
  LFigureListHeader: TFigureListHeaderVer1;
  i                : Integer;
  LFigureObj       : TgmFigureObject;
begin
  if Assigned(AStream) then
  begin
    if Self.Count > 0 then
    begin
      LFigureListHeader.FigureCount          := Self.Count;
      LFigureListHeader.LockedFigureCount    := Self.GetLockedFigureCount;
      LFigureListHeader.LineNumber           := FLineNumber;
      LFigureListHeader.CurveNumber          := FCurveNumber;
      LFigureListHeader.PolygonNumber        := FPolygonNumber;
      LFigureListHeader.RegularPolygonNumber := FRegularPolygonNumber;
      LFigureListHeader.RectangleNumber      := FRectangleNumber;
      LFigureListHeader.SquareNumber         := FSquareNumber;
      LFigureListHeader.RoundRectangleNumber := FRoundRectangleNumber;
      LFigureListHeader.RoundSquareNumber    := FRoundSquareNumber;
      LFigureListHeader.EllipseNumber        := FEllipseNumber;
      LFigureListHeader.CircleNumber         := FCircleNumber;

      AStream.Write(LFigureListHeader, SizeOf(TFigureListHeaderVer1));

      for i := 0 to (Self.Count - 1) do
      begin
        LFigureObj := TgmFigureObject(Self.Items[i]);
        LFigureObj.SaveToStream(AStream);
      end;
    end;
  end;
end;

function TgmFigureList.ValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Self.Count);
end;

//-- TgmFigureListReader -------------------------------------------------------

constructor TgmFigureListReader.Create(const AFigureList: TgmFigureList);
begin
  inherited Create;

  FFigureList := AFigureList;
end;

destructor TgmFigureListReader.Destroy;
begin
  FFigureList := nil;
  
  inherited Destroy;
end; 

//-- TgmFigureListReader1 ------------------------------------------------------

function TgmFigureListReader1.GetRectangle(const AStream: TStream;
  const IfRegular: Boolean): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue     : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmRectangleObject.Create(clBlack, clBlack, psSolid, bsSolid, 1,
                                        Point(0,0), Point(0, 0), IfRegular);

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LIntValue, 4);
    Result.OriginX := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.OriginY := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.Radius := LIntValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsRegular := LBooleanValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end; 

function TgmFigureListReader1.GetRoundedCornerRectangle(
  const AStream: TStream; const IfRegular: Boolean): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue     : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmRoundRectangleObject.Create(clBlack, clBlack, psSolid, bsSolid,
                                             1, Point(0,0), Point(0, 0), 0,
                                             IfRegular);

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.RoundCornerRadius := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LIntValue, 4);
    Result.OriginX := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.OriginY := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.Radius := LIntValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsRegular := LBooleanValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end; 

function TgmFigureListReader1.GetEllipse(const AStream: TStream;
  const IfRegular: Boolean): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue     : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmEllipseObject.Create(clBlack, clBlack, psSolid, bsSolid, 1,
                                      Point(0,0), Point(0, 0), IfRegular);

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LIntValue, 4);
    Result.OriginX := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.OriginY := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.Radius := LIntValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsRegular := LBooleanValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end;

function TgmFigureListReader1.GetCurve(const AStream: TStream): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue     : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmCurveObject.Create(clBlack, clBlack, psSolid, bsSolid, 1,
                                    Point(0,0), Point(0, 0), Point(0, 0),
                                    Point(0, 0));

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FCurvePoint1.X, 4);
    AStream.Read(Result.FCurvePoint1.Y, 4);
    AStream.Read(Result.FCurvePoint2.X, 4);
    AStream.Read(Result.FCurvePoint2.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LIntValue, 4);
    Result.CurveControl := TgmCurveControlPoint(LIntValue);

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end;

function TgmFigureListReader1.GetStraightLine(
  const AStream: TStream): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue     : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmLineObject.Create(clBlack, clBlack, psSolid, bsSolid, 1,
                                   Point(0,0), Point(0, 0));

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end; 

function TgmFigureListReader1.GetPolygon(
  const AStream: TStream): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue, i  : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
  LDummyArray   : array of TPoint;
  LPolygonObj   : TgmPolygonObject;
begin
  Result      := nil;
  LDummyArray := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmPolygonObject.Create(clBlack, clBlack, psSolid, bsSolid, 1,
                                      LDummyArray);

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    // read in the vertex count
    AStream.Read(LIntValue, 4);
    if LIntValue > 0 then
    begin
      SetLength(Result.FPolygonPoints, LIntValue);

      for i := Low(Result.FPolygonPoints) to High(Result.FPolygonPoints) do
      begin
        AStream.Read(Result.FPolygonPoints[i].X, 4);
        AStream.Read(Result.FPolygonPoints[i].Y, 4);
      end;
    end;

    // cast pointer
    LPolygonObj := TgmPolygonObject(Result);

    AStream.Read(LBooleanValue, 1);
    LPolygonObj.IsFillPolygon := LBooleanValue;

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end;

function TgmFigureListReader1.GetRegularPolygon(
  const AStream: TStream): TgmFigureObject;
var
  LStrValue     : ShortString;
  LIntValue, i  : Integer;
  LBooleanValue : Boolean;
  LColor        : TColor;
begin
  Result := nil;

  if Assigned(AStream) then
  begin
    { We pass arbitrary parameters to the creator, because we will load in
      the data from a stream. }
    Result := TgmRegularPolygonObject.Create(clBlack, clBlack, psSolid, bsSolid,
                                             1, 0, Point(0, 0), Point(0, 0));

    AStream.Read(LStrValue, SizeOf(LStrValue));
    Result.Name := LStrValue;

    AStream.Read(LColor, SizeOf(TColor));
    Result.PenColor := LColor;

    AStream.Read(LColor, SizeOf(TColor));
    Result.BrushColor := LColor;

    AStream.Read(LIntValue, 4);
    Result.PenStyle := TPenStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.BrushStyle := TBrushStyle(LIntValue);

    AStream.Read(LIntValue, 4);
    Result.PenWidth := LIntValue;

    AStream.Read(Result.FStartPoint.X, 4);
    AStream.Read(Result.FStartPoint.Y, 4);
    AStream.Read(Result.FEndPoint.X, 4);
    AStream.Read(Result.FEndPoint.Y, 4);

    AStream.Read(LIntValue, 4);
    Result.OriginX := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.OriginY := LIntValue;

    AStream.Read(LIntValue, 4);
    Result.Radius := LIntValue;

    // read in the side count
    AStream.Read(LIntValue, 4);
    Result.Sides := LIntValue;

    if Result.Sides > 2  then
    begin
      SetLength(Result.FPolygonPoints, Result.Sides + 1);

      for i := Low(Result.FPolygonPoints) to High(Result.FPolygonPoints) do
      begin
        AStream.Read(Result.FPolygonPoints[i].X, 4);
        AStream.Read(Result.FPolygonPoints[i].Y, 4);
      end;
    end;

    AStream.Read(LBooleanValue, 1);
    Result.IsLocked := LBooleanValue;
  end;
end; 

function TgmFigureListReader1.LoadFromStream(const AStream: TStream): Boolean;
var
  LFigureListHeader: TFigureListHeaderVer1;
  i, LIntValue     : Integer;
  LFigureFlag      : TgmFigureFlags;
  LFigureObj       : TgmFigureObject;
begin
  Result := False;

  if Assigned(AStream) then
  begin
    if Assigned(FFigureList) then
    begin
      if FFigureList.Count > 0 then
      begin
        FFigureList.DeleteAllFigures;
      end;

      AStream.Read(LFigureListHeader, SizeOf(TFigureListHeaderVer1));

      FFigureList.LineNumber           := LFigureListHeader.LineNumber;
      FFigureList.CurveNumber          := LFigureListHeader.CurveNumber;
      FFigureList.PolygonNumber        := LFigureListHeader.PolygonNumber;
      FFigureList.RegularPolygonNumber := LFigureListHeader.RegularPolygonNumber;
      FFigureList.RectangleNumber      := LFigureListHeader.RectangleNumber;
      FFigureList.SquareNumber         := LFigureListHeader.SquareNumber;
      FFigureList.RoundRectangleNumber := LFigureListHeader.RoundRectangleNumber;
      FFigureList.RoundSquareNumber    := LFigureListHeader.RoundSquareNumber;
      FFigureList.EllipseNumber        := LFigureListHeader.EllipseNumber;
      FFigureList.CircleNumber         := LFigureListHeader.CircleNumber;

      if LFigureListHeader.FigureCount > 0 then
      begin
        for i := 0 to (LFigureListHeader.FigureCount - 1) do
        begin
          AStream.Read(LIntValue, 4);
          
          LFigureFlag := TgmFigureFlags(LIntValue);
          LFigureObj  := nil;

          case LFigureFlag of
            ffStraightLine:
              begin
                LFigureObj := GetStraightLine(AStream);
              end;

            ffCurve:
              begin
                LFigureObj := GetCurve(AStream);
              end;

            ffPolygon:
              begin
                LFigureObj := GetPolygon(AStream);
              end;

            ffRegularPolygon:
              begin
                LFigureObj := GetRegularPolygon(AStream);
              end;

            ffRectangle:
              begin
                LFigureObj := GetRectangle(AStream, False);
              end;

            ffSquare:
              begin
                LFigureObj := GetRectangle(AStream, True);
              end;
              
            ffRoundRectangle:
              begin
                LFigureObj := GetRoundedCornerRectangle(AStream, False);
              end;

            ffRoundSquare:
              begin
                LFigureObj := GetRoundedCornerRectangle(AStream, True);
              end;
              
            ffEllipse:
              begin
                LFigureObj := GetEllipse(AStream, False);
              end;

            ffCircle:
              begin
                LFigureObj := GetEllipse(AStream, True);
              end;
          end;

          if Assigned(LFigureObj) then
          begin
            FFigureList.Add(LFigureObj);
          end;
        end;

        Result := True;
      end;
    end;
  end;
end; 

end.
