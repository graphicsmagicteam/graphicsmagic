unit gmSelection;

(* *******************************
 *
 * Selection routines for Graphics32
 * Version 1.1
 *
 * *******************************)

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

// Update Date: 2016/09/08


{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface

uses
{ Delphi }
  Windows, Graphics, Classes, ExtCtrls, Contnrs,
{ Graphics32 }
  GR32, GR32_Image, GR32_Transforms,
{ GraphicsMagicLib }
  gmTypes;

type
  TgmMarqueeTools = (mtNone,
                     mtMoveResize,
                     mtSingleRow,
                     mtSingleColumn,
                     mtRectangular,
                     mtRoundRectangular,
                     mtElliptical,
                     mtPolygonal,
                     mtRegularPolygon,
                     mtLasso,
                     mtMagicWand,
                     mtMagneticLasso);

  TgmMarqueeMode = (mmNew, mmAdd, mmSubtract, mmIntersect, mmExcludeOverlap);

  TgmTransformMode = (tmNone, tmDistort, tmRotate, tmScale, tmTranslate);

  // Indicating that move selection or move cut region.
  TgmTranslateTarget = (sttNone, sttSelection, sttCutRegion);

//-- TgmRectRegionNode ---------------------------------------------------------

  TgmRectRegionNode = class(TObject)
  private
    FStartPt, FEndPt : TPoint;
  public
    constructor Create;

    property StartPoint : TPoint read FStartPt write FStartPt;
    property EndPoint   : TPoint read FEndPt   write FEndPt;
  end;

//-- TgmSelection --------------------------------------------------------------

  TgmSelection = class(TThreadPersistent)
  private
    FImageControl         : TCustomImage32;
    FTimer                : TTimer;
    FSourceBitmap         : TBitmap32;    // original bitmap for restore the process of selection
    FOriginalMask         : TBitmap32;    // mask bitmap with the same size of original bitmap which for creating hRGN on
    FCutOriginal          : TBitmap32;    // original cutted region
    FCutMask              : TBitmap32;    // mask bitmap which was cutted from FOriginalMask
    FForeground           : TBitmap32;    // the foreground which copied from FCutOriginal, and could be scaled and paint to the dest
    FBackground           : TBitmap32;    // the background after creating the selection, used for clean up the screen
    FResizedMask          : TBitmap32;    // the resized FFeatherMask, it is the actual mask to be used
    FFeatherMask          : TBitmap32;    // Gaussian-blured FCutMask for getting the feathering mask effect
    FCompound             : TBitmap32;    // compound bitmap for temporarily used
    FMagicWandShadow      : TBitmap32;    // the mask of magic wand.

    FNewRGN               : hRGN;         // new region of the selection
    FCombinedRGN          : hRGN;         // compound region of selections
    FAccumRGN             : hRGN;         // accumulated region of the selection,
                                          // it is the actual region of the selection for
                                          // drawing the final mask

    FHorizRegionList      : TObjectList;  // used for holding the rectangular regions which horizontally searched from FResizeMask
    FVertRegionList       : TObjectList;  // used for holding the rectangular regions which vertically searched from FResizeMask
    FTopHorizLineList     : TObjectList;  // used for holding the top lines of vertically-searched regions
    FBottomHorizLineList  : TObjectList;  // used for holding the bottom lines of vertically-searched regions
    FLeftVertLineList     : TObjectList;  // used for holding the left lines of horizontally-searched regions
    FRightVertLineList    : TObjectList;  // used for holding the right lines of horizontally-searched regions
    FMarchingAntsLineList : TObjectList;  // used for holding the extended boundary lines of the selection,
                                          // for drawing the Marching Ants selection borders

    FFeatherRadius        : Integer;      // the feathering radius
    FFeathered            : Boolean;      // if selection is feathered
    FTranslated           : Boolean;      // if the selection is moved
    FCornerStretched      : Boolean;      // if the selection is resized
    FHasShadow            : Boolean;      // if the mask is existed
    FHorizFlipped         : Boolean;      // if the selection is flipped horizontally
    FVertFlipped          : Boolean;      // if the selection is flipped vertically
    FPrimitive            : Boolean;      // if the selection is an itself or a duplicated one
    FTransformed          : Boolean;      // if the selection is transformd (such as, be distorted, etc.)
    FTargetChanged        : Boolean;      // if the target of the selection is changed
    FIsForeAlphaChanged   : Boolean;      // if the alpha channel of selection foreground is changed

    FMaskBorderStart      : TPoint;       // The start point of the selection.
    FMaskBorderEnd        : TPoint;       // The end point of the selection.
    FMaskBorderWidth      : Integer;
    FMaskBorderHeight     : Integer;
    FMagicTolerance       : Double;

    FAnimateBias          : Integer;      // extracted from GR32_SeedFill.pas
    FAnimate              : Boolean;      // extracted from GR32_SeedFill.pas

    FOutputMsg            : string;

    // based on the code of GR32_SeedFill.pas written by Mattias Andersson
    procedure AnimateBorder(ASender: TObject);
    
    // based on the code of GR32_SeedFill.pas written by Mattias Andersson
    procedure SetAnimate(AValue: Boolean);

    // combine the current hRGN with the accumulated hRGN according to the marquee mode
    procedure CombineRGNToAccumRGN(const AMode: TgmMarqueeMode);

    // erase the pixels on FOriginalMask that out of the range of FAccumRGN
    procedure EraseOriginalMaskPixelsOutOfAccumRGN;
    
    // drawing the accumulated hRGN to FOriginalMask for the final mask
    procedure DrawAccumRGN;

    // get the rectangular regions from the mask by horizontally search
    procedure GetHorizontalRegions(AMaskBmp: TBitmap32);

    // get the rectangular regions from the mask by vertically search
    procedure GetVerticalRegions(AMaskBmp: TBitmap32);

    // get the horizontal lines of the regions that were vertically searched from mask
    procedure GetHorizontalLines;

    // get the verticall lines of the regions that were horizontally searched from mask
    procedure GetVerticalLines;

    // convert the MagicWand mask to path, and then convert the path to region
    procedure MagicWandPathToRegion;

    // convert the rectangles to path, and then convert the path to region
    procedure RectToPathToRegion;

    // rendering selection
    procedure RenderSelection(const ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet);

    procedure ScaleMarchingAntsLine(ASourceLine: TgmRectRegionNode;
      var AScaledStartPoint, AScaledEndPoint: TPoint);

    procedure SetImageControl(AImageControl: TCustomImage32);

    // check for whether the selection has been processed
    function SelectionHasProcessed: Boolean;
  public
    constructor Create; reintroduce; overload;
    
    constructor Create(const AControl: TCustomImage32); reintroduce; overload;

    constructor Create(const AControl: TCustomImage32;
      const ASourceBitmap: TBitmap32); reintroduce; overload;

    destructor Destroy; override;

    // based on the code of GR32_SeedFill.pas written by Mattias Andersson
    procedure DrawMarchingAnts; overload;
    procedure DrawMarchingAnts(ABuffer: TBitmap32); overload;

    // duplicate the information about the selecton
    procedure AssignSelectionData(const ASourceSelection: TgmSelection);
    procedure AssignAllSelectionData(const ASourceSelection: TgmSelection);
    
    // make the FMaskBorderStart is always at the top left
    procedure StandardizeOrder;

    // determine whether the mouse is over the selection border
    function ContainsPoint(const ATestPoint: TPoint):  Boolean;
    
    // determine whether the mouse is over the handles of the selection
    function GetHandleAtPoint(const AX, AY, ARadius: Integer): TgmDrawingHandle;
    
    // setting the original dimension of the mask
    procedure SetOriginalMask(const AWidth, AHeight: Integer);

    // creating single row selection
    procedure CreateSingleRowRGN(const AStartX, Y, AEndX: Integer;
      const AMode: TgmMarqueeMode);

    // creating single column selection
    procedure CreateSingleColumnRGN(const X, AStartY, AEndY: Integer;
      const AMode: TgmMarqueeMode);

    // creating rectangular marquee selection
    procedure CreateRectMarqueeRGN(const AStartPoint, AEndPoint: TPoint;
      const AMode: TgmMarqueeMode);

    // creating rounded rectangular marquee selection
    procedure CreateRoundRectMarqueeRGN(const AStartPoint, AEndPoint: TPoint;
      const AAngle1, AAngle2: Integer; const AMode: TgmMarqueeMode);

    // creating elliptical marquee selection
    procedure CreateEllipseMarqueeRGN(const AStartPoint, AEndPoint: TPoint;
      const AMode: TgmMarqueeMode);

    // creating polygonal and lass marquee selection.
    procedure CreatePolygonMarqueeRGN(const APolygon: array of TPoint;
      const AMode: TgmMarqueeMode);

    // creating regular polygonal marquee selection
    procedure CreateRegularPolygonRGN(const ACenterPoint, ACurrentPoint: TPoint;
      const ASides: Integer; const AMode: TgmMarqueeMode);
      
    // creating MagicWand marquee selection
    procedure CreateMagicWandMarqueeRGN(ASourceBmp: TBitmap32;
      const X, Y: Integer; const AOldColor: TColor32; const AMode: TgmMarqueeMode);
      
    // creating the Color Range marquee selection
    procedure CreateColorRangeRGN(ASourceBmp: TBitmap32;
      const ASampledColor: TColor32; const AFuzziness: Integer);

    procedure CreateCustomRGN(const ARgn: hRGN; const AMode: TgmMarqueeMode);

    // convert channel bitmap to selection
    function LoadChannelAsSelection(const AChannelBitmap: TBitmap32;
      const AMode: TgmMarqueeMode): Boolean;

    // cut regions from the original bitmap and mask
    procedure CutRegionFromOriginal;

    // get the border of the cutted area
    procedure GetActualMaskBorder;

    // get the Marching Ants lines
    procedure GetMarchingAntsLines;
    
    // get featherd mask
    procedure GetFeatherMask;
    
    // get the foreground of the selection
    procedure GetForeground;
    
    // get the background for updating the screen
    procedure GetBackgroundWithFilledColor(const AFillColor: TColor32;
      const AChannelSet: TgmChannelSet);

    procedure GetBackgroundWithTransparent;

    // resize the selection
    procedure ResizeSelection;
    
    // draw the border of the selection
    procedure DrawMarchingAntsBorder(ACanvas: TCanvas;
      const AXOffset, AYOffset: Integer; const ADrawHandles: Boolean); overload; 

    procedure DrawMarchingAntsBorder(ACanvas: TCanvas;    
      const ADrawHandles: Boolean); overload;

    procedure TranslateSelection(const ATranslateVector: TPoint);
    
    // flip the selection
    procedure FlipSelection(const AFlipMode: TgmFlipMode);
    
    // invert the selection
    procedure InvertSelection;

    // pick up all the area into the selection
    procedure SelectAll;
    
    // center align the selection according to the destination bitmap
    procedure CenterAlignSelection;
    
    // show the selection
    procedure ShowSelection(const ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet); overload;

    procedure ShowSelection(const ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet; const ARect: TRect); overload;

    // changing the alpha channel of the Foreground with the mask for getting
    // the feathering effect
    procedure ChangeForegroundAlphaChannel;

    procedure MakeRegionWithMask(AMaskBmp: TBitmap32);
    procedure TranslateCutRegion(const ATranslateVector: TPoint);
    procedure GetAccumRGN(const AMaskBmp: TBitmap32);

    // make FCutOriginal same as FForeground
    procedure ConfirmForeground;

    // clear the FOriginalMask bitmap and draw the FResizedMask bitmap on it
    procedure UpdateOriginalMaskWithResizedMask;

    // restore local background
    procedure RestoreBackground(ADestBmp: TBitmap32; const ARect: TRect);

    // determine whether the mouse is pointing on the selection
    function IfPointsOnSelection(const X, Y: Integer): Boolean;

    // convert APoint from selection coordinate space to bitmap coordinate space
    function SelectionPointToBitmapPoint(const APoint: TPoint): TPoint;

    property Background           : TBitmap32      read FBackground         write FBackground;
    property CutMask              : TBitmap32      read FCutMask            write FCutMask;
    property CutOriginal          : TBitmap32      read FCutOriginal        write FCutOriginal;
    property FeatherMask          : TBitmap32      read FFeatherMask        write FFeatherMask;
    property FeatherRadius        : Integer        read FFeatherRadius      write FFeatherRadius;
    property Foreground           : TBitmap32      read FForeground         write FForeground;
    property HasShadow            : Boolean        read FHasShadow;
    property ImageControl         : TCustomImage32 read FImageControl       write SetImageControl;
    property IsAnimated           : Boolean        read FAnimate            write SetAnimate;
    property IsCornerStretched    : Boolean        read FCornerStretched    write FCornerStretched;
    property IsFeathered          : Boolean        read FFeathered          write FFeathered;
    property IsForeAlphaChanged   : Boolean        read FIsForeAlphaChanged write FIsForeAlphaChanged;
    property IsHorizFlipped       : Boolean        read FHorizFlipped       write FHorizFlipped;
    property IsPrimitive          : Boolean        read FPrimitive;
    property IsProcessed          : Boolean        read SelectionHasProcessed;
    property IsTargetChanged      : Boolean        read FTargetChanged      write FTargetChanged;
    property IsTransformed        : Boolean        read FTransformed        write FTransformed;
    property IsTranslated         : Boolean        read FTranslated         write FTranslated;
    property IsVertFlipped        : Boolean        read FVertFlipped        write FVertFlipped;
    property MagicTolerance       : Double         read FMagicTolerance     write FMagicTolerance;
    property MagicWandShadow      : TBitmap32      read FMagicWandShadow;
    property MarchingAntsLineList : TObjectList    read FMarchingAntsLineList;
    property MaskBorderEnd        : TPoint         read FMaskBorderEnd      write FMaskBorderEnd;
    property MaskBorderHeight     : Integer        read FMaskBorderHeight   write FMaskBorderHeight;
    property MaskBorderStart      : TPoint         read FMaskBorderStart    write FMaskBorderStart;
    property MaskBorderWidth      : Integer        read FMaskBorderWidth    write FMaskBorderWidth;
    property OriginalMask         : TBitmap32      read FOriginalMask       write FOriginalMask;
    property OutputMsg            : string         read FOutputMsg;
    property ResizedMask          : TBitmap32      read FResizedMask        write FResizedMask;
    property SourceBitmap         : TBitmap32      read FSourceBitmap       write FSourceBitmap;
  end;

//-- TgmSelectionTransformation ------------------------------------------------

  TgmSelectionTransformation = class(TObject)
  protected
    FTransformMode        : TgmTransformMode;

    FSelection            : TgmSelection;  // pointer to passed selection object
    FMaskBorderStartCopy  : TPoint;        // a copy of the start point of the selection's border
    FMaskBorderEndCopy    : TPoint;        // a copy of the end point of the selection's border
    FForegroundCopy       : TBitmap32;     // a copy of the foreground of the selection
    FResizeMaskCopy       : TBitmap32;     // a copy of the resized mask of the selection

    FRectTopLeft          : TPoint;
    FRectBottomRight      : TPoint;
    FRectReady            : Boolean;
    FTransforming         : Boolean;          // if executing tranformation now

    FTT                   : TTransformation;
    FPT                   : TProjectiveTransformation;
    FVertices             : array [0 .. 3] of TPoint;    // apical coordinates of the transformed selection
    FExtraHandles         : array [0 .. 3] of TPoint;    // the coordinates of additional handles of the
                                                         // transformed selection (Order: Left, Top, Right, Bottom)

    FVerticesLength       : array [0 .. 3] of Extended;  // the distance of the vertices to the center point
    FVerticesRadians      : array [0 .. 3] of Extended;
    FSelectionCenterCoord : TFloatPoint;

    procedure InitVertices;
    procedure GenTransform;
    procedure DoTransform;      // transform the selection

    function GetVertexByIndex(AIndex: Byte): TPoint;
    function GetExtraHandleByIndex(AIndex: Byte): TPoint;
    function GetVertexLengthByIndex(AIndex: Byte): Extended;
    function GetVertexRadiansByIndex(AIndex: Byte): Extended;
  public
    constructor Create(ASelection: TgmSelection); virtual;
    destructor Destroy; override;

    procedure ExecuteTransform;
    procedure AssignTransformData(const ATransformation: TgmSelectionTransformation);

    procedure DrawOutline(ACanvas: TCanvas; const AOffsetVector: TPoint;
      const APenMode: TPenMode); overload;

    procedure DrawOutline(ACanvas: TCanvas; const APenMode: TPenMode); overload;  

    // determine whether the mouse is over the handles of the selection
    function GetHandleAtPoint(const AX, AY, ARadius: Integer): TgmDrawingHandle;
    function PointOnSelectionBody(const AX, AY: Integer): Boolean;

    // get all the vertices of the additional handles
    procedure GetExtraHandles;

    // change the coordinates of the vertices
    procedure ChangeVertices(const APointArray: array of TPoint);

    procedure ChangeVertices_Distort(const AOffsetVector: TPoint;
      const ADrawingHandle: TgmDrawingHandle);
      
    procedure ChangeVertices_Rotate(const ARadiansIncrement: Extended);

    procedure ChangeVertices_Scale(const AOffsetVector: TPoint;
      const ADrawingHandle: TgmDrawingHandle);
      
    procedure TranslateVertices(const AOffsetVector: TPoint);

    // accept the transformed selection
    procedure AcceptTransform;
    
    // cancel the transform manipulation on the selection
    procedure CancelTransform;

    procedure ShowTransformedSelection(const ADestBmp: TBitmap32;
      const AChannelSet: TgmChannelSet);

    procedure ConnectSelection(ASelection: TgmSelection);

    property TransformMode                : TgmTransformMode read FTransformMode write FTransformMode;
    property SelectionCenterCoord         : TFloatPoint      read FSelectionCenterCoord;
    property Vertices[index: Byte]        : TPoint           read GetVertexByIndex;
    property ExtraHandles[index: Byte]    : TPoint           read GetExtraHandleByIndex;
    property MaskBorderStartCopy          : TPoint           read FMaskBorderStartCopy;
    property MaskBorderEndCopy            : TPoint           read FMaskBorderEndCopy;
    property RectTopLeft                  : TPoint           read FRectTopLeft;
    property RectBottomRight              : TPoint           read FRectBottomRight;
    property IsRectReady                  : Boolean          read FRectReady;
    property IsTransforming               : Boolean          read FTransforming write FTransforming;
    property ForegroundCopy               : TBitmap32        read FForegroundCopy;
    property ResizeMaskCopy               : TBitmap32        read FResizeMaskCopy;
    property VerticesLength[index: Byte]  : Extended         read GetVertexLengthByIndex;
    property VerticesRadians[index: Byte] : Extended         read GetVertexRadiansByIndex;
  end;

//-- TgmSelectionDistort -------------------------------------------------------

  TgmSelectionDistort = class(TgmSelectionTransformation)
  public
    constructor Create(ASelection: TgmSelection); override;
  end;

//-- TgmSelectionRotate --------------------------------------------------------

  TgmSelectionRotate = class(TgmSelectionTransformation)
  private
    { Before rotating the selection, calculating the radians of the vertices
      to the center point on the selection. }
    procedure CalcVerticesRadians;

    { Before rotating the selection, calculating the distance of the vertices
      to the center point on the selection. }
    procedure CalcVerticesLength;
  public
    constructor Create(ASelection: TgmSelection); override;

    procedure UpdateRotateState;  // Update the state of rotation.
  end;

//-- TgmSelectionScale ---------------------------------------------------------

  TgmSelectionScale = class(TgmSelectionTransformation)
  public
    constructor Create(ASelection: TgmSelection); override;
  end;

  function GetMarqueeModeString(const AMode: TgmMarqueeMode): string;

const
  SELECTION_HANDLE_RADIUS: Integer = 3;  // The radius of the handle.

implementation

uses
{ Delphi }
  SysUtils,
  Math,                   // MaxIntValue(), MinIntValue()...
{ Graphics32 }
  GR32_LowLevel,
{ externals }
  LineLibrary,
  GR32_Add_BlendModes,
  GR32_SeedFill,
{ GraphicsMagicLib }
  gmGaussianBlur,        // GBlur32()
  gmMath,
  gmImageProcessFuncs,
  gmAlphaFuncs,          // functions for alpha channel processing
  gmCommonFuncs;

{ Custom Procedures and Functions }

// drawing the handle at the given point
procedure DrawSelectionHandle(ACanvas: TCanvas; const APoint: TPoint;
  const ARadius: Integer);
begin
  with ACanvas do
  begin
    Pen.Width   := 1;
    Pen.Style   := psSolid;
    Pen.Color   := clBlack;
    Pen.Mode    := pmNotXor;
    Brush.Style := bsClear;
    
    Rectangle(APoint.X - ARadius, APoint.Y - ARadius,
              APoint.X + ARadius, APoint.Y + ARadius);

    Pen.Mode := pmCopy;
  end;
end;

function GetMarqueeModeString(const AMode: TgmMarqueeMode): string;
var
  s : string;
begin
  case AMode of
    mmNew:
      begin
        s := 'New';
      end;

    mmAdd:
      begin
        s := 'Add';
      end;
      
    mmSubtract:
      begin
        s := 'Subtract';
      end;

    mmIntersect:
      begin
        s := 'Intersect';
      end;
      
    mmExcludeOverlap:
      begin
        s := 'Exclude Overlap';
      end;
  end;
  
  Result := s;
end;

//-- TgmRectRegionNode ---------------------------------------------------------

constructor TgmRectRegionNode.Create;
begin
  inherited Create;

  FStartPt := Point(0, 0);
  FEndPt   := Point(0, 0);
end;

{ TgmSelection }

// Don't use this to instantiate a selection object in normal use.
// This constructor could be used to create a selection and copy data
// from another selection object.
constructor TgmSelection.Create;
begin
  inherited Create;

  FSourceBitmap                := TBitmap32.Create;
  FSourceBitmap.DrawMode       := dmCustom;
  FSourceBitmap.OnPixelCombine := BlendMode.NormalBlend;
  FOriginalMask                := TBitmap32.Create;
  FForeground                  := TBitmap32.Create;
  FForeground.DrawMode         := dmBlend;
  FBackground                  := TBitmap32.Create;
  FBackground.DrawMode         := dmBlend;
  FCutMask                     := TBitmap32.Create;
  FFeatherMask                 := TBitmap32.Create;
  FResizedMask                 := TBitmap32.Create;
  FCompound                    := TBitmap32.Create;
  FCompound.DrawMode           := dmBlend;
  FCutOriginal                 := TBitmap32.Create;
  FCutOriginal.DrawMode        := dmBlend;
  FMagicWandShadow             := TBitmap32.Create;

  FHorizRegionList      := TObjectList.Create;
  FVertRegionList       := TObjectList.Create;
  FTopHorizLineList     := TObjectList.Create;
  FBottomHorizLineList  := TObjectList.Create;
  FLeftVertLineList     := TObjectList.Create;
  FRightVertLineList    := TObjectList.Create;
  FMarchingAntsLineList := TObjectList.Create;

  FFeatherRadius      := 0;
  FFeathered          := False;
  FTranslated         := False;
  FCornerStretched    := False;
  FHasShadow          := False;
  FPrimitive          := True;  // indicate that if the selection is whether primitive or duplicated one
  FHorizFlipped       := False;
  FVertFlipped        := False;
  FTransformed        := False;
  FTargetChanged      := False;
  FIsForeAlphaChanged := False;
  FMaskBorderStart    := Point(0, 0);
  FMaskBorderEnd      := Point(0, 0);
  FMaskBorderWidth    := 0;
  FMaskBorderHeight   := 0;
  FMagicTolerance     := 0.0;

  FOutputMsg := '';
end;

// this version of create() is for associate an image control
// with the selection
constructor TgmSelection.Create(const AControl: TCustomImage32);
begin
  if not Assigned(AControl) then
  begin
    raise Exception.Create('TgmSelection.Create -- Parameter AControl is nil.');
  end;

  Self.Create;

  FImageControl := AControl;

  // Marching Ants
  FTimer          := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.Enabled  := False;
  FTimer.OnTimer  := AnimateBorder;
  FAnimateBias    := 8; 
end;

// This version of create() is for associate an image control
// with the selection, and initialize the source bitmap,
// background bitmap and original mask of the selection with
// the passed source bitmap.
constructor TgmSelection.Create(const AControl: TCustomImage32;
  const ASourceBitmap: TBitmap32);
begin
  if not Assigned(AControl) then
  begin
    raise Exception.Create('TgmSelection.Create -- Parameter AControl is nil.');
  end;

  if not Assigned(ASourceBitmap) then
  begin
    raise Exception.Create('TgmSelection.Create -- Parameter ASourceBitmap is nil.');
  end;

  if (ASourceBitmap.Width <= 0) or (ASourceBitmap.Height <= 0) then
  begin
    raise Exception.Create('TgmSelection.Create -- The width/height of parameter ASourceBitmap is zero.');
  end;

  Self.Create;

  FImageControl := AControl;

  FSourceBitmap.Assign(ASourceBitmap);
  FBackground.Assign(ASourceBitmap);
  FOriginalMask.SetSizeFrom(ASourceBitmap);

  // Marching Ants
  FTimer          := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.Enabled  := False;
  FTimer.OnTimer  := AnimateBorder;
  FAnimateBias    := 8;                      
end;

destructor TgmSelection.Destroy;
begin
  FSourceBitmap.Free;
  FOriginalMask.Free;
  FForeground.Free;
  FBackground.Free;
  FCutMask.Free;
  FFeatherMask.Free;
  FResizedMask.Free;

  FCompound.Free;
  FCutOriginal.Free;
  FMagicWandShadow.Free;

  FHorizRegionList.Clear;
  FHorizRegionList.Free;

  FVertRegionList.Clear;
  FVertRegionList.Free;

  FTopHorizLineList.Clear;
  FTopHorizLineList.Free;

  FBottomHorizLineList.Clear;
  FBottomHorizLineList.Free;

  FLeftVertLineList.Clear;
  FLeftVertLineList.Free;

  FRightVertLineList.Clear;
  FRightVertLineList.Free;

  FMarchingAntsLineList.Clear;
  FMarchingAntsLineList.Free;

  DeleteObject(FNewRGN);
  DeleteObject(FCombinedRGN);
  DeleteObject(FAccumRGN);

  FTimer.Free;
  FImageControl := nil;          

  inherited Destroy;
end;

// based on the code of GR32_SeedFill.pas written by Mattias Andersson
procedure TgmSelection.AnimateBorder(ASender: TObject);
begin
  Dec(FAnimateBias);

  if FAnimateBias <= 0 then
  begin
    FAnimateBias := 8;
  end;

  DrawMarchingAnts;
end;

procedure TgmSelection.AssignAllSelectionData(
  const ASourceSelection: TgmSelection);
var
  i               : Integer;
  LRectRegionNode : TgmRectRegionNode;
  LNewNode        : TgmRectRegionNode;
begin
  FSourceBitmap.Assign(ASourceSelection.SourceBitmap);
  FOriginalMask.Assign(ASourceSelection.OriginalMask);
  FCutOriginal.Assign(ASourceSelection.CutOriginal);
  FCutMask.Assign(ASourceSelection.CutMask);
  FForeground.Assign(ASourceSelection.Foreground);
  FBackground.Assign(ASourceSelection.Background);
  FResizedMask.Assign(ASourceSelection.ResizedMask);
  FFeatherMask.Assign(ASourceSelection.FeatherMask);

  FHasShadow          := ASourceSelection.HasShadow;
  FPrimitive          := ASourceSelection.IsPrimitive;
  FFeatherRadius      := ASourceSelection.FeatherRadius;
  FFeathered          := ASourceSelection.IsFeathered;
  FTranslated         := ASourceSelection.IsTranslated;
  FCornerStretched    := ASourceSelection.IsCornerStretched;
  FHorizFlipped       := ASourceSelection.IsHorizFlipped;
  FVertFlipped        := ASourceSelection.IsVertFlipped;
  FTransformed        := ASourceSelection.IsTransformed;
  FTargetChanged      := ASourceSelection.IsTargetChanged;
  FIsForeAlphaChanged := ASourceSelection.IsForeAlphaChanged;

  FMaskBorderWidth  := ASourceSelection.MaskBorderWidth;
  FMaskBorderHeight := ASourceSelection.MaskBorderHeight;
  FMaskBorderStart  := ASourceSelection.FMaskBorderStart;
  FMaskBorderEnd    := ASourceSelection.FMaskBorderEnd;

  FMarchingAntsLineList.Clear;

  if ASourceSelection.FMarchingAntsLineList.Count > 0 then
  begin
    for i := 0 to (ASourceSelection.FMarchingAntsLineList.Count - 1) do
    begin
      LRectRegionNode     := TgmRectRegionNode(ASourceSelection.FMarchingAntsLineList.Items[i]);
      LNewNode            := TgmRectRegionNode.Create;
      LNewNode.StartPoint := LRectRegionNode.StartPoint;
      LNewNode.EndPoint   := LRectRegionNode.EndPoint;

      FMarchingAntsLineList.Add(LNewNode);
    end;
  end;
end;

// duplicate the infomation of the selection.
procedure TgmSelection.AssignSelectionData(
  const ASourceSelection: TgmSelection);
var
  i               : Integer;
  LRectRegionNode : TgmRectRegionNode;
  LNewNode        : TgmRectRegionNode;
begin
  FOriginalMask.Assign(ASourceSelection.OriginalMask);
  FCutOriginal.Assign(ASourceSelection.CutOriginal);
  FForeground.Assign(ASourceSelection.Foreground);
  FFeatherMask.Assign(ASourceSelection.FeatherMask);
  FResizedMask.Assign(ASourceSelection.ResizedMask);

  FHasShadow        := ASourceSelection.HasShadow;
  FPrimitive        := False;
  FMaskBorderWidth  := ASourceSelection.MaskBorderWidth;
  FMaskBorderHeight := ASourceSelection.MaskBorderHeight;

  FMarchingAntsLineList.Clear;

  if ASourceSelection.FMarchingAntsLineList.Count > 0 then
  begin
    for i := 0 to (ASourceSelection.FMarchingAntsLineList.Count - 1) do
    begin
      LRectRegionNode     := TgmRectRegionNode(ASourceSelection.FMarchingAntsLineList.Items[i]);
      LNewNode            := TgmRectRegionNode.Create;
      LNewNode.StartPoint := LRectRegionNode.StartPoint;
      LNewNode.EndPoint   := LRectRegionNode.EndPoint;

      FMarchingAntsLineList.Add(LNewNode);
    end;
  end;
end;

// to show the selection at center of the destination bitmap
procedure TgmSelection.CenterAlignSelection;
var
  LMaskBorderW : Integer;
  LMaskBorderH : Integer;
  LHandleSize  : Integer;
begin
  LHandleSize  := SELECTION_HANDLE_RADIUS * 2 + 1;
  LMaskBorderW := FMaskBorderWidth  + LHandleSize;
  LMaskBorderH := FMaskBorderHeight + LHandleSize;

  if (LMaskBorderW < FBackground.Width) and
     (LMaskBorderH < FBackground.Height) then
  begin
    FMaskBorderStart.X := FBackground.Width  div 2 - LMaskBorderW div 2 + SELECTION_HANDLE_RADIUS;
    FMaskBorderStart.Y := FBackground.Height div 2 - LMaskBorderH div 2 + SELECTION_HANDLE_RADIUS;
  end
  else
  begin
    FMaskBorderStart := Point(0, 0);
  end;

  FMaskBorderEnd.X := FMaskBorderStart.X + FMaskBorderWidth;
  FMaskBorderEnd.Y := FMaskBorderStart.Y + FMaskBorderHeight;
end;

// Modify the alpha value of each pixel on the FForeground with the mask,
// to get feathered foreground. 
procedure TgmSelection.ChangeForegroundAlphaChannel;
var
  i, j                : Integer;
  fa, fr, fg, fb      : Byte;
  mr, mg, mb, LWeight : Byte;
  LForeRow, LMaskRow  : PColor32Array;
begin
{$RANGECHECKS OFF}

  for j := 0 to (FForeground.Height - 1) do
  begin
    LForeRow := FForeground.ScanLine[j];
    LMaskRow := FResizedMask.ScanLine[j];

    for i := 0 to (FForeground.Width - 1) do
    begin
      fa := LForeRow[i] shr 24 and $FF;
      fr := LForeRow[i] shr 16 and $FF;
      fg := LForeRow[i] shr  8 and $FF;
      fb := LForeRow[i]        and $FF;

      mr := LMaskRow[i] shr 16 and $FF;
      mg := LMaskRow[i] shr  8 and $FF;
      mb := LMaskRow[i]        and $FF;
      
      LWeight := (mr + mg + mb) div 3;
      fa      := Clamp( fa - (255 - LWeight), 0, 255 );

      LForeRow[i] := (fa shl 24) or (fr shl 16) or (fg shl 8) or fb;
    end;
  end;

{$RANGECHECKS ON}
end;

// combine the new hRGN with accumulated hRGN
procedure TgmSelection.CombineRGNToAccumRGN(const AMode: TgmMarqueeMode);
begin
  case AMode of
    mmNew:
      begin
        FCombinedRGN := FNewRGN;
      end;

    mmAdd:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_OR);
      end;

    mmSubtract:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_DIFF);
      end;

    mmIntersect:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_AND);
      end;

    mmExcludeOverlap:
      begin
        FCombinedRGN := FNewRGN;
        CombineRGN(FCombinedRGN, FAccumRGN, FNewRGN, RGN_XOR);
      end;
  end;
  
  FAccumRGN := FCombinedRGN;
end;

// make FCutOriginal same as FForeground
procedure TgmSelection.ConfirmForeground;
begin
  SmoothResize32(FCutOriginal, FForeground.Width, FForeground.Height);
end;

// determine if the TestPoint is within the selection border
function TgmSelection.ContainsPoint(const ATestPoint: TPoint): Boolean;
var
  LStartPt, LEndPt : TPoint;
begin
  Result := False;

  if FHasShadow = False then
  begin
    Exit;
  end;

  LStartPt.X := FMaskBorderStart.X - 1 - SELECTION_HANDLE_RADIUS;
  LStartPt.Y := FMaskBorderStart.Y - 1 - SELECTION_HANDLE_RADIUS;
  LEndPt.X   := FMaskBorderEnd.X   + 1 + SELECTION_HANDLE_RADIUS;
  LEndPt.Y   := FMaskBorderEnd.Y   + 1 + SELECTION_HANDLE_RADIUS;

  Result := Windows.PtInRect( Rect(LStartPt.X, LStartPt.Y, LEndPt.X, LEndPt.Y),
                              ATestPoint );
end;

// create Color Range Selection
procedure TgmSelection.CreateColorRangeRGN(ASourceBmp: TBitmap32;
  const ASampledColor: TColor32; const AFuzziness: Integer);
begin
  // store the mask of Color Range Selection with FMagicWandShadow bitmap
  FMagicWandShadow.Width  := ASourceBmp.Width;
  FMagicWandShadow.Height := ASourceBmp.Height;
  
  MakeColorRangeShadow32(ASourceBmp, FMagicWandShadow, ASampledColor, AFuzziness);

  FOriginalMask.Clear(clBlack32);
  FOriginalMask.Draw(FMaskBorderStart.X, FMaskBorderStart.Y, FMagicWandShadow);

  // get rectangles which composed the Region
  GetHorizontalRegions(FOriginalMask);

  { Convert the rectangles to path and convert the path to region.
    Store the region with FNewRGN. }
  RectToPathToRegion;

  FAccumRGN := FNewRGN;
end;

procedure TgmSelection.CreateCustomRGN(const ARgn: hRGN;
  const AMode: TgmMarqueeMode);
begin
  // create a "empty" region
  FNewRGN := CreateRectRgn(0, 0, 0, 0);
  
  // copy the passed Region
  CombineRgn(FNewRGN, ARgn, 0, RGN_COPY);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);

  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// create elliptical selection
procedure TgmSelection.CreateEllipseMarqueeRGN(
  const AStartPoint, AEndPoint: TPoint; const AMode: TgmMarqueeMode);
begin
  FOriginalMask.Canvas.Brush.Color := clWhite;
  FNewRGN := CreateEllipticRGN(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);
  
  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// create MagicWand Selection
procedure TgmSelection.CreateMagicWandMarqueeRGN(ASourceBmp: TBitmap32;
  const X, Y: Integer; const AOldColor: TColor32; const AMode: TgmMarqueeMode);
var
  i, j         : Integer;
  LPtr         : PByte;
  LBit1, LBit2 : PColor32;
  LValue       : Byte;
  LFiller      : TSeedFill;
begin
  LFiller := TSeedFill.Create(ASourceBmp);
  try
    LFiller.Min := Trunc(FMagicTolerance * 255);
    LFiller.Max := LFiller.Min;

    LFiller.SetFillPoint(X, Y);
    LFiller.Update;

    FMagicWandShadow.SetSize(ASourceBmp.Width, ASourceBmp.Height);

    LBit1 := @FMagicWandShadow.Bits[0];
    LBit2 := @FOriginalMask.Bits[0];

    for j := 0 to (FMagicWandShadow.Height - 1) do
    begin
      for i := 0 to (FMagicWandShadow.Width - 1) do
      begin
        LPtr   := LFiller.ToleranceMaskPtr[i, j];
        LValue := LPtr^;
        LBit1^ := $FF000000 or (LValue shl 16) or (LValue shl 8) or LValue;

        if LValue > 0 then
        begin
          LBit2^ := $FFFFFFFF;
        end;

        Inc(LBit1);
        Inc(LBit2);
      end;
    end;

    // search rectangles from the FMagicWandShadow horizontally
    GetHorizontalRegions(FMagicWandShadow);

    { Convert the rectangles to path and convert the path to region,
      store the region in FNewRGB. }
    MagicWandPathToRegion;

    // combine the new region with the accumulated region
    CombineRGNToAccumRGN(AMode);

    if AMode <> mmAdd then
    begin
      EraseOriginalMaskPixelsOutOfAccumRGN;
    end;
  finally
    LFiller.Free;
  end;
end;

// create polygonal and lasso selection
procedure TgmSelection.CreatePolygonMarqueeRGN(
  const APolygon: array of TPoint; const AMode: TgmMarqueeMode);
var
  i              : Integer;
  LStaticPolygon : array [0 .. 100000] of TPoint;
begin
  if High(APolygon) > 0 then
  begin
    // Convert the passed dynamic array to static array.
    for i := Low(APolygon) to High(APolygon) do
    begin
      LStaticPolygon[i] := APolygon[i];
    end;

    FOriginalMask.Canvas.Brush.Color := clWhite;
    FNewRGN := CreatePolygonRGN(LStaticPolygon, High(APolygon), ALTERNATE);

    FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
    CombineRGNToAccumRGN(AMode);

    if AMode <> mmAdd then
    begin
      EraseOriginalMaskPixelsOutOfAccumRGN;
    end;
  end;
end;

// create rectangular selection
procedure TgmSelection.CreateRectMarqueeRGN(
  const AStartPoint, AEndPoint: TPoint; const AMode: TgmMarqueeMode);
begin
  FOriginalMask.Canvas.Brush.Color := clWhite;
  FNewRGN := CreateRectRGN(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);

  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// create regular polygonal selection
procedure TgmSelection.CreateRegularPolygonRGN(
  const ACenterPoint, ACurrentPoint: TPoint; const ASides: Integer;
  const AMode: TgmMarqueeMode);
var
  LVertices : array of TPoint;
begin
  SetLength(LVertices, ASides + 1);
  CalcRegularPolygonVertices(LVertices, ACenterPoint, ACurrentPoint, ASides);
  CreatePolygonMarqueeRGN(LVertices, AMode);
end;

// create rounded rectangular selection
procedure TgmSelection.CreateRoundRectMarqueeRGN(
  const AStartPoint, AEndPoint: TPoint; const AAngle1, AAngle2: Integer;
  const AMode: TgmMarqueeMode);
begin
  FOriginalMask.Canvas.Brush.Color := clWhite;
  FNewRGN := CreateRoundRectRGN(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y, AAngle1, AAngle2);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);

  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// create single column selection
procedure TgmSelection.CreateSingleColumnRGN(
  const X, AStartY, AEndY: Integer; const AMode: TgmMarqueeMode);
var
  LStaticPolygon : array [0 .. 3] of TPoint;
begin
  LStaticPolygon[0] := Point(X,     AStartY);
  LStaticPolygon[1] := Point(X + 1, AStartY);
  LStaticPolygon[2] := Point(X + 1, AEndY + 1);
  LStaticPolygon[3] := Point(X,     AEndY + 1);

  FOriginalMask.Canvas.Brush.Color := clWhite;
  FNewRGN := CreatePolygonRGN(LStaticPolygon, 4, ALTERNATE);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);

  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// create single row selection
procedure TgmSelection.CreateSingleRowRGN(const AStartX, Y, AEndX: Integer;
  const AMode: TgmMarqueeMode);
var
  LStaticPolygon : array [0 .. 3] of TPoint;
begin
  LStaticPolygon[0] := Point(AStartX,   Y);
  LStaticPolygon[1] := Point(AEndX + 1, Y);
  LStaticPolygon[2] := Point(AEndX + 1, Y + 1);
  LStaticPolygon[3] := Point(AStartX,   Y + 1);

  FOriginalMask.Canvas.Brush.Color := clWhite;
  FNewRGN := CreatePolygonRGN(LStaticPolygon, 4, ALTERNATE);

  FillRGN(FOriginalMask.Canvas.Handle, FNewRGN, FOriginalMask.Canvas.Brush.Handle);
  CombineRGNToAccumRGN(AMode);

  if AMode <> mmAdd then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;
end;

// cut the bitmaps and mask
procedure TgmSelection.CutRegionFromOriginal;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  FSourceBitmap.Changed;
  
  // cut the picture
  CopyRect32WithARGB( FCutOriginal, FSourceBitmap,
                      Rect(FMaskBorderStart.X, FMaskBorderStart.Y,
                           FMaskBorderEnd.X + 1, FMaskBorderEnd.Y + 1), clWhite32 );

  // cut the marquee region
  CopyRect32WithARGB( FCutMask, FOriginalMask,
                      Rect(FMaskBorderStart.X, FMaskBorderStart.Y,
                           FMaskBorderEnd.X + 1, FMaskBorderEnd.Y + 1), clWhite32 );

  // get feathering mask
  GetFeatherMask;
end;

// drawing the accumulated hRGN to the FOriginalMask
procedure TgmSelection.DrawAccumRGN;
begin
  FOriginalMask.Canvas.Brush.Color := clBlack;
  FOriginalMask.Canvas.FillRect(FOriginalMask.Canvas.ClipRect);

  FOriginalMask.Canvas.Brush.Color := clWhite;
  PaintRGN(FOriginalMask.Canvas.Handle, FAccumRGN);
end;

// based on the code of GR32_SeedFill.pas written by Mattias Andersson
procedure TgmSelection.DrawMarchingAnts;
var
  i, j, x, y        : Integer;
  LXOffset          : Integer;
  LYOffset          : Integer;
  LMarchingAntsLine : TgmRectRegionNode;
  LStartPoint       : TPoint;
  LEndPoint         : TPoint;
  LBitmapRect       : TRect;
begin
  LBitmapRect := FImageControl.GetBitmapRect;
  LXOffset    := Round(FMaskBorderStart.X * FImageControl.Scale) + LBitmapRect.Left - 1;
  LYOffset    := Round(FMaskBorderStart.Y * FImageControl.Scale) + LBitmapRect.Top  - 1;

  if FMarchingAntsLineList.Count > 0 then
  begin
    for i := 0 to (FMarchingAntsLineList.Count - 1) do
    begin
      // get the current Marching Ants line
      LMarchingAntsLine := TgmRectRegionNode(FMarchingAntsLineList.Items[i]);

      ScaleMarchingAntsLine(LMarchingAntsLine, LStartPoint, LEndPoint);

      LStartPoint.X := LStartPoint.X + LXOffset;
      LStartPoint.Y := LStartPoint.Y + LYOffset;
      LEndPoint.X   := LEndPoint.X + LXOffset;
      LEndPoint.Y   := LEndPoint.Y + LYOffset;

      if (LStartPoint.X = LEndPoint.X) and
         (LStartPoint.Y = LEndPoint.Y) then
      begin
        x := LStartPoint.X;
        y := LStartPoint.Y;

        if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
        begin
          FImageControl.Canvas.Pixels[x, y] := clWhite;
        end
        else
        begin
          FImageControl.Canvas.Pixels[x, y] := clBlack;
        end;
      end
      else if (LStartPoint.X = LEndPoint.X) then
      begin
        for j := LStartPoint.Y to (LEndPoint.Y - 1) do
        begin
          x := LStartPoint.X;
          y := j;

          if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
          begin
            FImageControl.Canvas.Pixels[x, y] := clWhite;
          end
          else
          begin
            FImageControl.Canvas.Pixels[x, y] := clBlack;
          end;
        end;
      end
      else if (LStartPoint.Y = LEndPoint.Y) then
      begin
        for j := LStartPoint.X to (LEndPoint.X - 1) do
        begin
          x := j;
          y := LStartPoint.Y;

          if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
          begin
            FImageControl.Canvas.Pixels[x, y] := clWhite;
          end
          else
          begin
            FImageControl.Canvas.Pixels[x, y] := clBlack;
          end;
        end;
      end;
    end;
  end;
end;

procedure TgmSelection.DrawMarchingAnts(ABuffer: TBitmap32);
var
  i, j, x, y        : Integer;
  LXOffset          : Integer;
  LYOffset          : Integer;
  LMarchingAntsLine : TgmRectRegionNode;
  LStartPoint       : TPoint;
  LEndPoint         : TPoint;
  LBitmapRect       : TRect;
begin
  LBitmapRect := FImageControl.GetBitmapRect;
  LXOffset    := Round(FMaskBorderStart.X * FImageControl.Scale) + LBitmapRect.Left - 1;
  LYOffset    := Round(FMaskBorderStart.Y * FImageControl.Scale) + LBitmapRect.Top  - 1;

  if FMarchingAntsLineList.Count > 0 then
  begin
    for i := 0 to (FMarchingAntsLineList.Count - 1) do
    begin
      // get the current Marching Ants line
      LMarchingAntsLine := TgmRectRegionNode(FMarchingAntsLineList.Items[i]);

      ScaleMarchingAntsLine(LMarchingAntsLine, LStartPoint, LEndPoint);

      LStartPoint.X := LStartPoint.X + LXOffset;
      LStartPoint.Y := LStartPoint.Y + LYOffset;
      LEndPoint.X   := LEndPoint.X + LXOffset;
      LEndPoint.Y   := LEndPoint.Y + LYOffset;

      if (LStartPoint.X = LEndPoint.X) and
         (LStartPoint.Y = LEndPoint.Y) then
      begin
        x := LStartPoint.X;
        y := LStartPoint.Y;

        if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
        begin
          ABuffer.PixelS[x, y] := clWhite;
        end
        else
        begin
          ABuffer.PixelS[x, y] := clBlack;
        end;
      end
      else if (LStartPoint.X = LEndPoint.X) then
      begin
        for j := LStartPoint.Y to (LEndPoint.Y - 1) do
        begin
          x := LStartPoint.X;
          y := j;

          if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
          begin
            ABuffer.PixelS[x, y] := clWhite;
          end
          else
          begin
            ABuffer.PixelS[x, y] := clBlack;
          end;
        end;
      end
      else if (LStartPoint.Y = LEndPoint.Y) then
      begin
        for j := LStartPoint.X to (LEndPoint.X - 1) do
        begin
          x := j;
          y := LStartPoint.Y;

          if ( (y + FAnimateBias + x) mod 8 ) >= 4 then
          begin
            ABuffer.PixelS[x, y] := clWhite;
          end
          else
          begin
            ABuffer.PixelS[x, y] := clBlack;
          end;
        end;
      end;
    end;
  end;
end;

// draw the border han handles of the selection
procedure TgmSelection.DrawMarchingAntsBorder(ACanvas: TCanvas;
  const AXOffset, AYOffset: Integer; const ADrawHandles: Boolean);
var
  LTopLeft, LBottomRight    : TPoint;
  LTopRight, LBottomLeft    : TPoint;
  LTopCenter, LBottomCenter : TPoint;
  LLeftCenter, LRightCenter : TPoint;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  LTopLeft.X      := FMaskBorderStart.X - 1 - SELECTION_HANDLE_RADIUS + AXOffset;
  LTopLeft.Y      := FMaskBorderStart.Y - 1 - SELECTION_HANDLE_RADIUS + AYOffset;
  LBottomRight.X  := FMaskBorderEnd.X   + 1 + SELECTION_HANDLE_RADIUS + AXOffset;
  LBottomRight.Y  := FMaskBorderEnd.Y   + 1 + SELECTION_HANDLE_RADIUS + AYOffset;

  LTopRight.X     := LBottomRight.X;
  LTopRight.Y     := LTopLeft.Y;
  LBottomLeft.X   := LTopLeft.X;
  LBottomLeft.Y   := LBottomRight.Y;
  LTopCenter.X    := (LBottomRight.X - LTopLeft.X) div 2 + LTopLeft.X;
  LTopCenter.Y    := LTopLeft.Y;
  LBottomCenter.X := LTopCenter.X;
  LBottomCenter.Y := LBottomRight.Y;
  LLeftCenter.X   := LTopLeft.X;
  LLeftCenter.Y   := (LBottomRight.Y - LTopLeft.Y) div 2 + LTopLeft.Y;
  LRightCenter.X  := LBottomRight.X;
  LRightCenter.Y  := LLeftCenter.Y;

  with ACanvas do
  begin
    Pen.Mode    := pmNotXor;
    Pen.Style   := psDot;
    Pen.Width   := 1;
    Pen.Color   := clBlack;
    Brush.Style := bsClear;

    Rectangle(LTopLeft.X, LTopLeft.Y, LBottomRight.X, LBottomRight.Y);
  end;

  if ADrawHandles then
  begin
    DrawSelectionHandle(ACanvas, LTopLeft,      SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomRight,  SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LTopRight,     SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomLeft,   SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LTopCenter,    SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomCenter, SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LLeftCenter,   SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LRightCenter,  SELECTION_HANDLE_RADIUS);
  end;
end;

procedure TgmSelection.DrawMarchingAntsBorder(ACanvas: TCanvas;  
  const ADrawHandles: Boolean);
var
  LTopLeft, LBottomRight    : TPoint;
  LTopRight, LBottomLeft    : TPoint;
  LTopCenter, LBottomCenter : TPoint;
  LLeftCenter, LRightCenter : TPoint;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  LTopLeft.X := FMaskBorderStart.X - 1 - SELECTION_HANDLE_RADIUS;
  LTopLeft.Y := FMaskBorderStart.Y - 1 - SELECTION_HANDLE_RADIUS;

  LTopLeft := FImageControl.BitmapToControl(LTopLeft);

  LBottomRight.X  := FMaskBorderEnd.X   + 1 + SELECTION_HANDLE_RADIUS;
  LBottomRight.Y  := FMaskBorderEnd.Y   + 1 + SELECTION_HANDLE_RADIUS;

  LBottomRight := FImageControl.BitmapToControl(LBottomRight);

  LTopRight.X     := LBottomRight.X;
  LTopRight.Y     := LTopLeft.Y;
  LBottomLeft.X   := LTopLeft.X;
  LBottomLeft.Y   := LBottomRight.Y;
  LTopCenter.X    := (LBottomRight.X - LTopLeft.X) div 2 + LTopLeft.X;
  LTopCenter.Y    := LTopLeft.Y;
  LBottomCenter.X := LTopCenter.X;
  LBottomCenter.Y := LBottomRight.Y;
  LLeftCenter.X   := LTopLeft.X;
  LLeftCenter.Y   := (LBottomRight.Y - LTopLeft.Y) div 2 + LTopLeft.Y;
  LRightCenter.X  := LBottomRight.X;
  LRightCenter.Y  := LLeftCenter.Y;

  with ACanvas do
  begin
    Pen.Mode    := pmNotXor;
    Pen.Style   := psDot;
    Pen.Width   := 1;
    Pen.Color   := clBlack;
    Brush.Style := bsClear;

    Rectangle(LTopLeft.X, LTopLeft.Y, LBottomRight.X, LBottomRight.Y);
  end;

  if ADrawHandles then
  begin
    DrawSelectionHandle(ACanvas, LTopLeft,      SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomRight,  SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LTopRight,     SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomLeft,   SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LTopCenter,    SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LBottomCenter, SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LLeftCenter,   SELECTION_HANDLE_RADIUS);
    DrawSelectionHandle(ACanvas, LRightCenter,  SELECTION_HANDLE_RADIUS);
  end;
end;

// erase the pixels on FOriginalMask that out of the range of FAccumRGN
procedure TgmSelection.EraseOriginalMaskPixelsOutOfAccumRGN;
var
  x, y : Integer;
  p    : PColor32Array;
begin
{$RANGECHECKS OFF}

  for y := 0 to (FOriginalMask.Height - 1) do
  begin
    p := FOriginalMask.ScanLine[y];

    for x := 0 to (FOriginalMask.Width - 1) do
    begin
      if not PtInRegion(FAccumRGN, x, y) then
      begin
        p[x] := clBlack32;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

// flip the selection
procedure TgmSelection.FlipSelection(const AFlipMode: TgmFlipMode);
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  // switching the flip state
  case AFlipMode of
    fmHorizontal:
      begin
        FHorizFlipped := not FHorizFlipped;
      end;

    fmVertical:
      begin
        FVertFlipped  := not FVertFlipped;
      end;
  end;
  
  FlipBitmap(FFeatherMask, AFlipMode);  // flip the FFeatherMask bitmap as well
  FlipBitmap(FResizedMask, AFlipMode);  // flip the mask of the selection
  FlipBitmap(FCutOriginal, AFlipMode);  // flip the original cutted bitmap
  GetForeground;                        // update the foreground of the selection
  GetMarchingAntsLines;
end;

procedure TgmSelection.GetAccumRGN(const AMaskBmp: TBitmap32);
begin
  // search for the rectangles on the mask horizontally
  GetHorizontalRegions(AMaskBmp);

  { Convert the rectangles to path and convert the path to region,
    then store the region in FNewRGN. }
  FMagicWandShadow.Width              := AMaskBmp.Width;
  FMagicWandShadow.Height             := AMaskBmp.Height;
  FMagicWandShadow.Canvas.Brush.Color := clBlack;

  FMagicWandShadow.Canvas.FillRect(FMagicWandShadow.Canvas.ClipRect);
  MagicWandPathToRegion;

  // combine the new region with the accumulated one
  CombineRGNToAccumRGN(mmAdd);

  // draw the accumulated region
  DrawAccumRGN;
end; 

// get the actual start point and end point of the selection
procedure TgmSelection.GetActualMaskBorder;
var
  i                : Integer;
  LStartX, LStartY : Integer;
  LEndX, LEndY     : Integer;
  LLineNode        : TgmRectRegionNode;
begin
  LStartX := 10000;
  LStartY := 10000;
  LEndX   := -1000;
  LEndY   := -1000;

  { Get the boundary of the mask.
    Note: we need to search it on the original mask not the cutted one. }

  GetHorizontalRegions(FOriginalMask);
  GetVerticalRegions(FOriginalMask);
  GetHorizontalLines;
  GetVerticalLines;

  { Calculating the start point and end point of the boundary of the mask --
    Search them in the Top-Horizontal line list, Bottom-Horizontal line list,
    Left-Vertical line list, Right-Vertical line list, separately. So we need
    perform loop four times. }

  if FTopHorizLineList.Count > 0 then
  begin
    for i := 0 to (FTopHorizLineList.Count - 1) do
    begin
      LLineNode := TgmRectRegionNode(FTopHorizLineList.Items[i]);
      LStartX   := MinIntValue([LStartX, LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LStartY   := MinIntValue([LStartY, LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
      LEndX     := MaxIntValue([LEndX,   LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LEndY     := MaxIntValue([LEndY,   LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
    end;
  end;

  if FBottomHorizLineList.Count > 0 then
  begin
    for i := 0 to (FBottomHorizLineList.Count - 1) do
    begin
      LLineNode := TgmRectRegionNode(FBottomHorizLineList.Items[i]);
      LStartX   := MinIntValue([LStartX, LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LStartY   := MinIntValue([LStartY, LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
      LEndX     := MaxIntValue([LEndX,   LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LEndY     := MaxIntValue([LEndY,   LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
    end;
  end;

  if FLeftVertLineList.Count > 0 then
  begin
    for i := 0 to (FLeftVertLineList.Count - 1) do
    begin
      LLineNode := TgmRectRegionNode(FLeftVertLineList.Items[i]);
      LStartX   := MinIntValue([LStartX, LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LStartY   := MinIntValue([LStartY, LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
      LEndX     := MaxIntValue([LEndX,   LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LEndY     := MaxIntValue([LEndY,   LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
    end;
  end;

  if FRightVertLineList.Count > 0 then
  begin
    for i := 0 to (FRightVertLineList.Count - 1) do
    begin
      LLineNode := TgmRectRegionNode(FRightVertLineList.Items[i]);
      LStartX   := MinIntValue([LStartX, LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LStartY   := MinIntValue([LStartY, LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
      LEndX     := MaxIntValue([LEndX,   LLineNode.StartPoint.X, LLineNode.EndPoint.X]);
      LEndY     := MaxIntValue([LEndY,   LLineNode.StartPoint.Y, LLineNode.EndPoint.Y]);
    end;
  end;

  { If the cut region is out of the source bitmap, then we don't use the
    feather. Otherwise, we apply the feather radius, so the cut region has to
    be larger. }

  if (LStartX < 10000) and
     (LStartY < 10000) and
     (LEndX   > (-1000)) and
     (LEndY   > (-1000)) then
  begin
    FHasShadow := True;

    if ( (LStartX - FFeatherRadius) < 0 ) or
       ( (LStartY - FFeatherRadius) < 0 ) or
       ( (LEndX   + FFeatherRadius) > (FSourceBitmap.Width  - 1) ) or
       ( (LEndY   + FFeatherRadius) > (FSourceBitmap.Height - 1) ) then
    begin
      FFeatherRadius   := 0;
      FMaskBorderStart := Point(LStartX, LStartY);
      FMaskBorderEnd   := Point(LEndX,   LEndY);
      FOutputMsg       := 'The feather radius is out of the range.';
    end
    else
    begin
      FMaskBorderStart.X := LStartX - FFeatherRadius;
      FMaskBorderStart.Y := LStartY - FFeatherRadius;
      FMaskBorderEnd.X   := LEndX   + FFeatherRadius;
      FMaskBorderEnd.Y   := LEndY   + FFeatherRadius;
    end;

    FMaskBorderWidth  := FMaskBorderEnd.X - FMaskBorderStart.X;
    FMaskBorderHeight := FMaskBorderEnd.Y - FMaskBorderStart.Y;
  end
  else
  begin
    FHasShadow := False;
  end;
end;

// get the background of the selection
procedure TgmSelection.GetBackgroundWithFilledColor(
  const AFillColor: TColor32; const AChannelSet: TgmChannelSet);
var
  a                      : Cardinal;
  i, j                   : Integer;
  r, g, b, LWeight       : Byte;
  cr, cg, cb             : Byte;
  LCutRegion             : TBitmap32;
  LCutRow, LMaskRow      : PColor32Array;
  LCompoundRow, LBackRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  if FHasShadow = False then
  begin
    Exit;
  end;

  LMaskRow     := nil;
  LCompoundRow := nil;
  LBackRow     := nil;

  LCutRegion := TBitmap32.Create;
  try
    LCutRegion.DrawMode := dmOpaque;

    // the color for filling the cutted area on the background
    r := AFillColor shr 16 and $FF;
    g := AFillColor shr  8 and $FF;
    b := AFillColor        and $FF;

    FBackground.Assign(FSourceBitmap);
    
    LCutRegion.Width  := FFeatherMask.Width;
    LCutRegion.Height := FFeatherMask.Height;
    
    // cut an area for blend it with the filling color later
    LCutRegion.Canvas.CopyRect( LCutRegion.Canvas.ClipRect, FSourceBitmap.Canvas,
                                Rect(FMaskBorderStart.X, FMaskBorderStart.Y,
                                     FMaskBorderEnd.X, FMaskBorderEnd.Y) );

    FCompound.Width  := FFeatherMask.Width;
    FCompound.Height := FFeatherMask.Height;

    // blend the cutted bitmap with the filling color according to the mask
    for j := 0 to (FFeatherMask.Height - 1) do
    begin
      LCutRow      := LCutRegion.ScanLine[j];
      LMaskRow     := FFeatherMask.ScanLine[j];
      LCompoundRow := FCompound.ScanLine[j];

      for i := 0 to (FFeatherMask.Width - 1) do
      begin
        if csGrayscale in AChannelSet then
        begin
          LWeight := LMaskRow[i] shr 16 and $FF;
          b       := Intensity(AFillColor);
          cb      := Intensity(LCutRow[i]);
          cb      := ( b * LWeight + cb * (255 - LWeight) ) div 255;

          LCompoundRow[i] := $FF000000 or (cb shl 16) or (cb shl 8) or cb;
        end
        else
        begin
          a  := LCutRow[i]        and $FF000000;
          cr := LCutRow[i] shr 16 and $FF;
          cg := LCutRow[i] shr  8 and $FF;
          cb := LCutRow[i]        and $FF;

          // blend red channel
          if csRed in AChannelSet then
          begin
            LWeight := LMaskRow[i] shr 16 and $FF;
            cr      := ( r * LWeight + cr * (255 - LWeight) ) div 255;
          end;

          // blend green channel
          if csGreen in AChannelSet then
          begin
            LWeight := LMaskRow[i] shr 8 and $FF;
            cg      := ( g * LWeight + cg * (255 - LWeight) ) div 255;
          end;

          // blend blue channel
          if csBlue in AChannelSet then
          begin
            LWeight := LMaskRow[i] and $FF;
            cb      := ( b * LWeight + cb * (255 - LWeight) ) div 255;
          end;

          LCompoundRow[i] := a or (cr shl 16) or (cg shl 8) or cb;
        end;
      end;
    end;

    { Render the blended bitmap with the destination bitmap. Note that,
      the FMaskBorderStart and FMaskBorderEnd is the vertices of the
      blend area. }

    for j := 0 to (FFeatherMask.Height - 1) do
    begin
      if ( (FMaskBorderStart.Y + j) < 0 ) or
         ( (FMaskBorderStart.Y + j) > (FBackground.Height - 1) ) then
      begin
        Continue;
      end
      else
      begin
        LCompoundRow := FCompound.ScanLine[j];
        LMaskRow     := FFeatherMask.ScanLine[j];
        LBackRow     := FBackground.ScanLine[FMaskBorderStart.Y + j];
      end;

      for i := 0 to (FFeatherMask.Width - 1) do
      begin
        if ( (FMaskBorderStart.X + i) < 0 ) or
           ( (FMaskBorderStart.X + i) > (FBackground.Width - 1) ) then
        begin
          Continue;
        end
        else
        begin
          if ( RedComponent  (LMaskRow[i]) <> 0 ) and
             ( GreenComponent(LMaskRow[i]) <> 0 ) and
             ( BlueComponent (LMaskRow[i]) <> 0 ) then
          begin
            LBackRow[FMaskBorderStart.X + i] := LCompoundRow[i];
          end;
        end;
      end;
    end;

  finally
    LCutRegion.Free;
  end;

{$RANGECHECKS ON}
end;

procedure TgmSelection.GetBackgroundWithTransparent;
var
  i, j, LIntensity   : Integer;
  ba, br, bg, bb     : Byte;
  ma, mr, mg, mb     : Byte;
  LMaskRow, LBackRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  if FHasShadow = False then
  begin
    Exit;
  end;

  LMaskRow := nil;
  LBackRow := nil;

  for j := 0 to (FResizedMask.Height - 1) do
  begin
    if ( (FMaskBorderStart.Y + j) < 0 ) or
       ( (FMaskBorderStart.Y + j) > (FBackground.Height - 1) ) then
    begin
      Continue;
    end
    else
    begin
      LMaskRow := FResizedMask.ScanLine[j];
      LBackRow := FBackground.ScanLine[FMaskBorderStart.Y + j];
    end;

    for i := 0 to (FResizedMask.Width - 1) do
    begin
      if ( (FMaskBorderStart.X + i) < 0 ) or
         ( (FMaskBorderStart.X + i) > (FBackground.Width - 1) ) then
      begin
        Continue;
      end
      else
      begin
        mr := LMaskRow[i] shr 16 and $FF;
        mg := LMaskRow[i] shr  8 and $FF;
        mb := LMaskRow[i]        and $FF;

        br := LBackRow[FMaskBorderStart.X + i] shr 16 and $FF;
        bg := LBackRow[FMaskBorderStart.X + i] shr  8 and $FF;
        bb := LBackRow[FMaskBorderStart.X + i]        and $FF;

        // get the new alpha channel value
        LIntensity := (mr + mg + mb) div 3;
        ma         := LIntensity;
        ba         := LBackRow[FMaskBorderStart.X + i] shr 24 and $FF;
        ba         := Clamp(ba - ma, 0, 255);

        // only process the mask parts
        if (mr <> 0) and (mg <> 0) and (mb <> 0) then
        begin
          LBackRow[FMaskBorderStart.X + i] := (ba shl 24) or (br shl 16) or (bg shl 8) or bb;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

// get feathered mask
procedure TgmSelection.GetFeatherMask;
var
  i      : Integer;
  p1, p2 : PColor32;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  FFeatherMask.Assign(FCutMask);

  // apply feather by calling GBlur procedure
  if FFeatherRadius > 0 then
  begin
    GBlur32(FFeatherMask, FFeatherRadius);

    p1 := @FCutMask.Bits[0];
    p2 := @FFeatherMask.Bits[0];
    
    for i := 0 to (FCutMask.Width * FCutMask.Height - 1) do
    begin
      { Compound the FFeatherMask with the FCutMask in order to restore the
        original marquee, and store the feathering marquee. }
      if BlueComponent(p1^) = 255 then
      begin
        p2^ := p1^;
      end;

      Inc(p1);
      Inc(p2);
    end;
  end;
  
  FResizedMask.Assign(FFeatherMask);
end;

// get the foreground of the selection
procedure TgmSelection.GetForeground;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;
  
  FForeground.Assign(FCutOriginal);
  SmoothResize32(FForeground, FResizedMask.Width, FResizedMask.Height);

  { Modify the alpha channel of every pixel of the foreground with mask,
    to get the feathered foreground. }
  ChangeForegroundAlphaChannel;
end;

{ Get the Marching Ants lines --
  Because of the line maybe in on pixel, so the effect of the LineDDA call is bad.
  We try to adjust these lines:

  1. Try to move the top-horizontal lines up for 1 pixel, and extend them.
  2. Try to move the bottom-horizontal lines down for 1 pixel, and extend them.
  3. Try to move the left-vertical lines left for 1 pixel, and extend them.
  4. Try to move the right-vertical lines right for 1 pixel, and extend them. }

procedure TgmSelection.GetMarchingAntsLines;
var
  i                      : Integer;
  LStartPt, LEndPt       : TPoint;
  LLineNode1, LLineNode2 : TgmRectRegionNode;
  p                      : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  // clear the Marching Ants line list first
  FMarchingAntsLineList.Clear;

  if FHasShadow = False then
  begin
    Exit;
  end;

  { Get the boundary of the mask.
    Note: we need to search it on the resized mask not the cutted one. }

  GetHorizontalRegions(FResizedMask);
  GetVerticalRegions(FResizedMask);
  GetHorizontalLines;
  GetVerticalLines;

  SetLength(p, FResizedMask.Height);
  try
    // get the entry of every scanline of the FResizeMask bitmap
    for i := 0 to (FResizedMask.Height - 1) do
    begin
      p[i] := FResizedMask.ScanLine[i];
    end;

    // extend the top-horizontal line...
    if FTopHorizLineList.Count > 0 then
    begin
      for i := 0 to (FTopHorizLineList.Count - 1) do
      begin
        // get a horizontal line
        LLineNode1 := TgmRectRegionNode(FTopHorizLineList.Items[i]);

        // determine the start point of the line
        if ( (LLineNode1.StartPoint.X - 1) <= 0 ) or
           ( (LLineNode1.StartPoint.Y - 1) <= 0 ) then
        begin
          LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.StartPoint.Y - 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.StartPoint.Y - 1, LLineNode1.StartPoint.X - 1]) > 127 then
          begin
            LStartPt := Point(LLineNode1.StartPoint.X, LLineNode1.StartPoint.Y - 1);
          end
          else
          begin
            LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.StartPoint.Y - 1);
          end;
        end;

        // determine the end point of the line
        if ( (LLineNode1.StartPoint.X + 1) > (FResizedMask.Width - 1) ) or
           ( (LLineNode1.StartPoint.Y - 1) < 0 ) then
        begin
          LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y - 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.StartPoint.Y - 1, LLineNode1.EndPoint.X + 1]) > 127 then
          begin
            LEndPt := Point(LLineNode1.EndPoint.X, LLineNode1.StartPoint.Y - 1);
          end
          else
          begin
            LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y - 1);
          end;
        end;

        // store the processed line to Marching Ants line list
        LLineNode2            := TgmRectRegionNode.Create;
        LLineNode2.StartPoint := LStartPt;
        LLineNode2.EndPoint   := LEndPt;
        
        FMarchingAntsLineList.Add(LLineNode2);
      end;
    end;

    // extend the bottom-horizontal line...
    if FBottomHorizLineList.Count > 0 then
    begin
      for i := 0 to (FBottomHorizLineList.Count - 1) do
      begin
        // get a horizontal line
        LLineNode1 := TgmRectRegionNode(FBottomHorizLineList.Items[i]);

        // determine the start point of the line
        if ( (LLineNode1.StartPoint.X - 1) < 0 ) or
           ( (LLineNode1.EndPoint.Y + 1)   > (FResizedMask.Height - 1) ) then
        begin
          LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.EndPoint.Y + 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.EndPoint.Y + 1, LLineNode1.StartPoint.X - 1]) > 127 then
          begin
            LStartPt := Point(LLineNode1.StartPoint.X, LLineNode1.EndPoint.Y + 1);
          end
          else
          begin
            LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.EndPoint.Y + 1);
          end;
        end;

        // determine the end point of the line
        if ( (LLineNode1.EndPoint.X + 1) > (FResizedMask.Width  - 1) ) or
           ( (LLineNode1.EndPoint.Y + 1) > (FResizedMask.Height - 1) ) then
        begin
          LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y + 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.EndPoint.Y + 1, LLineNode1.EndPoint.X + 1]) > 127 then
          begin
            LEndPt := Point(LLineNode1.EndPoint.X, LLineNode1.StartPoint.Y + 1);
          end
          else
          begin
            LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y + 1);
          end;
        end;

        // store the processed line to Marching Ants line list
        LLineNode2            := TgmRectRegionNode.Create;
        LLineNode2.StartPoint := LStartPt;
        LLineNode2.EndPoint   := LEndPt;
        
        FMarchingAntsLineList.Add(LLineNode2);
      end;
    end;

    // extend the left-vertical line...
    if FLeftVertLineList.Count > 0 then
    begin
      for i := 0 to (FLeftVertLineList.Count - 1) do
      begin
        // get a vertical line
        LLineNode1 := TgmRectRegionNode(FLeftVertLineList.Items[i]);

        // determine the start point of the line
        if ( (LLineNode1.StartPoint.X - 1) < 0 ) or
           ( (LLineNode1.StartPoint.Y - 1) < 0 ) then
        begin
          LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.StartPoint.Y - 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.StartPoint.Y - 1, LLineNode1.StartPoint.X - 1]) > 127 then
          begin
            LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.StartPoint.Y);
          end
          else
          begin
            LStartPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.StartPoint.Y - 1);
          end;
        end;

        // determine the end point of the line
        if ( (LLineNode1.StartPoint.X - 1) < 0 ) or
           ( (LLineNode1.EndPoint.Y   + 1) > (FResizedMask.Height - 1) ) then
        begin
          LEndPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.EndPoint.Y + 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.EndPoint.Y + 1, LLineNode1.StartPoint.X - 1]) > 127 then
          begin
            LEndPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.EndPoint.Y);
          end
          else
          begin
            LEndPt := Point(LLineNode1.StartPoint.X - 1, LLineNode1.EndPoint.Y + 1);
          end;
        end;

        // store the processed line to Marching Ants line list
        LLineNode2            := TgmRectRegionNode.Create;
        LLineNode2.StartPoint := LStartPt;
        LLineNode2.EndPoint   := LEndPt;
        
        FMarchingAntsLineList.Add(LLineNode2);
      end;
    end;

    // extend the right-vertical line...
    if FRightVertLineList.Count > 0 then
    begin
      for i := 0 to (FRightVertLineList.Count - 1) do
      begin
        // get a vertical line
        LLineNode1 := TgmRectRegionNode(FRightVertLineList.Items[i]);

        // determine the start point of the line
        if ( (LLineNode1.EndPoint.X   + 1) > (FResizedMask.Width - 1) ) or
           ( (LLineNode1.StartPoint.Y - 1) < 0 ) then
        begin
          LStartPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y - 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.StartPoint.Y - 1, LLineNode1.EndPoint.X + 1]) > 127 then
          begin
            LStartPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y);
          end
          else
          begin
            LStartPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.StartPoint.Y - 1);
          end;
        end;

        // determine the end point of the line
        if ( (LLineNode1.EndPoint.X + 1) > (FResizedMask.Width  - 1) ) or
           ( (LLineNode1.EndPoint.Y + 1) > (FResizedMask.Height - 1) ) then
        begin
          LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.EndPoint.Y + 1);
        end
        else
        begin
          if BlueComponent(p[LLineNode1.EndPoint.Y + 1, LLineNode1.EndPoint.X + 1]) > 127 then
          begin
            LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.EndPoint.Y);
          end
          else
          begin
            LEndPt := Point(LLineNode1.EndPoint.X + 1, LLineNode1.EndPoint.Y + 1);
          end;
        end;

        // store the processed line to Marching Ants line list
        LLineNode2            := TgmRectRegionNode.Create;
        LLineNode2.StartPoint := LStartPt;
        LLineNode2.EndPoint   := LEndPt;
        
        FMarchingAntsLineList.Add(LLineNode2);
      end;
    end;

    FTopHorizLineList.Clear;
    FBottomHorizLineList.Clear;
    FLeftVertLineList.Clear;
    FRightVertLineList.Clear;
  finally
    SetLength(p, 0);
    p := nil;
  end;

{$RANGECHECKS ON}
end;

// Determine whether the mouse is over any of eight hanldles of the selection.
// Because of that the viewport of the image control could be scaled,
// so we need using constrol space coordinates to do the test for getting
// correct result when the scale property of image control is not 1.0 .
// So the values for parameters AX and AY should be in control coordinates
// space.
function TgmSelection.GetHandleAtPoint(
  const AX, AY, ARadius: Integer): TgmDrawingHandle;
var
  LStartPt, LEndPt : TPoint;
begin
  Result := dhNone;

  if FHasShadow = False then
  begin
    Exit;
  end;

  if not Assigned(FImageControl) then  // added by Xiaoguang
  begin
    Exit;
  end;

  LStartPt.X := FMaskBorderStart.X - 1 - SELECTION_HANDLE_RADIUS;
  LStartPt.Y := FMaskBorderStart.Y - 1 - SELECTION_HANDLE_RADIUS;
  LEndPt.X   := FMaskBorderEnd.X   + 1 + SELECTION_HANDLE_RADIUS;
  LEndPt.Y   := FMaskBorderEnd.Y   + 1 + SELECTION_HANDLE_RADIUS;

  LStartPt := FImageControl.BitmapToControl(LStartPt);  // added by Xiaoguang
  LEndPt   := FImageControl.BitmapToControl(LEndPt);    // added by Xiaoguang

  if SquareContainsPoint( LStartPt, ARadius, Point(AX, AY) ) then
  begin
    Result := dhAxAy;
  end
  else
  if SquareContainsPoint( LEndPt, ARadius, Point(AX, AY) ) then
  begin
    Result := dhBxBy;
  end
  else
  if SquareContainsPoint( Point(LStartPt.X, LEndPt.Y), ARadius, Point(AX, AY) ) then
  begin
    Result := dhAxBy;
  end
  else
  if SquareContainsPoint( Point(LEndPt.X, LStartPt.Y), ARadius, Point(AX, AY) ) then
  begin
    Result := dhBxAy;
  end
  else
  if SquareContainsPoint(  Point( LStartPt.X, (LEndPt.Y - LStartPt.Y) div 2 + LStartPt.Y ),
                           ARadius, Point(AX, AY)  ) then
  begin
    Result := dhLeftHalfAYBY;
  end
  else
  if SquareContainsPoint(  Point( LEndPt.X, (LEndPt.Y - LStartPt.Y) div 2 + LStartPt.Y ),
                           ARadius, Point(AX, AY)  ) then
  begin
    Result := dhRightHalfAYBY;
  end
  else
  if SquareContainsPoint(  Point( (LEndPt.X - LStartPt.X) div 2 + LStartPt.X, LStartPt.Y ),
                           ARadius, Point(AX, AY)  ) then
  begin
    Result := dhTopHalfAXBX;
  end
  else
  if SquareContainsPoint(  Point( (LEndPt.X - LStartPt.X) div 2 + LStartPt.X, LEndPt.Y ),
                           ARadius, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAXBX;
  end;
end;

// Pick up the horizontal lines of the rectangles that were found by
// vertically search the mask. And determine whether we could combine
// some of them to be one line.
procedure TgmSelection.GetHorizontalLines;

  // store the combined horizontal lines to list
  procedure MergeHorizLinesToList(const ANode: TgmRectRegionNode; AList: TList);
  var
    i              : Integer;
    LIsLinesMerged : Boolean;
    LLineNode      : TgmRectRegionNode;
  begin
    // at the very start, indicate that the lines have not been merged
    LIsLinesMerged := False;

    { Compare the passed horizontal line with the horizontal lines that were
      in the list. Combine them if we could. Otherwise, adding the passed line
      to list.

      The precondition of combination:
      The y coordinate of the start point of the passed line and the lines
      that were in the list are same, and the X coordinate of the start point
      of the passed line is greater then the X coordinate of the end point
      of the lines that were in the list by one pixel. }

    if AList.Count > 0 then
    begin
      for i := 0 to (AList.Count - 1) do
      begin
        // get the current horizontal line
        LLineNode := TgmRectRegionNode(AList.Items[i]);

        if (ANode.StartPoint.Y = LLineNode.StartPoint.Y) and
           (  Abs( (ANode.StartPoint.X - LLineNode.EndPoint.X) ) = 1  ) then
        begin
          { If the current horizontal line could combine with the passed
            line, combine them and exit the loop. }

          LLineNode.EndPoint := ANode.EndPoint;  // combine
          LIsLinesMerged     := True;            // mark of combination

          Break;                                 // exit the loop
        end;
      end;

      { If we couldn't combine the passed line with any of the line which were
        in the list, then adding the passed line to list. }
      if LIsLinesMerged = False then
      begin
        AList.Add(ANode);
      end;
    end;
  end; 

var
  j                : Integer;
  LStartPt, LEndPt : TPoint;
  LRegionNode      : TgmRectRegionNode;
  LLineNode        : TgmRectRegionNode;
begin
  FTopHorizLineList.Clear;     // clear the top-horizontal line list
  FBottomHorizLineList.Clear;  // clear the bottom-horizontal line list

  // if there are rectangles that found by searching the mask vertically--
  if FVertRegionList.Count > 0 then
  begin
    for j := 0 to (FVertRegionList.Count - 1) do
    begin
      // get the rectangle at the current index of list
      LRegionNode := TgmRectRegionNode(FVertRegionList.Items[j]);

      // get the vertices of the top-horizontal line of the rectangle
      LStartPt := Point(LRegionNode.StartPoint.X, LRegionNode.StartPoint.Y);
      LEndPt   := Point(LRegionNode.EndPoint.X,   LRegionNode.StartPoint.Y);

      { Create a new rectangle variable instance, assign the point values of
        the horizontal line object to it. And then use it for determine whether
        add it to the horizontal line list or combine it with the lines that
        were in the list. }

      LLineNode            := TgmRectRegionNode.Create;
      LLineNode.StartPoint := LStartPt;
      LLineNode.EndPoint   := LEndPt;

      { If the list is empty, it is impossible to combine, so just add the
        horizontal line to list. Otherwise, call to the MergeHorizLinesToList()
        to determine whether add the line to list or combine it with the lines
        that were in the list. }

      if FTopHorizLineList.Count = 0 then
      begin
        FTopHorizLineList.Add(LLineNode);
      end
      else
      begin
        MergeHorizLinesToList(LLineNode, FTopHorizLineList);
      end;

      // get the vertices of the bottom horizontal line of the same rectanlge
      LStartPt := Point(LRegionNode.StartPoint.X, LRegionNode.EndPoint.Y);
      LEndPt   := Point(LRegionNode.EndPoint.X,   LRegionNode.EndPoint.Y);

      { Create a new rectangle variable instance, assign the point values of
        the horizontal line object to it. And then use it for determine whether
        add it to the horizontal line list or combine it with the lines that
        were in the list. }

      LLineNode            := TgmRectRegionNode.Create;
      LLineNode.StartPoint := LStartPt;
      LLineNode.EndPoint   := LEndPt;

      { If the list is empty, it is impossible to combine, so just add the
        horizontal line to list. Otherwise, call to the MergeHorizLinesToList()
        to determine whether add the line to list or combine it with the lines
        that were in the list. }

      if FBottomHorizLineList.Count = 0 then
      begin
        FBottomHorizLineList.Add(LLineNode);
      end
      else
      begin
        MergeHorizLinesToList(LLineNode, FBottomHorizLineList);
      end;
    end;
  end;

  FVertRegionList.Clear;
end;

// searching rectangular regions on the mask horizontally, and store them to list
procedure TgmSelection.GetHorizontalRegions(AMaskBmp: TBitmap32);
var
  x, y, i          : Integer;
  LRegionNode      : TgmRectRegionNode;
  LLastRegion      : TgmRectRegionNode;
  LPrevRegion      : TgmRectRegionNode;
  LIsStartPtFound  : Boolean;
  LIsEndPtFound    : Boolean;
  LStartPt, LEndPt : TPoint;
  p                : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  LStartPt        := Point(0, 0);
  LEndPt          := Point(0, 0);
  LIsStartPtFound := False;  // whether the start point has been found, default is false
  LIsEndPtFound   := False;  // whether the end point has been found, default is false

  FHorizRegionList.Clear;

  SetLength(p, AMaskBmp.Height);
  try
    // get every entries of the scanlines of the mask
    for i := 0 to (AMaskBmp.Height - 1) do
    begin
      p[i] := AMaskBmp.ScanLine[i];
    end;

    // searching for rectangles horizontally, row by row
    for y := 0 to (AMaskBmp.Height - 1) do
    begin
      for x := 0 to (AMaskBmp.Width - 1) do
      begin
        // if the start point has not been found...
        if LIsStartPtFound = False then
        begin
          { Determine the current point is whether the start point--
            If the current point is at the most left of the bitmap and is brighter,
            or, if the current point is not at the most left of the bitmap and
            is still brighter, and if the point which is at the left side of the
            current point is darker, then the start point is the current point. }

          if ( (x = 0) and (BlueComponent(p[y, x]) > 127) ) or
             ( (x > 0) and (BlueComponent(p[y, x]) > 127) and
               (BlueComponent(p[y, x - 1]) < 128) ) then
          begin
            LStartPt        := Point(x, y);  // the current point is the start point
            LIsStartPtFound := True;         // The start point has been found!

            { Determine whether the current point is also the end point immediately--
              If the current point is at the most right of the bitmap, or, if the
              current point is not at the most right of the bitmap, and if the
              point which is at the right side of the current point is darker,
              then the current point is also the end point. }
              
            if ( x = (AMaskBmp.Width - 1) ) or
               ( BlueComponent(p[y, x + 1]) < 128 ) then
            begin
              LEndPt        := Point(x, y);  // the current point is also the end point
              LIsEndPtFound := True;         // The end point has been found!
            end;
          end;
        end;

        { if we only found the start point, we are going to find the end point-- }
        if LIsStartPtFound and (LIsEndPtFound = False) then
        begin
          { Determine the current point is whether the end point--
            If the current point is at the most right of the bitmap and is brighter,
            or, if the current point is not at the most right of the bitmap and
            is brighter, and if the point which is at the right side of the current
            point is darker, then the end point is the current point. }

          if ( (x = (AMaskBmp.Width - 1)) and (BlueComponent(p[y, x]) > 127) ) or
             ( (x < (AMaskBmp.Width - 1)) and (BlueComponent(p[y, x]) > 127) and
               (BlueComponent(p[y, x + 1]) < 128) ) then
          begin
            LEndPt        := Point(x, y);  // the current point is the end point
            LIsEndPtFound := True;         // The end point has been found!
          end;
        end;

        { If both the start point and end point have been found, then adding
          the rectangle (actually the line) to the list, then set the StartPtFound
          and EndPtFound to false. Then searching the next rectangle. }

        if LIsStartPtFound and LIsEndPtFound then
        begin
          LRegionNode            := TgmRectRegionNode.Create;
          LRegionNode.StartPoint := Point(LStartPt.X, LStartPt.Y);
          LRegionNode.EndPoint   := Point(LEndPt.X,   LEndPt.Y);
          
          FHorizRegionList.Add(LRegionNode);
          
          LIsStartPtFound := False;
          LIsEndPtFound   := False;
        end;
      end;

      { After searching a whole row, if the list has more than one rectangles,
        then determine whether we could combine the last rectangle ( line ) with
        the previous one ( line ). If we could, assign the end point of the last
        rectangle ( line ) to the end point of the previous rectangle ( line )
        for combine the last rectangle to the previous one. Then delete the last one. }

      if FHorizRegionList.Count > 1 then
      begin
        LLastRegion := TgmRectRegionNode(FHorizRegionList.Items[FHorizRegionList.Count - 1]);
        LPrevRegion := TgmRectRegionNode(FHorizRegionList.Items[FHorizRegionList.Count - 2]);

        { If the start X and end X coordinates of the last rectangle ( line )
          is equal to the start X and end X of the previous rectangle, respectively,
          and their Y coordinates have one pixel distance, then they can be combined. }

        if (LLastRegion.StartPoint.X = LPrevRegion.StartPoint.X) and
           (LLastRegion.EndPoint.X   = LPrevRegion.EndPoint.X) and
           ( (LLastRegion.EndPoint.Y - LPrevRegion.EndPoint.Y) = 1) then
        begin
          LPrevRegion.EndPoint := LLastRegion.EndPoint;
          
          FHorizRegionList.Delete(FHorizRegionList.Count - 1);
        end;
      end;
    end;
    
  finally
    SetLength(p, 0);
    p := nil;
  end;

{$RANGECHECKS ON}
end;

// Pick up the vertical lines of the rectangles that were found by
// horizontally search the mask. And determine whether we could combine
// some of them to be one line. 
procedure TgmSelection.GetVerticalLines;

  // store the combined vertical lines to list
  procedure MergeVertLinesToList(const ANode: TgmRectRegionNode; AList: TList);
  var
    i            : Integer;
    LLinesMerged : Boolean;
    LLineNode    : TgmRectRegionNode;
  begin
    // at the very start, indicate that the lines have not been merged
    LLinesMerged := False;

    { Compare the passed vertical line with the vertical lines that were
      in the list. Combine them if we could. Otherwise, adding the passed line
      to list.

      The precondition of combination:
      The X coordinate of the start point of the passed line and the lines
      that were in the list are same, and the Y coordinate of the start point
      of the passed line is greater then the Y coordinate of the end point
      of the lines that were in the list by one pixel. }

    if AList.Count > 0 then
    begin
      for i := 0 to (AList.Count - 1) do
      begin
        // get the current vertical line
        LLineNode := TgmRectRegionNode(AList.Items[i]);
        
        if (ANode.StartPoint.X = LLineNode.StartPoint.X) and
           (  Abs( (ANode.StartPoint.Y - LLineNode.EndPoint.Y) ) = 1  ) then
        begin
          { If the current vertical line could combine with the passed
            line, combine them and exit the loop. }
            
          LLineNode.EndPoint := ANode.EndPoint;  // combine
          LLinesMerged       := True;            // mark of combination

          Break;                                 // exit the loop
        end;
      end;

      { If we couldn't combine the passed line with any of the line which were
        in the list, then adding the passed line to list. }
      if not LLinesMerged then
      begin
        AList.Add(ANode);
      end;
    end;
  end; 

var
  j                : Integer;
  LStartPt, LEndPt : TPoint;
  LRegionNode      : TgmRectRegionNode;
  LLineNode        : TgmRectRegionNode;
begin
  FLeftVertLineList.Clear;   // clear the left-vertical line list
  FRightVertLineList.Clear;  // clear the right-vertical line list

  // if there are rectangles that found by searching the mask horizontally--
  if FHorizRegionList.Count > 0 then
  begin
    for j := 0 to (FHorizRegionList.Count - 1) do
    begin
      // get the rectangle at the current index of list
      LRegionNode := TgmRectRegionNode(FHorizRegionList.Items[j]);
      
      // get the vertices of the left-vertical line of the rectangle
      LStartPt := Point(LRegionNode.StartPoint.X, LRegionNode.StartPoint.Y);
      LEndPt   := Point(LRegionNode.StartPoint.X, LRegionNode.EndPoint.Y);

      { Create a new rectangle variable instance, assign the point values of
        the vertical line object to it. And then use it for determine whether
        add it to the vertical line list or combine it with the lines that
        were in the list. }

      LLineNode            := TgmRectRegionNode.Create;
      LLineNode.StartPoint := LStartPt;
      LLineNode.EndPoint   := LEndPt;

      { If the list is empty, it is impossible to combine, so just add the
        vertical line to list. Otherwise, call to the MergeVertLinesToList()
        to determine whether add the line to list or combine it with the lines
        that were in the list. }

      if FLeftVertLineList.Count = 0 then
      begin
        FLeftVertLineList.Add(LLineNode);
      end
      else
      begin
        MergeVertLinesToList(LLineNode, FLeftVertLineList);
      end;

      // get the vertices of the right vertical line of the same rectanlge
      LStartPt := Point(LRegionNode.EndPoint.X, LRegionNode.StartPoint.Y);
      LEndPt   := Point(LRegionNode.EndPoint.X, LRegionNode.EndPoint.Y);

      { Create a new rectangle variable instance, assign the point values of
        the vertical line object to it. And then use it for determine whether
        add it to the vertical line list or combine it with the lines that
        were in the list. }

      LLineNode            := TgmRectRegionNode.Create;
      LLineNode.StartPoint := LStartPt;
      LLineNode.EndPoint   := LEndPt;

      { If the list is empty, it is impossible to combine, so just add the
        vertical line to list. Otherwise, call to the MergeVertLinesToList()
        to determine whether add the line to list or combine it with the lines
        that were in the list. }

      if FRightVertLineList.Count = 0 then
      begin
        FRightVertLineList.Add(LLineNode);
      end
      else
      begin
        MergeVertLinesToList(LLineNode, FRightVertLineList);
      end;
    end;
  end;

  FHorizRegionList.Clear;
end;

// Searching rectangular regions on the mask vertically, and store them to list.
procedure TgmSelection.GetVerticalRegions(AMaskBmp: TBitmap32);
var
  x, y, i          : Integer;
  LRegionNode      : TgmRectRegionNode;
  LLastRegion      : TgmRectRegionNode;
  LPrevRegion      : TgmRectRegionNode;
  LIsStartPtFound  : Boolean;
  LIsEndPtFound    : Boolean;
  LStartPt, LEndPt : TPoint;
  p                : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  LStartPt        := Point(0, 0);
  LEndPt          := Point(0, 0);
  LIsStartPtFound := False;  // whether the start point has been found, default is false
  LIsEndPtFound   := False;  // whether the end point has been found, default is false

  FVertRegionList.Clear;

  SetLength(p, AMaskBmp.Height);
  try
    // get every entries of the scanlines of the mask
    for i := 0 to (AMaskBmp.Height - 1) do
    begin
      p[i] := AMaskBmp.ScanLine[i];
    end;

    // searching for rectangles vertically, column by column
    for x := 0 to (AMaskBmp.Width - 1) do
    begin
      for y := 0 to (AMaskBmp.Height - 1) do
      begin
        // if the start point has not been found...
        if LIsStartPtFound = False then
        begin
          { Determine the current point is whether the start point--
            If the current point is at the most top of the bitmap and is brighter,
            or, if the current point is not at the most top of the bitmap and is
            brighter, and if the point which is at the top side of the current
            point is darker, then the start point is the current point. }

          if ( (y = 0) and (BlueComponent(p[y, x]) > 127) ) or
             ( (y > 0) and (BlueComponent(p[y, x]) > 127) and
               (BlueComponent(p[y - 1, x]) < 128) ) then
          begin
            LStartPt        := Point(x, y);  // the current point is the start point
            LIsStartPtFound := True;         // The start point has been found!

            { Determine whether the current point is also the end point immediately--
              If the current point is at the most bottom of the bitmap, or, if
              the current point is not at the most bottom of the bitmap, and if
              the point which is at the bottom side of the current point is darker,
              then the current point is also the end point. }

            if ( y = (AMaskBmp.Height - 1) ) or
               ( BlueComponent(p[y + 1, x]) < 128 ) then
            begin
              LEndPt        := Point(x, y);  // the current point is also the end point
              LIsEndPtFound := True;         // The end point has been found!
            end;
          end;
        end;

        { If we only found the start point, we are going to find the end point-- }
        if LIsStartPtFound and (LIsEndPtFound = False) then
        begin
          { Determine the current point is whether the end point--
            If the current point is at the most bottom of the bitmap and is
            brighter, or, if the current point is not at the most bottom of the
            bitmap and brighter, and if the point which is at the bottom side of
            the current point is darker, then the end point is the current point. }

          if ( (y = (AMaskBmp.Height - 1)) and (BlueComponent(p[y, x]) > 127) ) or
             ( (y < (AMaskBmp.Height - 1)) and (BlueComponent(p[y, x]) > 127) and
               (BlueComponent(p[y + 1, x]) < 128) ) then
          begin
            LEndPt        := Point(x, y);  // the current point is the end point
            LIsEndPtFound := True;         // The end point has been found!
          end;
        end;

        { If both the start point and end point have been found, then adding
          the rectangle (actually the line) to the list, then set the StartPtFound
          and EndPtFound to false. Then searching the next rectangle. }

        if LIsStartPtFound and LIsEndPtFound then
        begin
          LRegionNode            := TgmRectRegionNode.Create;
          LRegionNode.StartPoint := Point(LStartPt.X, LStartPt.Y);
          LRegionNode.EndPoint   := Point(LEndPt.X, LEndPt.Y);
          
          FVertRegionList.Add(LRegionNode);
          
          LIsStartPtFound := False;
          LIsEndPtFound   := False;
        end;
      end;

      { After searching a whole column, if the list has more than one rectangles,
        then determine whether we could combine the last rectangle ( line ) with
        the previous one ( line ). If we could, assign the end point of the last
        rectangle ( line ) to the end point of the previous rectangle ( line )
        for combine the last rectangle to the previous one. Then delete the last
        one. }

      if FVertRegionList.Count > 1 then
      begin
        LLastRegion := TgmRectRegionNode(FVertRegionList.Items[FVertRegionList.Count - 1]);
        LPrevRegion := TgmRectRegionNode(FVertRegionList.Items[FVertRegionList.Count - 2]);

        { If the start Y and end Y coordinates of the last rectangle ( line )
          is equal to the start Y and end Y of the previous rectangle,
          respectively, and their X coordinates have one pixel distance,
          then they can be combined. }

        if (LLastRegion.StartPoint.Y = LPrevRegion.StartPoint.Y) and
           (LLastRegion.EndPoint.Y   = LPrevRegion.EndPoint.Y) and
           ( (LLastRegion.EndPoint.X - LPrevRegion.EndPoint.X) = 1) then
        begin
          LPrevRegion.EndPoint := LLastRegion.EndPoint;
          
          FVertRegionList.Delete(FVertRegionList.Count - 1);
        end;
      end;
    end;
  finally
    SetLength(p, 0);
    p := nil;
  end;

{$RANGECHECKS ON}
end;

// determine whether the mouse is pointing on the selection
function TgmSelection.IfPointsOnSelection(const X, Y: Integer): Boolean;
begin
  Result := False;

  if (X >= 0) and (X < FFeatherMask.Width) and
     (Y >= 0) and (Y < FFeatherMask.Height) then
  begin
    Result := ( (FFeatherMask.PixelS[X, Y] and $FF) <> 0 );
  end;
end; 

// invert the selection
procedure TgmSelection.InvertSelection;
begin
  FBackground.Assign(FSourceBitmap);            // restore the background for modify the selection start afresh
  InvertBitmap32(FOriginalMask, [csGrayscale]); // invert the OriginalMask bitmap for select the inverted area
  GetHorizontalRegions(FOriginalMask);          // search the rectangular regions on the FOriginalMask again
  RectToPathToRegion;                           // convert the rectangles to region
  FAccumRGN := FNewRGN;                         // the new region is the new accumulated mask
end;

// convert channel bitmap to selection
function TgmSelection.LoadChannelAsSelection(const AChannelBitmap: TBitmap32;
  const AMode: TgmMarqueeMode): Boolean;
var
  LChannelBit : PColor32;
  LMaskBit    : PColor32;
  i           : Integer;
begin
  Result := False;

  if (AChannelBitmap.Width  <> FSourceBitmap.Width) or
     (AChannelBitmap.Height <> FSourceBitmap.Height) then
  begin
    FOutputMsg := 'Dimension of the channel bitmap must be same as the background.';
    Exit;
  end;

  FMagicWandShadow.Assign(AChannelBitmap);

  // clear the old selection if necessary
  if AMode = mmNew then
  begin
    FOriginalMask.Clear(clBlack32);
  end;

  // compose the channel to mask
  LChannelBit := @FMagicWandShadow.Bits[0];
  LMaskBit    := @FOriginalMask.Bits[0];
  
  for i := 0 to (FMagicWandShadow.Width * FMagicWandShadow.Height - 1) do
  begin
    if BlueComponent(LChannelBit^) > BlueComponent(LMaskBit^) then
    begin
      LMaskBit^ := LChannelBit^;
    end;

    Inc(LChannelBit);
    Inc(LMaskBit);
  end;

  // search rectangles from the GradientMaskBMP horizontally
  GetHorizontalRegions(FMagicWandShadow);

  { Convert the rectangles to path and convert the path to region,
    store the region in FNewRGB. }
  MagicWandPathToRegion;

  // combine the new region with the accumulated region
  CombineRGNToAccumRGN(AMode);

  if AMode in [mmSubtract, mmIntersect, mmExcludeOverlap] then
  begin
    EraseOriginalMaskPixelsOutOfAccumRGN;
  end;

  Result := True;
end;

// convert the MagicWand mask to path, and then convert the path to region
procedure TgmSelection.MagicWandPathToRegion;
var
  i                : Integer;
  LStartX, LStartY : Integer;
  LEndX, LEndY     : Integer;
  LRegionNode      : TgmRectRegionNode;
begin
  { Draw the rectangles which were found in FMagicWandShadow when
    the BeginPath is opened to get the corresponding path. Then
    Convert the path to region. Note: when drawing the rectangles,
    make sure the X and Y coordinates of the end point of the
    rectangle increase by 2 pixels for getting the exact Region. }

  if FHorizRegionList.Count > 0 then
  begin
    BeginPath(FMagicWandShadow.Canvas.Handle);

    for i := 0 to (FHorizRegionList.Count - 1) do
    begin
      LRegionNode := TgmRectRegionNode(FHorizRegionList.Items[i]);
      LStartX     := LRegionNode.StartPoint.X;
      LStartY     := LRegionNode.StartPoint.Y;
      LEndX       := LRegionNode.EndPoint.X + 2;
      LEndY       := LRegionNode.EndPoint.Y + 2;
      
      FMagicWandShadow.Canvas.Rectangle(LStartX, LStartY, LEndX, LEndY);
    end;

    EndPath(FMagicWandShadow.Canvas.Handle);
    
    FNewRGN := PathToRegion(FMagicWandShadow.Canvas.Handle);
  end;
end;

procedure TgmSelection.MakeRegionWithMask(AMaskBmp: TBitmap32);
begin
  // search for the rectangles on the mask horizontally
  GetHorizontalRegions(AMaskBmp);

  { Convert the rectangles to path and convert the path to region,
    then store the region in FNewRGN. }
  RectToPathToRegion;
  
  FAccumRGN := FNewRGN;
end;

// convert the rectangle to region
procedure TgmSelection.RectToPathToRegion;
var
  i                : Integer;
  LStartX, LStartY : Integer;
  LEndX, LEndY     : Integer;
  LRegionNode      : TgmRectRegionNode;
  LTempBitmap      : TBitmap32;
begin
  { Drawing the rectangles which were found by search the mask horizontally in
    BeginPath() and EndPath() pair for getting the correspongding Path. Then
    convert the Path to Region. Note: when drawing the rectangles, make sure
    the X and Y coordinates of the end point of the rectangle increase by 2
    pixels for getting the exact Region. }
  
  if FHorizRegionList.Count > 0 then
  begin
    LTempBitmap := TBitmap32.Create;
    try
      LTempBitmap.SetSize(FOriginalMask.Width, FOriginalMask.Height);
      LTempBitmap.Clear($FF000000);

      BeginPath(LTempBitmap.Canvas.Handle);

      for i := 0 to (FHorizRegionList.Count - 1) do
      begin
        LRegionNode := TgmRectRegionNode(FHorizRegionList.Items[i]);
        LStartX     := LRegionNode.StartPoint.X;
        LStartY     := LRegionNode.StartPoint.Y;
        LEndX       := LRegionNode.EndPoint.X + 2;
        LEndY       := LRegionNode.EndPoint.Y + 2;

        LTempBitmap.Canvas.Rectangle(LStartX, LStartY, LEndX, LEndY);
      end;

      EndPath(LTempBitmap.Canvas.Handle);

      FNewRGN := PathToRegion(LTempBitmap.Canvas.Handle);
      FOriginalMask.Canvas.Brush.Color := clWhite;

      FillRGN(FOriginalMask.Canvas.Handle, FNewRGN,
              FOriginalMask.Canvas.Brush.Handle);

    finally
      LTempBitmap.Free;
    end;
  end;
end;

// rendering selection
procedure TgmSelection.RenderSelection(const ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i, j               : Integer;
  LWeight, na        : Byte;
  fa, fr, fg, fb     : Byte;
  ba, br, bg, bb     : Byte;
  LForeRow, LDestRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  if FHasShadow = False then
  begin
    Exit;
  end;

  LForeRow := nil;
  LDestRow := nil;

  { Blend the foreground with the destination bitmap. Note that,
    blending is start from the FMaskBorderStart.}

  for j := 0 to (FResizedMask.Height - 1) do
  begin
    if ( (FMaskBorderStart.Y + j) < 0 ) or
       ( (FMaskBorderStart.Y + j) > (ADestBmp.Height - 1) ) then
    begin
      Continue;
    end
    else
    begin
      LForeRow := FForeground.ScanLine[j];
      LDestRow := ADestBmp.ScanLine[FMaskBorderStart.Y + j];
    end;

    for i := 0 to (FResizedMask.Width - 1) do
    begin
      if ( (FMaskBorderStart.X + i) < 0 ) or
         ( (FMaskBorderStart.X + i) > (ADestBmp.Width - 1) ) then
      begin
        Continue;
      end
      else
      begin
        // get the final alpha channel
        fa := LForeRow[i] shr 24 and $FF;
        ba := LDestRow[FMaskBorderStart.X + i] shr 24 and $FF;
        na := MaxIntValue([fa, ba]);

        // get the RGB components of the foregound
        fr := LForeRow[i] shr 16 and $FF;
        fg := LForeRow[i] shr  8 and $FF;
        fb := LForeRow[i]        and $FF;

        // get the RGB components of the backgound
        br := LDestRow[FMaskBorderStart.X + i] shr 16 and $FF;
        bg := LDestRow[FMaskBorderStart.X + i] shr  8 and $FF;
        bb := LDestRow[FMaskBorderStart.X + i]        and $FF;

        { If the alpha channel of the foreground greater than zero,
          then try to blend it with background. }
        if fa > 0 then
        begin
          // implement blend
          if ba > 0 then
          begin
            LWeight := fa;

            if csGrayscale in AChannelSet then
            begin
              br := ( fr * LWeight + br * (255 - LWeight) ) div 255;
              LDestRow[FMaskBorderStart.X + i] := (na shl 24)or (br shl 16) or (br shl 8) or br;
            end
            else
            begin
              if csRed in AChannelSet then
              begin
                br := ( fr * LWeight + br * (255 - LWeight) ) div 255;
              end;
              
              if csGreen in AChannelSet then
              begin
                bg := ( fg * LWeight + bg * (255 - LWeight) ) div 255;
              end;
              
              if csBlue in AChannelSet then
              begin
                bb := ( fb * LWeight + bb * (255 - LWeight) ) div 255;
              end;
              
              LDestRow[FMaskBorderStart.X + i] := (na shl 24)or (br shl 16) or (bg shl 8) or bb;
            end;
          end
          else
          begin
            if (csRed   in AChannelSet) and
               (csGreen in AChannelSet) and
               (csBlue  in AChannelSet) then
            begin
              LDestRow[FMaskBorderStart.X + i] := LForeRow[i];
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

// resize the selection
procedure TgmSelection.ResizeSelection;
var
  LNewWidth, LNewHeight : Integer;
  LStdFeatherMask       : TBitmap;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  LNewWidth  := Abs(FMaskBorderEnd.X - FMaskBorderStart.X) + 1;
  LNewHeight := Abs(FMaskBorderEnd.Y - FMaskBorderStart.Y) + 1;

  if (LNewWidth  <> FMaskBorderWidth) or
     (LNewHeight <> FMaskBorderHeight) then
  begin
    LStdFeatherMask := TBitmap.Create;
    try
      LStdFeatherMask.Assign(FFeatherMask);

      FResizedMask.Width  := LNewWidth;
      FResizedMask.Height := LNewHeight;

      FResizedMask.Canvas.StretchDraw(FResizedMask.Canvas.ClipRect, LStdFeatherMask);
    finally
      LStdFeatherMask.Free;
    end;

    // resize the foreground with GetForeground()
    GetForeground;

    FMaskBorderWidth  := LNewWidth;
    FMaskBorderHeight := LNewHeight;

    if not FCornerStretched then
    begin
      FCornerStretched := True;
    end;
  end;
end;

// restore local background
procedure TgmSelection.RestoreBackground(ADestBmp: TBitmap32;
  const ARect: TRect);
var
  i, j, w, h, x, y   : Integer;
  LForeRow, LDestRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  if (ARect.Left >= ARect.Right) or
     (ARect.Top >= ARect.Bottom) then
  begin
    Exit;
  end;

  if (ADestBmp.Width  <> FBackground.Width) or
     (ADestBmp.Height <> FBackground.Height) then
  begin
    Exit;
  end;

  if FHasShadow = False then
  begin
    Exit;
  end;

  LForeRow := nil;
  LDestRow := nil;

  { retoring is start from the FMaskBorderStart.}

  w := ARect.Right  - ARect.Left;
  h := ARect.Bottom - ARect.Top;
  
  for j := 0 to h do
  begin
    y := ARect.Top + j;

    if (y < 0) or (y >= ADestBmp.Height) then
    begin
      Continue;
    end
    else
    begin
      if FTranslated or
         FCornerStretched or
         FTransformed or
         FHorizFlipped or
         FVertFlipped or
         FIsForeAlphaChanged then
      begin
        LForeRow := FBackground.ScanLine[y];
      end
      else
      begin
        LForeRow := FSourceBitmap.ScanLine[y];
      end;
      
      LDestRow := ADestBmp.ScanLine[y];
    end;

    for i := 0 to w do
    begin
      x := ARect.Left + i;

      if (x < 0) or (x >= ADestBmp.Width) then
      begin
        Continue;
      end
      else
      begin
        LDestRow[x] := LForeRow[x];
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

procedure TgmSelection.ScaleMarchingAntsLine(ASourceLine: TgmRectRegionNode;
  var AScaledStartPoint, AScaledEndPoint: TPoint);
var
  LScaledBorderWidth  : Integer;
  LScaledBorderHeight : Integer;
  LOffset             : Integer;
  LStartX, LStartY    : Integer;
  LEndX, LEndY        : Integer;
begin
  if not Assigned(FImageControl) then
  begin
    Exit;
  end;
  
  { Explanations:

    The dimension of the Marching-Ants border should be scale up or down
    according to the scale property of the image control. So we got the
    dimension like this:

      LWidth := Round(FMaskBorderWidth  * FImageControl.Scale);
      LWidth := Round(FMaskBorderHeight * FImageControl.Scale);

    Since the Marching-Ants lines should surround the selection area,
    so both the width and height of the mask should be larger than the
    selection area by 2 pixels for drawing borders.
    The exceeded part should also be scaled. So we adjusted the dimension
    like this:

      LWidth  := Round(FMaskBorderWidth  * FImageControl.Scale) + Round(2 * FImageControl.Scale);
      LHeight := Round(FMaskBorderHeight * FImageControl.Scale) + Round(2 * FImageControl.Scale)

    And we add to extend the mask to one more pixel to make the line
    drawing correctly.  So we got the following equations.
    }

  LScaledBorderWidth  := Round(FMaskBorderWidth  * FImageControl.Scale) + Round(2 * FImageControl.Scale) + 1;
  LScaledBorderHeight := Round(FMaskBorderHeight * FImageControl.Scale) + Round(2 * FImageControl.Scale) + 1;
  LOffset             := Round(1 * FImageControl.Scale);

  { Get scaled Marching Ants line. Note that, both the start point and
    end point should add offset for make the line at right place. }
  LStartX := ASourceLine.StartPoint.X * LScaledBorderWidth  div (FMaskBorderWidth  + 3) + LOffset;
  LStartY := ASourceLine.StartPoint.Y * LScaledBorderHeight div (FMaskBorderHeight + 3) + LOffset;
  LEndX   := ASourceLine.EndPoint.X   * LScaledBorderWidth  div (FMaskBorderWidth  + 3) + LOffset;
  LEndY   := ASourceLine.EndPoint.Y   * LScaledBorderHeight div (FMaskBorderHeight + 3) + LOffset;

  AScaledStartPoint := Point(LStartX, LStartY);
  AScaledEndPoint   := Point(LEndX, LEndY);
end;

// select all area
procedure TgmSelection.SelectAll;
begin
  FBackground.Assign(FSourceBitmap);    // restore the background for modify the selection start afresh
  FOriginalMask.Clear(clWhite32);       // fill the FOriginalMask with white to make the select-all mask
  GetHorizontalRegions(FOriginalMask);  // search the rectangular regions on the FOriginalMask again
  RectToPathToRegion;                   // convert the rectangles to region
  FAccumRGN := FNewRGN;                 // the new region is the new accumulated mask
end;

// check for whether the selection has been processed
function TgmSelection.SelectionHasProcessed: Boolean;
begin
  Result := (FFeathered       or
             FTranslated      or
             FCornerStretched or
             FHorizFlipped    or
             FVertFlipped     or
             FTransformed     or
             FTargetChanged);
end;

// convert APoint from selection coordinate space to bitmap coordinate space
function TgmSelection.SelectionPointToBitmapPoint(const APoint: TPoint): TPoint;
begin
  Result.X := APoint.X + FMaskBorderStart.X;
  Result.Y := APoint.Y + FMaskBorderStart.Y;
end; 

// based on the code of GR32_SeedFill.pas written by Mattias Andersson
procedure TgmSelection.SetAnimate(AValue: Boolean);
begin
  if AValue <> FAnimate then
  begin
    FAnimate       := AValue;
    FTimer.Enabled := AValue;

    Changed;
  end;
end;

procedure TgmSelection.SetImageControl(AImageControl: TCustomImage32);
begin
  if Assigned(AImageControl) then
  begin
    FImageControl := AImageControl;
  end;
end;

// set up the size of the mask to make it same as the source bitmap
procedure TgmSelection.SetOriginalMask(const AWidth, AHeight: Integer);
begin
  FOriginalMask.Width  := AWidth;
  FOriginalMask.Height := AHeight;
end;

// show the selection
procedure TgmSelection.ShowSelection(const ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
begin
  if Assigned(ADestBmp) then
  begin
    if FTranslated or
       FCornerStretched or
       FTransformed or
       FHorizFlipped or
       FVertFlipped or
       FIsForeAlphaChanged then
    begin
      CopyBitmap32(ADestBmp, FBackground);
    end
    else
    begin
      CopyBitmap32(ADestBmp, FSourceBitmap);
    end;

    RenderSelection(ADestBmp, AChannelSet);
  end;
end; 

// show specified area of the selection, ARect in selection coordinate space
procedure TgmSelection.ShowSelection(const ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet; const ARect: TRect);
var
  i, j, w, h         : Integer;
  x, y, tx, ty       : Integer;
  LWeight, na        : Byte;
  fa, fr, fg, fb     : Byte;
  ba, br, bg, bb     : Byte;
  LForeRow, LDestRow : PColor32Array;
begin
{$RANGECHECKS OFF}

  if (ARect.Left >= ARect.Right) or
     (ARect.Top >= ARect.Bottom) then
  begin
    Exit;
  end;

  if FHasShadow = False then
  begin
    Exit;
  end;

  LForeRow := nil;
  LDestRow := nil;

  { Blend the foreground with the destination bitmap. Note that,
    blending is start from the FMaskBorderStart.}

  w := ARect.Right  - ARect.Left;
  h := ARect.Bottom - ARect.Top;
  
  for j := 0 to h do
  begin
    y  := ARect.Top + j;
    ty := y + FMaskBorderStart.Y;

    if (y  < 0) or (y  >= FResizedMask.Height) or
       (ty < 0) or (ty >= ADestBmp.Height) then
    begin
      Continue;
    end
    else
    begin
      LForeRow := FForeground.ScanLine[y];
      LDestRow := ADestBmp.ScanLine[ty];
    end;

    for i := 0 to w do
    begin
      x  := ARect.Left + i;
      tx := x + FMaskBorderStart.X;

      if (x  < 0) or (x  >= FResizedMask.Width) or
         (tx < 0) or (tx >= ADestBmp.Width) then
      begin
        Continue;
      end
      else
      begin
        // get the final alpha channel
        fa := LForeRow[x]  shr 24 and $FF;
        ba := LDestRow[tx] shr 24 and $FF;
        na := MaxIntValue([fa, ba]);

        // get the RGB components of the foregound
        fr := LForeRow[x] shr 16 and $FF;
        fg := LForeRow[x] shr  8 and $FF;
        fb := LForeRow[x]        and $FF;

        // get the RGB components of the backgound
        br := LDestRow[tx] shr 16 and $FF;
        bg := LDestRow[tx] shr  8 and $FF;
        bb := LDestRow[tx]        and $FF;

        { If the alpha channel of the foreground greater than zero,
          then try to blend it with background. }
        if fa > 0 then
        begin
          // implement blend
          if ba > 0 then
          begin
            LWeight := fa;

            if csGrayscale in AChannelSet then
            begin
              br := ( fr * LWeight + br * (255 - LWeight) ) div 255;
              LDestRow[tx] := (na shl 24)or (br shl 16) or (br shl 8) or br;
            end
            else
            begin
              if csRed in AChannelSet then
              begin
                br := ( fr * LWeight + br * (255 - LWeight) ) div 255;
              end;
              
              if csGreen in AChannelSet then
              begin
                bg := ( fg * LWeight + bg * (255 - LWeight) ) div 255;
              end;
              
              if csBlue in AChannelSet then
              begin
                bb := ( fb * LWeight + bb * (255 - LWeight) ) div 255;
              end;
              
              LDestRow[tx] := (na shl 24)or (br shl 16) or (bg shl 8) or bb;
            end;
          end
          else
          begin
            if (csRed   in AChannelSet) and
               (csGreen in AChannelSet) and
               (csBlue  in AChannelSet) then
            begin
              LDestRow[tx] := LForeRow[x];
            end;
          end;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end;

// make sure FMaskBorderStart is at the upper left and FMaskBorderEnd is at
// the lower right.
procedure TgmSelection.StandardizeOrder;
var
  LTempA, LTempB : TPoint;
begin
  if FHasShadow = False then
  begin
    Exit;
  end;

  LTempA := FMaskBorderStart;
  LTempB := FMaskBorderEnd;

  // FMaskBorderStart is at the upper left
  FMaskBorderStart.X := MinIntValue([LTempA.X, LTempB.X]);
  FMaskBorderStart.Y := MinIntValue([LTempA.Y, LTempB.Y]);
  
  // FMaskBorderEnd is at the lower right
  FMaskBorderEnd.X := MaxIntValue([LTempA.X, LTempB.X]);
  FMaskBorderEnd.Y := MaxIntValue([LTempA.Y, LTempB.Y]);
end;

procedure TgmSelection.TranslateCutRegion(const ATranslateVector: TPoint);
begin
  FMaskBorderStart := AddPoints(FMaskBorderStart, ATranslateVector);
  FMaskBorderEnd   := AddPoints(FMaskBorderEnd,   ATranslateVector);

  // cut the picture
  CopyRect32WithARGB( FCutOriginal, FSourceBitmap,
                      Rect(FMaskBorderStart.X, FMaskBorderStart.Y,
                           FMaskBorderEnd.X + 1, FMaskBorderEnd.Y + 1), clWhite32 );

  GetForeground;

  { Because of the background is changed by the last cutting process,
    so, we need to restore the changed parts back. }
  ReplaceAlphaChannelWithDifference(FBackground, FSourceBitmap);

  // translate the mask
  FOriginalMask.Clear($FF000000);
  FOriginalMask.Draw(FMaskBorderStart.X, FMaskBorderStart.Y, FCutMask);
  GetHorizontalRegions(FOriginalMask);
  RectToPathToRegion;

  FAccumRGN := FNewRGN;
end;

procedure TgmSelection.TranslateSelection(const ATranslateVector: TPoint);
begin
  if (ATranslateVector.X <> 0) or (ATranslateVector.Y <> 0) then
  begin
    FMaskBorderStart := AddPoints(FMaskBorderStart, ATranslateVector);
    FMaskBorderEnd   := AddPoints(FMaskBorderEnd,   ATranslateVector);

    if not FTranslated then
    begin
      FTranslated := True;
    end;
  end;
end;

// clear the FOriginalMask bitmap and draw the FResizedMask bitmap on it
procedure TgmSelection.UpdateOriginalMaskWithResizedMask;
begin
  FOriginalMask.Clear(clBlack32);
  FOriginalMask.Draw(FMaskBorderStart.X, FMaskBorderStart.Y, FResizedMask);
end;

//-- TgmSelectionTransformation ------------------------------------------------

constructor TgmSelectionTransformation.Create(ASelection: TgmSelection);
begin
  inherited Create;
  
  FTransformMode       := tmNone;
  FSelection           := ASelection;
  FMaskBorderStartCopy := FSelection.FMaskBorderStart;
  FMaskBorderEndCopy   := FSelection.FMaskBorderEnd;
  FForegroundCopy      := TBitmap32.Create;
  FResizeMaskCopy      := TBitmap32.Create;

  FForegroundCopy.Assign(FSelection.Foreground);
  FResizeMaskCopy.Assign(FSelection.ResizedMask);

  FPT         := TProjectiveTransformation.Create;
  FPT.SrcRect := FloatRect(0, 0, FSelection.CutOriginal.Width - 1, FSelection.CutOriginal.Height - 1);
  FTT         := FPT;

  FRectTopLeft     := Point(0, 0);
  FRectBottomRight := Point(0, 0);
  FRectReady       := False;
  FTransforming    := False;
  InitVertices;
end;

destructor TgmSelectionTransformation.Destroy;
begin
  FForegroundCopy.Free;
  FResizeMaskCopy.Free;
  FPT.Free;
  
  inherited Destroy;
end;

procedure TgmSelectionTransformation.ConnectSelection(ASelection: TgmSelection);
begin
  if Assigned(ASelection) then
  begin
    FSelection := ASelection;
  end;
end;

procedure TgmSelectionTransformation.ExecuteTransform;
begin
  GenTransform;
  DoTransform;
end;

procedure TgmSelectionTransformation.AssignTransformData(
  const ATransformation: TgmSelectionTransformation);
var
  i : Byte;
begin
  if Assigned(ATransformation) then
  begin
    FMaskBorderStartCopy  := ATransformation.MaskBorderStartCopy;
    FMaskBorderEndCopy    := ATransformation.MaskBorderEndCopy;
    FRectTopLeft          := ATransformation.RectTopLeft;
    FRectBottomRight      := ATransformation.RectBottomRight;
    FRectReady            := ATransformation.IsRectReady;
    FTransforming         := ATransformation.IsTransforming;
    FSelectionCenterCoord := ATransformation.SelectionCenterCoord;

    FForegroundCopy.Assign(ATransformation.ForegroundCopy);
    FResizeMaskCopy.Assign(ATransformation.ResizeMaskCopy);

    for i := 0 to 3 do
    begin
      FVertices[i]        := ATransformation.Vertices[i];
      FExtraHandles[i]    := ATransformation.ExtraHandles[i];
      FVerticesLength[i]  := ATransformation.VerticesLength[i];
      FVerticesRadians[i] := ATransformation.VerticesRadians[i];
    end;
  end;
end;

procedure TgmSelectionTransformation.InitVertices;
begin
  if Assigned(FSelection) then
  begin
    FVertices[0]   := FSelection.FMaskBorderStart;
    FVertices[1].X := FSelection.FMaskBorderEnd.X;
    FVertices[1].Y := FSelection.FMaskBorderStart.Y;
    FVertices[2]   := FSelection.FMaskBorderEnd;
    FVertices[3].X := FSelection.FMaskBorderStart.X;
    FVertices[3].Y := FSelection.FMaskBorderEnd.Y;
  end;
  
  GetExtraHandles;
end;

procedure TgmSelectionTransformation.GenTransform;
begin
  if FRectReady then
  begin
    FPT.X0 := FVertices[0].X - FRectTopLeft.X;
    FPT.Y0 := FVertices[0].Y - FRectTopLeft.Y;
    FPT.X1 := FVertices[1].X - FRectTopLeft.X;
    FPT.Y1 := FVertices[1].Y - FRectTopLeft.Y;
    FPT.X2 := FVertices[2].X - FRectTopLeft.X;
    FPT.Y2 := FVertices[2].Y - FRectTopLeft.Y;
    FPT.X3 := FVertices[3].X - FRectTopLeft.X;
    FPT.Y3 := FVertices[3].Y - FRectTopLeft.Y;
  end;
end;

{ Get the coordinates of the extra handles,
  the order is: left, top, right, bottom. }
procedure TgmSelectionTransformation.GetExtraHandles;
begin
  FExtraHandles[0] := GetMidPointAtLine(FVertices[0], FVertices[3]);
  FExtraHandles[1] := GetMidPointAtLine(FVertices[0], FVertices[1]);
  FExtraHandles[2] := GetMidPointAtLine(FVertices[1], FVertices[2]);
  FExtraHandles[3] := GetMidPointAtLine(FVertices[2], FVertices[3]);
end;

procedure TgmSelectionTransformation.DrawOutline(ACanvas: TCanvas;
  const AOffsetVector: TPoint; const APenMode: TPenMode);
var
  LTempPenColor   : TColor;
  LTempPenWidth   : Integer;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
  LTempBrushColor : TColor;
  i               : Byte;
  LMainHandles    : array [0..3] of TPoint;
  LExtraHandles   : array [0..3] of TPoint;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psSolid;
    Pen.Mode    := APenMode;
    Brush.Style := bsClear;

    for i := 0 to 3 do
    begin
      // add the offset to the main handles
      LMainHandles[i].X := FVertices[i].X + AOffsetVector.X;
      LMainHandles[i].Y := FVertices[i].Y + AOffsetVector.Y;

      // add the offset to the extra handles
      LExtraHandles[i].X := FExtraHandles[i].X + AOffsetVector.X;
      LExtraHandles[i].Y := FExtraHandles[i].Y + AOffsetVector.Y;
    end;

    Polygon(LMainHandles);

    // draw handles
    for i := 0 to 3 do
    begin
      // draw the main handles
      Rectangle(LMainHandles[i].X - SELECTION_HANDLE_RADIUS,
                LMainHandles[i].Y - SELECTION_HANDLE_RADIUS,
                LMainHandles[i].X + SELECTION_HANDLE_RADIUS,
                LMainHandles[i].Y + SELECTION_HANDLE_RADIUS);

      // draw the extra handles
      Rectangle(LExtraHandles[i].X - SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].Y - SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].X + SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].Y + SELECTION_HANDLE_RADIUS);
    end;
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

procedure TgmSelectionTransformation.DrawOutline(ACanvas: TCanvas;
  const APenMode: TPenMode);    
var
  LTempPenColor   : TColor;
  LTempPenWidth   : Integer;
  LTempPenStyle   : TPenStyle;
  LTempPenMode    : TPenMode;
  LTempBrushStyle : TBrushStyle;
  LTempBrushColor : TColor;
  i               : Byte;
  LMainHandles    : array [0..3] of TPoint;
  LExtraHandles   : array [0..3] of TPoint;
  LPoint          : TPoint;
begin
  GetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);

  with ACanvas do
  begin
    Pen.Color   := clBlack;
    Pen.Width   := 1;
    Pen.Style   := psSolid;
    Pen.Mode    := APenMode;
    Brush.Style := bsClear;

    for i := 0 to 3 do
    begin
      // add the offset to the main handles
      LMainHandles[i].X := FVertices[i].X;
      LMainHandles[i].Y := FVertices[i].Y;

      // add the offset to the extra handles
      LExtraHandles[i].X := FExtraHandles[i].X;
      LExtraHandles[i].Y := FExtraHandles[i].Y;
    end;

    LPoint          := LMainHandles[0];
    LMainHandles[0] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint          := LMainHandles[1];
    LMainHandles[1] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint          := LMainHandles[2];
    LMainHandles[2] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint          := LMainHandles[3];
    LMainHandles[3] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint           := LExtraHandles[0];
    LExtraHandles[0] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint           := LExtraHandles[1];
    LExtraHandles[1] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint           := LExtraHandles[2];
    LExtraHandles[2] := FSelection.FImageControl.BitmapToControl(LPoint);

    LPoint           := LExtraHandles[3];
    LExtraHandles[3] := FSelection.FImageControl.BitmapToControl(LPoint);

    Polygon(LMainHandles);

    // draw handles
    for i := 0 to 3 do
    begin
      // draw the main handles
      Rectangle(LMainHandles[i].X - SELECTION_HANDLE_RADIUS,
                LMainHandles[i].Y - SELECTION_HANDLE_RADIUS,
                LMainHandles[i].X + SELECTION_HANDLE_RADIUS,
                LMainHandles[i].Y + SELECTION_HANDLE_RADIUS);

      // draw the extra handles
      Rectangle(LExtraHandles[i].X - SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].Y - SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].X + SELECTION_HANDLE_RADIUS,
                LExtraHandles[i].Y + SELECTION_HANDLE_RADIUS);
    end;
  end;

  SetCanvasProperties(ACanvas, LTempPenColor, LTempBrushColor, LTempPenWidth,
                      LTempPenStyle, LTempPenMode, LTempBrushStyle);
end;

// Determine whether the mouse is over any of the handles of the selection
// Because of that the viewport of the image control could be scaled,
// so we need using constrol space coordinates to do the test for getting
// correct result when the scale property of image control is not 1.0 .
// So the values for parameters AX and AY should be in control coordinates
// space.
function TgmSelectionTransformation.GetHandleAtPoint(
  const AX, AY, ARadius: Integer): TgmDrawingHandle;
begin
  Result := dhNone;

  if not Assigned(FSelection.FImageControl) then  // added by Xiaoguang
  begin
    Exit;
  end;

  // Xiaoguang -- convert the vertices to control space

  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FVertices[0]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhAxAy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FVertices[1]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhBxAy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FVertices[2]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhBxBy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FVertices[3]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhAxBy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FExtraHandles[0]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhLeftHalfAyBy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FExtraHandles[1]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhTopHalfAxBx;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FExtraHandles[2]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhRightHalfAyBy;
  end
  else
  if SquareContainsPoint( FSelection.FImageControl.BitmapToControl(FExtraHandles[3]),
                          ARadius, Point(AX, AY) ) then
  begin
    Result := dhBottomHalfAxBx;
  end;
end;

function TgmSelectionTransformation.PointOnSelectionBody(
  const AX, AY: Integer): Boolean;
var
  LSelectionX, LSelectionY : Integer;
begin
  LSelectionX := AX - FSelection.FMaskBorderStart.X;
  LSelectionY := AY - FSelection.FMaskBorderStart.Y;

  if (FSelection.ResizedMask.PixelS[LSelectionX, LSelectionY] and $FFFFFF) = $FFFFFF then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TgmSelectionTransformation.ChangeVertices(
  const APointArray: array of TPoint);
var
  i : Byte;
begin
  if High(APointArray) > 2 then
  begin
    for i := 0 to 3 do
    begin
      FVertices[i] := APointArray[i];
    end;
  end;
  
  GetExtraHandles;
  
  FRectReady := GetPolygonRect(FVertices, FRectTopLeft, FRectBottomRight);
end; 

// change the coordinates of the vertices
procedure TgmSelectionTransformation.ChangeVertices_Distort(
  const AOffsetVector: TPoint; const ADrawingHandle: TgmDrawingHandle);
begin
  case ADrawingHandle of
    dhAXAY:
      begin
        FVertices[0] := AddPoints(FVertices[0], AOffsetVector);
      end;
      
    dhBXAY:
      begin
        FVertices[1] := AddPoints(FVertices[1], AOffsetVector);
      end;
      
    dhBXBY:
      begin
        FVertices[2] := AddPoints(FVertices[2], AOffsetVector);
      end;
      
    dhAXBY:
      begin
        FVertices[3] := AddPoints(FVertices[3], AOffsetVector);
      end;
      
    dhLeftHalfAYBY:
      begin
        FVertices[0] := AddPoints(FVertices[0], AOffsetVector);
        FVertices[3] := AddPoints(FVertices[3], AOffsetVector);
      end;

    dhTopHalfAXBX:
      begin
        FVertices[0] := AddPoints(FVertices[0], AOffsetVector);
        FVertices[1] := AddPoints(FVertices[1], AOffsetVector);
      end;

    dhRightHalfAYBY:
      begin
        FVertices[1] := AddPoints(FVertices[1], AOffsetVector);
        FVertices[2] := AddPoints(FVertices[2], AOffsetVector);
      end;

    dhBottomHalfAXBX:
      begin
        FVertices[2] := AddPoints(FVertices[2], AOffsetVector);
        FVertices[3] := AddPoints(FVertices[3], AOffsetVector);
      end;
  end;
  
  GetExtraHandles;
  
  FRectReady := GetPolygonRect(FVertices, FRectTopLeft, FRectBottomRight);
end;

procedure TgmSelectionTransformation.ChangeVertices_Rotate(
  const ARadiansIncrement: Extended);
var
  i        : Integer;
  LRadians : Extended;
begin
  for i := 0 to 3 do
  begin
    LRadians       := FVerticesRadians[i] + ARadiansIncrement;
    FVertices[i].X := Round( FSelectionCenterCoord.X + FVerticesLength[i] * Cos(LRadians) );
    FVertices[i].Y := Round( FSelectionCenterCoord.Y + FVerticesLength[i] * Sin(LRadians) );
  end;
  
  GetExtraHandles;
  
  FRectReady := GetPolygonRect(FVertices, FRectTopLeft, FRectBottomRight);
end; 

procedure TgmSelectionTransformation.ChangeVertices_Scale(
  const AOffsetVector: TPoint; const ADrawingHandle: TgmDrawingHandle);
begin
  case ADrawingHandle of
    dhAXAY:
      begin
        FVertices[0]   := AddPoints(FVertices[0], AOffsetVector);
        FVertices[1].Y := FVertices[1].Y + AOffsetVector.Y;
        FVertices[3].X := FVertices[3].X + AOffsetVector.X;
      end;

    dhBXAY:
      begin
        FVertices[1]   := AddPoints(FVertices[1], AOffsetVector);
        FVertices[0].Y := FVertices[0].Y + AOffsetVector.Y;
        FVertices[2].X := FVertices[2].X + AOffsetVector.X;
      end;

    dhBXBY:
      begin
        FVertices[2]   := AddPoints(FVertices[2], AOffsetVector);
        FVertices[1].X := FVertices[1].X + AOffsetVector.X;
        FVertices[3].Y := FVertices[3].Y + AOffsetVector.Y;
      end;

    dhAXBY:
      begin
        FVertices[3]   := AddPoints(FVertices[3], AOffsetVector);
        FVertices[0].X := FVertices[0].X + AOffsetVector.X;
        FVertices[2].Y := FVertices[2].Y + AOffsetVector.Y;
      end;

    dhLeftHalfAYBY:
      begin
        FVertices[0].X := FVertices[0].X + AOffsetVector.X;
        FVertices[3].X := FVertices[3].X + AOffsetVector.X;
      end;

    dhTopHalfAXBX:
      begin
        FVertices[0].Y := FVertices[0].Y + AOffsetVector.Y;
        FVertices[1].Y := FVertices[1].Y + AOffsetVector.Y;
      end;

    dhRightHalfAYBY:
      begin
        FVertices[1].X := FVertices[1].X + AOffsetVector.X;
        FVertices[2].X := FVertices[2].X + AOffsetVector.X;
      end;

    dhBottomHalfAXBX:
      begin
        FVertices[2].Y := FVertices[2].Y + AOffsetVector.Y;
        FVertices[3].Y := FVertices[3].Y + AOffsetVector.Y;
      end;
  end;
  
  GetExtraHandles;
  
  FRectReady := GetPolygonRect(FVertices, FRectTopLeft, FRectBottomRight);
end; 

procedure TgmSelectionTransformation.TranslateVertices(
  const AOffsetVector: TPoint);
var
  i : Integer;
begin
  for i := 0 to 3 do
  begin
    FVertices[i] := AddPoints(FVertices[i], AOffsetVector);
  end;

  FSelectionCenterCoord.X := FSelectionCenterCoord.X + AOffsetVector.X;
  FSelectionCenterCoord.Y := FSelectionCenterCoord.Y + AOffsetVector.Y;

  GetExtraHandles;
  
  FRectReady := GetPolygonRect(FVertices, FRectTopLeft, FRectBottomRight);
end;

procedure TgmSelectionTransformation.DoTransform;
begin
  if FTT <> nil then
  begin
    // change the start point of the selection
    FSelection.FMaskBorderStart := FRectTopLeft;

    // resize the mask of the selection
    FSelection.ResizedMask.SetSize(FRectBottomRight.X - FRectTopLeft.X + 1,
                                   FRectBottomRight.Y - FRectTopLeft.Y + 1);

    FSelection.ResizedMask.Clear(clBlack32);
    try
      FSelection.FeatherMask.DrawMode       := dmCustom;
      FSelection.ResizedMask.DrawMode       := dmCustom;
      FSelection.FeatherMask.OnPixelCombine := BlendMode.NormalBlend;
      FSelection.ResizedMask.OnPixelCombine := BlendMode.NormalBlend;

      ReplaceAlphaChannelWithNewValue(FSelection.FeatherMask, 255);
      ReplaceAlphaChannelWithNewValue(FSelection.ResizedMask, 255);

      Transform(FSelection.ResizedMask, FSelection.FeatherMask, FTT);
    finally
      FSelection.FeatherMask.DrawMode := dmOpaque;
      FSelection.ResizedMask.DrawMode := dmOpaque;
    end;

    // resize the foreground of the selection
    FSelection.Foreground.SetSize(FRectBottomRight.X - FRectTopLeft.X + 1,
                                  FRectBottomRight.Y - FRectTopLeft.Y + 1);

    { The foreground must be transparent first, otherwise when drawing
      the selection on transprent layer will get bad result. }
    FSelection.Foreground.Clear($00000000);
    Transform(FSelection.Foreground, FSelection.CutOriginal, FTT);

    { Change the alpha channel of every pixel of the Foreground by the
      Mask for getting feathered effect. }
    FSelection.ChangeForegroundAlphaChannel;
    FSelection.GetMarchingAntsLines;  // Recalculate the Marching Ants lines.
  end;
end;

function TgmSelectionTransformation.GetVertexByIndex(AIndex: Byte): TPoint;
begin
  if AIndex < 4 then
  begin
    Result := FVertices[AIndex];
  end
  else
  begin
    Result := FVertices[0];
  end;
end; 

function TgmSelectionTransformation.GetExtraHandleByIndex(AIndex: Byte): TPoint;
begin
  if AIndex < 4 then
  begin
    Result := FExtraHandles[AIndex];
  end
  else
  begin
    Result := FExtraHandles[0];
  end;
end; 

function TgmSelectionTransformation.GetVertexLengthByIndex(
  AIndex: Byte): Extended;
begin
  if AIndex < 4 then
  begin
    Result := FVerticesLength[AIndex];
  end
  else
  begin
    Result := FVerticesLength[0];
  end;
end;

function TgmSelectionTransformation.GetVertexRadiansByIndex(
  AIndex: Byte): Extended;
begin
  if AIndex < 4 then
  begin
    Result := FVerticesRadians[AIndex];
  end
  else
  begin
    Result := FVerticesRadians[0];
  end;
end;

// accept the transformation
procedure TgmSelectionTransformation.AcceptTransform;
begin
  if FRectReady then
  begin
    FSelection.CutOriginal.Assign(FSelection.Foreground);
    FSelection.FeatherMask.Assign(FSelection.ResizedMask);

    FSelection.IsTransformed    := True;
    FSelection.MaskBorderWidth  := FRectBottomRight.X - FRectTopLeft.X;
    FSelection.MaskBorderHeight := FRectBottomRight.Y - FRectTopLeft.Y;
    FSelection.FMaskBorderStart := FRectTopLeft;
    FSelection.FMaskBorderEnd   := FRectBottomRight;
  end;

  FTransforming := False;
end;

// Cancel the transformation.
procedure TgmSelectionTransformation.CancelTransform;
begin
  if FRectReady then
  begin
    FSelection.FMaskBorderStart := FMaskBorderStartCopy;
    FSelection.FMaskBorderEnd   := FMaskBorderEndCopy;

    FSelection.Foreground.Assign(FForegroundCopy);
    FSelection.ResizedMask.Assign(FResizeMaskCopy);
    FSelection.GetMarchingAntsLines;
  end;

  FTransforming := False;
end;

procedure TgmSelectionTransformation.ShowTransformedSelection(
  const ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);
begin
  if Assigned(FSelection) and Assigned(ADestBmp) then
  begin
    ADestBmp.Assign(FSelection.Background);
    FSelection.RenderSelection(ADestBmp, AChannelSet);
  end;
end;

//-- TgmSelectionDistort -------------------------------------------------------

constructor TgmSelectionDistort.Create(ASelection: TgmSelection);
begin
  inherited Create(ASelection);
  FTransformMode := tmDistort;
end;

//-- TgmSelectionRotate --------------------------------------------------------

constructor TgmSelectionRotate.Create(ASelection: TgmSelection);
begin
  inherited Create(ASelection);

  FTransformMode := tmRotate;

  { Calculate the coordinates of the center point of the selection.
    Use to calculate the radians of every vertex to the center point. }
  FSelectionCenterCoord.X := FSelection.FMaskBorderStart.X + FSelection.ResizedMask.Width  / 2;
  FSelectionCenterCoord.Y := FSelection.FMaskBorderStart.Y + FSelection.ResizedMask.Height / 2;

  CalcVerticesRadians;
  CalcVerticesLength;
end;

// Before rotate the selection, calculate the radians of every vertex to the center point first.
procedure TgmSelectionRotate.CalcVerticesRadians;
var
  i : Integer;
begin
  for i := 0 to 3 do
  begin
    FVerticesRadians[i] := ArcTan2(FVertices[i].Y - FSelectionCenterCoord.Y,
                                   FVertices[i].X - FSelectionCenterCoord.X);
  end;
end;

{ Before rotate the selection, calculate the distance of every vertex to the
  center point first. }
procedure TgmSelectionRotate.CalcVerticesLength;
var
  i : Integer;
begin
  for i := 0 to 3 do
  begin
    FVerticesLength[i] := Sqrt( Sqr(FVertices[i].X - FSelectionCenterCoord.X) +
                                Sqr(FVertices[i].Y - FSelectionCenterCoord.Y) );
  end;
end;

procedure TgmSelectionRotate.UpdateRotateState;
begin
  CalcVerticesRadians;
  CalcVerticesLength;
end;

//-- TgmSelectionScale ---------------------------------------------------------

constructor TgmSelectionScale.Create(ASelection: TgmSelection);
begin
  inherited Create(ASelection);
  FTransformMode := tmScale;
end;

end.
