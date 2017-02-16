unit gmGradient;

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
  Types, Contnrs, Classes, Graphics,
{ Graphics32 }
  GR32, GR32_LowLevel,
{ GraphicsMagic }
  gmGridBased, gmFileFormatList;

//-- .grd files ----------------------------------------------------------------

const
  GRADIENT_FILE_ID      = $474D4347; // i.e. GMCG - GraphicsMagic Color Gradient
  GRADIENT_FILE_VERSION = 1;         // the file version we could process so far

type
  TgmGradientFileHeader = record
    FileID       : Cardinal;       // may be $474D4347
    FileVersion  : TgmVersionRec;  // version of the file format
    GradientCount: Cardinal;       // indicating how many gradients are in this file
  end;

type
  TgmGradientItem = class; // far definition
  TgmGradientStopCollection = class; // far def

  { TgmGradientStopItem }
  
  TgmGradientStopItem = class(TCollectionItem)
  private
    FValue        : TColor;
    FLocationScale: Double;
    FMidPoint     : Double;
    FOnlyAlphaUsed: Boolean;
    FSavedColor   : TColor;

    procedure SetLocationScale(const AValue: Double);
    procedure SetMidPoint(const AValue: Double);
    procedure SetValue(const AValue: TColor);
    procedure SetOnlyAlphaUsed(const AValue: Boolean);

    function GetNextLocationScale: Double;
    function GetCollection: TgmGradientStopCollection;
    function GetOnlyAlphaUsed: Boolean;
    function GetValidValue: TColor;
    function GetNextColor32: TColor32;
    function GetByteValue: Byte;
    function IfMidPointNotIsHalf: Boolean;

  protected
    procedure AssignTo(ADest: TPersistent); override;
    
  public
    constructor Create(ACollection: TCollection); override;

    function MidPointLocation: Integer;

    property Collection       : TgmGradientStopCollection read GetCollection;
    property NextLocationScale: Double                    read GetNextLocationScale;
    property EndColor         : TColor32                  read GetNextColor32;
    property OnlyAlphaUsed    : Boolean                   read GetOnlyAlphaUsed write SetOnlyAlphaUsed;
    property ValidValue       : TColor                    read GetValidValue; // Mature Color, such as clDefault >>out=clBlack
    property ByteValue        : Byte                      read GetByteValue;  // only used by Alpha, RGB should not use it.
    property SavedColor       : TColor                    read FSavedColor write FSavedColor; // temporary save, used for tristate between clDefault and clBackground
  published
    property Value        : TColor read FValue         write SetValue; // moveto TColor, safety for special value, as clDefault, clBackground
    property LocationScale: Double read FLocationScale write SetLocationScale;
    property MidPoint     : Double read FMidPoint      write SetMidPoint stored IfMidPointNotIsHalf;
  end;

  { TgmGradientStopCollection }
  
  TgmGradientStopCollection = class(TOwnedCollection)
  private
    FOnChange        : TNotifyEvent;
    FOnlyAlphaUsed   : Boolean;
    FOutputColorArray: TArrayOfColor32;

    procedure SetItem(AIndex: Integer; const AValue: TgmGradientStopItem);
    
    function GetItem(Index: Integer): TgmGradientStopItem;
    function GetOutputColorArray: TArrayOfColor32;

  protected
    procedure Update(AItem: TCollectionItem); override;
    function  GradientLength: Integer;
    
    property OutputColors: TArrayOfColor32 read GetOutputColorArray;
  public
    constructor Create(AOwner: TgmGradientItem);
    destructor Destroy; override;

    procedure Sort;
    procedure Delete(const AIndex: Integer);
    procedure DistributeAverage;

    function Owner: TgmGradientItem;
    function Add: TgmGradientStopItem;

    function Insert(const AIndex: Integer): TgmGradientStopItem; overload;
    function Insert(ALocationScale: Double; AValue: TColor): Integer; overload;
    function Insert32(ALocationScale: Double; AValue: TColor32): Integer;
    function ChangeLocationScale(const AColorIndex: Integer; const AScale: Double): Integer;

    property OnlyAlphaUsed        : Boolean             read FOnlyAlphaUsed write FOnlyAlphaUsed;
    property OnChange             : TNotifyEvent        read FOnChange      write FOnChange;
    property Items[Index: Integer]: TgmGradientStopItem read GetItem        write SetItem; default;

  published
  end;

  { TgmGradientItem }

  // A TgmGradientItem contains two TgmGradientStopCollection, one for RGB
  // gradients and the another for Alpha gradients.
  //
  // Note that, an instance of TgmGradientStopCollection contains several
  // objects that type of TgmGradientStopItem. Each TgmGradientStopItem object
  // stores Value as TColor, not TColor32, because sometimes we actually need
  // special TColor, as Photoshop done. Such as clDefault = Foreground Color ;
  // and clBackground = Background Color .
  TgmGradientItem = class(TgmGridBasedItem)
  private
    FAlphaGradient    : TgmGradientStopCollection;
    FRGBGradient      : TgmGradientStopCollection;
    FOutputColorArray : TArrayOfColor32;
    FGradientLength   : Integer;
    FBackgroundColor  : TColor;
    FForegroundColor  : TColor;
    FColorSpace       : string;
    FTag              : Integer;
    FOnChange         : TNotifyEvent;
    FCachedBitmap     : TBitmap32;

    procedure SetGradientLength(const ALength: Integer);
    procedure SetAlphaGradient(const AValue: TgmGradientStopCollection);
    procedure SetRGBGradient(const AValue: TgmGradientStopCollection);

    function GetAbsolutEndColor: TColor32;
    function GetAbsolutStartColor: TColor32;
    function GetBackgroundColor: TColor;
    function GetForegroundColor: TColor;
    function IsGroundColorStored: Boolean;

  protected
    FSpectrumValid: Boolean;
    
    procedure AssignTo(ADest: TPersistent); override;
    procedure SpectrumChanged(ASender: TObject);
    
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Clear; dynamic;
    function IsEmpty: Boolean; dynamic;

    procedure RefreshColorArray;
    procedure DrawColorGradients(const ABmp: TBitmap32; const AReversed: Boolean = False);

    // high speed drawing: use cachedBitmap, validate CachedBitmap if not yet valid
    procedure DrawCachedBitmap(const AWidth, AHeight: Integer;
      const ADestBmp: TBitmap32; const ADestPoint: TPoint);
      
    function CachedBitmap(const AWidth, AHeight: Integer): TBitmap32; override;
    
    function IsSpecialColorUsed: Boolean;

    property AbsolutStartColor : TColor32         read GetAbsolutStartColor;
    property AbsolutEndColor   : TColor32         read GetAbsolutEndColor;
    property OutputColors      : TArrayOfColor32  read FOutputColorArray;
    property ColorSpace        : string           read FColorSpace     write FColorSpace; // in case other than "RGBC"
    property Tag               : Integer          read FTag            write FTag; // multi-purpose temporary property
    property GradientLength    : Integer          read FGradientLength write SetGradientLength;

    // may override by collection
    property ForegroundColor   : TColor           read GetForegroundColor write FForegroundColor stored IsGroundColorStored;
    property BackgroundColor   : TColor           read GetBackgroundColor write FBackgroundColor stored IsGroundColorStored;
  published
    property DisplayName;
    property AlphaGradient : TgmGradientStopCollection read FAlphaGradient write SetAlphaGradient;
    property RGBGradient   : TgmGradientStopCollection read FRGBGradient   write SetRGBGradient;
    property OnChange      : TNotifyEvent              read FOnChange      write FOnChange; // used by gradient editor
  end;

  { TgmGradientCollection }

  TgmGradientCollection = class(TgmGridBasedCollection)
  private
    FStreamVersion   : Double;
    FForegroundColor : TColor;
    FBackgroundColor : TColor;

    function GetItem(Index: Integer): TgmGradientItem;

    procedure SetItem(AIndex: Integer; const AValue: TgmGradientItem);
    procedure SetForegroundColor(const AColor: TColor);
    procedure SetBackgroundColor(const AColor: TColor);

  protected

  public
    constructor Create(AOwner: TComponent); override;

    procedure Assign(ASource: TPersistent); override;
    procedure SaveToStream(const AStream: TStream); override;

    function IsValidIndex(const AIndex: Integer): Boolean; override;
    function Add: TgmGradientItem;
    
    function Draw(const AGradientIndex: Integer;
      const ACanvas: TCanvas; const ARect: TRect): Boolean;

    class function GetFileReaders : TgmFileFormatsList; override;
    class function GetFileWriters : TgmFileFormatsList; override;

    property Items[Index: Integer]: TgmGradientItem read GetItem          write SetItem; default;
    property StreamVersion        : Double          read FStreamVersion   write FStreamVersion;
    property ForegroundColor      : TColor          read FForegroundColor write SetForegroundColor;
    property BackgroundColor      : TColor          read FBackgroundColor write SetBackgroundColor;

  published
  end;
  

implementation

uses
{ Standard }
  SysUtils, Math,
{ Graphics32 }
  GR32_Blend,
{ GraphicsMagic }
  gmGradient_List,
  gmGradient_rwVer1,
  gmGradient_rwPhotoshop,
  gmGradient_rwUnversioned,
  gmGradient_rwPegtopNew,
  gmGradientRender,
  gmMiscFuncs;

var
  UGradientReaders : TgmFileFormatsList;
  UGradientWriters : TgmFileFormatsList;


{ TgmGradientStopItem }

constructor TgmGradientStopItem.Create(ACollection: TCollection);
begin
  inherited;
  FSavedColor    := clNone;
  FValue         := clWhite;
  FMidPoint      := 0.5;
  FLocationScale := 0;
end;

function TgmGradientStopItem.GetByteValue: Byte;
begin
  Result := FValue and $000000FF;
end;

function TgmGradientStopItem.GetCollection: TgmGradientStopCollection;
begin
  Result := inherited Collection as TgmGradientStopCollection;
end;

function TgmGradientStopItem.GetNextColor32: TColor32;
var
  LStop: TgmGradientStopItem;
begin
  if Self.Index = (Collection.Count -1) then
  begin
    Result := Color32(Self.ValidValue);
  end
  else
  begin
    LStop  := Collection.Items[Index +1];
    Result := Color32(LStop.ValidValue);
  end;
end;

function TgmGradientStopItem.GetNextLocationScale: Double;
var
  LStop: TgmGradientStopItem;
begin
  if Self.Index = (Collection.Count -1) then
  begin
    Result := 1.0;
  end
  else
  begin
    LStop  := Collection.Items[index +1];
    Result := LStop.LocationScale;
  end;
end;

function TgmGradientStopItem.GetOnlyAlphaUsed: Boolean;
begin
  Result := FOnlyAlphaUsed;
  
  if Assigned(Collection) then
  begin
    Result := Collection.FOnlyAlphaUsed;
  end;
end;

function TgmGradientStopItem.GetValidValue: TColor;
begin
  Result := FValue;
  
  if Result = clDefault then
  begin
    Result := TgmGradientItem(Collection.Owner).ForegroundColor;
  end
  else
  if Result = clBackground then
  begin
    Result := TgmGradientItem(Collection.Owner).BackgroundColor;
  end;
end;

// Resturns the actual horizontal location of the mid point at the length of
// the total steps
function TgmGradientStopItem.MidPointLocation: Integer;
begin
  Result := Round( (LocationScale * Collection.GradientLength ) +
                   ((GetNextLocationScale - LocationScale) *
                    Collection.GradientLength * MidPoint) );
end;

procedure TgmGradientStopItem.SetLocationScale(const AValue: Double);
begin
  FLocationScale := AValue;
  Changed(False);
end;

procedure TgmGradientStopItem.SetMidPoint(const AValue: Double);
begin
  FMidPoint := AValue;
  Changed(False);
end;

procedure TgmGradientStopItem.SetOnlyAlphaUsed(const AValue: Boolean);
begin
  FOnlyAlphaUsed := AValue;
  //but it will overrides while has Collection
end;

procedure TgmGradientStopItem.SetValue(const AValue: TColor);
var
  LValue: TColor;
  B     : Byte;
begin
  LValue := AValue;
  
  if OnlyAlphaUsed then
  begin
    B      := AValue and $FF;
    LValue := B + (B shl 8) + (B shl 16);
    //FillChar(LValue, 4, B);
  end;

  if FValue <> LValue then
  begin
    FValue := LValue;
    Changed(False);
  end;
end;

function TgmGradientStopItem.IfMidPointNotIsHalf: Boolean;
begin
  Result := MidPoint <> 0.5;
end;

procedure TgmGradientStopItem.AssignTo(ADest: TPersistent);
var
  LDest: TgmGradientStopItem;
begin
  if ADest = Self then
  begin
    Exit;
  end;

  if ADest is TgmGradientStopItem then
  begin
    LDest                := ADest as TgmGradientStopItem;
    LDest.FValue         := Self.FValue;
    LDest.FLocationScale := Self.FLocationScale;
    LDest.FMidPoint      := Self.FMidPoint;
  end
  else
  begin
    inherited; // assign error
  end;
end;

{ TgmGradientStopCollection }

constructor TgmGradientStopCollection.Create(AOwner: TgmGradientItem);
begin
  inherited Create(AOwner, TgmGradientStopItem);
end;

destructor TgmGradientStopCollection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TgmGradientStopCollection.Add: TgmGradientStopItem;
begin
  Result := TgmGradientStopItem(inherited Add);
end;

procedure TgmGradientStopCollection.Delete(const AIndex: Integer);
begin
  inherited Delete(AIndex);
  Changed;
end;

function TgmGradientStopCollection.GetItem(
  Index: Integer): TgmGradientStopItem;
begin
  if (Index < 0) or (Index >= Count) then
    Result := nil
  else
    Result := TgmGradientStopItem(inherited GetItem(Index));
end;

function TgmGradientStopCollection.Insert(
  const AIndex: Integer): TgmGradientStopItem;
begin
  Result := TgmGradientStopItem(inherited Insert(AIndex));
end;

function TgmGradientStopCollection.GradientLength: Integer;
begin
  Result := Owner.GradientLength;
end;

function TgmGradientStopCollection.Insert(ALocationScale: Double;
  AValue: TColor): Integer;
var
  LGradientStop: TgmGradientStopItem;
begin
  LGradientStop               := Self.Add;
  LGradientStop.LocationScale := ALocationScale;
  LGradientStop.Value         := AValue;

  Sort;
  Result := LGradientStop.Index;
end; 

procedure TgmGradientStopCollection.SetItem(AIndex: Integer;
  const AValue: TgmGradientStopItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TgmGradientStopCollection.Update(AItem: TCollectionItem);
begin
  inherited Update(AItem);
  
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

function TgmGradientStopCollection.GetOutputColorArray: TArrayOfColor32;
var
  LIndex    : Integer; // current position color32 of FOutputColorArray
  LLastColor: TColor32;

    // draw gradient to FOutputColorArray, using LIndex as start position
    procedure DrawGradientColorArray(const AStartColor, AEndColor: TColor32;
      const AGradientLength: Integer);
    var
      i, LGradientLength: Integer;
      r, g, b           : Byte;
      sr, sg, sb        : Byte;
      er, eg, eb        : Byte;
      ir, ig, ib        : Single;
      LRedInc           : Single;
      LGreenInc         : Single;
      LBlueInc          : Single;
      LStepScale        : Single;
    begin
      if (AGradientLength > 0) and (LIndex < Owner.GradientLength) then
      begin
        sr := AStartColor shr 16 and $FF;
        sg := AStartColor shr  8 and $FF;
        sb := AStartColor        and $FF;

        er := AEndColor shr 16 and $FF;
        eg := AEndColor shr  8 and $FF;
        eb := AEndColor        and $FF;

        LStepScale := 1 / AGradientLength;
        LRedInc    := (er - sr) * LStepScale;
        LGreenInc  := (eg - sg) * LStepScale;
        LBlueInc   := (eb - sb) * LStepScale;

        FOutputColorArray[LIndex] := AStartColor;
        LLastColor                := AStartColor;

        ir := sr;
        ig := sg;
        ib := sb;
        
        LGradientLength := 0;

        for i := (LIndex + 1) to (LIndex + AGradientLength - 1) do
        begin
          if i >= Owner.GradientLength then
          begin
            Continue;
          end;

          Inc(LGradientLength);

          ir := ir + LRedInc;
          ig := ig + LGreenInc;
          ib := ib + LBlueInc;

          r := Round(ir);
          g := Round(ig);
          b := Round(ib);

          r := Clamp(r);
          g := Clamp(g);
          b := Clamp(b);

          LLastColor          := $FF000000 or (r shl 16) or (g shl 8) or b;
          FOutputColorArray[i]:= LLastColor;
        end;

        Inc(LIndex, LGradientLength);
      end;
    end; 

    // because of ROUND()ing,  we make sure that every stop in it's own
    // occupy position
    procedure BugFixScaled(AStop:TgmGradientStopItem);
    var
      LValidIndex : Integer;
    begin
      LValidIndex := Min(Owner.GradientLength, Round(AStop.LocationScale * Owner.GradientLength));

      while LIndex < LValidIndex do
      begin
        FOutputColorArray[LIndex] := LLastColor;
        Inc(LIndex);
      end;
    end;

    procedure FillGradientColorArray(const AStop: TgmGradientStopItem);
    var
      LCopyCount: Integer;
      LMidLength: Integer;
      LMidColor : TColor32;
      LNextColor: TColor32;
    begin
      // In some PS grd, their many fist stop is same [0.0, 0.0, 0.0, 0.0...0.344, ...]
      if AStop.NextLocationScale = AStop.LocationScale then
      begin
        Exit;
      end;

      // process the part that before the first color...
      if (AStop.Index = 0) and (AStop.LocationScale > 0.0) then
      begin
        LCopyCount := Round(AStop.LocationScale * Owner.GradientLength);
        LLastColor := Color32(AStop.ValidValue);

        FillLongword(FOutputColorArray[LIndex], LCopyCount, LLastColor);
        Inc(LIndex, LCopyCount);
      end;

      // is it the last?
      if AStop.Index >= (Self.Count - 1) then
      begin
        // process the part that after the last color
        if ( AStop.LocationScale < 1.0 ) and
           ( LIndex < GradientLength ) then
        begin
          FillLongword( FOutputColorArray[LIndex], GradientLength - LIndex,
                        Color32(AStop.ValidValue) );
        end;
      end
      else
      begin
        BugFixScaled(AStop);
        
        LNextColor := Color32(Self.Items[AStop.Index + 1].ValidValue);
        LMidColor  := CombineReg( Color32(AStop.ValidValue), LNextColor, 128 );
        EMMS;

        LMidLength := Round( (AStop.GetNextLocationScale - AStop.LocationScale) *
          Self.GradientLength * AStop.MidPoint );

        // get first part of gradient colors ...
        DrawGradientColorArray( Color32(AStop.ValidValue), LMidColor, LMidLength );

        // get second part of gradient colors ...
        LMidLength := Round(AStop.GetNextLocationScale *
          Self.GradientLength ) - LIndex ; // -1 for correct length: 0..width-1 = colorsArrayLength -1;

        DrawGradientColorArray(LMidColor, LNextColor, LMidLength);
      end;
    end;
    
var
  i: Integer;
begin
  SetLength(FOutputColorArray, Owner.GradientLength);
  
  if Self.Count <= 1 then
  begin
    FillChar(FOutputColorArray, Owner.GradientLength, 0);
  end;

  LIndex := 0;

  for i := 0 to (Count - 1) do
  begin
    FillGradientColorArray(Self.Items[i]);
  end;
  
  Result := FOutputColorArray;
end;

function TgmGradientStopCollection.Owner: TgmGradientItem;
begin
  Result := GetOwner() as TgmGradientItem;
end;

// return the new index of the color
function TgmGradientStopCollection.ChangeLocationScale(
  const AColorIndex: Integer; const AScale: Double): Integer;
var
  LGradientStop: TgmGradientStopItem;
begin
  BeginUpdate;

  LGradientStop := Items[AColorIndex];
  LGradientStop.LocationScale := AScale;

  Sort;
  Result := LGradientStop.Index;
  
  EndUpdate;
end; 

function TgmGradientStopCollection.Insert32(ALocationScale: Double;
  AValue: TColor32): Integer;
begin
  Result := Insert( ALocationScale, WinColor(AValue) );
end;

//=============sort===================
{type
  TgmGradientCompare = function (Item1, Item2: TgmGradientStopItem): Integer;}

function SCompare(Item1, Item2: TgmGradientStopItem): Integer;
begin
  if Item1.LocationScale > Item2.LocationScale then
    Result := 1
  else
  if Item1.LocationScale = Item2.LocationScale then
    Result := 0
  else
    Result := -1;
end;

procedure QuickSort(SortList: TgmGradientStopCollection; L, R: Integer{;  SCompare: TgmGradientCompare});
var
  I, J   : Integer;
  P      : Pointer;
  iT, iJ : TgmGradientStopItem;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    
    repeat
      while SCompare(SortList[I], P) < 0 do
      begin
        Inc(I);
      end;
      
      while SCompare(SortList[J], P) > 0 do
      begin
        Dec(J);
      end;
      
      if I <= J then
      begin
        // exchange I & J
        {T := SortList[I];
        SortList[I] := SortList[J];  //J.index = i
        SortList[J] := T;}           //I.Index = j
        iT := SortList[I];
        iJ := SortList[J];
        iJ.Index := I;
        iT.Index := J;

        Inc(I);
        Dec(J);
      end;
      
    until I > J;
    
    if L < J then
    begin
      QuickSort(SortList, L, J{, SCompare});
    end;
    
    L := I;
  until I >= R;
end;

procedure TgmGradientStopCollection.Sort;
begin
  BeginUpdate;
  QuickSort(Self, 0, Count - 1);
  EndUpdate;
end;

// make location be distributed averagely
procedure TgmGradientStopCollection.DistributeAverage;
var
  i      : Integer;
  LScale : Double;
  LStop  : TgmGradientStopItem;
begin
  LScale := 1 / (Count - 1);

  BeginUpdate;

  for i := 0 to (Count - 1) do
  begin
    LStop := Items[i];
    LStop.LocationScale := i * LScale;

    LStop.MidPoint := 0.5;
  end;
  
  EndUpdate;
end; 

{ TgmGradientItem }

constructor TgmGradientItem.Create(ACollection: TCollection);
var
  a, c : TgmGradientStopItem;
begin
  inherited Create(ACollection);

  FAlphaGradient               := TgmGradientStopCollection.Create(Self);
  FAlphaGradient.OnlyAlphaUsed := True;
  FAlphaGradient.OnChange      := SpectrumChanged;
  FAlphaGradient.Add;
  
  a               := FAlphaGradient.Add;
  a.LocationScale := 1.0;

  FRGBGradient    := TgmGradientStopCollection.Create(Self);
  c               := FRGBGradient.Add;
  c.LocationScale := 0;
  c.FValue        := clDefault;
  
  c                     := FRGBGradient.Add;
  c.LocationScale       := 1.0;
  c.FValue              := clBackground;
  FRGBGradient.OnChange := SpectrumChanged;

  SetLength(FOutputColorArray, 0);
  
  FGradientLength  := 0;
  FForegroundColor := clBlack;
  FBackgroundColor := clWhite;
end; 

destructor TgmGradientItem.Destroy;
begin
  if Length(FOutputColorArray) > 0 then
  begin
    SetLength(FOutputColorArray, 0);
    FOutputColorArray := nil;
  end;

  FAlphaGradient.Free;
  FRGBGradient.Free;

  if Assigned(FCachedBitmap) then
    FCachedBitmap.Free;

  inherited Destroy;
end;

procedure TgmGradientItem.SetGradientLength(const ALength: Integer);
begin
  if ALength > 0 then
  begin
    FGradientLength := ALength;
  end;
end;

procedure AlphaForceColor(const A: TColor32; var C: TColor32);
var
  CX: TColor32Entry absolute C;
  AX: TColor32Entry absolute A;
begin
  CX.A := AX.B;
end;

procedure TgmGradientItem.RefreshColorArray;
var
  i : Integer;
  A : TArrayOfColor32;
begin
  A := nil;
  
  if FGradientLength > 0 then
  begin
    FOutputColorArray := FRGBGradient.OutputColors;
    A                 := FAlphaGradient.OutputColors;

    // copy gradient colors to output color array
    for i := 0 to (FGradientLength - 1) do
    begin
      AlphaForceColor(A[i], FOutputColorArray[i] );
    end;
    
    EMMS;
  end;
end;

procedure TgmGradientItem.DrawColorGradients(const ABmp: TBitmap32;
  const AReversed: Boolean = False);
var
  x, y           : Integer;
  LBmpColorArray : PColor32Array;
begin
// If a project with range checking taken on, access to any pixels on an image
// by PColor32Array will cause the compiler reports range checking error.
// We know that this is just a potential error. We know what we want to do,
// we know the following code will not cause such problem, so we just need
// to take off range checking at here temporarily.

{$RANGECHECKS OFF}

  if ( not Assigned(ABmp)) or
     ( ABmp.Height <= 0 ) then
  begin
    Exit;
  end;

  if ABmp.Width < FGradientLength then
  begin
    ABmp.Width := FGradientLength;
  end;

  for y := 0 to (ABmp.Height - 1) do
  begin
    LBmpColorArray := ABmp.ScanLine[y];

    for x := 0 to (ABmp.Width - 1) do
    begin
      if AReversed then
      begin
        LBmpColorArray[x] := FOutputColorArray[ABmp.Width - 1 - x];
      end
      else
      begin
        LBmpColorArray[x] := FOutputColorArray[x];
      end;
    end;
  end;

{$RANGECHECKS ON}
end; { DrawColorGradients }

procedure TgmGradientItem.AssignTo(ADest: TPersistent);
var
  LDest : TgmGradientItem;
begin
  if ADest = Self then
  begin
    Exit;
  end;

  if ADest is TgmGradientItem then
  begin
    LDest                  := ADest as TgmGradientItem;
    LDest.FDisplayName     := Self.DisplayName;
    LDest.FForegroundColor := Self.FForegroundColor;
    LDest.FBackgroundColor := Self.FBackgroundColor;

    LDest.AlphaGradient.Assign(Self.AlphaGradient);
    LDest.RGBGradient.Assign(Self.RGBGradient);
    LDest.RefreshColorArray;

    if Assigned(LDest.FOnChange) then
    begin
      LDest.FOnChange(LDest);
    end
  end
  else
  begin
    inherited; //assign error
  end;
end;

function TgmGradientItem.GetAbsolutEndColor: TColor32;
var
  LAlpha: Byte;
  LRGB  : TColor32;
begin
  if Length(FOutputColorArray) > 0 then
  begin
    Result := FOutputColorArray[ High(FOutputColorArray) ];
  end
  else
  begin
    LAlpha := AlphaGradient[AlphaGradient.Count - 1].ValidValue and $FF;
    LRGB   := Color32(RGBGradient[RGBGradient.Count - 1].ValidValue);
    Result := (LAlpha shl 24) or (LRGB and $00FFFFFF);
  end;
end;

function TgmGradientItem.GetAbsolutStartColor: TColor32;
var
  LAlpha: Byte;
  LRGB  : TColor32;
begin
  if Length(FOutputColorArray) > 0 then
  begin
    Result := FOutputColorArray[0];
  end
  else
  begin
    LAlpha := AlphaGradient[0].ValidValue and $FF;
    LRGB   := Color32(RGBGradient[0].ValidValue);
    Result := (LAlpha shl 24) or (LRGB and $00FFFFFF);
  end;
end;

function TgmGradientItem.IsSpecialColorUsed: Boolean;
var
  i : Integer;
begin
  Result := False;
  
  for i := 0 to (RGBGradient.Count - 1) do
  begin
    //remember that Value and FValue is different
    if (RGBGradient[i].FValue = clNone) or
       (RGBGradient[i].FValue = clDefault) or
       (RGBGradient[i].FValue = clBackground) then
    begin
      Result := True;
      Break; //enough one to set to true
    end;
  end;
end;

procedure TgmGradientItem.SetAlphaGradient(
  const AValue: TgmGradientStopCollection);
begin
  FAlphaGradient.Assign(AValue);
end;

procedure TgmGradientItem.SetRGBGradient(
  const AValue: TgmGradientStopCollection);
begin
  FRGBGradient.Assign(AValue);
end;

procedure TgmGradientItem.Clear;
begin
  RGBGradient.Clear;
  AlphaGradient.Clear;
end;

function TgmGradientItem.IsEmpty: Boolean;
begin
  Result := (RGBGradient.Count = 0) and (AlphaGradient.Count = 0);
end;

function TgmGradientItem.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
  
  if Assigned(Collection) then
    Result := TgmGradientCollection(Collection).BackgroundColor;
end;

function TgmGradientItem.GetForegroundColor: TColor;
begin
  Result := FForegroundColor;
  
  if Assigned(Collection) then
    Result := TgmGradientCollection(Collection).ForegroundColor;
end;

function TgmGradientItem.IsGroundColorStored: Boolean;
begin
  //if has not collection, then save
  Result := not Assigned(Collection);
end;

procedure TgmGradientItem.SpectrumChanged(ASender: TObject);
begin
  FSpectrumValid := False;
  
  if Assigned(FOnChange) then
    FOnChange(Self);
    
  Changed(False);
end;

procedure TgmGradientItem.DrawCachedBitmap(const AWidth, AHeight: Integer;
  const ADestBmp: TBitmap32; const ADestPoint: TPoint);
var
  LEdge                  : Integer;
  LStartPoint, LEndPoint : TPoint;
begin
  if not Assigned(FCachedBitmap) then
  begin
    FCachedBitmap          := TBitmap32.Create;
    FCachedBitmap.DrawMode := dmBlend;
  end;
  
  if (not FSpectrumValid) or
     (FCachedBitmap.Width <> AWidth) or
     (FCachedBitmap.Height <> AHeight) then
  begin
    FCachedBitmap.SetSize(AWidth, AHeight);

    LEdge       := Round( Sqrt(AWidth * AHeight) * 0.05 );
    LStartPoint := Point(LEdge, LEdge); //because zero based, start from 0

    LEndPoint   := Point(FCachedBitmap.Width - LEdge-1,
                         FCachedBitmap.Height - LEdge-1);

    DrawLinearGradient(FCachedBitmap, LStartPoint, LEndPoint, Self, False);
  end;

  with ADestPoint do
    ADestBmp.Draw(X, Y, FCachedBitmap);
end;

function TgmGradientItem.CachedBitmap(
  const AWidth, AHeight: Integer): TBitmap32;
var
  LEdge                  : Integer;
  LStartPoint, LEndPoint : TPoint;
  LGradientBmp           : TBitmap32;
begin
  if not Assigned(FCachedBitmap) then
  begin
    FCachedBitmap          := TBitmap32.Create;
    FCachedBitmap.DrawMode := dmBlend;
  end;

  if (not FSpectrumValid) or
     (FCachedBitmap.Width <> AWidth) or
     (FCachedBitmap.Height <> AHeight) then
  begin
    FCachedBitmap.SetSize(AWidth, AHeight);
    DrawCheckerboard(FCachedBitmap, FCachedBitmap.BoundsRect);

    LEdge       := Round( Sqrt(AWidth * AHeight) * 0.05 );
    LStartPoint := Point(LEdge, LEdge); //because zero based, start from 0

    LEndPoint   := Point(FCachedBitmap.Width - LEdge-1,
                         FCachedBitmap.Height - LEdge-1);

    LGradientBmp := TBitmap32.Create;
    try
      LGradientBmp.DrawMode := dmBlend;
      LGradientBmp.SetSizeFrom(FCachedBitmap);
      DrawLinearGradient(LGradientBmp, LStartPoint, LEndPoint, Self, False);
      FCachedBitmap.Draw(0, 0, LGradientBmp);
    finally
      LGradientBmp.Free;
    end;
  end;
  
  Result := FCachedBitmap;
end;

{ TgmGradientCollection }

constructor TgmGradientCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TgmGradientItem);
  
  FForegroundColor := clBlack;
  FBackgroundColor := clWhite;
end;

class function TgmGradientCollection.GetFileReaders: TgmFileFormatsList;
begin
  if not Assigned(UGradientReaders) then
  begin
    UGradientReaders := TgmFileFormatsList.Create;
  end;

  Result := UGradientReaders;
end;

class function TgmGradientCollection.GetFileWriters: TgmFileFormatsList;
begin
  if not Assigned(UGradientWriters) then
  begin
    UGradientWriters := TgmFileFormatsList.Create;
  end;

  Result := UGradientWriters;
end;

//here is the trick!
function TgmGradientCollection.Add: TgmGradientItem;
begin
  Result                 := TgmGradientItem(inherited Add);
  Result.ForegroundColor := Self.ForegroundColor;
  Result.BackgroundColor := Self.BackgroundColor;
end;

function TgmGradientCollection.Draw(const AGradientIndex: Integer;
  const ACanvas: TCanvas; const ARect: TRect): Boolean;
var
  LGradientBmp : TBitmap32;
  LRect        : TRect;
begin
  Result := False;

  if not Assigned(ACanvas) then
  begin
    raise Exception.Create('TgmGradientCollection.Draw() -- Error: Canvas is nil.');
  end;

  if GR32.IsRectEmpty(ARect) then
  begin
    raise Exception.Create('Rect is Empty');
  end;

  with ARect do
  begin
    LRect := MakeRect(0, 0, Right - Left, Bottom - Top);
  end;
  
  LGradientBmp := TBitmap32.Create;
  try
    LGradientBmp.SetSize(LRect.Right + 1,LRect.Bottom + 1);
    DrawCheckerboard(LGradientBmp,LGradientBmp.BoundsRect);

    DrawLinearGradient(LGradientBmp, LRect.TopLeft, LRect.BottomRight,
                       Items[AGradientIndex], False);

    LGradientBmp.DrawTo(ACanvas.Handle, ARect.Left, ARect.Top);
  finally
    LGradientBmp.Free;
  end;
end; 

function TgmGradientCollection.GetItem(Index: Integer): TgmGradientItem;
begin
  Result := TgmGradientItem(inherited GetItem(Index));
end;

procedure TgmGradientCollection.SetItem(AIndex: Integer;
  const AValue: TgmGradientItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TgmGradientCollection.SetForegroundColor(const AColor: TColor);
var
  i             : Integer;
  LGradientItem : TgmGradientItem;
begin
  if FForegroundColor <> AColor then
  begin
    FForegroundColor := AColor;

    if Self.Count > 0 then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LGradientItem := Items[i];
        LGradientItem.FForegroundColor := Self.FForegroundColor;
      end;
    end;
  end;
end;

procedure TgmGradientCollection.SetBackgroundColor(const AColor: TColor);
var
  i             : Integer;
  LGradientItem : TgmGradientItem;
begin
  if FBackgroundColor <> AColor then
  begin
    FBackgroundColor := AColor;

    if Self.Count > 0 then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LGradientItem := Items[i];
        LGradientItem.FBackgroundColor := Self.FBackgroundColor;
      end;
    end;
  end;
end;

function TgmGradientCollection.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex > -1) and (AIndex < Count);
end;

procedure TgmGradientCollection.Assign(ASource: TPersistent);
begin
  if ASource is TgmGradientCollection then
  begin
    FForegroundColor := TgmGradientCollection(ASource).ForegroundColor;
    FBackgroundColor := TgmGradientCollection(ASource).BackgroundColor;
  end;
  
  inherited; //may error, so put at end
end;

procedure TgmGradientCollection.SaveToStream(const AStream: TStream);
var
  LWriter : TgmGrdVer1_Writer;
begin
  if Assigned(AStream) then
  begin
    LWriter := TgmGrdVer1_Writer.Create;
    try
      LWriter.SaveToStream(AStream, Self);
    finally
      LWriter.Free;
    end;
  end;
end;

end.

