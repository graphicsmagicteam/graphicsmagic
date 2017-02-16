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

// Update Date: 2017/01/23

interface

uses
{ Standard }
  Contnrs, Classes,
{ Graphics32 }
  GR32;

//-- .grd files ----------------------------------------------------------------

const
  GRADIENT_FILE_ID      = $474D4347; // i.e. GMCG - GraphicsMagic Color Gradient
  GRADIENT_FILE_VERSION = 1;         // the file version we could process so far

  // for load in old version of GraphicsMagic's gradient files
  OLD_GRADIENT_ID   = 'GMGradientFile';
  OLD_GRADIENT_MODE = 'Average';

type
  TgmGradientFileHeader = record
    FileID       : Cardinal;  // must be $474D4347
    FileVersion  : Cardinal;  // version of the file format
    GradientCount: Cardinal;  // indicating how many gradients are in this file
  end;

  TgmGradientInfoHeader = record
    Name          : ShortString;  // name of the gradients
    ColorStopCount: Cardinal;     // how many primary colors are in the file
    AlphaStopCount: Cardinal;     // how many primary alpha values are in the file
  end;

  // for load in old version of GraphicsMagic's gradient files
  TgmOldGradientFileHeader = record
    Info         : string[20];
    GradientCount: Integer;
    GradientMode : string[20];
  end;

  TgmOldGradientInfoHeader = record
    Name      : ShortString;
    ColorCount: Integer;
  end;

//------------------------------------------------------------------------------

  TgmGradientColorType = (gctNone = -1,
                          gctStaticColor,
                          gctDynamicForegroundColor,
                          gctDynamicBackgroundColor);

//-- TgmSimpleRGBGradient ------------------------------------------------------

  { This class is used to define a gradient between two colors. }
  TgmSimpleRGBGradient = class(TObject)
  private
    FStartColor        : TColor32;
    FMidColor          : TColor32;
    FEndColor          : TColor32;
    FStartLocationScale: Single;           // scale of location at the total steps
    FMidLocationScale  : Single;
    FEndLocationScale  : Single;          
    FTotalSteps        : Integer;
    FGradientSteps     : Integer;          // length of steps to gradient
    FGradientColorArray: TArrayOfColor32;  // the length of this array is FGradientSteps

    FStartColorType    : TgmGradientColorType;
    FEndColorType      : TgmGradientColorType;
    FForegroundColor   : TColor32;
    FBackgroundColor   : TColor32;

    procedure FillGradientColorArray;
    procedure SetStartColor(const AColor: TColor32);
    procedure SetEndColor(const AColor: TColor32);
    procedure SetStartLocationScale(const AScale: Single);
    procedure SetEndLocationScale(const AScale: Single);
    procedure SetMidLocationScale(const AScale: Single);

    procedure SetForegroundColor(const AColor: TColor32);
    procedure SetBackgroundColor(const AColor: TColor32);
    procedure SetStartColorType(const AType: TgmGradientColorType);
    procedure SetEndColorType(const AType: TgmGradientColorType);

    function GetMidColor: TColor32;
    function GetGradientSteps: Integer;
    function GetStartColorLocationAtTotalSteps: Integer;
    function GetEndColorLocationAtTotalSteps: Integer;
    function GetMidLocationAtTotalSteps: Integer;
  public
    constructor Create(const AStartColor, AEndColor: TColor32;
      const AStartLocationScale, AEndLocationScale: Single);

    destructor Destroy; override;

    procedure UpdateGradientColorArray;

    function CopyGradientColors(const ADestArray: TArrayOfColor32;
      const AStartIndex: Integer): Integer;

    property StartColor          : TColor32             read FStartColor         write SetStartColor;
    property EndColor            : TColor32             read FEndColor           write SetEndColor;
    property StartLocationScale  : Single               read FStartLocationScale write SetStartLocationScale;
    property EndLocationScale    : Single               read FEndLocationScale   write SetEndLocationScale;
    property MidLocationScale    : Single               read FMidLocationScale   write SetMidLocationScale;
    property TotalSteps          : Integer              read FTotalSteps         write FTotalSteps;
    property GradientSteps       : Integer              read FGradientSteps;
    property StartLocAtTotalSteps: Integer              read GetStartColorLocationAtTotalSteps;
    property EndLocAtTotalSteps  : Integer              read GetEndColorLocationAtTotalSteps;
    property MidLocAtTotalSteps  : Integer              read GetMidLocationAtTotalSteps;
    property ForegroundColor     : TColor32             read FForegroundColor    write SetForegroundColor;
    property BackgroundColor     : TColor32             read FBackgroundColor    write SetBackgroundColor;
    property StartColorType      : TgmGradientColorType read FStartColorType     write SetStartColorType;
    property EndColorType        : TgmGradientColorType read FEndColorType       write SetEndColorType;
  end;

//-- TgmRGBGradient ------------------------------------------------------------

  { A TgmRGBGradient is consists of one or more TgmSimpleRGBGradient. }
  TgmRGBGradient = class(TPersistent)
  private
    FList              : TObjectList;  // used to store simple RGB gradient objects
    FGradientColorArray: TArrayOfColor32;
    FTotalGradientSteps: Integer;
    FForegroundColor   : TColor32;
    FBackgroundColor   : TColor32;

    procedure SetTotalGradientSteps(const ASteps: Integer);
    procedure SetForegroundColor(const AColor: TColor32);
    procedure SetBackgroundColor(const AColor: TColor32);

    function GetColorCount: Integer;
    function GetPrimaryColors(AIndex: Integer): TColor32;
    function GetPrimaryScales(AIndex: Integer): Single;
    function GetMidPointLocation(AColorIndex: Integer): Integer;
    function GetMidPointScale(AColorIndex: Integer): Single;
    function GetPrimaryColorTypes(AIndex: Integer): TgmGradientColorType;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear; virtual; // why in protected? because clear() just call by Assign().
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalcGradientColors;
    procedure AverageColorLocationScales;
    procedure SetToDefault;

    function ChangeColorType(const AColorIndex: Integer; const AType: TgmGradientColorType): Boolean;
    function ChangeColor(const AColorIndex: Integer; const AColor: TColor32): Boolean;
    function ChangeMidPointScale(const AColorIndex1, AColorIndex2: Integer; const AScale: Single): Boolean;
    function ChangeColorLocationScale(const AColorIndex: Integer; const AScale: Single): Integer;
    function InsertColor(const ALocationScale: Single; const AColor: TColor32): Integer;
    function DeleteColor(const AColorIndex: Integer): Boolean;
    function GetSimpleColorGradient(const AColorIndex: Integer): TgmSimpleRGBGradient;

    procedure DrawColorGradient(const ABmp: TBitmap32);

    property TotalGradientSteps               : Integer              read FTotalGradientSteps write SetTotalGradientSteps;
    property ColorCount                       : Integer              read GetColorCount;
    property PrimaryColorTypes[index: Integer]: TgmGradientColorType read GetPrimaryColorTypes;
    property PrimaryColors[index: Integer]    : TColor32             read GetPrimaryColors;
    property PrimaryScales[index: Integer]    : Single               read GetPrimaryScales;
    property MidPointLocation[index: Integer] : Integer              read GetMidPointLocation;
    property MidPointScale[index: Integer]    : Single               read GetMidPointScale;
    property ForegroundColor                  : TColor32             read FForegroundColor    write SetForegroundColor;
    property BackgroundColor                  : TColor32             read FBackgroundColor    write SetBackgroundColor;
  end;

//-- TgmSimpleAlphaGradient ----------------------------------------------------

  { This class is used to define a gradient between two alpha values. }
  TgmSimpleAlphaGradient = class(TObject)
  private
    FStartAlpha        : Byte;
    FMidAlpha          : Byte;
    FEndAlpha          : Byte;
    FStartLocationScale: Single;        // scale of location at the total steps
    FMidLocationScale  : Single;
    FEndLocationScale  : Single;
    FTotalSteps        : Integer;
    FGradientSteps     : Integer;       // length of steps to gradient
    FGradientAlphaArray: TArrayOfByte;  // the length of this array is FGradientSteps

    procedure FillGradientAlphaArray;
    procedure SetStartAlpha(const AValue: Byte);
    procedure SetEndAlpha(const AValue: Byte);
    procedure SetStartLocationScale(const AScale: Single);
    procedure SetEndLocationScale(const AScale: Single);
    procedure SetMidLocationScale(const AScale: Single);

    function GetMidAlpha: Byte;
    function GetGradientSteps: Integer;
    function GetStartAlphaLocationAtTotalSteps: Integer;
    function GetEndAlphaLocationAtTotalSteps: Integer;
    function GetMidLocationAtTotalSteps: Integer;
  public
    constructor Create(const AStartAlpha, AEndAlpha: Byte;
      const AStartLocationScale, AEndLocationScale: Single);

    destructor Destroy; override;

    procedure UpdateGradientAlphaArray;

    function CopyGradientAlphaValues(const ADestArray: TArrayOfByte;
      const AStartIndex: Integer): Integer;

    property StartAlpha          : Byte    read FStartAlpha         write SetStartAlpha;
    property EndAlpha            : Byte    read FEndAlpha           write SetEndAlpha;
    property StartLocationScale  : Single  read FStartLocationScale write SetStartLocationScale;
    property EndLocationScale    : Single  read FEndLocationScale   write SetEndLocationScale;
    property MidLocationScale    : Single  read FMidLocationScale   write SetMidLocationScale;
    property TotalSteps          : Integer read FTotalSteps         write FTotalSteps;
    property GradientSteps       : Integer read FGradientSteps;
    property StartLocAtTotalSteps: Integer read GetStartAlphaLocationAtTotalSteps;
    property EndLocAtTotalSteps  : Integer read GetEndAlphaLocationAtTotalSteps;
    property MidLocAtTotalSteps  : Integer read GetMidLocationAtTotalSteps;
  end;

//-- TgmAlphaGradient ----------------------------------------------------------

  { A TgmAlphaGradient is consists of one or more TgmSimpleAlphaGradient. }
  TgmAlphaGradient = class(TPersistent)
  private
    FList              : TObjectList;  // used to store simple alpha gradient objects
    FGradientAlphaArray: TArrayOfByte;
    FTotalGradientSteps: Integer;

    procedure SetTotalGradientSteps(const ASteps: Integer);

    function GetAlphaValueCount: Integer;
    function GetPrimaryAlphaValues(AIndex: Integer): Byte;
    function GetPrimaryScales(AIndex: Integer): Single;
    function GetMidPointLocation(AIndex: Integer): Integer;
    function GetMidPointScale(AIndex: Integer): Single;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear; virtual; // why in protected? because clear() just call by Assign().
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalcGradientAlphaValues;
    procedure AverageAlphaLocationScales;
    procedure SetToDefault;

    function ChangeAlphaValue(const AIndex: Integer; const AValue: Byte): Boolean;
    function ChangeMidPointScale(const AAlphaIndex1, AAlphaIndex2: Integer; const AScale: Single): Boolean;
    function ChangeAlphaLocationScale(const AAlphaIndex: Integer; const AScale: Single): Integer;
    function InsertAlpha(const ALocationScale: Single; const AAlphaValue: Byte): Integer;
    function DeleteAlpha(const AAlphaIndex: Integer): Boolean;
    function GetSimpleAlphaGradient(const AAlphaIndex: Integer): TgmSimpleAlphaGradient;

    property TotalGradientSteps                : Integer read FTotalGradientSteps write SetTotalGradientSteps;
    property AlphaValueCount                   : Integer read GetAlphaValueCount;
    property PrimaryAlphaValues[index: Integer]: Byte    read GetPrimaryAlphaValues;
    property PrimaryScales[index: Integer]     : Single  read GetPrimaryScales;
    property MidPointLocation[index: Integer]  : Integer read GetMidPointLocation;
    property MidPointScale[index: Integer]     : Single  read GetMidPointScale;
  end;

//-- TgmColorGradient ----------------------------------------------------------

  { A TgmColorGradient contains a TgmRGBGradient and a TgmAlphaGradient. }
  TgmColorGradient = class(TCollectionItem)
  private
    FAlphaGradient   : TgmAlphaGradient;
    FRGBGradient     : TgmRGBGradient;
    FOutputColorArray: TArrayOfColor32;
    FGradientLength  : Integer;
    FSelected        : Boolean;
    FName            : string;
    FDisplayName     : string;

    procedure SetGradientLength(const ALength: Integer);
    procedure SetName(const AName: string);

    function GetAbsolutStartColor: TColor32;
    function GetAbsolutEndColor  : TColor32;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;

    function GetDisplayName: string; override;
    
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create; reintroduce; overload;

    destructor Destroy; override;

    procedure RefreshColorArray;
    procedure DrawColorGradients(const ABmp: TBitmap32);
    procedure SaveToStream(const AStream: TStream);

    function LoadFromStream(const AStream: TStream): Boolean;
    function LoadOldGradientsFromStream(const AStream: TStream): Boolean;

    property AbsolutStartColor: TColor32         read GetAbsolutStartColor;
    property AbsolutEndColor  : TColor32         read GetAbsolutEndColor;
    property OutputColors     : TArrayOfColor32  read FOutputColorArray;
    property AlphaGradient    : TgmAlphaGradient read FAlphaGradient;
    property RGBGradient      : TgmRGBGradient   read FRGBGradient;
    property GradientLength   : Integer          read FGradientLength write SetGradientLength;
    property Name             : string           read FName           write SetName;
    property IsSelected       : Boolean          read FSelected       write FSelected;
  end;

//-- TgmGradientCollection -----------------------------------------------------

  TgmGradientCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure SetItem(Index: Integer; const Value: TgmColorGradient);
    function GetItem(Index: Integer): TgmColorGradient;
    
    function LoadGradientsVersion1(const AStream: TStream;
      const ALoadCount: Integer): Boolean;

    function LoadGradientsOldVersion(const AStream: TStream;
      const ALoadCount: Integer): Boolean;

  protected
    procedure Update(Item: TCollectionItem); override;
    
  public
    constructor Create(AOwner:TComponent); virtual;

    function Add: TgmColorGradient;

    function LoadFromFile(const AFileName: string): Boolean; virtual;
    function LoadFromStream(const AStream: TStream): Boolean; virtual;
    
    procedure SaveToFile(const AFileName: string); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;

    property OnChange             : TNotifyEvent     read FOnChange write FOnChange;
    property Items[Index: Integer]: TgmColorGradient read GetItem   write SetItem; default;
  end;
  

implementation

uses
{ Standard }
  SysUtils, Graphics,
{ Graphics32 }
  GR32_LowLevel;

function GetGradientColorArray(const AStartColor, AEndColor: TColor32;
  const AGradientLength: Integer): TArrayOfColor32;
var
  i         : Integer;
  r, g, b   : Byte;
  sr, sg, sb: Byte;
  er, eg, eb: Byte;
  ir, ig, ib: Single;
  LRedInc   : Single;
  LGreenInc : Single;
  LBlueInc  : Single;
  LStepScale: Single;
begin
  Result := nil;

  if AGradientLength > 0 then
  begin
    SetLength(Result, AGradientLength);

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

    Result[0] := AStartColor;

    ir := sr;
    ig := sg;
    ib := sb;

    for i := 1 to (AGradientLength - 1) do
    begin
      ir := ir + LRedInc;
      ig := ig + LGreenInc;
      ib := ib + LBlueInc;

      r := Round(ir);
      g := Round(ig);
      b := Round(ib);
      
      r := Clamp(r);
      g := Clamp(g);
      b := Clamp(b);
      
      Result[i] := $FF000000 or (r shl 16) or (g shl 8) or b;
    end;
  end;
end; { GetGradientColorArray }

function GetGradientAlphaArray(const AStartAlpha, AEndAlpha: Byte;
  const AGradientLength: Integer): TArrayOfByte;
var
  i          : Integer;
  LAlpha     : Integer;
  LAccumAlpha: Single;
  LIncValue  : Single;
  LStepScale : Single;
begin
  Result := nil;

  if AGradientLength > 0 then
  begin
    SetLength(Result, AGradientLength);

    LStepScale := 1 / AGradientLength;
    LIncValue  := (AEndAlpha - AStartAlpha) * LStepScale;

    Result[0]   := AStartAlpha;
    LAccumAlpha := AStartAlpha;

    for i := 1 to (AGradientLength - 1) do
    begin
      LAccumAlpha := LAccumAlpha + LIncValue;
      LAlpha      := Round(LAccumAlpha);
      Result[i]   := Clamp(LAlpha);
    end;
  end;
end; { GetGradientAlphaArray }

//-- TgmSimpleRGBGradient ------------------------------------------------------

constructor TgmSimpleRGBGradient.Create(const AStartColor, AEndColor: TColor32;
  const AStartLocationScale, AEndLocationScale: Single);
begin
  inherited Create;

  FStartColor         := AStartColor;
  FEndColor           := AEndColor;
  FStartLocationScale := AStartLocationScale;
  FMidLocationScale   := 0.5;
  FEndLocationScale   := AEndLocationScale;
  FTotalSteps         := 0;
  FGradientColorArray := nil;
  FMidColor           := GetMidColor;

  FStartColorType     := gctStaticColor;
  FEndColorType       := gctStaticColor;
  FForegroundColor    := clBlack32;
  FBackgroundColor    := clWhite32;
end; { Create }

destructor TgmSimpleRGBGradient.Destroy;
begin
  SetLength(FGradientColorArray, 0);
  FGradientColorArray := nil;

  inherited Destroy;
end; { Destroy }

procedure TgmSimpleRGBGradient.FillGradientColorArray;
var
  i, LLength : Integer;
  LMidLength : Integer;
  LColorArray: TArrayOfColor32;
begin
  LMidLength := Round(FGradientSteps * FMidLocationScale);

  // get first part of gradient colors ...
  LColorArray := GetGradientColorArray(FStartColor, FMidColor, LMidLength);

  if Length(LColorArray) > 0 then
  begin
    for i := 0 to (LMidLength - 1) do
    begin
      FGradientColorArray[i] := LColorArray[i];
    end;

    SetLength(LColorArray, 0);
    LColorArray := nil;
  end;

  // get second part of gradient colors ...
  LLength     := FGradientSteps - LMidLength;
  LColorArray := GetGradientColorArray(FMidColor, FEndColor, LLength);

  if Length(LColorArray) > 0 then
  begin
    for i := 0 to (LLength - 1) do
    begin
      FGradientColorArray[i + LMidLength] := LColorArray[i];
    end;

    SetLength(LColorArray, 0);
    LColorArray := nil;
  end;
end; { FillGradientColorArray }

procedure TgmSimpleRGBGradient.SetStartColor(const AColor: TColor32);
begin
  if FStartColor <> AColor then
  begin
    FStartColor := AColor;
    FMidColor   := GetMidColor;
  end;
end; { SetStartColor }

procedure TgmSimpleRGBGradient.SetEndColor(const AColor: TColor32);
begin
  if FEndColor <> AColor then
  begin
    FEndColor := AColor;
    FMidColor := GetMidColor;
  end;
end; { SetEndColor }

procedure TgmSimpleRGBGradient.SetStartLocationScale(const AScale: Single);
begin
  if (AScale >= 0.0) and (AScale <= 1.0) then
  begin
    if FStartLocationScale <> AScale then
    begin
      FStartLocationScale := AScale;
      FMidColor           := GetMidColor;
      FGradientSteps      := GetGradientSteps;
    end;
  end;
end; { SetStartLocationScale }

procedure TgmSimpleRGBGradient.SetEndLocationScale(const AScale: Single);
begin
  if (AScale >= 0.0) and (AScale <= 1.0) then
  begin
    if FEndLocationScale <> AScale then
    begin
      FEndLocationScale := AScale;
      FMidColor         := GetMidColor;
      FGradientSteps    := GetGradientSteps;
    end;
  end;
end; { SetEndLocationScale }

procedure TgmSimpleRGBGradient.SetMidLocationScale(const AScale: Single);
begin
  if (AScale >= 0.05) and (AScale <= 0.95) then
  begin
    if FMidLocationScale <> AScale then
    begin
      FMidLocationScale := AScale;
      FMidColor         := GetMidColor;
    end;
  end;
end; { SetMidLocationScale }

procedure TgmSimpleRGBGradient.SetForegroundColor(const AColor: TColor32);
var
  LNeedUpdateMidColor: Boolean;
begin
  LNeedUpdateMidColor := False;
  FForegroundColor    := AColor;
  
  if FStartColorType = gctDynamicForegroundColor then
  begin
    if FStartColor <> FForegroundColor then
    begin
      FStartColor         := FForegroundColor;
      LNeedUpdateMidColor := True;
    end;
  end;

  if FEndColorType = gctDynamicForegroundColor then
  begin
    if FEndColor <> FForegroundColor then
    begin
      FEndColor           := FForegroundColor;
      LNeedUpdateMidColor := True;
    end;
  end;

  if LNeedUpdateMidColor then
  begin
    FMidColor := GetMidColor;
  end;
end; { SetForegroundColor }

procedure TgmSimpleRGBGradient.SetBackgroundColor(const AColor: TColor32);
var
  LNeedUpdateMidColor: Boolean;
begin
  LNeedUpdateMidColor := False;
  FBackgroundColor    := AColor;
  
  if FStartColorType = gctDynamicBackgroundColor then
  begin
    if FStartColor <> FBackgroundColor then
    begin
      FStartColor         := FBackgroundColor;
      LNeedUpdateMidColor := True;
    end;
  end;

  if FEndColorType = gctDynamicBackgroundColor then
  begin
    if FEndColor <> FBackgroundColor then
    begin
      FEndColor           := FBackgroundColor;
      LNeedUpdateMidColor := True;
    end;
  end;

  if LNeedUpdateMidColor then
  begin
    FMidColor := GetMidColor;
  end;
end; { SetBackgroundColor }

procedure TgmSimpleRGBGradient.SetStartColorType(
  const AType: TgmGradientColorType);
begin
  if FStartColorType <> AType then
  begin
    FStartColorType := AType;

    if FStartColorType = gctDynamicForegroundColor then
    begin
      FStartColor := FForegroundColor;
      FMidColor   := GetMidColor;
    end
    else
    if FStartColorType = gctDynamicBackgroundColor then
    begin
      FStartColor := FBackgroundColor;
      FMidColor   := GetMidColor;
    end;
  end;
end; { SetStartColorType }

procedure TgmSimpleRGBGradient.SetEndColorType(
  const AType: TgmGradientColorType);
begin
  if FEndColorType <> AType then
  begin
    FEndColorType := AType;

    if FEndColorType = gctDynamicForegroundColor then
    begin
      FEndColor := FForegroundColor;
      FMidColor := GetMidColor;
    end
    else
    if FEndColorType = gctDynamicBackgroundColor then
    begin
      FEndColor := FBackgroundColor;
      FMidColor := GetMidColor;
    end;
  end;
end; { SetEndColorType }

function TgmSimpleRGBGradient.GetMidColor: TColor32;
var
  sr, sg, sb: Byte;
  er, eg, eb: Byte;
  r, g, b   : Cardinal;
begin
  sr := FStartColor shr 16 and $FF;
  sg := FStartColor shr  8 and $FF;
  sb := FStartColor        and $FF;

  er := FEndColor shr 16 and $FF;
  eg := FEndColor shr  8 and $FF;
  eb := FEndColor        and $FF;

  r := Round( sr * FMidLocationScale + er * (1.0 - FMidLocationScale) );
  g := Round( sg * FMidLocationScale + eg * (1.0 - FMidLocationScale) );
  b := Round( sb * FMidLocationScale + eb * (1.0 - FMidLocationScale) );

  r := Clamp(r);
  g := Clamp(g);
  b := Clamp(b);

  Result := $FF000000 or (r shl 16) or (g shl 8) or b;
end; { GetMidColor }

function TgmSimpleRGBGradient.GetGradientSteps: Integer;
begin
  Result := 0;

  if (FTotalSteps > 0) and
     (FEndLocationScale >= FStartLocationScale) then
  begin
    Result := Round( (FEndLocationScale - FStartLocationScale) * FTotalSteps );
  end;
end; { GetGradientSteps }

{ Resturns the actual horizontal location of the start color at the length of
  the total steps. }
function TgmSimpleRGBGradient.GetStartColorLocationAtTotalSteps: Integer;
begin
  Result := Round(FStartLocationScale * FTotalSteps);
end; { GetStartColorLocationAtTotalSteps }

{ Resturns the actual horizontal location of the end color at the length of
  the total steps. }
function TgmSimpleRGBGradient.GetEndColorLocationAtTotalSteps: Integer;
begin
  Result := Round(FEndLocationScale * FTotalSteps);
end; { GetEndColorLocationAtTotalSteps }

{ Resturns the actual horizontal location of the mid point at the length of
  the total steps. }
function TgmSimpleRGBGradient.GetMidLocationAtTotalSteps: Integer;
begin
  Result := Round(FStartLocationScale * FTotalSteps +
                  FMidLocationScale * FGradientSteps);
end; { GetMidLocationAtTotalSteps }

procedure TgmSimpleRGBGradient.UpdateGradientColorArray;
begin
  FGradientSteps := GetGradientSteps;

  if FGradientSteps > 0 then
  begin
    SetLength(FGradientColorArray, FGradientSteps);
    FillGradientColorArray;
  end
  else
  begin
    SetLength(FGradientColorArray, 0);
    FGradientColorArray := nil;
  end;
end; { UpdateGradientColorArray }

{ Copy all of gradient colors to the dest array.
  @AStartIndex - indicates from where the copy begins at the dest array.
  The Result value is the number of actually copies. }
function TgmSimpleRGBGradient.CopyGradientColors(
  const ADestArray: TArrayOfColor32; const AStartIndex: Integer): Integer;
var
  LDestHighIndex: Integer;
  LCopyLength   : Integer;
  i             : Integer;
begin
  Result := 0;

  if (FGradientSteps > 0) and (AStartIndex >= 0) then
  begin
    LDestHighIndex := High(ADestArray);
    LCopyLength    := LDestHighIndex - AStartIndex + 1;

    if LCopyLength > FGradientSteps then
    begin
      LCopyLength := FGradientSteps;
    end;

    for i := 0 to (LCopyLength - 1) do
    begin
      ADestArray[AStartIndex + i] := FGradientColorArray[i];
    end;

    Result := LCopyLength;
  end;
end; { CopyGradientColors }

//-- TgmRGBGradient ------------------------------------------------------------

constructor TgmRGBGradient.Create;
var
  LRGBGradient: TgmSimpleRGBGradient;
begin
  inherited Create;

  FList := TObjectList.Create;

  // add a default simple RGB gradient to the list
  LRGBGradient := TgmSimpleRGBGradient.Create($FF000000, $FFFFFFFF, 0.0, 1.0);
  FList.Add(LRGBGradient);

  FGradientColorArray := nil;
  FTotalGradientSteps := 0;

  FForegroundColor := clBlack32;
  FBackgroundColor := clWhite32;
end; { Create }

destructor TgmRGBGradient.Destroy;
begin
  if Length(FGradientColorArray) > 0 then
  begin
    SetLength(FGradientColorArray, 0);
    FGradientColorArray := nil;
  end;

  FList.Clear;
  FList.Free;
  
  inherited Destroy;
end; { Destroy }

procedure TgmRGBGradient.SetTotalGradientSteps(const ASteps: Integer);
var
  i           : Integer;
  LRGBGradient: TgmSimpleRGBGradient;
begin
  if ASteps > 0 then
  begin
    FTotalGradientSteps := ASteps;

    if FList.Count > 0 then
    begin
      for i := 0 to (FList.Count - 1) do
      begin
        LRGBGradient            := TgmSimpleRGBGradient(FList.Items[i]);
        LRGBGradient.TotalSteps := FTotalGradientSteps;
      end;
    end;
  end;
end; { SetTotalGradientSteps }

procedure TgmRGBGradient.SetForegroundColor(const AColor: TColor32);
var
  i                 : Integer;
  LSimpleRGBGradient: TgmSimpleRGBGradient;
begin
  FForegroundColor := AColor;

  if FList.Count > 0 then
  begin
    for i := 0 to (FList.Count - 1) do
    begin
      LSimpleRGBGradient := TgmSimpleRGBGradient(FList.Items[i]);
      
      LSimpleRGBGradient.ForegroundColor := FForegroundColor;
    end;
  end;
end; { SetForegroundColor }

procedure TgmRGBGradient.SetBackgroundColor(const AColor: TColor32);
var
  i                 : Integer;
  LSimpleRGBGradient: TgmSimpleRGBGradient;
begin
  FBackgroundColor := AColor;

  if FList.Count > 0 then
  begin
    for i := 0 to (FList.Count - 1) do
    begin
      LSimpleRGBGradient := TgmSimpleRGBGradient(FList.Items[i]);

      LSimpleRGBGradient.BackgroundColor := FBackgroundColor;
    end;
  end;
end; { SetBackgroundColor }

function TgmRGBGradient.GetColorCount: Integer;
begin
  Result := FList.Count + 1;
end; { GetColorCount }

function TgmRGBGradient.GetPrimaryColors(AIndex: Integer): TColor32;
var
  LMaxColorIndex: Integer;
  LRGBGradient  : TgmSimpleRGBGradient;
begin
  Result         := $00000000;
  LMaxColorIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxColorIndex) then
  begin
    if AIndex < LMaxColorIndex then
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[AIndex]);
      Result       := LRGBGradient.StartColor;
    end
    else
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
      Result       := LRGBGradient.EndColor;
    end;
  end;
end; { GetPrimaryColors }

function TgmRGBGradient.GetPrimaryScales(AIndex: Integer): Single;
var
  LMaxColorIndex: Integer;
  LRGBGradient  : TgmSimpleRGBGradient;
begin
  Result         := 0.0;
  LMaxColorIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxColorIndex) then
  begin
    if AIndex < LMaxColorIndex then
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[AIndex]);
      Result       := LRGBGradient.StartLocationScale;
    end
    else
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
      Result       := LRGBGradient.EndLocationScale;
    end;
  end;
end; { GetPrimaryScales }

function TgmRGBGradient.GetMidPointLocation(AColorIndex: Integer): Integer;
var
  LMaxColorIndex: Integer;
  LRGBGradient  : TgmSimpleRGBGradient;
begin
  Result         := -1;
  LMaxColorIndex := FList.Count;

  if (AColorIndex >= 0) and (AColorIndex <= LMaxColorIndex) then
  begin
    if AColorIndex < LMaxColorIndex then
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
      Result       := LRGBGradient.MidLocAtTotalSteps;
    end
    else
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
      Result       := LRGBGradient.MidLocAtTotalSteps;
    end;
  end;
end; { GetMidPointLocation }

function TgmRGBGradient.GetMidPointScale(AColorIndex: Integer): Single;
var
  LMaxColorIndex: Integer;
  LRGBGradient  : TgmSimpleRGBGradient;
begin
  Result         := -1;
  LMaxColorIndex := FList.Count;

  if (AColorIndex >= 0) and (AColorIndex <= LMaxColorIndex) then
  begin
    if AColorIndex < LMaxColorIndex then
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
      Result       := LRGBGradient.MidLocationScale;
    end
    else
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
      Result       := LRGBGradient.MidLocationScale;
    end;
  end;
end; { GetMidPointScale }

function TgmRGBGradient.GetPrimaryColorTypes(
  AIndex: Integer): TgmGradientColorType;
var
  LMaxColorIndex    : Integer;
  LSimpleRGBGradient: TgmSimpleRGBGradient;
begin
  Result         := gctNone;
  LMaxColorIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxColorIndex) then
  begin
    if AIndex < LMaxColorIndex then
    begin
      LSimpleRGBGradient := TgmSimpleRGBGradient(FList.Items[AIndex]);
      Result             := LSimpleRGBGradient.StartColorType;
    end
    else
    begin
      LSimpleRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
      Result             := LSimpleRGBGradient.EndColorType;
    end;
  end;
end; { GetPrimaryColorTypes }

procedure TgmRGBGradient.AssignTo(Dest: TPersistent);
var
  i         : Integer;
  gmDest    : TgmRGBGradient;
  tempSimple: TgmSimpleRGBGradient;
  newSimple : TgmSimpleRGBGradient;
begin
  if Dest is TgmRGBGradient then
  begin
    gmDest := Dest as TgmRGBGradient;
    gmDest.Clear;
    
    for i := 0 to (Self.FList.Count -1) do
    begin
      tempSimple := Self.FList[i] as TgmSimpleRGBGradient;

      with tempSimple do
      begin
        newSimple := TgmSimpleRGBGradient.Create(StartColor,
                                                 EndColor,
                                                 StartLocationScale,
                                                 EndLocationScale);

        newSimple.StartColorType  := StartColorType;
        newSimple.EndColorType    := EndColorType;
        newSimple.ForegroundColor := ForegroundColor;
        newSimple.BackgroundColor := BackgroundColor;

        gmDest.FList.Add(newSimple);
      end;
    end;
  end
  else
  begin
    inherited; // assign error
  end;
end; { AssignTo }

procedure TgmRGBGradient.Clear;
begin
  FList.Clear;
end; { Clear }

function TgmRGBGradient.GetSimpleColorGradient(
  const AColorIndex: Integer): TgmSimpleRGBGradient;
var
  LMaxColorIndex: Integer;
begin
  Result         := nil;
  LMaxColorIndex := FList.Count;

  if (AColorIndex >= 0) and (AColorIndex <= LMaxColorIndex) then
  begin
    if AColorIndex < LMaxColorIndex then
    begin
      Result := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
    end
    else
    begin
      Result := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);
    end;
  end;
end; { GetSimpleColorGradient }

procedure TgmRGBGradient.CalcGradientColors;
var
  i, j, LIndex: Integer;
  LCopyCount  : Integer;
  LRGBGradient: TgmSimpleRGBGradient;
begin
  if FTotalGradientSteps > 0 then
  begin
    SetLength(FGradientColorArray, FTotalGradientSteps);

    if FList.Count > 0 then
    begin
      LIndex := 0;

      for i := 0 to (FList.Count - 1) do
      begin
        LRGBGradient := TgmSimpleRGBGradient(FList.Items[i]);
        LRGBGradient.UpdateGradientColorArray;

        // process the part that before the first color...
        if i = 0 then
        begin
          if LRGBGradient.StartLocationScale > 0.0 then
          begin
            LCopyCount := Round(LRGBGradient.StartLocationScale * FTotalGradientSteps);

            for j := 0 to (LCopyCount - 1) do
            begin
              FGradientColorArray[j] := LRGBGradient.StartColor;
            end;

            Inc(LIndex, LCopyCount);
          end;
        end;

        LCopyCount := LRGBGradient.CopyGradientColors(FGradientColorArray, LIndex);
        Inc(LIndex, LCopyCount);

        // process the part that after the last color
        if ( i = (FList.Count - 1) ) and
           ( LIndex < FTotalGradientSteps )then
        begin
          { Note that, perhaps due to the floating error in the calculations,
            the total entries of all the sub gradient array is always less
            then FTotalGradientSteps by one. So in the condition above,
            we check if (LIndex < FTotalGradientSteps) other than check for
            (LIndex < (FTotalGradientSteps-1). }

          for j := LIndex to (FTotalGradientSteps - 1) do
          begin
            FGradientColorArray[j] := LRGBGradient.EndColor;
          end;
        end;
      end;
    end;
  end;
end; { CalcGradientColors }

// make colors be distributed averagely
procedure TgmRGBGradient.AverageColorLocationScales;
var
  i            : Integer;
  LScale       : Single;
  LCurrGradient: TgmSimpleRGBGradient;
  LPrevGradient: TgmSimpleRGBGradient;
begin
  LScale := 1 / FList.Count;

  for i := 0 to (FList.Count - 1) do
  begin
    LCurrGradient := TgmSimpleRGBGradient(FList.Items[i]);

    if i = 0 then
    begin
      LCurrGradient.StartLocationScale := 0.0;
    end
    else
    begin
      LPrevGradient := TgmSimpleRGBGradient(FList.Items[i - 1]);

      LCurrGradient.StartLocationScale := LPrevGradient.EndLocationScale;
    end;

    LCurrGradient.EndLocationScale := LScale * (i + 1);
    LCurrGradient.MidLocationScale := 0.5;
  end;
end; { AverageColorLocationScales }

// set to the default state
procedure TgmRGBGradient.SetToDefault;
var
  i                 : Integer;
  LSimpleRGBGradient: TgmSimpleRGBGradient;
begin
  if FList.Count > 1 then
  begin
    for i := (FList.Count - 1) downto 1 do
    begin
      FList.Delete(i);

      LSimpleRGBGradient := TgmSimpleRGBGradient(FList.Items[0]);
      LSimpleRGBGradient.StartColor         := clBlack32;
      LSimpleRGBGradient.StartLocationScale := 0.0;
      LSimpleRGBGradient.EndColor           := clWhite32;
      LSimpleRGBGradient.EndLocationScale   := 1.0;
      LSimpleRGBGradient.MidLocationScale   := 0.5;
    end;
  end;
end; { SetToDefault }

function TgmRGBGradient.ChangeColorType(const AColorIndex: Integer;
  const AType: TgmGradientColorType): Boolean;
var
  LRGBGradient: TgmSimpleRGBGradient;
begin
  Result := False;

  if (AColorIndex >= 0) and (FList.Count > 0) then
  begin
    if AColorIndex = 0 then
    begin
      LRGBGradient := TgmSimpleRGBGradient(FList.Items[0]);
      LRGBGradient.StartColorType := AType;
    end
    else
    begin
      if AColorIndex <= (FList.Count) then
      begin
        LRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex - 1]);
        LRGBGradient.EndColorType := AType;

        if AColorIndex < (FList.Count) then
        begin
          LRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
          LRGBGradient.StartColorType := AType;
        end;
      end;
    end;

    Result := True;
  end;
end; { ChangeColorType }

{ Change a color in this gradient.
  @AColorIndex - which one in the gradient you want to change.
  @AColor - what color to be changed. }
function TgmRGBGradient.ChangeColor(const AColorIndex: Integer;
  const AColor: TColor32): Boolean;
var
  LRGBGradient: TgmSimpleRGBGradient;
begin
  Result := False;

  if GetPrimaryColorTypes(AColorIndex) <> gctStaticColor then
  begin
    Exit;
  end;

  if (AColorIndex >= 0) and (FList.Count > 0) then
  begin
    if AColorIndex = 0 then
    begin
      LRGBGradient            := TgmSimpleRGBGradient(FList.Items[0]);
      LRGBGradient.StartColor := AColor;
    end
    else
    begin
      if AColorIndex <= (FList.Count) then
      begin
        LRGBGradient          := TgmSimpleRGBGradient(FList.Items[AColorIndex - 1]);
        LRGBGradient.EndColor := AColor;

        if AColorIndex < (FList.Count) then
        begin
          LRGBGradient            := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
          LRGBGradient.StartColor := AColor;
        end;
      end;
    end;

    Result := True;
  end;
end; { ChangeColor }

{ Return the new index of the color. }
function TgmRGBGradient.ChangeColorLocationScale(const AColorIndex: Integer;
  const AScale: Single): Integer;
var
  LLeftRGBGradient : TgmSimpleRGBGradient;
  LRightRGBGradient: TgmSimpleRGBGradient;
  LRGBGradient     : TgmSimpleRGBGradient;
  LMaxColorIndex   : Integer;
  LNewColorIndex   : Integer;
  LColor           : TColor32;
  LColorType       : TgmGradientColorType;
  LLeftMidScale    : Single;
  LRightMidScale   : Single;
begin
  Result := -1;

  if FList.Count = 1 then
  begin
    LRGBGradient := TgmSimpleRGBGradient(FList.Items[0]);

    if AColorIndex = 0 then
    begin
      if AScale > LRGBGradient.EndLocationScale then
      begin
        LColor                          := LRGBGradient.StartColor;
        LRGBGradient.StartColor         := LRGBGradient.EndColor;
        LRGBGradient.StartLocationScale := LRGBGradient.EndLocationScale;
        LRGBGradient.EndColor           := LColor;
        LRGBGradient.EndLocationScale   := AScale;
        LRGBGradient.MidLocationScale   := 1.0 - LRGBGradient.MidLocationScale;

        LColorType                  := LRGBGradient.StartColorType;
        LRGBGradient.StartColorType := LRGBGradient.EndColorType;
        LRGBGradient.EndColorType   := LColorType;

        Result := 1;
      end
      else
      begin
        LRGBGradient.StartLocationScale := AScale;
        Result := AColorIndex;
      end;
    end
    else
    if AColorIndex = 1 then
    begin
      if AScale < LRGBGradient.StartLocationScale then
      begin
        LColor                          := LRGBGradient.EndColor;
        LRGBGradient.EndColor           := LRGBGradient.StartColor;
        LRGBGradient.EndLocationScale   := LRGBGradient.StartLocationScale;
        LRGBGradient.StartColor         := LColor;
        LRGBGradient.StartLocationScale := AScale;
        LRGBGradient.MidLocationScale   := 1.0 - LRGBGradient.MidLocationScale;

        LColorType                  := LRGBGradient.EndColorType;
        LRGBGradient.EndColorType   := LRGBGradient.StartColorType;
        LRGBGradient.StartColorType := LColorType;

        Result := 0;
      end
      else
      begin
        LRGBGradient.EndLocationScale := AScale;
        Result := AColorIndex;
      end;
    end;
  end
  else
  begin
    LMaxColorIndex := FList.Count;
    LNewColorIndex := AColorIndex;

    if (AColorIndex >= 0) and (AColorIndex <= LMaxColorIndex) then
    begin
      if AColorIndex = 0 then
      begin
        LRGBGradient := TgmSimpleRGBGradient(FList.Items[0]);

        if AScale > LRGBGradient.EndLocationScale then
        begin
          LColor         := LRGBGradient.StartColor;
          LColorType     := LRGBGradient.StartColorType;
          LRightMidScale := LRGBGradient.MidLocationScale;

          Self.DeleteColor(0);

          LNewColorIndex := Self.InsertColor(AScale, LColor);

          // process color type
          ChangeColorType(LNewColorIndex, LColorType);

          // process mid point scale
          if LNewColorIndex < Self.ColorCount then
          begin
            LRGBGradient := TgmSimpleRGBGradient(FList.Items[LNewColorIndex]);
            LRGBGradient.MidLocationScale := LRightMidScale;
          end;
        end
        else
        begin
          LRGBGradient.StartLocationScale := AScale;
        end;
      end
      else
      if AColorIndex = LMaxColorIndex then
      begin
        LRGBGradient := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);

        if AScale < LRGBGradient.StartLocationScale then
        begin
          LColor        := LRGBGradient.EndColor;
          LColorType    := LRGBGradient.EndColorType;
          LLeftMidScale := LRGBGradient.MidLocationScale;

          Self.DeleteColor(AColorIndex);
          LNewColorIndex := Self.InsertColor(AScale, LColor);

          // process color type
          ChangeColorType(LNewColorIndex, LColorType);

          // process mid point scale
          if LNewColorIndex > 0 then
          begin
            LRGBGradient := TgmSimpleRGBGradient(FList.Items[LNewColorIndex - 1]);
            LRGBGradient.MidLocationScale := LLeftMidScale;
          end;
        end
        else
        begin
          LRGBGradient.EndLocationScale := AScale;
        end;
      end
      else
      begin
        LLeftRGBGradient  := TgmSimpleRGBGradient(FList.Items[AColorIndex - 1]);
        LRightRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex]);
        LColor            := LRightRGBGradient.StartColor;
        LColorType        := LRightRGBGradient.StartColorType;

        if (AScale > LRightRGBGradient.EndLocationScale) or
           (AScale < LLeftRGBGradient.StartLocationScale) then
        begin
          LLeftMidScale  := LLeftRGBGradient.MidLocationScale;
          LRightMidScale := LRightRGBGradient.MidLocationScale;

          Self.DeleteColor(AColorIndex);
          LNewColorIndex := Self.InsertColor(AScale, LColor);

          // process color type
          ChangeColorType(LNewColorIndex, LColorType);

          // process mid point scale...
          if LNewColorIndex = 0 then
          begin
            LRightRGBGradient := TgmSimpleRGBGradient(FList.Items[LNewColorIndex]);
            LRightRGBGradient.MidLocationScale := LRightMidScale;
          end
          else
          if LNewColorIndex = (Self.ColorCount - 1) then
          begin
            LLeftRGBGradient := TgmSimpleRGBGradient(FList.Items[LNewColorIndex - 1]);
            LLeftRGBGradient.MidLocationScale := LLeftMidScale;
          end
          else
          begin
            LLeftRGBGradient  := TgmSimpleRGBGradient(FList.Items[LNewColorIndex - 1]);
            LRightRGBGradient := TgmSimpleRGBGradient(FList.Items[LNewColorIndex]);

            LLeftRGBGradient.MidLocationScale  := LLeftMidScale;
            LRightRGBGradient.MidLocationScale := LRightMidScale;
          end;
        end
        else
        begin
          LRightRGBGradient.StartLocationScale := AScale;
          LLeftRGBGradient.EndLocationScale    := AScale;
        end;
      end;

      Result := LNewColorIndex;
    end;
  end;
end; { ChangeColorLocationScale }

{ Change middle point gradient scale between two colors.
  @AColorIndex1 - The left side color of the middle point.
  @AColorIndex2 - The right side color of the middle point,
                  it is greater than AColorIndex1 by 1.
  @AScale -  range from 0..1 }
function TgmRGBGradient.ChangeMidPointScale(
  const AColorIndex1, AColorIndex2: Integer; const AScale: Single): Boolean;
var
  LRGBGradient: TgmSimpleRGBGradient;
begin
  Result := False;

  if (FList.Count > 0) and
     (AColorIndex1 >= 0) and (AColorIndex1 < FList.Count) and
     (AColorIndex2 >= 0) and (AColorIndex2 <= FList.Count) and
     ( (AColorIndex2 - AColorIndex1) = 1 ) then
  begin
    LRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex1]);
    LRGBGradient.MidLocationScale := AScale;

    Result := True;
  end;
end; { ChangeMidPointScale }

{ Returns the inserted color index.
  Note that the return value is not the index of a simple color gradient object. }
function TgmRGBGradient.InsertColor(const ALocationScale: Single;
  const AColor: TColor32): Integer;
var
  LFirstRGBGradient: TgmSimpleRGBGradient;
  LLastRGBGradient : TgmSimpleRGBGradient;
  LRGBGradient     : TgmSimpleRGBGradient;
  LNewRGBGradient  : TgmSimpleRGBGradient;
  LInsertIndex     : Integer;
  i                : Integer;
begin
  Result       := -1;
  LInsertIndex := -1;

  if (ALocationScale >= 0.0) and (ALocationScale <= 1.0) then
  begin
    LFirstRGBGradient := TgmSimpleRGBGradient(FList.Items[0]);
    LLastRGBGradient  := TgmSimpleRGBGradient(FList.Items[FList.Count - 1]);

    // check if we insert a color before all other colors in the gradient
    if ALocationScale <= LFirstRGBGradient.StartLocationScale then
    begin
      LNewRGBGradient := TgmSimpleRGBGradient.Create(AColor,
        LFirstRGBGradient.StartColor, ALocationScale,
        LFirstRGBGradient.StartLocationScale);

      // exchange color type
      LNewRGBGradient.EndColorType := LFirstRGBGradient.StartColorType;

      // set foreground/background color for the inserted simple RGB gradient
      LNewRGBGradient.ForegroundColor := FForegroundColor;
      LNewRGBGradient.BackgroundColor := FBackgroundColor;

      FList.Insert(0, LNewRGBGradient);
      
      LInsertIndex := 0;
    end
    else
    // check if we insert a color after all other colors in the gradient
    if ALocationScale >= LLastRGBGradient.EndLocationScale then
    begin
      LNewRGBGradient := TgmSimpleRGBGradient.Create(LLastRGBGradient.EndColor,
        AColor, LLastRGBGradient.EndLocationScale, ALocationScale);

      // exchange color type
      LNewRGBGradient.StartColorType := LLastRGBGradient.EndColorType;

      // set foreground/background color for the inserted simple RGB gradient
      LNewRGBGradient.ForegroundColor := FForegroundColor;
      LNewRGBGradient.BackgroundColor := FBackgroundColor;

      FList.Add(LNewRGBGradient);
      LInsertIndex := FList.Count;
    end
    else
    begin
      // the new color is inserted between two of the exist colors...

      for i := 0 to (FList.Count - 1) do
      begin
        LRGBGradient := TgmSimpleRGBGradient(FList.Items[i]);

        if (ALocationScale >= LRGBGradient.StartLocationScale) and
           (ALocationScale <= LRGBGradient.EndLocationScale) then
        begin
          LNewRGBGradient := TgmSimpleRGBGradient.Create(
            AColor, LRGBGradient.EndColor, ALocationScale,
            LRGBGradient.EndLocationScale);

          LRGBGradient.EndColor         := AColor;
          LRGBGradient.EndLocationScale := ALocationScale;

          // exchange color type
          LNewRGBGradient.EndColorType := LRGBGradient.EndColorType;
          LRGBGradient.EndColorType    := LNewRGBGradient.StartColorType;

          // set foreground/background color for the inserted simple RGB gradient
          LNewRGBGradient.ForegroundColor := FForegroundColor;
          LNewRGBGradient.BackgroundColor := FBackgroundColor;

          LInsertIndex := i + 1;
          FList.Insert(LInsertIndex, LNewRGBGradient);

          Break;
        end;
      end;
    end;

    // not that, the color index equals to the inserted index
    Result := LInsertIndex;
  end;
end; { InsertColor }

function TgmRGBGradient.DeleteColor(const AColorIndex: Integer): Boolean;
var
  LMaxColorIndex  : Integer;
  LCurrRGBGradient: TgmSimpleRGBGradient;
  LPrevRGBGradient: TgmSimpleRGBGradient;
begin
  Result := False;

  if FList.Count > 1 then
  begin
    LMaxColorIndex := FList.Count;

    if (AColorIndex >= 0) and (AColorIndex <= LMaxColorIndex) then
    begin
      if AColorIndex = 0 then
      begin
        // delete fist color
        FList.Delete(0);
      end
      else
      if AColorIndex = LMaxColorIndex then
      begin
        // delete last color
        FList.Delete(FList.Count - 1);
      end
      else
      begin
        // delete the color between the first and the last one
        LPrevRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex - 1]);
        LCurrRGBGradient := TgmSimpleRGBGradient(FList.Items[AColorIndex]);

        LPrevRGBGradient.EndColor         := LCurrRGBGradient.EndColor;
        LPrevRGBGradient.EndLocationScale := LCurrRGBGradient.EndLocationScale;

        LPrevRGBGradient.EndColorType     := LCurrRGBGradient.EndColorType;

        FList.Delete(AColorIndex);
      end;

      Result := True;
    end;
  end;
end; { DeleteColor }

procedure TgmRGBGradient.DrawColorGradient(const ABmp: TBitmap32);
var
  x, y          : Integer;
  LBmpColorArray: PColor32Array;
begin
{$RANGECHECKS OFF}
  if ABmp.Height <= 0 then
  begin
    Exit;
  end;

  if ABmp.Width < FTotalGradientSteps then
  begin
    ABmp.Width := FTotalGradientSteps;
  end;

  for y := 0 to (ABmp.Height - 1) do
  begin
    LBmpColorArray := ABmp.ScanLine[y];

    for x := 0 to (ABmp.Width - 1) do
    begin
      LBmpColorArray[x] := FGradientColorArray[x];
    end;
  end;
{$RANGECHECKS ON}
end; { DrawColorGradients }

//-- TgmSimpleAlphaGradient ----------------------------------------------------

constructor TgmSimpleAlphaGradient.Create(const AStartAlpha, AEndAlpha: Byte;
  const AStartLocationScale, AEndLocationScale: Single);
begin
  inherited Create;

  FStartAlpha         := AStartAlpha;
  FEndAlpha           := AEndAlpha;
  FStartLocationScale := AStartLocationScale;
  FMidLocationScale   := 0.5;
  FEndLocationScale   := AEndLocationScale;
  FTotalSteps         := 0;
  FGradientAlphaArray := nil;
  FMidAlpha           := GetMidAlpha;
end; { Create }

destructor TgmSimpleAlphaGradient.Destroy;
begin
  SetLength(FGradientAlphaArray, 0);
  FGradientAlphaArray := nil;

  inherited Destroy;
end; { Destroy }

procedure TgmSimpleAlphaGradient.FillGradientAlphaArray;
var
  i, LLength : Integer;
  LMidLength : Integer;
  LAlphaArray: TArrayOfByte;
begin
  LMidLength := Round(FGradientSteps * FMidLocationScale);

  // get first part of gradient alpha values ...
  LAlphaArray := GetGradientAlphaArray(FStartAlpha, FMidAlpha, LMidLength);

  if Length(LAlphaArray) > 0 then
  begin
    for i := 0 to (LMidLength - 1) do
    begin
      FGradientAlphaArray[i] := LAlphaArray[i];
    end;

    SetLength(LAlphaArray, 0);
    LAlphaArray := nil;
  end;

  // get second part of gradient alpha values ...
  LLength     := FGradientSteps - LMidLength;
  LAlphaArray := GetGradientAlphaArray(FMidAlpha, FEndAlpha, LLength);

  if Length(LAlphaArray) > 0 then
  begin
    for i := 0 to (LLength - 1) do
    begin
      FGradientAlphaArray[i + LMidLength] := LAlphaArray[i];
    end;

    SetLength(LAlphaArray, 0);
    LAlphaArray := nil;
  end;
end; { FillGradientAlphaArray }

procedure TgmSimpleAlphaGradient.UpdateGradientAlphaArray;
begin
  FGradientSteps := GetGradientSteps;

  if FGradientSteps > 0 then
  begin
    SetLength(FGradientAlphaArray, FGradientSteps);
    FillGradientAlphaArray;
  end
  else
  begin
    SetLength(FGradientAlphaArray, 0);
    FGradientAlphaArray := nil;
  end;
end; { UpdateGradientAlphaArray }

procedure TgmSimpleAlphaGradient.SetStartAlpha(const AValue: Byte);
begin
  if FStartAlpha <> AValue then
  begin
    FStartAlpha := AValue;
    FMidAlpha   := GetMidAlpha;
  end; 
end; { SetStartAlpha }

procedure TgmSimpleAlphaGradient.SetEndAlpha(const AValue: Byte);
begin
  if FEndAlpha <> AValue then
  begin
    FEndAlpha := AValue;
    FMidAlpha := GetMidAlpha;
  end;
end; { SetEndAlpha }

procedure TgmSimpleAlphaGradient.SetStartLocationScale(const AScale: Single);
begin
  if (AScale >= 0.0) and (AScale <= 1.0) then
  begin
    if FStartLocationScale <> AScale then
    begin
      FStartLocationScale := AScale;
      FMidAlpha           := GetMidAlpha;
      FGradientSteps      := GetGradientSteps;
    end;
  end;
end; { SetStartLocationScale }

procedure TgmSimpleAlphaGradient.SetEndLocationScale(const AScale: Single);
begin
  if (AScale >= 0.0) and (AScale <= 1.0) then
  begin
    if FEndLocationScale <> AScale then
    begin
      FEndLocationScale := AScale;
      FMidAlpha         := GetMidAlpha;
      FGradientSteps    := GetGradientSteps;
    end;
  end;
end; { SetEndLocationScale }

procedure TgmSimpleAlphaGradient.SetMidLocationScale(const AScale: Single);
begin
  if (AScale >= 0.05) and (AScale <= 0.95) then
  begin
    if FMidLocationScale <> AScale then
    begin
      FMidLocationScale := AScale;
      FMidAlpha         := GetMidAlpha;
    end;
  end;
end; { SetMidLocationScale }

{ Copy all of gradient alpha values to the dest array.
  @AStartIndex - indicates from where the copy begins at the dest array.
  The Result value is the number of actually copies. }
function TgmSimpleAlphaGradient.CopyGradientAlphaValues(
  const ADestArray: TArrayOfByte; const AStartIndex: Integer): Integer;
var
  LDestHighIndex: Integer;
  LCopyLength   : Integer;
  i             : Integer;
begin
  Result := 0;

  if (FGradientSteps > 0) and (AStartIndex >= 0) then
  begin
    LDestHighIndex := High(ADestArray);
    LCopyLength    := LDestHighIndex - AStartIndex + 1;

    if LCopyLength > FGradientSteps then
    begin
      LCopyLength := FGradientSteps;
    end;

    for i := 0 to (LCopyLength - 1) do
    begin
      ADestArray[AStartIndex + i] := FGradientAlphaArray[i];
    end;

    Result := LCopyLength;
  end;
end; { CopyGradientAlphaValues }

function TgmSimpleAlphaGradient.GetMidAlpha: Byte;
var
  LMidAlpha: Integer;
begin
  LMidAlpha := Round( FStartAlpha * FMidLocationScale + FEndAlpha * (1.0 - FMidLocationScale) );
  Result    := Clamp(LMidAlpha);
end; { GetMidAlpha }

function TgmSimpleAlphaGradient.GetGradientSteps: Integer;
begin
  Result := 0;

  if (FTotalSteps > 0) and
     (FEndLocationScale >= FStartLocationScale) then
  begin
    Result := Round( (FEndLocationScale - FStartLocationScale) * FTotalSteps );
  end;
end; { GetGradientSteps }

{ Resturns the actual horizontal location of the start alpha at the length of
  the total steps. }
function TgmSimpleAlphaGradient.GetStartAlphaLocationAtTotalSteps: Integer;
begin
  Result := Round(FStartLocationScale * FTotalSteps);
end; { GetStartAlphaLocationAtTotalSteps }

{ Resturns the actual horizontal location of the end alpha at the length of
  the total steps. }
function TgmSimpleAlphaGradient.GetEndAlphaLocationAtTotalSteps: Integer;
begin
  Result := Round(FEndLocationScale * FTotalSteps);
end; { GetEndAlphaLocationAtTotalSteps }

{ Resturns the actual horizontal location of the mid point at the length of
  the total steps. }
function TgmSimpleAlphaGradient.GetMidLocationAtTotalSteps: Integer;
begin
  Result := Round(FStartLocationScale * FTotalSteps +
                  FMidLocationScale * FGradientSteps);
end; { GetMidLocationAtTotalSteps }

//-- TgmAlphaGradient ----------------------------------------------------------

constructor TgmAlphaGradient.Create;
var
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  inherited Create;

  FList := TObjectList.Create;

  // add a default simple Alpha gradient to the list
  LSimpleAlphaGradient := TgmSimpleAlphaGradient.Create(255, 255, 0.0, 1.0);
  FList.Add(LSimpleAlphaGradient);

  FGradientAlphaArray := nil;
  FTotalGradientSteps := 0;
end; { Create }

destructor TgmAlphaGradient.Destroy;
begin
  if Length(FGradientAlphaArray) > 0 then
  begin
    SetLength(FGradientAlphaArray, 0);
    FGradientAlphaArray := nil;
  end;

  FList.Clear;
  FList.Free;
  
  inherited Destroy;
end; { Destroy }

procedure TgmAlphaGradient.SetTotalGradientSteps(const ASteps: Integer);
var
  i                   : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  if ASteps > 0 then
  begin
    FTotalGradientSteps := ASteps;

    if FList.Count > 0 then
    begin
      for i := 0 to (FList.Count - 1) do
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[i]);
        LSimpleAlphaGradient.TotalSteps := FTotalGradientSteps;
      end;
    end;
  end;
end; { SetTotalGradientSteps }

function TgmAlphaGradient.GetAlphaValueCount: Integer;
begin
  Result := FList.Count + 1;
end; { GetAlphaValueCount }

function TgmAlphaGradient.GetPrimaryAlphaValues(AIndex: Integer): Byte;
var
  LMaxIndex           : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result    := 0;
  LMaxIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxIndex) then
  begin
    if AIndex < LMaxIndex then
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex]);
      Result               := LSimpleAlphaGradient.StartAlpha;
    end
    else
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);
      Result               := LSimpleAlphaGradient.EndAlpha;
    end;
  end;
end; { GetPrimaryAlphaValues }

function TgmAlphaGradient.GetPrimaryScales(AIndex: Integer): Single;
var
  LMaxIndex           : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result    := 0.0;
  LMaxIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxIndex) then
  begin
    if AIndex < LMaxIndex then
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex]);
      Result               := LSimpleAlphaGradient.StartLocationScale;
    end
    else
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);
      Result               := LSimpleAlphaGradient.EndLocationScale;
    end;
  end;
end; { GetPrimaryScales }

function TgmAlphaGradient.GetMidPointLocation(AIndex: Integer): Integer;
var
  LMaxIndex           : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result    := -1;
  LMaxIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxIndex) then
  begin
    if AIndex < LMaxIndex then
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex]);
      Result               := LSimpleAlphaGradient.MidLocAtTotalSteps;
    end
    else
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);
      Result               := LSimpleAlphaGradient.MidLocAtTotalSteps;
    end;
  end;
end; { GetMidPointLocation }

function TgmAlphaGradient.GetMidPointScale(AIndex: Integer): Single;
var
  LMaxIndex           : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result    := -1;
  LMaxIndex := FList.Count;

  if (AIndex >= 0) and (AIndex <= LMaxIndex) then
  begin
    if AIndex < LMaxIndex then
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex]);
      Result               := LSimpleAlphaGradient.MidLocationScale;
    end
    else
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);
      Result               := LSimpleAlphaGradient.MidLocationScale;
    end;
  end;
end; { GetMidPointScale }

procedure TgmAlphaGradient.AssignTo(Dest: TPersistent);
var
  i         : Integer;
  gmDest    : TgmAlphaGradient;
  tempSimple: TgmSimpleAlphaGradient;
begin
  if Dest is TgmAlphaGradient then
  begin
    gmDest := Dest as TgmAlphaGradient;
    gmDest.Clear;
    
    for i := 0 to (Self.FList.Count -1) do
    begin
      tempSimple := Self.FList[i] as TgmSimpleAlphaGradient;
      
      with tempSimple do
      begin
        gmDest.FList.Add( TgmSimpleAlphaGradient.Create(StartAlpha,
                                                        EndAlpha,
                                                        StartLocationScale,
                                                        EndLocationScale) );
      end;
    end;
  end
  else
  begin
    inherited; // assign error
  end;
end; { AssignTo }

procedure TgmAlphaGradient.Clear;
begin
  FList.Clear;
end; { Clear }

procedure TgmAlphaGradient.CalcGradientAlphaValues;
var
  i, j, LIndex, LCopyCount: Integer;
  LSimpleAlphaGradient    : TgmSimpleAlphaGradient;
begin
  if FTotalGradientSteps > 0 then
  begin
    SetLength(FGradientAlphaArray, FTotalGradientSteps);

    if FList.Count > 0 then
    begin
      LIndex := 0;

      for i := 0 to (FList.Count - 1) do
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[i]);
        LSimpleAlphaGradient.UpdateGradientAlphaArray;

        // process the part that before the first alpha value ...
        if i = 0 then
        begin
          if LSimpleAlphaGradient.StartLocationScale > 0.0 then
          begin
            LCopyCount := Round(LSimpleAlphaGradient.StartLocationScale * FTotalGradientSteps);

            for j := 0 to (LCopyCount - 1) do
            begin
              FGradientAlphaArray[j] := LSimpleAlphaGradient.StartAlpha;
            end;

            Inc(LIndex, LCopyCount);
          end;
        end;

        LCopyCount := LSimpleAlphaGradient.CopyGradientAlphaValues(FGradientAlphaArray, LIndex);
        Inc(LIndex, LCopyCount);

        // process the part that after the last alpha value ...
        if ( i = (FList.Count - 1) ) and
           ( LIndex < FTotalGradientSteps )then
        begin
          { Note that, perhaps due to the floating error in the calculations,
            the total entries of all the sub gradient array is always less
            then FTotalGradientSteps by one. So in the condition above,
            we check if (LIndex < FTotalGradientSteps) other than check for
            (LIndex < (FTotalGradientSteps-1). }

          for j := LIndex to (FTotalGradientSteps - 1) do
          begin
            FGradientAlphaArray[j] := LSimpleAlphaGradient.EndAlpha;
          end;
        end;
      end;
    end;
  end;
end; { CalcGradientAlphaValues }

// make alpha values be distributed averagely
procedure TgmAlphaGradient.AverageAlphaLocationScales;
var
  i            : Integer;
  LScale       : Single;
  LCurrGradient: TgmSimpleAlphaGradient;
  LPrevGradient: TgmSimpleAlphaGradient;
begin
  LScale := 1 / FList.Count;

  for i := 0 to (FList.Count - 1) do
  begin
    LCurrGradient := TgmSimpleAlphaGradient(FList.Items[i]);

    if i = 0 then
    begin
      LCurrGradient.StartLocationScale := 0.0;
    end
    else
    begin
      LPrevGradient := TgmSimpleAlphaGradient(FList.Items[i - 1]);

      LCurrGradient.StartLocationScale := LPrevGradient.EndLocationScale;
    end;

    LCurrGradient.EndLocationScale := LScale * (i + 1);
    LCurrGradient.MidLocationScale := 0.5;
  end;
end; { AverageAlphaLocationScales }

// set to the default state
procedure TgmAlphaGradient.SetToDefault;
var
  i                   : Integer;
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  if FList.Count > 1 then
  begin
    for i := (FList.Count - 1) downto 1 do
    begin
      FList.Delete(i);

      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[0]);
      LSimpleAlphaGradient.StartAlpha         := 255;
      LSimpleAlphaGradient.StartLocationScale := 0.0;
      LSimpleAlphaGradient.EndAlpha           := 255;
      LSimpleAlphaGradient.EndLocationScale   := 1.0;
      LSimpleAlphaGradient.MidLocationScale   := 0.5;
    end;
  end;
end; { SetToDefault }

{ Change a alpha value in this gradient.
  @AIndex - which one in the gradient you want to change.
  @AValue - what alpha value to be changed. }
function TgmAlphaGradient.ChangeAlphaValue(const AIndex: Integer;
  const AValue: Byte): Boolean;
var
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result := False;

  if (AIndex >= 0) and (FList.Count > 0) then
  begin
    if AIndex = 0 then
    begin
      LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[0]);
      LSimpleAlphaGradient.StartAlpha := AValue;
    end
    else
    begin
      if AIndex <= (FList.Count) then
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex - 1]);
        LSimpleAlphaGradient.EndAlpha := AValue;

        if AIndex < (FList.Count) then
        begin
          LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AIndex]);
          LSimpleAlphaGradient.StartAlpha := AValue;
        end;
      end;
    end;

    Result := True;
  end;
end; { ChangeAlphaValue }

{ Change middle point gradient scale between two alpha values.
  @AAlphaIndex1 - The left side alpha value of the middle point.
  @AAlphaIndex2 - The right side alpha value of the middle point,
                  it is greater than AAlphaIndex1 by 1.
  @AScale -  range from 0..1 }
function TgmAlphaGradient.ChangeMidPointScale(
  const AAlphaIndex1, AAlphaIndex2: Integer; const AScale: Single): Boolean;
var
  LSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result := False;

  if (FList.Count > 0) and
     (AAlphaIndex1 >= 0) and (AAlphaIndex1 < FList.Count) and
     (AAlphaIndex2 >= 0) and (AAlphaIndex2 <= FList.Count) and
     ( (AAlphaIndex2 - AAlphaIndex1) = 1 ) then
  begin
    LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex1]);
    LSimpleAlphaGradient.MidLocationScale := AScale;

    Result := True;
  end;
end; { ChangeMidPointScale }

{ Return the new index of the alpha. }
function TgmAlphaGradient.ChangeAlphaLocationScale(const AAlphaIndex: Integer;
  const AScale: Single): Integer;
var
  LLeftSimpleAlphaGradient : TgmSimpleAlphaGradient;
  LRightSimpleAlphaGradient: TgmSimpleAlphaGradient;
  LSimpleAlphaGradient     : TgmSimpleAlphaGradient;
  LMaxIndex                : Integer;
  LNewAlphaIndex           : Integer;
  LAlphaValue              : Byte;
  LLeftMidScale            : Single;
  LRightMidScale           : Single;
begin
  Result := -1;

  if FList.Count = 1 then
  begin
    LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[0]);

    if AAlphaIndex = 0 then
    begin
      if AScale > LSimpleAlphaGradient.EndLocationScale then
      begin
        LAlphaValue := LSimpleAlphaGradient.StartAlpha;
        
        LSimpleAlphaGradient.StartAlpha         := LSimpleAlphaGradient.EndAlpha;
        LSimpleAlphaGradient.StartLocationScale := LSimpleAlphaGradient.EndLocationScale;
        LSimpleAlphaGradient.EndAlpha           := LAlphaValue;
        LSimpleAlphaGradient.EndLocationScale   := AScale;
        LSimpleAlphaGradient.MidLocationScale   := 1.0 - LSimpleAlphaGradient.MidLocationScale;
       
        Result := 1;
      end
      else
      begin
        LSimpleAlphaGradient.StartLocationScale := AScale;
        Result := AAlphaIndex;
      end;
    end
    else
    if AAlphaIndex = 1 then
    begin
      if AScale < LSimpleAlphaGradient.StartLocationScale then
      begin
        LAlphaValue := LSimpleAlphaGradient.EndAlpha;
        
        LSimpleAlphaGradient.EndAlpha           := LSimpleAlphaGradient.StartAlpha;
        LSimpleAlphaGradient.EndLocationScale   := LSimpleAlphaGradient.StartLocationScale;
        LSimpleAlphaGradient.StartAlpha         := LAlphaValue;
        LSimpleAlphaGradient.StartLocationScale := AScale;
        LSimpleAlphaGradient.MidLocationScale   := 1.0 - LSimpleAlphaGradient.MidLocationScale;

        Result := 0;
      end
      else
      begin
        LSimpleAlphaGradient.EndLocationScale := AScale;
        Result := AAlphaIndex;
      end;
    end;
  end
  else
  begin
    LMaxIndex      := FList.Count;
    LNewAlphaIndex := AAlphaIndex;

    if (AAlphaIndex >= 0) and (AAlphaIndex <= LMaxIndex) then
    begin
      if AAlphaIndex = 0 then
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[0]);

        if AScale > LSimpleAlphaGradient.EndLocationScale then
        begin
          LAlphaValue    := LSimpleAlphaGradient.StartAlpha;
          LRightMidScale := LSimpleAlphaGradient.MidLocationScale;

          Self.DeleteAlpha(0);

          LNewAlphaIndex := Self.InsertAlpha(AScale, LAlphaValue);

          // process mid point scale
          if LNewAlphaIndex < Self.AlphaValueCount then
          begin
            LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex]);
            LSimpleAlphaGradient.MidLocationScale := LRightMidScale;
          end;
        end
        else
        begin
          LSimpleAlphaGradient.StartLocationScale := AScale;
        end;
      end
      else
      if AAlphaIndex = LMaxIndex then
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);

        if AScale < LSimpleAlphaGradient.StartLocationScale then
        begin
          LAlphaValue   := LSimpleAlphaGradient.EndAlpha;
          LLeftMidScale := LSimpleAlphaGradient.MidLocationScale;

          Self.DeleteAlpha(AAlphaIndex);
          LNewAlphaIndex := Self.InsertAlpha(AScale, LAlphaValue);

          // process mid point scale
          if LNewAlphaIndex > 0 then
          begin
            LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex - 1]);
            LSimpleAlphaGradient.MidLocationScale := LLeftMidScale;
          end;
        end
        else
        begin
          LSimpleAlphaGradient.EndLocationScale := AScale;
        end;
      end
      else
      begin
        LLeftSimpleAlphaGradient  := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex - 1]);
        LRightSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex]);
        LAlphaValue               := LRightSimpleAlphaGradient.StartAlpha;

        if (AScale > LRightSimpleAlphaGradient.EndLocationScale) or
           (AScale < LLeftSimpleAlphaGradient.StartLocationScale) then
        begin
          LLeftMidScale  := LLeftSimpleAlphaGradient.MidLocationScale;
          LRightMidScale := LRightSimpleAlphaGradient.MidLocationScale;

          Self.DeleteAlpha(AAlphaIndex);
          LNewAlphaIndex := Self.InsertAlpha(AScale, LAlphaValue);

          // process mid point scale...
          if LNewAlphaIndex = 0 then
          begin
            LRightSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex]);
            LRightSimpleAlphaGradient.MidLocationScale := LRightMidScale;
          end
          else
          if LNewAlphaIndex = (Self.AlphaValueCount - 1) then
          begin
            LLeftSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex - 1]);
            LLeftSimpleAlphaGradient.MidLocationScale := LLeftMidScale;
          end
          else
          begin
            LLeftSimpleAlphaGradient  := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex - 1]);
            LRightSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[LNewAlphaIndex]);

            LLeftSimpleAlphaGradient.MidLocationScale  := LLeftMidScale;
            LRightSimpleAlphaGradient.MidLocationScale := LRightMidScale;
          end;
        end
        else
        begin
          LRightSimpleAlphaGradient.StartLocationScale := AScale;
          LLeftSimpleAlphaGradient.EndLocationScale    := AScale;
        end;
      end;

      Result := LNewAlphaIndex;
    end;
  end;
end; { ChangeAlphaLocationScale }

{ Returns the inserted alpha index.
  Note that the return value is not the index of a simple alpha gradient object. }
function TgmAlphaGradient.InsertAlpha(const ALocationScale: Single;
  const AAlphaValue: Byte): Integer;
var
  LFirstSimpleAlphaGradient: TgmSimpleAlphaGradient;
  LLastSimpleAlphaGradient : TgmSimpleAlphaGradient;
  LSimpleAlphaGradient     : TgmSimpleAlphaGradient;
  LNewSimpleAlphaGradient  : TgmSimpleAlphaGradient;
  LInsertIndex             : Integer;
  i                        : Integer;
begin
  Result       := -1;
  LInsertIndex := -1;

  if (ALocationScale >= 0.0) and (ALocationScale <= 1.0) then
  begin
    LFirstSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[0]);
    LLastSimpleAlphaGradient  := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);

    // check if we insert a alpha value before all other alpha values in the gradient
    if ALocationScale <= LFirstSimpleAlphaGradient.StartLocationScale then
    begin
      LNewSimpleAlphaGradient := TgmSimpleAlphaGradient.Create(AAlphaValue,
        LFirstSimpleAlphaGradient.StartAlpha, ALocationScale,
        LFirstSimpleAlphaGradient.StartLocationScale);

      FList.Insert(0, LNewSimpleAlphaGradient);
      LInsertIndex := 0;
    end
    else
    // check if we insert a alpha value after all other alpha values in the gradient
    if ALocationScale >= LLastSimpleAlphaGradient.EndLocationScale then
    begin
      LNewSimpleAlphaGradient := TgmSimpleAlphaGradient.Create(
        LLastSimpleAlphaGradient.EndAlpha, AAlphaValue,
        LLastSimpleAlphaGradient.EndLocationScale, ALocationScale);

      FList.Add(LNewSimpleAlphaGradient);
      LInsertIndex := FList.Count;
    end
    else
    begin
      // the new alpha value is inserted between two of the exist alpha values ...

      for i := 0 to (FList.Count - 1) do
      begin
        LSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[i]);

        if (ALocationScale >= LSimpleAlphaGradient.StartLocationScale) and
           (ALocationScale <= LSimpleAlphaGradient.EndLocationScale) then
        begin
          LNewSimpleAlphaGradient := TgmSimpleAlphaGradient.Create(
            AAlphaValue, LSimpleAlphaGradient.EndAlpha, ALocationScale,
            LSimpleAlphaGradient.EndLocationScale);

          LSimpleAlphaGradient.EndAlpha         := AAlphaValue;
          LSimpleAlphaGradient.EndLocationScale := ALocationScale;

          LInsertIndex := i + 1;
          FList.Insert(LInsertIndex, LNewSimpleAlphaGradient);

          Break;
        end;
      end;
    end;

    // not that, the color index equals to the inserted index
    Result := LInsertIndex;
  end;
end; { InsertAlpha }

function TgmAlphaGradient.DeleteAlpha(const AAlphaIndex: Integer): Boolean;
var
  LMaxIndex               : Integer;
  LCurrSimpleAlphaGradient: TgmSimpleAlphaGradient;
  LPrevSimpleAlphaGradient: TgmSimpleAlphaGradient;
begin
  Result := False;

  if FList.Count > 1 then
  begin
    LMaxIndex := FList.Count;

    if (AAlphaIndex >= 0) and (AAlphaIndex <= LMaxIndex) then
    begin
      if AAlphaIndex = 0 then
      begin
        // delete fist alpha value
        FList.Delete(0);
      end
      else
      if AAlphaIndex = LMaxIndex then
      begin
        // delete last alpha value
        FList.Delete(FList.Count - 1);
      end
      else
      begin
        // delete the alpha value between the first and the last one
        LPrevSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex - 1]);
        LCurrSimpleAlphaGradient := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex]);

        LPrevSimpleAlphaGradient.EndAlpha         := LCurrSimpleAlphaGradient.EndAlpha;
        LPrevSimpleAlphaGradient.EndLocationScale := LCurrSimpleAlphaGradient.EndLocationScale;

        FList.Delete(AAlphaIndex);
      end;

      Result := True;
    end;
  end;
end; { DeleteAlpha }

function TgmAlphaGradient.GetSimpleAlphaGradient(
  const AAlphaIndex: Integer): TgmSimpleAlphaGradient;
var
  LMaxIndex: Integer;
begin
  Result    := nil;
  LMaxIndex := FList.Count;

  if (AAlphaIndex >= 0) and (AAlphaIndex <= LMaxIndex) then
  begin
    if AAlphaIndex < LMaxIndex then
    begin
      Result := TgmSimpleAlphaGradient(FList.Items[AAlphaIndex]);
    end
    else
    begin
      Result := TgmSimpleAlphaGradient(FList.Items[FList.Count - 1]);
    end;
  end;
end; { GetSimpleAlphaGradient }

//-- TgmColorGradient ----------------------------------------------------------

constructor TgmColorGradient.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FAlphaGradient    := TgmAlphaGradient.Create;
  FRGBGradient      := TgmRGBGradient.Create;
  FOutputColorArray := nil;
  FGradientLength   := 0;
  FName             := '';
  FSelected         := False;
end; { Create }

constructor TgmColorGradient.Create;
begin
  Create(nil);
end; { Create }

destructor TgmColorGradient.Destroy;
begin
  if Length(FOutputColorArray) > 0 then
  begin
    SetLength(FOutputColorArray, 0);
    FOutputColorArray := nil;
  end;

  FAlphaGradient.Free;
  FRGBGradient.Free;
  
  inherited Destroy;
end; { Destroy }

procedure TgmColorGradient.SetGradientLength(const ALength: Integer);
begin
  if ALength > 0 then
  begin
    FGradientLength                   := ALength;
    FAlphaGradient.TotalGradientSteps := FGradientLength;
    FRGBGradient.TotalGradientSteps   := FGradientLength;
  end;
end; { SetGradientLength }

procedure TgmColorGradient.SetName(const AName: string);
begin
  FDisplayName := AName;
  FName        := AName;
end; { SetName }

function TgmColorGradient.GetAbsolutStartColor: TColor32;
var
  LAlpha: Byte;
  LRGB  : TColor32;
begin
  LAlpha := FAlphaGradient.PrimaryAlphaValues[0];
  LRGB   := FRGBGradient.PrimaryColors[0] and $FFFFFF;
  Result := (LAlpha shl 24) or LRGB;
end; { GetAbsolutStartColor }

function TgmColorGradient.GetAbsolutEndColor: TColor32;
var
  LAlpha: Byte;
  LRGB  : TColor32;
begin
  LAlpha := FAlphaGradient.PrimaryAlphaValues[FAlphaGradient.AlphaValueCount - 1];
  LRGB   := FRGBGradient.PrimaryColors[FRGBGradient.ColorCount - 1] and $FFFFFF;
  Result := (LAlpha shl 24) or LRGB;
end; { GetAbsolutEndColor }

procedure TgmColorGradient.AssignTo(Dest: TPersistent);
var
  gmDest: TgmColorGradient;
begin
  if Dest = Self then
  begin
    Exit;
  end;

  if Dest is TgmColorGradient then
  begin
    gmDest := Dest as TgmColorGradient;

    gmDest.AlphaGradient.Assign(Self.AlphaGradient);
    gmDest.RGBGradient.Assign(Self.RGBGradient);

    gmDest.GradientLength := Self.GradientLength;
    gmDest.Name           := Self.Name;
  end
  else
  begin
    inherited; // assign error
  end;
end; { AssignTo }

procedure TgmColorGradient.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
  FName        := Value;
end; { SetDisplayName }

function TgmColorGradient.GetDisplayName: string;
begin
  Result := FDisplayName;
end; { GetDisplayName }

procedure TgmColorGradient.RefreshColorArray;
var
  i     : Integer;
  LAlpha: Cardinal;
  LRGB  : Cardinal;
begin
  if FGradientLength > 0 then
  begin
    SetLength(FOutputColorArray, FGradientLength);

    // refresh Alpha and RGB gradients
    FAlphaGradient.CalcGradientAlphaValues;
    FRGBGradient.CalcGradientColors;

    // copy gradient colors to output color array
    for i := 0 to (FGradientLength - 1) do
    begin
      LAlpha := FAlphaGradient.FGradientAlphaArray[i] shl 24;
      LRGB   := (FRGBGradient.FGradientColorArray[i]) and $FFFFFF;

      FOutputColorArray[i] := LAlpha or LRGB;
    end;
  end;
end; { RefreshColorArray }

procedure TgmColorGradient.DrawColorGradients(const ABmp: TBitmap32);
var
  x, y          : Integer;
  LBmpColorArray: PColor32Array;
begin
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
      LBmpColorArray[x] := FOutputColorArray[x];
    end;
  end;
  
{$RANGECHECKS ON}
end; { DrawColorGradients }

procedure TgmColorGradient.SaveToStream(const AStream: TStream);
var
  LInfoHeader : TgmGradientInfoHeader;
  LColor      : TColor32;
  LWinColor   : TColor;
  LScale      : Single;
  LAlphaValue : Byte;
  i, LMaxIndex: Integer;
begin
  if Assigned(AStream) then
  begin
    // fill in info header...
    LInfoHeader.Name           := Self.FName;
    LInfoHeader.ColorStopCount := Self.FRGBGradient.ColorCount;
    LInfoHeader.AlphaStopCount := Self.FAlphaGradient.AlphaValueCount;

    // write in the data of info header
    AStream.Write( LInfoHeader, SizeOf(TgmGradientInfoHeader) );

    // Write in color stop data...
    
    if Self.FRGBGradient.ColorCount > 0 then
    begin
      LMaxIndex := Self.FRGBGradient.ColorCount - 1;

      for i := 0 to (LMaxIndex - 1) do
      begin
        LColor := Self.FRGBGradient.PrimaryColors[i];

        case Self.FRGBGradient.PrimaryColorTypes[i] of
          gctStaticColor:
            begin
              LWinColor := WinColor(LColor);
            end;

          gctDynamicForegroundColor:
            begin
              LWinColor := clDefault; // indicating dyanmaic foreground color
            end;

          gctDynamicBackgroundColor:
            begin
              LWinColor := clBackground; // indicating dyanmaic background color
            end;
        end;
        
        AStream.Write(LWinColor, 4);

        LScale := Self.FRGBGradient.PrimaryScales[i];
        AStream.Write(LScale, 4);

        LScale := Self.FRGBGradient.MidPointScale[i];
        AStream.Write(LScale, 4);
      end;

      // write in the last color stop
      LColor := Self.FRGBGradient.PrimaryColors[LMaxIndex];

      case Self.FRGBGradient.PrimaryColorTypes[LMaxIndex] of
        gctStaticColor:
          begin
            LWinColor := WinColor(LColor);
          end;

        gctDynamicForegroundColor:
          begin
            LWinColor := clDefault; // indicating dyanmaic foreground color
          end;

        gctDynamicBackgroundColor:
          begin
            LWinColor := clBackground; // indicating dyanmaic background color
          end;
      end;

      AStream.Write(LWinColor, 4);

      LScale := Self.FRGBGradient.PrimaryScales[LMaxIndex];
      AStream.Write(LScale, 4);

      // dummy data
      LScale := 0.5;
      AStream.Write(LScale, 4);
    end;

    // Write in alpha stop data...

    if Self.FAlphaGradient.AlphaValueCount > 0 then
    begin
      LMaxIndex := Self.FAlphaGradient.AlphaValueCount - 1;

      for i := 0 to (LMaxIndex - 1) do
      begin
        LAlphaValue := Self.FAlphaGradient.PrimaryAlphaValues[i];
        AStream.Write(LAlphaValue, 1);

        LScale := Self.AlphaGradient.PrimaryScales[i];
        AStream.Write(LScale, 4);

        LScale := Self.AlphaGradient.MidPointScale[i];
        AStream.Write(LScale, 4);
      end;

      // write in the last alpha stop
      LAlphaValue := Self.AlphaGradient.PrimaryAlphaValues[LMaxIndex];
      AStream.Write(LAlphaValue, 1);

      LScale := Self.AlphaGradient.PrimaryScales[LMaxIndex];
      AStream.Write(LScale, 4);

      // dummy data
      LScale := 0.5;
      AStream.Write(LScale, 4);
    end;
  end;
end; { SaveToStream }

function TgmColorGradient.LoadFromStream(const AStream: TStream): Boolean;
var
  LInfoHeader   : TgmGradientInfoHeader;
  LColor        : TColor;
  LAlphaValue   : Byte;
  LStopScale    : Single;
  LMidPointScale: Single;
  LMaxIndex     : Integer;
  LColorIndex   : Integer;
  i             : Integer;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    // read in the info header
    AStream.Read( LInfoHeader, SizeOf(TgmGradientInfoHeader) );

    Self.FName        := LInfoHeader.Name;
    Self.FDisplayName := LInfoHeader.Name;

    // read in data of color stops 
    if LInfoHeader.ColorStopCount >= 2  then
    begin
      // set the RGB gradient to default state
      Self.FRGBGradient.SetToDefault;

      LMaxIndex := LInfoHeader.ColorStopCount - 1;

      for i := 0 to LMaxIndex do
      begin
        AStream.Read(LColor, 4);         // read in color
        AStream.Read(LStopScale, 4);     // read in color location scale
        AStream.Read(LMidPointScale, 4); // read in mid point scale

        if i = 0 then
        begin
          Self.FRGBGradient.ChangeColor( 0, Color32(LColor) );
          Self.FRGBGradient.ChangeColorLocationScale(0, LStopScale);

          if LColor = clDefault then
          begin
            Self.FRGBGradient.ChangeColorType(0, gctDynamicForegroundColor);
          end
          else if LColor = clBackground then
          begin
            Self.FRGBGradient.ChangeColorType(0, gctDynamicBackgroundColor);
          end;
        end
        else
        begin
          if i < LMaxIndex then
          begin
            LColorIndex := Self.FRGBGradient.InsertColor(LStopScale, Color32(LColor));

            if LColor = clDefault then
            begin
              Self.FRGBGradient.ChangeColorType(LColorIndex, gctDynamicForegroundColor);
            end
            else if LColor = clBackground then
            begin
              Self.FRGBGradient.ChangeColorType(LColorIndex, gctDynamicBackgroundColor);
            end;
          end;
        end;

        if i < LMaxIndex then
        begin
          Self.FRGBGradient.ChangeMidPointScale(i, i + 1, LMidPointScale);
        end;

        if i = LMaxIndex then
        begin
          Self.FRGBGradient.ChangeColor(LMaxIndex, Color32(LColor));
          Self.FRGBGradient.ChangeColorLocationScale(LMaxIndex, LStopScale);

          if LColor = clDefault then
          begin
            Self.FRGBGradient.ChangeColorType(LMaxIndex, gctDynamicForegroundColor);
          end
          else if LColor = clBackground then
          begin
            Self.FRGBGradient.ChangeColorType(LMaxIndex, gctDynamicBackgroundColor);
          end;
        end;
      end;
    end;

    // read in data of alpha stops 
    if LInfoHeader.AlphaStopCount >= 2  then
    begin
      // set the Alpha gradient to default state
      Self.FAlphaGradient.SetToDefault;

      LMaxIndex := LInfoHeader.AlphaStopCount - 1;
      
      for i := 0 to LMaxIndex do
      begin
        AStream.Read(LAlphaValue, 1);    // read in alpha value
        AStream.Read(LStopScale, 4);     // read in color location scale
        AStream.Read(LMidPointScale, 4); // read in mid point scale

        if i = 0 then
        begin
          Self.FAlphaGradient.ChangeAlphaValue(0, LAlphaValue);
          Self.FAlphaGradient.ChangeAlphaLocationScale(0, LStopScale);
        end
        else
        begin
          if i < LMaxIndex then
          begin
            Self.AlphaGradient.InsertAlpha(LStopScale, LAlphaValue);
          end;
        end;

        if i < LMaxIndex then
        begin
          Self.FAlphaGradient.ChangeMidPointScale(i, i + 1, LMidPointScale);
        end;

        if i = LMaxIndex then
        begin
          Self.FAlphaGradient.ChangeAlphaValue(LMaxIndex, LAlphaValue);
          Self.FAlphaGradient.ChangeAlphaLocationScale(LMaxIndex, LStopScale);
        end;
      end;
    end;

    Result := True;
  end;
end; { LoadFromStream }

function TgmColorGradient.LoadOldGradientsFromStream(
  const AStream: TStream): Boolean;
var
  LInfoHeader: TgmOldGradientInfoHeader;
  LScale     : Single;
  LColor     : TColor;
  LMaxIndex  : Integer;
  i          : Integer;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    // read in the info header
    AStream.Read( LInfoHeader, SizeOf(TgmOldGradientInfoHeader) );

    Self.FName        := LInfoHeader.Name;
    Self.FDisplayName := LInfoHeader.Name;

    // read in data of color stops 
    if LInfoHeader.ColorCount >= 2  then
    begin
      // set the RGB gradient to default state
      Self.FRGBGradient.SetToDefault;

      LMaxIndex := LInfoHeader.ColorCount - 1;
      LScale    := 1 / (LInfoHeader.ColorCount - 1);

      for i := 0 to LMaxIndex do
      begin
        AStream.Read(LColor, SizeOf(TColor) );  // read in color

        if i = 0 then
        begin
          Self.FRGBGradient.ChangeColor( 0, Color32(LColor) );
        end
        else
        begin
          if i < LMaxIndex then
          begin
            Self.FRGBGradient.InsertColor( LScale * i, Color32(LColor) );
          end;
        end;

        if i = LMaxIndex then
        begin
          Self.FRGBGradient.ChangeColor( LMaxIndex, Color32(LColor) );
        end;
      end;
    end;

    // set the Alpha gradient to default state
    Self.FAlphaGradient.SetToDefault;

    Result := True;
  end;
end; { LoadOldGradientsFromStream }

//-- TgmGradientCollection -----------------------------------------------------

constructor TgmGradientCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TgmColorGradient);
end;

function TgmGradientCollection.GetItem(Index: Integer): TgmColorGradient;
begin
  Result := TgmColorGradient(inherited GetItem(Index));
end;

procedure TgmGradientCollection.SetItem(Index: Integer;
  const Value: TgmColorGradient);
begin
  inherited SetItem(Index, Value);
end;

function TgmGradientCollection.LoadGradientsVersion1(
  const AStream: TStream; const ALoadCount: Integer): Boolean;
var
  i             : Integer;
  LColorGradient: TgmColorGradient;
begin
  Result := False;

  if Assigned(AStream) and (ALoadCount > 0) then
  begin
    for i := 1 to ALoadCount do
    begin
      LColorGradient := Self.Add;
      LColorGradient.LoadFromStream(AStream);
    end;

    Result := True;
  end;
end; { LoadGradientsVersion1 }

function TgmGradientCollection.LoadGradientsOldVersion(
  const AStream: TStream; const ALoadCount: Integer): Boolean;
var
  i             : Integer;
  LColorGradient: TgmColorGradient;
begin
  Result := False;
  
  if Assigned(AStream) and (ALoadCount > 0) then
  begin
    for i := 1 to ALoadCount do
    begin
      LColorGradient := Self.Add;
      LColorGradient.LoadOldGradientsFromStream(AStream);
    end;

    Result := True;
  end;
end; { LoadGradientsOldVersion }

procedure TgmGradientCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);

  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end; { Update }

// here is the trick!
function TgmGradientCollection.Add: TgmColorGradient;
begin
  Result := TgmColorGradient(inherited Add);
end; { Add }

function TgmGradientCollection.LoadFromFile(const AFileName: string): Boolean;
var
  LStream: TStream;
begin
  if (AFileName = '') or
     ( not FileExists(AFileName) ) then
  begin
    raise Exception.Create('The file is not exists.');
  end;

  LStream := TFileStream.Create( ExpandFileName(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
  
  Changed;
end; { LoadFromFile }

function TgmGradientCollection.LoadFromStream(const AStream: TStream): Boolean;
var
  LFileHeader   : TgmGradientFileHeader;
  LOldFileHeader: TgmOldGradientFileHeader;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    BeginUpdate;
    try
      AStream.Position := 0;

      // read in file header of new version of gradient file
      AStream.Read( LFileHeader, SizeOf(TgmGradientFileHeader) );

      // to check for whether it is an GraphicsMagic's gradient file
      if LFileHeader.FileID = GRADIENT_FILE_ID then
      begin
        if LFileHeader.FileVersion <= GRADIENT_FILE_VERSION then
        begin
          case LFileHeader.FileVersion of
            1:
              begin
                Result := LoadGradientsVersion1(AStream, LFileHeader.GradientCount);
              end;
          end;
        end
        else
        begin
          raise Exception.Create('Cannot open the file because the file version is high.');
        end;
      end
      else
      begin
        // try to load an old version of GraphicsMagic's gradient file
        AStream.Position := 0;

        // read in file header of old version of gradient file
        AStream.Read( LOldFileHeader, SizeOf(TgmOldGradientFileHeader) );

        if (LOldFileHeader.Info = OLD_GRADIENT_ID) and
           (LOldFileHeader.GradientMode = OLD_GRADIENT_MODE) then
        begin
          Result := LoadGradientsOldVersion(AStream, LOldFileHeader.GradientCount);
        end
        else
        begin
          raise Exception.Create('Cannot open because the file is not supported by GraphicsMagic.');
        end;
      end;
      
    finally
      EndUpdate;
    end;
  end;
end; { LoadFromStream }

procedure TgmGradientCollection.SaveToFile(const AFileName: string);
var
  LStream: TStream;
begin
  if AFileName = '' then
  begin
    raise Exception.Create('Invalid file name.');
  end;

  if Self.Count = 0 then
  begin
    raise Exception.Create('Collection is empty.');
  end;

  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end; { SaveToFile }

procedure TgmGradientCollection.SaveToStream(const AStream: TStream);
var
  LFileHeader: TgmGradientFileHeader;
  LGradient  : TgmColorGradient;
  i          : Integer;
begin
  if Assigned(AStream) then
  begin
    // fill in file header
    LFileHeader.FileID        := GRADIENT_FILE_ID;
    LFileHeader.FileVersion   := GRADIENT_FILE_VERSION;
    LFileHeader.GradientCount := Self.Count;

    // write in file header
    AStream.Write( LFileHeader, SizeOf(TgmGradientFileHeader) );

    // write in data of color gradients
    for i := 0 to (Self.Count - 1) do
    begin
      LGradient := Items[i];
      LGradient.SaveToStream(AStream);
    end;
  end;
end; { SaveToFile }

end.
