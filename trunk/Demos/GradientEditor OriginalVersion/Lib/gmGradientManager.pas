unit gmGradientManager;

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
  Contnrs, Classes, Graphics,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib}
  gmGradient, gmTypes;

type
  TgmColorGradientManager = class(TObject)
  protected
    FColorGradientList: TObjectList;
    FModified         : Boolean;
    FResetted         : Boolean;
    FFileName         : string;
    FOutputMsg        : string;  // output info, such as errors, etc.

    FForegroundColor  : TColor32;
    FBackgroundColor  : TColor32;

    // for drawing thumbnails
    FGradientGalleryMap: TBitmap32;
    FThumbnailSizeMode : TgmThumbnailSizeMode;
    FThumbnailSize     : Integer;
    FRowCount          : Integer;
    FColumnCount       : Integer;

    procedure SetForegroundColor(const AColor: TColor32);
    procedure SetBackgroundColor(const AColor: TColor32);
    procedure SetThumbnailSizeMode(const AMode: TgmThumbnailSizeMode);

    procedure RefreshColumnCount;
    procedure RefreshRowCount;
    procedure RefreshGalleryMapHeight;
    procedure LoadInternalColorGradients;
    procedure ResetForeAndBackColorToGradients;

    function LoadGradientsVersion1(const AStream: TStream;
      const ALoadCount: Integer): Boolean;

    function LoadGradientsOldVersion(const AStream: TStream;
      const ALoadCount: Integer): Boolean;

    function GetGradientCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure Add(const AColorGradient: TgmColorGradient);
    procedure ResetGradients;
    procedure DeselectAllGradients;
    procedure SetGalleryMapWidth(const ALargeColumnCount: Integer);

    procedure DrawGradientGalleryMap(const ADestBmp: TBitmap32;
      const AMarkSelectedGradient: Boolean = False);

    function Draw(const AGradientIndex: Integer; const ABitmap: TBitmap32;
      const ARect: TRect): Boolean; overload;

    function Draw(const AGradientIndex: Integer; const ACanvas: TCanvas;
      const ARect: TRect): Boolean; overload;

    function GetGradientIndexByCoord(const AX, AY: Integer): Integer;
    function GetGradientByteIndex(const AIndex: Integer): TgmColorGradient;
    function CopyGradientByIndex(const AIndex: Integer): TgmColorGradient;
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(const AStream: TStream): Boolean;
    function ReplaceGradients(const AFileName: string): Boolean;
    function DeleteSelectedGradient: Boolean;
    function RenameSelectedGradient(const AName: string): Boolean;

    property IsModified       : Boolean              read FModified;
    property IsResetted       : Boolean              read FResetted;
    property FileName         : string               read FFileName;
    property OuputMsg         : string               read FOutputMsg;
    property GradientCount    : Integer              read GetGradientCount;
    property ForegroundColor  : TColor32             read FForegroundColor   write SetForegroundColor;
    property BackgroundColor  : TColor32             read FBackgroundColor   write SetBackgroundColor;
    property ThumbnailSizeMode: TgmThumbnailSizeMode read FThumbnailSizeMode write SetThumbnailSizeMode;
  end;

implementation

uses
{ Standard }
  SysUtils,
{ GraphicsMagic Lib }
  gmMiscFuncs, gmGradientRender;

const
  SMALL_THUMBNAIL_SIZE: Integer = 32;
  LARGE_THUMBNAIL_SIZE: Integer = 64;
  GALLERY_MAP_WIDTH   : Integer = 257;

//-- Local Methods -------------------------------------------------------------

function ForegroundToBackgroundGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Foreground to Background';
  
    RGBGradient.ChangeColorType(0, gctDynamicForegroundColor);
    RGBGradient.ChangeColorType(1, gctDynamicBackgroundColor);
  end;
end; { ForegroundToBackgroundGradient }

function ForegroundToTransparentGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Foreground to Transparent';

    RGBGradient.ChangeColorType(0, gctDynamicForegroundColor);
    RGBGradient.ChangeColorType(1, gctDynamicForegroundColor);

    AlphaGradient.ChangeAlphaValue(1, 0);
  end;
end; { ForegroundToTransparentGradient }

function BlackWhiteGradient: TgmColorGradient;
begin
  Result      := TgmColorGradient.Create;
  Result.Name := 'Black, White';
end; { BlackWhiteGradient }

function RedGreenGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Red, Green';

    RGBGradient.ChangeColor( 0, $FFE10019 );
    RGBGradient.ChangeColor( 1, $FF00601B );
  end;
end; { RedGreenGradient }

function VioletOrangeGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Violet, Orange';
    
    RGBGradient.ChangeColor( 0, $FF290A59 );
    RGBGradient.ChangeColor( 1, $FFFF7C00 );
  end;
end; { VioletOrangeGradient }

function BlueRedYellowGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Blue, Red, Yellow';

    RGBGradient.ChangeColor( 0, $FF0A00B2 );
    RGBGradient.ChangeColor( 1, $FFFFFC00 );
    RGBGradient.InsertColor( 0.5, $FFFF0000 );
  end;
end; { BlueRedYellowGradient }

function BlueYellowBlueGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Blue, Yellow, Blue';

    RGBGradient.ChangeColor( 0, $FF0B01B8 );
    RGBGradient.ChangeColor( 1, $FF0B02AA );

    RGBGradient.ChangeColorLocationScale(0, 0.1);
    RGBGradient.ChangeColorLocationScale(1, 0.9);
    
    RGBGradient.InsertColor( 0.5, $FFFDFA03 );
  end;
end; { BlueYellowBlueGradient }

function OrangeYellowOrangeGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Orange, Yellow, Orange';

    RGBGradient.ChangeColor( 0, $FFFF6E02 );
    RGBGradient.ChangeColor( 1, $FFFF6D00 );
    RGBGradient.InsertColor( 0.5, $FFFFFF00 );
    RGBGradient.ChangeMidPointScale(1, 2, 0.47);
  end;
end; { OrangeYellowOrangeGradient }

function VioletGreenOrangeGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Violet, Green, Orange';

    RGBGradient.ChangeColor( 0, $FF6F156C );
    RGBGradient.ChangeColor( 1, $FFFD7C00 );
    RGBGradient.InsertColor( 0.5, $FF00601B );
  end;
end; { VioletGreenOrangeGradient }

function YellowVioletOrangeBlueGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Yellow, Violet, Orange, Blue';

    RGBGradient.ChangeColor( 0, $FFF9E600 );
    RGBGradient.ChangeColor( 1, $FF002874 );

    RGBGradient.ChangeColorLocationScale(0, 0.05);
    RGBGradient.ChangeColorLocationScale(1, 0.95);
    
    RGBGradient.InsertColor( 0.35, $FF6F156C );
    RGBGradient.InsertColor( 0.65, $FFFD7C00 );
  end;
end; { YellowVioletOrangeBlueGradient }

function CopperGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Copper';
    
    RGBGradient.ChangeColor( 0, $FF97461A );
    RGBGradient.ChangeColor( 1, $FFEFDBCD );

    RGBGradient.InsertColor( 0.3, $FFFBD8C5 );
    RGBGradient.InsertColor( 0.83, $FF6C2E16 );

    RGBGradient.ChangeMidPointScale(2, 3, 0.6);
  end;
end; { CopperGradient }

function ChromeGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Chrome';

    RGBGradient.ChangeColor( 0, $FF2989CC );
    RGBGradient.ChangeColor( 1, $FFFFFFFF );
    RGBGradient.InsertColor( 0.5, $FFFFFFFF );
    RGBGradient.InsertColor( 0.52, $FF906A00 );
    RGBGradient.InsertColor( 0.64, $FFD99F00 );
    RGBGradient.ChangeMidPointScale(1, 2, 0.13);
  end;
end; { ChromeGradient }

function SpectrumGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Spectrum';

    RGBGradient.ChangeColor( 0, $FFFF0000 );
    RGBGradient.ChangeColor( 1, $FFFF0000 );
    RGBGradient.InsertColor( 0.15, $FFFF00FF );
    RGBGradient.InsertColor( 0.33, $FF0000FF );
    RGBGradient.InsertColor( 0.49, $FF00FFFF );
    RGBGradient.InsertColor( 0.67, $FF00FF00 );
    RGBGradient.InsertColor( 0.84, $FFFFFF00 );
  end;
end; { Spectrum }

function TransparentRainbowGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Transparent Rainbow';

    RGBGradient.ChangeColor( 0, clRed32 );
    RGBGradient.ChangeColorLocationScale(0, 0.12);
    RGBGradient.ChangeColor( 1, Color32(255, 0, 198) );
    RGBGradient.ChangeColorLocationScale(1, 0.88);
    RGBGradient.InsertColor( 0.28, Color32(255, 252, 0) );
    RGBGradient.InsertColor( 0.45, Color32(1, 180, 57) );
    RGBGradient.InsertColor( 0.6, Color32(0, 234, 255) );
    RGBGradient.InsertColor( 0.75, Color32(0, 3, 144) );
    RGBGradient.ChangeMidPointScale(2, 3, 0.6);
    RGBGradient.ChangeMidPointScale(3, 4, 0.35);
    RGBGradient.ChangeMidPointScale(4, 5, 0.6);

    AlphaGradient.ChangeAlphaValue(0, 0);
    AlphaGradient.ChangeAlphaValue(1, 0);
    AlphaGradient.InsertAlpha(0.07, 204);
    AlphaGradient.InsertAlpha(0.11, 255);
    AlphaGradient.InsertAlpha(0.88, 255);
    AlphaGradient.InsertAlpha(0.93, 204);
    AlphaGradient.ChangeMidPointScale(1, 2, 0.7);
    AlphaGradient.ChangeMidPointScale(3, 4, 0.2);
  end;
end; { TransparentRainbowGradient }

function TransparentStripesGradient: TgmColorGradient;
begin
  Result := TgmColorGradient.Create;

  with Result do
  begin
    Name := 'Transparent Stripes';

    RGBGradient.ChangeColorType(0, gctDynamicForegroundColor);
    RGBGradient.ChangeColorType(1, gctDynamicForegroundColor);

    AlphaGradient.ChangeAlphaValue(1, 0);
    AlphaGradient.InsertAlpha(0.09, 255);
    AlphaGradient.InsertAlpha(0.1, 0);
    AlphaGradient.InsertAlpha(0.19, 0);
    AlphaGradient.InsertAlpha(0.2, 255);
    AlphaGradient.InsertAlpha(0.29, 255);
    AlphaGradient.InsertAlpha(0.30, 0);
    AlphaGradient.InsertAlpha(0.39, 0);
    AlphaGradient.InsertAlpha(0.40, 255);
    AlphaGradient.InsertAlpha(0.49, 255);
    AlphaGradient.InsertAlpha(0.5, 0);
    AlphaGradient.InsertAlpha(0.59, 0);
    AlphaGradient.InsertAlpha(0.60, 255);
    AlphaGradient.InsertAlpha(0.69, 255);
    AlphaGradient.InsertAlpha(0.70, 0);
    AlphaGradient.InsertAlpha(0.79, 0);
    AlphaGradient.InsertAlpha(0.80, 255);
    AlphaGradient.InsertAlpha(0.89, 255);
    AlphaGradient.InsertAlpha(0.90, 0);
  end;
end; { TransparentStripesGradient }

//-- TgmColorGradientManager ---------------------------------------------------

constructor TgmColorGradientManager.Create;
begin
  inherited Create;

  FColorGradientList := TObjectList.Create;
  FModified          := False;
  FResetted          := True;
  FFileName          := '';
  FOutputMsg         := '';

  // default color gradients
  FForegroundColor := clBlack32;
  FBackgroundColor := clWhite32;

  // thumbnail
  FGradientGalleryMap       := TBitmap32.Create;
  FGradientGalleryMap.Width := GALLERY_MAP_WIDTH;
  FThumbnailSizeMode        := tsmLarge;
  FThumbnailSize            := LARGE_THUMBNAIL_SIZE;
  FRowCount                 := 0;
  FColumnCount              := 0;

  LoadInternalColorGradients;
end; { Create }

destructor TgmColorGradientManager.Destroy;
begin
  FColorGradientList.Clear;
  FColorGradientList.Free;

  FGradientGalleryMap.Free;

  inherited Destroy;
end; { Destroy }

procedure TgmColorGradientManager.SetForegroundColor(const AColor: TColor32);
var
  LColor        : TColor32;
  LColorGradient: TgmColorGradient;
  i             : Integer;
begin
  // we don't need transparent color
  LColor := $FF000000 or (AColor and $FFFFFF);

  if FForegroundColor <> LColor then
  begin
    FForegroundColor := LColor;

    if FColorGradientList.Count > 0 then
    begin
      for i := 0 to (FColorGradientList.Count - 1) do
      begin
        LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

        LColorGradient.RGBGradient.ForegroundColor := FForegroundColor;
      end;
    end;
  end;
end; { SetForegroundColor }

procedure TgmColorGradientManager.SetBackgroundColor(const AColor: TColor32);
var
  LColor        : TColor32;
  LColorGradient: TgmColorGradient;
  i             : Integer;
begin
  // we don't need transparent color
  LColor := $FF000000 or (AColor and $FFFFFF);

  if FBackgroundColor <> LColor then
  begin
    FBackgroundColor := LColor;
    
    if FColorGradientList.Count > 0 then
    begin
      for i := 0 to (FColorGradientList.Count - 1) do
      begin
        LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

        LColorGradient.RGBGradient.BackgroundColor := FBackgroundColor;
      end;
    end;
  end;
end; { SetBackgroundColor }

procedure TgmColorGradientManager.SetThumbnailSizeMode(
  const AMode: TgmThumbnailSizeMode);
begin
  if FThumbnailSizeMode <> AMode then
  begin
    FThumbnailSizeMode := AMode;

    case FThumbnailSizeMode of
      tsmSmall:
        begin
          FThumbnailSize := SMALL_THUMBNAIL_SIZE;
        end;

      tsmLarge:
        begin
          FThumbnailSize := LARGE_THUMBNAIL_SIZE;
        end;
    end;
  
    RefreshGalleryMapHeight;
  end;
end; { SetThumbnailSizeMode }

procedure TgmColorGradientManager.RefreshColumnCount;
begin
  FColumnCount := FGradientGalleryMap.Width div FThumbnailSize;
end; { RefreshColumnCount }

procedure TgmColorGradientManager.RefreshRowCount;
begin
  if (FColorGradientList.Count mod FColumnCount) > 0 then
  begin
    FRowCount := Trunc(FColorGradientList.Count / FColumnCount) + 1;
  end
  else
  begin
    FRowCount := FColorGradientList.Count div FColumnCount;
  end;
end; { RefreshRowCount }

procedure TgmColorGradientManager.RefreshGalleryMapHeight;
begin
  RefreshColumnCount;
  RefreshRowCount;
  
  FGradientGalleryMap.Height := FRowCount * FThumbnailSize;
end; { RefreshGalleryMapHeight }

function TgmColorGradientManager.LoadGradientsVersion1(
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
      LColorGradient := TgmColorGradient.Create;
      LColorGradient.LoadFromStream(AStream);

      FColorGradientList.Add(LColorGradient);
    end;

    Result := True;
  end;
end; { LoadGradientsVersion1 }

function TgmColorGradientManager.LoadGradientsOldVersion(
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
      LColorGradient := TgmColorGradient.Create;
      LColorGradient.LoadOldGradientsFromStream(AStream);

      FColorGradientList.Add(LColorGradient);
    end;

    Result := True;
  end;
end; { LoadGradientsOldVersion }

procedure TgmColorGradientManager.LoadInternalColorGradients;
begin
  with FColorGradientList do
  begin
    Clear;

    Add(ForegroundToBackgroundGradient);
    Add(ForegroundToTransparentGradient);
    Add(BlackWhiteGradient);
    Add(RedGreenGradient);
    Add(VioletOrangeGradient);
    Add(BlueRedYellowGradient);
    Add(BlueYellowBlueGradient);
    Add(OrangeYellowOrangeGradient);
    Add(VioletGreenOrangeGradient);
    Add(YellowVioletOrangeBlueGradient);
    Add(CopperGradient);
    Add(ChromeGradient);
    Add(SpectrumGradient);
    Add(TransparentRainbowGradient);
    Add(TransparentStripesGradient);
  end;

  ResetForeAndBackColorToGradients;
end; { LoadInternalColorGradients }

procedure TgmColorGradientManager.ResetForeAndBackColorToGradients;
var
  LColorGradient: TgmColorGradient;
  i             : Integer;
begin
  if FColorGradientList.Count > 0 then
  begin
    for i := 0 to (FColorGradientList.Count - 1) do
    begin
      LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

      LColorGradient.RGBGradient.ForegroundColor := FForegroundColor;
      LColorGradient.RGBGradient.BackgroundColor := FBackgroundColor;
    end;
  end;
end; { ResetForeAndBackColorToGradients }

procedure TgmColorGradientManager.SaveToFile(const AFileName: string);
var
  LOutputStream: TMemoryStream;
begin
  if AFileName <> '' then
  begin
    LOutputStream := TMemoryStream.Create;
    try
      Self.SaveToStream(LOutputStream);

      if LOutputStream.Size > 0 then
      begin
        LOutputStream.SaveToFile(AFileName);

        FFileName := AFileName;
        FModified := False;
        FResetted := False;
      end;
      
    finally
      LOutputStream.Clear;
      LOutputStream.Free;
    end;
  end;
end; { SaveToFile }

procedure TgmColorGradientManager.SaveToStream(const AStream: TStream);
var
  LFileHeader: TgmGradientFileHeader;
  LGradient  : TgmColorGradient;
  i          : Integer;
begin
  // don't save the default color gradients to a file
  if Assigned(AStream) and
     (FColorGradientList.Count > 0) then
  begin
    // fill in file header
    LFileHeader.FileID        := GRADIENT_FILE_ID;
    LFileHeader.FileVersion   := GRADIENT_FILE_VERSION;
    LFileHeader.GradientCount := FColorGradientList.Count;

    // write in file header
    AStream.Write( LFileHeader, SizeOf(TgmGradientFileHeader) );

    // write in data of color gradients
    for i := 0 to (FColorGradientList.Count - 1) do
    begin
      LGradient := TgmColorGradient(FColorGradientList.Items[i]);
      LGradient.SaveToStream(AStream);
    end;
  end;
end; { SaveToStream }

procedure TgmColorGradientManager.Add(const AColorGradient: TgmColorGradient);
begin
  if Assigned(AColorGradient) then
  begin
    FColorGradientList.Add(AColorGradient);
    FModified := True;
  end;
end; { Add }

procedure TgmColorGradientManager.ResetGradients;
begin
  LoadInternalColorGradients;

  FResetted := True;
  FModified := False;
  FFileName := '';
end; { ResetGradients }

procedure TgmColorGradientManager.DeselectAllGradients;
var
  i             : Integer;
  LColorGradient: TgmColorGradient;
begin
  if FColorGradientList.Count > 0 then
  begin
    for i := 0 to (FColorGradientList.Count - 1) do
    begin
      LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

      LColorGradient.IsSelected := False;
    end;
  end;
end; { DeselectAllGradients }

// Set width of gallery map by specifying how many gradient columns in a row.
// Note that, the size calculation will according to the size of
// large gradient thumbnail.
procedure TgmColorGradientManager.SetGalleryMapWidth(
  const ALargeColumnCount: Integer);
begin
  if ALargeColumnCount > 0 then
  begin
    FGradientGalleryMap.Width := LARGE_THUMBNAIL_SIZE * ALargeColumnCount + 1;
  end;
end; { SetGalleryMapWidth }

procedure TgmColorGradientManager.DrawGradientGalleryMap(
  const ADestBmp: TBitmap32; const AMarkSelectedGradient: Boolean = False);
const
  GRADIENT_OFFSET = 3;
var
  LIndex, i, j          : Integer;
  LIPixel, LJPixel      : Integer;
  LGradientBmpSize      : Integer;
  LColorGradient        : TgmColorGradient;
  LGradientBitmap       : TBitmap32;
  LStartPoint, LEndPoint: TPoint;
  LBorderRect           : TRect;
  LCheckerboardRect     : TRect;
begin
  RefreshGalleryMapHeight;

  LGradientBitmap := TBitmap32.Create;
  try
    LGradientBmpSize := FThumbnailSize - 2 * GRADIENT_OFFSET;

    LGradientBitmap.DrawMode := dmBlend;
    LGradientBitmap.SetSize(LGradientBmpSize, LGradientBmpSize);

    FGradientGalleryMap.Clear( Color32(clBtnFace) );

    LStartPoint.X  := Round(LGradientBitmap.Width  * 0.2);
    LStartPoint.Y  := Round(LGradientBitmap.Height * 0.2);
    LEndPoint.X    := Round(LGradientBitmap.Width  * 0.8);
    LEndPoint.Y    := Round(LGradientBitmap.Height * 0.8);

    for LIndex := 0 to (FColorGradientList.Count - 1) do
    begin
      LColorGradient := TgmColorGradient(FColorGradientList.Items[LIndex]);

      DrawLinearGradient(LGradientBitmap, LStartPoint, LEndPoint,
                         LColorGradient, False);

      i := LIndex mod FColumnCount; // column
      j := LIndex div FColumnCount; // row

      LIPixel := i * FThumbnailSize;
      LJPixel := j * FThumbnailSize;

      LBorderRect := Rect(LIPixel, LJPixel,
                          LIPixel + FThumbnailSize + 1,
                          LJPixel + FThumbnailSize);

      LCheckerboardRect := Rect(LIPixel + GRADIENT_OFFSET,
                                LJPixel + GRADIENT_OFFSET,
                                LIPixel + FThumbnailSize - GRADIENT_OFFSET + 1,
                                LJPixel + FThumbnailSize - GRADIENT_OFFSET);

      if AMarkSelectedGradient and LColorGradient.IsSelected then
      begin
        FGradientGalleryMap.FillRectS(LBorderRect, clBlack32);
      end;
      
      DrawCheckerboardPattern(FGradientGalleryMap, LCheckerboardRect, True);

      FGradientGalleryMap.Draw(LCheckerboardRect,
                               LGradientBitmap.Canvas.ClipRect,
                               LGradientBitmap);

      // draw border
      FGradientGalleryMap.FrameRectS(LBorderRect, clBlack32);
    end;

    ADestBmp.Assign(FGradientGalleryMap);
  finally
    LGradientBitmap.Free;
  end;
end; { DrawGradientGalleryMap }

function TgmColorGradientManager.Draw(const AGradientIndex: Integer;
  const ABitmap: TBitmap32; const ARect: TRect): Boolean;
const
  GRADIENT_OFFSET = 3;
var
  LColorGradient   : TgmColorGradient;
  LGradientBmp     : TBitmap32;
  LStartPoint      : TPoint;
  LEndPoint        : TPoint;
  LRect            : TRect;
  LCheckerboardRect: TRect;
  LDoubleOffset    : Integer;
  LRectWidth       : Integer;
  LRectHeight      : Integer;
  LDrawingX        : Integer;
  LDrawingY        : Integer;
begin
  Result := False;
  
  if not Assigned(ABitmap) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Target bitmap is nil.';
    Exit;
  end;

  if (AGradientIndex < 0) or (AGradientIndex >= FColorGradientList.Count) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Index is out of range.';
    Exit;
  end;

  if (ARect.Top = ARect.Bottom) or (ARect.Left = ARect.Right) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Invalid rect.';
    Exit;
  end;

  if ARect.Top > ARect.Bottom then
  begin
    LRect.Top    := ARect.Bottom;
    LRect.Bottom := ARect.Top;
  end
  else
  begin
    LRect.Top    := ARect.Top;
    LRect.Bottom := ARect.Bottom;
  end;

  if ARect.Left > ARect.Right then
  begin
    LRect.Left  := ARect.Right;
    LRect.Right := ARect.Left;
  end
  else
  begin
    LRect.Left  := ARect.Left;
    LRect.Right := ARect.Right;
  end;

  LRectWidth    := LRect.Right - LRect.Left;
  LRectHeight   := LRect.Bottom - LRect.Top;
  LDoubleOffset := 2 * GRADIENT_OFFSET;

  LGradientBmp := TBitmap32.Create;
  try
    LGradientBmp.DrawMode := dmBlend;
    LGradientBmp.SetSize(LRectWidth - LDoubleOffset, LRectHeight - LDoubleOffset - 1);

    // get gradient map
    LStartPoint.X  := Round(LGradientBmp.Width  * 0.2);
    LStartPoint.Y  := Round(LGradientBmp.Height * 0.2);
    LEndPoint.X    := Round(LGradientBmp.Width  * 0.8);
    LEndPoint.Y    := Round(LGradientBmp.Height * 0.8);

    LColorGradient := TgmColorGradient(FColorGradientList.Items[AGradientIndex]);

    DrawLinearGradient(LGradientBmp, LStartPoint, LEndPoint,
                       LColorGradient, False);

    // draw the result
    ABitmap.FillRectS( LRect, Color32(clBtnFace) );

    LDrawingX := LRect.Left + GRADIENT_OFFSET;
    LDrawingY := LRect.Top  + GRADIENT_OFFSET;
    
    LCheckerboardRect := Rect(LDrawingX, LDrawingY,
                              LDrawingX + LGradientBmp.Width,
                              LDrawingY + LGradientBmp.Height);

    DrawCheckerboardPattern(ABitmap, LCheckerboardRect, True);
    ABitmap.Draw(LDrawingX, LDrawingY, LGradientBmp);
    
    // draw border
    ABitmap.FrameRectS(LRect, clBlack32);

    Result := True;
  finally
    LGradientBmp.Free;
  end;
end; { Draw }

function TgmColorGradientManager.Draw(const AGradientIndex: Integer;
  const ACanvas: TCanvas; const ARect: TRect): Boolean;
const
  GRADIENT_OFFSET = 3;
var
  LColorGradient: TgmColorGradient;
  LGradientBmp  : TBitmap32;
  LFinalBmp     : TBitmap32;
  LWinBmp       : TBitmap;
  LStartPoint   : TPoint;
  LEndPoint     : TPoint;
  LRect         : TRect;
  LDoubleOffset : Integer;
  LRectWidth    : Integer;
  LRectHeight   : Integer;
begin
  Result := False;
  
  if not Assigned(ACanvas) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Canvas is nil.';
    Exit;
  end;

  if (AGradientIndex < 0) or (AGradientIndex >= FColorGradientList.Count) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Index is out of range.';
    Exit;
  end;

  if (ARect.Top = ARect.Bottom) or (ARect.Left = ARect.Right) then
  begin
    FOutputMsg := 'TgmColorGradientManager.Draw() -- Error: Invalid rect.';
    Exit;
  end;

  if ARect.Top > ARect.Bottom then
  begin
    LRect.Top    := ARect.Bottom;
    LRect.Bottom := ARect.Top;
  end
  else
  begin
    LRect.Top    := ARect.Top;
    LRect.Bottom := ARect.Bottom;
  end;

  if ARect.Left > ARect.Right then
  begin
    LRect.Left  := ARect.Right;
    LRect.Right := ARect.Left;
  end
  else
  begin
    LRect.Left  := ARect.Left;
    LRect.Right := ARect.Right;
  end;

  LRectWidth    := LRect.Right - LRect.Left;
  LRectHeight   := LRect.Bottom - LRect.Top;
  LDoubleOffset := 2 * GRADIENT_OFFSET;

  LGradientBmp := TBitmap32.Create;
  LFinalBmp    := TBitmap32.Create;
  LWinBmp      := TBitmap.Create;
  try
    LGradientBmp.DrawMode := dmBlend;
    LGradientBmp.SetSize(LRectWidth - LDoubleOffset, LRectHeight - LDoubleOffset - 1);
    
    LFinalBmp.SetSizeFrom(LGradientBmp);

    LWinBmp.Width       := LGradientBmp.Width;
    LWinBmp.Height      := LGradientBmp.Height;
    LWinBmp.PixelFormat := pf24bit;

    // draw gradient
    LStartPoint.X  := Round(LGradientBmp.Width  * 0.2);
    LStartPoint.Y  := Round(LGradientBmp.Height * 0.2);
    LEndPoint.X    := Round(LGradientBmp.Width  * 0.8);
    LEndPoint.Y    := Round(LGradientBmp.Height * 0.8);

    LColorGradient := TgmColorGradient(FColorGradientList.Items[AGradientIndex]);

    DrawLinearGradient(LGradientBmp, LStartPoint, LEndPoint,
                       LColorGradient, False);

    // draw final bitmap
    DrawCheckerboardPattern(LFinalBmp, True);
    LFinalBmp.Draw(0, 0, LGradientBmp);

    LWinBmp.Assign(LFinalBmp);

    // fill out the background
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clBtnFace;

    ACanvas.FillRect(ACanvas.ClipRect);
    ACanvas.Draw(GRADIENT_OFFSET, GRADIENT_OFFSET, LWinBmp);
    
    // draw border
    ACanvas.Brush.Color := clBlack;
    ACanvas.FrameRect(ACanvas.ClipRect);

    Result := True;
  finally
    LWinBmp.Free;
    LFinalBmp.Free;
    LGradientBmp.Free;
  end;
end; { Draw }

function TgmColorGradientManager.GetGradientCount: Integer;
begin
  Result := FColorGradientList.Count;
end; { GetGradientCount }

function TgmColorGradientManager.GetGradientIndexByCoord(
  const AX, AY: Integer): Integer;
var
  LRow, LColumn: Integer;
begin
  LRow := Trunc(AY / FThumbnailSize);

  if (AX mod FThumbnailSize) <> 0 then
  begin
    LColumn := Trunc(AX / FThumbnailSize) + 1;
  end
  else
  begin
    LColumn := AX div FThumbnailSize;
  end;

  Result := LRow * FColumnCount + LColumn - 1;
end; { GetGradientIndexByCoord }

function TgmColorGradientManager.GetGradientByteIndex(
  const AIndex: Integer): TgmColorGradient;
begin
  Result := nil;

  if (AIndex >= 0) and (AIndex < FColorGradientList.Count) then
  begin
    Result := TgmColorGradient(FColorGradientList.Items[AIndex]);
  end;
end; { GetGradientByteIndex }

function TgmColorGradientManager.CopyGradientByIndex(
  const AIndex: Integer): TgmColorGradient;
var
  LColorGradient: TgmColorGradient;
begin
  Result := nil;

  if (AIndex >= 0) and (AIndex < FColorGradientList.Count) then
  begin
    LColorGradient := TgmColorGradient(FColorGradientList.Items[AIndex]);
    Result         := TgmColorGradient.Create;

    Result.Assign(LColorGradient);
  end;
end; { CopyGradientByIndex }

function TgmColorGradientManager.LoadFromFile(
  const AFileName: string): Boolean;
var
  LInputStream: TMemoryStream;
begin
  Result := False;
  
  if (AFileName = '') or
     ( not FileExists(AFileName) ) then
  begin
    FOutputMsg := 'The file is not exists.';
    Exit;
  end;

  LInputStream := TMemoryStream.Create;
  try
    LInputStream.LoadFromFile(AFileName);
    
    Result := LoadFromStream(LInputStream);

    if Result then
    begin
      FFileName := AFileName;  // remember the opened file name
    end;
  finally
    LInputStream.Clear;
    LInputStream.Free;
  end;
end; { LoadFromFile }

function TgmColorGradientManager.LoadFromStream(
  const AStream: TStream): Boolean;
var
  LFileHeader   : TgmGradientFileHeader;
  LOldFileHeader: TgmOldGradientFileHeader;
begin
  Result := False;

  if Assigned(AStream) and (AStream.Size > 0) then
  begin
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
        FOutputMsg := 'Cannot open the file because the file version is high.';
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
        FOutputMsg := 'Cannot open because the file is not supported by GraphicsMagic.';
      end;
    end;
  end;

  if Result then
  begin
    FResetted := False;
    FModified := True;
    
    ResetForeAndBackColorToGradients;
  end;
end; { LoadFromStream }

function TgmColorGradientManager.ReplaceGradients(
  const AFileName: string): Boolean;
begin
  Result := False;

  if (AFileName = '') or
     ( not FileExists(AFileName) ) then
  begin
    FOutputMsg := 'The file is not exists.';
    Exit;
  end;

  FColorGradientList.Clear;
  Result := Self.LoadFromFile(AFileName);

  if Result then
  begin
    FModified := False;
  end;
end; { ReplaceGradients }

function TgmColorGradientManager.DeleteSelectedGradient: Boolean;
var
  i             : Integer;
  LColorGradient: TgmColorGradient;
begin
  Result := False;

  if FColorGradientList.Count > 0 then
  begin
    for i := 0 to (FColorGradientList.Count - 1) do
    begin
      LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

      if LColorGradient.IsSelected then
      begin
        FColorGradientList.Delete(i);

        FModified := True;
        Result    := True;

        Break;
      end;
    end;
  end;
end; { DeleteSelectedGradient }

function TgmColorGradientManager.RenameSelectedGradient(
  const AName: string): Boolean;
var
  i             : Integer;
  LColorGradient: TgmColorGradient;
begin
  Result := False;

  if FColorGradientList.Count > 0 then
  begin
    for i := 0 to (FColorGradientList.Count - 1) do
    begin
      LColorGradient := TgmColorGradient(FColorGradientList.Items[i]);

      if LColorGradient.IsSelected then
      begin
        LColorGradient.Name := AName;
        FModified           := True;
        Result              := True;

        Break;
      end;
    end;
  end;
end; { RenameSelectedGradient }

end.
