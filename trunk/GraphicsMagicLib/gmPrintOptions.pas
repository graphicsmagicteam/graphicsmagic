{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmPrintOptions;

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

interface

uses
  Windows, Graphics, GR32;

type
  TgmPrintOptions = class(TObject)
  private
    FSourceBitmap: TBitmap;
    FPaper       : TBitmap32;  // Represent printer's paper.
    FCenterImage : Boolean;
    FFitToMedia  : Boolean;
    FScale       : Double;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;

    function GetScale: Double;

    function GetLeftInInch: Double;
    function GetTopInInch: Double;
    function GetLeftInCM: Double;
    function GetTopInCM: Double;
    function GetLeftInPoint: Double;
    function GetTopInPoint: Double;
    function GetLeftInPixel: Integer;
    function GetTopInPixel: Integer;
    function GetRightInPixel: Integer;
    function GetBottomInPixel: Integer;

    function GetWidthInInch: Double;
    function GetHeightInInch: Double;
    function GetWidthInCM: Double;
    function GetHeightInCM: Double;
    function GetWidthInPoint: Double;
    function GetHeightInPoint: Double;
    function GetWidthInPixel: Integer;
    function GetHeightInPixel: Integer;

    function GetDrawingRect: TRect;

    procedure SetCenterImage(const AValue: Boolean);
    procedure SetScale(const AValue: Double);

    procedure SetLeftInInch(const AValue: Double);
    procedure SetTopInInch(const AValue: Double);
    procedure SetLeftInCM(const AValue: Double);
    procedure SetTopInCM(const AValue: Double);
    procedure SetLeftInPoint(const AValue: Double);
    procedure SetTopInPoint(const AValue: Double);
    procedure SetLeftInPixel(const AValue: Integer);
    procedure SetTopInPixel(const AValue: Integer);

    procedure SetWidthInInch(const AValue: Double);
    procedure SetHeightInInch(const AValue: Double);
    procedure SetWidthInCM(const AValue: Double);
    procedure SetHeightInCM(const AValue: Double);
    procedure SetWidthInPoint(const AValue: Double);
    procedure SetHeightInPoint(const AValue: Double);
    procedure SetWidthInPixel(const AValue: Integer);
    procedure SetHeightInPixel(const AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalculateDrawingCoord;
    procedure SetFitToMedia(const AValue: Boolean);
    procedure SetPaperSize(const ANewWidth, ANewHeight: Integer);
    procedure DrawPrintBitmap(ADestBmp: TBitmap32);

    property SourceBitmap : TBitmap   read FSourceBitmap;
    property Paper        : TBitmap32 read FPaper;
    property IsCenterImage: Boolean   read FCenterImage     write SetCenterImage;
    property IsFitToMedia : Boolean   read FFitToMedia      write SetFitToMedia;
    property Scale        : Double    read GetScale         write SetScale;

    property LeftInch   : Double      read GetLeftInInch    write SetLeftInInch;
    property TopInch    : Double      read GetTopInInch     write SetTopInInch;
    property LeftCM     : Double      read GetLeftInCM      write SetLeftInCM;
    property TopCM      : Double      read GetTopInCM       write SetTopInCM;
    property LeftPoint  : Double      read GetLeftInPoint   write SetLeftInPoint;
    property TopPoint   : Double      read GetTopInPoint    write SetTopInPoint;
    property LeftPixel  : Integer     read GetLeftInPixel   write SetLeftInPixel;
    property TopPixel   : Integer     read GetTopInPixel    write SetTopInPixel;
    property RightPixel : Integer     read GetRightInPixel;
    property BottomPixel: Integer     read GetBottomInPixel;

    property WidthInch  : Double      read GetWidthInInch   write SetWidthInInch;
    property HeightInch : Double      read GetHeightInInch  write SetHeightInInch;
    property WidthCM    : Double      read GetWidthInCM     write SetWidthInCM;
    property HeightCM   : Double      read GetHeightInCM    write SetHeightInCM;
    property WidthPoint : Double      read GetWidthInPoint  write SetWidthInPoint;
    property HeightPoint: Double      read GetHeightInPoint write SetHeightInPoint;
    property WidthPixel : Integer     read GetWidthInPixel  write SetWidthInPixel;
    property HeightPixel: Integer     read GetHeightInPixel write SetHeightInPixel;
  end;

implementation

uses
{ Standard }
  Types, Math,
{ external components }
  Preview,
{ GraphicsMagic Lib }
  gmMath, gmGUIFuncs;
  

function TgmPrintOptions.GetScale: Double;
begin
  Result := RoundTo(FScale * 100, -2);
end;

function TgmPrintOptions.GetLeftInInch: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FStartPoint.X, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmPrintOptions.GetTopInInch: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FStartPoint.Y, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmPrintOptions.GetLeftInCM: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FStartPoint.X, LDPI, mmPixel, mmLoMetric) / 100;
end;

function TgmPrintOptions.GetTopInCM: Double;
var
  LDPI: Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  Result := ConvertUnits(FStartPoint.Y, LDPI, mmPixel, mmLoMetric) / 100;
end;

function TgmPrintOptions.GetLeftInPoint: Double;
var
  LInches: Double;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LInches := ConvertUnits(FStartPoint.X, LDPI, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

function TgmPrintOptions.GetTopInPoint: Double;
var
  LInches: Double;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LInches := ConvertUnits(FStartPoint.Y, LDPI, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

function TgmPrintOptions.GetLeftInPixel: Integer;
begin
  Result := FStartPoint.X;
end;

function TgmPrintOptions.GetTopInPixel: Integer;
begin
  Result := FStartPoint.Y;
end; 

function TgmPrintOptions.GetRightInPixel: Integer;
begin
  Result := FEndPoint.X;
end;

function TgmPrintOptions.GetBottomInPixel: Integer;
begin
  Result := FEndPoint.Y;
end; 

function TgmPrintOptions.GetWidthInInch: Double;
var
  LWidth: Integer;
  LDPI  : Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  LWidth := Abs(FEndPoint.X - FStartPoint.X);
  Result := ConvertUnits(LWidth, LDPI, mmPixel, mmLoEnglish) / 100;
end;

function TgmPrintOptions.GetHeightInInch: Double;
var
  LHeight: Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LHeight := Abs(FEndPoint.Y - FStartPoint.Y);
  Result  := ConvertUnits(LHeight, LDPI, mmPixel, mmLoEnglish) / 100;
end; 

function TgmPrintOptions.GetWidthInCM: Double;
var
  LWidth: Integer;
  LDPI  : Integer;
begin
  LDPI   := GetScreenPixelsPerInch;
  LWidth := Abs(FEndPoint.X - FStartPoint.X);
  Result := ConvertUnits(LWidth, LDPI, mmPixel, mmLoMetric) / 100;
end;

function TgmPrintOptions.GetHeightInCM: Double;
var
  LHeight: Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LHeight := Abs(FEndPoint.Y - FStartPoint.Y);
  Result  := ConvertUnits(LHeight, LDPI, mmPixel, mmLoMetric) / 100;
end; 

function TgmPrintOptions.GetWidthInPoint: Double;
var
  LInches: Double;
  LWidth : Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LWidth  := Abs(FEndPoint.X - FStartPoint.X);
  LInches := ConvertUnits(LWidth, LDPI, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

function TgmPrintOptions.GetHeightInPoint: Double;
var
  LInches: Double;
  LHeight: Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LHeight := Abs(FEndPoint.Y - FStartPoint.Y);
  LInches := ConvertUnits(LHeight, LDPI, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end; 

function TgmPrintOptions.GetWidthInPixel: Integer;
begin
  Result := Abs(FEndPoint.X - FStartPoint.X);
end;

function TgmPrintOptions.GetHeightInPixel: Integer;
begin
  Result := Abs(FEndPoint.Y - FStartPoint.Y);
end;

function TgmPrintOptions.GetDrawingRect: TRect;
begin
  Result := Rect(FStartPoint.X, FStartPoint.Y, FEndPoint.X, FEndPoint.Y);
end;

procedure TgmPrintOptions.SetCenterImage(const AValue: Boolean);
begin
  if FCenterImage <> AValue then
  begin
    FCenterImage := AValue;
  end;
end; 

procedure TgmPrintOptions.SetFitToMedia(const AValue: Boolean);
begin
  if FFitToMedia <> AValue then
  begin
    FFitToMedia := AValue;
  end;

  if FFitToMedia then
  begin
    FScale := GetScaleToFitMedia(FSourceBitmap.Width, FSourceBitmap.Height,
                                 FPaper.Width, FPaper.Height);
  end;
end; 

procedure TgmPrintOptions.SetScale(const AValue: Double);
var
  LRValue: Double;
begin
  LRValue := AValue / 100;

  if LRValue <= 10 then
  begin
    if FScale <> LRValue then
    begin
      FScale := LRValue;
    end;
  end;
end; 

procedure TgmPrintOptions.SetLeftInInch(const AValue: Double);
var
  LLoEnglishValue: Integer;
  LDPI           : Integer;
begin
  LDPI            := GetScreenPixelsPerInch;
  LLoEnglishValue := Round(AValue * 100);
  FStartPoint.X   := ConvertUnits(LLoEnglishValue, LDPI, mmLoEnglish, mmPixel);
end; 

procedure TgmPrintOptions.SetTopInInch(const AValue: Double);
var
  LLoEnglishValue: Integer;
  LDPI           : Integer;
begin
  LDPI            := GetScreenPixelsPerInch;
  LLoEnglishValue := Round(AValue * 100);
  FStartPoint.Y   := ConvertUnits(LLoEnglishValue, LDPI, mmLoEnglish, mmPixel);
end; 

procedure TgmPrintOptions.SetLeftInCM(const AValue: Double);
var
  LLoMetricValue: Integer;
  LDPI          : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LLoMetricValue := Round(AValue * 100);
  FStartPoint.X  := ConvertUnits(LLoMetricValue, LDPI, mmLoMetric, mmPixel);
end; 

procedure TgmPrintOptions.SetTopInCM(const AValue: Double);
var
  LLoMetricValue: Integer;
  LDPI          : Integer;
begin
  LDPI           := GetScreenPixelsPerInch;
  LLoMetricValue := Round(AValue * 100);
  FStartPoint.Y  := ConvertUnits(LLoMetricValue, LDPI, mmLoMetric, mmPixel);
end; 

procedure TgmPrintOptions.SetLeftInPoint(const AValue: Double);
var
  LInches        : Double;
  LLoEnglishValue: Integer;
  LDPI           : Integer;
begin
  LDPI            := GetScreenPixelsPerInch;
  LInches         := AValue / 72;
  LLoEnglishValue := Round(LInches * 100);
  FStartPoint.X   := ConvertUnits(LLoEnglishValue, LDPI, mmLoEnglish, mmPixel);
end;

procedure TgmPrintOptions.SetTopInPoint(const AValue: Double);
var
  LInches        : Double;
  LLoEnglishValue: Integer;
  LDPI           : Integer;
begin
  LDPI            := GetScreenPixelsPerInch;
  LInches         := AValue / 72;
  LLoEnglishValue := Round(LInches * 100);
  FStartPoint.Y   := ConvertUnits(LLoEnglishValue, LDPI, mmLoEnglish, mmPixel);
end; 

procedure TgmPrintOptions.SetLeftInPixel(const AValue: Integer);
begin
  FStartPoint.X := AValue;
end;

procedure TgmPrintOptions.SetTopInPixel(const AValue: Integer);
begin
  FStartPoint.Y := AValue;
end; 

procedure TgmPrintOptions.SetWidthInInch(const AValue: Double);
var
  LRScale: Double;
  LWidth : Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LWidth  := Round(AValue * 100); // convert to mmLoEnglish unit
  LWidth  := ConvertUnits(LWidth, LDPI, mmLoEnglish, mmPixel);
  LRScale := LWidth / FSourceBitmap.Width;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end;

procedure TgmPrintOptions.SetHeightInInch(const AValue: Double);
var
  LRScale: Double;
  LHeight: Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LHeight := Round(AValue * 100); // convert to mmLoEnglish unit
  LHeight := ConvertUnits(LHeight, LDPI, mmLoEnglish, mmPixel);
  LRScale := LHeight / FSourceBitmap.Height;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetWidthInCM(const AValue: Double);
var
  LRScale: Double;
  LWidth : Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LWidth  := Round(AValue * 100); // convert to mmLoMetric unit
  LWidth  := ConvertUnits(LWidth, LDPI, mmLoMetric, mmPixel);
  LRScale := LWidth / FSourceBitmap.Width;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetHeightInCM(const AValue: Double);
var
  LRScale: Double;
  LHeight: Integer;
  LDPI   : Integer;
begin
  LDPI    := GetScreenPixelsPerInch;
  LHeight := Round(AValue * 100); // convert to mmLoMetric unit
  LHeight := ConvertUnits(LHeight, LDPI, mmLoMetric, mmPixel);
  LRScale := LHeight / FSourceBitmap.Height;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetWidthInPoint(const AValue: Double);
var
  LRScale              : Double;
  LWidth, LLoEnglishVal: Integer;
  LDPI                 : Integer;
begin
  LDPI          := GetScreenPixelsPerInch;
  LLoEnglishVal := Round(AValue / 72 * 100); // convert to mmLoEnglish unit
  LWidth        := ConvertUnits(LLoEnglishVal, LDPI, mmLoEnglish, mmPixel);
  LRScale       := LWidth / FSourceBitmap.Width;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetHeightInPoint(const AValue: Double);
var
  LRScale               : Double;
  LHeight, LLoEnglishVal: Integer;
  LDPI                  : Integer;
begin
  LDPI          := GetScreenPixelsPerInch;
  LLoEnglishVal := Round(AValue / 72 * 100); // convert to mmLoEnglish unit
  LHeight       := ConvertUnits(LLoEnglishVal, LDPI, mmLoEnglish, mmPixel);
  LRScale       := LHeight / FSourceBitmap.Height;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetWidthInPixel(const AValue: Integer);
var
  LRScale: Double;
begin
  LRScale := AValue / FSourceBitmap.Width;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end; 

procedure TgmPrintOptions.SetHeightInPixel(const AValue: Integer);
var
  LRScale: Double;
begin
  LRScale := AValue / FSourceBitmap.Height;

  if LRScale <= 10 then
  begin
    FScale := LRScale;
  end;
end;

procedure TgmPrintOptions.CalculateDrawingCoord;
var
  LBitmapWidth, LBitmapHeight: Integer;
begin
  LBitmapWidth  := Round(FSourceBitmap.Width  * FScale);
  LBitmapHeight := Round(FSourceBitmap.Height * FScale);

  if FCenterImage then
  begin
    FStartPoint.X := FPaper.Width  div 2 - LBitmapWidth  div 2;
    FStartPoint.Y := FPaper.Height div 2 - LBitmapHeight div 2;
  end;

  FEndPoint.X := FStartPoint.X + LBitmapWidth;
  FEndPoint.Y := FStartPoint.Y + LBitmapHeight;
end; 

constructor TgmPrintOptions.Create;
begin
  inherited Create;

  FPaper        := TBitmap32.Create;
  FSourceBitmap := TBitmap.Create;
  FCenterImage  := True;
  FFitToMedia   := False;
  FScale        := 1.0;
  FStartPoint   := GR32.Point(0, 0);
  FEndPoint     := GR32.Point(0, 0);
end; 

destructor TgmPrintOptions.Destroy;
begin
  FPaper.Free;
  FSourceBitmap.Free;
  
  inherited Destroy;
end; 

procedure TgmPrintOptions.SetPaperSize(const ANewWidth, ANewHeight: Integer);
begin
  FPaper.SetSize(ANewWidth, ANewHeight);
  FPaper.Clear(clWhite32);
end; 

procedure TgmPrintOptions.DrawPrintBitmap(ADestBmp: TBitmap32);
var
  r: TRect;
begin
  CalculateDrawingCoord;

  r := GetDrawingRect;
  
  ADestBmp.Assign(FPaper);
  ADestBmp.Canvas.StretchDraw(r, FSourceBitmap);
end; 

end.
