{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmSwatches;

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

{$WARN UNSAFE_CAST OFF}

interface

uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls;

type

//-- TgmSwatch -----------------------------------------------------------------

  // a single color information
  TgmSwatch = class(TObject)
  private
    FRed, FGreen, FBlue: Byte;
    FName              : string;
  public
    constructor Create(const R, G, B: Byte; const ASwatchName: string);

    property Red  : Byte   read FRed   write FRed;
    property Green: Byte   read FGreen write FGreen;
    property Blue : Byte   read FBlue  write FBlue;
    property Name : string read FName  write FName;
  end;

//-- TgmSwatchList -------------------------------------------------------------

  // a list for holding and operating swatches
  TgmSwatchList = class(TList)
  private
    FSwatchBitmap          : TBitmap;
    FPatchSize             : Integer;
    FColumnCount           : Integer;
    FRowCount              : Integer;
    FNewColorSwatchCount   : Integer;
    FSelectedSwatch        : TgmSwatch;
    FSelectedIndex         : Integer;
    FUsingInternalSwatches : Boolean;
    FModified              : Boolean;
    FAllowModifyColumnCount: Boolean;
    FFileName              : string;
    FOutputMsg             : string;  // output message, such as errors, etc.

    procedure GetColumnCount;
    procedure GetRowCount;
    procedure AdjustSwatchBMPSize;
    procedure DrawSwatches;
  public
    constructor Create(const ASwatchBMPWidth, ASwatchBMPHeight: Integer);
    destructor Destroy; override;

    procedure LoadInternalSwatchesToList;
    function LoadSwatchesToList(const AFileName: string): Boolean;
    
    procedure SaveSwatchesToFile(const AFileName: string);
    procedure ShowSwatches(AImage: TImage);
    procedure AddSwatch(const ASwatch: TgmSwatch);
    procedure DeleteSelectedSwatch;
    procedure ClearSwatchesInList;
    
    function GetSwatch(const X, Y: Integer): Boolean;
    function GetSwatchIndex(const X, Y: Integer): Integer;

    property NewColorSwatchCount: Integer   read FNewColorSwatchCount;
    property IsUsingInternal    : Boolean   read FUsingInternalSwatches;
    property FileName           : string    read FFileName;
    property SelectedSwatch     : TgmSwatch read FSelectedSwatch write FSelectedSwatch;
    property IsModified         : Boolean   read FModified       write FModified;
    property OutputMsg          : string    read FOutputMsg;
  end;

const
  MAX_SWATCH_COUNT = 500;

implementation

uses
{ externals }
  ColorNameLibrary;

const
  // default values
  DEFAULT_PATCH_SIZE = 15;

//-- TgmSwatch -----------------------------------------------------------------

constructor TgmSwatch.Create(const R, G, B: Byte; const ASwatchName: string);
begin
  inherited Create;

  FRed   := R;
  FGreen := G;
  FBlue  := B;
  FName  := ASwatchName;
end; 

//-- TgmSwatchList -------------------------------------------------------------

constructor TgmSwatchList.Create(
  const ASwatchBMPWidth, ASwatchBMPHeight: Integer);
begin
  inherited Create;

  FSwatchBitmap := TBitmap.Create;
  
  with FSwatchBitmap do
  begin
    PixelFormat        := pf24bit;
    Width              := ASwatchBMPWidth;
    Height             := ASwatchBMPHeight;
    Canvas.Pen.Width   := 1;
    Canvas.Pen.Color   := clBlack;
    Canvas.Pen.Style   := psSolid;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clWhite;
  end;

  FPatchSize              := DEFAULT_PATCH_SIZE;
  FColumnCount            := 0;
  FRowCount               := 0;
  FNewColorSwatchCount    := 0;
  FSelectedSwatch         := nil;
  FUsingInternalSwatches  := True;
  FModified               := False;
  FFileName               := '';
  FSelectedIndex          := -1;
  FAllowModifyColumnCount := True;
  FOutputMsg              := '';
end;

destructor TgmSwatchList.Destroy;
var
  i: Integer;
begin
  // free items in the list
  for i := (Self.Count - 1) downto 0 do
  begin
    Self.Delete(i);
  end;

  FSwatchBitmap.Free;
  
  inherited Destroy;
end;

procedure TgmSwatchList.LoadInternalSwatchesToList;
var
  LSwatch   : TgmSwatch;
  LColor    : TColor;
  LColorName: string;
  i         : Integer;
  r, g, b   : Byte;
begin
  ClearSwatchesInList;

  for i := 1 to 455 do
  begin
    LColorName := HYPEColorNames[i];
    LColor     := HYPEColors[i];
    r          := GetRValue(LColor);
    g          := GetGValue(LColor);
    b          := GetBValue(LColor);
    LSwatch    := TgmSwatch.Create(r, g, b, LColorName);
    
    Self.Add(LSwatch);
  end;

  FUsingInternalSwatches := True;
  FModified              := False;
  FFileName              := '';
  FNewColorSwatchCount   := 0;
end;

function TgmSwatchList.LoadSwatchesToList(const AFileName: string): Boolean;
var
  LSwatch          : TgmSwatch;
  LFile            : TextFile;
  LName, LColorName: string;
  LColorCount, i   : Integer;
  r, g, b          : Integer;
begin
  Result := False;

  if (AFileName <> '') and FileExists(AFileName) then
  begin
    try
      ClearSwatchesInList;
      AssignFile(LFile, AFileName);
      Reset(LFile);
      Readln(LFile, LColorCount);

      if LColorCount > MAX_SWATCH_COUNT then
      begin
        LColorCount := MAX_SWATCH_COUNT;
      end;

      if LColorCount > 0 then
      begin
        for i := 0 to (LColorCount - 1) do
        begin
          Readln(LFile, r, g, b, LName);

          LColorName := Copy( LName, 4, Length(LName) - 3 );
          LSwatch   := TgmSwatch.Create(r, g, b, LColorName);
          
          Self.Add(LSwatch);
        end;

        FUsingInternalSwatches := False;
        FModified              := False;
        FFileName              := AFileName;
        FNewColorSwatchCount   := 0;
      end;

      CloseFile(LFile);

      Result := True;
    except
      CloseFile(LFile);

      // if error in loading external files then load the internal data
      LoadInternalSwatchesToList;

      FOutputMsg := 'Can not open the Swatches file,' + #10#13 +
                    'application will load the internal Swatches data.';
    end;
  end
  else
  begin
    LoadInternalSwatchesToList;
  end;
end;

procedure TgmSwatchList.SaveSwatchesToFile(const AFileName: string);
const
  SPACE3 = '   ';
  SPACE4 = '    ';
  SPACE5 = '     ';
  
var
  LSwatch    : TgmSwatch;
  LFile      : TextFile;
  i          : Integer;
  r, g, b    : Byte;
  LColorName : string;
  LRedSpace  : string;
  LGreenSpace: string;
  LBlueSpace : string;
begin
  if Self.Count > 0 then
  begin
    AssignFile(LFile, AFileName);
    Rewrite(LFile);
    Writeln(LFile, Self.Count);

    for i := 0 to (Self.Count - 1) do
    begin
      LSwatch    := TgmSwatch(Self.Items[i]);
      r          := LSwatch.Red;
      g          := LSwatch.Green;
      b          := LSwatch.Blue;
      LColorName := LSwatch.Name;

      if r < 10 then
      begin
        LRedSpace := SPACE5;
      end
      else
      if (r >= 10) and (r < 100) then
      begin
        LRedSpace := SPACE4;
      end
      else
      begin
        LRedSpace := SPACE3;
      end;

      if g < 10 then
      begin
        LGreenSpace := SPACE5;
      end
      else
      if (g >= 10) and (g < 100) then
      begin
        LGreenSpace := SPACE4;
      end
      else
      begin
        LGreenSpace := SPACE3;
      end;

      if b < 10 then
      begin
        LBlueSpace := SPACE5;
      end
      else
      if (b >= 10) and (b < 100) then
      begin
        LBlueSpace := SPACE4;
      end
      else
      begin
        LBlueSpace := SPACE3;
      end;

      Writeln(LFile, LRedSpace, r, LGreenSpace, g, LBlueSpace, b, SPACE3, LColorName);
    end;

    FFileName            := AFileName;
    FModified            := False;
    FNewColorSwatchCount := 0;
    
    CloseFile(LFile);
  end;
end;

procedure TgmSwatchList.GetColumnCount;
var
  LWidth: Integer;
begin
  if FAllowModifyColumnCount then
  begin
    with FSwatchBitmap.Canvas.ClipRect do
    begin
      LWidth       := Right - Left - 10;
      FColumnCount := LWidth div FPatchSize;
    end;

    FAllowModifyColumnCount := False;
  end;
end;

procedure TgmSwatchList.GetRowCount;
begin
  if (Self.Count mod FColumnCount) <> 0 then
  begin
    FRowCount := Trunc(Self.Count / FColumnCount) + 1;
  end
  else
  begin
    FRowCount := Self.Count div FColumnCount;
  end;
end;

procedure TgmSwatchList.AdjustSwatchBMPSize;
begin
  FSwatchBitmap.Width  := FPatchSize * FColumnCount;
  FSwatchBitmap.Height := FPatchSize * FRowCount;
end;

procedure TgmSwatchList.DrawSwatches;
var
  LPatchIndex, i, j: Integer;
  LIPixel, LJPixel : Integer;
  r, g, b          : Integer;
  LColor           : TColor;
  LSwatch          : TgmSwatch;
begin
  FSwatchBitmap.Canvas.Brush.Color := clBtnFace;
  FSwatchBitmap.Canvas.FillRect(FSwatchBitmap.Canvas.ClipRect);

  if Self.Count = 0 then
  begin
    with FSwatchBitmap do
    begin
      Width             := 160;
      Height            := 50;
      Canvas.Font.Color := clRed;
      
      Canvas.TextOut(0, 0, 'No palette load!');
    end;
  end
  else
  begin
    for LPatchIndex := 0 to (Self.Count - 1) do
    begin
      j := LPatchIndex div FColumnCount;  // row
      i := LPatchIndex mod FColumnCount;  // column

      LIPixel := i * FPatchSize;
      LJPixel := j * FPatchSize;

      LSwatch := TgmSwatch(Self.Items[LPatchIndex]);
      r       := LSwatch.Red;
      g       := LSwatch.Green;
      b       := LSwatch.Blue;
      LColor  := RGB(r, g, b);

      FSwatchBitmap.Canvas.Brush.Color := LColor;

      FSwatchBitmap.Canvas.Rectangle(LIPixel, LJPixel,
                                     LIPixel + FPatchSize,
                                     LJPixel + FPatchSize);
    end;
  end;
end;

procedure TgmSwatchList.ShowSwatches(AImage: TImage);
begin
  GetColumnCount;
  GetRowCount;
  AdjustSwatchBMPSize;
  DrawSwatches;
  
  AImage.Picture.Graphic := FSwatchBitmap;
end;

procedure TgmSwatchList.AddSwatch(const ASwatch: TgmSwatch);
begin
  Add(ASwatch);
  Inc(FNewColorSwatchCount);
  GetColumnCount;
  GetRowCount;
  AdjustSwatchBMPSize;
  
  FModified := True;
end;

procedure TgmSwatchList.DeleteSelectedSwatch;
begin
  if Assigned(FSelectedSwatch) then
  begin
    FreeAndNil(FSelectedSwatch);
    Delete(FSelectedIndex);

    FSelectedIndex := -1;

    GetColumnCount;
    GetRowCount;
    AdjustSwatchBMPSize;
    
    FModified := True;
  end;
end;

procedure TgmSwatchList.ClearSwatchesInList;
begin
  Self.Clear;
  
  FNewColorSwatchCount := 0;
  FSelectedSwatch      := nil;
end;

function TgmSwatchList.GetSwatch(const X, Y: Integer): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  LIndex := GetSwatchIndex(X, Y);

  if (LIndex >= 0) and (LIndex < Self.Count) then
  begin
    FSelectedSwatch := TgmSwatch(Self.Items[LIndex]);
    FSelectedIndex  := LIndex;
    Result          := True;
  end;
end;

function TgmSwatchList.GetSwatchIndex(const X, Y: Integer): Integer;
var
  LRow, LColumn: Integer;
begin
  LRow := Trunc(Y / FPatchSize);

  if (X mod FPatchSize) <> 0 then
  begin
    LColumn := Trunc(X / FPatchSize) + 1;
  end
  else
  begin
    LColumn := X div FPatchSize;
  end;
  
  Result := LRow * FColumnCount + LColumn - 1;
end; 

end.
