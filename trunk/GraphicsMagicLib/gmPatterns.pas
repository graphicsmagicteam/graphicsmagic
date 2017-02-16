unit gmPatterns;

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

// Update Date: 2017/01/23

{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, SysUtils, Classes, Graphics,
{ Graphics32 }
  GR32, GR32_Image,
{ GtaphicsMagic Lib }
  gmTypes;


type
//-- TgmPattern ----------------------------------------------------------------

  TgmPattern = class(TObject)
  private
    FBitmap  : TBitmap32;
    FName    : string;
    FSizeInfo: string;
    FSelected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Bitmap    : TBitmap32 read FBitmap;
    property Name      : string    read FName     write FName;
    property SizeInfo  : string    read FSizeInfo write FSizeInfo;
    property IsSelected: Boolean   read FSelected write FSelected;
  end;

//-- TgmPatternList ------------------------------------------------------------

  TgmPatternList = class(TList)
  private
    FThumbnailSize         : Integer;
    FRowCount, FColumnCount: Integer;
    FThumbnailSizeMode     : TgmThumbnailSizeMode;
    FSelectedPattern       : TgmPattern;
    FSelectedIndex         : Integer;
    FPatternStage          : TBitmap32;
    FModified              : Boolean;
    FUsingInternalPatterns : Boolean;
    FFileName              : string;
    FOutputMsg             : string;  // output msg, such as errors, etc.
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadInternalPatternsToList;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);
    
    procedure DeleteAllPatterns;
    procedure DeleteSelectedPattern;
    procedure ChangePatternThumbnailSize(const AMode: TgmThumbnailSizeMode);
    procedure GetColumnCount;
    procedure GetRowCount;
    procedure ChangePatternStageHeight;
    procedure DrawPatternStage(const AShowingStage: TCustomImage32);
    procedure DrawPatternBorder(const AShowingStage: TCustomImage32);

    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(const AStream: TStream): Boolean;
    function SelectPatternByIndex(const AIndex: Integer): Boolean;
    function GetPatternIndex(const AX, AY: Integer): Integer;
    function GetPatternInfo(const AX, AY: Integer): string;

    property SelectedPattern  : TgmPattern           read FSelectedPattern;
    property SelectedIndex    : Integer              read FSelectedIndex;
    property ThumbnailSizeMode: TgmThumbnailSizeMode read FThumbnailSizeMode;
    property IsModified       : Boolean              read FModified write FModified;
    property IsUsingInternal  : Boolean              read FUsingInternalPatterns;
    property FileName         : string               read FFileName;
    property OutputMsg        : string               read FOutputMsg;
  end;

const
  MAX_PATTERN_COUNT: Integer = 42;

implementation

uses
{ GraphicsMagic Lib }
  CommonDataModule,
  gmGUIFuncs,
  gmImageProcessFuncs,
  gmPaintFuncs;

type
  TgmPatternFileHeader = record
    FileID       : Cardinal; // must be $474D5046
    FileVersion  : Cardinal; // version of the file format
    PatternCount : Cardinal; // indicating how many patterns are in this file
  end;

  // info about each pattern in the file
  TgmPatternInfoHeader = record
    Width  : Cardinal;
    Height : Cardinal;
    Size   : Cardinal;     // patter memory size in bytes
    Name   : ShortString;
  end;

const
  PATTERN_FILE_ID      = $474D5046; // i.e. GMPF - GraphicsMagic Patterns File
  PATTERN_FILE_VERSION = 1;         // the file version we could process so far


//-- TgmPattern ----------------------------------------------------------------

constructor TgmPattern.Create;
begin
  inherited Create;

  FBitmap          := TBitmap32.Create;
  FBitmap.DrawMode := dmBlend;
  
  FName     := '';
  FSizeInfo := '';
  FSelected := False;
end;

destructor TgmPattern.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

//-- TgmPatternList ------------------------------------------------------------

constructor TgmPatternList.Create;
begin
  inherited Create;

  FRowCount              := 0;
  FColumnCount           := 0;
  FSelectedPattern       := nil;
  FSelectedIndex         := -1;
  FPatternStage          := TBitmap32.Create;
  FPatternStage.DrawMode := dmBlend;
  FPatternStage.Width    := 192;
  FThumbnailSizeMode     := tsmLarge;
  FThumbnailSize         := LARGE_THUMBNAIL_SIZE;
  FModified              := False;
  FUsingInternalPatterns := True;
  FFileName              := '';
  FOutputMsg             := '';
end;

destructor TgmPatternList.Destroy;
begin
  if Assigned(FPatternStage) then
  begin
    FPatternStage.Free;
  end;

  DeleteAllPatterns;
  
  inherited Destroy;
end;

procedure TgmPatternList.LoadInternalPatternsToList;
var
  LPattern: TgmPattern;
  i       : Integer;
begin
  DeleteAllPatterns;

  { The internal pattern bitmaps are stored in GMDataModule wich was defined in
    CommonDataModule.pas, it was type of TGMDataModule that is descendent of
    TDataModule. }
    
  GMDataModule := TGMDataModule.Create(nil);
  try
    for i := 0 to (INTERNAL_PATTERN_COUNT - 1) do
    begin
      LPattern := TgmPattern.Create;

      // read pattern name
      LPattern.Name := INTERNAL_PATTERN_NAME[i];

      // read bitmap
      LPattern.Bitmap.Assign(GMDataModule.bmp32lstInternalPatterns.Bitmap[i]);

      LPattern.SizeInfo := '(' + IntToStr(LPattern.Bitmap.Width) + 'x' +
                           IntToStr(LPattern.Bitmap.Height) + ')';

      Self.Add(LPattern);  // add pattern to list
    end;
  finally
    FreeAndNil(GMDataModule);
  end;
  
  FUsingInternalPatterns := True;
  FModified              := False;
  FFileName              := '';
end;

function TgmPatternList.LoadFromFile(const AFileName: string): Boolean;
var
  LInputStream : TMemoryStream;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    // if the external file is not exists then loading internal patterns
    LoadInternalPatternsToList;
    Exit;
  end;

  // load data in...
  LInputStream := TMemoryStream.Create;
  try

    try
      LInputStream.LoadFromFile(AFileName);
      LInputStream.Position := 0;

      Result := LoadFromStream(LInputStream);

      if Result then
      begin
        FUsingInternalPatterns := False;
        FModified              := False;
        FFileName              := AFileName;
      end;

    except
      // if any error occurs in loading external files then load the internal data in
      LoadInternalPatternsToList;
      FOutputMsg := 'Cannot open the pattern file.';
    end;
    
  finally
    LInputStream.Free;
  end;
end;

function TgmPatternList.LoadFromStream(const AStream: TStream): Boolean;
var
  LFileHeader        : TgmPatternFileHeader;
  LPatternInfoHeader : TgmPatternInfoHeader;
  LPattern           : TgmPattern;
  LBmpStream         : TMemoryStream;
  LPatternCount      : Integer;
  i                  : Integer;
begin
  Result := False;
  
  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    // check for file ID
    AStream.Read(LFileHeader, SizeOf(TgmPatternFileHeader));

    if LFileHeader.FileID <> PATTERN_FILE_ID then
    begin
      FOutputMsg := 'This file is not a GraphicsMaigc pattern file.';
      Exit;
    end;

    if LFileHeader.FileVersion <> PATTERN_FILE_VERSION then
    begin
      FOutputMsg := 'This pattern file version is not supported by GraphicsMaigc at current version.';
      Exit;
    end;

    if LFileHeader.PatternCount <= 0 then
    begin
      FOutputMsg := 'There is no any pattern in this file.';
      Exit;
    end;

    // load in patterns...
    LPatternCount := LFileHeader.PatternCount;

    if LPatternCount > MAX_PATTERN_COUNT then
    begin
      LPatternCount := MAX_PATTERN_COUNT;
    end;

    Self.DeleteAllPatterns;

    LBmpStream := TMemoryStream.Create;
    try
      for i := 1 to LPatternCount do
      begin
        // read the pattern info header
        AStream.Read(LPatternInfoHeader, SizeOf(TgmPatternInfoHeader));

        LPattern := TgmPattern.Create;

        // read pattern name
        LPattern.Name := LPatternInfoHeader.Name;

        // read bitmap
        LBmpStream.Clear;
        LBmpStream.CopyFrom(AStream, LPatternInfoHeader.Size);
        LBmpStream.Position := 0;

        LPattern.Bitmap.LoadFromStream(LBmpStream);

        LPattern.SizeInfo := '(' + IntToStr(LPattern.Bitmap.Width) + 'x' +
                             IntToStr(LPattern.Bitmap.Height) + ')';

        Self.Add(LPattern);  // add pattern to list
      end;
    finally
      LBmpStream.Free;
    end;

    Result := True;
  end;
end;

procedure TgmPatternList.SaveToFile(const AFileName: string);
var
  LOutputStream : TMemoryStream;
begin
  if (AFileName <> '') and (Self.Count > 0) then
  begin
    LOutputStream := TMemoryStream.Create;
    try
      SaveToStream(LOutputStream);

      if LOutputStream.Size > 0 then
      begin
        LOutputStream.Position := 0;
        LOutputStream.SaveToFile(AFileName);

        FFileName := AFileName;  // remember the new filename
        FModified := False;
      end;
    finally
      LOutputStream.Free;
    end;
  end;
end;

procedure TgmPatternList.SaveToStream(const AStream: TStream);
var
  LFileHeader        : TgmPatternFileHeader;
  LPatternInfoHeader : TgmPatternInfoHeader;
  LBmpStream         : TMemoryStream;
  LPattern           : TgmPattern;
  i                  : Integer;
begin
  if Assigned(AStream) then
  begin
    // write in file header
    LFileHeader.FileID       := PATTERN_FILE_ID;
    LFileHeader.FileVersion  := PATTERN_FILE_VERSION;
    LFileHeader.PatternCount := Self.Count;

    AStream.Write(LFileHeader, SizeOf(TgmPatternFileHeader));

    LBmpStream  := TMemoryStream.Create;
    try
      // write in data of each pattern
      for i := 0 to (Self.Count - 1) do
      begin
        LPattern := TgmPattern(Self.Items[i]);

        // write in pattern data info
        LPattern.Bitmap.SaveToStream(LBmpStream);

        LPatternInfoHeader.Width  := LPattern.Bitmap.Width;
        LPatternInfoHeader.Height := LPattern.Bitmap.Height;
        LPatternInfoHeader.Size   := LBmpStream.Size;
        LPatternInfoHeader.Name   := LPattern.Name;

        // write in pattern info header
        AStream.Write(LPatternInfoHeader, SizeOf(TgmPatternInfoHeader));

        // write in pattern pixels data
        LBmpStream.Position := 0;
        AStream.CopyFrom(LBmpStream, LBmpStream.Size);

        LBmpStream.Clear;
      end;
      
    finally
      LBmpStream.Free;
    end;
  end;
end;

procedure TgmPatternList.DeleteAllPatterns;
var
  i       : Integer;
  LPattern: TgmPattern;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LPattern := TgmPattern(Self.Items[i]);
      LPattern.Free;
    end;

    Self.Clear;
  end;
  
  FSelectedPattern := nil;
  FSelectedIndex   := -1;
end;

procedure TgmPatternList.DeleteSelectedPattern;
begin
  // delete current pattern and select the last one
  Self.Delete(FSelectedIndex);
  
  FSelectedPattern            := Self.Items[Self.Count - 1];
  FSelectedPattern.IsSelected := True;
  FSelectedIndex              := Self.Count - 1;
  FModified                   := True;
end;

procedure TgmPatternList.ChangePatternThumbnailSize(
  const AMode: TgmThumbnailSizeMode);
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
  
  GetColumnCount;
  GetRowCount;
  ChangePatternStageHeight;
end;

procedure TgmPatternList.GetColumnCount;
begin
  FColumnCount := FPatternStage.Width div FThumbnailSize;
end;

procedure TgmPatternList.GetRowCount;
begin
  if (Self.Count mod FColumnCount) > 0 then
  begin
    FRowCount := Trunc(Self.Count / FColumnCount) + 1;
  end
  else
  begin
    FRowCount := Self.Count div FColumnCount;
  end;
end;

procedure TgmPatternList.ChangePatternStageHeight;
begin
  FPatternStage.Height := FRowCount * FThumbnailSize;
end;

procedure TgmPatternList.DrawPatternStage(const AShowingStage: TCustomImage32);
var
  LPatternIndex          : Integer;
  i, j, LIPixel, LJPixel : Integer;
  LHalfWidth, LHalfHeight: Integer;
  LCenterPoint           : TPoint;
  LPattern               : TgmPattern;
  LScaledBitmap          : TBitmap32;
  LRect                  : TRect;
begin
  ChangePatternThumbnailSize(FThumbnailSizeMode);
  
  LScaledBitmap := TBitmap32.Create;
  try
    FPatternStage.Clear($00000000);

    for LPatternIndex := 0 to (Self.Count - 1) do
    begin
      i := LPatternIndex mod FColumnCount; // column
      j := LPatternIndex div FColumnCount; // row

      LIPixel := i * FThumbnailSize;
      LJPixel := j * FThumbnailSize;

      // get each thumbnail's center point
      LCenterPoint := Point(LIPixel + FThumbnailSize div 2,
                            LJPixel + FThumbnailSize div 2);

      LPattern := TgmPattern(Self.Items[LPatternIndex]);

      GetScaledBitmap(LPattern.Bitmap, LScaledBitmap,
                      FThumbnailSize, FThumbnailSize);

      LScaledBitmap.DrawMode := dmBlend;

      // draw checkerboard pattern below tha filling pattern
      LHalfWidth  := LScaledBitmap.Width div 2;
      LHalfHeight := LScaledBitmap.Height div 2;

      LRect := Rect(LCenterPoint.X - LHalfWidth,
                    LCenterPoint.Y - LHalfHeight,
                    LCenterPoint.X + LHalfWidth,
                    LCenterPoint.Y + LHalfHeight);

      DrawCheckerboardPattern(FPatternStage, LRect);

      FPatternStage.Draw(LCenterPoint.X - LScaledBitmap.Width  div 2,
                         LCenterPoint.Y - LScaledBitmap.Height div 2,
                         LScaledBitmap);


      if LPattern.IsSelected then
      begin
        FPatternStage.FrameRectS(LIPixel, LJPixel,
                                 LIPixel + FThumbnailSize,
                                 LJPixel + FThumbnailSize,
                                 clRed32);
      end
      else
      begin
        FPatternStage.FrameRectS(LIPixel, LJPixel,
                                 LIPixel + FThumbnailSize,
                                 LJPixel + FThumbnailSize,
                                 clBlack32);
      end;
    end;

    AShowingStage.Bitmap.Assign(FPatternStage);
  finally
    LScaledBitmap.Free;
  end;
end;

procedure TgmPatternList.DrawPatternBorder(const AShowingStage: TCustomImage32);
var
  i, j, LIndex    : Integer;
  LIPixel, LJPixel: Integer;
  LPattern        : TgmPattern;
begin
  if Self.Count > 0 then
  begin
    for LIndex := 0 to (Self.Count - 1) do
    begin
      i := LIndex mod FColumnCount; // column
      j := LIndex div FColumnCount; // row

      LIPixel := i * FThumbnailSize;
      LJPixel := j * FThumbnailSize;

      LPattern := TgmPattern(Self.Items[LIndex]);

      if LPattern.IsSelected then
      begin
        FPatternStage.FrameRectS(LIPixel, LJPixel,
                                 LIPixel + FThumbnailSize,
                                 LJPixel + FThumbnailSize,
                                 clRed32);
      end
      else
      begin
        FPatternStage.FrameRectS(LIPixel, LJPixel,
                                 LIPixel + FThumbnailSize,
                                 LJPixel + FThumbnailSize,
                                 clBlack32);
      end;
    end;
  end;
  
  AShowingStage.Bitmap.Assign(FPatternStage);
end;

function TgmPatternList.SelectPatternByIndex(const AIndex: Integer): Boolean;
var
  i       : Integer;
  LPattern: TgmPattern;
begin
  Result := False;

  if (Self.Count > 0) then
  begin
    if (AIndex >= 0) and (AIndex < Self.Count) then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LPattern            := Self.Items[i];
        LPattern.IsSelected := False;
      end;
      
      FSelectedPattern            := Self.Items[AIndex];
      FSelectedPattern.IsSelected := True;

      FSelectedIndex := AIndex;

      Result := True;
    end;
  end;
end;

function TgmPatternList.GetPatternIndex(const AX, AY: Integer): Integer;
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
end;

function TgmPatternList.GetPatternInfo(const AX, AY: Integer): string;
var
  LIndex  : Integer;
  LPattern: TgmPattern;
begin
  Result := '';

  if Self.Count > 0 then
  begin
    LIndex := GetPatternIndex(AX, AY);
    
    if (LIndex >= 0) and (LIndex < Self.Count) then
    begin
      LPattern := Self.Items[LIndex];
      Result   := LPattern.Name + ' ' + LPattern.SizeInfo;
    end;
  end;
end;

end.
