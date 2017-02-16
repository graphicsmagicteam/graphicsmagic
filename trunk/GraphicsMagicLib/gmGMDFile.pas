unit gmGMDFile;

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

// Update Date: 2015/11/15

interface

uses
{ Standard }
  Classes,
  ComCtrls,
{ Graphics32 }
  GR32,
{ GraphicsMagic }
  gmChannels,
  gmLayers,
  gmChannelManager,
  gmPaths;           

type

  TgmGMDAfterFileLoadedEvent = procedure (
    ASender: TObject;
    const AFileVersion: Integer;
    const ALayerMaskColor: TColor32;
    const ALayerMaskOpacity: Single;
    const ALayerMaskColorIndicator: TgmMaskColorIndicator) of object; 

  { TgmGMDManager }
  
  TgmGMDManager = class(TObject)
  private
    FMemoryStream    : TMemoryStream;
    FOutputMsg       : string;            // output info, such as errors, etc.
    FLayerList       : TgmLayerList;
    FChannelManager  : TgmCustomChannelManager;
    FPathList        : TgmPathList;
    FTextEditor      : TRichEdit;

    // callbacks ...
    FAfterFileLoaded : TgmGMDAfterFileLoadedEvent;
  public
    constructor Create;
    destructor Destroy; override;

    function CheckFileValidity(const AFileName: string): Boolean;

    function LoadFromFile(const AFileName: string): Boolean;
    procedure SaveToFile(const AFileName: string);

    property OuputMsg          : string                     read FOutputMsg;
    property ChannelManager    : TgmCustomChannelManager    read FChannelManager  write FChannelManager;
    property LayerList         : TgmLayerList               read FLayerList       write FLayerList;
    property OnAfterFileLoaded : TgmGMDAfterFileLoadedEvent read FAfterFileLoaded write FAfterFileLoaded;
    property PathList          : TgmPathList                read FPathList        write FPathList;
    property TextEditor        : TRichEdit                  read FTextEditor      write FTextEditor;
  end;

implementation

uses
{ Standard Lib }
  SysUtils,
{ GraphicsMagic Lib }
  gmBrightContrastLayer,
  gmChannelIO,
  gmChannelMixerLayer,
  gmColorBalanceLayer,
  gmCurvesLayer,
  gmGradientFillLayer,
  gmGradientMapLayer,
  gmHueSaturationLayer,
  gmInvertLayer,
  gmLayerIO,
  gmLevelsLayer,
  gmPathIO,              
  gmPatternLayer,
  gmPosterizeLayer,
  gmShapeRegionLayer,
  gmSolidColorLayer,
  gmThresholdLayer,
  gmVectorLayer;


type
  // main file header (version 1)
  TgmGMDFileHeaderVer1 = record
    FileID                        : Cardinal; // must be $474D4446
    FileVersion                   : Cardinal; // version of the file format

    { Layer Part }
    LayerCount                    : Cardinal; // indicating how many layers in the file
    TransparentLayerNumber        : Cardinal; // accumulated transparent layer number
    ColorFillLayerNumber          : Cardinal;
    ColorBalanceLayerNumber       : Cardinal;
    BrightContrastLayerNumber     : Cardinal;
    HLSLayerNumber                : Cardinal;
    InvertLayerNumber             : Cardinal;
    ThresholdLayerNumber          : Cardinal;
    PosterizeLayerNumber          : Cardinal;
    LevelsLayerNumber             : Cardinal;
    CurvesLayerNumber             : Cardinal;
    GradientMapLayerNumber        : Cardinal;
    GradientFillLayerNumber       : Cardinal;
    PatternLayerNumber            : Cardinal;
    FigureLayerNumber             : Cardinal;
    ShapeRegionLayerNumber        : Cardinal;
    LayerWidth                    : Cardinal;
    LayerHeight                   : Cardinal;

    { Channel Part }
    ChannelCount                  : Cardinal; // indicating how many alpha channels (including Quick Mask channel) in the file
    AlphaChannelNumber            : Cardinal; // accumulated alpha channel number
    GlobalChannelMaskColor        : TColor32;
    GlobalQuickMaskColor          : TColor32;
    GlobalQuickMaskOpacityPercent : Single;
    GlobalQuickMaskColorIndicator : Cardinal;
    GlobalLayerMaskColor          : TColor32;
    GlobalLayerMaskOpacityPercent : Single;

    { Path Part }
    PathPanelCount                : Cardinal; // indicating how many path panels in the file
    PathPanelNumber               : Cardinal; // accumulated path panel number
  end;

  // main file header (version 2)
  TgmGMDFileHeaderVer2 = record
    FileID                        : Cardinal; // must be $474D4446
    FileVersion                   : Cardinal; // version of the file format

    { Layer Part }
    LayerCount                    : Cardinal; // indicating how many layers in the file
    TransparentLayerNumber        : Cardinal; // accumulated transparent layer number
    ColorFillLayerNumber          : Cardinal;
    ColorBalanceLayerNumber       : Cardinal;
    BrightContrastLayerNumber     : Cardinal;
    HLSLayerNumber                : Cardinal;
    InvertLayerNumber             : Cardinal;
    ThresholdLayerNumber          : Cardinal;
    PosterizeLayerNumber          : Cardinal;
    LevelsLayerNumber             : Cardinal;
    CurvesLayerNumber             : Cardinal;
    GradientMapLayerNumber        : Cardinal;
    GradientFillLayerNumber       : Cardinal;
    PatternLayerNumber            : Cardinal;
    FigureLayerNumber             : Cardinal;
    ShapeRegionLayerNumber        : Cardinal;
    ChannelMixerLayerNumber       : Cardinal; // new
    LayerWidth                    : Cardinal;
    LayerHeight                   : Cardinal;

    { Channel Part }
    ChannelCount                  : Cardinal; // indicating how many alpha channels (including Quick Mask channel) in the file
    AlphaChannelNumber            : Cardinal; // accumulated alpha channel number
    GlobalChannelMaskColor        : TColor32;
    GlobalQuickMaskColor          : TColor32;
    GlobalQuickMaskOpacityPercent : Single;
    GlobalQuickMaskColorIndicator : Cardinal;
    GlobalLayerMaskColor          : TColor32;
    GlobalLayerMaskOpacityPercent : Single;

    { Path Part }
    PathPanelCount                : Cardinal; // indicating how many path panels in the file
    PathPanelNumber               : Cardinal; // accumulated path panel number
  end;

  // main file header (version 4)
  TgmGMDFileHeaderVer4 = record
    FileID                        : Cardinal; // must be $474D4446
    FileVersion                   : Cardinal; // version of the file format

    { Layer Part }
    LayerWidth                    : Cardinal;
    LayerHeight                   : Cardinal;
    LayerCount                    : Cardinal; // indicating how many layers in the file
    NormalLayerCounter            : Cardinal; // accumulated normal layer counter
    SolidColorLayerCounter        : Cardinal;
    ColorBalanceLayerCounter      : Cardinal;
    BrightContrastLayerCounter    : Cardinal;
    HueSaturationLayerCounter     : Cardinal;
    InvertLayerCounter            : Cardinal;
    ThresholdLayerCounter         : Cardinal;
    PosterizeLayerCounter         : Cardinal;
    LevelsLayerCounter            : Cardinal;
    CurvesLayerCounter            : Cardinal;
    GradientMapLayerCounter       : Cardinal;
    GradientFillLayerCounter      : Cardinal;
    PatternLayerCounter           : Cardinal;
    VectorLayerCounter            : Cardinal;
    ShapeRegionLayerCounter       : Cardinal;
    ChannelMixerLayerCounter      : Cardinal;

    { Channel Part }
    ChannelCount                  : Cardinal; // indicating how many alpha channels (including Quick Mask channel) in the file
    AlphaChannelCounter           : Cardinal; // accumulated alpha channel counter
    DefaultChannelMaskColor       : TColor32;
    GlobalLayerMaskColor          : TColor32;
    GlobalLayerMaskOpacityPercent : Single;
    GlobalLayerMaskColorIndicator : Cardinal;

    { Path Part }
    PathCount                     : Cardinal; // indicating how many paths in the file
    PathCounter                   : Cardinal; // accumulated path counter
  end;

  { Custom GMD Reader }
  
  TgmCustomGMDReader = class(TObject)
  protected
    FFileFormatVersion : Cardinal;
    FLayerList         : TgmLayerList;
    FChannelManager    : TgmCustomChannelManager;
    FPathList          : TgmPathList;
    FTextEditor        : TRichEdit;
  public
    constructor Create(ALayerList: TgmLayerList;
                       AChannelManager: TgmCustomChannelManager;
                       APathList: TgmPathList;
                       ATextEditor: TRichEdit);

    destructor Destroy; override;

    function LoadFromStream(AMemoryStream: TMemoryStream): Boolean; virtual; abstract;
  end;

  { GMD Reader 1 }

  // a gmd reader for loading the .gmd file with file format is version 1
  TgmGMDReader1 = class(TgmCustomGMDReader)
  private
    FFileHeader : TgmGMDFileHeaderVer1;
  public
    constructor Create(ALayerList: TgmLayerList;
                       AChannelManager: TgmCustomChannelManager;
                       APathList: TgmPathList;
                       ATextEditor: TRichEdit);

    function LoadFromStream(AMemoryStream: TMemoryStream): Boolean; override;

    property FileHeader : TgmGMDFileHeaderVer1 read FFileHeader write FFileHeader;
  end;

  { GMD Reader 2 }

  // a gmd reader for loading the .gmd file with file format is version 2
  TgmGMDReader2 = class(TgmCustomGMDReader)
  private
    FFileHeader : TgmGMDFileHeaderVer2;
  public
    constructor Create(ALayerList: TgmLayerList;
                       AChannelManager: TgmCustomChannelManager;
                       APathList: TgmPathList;
                       ATextEditor: TRichEdit);

    function LoadFromStream(AMemoryStream: TMemoryStream): Boolean; override;

    property FileHeader : TgmGMDFileHeaderVer2 read FFileHeader write FFileHeader;
  end;

  { GMD Reader 3 }
  
  // a gmd reader for loading the .gmd file with file format is version 3
  TgmGMDReader3 = class(TgmGMDReader2)
  public
    constructor Create(ALayerList: TgmLayerList;
                       AChannelManager: TgmCustomChannelManager;
                       APathList: TgmPathList;
                       ATextEditor: TRichEdit);
  end;

  { GMD Reader 4 }

  // a gmd reader for loading the .gmd file with file format is version 4
  TgmGMDReader4 = class(TgmCustomGMDReader)
  private
    FFileHeader : TgmGMDFileHeaderVer4;
  public
    constructor Create(ALayerList: TgmLayerList;
                       AChannelManager: TgmCustomChannelManager;
                       APathList: TgmPathList;
                       ATextEditor: TRichEdit);

    function LoadFromStream(AMemoryStream: TMemoryStream): Boolean; override;

    property FileHeader : TgmGMDFileHeaderVer4 read FFileHeader write FFileHeader;
  end;

  
const
  FILE_ID      = $474D4446; // i.e. GMDF - GraphicsMagic Document File
  FILE_VERSION = 4;         // the file version we could process so far

{ TgmCustomGMDReader }

constructor TgmCustomGMDReader.Create(ALayerList: TgmLayerList;
  AChannelManager: TgmCustomChannelManager; APathList: TgmPathList;
  ATextEditor: TRichEdit);
begin
  inherited Create();

  FFileFormatVersion := 0;
  FLayerList         := ALayerList;
  FChannelManager    := AChannelManager;
  FPathList          := APathList;
  FTextEditor        := ATextEditor;
end;

destructor TgmCustomGMDReader.Destroy;
begin
  FLayerList      := nil;
  FChannelManager := nil;
  FPathList       := nil;

  inherited;
end; 

{ TgmGMDReader1 }

constructor TgmGMDReader1.Create(ALayerList: TgmLayerList;
  AChannelManager: TgmCustomChannelManager; APathList: TgmPathList;
  ATextEditor: TRichEdit);
begin
  inherited Create(ALayerList, AChannelManager, APathList, ATextEditor);

  FFileFormatVersion := 1;
end;

function TgmGMDReader1.LoadFromStream(AMemoryStream: TMemoryStream): Boolean;
var
  i     : Integer;
  LPath : TgmPath;
begin
  Result := False;

  if not Assigned(AMemoryStream) then
  begin
    Exit;
  end;

  if Assigned(FLayerList) then
  begin
    // load layers
    if FFileHeader.LayerCount > 0 then
    begin
      with FLayerList do
      begin
        SetLayerCounter(TgmNormalLayer,         FFileHeader.TransparentLayerNumber);
        SetLayerCounter(TgmSolidColorLayer,     FFileHeader.ColorFillLayerNumber);
        SetLayerCounter(TgmColorBalanceLayer,   FFileHeader.ColorBalanceLayerNumber);
        SetLayerCounter(TgmBrightContrastLayer, FFileHeader.BrightContrastLayerNumber);
        SetLayerCounter(TgmHueSaturationLayer,  FFileHeader.HLSLayerNumber);
        SetLayerCounter(TgmInvertLayer,         FFileHeader.InvertLayerNumber);
        SetLayerCounter(TgmThresholdLayer,      FFileHeader.ThresholdLayerNumber);
        SetLayerCounter(TgmPosterizeLayer,      FFileHeader.PosterizeLayerNumber);
        SetLayerCounter(TgmLevelsLayer,         FFileHeader.LevelsLayerNumber);
        SetLayerCounter(TgmCurvesLayer,         FFileHeader.CurvesLayerNumber);
        SetLayerCounter(TgmGradientMapLayer,    FFileHeader.GradientMapLayerNumber);
        SetLayerCounter(TgmGradientFillLayer,   FFileHeader.GradientFillLayerNumber);
        SetLayerCounter(TgmPatternLayer,        FFileHeader.PatternLayerNumber);
        SetLayerCounter(TgmVectorLayer,         FFileHeader.FigureLayerNumber);
        SetLayerCounter(TgmShapeRegionLayer,    FFileHeader.ShapeRegionLayerNumber);
      end;

      if not gmLayerIO.LoadLayersFromStream(
               AMemoryStream, FLayerList, FTextEditor, 
               FFileFormatVersion, FFileHeader.LayerCount,
               FFileHeader.LayerWidth, FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;

  if Assigned(FChannelManager) then
  begin
    // load channels
    with FChannelManager do
    begin
      AlphaChannelList.AccumulatedCount := FileHeader.AlphaChannelNumber;
      DefaultMaskColor                  := FileHeader.GlobalChannelMaskColor;
    end;

    if FFileHeader.ChannelCount > 0 then
    begin
      if not gmChannelIO.LoadChannelsFromStream(AMemoryStream,
                                                FChannelManager,
                                                FFileFormatVersion,
                                                FFileHeader.ChannelCount,
                                                FFileHeader.LayerWidth,
                                                FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;

  if Assigned(FPathList) then
  begin
    // load Pen Paths data
    FPathList.PathCounter := FileHeader.PathPanelNumber;

    if FFileHeader.PathPanelCount > 0 then
    begin
      if not gmPathIO.GMPLoadPathsFromStream(AMemoryStream, FPathList, 
               FFileFormatVersion, FFileHeader.PathPanelCount) then
      begin
        Exit;
      end;

      if FPathList.Count > 0 then
      begin
        for i := 0 to (FPathList.Count - 1) do
        begin
          LPath := FPathList.Paths[i];

          LPath.UpdateThumbnail( FFileHeader.LayerWidth, FFileHeader.LayerHeight,
                                 Point(0, 0) );
        end;
      end;
    end;
  end;   

  Result := True;
end;

{ GMD Reader 2 }

constructor TgmGMDReader2.Create(ALayerList: TgmLayerList;
  AChannelManager: TgmCustomChannelManager; APathList: TgmPathList;
  ATextEditor: TRichEdit);
begin
  inherited Create(ALayerList, AChannelManager, APathList, ATextEditor);
  
  FFileFormatVersion := 2;
end;

function TgmGMDReader2.LoadFromStream(AMemoryStream: TMemoryStream): Boolean;
var
  i     : Integer;
  LPath : TgmPath;
begin
  Result := False;

  if not Assigned(AMemoryStream) then
  begin
    Exit;
  end;

  if Assigned(FLayerList) then
  begin
    // load layers
    if FFileHeader.LayerCount > 0 then
    begin
      with FLayerList do
      begin
        SetLayerCounter(TgmNormalLayer,         FFileHeader.TransparentLayerNumber);
        SetLayerCounter(TgmSolidColorLayer,     FFileHeader.ColorFillLayerNumber);
        SetLayerCounter(TgmColorBalanceLayer,   FFileHeader.ColorBalanceLayerNumber);
        SetLayerCounter(TgmBrightContrastLayer, FFileHeader.BrightContrastLayerNumber);
        SetLayerCounter(TgmHueSaturationLayer,  FFileHeader.HLSLayerNumber);
        SetLayerCounter(TgmInvertLayer,         FFileHeader.InvertLayerNumber);
        SetLayerCounter(TgmThresholdLayer,      FFileHeader.ThresholdLayerNumber);
        SetLayerCounter(TgmPosterizeLayer,      FFileHeader.PosterizeLayerNumber);
        SetLayerCounter(TgmLevelsLayer,         FFileHeader.LevelsLayerNumber);
        SetLayerCounter(TgmCurvesLayer,         FFileHeader.CurvesLayerNumber);
        SetLayerCounter(TgmGradientMapLayer,    FFileHeader.GradientMapLayerNumber);
        SetLayerCounter(TgmGradientFillLayer,   FFileHeader.GradientFillLayerNumber);
        SetLayerCounter(TgmPatternLayer,        FFileHeader.PatternLayerNumber);
        SetLayerCounter(TgmVectorLayer,         FFileHeader.FigureLayerNumber);
        SetLayerCounter(TgmShapeRegionLayer,    FFileHeader.ShapeRegionLayerNumber);
        SetLayerCounter(TgmChannelMixerLayer,   FFileHeader.ChannelMixerLayerNumber);  // new
      end;

      if not gmLayerIO.LoadLayersFromStream(
               AMemoryStream, FLayerList, FTextEditor, 
               FFileFormatVersion, FFileHeader.LayerCount,
               FFileHeader.LayerWidth, FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;

  if Assigned(FChannelManager) then
  begin
    // load channels
    with FChannelManager do
    begin
      AlphaChannelList.AccumulatedCount := FileHeader.AlphaChannelNumber;
      DefaultMaskColor                  := FileHeader.GlobalChannelMaskColor;
    end;

    if FFileHeader.ChannelCount > 0 then
    begin
      if not gmChannelIO.LoadChannelsFromStream(AMemoryStream,
                                                FChannelManager,
                                                FFileFormatVersion,
                                                FFileHeader.ChannelCount,
                                                FFileHeader.LayerWidth,
                                                FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;

  if Assigned(FPathList) then
  begin
    // load Pen Paths data
    FPathList.PathCounter := FileHeader.PathPanelNumber;

    if FFileHeader.PathPanelCount > 0 then
    begin
      if not gmPathIO.GMPLoadPathsFromStream(AMemoryStream, FPathList, 
               FFileFormatVersion, FFileHeader.PathPanelCount) then
      begin
        Exit;
      end;

      if FPathList.Count > 0 then
      begin
        for i := 0 to (FPathList.Count - 1) do
        begin
          LPath := FPathList.Paths[i];

          LPath.UpdateThumbnail( FFileHeader.LayerWidth, FFileHeader.LayerHeight,
                                 Point(0, 0) );
        end;
      end;
    end;
  end;   

  Result := True;
end;

{ GMD Reader 3 }

constructor TgmGMDReader3.Create(ALayerList: TgmLayerList;
  AChannelManager: TgmCustomChannelManager; APathList: TgmPathList;
  ATextEditor: TRichEdit);
begin
  inherited Create(ALayerList, AChannelManager, APathList, ATextEditor);
  
  FFileFormatVersion := 3;
end;

{ GMD Reader 4 }

// a gmd reader for loading the .gmd file with file format is version 4
constructor TgmGMDReader4.Create(ALayerList: TgmLayerList;
  AChannelManager: TgmCustomChannelManager; APathList: TgmPathList;
  ATextEditor: TRichEdit);
begin
  inherited Create(ALayerList, AChannelManager, APathList, ATextEditor);
  
  FFileFormatVersion := 4;
end;

function TgmGMDReader4.LoadFromStream(AMemoryStream: TMemoryStream): Boolean;
var
  i     : Integer;
  LPath : TgmPath;
begin
  Result := False;

  if not Assigned(AMemoryStream) then
  begin
    Exit;
  end;

  if Assigned(FLayerList) then
  begin
    // load layers
    if FFileHeader.LayerCount > 0 then
    begin
      with FLayerList do
      begin
        SetLayerCounter(TgmNormalLayer,         FFileHeader.NormalLayerCounter);
        SetLayerCounter(TgmSolidColorLayer,     FFileHeader.SolidColorLayerCounter);
        SetLayerCounter(TgmColorBalanceLayer,   FFileHeader.ColorBalanceLayerCounter);
        SetLayerCounter(TgmBrightContrastLayer, FFileHeader.BrightContrastLayerCounter);
        SetLayerCounter(TgmHueSaturationLayer,  FFileHeader.HueSaturationLayerCounter);
        SetLayerCounter(TgmInvertLayer,         FFileHeader.InvertLayerCounter);
        SetLayerCounter(TgmThresholdLayer,      FFileHeader.ThresholdLayerCounter);
        SetLayerCounter(TgmPosterizeLayer,      FFileHeader.PosterizeLayerCounter);
        SetLayerCounter(TgmLevelsLayer,         FFileHeader.LevelsLayerCounter);
        SetLayerCounter(TgmCurvesLayer,         FFileHeader.CurvesLayerCounter);
        SetLayerCounter(TgmGradientMapLayer,    FFileHeader.GradientMapLayerCounter);
        SetLayerCounter(TgmGradientFillLayer,   FFileHeader.GradientFillLayerCounter);
        SetLayerCounter(TgmPatternLayer,        FFileHeader.PatternLayerCounter);
        SetLayerCounter(TgmVectorLayer,         FFileHeader.VectorLayerCounter);
        SetLayerCounter(TgmShapeRegionLayer,    FFileHeader.ShapeRegionLayerCounter);
        SetLayerCounter(TgmChannelMixerLayer,   FFileHeader.ChannelMixerLayerCounter); 
      end;

      if not gmLayerIO.LoadLayersFromStream(
               AMemoryStream, FLayerList, FTextEditor, 
               FFileFormatVersion, FFileHeader.LayerCount,
               FFileHeader.LayerWidth, FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;  

  if Assigned(FChannelManager) then
  begin
    // load channels
    with FChannelManager do
    begin
      AlphaChannelList.AccumulatedCount := FileHeader.AlphaChannelCounter;
      DefaultMaskColor                  := FileHeader.DefaultChannelMaskColor;
    end;

    if FFileHeader.ChannelCount > 0 then
    begin
      if not gmChannelIO.LoadChannelsFromStream(AMemoryStream,
                                                FChannelManager,
                                                FFileFormatVersion,
                                                FFileHeader.ChannelCount,
                                                FFileHeader.LayerWidth,
                                                FFileHeader.LayerHeight) then
      begin
        Exit;
      end;
    end;
  end;

  if Assigned(FPathList) then
  begin
    // load Pen Paths data
    FPathList.PathCounter := FileHeader.PathCounter;

    if FFileHeader.PathCount > 0 then
    begin
      if not gmPathIO.GMPLoadPathsFromStream(AMemoryStream, FPathList,
               FFileFormatVersion, FFileHeader.PathCount) then
      begin
        Exit;
      end;

      if FPathList.Count > 0 then
      begin
        for i := 0 to (FPathList.Count - 1) do
        begin
          LPath := FPathList.Paths[i];

          LPath.UpdateThumbnail( FFileHeader.LayerWidth, FFileHeader.LayerHeight,
                                 Point(0, 0) );
        end;
      end;
    end;
  end;

  Result := True;
end;

{ TgmGMDManager }

constructor TgmGMDManager.Create;
begin
  inherited;

  FMemoryStream := TMemoryStream.Create();
  FOutputMsg    := '';

  FLayerList      := nil;
  FChannelManager := nil;
  FPathList       := nil;
  FTextEditor     := nil;

  // callbacks ...
  FAfterFileLoaded := nil;
end; 

destructor TgmGMDManager.Destroy;
begin
  FMemoryStream.Clear();
  FMemoryStream.Free();

  FLayerList      := nil;
  FChannelManager := nil;
  FPathList       := nil;

  FAfterFileLoaded := nil;
  
  inherited;
end; 

// use this function to determine whether or not we could open a .gmd file
function TgmGMDManager.CheckFileValidity(const AFileName: string): Boolean;
var
  LMemStream: TMemoryStream;
  LFileID   : Cardinal;
  LFileVer  : Cardinal;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    FOutputMsg := 'The file "' + ExtractFileName(AFileName) + '" is not exists.';
    Exit;
  end;

  LMemStream := TMemoryStream.Create;
  try
    LMemStream.LoadFromFile(AFileName);
    LMemStream.Position := 0;

    // check the file ID
    LMemStream.Read(LFileID, 4);

    if LFileID <> FILE_ID then
    begin
      FOutputMsg := 'The file is not a valid GraphicsMagic document.';
      Exit;
    end;

    // check the file version
    LMemStream.Read(LFileVer, 4);

    if LFileVer > FILE_VERSION then
    begin
      FOutputMsg := 'Cannot open the file because the file version is high.';
      Exit;
    end;

    Result := True;
  finally
    LMemStream.Free;
  end;
end; 

// The file name must be valid and the file must be existed.
// Check these before calling this function, please. 
function TgmGMDManager.LoadFromFile(const AFileName: string): Boolean;
var
  LFileID         : Cardinal;
  LFileVersion    : Cardinal;
  LFileHeaderVer1 : TgmGMDFileHeaderVer1;
  LFileHeaderVer2 : TgmGMDFileHeaderVer2;
  LFileHeaderVer4 : TgmGMDFileHeaderVer4;
  LFileReader     : TgmCustomGMDReader;
begin
  Result := False;

  FMemoryStream.Clear();
  FMemoryStream.LoadFromFile(AFileName);
  FMemoryStream.Position := 0;

  // check the file ID
  FMemoryStream.Read(LFileID, 4);

  if LFileID <> FILE_ID then
  begin
    FOutputMsg := 'The file is not a valid GraphicsMagic document.';
    Exit;
  end;

  // check the file version
  FMemoryStream.Read(LFileVersion, 4);

  if LFileVersion > FILE_VERSION then
  begin
    FOutputMsg := 'Cannot open the file because the file version is high.';
    Exit;
  end;

  FMemoryStream.Position := 0;

  // read in data, according to different file version
  case LFileVersion of
    1:
      begin
        FMemoryStream.Read(LFileHeaderVer1, SizeOf(TgmGMDFileHeaderVer1));

        LFileReader := TgmGMDReader1.Create(FLayerList,
                                            FChannelManager,
                                            FPathList,
                                            FTextEditor);
        try
          TgmGMDReader1(LFileReader).FileHeader := LFileHeaderVer1;

          if LFileReader.LoadFromStream(FMemoryStream) then
          begin
            Result := True;

            if Assigned(FAfterFileLoaded) then
            begin
              FAfterFileLoaded( Self, LFileVersion,
                                LFileHeaderVer1.GlobalLayerMaskColor,
                                LFileHeaderVer1.GlobalLayerMaskOpacityPercent,
                                TgmMaskColorIndicator(mciMaskedArea) );
            end;
          end;
        finally
          LFileReader.Free();
        end;
      end;

    2:
      begin
        FMemoryStream.Read(LFileHeaderVer2, SizeOf(TgmGMDFileHeaderVer2));

        LFileReader := TgmGMDReader2.Create(FLayerList,
                                            FChannelManager,
                                            FPathList,
                                            FTextEditor);
        try
          TgmGMDReader2(LFileReader).FileHeader := LFileHeaderVer2;

          if LFileReader.LoadFromStream(FMemoryStream) then
          begin
            Result := True;

            if Assigned(FAfterFileLoaded) then
            begin
              FAfterFileLoaded( Self, LFileVersion,
                                LFileHeaderVer2.GlobalLayerMaskColor,
                                LFileHeaderVer2.GlobalLayerMaskOpacityPercent,
                                TgmMaskColorIndicator(mciMaskedArea) );
            end;
          end;
        finally
          LFileReader.Free;
        end;
      end;

    3:
      begin
        // the file header format is same as version 2
        FMemoryStream.Read(LFileHeaderVer2, SizeOf(TgmGMDFileHeaderVer2));

        LFileReader := TgmGMDReader3.Create(FLayerList,
                                            FChannelManager,
                                            FPathList,
                                            FTextEditor);
        try
          TgmGMDReader3(LFileReader).FileHeader := LFileHeaderVer2;

          if LFileReader.LoadFromStream(FMemoryStream) then
          begin
            Result := True;

            if Assigned(FAfterFileLoaded) then
            begin
              FAfterFileLoaded( Self, LFileVersion,
                                LFileHeaderVer2.GlobalLayerMaskColor,
                                LFileHeaderVer2.GlobalLayerMaskOpacityPercent,
                                TgmMaskColorIndicator(mciMaskedArea) );
            end;
          end;

        finally
          LFileReader.Free;
        end;
      end;

    4:
      begin
        // the file header format is same as version 2
        FMemoryStream.Read(LFileHeaderVer4, SizeOf(TgmGMDFileHeaderVer4));

        LFileReader := TgmGMDReader4.Create(FLayerList,
                                            FChannelManager,
                                            FPathList,
                                            FTextEditor);
        try
          TgmGMDReader4(LFileReader).FileHeader := LFileHeaderVer4;

          if LFileReader.LoadFromStream(FMemoryStream) then
          begin
            Result := True;

            if Assigned(FAfterFileLoaded) then
            begin
              FAfterFileLoaded( Self, LFileVersion,
                                LFileHeaderVer4.GlobalLayerMaskColor,
                                LFileHeaderVer4.GlobalLayerMaskOpacityPercent,
                                TgmMaskColorIndicator(LFileHeaderVer4.GlobalLayerMaskColorIndicator) );
            end;
          end;
        finally
          LFileReader.Free;
        end; 
      end;
  end;
end;

// The file name must be full, that is, with valid path and file extension.
// Check these before this function call, please.
procedure TgmGMDManager.SaveToFile(const AFileName: string);
var
  LFileHeader     : TgmGMDFileHeaderVer4;
  LLayersWriter   : TgmLayersWriter;
  LChannelsWriter : TgmChannelsWriter;
  LPathsWriter    : TgmPathsWriter;
begin
  if Assigned(FLayerList) and
     Assigned(FChannelManager) and
     Assigned(FPathList) then
  begin
    LFileHeader.FileID      := FILE_ID;
    LFileHeader.FileVersion := FILE_VERSION;

    // Layer Part ...
    with LFileHeader do
    begin
      LayerWidth                 := FLayerList.SelectedLayer.LayerBitmap.Width;
      LayerHeight                := FLayerList.SelectedLayer.LayerBitmap.Height;
      LayerCount                 := FLayerList.Count;
      NormalLayerCounter         := FLayerList.GetLayerCounter(TgmNormalLayer);
      SolidColorLayerCounter     := FLayerList.GetLayerCounter(TgmSolidColorLayer);
      ColorBalanceLayerCounter   := FLayerList.GetLayerCounter(TgmColorBalanceLayer);
      BrightContrastLayerCounter := FLayerList.GetLayerCounter(TgmBrightContrastLayer);
      HueSaturationLayerCounter  := FLayerList.GetLayerCounter(TgmHueSaturationLayer);
      InvertLayerCounter         := FLayerList.GetLayerCounter(TgmInvertLayer);
      ThresholdLayerCounter      := FLayerList.GetLayerCounter(TgmThresholdLayer);
      PosterizeLayerCounter      := FLayerList.GetLayerCounter(TgmPosterizeLayer);
      LevelsLayerCounter         := FLayerList.GetLayerCounter(TgmLevelsLayer);
      CurvesLayerCounter         := FLayerList.GetLayerCounter(TgmCurvesLayer);
      GradientMapLayerCounter    := FLayerList.GetLayerCounter(TgmGradientMapLayer);
      GradientFillLayerCounter   := FLayerList.GetLayerCounter(TgmGradientFillLayer);
      PatternLayerCounter        := FLayerList.GetLayerCounter(TgmPatternLayer);
      VectorLayerCounter         := FLayerList.GetLayerCounter(TgmVectorLayer);
      ShapeRegionLayerCounter    := FLayerList.GetLayerCounter(TgmShapeRegionLayer);
      ChannelMixerLayerCounter   := FLayerList.GetLayerCounter(TgmChannelMixerLayer);
    end;

    // Channel Part ...

    // channel count, including the Quick Mask channel
    LFileHeader.ChannelCount := FChannelManager.AlphaChannelList.Count;

    if Assigned(FChannelManager.QuickMaskChannel) then
    begin
      Inc(LFileHeader.ChannelCount);
    end;

    LFileHeader.AlphaChannelCounter     := FChannelManager.AlphaChannelList.AccumulatedCount;
    LFileHeader.DefaultChannelMaskColor := FChannelManager.DefaultMaskColor;

    if Assigned(FChannelManager.LayerMaskChannel) then
    begin
      LFileHeader.GlobalLayerMaskColor          := FChannelManager.LayerMaskChannel.MaskColor;
      LFileHeader.GlobalLayerMaskOpacityPercent := FChannelManager.LayerMaskChannel.MaskOpacity / 255;
      LFileHeader.GlobalLayerMaskColorIndicator := Ord(FChannelManager.LayerMaskChannel.MaskColorIndicator);
    end
    else
    begin
      LFileHeader.GlobalLayerMaskColor          := clRed32; // red
      LFileHeader.GlobalLayerMaskOpacityPercent := 0.5;
      LFileHeader.GlobalLayerMaskColorIndicator := Ord(mciMaskedArea);
    end;

    // Path Part ...
    LFileHeader.PathCount   := FPathList.Count;
    LFileHeader.PathCounter := FPathList.PathCounter;

    FMemoryStream.Clear();

    // write the file header to the stream
    FMemoryStream.Write(LFileHeader, SizeOf(TgmGMDFileHeaderVer4));

    // write the layer info to the stream
    if LFileHeader.LayerCount > 0 then
    begin
      LLayersWriter := TgmLayersWriter.Create(FLayerList);
      try
        LLayersWriter.SaveToStream(FMemoryStream);
      finally
        LLayersWriter.Free();
      end;
    end;

    // write the channel info to the stream
    if LFileHeader.ChannelCount > 0 then
    begin
      LChannelsWriter := TgmChannelsWriter.Create(FChannelManager);
      try
        LChannelsWriter.SaveToStream(FMemoryStream);
      finally
        LChannelsWriter.Free();
      end;
    end;

    // write the path info to the stream
    if LFileHeader.PathCount > 0 then
    begin
      LPathsWriter := TgmPathsWriter.Create(FPathList);
      try
        LPathsWriter.SaveToStream(FMemoryStream);
      finally
        LPathsWriter.Free();
      end;
    end;

    FMemoryStream.Position := 0;
    FMemoryStream.SaveToFile(AFileName);
  end;
end; 


end.
