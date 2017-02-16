unit gmCurvesTool;

{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "gimpcurvestool.h" and "gimpcurvestool.c" from GIMP 2.2.10 .
  The original source can be found at www.gimp.org.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis,
  for giving us the opportunity to know how to achieve Curves Tool.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }


// Update Date: 2014/07/15
// Updated by Ma Xiaoguang
// Modification: Adding Assign() method to class TgmCurvesTool.

// Update Date: 2015/04/28
// Updated by Ma Xiaoguang
// Modification: New file header for class TgmCurvesTool. 


interface

uses
{ Standard }
  Windows, Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmTypes,
  gmGimpBaseCurves,
  gmGimpColorBar,
  gmGimpHistogram,
  gmGimpLut;

type
  TgmCurvesFileHeader = record
    ID      : Cardinal;
    Version : Cardinal;
  end;

  TgmCurvesDataErrorType = (cdetNone,
                            cdetIDError,
                            cdetVersionError);


//-- TgmCurvesTool -------------------------------------------------------------

  TgmCurvesTool = class(TObject)
  private
    FLUT           : TgmGimpLut;

    FColored       : Boolean;
    FChannel       : Integer;

    FHist          : TgmGimpHistogram;
    FColorBar      : TgmGimpColorBar;  // Adding by ourselves.

    FXRange        : TBitmap32;
    FYRange        : TBitmap32;
    FSourceBmp     : TBitmap32;

    FScale         : TgmGimpHistogramScale;
    FCurveType     : Integer;

    FCurves        : TgmCurves;
    FGrabPoint     : Integer;
    FLast          : Integer;
    FLeftmost      : Integer;
    FRightmost     : Integer;

    FOutputMsg     : string; // output imformation, such as errors, etc.
    FDataErrorType : TgmCurvesDataErrorType;

    procedure Init;
    procedure CurvesGraphExpose(const AGraphBmp: TBitmap32);
    procedure FreeLUT; // Added by ourselves.
    
    procedure SetChannel(const AChannel: Integer);
    procedure SetScale(const AScale: TgmGimpHistogramScale);
    procedure SetCurveType(const ACurveType: Integer);
  public
    constructor Create(const ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure Assign(ACurvesTool: TgmCurvesTool);
    procedure CurvesUpdate(const AUpdate: Integer; AGraphBmp, AXRangeBmp, AYRangeBmp: TBitmap32);
    procedure LUTSetup(const NChannels: Integer);
    procedure Map(const ADestBmp: TBitmap32; const AChannelSet: TgmChannelSet);
    procedure CurvesCurrentChannelReset;
    procedure CurvesAllChannelReset;

    function LoadFromStream(const AStream: TStream): Boolean;  // load Curves settings from a stream
    procedure SaveToStream(const AStream: TStream);            // save Curves settings to a stream

    function LoadFromFile(const AFileName: string): Boolean;  // load Curves settings from file
    procedure SaveToFile(const AFileName: string);            // save Curves settings to file

    property Channel     : Integer               read FChannel   write SetChannel;
    property Scale       : TgmGimpHistogramScale read FScale     write SetScale;
    property CurveType   : Integer               read FCurveType write SetCurveType;
    property LUT         : TgmGimpLut            read FLUT       write FLUT;
    property Hist        : TgmGimpHistogram      read FHist      write FHist;
    property Curves      : TgmCurves             read FCurves;
    property GrabPoint   : Integer               read FGrabPoint write FGrabPoint;
    property Last        : Integer               read FLast      write FLast;
    property Leftmost    : Integer               read FLeftmost  write FLeftmost;
    property Rightmost   : Integer               read FRightmost write FRightmost;
    property SourceBitmap: TBitmap32             read FSourceBmp;
    property OutputMsg   : string                read FOutputMsg;
  end;

const
  DRAW_X_RANGE = (1 shl 0);
  DRAW_Y_RANGE = (1 shl 1);
  DRAW_GRAPH   = (1 shl 2);
  DRAW_ALL     = (DRAW_X_RANGE or DRAW_Y_RANGE or DRAW_GRAPH);
  BAR_SIZE     = 12;
  RADIUS       = 3;
  MIN_DISTANCE = 8;

  CURVES_FILE_EXT     = '.gcv';    // gcv stands for 'GraphicsMagic Curves'
  CURVES_FILE_ID      = $474D4356; // i.e. GMCV - GraphicsMagic Curves
  CURVES_FILE_VERSION = 1;

implementation

uses
{ Standard }
  SysUtils, Graphics,
{ GraphicsMagic Lib }
  gmGimpBaseEnums,
  gmGimpCommonFuncs,
  gmGtkEnums,
  gmGdkDrawable_Win32;

//-- TgmCurvesTool ---------------------------------------------------------

constructor TgmCurvesTool.Create(const ASourceBmp: TBitmap32);
begin
  inherited Create;

  FSourceBmp := TBitmap32.Create;
  FSourceBmp.Assign(ASourceBmp);

  FXRange        := TBitmap32.Create;
  FXRange.Width  := GRAPH_SIZE;
  FXRange.Height := BAR_SIZE;

  FYRange        := TBitmap32.Create;
  FYRange.Width  := BAR_SIZE;
  FYRange.Height := GRAPH_SIZE;

  FOutputMsg     := '';

  Init;
end;

destructor TgmCurvesTool.Destroy;
begin
  FCurves.Free;
  FHist.Free;
  FColorBar.Free;
  FreeLUT;
  FSourceBmp.Free;
  FXRange.Free;
  FYRange.Free;
  
  inherited Destroy;
end;

procedure TgmCurvesTool.Assign(ACurvesTool: TgmCurvesTool);
var
  i, j, k : Integer;
begin
{$RANGECHECKS OFF}
  if not Assigned(ACurvesTool) then
  begin
    Exit;
  end;

  Self.LUTSetup(3);
  
  for j := Low(ACurvesTool.LUT.luts) to High(ACurvesTool.LUT.luts) do
  begin
    for i := Low(ACurvesTool.LUT.luts[j]) to High(ACurvesTool.LUT.luts[j]) do
    begin
      Self.LUT.luts[j, i] := ACurvesTool.LUT.luts[j, i];
    end;
  end;

  for i := 0 to 4 do
  begin
    Self.Curves.CurveType[i] := ACurvesTool.Curves.CurveType[i];
  end;

  for i := 0 to 4 do
  begin
    for j := 0 to 16 do
    begin
      for k := 0 to 1 do
      begin
        Self.Curves.Points[i, j, k] := ACurvesTool.Curves.Points[i, j, k];
      end;
    end;
  end;

  for i := 0 to 4 do
  begin
    for j := 0 to 255 do
    begin
      Self.Curves.FCurve[i, j] := ACurvesTool.Curves.FCurve[i, j];
    end;
  end;

  Self.Channel   := ACurvesTool.Channel;
  Self.Scale     := ACurvesTool.Scale;
  Self.CurveType := ACurvesTool.CurveType;
{$RANGECHECKS ON}
end;

procedure TgmCurvesTool.Init;
begin
  FColored   := True;
  FLUT       := gimp_lut_new;
  FChannel   := Gimp_HISTOGRAM_VALUE;
  FScale     := GIMP_HISTOGRAM_SCALE_LINEAR;
  FCurveType := GIMP_CURVE_SMOOTH;

  FCurves := TgmCurves.Create;

  { Added by ourselves. }
  FHist := TgmGimpHistogram.Create;

  FHist.gimp_histogram_calculate(FSourceBmp);
  FHist.gimp_histogram_view_expose(FChannel);

  FColorBar := TgmGimpColorBar.Create;
end; 

{ Draw the curves map. }
procedure TgmCurvesTool.CurvesGraphExpose(const AGraphBmp: TBitmap32);
var
  LWidth : Integer;
  LHeight: Integer;
  x, y, i: Integer;
  LPoints: array [0..255] of TPoint;
begin
  LWidth  := AGraphBmp.Width  - 2 * RADIUS;
  LHeight := AGraphBmp.Height - 2 * RADIUS;

  if (LWidth < 1) or (LHeight < 1) then
  begin
    Exit;
  end;

  { Draw the grid lines }
  for i := 1 to 3 do
  begin
    AGraphBmp.LineS( RADIUS, RADIUS + i * (LHeight div 4),
                     RADIUS + LWidth - 1, RADIUS + i * (LHeight div 4), clGray32 );
                    
    AGraphBmp.LineS( RADIUS + i * (LWidth div 4), RADIUS,
                     RADIUS + i * (LWidth div 4), RADIUS + LHeight - 1, clGray32 );
  end;

  { Draw the border -- code by ourselves }
  AGraphBmp.FrameRectS(RADIUS, RADIUS, RADIUS + LWidth, RADIUS + LHeight, clGray32);

  {  Draw the curve  }
  for i := 0 to 255 do
  begin
    x := i;
    y := 255 - FCurves.FCurve[FChannel, x];

    LPoints[i].X := RADIUS + Round(LWidth  * x / 256.0);
    LPoints[i].Y := RADIUS + Round(LHeight * y / 256.0);
  end;

  { code by ourselves instead of gdk_draw_lines() }
  for i := 0 to 254 do
  begin
    AGraphBmp.LineS(LPoints[i].X, LPoints[i].Y, LPoints[i + 1].X, LPoints[i + 1].Y, clBlack32);
  end;

  if FCurves.CurveType[FChannel] = GIMP_CURVE_SMOOTH then
  begin
    { Draw the points }
    AGraphBmp.Canvas.Brush.Style := bsSolid;
    AGraphBmp.Canvas.Brush.Color := clBlack;

    for i := 0 to CURVES_NUM_POINTS - 1 do
    begin
      x := FCurves.Points[FChannel, i, 0];

      if x < 0 then
      begin
        Continue;
      end;

      y := 255 - FCurves.Points[FChannel, i, 1];

      GDK_Win32_Draw_Arc( AGraphBmp.Canvas, True,
                          Round(LWidth  * x / 256.0),
                          Round(LHeight * y / 256.0),
                          RADIUS * 2, RADIUS * 2, 0, 23040);
    end;
  end;
end;

procedure TgmCurvesTool.FreeLUT;
var
  i: Integer;
begin
  if High(FLUT.luts) > (-1) then
  begin
    for i := Low(FLUT.luts) to High(FLUT.luts) do
    begin
      if High(FLUT.luts[i]) > (-1) then
      begin
        SetLength(FLUT.luts[i], 0);
        FLUT.luts[i] := nil;
      end;
    end;

    SetLength(FLUT.luts, 0);
    FLUT.luts := nil;
  end;
end;

procedure TgmCurvesTool.SetChannel(const AChannel: Integer);
begin
  if FChannel <> AChannel then
  begin
    FChannel := AChannel;

    case FCurveType of
      GIMP_CURVE_SMOOTH:
        begin
          if FCurves.CurveType[FChannel] <> GIMP_CURVE_SMOOTH then
          begin
            FCurves.CurveType[FChannel] := GIMP_CURVE_SMOOTH;
          end;
        end;
        
      GIMP_CURVE_FREE:
        begin
          if FCurves.CurveType[FChannel] <> GIMP_CURVE_FREE then
          begin
            FCurves.CurveType[FChannel] := GIMP_CURVE_FREE;
          end;
        end;
    end;

    FHist.gimp_histogram_view_expose(FChannel);
  end;
end;

procedure TgmCurvesTool.SetScale(const AScale: TgmGimpHistogramScale);
begin
  if FScale <> AScale then
  begin
    FScale      := AScale;
    FHist.Scale := FScale;
    
    FHist.gimp_histogram_view_expose(FChannel);
  end;
end; 

procedure TgmCurvesTool.SetCurveType(const ACurveType: Integer);
var
  i, LIndex: Integer;
begin
  if FCurveType <> ACurveType then
  begin
    FCurveType := ACurveType;

    if FCurves.CurveType[FChannel] <> FCurveType then
    begin
      FCurves.CurveType[FChannel] := FCurveType;

      if FCurveType = GIMP_CURVE_SMOOTH then
      begin
        { pick representative points from the curve
          and make them control points }

        for i := 0 to 8 do
        begin
          LIndex := CLAMP0255(i * 32);
          
          FCurves.Points[FChannel, i * 2, 0] := LIndex;
          FCurves.Points[FChannel, i * 2, 1] := FCurves.FCurve[FChannel, LIndex];
        end;
      end;

      FCurves.CalculateCurve(FChannel);
    end;
  end;
end;

{ TODO: preview alpha channel stuff correctly.  -- austin, 20/May/99 }
procedure TgmCurvesTool.CurvesUpdate(const AUpdate: Integer;
  AGraphBmp, AXRangeBmp, AYRangeBmp: TBitmap32);
var
  LChannel: Integer;
begin
  if FColored then
  begin
    LChannel := FChannel;
  end
  else
  begin
    { FIXME: hack }
    if FChannel = 1 then
    begin
      LChannel := GIMP_HISTOGRAM_ALPHA;
    end
    else
    begin
      LChannel := GIMP_HISTOGRAM_VALUE;
    end;
  end;

  if (AUpdate and DRAW_GRAPH) > 0 then
  begin
    AGraphBmp.Draw(0, 0, FHist.HistogramMap);
    CurvesGraphExpose(AGraphBmp);
  end;

  if (AUpdate and DRAW_X_RANGE) > 0 then
  begin
    case LChannel of
      GIMP_HISTOGRAM_VALUE,
      GIMP_HISTOGRAM_ALPHA,
      GIMP_HISTOGRAM_RGB:
        begin
          FColorBar.gimp_color_bar_set_buffers(FCurves.FCurve[FChannel],
                                               FCurves.FCurve[FChannel],
                                               FCurves.FCurve[FChannel]);
        end;

      GIMP_HISTOGRAM_RED,
      GIMP_HISTOGRAM_GREEN,
      GIMP_HISTOGRAM_BLUE:
        begin
          FColorBar.gimp_color_bar_set_buffers(FCurves.FCurve[GIMP_HISTOGRAM_RED],
                                               FCurves.FCurve[GIMP_HISTOGRAM_GREEN],
                                               FCurves.FCurve[GIMP_HISTOGRAM_BLUE]);
        end;
    end;
    
    FColorBar.Orientation := GTK_ORIENTATION_HORIZONTAL;
    
    FColorBar.gimp_color_bar_expose(FXRange);
    AXRangeBmp.Draw(0, 0, FXRange);
  end;

  if (AUpdate and DRAW_Y_RANGE) > 0 then
  begin
    FColorBar.Channel     := LChannel;
    FColorBar.Orientation := GTK_ORIENTATION_VERTICAL;
    
    FColorBar.gimp_color_bar_expose(FYRange);
    AYRangeBmp.Draw(0, 0, FYRange);
  end;
end; 

procedure TgmCurvesTool.LUTSetup(const NChannels: Integer);
var
  i, v: Cardinal;
  LVal: Double;
begin
  if High(FLUT.luts) > (-1) then
  begin
    for i := 0 to FLUT.nchannels - 1 do
    begin
      SetLength(FLUT.luts[i], 0);
    end;
    
    SetLength(FLUT.luts, 0);
  end;

  FLUT.nchannels := NChannels;
  SetLength(FLUT.luts, FLUT.nchannels);

  for i := 0 to FLUT.nchannels - 1 do
  begin
    SetLength(FLUT.luts[i], 256);

    for v := 0 to 255 do
    begin
      { to add gamma correction use func(v ^ g) ^ 1/g instead. }
      LVal := 255.0 * FCurves.LUTFunc(FLUT.nchannels, i, v / 255.0) + 0.5;

      FLUT.luts[i, v] := CLAMP( Round(LVal), 0, 255 );
    end;
  end;
end;

procedure TgmCurvesTool.Map(const ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
begin
  LUTSetup(3);
  gimp_lut_process(FLUT, FSourceBmp, ADestBmp, AChannelSet);
end; 

procedure TgmCurvesTool.CurvesCurrentChannelReset;
begin
  FGrabPoint := -1;
  FCurves.ChannelReset(FChannel);
end; 

procedure TgmCurvesTool.CurvesAllChannelReset;
var
  LChannel: Integer;
begin
  FGrabPoint := -1;
  
  for LChannel := GIMP_HISTOGRAM_VALUE to GIMP_HISTOGRAM_ALPHA do
  begin
    FCurves.ChannelReset(LChannel);
  end;
end; 

// load Curves settings from a stream
function TgmCurvesTool.LoadFromStream(const AStream: TStream): Boolean;
var
  LFileHeader        : TgmCurvesFileHeader;
  i, j, k, LIntValue : Integer;
begin
  Result := False;

  if Assigned(AStream) then
  begin
    AStream.Read(LFileHeader, SizeOf(LFileHeader));

    FDataErrorType := cdetNone;
    if LFileHeader.ID <> CURVES_FILE_ID then
    begin
      FDataErrorType := cdetIDError;
      
      FOutputMsg := 'Cannot open the file,' + #10#13 +
                    'because the file header data is not correct.';
                    
      Exit;
    end
    else if LFileHeader.Version > CURVES_FILE_VERSION then
    begin
      FDataErrorType := cdetVersionError;

      FOutputMsg := 'Cannot open the file, ' + #10#13 +
                    'because the file version is higher than this program supports.';
                    
      Exit;
    end;

    // loading data ...

    for j := Low(FLUT.luts) to High(FLUT.luts) do
    begin
      for i := Low(FLUT.luts[j]) to High(FLUT.luts[j]) do
      begin
        AStream.Read(FLUT.luts[j, i], 1);
      end;
    end;

    for i := 0 to 4 do
    begin
      AStream.Read(LIntValue, 4);
      FCurves.CurveType[i] := LIntValue;
    end;

    for i := 0 to 4 do
    begin
      for j := 0 to 16 do
      begin
        for k := 0 to 1 do
        begin
          AStream.Read(LIntValue, 4);
          FCurves.Points[i, j, k] := LIntValue;
        end;
      end;
    end;

    for i := 0 to 4 do
    begin
      for j := 0 to 255 do
      begin
        AStream.Read(FCurves.FCurve[i, j], 1);
      end;
    end;

    AStream.Read(FChannel, 4);

    AStream.Read(LIntValue, 4);
    Self.Scale := TgmGimpHistogramScale(LIntValue); // setting up by property not field

    AStream.Read(LIntValue, 4);
    Self.CurveType := LIntValue; // setting up by property not field

    Result := True;
  end;
end; 

// save Curves settings to a stream
procedure TgmCurvesTool.SaveToStream(const AStream: TStream);
var
  LFileHeader        : TgmCurvesFileHeader;
  i, j, k, LIntValue : Integer;
begin
  if Assigned(AStream) then
  begin
    // write in header data
    LFileHeader.ID      := CURVES_FILE_ID;
    LFileHeader.Version := CURVES_FILE_VERSION;
    AStream.Write(LFileHeader, SizeOf(LFileHeader));

    for j := Low(FLUT.luts) to High(FLUT.luts) do
    begin
      for i := Low(FLUT.luts[j]) to High(FLUT.luts[j]) do
      begin
        AStream.Write(FLUT.luts[j, i], 1);
      end;
    end;

    for i := 0 to 4 do
    begin
      LIntValue := FCurves.CurveType[i];
      AStream.Write(LIntValue, 4);
    end;

    for i := 0 to 4 do
    begin
      for j := 0 to 16 do
      begin
        for k := 0 to 1 do
        begin
          LIntValue := FCurves.Points[i, j, k];
          AStream.Write(LIntValue, 4);
        end;
      end;
    end;

    for i := 0 to 4 do
    begin
      for j := 0 to 255 do
      begin
        AStream.Write(FCurves.FCurve[i, j], 1);
      end;
    end;

    AStream.Write(FChannel, 4);

    LIntValue := Ord(FScale);
    AStream.Write(LIntValue, 4);

    AStream.Write(FCurveType, 4);
  end;
end; 

// load Curves settings from file
function TgmCurvesTool.LoadFromFile(const AFileName: string): Boolean;
var
  LInputStream : TMemoryStream;
begin
  Result := False;

  if FileExists(AFileName) then
  begin
    LInputStream := TMemoryStream.Create();
    try
      LInputStream.LoadFromFile(AFileName);
      LInputStream.Position := 0;
      
      Result := Self.LoadFromStream(LInputStream);

      if Result = False then
      begin
        case FDataErrorType of
          cdetIDError:
            begin
              FOutputMsg :=
                'Cannot open the file "' +
                ExtractFileName(AFileName) + '",' + #10#13 +
                'because the file header data is not correct.'
            end;

          cdetVersionError:
            begin
              FOutputMsg :=
                'Cannot open the file "' +
                ExtractFileName(AFileName) + '",' + #10#13 +
                'because the file version is higher than this program supports.';
            end;
        end;
      end;

    finally
      LInputStream.Free();
    end;
  end;
end; 

// save Curves settings to file
procedure TgmCurvesTool.SaveToFile(const AFileName: string);
var
  LFileDir        : string;
  LFileExt        : string;
  LOutputFileName : string;
  LOutputStream   : TMemoryStream;
begin
  LOutputFileName := AFileName;
  LFileDir        := ExtractFileDir(LOutputFileName);

  if (LFileDir <> '') and DirectoryExists(LFileDir) then
  begin
    LFileExt := ExtractFileExt(LOutputFileName);

    if LFileExt = '' then
    begin
      LOutputFileName := LOutputFileName + CURVES_FILE_EXT;
    end
    else
    if LFileExt <> CURVES_FILE_EXT then
    begin
      LOutputFileName := ChangeFileExt(LOutputFileName, CURVES_FILE_EXT);
    end;

    LOutputStream := TMemoryStream.Create();
    try
      Self.SaveToStream(LOutputStream);  // write in curves settings

      // save to file
      LOutputStream.Position := 0;
      LOutputStream.SaveToFile(LOutputFileName);
    finally
      LOutputStream.Free();
    end;
  end;
end; 

end.
