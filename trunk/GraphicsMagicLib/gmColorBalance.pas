{ This library created in 09/04/2006.
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on the the gimp color_balance.c and color_balance.h from GIMP 1.2.4 .
  The original source can be found at www.gimp.org.

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
  Boston, MA 02111-1307, USA.

  Thanks to the authors of GIMP for giving us the opportunity to know how to
  achieve color balance. But we don't know the principles of these algorithms,
  if you know it, please let us know. }

unit gmColorBalance;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Classes,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmSelection;

type
  TgmTransferMode = (tmShadows, tmMidtones, tmHighlights);

  TColorTransferArray = array [0..255] of Double;
  PColorTransferArray = ^TColorTransferArray;

//-- TgmColorBalance -----------------------------------------------------------

  TgmColorBalance = class(TObject)
  private
    FSourceBitmap: TBitmap32;
    FRedLookup   : array [0..255] of Byte;
    FGreenLookup : array [0..255] of Byte;
    FBlueLookup  : array [0..255] of Byte;
    
    { Adjustment value in three modes
      0: Shadows; 1: Midtones; 2: Highlights }
    FCyanRed     : array [0..2] of Integer;
    FMagentaGreen: array [0..2] of Integer;
    FYellowBlue  : array [0..2] of Integer;

    FPreserveLuminosity: Boolean;
    FTransferMode      : TgmTransferMode;

    procedure Initialize;
    procedure CreateLookupTables;
    procedure GimpColorBalance(const ADestBmp: TBitmap32);

    function GetCyanRed: Integer;
    function GetMagentaGreen: Integer;
    function GetYellowBlue: Integer;

    function GetFromRedLookup(AIndex: Integer): Byte;
    function GetFromGreenLookup(AIndex: Integer): Byte;
    function GetFromBlueLookup(AIndex: Integer): Byte;

    function GetCyanRedWithIndex(AIndex: Integer): Integer;
    function GetMagentaGreenWithIndex(AIndex: Integer): Integer;
    function GetYellowBlueWithIndex(AIndex: Integer): Integer;

    // With lookup tables calculation capability.
    procedure SetCyanRed(const AValue: Integer);
    procedure SetMagentaGreen(const AValue: Integer);
    procedure SetYellowBlue(const AValue: Integer);

    procedure SetSourceBitmap(const ASourceBmp: TBitmap32);
    procedure SetPreserveLuminosity(const AValue: Boolean);
    procedure SetTransferMode(const AValue: TgmTransferMode);

    procedure SetToRedLookup(AIndex: Integer; AValue: Byte);
    procedure SetToGreenLookup(AIndex: Integer; AValue: Byte);
    procedure SetToBlueLookup(AIndex: Integer; AValue: Byte);

    // Do not with lookup tables calculation capability.
    procedure SetCyanRedWithIndex(AIndex, AValue: Integer);
    procedure SetMagentaGreenWithIndex(AIndex, AValue: Integer);
    procedure SetYellowBlueWithIndex(AIndex, AValue: Integer);
  public
    constructor Create(const ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure Execute(const ADestBmp: TBitmap32);
    procedure Reset;

    // load color balance settings from a stream
    function LoadFromStream(const AStream: TStream): Boolean;
    
    // save color balance settings to a stream
    procedure SaveToStream(const AStream: TStream);

    // With lookup tables calculation capability.
    property CyanRed           : Integer         read GetCyanRed          write SetCyanRed;
    property MagentaGreen      : Integer         read GetMagentaGreen     write SetMagentaGreen;
    property YellowBlue        : Integer         read GetYellowBlue       write SetYellowBlue;

    property SourceBitmap      : TBitmap32       read FSourceBitmap       write SetSourceBitmap;
    property PreserveLuminosity: Boolean         read FPreserveLuminosity write SetPreserveLuminosity;
    property TransferMode      : TgmTransferMode read FTransferMode       write SetTransferMode;

    property RedLookup  [index: Integer]: Byte   read GetFromRedLookup    write SetToRedLookup;
    property GreenLookup[index: Integer]: Byte   read GetFromGreenLookup  write SetToGreenLookup;
    property BlueLookup [index: Integer]: Byte   read GetFromBlueLookup   write SetToBlueLookup;

    // Do not with lookup tables calculation capability.
    property CyanRedArray     [index: Integer]: Integer read GetCyanRedWithIndex      write SetCyanRedWithIndex;
    property MagentaGreenArray[index: Integer]: Integer read GetMagentaGreenWithIndex write SetMagentaGreenWithIndex;
    property YellowBlueArray  [index: Integer]: Integer read GetYellowBlueWithIndex   write SetYellowBlueWithIndex;
  end;

const
  SHADOWS   : Integer = 0;
  MIDTONES  : Integer = 1;
  HIGHLIGHTS: Integer = 2;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmColorTransfer, gmColorSpace;

//-- TgmColorBalance -----------------------------------------------------------

constructor TgmColorBalance.Create(const ASourceBmp: TBitmap32);
begin
  inherited Create;
  
  FSourceBitmap := TBitmap32.Create;
  FSourceBitmap.Assign(ASourceBmp);

  FPreserveLuminosity := True;
  FTransferMode       := tmMidtones;
  Initialize;
end;

destructor TgmColorBalance.Destroy;
begin
  FSourceBitmap.Free;
  inherited Destroy;
end; 

procedure TgmColorBalance.Initialize;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    FCyanRed[i]      := 0;
    FMagentaGreen[i] := 0;
    FYellowBlue[i]   := 0;
  end;
  
  CreateLookupTables;
end;

procedure TgmColorBalance.CreateLookupTables;
var
  LCyanRedTransfer     : array [0..2] of PColorTransferArray;
  LMagentaGreenTransfer: array [0..2] of PColorTransferArray;
  LYellowBlueTransfer  : array [0..2] of PColorTransferArray;
  i, rn, gn, bn        : Integer;
begin
  // Set the transfer arrays  (for speed)
  // Cyan_Red
  if FCyanRed[SHADOWS] > 0 then
  begin
    LCyanRedTransfer[SHADOWS] := @ShadowsAdd;
  end
  else
  begin
    LCyanRedTransfer[SHADOWS] := @ShadowsSub;
  end;

  if FCyanRed[MIDTONES] > 0 then
  begin
    LCyanRedTransfer[MIDTONES] := @MidtonesAdd;
  end
  else
  begin
    LCyanRedTransfer[MIDTONES] := @MidtonesSub;
  end;

  if FCyanRed[HIGHLIGHTS] > 0 then
  begin
    LCyanRedTransfer[HIGHLIGHTS] := @HighlightsAdd;
  end
  else
  begin
    LCyanRedTransfer[HIGHLIGHTS] := @HighlightsSub;
  end;

  // Magenta_Green
  if FMagentaGreen[SHADOWS] > 0 then
  begin
    LMagentaGreenTransfer[SHADOWS] := @ShadowsAdd;
  end
  else
  begin
    LMagentaGreenTransfer[SHADOWS] := @ShadowsSub;
  end;

  if FMagentaGreen[MIDTONES] > 0 then
  begin
    LMagentaGreenTransfer[MIDTONES] := @MidtonesAdd;
  end
  else
  begin
    LMagentaGreenTransfer[MIDTONES] := @MidtonesSub;
  end;

  if FMagentaGreen[HIGHLIGHTS] > 0 then
  begin
    LMagentaGreenTransfer[HIGHLIGHTS] := @HighlightsAdd;
  end
  else
  begin
    LMagentaGreenTransfer[HIGHLIGHTS] := @HighlightsSub;
  end;

  // Yellow_Blue
  if FYellowBlue[SHADOWS] > 0 then
  begin
    LYellowBlueTransfer[SHADOWS] := @ShadowsAdd;
  end
  else
  begin
    LYellowBlueTransfer[SHADOWS] := @ShadowsSub;
  end;

  if FYellowBlue[MIDTONES] > 0 then
  begin
    LYellowBlueTransfer[MIDTONES] := @MidtonesAdd;
  end
  else
  begin
    LYellowBlueTransfer[MIDTONES] := @MidtonesSub;
  end;

  if FYellowBlue[HIGHLIGHTS] > 0 then
  begin
    LYellowBlueTransfer[HIGHLIGHTS] := @HighlightsAdd;
  end
  else
  begin
    LYellowBlueTransfer[HIGHLIGHTS] := @HighlightsSub;
  end;

  for i := 0 to 255 do
  begin
    rn := i;
    gn := i;
    bn := i;

    rn := rn + Round(FCyanRed[SHADOWS] * LCyanRedTransfer[SHADOWS, rn]);
    rn := Clamp(rn, 0, 255);
    rn := rn + Round(FCyanRed[MIDTONES] * LCyanRedTransfer[MIDTONES, rn]);
    rn := Clamp(rn, 0, 255);
    rn := rn + Round(FCyanRed[HIGHLIGHTS] * LCyanRedTransfer[HIGHLIGHTS, rn]);
    rn := Clamp(rn, 0, 255);

    gn := gn + Round(FMagentaGreen[SHADOWS] * LMagentaGreenTransfer[SHADOWS, gn]);
    gn := Clamp(gn, 0, 255);
    gn := gn + Round(FMagentaGreen[MIDTONES] * LMagentaGreenTransfer[MIDTONES, gn]);
    gn := Clamp(gn, 0, 255);
    gn := gn + Round(FMagentaGreen[HIGHLIGHTS] * LMagentaGreenTransfer[HIGHLIGHTS, gn]);
    gn := Clamp(gn, 0, 255);

    bn := bn + Round(FYellowBlue[SHADOWS] * LYellowBlueTransfer[SHADOWS, bn]);
    bn := Clamp(bn, 0, 255);
    bn := bn + Round(FYellowBlue[MIDTONES] * LYellowBlueTransfer[MIDTONES, bn]);
    bn := Clamp(bn, 0, 255);
    bn := bn + Round(FYellowBlue[HIGHLIGHTS] * LYellowBlueTransfer[HIGHLIGHTS, bn]);
    bn := Clamp(bn, 0, 255);

    FRedLookup[i]   := rn;
    FGreenLookup[i] := gn;
    FBlueLookup[i]  := bn;
  end;
end;

procedure TgmColorBalance.GimpColorBalance(const ADestBmp: TBitmap32);
var
  i, H, L, S        : Integer;
  A, R, G, B        : Byte;
  LSrcBits, LDstBits: PColor32;
begin
  ADestBmp.SetSize(FSourceBitmap.Width, FSourceBitmap.Height);

  LSrcBits := @FSourceBitmap.Bits[0];
  LDstBits := @ADestBmp.Bits[0];
  
  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    // preserve the Alpha channel of the destination bitmap
    A := LDstBits^ shr 24 and $FF;

    // extract the RGB values from the source bitmap
    R := LSrcBits^ shr 16 and $FF;
    G := LSrcBits^ shr  8 and $FF;
    B := LSrcBits^        and $FF;
    
    // get the new RGB values from lookup tables
    R := FRedLookup[R];
    G := FGreenLookup[G];
    B := FBlueLookup[B];
    
    // modify the RGB values of the destination bitmap
    LDstBits^ := (A shl 24) or (R shl 16) or (G shl 8) or B;

    if FPreserveLuminosity then
    begin
      // get the new HLS
      RGBToHLS32(LDstBits^, H, L, S);
      S := Clamp(S, 1, 255);

      // get the original L
      L := Clamp( RGBToLightness32(LSrcBits^), 0, 255 );

      // combine the new H, S and the original L
      LDstBits^ := HLSToRGB32(A, H, L, S);
    end;

    // go to the next pixel entry
    Inc(LSrcBits);
    Inc(LDstBits);
  end;
end;

function TgmColorBalance.GetCyanRed: Integer;
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);
  Result := FCyanRed[LIndex];
end;

function TgmColorBalance.GetMagentaGreen: Integer;
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);
  Result := FMagentaGreen[LIndex];
end;

function TgmColorBalance.GetYellowBlue: Integer;
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);
  Result := FYellowBlue[LIndex];
end;

function TgmColorBalance.GetFromRedLookup(AIndex: Integer): Byte;
begin
  if AIndex in [0..255] then
  begin
    Result := FRedLookup[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmColorBalance.GetFromGreenLookup(AIndex: Integer): Byte;
begin
  if AIndex in [0..255] then
  begin
    Result := FGreenLookup[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmColorBalance.GetFromBlueLookup(AIndex: Integer): Byte;
begin
  if AIndex in [0..255] then
  begin
    Result := FBlueLookup[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmColorBalance.GetCyanRedWithIndex(AIndex: Integer): Integer;
begin
  if AIndex in [0..2] then
  begin
    Result := FCyanRed[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

function TgmColorBalance.GetMagentaGreenWithIndex(AIndex: Integer): Integer;
begin
  if AIndex in [0..2] then
  begin
    Result := FMagentaGreen[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end;

function TgmColorBalance.GetYellowBlueWithIndex(AIndex: Integer): Integer;
begin
  if AIndex in [0..2] then
  begin
    Result := FYellowBlue[AIndex];
  end
  else
  begin
    Result := 0;
  end;
end; 

procedure TgmColorBalance.SetCyanRed(const AValue: Integer);
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);

  if FCyanRed[LIndex] <> AValue then
  begin
    FCyanRed[LIndex] := AValue;
    CreateLookupTables;
  end;
end; 

procedure TgmColorBalance.SetMagentaGreen(const AValue: Integer);
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);

  if FMagentaGreen[LIndex] <> AValue then
  begin
    FMagentaGreen[LIndex] := AValue;
    CreateLookupTables;
  end;
end;

procedure TgmColorBalance.SetYellowBlue(const AValue: Integer);
var
  LIndex: Integer;
begin
  LIndex := Ord(FTransferMode);

  if FYellowBlue[LIndex] <> AValue then
  begin
    FYellowBlue[LIndex] := AValue;
    CreateLookupTables;
  end;
end; 

procedure TgmColorBalance.SetSourceBitmap(const ASourceBmp: TBitmap32);
begin
  FSourceBitmap.Assign(ASourceBmp);
end;

procedure TgmColorBalance.SetPreserveLuminosity(const AValue: Boolean);
begin
  if FPreserveLuminosity <> AValue then
  begin
    FPreserveLuminosity := AValue;
  end;
end;

procedure TgmColorBalance.SetTransferMode(const AValue: TgmTransferMode);
begin
  if FTransferMode <> AValue then
  begin
    FTransferMode := AValue;
  end;
end; 

procedure TgmColorBalance.SetToRedLookup(AIndex: Integer; AValue: Byte);
begin
  if AIndex in [0..255] then
  begin
    FRedLookup[AIndex] := AValue;
  end;
end; 

procedure TgmColorBalance.SetToGreenLookup(AIndex: Integer; AValue: Byte);
begin
  if AIndex in [0..255] then
  begin
    FGreenLookup[AIndex] := AValue;
  end;
end;

procedure TgmColorBalance.SetToBlueLookup(AIndex: Integer; AValue: Byte);
begin
  if AIndex in [0..255] then
  begin
    FBlueLookup[AIndex] := AValue;
  end;
end;

procedure TgmColorBalance.SetCyanRedWithIndex(AIndex, AValue: Integer);
begin
  if AIndex in [0..2] then
  begin
    FCyanRed[AIndex] := AValue;
  end;
end;

procedure TgmColorBalance.SetMagentaGreenWithIndex(AIndex, AValue: Integer);
begin
  if AIndex in [0..2] then
  begin
    FMagentaGreen[AIndex] := AValue;
  end;
end;

procedure TgmColorBalance.SetYellowBlueWithIndex(AIndex, AValue: Integer);
begin
  if AIndex in [0..2] then
  begin
    FYellowBlue[AIndex] := AValue;
  end;
end;

procedure TgmColorBalance.Execute(const ADestBmp: TBitmap32);
begin
  GimpColorBalance(ADestBmp);
end;

procedure TgmColorBalance.Reset;
var
  LIndex: Integer;
begin
  LIndex                := Ord(FTransferMode);
  FCyanRed[LIndex]      := 0;
  FMagentaGreen[LIndex] := 0;
  FYellowBlue[LIndex]   := 0;
  
  CreateLookupTables;
end; 

// load color balance settints from a stream
function TgmColorBalance.LoadFromStream(const AStream: TStream): Boolean;
var
  i, LTemp: Integer;
begin
  Result := False;

  if Assigned(AStream) then
  begin
    for i := 0 to 2 do
    begin
      AStream.Read(FCyanRed[i],      4);
      AStream.Read(FMagentaGreen[i], 4);
      AStream.Read(FYellowBlue[i],   4);
    end;

    for i := 0 to 255 do
    begin
      AStream.Read(FRedLookup[i],   1);
      AStream.Read(FGreenLookup[i], 1);
      AStream.Read(FBlueLookup[i],  1);
    end;

    AStream.Read(FPreserveLuminosity, 1);
    AStream.Read(LTemp, 4);

    FTransferMode := TgmTransferMode(LTemp);

    Result := True;
  end;
end;

// save color balance settings to a stream
procedure TgmColorBalance.SaveToStream(const AStream: TStream);
var
  i, LTemp: Integer;
begin
  if Assigned(AStream) then
  begin
    for i := 0 to 2 do
    begin
      AStream.Write(FCyanRed[i],      4);
      AStream.Write(FMagentaGreen[i], 4);
      AStream.Write(FYellowBlue[i],   4);
    end;

    for i := 0 to 255 do
    begin
      AStream.Write(FRedLookup[i],   1);
      AStream.Write(FGreenLookup[i], 1);
      AStream.Write(FBlueLookup[i],  1);
    end;

    AStream.Write(FPreserveLuminosity, 1);

    LTemp := Ord(FTransferMode);
    AStream.Write(LTemp, 4);
  end;
end; 

end.
