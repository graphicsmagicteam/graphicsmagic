////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorServices.pas
// Components: TPegtopColorDefinitionService
// Version:    1.01
// Date:       25 Jan 2005 created 1.00
//             18 Apr 2005 modified 1.01 (color gradient support added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorClipboard manages clipboard operations for colors and color
// gradients. Use the predefined ColorClipboard to minimize overhead.
// TPegtopColorDefinitionService manages user defined colors (synchronized
// between applications). Use the predefined ColorDefinitionService to
// minimize overhead.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorServices;

interface

uses
  Windows, Messages, Classes, Graphics, Clipbrd,
  PegtopMessageReceivers, PegtopMemoryMappedFiles, PegtopMethodHashes,
  PegtopColorGradients;

type
  TPegtopColorClipboard = class
  private
    function GetSystemColorData(var Color: TColor): Boolean;
    function GetRGBAColorData(var Color: Longword): Boolean;
    function GetTextColorData(var Color: TColor): Boolean;
    function SetSystemColorData(const Color: TColor): Boolean;
    function SetRGBAColorData(const Color: Longword): Boolean;
    function SetTextColorData(const Color: TColor): Boolean;
  public
    function GetSystemColor(var Color: TColor): Boolean;
    function GetRGBAColor(var Color: Longword): Boolean;
    function GetColorGradient(const Gradient: TPegtopCustomColorGradient): Boolean;
    function SetSystemColor(const Color: TColor): Boolean;
    function SetRGBAColor(const Color: Longword): Boolean;
    function SetColorGradient(const Gradient: TPegtopCustomColorGradient): Boolean;
    function HasSystemColor: Boolean;
    function HasRGBAColor: Boolean;
    function HasColor: Boolean;
    function HasColorGradient: Boolean;
  end;

  TPegtopUserColorChangeEvent = procedure (Sender: TObject; Index: Integer; Color: TColor) of object;

  TPegtopUserDefinedColor = packed record
    Color: Longword;
    Flags: Longword;
  end;
  PPegtopUserDefinedColor = ^TPegtopUserDefinedColor;

  TPegtopUserDefinedColors = packed record
    Size: Integer;
    Colors: packed array[0..31] of TPegtopUserDefinedColor;
  end;

  TPegtopColorDialogWindows = packed record
    Size: Integer;
    Handles: packed array[0..63] of THandle;
  end;

  TPegtopColorDialogSharedData = packed record
    Windows: TPegtopColorDialogWindows;
    Colors: TPegtopUserDefinedColors;
  end;
  PPegtopColorDialogSharedData = ^TPegtopColorDialogSharedData;

  TPegtopColorDefinitionService = class
  private
    FSharedMemory: TPegtopMutexSharedMemory;
    FWindowIndex: Integer;
    FColorChangeEvents: TPegtopMethodHash;
    FWindowHandle: THandle;
    procedure WndProc(var Msg: TMessage);
    procedure NotifyColorChange(const Index: Integer; Color: TColor);
    function GetColor(Index: Integer): TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    function GetSharedData: PPegtopColorDialogSharedData;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); virtual;
    function GetServiceName: String; virtual;
    function GetRegistryKey: String; virtual;
    procedure LoadColors; virtual;
    procedure SaveColors; virtual;
    procedure ReadColorsFromRegistry(const RegKey: String);
    procedure WriteColorsToRegistry(const RegKey: String);
    property SharedData: PPegtopColorDialogSharedData read GetSharedData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddColorChangeEvent(const Event: TPegtopUserColorChangeEvent);
    procedure RemoveColorChangeEvent(const Event: TPegtopUserColorChangeEvent);
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
  end;

function ColorClipboard: TPegtopColorClipboard;
function ColorDefinitionService: TPegtopColorDefinitionService;
procedure SetColorDefinitionService(Service: TPegtopColorDefinitionService);

var
  CF_SYSTEM_COLOR: Word;
  CF_RGBA_COLOR: Word;
  CF_COLOR_GRADIENT: Word;

implementation

uses
  SysUtils, Forms, Registry, PegtopColorUtils, PegtopBufferStreams;

const
  WM_SETUSERCOLOR = WM_USER + 1;

var
  GlobalColorClipboard: TPegtopColorClipboard;
  GlobalColorDefinitionService: TPegtopColorDefinitionService;

function ColorClipboard: TPegtopColorClipboard;
begin
  if GlobalColorClipboard = NIL then
    GlobalColorClipboard := TPegtopColorClipboard.Create;
  Result := GlobalColorClipboard;
end;

function ColorDefinitionService: TPegtopColorDefinitionService;
begin
  if GlobalColorDefinitionService = NIL then begin
    GlobalColorDefinitionService := TPegtopColorDefinitionService.Create;
  end;
  Result := GlobalColorDefinitionService;
end;

procedure SetColorDefinitionService(Service: TPegtopColorDefinitionService);
begin
  if GlobalColorDefinitionService <> NIL then GlobalColorDefinitionService.Free;
  GlobalColorDefinitionService := Service;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorClipboard
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorClipboard.GetSystemColor(var Color: TColor): Boolean;
begin
  Result := False;
  if OpenClipboard(0) then try
    Result := GetSystemColorData(Color);
    if not Result then begin
      Result := GetRGBAColorData(Longword(Color));
      if Result then
        Color := Color and $FFFFFF // remove alpha byte of color
      else
        Result := GetTextColorData(Color);
    end;
  finally
    CloseClipboard;
  end;
end;

function TPegtopColorClipboard.GetRGBAColor(var Color: Longword): Boolean;
begin
  Result := False;
  if OpenClipboard(0) then try
    Result := GetRGBAColorData(Color);
    if not Result then begin
      // no RGBA color found, so try other color formats:
      Result := GetSystemColorData(TColor(Color));
      if not Result then
        Result := GetTextColorData(TColor(Color));
      if Result then
        Color := Longword(ColorToRGB(Color)) or $FF000000 // convert to RGBA (with alpha = 100%)
    end;
  finally
    CloseClipboard;
  end;
end;

function TPegtopColorClipboard.GetColorGradient(const Gradient: TPegtopCustomColorGradient): Boolean;
var
  DataHandle: THandle;
  Stream: TPegtopGlobalMemoryStream;
begin
  Result := False;
  if OpenClipboard(0) then try
    DataHandle := GetClipboardData(CF_COLOR_GRADIENT);
    if DataHandle <> 0 then begin
      Stream := TPegtopGlobalMemoryStream.Create(DataHandle);
      try
        Gradient.LoadFromStream(Stream);
        Result := True;
      finally
        Stream.Free;
      end;
    end;
  finally
    CloseClipboard;
  end;
end;

function TPegtopColorClipboard.SetSystemColor(const Color: TColor): Boolean;
begin
  Result := False;
  if OpenClipboard(0) then try
    if EmptyClipboard then begin
      Result := SetSystemColorData(Color)
      and SetRGBAColorData(Longword(ColorToRGB(Color)) or $FF000000) // convert to RGBA (with alpha = 100%)
      and SetTextColorData(ColorToRGB(Color)); // convert to RGB
    end;
  finally
    CloseClipboard;
  end;
end;

function TPegtopColorClipboard.SetRGBAColor(const Color: Longword): Boolean;
begin
  Result := False;
  if OpenClipboard(0) then try
    if EmptyClipboard then begin
      Result := SetRGBAColorData(Color)
      and SetSystemColorData(Color and $FFFFFF) // remove alpha byte of color
      and SetTextColorData(Color and $FFFFFF); // remove alpha byte of color
    end;
  finally
    CloseClipboard;
  end;
end;

function TPegtopColorClipboard.SetColorGradient(const Gradient: TPegtopCustomColorGradient): Boolean;
var
  DataHandle: THandle;
  Stream: TMemoryStream;
  GradientPtr: Pointer;
begin
  Result := False;
  Stream := TMemoryStream.Create;
  try
    Gradient.SaveToStream(Stream);
    if OpenClipboard(0) then try
      if EmptyClipboard then begin
        DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Stream.Size);
        if DataHandle <> 0 then begin
          GradientPtr := GlobalLock(DataHandle);
          if GradientPtr <> NIL then try
            Move(Stream.Memory^, GradientPtr^, Stream.Size);
            Result := SetClipboardData(CF_COLOR_GRADIENT, DataHandle) <> 0;
          finally
            GlobalUnlock(DataHandle);
          end;
        end;
      end;
    finally
      CloseClipboard;
    end;
  finally
    Stream.Free;
  end;
end;

function TPegtopColorClipboard.HasSystemColor: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_SYSTEM_COLOR);
end;

function TPegtopColorClipboard.HasRGBAColor: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_RGBA_COLOR);
end;

function TPegtopColorClipboard.HasColor: Boolean;
var
  C: TColor;
begin
  Result := IsClipboardFormatAvailable(CF_SYSTEM_COLOR)
  or IsClipboardFormatAvailable(CF_RGBA_COLOR)
  or (IsClipboardFormatAvailable(CF_TEXT) and GetSystemColor(C));
end;

function TPegtopColorClipboard.HasColorGradient: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_COLOR_GRADIENT);
end;

function TPegtopColorClipboard.GetSystemColorData(var Color: TColor): Boolean;
var
  DataHandle: THandle;
  ColorPtr: ^TColor;
begin
  Result := False;
  DataHandle := GetClipboardData(CF_SYSTEM_COLOR);
  if DataHandle <> 0 then begin
    ColorPtr := GlobalLock(DataHandle);
    if ColorPtr <> NIL then try
      Color := ColorPtr^;
      Result := True;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

function TPegtopColorClipboard.GetRGBAColorData(var Color: Longword): Boolean;
var
  DataHandle: THandle;
  ColorPtr: ^Longword;
begin
  Result := False;
  DataHandle := GetClipboardData(CF_RGBA_COLOR);
  if DataHandle <> 0 then begin
    ColorPtr := GlobalLock(DataHandle);
    if ColorPtr <> NIL then try
      Color := ColorPtr^;
      Result := True;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

function TPegtopColorClipboard.GetTextColorData(var Color: TColor): Boolean;
var
  DataHandle: THandle;
  HexPtr: PChar;
  C: TColor;
begin
  Result := False;
  DataHandle := GetClipboardData(CF_TEXT);
  if DataHandle <> 0 then begin
    HexPtr := GlobalLock(DataHandle);
    if HexPtr <> NIL then try
      if StrLen(HexPtr) = 6 then begin
        C := SwapColorBytes(TPegtopColor(StrToIntDef('$' + HexPtr, -1))).Def;
        if C <> -1 then begin
          Color := C;
          Result := True;
        end;
      end;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

function TPegtopColorClipboard.SetSystemColorData(const Color: TColor): Boolean;
var
  DataHandle: THandle;
  ColorPtr: ^TColor;
begin
  Result := False;
  DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, 4);
  if DataHandle <> 0 then begin
    ColorPtr := GlobalLock(DataHandle);
    if ColorPtr <> NIL then try
      ColorPtr^ := Color;
      Result := SetClipboardData(CF_SYSTEM_COLOR, DataHandle) <> 0;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

function TPegtopColorClipboard.SetRGBAColorData(const Color: Longword): Boolean;
var
  DataHandle: THandle;
  ColorPtr: ^Longword;
begin
  Result := False;
  DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, 4);
  if DataHandle <> 0 then begin
    ColorPtr := GlobalLock(DataHandle);
    if ColorPtr <> NIL then try
      ColorPtr^ := Color;
      Result := SetClipboardData(CF_RGBA_COLOR, DataHandle) <> 0;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

function TPegtopColorClipboard.SetTextColorData(const Color: TColor): Boolean;
var
  DataHandle: THandle;
  HexText: String;
  HexPtr: PChar;
begin
  Result := False;
  HexText := IntToHex(SwapColorBytes(TPegtopColor(Color)).Def, 6);
  DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, 7);
  if DataHandle <> 0 then begin
    HexPtr := GlobalLock(DataHandle);
    if HexPtr <> NIL then try
      Move(HexText[1], HexPtr^, 7);
      Result := SetClipboardData(CF_TEXT, DataHandle) <> 0;
    finally
      GlobalUnlock(DataHandle);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorDefinitionService
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorDefinitionService.Create;
var
  I: Integer;
begin
  FWindowHandle := AllocateHWnd(WndProc);
  FWindowIndex := -1;
  FSharedMemory := TPegtopMutexSharedMemory.Create(GetServiceName, SizeOf(TPegtopColorDialogWindows));
  if FSharedMemory.IsFirstInstance then begin
    // init shared memory:
    SharedData^.Windows.Size := Length(SharedData^.Windows.Handles);
    for I := Low(SharedData^.Windows.Handles) to High(SharedData^.Windows.Handles) do begin
      SharedData^.Windows.Handles[I] := 0;
    end;
    SharedData^.Colors.Size := Length(SharedData^.Colors.Colors);
    // init with default colors:
    for I := Low(SharedData^.Colors.Colors) to High(SharedData^.Colors.Colors) do begin
      SharedData^.Colors.Colors[I].Color := $FFFFFF;
      SharedData^.Colors.Colors[I].Flags := 0;
    end;
    SharedData^.Colors.Colors[0].Color := $000000;
    SharedData^.Colors.Colors[1].Color := $404040;
    SharedData^.Colors.Colors[2].Color := $808080;
    SharedData^.Colors.Colors[3].Color := $C0C0C0;
    SharedData^.Colors.Colors[4].Color := $FFFFFF;
    // replace with user defined colors:
    LoadColors;
  end;
  // join window handle list:
  FSharedMemory.Enter;
  try
    I := 0;
    while (I < SharedData^.Windows.Size)
    and (SharedData^.Windows.Handles[I] <> 0) do Inc(I);
    if I < SharedData^.Windows.Size then begin
      FWindowIndex := I;
      SharedData^.Windows.Handles[FWindowIndex] := FWindowHandle;
    end
    else begin
      FWindowIndex := -1;
    end;
  finally
    FSharedMemory.Leave;
  end;
  // init event hash:
  FColorChangeEvents := TPegtopMethodHash.Create;
end;

destructor TPegtopColorDefinitionService.Destroy;
begin
  // free window handle:
  DeAllocateHWnd(FWindowHandle);
  // free event hash:
  FColorChangeEvents.Free;
  // leave window handle list:
  if FWindowIndex <> -1 then begin
    FSharedMemory.Enter;
    try
      SharedData^.Windows.Handles[FWindowIndex] := 0;
      FWindowIndex := -1;
    finally
      FSharedMemory.Leave;
    end;
  end;
  FSharedMemory.Free;
  inherited;
end;

procedure TPegtopColorDefinitionService.WndProc(var Msg: TMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  HandleMessage(Msg, Handled);
  if not Handled then
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

function TPegtopColorDefinitionService.GetServiceName: String;
begin
  Result := 'PegtopColorDefinitionService';
end;

function TPegtopColorDefinitionService.GetRegistryKey: String;
begin
  Result := '\Software\Pegtop\Colors';
end;

procedure TPegtopColorDefinitionService.LoadColors;
begin
  ReadColorsFromRegistry(GetRegistryKey);
end;

procedure TPegtopColorDefinitionService.SaveColors;
begin
  WriteColorsToRegistry(GetRegistryKey);
end;

procedure TPegtopColorDefinitionService.ReadColorsFromRegistry(const RegKey: String);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKeyReadOnly(RegKey) then begin
      Reg.ReadBinaryData('UserDefined', SharedData^.Colors.Colors, SizeOf(SharedData^.Colors.Colors));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TPegtopColorDefinitionService.WriteColorsToRegistry(const RegKey: String);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(RegKey, True) then begin
      Reg.WriteBinaryData('UserDefined', SharedData^.Colors.Colors, SizeOf(SharedData^.Colors.Colors));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TPegtopColorDefinitionService.AddColorChangeEvent(const Event: TPegtopUserColorChangeEvent);
begin
  FColorChangeEvents.Add(TMethod(Event), NIL);
end;

procedure TPegtopColorDefinitionService.RemoveColorChangeEvent(const Event: TPegtopUserColorChangeEvent);
begin
  FColorChangeEvents.Remove(TMethod(Event));
end;

procedure TPegtopColorDefinitionService.NotifyColorChange(const Index: Integer; Color: TColor);
var
  I: Integer;
  Event: TPegtopUserColorChangeEvent;
begin
  for I := 0 to FColorChangeEvents.Count - 1 do begin
    Event := TPegtopUserColorChangeEvent(FColorChangeEvents.Key[I]);
    Event(Self, Index, Color);
  end;
end;

procedure TPegtopColorDefinitionService.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_SETUSERCOLOR then begin
    NotifyColorChange(Msg.WParam, Msg.LParam);
    Handled := True;
  end;
end;

function TPegtopColorDefinitionService.GetColor(Index: Integer): TColor;
begin
  if (Index >= Low(SharedData^.Colors.Colors))
  and (Index <= High(SharedData^.Colors.Colors)) then begin
    FSharedMemory.Enter;
    try
      Result := SharedData^.Colors.Colors[Index].Color;
    finally
      FSharedMemory.Leave;
    end;
  end
  else begin
    Result := -1;
  end;
end;

procedure TPegtopColorDefinitionService.SetColor(Index: Integer; Value: TColor);
var
  J: Integer;
begin
  if (Index >= Low(SharedData^.Colors.Colors))
  and (Index <= High(SharedData^.Colors.Colors)) then begin
    FSharedMemory.Enter;
    try
      // set color:
      SharedData^.Colors.Colors[Index].Color := Value;
      // notify other instances:
      for J := Low(SharedData^.Windows.Handles) to High(SharedData^.Windows.Handles) do begin
        if (J <> FWindowIndex) and (SharedData^.Windows.Handles[J] <> 0) then begin
          PostMessage(SharedData^.Windows.Handles[J], WM_SETUSERCOLOR, Index, Value);
        end;
      end;
      SaveColors;
    finally
      FSharedMemory.Leave;
    end;
    // notify listeners of this instance
    // (after leaving shared memory to avoid deadlocks):
    NotifyColorChange(Index, Value);
  end;
end;

function TPegtopColorDefinitionService.GetSharedData: PPegtopColorDialogSharedData;
begin
  Result := FSharedMemory.Memory
end;

initialization
  CF_SYSTEM_COLOR := RegisterClipboardFormat('System Color');
  CF_RGBA_COLOR := RegisterClipboardFormat('RGBA Color');
  CF_COLOR_GRADIENT := RegisterClipboardFormat('Pegtop Color Gradient');
  GlobalColorClipboard := NIL;
  GlobalColorDefinitionService := NIL;
finalization
  GlobalColorDefinitionService.Free;
  GlobalColorClipboard.Free;
end.
