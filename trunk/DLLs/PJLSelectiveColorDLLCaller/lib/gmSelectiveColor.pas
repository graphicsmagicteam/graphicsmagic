unit gmSelectiveColor;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * Update Date: February 13, 2015
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
{ Delphi }
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  Forms,
{ Graphics32 }
  GR32;

type
  TgmSCChannel = (sccReds,
                  sccYellows,
                  sccGreens,
                  sccCyans,
                  sccBlues,
                  sccMagentas,
                  sccWhites,
                  sccNeutrals,
                  sccBlacks);


  TgmSCAdjustMode = (amRelative,
                     amAbsolute);
                  

  TgmSCSettings = record
    Cyan    : Integer; // -100 to 100
    Magenta : Integer; // -100 to 100
    Yellow  : Integer; // -100 to 100
    Black   : Integer; // -100 to 100
  end;


  TgmSelectiveColor = class
  private
    FAdjustMode   : TgmSCAdjustMode;
    FColorChannel : TgmSCChannel;
    FSettings     : array [0..8] of TgmSCSettings;

    function GetBlack(AChannel: Integer): Integer;
    function GetCyan(AChannel: Integer): Integer;
    function GetMagenta(AChannel: Integer): Integer;
    function GetYellow(AChannel: Integer): Integer;
    function ValidColorChannel(const AChannel: Integer): Boolean;
    
    procedure InitSettings;
    procedure SetBlack(AChannel: Integer; AValue: Integer);
    procedure SetCyan(AChannel: Integer; AValue: Integer);
    procedure SetMagenta(AChannel: Integer; AValue: Integer);
    procedure SetYellow(AChannel: Integer; AValue: Integer);
  public
    constructor Create;
    
    procedure Execute(ABitmap: TBitmap32);
    procedure Reset;

    property AdjustMode                : TgmSCAdjustMode read FAdjustMode write FAdjustMode;
    property Black[Channel: Integer]   : Integer         read GetBlack    write SetBlack;
    property Cyan[Channel: Integer]    : Integer         read GetCyan     write SetCyan;
    property Magenta[Channel: Integer] : Integer         read GetMagenta  write SetMagenta;
    property Yellow[Channel: Integer]  : Integer         read GetYellow   write SetYellow;
  end;

const
  SC_RED     = 0;
  SC_YELLOW  = 1;
  SC_GREEN   = 2;
  SC_CYAN    = 3;
  SC_BLUE    = 4;
  SC_MAGENTA = 5;
  SC_WHITE   = 6;
  SC_NEUTRAL = 7;
  SC_BLACK   = 8;

function SelectiveColorChannelList: TStringList;

type
  //
  // The original VB 6.0 code of Selective Color is written by Peng Jia Le.
  // Cannot release this piece of code temporarily. So we can only call it
  // from a DLL wrapper.
  //
  // Parameters:
  //
  //   PixelChannels  - channels of each pixel, it should be 3 or 4.
  //
  //   AbsoluteMode   - if greater than zero then in absolute mode,
  //                    in relative mode otherwise.
  //
  //   MultiplyAlpha  - if greater than zero then the new RGB channels
  //                    will multiply to the alpha channel respectively.
  //
  // Return Value:
  //
  //   Successed, return 1.
  //   Falied, return 0.
  //

  TSelectiveColorFunc = function(PixelPtr: PByte;
                                 ImageWidth: Integer;
                                 ImageHeight: Integer;
                                 PixelChannels: Integer;
                                 CyanRed: Integer;
                                 MagentaRed: Integer;
                                 YellowRed: Integer;
                                 BlackRed: Integer;
                                 CyanYellow: Integer;
                                 MagentaYellow: Integer;
                                 YellowYellow: Integer;
                                 BlackYellow: Integer;
                                 CyanGreen: Integer;
                                 MagentaGreen: Integer;
                                 YellowGreen: Integer;
                                 BlackGreen: Integer;
                                 CyanCyan: Integer;
                                 MagentaCyan: Integer;
                                 YellowCyan: Integer;
                                 BlackCyan: Integer;
                                 CyanBlue: Integer;
                                 MagentaBlue: Integer;
                                 YellowBlue: Integer;
                                 BlackBlue: Integer;
                                 CyanMagenta: Integer;
                                 MagentaMagenta: Integer;
                                 YellowMagenta: Integer;
                                 BlackMagenta: Integer;
                                 CyanWhite: Integer;
                                 MagentaWhite: Integer;
                                 YellowWhite: Integer;
                                 BlackWhite: Integer;
                                 CyanGray: Integer;
                                 MagentaGray: Integer;
                                 YellowGray: Integer;
                                 BlackGray: Integer;
                                 CyanBlack: Integer;
                                 MagentaBlack: Integer;
                                 YellowBlack: Integer;
                                 BlackBlack: Integer;
                                 AbsoluteMode: Integer;
                                 MultiplyAlpha: Integer): Integer; stdcall;

var
  SelectiveColor: TSelectiveColorFunc;
  

implementation

uses
{ Graphics32 }
  GR32_LowLevel;


function SelectiveColorChannelList: TStringList;
begin
  Result := TStringList.Create();

  with Result do
  begin
    Add('Reds');
    Add('Yellows');
    Add('Greens');
    Add('Cyans');
    Add('Blues');
    Add('Magentas');
    Add('Whites');
    Add('Neutrals');
    Add('Blacks');
  end;
end;


{ TgmSelectiveColor }

constructor TgmSelectiveColor.Create;
begin
  inherited Create();

  FAdjustMode   := amRelative;
  FColorChannel := sccReds;
  InitSettings();
end;

procedure TgmSelectiveColor.Execute(ABitmap: TBitmap32);
begin
  if Assigned(ABitmap) and
     (ABitmap.Width > 0) and
     (ABitmap.Height > 0) then
  begin
    SelectiveColor(@ABitmap.Bits[0],
                   ABitmap.Width,
                   ABitmap.Height,
                   4,
                   
                   FSettings[SC_RED].Cyan,
                   FSettings[SC_RED].Magenta,
                   FSettings[SC_RED].Yellow,
                   FSettings[SC_RED].Black,

                   FSettings[SC_YELLOW].Cyan,
                   FSettings[SC_YELLOW].Magenta,
                   FSettings[SC_YELLOW].Yellow,
                   FSettings[SC_YELLOW].Black,

                   FSettings[SC_GREEN].Cyan,
                   FSettings[SC_GREEN].Magenta,
                   FSettings[SC_GREEN].Yellow,
                   FSettings[SC_GREEN].Black,

                   FSettings[SC_CYAN].Cyan,
                   FSettings[SC_CYAN].Magenta,
                   FSettings[SC_CYAN].Yellow,
                   FSettings[SC_CYAN].Black,

                   FSettings[SC_BLUE].Cyan,
                   FSettings[SC_BLUE].Magenta,
                   FSettings[SC_BLUE].Yellow,
                   FSettings[SC_BLUE].Black,

                   FSettings[SC_MAGENTA].Cyan,
                   FSettings[SC_MAGENTA].Magenta,
                   FSettings[SC_MAGENTA].Yellow,
                   FSettings[SC_MAGENTA].Black,

                   FSettings[SC_WHITE].Cyan,
                   FSettings[SC_WHITE].Magenta,
                   FSettings[SC_WHITE].Yellow,
                   FSettings[SC_WHITE].Black,

                   FSettings[SC_NEUTRAL].Cyan,
                   FSettings[SC_NEUTRAL].Magenta,
                   FSettings[SC_NEUTRAL].Yellow,
                   FSettings[SC_NEUTRAL].Black,

                   FSettings[SC_BLACK].Cyan,
                   FSettings[SC_BLACK].Magenta,
                   FSettings[SC_BLACK].Yellow,
                   FSettings[SC_BLACK].Black,
                   
                   Ord(FAdjustMode),
                   Ord(False) );

  end;
end;

function TgmSelectiveColor.GetBlack(AChannel: Integer): Integer;
begin
  if ValidColorChannel(AChannel) then
  begin
    Result := FSettings[AChannel].Black;
  end
  else
  begin
    Result := 0;
  end;
end;

function TgmSelectiveColor.GetCyan(AChannel: Integer): Integer;
begin
  if ValidColorChannel(AChannel) then
  begin
    Result := FSettings[AChannel].Cyan;
  end
  else
  begin
    Result := 0;
  end;
end;

function TgmSelectiveColor.GetMagenta(AChannel: Integer): Integer;
begin
  if ValidColorChannel(AChannel) then
  begin
    Result := FSettings[AChannel].Magenta;
  end
  else
  begin
    Result := 0;
  end;
end;

function TgmSelectiveColor.GetYellow(AChannel: Integer): Integer;
begin
  if ValidColorChannel(AChannel) then
  begin
    Result := FSettings[AChannel].Yellow;
  end
  else
  begin
    Result := 0;
  end;
end;

procedure TgmSelectiveColor.InitSettings;
var
  i : Integer;
begin
  for i := 0 to 8 do
  begin
    with FSettings[i] do
    begin
      Cyan    := 0;
      Magenta := 0;
      Yellow  := 0;
      Black   := 0;
    end;
  end;
end;

procedure TgmSelectiveColor.Reset;
begin
  FAdjustMode   := amRelative;
  FColorChannel := sccReds;
  
  InitSettings();
end;

procedure TgmSelectiveColor.SetBlack(AChannel: Integer; AValue: Integer);
begin
  if ValidColorChannel(AChannel) then
  begin
    FSettings[AChannel].Black := Clamp(AValue, -100, 100);
  end;
end;

procedure TgmSelectiveColor.SetCyan(AChannel: Integer; AValue: Integer);
begin
  if ValidColorChannel(AChannel) then
  begin
    FSettings[AChannel].Cyan := Clamp(AValue, -100, 100);
  end;
end;

procedure TgmSelectiveColor.SetMagenta(AChannel: Integer; AValue: Integer);
begin
  if ValidColorChannel(AChannel) then
  begin
    FSettings[AChannel].Magenta := Clamp(AValue, -100, 100);
  end;
end;

procedure TgmSelectiveColor.SetYellow(AChannel: Integer; AValue: Integer);
begin
  if ValidColorChannel(AChannel) then
  begin
    FSettings[AChannel].Yellow := Clamp(AValue, -100, 100);
  end;
end;

function TgmSelectiveColor.ValidColorChannel(const AChannel: Integer): Boolean;
begin
  Result := (AChannel >= 0) and (AChannel < 9);
end;

//------------------------------------------------------------------------------

var
  LibHandle: THandle;

procedure LoadDLLFuntions;
begin
  SelectiveColor := nil;

  if FileExists('PJLSelectiveColor.dll') then
  begin
    LibHandle := LoadLibrary( PAnsiChar('PJLSelectiveColor.dll') );

    if LibHandle >= 32 then
    begin
      SelectiveColor := GetProcAddress(LibHandle, 'SelectiveColor');
    end
    else
    begin
      // raise some exception (DLL not loaded)
      MessageDlg('Cannot load the PJLSelectiveColor.dll.', mtError, [mbOK], 0);
    end;
  end
  else
  begin
    MessageDlg('The PJLSelectiveColor.dll is not found.', mtError, [mbOK], 0);
    Application.Terminate;
  end;
end; 

initialization
  LoadDLLFuntions;

finalization
  SelectiveColor := nil;
  LibHandle      := 0;


end.
