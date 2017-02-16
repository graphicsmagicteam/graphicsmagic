{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

library Sharpen;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{ This include file is suggested by Andre Felix Miertschink to
  make this program could be compiled by Delphi XE. }

{$I ..\..\..\Source\GraphicsMagic.inc}

uses
  SysUtils,
  Classes,
  Forms,
  GR32,
  gmImageProcessFuncs,
  gmTypes in '..\..\..\GraphicsMagicLib\gmTypes.pas',
  gmPluginFuncs in '..\..\..\GraphicsMagicLib\gmPluginFuncs.pas',
  gmSharpen in '..\..\..\GraphicsMagicLib\gmSharpen.pas';

{$E gmp}

{$R *.res}

{$WARN UNSAFE_TYPE OFF}

type
  TUpdateViewProc = procedure;

var
  SelectedChannelSet: TgmChannelSet;

function GetPluginCategory: PChar; stdcall;
begin
  Result := 'Sharpen';
end;

function GetPluginName: PChar; stdcall;
begin
  Result := 'Sharpen';
end;

function IsAdjustablePlugin: Boolean; stdcall;
begin
  Result := False;
end;

// indicates whether this plugin supports single channel operations
function IsChannelSupported: Boolean; stdcall;
begin
  Result := True;
end;

// set selected channels to process
function SetChannels(const AChannelSet: TgmChannelSet): Boolean; stdcall;
begin
  SelectedChannelSet := AChannelSet;
  Result             := True;
end;

function ExecutePlugin(AppHandle: THandle; DestBmpPtr: PColor32;
  const Width, Height: Integer; UpdateViewProc: TUpdateViewProc;
  const BKColor: TColor32 = $00000000): Boolean; stdcall;
var
  LOldAppHandle     : THandle;
  LIsSuccessed      : Boolean;
  LProcessedBmp     : TBitmap32;
  LTempBmp          : TBitmap32;
  LColorChannelCount: Integer;
begin
  LIsSuccessed  := True;
  LOldAppHandle := Application.Handle;

  Application.Handle := AppHandle;
  LProcessedBmp      := TBitmap32.Create;
  try
    try
      CopyBmpDataFromPtr(DestBmpPtr, Width, Height, LProcessedBmp);

      SharpenBitmap(LProcessedBmp, smSharpen);

      if csGrayscale in SelectedChannelSet then
      begin
        Desaturate32(LProcessedBmp);
      end
      else
      begin
        LColorChannelCount := GetColorChannelCount(SelectedChannelSet);

        if (LColorChannelCount > 0) and (LColorChannelCount < 3) then
        begin
          LTempBmp := TBitmap32.Create;
          try
            CopyBmpDataFromPtr(DestBmpPtr, Width, Height, LTempBmp);
            ReplaceRGBChannels(LTempBmp, LProcessedBmp, SelectedChannelSet, crsRemainDest);
          finally
            LTempBmp.Free;
          end;
        end;
      end;

      CopyBmpDataToPtr(LProcessedBmp, DestBmpPtr, Width, Height);

      if Assigned(UpdateViewProc) then
      begin
        UpdateViewProc;
      end;
    except
      LIsSuccessed := False;
    end;

  finally
    LProcessedBmp.Free;
    Application.Handle := LOldAppHandle;
  end;
  
  Result := LIsSuccessed;
end; 

exports
  ExecutePlugin,
  GetPluginCategory,
  GetPluginName,
  IsAdjustablePlugin,
  IsChannelSupported,
  SetChannels;

begin
  // initial settings
  SelectedChannelSet := [csRed, csGreen, csBlue];
end.
