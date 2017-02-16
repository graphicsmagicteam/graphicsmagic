{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2010 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

library CrossProcessing;

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
  gmTypes in '..\..\..\GraphicsMagicLib\gmTypes.pas',
  gmPluginFuncs in '..\..\..\GraphicsMagicLib\gmPluginFuncs.pas',
  gmReversalFilm in '..\..\..\GraphicsMagicLib\gmReversalFilm.pas';

{$E gmp}

{$R *.res}

type
  TUpdateViewProc = procedure;

function GetPluginCategory: PChar; stdcall;
begin
  Result := 'Artistic';
end;

function GetPluginName: PChar; stdcall;
begin
  Result := 'Cross Processing';
end;

function IsAdjustablePlugin: Boolean; stdcall;
begin
  Result := False;
end;

// indicates whether this plugin supports single channel operations
function IsChannelSupported: Boolean; stdcall;
begin
  Result := False;
end;

function ExecutePlugin(AppHandle: THandle; DestBmpPtr: PColor32;
  const Width, Height: Integer; UpdateViewProc: TUpdateViewProc;
  const BKColor: TColor32 = $00000000): Boolean; stdcall;
var
  LOldAppHandle    : THandle;
  LIsSuccessed     : Boolean;
  LSrcBmp, LDstBmp : TBitmap32;
  LRFNDFilter      : TgmRFNDFilter;
begin
  LIsSuccessed  := True;
  LOldAppHandle := Application.Handle;

  Application.Handle := AppHandle;
  LSrcBmp            := TBitmap32.Create;
  LDstBmp            := TBitmap32.Create;
  LRFNDFilter        := TgmRFNDFilter.Create;
  try
    try
      CopyBmpDataFromPtr(DestBmpPtr, Width, Height, LDstBmp);
      LSrcBmp.Assign(LDstBmp);

      LRFNDFilter.ExecuteFilter(LSrcBmp, LDstBmp);

      CopyBmpDataToPtr(LDstBmp, DestBmpPtr, Width, Height);

      if Assigned(UpdateViewProc) then
      begin
        UpdateViewProc;
      end;
    except
      LIsSuccessed := False;
    end;

  finally
    LSrcBmp.Free;
    LDstBmp.Free;
    LRFNDFilter.Free;
    
    Application.Handle := LOldAppHandle;
  end;
  
  Result := LIsSuccessed;
end; 

exports
  ExecutePlugin,
  GetPluginCategory,
  GetPluginName,
  IsAdjustablePlugin,
  IsChannelSupported;

begin
end.
