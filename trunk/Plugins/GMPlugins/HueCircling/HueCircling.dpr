{ This is a plug-in filter specifically designed for GraphicsMagic.
  Written by: Gerd Platl

  release 2009-01-22 }

// Updated Date: 2017/01/24

library HueCircling;

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
  Controls,
  GR32,
  HueCirclingDlg in 'HueCirclingDlg.pas' {HueCirclingForm},
  gmTypes in '..\..\..\GraphicsMagicLib\gmTypes.pas',
  gmPluginFuncs in '..\..\..\GraphicsMagicLib\gmPluginFuncs.pas';

{$E gmp}    // Extention: Graphics_Magic_Plugin

{$R *.res}

type
  TUpdateViewProc = procedure;

function GetPluginCategory: PChar; stdcall;
begin
  Result := 'Colorize';
end;

function GetPluginName: PChar; stdcall;
begin
  Result := 'HueCircling';
end;

function IsAdjustablePlugin: Boolean; stdcall;
begin
  Result := True;
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
  LOldAppHandle: THandle;
  LIsSuccessed : Boolean;
begin
  LIsSuccessed  := True;
  LOldAppHandle := Application.Handle;

  Application.Handle := AppHandle;
  HueCirclingForm := THueCirclingForm.Create(Application);
  try
    try
      HueCirclingForm.FDestBmpPtr     := DestBmpPtr;
      HueCirclingForm.FUpdateViewProc := UpdateViewProc;
      CopyBmpDataFromPtr(DestBmpPtr, Width, Height, HueCirclingForm.FSourceBmp);

      if HueCirclingForm.ShowModal = mrOK then
      begin
        if not HueCirclingForm.checkboxPreview.Checked then
        begin
          CopyBmpDataToPtr(HueCirclingForm.FProcessedBmp, DestBmpPtr, Width, Height);

          if Assigned(UpdateViewProc) then
          begin
            UpdateViewProc;
          end;
        end;
      end
      else
      begin
        CopyBmpDataToPtr(HueCirclingForm.FSourceBmp, DestBmpPtr, Width, Height);

        if Assigned(UpdateViewProc) then
        begin
          UpdateViewProc;
        end;

        LIsSuccessed := False;
      end;

    except
      LIsSuccessed := False;
    end;

  finally
    HueCirclingForm.Free;
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
