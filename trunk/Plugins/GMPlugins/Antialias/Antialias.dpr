{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang, Ma Xiaoming < gmbros@hotmail.com >
  and GraphicsMagic Team, all rights reserved. }

// Updated Date: 2017/01/24 

library Antialias;

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
  AntialiasDlg in 'AntialiasDlg.pas' {frmAntialias},
  gmTypes in '..\..\..\GraphicsMagicLib\gmTypes.pas',
  gmPluginFuncs in '..\..\..\GraphicsMagicLib\gmPluginFuncs.pas';

{$E gmp}

{$R *.res}

{$WARN UNSAFE_TYPE OFF}

type
  TUpdateViewProc = procedure;

var
  SelectedChannelSet: TgmChannelSet;

function GetPluginCategory: PChar; stdcall;
begin
  Result := 'Blur';
end;

function GetPluginName: PChar; stdcall;
begin
  Result := 'Antialias';
end;

function IsAdjustablePlugin: Boolean; stdcall;
begin
  Result := True;
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
  LOldAppHandle: THandle;
  LIsSuccessed : Boolean;
begin
  LIsSuccessed  := True;
  LOldAppHandle := Application.Handle;

  Application.Handle := AppHandle;
  frmAntialias       := TfrmAntialias.Create(Application);
  try
    try
      frmAntialias.FDestBmpPtr     := DestBmpPtr;
      frmAntialias.FUpdateViewProc := UpdateViewProc;
      frmAntialias.FChannelSet     := SelectedChannelSet;

      CopyBmpDataFromPtr(DestBmpPtr, Width, Height, frmAntialias.FSourceBmp);

      if frmAntialias.ShowModal = mrOK then
      begin
        if not frmAntialias.chckbxPreview.Checked then
        begin
          CopyBmpDataToPtr(frmAntialias.FProcessedBmp, DestBmpPtr, Width, Height);

          if Assigned(UpdateViewProc) then
          begin
            UpdateViewProc;
          end;
        end;
      end
      else
      begin
        CopyBmpDataToPtr(frmAntialias.FSourceBmp, DestBmpPtr, Width, Height);

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
    frmAntialias.Free;
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
