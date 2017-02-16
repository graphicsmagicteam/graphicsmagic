{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

// Updated Date: 2017/01/24

library Resynthesizer;

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
  ResynthDlg in 'ResynthDlg.pas' {frmResynthesizer},
  AboutResynthDlg in 'AboutResynthDlg.pas' {frmAboutResynthesizer},
  gmTypes in '..\..\..\GraphicsMagicLib\gmTypes.pas',
  gmPluginFuncs in '..\..\..\GraphicsMagicLib\gmPluginFuncs.pas';

{$E gmp}  // Filter plugin extension name for GraphicsMagic Pro

{$R *.res}

{$WARN UNSAFE_TYPE OFF}

type
  TUpdateViewProc = procedure;

var
  SelectedChannelSet: TgmChannelSet;

function GetPluginCategory: PChar; stdcall;
begin
  Result := 'Texture';
end;

function GetPluginName: PChar; stdcall;
begin
  Result := 'Resynthesizer';
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
  LIsSuccessful: Boolean;
begin
  LIsSuccessful := True;

  LOldAppHandle      := Application.Handle;
  Application.Handle := AppHandle;
  try
    frmResynthesizer := TfrmResynthesizer.Create(Application);
    try
      try
        frmResynthesizer.FDestBmpPtr     := DestBmpPtr;
        frmResynthesizer.FUpdateViewProc := UpdateViewProc;
        frmResynthesizer.FChannelSet     := SelectedChannelSet;

        CopyBmpDataFromPtr(DestBmpPtr, Width, Height, frmResynthesizer.FSourceBmp);
        CopyBmpDataFromPtr(DestBmpPtr, Width, Height, frmResynthesizer.imgvwSrcTexture.Bitmap);

        if frmResynthesizer.ShowModal = mrOK then
        begin
          if frmResynthesizer.FIsNewSettingExecuted = False then
          begin
            frmResynthesizer.ExecuteResynthesizer;
          end;
        end
        else
        begin
          CopyBmpDataToPtr(frmResynthesizer.FSourceBmp, DestBmpPtr, Width, Height);

          if Assigned(UpdateViewProc) then
          begin
            UpdateViewProc;
          end;

          LIsSuccessful := False;
        end;

      except
        LIsSuccessful := False;
      end;

    finally
      frmResynthesizer.Free;
    end;
  finally
    Application.Handle := LOldAppHandle;
  end;

  Result := LIsSuccessful;
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
