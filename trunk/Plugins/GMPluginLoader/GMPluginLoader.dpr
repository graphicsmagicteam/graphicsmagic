{ This program is for testing how to load/run the .gmp plugins which
  specifically designed for GraphicsMagic.

  CopyRight(C) 2001-2008, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

program GMPluginLoader;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
