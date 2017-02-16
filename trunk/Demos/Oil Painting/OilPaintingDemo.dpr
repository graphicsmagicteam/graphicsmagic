program OilPaintingDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  gmOilPaintingFilter in '..\..\GraphicsMagicLib\gmOilPaintingFilter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
