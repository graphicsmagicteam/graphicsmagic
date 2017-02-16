program PJLSelectiveColor;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  gmSelectiveColor in 'lib\gmSelectiveColor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
