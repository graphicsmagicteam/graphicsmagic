program GimpLevelsTool;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  GimpLevelsToolDlg in 'GimpLevelsToolDlg.pas' {frmGimpLevelsTool};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGimpLevelsTool, frmGimpLevelsTool);
  Application.Run;
end.
