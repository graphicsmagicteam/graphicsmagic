program GimpCurvesTool;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  GimpCurvesDlg in 'GimpCurvesDlg.pas' {frmGimpCurves};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
