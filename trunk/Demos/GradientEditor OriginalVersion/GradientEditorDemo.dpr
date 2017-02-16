program GradientEditorDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  gmGradient in 'Lib\gmGradient.pas',
  gmGeometry2D in 'Lib\gmGeometry2D.pas',
  gmGeometricObjects2D in 'Lib\gmGeometricObjects2D.pas',
  gmStopObject in 'Lib\gmStopObject.pas',
  gmGradientEditor in 'Lib\gmGradientEditor.pas',
  gmMiscFuncs in 'Lib\gmMiscFuncs.pas',
  gmGradientManager in 'Lib\gmGradientManager.pas',
  gmGradientRender in 'Lib\gmGradientRender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
