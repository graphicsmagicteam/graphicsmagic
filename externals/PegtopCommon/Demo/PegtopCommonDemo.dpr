program PegtopCommonDemo;

uses
  Forms,
  MainForms in 'MainForms.pas' {MainForm},
  ColorForms in 'ColorForms.pas' {ColorForm},
  FileDialogForms in 'FileDialogForms.pas' {FileDialogForm},
  ColorGradientForms in 'ColorGradientForms.pas' {ColorGradientForm},
  RadioGroupForms in 'RadioGroupForms.pas' {RadioGroupForm},
  DesktopMagnifierForms in 'DesktopMagnifierForms.pas' {DesktopMagnifierForm},
  TrackBarForms in 'TrackBarForms.pas' {TrackBarForm},
  ProgressBarForms in 'ProgressBarForms.pas' {ProgressBarForm},
  LinkForms in 'LinkForms.pas' {LinkForm},
  AlarmScheduleForms in 'AlarmScheduleForms.pas' {AlarmScheduleForm},
  SystemImageForms in 'SystemImageForms.pas' {SystemImageForm},
  LabelForms in 'LabelForms.pas' {LabelForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
