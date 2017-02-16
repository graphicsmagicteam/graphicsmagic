unit MainForms;

{$UNDEF XPThemes}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormClass = class of TForm;

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    procedure DemoButtonClick(Sender: TObject);
  private
    procedure ShowDemoForm(FormClass: TFormClass);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IFDEF XPThemes}
  ThemeMgr,
{$ENDIF}
  ColorForms, FileDialogForms, ColorGradientForms, RadioGroupForms,
  DesktopMagnifierForms, TrackBarForms, ProgressBarForms, LinkForms,
  AlarmScheduleForms, SystemImageForms, LabelForms;

{$R *.DFM}
{$IFDEF XPThemes}
  {$R winxp.res}
{$ENDIF}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF XPThemes}
  with TThemeManager.Create(Self) do
    Options := Options - [toSubClassPanel];
{$ENDIF}
end;

procedure TMainForm.ShowDemoForm(FormClass: TFormClass);
var
  Form: TForm;
begin
  Form := FormClass.Create(Self);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TMainForm.DemoButtonClick(Sender: TObject);
const
  DemoFormClasses: array[0..10] of TFormClass = (
    TColorForm, TFileDialogForm, TColorGradientForm, TRadioGroupForm,
    TDesktopMagnifierForm, TTrackBarForm, TProgressBarForm, TLinkForm,
    TAlarmScheduleForm, TSystemImageForm, TLabelForm
  );
begin
  ShowDemoForm(DemoFormClasses[TComponent(Sender).Tag]);
end;

end.
