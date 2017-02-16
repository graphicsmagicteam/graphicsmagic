unit DesktopMagnifierForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PegtopDesktopMagnifiers, PegtopTrackBars, ExtCtrls, StdCtrls,
  PegtopCheckBoxes;

type
  TDesktopMagnifierForm = class(TForm)
    PegtopDesktopMagnifier1: TPegtopDesktopMagnifier;
    Panel1: TPanel;
    PegtopTrackBar1: TPegtopTrackBar;
    PegtopCheckBox1: TPegtopCheckBox;
    Panel2: TPanel;
    procedure PegtopTrackBar1Change(Sender: TObject);
    procedure PegtopCheckBox1Click(Sender: TObject);
    procedure PegtopDesktopMagnifier1TargetColorChange(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

var
  DesktopMagnifierForm: TDesktopMagnifierForm;

implementation

{$R *.DFM}

procedure TDesktopMagnifierForm.WMEraseBkgnd(var Msg: TMessage);
begin
  // Msg.Result := 1;
  inherited;
end;

procedure TDesktopMagnifierForm.PegtopTrackBar1Change(Sender: TObject);
begin
  PegtopDesktopMagnifier1.Zoom := TPegtopTrackBar(Sender).Position;
end;

procedure TDesktopMagnifierForm.PegtopCheckBox1Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then begin
    PegtopDesktopMagnifier1.Options := PegtopDesktopMagnifier1.Options + [pmoDrawCursor];
    PegtopDesktopMagnifier1.CrossHairs := pmcNone;
  end
  else begin
    PegtopDesktopMagnifier1.Options := PegtopDesktopMagnifier1.Options - [pmoDrawCursor];
    PegtopDesktopMagnifier1.CrossHairs := pmcOpenCross;
  end;
end;

procedure TDesktopMagnifierForm.PegtopDesktopMagnifier1TargetColorChange(Sender: TObject);
begin
  Panel2.Visible := PegtopDesktopMagnifier1.TargetColor <> clNone;
  Panel2.Color := PegtopDesktopMagnifier1.TargetColor;
end;

end.
