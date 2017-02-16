unit ProgressBarForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PegtopProgressBars, StdCtrls, ComCtrls, PegtopNumEdits, ExtCtrls;

type
  TProgressBarForm = class(TForm)
    Label1: TLabel;
    PegtopIntEdit1: TPegtopIntEdit;
    PegtopProgressBar1: TPegtopProgressBar;
    PegtopProgressBar2: TPegtopProgressBar;
    PegtopProgressBar3: TPegtopProgressBar;
    PegtopProgressBar4: TPegtopProgressBar;
    PegtopProgressBar5: TPegtopProgressBar;
    PegtopProgressBar6: TPegtopProgressBar;
    PegtopProgressBar7: TPegtopProgressBar;
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;
    PegtopProgressBar8: TPegtopProgressBar;
    PegtopProgressBar9: TPegtopProgressBar;
    procedure PegtopProgressBar4Color(Sender: TObject; Position: Integer;
      var Color: TColor);
    procedure PegtopProgressBar5Color(Sender: TObject; Position: Integer;
      var Color: TColor);
    procedure PegtopProgressBar6Color(Sender: TObject; Position: Integer;
      var Color: TColor);
    procedure PegtopProgressBar7Color(Sender: TObject; Position: Integer;
      var Color: TColor);
    procedure PegtopProgressBar8Color(Sender: TObject; Position: Integer;
      var Color: TColor);
    procedure PegtopIntEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProgressBarForm: TProgressBarForm;

implementation

uses
  PegtopThemes;

{$R *.DFM}

procedure TProgressBarForm.PegtopProgressBar4Color(Sender: TObject;
  Position: Integer; var Color: TColor);
begin
  // recolor right half:
  if Position > 50 then Color := clRed;
end;

procedure TProgressBarForm.PegtopProgressBar5Color(Sender: TObject;
  Position: Integer; var Color: TColor);
begin
  // gradient all over the progress bar (from 0% to 100%):
  Color := PegtopProgressBar2.GetGradientColor(clYellow, clRed, Position);
end;

procedure TProgressBarForm.PegtopProgressBar6Color(Sender: TObject;
  Position: Integer; var Color: TColor);
begin
  // gradient between 40% and 60%:
  Color := PegtopProgressBar2.GetGradientColor(clYellow, clRed, 40, 60, Position);
end;

procedure TProgressBarForm.PegtopProgressBar7Color(Sender: TObject;
  Position: Integer; var Color: TColor);
begin
  // two separate gradients for left / right half:
  if Position < 50 then
    Color := PegtopProgressBar2.GetGradientColor(clGreen, clYellow, 0, 50, Position)
  else
    Color := PegtopProgressBar2.GetGradientColor(clYellow, clRed, 50, 100, Position);
end;

procedure TProgressBarForm.PegtopProgressBar8Color(Sender: TObject;
  Position: Integer; var Color: TColor);
begin
  // single color:
  Color := $A0A0FF;
end;

procedure TProgressBarForm.PegtopIntEdit1Change(Sender: TObject);
begin
  // update progress bar positions:
  PegtopProgressBar1.Position := PegtopIntEdit1.Value;
  PegtopProgressBar1.Caption := IntToStr(PegtopIntEdit1.Value) + ' %';
  PegtopProgressBar2.Position := PegtopIntEdit1.Value;
  PegtopProgressBar3.Position := PegtopIntEdit1.Value;
  PegtopProgressBar4.Position := PegtopIntEdit1.Value;
  PegtopProgressBar5.Position := PegtopIntEdit1.Value;
  PegtopProgressBar6.Position := PegtopIntEdit1.Value;
  PegtopProgressBar7.Position := PegtopIntEdit1.Value;
  PegtopProgressBar7.Caption := IntToStr(PegtopIntEdit1.Value) + ' %';
  PegtopProgressBar8.Position := PegtopIntEdit1.Value;
  PegtopProgressBar8.Caption := IntToStr(PegtopIntEdit1.Value) + ' %';
  PegtopProgressBar9.Position := PegtopIntEdit1.Value;
end;

procedure TProgressBarForm.Button1Click(Sender: TObject);
begin
  // start / stop timer:
  Timer1.Enabled := not Timer1.Enabled;
  PegtopIntEdit1.Enabled := not Timer1.Enabled;
end;

procedure TProgressBarForm.Timer1Timer(Sender: TObject);
begin
  // inc. positions:
  PegtopIntEdit1.Value := (PegtopIntEdit1.Value + 1) mod 101;
  PegtopIntEdit1Change(Sender);
end;

procedure TProgressBarForm.Button2Click(Sender: TObject);
begin
  // enable / disable XP themes (if available):
  DefaultTheme.Enabled := not DefaultTheme.Enabled;
  Invalidate;
end;

procedure TProgressBarForm.FormCreate(Sender: TObject);
begin
  // XP themes available?
  Button2.Enabled := DefaultTheme.Supported;
end;

end.
