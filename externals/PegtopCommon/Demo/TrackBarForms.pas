unit TrackBarForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PegtopTrackBars, StdCtrls;

type
  TTrackBarForm = class(TForm)
    PegtopTrackBar1: TPegtopTrackBar;
    PegtopTrackBar2: TPegtopTrackBar;
    PegtopRangeBar1: TPegtopRangeBar;
    PegtopColorTrackBar1: TPegtopColorTrackBar;
    Label1: TLabel;
    PegtopTrackBar3: TPegtopTrackBar;
    procedure PegtopColorTrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TrackBarForm: TTrackBarForm;

implementation

{$R *.DFM}

procedure TTrackBarForm.PegtopColorTrackBar1Change(Sender: TObject);
begin
  TPegtopColorTrackBar(Sender).ButtonColor := TPegtopColorTrackBar(Sender).Position * $010101;
end;

end.
