unit ColorForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PegtopColorDialogs, PegtopColorControls, PegtopNumEdits;

type
  TColorForm = class(TForm)
    Button1: TButton;
    PegtopColorDialog1: TPegtopColorDialog;
    Button2: TButton;
    PegtopColorDialog2: TPegtopColorDialog;
    PegtopColorBox1: TPegtopColorBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColorForm: TColorForm;

implementation

{$R *.DFM}

procedure TColorForm.Button1Click(Sender: TObject);
begin
  PegtopColorDialog1.Execute;
end;

procedure TColorForm.Button2Click(Sender: TObject);
begin
  if PegtopColorDialog2.Execute then Color := PegtopColorDialog2.Color;
end;

end.
