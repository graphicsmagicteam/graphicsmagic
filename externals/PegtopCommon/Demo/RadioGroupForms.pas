unit RadioGroupForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PegtopRadioGroups;

type
  TRadioGroupForm = class(TForm)
    PegtopRadioGroup1: TPegtopRadioGroup;
    PegtopRadioGroup2: TPegtopRadioGroup;
    PegtopRadioGroup3: TPegtopRadioGroup;
    PegtopRadioGroup4: TPegtopRadioGroup;
    PegtopRadioGroup5: TPegtopRadioGroup;
    PegtopRadioGroup6: TPegtopRadioGroup;
    PegtopRadioGroup7: TPegtopRadioGroup;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RadioGroupForm: TRadioGroupForm;

implementation

{$R *.DFM}

procedure TRadioGroupForm.FormCreate(Sender: TObject);
begin
  PegtopRadioGroup6.ButtonFont[0].Style := [fsBold];
  PegtopRadioGroup6.ButtonEnabled[1] := False;
  PegtopRadioGroup6.ButtonEnabled[4] := False;
end;

end.
