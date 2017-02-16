unit PegtopRadioButtons;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TPegtopRadioButton = class(TRadioButton)
  private
    { Private declarations }
  protected
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure TPegtopRadioButton.Paint;
begin
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Pegtop Controls', [TPegtopRadioButton]);
end;

end.
