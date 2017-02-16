unit LinkForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PegtopLinks, StdCtrls;

type
  TLinkForm = class(TForm)
    PegtopLink1: TPegtopLink;
    PegtopWebLink1: TPegtopWebLink;
    procedure PegtopLink1Visit(Sender: TObject);
    procedure PegtopLink1StateChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LinkForm: TLinkForm;

implementation

{$R *.DFM}

procedure TLinkForm.PegtopLink1Visit(Sender: TObject);
begin
  ShowMessage('Very nice. Visited links can change their appearence (color and style).');
end;

procedure TLinkForm.PegtopLink1StateChange(Sender: TObject);
var
  Link: TPegtopLink;
begin
  Link := TPegtopLink(Sender);
  if Link.State in [plsHover, plsActive] then
    Link.LinkFont.Style := Link.LinkFont.Style + [fsUnderline]
  else
    Link.LinkFont.Style := Link.LinkFont.Style - [fsUnderline];
  if Link.State in [plsActive] then
    Link.LinkFont.Color := clRed
  else if Link.Visited then
    Link.LinkFont.Color := clPurple
  else
    Link.LinkFont.Color := clBlue;
end;

end.
