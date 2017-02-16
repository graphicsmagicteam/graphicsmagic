unit PegtopStatusBars;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TPegtopStatusBar = class(TStatusBar)
  private
//    procedure WMEraseBkgnd (Var Msg: TMessage); message WM_EraseBkgnd;
  protected
//    procedure DrawPanel (Panel: TStatusPanel; const Rect: TRect); override;
//    function GetClientRect: TRect; override;
//    function GetClientOrigin: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation

constructor TPegtopStatusBar.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

end.
