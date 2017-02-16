////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopCheckBox
// Version:    1.00
// Date:       13 Jun 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopCheckBox is a simple extension of TCheckBox. You can change its state
// (by code) without generating an OnClick event.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////

unit PegtopCheckBoxes;

interface

uses
  Windows, Classes, Messages, StdCtrls;

type
  TPegtopCheckBox = class(TCheckBox)
  public
    procedure Change(Value: Boolean; FireOnClick: Boolean = False); overload;
    procedure Change(Value: TCheckBoxState; FireOnClick: Boolean = False); overload;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pegtop Controls', [TPegtopCheckBox]);
end;

procedure TPegtopCheckBox.Change(Value: Boolean; FireOnClick: Boolean = False);
var
  TempOnClick: TNotifyEvent;
begin
  if FireOnClick then begin
    Checked := Value;
  end
  else begin
    TempOnClick := OnClick;
    OnClick := NIL;
    Checked := Value;
    OnClick := TempOnClick;
  end;
end;

procedure TPegtopCheckBox.Change(Value: TCheckBoxState; FireOnClick: Boolean = False);
var
  TempOnClick: TNotifyEvent;
begin
  if FireOnClick then begin
    State := Value;
  end
  else begin
    TempOnClick := OnClick;
    OnClick := NIL;
    State := Value;
    OnClick := TempOnClick;
  end;
end;

end.
