////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopRadioGroup
// Version:    1.00
// Date:       13 Jun 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopRadioGroup is a simple extension of TRadioGroup. Items can be aligned
// in different ways. Individual items can be formatted or disabled at runtime.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopRadioGroups;

interface

uses
  Windows, Classes, Graphics, StdCtrls, ExtCtrls;

type
  TPegtopRadioButtonAlignment = (praTop, praBottom, praCenter, praBlock);

  TPegtopRadioGroup = class(TRadioGroup)
  private
    FButtonAlignment: TPegtopRadioButtonAlignment;
    FButtonHeight: Integer;
    FClientRect: TRect;
    FClientKnown: Boolean;
    procedure SetButtonAlignment(Value: TPegtopRadioButtonAlignment);
    procedure SetButtonHeight(Value: Integer);
    function GetButtonEnabled(Index: Integer): Boolean;
    procedure SetButtonEnabled(Index: Integer; V: Boolean);
    function GetButtonFont(Index: Integer): TFont;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RealignButtons; virtual;
    property ButtonEnabled[Index: Integer]: Boolean read GetButtonEnabled write SetButtonEnabled;
    property ButtonFont[Index: Integer]: TFont read GetButtonFont;
  published
    property ButtonAlignment: TPegtopRadioButtonAlignment read FButtonAlignment write SetButtonAlignment;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
  end;

implementation

constructor TPegtopRadioGroup.Create (AOwner: TComponent);
begin
  FButtonAlignment := praTop;
  FButtonHeight := 20;
  FClientKnown := False;
  inherited Create (AOwner);
end;

procedure TPegtopRadioGroup.RealignButtons;
var
  I, Y, H: Integer;
  ItemsPerColumn: Integer;
begin
  if FClientKnown and (ControlCount > 0) then begin
    Y := FClientRect.Top;
    ItemsPerColumn := (ControlCount + Columns - 1) DIV Columns; // round up
    if FButtonAlignment = praTop then begin
      for I := 0 to ControlCount - 1 do begin
        if (I mod ItemsPerColumn) = 0 then
          Y := FClientRect.Top;
        Controls[I].Height := FButtonHeight;
        Controls[I].Top := Y;
        Inc (Y,FButtonHeight);
      end;
    end
    else if FButtonAlignment = praBottom then begin
      for I := 0 to ControlCount - 1 do begin
        if (I mod ItemsPerColumn) = 0 then
          Y := FClientRect.Bottom - ItemsPerColumn*FButtonHeight - 4;
        Controls[I].Height := FButtonHeight;
        Controls[I].Top := Y;
        Inc (Y,FButtonHeight);
      end;
    end
    else if FButtonAlignment = praCenter then begin
      for I := 0 to ControlCount - 1 do begin
        if (I mod ItemsPerColumn) = 0 then
          Y := ((FClientRect.Bottom-FClientRect.Top) - ItemsPerColumn*FButtonHeight - 4) div 2 + FClientRect.Top;
        Controls[I].Height := FButtonHeight;
        Controls[I].Top := Y;
        Inc (Y,FButtonHeight);
      end;
    end
    else begin
      H := 0;
      for I := 0 to ControlCount - 1 do begin
        if (I mod ItemsPerColumn) = 0 then H := 0;
        Y := ((H*2+1)*(FClientRect.Bottom-FClientRect.Top-4) div (ItemsPerColumn*2));
        Y := FClientRect.Top + Y - FButtonHeight div 2;
        Controls[I].Height := FButtonHeight;
        Controls[I].Top := Y;
        Inc(H);
      end;
    end;
  end;
end;

procedure TPegtopRadioGroup.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  FClientRect := Rect;
  FClientKnown := True;
  RealignButtons;
end;

procedure TPegtopRadioGroup.Resize;
begin
  inherited;
  RealignButtons;
end;

procedure TPegtopRadioGroup.SetButtonAlignment(Value: TPegtopRadioButtonAlignment);
begin
  if Value <> FButtonAlignment then begin
    FButtonAlignment := Value;
    RealignButtons;
  end;
end;

procedure TPegtopRadioGroup.SetButtonHeight(Value: Integer);
begin
  if Value <> FButtonHeight then begin
    FButtonHeight := Value;
    RealignButtons;
  end;
end;

function TPegtopRadioGroup.GetButtonEnabled(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < ControlCount) then Result := Controls[Index].Enabled else Result := False;
end;

procedure TPegtopRadioGroup.SetButtonEnabled(Index: Integer; V: Boolean);
begin
  if (Index >= 0) and (Index < ControlCount) then Controls[Index].Enabled := V;
end;

function TPegtopRadioGroup.GetButtonFont(Index: Integer): TFont;
begin
  if (Index >= 0) and (Index < ControlCount) then Result := TRadioButton(Controls[Index]).Font
  else Result := NIL;
end;

end.
