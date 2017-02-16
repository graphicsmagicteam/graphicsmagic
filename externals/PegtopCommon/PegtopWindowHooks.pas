////////////////////////////////////////////////////////////////////////////////
// File:       PegtopWindowHooks.pas
// Classes:    TPegtopCustomWindowHook, TPegtopWindowHook,
//             TPegtopCustomFormHook, TPegtopFormHook
// Version:    1.00
// Date:       01 May 2004
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopWindowHook can be used to hook into any windowed control's messages.
// TPegtopFormHook does the same, but automatically hooks the form it is placed
// onto.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopWindowHooks;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms;

type
  TPegtopMessageEvent = procedure(Sender: TObject; var Msg: TMessage; var Handled: Boolean) of object;

  TPegtopCustomWindowHook = class(TComponent)
  private
    FActive: Boolean;
    FControl: TWinControl;
    FOldWindowProc: TWndMethod;
    procedure WndProc(var Msg: TMessage);
    procedure SetControl(Value: TWinControl);
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); virtual; abstract;
    property Active: Boolean read FActive write FActive;
    property Control: TWinControl read FControl write SetControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPegtopWindowHook = class(TPegtopCustomWindowHook)
  private
    FOnMessage: TPegtopMessageEvent;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Control;
    property Active;
    property OnMessage: TPegtopMessageEvent read FOnMessage write FOnMessage;
  end;

  TPegtopCustomFormHook = class(TPegtopCustomWindowHook)
  private
    function GetForm: TCustomForm;
  protected
    function CountInstances(AOwner: TComponent): Integer;
    property Form: TCustomForm read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPegtopFormHook = class(TPegtopCustomFormHook)
  private
    FOnMessage: TPegtopMessageEvent;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active;
    property OnMessage: TPegtopMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
  PegtopMethodHashes;

var
  HookHash: TPegtopMethodHash;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomWindowHook
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomWindowHook.Create(AOwner: TComponent);
begin
  FActive := True;
  FControl := NIL;
  FOldWindowProc := NIL;
  inherited Create(AOwner);
end;

destructor TPegtopCustomWindowHook.Destroy;
begin
  Control := NIL; // unhook control
  inherited;
end;

procedure TPegtopCustomWindowHook.SetControl(Value: TWinControl);
var
  Hook: TPegtopCustomWindowHook;
  WndProcMethod: TWndMethod;
begin
  if FControl <> Value then begin
    if csDesigning in ComponentState then begin
      FControl := Value;
    end
    else begin
      if FControl <> NIL then begin
        WndProcMethod := WndProc;
        Hook := HookHash[TMethod(WndProcMethod)];
        if Hook <> NIL then begin // someone else hooks us
          Hook.FOldWindowProc := FOldWindowProc; // close gap
          HookHash[TMethod(FOldWindowProc)] := Hook;
          HookHash.Remove(TMethod(WndProcMethod));
        end
        else begin // we are the hook on top of it all
          FControl.WindowProc := FOldWindowProc;
        end;
      end;
      FControl := Value;
      if FControl <> NIL then begin
        FOldWindowProc := FControl.WindowProc;
        FControl.WindowProc := WndProc;
        HookHash[TMethod(FOldWindowProc)] := Self;
      end;
    end;
  end;
end;

procedure TPegtopCustomWindowHook.WndProc(var Msg: TMessage);
var
  Handled: Boolean;
begin
  if FActive then begin
    Handled := False;
    HandleMessage(Msg, Handled);
    if not Handled then FOldWindowProc(Msg);
  end
  else begin
    FOldWindowProc(Msg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopWindowHook
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopWindowHook.Create(AOwner: TComponent);
begin
  FOnMessage := NIL;
  inherited;
end;

procedure TPegtopWindowHook.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FOnMessage) then FOnMessage(Self, Msg, Handled);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomFormHook
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomFormHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner as TCustomForm); // raise exception if AOwner is not TCustomForm
  Control := TWinControl(AOwner);
end;

function TPegtopCustomFormHook.CountInstances(AOwner: TComponent): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AOwner.ComponentCount-1 do begin
    if AOwner.Components[I].ClassType = ClassType then Inc(Result);
  end;
end;

function TPegtopCustomFormHook.GetForm: TCustomForm;
begin
  Result := TCustomForm(Control);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopFormHook
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopFormHook.Create(AOwner: TComponent);
begin
  FOnMessage := NIL;
  inherited;
end;

procedure TPegtopFormHook.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FOnMessage) then FOnMessage(Self, Msg, Handled);
end;

initialization
  HookHash := TPegtopMethodHash.Create(10);
finalization
  HookHash.Free;
end.
