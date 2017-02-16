unit GhostForm;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Update Date: April 9, 2014
 *
 * The Initial Developer of this unit are
 *
 * x2nie - Fathony Luthfillah < x2nie@yahoo.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)
 
interface

{$I GraphicsMagic.inc} // added by Xiaoguang

{$WARN UNSAFE_CAST OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfrmGhost = class(TForm)
    timerFade: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure timerFadeTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSnaps : TAnchors;
    FDistances : TRect;
    FHouse: TCustomForm;
    F10, FMinBlend, FMaxOpaque : Integer;
    FGoFade : Boolean;
    FScaring: Boolean;
    FUntouchable: Boolean;
    function IsDockedClient(Client: TControl): Boolean;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION; //visibility
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER; //transparency trigger
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetUntouchable(const Value: Boolean); //transparency trigger
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;

  public
    { Public declarations }
    procedure Opaqueing ; //show to opaque
    procedure Blending  ; //hide
    procedure Eat(AControl : TWinControl; GhostFamily: TList);
    procedure Fly;
    property House : TCustomForm read FHouse write FHouse; //always in house even house is being moved
    property MinBlend : Integer read FMinBlend write FMinBlend;
    property MaxOpaque : integer read FMaxOpaque write FMaxOpaque;
    property Scaring : Boolean read FScaring write FScaring;// can fade?
    property Untouchable : Boolean read FUntouchable write SetUntouchable; // can't click?
  end;

var
  frmGhost: TfrmGhost;

implementation

{$R *.dfm}

{ TfrmGhost }

procedure TfrmGhost.Blending;
begin
  FGoFade := True;
  timerFade.Enabled := True;
end;

procedure TfrmGhost.Opaqueing;
begin
  FGoFade := False;
  timerFade.Enabled := True;
end;

procedure TfrmGhost.CMDockNotification(var Message: TCMDockNotification);
begin
  if IsDockedClient(Message.Client) then
    case Message.NotifyRec.ClientMsg of
      CM_VISIBLECHANGED:
        //Self.Visible := Boolean(Message.NotifyRec.MsgWParam);
        if Boolean(Message.NotifyRec.MsgWParam) then
          ShowWindow(Self.Handle, SW_SHOWNOACTIVATE)
        else
          ShowWindow(Self.Handle, SW_HIDE)
    end;
  inherited;
end;

procedure TfrmGhost.CMMouseEnter(var Message: TMessage);
begin
  if Scaring then
  Opaqueing;
end;

procedure TfrmGhost.CMMouseLeave(var Message: TMessage);
begin
  if Scaring then
  Blending;
end;

procedure TfrmGhost.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE {or WS_EX_TRANSPARENT};
end;

procedure TfrmGhost.Eat(AControl: TWinControl; GhostFamily: TList);
var P  : TPoint;
    Op : TPoint;
    F  : TForm;
    Cr : TRect;
begin
  //register as new member
  GhostFamily.Add(self);

  F  := GetParentForm(AControl) as TForm;
  Op := F.ClientOrigin;
  Cr := F.ClientRect;

  //remember the origins.
  //we finally need the distance between a control to form pos
  with AControl,FDistances do
  begin
    FSnaps      := Anchors;
    TopLeft     := ClientToScreen(Point(0,0));            //control in screen
    BottomRight := ClientToScreen(Point(Width, AControl.Top + AControl.Height)); //control in screen
    //FDistances

    //TopLeft := F.ScreenToClient(TopLeft);  //<-- = ctrl.pos within root form.client

    //OffsetRect(FDistances , Cr.Left, Cr.Top); // screen
    //Pt      := F.ScreenToClient(BottomRight);
    Right   := (F.Left + F.Width) - Right; // + F.Width - Right; //distance
    Bottom  := (F.Top + F.Height) - Bottom; //F.Height - Bottom; //distance

    Left    := Left - F.Left;
    Top     := Top - F.Top;
  end;

  //hungry? eat it!   but... anything eaten by ghost will always visible whith in.
  AControl.ManualDock(Self);
  self.ClientWidth := AControl.Width;
  self.ClientHeight := AControl.Height;
  AControl.Align := alClient;
  P := AControl.ClientToScreen(Point(0,0));
  Self.BoundsRect := Rect(P.X, P.Y, P.X + self.Width, P.Y + self.Height);
end;

procedure TfrmGhost.Fly;
var R : TRect;
    LHeight : Integer;
    P : TPoint;
begin
  LHeight := Height;
  R := Rect(0,0,Width,Height);
  if akleft   in FSnaps then    OffsetRect(R, House.Left + FDistances.Left,0);
  if akTop    in FSnaps then    OffsetRect(R, 0, House.Top + FDistances.Top);
  if akRight  in FSnaps then    OffsetRect(R, (House.Left + House.Width) - FDistances.Right - Width -1,0);
  if akBottom in FSnaps then
  begin
    P := Point(0, House.Top + House.height - FDistances.Bottom ); // screen
    //within form
    //P := House.ClientToScreen(P);                     //onscreen
    LHeight := P.Y - R.Top -1;  //HouseTop + House.height - FDistances.Bottom;
  end;
//  if akBottom in FSnaps then    R.Bottom := House.

  //we avoid using SetBounds() here, because it too many flicker + very slow!
  {if R <> Self.BoundsRect then
    with R do
      self.SetBounds(Left, Top, Width, Height);}
  with R do
  SetWindowPos(self.Handle, 0, Left, Top, Width, LHeight,
        SWP_NOZORDER + SWP_NOACTIVATE)
end;

function TfrmGhost.IsDockedClient(Client: TControl): Boolean;
begin
  Result := (Self.ControlCount > 0) and (Self.Controls[0] = Client);
end;



procedure TfrmGhost.FormCreate(Sender: TObject);
begin
  FMinBlend := Self.AlphaBlendValue;
  FMaxOpaque := 255;
  F10 := 15;
  FScaring := True; //=active! go scaring...!
end;

procedure TfrmGhost.timerFadeTimer(Sender: TObject);
begin
  if FGoFade then
  begin
    //goto blend, mouse out
    if AlphaBlendValue - F10<= FMinBlend then
    begin
      AlphaBlendValue := FMinBlend;
      {$IFNDEF COMPILER2006_UP}   // compiler directive added by Xiaoguang
      timerFade.Enabled := False;
      {$ENDIF}
    end
    else
    begin
      AlphaBlendValue := AlphaBlendValue - F10;
    end;
  end
  else
  begin
    //goto opaque, mouse in
    if AlphaBlendValue + F10>= FMaxOpaque then
    begin
      AlphaBlendValue := FMaxOpaque;
      {$IFNDEF COMPILER2006_UP}   // compiler directive added by Xiaoguang
      timerFade.Enabled := False;
      {$ENDIF}
    end
    else
    begin
      AlphaBlendValue := AlphaBlendValue + F10;
    end;
  end;
end;

procedure TfrmGhost.WndProc(var Message: TMessage);
var
  P: TPoint;
begin
  inherited WndProc(Message);

  with Message do
    case Msg of
      //WM_ACTIVATE, WM_SETFOCUS, WM_KILLFOCUS:
      WM_MOUSEMOVE:
        begin
          P := SmallPointToPoint(TWMMouse(Message).Pos);  //within form
          //ClientToScreen(DragCapture, P);
          //DragTo(P);
          if P.X > self.Width - self.VertScrollBar.Size then //in vert sb
            Visible := False;
        end;
      else begin end;
    end;


end;

procedure TfrmGhost.SetUntouchable(const Value: Boolean);
var
  L : Longint;
begin
  FUntouchable := Value;
  L := GetWindowLong(Self.Handle, GWL_EXSTYLE);
  if Value then
  begin
    //do untouched
    if L and WS_EX_TRANSPARENT = 0 then //currently can touched?
      SetWindowLong(Self.Handle, GWL_EXSTYLE, L or WS_EX_TRANSPARENT);
  end
  else
  begin
    //do can be touched
    if L and WS_EX_TRANSPARENT <> 0 then //currently untouched?
      SetWindowLong(Self.Handle, GWL_EXSTYLE, L and not WS_EX_TRANSPARENT);
  end;
end;

procedure TfrmGhost.FormShow(Sender: TObject);
begin
  // TODO: add this line for higher versions of Delphi
  // but it may not be a good idea...
  // -- Xiaoguang
  timerFade.Enabled := True;
end;

end.
