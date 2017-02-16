unit ChannelForm;

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
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
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

// Update Date: 2017/01/12

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls,
{ GraphicsMagic lib }
  gmChannelViewer;

type
  TfrmChannels = class(TForm)
    tlbrChannelTools: TToolBar;
    tlbtnSeparator1: TToolButton;
    tlbtnNewAlphaChannel: TToolButton;
    tlbtnDeleteAlphaChannel: TToolButton;
    tlbtnLoadChannelAsSelection: TToolButton;
    tlbtnSaveSelectionAsChannel: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FChannelViewer : TgmChannelViewer;

    procedure ChannelViewerBeforeMouseDown(ASender: TObject);
    procedure OnChannelViewerRightClick(ASender: TObject);
  public
    property ChannelViewer : TgmChannelViewer read FChannelViewer;
  end;

var
  frmChannels: TfrmChannels;

implementation

uses
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms }
  MainForm;

{$R *.dfm}

procedure TfrmChannels.ChannelViewerBeforeMouseDown(ASender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    if Assigned(FChannelViewer) then
    begin
      FChannelViewer.IsEnabled := not ActiveChildForm.ToolsInPendingStatus();
    end;
  end;
end;

procedure TfrmChannels.FormActivate(Sender: TObject);
begin
  if Assigned(FChannelViewer) then
  begin
    FChannelViewer.SetFocus();
  end;
end;

procedure TfrmChannels.FormCreate(Sender: TObject);
begin
  FChannelViewer := TgmChannelViewer.Create(Self);
  
  with FChannelViewer do
  begin
    Parent                  := Self;
    Align                   := alClient;
    BeforeMouseDown         := Self.ChannelViewerBeforeMouseDown;
    OnMouseRightButtonClick := Self.OnChannelViewerRightClick;
  end;

  ManualDock(frmMain.pgcntrlDockSite3);
  Show();
end;

procedure TfrmChannels.FormDestroy(Sender: TObject);
begin
  FChannelViewer.Free();
end;

procedure TfrmChannels.OnChannelViewerRightClick(ASender: TObject);
var
  LPoint : TPoint;
begin
  GetCursorPos(LPoint);

  dmMain.pmnChannelOptions.Popup(LPoint.X, LPoint.Y);
end;

end.
