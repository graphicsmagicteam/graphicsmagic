unit HistoryForm;

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
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin,
{ GraphicsMagic Lib }
  gmCommandViewer;

type
  TfrmHistory = class(TForm)
    tlbrHistoryTools: TToolBar;
    ToolButton1: TToolButton;
    tlbtnDeleteCurrentState: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FCommandViewer : TgmCommandViewer;

    procedure CommandViewerBeforeMouseDown(ASender: TObject);
  public
    property CommandViewer : TgmCommandViewer read FCommandViewer;
  end;

var
  frmHistory: TfrmHistory;

implementation

uses
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.dfm}

procedure TfrmHistory.CommandViewerBeforeMouseDown(ASender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    if Assigned(FCommandViewer) then
    begin
      FCommandViewer.IsEnabled := not ActiveChildForm.ToolsInPendingStatus();
    end;
  end;
end;

procedure TfrmHistory.FormActivate(Sender: TObject);
begin
  if Assigned(FCommandViewer) then
  begin
    FCommandViewer.SetFocus();
  end;
end;

procedure TfrmHistory.FormCreate(Sender: TObject);
begin
  FCommandViewer := TgmCommandViewer.Create(Self);
  with FCommandViewer do
  begin
    Parent          := Self;
    Align           := alClient;
    BeforeMouseDown := Self.CommandViewerBeforeMouseDown;
  end;

  ManualDock(frmMain.pgcntrlDockSite1);
  Show();
end;

procedure TfrmHistory.FormDestroy(Sender: TObject);
begin
  FCommandViewer.Free();
end;

end.
