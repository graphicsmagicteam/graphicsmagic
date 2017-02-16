unit PathForm;

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
  Dialogs, ToolWin, ComCtrls,
{ GraphicsMagic lib }
  gmPathManager;

type
  TfrmPaths = class(TForm)
    tlbrPathTools: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tlbtnCreateNewPath: TToolButton;
    tlbtnDeleteCurrentPath: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FPathManager : TgmPathManager;

    procedure PathManagerBeforeMouseDown(ASender: TObject);

    procedure PathManagerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    // callback function for double-click on path panel
    procedure PathPanelDblClick(Sender: TObject);
  public
    property PathManager : TgmPathManager read FPathManager;
  end;

var
  frmPaths: TfrmPaths;

implementation

uses
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagicLib }
  gmGUIFuncs,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  RichTextEditorForm,
  SavePathDlg;

{$R *.dfm}

procedure TfrmPaths.FormActivate(Sender: TObject);
begin
  if Assigned(FPathManager) then
  begin
    FPathManager.SetFocus();
  end;
end;

procedure TfrmPaths.FormCreate(Sender: TObject);
begin
  FPathManager := TgmPathManager.Create(Self);

  with FPathManager do
  begin
    Parent              := Self;
    Align               := alClient;
    BeforeMouseDown     := Self.PathManagerBeforeMouseDown;
    OnMouseUp           := Self.PathManagerMouseUp;
    OnPathPanelDblClick := Self.PathPanelDblClick;
    OnPathThumbDblClick := Self.PathPanelDblClick;
  end;

  ManualDock(frmMain.pgcntrlDockSite3);
  Show();
end;

procedure TfrmPaths.FormDestroy(Sender: TObject);
begin
  FPathManager.Free();
end;

procedure TfrmPaths.PathManagerBeforeMouseDown(ASender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    if Assigned(FPathManager) then
    begin
      FPathManager.IsEnabled := not ActiveChildForm.ToolsInPendingStatus();
    end;
  end;
end;

procedure TfrmPaths.PathManagerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FPathManager.PathList.SelectedPath) then
  begin
    if frmMain.MainTool <> gmtPenTools then
    begin
      frmMain.spdbtnPenTools.Down := True;
      frmMain.ChangeMainToolClick(frmMain.spdbtnPenTools);
    end;
  end;

  ActiveChildForm.imgWorkArea.Cursor := crPenToolDeselected;
  Screen.Cursor                      := crDefault;
end;

// callback function for double-click on path panel
procedure TfrmPaths.PathPanelDblClick(Sender: TObject);
begin
  if (ActiveChildForm.SelectionTransformation <> nil) or
     (ActiveChildForm.Crop <> nil) or
     frmRichTextEditor.Visible then
  begin
    Exit;
  end;

  frmSavePath := TfrmSavePath.Create(nil);
  try
    if frmSavePath.ShowModal = mrOK then
    begin
      with ActiveChildForm.PathList.SelectedPath do
      begin
        IsNamed  := True;
        PathName := frmSavePath.edtPathName.Text;
        
        FPathManager.Invalidate();
      end;
    end;
  finally
    FreeAndNil(frmSavePath);
  end;
end;

end.
