unit SavePathDlg;

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

// Update Date: 2015/07/04

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfrmSavePath = class(TForm)
    lblPathName: TLabel;
    edtPathName: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSavePath: TfrmSavePath;

implementation

uses
  MainForm;

{$R *.DFM}

procedure TfrmSavePath.FormShow(Sender: TObject);
begin
  if ActiveChildForm.PathList.SelectedPath.IsNamed then
  begin
    edtPathName.Text := ActiveChildForm.PathList.SelectedPath.PathName;
  end
  else
  begin
    edtPathName.Text := 'Path ' + IntToStr(ActiveChildForm.PathList.PathCounter + 1);
  end;
  
  ActiveControl := edtPathName;
end;

procedure TfrmSavePath.btbtnOKClick(Sender: TObject);
begin
  if edtPathName.Text = '' then
  begin
    MessageDlg('A non-empty path name is required.', mtError, [mbOK], 0);
    edtPathName.SetFocus;
    
    ModalResult := mrNone;
  end
  else
  begin
    ModalResult := mrOK;
  end;
end; 

end.
