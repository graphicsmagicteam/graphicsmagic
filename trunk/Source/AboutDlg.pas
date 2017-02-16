unit AboutDlg;

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

// Update Date: 2017/01/23

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, jpeg;

type
  TfrmAbout = class(TForm)
    pnlPicture: TPanel;
    lblProgramName: TLabel;
    lblCopyright: TLabel;
    btbtnOk: TBitBtn;
    lblVersion: TLabel;
    btnLicence: TButton;
    memoSpeeches: TMemo;
    btnCredits: TButton;
    Label1: TLabel;
    procedure ShowLicence(Sender: TObject);
    procedure ShowCredits(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure ShowGraphicsMagicAbout;

var
  frmAbout: TfrmAbout;

implementation

uses
  LicenceDlg, CreditsDlg, MainForm;

{$R *.DFM}

procedure ShowGraphicsMagicAbout;
begin
  frmAbout := TfrmAbout.Create(nil);
  try
    frmAbout.ShowModal;
  finally
    FreeAndNil(frmAbout);
  end;
end;

procedure TfrmAbout.ShowLicence(Sender: TObject);
begin
  frmLicence := TfrmLicence.Create(nil);
  try
    frmLicence.ShowModal;
  finally
    FreeAndNil(frmLicence);
  end;
end;

procedure TfrmAbout.ShowCredits(Sender: TObject);
begin
  frmCredits := TfrmCredits.Create(nil);
  try
    frmCredits.ShowModal;
  finally
    FreeAndNil(frmCredits);
  end;
end;

function getSvnBuild():string;
var
  LBuildver: string;
  i        : Integer;
begin
  Result := '';
  //do not edit the number yourself. SVN will update automatically
  LBuildver := '$Revision: 1246 $';

  for i := 1 to Length(LBuildver) do
  begin
    if LBuildver[i] in ['0'..'9'] then
      Result := Result + LBuildver[i];
  end;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := Format('Professional %s (Build %s)',[VER,getSvnBuild()]);
end;

end.
