unit FeatherSelectionDlg;

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
 * Update Date: June 5, 2014
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfrmFeatherSelection = class(TForm)
    grpbxFeatherRadius: TGroupBox;
    lblFeatherRadius: TLabel;
    edtFeatherRadius: TEdit;
    lblPixels: TLabel;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtFeatherRadiusChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FFeatherRadius: Integer;
  public
    property FeatherRadius : Integer read FFeatherRadius;
  end;

var
  frmFeatherSelection: TfrmFeatherSelection;

implementation

uses
{ GraphicsMagic Lib }
  gmMath,
  gmIni;


{$R *.DFM}

procedure TfrmFeatherSelection.FormShow(Sender: TObject);
begin
  ActiveControl := edtFeatherRadius;
end;

procedure TfrmFeatherSelection.FormCreate(Sender: TObject);
begin
  FFeatherRadius        := StrToInt(ReadInfoFromIniFile(SECTION_SELECTION, IDENT_FEATHER_RADIUS, '0'));
  edtFeatherRadius.Text := IntToStr(FFeatherRadius);
end; 

procedure TfrmFeatherSelection.edtFeatherRadiusChange(Sender: TObject);
begin
  try
    FFeatherRadius := StrToInt(edtFeatherRadius.Text);
    EnsureValueInRange(FFeatherRadius, 0, 255);
    edtFeatherRadius.Text := IntToStr(FFeatherRadius);
  except
    edtFeatherRadius.Text := IntToStr(FFeatherRadius);
  end;
end;

procedure TfrmFeatherSelection.btbtnOKClick(Sender: TObject);
begin
  WriteInfoToIniFile(SECTION_SELECTION, IDENT_FEATHER_RADIUS, IntToStr(FFeatherRadius));
end; 

end.
