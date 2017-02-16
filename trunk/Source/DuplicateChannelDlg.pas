unit DuplicateChannelDlg;

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

// Update Date: 2015/11/15

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
{ GraphicsMagic lib }
  gmChannels;

type
  TfrmDuplicateChannel = class(TForm)
    lblDuplicateChannelName: TLabel;
    lblDuplicateChannelAs: TLabel;
    edtDuplicateChannelAs: TEdit;
    chckbxInvertChannel: TCheckBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure btbtnOKClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure FormSetup(AChannel: TgmCustomChannel);
  end;

var
  frmDuplicateChannel: TfrmDuplicateChannel;

implementation

{$R *.dfm}

procedure TfrmDuplicateChannel.FormSetup(AChannel: TgmCustomChannel);
begin
  if not Assigned(AChannel) then
  begin
    raise Exception.Create('[Error] TfrmDuplicateChannel.FormSetup(): Parameter AChannel is nil.');
  end;

  lblDuplicateChannelName.Caption := lblDuplicateChannelName.Caption + ' ' + AChannel.ChannelName;
  edtDuplicateChannelAs.Text      := AChannel.ChannelName + ' copy';
end; 


procedure TfrmDuplicateChannel.btbtnOKClick(Sender: TObject);
begin
  if edtDuplicateChannelAs.Text = '' then
  begin
    MessageDlg('The Channel Name cannot be empty.', mtError, [mbOK], 0);
    edtDuplicateChannelAs.SetFocus;

    Self.ModalResult := mrNone;
  end;
end;

end.
