unit ChannelOptionsDlg;

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
  Dialogs, ExtCtrls, StdCtrls, Buttons,
{ GraphicsMagicLib }
  gmChannels,
  gmChannelManager;

type
  TfrmChannelOptions = class(TForm)
    lblChannelName: TLabel;
    edtChannelName: TEdit;
    rdgrpColorIndicator: TRadioGroup;
    grpbxColor: TGroupBox;
    shpMaskColor: TShape;
    lblMaskOpacity: TLabel;
    edtMaskOpacity: TEdit;
    Label1: TLabel;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    clrdlgMaskColorSelector: TColorDialog;
    procedure shpMaskColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtMaskOpacityChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FAssociatedChannel : TgmAlphaChannel;
  public
    procedure FormSetup(AChannel: TgmAlphaChannel;
      const AChannelType: TgmChannelType);
  end;

var
  frmChannelOptions: TfrmChannelOptions;

implementation

uses
{ Graphics32 }
  GR32,
  GR32_LowLevel;

{$R *.dfm}

procedure TfrmChannelOptions.shpMaskColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clrdlgMaskColorSelector.Color := shpMaskColor.Brush.Color;

  if clrdlgMaskColorSelector.Execute then
  begin
    shpMaskColor.Brush.Color := clrdlgMaskColorSelector.Color;
  end;
end;

procedure TfrmChannelOptions.edtMaskOpacityChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtMaskOpacity.Text);
    LChangedValue := Clamp(LChangedValue, 0, 100);

    edtMaskOpacity.Text := IntToStr(LChangedValue);
  except
    edtMaskOpacity.Text := IntToStr(Round(FAssociatedChannel.MaskOpacity / 255 * 100));
  end; 
end;

procedure TfrmChannelOptions.FormSetup(AChannel: TgmAlphaChannel;
  const AChannelType: TgmChannelType);
begin
  FAssociatedChannel  := AChannel;
  edtChannelName.Text := AChannel.ChannelName;

  case AChannelType of
    ctLayerMaskChannel:
      begin
        Caption                     := 'Layer Mask Options';
        lblChannelName.Caption      := 'Name: ' + edtChannelName.Text;
        grpbxColor.Caption          := 'Overlay';
        rdgrpColorIndicator.Enabled := False;
      end;

    ctQuickMaskChannel:
      begin
        Caption                := 'Quick Mask Options';
        lblChannelName.Caption := 'Name: ' + edtChannelName.Text;
      end;
  end;

  edtChannelName.Visible        := (AChannelType = ctAlphaChannel);
  rdgrpColorIndicator.ItemIndex := Ord(AChannel.MaskColorIndicator);
  shpMaskColor.Brush.Color      := WinColor(AChannel.MaskColor);
  edtMaskOpacity.Text           := IntToStr(Round(AChannel.MaskOpacity / 255 * 100));
end;

procedure TfrmChannelOptions.btbtnOKClick(Sender: TObject);
begin
  if edtChannelName.Text = '' then
  begin
    MessageDlg('The Channel Name cannot be empty.', mtError, [mbOK], 0);
    edtChannelName.SetFocus;

    Self.ModalResult := mrNone;
  end;
end;

end.
