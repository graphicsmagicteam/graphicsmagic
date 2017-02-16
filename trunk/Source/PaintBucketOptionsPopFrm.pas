{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit PaintBucketOptionsPopFrm;

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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
  TfrmPaintBucketAdvancedOptions = class(TForm)
    pnlPaintBucketAdvancedOptions: TPanel;
    grpbxPaintBucketAdvancedSettings: TGroupBox;
    lblFillType: TLabel;
    cmbbxFillType: TComboBox;
    lblFillIntensity: TLabel;
    edtFillIntensity: TEdit;
    updwnFillIntensity: TUpDown;
    lblFillIntensityPercentSign: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmbbxFillTypeChange(Sender: TObject);
    procedure edtFillIntensityChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCanChange    : Boolean;
    FFillIntensity: Integer;
  public
    { Public declarations }
  end;

var
  frmPaintBucketAdvancedOptions: TfrmPaintBucketAdvancedOptions;

implementation

{$R *.DFM}

procedure TfrmPaintBucketAdvancedOptions.FormCreate(Sender: TObject);
begin
  cmbbxFillType.ItemIndex := 0;
  FFillIntensity          := 0;
  FCanChange              := False;
end;

procedure TfrmPaintBucketAdvancedOptions.cmbbxFillTypeChange(
  Sender: TObject);
var
  LIntensityEnabled: Boolean;
begin
  LIntensityEnabled                   := (cmbbxFillType.ItemIndex > 3);
  lblFillIntensity.Enabled            := LIntensityEnabled;
  lblFillIntensityPercentSign.Enabled := LIntensityEnabled;
  edtFillIntensity.Enabled            := LIntensityEnabled;
  updwnFillIntensity.Enabled          := LIntensityEnabled;
  
  if LIntensityEnabled then
  begin
    edtFillIntensity.Color := clWindow;
  end
  else
  begin
    edtFillIntensity.Color := clBtnFace;
  end;
end; 

procedure TfrmPaintBucketAdvancedOptions.edtFillIntensityChange(
  Sender: TObject);
begin
  if FCanChange then
  begin
    try
      updwnFillIntensity.Position := StrToInt(edtFillIntensity.Text);
      FFillIntensity              := updwnFillIntensity.Position;
    Except
      edtFillIntensity.Text := IntToStr(FFillIntensity);
    end;
  end;
end;

procedure TfrmPaintBucketAdvancedOptions.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmPaintBucketAdvancedOptions.FormShow(Sender: TObject);
begin
  FCanChange := True;
end; 

end.
