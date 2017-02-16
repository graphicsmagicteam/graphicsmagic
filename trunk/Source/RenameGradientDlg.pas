{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit RenameGradientDlg;

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
  StdCtrls, Buttons, ExtCtrls, GR32_Image, GR32;

type
  TfrmGradientName = class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    Panel1: TPanel;
    imgSelectedGradient: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure imgSelectedGradientPaintStage(Sender: TObject;
      Buffer: TBitmap32; StageNum: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmGradientName: TfrmGradientName;

implementation

uses
{ GraphicsMagic Package Lib }
  gmGradient,
{ GraphicsMagic Lib }
  gmPaintFuncs,
{ GraphicsMagic Forms/Dialogs }
  GradientPickerPopFrm;

{$R *.DFM}

procedure TfrmGradientName.FormCreate(Sender: TObject);
begin
  imgSelectedGradient.SetupBitmap;
  imgSelectedGradient.Bitmap.DrawMode := dmBlend;

  with imgSelectedGradient.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;
end; 

procedure TfrmGradientName.FormShow(Sender: TObject);
var
  LGradient: TgmGradientItem;
begin
  if Assigned(frmGradientPicker.ggGradients.Gradients) and
     (frmGradientPicker.ggGradients.Gradients.IsValidIndex(frmGradientPicker.ggGradients.GradientIndex)) then
  begin
    LGradient := frmGradientPicker.ggGradients.Gradients[frmGradientPicker.ggGradients.GradientIndex];

    LGradient.GradientLength := imgSelectedGradient.Bitmap.Width;
    LGradient.RefreshColorArray;
    
    LGradient.DrawColorGradients(imgSelectedGradient.Bitmap, False);
    imgSelectedGradient.Bitmap.Changed;

    edtName.Text := LGradient.DisplayName;
    edtName.SetFocus;
  end;
end;

procedure TfrmGradientName.btbtnOKClick(Sender: TObject);
begin
  if edtName.Text = '' then
  begin
    edtName.Text := 'Not named';
  end;
end;

procedure TfrmGradientName.imgSelectedGradientPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboardPattern(Buffer, True);
end;

end.
