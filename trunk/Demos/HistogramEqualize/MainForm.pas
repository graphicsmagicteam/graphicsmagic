unit MainForm;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 *
 * The Original Code is gmLinearAutoLevel.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2015/06/28

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ExtDlgs,
{ Graphics32 }
  GR32, GR32_Image;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    imgView: TImgView32;
    btnLoadImage: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    btnExecute: TButton;
    btnRestore: TButton;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FOriginalBitmap : TBitmap32;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG,
{ GraphicsMagicLib }
  gmHistogramEqualize;

{$R *.dfm}

const
  APP_CAPTION = 'Histogram Equalize';


procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  LStart : Cardinal;
  LEnd   : Cardinal;
begin
  Screen.Cursor := crHourGlass;
  try
    imgView.Bitmap.Assign(FOriginalBitmap);

    LStart := GetTickCount();
    HistogramEqualize(imgView.Bitmap);
    LEnd := GetTickCount();

    imgView.Bitmap.Changed();
    
    StatusBar.Panels[0].Text := Format('Process Time: %d ms', [ (LEnd - LStart) ]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'JPEG Image|*.jpg';

  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if Assigned(FOriginalBitmap) then
      begin
        FreeAndNil(FOriginalBitmap);
      end;

      FOriginalBitmap := TBitmap32.Create();
      FOriginalBitmap.LoadFromFile(OpenPictureDialog.FileName);
      imgView.Bitmap.Assign(FOriginalBitmap);

      Caption := APP_CAPTION + ' - ' + ExtractFileName(OpenPictureDialog.FileName);

      lblZoom.Enabled     := True;
      cmbbxZoom.Enabled   := True;
      cmbbxZoom.ItemIndex := 3;
      cmbbxZoomChange(nil);

      btnRestore.Enabled := True;
      btnExecute.Enabled := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgView.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

procedure TfrmMain.btnRestoreClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    imgView.Bitmap.Assign(FOriginalBitmap);
    imgView.Bitmap.Changed();
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FOriginalBitmap := nil;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FOriginalBitmap.Free();
end;

end.
