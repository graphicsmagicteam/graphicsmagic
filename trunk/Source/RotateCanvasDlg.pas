unit RotateCanvasDlg;

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

// Update Date: 2015/09/19

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, gmTypes;

type
  TfrmRotateCanvas = class(TForm)
    bvlRotateCanvas: TBevel;
    lblAngle: TLabel;
    edtAngle: TEdit;
    rdbtnClockwiseRotate: TRadioButton;
    rdbtnCounterclockwiseRotate: TRadioButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    imgDegrees: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtAngleChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ChangeRotateDirection(Sender: TObject);
  private
    FAngle           : Integer;
    FRotateDirection : TgmRotateDirection;

    procedure ChangeBitmapColor;
  public
    property RotateAngle     : Integer            read FAngle;
    property RotateDirection : TgmRotateDirection read FRotateDirection;
  end;

var
  frmRotateCanvas: TfrmRotateCanvas;

implementation

uses
  gmMath, gmIni;

{$R *.DFM}

const
  MAX_ROTATE_ANGLE = 360;
  MIN_ROTATE_ANGLE = 0;


procedure TfrmRotateCanvas.ChangeBitmapColor;
var
  x, y : Integer;
begin
  with imgDegrees.Picture do
  begin
    Bitmap.PixelFormat := pf24bit;

    for y := 0 to (Bitmap.Height - 1) do
    begin
      for x := 0 to (Bitmap.Width - 1) do
      begin
        if Bitmap.Canvas.Pixels[x, y] = clWhite then
        begin
          Bitmap.Canvas.Pixels[x, y] := clBtnFace;
        end;
      end;
    end;
  end;
end;

procedure TfrmRotateCanvas.FormShow(Sender: TObject);
begin
  ActiveControl := edtAngle;
end; 

procedure TfrmRotateCanvas.FormCreate(Sender: TObject);
begin
  FAngle := StrToInt( ReadInfoFromIniFile(SECTION_ROTATE_CANVAS_DIALOG,
                                          IDENT_ROTATE_CANVAS_ANGLE, '0') );

  FRotateDirection := TgmRotateDirection(
    StrToInt(ReadInfoFromIniFile(SECTION_ROTATE_CANVAS_DIALOG,
                                 IDENT_ROTATE_CANVAS_DIRECTION, '0')));

  edtAngle.Text := IntToStr(FAngle);
  
  case FRotateDirection of
    rdClockwise:
      begin
        rdbtnClockwiseRotate.Checked := True;
      end;
      
    rdCounterclockwise:
      begin
        rdbtnCounterclockwiseRotate.Checked := True;
      end;
  end;

  ChangeBitmapColor();
end; 

procedure TfrmRotateCanvas.edtAngleChange(Sender: TObject);
begin
  try
    FAngle := StrToInt(edtAngle.Text);
    
    if (FAngle < MIN_ROTATE_ANGLE) or (FAngle > MAX_ROTATE_ANGLE) then
    begin
      EnsureValueInRange(FAngle, MIN_ROTATE_ANGLE, MAX_ROTATE_ANGLE);
      edtAngle.Text := IntToStr(FAngle);
    end;
  except
    edtAngle.Text := IntToStr(FAngle);
  end;
end; 

procedure TfrmRotateCanvas.btbtnOKClick(Sender: TObject);
begin
  // save settings in INI file
  WriteInfoToIniFile(SECTION_ROTATE_CANVAS_DIALOG, IDENT_ROTATE_CANVAS_ANGLE, IntToStr(FAngle));
  WriteInfoToIniFile(SECTION_ROTATE_CANVAS_DIALOG, IDENT_ROTATE_CANVAS_DIRECTION, IntToStr(Ord(FRotateDirection)));
end; 

procedure TfrmRotateCanvas.ChangeRotateDirection(Sender: TObject);
begin
  if Sender = rdbtnClockwiseRotate then
  begin
    FRotateDirection := rdClockwise;
  end
  else if Sender = rdbtnCounterclockwiseRotate then
  begin
    FRotateDirection := rdCounterclockwise;
  end;
end; 

end.
