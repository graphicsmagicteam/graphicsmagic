unit PatternFillDlg;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{ Graphics32 }
  GR32_RangeBars, GR32_Image;

type
  TfrmPatternFill = class(TForm)
    pnlPattern: TPanel;
    spdbtnOpenPattern: TSpeedButton;
    lblScale: TLabel;
    edtScale: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    lblScalePercent: TLabel;
    ggbrScale: TGaugeBar;
    imgPattern: TImage32;
    procedure FormShow(Sender: TObject);
    procedure spdbtnOpenPatternClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtScaleChange(Sender: TObject);
    procedure ggbrScaleChange(Sender: TObject);
  private
    FFormShowing   : Boolean;
    FBarIsChanging : Boolean;
  public
    { Public declarations }
  end;

var
  frmPatternFill: TfrmPatternFill;

implementation

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmGUIFuncs,
  gmImageProcessFuncs,
  gmMath,
  gmPatternLayer,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PatternsPopFrm;

{$R *.DFM}

procedure TfrmPatternFill.FormCreate(Sender: TObject);
begin
  FFormShowing   := False;
  FBarIsChanging := False;
end;

procedure TfrmPatternFill.FormShow(Sender: TObject);
var
  LPatternLayer : TgmPatternLayer;
  LScaledBitmap : TBitmap32;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;
  
  FFormShowing := True;
  try
    LScaledBitmap := TBitmap32.Create;
    try
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmPatternLayer then
        begin
          LPatternLayer := TgmPatternLayer(LayerList.SelectedLayer);

          GetScaledBitmap(LPatternLayer.PatternBitmap, LScaledBitmap,
                          pnlPattern.Width - 4, pnlPattern.Height - 4);

          imgPattern.Bitmap.Assign(LScaledBitmap);
          CenterImageInPanel(pnlPattern, imgPattern);
          
          ggbrScale.Position := Round(LPatternLayer.Scale * 100);
        end;
      end;
    finally
      LScaledBitmap.Free;
    end;
    
  finally
    FFormShowing := False;
  end;
  
  ActiveControl := btbtnOK;
end;

procedure TfrmPatternFill.spdbtnOpenPatternClick(Sender: TObject);
var
  LShowingPoint : TPoint;
begin
  GetCursorPos(LShowingPoint);
  
  frmPatterns.Left            := LShowingPoint.X;
  frmPatterns.Top             := LShowingPoint.Y;
  frmPatterns.PatternListUser := pluPatternLayer;
  
  frmPatterns.Show;
end;

procedure TfrmPatternFill.edtScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  if not FBarIsChanging then
  begin
    try
      LChangedValue := StrToInt(edtScale.Text);
      EnsureValueInRange(LChangedValue, ggbrScale.Min, ggbrScale.Max);
      ggbrScale.Position := LChangedValue;
    except
      edtScale.Text := IntToStr(ggbrScale.Position);
    end;
  end;
end;

procedure TfrmPatternFill.ggbrScaleChange(Sender: TObject);
begin
  FBarIsChanging := True;
  try
    edtScale.Text := IntToStr(ggbrScale.Position);

    if not FFormShowing then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmPatternLayer then
        begin
          TgmPatternLayer(LayerList.SelectedLayer).Scale := ggbrScale.Position / 100;
          LayerList.SelectedLayer.Changed();
        end;
      end;
    end;
  finally
    FBarIsChanging := False;
  end;
end; 

end.
