unit GradientMapDlg;

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

// Update Date: 2016/04/20

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
{ Graphics32 }
  GR32_Image,
{ GraphicsMagic Lib }
  gmGradientMap;

type
  TfrmGradientMap = class(TForm)
    grpbxGradientMapHolder: TGroupBox;
    pnlGradientMapHolder: TPanel;
    imgSelectedGradient: TImage32;
    spdbtnOpenGradientPicker: TSpeedButton;
    grpbxGradientOptions: TGroupBox;
    chckbxReverse: TCheckBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure spdbtnOpenGradientPickerClick(Sender: TObject);
    procedure spdbtnOpenGradientPickerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chckbxReverseClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure imgSelectedGradientClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FGradientMap       : TgmGradientMap;  // for Gradient Map menu Command
    FButtonX, FButtonY : Integer;

    procedure ShowMapLayerGradients;
  public
    procedure ExecuteGradientMap;
  end;

var
  frmGradientMap: TfrmGradientMap;

implementation

uses
{ GraphicsMagic Package Lib }
  gmGradient,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmGradientMapLayer,
  gmIni,
  gmLayers,
{ GraphicsMagic Forms }
  GradientPickerPopFrm,
  MainForm,
{ GraphicsMagic Dialogs }
  GradientEditorDlg;

{$R *.dfm}

{ Custom Methods }

procedure TfrmGradientMap.ShowMapLayerGradients;
var
  LGradientMapLayer : TgmGradientMapLayer;
  LTempGradient     : TgmGradientItem;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      // show gradients in this form
      LTempGradient := TgmGradientItem.Create(nil);
      try
        LGradientMapLayer := TgmGradientMapLayer(LayerList.SelectedLayer);

        LTempGradient.Assign(LGradientMapLayer.Gradient);
        LTempGradient.GradientLength := imgSelectedGradient.Bitmap.Width;
        LTempGradient.RefreshColorArray;

        LTempGradient.DrawColorGradients(imgSelectedGradient.Bitmap,
                                         chckbxReverse.Checked);

        imgSelectedGradient.Bitmap.Changed;
      finally
        LTempGradient.Free;
      end;
    end;
  end;
end;

procedure TfrmGradientMap.ExecuteGradientMap;
var
  LGradient : TgmGradientItem;
  LIndex    : Integer;
begin
  with ActiveChildForm do
  begin
    if (LayerList.SelectedLayer is TgmNormalLayer) and
       (ChannelManager.CurrentChannelType = ctColorChannel) then
    begin
      if Assigned(FGradientMap) then
      begin
        LIndex    := frmGradientPicker.FMapCommandGradientListState.SelectedIndex;
        LGradient := frmGradientPicker.glMapCommandGradients.Items[LIndex];

        FGradientMap.Draw(frmMain.FBitmapAfter, LGradient, chckbxReverse.Checked);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      if (LayerList.SelectedLayer is TgmNormalLayer) and
         (ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
          ShowProcessedSelection();
        end
        else
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
          LayerList.SelectedLayer.Changed();
        end;
      end
      else if LayerList.SelectedLayer is TgmGradientMapLayer then
      begin
        LayerList.SelectedLayer.Changed();
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmGradientMap.FormCreate(Sender: TObject);
begin
  imgSelectedGradient.SetupBitmap();
  FGradientMap := nil;
end;

procedure TfrmGradientMap.FormDestroy(Sender: TObject);
begin
  if Assigned(FGradientMap) then
  begin
    FGradientMap.Free();
  end;
end; 

procedure TfrmGradientMap.FormShow(Sender: TObject);
var
  LGradientMapLayer : TgmGradientMapLayer;
begin
  with ActiveChildForm do
  begin
    if (LayerList.SelectedLayer is TgmNormalLayer) and
       (ChannelManager.CurrentChannelType = ctColorChannel) then
    begin
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(ActiveChildForm.Selection.CutOriginal);
      end
      else
      begin
        frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
      end;

      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

      if Assigned(FGradientMap) then
      begin
        FreeAndNil(FGradientMap);
      end;

      // Create Gradient Map for menu command 
      FGradientMap := TgmGradientMap.Create(frmMain.FBitmapBefore);

      chckbxReverse.Checked := Boolean(StrToInt(
        ReadInfoFromIniFile(SECTION_GRADIENT_MAP_DIALOG, IDENT_GRADIENT_MAP_DIALOG_REVERSE, '1')));

      chckbxPreview.Checked := Boolean(StrToInt(
        ReadInfoFromIniFile(SECTION_GRADIENT_MAP_DIALOG, IDENT_GRADIENT_MAP_DIALOG_PREVIEW, '1')));

      frmGradientPicker.ShowMapCommandSelectedGradient;
    end
    else if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      LGradientMapLayer     := TgmGradientMapLayer(LayerList.SelectedLayer);
      chckbxReverse.Checked := LGradientMapLayer.IsReversed;
      chckbxPreview.Checked := True;

      // show gradients in this form
      ShowMapLayerGradients;
    end;
  end;

  ExecuteGradientMap;
  ActiveControl := btbtnOK;
end;

procedure TfrmGradientMap.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FGradientMap) then
  begin
    FreeAndNil(FGradientMap);
  end;
end; 

procedure TfrmGradientMap.spdbtnOpenGradientPickerClick(Sender: TObject);
var
  p : TPoint;
begin
  GetCursorPos(p);
  frmGradientPicker.Left := p.X - frmGradientPicker.Width - FButtonX;
  frmGradientPicker.Top  := p.Y + (spdbtnOpenGradientPicker.Height - FButtonY);

  with ActiveChildForm do
  begin
    if (LayerList.SelectedLayer is TgmNormalLayer) and
       (ChannelManager.CurrentChannelType = ctColorChannel) then
    begin
      frmGradientPicker.GradientUser := guGradientMapCommand;
    end
    else if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      frmGradientPicker.GradientUser := guGradientMapLayer;
    end;
  end;

  frmGradientPicker.Show();
end; 

procedure TfrmGradientMap.spdbtnOpenGradientPickerMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FButtonX := X;
  FButtonY := Y;
end; 

procedure TfrmGradientMap.chckbxReverseClick(Sender: TObject);
var
  LGradientMapLayer : TgmGradientMapLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      frmGradientPicker.ShowMapCommandSelectedGradient();
    end
    else if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      LGradientMapLayer := TgmGradientMapLayer(LayerList.SelectedLayer);
      LGradientMapLayer.IsReversed := chckbxReverse.Checked;

      ShowMapLayerGradients();
    end;

    ExecuteGradientMap();
  end;
end; 

procedure TfrmGradientMap.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteGradientMap();
end;

procedure TfrmGradientMap.imgSelectedGradientClick(Sender: TObject);
begin
  frmGradientEditor := TfrmGradientEditor.Create(nil);
  try
    with ActiveChildForm do
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
      begin
        frmGradientEditor.GradientUser := guGradientMapCommand;
      end
      else if LayerList.SelectedLayer is TgmGradientMapLayer then
      begin
        frmGradientEditor.GradientUser := guGradientMapLayer;
      end;
    end;

    frmGradientEditor.ShowModal();
  finally
    FreeAndNil(frmGradientEditor);
  end;
end;

procedure TfrmGradientMap.btbtnOKClick(Sender: TObject);
var
  LGradientMapLayer : TgmGradientMapLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      WriteInfoToIniFile(SECTION_GRADIENT_MAP_DIALOG,
                         IDENT_GRADIENT_MAP_DIALOG_Reverse,
                         IntToStr(Integer(chckbxReverse.Checked)));

      WriteInfoToIniFile(SECTION_GRADIENT_MAP_DIALOG,
                         IDENT_GRADIENT_MAP_DIALOG_PREVIEW,
                         IntToStr(Integer(chckbxPreview.Checked)));
    end
    else if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      LGradientMapLayer := TgmGradientMapLayer(LayerList.SelectedLayer);
      LGradientMapLayer.IsReversed := chckbxReverse.Checked;
    end;
  end;
end;

end.
