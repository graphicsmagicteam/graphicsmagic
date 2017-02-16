unit HueSaturationDlg;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{ Graphics32 }
  GR32, GR32_RangeBars,
{ GraphicsMagic Lib }
  gmHueSaturationLayer,
  gmTypes;

type
  TfrmHueSaturation = class(TForm)
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    grpbxHueSaturation: TGroupBox;
    lblHue: TLabel;
    lblSaturation: TLabel;
    lblLightnessOrValue: TLabel;
    rdbtnHLS: TRadioButton;
    rdbtnHSV: TRadioButton;
    edtLightnessOrValue: TEdit;
    edtSaturation: TEdit;
    edtHue: TEdit;
    ggbrHue: TGaugeBar;
    ggbrSaturation: TGaugeBar;
    ggbrLightnessOrValue: TGaugeBar;
    procedure AdjustHue(Sender: TObject);
    procedure AdjustSaturation(Sender: TObject);
    procedure AdjustLightnessOrValue(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rdbtnHLSClick(Sender: TObject);
    procedure rdbtnHSVClick(Sender: TObject);
    procedure edtHueChange(Sender: TObject);
    procedure edtSaturationChange(Sender: TObject);
    procedure edtLightnessOrValueChange(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ExecuteChanges(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FAdjustMode           : TgmHueSaturationAdjustMode;
    FChangedH             : Integer;
    FChangedS             : Integer;
    FChangedLV            : Integer;
    FBarIsChanging        : Boolean;
    FEditIsChanging       : Boolean;
    FFormShowing          : Boolean;
    FWorkingOnEffectLayer : Boolean;
    FHueSaturationLayer   : TgmHueSaturationLayer;  // pointer to a Hue/Saturation layer

    procedure ExecuteHueSaturationAdjustment;
    procedure ExecuteHLS;
    procedure ExecuteHSV;
  public
    procedure AssociateToHueSaturationLayer(ALayer: TgmHueSaturationLayer);
  end;

var
  frmHueSaturation: TfrmHueSaturation;

implementation

uses
{ GraphicsMagic Lib }
  gmChannelManager,
  gmImageProcessFuncs,
  gmIni,
  gmLayers,
  gmMath,               // EnsureValueInRange()
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

const
  MEDIAN_S_L_V    : Integer = 128;
  MEDIAN_H        : Integer = 180;
  MIN_RANGE_H     : Integer = -180;
  MAX_RANGE_H     : Integer = 180;
  MIN_RANGE_S_L_V : Integer = -128;
  MAX_RANGE_S_L_V : Integer = 128;

{ Custom procedures and functions }

procedure TfrmHueSaturation.AssociateToHueSaturationLayer(
  ALayer: TgmHueSaturationLayer);
begin
  if Assigned(ALayer) then
  begin
    FHueSaturationLayer := ALayer;

    rdbtnHLS.Checked := (FHueSaturationLayer.AdjustMode = hsamHSL);
    rdbtnHSV.Checked := (FHueSaturationLayer.AdjustMode = hsamHSV);

    ggbrHue.Position              := FHueSaturationLayer.Hue + MEDIAN_H;
    ggbrSaturation.Position       := FHueSaturationLayer.Saturation + MEDIAN_S_L_V;
    ggbrLightnessOrValue.Position := FHueSaturationLayer.LightValue + MEDIAN_S_L_V;

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmHueSaturation.ExecuteHueSaturationAdjustment;
begin
  case FAdjustMode of
    hsamHSL:
      begin
        ExecuteHLS;
      end;

    hsamHSV:
      begin
        ExecuteHSV;
      end;
  end;
end;

procedure TfrmHueSaturation.ExecuteHLS;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      AdjustImageHLS(frmMain.FBitmapBefore, frmMain.FBitmapAfter,
                     FChangedH, FChangedLV, FChangedS);
    end;

    if chckbxPreview.Checked then
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
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
      else if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
      begin
        FHueSaturationLayer.Changed();
      end;
    end;
  end;
end; 

procedure TfrmHueSaturation.ExecuteHSV;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      AdjustImageHSV(frmMain.FBitmapBefore, frmMain.FBitmapAfter,
                     FChangedH, FChangedS, FChangedLV);
    end;

    if chckbxPreview.Checked then
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
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
      else if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
      begin
        FHueSaturationLayer.Changed();
      end;
    end;
  end;
end; 

//------------------------------------------------------------------------------

procedure TfrmHueSaturation.AdjustHue(Sender: TObject);
var
  LChangedH : Integer;
begin
  FBarIsChanging := True;
  try
    LChangedH := ggbrHue.Position - MEDIAN_H;

    if FEditIsChanging then
    begin
      edtHue.Text := IntToStr(LChangedH);
    end
    else
    begin
      if LChangedH > 0 then
      begin
        edtHue.Text := '+' + IntToStr(LChangedH);
      end
      else
      begin
        edtHue.Text := IntToStr(LChangedH);
      end;
    end;

    // if on Hue/Saturation layer ...
    if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
    begin
      FHueSaturationLayer.Hue := LChangedH;
    end
    else
    begin
      FChangedH := LChangedH;
    end;
    
  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmHueSaturation.AdjustSaturation(Sender: TObject);
var
  LChangedS : Integer;
begin
  FBarIsChanging := True;
  try
    LChangedS := ggbrSaturation.Position - MEDIAN_S_L_V;

    if FEditIsChanging then
    begin
      edtSaturation.Text := IntToStr(LChangedS);
    end
    else
    begin
      if LChangedS > 0 then
      begin
        edtSaturation.Text := '+' + IntToStr(LChangedS);
      end
      else
      begin
        edtSaturation.Text := IntToStr(LChangedS);
      end;
    end;

    // if on Hue/Saturation layer ...
    if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
    begin
      FHueSaturationLayer.Saturation := LChangedS;
    end
    else
    begin
      FChangedS := LChangedS;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmHueSaturation.AdjustLightnessOrValue(Sender: TObject);
var
  LChangedLV : Integer;
begin
  FBarIsChanging := True;
  try
    LChangedLV := ggbrLightnessOrValue.Position - MEDIAN_S_L_V;

    if FEditIsChanging then
    begin
      edtLightnessOrValue.Text := IntToStr(LChangedLV);
    end
    else
    begin
      if LChangedLV > 0 then
      begin
        edtLightnessOrValue.Text := '+' + IntToStr(LChangedLV);
      end
      else
      begin
        edtLightnessOrValue.Text := IntToStr(LChangedLV);
      end;
    end;

    // if on Hue/Saturation layer ...
    if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
    begin
      FHueSaturationLayer.LightValue := LChangedLV;
    end
    else
    begin
      FChangedLV := LChangedLV;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmHueSaturation.ExecuteChanges(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteHueSaturationAdjustment;
end;

procedure TfrmHueSaturation.FormCreate(Sender: TObject);
begin
  FChangedH             := 0;
  FChangedS             := 0;
  FChangedLV            := 0;
  FFormShowing          := False;
  FBarIsChanging        := False;
  FEditIsChanging       := False;
  FWorkingOnEffectLayer := False;
  FHueSaturationLayer   := nil;

  FAdjustMode := TgmHueSaturationAdjustMode(
    StrToInt(ReadInfoFromIniFile(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_MODE, '0')));
end; 

procedure TfrmHueSaturation.FormShow(Sender: TObject);
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  FFormShowing := True;
  try
    // if not working on special layer ...
    if not FWorkingOnEffectLayer then
    begin
      rdbtnHLS.Checked := (FAdjustMode = hsamHSL);
      rdbtnHSV.Checked := (FAdjustMode = hsamHSV);

      chckbxPreview.Checked :=
        Boolean(StrToInt(ReadInfoFromIniFile(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_PREVIEW, '1')));

      with ActiveChildForm do
      begin
        if Assigned(Selection) then
        begin
          frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
        end
        else
        begin
          if (LayerList.SelectedLayer is TgmNormalLayer) and
             (ChannelManager.CurrentChannelType = ctColorChannel) then
          begin
            frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
          end;
        end;
      end;

      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

      ggbrHue.Position              := MEDIAN_H;
      ggbrSaturation.Position       := MEDIAN_S_L_V;
      ggbrLightnessOrValue.Position := MEDIAN_S_L_V;
    end;

    if rdbtnHLS.Checked then
    begin
      lblLightnessOrValue.Caption := 'Lightness:'
    end
    else if rdbtnHSV.Checked then
    begin
      lblLightnessOrValue.Caption := 'Value:';
    end;

    ActiveControl := btbtnOK;
  finally
    FFormShowing := False;
  end;
end;

procedure TfrmHueSaturation.rdbtnHLSClick(Sender: TObject);
begin
  if not FFormShowing then
  begin
    // if on Hue/Saturation layer ...
    if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
    begin
      FHueSaturationLayer.AdjustMode := hsamHSL;
    end
    else
    begin
      FAdjustMode := hsamHSL;
    end;

    lblLightnessOrValue.Caption   := 'Lightness:';
    ggbrHue.Position              := MEDIAN_H;
    ggbrSaturation.Position       := MEDIAN_S_L_V;
    ggbrLightnessOrValue.Position := MEDIAN_S_L_V;

    ExecuteHueSaturationAdjustment;
  end;
end;

procedure TfrmHueSaturation.rdbtnHSVClick(Sender: TObject);
begin
  if not FFormShowing then
  begin
    // if on Hue/Saturation layer ...
    if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
    begin
      FHueSaturationLayer.AdjustMode := hsamHSV;
    end
    else
    begin
      FAdjustMode := hsamHSV;
    end;

    lblLightnessOrValue.Caption   := 'Value:';
    ggbrHue.Position              := MEDIAN_H;
    ggbrSaturation.Position       := MEDIAN_S_L_V;
    ggbrLightnessOrValue.Position := MEDIAN_S_L_V;

    ExecuteHueSaturationAdjustment;
  end;  
end;

procedure TfrmHueSaturation.edtHueChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtHue.Text);
        EnsureValueInRange(LChangedValue, MIN_RANGE_H, MAX_RANGE_H);
        ggbrHue.Position := MEDIAN_H + LChangedValue;
        ExecuteHueSaturationAdjustment;
      except
        if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FHueSaturationLayer.Hue;
        end
        else
        begin
          LRightValue := FChangedH;
        end;

        edtHue.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end;

procedure TfrmHueSaturation.edtSaturationChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtSaturation.Text);
        EnsureValueInRange(LChangedValue, MIN_RANGE_S_L_V, MAX_RANGE_S_L_V);
        ggbrSaturation.Position := MEDIAN_S_L_V + LChangedValue;
        ExecuteHueSaturationAdjustment;
      except
        if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FHueSaturationLayer.Saturation;
        end
        else
        begin
          LRightValue := FChangedS;
        end;

        edtSaturation.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end;

procedure TfrmHueSaturation.edtLightnessOrValueChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtLightnessOrValue.Text);
        EnsureValueInRange(LChangedValue, MIN_RANGE_S_L_V, MAX_RANGE_S_L_V);
        ggbrLightnessOrValue.Position := MEDIAN_S_L_V + LChangedValue;
        ExecuteHueSaturationAdjustment;
      except
        if Assigned(FHueSaturationLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FHueSaturationLayer.LightValue;
        end
        else
        begin
          LRightValue := FChangedLV;
        end;

        edtLightnessOrValue.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end;

procedure TfrmHueSaturation.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteHueSaturationAdjustment;
end;

procedure TfrmHueSaturation.btbtnOKClick(Sender: TObject);
begin
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_MODE, IntToStr(Ord(FAdjustMode)));
    WriteInfoToIniFile(SECTION_HUE_SATURATION_DIALOG, IDENT_HUE_SATURATION_PREVIEW, IntToStr(Integer(chckbxPreview.Checked)));
  end;
end;

end.


