unit ChannelMixerDlg;

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

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
{ Graphics32 }
  GR32_RangeBars,
{ GraphicsMagic Lib }
  gmChannelMixer,
  gmChannelMixerLayer;

type
  TfrmChannelMixer = class(TForm)
    lblOutputChannel: TLabel;
    cmbbxOutputChannel: TComboBox;
    GroupBox1: TGroupBox;
    lblRed: TLabel;
    lblRedPercent: TLabel;
    lblGreen: TLabel;
    lblGreenPercent: TLabel;
    lblBlue: TLabel;
    lblBluePercent: TLabel;
    edtRedScale: TEdit;
    edtGreenScale: TEdit;
    edtBlueScale: TEdit;
    ggbrRedScale: TGaugeBar;
    ggbrGreenScale: TGaugeBar;
    ggbrBlueScale: TGaugeBar;
    lblConstant: TLabel;
    edtConstantScale: TEdit;
    lblConstantPercent: TLabel;
    chckbxMonochrome: TCheckBox;
    ggbrConstantScale: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    btnRestore: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeOutputChannel(Sender: TObject);
    procedure ggbrRedScaleChange(Sender: TObject);
    procedure ggbrGreenScaleChange(Sender: TObject);
    procedure ggbrBlueScaleChange(Sender: TObject);
    procedure ggbrRedScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrGreenScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrBlueScaleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrConstantScaleChange(Sender: TObject);
    procedure ggbrConstantScaleMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chckbxMonochromeClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure edtRedScaleChange(Sender: TObject);
    procedure edtGreenScaleChange(Sender: TObject);
    procedure edtBlueScaleChange(Sender: TObject);
    procedure edtConstantScaleChange(Sender: TObject);
    procedure edtRedScaleExit(Sender: TObject);
    procedure edtGreenScaleExit(Sender: TObject);
    procedure edtBlueScaleExit(Sender: TObject);
    procedure edtConstantScaleExit(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
  private
    FNormalChannelMixer   : TgmChannelMixer;  // used for menu command
    FChannelMixer         : TgmChannelMixer;  // pointer to an existing Channel Mixer
    FChannelMixerLayer    : TgmChannelMixerLayer;
    FEditBoxIsChanging    : Boolean;          // indicating the OnChange event of a TEdit is occur
    FSliderIsChanging     : Boolean;          // indicating the slider's position is changing
    FWorkingOnEffectLayer : Boolean;

    procedure UpdateForm;
    procedure ExecuteChannelMix;
  public
    procedure AssociateToChannelMixerLayer(ALayer: TgmChannelMixerLayer);
  end;

var
  frmChannelMixer: TfrmChannelMixer;

implementation

uses
{ Graphics32 }
  GR32,
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmIni,
  gmLayers,
  gmTypes,
{ GraphicsMagic Form/Dialogs }
  MainForm;

{$R *.dfm}

const
  MID_POS   = 200;
  MIN_VALUE = -200;
  MAX_VALUE = 200;

procedure TfrmChannelMixer.AssociateToChannelMixerLayer(
  ALayer: TgmChannelMixerLayer);
begin
  if Assigned(ALayer) then
  begin
    FChannelMixerLayer := ALayer;

    // pointer to the Channel Mixer that for the Channel Mixer layer
    FChannelMixer := FChannelMixerLayer.ChannelMixer;

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmChannelMixer.UpdateForm;
begin
  if Assigned(FChannelMixer) then
  begin
    cmbbxOutputChannel.Items.Clear;

    if FChannelMixer.Monochrome then
    begin
      cmbbxOutputChannel.Items.Add('Gray');
      cmbbxOutputChannel.ItemIndex := 0;
    end
    else
    begin
      cmbbxOutputChannel.Items.Add('Red');
      cmbbxOutputChannel.Items.Add('Green');
      cmbbxOutputChannel.Items.Add('Blue');

      cmbbxOutputChannel.ItemIndex := Ord(FChannelMixer.Channel);
    end;

    chckbxMonochrome.Checked := FChannelMixer.Monochrome;

    ggbrRedScale.Position      := Round(FChannelMixer.RedScale      * 100) + MID_POS;
    ggbrGreenScale.Position    := Round(FChannelMixer.GreenScale    * 100) + MID_POS;
    ggbrBlueScale.Position     := Round(FChannelMixer.BlueScale     * 100) + MID_POS;
    ggbrConstantScale.Position := Round(FChannelMixer.ConstantScale * 100) + MID_POS;

    // if the slider's position has not changed to modify the values in edit boxes
    if ggbrRedScale.Position = 0 then
    begin
      edtRedScale.Text := IntToStr(ggbrRedScale.Position - MID_POS);
    end;
    
    if ggbrGreenScale.Position = 0 then
    begin
      edtGreenScale.Text := IntToStr(ggbrGreenScale.Position - MID_POS);
    end;
    
    if ggbrBlueScale.Position = 0 then
    begin
      edtBlueScale.Text := IntToStr(ggbrBlueScale.Position - MID_POS);
    end;
    
    if ggbrConstantScale.Position = 0 then
    begin
      edtConstantScale.Text := IntToStr(ggbrConstantScale.Position - MID_POS);
    end;
  end;

  ActiveControl := btbtnOK;
end;

procedure TfrmChannelMixer.ExecuteChannelMix;
var
  i        : Integer;
  LSrcBits : PColor32;
  LDstBits : PColor32;
begin
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FChannelMixer) then
    begin
      if (ActiveChildForm.LayerList.SelectedLayer is TgmNormalLayer) and
         (ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        LSrcBits := @frmMain.FBitmapBefore.Bits[0];
        LDstBits := @frmMain.FBitmapAfter.Bits[0];

        for i := 0 to (frmMain.FBitmapAfter.Width * frmMain.FBitmapAfter.Height - 1) do
        begin
          FChannelMixer.InputColor := LSrcBits^;
          LDstBits^                := FChannelMixer.OutputColor;

          Inc(LSrcBits);
          Inc(LDstBits);
        end;
      end;

      if chckbxPreview.Checked then
      begin
        with ActiveChildForm do
        begin
          if Assigned(FChannelMixerLayer) and FWorkingOnEffectLayer then
          begin
            FChannelMixerLayer.Changed;
          end
          else
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
                LayerList.SelectedLayer.Changed;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmChannelMixer.FormCreate(Sender: TObject);
begin
  FNormalChannelMixer   := nil;
  FChannelMixer         := nil;
  FChannelMixerLayer    := nil;
  FEditBoxIsChanging    := False;
  FSliderIsChanging     := False;
  FWorkingOnEffectLayer := False;
end; 

procedure TfrmChannelMixer.FormDestroy(Sender: TObject);
begin
  if Assigned(FNormalChannelMixer) then
  begin
    FNormalChannelMixer.Free;
  end;
end;

procedure TfrmChannelMixer.FormShow(Sender: TObject);
var
  LDataStr : string;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  // if not working on special layer ...
  if not FWorkingOnEffectLayer then
  begin
    with ActiveChildForm do
    begin
      if (LayerList.SelectedLayer is TgmNormalLayer) and
         (ChannelManager.CurrentChannelType = ctColorChannel) then
      begin
        LDataStr := ReadInfoFromIniFile(SECTION_CHANNEL_MIXER_DIALOG,
                                        IDENT_CHANNEL_MIXER_PREVIEW, '1');
                                        
        chckbxPreview.Checked := Boolean( StrToInt(LDataStr) );

        if Assigned(Selection) then
        begin
          frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
        end
        else
        begin
          frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
        end;

        frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

        if Assigned(FNormalChannelMixer) then
        begin
          FreeAndNil(FNormalChannelMixer);
        end;

        FNormalChannelMixer := TgmChannelMixer.Create; // create Channel Mixer for menu command
        FChannelMixer       := FNormalChannelMixer;    // pointing to the Channel Mixer that menu command
      end;

    end;
  end;

  UpdateForm;
end;

procedure TfrmChannelMixer.ChangeOutputChannel(Sender: TObject);
begin
  if Assigned(FChannelMixer) then
  begin
    if not FChannelMixer.Monochrome then
    begin
      FChannelMixer.Channel := TgmChannelSelector(cmbbxOutputChannel.ItemIndex);
    end;

    ggbrRedScale.Position      := Round(FChannelMixer.RedScale      * 100) + MID_POS;
    ggbrGreenScale.Position    := Round(FChannelMixer.GreenScale    * 100) + MID_POS;
    ggbrBlueScale.Position     := Round(FChannelMixer.BlueScale     * 100) + MID_POS;
    ggbrConstantScale.Position := Round(FChannelMixer.ConstantScale * 100) + MID_POS;
  end;
end;

procedure TfrmChannelMixer.ggbrRedScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  FSliderIsChanging := True;
  try
    LChangedValue := ggbrRedScale.Position - MID_POS;

    if FEditBoxIsChanging = False then
    begin
      if LChangedValue > 0 then
      begin
        edtRedScale.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtRedScale.Text := IntToStr(LChangedValue);
      end;
    end
    else
    begin
      edtRedScale.Text := IntToStr(LChangedValue);
    end;

  finally
    FSliderIsChanging := False;
  end;
end; 

procedure TfrmChannelMixer.ggbrGreenScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  FSliderIsChanging := True;
  try
    LChangedValue := ggbrGreenScale.Position - MID_POS;

    if FEditBoxIsChanging = False then
    begin
      if LChangedValue > 0 then
      begin
        edtGreenScale.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtGreenScale.Text := IntToStr(LChangedValue);
      end;
    end
    else
    begin
      edtGreenScale.Text := IntToStr(LChangedValue);
    end;

  finally
    FSliderIsChanging := False;
  end;
end; 

procedure TfrmChannelMixer.ggbrBlueScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  FSliderIsChanging := True;
  try
    LChangedValue := ggbrBlueScale.Position - MID_POS;

    if FEditBoxIsChanging = False then
    begin
      if LChangedValue > 0 then
      begin
        edtBlueScale.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtBlueScale.Text := IntToStr(LChangedValue);
      end;
    end
    else
    begin
      edtBlueScale.Text := IntToStr(LChangedValue);
    end;

  finally
    FSliderIsChanging := False;
  end;
end;

procedure TfrmChannelMixer.ggbrConstantScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  FSliderIsChanging := True;
  try
    LChangedValue := ggbrConstantScale.Position - MID_POS;

    if FEditBoxIsChanging = False then
    begin
      if LChangedValue > 0 then
      begin
        edtConstantScale.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtConstantScale.Text := IntToStr(LChangedValue);
      end;
    end
    else
    begin
      edtConstantScale.Text := IntToStr(LChangedValue);
    end;

  finally
    FSliderIsChanging := False;
  end;
end;

procedure TfrmChannelMixer.ggbrRedScaleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.RedScale := (ggbrRedScale.Position - MID_POS) / 100;
    ExecuteChannelMix;
  end;
end;

procedure TfrmChannelMixer.ggbrGreenScaleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.GreenScale := (ggbrGreenScale.Position - MID_POS) / 100;
    ExecuteChannelMix;
  end;
end;

procedure TfrmChannelMixer.ggbrBlueScaleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.BlueScale := (ggbrBlueScale.Position - MID_POS) / 100;
    ExecuteChannelMix;
  end;
end;

procedure TfrmChannelMixer.ggbrConstantScaleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.ConstantScale := (ggbrConstantScale.Position - MID_POS) / 100;
    ExecuteChannelMix;
  end;
end;

procedure TfrmChannelMixer.chckbxMonochromeClick(Sender: TObject);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.Monochrome := chckbxMonochrome.Checked;

    if FChannelMixer.Monochrome then
    begin
      ExecuteChannelMix;

      ggbrRedScale.Position      := Round(FChannelMixer.RedScale      * 100) + MID_POS;
      ggbrGreenScale.Position    := Round(FChannelMixer.GreenScale    * 100) + MID_POS;
      ggbrBlueScale.Position     := Round(FChannelMixer.BlueScale     * 100) + MID_POS;
      ggbrConstantScale.Position := Round(FChannelMixer.ConstantScale * 100) + MID_POS;

      cmbbxOutputChannel.Items.Clear;
      cmbbxOutputChannel.Items.Add('Gray');
      cmbbxOutputChannel.ItemIndex := 0;
    end
    else
    begin
      cmbbxOutputChannel.Items.Clear;
      cmbbxOutputChannel.Items.Add('Red');
      cmbbxOutputChannel.Items.Add('Green');
      cmbbxOutputChannel.Items.Add('Blue');
      cmbbxOutputChannel.ItemIndex := 0;
    end;
  end;
end; 

procedure TfrmChannelMixer.btnRestoreClick(Sender: TObject);
begin
  if Assigned(FChannelMixer) then
  begin
    FChannelMixer.InitParameters;
    ExecuteChannelMix;

    ggbrRedScale.Position        := Round(FChannelMixer.RedScale      * 100) + MID_POS;
    ggbrGreenScale.Position      := Round(FChannelMixer.GreenScale    * 100) + MID_POS;
    ggbrBlueScale.Position       := Round(FChannelMixer.BlueScale     * 100) + MID_POS;
    ggbrConstantScale.Position   := Round(FChannelMixer.ConstantScale * 100) + MID_POS;
    cmbbxOutputChannel.ItemIndex := 0;
  end;
end; 

procedure TfrmChannelMixer.edtRedScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := 0;

  if FSliderIsChanging then
  begin
    Exit;
  end;

  FEditBoxIsChanging := True;
  try
    try
      LChangedValue         := StrToInt(edtRedScale.Text);
      LChangedValue         := Clamp(LChangedValue, MIN_VALUE, MAX_VALUE);
      ggbrRedScale.Position := MID_POS + LChangedValue;

      if Assigned(FChannelMixer) then
      begin
        FChannelMixer.RedScale := (ggbrRedScale.Position - MID_POS) / 100;
        ExecuteChannelMix;
      end;
    except
      if Assigned(FChannelMixer) then
      begin
        LChangedValue := Round(FChannelMixer.RedScale * 100);
      end;

      edtRedScale.Text := IntToStr(LChangedValue);
    end;
  finally
    FEditBoxIsChanging := False;
  end;
end; 

procedure TfrmChannelMixer.edtGreenScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := 0;

  if FSliderIsChanging then
  begin
    Exit;
  end;

  FEditBoxIsChanging := True;
  try
    try
      LChangedValue           := StrToInt(edtGreenScale.Text);
      LChangedValue           := Clamp(LChangedValue, MIN_VALUE, MAX_VALUE);
      ggbrGreenScale.Position := MID_POS + LChangedValue;

      if Assigned(FChannelMixer) then
      begin
        FChannelMixer.GreenScale := (ggbrGreenScale.Position - MID_POS) / 100;
        ExecuteChannelMix;
      end;
    except
      if Assigned(FChannelMixer) then
      begin
        LChangedValue := Round(FChannelMixer.GreenScale * 100);
      end;

      edtGreenScale.Text := IntToStr(LChangedValue);
    end;
  finally
    FEditBoxIsChanging := False;
  end;
end;

procedure TfrmChannelMixer.edtBlueScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := 0;

  if FSliderIsChanging then
  begin
    Exit;
  end;

  FEditBoxIsChanging := True;
  try
    try
      LChangedValue          := StrToInt(edtBlueScale.Text);
      LChangedValue          := Clamp(LChangedValue, MIN_VALUE, MAX_VALUE);
      ggbrBlueScale.Position := MID_POS + LChangedValue;

      if Assigned(FChannelMixer) then
      begin
        FChannelMixer.BlueScale := (ggbrBlueScale.Position - MID_POS) / 100;
        ExecuteChannelMix;
      end;
    except
      if Assigned(FChannelMixer) then
      begin
        LChangedValue := Round(FChannelMixer.BlueScale * 100);
      end;

      edtBlueScale.Text := IntToStr(LChangedValue);
    end;
  finally
    FEditBoxIsChanging := False;
  end;
end; 

procedure TfrmChannelMixer.edtConstantScaleChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := 0;

  if FSliderIsChanging then
    Exit;

  FEditBoxIsChanging := True;
  try
    try
      LChangedValue              := StrToInt(edtConstantScale.Text);
      LChangedValue              := Clamp(LChangedValue, MIN_VALUE, MAX_VALUE);
      ggbrConstantScale.Position := MID_POS + LChangedValue;

      if Assigned(FChannelMixer) then
      begin
        FChannelMixer.ConstantScale := (ggbrConstantScale.Position - MID_POS) / 100;
        ExecuteChannelMix;
      end;
    except
      if Assigned(FChannelMixer) then
      begin
        LChangedValue := Round(FChannelMixer.ConstantScale * 100);
      end;

      edtConstantScale.Text := IntToStr(LChangedValue);
    end;
  finally
    FEditBoxIsChanging := False;
  end;
end; 

procedure TfrmChannelMixer.edtRedScaleExit(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := ggbrRedScale.Position - MID_POS;

  if (LChangedValue) > 0 then
  begin
    edtRedScale.Text := '+' + IntToStr(LChangedValue);
  end
  else
  begin
    edtRedScale.Text := IntToStr(LChangedValue);
  end;
end; 

procedure TfrmChannelMixer.edtGreenScaleExit(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := ggbrGreenScale.Position - MID_POS;

  if (LChangedValue) > 0 then
  begin
    edtGreenScale.Text := '+' + IntToStr(LChangedValue);
  end
  else
  begin
    edtGreenScale.Text := IntToStr(LChangedValue);
  end;
end;

procedure TfrmChannelMixer.edtBlueScaleExit(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := ggbrBlueScale.Position - MID_POS;

  if (LChangedValue) > 0 then
  begin
    edtBlueScale.Text := '+' + IntToStr(LChangedValue);
  end
  else
  begin
    edtBlueScale.Text := IntToStr(LChangedValue);
  end;
end;

procedure TfrmChannelMixer.edtConstantScaleExit(Sender: TObject);
var
  LChangedValue : Integer;
begin
  LChangedValue := ggbrConstantScale.Position - MID_POS;

  if (LChangedValue) > 0 then
  begin
    edtConstantScale.Text := '+' + IntToStr(LChangedValue);
  end
  else
  begin
    edtConstantScale.Text := IntToStr(LChangedValue);
  end;
end; 

procedure TfrmChannelMixer.btbtnOKClick(Sender: TObject);
begin
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile(SECTION_CHANNEL_MIXER_DIALOG,
                       IDENT_CHANNEL_MIXER_PREVIEW,
                       IntToStr(Integer(chckbxPreview.Checked)));
  end;
end; 

procedure TfrmChannelMixer.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    ExecuteChannelMix;
  end;
end; 

end.
