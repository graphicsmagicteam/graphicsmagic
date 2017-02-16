unit BrightnessContrastDlg;

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
  gmBrightContrastLayer;

type
  TfrmBrightnessContrast = class(TForm)
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    grpbxBrightnessContrast: TGroupBox;
    lblBrightness: TLabel;
    edtBrightness: TEdit;
    lblContrast: TLabel;
    edtContrast: TEdit;
    ggbrBrightness: TGaugeBar;
    ggbrContrast: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AdjustBrightness(Sender: TObject);
    procedure AdjustContrast(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtBrightnessChange(Sender: TObject);
    procedure edtContrastChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrBrightnessMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrContrastMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FTempBitmap           : TBitmap32;
    FBrightnessAmount     : Integer;
    FContrastAmount       : Integer;
    FBarIsChanging        : Boolean;
    FEditIsChanging       : Boolean;
    FWorkingOnEffectLayer : Boolean;
    FBrightContrastLayer  : TgmBrightContrastLayer;  // pointer to a Brightness Contrast layer 

    procedure ExecuteBCOnSelection;
    procedure ExecuteBCOnAlphaChannel;
    procedure ExecuteBCOnQuickMask;
    procedure ExecuteBCOnLayerMask;
    procedure ExecuteBCOnLayer;
    procedure ExecuteBrightnessContrast;
  public
    procedure AssociateToBrightContrastLayer(ABrightContrastLayer: TgmBrightContrastLayer);
  end;

var
  frmBrightnessContrast: TfrmBrightnessContrast;

implementation

uses
{ GraphicsMagic Lib }
  gmChannelManager,
  gmImageProcessFuncs,
  gmIni,
  gmLayers,
  gmMath,              // EnsureIntegerInRange()
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

const
  MEDIAN_VALUE : Integer = 100;
  MIN_VALUE    : Integer = -100;
  MAX_VALUE    : Integer = 100;

//-- Custom procedures and functions -------------------------------------------

procedure TfrmBrightnessContrast.AssociateToBrightContrastLayer(
  ABrightContrastLayer: TgmBrightContrastLayer);
begin
  if Assigned(ABrightContrastLayer) then
  begin
    FBrightContrastLayer := ABrightContrastLayer;

    ggbrBrightness.Position := FBrightContrastLayer.BrightAmount + MEDIAN_VALUE;
    ggbrContrast.Position   := FBrightContrastLayer.ContrastAmount + MEDIAN_VALUE;

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

// process on selection
procedure TfrmBrightnessContrast.ExecuteBCOnSelection;
var
  LChannelSet : TgmChannelSet;
begin
  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // can not processing on special layers
      if not (LayerList.SelectedLayer is TgmNormalLayer) then
      begin
        Exit;
      end;
    end;

    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      LChannelSet := ChannelManager.SelectedColorChannels;
    end
    else
    begin
      LChannelSet := [csGrayscale];
    end;

    // change brightness first
    Brightness32(frmMain.FBitmapBefore, FTempBitmap, FBrightnessAmount, LChannelSet);

    // After brightness adjustment, change contrast.
    // Note that the parameter is FContrast * 3,
    // which is for getting the effect be more obvious.
    
    Contrast32(FTempBitmap, frmMain.FBitmapAfter, FContrastAmount * 3, LChannelSet);

    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end;
  end;
end;

procedure TfrmBrightnessContrast.ExecuteBCOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.SelectedAlphaChannel) then
    begin
      Brightness32(frmMain.FBitmapBefore, FTempBitmap, FBrightnessAmount, [csGrayscale]);
      Contrast32(FTempBitmap, frmMain.FBitmapAfter, FContrastAmount * 3, [csGrayscale]);

      if chckbxPreview.Checked then
      begin
        with ChannelManager.SelectedAlphaChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed;
        end;
      end;
    end;
  end;
end;

procedure TfrmBrightnessContrast.ExecuteBCOnQuickMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.QuickMaskChannel) then
    begin
      Brightness32(frmMain.FBitmapBefore, FTempBitmap, FBrightnessAmount, [csGrayscale]);
      Contrast32(FTempBitmap, frmMain.FBitmapAfter, FContrastAmount * 3, [csGrayscale]);

      if chckbxPreview.Checked then
      begin
        with ChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed;
        end;
      end;
    end;
  end;
end;

procedure TfrmBrightnessContrast.ExecuteBCOnLayerMask;
begin
  with ActiveChildForm do
  begin
    Brightness32(frmMain.FBitmapBefore, FTempBitmap, FBrightnessAmount, [csGrayscale]);
    Contrast32(FTempBitmap, frmMain.FBitmapAfter, FContrastAmount * 3, [csGrayscale]);

    if chckbxPreview.Checked then
    begin
      LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapAfter);

      // update the layer mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(0, 0,
          LayerList.SelectedLayer.MaskBitmap);
      end;

      LayerList.SelectedLayer.Changed;
    end;
  end;
end; 

procedure TfrmBrightnessContrast.ExecuteBCOnLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      Brightness32(frmMain.FBitmapBefore, FTempBitmap, FBrightnessAmount,
                   ChannelManager.SelectedColorChannels);

      Contrast32(FTempBitmap, frmMain.FBitmapAfter, FContrastAmount * 3,
                 ChannelManager.SelectedColorChannels);
    end;

    if chckbxPreview.Checked then
    begin
      if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
      begin
        FBrightContrastLayer.Changed;
      end
      else
      begin
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
          LayerList.SelectedLayer.Changed;
        end;
      end;
    end;
  end;
end;

procedure TfrmBrightnessContrast.ExecuteBrightnessContrast;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
    begin
      ExecuteBCOnLayer;
    end
    else
    begin
      if Assigned(Selection) then
      begin
        ExecuteBCOnSelection;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ExecuteBCOnAlphaChannel;
            end;

          ctQuickMaskChannel:
            begin
              ExecuteBCOnQuickMask;
            end;
            
          ctLayerMaskChannel:
            begin
              ExecuteBCOnLayerMask;
            end;

          ctColorChannel:
            begin
              ExecuteBCOnLayer;
            end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmBrightnessContrast.FormCreate(Sender: TObject);
begin
  FTempBitmap           := TBitmap32.Create;
  FTempBitmap.DrawMode  := dmBlend;
  FBrightnessAmount     := 0;
  FContrastAmount       := 0;
  FBarIsChanging        := False;
  FEditIsChanging       := False;
  FWorkingOnEffectLayer := False;
  FBrightContrastLayer  := nil;
end;

procedure TfrmBrightnessContrast.FormShow(Sender: TObject);
var
  LChangedValue : Integer;
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
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                frmMain.FBitmapBefore.Assign(
                  ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
              end;
            end;

          ctQuickMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(
                ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
              end;
            end;
        end;
      end;

      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);
      FTempBitmap.SetSizeFrom(frmMain.FBitmapBefore);
    end;

    ggbrBrightness.Position := MEDIAN_VALUE;
    ggbrContrast.Position   := MEDIAN_VALUE;
    chckbxPreview.Checked   := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_BRIGHT_CONTRAST_DIALOG, IDENT_BRIGHT_CONTRAST_PREVIEW, '1')));
  end;

  // display bar positions in edit boxes
  FBarIsChanging := True;
  try
    LChangedValue := ggbrBrightness.Position - MEDIAN_VALUE;

    if LChangedValue > 0 then
    begin
      edtBrightness.Text := '+' + IntToStr(LChangedValue);
    end
    else
    begin
      edtBrightness.Text := IntToStr(LChangedValue);
    end;

    LChangedValue := ggbrContrast.Position - MEDIAN_VALUE;

    if LChangedValue > 0 then
    begin
      edtContrast.Text := '+' + IntToStr(LChangedValue);
    end
    else
    begin
      edtContrast.Text := IntToStr(LChangedValue);
    end;

  finally
    FBarIsChanging := False;
  end; 

  ActiveControl := btbtnOK;
end;

procedure TfrmBrightnessContrast.AdjustBrightness(
  Sender: TObject);
var
  LChangedValue : Integer;
begin
  FBarIsChanging := True;
  try
    LChangedValue := ggbrBrightness.Position - MEDIAN_VALUE;

    if FEditIsChanging then
    begin
      edtBrightness.Text := IntToStr(LChangedValue);
    end
    else
    begin
      if LChangedValue > 0 then
      begin
        edtBrightness.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtBrightness.Text := IntToStr(LChangedValue);
      end;
    end;

    // if on brightness/contrast layer...
    if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
    begin
      FBrightContrastLayer.BrightAmount := LChangedValue;
    end
    else
    begin
      with ActiveChildForm do
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              if FBrightnessAmount <> LChangedValue then
              begin
                FBrightnessAmount := LChangedValue;
              end;
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                if FBrightnessAmount <> LChangedValue then
                begin
                  FBrightnessAmount := LChangedValue;
                end;
            end;
          end;
        end;
      end;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmBrightnessContrast.AdjustContrast(Sender: TObject);
var
  LChangedValue : Integer;
begin
  FBarIsChanging := True;
  try
    LChangedValue := ggbrContrast.Position - MEDIAN_VALUE;

    if FEditIsChanging then
    begin
      edtContrast.Text := IntToStr(LChangedValue);
    end
    else
    begin
      if LChangedValue > 0 then
      begin
        edtContrast.Text := '+' + IntToStr(LChangedValue);
      end
      else
      begin
        edtContrast.Text := IntToStr(LChangedValue);
      end;
    end;

    // if on brightness/contrast layer...
    if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
    begin
      FBrightContrastLayer.ContrastAmount := LChangedValue;
    end
    else
    begin
      with ActiveChildForm do
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              if FContrastAmount <> LChangedValue then
              begin
                FContrastAmount := LChangedValue;
              end;
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                if FContrastAmount <> LChangedValue then
                begin
                  FContrastAmount := LChangedValue;
                end;
              end;
            end;
        end;
      end;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmBrightnessContrast.FormDestroy(Sender: TObject);
begin
  FTempBitmap.Free;
end;

procedure TfrmBrightnessContrast.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteBrightnessContrast;
end;

procedure TfrmBrightnessContrast.edtBrightnessChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtBrightness.Text);
        EnsureValueInRange(LChangedValue, MIN_VALUE, MAX_VALUE);
        ggbrBrightness.Position := MEDIAN_VALUE + LChangedValue;
        ExecuteBrightnessContrast;
      except
        if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FBrightContrastLayer.BrightAmount;
        end
        else
        begin
          LRightValue := FBrightnessAmount;
        end;

        edtBrightness.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end;

procedure TfrmBrightnessContrast.edtContrastChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtContrast.Text);
        EnsureValueInRange(LChangedValue, MIN_VALUE, MAX_VALUE);
        ggbrContrast.Position := MEDIAN_VALUE + LChangedValue;
        ExecuteBrightnessContrast;
      except
        if Assigned(FBrightContrastLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FBrightContrastLayer.ContrastAmount;
        end
        else
        begin
          LRightValue := FContrastAmount;
        end;

        edtBrightness.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end;

procedure TfrmBrightnessContrast.btbtnOKClick(Sender: TObject);
begin
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile(SECTION_BRIGHT_CONTRAST_DIALOG,
                       IDENT_BRIGHT_CONTRAST_PREVIEW,
                       IntToStr(Integer(chckbxPreview.Checked)));
  end;
end;

procedure TfrmBrightnessContrast.ggbrBrightnessMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteBrightnessContrast;
end;

procedure TfrmBrightnessContrast.ggbrContrastMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteBrightnessContrast;
end;

end.
