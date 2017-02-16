unit ThresholdDlg;

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
  GR32_RangeBars,
{ GraphicsMagic Lib }
  gmThresholdLayer;

type
  TfrmThreshold = class(TForm)
    grpbxThreshold: TGroupBox;
    lblThresholdLevels: TLabel;
    edtLevel: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    imgThresholdHistogram: TImage;
    ggbrLevel: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtLevelChange(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ggbrLevelChange(Sender: TObject);
    procedure ggbrLevelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FLevel                : Integer;
    FBarIsChanging        : Boolean;
    FWorkingOnEffectLayer : Boolean;
    FThresholdLayer       : TgmThresholdLayer;  // pointer to a Threshold layer 

    procedure DrawHistogram;
    procedure ThresholdOnSelection;
    procedure ThresholdOnAlphaChannel;
    procedure ThresholdOnQuickMask;
    procedure ThresholdOnLayerMask;
    procedure ThresholdOnLayer;
    procedure ExecuteThreshold;
  public
    procedure AssociateToThresholdLayer(ALayer: TgmThresholdLayer);
  end;

var
  frmThreshold: TfrmThreshold;

implementation

uses
{ Graphics32 }
  GR32,
{ Externals }
  HistogramLibrary,         // THistogram
{ GraphicsMagic Lib }
  gmChannelManager,
  gmImageProcessFuncs,
  gmIni,
  gmLayers,
  gmMath,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

{ Custom Procedures And Functions }

procedure TfrmThreshold.AssociateToThresholdLayer(ALayer: TgmThresholdLayer);
begin
  if Assigned(ALayer) then
  begin
    FThresholdLayer := ALayer;

    ggbrLevel.Position := FThresholdLayer.Level;
    edtLevel.Text      := IntToStr(FThresholdLayer.Level);

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmThreshold.DrawHistogram;
const
  clSkyBlue = TColor($F0CAA6);   // RGB: 166 202 240
var
  LHistogram         : THistogram;
  LFlattenedBmp      : TBitmap32;
  LColorChannelCount : Integer;
begin
  LHistogram := THistogram.Create;
  try
    with ActiveChildForm do
    begin
      if Assigned(FThresholdLayer) and FWorkingOnEffectLayer then
      begin
        // We have to blend the layers that beneath this Threshold layer
        // into a bitmap to calculate the histogram.

        LFlattenedBmp := LayerList.GetLayerBlendResult(
          0, LayerList.SelectedIndex - 1, True);

        if Assigned(LFlattenedBmp) then
        begin
          GetHistogram(cpIntensity, LFlattenedBmp, LHistogram);
          LFlattenedBmp.Free;
        end;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              GetHistogram(cpValue, frmMain.FBitmapBefore, LHistogram);
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                LColorChannelCount := ChannelManager.ColorChannelList.SelectedChannelCount;

                if LColorChannelCount = 1 then
                begin
                  if csRed in ChannelManager.SelectedColorChannels then
                  begin
                    GetHistogram(cpRed, frmMain.FBitmapBefore, LHistogram);
                  end
                  else
                  if csGreen in ChannelManager.SelectedColorChannels then
                  begin
                    GetHistogram(cpGreen, frmMain.FBitmapBefore, LHistogram);
                  end
                  else
                  if csBlue in ChannelManager.SelectedColorChannels then
                  begin
                    GetHistogram(cpBlue, frmMain.FBitmapBefore, LHistogram);
                  end;
                end
                else
                if LColorChannelCount = 2 then
                begin
                  if (csRed   in ChannelManager.SelectedColorChannels) and
                     (csGreen in ChannelManager.SelectedColorChannels) then
                  begin
                    GetHistogram(cpYellow, frmMain.FBitmapBefore, LHistogram);
                  end
                  else
                  if (csRed  in ChannelManager.SelectedColorChannels) and
                     (csBlue in ChannelManager.SelectedColorChannels) then
                  begin
                    GetHistogram(cpMagenta, frmMain.FBitmapBefore, LHistogram);
                  end
                  else
                  if (csGreen in ChannelManager.SelectedColorChannels) and
                     (csBlue  in ChannelManager.SelectedColorChannels) then
                  begin
                    GetHistogram(cpCyan, frmMain.FBitmapBefore, LHistogram);
                  end;
                end
                else
                begin
                  GetHistogram(cpIntensity, frmMain.FBitmapBefore, LHistogram);
                end;
              end;
            end;
        end;
      end;
    end;

    imgThresholdHistogram.Canvas.Brush.Color := clSkyBlue;
    imgThresholdHistogram.Canvas.FillRect(imgThresholdHistogram.Canvas.ClipRect);
    LHistogram.Draw(imgThresholdHistogram.Canvas);
  finally
    LHistogram.Free;
  end;
end;

procedure TfrmThreshold.ThresholdOnSelection;
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

    ThresholdBitmap32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, FLevel, LChannelSet);

    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end;
  end;
end;

procedure TfrmThreshold.ThresholdOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.SelectedAlphaChannel) then
    begin
      ThresholdBitmap32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, FLevel, [csGrayscale]);

      if chckbxPreview.Checked then
      begin
        with ChannelManager.SelectedAlphaChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed();
        end;
      end;
    end;
  end;
end;

procedure TfrmThreshold.ThresholdOnQuickMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.QuickMaskChannel) then
    begin
      ThresholdBitmap32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, FLevel, [csGrayscale]);

      if chckbxPreview.Checked then
      begin
        with ChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed();
        end;
      end;
    end;
  end;
end;

procedure TfrmThreshold.ThresholdOnLayerMask;
begin
  with ActiveChildForm do
  begin
    ThresholdBitmap32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, FLevel, [csGrayscale]);

    if chckbxPreview.Checked then
    begin
      LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapAfter);

      // update the layer mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
          0, 0, LayerList.SelectedLayer.MaskBitmap);
      end;

      LayerList.SelectedLayer.Changed;
    end;
  end;
end;

procedure TfrmThreshold.ThresholdOnLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      ThresholdBitmap32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, FLevel,
                        ChannelManager.SelectedColorChannels);
    end;

    if chckbxPreview.Checked then
    begin
      if Assigned(FThresholdLayer) and FWorkingOnEffectLayer then
      begin
        FThresholdLayer.Changed;
      end
      else
      begin
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
          LayerList.SelectedLayer.Changed();
        end;
      end;
    end;
  end;
end; 

procedure TfrmThreshold.ExecuteThreshold;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if Assigned(FThresholdLayer) and FWorkingOnEffectLayer then
    begin
      ThresholdOnLayer;
    end
    else
    begin
      if Assigned(Selection) then
      begin
        ThresholdOnSelection;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ThresholdOnAlphaChannel;
            end;

          ctQuickMaskChannel:
            begin
              ThresholdOnQuickMask;
            end;

          ctLayerMaskChannel:
            begin
              ThresholdOnLayerMask;
            end;

          ctColorChannel:
            begin
              ThresholdOnLayer;
            end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmThreshold.FormCreate(Sender: TObject);
begin
  FLevel                := 127;
  FBarIsChanging        := False;
  FWorkingOnEffectLayer := False;
  FThresholdLayer       := nil;
end;

procedure TfrmThreshold.FormShow(Sender: TObject);
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

      chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_THRESHOLD_DIALOG, IDENT_THRESHOLD_PREVIEW, '1')));
      ggbrLevel.Position    := FLevel;

      ExecuteThreshold();
    end;
  end;

  DrawHistogram;
  ActiveControl := btbtnOK;
end;

procedure TfrmThreshold.ggbrLevelChange(Sender: TObject);
begin
  FBarIsChanging := True;
  try
    edtLevel.Text := IntToStr(ggbrLevel.Position);

    if Assigned(FThresholdLayer) and FWorkingOnEffectLayer then
    begin
      FThresholdLayer.Level := ggbrLevel.Position;
    end
    else
    begin
      FLevel := ggbrLevel.Position;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmThreshold.ggbrLevelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteThreshold;
end;

procedure TfrmThreshold.edtLevelChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Byte;
begin
  if not FBarIsChanging then
  begin
    try
      LChangedValue := StrToInt(edtLevel.Text);
      EnsureValueInRange(LChangedValue, 0, 255);
      ggbrLevel.Position := LChangedValue;
      ExecuteThreshold;
    except
      if Assigned(FThresholdLayer) and FWorkingOnEffectLayer then
      begin
        LRightValue := FThresholdLayer.Level;
      end
      else
      begin
        LRightValue := FLevel;
      end;

      edtLevel.Text := IntToStr(LRightValue);
    end;
  end;
end;

procedure TfrmThreshold.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteThreshold;
end;

procedure TfrmThreshold.btbtnOKClick(Sender: TObject);
begin
  // not on Threshold layer...
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile( SECTION_THRESHOLD_DIALOG, IDENT_THRESHOLD_PREVIEW,
                        IntToStr(Integer(chckbxPreview.Checked)) );
  end;
end;

end.
