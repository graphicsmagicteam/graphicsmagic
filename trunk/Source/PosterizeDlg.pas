unit PosterizeDlg;

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
  StdCtrls, Buttons, 
{ Graphics32 }
  GR32_RangeBars,
{ GraphicsMagic Lib }
  gmPosterizeLayer;

type
  TfrmPosterize = class(TForm)
    lblLevel: TLabel;
    edtLevel: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    ggbrLevel: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtLevelChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure ggbrLevelChange(Sender: TObject);
    procedure ggbrLevelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FLevel                : Byte;
    FBarIsChanging        : Boolean;
    FWorkingOnEffectLayer : Boolean;
    FPosterizeLayer       : TgmPosterizeLayer;  // pointer to a Posterize layer

    procedure PosterizeOnSelection;
    procedure PosterizeOnAlphaChannel;
    procedure PosterizeOnQuickMask;
    procedure PosterizeOnLayerMask;
    procedure PosterizeOnLayer;
    procedure ExecutePosterize;
  public
    procedure AssociateToPosterizeLayer(ALayer: TgmPosterizeLayer);
  end;

var
  frmPosterize: TfrmPosterize;

implementation

uses
{ Graphics32 }
  GR32,
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

{ Custom procedures and functions }

procedure TfrmPosterize.AssociateToPosterizeLayer(ALayer: TgmPosterizeLayer);
begin
  if Assigned(ALayer) then
  begin
    FPosterizeLayer := ALayer;

    ggbrLevel.Position := FPosterizeLayer.Level;
    edtLevel.Text      := IntToStr(FPosterizeLayer.Level);

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmPosterize.PosterizeOnSelection;
var
  LChannelSet : TgmChannelSet;
begin
  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // don't process on special layers
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

    Posterize32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, 255 - FLevel, LChannelSet);

    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end;
  end;
end;

procedure TfrmPosterize.PosterizeOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.SelectedAlphaChannel) then
    begin
      Posterize32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, 255 - FLevel, [csGrayscale]);

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

procedure TfrmPosterize.PosterizeOnQuickMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.QuickMaskChannel) then
    begin
      Posterize32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, 255 - FLevel, [csGrayscale]);

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

procedure TfrmPosterize.PosterizeOnLayerMask;
begin
  with ActiveChildForm do
  begin
    Posterize32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, 255 - FLevel, [csGrayscale]);
             
    if chckbxPreview.Checked then
    begin
      LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapAfter);

      // update the layer mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
          0, 0, LayerList.SelectedLayer.MaskBitmap);
      end;

      LayerList.SelectedLayer.Changed();
    end;
  end;
end;

procedure TfrmPosterize.PosterizeOnLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      Posterize32(frmMain.FBitmapBefore, frmMain.FBitmapAfter, 255 - FLevel,
                  ChannelManager.SelectedColorChannels);
    end;

    if chckbxPreview.Checked then
    begin
      if Assigned(FPosterizeLayer) and FWorkingOnEffectLayer then
      begin
        FPosterizeLayer.Changed;
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

procedure TfrmPosterize.ExecutePosterize;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if Assigned(FPosterizeLayer) and FWorkingOnEffectLayer then
    begin
      PosterizeOnLayer;
    end
    else
    begin
      if Assigned(Selection) then
      begin
        PosterizeOnSelection;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              PosterizeOnAlphaChannel;
            end;

          ctQuickMaskChannel:
            begin
              PosterizeOnQuickMask;
            end;

          ctLayerMaskChannel:
            begin
              PosterizeOnLayerMask;
            end;

          ctColorChannel:
            begin
              PosterizeOnLayer;
            end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmPosterize.FormCreate(Sender: TObject);
begin
  FLevel                := StrToInt(ReadInfoFromIniFile(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_LEVELS, '2'));
  FBarIsChanging        := False;
  FWorkingOnEffectLayer := False;
  FPosterizeLayer       := nil;
end;

procedure TfrmPosterize.FormShow(Sender: TObject);
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

      chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_PREVIEW, '1')));
      ggbrLevel.Position    := FLevel;

      ExecutePosterize();
    end;
  end;

  ActiveControl := btbtnOK;
end;

procedure TfrmPosterize.ggbrLevelChange(Sender: TObject);
begin
  FBarIsChanging := True;
  try
    edtLevel.Text := IntToStr(ggbrLevel.Position);

    if Assigned(FPosterizeLayer) and FWorkingOnEffectLayer then
    begin
      FPosterizeLayer.Level := ggbrLevel.Position;
    end
    else
    begin
      FLevel := ggbrLevel.Position;
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmPosterize.ggbrLevelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecutePosterize;
end;

procedure TfrmPosterize.edtLevelChange(Sender: TObject);
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
      ExecutePosterize;
    except
      // on Posterize layer
      if Assigned(FPosterizeLayer) and FWorkingOnEffectLayer then
      begin
        LRightValue := FPosterizeLayer.Level;
      end
      else
      begin
        LRightValue := FLevel;
      end;

      edtLevel.Text := IntToStr(LRightValue);
    end;
  end;
end;

procedure TfrmPosterize.chckbxPreviewClick(Sender: TObject);
begin
  ExecutePosterize;
end;

procedure TfrmPosterize.btbtnOKClick(Sender: TObject);
begin
  // not on Threshold layer...
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_LEVELS, IntToStr(FLevel));
    WriteInfoToIniFile(SECTION_POSTERIZE_DIALOG, IDENT_POSTERIZE_PREVIEW, IntToStr(Integer(chckbxPreview.Checked)));
  end;
end;

end.
