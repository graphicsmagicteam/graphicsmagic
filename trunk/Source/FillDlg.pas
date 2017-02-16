unit FillDlg;

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

// Update Date: 2017/01/22

interface

uses
{ Standard Lib }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{ Graphics32 }
  GR32_Image,
{ GraphicsMagic Lib }
  gmFill;

type
  TfrmFill = class(TForm)
    grpbxFillContent: TGroupBox;
    grpbxFillBlend: TGroupBox;
    lblUseFillMode: TLabel;
    cmbbxFillOptions: TComboBox;
    lblCustomPattern: TLabel;
    pnlCustomPattern: TPanel;
    spdbtnSelectPattern: TSpeedButton;
    lblBlendMode: TLabel;
    cmbbxBlendMode: TComboBox;
    lblBlendOpacity: TLabel;
    edtBlendOpacity: TEdit;
    updwnBlendOpacity: TUpDown;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreserveTransparency: TCheckBox;
    Label1: TLabel;
    imgSelectedPattern: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbbxFillOptionsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbbxBlendModeChange(Sender: TObject);
    procedure edtBlendOpacityChange(Sender: TObject);
    procedure chckbxPreserveTransparencyClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure spdbtnSelectPatternClick(Sender: TObject);
  private
    FFillTool: TgmFill;

    function ExecuteFill: Boolean;
    function FillOnAlphaChannel: Boolean;
    function FillOnLayer:Boolean;
    function FillOnLayerMask: Boolean;
    function FillOnQuickMask: Boolean;
    function FillOnSelection: Boolean;

    procedure UpdateFillDlg;
  public
    { Public declarations }
  end;

var
  frmFill: TfrmFill;

implementation

uses
{ Graphics32 }
  GR32,
{ Externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmImageProcessFuncs,
  gmLayers,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PatternsPopFrm;    // frmPatterns

{$R *.DFM}

procedure TfrmFill.UpdateFillDlg;
begin
  lblCustomPattern.Enabled    := FFillTool.FillOptions in [gmfoPattern];
  imgSelectedPattern.Enabled  := lblCustomPattern.Enabled;
  pnlCustomPattern.Enabled    := lblCustomPattern.Enabled;
  spdbtnSelectPattern.Enabled := lblCustomPattern.Enabled;
  edtBlendOpacity.Text        := IntToStr(FFillTool.Opacity);
end;

function TfrmFill.FillOnSelection: Boolean;
var
  LCuttedBmp : TBitmap32;
  LCutRect   : TRect;
begin
  Result := False;

  with ActiveChildForm do
  begin
    if FFillTool.FillOptions = gmfoHistory then
    begin
      if ChannelManager.CurrentChannelType in
           [ctAlphaChannel, ctQuickMaskChannel, ctLayerMaskChannel] then
      begin
        MessageDlg('Could not use the history fill because the' + #10#13 +
                   'history state does not contain a corresponding' + #10#13 +
                   'layer.', mtError, [mbOK], 0);

        Exit;
      end
      else
      begin
        // must be on a layer...
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          // cut pixels out from history bitmap

          LCuttedBmp := TBitmap32.Create();
          try
            LCuttedBmp.DrawMode := dmBlend;

            LCuttedBmp.Width  := Abs(Selection.MaskBorderEnd.X - Selection.MaskBorderStart.X) + 1;
            LCuttedBmp.Height := Abs(Selection.MaskBorderEnd.Y - Selection.MaskBorderStart.Y) + 1;

            LCutRect := Rect(Selection.MaskBorderStart.X,
                             Selection.MaskBorderStart.Y,
                             Selection.MaskBorderEnd.X + 1,
                             Selection.MaskBorderEnd.Y + 1);

            CopyRect32WithARGB(LCuttedBmp, HistoryBitmap, LCutRect, clWhite32);
            FFillTool.SetForegroundWithHistory(LCuttedBmp);

            // make the original cropped area of the selection be same as
            // the forground of the selection
            Selection.CutOriginal.Assign(Selection.Foreground);
          finally
            LCuttedBmp.Free();
          end;
        end;
      end;
    end
    else if FFillTool.FillOptions = gmfoPattern then
    begin
      FFillTool.SetForegroundWithPattern(Selection.Foreground.Width,
                                         Selection.Foreground.Height,
                                         frmPatterns.FillingPattern);

      // make the original cropped area of the selection be same as
      // the forground of the selection
      Selection.CutOriginal.Assign(Selection.Foreground);
    end;

    // filling
    FFillTool.DoFill32(Selection.CutOriginal);
    ShowProcessedSelection();
  end;

  Result := True;
end;

function TfrmFill.FillOnAlphaChannel: Boolean;
begin
  Result := False;

  with ActiveChildForm do
  begin
    if not Assigned(ChannelManager.SelectedAlphaChannel) then
    begin
      Exit;
    end;

    if FFillTool.FillOptions = gmfoHistory then
    begin
      MessageDlg('Could not use the history fill because the' + #10#13 +
                 'history state does not contain a corresponding' + #10#13 +
                 'layer.', mtError, [mbOK], 0);

      Exit;
    end
    else if FFillTool.FillOptions = gmfoPattern then
    begin
      FFillTool.SetForegroundWithPattern(
        ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Width,
        ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Height,
        frmPatterns.FillingPattern);
    end;

    // filling
    FFillTool.DoFill32(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
    ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
  end;
  
  Result := True;
end;

function TfrmFill.FillOnQuickMask: Boolean;
begin
  Result := False;

  with ActiveChildForm do
  begin
    if not Assigned(ChannelManager.QuickMaskChannel) then
    begin
      Exit;
    end;

    if FFillTool.FillOptions = gmfoHistory then
    begin
      MessageDlg('Could not use the history fill because the' + #10#13 +
                 'history state does not contain a corresponding' + #10#13 +
                 'layer.', mtError, [mbOK], 0);

      Exit;
    end
    else if FFillTool.FillOptions = gmfoPattern then
    begin
      FFillTool.SetForegroundWithPattern(
        ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Width,
        ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Height,
        frmPatterns.FillingPattern);
    end;

    // filling
    FFillTool.DoFill32(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
    ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap.Changed();
  end;

  Result := True;
end;

function TfrmFill.FillOnLayerMask: Boolean;
begin
  Result := False;

  with ActiveChildForm do
  begin
    if FFillTool.FillOptions = gmfoHistory then
    begin
      MessageDlg('Could not use the history fill because the' + #10#13 +
                 'history state does not contain a corresponding' + #10#13 +
                 'layer.', mtError, [mbOK], 0);

      Exit;
    end
    else if FFillTool.FillOptions = gmfoPattern then
    begin
      FFillTool.SetForegroundWithPattern(
        LayerList.SelectedLayer.MaskBitmap.Width,
        LayerList.SelectedLayer.MaskBitmap.Height,
        frmPatterns.FillingPattern);
    end;

    // filling
    FFillTool.DoFill32(LayerList.SelectedLayer.MaskBitmap);

    if Assigned(ChannelManager.LayerMaskChannel) then
    begin
      ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
        0, 0, LayerList.SelectedLayer.MaskBitmap);
    end;

    LayerList.SelectedLayer.Changed();
  end;

  Result := True;
end; 

function TfrmFill.FillOnLayer: Boolean;
begin
  Result := False;

  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      if FFillTool.FillOptions = gmfoHistory then
      begin
        if (HistoryBitmap.Width  <> LayerList.SelectedLayer.LayerBitmap.Width) or
           (HistoryBitmap.Height <> LayerList.SelectedLayer.LayerBitmap.Height) then
        begin
          MessageDlg('Could not use the history fill, because the current' + #10#13 +
                     'canvas size does not match that of the history state!',
                     mtError, [mbOK], 0);

          Exit;
        end;

        FFillTool.SetForegroundWithHistory(HistoryBitmap);
      end
      else if FFillTool.FillOptions = gmfoPattern then
      begin
        FFillTool.SetForegroundWithPattern(
          LayerList.SelectedLayer.LayerBitmap.Width,
          LayerList.SelectedLayer.LayerBitmap.Height,
          frmPatterns.FillingPattern);
      end;

      // filling
      FFillTool.DoFill32(LayerList.SelectedLayer.LayerBitmap);

      LayerList.SelectedLayer.Changed();

      Result := True;
    end;
  end;
end; 

function TfrmFill.ExecuteFill: Boolean;
begin
  Result := False;

  if Assigned(ActiveChildForm.Selection) then
  begin
    Result := FillOnSelection();
  end
  else
  begin
    case ActiveChildForm.ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          Result := FillOnAlphaChannel();
        end;

      ctQuickMaskChannel:
        begin
          Result := FillOnQuickMask();
        end;

      ctLayerMaskChannel:
        begin
          Result := FillOnLayerMask();
        end;

      ctColorChannel:
        begin
          Result := FillOnLayer();
        end;
    end;
  end;
end;

procedure TfrmFill.FormCreate(Sender: TObject);
begin
  FFillTool                  := TgmFill.Create();
  FFillTool.FillOptions      := gmfoForeground;
  
  cmbbxFillOptions.ItemIndex := 0;
  cmbbxBlendMode.Items       := BlendModeList();
  cmbbxBlendMode.ItemIndex   := 0;
  updwnBlendOpacity.Position := 100;
end;

procedure TfrmFill.FormDestroy(Sender: TObject);
begin
  FFillTool.Free();
end; 

procedure TfrmFill.cmbbxFillOptionsChange(Sender: TObject);
begin
  case cmbbxFillOptions.ItemIndex of
    0:
      begin // foreground color filling
        FFillTool.FillMode := gmfmColorFill;

        if ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          FFillTool.FillColor := Color32(frmMain.GlobalForeColor);
        end
        else
        begin
          FFillTool.FillColor := Color32(frmMain.ForeGrayColor);
        end;
      end;

    1:
      begin // background color filling
        FFillTool.FillMode := gmfmColorFill;

        if ActiveChildForm.ChannelManager.CurrentChannelType = ctColorChannel then
        begin
          FFillTool.FillColor := Color32(frmMain.GlobalBackColor);
        end
        else
        begin
          FFillTool.FillColor := Color32(frmMain.BackGrayColor);
        end;
      end;

    2,
    3:
      begin
        FFillTool.FillMode := gmfmBitmapFill;  // pattern/history filling
      end;

    4:
      begin // black filling
        FFillTool.FillMode  := gmfmColorFill;
        FFillTool.FillColor := clBlack32;
      end;

    5:
      begin // 50% gray filling
        FFillTool.FillMode  := gmfmColorFill;
        FFillTool.FillColor := clGray32;
      end;

    6:
      begin // white filling
        FFillTool.FillMode  := gmfmColorFill;
        FFillTool.FillColor := clWhite32;
      end; 
  end;

  FFillTool.FillOptions := TgmFillOptions(cmbbxFillOptions.ItemIndex);

  UpdateFillDlg();
end;

procedure TfrmFill.FormShow(Sender: TObject);
begin
  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      FFillTool.SelectedChannelSet := ChannelManager.SelectedColorChannels;
    end
    else
    begin
      FFillTool.SelectedChannelSet := [csGrayscale];
    end;
    
    cmbbxFillOptionsChange(Sender);
  end;
end;

procedure TfrmFill.cmbbxBlendModeChange(Sender: TObject);
begin
  FFillTool.BlendMode := TBlendMode32(cmbbxBlendMode.ItemIndex);
end;

procedure TfrmFill.edtBlendOpacityChange(Sender: TObject);
begin
  try
    updwnBlendOpacity.Position := StrToInt(edtBlendOpacity.Text);
    FFillTool.Opacity          := updwnBlendOpacity.Position;
  except
    edtBlendOpacity.Text := IntToStr(updwnBlendOpacity.Position);
  end;
end; 

procedure TfrmFill.chckbxPreserveTransparencyClick(Sender: TObject);
begin
  FFillTool.IsPreserveTransparency := chckbxPreserveTransparency.Checked;
end;

procedure TfrmFill.btbtnOKClick(Sender: TObject);
begin
  if not ExecuteFill() then
  begin
    ModalResult := mrNone;
    Exit;
  end;
end;

procedure TfrmFill.spdbtnSelectPatternClick(Sender: TObject);
var
  LShowingPoint : TPoint;
begin
  GetCursorPos(LShowingPoint);
  
  frmPatterns.Left            := LShowingPoint.X;
  frmPatterns.Top             := LShowingPoint.Y;
  frmPatterns.PatternListUser := pluFill;
  frmPatterns.Show();
end; 

end.
