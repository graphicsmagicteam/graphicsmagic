unit ApplyImageDlg;

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
  Dialogs, StdCtrls, Buttons,
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmApplyImage,
  gmLayers,
{ GraphicsMagic Forms/Dialogs }
  ChildForm;

type
  TgmChannelOptionsStatus = (cosIncludingRGB, cosExcludingRGB);

  TfrmApplyImage = class(TForm)
    grpbxSource: TGroupBox;
    lblSourceImage: TLabel;
    cmbbxSourceImage: TComboBox;
    lblSourceLayer: TLabel;
    cmbbxSourceLayer: TComboBox;
    lblSourceChannel: TLabel;
    cmbbxSourceChannel: TComboBox;
    chckbxSourceChannelInvert: TCheckBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    lblTarget: TLabel;
    lblTargetString: TLabel;
    grpbxBlending: TGroupBox;
    lblBlendMode: TLabel;
    cmbbxBlendMode: TComboBox;
    lblBlendOpacity: TLabel;
    edtBlendOpacity: TEdit;
    lblOpacityPercent: TLabel;
    chckbxPreserveTransparency: TCheckBox;
    chckbxEnableMask: TCheckBox;
    grpbxMask: TGroupBox;
    lblMaskImage: TLabel;
    cmbbxMaskImage: TComboBox;
    lblMaskLayer: TLabel;
    cmbbxMaskLayer: TComboBox;
    lblMaskChannel: TLabel;
    cmbbxMaskChannel: TComboBox;
    chckbxMaskChannelInvert: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbbxSourceImageChange(Sender: TObject);
    procedure cmbbxSourceLayerChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbbxSourceChannelChange(Sender: TObject);
    procedure chckbxSourceChannelInvertClick(Sender: TObject);
    procedure cmbbxBlendModeChange(Sender: TObject);
    procedure edtBlendOpacityChange(Sender: TObject);
    procedure chckbxPreserveTransparencyClick(Sender: TObject);
    procedure chckbxEnableMaskClick(Sender: TObject);
    procedure cmbbxMaskImageChange(Sender: TObject);
    procedure cmbbxMaskLayerChange(Sender: TObject);
    procedure cmbbxMaskChannelChange(Sender: TObject);
    procedure chckbxMaskChannelInvertClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
  private
    { Private declarations }
    FApplyImage                   : TgmApplyImage;
    FEnableChanging               : Boolean;

    FSourceChildForm              : TfrmChild;
    FSourceLayer                  : TgmCustomLayer;  // pointer to selected source layer 
    FSourceBitmap                 : TBitmap32;
    FSourceChannelOptionsStatus   : TgmChannelOptionsStatus;
    FSourceAlphaChannelStartIndex : Integer;         // the first index of alpha channel item in cmbbxSourceChannel.Items[]

    FMaskChildForm                : TfrmChild;
    FMaskLayer                    : TgmCustomLayer;  // pointer to selected mask layer
    FMaskBitmap                   : TBitmap32;
    FMaskChannelOptionsStatus     : TgmChannelOptionsStatus;
    FMaskAlphaChannelStartIndex   : Integer;         // the first index of alpha channel item in cmbbxMaskChannel.Items[]

    function GetTargetString: string;
    function GetLayerNames(AChildForm: TfrmChild): TStringList;
    function GetSourceFileNameIndex(const AFileName: string): Integer;
    function GetSourceChannelNames: TStringList;
    function GetMaskFileNameIndex(const AFileName: string): Integer;
    function GetMaskChannelNames: TStringList;

    // setting source bitmap
    procedure SetSourceBitmapBySourceLayerSettings;
    procedure SetSourceBitmapBySourceChannelSettings;

    // setting mask bitmap
    procedure SetMaskBitmapByMaskLayerSettings;
    procedure SetMaskBitmapByMaskChannelSettings;

    procedure ApplyImageOnSelection;
    procedure ApplyImageOnAlphaChannel;
    procedure ApplyImageOnLayerMask;
    procedure ApplyImageOnLayer;
    procedure ExecuteApplyImage;
  public
    { Public declarations }
  end;

var
  frmApplyImage: TfrmApplyImage;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmAlphaFuncs,
  gmChannels,
  gmChannelManager,
  gmImageProcessFuncs,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.dfm}

function TfrmApplyImage.GetTargetString: string;
var
  LTargetFileName : string;
begin
  Result := '';

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      LTargetFileName := ExtractFileName(FileName);
      Result          := Result + LTargetFileName + ' ( ';

      case ChannelManager.CurrentChannelType of
        ctColorChannel:
          begin
            if ChannelManager.ColorChannelList.SelectedChannelCount > 2 then
            begin
              Result := Result + LayerList.SelectedLayer.LayerName + ', RGB';
            end
            else if ChannelManager.ColorChannelList.SelectedChannelCount = 1 then
            begin
              if csRed in ChannelManager.SelectedColorChannels then
              begin
                Result := Result + LayerList.SelectedLayer.LayerName + ', Red';
              end
              else if csGreen in ChannelManager.SelectedColorChannels then
              begin
                Result := Result + LayerList.SelectedLayer.LayerName + ', Green';
              end
              else if csBlue in ChannelManager.SelectedColorChannels then
              begin
                Result := Result + LayerList.SelectedLayer.LayerName + ', Blue';
              end;
            end;
          end;

        ctLayerMaskChannel:
          begin
            Result := Result + LayerList.SelectedLayer.LayerName + ', Layer Mask';
          end;
          
        ctAlphaChannel:
          begin
            Result := Result + ChannelManager.SelectedAlphaChannel.ChannelName;
          end;
      end;
    end;
    
    Result := Result + ' )';
  end;
end;

function TfrmApplyImage.GetLayerNames(AChildForm: TfrmChild): TStringList;
var
  i          : Integer;
  LTempLayer : TgmCustomLayer;
begin
  Result := nil;

  if Assigned(AChildForm) then
  begin
    Result := TStringList.Create();

    if AChildForm.LayerList.Count > 1 then
    begin
      Result.Add('Merged');
    end;

    for i := (AChildForm.LayerList.Count - 1) downto 0 do
    begin
      LTempLayer := AChildForm.LayerList.Layers[i];

      if not (LTempLayer is TgmNormalLayer) then
      begin
        if not LTempLayer.IsMaskEnabled then
        begin
          Continue;
        end;
      end;

      Result.Add(LTempLayer.LayerName);
    end;
  end;
end;

function TfrmApplyImage.GetSourceFileNameIndex(const AFileName: string): Integer;
var
  i : Integer;
begin
  Result := -1;

  if cmbbxSourceImage.Items.Count > 0 then
  begin
    for i := 0 to (cmbbxSourceImage.Items.Count - 1) do
    begin
      if AFileName = cmbbxSourceImage.Items[i] then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TfrmApplyImage.GetSourceChannelNames: TStringList;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  Result := TStringList.Create();

  case FSourceChannelOptionsStatus of
    cosIncludingRGB:
      begin
        Result.Add('RGB');
        Result.Add('Red');
        Result.Add('Green');
        Result.Add('Blue');
        Result.Add('Transparency');

        if Assigned(FSourceLayer) then
        begin
          if FSourceLayer.IsMaskEnabled then
          begin
            Result.Add('Layer Mask');
          end;
        end;
      end;

    cosExcludingRGB:
      begin
        Result.Add('Layer Mask');
      end;
  end;

  if Assigned(FSourceChildForm) then
  begin
    FSourceAlphaChannelStartIndex := -1;
    
    if FSourceChildForm.ChannelManager.AlphaChannelList.Count > 0 then
    begin
      FSourceAlphaChannelStartIndex := Result.Count;

      for i := 0 to (FSourceChildForm.ChannelManager.AlphaChannelList.Count - 1) do
      begin
        LChannel := FSourceChildForm.ChannelManager.AlphaChannelList.Channels[i];
        Result.Add(LChannel.ChannelName);
      end;
    end;

    // selection channel
    if Assigned(FSourceChildForm.Selection) then
    begin
      Result.Add('Selection');
    end;
  end;
end;

function TfrmApplyImage.GetMaskFileNameIndex(const AFileName: string): Integer;
var
  i : Integer;
begin
  Result := -1;

  if cmbbxMaskImage.Items.Count > 0 then
  begin
    for i := 0 to (cmbbxMaskImage.Items.Count - 1) do
    begin
      if AFileName = cmbbxMaskImage.Items[i] then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TfrmApplyImage.GetMaskChannelNames: TStringList;
var
  i        : Integer;
  LChannel : TgmCustomChannel;
begin
  Result := TStringList.Create();

  case FMaskChannelOptionsStatus of
    cosIncludingRGB:
      begin
        Result.Add('Gray');
        Result.Add('Red');
        Result.Add('Green');
        Result.Add('Blue');
        Result.Add('Transparency');

        if Assigned(FMaskLayer) then
        begin
          if FMaskLayer.IsMaskEnabled then
          begin
            Result.Add('Layer Mask');
          end;
        end;
      end;

    cosExcludingRGB:
      begin
        Result.Add('Layer Mask');
      end;
  end;

  if Assigned(FMaskChildForm) then
  begin
    FMaskAlphaChannelStartIndex := -1;
    
    if FMaskChildForm.ChannelManager.AlphaChannelList.Count > 0 then
    begin
      FMaskAlphaChannelStartIndex := Result.Count;

      for i := 0 to (FMaskChildForm.ChannelManager.AlphaChannelList.Count - 1) do
      begin
        LChannel := FMaskChildForm.ChannelManager.AlphaChannelList.Channels[i];
        Result.Add(LChannel.ChannelName);
      end;
    end;

    // selection channel
    if Assigned(FMaskChildForm.Selection) then
    begin
      Result.Add('Selection');
    end;
  end;
end;

// setting source bitmap
procedure TfrmApplyImage.SetSourceBitmapBySourceLayerSettings;
var
  LLayerIndex : Integer;
begin
  if Assigned(FSourceChildForm) then
  begin
    if cmbbxSourceLayer.ItemIndex >= 0 then
    begin
      if (FSourceChildForm.LayerList.Count > 1) and
         (cmbbxSourceLayer.ItemIndex = 0) then  // merged image
      begin
        FSourceBitmap.Assign(FSourceChildForm.LayerList.CombineResult);
        FSourceLayer := nil;  // not pointer to any layer
      end
      else
      begin
        LLayerIndex  := cmbbxSourceLayer.Items.Count - cmbbxSourceLayer.ItemIndex - 1;
        FSourceLayer := FSourceChildForm.LayerList.Layers[LLayerIndex];

        if Assigned(FSourceLayer) then
        begin
          if FSourceLayer is TgmNormalLayer then
          begin
            FSourceBitmap.Assign(FSourceLayer.LayerBitmap);
          end
          else
          begin
            FSourceBitmap.Assign(FSourceLayer.MaskBitmap);
          end
        end;
      end;
    end;
  end;
end;

// setting source bitmap
procedure TfrmApplyImage.SetSourceBitmapBySourceChannelSettings;
var
  LAlphaChannel : TgmAlphaChannel;
  LChannelIndex : Integer;
begin
  if Assigned(FApplyImage) then
  begin
    // if the selection channel is chosen
    if Assigned(FSourceChildForm.Selection) and
       (cmbbxSourceChannel.ItemIndex = (cmbbxSourceChannel.Items.Count - 1)) then
    begin
      FApplyImage.SourceChannel := gmscsAlphaChannel;

      FSourceBitmap.SetSize(FSourceChildForm.Selection.SourceBitmap.Width,
                            FSourceChildForm.Selection.SourceBitmap.Height);

      FSourceBitmap.Clear($FF000000);

      FSourceBitmap.Draw(FSourceChildForm.Selection.MaskBorderStart.X,
                         FSourceChildForm.Selection.MaskBorderStart.Y,
                         FSourceChildForm.Selection.ResizedMask);

      Exit;
    end;

    case FSourceChannelOptionsStatus of
      cosIncludingRGB:
        begin
          if (cmbbxSourceChannel.ItemIndex >= 0) and
             (cmbbxSourceChannel.ItemIndex < 5) then
          begin
            FApplyImage.SourceChannel := TgmSourceChannelSelector(cmbbxSourceChannel.ItemIndex);

            SetSourceBitmapBySourceLayerSettings();
          end
          else
          begin
            FApplyImage.SourceChannel := gmscsAlphaChannel;

            if Assigned(FSourceLayer) then
            begin
              // this layer has a mask, see if we select it as source bitmap
              if FSourceLayer.IsMaskEnabled then
              begin
                if cmbbxSourceChannel.ItemIndex = 5 then // layer mask
                begin
                  FSourceBitmap.Assign(FSourceLayer.MaskBitmap);
                end
                else
                begin
                  if Assigned(FSourceChildForm) then
                  begin
                    if FSourceAlphaChannelStartIndex >= 0 then
                    begin
                      LChannelIndex := cmbbxSourceChannel.ItemIndex - FSourceAlphaChannelStartIndex;
                      LAlphaChannel := TgmAlphaChannel(FSourceChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                      if Assigned(LAlphaChannel) then
                      begin
                        FSourceBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                      end;
                    end;
                  end;
                end;
              end
              else
              begin
                if Assigned(FSourceChildForm) then
                begin
                  if FSourceAlphaChannelStartIndex >= 0 then
                  begin
                    LChannelIndex := cmbbxSourceChannel.ItemIndex - FSourceAlphaChannelStartIndex;
                    LAlphaChannel := TgmAlphaChannel(FSourceChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                    if Assigned(LAlphaChannel) then
                    begin
                      FSourceBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                    end;
                  end;
                end;
              end;
            end
            else  // if the FSourceLayerPanel = nil
            begin
              if Assigned(FSourceChildForm) then
              begin
                if FSourceAlphaChannelStartIndex >= 0 then
                begin
                  LChannelIndex := cmbbxSourceChannel.ItemIndex - FSourceAlphaChannelStartIndex;
                  LAlphaChannel := TgmAlphaChannel(FSourceChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                  if Assigned(LAlphaChannel) then
                  begin
                    FSourceBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                  end;
                end;
              end;
            end;
          end;
        end;

      cosExcludingRGB:
        begin
          FApplyImage.SourceChannel := gmscsAlphaChannel;

          if cmbbxSourceChannel.ItemIndex = 0 then
          begin
            SetSourceBitmapBySourceLayerSettings();
          end
          else
          begin
            if Assigned(FSourceChildForm) then
            begin
              if FSourceAlphaChannelStartIndex >= 0 then
              begin
                LChannelIndex := cmbbxSourceChannel.ItemIndex - FSourceAlphaChannelStartIndex;
                LAlphaChannel := TgmAlphaChannel(FSourceChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                if Assigned(LAlphaChannel) then
                begin
                  FSourceBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                end;
              end;
            end;
          end;
        end;
    end;
  end;
end;

// setting mask bitmap
procedure TfrmApplyImage.SetMaskBitmapByMaskLayerSettings;
var
  LLayerIndex : Integer;
begin
  if Assigned(FMaskChildForm) then
  begin
    if cmbbxMaskLayer.ItemIndex >= 0 then
    begin
      if (FMaskChildForm.LayerList.Count > 1) and
         (cmbbxMaskLayer.ItemIndex = 0) then  // merged image
      begin
        FMaskBitmap.Assign(FMaskChildForm.LayerList.CombineResult);
        FMaskLayer := nil; // not pointer to any layer 
      end
      else
      begin
        LLayerIndex := cmbbxMaskLayer.Items.Count - cmbbxMaskLayer.ItemIndex - 1;
        FMaskLayer  := FMaskChildForm.LayerList.Layers[LLayerIndex];

        if Assigned(FMaskLayer) then
        begin
          if FMaskLayer is TgmNormalLayer then
          begin
            FMaskBitmap.Assign(FMaskLayer.LayerBitmap);
          end
          else
          begin
            FMaskBitmap.Assign(FMaskLayer.MaskBitmap);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmApplyImage.SetMaskBitmapByMaskChannelSettings;
var
  LAlphaChannel : TgmAlphaChannel;
  LChannelIndex : Integer;
begin
  if Assigned(FApplyImage) then
  begin
    // if the selection channel is chosen
    if Assigned(FMaskChildForm.Selection) and
       (cmbbxMaskChannel.ItemIndex = (cmbbxMaskChannel.Items.Count - 1)) then
    begin
      FApplyImage.MaskChannel := gmscsAlphaChannel;

      FMaskBitmap.SetSize(FMaskChildForm.Selection.SourceBitmap.Width,
                          FMaskChildForm.Selection.SourceBitmap.Height);

      FMaskBitmap.Clear($FF000000);
      
      FMaskBitmap.Draw(FMaskChildForm.Selection.MaskBorderStart.X,
                       FMaskChildForm.Selection.MaskBorderStart.Y,
                       FMaskChildForm.Selection.ResizedMask);
                         
      Exit;
    end;

    case FMaskChannelOptionsStatus of
      cosIncludingRGB:
        begin
          if (cmbbxMaskChannel.ItemIndex >= 0) and
             (cmbbxMaskChannel.ItemIndex < 5) then
          begin
            FApplyImage.MaskChannel := TgmSourceChannelSelector(cmbbxMaskChannel.ItemIndex);

            SetMaskBitmapByMaskLayerSettings();
          end
          else
          begin
            FApplyImage.MaskChannel := gmscsAlphaChannel;

            if Assigned(FMaskLayer) then
            begin
              // this layer has a mask, see if we select it as mask bitmap
              if FMaskLayer.IsMaskEnabled then
              begin
                if cmbbxMaskChannel.ItemIndex = 5 then  // layer mask
                begin
                  FMaskBitmap.Assign(FMaskLayer.MaskBitmap);
                end
                else
                begin
                  if Assigned(FMaskChildForm) then
                  begin
                    if FMaskAlphaChannelStartIndex >= 0 then
                    begin
                      LChannelIndex := cmbbxMaskChannel.ItemIndex - FMaskAlphaChannelStartIndex;
                      LAlphaChannel := TgmAlphaChannel(FMaskChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                      if Assigned(LAlphaChannel) then
                      begin
                        FMaskBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                      end;
                    end;
                  end;
                end;
              end
              else
              begin
                if Assigned(FMaskChildForm) then
                begin
                  if FMaskAlphaChannelStartIndex >= 0 then
                  begin
                    LChannelIndex := cmbbxMaskChannel.ItemIndex - FMaskAlphaChannelStartIndex;
                    LAlphaChannel := TgmAlphaChannel(FMaskChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                    if Assigned(LAlphaChannel) then
                    begin
                      FMaskBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                    end;
                  end;
                end;
              end;
            end
            else  // if the FMaskLayerPanel = nil
            begin
              if Assigned(FMaskChildForm) then
              begin
                if FMaskAlphaChannelStartIndex >= 0 then
                begin
                  LChannelIndex := cmbbxMaskChannel.ItemIndex - FMaskAlphaChannelStartIndex;
                  LAlphaChannel := TgmAlphaChannel(FMaskChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                  if Assigned(LAlphaChannel) then
                  begin
                    FMaskBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                  end;
                end;
              end;
            end;
          end;
        end;

      cosExcludingRGB:
        begin
          FApplyImage.MaskChannel := gmscsAlphaChannel;

          if cmbbxMaskChannel.ItemIndex = 0 then
          begin
            SetMaskBitmapByMaskLayerSettings();
          end
          else
          begin
            if Assigned(FMaskChildForm) then
            begin
              if FMaskAlphaChannelStartIndex >= 0 then
              begin
                LChannelIndex := cmbbxMaskChannel.ItemIndex - FMaskAlphaChannelStartIndex;
                LAlphaChannel := TgmAlphaChannel(FMaskChildForm.ChannelManager.AlphaChannelList.Channels[LChannelIndex]);

                if Assigned(LAlphaChannel) then
                begin
                  FMaskBitmap.Assign(LAlphaChannel.ChannelLayer.Bitmap);
                end;
              end;
            end;
          end;
        end;
    end;
  end;
end;

// process on selection
procedure TfrmApplyImage.ApplyImageOnSelection;
var
  LChannelSet    : TgmChannelSet;
  LRect          : TRect;
  LTempSourceBmp : TBitmap32;
  LTempMaskBmp   : TBitmap32;
begin
  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // cannot process on special layers
      if not (LayerList.SelectedLayer is TgmNormalLayer) then
      begin
        Exit;
      end;

      LChannelSet := ChannelManager.SelectedColorChannels;
    end
    else
    begin
      LChannelSet := [csGrayscale];
    end;

    // retore the destination to original state
    frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

    LTempSourceBmp := TBitmap32.Create();
    try
      LRect.Left   := Selection.MaskBorderStart.X;
      LRect.Top    := Selection.MaskBorderStart.Y;
      LRect.Right  := Selection.MaskBorderEnd.X + 1;
      LRect.Bottom := Selection.MaskBorderEnd.Y + 1;

      LTempSourceBmp.DrawMode    := dmBlend;
      LTempSourceBmp.MasterAlpha := FSourceBitmap.MasterAlpha;
      CopyRect32WithARGB(LTempSourceBmp, FSourceBitmap, LRect, $00FFFFFF);

      if FApplyImage.SourceChannel in [gmscsTransparency, gmscsAlphaChannel] then
      begin
        ReplaceAlphaChannelWithNewValue(LTempSourceBmp, 255);
      end;
      
      // blending
      if chckbxEnableMask.Checked then
      begin
        LTempMaskBmp := TBitmap32.Create();
        try
          CopyRect32WithARGB(LTempMaskBmp, FMaskBitmap, LRect, $FF000000);

          FApplyImage.Execute(LTempSourceBmp, frmMain.FBitmapAfter, LTempMaskBmp,
                              LChannelSet);
        finally
          LTempMaskBmp.Free();
        end;
      end
      else
      begin
        FApplyImage.Execute(LTempSourceBmp, frmMain.FBitmapAfter, LChannelSet);
      end;
      
    finally
      LTempSourceBmp.Free();
    end;

    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end
    else
    begin
      imgWorkArea.Update();
      Selection.DrawMarchingAnts();
    end;
  end;
end;

// process on alpha channel
procedure TfrmApplyImage.ApplyImageOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(ChannelManager.SelectedAlphaChannel) then
    begin
      // retore the destination to original state
      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

      // blending
      if chckbxEnableMask.Checked then
      begin
        FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter, FMaskBitmap,
                            [csGrayscale]);
      end
      else
      begin
        FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter, [csGrayscale]);
      end;

      if chckbxPreview.Checked then
      begin
        ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
        ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
      end;
    end;
  end;
end;

procedure TfrmApplyImage.ApplyImageOnLayerMask;
begin
  // retore the destination to original state
  frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

  with ActiveChildForm do
  begin
    // blending
    if chckbxEnableMask.Checked then
    begin
      FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter, FMaskBitmap,
                          [csGrayscale]);
    end
    else
    begin
      FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter, [csGrayscale]);
    end;

    if chckbxPreview.Checked then
    begin
      LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapAfter);

      // update the layer mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(0, 0,
          LayerList.SelectedLayer.MaskBitmap);
      end;

      LayerList.SelectedLayer.Changed();
    end;
  end;
end; 

// process on layer
procedure TfrmApplyImage.ApplyImageOnLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      // retore the destination to original state
      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

      // blending
      if chckbxEnableMask.Checked then
      begin
        FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter, FMaskBitmap,
                            ChannelManager.SelectedColorChannels);
      end
      else
      begin
        FApplyImage.Execute(FSourceBitmap, frmMain.FBitmapAfter,
                            ChannelManager.SelectedColorChannels);
      end;


      if chckbxPreview.Checked then
      begin
        LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
        LayerList.SelectedLayer.Changed();
      end;
    end;
  end;
end; 

procedure TfrmApplyImage.ExecuteApplyImage;
begin
  Screen.Cursor := crHourGlass;
  try
    if Assigned(ActiveChildForm) then
    begin
      if Assigned(ActiveChildForm.Selection) then
      begin
        ApplyImageOnSelection();
      end
      else
      begin
        case ActiveChildForm.ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ApplyImageOnAlphaChannel();
            end;
            
          ctLayerMaskChannel:
            begin
              ApplyImageOnLayerMask();
            end;
            
          ctColorChannel:
            begin
              ApplyImageOnLayer();
            end;
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmApplyImage.FormCreate(Sender: TObject);
begin
  FEnableChanging := True;
  FApplyImage     := TgmApplyImage.Create();

  FSourceChildForm              := nil;
  FSourceLayer                  := nil;
  FSourceBitmap                 := TBitmap32.Create();
  FSourceChannelOptionsStatus   := cosIncludingRGB;
  FSourceAlphaChannelStartIndex := -1;

  FMaskChildForm              := nil;
  FMaskLayer                  := nil;
  FMaskBitmap                 := TBitmap32.Create();
  FMaskChannelOptionsStatus   := cosIncludingRGB;
  FMaskAlphaChannelStartIndex := -1;
end;

procedure TfrmApplyImage.FormDestroy(Sender: TObject);
begin
  FSourceChildForm  := nil;
  FSourceLayer      := nil;
  FMaskChildForm    := nil;
  FMaskLayer        := nil;

  FSourceBitmap.Free();
  FMaskBitmap.Free();
  FApplyImage.Free();
end;

procedure TfrmApplyImage.cmbbxSourceImageChange(Sender: TObject);
begin
  if FEnableChanging then
  begin
    FSourceChildForm := frmMain.GetChildFormPointerByFileName(
      cmbbxSourceImage.Items[cmbbxSourceImage.ItemIndex]);

    // restore the channel selection to default -- RGB channel
    FSourceChannelOptionsStatus := cosIncludingRGB;
    cmbbxSourceChannel.Items    := GetSourceChannelNames;
    FEnableChanging := False; // avoid execute the OnChange event of this combo box
    try
      if cmbbxSourceChannel.ItemIndex <> 0 then
      begin
        cmbbxSourceChannel.ItemIndex := 0;
        FApplyImage.SourceChannel    := TgmSourceChannelSelector(0);
      end;
    finally
      FEnableChanging := True;
    end;

    // restore the layer selection to default -- merged layer
    cmbbxSourceLayer.Items     := GetLayerNames(FSourceChildForm);
    cmbbxSourceLayer.ItemIndex := 0;
    cmbbxSourceLayerChange(Sender);
  end;
end;

procedure TfrmApplyImage.cmbbxSourceLayerChange(Sender: TObject);
begin
  if FEnableChanging then
  begin
    if Assigned(FSourceChildForm) then
    begin
      // Because of the selected source maybe just the target, and the target
      // maybe changed due to preview the process result on target, so we need
      // to restore the target before make the target as the source. 
      with ActiveChildForm do
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(frmMain.FBitmapBefore);
          ShowProcessedSelection(False);
        end
        else
        begin
          case ChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapBefore);
              end;
              
            ctLayerMaskChannel:
              begin
                LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapBefore);
              end;

            ctColorChannel:
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapBefore);
              end;
          end;
        end;
      end;

      if cmbbxSourceLayer.ItemIndex >= 0 then
      begin
        // get the proper source bitmap
        SetSourceBitmapBySourceLayerSettings();

        if (FSourceChildForm.LayerList.Count > 1) and
           (cmbbxSourceLayer.ItemIndex = 0) then // merged image
        begin
          FSourceChannelOptionsStatus := cosIncludingRGB;
          FApplyImage.SourceChannel   := gmscsRGB;
        end
        else
        begin
          if Assigned(FSourceLayer) then
          begin
            if FSourceLayer is TgmNormalLayer then
            begin
              FSourceChannelOptionsStatus := cosIncludingRGB;
              FApplyImage.SourceChannel   := gmscsRGB;
            end
            else
            begin
              FSourceChannelOptionsStatus := cosExcludingRGB;
              FApplyImage.SourceChannel   := gmscsAlphaChannel;
            end;
          end;
        end;

        cmbbxSourceChannel.Items.Clear();
        cmbbxSourceChannel.Items     := GetSourceChannelNames;
        cmbbxSourceChannel.ItemIndex := 0;

        ExecuteApplyImage();
      end;
    end;
  end;
end;

procedure TfrmApplyImage.FormShow(Sender: TObject);
var
  LLayerIndex : Integer;
begin
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        // make the CutOriginal same as the foreground of the selection
        Selection.ConfirmForeground();
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
    end;

    // initialize components
    lblTargetString.Caption := GetTargetString();

    // initialize settings
    FEnableChanging := False; // suppress OnChange events occur temporarily
    try
      // source image
      cmbbxSourceImage.Items     := frmMain.GetFileNamesWithSameSizeImage();
      cmbbxSourceImage.ItemIndex := GetSourceFileNameIndex( ExtractFileName(ActiveChildForm.FileName) );
      FSourceChildForm           := frmMain.GetChildFormPointerByFileName(cmbbxSourceImage.Items[cmbbxSourceImage.ItemIndex]);

      // source layer
      cmbbxSourceLayer.Items     := GetLayerNames(FSourceChildForm);
      cmbbxSourceLayer.ItemIndex := 0;

      if FSourceChildForm.LayerList.Count > 1 then // merged image
      begin
        FSourceLayer := nil; // not pointer to any layer 
        FSourceBitmap.Assign(FSourceChildForm.LayerList.CombineResult);
      end
      else
      begin
        LLayerIndex  := cmbbxSourceLayer.Items.Count - cmbbxSourceLayer.ItemIndex - 1;
        FSourceLayer := FSourceChildForm.LayerList.Layers[LLayerIndex];

        if Assigned(FSourceLayer) then
        begin
          if FSourceLayer is TgmNormalLayer then
          begin
            FSourceChannelOptionsStatus := cosIncludingRGB;
            FApplyImage.SourceChannel   := gmscsRGB;
            FSourceBitmap.Assign(FSourceLayer.LayerBitmap);
          end
          else
          begin
            FSourceChannelOptionsStatus := cosExcludingRGB;
            FApplyImage.SourceChannel   := gmscsAlphaChannel;
            FSourceBitmap.Assign(FSourceLayer.MaskBitmap);
          end;
        end;
      end;

      // source channel
      cmbbxSourceChannel.Items     := GetSourceChannelNames();
      cmbbxSourceChannel.ItemIndex := 0;

      // blend mode
      cmbbxBlendMode.Items       := BlendModeList();
      cmbbxBlendMode.ItemIndex   := 1;
      FApplyImage.BlendModeIndex := 1;

      // mask image
      cmbbxMaskImage.Items     := frmMain.GetFileNamesWithSameSizeImage();
      cmbbxMaskImage.ItemIndex := GetMaskFileNameIndex( ExtractFileName(ActiveChildForm.FileName) );
      FMaskChildForm           := frmMain.GetChildFormPointerByFileName(cmbbxMaskImage.Items[cmbbxMaskImage.ItemIndex]);

      // mask layer
      cmbbxMaskLayer.Items     := GetLayerNames(FMaskChildForm);
      cmbbxMaskLayer.ItemIndex := 0;

      if FMaskChildForm.LayerList.Count > 1 then // merged image
      begin
        FMaskLayer := nil; // not pointer to any layer 
        FMaskBitmap.Assign(FMaskChildForm.LayerList.CombineResult);
      end
      else
      begin
        LLayerIndex := cmbbxMaskLayer.Items.Count - cmbbxMaskLayer.ItemIndex - 1;
        FMaskLayer  := FMaskChildForm.LayerList.Layers[LLayerIndex];

        if Assigned(FMaskLayer) then
        begin
          if FMaskLayer is TgmNormalLayer then
          begin
            FMaskChannelOptionsStatus := cosIncludingRGB;
            FApplyImage.MaskChannel   := gmscsRGB;
            FMaskBitmap.Assign(FMaskLayer.LayerBitmap);
          end
          else
          begin
            FMaskChannelOptionsStatus := cosExcludingRGB;
            FApplyImage.MaskChannel   := gmscsAlphaChannel;
            FMaskBitmap.Assign(FMaskLayer.MaskBitmap);
          end;
        end;
      end;

      // mask channel
      cmbbxMaskChannel.Items     := GetMaskChannelNames();
      cmbbxMaskChannel.ItemIndex := 0;
    finally
      FEnableChanging := True;
    end;

    ExecuteApplyImage();
  end;

  ActiveControl := btbtnOK;
end;

procedure TfrmApplyImage.cmbbxSourceChannelChange(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    // Because of the selected source maybe just the target, and the target
    // maybe changed due to preview the process result on target, so we need
    // to restore the target before make the target as the source. 
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(frmMain.FBitmapBefore);
        ShowProcessedSelection(False);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapBefore);
            end;
            
          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapBefore);
            end;

          ctColorChannel:
            begin
              LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapBefore);
            end;
        end;
      end;
    end;

    // get the proper source bitmap
    SetSourceBitmapBySourceChannelSettings();
    ExecuteApplyImage();
  end;
end;

procedure TfrmApplyImage.chckbxSourceChannelInvertClick(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    FApplyImage.IsSourceChannelInverse := chckbxSourceChannelInvert.Checked;
    ExecuteApplyImage();
  end;
end;

procedure TfrmApplyImage.cmbbxBlendModeChange(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    FApplyImage.BlendModeIndex := cmbbxBlendMode.ItemIndex;
    ExecuteApplyImage();
  end;
end; 

procedure TfrmApplyImage.edtBlendOpacityChange(Sender: TObject);
var
  LChangedValue : Integer;
begin
  if Assigned(FApplyImage) then
  begin
    try
      LChangedValue := StrToInt(edtBlendOpacity.Text);

      if (LChangedValue < 0) or (LChangedValue > 100) then
      begin
        LChangedValue := Clamp(LChangedValue, 0, 100);
        edtBlendOpacity.Text := IntToStr(LChangedValue);
      end;
      
      FApplyImage.BlendOpacity := 255 * LChangedValue div 100;
      ExecuteApplyImage();
    except
      edtBlendOpacity.Text := IntToStr(Round(FApplyImage.BlendOpacity / 255 * 100));
    end;
  end;
end;

procedure TfrmApplyImage.chckbxPreserveTransparencyClick(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    FApplyImage.IsPreserveTransparency := chckbxPreserveTransparency.Checked;
    ExecuteApplyImage();
  end;  
end;

procedure TfrmApplyImage.chckbxEnableMaskClick(Sender: TObject);
begin
  lblMaskImage.Enabled   := chckbxEnableMask.Checked;
  lblMaskLayer.Enabled   := chckbxEnableMask.Checked;
  lblMaskChannel.Enabled := chckbxEnableMask.Checked;

  cmbbxMaskImage.Enabled   := chckbxEnableMask.Checked;
  cmbbxMaskLayer.Enabled   := chckbxEnableMask.Checked;
  cmbbxMaskChannel.Enabled := chckbxEnableMask.Checked;

  chckbxMaskChannelInvert.Enabled := chckbxEnableMask.Checked;
  ExecuteApplyImage();
end;

procedure TfrmApplyImage.cmbbxMaskImageChange(Sender: TObject);
begin
  if FEnableChanging then
  begin
    FMaskChildForm := frmMain.GetChildFormPointerByFileName(
      cmbbxMaskImage.Items[cmbbxMaskImage.ItemIndex]);

    // restore the channel selection to default -- Gray channel
    FMaskChannelOptionsStatus := cosIncludingRGB;
    cmbbxMaskChannel.Items    := GetMaskChannelNames();
    FEnableChanging := False; // avoid execute the OnChange event of this combo box
    try
      if cmbbxMaskChannel.ItemIndex <> 0 then
      begin
        cmbbxMaskChannel.ItemIndex := 0;
        FApplyImage.MaskChannel    := TgmSourceChannelSelector(0);
      end;
    finally
      FEnableChanging := True;
    end;

    // restore the layer selection to default -- merged layer
    cmbbxMaskLayer.Items     := GetLayerNames(FMaskChildForm);
    cmbbxMaskLayer.ItemIndex := 0;
    cmbbxMaskLayerChange(Sender);
  end;
end;

procedure TfrmApplyImage.cmbbxMaskLayerChange(Sender: TObject);
begin
  if FEnableChanging then
  begin
    if Assigned(FMaskChildForm) then
    begin
      // Because of the selected source maybe just the target, and the target
      // maybe changed due to preview the process result on target, so we need
      // to restore the target before make the target as the source. 
      with ActiveChildForm do
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(frmMain.FBitmapBefore);
          ShowProcessedSelection(False);
        end
        else
        begin
          case ChannelManager.CurrentChannelType of
            ctAlphaChannel:
              begin
                ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapBefore);
              end;
              
            ctLayerMaskChannel:
              begin
                LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapBefore);
              end;

            ctColorChannel:
              begin
                LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapBefore);
              end;
          end;
        end;
      end;

      if cmbbxMaskLayer.ItemIndex >= 0 then
      begin
        // get the proper mask bitmap
        SetMaskBitmapByMaskLayerSettings();

        if (FMaskChildForm.LayerList.Count > 1) and
           (cmbbxMaskLayer.ItemIndex = 0) then  // merged image
        begin
          FMaskChannelOptionsStatus := cosIncludingRGB;
          FApplyImage.MaskChannel   := gmscsRGB;
        end
        else
        begin
          if Assigned(FMaskLayer) then
          begin
            if FMaskLayer is TgmNormalLayer then
            begin
              FMaskChannelOptionsStatus := cosIncludingRGB;
              FApplyImage.MaskChannel   := gmscsRGB;
            end
            else
            begin
              FMaskChannelOptionsStatus := cosExcludingRGB;
              FApplyImage.MaskChannel   := gmscsAlphaChannel;
            end;
          end;
        end;
        
        cmbbxMaskChannel.Items.Clear;
        cmbbxMaskChannel.Items     := GetMaskChannelNames();
        cmbbxMaskChannel.ItemIndex := 0;

        ExecuteApplyImage();
      end;
    end;
  end;
end;

procedure TfrmApplyImage.cmbbxMaskChannelChange(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    // Because of the selected source maybe just the target, and the target
    // maybe changed due to preview the process result on target, so we need
    // to restore the target before make the target as the source. 
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(frmMain.FBitmapBefore);
        ShowProcessedSelection(False);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapBefore);
            end;
            
          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapBefore);
            end;

          ctColorChannel:
            begin
              LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapBefore);
            end;
        end;
      end;
    end;

    // get the proper mask bitmap
    SetMaskBitmapByMaskChannelSettings();
    ExecuteApplyImage();
  end;
end;

procedure TfrmApplyImage.chckbxMaskChannelInvertClick(Sender: TObject);
begin
  if Assigned(FApplyImage) then
  begin
    FApplyImage.IsMaskChannelInverse := chckbxMaskChannelInvert.Checked;
    ExecuteApplyImage();
  end; 
end;

procedure TfrmApplyImage.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    ExecuteApplyImage();
  end
  else
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        Selection.CutOriginal.Assign(frmMain.FBitmapBefore);
        ShowProcessedSelection();
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Assign(frmMain.FBitmapBefore);
              ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap.Changed();
            end;

          ctLayerMaskChannel:
            begin
              LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapBefore);

              // update the layer mask channel
              if Assigned(ChannelManager.LayerMaskChannel) then
              begin
                ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
                  0, 0, LayerList.SelectedLayer.MaskBitmap);
              end;

              LayerList.SelectedLayer.Changed();
            end;

          ctColorChannel:
            begin
              LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapBefore);
              LayerList.SelectedLayer.Changed();
            end;
        end;
      end;
    end;
  end;
end; 

end.
