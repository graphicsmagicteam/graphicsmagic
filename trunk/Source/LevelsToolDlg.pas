unit LevelsToolDlg;

{ This library created in 07/09/2006.
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on the the Gimp 2.2.10 .
  The original source can be found at www.gimp.org.

  Thanks to the authors of GIMP for giving us the opportunity to know how to
  achieve Levels Tool.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }

// Update Date: 2016/04/20

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ImgList, ComCtrls, ToolWin, 
{ Graphics32 Lib }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmGimpColorBar,
  gmGimpHistogram,
  gmLevelsLayer,
  gmLevelsTool;

type
  TfrmLevelsTool = class(TForm)
    GroupBox1: TGroupBox;
    lblChannel: TLabel;
    cmbbxChannel: TComboBox;
    btnResetChannel: TButton;
    spdbtnLinearHistogram: TSpeedButton;
    spdbtnLogarithmicHistogram: TSpeedButton;
    lblInputLevels: TLabel;
    pnlHistogramHolder: TPanel;
    imgHistogram: TImage32;
    ImageList1: TImageList;
    pnlInputArea: TPanel;
    imgInputBar: TImage32;
    edtLevelsLowInput: TEdit;
    updwnLevelsLowInput: TUpDown;
    edtLevelsGamma: TEdit;
    updwnLevelsGamma: TUpDown;
    edtLevelsHighInput: TEdit;
    updwnLevelsHighInput: TUpDown;
    lblOutputLevels: TLabel;
    pnlOutputArea: TPanel;
    imgOutputBar: TImage32;
    edtLevelsLowOutput: TEdit;
    updwnLevelsLowOutput: TUpDown;
    edtLevelsHighOutput: TEdit;
    updwnLevelsHighOutput: TUpDown;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    btnResetLevelsTool: TButton;
    btnAutoAdjustment: TButton;
    chckbxPreview: TCheckBox;
    ToolBar1: TToolBar;
    tlbtnBlackPicker: TToolButton;
    tlbtnGrayPicker: TToolButton;
    tlbtnWhitePicker: TToolButton;
    btnLoadLevels: TButton;
    btnSaveLevels: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure imgInputBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgInputBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgInputBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure updwnLevelsGammaClick(Sender: TObject; Button: TUDBtnType);
    procedure updwnLevelsGammaMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgOutputBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgOutputBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgOutputBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure edtLevelsLowInputChange(Sender: TObject);
    procedure edtLevelsHighInputChange(Sender: TObject);
    procedure edtLevelsLowOutputChange(Sender: TObject);
    procedure edtLevelsHighOutputChange(Sender: TObject);
    procedure ChangeLevelsChannel(Sender: TObject);
    procedure ChangeHistogramType(Sender: TObject);
    procedure ResetChannel(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure ResetLevelsTool(Sender: TObject);
    procedure updwnLevelsLowInputMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure updwnLevelsHighInputMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure updwnLevelsLowOutputMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure updwnLevelsHighOutputMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnAutoAdjustmentClick(Sender: TObject);
    procedure ChangeActivePicker(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure btnLoadLevelsClick(Sender: TObject);
    procedure btnSaveLevelsClick(Sender: TObject);
  private
    FColorBar             : TgmGimpColorBar;
    FHistogram            : TgmGimpHistogram;
    FLevelsLayer          : TgmLevelsLayer;
    FLevelsTool           : TgmLevelsTool;       // Pointer to an existing Levels tool object
    FNormalLevelsTool     : TgmLevelsTool;       // Created for normal layer...

    FInputColorBarBmp     : TBitmap32;
    FOutputColorBarBmp    : TBitmap32;
    FCanChange            : Boolean;
    FDrawing              : Boolean;
    FLevelsFileName       : string;              // opened levels file name
    FWorkingOnEffectLayer : Boolean;

    procedure ChangeChannelItems; // change the items in the Channel combobox
    procedure ExecuteLevelsOnSelection;
    procedure ExecuteLevelsOnAlphaChannel;
    procedure ExecuteLevelsOnQuickMask;
    procedure ExecuteLevelsOnLayerMask;
    procedure ExecuteLevelsOnLayer;
    procedure UpdateChannelComboBoxHint;
    procedure UpdateLevelsInput(AX: Integer);
    procedure UpdateLevelsOutput(AX: Integer);
  public
    procedure AssociateToLevelsLayer(ALayer: TgmLevelsLayer);
    procedure LevelsUpdate(const AUpdateItems: Integer);
    procedure ExecuteGimpLevels;

    property LevelsTool : TgmLevelsTool read FLevelsTool;
  end;

var
  frmLevelsTool: TfrmLevelsTool;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmGimpBaseEnums,
  gmGimpCommonFuncs,
  gmIni,
  gmLayers,
  gmLevels,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  ImageColorPickerForm,
  MainForm;

{$R *.dfm}

const
  UP_DOWN_GAMMA_MAX  = 100;
  UP_DOWN_GAMMA_MIN  = 1;
  UP_DOWN_GAMMA_INIT = 10;


procedure TfrmLevelsTool.AssociateToLevelsLayer(ALayer: TgmLevelsLayer);
var
  LFlattenedBmp : TBitmap32;
begin
  if Assigned(ALayer) then
  begin
    FLevelsLayer := ALayer;

    // pointer to the Levels tool which for Levels layer
    FLevelsTool := FLevelsLayer.LevelsTool;

    FHistogram.Scale := FLevelsLayer.HistogramScale;
    LevelsUpdate(ALL);

    // We have to blend the layers that beneath this Levels layer
    // into a bitmap to calculate the histogram.

    LFlattenedBmp := ActiveChildForm.LayerList.GetLayerBlendResult(
      0, ActiveChildForm.LayerList.SelectedIndex - 1, True);

    if Assigned(LFlattenedBmp) then
    begin
      FHistogram.gimp_histogram_calculate(LFlattenedBmp);
      LFlattenedBmp.Free;
    end;

    spdbtnLinearHistogram.Down      := (FLevelsLayer.HistogramScale = GIMP_HISTOGRAM_SCALE_LINEAR);
    spdbtnLogarithmicHistogram.Down := (FLevelsLayer.HistogramScale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmLevelsTool.UpdateLevelsInput(AX: Integer);
var
  LDelta, LMid, LTmp : Double;
  LWidth             : Integer;
begin
  if FLevelsTool <> nil then
  begin
    LWidth := imgInputBar.Width - 2 * BORDER;

    if LWidth < 1 then
    begin
      Exit;
    end;

    case FLevelsTool.ActiveSlider of
      0:  // low input
        begin
          LTmp := (AX - BORDER) / LWidth * 255.0;
          FLevelsTool.LevelsLowInput := CLAMP(LTmp, 0, FLevelsTool.LevelsHighInput);
        end;

      1:  // gamma
        begin
          LDelta := (FLevelsTool.SliderPos[2] - FLevelsTool.SliderPos[0]) / 2.0;
          LMid   := FLevelsTool.SliderPos[0] + LDelta;

          AX   := CLAMP(AX, FLevelsTool.SliderPos[0], FLevelsTool.SliderPos[2]);
          LTmp := (AX - LMid) / LDelta;

          FLevelsTool.LevelsGamma := 1.0 / Power(10, LTmp);
          //  round the gamma value to the nearest 1/100th  */
          FLevelsTool.LevelsGamma := RoundTo(FLevelsTool.LevelsGamma, -2);
        end;

      2:  // high input
        begin
          LTmp := (AX - BORDER) / LWidth * 255.0;
          FLevelsTool.LevelsHighInput := CLAMP(LTmp, FLevelsTool.LevelsLowInput, 255);
        end;
    end;

    LevelsUpdate(INPUT_SLIDERS or INPUT_LEVELS);
  end;
end;

procedure TfrmLevelsTool.UpdateLevelsOutput(AX: Integer);
var
  LTmp   : Double;
  LWidth : Integer;
begin
  if FLevelsTool <> nil then
  begin
    LWidth := imgOutputBar.Width - 2 * BORDER;

    if LWidth < 1 then
    begin
      Exit;
    end;

    case FLevelsTool.ActiveSlider of
      3:  // low output
        begin
          LTmp := (AX - BORDER) / LWidth * 255.0;
          FLevelsTool.LevelsLowOutput := CLAMP(LTmp, 0, 255);
        end;

      4:  // high output
        begin
          LTmp := (AX - BORDER) / LWidth * 255.0;
          FLevelsTool.LevelsHighOutput := CLAMP(LTmp, 0, 255);
        end;
    end;

    LevelsUpdate(OUTPUT_SLIDERS);
  end;
end; 

procedure TfrmLevelsTool.ExecuteLevelsOnSelection;
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

    if Assigned(FLevelsTool) then
    begin
      FLevelsTool.Map(frmMain.FBitmapAfter, LChannelSet);
    end;

    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end;
  end;
end; 

procedure TfrmLevelsTool.ExecuteLevelsOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(FLevelsTool) then
    begin
      FLevelsTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
    end;
    
    if chckbxPreview.Checked then
    begin
      if Assigned(ChannelManager.SelectedAlphaChannel) then
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

procedure TfrmLevelsTool.ExecuteLevelsOnQuickMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(FLevelsTool) then
    begin
      FLevelsTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
    end;
    
    if chckbxPreview.Checked then
    begin
      if Assigned(ChannelManager.QuickMaskChannel) then
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

procedure TfrmLevelsTool.ExecuteLevelsOnLayerMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(FLevelsTool) then
    begin
      FLevelsTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
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

      LayerList.SelectedLayer.Changed;
    end;
  end;
end; 

procedure TfrmLevelsTool.ExecuteLevelsOnLayer;
begin
  with ActiveChildForm do
  begin
    if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
    begin
      FLevelsLayer.LevelsTool.LUTSetup(3);
    end
    else
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
      begin
        FLevelsTool.Map(frmMain.FBitmapAfter,
                        ChannelManager.SelectedColorChannels);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
      begin
        FLevelsLayer.Changed;
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

procedure TfrmLevelsTool.ExecuteGimpLevels;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
    begin
      ExecuteLevelsOnLayer;
    end
    else
    begin
      if Assigned(Selection) then
      begin
        ExecuteLevelsOnSelection;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ExecuteLevelsOnAlphaChannel;
            end;

          ctQuickMaskChannel:
            begin
              ExecuteLevelsOnQuickMask;
            end;

          ctLayerMaskChannel:
            begin
              ExecuteLevelsOnLayerMask;
            end;

          ctColorChannel:
            begin
              ExecuteLevelsOnLayer;
            end;
        end;
      end;
    end;
  end;
end; 

// Change the items in the Channel combobox.
procedure TfrmLevelsTool.ChangeChannelItems;
begin
  cmbbxChannel.Items.Clear;

  if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
  begin
    cmbbxChannel.Items.Add('Value');
    cmbbxChannel.Items.Add('Red');
    cmbbxChannel.Items.Add('Green');
    cmbbxChannel.Items.Add('Blue');
  end
  else
  begin
    with ActiveChildForm do
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              cmbbxChannel.Items.Add(
                ChannelManager.SelectedAlphaChannel.ChannelName);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              cmbbxChannel.Items.Add(
                ChannelManager.QuickMaskChannel.ChannelName);
            end;
          end;

        ctLayerMaskChannel:
          begin
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              cmbbxChannel.Items.Add(
                ChannelManager.LayerMaskChannel.ChannelName);
            end;
          end;

        ctColorChannel:
          begin
            cmbbxChannel.Items.Add('Value');
            cmbbxChannel.Items.Add('Red');
            cmbbxChannel.Items.Add('Green');
            cmbbxChannel.Items.Add('Blue');
          end;
      end;
    end;
  end;

  if Assigned(FLevelsTool) then
  begin
    cmbbxChannel.ItemIndex := FLevelsTool.Channel;
  end;
  
  UpdateChannelComboBoxHint;
end;

procedure TfrmLevelsTool.UpdateChannelComboBoxHint;
begin
  if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
  begin
    case cmbbxChannel.ItemIndex of
      0:
        begin
          cmbbxChannel.Hint := 'Value';
        end;
        
      1:
        begin
          cmbbxChannel.Hint := 'Red Channel';
        end;
        
      2:
        begin
          cmbbxChannel.Hint := 'Green Channel';
        end;
        
      3:
        begin
          cmbbxChannel.Hint := 'Blue Channel';
        end;
    end;
  end
  else
  begin
    with ActiveChildForm do
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              cmbbxChannel.Hint :=
                ChannelManager.SelectedAlphaChannel.ChannelName;
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              cmbbxChannel.Hint :=
                ChannelManager.QuickMaskChannel.ChannelName;
            end;
          end;

        ctLayerMaskChannel:
          begin
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              cmbbxChannel.Hint :=
                ChannelManager.LayerMaskChannel.ChannelName;
            end;
          end;

        ctColorChannel:
          begin
            case cmbbxChannel.ItemIndex of
              0:
                begin
                  cmbbxChannel.Hint := 'Value';
                end;

              1:
                begin
                  cmbbxChannel.Hint := 'Red Channel';
                end;

              2:
                begin
                  cmbbxChannel.Hint := 'Green Channel';
                end;

              3:
                begin
                  cmbbxChannel.Hint := 'Blue Channel';
                end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmLevelsTool.LevelsUpdate(const AUpdateItems: Integer);
var
  LChannel : Integer;
  LRect    : TRect;
begin
  if FLevelsTool <> nil then
  begin
    if FLevelsTool.IsColored then
    begin
      LChannel := FLevelsTool.Channel;
    end
    else
    begin
      // FIXME: hack
      if FLevelsTool.Channel = 1 then
      begin
        LChannel := GIMP_HISTOGRAM_ALPHA;
      end
      else
      begin
        LChannel := GIMP_HISTOGRAM_VALUE;
      end;
    end;

    // Recalculate the transfer arrays
    FLevelsTool.LevelsCalculateTransfers;

    // Don't execute the OnChange event of the following components temporarily.
    FCanChange := False;
    try
      if (AUpdateItems and LOW_INPUT) <> 0 then
      begin
        updwnLevelsLowInput.Position := FLevelsTool.LevelsLowInput;
      end;

      if (AUpdateItems and GAMMA) <> 0 then
      begin
        updwnLevelsGamma.Position := Round( (FLevelsTool.LevelsGamma - 1.0) * 10 ) + UP_DOWN_GAMMA_INIT;
        edtLevelsGamma.Text       := FloatToStr(FLevelsTool.LevelsGamma);
      end;

      if (AUpdateItems and HIGH_INPUT) <> 0 then
      begin
        updwnLevelsHighInput.Position := FLevelsTool.LevelsHighInput;
      end;

      if (AUpdateItems and LOW_OUTPUT) <> 0 then
      begin
        updwnLevelsLowOutput.Position := FLevelsTool.LevelsLowOutput;
      end;

      if (AUpdateItems and HIGH_OUTPUT) <> 0 then
      begin
        updwnLevelsHighOutput.Position := FLevelsTool.LevelsHighOutput;
      end;
    finally
      FCanChange := True;
    end;

    if FColorBar <> nil then
    begin
      FColorBar.Channel := LChannel;
    end;

    if (AUpdateItems and INPUT_LEVELS) <> 0 then
    begin
      imgInputBar.Bitmap.Canvas.Brush.Color := clBtnFace;
      imgInputBar.Bitmap.Canvas.FillRect(imgInputBar.Bitmap.Canvas.ClipRect);

      case LChannel of
        GIMP_HISTOGRAM_VALUE,
        GIMP_HISTOGRAM_ALPHA,
        GIMP_HISTOGRAM_RGB:
          begin
            FColorBar.gimp_color_bar_set_buffers(
              FLevelsTool.Levels.FInput[FLevelsTool.Channel],
              FLevelsTool.Levels.FInput[FLevelsTool.Channel],
              FLevelsTool.Levels.FInput[FLevelsTool.Channel]);
          end;

        GIMP_HISTOGRAM_RED,
        GIMP_HISTOGRAM_GREEN,
        GIMP_HISTOGRAM_BLUE:
          begin
            FColorBar.gimp_color_bar_set_buffers(
              FLevelsTool.Levels.FInput[GIMP_HISTOGRAM_RED],
              FLevelsTool.Levels.FInput[GIMP_HISTOGRAM_GREEN],
              FLevelsTool.Levels.FInput[GIMP_HISTOGRAM_BLUE]);
          end;
      end;
      
      FColorBar.gimp_color_bar_expose(FInputColorBarBmp);
      imgInputBar.Bitmap.Draw(BORDER, 0, FInputColorBarBmp);
    end;

    if (AUpdateItems and OUTPUT_LEVELS) <> 0 then
    begin
      imgOutputBar.Bitmap.Canvas.Brush.Color := clBtnFace;
      imgOutputBar.Bitmap.Canvas.FillRect(imgOutputBar.Bitmap.Canvas.ClipRect);

      FColorBar.IsHalfProcess := False;    // Draw all the height of Color Bar
      FColorBar.Channel       := LChannel;

      FColorBar.gimp_color_bar_expose(FOutputColorBarBmp);
      imgOutputBar.Bitmap.Draw(BORDER, 0, FOutputColorBarBmp);
    end;

    if (AUpdateItems and INPUT_SLIDERS) <> 0 then
    begin
      LRect.Left   := 0;
      LRect.Top    := GRADIENT_HEIGHT;
      LRect.Right  := imgInputBar.Width;
      LRect.Bottom := imgInputBar.Height;

      // Clear the old slider.
      imgInputBar.Bitmap.Canvas.Brush.Color := clBtnFace;
      imgInPutBar.Bitmap.Canvas.FillRect(LRect);

      // Draw the new one.
      FLevelsTool.LevelsInputAreaExpose(imgInputBar.Bitmap.Canvas, BORDER);
    end;

    if (AUpdateItems and OUTPUT_SLIDERS) <> 0 then
    begin
      LRect.Left   := 0;
      LRect.Top    := GRADIENT_HEIGHT;
      LRect.Right  := imgOutputBar.Width;
      LRect.Bottom := imgOutputBar.Height;

      // Clear the old slider.
      imgOutputBar.Bitmap.Canvas.Brush.Color := clBtnFace;
      imgOutPutBar.Bitmap.Canvas.FillRect(LRect);

      // Draw the new one.
      FLevelsTool.LevelsOutputAreaExpose(imgOutputBar.Bitmap.Canvas, BORDER);
    end;
  end;
end;

procedure TfrmLevelsTool.FormCreate(Sender: TObject);
begin
  FWorkingOnEffectLayer := False;
  FLevelsFileName       := '';
  FNormalLevelsTool     := nil;
  FLevelsTool           := nil;
  FLevelsLayer          := nil;

  FColorBar            := TgmGimpColorBar.Create;
  FHistogram           := TgmGimpHistogram.Create;
  FHistogram.LineColor := clBlack;
  FHistogram.Scale     := TgmGimpHistogramScale(StrToInt(ReadInfoFromIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_HISTOGRAM, '0')));
  
  FDrawing           := False;
  FCanChange         := True;
  FInputColorBarBmp  := TBitmap32.Create;
  FOutputColorBarBmp := TBitmap32.Create;

  with updwnLevelsGamma do
  begin
    Max      := UP_DOWN_GAMMA_MAX;
    Min      := UP_DOWN_GAMMA_MIN;
    Position := UP_DOWN_GAMMA_INIT;
  end;

  spdbtnLinearHistogram.Down      := (FHistogram.Scale = GIMP_HISTOGRAM_SCALE_LINEAR);
  spdbtnLogarithmicHistogram.Down := (FHistogram.Scale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);

  imgInputBar.Bitmap.Width  := imgInputBar.Width;
  imgInputBar.Bitmap.Height := GRADIENT_HEIGHT  + CONTROL_HEIGHT;
  pnlInputArea.Width        := imgInputBar.Width  + 4;
  pnlInputArea.Height       := imgInputBar.Height + 4;

  imgOutputBar.Bitmap.Width  := imgOutputBar.Width;
  imgOutputBar.Bitmap.Height := GRADIENT_HEIGHT   + CONTROL_HEIGHT;
  pnlOutputArea.Width        := imgOutputBar.Width  + 4;
  pnlOutputArea.Height       := imgOutputBar.Height + 4;

  imgHistogram.Width  := pnlHistogramHolder.Width - 2 * BORDER - 4;
  imgHistogram.Height := pnlHistogramHolder.Height - 8;

  with FInputColorBarBmp do
  begin
    Width  := imgInputBar.Bitmap.Width - 2 * BORDER;
    Height := GRADIENT_HEIGHT;
  end;

  with FOutputColorBarBmp do
  begin
    Width  := imgOutputBar.Bitmap.Width - 2 * BORDER;
    Height := GRADIENT_HEIGHT;
  end;
end;

procedure TfrmLevelsTool.FormDestroy(Sender: TObject);
begin
  if Assigned(FNormalLevelsTool) then
  begin
    FNormalLevelsTool.Free;
  end;

  if Assigned(FColorBar) then
  begin
    FColorBar.Free;
  end;

  if Assigned(FHistogram) then
  begin
    FHistogram.Free;
  end;

  FInputColorBarBmp.Free;
  FOutputColorBarBmp.Free;
end; 

procedure TfrmLevelsTool.FormShow(Sender: TObject);
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
              if Assigned(ChannelManager.QuickMaskChannel) then
              begin
                frmMain.FBitmapBefore.Assign(
                  ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
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
      FHistogram.gimp_histogram_calculate(frmMain.FBitmapBefore);

      if FNormalLevelsTool <> nil then
      begin
        FreeAndNil(FNormalLevelsTool);
      end;

      // Create Levels Tool for menu command
      FNormalLevelsTool := TgmLevelsTool.Create(frmMain.FBitmapBefore);

      // if work with layers...
      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          FNormalLevelsTool.Channel := StrToInt(ReadInfoFromIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_CHANNEL, '0'));
        end;
      end;

      // Pointing to the Levels tool that for menu command
      FLevelsTool := FNormalLevelsTool;

      chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_PREVIEW, '1')));
    end;
  end;

  ChangeChannelItems;  // Change the items in the Channel combobox.

  if Assigned(FLevelsTool) then
  begin
    FHistogram.gimp_histogram_view_expose(FLevelsTool.Channel);
  end;
  
  imgHistogram.Bitmap.Assign(FHistogram.HistogramMap);
  LevelsUpdate(ALL);

  if tlbtnBlackPicker.Down then
  begin
    tlbtnBlackPicker.Down := False;
  end;
  
  if tlbtnGrayPicker.Down then
  begin
    tlbtnGrayPicker.Down  := False;
  end;
  
  if tlbtnWhitePicker.Down then
  begin
    tlbtnWhitePicker.Down := False;
  end;
  
  // Create frmImageColorPicker
  frmImageColorPicker := TfrmImageColorPicker.Create(nil);

  ActiveControl := btbtnOK;  
end;

procedure TfrmLevelsTool.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if frmImageColorPicker.Visible then
  begin
    frmImageColorPicker.Close;
  end;

  if Assigned(FNormalLevelsTool) then
  begin
    FreeAndNil(FNormalLevelsTool);
  end;

  // Free frmImageColorPicker
  FreeAndNil(frmImageColorPicker);
end; 

procedure TfrmLevelsTool.imgInputBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i, LDistance : Integer;
begin
  LDistance := G_MAXINT;

  for i := 0 to 2 do
  begin
    if Abs(X - FLevelsTool.SliderPos[i]) < LDistance then
    begin
      FLevelsTool.ActiveSlider := i;
      LDistance := Abs(X - FLevelsTool.SliderPos[i]);
    end;
  end;

  UpdateLevelsInput(X);

  FDrawing := True;
end; 

procedure TfrmLevelsTool.imgInputBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    UpdateLevelsInput(X);
  end;
end;

procedure TfrmLevelsTool.imgInputBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;
  
  LevelsUpdate(ALL);
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.updwnLevelsGammaClick(Sender: TObject;
  Button: TUDBtnType);
var
  LValue : Integer;
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      LValue                  := updwnLevelsGamma.Position - UP_DOWN_GAMMA_INIT;
      FLevelsTool.LevelsGamma := 1.0 + LValue / 10;
      edtLevelsGamma.Text     := FloatToStr(FLevelsTool.LevelsGamma);
      
      LevelsUpdate(INPUT_SLIDERS or INPUT_LEVELS);
    end;
  end;
end;

procedure TfrmLevelsTool.updwnLevelsGammaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels;
end; 

procedure TfrmLevelsTool.imgOutputBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i, LDistance : Integer;
begin
  LDistance := G_MAXINT;

  for i := 3 to 4 do
  begin
    if Abs(X - FLevelsTool.SliderPos[i]) < LDistance then
    begin
      FLevelsTool.ActiveSlider := i;
      LDistance := Abs(X - FLevelsTool.SliderPos[i]);
    end;
  end;

  UpdateLevelsOutput(X);

  FDrawing := True;
end;

procedure TfrmLevelsTool.imgOutputBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    UpdateLevelsOutput(X);
  end;
end;

procedure TfrmLevelsTool.imgOutputBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;

  LevelsUpdate(ALL);
  ExecuteGimpLevels;
end; 

procedure TfrmLevelsTool.edtLevelsLowInputChange(Sender: TObject);
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      FLevelsTool.LevelsLowInput := updwnLevelsLowInput.Position;
      LevelsUpdate(INPUT_SLIDERS or INPUT_LEVELS);
    end;
  end;
end; 

procedure TfrmLevelsTool.edtLevelsHighInputChange(Sender: TObject);
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      FLevelsTool.LevelsHighInput := updwnLevelsHighInput.Position;
      LevelsUpdate(INPUT_SLIDERS or INPUT_LEVELS);
    end;
  end;
end; 

procedure TfrmLevelsTool.edtLevelsLowOutputChange(Sender: TObject);
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      FLevelsTool.LevelsLowOutput := updwnLevelsLowOutput.Position;
      LevelsUpdate(OUTPUT_SLIDERS);
    end;
  end;
end; 

procedure TfrmLevelsTool.edtLevelsHighOutputChange(Sender: TObject);
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      FLevelsTool.LevelsHighOutput := updwnLevelsHighOutput.Position;
      LevelsUpdate(OUTPUT_SLIDERS);
    end;
  end;
end;

procedure TfrmLevelsTool.ChangeLevelsChannel(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Channel := cmbbxChannel.ItemIndex;
    UpdateChannelComboBoxHint;
    LevelsUpdate(ALL);

    if FHistogram <> nil then
    begin
      FHistogram.gimp_histogram_view_expose(FLevelsTool.Channel);
      imgHistogram.Bitmap.Assign(FHistogram.HistogramMap);
    end;
  end;
end; 

procedure TfrmLevelsTool.ChangeHistogramType(Sender: TObject);
begin
  if FHistogram <> nil then
  begin
    if Sender = spdbtnLinearHistogram then
    begin
      FHistogram.Scale := GIMP_HISTOGRAM_SCALE_LINEAR;
    end
    else if Sender = spdbtnLogarithmicHistogram then
    begin
      FHistogram.Scale := GIMP_HISTOGRAM_SCALE_LOGARITHMIC;
    end;

    FHistogram.gimp_histogram_view_expose(FLevelsTool.Channel);
    imgHistogram.Bitmap.Assign(FHistogram.HistogramMap);
  end;
end;

procedure TfrmLevelsTool.ResetChannel(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.LevelsChannelReset;
    LevelsUpdate(ALL);
    ExecuteGimpLevels;
  end;
end; 

procedure TfrmLevelsTool.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.ResetLevelsTool(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Reset;
    LevelsUpdate(ALL);
    ExecuteGimpLevels;
  end;
end;

procedure TfrmLevelsTool.updwnLevelsLowInputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.updwnLevelsHighInputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.updwnLevelsLowOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.updwnLevelsHighOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels;
end;

procedure TfrmLevelsTool.btnAutoAdjustmentClick(Sender: TObject);
begin
  if (FLevelsTool <> nil) and (FHistogram <> nil) then
  begin
    FLevelsTool.LevelsStretch(FHistogram);
    LevelsUpdate(ALL);
    ExecuteGimpLevels;
  end;
end;

procedure TfrmLevelsTool.ChangeActivePicker(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    if Sender = tlbtnBlackPicker then
    begin
      if tlbtnBlackPicker.Down then
      begin
        FLevelsTool.ActivePicker := lapBlack;

        if not frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Show;
        end;
      end
      else
      begin
        FLevelsTool.ActivePicker := lapNone;

        if frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Close;
        end;
      end;
    end
    else
    if Sender = tlbtnGrayPicker then
    begin
      if tlbtnGrayPicker.Down then
      begin
        FLevelsTool.ActivePicker := lapGray;

        if not frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Show;
        end;
      end
      else
      begin
        FLevelsTool.ActivePicker := lapNone;

        if frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Close;
        end;
      end;
    end
    else
    if Sender = tlbtnWhitePicker then
    begin
      if tlbtnWhitePicker.Down then
      begin
        FLevelsTool.ActivePicker := lapWhite;

        if not frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Show;
        end;
      end
      else
      begin
        FLevelsTool.ActivePicker := lapNone;
        
        if frmImageColorPicker.Visible then
        begin
          frmImageColorPicker.Close;
        end;
      end;
    end;

    tlbtnBlackPicker.Down := (FLevelsTool.ActivePicker = lapBlack);
    tlbtnGrayPicker.Down  := (FLevelsTool.ActivePicker = lapGray);
    tlbtnWhitePicker.Down := (FLevelsTool.ActivePicker = lapWhite);
  end;
end;

procedure TfrmLevelsTool.btbtnOKClick(Sender: TObject);
begin
  if Assigned(FLevelsLayer) and FWorkingOnEffectLayer then
  begin
    FLevelsLayer.HistogramScale := FHistogram.Scale;
  end
  else
  begin
    // save settings to INI file
    if Assigned(FLevelsTool) then
    begin
      WriteInfoToIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_CHANNEL,
                         IntToStr(FLevelsTool.Channel));
    end;

    WriteInfoToIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_HISTOGRAM,
                       IntToStr(Ord(FHistogram.Scale)));
                       
    WriteInfoToIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_PREVIEW,
                       IntToStr(Integer(chckbxPreview.Checked)));
  end;
end; 

procedure TfrmLevelsTool.btnLoadLevelsClick(Sender: TObject);
begin
  if Assigned(FLevelsTool) then
  begin
    OpenDialog.InitialDir := ReadInfoFromIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_FILE_DIR, ExtractFilePath( ParamStr(0) ));

    if OpenDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        FLevelsFileName := OpenDialog.FileName;
        try
          // The following methold must be called first, otherwise, the
          // FLevelsTool.LoadFormFile() will causes an exception.
          // The reason for the problem is not clear, yet. 
          ResetLevelsTool(Sender);

          if FLevelsTool.LoadFromFile(FLevelsFileName) then
          begin
            if cmbbxChannel.Items.Count > 1 then
            begin
              cmbbxChannel.ItemIndex := FLevelsTool.Channel;
            end
            else
            begin
              cmbbxChannel.ItemIndex := 0;
              ChangeLevelsChannel(Sender);
            end;

            LevelsUpdate(ALL);
            ExecuteGimpLevels;

            WriteInfoToIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_FILE_DIR,
                               ExtractFilePath(FLevelsFileName));
          end
          else // loading error
          begin
            MessageDlg(FLevelsTool.OutputMsg, mtError, [mbOK], 0);
          end;

        except
          MessageDlg('Cannot open the file "' + ExtractFileName(OpenDialog.FileName) + '".', mtError, [mbOK], 0)
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end; 

procedure TfrmLevelsTool.btnSaveLevelsClick(Sender: TObject);
var
  LFileDir        : string;
  LFileExt        : string;
  LOutputFileName : string;
begin
  if Assigned(FLevelsTool) then
  begin
    if FLevelsFileName = '' then
    begin
      SaveDialog.FileName := 'Untitled' + LEVELS_FILE_EXT;
    end
    else
    begin
      SaveDialog.FileName := ExtractFileName(FLevelsFileName);
    end;

    SaveDialog.InitialDir := ReadInfoFromIniFile(SECTION_LEVELS_DIALOG,
                                                 IDENT_LEVELS_FILE_DIR,
                                                 ExtractFilePath( ParamStr(0) ));

    if SaveDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        LOutputFileName := SaveDialog.FileName;
        LFileDir        := ExtractFileDir(LOutputFileName);

        if (LFileDir <> '') and DirectoryExists(LFileDir) then
        begin
          LFileExt := ExtractFileExt(LOutputFileName);

          if LFileExt = '' then
          begin
            LOutputFileName := LOutputFileName + LEVELS_FILE_EXT;
          end
          else
          if LFileExt <> LEVELS_FILE_EXT then
          begin
            LOutputFileName := ChangeFileExt(LOutputFileName, LEVELS_FILE_EXT);
          end;

          if FileExists(LOutputFileName) then
          begin
            if MessageDlg('The file "' + ExtractFileName(LOutputFileName) + '" is already exists.' + #10#13 +
                          'Do you want to replace it?', mtConfirmation, mbOKCancel, 0) <> mrOK then
            begin
              Exit;
            end;
          end;

          FLevelsTool.SaveToFile(SaveDialog.FileName);

          FLevelsFileName := SaveDialog.FileName;

          WriteInfoToIniFile(SECTION_LEVELS_DIALOG, IDENT_LEVELS_FILE_DIR,
                             ExtractFilePath(FLevelsFileName));
        end;
        
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end; 

end.
