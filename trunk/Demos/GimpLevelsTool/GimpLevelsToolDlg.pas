unit GimpLevelsToolDlg;

{ This library created in 2006/09/07.
  Copyright (c) 2006 by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com).
  All rights reserved.

  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  * 2. The name of the author may not be used to endorse or promote products
  *    derived from this software withough specific prior written permission
  *
  * Based on the the Gimp 2.2.10 .
  * The original source can be found at www.gimp.org.
  *
  * This library is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Library General Public
  * License as published by the Free Software Foundation; either
  * version 2 of the License, or (at your option) any later version.
  *
  * This library is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Library General Public License for more details.
  *
  * You should have received a copy of the GNU Library General Public
  * License along with this library; if not, write to the
  * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.

  Thanks to the authors of GIMP for giving us the opportunity to know how to
  achieve Levels Tool. }

// Update Data: 2015/04/13

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ImgList, ComCtrls, ToolWin,
{ Graphics32 }
  GR32,
  GR32_Image,
  GR32_Layers,
{ GraphicsMagicLib }
  gmGimpColorBar,
  gmGimpHistogram,
  gmLevelsTool;

type
  TfrmGimpLevelsTool = class(TForm)
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
    procedure btbtnCancelClick(Sender: TObject);
  private
    FDrawing           : Boolean;
    FInputColorBarBmp  : TBitmap32;
    FOutputColorBarBmp : TBitmap32;
    FCanChange         : Boolean;

    FProcessedBmp      : TBitmap32;
    FLevelsTool        : TgmLevelsTool;
    FColorBar          : TgmGimpColorBar;
    FHistogram         : TgmGimpHistogram;

    procedure UpdateLevelsInput(AX: Integer);
    procedure UpdateLevelsOutput(AX: Integer);
  public
    procedure LevelsUpdate(const AUpdate: Integer);
    procedure ExecuteGimpLevels;

    property LevelsTool : TgmLevelsTool read FLevelsTool;
  end;

var
  frmGimpLevelsTool: TfrmGimpLevelsTool;

implementation

uses
{ Delphi }
  Math,
{ GraphicsMagicLib }
  gmGimpBaseEnums,
  gmGimpCommonFuncs,
  gmLevels,
  gmTypes,
{ Demo }
  Main;

{$R *.dfm}

const
  GAMMA_Max  = 100;
  GAMMA_MIN  = 1;
  GAMMA_INIT = 10;

procedure TfrmGimpLevelsTool.UpdateLevelsInput(AX: Integer);
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

procedure TfrmGimpLevelsTool.UpdateLevelsOutput(AX: Integer);
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

procedure TfrmGimpLevelsTool.LevelsUpdate(const AUpdate: Integer);
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
        LChannel := GIMP_HISTOGRAM_ALPHA
      else
        LChannel := GIMP_HISTOGRAM_VALUE;
    end;

    // Recalculate the transfer arrays
    FLevelsTool.LevelsCalculateTransfers();

    // Don't execute the OnChange event of the following components temporarily.
    FCanChange := False;
    try
      if (AUpdate and LOW_INPUT) <> 0 then
      begin
        updwnLevelsLowInput.Position := FLevelsTool.LevelsLowInput;
      end;

      if (AUpdate and GAMMA) <> 0 then
      begin
        updwnLevelsGamma.Position := Round( (FLevelsTool.LevelsGamma - 1.0) * 10 ) + GAMMA_INIT;
        edtLevelsGamma.Text       := FloatToStr(FLevelsTool.LevelsGamma);
      end;

      if (AUpdate and HIGH_INPUT) <> 0 then
      begin
        updwnLevelsHighInput.Position := FLevelsTool.LevelsHighInput;
      end;

      if (AUpdate and LOW_OUTPUT) <> 0 then
      begin
        updwnLevelsLowOutput.Position := FLevelsTool.LevelsLowOutput;
      end;

      if (AUpdate and HIGH_OUTPUT) <> 0 then
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

    if (AUpdate and INPUT_LEVELS) <> 0 then
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
      imgInputBar.Bitmap.Draw(Border, 0, FInputColorBarBmp);
    end;

    if (AUpdate and OUTPUT_LEVELS) <> 0 then
    begin
      imgOutputBar.Bitmap.Canvas.Brush.Color := clBtnFace;
      imgOutputBar.Bitmap.Canvas.FillRect(imgOutputBar.Bitmap.Canvas.ClipRect);

      FColorBar.IsHalfProcess := False;    // Draw all the height of Color Bar
      FColorBar.Channel       := LChannel;

      FColorBar.gimp_color_bar_expose(FOutputColorBarBmp);
      imgOutputBar.Bitmap.Draw(Border, 0, FOutputColorBarBmp);
    end;

    if (AUpdate and INPUT_SLIDERS) <> 0 then
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

    if (AUpdate and OUTPUT_SLIDERS) <> 0 then
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

procedure TfrmGimpLevelsTool.ExecuteGimpLevels;
begin
  FLevelsTool.Map(FProcessedBmp, [csRed, csGreen, csBlue]);

  if chckbxPreview.Checked then
    frmMain.imgViewer.Bitmap.Assign(FProcessedBmp);
end; 

procedure TfrmGimpLevelsTool.FormCreate(Sender: TObject);
begin
  FLevelsTool        := nil;
  FColorBar          := nil;
  FHistogram         := nil;
  FDrawing           := False;
  FCanChange         := True;
  FProcessedBmp      := TBitmap32.Create();
  FInputColorBarBmp  := TBitmap32.Create();
  FOutputColorBarBmp := TBitmap32.Create();

  with updwnLevelsGamma do
  begin
    Max      := GAMMA_Max;
    Min      := GAMMA_MIN;
    Position := GAMMA_INIT;
  end;
end; 

procedure TfrmGimpLevelsTool.FormDestroy(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Free();
  end;

  if FColorBar <> nil then
  begin
    FColorBar.Free();
  end;

  if FHistogram <> nil then
  begin
    FHistogram.Free();
  end;

  FProcessedBmp.Free();
  FInputColorBarBmp.Free();
  FOutputColorBarBmp.Free();
end; 

procedure TfrmGimpLevelsTool.FormShow(Sender: TObject);
begin
  imgInputBar.Bitmap.Width  := imgInputBar.Width;
  imgInputBar.Bitmap.Height := GRADIENT_HEIGHT + CONTROL_HEIGHT;
  pnlInputArea.Width        := imgInputBar.Width + 4;
  pnlInputArea.Height       := imgInputBar.Height + 4;

  imgOutputBar.Bitmap.Width  := imgOutputBar.Width;
  imgOutputBar.Bitmap.Height := GRADIENT_HEIGHT + CONTROL_HEIGHT;
  pnlOutputArea.Width        := imgOutputBar.Width + 4;
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

  cmbbxChannel.ItemIndex := GIMP_HISTOGRAM_VALUE;

  FLevelsTool          := TgmLevelsTool.Create(frmMain.OriginalBitmap);
  FColorBar            := TgmGimpColorBar.Create();
  FHistogram           := TgmGimpHistogram.Create();
  FHistogram.LineColor := clBlack;

  if spdbtnLinearHistogram.Down then
  begin
    FHistogram.Scale := GIMP_HISTOGRAM_SCALE_LINEAR;
  end
  else if spdbtnLogarithmicHistogram.Down then
  begin
    FHistogram.Scale := GIMP_HISTOGRAM_SCALE_LOGARITHMIC;
  end;

  FHistogram.gimp_histogram_calculate(frmMain.OriginalBitmap);
  FHistogram.gimp_histogram_view_expose(FLevelsTool.Channel);
  imgHistogram.Bitmap.Assign(FHistogram.HistogramMap);
  LevelsUpdate(ALL);

  if tlbtnBlackPicker.Down then
  begin
    tlbtnBlackPicker.Down := False;
  end;
  
  if tlbtnGrayPicker.Down then
  begin
    tlbtnGrayPicker.Down := False;
  end;
  
  if tlbtnWhitePicker.Down then
  begin
    tlbtnWhitePicker.Down := False;
  end;

  frmMain.btnOpenImage.Enabled     := False;
  frmMain.btnOpenLevelsDlg.Enabled := False;
end; 

procedure TfrmGimpLevelsTool.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Free();
    FLevelsTool := nil;
  end;

  if FColorBar <> nil then
  begin
    FColorBar.Free();
    FColorBar := nil;
  end;

  if FHistogram <> nil then
  begin
    FHistogram.Free();
    FHistogram := nil;
  end;

  frmMain.btnOpenImage.Enabled     := True;
  frmMain.btnOpenLevelsDlg.Enabled := True;
end; 

procedure TfrmGimpLevelsTool.imgInputBarMouseDown(Sender: TObject;
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
      LDistance                := Abs(X - FLevelsTool.SliderPos[i]);
    end;
  end;

  UpdateLevelsInput(X);

  FDrawing := True;
end; 

procedure TfrmGimpLevelsTool.imgInputBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
    UpdateLevelsInput(X);
end;

procedure TfrmGimpLevelsTool.imgInputBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;
  LevelsUpdate(ALL);
  ExecuteGimpLevels();
end; 

procedure TfrmGimpLevelsTool.updwnLevelsGammaClick(Sender: TObject;
  Button: TUDBtnType);
var
  LValue : Integer;
begin
  if FCanChange then
  begin
    if FLevelsTool <> nil then
    begin
      LValue := updwnLevelsGamma.Position - GAMMA_INIT;
      FLevelsTool.LevelsGamma := 1.0 + LValue / 10;
      edtLevelsGamma.Text := FloatToStr(FLevelsTool.LevelsGamma);

      LevelsUpdate(INPUT_SLIDERS or INPUT_LEVELS);
    end;
  end;
end; 

procedure TfrmGimpLevelsTool.updwnLevelsGammaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels();
end; 

procedure TfrmGimpLevelsTool.imgOutputBarMouseDown(Sender: TObject;
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
      LDistance                := Abs(X - FLevelsTool.SliderPos[i]);
    end;
  end;

  UpdateLevelsOutput(X);

  FDrawing := True;
end; 

procedure TfrmGimpLevelsTool.imgOutputBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
    UpdateLevelsOutput(X);
end;

procedure TfrmGimpLevelsTool.imgOutputBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FDrawing := False;
  LevelsUpdate(ALL);
  ExecuteGimpLevels();
end; 

procedure TfrmGimpLevelsTool.edtLevelsLowInputChange(Sender: TObject);
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

procedure TfrmGimpLevelsTool.edtLevelsHighInputChange(Sender: TObject);
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

procedure TfrmGimpLevelsTool.edtLevelsLowOutputChange(Sender: TObject);
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

procedure TfrmGimpLevelsTool.edtLevelsHighOutputChange(Sender: TObject);
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

procedure TfrmGimpLevelsTool.ChangeLevelsChannel(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Channel := cmbbxChannel.ItemIndex;
    LevelsUpdate(ALL);
    
    if FHistogram <> nil then
    begin
      FHistogram.gimp_histogram_view_expose(FLevelsTool.Channel);
      imgHistogram.Bitmap.Assign(FHistogram.HistogramMap);
    end;
  end;
end; 

procedure TfrmGimpLevelsTool.ChangeHistogramType(Sender: TObject);
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

procedure TfrmGimpLevelsTool.ResetChannel(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Reset();
    LevelsUpdate(ALL);
    ExecuteGimpLevels();
  end;
end; 

procedure TfrmGimpLevelsTool.chckbxPreviewClick(Sender: TObject);
begin
  if chckbxPreview.Checked then
  begin
    frmMain.imgViewer.Bitmap.Assign(FProcessedBmp);
  end;
end; 

procedure TfrmGimpLevelsTool.ResetLevelsTool(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    FLevelsTool.Reset();
    LevelsUpdate(ALL);
    ExecuteGimpLevels();
  end;
end; 

procedure TfrmGimpLevelsTool.updwnLevelsLowInputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels();
end; 

procedure TfrmGimpLevelsTool.updwnLevelsHighInputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels();
end;

procedure TfrmGimpLevelsTool.updwnLevelsLowOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels();
end; 

procedure TfrmGimpLevelsTool.updwnLevelsHighOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteGimpLevels();
end;

procedure TfrmGimpLevelsTool.btnAutoAdjustmentClick(Sender: TObject);
begin
  if (FLevelsTool <> nil) and (FHistogram <> nil) then
  begin
    FLevelsTool.LevelsStretch(FHistogram);
    LevelsUpdate(ALL);
    ExecuteGimpLevels();
  end;
end;

procedure TfrmGimpLevelsTool.ChangeActivePicker(Sender: TObject);
begin
  if FLevelsTool <> nil then
  begin
    if Sender = tlbtnBlackPicker then
    begin
      if tlbtnBlackPicker.Down then
        FLevelsTool.ActivePicker := lapBlack
      else
        FLevelsTool.ActivePicker := lapNone;
    end
    else
    if Sender = tlbtnGrayPicker then
    begin
      if tlbtnGrayPicker.Down then
        FLevelsTool.ActivePicker := lapGray
      else
        FLevelsTool.ActivePicker := lapNone;
    end
    else
    if Sender = tlbtnWhitePicker then
    begin
      if tlbtnWhitePicker.Down then
        FLevelsTool.ActivePicker := lapWhite
      else
        FLevelsTool.ActivePicker := lapNone;
    end;

    tlbtnBlackPicker.Down := (FLevelsTool.ActivePicker = lapBlack);
    tlbtnGrayPicker.Down  := (FLevelsTool.ActivePicker = lapGray);
    tlbtnWhitePicker.Down := (FLevelsTool.ActivePicker = lapWhite);
  end;
end; 

procedure TfrmGimpLevelsTool.btbtnOKClick(Sender: TObject);
begin
  frmMain.OriginalBitmap.Assign(FProcessedBmp);
  
  if not chckbxPreview.Checked then
  begin
    frmMain.imgViewer.Bitmap.Assign(FProcessedBmp);
  end;
  
  Close();
end;

procedure TfrmGimpLevelsTool.btbtnCancelClick(Sender: TObject);
begin
  frmMain.imgViewer.Bitmap.Assign(frmMain.OriginalBitmap);
  Close();
end; 

end.
