unit HistogramDlg;

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

(************************************************************
 *
 * This unit is based on efg's HistoStretchGrays project.
 *
 * You could find the original code from:
 *   http://www.efg2.com/Lab/ImageProcessing/HistoStretchGrays.htm
 *   
 ************************************************************)

// Update Date: 2015/11/15


interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
{ Graphics32 }
  GR32,
{ externals }
  HistogramLibrary;

type
  TColorChannel = (ccLuminosity, ccRed, ccGreen, ccBlue, ccIntensity);

  TfrmHistogram = class(TForm)
    btbtnOK: TBitBtn;
    grpbxStatistics: TGroupBox;
    lblMinimum: TLabel;
    lblMaximum: TLabel;
    lblMode: TLabel;
    lblMedian: TLabel;
    lblMean: TLabel;
    lblStandardDeviation: TLabel;
    lblExcessKurtosis: TLabel;
    lblColors: TLabel;
    lblPixels: TLabel;
    lblMinimumValue: TLabel;
    lblMaximumValue: TLabel;
    lblModeValue: TLabel;
    lblMedianValue: TLabel;
    lblMeanValue: TLabel;
    lblStandardDeviationValue: TLabel;
    lblExcessKurtosisValue: TLabel;
    lblColorsValue: TLabel;
    lblPixelsValue: TLabel;
    grpbxHistogram: TGroupBox;
    lblChannel: TLabel;
    cmbbxChannel: TComboBox;
    imgHistogram: TImage;
    imgColorBar: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbbxChannelChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btbtnOKClick(Sender: TObject);
  private
    FSourceBitmap : TBitmap32;
    FColorChannel : TColorChannel;

    procedure UpdateDisplay(ASourceBmp: TBitmap32;
      const AColorChannel: TColorChannel);

    procedure DrawColorBar(const AColorChannel: TColorChannel);

    procedure ShowHistogram(AHistogram: THistogram; ABitmap: TBitmap;
      AImage: TImage; AMinimumValue, AMaximumValue: TLabel;
      AModeValue, AMedianValue, AMeanValue, AStdDevValue: TLabel;
      AKurtosisValue, AColorsValue, APixelsValue: TLabel);
  public
    { Public declarations }
  end;

var
  frmHistogram: TfrmHistogram;

implementation

uses
{ externals }
  ImageProcessingPrimitives,  // CountColors
{ GraphicsMagic Lib }
  gmChannelManager,
  gmImageProcessFuncs,
  gmIni,
  gmLayers,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm;                   // ActiveChildForm defined in this unit


{$R *.DFM}

const
  HISTOGRAM_WIDTH  = 256;
  HISTOGRAM_HEIGHT = 84;
  COLOR_BAR_HEIGHT = 16;

//-- Custom Procedures and Functions -------------------------------------------

procedure TfrmHistogram.UpdateDisplay(ASourceBmp: TBitmap32;
  const AColorChannel: TColorChannel);
var
  LHistogram      : THistogram;
  LStandardBitmap : TBitmap;
begin
  Screen.Cursor := crHourGlass;
  try
    // Update Display of Histograms.  Some work will be redone below to create
    // optimal Histostretch parameters.
    LHistogram := THistogram.Create();
    try
      // Ask for cpRed, but cpGreen or cpBlue will give identical results
      // since R = G = B for shades of gray

      // get histogram according to different color channel
      case AColorChannel of
        ccLuminosity:
          begin
            GetHistogram(cpValue, ASourceBmp, LHistogram);
          end;

        ccRed:
          begin
            GetHistogram(cpRed, ASourceBmp, LHistogram);
          end;
          
        ccGreen:
          begin
            GetHistogram(cpGreen, ASourceBmp, LHistogram);
          end;
          
        ccBlue:
          begin
            GetHistogram(cpBlue, ASourceBmp, LHistogram);
          end;
          
        ccIntensity:
          begin
            GetHistogram(cpIntensity, ASourceBmp, LHistogram);
          end;
      end;

      LStandardBitmap := TBitmap.Create();
      try
        LStandardBitmap.Assign(ASourceBmp);
        LStandardBitmap.PixelFormat := pf24bit;

        // Show Original Histogram
        ShowHistogram(LHistogram, LStandardBitmap, imgHistogram,
                      lblMinimumValue, lblMaximumValue, lblModeValue,
                      lblMedianValue, lblMeanValue, lblStandardDeviationValue,
                      lblExcessKurtosisValue, lblColorsValue, lblPixelsValue);
      finally
        LStandardBitmap.Free();
      end;

    finally
      LHistogram.Free();
    end;

  finally
    Screen.Cursor := crDefault
  end;
  
  DrawColorBar(AColorChannel);
end;

procedure TfrmHistogram.DrawColorBar(const AColorChannel: TColorChannel);
type
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTipleArray  = array [0..65535] of TRGBTriple;
var
  i, j : Integer;
  LRow : pRGBTripleArray;
  LBmp : TBitmap;
begin
  LBmp := TBitmap.Create();
  try
    LBmp.PixelFormat := pf24bit;
    LBmp.Width       := HISTOGRAM_WIDTH;
    LBmp.Height      := COLOR_BAR_HEIGHT;

    for j := 0 to (LBmp.Height - 1) do
    begin
      LRow := LBmp.Scanline[j];

      for i := 0 to (LBmp.Width - 1) do
      begin
        case AColorChannel of
          ccLuminosity,
          ccIntensity:
            begin
              LRow[i].rgbtRed   := i;
              LRow[i].rgbtGreen := i;
              LRow[i].rgbtBlue  := i;
            end;

          ccRed:
            begin
              LRow[i].rgbtRed   := i;
              LRow[i].rgbtGreen := 0;
              LRow[i].rgbtBlue  := 0;
            end;

          ccGreen:
            begin
              LRow[i].rgbtRed   := 0;
              LRow[i].rgbtGreen := i;
              LRow[i].rgbtBlue  := 0;
            end;
            
          ccBlue:
            begin
              LRow[i].rgbtRed   := 0;
              LRow[i].rgbtGreen := 0;
              LRow[i].rgbtBlue  := i;
            end;
        end;
      end;
    end;
    
    imgColorBar.Picture.Bitmap.Assign(LBmp);
  finally
    LBmp.Free();
  end;
end;

procedure TfrmHistogram.ShowHistogram(AHistogram: THistogram;
  ABitmap: TBitmap; AImage: TImage; AMinimumValue, AMaximumValue: TLabel;
  AModeValue, AMedianValue, AMeanValue, AStdDevValue: TLabel;
  AKurtosisValue, AColorsValue, APixelsValue: TLabel);
const
  clSkyBlue = TColor($F0CAA6);   // RGB:  166 202 240
var
  Kurtosis, Mean, Skewness, StandardDeviation : Double;
  Maximum, Median, Minimum, Mode              : Byte;
  n, ColorCount                               : Integer;
begin
  AImage.Canvas.Brush.Color := clSkyBlue;
  AImage.Canvas.FillRect( Rect(0, 0, AImage.Width, AImage.Height) );

  AHistogram.Draw(AImage.Canvas);
  AHistogram.GetStatistics(n, Minimum, Maximum, Mode, Median, Mean,
                           StandardDeviation, Skewness, Kurtosis);

  ColorCount := CountColors(ABitmap);

  AMinimumValue.Caption  := Format('%d',   [Minimum]);
  AMaximumValue.Caption  := Format('%d',   [Maximum]);
  AModeValue.Caption     := Format('%d',   [Mode]);
  AMedianValue.Caption   := Format('%d',   [Median]);
  AMeanValue.Caption     := Format('%.1f', [Mean]);
  AStdDevValue.Caption   := Format('%.1f', [StandardDeviation]);
  AKurtosisValue.Caption := Format('%.1f', [Kurtosis - 3.0]);
  AColorsValue.Caption   := Format('%d',   [ColorCount]);
  APixelsValue.Caption   := FormatFloat(',##0', n);
end;

//------------------------------------------------------------------------------

procedure TfrmHistogram.FormShow(Sender: TObject);
var
  LChannelName : string;
begin
  if FSourceBitmap = nil then
  begin
    FSourceBitmap := TBitmap32.Create();
  end;
  
  cmbbxChannel.Items.Clear();
  cmbbxChannel.ItemIndex := -1;

  with ActiveChildForm do
  begin
    LChannelName := '';
    
    // determine the name of channel
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel:
        begin
          if Assigned(ChannelManager.SelectedAlphaChannel) then
          begin
            LChannelName := ChannelManager.SelectedAlphaChannel.ChannelName;
          end;
        end;

      ctQuickMaskChannel:
        begin
          if Assigned(ChannelManager.QuickMaskChannel) then
          begin
            LChannelName := ChannelManager.QuickMaskChannel.ChannelName;
          end;
        end;

      ctLayerMaskChannel:
        begin
          if Assigned(ChannelManager.LayerMaskChannel) then
          begin
            LChannelName := ChannelManager.LayerMaskChannel.ChannelName;
          end;
        end;
    end;

    // update Channel Combobox
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel,
      ctQuickMaskChannel,
      ctLayerMaskChannel:
        begin
          cmbbxChannel.Items.Add(LChannelName);

          cmbbxChannel.Hint      := LChannelName;
          cmbbxChannel.ItemIndex := 0;
        end;

      ctColorChannel:
        begin
          cmbbxChannel.Items.Add('Luminosity');
          cmbbxChannel.Items.Add('Red');
          cmbbxChannel.Items.Add('Green');
          cmbbxChannel.Items.Add('Blue');
          cmbbxChannel.Items.Add('Intensity');

          cmbbxChannel.Hint      := '';
          cmbbxChannel.ItemIndex := Integer(FColorChannel);
        end;
    end;

    // determine the source of histogram
    if Assigned(Selection) then
    begin
      Selection.IsAnimated := False;
      FSourceBitmap.Assign(Selection.CutOriginal);
    end
    else
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              FSourceBitmap.Assign(ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              FSourceBitmap.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctLayerMaskChannel:
          begin
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              FSourceBitmap.Assign(ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap);
            end;
          end;

        ctColorChannel:
          begin
            if LayerList.Count > 1 then
            begin
              FSourceBitmap.Assign(LayerList.CombineResult);
            end
            else
            begin
              FSourceBitmap.Assign(LayerList.SelectedLayer.LayerBitmap);
            end;
          end;
      end;
    end;

    // update the histogram
    case ChannelManager.CurrentChannelType of
      ctAlphaChannel,
      ctQuickMaskChannel,
      ctLayerMaskChannel:
        begin
          UpdateDisplay(FSourceBitmap, ccIntensity);
        end;
        
      ctColorChannel:
        begin
          UpdateDisplay(FSourceBitmap, FColorChannel);
        end;
    end;
  end;

  ActiveControl := btbtnOK;
end;

procedure TfrmHistogram.FormCreate(Sender: TObject);
begin
  FSourceBitmap                      := TBitmap32.Create();
  FColorChannel                      := TColorChannel(StrToInt(ReadInfoFromIniFile(SECTION_HISTOGRAM_DIALOG, IDENT_HISTOGRAM_COLOR_CHANNEL, '0')));
  cmbbxChannel.ItemIndex             := 0;
  imgHistogram.Picture.Bitmap.Width  := HISTOGRAM_WIDTH;
  imgHistogram.Picture.Bitmap.Height := HISTOGRAM_HEIGHT;
  imgColorBar.Picture.Bitmap.Width   := HISTOGRAM_WIDTH;
  imgColorBar.Picture.Bitmap.Height  := COLOR_BAR_HEIGHT;
end; 

procedure TfrmHistogram.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free();
end;

procedure TfrmHistogram.cmbbxChannelChange(Sender: TObject);
begin
  FColorChannel := TColorChannel(cmbbxChannel.ItemIndex);

  // update the histogram
  case ActiveChildForm.ChannelManager.CurrentChannelType of
    ctAlphaChannel,
    ctQuickMaskChannel,
    ctLayerMaskChannel:
      begin
        UpdateDisplay(FSourceBitmap, ccIntensity);
      end;
      
    ctColorChannel:
      begin
        UpdateDisplay(FSourceBitmap, FColorChannel);
      end;
  end;
end;

procedure TfrmHistogram.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FSourceBitmap <> nil then
  begin
    FreeAndNil(FSourceBitmap);
  end;
end;

procedure TfrmHistogram.btbtnOKClick(Sender: TObject);
begin
  WriteInfoToIniFile(SECTION_HISTOGRAM_DIALOG, IDENT_HISTOGRAM_COLOR_CHANNEL,
                     IntToStr(Ord(FColorChannel)));
end; 

end.
