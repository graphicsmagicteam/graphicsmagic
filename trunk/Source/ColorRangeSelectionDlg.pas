unit ColorRangeSelectionDlg;

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

// Update Date: 2015/11/15

interface

uses
{ Standard Lib }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls,
{ Graphics32 Lib }
  GR32, GR32_Image, GR32_Layers, GR32_RangeBars,
{ GraphicsMagic Lib }
  gmTypes;

type
  TfrmColorRangeSelection = class(TForm)
    grpbxColorRangeOptions: TGroupBox;
    lblFuzziness: TLabel;
    edtFuzzinessValue: TEdit;
    pnlColorRangeThumbnail: TPanel;
    imgColorRangeThumbnail: TImage32;
    rdbtnSelection: TRadioButton;
    rdbtnImage: TRadioButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    lblSampledColor: TLabel;
    shpSampledColor: TShape;
    lblCurrentColor: TLabel;
    shpCurrentColor: TShape;
    ggbrFuzziness: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeFuzziness(Sender: TObject);
    procedure imgColorRangeThumbnailMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgColorRangeThumbnailMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure rdbtnSelectionClick(Sender: TObject);
    procedure rdbtnImageClick(Sender: TObject);
    procedure edtFuzzinessValueChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FSourceBitmap      : TBitmap32;
    FShadowBitmap      : TBitmap32;
    FScaleBitmap       : TBitmap32;
    FBarIsChanging     : Boolean;
    FSampledColor      : TColor32;
    FFuzziness         : Integer;
    FXActual, FYActual : Integer;
    FScaleWidth        : Integer;
    FScaleHeight       : Integer;
    FThumbnailMode     : TgmThumbnailMode;

    procedure ShowThumbnail(const AThumbnailMode: TgmThumbnailMode);
  public
    property SourceBitmap : TBitmap32 read FSourceBitmap;
    property SampledColor : TColor32  read FSampledColor;
    property Fuzziness    : Integer   read FFuzziness;
  end;

var
  frmColorRangeSelection: TfrmColorRangeSelection;

implementation

uses
{ GraphicsMagic Lib }
  gmImageProcessFuncs,      // MakeColorRangeShadow32()...
  gmMath,
  gmIni,
  gmGUIFuncs,
  gmChannelManager,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

procedure TfrmColorRangeSelection.FormCreate(Sender: TObject);
begin
  FSourceBitmap               := TBitmap32.Create;
  FShadowBitmap               := TBitmap32.Create;
  FScaleBitmap                := TBitmap32.Create;
  FSampledColor               := Color32(TColor(StrToInt(ReadInfoFromIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_SAMPLE_COLOR, IntToStr(ColorToRGB(clBlack))))));
  FFuzziness                  := StrToInt(ReadInfoFromIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_FUZZINESS, '10'));
  FXActual                    := 0;
  FYActual                    := 0;
  FScaleWidth                 := pnlColorRangeThumbnail.Width  - 16;
  FScaleHeight                := pnlColorRangeThumbnail.Height - 16;
  FThumbnailMode              := TgmThumbnailMode(StrToInt(ReadInfoFromIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_THUMBNAIL_MODE, '0')));
  FBarIsChanging              := False;

  shpSampledColor.Brush.Color := WinColor(FSampledColor);
  shpCurrentColor.Brush.Color := WinColor(FSampledColor);
  ggbrFuzziness.Position      := FFuzziness;
  rdbtnSelection.Checked      := (FThumbnailMode = tmSelection);
  rdbtnImage.Checked          := (FThumbnailMode = tmImage);
end;

procedure TfrmColorRangeSelection.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free;
  FShadowBitmap.Free;
  FScaleBitmap.Free;
end;

procedure TfrmColorRangeSelection.FormShow(Sender: TObject);
var
  LChannelMap : TBitmap32;
begin
  LChannelMap := nil;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel,
          ctQuickMaskChannel,
          ctLayerMaskChannel:
            begin
              FSourceBitmap.Assign(Selection.CutOriginal);
            end;

          ctColorChannel:
            begin
              FSourceBitmap.SetSizeFrom(Selection.CutOriginal);

              if ChannelManager.ColorChannelList.SelectedChannelCount = 1 then
              begin
                if csRed in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csRed, False);
                end
                else if csGreen in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csGreen, False);
                end
                else if csBlue in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csBlue, False);
                end;
              end
              else if ChannelManager.ColorChannelList.SelectedChannelCount > 2 then
              begin
                // full RGB channels
                LChannelMap := TBitmap32.Create;
                LChannelMap.Assign(LayerList.CombineResult);
              end;

              if Assigned(LChannelMap) then
              begin
                FSourceBitmap.Draw( FSourceBitmap.ClipRect,
                                    Rect(Selection.MaskBorderStart.X,
                                         Selection.MaskBorderStart.Y,
                                         Selection.MaskBorderEnd.X,
                                         Selection.MaskBorderEnd.Y),
                                    LChannelMap );

                LChannelMap.Free;
              end;
            end;
        end;
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
              FSourceBitmap.Assign(ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
            end;

          ctLayerMaskChannel:
            begin
              FSourceBitmap.Assign(LayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              FSourceBitmap.SetSizeFrom(LayerList.SelectedLayer.LayerBitmap);

              if ChannelManager.ColorChannelList.SelectedChannelCount = 1 then
              begin
                if csRed in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csRed, False);
                end
                else if csGreen in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csGreen, False);
                end
                else if csBlue in ChannelManager.SelectedColorChannels then
                begin
                  LChannelMap := GetChannelMap(LayerList.CombineResult, csBlue, False);
                end;

                if Assigned(LChannelMap) then
                begin
                  FSourceBitmap.Assign(LChannelMap);
                  LChannelMap.Free;
                end;
              end
              else if ChannelManager.ColorChannelList.SelectedChannelCount > 2 then
              begin
                // full RGB channels
                FSourceBitmap.Assign(LayerList.CombineResult);
              end;
            end;
        end;
      end;
    end;
  end;

  ShowThumbnail(FThumbnailMode);
  CenterImageInPanel(pnlColorRangeThumbnail, imgColorRangeThumbnail);
end;

procedure TfrmColorRangeSelection.ShowThumbnail(
  const AThumbnailMode: TgmThumbnailMode);
begin
  case AThumbnailMode of
    tmSelection:
      begin
        MakeColorRangeShadow32(FSourceBitmap, FShadowBitmap, FSampledColor, FFuzziness);
        GetScaledBitmap(FShadowBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
        imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
      end;

    tmImage:
      begin
        GetScaledBitmap(FSourceBitmap, FScaleBitmap, FScaleWidth, FScaleHeight);
        imgColorRangeThumbnail.Bitmap.Assign(FScaleBitmap);
      end;
  end;
end;

procedure TfrmColorRangeSelection.ChangeFuzziness(Sender: TObject);
begin
  FBarIsChanging := True;
  try
    FFuzziness             := ggbrFuzziness.Position;
    edtFuzzinessValue.Text := IntToStr(FFuzziness);

    if FThumbnailMode = tmSelection then
    begin
      ShowThumbnail(FThumbnailMode);
    end;

  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmColorRangeSelection.imgColorRangeThumbnailMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FXActual := MulDiv(X, FSourceBitmap.Width  - 1, imgColorRangeThumbnail.Width  - 1);
  FYActual := MulDiv(Y, FSourceBitmap.Height - 1, imgColorRangeThumbnail.Height - 1);

  shpCurrentColor.Brush.Color := FSourceBitmap.Canvas.Pixels[FXActual, FYActual];
end;

procedure TfrmColorRangeSelection.imgColorRangeThumbnailMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FXActual := MulDiv(X, FSourceBitmap.Width  - 1, imgColorRangeThumbnail.Width  - 1);
  FYActual := MulDiv(Y, FSourceBitmap.Height - 1, imgColorRangeThumbnail.Height - 1);

  FSampledColor               := FSourceBitmap.Pixel[FXActual, FYActual];
  shpSampledColor.Brush.Color := WinColor(FSampledColor);

  if FThumbnailMode = tmSelection then
  begin
    ShowThumbnail(FThumbnailMode);
  end;
end;

procedure TfrmColorRangeSelection.rdbtnSelectionClick(Sender: TObject);
begin
  FThumbnailMode := tmSelection;
  ShowThumbnail(FThumbnailMode);
end;

procedure TfrmColorRangeSelection.rdbtnImageClick(Sender: TObject);
begin
  FThumbnailMode := tmImage;
  ShowThumbnail(FThumbnailMode);
end;

procedure TfrmColorRangeSelection.edtFuzzinessValueChange(Sender: TObject);
begin
  if not FBarIsChanging then
  begin
    try
      FFuzziness := StrToInt(edtFuzzinessValue.Text);
      EnsureValueInRange(FFuzziness, ggbrFuzziness.Min, ggbrFuzziness.Max);
      ggbrFuzziness.Position := FFuzziness;
    except
      edtFuzzinessValue.Text := IntToStr(FFuzziness);
    end;
  end;
end;

procedure TfrmColorRangeSelection.btbtnOKClick(Sender: TObject);
begin
  WriteInfoToIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_FUZZINESS,      IntToStr(FFuzziness));
  WriteInfoToIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_THUMBNAIL_MODE, IntToStr(Ord(FThumbnailMode)));
  WriteInfoToIniFile(SECTION_COLOR_RANGE_SELECTION_DIALOG, IDENT_SAMPLE_COLOR,   IntToStr(Integer(WinColor(FSampledColor))));
end; 

end.
