unit ImageSizeDlg;

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

// Update Date: 2015/08/08

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_Image, GR32_Resamplers, GR32_RangeBars,
{ GraphicsMagicLib}
  gmTypes, gmResamplers;

type
  TfrmImageSize = class(TForm)
    grpbxImageSize: TGroupBox;
    edtImageWidth: TEdit;
    lblImageWidth: TLabel;
    lblImageHeight: TLabel;
    edtImageHeight: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    cmbbxWidthUnit: TComboBox;
    cmbbxHeightUnit: TComboBox;
    imgvwChain: TImgView32;
    chckbxConstrainProperties: TCheckBox;
    grpbxResamplingOptions: TGroupBox;
    lblResampler: TLabel;
    cmbbxResampler: TComboBox;
    lblPAM: TLabel;
    lblWrapMode: TLabel;
    cmbbxPAM: TComboBox;
    cmbbxWrapMode: TComboBox;
    grpbxKernelOptions: TGroupBox;
    lblKernel: TLabel;
    lblKernelMode: TLabel;
    lblTableSize: TLabel;
    cmbbxKernel: TComboBox;
    cmbbxKernelMode: TComboBox;
    ggbrTableSize: TGaugeBar;
    procedure FormShow(Sender: TObject);
    procedure cmbbxWidthUnitChange(Sender: TObject);
    procedure cmbbxHeightUnitChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtImageWidthExit(Sender: TObject);
    procedure edtImageHeightExit(Sender: TObject);
    procedure edtImageWidthChange(Sender: TObject);
    procedure edtImageHeightChange(Sender: TObject);
    procedure chckbxConstrainPropertiesClick(Sender: TObject);
    procedure cmbbxResamplerChange(Sender: TObject);
    procedure cmbbxPAMChange(Sender: TObject);
    procedure cmbbxWrapModeChange(Sender: TObject);
    procedure cmbbxKernelChange(Sender: TObject);
    procedure cmbbxKernelModeChange(Sender: TObject);
    procedure ggbrTableSizeChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FCanChange           : Boolean;
    FConstrainProperties : Boolean;
    FOriginalWidth       : Integer;
    FOriginalHeight      : Integer;
    FNewWidth            : Integer;
    FNewHeight           : Integer;
    FWidthUnit           : TgmAppliedUnit;
    FHeightUnit          : TgmAppliedUnit;
    FResamplingOptions   : TgmResamplingOptions;

    function GetWidthByInch: Double;
    function GetWidthByCM: Double;
    function GetWidthByPoint: Double;
    function GetHeightByInch: Double;
    function GetHeightByCM: Double;
    function GetHeightByPoint: Double;

    procedure SetWidthByInch(const AWidth: Double);
    procedure SetWidthByCM(const AWidth: Double);
    procedure SetWidthByPoint(const AWidth: Double);
    procedure SetHeightByInch(const AHeight: Double);
    procedure SetHeightByCM(const AHeight: Double);
    procedure SetHeightByPoint(const AHeight: Double);

    procedure ShowWidth;
    procedure ShowHeight;
    procedure UpdateKernelOptionsDisplay;
  public
    property NewWidth          : Integer              read FNewWidth;
    property NewHeight         : Integer              read FNewHeight;
    property ResamplingOptions : TgmResamplingOptions read FResamplingOptions;
  end;

var
  frmImageSize: TfrmImageSize;

implementation

uses
{ Dephi }
  Printers,
{ Graphics32 }
  GR32_Layers,
{ externals -- Preview }
  Preview,
{ GraphicsMagicLib }
  gmIni,
  gmMath,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PrintPreviewDlg;

{$R *.DFM}

{ Custom procedures and functions }

function TfrmImageSize.GetWidthByInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoEnglish) / 100;
end;

function TfrmImageSize.GetWidthByCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoMetric) / 100;
end;

function TfrmImageSize.GetWidthByPoint: Double;
var
  LInches : Double;
begin
  LInches := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

function TfrmImageSize.GetHeightByInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoEnglish) / 100;
end;

function TfrmImageSize.GetHeightByCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoMetric) / 100;
end;

function TfrmImageSize.GetHeightByPoint: Double;
var
  LInches : Double;
begin
  LInches := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end; 

procedure TfrmImageSize.SetWidthByInch(const AWidth: Double);
var
  w : Integer;
begin
  w         := Round(AWidth * 100); // convert to mmLoEnglish unit
  FNewWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoEnglish, mmPixel);
end;

procedure TfrmImageSize.SetWidthByCM(const AWidth: Double);
var
  w : Integer;
begin
  w         := Round(AWidth * 100); // convert to mmLoMetric unit
  FNewWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoMetric, mmPixel);
end;

procedure TfrmImageSize.SetWidthByPoint(const AWidth: Double);
var
  LoEnglishVal : Integer;
begin
  LoEnglishVal := Round(AWidth / 72 * 100); // convert to mmLoEnglish unit
  FNewWidth    := frmPrintPreview.prntprvwPreview.ConvertX(LoEnglishVal, mmLoEnglish, mmPixel);
end;

procedure TfrmImageSize.SetHeightByInch(const AHeight: Double);
var
  h : Integer;
begin
  h          := Round(AHeight * 100); // convert to mmLoEnglish unit
  FNewHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoEnglish, mmPixel);
end;

procedure TfrmImageSize.SetHeightByCM(const AHeight: Double);
var
  h : Integer;
begin
  h          := Round(AHeight * 100); // convert to mmLoMetric unit
  FNewHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoMetric, mmPixel);
end;

procedure TfrmImageSize.SetHeightByPoint(const AHeight: Double);
var
  LoEnglishVal : Integer;
begin
  LoEnglishVal := Round(AHeight / 72 * 100); // convert to mmLoEnglish unit
  FNewHeight   := frmPrintPreview.prntprvwPreview.ConvertY(LoEnglishVal, mmLoEnglish, mmPixel);
end;

procedure TfrmImageSize.ShowWidth;
begin
  FCanChange := False;
  try
    case FWidthUnit of
      auInch:
        begin
          edtImageWidth.Text := FloatToStr( GetWidthByInch() );
        end;

      auCentimeter:
        begin
          edtImageWidth.Text := FloatToStr( GetWidthByCM() );
        end;

      auPoint:
        begin
          edtImageWidth.Text := FloatToStr( GetWidthByPoint() );
        end;

      auPixel:
        begin
          edtImageWidth.Text := IntToStr(FNewWidth);
        end;
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmImageSize.ShowHeight;
begin
  FCanChange := False;
  try
    case FHeightUnit of
      auInch:
        begin
          edtImageHeight.Text := FloatToStr( GetHeightByInch() );
        end;

      auCentimeter:
        begin
          edtImageHeight.Text := FloatToStr( GetHeightByCM() );
        end;

      auPoint:
        begin
          edtImageHeight.Text := FloatToStr( GetHeightByPoint() );
        end;
        
      auPixel:
        begin
          edtImageHeight.Text := IntToStr(FNewHeight);
        end;
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmImageSize.UpdateKernelOptionsDisplay;
begin
  if FResamplingOptions.Resampler = rsKernel then
  begin
    grpbxKernelOptions.Visible := True;
    frmImageSize.Height        := grpbxKernelOptions.Top + grpbxKernelOptions.Height + 55;
  end
  else
  begin
    grpbxKernelOptions.Visible := False;
    frmImageSize.Height        := grpbxResamplingOptions.Top + grpbxResamplingOptions.Height + 55;
  end;
end;

procedure TfrmImageSize.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    UpdateKernelOptionsDisplay();
    
    FOriginalWidth  := ActiveChildForm.imgWorkArea.Bitmap.Width;
    FOriginalHeight := ActiveChildForm.imgWorkArea.Bitmap.Height;
    FNewWidth       := ActiveChildForm.imgWorkArea.Bitmap.Width;
    FNewHeight      := ActiveChildForm.imgWorkArea.Bitmap.Height;

    ShowWidth();
    ShowHeight();
    
    ActiveControl := edtImageWidth;
  finally
    Screen.Cursor := crDefault;
  end;
end; 

procedure TfrmImageSize.cmbbxWidthUnitChange(Sender: TObject);
begin
  FWidthUnit := TgmAppliedUnit(cmbbxWidthUnit.ItemIndex);
  ShowWidth();
end;

procedure TfrmImageSize.cmbbxHeightUnitChange(Sender: TObject);
begin
  FHeightUnit := TgmAppliedUnit(cmbbxHeightUnit.ItemIndex);
  ShowHeight();
end;

procedure TfrmImageSize.FormCreate(Sender: TObject);
begin
  FCanChange           := True;
  FConstrainProperties := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_CONSTRAIN, '1')));
  FNewWidth            := 0;
  FNewHeight           := 0;
  FWidthUnit           := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WIDTH_UNIT,  '1')));
  FHeightUnit          := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_HEIGHT_UNIT, '1')));

  with FResamplingOptions do
  begin
    Resampler       := TgmResamplerSelector(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_RESAMPLER, '3')));
    PixelAccessMode := TPixelAccessMode(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_PIXEL_ACCESS_MODE, '1')));
    WrapMode        := TWrapMode(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WRAP_MODE, '0')));
    Kernel          := TgmKernelSelector(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL, '4')));
    KernelMode      := TKernelMode(StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL_MODE, '0')));
    TableSize       := StrToInt(ReadInfoFromIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_TABLE_SIZE, '32'));
  end;

  cmbbxWidthUnit.ItemIndex  := Ord(FWidthUnit);
  cmbbxHeightUnit.ItemIndex := Ord(FHeightUnit);

  cmbbxResampler.Items     := ResamplerNameList();
  cmbbxResampler.ItemIndex := Ord(FResamplingOptions.Resampler);

  cmbbxPAM.Items     := PixelAccessModeList();
  cmbbxPAM.ItemIndex := Ord(FResamplingOptions.PixelAccessMode);

  cmbbxWrapMode.Items     := WrapModeList();
  cmbbxWrapMode.ItemIndex := Ord(FResamplingOptions.WrapMode);

  cmbbxKernel.Items     := KernelNameList();
  cmbbxKernel.ItemIndex := Ord(FResamplingOptions.Kernel);

  cmbbxKernelMode.Items     := KernelModeList();
  cmbbxKernelMode.ItemIndex := Ord(FResamplingOptions.KernelMode);

  imgvwChain.Bitmap.DrawMode := dmBlend;
  imgvwChain.Bitmap.Changed();

  chckbxConstrainProperties.Checked := FConstrainProperties;
  ggbrTableSize.Position            := FResamplingOptions.TableSize;
end;

procedure TfrmImageSize.edtImageWidthExit(Sender: TObject);
begin
  ShowWidth();
end;

procedure TfrmImageSize.edtImageHeightExit(Sender: TObject);
begin
  ShowHeight();
end; 

procedure TfrmImageSize.edtImageWidthChange(Sender: TObject);
var
  LRealVal, LInches, LScale      : Double;
  LMinFloatWidth, LMaxFloatWidth : Double;
  LIntVal                        : Integer;
  LMinIntWidth, LMaxIntWidth     : Integer;
begin
  LMinFloatWidth := 0.00;
  LMaxFloatWidth := 0.00;
  
  if FCanChange then
  begin
    LMinIntWidth := 16;
    LMaxIntWidth := Printer.PageWidth;

    if FWidthUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtImageWidth.Text);

        with frmPrintPreview.prntprvwPreview do
        begin
          case FWidthUnit of
            auInch:
              begin
                LMinFloatWidth := ConvertX(LMinIntWidth, mmPixel, mmLoEnglish) / 100;
                LMaxFloatWidth := ConvertX(LMaxIntWidth, mmPixel, mmLoEnglish) / 100;
              end;

            auCentimeter:
              begin
                LMinFloatWidth := ConvertX(LMinIntWidth, mmPixel, mmLoMetric) / 100;
                LMaxFloatWidth := ConvertX(LMaxIntWidth, mmPixel, mmLoMetric) / 100;
              end;

            auPoint:
              begin
                LInches        := ConvertX(LMinIntWidth, mmPixel, mmLoEnglish) / 100;
                LMinFloatWidth := LInches * 72;
                LInches        := ConvertX(LMaxIntWidth, mmPixel, mmLoEnglish) / 100;
                LMaxFloatWidth := LInches * 72;
              end;
          end;
        end;

        EnsureValueInRange(LRealVal, LMinFloatWidth, LMaxFloatWidth);
      except
        case FWidthUnit of
          auInch:
            begin
              LRealVal := GetWidthByInch();
            end;

          auCentimeter:
            begin
              LRealVal := GetWidthByCM();
            end;

          auPoint:
            begin
              LRealVal := GetWidthByPoint();
            end;
        end;
        
        ShowWidth();
      end;
    end;

    case FWidthUnit of
      auInch:
        begin
          SetWidthByInch(LRealVal);
        end;
        
      auCentimeter:
        begin
          SetWidthByCM(LRealVal);
        end;
        
      auPoint:
        begin
          SetWidthByPoint(LRealVal);
        end;
        
      auPixel:
        begin
          try
            LIntVal := StrToInt(edtImageWidth.Text);
            EnsureValueInRange(LIntVal, LMinIntWidth, LMaxIntWidth);
            FNewWidth := LIntVal;
          except
            ShowWidth();
          end;
        end;
    end;

    if FConstrainProperties then
    begin
      LScale     := FNewWidth / FOriginalWidth;
      FNewHeight := Round(FOriginalHeight * LScale);
      
      ShowHeight();
    end;
  end;
end;

procedure TfrmImageSize.edtImageHeightChange(Sender: TObject);
var
  LRealVal, LInches, LScale        : Double;
  LMinFloatHeight, LMaxFloatHeight : Double;
  LIntVal                          : Integer;
  LMinIntHeight, LMaxIntHeight     : Integer;
begin
  LMinFloatHeight := 0.00;
  LMaxFloatHeight := 0.00;

  if FCanChange then
  begin
    LMinIntHeight := 16;
    LMaxIntHeight := Printer.PageHeight;

    if FHeightUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtImageHeight.Text);

        with frmPrintPreview.prntprvwPreview do
        begin
          case FHeightUnit of
            auInch:
              begin
                LMinFloatHeight := ConvertY(LMinIntHeight, mmPixel, mmLoEnglish) / 100;
                LMaxFloatHeight := ConvertY(LMaxIntHeight, mmPixel, mmLoEnglish) / 100;
              end;

            auCentimeter:
              begin
                LMinFloatHeight := ConvertY(LMinIntHeight, mmPixel, mmLoMetric) / 100;
                LMaxFloatHeight := ConvertY(LMaxIntHeight, mmPixel, mmLoMetric) / 100;
              end;

            auPoint:
              begin
                LInches         := ConvertY(LMinIntHeight, mmPixel, mmLoEnglish) / 100;
                LMinFloatHeight := LInches * 72;
                LInches         := ConvertY(LMaxIntHeight, mmPixel, mmLoEnglish) / 100;
                LMaxFloatHeight := LInches * 72;
              end;
          end;
        end;

        EnsureValueInRange(LRealVal, LMinFloatHeight, LMaxFloatHeight);
      except
        case FHeightUnit of
          auInch:
            begin
              LRealVal := GetHeightByInch();
            end;

          auCentimeter:
            begin
              LRealVal := GetHeightByCM();
            end;

          auPoint:
            begin
              LRealVal := GetHeightByPoint();
            end;
        end;
        
        ShowHeight();
      end;
    end;

    case FHeightUnit of
      auInch:
        begin
          SetHeightByInch(LRealVal);
        end;

      auCentimeter:
        begin
          SetHeightByCM(LRealVal);
        end;

      auPoint:
        begin
          SetHeightByPoint(LRealVal);
        end;
        
      auPixel:
        begin
          try
            LIntVal := StrToInt(edtImageHeight.Text);
            EnsureValueInRange(LIntVal, LMinIntHeight, LMaxIntHeight);
            FNewHeight := LIntVal;
          except
            ShowHeight();
          end;
        end;
    end;

    if FConstrainProperties then
    begin
      LScale    := FNewHeight / FOriginalHeight;
      FNewWidth := Round(FOriginalWidth * LScale);
      
      ShowWidth();
    end;
  end;
end;

procedure TfrmImageSize.chckbxConstrainPropertiesClick(Sender: TObject);
begin
  FConstrainProperties := chckbxConstrainProperties.Checked;
  imgvwChain.Visible   := FConstrainProperties;

  if chckbxConstrainProperties.Checked then
  begin
    FNewWidth  := FOriginalWidth;
    FNewHeight := FOriginalHeight;

    ShowWidth();
    ShowHeight();
  end;
end;

procedure TfrmImageSize.cmbbxResamplerChange(Sender: TObject);
begin
  FResamplingOptions.Resampler := TgmResamplerSelector(cmbbxResampler.ItemIndex);
  UpdateKernelOptionsDisplay();
end; 

procedure TfrmImageSize.cmbbxPAMChange(Sender: TObject);
begin
  FResamplingOptions.PixelAccessMode := TPixelAccessMode(cmbbxPAM.ItemIndex);
end;

procedure TfrmImageSize.cmbbxWrapModeChange(Sender: TObject);
begin
  FResamplingOptions.WrapMode := TWrapMode(cmbbxWrapMode.ItemIndex);
end; 

procedure TfrmImageSize.cmbbxKernelChange(Sender: TObject);
begin
  FResamplingOptions.Kernel := TgmKernelSelector(cmbbxKernel.ItemIndex);
end; 

procedure TfrmImageSize.cmbbxKernelModeChange(Sender: TObject);
begin
  FResamplingOptions.KernelMode := TKernelMode(cmbbxKernelMode.ItemIndex);
end; 

procedure TfrmImageSize.ggbrTableSizeChange(Sender: TObject);
begin
  FResamplingOptions.TableSize := ggbrTableSize.Position;
  lblTableSize.Caption         := Format('Table Size (%d/100):', [ggbrTableSize.Position]);
end; 

procedure TfrmImageSize.btbtnOKClick(Sender: TObject);
begin
  // save settings to ini file
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WIDTH_UNIT, IntToStr(Ord(FWidthUnit)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_HEIGHT_UNIT, IntToStr(Ord(FHeightUnit)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_RESAMPLER, IntToStr(Ord(FResamplingOptions.Resampler)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_PIXEL_ACCESS_MODE, IntToStr(Ord(FResamplingOptions.PixelAccessMode)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_WRAP_MODE, IntToStr(Ord(FResamplingOptions.WrapMode)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL, IntToStr(Ord(FResamplingOptions.Kernel)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_KERNEL_MODE, IntToStr(Ord(FResamplingOptions.KernelMode)));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_TABLE_SIZE, IntToStr(FResamplingOptions.TableSize));
  WriteInfoToIniFile(SECTION_IMAGE_SIZE_DIALOG, IDENT_IMAGE_SIZE_CONSTRAIN, IntToStr(Integer(FConstrainProperties)));
end; 

end.
