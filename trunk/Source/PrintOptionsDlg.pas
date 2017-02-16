unit PrintOptionsDlg;

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

// Update Date: 2015/10/18

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
{ Graphics32 }
  GR32_Image,
{ GraphicsMagicLib }
  gmPrintOptions,
  gmTypes;

type
  TfrmPrintOptions = class(TForm)
    pnlPrintOptionsPreview: TPanel;
    grpbxPrintPosition: TGroupBox;
    lblTopPos: TLabel;
    lblLeftPos: TLabel;
    edtTopPos: TEdit;
    edtLeftPos: TEdit;
    cmbbxTopPosUnit: TComboBox;
    cmbbxLeftPosUnit: TComboBox;
    chckbxCenterImage: TCheckBox;
    grpbxPrintScaled: TGroupBox;
    lblPrintScale: TLabel;
    lblPrintHeight: TLabel;
    lblPrintWidth: TLabel;
    edtPrintScale: TEdit;
    edtPrintHeight: TEdit;
    edtPrintWidth: TEdit;
    cmbbxPrintWidthUnit: TComboBox;
    cmbbxPrintHeightUnit: TComboBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxFitMedia: TCheckBox;
    lblScalePercent: TLabel;
    imgPrintOptionsPreview: TImage32;
    btnPageSetup: TButton;
    btnPrintImage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chckbxCenterImageClick(Sender: TObject);
    procedure edtLeftPosChange(Sender: TObject);
    procedure edtTopPosChange(Sender: TObject);
    procedure edtPrintHeightChange(Sender: TObject);
    procedure edtPrintWidthChange(Sender: TObject);
    procedure edtPrintScaleChange(Sender: TObject);
    procedure cmbbxTopPosUnitChange(Sender: TObject);
    procedure cmbbxLeftPosUnitChange(Sender: TObject);
    procedure cmbbxPrintHeightUnitChange(Sender: TObject);
    procedure cmbbxPrintWidthUnitChange(Sender: TObject);
    procedure chckbxFitMediaClick(Sender: TObject);
    procedure btnPageSetupClick(Sender: TObject);
    procedure btnPrintImageClick(Sender: TObject);
    procedure edtTopPosExit(Sender: TObject);
    procedure edtLeftPosExit(Sender: TObject);
    procedure edtPrintHeightExit(Sender: TObject);
    procedure edtPrintWidthExit(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FCanSetLeft   : Boolean;
    FCanSetTop    : Boolean;
    FCanSetWidth  : Boolean;
    FCanSetHeight : Boolean;
    FCanSetScale  : Boolean;
    FTopUnit      : TgmAppliedUnit;
    FLeftUnit     : TgmAppliedUnit;
    FWidthUnit    : TgmAppliedUnit;
    FHeightUnit   : TgmAppliedUnit;
    FPrintOptions : TgmPrintOptions;

    procedure ChangeEditBackgroundColor;
    procedure ShowLeftValue;
    procedure ShowTopValue;
    procedure ShowWidthValue;
    procedure ShowHeightValue;
    procedure ShowScaleValue;
  public
    procedure ShowPrintThumbnial;

    property PrintOptions : TgmPrintOptions read FPrintOptions;
  end;

var
  frmPrintOptions: TfrmPrintOptions;

implementation

uses
{ Graphics32 }
  GR32,
{ externals }
  Preview,
{ GraphicsMagic Lib }
  gmGUIFuncs,
  gmImageProcessFuncs,
  gmIni,
  gmMath,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PrintPreviewDlg;

const
  PREVIEW_WIDTH  = 192;
  PREVIEW_HEIGHT = 256;

{$R *.DFM}

procedure TfrmPrintOptions.ChangeEditBackgroundColor;
begin
  if edtTopPos.Enabled then
  begin
    edtTopPos.Color := clWhite;
  end
  else
  begin
    edtTopPos.Color := clBtnFace;
  end;

  if edtLeftPos.Enabled then
  begin
    edtLeftPos.Color := clWhite;
  end
  else
  begin
    edtLeftPos.Color := clBtnFace;
  end;

  if edtPrintScale.Enabled then
  begin
    edtPrintScale.Color := clWhite;
  end
  else
  begin
    edtPrintScale.Color := clBtnFace;
  end;

  if edtPrintHeight.Enabled then
  begin
    edtPrintHeight.Color := clWhite;
  end
  else
  begin
    edtPrintHeight.Color := clBtnFace;
  end;

  if edtPrintWidth.Enabled then
  begin
    edtPrintWidth.Color := clWhite;
  end
  else
  begin
    edtPrintWidth.Color := clBtnFace;
  end;
end; 

procedure TfrmPrintOptions.ShowLeftValue;
begin
  FCanSetLeft := False;
  try
    case FLeftUnit of
      auInch:
        begin
          edtLeftPos.Text := FloatToStr(FPrintOptions.LeftInch);
        end;
        
      auCentimeter:
        begin
          edtLeftPos.Text := FloatToStr(FPrintOptions.LeftCM);
        end;

      auPoint:
        begin
          edtLeftPos.Text := FloatToStr(FPrintOptions.LeftPoint);
        end;

      auPixel:
        begin
          edtLeftPos.Text := IntToStr(FPrintOptions.LeftPixel);
        end;
    end;
  finally
    FCanSetLeft := True;
  end;
end; 

procedure TfrmPrintOptions.ShowTopValue;
begin
  FCanSetTop := False;
  try
    case FTopUnit of
      auInch:
        begin
          edtTopPos.Text := FloatToStr(FPrintOptions.TopInch);
        end;

      auCentimeter:
        begin
          edtTopPos.Text := FloatToStr(FPrintOptions.TopCM);
        end;

      auPoint:
        begin
          edtTopPos.Text := FloatToStr(FPrintOptions.TopPoint);
        end;

      auPixel:
        begin
          edtTopPos.Text := IntToStr(FPrintOptions.TopPixel);
        end;
    end;
  finally
    FCanSetTop := True;
  end;
end; 

procedure TfrmPrintOptions.ShowWidthValue;
begin
  FCanSetWidth := False;
  try
    case FWidthUnit of
      auInch:
        begin
          edtPrintWidth.Text := FloatToStr(FPrintOptions.WidthInch);
        end;
        
      auCentimeter:
        begin
          edtPrintWidth.Text := FloatToStr(FPrintOptions.WidthCM);
        end;

      auPoint:
        begin
          edtPrintWidth.Text := FloatToStr(FPrintOptions.WidthPoint);
        end;

      auPixel:
        begin
          edtPrintWidth.Text := IntToStr(FPrintOptions.WidthPixel);
        end;
    end;
  finally
    FCanSetWidth := True;
  end;
end; 

procedure TfrmPrintOptions.ShowHeightValue;
begin
  FCanSetHeight := False;
  try
    case FHeightUnit of
      auInch:
        begin
          edtPrintHeight.Text := FloatToStr(FPrintOptions.HeightInch);
        end;

      auCentimeter:
        begin
          edtPrintHeight.Text := FloatToStr(FPrintOptions.HeightCM);
        end;

      auPoint:
        begin
          edtPrintHeight.Text := FloatToStr(FPrintOptions.HeightPoint);
        end;

      auPixel:
        begin
          edtPrintHeight.Text := IntToStr(FPrintOptions.HeightPixel);
        end;
    end;
  finally
    FCanSetHeight := True;
  end;
end;

procedure TfrmPrintOptions.ShowScaleValue;
begin
  FCanSetScale := False;
  try
    edtPrintScale.Text := FloatToStr(FPrintOptions.Scale);
  finally
    FCanSetScale := True;
  end;
end; 

procedure TfrmPrintOptions.ShowPrintThumbnial;
var
  LTempBitmap : TBitmap32;
begin
  if ActiveChildForm.LayerList.Count > 1 then
  begin
    FPrintOptions.SourceBitmap.Assign(ActiveChildForm.LayerList.CombineResult);
  end
  else
  begin
    LTempBitmap := TBitmap32.Create();
    try
      LTempBitmap.Assign(ActiveChildForm.LayerList.SelectedLayer.LayerBitmap);
      LTempBitmap.DrawMode := dmBlend;

      // convert the transparent area to white
      MergeBitmapToColoredBackground(LTempBitmap, clWhite32);
      FPrintOptions.SourceBitmap.Assign(LTempBitmap);
    finally
      LTempBitmap.Free;
    end;
  end;

  FPrintOptions.SourceBitmap.PixelFormat := pf24bit;

  FPrintOptions.SetPaperSize(frmPrintPreview.prntprvwPreview.PaperWidth,
                             frmPrintPreview.prntprvwPreview.PaperHeight);
                             
  imgPrintOptionsPreview.Bitmap.Assign(FPrintOptions.Paper);

  ScaleImage32(imgPrintOptionsPreview.Bitmap,
               imgPrintOptionsPreview,
               PREVIEW_WIDTH, PREVIEW_HEIGHT);
               
  CenterImageInPanel(pnlPrintOptionsPreview, imgPrintOptionsPreview);

  if FPrintOptions.IsFitToMedia then
  begin
    FPrintOptions.SetFitToMedia(True);
  end;
  
  FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
end; 

procedure TfrmPrintOptions.FormCreate(Sender: TObject);
begin
  FPrintOptions := TgmPrintOptions.Create();

  FCanSetLeft   := True;
  FCanSetTop    := True;
  FCanSetWidth  := True;
  FCanSetHeight := True;
  FCanSetScale  := True;

  FTopUnit    := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_TOP_UNIT,    '1')));
  FLeftUnit   := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_LEFT_UNIT,   '1')));
  FWidthUnit  := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_WIDTH_UNIT,  '1')));
  FHeightUnit := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_HEIGHT_UNIT, '1')));

  cmbbxTopPosUnit.ItemIndex      := Ord(FTopUnit);
  cmbbxLeftPosUnit.ItemIndex     := Ord(FLeftUnit);
  cmbbxPrintWidthUnit.ItemIndex  := Ord(FWidthUnit);
  cmbbxPrintHeightUnit.ItemIndex := Ord(FHeightUnit);
  edtTopPos.Enabled              := not chckbxCenterImage.Checked;
  edtLeftPos.Enabled             := not chckbxCenterImage.Checked;
  chckbxCenterImage.Enabled      := not chckbxFitMedia.Checked;

  ChangeEditBackgroundColor();
end;

procedure TfrmPrintOptions.FormDestroy(Sender: TObject);
begin
  FPrintOptions.Free();
end; 

procedure TfrmPrintOptions.FormShow(Sender: TObject);
begin
  ShowPrintThumbnial();
  ShowTopValue();
  ShowLeftValue();
  ShowScaleValue();
  ShowWidthValue();
  ShowHeightValue();

  chckbxCenterImage.Checked := FPrintOptions.IsCenterImage;
end; 

procedure TfrmPrintOptions.chckbxCenterImageClick(Sender: TObject);
begin
  FPrintOptions.IsCenterImage := chckbxCenterImage.Checked;
  edtTopPos.Enabled           := not chckbxCenterImage.Checked;
  edtLeftPos.Enabled          := not chckbxCenterImage.Checked;

  ChangeEditBackgroundColor();

  if chckbxCenterImage.Checked then
  begin
    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
    ShowLeftValue();
    ShowTopValue();
  end;
end; 

procedure TfrmPrintOptions.edtLeftPosChange(Sender: TObject);
var
  LRealVal : Double;
begin
  LRealVal := 0.00;
  
  if FCanSetLeft then
  begin
    if FLeftUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtLeftPos.Text);
      except
        case FTopUnit of
          auInch:
            begin
              LRealVal := FPrintOptions.LeftInch;
            end;

          auCentimeter:
            begin
              LRealVal := FPrintOptions.LeftCM;
            end;

          auPoint:
            begin
              LRealVal := FPrintOptions.LeftPoint;
            end;
        end;

        ShowLeftValue();
      end;
    end;

    case FLeftUnit of
      auInch:
        begin
          FPrintOptions.LeftInch := LRealVal;
        end;
        
      auCentimeter:
        begin
          FPrintOptions.LeftCM := LRealVal;
        end;

      auPoint:
        begin
          FPrintOptions.LeftPoint := LRealVal;
        end;
        
      auPixel:
        begin
          try
            FPrintOptions.LeftPixel := StrToInt(edtLeftPos.Text);
          except
            ShowLeftValue();
          end;
        end;
    end;
    
    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
  end;
end; 

procedure TfrmPrintOptions.edtTopPosChange(Sender: TObject);
var
  LRealVal: Double;
begin
  LRealVal := 0.00;
  
  if FCanSetTop then
  begin
    if FTopUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtTopPos.Text);
      except
        case FTopUnit of
          auInch:
            begin
              LRealVal := FPrintOptions.TopInch;
            end;

          auCentimeter:
            begin
              LRealVal := FPrintOptions.TopCM;
            end;

          auPoint:
            begin
              LRealVal := FPrintOptions.TopPoint;
            end;
        end;
        
        ShowTopValue();
      end;
    end;

    case FTopUnit of
      auInch:
        begin
          FPrintOptions.TopInch := LRealVal;
        end;

      auCentimeter:
        begin
          FPrintOptions.TopCM := LRealVal;
        end;

      auPoint:
        begin
          FPrintOptions.TopPoint := LRealVal;
        end;
        
      auPixel:
        begin
          try
            FPrintOptions.TopPixel := StrToInt(edtTopPos.Text);
          except
            ShowTopValue();
          end;
        end;
    end;
    
    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
  end;
end; 

procedure TfrmPrintOptions.edtPrintHeightChange(Sender: TObject);
var
  LRealVal: Double;
begin
  LRealVal := 0.00;
    
  if FCanSetHeight then
  begin
    if FHeightUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtPrintHeight.Text);
      except
        case FTopUnit of
          auInch:
            begin
              LRealVal := FPrintOptions.HeightInch;
            end;

          auCentimeter:
            begin
              LRealVal := FPrintOptions.HeightCM;
            end;

          auPoint:
            begin
              LRealVal := FPrintOptions.HeightPoint;
            end;
        end;
        
        ShowHeightValue();
      end;
    end;

    case FHeightUnit of
      auInch:
        begin
          FPrintOptions.HeightInch := LRealVal;
        end;

      auCentimeter:
        begin
          FPrintOptions.HeightCM := LRealVal;
        end;

      auPoint:
        begin
          FPrintOptions.HeightPoint := LRealVal;
        end;
        
      auPixel:
        begin
          try
            FPrintOptions.HeightPixel := StrToInt(edtPrintHeight.Text);
          except
            ShowHeightValue();
          end;
        end;
    end;

    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);

    // We calculated the new coordinates when drawing bitmap, so we need to
    // show respective information after drawing. 
    if chckbxCenterImage.Checked then
    begin
      ShowTopValue();
    end;

    ShowWidthValue();
    ShowScaleValue();
  end;
end; 

procedure TfrmPrintOptions.edtPrintWidthChange(Sender: TObject);
var
  LRealVal: Double;
begin
  LRealVal := 0.00;
  
  if FCanSetWidth then
  begin
    if FWidthUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtPrintWidth.Text);
      except
        case FTopUnit of
          auInch:
            begin
              LRealVal := FPrintOptions.WidthInch;
            end;

          auCentimeter:
            begin
              LRealVal := FPrintOptions.WidthCM;
            end;

          auPoint:
            begin
              LRealVal := FPrintOptions.WidthPoint;
            end;
        end;

        ShowWidthValue();
      end;
    end;

    case FWidthUnit of
      auInch:
        begin
          FPrintOptions.WidthInch := LRealVal;
        end;
        
      auCentimeter:
        begin
          FPrintOptions.WidthCM := LRealVal;
        end;

      auPoint:
        begin
          FPrintOptions.WidthPoint := LRealVal;
        end;
        
      auPixel:
        begin
          try
            FPrintOptions.WidthPixel := StrToInt(edtPrintWidth.Text);
          except
            ShowWidthValue();
          end;
        end;
    end;

    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);

    // We calculated the new coordinates when drawing bitmap, so we need to
    // show respective information after drawing. 
    if chckbxCenterImage.Checked then
    begin
      ShowLeftValue();
    end;

    ShowHeightValue();
    ShowScaleValue();
  end;
end; 

procedure TfrmPrintOptions.edtPrintScaleChange(Sender: TObject);
var
  LScale: Double;
begin
  if FCanSetScale then
  begin
    try
      LScale := StrToFloat(edtPrintScale.Text);
      EnsureValueInRange(LScale, 1.0, 1000.0);

      FPrintOptions.Scale := LScale;
      FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
      ShowScaleValue();
    except
      ShowScaleValue();
    end;
    
    if chckbxCenterImage.Checked then
    begin
      ShowTopValue();
      ShowLeftValue();
    end;

    ShowHeightValue();
    ShowWidthValue();
  end;
end;

procedure TfrmPrintOptions.cmbbxTopPosUnitChange(Sender: TObject);
begin
  FTopUnit := TgmAppliedUnit(cmbbxTopPosUnit.ItemIndex);
  ShowTopValue();
end; 

procedure TfrmPrintOptions.cmbbxLeftPosUnitChange(Sender: TObject);
begin
  FLeftUnit := TgmAppliedUnit(cmbbxLeftPosUnit.ItemIndex);
  ShowLeftValue();
end;

procedure TfrmPrintOptions.cmbbxPrintHeightUnitChange(Sender: TObject);
begin
  FHeightUnit := TgmAppliedUnit(cmbbxPrintHeightUnit.ItemIndex);
  ShowHeightValue();
end;

procedure TfrmPrintOptions.cmbbxPrintWidthUnitChange(Sender: TObject);
begin
  FWidthUnit := TgmAppliedUnit(cmbbxPrintWidthUnit.ItemIndex);
  ShowWidthValue();
end;

procedure TfrmPrintOptions.chckbxFitMediaClick(Sender: TObject);
begin
  FPrintOptions.IsFitToMedia  := chckbxFitMedia.Checked;
  chckbxCenterImage.Checked   := True;
  chckbxCenterImage.Enabled   := not chckbxFitMedia.Checked;
  FPrintOptions.IsCenterImage := chckbxCenterImage.Checked;
  edtTopPos.Enabled           := not chckbxCenterImage.Checked;
  edtLeftPos.Enabled          := not chckbxCenterImage.Checked;
  edtPrintHeight.Enabled      := not chckbxFitMedia.Checked;
  edtPrintWidth.Enabled       := not chckbxFitMedia.Checked;
  edtPrintScale.Enabled       := not chckbxFitMedia.Checked;

  ChangeEditBackgroundColor();

  if chckbxFitMedia.Checked then
  begin
    FPrintOptions.DrawPrintBitmap(imgPrintOptionsPreview.Bitmap);
    ShowTopValue();
    ShowLeftValue();
    ShowHeightValue();
    ShowWidthValue();
    ShowScaleValue();
  end;
end; 

procedure TfrmPrintOptions.btnPageSetupClick(Sender: TObject);
begin
  if dmMain.PrinterSetupDialog.Execute() then
  begin
    // The following routines must be called after the printer is set properly,
    // and these routines could make the print preview take effect. 
    frmPrintPreview.prntprvwPreview.BeginDoc();
    frmPrintPreview.prntprvwPreview.EndDoc();

    ShowPrintThumbnial();
    ShowTopValue();
    ShowLeftValue();
    ShowScaleValue();
    ShowWidthValue();
    ShowHeightValue();
  end;
end; 

procedure TfrmPrintOptions.btnPrintImageClick(Sender: TObject);
begin
  frmPrintPreview.DrawBitmapOnPreview();
  
  if frmPrintPreview.prntprvwPreview.State = psReady then
  begin
    if dmMain.PrintDialog.Execute() then
    begin
      frmPrintPreview.prntprvwPreview.Print();
    end;
  end;
end;

procedure TfrmPrintOptions.edtTopPosExit(Sender: TObject);
begin
  ShowTopValue;
end;

procedure TfrmPrintOptions.edtLeftPosExit(Sender: TObject);
begin
  ShowLeftValue();
end;

procedure TfrmPrintOptions.edtPrintHeightExit(Sender: TObject);
begin
  ShowHeightValue();
end;

procedure TfrmPrintOptions.edtPrintWidthExit(Sender: TObject);
begin
  ShowWidthValue();
end;

procedure TfrmPrintOptions.btbtnOKClick(Sender: TObject);
begin
  WriteInfoToIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_TOP_UNIT,    IntToStr(Ord(FTopUnit)));
  WriteInfoToIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_LEFT_UNIT,   IntToStr(Ord(FLeftUnit)));
  WriteInfoToIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_WIDTH_UNIT,  IntToStr(Ord(FWidthUnit)));
  WriteInfoToIniFile(SECTION_PRINT_OPTIONS_DIALOG, IDENT_PRINT_OPTIONS_HEIGHT_UNIT, IntToStr(Ord(FHeightUnit)));
end;

end.
