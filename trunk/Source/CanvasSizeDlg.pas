unit CanvasSizeDlg;

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

// Update Date: 2015/08/20

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ImgList,
{ GraphicsMagicLib }
  gmTypes;

type
  TfrmCanvasSize = class(TForm)
    grpbxCurrentSize: TGroupBox;
    lblCurrentWidth: TLabel;
    lblCurrentHeight: TLabel;
    lblCurrentWidthValue: TLabel;
    lblCurrentHeightValue: TLabel;
    grpbxNewSize: TGroupBox;
    lblNewWidth: TLabel;
    lblNewHeight: TLabel;
    edtNewWidth: TEdit;
    edtNewHeight: TEdit;
    cmbbxNewWidthUnit: TComboBox;
    cmbbxNewHeightUnit: TComboBox;
    lblAnchor: TLabel;
    Panel1: TPanel;
    spdbtnTopLeft: TSpeedButton;
    spdbtnTop: TSpeedButton;
    spdbtnTopRight: TSpeedButton;
    spdbtnLeft: TSpeedButton;
    spdbtnCenter: TSpeedButton;
    spdbtnRight: TSpeedButton;
    spdbtnBottomLeft: TSpeedButton;
    spdbtnBottom: TSpeedButton;
    spdbtnBottomRight: TSpeedButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ChangeAnchorDirectionGlyphs(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtNewWidthChange(Sender: TObject);
    procedure edtNewHeightChange(Sender: TObject);
    procedure cmbbxNewWidthUnitChange(Sender: TObject);
    procedure cmbbxNewHeightUnitChange(Sender: TObject);
    procedure edtNewWidthExit(Sender: TObject);
    procedure edtNewHeightExit(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FCanChange       : Boolean;
    FCurrentWidth    : Integer;
    FCurrentHeight   : Integer;
    FNewWidth        : Integer;
    FNewHeight       : Integer;
    FWidthUnit       : TgmAppliedUnit;
    FHeightUnit      : TgmAppliedUnit;
    FAnchorDirection : TgmAnchorDirection;

    function GetWidthInch: Double;
    function GetWidthCM: Double;
    function GetWidthPoint: Double;
    function GetHeightInch: Double;
    function GetHeightCM: Double;
    function GetHeightPoint: Double;

    procedure SetWidthInch(const AWidth: Double);
    procedure SetWidthCM(const AWidth: Double);
    procedure SetWidthPoint(const AWidth: Double);
    procedure SetHeightInch(const AHeight: Double);
    procedure SetHeightCM(const AHeight: Double);
    procedure SetHeightPoint(const AHeight: Double);

    procedure ShowWidth;
    procedure ShowHeight;
  public
    property AnchorDirection : TgmAnchorDirection read FAnchorDirection;
    property NewWidth        : Integer            read FNewWidth;
    property NewHeight       : Integer            read FNewHeight;
  end;

var
  frmCanvasSize: TfrmCanvasSize;

implementation

uses
{ Standard }
  Printers,
{ Graphics32 }
  GR32_Layers,
{ externals -- Preview }
  Preview,
{ GraphicsMagic Lib }
  gmIni,
  gmMath,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PrintPreviewDlg;

{$R *.DFM}

//-- Custom procedures and functions -------------------------------------------

function TfrmCanvasSize.GetWidthInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoEnglish) / 100;
end;

function TfrmCanvasSize.GetWidthCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoMetric) / 100;
end;

function TfrmCanvasSize.GetWidthPoint: Double;
var
  LInches : Double;
begin
  LInches := frmPrintPreview.prntprvwPreview.ConvertX(FNewWidth, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

function TfrmCanvasSize.GetHeightInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoEnglish) / 100;
end;

function TfrmCanvasSize.GetHeightCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoMetric) / 100;
end;

function TfrmCanvasSize.GetHeightPoint: Double;
var
  LInches : Double;
begin
  LInches := frmPrintPreview.prntprvwPreview.ConvertY(FNewHeight, mmPixel, mmLoEnglish) / 100;
  Result  := LInches * 72;
end;

procedure TfrmCanvasSize.SetWidthInch(const AWidth: Double);
var
  w : Integer;
begin
  w         := Round(AWidth * 100); // convert to mmLoEnglish unit
  FNewWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoEnglish, mmPixel);
end;

procedure TfrmCanvasSize.SetWidthCM(const AWidth: Double);
var
  w : Integer;
begin
  w         := Round(AWidth * 100); // convert to mmLoMetric unit
  FNewWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoMetric, mmPixel);
end;

procedure TfrmCanvasSize.SetWidthPoint(const AWidth: Double);
var
  LLoEnglishVal : Integer;
begin
  LLoEnglishVal := Round(AWidth / 72 * 100); // convert to mmLoEnglish unit
  FNewWidth     := frmPrintPreview.prntprvwPreview.ConvertX(LLoEnglishVal, mmLoEnglish, mmPixel);
end; 

procedure TfrmCanvasSize.SetHeightInch(const AHeight: Double);
var
  h : Integer;
begin
  h          := Round(AHeight * 100); // convert to mmLoEnglish unit
  FNewHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoEnglish, mmPixel);
end;

procedure TfrmCanvasSize.SetHeightCM(const AHeight: Double);
var
  h : Integer;
begin
  h          := Round(AHeight * 100); // convert to mmLoMetric unit
  FNewHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoMetric, mmPixel);
end;

procedure TfrmCanvasSize.SetHeightPoint(const AHeight: Double);
var
  LLoEnglishVal : Integer;
begin
  LLoEnglishVal := Round(AHeight / 72 * 100); // convert to mmLoEnglish unit
  FNewHeight    := frmPrintPreview.prntprvwPreview.ConvertY(LLoEnglishVal, mmLoEnglish, mmPixel);
end;

procedure TfrmCanvasSize.ShowWidth;
begin
  FCanChange := False;
  try
    case FWidthUnit of
      auInch:
        begin
          edtNewWidth.Text := FloatToStr(GetWidthInch);
        end;

      auCentimeter:
        begin
          edtNewWidth.Text := FloatToStr(GetWidthCM);
        end;

      auPoint:
        begin
          edtNewWidth.Text := FloatToStr(GetWidthPoint);
        end;
        
      auPixel:
        begin
          edtNewWidth.Text := IntToStr(FNewWidth);
        end;
    end;
  finally
    FCanChange := True;
  end;
end; 

procedure TfrmCanvasSize.ShowHeight;
begin
  FCanChange := False;
  try
    case FHeightUnit of
      auInch:
        begin
          edtNewHeight.Text := FloatToStr(GetHeightInch);
        end;
        
      auCentimeter:
        begin
          edtNewHeight.Text := FloatToStr(GetHeightCM);
        end;
        
      auPoint:
        begin
          edtNewHeight.Text := FloatToStr(GetHeightPoint);
        end;
        
      auPixel:
        begin
          edtNewHeight.Text := IntToStr(FNewHeight);
        end;
    end;
  finally
    FCanChange := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmCanvasSize.FormCreate(Sender: TObject);
begin
  FCanChange       := True;
  FAnchorDirection := adCenter;
  FCurrentWidth    := 0;
  FCurrentHeight   := 0;
  FNewWidth        := 0;
  FNewHeight       := 0;
  FWidthUnit       := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_WIDTH_UNIT,  '1')));
  FHeightUnit      := TgmAppliedUnit(StrToInt(ReadInfoFromIniFile(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_HEIGHT_UNIT, '1')));

  cmbbxNewWidthUnit.ItemIndex  := Ord(FWidthUnit);
  cmbbxNewHeightUnit.ItemIndex := Ord(FHeightUnit);
end;

procedure TfrmCanvasSize.ChangeAnchorDirectionGlyphs(Sender: TObject);
begin
  if Sender = spdbtnTopLeft then
  begin
    FAnchorDirection := adTopLeft;
  end
  else if Sender = spdbtnTop then
  begin
    FAnchorDirection := adTop;
  end
  else if Sender = spdbtnTopRight then
  begin
    FAnchorDirection := adTopRight;
  end
  else if Sender = spdbtnLeft then
  begin
    FAnchorDirection := adLeft;
  end
  else if Sender = spdbtnCenter then
  begin
    FAnchorDirection := adCenter;
  end
  else if Sender = spdbtnRight then
  begin
    FAnchorDirection := adRight;
  end
  else if Sender = spdbtnBottomLeft then
  begin
    FAnchorDirection := adBottomLeft;
  end
  else if Sender = spdbtnBottom then
  begin
    FAnchorDirection := adBottom;
  end
  else if Sender = spdbtnBottomRight then
  begin
    FAnchorDirection := adBottomRight;
  end;
end;

procedure TfrmCanvasSize.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FCurrentWidth                 := ActiveChildForm.imgWorkArea.Bitmap.Width;
    FCurrentHeight                := ActiveChildForm.imgWorkArea.Bitmap.Height;
    lblCurrentWidthValue.Caption  := IntToStr(FCurrentWidth)  + ' Pixels';
    lblCurrentHeightValue.Caption := IntToStr(FCurrentHeight) + ' Pixels';

    FNewWidth  := FCurrentWidth;
    FNewHeight := FCurrentHeight;
    
    ShowWidth();
    ShowHeight();
    
    ActiveControl := edtNewWidth;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmCanvasSize.edtNewWidthChange(Sender: TObject);
var
  LRealVal       : Double;
  LInches        : Double;
  LMinFloatWidth : Double;
  LMaxFloatWidth : Double;
  LIntVal        : Integer;
  LMinIntWidth   : Integer;
  LMaxIntWidth   : Integer;
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
        LRealVal := StrToFloat(edtNewWidth.Text);
        
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
              LRealVal := GetWidthInch();
            end;

          auCentimeter:
            begin
              LRealVal := GetWidthCM();
            end;

          auPoint:
            begin
              LRealVal := GetWidthPoint();
            end;
        end;
        
        ShowWidth();
      end;
    end;

    case FWidthUnit of
      auInch:
        begin
          SetWidthInch(LRealVal);
        end;
        
      auCentimeter:
        begin
          SetWidthCM(LRealVal);
        end;
        
      auPoint:
        begin
          SetWidthPoint(LRealVal);
        end;
        
      auPixel:
        begin
          try
            LIntVal := StrToInt(edtNewWidth.Text);

            EnsureValueInRange(LIntVal, LMinIntWidth, LMaxIntWidth);

            FNewWidth := LIntVal;
          except
            ShowWidth();
          end;
        end;
    end;
  end;
end;

procedure TfrmCanvasSize.edtNewHeightChange(Sender: TObject);
var
  LRealVal        : Double;
  LInches         : Double;
  LMinFloatHeight : Double;
  LMaxFloatHeight : Double;
  LIntVal         : Integer;
  LMinIntHeight   : Integer;
  LMaxIntHeight   : Integer;
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
        LRealVal := StrToFloat(edtNewHeight.Text);
        
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
              LRealVal := GetHeightInch();
            end;

          auCentimeter:
            begin
              LRealVal := GetHeightCM();
            end;
            
          auPoint:
            begin
              LRealVal := GetHeightPoint();
            end;
        end;
        
        ShowHeight();
      end;
    end;

    case FHeightUnit of
      auInch:
        begin
          SetHeightInch(LRealVal);
        end;
        
      auCentimeter:
        begin
          SetHeightCM(LRealVal);
        end;
        
      auPoint:
        begin
          SetHeightPoint(LRealVal);
        end;
        
      auPixel:
        begin
          try
            LIntVal := StrToInt(edtNewHeight.Text);

            EnsureValueInRange(LIntVal, LMinIntHeight, LMaxIntHeight);

            FNewHeight := LIntVal;
          except
            ShowHeight();
          end;
        end;
    end;
  end;
end;

procedure TfrmCanvasSize.cmbbxNewWidthUnitChange(Sender: TObject);
begin
  FWidthUnit := TgmAppliedUnit(cmbbxNewWidthUnit.ItemIndex);
  ShowWidth();
end;

procedure TfrmCanvasSize.cmbbxNewHeightUnitChange(Sender: TObject);
begin
  FHeightUnit := TgmAppliedUnit(cmbbxNewHeightUnit.ItemIndex);
  ShowHeight();
end;

procedure TfrmCanvasSize.edtNewWidthExit(Sender: TObject);
begin
  ShowWidth();
end; 

procedure TfrmCanvasSize.edtNewHeightExit(Sender: TObject);
begin
  ShowHeight();
end;

procedure TfrmCanvasSize.btbtnOKClick(Sender: TObject);
begin
  // save settings in INI file
  WriteInfoToIniFile(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_WIDTH_UNIT,  IntToStr(Ord(FWidthUnit)));
  WriteInfoToIniFile(SECTION_CANVAS_SIZE_DIALOG, IDENT_CANVAS_SIZE_HEIGHT_UNIT, IntToStr(Ord(FHeightUnit)));
end; 

end.
