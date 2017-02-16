{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit NewFileDlg;

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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Math, gmTypes;

type
  TfrmCreateNewFile = class(TForm)
    grpbxBitmapDimension: TGroupBox;
    lblBitmapWidth: TLabel;
    lblBitmapHeight: TLabel;
    lblResolution: TLabel;
    edtBitmapWidth: TEdit;
    edtBitmapHeight: TEdit;
    edtResolution: TEdit;
    cmbbxWidthUnit: TComboBox;
    cmbbxHeightUnit: TComboBox;
    cmbbxResolutionUnit: TComboBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtBitmapWidthChange(Sender: TObject);
    procedure edtBitmapWidthExit(Sender: TObject);
    procedure edtBitmapHeightChange(Sender: TObject);
    procedure edtBitmapHeightExit(Sender: TObject);
    procedure cmbbxWidthUnitChange(Sender: TObject);
    procedure cmbbxHeightUnitChange(Sender: TObject);
    procedure cmbbxResolutionUnitChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
  private
    FCanChange     : Boolean;
    FBitmapWidth   : Integer;
    FBitmapHeight  : Integer;
    FWidthUnit     : TgmAppliedUnit;
    FHeightUnit    : TgmAppliedUnit;
    FResolutionUnit: TgmResolutionUnit;

    function GetBitmapWidthByInch: Double;
    function GetBitmapWidthByCM: Double;
    function GetBitmapWidthByPoint: Double;
    function GetBitmapHeightByInch: Double;
    function GetBitmapHeightByCM: Double;
    function GetBitmapHeightByPoint: Double;

    procedure SetBitmapWidthByInch(const AWidth: Double);
    procedure SetBitmapWidthByCM(const AWidth: Double);
    procedure SetBitmapWidthByPoint(const AWidth: Double);
    procedure SetBitmapHeightByInch(const AHeight: Double);
    procedure SetBitmapHeightByCM(const AHeight: Double);
    procedure SetBitmapHeightByPoint(const AHeight: Double);

    procedure ShowBitmapWidth;
    procedure ShowBitmapHeight;
    procedure ShowResolution;
    procedure SaveSettings;
  public
    property BitmapWidth : Integer read FBitmapWidth;
    property BitmapHeight: Integer read FBitmapHeight;
  end;

var
  frmCreateNewFile: TfrmCreateNewFile;

implementation

uses
{ Standard }
  Printers,
{ externals -- Preview }
  Preview,
{ GraphicsMagic Lib }
  gmMath,
  gmIni,
{ GraphicsMagic Forms/Dialogs }
  PrintPreviewDlg;

{$R *.DFM}

//-- Custom procedures and functions -------------------------------------------

function TfrmCreateNewFile.GetBitmapWidthByInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FBitmapWidth, mmPixel, mmLoEnglish) / 100;
end; 

function TfrmCreateNewFile.GetBitmapWidthByCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertX(FBitmapWidth, mmPixel, mmLoMetric) / 100;
end;

function TfrmCreateNewFile.GetBitmapWidthByPoint: Double;
var
  Inches: Double;
begin
  Inches := frmPrintPreview.prntprvwPreview.ConvertX(FBitmapWidth, mmPixel, mmLoEnglish) / 100;
  Result := Inches * 72;
end;

function TfrmCreateNewFile.GetBitmapHeightByInch: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FBitmapHeight, mmPixel, mmLoEnglish) / 100;
end;

function TfrmCreateNewFile.GetBitmapHeightByCM: Double;
begin
  Result := frmPrintPreview.prntprvwPreview.ConvertY(FBitmapHeight, mmPixel, mmLoMetric) / 100;
end;

function TfrmCreateNewFile.GetBitmapHeightByPoint: Double;
var
  Inches: Double;
begin
  Inches := frmPrintPreview.prntprvwPreview.ConvertY(FBitmapHeight, mmPixel, mmLoEnglish) / 100;
  Result := Inches * 72;
end;

procedure TfrmCreateNewFile.SetBitmapWidthByInch(const AWidth: Double);
var
  w: Integer;
begin
  w            := Round(AWidth * 100); // convert to mmLoEnglish unit
  FBitmapWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoEnglish, mmPixel);
end;

procedure TfrmCreateNewFile.SetBitmapWidthByCM(const AWidth: Double);
var
  w: Integer;
begin
  w            := Round(AWidth * 100); // convert to mmLoMetric unit
  FBitmapWidth := frmPrintPreview.prntprvwPreview.ConvertX(w, mmLoMetric, mmPixel);
end;

procedure TfrmCreateNewFile.SetBitmapWidthByPoint(const AWidth: Double);
var
  LoEnglishVal: Integer;
begin
  LoEnglishVal  := Round(AWidth / 72 * 100); // convert to mmLoEnglish unit
  FBitmapWidth  := frmPrintPreview.prntprvwPreview.ConvertX(LoEnglishVal, mmLoEnglish, mmPixel);
end;

procedure TfrmCreateNewFile.SetBitmapHeightByInch(const AHeight: Double);
var
  h: Integer;
begin
  h             := Round(AHeight * 100); // convert to mmLoEnglish unit
  FBitmapHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoEnglish, mmPixel);
end;

procedure TfrmCreateNewFile.SetBitmapHeightByCM(const AHeight: Double);
var
  h: Integer;
begin
  h             := Round(AHeight * 100); // convert to mmLoMetric unit
  FBitmapHeight := frmPrintPreview.prntprvwPreview.ConvertY(h, mmLoMetric, mmPixel);
end;

procedure TfrmCreateNewFile.SetBitmapHeightByPoint(const AHeight: Double);
var
  LoEnglishVal: Integer;
begin
  LoEnglishVal  := Round(AHeight / 72 * 100); // convert to mmLoEnglish unit
  FBitmapHeight := frmPrintPreview.prntprvwPreview.ConvertY(LoEnglishVal, mmLoEnglish, mmPixel);
end;

procedure TfrmCreateNewFile.ShowBitmapWidth;
begin
  FCanChange := False;
  try
    case FWidthUnit of
      auInch      : edtBitmapWidth.Text := FloatToStr(GetBitmapWidthByInch);
      auCentimeter: edtBitmapWidth.Text := FloatToStr(GetBitmapWidthByCM);
      auPoint     : edtBitmapWidth.Text := FloatToStr(GetBitmapWidthByPoint);
      auPixel     : edtBitmapWidth.Text := IntToStr(FBitmapWidth);
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmCreateNewFile.ShowBitmapHeight;
begin
  FCanChange := False;
  try
    case FHeightUnit of
      auInch      : edtBitmapHeight.Text := FloatToStr(GetBitmapHeightByInch);
      auCentimeter: edtBitmapHeight.Text := FloatToStr(GetBitmapHeightByCM);
      auPoint     : edtBitmapHeight.Text := FloatToStr(GetBitmapHeightByPoint);
      auPixel     : edtBitmapHeight.Text := IntToStr(FBitmapHeight);
    end;
  finally
    FCanChange := True;
  end;
end;

procedure TfrmCreateNewFile.ShowResolution;
begin
  case FResolutionUnit of
    ruPixelsPerInch: edtResolution.Text := IntToStr(Screen.PixelsPerInch);
    ruPixelsPerCM  : edtResolution.Text := FloatToStr( RoundTo(Screen.PixelsPerInch / 2.54, -3) );
  end;
end; 

procedure TfrmCreateNewFile.SaveSettings;
begin
  WriteInfoToIniFile(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_WIDTH,            IntToStr(FBitmapWidth));
  WriteInfoToIniFile(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_HEIGHT,           IntToStr(FBitmapHeight));
  WriteInfoToIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_WIDTH_UNIT_INDEX,      IntToStr(Ord(FWidthUnit)));
  WriteInfoToIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_HEIGHT_UNIT_INDEX,     IntToStr(Ord(FHeightUnit)));
  WriteInfoToIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_RESOLUTION_UNIT_INDEX, IntToStr(Ord(FResolutionUnit)));
end;

//------------------------------------------------------------------------------

procedure TfrmCreateNewFile.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ShowBitmapWidth;
    ShowBitmapHeight;
    ShowResolution;
    
    ActiveControl := edtBitmapWidth;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmCreateNewFile.FormCreate(Sender: TObject);
begin
  FCanChange    := True;
  FBitmapWidth  := StrToInt(ReadInfoFromIniFile(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_WIDTH,  '640'));
  FBitmapHeight := StrToInt(ReadInfoFromIniFile(SECTION_NEW_FILE_DIALOG, IDENT_DEFAULT_IMAGE_HEIGHT, '480'));

  cmbbxWidthUnit.ItemIndex      := StrToInt(ReadInfoFromIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_WIDTH_UNIT_INDEX,      '1'));
  cmbbxHeightUnit.ItemIndex     := StrToInt(ReadInfoFromIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_HEIGHT_UNIT_INDEX,     '1'));
  cmbbxResolutionUnit.ItemIndex := StrToInt(ReadInfoFromIniFile(SECTION_NEW_FILE_DIALOG, IDENT_NEW_FILE_RESOLUTION_UNIT_INDEX, '0'));

  FWidthUnit      := TgmAppliedUnit(cmbbxWidthUnit.ItemIndex);
  FHeightUnit     := TgmAppliedUnit(cmbbxHeightUnit.ItemIndex);
  FResolutionUnit := TgmResolutionUnit(cmbbxResolutionUnit.ItemIndex);
end;

procedure TfrmCreateNewFile.edtBitmapWidthChange(Sender: TObject);
var
  RealVal, MinFloatWidth, MaxFloatWidth, Inches: Double;
  IntVal, MinIntWidth, MaxIntWidth             : Integer;
begin
  MinFloatWidth := 0.0;
  MaxFloatWidth := 0.0;

  if FCanChange then
  begin
    MinIntWidth := 16;
    MaxIntWidth := Printer.PageWidth;

    if FWidthUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        RealVal := StrToFloat(edtBitmapWidth.Text);

        case FWidthUnit of
          auInch:
            begin
              MinFloatWidth := frmPrintPreview.prntprvwPreview.ConvertX(MinIntWidth, mmPixel, mmLoEnglish) / 100;
              MaxFloatWidth := frmPrintPreview.prntprvwPreview.ConvertX(MaxIntWidth, mmPixel, mmLoEnglish) / 100;
            end;

          auCentimeter:
            begin
              MinFloatWidth := frmPrintPreview.prntprvwPreview.ConvertX(MinIntWidth, mmPixel, mmLoMetric) / 100;
              MaxFloatWidth := frmPrintPreview.prntprvwPreview.ConvertX(MaxIntWidth, mmPixel, mmLoMetric) / 100;
            end;

          auPoint:
            begin
              Inches        := frmPrintPreview.prntprvwPreview.ConvertX(MinIntWidth, mmPixel, mmLoEnglish) / 100;
              MinFloatWidth := Inches * 72;
              Inches        := frmPrintPreview.prntprvwPreview.ConvertX(MaxIntWidth, mmPixel, mmLoEnglish) / 100;
              MaxFloatWidth := Inches * 72;
            end;
        end;

        EnsureValueInRange(RealVal, MinFloatWidth, MaxFloatWidth);
      except
        case FWidthUnit of
          auInch      : RealVal := GetBitmapWidthByInch;
          auCentimeter: RealVal := GetBitmapWidthByCM;
          auPoint     : RealVal := GetBitmapWidthByPoint;
        end;

        ShowBitmapWidth;
      end;
    end;

    case FWidthUnit of
      auInch:
        begin
          SetBitmapWidthByInch(RealVal);
        end;
        
      auCentimeter:
        begin
          SetBitmapWidthByCM(RealVal);
        end;
        
      auPoint:
        begin
          SetBitmapWidthByPoint(RealVal);
        end;
      
      auPixel:
        begin
          try
            IntVal := StrToInt(edtBitmapWidth.Text);
            EnsureValueInRange(IntVal, MinIntWidth, MaxIntWidth);
            FBitmapWidth := IntVal;
          except
            ShowBitmapWidth;
          end;
        end;
    end;
  end;
end;

procedure TfrmCreateNewFile.edtBitmapWidthExit(Sender: TObject);
begin
  ShowBitmapWidth;
end;

procedure TfrmCreateNewFile.edtBitmapHeightChange(Sender: TObject);
var
  RealVal, MinFloatHeight, MaxFloatHeight, Inches: Double;
  IntVal, MinIntHeight, MaxIntHeight             : Integer;
begin
  MinFloatHeight := 0.0;
  MaxFloatHeight := 0.0;

  if FCanChange then
  begin
    MinIntHeight := 16;
    MaxIntHeight := Printer.PageHeight;

    if FHeightUnit in [auInch, auCentimeter, auPoint] then
    begin
      try
        RealVal := StrToFloat(edtBitmapHeight.Text);

        case FHeightUnit of
          auInch:
            begin
              MinFloatHeight := frmPrintPreview.prntprvwPreview.ConvertY(MinIntHeight, mmPixel, mmLoEnglish) / 100;
              MaxFloatHeight := frmPrintPreview.prntprvwPreview.ConvertY(MaxIntHeight, mmPixel, mmLoEnglish) / 100;
            end;

          auCentimeter:
            begin
              MinFloatHeight := frmPrintPreview.prntprvwPreview.ConvertY(MinIntHeight, mmPixel, mmLoMetric) / 100;
              MaxFloatHeight := frmPrintPreview.prntprvwPreview.ConvertY(MaxIntHeight, mmPixel, mmLoMetric) / 100;
            end;

          auPoint:
            begin
              Inches         := frmPrintPreview.prntprvwPreview.ConvertY(MinIntHeight, mmPixel, mmLoEnglish) / 100;
              MinFloatHeight := Inches * 72;
              Inches         := frmPrintPreview.prntprvwPreview.ConvertY(MaxIntHeight, mmPixel, mmLoEnglish) / 100;
              MaxFloatHeight := Inches * 72;
            end;
        end;

        EnsureValueInRange(RealVal, MinFloatHeight, MaxFloatHeight);
      except
        case FHeightUnit of
          auInch      : RealVal := GetBitmapHeightByInch;
          auCentimeter: RealVal := GetBitmapHeightByCM;
          auPoint     : RealVal := GetBitmapHeightByPoint;
        end;

        ShowBitmapHeight;
      end;
    end;

    case FHeightUnit of
      auInch:
        begin
          SetBitmapHeightByInch(RealVal);
        end;
        
      auCentimeter:
        begin
          SetBitmapHeightByCM(RealVal);
        end;
        
      auPoint:
        begin
          SetBitmapHeightByPoint(RealVal);
        end;
      
      auPixel:
        begin
          try
            IntVal := StrToInt(edtBitmapHeight.Text);
            EnsureValueInRange(IntVal, MinIntHeight, MaxIntHeight);
            FBitmapHeight := IntVal;
          except
            ShowBitmapHeight;
          end;
        end;
    end;
  end;
end;

procedure TfrmCreateNewFile.edtBitmapHeightExit(Sender: TObject);
begin
  ShowBitmapHeight;
end;

procedure TfrmCreateNewFile.cmbbxWidthUnitChange(Sender: TObject);
begin
  FWidthUnit := TgmAppliedUnit(cmbbxWidthUnit.ItemIndex);
  ShowBitmapWidth;
end;

procedure TfrmCreateNewFile.cmbbxHeightUnitChange(Sender: TObject);
begin
  FHeightUnit := TgmAppliedUnit(cmbbxHeightUnit.ItemIndex);
  ShowBitmapHeight;
end;

procedure TfrmCreateNewFile.cmbbxResolutionUnitChange(Sender: TObject);
begin
  FResolutionUnit := TgmResolutionUnit(cmbbxResolutionUnit.ItemIndex);
  ShowResolution;
end;

procedure TfrmCreateNewFile.btbtnOKClick(Sender: TObject);
begin
  SaveSettings;
end; 

end.
