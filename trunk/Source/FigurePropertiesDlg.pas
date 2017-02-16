unit FigurePropertiesDlg;

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

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Menus,
{ GraphicsMagic }
  gmLayerFigureManager,
  gmTypes;

type
  TfrmFigureProperties = class(TForm)
    grpbxFigureProperties: TGroupBox;
    lblLayerName: TLabel;
    edtLayerName: TEdit;
    lblFigureIndex: TLabel;
    edtFigureIndex: TEdit;
    lblLayerIndex: TLabel;
    edtLayerIndex: TEdit;
    lblFigureName: TLabel;
    edtFigureName: TEdit;
    lblPenColor: TLabel;
    shpPenColor: TShape;
    spdbtnChangePenColor: TSpeedButton;
    lblBrushColor: TLabel;
    shpBrushColor: TShape;
    spdbtnBrushColor: TSpeedButton;
    lblPenWidth: TLabel;
    edtPenWidth: TEdit;
    updwnPenWidth: TUpDown;
    lblPenStyle: TLabel;
    cmbbxPenStyle: TComboBox;
    lblBrushStyle: TLabel;
    pnlBrushStyle: TPanel;
    imgBrushStyle: TImage;
    spdbtnBrushStyle: TSpeedButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    pmnFigureBrushStyle: TPopupMenu;
    pmnitmClearBrush: TMenuItem;
    pmnitmSolidBrush: TMenuItem;
    pmnitmHorizontalBrush: TMenuItem;
    pmnitmVerticalBrush: TMenuItem;
    pmnitmFDiagonalBrush: TMenuItem;
    pmnitmBDiagonalBrush: TMenuItem;
    pmnitmCrossBrush: TMenuItem;
    pmnitmDiagonalCrossBrush: TMenuItem;
    lblOriginX: TLabel;
    lblOriginY: TLabel;
    lblRadius: TLabel;
    edtOriginX: TEdit;
    edtOriginY: TEdit;
    edtRadius: TEdit;
    lblUnit: TLabel;
    cmbbxUnit: TComboBox;
    procedure cmbbxPenStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure spdbtnBrushStyleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeBrushStyle(Sender: TObject);
    procedure pmnFigureBrushStylePopup(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure cmbbxPenStyleChange(Sender: TObject);
    procedure spdbtnChangePenColorClick(Sender: TObject);
    procedure spdbtnBrushColorClick(Sender: TObject);
    procedure edtPenWidthChange(Sender: TObject);
    procedure cmbbxUnitChange(Sender: TObject);
    procedure edtOriginXChange(Sender: TObject);
    procedure edtOriginYChange(Sender: TObject);
    procedure edtRadiusChange(Sender: TObject);
  private
    FOriginXCopy : Integer;
    FOriginYCopy : Integer;
    FRadiusCopy  : Integer;
    FFigureInfo  : TgmFigureInfo;
    FCanChange   : Boolean;
    FPenStyle    : TPenStyle;
    FBrushStyle  : TBrushStyle;
    
    procedure ShowFigurePropertiesInUnits(const AUnits: TgmAppliedUnit);
    procedure DrawBrushStyle(ABitmap: TBitmap; const AStyle: TBrushStyle);
  public
    procedure SetFigureProperties(const AUnits: TgmAppliedUnit);
    procedure RestoreSettings;

    property PenStyle   : TPenStyle   read FPenStyle;
    property BrushStyle : TBrushStyle read FBrushStyle;
  end;

var
  frmFigureProperties: TfrmFigureProperties;

implementation

uses
{ GraphicsMagic Lib }
  gmFigures,
  gmGUIFuncs,
  gmVectorLayer,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

//-- Custom procedures and functions -------------------------------------------

procedure TfrmFigureProperties.ShowFigurePropertiesInUnits(
  const AUnits: TgmAppliedUnit);
begin
  if Assigned(ActiveChildForm.FSelectedFigure) then
  begin
    case AUnits of
      auInch:
        begin
          edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginInchX);
          edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginInchY);
          edtRadius.Text  := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusInch);
        end;

      auCentimeter:
        begin
          edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginCMX);
          edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginCMY);
          edtRadius.Text  := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusCM);
        end;

      auPoint:
        begin
          edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginPointX);
          edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginPointY);
          edtRadius.Text  := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusPoint);
        end;

      auPixel:
        begin
          edtOriginX.Text := IntToStr(ActiveChildForm.FSelectedFigure.OriginPixelX);
          edtOriginY.Text := IntToStr(ActiveChildForm.FSelectedFigure.OriginPixelY);
          edtRadius.Text  := IntToStr(ActiveChildForm.FSelectedFigure.RadiusPixel);
        end;
    end;
  end;
end;

procedure TfrmFigureProperties.DrawBrushStyle(ABitmap: TBitmap;
  const AStyle: TBrushStyle);
begin
  with ABitmap.Canvas do
  begin
    Pen.Width   := 1;
    Pen.Color   := clBlack;
    Pen.Style   := psSolid;
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    
    FillRect(ABitmap.Canvas.ClipRect);

    Brush.Color := clBlack;
    Brush.Style := AStyle;
    
    Rectangle(1, 1, ABitmap.Width - 2, ABitmap.Height - 2);
  end;
end; 

procedure TfrmFigureProperties.SetFigureProperties(
  const AUnits: TgmAppliedUnit);
begin
  if Assigned(ActiveChildForm.FSelectedFigure) then
  begin
    case AUnits of
      auInch:
        begin
          ActiveChildForm.FSelectedFigure.OriginInchX := StrToFloat(edtOriginX.Text);
          ActiveChildForm.FSelectedFigure.OriginInchY := StrToFloat(edtOriginY.Text);
          ActiveChildForm.FSelectedFigure.RadiusInch  := StrToFloat(edtRadius.Text);
        end;

      auCentimeter:
        begin
          ActiveChildForm.FSelectedFigure.OriginCMX := StrToFloat(edtOriginX.Text);
          ActiveChildForm.FSelectedFigure.OriginCMY := StrToFloat(edtOriginY.Text);
          ActiveChildForm.FSelectedFigure.RadiusCM  := StrToFloat(edtRadius.Text);
        end;

      auPoint:
        begin
          ActiveChildForm.FSelectedFigure.OriginPointX := StrToFloat(edtOriginX.Text);
          ActiveChildForm.FSelectedFigure.OriginPointY := StrToFloat(edtOriginY.Text);
          ActiveChildForm.FSelectedFigure.RadiusPoint  := StrToFloat(edtRadius.Text);
        end;

      auPixel:
        begin
          ActiveChildForm.FSelectedFigure.OriginPixelX := StrToInt(edtOriginX.Text);
          ActiveChildForm.FSelectedFigure.OriginPixelY := StrToInt(edtOriginY.Text);
          ActiveChildForm.FSelectedFigure.RadiusPixel  := StrToInt(edtRadius.Text);
        end;
    end;
  end;
end;

procedure TfrmFigureProperties.RestoreSettings;
begin
  if Assigned(ActiveChildForm.FSelectedFigure) then
  begin
    ActiveChildForm.FSelectedFigure.OriginPixelX := FOriginXCopy;
    ActiveChildForm.FSelectedFigure.OriginPixelY := FOriginYCopy;
    ActiveChildForm.FSelectedFigure.RadiusPixel  := FRadiusCopy;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmFigureProperties.cmbbxPenStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LBrushInfo : TLogBrush;
  i1, i2     : Integer;
  LPenStyle  : DWORD;
begin
  with LBrushInfo do
  begin
    lbStyle := BS_SOLID;
    lbColor := clBlack;
    lbHatch := 0
  end;

  LPenStyle := PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_MITER;

  case Index of
    0:
      begin
        LPenStyle := LPenStyle + PS_SOLID;
      end;

    1:
      begin
        LPenStyle := LPenStyle + PS_DASH;
      end;

    2:
      begin
        LPenStyle := LPenStyle + PS_DOT;
      end;
      
    3:
      begin
        LPenStyle := LPenStyle + PS_DASHDOT;
      end;

    4:
      begin
        LPenStyle := LPenStyle + PS_DASHDOTDOT;
      end;
  end;

  (Control as TComboBox).Canvas.Pen.Handle := ExtCreatePen(LPenStyle, 3, LBrushInfo, 0, nil);

  with (Control as TComboBox).Canvas do
  begin
    i1 := MulDiv(Rect.Left + Rect.Right, 1, 5);
    i2 := MulDiv(Rect.Left + Rect.Right, 4, 5);
    
    MoveTo(i1, (Rect.Top + Rect.Bottom) div 2);
    LineTo(i2, (Rect.Top + Rect.Bottom) div 2);
  end;
end;

procedure TfrmFigureProperties.spdbtnBrushStyleClick(Sender: TObject);
var
  LPoint : TPoint;
begin
  GetCursorPos(LPoint);
  pmnFigureBrushStyle.Popup(LPoint.X, LPoint.Y);
end;

procedure TfrmFigureProperties.FormCreate(Sender: TObject);
begin
  FCanChange  := True;
  FPenStyle   := psSolid;
  FBrushStyle := bsSolid;
  
  imgBrushStyle.Picture.Bitmap.Width  := imgBrushStyle.Width;
  imgBrushStyle.Picture.Bitmap.Height := imgBrushStyle.Height;
end;

procedure TfrmFigureProperties.FormShow(Sender: TObject);
var
  LVectorLayer  : TgmVectorLayer;
  LEditColor    : TColor;
  LEditReadOnly : Boolean;
begin
  ActiveChildForm.FSelectedFigure := nil;
  ActiveChildForm.FSelectedFigure := ActiveChildForm.FigureManager.GetOnlyOneSelectedFigure;

  if Assigned(ActiveChildForm.FSelectedFigure) then
  begin
    FFigureInfo  := ActiveChildForm.FigureManager.FSelectedFigureInfoArray[0];
    LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[FFigureInfo.LayerIndex]);

    edtLayerName.Text   := LVectorLayer.LayerName;
    edtLayerName.Hint   := edtLayerName.Text;
    edtLayerIndex.Text  := IntToStr(FFigureInfo.LayerIndex);
    edtFigureIndex.Text := IntToStr(FFigureInfo.FigureIndex);
    edtFigureName.Text  := ActiveChildForm.FSelectedFigure.Name;
    cmbbxUnit.ItemIndex := Integer(frmMain.FigureUnits);

    if ActiveChildForm.FSelectedFigure.Flag in [ffRegularPolygon,
                                                ffSquare,
                                                ffRoundSquare,
                                                ffCircle] then
    begin
      FOriginXCopy  := ActiveChildForm.FSelectedFigure.OriginPixelX;
      FOriginYCopy  := ActiveChildForm.FSelectedFigure.OriginPixelY;
      FRadiusCopy   := ActiveChildForm.FSelectedFigure.RadiusPixel;
      LEditColor    := clWindow;
      LEditReadOnly := False;
      
      ShowFigurePropertiesInUnits(frmMain.FigureUnits);
    end
    else
    begin
      LEditColor      := clBtnFace;
      LEditReadOnly   := True;
      edtOriginX.Text := '';
      edtOriginY.Text := '';
      edtRadius.Text  := '';
    end;

    edtOriginX.Color    := LEditColor;
    edtOriginX.ReadOnly := LEditReadOnly;
    edtOriginY.Color    := LEditColor;
    edtOriginY.ReadOnly := LEditReadOnly;
    edtRadius.Color     := LEditColor;
    edtRadius.ReadOnly  := LEditReadOnly;

    shpPenColor.Brush.Color   := ActiveChildForm.FSelectedFigure.PenColor;
    shpBrushColor.Brush.Color := ActiveChildForm.FSelectedFigure.BrushColor;
    updwnPenWidth.Position    := ActiveChildForm.FSelectedFigure.PenWidth;
    FPenStyle                 := ActiveChildForm.FSelectedFigure.PenStyle;
    FBrushStyle               := ActiveChildForm.FSelectedFigure.BrushStyle;

    case FPenStyle of
      psSolid:
        begin
          cmbbxPenStyle.ItemIndex := 0;
        end;
        
      psDash:
        begin
          cmbbxPenStyle.ItemIndex := 1;
        end;
        
      psDot:
        begin
          cmbbxPenStyle.ItemIndex := 2;
        end;
        
      psDashDot:
        begin
          cmbbxPenStyle.ItemIndex := 3;
        end;
        
      psDashDotDot:
        begin
          cmbbxPenStyle.ItemIndex := 4;
        end;
    end;

    case FBrushStyle of
      bsClear:
        begin
          ChangeBrushStyle(pmnitmClearBrush);
        end;
        
      bsSolid:
        begin
          ChangeBrushStyle(pmnitmSolidBrush);
        end;
        
      bsHorizontal:
        begin
          ChangeBrushStyle(pmnitmHorizontalBrush);
        end;
        
      bsVertical:
        begin
          ChangeBrushStyle(pmnitmVerticalBrush);
        end;
        
      bsFDiagonal:
        begin
          ChangeBrushStyle(pmnitmFDiagonalBrush);
        end;
        
      bsBDiagonal:
        begin
          ChangeBrushStyle(pmnitmBDiagonalBrush);
        end;
        
      bsCross:
        begin
          ChangeBrushStyle(pmnitmCrossBrush);
        end;
        
      bsDiagCross:
        begin
          ChangeBrushStyle(pmnitmDiagonalCrossBrush);
        end;
    end;
  end;
  
  ActiveControl := btbtnOK;
end;

procedure TfrmFigureProperties.ChangeBrushStyle(Sender: TObject);
begin
  if Sender = pmnitmClearBrush then
  begin
    FBrushStyle        := bsClear;
    imgBrushStyle.Hint := pmnitmClearBrush.Hint;
  end
  else
  if Sender = pmnitmSolidBrush then
  begin
    FBrushStyle        := bsSolid;
    imgBrushStyle.Hint := pmnitmSolidBrush.Hint;
  end
  else
  if Sender = pmnitmHorizontalBrush then
  begin
    FBrushStyle        := bsHorizontal;
    imgBrushStyle.Hint := pmnitmHorizontalBrush.Hint;
  end
  else
  if Sender = pmnitmVerticalBrush then
  begin
    FBrushStyle        := bsVertical;
    imgBrushStyle.Hint := pmnitmVerticalBrush.Hint;
  end
  else
  if Sender = pmnitmFDiagonalBrush then
  begin
    FBrushStyle        := bsFDiagonal;
    imgBrushStyle.Hint := pmnitmFDiagonalBrush.Hint;
  end
  else
  if Sender = pmnitmBDiagonalBrush then
  begin
    FBrushStyle        := bsBDiagonal;
    imgBrushStyle.Hint := pmnitmBDiagonalBrush.Hint;
  end
  else
  if Sender = pmnitmCrossBrush then
  begin
    FBrushStyle        := bsCross;
    imgBrushStyle.Hint := pmnitmCrossBrush.Hint;
  end
  else
  if Sender = pmnitmDiagonalCrossBrush then
  begin
    FBrushStyle        := bsDiagCross;
    imgBrushStyle.Hint := pmnitmDiagonalCrossBrush.Hint;
  end;
  
  DrawBrushStyle(imgBrushStyle.Picture.Bitmap, FBrushStyle);
end;

procedure TfrmFigureProperties.pmnFigureBrushStylePopup(Sender: TObject);
begin
  pmnitmClearBrush.Checked         := (FBrushStyle = bsClear);
  pmnitmSolidBrush.Checked         := (FBrushStyle = bsSolid);
  pmnitmHorizontalBrush.Checked    := (FBrushStyle = bsHorizontal);
  pmnitmVerticalBrush.Checked      := (FBrushStyle = bsVertical);
  pmnitmFDiagonalBrush.Checked     := (FBrushStyle = bsFDiagonal);
  pmnitmBDiagonalBrush.Checked     := (FBrushStyle = bsBDiagonal);
  pmnitmCrossBrush.Checked         := (FBrushStyle = bsCross);
  pmnitmDiagonalCrossBrush.Checked := (FBrushStyle = bsDiagCross);
end;

procedure TfrmFigureProperties.btbtnOKClick(Sender: TObject);
begin
  if CheckIfEditEmpty(edtFigureName) then
  begin
    ModalResult := mrNone;
  end
  else
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmFigureProperties.cmbbxPenStyleChange(Sender: TObject);
begin
  FPenStyle := TPenStyle(cmbbxPenStyle.ItemIndex);
end;

procedure TfrmFigureProperties.spdbtnChangePenColorClick(Sender: TObject);
begin
  dmMain.clrdlgRGB.Color := shpPenColor.Brush.Color;
  
  if dmMain.clrdlgRGB.Execute then
  begin
    shpPenColor.Brush.Color := dmMain.clrdlgRGB.Color;
  end;
end; 

procedure TfrmFigureProperties.spdbtnBrushColorClick(Sender: TObject);
begin
  dmMain.clrdlgRGB.Color := shpBrushColor.Brush.Color;
  
  if dmMain.clrdlgRGB.Execute then
  begin
    shpBrushColor.Brush.Color := dmMain.clrdlgRGB.Color;
  end;
end;

procedure TfrmFigureProperties.edtPenWidthChange(Sender: TObject);
begin
  try
    updwnPenWidth.Position := StrToInt(edtPenWidth.Text);
  except
    edtPenWidth.Text := IntToStr(updwnPenWidth.Position);
  end;
end;

procedure TfrmFigureProperties.cmbbxUnitChange(Sender: TObject);
begin
  FCanChange := False;
  try
    frmMain.FigureUnits := TgmAppliedUnit(cmbbxUnit.ItemIndex);
    ShowFigurePropertiesInUnits(frmMain.FigureUnits);
  finally
    FCanChange := True;
  end;
end;

procedure TfrmFigureProperties.edtOriginXChange(Sender: TObject);
var
  LRealVal : Double;
begin
  if FCanChange then
  begin
    if frmMain.FigureUnits in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtOriginX.Text);

        case frmMain.FigureUnits of
          auInch:
            begin
              ActiveChildForm.FSelectedFigure.OriginInchX := LRealVal;
            end;

          auCentimeter:
            begin
              ActiveChildForm.FSelectedFigure.OriginCMX := LRealVal;
            end;
            
          auPoint:
            begin
              ActiveChildForm.FSelectedFigure.OriginPointX := LRealVal;
            end;
        end;
      except
        case frmMain.FigureUnits of
          auInch:
            begin
              edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginInchX);
            end;
            
          auCentimeter:
            begin
              edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginCMX);
            end;
            
          auPoint:
            begin
              edtOriginX.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginPointX);
            end;
        end;
      end;
    end
    else
    if frmMain.FigureUnits = auPixel then
    begin
      try
        ActiveChildForm.FSelectedFigure.OriginPixelX := StrToInt(edtOriginX.Text);
      except
        edtOriginX.Text := IntToStr(ActiveChildForm.FSelectedFigure.OriginPixelX);
      end;
    end;
  end;
end;

procedure TfrmFigureProperties.edtOriginYChange(Sender: TObject);
var
  LRealVal : Double;
begin
  if FCanChange then
  begin
    if frmMain.FigureUnits in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtOriginY.Text);

        case frmMain.FigureUnits of
          auInch:
            begin
              ActiveChildForm.FSelectedFigure.OriginInchY := LRealVal;
            end;

          auCentimeter:
            begin
              ActiveChildForm.FSelectedFigure.OriginCMY := LRealVal;
            end;

          auPoint:
            begin
              ActiveChildForm.FSelectedFigure.OriginPointY := LRealVal;
            end;
        end;
      except
        case frmMain.FigureUnits of
          auInch:
            begin
              edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginInchY);
            end;
            
          auCentimeter:
            begin
              edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginCMY);
            end;
            
          auPoint:
            begin
              edtOriginY.Text := FloatToStr(ActiveChildForm.FSelectedFigure.OriginPointY);
            end;
        end;
      end;
    end
    else
    if frmMain.FigureUnits = auPixel then
    begin
      try
        ActiveChildForm.FSelectedFigure.OriginPixelY := StrToInt(edtOriginY.Text);
      except
        edtOriginY.Text := IntToStr(ActiveChildForm.FSelectedFigure.OriginPixelY);
      end;
    end;
  end;
end;

procedure TfrmFigureProperties.edtRadiusChange(Sender: TObject);
const
  MIN_RADIUS_PIXEL = 1;
  MIN_RADIUS_INCH  = 0.01;
  MIN_RADIUS_CM    = 0.03;
  MIN_RADIUS_POINT = 0.72;
var
  LRealVal, LMinRVal : Double;
  LIntVal            : Integer;
begin
  LMinRVal := 0.00;
  
  if FCanChange then
  begin
    if frmMain.FigureUnits in [auInch, auCentimeter, auPoint] then
    begin
      try
        LRealVal := StrToFloat(edtRadius.Text);

        case frmMain.FigureUnits of
          auInch:
            begin
              LMinRVal := MIN_RADIUS_INCH;
            end;
            
          auCentimeter:
            begin
              LMinRVal := MIN_RADIUS_CM;
            end;
            
          auPoint:
            begin
              LMinRVal := MIN_RADIUS_POINT;
            end;
        end;

        if LRealVal < LMinRVal then
        begin
          LRealVal       := LMinRVal;
          edtRadius.Text := FloatToStr(LRealVal);
        end;

        case frmMain.FigureUnits of
          auInch:
            begin
              ActiveChildForm.FSelectedFigure.RadiusInch := LRealVal;
            end;
            
          auCentimeter:
            begin
              ActiveChildForm.FSelectedFigure.RadiusCM := LRealVal;
            end;
            
          auPoint:
            begin
              ActiveChildForm.FSelectedFigure.RadiusPoint := LRealVal;
            end;
        end;

      except
        case frmMain.FigureUnits of
          auInch:
            begin
              edtRadius.Text := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusInch);
            end;
            
          auCentimeter:
            begin
              edtRadius.Text := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusCM);
            end;
            
          auPoint:
            begin
              edtRadius.Text := FloatToStr(ActiveChildForm.FSelectedFigure.RadiusPoint);
            end;
        end;
      end;
    end
    else
    if frmMain.FigureUnits = auPixel then
    begin
      try
        LIntVal := StrToInt(edtRadius.Text);
        
        if LIntVal < MIN_RADIUS_PIXEL then
        begin
          LIntVal        := MIN_RADIUS_PIXEL;
          edtRadius.Text := IntToStr(LIntVal);
        end;

        ActiveChildForm.FSelectedFigure.RadiusPixel := LIntVal;
      except
        edtRadius.Text := IntToStr(ActiveChildForm.FSelectedFigure.RadiusPixel);
      end;
    end;
  end;
end;

end.
