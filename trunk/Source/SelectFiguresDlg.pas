unit SelectFiguresDlg;

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
  ImgList, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Buttons,
{ GraphicsMagic Lib }
  gmFigures;

type
  TfrmSelectFigures = class(TForm)
    pnlFigureList: TPanel;
    pnlButtons: TPanel;
    spltrSelectFigures: TSplitter;
    grpbxFigureProperties: TGroupBox;
    tlbrLayerSelect: TToolBar;
    ToolButton1: TToolButton;
    cmbbxLayerSelect: TComboBox;
    lblLayerName: TLabel;
    lstbxFigureList: TListBox;
    imglstSelectFigure: TImageList;
    lblFigureName: TLabel;
    lblFigureNameValue: TLabel;
    lblStartingCoordinate: TLabel;
    lblStartCoordValue: TLabel;
    lblEndingCoordinate: TLabel;
    lblEndCoordValue: TLabel;
    lblCurveControlPoint1: TLabel;
    lblCurvePoint1Value: TLabel;
    lblCurveControlPoint2: TLabel;
    lblCurvePoint2Value: TLabel;
    lblPenWidth: TLabel;
    lblPenWidthValue: TLabel;
    lblCornerRadius: TLabel;
    lblCornerRadiusValue: TLabel;
    bvlHorizontalLine: TBevel;
    lblPenColor: TLabel;
    shpPenColor: TShape;
    lblBrushColor: TLabel;
    shpBrushColor: TShape;
    imgPenStyle: TImage;
    lblPenStyle: TLabel;
    imgBrushStyle: TImage;
    lblBrushStyle: TLabel;
    tlbrListTools: TToolBar;
    ToolButton2: TToolButton;
    tlbtnMoveUp: TToolButton;
    tlbtnMoveDown: TToolButton;
    tlbtnDeleteSelectedFigures: TToolButton;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure cmbbxLayerSelectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstbxFigureListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChangeSelectedFigureIndex(Sender: TObject);
    procedure DeleteSelectedFigures(Sender: TObject);
    procedure lstbxFigureListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    // 2D interger array FSelectedInfo is used to hold the information of
    // selected figures. The first dimension is used to hold the indices for
    // indexing the SelectedFigureLayerIndexArray field of the Figure Manager
    // for getting the index of layers that with selected figures. The second
    // dimension is used to hold the indices of the selected figures that on
    // certain layers with selected figures. 
      
    FSelectedInfo       : array of array of Integer;
    FAllOriginalFigures : array of array of TgmFigureObject;

    FFiguresModified    : Boolean;  // if any of figures has been deleted or changed its order

    procedure ListAllFiguresOnOneVectorLayer(const AIndex: Integer);
    procedure GetVectorLayerName;
    procedure GetSelectedInfo; 
    procedure ShowSelectedFigureProperties;
    procedure HideAllFigureProperties;
    procedure BackupAllFigures;
    procedure ChangeButtonsStatus;
  public
    procedure ClearSelectedInfoArray;
    procedure ClearAllOriginalFiguresArray;
    procedure ApplyConfiguration;
    procedure CancelConfiguration;

    property IsFiguresModified : Boolean read FFiguresModified;
  end;

var
  frmSelectFigures: TfrmSelectFigures;

implementation

uses
{ Graphics32 }
  GR32_Layers,
{ GraphicsMagic Lib }
  gmLayers,
  gmVectorLayer,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

//-- Custom Procedures and Functions -------------------------------------------

procedure TfrmSelectFigures.GetVectorLayerName;
var
  i, LLayerIndex : Integer;
  LLayer         : TgmCustomLayer;
begin
  cmbbxLayerSelect.Items.Clear;

  with ActiveChildForm do
  begin
    if Length(FigureManager.FVectorLayerIndexes) > 0 then
    begin
      for i := 0 to High(FigureManager.FVectorLayerIndexes) do
      begin
        LLayerIndex := FigureManager.FVectorLayerIndexes[i];
        LLayer      := LayerList.Layers[LLayerIndex];

        cmbbxLayerSelect.Items.Add(LLayer.LayerName);

        // The number of elements in the FSelectedInfo array is same as
        // the number of vector layers. 
        SetLength( FSelectedInfo, Length(FSelectedInfo) + 1 );
      end;
    end;
  end;
end;

procedure TfrmSelectFigures.ListAllFiguresOnOneVectorLayer(
  const AIndex: Integer);
var
  i, LLayerIndex        : Integer;
  LLowIndex, LHighIndex : Integer;
  LVectorLayer          : TgmVectorLayer;
  LFigureObj            : TgmFigureObject;
begin
  lstbxFigureList.Items.Clear;

  with ActiveChildForm do
  begin
    LLowIndex  := Low(FigureManager.FVectorLayerIndexes);
    LHighIndex := High(FigureManager.FVectorLayerIndexes);

    if LHighIndex > (-1) then
    begin
      if (AIndex >= LLowIndex) and (AIndex <= LHighIndex) then
      begin
        LLayerIndex  := FigureManager.FVectorLayerIndexes[AIndex];
        LVectorLayer := TgmVectorLayer(LayerList.Layers[LLayerIndex]);

        if LVectorLayer.FigureList.Count > 0 then
        begin
          for i := 0 to (LVectorLayer.FigureList.Count - 1) do
          begin
            LFigureObj := TgmFigureObject(LVectorLayer.FigureList.Items[i]);
            lstbxFigureList.Items.Add(LFigureObj.Name);
          end;
        end;
      end;
    end;
  end;

  lstbxFigureList.Update;
end;

procedure TfrmSelectFigures.GetSelectedInfo;
var
  i, j        : Integer;
  LIndex      : Integer;
  LLayerIndex : Integer;
begin
  with ActiveChildForm do
  begin
    FigureManager.ClearSelectedFiguresInfo;

    if Length(FSelectedInfo) > 0 then
    begin
      LIndex := cmbbxLayerSelect.ItemIndex;
      SetLength(FSelectedInfo[LIndex], 0);

      if lstbxFigureList.Items.Count > 0 then
      begin
        for i := 0 to (lstbxFigureList.Items.Count - 1) do
        begin
          if lstbxFigureList.Selected[i] then
          begin
            SetLength( FSelectedInfo[LIndex], Length(FSelectedInfo[LIndex]) + 1 );
            FSelectedInfo[LIndex, High(FSelectedInfo[LIndex])] := i;
          end;
        end;
      end;

      for i := Low(FSelectedInfo) to High(FSelectedInfo) do
      begin
        if Length(FSelectedInfo[i]) > 0 then
        begin
          LLayerIndex := FigureManager.FVectorLayerIndexes[i];

          SetLength( FigureManager.FSelectedFigureLayerIndexArray,
                     Length(FigureManager.FSelectedFigureLayerIndexArray) + 1 );

          LIndex := High(FigureManager.FSelectedFigureLayerIndexArray);

          FigureManager.FSelectedFigureLayerIndexArray[LIndex] := LLayerIndex;

          for j := Low(FSelectedInfo[i]) to High(FSelectedInfo[i]) do
          begin
            SetLength( FigureManager.FSelectedFigureInfoArray,
                       Length(FigureManager.FSelectedFigureInfoArray) + 1 );

            LIndex := High(FigureManager.FSelectedFigureInfoArray);

            FigureManager.FSelectedFigureInfoArray[LIndex].LayerIndex  := LLayerIndex;
            FigureManager.FSelectedFigureInfoArray[LIndex].FigureIndex := FSelectedInfo[i, j];
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmSelectFigures.ShowSelectedFigureProperties;
var
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  i, LX1, LX2  : Integer;
  LFigureObj   : TgmFigureObject;
  LVectorLayer : TgmVectorLayer;
begin
  LFigureIndex := 0;
  
  if lstbxFigureList.Items.Count > 0 then
  begin
    HideAllFigureProperties;

    LLayerIndex := ActiveChildForm.FigureManager.FVectorLayerIndexes[cmbbxLayerSelect.ItemIndex];

    for i := 0 to (lstbxFigureList.Items.Count - 1) do
    begin
      if lstbxFigureList.Selected[i] then
      begin
        LFigureIndex := i;
        Break;
      end;
    end;

    LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);
    LFigureObj   := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);

    lblFigureNameValue.Caption := LFigureObj.Name;
    lblFigureNameValue.Hint    := LFigureObj.Name;
    lblPenWidthValue.Caption   := IntToStr(LFigureObj.PenWidth) + '  Pixels';
    shpPenColor.Brush.Color    := LFigureObj.PenColor;
    shpBrushColor.Brush.Color  := LFigureObj.BrushColor;

    with imgBrushStyle.Canvas do
    begin
      Pen.Color   := clBlack;
      Pen.Style   := psSolid;
      Brush.Color := clWhite;
      Brush.Style := bsSolid;

      Rectangle(0, 0, imgBrushStyle.Width, imgBrushStyle.Height);

      Brush.Color := LFigureObj.BrushColor;
      Brush.Style := LFigureObj.BrushStyle;

      Rectangle(0, 0, imgBrushStyle.Width, imgBrushStyle.Height);
    end;

    with imgPenStyle.Canvas do
    begin
      Pen.Color   := clBlack;
      Pen.Style   := psSolid;
      Brush.Style := bsSolid;

      Rectangle(0, 0, imgPenStyle.Width, imgPenStyle.Height);

      Pen.Style := LFigureObj.PenStyle;
      LX1       := MulDiv(0 + imgPenStyle.Width, 1, 5);
      LX2       := MulDiv(0 + imgPenStyle.Width, 4, 5);

      MoveTo(LX1, (0 + imgPenStyle.Height) div 2);
      LineTo(LX2, (0 + imgPenStyle.Height) div 2);
    end;

    lblFigureNameValue.Visible := True;
    lblPenWidthValue.Visible   := True;
    shpPenColor.Visible        := True;
    shpBrushColor.Visible      := True;
    imgPenStyle.Visible        := True;
    imgBrushStyle.Visible      := True;

    if LFigureObj.Flag in [ffStraightLine,
                           ffCurve,
                           ffRectangle,
                           ffSquare,
                           ffRoundRectangle,
                           ffRoundSquare,
                           ffEllipse,
                           ffCircle] then
    begin
      lblStartCoordValue.Caption := Format('(%d, %d)', [LFigureObj.FStartPoint.X, LFigureObj.FStartPoint.Y]);
      lblEndCoordValue.Caption   := Format('(%d, %d)', [LFigureObj.FEndPoint.X, LFigureObj.FEndPoint.Y]);
      lblStartCoordValue.Visible := True;
      lblEndCoordValue.Visible   := True;
    end;

    if LFigureObj.Flag = ffCurve then
    begin
      lblCurvePoint1Value.Caption := Format('(%d, %d)', [LFigureObj.FCurvePoint1.X, LFigureObj.FCurvePoint1.Y]);
      lblCurvePoint2Value.Caption := Format('(%d, %d)', [LFigureObj.FCurvePoint2.X, LFigureObj.FCurvePoint2.Y]);
      lblCurvePoint1Value.Visible := True;
      lblCurvePoint2Value.Visible := True;
    end;

    if LFigureObj.Flag in [ffRoundRectangle, ffRoundSquare] then
    begin
      lblCornerRadiusValue.Caption := IntToStr(LFigureObj.RoundCornerRadius);
      lblCornerRadiusValue.Visible := True;
    end;
  end;
end;

procedure TfrmSelectFigures.HideAllFigureProperties;
begin
  lblFigureNameValue.Visible   := False;
  lblStartCoordValue.Visible   := False;
  lblEndCoordValue.Visible     := False;
  lblCurvePoint1Value.Visible  := False;
  lblCurvePoint2Value.Visible  := False;
  lblCornerRadiusValue.Visible := False;
  lblPenWidthValue.Visible     := False;
  shpPenColor.Visible          := False;
  shpBrushColor.Visible        := False;
  imgPenStyle.Visible          := False;
  imgBrushStyle.Visible        := False;
end;

procedure TfrmSelectFigures.BackupAllFigures;
var
  i, j         : Integer;
  LLayerIndex  : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  ClearAllOriginalFiguresArray;

  if Length(ActiveChildForm.FigureManager.FVectorLayerIndexes) > 0 then
  begin
    for i := 0 to High(ActiveChildForm.FigureManager.FVectorLayerIndexes) do
    begin
      // The number of elements in the FSelectedInfo array is same as
      // the number of figure layers. 
      SetLength( FAllOriginalFigures, Length(FAllOriginalFigures) + 1 );

      LLayerIndex  := ActiveChildForm.FigureManager.FVectorLayerIndexes[i];
      LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);

      if LVectorLayer.FigureList.Count > 0 then
      begin
        for j := 0 to (LVectorLayer.FigureList.Count - 1) do
        begin
          LFigureObj := TgmFigureObject(LVectorLayer.FigureList.Items[j]);
         
          SetLength( FAllOriginalFigures[i], Length(FAllOriginalFigures[i]) + 1 );
          FAllOriginalFigures[i, j] := LFigureObj.GetSelfBackup;
        end;
      end;
    end;
  end;
end;

procedure TfrmSelectFigures.ChangeButtonsStatus;
begin
  tlbtnDeleteSelectedFigures.Enabled := (lstbxFigureList.SelCount > 0);
  tlbtnMoveUp.Enabled                := (lstbxFigureList.SelCount = 1) and (lstbxFigureList.ItemIndex > 0);
  tlbtnMoveDown.Enabled              := (lstbxFigureList.SelCount = 1) and (lstbxFigureList.ItemIndex < lstbxFigureList.Items.Count - 1);
end;

procedure TfrmSelectFigures.ClearSelectedInfoArray;
var
  i : Integer;
begin
  if Length(FSelectedInfo) > 0 then
  begin
    for i := 0 to High(FSelectedInfo) do
    begin
      SetLength(FSelectedInfo[i], 0);
    end;

    SetLength(FSelectedInfo, 0);
  end;
end;

procedure TfrmSelectFigures.ClearAllOriginalFiguresArray;
var
  i : Integer;
begin
  if Length(FAllOriginalFigures) > 0 then
  begin
    for i := 0 to High(FAllOriginalFigures) do
    begin
      SetLength(FAllOriginalFigures[i], 0);
    end;

    SetLength(FAllOriginalFigures, 0);
  end;
end;

procedure TfrmSelectFigures.ApplyConfiguration;
var
  i, j         : Integer;
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Length(FSelectedInfo) > 0 then
  begin
    for i := 0 to High(FSelectedInfo) do
    begin
      LLayerIndex := ActiveChildForm.FigureManager.FVectorLayerIndexes[i];

      if Length(FSelectedInfo[i]) > 0 then
      begin
        for j := 0 to High(FSelectedInfo[i]) do
        begin
          LFigureIndex          := FSelectedInfo[i, j];
          LVectorLayer          := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);
          LFigureObj            := TgmFigureObject(LVectorLayer.FigureList.Items[LFigureIndex]);
          LFigureObj.IsSelected := True;
        end;
      end;
    end;

    FFiguresModified := True;
  end;
end;

procedure TfrmSelectFigures.CancelConfiguration;
var
  i, j         : Integer;
  LLayerIndex  : Integer;
  LVectorLayer : TgmVectorLayer;
  LFigureObj   : TgmFigureObject;
begin
  if Length(FAllOriginalFigures) > 0 then
  begin
    ActiveChildForm.FigureManager.ClearSelectedFiguresInfo;

    for i := 0 to High(FAllOriginalFigures) do
    begin
      LLayerIndex  := ActiveChildForm.FigureManager.FVectorLayerIndexes[i];
      LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);

      LVectorLayer.FigureList.DeleteAllFigures();

      for j := Low(FAllOriginalFigures[i]) to High(FAllOriginalFigures[i]) do
      begin
        LFigureObj := FAllOriginalFigures[i, j].GetSelfBackup;
        LVectorLayer.FigureList.Add(LFigureObj);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmSelectFigures.FormShow(Sender: TObject);
begin
  GetVectorLayerName;
  BackupAllFigures;
  cmbbxLayerSelect.ItemIndex := 0;
  ListAllFiguresOnOneVectorLayer(0);
  HideAllFigureProperties;
  ChangeButtonsStatus;
end;

procedure TfrmSelectFigures.cmbbxLayerSelectChange(Sender: TObject);
var
  i      : Integer;
  LIndex : Integer;
begin
  ListAllFiguresOnOneVectorLayer(cmbbxLayerSelect.ItemIndex);

  if lstbxFigureList.Items.Count > 0 then
  begin
    for i := Low(FSelectedInfo[cmbbxLayerSelect.ItemIndex]) to
             High(FSelectedInfo[cmbbxLayerSelect.ItemIndex]) do
    begin
      LIndex := FSelectedInfo[cmbbxLayerSelect.ItemIndex, i];
      lstbxFigureList.Selected[LIndex] := True;
    end;

    if lstbxFigureList.SelCount = 1 then
    begin
      ShowSelectedFigureProperties;
    end
    else
    begin
      HideAllFigureProperties;
    end;

    ChangeButtonsStatus;
  end;
end; 

procedure TfrmSelectFigures.FormCreate(Sender: TObject);
begin
  SetLength(FSelectedInfo, 0);
  SetLength(FAllOriginalFigures, 0);

  FFiguresModified := False;
end;

procedure TfrmSelectFigures.FormDestroy(Sender: TObject);
begin
  ClearSelectedInfoArray;
  ClearAllOriginalFiguresArray;
end; 

procedure TfrmSelectFigures.lstbxFigureListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetSelectedInfo;
  ApplyConfiguration;

  // update view
  ActiveChildForm.imgWorkArea.Changed;

  if lstbxFigureList.SelCount = 1 then
  begin
    ShowSelectedFigureProperties;
  end
  else
  begin
    HideAllFigureProperties;
  end;

  ChangeButtonsStatus;
end;

procedure TfrmSelectFigures.ChangeSelectedFigureIndex(Sender: TObject);
var
  LLayerIndex  : Integer;
  LFigureIndex : Integer;
  LTargetIndex : Integer;
  i            : Integer;
  LVectorLayer : TgmVectorLayer;
begin
  LFigureIndex := 0;
  LTargetIndex := 0;
  LLayerIndex  := ActiveChildForm.FigureManager.FVectorLayerIndexes[cmbbxLayerSelect.ItemIndex];

  for i := 0 to (lstbxFigureList.Items.Count - 1) do
  begin
    if lstbxFigureList.Selected[i] then
    begin
      LFigureIndex := i;
      Break;
    end;
  end;

  if Sender = tlbtnMoveUp then
  begin
    LTargetIndex := LFigureIndex - 1;
  end
  else if Sender = tlbtnMoveDown then
  begin
    LTargetIndex := LFigureIndex + 1;
  end;

  lstbxFigureList.Items.Move(LFigureIndex, LTargetIndex);

  LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);
  LVectorLayer.FigureList.Move(LFigureIndex, LTargetIndex);

  lstbxFigureList.Selected[LTargetIndex] := True;

  GetSelectedInfo();

  ActiveChildForm.FigureManager.DrawAllFiguresOnSpecifiedVectorLayer(LLayerIndex);
  ActiveChildForm.LayerList.SelectedLayer.Changed();

  ChangeButtonsStatus();

  FFiguresModified := True;
end;

procedure TfrmSelectFigures.DeleteSelectedFigures(Sender: TObject);
var
  i, LLayerIndex : Integer;
  LVectorLayer   : TgmVectorLayer;
begin
  LLayerIndex := ActiveChildForm.FigureManager.FVectorLayerIndexes[cmbbxLayerSelect.ItemIndex];

  // Deleting in inverse order,
  // an index error exception will be raised, otherwise. 
  for i := (lstbxFigureList.Items.Count - 1) downto 0 do
  begin
    if lstbxFigureList.Selected[i] then
    begin
      lstbxFigureList.Items.Delete(i);

      LVectorLayer := TgmVectorLayer(ActiveChildForm.LayerList.Layers[LLayerIndex]);
      LVectorLayer.FigureList.Delete(i);
    end;
  end;

  GetSelectedInfo();

  ActiveChildForm.FigureManager.DrawAllFiguresOnSpecifiedVectorLayer(LLayerIndex);
  ActiveChildForm.LayerList.SelectedLayer.Changed();

  ChangeButtonsStatus();

  FFiguresModified := True;
end;

procedure TfrmSelectFigures.lstbxFigureListKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN] then
  begin
    if lstbxFigureList.SelCount > 0 then
    begin
      GetSelectedInfo;
      ApplyConfiguration;

      // update view
      ActiveChildForm.imgWorkArea.Changed;

      if lstbxFigureList.SelCount = 1 then
      begin
        ShowSelectedFigureProperties;
      end
      else
      begin
        HideAllFigureProperties;
      end;
      
      ChangeButtonsStatus;
    end;
  end;
end; 

end.
