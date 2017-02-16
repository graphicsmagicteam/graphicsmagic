unit GradientEditorDlg;

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

{$I GraphicsMagic.inc}

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, 
{ Graphics32 }
  GR32_Image,
  GR32_Layers,
  GR32_RangeBars, 
{ GraphicsMagic Lib }
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  GradientPickerPopFrm, 
{ GraphicsMagic Package Lib }
  gmGridBased_List,
  gmGradient_List,
  gmGridBased_ListView,
  gmGradient_ListView,
  gmGradientEditor;

type
  TfrmGradientEditor = class(TForm)
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    clrdlgColorDialog: TColorDialog;
    pmnGradientEditorOptions: TPopupMenu;
    pmnitmSmallThumbnail: TMenuItem;
    pmnitmLargeThumbnail: TMenuItem;
    pmnitmDeleteGradient: TMenuItem;
    N1: TMenuItem;
    glGradients: TgmGradientList;
    btnSaveGradients: TButton;
    btnLoadGradients: TButton;
    Panel1: TPanel;
    grpbxStops: TGroupBox;
    lbl2: TLabel;
    lblOpacity: TLabel;
    lblColor: TLabel;
    lblAlphaLocation: TLabel;
    lblColorLocation: TLabel;
    lblAlphaLocationPercent: TLabel;
    lblColorLocationPercent: TLabel;
    ggbrOpacity: TGaugeBar;
    pnlPrimaryColor: TPanel;
    shpStopColor: TShape;
    edtAlphaLocation: TEdit;
    edtColorLocation: TEdit;
    btnDeleteSelectedAlphaValue: TButton;
    btnDeleteSelectedColor: TButton;
    btnAverageAlphaValues: TButton;
    btnAverageColors: TButton;
    geGradientEditor: TgmGradientEditor;
    Panel2: TPanel;
    lblGradientName: TLabel;
    edtGradientName: TEdit;
    btnNewGradient: TButton;
    grpbxPresets: TGroupBox;
    scrlbxGradientEditor: TScrollBox;
    glvGradients: TgmGradientListView;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmnGradientEditorOptionsPopup(Sender: TObject);
    procedure ChangeEditorThumbnailSizeMode(Sender: TObject);
    procedure glvGradientsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure glvGradientsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure glvGradientsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure btnNewGradientClick(Sender: TObject);
    procedure geGradientEditorCursorPosChanged(ASender: TObject;
      const ACursorPosition: TgmEditorCursorPosition);
    procedure geGradientEditorStateChanged(ASender: TObject;
      const AState: TgmGradientEditorState);
    procedure geGradientEditorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure geGradientEditorMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure geGradientEditorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrOpacityChange(Sender: TObject);
    procedure shpStopColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtAlphaLocationChange(Sender: TObject);
    procedure edtColorLocationChange(Sender: TObject);
    procedure btnDeleteSelectedAlphaValueClick(Sender: TObject);
    procedure btnDeleteSelectedColorClick(Sender: TObject);
    procedure btnAverageAlphaValuesClick(Sender: TObject);
    procedure btnAverageColorsClick(Sender: TObject);
    procedure pmnitmDeleteGradientClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure btnSaveGradientsClick(Sender: TObject);
    procedure btnLoadGradientsClick(Sender: TObject);
  private
    FGradientUser      : TgmGradientUser;
    FThumbnailSizeMode : TgmThumbnailSizeMode;
    FModified          : Boolean;
    FAllowChange       : Boolean;
    FLastSelectedIndex : Integer;

    procedure LoadGradientsToEditor;
    procedure ChangeThumbnailSize(const ASizeMode: TgmThumbnailSizeMode);
    procedure SaveSettingsForDrawingToolGradients;
    procedure SaveSettingsForMapCommandGradients;
    procedure SaveSettingsForFillLayerGradients;
    procedure SaveSettingsForMapLayerGradients;
    procedure SaveGradientsSettings;
  public
    property GradientUser : TgmGradientUser read FGradientUser write FGradientUser;
  end;

var
  frmGradientEditor: TfrmGradientEditor;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Package Lib }
  gmGradient,
{ GraphicsMagic Lib }
  gmConstants,
  gmGradientFillLayer,
  gmGradientMapLayer,
  gmIni,
{ GraphicsMagic Forms }
  MainForm,
{ GraphicsMagic Dialogs }
  GradientMapDlg;

{$R *.DFM}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmGradientEditor.LoadGradientsToEditor;
begin
  case FGradientUser of
    guGradientTools:
      begin
        glGradients.Gradients.Assign(frmGradientPicker.glDrawingToolGradients.Gradients);
      end;

    guGradientMapCommand:
      begin
        glGradients.Gradients.Assign(frmGradientPicker.glMapCommandGradients.Gradients);
      end;

    guGradientFillLayer:
      begin
        glGradients.Gradients.Assign(frmGradientPicker.glFillLayerGradients.Gradients);
      end;

    guGradientMapLayer:
      begin
        glGradients.Gradients.Assign(frmGradientPicker.glMapLayerGradients.Gradients);
      end;
  end;
end;

procedure TfrmGradientEditor.ChangeThumbnailSize(
  const ASizeMode: TgmThumbnailSizeMode);
begin
  case ASizeMode of
    tsmSmall:
      begin
        glvGradients.SetThumbSize(THUMBNAIL_SIZE_SMALL);
      end;

    tsmLarge:
      begin
        glvGradients.SetThumbSize(THUMBNAIL_SIZE_LARGE);
      end;
  end;
end;

procedure TfrmGradientEditor.SaveSettingsForDrawingToolGradients;
var
  LFileName : string;
  LFileExt  : string;
begin
  LFileName := frmGradientPicker.FDrawingToolGradientListState.FileName;

  if LFileName = '' then
  begin
    if frmGradientPicker.sgdSaveGradientDialog.Execute then
    begin
      LFileName := frmGradientPicker.sgdSaveGradientDialog.FileName;
      LFileExt  := LowerCase( ExtractFileExt(LFileName) );

      if LFileExt = '' then
      begin
        LFileName := LFileName + '.grd';
      end
      else
      begin
        if LFileExt <> '.grd' then
        begin
          LFileName := ChangeFileExt(LFileName, '.grd');
        end;
      end;

      if FileExists(LFileName) then
      begin
        if MessageDlg('The file is already existed. Do you want to replace it?',
                       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
  end;

  frmGradientPicker.glDrawingToolGradients.Gradients.Assign(glGradients.Gradients);
  frmGradientPicker.glDrawingToolGradients.Gradients.SaveToFile(LFileName);

  with frmGradientPicker.FDrawingToolGradientListState do
  begin
    FileName    := LFileName;
    UseInternal := False;

    if not glGradients.IsValidIndex(SelectedIndex) then
    begin
      SelectedIndex := 0;
    end;
  end;

  frmGradientPicker.ShowDrawingToolSelectedGradient;

  WriteInfoToIniFile(SECTION_GRADIENT, IDENT_OPEN_GRADIENTS_FILE, LFileName);
  WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '0');

  WriteInfoToIniFile(
    SECTION_GRADIENT, IDENT_GRADIENTS_INDEX,
    IntToStr(frmGradientPicker.FDrawingToolGradientListState.SelectedIndex) );
end;

procedure TfrmGradientEditor.SaveSettingsForMapCommandGradients;
var
  LFileName : string;
  LFileExt  : string;
begin
  LFileName := frmGradientPicker.FMapCommandGradientListState.FileName;

  if LFileName = '' then
  begin
    if frmGradientPicker.sgdSaveGradientDialog.Execute then
    begin
      LFileName := frmGradientPicker.sgdSaveGradientDialog.FileName;
      LFileExt  := LowerCase( ExtractFileExt(LFileName) );

      if LFileExt = '' then
      begin
        LFileName := LFileName + '.grd';
      end
      else
      begin
        if LFileExt <> '.grd' then
        begin
          LFileName := ChangeFileExt(LFileName, '.grd');
        end;
      end;

      if FileExists(LFileName) then
      begin
        if MessageDlg('The file is already existed. Do you want to replace it?',
                       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
  end;

  frmGradientPicker.glMapCommandGradients.Gradients.Assign(glGradients.Gradients);
  frmGradientPicker.glMapCommandGradients.Gradients.SaveToFile(LFileName);

  with frmGradientPicker.FMapCommandGradientListState do
  begin
    FileName      := LFileName;
    UseInternal   := False;

    if not glGradients.IsValidIndex(SelectedIndex) then
    begin
      SelectedIndex := 0;
    end;
  end;

  frmGradientPicker.ShowMapCommandSelectedGradient;
  frmGradientMap.ExecuteGradientMap;

  WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_OPEN_FILE, LFileName);
  WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '0');

  WriteInfoToIniFile(
    SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX,
    IntToStr(frmGradientPicker.FMapCommandGradientListState.SelectedIndex) );
end;

procedure TfrmGradientEditor.SaveSettingsForFillLayerGradients;
var
  LFileName, LFileExt : string;
  LGradientFillLayer  : TgmGradientFillLayer;
begin
  LFileName := frmGradientPicker.FFillLayerGradientListState.FileName;

  if LFileName = '' then
  begin
    if frmGradientPicker.sgdSaveGradientDialog.Execute then
    begin
      LFileName := frmGradientPicker.sgdSaveGradientDialog.FileName;
      LFileExt  := LowerCase( ExtractFileExt(LFileName) );

      if LFileExt = '' then
      begin
        LFileName := LFileName + '.grd';
      end
      else
      begin
        if LFileExt <> '.grd' then
        begin
          LFileName := ChangeFileExt(LFileName, '.grd');
        end;
      end;

      if FileExists(LFileName) then
      begin
        if MessageDlg('The file is already existed. Do you want to replace it?',
                       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
  end;

  frmGradientPicker.glFillLayerGradients.Gradients.Assign(glGradients.Gradients);
  frmGradientPicker.glFillLayerGradients.Gradients.SaveToFile(LFileName);

  with frmGradientPicker.FFillLayerGradientListState do
  begin
    FileName      := LFileName;
    UseInternal   := False;

    if not glGradients.IsValidIndex(SelectedIndex) then
    begin
      SelectedIndex := 0;
    end;
  end;

  if ActiveChildForm.LayerList.SelectedLayer is TgmGradientFillLayer then
  begin
    LGradientFillLayer :=
      TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

    LGradientFillLayer.Gradient := frmGradientPicker.GetFillLayerGradient;
    LGradientFillLayer.Changed;

    frmGradientPicker.ShowFillLayerSelectedGradient;
  end;

  WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_OPEN_FILE, LFileName);
  WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '0');

  WriteInfoToIniFile(
    SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX,
    IntToStr(frmGradientPicker.FFillLayerGradientListState.SelectedIndex) );
end;

procedure TfrmGradientEditor.SaveSettingsForMapLayerGradients;
var
  LFileName, LFileExt : string;
  LGradientMapLayer   : TgmGradientMapLayer;
begin
  LFileName := frmGradientPicker.FMapLayerGradientListState.FileName;

  if LFileName = '' then
  begin
    if frmGradientPicker.sgdSaveGradientDialog.Execute then
    begin
      LFileName := frmGradientPicker.sgdSaveGradientDialog.FileName;
      LFileExt  := LowerCase( ExtractFileExt(LFileName) );

      if LFileExt = '' then
      begin
        LFileName := LFileName + '.grd';
      end
      else
      begin
        if LFileExt <> '.grd' then
        begin
          LFileName := ChangeFileExt(LFileName, '.grd');
        end;
      end;

      if FileExists(LFileName) then
      begin
        if MessageDlg('The file is already existed. Do you want to replace it?',
                       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end
    else
    begin
      Exit;
    end;
  end;

  frmGradientPicker.glMapLayerGradients.Gradients.Assign(glGradients.Gradients);
  frmGradientPicker.glMapLayerGradients.Gradients.SaveToFile(LFileName);

  with frmGradientPicker.FMapLayerGradientListState do
  begin
    FileName    := LFileName;
    UseInternal := False;

    if not glGradients.IsValidIndex(SelectedIndex) then
    begin
      SelectedIndex := 0;
    end;
  end;

  if ActiveChildForm.LayerList.SelectedLayer is TgmGradientMapLayer then
  begin
    LGradientMapLayer :=
      TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

    LGradientMapLayer.Gradient := frmGradientPicker.GetMapLayerGradient;

    if frmGradientMap.chckbxPreview.Checked then
    begin
      LGradientMapLayer.Changed;
    end;

    frmGradientPicker.ShowMapLayerSelectedGradient;
  end;

  WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_OPEN_FILE, LFileName);
  WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '0');

  WriteInfoToIniFile(
    SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_INDEX,
    IntToStr(frmGradientPicker.FMapLayerGradientListState.SelectedIndex) );
end;

procedure TfrmGradientEditor.SaveGradientsSettings;
begin
  case FGradientUser of
    guGradientTools:
      begin
        SaveSettingsForDrawingToolGradients;
      end;

    guGradientMapCommand:
      begin
        SaveSettingsForMapCommandGradients;
      end;

    guGradientFillLayer:
      begin
        SaveSettingsForFillLayerGradients;
      end;

    guGradientMapLayer:
      begin
        SaveSettingsForMapLayerGradients;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmGradientEditor.FormCreate(Sender: TObject);
var
  LValueStr : string;
begin
  FGradientUser      := guNone;
  FModified          := False;
  FAllowChange       := True;
  FLastSelectedIndex := glvGradients.ItemIndex;

  // Load thumbnail info
  try
    LValueStr := ReadInfoFromIniFile(SECTION_GRADIENT_EDITOR,
                                     IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE,
                                     '0');
                                  
    FThumbnailSizeMode := TgmThumbnailSizeMode( StrToInt(LValueStr) );


    if not (FThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      FThumbnailSizeMode := tsmLarge;
      
      WriteInfoToIniFile(  SECTION_GRADIENT_EDITOR,
                           IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(FThumbnailSizeMode) )  );
    end;
    
  except
    FThumbnailSizeMode := tsmLarge;
    
    WriteInfoToIniFile(  SECTION_GRADIENT_EDITOR,
                         IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(FThumbnailSizeMode) )  );
  end;

  ChangeThumbnailSize(FThumbnailSizeMode);
end; 

procedure TfrmGradientEditor.FormShow(Sender: TObject);
begin
  LoadGradientsToEditor;
end; 

procedure TfrmGradientEditor.pmnGradientEditorOptionsPopup(
  Sender: TObject);
begin
  pmnitmDeleteGradient.Enabled := (glvGradients.GradientList.Count > 1) and
                                  (glvGradients.GradientList.IsValidIndex(glvGradients.ItemIndex));

  pmnitmSmallThumbnail.Enabled := (glvGradients.GradientList.Count > 0);
  pmnitmLargeThumbnail.Enabled := pmnitmSmallThumbnail.Enabled;

  pmnitmSmallThumbnail.Checked := (glvGradients.GradientList.Count > 0) and 
                                  (FThumbnailSizeMode = tsmSmall);

  pmnitmLargeThumbnail.Checked := (glvGradients.GradientList.Count > 0) and
                                  (FThumbnailSizeMode = tsmLarge);
end; 

procedure TfrmGradientEditor.ChangeEditorThumbnailSizeMode(
  Sender: TObject);
begin
  // TODO: this enumeration should have a default value, such as tsmNone
  FThumbnailSizeMode := tsmSmall;

  if Sender = pmnitmSmallThumbnail then
  begin
    FThumbnailSizeMode := tsmSmall;
  end
  else if Sender = pmnitmLargeThumbnail then
  begin
    FThumbnailSizeMode := tsmLarge;
  end;

  ChangeThumbnailSize(FThumbnailSizeMode);
  
  WriteInfoToIniFile(  SECTION_GRADIENT_EDITOR,
                       IDENT_GRADIENT_EDITOR_THUMBNAIL_SIZE_MODE,
                       IntToStr( Ord(FThumbnailSizeMode) )  );
end;

procedure TfrmGradientEditor.glvGradientsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  FLastSelectedIndex             := geGradientEditor.GradientIndex;
  geGradientEditor.GradientIndex := glvGradients.ItemIndex;

  if geGradientEditor.GradientIndex >= 0 then
  begin
    edtGradientName.Text := glGradients[geGradientEditor.GradientIndex].DisplayName;
  end
  else
  begin
    edtGradientName.Text := '';
  end;
end;

procedure TfrmGradientEditor.glvGradientsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LIndex : Integer;
begin
  LIndex := glvGradients.MatrixIndex(X, Y);

  if glvGradients.GradientList.IsValidIndex(LIndex) then
  begin
    glvGradients.Cursor := crHandPoint;
  end
  else
  begin
    glvGradients.Cursor := crNo;
  end;
end;

procedure TfrmGradientEditor.glvGradientsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if FLastSelectedIndex <> glvGradients.ItemIndex then
  begin
    FModified := True;
  end;
end;

procedure TfrmGradientEditor.btnNewGradientClick(Sender: TObject);
var
  LGradient : TgmGradientItem;
begin
  LGradient := glGradients.Gradients.Add;
  LGradient.Assign(geGradientEditor.Gradient);
  
  LGradient.DisplayName  := edtGradientName.Text;
  glvGradients.ItemIndex := LGradient.Index; //select!

  FModified := True;
end;

procedure TfrmGradientEditor.geGradientEditorCursorPosChanged(
  ASender: TObject; const ACursorPosition: TgmEditorCursorPosition);
begin
  if ASender = geGradientEditor then
  begin
    case ACursorPosition of
      ecpOnStop,
      ecpOnMidPoint:
        begin
          geGradientEditor.Cursor := crHandPoint;
        end;

      ecpOnAddStopArea:
        begin
          geGradientEditor.Cursor := crCross;
        end;

    else
      geGradientEditor.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmGradientEditor.geGradientEditorStateChanged(ASender: TObject;
  const AState: TgmGradientEditorState);
begin
  lblColor.Enabled     := False;
  shpStopColor.Visible := False;

  lblOpacity.Enabled  := False;
  ggbrOpacity.Enabled := False;

  lblColorLocation.Enabled        := False;
  lblColorLocationPercent.Enabled := False;
  edtColorLocation.Enabled        := False;
  btnDeleteSelectedColor.Enabled  := False;

  lblAlphaLocation.Enabled            := False;
  lblAlphaLocationPercent.Enabled     := False;
  edtAlphaLocation.Enabled            := False;
  btnDeleteSelectedAlphaValue.Enabled := False;

  if not (AState in [gesColorStopSelected , gesColorMidPointSelected]) then
  begin
    edtColorLocation.Text  := '';
    edtColorLocation.Color := clBtnFace;
  end
  else
  begin
    edtColorLocation.Color := clWindow;
  end;

  if not (AState in [gesAlphaStopSelected, gesAlphaMidPointSelected]) then
  begin
    edtAlphaLocation.Text  := '';
    edtAlphaLocation.Color := clBtnFace;
  end
  else
  begin
    edtAlphaLocation.Color := clWindow;
  end;

  case AState of
    gesColorStopSelected:
      begin
        case geGradientEditor.DefaultNewColor of
          clDefault :
            begin
              shpStopColor.Brush.Color := glGradients.ForegroundColor;
            end;

          clBackground :
            begin
              shpStopColor.Brush.Color := glGradients.BackgroundColor;
            end;

        else
          shpStopColor.Brush.Color := (geGradientEditor.DefaultNewColor);
        end;
        
        shpStopColor.Visible     := True;
        lblColor.Enabled         := True;

        lblColorLocation.Enabled        := True;
        edtColorLocation.Enabled        := True;
        lblColorLocationPercent.Enabled := True;

        edtColorLocation.Text := IntToStr( Round(geGradientEditor.Gradient.RGBGradient[geGradientEditor.SelectedColorIndex].LocationScale * 100) );

        if geGradientEditor.Gradient.RGBGradient.Count > 2 then
        begin
          btnDeleteSelectedColor.Enabled := True;
        end;
      end;

    gesColorMidPointSelected:
      begin
        lblColorLocation.Enabled        := True;
        edtColorLocation.Enabled        := True;
        lblColorLocationPercent.Enabled := True;

        edtColorLocation.Text := IntToStr( Round(geGradientEditor.Gradient.RGBGradient[geGradientEditor.SelectedMidPointIndex].MidPoint * 100) );
      end;

    gesAlphaStopSelected:
      begin
        ggbrOpacity.Position := Round(geGradientEditor.DefaultNewAlpha / 255 * 100);
        ggbrOpacity.Enabled  := True;
        lblOpacity.Enabled   := True;

        lblAlphaLocation.Enabled        := True;
        edtAlphaLocation.Enabled        := True;
        lblAlphaLocationPercent.Enabled := True;
        edtAlphaLocation.Text := IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[geGradientEditor.SelectedAlphaIndex].LocationScale * 100) );

        if geGradientEditor.Gradient.AlphaGradient.Count > 2 then
        begin
          btnDeleteSelectedAlphaValue.Enabled := True;
        end;
      end;

    gesAlphaMidPointSelected:
      begin
        lblAlphaLocation.Enabled        := True;
        edtAlphaLocation.Enabled        := True;
        lblAlphaLocationPercent.Enabled := True;

        edtAlphaLocation.Text := IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[geGradientEditor.SelectedMidPointIndex].MidPoint * 100) );
      end;
  end;
end;

procedure TfrmGradientEditor.geGradientEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmGradientEditor.geGradientEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  case geGradientEditor.State of
    gesColorStopSelected:
      begin
        edtColorLocation.Text :=
          IntToStr( Round(geGradientEditor.Gradient.RGBGradient[geGradientEditor.SelectedColorIndex].LocationScale * 100) );
      end;

    gesColorMidPointSelected:
      begin
        edtColorLocation.Text :=
          IntToStr( Round(geGradientEditor.Gradient.RGBGradient[geGradientEditor.SelectedMidPointIndex].MidPoint * 100) );
      end;

    gesAlphaStopSelected:
      begin
        edtAlphaLocation.Text :=
          IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[geGradientEditor.SelectedAlphaIndex].LocationScale * 100) );
      end;

    gesAlphaMidPointSelected:
      begin
        edtAlphaLocation.Text :=
          IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[geGradientEditor.SelectedMidPointIndex].MidPoint * 100) );
      end;
  end;
end;

procedure TfrmGradientEditor.geGradientEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := True;
end;

procedure TfrmGradientEditor.ggbrOpacityChange(Sender: TObject);
begin
  lblOpacity.Caption := 'Opacity: ' + IntToStr(ggbrOpacity.Position) + '%';

  geGradientEditor.DefaultNewAlpha := Round(ggbrOpacity.Position / 100 * 255);
  geGradientEditor.ChangeSelectedAlphaValue(geGradientEditor.DefaultNewAlpha);
end;

procedure TfrmGradientEditor.shpStopColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clrdlgColorDialog.Color := shpStopColor.Brush.Color;

  if clrdlgColorDialog.Execute then
  begin
    shpStopColor.Brush.Color         := clrdlgColorDialog.Color;
    geGradientEditor.DefaultNewColor := shpStopColor.Brush.Color;
    geGradientEditor.ChangeSelectedPrimaryColor(geGradientEditor.DefaultNewColor);
  end;
end;

procedure TfrmGradientEditor.edtAlphaLocationChange(Sender: TObject);
var
  LChangedValue : Integer;
  LIndex        : Integer;
begin
  if FAllowChange then
  begin
    try
      case geGradientEditor.State of
        gesAlphaStopSelected:
          begin
            LChangedValue := StrToInt(edtAlphaLocation.Text);

            if (LChangedValue < 0) or (LChangedValue > 100) then
            begin
              MessageDlg('An integer between 0 and 100 is required.' + #10#13 +
                         'Closest value inserted.',
                         mtError, [mbOK], 0);

              LChangedValue := Clamp(LChangedValue, 0, 100);

              edtAlphaLocation.Text := IntToStr(LChangedValue);
            end;
            
            geGradientEditor.ChangeSelectedAlphaLocationScale(LChangedValue / 100);
          end;

        gesAlphaMidPointSelected:
          begin
            LChangedValue := StrToInt(edtAlphaLocation.Text);

            if (LChangedValue < 5) or (LChangedValue > 95) then
            begin
              MessageDlg('An integer between 5 and 95 is required.' + #10#13 +
                         'Closest value inserted.',
                         mtError, [mbOK], 0);

              LChangedValue := Clamp(LChangedValue, 5, 95);

              edtAlphaLocation.Text := IntToStr(LChangedValue);
            end;

            geGradientEditor.ChangeSelectedMidPointScale(LChangedValue / 100);
          end;
      end;

    except
      case geGradientEditor.State of
        gesAlphaStopSelected:
          begin
            LIndex                := geGradientEditor.SelectedAlphaIndex;
            edtAlphaLocation.Text := IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[LIndex].LocationScale * 100) );
          end;

        gesAlphaMidPointSelected:
          begin
            LIndex                := geGradientEditor.SelectedMidPointIndex;
            edtAlphaLocation.Text := IntToStr( Round(geGradientEditor.Gradient.AlphaGradient[LIndex].MidPoint * 100) );
          end;
      end;
    end;
  end;
end;

procedure TfrmGradientEditor.edtColorLocationChange(Sender: TObject);
var
  LChangedValue : Integer;
  LIndex        : Integer;
begin
  if FAllowChange then
  begin
    try
      case geGradientEditor.State of
        gesColorStopSelected:
          begin
            LChangedValue := StrToInt(edtColorLocation.Text);

            if (LChangedValue < 0) or (LChangedValue > 100) then
            begin
              MessageDlg('An integer between 0 and 100 is required.' + #10#13 +
                         'Closest value inserted.',
                         mtError, [mbOK], 0);

              LChangedValue := Clamp(LChangedValue, 0, 100);

              edtColorLocation.Text := IntToStr(LChangedValue);
            end;

            geGradientEditor.ChangeSelectedColorLocationScale(LChangedValue / 100);
          end;

        gesColorMidPointSelected:
          begin
            LChangedValue := StrToInt(edtColorLocation.Text);

            if (LChangedValue < 5) or (LChangedValue > 95) then
            begin
              MessageDlg('An integer between 5 and 95 is required.' + #10#13 +
                         'Closest value inserted.',
                         mtError, [mbOK], 0);

              LChangedValue := Clamp(LChangedValue, 5, 95);

              edtColorLocation.Text := IntToStr(LChangedValue);
            end;

            geGradientEditor.ChangeSelectedMidPointScale(LChangedValue / 100);
          end;
      end;
      
    except
      case geGradientEditor.State of
        gesColorStopSelected:
          begin
            LIndex                := geGradientEditor.SelectedColorIndex;
            edtColorLocation.Text := IntToStr( Round(geGradientEditor.Gradient.RGBGradient[LIndex].LocationScale * 100) );
          end;

        gesColorMidPointSelected:
          begin
            LIndex                := geGradientEditor.SelectedMidPointIndex;
            edtColorLocation.Text := IntToStr( Round(geGradientEditor.Gradient.RGBGradient[LIndex].MidPoint * 100) );
          end;
      end;
    end;
  end;
end;

procedure TfrmGradientEditor.btnDeleteSelectedAlphaValueClick(
  Sender: TObject);
begin
  geGradientEditor.DeleteSelectedAlphaStop;
end;

procedure TfrmGradientEditor.btnDeleteSelectedColorClick(Sender: TObject);
begin
  geGradientEditor.DeleteSelectedColorStop;
end;

procedure TfrmGradientEditor.btnAverageAlphaValuesClick(Sender: TObject);
begin
  geGradientEditor.AlphaStopDistributeAverage;
end;

procedure TfrmGradientEditor.btnAverageColorsClick(Sender: TObject);
begin
  geGradientEditor.ColorStopDistributeAverage;
end;

procedure TfrmGradientEditor.pmnitmDeleteGradientClick(Sender: TObject);
begin
  if Assigned(glvGradients.GradientList) and
     (glvGradients.GradientList.IsValidIndex(glvGradients.ItemIndex)) then
  begin
    glvGradients.GradientList.Gradients.Delete(glvGradients.ItemIndex);
    FModified := True;
  end;
end;

procedure TfrmGradientEditor.btbtnOKClick(Sender: TObject);
begin
  if FModified then
  begin
    if MessageDlg('The Gradients are changed. Do you want to save these changes?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      SaveGradientsSettings;
    end;
  end;
end;

procedure TfrmGradientEditor.btnSaveGradientsClick(Sender: TObject);
var
  LFileName : string;
  LFileExt  : string;
begin
  if frmGradientPicker.sgdSaveGradientDialog.Execute then
  begin
    LFileName := frmGradientPicker.sgdSaveGradientDialog.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );
    
    if LFileExt = '' then
    begin
      LFileName := LFileName + '.grd';
    end
    else
    begin
      if LFileExt <> '.grd' then
      begin
        LFileName := ChangeFileExt(LFileName, '.grd');
      end;
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already existed. Do you want to replace it?',
                     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        Exit;
      end;
    end;

    case FGradientUser of
      guGradientTools:
        begin
          with frmGradientPicker.FDrawingToolGradientListState do
          begin
            FileName := LFileName;
          end;
        end;

      guGradientMapCommand:
        begin
          with frmGradientPicker.FMapCommandGradientListState do
          begin
            FileName := LFileName;
          end;
        end;

      guGradientFillLayer:
        begin
          with frmGradientPicker.FFillLayerGradientListState do
          begin
            FileName := LFileName;
          end;
        end;

      guGradientMapLayer:
        begin
          with frmGradientPicker.FMapLayerGradientListState do
          begin
            FileName := LFileName;
          end;
        end;
    end;

    SaveGradientsSettings;
    FModified := False;
  end;
end;

procedure TfrmGradientEditor.btnLoadGradientsClick(Sender: TObject);
begin
  if frmGradientPicker.ogdOpenGradientDialog.Execute then
  begin
    glGradients.LoadFromFile(frmGradientPicker.ogdOpenGradientDialog.FileName);
  end;
end;

end.
