unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image, gmGradientEditor, ExtCtrls, StdCtrls,
  gmGridBased_List, gmGradient_List, gmGridBased_ListView,
  gmGradient_ListView, Gr32_Layers, ActnList, Menus, gmGradientsGrid,
  gmGridBased_FileDlg, gmGradient_FileDlgs, GR32_RangeBars, Buttons, GR32;

type
  TfrmMain = class(TForm)
    pnlEdit: TPanel;
    grpbxStops: TGroupBox;
    Bevel1: TBevel;
    geGradientEditor: TgmGradientEditor;
    glGradientList: TgmGradientList;
    Bevel2: TBevel;
    pnlNewGradient: TPanel;
    edtGradientName: TEdit;
    btnAdd: TButton;
    lblGradientName: TLabel;
    Panel1: TPanel;
    grpbxPreset: TGroupBox;
    ScrollBox1: TScrollBox;
    rdbtnDirectMode: TRadioButton;
    rdbtnCopyMode: TRadioButton;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    mnitmOptions: TMenuItem;
    mnitmDelete: TMenuItem;
    actDelete: TAction;
    actClear: TAction;
    mnitmClear: TMenuItem;
    N1: TMenuItem;
    mnitmLoadGradients: TMenuItem;
    OpenGradientDialog: TOpenGradientDialog;
    actLoadGradients: TAction;
    actReplaceGradients: TAction;
    mnitmReplaceGradients1: TMenuItem;
    N2: TMenuItem;
    SaveGradientDialog: TSaveGradientDialog;
    actSaveGradients: TAction;
    mnitmSaveGradients: TMenuItem;
    N3: TMenuItem;
    actThumbSize16: TAction;
    mnitmThumbSize16: TMenuItem;
    actThumbSize32: TAction;
    mnitmThumbSize32: TMenuItem;
    actThumbSize48: TAction;
    actThumbSize64: TAction;
    mnitmThumbSize48: TMenuItem;
    mnitmThumbSize64: TMenuItem;
    N4: TMenuItem;
    actExit: TAction;
    mnitmExit: TMenuItem;
    glvGradientListView: TgmGradientListView;
    Label1: TLabel;
    shpForeColor: TShape;
    Label2: TLabel;
    shpBackColor: TShape;
    ColorDialog: TColorDialog;
    lbl2: TLabel;
    lblOpacity: TLabel;
    ggbrOpacity: TGaugeBar;
    lblColor: TLabel;
    pnlPrimaryColor: TPanel;
    shpStopColor: TShape;
    lblAlphaLocation: TLabel;
    lblColorLocation: TLabel;
    edtAlphaLocation: TEdit;
    edtColorLocation: TEdit;
    lblAlphaLocationPercent: TLabel;
    lblColorLocationPercent: TLabel;
    btnDeleteSelectedAlphaValue: TButton;
    btnDeleteSelectedColor: TButton;
    btnAverageAlphaValues: TButton;
    btnAverageColors: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    grp1: TGroupBox;
    imgvwDrawingArea: TImgView32;
    grp2: TGroupBox;
    btnLinearGradient: TSpeedButton;
    btnRadialGradient: TSpeedButton;
    btnAngleGradient: TSpeedButton;
    btnReflectedGradient: TSpeedButton;
    btnDiamondGradient: TSpeedButton;
    lblBlendMode: TLabel;
    lblBlendOpacity: TLabel;
    lblBlendOpacityPercent: TLabel;
    lblChannel: TLabel;
    chkReverse: TCheckBox;
    cbbBlendModes: TComboBox;
    rdbtnDrawNormal: TRadioButton;
    rdbtnDrawBlend: TRadioButton;
    scrollBlendOpacity: TGaugeBar;
    cbbChannel: TComboBox;
    procedure geGradientEditorStateChanged(ASender: TObject;
      const AState: TgmGradientEditorState);
    procedure FormShow(Sender: TObject);
    procedure ChangeEditMode(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure actLoadGradientsExecute(Sender: TObject);
    procedure actReplaceGradientsExecute(Sender: TObject);
    procedure actSaveGradientsExecute(Sender: TObject);
    procedure ChangeThumbSize(Sender: TObject);
    procedure ThumbSizeUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure glvGradientListViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure FormCreate(Sender: TObject);
    procedure geGradientEditorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure glvGradientListViewChange(Sender: TObject);
    procedure shpForeColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure geGradientEditorMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ggbrOpacityChange(Sender: TObject);
    procedure shpStopColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure geGradientEditorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edtAlphaLocationChange(Sender: TObject);
    procedure edtColorLocationChange(Sender: TObject);
    procedure btnDeleteSelectedAlphaValueClick(Sender: TObject);
    procedure btnDeleteSelectedColorClick(Sender: TObject);
    procedure btnAverageAlphaValuesClick(Sender: TObject);
    procedure btnAverageColorsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure imgvwDrawingAreaPaintStage(Sender: TObject;
      Buffer: TBitmap32; StageNum: Cardinal);
    procedure imgvwDrawingAreaMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgvwDrawingAreaMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwDrawingAreaMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure ChangeDrawingModeClick(Sender: TObject);
    procedure scrollBlendOpacityChange(Sender: TObject);
    procedure geGradientEditorCursorPosChanged(ASender: TObject;
      const ACursorPosition: TgmEditorCursorPosition);
  private
    { Private declarations }
    FModified    : Boolean;
    FAllowChange : Boolean;
    FFileName    : string;
    FDrawing     : Boolean;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;

    function ConfirmSave: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ Externals }
  GR32_Add_BlendModes,
{ GraphicsMagic }
  gmGradient, gmGradientRender, gmMiscFuncs, gmTypes;

{$R *.dfm}

function TfrmMain.ConfirmSave: Boolean;
begin
  Result := True;
  
  if FModified and (glGradientList.Count > 0) then
  begin
    case MessageDlg('The gradients have been modified,' + #10#13 +
                    'Do you want to save these changes?', mtConfirmation,
                    mbYesNoCancel, 0) of
      mrYes :
        begin
          if FFileName = '' then
          begin
            actSaveGradientsExecute(Self);
          end
          else
          begin
            glGradientList.SaveToFile(FFileName);
          end;
        end;

      mrCancel :
        begin
          Result := False;
          Abort;
        end;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FAllowChange := True;
  FDrawing     := False;
  FModified    := False;
  FFileName    := '';

  glGradientList.ForegroundColor := shpForeColor.Brush.Color;
  glGradientList.BackgroundColor := shpBackColor.Brush.Color;

  lblOpacity.Enabled              := False;
  ggbrOpacity.Enabled             := False;
  lblColor.Enabled                := False;
  shpStopColor.Visible            := False;
  lblAlphaLocation.Enabled        := False;
  edtAlphaLocation.Enabled        := False;
  lblAlphaLocationPercent.Enabled := False;
  lblColorLocation.Enabled        := False;
  edtColorLocation.Enabled        := False;
  lblColorLocationPercent.Enabled := False;

  if imgvwDrawingArea.PaintStages[0]^.Stage = PST_CLEAR_BACKGND then
  begin
    imgvwDrawingArea.PaintStages[0]^.Stage := PST_Custom;
  end;

  imgvwDrawingArea.Bitmap.SetSize(imgvwDrawingArea.Width - 50,
                                  imgvwDrawingArea.Height - 50);

  imgvwDrawingArea.Bitmap.DrawMode := dmBlend;
  cbbBlendModes.Items              := BlendModeList;
  cbbBlendModes.ItemIndex          := 0;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  rdbtnCopyMode.Checked   := (geGradientEditor.EditMode = eemCopyEdit);
  rdbtnDirectMode.Checked := (geGradientEditor.EditMode = eemDirectEdit);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ConfirmSave;
end;

procedure TfrmMain.geGradientEditorStateChanged(ASender: TObject;
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
              shpStopColor.Brush.Color := glGradientList.ForegroundColor;
            end;

          clBackground :
            begin
              shpStopColor.Brush.Color := glGradientList.BackgroundColor;
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

procedure TfrmMain.ChangeEditMode(Sender: TObject);
begin
  if Sender = rdbtnCopyMode then
  begin
    geGradientEditor.EditMode := eemCopyEdit;
  end
  else if Sender = rdbtnDirectMode then
  begin
    geGradientEditor.EditMode      := eemDirectEdit;
    geGradientEditor.GradientIndex := glvGradientListView.ItemIndex;

    if geGradientEditor.GradientIndex < 0 then
    begin
      edtGradientName.Text := '';
    end;
  end;

  rdbtnCopyMode.Checked   := (geGradientEditor.EditMode = eemCopyEdit);
  rdbtnDirectMode.Checked := (geGradientEditor.EditMode = eemDirectEdit);
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
begin
  if Assigned(glvGradientListView.GradientList) and
     (glvGradientListView.ItemIndex >= 0) then
  begin
    glvGradientListView.GradientList.Gradients.Delete(glvGradientListView.ItemIndex);
    FModified := True;

    if geGradientEditor.EditMode = eemDirectEdit then
    begin
      geGradientEditor.GradientIndex := -1;
      edtGradientName.Text           := '';
    end;
  end;
end;

procedure TfrmMain.actDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (glvGradientListView.ItemIndex >= 0);
end;

procedure TfrmMain.actClearExecute(Sender: TObject);
begin
  glGradientList.Gradients.Clear;
  glvGradientListView.ItemIndex := -1;

  FModified := True;

  if geGradientEditor.EditMode = eemDirectEdit then
  begin
    geGradientEditor.GradientIndex := -1;
    edtGradientName.Text           := '';
  end;
end;

procedure TfrmMain.actClearUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (glGradientList.Gradients.Count > 0);
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  LGradient : TgmGradientItem;
begin
  LGradient := glGradientList.Gradients.Add;
  LGradient.Assign(geGradientEditor.Gradient);
  
  LGradient.DisplayName          := edtGradientName.Text;
  glvGradientListView.ItemIndex  := LGradient.Index; //select!

  if geGradientEditor.EditMode = eemDirectEdit then
  begin
    geGradientEditor.GradientIndex := LGradient.Index;
  end;

  FModified := True;
end;

procedure TfrmMain.actLoadGradientsExecute(Sender: TObject);
begin
  if OpenGradientDialog.Execute then
  begin
    glGradientList.LoadFromFile(OpenGradientDialog.FileName);
    FModified := True;
    FFileName := OpenGradientDialog.FileName;
  end;
end;

procedure TfrmMain.actReplaceGradientsExecute(Sender: TObject);
begin
  if ConfirmSave then
  begin
    if OpenGradientDialog.Execute then
    begin
      glGradientList.Gradients.Clear;
      glGradientList.LoadFromFile(OpenGradientDialog.FileName);

      glvGradientListView.ItemIndex := -1;
      FModified := False;
      FFileName := OpenGradientDialog.FileName;

      if geGradientEditor.EditMode = eemDirectEdit then
      begin
        geGradientEditor.GradientIndex := -1;
        edtGradientName.Text           := '';
      end;
    end;
  end;
end;

procedure TfrmMain.actSaveGradientsExecute(Sender: TObject);
var
  LFileName : string;
  LFileExt  : string;
begin
  if SaveGradientDialog.Execute then
  begin
    LFileName := SaveGradientDialog.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );

    if LFileExt = '' then
    begin
      LFileName := LFileName + '.grd';
    end
    else
    if LFileExt <> '.grd' then
    begin
      LFileName := ChangeFileExt(LFileName, '.grd');
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already exists.' + #10#13 +
                    'Do you want to overwrite it?' + #10#13 + #10#13 +
                    'Note: if the existed file is other types of gradient file,' + #10#13 +
                    'after the file is overwritten, The original application cannot open it again.', mtConfirmation,
                    [mbOK, mbCancel], 0) = mrOK then
      begin
        Screen.Cursor := crHourGlass;
        try
          glGradientList.SaveToFile(LFileName);
          FModified := False;
          FFileName := LFileName;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
    end
    else
    begin
      Screen.Cursor := crHourGlass;
      try
        glGradientList.SaveToFile(LFileName);
        FModified := False;
        FFileName := LFileName;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeThumbSize(Sender: TObject);
begin
  glvGradientListView.SetThumbSize( TAction(Sender).Tag );
end;

procedure TfrmMain.ThumbSizeUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ( TAction(Sender).Tag = glvGradientListView.ThumbWidth );
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.glvGradientListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  geGradientEditor.GradientIndex := glvGradientListView.ItemIndex;

  if geGradientEditor.GradientIndex > (-1) then
  begin
    edtGradientName.Text := glGradientList[geGradientEditor.GradientIndex].DisplayName;
  end
  else
  begin
    edtGradientName.Text := '';
  end;
end;

procedure TfrmMain.glvGradientListViewChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TfrmMain.shpForeColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := shpForeColor.Brush.Color;

  if ColorDialog.Execute then
  begin
    shpForeColor.Brush.Color       := ColorDialog.Color;
    glGradientList.ForegroundColor := ColorDialog.Color;
  end;
end;

procedure TfrmMain.shpBackColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := shpBackColor.Brush.Color;

  if ColorDialog.Execute then
  begin
    shpBackColor.Brush.Color       := ColorDialog.Color;
    glGradientList.BackgroundColor := ColorDialog.Color;
  end;
end;

procedure TfrmMain.ggbrOpacityChange(Sender: TObject);
begin
  lblOpacity.Caption := 'Opacity: ' + IntToStr(ggbrOpacity.Position) + '%';

  geGradientEditor.DefaultNewAlpha := Round(ggbrOpacity.Position / 100 * 255);
  geGradientEditor.ChangeSelectedAlphaValue(geGradientEditor.DefaultNewAlpha);
end;

procedure TfrmMain.shpStopColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := shpStopColor.Brush.Color;

  if ColorDialog.Execute then
  begin
    shpStopColor.Brush.Color         := ColorDialog.Color;
    geGradientEditor.DefaultNewColor := shpStopColor.Brush.Color;
    geGradientEditor.ChangeSelectedPrimaryColor(geGradientEditor.DefaultNewColor);
  end;
end;

procedure TfrmMain.geGradientEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end;

procedure TfrmMain.geGradientEditorMouseMove(Sender: TObject;
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

procedure TfrmMain.geGradientEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(geGradientEditor.Gradients) and
     ( geGradientEditor.Gradients.IsValidIndex(geGradientEditor.GradientIndex) ) and
     ( geGradientEditor.EditMode = eemDirectEdit) then
  begin
    FModified := True;
  end;

  FAllowChange := True;
end;

procedure TfrmMain.edtAlphaLocationChange(Sender: TObject);
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

procedure TfrmMain.edtColorLocationChange(Sender: TObject);
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

procedure TfrmMain.btnDeleteSelectedAlphaValueClick(Sender: TObject);
begin
  geGradientEditor.DeleteSelectedAlphaStop;
end;

procedure TfrmMain.btnDeleteSelectedColorClick(Sender: TObject);
begin
  geGradientEditor.DeleteSelectedColorStop;
end;

procedure TfrmMain.btnAverageAlphaValuesClick(Sender: TObject);
begin
  geGradientEditor.AlphaStopDistributeAverage;
end;

procedure TfrmMain.btnAverageColorsClick(Sender: TObject);
begin
  geGradientEditor.ColorStopDistributeAverage;
end;


procedure TfrmMain.imgvwDrawingAreaPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
var
  LRect: TRect;
begin
  Buffer.Clear( Color32(clBtnFace) );

  LRect := imgvwDrawingArea.GetBitmapRect;
  DrawCheckerboard(Buffer, LRect, 8, $B0B0B0);

  InflateRect(LRect, 1, 1);
  Buffer.FrameRectS(LRect, clBlack32);
end;

procedure TfrmMain.imgvwDrawingAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Button = mbLeft then
  begin
    FStartPoint := Point(X, Y);
    FEndPoint   := Point(X, Y);

    imgvwDrawingArea.Canvas.Pen.Mode := pmNotXor;
    imgvwDrawingArea.Canvas.MoveTo(FStartPoint.X, FStartPoint.Y);
    imgvwDrawingArea.Canvas.LineTo(FEndPoint.X, FEndPoint.Y);

    FDrawing := True;
  end;
end;

procedure TfrmMain.imgvwDrawingAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    // erase old line
    imgvwDrawingArea.Canvas.MoveTo(FStartPoint.X, FStartPoint.Y);
    imgvwDrawingArea.Canvas.LineTo(FEndPoint.X, FEndPoint.Y);

    FEndPoint := Point(X, Y);

    // draw new line
    imgvwDrawingArea.Canvas.MoveTo(FStartPoint.X, FStartPoint.Y);
    imgvwDrawingArea.Canvas.LineTo(FEndPoint.X, FEndPoint.Y);
  end;
end;

procedure TfrmMain.imgvwDrawingAreaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LRender     : TgmGradientRender;
  LStartPoint : TPoint;
  LEndPoint   : TPoint;
  LBmp        : TBitmap32;
begin
  if FDrawing then
  begin
    FDrawing  := False;
    imgvwDrawingArea.Canvas.Pen.Mode := pmCopy;

    LStartPoint := imgvwDrawingArea.ControlToBitmap(FStartPoint);
    LEndPoint   := imgvwDrawingArea.ControlToBitmap(FEndPoint);

    Screen.Cursor := crHourGlass;
    LRender       := TgmGradientRender.Create;
    try
      LRender.ColorGradient := geGradientEditor.Gradient;
      LRender.BlendMode     := TBlendMode32(cbbBlendModes.ItemIndex);
      LRender.Opacity       := scrollBlendOpacity.Position / 100;
      LRender.IsReverse     := chkReverse.Checked;
      LRender.StartPoint    := LStartPoint;
      LRender.EndPoint      := LEndPoint;

      if btnLinearGradient.Down then
      begin
        LRender.RenderMode := grmLinear;
      end
      else
      if btnRadialGradient.Down then
      begin
        LRender.RenderMode := grmRadial;
      end
      else
      if btnAngleGradient.Down then
      begin
        LRender.RenderMode := grmAngle;
      end
      else
      if btnDiamondGradient.Down then
      begin
        LRender.RenderMode := grmDiamond;
      end
      else
      if btnReflectedGradient.Down then
      begin
        LRender.RenderMode := grmReflected;
      end;

      if rdbtnDrawNormal.Checked then
      begin
        LRender.ChannelSet := [csRed, csGreen, csBlue];

        LBmp := TBitmap32.Create;
        try
          LBmp.SetSizeFrom(imgvwDrawingArea.Bitmap);
          LBmp.DrawMode := dmBlend;
          LBmp.Clear($00000000);

          LRender.Render(LBmp);
          imgvwDrawingArea.Bitmap.Assign(LBmp);
        finally
          LBmp.Free;
        end;
      end
      else
      if rdbtnDrawBlend.Checked then
      begin
        case cbbChannel.ItemIndex of
          0:
            begin
              LRender.ChannelSet := [csRed, csGreen, csBlue];
            end;

          1:
            begin
              LRender.ChannelSet := [csRed];
            end;

          2:
            begin
              LRender.ChannelSet := [csGreen];
            end;

          3:
            begin
              LRender.ChannelSet := [csBlue];
            end;
        end;
        
        LRender.Render(imgvwDrawingArea.Bitmap);
      end;

      imgvwDrawingArea.Bitmap.Changed;

    finally
      LRender.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ChangeDrawingModeClick(Sender: TObject);
begin
  lblBlendMode.Enabled           := rdbtnDrawBlend.Checked;
  cbbBlendModes.Enabled          := rdbtnDrawBlend.Checked;
  lblBlendOpacity.Enabled        := rdbtnDrawBlend.Checked;
  scrollBlendOpacity.Enabled     := rdbtnDrawBlend.Checked;
  lblBlendOpacityPercent.Enabled := rdbtnDrawBlend.Checked;
  lblChannel.Enabled             := rdbtnDrawBlend.Checked;
  cbbChannel.Enabled             := rdbtnDrawBlend.Checked;
end;

procedure TfrmMain.scrollBlendOpacityChange(Sender: TObject);
begin
  lblBlendOpacityPercent.Caption := IntToStr(scrollBlendOpacity.Position) + '%';
end;

procedure TfrmMain.geGradientEditorCursorPosChanged(ASender: TObject;
  const ACursorPosition: TgmEditorCursorPosition);
begin
  if ASender = geGradientEditor then
  begin
    case ACursorPosition of
      ecpOnStop, ecpOnMidPoint:
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

end.
