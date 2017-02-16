unit MainForm;

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
 * The Original Code is Gradient Editor.
 *
 * The Initial Developer of the Original Code are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 * x2nie - Fathony Luthfillah  <x2nie@yahoo.com>
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
{ Standard }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
{ Graphics32 }
  GR32, GR32_Image, GR32_RangeBars, GR32_Layers,
{ GraphicsMagic Lib }
  GR32_Add_BlendModes, gmGradient, gmGradientEditor, gmGradientManager,
  Menus;

type
  TfrmMain = class(TForm)
    imgGradientEditor: TImage32;
    clrdlgPickColor: TColorDialog;
    grpbxStops: TGroupBox;
    lblColor: TLabel;
    lblOpacity: TLabel;
    ggbrOpacity: TGaugeBar;
    pnlPrimaryColor: TPanel;
    shpPrimaryColor: TShape;
    GroupBox1: TGroupBox;
    imgvwDrawingArea: TImgView32;
    GroupBox2: TGroupBox;
    spdbtnLinearGradient: TSpeedButton;
    spdbtnRadialGradient: TSpeedButton;
    spdbtnAngleGradient: TSpeedButton;
    spdbtnReflectedGradient: TSpeedButton;
    spdbtnDiamondGradient: TSpeedButton;
    chckbxReverse: TCheckBox;
    cmbbxBlendModes: TComboBox;
    rdbtnDrawNormal: TRadioButton;
    rdbtnDrawBlend: TRadioButton;
    ggbrBlendOpacity: TGaugeBar;
    lblBlendMode: TLabel;
    lblBlendOpacity: TLabel;
    lblBlendOpacityPercent: TLabel;
    lblColorLocation: TLabel;
    edtColorLocation: TEdit;
    lblColorLocationPercent: TLabel;
    lblAlphaLocation: TLabel;
    edtAlphaLocation: TEdit;
    lblAlphaLocationPercent: TLabel;
    btnDeleteSelectedColor: TButton;
    btnDeleteSelectedAlphaValue: TButton;
    btnAverageColors: TButton;
    btnAverageAlphaValues: TButton;
    grpbxPresets: TGroupBox;
    imgvwGradientGallery: TImgView32;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    shpForegroundColor: TShape;
    Label2: TLabel;
    shpBackgroundColor: TShape;
    edtGradientName: TEdit;
    btnAddNewGradient: TButton;
    Label3: TLabel;
    opndlgLoadGradients: TOpenDialog;
    svdlgSaveGradients: TSaveDialog;
    lblChannel: TLabel;
    cmbbxChannel: TComboBox;
    pmnGradientOptions: TPopupMenu;
    mnitmSmallThumbnail: TMenuItem;
    mnitmLargeThumbnail: TMenuItem;
    btnGradientOptions: TButton;
    N1: TMenuItem;
    mnitmResetGradients: TMenuItem;
    mnitmLoadGradients: TMenuItem;
    mnitmReplaceGradients: TMenuItem;
    mnitmSaveGradients: TMenuItem;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure shpPrimaryColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ggbrOpacityChange(Sender: TObject);
    procedure imgvwDrawingAreaMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgvwDrawingAreaMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwDrawingAreaMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imgvwDrawingAreaPaintStage(Sender: TObject;
      Buffer: TBitmap32; StageNum: Cardinal);
    procedure ggbrBlendOpacityChange(Sender: TObject);
    procedure ChangeDrawingModeClick(Sender: TObject);
    procedure edtColorLocationChange(Sender: TObject);
    procedure edtAlphaLocationChange(Sender: TObject);
    procedure btnDeleteSelectedColorClick(Sender: TObject);
    procedure btnDeleteSelectedAlphaValueClick(Sender: TObject);
    procedure btnAverageColorsClick(Sender: TObject);
    procedure btnAverageAlphaValuesClick(Sender: TObject);
    procedure shpForegroundColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shpBackgroundColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgvwGradientGalleryMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwGradientGalleryMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure edtGradientNameChange(Sender: TObject);
    procedure btnAddNewGradientClick(Sender: TObject);
    procedure btnGradientOptionsClick(Sender: TObject);
    procedure pmnGradientOptionsPopup(Sender: TObject);
    procedure mnitmSmallThumbnailClick(Sender: TObject);
    procedure mnitmLargeThumbnailClick(Sender: TObject);
    procedure mnitmLoadGradientsClick(Sender: TObject);
    procedure mnitmResetGradientsClick(Sender: TObject);
    procedure mnitmReplaceGradientsClick(Sender: TObject);
    procedure mnitmSaveGradientsClick(Sender: TObject);
  private
    { Private declarations }
    FGradientEditor : TgmGradientEditor;
    FGradientManager: TgmColorGradientManager;

    FDrawing       : Boolean;
    FStartPoint    : TPoint;
    FEndPoint      : TPoint;

    procedure ChangeCursor(const ACursorPosition: TgmEditorCursorPosition);
    procedure ChangeState(const AState: TgmGradientEditorState);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Graphics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmMiscFuncs, gmGradientRender, gmTypes;

{$R *.dfm}

procedure TfrmMain.ChangeCursor(const ACursorPosition: TgmEditorCursorPosition);
begin
  case ACursorPosition of
    ecpOnStop, ecpOnMidPoint:
      begin
        Screen.Cursor := crHandPoint;
      end;

    ecpOnAddStopArea:
      begin
        Screen.Cursor := crCross;
      end;

  else
    Screen.Cursor := crDefault;
  end;
end; { ChangeCursor }

procedure TfrmMain.ChangeState(const AState: TgmGradientEditorState);
begin
  lblColor.Enabled        := False;
  shpPrimaryColor.Visible := False;

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

  if not (AState in [gesPrimaryColorSelected, gesColorMidPointSelected]) then
  begin
    edtColorLocation.Text  := '';
    edtColorLocation.Color := clBtnFace;
  end
  else
  begin
    edtColorLocation.Color := clWindow;
  end;

  if not (AState in [gesPrimaryAlphaSelected, gesAlphaMidPointSelected]) then
  begin
    edtAlphaLocation.Text := '';
    edtAlphaLocation.Color := clBtnFace;
  end
  else
  begin
    edtAlphaLocation.Color := clWindow;
  end;

  case AState of
    gesPrimaryColorSelected:
      begin
        shpPrimaryColor.Brush.Color := WinColor(FGradientEditor.DefaultNewColor);
        shpPrimaryColor.Visible     := True;
        lblColor.Enabled            := True;

        lblColorLocation.Enabled        := True;
        edtColorLocation.Enabled        := True;
        lblColorLocationPercent.Enabled := True;
        edtColorLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedPrimaryColorLocationScale * 100) );

        if FGradientEditor.ColorCount > 2 then
        begin
          btnDeleteSelectedColor.Enabled := True;
        end;
      end;

    gesColorMidPointSelected:
      begin
        lblColorLocation.Enabled        := True;
        edtColorLocation.Enabled        := True;
        lblColorLocationPercent.Enabled := True;

        edtColorLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedColorMidPointScale * 100) );
      end;

    gesPrimaryAlphaSelected:
      begin
        ggbrOpacity.Position := Round(FGradientEditor.DefaultNewAlpha / 255 * 100);
        ggbrOpacity.Enabled  := True;
        lblOpacity.Enabled   := True;

        lblAlphaLocation.Enabled        := True;
        edtAlphaLocation.Enabled        := True;
        lblAlphaLocationPercent.Enabled := True;
        edtAlphaLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedAlphaLocationScale * 100) );

        if FGradientEditor.AlphaValueCount > 2 then
        begin
          btnDeleteSelectedAlphaValue.Enabled := True;
        end;
      end;

    gesAlphaMidPointSelected:
      begin
        lblAlphaLocation.Enabled        := True;
        edtAlphaLocation.Enabled        := True;
        lblAlphaLocationPercent.Enabled := True;

        edtAlphaLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedAlphaMidPointScale * 100) );
      end;
  end;
end; { ChangeState }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FGradientManager := TgmColorGradientManager.Create;
  FGradientManager.SetGalleryMapWidth(5);
  FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);

  FGradientEditor := TgmGradientEditor.Create(imgGradientEditor);
  FGradientEditor.OnCursorPosChanged := ChangeCursor;
  FGradientEditor.OnStateChanged     := ChangeState;
  FGradientEditor.DefaultNewColor    := Color32(shpPrimaryColor.Brush.Color);

  FGradientEditor.UpdatePreview;

  imgGradientEditor.OnMouseDown := FGradientEditor.MouseDown;
  imgGradientEditor.OnMouseMove := FGradientEditor.MouseMove;
  imgGradientEditor.OnMouseUp   := FGradientEditor.MouseUp;

  if imgvwDrawingArea.PaintStages[0]^.Stage = PST_CLEAR_BACKGND then
  begin
    imgvwDrawingArea.PaintStages[0]^.Stage := PST_Custom;
  end;

  imgvwDrawingArea.Bitmap.SetSize(imgvwDrawingArea.Width - 50,
                                  imgvwDrawingArea.Height - 50);

  imgvwDrawingArea.Bitmap.DrawMode := dmBlend;
  
  FDrawing := False;

  cmbbxBlendModes.Items     := BlendModeList;
  cmbbxBlendModes.ItemIndex := 0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  imgGradientEditor.OnMouseDown := nil;
  imgGradientEditor.OnMouseMove := nil;
  imgGradientEditor.OnMouseUp   := nil;

  FGradientEditor.Free;
  FGradientManager.Free;
end;

procedure TfrmMain.shpPrimaryColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clrdlgPickColor.Color := shpPrimaryColor.Brush.Color;

  if clrdlgPickColor.Execute then
  begin
    shpPrimaryColor.Brush.Color     := clrdlgPickColor.Color;
    FGradientEditor.DefaultNewColor := Color32(shpPrimaryColor.Brush.Color);
    FGradientEditor.ChangeSelectedPrimaryColor(FGradientEditor.DefaultNewColor);
  end;
end;

procedure TfrmMain.ggbrOpacityChange(Sender: TObject);
begin
  lblOpacity.Caption := 'Opacity: ' + IntToStr(ggbrOpacity.Position) + '%';

  FGradientEditor.DefaultNewAlpha := Round(ggbrOpacity.Position / 100 * 255);
  FGradientEditor.ChangeSelectedAlphaValue(FGradientEditor.DefaultNewAlpha);
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
  LRender    : TgmGradientRender;
  LStartPoint: TPoint;
  LEndPoint  : TPoint;
  LBmp       : TBitmap32;
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
      LRender.ColorGradient := FGradientEditor.ColorGradient;
      LRender.BlendMode     := TBlendMode32(cmbbxBlendModes.ItemIndex);
      LRender.Opacity       := ggbrBlendOpacity.Position / 100;
      LRender.IsReverse     := chckbxReverse.Checked;
      LRender.StartPoint    := LStartPoint;
      LRender.EndPoint      := LEndPoint;

      if spdbtnLinearGradient.Down then
      begin
        LRender.RenderMode := grmLinear;
      end
      else
      if spdbtnRadialGradient.Down then
      begin
        LRender.RenderMode := grmRadial;
      end
      else
      if spdbtnAngleGradient.Down then
      begin
        LRender.RenderMode := grmAngle;
      end
      else
      if spdbtnDiamondGradient.Down then
      begin
        LRender.RenderMode := grmDiamond;
      end
      else
      if spdbtnReflectedGradient.Down then
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
        case cmbbxChannel.ItemIndex of
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

procedure TfrmMain.imgvwDrawingAreaPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
var
  LRect: TRect;
begin
  Buffer.Clear( Color32(clBtnFace) );

  LRect := imgvwDrawingArea.GetBitmapRect;
  DrawCheckerboardPattern(Buffer, LRect);

  InflateRect(LRect, 1, 1);
  Buffer.FrameRectS(LRect, clBlack32);
end;

procedure TfrmMain.ggbrBlendOpacityChange(Sender: TObject);
begin
  lblBlendOpacityPercent.Caption := IntToStr(ggbrBlendOpacity.Position) + '%';
end;

procedure TfrmMain.ChangeDrawingModeClick(Sender: TObject);
begin
  lblBlendMode.Enabled           := rdbtnDrawBlend.Checked;
  cmbbxBlendModes.Enabled        := rdbtnDrawBlend.Checked;
  lblBlendOpacity.Enabled        := rdbtnDrawBlend.Checked;
  ggbrBlendOpacity.Enabled       := rdbtnDrawBlend.Checked;
  lblBlendOpacityPercent.Enabled := rdbtnDrawBlend.Checked;
  lblChannel.Enabled             := rdbtnDrawBlend.Checked;
  cmbbxChannel.Enabled           := rdbtnDrawBlend.Checked;
end;

procedure TfrmMain.edtColorLocationChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if not FGradientEditor.IsMouseButtonDown then
  begin
    try
      case FGradientEditor.State of
        gesPrimaryColorSelected:
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

            FGradientEditor.ChangeSelectedPrimaryColorLocationScale(LChangedValue / 100);
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

            FGradientEditor.ChangeSelectedColorMidPointScale(LChangedValue / 100);
          end;
      end;
      
    except
      case FGradientEditor.State of
        gesPrimaryColorSelected:
          begin
            edtColorLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedPrimaryColorLocationScale * 100) );
          end;

        gesColorMidPointSelected:
          begin
            edtColorLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedColorMidPointScale * 100) );
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.edtAlphaLocationChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  if not FGradientEditor.IsMouseButtonDown then
  begin
    try
      case FGradientEditor.State of
        gesPrimaryAlphaSelected:
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

            FGradientEditor.ChangeSelectedAlphaLocationScale(LChangedValue / 100);
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

            FGradientEditor.ChangeSelectedAlphaMidPointScale(LChangedValue / 100);
          end;
      end;

    except
      case FGradientEditor.State of
        gesPrimaryAlphaSelected:
          begin
            edtAlphaLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedAlphaLocationScale * 100) );
          end;

        gesAlphaMidPointSelected:
          begin
            edtAlphaLocation.Text := IntToStr( Round(FGradientEditor.GetSelectedAlphaMidPointScale * 100) );
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.btnDeleteSelectedColorClick(Sender: TObject);
begin
  if FGradientEditor.State = gesPrimaryColorSelected then
  begin
    FGradientEditor.DeleteSelectedPrimaryColor;
  end;
end;

procedure TfrmMain.btnDeleteSelectedAlphaValueClick(Sender: TObject);
begin
  if FGradientEditor.State = gesPrimaryAlphaSelected then
  begin
    FGradientEditor.DeleteSelectedAlphaValue;
  end;
end;

procedure TfrmMain.btnAverageColorsClick(Sender: TObject);
begin
  FGradientEditor.AverageColorLocationScales;
end;

procedure TfrmMain.btnAverageAlphaValuesClick(Sender: TObject);
begin
  FGradientEditor.AverageAlphaLocationScales;
end;

procedure TfrmMain.shpForegroundColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clrdlgPickColor.Color := shpForegroundColor.Brush.Color;

  if clrdlgPickColor.Execute then
  begin
    shpForegroundColor.Brush.Color   := clrdlgPickColor.Color;
    FGradientManager.ForegroundColor := Color32(shpForegroundColor.Brush.Color);

    FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);

    FGradientEditor.ColorGradient.RGBGradient.ForegroundColor := Color32(shpForegroundColor.Brush.Color);
    FGradientEditor.UpdatePreview;
  end;
end;

procedure TfrmMain.shpBackgroundColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clrdlgPickColor.Color := shpBackgroundColor.Brush.Color;

  if clrdlgPickColor.Execute then
  begin
    shpBackgroundColor.Brush.Color   := clrdlgPickColor.Color;
    FGradientManager.BackgroundColor := Color32(shpBackgroundColor.Brush.Color);

    FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);

    FGradientEditor.ColorGradient.RGBGradient.BackgroundColor := Color32(shpBackgroundColor.Brush.Color);
    FGradientEditor.UpdatePreview;
  end;
end;

procedure TfrmMain.imgvwGradientGalleryMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LIndex   : Integer;
  LBmpCoord: TPoint;
begin
  LBmpCoord := imgvwGradientGallery.ControlToBitmap( Point(X, Y) );

  if FGradientManager.GradientCount > 0 then
  begin
    LIndex := FGradientManager.GetGradientIndexByCoord(LBmpCoord.X, LBmpCoord.Y);
    
    if (LIndex >= 0) and (LIndex < FGradientManager.GradientCount) and
       (LBmpCoord.X >= 0) and (LBmpCoord.X < imgvwGradientGallery.Bitmap.Width) and
       (LBmpCoord.Y >= 0) and (LBmpCoord.Y < imgvwGradientGallery.Bitmap.Height)  then
    begin
      imgvwGradientGallery.Cursor := crHandPoint;
    end
    else
    begin
      imgvwGradientGallery.Cursor := crNo;
    end;
  end;
end;

procedure TfrmMain.imgvwGradientGalleryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LIndex        : Integer;
  LBmpCoord     : TPoint;
  LColorGradient: TgmColorGradient;
begin
  LBmpCoord := imgvwGradientGallery.ControlToBitmap( Point(X, Y) );

  if Button = mbLeft then
  begin
    if FGradientManager.GradientCount > 0 then
    begin
      LIndex := FGradientManager.GetGradientIndexByCoord(LBmpCoord.X, LBmpCoord.Y);

      if (LIndex >= 0) and (LIndex < FGradientManager.GradientCount) and
         (LBmpCoord.X >= 0) and (LBmpCoord.X < imgvwGradientGallery.Bitmap.Width) and
         (LBmpCoord.Y >= 0) and (LBmpCoord.Y < imgvwGradientGallery.Bitmap.Height)  then
      begin
        LColorGradient := FGradientManager.CopyGradientByIndex(LIndex);

        if Assigned(LColorGradient) then
        begin
          FGradientEditor.ColorGradient := LColorGradient;
          FGradientEditor.UpdatePreview;

          edtGradientName.Text := LColorGradient.Name;
        end;
      end
    end;
  end;
end;

procedure TfrmMain.edtGradientNameChange(Sender: TObject);
begin
  FGradientEditor.ColorGradient.Name := edtGradientName.Text;
end;

procedure TfrmMain.btnAddNewGradientClick(Sender: TObject);
var
  LNewGradient: TgmColorGradient;
begin
  LNewGradient := TgmColorGradient.Create;
  LNewGradient.Assign(FGradientEditor.ColorGradient);
    
  FGradientManager.Add(LNewGradient);
  FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
end;

procedure TfrmMain.btnGradientOptionsClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  pmnGradientOptions.Popup(p.X, p.Y);
end;

procedure TfrmMain.pmnGradientOptionsPopup(Sender: TObject);
begin
  mnitmSmallThumbnail.Checked := (FGradientManager.ThumbnailSizeMode = tsmSmall);
  mnitmLargeThumbnail.Checked := (FGradientManager.ThumbnailSizeMode = tsmLarge);
  mnitmResetGradients.Enabled := not FGradientManager.IsResetted;
end;

procedure TfrmMain.mnitmSmallThumbnailClick(Sender: TObject);
begin
  FGradientManager.ThumbnailSizeMode := tsmSmall;
  FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
end;

procedure TfrmMain.mnitmLargeThumbnailClick(Sender: TObject);
begin
  FGradientManager.ThumbnailSizeMode := tsmLarge;
  FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
end;

procedure TfrmMain.mnitmLoadGradientsClick(Sender: TObject);
begin
  if opndlgLoadGradients.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if FGradientManager.LoadFromFile(opndlgLoadGradients.FileName) then
      begin
        FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
      end
      else
      begin
        MessageDlg(FGradientManager.OuputMsg, mtError, [mbOK], 0);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.mnitmResetGradientsClick(Sender: TObject);
begin
  FGradientManager.ResetGradients;
  FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
end;

procedure TfrmMain.mnitmReplaceGradientsClick(Sender: TObject);
begin
  if opndlgLoadGradients.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      if FGradientManager.ReplaceGradients(opndlgLoadGradients.FileName) then
      begin
        FGradientManager.DrawGradientGalleryMap(imgvwGradientGallery.Bitmap);
      end
      else
      begin
        MessageDlg(FGradientManager.OuputMsg, mtError, [mbOK], 0);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;  
end;

procedure TfrmMain.mnitmSaveGradientsClick(Sender: TObject);
var
  LFileName: string;
  LFileExt : string;
begin
  if svdlgSaveGradients.Execute then
  begin
    LFileName := svdlgSaveGradients.FileName;
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
                    'Note: if the existed file is a Photoshop gradient file,' + #10#13 +
                    'after the file is overwritten, Photoshop cannot open it again.', mtConfirmation,
                    [mbOK, mbCancel], 0) = mrOK then
      begin
        Screen.Cursor := crHourGlass;
        try
          FGradientManager.SaveToFile(LFileName);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
    end
    else
    begin
      Screen.Cursor := crHourGlass;
      try
        FGradientManager.SaveToFile(LFileName);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

end.
