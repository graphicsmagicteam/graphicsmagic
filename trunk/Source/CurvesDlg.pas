unit CurvesDlg;

{ This library created in 01/27/2006.
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on the Gimp 2.2.10 .
  The original source can be found at www.gimp.org.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

  Thanks to the authors of GIMP for giving us the opportunity to know how to
  achieve Curves Tool. }

// Update Date: 2016/04/20

interface

uses
{ Standard Lib }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, 
{ Graphics32 }
  GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmCurvesLayer,
  gmCurvesTool;

type
  TfrmCurves = class(TForm)
    Panel1: TPanel;
    img32Graph: TImage32;
    img32YRange: TImage32;
    img32XRange: TImage32;
    lblCurvesChannel: TLabel;
    cmbbxCurvesChannel: TComboBox;
    grpbxCurvesType: TGroupBox;
    spdbtnSmoothCurve: TSpeedButton;
    spdbtnFreeCurve: TSpeedButton;
    grpbxHistogramType: TGroupBox;
    spdbtnLinearHistogram: TSpeedButton;
    spdbtnLogHistogram: TSpeedButton;
    GroupBox1: TGroupBox;
    lblCurvesCoord: TLabel;
    btnCurvesResetAllChannels: TButton;
    btnCurveChannelRest: TButton;
    btbtnCancel: TBitBtn;
    btbtnOK: TBitBtn;
    chckbxPreview: TCheckBox;
    btnLoadCurves: TButton;
    btnSaveCurves: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ChangeCurvesChannel(Sender: TObject);
    procedure ChangneCurvesType(Sender: TObject);
    procedure ChangeHistogramType(Sender: TObject);
    procedure btnCurvesResetAllChannelsClick(Sender: TObject);
    procedure btnCurveChannelRestClick(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure btnLoadCurvesClick(Sender: TObject);
    procedure btnSaveCurvesClick(Sender: TObject);
  private
    { Private declarations }
    FNormalCurvesTool     : TgmCurvesTool;   // used for menu command
    FCurvesLayer          : TgmCurvesLayer;  // pointer to a Curves layer 
    FCurvesTool           : TgmCurvesTool;   // pointer to a Curves tool
    FCurvesDrawing        : Boolean;
    FCurvesFileName       : string;          // opened curves file name
    FWorkingOnEffectLayer : Boolean;

    procedure ExecuteCurvesOnSelection;
    procedure ExecuteCurvesOnAlphaChannel;
    procedure ExecuteCurvesOnQuickMask;
    procedure ExecuteCurvesOnLayerMask;
    procedure ExecuteCurvesOnLayer;
    procedure ExecuteCurves;

    procedure ChangeChannelItems; // Change the items in the Channel combobox.
    procedure UpdateChannelComboBoxHint;

    { Mouse events for normal layers, transparent layers and mask. }
    procedure GimpCurvesToolMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);

    procedure GimpCurvesToolMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure GimpCurvesToolMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
  public
    procedure AssociateToCurvesLayer(ALayer: TgmCurvesLayer);
  end;

var
  frmCurves: TfrmCurves;

implementation

uses
{ Graphics32 }
  GR32,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmGimpBaseCurves,
  gmGimpBaseEnums,
  gmGimpColorBar,
  gmGimpCommonFuncs,
  gmGimpHistogram,
  gmGtkEnums,
  gmIni,
  gmLayers,
  gmTypes,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.dfm}

{ Custom Methods }

procedure TfrmCurves.AssociateToCurvesLayer(ALayer: TgmCurvesLayer);
var
  LFlattenedBmp : TBitmap32;
begin
  if Assigned(ALayer) then
  begin
    FCurvesLayer := ALayer;
    
    // We have to blend the layers that beneath this Curves layer
    // into a bitmap to calculate the histogram.

    LFlattenedBmp := ActiveChildForm.LayerList.GetLayerBlendResult(
      0, ActiveChildForm.LayerList.SelectedIndex - 1, True);

    if Assigned(LFlattenedBmp) then
    begin
      FCurvesLayer.CalculateHistogram(LFlattenedBmp);
      LFlattenedBmp.Free;
    end;

    // pointing to the Curves tool that in the Curves layer 
    FCurvesTool := FCurvesLayer.CurvesTool;

    FCurvesTool.CurvesUpdate(DRAW_ALL, img32Graph.Bitmap,
      img32XRange.Bitmap, img32YRange.Bitmap);

    spdbtnLinearHistogram.Down := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LINEAR);
    spdbtnLogHistogram.Down    := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);
    spdbtnSmoothCurve.Down     := (FCurvesTool.CurveType = GIMP_CURVE_SMOOTH);
    spdbtnFreeCurve.Down       := (FCurvesTool.CurveType = GIMP_CURVE_FREE);

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmCurves.ExecuteCurvesOnSelection;
var
  LChannelSet : TgmChannelSet;
begin
  with ActiveChildForm do
  begin
    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      // can not processing on special layers
      if not (LayerList.SelectedLayer is TgmNormalLayer) then
      begin
        Exit;
      end;
    end;

    if ChannelManager.CurrentChannelType = ctColorChannel then
    begin
      LChannelSet := ChannelManager.SelectedColorChannels;
    end
    else
    begin
      LChannelSet := [csGrayscale];
    end;

    if Assigned(FCurvesTool) then
    begin
      FCurvesTool.Map(frmMain.FBitmapAfter, LChannelSet);
    end;
    
    if chckbxPreview.Checked then
    begin
      Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
      ShowProcessedSelection();
    end;
  end;
end;

procedure TfrmCurves.ExecuteCurvesOnAlphaChannel;
begin
  with ActiveChildForm do
  begin
    if Assigned(FCurvesTool) then
    begin
      FCurvesTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
    end;
    
    if chckbxPreview.Checked then
    begin
      if Assigned(ChannelManager.SelectedAlphaChannel) then
      begin
        with ChannelManager.SelectedAlphaChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed();
        end;
      end;
    end;
  end;
end;

procedure TfrmCurves.ExecuteCurvesOnQuickMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(FCurvesTool) then
    begin
      FCurvesTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
    end;
    
    if chckbxPreview.Checked then
    begin
      if Assigned(ChannelManager.QuickMaskChannel) then
      begin
        with ChannelManager.QuickMaskChannel do
        begin
          ChannelLayer.Bitmap.Assign(frmMain.FBitmapAfter);
          ChannelLayer.Bitmap.Changed();
        end;
      end;
    end;
  end;
end;

procedure TfrmCurves.ExecuteCurvesOnLayerMask;
begin
  with ActiveChildForm do
  begin
    if Assigned(FCurvesTool) then
    begin
      FCurvesTool.Map(frmMain.FBitmapAfter, [csGrayscale]);
    end;
    
    if chckbxPreview.Checked then
    begin
      LayerList.SelectedLayer.MaskBitmap.Assign(frmMain.FBitmapAfter);

      // update the layer mask channel
      if Assigned(ChannelManager.LayerMaskChannel) then
      begin
        ChannelManager.LayerMaskChannel.ChannelLayer.Bitmap.Draw(
          0, 0, LayerList.SelectedLayer.MaskBitmap);
      end;

      LayerList.SelectedLayer.Changed;
    end;
  end;
end;

procedure TfrmCurves.ExecuteCurvesOnLayer;
begin
  with ActiveChildForm do
  begin
    if Assigned(FCurvesLayer) and FWorkingOnEffectLayer then
    begin
      FCurvesLayer.CurvesTool.LUTSetup(3);
    end
    else
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
      begin
        FCurvesTool.Map(frmMain.FBitmapAfter,
                        ChannelManager.SelectedColorChannels);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      if Assigned(FCurvesLayer) and FWorkingOnEffectLayer then
      begin
        FCurvesLayer.Changed;
      end
      else
      begin
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
          LayerList.SelectedLayer.Changed();
        end;
      end;
    end;
  end;
end; 

procedure TfrmCurves.ExecuteCurves;
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  with ActiveChildForm do
  begin
    if Assigned(FCurvesLayer) and FWorkingOnEffectLayer then
    begin
      ExecuteCurvesOnLayer;
    end
    else
    begin
      if Assigned(Selection) then
      begin
        ExecuteCurvesOnSelection;
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              ExecuteCurvesOnAlphaChannel;
            end;

          ctQuickMaskChannel:
            begin
              ExecuteCurvesOnQuickMask;
            end;

          ctLayerMaskChannel:
            begin
              ExecuteCurvesOnLayerMask;
            end;

          ctColorChannel:
            begin
              ExecuteCurvesOnLayer;
            end;
        end;
      end;
    end;
  end;
end;

// Change the items in the Channel combobox.
procedure TfrmCurves.ChangeChannelItems;
begin
  cmbbxCurvesChannel.Items.Clear;

  if Assigned(FCurvesLayer) and FWorkingOnEffectLayer then
  begin
    cmbbxCurvesChannel.Items.Add('Value');
    cmbbxCurvesChannel.Items.Add('Red');
    cmbbxCurvesChannel.Items.Add('Green');
    cmbbxCurvesChannel.Items.Add('Blue');
  end
  else
  begin
    with ActiveChildForm do
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              cmbbxCurvesChannel.Items.Add(
                ChannelManager.SelectedAlphaChannel.ChannelName);
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              cmbbxCurvesChannel.Items.Add(
                ChannelManager.QuickMaskChannel.ChannelName);
            end;
          end;

        ctLayerMaskChannel:
          begin
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              cmbbxCurvesChannel.Items.Add(
                ChannelManager.LayerMaskChannel.ChannelName);
            end;
          end;

        ctColorChannel:
          begin
            cmbbxCurvesChannel.Items.Add('Value');
            cmbbxCurvesChannel.Items.Add('Red');
            cmbbxCurvesChannel.Items.Add('Green');
            cmbbxCurvesChannel.Items.Add('Blue');
          end;
      end;
    end;
  end;

  if Assigned(FCurvesTool) then
  begin
    cmbbxCurvesChannel.ItemIndex := FCurvesTool.Channel;
  end;

  UpdateChannelComboBoxHint;
end;

procedure TfrmCurves.UpdateChannelComboBoxHint;
begin
  if Assigned(FCurvesLayer) and FWorkingOnEffectLayer then
  begin
    case cmbbxCurvesChannel.ItemIndex of
      0:
        begin
          cmbbxCurvesChannel.Hint := 'Value';
        end;

      1:
        begin
          cmbbxCurvesChannel.Hint := 'Red Channel';
        end;
        
      2:
        begin
          cmbbxCurvesChannel.Hint := 'Green Channel';
        end;
        
      3:
        begin
          cmbbxCurvesChannel.Hint := 'Blue Channel';
        end;
    end;
  end
  else
  begin
    with ActiveChildForm do
    begin
      case ChannelManager.CurrentChannelType of
        ctAlphaChannel:
          begin
            if Assigned(ChannelManager.SelectedAlphaChannel) then
            begin
              cmbbxCurvesChannel.Hint :=
                ChannelManager.SelectedAlphaChannel.ChannelName;
            end;
          end;

        ctQuickMaskChannel:
          begin
            if Assigned(ChannelManager.QuickMaskChannel) then
            begin
              cmbbxCurvesChannel.Hint :=
                ChannelManager.QuickMaskChannel.ChannelName;
            end;
          end;

        ctLayerMaskChannel:
          begin
            if Assigned(ChannelManager.LayerMaskChannel) then
            begin
              cmbbxCurvesChannel.Hint :=
                ChannelManager.LayerMaskChannel.ChannelName;
            end;
          end;

        ctColorChannel:
          begin
            case cmbbxCurvesChannel.ItemIndex of
              0:
                begin
                  cmbbxCurvesChannel.Hint := 'Value';
                end;

              1:
                begin
                  cmbbxCurvesChannel.Hint := 'Red Channel';
                end;

              2:
                begin
                  cmbbxCurvesChannel.Hint := 'Green Channel';
                end;

              3:
                begin
                  cmbbxCurvesChannel.Hint := 'Blue Channel';
                end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmCurves.GimpCurvesToolMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i               : Integer;
  tx, ty          : Integer;
  xx, yy          : Integer;
  LWidth, LHeight : Integer;
  LClosestPoint   : Integer;
  LDistance       : Integer;
begin
  if FCurvesTool <> nil then
  begin
    LWidth  := img32Graph.Bitmap.Width  - 2 * RADIUS;
    LHeight := img32Graph.Bitmap.Height - 2 * RADIUS;

    {  get the pointer position  }
    tx := X;
    ty := Y;

    xx := Round( (tx - RADIUS) / LWidth  * 255.0);
    yy := Round( (ty - RADIUS) / LHeight * 255.0);

    xx := CLAMP0255(xx);
    yy := CLAMP0255(yy);

    LDistance     := G_MAXINT;
    LClosestPoint := 0;
    
    for i := 0 to (CURVES_NUM_POINTS - 1) do
    begin
      if FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0] <> (-1) then
      begin
        if Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]) < LDistance then
        begin
          LDistance     := Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]);
          LClosestPoint := i;
        end;
      end;
    end;

    if LDistance > MIN_DISTANCE then
    begin
      LClosestPoint := (xx + 8) div 16;
    end;

    case FCurvesTool.Curves.CurveType[FCurvesTool.Channel] of
      GIMP_CURVE_SMOOTH:
        begin
          {  determine the leftmost and rightmost points  }
          FCurvesTool.Leftmost := -1;

          for i := (LClosestPoint - 1) downto 0 do
          begin
            if FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0] <> (-1) then
            begin
              FCurvesTool.Leftmost := FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0];
              Break;
            end;
          end;

          FCurvesTool.Rightmost := 256;

          for i := (LClosestPoint + 1) to (CURVES_NUM_POINTS - 1) do
          begin
            if FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0] <> (-1) then
            begin
              FCurvesTool.Rightmost := FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0];
              Break;
            end;
          end;

          FCurvesTool.GrabPoint := LClosestPoint;
          FCurvesTool.Curves.Points[FCurvesTool.Channel, FCurvesTool.GrabPoint, 0] := xx;
          FCurvesTool.Curves.Points[FCurvesTool.Channel, FCurvesTool.GrabPoint, 1] := 255 - yy;
        end;

      GIMP_CURVE_FREE:
        begin
          FCurvesTool.Curves.FCurve[FCurvesTool.Channel, xx] := 255 - yy;
          FCurvesTool.GrabPoint := xx;
          FCurvesTool.Last      := yy;
        end;
    end;

    FCurvesTool.Curves.CalculateCurve(FCurvesTool.Channel);

    FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);

    FCurvesDrawing := True;
  end;
end;

procedure TfrmCurves.GimpCurvesToolMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  i               : Integer;
  tx, ty          : Integer;
  xx, yy          : Integer;
  LWidth, LHeight : Integer;
  LClosestPoint   : Integer;
  LDistance       : Integer;
  x1, x2, y1, y2  : Integer;
begin
  if FCurvesTool <> nil then
  begin
    LWidth  := img32Graph.Bitmap.Width  - 2 * RADIUS;
    LHeight := img32Graph.Bitmap.Height - 2 * RADIUS;

    {  get the pointer position  }
    tx := X;
    ty := Y;

    xx := Round( (tx - RADIUS) / LWidth  * 255.0);
    yy := Round( (ty - RADIUS) / LHeight * 255.0);

    xx := CLAMP0255(xx);
    yy := CLAMP0255(yy);

    LDistance     := G_MAXINT;
    LClosestPoint := 0;

    for i := 0 to (CURVES_NUM_POINTS - 1) do
    begin
      if FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0] <> (-1) then
      begin
        if Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]) < LDistance then
        begin
          LDistance     := Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]);
          LClosestPoint := i;
        end;
      end;
    end;

    if LDistance > MIN_DISTANCE then
    begin
      LClosestPoint := (xx + 8) div 16;
    end;

    if FCurvesDrawing then  // if mouse left button is pressed...
    begin
      case FCurvesTool.Curves.CurveType[FCurvesTool.Channel] of
        GIMP_CURVE_SMOOTH:
          begin
            { If no point is grabbed...  }
            if FCurvesTool.GrabPoint = (-1) then
            begin
              if FCurvesTool.Curves.Points[FCurvesTool.Channel, LClosestPoint, 0] <> (-1) then
              begin
                Screen.Cursor := crDefault;
              end
              else
              begin
                Screen.Cursor := crSizeAll;
              end;
            end
            {  Else, drag the grabbed point  }
            else
            begin
              Screen.Cursor := crDefault;

              FCurvesTool.Curves.Points[FCurvesTool.Channel, FCurvesTool.GrabPoint, 0] := -1;

              if (xx > FCurvesTool.Leftmost) and (xx < FCurvesTool.Rightmost) then
              begin
                LClosestPoint := (xx + 8) div 16;
                
                if FCurvesTool.Curves.Points[FCurvesTool.Channel, LClosestPoint, 0] = (-1) then
                begin
                  FCurvesTool.GrabPoint := LClosestPoint;
                end;

                FCurvesTool.Curves.Points[FCurvesTool.Channel, FCurvesTool.GrabPoint, 0] := xx;
                FCurvesTool.Curves.Points[FCurvesTool.Channel, FCurvesTool.GrabPoint, 1] := 255 - yy;
              end;

              FCurvesTool.Curves.CalculateCurve(FCurvesTool.Channel);
            end;
          end;

        GIMP_CURVE_FREE:
          begin
            if FCurvesTool.GrabPoint <> (-1) then
            begin
              if FCurvesTool.GrabPoint > xx then
              begin
                x1 := xx;
                x2 := FCurvesTool.GrabPoint;
                y1 := yy;
                y2 := FCurvesTool.Last;
              end
              else
              begin
                x1 := FCurvesTool.GrabPoint;
                x2 := xx;
                y1 := FCurvesTool.Last;
                y2 := yy;
              end;

              if x2 <> x1 then
              begin
                for i := x1 to x2 do
                begin
                  FCurvesTool.Curves.FCurve[FCurvesTool.Channel, i] :=
                    Round(  255 - ( y1 + (y2 - y1) * (i - x1) / (x2 - x1) )  );
                end;
              end
              else
              begin
                FCurvesTool.Curves.FCurve[FCurvesTool.Channel, xx] := 255 - yy;
              end;

              FCurvesTool.GrabPoint := xx;
              FCurvesTool.Last      := yy;
            end;
          end;
      end;

      FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
        img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
    end
    else
    begin  // if mouse left button is released...
      case FCurvesTool.Curves.CurveType[FCurvesTool.Channel] of
        GIMP_CURVE_SMOOTH:
          begin
            { If no point is grabbed...  }
            if FCurvesTool.GrabPoint = (-1) then
            begin
              if FCurvesTool.Curves.Points[FCurvesTool.Channel, LClosestPoint, 0] <> (-1) then
              begin
                Screen.Cursor := crDefault;
              end
              else
              begin
                Screen.Cursor := crSizeAll;
              end;
            end;
          end;

        GIMP_CURVE_FREE:
          begin
            // Do nothing.
          end;
      end;
    end;
    lblCurvesCoord.Caption := Format('X: %d,  Y: %d', [xx, yy]);
  end;
end;

procedure TfrmCurves.GimpCurvesToolMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i         : Integer;
  tx        : Integer;
  xx        : Integer;
  LWidth    : Integer;
  LDistance : Integer;
begin
  if FCurvesTool <> nil then
  begin
    FCurvesDrawing := False;

    LWidth  := img32Graph.Bitmap.Width  - 2 * RADIUS;

    {  get the pointer position  }
    tx := X;
    xx := Round( (tx - RADIUS) / LWidth  * 255.0);
    xx := CLAMP0255(xx);

    LDistance := G_MAXINT;

    for i := 0 to (CURVES_NUM_POINTS - 1) do
    begin
      if FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0] <> -1 then
      begin
        if Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]) < LDistance then
        begin
          LDistance := Abs(xx - FCurvesTool.Curves.Points[FCurvesTool.Channel, i, 0]);
        end;
      end;
    end;

    Screen.Cursor         := crDefault;
    FCurvesTool.GrabPoint := -1;

    ExecuteCurves;
  end;  
end;

procedure TfrmCurves.FormCreate(Sender: TObject);
begin
  img32Graph.Bitmap.SetSize(GRAPH_SIZE, GRAPH_SIZE);
  img32XRange.Bitmap.SetSize(GRAPH_SIZE, BAR_SIZE);
  img32YRange.Bitmap.SetSize(BAR_SIZE, GRAPH_SIZE);

  FCurvesLayer          := nil;
  FNormalCurvesTool     := nil;
  FCurvesTool           := nil;
  FCurvesDrawing        := False;
  FCurvesFileName       := '';
  FWorkingOnEffectLayer := False;
end;

procedure TfrmCurves.FormDestroy(Sender: TObject);
begin
  if Assigned(FNormalCurvesTool) then
  begin
    FNormalCurvesTool.Free;
  end;
end;

procedure TfrmCurves.FormShow(Sender: TObject);
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  // if not working on special layer ...
  if not FWorkingOnEffectLayer then
  begin
    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
      end
      else
      begin
        case ChannelManager.CurrentChannelType of
          ctAlphaChannel:
            begin
              if Assigned(ChannelManager.SelectedAlphaChannel) then
              begin
                frmMain.FBitmapBefore.Assign(
                  ChannelManager.SelectedAlphaChannel.ChannelLayer.Bitmap);
              end;
            end;

          ctQuickMaskChannel:
            begin
              if Assigned(ChannelManager.QuickMaskChannel) then
              begin
                frmMain.FBitmapBefore.Assign(
                  ChannelManager.QuickMaskChannel.ChannelLayer.Bitmap);
              end;
            end;

          ctLayerMaskChannel:
            begin
              frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.MaskBitmap);
            end;

          ctColorChannel:
            begin
              if LayerList.SelectedLayer is TgmNormalLayer then
              begin
                frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
              end;
            end;
        end;
      end;

      frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

      if FNormalCurvesTool <> nil then
      begin
        FreeAndNil(FNormalCurvesTool);
      end;

      // create Curves Tool for menu command
      FNormalCurvesTool := TgmCurvesTool.Create(frmMain.FBitmapBefore);

      // if work with layers...
      if ChannelManager.CurrentChannelType = ctColorChannel then
      begin
        if LayerList.SelectedLayer is TgmNormalLayer then
        begin
          FNormalCurvesTool.Channel := StrToInt(ReadInfoFromIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_CHANNEL, '0'));
        end;
      end;

      FNormalCurvesTool.Scale     := TgmGimpHistogramScale(StrToInt(ReadInfoFromIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_HISTOGRAM, '0')));
      FNormalCurvesTool.CurveType := StrToInt(ReadInfoFromIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_TYPE, '0'));

      // pointing to the Curves Tool that for menu command
      FCurvesTool := FNormalCurvesTool;

      // setting histogram
      FCurvesTool.Hist.gimp_histogram_calculate(frmMain.FBitmapBefore);
      FCurvesTool.Hist.gimp_histogram_view_expose(FCurvesTool.Channel);

      FCurvesTool.CurvesUpdate(DRAW_ALL, img32Graph.Bitmap,
        img32XRange.Bitmap, img32YRange.Bitmap);

      chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_PREVIEW, '1')));
    end;

    spdbtnLinearHistogram.Down := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LINEAR);
    spdbtnLogHistogram.Down    := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);
    spdbtnSmoothCurve.Down     := (FCurvesTool.CurveType = GIMP_CURVE_SMOOTH);
    spdbtnFreeCurve.Down       := (FCurvesTool.CurveType = GIMP_CURVE_FREE);
  end;

  lblCurvesCoord.Caption := '';

  // Change the items in the Channel combobox.
  ChangeChannelItems;

  // Connect mouse events.
  img32Graph.OnMouseDown := GimpCurvesToolMouseDown;
  img32Graph.OnMouseMove := GimpCurvesToolMouseMove;
  img32Graph.OnMouseUp   := GimpCurvesToolMouseUp;

  ActiveControl := btbtnOK;
end;

procedure TfrmCurves.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Disconnect mouse events.
  img32Graph.OnMouseDown := nil;
  img32Graph.OnMouseMove := nil;
  img32Graph.OnMouseUp   := nil;
end; 

procedure TfrmCurves.ChangeCurvesChannel(Sender: TObject);
begin
  if Assigned(FCurvesTool) then
  begin
    FCurvesTool.Channel := cmbbxCurvesChannel.ItemIndex;

    FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_Y_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end; 

procedure TfrmCurves.ChangneCurvesType(Sender: TObject);
var
  LCurveType: Integer;
begin
  LCurveType := 0;

  if Sender = spdbtnSmoothCurve then
  begin
    LCurveType := GIMP_CURVE_SMOOTH;
  end
  else if Sender = spdbtnFreeCurve then
  begin
    LCurveType := GIMP_CURVE_FREE;
  end;

  if Assigned(FCurvesTool) then
  begin
    FCurvesTool.CurveType := LCurveType;
    
    FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end;

procedure TfrmCurves.ChangeHistogramType(Sender: TObject);
begin
  if Assigned(FCurvesTool) then
  begin
    if Sender = spdbtnLinearHistogram then
    begin
      FCurvesTool.Scale := GIMP_HISTOGRAM_SCALE_LINEAR;
    end
    else if Sender = spdbtnLogHistogram then
    begin
      FCurvesTool.Scale := GIMP_HISTOGRAM_SCALE_LOGARITHMIC;
    end;

    FCurvesTool.CurvesUpdate(DRAW_GRAPH, img32Graph.Bitmap,
                             img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end; 

procedure TfrmCurves.btnCurvesResetAllChannelsClick(Sender: TObject);
begin
  if Assigned(FCurvesTool) then
  begin
    FCurvesTool.CurvesAllChannelReset;

    FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH, img32Graph.Bitmap,
                             img32XRange.Bitmap, img32YRange.Bitmap);

    ExecuteCurves;
  end;
end;

procedure TfrmCurves.btnCurveChannelRestClick(Sender: TObject);
begin
  if Assigned(FCurvesTool) then
  begin
    FCurvesTool.CurvesCurrentChannelReset;

    FCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH, img32Graph.Bitmap,
                             img32XRange.Bitmap, img32YRange.Bitmap);

    ExecuteCurves;
  end;
end;

procedure TfrmCurves.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteCurves;
end; 

procedure TfrmCurves.btbtnOKClick(Sender: TObject);
begin
  if not FWorkingOnEffectLayer then
  begin
    if Assigned(FNormalCurvesTool) then
    begin
      WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_CHANNEL,
                         IntToStr(FNormalCurvesTool.Channel));

      WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_HISTOGRAM,
                         IntToStr(Ord(FNormalCurvesTool.Scale)));

      WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_TYPE,
                         IntToStr(FNormalCurvesTool.CurveType));
    end;

    WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_PREVIEW,
                       IntToStr(Integer(chckbxPreview.Checked)));
  end;
end;

procedure TfrmCurves.btnLoadCurvesClick(Sender: TObject);
begin
  if Assigned(FCurvesTool) then
  begin
    OpenDialog.InitialDir := ReadInfoFromIniFile(SECTION_CURVES_DIALOG,
      IDENT_CURVES_FILE_DIR, ExtractFilePath( ParamStr(0) ));

    if OpenDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        FCurvesFileName := OpenDialog.FileName;
        try
          // The following methold must be called first, otherwise, the
          // FCurvesTool.LoadFormFile() will causes an exception.
          // The reason for this problem is not clear, yet. 
          btnCurvesResetAllChannelsClick(Sender);

          if FCurvesTool.LoadFromFile(FCurvesFileName) then
          begin
            if cmbbxCurvesChannel.Items.Count > 1 then
            begin
              cmbbxCurvesChannel.ItemIndex := FCurvesTool.Channel;
            end
            else
            begin
              cmbbxCurvesChannel.ItemIndex := 0;
              ChangeCurvesChannel(Sender);
            end;

            FCurvesTool.CurvesUpdate(DRAW_ALL, img32Graph.Bitmap,
                                     img32XRange.Bitmap, img32YRange.Bitmap);

            spdbtnLinearHistogram.Down := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LINEAR);
            spdbtnLogHistogram.Down    := (FCurvesTool.Scale = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);
            spdbtnSmoothCurve.Down     := (FCurvesTool.CurveType = GIMP_CURVE_SMOOTH);
            spdbtnFreeCurve.Down       := (FCurvesTool.CurveType = GIMP_CURVE_FREE);

            ExecuteCurves;

            WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_FILE_DIR,
                               ExtractFilePath(FCurvesFileName));
          end
          else
          begin
            // Curves file loading error
            MessageDlg(FCurvesTool.OutputMsg, mtError, [mbOK], 0);
          end;

        except
          MessageDlg('Cannot open the file "' + ExtractFileName(OpenDialog.FileName) + '".', mtError, [mbOK], 0)
        end;
        
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfrmCurves.btnSaveCurvesClick(Sender: TObject);
var
  LFileDir        : string;
  LFileExt        : string;
  LOutputFileName : string;
begin
  if Assigned(FCurvesTool) then
  begin
    if FCurvesFileName = '' then
    begin
      SaveDialog.FileName := 'Untitled' + CURVES_FILE_EXT;
    end
    else
    begin
      SaveDialog.FileName := ExtractFileName(FCurvesFileName);
    end;

    SaveDialog.InitialDir := ReadInfoFromIniFile(SECTION_CURVES_DIALOG,
      IDENT_CURVES_FILE_DIR, ExtractFilePath( ParamStr(0) ));

    if SaveDialog.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        LOutputFileName := SaveDialog.FileName;
        LFileDir        := ExtractFileDir(LOutputFileName);

        if (LFileDir <> '') and DirectoryExists(LFileDir) then
        begin
          LFileExt := ExtractFileExt(LOutputFileName);

          if LFileExt = '' then
          begin
            LOutputFileName := LOutputFileName + CURVES_FILE_EXT;
          end
          else
          if LFileExt <> CURVES_FILE_EXT then
          begin
            LOutputFileName := ChangeFileExt(LOutputFileName, CURVES_FILE_EXT);
          end;

          if FileExists(LOutputFileName) then
          begin
            if MessageDlg('The file "' + ExtractFileName(LOutputFileName) + '" is already exists.' + #10#13 +
                          'Do you want to replace it?', mtConfirmation, mbOKCancel, 0) <> mrOK then
            begin
              Exit;
            end;
          end;

          FCurvesTool.SaveToFile(SaveDialog.FileName);

          FCurvesFileName := SaveDialog.FileName;

          WriteInfoToIniFile(SECTION_CURVES_DIALOG, IDENT_CURVES_FILE_DIR,
                             ExtractFilePath(FCurvesFileName));
        end;
        
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end; 

end.
