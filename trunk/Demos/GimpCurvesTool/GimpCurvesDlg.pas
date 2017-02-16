{ This library created in 01/27/2006.
  Copyright (c) 2006 by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com).
  All rights reserved.

  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  * 2. The name of the author may not be used to endorse or promote products
  *    derived from this software withough specific prior written permission
  *
  * Based on the the Gimp 2.2.10 .
  * The original source can be found at www.gimp.org.
  *
  * This library is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Library General Public
  * License as published by the Free Software Foundation; either
  * version 2 of the License, or (at your option) any later version.
  *
  * This library is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Library General Public License for more details.
  *
  * You should have received a copy of the GNU Library General Public
  * License along with this library; if not, write to the
  * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.

  Thanks to the authors of GIMP for giving us the opportunity to know how to
  achieve Curves Tool. }

unit GimpCurvesDlg;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  Buttons,
{ Graphics32 }
  GR32, GR32_Layers, GR32_Image,
{ lib }
  gmCurvesTool;

type
  TfrmGimpCurves = class(TForm)
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
    lblCoordinate: TLabel;
    btnCurvesResetAllChannels: TButton;
    btnCurveChannelRest: TButton;
    btbtnCancel: TBitBtn;
    btbtnOK: TBitBtn;
    btnLoadCurves: TButton;
    btnSaveCurves: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure img32GraphMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure img32GraphMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure img32GraphMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ChangeCurvesChannel(Sender: TObject);
    procedure ChangneCurvesType(Sender: TObject);
    procedure ChangeHistogramType(Sender: TObject);
    procedure btnCurvesResetAllChannelsClick(Sender: TObject);
    procedure btnCurveChannelRestClick(Sender: TObject);
    procedure btnLoadCurvesClick(Sender: TObject);
    procedure btnSaveCurvesClick(Sender: TObject);
  private
    { Private declarations }
    FGimpCurvesTool : TgmCurvesTool;
    FCurvesDrawing  : Boolean;
    FCurvesFileName : string;  // opened curves file name

    procedure ExecuteCurves;
  public
    { Public declarations }
    property CurvesTool : TgmCurvesTool read FGimpCurvesTool;
  end;

var
  frmGimpCurves: TfrmGimpCurves;

implementation

uses
{ Delphi }
  Main,
{ lib }
  gmGimpBaseCurves,
  gmGimpBaseEnums,
  gmGimpColorBar,
  gmGimpCommonFuncs,
  gmGimpHistogram,
  gmGtkEnums,
  gmTypes;

{$R *.dfm}

procedure TfrmGimpCurves.ExecuteCurves;
begin
  FGimpCurvesTool.Map(frmMain.imgPreview.Bitmap, [csRed, csGreen, csBlue]);
  frmMain.imgPreview.Bitmap.Changed();
end; 

procedure TfrmGimpCurves.FormCreate(Sender: TObject);
begin
  img32Graph.Bitmap.SetSize(GRAPH_SIZE, GRAPH_SIZE);
  img32XRange.Bitmap.SetSize(GRAPH_SIZE, BAR_SIZE);
  img32YRange.Bitmap.SetSize(BAR_SIZE, GRAPH_SIZE);
  FGimpCurvesTool := nil;
  FCurvesDrawing  := False;
  FCurvesFileName := '';
end; 

procedure TfrmGimpCurves.FormDestroy(Sender: TObject);
begin
  FGimpCurvesTool.Free();
end; 

procedure TfrmGimpCurves.FormShow(Sender: TObject);
begin
  lblCoordinate.Caption := '';

  if Assigned(FGimpCurvesTool) then
  begin
    FreeAndNil(FGimpCurvesTool);
  end;

  FGimpCurvesTool := TgmCurvesTool.Create(frmMain.imgPreview.Bitmap);

  if cmbbxCurvesChannel.ItemIndex <> 0 then
  begin
    cmbbxCurvesChannel.ItemIndex := 0;
  end;

  if spdbtnLogHistogram.Down then
  begin
    spdbtnLinearHistogram.Down := True;
  end;

  if spdbtnFreeCurve.Down then
  begin
    spdbtnSmoothCurve.Down := True;
  end;

  FGimpCurvesTool.CurvesUpdate(DRAW_ALL,
                               img32Graph.Bitmap,
                               img32XRange.Bitmap,
                               img32YRange.Bitmap);
end; 

procedure TfrmGimpCurves.img32GraphMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i, w, h        : Integer;
  tx, ty         : Integer;
  xx, yy         : Integer;
  LClosestPoint  : Integer;
  LDistance      : Integer;
  x1, x2, y1, y2 : Integer;
begin
  w := img32Graph.Bitmap.Width  - 2 * RADIUS;
  h := img32Graph.Bitmap.Height - 2 * RADIUS;

  // get the pointer position 
  tx := X;
  ty := Y;

  xx := Round( (tx - RADIUS) / w * 255.0 );
  yy := Round( (ty - RADIUS) / h * 255.0 );

  xx := CLAMP0255(xx);
  yy := CLAMP0255(yy);

  LDistance     := G_MAXINT;
  LClosestPoint := 0;
  for i := 0 to CURVES_NUM_POINTS - 1 do
  begin
    if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0] <> -1 then
    begin
      if Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]) < LDistance then
      begin
        LDistance     := Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]);
        LClosestPoint := i;
      end;
    end;
  end;

  if LDistance > MIN_DISTANCE then
  begin
    LClosestPoint := (xx + 8) div 16;
  end;

  case FGimpCurvesTool.Curves.CurveType[FGimpCurvesTool.Channel] of
    GIMP_CURVE_SMOOTH:
      begin
        // determine the leftmost and rightmost points... 
        FGimpCurvesTool.Leftmost := -1;
        for i := (LClosestPoint - 1) downto 0 do
        begin
          if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0] <> -1 then
          begin
            FGimpCurvesTool.Leftmost := FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0];
            Break;
          end;
        end;

        FGimpCurvesTool.Rightmost := 256;
        for i := (LClosestPoint + 1) to (CURVES_NUM_POINTS - 1) do
        begin
          if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0] <> -1 then
          begin
            FGimpCurvesTool.Rightmost := FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0];
            Break;
          end;
        end;

        FGimpCurvesTool.GrabPoint := LClosestPoint;
        FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, FGimpCurvesTool.GrabPoint, 0] := xx;
        FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, FGimpCurvesTool.GrabPoint, 1] := 255 - yy;
      end;

    GIMP_CURVE_FREE:
      begin
        FGimpCurvesTool.Curves.FCurve[FGimpCurvesTool.Channel, xx] := 255 - yy;
        FGimpCurvesTool.GrabPoint := xx;
        FGimpCurvesTool.Last      := yy;
      end;
  end;

  FGimpCurvesTool.Curves.CalculateCurve(FGimpCurvesTool.Channel);

  FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
    img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);

  FCurvesDrawing := True;
end; 

procedure TfrmGimpCurves.img32GraphMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  i, w, h        : Integer;
  tx, ty         : Integer;
  xx, yy         : Integer;
  LClosestPoint  : Integer;
  LDistance      : Integer;
  x1, x2, y1, y2 : Integer;
begin
  w := img32Graph.Bitmap.Width  - 2 * RADIUS;
  h := img32Graph.Bitmap.Height - 2 * RADIUS;

  // get the pointer position 
  tx := X;
  ty := Y;

  xx := Round( (tx - RADIUS) / w * 255.0 );
  yy := Round( (ty - RADIUS) / h * 255.0 );

  xx := CLAMP0255(xx);
  yy := CLAMP0255(yy);

  LDistance     := G_MAXINT;
  LClosestPoint := 0;
  for i := 0 to CURVES_NUM_POINTS - 1 do
  begin
    if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0] <> -1 then
    begin
      if Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]) < LDistance then
      begin
        LDistance     := Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]);
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
    case FGimpCurvesTool.Curves.CurveType[FGimpCurvesTool.Channel] of
      GIMP_CURVE_SMOOTH:
        begin
          // if no point is grabbed... 
          if FGimpCurvesTool.GrabPoint = (-1) then
          begin
            if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, LClosestPoint, 0] <> (-1) then
            begin
              Screen.Cursor := crDefault;
            end
            else
            begin
              Screen.Cursor := crSizeAll;
            end;
          end
          else  // else, drag the grabbed point...
          begin
            Screen.Cursor := crDefault;

            FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, FGimpCurvesTool.GrabPoint, 0] := -1;

            if (xx > FGimpCurvesTool.Leftmost) and (xx < FGimpCurvesTool.Rightmost) then
            begin
              LClosestPoint := (xx + 8) div 16;
              if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, LClosestPoint, 0] = (-1) then
              begin
                FGimpCurvesTool.GrabPoint := LClosestPoint;
              end;

              FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, FGimpCurvesTool.GrabPoint, 0] := xx;
              FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, FGimpCurvesTool.GrabPoint, 1] := 255 - yy;
            end;

            FGimpCurvesTool.Curves.CalculateCurve(FGimpCurvesTool.Channel);
          end;
        end;

      GIMP_CURVE_FREE:
        begin
          if FGimpCurvesTool.GrabPoint <> (-1) then
          begin
            if FGimpCurvesTool.GrabPoint > xx then
            begin
              x1 := xx;
              x2 := FGimpCurvesTool.GrabPoint;
              y1 := yy;
              y2 := FGimpCurvesTool.Last;
            end
            else
            begin
              x1 := FGimpCurvesTool.GrabPoint;
              x2 := xx;
              y1 := FGimpCurvesTool.Last;
              y2 := yy;
            end;

            if x2 <> x1 then
            begin
              for i := x1 to x2 do
              begin
                FGimpCurvesTool.Curves.FCurve[FGimpCurvesTool.Channel, i] :=
                  Round(  255 - ( y1 + (y2 - y1) * (i - x1) / (x2 - x1) )  );
              end;
            end
            else
            begin
              FGimpCurvesTool.Curves.FCurve[FGimpCurvesTool.Channel, xx] := 255 - yy;
            end;

            FGimpCurvesTool.GrabPoint := xx;
            FGimpCurvesTool.Last      := yy;
          end;
        end;
    end;

    FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end
  else
  begin  // if mouse left button is released...
    case FGimpCurvesTool.Curves.CurveType[FGimpCurvesTool.Channel] of
      GIMP_CURVE_SMOOTH:
        begin
          { If no point is grabbed...  }
          if FGimpCurvesTool.GrabPoint = (-1) then
          begin
            if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, LClosestPoint, 0] <> (-1) then
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

        end;
    end;
  end;

  lblCoordinate.Caption := Format('X: %d   Y: %d', [xx, yy]);
end; 

procedure TfrmGimpCurves.img32GraphMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  i, w      : Integer;
  tx        : Integer;
  xx        : Integer;
  LDistance : Integer;
begin
  FCurvesDrawing := False;

  w := img32Graph.Bitmap.Width  - 2 * RADIUS;

  {  get the pointer position  }
  tx := X;

  xx := Round( (tx - RADIUS) / w * 255.0 );
  xx := CLAMP0255(xx);

  LDistance := G_MAXINT;
  for i := 0 to (CURVES_NUM_POINTS - 1) do
  begin
    if FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0] <> -1 then
    begin
      if Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]) < LDistance then
      begin
        LDistance := Abs(xx - FGimpCurvesTool.Curves.Points[FGimpCurvesTool.Channel, i, 0]);
      end;
    end;
  end;

  Screen.Cursor             := crDefault;
  FGimpCurvesTool.GrabPoint := -1;

  ExecuteCurves;
end; 

procedure TfrmGimpCurves.ChangeCurvesChannel(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    FGimpCurvesTool.Channel := cmbbxCurvesChannel.ItemIndex;

    FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_Y_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end; 

procedure TfrmGimpCurves.ChangneCurvesType(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    if Sender = spdbtnSmoothCurve then
    begin
      FGimpCurvesTool.CurveType := GIMP_CURVE_SMOOTH;
    end
    else if Sender = spdbtnFreeCurve then
    begin
      FGimpCurvesTool.CurveType := GIMP_CURVE_FREE;
    end;

    FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end; 

procedure TfrmGimpCurves.ChangeHistogramType(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    if Sender = spdbtnLinearHistogram then
    begin
      FGimpCurvesTool.Scale := GIMP_HISTOGRAM_SCALE_LINEAR;
    end
    else if Sender = spdbtnLogHistogram then
    begin
      FGimpCurvesTool.Scale := GIMP_HISTOGRAM_SCALE_LOGARITHMIC;
    end;

    FGimpCurvesTool.CurvesUpdate(DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);
  end;
end; 

procedure TfrmGimpCurves.btnCurvesResetAllChannelsClick(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    FGimpCurvesTool.CurvesAllChannelReset();

    FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);

    ExecuteCurves();
  end;
end; 

procedure TfrmGimpCurves.btnCurveChannelRestClick(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    FGimpCurvesTool.CurvesCurrentChannelReset();

    FGimpCurvesTool.CurvesUpdate(DRAW_X_RANGE or DRAW_GRAPH,
      img32Graph.Bitmap, img32XRange.Bitmap, img32YRange.Bitmap);

    ExecuteCurves();
  end;
end; 

procedure TfrmGimpCurves.btnLoadCurvesClick(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    if OpenDialog.Execute then
    begin
      FCurvesFileName := OpenDialog.FileName;
      try
        // The following methold must be called first, otherwise, the
        // FCurvesTool.LoadFormFile() will causes an exception. The reason for
        // why is not clear, yet. 
        btnCurvesResetAllChannelsClick(Sender);

        if FGimpCurvesTool.LoadFromFile(FCurvesFileName) then
        begin
          if cmbbxCurvesChannel.Items.Count > 1 then
          begin
            cmbbxCurvesChannel.ItemIndex := FGimpCurvesTool.Channel;
          end
          else
          begin
            cmbbxCurvesChannel.ItemIndex := 0;
            ChangeCurvesChannel(Sender);
          end;
          
          FGimpCurvesTool.CurvesUpdate(DRAW_ALL, img32Graph.Bitmap,
            img32XRange.Bitmap, img32YRange.Bitmap);

          spdbtnLinearHistogram.Down := (FGimpCurvesTool.Scale     = GIMP_HISTOGRAM_SCALE_LINEAR);
          spdbtnLogHistogram.Down    := (FGimpCurvesTool.Scale     = GIMP_HISTOGRAM_SCALE_LOGARITHMIC);
          spdbtnSmoothCurve.Down     := (FGimpCurvesTool.CurveType = GIMP_CURVE_SMOOTH);
          spdbtnFreeCurve.Down       := (FGimpCurvesTool.CurveType = GIMP_CURVE_FREE);

          ExecuteCurves();
        end;
      except
        MessageDlg('Cannot open the file "' +
                   ExtractFileName(OpenDialog.FileName) + '".',
                   mtError, [mbOK], 0)
      end;
    end;
  end;
end; 

procedure TfrmGimpCurves.btnSaveCurvesClick(Sender: TObject);
begin
  if Assigned(FGimpCurvesTool) then
  begin
    if FCurvesFileName = '' then
    begin
      SaveDialog.FileName := 'Untitled' + CURVES_FILE_EXT;
    end
    else
    begin
      SaveDialog.FileName := ExtractFileName(FCurvesFileName);
    end;
    
    if SaveDialog.Execute then
    begin
      FGimpCurvesTool.SaveToFile(SaveDialog.FileName);
      FCurvesFileName := SaveDialog.FileName;
    end;
  end;
end;

end.
