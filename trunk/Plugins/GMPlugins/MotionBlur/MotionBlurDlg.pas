{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved. }

unit MotionBlurDlg;

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

// Updated Date: 2017/01/24

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, GR32, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmMotionBlur = class(TForm)
    GroupBox1: TGroupBox;
    pnlImageHolder: TPanel;
    imgCircle: TImage;
    lblAngle: TLabel;
    edtAngle: TEdit;
    updwnAngle: TUpDown;
    lblDegrees: TLabel;
    lblPixels: TLabel;
    updwnRadius: TUpDown;
    edtRadius: TEdit;
    lblRadius: TLabel;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtAngleChange(Sender: TObject);
    procedure edtRadiusChange(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure imgCircleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgCircleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgCircleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnAngleRadiusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure updwnAngleRadiusMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FAngle, FRadius, FCircleRadius            : Integer;
    FDrawing, FAllowChange                    : Boolean;
    FCenterPoint, FDistanceStart, FDistanceEnd: TPoint;
  public
    FUpdateViewProc: TUpdateViewProc;
    FSourceBmp     : TBitmap32;
    FProcessedBmp  : TBitmap32;
    FDestBmpPtr    : PColor32;
    FChannelSet    : TgmChannelSet;

    procedure ExecuteMotionBlur;
  end;

var
  frmMotionBlur: TfrmMotionBlur;

implementation

uses
{ Standard }
  Math, IniFiles,
{ externals }
  LineLibrary,
{ GraphicsMagic Lib }
  gmMotionBlur,
  gmPluginFuncs,
  gmImageProcessFuncs,
  gmPaintFuncs,
  gmMath;

const
  INI_SECTION                  = 'MotionBlurSettings';
  INI_IDENT_MOTION_BLUR_ANGLE  = 'Angle';
  INI_IDENT_MOTION_BLUR_RADIUS = 'Radius';

  EDGE_PEN_WIDTH     : Integer = 2;
  DISTANCE_PEN_WIDTH : Integer = 1;
  CENTER_POINT_RADIUS: Integer = 2;
  CIRCLE_PEN_COLOR   : TColor = clBlack;
  CIRCLE_BRUSH_COLOR : TColor = clBlack;

{$R *.dfm}

procedure TfrmMotionBlur.ExecuteMotionBlur;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    MotionBlur(FSourceBmp, FProcessedBmp, FRadius, FAngle);

    if csGrayscale in FChannelSet then
    begin
      Desaturate32(FProcessedBmp);
    end
    else
    begin
      LColorChannelCount := GetColorChannelCount(FChannelSet);

      if (LColorChannelCount > 0) and (LColorChannelCount < 3) then
      begin
        ReplaceRGBChannels(FSourceBmp, FProcessedBmp, FChannelSet, crsRemainDest);
      end;
    end;

    CopyBmpDataToPtr(FProcessedBmp, FDestBmpPtr, FProcessedBmp.Width, FProcessedBmp.Height);

    if Assigned(FUpdateViewProc) then
    begin
      FUpdateViewProc;
    end;
    
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMotionBlur.FormCreate(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;

  imgCircle.Picture.Bitmap.Width  := imgCircle.Width;
  imgCircle.Picture.Bitmap.Height := imgCircle.Height;

  FCenterPoint   := Point(imgCircle.Width div 2, imgCircle.Height div 2);
  FDistanceStart := Point(0, 0);
  FDistanceEnd   := Point(0, 0);
  FCircleRadius  := MinIntValue([imgCircle.Width div 2, imgCircle.Height div 2]);
  FDrawing       := False;
  FChannelSet    := [csRed, csGreen, csBlue];
  FAllowChange   := True;
end;

procedure TfrmMotionBlur.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;

  FSourceBmp.Free;
  FProcessedBmp.Free;
end;

procedure TfrmMotionBlur.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    FRadius := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_MOTION_BLUR_RADIUS, 10);
    FAngle  := LIniFile.ReadInteger(INI_SECTION, INI_IDENT_MOTION_BLUR_ANGLE, 0);
  finally
    LIniFile.Free;
  end;

  updwnRadius.Position := FRadius;
  updwnAngle.Position  := FAngle;

  DrawDistanceCircle(imgCircle.Picture.Bitmap.Canvas, FCenterPoint, FCircleRadius,
                     FAngle, EDGE_PEN_WIDTH, DISTANCE_PEN_WIDTH, CENTER_POINT_RADIUS,
                     CIRCLE_PEN_COLOR, CIRCLE_BRUSH_COLOR, frmMotionBlur.Color, psSolid, pmCopy,
                     bsClear, FDistanceStart, FDistanceEnd);

  FProcessedBmp.Width  := FSourceBmp.Width;
  FProcessedBmp.Height := FSourceBmp.Height;
  ExecuteMotionBlur;
  ActiveControl := btbtnOK;
end;

procedure TfrmMotionBlur.edtAngleChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtAngle.Text);
    ClampValue(LChangedValue, updwnAngle.Min, updwnAngle.Max);

    updwnAngle.Position := LChangedValue;
    edtAngle.Text       := IntToStr(LChangedValue);
    FAngle              := LChangedValue;

    if FAllowChange then
    begin
      ExecuteMotionBlur;
    end;
    
  except
    updwnAngle.Position := FAngle;
  end;

  DrawDistanceCircle(imgCircle.Picture.Bitmap.Canvas, FCenterPoint, FCircleRadius,
                     FAngle, EDGE_PEN_WIDTH, DISTANCE_PEN_WIDTH, CENTER_POINT_RADIUS,
                     CIRCLE_PEN_COLOR, CIRCLE_BRUSH_COLOR, frmMotionBlur.Color, psSolid, pmCopy,
                     bsClear, FDistanceStart, FDistanceEnd);
end; 

procedure TfrmMotionBlur.edtRadiusChange(Sender: TObject);
var
  LChangedValue: Integer;
begin
  try
    LChangedValue := StrToInt(edtRadius.Text);
    ClampValue(LChangedValue, updwnRadius.Min, updwnRadius.Max);

    updwnRadius.Position := LChangedValue;
    edtRadius.Text       := IntToStr(LChangedValue);
    FRadius              := LChangedValue;

    if FAllowChange then
    begin
      ExecuteMotionBlur;
    end;
    
  except
    updwnRadius.Position := FRadius;
  end;

  DrawDistanceCircle(imgCircle.Picture.Bitmap.Canvas, FCenterPoint, FCircleRadius,
                     FAngle, EDGE_PEN_WIDTH, DISTANCE_PEN_WIDTH, CENTER_POINT_RADIUS,
                     CIRCLE_PEN_COLOR, CIRCLE_BRUSH_COLOR, frmMotionBlur.Color, psSolid, pmCopy,
                     bsClear, FDistanceStart, FDistanceEnd);
end; 

procedure TfrmMotionBlur.btbtnOKClick(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_MOTION_BLUR_RADIUS, FRadius);
    LIniFile.WriteInteger(INI_SECTION, INI_IDENT_MOTION_BLUR_ANGLE, FAngle);
  finally
    LIniFile.Free;
  end;
end;

procedure TfrmMotionBlur.imgCircleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDrawing := NearLine( Point(X, Y), FDistanceStart, FDistanceEnd);

  if FDrawing then
  begin
    FAllowChange := False;
  end;
end;

procedure TfrmMotionBlur.imgCircleMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LAngle: Integer;
begin
  if NearLine( Point(X, Y), FDistanceStart, FDistanceEnd) then
  begin
    Screen.Cursor := crHandPoint;
  end
  else
  begin
    Screen.Cursor := crDefault;
  end;
  
  if FDrawing then
  begin
    GetAcuteAngle( FCenterPoint, Point(X, Y), LAngle );
    updwnAngle.Position := LAngle;
  end;
end; 

procedure TfrmMotionBlur.imgCircleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDrawing then
  begin
    FDrawing     := False;
    FAllowChange := True;

    ExecuteMotionBlur;
  end;
end; 

procedure TfrmMotionBlur.updwnAngleRadiusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAllowChange := False;
end; 

procedure TfrmMotionBlur.updwnAngleRadiusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteMotionBlur;
  FAllowChange := True;
end; 

end.
