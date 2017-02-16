unit Main;

{ This library created in 2006/09/07.
  Copyright (c) 2006 by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com).
  All rights reserved.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis,
  for giving us the opportunity to know how to achieve Levels Tool.

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
  * Boston, MA 02111-1307, USA. }

// Update Date: 2015/04/13

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ExtDlgs,
{ Graphics32 }
  GR32, GR32_Layers, GR32_Image;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnOpenImage: TButton;
    btnOpenLevelsDlg: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    imgViewer: TImgView32;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure OpenImage(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenLevelsDialog(Sender: TObject);
    procedure imgViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    FOriginalBitmap : TBitmap32;
  public
    property OriginalBitmap : TBitmap32 read FOriginalBitmap;
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG,
{ GraphicsMagicLib }
  gmLevels,
  gmLevelsTool,
{ Demo }
  GimpLevelsToolDlg;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FOriginalBitmap := TBitmap32.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FOriginalBitmap.Free;
end;

procedure TfrmMain.OpenImage(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'Images|*.bmp;*jpg';

  if OpenPictureDialog.Execute then
  begin
    FOriginalBitmap.LoadFromFile(OpenPictureDialog.FileName);
    imgViewer.Bitmap.Assign(FOriginalBitmap);
    
    imgViewer.Width  := FOriginalBitmap.Width;
    imgViewer.Height := FOriginalBitmap.Height;

    btnOpenLevelsDlg.Enabled := True;
    lblZoom.Enabled          := True;
    cmbbxZoom.Enabled        := True;
    cmbbxZoom.ItemIndex      := 3;
    cmbbxZoomChange(nil);
  end;
end;

procedure TfrmMain.OpenLevelsDialog(Sender: TObject);
begin
  frmGimpLevelsTool.Show;
end;


procedure TfrmMain.imgViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  with frmGimpLevelsTool do
  begin
    if LevelsTool <> nil then
    begin
      if LevelsTool.ActivePicker <> lapNone then
      begin
        LevelsTool.ColorPicked( Color32(FOriginalBitmap.Canvas.Pixels[X, Y]) );
        LevelsUpdate(ALL);
        ExecuteGimpLevels();
      end;
    end;
  end;
end;

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgViewer.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

end.
