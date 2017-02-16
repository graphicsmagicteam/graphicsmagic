{ This library created in 01/27/2006.
  Copyright (c) 2006 by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com).
  All rights reserved.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis,
  for giving us the opportunity to know how to achieve Curves Tool.

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

unit Main;

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ExtDlgs,
{ Graphics32 }
  GR32, GR32_Image;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnOpenBitmap: TButton;
    btnGimpCurves: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    btnRestore: TButton;
    ScrollBox1: TScrollBox;
    imgPreview: TImgView32;
    lblZoom: TLabel;
    cmbbxZoom: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenBitmap(Sender: TObject);
    procedure OpenGimpCurvesDialog(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure cmbbxZoomChange(Sender: TObject);
  private
    { Private declarations }
    FSourceBitmap : TBitmap32;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ Delphi }
  JPEG,
{ Demo }
  GimpCurvesDlg;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSourceBitmap := TBitmap32.Create();
end; 

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSourceBitmap.Free();
end; 

procedure TfrmMain.OpenBitmap(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'Images|*.jpg;*.bmp';
  
  if OpenPictureDialog.Execute() then
  begin
    FSourceBitmap.LoadFromFile(OpenPictureDialog.FileName);
    imgPreview.Bitmap.Assign(FSourceBitmap);
    btnGimpCurves.Enabled := True;
    btnRestore.Enabled    := False;

    lblZoom.Enabled     := True;
    cmbbxZoom.Enabled   := True;
    cmbbxZoom.ItemIndex := 3;
    cmbbxZoomChange(nil);
  end;
end; 

procedure TfrmMain.OpenGimpCurvesDialog(Sender: TObject);
begin
  frmGimpCurves := TfrmGimpCurves.Create(Self);
  try
    case frmGimpCurves.ShowModal() of
      mrOK:
        begin
          btnRestore.Enabled := True;
        end;

      mrCancel:
        begin
          imgPreview.Bitmap.Assign(frmGimpCurves.CurvesTool.SourceBitmap);
        end;
    end;
  finally
    FreeAndNil(frmGimpCurves);
  end;
end;

procedure TfrmMain.btnRestoreClick(Sender: TObject);
begin
  imgPreview.Bitmap.Assign(FSourceBitmap);
  imgPreview.Bitmap.Changed();
end; 

procedure TfrmMain.cmbbxZoomChange(Sender: TObject);
begin
  imgPreview.Scale := (cmbbxZoom.ItemIndex + 1) * 0.25;
end;

end.
