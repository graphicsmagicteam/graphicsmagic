{ This is a plug-in filter specifically designed for GraphicsMagic.
  Copyright (C) 2008 Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >,
  all rights reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

// Updated Date: 2017/01/24

unit ResynthDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, GR32_RangeBars, StdCtrls, ExtDlgs, GR32, GR32_Image,
  gmResynth, gmTypes;

type
  TUpdateViewProc = procedure;

  TfrmResynthesizer = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    btnLoadSrcTexture: TButton;
    imgvwSrcTexture: TImgView32;
    GroupBox2: TGroupBox;
    chckbxHTile: TCheckBox;
    chckbxVTile: TCheckBox;
    chckbxUseBorder: TCheckBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    edtAutism: TEdit;
    ggbrAutism: TGaugeBar;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    OpenPictureDialog: TOpenPictureDialog;
    btnPreview: TButton;
    btnAbout: TButton;
    procedure LoadSourceTexture(Sender: TObject);
    procedure ggbrAutismChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtAutismChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chckbxHTileClick(Sender: TObject);
    procedure chckbxVTileClick(Sender: TObject);
    procedure chckbxUseBorderClick(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    { private declarations }
  public
    FResynth             : TgmResynthesizer;
    FUpdateViewProc      : TUpdateViewProc;
    FSourceBmp           : TBitmap32;
    FProcessedBmp        : TBitmap32;
    FDestBmpPtr          : PColor32;
    FIsNewSettingExecuted: Boolean;
    FChannelSet          : TgmChannelSet;

    procedure ExecuteResynthesizer;
  end;

var
  frmResynthesizer: TfrmResynthesizer;

implementation

uses
{ Standard }
  JPEG, IniFiles,
{ GraphicsMagic Lib }
  gmPluginFuncs, gmImageProcessFuncs,
{ Plugin Dialogs }
  AboutResynthDlg;

const
  INI_SECTION          = 'ResynthesizerSettings';
  INI_IDENT_AUTISM     = 'Autism';
  INI_IDENT_HTILE      = 'HorizontallyTile';
  INI_IDENT_VTILE      = 'VerticallyTile';
  INI_IDENT_USE_BORDER = 'UseBorder';

{$R *.dfm}

// Create TBitmap from BMP, JPG, WMF, EMF or GIF disk file.
// Could be easily extended to other image types.
//
// This method is extracted from the same named method that in
// GarphicsConversionsLibrary.pas, the original author is Earl F. Glynn.
// You could find it at http://www.efg2.com/
function LoadGraphicsFile(const AFileName: string ): TBitmap;
var
  LExtension: string;
  LJPEGImage: TJPEGImage;
begin
  Result := nil;  // In case anything goes wrong
  
  if FileExists(AFileName) then
  begin
    LExtension := UpperCase(  Copy( AFileName, Length(AFileName) - 2, 3 )  );
    
    // Quick and dirty check that file type is OK
    Assert( (LExtension = 'BMP') or
            (LExtension = 'JPG') );

    Result := TBitmap.Create;
    
    // BMP File -- no additional work to get TBitmap
    if LExtension = 'BMP' then
    begin
      Result.LoadFromFile(AFileName);
    end;

    // JPG File
    if LExtension = 'JPG' then
    begin
      LJPEGImage := TJPEGImage.Create;
      try
        LJPEGImage.LoadFromFile(AFileName);

        Result.Height      := LJPEGImage.Height;
        Result.Width       := LJPEGImage.Width;
        Result.PixelFormat := pf24bit;
        
        Result.Canvas.Draw(0, 0, LJPEGImage);
      finally
        LJPEGImage.Free;
      end;
    end;

    // If Graphic is missing or invalid, create the "Red X"
    if Result = nil then
    begin
      Result.Height      := 32;
      Result.Width       := 32;
      Result.PixelFormat := pf24bit;
      
      with Result.Canvas do
      begin
        Pen.Color := clRed;
        Pen.Width := 3;
        
        MoveTo(2,2);
        LineTo(29,29);
        MoveTo(2,29);
        LineTo(29,2);
      end;
    end;
  end;
end; 

procedure TfrmResynthesizer.ExecuteResynthesizer;
var
  LColorChannelCount: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FResynth.Data.Assign(FSourceBmp);

    if FResynth.Run(FProcessedBmp) = False then
    begin
      MessageDlg(FResynth.OutputMsg, mtError, [mbOK], 0);
    end;
    
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

procedure TfrmResynthesizer.LoadSourceTexture(Sender: TObject);
var
  LLoadedBitmap: TBitmap;
begin
  if OpenPictureDialog.Execute then
  begin
    LLoadedBitmap := LoadGraphicsFile(OpenPictureDialog.FileName);
    try
      imgvwSrcTexture.Bitmap.Assign(LLoadedBitmap);
      FResynth.Corpus.Assign(imgvwSrcTexture.Bitmap);
      FIsNewSettingExecuted := False;
    finally
      LLoadedBitmap.Free;
    end;
  end;
end; 

procedure TfrmResynthesizer.ggbrAutismChange(Sender: TObject);
begin
  edtAutism.Text        := FloatToStr(ggbrAutism.Position / 100);
  FResynth.Autism       := ggbrAutism.Position / 100;
  FIsNewSettingExecuted := False;
end;

procedure TfrmResynthesizer.FormShow(Sender: TObject);
var
  LIniFile : TIniFile;
  LDLLName : array [0..255] of Char;
  LFileName: string;
  LAutism  : Double;
begin
  GetModuleFileName(hInstance, LDLLName, 256);

  LFileName := LDLLName;
  LFileName := ChangeFileExt(LFileName, '.ini');

  LIniFile := TIniFile.Create(LFileName);
  try
    LAutism                 := LIniFile.ReadFloat(INI_SECTION, INI_IDENT_AUTISM, 0.12);
    chckbxHTile.Checked     := LIniFile.ReadBool(INI_SECTION, INI_IDENT_HTILE, True);
    chckbxVTile.Checked     := LIniFile.ReadBool(INI_SECTION, INI_IDENT_VTILE, True);
    chckbxUseBorder.Checked := LIniFile.ReadBool(INI_SECTION, INI_IDENT_USE_BORDER, True);
  finally
    LIniFile.Free;
  end;

  if Assigned(FResynth) then
  begin
    FResynth.Corpus.Assign(imgvwSrcTexture.Bitmap);
    
    FResynth.IsHorizTile := chckbxHTile.Checked;
    FResynth.IsVertTile  := chckbxVTile.Checked;
    FResynth.IsUseBorder := chckbxUseBorder.Checked;
    FResynth.Autism      := LAutism;
  end;

  ggbrAutism.Position := Round(LAutism * 100);
  edtAutism.Text      := FloatToStr(LAutism);
end; 

procedure TfrmResynthesizer.btbtnOKClick(Sender: TObject);
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
    LIniFile.WriteFloat(INI_SECTION, INI_IDENT_AUTISM, FResynth.Autism);
    LIniFile.WriteBool(INI_SECTION, INI_IDENT_HTILE, FResynth.IsHorizTile);
    LIniFile.WriteBool(INI_SECTION, INI_IDENT_VTILE, FResynth.IsVertTile);
    LIniFile.WriteBool(INI_SECTION, INI_IDENT_USE_BORDER, FResynth.IsUseBorder);
  finally
    LIniFile.Free;
  end;
end; 

procedure TfrmResynthesizer.FormCreate(Sender: TObject);
begin
  FUpdateViewProc       := nil;
  FDestBmpPtr           := nil;
  FIsNewSettingExecuted := False;
  FChannelSet           := [csRed, csGreen, csBlue];

  FResynth      := TgmResynthesizer.Create;
  FSourceBmp    := TBitmap32.Create;
  FProcessedBmp := TBitmap32.Create;
end; 

procedure TfrmResynthesizer.edtAutismChange(Sender: TObject);
var
  LChangedValue: Double;
begin
  try
    LChangedValue := StrToFloat(edtAutism.Text);
    ClampValue(LChangedValue, ggbrAutism.Min / 100, ggbrAutism.Max / 100);

    ggbrAutism.Position := Round(LChangedValue * 100);
    edtAutism.Text      := FloatToStr(ggbrAutism.Position / 100);
  except
    edtAutism.Text := FloatToStr(ggbrAutism.Position / 100);
  end;
end; 

procedure TfrmResynthesizer.FormDestroy(Sender: TObject);
begin
  FUpdateViewProc := nil;
  FDestBmpPtr     := nil;
  FResynth.Free;
  FSourceBmp.Free;
  FProcessedBmp.Free;
end; 

procedure TfrmResynthesizer.chckbxHTileClick(Sender: TObject);
begin
  FResynth.IsHorizTile  := chckbxHTile.Checked;
  FIsNewSettingExecuted := False;
end; 

procedure TfrmResynthesizer.chckbxVTileClick(Sender: TObject);
begin
  FResynth.IsVertTile   := chckbxVTile.Checked;
  FIsNewSettingExecuted := False;
end;

procedure TfrmResynthesizer.chckbxUseBorderClick(Sender: TObject);
begin
  FResynth.IsUseBorder  := chckbxUseBorder.Checked;
  FIsNewSettingExecuted := False;
end;

procedure TfrmResynthesizer.btnPreviewClick(Sender: TObject);
begin
  ExecuteResynthesizer;
  FIsNewSettingExecuted := True;
end; 

procedure TfrmResynthesizer.btnAboutClick(Sender: TObject);
begin
  frmAboutResynthesizer := TfrmAboutResynthesizer.Create(Self);
  try
    frmAboutResynthesizer.ShowModal;
  finally
    frmAboutResynthesizer.Free;
  end;
end;

end.
