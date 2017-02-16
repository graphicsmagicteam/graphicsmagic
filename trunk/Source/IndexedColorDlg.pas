unit IndexedColorDlg;

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

// Update Date: 2016/04/20

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, GIFImage, GR32_Layers;

type
  TfrmIndexedColor = class(TForm)
    grpbxOptions: TGroupBox;
    lblDither: TLabel;
    cmbbxDitherMode: TComboBox;
    grpbxPalette: TGroupBox;
    lblColorReduction: TLabel;
    cmbbxColorReduction: TComboBox;
    lblPaletteColors: TLabel;
    edtPaletteColors: TEdit;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    chckbxPreview: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmbbxColorReductionChange(Sender: TObject);
    procedure cmbbxDitherModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure btbtnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPreviewLayer      : TBitmapLayer;
    FColorReduction    : TColorReduction;
    FGIFColorReduction : TColorReduction;
    FDitherMode        : TDitherMode;
    FGIFDitherMode     : TDitherMode;

    function GetColorReductionList: TStringList;
    function GetDitherModeList: TStringList;
    
    procedure ShowColorCounts(const AColorReduction: TColorReduction);
    procedure Preview;
    procedure CreatePreviewLayer;
  public
    function GetColorReductionIndex(const AColorReduction: TColorReduction): Integer;
    function GetDitherModeIndex(const AMode: TDitherMode): Integer;

    property ColorReduction : TColorReduction read FColorReduction;
    property DitherMode     : TDitherMode     read FDitherMode;
  end;

var
  frmIndexedColor: TfrmIndexedColor;

implementation

uses
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmIni,
{ GraphicsMagic Forms }
  MainForm;    // FBitmapBefore, FBitmapAfter
  

{$R *.DFM}

{ Custom procedures and functions }

function TfrmIndexedColor.GetColorReductionList: TStringList;
begin
  Result := TStringList.Create();

  with Result do
  begin
    Add('Windows 20');
    Add('Windows 256');
    Add('Windows Gray');
    Add('Monochrome');
    Add('Gray Scale');
    Add('Netscape');
    Add('Quantize');
    Add('Quantize Windows');
  end;
end;

function TfrmIndexedColor.GetDitherModeList: TStringList;
begin
  Result := TStringList.Create();

  with Result do
  begin
    Add('Nearest');
    Add('Floyd Steinberg');
    Add('Stucki');
    Add('Sierra');
    Add('JaJuNi');
    Add('SteveArche');
    Add('Burkes');
  end;
end;

procedure TfrmIndexedColor.ShowColorCounts(
  const AColorReduction: TColorReduction);
begin
  case AColorReduction of
    rmWindows20:
      begin
        edtPaletteColors.Text := '20';
      end;

    rmWindows256:
      begin
        edtPaletteColors.Text := '256';
      end;

    rmWindowsGray:
      begin
        edtPaletteColors.Text := '4';
      end;
      
    rmMonochrome:
      begin
        edtPaletteColors.Text := '2';
      end;
      
    rmGrayScale:
      begin
        edtPaletteColors.Text := '256';
      end;

    rmNetscape:
      begin
        edtPaletteColors.Text := '216';
      end;
      
    rmQuantize:
      begin
        edtPaletteColors.Text := '2^n';
      end;
      
    rmQuantizeWindows:
      begin
        edtPaletteColors.Text := '256';
      end;
  end;
end;

procedure TfrmIndexedColor.Preview;
var
  LSourceBitmap, LDestBitmap : TBitmap;
begin
  LDestBitmap := nil;

  LSourceBitmap := TBitmap.Create();
  try
    LSourceBitmap.Assign(frmMain.FBitmapBefore);
    LSourceBitmap.PixelFormat := pf24bit;

    LDestBitmap := ReduceColors(LSourceBitmap, FColorReduction, FDitherMode,
                                GIFImageDefaultColorReductionBits, 0);

    frmMain.FBitmapAfter.Assign(LDestBitmap);

    if chckbxPreview.Checked then
    begin
      if FPreviewLayer <> nil then
      begin
        FPreviewLayer.Bitmap.Assign(frmMain.FBitmapAfter);
        FPreviewLayer.Bitmap.Changed();
      end;
    end;  

  finally
    LSourceBitmap.Free();

    if Assigned(LDestBitmap) then
    begin
      LDestBitmap.Free();
    end;
  end;
end;

procedure TfrmIndexedColor.CreatePreviewLayer;
var
  LRect : TRect;
begin
  if FPreviewLayer = nil then
  begin
    FPreviewLayer := TBitmapLayer.Create(ActiveChildForm.imgWorkArea.Layers);

    with FPreviewLayer do
    begin
      LRect             := ActiveChildForm.imgWorkArea.GetBitmapRect();
      LRect.TopLeft     := ActiveChildForm.imgWorkArea.ControlToBitmap(LRect.TopLeft);
      LRect.BottomRight := ActiveChildForm.imgWorkArea.ControlToBitmap(LRect.BottomRight);
      
      Location := FloatRect(LRect);
      Scaled   := True;

      Bitmap.Assign(frmMain.FBitmapBefore);
      Bitmap.DrawMode := dmBlend;
    end;
  end;
end;

function TfrmIndexedColor.GetColorReductionIndex(
  const AColorReduction: TColorReduction): Integer;
var
  n : Integer;
begin
  Result := -1;

  n := Ord(AColorReduction);
  if (n >= 1) and (n <= 8) then
  begin
    Result := n - 1;
  end;
end;

function TfrmIndexedColor.GetDitherModeIndex(const AMode: TDitherMode): Integer;
begin
  Result := Ord(AMode);
end;

{ GUI Methods }

procedure TfrmIndexedColor.FormCreate(Sender: TObject);
begin
  FPreviewLayer             := nil;
  FColorReduction           := TColorReduction(StrToInt(ReadInfoFromIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_REDUCTION, '7')));
  FDitherMode               := TDitherMode(StrToInt(ReadInfoFromIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_DITHER_MODE, '1')));
  FGIFColorReduction        := FColorReduction;
  FGIFDitherMode            := FDitherMode;
  cmbbxColorReduction.Items := GetColorReductionList();
  cmbbxDitherMode.Items     := GetDitherModeList();
end;

procedure TfrmIndexedColor.FormShow(Sender: TObject);
begin
  FColorReduction               := FGIFColorReduction;
  FDitherMode                   := FGIFDitherMode;
  cmbbxColorReduction.ItemIndex := GetColorReductionIndex(FColorReduction);
  cmbbxDitherMode.ItemIndex     := GetDitherModeIndex(FDitherMode);

  ShowColorCounts(FColorReduction);

  if FPreviewLayer = nil then
  begin
    CreatePreviewLayer();
  end;

  chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_PREVIEW, '1')));
  Preview();
end;

procedure TfrmIndexedColor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FPreviewLayer) then
  begin
    FreeAndNil(FPreviewLayer);
  end;
end;

procedure TfrmIndexedColor.cmbbxColorReductionChange(Sender: TObject);
begin
  FColorReduction := TColorReduction(cmbbxColorReduction.ItemIndex + 1);
  ShowColorCounts(FColorReduction);
  Preview();
end;

procedure TfrmIndexedColor.cmbbxDitherModeChange(Sender: TObject);
begin
  FDitherMode := TDitherMode(cmbbxDitherMode.ItemIndex);
  Preview();
end;

procedure TfrmIndexedColor.chckbxPreviewClick(Sender: TObject);
begin
  Preview();
end;

procedure TfrmIndexedColor.btbtnOKClick(Sender: TObject);
begin
  FGIFColorReduction := FColorReduction;
  FGIFDitherMode     := FDitherMode;

  // save settings in INI file
  WriteInfoToIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_REDUCTION,   IntToStr(Ord(FColorReduction)));
  WriteInfoToIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_DITHER_MODE, IntToStr(Ord(FDitherMode)));
  WriteInfoToIniFile(SECTION_INDEXED_COLOR_DIALOG, IDENT_INDEXED_COLOR_PREVIEW,     IntToStr(Integer(chckbxPreview.Checked)));
end;

procedure TfrmIndexedColor.btbtnCancelClick(Sender: TObject);
begin
  FColorReduction := FGIFColorReduction;
  FDitherMode     := FGIFDitherMode;
end;


end.
