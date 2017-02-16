{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.}

unit PaintingBrushPopFrm;

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

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtDlgs, Menus, ExtCtrls, ComCtrls,
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmBrushes;

type
  // indicating who use the stroke list
  TgmStrokeListUser = (sluNone, sluBrush, sluEraser);

  TfrmPaintingBrush = class(TForm)
    pnlPaintingBrushViewer: TPanel;
    stsbrPaintingBrushInfo: TStatusBar;
    pmnPaintingBrushOptions: TPopupMenu;
    opndlgOpenBrush: TOpenDialog;
    svdlgSaveBrushes: TSaveDialog;
    opnpctrdlgOpenBitmap: TOpenPictureDialog;
    spdbtnPaintingBrushOptions: TSpeedButton;
    pmnitmSaveChanges: TMenuItem;
    pmnitmReplaceBrushes: TMenuItem;
    pmnitmSaveBrushesAs: TMenuItem;
    pmnitmSeparator1: TMenuItem;
    pmnitmSmallThumbnail: TMenuItem;
    pmnitmLargeThumbnail: TMenuItem;
    pmnitmSeparator2: TMenuItem;
    pmnitmLoadMask: TMenuItem;
    pmnitmRenameBrush: TMenuItem;
    pmnitmDeleteBrush: TMenuItem;
    pmnitmResetBrushes: TMenuItem;
    Panel1: TPanel;
    imgPaintingBrushes: TImgView32;
    procedure FormDeactivate(Sender: TObject);
    procedure pmnitmLoadMaskClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spdbtnPaintingBrushOptionsClick(Sender: TObject);
    procedure pmnitmSaveBrushesAsClick(Sender: TObject);
    procedure pmnitmReplaceBrushesClick(Sender: TObject);
    procedure pmnitmSaveChangesClick(Sender: TObject);
    procedure ChangeBrushesThumbnailSize(Sender: TObject);
    procedure pmnitmRenameBrushClick(Sender: TObject);
    procedure pmnitmDeleteBrushClick(Sender: TObject);
    procedure pmnPaintingBrushOptionsPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pmnitmResetBrushesClick(Sender: TObject);
    procedure imgPaintingBrushesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgPaintingBrushesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
  private
    FStrokeListUser: TgmStrokeListUser;   // current object that using the stroke list
    FStrokeList    : TgmStrokeList;       // in gmBrushes.pas
    FBrushStroke   : TBitmap32;           
    FEraserStroke  : TBitmap32;

    procedure VisiblePartialPaintingBrushOptions(const IsVisible: Boolean);

    procedure LoadBrushStrokes;
    procedure LoadEraserStrokes;
    procedure ShowSelectedPaintingBrush;
  public
    property StrokeListUser: TgmStrokeListUser read FStrokeListUser write FStrokeListUser;
    property BrushStroke   : TBitmap32         read FBrushStroke;
    property EraserStroke  : TBitmap32         read FEraserStroke;
  end;

var
  frmPaintingBrush: TfrmPaintingBrush;

implementation

uses
{ GraphisMagic Lib }
  gmTypes,
  gmIni,
  gmGUIFuncs,
  gmImageProcessFuncs,
{ GraphicsMagic Form/Dialog }
  MainForm,         // frmMain
  PatternNameDlg;   // call the frmPatternName for PaintingBrush temporarily

{$R *.DFM}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmPaintingBrush.VisiblePartialPaintingBrushOptions(
  const IsVisible: Boolean);
begin
  pmnitmSaveChanges.Visible    := IsVisible;
  pmnitmReplaceBrushes.Visible := IsVisible;
  pmnitmLoadMask.Visible       := IsVisible;
  pmnitmSaveBrushesAs.Visible  := IsVisible;
  pmnitmSmallThumbnail.Visible := IsVisible;
  pmnitmLargeThumbnail.Visible := IsVisible;
  pmnitmSeparator1.Visible     := IsVisible;
  pmnitmSeparator2.Visible     := IsVisible;
end;

procedure TfrmPaintingBrush.LoadBrushStrokes;
var
  UseInternalStrokes: Integer;
  FileName          : string;
  Index             : Integer;
  ThumbnailSizeMode : TgmThumbnailSizeMode;
begin
  FStrokeList.DeleteAllBrushStrokes;

  try
    UseInternalStrokes := StrToInt( ReadInfoFromIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '1') );
  except
    UseInternalStrokes := 1;
    WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, IntToStr(UseInternalStrokes) );
  end;

  if UseInternalStrokes > 0 then
  begin
    FStrokeList.LoadInternalBrushesToList;
  end
  else
  begin
    FileName := ReadInfoFromIniFile(SECTION_BRUSH, IDENT_OPEN_BRUSH_FILE, '');
    FStrokeList.LoadFromFile(FileName);

    // if failure in loading external Brushes, then load the internal Brushes.
    if FStrokeList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    Index := StrToInt( ReadInfoFromIniFile(SECTION_BRUSH, IDENT_BRUSH_INDEX, '0') );
  except
    Index := 0;
    WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX, IntToStr(Index) );
  end;

  // load thumbnail info
  try
    ThumbnailSizeMode := TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_BRUSH, IDENT_BRUSH_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (ThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      ThumbnailSizeMode := tsmLarge;
      WriteInfoToIniFile(  SECTION_BRUSH, IDENT_BRUSH_THUMBNAIL_SIZE_MODE, IntToStr( Integer(ThumbnailSizeMode) )  );
    end;
  except
    ThumbnailSizeMode := tsmLarge;
    WriteInfoToIniFile(  SECTION_BRUSH, IDENT_BRUSH_THUMBNAIL_SIZE_MODE, IntToStr( Integer(ThumbnailSizeMode) )  );
  end;

  FStrokeList.ChangeStrokeThumbnailSize(ThumbnailSizeMode);

  if FStrokeList.Count > 0 then
  begin
    if Index > FStrokeList.Count - 1 then
    begin
      Index := 0;
      WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX, IntToStr(Index) );
    end;
    
    FStrokeList.SelectStrokeByIndex(Index);
    ShowSelectedPaintingBrush;
  end;
end;

procedure TfrmPaintingBrush.LoadEraserStrokes;
var
  UseInternalStrokes: Integer;
  FileName          : string;
  Index             : Integer;
  ThumbnailSizeMode : TgmThumbnailSizeMode;
begin
  FStrokeList.DeleteAllBrushStrokes;

  try
    UseInternalStrokes := StrToInt( ReadInfoFromIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '1') );
  except
    UseInternalStrokes := 1;
    WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, IntToStr(UseInternalStrokes) );
  end;

  if UseInternalStrokes > 0 then
  begin
    FStrokeList.LoadInternalBrushesToList;
  end
  else
  begin
    FileName := ReadInfoFromIniFile(SECTION_ERASER, IDENT_ERASER_OPEN_BRUSH_FILE, '');
    FStrokeList.LoadFromFile(FileName);

    // if failure in loading external Brushes, then load the internal Brushes.
    if FStrokeList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    Index := StrToInt( ReadInfoFromIniFile(SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX, '0') );
  except
    Index := 0;
    WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX, IntToStr(Index) );
  end;

  // load thumbnail info
  try
    ThumbnailSizeMode := TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_ERASER, IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (ThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      ThumbnailSizeMode := tsmLarge;
      WriteInfoToIniFile(  SECTION_ERASER, IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE, IntToStr( Integer(ThumbnailSizeMode) )  );
    end;
  except
    ThumbnailSizeMode := tsmLarge;
    WriteInfoToIniFile(  SECTION_ERASER, IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE, IntToStr( Integer(ThumbnailSizeMode) )  );
  end;

  FStrokeList.ChangeStrokeThumbnailSize(ThumbnailSizeMode);

  if FStrokeList.Count > 0 then
  begin
    if Index > FStrokeList.Count - 1 then
    begin
      Index := 0;
      WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX, IntToStr(Index) );
    end;
    
    FStrokeList.SelectStrokeByIndex(Index);
    ShowSelectedPaintingBrush;
  end;
end;

procedure TfrmPaintingBrush.ShowSelectedPaintingBrush;
const
  IMAGE_SIZE: Integer = 20;
var
  LScaledBmp: TBitmap32;
begin
  LScaledBmp := TBitmap32.Create;
  try
    if FStrokeList.Count > 0 then
    begin
      GetScaledBitmap(FStrokeList.SelectedStroke.StrokeBitmap,
                      LScaledBmp, IMAGE_SIZE, IMAGE_SIZE);

      MakeStrokeAgainstColor( LScaledBmp, Color32(clBtnFace) );

      case FStrokeListUser of
        sluBrush:
          begin
            frmMain.imgPaintingBrush.Bitmap.Assign(LScaledBmp);

            CenterImageInPanel(frmMain.pnlPaintingBrushHolder,
                               frmMain.imgPaintingBrush);
                               
            frmMain.imgPaintingBrush.Hint :=
              'PaintingBrush - ' + FStrokeList.GetCurrentStrokeInfo;
          end;

        sluEraser:
          begin
            frmMain.imgEraserPaintingBrush.Bitmap.Assign(LScaledBmp);

            CenterImageInPanel(frmMain.pnlEraserBrushHolder,
                               frmMain.imgEraserPaintingBrush);

            frmMain.imgEraserPaintingBrush.Hint :=
              'PaintingBrush - ' + FStrokeList.GetCurrentStrokeInfo;
          end;
      end;
    end;
    
  finally
    LScaledBmp.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmPaintingBrush.FormDeactivate(Sender: TObject);
begin
  Close; // Close the form when it deactivate.
end;

procedure TfrmPaintingBrush.pmnitmLoadMaskClick(Sender: TObject);
const
  MAX_BMP_SIZE: Integer = 601;
var
  LStroke    : TgmStroke;
  LStrokeName: string;
begin
  LStroke := TgmStroke.Create;

  if opnpctrdlgOpenBitmap.Execute then
  begin
    LStroke.StrokeBitmap.LoadFromFile(opnpctrdlgOpenBitmap.FileName);
    
    LStrokeName := ExtractFileName(opnpctrdlgOpenBitmap.FileName);
    LStrokeName := Copy( LStrokeName, 1, Length(LStrokeName) - 4 );

    if (LStroke.StrokeBitmap.Width  > MAX_BMP_SIZE) or
       (LStroke.StrokeBitmap.Height > MAX_BMP_SIZE) then
    begin
      MessageDlg('Cannot load the mask bitmap, bacause the width or' + #10#13 +
                 'height of the mask bitmap is larger than 601 pixel.',
                 mtError, [mbOK], 0);
    end
    else
    begin
      Desaturate32(LStroke.StrokeBitmap);

      LStroke.GetPaintingBrushSize;
      LStroke.GetPaintingBrushRadius;

      LStroke.Name       := LStrokeName;
      LStroke.IsSelected := False;

      FStrokeList.Add(LStroke);
      FStrokeList.DrawStrokeStage(imgPaintingBrushes);
      FStrokeList.IsModified := True;
    end;
  end;
end;

procedure TfrmPaintingBrush.FormCreate(Sender: TObject);
begin
  FBrushStroke  := TBitmap32.Create;
  FEraserStroke := TBitmap32.Create;

  FStrokeList := TgmStrokeList.Create;

{ Brush User}
  FStrokeListUser := sluBrush;
  LoadBrushStrokes;
  FBrushStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

{ Eraser User}
  FStrokeListUser := sluEraser;
  LoadEraserStrokes;
  FEraserStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

  // clear list
  FStrokeList.DeleteAllBrushStrokes;
  FStrokeListUser := sluNone;
end;

procedure TfrmPaintingBrush.FormDestroy(Sender: TObject);
begin
  FStrokeList.Free;
  FBrushStroke.Free;
  FEraserStroke.Free;
end;

procedure TfrmPaintingBrush.spdbtnPaintingBrushOptionsClick(
  Sender: TObject);
var
  p: TPoint;
begin
  VisiblePartialPaintingBrushOptions(True);
  GetCursorPos(p);
  pmnPaintingBrushOptions.Popup(p.X, p.Y);
end;

procedure TfrmPaintingBrush.pmnitmSaveBrushesAsClick(Sender: TObject);
var
  LFileName, LFileExt: string;

  procedure OverwriteBrushFile(const AFileName: string);
  begin
    if FStrokeList.Count > 0 then
    begin
      FStrokeList.SaveToFile(AFileName);

      case FStrokeListUser of
        sluBrush:
          begin
            WriteInfoToIniFile(SECTION_BRUSH, IDENT_OPEN_BRUSH_FILE, AFileName);
            WriteInfoToIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '0');
          end;

        sluEraser:
          begin
            WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_OPEN_BRUSH_FILE, AFileName);
            WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '0');
          end;
      end;
    end;
  end;

begin
  if svdlgSaveBrushes.Execute then
  begin
    LFileName := svdlgSaveBrushes.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );

    if LFileExt = '' then
    begin
      LFileName := LFileName + '.gmb';
    end
    else
    begin
      if LFileExt <> '.gmb' then
      begin
        LFileName := ChangeFileExt(LFileName, '.gmb');
      end;
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already existed. Do you want to replace it?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        OverwriteBrushFile(LFileName);
      end;
    end
    else
    begin
      OverwriteBrushFile(LFileName);
    end;
  end;
end;

procedure TfrmPaintingBrush.pmnitmReplaceBrushesClick(Sender: TObject);
var
  FileName, OpenDir: string;
  LExternalLoadOK  : Boolean;
begin
  if FStrokeList.IsModified then
  begin
    case MessageDlg('The Brushes has been changed. Do you want to save these changes?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          pmnitmSaveChangesClick(Sender);
        end;

      mrCancel:
        begin
          Exit;
        end;
    end;
  end;

  case FStrokeListUser of
    sluBrush:
      begin
        FileName := ReadInfoFromIniFile(SECTION_BRUSH, IDENT_OPEN_BRUSH_FILE, '');
      end;

    sluEraser:
      begin
        FileName := ReadInfoFromIniFile(SECTION_ERASER, IDENT_ERASER_OPEN_BRUSH_FILE, '');
      end;
  end;
  
  if FileName <> '' then
  begin
    OpenDir := ExtractFilePath(FileName);
  end
  else
  begin
    OpenDir := ExtractFilePath( ParamStr(0) );
  end;

  opndlgOpenBrush.InitialDir := OpenDir;

  if opndlgOpenBrush.Execute then
  begin
    LExternalLoadOK := FStrokeList.LoadFromFile(opndlgOpenBrush.FileName);

    if not LExternalLoadOK then
    begin
      MessageDlg('Cannot open the brush stroke file.', mtError, [mbOK], 0);
    end;

    if FStrokeList.Count > 0 then
    begin
      FStrokeList.SelectStrokeByIndex(0);
      FStrokeList.DrawStrokeStage(imgPaintingBrushes);
      ShowSelectedPaintingBrush;

      case FStrokeListUser of
        sluBrush:
          begin
            FBrushStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            if LExternalLoadOK then
            begin
              WriteInfoToIniFile(SECTION_BRUSH, IDENT_OPEN_BRUSH_FILE, opndlgOpenBrush.FileName);
            end;
            
            WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX, IntToStr(FStrokeList.SelectedIndex) );

            // if failure in loading external Brushes, then load the internal Brushes
            if FStrokeList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '0');
            end;
          end;

        sluEraser:
          begin
            FEraserStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            if LExternalLoadOK then
            begin
              WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_OPEN_BRUSH_FILE, opndlgOpenBrush.FileName);
            end;
            
            WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX, IntToStr(FStrokeList.SelectedIndex) );

            // if failure in loading external Brushes, then load the internal Brushes
            if FStrokeList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '0');
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmPaintingBrush.pmnitmSaveChangesClick(Sender: TObject);
begin
  if FStrokeList.Count > 0 then
  begin
    if FStrokeList.FileName <> '' then
    begin
      FStrokeList.SaveToFile(FStrokeList.FileName);
    end
    else
    begin
      pmnitmSaveBrushesAsClick(Sender);
    end;
  end;
end;

procedure TfrmPaintingBrush.ChangeBrushesThumbnailSize(Sender: TObject);
var
  LThumbnailSizeMode: TgmThumbnailSizeMode;
begin
  LThumbnailSizeMode := tsmSmall;

  if Sender = pmnitmSmallThumbnail then
  begin
    LThumbnailSizeMode := tsmSmall;
  end
  else if Sender = pmnitmLargeThumbnail then
  begin
    LThumbnailSizeMode := tsmLarge;
  end;

  FStrokeList.ChangeStrokeThumbnailSize(LThumbnailSizeMode);
  FStrokeList.DrawStrokeStage(imgPaintingBrushes);

  case FStrokeListUser of
    sluBrush:
      begin
        WriteInfoToIniFile(  SECTION_BRUSH, IDENT_BRUSH_THUMBNAIL_SIZE_MODE,
                             IntToStr( Ord(LThumbnailSizeMode) )  );
      end;

    sluEraser:
      begin
        WriteInfoToIniFile(  SECTION_ERASER, IDENT_ERASER_BRUSH_THUMBNAIL_SIZE_MODE,
                             IntToStr( Ord(LThumbnailSizeMode) )  );
      end;
  end;
end;

procedure TfrmPaintingBrush.pmnitmRenameBrushClick(Sender: TObject);
var
  LScaledBitmap: TBitmap32;
begin
  LScaledBitmap  := TBitmap32.Create;
  frmPatternName := TfrmPatternName.Create(nil);
  try
    frmPatternName.Caption             := 'Painting Brush Name';
    frmPatternName.edtPatternName.Text := FStrokeList.SelectedStroke.Name;

    GetScaledBitmap(FStrokeList.SelectedStroke.StrokeBitmap, LScaledBitmap,
                    frmPatternName.pnlPatternView.Width - 4,
                    frmPatternName.pnlPatternView.Height - 4);

    MakeStrokeAgainstColor( LScaledBitmap, Color32(clBtnFace) );
    frmPatternName.imgPatternView.Bitmap.Assign(LScaledBitmap);
    CenterImageInPanel(frmPatternName.pnlPatternView, frmPatternName.imgPatternView);

    if frmPatternName.ShowModal = idOK then
    begin
      if Assigned(FStrokeList.SelectedStroke) then
      begin
        FStrokeList.SelectedStroke.Name := frmPatternName.edtPatternName.Text;
        FStrokeList.IsModified          := True;

        case FStrokeListUser of
          sluBrush:
            begin
              frmMain.imgPaintingBrush.Hint :=
                'PaintingBrush - ' + FStrokeList.GetCurrentStrokeInfo;
            end;

          sluEraser:
            begin
              frmMain.imgEraserPaintingBrush.Hint :=
                'PaintingBrush - ' + FStrokeList.GetCurrentStrokeInfo;
            end;
        end;
      end;
    end;
    
  finally
    FreeAndNil(frmPatternName);
    LScaledBitmap.Free;
  end;
end;

procedure TfrmPaintingBrush.pmnitmDeleteBrushClick(Sender: TObject);
begin
  FStrokeList.DeleteSelectedBrushStroke;
  FStrokeList.DrawStrokeStage(imgPaintingBrushes);

  if Assigned(FStrokeList.SelectedStroke) then
  begin
    ShowSelectedPaintingBrush;

    case FStrokeListUser of
      sluBrush:
        begin
          FBrushStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);
          
          WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX,
                              IntToStr(FStrokeList.SelectedIndex) );
        end;

      sluEraser:
        begin
          FEraserStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

          WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX,
                              IntToStr(FStrokeList.SelectedIndex) );
        end;
    end;
  end;
end;

procedure TfrmPaintingBrush.pmnPaintingBrushOptionsPopup(Sender: TObject);
begin
  pmnitmResetBrushes.Enabled   := (not FStrokeList.IsUsingInternal) or FStrokeList.IsModified;
  pmnitmSaveChanges.Enabled    := (FStrokeList.Count > 0) and FStrokeList.IsModified;
  pmnitmSaveBrushesAs.Enabled  := (FStrokeList.Count > 0);
  pmnitmSmallThumbnail.Enabled := pmnitmSaveBrushesAs.Enabled;
  pmnitmLargeThumbnail.Enabled := pmnitmSaveBrushesAs.Enabled;
  pmnitmRenameBrush.Enabled    := Assigned(FStrokeList.SelectedStroke);
  pmnitmDeleteBrush.Enabled    := (FStrokeList.Count > 1) and Assigned(FStrokeList.SelectedStroke);
  pmnitmLoadMask.Enabled       := (FStrokeList.Count < MAX_BRUSH_COUNT);
  pmnitmSmallThumbnail.Checked := (FStrokeList.Count > 0) and (FStrokeList.ThumbnailSizeMode = tsmSmall);
  pmnitmLargeThumbnail.Checked := (FStrokeList.Count > 0) and (FStrokeList.ThumbnailSizeMode = tsmLarge);
end;

procedure TfrmPaintingBrush.FormShow(Sender: TObject);
begin
  case FStrokeListUser of
    sluBrush:
      begin
        LoadBrushStrokes;
      end;

    sluEraser:
      begin
        LoadEraserStrokes;
      end;
  end;
  
  FStrokeList.DrawStrokeStage(imgPaintingBrushes);
end;

procedure TfrmPaintingBrush.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FStrokeList.IsModified then
  begin
    case MessageDlg('The Brushes has been changed. Do you want to save these changes?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          pmnitmSaveChangesClick(Sender);
        end;
        
      mrCancel:
        begin
          Exit;
        end;
    end;
  end;
  
  FStrokeList.DeleteAllBrushStrokes;
end; 

procedure TfrmPaintingBrush.pmnitmResetBrushesClick(Sender: TObject);
begin
  if MessageDlg('Replace current brushes with the default brushes?',
                mtConfirmation, [mbOK, mbCancel], 0) = idOK then
  begin
    if FStrokeList.IsModified then
    begin
      case MessageDlg('The Brushes has been changed. Do you want to save these changes?',
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
          begin
            pmnitmSaveChangesClick(Sender);
          end;
          
        mrCancel:
          begin
            Exit;
          end;
      end;
    end;

    FStrokeList.LoadInternalBrushesToList;
    
    if FStrokeList.Count > 0 then
    begin
      FStrokeList.SelectStrokeByIndex(0);
      FStrokeList.DrawStrokeStage(imgPaintingBrushes);
      ShowSelectedPaintingBrush;

      case FStrokeListUser of
        sluBrush:
          begin
            FBrushStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX,
                                IntToStr(FStrokeList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_BRUSH, IDENT_BRUSH_USE_INTERNAL, '1');
          end;

        sluEraser:
          begin
            FEraserStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX,
                                IntToStr(FStrokeList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_ERASER, IDENT_ERASER_USE_INTERNAL, '1');
          end;
      end;
    end;
  end;
end;

procedure TfrmPaintingBrush.imgPaintingBrushesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LIndex   : Integer;
  LBmpCoord: TPoint;
begin
  if FStrokeList.Count > 0 then
  begin
    LBmpCoord := imgPaintingBrushes.ControlToBitmap( Point(X, Y) );

    LIndex := FStrokeList.GetStrokeIndex(LBmpCoord.X, LBmpCoord.Y);

    if (LIndex >= 0) and (LIndex <> FStrokeList.SelectedIndex) and
       (LBmpCoord.X >= 0) and (LBmpCoord.X < imgPaintingBrushes.Bitmap.Width) and
       (LBmpCoord.Y >= 0) and (LBmpCoord.Y < imgPaintingBrushes.Bitmap.Height)  then
    begin
      FStrokeList.SelectStrokeByIndex(LIndex);
      FStrokeList.DrawStrokeStage(imgPaintingBrushes);
      ShowSelectedPaintingBrush;

      case FStrokeListUser of
        sluBrush:
          begin
            FBrushStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            WriteInfoToIniFile( SECTION_BRUSH, IDENT_BRUSH_INDEX,
                                IntToStr(FStrokeList.SelectedIndex) );
          end;

        sluEraser:
          begin
            FEraserStroke.Assign(FStrokeList.SelectedStroke.StrokeBitmap);

            WriteInfoToIniFile( SECTION_ERASER, IDENT_ERASER_BRUSH_INDEX,
                                IntToStr(FStrokeList.SelectedIndex) );
          end;
      end;
    end;

    if Button = mbRight then
    begin
      if (LIndex >= 0) and (LIndex < FStrokeList.Count) and
         (LBmpCoord.X >= 0) and (LBmpCoord.X < imgPaintingBrushes.Bitmap.Width) and
         (LBmpCoord.Y >= 0) and (LBmpCoord.Y < imgPaintingBrushes.Bitmap.Height)  then
      begin
        VisiblePartialPaintingBrushOptions(False);
        imgPaintingBrushes.PopupMenu := pmnPaintingBrushOptions;
      end
      else
      begin
        imgPaintingBrushes.PopupMenu := nil;
      end;
    end;
  end;
end;

procedure TfrmPaintingBrush.imgPaintingBrushesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LIndex   : Integer;
  LBmpCoord: TPoint;
begin
  if FStrokeList.Count > 0 then
  begin
    LBmpCoord := imgPaintingBrushes.ControlToBitmap( Point(X, Y) );

    stsbrPaintingBrushInfo.Panels[0].Text := FStrokeList.GetStrokeInfo(LBmpCoord.X, LBmpCoord.Y);

    LIndex := FStrokeList.GetStrokeIndex(LBmpCoord.X, LBmpCoord.Y);

    if (LIndex >= 0) and (LIndex < FStrokeList.Count) and
       (LBmpCoord.X >= 0) and (LBmpCoord.X < imgPaintingBrushes.Bitmap.Width) and
       (LBmpCoord.Y >= 0) and (LBmpCoord.Y < imgPaintingBrushes.Bitmap.Height) then
    begin
      imgPaintingBrushes.Cursor := crHandPoint;
    end
    else
    begin
      imgPaintingBrushes.Cursor := crNo;
    end;
  end
  else
  begin
    imgPaintingBrushes.Cursor := crDefault;
  end;
end; 

end.
