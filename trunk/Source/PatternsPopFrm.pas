unit PatternsPopFrm;

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

// Update Date: 2015/11/15

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtDlgs, Buttons, ComCtrls, ExtCtrls,
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers,
{ GraphicsMagic Lib }
  gmTypes,
  gmPatterns;

type
  // who uses the pattern list
  TgmPatternListUser = (pluNone,
                        pluFill,
                        pluStamp,
                        pluPatternLayer,
                        pluPaintBucket);

  TfrmPatterns = class(TForm)
    pnlPatternFrmBorder: TPanel;
    stsbrPatternInfo: TStatusBar;
    spdbtnPatternOptions: TSpeedButton;
    pmnPatternOptions: TPopupMenu;
    opndlgOpenPattern: TOpenDialog;
    svdlgSavePattern: TSaveDialog;
    opnpctrdlgOpenImage: TOpenPictureDialog;
    pmnitmSaveChanges: TMenuItem;
    pmnitmReplacePattern: TMenuItem;
    pmnitmSavePatternAs: TMenuItem;
    pmnitmSeparator1: TMenuItem;
    pmnitmSmallThumbnail: TMenuItem;
    pmnitmLargeThumbnail: TMenuItem;
    pmnitmSeparator2: TMenuItem;
    pmnitmLoadImageToPattern: TMenuItem;
    pmnitmRenamePattern: TMenuItem;
    pmnitmDeletePattern: TMenuItem;
    pmnitmResetPattern: TMenuItem;
    Panel1: TPanel;
    imgvwPatterns: TImgView32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spdbtnPatternOptionsClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmnitmReplacePatternClick(Sender: TObject);
    procedure pmnitmSavePatternAsClick(Sender: TObject);
    procedure pmnitmSaveChangesClick(Sender: TObject);
    procedure pmnitmLoadImageToPatternClick(Sender: TObject);
    procedure pmnPatternOptionsPopup(Sender: TObject);
    procedure pmnitmRenamePatternClick(Sender: TObject);
    procedure pmnitmDeletePatternClick(Sender: TObject);
    procedure ChangePatternThumbnailSize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pmnitmResetPatternClick(Sender: TObject);
    procedure imgvwPatternsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure imgvwPatternsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
  private
    FPatternList        : TgmPatternList;
    FPatternListUser    : TgmPatternListUser;  // indicate who use the pattern list now
    FStampPattern       : TBitmap32;
    FFillingPattern     : TBitmap32;
    FPaintBucketPattern : TBitmap32;
    FLayerPattern       : TBitmap32;

    procedure VisiblePartialPatternOptions(const IsVisible: Boolean);
    procedure LoadStampPatterns;
    procedure LoadFillPatterns;
    procedure LoadPaintBucketPatterns;
    procedure LoadLayerPatterns;
  public
    procedure ShowSelectedPattern(APatternList: TgmPatternList;
      AImage: TImage32; AImageHolder: TPanel; const ZoomSize: Integer);

    property PatternListUser    : TgmPatternListUser read FPatternListUser write FPatternListUser;
    property StampPattern       : TBitmap32          read FStampPattern;
    property FillingPattern     : TBitmap32          read FFillingPattern;
    property PaintBucketPattern : TBitmap32          read FPaintBucketPattern;
    property LayerPattern       : TBitmap32          read FLayerPattern;
  end;

var
  frmPatterns: TfrmPatterns;

implementation

uses
{ GraphicsMagic Lib }
  gmGUIFuncs,
  gmImageProcessFuncs,
  gmIni,              // ReadInfoFromINIFile()...
  gmIO,               // LoadGraphicsFile(), SaveGraphicsFile()
  gmPatternLayer,
{ GraphicsMagic Forms/Dialogs }
  MainForm,
  PatternNameDlg,
  FillDlg,            // frmFill;
  PatternFillDlg;     // frmPatternFill

{$R *.DFM}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmPatterns.VisiblePartialPatternOptions(const IsVisible: Boolean);
begin
  pmnitmResetPattern.Visible       := IsVisible;
  pmnitmSaveChanges.Visible        := IsVisible;
  pmnitmReplacePattern.Visible     := IsVisible;
  pmnitmLoadImageToPattern.Visible := IsVisible;
  pmnitmSavePatternAs.Visible      := IsVisible;
  pmnitmSmallThumbnail.Visible     := IsVisible;
  pmnitmLargeThumbnail.Visible     := IsVisible;
  pmnitmSeparator1.Visible         := IsVisible;
  pmnitmSeparator2.Visible         := IsVisible;
end;

procedure TfrmPatterns.ShowSelectedPattern(APatternList: TgmPatternList;
  AImage: TImage32; AImageHolder: TPanel; const ZoomSize: Integer);
var
  LScaledBitmap : TBitmap32;
begin
  LScaledBitmap := TBitmap32.Create;
  try
    if Assigned(APatternList.SelectedPattern) then
    begin
      GetScaledBitmap(APatternList.SelectedPattern.Bitmap, LScaledBitmap,
                      ZoomSize, ZoomSize);
                      
      AImage.Bitmap.Assign(LScaledBitmap);
      AImage.Bitmap.DrawMode := dmBlend;
      
      CenterImageInPanel(AImageHolder, AImage);
    end;
  finally
    LScaledBitmap.Free;
  end;
end; 

procedure TfrmPatterns.LoadStampPatterns;
var
  LFileName           : string;
  LIndex              : Integer;
  LUseInternalPattern : Integer;
  LThumbnailSizeMode  : TgmThumbnailSizeMode;
begin
  FPatternList.DeleteAllPatterns;

  try
    LUseInternalPattern := StrToInt(
      ReadInfoFromIniFile(SECTION_STAMP_PATTERN,
                          IDENT_STAMP_PATTERN_USE_INTERNAL, '1') );
  except
    LUseInternalPattern := 1;
    
    WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                        IDENT_STAMP_PATTERN_USE_INTERNAL,
                        IntToStr(LUseInternalPattern) );
  end;

  if LUseInternalPattern > 0 then
  begin
    FPatternList.LoadInternalPatternsToList;
  end
  else
  begin
    LFileName := ReadInfoFromIniFile(SECTION_STAMP_PATTERN,
                                     IDENT_STAMP_OPEN_PATTERN_FILE, '');
                                     
    FPatternList.LoadFromFile(LFileName);

    // If failure in loading external Patterns, then load the internal Patterns.
    if FPatternList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_STAMP_PATTERN, IDENT_STAMP_PATTERN_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    LIndex := StrToInt( ReadInfoFromIniFile(SECTION_STAMP_PATTERN,
                                            IDENT_STAMP_PATTERN_INDEX, '0') );
  except
    LIndex := 0;
    
    WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                        IDENT_STAMP_PATTERN_INDEX,
                        IntToStr(LIndex) );
  end;

  // load thumbnail info
  try
    LThumbnailSizeMode := TgmThumbnailSizeMode(
      StrToInt( ReadInfoFromIniFile(SECTION_STAMP_PATTERN,
                                    IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (LThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      LThumbnailSizeMode := tsmLarge;

      WriteInfoToIniFile(  SECTION_STAMP_PATTERN,
                           IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(LThumbnailSizeMode) )  );
    end;
  except
    LThumbnailSizeMode := tsmLarge;

    WriteInfoToIniFile(  SECTION_STAMP_PATTERN,
                         IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(LThumbnailSizeMode) )  );
  end;

  FPatternList.ChangePatternThumbnailSize(LThumbnailSizeMode);

  if FPatternList.Count > 0 then
  begin
    if LIndex > FPatternList.Count - 1 then
    begin
      LIndex := 0;
      
      WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                          IDENT_STAMP_PATTERN_INDEX,
                          IntToStr(LIndex) );
    end;

    FPatternList.SelectPatternByIndex(LIndex);
    
    ShowSelectedPattern(FPatternList,
                        frmMain.imgPatternForStamp,
                        frmMain.pnlBrushPatternHolder,
                        frmMain.pnlBrushPatternHolder.Width - 4);
  end;
end;

procedure TfrmPatterns.LoadFillPatterns;
var
  LFileName           : string;
  LIndex              : Integer;
  LUseInternalPattern : Integer;
  LThumbnailSizeMode  : TgmThumbnailSizeMode;
begin
  FPatternList.DeleteAllPatterns;

  try
    LUseInternalPattern := StrToInt(
      ReadInfoFromIniFile(SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_USE_INTERNAL, '1') );
  except
    LUseInternalPattern := 1;
    
    WriteInfoToIniFile( SECTION_FILL_DIALOG,
                        IDENT_FILL_PATTERN_USE_INTERNAL,
                        IntToStr(LUseInternalPattern) );
  end;

  if LUseInternalPattern > 0 then
  begin
    FPatternList.LoadInternalPatternsToList;
  end
  else
  begin
    LFileName := ReadInfoFromIniFile(SECTION_FILL_DIALOG,
                                     IDENT_FILL_OPEN_PATTERN_FILE, '');
                                     
    FPatternList.LoadFromFile(LFileName);

    // If failure in loading external Patterns, then load the internal Patterns.
    if FPatternList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    LIndex := StrToInt( ReadInfoFromIniFile(SECTION_FILL_DIALOG,
                                            IDENT_FILL_PATTERN_INDEX, '0') );
  except
    LIndex := 0;
    
    WriteInfoToIniFile( SECTION_FILL_DIALOG,
                        IDENT_FILL_PATTERN_INDEX,
                        IntToStr(LIndex) );
  end;

  // load thumbnail info
  try
    LThumbnailSizeMode := TgmThumbnailSizeMode(
      StrToInt( ReadInfoFromIniFile(SECTION_FILL_DIALOG,
                                    IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE,
                                    '0') )  );

    if not (LThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      LThumbnailSizeMode := tsmLarge;

      WriteInfoToIniFile(  SECTION_FILL_DIALOG,
                           IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(LThumbnailSizeMode) )  );
    end;
  except
    LThumbnailSizeMode := tsmLarge;

    WriteInfoToIniFile(  SECTION_FILL_DIALOG,
                         IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(LThumbnailSizeMode) )  );
  end;

  FPatternList.ChangePatternThumbnailSize(LThumbnailSizeMode);

  if FPatternList.Count > 0 then
  begin
    if LIndex > FPatternList.Count - 1 then
    begin
      LIndex := 0;
      
      WriteInfoToIniFile( SECTION_FILL_DIALOG, IDENT_FILL_PATTERN_INDEX,
                          IntToStr(LIndex) );
    end;

    FPatternList.SelectPatternByIndex(LIndex);
    
    ShowSelectedPattern(FPatternList, frmFill.imgSelectedPattern,
                        frmFill.pnlCustomPattern,
                        frmFill.pnlCustomPattern.Width - 4);
  end;
end;

procedure TfrmPatterns.LoadPaintBucketPatterns;
var
  LFileName           : string;
  LIndex              : Integer;
  LUseInternalPattern : Integer;
  LThumbnailSizeMode  : TgmThumbnailSizeMode;
begin
  FPatternList.DeleteAllPatterns;

  try
    LUseInternalPattern := StrToInt(
      ReadInfoFromIniFile(SECTION_PAINT_BUCKET,
                          IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL, '1') );
  except
    LUseInternalPattern := 1;

    WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                        IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL,
                        IntToStr(LUseInternalPattern) );
  end;

  if LUseInternalPattern > 0 then
  begin
    FPatternList.LoadInternalPatternsToList;
  end
  else
  begin
    LFileName := ReadInfoFromIniFile(SECTION_PAINT_BUCKET,
                                     IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE, '');
                                     
    FPatternList.LoadFromFile(LFileName);

    // If failure in loading external Patterns, then load the internal Patterns.
    if FPatternList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                         IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    LIndex := StrToInt(
      ReadInfoFromIniFile(SECTION_PAINT_BUCKET,
                          IDENT_PAINT_BUCKET_PATTERN_INDEX, '0') );
  except
    LIndex := 0;
    
    WriteInfoToIniFile( SECTION_PAINT_BUCKET, IDENT_PAINT_BUCKET_PATTERN_INDEX,
                        IntToStr(LIndex) );
  end;

  // load thumbnail info
  try
    LThumbnailSizeMode := TgmThumbnailSizeMode(
      StrToInt( ReadInfoFromIniFile(SECTION_PAINT_BUCKET,
                                    IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE,
                                    '0') )  );

    if not (LThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      LThumbnailSizeMode := tsmLarge;

      WriteInfoToIniFile(  SECTION_PAINT_BUCKET,
                           IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(LThumbnailSizeMode) )  );
    end;
  except
    LThumbnailSizeMode := tsmLarge;
    
    WriteInfoToIniFile(  SECTION_PAINT_BUCKET,
                         IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(LThumbnailSizeMode) )  );
  end;

  FPatternList.ChangePatternThumbnailSize(LThumbnailSizeMode);

  if FPatternList.Count > 0 then
  begin
    if LIndex > FPatternList.Count - 1 then
    begin
      LIndex := 0;
      
      WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                          IDENT_PAINT_BUCKET_PATTERN_INDEX,
                          IntToStr(LIndex) );
    end;

    FPatternList.SelectPatternByIndex(LIndex);
    
    ShowSelectedPattern(FPatternList,
                        frmMain.imgPatternForPaintBucket,
                        frmMain.pnlFillPatternHolder,
                        frmMain.pnlFillPatternHolder.Width - 4);
  end;
end;

procedure TfrmPatterns.LoadLayerPatterns;
var
  LFileName           : string;
  LIndex              : Integer;
  LUseInternalPattern : Integer;
  LThumbnailSizeMode  : TgmThumbnailSizeMode;
begin
  FPatternList.DeleteAllPatterns;

  try
    LUseInternalPattern := StrToInt(
      ReadInfoFromIniFile(SECTION_PATTERN_LAYER,
                          IDENT_LAYER_PATTERN_USE_INTERNAL, '1') );
  except
    LUseInternalPattern := 1;

    WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                        IDENT_LAYER_PATTERN_USE_INTERNAL,
                        IntToStr(LUseInternalPattern) );
  end;

  if LUseInternalPattern > 0 then
  begin
    FPatternList.LoadInternalPatternsToList;
  end
  else
  begin
    LFileName := ReadInfoFromIniFile(SECTION_PATTERN_LAYER,
                                     IDENT_LAYER_OPEN_PATTERN_FILE, '');

    FPatternList.LoadFromFile(LFileName);

    // If failure in loading external Patterns, then load the internal Patterns.
    if FPatternList.IsUsingInternal then
    begin
      WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                         IDENT_LAYER_PATTERN_USE_INTERNAL, '1');
    end;
  end;

  // load selected index
  try
    LIndex := StrToInt(
      ReadInfoFromIniFile(SECTION_PATTERN_LAYER,
                          IDENT_LAYER_PATTERN_INDEX, '0') );
  except
    LIndex := 0;
    WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                        IDENT_LAYER_PATTERN_INDEX,
                        IntToStr(LIndex) );
  end;

  // load thumbnail info
  try
    LThumbnailSizeMode := TgmThumbnailSizeMode(
      StrToInt( ReadInfoFromIniFile(SECTION_PATTERN_LAYER,
                                    IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE,
                                    '0') )  );

    if not (LThumbnailSizeMode in [tsmSmall, tsmLarge]) then
    begin
      LThumbnailSizeMode := tsmLarge;

      WriteInfoToIniFile(  SECTION_PATTERN_LAYER,
                           IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(LThumbnailSizeMode) )  );
    end;
  except
    LThumbnailSizeMode := tsmLarge;

    WriteInfoToIniFile(  SECTION_PATTERN_LAYER,
                         IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(LThumbnailSizeMode) )  );
  end;

  FPatternList.ChangePatternThumbnailSize(LThumbnailSizeMode);

  if FPatternList.Count > 0 then
  begin
    if LIndex > FPatternList.Count - 1 then
    begin
      LIndex := 0;

      WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                          IDENT_LAYER_PATTERN_INDEX,
                          IntToStr(LIndex) );
    end;

    FPatternList.SelectPatternByIndex(LIndex);

    ShowSelectedPattern(FPatternList,
                        frmPatternFill.imgPattern,
                        frmPatternFill.pnlPattern,
                        frmPatternFill.pnlPattern.Width - 4);

    if Assigned(ActiveChildForm) then
    begin
      with ActiveChildForm do
      begin
        if LayerList.SelectedLayer is TgmPatternLayer then
        begin
          TgmPatternLayer(LayerList.SelectedLayer).PatternBitmap :=
            FPatternList.SelectedPattern.Bitmap;
            
          LayerList.SelectedLayer.Changed();
        end;
      end;
    end;
  end;
end; 

//------------------------------------------------------------------------------

procedure TfrmPatterns.FormCreate(Sender: TObject);
begin
  FPatternListUser    := pluNone;
  FStampPattern       := TBitmap32.Create;
  FFillingPattern     := TBitmap32.Create;
  FPaintBucketPattern := TBitmap32.Create;
  FLayerPattern       := TBitmap32.Create;
  FPatternList        := TgmPatternList.Create;

{ Pattern Stamp }
  LoadStampPatterns;
  FStampPattern.Assign(FPatternList.SelectedPattern.Bitmap);

{ Fill Dialog }
  LoadFillPatterns;
  FFillingPattern.Assign(FPatternList.SelectedPattern.Bitmap);

{ Paint Bucket }
  LoadPaintBucketPatterns;
  FPaintBucketPattern.Assign(FPatternList.SelectedPattern.Bitmap);

{ Pattern Layer }
  LoadLayerPatterns;
  FLayerPattern.Assign(FPatternList.SelectedPattern.Bitmap);

  // Clear list
  FPatternList.DeleteAllPatterns;
end;

procedure TfrmPatterns.FormDestroy(Sender: TObject);
begin
  FPatternList.Free;
  FStampPattern.Free;
  FFillingPattern.Free;
  FPaintBucketPattern.Free;
  FLayerPattern.Free;
end;

procedure TfrmPatterns.spdbtnPatternOptionsClick(Sender: TObject);
var
  p : TPoint;
begin
  VisiblePartialPatternOptions(True);
  GetCursorPos(p);
  pmnPatternOptions.Popup(p.X, p.Y);
end;

procedure TfrmPatterns.FormDeactivate(Sender: TObject);
begin
  Close;  // Close itself when missing focus.
end; 

procedure TfrmPatterns.FormShow(Sender: TObject);
begin
  case FPatternListUser of
    pluFill:
      begin
        LoadFillPatterns;
      end;
      
    pluStamp:
      begin
        LoadStampPatterns;
      end;
      
    pluPaintBucket:
      begin
        LoadPaintBucketPatterns;
      end;
      
    pluPatternLayer:
      begin
        LoadLayerPatterns;
      end;
  end;

  FPatternList.DrawPatternStage(imgvwPatterns);
end;

procedure TfrmPatterns.pmnitmReplacePatternClick(Sender: TObject);
var
  LFileName, LOpenDir : string;
  LLoadOK             : Boolean;
begin
  if FPatternList.IsModified then
  begin
    case MessageDlg('The Pattern has been changed. Do you want to save these changes?',
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

  case FPatternListUser of
    pluFill:
      begin
        LFileName := ReadInfoFromIniFile(SECTION_FILL_DIALOG,
                                         IDENT_FILL_OPEN_PATTERN_FILE, '');
      end;
      
    pluStamp:
      begin
        LFileName := ReadInfoFromIniFile(SECTION_STAMP_PATTERN,
                                         IDENT_STAMP_OPEN_PATTERN_FILE, '');
      end;
      
    pluPaintBucket:
      begin
        LFileName := ReadInfoFromIniFile(SECTION_PAINT_BUCKET,
                                         IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE, '');
      end;
      
    pluPatternLayer:
      begin
        LFileName := ReadInfoFromIniFile(SECTION_PATTERN_LAYER,
                                         IDENT_LAYER_OPEN_PATTERN_FILE, '');
      end;
  end;

  if LFileName <> '' then
  begin
    LOpenDir := ExtractFilePath(LFileName);
  end
  else
  begin
    LOpenDir := ExtractFilePath( ParamStr(0) );
  end;

  opndlgOpenPattern.InitialDir := LOpenDir;

  if opndlgOpenPattern.Execute then
  begin
    LLoadOK := FPatternList.LoadFromFile(opndlgOpenPattern.FileName);

    if not LLoadOK then
    begin
      MessageDlg(FPatternList.OutputMsg, mtError, [mbOK], 0);
    end;

    if FPatternList.Count > 0 then
    begin
      FPatternList.SelectPatternByIndex(0);
      FPatternList.DrawPatternStage(imgvwPatterns);

      case FPatternListUser of
        pluFill:
          begin
            FFillingPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmFill.imgSelectedPattern,
                                frmFill.pnlCustomPattern,
                                frmFill.pnlCustomPattern.Width - 4);

            WriteInfoToIniFile( SECTION_FILL_DIALOG,
                                IDENT_FILL_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            if LLoadOK then
            begin
              WriteInfoToIniFile(SECTION_FILL_DIALOG,
                                 IDENT_FILL_OPEN_PATTERN_FILE,
                                 opndlgOpenPattern.FileName);
            end;

            // If failure in loading external Patterns, then load the internal Patterns.
            if FPatternList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_FILL_DIALOG,
                                 IDENT_FILL_PATTERN_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_FILL_DIALOG,
                                 IDENT_FILL_PATTERN_USE_INTERNAL, '0');
            end;
          end;

        pluStamp:
          begin
            FStampPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForStamp,
                                frmMain.pnlBrushPatternHolder,
                                frmMain.pnlBrushPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                                IDENT_STAMP_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            if LLoadOK then
            begin
              WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                                 IDENT_STAMP_OPEN_PATTERN_FILE,
                                 opndlgOpenPattern.FileName);
            end;

            // If failure in loading external Patterns, then load the internal Patterns.
            if FPatternList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                                 IDENT_STAMP_PATTERN_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                                 IDENT_STAMP_PATTERN_USE_INTERNAL, '0');
            end;
          end;

        pluPaintBucket:
          begin
            FPaintBucketPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForStamp,
                                frmMain.pnlFillPatternHolder,
                                frmMain.pnlFillPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                                IDENT_PAINT_BUCKET_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            if LLoadOK then
            begin
              WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                                 IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE,
                                 opndlgOpenPattern.FileName);
            end;

            // If failure in loading external Patterns, then load the internal Patterns.
            if FPatternList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                                 IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                                 IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL, '0');
            end;
          end;

        pluPatternLayer:
          begin
            FLayerPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmPatternFill.imgPattern,
                                frmPatternFill.pnlPattern,
                                frmPatternFill.pnlPattern.Width - 4);

            WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                                IDENT_LAYER_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            if LLoadOK then
            begin
              WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                                 IDENT_LAYER_OPEN_PATTERN_FILE,
                                 opndlgOpenPattern.FileName);
            end;

            // if failure in loading external Patterns,
            // then load the internal Patterns
            if FPatternList.IsUsingInternal then
            begin
              WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                                 IDENT_LAYER_PATTERN_USE_INTERNAL, '1');
            end
            else
            begin
              WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                                 IDENT_LAYER_PATTERN_USE_INTERNAL, '0');
            end;

            with ActiveChildForm do
            begin
              if LayerList.SelectedLayer is TgmPatternLayer then
              begin
                TgmPatternLayer(LayerList.SelectedLayer).PatternBitmap := FLayerPattern;
                LayerList.SelectedLayer.Changed;
              end;
            end;
          end;
        end;
    end;
  end;
end; 

procedure TfrmPatterns.pmnitmSavePatternAsClick(Sender: TObject);
var
  LFileName, LFileExt : string;

  procedure OverwritePatternFile(const AFileName: string);
  begin
    if FPatternList.Count > 0 then
    begin
      FPatternList.SaveToFile(AFileName);

      case FPatternListUser of
        pluFill:
          begin
            WriteInfoToIniFile(SECTION_FILL_DIALOG,
                               IDENT_FILL_OPEN_PATTERN_FILE,
                               AFileName);

            WriteInfoToIniFile(SECTION_FILL_DIALOG,
                               IDENT_FILL_PATTERN_USE_INTERNAL,
                               '0');
          end;

        pluStamp:
          begin
            WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                               IDENT_STAMP_OPEN_PATTERN_FILE,
                               AFileName);

            WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                               IDENT_STAMP_PATTERN_USE_INTERNAL,
                               '0');
          end;

        pluPaintBucket:
          begin
            WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                               IDENT_PAINT_BUCKET_OPEN_PATTERN_FILE,
                               AFileName);

            WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                               IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL,
                               '0');
          end;

        pluPatternLayer:
          begin
            WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                               IDENT_LAYER_OPEN_PATTERN_FILE,
                               AFileName);

            WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                               IDENT_LAYER_PATTERN_USE_INTERNAL,
                               '0');
          end;
      end;
    end;
  end; 

begin
  if svdlgSavePattern.Execute then
  begin
    LFileName := svdlgSavePattern.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );

    if LFileExt = '' then
    begin
      LFileName := LFileName + '.pat';
    end
    else if LFileExt <> '.pat' then
    begin
      LFileName := ChangeFileExt(LFileName, '.pat');
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already existed. Do you want to replace it?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        OverwritePatternFile(LFileName);
      end;
    end
    else
    begin
      OverwritePatternFile(LFileName);
    end;
  end;
end;

procedure TfrmPatterns.pmnitmSaveChangesClick(Sender: TObject);
begin
  if FPatternList.Count > 0 then
  begin
    if FPatternList.FileName <> '' then
    begin
      FPatternList.SaveToFile(FPatternList.FileName);
    end
    else
    begin
      pmnitmSavePatternAsClick(Sender);
    end;
  end;
end; 

procedure TfrmPatterns.pmnitmLoadImageToPatternClick(Sender: TObject);
const
  MAX_BMP_SIZE : Integer = 200;
var
  LPattern     : TgmPattern;
  LPatternName : string;
  LTempBmp     : TBitmap32;
begin
  if opnpctrdlgOpenImage.Execute then
  begin
    if FileExists(opnpctrdlgOpenImage.FileName) then
    begin
      LTempBmp := LoadGraphicsFile(opnpctrdlgOpenImage.FileName);

      if Assigned(LTempBmp) then
      begin
        LPattern := TgmPattern.Create;

        LPattern.Bitmap.Assign(LTempBmp);
        LPattern.Bitmap.DrawMode := dmBlend;

        LTempBmp.Free;

        LPatternName := ExtractFileName(opnpctrdlgOpenImage.FileName);
        LPatternName := Copy( LPatternName, 1, Length(LPatternName) - 4 );

        if (LPattern.Bitmap.Width  > MAX_BMP_SIZE) or
           (LPattern.Bitmap.Height > MAX_BMP_SIZE) then
        begin
          MessageDlg('Cannot load the bitmap, bacause the width or' + #10#13 +
                     'height of the bitmap is larger than 200 pixels.',
                     mtError, [mbOK], 0);
        end
        else
        begin
          LPattern.Name       := LPatternName;
          LPattern.IsSelected := False;

          LPattern.SizeInfo := '(' + IntToStr(LPattern.Bitmap.Width) + 'x' +
                               IntToStr(LPattern.Bitmap.Height) + ')';

          FPatternList.Add(LPattern);
          FPatternList.DrawPatternStage(imgvwPatterns);
          FPatternList.IsModified := True;
        end;
      end;
    end;
  end;
end; 

procedure TfrmPatterns.pmnPatternOptionsPopup(Sender: TObject);
begin
  pmnitmResetPattern.Enabled       := (not FPatternList.IsUsingInternal) or FPatternList.IsModified;
  pmnitmSaveChanges.Enabled        := (FPatternList.Count > 0) and FPatternList.IsModified;
  pmnitmSavePatternAs.Enabled      := (FPatternList.Count > 0);
  pmnitmSmallThumbnail.Enabled     := pmnitmSavePatternAs.Enabled;
  pmnitmLargeThumbnail.Enabled     := pmnitmSavePatternAs.Enabled;
  pmnitmRenamePattern.Enabled      := Assigned(FPatternList.SelectedPattern);
  pmnitmDeletePattern.Enabled      := (FPatternList.Count > 1) and Assigned(FPatternList.SelectedPattern);
  pmnitmLoadImageToPattern.Enabled := (FPatternList.Count < MAX_PATTERN_COUNT);
  pmnitmSmallThumbnail.Checked     := (FPatternList.Count > 0) and (FPatternList.ThumbnailSizeMode = tsmSmall);
  pmnitmLargeThumbnail.Checked     := (FPatternList.Count > 0) and (FPatternList.ThumbnailSizeMode = tsmLarge);
end; 

procedure TfrmPatterns.pmnitmRenamePatternClick(Sender: TObject);
var
  LScaledBitmap : TBitmap32;
begin
  LScaledBitmap  := TBitmap32.Create;
  frmPatternName := TfrmPatternName.Create(nil);
  try
    frmPatternName.Caption := 'Pattern Name';

    if Assigned(FPatternList.SelectedPattern) then
    begin
      frmPatternName.edtPatternName.Text := FPatternList.SelectedPattern.Name;

      GetScaledBitmap(FPatternList.SelectedPattern.Bitmap,
                      LScaledBitmap,
                      frmPatternName.pnlPatternView.Width - 4,
                      frmPatternName.pnlPatternView.Height - 4);
    end;

    frmPatternName.imgPatternView.Bitmap.Assign(LScaledBitmap);

    CenterImageInPanel(frmPatternName.pnlPatternView,
                       frmPatternName.imgPatternView);

    if frmPatternName.ShowModal = idOK then
    begin
      if Assigned(FPatternList.SelectedPattern) then
      begin
        FPatternList.SelectedPattern.Name := frmPatternName.edtPatternName.Text;
        FPatternList.IsModified           := True;
      end;
    end;
    
  finally
    FreeAndNil(frmPatternName);
    LScaledBitmap.Free;
  end;
end; 

procedure TfrmPatterns.pmnitmDeletePatternClick(Sender: TObject);
begin
  FPatternList.DeleteSelectedPattern;
  FPatternList.DrawPatternStage(imgvwPatterns);

  case FPatternListUser of
    pluFill:
      begin
        FFillingPattern.Assign(FPatternList.SelectedPattern.Bitmap);

        ShowSelectedPattern(FPatternList,
                            frmFill.imgSelectedPattern,
                            frmFill.pnlCustomPattern,
                            frmFill.pnlCustomPattern.Width - 4);

        WriteInfoToIniFile( SECTION_FILL_DIALOG,
                            IDENT_FILL_PATTERN_INDEX,
                            IntToStr(FPatternList.SelectedIndex) );
      end;

    pluStamp:
      begin
        FStampPattern.Assign(FPatternList.SelectedPattern.Bitmap);

        ShowSelectedPattern(FPatternList,
                            frmMain.imgPatternForStamp,
                            frmMain.pnlBrushPatternHolder,
                            frmMain.pnlBrushPatternHolder.Width - 4);

        WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                            IDENT_STAMP_PATTERN_INDEX,
                            IntToStr(FPatternList.SelectedIndex) );
      end;

    pluPaintBucket:
      begin
        FPaintBucketPattern.Assign(FPatternList.SelectedPattern.Bitmap);

        ShowSelectedPattern(FPatternList,
                            frmMain.imgPatternForPaintBucket,
                            frmMain.pnlFillPatternHolder,
                            frmMain.pnlFillPatternHolder.Width - 4);

        WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                            IDENT_PAINT_BUCKET_PATTERN_INDEX,
                            IntToStr(FPatternList.SelectedIndex) );
      end;

    pluPatternLayer:
      begin
        FLayerPattern.Assign(FPatternList.SelectedPattern.Bitmap);

        ShowSelectedPattern(FPatternList,
                            frmPatternFill.imgPattern,
                            frmPatternFill.pnlPattern,
                            frmPatternFill.pnlPattern.Width - 4);

        WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                            IDENT_LAYER_PATTERN_INDEX,
                            IntToStr(FPatternList.SelectedIndex) );
      end;
  end;
end; 

procedure TfrmPatterns.ChangePatternThumbnailSize(Sender: TObject);
var
  LThumbnailSizeMode : TgmThumbnailSizeMode;
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

  FPatternList.ChangePatternThumbnailSize(LThumbnailSizeMode);
  FPatternList.DrawPatternStage(imgvwPatterns);

  case FPatternListUser of
    pluFill:
      begin
        WriteInfoToIniFile(  SECTION_FILL_DIALOG,
                             IDENT_FILL_PATTERN_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;

    pluStamp:
      begin
        WriteInfoToIniFile(  SECTION_STAMP_PATTERN,
                             IDENT_STAMP_PATTERN_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;

    pluPaintBucket:
      begin
        WriteInfoToIniFile(  SECTION_PAINT_BUCKET,
                             IDEMT_PAINT_BUCKET_PATTERN_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;

    pluPatternLayer:
      begin
        WriteInfoToIniFile(  SECTION_PATTERN_LAYER,
                             IDENT_LAYER_PATTERN_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;
  end;
end; 

procedure TfrmPatterns.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FPatternList.IsModified then
  begin
    case MessageDlg('The Pattern has been changed. Do you want to save these changes?',
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
  
  FPatternList.DeleteAllPatterns;
end;

procedure TfrmPatterns.pmnitmResetPatternClick(Sender: TObject);
begin
  if MessageDlg('Replace current patterns with the default patterns?',
                mtConfirmation, [mbOK, mbCancel], 0) = idOK then
  begin
    if FPatternList.IsModified then
    begin
      case MessageDlg('The Pattern has been changed. Do you want to save these changes?',
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

    FPatternList.LoadInternalPatternsToList;

    if FPatternList.Count > 0 then
    begin
      FPatternList.SelectPatternByIndex(0);
      FPatternList.DrawPatternStage(imgvwPatterns);

      case FPatternListUser of
        pluFill:
          begin
            FFillingPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmFill.imgSelectedPattern,
                                frmFill.pnlCustomPattern,
                                frmFill.pnlCustomPattern.Width - 4);

            WriteInfoToIniFile( SECTION_FILL_DIALOG,
                                IDENT_FILL_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_FILL_DIALOG,
                               IDENT_FILL_PATTERN_USE_INTERNAL,
                               '1');
          end;

        pluStamp:
          begin
            FStampPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForStamp,
                                frmMain.pnlBrushPatternHolder,
                                frmMain.pnlBrushPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                                IDENT_STAMP_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_STAMP_PATTERN,
                               IDENT_STAMP_PATTERN_USE_INTERNAL,
                               '1');
          end;

        pluPaintBucket:
          begin
            FPaintBucketPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForStamp,
                                frmMain.pnlFillPatternHolder,
                                frmMain.pnlFillPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                                IDENT_PAINT_BUCKET_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_PAINT_BUCKET,
                               IDENT_PAINT_BUCKET_PATTERN_USE_INTERNAL,
                               '1');
          end;

        pluPatternLayer:
          begin
            FLayerPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmPatternFill.imgPattern,
                                frmPatternFill.pnlPattern,
                                frmPatternFill.pnlPattern.Width - 4);

            WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                                IDENT_LAYER_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            WriteInfoToIniFile(SECTION_PATTERN_LAYER,
                               IDENT_LAYER_PATTERN_USE_INTERNAL,
                               '1');

            with ActiveChildForm do
            begin
              if LayerList.SelectedLayer is TgmPatternLayer then
              begin
                TgmPatternLayer(LayerList.SelectedLayer).PatternBitmap := FLayerPattern;
                LayerList.SelectedLayer.Changed();
              end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TfrmPatterns.imgvwPatternsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  LIndex    : Integer;
  LBmpCoord : TPoint;
begin
  if FPatternList.Count > 0 then
  begin
    LBmpCoord := imgvwPatterns.ControlToBitmap(Point(X, Y));
    LIndex    := FPatternList.GetPatternIndex(LBmpCoord.X, LBmpCoord.Y);
    
    if (LIndex >= 0) and
       (LIndex <> FPatternList.SelectedIndex) and
       (LBmpCoord.X >= 0) and
       (LBmpCoord.X < imgvwPatterns.Bitmap.Width) and
       (LBmpCoord.Y >= 0) and
       (LBmpCoord.Y < imgvwPatterns.Bitmap.Height) then
    begin
      FPatternList.SelectPatternByIndex(LIndex);
      FPatternList.DrawPatternBorder(imgvwPatterns);

      case FPatternListUser of
        pluFill:
          begin
            FFillingPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmFill.imgSelectedPattern,
                                frmFill.pnlCustomPattern,
                                frmFill.pnlCustomPattern.Width - 4);

            WriteInfoToIniFile( SECTION_FILL_DIALOG,
                                IDENT_FILL_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );
          end;

        pluStamp:
          begin
            FStampPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForStamp,
                                frmMain.pnlBrushPatternHolder,
                                frmMain.pnlBrushPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_STAMP_PATTERN,
                                IDENT_STAMP_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );
          end;

        pluPaintBucket:
          begin
            FPaintBucketPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmMain.imgPatternForPaintBucket,
                                frmMain.pnlFillPatternHolder,
                                frmMain.pnlFillPatternHolder.Width - 4);

            WriteInfoToIniFile( SECTION_PAINT_BUCKET,
                                IDENT_PAINT_BUCKET_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );
          end;

        pluPatternLayer:
          begin
            FLayerPattern.Assign(FPatternList.SelectedPattern.Bitmap);

            ShowSelectedPattern(FPatternList,
                                frmPatternFill.imgPattern,
                                frmPatternFill.pnlPattern,
                                frmPatternFill.pnlPattern.Width - 4);

            WriteInfoToIniFile( SECTION_PATTERN_LAYER,
                                IDENT_LAYER_PATTERN_INDEX,
                                IntToStr(FPatternList.SelectedIndex) );

            with ActiveChildForm do
            begin
              if LayerList.SelectedLayer is TgmPatternLayer then
              begin
                TgmPatternLayer(LayerList.SelectedLayer).PatternBitmap := FLayerPattern;
                LayerList.SelectedLayer.Changed();
              end;
            end;
          end;
      end;
    end;

    if Button = mbRight then
    begin
      if (LIndex >= 0) and
         (LIndex < FPatternList.Count) and
         (LBmpCoord.X >= 0) and
         (LBmpCoord.X < imgvwPatterns.Bitmap.Width) and
         (LBmpCoord.Y >= 0) and
         (LBmpCoord.Y < imgvwPatterns.Bitmap.Height) then
      begin
        VisiblePartialPatternOptions(False);
        imgvwPatterns.PopupMenu := pmnPatternOptions;
      end
      else
      begin
        imgvwPatterns.PopupMenu := nil;
      end;
    end;
  end;
end;

procedure TfrmPatterns.imgvwPatternsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  LIndex    : Integer;
  LBmpCoord : TPoint;
begin
  if FPatternList.Count > 0 then
  begin
    LBmpCoord := imgvwPatterns.ControlToBitmap(Point(X,Y));
    
    stsbrPatternInfo.Panels[0].Text :=
      FPatternList.GetPatternInfo(LBmpCoord.X, LBmpCoord.Y);

    LIndex := FPatternList.GetPatternIndex(LBmpCoord.X, LBmpCoord.Y);

    if (LIndex >= 0) and
       (LIndex < FPatternList.Count) and
       (LBmpCoord.X >= 0) and
       (LBmpCoord.X < imgvwPatterns.Bitmap.Width) and
       (LBmpCoord.Y >= 0) and
       (LBmpCoord.Y < imgvwPatterns.Bitmap.Height) then
    begin
      imgvwPatterns.Cursor := crHandPoint;
    end
    else
    begin
      imgvwPatterns.Cursor := crNo;
    end;
  end
  else
  begin
    imgvwPatterns.Cursor := crDefault;
  end;
end;

end.
