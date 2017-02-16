unit GradientPickerPopFrm;

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
  ExtCtrls, ComCtrls, Menus, Buttons, StdCtrls,
{ Graphics32 }
  GR32_Image,
  GR32_Layers,
{ GraphicsMagic Lib }
  gmTypes,
{ GraphicsMagic Package Lib }
  gmGradient,
  gmGridBased_List,
  gmGradient_List,
  gmGradientsGrid,
  gmGridBased_FileDlg,
  gmGradient_FileDlgs;

type
  TgmGradientUser = (guNone,
                     guGradientTools,
                     guGradientMapCommand,
                     guGradientMapLayer,
                     guGradientFillLayer);

  TgmGradientListState = record
    FileName          : string;
    SelectedIndex     : Integer;
    PreviousIndex     : Integer;
    ThumbnailSizeMode : TgmThumbnailSizeMode;
    Modified          : Boolean;
    UseInternal       : Boolean;
  end;

  TfrmGradientPicker = class(TForm)
    pnlFormBorder: TPanel;
    scrlbxGradientPicker: TScrollBox;
    stsbrGradientInfo: TStatusBar;
    spdbtnGradientOptions: TSpeedButton;
    pmnGradientOptions: TPopupMenu;
    pmnitmSmallThumbnail: TMenuItem;
    pmnitmLargeThumbnail: TMenuItem;
    pmnitmSeparator1: TMenuItem;
    pmnitmSaveGradientsAs: TMenuItem;
    pmnitmReplaceGradients: TMenuItem;
    pmnitmSaveChanges: TMenuItem;
    pmnitmSeparator2: TMenuItem;
    pmnitmDeleteGradient: TMenuItem;
    pmnitmRenameGradient: TMenuItem;
    pmnitmNewGradient: TMenuItem;
    pmnitmSeparator3: TMenuItem;
    pmnitmResetGradients: TMenuItem;
    glDefaultGradients: TgmGradientList;
    glDrawingToolGradients: TgmGradientList;
    ogdOpenGradientDialog: TOpenGradientDialog;
    sgdSaveGradientDialog: TSaveGradientDialog;
    pmnitmLoadGradients: TMenuItem;
    glMapCommandGradients: TgmGradientList;
    glFillLayerGradients: TgmGradientList;
    glMapLayerGradients: TgmGradientList;
    ggGradients: TgmGradientsGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeGradientThumbnailSize(Sender: TObject);
    procedure pmnGradientOptionsPopup(Sender: TObject);
    procedure spdbtnGradientOptionsClick(Sender: TObject);
    procedure pmnitmSaveGradientsAsClick(Sender: TObject);
    procedure pmnitmReplaceGradientsClick(Sender: TObject);
    procedure pmnitmSaveChangesClick(Sender: TObject);
    procedure pmnitmDeleteGradientClick(Sender: TObject);
    procedure pmnitmRenameGradientClick(Sender: TObject);
    procedure pmnitmNewGradientClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pmnitmResetGradientsClick(Sender: TObject);
    procedure ggGradientsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ggGradientsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ggGradientsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pmnitmLoadGradientsClick(Sender: TObject);
  private
    FGradientUser : TgmGradientUser;

    procedure ShowPartialGradientOptions(const AVisible: Boolean);
  
    procedure UpdateDrawingToolGradients;
    procedure UpdateMapCommandGradients;
    procedure UpdateFillLayerGradients;
    procedure UpdateMapLayerGradients;
    procedure UpdateThumbnailSize;
  public
    FDrawingToolGradientListState : TgmGradientListState;
    FMapCommandGradientListState  : TgmGradientListState;
    FFillLayerGradientListState   : TgmGradientListState;
    FMapLayerGradientListState    : TgmGradientListState;

    procedure ShowDrawingToolSelectedGradient;
    procedure ShowMapCommandSelectedGradient;
    procedure ShowFillLayerSelectedGradient;
    procedure ShowMapLayerSelectedGradient;
    procedure SetDynamicColors(const AForeColor, ABackColor: TColor);

    function GetFillLayerGradient: TgmGradientItem;
    function GetMapLayerGradient: TgmGradientItem;

    property GradientUser: TgmGradientUser read FGradientUser write FGradientUser;
  end;

var
  frmGradientPicker: TfrmGradientPicker;

implementation

uses
{ GraphicsMagic Lib }
  gmConstants,
  gmGradientFillLayer,
  gmGradientMapLayer,
  gmIni,
  gmLayers,
{ GraphicsMagic Forms }
  MainForm,
{ GraphicsMagic Dialogs }
  GradientFillDlg,
  GradientMapDlg,
  RenameGradientDlg;

{$R *.DFM}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmGradientPicker.ShowPartialGradientOptions(
  const AVisible: Boolean);
begin
  pmnitmResetGradients.Visible   := AVisible;
  pmnitmSaveChanges.Visible      := AVisible;
  pmnitmReplaceGradients.Visible := AVisible;
  pmnitmSaveGradientsAs.Visible  := AVisible;
  pmnitmSmallThumbnail.Visible   := AVisible;
  pmnitmLargeThumbnail.Visible   := AVisible;
  pmnitmSeparator1.Visible       := AVisible;
  pmnitmSeparator2.Visible       := AVisible;
  pmnitmSeparator3.Visible       := AVisible;
end;

procedure TfrmGradientPicker.UpdateDrawingToolGradients;
begin
  glDrawingToolGradients.Gradients.Clear;

  // whether we use default gradients...
  try
    FDrawingToolGradientListState.UseInternal := 
      Boolean(StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1') ));
  except
    FDrawingToolGradientListState.UseInternal := True;
    WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, IntToStr(1) );
  end;

  if FDrawingToolGradientListState.UseInternal then
  begin
    glDrawingToolGradients.Gradients.Assign(glDefaultGradients.Gradients);
    FDrawingToolGradientListState.FileName := '';
  end
  else
  begin
    FDrawingToolGradientListState.FileName :=
      ReadInfoFromIniFile(SECTION_GRADIENT, IDENT_OPEN_GRADIENTS_FILE, '');
  
    try
      glDrawingToolGradients.LoadFromFile(FDrawingToolGradientListState.FileName);

      if glDrawingToolGradients.Count = 0 then
      begin
        glDrawingToolGradients.Gradients.Assign(glDefaultGradients.Gradients);
        WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1');

        FDrawingToolGradientListState.FileName    := '';
        FDrawingToolGradientListState.UseInternal := True;
      end;
      
    except
      // If failure in loading external Gradients,
      // then load the internal Gradients.
      glDrawingToolGradients.Gradients.Assign(glDefaultGradients.Gradients);
      WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1');

      FDrawingToolGradientListState.FileName    := '';
      FDrawingToolGradientListState.UseInternal := True;
    end;
  end;

  // get selected index...
  try
    FDrawingToolGradientListState.SelectedIndex :=
      StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, '0') );
  except
    FDrawingToolGradientListState.SelectedIndex := 0;
    WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, IntToStr(0) );
  end;

  // get thumbnail size...
  try
    FDrawingToolGradientListState.ThumbnailSizeMode := 
      TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT,
                                       IDENT_GRADIENT_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (FDrawingToolGradientListState.ThumbnailSizeMode in [
              tsmSmall, tsmLarge]) then
    begin
      FDrawingToolGradientListState.ThumbnailSizeMode := tsmLarge;
      WriteInfoToIniFile(  SECTION_GRADIENT, IDENT_GRADIENT_THUMBNAIL_SIZE_MODE, IntToStr( Ord(FDrawingToolGradientListState.ThumbnailSizeMode) )  );
    end;
  except
    FDrawingToolGradientListState.ThumbnailSizeMode := tsmLarge;
    WriteInfoToIniFile(  SECTION_GRADIENT, IDENT_GRADIENT_THUMBNAIL_SIZE_MODE, IntToStr( Ord(FDrawingToolGradientListState.ThumbnailSizeMode) )  );
  end;

  UpdateThumbnailSize;

  // selecting gradients...
  if glDrawingToolGradients.Count > 0 then
  begin
    if FDrawingToolGradientListState.SelectedIndex > (glDrawingToolGradients.Count - 1) then
    begin
      FDrawingToolGradientListState.SelectedIndex := 0;
      WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, IntToStr(0) );
    end;
  end;

  ggGradients.Gradients     := glDrawingToolGradients;
  ggGradients.GradientIndex := FDrawingToolGradientListState.SelectedIndex;

  FDrawingToolGradientListState.Modified      := False;
  FDrawingToolGradientListState.PreviousIndex := FDrawingToolGradientListState.SelectedIndex;
end;

procedure TfrmGradientPicker.UpdateMapCommandGradients;
begin
  glMapCommandGradients.Gradients.Clear;

  // whether we use default gradients...
  try
    FMapCommandGradientListState.UseInternal := 
      Boolean(StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1') ));
  except
    FMapCommandGradientListState.UseInternal := True;
    WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, IntToStr(1) );
  end;

  if FMapCommandGradientListState.UseInternal then
  begin
    glMapCommandGradients.Gradients.Assign(glDefaultGradients.Gradients);
    FMapCommandGradientListState.FileName := '';
  end
  else
  begin
    FMapCommandGradientListState.FileName :=
      ReadInfoFromIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_OPEN_FILE, '');
  
    try
      glMapCommandGradients.LoadFromFile(FMapCommandGradientListState.FileName);

      if glMapCommandGradients.Count = 0 then
      begin
        glMapCommandGradients.Gradients.Assign(glDefaultGradients.Gradients);
        WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1');

        FMapCommandGradientListState.FileName    := '';
        FMapCommandGradientListState.UseInternal := True;
      end;
      
    except
      // If failure in loading external Gradients,
      // then load the internal Gradients.
      glMapCommandGradients.Gradients.Assign(glDefaultGradients.Gradients);
      WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1');

      FMapCommandGradientListState.FileName    := '';
      FMapCommandGradientListState.UseInternal := True;
    end;
  end;

  // get selected index...
  try
    FMapCommandGradientListState.SelectedIndex :=
      StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, '0') );
  except
    FMapCommandGradientListState.SelectedIndex := 0;
    WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, IntToStr(0) );
  end;

  // get thumbnail size...
  try
    FMapCommandGradientListState.ThumbnailSizeMode := 
      TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP,
                                       IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (FMapCommandGradientListState.ThumbnailSizeMode in [
              tsmSmall, tsmLarge]) then
    begin
      FMapCommandGradientListState.ThumbnailSizeMode := tsmLarge;
      
      WriteInfoToIniFile(  SECTION_GRADIENT_MAP, IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(FMapCommandGradientListState.ThumbnailSizeMode) )  );
    end;
  except
    FMapCommandGradientListState.ThumbnailSizeMode := tsmLarge;
    
    WriteInfoToIniFile(  SECTION_GRADIENT_MAP, IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(FMapCommandGradientListState.ThumbnailSizeMode) )  );
  end;

  UpdateThumbnailSize;

  // selecting gradients...
  if glMapCommandGradients.Count > 0 then
  begin
    if FMapCommandGradientListState.SelectedIndex > (glMapCommandGradients.Count - 1) then
    begin
      FMapCommandGradientListState.SelectedIndex := 0;
      WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, IntToStr(0) );
    end;
  end;

  ggGradients.Gradients     := glMapCommandGradients;
  ggGradients.GradientIndex := FMapCommandGradientListState.SelectedIndex;

  FMapCommandGradientListState.Modified      := False;
  FMapCommandGradientListState.PreviousIndex := FMapCommandGradientListState.SelectedIndex;
end;

procedure TfrmGradientPicker.UpdateFillLayerGradients;
begin
  glFillLayerGradients.Gradients.Clear;

  // whether we use default gradients...
  try
    FFillLayerGradientListState.UseInternal := 
      Boolean(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1') )  );
  except
    FFillLayerGradientListState.UseInternal := True;
    WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, IntToStr(1) );
  end;

  if FFillLayerGradientListState.UseInternal then
  begin
    glFillLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
    FFillLayerGradientListState.FileName := '';
  end
  else
  begin
    FFillLayerGradientListState.FileName :=
      ReadInfoFromIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_OPEN_FILE, '');
  
    try
      glFillLayerGradients.LoadFromFile(FFillLayerGradientListState.FileName);

      if glFillLayerGradients.Count = 0 then
      begin
        glFillLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
        WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1');

        FFillLayerGradientListState.FileName    := '';
        FFillLayerGradientListState.UseInternal := True;
      end;
      
    except
      // If failure in loading external Gradients,
      // then load the internal Gradients.
      glFillLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
      WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1');

      FFillLayerGradientListState.FileName    := '';
      FFillLayerGradientListState.UseInternal := True;
    end;
  end;

  // get selected index...
  try
    FFillLayerGradientListState.SelectedIndex :=
      StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, '0') );
  except
    FFillLayerGradientListState.SelectedIndex := 0;
    WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, IntToStr(0) );
  end;

  // get thumbnail size...
  try
    FFillLayerGradientListState.ThumbnailSizeMode := 
      TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_LAYER,
                                       IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (FFillLayerGradientListState.ThumbnailSizeMode in [
              tsmSmall, tsmLarge]) then
    begin
      FFillLayerGradientListState.ThumbnailSizeMode := tsmLarge;
      
      WriteInfoToIniFile(  SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(FFillLayerGradientListState.ThumbnailSizeMode) )  );
    end;
  except
    FFillLayerGradientListState.ThumbnailSizeMode := tsmLarge;
    
    WriteInfoToIniFile(  SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(FFillLayerGradientListState.ThumbnailSizeMode) )  );
  end;

  UpdateThumbnailSize;

  // selecting gradients...
  if glFillLayerGradients.Count > 0 then
  begin
    if FFillLayerGradientListState.SelectedIndex > (glFillLayerGradients.Count - 1) then
    begin
      FFillLayerGradientListState.SelectedIndex := 0;
      WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, IntToStr(0) );
    end;
  end;

  ggGradients.Gradients     := glFillLayerGradients;
  ggGradients.GradientIndex := FFillLayerGradientListState.SelectedIndex;

  FFillLayerGradientListState.Modified      := False;
  FFillLayerGradientListState.PreviousIndex := FFillLayerGradientListState.SelectedIndex;
end;

procedure TfrmGradientPicker.UpdateMapLayerGradients;
begin
  glMapLayerGradients.Gradients.Clear;

  // whether we use default gradients...
  try
    FMapLayerGradientListState.UseInternal := 
      Boolean(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP_LAYER,
                                              IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1') )  );
  except
    FMapLayerGradientListState.UseInternal := True;

    WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                        IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, IntToStr(1) );
  end;

  if FMapLayerGradientListState.UseInternal then
  begin
    glMapLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
    FMapLayerGradientListState.FileName := '';
  end
  else
  begin
    FMapLayerGradientListState.FileName :=
      ReadInfoFromIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_OPEN_FILE, '');
  
    try
      glMapLayerGradients.LoadFromFile(FMapLayerGradientListState.FileName);

      if glMapLayerGradients.Count = 0 then
      begin
        glMapLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);

        WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                           IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1');

        FMapLayerGradientListState.FileName    := '';
        FMapLayerGradientListState.UseInternal := True;
      end;
      
    except
      // If failure in loading external Gradients,
      // then load the internal Gradients.
      glMapLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);

      WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                         IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1');

      FMapLayerGradientListState.FileName    := '';
      FMapLayerGradientListState.UseInternal := True;
    end;
  end;

  // get selected index...
  try
    FMapLayerGradientListState.SelectedIndex :=
      StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP_LAYER,
                                    IDENT_GRADIENT_MAP_LAYER_INDEX, '0') );
  except
    FMapLayerGradientListState.SelectedIndex := 0;
    
    WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                        IDENT_GRADIENT_MAP_LAYER_INDEX, IntToStr(0) );
  end;

  // get thumbnail size...
  try
    FMapLayerGradientListState.ThumbnailSizeMode := 
      TgmThumbnailSizeMode(  StrToInt( ReadInfoFromIniFile(SECTION_GRADIENT_MAP_LAYER,
                                       IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE, '0') )  );

    if not (FMapLayerGradientListState.ThumbnailSizeMode in [
              tsmSmall, tsmLarge]) then
    begin
      FMapLayerGradientListState.ThumbnailSizeMode := tsmLarge;
      
      WriteInfoToIniFile(  SECTION_GRADIENT_MAP_LAYER,
                           IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE,
                           IntToStr( Ord(FMapLayerGradientListState.ThumbnailSizeMode) )  );
    end;
  except
    FMapLayerGradientListState.ThumbnailSizeMode := tsmLarge;
    
    WriteInfoToIniFile(  SECTION_GRADIENT_MAP_LAYER,
                         IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE,
                         IntToStr( Ord(FMapLayerGradientListState.ThumbnailSizeMode) )  );
  end;

  UpdateThumbnailSize;

  // selecting gradients...
  if glMapLayerGradients.Count > 0 then
  begin
    if FMapLayerGradientListState.SelectedIndex > (glMapLayerGradients.Count - 1) then
    begin
      FMapLayerGradientListState.SelectedIndex := 0;

      WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                          IDENT_GRADIENT_MAP_LAYER_INDEX, IntToStr(0) );
    end;
  end;

  ggGradients.Gradients     := glMapLayerGradients;
  ggGradients.GradientIndex := FMapLayerGradientListState.SelectedIndex;

  FMapLayerGradientListState.Modified      := False;
  FMapLayerGradientListState.PreviousIndex := FMapLayerGradientListState.SelectedIndex;
end;

procedure TfrmGradientPicker.UpdateThumbnailSize;
begin
  case FGradientUser of
    guGradientTools:
      begin
        case FDrawingToolGradientListState.ThumbnailSizeMode of
          tsmSmall:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_SMALL;
            end;

          tsmLarge:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_LARGE;
            end;
        end;
      end;

    guGradientMapCommand:
      begin
        case FMapCommandGradientListState.ThumbnailSizeMode of
          tsmSmall:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_SMALL;
            end;

          tsmLarge:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_LARGE;
            end;
        end;
      end;

    guGradientFillLayer:
      begin
        case FFillLayerGradientListState.ThumbnailSizeMode of
          tsmSmall:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_SMALL;
            end;

          tsmLarge:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_LARGE;
            end;
        end;
      end;

    guGradientMapLayer:
      begin
        case FMapLayerGradientListState.ThumbnailSizeMode of
          tsmSmall:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_SMALL;
            end;

          tsmLarge:
            begin
              ggGradients.ThumbSize := THUMBNAIL_SIZE_LARGE;
            end;
        end;
      end;
  end;
end;

procedure TfrmGradientPicker.ShowDrawingToolSelectedGradient;
var
  LGradient : TgmGradientItem;
begin
  if glDrawingToolGradients.IsValidIndex(FDrawingToolGradientListState.SelectedIndex) then
  begin
    LGradient := glDrawingToolGradients.Items[FDrawingToolGradientListState.SelectedIndex];
    LGradient.GradientLength := frmMain.imgSelectedGradient.Bitmap.Width;
    LGradient.RefreshColorArray;

    LGradient.DrawColorGradients(frmMain.imgSelectedGradient.Bitmap,
                                 frmMain.chckbxReverseGradient.Checked);

    frmMain.imgSelectedGradient.Bitmap.Changed;
  end;
end;

procedure TfrmGradientPicker.ShowMapCommandSelectedGradient;
var
  LGradient : TgmGradientItem;
begin
  if glMapCommandGradients.IsValidIndex(FMapCommandGradientListState.SelectedIndex) then
  begin
    LGradient := glMapCommandGradients.Items[FMapCommandGradientListState.SelectedIndex];
    LGradient.GradientLength := frmGradientMap.imgSelectedGradient.Bitmap.Width;
    LGradient.RefreshColorArray;

    LGradient.DrawColorGradients(frmGradientMap.imgSelectedGradient.Bitmap,
                                 frmGradientMap.chckbxReverse.Checked);

    frmGradientMap.imgSelectedGradient.Bitmap.Changed;
  end;
end;

procedure TfrmGradientPicker.ShowFillLayerSelectedGradient;
var
  LGradientFillLayer : TgmGradientFillLayer;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmGradientFillLayer then
    begin
      LGradientFillLayer := TgmGradientFillLayer(LayerList.SelectedLayer);

      LGradientFillLayer.Gradient.GradientLength := frmGradientFill.imgSelectedGradient.Bitmap.Width;
      LGradientFillLayer.Gradient.RefreshColorArray;

      LGradientFillLayer.Gradient.DrawColorGradients(
        frmGradientFill.imgSelectedGradient.Bitmap,
        LGradientFillLayer.IsReversed);

      frmGradientFill.imgSelectedGradient.Bitmap.Changed;
    end;
  end;
end;

procedure TfrmGradientPicker.ShowMapLayerSelectedGradient;
var
  LGradientMapLayer : TgmGradientMapLayer;
  LTempGradient     : TgmGradientItem;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmGradientMapLayer then
    begin
      LGradientMapLayer := TgmGradientMapLayer(LayerList.SelectedLayer);

      LTempGradient := TgmGradientItem.Create(nil);
      try
        LTempGradient.Assign(LGradientMapLayer.Gradient);

        LTempGradient.GradientLength := frmGradientMap.imgSelectedGradient.Bitmap.Width;
        LTempGradient.RefreshColorArray;
    
        LTempGradient.DrawColorGradients(frmGradientMap.imgSelectedGradient.Bitmap,
                                         frmGradientMap.chckbxReverse.Checked);

        frmGradientMap.imgSelectedGradient.Bitmap.Changed;
      finally
        LTempGradient.Free;
      end;
    end;
  end;
end;

procedure TfrmGradientPicker.SetDynamicColors(
  const AForeColor, ABackColor: TColor);
begin
  glDefaultGradients.ForegroundColor := AForeColor;
  glDefaultGradients.BackgroundColor := ABackColor;

  glDrawingToolGradients.ForegroundColor := AForeColor;
  glDrawingToolGradients.BackgroundColor := ABackColor;
  ShowDrawingToolSelectedGradient;

  glMapCommandGradients.ForegroundColor := AForeColor;
  glMapCommandGradients.BackgroundColor := ABackColor;

  glFillLayerGradients.ForegroundColor := AForeColor;
  glFillLayerGradients.BackgroundColor := ABackColor;

  glMapLayerGradients.ForegroundColor := AForeColor;
  glMapLayerGradients.BackgroundColor := ABackColor;
end;

function TfrmGradientPicker.GetFillLayerGradient: TgmGradientItem;
begin
  Result := nil;

  if (glFillLayerGradients.Count > 0) and
     (glFillLayerGradients.IsValidIndex(FFillLayerGradientListState.SelectedIndex)) then
  begin
    Result := glFillLayerGradients.Items[FFillLayerGradientListState.SelectedIndex];
  end;
end;

function TfrmGradientPicker.GetMapLayerGradient: TgmGradientItem;
begin
  Result := nil;

  if (glMapLayerGradients.Count > 0) and
     (glMapLayerGradients.IsValidIndex(FMapLayerGradientListState.SelectedIndex)) then
  begin
    Result := glMapLayerGradients.Items[FMapLayerGradientListState.SelectedIndex];
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmGradientPicker.FormCreate(Sender: TObject);
begin
  FGradientUser := guNone;
  
  UpdateDrawingToolGradients; // Gradient Tools
  UpdateMapCommandGradients;  // Gradient Map manu command
  UpdateFillLayerGradients;   // Gradient Fill Layer
  UpdateMapLayerGradients;    // Gradient Map Layer 
end; 

procedure TfrmGradientPicker.FormDeactivate(Sender: TObject);
begin
  Close;
end; 

procedure TfrmGradientPicker.FormShow(Sender: TObject);
var
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  ggGradients.GradientIndex := -1;
  ggGradients.Gradients     := nil;

  glDefaultGradients.ForegroundColor := frmMain.GlobalForeColor;
  glDefaultGradients.BackgroundColor := frmMain.GlobalBackColor;

  glDrawingToolGradients.ForegroundColor := frmMain.GlobalForeColor;
  glDrawingToolGradients.BackgroundColor := frmMain.GlobalBackColor;

  glMapCommandGradients.ForegroundColor := frmMain.GlobalForeColor;
  glMapCommandGradients.BackgroundColor := frmMain.GlobalBackColor;

  glFillLayerGradients.ForegroundColor := frmMain.GlobalForeColor;
  glFillLayerGradients.BackgroundColor := frmMain.GlobalBackColor;

  glMapLayerGradients.ForegroundColor := frmMain.GlobalForeColor;
  glMapLayerGradients.BackgroundColor := frmMain.GlobalBackColor;
  
  case FGradientUser of
    guGradientTools:
      begin
        ggGradients.Gradients     := glDrawingToolGradients;
        ggGradients.GradientIndex := FDrawingToolGradientListState.SelectedIndex;

        ShowDrawingToolSelectedGradient;
      end;

    guGradientMapCommand:
      begin
        ggGradients.Gradients     := glMapCommandGradients;
        ggGradients.GradientIndex := FMapCommandGradientListState.SelectedIndex;
      end;

    guGradientFillLayer:
      begin
        ggGradients.Gradients     := glFillLayerGradients;
        ggGradients.GradientIndex := FFillLayerGradientListState.SelectedIndex;

        LGradientFillLayer := TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);
        LGradientFillLayer.Gradient := GetFillLayerGradient;
        LGradientFillLayer.Changed;

        ShowFillLayerSelectedGradient;
      end;

    guGradientMapLayer:
      begin
        ggGradients.Gradients     := glMapLayerGradients;
        ggGradients.GradientIndex := FMapLayerGradientListState.SelectedIndex;

        LGradientMapLayer := TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);
        LGradientMapLayer.Gradient := GetMapLayerGradient;

        if frmGradientMap.chckbxPreview.Checked then
        begin
          LGradientMapLayer.Changed;
        end;

        ShowMapLayerSelectedGradient;
      end;
  end;

  UpdateThumbnailSize;
end;

procedure TfrmGradientPicker.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  case FGradientUser of
    guGradientTools:
      begin
        if FDrawingToolGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
                          mtConfirmation, [mbYes, mbNo], 0) of
            mrYes:
              begin
                pmnitmSaveChangesClick(Sender);
              end;

            mrNo:
              begin
                if FDrawingToolGradientListState.UseInternal then
                begin
                  glDrawingToolGradients.Gradients.Assign(glDefaultGradients.Gradients);
                end
                else
                begin
                  glDrawingToolGradients.Gradients.LoadFromFile(FDrawingToolGradientListState.FileName);
                end;

                FDrawingToolGradientListState.SelectedIndex := FDrawingToolGradientListState.PreviousIndex;
                FDrawingToolGradientListState.Modified      := False;

                ggGradients.GradientIndex := FDrawingToolGradientListState.SelectedIndex;

                ShowDrawingToolSelectedGradient;
              end;
          end;
        end;
      end;

    guGradientMapCommand:
      begin
        if FMapCommandGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
                          mtConfirmation, [mbYes, mbNo], 0) of
            mrYes:
              begin
                pmnitmSaveChangesClick(Sender);
              end;

            mrNo:
              begin
                if FMapCommandGradientListState.UseInternal then
                begin
                  glMapCommandGradients.Gradients.Assign(glDefaultGradients.Gradients);
                end
                else
                begin
                  glMapCommandGradients.Gradients.LoadFromFile(FMapCommandGradientListState.FileName);
                end;

                FMapCommandGradientListState.SelectedIndex := FMapCommandGradientListState.PreviousIndex;
                FMapCommandGradientListState.Modified      := False;

                ggGradients.GradientIndex := FMapCommandGradientListState.SelectedIndex;

                frmGradientMap.ExecuteGradientMap;
              end;
          end;
        end;
      end;

    guGradientFillLayer:
      begin
        if FFillLayerGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
                          mtConfirmation, [mbYes, mbNo], 0) of
            mrYes:
              begin
                pmnitmSaveChangesClick(Sender);
              end;

            mrNo:
              begin
                if FFillLayerGradientListState.UseInternal then
                begin
                  glFillLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
                end
                else
                begin
                  glFillLayerGradients.Gradients.LoadFromFile(FFillLayerGradientListState.FileName);
                end;

                FFillLayerGradientListState.SelectedIndex := FFillLayerGradientListState.PreviousIndex;
                FFillLayerGradientListState.Modified      := False;

                ggGradients.GradientIndex := FFillLayerGradientListState.SelectedIndex;

                LGradientFillLayer :=
                  TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

                LGradientFillLayer.Gradient := GetFillLayerGradient;
                LGradientFillLayer.Changed;
              end;
          end;
        end;
      end;

    guGradientMapLayer:
      begin
        if FMapLayerGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
                          mtConfirmation, [mbYes, mbNo], 0) of
            mrYes:
              begin
                pmnitmSaveChangesClick(Sender);
              end;

            mrNo:
              begin
                if FMapLayerGradientListState.UseInternal then
                begin
                  glMapLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
                end
                else
                begin
                  glMapLayerGradients.Gradients.LoadFromFile(FMapLayerGradientListState.FileName);
                end;

                FMapLayerGradientListState.SelectedIndex := FMapLayerGradientListState.PreviousIndex;
                FMapLayerGradientListState.Modified      := False;

                ggGradients.GradientIndex := FMapLayerGradientListState.SelectedIndex;

                LGradientMapLayer :=
                  TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

                LGradientMapLayer.Gradient := GetMapLayerGradient;

                if frmGradientMap.chckbxPreview.Checked then
                begin
                  LGradientMapLayer.Changed;
                end;
              end;
          end;
        end;
      end;
  end;
end;

procedure TfrmGradientPicker.ChangeGradientThumbnailSize(Sender: TObject);
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

  case LThumbnailSizeMode of
    tsmSmall:
      begin
        ggGradients.ThumbSize := THUMBNAIL_SIZE_SMALL;
      end;

    tsmLarge:
      begin
        ggGradients.ThumbSize := THUMBNAIL_SIZE_LARGE;
      end;
  end;
  
  case FGradientUser of
    guGradientTools:
      begin
        FDrawingToolGradientListState.ThumbnailSizeMode := LThumbnailSizeMode;
        
        WriteInfoToIniFile(  SECTION_GRADIENT,
                             IDENT_GRADIENT_THUMBNAIL_SIZE_MODE,
                             IntToStr( Ord(LThumbnailSizeMode) )  );
      end;
      
    guGradientMapCommand:
      begin
        FMapCommandGradientListState.ThumbnailSizeMode := LThumbnailSizeMode;

        WriteInfoToIniFile(  SECTION_GRADIENT_MAP,
                             IDNET_GRADIENT_MAP_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;

    guGradientFillLayer:
      begin
        FFillLayerGradientListState.ThumbnailSizeMode := LThumbnailSizeMode;

        WriteInfoToIniFile(  SECTION_GRADIENT_LAYER,
                             IDENT_GRADIENT_LAYER_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;

    guGradientMapLayer:
      begin
        FMapLayerGradientListState.ThumbnailSizeMode := LThumbnailSizeMode;

        WriteInfoToIniFile(  SECTION_GRADIENT_MAP_LAYER,
                             IDENT_GRADIENT_MAP_LAYER_THUMBNAIL_SIZE_MODE,
                             IntToStr( Integer(LThumbnailSizeMode) )  );
      end;
  end;
end; 

procedure TfrmGradientPicker.pmnGradientOptionsPopup(Sender: TObject);
begin
  pmnitmSaveGradientsAs.Enabled := (ggGradients.Gradients.Count > 0);
  pmnitmSmallThumbnail.Enabled  := (ggGradients.Gradients.Count > 0);
  pmnitmLargeThumbnail.Enabled  := (ggGradients.Gradients.Count > 0);

  case FGradientUser of
    guGradientTools:
      begin
        with FDrawingToolGradientListState do
        begin
          pmnitmResetGradients.Enabled := (not UseInternal) or Modified;
          pmnitmSaveChanges.Enabled    := (ggGradients.Gradients.Count > 0) and Modified;
          pmnitmRenameGradient.Enabled := (ggGradients.Gradients.Count > 0) and (SelectedIndex >= 0);
          pmnitmDeleteGradient.Enabled := (ggGradients.Gradients.Count > 1) and (ggGradients.Gradients.IsValidIndex(SelectedIndex));
          pmnitmSmallThumbnail.Checked := (ThumbnailSizeMode = tsmSmall);
          pmnitmLargeThumbnail.Checked := (ThumbnailSizeMode = tsmLarge);
        end;
      end;
      
    guGradientMapCommand:
      begin
        with FMapCommandGradientListState do
        begin
          pmnitmResetGradients.Enabled := (not UseInternal) or Modified;
          pmnitmSaveChanges.Enabled    := (ggGradients.Gradients.Count > 0) and Modified;
          pmnitmRenameGradient.Enabled := (ggGradients.Gradients.Count > 0) and (SelectedIndex >= 0);
          pmnitmDeleteGradient.Enabled := (ggGradients.Gradients.Count > 1) and (ggGradients.Gradients.IsValidIndex(SelectedIndex));
          pmnitmSmallThumbnail.Checked := (ThumbnailSizeMode = tsmSmall);
          pmnitmLargeThumbnail.Checked := (ThumbnailSizeMode = tsmLarge);
        end;
      end;

    guGradientFillLayer:
      begin
        with FFillLayerGradientListState do
        begin
          pmnitmResetGradients.Enabled := (not UseInternal) or Modified;
          pmnitmSaveChanges.Enabled    := (ggGradients.Gradients.Count > 0) and Modified;
          pmnitmRenameGradient.Enabled := (ggGradients.Gradients.Count > 0) and (SelectedIndex >= 0);
          pmnitmDeleteGradient.Enabled := (ggGradients.Gradients.Count > 1) and (ggGradients.Gradients.IsValidIndex(SelectedIndex));
          pmnitmSmallThumbnail.Checked := (ThumbnailSizeMode = tsmSmall);
          pmnitmLargeThumbnail.Checked := (ThumbnailSizeMode = tsmLarge);
        end;
      end;

    guGradientMapLayer:
      begin
        with FMapLayerGradientListState do
        begin
          pmnitmResetGradients.Enabled := (not UseInternal) or Modified;
          pmnitmSaveChanges.Enabled    := (ggGradients.Gradients.Count > 0) and Modified;
          pmnitmRenameGradient.Enabled := (ggGradients.Gradients.Count > 0) and (SelectedIndex >= 0);
          pmnitmDeleteGradient.Enabled := (ggGradients.Gradients.Count > 1) and (ggGradients.Gradients.IsValidIndex(SelectedIndex));
          pmnitmSmallThumbnail.Checked := (ThumbnailSizeMode = tsmSmall);
          pmnitmLargeThumbnail.Checked := (ThumbnailSizeMode = tsmLarge);
        end;
      end;
  end;
end;

procedure TfrmGradientPicker.spdbtnGradientOptionsClick(Sender: TObject);
var
  p : TPoint;
begin
  ShowPartialGradientOptions(True);
  GetCursorPos(p);
  pmnGradientOptions.Popup(p.X, p.Y);
end; 

procedure TfrmGradientPicker.pmnitmSaveGradientsAsClick(Sender: TObject);

  procedure OverwriteGradientsFile(const AFileName: string);
  begin
    if ggGradients.Gradients.Count > 0 then
    begin
      ggGradients.Gradients.SaveToFile(AFileName);

      case FGradientUser of
        guGradientTools:
          begin
            FDrawingToolGradientListState.FileName    := AFileName;
            FDrawingToolGradientListState.UseInternal := False;
            FDrawingToolGradientListState.Modified    := False;

            WriteInfoToIniFile(SECTION_GRADIENT, IDENT_OPEN_GRADIENTS_FILE, AFileName);
            WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '0');
          end;

        guGradientMapCommand:
          begin
            FMapCommandGradientListState.FileName    := AFileName;
            FMapCommandGradientListState.UseInternal := False;
            FMapCommandGradientListState.Modified    := False;

            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_OPEN_FILE, AFileName);
            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '0');
          end;

        guGradientFillLayer:
          begin
            FFillLayerGradientListState.FileName    := AFileName;
            FFillLayerGradientListState.UseInternal := False;
            FFillLayerGradientListState.Modified    := False;

            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_OPEN_FILE, AFileName);
            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '0');
          end;

        guGradientMapLayer:
          begin
            FMapLayerGradientListState.FileName    := AFileName;
            FMapLayerGradientListState.UseInternal := False;
            FMapLayerGradientListState.Modified    := False;
            
            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_OPEN_FILE, AFileName);
            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '0');
          end;
      end;
    end;
  end; 

var
  LFileName, LFileExt : string;
begin
  if sgdSaveGradientDialog.Execute then
  begin
    LFileName := sgdSaveGradientDialog.FileName;
    LFileExt  := LowerCase( ExtractFileExt(LFileName) );

    if LFileExt = '' then
    begin
      LFileName := LFileName + '.grd';
    end
    else
    begin
      if LFileExt <> '.grd' then
      begin
        LFileName := ChangeFileExt(LFileName, '.grd');
      end;
    end;

    if FileExists(LFileName) then
    begin
      if MessageDlg('The file is already existed. Do you want to replace it?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        OverwriteGradientsFile(LFileName);
      end;
    end
    else
    begin
      OverwriteGradientsFile(LFileName);
    end;
  end;
end;

procedure TfrmGradientPicker.pmnitmReplaceGradientsClick(Sender: TObject);
var
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  case FGradientUser of
    guGradientTools:
      begin
        if FDrawingToolGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

        glDrawingToolGradients.Gradients.Clear;
        pmnitmLoadGradientsClick(Sender);

        FDrawingToolGradientListState.Modified      := False;
        FDrawingToolGradientListState.SelectedIndex := 0;
        ggGradients.GradientIndex                   := 0;

        ShowDrawingToolSelectedGradient;
        WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, IntToStr(0) );
      end;

    guGradientMapCommand:
      begin
        if FMapCommandGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

        glMapCommandGradients.Gradients.Clear;
        pmnitmLoadGradientsClick(Sender);

        FMapCommandGradientListState.Modified      := False;
        FMapCommandGradientListState.SelectedIndex := 0;
        ggGradients.GradientIndex                  := 0;

        ShowMapCommandSelectedGradient;
        frmGradientMap.ExecuteGradientMap;
        
        WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, IntToStr(0) );
      end;

    guGradientFillLayer:
      begin
        if FFillLayerGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

        glFillLayerGradients.Gradients.Clear;
        pmnitmLoadGradientsClick(Sender);

        FFillLayerGradientListState.Modified      := False;
        FFillLayerGradientListState.SelectedIndex := 0;
        ggGradients.GradientIndex                 := 0;

        LGradientFillLayer :=
          TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

        LGradientFillLayer.Gradient := GetFillLayerGradient;
        LGradientFillLayer.Changed;

        ShowFillLayerSelectedGradient;
    
        WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, IntToStr(0) );
      end;

    guGradientMapLayer:
      begin
        if FMapLayerGradientListState.Modified then
        begin
          case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

        glMapLayerGradients.Gradients.Clear;
        pmnitmLoadGradientsClick(Sender);

        FMapLayerGradientListState.Modified      := False;
        FMapLayerGradientListState.SelectedIndex := 0;
        ggGradients.GradientIndex                := 0;

        LGradientMapLayer :=
          TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

        LGradientMapLayer.Gradient := GetMapLayerGradient;

        if frmGradientMap.chckbxPreview.Checked then
        begin
          LGradientMapLayer.Changed();
        end;

        ShowMapLayerSelectedGradient;
    
        WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                            IDENT_GRADIENT_MAP_LAYER_INDEX, IntToStr(0) );
      end;
  end;
end; 

procedure TfrmGradientPicker.pmnitmSaveChangesClick(Sender: TObject);
var
  LFileName : string;
begin
  if ggGradients.Gradients.Count > 0 then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          LFileName := FDrawingToolGradientListState.FileName;
        end;

      guGradientMapCommand:
        begin
          LFileName := FMapCommandGradientListState.FileName;
        end;

      guGradientFillLayer:
        begin
          LFileName := FFillLayerGradientListState.FileName;
        end;

      guGradientMapLayer:
        begin
          LFileName := FMapLayerGradientListState.FileName;
        end;
    end;

    if LFileName <> '' then
    begin
      ggGradients.Gradients.SaveToFile(LFileName);

      case FGradientUser of
        guGradientTools:
          begin
            FDrawingToolGradientListState.Modified := False;
          end;

        guGradientMapCommand:
          begin
            FMapCommandGradientListState.Modified := False;
          end;

        guGradientFillLayer:
          begin
            FFillLayerGradientListState.Modified := False;
          end;

        guGradientMapLayer:
          begin
            FMapLayerGradientListState.Modified := False;
          end;
      end;
    end
    else
    begin
      pmnitmSaveGradientsAsClick(Sender);
    end;
  end;
end; 

procedure TfrmGradientPicker.pmnitmDeleteGradientClick(Sender: TObject);
var
  LPreviousIndex     : Integer;
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  if Assigned(ggGradients.Gradients) and
     (ggGradients.Gradients.Count > 1) and
     (ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex)) then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          FDrawingToolGradientListState.PreviousIndex :=
            FDrawingToolGradientListState.SelectedIndex;
        end;

      guGradientMapCommand:
        begin
          FMapCommandGradientListState.PreviousIndex :=
            FMapCommandGradientListState.SelectedIndex;
        end;

      guGradientFillLayer:
        begin
          FFillLayerGradientListState.PreviousIndex :=
            FFillLayerGradientListState.SelectedIndex;
        end;

      guGradientMapLayer:
        begin
          FMapLayerGradientListState.PreviousIndex :=
            FMapLayerGradientListState.SelectedIndex;
        end;
    end;
    
    LPreviousIndex := ggGradients.GradientIndex - 1;

    if LPreviousIndex < 0 then
    begin
      LPreviousIndex := 0;
    end;

    ggGradients.Gradients.Gradients.Delete(ggGradients.GradientIndex);
    ggGradients.GradientIndex := LPreviousIndex;

    case FGradientUser of
      guGradientTools:
        begin
          FDrawingToolGradientListState.SelectedIndex := ggGradients.GradientIndex;
          FDrawingToolGradientListState.Modified      := True;

          WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX,
                              IntToStr(ggGradients.GradientIndex) );

          ShowDrawingToolSelectedGradient;
        end;

      guGradientMapCommand:
        begin
          FMapCommandGradientListState.SelectedIndex := ggGradients.GradientIndex;
          FMapCommandGradientListState.Modified      := True;

          WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX,
                              IntToStr(ggGradients.GradientIndex) );

          ShowMapCommandSelectedGradient;
          frmGradientMap.ExecuteGradientMap;
        end;

      guGradientFillLayer:
        begin
          FFillLayerGradientListState.SelectedIndex := ggGradients.GradientIndex;
          FFillLayerGradientListState.Modified      := True;

          WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX,
                              IntToStr(ggGradients.GradientIndex) );


          LGradientFillLayer :=
            TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

          LGradientFillLayer.Gradient := GetFillLayerGradient;
          LGradientFillLayer.Changed;

          ShowFillLayerSelectedGradient;
        end;

      guGradientMapLayer:
        begin
          FMapLayerGradientListState.SelectedIndex := ggGradients.GradientIndex;
          FMapLayerGradientListState.Modified      := True;

          WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                              IDENT_GRADIENT_MAP_LAYER_INDEX,
                              IntToStr(ggGradients.GradientIndex) );

          LGradientMapLayer :=
            TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

          LGradientMapLayer.Gradient := GetMapLayerGradient();

          if frmGradientMap.chckbxPreview.Checked then
          begin
            LGradientMapLayer.Changed();
          end;

          ShowMapLayerSelectedGradient();
        end;
    end;
  end;
end;

procedure TfrmGradientPicker.pmnitmRenameGradientClick(Sender: TObject);
var
  LGradient : TgmGradientItem;
begin
  frmGradientName := TfrmGradientName.Create(nil);
  try
    if frmGradientName.ShowModal = mrOK then
    begin
      if (ggGradients.Gradients.Count > 0) and
         ( ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) ) then
      begin
        LGradient             := ggGradients.Gradients.Items[ggGradients.GradientIndex];
        LGradient.DisplayName := frmGradientName.edtName.Text;
         
        case FGradientUser of
          guGradientTools:
            begin
              FDrawingToolGradientListState.Modified := True;
            end;

          guGradientMapCommand:
            begin
              FMapCommandGradientListState.Modified := True;
            end;

          guGradientFillLayer:
            begin
              FFillLayerGradientListState.Modified := True;
            end;

          guGradientMapLayer:
            begin
              FMapLayerGradientListState.Modified := True;
            end;
        end;
      end;
    end;
  finally
    FreeAndNil(frmGradientName);
  end;
end; 

procedure TfrmGradientPicker.pmnitmNewGradientClick(Sender: TObject);
var
  LSelectedGradient  : TgmGradientItem;
  LNewGradient       : TgmGradientItem;
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  if (ggGradients.Gradients.Count > 0) and
     (ggGradients.GradientIndex >= 0) then
  begin
    frmGradientName := TfrmGradientName.Create(nil);
    try
      if frmGradientName.ShowModal = mrOK then
      begin
        case FGradientUser of
          guGradientTools:
            begin
              FDrawingToolGradientListState.PreviousIndex :=
                FDrawingToolGradientListState.SelectedIndex;
            end;

          guGradientMapCommand:
            begin
              FMapCommandGradientListState.PreviousIndex :=
                FMapCommandGradientListState.SelectedIndex;
            end;

          guGradientFillLayer:
            begin
              FFillLayerGradientListState.PreviousIndex :=
                FFillLayerGradientListState.SelectedIndex;
            end;

          guGradientMapLayer:
            begin
              FMapLayerGradientListState.PreviousIndex :=
                FMapLayerGradientListState.SelectedIndex;
            end;
        end;

        LSelectedGradient := ggGradients.Gradients.Items[ggGradients.GradientIndex];
        LNewGradient      := ggGradients.Gradients.Gradients.Add;

        LNewGradient.Assign(LSelectedGradient);
        LNewGradient.DisplayName  := frmGradientName.edtName.Text;
        ggGradients.GradientIndex := ggGradients.Gradients.Count - 1;

        case FGradientUser of
          guGradientTools:
            begin
              FDrawingToolGradientListState.SelectedIndex := ggGradients.Gradients.Count - 1;
              FDrawingToolGradientListState.Modified      := True;

              WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX,
                                  IntToStr(FDrawingToolGradientListState.SelectedIndex) );
            end;

          guGradientMapCommand:
            begin
              FMapCommandGradientListState.SelectedIndex := ggGradients.Gradients.Count - 1;
              FMapCommandGradientListState.Modified      := True;

              WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX,
                                  IntToStr(FMapCommandGradientListState.SelectedIndex) );
            end;

          guGradientFillLayer:
            begin
              FFillLayerGradientListState.SelectedIndex := ggGradients.Gradients.Count - 1;
              FFillLayerGradientListState.Modified      := True;

              LGradientFillLayer :=
                TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

              LGradientFillLayer.Gradient := Self.GetFillLayerGradient;
              LGradientFillLayer.Changed;
            end;

          guGradientMapLayer:
            begin
              FMapLayerGradientListState.SelectedIndex := ggGradients.Gradients.Count - 1;
              FMapLayerGradientListState.Modified      := True;

              LGradientMapLayer :=
                TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

              LGradientMapLayer.Gradient := Self.GetMapLayerGradient();

              if frmGradientMap.chckbxPreview.Checked then
              begin
                LGradientMapLayer.Changed();
              end;
            end;
        end;
      end;
    finally
      FreeAndNil(frmGradientName);
    end;
  end;
end;

procedure TfrmGradientPicker.pmnitmResetGradientsClick(Sender: TObject);
var
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
begin
  if MessageDlg('Replace current Gradient with the default Gradient?',
                mtConfirmation, [mbOK, mbCancel], 0) = idOK then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          if FDrawingToolGradientListState.Modified then
          begin
            case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

          glDrawingToolGradients.Gradients.Assign(glDefaultGradients.Gradients);
          FDrawingToolGradientListState.FileName      := '';
          FDrawingToolGradientListState.SelectedIndex := 0;
          FDrawingToolGradientListState.UseInternal   := True;
          FDrawingToolGradientListState.Modified      := False;

          WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, IntToStr(0) );
          WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1');

          ShowDrawingToolSelectedGradient;
        end;

      guGradientMapCommand:
        begin
          if FMapCommandGradientListState.Modified then
          begin
            case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

          glMapCommandGradients.Gradients.Assign(glDefaultGradients.Gradients);
          FMapCommandGradientListState.FileName      := '';
          FMapCommandGradientListState.SelectedIndex := 0;
          FMapCommandGradientListState.UseInternal   := True;
          FMapCommandGradientListState.Modified      := False;

          WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, IntToStr(0) );
          WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1');

          ShowMapCommandSelectedGradient;
          frmGradientMap.ExecuteGradientMap;
        end;

      guGradientFillLayer:
        begin
          if FFillLayerGradientListState.Modified then
          begin
            case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

          glFillLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
          FFillLayerGradientListState.FileName      := '';
          FFillLayerGradientListState.SelectedIndex := 0;
          FFillLayerGradientListState.UseInternal   := True;
          FFillLayerGradientListState.Modified      := False;

          WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, IntToStr(0) );
          WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1');

          LGradientFillLayer :=
            TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

          LGradientFillLayer.Gradient := GetFillLayerGradient;
          LGradientFillLayer.Changed;

          ShowFillLayerSelectedGradient;
        end;

      guGradientMapLayer:
        begin
          if FMapLayerGradientListState.Modified then
          begin
            case MessageDlg('The Gradient has been changed. Do you want to save these changes?',
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

          glMapLayerGradients.Gradients.Assign(glDefaultGradients.Gradients);
          FMapLayerGradientListState.FileName      := '';
          FMapLayerGradientListState.SelectedIndex := 0;
          FMapLayerGradientListState.UseInternal   := True;
          FMapLayerGradientListState.Modified      := False;

          WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_INDEX, IntToStr(0) );
          WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER, IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1');

          LGradientMapLayer :=
            TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

          LGradientMapLayer.Gradient := GetMapLayerGradient();

          if frmGradientMap.chckbxPreview.Checked then
          begin
            LGradientMapLayer.Changed();
          end;

          ShowMapLayerSelectedGradient();
        end;
    end;

    ggGradients.GradientIndex := 0;
  end;
end;

procedure TfrmGradientPicker.ggGradientsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ggGradients.Gradients.Count > 0 then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          // remember the old index
          FDrawingToolGradientListState.PreviousIndex := ggGradients.GradientIndex;
        end;

      guGradientMapCommand:
        begin
          // remember the old index
          FMapCommandGradientListState.PreviousIndex := ggGradients.GradientIndex;
        end;

      guGradientFillLayer:
        begin
          // remember the old index
          FFillLayerGradientListState.PreviousIndex := ggGradients.GradientIndex;
        end;

      guGradientMapLayer:
        begin
          // remember the old index
          FMapLayerGradientListState.PreviousIndex := ggGradients.GradientIndex;
        end;
    end;
  end;
end;

procedure TfrmGradientPicker.ggGradientsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LIndex    : Integer;
  LGradient : TgmGradientItem;
begin
  LIndex := ggGradients.GetGradientIndex(X, Y);

  if LIndex >= 0 then
  begin
    LGradient := ggGradients.Gradients.Items[LIndex];
    stsbrGradientInfo.Panels[0].Text := LGradient.DisplayName;
    ggGradients.Cursor               := crHandPoint;
  end
  else
  begin
    stsbrGradientInfo.Panels[0].Text := '';
    ggGradients.Cursor               := crNo;
  end;
end;

procedure TfrmGradientPicker.ggGradientsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LGradientFillLayer : TgmGradientFillLayer;
  LGradientMapLayer  : TgmGradientMapLayer;
  p                  : TPoint;
begin
  if ggGradients.Gradients.Count > 0 then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          FDrawingToolGradientListState.SelectedIndex := ggGradients.GradientIndex;

          if ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) and
             (ggGradients.Gradients = glDrawingToolGradients) then
          begin
            WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX,
                                IntToStr(ggGradients.GradientIndex) );

            ShowDrawingToolSelectedGradient;
          end;
        end;

      guGradientMapCommand:
        begin
          FMapCommandGradientListState.SelectedIndex := ggGradients.GradientIndex;

          if ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) and
             (ggGradients.Gradients = glMapCommandGradients) then
          begin
            WriteInfoToIniFile( SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX,
                                IntToStr(ggGradients.GradientIndex) );

            ShowMapCommandSelectedGradient;
            frmGradientMap.ExecuteGradientMap;
          end;
        end;

      guGradientFillLayer:
        begin
          FFillLayerGradientListState.SelectedIndex := ggGradients.GradientIndex;

          if ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) and
             (ggGradients.Gradients = glFillLayerGradients) then
          begin
            WriteInfoToIniFile( SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX,
                                IntToStr(ggGradients.GradientIndex) );

            if ActiveChildForm.LayerList.SelectedLayer is TgmGradientFillLayer then
            begin
              LGradientFillLayer :=
                TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);

              LGradientFillLayer.Gradient := GetFillLayerGradient;
              LGradientFillLayer.Changed;

              ShowFillLayerSelectedGradient;
            end;
          end;
        end;

      guGradientMapLayer:
        begin
          FMapLayerGradientListState.SelectedIndex := ggGradients.GradientIndex;

          if ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) and
             (ggGradients.Gradients = glMapLayerGradients) then
          begin
            WriteInfoToIniFile( SECTION_GRADIENT_MAP_LAYER,
                                IDENT_GRADIENT_MAP_LAYER_INDEX,
                                IntToStr(ggGradients.GradientIndex) );

            if ActiveChildForm.LayerList.SelectedLayer is TgmGradientMapLayer then
            begin
              LGradientMapLayer :=
                TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);

              LGradientMapLayer.Gradient := GetMapLayerGradient;

              if frmGradientMap.chckbxPreview.Checked then
              begin
                LGradientMapLayer.Changed;
              end;

              ShowMapLayerSelectedGradient;
            end;
          end;
        end;
    end;

    if Button = mbRight then
    begin
      if ggGradients.Gradients.IsValidIndex(ggGradients.GradientIndex) then
      begin
        ShowPartialGradientOptions(False);
        GetCursorPos(p);
        pmnGradientOptions.Popup(p.X, p.Y);
      end;
    end;
  end;
end;

procedure TfrmGradientPicker.pmnitmLoadGradientsClick(Sender: TObject);
var
  LFileName, LOpenDir : string;
  LExternalLoadOK     : Boolean;
  LGradientFillLayer  : TgmGradientFillLayer;
  LGradientMapLayer   : TgmGradientMapLayer;
begin
  LExternalLoadOK := False;

  case FGradientUser of
    guGradientTools:
      begin
        LFileName := FDrawingToolGradientListState.FileName;
      end;

    guGradientMapCommand:
      begin
        LFileName := FMapCommandGradientListState.FileName;
      end;

    guGradientFillLayer:
      begin
        LFileName := FFillLayerGradientListState.FileName;
      end;

    guGradientMapLayer:
      begin
        LFileName := FMapLayerGradientListState.FileName;
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

  ogdOpenGradientDialog.InitialDir := LOpenDir;

  if ogdOpenGradientDialog.Execute then
  begin
    case FGradientUser of
      guGradientTools:
        begin
          try
            glDrawingToolGradients.LoadFromFile(ogdOpenGradientDialog.FileName);

            if glDrawingToolGradients.Count > 0 then
            begin
              LExternalLoadOK := True;
            end;

          except
            glDrawingToolGradients.Assign(glDefaultGradients);
          end;

          if LExternalLoadOK then
          begin
            FDrawingToolGradientListState.FileName    := ogdOpenGradientDialog.FileName;
            FDrawingToolGradientListState.UseInternal := False;
            FDrawingToolGradientListState.Modified    := True;

            WriteInfoToIniFile(SECTION_GRADIENT, IDENT_OPEN_GRADIENTS_FILE,
                               ogdOpenGradientDialog.FileName);

            WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '0');
          end
          else
          begin
            FDrawingToolGradientListState.FileName      := '';
            FDrawingToolGradientListState.UseInternal   := True;
            FDrawingToolGradientListState.SelectedIndex := 0;
            FDrawingToolGradientListState.Modified      := False;
            ShowDrawingToolSelectedGradient;

            WriteInfoToIniFile(SECTION_GRADIENT, IDENT_GRADIENT_USE_INTERNAL, '1');
            WriteInfoToIniFile( SECTION_GRADIENT, IDENT_GRADIENTS_INDEX, IntToStr(0) );
          end;
        end;

      guGradientMapCommand:
        begin
          try
            glMapCommandGradients.LoadFromFile(ogdOpenGradientDialog.FileName);

            if glMapCommandGradients.Count > 0 then
            begin
              LExternalLoadOK := True;
            end;

          except
            glMapCommandGradients.Assign(glDefaultGradients);
          end;

          if LExternalLoadOK then
          begin
            FMapCommandGradientListState.FileName    := ogdOpenGradientDialog.FileName;
            FMapCommandGradientListState.UseInternal := False;
            FMapCommandGradientListState.Modified    := True;

            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_OPEN_FILE,
                               ogdOpenGradientDialog.FileName);

            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '0');
          end
          else
          begin
            FMapCommandGradientListState.FileName      := '';
            FMapCommandGradientListState.UseInternal   := True;
            FMapCommandGradientListState.SelectedIndex := 0;
            FMapCommandGradientListState.Modified      := False;

            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_USE_INTERNAL, '1');
            WriteInfoToIniFile(SECTION_GRADIENT_MAP, IDENT_GRADIENT_MAP_INDEX, IntToStr(0));

            ShowMapCommandSelectedGradient;
            frmGradientMap.ExecuteGradientMap;
          end;
        end;

      guGradientFillLayer:
        begin
          try
            glFillLayerGradients.LoadFromFile(ogdOpenGradientDialog.FileName);

            if glFillLayerGradients.Count > 0 then
            begin
              LExternalLoadOK := True;
            end;

          except
            glFillLayerGradients.Assign(glDefaultGradients);
          end;

          if LExternalLoadOK then
          begin
            FFillLayerGradientListState.FileName    := ogdOpenGradientDialog.FileName;
            FFillLayerGradientListState.UseInternal := False;
            FFillLayerGradientListState.Modified    := True;

            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_OPEN_FILE,
                               ogdOpenGradientDialog.FileName);

            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '0');
          end
          else
          begin
            FFillLayerGradientListState.FileName      := '';
            FFillLayerGradientListState.UseInternal   := True;
            FFillLayerGradientListState.SelectedIndex := 0;
            FFillLayerGradientListState.Modified      := False;

            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_USE_INTERNAL, '1');
            WriteInfoToIniFile(SECTION_GRADIENT_LAYER, IDENT_GRADIENT_LAYER_INDEX, IntToStr(0));

            LGradientFillLayer :=
              TgmGradientFillLayer(ActiveChildForm.LayerList.SelectedLayer);
              
            LGradientFillLayer.Gradient := GetFillLayerGradient;
            LGradientFillLayer.Changed;

            ShowFillLayerSelectedGradient;
          end;
        end;

      guGradientMapLayer:
        begin
          try
            glMapLayerGradients.LoadFromFile(ogdOpenGradientDialog.FileName);

            if glMapLayerGradients.Count > 0 then
            begin
              LExternalLoadOK := True;
            end;

          except
            glMapLayerGradients.Assign(glDefaultGradients);
          end;

          if LExternalLoadOK then
          begin
            FMapLayerGradientListState.FileName    := ogdOpenGradientDialog.FileName;
            FMapLayerGradientListState.UseInternal := False;
            FMapLayerGradientListState.Modified    := True;

            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                               IDENT_GRADIENT_MAP_LAYER_OPEN_FILE,
                               ogdOpenGradientDialog.FileName);

            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                               IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '0');
          end
          else
          begin
            FMapLayerGradientListState.FileName      := '';
            FMapLayerGradientListState.UseInternal   := True;
            FMapLayerGradientListState.SelectedIndex := 0;
            FMapLayerGradientListState.Modified      := False;

            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                               IDENT_GRADIENT_MAP_LAYER_USE_INTERNAL, '1');

            WriteInfoToIniFile(SECTION_GRADIENT_MAP_LAYER,
                               IDENT_GRADIENT_MAP_LAYER_INDEX, IntToStr(0));

            LGradientMapLayer :=
              TgmGradientMapLayer(ActiveChildForm.LayerList.SelectedLayer);
              
            LGradientMapLayer.Gradient := GetMapLayerGradient();

            if frmGradientMap.chckbxPreview.Checked then
            begin
              LGradientMapLayer.Changed();
            end;

            ShowMapLayerSelectedGradient();
          end;
        end;
    end;
  end;
end;

end.
