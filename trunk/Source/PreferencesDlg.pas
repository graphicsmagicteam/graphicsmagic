unit PreferencesDlg;

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

// Update Date: 2015/10/17

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmPreferences = class(TForm)
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    lstbxCategories: TListBox;
    ntbkPreferences: TNotebook;
    pnlInterface: TPanel;
    pnlPerformance: TPanel;
    grpbxHistory: TGroupBox;
    lblHistoryStates: TLabel;
    edtHistoryStates: TEdit;
    pnlInterfaceGeneral: TGroupBox;
    chckbxGhostMode: TCheckBox;
    lblGhostFadeInterval: TLabel;
    edtGhostFadeInterval: TEdit;
    lblGhostFadeIntervalUnit: TLabel;
    lblGhostMaxOpaque: TLabel;
    edtGhostMaxOpaque: TEdit;
    lblGhostMaxOpaquePercent: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure edtHistoryStatesChange(Sender: TObject);
    procedure lstbxCategoriesClick(Sender: TObject);
    procedure chckbxGhostModeClick(Sender: TObject);
    procedure edtGhostFadeIntervalChange(Sender: TObject);
    procedure edtGhostMaxOpaqueChange(Sender: TObject);
  private
    FHistoryStateCount : Integer;
    FGhostFadeInterval : Integer;
    FGhostMaxOpaque    : Integer;

    function GetHistoryStateCount: Integer;
  public
    property HistoryStatesCount : Integer read GetHistoryStateCount;
  end;

var
  frmPreferences: TfrmPreferences;

implementation

uses
{ Grpahics32 }
  GR32_LowLevel,
{ GraphicsMagic Lib }
  gmConstants,
  gmIni,
  gmMath;

{$R *.dfm}


function TfrmPreferences.GetHistoryStateCount: Integer;
begin
  Result := FHistoryStateCount;
end;

procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  // Interface Pager...
  chckbxGhostMode.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_MODE_ENABLED, '1')));

  edtGhostFadeInterval.Text := ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_FADE_INTERVAL, '50');
  FGhostFadeInterval        := StrToInt(edtGhostFadeInterval.Text);

  FGhostMaxOpaque := StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_GHOST_MAX_OPAQUE, '255'));
  FGhostMaxOpaque := Clamp(FGhostMaxOpaque, MAX_GHOST_OPAQUE_LOWER, MAX_GHOST_OPAQUE_UPPER);

  edtGhostMaxOpaque.Text := IntToStr(FGhostMaxOpaque * 100 div 255);

  lblGhostFadeInterval.Enabled     := chckbxGhostMode.Checked;
  edtGhostFadeInterval.Enabled     := chckbxGhostMode.Checked;
  lblGhostFadeIntervalUnit.Enabled := chckbxGhostMode.Checked;
  lblGhostMaxOpaque.Enabled        := chckbxGhostMode.Checked;
  edtGhostMaxOpaque.Enabled        := chckbxGhostMode.Checked;
  lblGhostMaxOpaquePercent.Enabled := chckbxGhostMode.Checked;

  // Performance Page...
  FHistoryStateCount    := StrToInt(ReadInfoFromIniFile(SECTION_PREFERENCES, IDENT_HISTORY_STATES, '20'));
  edtHistoryStates.Text := IntToStr(FHistoryStateCount);

  ntbkPreferences.ActivePage := 'Interface';
end;

procedure TfrmPreferences.btbtnOKClick(Sender: TObject);
var
  LGhostFadeInterval : Integer;
  LGhostMaxOpaque    : Integer;
begin
  // Interface Pager...
  WriteInfoToIniFile(SECTION_PREFERENCES, IDENT_GHOST_MODE_ENABLED, IntToStr(Ord(chckbxGhostMode.Checked)));

  LGhostFadeInterval := StrToInt(edtGhostFadeInterval.Text);
  LGhostFadeInterval := Clamp(LGhostFadeInterval, MIN_GHOST_FADE_INTERVAL, MAX_GHOST_FADE_INTERVAL);
  WriteInfoToIniFile(SECTION_PREFERENCES, IDENT_GHOST_FADE_INTERVAL, IntToStr(LGhostFadeInterval));

  LGhostMaxOpaque := StrToInt(edtGhostMaxOpaque.Text);
  LGhostMaxOpaque := LGhostMaxOpaque * 255 div 100;
  LGhostMaxOpaque := Clamp(LGhostMaxOpaque, MAX_GHOST_OPAQUE_LOWER, MAX_GHOST_OPAQUE_UPPER);
  WriteInfoToIniFile(SECTION_PREFERENCES, IDENT_GHOST_MAX_OPAQUE, IntToStr(LGhostMaxOpaque));

  // Performance Page...
  WriteInfoToIniFile(SECTION_PREFERENCES, IDENT_HISTORY_STATES, IntToStr(FHistoryStateCount));
end;

procedure TfrmPreferences.edtHistoryStatesChange(Sender: TObject);
begin
  try
    FHistoryStateCount := StrToInt(edtHistoryStates.Text);
    EnsureValueInRange(FHistoryStateCount, 1, 100);
    edtHistoryStates.Text := IntToStr(FHistoryStateCount);
  except
    edtHistoryStates.Text := IntToStr(FHistoryStateCount);
  end;
end; 

procedure TfrmPreferences.lstbxCategoriesClick(Sender: TObject);
begin
  ntbkPreferences.PageIndex := lstbxCategories.ItemIndex;
end;

procedure TfrmPreferences.chckbxGhostModeClick(Sender: TObject);
begin
  lblGhostFadeInterval.Enabled     := chckbxGhostMode.Checked;
  edtGhostFadeInterval.Enabled     := chckbxGhostMode.Checked;
  lblGhostFadeIntervalUnit.Enabled := chckbxGhostMode.Checked;
  lblGhostMaxOpaque.Enabled        := chckbxGhostMode.Checked;
  edtGhostMaxOpaque.Enabled        := chckbxGhostMode.Checked;
  lblGhostMaxOpaquePercent.Enabled := chckbxGhostMode.Checked;
end;

procedure TfrmPreferences.edtGhostFadeIntervalChange(Sender: TObject);
var
  LInterval : Integer;
begin
  try
    LInterval                 := StrToInt(edtGhostFadeInterval.Text);
    FGhostFadeInterval        := Clamp(LInterval, MIN_GHOST_FADE_INTERVAL, MAX_GHOST_FADE_INTERVAL);
    edtGhostFadeInterval.Text := IntToStr(FGhostFadeInterval);
  except
    edtGhostFadeInterval.Text := IntToStr(FGhostFadeInterval);
  end;
end;

procedure TfrmPreferences.edtGhostMaxOpaqueChange(Sender: TObject);
var
  LMaxOpaque : Integer;
begin
  try
    LMaxOpaque             := StrToInt(edtGhostMaxOpaque.Text);
    FGhostMaxOpaque        := LMaxOpaque * 255 div 100;
    FGhostMaxOpaque        := Clamp(FGhostMaxOpaque, MAX_GHOST_OPAQUE_LOWER, MAX_GHOST_OPAQUE_UPPER);
    edtGhostMaxOpaque.Text := IntToStr(FGhostMaxOpaque * 100 div 255);
  except
    edtGhostMaxOpaque.Text := IntToStr(FGhostMaxOpaque * 100 div 255);
  end;
end;

end.
