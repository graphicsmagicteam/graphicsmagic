unit LayerForm;

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
 *   x2nie - Fathony Luthfillah < x2nie@yahoo.com >
 *     Adding additional Paint-Stages that above all layers.
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

// Update Date: 2017/01/12

interface

uses
{ Delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, StdCtrls, ExtCtrls, 
{ Graphics32 }
  GR32_RangeBars,
{ GraphicsMagic lib }
  gmLayerPanelManager;

type
  TfrmLayers = class(TForm)
    tlbrLayerTools: TToolBar;
    tlbtnNewLayer: TToolButton;
    tlbtnSeparator1: TToolButton;
    tlbtnDeleteLayer: TToolButton;
    tlbrAddMask: TToolButton;
    tlbtnSpecialLayers: TToolButton;
    tlbrBlendModes: TToolBar;
    tlbnSeparator2: TToolButton;
    cmbbxBlendModes: TComboBox;
    tlbrLayerOpacity: TToolBar;
    ToolButton1: TToolButton;
    ggbrLayerOpacity: TGaugeBar;
    edtLayerOpacity: TEdit;
    lblLayerOpacity: TLabel;
    tlbrLayerLockTools: TToolBar;
    ToolButton5: TToolButton;
    lblLockOption: TLabel;
    chckbxLockTransparency: TCheckBox;
    imgLockTransparency: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cmbbxBlendModesChange(Sender: TObject);
    procedure ggbrLayerOpacityChange(Sender: TObject);
    procedure ggbrLayerOpacityMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FChangeLocked      : Boolean;
    FLayerPanelManager : TgmLayerPanelManager;

    procedure LayerPanelManagerBeforeMouseDown(ASender: TObject);
  public
    procedure LockChange;
    procedure UnlockChange;

    property IsChangeLocked    : Boolean              read FChangeLocked;
    property LayerPanelManager : TgmLayerPanelManager read FLayerPanelManager;
  end;

var
  frmLayers: TfrmLayers;

implementation

uses
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Data Modules }
  MainDataModule,
{ GraphicsMagic Form }
  MainForm;

{$R *.dfm}

{ Custom Methods }

procedure TfrmLayers.LayerPanelManagerBeforeMouseDown(ASender: TObject);
begin
  if Assigned(ActiveChildForm) then
  begin
    if Assigned(FLayerPanelManager) then
    begin
      FLayerPanelManager.IsEnabled := not ActiveChildForm.ToolsInPendingStatus();
    end;
  end;
end;

procedure TfrmLayers.LockChange;
begin
  FChangeLocked := True;
end;

procedure TfrmLayers.UnlockChange;
begin
  FChangeLocked := False;
end;

procedure TfrmLayers.FormCreate(Sender: TObject);
begin
  FLayerPanelManager                 := TgmLayerPanelManager.Create(Self);
  FLayerPanelManager.Parent          := Self;
  FLayerPanelManager.Align           := alClient;
  FLayerPanelManager.BeforeMouseDown := Self.LayerPanelManagerBeforeMouseDown;

  cmbbxBlendModes.Items     := BlendModeList;
  cmbbxBlendModes.ItemIndex := 0;

  ManualDock(frmMain.pgcntrlDockSite3);
  Show();

  FChangeLocked := False;
end;

procedure TfrmLayers.FormDestroy(Sender: TObject);
begin
  FLayerPanelManager.Free();
end;

procedure TfrmLayers.FormActivate(Sender: TObject);
begin
  if Assigned(FLayerPanelManager) then
  begin
    FLayerPanelManager.SetFocus();
  end;
end;

procedure TfrmLayers.cmbbxBlendModesChange(Sender: TObject);
begin
  if FChangeLocked then
  begin
    Exit;
  end;
  
  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm.LayerList.SelectedLayer do
    begin
      LayerBlendMode := TBlendMode32(cmbbxBlendModes.ItemIndex);
    end;
  end;
end;

procedure TfrmLayers.ggbrLayerOpacityChange(Sender: TObject);
begin
  edtLayerOpacity.Text := IntToStr(ggbrLayerOpacity.Position);
end;

procedure TfrmLayers.ggbrLayerOpacityMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FChangeLocked then
  begin
    Exit;
  end;

  if Assigned(ActiveChildForm) then
  begin
    with ActiveChildForm.LayerList.SelectedLayer do
    begin
      LayerOpacity := MulDiv(ggbrLayerOpacity.Position, 255, 100);
    end;
  end;
end;

end.
