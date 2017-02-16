{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit BrushDynamicsPopFrm;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, gmBrushes;

type
  TfrmBrushDynamics = class(TForm)
    pnlBrushDynamics: TPanel;
    grpbxBrushDynamics: TGroupBox;
    lblSizeDynamics: TLabel;
    cmbbxSizeDynamics: TComboBox;
    edtSizeSteps: TEdit;
    lblSizeSteps: TLabel;
    lblOpacityDynamics: TLabel;
    cmbbxOpacityDynamics: TComboBox;
    edtOpacitySteps: TEdit;
    lblOpacitySteps: TLabel;
    lblColorDynamics: TLabel;
    cmbbxColorDynamics: TComboBox;
    edtColorSteps: TEdit;
    lblColorSteps: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure cmbbxSizeDynamicsChange(Sender: TObject);
    procedure cmbbxOpacityDynamicsChange(Sender: TObject);
    procedure cmbbxColorDynamicsChange(Sender: TObject);
    procedure edtSizeStepsChange(Sender: TObject);
    procedure edtOpacityStepsChange(Sender: TObject);
    procedure edtColorStepsChange(Sender: TObject);
  private
    FSizeSteps           : Integer;
    FOpacitySteps        : Integer;
    FColorSteps          : Integer;
    FSizeDynamicsState   : TgmBrushDynamicState;
    FOpacityDynamicsState: TgmBrushDynamicState;
    FColorDynamicsState  : TgmBrushDynamicState;
  public
    procedure UpdateSizeStepsEditStatus;
    procedure UpdateOpacityStepsEditStatus;
    procedure UpdateColorStepsEditStatus;

    property SizeSteps           : Integer              read FSizeSteps;
    property OpacitySteps        : Integer              read FOpacitySteps;
    property ColorSteps          : Integer              read FColorSteps;
    property SizeDynamicsState   : TgmBrushDynamicState read FSizeDynamicsState;
    property OpacityDynamicsState: TgmBrushDynamicState read FOpacityDynamicsState;
    property ColorDynamicsState  : TgmBrushDynamicState read FColorDynamicsState;
  end;

var
  frmBrushDynamics: TfrmBrushDynamics;

implementation

uses
{ GraphicsMagic Lib }
  gmMath,
{ GraphicsMagic Forms/Dialogs }
  MainForm;

{$R *.DFM}

//-- Custom Methods ------------------------------------------------------------

procedure TfrmBrushDynamics.UpdateSizeStepsEditStatus;
begin
  case cmbbxSizeDynamics.ItemIndex of
    0:
      begin
        edtSizeSteps.Color    := clBtnFace;
        edtSizeSteps.Text     := '';
        edtSizeSteps.ReadOnly := True;
      end;

    1:
      begin
        edtSizeSteps.Color    := clWhite;
        edtSizeSteps.Text     := IntToStr(FSizeSteps);
        edtSizeSteps.ReadOnly := False;
      end;
  end;
end;

procedure TfrmBrushDynamics.UpdateOpacityStepsEditStatus;
begin
  case cmbbxOpacityDynamics.ItemIndex of
    0:
      begin
        edtOpacitySteps.Color    := clBtnFace;
        edtOpacitySteps.Text     := '';
        edtOpacitySteps.ReadOnly := True;
      end;

    1:
      begin
        edtOpacitySteps.Color    := clWhite;
        edtOpacitySteps.Text     := IntToStr(FOpacitySteps);
        edtOpacitySteps.ReadOnly := False;
      end;
  end;
end;

procedure TfrmBrushDynamics.UpdateColorStepsEditStatus;
begin
  case cmbbxColorDynamics.ItemIndex of
    0:
      begin
        edtColorSteps.Color    := clBtnFace;
        edtColorSteps.Text     := '';
        edtColorSteps.ReadOnly := True;
      end;

    1:
      begin
        edtColorSteps.Color    := clWhite;
        edtColorSteps.Text     := IntToStr(FColorSteps);
        edtColorSteps.ReadOnly := False;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmBrushDynamics.FormCreate(Sender: TObject);
begin
  FSizeSteps                     := 1;
  FOpacitySteps                  := 1;
  FColorSteps                    := 1;
  FSizeDynamicsState             := bdsOff;
  FOpacityDynamicsState          := bdsOff;
  FColorDynamicsState            := bdsOff;
  cmbbxSizeDynamics.ItemIndex    := 0;
  cmbbxOpacityDynamics.ItemIndex := 0;
  cmbbxColorDynamics.ItemIndex   := 0;
  
  UpdateSizeStepsEditStatus;
  UpdateOpacityStepsEditStatus;
  UpdateColorStepsEditStatus;
end;

procedure TfrmBrushDynamics.FormShow(Sender: TObject);
begin
  case frmMain.MainTool of
    gmtBrush:
      begin
        if frmMain.BrushTool in [btAirBrush, btBlurSharpenBrush,
                                 btJetGunBrush, btSmudge] then
        begin
          lblOpacityDynamics.Caption := 'Pressure:';
        end
        else if frmMain.BrushTool = btLightBrush then
        begin
          lblOpacityDynamics.Caption := 'Intensity:';
        end
        else if frmMain.BrushTool = btDodgeBurnBrush then
        begin
          lblOpacityDynamics.Caption := 'Exposure:';
        end
        else
        begin
          lblOpacityDynamics.Caption := 'Opacity:';
        end;

        lblColorDynamics.Enabled   := (frmMain.BrushTool in [btPaintBrush, btAirBrush, btJetGunBrush]);
        edtColorSteps.Enabled      := lblColorDynamics.Enabled;
        lblColorSteps.Enabled      := lblColorDynamics.Enabled;
        cmbbxColorDynamics.Enabled := lblColorDynamics.Enabled;

        if cmbbxColorDynamics.Enabled then
        begin
          cmbbxColorDynamics.Color := clWindow;
        end
        else
        begin
          cmbbxColorDynamics.Color := clBtnFace;
        end;

        if FColorDynamicsState = bdsFade then
        begin
          if edtColorSteps.Enabled then
          begin
            edtColorSteps.Color := clWindow;
            edtColorSteps.Text  := IntToStr(FColorSteps);
          end
          else
          begin
            edtColorSteps.Color := clBtnFace;
            edtColorSteps.Text  := '';
          end;
        end;
      end;

    gmtEraser:
      begin
        case frmMain.EraserTool of
          etEraser:
            begin
              case frmMain.ErasingMode of
                emPaintBrush:
                  begin
                    lblOpacityDynamics.Caption := 'Opacity:';
                  end;
                  
                emAirBrush:
                  begin
                    lblOpacityDynamics.Caption := 'Pressure:';
                  end;
              end;
            end;

          etBackgroundEraser:
            begin
              lblOpacityDynamics.Caption := 'Tolerance:';
            end;
        end;

        lblColorDynamics.Enabled   := False;
        edtColorSteps.Enabled      := lblColorDynamics.Enabled;
        lblColorSteps.Enabled      := lblColorDynamics.Enabled;
        cmbbxColorDynamics.Enabled := lblColorDynamics.Enabled;

        if cmbbxColorDynamics.Enabled then
        begin
          cmbbxColorDynamics.Color := clWindow;
        end
        else
        begin
          cmbbxColorDynamics.Color := clBtnFace;
        end;
        
        edtColorSteps.Color := clBtnFace;
        edtColorSteps.Text  := '';
      end;
  end;

  ActiveControl := grpbxBrushDynamics;
end;

procedure TfrmBrushDynamics.FormDeactivate(Sender: TObject);
begin
  Close;  // Close itself when missing the focus.
end;

procedure TfrmBrushDynamics.cmbbxSizeDynamicsChange(Sender: TObject);
begin
  case cmbbxSizeDynamics.ItemIndex of
    0:
      begin
        FSizeDynamicsState := bdsOff;
      end;
      
    1:
      begin
        FSizeDynamicsState := bdsFade;
      end;
  end;

  UpdateSizeStepsEditStatus;
end;

procedure TfrmBrushDynamics.cmbbxOpacityDynamicsChange(Sender: TObject);
begin
  case cmbbxOpacityDynamics.ItemIndex of
    0:
      begin
        FOpacityDynamicsState := bdsOff;
      end;

    1:
      begin
        FOpacityDynamicsState := bdsFade;
      end;
  end;
  
  UpdateOpacityStepsEditStatus;
end; 

procedure TfrmBrushDynamics.cmbbxColorDynamicsChange(Sender: TObject);
begin
  case cmbbxColorDynamics.ItemIndex of
    0:
      begin
        FColorDynamicsState := bdsOff;
      end;

    1:
      begin
        FColorDynamicsState := bdsFade;
      end;
  end;
  
  UpdateColorStepsEditStatus;
end;

procedure TfrmBrushDynamics.edtSizeStepsChange(Sender: TObject);
begin
  try
    if edtSizeSteps.Text <> '' then
    begin
      FSizeSteps := StrToInt(edtSizeSteps.Text);
      EnsureValueInRange(FSizeSteps, 1, 9999);
      edtSizeSteps.Text := IntToStr(FSizeSteps);
    end;
  except
    edtSizeSteps.Text := IntToStr(FSizeSteps);
  end;
end;

procedure TfrmBrushDynamics.edtOpacityStepsChange(Sender: TObject);
begin
  try
    if edtOpacitySteps.Text <> '' then
    begin
      FOpacitySteps := StrToInt(edtOpacitySteps.Text);
      EnsureValueInRange(FOpacitySteps, 1, 9999);
      edtOpacitySteps.Text := IntToStr(FOpacitySteps);
    end;
  except
    edtOpacitySteps.Text := IntToStr(FOpacitySteps);
  end;
end;

procedure TfrmBrushDynamics.edtColorStepsChange(Sender: TObject);
begin
  try
    if edtColorSteps.Text <> '' then
    begin
      FColorSteps := StrToInt(edtColorSteps.Text);
      EnsureValueInRange(FColorSteps, 1, 9999);
      edtColorSteps.Text := IntToStr(FColorSteps);
    end;
  except
    edtColorSteps.Text := IntToStr(FColorSteps);
  end;
end; 

end.
