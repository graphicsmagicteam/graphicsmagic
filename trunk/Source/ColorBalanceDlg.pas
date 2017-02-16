unit ColorBalanceDlg;

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

// Update Date: 2016/04/20

interface

uses
{ Standard }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls,
{ Graphics32 Lib }
  GR32_RangeBars,
{ GraphicsMagic Lib }
  gmColorBalance,
  gmColorBalanceLayer;

type
  TfrmColorBalance = class(TForm)
    grpbxColorBalance: TGroupBox;
    lblColorLevels: TLabel;
    edtCyanToRed: TEdit;
    edtMagentaToGreen: TEdit;
    edtYellowToBlue: TEdit;
    lblCyan: TLabel;
    lblRed: TLabel;
    lblMagenta: TLabel;
    lblYellow: TLabel;
    lblGreen: TLabel;
    lblBlue: TLabel;
    grpbxToneBalance: TGroupBox;
    rdbtnShadows: TRadioButton;
    rdbtnMidtones: TRadioButton;
    rdbtnHighlights: TRadioButton;
    chckbxPreserveLuminosity: TCheckBox;
    btbtnOK: TBitBtn;
    btbtnCancel: TBitBtn;
    btnReset: TButton;
    chckbxPreview: TCheckBox;
    ggbrYellowToBlue: TGaugeBar;
    ggbrCyanToRed: TGaugeBar;
    ggbrMagentaToGreen: TGaugeBar;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdjustColorBalance(Sender: TObject);
    procedure chckbxPreviewClick(Sender: TObject);
    procedure edtCyanToRedChange(Sender: TObject);
    procedure edtMagentaToGreenChange(Sender: TObject);
    procedure edtYellowToBlueChange(Sender: TObject);
    procedure ChangeTransferMode(Sender: TObject);
    procedure chckbxPreserveLuminosityClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btbtnOKClick(Sender: TObject);
    procedure ColorBalanceBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FColorBalance         : TgmColorBalance;       // used for menu command
    FColorBalanceLayer    : TgmColorBalanceLayer;  // pointer to a Color Balance layer 
    FTransferMode         : TgmTransferMode;
    FPreserveLuminosity   : Boolean;
    FBarIsChanging        : Boolean;
    FEditIsChanging       : Boolean;
    FAllowChange          : Boolean;
    FWorkingOnEffectLayer : Boolean;
    
    procedure ExecuteColorBalance;
  public
    procedure AssociateToColorBalanceLayer(ALayer: TgmColorBalanceLayer);
  end;

var
  frmColorBalance: TfrmColorBalance;

implementation

uses
{ GraphicsMagic Lib }
  gmChannelManager,
  gmIni,
  gmLayers,
  gmMath,               // EnsureValueInRange()
{ GraphicsMagic Forma/Dialogs }
  MainForm;             // global variable ActiveChildForm is defined in this unit


{$R *.DFM}

const
  MEDIAN_VALUE : Integer = 100;
  MIN_VALUE    : Integer = -100;
  MAX_VALUE    : Integer = 100;

{ Custom procedures and functions }

procedure TfrmColorBalance.AssociateToColorBalanceLayer(
  ALayer: TgmColorBalanceLayer);
begin
  if Assigned(ALayer) then
  begin
    FColorBalanceLayer := ALayer;

    with FColorBalanceLayer do
    begin
      ggbrCyanToRed.Position           := MEDIAN_VALUE + ColorBalance.CyanRed;
      ggbrMagentaToGreen.Position      := MEDIAN_VALUE + ColorBalance.MagentaGreen;
      ggbrYellowToBlue.Position        := MEDIAN_VALUE + ColorBalance.YellowBlue;
      chckbxPreserveLuminosity.Checked := ColorBalance.PreserveLuminosity;

      rdbtnShadows.Checked    := (ColorBalance.TransferMode = tmShadows);
      rdbtnMidtones.Checked   := (ColorBalance.TransferMode = tmMidtones);
      rdbtnHighlights.Checked := (ColorBalance.TransferMode = tmHighlights);
    end;

    chckbxPreview.Checked := True;
    FWorkingOnEffectLayer := True;
  end;
end;

procedure TfrmColorBalance.ExecuteColorBalance;
begin
  with ActiveChildForm do
  begin
    if LayerList.SelectedLayer is TgmNormalLayer then
    begin
      if Assigned(FColorBalance) then
      begin
        FColorBalance.Execute(frmMain.FBitmapAfter);
      end;
    end;

    if chckbxPreview.Checked then
    begin
      if LayerList.SelectedLayer is TgmNormalLayer then
      begin
        if Assigned(Selection) then
        begin
          Selection.CutOriginal.Assign(frmMain.FBitmapAfter);
          ShowProcessedSelection();
        end
        else
        begin
          LayerList.SelectedLayer.LayerBitmap.Assign(frmMain.FBitmapAfter);
          LayerList.SelectedLayer.Changed();
        end;
      end
      else if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
      begin
        FColorBalanceLayer.Changed;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmColorBalance.FormCreate(Sender: TObject);
begin
  FBarIsChanging        := False;
  FEditIsChanging       := False;
  FAllowChange          := True;
  FPreserveLuminosity   := True;
  FWorkingOnEffectLayer := False;
  FTransferMode         := tmMidtones;
  FColorBalance         := nil;
  FColorBalanceLayer    := nil;
end;

procedure TfrmColorBalance.FormDestroy(Sender: TObject);
begin
  FColorBalance.Free;
end;

procedure TfrmColorBalance.FormShow(Sender: TObject);
begin
  if not Assigned(ActiveChildForm) then
  begin
    Exit;
  end;

  // if not working on special layer ...
  if not FWorkingOnEffectLayer then
  begin
    chckbxPreview.Checked := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PREVIEW, '1')));

    with ActiveChildForm do
    begin
      if Assigned(Selection) then
      begin
        frmMain.FBitmapBefore.Assign(Selection.CutOriginal);
      end
      else
      begin
        if (LayerList.SelectedLayer is TgmNormalLayer) and
           (ChannelManager.CurrentChannelType = ctColorChannel) then
        begin
          frmMain.FBitmapBefore.Assign(LayerList.SelectedLayer.LayerBitmap);
        end;
      end;
    end;

    frmMain.FBitmapAfter.Assign(frmMain.FBitmapBefore);

    // create Color Balance for manu command
    FColorBalance := TgmColorBalance.Create(frmMain.FBitmapBefore);
    FColorBalance.PreserveLuminosity := Boolean(StrToInt(ReadInfoFromIniFile(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PRESERVE_LUMINOSITY, '1')));
    
    ggbrCyanToRed.Position           := MEDIAN_VALUE;
    ggbrMagentaToGreen.Position      := MEDIAN_VALUE;
    ggbrYellowToBlue.Position        := MEDIAN_VALUE;
    chckbxPreserveLuminosity.Checked := FColorBalance.PreserveLuminosity;
  end;
  
  ActiveControl := btbtnOK;
end;

procedure TfrmColorBalance.AdjustColorBalance(Sender: TObject);
var
  LChangedAmount : Integer;
begin
  FBarIsChanging := True;
  try
    if Sender = ggbrCyanToRed then
    begin
      LChangedAmount := ggbrCyanToRed.Position - MEDIAN_VALUE;

      if FEditIsChanging then
      begin
        edtCyanToRed.Text := IntToStr(LChangedAmount);
      end
      else
      begin
        if LChangedAmount > 0 then
        begin
          edtCyanToRed.Text := '+' + IntToStr(LChangedAmount);
        end
        else
        begin
          edtCyanToRed.Text := IntToStr(LChangedAmount);
        end;
      end;

      // if on Color Balance layer ...
      if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
      begin
        FColorBalanceLayer.ColorBalance.CyanRed := LChangedAmount;
      end
      else if Assigned(FColorBalance) then
      begin
        FColorBalance.CyanRed := LChangedAmount;
      end;
    end
    else
    if Sender = ggbrMagentaToGreen then
    begin
      LChangedAmount := ggbrMagentaToGreen.Position - MEDIAN_VALUE;

      if FEditIsChanging then
      begin
        edtMagentaToGreen.Text := IntToStr(LChangedAmount);
      end
      else
      begin
        if LChangedAmount > 0 then
        begin
          edtMagentaToGreen.Text := '+' + IntToStr(LChangedAmount);
        end
        else
        begin
          edtMagentaToGreen.Text := IntToStr(LChangedAmount);
        end;
      end;

      // if on Color Balance layer ...
      if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
      begin
        FColorBalanceLayer.ColorBalance.MagentaGreen := LChangedAmount;
      end
      else if Assigned(FColorBalance) then
      begin
        FColorBalance.MagentaGreen := LChangedAmount;
      end;
    end
    else
    if Sender = ggbrYellowToBlue then
    begin
      LChangedAmount := ggbrYellowToBlue.Position - MEDIAN_VALUE;

      if FEditIsChanging then
      begin
        edtYellowToBlue.Text := IntToStr(LChangedAmount);
      end
      else
      begin
        if LChangedAmount > 0 then
        begin
          edtYellowToBlue.Text := '+' + IntToStr(LChangedAmount);
        end
        else
        begin
          edtYellowToBlue.Text := IntToStr(LChangedAmount);
        end;
      end;

      // if on Color Balance layer ...
      if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
      begin
        FColorBalanceLayer.ColorBalance.YellowBlue := LChangedAmount;
      end
      else if Assigned(FColorBalance) then
      begin
        FColorBalance.YellowBlue := LChangedAmount;
      end;
    end;
    
  finally
    FBarIsChanging := False;
  end;
end;

procedure TfrmColorBalance.ColorBalanceBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ExecuteColorBalance;
end;

procedure TfrmColorBalance.chckbxPreviewClick(Sender: TObject);
begin
  ExecuteColorBalance;
end;

procedure TfrmColorBalance.edtCyanToRedChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  LRightValue := 0;
  
  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtCyanToRed.Text);
        EnsureValueInRange(LChangedValue, MIN_VALUE, MAX_VALUE);
        ggbrCyanToRed.Position := MEDIAN_VALUE + LChangedValue;
        ExecuteColorBalance;
      except
        if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FColorBalanceLayer.ColorBalance.CyanRed;
        end
        else if Assigned(FColorBalance) then
        begin
          LRightValue := FColorBalance.CyanRed;
        end;

        edtCyanToRed.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end; 

procedure TfrmColorBalance.edtMagentaToGreenChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  LRightValue := 0;

  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtMagentaToGreen.Text);
        EnsureValueInRange(LChangedValue, MIN_VALUE, MAX_VALUE);
        ggbrMagentaToGreen.Position := MEDIAN_VALUE + LChangedValue;
        ExecuteColorBalance;
      except
        if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FColorBalanceLayer.ColorBalance.MagentaGreen;
        end
        else if Assigned(FColorBalance) then
        begin
          LRightValue := FColorBalance.MagentaGreen;
        end;

        edtMagentaToGreen.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end; 

procedure TfrmColorBalance.edtYellowToBlueChange(Sender: TObject);
var
  LChangedValue : Integer;
  LRightValue   : Integer;
begin
  LRightValue := 0;

  if not FBarIsChanging then
  begin
    FEditIsChanging := True;
    try

      try
        LChangedValue := StrToInt(edtYellowToBlue.Text);
        EnsureValueInRange(LChangedValue, MIN_VALUE, MAX_VALUE);
        ggbrYellowToBlue.Position := MEDIAN_VALUE + LChangedValue;
        ExecuteColorBalance;
      except
        if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
        begin
          LRightValue := FColorBalanceLayer.ColorBalance.YellowBlue;
        end
        else if Assigned(FColorBalance) then
        begin
          LRightValue := FColorBalance.YellowBlue;
        end;

        edtYellowToBlue.Text := IntToStr(LRightValue);
      end;

    finally
      FEditIsChanging := False;
    end;
  end;
end; 

procedure TfrmColorBalance.ChangeTransferMode(Sender: TObject);
var
  LTransferMode : TgmTransferMode;
begin
  LTransferMode := tmMidtones;
  
  if Sender = rdbtnShadows then
  begin
    LTransferMode := tmShadows;
  end
  else if Sender = rdbtnHighlights then
  begin
    LTransferMode := tmHighlights;
  end;

  // if on Color Balance layer ...
  if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
  begin
    with FColorBalanceLayer do
    begin
      ColorBalance.TransferMode   := LTransferMode;
      ggbrCyanToRed.Position      := ColorBalance.CyanRed      + MEDIAN_VALUE;
      ggbrMagentaToGreen.Position := ColorBalance.MagentaGreen + MEDIAN_VALUE;
      ggbrYellowToBlue.Position   := ColorBalance.YellowBlue   + MEDIAN_VALUE;
    end;
  end
  else if Assigned(FColorBalance) then
  begin
    FColorBalance.TransferMode  := LTransferMode;
    ggbrCyanToRed.Position      := FColorBalance.CyanRed      + MEDIAN_VALUE;
    ggbrMagentaToGreen.Position := FColorBalance.MagentaGreen + MEDIAN_VALUE;
    ggbrYellowToBlue.Position   := FColorBalance.YellowBlue   + MEDIAN_VALUE;
  end;
end; 

procedure TfrmColorBalance.chckbxPreserveLuminosityClick(Sender: TObject);
begin
  // if on Color Balance layer ...
  if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
  begin
    with FColorBalanceLayer do
    begin
      ColorBalance.PreserveLuminosity := chckbxPreserveLuminosity.Checked;
    end;
  end
  else if Assigned(FColorBalance) then
  begin
    FColorBalance.PreserveLuminosity := chckbxPreserveLuminosity.Checked;
  end;
  
  ExecuteColorBalance;
end; 

procedure TfrmColorBalance.btnResetClick(Sender: TObject);
begin
  // if on Color Balance layer ...
  if Assigned(FColorBalanceLayer) and FWorkingOnEffectLayer then
  begin
    FColorBalanceLayer.ColorBalance.Reset;
  end
  else if Assigned(FColorBalance) then
  begin
    FColorBalance.Reset;
  end;

  ExecuteColorBalance;

  ggbrCyanToRed.Position      := MEDIAN_VALUE;
  ggbrMagentaToGreen.Position := MEDIAN_VALUE;
  ggbrYellowToBlue.Position   := MEDIAN_VALUE;
end;

procedure TfrmColorBalance.btbtnOKClick(Sender: TObject);
begin
  if not FWorkingOnEffectLayer then
  begin
    WriteInfoToIniFile(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PREVIEW, IntToStr(Integer(chckbxPreview.Checked)));
    WriteInfoToIniFile(SECTION_COLOR_BALANCE_DIALOG, IDENT_COLOR_BALANCE_PRESERVE_LUMINOSITY, IntToStr(Integer(FPreserveLuminosity)));
  end;
end;


end.
