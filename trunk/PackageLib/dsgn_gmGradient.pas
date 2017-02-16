unit dsgn_gmGradient;

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
 * x2nie - Fathony Luthfillah  <x2nie@yahoo.com>
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

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLClasses, LCLType, LResources, RtlConsts, Forms,
  ComCtrls, Menus, ToolWin, Registry, ImgList, Clipbrd, Graphics, Controls,
  ExtCtrls, StdCtrls, Buttons, LazIDEIntf, PropEdits, ComponentEditors,
  Dialogs, FormEditingIntf,
{$ELSE}
  Windows, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ExtDlgs,
  ComCtrls, Menus, ToolWin, Registry, ImgList, Clipbrd,
  Consts,
  DesignIntf, DesignEditors, DesignWindows, VCLEditors,
{$ENDIF}
{ Standard }
  SysUtils, Classes, AppEvnts, ActnList,
{ Graphics32 }
  GR32, GR32_Image, GR32_Layers, GR32_Filters,
{ GraphicsMagic }
  gmGradient,
  gmGradient_FileDlgs,
  gmGridBased_List,
  gmGradient_List, gmGridBased_FileDlg;

type
  TGradientEditorForm = class(TForm)
    dlgSave1: TSaveDialog;
    dlgColor1: TColorDialog;
    pnl1: TPanel;
    lstNames: TListBox;
    spl1: TSplitter;
    pnl2: TPanel;
    pnl3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    grp1: TGroupBox;
    chkReplace: TCheckBox;
    btnImport: TButton;
    grp2: TGroupBox;
    btnExport: TButton;
    chkSelected: TCheckBox;
    lbl6: TLabel;
    shpBg: TShape;
    shpFg: TShape;
    Bevel1: TBevel;
    imgGradientPreview: TImage32;
    il1: TImageList;
    acts1: TActionList;
    actAddCmd: TAction;
    actDeleteCmd: TAction;
    actMoveUpCmd: TAction;
    actMoveDownCmd: TAction;
    actSelectAllCmd: TAction;
    pm1: TPopupMenu;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N2: TMenuItem;
    tlb1: TToolBar;
    tibtnAdd: TToolButton;
    tlbtnDelete: TToolButton;
    ToolButton3: TToolButton;
    tlbtnMoveUp: TToolButton;
    tlbtnMoveDown: TToolButton;
    ape1: TApplicationEvents;
    lbl1: TLabel;
    lbl2: TLabel;
    actSaveToFile: TAction;
    actLoadFromFile: TAction;
    dlgOpen1: TOpenGradientDialog;
    procedure btnNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure lstNamesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgGradientPreviewPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure btnExportClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure shpFgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SelectedUpdate(Sender: TObject);
    procedure actSelectAllCmdUpdate(Sender: TObject);
    procedure actMoveUpCmdExecute(Sender: TObject);
    procedure actMoveDownCmdExecute(Sender: TObject);
    procedure actSelectAllCmdExecute(Sender: TObject);
    procedure ape1ShowHint(var HintStr: String; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure ape1Hint(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
    FCurrentAppHintPause : Integer;
    FSelected            : TList;
    FCollection          : TgmGradientCollection;

    procedure UpdateView;
    procedure SetSelection;
    procedure PopSelected;
    procedure PushSelected;
  public
    { Public declarations }

    property Collection : TgmGradientCollection read FCollection write FCollection;
  end;

  TGradientsEditor = class(TComponent)
  private
    FGradientEditorDlg : TGradientEditorForm;
    FGradients         : TgmGradientCollection;

    procedure SetGradients(const AValue: TgmGradientCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;

    property Gradients : TgmGradientCollection read FGradients write SetGradients;
  end;

  TGradientsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    procedure SetValue(const AValue: string); override;

    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TGradientListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGradientIndexProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetGradientListAt(Index: Integer): TgmGradientList;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);

    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
      
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

var
  GradientEditorForm: TGradientEditorForm;

procedure Register;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic }
  gmMiscFuncs;

{$R *.dfm}

{ Registration }
procedure Register;
begin
  RegisterComponentEditor(TgmGradientList, TGradientListEditor);
  RegisterPropertyEditor(TypeInfo(TgmGradientCollection), TgmGradientList, 'Gradients', TGradientsProperty);

  //RegisterPropertyEditor(TypeInfo(TgmGradientIndex),TgmGradientPreview , 'GradientIndex', TGradientIndexProperty);
  //RegisterPropertyEditor(TypeInfo(TgmGradientIndex),TgmGradientEditor , 'GradientIndex', TGradientIndexProperty);
  //RegisterPropertyEditor(TypeInfo(TgmGradientIndex),TgmGradientsGrid , 'GradientIndex', TGradientIndexProperty);
end;

{ TGradientsEditor }

constructor TGradientsEditor.Create(AOwner: TComponent);
begin
  inherited;

  FGradients         := TgmGradientCollection.Create(Self);
  FGradientEditorDlg := TGradientEditorForm.Create(Self);
end;

destructor TGradientsEditor.Destroy;
begin
  FGradientEditorDlg.Free;
  FGradients.Free;

  inherited;
end;

function TGradientsEditor.Execute: Boolean;
begin
  FGradientEditorDlg.Collection := FGradients;
  Result := (FGradientEditorDlg.ShowModal = mrOK);
end;

procedure TGradientsEditor.SetGradients(const AValue: TgmGradientCollection);
begin
  try
    FGradients.Assign(AValue);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{ TGradientsProperty }

procedure TGradientsProperty.Edit;
var
  LGradientsEditor: TGradientsEditor;
begin
  try
    LGradientsEditor := TGradientsEditor.Create(nil);
    try
      LGradientsEditor.Gradients := TgmGradientCollection(Pointer(GetOrdValue));

      if LGradientsEditor.Execute then
      begin
        SetOrdValue(Longint(LGradientsEditor.Gradients));
        {$IFNDEF FPC} Designer.Modified; {$ENDIF}
      end;
    finally
      LGradientsEditor.Free;
    end;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TGradientsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

function TGradientsProperty.GetValue: string;
var
  LGrads : TgmGradientCollection;
begin
  try
    LGrads := TgmGradientCollection(GetOrdValue);

    if (LGrads = nil) {or Grads.Empty} then
      Result := srNone
    else
      Result := Format('%d Items', [LGrads.Count]);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TGradientsProperty.SetValue(const AValue: string);
begin
  if AValue = '' then
    SetOrdValue(0);
end;

{ TGradientListEditor }

procedure TGradientListEditor.ExecuteVerb(AIndex: Integer);
var
  LGradientsEditor : TGradientsEditor;
  LGradientList    : TgmGradientList;
begin
  LGradientList := Component as TgmGradientList;
  
  if AIndex = 0 then
  begin
    LGradientsEditor := TGradientsEditor.Create(nil);
    try
      LGradientsEditor.Gradients := LGradientList.Gradients;
      
      if LGradientsEditor.Execute then
      begin
        LGradientList.Gradients := LGradientsEditor.Gradients;
        Designer.Modified;
      end;
      
    finally
      LGradientsEditor.Free;
    end;
  end;
end;

function TGradientListEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Gradients Editor...';
end;

function TGradientListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TGradientEditorForm }

procedure TGradientEditorForm.FormCreate(Sender: TObject);
begin
  with imgGradientPreview.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;
  
  FSelected := TList.Create;
end;

procedure TGradientEditorForm.FormShow(Sender: TObject);
begin
  Self.shpFg.Brush.Color := FCollection.ForegroundColor;
  Self.shpBg.Brush.Color := FCollection.BackgroundColor;
  FCurrentAppHintPause   := Application.HintPause;
  Application.HintPause  := 1;
  UpdateView;
end;

procedure TGradientEditorForm.FormHide(Sender: TObject);
begin
  Application.HintPause := FCurrentAppHintPause;
end;

procedure TGradientEditorForm.UpdateView;
var
  i : Integer;
begin
  lstNames.Items.BeginUpdate;
  lstNames.Clear;

  for i := 0 to FCollection.Count - 1 do
  begin
    lstNames.Items.Add(FCollection[i].DisplayName);
  end;

  lstNames.Items.EndUpdate;
end;

procedure TGradientEditorForm.SetSelection;
var
  i : Integer;
begin
  for i := (FCollection.Count - 1) downto 0 do
  begin
    if TgmGradientItem(FCollection.Items[i]).Tag = 1 then
    begin
      lstNames.Selected[i] := True;
    end;
  end;
end;

procedure TGradientEditorForm.PopSelected;
begin

end;

procedure TGradientEditorForm.PushSelected;
begin
  FSelected.Clear;
end;

procedure TGradientEditorForm.btnNewClick(Sender: TObject);
var
  LItem : TgmGradientItem;
  LNew  : string;
begin
  LNew              := InputBox('New gradient', 'Name:', 'Custom');
  LItem             := FCollection.Add;
  LItem.DisplayName := LNew;
  UpdateView;
end;

procedure TGradientEditorForm.btnDeleteClick(Sender: TObject);
var
  i : integer;
begin
  for i := (FCollection.Count - 1) downto 0 do
  begin
    if lstNames.Selected[i] then
      FCollection.Delete(i);
  end;

  UpdateView;
end;

procedure TGradientEditorForm.actMoveUpCmdExecute(Sender: TObject);
var
  i, LInsPos : Integer;
begin
  if (lstNames.SelCount = 0) or
     (lstNames.SelCount = FCollection.Count) then
  begin
    Exit;
  end;

  LInsPos := 0;
  while not lstNames.Selected[LInsPos] do
  begin
    Inc(LInsPos);
  end;

  if LInsPos > 0 then
  begin
    Dec(LInsPos);
  end;

  FCollection.BeginUpdate;
  try
    for i := 0 to (lstNames.Items.Count - 1) do
    begin
      if lstNames.Selected[i] then
      begin
        TgmGradientItem(FCollection.Items[i]).Tag := 1;
        FCollection.Items[i].Index                := LInsPos;
        Inc(LInsPos);
      end
      else
      begin
        TgmGradientItem(FCollection.Items[i]).Tag := 0;
      end;
    end;

  finally
    FCollection.EndUpdate;
  end;
  
  UpdateView;
  SetSelection;
end;

procedure TGradientEditorForm.actMoveDownCmdExecute(Sender: TObject);
var
  i, LInsPos: Integer;
begin
  if (lstNames.SelCount = 0) or
     (lstNames.SelCount = FCollection.Count) then
  begin
    Exit;
  end;

  LInsPos := lstNames.Items.Count - 1;

  while not lstNames.Selected[LInsPos] do
  begin
    Dec(LInsPos);
  end;

  if LInsPos < (lstNames.Items.Count - 1) then
  begin
    Inc(LInsPos);
  end;

  FCollection.BeginUpdate;
  try
    for i := (lstNames.Items.Count - 1) downto 0 do
    begin
      if lstNames.Selected[i] then
      begin
        TgmGradientItem(FCollection.Items[i]).Tag := 1;
        FCollection.Items[i].Index                := LInsPos;
        Dec(LInsPos);
      end
      else
      begin
        TgmGradientItem(FCollection.Items[i]).Tag := 0;
      end;
    end;
  finally
    FCollection.EndUpdate;
  end;

  UpdateView;
  SetSelection;
end;

procedure TGradientEditorForm.actSelectAllCmdExecute(Sender: TObject);
begin
  lstNames.SelectAll();
end;

procedure TGradientEditorForm.lstNamesClick(Sender: TObject);
var
  LItem : TgmGradientItem;
  cc    : TArrayOfColor32;
  i     : Integer;
begin
  if lstNames.ItemIndex <> -1 then
  begin
    LItem := FCollection.Items[lstNames.ItemIndex];
    LItem.GradientLength := imgGradientPreview.Width;
    LItem.RefreshColorArray;
    cc := LItem.OutputColors;

    imgGradientPreview.Bitmap.SetSize(imgGradientPreview.Width, 1);
    for i := 0 to Length(cc)-1 do
    begin
      imgGradientPreview.Bitmap.Pixel[i, 0] := cc[i];
    end;

    imgGradientPreview.Invalidate;
  end;
end;

procedure TGradientEditorForm.ape1ShowHint(var HintStr: String;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  lbl1.Caption := HintStr;
  CanShow      := False;
end;

procedure TGradientEditorForm.ape1Hint(Sender: TObject);
begin
  lbl2.Caption := Application.Hint;
end;

procedure TGradientEditorForm.shpFgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor1.Color := TShape(Sender).Brush.Color;

  if dlgColor1.Execute then
  begin
    if TShape(Sender).Tag = 1 then
      FCollection.ForegroundColor := dlgColor1.Color
    else
      FCollection.BackgroundColor := dlgColor1.Color;

    TShape(Sender).Brush.Color := dlgColor1.Color;
  end;

  if lstNames.ItemIndex < 0 then
  begin
    Exit;
  end;
  
  if (FCollection.Count > 0) and
     FCollection[lstNames.ItemIndex].IsSpecialColorUsed then
  begin
    lstNamesClick(lstNames);
  end;
end;

procedure TGradientEditorForm.SelectedUpdate(Sender: TObject);
var
  LEnabled: Boolean;
begin
  LEnabled := lstNames.SelCount > 0;
  (Sender as TAction).Enabled := LEnabled;
end;

procedure TGradientEditorForm.actSelectAllCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := lstNames.Items.Count > 0;
end;

procedure TGradientEditorForm.imgGradientPreviewPaintStage(Sender: TObject;
  Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawCheckerboard(Buffer);
end;

procedure TGradientEditorForm.btnImportClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    if chkReplace.Checked then
    begin
      FCollection.Clear;
    end;

    FCollection.LoadFromFile(dlgOpen1.FileName);
  end;
  
  UpdateView;
end;

procedure TGradientEditorForm.btnExportClick(Sender: TObject);
var
  i          : Integer;
  LSelection : TgmGradientCollection;
begin
  if dlgSave1.Execute then
  begin
    if not chkSelected.Checked then  //select all:
    begin
      FCollection.SaveToFile(dlgSave1.FileName);
    end
    else
    begin
      if lstNames.SelCount <=0 then
      begin
        raise Exception.Create('No selection found, please select item');
      end
      else
      begin //create temporary, then export..
        LSelection := TgmGradientCollection.Create(nil);
        try
          for i := 0 to (lstNames.Count -1) do
          begin
            if lstNames.Selected[i] then
            begin
              with LSelection.Add do
                Assign(FCollection.Items[i]);
            end;
          end;

          LSelection.SaveToFile(dlgSave1.FileName);
        finally
          LSelection.Free;
        end;
      end;
    end;  
  end;
end;

{ TGradientIndexProperty }

function TGradientIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, {paSortList,} paRevertable];
end;

procedure TGradientIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  LRight         : Integer;
  LGradientIndex : Integer;
  LCursorHandle  : THandle;
  LGradientList  : TgmGradientList;
  LRect          : TRect;
begin
  LGradientList := Self.GetGradientListAt(0);
  LRight        := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;

  with ACanvas do
  begin
    Lock;
    LRect := Rect(ARect.TopLeft, Point(LRight, ARect.Bottom));
    InflateRect(LRect, -2, -2);

    LGradientIndex := StrToInt(Value);
    ACanvas.FillRect(ARect);

    if Assigned(LGradientList) then
    begin
      LGradientList.Gradients.Draw(LGradientIndex, ACanvas, LRect);
    end;

    DefaultPropertyListDrawValue(Value, ACanvas, Rect(LRight, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
      
    Unlock;
  end;
end;

procedure TGradientIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

procedure TGradientIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight('Wg'), GetSystemMetrics(SM_CYCURSOR) + 4);
end;

function TGradientIndexProperty.GetGradientListAt(
  Index: Integer): TgmGradientList;
var
  C             : TPersistent;
  IGradientList : IgmGradientListSupport;
begin
  Result := nil;
  { ? I'm guessing that the Index parameter is a component index (one that
    would be passed to the GetComponent function). }
  C := GetComponent(Index);

  if Supports(C, IgmGradientListSupport, IGradientList) then
  begin
    Result := IGradientList.GetGradients;
  end;
end;

procedure TGradientIndexProperty.GetValues(Proc: TGetStrProc);
var
  LGradientList : TgmGradientList;
  i             : Integer;
begin
  LGradientList := GetGradientListAt(0);

  if Assigned(LGradientList) then
    for i := 0 to (LGradientList.Count - 1) do
      Proc(IntToStr(i));
end;

end.
