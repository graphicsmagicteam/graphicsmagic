////////////////////////////////////////////////////////////////////////////////
// File:       PegtopFileDialogs.pas
// Classes:    TPegtopCustomFileDialog, TPegtopOpenDialog, TPegtopSaveDialog,
//             TPegtopCustomExtendedFileDialog, TPegtopExtendedOpenDialog,
//             TPegtopExtendedSaveDialog
// Version:    1.02
// Date:       31 Oct 2004 1.00
//             29 Mar 2005 1.01 (access to dialog items added)
//             15 Aug 2005 1.02 (access to visibility of dialog items added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopOpenDialog and TPegtopSaveDialog have methods to access the most
// important dialog items (such as buttons and labels) directly, e.g. you can
// change the caption text dynamically.
// TPegtopSaveDialog introduces a new property AutoAdjustExtension. If set to
// True, the file extension will automatically be adjusted if another filter
// is selected.
// TPegtopExtendedOpenDialog and TPegtopExtendedSaveDialog extend the existing
// file dialogs by a container (either at the right side or at the bottom of
// the dialog), which can be sized as desired and be used to add user defined
// controls. No additional template is needed because the existing file dialog
// is modified dynamically.
////////////////////////////////////////////////////////////////////////////////
// Usage:
// You have to add extended controls by code, setting their parent property to
// ExtendedContainer (the most simple way to do this is to create a frame for
// your controls). This container is not sized until the dialog is shown, so
// either don't rely on the container's size or use the OnAlign event to locate
// and size your controls. OnAlign is also called whenever the dialog is resized
// by the user.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopFileDialogs;

interface

Uses
  Messages, Windows, SysUtils, Classes, Controls, Dialogs;

type
  TPegtopFileDialogItem = (pfiFolderLabel, pfiFolderComboBox, pfiFileListBox, pfiFileNameLabel, pfiFileNameEdit, pfiFileTypeLabel, pfiFileTypeComboBox, pfiOkButton, pfiCancelButton);
  TPegtopFileDialogItemInfo = record
    ID: Integer;
    Handle: THandle;
    Found: Boolean;
  end;
  TPegtopFileDialogItemInfos = array[TPegtopFileDialogItem] of TPegtopFileDialogItemInfo;
  PPegtopFileDialogItemInfos = ^TPegtopFileDialogItemInfos;

  TPegtopFileDialogViewStyle = (pvsDefault, pvsLargeIcon, pvsSmallIcon, pvsList, pvsReport, pvsTile, pvmThumbnail);

  TPegtopCustomFileDialog = class(TOpenDialog)
  private
    FVisible: Boolean;
    FItemInfos: TPegtopFileDialogItemInfos;
    FAutoAdjustExtension: Boolean;
    FViewStyle: TPegtopFileDialogViewStyle;
    FChangeViewStyle: Boolean;
    procedure AdjustViewStyle;
    function GetItemHandle(Item: TPegtopFileDialogItem): THandle;
    function GetItemRect(Item: TPegtopFileDialogItem): TRect;
    function GetItemText(Item: TPegtopFileDialogItem): String;
    procedure SetItemText(Item: TPegtopFileDialogItem; Value: String);
    function GetItemVisibility(Item: TPegtopFileDialogItem): Boolean;
    procedure SetItemVisibility(Item: TPegtopFileDialogItem; Value: Boolean);
    procedure SetViewStyle(Value: TPegtopFileDialogViewStyle);
  protected
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoTypeChange; override;
    procedure DoFolderChange; override;
    function IsSaveDialog: Boolean; virtual;
    property AutoAdjustExtension: Boolean read FAutoAdjustExtension write FAutoAdjustExtension default False;
    property ViewStyle: TPegtopFileDialogViewStyle read FViewStyle write SetViewStyle default pvsDefault;
  public
    function Execute: Boolean; override;
    procedure AdjustExtension;
    procedure Close;
    property Visible: Boolean read FVisible;
    property ItemHandles[Item: TPegtopFileDialogItem]: THandle read GetItemHandle;
    property ItemRects[Item: TPegtopFileDialogItem]: TRect read GetItemRect;
    property ItemTexts[Item: TPegtopFileDialogItem]: String read GetItemText write SetItemText;
    property ItemVisibilities[Item: TPegtopFileDialogItem]: Boolean read GetItemVisibility write SetItemVisibility;
  end;

  TPegtopOpenDialog = class(TPegtopCustomFileDialog)
  published
    property ViewStyle;
  end;

  TPegtopSaveDialog = class(TPegtopOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

  TPegtopExtendedDialogAlignment = (peaBottom, peaRight);

  TPegtopCustomExtendedFileDialog = class(TPegtopCustomFileDialog)
  private
    FAlignEnabled: Boolean;
    FOldDialogWndProc: Pointer;
    FDialogHandle: THandle;
    FDialogMethodInstance: Pointer;
    FExtendedContainer: TWinControl;
    FExtendedSize: Integer;
    FExtendedAlignment: TPegtopExtendedDialogAlignment;
    FDialogRect: TRect;
    FClientRect: TRect;
    FOnResize: TNotifyEvent;
    FOnAlign: TNotifyEvent;
    procedure DialogWndProc(var Msg: TMessage);
  protected
    function MessageHook(var Msg: TMessage): Boolean; override;
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoAlign; virtual;
    procedure DoResize; virtual;
    function CreateExtendedContainer: TWinControl; virtual;
    property ExtendedContainer: TWinControl read FExtendedContainer;
    property ExtendedSize: Integer read FExtendedSize write FExtendedSize default 24;
    property ExtendedAlignment: TPegtopExtendedDialogAlignment read FExtendedAlignment write FExtendedAlignment default peaBottom;
    property OnAlign: TNotifyEvent read FOnAlign write FOnAlign;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  public
    constructor Create (AOwner: TComponent); override;
    function Execute: Boolean; override;
  end;

  TPegtopExtendedOpenDialog = class(TPegtopCustomExtendedFileDialog)
  public
    property ExtendedContainer;
  published
    property ExtendedSize;
    property ExtendedAlignment;
    property ViewStyle;
    property OnResize;
    property OnAlign;
  end;

  TPegtopExtendedSaveDialog = class(TPegtopExtendedOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

implementation

uses
  Forms, CommDlg, ExtCtrls, Dlgs, ComCtrls;

const
  WM_PEGTOP_ALIGN_FILE_DIALOG = WM_USER + $AFD;

  OD_LARGEICON    = $7029; // 28713
  OD_SMALLICON    = $702A; // 28714
  OD_LIST         = $702B; // 28715  Default
  OD_REPORT       = $702C; // 28716
  OD_SHVIEW_TILE  = $702E; // 28718
  OD_THUMBNAIL    = $702D; // 28717  XP

  ViewStyleIDs: array[TPegtopFileDialogViewStyle] of Integer = (0, OD_LARGEICON, OD_SMALLICON, OD_LIST, OD_REPORT, OD_SHVIEW_TILE, OD_THUMBNAIL);

////////////////////////////////////////////////////////////////////////////////
// Enumeration
////////////////////////////////////////////////////////////////////////////////

function GetFilterMask(Filter: String; Index: Integer): String;
var
  I, L: Integer;
  CurrentPart: Integer;
begin
  SetLength(Result, Length(Filter));
  I := 0;
  L := 0;
  CurrentPart := 0;
  while (I < Length(Filter)) and (CurrentPart + 1 <= Index * 2) do begin
    Inc(I);
    if Filter[I] = '|' then begin
      Inc(CurrentPart);
    end
    else if CurrentPart + 1 = Index * 2 then begin
      Inc(L);
      Result[L] := Filter[I];
    end;
  end;
  SetLength(Result, L);
end;

function EnumChildProc(Handle: HWND; Infos: PPegtopFileDialogItemInfos): Bool; stdcall;
var
  // ClassName: array[0..80] of Char;
  ID: Integer;
  procedure Found(Item: TPegtopFileDialogItem);
  begin
    Infos^[Item].ID := ID;
    Infos^[Item].Handle := Handle;
    Infos^[Item].Found := True;
  end;
begin
  Result := True;
  // GetClassName(Handle, ClassName, 80);
  ID := GetDlgCtrlID(Handle);
  // Compare ID to the ones declared in Dlgs.pas:
  if      ID = stc4 then Found(pfiFolderLabel)
  else if ID = cmb2 then Found(pfiFolderComboBox)
  else if ID = lst1 then Found(pfiFileListBox)
  else if ID = stc3 then Found(pfiFileNameLabel)
  else if ID = edt1 then Found(pfiFileNameEdit)
  else if ID = stc2 then Found(pfiFileTypeLabel)
  else if ID = cmb1 then Found(pfiFileTypeComboBox)
  else if ID = 1    then Found(pfiOkButton)
  else if ID = 2    then Found(pfiCancelButton);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomFileDialog
////////////////////////////////////////////////////////////////////////////////

procedure TPegtopCustomFileDialog.DoShow;
begin
  inherited;
  EnumChildWindows(Windows.GetParent(Handle), @EnumChildProc, Integer(@FItemInfos));
  FVisible := True;
  FChangeViewStyle := True;
end;

procedure TPegtopCustomFileDialog.DoClose;
begin
  FVisible := False;
  FillChar(FItemInfos, SizeOf(FItemInfos), 0);
  inherited;
  Application.HideHint;
end;

procedure TPegtopCustomFileDialog.DoTypeChange;
begin
  if FAutoAdjustExtension then AdjustExtension;
  inherited;
end;

procedure TPegtopCustomFileDialog.DoFolderChange;
begin
  if FChangeViewStyle then begin
    AdjustViewStyle;
    FChangeViewStyle := False;
  end;
  inherited;
end;

function TPegtopCustomFileDialog.IsSaveDialog: Boolean;
begin
  Result := False;
end;

function TPegtopCustomFileDialog.Execute: Boolean;
begin
  if IsSaveDialog then
    Result := DoExecute(@GetSaveFileName)
  else
    Result := DoExecute(@GetOpenFileName);
end;

procedure TPegtopCustomFileDialog.Close;
begin
  PostMessage(Windows.GetParent(Handle), WM_CLOSE, 0, 0);
end;

procedure TPegtopCustomFileDialog.AdjustExtension;
var
  CurrentMask: String;
  CurrentText: String;
  P: Integer;
begin
  if FItemInfos[pfiFileNameEdit].Found then begin
    CurrentMask := GetFilterMask(Filter, FilterIndex);
    if (CurrentMask <> '') then begin
      P := Pos(';', CurrentMask);
      if P > 0 then CurrentMask := Copy(CurrentMask, 1, P - 1); // use first mask if multiple masks found
      CurrentMask := ExtractFileExt(CurrentMask);
      if (Pos('*', CurrentMask) = 0) and (Pos('?', CurrentMask) = 0) then begin
        CurrentText := ItemTexts[pfiFileNameEdit];
        CurrentText := Copy(CurrentText, 1, Length(CurrentText) - Length(ExtractFileExt(CurrentText)))
          + CurrentMask;
        ItemTexts[pfiFileNameEdit] := CurrentText;
      end;
    end;
  end;
end;

procedure TPegtopCustomFileDialog.AdjustViewStyle;
var
  ViewHandle: THandle;
begin
  if Visible and (FViewStyle <> pvsDefault) then begin
    ViewHandle := FindWindowEx(Windows.GetParent(Handle), 0, 'SHELLDLL_DefView', NIL);
    if (ViewHandle <> 0) then begin
      SendMessage(ViewHandle, WM_COMMAND, ViewStyleIDs[FViewStyle], 0);
    end;
  end;
end;

function TPegtopCustomFileDialog.GetItemHandle(Item: TPegtopFileDialogItem): THandle;
begin
  if FItemInfos[Item].Found then
    // Result := GetDlgItem(Windows.GetParent(Handle), FItemInfos[Item].ID)
    Result := FItemInfos[Item].Handle
  else
    Result := 0;
end;

function TPegtopCustomFileDialog.GetItemRect(Item: TPegtopFileDialogItem): TRect;
var
  H: THandle;
begin
  if FItemInfos[Item].Found then begin
    H := GetParent(ItemHandles[Item]);
    if (H <> 0)
    and GetWindowRect(ItemHandles[Item], Result) then begin
      Windows.ScreenToClient(H, Result.TopLeft);
      Windows.ScreenToClient(H, Result.BottomRight);
    end
    else begin
      Result := Rect(0, 0, 0, 0);
    end;
  end
  else begin
    Result := Rect(0, 0, 0, 0);
  end;
end;

function TPegtopCustomFileDialog.GetItemText(Item: TPegtopFileDialogItem): String;
var
  H: THandle;
  L: Integer;
begin
  if FItemInfos[Item].Found then begin
    H := Windows.GetParent(Handle);
    if H <> 0 then begin
      SetLength(Result, 256);
      L := GetDlgItemText(H, FItemInfos[Item].ID, @Result[1], Length(Result));
      SetLength(Result, L);
    end
    else begin
      Result := '';
    end;
  end
  else begin
    Result := '';
  end;
end;

procedure TPegtopCustomFileDialog.SetItemText(Item: TPegtopFileDialogItem; Value: String);
var
  H: THandle;
begin
  if FItemInfos[Item].Found then begin
    H := Windows.GetParent(Handle);
    if H <> 0 then
      SetDlgItemText(H, FItemInfos[Item].ID, PChar(Value));
  end;
end;

function TPegtopCustomFileDialog.GetItemVisibility(Item: TPegtopFileDialogItem): Boolean;
var
  H: THandle;
begin
  H := GetItemHandle(Item);
  if H <> 0 then
    Result := IsWindowVisible(H)
  else
    Result := False;
end;

procedure TPegtopCustomFileDialog.SetItemVisibility(Item: TPegtopFileDialogItem; Value: Boolean);
const
  VisibilityFlags: array[Boolean] of Cardinal = (SWP_HIDEWINDOW, SWP_SHOWWINDOW);
var
  H: THandle;
begin
  H := GetItemHandle(Item);
  if H <> 0 then
    SetWindowPos(H, 0, 0, 0, 0, 0, VisibilityFlags[Value] or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

procedure TPegtopCustomFileDialog.SetViewStyle(Value: TPegtopFileDialogViewStyle);
begin
  if FViewStyle <> Value then begin
    FViewStyle := Value;
    AdjustViewStyle;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopCustomExtendedFileDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopCustomExtendedFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtendedSize := 24;
  FExtendedAlignment := peaBottom;
  FExtendedContainer := CreateExtendedContainer;
end;

function TPegtopCustomExtendedFileDialog.CreateExtendedContainer: TWinControl;
var
  Panel: TPanel;
begin
  Panel := TPanel.Create(Self);
  Panel.Name := 'ExtendedContainer';
  Panel.Caption := '';
  Panel.BevelOuter := bvNone;
  Panel.BevelInner := bvNone;
  Panel.BorderWidth := 0;
  Result := Panel;
end;

function TPegtopCustomExtendedFileDialog.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := inherited MessageHook(Msg);
end;

procedure TPegtopCustomExtendedFileDialog.DialogWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_PEGTOP_ALIGN_FILE_DIALOG then begin
    FAlignEnabled := True;
    DoAlign;
    Msg.Result := 0;
  end
  else begin
    Msg.Result := CallWindowProc(FOldDialogWndProc, FDialogHandle, Msg.Msg, Msg.WParam, Msg.LParam);
    if (Msg.Msg = WM_WINDOWPOSCHANGED)
    and ((TWMWindowPosMsg(Msg).WindowPos.Flags and SWP_NOSIZE) = 0) then begin
      if FExtendedContainer <> NIL then begin
        if FExtendedAlignment = peaRight then
          FExtendedContainer.Height := TWMWindowPosMsg(Msg).WindowPos.cy - 16
          - FDialogRect.Bottom + FDialogrect.Top + FClientRect.Bottom
        else
          FExtendedContainer.Width := TWMWindowPosMsg(Msg).WindowPos.cx - 14
          - FDialogRect.Right + FDialogrect.Left + FClientRect.Right;
      end;
      if FAlignEnabled then begin
        DoResize;
        DoAlign;
      end;
    end;
  end;
end;

procedure TPegtopCustomExtendedFileDialog.DoResize;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

procedure TPegtopCustomExtendedFileDialog.DoAlign;
begin
  if Assigned(FOnAlign) then FOnAlign(Self);
end;

procedure TPegtopCustomExtendedFileDialog.DoShow;
var
  DeltaWidth, DeltaHeight: Integer;
  ExtendedRect: TRect;
  NewRect: TRect;
begin
  FAlignEnabled := False;
  FDialogHandle := Windows.GetParent(Handle);
  if FDialogHandle <> 0 then begin
    FDialogMethodInstance := MakeObjectInstance(DialogWndProc);
    FOldDialogWndProc := Pointer(SetWindowLong(FDialogHandle, GWL_WNDPROC, Integer(FDialogMethodInstance)));
    if FExtendedContainer <> NIL then begin
      GetWindowRect(FDialogHandle, FDialogRect);
      GetClientRect(FDialogHandle, FClientRect);
      if FExtendedAlignment = peaRight then begin
        DeltaWidth := FExtendedSize + 8;
        DeltaHeight := 0;
        ExtendedRect.Left := FClientRect.Right;
        ExtendedRect.Top := 8;
        ExtendedRect.Right := FClientRect.Right + DeltaWidth - 8;
        ExtendedRect.Bottom := FClientRect.Bottom - 8;
      end
      else begin
        DeltaWidth := 0;
        DeltaHeight := FExtendedSize + 8;
        ExtendedRect.Left := FClientRect.Left + 8;
        ExtendedRect.Top := FClientRect.Bottom;
        ExtendedRect.Right := FClientRect.Right - 6;
        ExtendedRect.Bottom := FClientRect.Bottom + DeltaHeight - 8;
      end;
      FExtendedContainer.ParentWindow := FDialogHandle;
      FExtendedContainer.BoundsRect := ExtendedRect;
      NewRect := Bounds(FDialogRect.Left - DeltaWidth div 2, FDialogRect.Top - DeltaHeight div 2,
        FDialogRect.Right - FDialogRect.Left + DeltaWidth, FDialogRect.Bottom - FDialogRect.Top + DeltaHeight);
      SetWindowPos(FDialogHandle, 0, NewRect.Left, NewRect.Top,
        NewRect.Right - NewRect.Left, NewRect.Bottom - NewRect.Top,
        SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER);
    end;
  end;
  inherited;
  if FDialogHandle <> 0 then PostMessage(FDialogHandle, WM_PEGTOP_ALIGN_FILE_DIALOG, 0, 0);
end;

procedure TPegtopCustomExtendedFileDialog.DoClose;
begin
  if Assigned(FDialogMethodInstance) then begin
    SetWindowLong(FDialogHandle, GWL_WNDPROC, Integer(FOldDialogWndProc));
    FreeObjectInstance(FDialogMethodInstance);
  end;
  FDialogHandle := 0;
  FDialogMethodInstance := NIL;
  inherited;
end;

function TPegtopCustomExtendedFileDialog.Execute: Boolean;
begin
  Result := inherited Execute;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopExtendedSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopExtendedSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
