unit gmGridBased_FileDlg;

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
 * The Original Code is Gradient Editor.
 *
 * The Initial Developer of the Original Code are
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

uses
{ Standard }
  Messages, Windows, SysUtils, Classes, Controls, Forms,StdCtrls, Graphics,
  ExtCtrls, Buttons, CommDlg, Dlgs, ExtDlgs, Dialogs,
{ GraphicsMagic }
  gmGridBased, gmGridBased_List, gmGridBased_ListView;

type

{ TOpenGridBasedDialog }

  TOpenGridBasedDialog = class(TOpenDialog)
  private
    FGridPanel     : TPanel;
    FGridLabel     : TLabel;
    FSavedFilename : string;

    procedure SetGridBasedCollection(const AValue: TgmGridBasedCollection);

    function GetGridBasedCollection: TgmGridBasedCollection;
    function IsFilterStored: Boolean;
  protected
    FScrollBox         : TScrollbox;
    FGridBasedList     : TgmGridBasedList;
    FGridBasedListView : TgmGridBasedListView;

    // polymorphism
    function GetGridBasedListClass: TgmGridBasedListClass; virtual; abstract;
    function GetGridBasedListViewClass: TgmGridBasedListViewClass; virtual; abstract;
    function GetGridLabelCaption: string; virtual;
    function IsSaveDialog: Boolean; virtual;
    function GetFilter: string; 

    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;

    property GridBasedListView : TgmGridBasedListView read FGridBasedListView;
    property GridLabel         : TLabel               read FGridLabel;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;

    property GridBasedCollection : TgmGridBasedCollection read GetGridBasedCollection write SetGridBasedCollection;
  published
    property Filter stored IsFilterStored;
  end;


implementation

uses
  Consts, Math;


type
  TGridBasedListViewAccess = class(TgmGridBasedListView);
  TGridBasedListAccess     = class(TgmGridBasedList);


{ TOpenGridBasedDialog }

constructor TOpenGridBasedDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FGridBasedList := GetGridBasedListClass.Create(Self);
  Filter         := GetFilter();

  FGridPanel := TPanel.Create(Self);
  with FGridPanel do
  begin
    Name        := 'GridPanel';
    Caption     := '';
    BevelOuter  := bvNone;
    BorderWidth := 6;
    TabOrder    := 1;
    SetBounds(204, 5, 169, 200);

    FGridLabel := TLabel.Create(Self);
    with FGridLabel do
    begin
      Name     := 'GridLabel';
      Caption  := GetGridLabelCaption;
      Align    := alTop;
      AutoSize := False;
      Parent   := FGridPanel;
      SetBounds(6, 6, 157, 23);
    end;

    FScrollBox := TScrollbox.Create(Self);
    with FScrollBox do
    begin
      Name       := 'PaintPanel';
      Align      := alClient;
      BevelInner := bvRaised;
      BevelOuter := bvLowered;
      Ctl3D      := True;
      Color      := clBtnShadow;
      TabOrder   := 0;
      Parent     := FGridPanel;
      SetBounds(6, 29, 157, 145);

      FGridBasedListView := GetGridBasedListViewClass.Create(Self);
      with TGridBasedListViewAccess(FGridBasedListView) do
      begin
        Name      := 'PaintBox';
        Align     := alTop;
        Hint      := 'Preview of selected file';
        Parent    := FScrollBox;
        GrowFlow  := ZWidth2Bottom;
        AutoSize  := True;
        ItemList  := FGridBasedList;
        SetThumbSize(32, 32);
      end;
    end;
  end;
end;

procedure TOpenGridBasedDialog.DoSelectionChange;

  function ValidFile(const AFileName: string): Boolean;
  begin
    Result := GetFileAttributes( PChar(AFileName) ) <> $FFFFFFFF;
  end;

var
  LFullName   : string;
  LValidFile  : Boolean;
  LCollection : TgmGridBasedCollection;
begin
  LCollection := TGridBasedListAccess(FGridBasedList).Collection;
  LFullName   := FileName;
  
  if LFullName <> FSavedFilename then
  begin
    FSavedFilename := LFullName;
    LValidFile     := FileExists(LFullName) and ValidFile(LFullName);

    if LValidFile then
    try
      LCollection.BeginUpdate;
      try
        LCollection.Clear;
        LCollection.LoadFromFile(LFullName);
      finally
        LCollection.EndUpdate;
        FGridBasedListView.Height         := 1; //force
        FScrollBox.VertScrollBar.Position := 0;
        FGridLabel.Caption                := Format('%d Items', [LCollection.Count]);
      end;
    except
      LValidFile := False;
    end;
    
    if not LValidFile then
    begin
      FGridLabel.Caption := GetGridLabelCaption;
      LCollection.Clear;
    end;
  end;
  
  inherited DoSelectionChange;
end;

procedure TOpenGridBasedDialog.DoClose;
begin
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TOpenGridBasedDialog.DoShow;
var
  LPreviewRect, LStaticRect : TRect;
begin
  // Set preview area to entire dialog 
  GetClientRect(Handle, LPreviewRect);
  LStaticRect := GetStaticRect;
  
  // Move preview area to right of static area 
  LPreviewRect.Left := LStaticRect.Left + (LStaticRect.Right - LStaticRect.Left);
  Inc(LPreviewRect.Top, 4);

  FGridPanel.BoundsRect := LPreviewRect;
  FGridPanel.Anchors    := [akLeft, akTop, akRight, akBottom];

  if not Self.IsSaveDialog then
  begin
    TGridBasedListAccess(FGridBasedList).Collection.Clear;
  end;

  FSavedFilename          := '';
  FGridPanel.ParentWindow := Handle;

  inherited DoShow;
end;

function TOpenGridBasedDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' 
  else
    Template := nil;

  if Self.IsSaveDialog then
    Result := DoExecute(@GetSaveFileName)
  else
    Result := inherited Execute;

  TGridBasedListAccess(FGridBasedList).Collection.Clear;
end;

function TOpenGridBasedDialog.IsFilterStored: Boolean;
begin
  Result := not ( Filter = GetFilter() );
end;

procedure TOpenGridBasedDialog.SetGridBasedCollection(
  const AValue: TgmGridBasedCollection);
begin
  TGridBasedListAccess(FGridBasedList).Collection.Assign(AValue);
end;

function TOpenGridBasedDialog.GetGridBasedCollection: TgmGridBasedCollection;
begin
  Result := TGridBasedListAccess(FGridBasedList).Collection;
end;

function TOpenGridBasedDialog.GetFilter: string;
begin
  if Self.IsSaveDialog then
    Result := TGridBasedListAccess(FGridBasedList).Collection.WritersFilter
  else
    Result := TGridBasedListAccess(FGridBasedList).Collection.ReadersFilter;
end;

function TOpenGridBasedDialog.IsSaveDialog: boolean;
begin
  Result := False;
end;

function TOpenGridBasedDialog.GetGridLabelCaption: string;
begin
  Result := SPictureLabel;
end;


end.
