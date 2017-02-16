unit gmGridBased_Preview;

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
  SysUtils, Classes, Controls,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic }
  gmGridBased, gmGridBased_List;

type
  TgmGridBasedPreview = class(TCustomImage32, IGridBasedListSupport)
  private
    FItemIndex  : TgmGridBasedIndex;
    FItemList   : TgmGridBasedList;
    FChangeLink : TgmGridBasedChangeLink;
    
    procedure SetItemIndex(const AValue: TgmGridBasedIndex);
    procedure SetItemList(const AValue: TgmGridBasedList);
    procedure GridBasedListChanged(ASender: TObject);

    { IGrid BasedListSupport }
    function GetGridBasedList: TgmGridBasedList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdatePreview; dynamic;
    
    property ItemList : TgmGridBasedList read GetGridBasedList write SetItemList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure ExecCustom(ADest: TBitmap32; AStageNum: Integer); override; // PST_CUSTOM
  published
    property ItemIndex : TgmGridBasedIndex read FItemIndex write SetItemIndex;
  end;


implementation

uses
{ GraphicsMagic }
  gmMiscFuncs;

type
  TgmGridBasedListAccess = class(TgmGridBasedList);

{ TgmGridBasedPreview }

constructor TgmGridBasedPreview.Create(AOwner: TComponent);
begin
  inherited;
  
  FChangeLink          := TgmGridBasedChangeLink.Create;
  FChangeLink.OnChange := GridBasedListChanged;
  
  ScaleMode       := smStretch;
  Bitmap.DrawMode := dmBlend;
  BitmapAlign     := baTopLeft;
  
  with PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then
    begin
      Stage := PST_CUSTOM;
    end;
  end;
end;

destructor TgmGridBasedPreview.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TgmGridBasedPreview.ExecCustom(ADest: TBitmap32;
  AStageNum: Integer);
begin
  DrawCheckerboard(ADest);
  inherited;
end;

function TgmGridBasedPreview.GetGridBasedList: TgmGridBasedList;
begin
  Result := FItemList;
end;

procedure TgmGridBasedPreview.GridBasedListChanged(ASender: TObject);
begin
  if Assigned(ASender) and (ASender = FItemList) then
  begin
    UpdatePreview;
    Invalidate;
  end;
end;

procedure TgmGridBasedPreview.SetItemIndex(const AValue: TgmGridBasedIndex);
begin
  FItemIndex := AValue;
  
  UpdatePreview;
  Invalidate;
end;

procedure TgmGridBasedPreview.SetItemList(const AValue: TgmGridBasedList);
begin
  if FItemList <> nil then
  begin
    FItemList.UnRegisterChanges(FChangeLink);
    FItemList.RemoveFreeNotification(Self);
  end;
  
  FItemList := AValue;
  
  if FItemList <> nil then
  begin
    FItemList.RegisterChanges(FChangeLink);
    FItemList.FreeNotification(Self);
  end;
  
  UpdatePreview;
  Invalidate;
end;

procedure TgmGridBasedPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FItemList) then
  begin
    FItemList  := nil;
    FItemIndex := -1;

    UpdatePreview;
    Invalidate;
  end;
end; 

procedure TgmGridBasedPreview.UpdatePreview;
var
  LItem : TgmGridBasedItem;
begin
  if Assigned(FItemList) and FItemList.IsValidIndex(Self.FItemIndex) then
  begin
    LItem := TgmGridBasedListAccess(FItemList).Collection.Items[FItemIndex] as TgmGridBasedItem;
      
    Bitmap.BeginUpdate;
    Bitmap.SetSize(Width, Height);
    Bitmap.Assign( LItem.CachedBitmap(Width, Height) );
    Bitmap.EndUpdate;
  end
  else
  begin
    Bitmap.BeginUpdate;
    Bitmap.SetSize(0, 0);
    Bitmap.EndUpdate;
  end;
end;

end.
