unit gmGradient_ListView;

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
  Classes,
{ GraphicsMagic }
  gmGridBased_ListView, gmGradient_List;

type
  TgmGradientListView = class(TgmGridBasedListView)
  private
    function GetGradientList: TgmGradientList;
    procedure SetGradientList(const AValue: TgmGradientList);
  protected

  public

  published
    property Align;
    property AutoSize;
    property Anchors;
    property Color;
    property ParentColor;

    property GrowFlow;
    property GradientList : TgmGradientList read GetGradientList write SetGradientList;
    property ThumbWidth;
    property ThumbHeight;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property Scale;
    property ScaleMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property GridOptions;
    property CellBorderStyle;
    property FrameColor;
    property SelectedColor;
    property ItemIndex;

    property OnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;    
  end;


implementation


{ TgmGradientListView }

function TgmGradientListView.GetGradientList: TgmGradientList;
begin
  Result := ItemList as TgmGradientList;
end;

procedure TgmGradientListView.SetGradientList(const AValue: TgmGradientList);
begin
  ItemList := AValue;
end;

end.
