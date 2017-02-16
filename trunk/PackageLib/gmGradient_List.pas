unit gmGradient_List;

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
  Classes, Graphics,
{ GraphicsMagic }
  gmGradient, gmGridBased, gmGridBased_List;


type
  TgmGradientIndex = type TgmGridBasedIndex; //used for display gradient in property editor

  TgmGradientList = class(TgmGridBasedList)
  private
    function GetItem(AIndex: TgmGradientIndex): TgmGradientItem;
    function GetGradients: TgmGradientCollection;
    function GetForegroundColor: TColor;
    function GetBackgroundColor: TColor;

    procedure SetItem(AIndex: TgmGradientIndex; const AValue: TgmGradientItem);
    procedure SetGradients(const AValue: TgmGradientCollection);
    procedure SetForegroundColor(const AValue: TColor);
    procedure SetBackgroundColor(const AValue: TColor);
  protected
    function CollectionClass: TgmGridBasedCollectionClass; override;
  public
    property Items[Index: TgmGradientIndex]: TgmGradientItem read GetItem write SetItem; default;
  published
    property Gradients : TgmGradientCollection read GetGradients write SetGradients;

    property ForegroundColor : TColor read GetForegroundColor write SetForegroundColor;
    property BackgroundColor : TColor read GetBackgroundColor write SetBackgroundColor;
  end;

  IgmGradientListSupport = interface
    ['{ACB97BAF-7F3F-425D-AB8C-90B197C31DB0}']
    function GetGradients: TgmGradientList;
  end;

implementation

{ TgmGradientList }

function TgmGradientList.CollectionClass: TgmGridBasedCollectionClass;
begin
  Result := TgmGradientCollection;
end;

function TgmGradientList.GetBackgroundColor: TColor;
begin
  Result := Gradients.BackgroundColor;
end;

function TgmGradientList.GetForegroundColor: TColor;
begin
  Result := Gradients.ForegroundColor;
end;

function TgmGradientList.GetGradients: TgmGradientCollection;
begin
  Result := TgmGradientCollection(Self.Collection);
end;

function TgmGradientList.GetItem(AIndex: TgmGradientIndex): TgmGradientItem;
begin
  Result := TgmGradientItem(Collection.Items[AIndex])
end;

procedure TgmGradientList.SetBackgroundColor(const AValue: TColor);
begin
  Gradients.BackgroundColor := AValue;
  Self.Change;
end;

procedure TgmGradientList.SetForegroundColor(const AValue: TColor);
begin
  Gradients.ForegroundColor := AValue;
  Self.Change;
end;

procedure TgmGradientList.SetGradients(const AValue: TgmGradientCollection);
begin
  Collection.Assign(AValue);
end;

procedure TgmGradientList.SetItem(AIndex: TgmGradientIndex;
  const AValue: TgmGradientItem);
begin
  Collection.Items[AIndex].Assign(AValue);
end;

end.
