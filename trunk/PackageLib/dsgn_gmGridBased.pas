unit dsgn_gmGridBased;

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
  SysUtils, Classes, GR32, GR32_Image, GR32_Layers, GR32_Filters,
  gmGridBased, AppEvnts, ActnList, gmGradient_FileDlgs,
  gmGridBased_List;

type
  TGridBasedIndexProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetGridBasedListAt(Index: Integer): TgmGridBasedList;
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


procedure Register;

implementation

uses
{ Standard }
  Math,
{ GraphicsMagic }
  gmGridBased_ListView;

{ Registration }
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TgmGridBasedIndex), TgmGridBasedListView, 'GridBasedIndex', TGridBasedIndexProperty);
  //RegisterPropertyEditor(TypeInfo(TgmGridBasedIndex),TgmGridBasedEditor , 'GridBasedIndex', TGridBasedIndexProperty);
  //RegisterPropertyEditor(TypeInfo(TgmGridBasedIndex),TgmGridBasedsGrid , 'GridBasedIndex', TGridBasedIndexProperty);
end;


type
  TGridBasedListAccess = class(TgmGridBasedList);


{ TGridBasedIndexProperty }

function TGridBasedIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, {paSortList,} paRevertable];
end;

procedure TGridBasedIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  LRight          : Integer;
  LGridBasedIndex : Integer;
  LGridBasedList  : TgmGridBasedList;
  LRect           : TRect;
begin
  LGridBasedList := Self.GetGridBasedListAt(0);
  LRight         := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;

  with ACanvas do
  begin
    Lock;
    LRect := Rect(ARect.TopLeft, Point(LRight, ARect.Bottom));
    InflateRect(LRect, -2, -2);

    LGridBasedIndex := StrToInt(Value);
    ACanvas.FillRect(ARect);

    if Assigned(LGridBasedList) then
    begin
      TGridBasedListAccess(LGridBasedList).Collection.Draw(LGridBasedIndex, ACanvas, LRect);
    end;

    DefaultPropertyListDrawValue(Value, ACanvas, Rect(LRight, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);

    Unlock;
  end;
end;

procedure TGridBasedIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

procedure TGridBasedIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight('Wg'), GetSystemMetrics(SM_CYCURSOR) + 4);
end;

function TGridBasedIndexProperty.GetGridBasedListAt(
  Index: Integer): TgmGridBasedList;
var
  C              : TPersistent;
  IGridBasedList : IGridBasedListSupport;
begin
  Result := nil;

  { ? I'm guessing that the Index parameter is a component index (one that
    would be passed to the GetComponent function). }
  C := GetComponent(Index);

  if Supports(C, IGridBasedListSupport, IGridBasedList) then
  begin  //if C is Tgmgrid
    Result := IGridBasedList.GetGridBasedList;
  end;
end;

procedure TGridBasedIndexProperty.GetValues(Proc: TGetStrProc);
var
  LList : TgmGridBasedList;
  i     : Integer;
begin
  LList := GetGridBasedListAt(0);

  if Assigned(LList) then
    for i := 0 to (LList.Count - 1) do
      Proc(IntToStr(i));
end;

end.
