unit gmGridBased_List;

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
  SysUtils, Classes, Graphics,
{ GraphicsMagic }
  gmGridBased;

type

{ TChangeLink }
  TgmGridBasedList  = class;        //later definition
  TgmGridBasedIndex = type Integer; //used for display gradient in property editor


  TgmGridBasedChangeLink = class(TObject)
  private
    FSender   : TgmGridBasedList;
    FOnChange : TNotifyEvent;
  public
    destructor Destroy; override;
    procedure Change; dynamic;

    property OnChange : TNotifyEvent     read FOnChange write FOnChange;
    property Sender   : TgmGridBasedList read FSender   write FSender;
  end;

  { TgmGridBasedList }

  TgmGridBasedList = class(TComponent)
  private
    FCollection : TgmGridBasedCollection;
    FClients    : TList;
    FOnChange   : TNotifyEvent;
    
    procedure SetCollection(const AValue: TgmGridBasedCollection);
    procedure GridChanged(ASender: TObject);

    function GetCount: Integer;
  protected
    procedure Change; dynamic;
    function CollectionClass: TgmGridBasedCollectionClass; virtual;

    property Collection : TgmGridBasedCollection read FCollection write SetCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    function IsValidIndex(const AIndex: Integer): Boolean;
    
    procedure RegisterChanges(const AValue: TgmGridBasedChangeLink);
    procedure UnRegisterChanges(const AValue: TgmGridBasedChangeLink);

    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure SaveToFile(const AFileName: string); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;

    property Count : Integer read GetCount;
  published
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  TgmGridBasedListClass = class of TgmGridBasedList;

  IGridBasedListSupport = interface
    //used by property editor in design time
    //implement it in various component
    ['{6CC76557-5CA1-4B58-8D90-CDE901548414}']
    function GetGridBasedList: TgmGridBasedList;
  end;

  
implementation

{ TgmGridBasedChangeLink }

procedure TgmGridBasedChangeLink.Change;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Sender);
  end;
end;

destructor TgmGridBasedChangeLink.Destroy;
begin
  if Sender <> nil then
  begin
    Sender.UnRegisterChanges(Self);
  end;

  inherited;
end;

{ TgmGridBasedList }

constructor TgmGridBasedList.Create(AOwner: TComponent);
begin
  inherited;

  FClients             := TList.Create;
  FCollection          := CollectionClass.Create(Self);
  FCollection.OnChange := GridChanged;
end;

destructor TgmGridBasedList.Destroy;
begin
  while FClients.Count > 0 do
  begin
    UnRegisterChanges( TgmGridBasedChangeLink(FClients.Last) );
  end;

  FClients.Free;
  FClients := nil;

  inherited;
end;

procedure TgmGridBasedList.Change;
var
  i : Integer;
begin
  if FClients <> nil then
  begin
    for i := 0 to (FClients.Count - 1) do
    begin
      TgmGridBasedChangeLink(FClients[i]).Change;
    end;
  end;

  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

function TgmGridBasedList.CollectionClass: TgmGridBasedCollectionClass;
begin
  Result := TgmGridBasedCollection; // descendant must override it
end;

function TgmGridBasedList.GetCount: Integer;
begin
  Result := Collection.Count;
end;

procedure TgmGridBasedList.GridChanged(ASender: TObject);
begin
  Change;
end;

function TgmGridBasedList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex > -1) and (AIndex < Count);
end;

procedure TgmGridBasedList.LoadFromFile(const AFileName: string);
begin
  Collection.LoadFromFile(AFileName);
end;

procedure TgmGridBasedList.LoadFromStream(const AStream: TStream);
begin
  Collection.LoadFromStream(AStream);
end;

procedure TgmGridBasedList.RegisterChanges(const AValue: TgmGridBasedChangeLink);
begin
  AValue.Sender := Self;

  if FClients <> nil then
  begin
    FClients.Add(AValue);
  end;
end;

procedure TgmGridBasedList.UnRegisterChanges(
  const AValue: TgmGridBasedChangeLink);
var
  i : Integer;
begin
  if FClients <> nil then
  begin
    for i := 0 to (FClients.Count - 1) do
    begin
      if FClients[i] = AValue then
      begin
        AValue.Sender := nil;
        FClients.Delete(i);
        Break;
      end;
    end;
  end;
end;

procedure TgmGridBasedList.SaveToFile(const AFileName: string);
begin
  Collection.SaveToFile(AFileName);
end;

procedure TgmGridBasedList.SaveToStream(const AStream: TStream);
begin
  Collection.SaveToStream(AStream);
end;

procedure TgmGridBasedList.SetCollection(const AValue: TgmGridBasedCollection);
begin
  FCollection.Assign(AValue);
end;


end.
