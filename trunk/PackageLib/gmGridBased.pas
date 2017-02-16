unit gmGridBased;

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

uses
{ Standard }
  Types, Classes, SysUtils, Graphics,
{ Graphics32 }
  GR32, GR32_LowLevel,
{ GraphicsMagic }
  gmFileFormatList;

type
  TgmGrowFlow = (ZWidth2Bottom,
                 NHeight2Right,
                 OSquaredGrow,
                 XStretchInnerGrow);
                 

  TgmGridBasedItem = class(TCollectionItem)
  private
    
  protected
    FDisplayName       : string;
    FCachedBitmap      : TBitmap32;
    FCachedBitmapValid : Boolean;
    
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;

    function CachedBitmap(const AWidth, AHeight: Integer): TBitmap32; virtual;
    function GetHint:string; virtual;
  published
    property DisplayName read GetDisplayName write SetDisplayName;
  end;


  TgmGridBasedCollection = class(TOwnedCollection)
  private
    FOnChange : TNotifyEvent;
  protected
    procedure Update(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent); overload; virtual;
    constructor Create(AOwner: TComponent; AItemClass: TCollectionItemClass); overload; virtual;

    function IsValidIndex(const AIndex: Integer): Boolean; virtual;

    { "Simulating class properties in (Win32) Delphi"
      http://delphi.about.com/library/weekly/aa031505a.htm
      While (Win32) Delphi enables you to create class (static) methods
      (function or procedure), you cannot mark a property of a class to be a
      class (static) property. False. You can! Let's see how to simulate
      class properties using typed constants. }
    class function GetFileReaders : TgmFileFormatsList; virtual; abstract; 
    class function GetFileWriters : TgmFileFormatsList; virtual; abstract;

    class procedure RegisterConverterReader(const AExtension, ADescription: string;
      const ADescID: Integer; const AReaderClass: TgmConverterClass);

    class procedure RegisterConverterWriter(const AExtension, ADescription: string;
      const ADescID: Integer; const AWriterClass: TgmConverterClass);
      
    class function ReadersFilter: string;
    class function WritersFilter: string;

    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure SaveToFile(const AFileName: string); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;

    function Draw(const AIndex: Integer; const ACanvas: TCanvas;
      const ARect: TRect): Boolean;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;
  TgmGridBasedCollectionClass = class of TgmGridBasedCollection;
  

implementation

uses
{ GraphicsMagic }
  gmMiscFuncs;
  

{ TgmGridBasedItem }

constructor TgmGridBasedItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDisplayName := 'Custom';
end;

function TgmGridBasedItem.CachedBitmap(
  const AWidth, AHeight: Integer): TBitmap32;
begin
  Result := nil; //descendant must override;
  
  if Assigned(FCachedBitmap) then
  begin
    Result := FCachedBitmap;
  end;
end;

function TgmGridBasedItem.GetHint: string;
begin
  Result := DisplayName;
end;

function TgmGridBasedItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TgmGridBasedItem.SetDisplayName(const AValue: string);
begin
  FDisplayName := AValue;
  Changed(False);
end;

{ TgmGridBasedCollection }

constructor TgmGridBasedCollection.Create(AOwner: TComponent);
begin
  Create(AOwner, TgmGridBasedItem);
end;

constructor TgmGridBasedCollection.Create(AOwner: TComponent;
  AItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AItemClass);
end;

function TgmGridBasedCollection.Draw(const AIndex: Integer;
  const ACanvas: TCanvas; const ARect: TRect): Boolean;
var
  LBmp  : TBitmap32;
  LRect : TRect;
  LItem : TgmGridBasedItem;
begin
  Result := False;

  if not IsValidIndex(AIndex) then
  begin
    raise Exception.Create('GridBasedCollection.Draw() -- Error: Invalid index.');
  end;
  
  if not Assigned(ACanvas) then
  begin
    raise Exception.Create('GridBasedCollection.Draw() -- Error: Canvas is nil.');
  end;

  if GR32.IsRectEmpty(ARect) then
  begin
    raise Exception.Create('GridBasedCollection.Draw() -- Error: Rect is Empty');
  end;

  with ARect do
  begin
    LRect := MakeRect(0, 0, Right - Left, Bottom - Top);
  end;

  LItem := TgmGridBasedItem(Items[AIndex]);

  LBmp := TBitmap32.Create;
  try
    LBmp.SetSize(LRect.Right +1,LRect.Bottom + 1);
    DrawCheckerboard(LBmp, LBmp.BoundsRect);

    LBmp.Draw( 0, 0, LItem.CachedBitmap(LRect.Right + 1, LRect.Bottom + 1) );
    LBmp.DrawTo(ACanvas.Handle, ARect.Left, ARect.Top);
  finally
    LBmp.Free;
  end;
end;

function TgmGridBasedCollection.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex > -1) and (AIndex < Count);
end;

procedure TgmGridBasedCollection.LoadFromFile(const AFileName: string);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create( ExpandFileName(AFileName), fmOpenRead or fmShareDenyWrite );
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TgmGridBasedCollection.LoadFromStream(const AStream: TStream);
var
  i                    : Integer;
  LFirstStreamPosition : Int64;
  LReader              : TgmConverter;
  LReaderClass         : TgmConverterClass;
  LReaders             : TgmFileFormatsList;
  LReaderAccepted      : Boolean;
begin
  BeginUpdate;
  try
    // In case current stream position is not zero, we remember the position.
    LFirstStreamPosition := AStream.Position;


    // Because many readers have same MagicWord as their signature,
    // we want to ask all of them,
    // when anyone is accepting, we break.
    // if UGradientReaders.Find(IntToHex(MagicWord,8),i) then
    //  (UGradientReaders.Objects[i] as TgmCustomGradientReader).LoadFromStream(Stream, Self);
    LReaders := GetFileReaders;
    
    for i := 0 to (LReaders.Count -1) do
    begin
      LReaderClass := LReaders.Readers(i);

      //do test
      LReaderAccepted := LReaderClass.WantThis(AStream);

      //set to beginning stream
      AStream.Seek(LFirstStreamPosition, soFromBeginning);

      //do real dinner!
      if LReaderAccepted then
      begin
        LReader := LReaderClass.Create;
        try
          LReader.LoadFromStream(AStream, Self);
        finally
          LReader.Free;
        end;

        Break;
      end
    end;
  finally
    EndUpdate;
    Changed;    
  end;
end;

class function TgmGridBasedCollection.ReadersFilter: string;
var
  LFilters: string;
begin
  Self.GetFileReaders.BuildFilterStrings(TgmConverter, Result, LFilters);
end;

class procedure TgmGridBasedCollection.RegisterConverterReader(
  const AExtension, ADescription: string; const ADescID: Integer;
  const AReaderClass: TgmConverterClass);
begin
  Self.GetFileReaders.Add(AExtension, ADescription, ADescID, AReaderClass);
end;

class procedure TgmGridBasedCollection.RegisterConverterWriter(
  const AExtension, ADescription: string; const ADescID: Integer;
  const AWriterClass: TgmConverterClass);
begin
  Self.GetFileWriters.Add(AExtension, ADescription, ADescID, AWriterClass);
end;

procedure TgmGridBasedCollection.SaveToFile(const AFileName: string);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TgmGridBasedCollection.SaveToStream(const AStream: TStream);
begin

end;

procedure TgmGridBasedCollection.Update(AItem: TCollectionItem);
begin
  inherited Update(AItem);
  
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

class function TgmGridBasedCollection.WritersFilter: string;
var
  LFilters: string;
begin
  Self.GetFileWriters.BuildFilterStrings(TgmConverter, Result, LFilters);
end;

end.

