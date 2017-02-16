unit gmFileFormatList;

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
  SysUtils, Classes;

type
  TgmVersionRec = packed record
    case Integer of
      0: (AsLongWord : Cardinal);
      1: (AsInteger  : Integer);
      2: (BEMajor, BEMinor: SmallInt);// BigEndian
      3: (Minor, Major: Word);        // LitteEndian
      4: (Words: array [0..1] of Word);
      5: (Bytes: array [0..3] of Byte);
  end;

  TgmFileHeader = record
    FileID       : Cardinal;       // magic number for file ID
    Version      : TgmVersionRec;  // version of the stream/file format
    ItemCount    : Cardinal;       // indicating how many items are in this file
  end;

  TgmConverter = class(TObject)
  public
    constructor Create; virtual;
    class function WantThis(const AStream: TStream): Boolean; virtual; abstract;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); virtual; abstract;
    procedure LoadItemFromStream(const AStream: TStream; const AItem: TCollectionItem); virtual; abstract;
    procedure SaveToStream(const AStream: TStream; const ACollection: TCollection); virtual; abstract;
    procedure SaveItemToStream(const AStream: TStream; const AItem: TCollectionItem); virtual; abstract;
  end;

  TgmConverterClass = class of TgmConverter;


  PgmFileFormat = ^TgmFileFormat;
  TgmFileFormat = record
    ConverterClass : TgmConverterClass;
    Extension      : string;
    Description    : string;
    DescResID      : Integer;
  end;

  
  TgmFileFormatsList = class(TList)
  private
    FShowAllExts : Boolean;
    
    function AvailableClass(const AClass: TgmConverterClass): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Add(const AExt, ADesc: string; const ADescID: Integer;
      const AClass: TgmConverterClass);

    procedure Remove(AClass: TgmConverterClass);

    procedure BuildFilterStrings(const AConverterClass: TgmConverterClass;
      var ADescriptions, AFilters: string);
    
    function FindExt(const AExt: string): TgmConverterClass;
    function FindClassName(const AClassname: string): TgmConverterClass;
    function Readers(const AIndex: Integer): TgmConverterClass;

    property ShowAllExts : Boolean read FShowAllExts write FShowAllExts;
  end;
  

implementation

uses
  Consts;

constructor TgmFileFormatsList.Create;
begin
  inherited Create;
  
  FShowAllExts := True;
  //Add('wmf', SVMetafiles, 0, TMetafile);
  //Add('emf', SVEnhMetafiles, 0, TMetafile);
  //Add('ico', SVIcons, 0, TIcon);
  //Add('bmp', SVBitmaps, 0, TBitmap);
end;

destructor TgmFileFormatsList.Destroy;
var
  I : Integer;
begin
  for I := (Count - 1) downto 0 do
    Dispose( PgmFileFormat(Items[I]) );
    
  inherited Destroy;
end;

procedure TgmFileFormatsList.Add(const AExt, ADesc: string;
  const ADescID: Integer; const AClass: TgmConverterClass);
var
  LNewRec : PgmFileFormat;
begin
  New(LNewRec);
  
  with LNewRec^ do
  begin
    Extension      := AnsiLowerCase(AExt);
    ConverterClass := AClass;
    Description    := ADesc;
    DescResID      := ADescID;
  end;
  
  inherited Add(LNewRec);
end;

function TgmFileFormatsList.FindExt(const AExt: string): TgmConverterClass;
var
  I    : Integer;
  LExt : string;
begin
  LExt := AnsiLowerCase(AExt);
  
  for I := (Count - 1) downto 0 do
    with PgmFileFormat(Items[I])^ do
      if Extension = LExt then
      begin
        Result := ConverterClass;
        Exit;
      end;
      
  Result := nil;
end;

function TgmFileFormatsList.FindClassName(
  const AClassName: string): TgmConverterClass;
var
  I : Integer;
begin
  for I := (Count - 1) downto 0 do
  begin
    Result := PgmFileFormat(Items[I])^.ConverterClass;
    
    if Result.ClassName = AClassName then
    begin
      Exit;
    end;
  end;
  
  Result := nil;
end;

function TgmFileFormatsList.AvailableClass(
  const AClass: TgmConverterClass): Boolean;
var
  I : Integer;
begin
  Result := False;
  
  for I := (Count - 1) downto 0 do
    if AClass = PgmFileFormat(Items[I])^.ConverterClass then
    begin
      Result := True;
      Break;
    end;
end;

procedure TgmFileFormatsList.Remove(AClass: TgmConverterClass);
var
  I : Integer;
  P : PgmFileFormat;
begin
  for I := (Count - 1) downto 0 do
  begin
    P := PgmFileFormat(Items[I]);
    
    if P^.ConverterClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

procedure TgmFileFormatsList.BuildFilterStrings(
  const AConverterClass: TgmConverterClass;
  var ADescriptions, AFilters: string);
var
  C, I  : Integer;
  P     : PgmFileFormat;
  LExts : TStringList;
begin
  LExts := TStringList.Create;
  try
    ADescriptions := '';
    AFilters      := '';
    C             := 0;
  
    for I := (Count - 1) downto 0 do
    begin
      P := PgmFileFormat(Items[I]);
      
      if P^.ConverterClass.InheritsFrom(AConverterClass) and
         (P^.Extension <> '') then
      begin
        with P^ do
        begin
          if C <> 0 then
          begin
            ADescriptions := ADescriptions + '|';
            
            if LExts.IndexOf(Extension) < 0 then
              AFilters := AFilters + ';';
          end;
          
          if (Description = '') and (DescResID <> 0) then
          begin
            Description := LoadStr(DescResID);
          end;
          
          FmtStr(ADescriptions, '%s%s (*.%s)|*.%2:s',
                 [ADescriptions, Description, Extension]);

          if LExts.IndexOf(Extension) < 0 then
          begin
            FmtStr(AFilters, '%s*.%s', [AFilters, Extension]);
            LExts.Add(Extension)
          end;
          
          Inc(C);
        end;
      end;
    end;

  finally
    LExts.Free;
  end;
  
  if FShowAllExts and (C > 1) then
  begin
    FmtStr(ADescriptions, '%s (%s)|%1:s|%s',
           [{sAllFilter}'All supported files', AFilters, ADescriptions]);
  end;
end;

function TgmFileFormatsList.Readers(const AIndex: Integer): TgmConverterClass;
var
  P : PgmFileFormat;
begin
  //unsafe!!!!!
  P      := PgmFileFormat(Items[AIndex]);
  Result := P^.ConverterClass;
end;

{ TgmConverter }

constructor TgmConverter.Create;
begin

end;

end.
