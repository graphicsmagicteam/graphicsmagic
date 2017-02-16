unit gmPathIO;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 *
 * The Original Code is gmPathIO.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/11/27

interface

uses
{ Delphi }
  Classes,
{ GraphicsMagicLib }
  gmPaths;

type
  { Paths Writer }

  TgmPathsWriter = class(TObject)
  private
    FPathList : TgmPathList;   // pointer to an outside path list
  public
    constructor Create(APathList: TgmPathList);

    procedure SaveToStream(AStream: TStream);
  end;
  

// load paths from a '*.gmd' file that is already loaded in a stream
function GMPLoadPathsFromStream(AStream: TStream; APathList: TgmPathList;
  const AVersionNum, ALoadCount: Cardinal): Boolean;

implementation

type
  TgmPathHeaderVer1 = record
    PathType : Cardinal;
    PathName : ShortString;
  end;

  { TgmCustomPathLoader }

  TgmCustomPathLoader = class(TObject)
  protected
    FPathList : TgmPathList;  // pointer to a path list
  public
    constructor Create(APathList: TgmPathList);
    destructor Destroy; override;

    function LoadPathsFromStream(AStream: TStream;
      const ALoadCount: Cardinal): Boolean; virtual; abstract;
  end;

  { TgmGMDPathLoader1 }

  TgmGMDPathLoader1 = class(TgmCustomPathLoader)
  public
    function LoadPathsFromStream(AStream: TStream;
      const ALoadCount: Cardinal): Boolean; override;
  end;


// load paths from a '*.gmd' file that is already loaded in a stream
function GMPLoadPathsFromStream(AStream: TStream; APathList: TgmPathList;
  const AVersionNum, ALoadCount: Cardinal): Boolean;
var
  LPathLoader : TgmCustomPathLoader;
begin
  Result := False;

  if (not Assigned(AStream)) or
     (AStream.Size <= 0) or
     (ALoadCount = 0) then
  begin
    Exit;
  end;

  if AVersionNum in [1, 2, 3, 4] then
  begin
    // The path data structure is identical in a .gmd file with those versions.
    LPathLoader := TgmGMDPathLoader1.Create(APathList);
    try
      Result := LPathLoader.LoadPathsFromStream(AStream, ALoadCount);
    finally
      LPathLoader.Free();
    end;
  end;
end;


{ TgmCustomPathLoader }

constructor TgmCustomPathLoader.Create(APathList: TgmPathList);
begin
  inherited Create();

  FPathList := APathList;
end;

destructor TgmCustomPathLoader.Destroy;
begin
  FPathList := nil;

  inherited;
end;


{ TgmGMDPathLoader1 }

function TgmGMDPathLoader1.LoadPathsFromStream(AStream: TStream;
  const ALoadCount: Cardinal): Boolean;
var
  LPathHeader : TgmPathHeaderVer1;
  LPath       : TgmPath;
  LOldCount   : Integer;
  i           : Integer;
begin
  Result := False;

  if Assigned(AStream) and
     Assigned(FPathList) and
     (AStream.Size > 0) and
     (ALoadCount > 0) then
  begin
    LOldCount := FPathList.Count;

    for i := 1 to ALoadCount do
    begin
      AStream.Read( LPathHeader, SizeOf(TgmPathHeaderVer1) );

      LPath := TgmPath.Create( FPathList, TgmPathType(LPathHeader.PathType) );
      with LPath do
      begin
        PathName := LPathHeader.PathName;

        CurvePathList.LoadCurvePathsFromStream(TMemoryStream(AStream), 1);
      end;

      FPathList.SimplyAdd(LPath);
    end;

    Result := ( (FPathList.Count - LOldCount) > 0 );
  end;
end;

{ Paths Writer }

constructor TgmPathsWriter.Create(APathList: TgmPathList);
begin
  inherited Create();

  FPathList := APathList;
end;

procedure TgmPathsWriter.SaveToStream(AStream: TStream);
var
  i           : Integer;
  LPath       : TgmPath;
  LPathHeader : TgmPathHeaderVer1;
begin
  if not Assigned(AStream) then
  begin
    Exit;
  end;

  if Assigned(FPathList) then
  begin
    if FPathList.Count > 0 then
    begin
      for i := 0 to (FPathList.Count - 1) do
      begin
        LPath := FPathList.Paths[i];

        // saving path header first ...
        if LPath.IsNamed then
        begin
          LPathHeader.PathType := Ord(ptNamedPath);
        end
        else
        begin
          LPathHeader.PathType := Ord(ptWorkPath);
        end;

        LPathHeader.PathName := LPath.PathName;

        AStream.Write( LPathHeader, SizeOf(TgmPathHeaderVer1) );

        // then saving the data of curve paths ...
        LPath.CurvePathList.SaveCurvePathsToStream( TMemoryStream(AStream) );
      end;
    end;
  end;
end;

end.
