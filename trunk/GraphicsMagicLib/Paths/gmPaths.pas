unit gmPaths;

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
 * The Original Code is igChannels.pas.
 *
 * The Initial Developer of this unit are
 *   Ma Xiaoguang and Ma Xiaoming < gmbros[at]hotmail[dot]com >
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2016/12/27

interface

uses
{ Delphi }
  Classes,
  Contnrs,
{ Graphics32 }
  GR32,
{ GraphicsMagicLib }
  gmPenTools;

type
  TgmPathType = (ptWorkPath, ptNamedPath);

  { Forward Declarations }
  TgmPathList = class;

  { TgmPath }

  // The class TgmPath contains a pen path and the information about the path,
  // such as the path name and its thumbnail, etc.
  TgmPath = class(TObject)
  private
    FOwner         : TgmPathList;
    FPathName      : string;
    FNamed         : Boolean;          // Whether or not the path is named.
    FSelected      : Boolean;
    FThumbnail     : TBitmap32;
    FCurvePathList : TgmCurvePathList;
  protected
    function GetRealThumbRect(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer;
      const AMarginSize: Integer = 4): TRect;

    function GetThumbZoomScale(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;
  public
    constructor Create(AOwner: TgmPathList; const AType: TgmPathType);
    destructor Destroy; override;

    function GetCopy: TgmPath;

    procedure UpdateThumbnail(const AOriginalWidth, AOriginalHeight: Integer;
      const AOffsetVector: TPoint);

    property CurvePathList : TgmCurvePathList read FCurvePathList;
    property IsNamed       : Boolean          read FNamed    write FNamed;
    property IsSelected    : Boolean          read FSelected write FSelected;
    property PathName      : string           read FPathName write FPathName;
    property Thumbnail     : TBitmap32        read FThumbnail;
  end;

  { TgmPathList }

  TgmInsertPathEvent = procedure (ASender: TObject; const AIndex: Integer) of object;

  // The class TgmPathList holding a list of TgmPath object.
  TgmPathList = class(TObject)
  private
    FItems              : TObjectList;
    FSelectedPath       : TgmPath;
    FPathCounter        : Integer;

    FOnInsertPath       : TgmInsertPathEvent;
    FOnPathDeleted      : TNotifyEvent;
    FOnPathOrderChanged : TNotifyEvent;
    FOnSelectionChanged : TNotifyEvent;

    function GetPath(AIndex: Integer): TgmPath;
    function GetPathCount: Integer;
    function GetPathMaxIndex: Integer;
    function GetSelectedPathIndex: Integer;
    function GetWorkPathIndex: Integer;

    procedure SetPathCounter(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function ActivateWorkPath: TgmPath;  // activate and return Work Path
    function IsLastPathNamed: Boolean; // determine whether the last path is named
    function IsValidIndex(const AIndex: Integer): Boolean;
    function SimplyAdd(const APath: TgmPath): Integer;
    function SimplyInsert(APath: TgmPath; AIndex: Integer): Integer;

    procedure Add(const APath: TgmPath);
    procedure Clear;
    procedure DeselectAllPaths;
    procedure DeletePath(AIndex: Integer);
    procedure DeleteSelectedPath;
    procedure Insert(const APath: TgmPath; const AIndex: Integer);
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure SelectPath(const AIndex: Integer);
    procedure SimplyDelete(const AIndex: Integer);

    procedure UpdateAllPathThumbnails(
      const AOriginalWidth, AOriginalHeight: Integer;
      const AOffsetVector: TPoint);

    property Count                  : Integer            read GetPathCount;
    property MaxIndex               : Integer            read GetPathMaxIndex;
    property OnInsertPath           : TgmInsertPathEvent read FOnInsertPath       write FOnInsertPath;
    property OnPathDeleted          : TNotifyEvent       read FOnPathDeleted      write FOnPathDeleted;
    property OnPathOrderChanged     : TNotifyEvent       read FOnPathOrderChanged write FOnPathOrderChanged;
    property OnSelectionChanged     : TNotifyEvent       read FOnSelectionChanged write FOnSelectionChanged;
    property PathCounter            : Integer            read FPathCounter        write SetPathCounter;
    property Paths[AIndex: Integer] : TgmPath            read GetPath;
    property SelectedIndex          : Integer            read GetSelectedPathIndex;
    property SelectedPath           : TgmPath            read FSelectedPath;
  end;


const
  PATH_THUMB_SIZE = 36;


implementation

uses
{ Delphi }
  SysUtils,
  Graphics;


{ TgmPath }

constructor TgmPath.Create(AOwner: TgmPathList; const AType: TgmPathType);
begin
  inherited Create();

  FOwner         := AOwner;
  FNamed         := (AType = ptNamedPath);
  FPathName      := '';
  FSelected      := False;
  FCurvePathList := TgmCurvePathList.Create();

  if AType = ptWorkPath then
  begin
    FPathName := 'Work Path';
  end;

  FThumbnail := TBitmap32.Create();
  FThumbnail.SetSize(PATH_THUMB_SIZE, PATH_THUMB_SIZE);
end;

destructor TgmPath.Destroy;
begin
  FOwner := nil;

  FThumbnail.Free();
  FCurvePathList.Free();

  inherited;
end;

function TgmPath.GetCopy: TgmPath;
begin
  Result := TgmPath.Create(Self.FOwner, ptNamedPath);

  Result.PathName := Self.FPathName;
  Result.IsNamed  := Self.FNamed;

  Result.Thumbnail.Assign(Self.FThumbnail);
  Result.CurvePathList.AssignCurvePathListData(Self.FCurvePathList);
end;

function TgmPath.GetRealThumbRect(
  const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer;
  const AMarginSize: Integer = 4): TRect;
var
  LThumbWidth  : Integer;
  LThumbHeight : Integer;
  LScale       : Single;
begin
  LScale := GetThumbZoomScale(ASrcWidth, ASrcHeight,
    AThumbWidth - AMarginSize, AThumbHeight - AMarginSize);

  LThumbWidth  := Round(ASrcWidth  * LScale);
  LThumbHeight := Round(ASrcHeight * LScale);

  with Result do
  begin
    Left   := (PATH_THUMB_SIZE - LThumbWidth)  div 2;
    Top    := (PATH_THUMB_SIZE - LThumbHeight) div 2;
    Right  := Left + LThumbWidth;
    Bottom := Top  + LThumbHeight;
  end;
end;

function TgmPath.GetThumbZoomScale(
  const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;
var
  ws, hs : Single;
begin
  if (ASrcWidth <= AThumbWidth) and (ASrcHeight <= AThumbHeight) then
  begin
    Result := 1.0;
  end
  else
  begin
    ws := AThumbWidth  / ASrcWidth;
    hs := AThumbHeight / ASrcHeight;

    if ws < hs then
    begin
      Result := ws;
    end
    else
    begin
      Result := hs;
    end;
  end;
end;

procedure TgmPath.UpdateThumbnail(
  const AOriginalWidth, AOriginalHeight: Integer;
  const AOffsetVector: TPoint);
var
  LPathBitmap : TBitmap32;
  LRect       : TRect;
begin
  if (AOriginalWidth > 0) and (AOriginalHeight > 0) then
  begin
    LRect := Self.GetRealThumbRect(AOriginalWidth, AOriginalHeight,
                                   PATH_THUMB_SIZE, PATH_THUMB_SIZE);

    // filling the background
    LPathBitmap := TBitmap32.Create();
    try
      LPathBitmap.SetSize(AOriginalWidth, AOriginalHeight);
      LPathBitmap.Clear(clGray32);

      // drawing the path, filling regions and drawing outlines of curve paths
      with FCurvePathList do
      begin
        DrawAllPathRegionsForThumbnail(LPathBitmap.Canvas, AOffsetVector);
        DrawAllPathsForThumbnail(LPathBitmap.Canvas, AOffsetVector);
      end;

      // the thumbnail
      FThumbnail.Clear( Color32(clBtnFace) );
      FThumbnail.Draw(LRect, LPathBitmap.BoundsRect, LPathBitmap);

    finally
      LPathBitmap.Free();
    end;
  end;
end;


{ TgmPathList }

constructor TgmPathList.Create;
begin
  inherited;

  FSelectedPath       := nil;
  FPathCounter        := 0;
  FOnInsertPath       := nil;
  FOnPathDeleted      := nil;
  FOnPathOrderChanged := nil;
  FOnSelectionChanged := nil;

  FItems := TObjectList.Create(True);
end;

destructor TgmPathList.Destroy;
begin
  FItems.Clear();
  FItems.Free();

  inherited;
end;

function TgmPathList.ActivateWorkPath: TgmPath;
var
  LWorkPathIndex : Integer;
begin
  Result := nil;

  LWorkPathIndex := Self.GetWorkPathIndex();
  if LWorkPathIndex >= 0 then
  begin
    Self.SelectPath(LWorkPathIndex);
    Result := FSelectedPath;
  end;
end;

procedure TgmPathList.Add(const APath: TgmPath);
begin
  if Assigned(APath) then
  begin
    FItems.Add(APath);

    // give the path a default name
    if APath.IsNamed then
    begin
      Inc(FPathCounter);
      APath.PathName := 'Path ' + IntToStr(FPathCounter);
    end;

    SelectPath(FItems.Count - 1);

    if Assigned(FOnInsertPath) then
    begin
      FOnInsertPath(Self, FItems.Count - 1);
    end;
  end;
end;

procedure TgmPathList.Clear;
begin
  FItems.Clear();
end;

procedure TgmPathList.DeselectAllPaths;
var
  i     : Integer;
  LPath : TgmPath;
begin
  FSelectedPath := nil;

  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LPath := Self.GetPath(i);

      if LPath.IsSelected then
      begin
        LPath.IsSelected := False;
        LPath.CurvePathList.Status := cplsAddNewPath;
        LPath.CurvePathList.DeselectAllPaths();
      end;
    end; 
  end;
end;

procedure TgmPathList.DeletePath(AIndex: Integer);
var
  LPath : TgmPath;
begin
  if (FItems.Count <= 0) or ( not IsValidIndex(AIndex) ) then
  begin
    Exit;
  end;

  FSelectedPath := nil;
  LPath         := Self.Paths[AIndex];

  LPath.CurvePathList.Clear();
  FItems.Delete(AIndex);

  if FItems.Count > 0 then
  begin
    // select the previous path, if any ...
    AIndex := AIndex - 1;

    if AIndex < 0 then
    begin
      AIndex := 0;
    end;

    SelectPath(AIndex);
  end
  else
  begin
    // Invoke callback function, if any.
    // Even if we don't have any path in the list now,
    // after deletion, the path selection is also changed,
    // so we have to invoke the callback function as well.
    if Assigned(FOnSelectionChanged) then
    begin
      FOnSelectionChanged(Self);
    end;
  end;
end;

procedure TgmPathList.DeleteSelectedPath;
var
  LIndex : Integer;
begin
  LIndex := GetSelectedPathIndex();
  DeletePath(LIndex);

  if Assigned(FOnPathDeleted) then
  begin
    FOnPathDeleted(Self);
  end;
end;

function TgmPathList.GetPath(AIndex: Integer): TgmPath;
begin
  Result := nil;

  if IsValidIndex(AIndex) then
  begin
    Result := TgmPath(FItems.Items[AIndex]);
  end;
end;

function TgmPathList.GetPathCount: Integer;
begin
  Result := FItems.Count;
end;

function TgmPathList.GetPathMaxIndex: Integer;
begin
  Result := FItems.Count - 1;
end;

function TgmPathList.GetSelectedPathIndex: Integer;
var
  i : Integer;
begin
  Result := -1;

  if (FItems.Count > 0) and Assigned(FSelectedPath) then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      if FSelectedPath = Self.Paths[i] then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TgmPathList.GetWorkPathIndex: Integer;
var
  i     : Integer;
  LPath : TgmPath;
begin
  Result := -1;

  if FItems.Count > 0 then
  begin
    for i := (FItems.Count - 1) downto 0 do
    begin
      LPath := Self.GetPath(i);

      // A work path is the one that has not been named.
      if not LPath.IsNamed then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TgmPathList.Insert(const APath: TgmPath; const AIndex: Integer);
begin
  if Assigned(APath) and IsValidIndex(AIndex) then
  begin
    FItems.Insert(AIndex, APath);

    // give the path a default name
    if APath.IsNamed then
    begin
      Inc(FPathCounter);
      APath.PathName := 'Path ' + IntToStr(FPathCounter);
    end;

    SelectPath(AIndex);

    if Assigned(FOnInsertPath) then
    begin
      FOnInsertPath(Self, AIndex);
    end;
  end;
end;

// determine whether the last path is named
function TgmPathList.IsLastPathNamed: Boolean;
var
  LPath : TgmPath;
begin
  Result := False;

  if Self.Count > 0 then
  begin
    LPath  := Self.Paths[Self.MaxIndex];
    Result := LPath.IsNamed;
  end;
end;

function TgmPathList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

// This function does the similar thing as the Add() procedure above,
// but it won't select any added paths, given a default name to the added path,
// etc. It simply adds a path to the path list, and return the index of the
// added path back.
function TgmPathList.SimplyAdd(const APath: TgmPath): Integer;
begin
  Result := -1;
  
  if Assigned(APath) then
  begin
    FItems.Add(APath);
    Result := FItems.Count - 1;
  end;
end;

function TgmPathList.SimplyInsert(APath: TgmPath; AIndex: Integer): Integer;
begin
  Result := -1;
  
  if not Assigned(APath) then
  begin
    Exit;
  end;
  
  if AIndex < 0 then
  begin
    AIndex := 0;
  end
  else if AIndex > FItems.Count then
  begin
    AIndex := FItems.Count;
  end;

  FItems.Insert(AIndex, APath);
  Result := AIndex;
end;

procedure TgmPathList.Move(ACurIndex, ANewIndex: Integer);
begin
  if IsValidIndex(ACurIndex) and
     IsValidIndex(ANewIndex) and
     (ACurIndex <> ANewIndex) then
  begin
    FItems.Move(ACurIndex, ANewIndex);

    if Assigned(FOnPathOrderChanged) then
    begin
      FOnPathOrderChanged(Self);
    end;
  end;
end;

procedure TgmPathList.SelectPath(const AIndex: Integer);
var
  LPath : TgmPath;
begin
  LPath := GetPath(AIndex);

  if Assigned(LPath) then
  begin
    if FSelectedPath <> LPath then
    begin
      DeselectAllPaths();

      FSelectedPath            := LPath;
      FSelectedPath.IsSelected := True;

      // invoke callback function, if any
      if Assigned(FOnSelectionChanged) then
      begin
        FOnSelectionChanged(Self);
      end;
    end; 
  end;
end;

procedure TgmPathList.SetPathCounter(AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FPathCounter := AValue;
  end;
end;

procedure TgmPathList.SimplyDelete(const AIndex: Integer);
begin
  if IsValidIndex(AIndex) then
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TgmPathList.UpdateAllPathThumbnails(
  const AOriginalWidth, AOriginalHeight: Integer;
  const AOffsetVector: TPoint);
var
  i     : Integer;
  LPath : TgmPath;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LPath := Self.GetPath(i);
      LPath.UpdateThumbnail(AOriginalWidth, AOriginalHeight, AOffsetVector);
    end;
  end;
end;

end.
