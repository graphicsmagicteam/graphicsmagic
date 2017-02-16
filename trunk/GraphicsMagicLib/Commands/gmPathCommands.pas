unit gmPathCommands;

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
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
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

// Update Date: 2016/12/20

interface

{$WARN UNSAFE_CAST OFF}

uses
{ Delphi }
  Windows, SysUtils,
{ GraphicsMagicLib }
  gmHistoryCommands,
  gmPaths,
  gmPenTools;  // TgmCurvePathList

type
  TgmPathModificationMode = (pmmNone,
                             pmmNewAnchorPoint,
                             pmmPickupPath,
                             pmmAddAnchorPoint,
                             pmmDeleteAnchorPoint,
                             pmmNewPathComponent,
                             pmmChangeAnchorPoint,
                             pmmCornerDrag,
                             pmmDragAnchorPoint,
                             pmmDragControlPoint);

  { TgmNewWorkPathCommand }

  TgmNewWorkPathCommand = class(TgmCustomCommand)
  private
    FPathList       : TgmPathList;  // pointer to a path list
    FPathIndex      : Integer;
    FPath           : TgmPath;      // copy of a path
    FPathBackWidth  : Integer;      // for thumbnail update
    FPathBackHeight : Integer;
  public
    constructor Create(APathList: TgmPathList; APath: TgmPath;
      const APathIndex, APathBackWidth, APathBackHeight: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPathModificationCommand }

  TgmPathModificationCommand = class(TgmCustomCommand)
  private
    FPathList         : TgmPathList;       // pointer to a path list
    FModificationMode : TgmPathModificationMode;
    FPathBackWidth    : Integer;           // for thumbnail update
    FPathBackHeight   : Integer;
    FPathIndex        : Integer;
    FOldCurvePathList : TgmCurvePathList;  // copy of curve paths
    FNewCurvePathList : TgmCurvePathList;

    function GetCommandName(const AModificationMode: TgmPathModificationMode): string;

    procedure SetCommandIcon(const AModificationMode: TgmPathModificationMode);
  public
    constructor Create(APathList: TgmPathList; const APathIndex: Integer;
      AOldCurvePathList, ANewCurvePathList: TgmCurvePathList;
      const AModificationMode: TgmPathModificationMode;
      const APathBackWidth, APathBackHeight: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmClosePathCommand }

  TgmClosePathCommand = class(TgmCustomCommand)
  private
    FPathList         : TgmPathList;       // pointer to a path list
    FPathIndex        : Integer;
    FPathBackWidth    : Integer;           // for thumbnail update
    FPathBackHeight   : Integer;
    FOldCurvePathList : TgmCurvePathList;  // copy of curve paths
  public
    constructor Create(APathList: TgmPathList;
      const APathIndex: Integer; AOldCurvePathList: TgmCurvePathList;
      const APathBackWidth, APathBackHeight: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmActiveWorkPathCommand }

  TgmActiveWorkPathCommand = class(TgmCustomCommand)
  private
    FPathList         : TgmPathList;       // pointer to a path list
    FPathBackWidth    : Integer;           // for thumbnail update
    FPathBackHeight   : Integer;
    FOldCurvePathList : TgmCurvePathList;
    FNewCurvePath     : TgmCurvePath;
  public
    constructor Create(APathList: TgmPathList;
      AOldCurvePathList: TgmCurvePathList; ACurvePath: TgmCurvePath;
      const APathBackWidth, APathBackHeight: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewPathCommand }

  TgmNewPathCommand = class(TgmCustomCommand)
  private
    FPathList             : TgmPathList;  // pointer to a path list
    FOldSelectedPathIndex : Integer;
    FNewPathIndex         : Integer;
    FNewPath              : TgmPath;      // a copy of the new path
  public
    constructor Create(APathList: TgmPathList;
      const AOldSelectedPathIndex: Integer; ANewPath: TgmPath;
      const ANewPathIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeletePathCommand }

  TgmDeletePathCommand = class(TgmCustomCommand)
  private
    FPathList         : TgmPathList;  // pointer to a path list
    FDeletedPathIndex : Integer;
    FDeletedPath      : TgmPath;
  public
    constructor Create(APathList: TgmPathList; ADeletedPath: TgmPath;
      const ADeletedPathIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPathsTranslationCommand }

  TgmPathsTranslationCommand = class(TgmCustomCommand)
  private
    FPathList               : TgmPathList;  // pointer to a path list
    FPathIndex              : Integer;
    FPathBackWidth          : Integer;           // for thumbnail update
    FPathBackHeight         : Integer;
    FTranslatedVector       : TPoint;
    FSelectedPathIndexArray : array of Integer;
  public
    constructor Create(APathList: TgmPathList;
      const APathIndex: Integer; const ATranslatedVector: TPoint;
      const APathBackWidth, APathBackHeight: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;


const
  NEW_PATH_ICON_RES_NAME = 'NEWPATHICONMASK';
  

implementation

{$R gmPathCommandIcons.res }


{ TgmActiveWorkPathCommand }

constructor TgmActiveWorkPathCommand.Create(APathList: TgmPathList;
  AOldCurvePathList: TgmCurvePathList; ACurvePath: TgmCurvePath;
  const APathBackWidth, APathBackHeight: Integer);
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Create(): parameter APathList is a nil.');
  end;

  if not Assigned(AOldCurvePathList) then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Create(): parameter AOldCurvePathList is a nil.');
  end;

  if not Assigned(ACurvePath) then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Create(): parameter ACurvePath is a nil.');
  end;

  if APathBackWidth <= 0 then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Create(): parameter APathBackWidth should be greater than zero.');
  end;

  if APathBackHeight <= 0 then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Create(): parameter APathBackHeight should be greater than zero.');
  end;

  inherited Create('New Work Path');

  FPathList       := APathList;
  FPathBackWidth  := APathBackWidth;
  FPathBackHeight := APathBackHeight;
  FNewCurvePath   := ACurvePath.GetSelfBackup();
  
  FOldCurvePathList := TgmCurvePathList.Create();
  FOldCurvePathList.AssignCurvePathListData(AOldCurvePathList);
end;

destructor TgmActiveWorkPathCommand.Destroy;
begin
  FOldCurvePathList.Free();
  FNewCurvePath.Free();
  inherited;
end;

procedure TgmActiveWorkPathCommand.Execute;
var
  LPath      : TgmPath;
  LCurvePath : TgmCurvePath;
  LOffset    : TPoint;
begin
  inherited;

  FPathList.DeselectAllPaths();
  LPath := FPathList.ActivateWorkPath();

  if not Assigned(LPath) then
  begin
    raise Exception.Create('TgmActiveWorkPathCommand.Execute(): there was no a work path in the path list.');
  end;

  if Assigned(FPathList.SelectedPath) then
  begin
    LCurvePath := FNewCurvePath.GetSelfBackup();
    LPath.CurvePathList.DeleteAllCurvePaths();
    LPath.CurvePathList.AddNewPathToList(LCurvePath);

    LOffset.X := 0;
    LOffset.Y := 0;
    LPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
  end;
end;

procedure TgmActiveWorkPathCommand.Rollback;
var
  LPath   : TgmPath;
  LOffset : TPoint;
begin
  inherited;

  if FPathList.Count > 0 then
  begin
    FPathList.DeselectAllPaths();
    LPath := FPathList.ActivateWorkPath();

    if not Assigned(LPath) then
    begin
      raise Exception.Create('TgmActiveWorkPathCommand.Rollback(): there was no a work path in the path list.');
    end;

    LPath.CurvePathList.AssignCurvePathListData(FOldCurvePathList);

    LOffset.X := 0;
    LOffset.Y := 0;
    LPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
  end;
end;

{ TgmClosePathCommand }

constructor TgmClosePathCommand.Create(APathList: TgmPathList;
  const APathIndex: Integer; AOldCurvePathList: TgmCurvePathList;
  const APathBackWidth, APathBackHeight: Integer);
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmClosePathCommand.Create(): parameter APathList is a nil.');
  end;

  if not APathList.IsValidIndex(APathIndex) then
  begin
    raise Exception.Create('TgmClosePathCommand.Create(): parameter APathIndex is out of range.');
  end;

  if not Assigned(AOldCurvePathList) then
  begin
    raise Exception.Create('TgmClosePathCommand.Create(): parameter AOldCurvePathList is a nil.');
  end;

  if APathBackWidth <= 0 then
  begin
    raise Exception.Create('TgmClosePathCommand.Create(): parameter APathBackWidth should be greater than zero.');
  end;

  if APathBackHeight <= 0 then
  begin
    raise Exception.Create('TgmClosePathCommand.Create(): parameter APathBackHeight should be greater than zero.');
  end;

  inherited Create('Close Path');

  FPathList       := APathList;
  FPathIndex      := APathIndex;
  FPathBackWidth  := APathBackWidth;
  FPathBackHeight := APathBackHeight;
  
  FOldCurvePathList := TgmCurvePathList.Create();
  FOldCurvePathList.AssignCurvePathListData(AOldCurvePathList);
end;

destructor TgmClosePathCommand.Destroy;
begin
  FOldCurvePathList.Free();
  inherited;
end;

procedure TgmClosePathCommand.Execute;
var
  LOffset : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmClosePathCommand.Execute(): FPathIndex is out of range.');
  end;

  if FPathList.SelectedIndex <> FPathIndex then
  begin
    FPathList.SelectPath(FPathIndex);
  end;

  if FPathList.SelectedPath.CurvePathList.SelectedPathIndex <> FOldCurvePathList.SelectedPathIndex then
  begin
    FPathList.SelectedPath.CurvePathList.SelectPathByIndex(FOldCurvePathList.SelectedPathIndex);
  end;

  FPathList.SelectedPath.CurvePathList.SelectedPath.CurveSegmentsList.ClosePath();
  FPathList.SelectedPath.CurvePathList.Status := cplsAddNewPath;

  LOffset.X := 0;
  LOffset.Y := 0;
  FPathList.SelectedPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
end;

procedure TgmClosePathCommand.Rollback;
var
  LOffset : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmClosePathCommand.Rollback(): FPathIndex is out of range.');
  end;

  if FPathList.SelectedIndex <> FPathIndex then
  begin
    FPathList.SelectPath(FPathIndex);
  end;

  FPathList.SelectedPath.CurvePathList.AssignCurvePathListData(FOldCurvePathList);
  FPathList.SelectedPath.CurvePathList.SelectPathByIndex(FOldCurvePathList.SelectedPathIndex);

  LOffset.X := 0;
  LOffset.Y := 0;
  FPathList.SelectedPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
end;

{ TgmDeletePathCommand }

constructor TgmDeletePathCommand.Create(APathList: TgmPathList;
  ADeletedPath: TgmPath; const ADeletedPathIndex: Integer);
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmDeletePathCommand.Create(): parameter APathList is a nil.');
  end;

  if not Assigned(ADeletedPath) then
  begin
    raise Exception.Create('TgmDeletePathCommand.Create(): parameter ADeletedPath is a nil.');
  end;

  inherited Create('Delete Path');

  FPathList         := APathList;
  FDeletedPathIndex := ADeletedPathIndex;
  FDeletedPath      := ADeletedPath.GetCopy();
end;

destructor TgmDeletePathCommand.Destroy;
begin
  FDeletedPath.Free();
  inherited;
end;

procedure TgmDeletePathCommand.Execute;
var
  LSelectedIndex : Integer;
begin
  inherited;

  if not FPathList.IsValidIndex(FDeletedPathIndex) then
  begin
    raise Exception.Create('TgmDeletePathCommand.Execute(): FDeletedPathIndex is out of range.');
  end;

  FPathList.DeletePath(FDeletedPathIndex);

  if FPathList.Count > 0 then
  begin
    LSelectedIndex := FDeletedPathIndex - 1;
    if LSelectedIndex < 0 then
    begin
      LSelectedIndex := 0;
    end;

    FPathList.SelectPath(LSelectedIndex);
  end;
end;

procedure TgmDeletePathCommand.Rollback;
var
  LPath  : TgmPath;
  LIndex : Integer;
begin
  inherited;

  LPath  := FDeletedPath.GetCopy();
  LIndex := FPathList.SimplyInsert(LPath, FDeletedPathIndex);

  FPathList.SelectPath(LIndex);
end;

{ TgmNewPathCommand }

constructor TgmNewPathCommand.Create(APathList: TgmPathList;
  const AOldSelectedPathIndex: Integer; ANewPath: TgmPath;
  const ANewPathIndex: Integer);
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmNewPathCommand.Create(): parameter APathList is a nil.');
  end;

  if not Assigned(ANewPath) then
  begin
    raise Exception.Create('TgmNewPathCommand.Create(): parameter ANewPath is a nil.');
  end;

  if AOldSelectedPathIndex >= APathList.Count then
  begin
    raise Exception.Create('TgmNewPathCommand.Create(): parameter AOldSelectedPathIndex is beyond the range of the path list.');
  end;

  if not APathList.IsValidIndex(ANewPathIndex) then
  begin
    raise Exception.Create('TgmNewPathCommand.Create(): parameter ANewPathIndex is out of range.');
  end;

  inherited Create('New Path');

  FPathList             := APathList;
  FOldSelectedPathIndex := AOldSelectedPathIndex;
  FNewPathIndex         := ANewPathIndex;
  FNewPath              := ANewPath.GetCopy();
end;

destructor TgmNewPathCommand.Destroy;
begin
  FNewPath.Free();
  inherited;
end;

procedure TgmNewPathCommand.Execute;
var
  LNewPath   : TgmPath;
  LPathIndex : Integer;
begin
  inherited;

  LNewPath   := FNewPath.GetCopy();
  LPathIndex := FPathList.SimplyInsert(LNewPath, FNewPathIndex);

  FPathList.SelectPath(LPathIndex);
end;

procedure TgmNewPathCommand.Rollback;
begin
  inherited;

  if not FPathList.IsValidIndex(FNewPathIndex) then
  begin
    raise Exception.Create('TgmNewPathCommand.Rollback(): FNewPathIndex is out of range.');
  end;

  FPathList.SimplyDelete(FNewPathIndex);

  if FOldSelectedPathIndex >= 0 then
  begin
    if not FPathList.IsValidIndex(FOldSelectedPathIndex) then
    begin
      raise Exception.Create('TgmNewPathCommand.Rollback(): FOldSelectedPathIndex is out of range.');
    end;

    FPathList.SelectPath(FOldSelectedPathIndex);
  end;
end;

{ TgmNewWorkPathCommand }

constructor TgmNewWorkPathCommand.Create(APathList: TgmPathList;
  APath: TgmPath; const APathIndex, APathBackWidth, APathBackHeight: Integer);
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmNewWorkPathCommand.Create(): parameter APathList is a nil.');
  end;

  if not Assigned(APath) then
  begin
    raise Exception.Create('TgmNewWorkPathCommand.Create(): parameter APath is a nil.');
  end;

  if APathBackWidth <= 0 then
  begin
    raise Exception.Create('TgmNewWorkPathCommand.Create(): parameter APathBackWidth should be greater than zero.');
  end;

  if APathBackHeight <= 0 then
  begin
    raise Exception.Create('TgmNewWorkPathCommand.Create(): parameter APathBackHeight should be greater than zero.');
  end;

  inherited Create('New Work Path');

  FPathList       := APathList;
  FPath           := APath.GetCopy();
  FPathBackWidth  := APathBackWidth;
  FPathBackHeight := APathBackHeight;
  FPathIndex      := APathIndex;

  if FPathIndex < 0 then
  begin
    FPathIndex := 0;
  end
  else if FPathIndex > FPathList.Count then
  begin
    FPathIndex := FPathList.Count;
  end;

  ChangeCommandIconByResourceName(NEW_PATH_ICON_RES_NAME);
end;

destructor TgmNewWorkPathCommand.Destroy;
begin
  FPath.Free();
  inherited;
end;

procedure TgmNewWorkPathCommand.Execute;
var
  LPathCopy : TgmPath;
  LOffset   : TPoint;
begin
  inherited;

  LPathCopy := FPath.GetCopy();
  LOffset.X := 0;
  LOffset.Y := 0;
  LPathCopy.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
  FPathList.SimplyInsert(LPathCopy, FPathIndex);
  FPathList.SelectPath(FPathIndex);
end;

procedure TgmNewWorkPathCommand.Rollback;
begin
  inherited;

  FPathList.DeselectAllPaths();
  FPathList.SimplyDelete(FPathIndex);
end;

{ TgmPathModificationCommand }

constructor TgmPathModificationCommand.Create(
  APathList: TgmPathList; const APathIndex: Integer;
  AOldCurvePathList, ANewCurvePathList: TgmCurvePathList;
  const AModificationMode: TgmPathModificationMode;
  const APathBackWidth, APathBackHeight: Integer);
var
  LCommandName : string;
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter APathList is a nil.');
  end;

  if not APathList.IsValidIndex(APathIndex) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter APathIndex is out of range.');
  end;

  if AModificationMode = pmmNone then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): the value in parameter AModificationMode is pmmNone');
  end;

  if not Assigned(AOldCurvePathList) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter AOldCurvePathList is a nil.');
  end;

  if not Assigned(ANewCurvePathList) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter ANewCurvePathList is a nil.');
  end;

  if APathBackWidth <= 0 then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter APathBackWidth should be greater than zero.');
  end;

  if APathBackHeight <= 0 then
  begin
    raise Exception.Create('TgmPathModificationCommand.Create(): parameter APathBackHeight should be greater than zero.');
  end;

  LCommandName := GetCommandName(AModificationMode);
  inherited Create(LCommandName);
  
  FPathList         := APathList;
  FModificationMode := AModificationMode;
  FPathBackWidth    := APathBackWidth;
  FPathBackHeight   := APathBackHeight;
  FPathIndex        := APathIndex;

  FOldCurvePathList := TgmCurvePathList.Create();
  FOldCurvePathList.AssignCurvePathListData(AOldCurvePathList);
  
  FNewCurvePathList := TgmCurvePathList.Create();
  FNewCurvePathList.AssignCurvePathListData(ANewCurvePathList);

  SetCommandIcon(FModificationMode);
end;

destructor TgmPathModificationCommand.Destroy;
begin
  FOldCurvePathList.Free();
  FNewCurvePathList.Free();
  
  inherited;
end;

function TgmPathModificationCommand.GetCommandName(
  const AModificationMode: TgmPathModificationMode): string;
begin
  Result := '';
  
  case AModificationMode of
    pmmNewAnchorPoint:
      begin
        Result := 'New Anchor Point';
      end;

    pmmPickupPath:
      begin
        Result := 'Pick-up Path';
      end;
      
    pmmAddAnchorPoint:
      begin
        Result := 'Add Anchor Point';
      end;

    pmmDeleteAnchorPoint:
      begin
        Result := 'Delete Anchor Point';
      end;

    pmmNewPathComponent:
      begin
        Result := 'New Path Component';
      end;

    pmmChangeAnchorPoint:
      begin
        Result := 'Change Anchor Point';
      end;

    pmmCornerDrag:
      begin
        Result := 'Corner Drag';
      end;

    pmmDragAnchorPoint:
      begin
        Result := 'Drag Anchor Point';
      end;

    pmmDragControlPoint:
      begin
        Result := 'Drag Control Point';
      end;
  end;
end;

procedure TgmPathModificationCommand.Execute;
var
  LPath   : TgmPath;
  LOffset : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Execute(): FPathIndex is out of range.');
  end;

  LPath := FPathList.Paths[FPathIndex];
  LPath.CurvePathList.AssignCurvePathListData(FNewCurvePathList);

  LOffset.X := 0;
  LOffset.Y := 0;
  LPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);

  FPathList.SelectPath(FPathIndex);
  FPathList.SelectedPath.CurvePathList.SelectPathByIndex(FNewCurvePathList.SelectedPathIndex);
end;

procedure TgmPathModificationCommand.Rollback;
var
  LPath   : TgmPath;
  LOffset : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmPathModificationCommand.Rollback(): FPathIndex is out of range.');
  end;

  LPath := FPathList.Paths[FPathIndex];
  LPath.CurvePathList.AssignCurvePathListData(FOldCurvePathList);

  LOffset.X := 0;
  LOffset.Y := 0;
  LPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);

  FPathList.SelectPath(FPathIndex);
  FPathList.SelectedPath.CurvePathList.SelectPathByIndex(FOldCurvePathList.SelectedPathIndex);
end;

procedure TgmPathModificationCommand.SetCommandIcon(
  const AModificationMode: TgmPathModificationMode);
begin
  case AModificationMode of
//    pmmNewAnchorPoint,
//    pmmPickupPath,
//    pmmAddAnchorPoint,
//    pmmDeleteAnchorPoint,
    pmmNewPathComponent:
      begin
        ChangeCommandIconByResourceName(NEW_PATH_ICON_RES_NAME);
      end;
//    pmmChangeAnchorPoint,
//    pmmCornerDrag,
//    pmmDragAnchorPoint,
//    pmmDragControlPoint
  end;
end;

{ TgmPathsTranslationCommand }

constructor TgmPathsTranslationCommand.Create(APathList: TgmPathList;
  const APathIndex: Integer; const ATranslatedVector: TPoint;
  const APathBackWidth, APathBackHeight: Integer);
var
  i              : Integer;
  LSelectedCount : Integer;
  LIndex         : Integer;
  LPath          : TgmPath;
  LCurvePath     : TgmCurvePath;
begin
  if not Assigned(APathList) then
  begin
    raise Exception.Create('TgmPathsTranslationCommand.Create(): parameter APathList is a nil.');
  end;

  if not APathList.IsValidIndex(APathIndex) then
  begin
    raise Exception.Create('TgmPathsTranslationCommand.Create(): parameter APathIndex is out of range.');
  end;

  LPath          := APathList.Paths[APathIndex];
  LSelectedCount := LPath.CurvePathList.GetWholeSelectedPathsCount();
  
  if LSelectedCount <= 0 then
  begin
    raise Exception.Create('TgmPathsTranslationCommand.Create(): there is no any wholly selected path in APathList at index APathIndex.');
  end;

  inherited Create('Drag Paths');

  FPathList         := APathList;
  FPathIndex        := APathIndex;
  FTranslatedVector := ATranslatedVector;
  FPathBackWidth    := APathBackWidth;
  FPathBackHeight   := APathBackHeight;

  SetLength(FSelectedPathIndexArray, LSelectedCount);
  LIndex := 0;
  
  for i := 0 to (LPath.CurvePathList.Count - 1) do
  begin
    LCurvePath := TgmCurvePath(LPath.CurvePathList.Items[i]);

    if LCurvePath.CurveSegmentsList.IsSelectedAll then
    begin
      FSelectedPathIndexArray[LIndex] := i;
      LIndex := LIndex + 1;
    end;
  end;
end;

destructor TgmPathsTranslationCommand.Destroy;
begin
  SetLength(FSelectedPathIndexArray, 0);
  FSelectedPathIndexArray := nil;
  
  inherited;
end;

procedure TgmPathsTranslationCommand.Execute;
var
  i          : Integer;
  LIndex     : Integer;
  LCurvePath : TgmCurvePath;
  LOffset    : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmPathsTranslationCommand.Execute(): FPathIndex is out of range.');
  end;

  if FPathList.SelectedIndex <> FPathIndex then
  begin
    FPathList.SelectPath(FPathIndex);
  end;

  for i := 0 to High(FSelectedPathIndexArray) do
  begin
    LIndex := FSelectedPathIndexArray[i];

    if LIndex >= FPathList.SelectedPath.CurvePathList.Count then
    begin
      raise Exception.Create('TgmPathsTranslationCommand.Execute(): LIndex is out of range.');
    end;

    LCurvePath := TgmCurvePath(FPathList.SelectedPath.CurvePathList.Items[LIndex]);
    LCurvePath.CurveSegmentsList.TranslateAllSegments(FTranslatedVector);

    LOffset.X := 0;
    LOffset.Y := 0;
    FPathList.SelectedPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
  end;
end;

procedure TgmPathsTranslationCommand.Rollback;
var
  i                 : Integer;
  LIndex            : Integer;
  LCurvePath        : TgmCurvePath;
  LOffset           : TPoint;
  LTranslatedVector : TPoint;
begin
  inherited;

  if not FPathList.IsValidIndex(FPathIndex) then
  begin
    raise Exception.Create('TgmPathsTranslationCommand.Rollback(): FPathIndex is out of range.');
  end;

  if FPathList.SelectedIndex <> FPathIndex then
  begin
    FPathList.SelectPath(FPathIndex);
  end;

  LTranslatedVector.X := -FTranslatedVector.X;
  LTranslatedVector.Y := -FTranslatedVector.Y;

  for i := 0 to High(FSelectedPathIndexArray) do
  begin
    LIndex := FSelectedPathIndexArray[i];

    if LIndex >= FPathList.SelectedPath.CurvePathList.Count then
    begin
      raise Exception.Create('TgmPathsTranslationCommand.Rollback(): LIndex is out of range.');
    end;

    LCurvePath := TgmCurvePath(FPathList.SelectedPath.CurvePathList.Items[LIndex]);
    LCurvePath.CurveSegmentsList.TranslateAllSegments(LTranslatedVector);

    LOffset.X := 0;
    LOffset.Y := 0;
    FPathList.SelectedPath.UpdateThumbnail(FPathBackWidth, FPathBackHeight, LOffset);
  end;
end;

end.
