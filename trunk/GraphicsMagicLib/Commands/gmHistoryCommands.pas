unit gmHistoryCommands;

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

// Update Date: 2017/02/14

interface

uses
{ Delphi }
  Windows, SysUtils, Graphics, Contnrs, Classes,
{ Graphics32 }
  GR32;
  

type
  TgmCommandTypeToggle = (cttNone, cttUndo, cttRedo);


  { forward declarations }
  TgmCommandList = class;

  { TgmSnapshot }

  TgmSnapshot = class(TObject)
  protected
    FName          : string;
    FSelected      : Boolean;
    FThumbnail     : TBitmap32;  // thumbnail of the snapshot
    FOnThumbUpdate : TNotifyEvent;

    function GetRealThumbRect(ASrcBitmap: TCustomBitmap32;
      const AMarginSize: Integer = 4): TRect;

    function GetThumbScale(
      const ASrcWidth, ASrcHeight, AThumbWidth, AThumbHeight: Integer): Single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateThumnail(ABitmap: TBitmap32); virtual;

    property Name              : string       read FName          write FName;
    property OnThumbnailUpdate : TNotifyEvent read FOnThumbUpdate write FOnThumbUpdate;
    property IsSelected        : Boolean      read FSelected      write FSelected;
    property Thumbnail         : TBitmap32    read FThumbnail;
  end;
  

  { TgmSnapshotList }

  TgmSnapshotList = class(TObject)
  protected
    FItems            : TObjectList;
    FSelectedIndex    : Integer;
    FSelectedSnapshot : TgmSnapshot;

    function GetSnapshot(AIndex: Integer): TgmSnapshot;
    function GetSnapshotCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ASnapshot: TgmSnapshot): Integer;
    function IsValidIndex(const AIndex: Integer): Boolean;

    procedure DeselectAllSnapshots();
    procedure SelectSnapshot(const AIndex: Integer);

    property Count                      : Integer     read GetSnapshotCount;
    property SelectedIndex              : Integer     read FSelectedIndex;
    property SelectedSnapshot           : TgmSnapshot read FSelectedSnapshot;
    property Snapshots[AIndex: Integer] : TgmSnapshot read GetSnapshot;
  end;


  { TgmCustomCommand }

  TgmCustomCommand = class(TObject)
  protected
    FOwner       : TgmCommandList;
    FIcon        : TBitmap32;  // command icon
    FIconMask    : TBitmap32;  // note that, this is a mask, we have to render the icon with this mask
    FCommandName : string;
    FExecuted    : Boolean;    // if this command is executed
    FSelected    : Boolean;
  public
    constructor Create(const ACommandName: string);
    destructor Destroy; override;

    procedure ChangeCommandIconByResourceName(const AResourceName: string);
    procedure Execute; virtual;
    procedure Rollback; virtual;

    property CommandName : string         read FCommandName;
    property Icon        : TBitmap32      read FIcon;
    property IconMask    : TBitmap32      read FIconMask;
    property IsExecuted  : Boolean        read FExecuted write FExecuted;
    property IsSelected  : Boolean        read FSelected write FSelected;
    property Owner       : TgmCommandList read FOwner    write FOwner;
  end;


  { TgmImageProcessCommand }
  
  TgmImageProcessCommand = class(TgmCustomCommand)
  protected
    FUndoBmpFileName : string;
    FRedoBmpFileName : string;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32);

    destructor Destroy; override;
  end;
  

  { TgmByteMapProcessCommand }

  TgmByteMapProcessCommand = class(TgmCustomCommand)
  protected
    FUndoMapFileName : string;
    FRedoMapFileName : string;
    FRedoMapWidth    : Integer;
    FRedoMapHeight   : Integer;
    FUndoMapWidth    : Integer;
    FUndoMapHeight   : Integer;
  public
    constructor Create(const ACommandName: string;
      const AUndoBmp, ARedoBmp: TBitmap32);

    destructor Destroy; override;
  end;


  { TgmCommandList }

  TgmCommandList = class(TObject)
  protected
    FItems           : TObjectList;
    FSelectedCommand : TgmCustomCommand;
    FSelectedIndex   : Integer;

    function GetCommand(AIndex: Integer): TgmCustomCommand;
    function GetCommandCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ACommand: TgmCustomCommand): Integer;
    function Insert(AIndex: Integer; ACommand: TgmCustomCommand): Integer;
    function IsValidIndex(const AIndex: Integer): Boolean;

    procedure Delete(const AIndex: Integer);
    procedure DeselectAllCommands();
    procedure SelectCommand(const AIndex: Integer);

    property Commands[AIndex: Integer] : TgmCustomCommand read GetCommand;
    property Count                     : Integer          read GetCommandCount;
    property SelectedCommand           : TgmCustomCommand read FSelectedCommand;
    property SelectedIndex             : Integer          read FSelectedIndex;
  end;
  

  { TgmCommandManager }

  TgmOnSelectedCommandChanged = procedure (ACommandList: TgmCommandList;
                                           ASelectedCommand: TgmCustomCommand;
                                           const AOldIndex, ACurrIndex: Integer) of object;

  TgmOnSelectedSnapshotChanged = procedure (ASnapshotList: TgmSnapshotList;
                                            ASelectedSnapshot: TgmSnapshot;
                                            const AOldIndex, ACurrIndex: Integer) of object;

  TgmCommandManager = class(TObject)
  private
    FSnapshotList              : TgmSnapshotList;
    FCommandList               : TgmCommandList;
    FCommandTypeToggle         : TgmCommandTypeToggle;  // switch back and forth between Undo and Redo
    FCommandMaxCount           : Integer;

    FOnCommandAdded            : TNotifyEvent;
    FOnSelectedCommandChanged  : TgmOnSelectedCommandChanged;
    FOnSelectedSnapshotChanged : TgmOnSelectedSnapshotChanged;
    FOnSnapshotAdded           : TNotifyEvent;

    procedure SetCommandMaxCount(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function AddCommand(ACommand: TgmCustomCommand): Integer;
    function AddSnapshot(ASnapshotObject: TgmSnapshot): Integer; overload;
    function AddSnapshot(ASnapshotBitmap: TBitmap32; const ASnapshotName: string): Integer; overload;
    function RedoCommands(const ALowerIndex, AHigherIndex: Integer): Boolean;
    function UndoCommands(const AHigherIndex, ALowerIndex: Integer): Boolean;

    procedure DeleteCommandsFromSpecifiedIndex(const AIndex: Integer);
    procedure SelectCommand(const AIndex: Integer);
    procedure SimplySelectCommand(const AIndex: Integer);
    procedure SelectSnapshot(const AIndex: Integer);

    property CommandList               : TgmCommandList               read FCommandList;
    property CommandMaxCount           : Integer                      read FCommandMaxCount write SetCommandMaxCount;
    property CommandTypeToggle         : TgmCommandTypeToggle         read FCommandTypeToggle;
    property OnCommandAdded            : TNotifyEvent                 read FOnCommandAdded            write FOnCommandAdded;
    property OnSelectedCommandChanged  : TgmOnSelectedCommandChanged  read FOnSelectedCommandChanged  write FOnSelectedCommandChanged;
    property OnSelectedSnapshotChanged : TgmOnSelectedSnapshotChanged read FOnSelectedSnapshotChanged write FOnSelectedSnapshotChanged;
    property OnSnapshotAdded           : TNotifyEvent                 read FOnSnapshotAdded           write FOnSnapshotAdded;
    property SnapshotList              : TgmSnapshotList              read FSnapshotList;
  end;


const
  SNAPSHOT_THUMB_SIZE = 36;
  COMMAND_ICON_SIZE   = 18;

  DEFAULT_COMMAND_ICON_RES_NAME     = 'DEFAULTCOMMANDICONMASK';
  PAINT_BRUSH_COMMAND_ICON_RES_NAME = 'PAINTBRUSHCOMMANDICONMASK';
  MOVE_COMMAND_ICON_RES_NAME        = 'MOVECOMMANDICONMASK';

var
  // A directory for holding temporary files for commands.
  COMMAND_DATA_DIR : string = '';


implementation

uses
{ Graphics32 }
  GR32_OrdinalMaps,
{ GraphicsMagiclib }
  gmPaintFuncs;


{$R gmDefaultCommandIcons.res}


{ TgmByteMapProcessCommand }

constructor TgmByteMapProcessCommand.Create(const ACommandName: string;
  const AUndoBmp, ARedoBmp: TBitmap32);
var
  i             : Integer;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  inherited Create(ACommandName);

  // Storing the undo/redo bitmaps to disk for saving the system memory.
  // We convert the bitmap to Byte Map first and then save the Byte Map
  // to disk for saving disk space. 
  // 
  // The file name we are using is randomly generated and without
  // a file extension.

  LRandomString := IntToStr( GetTickCount() );

  if Assigned(AUndoBmp) then
  begin
    FUndoMapFileName := COMMAND_DATA_DIR + '\Undo' + LRandomString;
  end
  else
  begin
    FUndoMapFileName := '';
  end;

  if Assigned(ARedoBmp) then
  begin
    FRedoMapFileName := COMMAND_DATA_DIR + '\Redo' + LRandomString;
  end
  else
  begin
    FRedoMapFileName := '';
  end;

  FUndoMapWidth  := 0;
  FUndoMapHeight := 0;
  FRedoMapWidth  := 0;
  FRedoMapHeight := 0;

  LDataStream := TMemoryStream.Create();
  try
    if Assigned(AUndoBmp) and
       (AUndoBmp.Width > 0) and
       (AUndoBmp.Height > 0) then
    begin
      FUndoMapWidth  := AUndoBmp.Width;
      FUndoMapHeight := AUndoBmp.Height;

      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(FUndoMapWidth, FUndoMapHeight);
        LByteMap.ReadFrom(AUndoBmp, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
        // save byte map to a stream
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Write(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LDataStream.Position := 0;
        LDataStream.SaveToFile(FUndoMapFileName);
      finally
        LByteMap.Free();
      end;
    end;

    LDataStream.Clear();

    if Assigned(ARedoBmp) and
       (ARedoBmp.Width > 0) and
       (ARedoBmp.Height > 0) then
    begin
      FRedoMapWidth  := ARedoBmp.Width;
      FRedoMapHeight := ARedoBmp.Height;

      LByteMap := TByteMap.Create();
      try
        LByteMap.SetSize(FRedoMapWidth, FRedoMapHeight);
        LByteMap.ReadFrom(ARedoBmp, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
        // save byte map to a stream
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Write(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LDataStream.Position := 0;
        LDataStream.SaveToFile(FRedoMapFileName);
      finally
        LByteMap.Free();
      end;
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmByteMapProcessCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FUndoMapFileName) then
  begin
    DeleteFile(PChar(FUndoMapFileName));
  end;

  if FileExists(FRedoMapFileName) then
  begin
    DeleteFile(PChar(FRedoMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;


{ TgmCommandList }

constructor TgmCommandList.Create;
begin
  inherited;

  FItems           := TObjectList.Create();
  FSelectedCommand := nil;
  FSelectedIndex   := -1;
end;

destructor TgmCommandList.Destroy;
begin
  FItems.Free();
  inherited;
end;

function TgmCommandList.Add(ACommand: TgmCustomCommand): Integer;
begin
  if Assigned(ACommand) then
  begin
    ACommand.Owner := Self;
    Result         := FItems.Add(ACommand);
  end
  else
  begin
    Result := -1;
  end;
end;

procedure TgmCommandList.Delete(const AIndex: Integer);
begin
  if IsValidIndex(AIndex) then
  begin
    FItems.Delete(AIndex);

    if AIndex = FSelectedIndex then
    begin
      FSelectedIndex   := -1;
      FSelectedCommand := nil;
    end;
  end;
end;

procedure TgmCommandList.DeselectAllCommands();
var
  i        : Integer;
  LCommand : TgmCustomCommand;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LCommand           := TgmCustomCommand(FItems.Items[i]);
      LCommand.FSelected := False;
    end;

    FSelectedCommand := nil;
    FSelectedIndex   := -1;
  end;
end;

function TgmCommandList.GetCommand(AIndex: Integer): TgmCustomCommand;
begin
  if IsValidIndex(AIndex) then
  begin
    Result := TgmCustomCommand(FItems.Items[AIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TgmCommandList.GetCommandCount: Integer;
begin
  Result := FItems.Count;
end;

function TgmCommandList.Insert(AIndex: Integer;
  ACommand: TgmCustomCommand): Integer;
begin
  if Assigned(ACommand) then
  begin
    if AIndex < 0 then
    begin
      AIndex := 0;
    end
    else if AIndex > FItems.Count then
    begin
      AIndex := FItems.Count;
    end;

    ACommand.Owner := Self;
    FItems.Insert(AIndex, ACommand);
    Result := AIndex;
  end
  else
  begin
    Result := -1;
  end;
end;

function TgmCommandList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

procedure TgmCommandList.SelectCommand(const AIndex: Integer);
begin
  if not IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  DeselectAllCommands();
  
  FSelectedIndex              := AIndex;
  FSelectedCommand            := TgmCustomCommand(FItems.Items[AIndex]);
  FSelectedCommand.IsSelected := True;
end;


{ TgmCommandManager }

constructor TgmCommandManager.Create;
begin
  inherited;
  
  FSnapshotList := TgmSnapshotList.Create();
  FCommandList  := TgmCommandList.Create();

  FCommandMaxCount   := 20;
  FCommandTypeToggle := cttNone;

  FOnCommandAdded            := nil;
  FOnSelectedCommandChanged  := nil;
  FOnSelectedSnapshotChanged := nil;
  FOnSnapshotAdded           := nil;
end;

destructor TgmCommandManager.Destroy;
begin
  FCommandList.Free();
  FSnapshotList.Free();

  inherited;
end;

function TgmCommandManager.AddCommand(ACommand: TgmCustomCommand): Integer;
var
  i, LIndex : Integer;
begin
  LIndex := FCommandList.SelectedIndex + 1;
  Result := FCommandList.Insert(LIndex, ACommand);

  if FCommandList.Count > FCommandMaxCount then
  begin
    FCommandList.Delete(0);
    Result := FCommandList.Count - 1;
  end;

  FCommandTypeToggle := cttRedo; 

  if Result >= 0 then
  begin
    FSnapshotList.DeselectAllSnapshots();
    FCommandList.DeselectAllCommands();
    FCommandList.SelectCommand(Result);

    // delete the commands that followed the new inserted one
    if FCommandList.SelectedIndex < (FCommandList.Count - 1) then
    begin
      for i := (FCommandList.Count - 1) downto (FCommandList.SelectedIndex + 1) do
      begin
        FCommandList.Delete(i);
      end;
    end;

    if Assigned(FOnCommandAdded) then
    begin
      FOnCommandAdded(Self);
    end;
  end;
end;

function TgmCommandManager.AddSnapshot(ASnapshotObject: TgmSnapshot): Integer;
begin
  Result := FSnapshotList.Add(ASnapshotObject);

  if (Result >= 0) and Assigned(FOnSnapshotAdded) then
  begin
    FOnSnapshotAdded(Self);
  end;
end;

function TgmCommandManager.AddSnapshot(ASnapshotBitmap: TBitmap32;
  const ASnapshotName: string): Integer;
var
  LSnapshot : TgmSnapshot;
begin
  LSnapshot      := TgmSnapshot.Create();
  LSnapshot.Name := ASnapshotName;
  
  if Assigned(ASnapshotBitmap) then
  begin
    LSnapshot.UpdateThumnail(ASnapshotBitmap);
  end;

  Result := FSnapshotList.Add(LSnapshot);
end;

function TgmCommandManager.RedoCommands(
  const ALowerIndex, AHigherIndex: Integer): Boolean;
var
  i        : Integer;
  LCommand : TgmCustomCommand;
begin
  Result := False;
  
  if FCommandList.Count > 0 then
  begin
    if FCommandList.IsValidIndex(ALowerIndex) and
       FCommandList.IsValidIndex(AHigherIndex) and
       (ALowerIndex <= AHigherIndex) then
    begin
      for i := ALowerIndex to AHigherIndex do
      begin
        LCommand := FCommandList.Commands[i];
        LCommand.Execute();
      end;

      FCommandTypeToggle := cttRedo;
      Result             := True;
    end;
  end;
end;

procedure TgmCommandManager.DeleteCommandsFromSpecifiedIndex(
  const AIndex: Integer);
var
  i : Integer;
begin
  if FCommandList.IsValidIndex(AIndex) then
  begin
    for i := (FCommandList.Count - 1) downto AIndex do
    begin
      FCommandList.Delete(i);
    end;
  end;
end;

procedure TgmCommandManager.SelectCommand(const AIndex: Integer);
var
  LOldIndex : Integer;
begin
  if FCommandList.IsValidIndex(AIndex) then
  begin
    if FCommandList.SelectedIndex <> AIndex then
    begin
      LOldIndex := FCommandList.SelectedIndex;
      FCommandList.SelectCommand(AIndex);

      if Assigned(FOnSelectedCommandChanged) then
      begin
        FOnSelectedCommandChanged(FCommandList,
                                  FCommandList.SelectedCommand,
                                  LOldIndex, FCommandList.SelectedIndex);
      end;
    end;
  end;
end;

procedure TgmCommandManager.SelectSnapshot(const AIndex: Integer);
var
  LOldIndex : Integer;
begin
  if FSnapshotList.IsValidIndex(AIndex) then
  begin
    if FSnapshotList.SelectedIndex <> AIndex then
    begin
      LOldIndex := FSnapshotList.SelectedIndex;
      FSnapshotList.SelectSnapshot(AIndex);

      if Assigned(FOnSelectedSnapshotChanged) then
      begin
        FOnSelectedSnapshotChanged(FSnapshotList,
                                   FSnapshotList.SelectedSnapshot,
                                   LOldIndex, FSnapshotList.SelectedIndex);
      end;
    end;
  end;
end;

procedure TgmCommandManager.SetCommandMaxCount(AValue: Integer);
begin
  if (AValue > 0) and (AValue <> FCommandMaxCount) then
  begin
    FCommandMaxCount := AValue;
  end;
end;

// no callback version of SelectCommand()
procedure TgmCommandManager.SimplySelectCommand(const AIndex: Integer);
begin
  if FSnapshotList.IsValidIndex(AIndex) then
  begin
    if FSnapshotList.SelectedIndex <> AIndex then
    begin
      FSnapshotList.SelectSnapshot(AIndex);
    end;
  end;
end;

function TgmCommandManager.UndoCommands(
  const AHigherIndex, ALowerIndex: Integer): Boolean;
var
  i        : Integer;
  LCommand : TgmCustomCommand;
begin
  Result := False;
  
  if FCommandList.Count > 0 then
  begin
    if FCommandList.IsValidIndex(AHigherIndex) and
       FCommandList.IsValidIndex(ALowerIndex) and
       (AHigherIndex >= ALowerIndex) then
    begin
      for i := AHigherIndex downto ALowerIndex do
      begin
        LCommand := FCommandList.Commands[i];
        LCommand.Rollback();
      end;

      FCommandTypeToggle := cttUndo;
      Result             := True;
    end;
  end;
end;

{ TgmCustomCommand }

constructor TgmCustomCommand.Create(const ACommandName: string);
begin
  inherited Create();

  FOwner       := nil;
  FCommandName := ACommandName;
  FExecuted    := False;
  FSelected    := False;

  FIconMask := TBitmap32.Create();
  with FIconMask do
  begin
    LoadFromResourceName(HInstance, DEFAULT_COMMAND_ICON_RES_NAME);
  end;

  FIcon := TBitmap32.Create();
  with FIcon do
  begin
    SetSize(COMMAND_ICON_SIZE, COMMAND_ICON_SIZE);
    DrawMode    := dmBlend;
    CombineMode := cmMerge;
  end;
end;

destructor TgmCustomCommand.Destroy;
begin
  FOwner := nil;
  
  FIcon.Free();
  FIconMask.Free();

  inherited;
end;

procedure TgmCustomCommand.ChangeCommandIconByResourceName(
  const AResourceName: string);
var
  LMaskBmp : TBitmap32;
begin
  LMaskBmp := TBitmap32.Create();
  try
    LMaskBmp.LoadFromResourceName(HInstance, AResourceName);

    if (LMaskBmp.Width > 0) and (LMaskBmp.Height > 0) then
    begin
      FIconMask.Assign(LMaskBmp);
    end;
  finally
    LMaskBmp.Free();
  end;
end;

procedure TgmCustomCommand.Execute;
begin
  // dummy
end;

procedure TgmCustomCommand.Rollback;
begin
  // dummy
end;

{ TgmImageProcessCommand }

constructor TgmImageProcessCommand.Create(const ACommandName: string;
  const AUndoBmp, ARedoBmp: TBitmap32);
var
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  inherited Create(ACommandName);

  // Storing the undo/redo bitmaps to disk for saving the system memory.
  // We save the bitmap to a memory stream first, and save the stream to
  // a file that with a random generated filename without a file extension.

  LRandomString := IntToStr( GetTickCount() );

  if Assigned(AUndoBmp) then
  begin
    FUndoBmpFileName := COMMAND_DATA_DIR + '\Undo' + LRandomString;
  end
  else
  begin
    FUndoBmpFileName := '';
  end;

  if Assigned(ARedoBmp) then
  begin
    FRedoBmpFileName := COMMAND_DATA_DIR + '\Redo' + LRandomString;
  end
  else
  begin
    FRedoBmpFileName := '';
  end;

  LDataStream := TMemoryStream.Create();
  try
    if Assigned(AUndoBmp) then
    begin
      AUndoBmp.SaveToStream(LDataStream);
      LDataStream.Position := 0;
      LDataStream.SaveToFile(FUndoBmpFileName);
    end;

    LDataStream.Clear();

    if Assigned(ARedoBmp) then
    begin
      ARedoBmp.SaveToStream(LDataStream);
      LDataStream.Position := 0;
      LDataStream.SaveToFile(FRedoBmpFileName);
    end;

  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmImageProcessCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FUndoBmpFileName) then
  begin
    DeleteFile(PChar(FUndoBmpFileName));
  end;

  if FileExists(FRedoBmpFileName) then
  begin
    DeleteFile(PChar(FRedoBmpFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

{ TgmSnapshot }

constructor TgmSnapshot.Create;
begin
  inherited;

  FName          := '';
  FSelected      := False;
  FOnThumbUpdate := nil;

  FThumbnail := TBitmap32.Create();
  with FThumbnail do
  begin
    SetSize(SNAPSHOT_THUMB_SIZE, SNAPSHOT_THUMB_SIZE);
  end;
end;

destructor TgmSnapshot.Destroy;
begin
  if Assigned(FThumbnail) then
  begin
    FThumbnail.Free();
  end;

  inherited;
end;

function TgmSnapshot.GetRealThumbRect(ASrcBitmap: TCustomBitmap32;
  const AMarginSize: Integer = 4): TRect;
var
  LThumbWidth  : Integer;
  LThumbHeight : Integer;
  LScale       : Single;
begin
  LScale := GetThumbScale(ASrcBitmap.Width, ASrcBitmap.Height,
    SNAPSHOT_THUMB_SIZE - AMarginSize, SNAPSHOT_THUMB_SIZE - AMarginSize);

  LThumbWidth  := Round(ASrcBitmap.Width  * LScale);
  LThumbHeight := Round(ASrcBitmap.Height * LScale);

  with Result do
  begin
    Left   := (SNAPSHOT_THUMB_SIZE - LThumbWidth)  div 2;
    Top    := (SNAPSHOT_THUMB_SIZE - LThumbHeight) div 2;
    Right  := Left + LThumbWidth;
    Bottom := Top  + LThumbHeight;
  end;
end;

function TgmSnapshot.GetThumbScale(
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

procedure TgmSnapshot.UpdateThumnail(ABitmap: TBitmap32);
var
  LRect : TRect;
begin
  if Assigned(ABitmap) and (ABitmap.Width > 0) and (ABitmap.Height > 0) then
  begin
    LRect := GetRealThumbRect(ABitmap);

    FThumbnail.Clear( Color32(clBtnFace) );
    DrawCheckerboardPattern(FThumbnail, LRect, True);
    FThumbnail.Draw(LRect, ABitmap.BoundsRect, ABitmap);

    InflateRect(LRect, 1, 1);
    FThumbnail.FrameRectS(LRect, clBlack32);

    if Assigned(FOnThumbUpdate) then
    begin
      FOnThumbUpdate(Self);
    end;
  end;
end;


{ TgmSnapshotList }

constructor TgmSnapshotList.Create;
begin
  inherited;

  FItems            := TObjectList.Create();
  FSelectedIndex    := -1;
  FSelectedSnapshot := nil;
end;

destructor TgmSnapshotList.Destroy;
begin
  FItems.Free();
  inherited;
end;

function TgmSnapshotList.Add(ASnapshot: TgmSnapshot): Integer;
begin
  if Assigned(ASnapshot) then
  begin
    Result := FItems.Add(ASnapshot);
  end
  else
  begin
    Result := -1;
  end;
end;

procedure TgmSnapshotList.DeselectAllSnapshots();
var
  i         : Integer;
  LSnapshot : TgmSnapshot;
begin
  if FItems.Count > 0 then
  begin
    for i := 0 to (FItems.Count - 1) do
    begin
      LSnapshot           := TgmSnapshot(FItems.Items[i]);
      LSnapshot.FSelected := False;
    end;

    FSelectedSnapshot := nil;
    FSelectedIndex    := -1;
  end;
end;

function TgmSnapshotList.GetSnapshot(AIndex: Integer): TgmSnapshot;
begin
  if IsValidIndex(AIndex) then
  begin
    Result := TgmSnapshot(FItems.Items[AIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TgmSnapshotList.GetSnapshotCount: Integer;
begin
  Result := FItems.Count;
end;

function TgmSnapshotList.IsValidIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count);
end;

procedure TgmSnapshotList.SelectSnapshot(const AIndex: Integer);
begin
  if not IsValidIndex(AIndex) then
  begin
    Exit;
  end;

  DeselectAllSnapshots();
  
  FSelectedIndex               := AIndex;
  FSelectedSnapshot            := TgmSnapshot(FItems.Items[AIndex]);
  FSelectedSnapshot.IsSelected := True;
end;

////////////////////////////////////////////////////////////////////////////////

procedure InitCommandDataDir;
begin
  COMMAND_DATA_DIR := ExtractFileDir(ParamStr(0)) + '\History';
  if not DirectoryExists(COMMAND_DATA_DIR) then
  begin
    CreateDir(COMMAND_DATA_DIR);
  end;
end;


initialization
  InitCommandDataDir();


end.
