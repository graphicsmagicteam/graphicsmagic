unit gmPluginManager;

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
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 * Jens Gruschel (http://www.pegtop.net)
 *
 * Many thanks to Jens Gruschel for teaching us how to make a plug-in system.
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

// Update Date: 2017/01/24 

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
{ Standard Lib }
  Windows,
  SysUtils,
  Classes,
  Menus,
{ Graphics32 Lib }
  GR32,
{ GraphicsMagic Lib }
  gmChannelManager,
  gmTypes;

  {
    TODO :
      * Split this in two different parts
        - one for pluin loading and freeing up,
          just like plugin loader and container
        - And then the PlugInGUI lib, because IMHO
          pluin manager/loader/containetr should not
          know anything about the GUI, just about 
          the handling the plugins.
  }

type
  EgmCustomPluginException = class(Exception);
  EgmPluginFunctionNotSupported = class(EgmCustomPluginException);
  EgmPluginNotValid = class(EgmCustomPluginException);
  EgmPluginCanNotLoad = class(EgmCustomPluginException);
  
  TUpdateViewProc = procedure;

  TgmPluginFunc = function(AppHandle: THandle; DestBmpPtr: PColor32;
    const Width, Height: Integer; UpdateViewProc: TUpdateViewProc;
    const BKColor: TColor32 = $00000000): Boolean; stdcall;

  TgmPluginGetStringFunc = function : PChar; stdcall;
  TgmPluginBooleanFunc = function : Boolean; stdcall;
  TgmPluginSetChannelsFunc = function(const ChannelSet: TgmChannelSet): Boolean; stdcall;

type
  TgmCategoryMenuItemList = class(TList)
  public
    destructor Destroy; override;
    procedure DeleteAllMenuItems;
  end;

  TgmPlugin = class(TObject)
  private
    FLibHandle         : THandle;
    FFunc              : TgmPluginFunc;
    FGetPluginCategory : TgmPluginGetStringFunc;
    FGetPluginName     : TgmPluginGetStringFunc;
    FIsAdjustablePlugin: TgmPluginBooleanFunc;
    FIsChannelSupported: TgmPluginBooleanFunc;
    FSetChannels       : TgmPluginSetChannelsFunc;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    function RunPlugin(ADestBmpPtr: PColor32; const AWidth, AHeight: Integer;
      AUpdateViewProc: TUpdateViewProc; const ABackColor: TColor32 = $00000000): Boolean;

    function GetPluginCategory: string;
    function GetPluginName: string;
    function IsAdjustablePlugin: Boolean;
    function IsChannelSupported: Boolean;
    function SetChannels(const AChannelSet: TgmChannelSet): Boolean;
  end;

  TgmUniqueStringList = class(TStringList)
  public
    procedure AddUniqueString(const AString: string);
  end;

  TgmPluginInfo = class(TObject)
  private
    FLibFileName       : string;
    FCategoryName      : string;
    FPluginName        : string;
    FIsAdjustable      : Boolean;
    FIsChannelSupported: Boolean;
    FPluginMenu        : TMenuItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdatePluginMenuEnableState(const AChannelType: TgmChannelType); overload;
    procedure UpdatePluginMenuEnableState(AChannelManager: TgmCustomChannelManager); overload;

    property LibFileName       : string    read FLibFileName        write FLibFileName;
    property CategoryName      : string    read FCategoryName       write FCategoryName;
    property PluginName        : string    read FPluginName         write FPluginName;
    property IsAdjustable      : Boolean   read FIsAdjustable       write FIsAdjustable;
    property IsChannelSupported: Boolean   read FIsChannelSupported write FIsChannelSupported;
    property PluginMenu        : TMenuItem read FPluginMenu         write FPluginMenu;
  end;

  TgmPluginInfoList = class(TList)
  private
    procedure DeleteAllPluginInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSortedCategoryList: TgmUniqueStringList;
    function GetSortedPluginNameList(const ACategoryName: string): TgmUniqueStringList;
    function GetPluginInfoByName(const APluginName: string): TgmPluginInfo;
    function GetPluginInfoByMenuItem(ASender: TObject): TgmPluginInfo;

    procedure UpdatePluginMenusEnableState(const AChannelType: TgmChannelType); overload;
    procedure UpdatePluginMenusEnableState(AChannelManager: TgmCustomChannelManager); overload;
  end;

  procedure ZarkoFileSearch(const PathName, FileName: string;
    const InDir: Boolean; StrList: TStringList);

implementation

uses
  Forms;

//-- Private procedures and functions ------------------------------------------

{
  Searching for Files

  Stop. This is the one and only solution to file searching.
  Use Delphi to find any file in any directory and/or subdirectory that match a
  certain mask. Start searching.

  Article:
  . http://delphi.about.com/library/weekly/aa051600a.htm

  ********************************************
  Zarko Gajic
  About.com Guide to Delphi Programming
  http://delphi.about.com
  email: delphi.guide@about.com
  ********************************************
}

{ Method Usage:

  PathName -- The directory you want to search in.
  FileName -- The extansion name (mask) of files you are looking for.
              Format is *.xxx, such as *.bmp.
  InDir    -- Whether search files in the sub directory.
}

procedure ZarkoFileSearch(const PathName, FileName: string; const InDir: Boolean;
  StrList: TStringList);
var
  LRec : TSearchRec;
  LPath: string;
begin
  if Assigned(StrList) then
  begin
    LPath :=  IncludeTrailingPathDelimiter(PathName);
    
    if FindFirst(LPath + FileName, faAnyFile - faDirectory, LRec) = 0 then
    try
      repeat
        StrList.Add(LPath + LRec.Name);
      until FindNext(LRec) <> 0;
    finally
      FindClose(LRec);
    end;

    If not InDir then
    begin
      Exit;
    end;

    if FindFirst(LPath + '*.*', faDirectory, LRec) = 0 then
    try
    
      repeat
        if ((LRec.Attr and faDirectory) <> 0)  and
           (LRec.Name<>'.') and
           (LRec.Name<>'..') then
        begin
          ZarkoFileSearch(LPath + LRec.Name, FileName, True, StrList);
        end;
      until FindNext(LRec) <> 0;

    finally
      FindClose(LRec);
    end;
  end;
end; 

//-- TgmCategoryMenuItemList ---------------------------------------------------

destructor TgmCategoryMenuItemList.Destroy;
begin
  DeleteAllMenuItems;
  inherited Destroy;
end;

procedure TgmCategoryMenuItemList.DeleteAllMenuItems;
var
  i        : Integer;
  LMenuItem: TMenuItem;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LMenuItem := TMenuItem(Self.Items[i]);
      LMenuItem.Free;
    end;
    
    Self.Clear;
  end;
end; 

//-- TgmPlugin -----------------------------------------------------------------

constructor TgmPlugin.Create(const AFileName: string);
begin
   inherited Create;

   FLibHandle := LoadLibrary( PChar(AFileName) );
   
   if FLibHandle >= 32 then
   begin
     FFunc               := GetProcAddress(FLibHandle, 'ExecutePlugin');
     FGetPluginCategory  := GetProcAddress(FLibHandle, 'GetPluginCategory');
     FGetPluginName      := GetProcAddress(FLibHandle, 'GetPluginName');
     FIsAdjustablePlugin := GetProcAddress(FLibHandle, 'IsAdjustablePlugin');
     FIsChannelSupported := GetProcAddress(FLibHandle, 'IsChannelSupported');
     FSetChannels        := GetProcAddress(FLibHandle, 'SetChannels');

     if not Assigned(FFunc) then
     begin
       raise EgmPluginNotValid.Create('Plug-in is not valid.');
     end;
   end
   else
   begin
     raise EgmPluginCanNotLoad.Create('Cannot load the plug-in you specified.');
   end;
end;

destructor TgmPlugin.Destroy;
begin
  if FLibHandle <> 0 then
  begin
    FreeLibrary(FLibHandle);
  end;

  inherited Destroy;
end; 

function TgmPlugin.RunPlugin(ADestBmpPtr: PColor32;
  const AWidth, AHeight: Integer; AUpdateViewProc: TUpdateViewProc;
  const ABackColor: TColor32 = $00000000): Boolean;
begin
  if Assigned(FFunc) then
  begin
    Result := FFunc(Application.Handle, ADestBmpPtr, AWidth, AHeight,
                    AUpdateViewProc, ABackColor);
  end
  else
  begin
    raise EgmPluginFunctionNotSupported.Create('Function not supported by plug-in.');
  end;
end;

function TgmPlugin.GetPluginCategory: string;
begin
  if Assigned(FGetPluginCategory) then
  begin
    Result := FGetPluginCategory;
  end
  else
  begin
    Result := '';
  end;
end;

function TgmPlugin.GetPluginName: string;
begin
  if Assigned(FGetPluginName) then
  begin
    Result := FGetPluginName;
  end
  else
  begin
    Result := '';
  end;
end;

function TgmPlugin.IsAdjustablePlugin: Boolean;
begin
  if Assigned(FIsAdjustablePlugin) then
  begin
    Result := FIsAdjustablePlugin;
  end
  else
  begin
    Result := False;
  end;
end; 

function TgmPlugin.IsChannelSupported: Boolean;
begin
  if Assigned(FIsChannelSupported) then
  begin
    Result := FIsChannelSupported;
  end
  else
  begin
    Result := False;
  end;
end;

function TgmPlugin.SetChannels(const AChannelSet: TgmChannelSet): Boolean;
begin
  if Assigned(FSetChannels) then
  begin
    Result := FSetChannels(AChannelSet);
  end
  else
  begin
    Result := False;
  end;
end; 

//-- TgmUniqueStringList -------------------------------------------------------

procedure TgmUniqueStringList.AddUniqueString(const AString: string);
var
  i, LIndex: Integer;
begin
  LIndex := -1;
  
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      if CompareStr( LowerCase(Self.Strings[i]), LowerCase(AString) ) = 0 then
      begin
        LIndex := i;
        Break;
      end;
    end;

    if LIndex < 0 then
    begin
      Self.Add(AString);
      Self.Sort;
    end;
  end
  else
  begin
    Self.Add(AString);
    Self.Sort;
  end;
end; 

//-- TgmPluginInfo -------------------------------------------------------------

constructor TgmPluginInfo.Create;
begin
  inherited Create;

  FLibFileName        := '';
  FCategoryName       := '';
  FPluginName         := '';
  FIsAdjustable       := False;
  FIsChannelSupported := False;
  FPluginMenu         := nil;
end; 

destructor TgmPluginInfo.Destroy;
begin
  if Assigned(FPluginMenu) then
  begin
    FPluginMenu.Visible := False;
    FPluginMenu.Free;
  end;

  inherited Destroy;
end;

procedure TgmPluginInfo.UpdatePluginMenuEnableState(
  const AChannelType: TgmChannelType);
begin
  if Assigned(FPluginMenu) then
  begin
    if AChannelType = ctColorChannel then
    begin
      FPluginMenu.Enabled := True;
    end
    else
    begin
      FPluginMenu.Enabled := FIsChannelSupported;
    end;
  end;
end;

procedure TgmPluginInfo.UpdatePluginMenuEnableState(
  AChannelManager: TgmCustomChannelManager);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPluginInfo.UpdatePluginMenuEnableState(): parameter AChannelManager is a nil pointer.');
  end;
  
  if Assigned(FPluginMenu) then
  begin
    if AChannelManager.CurrentChannelType = ctColorChannel then
    begin
      if AChannelManager.ColorChannelList.SelectedChannelCount >= 3 then
      begin
        FPluginMenu.Enabled := True;
      end
      else
      begin
        FPluginMenu.Enabled := FIsChannelSupported;
      end;
    end
    else
    begin
      FPluginMenu.Enabled := FIsChannelSupported;
    end;
  end;
end;

//-- TgmPluginInfoList ---------------------------------------------------------

constructor TgmPluginInfoList.Create;
begin
  inherited Create;
end;

destructor TgmPluginInfoList.Destroy;
begin
  DeleteAllPluginInfo;
  inherited Destroy;
end;

procedure TgmPluginInfoList.DeleteAllPluginInfo;
var
  i          : Integer;
  LPluginInfo: TgmPluginInfo;
begin
  if Self.Count > 0 then
  begin
    for i := (Self.Count - 1) downto 0 do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);

      FreeAndNil(LPluginInfo);
    end;
    
    Self.Clear;
  end;
end;

function TgmPluginInfoList.GetSortedCategoryList: TgmUniqueStringList;
var
  i          : Integer;
  LPluginInfo: TgmPluginInfo;
begin
  if Self.Count > 0 then
  begin
    Result := TgmUniqueStringList.Create;

    for i := 0 to (Self.Count - 1) do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);

      Result.AddUniqueString(LPluginInfo.FCategoryName);
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

function TgmPluginInfoList.GetSortedPluginNameList(
  const ACategoryName: string): TgmUniqueStringList;
var
  i, LMaxIndex, LIndex: Integer;
  LIndexArray         : array of Integer;
  LPluginInfo         : TgmPluginInfo;
begin
  Result      := nil;
  LIndexArray := nil;

  if Self.Count > 0 then
  begin
    Result := TgmUniqueStringList.Create;
    
    for i := 0 to (Self.Count - 1) do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);
      
      if CompareStr( LowerCase(LPluginInfo.FCategoryName), LowerCase(ACategoryName) ) = 0 then
      begin
        SetLength( LIndexArray, Length(LIndexArray) + 1 );
        LIndexArray[High(LIndexArray)] := i;
      end;
    end;

    LMaxIndex := High(LIndexArray);
    
    if LMaxIndex >= 0 then
    begin
      Result := TgmUniqueStringList.Create;
      
      for i := 0 to LMaxIndex do
      begin
        LIndex      := LIndexArray[i];
        LPluginInfo := TgmPluginInfo(Self.Items[LIndex]);

        Result.AddUniqueString(LPluginInfo.FPluginName);
      end;
      
      Result.Sort;
      LIndexArray := nil;
    end;
  end;
end; 

function TgmPluginInfoList.GetPluginInfoByName(
  const APluginName: string): TgmPluginInfo;
var
  i          : Integer;
  LPluginInfo: TgmPluginInfo;
begin
  Result := nil;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);
      
      if CompareStr( LowerCase(LPluginInfo.FPluginName), LowerCase(APluginName) ) = 0 then
      begin
        Result := LPluginInfo;
        Break;
      end;
    end;
  end;
end; 

function TgmPluginInfoList.GetPluginInfoByMenuItem(
  ASender: TObject): TgmPluginInfo;
var
  i          : Integer;
  LPluginInfo: TgmPluginInfo;
begin
  LPluginInfo := nil;

  if Self.Count > 0 then
  begin
    if (ASender <> nil) and (ASender is TMenuItem) then
    begin
      for i := 0 to (Self.Count - 1) do
      begin
        LPluginInfo := TgmPluginInfo(Self.Items[i]);
        
        if ASender = LPluginInfo.PluginMenu then
        begin
          Break;
        end;
      end;
    end;
  end;

  Result := LPluginInfo;
end; 

procedure TgmPluginInfoList.UpdatePluginMenusEnableState(
  const AChannelType: TgmChannelType);
var
  i           : Integer;
  LPluginInfo : TgmPluginInfo;
begin
  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);
      LPluginInfo.UpdatePluginMenuEnableState(AChannelType);
    end;
  end;
end;

procedure TgmPluginInfoList.UpdatePluginMenusEnableState(
  AChannelManager: TgmCustomChannelManager);
var
  i           : Integer;
  LPluginInfo : TgmPluginInfo;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPluginInfoList.UpdatePluginMenusEnableState(): parameter AChannelManager is a nil pointer.');
  end;

  if Self.Count > 0 then
  begin
    for i := 0 to (Self.Count - 1) do
    begin
      LPluginInfo := TgmPluginInfo(Self.Items[i]);
      LPluginInfo.UpdatePluginMenuEnableState(AChannelManager);
    end;
  end;
end;

end.
