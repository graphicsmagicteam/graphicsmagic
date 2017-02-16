////////////////////////////////////////////////////////////////////////////////
// File:       PegtopSystemImages.pas
// Components: TPegtopSystemImages
// Version:    1.01
// Date:       05 Jul 2003 1.00
//             10 May 2005 1.01 (GetExtIconIndex modified)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopSystemImages is an image list, that holds the system images (icons
// of all file types and folders). You can use it for a "custom explorer", or
// to get icons for special folders or file extensions. There are also public
// functions to get information (like file type name) for a given filename,
// special folder, or extension.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopSystemImages;

interface

uses
  Windows, Classes, Controls, ImgList, SysUtils, Messages, ShlObj;

type
  EInvalidListHandle = class(Exception);

  TPegtopSystemImageSize = (pisSmallIcons, pisLargeIcons);

  TPegtopSpecialFolder = (
    sfDesktop, sfPrograms, sfControls, sfPrinters, sfPersonal,
    sfFavorites, sfStartUp, sfRecent, sfSendTo, sfBitBucket,
    sfStartMenu, sfDesktopDirectory, sfDrives, sfNetwork, sfNetHood,
    sfFonts, sfTemplates, sfCommonStartMenu, sfCommonPrograms, sfCommonStartUp,
    sfCommonDesktopDirectory, sfAppData, sfPrintHood);

  TPegtopFileInfo = record
    IconIndex: Integer;
    DisplayName: String;
    TypeName: String;
    Attributes: Cardinal;
  end;

  TPegtopSystemImages = class(TCustomImageList)
  private
    FImageSize: TPegtopSystemImageSize;
    procedure SetImageSize(Value: TPegtopSystemImageSize);
    function GetImageSizeFlag: Integer;
    function ValidateExtension(Extension: String): String;
  protected
    procedure WriteState(Writer: TWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure ApplySystemImageListHandle;
    function GetFileIconIndex(FileName: String): Integer;
    function GetFileIconIndexW(FileName: WideString): Integer;
    function GetExtIconIndex(Extension: String; Attr: Integer = FILE_ATTRIBUTE_NORMAL): Integer;
    function GetExtIcon(Extension: String; Hidden: Boolean = False): HICON;
    function GetSpecialIconIndex(SpecialFolder: TPegtopSpecialFolder): Integer;
    function GetFileTypeName(FileName: String): String;
    function GetExtTypeName(Extension: String): String;
    function GetSpecialDisplayName(SpecialFolder: TPegtopSpecialFolder): String;
    function GetSpecialTypeName(SpecialFolder: TPegtopSpecialFolder): String;
    function GetFileInfo(FileName: String; var Info: TPegtopFileInfo): Boolean;
    procedure ReadFileExtensions(Extensions: TStrings);
  published
    property ImageSize: TPegtopSystemImageSize read FImageSize write SetImageSize;
    property ImageType;
    property Masked;
    property BlendColor;
    property BkColor;
    property DrawingStyle;
  end;

function SmallSystemImages: TPegtopSystemImages;

implementation

uses
  Registry,
  ShellAPI; // for TSHFileInfo

const
  SpecialFolderIDs: array[TPegtopSpecialFolder] of Integer = (
    CSIDL_DESKTOP, CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS, CSIDL_PERSONAL,
    CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET,
    CSIDL_STARTMENU, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD);

var
  SmallSystemImageList: TPegtopSystemImages;

function SmallSystemImages: TPegtopSystemImages;
begin
  if SmallSystemImageList = NIL then
    SmallSystemImageList := TPegtopSystemImages.Create(NIL);
  Result := SmallSystemImageList;
end;

{$EXTERNALSYM SHGetFileInfoW}
function SHGetFileInfoW(pszPath: PWideChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfoW; cbFileInfo, uFlags: UINT): DWORD; stdcall;
  external 'shell32.dll' name 'SHGetFileInfoW';

////////////////////////////////////////////////////////////////////////////////
// TPegtopSystemImages
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopSystemImages.Create(AOwner: TComponent);
begin
  FImageSize := pisSmallIcons;
  inherited;
end;

destructor TPegtopSystemImages.Destroy;
begin
  inherited;
end;

procedure TPegtopSystemImages.AfterConstruction;
begin
  inherited;
  ApplySystemImageListHandle;
end;

procedure TPegtopSystemImages.WriteState(Writer: TWriter);
var
  TempHandle: THandle;
begin
  TempHandle := Handle;
  Handle := 0;
  ShareImages := True;
  inherited;
  Handle := TempHandle;
  ShareImages := True;
end;

procedure TPegtopSystemImages.ApplySystemImageListHandle;
var
  FileInfo: TSHFileInfo;
  SysHandle: THandle;
begin
  SysHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or GetImageSizeFlag);
  if SysHandle <> 0 then begin
    Handle := SysHandle;
    ShareImages := True;
  end
  else begin
    raise EInvalidListHandle.Create('The handle of the system image list is not valid.');
  end;
end;

function TPegtopSystemImages.GetImageSizeFlag: Integer;
begin
  if FImageSize = pisSmallIcons then
    Result := SHGFI_SMALLICON
  else
    Result := SHGFI_LARGEICON;
end;

function TPegtopSystemImages.ValidateExtension(Extension: String): String;
begin
  if Extension = '' then begin
    Result := '';
  end
  else if Extension[1] = '.' then begin
    Result := '*' + Extension;
  end
  else if Extension[1] = '*' then begin
    Result := Extension;
  end
  else begin
    Result := '*.' + Extension;
  end;
end;

{
      SHGFI_ICON              = 0x000000100,     // get icon
      SHGFI_DISPLAYNAME       = 0x000000200,     // get display name
      SHGFI_TYPENAME          = 0x000000400,     // get type name
      SHGFI_ATTRIBUTES        = 0x000000800,     // get attributes
      SHGFI_ICONLOCATION      = 0x000001000,     // get icon location
      SHGFI_EXETYPE           = 0x000002000,     // return exe type
      SHGFI_SYSICONINDEX      = 0x000004000,     // get system icon index
      SHGFI_LINKOVERLAY       = 0x000008000,     // put a link overlay on icon
      SHGFI_SELECTED          = 0x000010000,     // show icon in selected state
      SHGFI_ATTR_SPECIFIED    = 0x000020000,     // get only specified attributes
      SHGFI_LARGEICON         = 0x000000000,     // get large icon
      SHGFI_SMALLICON         = 0x000000001,     // get small icon
      SHGFI_OPENICON          = 0x000000002,     // get open icon
      SHGFI_SHELLICONSIZE     = 0x000000004,     // get shell size icon
      SHGFI_PIDL              = 0x000000008,     // pszPath is a pidl
      SHGFI_USEFILEATTRIBUTES = 0x000000010,     // use passed dwFileAttribute
      SHGFI_ADDOVERLAYS       = 0x000000020,     // apply the appropriate overlays
      SHGFI_OVERLAYINDEX      = 0x000000040,     // Get the index of the overlay in 
                                                 // the upper 8 bits of the iIcon
}

function TPegtopSystemImages.GetFileIconIndex(FileName: String): Integer;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
  SHGFI_SYSICONINDEX or GetImageSizeFlag) <> 0 then
    Result := FileInfo.iIcon
  else
    Result := -1;
end;

function TPegtopSystemImages.GetFileIconIndexW(FileName: WideString): Integer;
var
  FileInfo: TSHFileInfoW;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result := GetFileIconIndex(FileName);
  end
  else begin
    if SHGetFileInfoW(PWideChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or GetImageSizeFlag) <> 0 then
      Result := FileInfo.iIcon
    else
      Result := -1;
  end;
end;

function TPegtopSystemImages.GetExtIconIndex(Extension: String; Attr: Integer = FILE_ATTRIBUTE_NORMAL): Integer;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(ValidateExtension(Extension)), Attr,
  FileInfo, SizeOf(FileInfo),
  SHGFI_SYSICONINDEX or GetImageSizeFlag or SHGFI_USEFILEATTRIBUTES) <> 0 then
    Result := FileInfo.iIcon
  else
    Result := -1;
end;

function TPegtopSystemImages.GetExtIcon(Extension: String; Hidden: Boolean = False): HICON;
var
  FileInfo: TSHFileInfo;
  Attr: LongInt;
begin
  if Hidden then
    Attr := FILE_ATTRIBUTE_SYSTEM // FILE_ATTRIBUTE_HIDDEN
  else
    Attr := FILE_ATTRIBUTE_NORMAL;
  if Extension = '' then begin
    Result := 0;
  end
  else begin
    if SHGetFileInfo(PChar(ValidateExtension(Extension)), Attr,
    FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or GetImageSizeFlag or SHGFI_USEFILEATTRIBUTES) <> 0 then
      Result := FileInfo.hIcon
    else
      Result := 0;
  end;
end;

function TPegtopSystemImages.GetSpecialIconIndex(SpecialFolder: TPegtopSpecialFolder): Integer;
var
  FileInfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  SHGetSpecialFolderLocation(0, SpecialFolderIDs[SpecialFolder], PIDL);
  if SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(TSHFileInfo),
  SHGFI_PIDL or SHGFI_SYSICONINDEX or GetImageSizeFlag) <> 0 then
    Result := FileInfo.iIcon
  else
    Result := -1;
end;

function TPegtopSystemImages.GetFileTypeName(FileName: String): String;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
  SHGFI_TYPENAME) <> 0 then
    Result := FileInfo.szTypeName
  else
    Result := '';
end;

function TPegtopSystemImages.GetExtTypeName(Extension: String): String;
var
  FileInfo: TSHFileInfo;
begin
  if Extension = '' then begin
    Result := '';
  end
  else begin
    if SHGetFileInfo(PChar(ValidateExtension(Extension)), FILE_ATTRIBUTE_NORMAL,
    FileInfo, SizeOf(FileInfo),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0 then
      Result := FileInfo.szTypeName
    else
      Result := '';
  end;
end;

function TPegtopSystemImages.GetSpecialDisplayName(SpecialFolder: TPegtopSpecialFolder): String;
var
  FileInfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  SHGetSpecialFolderLocation(0, SpecialFolderIDs[SpecialFolder], PIDL);
  if SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
  SHGFI_PIDL or SHGFI_DISPLAYNAME) <> 0 then
    Result := FileInfo.szDisplayName
  else
    Result := '';
end;

function TPegtopSystemImages.GetSpecialTypeName(SpecialFolder: TPegtopSpecialFolder): String;
var
  FileInfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  SHGetSpecialFolderLocation(0, SpecialFolderIDs[SpecialFolder], PIDL);
  if SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
  SHGFI_PIDL or SHGFI_TYPENAME) <> 0 then
    Result := FileInfo.szTypeName
  else
    Result := '';
end;

function TPegtopSystemImages.GetFileInfo(FileName: String; var Info: TPegtopFileInfo): Boolean;
var
  FileInfo: TSHFileInfo;
begin
  if SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
  SHGFI_DISPLAYNAME or SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_ATTRIBUTES) <> 0 then begin
    Result := True;
    Info.IconIndex := FileInfo.iIcon;
    Info.DisplayName := FileInfo.szDisplayName;
    Info.TypeName := FileInfo.szTypeName;
    Info.Attributes := FileInfo.dwAttributes;
  end
  else begin
    Result := False;
  end;
end;

procedure TPegtopSystemImages.SetImageSize(Value: TPegtopSystemImageSize);
begin
  if FImageSize <> Value then begin
    FImageSize := Value;
    ApplySystemImageListHandle;
  end;
end;

procedure TPegtopSystemImages.ReadFileExtensions(Extensions: TStrings);
var
  Reg: TRegistry;
  RegList: TStringList;
  I: Integer;
begin
  RegList := TStringList.Create;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKeyReadOnly('\') then begin
        try
          Reg.GetKeyNames(RegList);
        finally
          Reg.CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
    I := RegList.Count;
    while I > 0 do begin
      Dec(I);
      if (Length(RegList[I]) < 2) or (RegList[I][1] <> '.') then RegList.Delete(I);
    end;
    Extensions.Assign(RegList);
  finally
    RegList.Free;
  end;
end;

initialization
  SmallSystemImageList := NIL;
finalization
  SmallSystemImageList.Free;
end.

