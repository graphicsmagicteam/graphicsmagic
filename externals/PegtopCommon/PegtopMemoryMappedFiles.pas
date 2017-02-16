////////////////////////////////////////////////////////////////////////////////
// File:       PegtopMemoryMappedFiles.pas
// Classes:    TPegtopMemoryMappedFile, TPegtopSharedMemory,
//             TPegtopMutexSharedMemory
// Version:    1.01
// Date:       12 Jan 2005 created 1.00
//             05 Apr 2005 modified 1.01 (TPegtopMemoryMappedFile added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopMemoryMappedFile created a named file-mapping object backed by the
// operating system paging, accessible by other processes (using the same name).
// An empty string can be used to create a nameless memory mapped file (not
// accessible from elsewhere). The whole object or parts of it can be mapped
// into memory by using MapView (clean up with UnmapView).
// TPegtopSharedMemory allocates a named block of shared memory, using a
// memory mapped file (mapping is done automatically, the memory is accessible
// by using the Memory property).
// TPegtopMutexSharedMemory introduces the methods Enter and Leave, making sure
// no other thread does access the memory at the same time (of course all
// threads have to use Enter / Leave before / after accessing shared memory).
// Internally a TPegtopMutexSection is used to synchronize access.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopMemoryMappedFiles;

interface

uses
  Windows, SysUtils, PegtopMutexSections;

type
  TPegtopMemoryMappedFile = class
  private
    FName: String;
    FFileHandle: THandle;
    FIsFirstInstance: Boolean;
    FHandle: THandle;
    FSize: Int64;
    FOffset: Int64;
  public
    constructor Create(const AName: String; const ASize: Int64;
      const AFileHandle: THandle = INVALID_HANDLE_VALUE);
      // INVALID_HANDLE_VALUE = $FFFFFFFF = use Windows swap file
    destructor Destroy; override;
    function MapView: Pointer; overload;
    function MapView(Offset: Int64; Size: Longword): Pointer; overload;
    procedure UnmapView(Memory: Pointer);
    function FlushView(Memory: Pointer; Size: Longword): Boolean;
    property Name: String read FName;
    property FileHandle: THandle read FFileHandle;
    property IsFirstInstance: Boolean read FIsFirstInstance;
    property Handle: THandle read FHandle;
    property Size: Int64 read FSize;
    property Offset: Int64 read FOffset;
  end;

  TPegtopSharedMemory = class(TPegtopMemoryMappedFile)
  private
    FMemory: Pointer;
  public
    constructor Create(const AName: String; const ASize: Int64;
      const AFileHandle: THandle = INVALID_HANDLE_VALUE);
    destructor Destroy; override;
    function Flush: Boolean;
    property Memory: Pointer read FMemory;
  end;

  TPegtopMutexSharedMemory = class(TPegtopSharedMemory)
  private
    FMutexSection: TPegtopMutexSection;
  public
    constructor Create(const AName: String; const ASize: Int64;
      const AFileHandle: THandle = INVALID_HANDLE_VALUE; ImmediateEnter: Boolean = False);
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

  EPegtopFileMappingError = class(Exception);

var
  AllocationGranularity: Longword;

implementation

procedure GetAllocationGranularity;
var
  Info: TSystemInfo;
begin
  GetSystemInfo(Info);
  AllocationGranularity := Info.dwAllocationGranularity
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMemoryMappedFile
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMemoryMappedFile.Create(const AName: String; const ASize: Int64;
  const AFileHandle: THandle = INVALID_HANDLE_VALUE);
var
  PName: PChar;
  SizeHi, SizeLo: Longword;
begin
  FName := StringReplace(AName, '\', '_', [rfReplaceAll]); // name must not contain '\'
  if FName = '' then PName := NIL else PName := PChar(FName);
  FSize := ASize;
  SizeHi := FSize shr 32;
  SizeLo := FSize and $FFFFFFFF;
  FFileHandle := AFileHandle;
  FHandle := CreateFileMapping(
    FFileHandle,
    NIL,            // no security attributes
    PAGE_READWRITE, // read and write access for all
    SizeHi,
    SizeLo,
    PName);
  FIsFirstInstance := GetLastError <> ERROR_ALREADY_EXISTS;
  if (FHandle = 0) then raise EPegtopFileMappingError.Create('Memory mapped file could not be created.');
end;

destructor TPegtopMemoryMappedFile.Destroy;
begin
  if FHandle <> 0 then CloseHandle(FHandle);
  inherited;
end;

function TPegtopMemoryMappedFile.MapView: Pointer;
begin
  Result := MapView(0, 0);
end;

function TPegtopMemoryMappedFile.MapView(Offset: Int64; Size: Longword): Pointer;
var
  OffsetHi, OffsetLo: Longword;
begin
  OffsetHi := Offset shr 32;
  OffsetLo := Offset and $FFFFFFFF;
  Result := MapViewOfFile(FHandle, FILE_MAP_ALL_ACCESS,
    OffsetHi,
    OffsetLo,
    Size);
  if (Result = NIL) then raise EPegtopFileMappingError.Create('Memory mapped file could not be mapped.');
end;

procedure TPegtopMemoryMappedFile.UnmapView(Memory: Pointer);
begin
  if (Memory <> NIL) then UnmapViewOfFile(Memory);
end;

function TPegtopMemoryMappedFile.FlushView(Memory: Pointer; Size: Longword): Boolean;
begin
  Result := FlushViewOfFile(Memory, Size);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopSharedMemory
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopSharedMemory.Create(const AName: String; const ASize: Int64;
  const AFileHandle: THandle = INVALID_HANDLE_VALUE);
begin
  inherited;
  FMemory := MapView;
end;

destructor TPegtopSharedMemory.Destroy;
begin
  UnmapView(FMemory);
  inherited;
end;

function TPegtopSharedMemory.Flush: Boolean;
begin
  Result := FlushView(FMemory, FSize);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMutexSharedMemory
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMutexSharedMemory.Create(const AName: String; const ASize: Int64;
  const AFileHandle: THandle = INVALID_HANDLE_VALUE; ImmediateEnter: Boolean = False);
begin
  inherited Create(AName, ASize, AFileHandle);
  FMutexSection := TPegtopMutexSection.Create(FName + 'Mutex', ImmediateEnter);
end;

destructor TPegtopMutexSharedMemory.Destroy;
begin
  FMutexSection.Free;
  inherited;
end;

procedure TPegtopMutexSharedMemory.Enter;
begin
  FMutexSection.Enter;
end;

procedure TPegtopMutexSharedMemory.Leave;
begin
  FMutexSection.Leave;
end;

initialization
  GetAllocationGranularity;
end.
