////////////////////////////////////////////////////////////////////////////////
// File:       PegtopSysUtils.pas
// Version:    1.01
// Date:       20 Oct 2004 1.00
//             20 Sep 2005 1.01 (PegtopForceForegroundWindow added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Some system functions used by other units.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopSysUtils;

interface

uses
  Windows, Classes;

type
  TUUID = packed record
    // universally unique identifier
    Data1: Longword;
    Data2: Word;
    Data3: Word;
    Data4: array[0..7] of Byte;
  end;

function PegtopGetWinDir: String;
function PegtopGetSysDir: String;

function PegtopGetCPUCounter: Int64; assembler;
function PegtopCreateUUID: TUUID;
function PegtopUUIDToString(UUID: TUUID): String;

function PegtopGetDiskSize(Drive: Char; out UsableSize, TotalSize, FreeSize: Int64): Boolean;
function PegtopGetVolumeInformation(Drive: Char; out VolumeName, FileSystemName: String;
  out SerialNumber, MaxFileNameLength, FileSystemFlags: Cardinal): Boolean;

procedure PegtopForceForegroundWindow(Handle: THandle);
procedure PegtopTrimWorkingSetSize;

function PegtopUserIsAdmin: Boolean;

implementation

uses
  SysUtils;

function PegtopGetWinDir: String;
begin
  SetLength(Result, MAX_PATH);
  GetWindowsDirectory(PChar(Result), Length(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function PegtopGetSysDir: String;
begin
  SetLength(Result, MAX_PATH);
  GetSystemDirectory(PChar(Result), Length(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function PegtopGetCPUCounter: Int64; assembler;
asm
  db $0F, $31 // RDTSC ("read time stamp counter" result in edx:eax)
end;

const
  RPC_S_OK = $0;
  RPC_S_UUID_LOCAL_ONLY = $720;
  RPC_S_UUID_NO_ADDRESS = $6CB;

{$EXTERNALSYM UUidCreate}
function UuidCreate(out UUID: TUUID): Longint; stdcall; external 'rpcrt4.dll' name 'UuidCreate';

function PegtopCreateUUID: TUUID;
begin
  if UuidCreate(Result) <> RPC_S_OK then FillChar(Result, SizeOf(Result), 0);
end;

function PegtopUUIDToString(UUID: TUUID): String;
begin
  Result := IntToHex(UUID.Data1, 8) + '-'
  + IntToHex(UUID.Data2, 4) + '-'
  + IntToHex(UUID.Data3, 4) + '-'
  + IntToHex(UUID.Data4[0], 2) + IntToHex(UUID.Data4[1], 2) + '-'
  + IntToHex(UUID.Data4[2], 2) + IntToHex(UUID.Data4[3], 2)
  + IntToHex(UUID.Data4[4], 2) + IntToHex(UUID.Data4[5], 2)
  + IntToHex(UUID.Data4[6], 2) + IntToHex(UUID.Data4[7], 2);
end;

function PegtopGetDiskSize(Drive: Char; out UsableSize, TotalSize, FreeSize: Int64): Boolean;
var
  Root: array[0..4] of Char;
begin
  Root[0] := Drive;
  Root[1] := ':';
  Root[2] := '\';
  Root[3] := #0;
  Result := GetDiskFreeSpaceEx(@Root[0], UsableSize, TotalSize, @FreeSize);
  if not Result then begin
    UsableSize := 0;
    TotalSize := 0;
    FreeSize := 0;
  end;
end;

function PegtopGetVolumeInformation(Drive: Char; out VolumeName, FileSystemName: String;
  out SerialNumber, MaxFileNameLength, FileSystemFlags: Cardinal): Boolean;
var
  Root: array[0..4] of Char;
begin
  Root[0] := Drive;
  Root[1] := ':';
  Root[2] := '\';
  Root[3] := #0;
  SetLength(VolumeName, 256);
  SetLength(FileSystemName, 256);
  Result := GetVolumeInformation(@Root[0], PChar(VolumeName), Length(VolumeName),
    @SerialNumber, MaxFileNameLength, FileSystemFlags,
    PChar(FileSystemName), Length(FileSystemName));
  if Result then begin
    VolumeName := StrPas(PChar(VolumeName));
    FileSystemName := StrPas(PChar(FileSystemName));
  end
  else begin
    VolumeName := '';
    FileSystemName := '';
    SerialNumber := 0;
    MaxFileNameLength := 0;
    FileSystemFlags := 0;
  end;
end;

procedure PegtopForceForegroundWindow(Handle: THandle);
var
  SwitchToThisWindowProc: procedure (Handle: HWnd; AltTab: Bool); stdcall;
begin
  if GetForegroundWindow <> Handle then begin
    @SwitchToThisWindowProc := GetProcAddress(GetModuleHandle('user32.dll'), 'SwitchToThisWindow');
    if Assigned(SwitchToThisWindowProc) then begin
      SwitchToThisWindowProc(Handle, False);
    end
    else begin
      SetForegroundWindow(Handle);
    end;
  end;
end;

procedure PegtopTrimWorkingSetSize;
var
  Process: THandle;
begin
  Process := OpenProcess(PROCESS_SET_QUOTA, False, GetCurrentProcessId);
  try
    SetProcessWorkingSetSize(Process, $FFFFFFFF, $FFFFFFFF);
  finally
    CloseHandle(Process);
  end;
end;

function PegtopUserIsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (Value: (0,0,0,0,0,5));
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
  DOMAIN_ALIAS_RID_USERS : DWORD = $00000221;
  DOMAIN_ALIAS_RID_GUESTS: DWORD = $00000222;
  DOMAIN_ALIAS_RID_POWER_: DWORD = $00000223;
var
  AccessToken: THandle;
  Groups: PTokenGroups;
  InfoBufferSize: Longword;
  AdministratorsSid: PSID;
  I: Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin

    // get thread token:
    Result := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, AccessToken);

    // get process token:
    if (not Result) and (GetLastError = ERROR_NO_TOKEN) then
      Result := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, AccessToken);

    // examine token:
    if Result then try
      GetMem(Groups, 1024);
      try
        Result := GetTokenInformation(AccessToken, TokenGroups, Groups, 1024, InfoBufferSize);
        if InfoBufferSize > 1024 then begin
          // buffer too small, try again with a larger buffer
          FreeMem(Groups);
          GetMem(Groups, InfoBufferSize);
          Result := GetTokenInformation(AccessToken, TokenGroups, Groups, InfoBufferSize, InfoBufferSize);
        end;
        if Result then begin
          AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID,
            DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, AdministratorsSid);
          try
            Result := False;
            I := 0;
            while (not Result) and (I < Integer(Groups^.GroupCount)) do begin
              if EqualSid(AdministratorsSid, Groups^.Groups[I].Sid) then Result := True;
              Inc(I);
            end;
          finally
            FreeSid(AdministratorsSid);
          end;
        end;
      finally
        FreeMem(Groups);
      end;
    finally
      CloseHandle(AccessToken);
    end;

  end
  else begin
    // Windows 9x
    Result := True; // every user is administrator
  end;
end;

end.
