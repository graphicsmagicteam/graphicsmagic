////////////////////////////////////////////////////////////////////////////////
// File:       PegtopMutexSections.pas
// Components: TPegtopMutexSection
// Version:    1.00
// Date:       12 Jan 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopMutexSection creates a mutex, which can be entered / left to protect
// sections, which are using resources shared by more than one process. Other
// processes using the same resource must use a mutex with the same name.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopMutexSections;

interface

uses
  Windows, SysUtils;

type
  TPegtopMutexSection = class
  private
    FName: string;
    FHandle: THandle;
    FEntered: Integer;
    FIsFirstInstance: Boolean;
  public
    constructor Create(const AName: String; ImmediateEnter: Boolean = False);
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    property Handle: THandle read FHandle;
    property IsFirstInstance: Boolean read FIsFirstInstance;
  end;

  EPegtopMutexError = class(Exception);

implementation

constructor TPegtopMutexSection.Create(const AName: String; ImmediateEnter: Boolean = False);
begin
  FName := AName;
  FHandle := CreateMutex(NIL, ImmediateEnter, PChar(FName));
  if (FHandle = 0) then raise EPegtopMutexError.Create('Mutex could not be created.');
  FIsFirstInstance := GetLastError <> ERROR_ALREADY_EXISTS;
  FEntered := 0;
  if ImmediateEnter then Inc(FEntered);
end;

destructor TPegtopMutexSection.Destroy;
begin
  if (FHandle <> 0) then begin
    if not CloseHandle(FHandle) then raise EPegtopMutexError.Create('Mutex could not be closed.');
  end;
  inherited;
end;

procedure TPegtopMutexSection.Enter;
begin
  if FHandle <> 0 then begin
    WaitForSingleObject(FHandle, INFINITE);
    Inc(FEntered);
  end;
end;

procedure TPegtopMutexSection.Leave;
begin
  if FHandle <> 0 then begin
    ReleaseMutex(FHandle);
    if FEntered > 0 then Dec(FEntered);
  end;
end;

end.
