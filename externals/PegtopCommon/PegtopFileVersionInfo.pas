////////////////////////////////////////////////////////////////////////////////
// File:       PegtopFileVersionInfo.pas
// Classes:    TPegtopFileVersionInfo
// Version:    1.01
// Date:       11 Aug 2003 1.00
//             05 Sep 2005 1.01 (better language support)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopFileVersionInfo can be used to obtain the file version info for any
// file (with file version info included). The information is read as soon as
// the first property is accessed.
// ApplicationVersionInfo (this instance is created automatically) holds the
// file version info of the application (without much overhead if you don't make
// use of it).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopFileVersionInfo;

interface

uses
  Windows;

const
  PegtopFileVersionInfoCount = 10;

type
  TPegtopFileVersionInfo = class
  private
    FFileName: String;
    FLangCharSet: String;
    FValues: array[0..PegtopFileVersionInfoCount-1] of String;
    FValid: Boolean;
    FRead: Boolean;
    procedure Read;
    function GetValues(Index: Integer): String;
    function GetValid: Boolean;
  public
    constructor Create(AFileName: String; ALangCharSet: String = ''); // 040904E4 for English (USA)
    property Valid: Boolean read GetValid;
    property LangCharSet: String read FLangCharSet;
    property Values[Index: Integer]: String read GetValues;
    property CompanyName: String index 0 read GetValues;
    property FileDescription: String index 1 read GetValues;
    property FileVersion: String index 2 read GetValues;
    property InternalName: String index 3 read GetValues;
    property LegalCopyright: String index 4 read GetValues;
    property LegalTradeMarks: String index 5 read GetValues;
    property OriginalFilename: String index 6 read GetValues;
    property ProductName: String index 7 read GetValues;
    property ProductVersion: String index 8 read GetValues;
    property Comments: String index 9 read GetValues;
  end;

var
  ApplicationVersionInfo: TPegtopFileVersionInfo;

implementation

uses
  SysUtils;

constructor TPegtopFileVersionInfo.Create(AFileName: String; ALangCharSet: String = '');
begin
  FValid := False;
  FFileName := AFileName;
  FLangCharSet := ALangCharSet;
  FRead := False;
end;

procedure TPegtopFileVersionInfo.Read;
type
  THexValue = packed array[0..3] of Byte;
  PHexValue = ^THexValue;
const
  InfoID: array[0..PegtopFileVersionInfoCount-1] of String = (
    'CompanyName', 'FileDescription', 'FileVersion', 'InternalName',
    'LegalCopyright', 'LegalTradeMarks', 'OriginalFilename',
    'ProductName', 'ProductVersion', 'Comments');
var
  InfoSize: DWord;
  Buffer: Pointer;
  I: Integer;
  Value: PChar;
  HexValue: PHexValue absolute Value;
  Len: DWord;
  Dummy: DWord;
begin
  if not FRead then begin
    FRead := True;
    InfoSize := GetFileVersionInfoSize(PChar(FFileName), Dummy);
    if InfoSize > 0 then begin // version info supported
      GetMem(Buffer, InfoSize);
      try
        if GetFileVersionInfo(PChar(FFileName), 0, InfoSize, Buffer) then begin
          if FLangCharSet = '' then begin
            if VerQueryValue(Buffer, '\VarFileInfo\Translation', Pointer(HexValue), Len) then begin
              if Len = 4 then begin
                FLangCharSet := IntToHex(HexValue[2] or (HexValue[3] shl 8) or (HexValue[0] shl 16) or (HexValue[1] shl 24), 8);
              end;
            end;
          end;
          for I := 0 to PegtopFileVersionInfoCount - 1 do begin
            if VerQueryValue(Buffer, PChar('StringFileInfo\\' + FLangCharSet + '\\' + InfoID[I]),
              Pointer(Value), Len)
            then begin
              FValues[I] := Value;
            end
            else begin
              FValues[I] := '';
            end;
          end;
        end;
      finally
        FreeMem(Buffer);
      end;
      FValid := True;
    end;
  end;
end;

function TPegtopFileVersionInfo.GetValues(Index: Integer): String;
begin
  if (Index >= 0) and (Index < PegtopFileVersionInfoCount) then begin
    Read;
    Result := FValues[Index];
  end
  else begin
    Result := '';
  end;
end;

function TPegtopFileVersionInfo.GetValid: Boolean;
begin
  Read;
  Result := FValid;
end;

initialization
  ApplicationVersionInfo := TPegtopFileVersionInfo.Create(ParamStr(0));
  // ParamStr(0) is used instead of Application.ExeName to make this
  // unit work without the Forms unit (which is useful for creating
  // console applications etc.)
finalization
  ApplicationVersionInfo.Free;
end.
