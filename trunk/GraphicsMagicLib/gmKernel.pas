{ Written by Ma Xiaoguang, Ma Xiaoming ( gmbros@hotmail.com )
  Update Date: 2012-02-17 }

unit gmKernel;

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

interface

uses
  Classes;

type
  { The Kernel class defines a matrix that describes how a specified pixel and
    its surrounding pixels affect the value computed for the pixel's position
    in the output image of a filtering operation. The X origin and Y origin
    indicate the kernel matrix element that corresponds to the pixel position
    for which an output value is being computed.

    The X origin is (width-1)/2 and the Y origin is (height-1)/2.}
    
  TgmKernel = class(TPersistent)
  private
    FData      : array of Single;
    FWidth     : Integer;
    FHeight    : Integer;
    FKernelSize: Integer;

    function GetKernelData(AIndex: Integer): Single;
    function GetXOrigin: Integer;
    function GetYOrigin: Integer;
  public
    constructor Create(const AWidth, AHeight: Integer;
      const AData: array of Single);

    destructor Destroy; override;

    procedure SetKernelData(const AWidth, AHeight: Integer;
      const AData: array of Single);

    property Data[index: Integer]: Single  read GetKernelData;
    property Width               : Integer read FWidth;
    property Height              : Integer read FHeight;
    property KenelSize           : Integer read FKernelSize;
    property XOrigin             : Integer read GetXOrigin;
    property YOrigin             : Integer read GetYOrigin;
  end;

implementation

uses
  SysUtils;

constructor TgmKernel.Create(const AWidth, AHeight: Integer;
  const AData: array of Single);
begin
  inherited Create;

  FWidth      := 0;
  FHeight     := 0;
  FKernelSize := 0;

  SetKernelData(AWidth, AHeight, AData);
end;

destructor TgmKernel.Destroy;
begin
  SetLength(FData, 0);
  FData := nil;
  
  inherited Destroy;
end;

function TgmKernel.GetKernelData(AIndex: Integer): Single;
begin
  if (AIndex >= 0) and (AIndex < FKernelSize) then
  begin
    Result := FData[AIndex];
  end
  else
  begin
    raise Exception.Create('The index is out of the range');
  end;
end;

function TgmKernel.GetXOrigin: Integer;
begin
  Result := (FWidth - 1) div 2;
end;

function TgmKernel.GetYOrigin: Integer;
begin
  Result := (FHeight - 1) div 2;
end;

procedure TgmKernel.SetKernelData(const AWidth, AHeight: Integer;
  const AData: array of Single);
var
  i, LKernelSize: Integer;
begin
  if AWidth <= 0 then
  begin
    raise Exception.Create('Kenel width is less than or equal to zero.');
  end;

  if AHeight <= 0 then
  begin
    raise Exception.Create('Kernel height is less than or equal to zero.');
  end;

  LKernelSize := AWidth * AHeight;

  if Length(AData) < LKernelSize then
  begin
    raise Exception.Create('The length of the data array is less than the kernel size.');
  end;

  FWidth      := AWidth;
  FHeight     := AHeight;
  FKernelSize := LKernelSize;

  SetLength(FData, FKernelSize);

  for i := 0 to (FKernelSize - 1) do
  begin
    FData[i] := AData[i];
  end;
end; 

end.
