{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Many thanks to authors of GIMP -- Spencer Kimball and Peter Mattis.

  Based on the the Gimp 2.2.10 .
  The original source can be found at www.gimp.org.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA. }

unit gmGimpCommonFuncs;

interface

  function CLAMP(const n, AMin, AMax: Integer): Integer; overload;
  function CLAMP(const n, AMin, AMax: Double): Double; overload;
  function CLAMP(const n: Double; const AMin, AMax: Integer): Integer; overload;
  function CLAMP0255(const n: Integer): Integer;

const
  G_MAXINT = 2147483647;

implementation

function CLAMP(const n, AMin, AMax: Integer): Integer;
begin
  if n < AMin then
  begin
    Result := AMin;
  end
  else if n > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := n;
  end;
end;

function CLAMP(const n, AMin, AMax: Double): Double;
begin
  if n < AMin then
  begin
    Result := AMin;
  end
  else if n > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := n;
  end;
end;

function CLAMP(const n: Double; const AMin, AMax: Integer): Integer;
var
  rn: Integer;
begin
  rn := Round(n);

  if rn < AMin then
  begin
    Result := AMin;
  end
  else if rn > AMax then
  begin
    Result := AMax;
  end
  else
  begin
    Result := rn;
  end;
end;

function CLAMP0255(const n: Integer): Integer;
begin
  Result := CLAMP(n, 0, 255);
end; 

end.
