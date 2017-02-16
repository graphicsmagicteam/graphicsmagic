{ Exponential Blur for GR32

  Copyright (C) 2008 by Ma Xiaoguang & Ma Xiaoming <gmbros@hotmail.com>.
  All rights reserved.

  We translated this algorithm from C/C++ to Pascal.

  The author of original C/C++ code is Petri Damsten ( petri.damsten@iki.fi )

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the
  Free Software Foundation, Inc.,
  59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit gmExpBlur;

interface

uses
  SysUtils, GR32;

  procedure ExpBlur(const ADestBmp: TBitmap32; const ARadius: Integer);

implementation

const
  aprec = 16; // precision of alpha parameter
  zprec =  7; // precision of state parameters

procedure BlurInner(var AColor: TColor32; var zR, zG, zB, zA: Cardinal;
  const AAlpha: Integer);
var
  { NOTE:

    The original type of the following variables are Integer, and the code
    works well. But the Integer type will cause Delphi reports Warnings/Hints
    information at the bottom of the code editor when compile this unit.

    Steven Smith (who is from Graphics32 newsgroup) has handled the problem by
    changing the type of these variables to Int64. And the code works well, too.

    Many thanks for Steven Smith for the help.
  }
  
  R, G, B, A: Int64; 
begin
{$RANGECHECKS OFF}

  A := AColor shr 24 and $FF;
  R := AColor shr 16 and $FF;
  G := AColor shr  8 and $FF;
  B := AColor        and $FF;

  zA := zA + ((AAlpha * ((A shl zprec) - zA)) shr aprec);
  zR := zR + ((AAlpha * ((R shl zprec) - zR)) shr aprec);
  zG := zG + ((AAlpha * ((G shl zprec) - zG)) shr aprec);
  zB := zB + ((AAlpha * ((B shl zprec) - zB)) shr aprec);

  A := zA shr zprec;
  R := zR shr zprec;
  G := zG shr zprec;
  B := zB shr zprec;

  AColor := (A shl 24) or (R shl 16) or (G shl 8) or B;
  
{$RANGECHECKS ON}
end;

procedure BlurRow(const ADestBmp: TBitmap32; const ALine, AAlpha: Integer);
var
  zR, zG, zB, zA: Cardinal;
  LIndex        : Integer;
  ptr           : PColor32Array;
begin
{$RANGECHECKS OFF}

  ptr := ADestBmp.ScanLine[ALine];

  zA := (ptr[0] shr 24 and $FF) shl zprec;
  zR := (ptr[0] shr 16 and $FF) shl zprec;
  zG := (ptr[0] shr  8 and $FF) shl zprec;
  zB := (ptr[0]        and $FF) shl zprec;

  for LIndex := 1 to (ADestBmp.Width - 1) do
  begin
    BlurInner(ptr[LIndex], zR, zG, zB, zA, AAlpha);
  end;

  for LIndex := (ADestBmp.Width - 2) downto 0 do
  begin
    BlurInner(ptr[LIndex], zR, zG, zB, zA, AAlpha);
  end;

{$RANGECHECKS ON}
end;

{ Version 1 -- We think that this is more accurate than Version 2 technically.
  But the effects of both versions are similar. But version 1 may cause range
  checking error. }
procedure BlurCol(const ADestBmp: TBitmap32; const ACol, AAlpha: Integer);
var
  zR, zG, zB, zA: Cardinal;
  Index         : Integer;
  ptr           : PColor32Array;
begin
{$RANGECHECKS OFF}

  ptr := ADestBmp.ScanLine[0];

  zA := (ptr[ACol] shr 24 and $FF) shl zprec;
  zR := (ptr[ACol] shr 16 and $FF) shl zprec;
  zG := (ptr[ACol] shr  8 and $FF) shl zprec;
  zB := (ptr[ACol]        and $FF) shl zprec;

  for Index := 1 to (ADestBmp.Height - 1) do
  begin
    ptr := ADestBmp.ScanLine[Index];
    BlurInner(ptr[ACol], zR, zG, zB, zA, AAlpha);
  end;

  for Index := (ADestBmp.Height - 2) downto 0 do
  begin
    ptr := ADestBmp.ScanLine[Index];
    BlurInner(ptr[ACol], zR, zG, zB, zA, AAlpha);
  end;

{$RANGECHECKS ON}
end;

// Version 2
{procedure BlurCol(DestBmp: TBitmap32; const Col, Alpha: Integer);
var
  zR, zG, zB, zA: Cardinal;
  Index         : Integer;
  ptr           : PColor32;
begin
  ptr := @DestBmp.Bits[Col];

  zA := (ptr^ shr 24 and $FF) shl zprec;
  zR := (ptr^ shr 16 and $FF) shl zprec;
  zG := (ptr^ shr  8 and $FF) shl zprec;
  zB := (ptr^        and $FF) shl zprec;

  Inc(ptr, DestBmp.Width);
  for Index := 1 to DestBmp.Height - 1 do
  begin
    BlurInner(ptr^, zR, zG, zB, zA, Alpha);
    Inc(ptr, DestBmp.Width);
  end;

  Dec(ptr, DestBmp.Width);
  for Index := 1 to DestBmp.Height - 1 do
  begin
    BlurInner(ptr^, zR, zG, zB, zA, Alpha);
    Dec(ptr, DestBmp.Width);
  end;
end;}

{*
 * ExpBlur(const ADestBmp: TBitmap32; const ARadius: Integer)
 *
 * In-place blur of image 'ADestBmp' with kernel
 * of approximate radius 'radius'.
 *
 * Blurs with two sided exponential impulse
 * response.
 *
 * aprec = precision of alpha parameter
 * in fixed-point format 0.aprec
 *
 * zprec = precision of state parameters
 * zR,zG,zB and zA in fp format 8.zprec
 *}
procedure ExpBlur(const ADestBmp: TBitmap32; const ARadius: Integer);
var
  LRow, LCol: Integer;
  LAlpha   : Integer;
begin
  if ARadius < 1 then
  begin
    Exit;
  end;

  { Calculate the alpha such that 90% of
    the kernel is within the radius.
    (Kernel extends to infinity) }

  LAlpha := Round( (1 shl aprec) * (1.0 - Exp(-2.3 / (ARadius + 1.0))) );

  for LRow := 0 to (ADestBmp.Height - 1) do
  begin
    BlurRow(ADestBmp, LRow, LAlpha);
  end;

  for LCol := 0 to (ADestBmp.Width - 1) do
  begin
    BlurCol(ADestBmp, LCol, LAlpha);
  end;
end;

end.
