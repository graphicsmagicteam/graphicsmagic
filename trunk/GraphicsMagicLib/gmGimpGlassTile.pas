{ CopyRight(C) Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  This is the Glass Tile plug-in for the GIMP 1.2
  Version 1.02

  Copyright (C) 1997 Karl-Johan Andersson (t96kja@student.tdb.uu.se)

  We adapted the source code for supporting Graphics32.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{ This filter divide the image into square "glass"-blocks in which
  the image is refracted.

  The alpha-channel is left unchanged.

  Please send any comments or suggestions to
  Karl-Johan Andersson (t96kja@student.tdb.uu.se)

  May 2000 - tim copperfield [timecop@japan.co.jp]
  Added preview mode.
  Noticed there is an issue with the algorithm if odd number of rows or
  columns is requested.  Dunno why.  I am not a graphics expert :(

  May 2000 alt@gimp.org Made preview work and removed some boundary
  conditions that caused "streaks" to appear when using some tile spaces.
}

unit gmGimpGlassTile;

interface

uses
  GR32;

  procedure GimpGlassTile32(const ASourceBmp, ADestBmp: TBitmap32;
    const ATileWidth, ATileHeight: Integer);

implementation

procedure GimpGlassTile32(const ASourceBmp, ADestBmp: TBitmap32;
  const ATileWidth, ATileHeight: Integer);
var
  i, j                              : Integer;
  LHalfTileW, LXPlus, LXMitt, LXOffs: Integer;
  LHalfTileH, LYPlus, LYMitt, LYOffs: Integer;
  dx, sx, sy                        : Integer;
  LSrcRows, LDstRows                : array of PColor32Array;
begin
{$RANGECHECKS OFF}

  if ADestBmp.Width <> ASourceBmp.Width then
  begin
    ADestBmp.Width := ASourceBmp.Width;
  end;

  if ADestBmp.Height <> ASourceBmp.Height then
  begin
    ADestBmp.Height := ASourceBmp.Height;
  end;

  SetLength(LSrcRows, ASourceBmp.Height);
  SetLength(LDstRows, ASourceBmp.Height);

  for j := 0 to (ASourceBmp.Height - 1) do
  begin
    LSrcRows[j] := ASourceBmp.ScanLine[j];
    LDstRows[j] := ADestBmp.ScanLine[j];
  end;

  LHalfTileW := ATileWidth  div 2;
  LHalfTileH := ATileHeight div 2;
  LXPlus     := ATileWidth  mod 2;
  LYPlus     := ATileHeight mod 2;

  LYMitt := 0;
  LYOffs := 0;
  
  for j := 0 to (ASourceBmp.Height - 1) do
  begin
    sy := LYMitt + LYOffs * 2;
    Inc(LYOffs);

    if sy < 0 then
    begin
      sy := 0;
    end;

    if sy > (ASourceBmp.Height - 1) then
    begin
      sy := ASourceBmp.Height - 1;
    end;

    if LYOffs = LHalfTileH then
    begin
      LYMitt := LYMitt + ATileHeight;
      LYOffs := -LHalfTileH;
      LYOffs := LYOffs - LYPlus;
    end;

    LXMitt := 0;
    LXOffs := 0;
    
    for i := 0 to (ASourceBmp.Width - 1) do
    begin
      dx := LXMitt + LXOffs;
      sx := LXMitt + LXOffs * 2;

      if sx < 0 then
      begin
        sx := 0;
      end;

      if sx < (ASourceBmp.Width - 1) then
      begin
        LDstRows[j, dx] := LSrcRows[sy, sx];
      end
      else
      begin
        LDstRows[j, dx] := LSrcRows[sy, dx];
      end;

      Inc(LXOffs);
      
      if LXOffs = LHalfTileW then
      begin
        LXMitt := LXMitt + ATileWidth;
        LXOffs := -LHalfTileW;
        LXOffs := LXOffs - LXPlus;
      end;
    end;
  end;

  SetLength(LSrcRows, 0);
  SetLength(LDstRows, 0);

{$RANGECHECKS ON}
end; 

end.
