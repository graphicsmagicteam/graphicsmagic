{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  This unit is base on color_transfer.c code of GIMP 1.2.4 .

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit gmColorTransfer;

interface

  // Color transfer pocedure
  procedure ColorTransferInit;

var
  // for lightening
  HighlightsAdd: array [0..255] of Double;
  MidtonesAdd  : array [0..255] of Double;
  ShadowsAdd   : array [0..255] of Double;

  // for darkening
  HighlightsSub: array [0..255] of Double;
  MidtonesSub  : array [0..255] of Double;
  ShadowsSub   : array [0..255] of Double;

implementation

// Color transfer pocedure
procedure ColorTransferInit;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    ShadowsSub[255 - i] := (  1.075 - 1 / ( i / 16.0 + 1 )  );
    HighlightsAdd[i]    := ShadowsSub[255 - i];

    MidtonesSub[i]      := 0.667 * (  1 - Sqr( (i - 127.0) / 127.0 )  );
    MidtonesAdd[i]      := MidtonesSub[i];

    HighlightsSub[i]    := 0.667 * (  1 - Sqr( (i - 127.0) / 127.0 )  );
    ShadowsAdd[i]       := HighlightsSub[i];
  end;
end; 

initialization
  // initialize the global varieties
  ColorTransferInit;

end.
