{ Found at: http://www.hi-components.com/ndownloads_addons.asp

  Author: Valentim Batista

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

unit gmRemoveRedEyes;

interface

uses
  Math, GR32;

type
  TARGB = record // color splitting type
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;

  PARGB      = ^TARGB;
  TArrayARGB = array [0..0] of TARGB;
  PArrayARGB = ^TArrayARGB;

  procedure RemoveRedEyes(bm: TBitmap32);

implementation

procedure RemoveRedEyes(bm: TBitmap32);
var
  x, y, w, h      : Integer;
  pixptr          : PARGB;
  nrv, bluf, redq : Single;
  powr, powb, powg: Single;
begin
{$RANGECHECKS OFF}

  w := bm.Width;
  h := bm.Height;

  for y := 0 to (h - 1) do
  begin
    for x := 0 to (w - 1) do
    begin
      pixptr := PARGB(bm.PixelPtr[x, y]);
      nrv    := pixptr^.g + pixptr^.b;

      if nrv < 1 then
      begin
        nrv := 1;
      end;

      if pixptr^.g > 1 then
      begin
        bluf := pixptr^.b / pixptr^.g;
      end
      else
      begin
        bluf := pixptr^.b;
      end;

      bluf := Max ( 0.5, Min ( 1.5, Sqrt(bluf) ) );
      redq := (pixptr^.r / nrv) * bluf;

      if redq > 0.7 then
      begin
        powr := 1.775 - (redq * 0.75 + 0.25);

        if powr < 0 then
        begin
          powr := 0;
        end;

        powr := powr * powr;
        powb := 1 - (1 - powr) / 2;
        powg := 1 - (1 - powr) / 4;

        pixptr^.r := Round (powr * pixptr^.r);
        pixptr^.b := Round (powb * pixptr^.b);
        pixptr^.g := Round (powg * pixptr^.g);
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

end.
