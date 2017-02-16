{ This library created in 01/27/2006
  CopyRight(C) 2006, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

  Based on "gdkdrawable-win32.h" and "gdkdrawable-win32.c" from Gtk+ 2.8.9 .

  Many thanks to GTK+ Team and others.

  ******************************************************************************

  GDK - The GIMP Drawing Kit
  Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
  Copyright (C) 1998-2004 Tor Lillqvist
  Copyright (C) 2001-2005 Hans Breuer

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

  Modified by the GTK+ Team and others 1997-2000. See the AUTHORS
  file for a list of people on the GTK+ Team.  See the ChangeLog
  files for a list of changes.  These files are distributed with
  GTK+ at ftp://ftp.gtk.org/pub/gtk/. }

unit gmGdkDrawable_Win32;

interface

uses
  Graphics;

  procedure GDK_Win32_Draw_Arc(const ACanvas: TCanvas; const AFilled: Boolean;
    const AX, AY, AWidth, AHeight, Angle1, Angle2: Integer);

implementation

uses
  Math;

procedure GDK_Win32_Draw_Arc(const ACanvas: TCanvas; const AFilled: Boolean;
  const AX, AY, AWidth, AHeight, Angle1, Angle2: Integer);
var
  nXStartArc, nYStartArc, nXEndArc, nYEndArc: Integer;
begin
  if Angle2 >= (360*64) then
  begin
    nXStartArc := 0;
    nYStartArc := 0;
    nXEndArc   := 0;
    nYEndArc   := 0;
  end
  else
  if Angle2 > 0 then
  begin
    nXStartArc := Round(  AX + AWidth  / 2 + AWidth     * Cos(Angle1 / 64.0 * 2.0 * PI / 360.0) );
    nYStartArc := Round(  AY + AHeight / 2 + (-AHeight) * Sin(Angle1 / 64.0 * 2.0 * PI / 360.0));
    nXEndArc   := Round(  AX + AWidth  / 2 + AWidth     * Cos( (Angle1 + Angle2) / 64.0 * 2.0 * PI / 360.0 )  );
    nYEndArc   := Round(  AY + AHeight / 2 + (-AHeight) * Sin( (Angle1 + Angle2) / 64.0 * 2.0 * PI / 360.0 )  );
  end
  else
  begin
    nXEndArc   := Round(  AX + AWidth  / 2 + AWidth     * Cos(Angle1 / 64.0 * 2.0 * PI / 360.0) );
    nYEndArc   := Round(  AY + AHeight / 2 + (-AHeight) * Sin(Angle1 / 64.0 * 2.0 * PI / 360.0) );
    nXStartArc := Round(  AX + AWidth  / 2 + AWidth     * Cos( (Angle1 + Angle2) / 64.0 * 2.0 * PI / 360.0 )  );
    nYStartArc := Round(  AY + AHeight / 2 + (-AHeight) * Sin( (Angle1 + Angle2) / 64.0 * 2.0 * PI / 360.0 )  );
  end;

  if AFilled then
  begin
    ACanvas.Pie(AX, AY, AX + AWidth, AY + AHeight, nXStartArc, nYStartArc, nXEndArc, nYEndArc);
  end
  else
  begin
    ACanvas.Arc(AX, AY, AX + AWidth, AY + AHeight, nXStartArc, nYStartArc, nXEndArc, nYEndArc);
  end;
end; 

end.
 