{ -- LICENSE -------------------------------------------------------------------

  This unit is written by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
  at 2011-02-23.

  Based on gtk\gtkenums.h of gtk+ 2.8.9

  GTK - The GIMP Toolkit
  Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald

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

  -- COMMENTS FROM GTK+ TEAM ---------------------------------------------------
  
  Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
  file for a list of people on the GTK+ Team.  See the ChangeLog
  files for a list of changes.  These files are distributed with
  GTK+ at ftp://ftp.gtk.org/pub/gtk/.
}

unit gmGtkEnums;

interface

type
  // anchor types
  TGtkAnchorType = (gtkatCenter,
                    gtkatNorth,
                    gtkatNorthWest,
                    gtkatNorthEast,
                    gtkatSouth,
                    gtkatSouthWest,
                    gtkatSouthEast,
                    gtkatWest,
                    gtkatEast);

  TGtkOrientation = (GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL);
                    
implementation

end.
