{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-2011, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.

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

{------------------------------------------------------------------------------}
{ GraphicsMagic specific mappings                                              }
{------------------------------------------------------------------------------}

{$I GR32.inc}

//Use FastMM4
{$DEFINE USE_FASTMM4}

//Use FastCode
{.$IFNDEF COMPILERXE1_UP}
{$IFNDEF COMPILER2006_UP}
  {$DEFINE USE_FASTCODE}
{$ENDIF}

//Use FastMove
{.$IFNDEF COMPILERXE1_UP}
{$IFNDEF COMPILER2006_UP}
  {$DEFINE USE_FASTMOVE}
{$ENDIF}

//Use VCLFixPack
{.$IFNDEF COMPILERXE1_UP}
{$IFNDEF COMPILER2006_UP}
  {$DEFINE USE_VCLFIXPACK}
{$ENDIF}

//Use RtlVclOptimize
{.$IFNDEF COMPILERXE1_UP}
{$IFNDEF COMPILER2006_UP}
  {$DEFINE USE_RTLVCLOPTIMIZE}
{$ENDIF}

