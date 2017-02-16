unit gmMiscFuncs;

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
{ Standard }
  Windows, Classes, 
{ Graphics32 }
  GR32, GR32_LowLevel;

  procedure OffsetRect(var ARect: TRect; const ADeltaX, ADeltaY: Integer);
  procedure OffsetPoint(var APoint: TPoint; const ADeltaX, ADeltaY: Integer);

  function MakeArrayOfFixedPoints(const APointArray: TArrayOfPoint): TArrayOfFixedPoint;

  // this routine is adapted from a demo program of GR32
  procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
    const ASmallPattern: Boolean = False); overload;

  // this routine is adapted from a demo program of GR32
  procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
    const ARect: TRect; const ASmallPattern: Boolean = False); overload;


implementation


procedure OffsetRect(var ARect: TRect; const ADeltaX, ADeltaY: Integer);
begin
  ARect := Rect(ARect.Left   + ADeltaX,
                ARect.Top    + ADeltaY,
                ARect.Right  + ADeltaX,
                ARect.Bottom + ADeltaY);
end; { OffsetRect }

procedure OffsetPoint(var APoint: TPoint; const ADeltaX, ADeltaY: Integer);
begin
  APoint.X := APoint.X + ADeltaX;
  APoint.Y := APoint.Y + ADeltaY;
end; { OffsetRect }

function MakeArrayOfFixedPoints(const APointArray: TArrayOfPoint): TArrayOfFixedPoint;
var
  i, LPointCount: Integer;
begin
  LPointCount := Length(APointArray);
  SetLength(Result, LPointCount);
  
  for i := 0 to (LPointCount - 1) do
  begin
    Result[i] := FixedPoint(APointArray[i]);
  end;
end; { MakeArrayOfFixedPoints }

// this routine is adapted from a demo program of GR32
procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32;
  const ASmallPattern: Boolean = False);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  i, LParity    : Integer;
  LLine1, LLine2: TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with ADestBmp do
  begin
    SetLength(LLine1, Width);
    SetLength(LLine2, Width);
    
    for i := 0 to (Width - 1) do
    begin
      if ASmallPattern then
      begin
        LParity := i shr 2 and $1;
      end
      else
      begin
        LParity := i shr 3 and $1;
      end;

      LLine1[i] := Colors[LParity];
      LLine2[i] := Colors[1 - LParity];
    end;
    
    for i := 0 to (Height - 1) do
    begin
      if ASmallPattern then
      begin
        LParity := i shr 2 and $1;
      end
      else
      begin
        LParity := i shr 3 and $1;
      end;
      
      if Boolean(LParity) then
      begin
        MoveLongword(LLine1[0], ScanLine[i]^, Width);
      end
      else
      begin
        MoveLongword(LLine2[0], ScanLine[i]^, Width);
      end;
    end;
  end;
end; { DrawCheckerboardPattern }

// this routine is adapted from a demo program of GR32
procedure DrawCheckerboardPattern(const ADestBmp: TBitmap32; const ARect: TRect;
  const ASmallPattern: Boolean = False);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  w, h, i, j    : Integer;
  x, y, LParity : Integer;
  LLine1, LLine2: TArrayOfColor32; // a buffer for a couple of scanlines
  LDestRow      : PColor32Array;
  LRect         : TRect;
begin
{$RANGECHECKS OFF}

  if not Assigned(ADestBmp) then
  begin
    Exit;
  end;

  LRect.Left   := Clamp(ARect.Left,   0, ADestBmp.Width);
  LRect.Right  := Clamp(ARect.Right,  0, ADestBmp.Width);
  LRect.Top    := Clamp(ARect.Top,    0, ADestBmp.Height);
  LRect.Bottom := Clamp(ARect.Bottom, 0, ADestBmp.Height);

  if (LRect.Left >= LRect.Right) or
     (LRect.Top  >= LRect.Bottom) then
  begin
    Exit;
  end;

  w := LRect.Right  - LRect.Left;
  h := LRect.Bottom - LRect.Top;

  SetLength(LLine1, w);
  SetLength(LLine2, w);

  for i := 0 to (w - 1) do
  begin
    if ASmallPattern then
    begin
      LParity := i shr 2 and $1;
    end
    else
    begin
      LParity := i shr 3 and $1;
    end;

    LLine1[i] := Colors[LParity];
    LLine2[i] := Colors[1 - LParity];
  end;
    
  for j := 0 to (h - 1) do
  begin
    y        := j + LRect.Top;
    LDestRow := ADestBmp.Scanline[y];

    if ASmallPattern then
    begin
      LParity := j shr 2 and $1;
    end
    else
    begin
      LParity := j shr 3 and $1;
    end;

    for i := 0 to (w - 1) do
    begin
      x := i + LRect.Left;

      if Boolean(LParity) then
      begin
        LDestRow[x] := LLine1[i];
      end
      else
      begin
        LDestRow[x] := LLine2[i];
      end;
    end;
  end;

{$RANGECHECKS ON}
end; { DrawCheckerboardPattern }

end.
