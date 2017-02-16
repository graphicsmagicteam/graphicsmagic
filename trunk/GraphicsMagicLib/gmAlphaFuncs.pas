unit gmAlphaFuncs;

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
 * Update Date: September 4th, 2014
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

{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows, GR32;

{ Alter alpha channel... }

  // Subtract the mask intensity from source alpha channel.
  procedure ChangeAlphaChannelBySubMask(
    const ADestBmp, ASrcMask, ANewMask: TBitmap32); overload;

  procedure ChangeAlphaChannelBySubMask(
    const ADestBmp, ASrcMask, ANewMask: TBitmap32; const ARect: TRect); overload;

  procedure InvertAlphaChannel(ADestBmp: TBitmap32);

  { Replace alpha channel... }
  procedure ReplaceAlphaChannelWithSource(const ADestBmp, ASourceBmp: TBitmap32);
  procedure ReplaceAlphaChannelWithMask(const ADestBmp, AMaskBmp: TBitmap32); overload;
  procedure ReplaceAlphaChannelWithMask(const ADestBmp, AMaskBmp: TBitmap32; const ARect: TRect); overload;
  procedure ReplaceAlphaChannelWithNewValue(const ADestBmp: TBitmap32; const AAlphaValue: Byte);
  procedure ReplaceAlphaChannelWithDifference(const ADestBmp, AOriginalBmp: TBitmap32);

  procedure ReplaceAlphaChannelWithNewValueRect(const ADestBmp: TBitmap32;
    const AAlphaChannel: Cardinal; const ARect: TRect);

  procedure MakeCanvasProcessedOpaque(const ADestBmp, AMaskBmp: TBitmap32);
  procedure GetAlphaChannelBitmap(const ASourceBmp, ADestBmp: TBitmap32); overload;
  procedure GetAlphaChannelBitmap(const ASourceBmp, ADestBmp: TBitmap32; const ARect: TRect); overload;
  procedure RestoreFloodFillAlphaChannel(const ACurrentColorFilled, ADifferentColorFilled: TBitmap32);

  // idea from Tommi Prami
  procedure SetAlphaForColor(const ABitmap: TBitmap32; const AColor: TColor32; const AAlpha: Byte = 0);

{ Process things with alpha channel... }

  // Make mask with the alpha channel of every pixels in source bitmap.
  procedure MakeMaskByAlphaChannel(const ADestBmp, ASourceBmp: TBitmap32; const ASourceRect: TRect);

implementation

uses
  GR32_LowLevel;

// Subtract the mask intensity from source alpha channel.
procedure ChangeAlphaChannelBySubMask(
  const ADestBmp, ASrcMask, ANewMask: TBitmap32);
var
  i, LIntensity, LDelta: Integer;
  LDestRGB             : Cardinal;
  LOldOpacity          : Integer;
  LNewOpacity          : Cardinal;
  s, d, m              : PColor32;
begin
  if (ADestBmp.Width  = ASrcMask.Width) and
     (ADestBmp.Height = ASrcMask.Height) and
     (ANewMask.Width  = ASrcMask.Width) and
     (ANewMask.Height = ASrcMask.Height)  then
  begin
    s := @ASrcMask.Bits[0];
    d := @ADestBmp.Bits[0];
    m := @ANewMask.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      // extract RGB components of destination bitmap
      LDestRGB := d^ and $FFFFFF;

      // calculating new alpha value
      LIntensity  := m^ and $FF;
      LOldOpacity := s^ and $FF;
      LDelta      := $FF - LIntensity;
      LNewOpacity := Clamp(LOldOpacity - LDelta, 0, 255);

      // combine
      d^ := (LNewOpacity shl 24) or LDestRGB;

      // advance pointers
      Inc(s);
      Inc(d);
      Inc(m);
    end;
  end;
end; 

// Subtract the mask intensity from source alpha channel.
procedure ChangeAlphaChannelBySubMask(
  const ADestBmp, ASrcMask, ANewMask: TBitmap32;
  const ARect: TRect);
var
  i, j, x, y, w, h, LDelta         : Integer;
  LOldOpacity, LNewOpacity         : Byte;
  LDstRow, LSrcMaskRow, LNewMaskRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  if (ADestBmp.Width  <> ASrcMask.Width) or
     (ADestBmp.Height <> ASrcMask.Height) then
  begin
    Exit;
  end;

  if (ANewMask.Width  <> ASrcMask.Width) or
     (ANewMask.Height <> ASrcMask.Height) then
  begin
    Exit;
  end;

  if (ARect.Left >= ARect.Right) or
     (ARect.Top  >= ARect.Bottom) then
  begin
    Exit;
  end;

  w := ARect.Right  - ARect.Left + 1;
  h := ARect.Bottom - ARect.Top  + 1;

  for j := 0 to h do
  begin
    y := j + ARect.Top;

    if (y < 0) or (y >= ADestBmp.Height) then
    begin
      Continue;
    end;

    LDstRow     := ADestBmp.ScanLine[y];
    LSrcMaskRow := ASrcMask.ScanLine[y];
    LNewMaskRow := ANewMask.ScanLine[y];

    for i := 0 to w do
    begin
      x := i + ARect.Left;

      if (x < 0) or (x >= ADestBmp.Width) then
      begin
        Continue;
      end;

      // calculating new alpha value
      LDelta      := $FF - (LNewMaskRow[x] and $FF);
      LOldOpacity := LSrcMaskRow[x] and $FF;
      LNewOpacity := Clamp(LOldOpacity - LDelta, 0, 255);

      // combine
      LDstRow[x] := (LNewOpacity shl 24) or (LDstRow[x] and $FFFFFF);
    end;
  end;

{$RANGECHECKS ON}
end;

// invert the alpha channel of each pixel on a bitmap
procedure InvertAlphaChannel(ADestBmp: TBitmap32);
var
  i : Integer;
  a : Cardinal;
  p : PColor32;
begin
  if Assigned(ADestBmp) then
  begin
    p := @ADestBmp.Bits[0];

    for i := 1 to (ADestBmp.Width * ADestBmp.Height) do
    begin
      a  := p^ shr 24 and $FF;
      a  := 255 - a;
      p^ := (a shl 24) or (p^ and $FFFFFF);

      Inc(p);
    end;
  end;
end;

procedure ReplaceAlphaChannelWithSource(const ADestBmp, ASourceBmp: TBitmap32);
var
  i               : Integer;
  LSrcBit, LDstBit: PColor32;
begin
  if (ADestBmp.Width  = ASourceBmp.Width) and
     (ADestBmp.Height = ASourceBmp.Height) then
  begin
    LSrcBit := @ASourceBmp.Bits[0];
    LDstBit := @ADestBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      LDstBit^ := (LSrcBit^ and $FF000000) or (LDstBit^ and $FFFFFF);

      Inc(LSrcBit);
      Inc(LDstBit);
    end;
  end;
end;

// Replace alpha channels with mask intensity.
procedure ReplaceAlphaChannelWithMask(const ADestBmp, AMaskBmp: TBitmap32);
var
  i                   : Integer;
  da, LDestRGB        : Cardinal;
  mr, mg, mb          : Byte;
  LDestBits, LMaskBits: PColor32;
begin
  if (ADestBmp.Width <= 0) or (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  if (ADestBmp.Width  <> AMaskBmp.Width) or
     (ADestBmp.Height <> AMaskBmp.Height) then
  begin
    Exit;
  end;

  LDestBits := @ADestBmp.Bits[0];
  LMaskBits := @AMaskBmp.Bits[0];
  
  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    mr         := LMaskBits^ shr 16 and $FF;
    mg         := LMaskBits^ shr  8 and $FF;
    mb         := LMaskBits^        and $FF;
    da         := (mr + mg + mb) div 3;
    LDestRGB   := LDestBits^ and $FFFFFF;
    LDestBits^ := (da shl 24) or LDestRGB;

    Inc(LDestBits);
    Inc(LMaskBits);
  end;
end; 

procedure ReplaceAlphaChannelWithMask(const ADestBmp, AMaskBmp: TBitmap32;
  const ARect: TRect);
var
  i, j, x, y, w, h  : Integer;
  mr, mg, mb, LAlpha: Byte;
  LDestRow, LMaskRow: PColor32Array;
  LDestRGB          : Cardinal;
begin
{$RANGECHECKS OFF}

  if (ADestBmp.Width <= 0) or (ADestBmp.Height <= 0) then
  begin
    Exit;
  end;

  if (ADestBmp.Width  <> AMaskBmp.Width) or
     (ADestBmp.Height <> AMaskBmp.Height) then
  begin
    Exit;
  end;

  if (ARect.Left >= ARect.Right) or
     (ARect.Top  >= ARect.Bottom) then
  begin
    Exit;
  end;

  w := ARect.Right  - ARect.Left + 1;
  h := ARect.Bottom - ARect.Top  + 1;

  for j := 0 to h do
  begin
    y := ARect.Top + j;

    if (y < 0) or (y >= ADestBmp.Height) then
    begin
      Continue;
    end;

    LDestRow := ADestBmp.ScanLine[y];
    LMaskRow := AMaskBmp.ScanLine[y];

    for i := 0 to w do
    begin
      x := ARect.Left + i;

      if (x < 0) or (x >= ADestBmp.Width) then
      begin
        Continue;
      end;

      mr          := LMaskRow[x] shr 16 and $FF;
      mg          := LMaskRow[x] shr  8 and $FF;
      mb          := LMaskRow[x]        and $FF;
      LAlpha      := (mr + mg + mb) div 3;
      LDestRGB    := LDestRow[x] and $FFFFFF;
      LDestRow[x] := (LAlpha shl 24) or LDestRGB;
    end;
  end;

{$RANGECHECKS ON}
end; 

procedure ReplaceAlphaChannelWithNewValue(const ADestBmp: TBitmap32;
  const AAlphaValue: Byte);
var
  i        : Integer;
  LNewAlpha: Cardinal;
  LDestBits: PColor32;
begin
  LNewAlpha := AAlphaValue shl 24;
  LDestBits := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    if (LDestBits^ and $FF000000) <> LNewAlpha then
    begin
      LDestBits^ := LNewAlpha or (LDestBits^ and $FFFFFF);
    end;

    Inc(LDestBits);
  end;
end; 

procedure ReplaceAlphaChannelWithDifference(
  const ADestBmp, AOriginalBmp: TBitmap32);
var
  i     : Integer;
  p1, p2: PColor32;
begin
  if (ADestBmp.Width  = AOriginalBmp.Width) and
     (ADestBmp.Height = AOriginalBmp.Height) then
  begin
    p1 := @ADestBmp.Bits[0];
    p2 := @AOriginalBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      if p1^ <> p2^ then
      begin
        p1^ := p2^;
      end;

      Inc(p1);
      Inc(p2);
    end;
  end;
end;

procedure ReplaceAlphaChannelWithNewValueRect(const ADestBmp: TBitmap32;
  const AAlphaChannel: Cardinal; const ARect: TRect);
var
  i, j       : Integer;
  LRectWidth : Integer;
  LRectHeight: Integer;
  LDestRGB   : Cardinal;
  LDestRow   : PColor32Array;
begin
{$RANGECHECKS OFF}

  LRectWidth  := ARect.Right  - ARect.Left - 1;
  LRectHeight := ARect.Bottom - ARect.Top  - 1;

  for j := 0 to LRectHeight do
  begin
    if ( (ARect.Top + j) < 0 ) or
       ( (ARect.Top + j) > (ADestBmp.Height - 1) ) then
    begin
      Continue;
    end
    else
    begin
      LDestRow := ADestBmp.ScanLine[ARect.Top + j];

      for i := 0 to LRectWidth do
      begin
        if ( (ARect.Left + i) < 0 ) or
           ( (ARect.Left + i) > (ADestBmp.Width - 1) ) then
        begin
          Continue;
        end
        else
        begin
          LDestRGB                 := LDestRow[ARect.Left + i] and $FFFFFF;
          LDestRow[ARect.Left + i] := (AAlphaChannel shl 24) or LDestRGB;
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}  
end; 

procedure MakeCanvasProcessedOpaque(const ADestBmp, AMaskBmp: TBitmap32);
var
  i     : Integer;
  p1, p2: PColor32;
begin
  if (ADestBmp.Width  = AMaskBmp.Width) and
     (ADestBmp.Height = AMaskBmp.Height) then
  begin
    p1 := @ADestBmp.Bits[0];
    p2 := @AMaskBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      if (p2^ and $FFFFFF) > 0 then
      begin
        p1^ := p1^ or $FF000000;
      end;
      
      Inc(p1);
      Inc(p2);
    end;
  end;
end; 

procedure GetAlphaChannelBitmap(const ASourceBmp, ADestBmp: TBitmap32);
var
  a   : Cardinal;
  i   : Integer;
  s, d: PColor32;
begin
  if ADestBmp.DrawMode <> dmOpaque then
  begin
    ADestBmp.DrawMode := dmOpaque;
  end;

  ADestBmp.SetSize(ASourceBmp.Width, ASourceBmp.Height);
  ADestBmp.Clear(clBlack32);
  
  s := @ASourceBmp.Bits[0];
  d := @ADestBmp.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    a  := s^ shr 24 and $FF;
    d^ := $FF000000 or (a shl 16) or (a shl 8) or a;
    
    Inc(s);
    Inc(d);
  end;
end; 

procedure GetAlphaChannelBitmap(const ASourceBmp, ADestBmp: TBitmap32;
  const ARect: TRect);
var
  a               : Cardinal;
  i, j, w, h, x, y: Integer;
  LSrcRow, LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  if (ASourceBmp.Width  <> ADestBmp.Width) or
     (ASourceBmp.Height <> ADestBmp.Height) then
  begin
    Exit;
  end;

  if (ARect.Top  >= ARect.Bottom) or
     (ARect.Left >= ARect.Right) then
  begin
    Exit;
  end;

  if ADestBmp.DrawMode <> dmOpaque then
  begin
    ADestBmp.DrawMode := dmOpaque;
  end;

  w := ARect.Right  - ARect.Left;
  h := ARect.Bottom - ARect.Top;

  for j := 0 to h do
  begin
    y := j + ARect.Top;

    if (y < 0) or (y >= (ADestBmp.Height - 1)) then
    begin
      Continue;
    end;

    LSrcRow := ASourceBmp.ScanLine[y];
    LDstRow := ADestBmp.ScanLine[y];

    for i := 0 to w do
    begin
      x := i + ARect.Left;

      if (x < 0) or (x > (ADestBmp.Width - 1)) then
      begin
        Continue;
      end;

      a          := LSrcRow[x] shr 24 and $FF;
      LDstRow[x] := $FF000000 or (a shl 16) or (a shl 8) or a;
    end;
  end;
  
{$RANGECHECKS ON}
end; 

procedure RestoreFloodFillAlphaChannel(
  const ACurrentColorFilled, ADifferentColorFilled: TBitmap32);
var
  i     : Integer;
  p1, p2: PColor32;
begin
  if (ACurrentColorFilled.Width  = ADifferentColorFilled.Width) and
     (ACurrentColorFilled.Height = ADifferentColorFilled.Height) then
  begin
    p1 := @ACurrentColorFilled.Bits[0];
    p2 := @ADifferentColorFilled.Bits[0];

    for i := 0 to (ACurrentColorFilled.Width * ACurrentColorFilled.Height - 1) do
    begin
      if p1^ <> p2^ then
      begin
        p1^ := p1^ or $FF000000;
      end;
      
      Inc(p1);
      Inc(p2);
    end;
  end;
end; 

// idea from Tommi Prami
procedure SetAlphaForColor(const ABitmap: TBitmap32; const AColor: TColor32;
  const AAlpha: Byte = 0);
var
  i: Integer;
  p: PColor32;
begin
  // set alpha for given color on all pixels
  if Assigned(ABitmap) then
  begin
    if (ABitmap.Width  = 0) or
       (ABitmap.Height = 0) then
    begin
      Exit;
    end;

    p := @ABitmap.Bits[0];
    
    for i := 0 to (ABitmap.Width * ABitmap.Height - 1) do
    begin
      if (p^ and $FFFFFF) = (AColor and $FFFFFF) then
      begin
        p^ := (AAlpha shl 24) or (p^ and $FFFFFF);
      end;
      
      Inc(p);
    end;
  end;
end; 

// Make mask with the alpha channel of every pixels in source bitmap.
procedure MakeMaskByAlphaChannel(const ADestBmp, ASourceBmp: TBitmap32;
  const ASourceRect: TRect);
var
  i, j, LWidth, LHeight: Integer;
  a                    : Cardinal;
  LSrcRow, LDstRow     : PColor32Array;
begin
{$RANGECHECKS OFF}

  LSrcRow := nil;
  LDstRow := nil;

  if ADestBmp.DrawMode <> dmOpaque then
  begin
    ADestBmp.DrawMode := dmOpaque;
  end;

  LWidth  := ASourceRect.Right  - ASourceRect.Left + 1;
  LHeight := ASourceRect.Bottom - ASourceRect.Top  + 1;

  if (LWidth  <= ASourceBmp.Width) and
     (LHeight <= ASourceBmp.Height) then
  begin
    ADestBmp.SetSize(LWidth, LHeight);

    for j := 0 to (LHeight - 1) do
    begin
      if ( (ASourceRect.Top + j) < 0 ) or
         ( (ASourceRect.Top + j) > (ASourceBmp.Height - 1) ) then
      begin
        Continue;
      end
      else
      begin
        LSrcRow := ASourceBmp.ScanLine[ASourceRect.Top + j];
        LDstRow := ADestBmp.ScanLine[j];
      end;

      for i := 0 to (LWidth - 1) do
      begin
        if ( (ASourceRect.Left + i) < 0 ) or
           ( (ASourceRect.Left + i) > (ASourceBmp.Width - 1) ) then
        begin
          Continue;
        end
        else
        begin
          a          := LSrcRow[ASourceRect.Left + i] shr 24 and $FF;
          LDstRow[i] := $FF000000 or (a shl 16) or (a shl 8) or a;
        end;
      end;
    end
  end;

{$RANGECHECKS ON}
end; 

end.
