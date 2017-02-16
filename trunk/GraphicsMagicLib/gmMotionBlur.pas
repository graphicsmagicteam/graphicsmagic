{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmMotionBlur;

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
 * PAEz -- Speed up the algorithm.
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
  GR32, GR32_LowLevel;

  procedure MotionBlur(const ASourceBmp, ADestBmp: TBitmap32;
    const ARadius: Cardinal; const AAngle: Integer);

implementation

uses
  gmMath;  // PAEz_COS_TABLE, PAEz_SIN_TABLE

procedure MotionBlur(const ASourceBmp, ADestBmp: TBitmap32;
  const ARadius: Cardinal; const AAngle: Integer);
var
  LAngle          : Integer;
  i, j, k, x, y   : Integer;
  LDeltaX, LDeltaY: Integer;
  x1, y1, x2, y2  : Integer;
  a1, r1, g1, b1  : Cardinal;
  a2, r2, g2, b2  : Cardinal;
  sa, sr, sg, sb  : Cardinal;
  da, dr, dg, db  : Cardinal;
  LSrcRow, LDstRow: array of PColor32Array;
  LSinCalc        : array of Integer;
  LCosCalc        : array of Integer;
  LWidth, LHeight : LongInt;
begin
{$RANGECHECKS OFF}

  LAngle := AAngle;
  x1     := 0;
  y1     := 0;
  x2     := 0;
  y2     := 0;

  if (ASourceBmp.Width  = ADestBmp.Width) and
     (ASourceBmp.Height = ADestBmp.Height) then
  begin
    SetLength(LSrcRow, ASourceBmp.Height);
    SetLength(LDstRow, ADestBmp.Height);
    SetLength(LSinCalc, ARadius + 1);
    SetLength(LCosCalc, ARadius + 1);
    try
      if ( LAngle >= (-90) ) and (LAngle <= 90) then
      begin
        Inc(LAngle, 90);

        LWidth  := ASourceBmp.Width  - 1;
        LHeight := ASourceBmp.Height - 1;

        for k := 1 to ARadius do
        begin
          LSinCalc[k] := Round(  PAEz_SIN_TABLE[LAngle] * k  );
          LCosCalc[k] := Round(  PAEz_COS_TABLE[LAngle] * k  );
        end;

        // get all the access to the pixels of the image
        for i := 0 to LHeight do
        begin
          LSrcRow[i] := ASourceBmp.ScanLine[i];
          LDstRow[i] := ADestBmp.ScanLine[i];
        end;
        
        x := LCosCalc[ARadius];
        y := LSinCalc[ARadius];

        for j := 0 to LHeight do
        begin
          for i := 0 to LWidth do
          begin
            a1 := 0;
            r1 := 0;
            g1 := 0;
            b1 := 0;

            a2 := 0;
            r2 := 0;
            g2 := 0;
            b2 := 0;

            for k := 1 to ARadius do
            begin
              LDeltaX := LCosCalc[k];
              LDeltaY := LSinCalc[k];

              { if Angle < 0, sample the pixels of the top-left and bottom-right }
              if LAngle < 90 then
              begin
                if x = y then
                begin
                  x1 := i - k;
                  y1 := j - k;
                  x2 := i + k;
                  y2 := j + k;
                end
                else if x > y then
                begin
                  x1 := i - k;
                  y1 := j - LDeltaY;
                  x2 := i + k;
                  y2 := j + LDeltaY;
                end
                else if x < y then
                begin
                  x1 := i - LDeltaX;
                  y1 := j - k;
                  x2 := i + LDeltaX;
                  y2 := j + k;
                end;
              end
              else
              { if Angle >= 0, sample the pixels of the top-right and bottom-left }
              begin
                if x = y then
                begin
                  x1 := i + k;
                  y1 := j - k;
                  x2 := i - k;
                  y2 := j + k
                end
                else if x > y then
                begin
                  x1 := i + k;
                  y1 := j - LDeltaY;
                  x2 := i - k;
                  y2 := j + LDeltaY;
                end
                else if x < y then
                begin
                  x1 := i + LDeltaX;
                  y1 := j - k;
                  x2 := i - LDeltaX;
                  y2 := j + k;
                end;
              end;

              if x1 < 0 then
              begin
                x1 := 0;
              end
              else if x1 > LWidth then
              begin
                x1 := LWidth;
              end;
              
              if y1 < 0 then
              begin
                y1 := 0;
              end
              else if y1 > LHeight then
              begin
                y1 := LHeight;
              end;

              if x2 < 0 then
              begin
                x2 := 0;
              end
              else if x2 > LWidth then
              begin
                x2 := LWidth;
              end;
              
              if y2 < 0 then
              begin
                y2 := 0;
              end
              else if y2 > LHeight then
              begin
                y2 := LHeight;
              end;

              a1 := a1 + (LSrcRow[y1, x1] shr 24 and $FF);
              r1 := r1 + (LSrcRow[y1, x1] shr 16 and $FF);
              g1 := g1 + (LSrcRow[y1, x1] shr  8 and $FF);
              b1 := b1 + (LSrcRow[y1, x1]        and $FF);

              a2 := a2 + (LSrcRow[y2, x2] shr 24 and $FF);
              r2 := r2 + (LSrcRow[y2, x2] shr 16 and $FF);
              g2 := g2 + (LSrcRow[y2, x2] shr  8 and $FF);
              b2 := b2 + (LSrcRow[y2, x2]        and $FF);
            end;

            // Composition
            sa := LSrcRow[j, i] shr 24 and $FF;
            sr := LSrcRow[j, i] shr 16 and $FF;
            sg := LSrcRow[j, i] shr  8 and $FF;
            sb := LSrcRow[j, i]        and $FF;

            da := (sa + a1 + a2) div (ARadius * 2 + 1);
            dr := (sr + r1 + r2) div (ARadius * 2 + 1);
            dg := (sg + g1 + g2) div (ARadius * 2 + 1);
            db := (sb + b1 + b2) div (ARadius * 2 + 1);

            if da > 255 then
            begin
              da := 255;
            end;

            if dr > 255 then
            begin
              dr := 255;
            end;

            if dg > 255 then
            begin
              dg := 255;
            end;

            if db > 255 then
            begin
              db := 255;
            end;

            LDstRow[j, i] := (da shl 24) or (dr shl 16) or (dg shl 8) or db;
          end;
        end;
      end;
      
    finally
      SetLength(LSrcRow,  0);
      SetLength(LDstRow,  0);
      SetLength(LSinCalc, 0);
      SetLength(LCosCalc, 0);
    end;
  end;

{$RANGECHECKS ON}
end;

end.
