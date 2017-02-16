{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmPluginFuncs;

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

{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Math, Classes,
{ Graphics32 }
  GR32, GR32_LowLevel,
{ GraphicsMagic Lib }
  gmTypes;
  

  function CopyBmpDataFromPtr(const ABmpPtr: PColor32;
    const AWidth, AHeight: Integer; const ADestBmp: TBitmap32): Boolean;

  function CopyBmpDataToPtr(const ASourceBmp: TBitmap32;
    const ADestBmpPtr: PColor32; const ADestWidth, ADestHeight: Integer): Boolean;

  function GetColorChannelCount(const AChannelSet: TgmChannelSet): Integer;

  procedure ClampValue(var AValue: Integer; const AMinimumValue, AMaximumValue: Integer); overload;
  procedure ClampValue(var AValue: Real;    const AMinimumValue, AMaximumValue: Real);    overload;
  procedure ClampValue(var AValue: Double;  const AMinimumValue, AMaximumValue: Double);  overload;

  procedure DrawBitmap32WithARGB(const ASourceBmp, ADestBmp: TBitmap32;
    const AX, AY: Integer);

  procedure CopyRegion(const ASourceBmp, ADestBmp: TBitmap32;
    const AStartX, AStartY, AEndX, AEndY: Integer);

  procedure ExtractBitmapChannels(const ADestBmp: TBitmap32;
    const AChannelSet: TgmChannelSet);
    
  procedure ReplaceAlphaChannel(const ADestBmp, ASourceBmp: TBitmap32);

implementation

uses
{ GraphicsMagicLib }
  gmImageProcessFuncs;

function CopyBmpDataFromPtr(const ABmpPtr: PColor32;
  const AWidth, AHeight: Integer; const ADestBmp: TBitmap32): Boolean;
var
  LSrcPtr: PColor32;
  LDstPtr: PColor32;
  i      : Integer;
begin
  Result := False;

  if Assigned(ABmpPtr) and
     Assigned(ADestBmp) and
     (AWidth > 0) and
     (AHeight > 0) then
  begin
    LSrcPtr := ABmpPtr;

    ADestBmp.SetSize(AWidth, AHeight);
    LDstPtr := @ADestBmp.Bits[0];

    for i := 0 to (AWidth * AHeight - 1) do
    begin
      LDstPtr^ := LSrcPtr^;
      
      Inc(LSrcPtr);
      Inc(LDstPtr);
    end;

    Result := True;
  end;
end; 

function CopyBmpDataToPtr(const ASourceBmp: TBitmap32;
  const ADestBmpPtr: PColor32; const ADestWidth, ADestHeight: Integer): Boolean;
var
  LSrcPtr: PColor32;
  LDstPtr: PColor32;
  i      : Integer;
begin
  Result := False;
  
  if Assigned(ASourceBmp) and Assigned(ADestBmpPtr) then
  begin
    if (ASourceBmp.Width  = ADestWidth) and
       (ASourceBmp.Height = ADestHeight) then
    begin
      LSrcPtr := @ASourceBmp.Bits[0];
      LDstPtr := ADestBmpPtr;

      for i := 0 to (ADestWidth * ADestHeight - 1) do
      begin
        LDstPtr^ := LSrcPtr^;
        
        Inc(LSrcPtr);
        Inc(LDstPtr);
      end;
      
      Result := True;
    end;
  end;
end; 

function GetColorChannelCount(const AChannelSet: TgmChannelSet): Integer;
begin
  Result := 0;

  if csRed in AChannelSet then
  begin
    Inc(Result);
  end;

  if csGreen in AChannelSet then
  begin
    Inc(Result);
  end;

  if csBlue in AChannelSet then
  begin
    Inc(Result);
  end;
end; 

procedure ClampValue(var AValue: Integer;
  const AMinimumValue, AMaximumValue: Integer);
begin
  if AValue < AMinimumValue then
  begin
    AValue := AMinimumValue;
  end
  else if AValue > AMaximumValue then
  begin
    AValue := AMaximumValue;
  end;
end; 

procedure ClampValue(var AValue: Real;
  const AMinimumValue, AMaximumValue: Real);
begin
  if AValue < AMinimumValue then
  begin
    AValue := AMinimumValue;
  end
  else if AValue > AMaximumValue then
  begin
    AValue := AMaximumValue;
  end;
end; 

procedure ClampValue(var AValue: Double;
  const AMinimumValue, AMaximumValue: Double);
begin
  if AValue < AMinimumValue then
  begin
    AValue := AMinimumValue;
  end
  else if AValue > AMaximumValue then
  begin
    AValue := AMaximumValue;
  end;
end; 

procedure DrawBitmap32WithARGB(const ASourceBmp, ADestBmp: TBitmap32;
  const AX, AY: Integer);
var
  i, j   : Integer;
  LSrcRow: PColor32Array;
  LDstRow: PColor32Array;
begin
{$RANGECHECKS OFF}

  LSrcRow := nil;
  LDstRow := nil;

  if Assigned(ASourceBmp) and Assigned(ADestBmp) then
  begin
    for j := 0 to (ASourceBmp.Height - 1) do
    begin
      if ((j + AY) < 0) or ((j + AY) >= ADestBmp.Height) then
      begin
        Continue;
      end
      else
      begin
        LSrcRow := ASourceBmp.ScanLine[j];
        LDstRow := ADestBmp.ScanLine[j + AY];
      end;

      for i := 0 to (ASourceBmp.Width - 1) do
      begin
        if ((i + AX) < 0) or ((i + AX) >= ADestBmp.Width) then
        begin
          Continue;
        end
        else
        begin
          LDstRow[i + AX] := LSrcRow[i];
        end;
      end;
    end;
  end;

{$RANGECHECKS ON}
end; 

procedure CopyRegion(const ASourceBmp, ADestBmp: TBitmap32;
  const AStartX, AStartY, AEndX, AEndY: Integer);
var
  sx, sy, ex, ey: Integer;
  LRect         : TRect;
begin
  sx := MinIntValue([AStartX, AEndX]);
  sy := MinIntValue([AStartY, AEndY]);
  ex := MaxIntValue([AStartX, AEndX]);
  ey := MaxIntValue([AStartY, AEndY]);

  ClampValue(sx, 0, ASourceBmp.Width);
  ClampValue(sy, 0, ASourceBmp.Height);
  ClampValue(ex, 0, ASourceBmp.Width);
  ClampValue(ey, 0, ASourceBmp.Height);

  LRect := Rect(sx, sy, ex, ey);
  CopyRect32WithARGB(ADestBmp, ASourceBmp, LRect, $00FFFFFF);
end; 

procedure ExtractBitmapChannels(const ADestBmp: TBitmap32;
  const AChannelSet: TgmChannelSet);
var
  i, LColorChannelCount: Integer;
  p                    : PColor32;
  a, r, g, b           : Cardinal;
begin
  if Assigned(ADestBmp) then
  begin
    if csGrayscale in AChannelSet then
    begin
      Desaturate32(ADestBmp);
    end
    else
    begin
      LColorChannelCount := GetColorChannelCount(AChannelSet);

      if LColorChannelCount > 0 then
      begin
        if LColorChannelCount = 1 then
        begin
          if csRed in AChannelSet then
          begin
            p := @ADestBmp.Bits[0];

            for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
            begin
              a  := p^ and $FF000000;
              r  := p^ shr 16 and $FF;
              p^ := a or (r shl 16) or (r shl 8) or r;

              Inc(p);
            end;
          end;

          if csGreen in AChannelSet then
          begin
            p := @ADestBmp.Bits[0];

            for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
            begin
              a  := p^ and $FF000000;
              g  := p^ shr 8 and $FF;
              p^ := a or (g shl 16) or (g shl 8) or g;

              Inc(p);
            end;
          end;

          if csBlue in AChannelSet then
          begin
            p := @ADestBmp.Bits[0];

            for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
            begin
              a  := p^ and $FF000000;
              b  := p^ and $FF;
              p^ := a or (b shl 16) or (b shl 8) or b;

              Inc(p);
            end;
          end;
        end
        else if LColorChannelCount > 1 then
        begin
          p := @ADestBmp.Bits[0];

          for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
          begin
            a  := p^ and $FF000000;

            if csRed in AChannelSet then
            begin
              r := p^ and $FF0000;
            end
            else
            begin
              r := 0;
            end;

            if csGreen in AChannelSet then
            begin
              g := p^ and $FF00;
            end
            else
            begin
              g := 0;
            end;

            if csBlue in AChannelSet then
            begin
              b := p^ and $FF;
            end
            else
            begin
              b := 0;
            end;

            p^ := a or r or g or b;

            Inc(p);
          end;
        end;
      end;
    end;
  end;
end; 

procedure ReplaceAlphaChannel(const ADestBmp, ASourceBmp: TBitmap32);
var
  a, LDestRGB: Cardinal;
  i          : Integer;
  p1, p2     : PColor32;
begin
  if (ADestBmp.Width  = ASourceBmp.Width) and
     (ADestBmp.Height = ASourceBmp.Height) then
  begin
    p1 := @ADestBmp.Bits[0];
    p2 := @ASourceBmp.Bits[0];

    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      a        := p2^ and $FF000000;
      LDestRGB := p1^ and $FFFFFF;
      p1^      := a or LDestRGB;

      Inc(p1);
      Inc(p2);
    end;
  end;
end;

end.
