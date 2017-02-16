////////////////////////////////////////////////////////////////////////////////
// File:       PegtopResampleUtils.pas
// Version:    1.01
// History:    1.00 16 Mar 2005 created
//             1.02 13 Nov 2005 support for non-MMX CPUs added
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Procedures to resample bitmaps using MMX.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopResampleUtils;

interface

uses
  Windows, PegtopCPUUtils;

// Bitmap resampling (without clipping):
procedure PegtopResample2x32(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
procedure PegtopResample4x32(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);

implementation

////////////////////////////////////////////////////////////////////////////////
// Bitmap resampling (without clipping)
////////////////////////////////////////////////////////////////////////////////

procedure PegtopResample2x32CPU(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
var
  X, Y: Integer;
  DestQ, DestP: ^Byte;
  SourceQ, SourceP: ^Byte;
  R, G, B: Integer;
begin
  DestQ := Pointer(Integer(DestOrigin) + DestRect.Top * DestPitch + DestRect.Left * 4);
  SourceQ := Pointer(Integer(SourceOrigin) + SourceY * SourcePitch + SourceX * 4);
  for Y := DestRect.Top to DestRect.Bottom - 1 do begin
    DestP := DestQ;
    SourceP := SourceQ;
    for X := DestRect.Left to DestRect.Right - 1 do begin
      // source pixel (0,0):
      B := SourceP^;
      Inc(SourceP);
      G := SourceP^;
      Inc(SourceP);
      R := SourceP^;
      Inc(SourceP, 2);
      // source pixel (1,0):
      Inc(B, SourceP^);
      Inc(SourceP);
      Inc(G, SourceP^);
      Inc(SourceP);
      Inc(R, SourceP^);
      Inc(SourceP, SourcePitch - 6);
      // source pixel (0,1):
      Inc(B, SourceP^);
      Inc(SourceP);
      Inc(G, SourceP^);
      Inc(SourceP);
      Inc(R, SourceP^);
      Inc(SourceP, 2);
      // source pixel (1,1):
      Inc(B, SourceP^);
      Inc(SourceP);
      Inc(G, SourceP^);
      Inc(SourceP);
      Inc(R, SourceP^);
      Dec(SourceP, SourcePitch - 2);
      // set destination pixel:
      DestP^ := B shr 2;
      Inc(DestP);
      DestP^ := G shr 2;
      Inc(DestP);
      DestP^ := R shr 2;
      Inc(DestP, 2);
    end;
    DestQ := Pointer(Integer(DestQ) + DestPitch);
    SourceQ := Pointer(Integer(SourceQ) + SourcePitch + SourcePitch);
  end;
end;

procedure PegtopResample4x32CPU(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
var
  X, Y, I, J: Integer;
  DestQ, DestP: ^Byte;
  SourceQ, SourceP: ^Byte;
  R, G, B: Integer;
begin
  DestQ := Pointer(Integer(DestOrigin) + DestRect.Top * DestPitch + DestRect.Left * 4);
  SourceQ := Pointer(Integer(SourceOrigin) + SourceY * SourcePitch + SourceX * 4);
  for Y := DestRect.Top to DestRect.Bottom - 1 do begin
    DestP := DestQ;
    SourceP := SourceQ;
    for X := DestRect.Left to DestRect.Right - 1 do begin
      B := 0;
      G := 0;
      R := 0;
      // sum up source pixels in a 4x4 square:
      for I := 0 to 3 do begin
        for J := 0 to 3 do begin
          Inc(B, SourceP^);
          Inc(SourceP);
          Inc(G, SourceP^);
          Inc(SourceP);
          Inc(R, SourceP^);
          Inc(SourceP, 2);
        end;
        // next pixel line:
        Inc(SourceP, SourcePitch - 16);
      end;
      // 4 lines back:
      Dec(SourceP, SourcePitch * 4 - 16);
      // set destination pixel:
      DestP^ := B shr 4;
      Inc(DestP);
      DestP^ := G shr 4;
      Inc(DestP);
      DestP^ := R shr 4;
      Inc(DestP, 2);
    end;
    DestQ := Pointer(Integer(DestQ) + DestPitch);
    SourceQ := Pointer(Integer(SourceQ) + SourcePitch * 4);
  end;
end;

procedure PegtopResample2x32MMX(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
var
  W, Y: Integer;
  DestQ: Pointer;
  SourceQ: Pointer;
begin
  DestQ := Pointer(Integer(DestOrigin) + DestRect.Top * DestPitch + DestRect.Left * 4);
  SourceQ := Pointer(Integer(SourceOrigin) + SourceY * SourcePitch + SourceX * 4);
  W := DestRect.Right - DestRect.Left;
  asm
    // we need mm0 to be zero during the whole loop:
    db $0F,$EF,$C0           /// pxor      mm0, mm0
  end;
  for Y := DestRect.Top to DestRect.Bottom - 1 do begin
    asm
      mov       eax, SourceQ
      mov       edx, DestQ
      mov       ecx, W
      @xloop:
        // source pixels (0,0) and (1,0):
        db $0F,$6F,$08           /// movq      mm1, [eax]
        db $0F,$6F,$D1           /// movq      mm2, mm1
        db $0F,$60,$C8           /// punpcklbw mm1, mm0
        db $0F,$68,$D0           /// punpckhbw mm2, mm0
        db $0F,$DD,$CA           /// paddusw   mm1, mm2
        // source pixels (0,1) and (1,1):
        add       eax, SourcePitch
        db $0F,$6F,$10           /// movq      mm2, [eax]
        db $0F,$6F,$DA           /// movq      mm3, mm2
        db $0F,$60,$D0           /// punpcklbw mm2, mm0
        db $0F,$68,$D8           /// punpckhbw mm3, mm0
        db $0F,$DD,$CA           /// paddusw   mm1, mm2
        db $0F,$DD,$CB           /// paddusw   mm1, mm3
        // set average color (divide by 4):
        db $0F,$71,$D1,$02       /// psrlw mm1, 2
        db $0F,$67,$C8           /// packuswb  mm1, mm0
        db $0F,$7E,$0A           /// movd      [edx], mm1
        // next source pixel block:
        sub       eax, SourcePitch
        add       eax, 8
        // next destination pixel:
        add       edx, 4
        dec ecx
      jnz @xloop
    end;
    DestQ := Pointer(Integer(DestQ) + DestPitch);
    SourceQ := Pointer(Integer(SourceQ) + SourcePitch + SourcePitch);
  end;
  asm
    // finish MMX usage:
    db $0F,$77               /// emms
  end;
end;

procedure PegtopResample4x32MMX(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
var
  W, Y: Integer;
  DestQ: Pointer;
  SourceQ: Pointer;
  BlockStep: Integer;
begin
  DestQ := Pointer(Integer(DestOrigin) + DestRect.Top * DestPitch + DestRect.Left * 4);
  SourceQ := Pointer(Integer(SourceOrigin) + SourceY * SourcePitch + SourceX * 4);
  BlockStep := SourcePitch * 4 - 16;
  W := DestRect.Right - DestRect.Left;
  asm
    // we need mm0 to be zero during the whole loop:
    db $0F,$EF,$C0           /// pxor      mm0, mm0
  end;
  for Y := DestRect.Top to DestRect.Bottom - 1 do begin
    asm
      mov       eax, SourceQ
      mov       edx, DestQ
      mov       ecx, W
      @xloop:
        push ecx
        db $0F,$EF,$C9           /// pxor      mm1, mm1
        // process 4 lines of source bitmap:
        mov ecx, 4
        @ploop:
          // source pixels (0,p) and (1,p):
          db $0F,$6F,$10           /// movq      mm2, [eax]
          db $0F,$6F,$DA           /// movq      mm3, mm2
          db $0F,$60,$D0           /// punpcklbw mm2, mm0
          db $0F,$68,$D8           /// punpckhbw mm3, mm0
          db $0F,$DD,$CA           /// paddusw   mm1, mm2
          db $0F,$DD,$CB           /// paddusw   mm1, mm3
          // source pixels (2,p) and (3,p):
          add       eax, 8
          db $0F,$6F,$10           /// movq      mm2, [eax]
          db $0F,$6F,$DA           /// movq      mm3, mm2
          db $0F,$60,$D0           /// punpcklbw mm2, mm0
          db $0F,$68,$D8           /// punpckhbw mm3, mm0
          db $0F,$DD,$CA           /// paddusw   mm1, mm2
          db $0F,$DD,$CB           /// paddusw   mm1, mm3
          // next source line:
          sub       eax, 8
          add       eax, SourcePitch
          dec ecx
        jnz @ploop
        // set average color (divide by 16):
        db $0F,$71,$D1,$04       /// psrlw mm1, 4
        db $0F,$67,$C8           /// packuswb  mm1, mm0
        db $0F,$7E,$0A           /// movd      [edx], mm1
        // next source pixel block:
        sub       eax, BlockStep
        // next destination pixel:
        add       edx, 4
        pop ecx
        dec ecx
      jnz @xloop
    end;
    DestQ := Pointer(Integer(DestQ) + DestPitch);
    SourceQ := Pointer(Integer(SourceQ) + SourcePitch * 4);
  end;
  asm
    // finish MMX usage:
    db $0F,$77               /// emms
  end;
end;

procedure PegtopResample2x32(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
var
  W, X, Y: Integer;
  DestQ: Pointer;
  SourceQ: Pointer;
begin
  if pcfMMX in CPUFlags then
    PegtopResample2x32MMX(DestOrigin, DestPitch, DestRect, SourceOrigin, SourcePitch, SourceX, SourceY)
  else
    PegtopResample2x32CPU(DestOrigin, DestPitch, DestRect, SourceOrigin, SourcePitch, SourceX, SourceY);
end;

procedure PegtopResample4x32(const DestOrigin: Pointer; const DestPitch: Integer; const DestRect: TRect;
  const SourceOrigin: Pointer; const SourcePitch: Integer; const SourceX, SourceY: Integer);
begin
  if pcfMMX in CPUFlags then
    PegtopResample4x32MMX(DestOrigin, DestPitch, DestRect, SourceOrigin, SourcePitch, SourceX, SourceY)
  else
    PegtopResample4x32CPU(DestOrigin, DestPitch, DestRect, SourceOrigin, SourcePitch, SourceX, SourceY);
end;

end.
