unit GR32_SeedFill;

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
 * The Original Code is the SeedFill for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
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

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  ExtCtrls, GR32, GR32_OrdinalMaps;

type
  { Specifies a certain color distance metric }
  TToleranceMode = (tmAverageRGB, tmMaxRGB, tmHue, tmSaturation, tmBrightness);

  { Callback procedure to measure color difference }
  TEvalToleranceProc = function(Old, New: TColor32): Integer;

  { Either measure absolute difference from color at origin or relative
    difference between adjacent pixels. }
  TMaskMode = (mmAbsolute, mmRelative);

  { A class for performing various floodfill operations }
  { TSeedFill }
  TSeedFill = class(TThreadPersistent)
  private
    FToleranceMap: PByte;
    FMask: PByte;
    FStackLeft: PByte;
    FStackRight: PByte;
    FBuffer: TBitmap32;
    FBits: PColor32Array;
    FBufferSize: Integer;
    FWidth: Integer;
    FTimer: TTimer;
    FOffset: Integer;
    FToleranceMode: TToleranceMode;
    FMin: Integer;
    FMax: Integer;
    FMaskMode: TMaskMode;
    FStartColor: TColor32;
    FAnimateBias: Integer;
    FAnimate: Boolean;
    procedure ApplyEdges;
    procedure SetAnimate(Value: Boolean);
    procedure AnimateBorder(Sender: TObject);
    function GetMaskPtr(X, Y: Integer): PByte;
    function GetToleranceMaskPtr(X, Y: Integer): PByte;
    procedure SetToleranceMode(Value: TToleranceMode);
    procedure CreateMaskAbsolute;
    procedure CreateMaskRelative;
    procedure CreateToleranceMap;
    procedure CreateSelectionMap;
    procedure CreateBorderMask;
  public
    constructor Create(Bitmap: TBitmap32); reintroduce;
    destructor Destroy; override;
    procedure Update;
    procedure DrawGradientSurface(const Gradient: TPalette32);
    procedure DrawSolidSurface(FillColor: TColor32);
    procedure DrawBlendSurface(FillColor: TColor32);
    procedure DrawBorder;
    procedure SetFillPoint(X, Y: Integer);
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    property Animate: Boolean read FAnimate write SetAnimate;
    property Buffer: TBitmap32 read FBuffer write FBuffer;
    property MaskPtr[X, Y: Integer]: PByte read GetMaskPtr;
    property ToleranceMaskPtr[X, Y: Integer]: PByte read GetToleranceMaskPtr;
    property ToleranceMode: TToleranceMode read FToleranceMode write SetToleranceMode;
    property MaskMode: TMaskMode read FMaskMode write FMaskMode;
    property Timer: TTimer read FTimer write FTimer;
  end;

implementation

uses
  GR32_Blend;

{ TSeedFill }

procedure TSeedFill.AnimateBorder(Sender: TObject);
begin
  Dec(FAnimateBias);
  if FAnimateBias <= 0 then FAnimateBias := 8;
  DrawBorder;
  FBuffer.Changed;
end;

procedure TSeedFill.ApplyEdges;
var
  I, W, H: Integer;
  P, Q: PByte;
begin
  P := FMask;
  Q := P;
  W := FBuffer.Width;
  H := FBuffer.Height;
  FillChar(P^, W * H - 1, 0);
  FillChar(P^, W, 4);
  Inc(P, W *(H - 1));
  FillChar(P^, W, 8);
  Q^ := Q^ or 1;
  Inc(Q, W - 1);
  for I := 0 to H - 2 do
  begin
    PWord(Q)^ := PWord(Q)^ or $0102;
    Inc(Q, W);
  end;
  Q^ := Q^ or $02;
end;

constructor TSeedFill.Create(Bitmap: TBitmap32);
var
  StackSize: Integer;
begin
  inherited Create;
  
  FAnimate := False;
  FBuffer := Bitmap;
  FAnimateBias := 8;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 120;
  FTimer.OnTimer := AnimateBorder;

  with FBuffer do
  begin
    FBits := Bits;
    FWidth := Width;
    FBufferSize := Width * Height - 1;
    StackSize := Width * Height * 4;
    GetMem(FStackLeft, StackSize);
    Integer(FStackRight) := Integer(FStackLeft) + StackSize;
    GetMem(FMask, Width * Height);
    GetMem(FToleranceMap, Width * Height);
  end;
end;

destructor TSeedFill.Destroy;
begin
  if Assigned(FMask) then FreeMem(FMask);
  if Assigned(FStackLeft) then FreeMem(FStackLeft);
  if Assigned(FToleranceMap) then FreeMem(FToleranceMap);
  FTimer.Destroy;
  FMask := nil;
  FStackLeft := nil;
end;

procedure TSeedFill.DrawBlendSurface(FillColor: TColor32);
var
  I: Integer;
  PTolerance: PByte;
  PBits: PColor32Array;
begin
  PTolerance := FToleranceMap;
  PBits := FBuffer.Bits;

  for I := 0 to FBufferSize do
  begin
    CombineMem(FillColor, PBits[I], PTolerance^);
    Inc(PTolerance);
  end;
  EMMS;
end;

procedure TSeedFill.DrawGradientSurface(const Gradient: TPalette32);
var
  I: Integer;
  PTolerance: PByte;
  PBits: PColor32Array;
begin
  PTolerance := FToleranceMap;
  PBits := FBuffer.Bits;

  for I := 0 to FBufferSize do
  begin
    BlendMem(Gradient[PTolerance^ xor $FF], PBits[I]);
    Inc(PTolerance);
  end;
  EMMS;
end;

procedure TSeedFill.DrawSolidSurface(FillColor: TColor32);
var
  I: Integer;
  P: PColor32;
  PTolerance: PByte;
begin
  P := PColor32(FBits);
  PTolerance := FToleranceMap;
  for I := 0 to FBufferSize do
  begin
    if PTolerance^ > 127 then P^ := FillColor;
    Inc(PTolerance);
    Inc(P);
  end;
end;

function TSeedFill.GetMaskPtr(X, Y: Integer): PByte;
begin
  Integer(Result) := Integer(FMask) + X + Y * FWidth;
end;

function TSeedFill.GetToleranceMaskPtr(X, Y: Integer): PByte;
begin
  Integer(Result) := Integer(FToleranceMap) + X + Y * FWidth;
end;

procedure TSeedFill.SetAnimate(Value: Boolean);
begin
  if Value <> FAnimate then
  begin
    FAnimate := Value;
    FTimer.Enabled := Value;
    Changed;
  end;
end;

procedure TSeedFill.SetFillPoint(X, Y: Integer);
begin
{$RANGECHECKS OFF}

  FOffset := X + Y * FWidth;
  FStartColor := FBuffer.Bits[FOffset];
  
{$RANGECHECKS ON}
end;

procedure TSeedFill.SetToleranceMode(Value: TToleranceMode);
begin
  if FToleranceMode <> Value then
  begin
    FToleranceMode := Value;
    Changed;
  end;
end;

procedure TSeedFill.CreateMaskAbsolute;
asm
  //  x/3 - x * (65536/3) / 65536 ~- x * $5556 shr 16
  //  procedure Recurse(S, B, M: Pointer; XY, W, T: Integer);
  //  Recurse(StackRight, Bits, ByteMap, Y * Width + X, Width, 3 * Max);

  //  CMPXCHG : if EAX = OP1 then OP1 := OP2
  //  CMOV    :

        PUSH      EBX
        PUSH      EDI
        PUSH      ESI
        PUSH      EBP
//----------------------------------------------------------------------------//
        MOV       EBX,Self.FMax
        MOV       ECX,Self.FMask
        MOV       EDX,Self.FToleranceMap
        MOV       EDI,Self.FOffset
        MOVD      MM5,ESP                     // Old stack pointer
        MOV       ESP,Self.FStackRight        // New stack pointer
        MOV       EBP,Self.FWidth
        LEA       ESI,[@table]
        MOVZX     EAX,BYTE ptr [ECX+EDI]
//----------------------------------------------------------------------------//

        OR        BYTE ptr [ECX+EDI],01000000B
        JMP       @x0

@x1:    CMP       BL,byte ptr [EDX+EDI]
        JB        @x3

@x0:    OR        BYTE ptr [ECX+EDI],00010000B  // Fill this pixel
        PUSH      EDI
@x2:    AND       AL,$0F

        JMP       [offset @table + EAX*4]
@table: DD @1, @2, @1, @3, @1, @2, @1, @4, @1, @2, @1, @3, @1, @2, @1, @5

@1:     DEC       EDI
        OR        WORD ptr [ECX+EDI],1000000100000010B
        MOV       AL,[ECX+EDI]
        TEST      AL,$FF
        JNS       @x1
        JMP       @x4
@2:     OR        WORD ptr [ECX+EDI],0000000110000010B
        INC       EDI
        MOV       AL,[ECX+EDI]
        TEST      AL,$FF
        JNS       @x1
        JMP       @x4
@3:     OR        BYTE ptr [ECX+EDI],10000100B
        SUB       EDI,EBP
        OR        BYTE ptr [ECX+EDI],00001000B
        MOV       AL,[ECX+EDI]
        JNS       @x1
        JMP       @x4
@4:     OR        BYTE ptr [ECX+EDI],10001000B
        ADD       EDI,EBP
        OR        BYTE ptr [ECX+EDI],00000100B
        MOV       AL,[ECX+EDI]
        JNS       @x1
        JMP       @x4
@5:     POP       EDI
        MOV       AL,[ECX+EDI]
        AND       AL,11001111B
        CMP       AL,11001111B
        JNE       @x4
        MOVD      ESP,MM5
        EMMS
        POP       EBP
        POP       ESI
        POP       EDI
        POP       EBX
        RET
@x3:    MOV       EDI,[ESP]
        OR        BYTE ptr [ECX+EDI],00100000B    // Frame this pixel
        MOV       AL,[ECX+EDI]
        JMP       @x2
@x4:    MOV       EDI,[ESP]
        MOV       AL,[ECX+EDI]
        JMP       @x2
end;

procedure TSeedFill.CreateMaskRelative;
asm
  //  x/3 - x * (65536/3) / 65536 ~- x * $5556 shr 16
  //  procedure Recurse(S, B, M: Pointer; XY, W, T: Integer);
  //  Recurse(StackRight, Bits, ByteMap, Y * Width + X, Width, 3 * Max);

  //  CMPXCHG : if EAX = OP1 then OP1 := OP2
  //  CMOV    :

        PUSH      EBX
        PUSH      EDI
        PUSH      ESI
        PUSH      EBP
//----------------------------------------------------------------------------//
        MOV       ESI,Self.FMax
        MOV       ECX,Self.FMask
        MOV       EDX,Self.FToleranceMap
        MOV       EDI,Self.FOffset
        MOVD      MM5,ESP                     // Old stack pointer
        MOV       ESP,Self.FStackRight        // New stack pointer
        MOV       EBP,Self.FWidth
        MOVZX     EAX,BYTE ptr [ECX+EDI]
//----------------------------------------------------------------------------//

        OR        BYTE ptr [ECX+EDI],01000000B
        JMP       @x0

@x1:    SUB       BL,byte ptr[EDX+EDI]
        JNS       @next
        NEG       BL
@next:
        CMP       ESI,EBX
        JB        @x3

@x0:
        OR        BYTE ptr [ECX+EDI],00010000B  // Fill this pixel
        PUSH      EDI

@x2:    MOVZX     EBX,byte ptr[EDX+EDI]
        AND       AL,$0F

        JMP       [offset @table + EAX*4]
@table: DD @1, @2, @1, @3, @1, @2, @1, @4, @1, @2, @1, @3, @1, @2, @1, @5

@1:     DEC       EDI
        OR        WORD ptr [ECX+EDI],1000000100000010B
        MOV       AL,[ECX+EDI]
        TEST      AL,$FF
        JNS       @x1
        JMP       @x4
@2:     OR        WORD ptr [ECX+EDI],0000000110000010B
        INC       EDI
        MOV       AL,[ECX+EDI]
        TEST      AL,$FF
        JNS       @x1
        JMP       @x4
@3:     OR        BYTE ptr [ECX+EDI],10000100B
        SUB       EDI,EBP
        OR        BYTE ptr [ECX+EDI],00001000B
        MOV       AL,[ECX+EDI]
        JNS       @x1
        JMP       @x4
@4:     OR        BYTE ptr [ECX+EDI],10001000B
        ADD       EDI,EBP
        OR        BYTE ptr [ECX+EDI],00000100B
        MOV       AL,[ECX+EDI]
        JNS       @x1
        JMP       @x4
@5:     POP       EDI
        MOV       AL,[ECX+EDI]
        AND       AL,11001111B
        CMP       AL,11001111B
        JNE       @x4
        MOVD      ESP,MM5
        EMMS
        POP       EBP
        POP       ESI
        POP       EDI
        POP       EBX
        RET
@x3:    MOV       EDI,[ESP]
        OR        BYTE ptr [ECX+EDI],00100000B    // Frame this pixel
        MOV       AL,[ECX+EDI]
        JMP       @x2
@x4:    MOV       EDI,[ESP]
        MOV       AL,[ECX+EDI]
        JMP       @x2
end;


procedure TSeedFill.Update;
begin
  ApplyEdges;
  CreateToleranceMap;     // Changes FToleranceMap
  if FMaskMode = mmAbsolute then
    CreateMaskAbsolute
  else
    CreateMaskRelative;
  CreateSelectionMap;     // Changes FToleranceMap
  CreateBorderMask;       // Changes FMask (only for border)
end;


function EvalAverageRGB(Old, New: TColor32): Integer;
asm
    AND       EAX,$00FFFFFF
    AND       EDX,$00FFFFFF
    MOVD      MM0,EAX
    MOVD      MM1,EDX
    PSADBW    MM0,MM1
    MOVD      EAX,MM0
    IMUL      EAX,$555555
    SHR       EAX,24
  //Result := ((C shr 16 and $FF) + (C shr 8 and $FF) + (C and $FF)) div 3;
end;

function EvalHue(Old, New: TColor32): Integer;
var
  Z, A, B: Byte;
begin
  RGBtoHSL(Old, A, Z, Z);
  RGBtoHSL(New, B, Z, Z);
  Result := Abs(A - B);
end;

function EvalSaturation(Old, New: TColor32): Integer;
var
  Z, A, B: Byte;
begin
  RGBtoHSL(Old, Z, A, Z);
  RGBtoHSL(New, Z, B, Z);
  Result := Abs(A - B);
end;

function EvalBrightness(Old, New: TColor32): Integer;
var
  Z, A, B: Byte;
begin
  RGBtoHSL(Old, Z, Z, A);
  RGBtoHSL(New, Z, Z, B);
  Result := Abs(A - B);
end;

procedure TSeedFill.CreateToleranceMap;
var
  I: Integer;
  P: PByte;
  B: PColor32Array;
  EvalProc: TEvalToleranceProc;
begin
{$RANGECHECKS OFF}

  EvalProc := nil;
  
  case FToleranceMode of
    tmAverageRGB: EvalProc := EvalAverageRGB;
    tmMaxRGB: ;
    tmHue: EvalProc := EvalHue;
    tmSaturation: EvalProc := EvalSaturation;
    tmBrightness: EvalProc := EvalBrightness;
  end;

  P := FToleranceMap;
  B := FBuffer.Bits;

  for I := 0 to FBufferSize do
  begin
    P^ := EvalProc(FStartColor, B[I]);
    Inc(P);
  end;

  EMMS;

{$RANGECHECKS ON}
end;

procedure TSeedFill.CreateSelectionMap;
var
  PTol, PMask: PByte;
  X, I, Divisor, Min: Integer;
begin
  Min := FMin;
  if FMax = FMin then Divisor := $FFFFFF else Divisor := $FFFFFF div (FMax - FMin);
  PTol := FToleranceMap;
  PMask := FMask;

  if FMaskMode = mmAbsolute then
  begin
    for I := 0 to FBufferSize do
    begin
      if (PMask^ and $10) <> 0 then
      begin
        X := PTol^ - Min;
        if X < 0 then X := 0;
        PTol^ := (X * Divisor shr 16) xor $FF;
      end
      else PTol^ := 0;
      Inc(PTol);
      Inc(PMask);
    end;
  end
  else
  begin
    for I := 0 to FBufferSize do
    begin
      if (PMask^ and $10) <> 0 then
        PTol^ := $ff
      else
        PTol^ := 0;
      Inc(PTol);
      Inc(PMask);
    end;
  end;
end;

procedure TSeedFill.DrawBorder;
var
  I, J: Integer;
  P: PColor32;
  M: PByte;
begin
  P := PColor32(FBits);
  M := FMask;
  for J := FAnimateBias  to FAnimateBias + FBuffer.Height - 1 do
    for I := 0 to FBuffer.Width - 1 do
    begin
      if M^ <> 0 then
      if  (J + I) mod 8 >= 4 then
        P^ := clWhite32
      else
        P^ := clBlack32;
      Inc(P);
      Inc(M);
    end;
end;

procedure TSeedFill.CreateBorderMask;
const
// Andre Felix Miertschink suggested that take off RangeCheck at here
// to avoid Delphi XE compiler warnings.

// The original auther of this unit, Mattias Andersson, suggested
// using UInt64 to avoid compiler warnings. This way, we don't need
// the following compiler directives.

// Here, we don't want to make any changes to the original code,
// so we chose the first solution.
//
// -- Ma Xiaoguang and Ma Xiaoming

{$RANGECHECKS OFF}
  const_inverse: Int64 = $FFFFFFFFFFFFFFFF;
{$RANGECHECKS ON}
asm
        PUSH        EBX
        PUSH        EDI
        PUSH        ESI
        PUSH        EAX

//----------------------------------------------------------------------------//
        MOV         EBX,Self.FToleranceMap  // Src for evaluation
        MOV         ECX,Self.FMask          // Dst [Top]
        MOV         EDI,Self.FBufferSize
        MOV         EAX,Self.FWidth
        ADD         ECX,EAX                 // FMask + Width (Dst)
        MOV         EDX,EBX
        ADD         EDX,EAX                 // FToleranceMap + Width
        ADD         EAX,EAX
        SUB         EDI,EAX                 // FBufferSize - 2 * Width
        ADD         EAX,EBX                 // FToleranceMap + 2 * Width
//----------------------------------------------------------------------------//
        MOVQ        MM5,const_inverse

        INC         ECX
        INC         EBX
        INC         EAX

        SHR         EDI,3

@1:     TEST        EDI,$1F
        JNZ         @1_1

@1_1:   MOVQ        MM0,[EBX+EDI*8]         // top
        MOVQ        MM1,[EDX+EDI*8]         // left
        MOVQ        MM2,[EAX+EDI*8]         // bottom
        MOVQ        MM3,[EDX+EDI*8+1]       // middle
        MOVQ        MM4,[EDX+EDI*8+2]       // right
        PCMPGTB     MM0,MM5
        PCMPGTB     MM1,MM5
        PCMPGTB     MM2,MM5
        PCMPGTB     MM4,MM5
        PXOR        MM3,MM5
        PCMPGTB     MM3,MM5
        POR         MM0,MM1
        POR         MM0,MM2
        POR         MM0,MM4
        PAND        MM0,MM3

        MOVNTQ      [ECX+EDI*8],MM0

        DEC         EDI
        JNS         @1

//----------------------------------------------------------------------------//
// Top and bottom horizontally

        POP         EAX
        MOV         ECX,Self.FMask
        MOV         ESI,Self.FToleranceMap

        MOV         EDX,Self.FWidth
        MOV         EDI,Self.FBufferSize

        DEC         EDI

@2:     MOV         BL,byte ptr[ESI+EDX]
        CMP         BL,$7F
        JA          @2_1
        MOV         byte ptr[ECX+EDX],$00
        JMP         @2_2
@2_1:   MOV         byte ptr[ECX+EDX],$FF
@2_2:   MOV         BL,byte ptr[ESI+EDI]
        CMP         BL,$7F
        JA          @2_3
        MOV         byte ptr[ECX+EDI],$00
        JMP         @2_4
@2_3:   MOV         byte ptr[ECX+EDI],$FF
@2_4:   DEC         EDI
        DEC         EDX
        JG          @2

// Left and right vertically

        MOV         ECX,Self.FMask
        MOV         ESI,Self.FToleranceMap
        MOV         EDX,Self.FWidth
        DEC         EDX
        MOV         EDI,Self.FBufferSize

@3:     MOV         BL,byte ptr[ESI+EDI]
        CMP         BL,$7F
        JA          @3_1
        MOV         byte ptr[ECX+EDI],$00
        JMP         @3_2
@3_1:   MOV         byte ptr[ECX+EDI],$FF
@3_2:
        SUB         EDI,EDX
        MOV         BL,byte ptr[ESI+EDI]
        CMP         BL,$7F
        JA          @3_3
        MOV         byte ptr[ECX+EDI],$00
        JMP         @3_4
@3_3:   MOV         byte ptr[ECX+EDI],$FF
@3_4:   DEC         EDI
        JNS         @3

@exit:
        EMMS
        POP       ESI
        POP       EDI
        POP       EBX
end;

end.
