////////////////////////////////////////////////////////////////////////////////
// File:       PegtopCPUUtils.pas
// Version:    1.00
// Date:       01 Jun 2004 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// Some functions to gain information about the CPU. CPUFlags is a global
// variable, which holds the capabilities of the CPU.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopCPUUtils;

interface

type
  TPegtopCPUFlag = (pcfPentium, pcfMMX, pcfSSE, pcfSSE2, pcf3DNow, pcf3DNowExt);
  TPegtopCPUFlags = set of TPegtopCPUFlag;
  TPegtopCPUID = array[0..3] of Longword;
  TPegtopCPUVendor = array[0..11] of Char;

function GetCPUID: TPegtopCPUID;
function GetCPUVendor: TPegtopCPUVendor;
function GetCPUName: String;
procedure FinishMMX;

var
  CPUFlags: TPegtopCPUFlags;

implementation

uses
  Windows, SysUtils;

function HasCPUID: Boolean;
asm
  mov    edx, 0   // return False
  pushfd
  pop    eax
  mov    ecx, eax
  xor    eax, $00200000
  push   eax
  popfd
  pushfd
  pop    eax
  xor    ecx, eax
  jz     @NoCPUId
  mov    edx, 1   // return True
  @NoCPUId:
  push   eax
  popfd
  mov    eax, edx
end;

function GetCPUSignature: Integer;
asm
  push ebx
  mov  eax,1
  dw   $A20F // cpuid
  pop  ebx
end;

function GetCPUID: TPegtopCPUID;
asm
  push   ebx
  push   edi
  mov    edi, eax // @Result
  mov    eax, 1
  dw     $A20F    // cpuid
  stosd           // Result[0]
  mov    eax, ebx
  stosd           // Result[1]
  mov    eax, ecx
  stosd           // Result[2]
  mov    eax, edx
  stosd           // Result[3]
  pop    edi
  pop    ebx
end;

function GetCPUVendor: TPegtopCPUVendor;
asm
  push    ebx
  push    edi
  mov     edi, eax // @Result
  mov     eax, 0
  dw      $A20F    // cpuid
  mov     eax, ebx
  xchg    ebx, ecx
  mov     ecx, 4
  @FirstDWord:
  stosb
  shr     eax, 8
  loop    @FirstDWord
  mov     eax, edx
  mov     ecx, 4
  @SecondDWord:
  stosb
  shr     eax, 8
  loop    @SecondDWord
  mov     eax, ebx
  mov     ecx, 4
  @ThirdDWord:
  stosb
  shr     eax, 8
  loop    @ThirdDWord
  pop     edi
  pop     ebx
end;

function GetCPUFeatures: Integer;
asm
  push ebx
  mov  eax, 1
  dw   $A20F   // cpuid
  pop  ebx
  mov  eax, edx
end;

function GetAMDExtensions: Integer;
asm
  push ebx
  mov  eax, $80000000
  dw   $A20F   // cpuid
  pop  ebx
  mov  eax,edx
end;

function GetAMDExtFeatures: Integer;
asm
  push ebx
  mov  eax, $80000001
  dw   $A20F   // cpuid
  pop  ebx
  mov  eax, edx
end;

procedure FinishMMX;
asm
  db $0F,$77               /// emms
end;

function GetCPUFlags: TPegtopCPUFlags;
var
  CPUFeatures: Integer;
  AMDExtensions: Integer;
  AMDExtFeatures: Integer;
begin
  Result := [];
  if HasCPUID and ((GetCPUSignature shr 8 and $0F) >= 5) then begin
    Include(Result, pcfPentium);
    CPUFeatures := GetCPUFeatures;
    if (CPUFeatures and $800000) <> 0 then
      Include(Result, pcfMMX);
    if (CPUFeatures and $2000000) <> 0 then
      Include(Result, pcfSSE);
    if (CPUFeatures and $4000000) <> 0 then
      Include(Result, pcfSSE2);
    AMDExtensions := GetAMDExtensions;
    if (AMDExtensions and $F000000) <> 0 then begin
      AMDExtFeatures := GetAMDExtFeatures;
      if (AMDExtFeatures and $F000000) = 0 then
        Include(Result, pcf3DNow);
      if (AMDExtFeatures and $F00000) = 0 then
        Include(Result, pcf3DNowExt);
    end;
  end;
end;

function GetCPUName: String;
var
  Vendor: TPegtopCPUVendor;
  CPUID: TPegtopCPUID;
begin
  if HasCPUID then begin
    Vendor := GetCPUVendor;
    if Vendor = 'AuthenticAMD' then begin
      CPUID := GetCPUID;
      case CPUID[0] of
        $484, $494:       Result := 'AMD 80486';
        $4E4, $4F4:       Result := 'AMD 5x86';
        $501, $514, $524: Result := 'AMD K5';
        $561, $562, $570: Result := 'AMD K6';
        $580, $589, $58C: Result := 'AMD K6-2';
        $591:             Result := 'AMD K6-III';
        $5D0, $5D4:       Result := 'AMD K6-2+ / K6-III+';
        $612, $621, $622: Result := 'AMD Athlon';
        $630, $631:       Result := 'AMD Duron Spitfire';
        $642, $644:       Result := 'AMD Athlon Thunderbird';
        $661, $662:       Result := 'AMD Athlon Palomino';
        $670, $671:       Result := 'AMD Duron Morgan';
        $680, $681:       Result := 'AMD Athlon Thoroughbred';
        $6A0:             Result := 'AMD Athlon Barton';
        $F48, $FC0, $FF0,
        $10FF0:           Result := 'AMD 64';
        $F51, $F58:       Result := 'AMD Opteron';
        else              Result := 'AMD (' + IntToHex(CPUID[0], 8) + ')';
      end;
    end
    else if Vendor = 'GenuineIntel' then begin
      CPUID := GetCPUID;
      case CPUID[0] of
        $423, $45B:             Result := 'Intel 80486 SX';
        $435, $436, $470, $480: Result := 'Intel 80486 DX';
        $513, $515, $517, $51A,
        $521, $522, $524, $525,
        $2525, $526, $52B,
        $52C, $252C, $1532,
        $570:                   Result := 'Intel Pentium';
        $543, $1543, $2543,
        $544, $581, $582:       Result := 'Intel Pentium MMX';
        $611, $612, $616, $617,
        $619:                   Result := 'Intel Pentium Pro';
        $633, $634, $650, $651,
        $652, $653, $660, $665,
        $66A:                   Result := 'Intel Pentium II';
        $672, $673, $681, $683,
        $686, $68A, $6A1, $6A4,
        $6B1, $6B4:             Result := 'Intel Pentium III';
        $695, $6D6:             Result := 'Intel Pentium M';
        $F05, $F07, $F0A, $F12,
        $F13:                   Result := 'Intel Pentium 4 Wilamette';
        $F22, $F25, $F27, $F28,
        $F29:                   Result := 'Intel Pentium 4 Northwood';
        $F24:                   Result := 'Intel Pentium 4-M';
        $F33, $F34, $F41:       Result := 'Intel Pentium 4 Prescott';
        else                    Result := 'Intel (' + IntToHex(CPUID[0], 8) + ')';
      end;
    end
    else begin
      Result := Vendor;
    end;
  end
  else begin
    Result := '?';
  end;
end;

{
Manufacturer-00000000-00000001	FPU
Manufacturer-00000000-00000010	RDTSC
Manufacturer-00000000-00000200	APIC (can be disabled by OS)
Manufacturer-00000000-0000f000	CMOV
Manufacturer-00000000-00800000	MMX
Manufacturer-00000000-02000000	SSE
Manufacturer-00000000-04000000	SSE2
AuthenticAMD-00000484-00000001	80486DX4
AuthenticAMD-00000494-00000001	80486DX4WB
AuthenticAMD-000004e4-00000001	5x86
AuthenticAMD-000004f4-00000001	5x86WB
AuthenticAMD-00000501-000003bf	K5 SSA5 (PR75, PR90, PR100)
AuthenticAMD-00000514-000021bf	K5 5k86 (PR120, PR133)
AuthenticAMD-00000524-000021bf	K5 5k86 (PR166)
AuthenticAMD-00000561-008001bf	K6, 0.30um
AuthenticAMD-00000562-008001bf	K6, 0.30um
AuthenticAMD-00000570-008001bf	K6, 0.25um
AuthenticAMD-00000580-008001bf	K6-2
AuthenticAMD-00000589-008021bf	K6-2
AuthenticAMD-0000058c-008021bf	K6-2
AuthenticAMD-00000591-008021bf	K6-III
AuthenticAMD-000005d0-008021bf	K6-2+ or K6-III+, 0.18um
AuthenticAMD-000005d4-008021bf	K6-2+ or K6-III+, 0.18um
AuthenticAMD-00000612-0081f9ff	Athlon, 0.25um
AuthenticAMD-00000621-0183fbff	Athlon, 0.18um
AuthenticAMD-00000622-0183fbff	Athlon, 0.18um
AuthenticAMD-00000630-0183fbff	Duron Spitfire
AuthenticAMD-00000631-0183fbff	Duron Spitfire
AuthenticAMD-00000642-0183fbff	Athlon Thunderbird
AuthenticAMD-00000644-0183fbff	Athlon Thunderbird
AuthenticAMD-00000661-0183fbff	Athlon Palomino
AuthenticAMD-00000661-0383fbff	Athlon Palomino
AuthenticAMD-00000662-0383fbff	Athlon Palomino
AuthenticAMD-00000670-0183fbff	Duron Morgan
AuthenticAMD-00000670-0383fbff	Duron Morgan
AuthenticAMD-00000671-0183f9ff	Duron Morgan
AuthenticAMD-00000671-0383fbff	Duron Morgan
AuthenticAMD-00000680-0183fbff	Athlon Thoroughbred
AuthenticAMD-00000680-0383fbff	Athlon Thoroughbred
AuthenticAMD-00000681-0383fbff	Athlon Thoroughbred
AuthenticAMD-000006a0-0383fbff	Athlon Barton
AuthenticAMD-00000f48-078bfbff	Athlon 64, 0.13um, 754
AuthenticAMD-00000f51-078bfbff	Opteron, 0.13um
AuthenticAMD-00000f58-078bfbff	Opteron, 0.13um
AuthenticAMD-00000fc0-078bfbff	Athlon 64, 0.13um, 754
AuthenticAMD-00000ff0-078bfbff	Athlon 64, 0.13um, 939
AuthenticAMD-00010ff0-078bfbff	Athlon 64, 0.13um, 939
GenuineIntel-00000423-00000002	80486SX
GenuineIntel-00000435-00000003	80486DX2
GenuineIntel-00000436-0000000b	80486DX2
GenuineIntel-0000045b-00000002	80486SX2
GenuineIntel-00000470-0000000b	80486DX2WB
GenuineIntel-00000480-00000003	80486DX4
GenuineIntel-00000513-000001bf	Pentium 60/66
GenuineIntel-00000515-000001bf	Pentium 60/66
GenuineIntel-00000517-000001bf	Pentium 60/66
GenuineIntel-0000051a-000001bf	Pentium 60/66
GenuineIntel-00000521-000001bf	Pentium
GenuineIntel-00000521-000007bf	Pentium
GenuineIntel-00000522-000001bf	Pentium
GenuineIntel-00000524-000003bf	Pentium
GenuineIntel-00002524-000007bf	Pentium, second CPU
GenuineIntel-00000525-000003bf	Pentium C2, 75/90/100/120/133
GenuineIntel-00002525-000003bf	Pentium C2, second CPU
GenuineIntel-00000526-000003bf	Pentium 75-
GenuineIntel-0000052b-000003bf	Pentium cB1/mcB1, 120/133
GenuineIntel-0000052c-000003bf	Pentium cC0, 120/133/150/166/200
GenuineIntel-0000252c-000003bf	Pentium cC0, second CPU
GenuineIntel-00001532-0000013f	Pentium OverDrive
GenuineIntel-00000543-008001bf	Pentium MMX xB1/mxB1
GenuineIntel-00001543-008001bf	Pentium MMX xB1/mxB1 OverDrive
GenuineIntel-00000543-008003bf	Pentium MMX xB1/mxB1
GenuineIntel-00002543-008003bf	Pentium MMX xB1/mxB1 second CPU
GenuineIntel-00000544-008003bf	Pentium MMX xA3
GenuineIntel-00000570-000001bf	Pentium mA4, 75/90/100
GenuineIntel-00000581-008001bf	Pentium MMX myA0
GenuineIntel-00000582-008001bf	Pentium MMX myB2
GenuineIntel-00000611-0000f9ff	Pentium Pro B0
GenuineIntel-00000612-0000f9ff	Pentium Pro C0
GenuineIntel-00000616-0000fbff	Pentium Pro sA0
GenuineIntel-00000617-0000fbff	Pentium Pro sA1
GenuineIntel-00000619-0000fbff	Pentium Pro sB1
GenuineIntel-00000633-0080fbff	Pentium II C0, 0.28um
GenuineIntel-00000634-0080fbff	Pentium II C1, 0.28um
GenuineIntel-00000650-0183fbff	Pentium II mdA0, 0.25um
GenuineIntel-00000651-0183fbff	Pentium II dA1, 0.25um
GenuineIntel-00000652-0183fbff	Pentium II dB0 or mdB0 or Xeon B0, 0.25um
GenuineIntel-00000653-0183fbff	Pentium II dB1 or Xeon B1, 0.25um
GenuineIntel-00000660-0183fbff	Pentium II
GenuineIntel-00000665-0183fbff	Pentium II
GenuineIntel-0000066a-0187f9ff	Pentium II mdxA0/mqbA1/mqpA1/mdbA0/mdxA0/mdpA0
GenuineIntel-00000672-0383fbff	Pentium III kB0, 0.25um
GenuineIntel-00000672-0387fbff	Pentium III kB0, 0.25um
GenuineIntel-00000673-0383fbff	Pentium III kC0, 0.25um
GenuineIntel-00000673-0387fbff	Pentium III kC0, 0.25um
GenuineIntel-00000681-0383fbff	Pentium III cA2, 0.18um, 256KB
GenuineIntel-00000681-0387fbff	Pentium III cA2, 0.18um, 256KB
GenuineIntel-00000683-0383fbff	Pentium III cB0, 0.18um, 256KB
GenuineIntel-00000683-0387fbff	Pentium III cB0, 0.18um, 256KB
GenuineIntel-00000686-0383fbff	Pentium III cC0, 0.18um, 256KB
GenuineIntel-00000686-0387fbff	Pentium III cC0, 0.18um, 256KB
GenuineIntel-0000068a-0383fbff	Pentium III cD0, 0.18um, 256KB
GenuineIntel-0000068a-0387fbff	Pentium III cD0, 0.18um, 256KB
GenuineIntel-00000695-a7e9fbbf	Pentium M B1, 0.13um, 512KB/1024KB
GenuineIntel-000006a1-0383fbff	Pentium III, 0.18um, 1024KB/2048KB
GenuineIntel-000006a4-0383fbff	Pentium III, 0.18um, 1024KB/2048KB
GenuineIntel-000006b1-0383fbff	Pentium III tA1, 0.13um, 256KB/512KB
GenuineIntel-000006b4-0383fbff	Pentium III tB1, 0.13um, 256KB/512KB
GenuineIntel-000006d6-afe9f9bf	Pentium M B1, 0.09um, 512KB/2048KB
GenuineIntel-00000f05-3febf9ff	Pentium 4 Willamette, 0.18um
GenuineIntel-00000f07-3febfbff	Pentium 4 Willamette B2, 0.18um
GenuineIntel-00000f0a-3febfbff	Pentium 4 Willamette C1, 0.18um, 1300/1400/1500/1600/1700/1800
GenuineIntel-00000f12-3febfbff	Pentium 4 Willamette D0, 0.18um, 1400/1500/1600/1700/1800/1900/2000
GenuineIntel-00000f13-3febfbff	Pentium 4 Willamette, 0.18um
GenuineIntel-00000f22-3febfbff	Pentium 4 Northwood, 0.13um
GenuineIntel-00000f24-3febfbff	Pentium 4-M B0, 0.13um
GenuineIntel-00000f25-bfebfbff	Pentium 4 Northwood M0, 0.13um, 2260/2400/2800/3000
GenuineIntel-00000f27-bfebfbff	Pentium 4 Northwood C1, 0.13um, 2000/2200/2260/2400/2500/2530/2600/2660/2800/3060
GenuineIntel-00000f28-3febfbff	Pentium 4 Northwood, 0.13um
GenuineIntel-00000f29-bfebfbff	Pentium 4 Northwood D1, 0.13um, 3000/3060/3200/3400; or Pentium 4-M
GenuineIntel-00000f33-bfebfbff	Pentium 4 Prescott C0, 0.09um, 2260/2400/2660/2800/3000/3200/3400, 512KB/1024KB
GenuineIntel-00000f34-bfebfbff	Pentium 4 Prescott D0, 0.09um, 2400/2660/2800/2930/3000/3200/3400/3600, 1024KB
GenuineIntel-00000f41-bfebfbff	Pentium 4 Prescott E0, 0.09um, 2660/2800/2930/3000/3060/3200/3400/3600/3800, 1024KB
}

initialization
  CPUFlags := GetCPUFlags;
end.

