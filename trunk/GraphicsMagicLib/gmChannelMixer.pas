{ This library created in April 16th, 2010.
  CopyRight(C) 2010, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmChannelMixer;

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
  Classes, GR32, gmTypes;

type
  TgmChannelMixer = class(TObject)
  private
    FChannel   : TgmChannelSelector;
    FInputColor: TColor32;
    FMonochrome: Boolean;

    // distribution to red channel
    FRedScaleForOutputRed  : Single;
    FGreenScaleForOutputRed: Single;
    FBlueScaleForOutputRed : Single;

    // distribution to green channel
    FRedScaleForOutputGreen  : Single;
    FGreenScaleForOutputGreen: Single;
    FBlueScaleForOutputGreen : Single;

    // distribution to blue channel
    FRedScaleForOutputBlue  : Single;
    FGreenScaleForOutputBlue: Single;
    FBlueScaleForOutputBlue : Single;

    // constant distribution -- Black -> White
    FConstantScaleForOutputRed  : Single;
    FConstantScaleForOutputGreen: Single;
    FConstantScaleForOutputBlue : Single;

    // gray channel settings...
    FGrayScaleForRed  : Single;
    FGrayScaleForGreen: Single;
    FGrayScaleForBlue : Single;
    FGrayScaleForConst: Single;

    function GetOutputColor: TColor32;
    function GetRedScale: Single;
    function GetGreenScale: Single;
    function GetBlueScale: Single;
    function GetConstantScale: Single;

    procedure SetRedScale(const Value: Single);
    procedure SetGreenScale(const Value: Single);
    procedure SetBlueScale(const Value: Single);
    procedure SetConstantScale(const Value: Single);
    procedure SetMonochrome(const Value: Boolean);
    procedure SetChannel(const Value: TgmChannelSelector);
  public
    constructor Create;
    procedure InitParameters;
    procedure AssignData(const AChannelMixer: TgmChannelMixer);

    // load channel mixer settings from a stream
    function LoadFromStream(const AStream: TStream): Boolean;
    
    // save channel mixer settings to a stream
    procedure SaveToStream(const AStream: TStream);

    property Channel      : TgmChannelSelector read FChannel         write SetChannel;
    property InputColor   : TColor32           read FInputColor      write FInputColor;
    property OutputColor  : TColor32           read GetOutputColor;
    property Monochrome   : Boolean            read FMonochrome      write SetMonochrome;
    property RedScale     : Single             read GetRedScale      write SetRedScale;
    property GreenScale   : Single             read GetGreenScale    write SetGreenScale;
    property BlueScale    : Single             read GetBlueScale     write SetBlueScale;
    property ConstantScale: Single             read GetConstantScale write SetConstantScale;
  end;

implementation

uses
  GR32_LowLevel;

constructor TgmChannelMixer.Create;
begin
  inherited Create;
  InitParameters;
end;

procedure TgmChannelMixer.InitParameters;
begin
  FChannel    := csRed;
  FInputColor := clBlack32;
  FMonochrome := False;

  // distribution to red channel
  FRedScaleForOutputRed   := 1.0;
  FGreenScaleForOutputRed := 0.0;
  FBlueScaleForOutputRed  := 0.0;

  // distribution to green channel
  FRedScaleForOutputGreen   := 0.0;
  FGreenScaleForOutputGreen := 1.0;
  FBlueScaleForOutputGreen  := 0.0;

  // distribution to blue channel
  FRedScaleForOutputBlue   := 0.0;
  FGreenScaleForOutputBlue := 0.0;
  FBlueScaleForOutputBlue  := 1.0;

  // constant distribution -- Black -> White
  FConstantScaleForOutputRed   := 0.0;
  FConstantScaleForOutputGreen := 0.0;
  FConstantScaleForOutputBlue  := 0.0;

  // gray channel...
  FGrayScaleForRed   := 0.0;
  FGrayScaleForGreen := 0.0;
  FGrayScaleForBlue  := 0.0;
  FGrayScaleForConst := 0.0;
end;

procedure TgmChannelMixer.AssignData(const AChannelMixer: TgmChannelMixer);
begin
  if Assigned(AChannelMixer) then
  begin
    Self.FChannel    := AChannelMixer.FChannel;
    Self.FInputColor := AChannelMixer.FInputColor;
    Self.FMonochrome := AChannelMixer.FMonochrome;

    // distribution to red channel
    Self.FRedScaleForOutputRed   := AChannelMixer.FRedScaleForOutputRed;
    Self.FGreenScaleForOutputRed := AChannelMixer.FGreenScaleForOutputRed;
    Self.FBlueScaleForOutputRed  := AChannelMixer.FBlueScaleForOutputRed;

    // distribution to green channel
    Self.FRedScaleForOutputGreen   := AChannelMixer.FRedScaleForOutputGreen;
    Self.FGreenScaleForOutputGreen := AChannelMixer.FGreenScaleForOutputGreen;
    Self.FBlueScaleForOutputGreen  := AChannelMixer.FBlueScaleForOutputGreen;

    // distribution to blue channel
    Self.FRedScaleForOutputBlue   := AChannelMixer.FRedScaleForOutputBlue;
    Self.FGreenScaleForOutputBlue := AChannelMixer.FGreenScaleForOutputBlue;
    Self.FBlueScaleForOutputBlue  := AChannelMixer.FBlueScaleForOutputBlue;

    // constant distribution -- Black -> White
    Self.FConstantScaleForOutputRed   := AChannelMixer.FConstantScaleForOutputRed;
    Self.FConstantScaleForOutputGreen := AChannelMixer.FConstantScaleForOutputGreen;
    Self.FConstantScaleForOutputBlue  := AChannelMixer.FConstantScaleForOutputBlue;

    // gray channel...
    Self.FGrayScaleForRed   := AChannelMixer.FGrayScaleForRed;
    Self.FGrayScaleForGreen := AChannelMixer.FGrayScaleForGreen;
    Self.FGrayScaleForBlue  := AChannelMixer.FGrayScaleForBlue;
    Self.FGrayScaleForConst := AChannelMixer.FGrayScaleForConst;
  end;
end;

// load channel mixer settings from a stream
function TgmChannelMixer.LoadFromStream(const AStream: TStream): Boolean;
var
  LIntValue: Integer;
begin
  Result := False;
  
  if Assigned(AStream) then
  begin
    AStream.Read(LIntValue, 4);
    FChannel := TgmChannelSelector(LIntValue);

    AStream.Read(FMonochrome, 1);

    // distribution to red channel
    AStream.Read(FRedScaleForOutputRed,   4);
    AStream.Read(FGreenScaleForOutputRed, 4);
    AStream.Read(FBlueScaleForOutputRed,  4);

    // distribution to green channel
    AStream.Read(FRedScaleForOutputGreen,   4);
    AStream.Read(FGreenScaleForOutputGreen, 4);
    AStream.Read(FBlueScaleForOutputGreen,  4);

    // distribution to blue channel
    AStream.Read(FRedScaleForOutputBlue,   4);
    AStream.Read(FGreenScaleForOutputBlue, 4);
    AStream.Read(FBlueScaleForOutputBlue,  4);

    // constant distribution -- Black -> White
    AStream.Read(FConstantScaleForOutputRed,   4);
    AStream.Read(FConstantScaleForOutputGreen, 4);
    AStream.Read(FConstantScaleForOutputBlue,  4);

    // gray channel settings...
    AStream.Read(FGrayScaleForRed,   4);
    AStream.Read(FGrayScaleForGreen, 4);
    AStream.Read(FGrayScaleForBlue,  4);
    AStream.Read(FGrayScaleForConst, 4);
  end;
end;

// save channel mixer settings to a stream
procedure TgmChannelMixer.SaveToStream(const AStream: TStream);
var
  LIntValue: Integer;
begin
  if Assigned(AStream) then
  begin
    LIntValue := Ord(FChannel);
    AStream.Write(LIntValue, 4);
    AStream.Write(FMonochrome, 1);

    // distribution to red channel
    AStream.Write(FRedScaleForOutputRed,   4);
    AStream.Write(FGreenScaleForOutputRed, 4);
    AStream.Write(FBlueScaleForOutputRed,  4);

    // distribution to green channel
    AStream.Write(FRedScaleForOutputGreen,   4);
    AStream.Write(FGreenScaleForOutputGreen, 4);
    AStream.Write(FBlueScaleForOutputGreen,  4);

    // distribution to blue channel
    AStream.Write(FRedScaleForOutputBlue,   4);
    AStream.Write(FGreenScaleForOutputBlue, 4);
    AStream.Write(FBlueScaleForOutputBlue,  4);

    // constant distribution -- Black -> White
    AStream.Write(FConstantScaleForOutputRed,   4);
    AStream.Write(FConstantScaleForOutputGreen, 4);
    AStream.Write(FConstantScaleForOutputBlue,  4);

    // gray channel settings...
    AStream.Write(FGrayScaleForRed,   4);
    AStream.Write(FGrayScaleForGreen, 4);
    AStream.Write(FGrayScaleForBlue,  4);
    AStream.Write(FGrayScaleForConst, 4);
  end;
end;

function TgmChannelMixer.GetRedScale: Single;
begin
  Result := 0.00;

  if FMonochrome then
  begin
    Result := FGrayScaleForRed;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          Result := FRedScaleForOutputRed;
        end;

      csGreen:
        begin
          Result := FRedScaleForOutputGreen;
        end;

      csBlue:
        begin
          Result := FRedScaleForOutputBlue;
        end;
    end;
  end;
end;

function TgmChannelMixer.GetGreenScale: Single;
begin
  Result := 0.00;

  if FMonochrome then
  begin
    Result := FGrayScaleForGreen;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          Result := FGreenScaleForOutputRed;
        end;

      csGreen:
        begin
          Result := FGreenScaleForOutputGreen;
        end;

      csBlue:
        begin
          Result := FGreenScaleForOutputBlue;
        end;
    end;
  end;
end;

function TgmChannelMixer.GetBlueScale: Single;
begin
  Result := 0.00;

  if FMonochrome then
  begin
    Result := FGrayScaleForBlue;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          Result := FBlueScaleForOutputRed;
        end;

      csGreen:
        begin
          Result := FBlueScaleForOutputGreen;
        end;

      csBlue:
        begin
          Result := FBlueScaleForOutputBlue;
        end;
    end;
  end;
end;

function TgmChannelMixer.GetConstantScale: Single;
begin
  Result := 0.00;
  
  if FMonochrome then
  begin
    Result := FGrayScaleForConst;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          Result := FConstantScaleForOutputRed;
        end;

      csGreen:
        begin
          Result := FConstantScaleForOutputGreen;
        end;

      csBlue:
        begin
          Result := FConstantScaleForOutputBlue;
        end;
    end;
  end;
end;

function TgmChannelMixer.GetOutputColor: TColor32;
var
  LInR, LInG, LInB         : Cardinal;
  LOutR, LOutG, LOutB      : Cardinal;
  LAccumR, LAccumG, LAccumB: Single;
  LTmpR, LTmpG, LTmpB      : Integer;
begin
  LInR := FInputColor shr 16 and $FF;
  LInG := FInputColor shr  8 and $FF;
  LInB := FInputColor        and $FF;

  LAccumR := 0;
  LAccumG := 0;
  LAccumB := 0;

  // get output red channel...
  LAccumR := LAccumR + LInR * FRedScaleForOutputRed;
  LAccumR := LAccumR + LInG * FGreenScaleForOutputRed;
  LAccumR := LAccumR + LInB * FBlueScaleForOutputRed;
  LAccumR := LAccumR + 255  * FConstantScaleForOutputRed;

  // get output green channel...
  LAccumG := LAccumG + LInR * FRedScaleForOutputGreen;
  LAccumG := LAccumG + LInG * FGreenScaleForOutputGreen;
  LAccumG := LAccumG + LInB * FBlueScaleForOutputGreen;
  LAccumG := LAccumG + 255  * FConstantScaleForOutputGreen;

  // get output blue channel...
  LAccumB := LAccumB + LInR * FRedScaleForOutputBlue;
  LAccumB := LAccumB + LInG * FGreenScaleForOutputBlue;
  LAccumB := LAccumB + LInB * FBlueScaleForOutputBlue;
  LAccumB := LAccumB + 255  * FConstantScaleForOutputBlue;

  LTmpR := Round(LAccumR);
  LTmpG := Round(LAccumG);
  LTmpB := Round(LAccumB);

  LOutR := Clamp(LTmpR, 0, 255);
  LOutG := Clamp(LTmpG, 0, 255);
  LOutB := Clamp(LTmpB, 0, 255);

  Result := (FInputColor and $FF000000) or (LOutR shl 16) or (LOutG shl 8) or LOutB;
end;

procedure TgmChannelMixer.SetRedScale(const Value: Single);
begin
  if FMonochrome then
  begin
    FGrayScaleForRed        := Value;
    FRedScaleForOutputRed   := FGrayScaleForRed;
    FRedScaleForOutputGreen := FGrayScaleForRed;
    FRedScaleForOutputBlue  := FGrayScaleForRed;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          FRedScaleForOutputRed := Value;
        end;

      csGreen:
        begin
          FRedScaleForOutputGreen := Value;
        end;

      csBlue:
        begin
          FRedScaleForOutputBlue := Value;
        end;
    end;
  end;
end;

procedure TgmChannelMixer.SetGreenScale(const Value: Single);
begin
  if FMonochrome then
  begin
    FGrayScaleForGreen        := Value;
    FGreenScaleForOutputRed   := FGrayScaleForGreen;
    FGreenScaleForOutputGreen := FGrayScaleForGreen;
    FGreenScaleForOutputBlue  := FGrayScaleForGreen;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          FGreenScaleForOutputRed := Value;
        end;

      csGreen:
        begin
          FGreenScaleForOutputGreen := Value;
        end;

      csBlue:
        begin
          FGreenScaleForOutputBlue := Value;
        end;
    end;
  end;
end;

procedure TgmChannelMixer.SetBlueScale(const Value: Single);
begin
  if FMonochrome then
  begin
    FGrayScaleForBlue        := Value;
    FBlueScaleForOutputRed   := FGrayScaleForBlue;
    FBlueScaleForOutputGreen := FGrayScaleForBlue;
    FBlueScaleForOutputBlue  := FGrayScaleForBlue;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          FBlueScaleForOutputRed := Value;
        end;

      csGreen:
        begin
          FBlueScaleForOutputGreen := Value;
        end;

      csBlue:
        begin
          FBlueScaleForOutputBlue  := Value;
        end;
    end;
  end;
end;

procedure TgmChannelMixer.SetConstantScale(const Value: Single);
begin
  if FMonochrome then
  begin
    FGrayScaleForConst           := Value;
    FConstantScaleForOutputRed   := FGrayScaleForConst;
    FConstantScaleForOutputGreen := FGrayScaleForConst;
    FConstantScaleForOutputBlue  := FGrayScaleForConst;
  end
  else
  begin
    case FChannel of
      csRed:
        begin
          FConstantScaleForOutputRed := Value;
        end;

      csGreen:
        begin
          FConstantScaleForOutputGreen := Value;
        end;

      csBlue:
        begin
          FConstantScaleForOutputBlue := Value;
        end;
    end;
  end;
end;

procedure TgmChannelMixer.SetMonochrome(const Value: Boolean);
begin
  FMonochrome := Value;

  if FMonochrome then
  begin
    case FChannel of
      csRed:
        begin
          SetRedScale(FRedScaleForOutputRed);
          SetGreenScale(FGreenScaleForOutputRed);
          SetBlueScale(FBlueScaleForOutputRed);
          SetConstantScale(FConstantScaleForOutputRed);
        end;

      csGreen:
        begin
          SetRedScale(FRedScaleForOutputGreen);
          SetGreenScale(FGreenScaleForOutputGreen);
          SetBlueScale(FBlueScaleForOutputGreen);
          SetConstantScale(FConstantScaleForOutputGreen);
        end;

      csBlue:
        begin
          SetRedScale(FRedScaleForOutputBlue);
          SetGreenScale(FGreenScaleForOutputBlue);
          SetBlueScale(FBlueScaleForOutputBlue);
          SetConstantScale(FConstantScaleForOutputBlue);
        end;
    end;
  end;
end;

procedure TgmChannelMixer.SetChannel(const Value: TgmChannelSelector);
begin
  if Value in [csRed, csGreen, csBlue] then
  begin
    FChannel := Value;
  end;
end; 

end.
