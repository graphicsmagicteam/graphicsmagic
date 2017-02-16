{
  Original authors:
    Ma Xiaoguang, Ma Xiaoming

  CopyRight(C) Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved.
  
  Reference materials:
    http://www.dougkerr.net/pumpkin/articles/Unsharp_Mask.pdf

  Last Update:
    2012-09-13

  ------------------------------------------------------------------

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

unit UnsharpMaskLib;

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
{ Graphics32 }
  GR32, GR32_LowLevel,
{ GraphicsMagic }
  gmGimpGaussianBlur;

type
  TgmUnsharpMask = class(TObject)
  private
    FSourceBitmap : TBitmap32;  // pointer to a bitmap
    FBlurredBitmap: TBitmap32;
    FAmount       : Double;
    FRadius       : Double;
    FThreshold    : Byte;
    FBlurFilter   : TgmGimpGaussianBlur;

    procedure SetAmount(AValue: Double);
    procedure SetRadius(AValue: Double);
  public
    constructor Create(ASourceBmp: TBitmap32);
    destructor Destroy; override;

    procedure SetSourceBitmap(ASourceBmp: TBitmap32);
    procedure Execute(ADestBmp: TBitmap32);

    property Amount   : Double read FAmount    write SetAmount;
    property Radius   : Double read FRadius    write SetRadius;
    property Threshold: Byte   read FThreshold write FThreshold;
  end;

  procedure UnsharpMask(ADestBmp: TBitmap32; const AAmount, ARadius: Double;
    const AThreshold: Byte);

implementation

uses
  Math;

const
  MIN_AMOUNT = 0.01;
  MAX_AMOUNT = 5.00;
  MIN_RADIUS = 0.1;
  MAX_RADIUS = 250.0;

procedure UnsharpMask(ADestBmp: TBitmap32; const AAmount, ARadius: Double;
  const AThreshold: Byte);
var
  LBlurredBmp : TBitmap32;
  LAmount     : Double;
  LRadius     : Double;
  i           : Integer;
  mr, mg, mb  : Integer;
  br, bg, bb  : Byte;
  dr, dg, db  : Byte;
  LMaskLight  : Cardinal;
  LDestBit    : PColor32;
  LBlurBit    : PColor32;
  LBlurFilter : TgmGimpGaussianBlur;
begin
  if ( not Assigned(ADestBmp) ) or
     ( ADestBmp.Width = 0 ) or
     ( ADestBmp.Height = 0 ) then
  begin
    Exit;
  end;

  LAmount := AAmount;
  LRadius := ARadius;

  if LAmount < MIN_AMOUNT then
  begin
    LAmount := MIN_AMOUNT;
  end
  else if LAmount > MAX_AMOUNT then
  begin
    LAmount := MAX_AMOUNT;
  end;

  if LRadius < MIN_RADIUS then
  begin
    LRadius := MIN_RADIUS;
  end
  else if LRadius > MAX_RADIUS then
  begin
    LRadius := MAX_RADIUS;
  end;

  LBlurredBmp := TBitmap32.Create;
  try
    LBlurredBmp.Assign(ADestBmp);

    LBlurFilter := TgmGimpGaussianBlur.Create;
    try
      LBlurFilter.HorizontalRadius := LRadius;
      LBlurFilter.VerticalRadius   := LRadius;

      LBlurFilter.Execute(LBlurredBmp);
    finally
      LBlurFilter.Free;
    end;

    LDestBit := @ADestBmp.Bits[0];
    LBlurBit := @LBlurredBmp.Bits[0];
    
    for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
    begin
      br := LBlurBit^ shr 16 and $FF;
      bg := LBlurBit^ shr  8 and $FF;
      bb := LBlurBit^        and $FF;

      dr := LDestBit^ shr 16 and $FF;
      dg := LDestBit^ shr  8 and $FF;
      db := LDestBit^        and $FF;

      mr := dr - br;
      mg := dg - bg;
      mb := db - bb;

      mr := Round(mr * LAmount);
      mg := Round(mg * LAmount);
      mb := Round(mb * LAmount);

      LMaskLight := ( Abs(mr) + Abs(mg) + Abs(mb) ) div 3;

      if LMaskLight >= AThreshold then
      begin
        if mr > 0 then
        begin
          mr := mr - AThreshold;

          if mr < 0 then
          begin
            mr := 0;
          end;
        end
        else
        begin
          mr := mr + AThreshold;

          if mr > 0 then
          begin
            mr := 0;
          end;
        end;

        if mg > 0 then
        begin
          mg := mg - AThreshold;

          if mg < 0 then
          begin
            mg := 0;
          end;
        end
        else
        begin
          mg := mg + AThreshold;

          if mg > 0 then
          begin
            mg := 0;
          end;
        end;

        if mb > 0 then
        begin
          mb := mb - AThreshold;

          if mb < 0 then
          begin
            mb := 0;
          end;
        end
        else
        begin
          mb := mb + AThreshold;
          
          if mb > 0 then
          begin
            mb := 0;
          end;
        end;

        mr := dr + mr;
        mg := dg + mg;
        mb := db + mb;

        dr := Clamp(mr, 0, 255);
        dg := Clamp(mg, 0, 255);
        db := Clamp(mb, 0, 255);

        LDestBit^ := (LDestBit^ and $FF000000) or (dr shl 16) or (dg shl 8) or db;
      end;

      Inc(LDestBit);
      Inc(LBlurBit);
    end;
  finally
    LBlurredBmp.Free;
  end;
end; 

{ TgmUnsharpMask }

constructor TgmUnsharpMask.Create(ASourceBmp: TBitmap32);
begin
  inherited Create;

  FSourceBitmap  := ASourceBmp;
  FBlurredBitmap := nil;
  FBlurFilter    := TgmGimpGaussianBlur.Create;

  FAmount    := 50.0;
  FRadius    := 1.0;
  FThreshold := 0;

  if Assigned(FSourceBitmap) then
  begin
    FBlurredBitmap := TBitmap32.Create;
    FBlurredBitmap.Assign(FSourceBitmap);

    FBlurFilter.HorizontalRadius := FRadius;
    FBlurFilter.VerticalRadius   := FRadius;

    FBlurFilter.Execute(FBlurredBitmap);
  end;
end; 

destructor TgmUnsharpMask.Destroy;
begin
  FSourceBitmap := nil;
  FBlurredBitmap.Free;
  FBlurFilter.Free;
  
  inherited Destroy;
end; 

procedure TgmUnsharpMask.SetAmount(AValue: Double);
begin
  if AValue < MIN_AMOUNT then
  begin
    FAmount := MIN_AMOUNT;
  end
  else if AValue > MAX_AMOUNT then
  begin
    FAmount := MAX_AMOUNT;
  end
  else
  begin
    FAmount := AValue;
  end;
end; 

procedure TgmUnsharpMask.SetRadius(AValue: Double);
begin
  if AValue < MIN_RADIUS then
  begin
    FRadius := MIN_RADIUS;
  end
  else if AValue > MAX_RADIUS then
  begin
    FRadius := MAX_RADIUS;
  end
  else
  begin
    FRadius := AValue;
  end;

  if Assigned(FSourceBitmap) and Assigned(FBlurredBitmap) then
  begin
    FBlurredBitmap.Assign(FSourceBitmap);

    FBlurFilter.HorizontalRadius := FRadius;
    FBlurFilter.VerticalRadius   := FRadius;

    FBlurFilter.Execute(FBlurredBitmap);
  end;
end; 

procedure TgmUnsharpMask.SetSourceBitmap(ASourceBmp: TBitmap32);
begin
  if Assigned(ASourceBmp) then
  begin
    FSourceBitmap := ASourceBmp;

    if Assigned(FSourceBitmap) then
    begin
      if not Assigned(FBlurredBitmap) then
      begin
        FBlurredBitmap := TBitmap32.Create;
      end;

      FBlurredBitmap.Assign(FSourceBitmap);

      FBlurFilter.HorizontalRadius := FRadius;
      FBlurFilter.VerticalRadius   := FRadius;

      FBlurFilter.Execute(FBlurredBitmap);
    end;
  end;
end; 

procedure TgmUnsharpMask.Execute(ADestBmp: TBitmap32);
var
  i          : Integer;
  mr, mg, mb : Integer;
  br, bg, bb : Byte;
  dr, dg, db : Byte;
  LMaskLight : Cardinal;
  LDestBit   : PColor32;
  LBlurBit   : PColor32;
begin
  if ( not Assigned(ADestBmp) ) or
     ( not Assigned(FSourceBitmap) ) or
     ( not Assigned(FBlurredBitmap) ) then
  begin
    Exit;
  end;

  ADestBmp.Assign(FSourceBitmap);

  LDestBit := @ADestBmp.Bits[0];
  LBlurBit := @FBlurredBitmap.Bits[0];

  for i := 0 to (ADestBmp.Width * ADestBmp.Height - 1) do
  begin
    br := LBlurBit^ shr 16 and $FF;
    bg := LBlurBit^ shr  8 and $FF;
    bb := LBlurBit^        and $FF;

    dr := LDestBit^ shr 16 and $FF;
    dg := LDestBit^ shr  8 and $FF;
    db := LDestBit^        and $FF;

    mr := dr - br;
    mg := dg - bg;
    mb := db - bb;

    mr := Round(mr * FAmount);
    mg := Round(mg * FAmount);
    mb := Round(mb * FAmount);

    LMaskLight := ( Abs(mr) + Abs(mg) + Abs(mb) ) div 3;

    if LMaskLight >= FThreshold then
    begin
      if mr > 0 then
      begin
        mr := mr - FThreshold;

        if mr < 0 then
        begin
          mr := 0;
        end;
      end
      else
      begin
        mr := mr + FThreshold;

        if mr > 0 then
        begin
          mr := 0;
        end;
      end;

      if mg > 0 then
      begin
        mg := mg - FThreshold;

        if mg < 0 then
        begin
          mg := 0;
        end;
      end
      else
      begin
        mg := mg + FThreshold;

        if mg > 0 then
        begin
          mg := 0;
        end;
      end;

      if mb > 0 then
      begin
        mb := mb - FThreshold;

        if mb < 0 then
        begin
          mb := 0;
        end;
      end
      else
      begin
        mb := mb + FThreshold;
        
        if mb > 0 then
        begin
          mb := 0;
        end;
      end;

      mr := dr + mr;
      mg := dg + mg;
      mb := db + mb;

      dr := Clamp(mr, 0, 255);
      dg := Clamp(mg, 0, 255);
      db := Clamp(mb, 0, 255);

      LDestBit^ := (LDestBit^ and $FF000000) or (dr shl 16) or (dg shl 8) or db;
    end;

    Inc(LDestBit);
    Inc(LBlurBit);
  end;
end; 

end.
