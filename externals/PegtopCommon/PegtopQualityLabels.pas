////////////////////////////////////////////////////////////////////////////////
// File:       PegtopQualityLabels.pas
// Classes:    TPegtopQualityLabel
// Version:    1.02
// History:    1.00 18 Mar 2005 created
//             1.01 19 Mar 2005 optimized for speed and resources:
//                              drawing only in clipping rectangle,
//                              one temporary bitmap instead of two,
//                              exception handling added
//             1.02 13 Nov 2005 alpha blending added (new opacity property)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// PegtopQualityLabels is a TLabel which supports 2x and 4x antialiasing
// (only if the CPU supports MMX, otherwise the text is drawn normally).
// and alpha blending for semi transparent text (uses more resources).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopQualityLabels;

interface

uses
  Windows, Classes, Controls, Graphics, StdCtrls;

type
  TPegtopLabelQuality = (plqNormal, plqResampling2x, plqResampling4x);

  TPegtopQualityLabel = class(TLabel)
  private
    FQuality: TPegtopLabelQuality;
    FOpacity: Integer;
    procedure RenderText(var Rect: TRect; Flags: Longint);
    procedure SetQuality(Value: TPegtopLabelQuality);
    procedure SetOpacity(Value: Integer);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Quality: TPegtopLabelQuality read FQuality write SetQuality default plqResampling2x;
    property Opacity: Integer read FOpacity write SetOpacity default 256;
  end;

implementation

uses
  PegtopResampleUtils;

const
  AAFactors: array[TPegtopLabelQuality] of Integer = (1, 2, 4);

procedure BlendBitmap32(const DestOrigin: Pointer; const DestPitch: Integer;
  const SourceOrigin: Pointer; const SourcePitch: Integer;
  const DestRect: TRect; const SourceX, SourceY, Alpha: Integer);
var
  X, Y: Integer;
  A: Integer;
  DestQ, DestP: ^Byte;
  SourceQ, SourceP: ^Byte;
begin
  DestQ := Pointer(Integer(DestOrigin) + DestRect.Top * DestPitch + DestRect.Left * 4);
  SourceQ := Pointer(Integer(SourceOrigin) + SourceY * DestPitch + SourceX * 4);
  for Y := DestRect.Top to DestRect.Bottom - 1 do begin
    DestP := DestQ;
    SourceP := SourceQ;
    for X := DestRect.Left to DestRect.Right - 1 do begin
      DestP^ := DestP^ + (SourceP^ - DestP^) * Alpha div 256;
      Inc(DestP);
      Inc(SourceP);
      DestP^ := DestP^ + (SourceP^ - DestP^) * Alpha div 256;
      Inc(DestP);
      Inc(SourceP);
      DestP^ := DestP^ + (SourceP^ - DestP^) * Alpha div 256;
      Inc(DestP, 2);
      Inc(SourceP, 2);
    end;
    DestQ := Pointer(Integer(DestQ) + DestPitch);
    SourceQ := Pointer(Integer(SourceQ) + SourcePitch);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopQualityLabel
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopQualityLabel.Create(AOwner: TComponent);
begin
  inherited;
  FQuality := plqResampling2x;
  FOpacity := 256;
end;

procedure TPegtopQualityLabel.RenderText(var Rect: TRect; Flags: Longint);
var
  TempBitmap: TBitmap;
  TempOrigin: Pointer;
  TempPitch: Integer;
  TempRect: TRect;
  AlphaBitmap: TBitmap;
  AlphaOrigin: Pointer;
  AlphaPitch: Integer;
  L, T: Integer;
  W, H: Integer;
  X, Y: Integer;
  MaxTileSize: TSize;
  TileSize: TSize;
  FirstTile: Boolean;
begin
  if Opacity <= 0 then Exit;
  if (FQuality <> plqNormal) or (FOpacity < 256) then begin
    try
      // to save resources we are working with tiles not larger than 256x256:
      MaxTileSize.CX := 256 div AAFactors[FQuality];
      MaxTileSize.CY := 256 div AAFactors[FQuality];
      L := Rect.Left;
      T := Rect.Top;
      W := (Rect.Right - Rect.Left);
      H := (Rect.Bottom - Rect.Top);
      // if the text is small enough, tiles can be made smaller:
      if W < MaxTileSize.CX then MaxTileSize.CX := W;
      if H < MaxTileSize.CY then MaxTileSize.CY := H;
      TempBitmap := TBitmap.Create;
      try
        // prepare temporary buffer bitmap:
        TempBitmap.PixelFormat := pf32bit;
        TempBitmap.Width := MaxTileSize.CX * AAFactors[FQuality];
        TempBitmap.Height := MaxTileSize.CY * AAFactors[FQuality];
        TempBitmap.Canvas.Brush := Canvas.Brush;
        TempBitmap.Canvas.Font := Canvas.Font;
        TempBitmap.Canvas.Font.Size := Canvas.Font.Size * AAFactors[FQuality];
        TempOrigin := TempBitmap.ScanLine[0];
        TempPitch := Integer(TempBitmap.ScanLine[1]) - Integer(TempOrigin);
        if FOpacity <= 256 then AlphaBitmap := TBitmap.Create else AlphaBitmap := NIL;
        try
          if Assigned(AlphaBitmap) then begin
            AlphaBitmap.PixelFormat := pf32bit;
            AlphaBitmap.Width := TempBitmap.Width;
            AlphaBitmap.Height := TempBitmap.Height;
            AlphaOrigin := AlphaBitmap.ScanLine[0];
            AlphaPitch := Integer(AlphaBitmap.ScanLine[1]) - Integer(AlphaOrigin);
          end;
          // initialization for loop:
          FirstTile := True;
          TileSize.CY := MaxTileSize.CY;
          Y := 0;
          if Y < Canvas.ClipRect.Top - T then Y := Canvas.ClipRect.Top - T;
          while (Y < H) and (Y < Canvas.ClipRect.Bottom - T) do begin
            // reduce tile height if larger than required:
            if (TileSize.CY > Canvas.ClipRect.Bottom - T - Y) then
              TileSize.CY := Canvas.ClipRect.Bottom - T - Y;
            // restore tile width:
            TileSize.CX := MaxTileSize.CX;
            X := 0;
            if X < Canvas.ClipRect.Left - L then X := Canvas.ClipRect.Left - L;
            while (X < W) and (X < Canvas.ClipRect.Right - L) do begin
              // reduce tile width if larger than required:
              if (TileSize.CX > Canvas.ClipRect.Right - L - X) then
                TileSize.CX := Canvas.ClipRect.Right - L - X;
              // copy background to temporary buffer:
              StretchBlt(TempBitmap.Canvas.Handle,
                0, 0, TileSize.CX * AAFactors[FQuality], TileSize.CY * AAFactors[FQuality],
                Canvas.Handle, L + X, T + Y, TileSize.CX, TileSize.CY,
                SRCCOPY);
              // copy background to alpha blending buffer:
              if Assigned(AlphaBitmap) then begin
                BitBlt(AlphaBitmap.Canvas.Handle,
                  0, 0, TileSize.CX, TileSize.CY,
                  Canvas.Handle, L + X, T + Y,
                  SRCCOPY);
              end;
              // draw text to temporary buffer:
              TempRect := Classes.Bounds(-X * AAFactors[FQuality], -Y * AAFactors[FQuality],
                W * AAFactors[FQuality], H * AAFactors[FQuality]);
              DrawText(TempBitmap.Canvas.Handle, PChar(Text), Length(Text), TempRect, Flags);
              // resample buffer:
              // (in this special case we can use the same bitmap as source and destination,
              // because source coordinates >= destination coordinates)
              if FQuality = plqResampling4x then
                PegtopResample4x32(TempOrigin, TempPitch, Classes.Rect(0, 0, TileSize.CX, TileSize.CY),
                  TempOrigin, TempPitch, 0, 0)
              else if FQuality = plqResampling2x then
                PegtopResample2x32(TempOrigin, TempPitch, Classes.Rect(0, 0, TileSize.CX, TileSize.CY),
                  TempOrigin, TempPitch, 0, 0);
              // do alpha blending
              if Assigned(AlphaBitmap) then begin
                BlendBitmap32(TempOrigin, TempPitch, AlphaOrigin, AlphaPitch,
                  Bounds(0, 0, TileSize.CX, TileSize.CY), 0, 0, 256 - FOpacity);
              end;
              // draw temporary buffer to screen:
              BitBlt(Canvas.Handle, L + X, T + Y, TileSize.CX, TileSize.CY,
                TempBitmap.Canvas.Handle, 0, 0, SRCCOPY);
              // we need to return the rectangle the text actually is drawn to
              // (is used to resize the label automatically):
              if FirstTile then begin
                Rect := Classes.Rect(
                  L + (TempRect.Left + AAFactors[FQuality] - 1) div AAFactors[FQuality],
                  T + (TempRect.Top + AAFactors[FQuality] - 1) div AAFactors[FQuality],
                  (TempRect.Right - TempRect.Left + AAFactors[FQuality] - 1) div AAFactors[FQuality],
                  (TempRect.Bottom - TempRect.Top + AAFactors[FQuality] - 1) div AAFactors[FQuality]
                );
                FirstTile := False;
              end;
              Inc(X, TileSize.CX);
            end;
            Inc(Y, TileSize.CY);
          end;
        finally
          AlphaBitmap.Free;
        end;
      finally
        TempBitmap.Free;
      end;
    except
      // if an error occured (bitmap could not be created)
      // draw text normally:
      DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    end;
  end
  else begin
    // draw text normally:
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end;
end;

procedure TPegtopQualityLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
begin
  // this procedure very much equals TLabel.DoDrawText
  // (has to be overridden, because we have to call our RenderText method
  // instead of DrawText API directly)
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    RenderText(Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    RenderText(Rect, Flags);
  end
  else
    RenderText(Rect, Flags);
end;

procedure TPegtopQualityLabel.SetQuality(Value: TPegtopLabelQuality);
begin
  if FQuality <> Value then begin
    FQuality := Value;
    Invalidate;
  end;
end;

procedure TPegtopQualityLabel.SetOpacity(Value: Integer);
begin
  if Value < 0 then Value := 0 else if Value > 256 then Value := 256;
  if FOpacity <> Value then begin
    FOpacity := Value;
    Invalidate;
  end;
end;

end.
