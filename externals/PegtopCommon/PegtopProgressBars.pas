////////////////////////////////////////////////////////////////////////////////
// File:       PegtopProgressBars.pas
// Classes:    TPegtopProgressBar
// Version:    1.01
// Date:       16 Jun 2004 1.00
//             14 Aug 2005 1.01 (Snake property added for "continual progress")
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopProgressBar is another progress bar offering much more possibilities
// the normal ones. Using XP themes (if possible) the single dots can be colored
// (color gradients can be made easily), a caption can be added (even vertical)
// and many other properties can be set.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopProgressBars;

interface

uses
  Classes, Windows, Graphics, Controls,
  PegtopThemes;

type
  TPegtopProgressBarStyle = (ppsDefault, ppsSolid);
  TPegtopProgressBarDirection = (ppdRight, ppdLeft, ppdUp, ppdDown);
  TPegtopProgressBarCaptionAlignment = (ppaFront, ppaBack, ppaCenter);

  TPegtopProgressBarColorEvent = procedure(Sender: TObject; Position: Integer; var Color: TColor) of object;

  TPegtopProgressBar = class(TGraphicControl)
  private
    FMIn: Integer;
    FMax: Integer;
    FPosition: Integer;
    FDirection: TPegtopProgressBarDirection;
    FExtent: Integer;
    FSpacing: Integer;
    FSnake: Integer;
    FCaption: TCaption;
    FCaptionAlignment: TPegtopProgressBarCaptionAlignment;
    FFrontSpace: Integer;
    FBackSpace: Integer;
    FInvers: Boolean;
    FStyle: TPegtopProgressBarStyle;
    FOnColor: TPegtopProgressBarColorEvent;
    function GetProgressWidth: Integer;
    function PositionToIndex(Value: Integer): Integer;
    function IndexToPosition(Index: Integer): Integer;
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetDirection(Value: TPegtopProgressBarDirection);
    procedure SetExtent(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetSnake(Value: Integer);
    procedure SetCaption(Value: TCaption);
    procedure SetCaptionAlignment(Value: TPegtopProgressBarCaptionAlignment);
    procedure SetFrontSpace(Value: Integer);
    procedure SetBackSpace(Value: Integer);
    procedure SetInvers(Value: Boolean);
    procedure SetOnColor(Value: TPegtopProgressBarColorEvent);
    procedure SetStyle(Value: TPegtopProgressBarStyle);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetGradientColor(Color1, Color2: TColor; Pos: Integer): TColor; overload;
    function GetGradientColor(Color1, Color2: TColor; Pos1, Pos2, Pos: Integer): TColor; overload;
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition;
    property Direction: TPegtopProgressBarDirection read FDirection write SetDirection default ppdRight;
    property Extent: Integer read FExtent write SetExtent;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Snake: Integer read FSnake write SetSnake default 0;
    property Caption: TCaption read FCaption write SetCaption;
    property CaptionAlignment: TPegtopProgressBarCaptionAlignment read FCaptionAlignment write SetCaptionAlignment default ppaBack;
    property FrontSpace: Integer read FFrontSpace write SetFrontSpace default 0;
    property BackSpace: Integer read FBackSpace write SetBackSpace default 0;
    property Invers: Boolean read FInvers write SetInvers default False;
    property Style: TPegtopProgressBarStyle read FStyle write SetStyle default ppsDefault;
    property OnColor: TPegtopProgressBarColorEvent read FOnColor write SetOnColor;
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils;

const
  DefBorder = 4;
  MinBorder = 2;

////////////////////////////////////////////////////////////////////////////////
// TPegtopProgressBar
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopProgressBar.Create(AOwner: TComponent);
begin
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FDirection := ppdRight;
  FExtent := 6;
  FSpacing := 2;
  FCaption := '';
  FCaptionAlignment := ppaBack;
  FFrontSpace := 0;
  FBackSpace := 0;
  FInvers := False;
  FStyle := ppsDefault;
  FOnColor := NIL;
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  Width := 129;
  Height := 17;
end;

procedure TPegtopProgressBar.Paint;
var
  I, A, B: Integer;
  DrawRect: TRect;
  C: TColor;
  TempBitmap: TBitmap;
  CaptionSize: TSize;
  LogFont: TLogFont;
  TextMetric: TTextMetric;
  RotFont, OrgFont: THandle;
  ProgressRect: TRect;
begin
  if FSnake > 0 then begin
    A := PositionToIndex(FPosition) - FSnake;
    if A < 0 then A := 0;
    B := PositionToIndex(FPosition) - 1;
    if B < 0 then B := 0;
  end
  else begin
    if FInvers then begin
      A := PositionToIndex(FPosition);
      B := PositionToIndex(FMax) - 1;
    end
    else begin
      A := 0;
      B := PositionToIndex(FPosition) - 1;
    end;
  end;

  TempBitmap := TBitmap.Create;
  try
    TempBitmap.Width := ClientWidth;
    TempBitmap.Height := ClientHeight;

    case FStyle of
      ppsDefault:
        if FDirection in [ppdUp, ppdDown] then
          DefaultTheme.DrawProgressBox(TempBitmap.Canvas.Handle, ClientRect, peoVertical)
        else
          DefaultTheme.DrawProgressBox(TempBitmap.Canvas.Handle, ClientRect, peoHorizontal);
      ppsSolid:
        begin
          TempBitmap.Canvas.Brush.Color := Color;
          TempBitmap.Canvas.FillRect(ClientRect);
        end;
    end;

    case FDirection of
      ppdRight:
        begin
          ProgressRect.Left := DefBorder + FFrontSpace;
          ProgressRect.Right := ClientWidth - MinBorder - FBackSpace;
          ProgressRect.Top := 3;
          ProgressRect.Bottom := ClientHeight - 3;
          if Assigned(FOnColor) then begin
            TempBitmap.Canvas.Brush.Style := bsSolid;
            for I := A to B do begin
              C := clHighlight;
              FOnColor(Self, IndexToPosition(I), C);
              TempBitmap.Canvas.Brush.Color := C;
              DrawRect := Rect(ProgressRect.Left + I * (FExtent+FSpacing), ProgressRect.Top + 1,
                ProgressRect.Left + I * (FExtent + FSpacing) + FExtent, ProgressRect.Bottom - 1);
              if DrawRect.Right > ProgressRect.Right then DrawRect.Right := ProgressRect.Right;
              if DrawRect.Left < DrawRect.Right then TempBitmap.Canvas.FillRect(DrawRect);
            end;
          end
          else begin
            for I := A to B do begin
              DrawRect := Rect(ProgressRect.Left + I * (FExtent+FSpacing), ProgressRect.Top,
                ProgressRect.Left + I * (FExtent + FSpacing) + FExtent, ProgressRect.Bottom);
              if DrawRect.Right > ProgressRect.Right then DrawRect.Right := ProgressRect.Right;
              if DrawRect.Left < DrawRect.Right then DefaultTheme.DrawProgressBar(TempBitmap.Canvas.Handle, DrawRect, peoHorizontal);
            end;
          end;
          if FCaption <> '' then begin
            TempBitmap.Canvas.Brush.Style := bsClear;
            TempBitmap.Canvas.Font.Assign(Font);
            CaptionSize := TempBitmap.Canvas.TextExtent(FCaption);
            case FCaptionAlignment of
              ppaFront:
                TempBitmap.Canvas.TextOut(DefBorder,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
              ppaBack:
                TempBitmap.Canvas.TextOut(ClientWidth - DefBorder - CaptionSize.cx,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
              ppaCenter:
                TempBitmap.Canvas.TextOut((ClientWidth - CaptionSize.cx) div 2,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
            end;
          end;
        end;
      ppdLeft:
        begin
          ProgressRect.Left := MinBorder + FFrontSpace;
          ProgressRect.Right := ClientWidth - DefBorder - FBackSpace;
          ProgressRect.Top := 3;
          ProgressRect.Bottom := ClientHeight - 3;
          if Assigned(FOnColor) then begin
            TempBitmap.Canvas.Brush.Style := bsSolid;
            for I := A to B do begin
              C := clHighlight;
              FOnColor(Self, IndexToPosition(I), C);
              TempBitmap.Canvas.Brush.Color := C;
              DrawRect := Rect(ProgressRect.Right - I * (FExtent+FSpacing) - FExtent, ProgressRect.Top + 1,
                ProgressRect.Right - I * (FExtent + FSpacing), ProgressRect.Bottom - 1);
              if DrawRect.Left < ProgressRect.Left then DrawRect.Left := ProgressRect.Left;
              if DrawRect.Left < DrawRect.Right then TempBitmap.Canvas.FillRect(DrawRect);
            end;
          end
          else begin
            for I := A to B do begin
              DrawRect := Rect(ProgressRect.Right - I * (FExtent+FSpacing) - FExtent, ProgressRect.Top,
                ProgressRect.Right - I * (FExtent + FSpacing), ProgressRect.Bottom);
              if DrawRect.Left < ProgressRect.Left then DrawRect.Left := ProgressRect.Left;
              if DrawRect.Left < DrawRect.Right then DefaultTheme.DrawProgressBar(TempBitmap.Canvas.Handle, DrawRect, peoHorizontal);
            end;
          end;
          if FCaption <> '' then begin
            TempBitmap.Canvas.Brush.Style := bsClear;
            TempBitmap.Canvas.Font.Assign(Font);
            CaptionSize := TempBitmap.Canvas.TextExtent(FCaption);
            case FCaptionAlignment of
              ppaFront:
                TempBitmap.Canvas.TextOut(DefBorder,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
              ppaBack:
                TempBitmap.Canvas.TextOut(ClientWidth - DefBorder - CaptionSize.cx,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
              ppaCenter:
                TempBitmap.Canvas.TextOut((ClientWidth - CaptionSize.cx) div 2,
                  (ClientHeight - CaptionSize.cy) div 2, FCaption);
            end;
          end;
        end;
      ppdUp:
        begin
          ProgressRect.Top := MinBorder + FBackSpace;
          ProgressRect.Bottom := ClientHeight - DefBorder - FFrontSpace;
          ProgressRect.Left := 3;
          ProgressRect.Right := ClientWidth - 3;
          if Assigned(FOnColor) then begin
            TempBitmap.Canvas.Brush.Style := bsSolid;
            for I := A to B do begin
              C := clHighlight;
              FOnColor(Self, IndexToPosition(I), C);
              TempBitmap.Canvas.Brush.Color := C;
              DrawRect := Rect(ProgressRect.Left + 1, ProgressRect.Bottom - I * (FExtent+FSpacing) - FExtent,
                ProgressRect.Right - 1, ProgressRect.Bottom - I * (FExtent+FSpacing));
              if DrawRect.Top < ProgressRect.Top then DrawRect.Top := ProgressRect.Top;
              if DrawRect.Top < DrawRect.Bottom then TempBitmap.Canvas.FillRect(DrawRect);
            end;
          end
          else begin
            for I := A to B do begin
              DrawRect := Rect(ProgressRect.Left, ProgressRect.Bottom - I * (FExtent+FSpacing) - FExtent,
                ProgressRect.Right, ProgressRect.Bottom - I * (FExtent+FSpacing));
              if DrawRect.Top < ProgressRect.Top then DrawRect.Top := ProgressRect.Top;
              if DrawRect.Top < DrawRect.Bottom then DefaultTheme.DrawProgressBar(TempBitmap.Canvas.Handle, DrawRect, peoVertical);
            end;
          end;
          if FCaption <> '' then begin
            TempBitmap.Canvas.Brush.Style := bsClear;
            TempBitmap.Canvas.Font.Assign(Font);
            GetObject(TempBitmap.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
            GetTextMetrics(TempBitmap.Canvas.Handle, TextMetric);
            if (TextMetric.tmPitchAndFamily and (TMPF_TRUETYPE or TMPF_VECTOR)) = 0 then // no truetype font
              StrCopy(LogFont.lfFaceName, 'Arial');
            LogFont.lfEscapement := 900; // 90.0°
            LogFont.lfOrientation := 900; // 90.0°
            RotFont := CreateFontIndirect(LogFont);
            try
              OrgFont := SelectObject(TempBitmap.Canvas.Handle, RotFont);
              try
                CaptionSize := TempBitmap.Canvas.TextExtent(FCaption);
                case FCaptionAlignment of
                  ppaFront:
                    TempBitmap.Canvas.TextOut((ClientWidth - CaptionSize.cy) div 2,
                      ClientHeight - DefBorder, FCaption);
                  ppaBack:
                    TempBitmap.Canvas.TextOut((ClientWidth - CaptionSize.cy) div 2,
                      DefBorder + CaptionSize.cx, FCaption);
                  ppaCenter:
                    TempBitmap.Canvas.TextOut((ClientWidth - CaptionSize.cy) div 2,
                      (ClientHeight - CaptionSize.cx) div 2 + CaptionSize.cx, FCaption);
                end;
              finally
                SelectObject(TempBitmap.Canvas.Handle, OrgFont);
              end;
            finally
              DeleteObject(RotFont);
            end;
          end;
        end;
      ppdDown:
        begin
          ProgressRect.Top := DefBorder + FFrontSpace;
          ProgressRect.Bottom := ClientHeight - MinBorder - FBackSpace;
          ProgressRect.Left := 3;
          ProgressRect.Right := ClientWidth - 3;
          if Assigned(FOnColor) then begin
            TempBitmap.Canvas.Brush.Style := bsSolid;
            for I := A to B do begin
              C := clHighlight;
              FOnColor(Self, IndexToPosition(I), C);
              TempBitmap.Canvas.Brush.Color := C;
              DrawRect := Rect(ProgressRect.Left + 1, ProgressRect.Top + I * (FExtent+FSpacing),
                ProgressRect.Right - 1, ProgressRect.Top + I * (FExtent+FSpacing) + FExtent);
              if DrawRect.Bottom > ProgressRect.Bottom then DrawRect.Bottom := ProgressRect.Bottom;
              if DrawRect.Top < DrawRect.Bottom then TempBitmap.Canvas.FillRect(DrawRect);
            end;
          end
          else begin
            for I := A to B do begin
              DrawRect := Rect(ProgressRect.Left, ProgressRect.Top + I * (FExtent+FSpacing),
                ProgressRect.Right, ProgressRect.Top + I * (FExtent+FSpacing) + FExtent);
              if DrawRect.Bottom > ProgressRect.Bottom then DrawRect.Bottom := ProgressRect.Bottom;
              if DrawRect.Top < DrawRect.Bottom then DefaultTheme.DrawProgressBar(TempBitmap.Canvas.Handle, DrawRect, peoVertical);
            end;
          end;
          if FCaption <> '' then begin
            TempBitmap.Canvas.Brush.Style := bsClear;
            TempBitmap.Canvas.Font.Assign(Font);;
            GetObject(TempBitmap.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
            GetTextMetrics(TempBitmap.Canvas.Handle, TextMetric);
            if (TextMetric.tmPitchAndFamily and (TMPF_TRUETYPE or TMPF_VECTOR)) = 0 then // no truetype font
              StrCopy(LogFont.lfFaceName, 'Arial');
            LogFont.lfEscapement := 2700; // 360.0° - 90.0°
            LogFont.lfOrientation := 2700; // 360.0° - 90.0°
            RotFont := CreateFontIndirect(LogFont);
            try
              OrgFont := SelectObject(TempBitmap.Canvas.Handle, RotFont);
              try
                CaptionSize := TempBitmap.Canvas.TextExtent(FCaption);
                case FCaptionAlignment of
                  ppaFront:
                    TempBitmap.Canvas.TextOut(ClientWidth - (ClientWidth - CaptionSize.cy) div 2,
                      DefBorder, FCaption);
                  ppaBack:
                    TempBitmap.Canvas.TextOut(ClientWidth - (ClientWidth - CaptionSize.cy) div 2,
                      ClientHeight - DefBorder - CaptionSize.cx, FCaption);
                  ppaCenter:
                    TempBitmap.Canvas.TextOut(ClientWidth - (ClientWidth - CaptionSize.cy) div 2,
                      (ClientHeight - CaptionSize.cx) div 2, FCaption);
                end;
              finally
                SelectObject(TempBitmap.Canvas.Handle, OrgFont);
              end;
            finally
              DeleteObject(RotFont);
            end;
          end;
        end;
    end;
    Canvas.Draw(0, 0, TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

function TPegtopProgressBar.GetGradientColor(Color1, Color2: TColor; Pos: Integer): TColor;
begin
  Result := GetGradientColor(Color1, Color2, FMin, FMax, Pos);
end;

function TPegtopProgressBar.GetGradientColor(Color1, Color2: TColor; Pos1, Pos2, Pos: Integer): TColor;
var
  S, P: Integer;
begin
  if Pos2 <= Pos1 then begin
    Result := Color1;
  end
  else begin
    if Pos < Pos1 then begin
      Result := Color1;
    end
    else if Pos > Pos2 then begin
      Result := Color2;
    end
    else begin
      S := Pos2 - Pos1;
      P := Pos - Pos1;
      Result := (((Color1 and $FF) * (S-P) + (Color2 and $FF) * P) div S)
      or (((Color1 shr 8 and $FF) * (S-P) + (Color2 shr 8 and $FF) * P) div S shl 8)
      or (((Color1 shr 16 and $FF) * (S-P) + (Color2 shr 16 and $FF) * P) div S shl 16);
    end;
  end;
end;

function TPegtopProgressBar.GetProgressWidth: Integer;
begin
  if FDirection in [ppdRight, ppdLeft] then
    Result := ClientWidth - (DefBorder + MinBorder) - (FFrontSpace + FBackSpace)
  else
    Result := ClientHeight - (DefBorder + MinBorder) - (FFrontSpace + FBackSpace);
end;

function TPegtopProgressBar.PositionToIndex(Value: Integer): Integer;
begin
  if FMax > FMin then begin
    if FSnake > 0 then begin
      Result := MulDiv(Value - FMin, GetProgressWidth + FSpacing + FSnake * (FExtent + FSpacing), (FMax - FMin) * (FExtent + FSpacing));
    end
    else begin
      Result := MulDiv(Value - FMin, GetProgressWidth + FSpacing, (FMax - FMin) * (FExtent + FSpacing));
    end;
  end
  else begin
    Result := 0;
  end;
end;

function TPegtopProgressBar.IndexToPosition(Index: Integer): Integer;
var
  C: Integer;
begin
  C := PositionToIndex(FMax);
  if C > 0 then
    Result := FMin + MulDiv(Index * 2 + 1, FMax - FMin + 1, C * 2)
  else
    Result := FMin;
end;

procedure TPegtopProgressBar.SetMin(Value: Integer);
begin
  if FMin <> Value then begin
    FMin := Value;
    if FMax < FMin then FMax := FMin;
    if FPosition < FMin then FPosition := FMin;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetMax(Value: Integer);
begin
  if FMax <> Value then begin
    FMax := Value;
    if FMin > FMax then FMin := FMax;
    if FPosition > FMax then FPosition := FMax;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetPosition(Value: Integer);
begin
  if Value < FMin then Value := FMin
  else if Value > FMax then Value := FMax;
  if FPosition <> Value then begin
    if PositionToIndex(FPosition) = PositionToIndex(Value) then begin
      FPosition := Value;
    end
    else begin
      FPosition := Value;
      Invalidate;
    end;
  end;
end;

procedure TPegtopProgressBar.SetDirection(Value: TPegtopProgressBarDirection);
begin
  if FDirection <> Value then begin
    FDirection := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetExtent(Value: Integer);
begin
  if Value < 1 then Value := 1
  else if Value > 8 then Value := 8;
  if FExtent <> Value then begin
    FExtent := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetSpacing(Value: Integer);
begin
  if Value < 1 then Value := 1
  else if Value > 8 then Value := 8;
  if FSpacing <> Value then begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetSnake(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSnake <> Value then begin
    FSnake := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetCaptionAlignment(Value: TPegtopProgressBarCaptionAlignment);
begin
  if FCaptionAlignment <> Value then begin
    FCaptionAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetFrontSpace(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FFrontSpace <> Value then begin
    FFrontSpace := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetBackSpace(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBackSpace <> Value then begin
    FBackSpace := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetInvers(Value: Boolean);
begin
  if FInvers <> Value then begin
    FInvers := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetOnColor(Value: TPegtopProgressBarColorEvent);
begin
  if (TMethod(FOnColor).Code <> TMethod(Value).Code)
  or (TMethod(FOnColor).Data <> TMethod(Value).Data) then begin
    FOnColor := Value;
    Invalidate;
  end;
end;

procedure TPegtopProgressBar.SetStyle(Value: TPegtopProgressBarStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Invalidate;
  end;
end;

end.
