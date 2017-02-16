////////////////////////////////////////////////////////////////////////////////
// File:       PegtopScrollers.pas
// Classes:    TPegtopScroller
// Version:    1.00
// Date:       16 Jun 2004
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopScroller is a simple scroller (know from motion pictures). Two
// columns are supported by using the separator '|' within the single text
// lines. OnLayout can be used to change the appearance of single lines (you
// can even change the text, e.g. after parsing it).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopScrollers;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms;

type
  TPegtopScrollerPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; DeltaX, DeltaY: Integer) of object;
  TPegtopScrollerColumn = (pscSingle, pscLeft, pscRight);
  TPegtopScrollerLayoutEvent = procedure(Sender: TObject; Line: Integer; Column: TPegtopScrollerColumn; var Text: String; LineFont: TFont; var LineAlignment: TAlignment) of object;

  TPegtopScroller = class(TCustomControl)
  private
    FLines: TStrings;
    FLineDistance: Integer;
    FPosition: Integer;
    FActive: Boolean;
    FInvisible: Boolean;
    FAlignment: TAlignment;
    FLeftColumnAlignment: TAlignment;
    FRightColumnAlignment: TAlignment;
    FOnPaint: TPegtopScrollerPaintEvent;
    FOnLayout: TPegtopScrollerLayoutEvent;
    procedure ScrollBy(DeltaY: Integer);
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure LinesChange(Sender: TObject);
    procedure SetLines(Value: TStrings);
    procedure SetLineDistance(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetLeftColumnAlignment(Value: TAlignment);
    procedure SetRightColumnAlignment(Value: TAlignment);
    procedure SetActive(Value: Boolean);
    procedure SetInvisible(Value: Boolean);
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
  published
    property Position: Integer read FPosition write SetPosition stored False;
    property Lines: TStrings read FLines write SetLines;
    property LineDistance: Integer read FLineDistance write SetLineDistance;
    property LeftColumnAlignment: TAlignment read FLeftColumnAlignment write SetLeftColumnAlignment;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property RightColumnAlignment: TAlignment read FRightColumnAlignment write SetRightColumnAlignment;
    property Active: Boolean read FActive write SetActive {stored False};
    property Invisible: Boolean read FInvisible write SetInvisible;
    property OnPaint: TPegtopScrollerPaintEvent read FOnPaint write FOnPaint;
    property OnLayout: TPegtopScrollerLayoutEvent read FOnLayout write FOnLayout;
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

const
  ScrollInterval = 20;

////////////////////////////////////////////////////////////////////////////////
// TPegtopScroller
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopScroller.Create(AOwner: TComponent);
begin
  FActive := True;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChange;
  FLineDistance := 24;
  FPosition := 0;
  FAlignment := taCenter;
  FLeftColumnAlignment := taRightJustify;
  FRightColumnAlignment := taLeftJustify;
  FOnPaint := NIL;
  FOnLayout := NIL;
  inherited;
  TabStop := False;
  Width := 129;
  Height := 129;
  // Color := clBlack;
  // Font.Color := clWhite;
end;

destructor TPegtopScroller.Destroy;
begin
  inherited;
  FLines.Free;
end;

procedure TPegtopScroller.WMTimer(var Msg: TWMTimer);
begin
  if IsWindowVisible(Handle) then ScrollBy(1);
  SetTimer(Handle, 0, ScrollInterval, NIL);
  inherited;
end;

procedure TPegtopScroller.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TPegtopScroller.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then Start;
end;

procedure TPegtopScroller.DestroyWnd;
begin
  KillTimer(Handle, 0);
  inherited;
end;

procedure TPegtopScroller.Paint;
var
  TempBitmap: TBitmap;
  I: Integer;
  S, S1, S2: String;
  P: Integer;
  X, Y: Integer;
  A: TAlignment;
begin
  TempBitmap := TBitmap.Create;
  try
    // size bitmap:
    TempBitmap.Width := Canvas.ClipRect.Right - Canvas.ClipRect.Left;
    TempBitmap.Height := Canvas.ClipRect.Bottom - Canvas.ClipRect.Top;

    // paint background:
    TempBitmap.Canvas.Brush.Style := bsSolid;
    TempBitmap.Canvas.Brush.Color := Color;
    TempBitmap.Canvas.FillRect(Rect(0, 0, TempBitmap.Width, TempBitmap.Height));

    // paint user drawings:
    if Assigned(FOnPaint) then FOnPaint(Self, TEmpBitmap.Canvas, Canvas.ClipRect.Left, FPosition + Canvas.ClipRect.Top);

    // paint text:
    if not FInvisible then begin
      TempBitmap.Canvas.Brush.Style := bsClear;
      TempBitmap.Canvas.Font.Assign(Font);
      I := (FPosition + Canvas.ClipRect.Top - FLineDistance) div FLineDistance;
      if (I < 0) then I := 0;
      X := - Canvas.ClipRect.Left;
      Y := FLineDistance * I - FPosition - Canvas.ClipRect.Top;
      while (I < FLines.Count) and (Y < TempBitmap.Height) do begin
        S := FLines[I];
        P := Pos('|', S);
        if P > 0 then begin // two columns
          S1 := Copy(S, 1, P - 1);
          S2 := Copy(S, P + 1, Length(S) - P);
          // left column:
          A := FLeftColumnAlignment;
          if Assigned(FOnLayout) then FOnLayout(Self, I, pscLeft, S1, TempBitmap.Canvas.Font, A);
          if A = taLeftJustify then begin
            TempBitmap.Canvas.TextOut(X + 4, Y, S1);
          end
          else if A = taRightJustify then begin
            TempBitmap.Canvas.TextOut(X + ClientWidth div 2 - 6 - TempBitmap.Canvas.TextWidth(S1), Y, S1);
          end
          else if A = taCenter then begin
            TempBitmap.Canvas.TextOut(X + (ClientWidth div 2 - TempBitmap.Canvas.TextWidth(S1)) div 2, Y, S1);
          end;
          if Assigned(FOnLayout) then TempBitmap.Canvas.Font.Assign(Font);
          // right column:
          A := FRightColumnAlignment;
          if Assigned(FOnLayout) then FOnLayout(Self, I, pscRight, S2, TempBitmap.Canvas.Font, A);
          if A = taLeftJustify then begin
            TempBitmap.Canvas.TextOut(X + ClientWidth div 2 + 6, Y, S2);
          end
          else if A = taRightJustify then begin
            TempBitmap.Canvas.TextOut(X + ClientWidth - 4 - TempBitmap.Canvas.TextWidth(S2), Y, S2);
          end
          else if A = taCenter then begin
            TempBitmap.Canvas.TextOut(X + ClientWidth div 2 + (ClientWidth div 2 - TempBitmap.Canvas.TextWidth(S2)) div 2, Y, S2);
          end;
          if Assigned(FOnLayout) then TempBitmap.Canvas.Font.Assign(Font);
        end
        else begin // single column
          A := FAlignment;
          if Assigned(FOnLayout) then FOnLayout(Self, I, pscSingle, S, TempBitmap.Canvas.Font, A);
          if A = taLeftJustify then begin
            TempBitmap.Canvas.TextOut(X + 4, Y, S);
          end
          else if A = taRightJustify then begin
            TempBitmap.Canvas.TextOut(X + ClientWidth - 4 - TempBitmap.Canvas.TextWidth(S), Y, S);
          end
          else if A = taCenter then begin
            TempBitmap.Canvas.TextOut(X + (ClientWidth - TempBitmap.Canvas.TextWidth(S)) div 2, Y, S);
          end;
          if Assigned(FOnLayout) then TempBitmap.Canvas.Font.Assign(Font);
        end;
        Inc(I);
        Inc(Y, FLineDistance);
      end;
    end;

    // show paintings:
    Canvas.Draw(Canvas.ClipRect.Left, Canvas.ClipRect.Top, TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

procedure TPegtopScroller.ScrollBy(DeltaY: Integer);
begin
  Inc(FPosition, DeltaY);
  if DeltaY <> 0 then begin
    if FPosition > FLines.Count * FLineDistance then begin
      FPosition := -ClientHeight;
      Invalidate;
    end
    else begin
      ScrollWindowEx(Handle, 0, -DeltaY, NIL, NIL, 0, NIL, SW_INVALIDATE);
    end;
  end;
end;

procedure TPegtopScroller.Start;
begin
  FPosition := -ClientHeight;
  FActive := True;
  Invalidate;
  if not (csDesigning in ComponentState) then SetTimer(Handle, 0, ScrollInterval, NIL);
end;

procedure TPegtopScroller.LinesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TPegtopScroller.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TPEgtopScroller.SetPosition(Value: Integer);
begin
  ScrollBy(Value - FPosition);
end;

procedure TPegtopScroller.SetLineDistance(Value: Integer);
begin
  if Value < 1 then Value := 1 else if Value > 100 then Value := 100;
  if FLineDistance <> Value then begin
    FLineDistance := Value;
    Invalidate;
  end;
end;

procedure TPegtopScroller.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopScroller.SetLeftColumnAlignment(Value: TAlignment);
begin
  if FLeftColumnAlignment <> Value then begin
    FLeftColumnAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopScroller.SetRightColumnAlignment(Value: TAlignment);
begin
  if FRightColumnAlignment <> Value then begin
    FRightColumnAlignment := Value;
    Invalidate;
  end;
end;

procedure TPegtopScroller.SetActive(Value: Boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    if FActive then begin
      if not (csDesigning in ComponentState) then SetTimer(Handle, 0, ScrollInterval, NIL);
    end
    else begin
      if not (csDesigning in ComponentState) then KillTimer(Handle, 0);
    end;
  end;
end;

procedure TPegtopScroller.SetInvisible(Value: Boolean);
begin
  if FInvisible <> Value then begin
    FInvisible := Value;
    Invalidate;
  end;
end;

end.
