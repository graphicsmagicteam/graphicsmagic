////////////////////////////////////////////////////////////////////////////////
// File:       PegtopThumbnails.pas
// Classes:    TPegtopThumbnail
// Version:    1.01
// Date:       16 Mar 2005 1.00
//             30 Sep 2005 1.01 (graphic progress support added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopThumbnail holds a thumbnail of a TGraphic.
// Supports 2x and 4x antialiasing and fast jpeg decoding.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopThumbnails;

interface

uses
  Windows, Classes, Controls, Graphics, JPeg;

type
  TPegtopThumbnailOption = (ptoConstrainRatio, ptoEnlargeSmall);
  TPegtopThumbnailOptions = set of TPegtopThumbnailOption;
  TPegtopThumbnailQuality = (ptqNearestPixel, ptqResampling2x, ptqResampling4x);
  TPegtopThumbnailBorder = (ptbNone, ptbImage, ptbControl);

  TPegtopThumbnail = class(TGraphicControl)
  private
    FBitmap: TBitmap;
    FOptions: TPegtopThumbnailOptions;
    FQuality: TPegtopThumbnailQuality;
    FBorder: TPegtopThumbnailBorder;
    FPadding: Integer;
    FOnPaint: TNotifyEvent;
    FImageRect: TRect;
    FProgressPercent: Byte;
    procedure GraphicProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure PrepareGraphic(Graphic: TGraphic; W, H: Integer);
    procedure GetOptimalSize(SourceW, SourceH: Integer; out W, H: Integer);
    function GetGraphic: TGraphic;
    procedure SetGraphic(Value: TGraphic);
    procedure SetOptions(Value: TPegtopThumbnailOptions);
    procedure SetQuality(Value: TPegtopThumbnailQuality);
    procedure SetBorder(Value: TPegtopThumbnailBorder);
    procedure SetPadding(Value: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
    procedure Draw(const SourceRect: TRect; const SourceCanvas: TCanvas);
    property ImageRect: TRect read FImageRect;
    property Canvas;
  published
    property Graphic: TGraphic read GetGraphic write SetGraphic;
    property Options: TPegtopThumbnailOptions read FOptions write SetOptions;
    property Quality: TPegtopThumbnailQuality read FQuality write SetQuality default ptqNearestPixel;
    property Border: TPegtopThumbnailBorder read FBorder write SetBorder default ptbImage;
    property Padding: Integer read FPadding write SetPadding;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  PegtopCPUUtils, PegtopResampleUtils;

const
  AAFactors: array[TPegtopThumbnailQuality] of Integer = (1, 2, 4);

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopThumbnail
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopThumbnail.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FBitmap := TBitmap.Create;
  FOptions := [ptoConstrainRatio];
  FQuality := ptqNearestPixel;
  FBorder := ptbImage;
end;

destructor TPegtopThumbnail.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TPegtopThumbnail.GraphicProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Sender is TGraphic) and (Stage = psRunning) and RedrawNow and (PercentDone >= FProgressPercent + 10) then begin
    FProgressPercent := PercentDone;
    Graphic := TGraphic(Sender);
    Update;
  end;
end;

procedure TPegtopThumbnail.LoadFromFile(const FileName: String);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    FProgressPercent := 0;
    Picture.OnProgress := GraphicProgress;
    Picture.LoadFromFile(FileName);
    Graphic := Picture.Graphic;
  finally
    Picture.Free;
  end;
end;

procedure TPegtopThumbnail.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.LoadFromClipboardFormat(AFormat, AData, APalette);
    Graphic := Picture.Graphic;
  finally
    Picture.Free;
  end;
end;

procedure TPegtopThumbnail.Paint;
var
  X, Y: Integer;
  W, H: Integer;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  if (FBitmap.Empty) or (FBitmap.Width = 0) or (FBitmap.Height = 0) then begin
    Canvas.FillRect(ClientRect);
    FImageRect := Rect(0, 0, 0, 0);
  end
  else begin
    GetOptimalSize(FBitmap.Width, FBitmap.Height, W, H);
    X := (ClientWidth - W) div 2;
    Y := (ClientHeight - H) div 2;
    FImageRect := Bounds(X, Y, W, H);
    if FBorder = ptbImage then begin
      Canvas.FillRect(Rect(0, 0, ClientWidth, Y - 1));
      Canvas.FillRect(Rect(0, Y - 1, X - 1, Y + H + 1));
      Canvas.FillRect(Rect(X + W + 1, Y - 1, ClientWidth, Y + H + 1));
      Canvas.FillRect(Rect(0, Y + H + 1, ClientWidth, ClientHeight));
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(Bounds(X - 1, Y - 1, W + 2, H + 2));
    end
    else if FBorder = ptbControl then begin
      Canvas.FillRect(Rect(0, 0, ClientWidth, Y - 1));
      Canvas.FillRect(Rect(0, Y - 1, X - 1, Y + H + 1));
      Canvas.FillRect(Rect(X + W + 1, Y - 1, ClientWidth, Y + H + 1));
      Canvas.FillRect(Rect(0, Y + H + 1, ClientWidth, ClientHeight));
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(ClientRect);
    end
    else begin
      Canvas.FillRect(Rect(0, 0, ClientWidth, Y));
      Canvas.FillRect(Rect(0, Y, X, Y + H));
      Canvas.FillRect(Rect(X + W, Y, ClientWidth, Y + H));
      Canvas.FillRect(Rect(0, Y + H, ClientWidth, ClientHeight));
    end;
    Canvas.StretchDraw(FImageRect, FBitmap);
  end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPegtopThumbnail.PrepareGraphic(Graphic: TGraphic; W, H: Integer);
var
  Scale: Integer;
begin
  if Graphic is TJPegImage then begin
    // scale JPeg to speed up drawing
    Scale := Min(Graphic.Width div W, Graphic.Height div H);
    if Scale < 2 then TJPegImage(Graphic).Scale := jsFullSize
    else if Scale < 4 then TJPegImage(Graphic).Scale := jsHalf
    else if Scale < 8 then TJPegImage(Graphic).Scale := jsQuarter
    else TJPegImage(Graphic).Scale := jsEighth;
  end;
end;

procedure TPegtopThumbnail.Draw(const SourceRect: TRect; const SourceCanvas: TCanvas);
var
  SourceW, SourceH: Integer;
  W, H: Integer;
  TempBitmap: TBitmap;
  DestOrigin: Pointer;
  DestPitch: Integer;
  SourceOrigin: Pointer;
  SourcePitch: Integer;
begin
  SourceW := SourceRect.Right - SourceRect.Left;
  SourceH := SourceRect.Bottom - SourceRect.Top;
  if (SourceW <= 0) or (SourceH <= 0) then begin
    FBitmap.Assign(NIL);
  end
  else begin
    GetOptimalSize(SourceW, SourceH, W, H);
    if (FQuality = ptqNearestPixel)
    or ((SourceW <= W) and (SourceH <= H)) // small images don't have to be resampled
    then begin
      FBitmap.PixelFormat := pfDevice;
      FBitmap.Width := W;
      FBitmap.Height := H;
      StretchBlt(FBitmap.Canvas.Handle, 0, 0, W, H,
        SourceCanvas.Handle, SourceRect.Left, SourceRect.Top, SourceW, SourceH,
        SRCCOPY);
    end
    else begin
      FBitmap.PixelFormat := pf32bit;
      FBitmap.Width := W;
      FBitmap.Height := H;
      TempBitmap := TBitmap.Create;
      try
        TempBitmap.PixelFormat := pf32bit;
        TempBitmap.Width := W * AAFactors[FQuality];
        TempBitmap.Height := H * AAFactors[FQuality];
        StretchBlt(TempBitmap.Canvas.Handle, 0, 0, TempBitmap.Width, TempBitmap.Height,
          SourceCanvas.Handle, SourceRect.Left, SourceRect.Top, SourceW, SourceH,
          SRCCOPY);
        DestOrigin := FBitmap.ScanLine[0];
        DestPitch := Integer(FBitmap.ScanLine[1]) - Integer(DestOrigin);
        SourceOrigin := TempBitmap.ScanLine[0];
        SourcePitch := Integer(TempBitmap.ScanLine[1]) - Integer(SourceOrigin);
        if FQuality = ptqResampling4x then
          PegtopResample4x32(DestOrigin, DestPitch, Rect(0, 0, FBitmap.Width, FBitmap.Height), SourceOrigin, SourcePitch, 0, 0)
        else
          PegtopResample2x32(DestOrigin, DestPitch, Rect(0, 0, FBitmap.Width, FBitmap.Height), SourceOrigin, SourcePitch, 0, 0);
      finally
        TempBitmap.Free;
      end;
    end;
  end;
  Invalidate;
end;

procedure TPegtopThumbnail.GetOptimalSize(SourceW, SourceH: Integer; out W, H: Integer);
var
  T: Integer;
begin
  if FBorder = ptbNone then begin
    W := ClientWidth - 2 * FPadding;
    H := ClientHeight - 2 * FPadding;
  end
  else begin
    W := ClientWidth - 2 * FPadding - 2;
    H := ClientHeight - 2 * FPadding - 2;
  end;
  if (not (ptoEnlargeSmall in FOptions)) and (SourceW <= W) and (SourceH <= H) then begin
    W := SourceW;
    H := SourceH;
  end
  else if (ptoConstrainRatio in FOptions) then begin
    T := W * SourceH div SourceW;
    if T < H then H := T
    else W := H * SourceW div SourceH;
  end;
  if W < 1 then W := 1;
  if H < 1 then H := 1;
end;

function TPegtopThumbnail.GetGraphic: TGraphic;
begin
  Result := FBitmap;
end;

procedure TPegtopThumbnail.SetGraphic(Value: TGraphic);
var
  W, H: Integer;
  TempBitmap: TBitmap;
  DestOrigin: Pointer;
  DestPitch: Integer;
  SourceOrigin: Pointer;
  SourcePitch: Integer;
begin
  if Value = NIL then begin
    FBitmap.Assign(NIL);
  end
  else begin
    GetOptimalSize(Value.Width, Value.Height, W, H);
    if (FQuality = ptqNearestPixel)
    or ((Value.Width <= W) and (Value.Height <= H)) // small images don't have to be resampled
    then begin
      FBitmap.PixelFormat := pfDevice;
      FBitmap.Width := W;
      FBitmap.Height := H;
      FBitmap.Canvas.FillRect(Rect(0, 0, W, H)); // clear bitmap (some graphics might have transparent parts)
      PrepareGraphic(Value, W, H);
      FBitmap.Canvas.StretchDraw(Rect(0, 0, W, H), Value);
    end
    else begin
      FBitmap.PixelFormat := pf32bit;
      FBitmap.Width := W;
      FBitmap.Height := H;
      TempBitmap := TBitmap.Create;
      try
        TempBitmap.PixelFormat := pf32bit;
        TempBitmap.Width := W * AAFactors[FQuality];
        TempBitmap.Height := H * AAFactors[FQuality];
        FBitmap.Canvas.FillRect(Rect(0, 0, TempBitmap.Width, TempBitmap.Height)); // clear bitmap (some graphics might have transparent parts)
        PrepareGraphic(Value, TempBitmap.Width, TempBitmap.Height);
        TempBitmap.Canvas.StretchDraw(Rect(0, 0, TempBitmap.Width, TempBitmap.Height), Value);
        DestOrigin := FBitmap.ScanLine[0];
        DestPitch := Integer(FBitmap.ScanLine[1]) - Integer(DestOrigin);
        SourceOrigin := TempBitmap.ScanLine[0];
        SourcePitch := Integer(TempBitmap.ScanLine[1]) - Integer(SourceOrigin);
        if FQuality = ptqResampling4x then
          PegtopResample4x32(DestOrigin, DestPitch, Rect(0, 0, FBitmap.Width, FBitmap.Height), SourceOrigin, SourcePitch, 0, 0)
        else
          PegtopResample2x32(DestOrigin, DestPitch, Rect(0, 0, FBitmap.Width, FBitmap.Height), SourceOrigin, SourcePitch, 0, 0);
      finally
        TempBitmap.Free;
      end;
    end;
  end;
  Invalidate;
end;

procedure TPegtopThumbnail.SetOptions(Value: TPegtopThumbnailOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TPegtopThumbnail.SetQuality(Value: TPegtopThumbnailQuality);
begin
  if FQuality <> Value then begin
    FQuality := Value;
    Invalidate;
  end;
end;

procedure TPegtopThumbnail.SetBorder(Value: TPegtopThumbnailBorder);
begin
  if FBorder <> Value then begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TPegtopThumbnail.SetPadding(Value: Integer);
begin
  if Value < 0 then Value := 0
  else if Value > 10 then Value := 10;
  if FPadding <> Value then begin
    FPadding := Value;
    Invalidate;
  end;
end;

end.
