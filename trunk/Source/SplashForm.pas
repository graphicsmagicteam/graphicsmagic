unit SplashForm;

{ The semi-transparent splash form is made by
  x2nie - Fathony Luthfillah  <x2nie@yahoo.com>

  The code is based on a topic written by Anders Melander:
  http://melander.dk/articles/alphasplash/1/

  Anders's original code is released under
  "Creative Commons Attribution-Share Alike 3.0 Unported License":
  http://creativecommons.org/licenses/by-sa/3.0/

  x2nie adapted the original code to make it use of GR32. }

// Update Date: 2017/01/23

{$DEFINE EX_LAYERED}
{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GR32_Image;

type
  TfrmSplash = class(TForm)
    imgSplash: TImage32;
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgSplashClick(Sender: TObject);
  private
    FBlendFunction: TBlendFunction;
    FBitmapPos    : TPoint;
    FBitmapSize   : TSize;
    FExStyle      : DWORD;
    FBitmap       : TBitmap;
    FClickEnabled : Boolean;

    procedure WMNCHitTest(var Message: TWMNCHitTest);
  public
    property ClickEnabled: Boolean read FClickEnabled write FClickEnabled;
  end;

var
  frmSplash: TfrmSplash;

implementation

uses
  GR32;

{$R *.DFM}

var
  PreMult: array[Byte, Byte] of Byte;

procedure BuildPremultiply;
var
  Row, Col : Integer;
begin
  // precalculate all possible values of a*b
  for Row := 0 to 255 do
    for Col := Row to 255 do
    begin
      PreMult[Row, Col] := Row*Col div 255;
      
      if (Row <> Col) then
      begin
        PreMult[Col, Row] := PreMult[Row, Col]; // a*b = b*a
      end;
    end;
end;

procedure PremultiplyBitmap(ABitmap: TBitmap);
var
  Row, Col : Integer;
  p        : PRGBQuad;
begin
  for Row := 0 to (ABitmap.Height - 1) do
  begin
    Col := ABitmap.Width;
    p   := ABitmap.ScanLine[Row];
    
    while (Col > 0) do
    begin
      p.rgbBlue := PreMult[p.rgbReserved, p.rgbBlue];
      p.rgbGreen := PreMult[p.rgbReserved, p.rgbGreen];
      p.rgbRed := PreMult[p.rgbReserved, p.rgbRed];
      inc(p);
      dec(Col);
    end;
  end;
end;

procedure TfrmSplash.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTCAPTION;
end;

procedure TfrmSplash.FormDeactivate(Sender: TObject);
begin
  Close();
end;

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
{$IFDEF EX_LAYERED}
  // Enable window layering
  FExStyle := GetWindowLongA(Handle, GWL_EXSTYLE);

  if (FExStyle and WS_EX_LAYERED = 0) then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE, FExStyle or WS_EX_LAYERED);
  end;
{$ENDIF}

  FBitmap := TBitmap.Create;
  FBitmap.Assign(imgSplash.Bitmap);

  ASSERT(FBitmap.PixelFormat = pf32bit, 'Wrong bitmap format - must be 32 bits/pixel');
  imgSplash.Buffer.DrawMode := dmBlend;
  imgSplash.Bitmap.Assign(FBitmap);

  imgSplash.Width  := FBitmap.Width;
  imgSplash.Height := FBitmap.Height;

{$IFDEF EX_LAYERED}
  // Position bitmap on form
  FBitmapPos     := Point(0, 0);
  FBitmapSize.cx := FBitmap.Width;
  FBitmapSize.cy := FBitmap.Height;

  // Setup alpha blending parameters
  FBlendFunction.BlendOp             := AC_SRC_OVER;
  FBlendFunction.BlendFlags          := 0;
  FBlendFunction.SourceConstantAlpha := 255; // Start completely transparent
  FBlendFunction.AlphaFormat         := AC_SRC_ALPHA;
{$ENDIF}

  FClickEnabled := True;
end;

procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TfrmSplash.FormShow(Sender: TObject);
begin
{$IFDEF EX_LAYERED}
  imgSplash.Bitmap.DrawTo(FBitmap.Canvas.Handle,0,0);
  PremultiplyBitmap(FBitmap);
  
  UpdateLayeredWindow(Handle, 0000, nil, @FBitmapSize,
                      FBitmap.Canvas.Handle,
                      @FBitmapPos, 0, @FBlendFunction,
                      ULW_ALPHA);
{$ENDIF}
end;

procedure TfrmSplash.imgSplashClick(Sender: TObject);
begin
  if FClickEnabled then
  begin
    Close();
  end;
end;

initialization
  BuildPremultiply();

end.
