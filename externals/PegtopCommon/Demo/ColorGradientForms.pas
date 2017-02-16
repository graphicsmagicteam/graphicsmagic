unit ColorGradientForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,
  PegtopColorGradients, PegtopTrackBars,
  PegtopColorGradientControls, PegtopColorGradientDialogs,
  PegtopColorControls, PegtopPanels, PegtopColorGradientLists;

type
  TColorGradientForm = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPegtopPanel;
    StyleComboBox: TComboBox;
    IterationsTrackBar: TPegtopTrackBar;
    StatusBar1: TStatusBar;
    ReverseCheckBox: TCheckBox;
    SymmetricalCheckBox: TCheckBox;
    DitherCheckBox: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    BlendCheckBox: TCheckBox;
    PegtopColorGradientBox1: TPegtopColorGradientBox;
    Label1: TLabel;
    Label2: TLabel;
    PegtopColorGradientBox2: TPegtopColorGradientBox;
    Label3: TLabel;
    pgtpclrgrdntlst1: TPegtopColorGradientList;
    pgtpclrgrdntlbry1: TPegtopColorGradientLibrary;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PropertyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PegtopColorGradientBox1Change(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

var
  ColorGradientForm: TColorGradientForm;

implementation

{$R *.DFM}

procedure TColorGradientForm.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R: TRect;
begin
  // avoid flickering of the gradient
  // (disadvantage: we need to draw the background of the panel)
  R := Rect(0, 0, Panel1.Width, Panel1.Height);
  Windows.FillRect(Msg.DC, R, HBRUSH(clBtnFace + 1));
  Msg.Result := 1;
end;

procedure TColorGradientForm.PaintBox1Paint(Sender: TObject);
var
  PaintBox: TPaintBox;
  Options: TPegtopColorGradientOptions;
  Tempbitmap: TBitmap;
  Origin: Pointer;
  Pitch: Integer;
begin
  PaintBox := TPaintBox(Sender);

  Options := [];
  if ReverseCheckBox.Checked then Include(Options, pgoReverse);
  if SymmetricalCheckBox.Checked then Include(Options, pgoSymmetrical);

  if not BlendCheckBox.Checked then begin
    // only draw one gradient directly to canvas:
    PegtopColorGradientBox1.Gradient.Draw(TPaintBox(Sender).Canvas, Rect(0, 0, TPaintBox(Sender).Width, TPaintBox(Sender).Height),
      Point(120, 120), Point(TPaintBox(Sender).Width - 121, TPaintBox(Sender).Height - 121),
      TPegtopColorGradientStyle(StyleComboBox.ItemIndex), DitherCheckBox.Checked, Options, IterationsTrackBar.Position);
  end
  else begin
    // draw two gradients efficiently:
    // to avoid flickering draw both to a temporary bitmap first
    Tempbitmap := TBitmap.Create;
    try
      TempBitmap.PixelFormat := pf32bit;
      TempBitmap.Width := PaintBox.Width;
      TempBitmap.Height := PaintBox.Height;
      Origin := TempBitmap.ScanLine[0];
      Pitch := Integer(TempBitmap.ScanLine[1]) - Integer(Origin);
      PegtopColorGradientBox1.Gradient.Draw32(Origin, Pitch,
        Rect(0, 0, TPaintBox(Sender).Width, TPaintBox(Sender).Height),
        Point(120, 120), Point(TPaintBox(Sender).Width - 121, TPaintBox(Sender).Height - 121),
        TPegtopColorGradientStyle(StyleComboBox.ItemIndex), Options, IterationsTrackBar.Position);
      PegtopColorGradientBox2.Gradient.Blend32(Origin, Pitch,
        Rect(0, 0, TempBitmap.Width, TempBitmap.Height),
        Point(120, 120), Point(TPaintBox(Sender).Width - 121, TPaintBox(Sender).Height - 121),
        pgsPolar, [], 2);
      PaintBox.Canvas.Draw(0, 0, TempBitmap);
    finally
      Tempbitmap.Free;
    end;
  end;

  // draw gradient vector:
  TPaintBox(Sender).Canvas.MoveTo(120, 120);
  TPaintBox(Sender).Canvas.LineTo(TPaintBox(Sender).Width - 121, TPaintBox(Sender).Height - 121);
end;

procedure TColorGradientForm.PropertyChange(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TColorGradientForm.FormCreate(Sender: TObject);
begin
  StyleComboBox.ItemIndex := 0;
end;

procedure TColorGradientForm.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  PegtopColorGradientBox1.Gradient.Color.Noise.Strength := 0;
  PegtopColorGradientBox1.Gradient.Color.Keys.Clear;
  for I := 0 to 3 do begin
    with PegtopColorGradientBox1.Gradient.Color.Keys.Add do begin
      Position := Random(1000);
      Color := Random($1000000);
    end;
  end;
  PaintBox1.Invalidate;
end;

procedure TColorGradientForm.Button2Click(Sender: TObject);
var
  C: TColor;
begin
  PegtopColorGradientBox1.Gradient.Color.Noise.Strength := 0;
  C := (Random($E0) shl 16) or (Random($E0) shl 8) or Random($E0);
  PegtopColorGradientBox1.Gradient.Color.Keys.Define([C, C + $1F1F1F]);
  PaintBox1.Invalidate;
end;

procedure TColorGradientForm.PegtopColorGradientBox1Change(
  Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

end.
