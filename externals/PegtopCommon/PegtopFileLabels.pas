////////////////////////////////////////////////////////////////////////////////
// File:       PegtopFileLabels.pas
// Components: TPegtopFileLabel
// Version:    1.00
// Date:       10 May 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopFileLabel is a label for a file / folder path. Supports minimizing the
// path if it is longer than the width of the label, and displaying the icon
// of he file / folder.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopFileLabels;

interface

uses
  Windows, Classes, Controls, StdCtrls, PegtopSystemImages;

type
  TPegtopFileLabelOption = (pflShowIcon, pflMinimizePath);
  TPegtopFileLabelOptions = set of TPegtopFileLabelOption;

  TPegtopFileIconEvent = procedure (Sender: TObject; Images: TPegtopSystemImages; var Index: Integer) of object;

  TPegtopFileLabel = class(TCustomLabel)
  private
    FOptions: TPegtopFileLabelOptions;
    FOnGetIcon: TPegtopFileIconEvent;
    procedure SetOptions(Value: TPegtopFileLabelOptions);
    procedure SetOnGetIcon(Value: TPegtopFileIconEvent);
  protected
    function GetLabelText: String; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TPegtopFileLabelOptions read FOptions write SetOptions;
    property OnGetIcon: TPegtopFileIconEvent read FOnGetIcon write SetOnGetIcon;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
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
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, Graphics, FileCtrl;

constructor TPegtopFileLabel.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  Caption := 'C:\';
  FOptions := [pflShowIcon, pflMinimizePath];
end;

function TPegtopFileLabel.GetLabelText: String;
begin
  Result := MinimizeName(Caption, Canvas, ClientWidth);
end;

procedure TPegtopFileLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
  ImageIndex: Integer;
  OldColor: TColor;
begin
  ImageIndex := -1;
  if pflShowIcon in FOptions then begin
    ImageIndex := SmallSystemImages.GetFileIconIndex(Caption);
    if Assigned(FOnGetIcon) then FOnGetIcon(Self, SmallSystemImages, ImageIndex);
  end;
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    if pflShowIcon in FOptions then Rect.Left := Rect.Left + 18;
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
    { Calculate vertical layout }
    if Layout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;
  if ImageIndex >= 0 then begin
    OldColor := SmallSystemImages.BkColor;
    try
      SmallSystemImages.BkColor := Color;
      SmallSystemImages.Draw(Canvas, 0, 0, ImageIndex);
    finally
      SmallSystemImages.BkColor := OldColor;
    end;
  end;
end;

procedure TPegtopFileLabel.SetOptions(Value: TPegtopFileLabelOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TPegtopFileLabel.SetOnGetIcon(Value: TPegtopFileIconEvent);
begin
  if (TMethod(FOnGetIcon).Code <> TMethod(Value).Code)
  or (TMethod(FOnGetIcon).Data <> TMethod(Value).Data) then begin
    FOnGetIcon := Value;
    Invalidate;
  end;
end;

end.
