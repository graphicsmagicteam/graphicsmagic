////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopPasswordEdit
// Version:    1.00
// Date:       13 Jun 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopPasswordEdit is a TCustomEdit control for passwords. It supports the
// Windows 9x password character (*) as well as the new Windows XP password
// dot (when XP theming is activated for your application)
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopPasswordEdits;

interface

uses
  Windows, Classes, Controls, StdCtrls;

type
  TPegtopPasswordEdit = class(TCustomEdit)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF VER130} // Delphi 5.x
    property Anchors;
    property Constraints;
    {$ENDIF}
  end;

implementation

uses
  Forms;

constructor TPegtopPasswordEdit.Create(AOwner: TComponent);
const
  FavouredFonts: array[0..2] of String = ('Tahoma', 'Arial', 'MS Sans Serif');
var
  I: Integer;
begin
  inherited;
  I := Low(FavouredFonts);
  while (I <= High(FavouredFonts))
  and (Screen.Fonts.IndexOf(FavouredFonts[I]) < 0) do Inc(I);
  if I <= High(FavouredFonts) then Font.Name := FavouredFonts[I];
end;

procedure TPegtopPasswordEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_PASSWORD;
end;

end.
