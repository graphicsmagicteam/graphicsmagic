////////////////////////////////////////////////////////////////////////////////
// Components: TPegtopButtonEdit, TPegtopItemSwitch
// Version:    1.00
// Date:       05 Jul 2003
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopButtonEdit is an edit control with an additional button inside of it.
// TPegtopItemSwitch is a control to select an item from a list (using two
// buttons to switch between list items). TPegtopItemSwitch basically is a
// edit control, so the selected text can be copied to clipboard.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopControlEdits;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, ComCtrls;

type
  TPegtopContolEdit = class(TCustomEdit)
  private
    FWindowCreated: Boolean;
    FControl: TControl;
    procedure SetControl(Value: TControl);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure AlignControl;
    property InnerControl: TControl read FControl write SetControl;
    procedure SetEnabled(Value: Boolean); override;
    function GetInnerControlEnabled: Boolean;
    procedure SetInnerControlEnabled(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property HideSelection;
    property ReadOnly;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TPegtopButtonEdit = class(TPegtopContolEdit)
  private
    FButton: TButton;
    FOnButtonClick: TNotifyEvent;
    function GetButtonCaption: TCaption;
    procedure SetButtonCaption(Value: TCaption);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    procedure ButtonClick(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ButtonCaption: TCaption read GetButtonCaption write SetButtonCaption;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 24;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ButtonEnabled: Boolean read GetInnerControlEnabled write SetInnerControlEnabled;
  end;

  TPegtopItemSwitchOption = (isEmptyAllowed, isUpsideDown, isHorizontal, isWrap, isScrollWheel);
  TPegtopItemSwitchOptions = set of TPegtopItemSwitchOption;

  TPegtopItemSwitch = class(TPegtopContolEdit)
  private
    FUpDown: TUpDown;
    FItems: TStrings;
    FItemIndex: Integer;
    FOptions: TPegtopItemSwitchOptions;
    procedure ItemsChanged(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure SwitchItem(Delta: Integer);
    function GetMinIndex: Integer;
    function GetUpKey: Word;
    function GetDownKey: Word;
    procedure SetItems(Value: TStrings);
    procedure SetItemIndex(Value: Integer);
    procedure SetOptions(Value: TPegtopItemSwitchOptions);
    procedure MWMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex {stored False};
    property Options: TPegtopItemSwitchOptions read FOptions write SetOptions;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopContolEdit
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopContolEdit.Create(AOwner: TComponent);
begin
  FWindowCreated := False;
  FControl := NIL;
  inherited;
end;

procedure TPegtopContolEdit.Resize;
begin
  inherited;
  AlignControl;
end;

procedure TPegtopContolEdit.CreateWnd;
begin
  inherited;
  FWindowCreated := True;
  AlignControl;
end;

procedure TPegtopContolEdit.AlignControl;
begin
  if FWindowCreated then begin
    Perform(EM_SETMARGINS, EC_RIGHTMARGIN, (FControl.Width + 2) SHL 16);
    FControl.SetBounds(ClientRect.Right - FControl.Width, ClientRect.Top,
      FControl.Width, ClientRect.Bottom - ClientRect.Top);
  end;
end;

procedure TPegtopContolEdit.SetControl(Value: TControl);
begin
  FControl := Value;
  FControl.Parent := Self;
  AlignControl;
end;

procedure TPegtopContolEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  if (FControl is TWinControl) then
    EnableWindow(TWinControl(FControl).Handle, Value and FControl.Enabled);
end;

function TPegtopContolEdit.GetInnerControlEnabled: Boolean;
begin
  Result := FControl.Enabled;
end;

procedure TPegtopContolEdit.SetInnerControlEnabled(Value: Boolean);
begin
  FControl.Enabled := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopButtonEdit
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopButtonEdit.Create(AOwner: TComponent);
begin
  FOnButtonClick := NIL;
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FButton := TButton.Create(Self);
  FButton.Width := 24;
  FButton.Caption := '...';
  FButton.OnClick := ButtonClick;
  InnerControl := FButton;
end;

procedure TPegtopButtonEdit.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
end;

function TPegtopButtonEdit.GetButtonCaption: TCaption;
begin
  Result := FButton.Caption;
end;

procedure TPegtopButtonEdit.SetButtonCaption(Value: TCaption);
begin
  FButton.Caption := Value;
end;

function TPegtopButtonEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TPegtopButtonEdit.SetButtonWidth(Value: Integer);
begin
  FButton.Width := Value;
  AlignControl;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopItemSwitch
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopItemSwitch.Create(AOwner: TComponent);
begin
  FOptions := [isScrollWheel];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FItemIndex := -1;
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FUpDown := TUpDown.Create(Self);
  FUpDown.Min := -1;
  FUpDown.Max := 1;
  FUpDown.Position := 0;
  FUpDown.Orientation := udVertical;
  FUpDown.OnChangingEx := UpDownChangingEx;
  InnerControl := FUpDown;
end;

procedure TPegtopItemSwitch.Loaded;
begin
  inherited;
  ItemIndex := GetMinIndex;
end;

procedure TPegtopItemSwitch.MWMouseWheel(var Msg: TWMMouseWheel);
type
  TWParam = packed record
    Keys: Word;
    ZDelta: SmallInt;
  end;
  TLParam = packed record
    XPos: SmallInt;
    YPos: SmallInt;
  end;
begin
  if isScrollWheel in FOptions then SwitchItem(Msg.WheelDelta div WHEEL_DELTA);
end;

procedure TPegtopItemSwitch.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if (Direction = updUp) then SwitchItem(1)
  else if (Direction = updDown) then SwitchItem(-1);
  AllowChange := False;
end;

procedure TPegtopItemSwitch.SwitchItem(Delta: Integer);
var
  NewIndex: Integer;
begin
  if isUpsideDown in FOptions then
    NewIndex := ItemIndex - Delta
  else
    NewIndex := ItemIndex + Delta;
  if isWrap in FOptions then begin
    if NewIndex < GetMinIndex then NewIndex := FItems.Count - 1
    else if NewIndex > (FItems.Count - 1) then NewIndex := GetMinIndex;
  end
  else begin
    if NewIndex < GetMinIndex then NewIndex := GetMinIndex;
  end;
  ItemIndex := NewIndex;
end;

function TPegtopItemSwitch.GetMinIndex: Integer;
begin
  if (isEmptyAllowed in FOptions) or (FItems.Count < 1) then Result := -1
  else Result := 0;
end;

function TPegtopItemSwitch.GetUpKey: Word;
begin
  if isHorizontal in FOptions then Result := VK_RIGHT
  else Result := VK_UP;
end;

function TPegtopItemSwitch.GetDownKey: Word;
begin
  if isHorizontal in FOptions then Result := VK_LEFT
  else Result := VK_DOWN;
end;

procedure TPegtopItemSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = GetUpKey then begin
    SwitchItem(1);
    Key := 0;
  end
  else if Key = GetDownKey then begin
    SwitchItem(-1);
    Key := 0;
  end
  else begin
    inherited;
  end;
end;

procedure TPegtopItemSwitch.KeyPress(var Key: Char);
begin
  Key := #0;
end;

procedure TPegtopItemSwitch.ItemsChanged(Sender: TObject);
begin
  ItemIndex := ItemIndex; // validate
end;

procedure TPegtopItemSwitch.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  ItemIndex := GetMinIndex;
end;

procedure TPegtopItemSwitch.SetItemIndex(Value: Integer);
begin
  if Value < -1 then Value := -1 else if Value > FItems.Count-1 then Value := FItems.Count-1;
  FItemIndex := Value;
  if FItemIndex >= 0 then Text := FItems[FItemIndex] else Text := '';
end;

procedure TPegtopItemSwitch.SetOptions(Value: TPegtopItemSwitchOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    if isHorizontal in FOptions then begin
      FUpDown.Orientation := udHorizontal;
      FUpDown.Width := 24;
    end
    else begin
      FUpDown.Orientation := udVertical;
    end;
    AlignControl;
  end;
end;

end.

