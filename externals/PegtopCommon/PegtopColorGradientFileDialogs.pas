////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradientFileDialogs.pas
// Classes:    TPegtopColorGradientOpenDialog, TPegtopColorGradientSaveDialog
// Version:    1.00
// Date:       23 May 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradientOpenDialog and TPegtopColorGradientSaveDialog are file
// dialogs for selecting color gradient files.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradientFileDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls, ExtCtrls, PegtopFileDialogs, PegtopColorGradientLists,
  PegtopColorGradientListBoxes;

type
  TPegtopColorGradientOpenDialog = class(TPegtopCustomExtendedFileDialog)
  private
    FListBox: TPegtopColorGradientListBox;
    FLabel: TLabel;
    function IsFilterStored: Boolean;
    function GetItemIndex: Integer;
  protected
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoAlign; override;
    procedure DoSelectionChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemIndex: Integer read GetItemIndex;
  published
    property Filter stored IsFilterStored;
    property ExtendedSize;
    property ViewStyle;
  end;

  TPegtopColorGradientSaveDialog = class(TPegtopColorGradientOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

implementation

uses
  PegtopVirtualListBoxes;

resourcestring
  PegtopColorGradientFilterDef = 'Color gradient files (*.xgr)|*.xgr|All files (*.*)|*.*';

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientOpenDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopColorGradientOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FListBox := TPegtopColorGradientListBox.Create(Self);
  FListBox.Gradients := TPegtopColorGradientList.Create(FListBox);
  FListBox.Parent := ExtendedContainer;
  FLabel := TLabel.Create(Self);
  FLabel.AutoSize := False;
  FLabel.Layout := tlCenter;
  FLabel.Parent := ExtendedContainer;
  ExtendedAlignment := peaRight;
  ExtendedSize := 160;
  DefaultExt := 'xgr';
  Filter := PegtopColorGradientFilterDef;
end;

procedure TPegtopColorGradientOpenDialog.DoShow;
begin
  FListBox.Gradients.Items.Clear;
  FLabel.Caption := '';
  inherited;
end;

procedure TPegtopColorGradientOpenDialog.DoClose;
begin
  inherited;
end;

procedure TPegtopColorGradientOpenDialog.DoAlign;
begin
  inherited;
  FListBox.SetBounds(0, 0, FListBox.Parent.ClientWidth, FListBox.Parent.ClientHeight - 20);
  FLabel.SetBounds(0, FListBox.Parent.ClientHeight - 20, FListBox.Parent.ClientWidth, 20);
end;

procedure TPegtopColorGradientOpenDialog.DoSelectionChange;
var
  Valid: Boolean;
begin
  Valid := FileExists(FileName) and (GetFileAttributes(PChar(FileName)) <> $FFFFFFFF);
  if Valid then begin
    try
      FListBox.Gradients.Items.LoadFromFile(FileName);
      if FListBox.Gradients.Items.Count > 0 then begin
        FListBox.SelectItem(0);
      end;
      if FListBox.Gradients.Items.Count = 1 then
        FLabel.Caption := '1 color gradient'
      else
        FLabel.Caption := IntToStr(FListBox.Gradients.Items.Count) + ' color gradients';
    except
      FListBox.Gradients.Items.Clear;
      FLabel.Caption := '';
      Valid := False;
    end;
  end;
  inherited;
end;

function TPegtopColorGradientOpenDialog.IsFilterStored: Boolean;
begin
  Result := Filter <> PegtopColorGradientFilterDef;
end;

function TPegtopColorGradientOpenDialog.GetItemIndex: Integer;
begin
  Result := FListBox.ItemIndex;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
