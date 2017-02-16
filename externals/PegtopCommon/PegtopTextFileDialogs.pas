////////////////////////////////////////////////////////////////////////////////
// File:       PegtopTextFileDialogs.pas
// Classes:    TPegtopTextOpenDialog, TPegtopTextSaveDialog
// Version:    1.00
// Date:       13 Mar 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopGraphicOpenDialog and TPegtopGraphicSaveDialog are file dialogs for
// selecting graphic files. Much like Delphi's TPictureOpen/SaveDialog (which I
// don't like for some reasons, especially because square bitmaps are not
// displayed properly). Supports fast jpeg decoding.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopTextFileDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, PegtopFileDialogs;

type
  TPegtopTextOpenDialog = class(TPegtopCustomExtendedFileDialog)
  private
    FRichEdit: TRichEdit;
    FInfoLabel: TLabel;
    function IsFilterStored: Boolean;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
  protected
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoAlign; override;
    procedure DoSelectionChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font: TFont read GetFont write SetFont;
    property ExtendedSize;
    property ExtendedAlignment;
    property Filter stored IsFilterStored;
  end;

  TPegtopTextSaveDialog = class(TPegtopTextOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

implementation

uses
  Forms;

resourcestring
  PegtopTextFilterDef = 'Text files (*.txt)|*.txt|Rich text files (*.rtf)|*.rtf|All files (*.*)|*.*';

////////////////////////////////////////////////////////////////////////////////
// TPegtopTextOpenDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopTextOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FRichEdit := TRichEdit.Create(Self);
  FRichEdit.ReadOnly := True;
  FRichEdit.ScrollBars := ssBoth;
  FRichEdit.WordWrap := False;
  FRichEdit.Font.Name := 'Courier New';
  FRichEdit.Font.Size := 10;
  FRichEdit.BorderStyle := bsNone;
  FRichEdit.WantReturns := False;
  FRichEdit.WantTabs := False;
  FRichEdit.Parent := ExtendedContainer;
  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.AutoSize := False;
  FInfoLabel.Layout := tlCenter;
  FInfoLabel.Transparent := True;
  FInfoLabel.Parent := ExtendedContainer;
  ExtendedSize := 160;
  DefaultExt := 'txt';
  Filter := PegtopTextFilterDef;
end;

destructor TPegtopTextOpenDialog.Destroy;
begin
  FRichEdit.Free;
  inherited;
end;

procedure TPegtopTextOpenDialog.DoShow;
begin
  // FRichEdit.Lines.Clear; // doesn't seem to work while parent window is not created
  inherited;
end;

procedure TPegtopTextOpenDialog.DoClose;
begin
  FRichEdit.Lines.Clear;
  inherited;
end;

procedure TPegtopTextOpenDialog.DoAlign;
begin
  FRichEdit.SetBounds(0, 0, ExtendedContainer.Width, ExtendedContainer.Height - 20);
  FInfoLabel.SetBounds(0, ExtendedContainer.Height - 16, ExtendedContainer.Width, 16);
  inherited;
end;

procedure TPegtopTextOpenDialog.DoSelectionChange;
begin
  if (FileGetAttr(FileName) and faDirectory) = 0 then begin
    try
      FRichEdit.Lines.LoadFromFile(FileName)
    except
      FRichEdit.Lines.Clear;
    end;
  end
  else begin
    FRichEdit.Lines.Clear;
  end;
  if FRichEdit.Lines.Count > 0 then
    FInfoLabel.Caption := IntToStr(FRichEdit.Lines.Count) + ' lines'
  else
    FInfoLabel.Caption := '';
  inherited;
end;

function TPegtopTextOpenDialog.IsFilterStored: Boolean;
begin
  Result := Filter <> PegtopTextFilterDef;
end;

function TPegtopTextOpenDialog.GetFont: TFont;
begin
  Result := FRichEdit.Font;
end;

procedure TPegtopTextOpenDialog.SetFont(Value: TFont);
begin
  FRichEdit.Font := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopTextSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopTextSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
