////////////////////////////////////////////////////////////////////////////////
// File:       PegtopColorGradientEditors.pas
// Version:    1.00
// Date:       17 Apr 2005 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopColorGradientDialogEditor and TPegtopColorGradientPropertyEditor
// are design time editors for color gradients.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopColorGradientEditors;

interface

{$INCLUDE PegtopDelphiVersions.inc}

uses
{$IFDEF LE_DELPHI5} // <= D5
  DsgnIntf
{$ELSE} // > D5
  DesignIntf, DesignWindows, DesignEditors
{$ENDIF}
  ;

type
  TPegtopColorGradientDialogEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TPegtopColorGradientBoxEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TPegtopColorGradientProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TPegtopColorGradientListEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TPegtopColorGradientLibraryEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  PegtopColorGradients, PegtopColorGradientDialogs, PegtopColorGradientControls,
  PegtopColorGradientLists, Dialogs, PegtopColorGradientFileDialogs;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientDialogEditor
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TPegtopColorGradientDialogEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := '&Edit gradient...';
  end;
end;

procedure TPegtopColorGradientDialogEditor.ExecuteVerb(Index: Integer);
var
  Dialog: TPegtopColorGradientDialog;
begin
  case Index of
    0: begin
      Dialog := TPegtopColorGradientDialog.Create(NIL);
      try
        Dialog.GradientHook := TPegtopColorGradientDialog(Component).Gradient;
        if Dialog.Execute then Designer.Modified;
      finally
        Dialog.Free;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientBoxEditor
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientBoxEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TPegtopColorGradientBoxEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := '&Edit gradient...';
  end;
end;

procedure TPegtopColorGradientBoxEditor.ExecuteVerb(Index: Integer);
var
  Dialog: TPegtopColorGradientDialog;
begin
  case Index of
    0: begin
      Dialog := TPegtopColorGradientDialog.Create(NIL);
      try
        Dialog.GradientHook := TPegtopColorGradientBox(Component).Gradient;
        if Dialog.Execute then Designer.Modified;
      finally
        Dialog.Free;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientProperty
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TPegtopColorGradientProperty.Edit;
var
  Dialog: TPegtopColorGradientDialog;
begin
  Dialog := TPegtopColorGradientDialog.Create(NIL);
  try
    Dialog.GradientHook := TPegtopColorGradient(GetOrdValue);
    if Dialog.Execute then begin
      Designer.Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientListEditor
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientListEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TPegtopColorGradientListEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := '&Open from file...';
    1: Result := '&Merge file...';
    2: Result := '&Save to file...';
    3: Result := '&Clear';
  end;
end;

procedure TPegtopColorGradientListEditor.ExecuteVerb(Index: Integer);
var
  Dialog: TPegtopColorGradientOpenDialog;
  I: Integer;
begin
  case Index of
    0: begin
      Dialog := TPegtopColorGradientOpenDialog.Create(NIL);
      try
        Dialog.Options := Dialog.Options
        + [ofPathMustExist, ofFileMustExist]
        - [ofOverwritePrompt, ofAllowMultiSelect];
        if Dialog.Execute then begin
          TPegtopColorGradientList(Component).Items.LoadFromFile(Dialog.FileName);
          Designer.Modified;
        end;
      finally
        Dialog.Free;
      end;
    end;
    1: begin
      Dialog := TPegtopColorGradientOpenDialog.Create(NIL);
      try
        Dialog.Options := Dialog.Options
        + [ofPathMustExist, ofFileMustExist, ofAllowMultiSelect]
        - [ofOverwritePrompt];
        if Dialog.Execute then begin
          for I := 0 to Dialog.Files.Count - 1 do
            TPegtopColorGradientList(Component).Items.AddFromFile(Dialog.Files[I]);
          Designer.Modified;
        end;
      finally
        Dialog.Free;
      end;
    end;
    2: begin
      Dialog := TPegtopColorGradientSaveDialog.Create(NIL);
      try
        Dialog.Options := Dialog.Options
        + [ofOverwritePrompt, ofPathMustExist]
        - [ofAllowMultiSelect];
        if Dialog.Execute then begin
          TPegtopColorGradientList(Component).Items.SaveToFile(Dialog.FileName);
        end;
      finally
        Dialog.Free;
      end;
    end;
    3: begin
      TPegtopColorGradientList(Component).Items.Clear;
      Designer.Modified;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopColorGradientLibraryEditor
////////////////////////////////////////////////////////////////////////////////

function TPegtopColorGradientLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TPegtopColorGradientLibraryEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := '&Edit...';
  end;
end;

procedure TPegtopColorGradientLibraryEditor.ExecuteVerb(Index: Integer);
var
  Dialog: TPegtopColorGradientDialog;
begin
  case Index of
    0: begin
      Dialog := TPegtopColorGradientDialog.Create(NIL);
      try
        Dialog.GradientLibrary := TPegtopColorGradientLibrary(Component);
        if Dialog.Execute then begin
          Designer.Modified;
        end;
      finally
        Dialog.Free;
      end;
    end;
  end;
end;

end.
