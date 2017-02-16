unit FileDialogForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PegtopFileDialogs, StdCtrls, PegtopWaveFileDialogs,
  PegtopTextFileDialogs, PegtopGraphicFileDialogs;

type
  TFileDialogForm = class(TForm)
    Button1: TButton;
    PegtopExtendedSaveDialog1: TPegtopExtendedSaveDialog;
    Button2: TButton;
    Button3: TButton;
    PegtopGraphicOpenDialog1: TPegtopGraphicOpenDialog;
    PegtopTextOpenDialog1: TPegtopTextOpenDialog;
    Button4: TButton;
    PegtopWaveOpenDialog1: TPegtopWaveOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PegtopExtendedSaveDialog1Show(Sender: TObject);
  private
    FExButton: TButton;
    procedure ExButtonClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FileDialogForm: TFileDialogForm;

implementation

uses
  JPeg;

{$R *.DFM}

procedure TFileDialogForm.ExButtonClick(Sender: TObject);
begin
  // change captions:
  PegtopExtendedSaveDialog1.ItemTexts[pfiFolderLabel] := '8-)';
  PegtopExtendedSaveDialog1.ItemTexts[pfiFileNameLabel] := ';-)';
  PegtopExtendedSaveDialog1.ItemTexts[pfiFileTypeLabel] := ':-P';
  PegtopExtendedSaveDialog1.ItemTexts[pfiOkButton] := ':-)';
  PegtopExtendedSaveDialog1.ItemTexts[pfiCancelButton] := ':-(';
  // toggle visibility of folder combo box:
  PegtopExtendedSaveDialog1.ItemVisibilities[pfiFolderComboBox]
    := not PegtopExtendedSaveDialog1.ItemVisibilities[pfiFolderComboBox];
  ShowMessage('Take a look at the labels!');
end;

procedure TFileDialogForm.FormCreate(Sender: TObject);
var
  ExLabel: TLabel;
begin
  ExLabel := TLabel.Create(Self);
  with ExLabel do begin
    WordWrap := True;
    Width := PegtopExtendedSaveDialog1.ExtendedSize;
    AutoSize := True;
    Caption := 'Here is some additional space for any controls you like. You can adjust the extended size and as well as the alignment (right or bottom)';
    Parent := PegtopExtendedSaveDialog1.ExtendedContainer;
  end;
  FExButton := TButton.Create(Self);
  with FExButton do begin
    Top := ExLabel.Top + ExLabel.Height + 8;
    Width := PegtopExtendedSaveDialog1.ExtendedSize;
    Parent := PegtopExtendedSaveDialog1.ExtendedContainer;
    OnClick := ExButtonClick;
  end;
  with TLabel.Create(Self) do begin
    Top := FExButton.Top + FExButton.Height + 8;
    WordWrap := True;
    Width := PegtopExtendedSaveDialog1.ExtendedSize;
    AutoSize := True;
    Caption := 'Note that the file extension is automatically adjusted if you change the filter (if AutoAdjustExtension is True)!';
    Parent := PegtopExtendedSaveDialog1.ExtendedContainer;
  end;
end;

procedure TFileDialogForm.Button1Click(Sender: TObject);
begin
  PegtopExtendedSaveDialog1.FileName := ExtractFilePath(Application.ExeName) + 'Test.txt';
  PegtopExtendedSaveDialog1.Execute;
end;

procedure TFileDialogForm.Button2Click(Sender: TObject);
begin
  PegtopGraphicOpenDialog1.Execute;
end;

procedure TFileDialogForm.Button3Click(Sender: TObject);
begin
  PegtopTextOpenDialog1.Execute;
end;

procedure TFileDialogForm.Button4Click(Sender: TObject);
begin
  PegtopWaveOpenDialog1.Execute;
end;

procedure TFileDialogForm.PegtopExtendedSaveDialog1Show(Sender: TObject);
begin
  PegtopExtendedSaveDialog1.ViewStyle := pvsReport;
  // button captions are lost when reopening the dialog,
  // so set it each time it is shown!
  FExButton.Caption := 'Click me!';
end;

end.
