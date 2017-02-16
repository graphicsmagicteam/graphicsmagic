program SelectionDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  ColorRangeSelectionDlg in 'ColorRangeSelectionDlg.pas' {frmColorRangeSelection};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmColorRangeSelection, frmColorRangeSelection);
  Application.Run;
end.
