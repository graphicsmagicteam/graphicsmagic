unit PegtopCommonReg;

interface

{$INCLUDE PegtopDelphiVersions.inc}

procedure Register;

implementation

uses
  Classes,
{$IFDEF LE_DELPHI5} // <= D5
  DsgnIntf,
{$ELSE} // > D5
  DesignIntf, DesignWindows, DesignEditors,
{$ENDIF}
  PegtopAlarmSchedules, PegtopBroadcasts, PegtopColorControls,
  PegtopColorDialogs, PegtopControlEdits,
  PegtopDesktopMagnifiers, PegtopFileDialogs, PegtopFireButtons, PegtopFormMagnets,
  PegtopGraphicFileDialogs, PegtopLinks, PegtopMessageReceivers, PegtopNumEdits,
  PegtopPanels, PegtopPasswordEdits, PegtopProgressBars, PegtopRadioGroups,
  PegtopScrollers, PegtopSystemImages, PegtopTrackBars, PegtopWindowHooks,
  PegtopNetworkTimeSockets, PegtopComboBoxes, PegtopTextFileDialogs,
  PegtopWaveFileDialogs, PegtopStatusBars, PegtopThumbnails, PegtopQualityLabels,
  PegtopColorGradientDialogs, PegtopColorGradientBars, PegtopCheckBoxes,
  PegtopColorGradientEditors, PegtopColorGradients, PegtopColorGradientLists,
  PegtopFileLabels, PegtopColorGradientControls, PegtopLineLabels,
  PegtopVirtualListBoxes, PegtopLogListBoxes, PegtopColorGradientListBoxes,
  PegtopColorGradientFileDialogs;

const
  PegtopControls = 'Pegtop Controls';
  PegtopColor = 'Pegtop Color';
  PegtopSystem = 'Pegtop System';
  PegtopDialogs = 'Pegtop Dialogs';

procedure Register;
begin
  RegisterComponents(PegtopControls, [TPegtopButtonEdit, TPegtopItemSwitch]);
  RegisterComponents(PegtopControls, [TPegtopDesktopMagnifier]);
  RegisterComponents(PegtopControls, [TPegtopFireButton]);
  RegisterComponents(PegtopControls, [TPegtopLink, TPegtopWebLink]);
  RegisterComponents(PegtopControls, [TPegtopIntEdit, TPegtopFloatEdit]);
  RegisterComponents(PegtopControls, [TPegtopCheckBox]);
  RegisterComponents(PegtopControls, [TPegtopComboBox]);
  RegisterComponents(PegtopControls, [TPegtopPanel, TPegtopGradientPanel]);
  RegisterComponents(PegtopControls, [TPegtopPasswordEdit]);
  RegisterComponents(PegtopControls, [TPegtopProgressBar]);
  RegisterComponents(PegtopControls, [TPegtopRadioGroup]);
  RegisterComponents(PegtopControls, [TPegtopScroller]);
  RegisterComponents(PegtopControls, [TPegtopTrackBar, TPegtopColorTrackBar, TPegtopRangeBar]);
  RegisterComponents(PegtopControls, [TPegtopStatusBar]);
  RegisterComponents(PegtopControls, [TPegtopThumbnail]);
  RegisterComponents(PegtopControls, [TPegtopQualityLabel]);
  RegisterComponents(PegtopControls, [TPegtopFileLabel, TPegtopLineLabel]);
  RegisterComponents(PegtopControls, [TPegtopVirtualListBox]);
  RegisterComponents(PegtopControls, [TPegtopLogListBox]);

  RegisterComponents(PegtopSystem, [TPegtopAlarmSchedule]);
  RegisterComponents(PegtopSystem, [TPegtopBroadcast]);
  RegisterComponents(PegtopSystem, [TPegtopFormMagnet]);
  RegisterComponents(PegtopSystem, [TPegtopMessageReceiver]);
  RegisterComponents(PegtopSystem, [TPegtopSystemImages]);
  RegisterComponents(PegtopSystem, [TPegtopWindowHook, TPegtopFormHook]);
  RegisterComponents(PegtopSystem, [TPegtopNetworkTimeSocket]);

  RegisterComponents(PegtopDialogs, [TPegtopOpenDialog, TPegtopSaveDialog, TPegtopExtendedOpenDialog, TPegtopExtendedSaveDialog]);
  RegisterComponents(PegtopDialogs, [TPegtopGraphicOpenDialog, TPegtopGraphicSaveDialog]);
  RegisterComponents(PegtopDialogs, [TPegtopTextOpenDialog, TPegtopTextSaveDialog]);
  RegisterComponents(PegtopDialogs, [TPegtopWaveOpenDialog, TPegtopWaveSaveDialog]);

  RegisterComponents(PegtopColor, [TPegtopColorBox, TPegtopColorGradientBox]);
  RegisterComponents(PegtopColor, [TPegtopColorGradientBar, TPegtopOpacityGradientBar]);
  RegisterComponents(PegtopColor, [TPegtopColorDialog, TPegtopColorGradientDialog]);
  RegisterComponents(PegtopColor, [TPegtopColorGradientOpenDialog, TPegtopColorGradientSaveDialog]);
  RegisterComponents(PegtopColor, [TPegtopColorGradientList, TPegtopColorGradientLibrary]);
  RegisterComponents(PegtopColor, [TPegtopColorGradientListBox, TPegtopColorGradientLibraryBox, TPegtopColorGradientManager]);

  RegisterComponentEditor(TPegtopColorGradientDialog, TPegtopColorGradientDialogEditor);
  RegisterComponentEditor(TPegtopColorGradientBox, TPegtopColorGradientBoxEditor);
  RegisterComponentEditor(TPegtopColorGradientList, TPegtopColorGradientListEditor);
  RegisterComponentEditor(TPegtopColorGradientLibrary, TPegtopColorGradientLibraryEditor);

  RegisterPropertyEditor(TypeInfo(TPegtopCustomColorGradient), NIL, '', TPegtopColorGradientProperty);
end;

end.
