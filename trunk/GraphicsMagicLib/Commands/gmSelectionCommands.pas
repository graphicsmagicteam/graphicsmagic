unit gmSelectionCommands;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

// Update Date: 2017/01/17

interface

uses
{ Delphi }
  Windows,
  SysUtils,
  Classes,
{ Graphics32 }
  GR32,
  GR32_OrdinalMaps,
{ GraphicsMagicLib }
  gmChannelManager,
  gmHistoryCommands,
  gmLayers,
  gmSelection,
  gmTypes;      // TgmChannelSet


type
  TgmSelectionStatus = record
     CornerStretched     : Boolean;
     FlippedHorizontally : Boolean;
     FlippedVertically   : Boolean;
     ForeAlphaChanged    : Boolean;
     Transformed         : Boolean;
     Translated          : Boolean;
  end;

  TgmSelectionAdjustmentType = (satTranslate,
                                satStretchCorner,
                                satInverse,
                                satFeather,
                                satNudgeOutline,
                                satFlipHorizontally,
                                satFlipVertically);

  TgmExitTransformModeChoices = (etmcApplyTransform, etmcCancelTransform);

  TgmGetExternalSelectionFunc = function(const ACreateIfNone: Boolean): TgmSelection of object;
  TgmDeleteExternalSelectionProc = procedure() of object;

  // Such type of functions should enter into the specified transform mode,
  // and return the selection transform object back. Return nil of the
  // selection is not existed. If the ACreateIfNone is false, return the
  // external selection transform object even if it was in nil.
  TgmEnterSelectionTransformModeFunc = function(const ATransformMode: TgmTransformMode;
    const ACreateIfNone: Boolean): TgmSelectionTransformation of object;
    
  TgmExitSelectionTransformModeProc = procedure() of object;
  TgmGetExternalSelectionTransformationFunc = function(): TgmSelectionTransformation of object;


  { TgmNewSelectionOnLayerCommand }

  TgmNewSelectionOnLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FOldSelection   : TgmSelection;             // a copy to an old selection, if any
    FNewSelection   : TgmSelection;             // a copy to the new created selection
    FLayerIndex     : Integer;                  // the selected layer index when the new selection is creating

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
      AOldSelection, ANewSelection: TgmSelection;
      const ALayerIndex: Integer;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewSelectionOnLayerMaskCommand }

  TgmNewSelectionOnLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FOldSelection   : TgmSelection;             // a copy to an old selection, if any
    FNewSelection   : TgmSelection;             // a copy to the new created selection
    FLayerIndex     : Integer;                  // the selected layer index when the new selection is creating

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
      AOldSelection, ANewSelection: TgmSelection;
      const ALayerIndex: Integer;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewSelectionOnAlphaChannelCommand }

  TgmNewSelectionOnAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection      : TgmSelection;             // a copy to an old selection, if any
    FNewSelection      : TgmSelection;             // a copy to the new created selection
    FAlphaChannelIndex : Integer;                  // the index of the selected alpha channel on which the selection will be created

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; 
      AOldSelection, ANewSelection: TgmSelection;
      const AAlphaChannelIndex: Integer;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;
  

  { TgmSelectionPixelProcessOnLayerCommand }

  TgmSelectionPixelProcessOnLayerCommand = class(TgmImageProcessCommand)
  private
    FChannelManager        : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList             : TgmLayerList;            // pointer to an external layer list
    FLayerIndex            : Integer;                 // layer the selection is processing on
    FChannelSet            : TgmChannelSet;
    FSourceBmpFileName     : string;
    FBackgroundBmpFileName : string;
    FSelectionStatus       : TgmSelectionStatus;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList;
      const ACurrentLayerIndex: Integer;
      const AUndoBmp, ARedoBmp: TBitmap32;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionPixelProcessOnLayerMaskCommand }

  TgmSelectionPixelProcessOnLayerMaskCommand = class(TgmByteMapProcessCommand)
  private
    FChannelManager        : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList             : TgmLayerList;            // pointer to an external layer list
    FLayerIndex            : Integer;                 // layer the selection is processing on
    FSourceMapFileName     : string;
    FBackgroundMapFileName : string;
    FBackgroundWidth       : Integer;
    FBackgroundHeight      : Integer;
    FSelectionStatus       : TgmSelectionStatus;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList;
      const ALayerIndex: Integer;
      const AUndoBmp, ARedoBmp: TBitmap32;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionPixelProcessOnAlphaChannelCommand }

  TgmSelectionPixelProcessOnAlphaChannelCommand = class(TgmByteMapProcessCommand)
  private
    FChannelManager        : TgmCustomChannelManager; // pointer to an external channel manager
    FAlphaChannelIndex     : Integer;                 // alpha channel the selection is processing on
    FSourceMapFileName     : string;
    FBackgroundMapFileName : string;
    FBackgroundWidth       : Integer;
    FBackgroundHeight      : Integer;
    FSelectionStatus       : TgmSelectionStatus;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer;
      const AUndoBmp, ARedoBmp: TBitmap32;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionAdjustmentCommand }

  TgmSelectionAdjustmentCommand = class(TgmCustomCommand)
  protected
    function GetCommandNameByAdjustmentType(
      const AType: TgmSelectionAdjustmentType): string;
  end;

  { TgmSelectionAdjustmentOnLayerCommand }

  TgmSelectionAdjustmentOnLayerCommand = class(TgmSelectionAdjustmentCommand)
  private
    FAdjustmentType : TgmSelectionAdjustmentType;
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FChannelSet     : TgmChannelSet;
    FLayerList      : TgmLayerList;            // pointer to an external layer list
    FLayerIndex     : Integer;                 // layer the selection is processing on
    FOldSelection   : TgmSelection;
    FNewSelection   : TgmSelection;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const AAdjustmentType: TgmSelectionAdjustmentType;
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionAdjustmentOnLayerMaskCommand }

  TgmSelectionAdjustmentOnLayerMaskCommand = class(TgmSelectionAdjustmentCommand)
  private
    FAdjustmentType : TgmSelectionAdjustmentType;
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FLayerList      : TgmLayerList;            // pointer to an external layer list
    FLayerIndex     : Integer;                 // layer the selection is processing on
    FOldSelection   : TgmSelection;
    FNewSelection   : TgmSelection;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const AAdjustmentType: TgmSelectionAdjustmentType;
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionAdjustmentOnAlphaChannelCommand }

  TgmSelectionAdjustmentOnAlphaChannelCommand = class(TgmSelectionAdjustmentCommand)
  private
    FAdjustmentType    : TgmSelectionAdjustmentType;
    FChannelManager    : TgmCustomChannelManager; // pointer to an external channel manager
    FAlphaChannelIndex : Integer;                 // alpha channel the selection is processing on
    FOldSelection      : TgmSelection;
    FNewSelection      : TgmSelection;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const AAdjustmentType: TgmSelectionAdjustmentType;
      AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCommitSelectionOnLayerCommand }

  TgmCommitSelectionOnLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCommitSelectionOnLayerMaskCommand }

  TgmCommitSelectionOnLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCommitSelectionOnAlphaChannelCommand }

  TgmCommitSelectionOnAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FAlphaChannelIndex : Integer;                  // the index of the selected alpha channel

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCancelSelectionFromLayerCommand }

  TgmCancelSelectionFromLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer
    FChannelSet        : TgmChannelSet;

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCancelSelectionFromAlphaChannelCommand }

  TgmCancelSelectionFromAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FAlphaChannelIndex : Integer;                  // the index of the selected alpha channel

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCancelSelectionFromLayerMaskCommand }

  TgmCancelSelectionFromLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteSelectionFromLayerCommand }

  TgmDeleteSelectionFromLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer
    FChannelSet        : TgmChannelSet;

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteSelectionFromLayerMaskCommand }

  TgmDeleteSelectionFromLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList         : TgmLayerList;             // pointer to an external layer list
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FLayerIndex        : Integer;                  // the index of the selected layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteSelectionFromAlphaChannelCommand }

  TgmDeleteSelectionFromAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection
    FAlphaChannelIndex : Integer;                  // the index of the selected alpha channel

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCutPixelsFromLayerCommand }

  TgmCutPixelsFromLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager        : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList             : TgmLayerList;             // pointer to an external layer list
    FOldSelection          : TgmSelection;             // a copy to a selection
    FOldClipboardSelection : TgmSelection;             // a copy to the clipboard selection
    FLayerIndex            : Integer;                  // the index of the selected layer
    FChannelSet            : TgmChannelSet;

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc     : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc  : TgmDeleteExternalSelectionProc;
    FGetClipboardSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteClipboardSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection, AClipboardSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
      AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCutPixelsFromLayerMaskCommand }

  TgmCutPixelsFromLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager        : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList             : TgmLayerList;             // pointer to an external layer list
    FOldSelection          : TgmSelection;             // a copy to a selection
    FOldClipboardSelection : TgmSelection;             // a copy to the clipboard selection
    FLayerIndex            : Integer;                  // the index of the selected layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc     : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc  : TgmDeleteExternalSelectionProc;
    FGetClipboardSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteClipboardSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      ASelection, AClipboardSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
      AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCutPixelsFromAlphaChannelCommand }

  TgmCutPixelsFromAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager        : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection          : TgmSelection;             // a copy to a selection
    FOldClipboardSelection : TgmSelection;             // a copy to the clipboard selection
    FAlphaChannelIndex     : Integer;                  // the index of the selected alpha channel

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc     : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc  : TgmDeleteExternalSelectionProc;
    FGetClipboardSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteClipboardSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AAlphaChannelIndex: Integer;
      ASelection, AClipboardSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
      AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCopyPixelsCommand }

  TgmCopyPixelsCommand = class(TgmCustomCommand)
  private
    FOldSelection          : TgmSelection; // a copy to a selection
    FOldClipboardSelection : TgmSelection; // a copy to the clipboard selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc     : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc  : TgmDeleteExternalSelectionProc;
    FGetClipboardSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteClipboardSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(ASelection, AClipboardSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
      AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPastePixelsOnNewLayerCommand }

  TgmPastePixelsOnNewLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FOldSelection   : TgmSelection;             // copy to the selection (if any) that before the pasting
    FNewSelection   : TgmSelection;             // copy to the selection that were pasted on the new layer
    FNewLayer       : TgmCustomLayer;
    FNewLayerName   : string;
    FOldLayerIndex  : Integer;                  // the active layer index that before the pasting
    FNewLayerIndex  : Integer;                  // the active layer index that after the pasting

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList;
      AOldSelection, ANewSelection: TgmSelection;
      ANewLayer: TgmCustomLayer;
      const AOldLayerIndex, ANewLayerIndex: Integer;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPastePixelsOnColorChannelsCommand }

  TgmPastePixelsOnColorChannelsCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FOldSelection   : TgmSelection;             // copy to the selection (if any) that before the pasting
    FNewSelection   : TgmSelection;             // copy to the selection that were pasted on the new layer
    FLayerIndex     : Integer;                  // the active layer index that before the pasting
    FChannelSet     : TgmChannelSet;            // the channel set after the pasting

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPastePixelsOnLayerMaskCommand }

  TgmPastePixelsOnLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FOldSelection   : TgmSelection;             // copy to the selection (if any) that before the pasting
    FNewSelection   : TgmSelection;             // copy to the selection that were pasted on the new layer
    FLayerIndex     : Integer;                  // the active layer index that before the pasting

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPastePixelsOnAlphaChannelCommand }

  TgmPastePixelsOnAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection   : TgmSelection;             // copy to the selection (if any) that before the pasting
    FNewSelection   : TgmSelection;             // copy to the selection that were pasted on the new layer
    FChannelIndex   : Integer;                  // the active alpha channel index that before the pasting

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelIndex: Integer; AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmNewSelectionOnQuickMaskChannelCommand }

  TgmNewSelectionOnQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection   : TgmSelection;             // a copy to an old selection, if any
    FNewSelection   : TgmSelection;             // a copy to the new created selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager; 
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionPixelProcessOnQuickMaskChannelCommand }

  TgmSelectionPixelProcessOnQuickMaskChannelCommand = class(TgmByteMapProcessCommand)
  private
    FChannelManager        : TgmCustomChannelManager; // pointer to an external channel manager
    FSourceMapFileName     : string;
    FBackgroundMapFileName : string;
    FBackgroundWidth       : Integer;
    FBackgroundHeight      : Integer;
    FSelectionStatus       : TgmSelectionStatus;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const ACommandName: string;
      AChannelManager: TgmCustomChannelManager;
      const AUndoBmp, ARedoBmp: TBitmap32;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionAdjustmentOnQuickMaskChannelCommand }

  TgmSelectionAdjustmentOnQuickMaskChannelCommand = class(TgmSelectionAdjustmentCommand)
  private
    FAdjustmentType : TgmSelectionAdjustmentType;
    FChannelManager : TgmCustomChannelManager; // pointer to an external channel manager
    FOldSelection   : TgmSelection;
    FNewSelection   : TgmSelection;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
  public
    constructor Create(const AAdjustmentType: TgmSelectionAdjustmentType;
      AChannelManager: TgmCustomChannelManager;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCancelSelectionFromQuickMaskChannelCommand }

  TgmCancelSelectionFromQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCommitSelectionOnQuickMaskChannelCommand }

  TgmCommitSelectionOnQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmCutPixelsFromQuickMaskChannelCommand }

  TgmCutPixelsFromQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager        : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection          : TgmSelection;             // a copy to a selection
    FOldClipboardSelection : TgmSelection;             // a copy to the clipboard selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc     : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc  : TgmDeleteExternalSelectionProc;
    FGetClipboardSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteClipboardSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ASelection, AClipboardSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
      AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmDeleteSelectionFromQuickMaskChannelCommand }

  TgmDeleteSelectionFromQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager    : TgmCustomChannelManager;  // pointer to an external channel manager
    FOriginalSelection : TgmSelection;             // a copy to a selection

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmPastePixelsOnQuickMaskChannelCommand }

  TgmPastePixelsOnQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FOldSelection   : TgmSelection;             // copy to the selection (if any) that before the pasting
    FNewSelection   : TgmSelection;             // copy to the selection that were pasted on the new layer

    // methods for getting/deleting an external selection
    FGetExternalSelectionFunc    : TgmGetExternalSelectionFunc;
    FDeleteExternalSelectionProc : TgmDeleteExternalSelectionProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      AOldSelection, ANewSelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      ADeleteSelectionProc: TgmDeleteExternalSelectionProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEnterSelectionTransformModeForLayerCommand }

  TgmEnterSelectionTransformModeForLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FLayerIndex     : Integer;
    FTransformMode  : TgmTransformMode;
    FSelection      : TgmSelection;             // copy to the selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const ATransformMode: TgmTransformMode; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEnterSelectionTransformModeForLayerMaskCommand }

  TgmEnterSelectionTransformModeForLayerMaskCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FLayerIndex     : Integer;
    FTransformMode  : TgmTransformMode;
    FSelection      : TgmSelection;             // copy to the selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const ATransformMode: TgmTransformMode; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEnterSelectionTransformModeForAlphaChannelCommand }

  TgmEnterSelectionTransformModeForAlphaChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FChannelIndex   : Integer;
    FTransformMode  : TgmTransformMode;
    FSelection      : TgmSelection;             // copy to the selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelIndex: Integer; const ATransformMode: TgmTransformMode;
      ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEnterSelectionTransformModeForQuickMaskChannelCommand }

  TgmEnterSelectionTransformModeForQuickMaskChannelCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FTransformMode  : TgmTransformMode;
    FSelection      : TgmSelection;             // copy to the selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const ATransformMode: TgmTransformMode; ASelection: TgmSelection;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionTransformCommand }

  TgmSelectionTransformCommand = class(TgmCustomCommand)
  protected
    function GetCommandName(const ATransformMode: TgmTransformMode): string;
  end; 
  

  { TgmSelectionTransformOnLayerCommand }

  TgmSelectionTransformOnLayerCommand = class(TgmSelectionTransformCommand)
  private
    FLayerList      : TgmLayerList;                // pointer to an external layer list;
    FLayerIndex     : Integer;
    FChannelSet     : TgmChannelSet;
    FOldTransform   : TgmSelectionTransformation;  // a copy to the old transformation of a selection
    FNewTransform   : TgmSelectionTransformation;  // a copy to the new transformation of a selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;
    
    // methods for getting an external selection transformation object
    FGetSelectionTransformationFunc : TgmGetExternalSelectionTransformationFunc;
  public
    constructor Create(const ATransformMode: TgmTransformMode;
      AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldTransform, ANewTransform: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionTransformOnLayerMaskCommand }

  TgmSelectionTransformOnLayerMaskCommand = class(TgmSelectionTransformCommand)
  private
    FLayerList    : TgmLayerList;                // pointer to an external layer list;
    FLayerIndex   : Integer;
    FOldTransform : TgmSelectionTransformation;  // a copy to the old transformation of a selection
    FNewTransform : TgmSelectionTransformation;  // a copy to the new transformation of a selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for getting an external selection transformation object
    FGetSelectionTransformationFunc : TgmGetExternalSelectionTransformationFunc;
  public
    constructor Create(const ATransformMode: TgmTransformMode;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      AOldTransform, ANewTransform: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionTransformOnAlphaChannelCommand }

  TgmSelectionTransformOnAlphaChannelCommand = class(TgmSelectionTransformCommand)
  private
    FChannelManager : TgmCustomChannelManager;     // pointer to an external channel manager;
    FChannelIndex   : Integer;
    FOldTransform   : TgmSelectionTransformation;  // a copy to the old transformation of a selection
    FNewTransform   : TgmSelectionTransformation;  // a copy to the new transformation of a selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for getting an external selection transformation object
    FGetSelectionTransformationFunc : TgmGetExternalSelectionTransformationFunc;
  public
    constructor Create(const ATransformMode: TgmTransformMode;
      AChannelManager: TgmCustomChannelManager; const AChannelIndex: Integer;
      AOldTransform, ANewTransform: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmSelectionTransformOnQuickMaskChannelCommand }

  TgmSelectionTransformOnQuickMaskChannelCommand = class(TgmSelectionTransformCommand)
  private
    FChannelManager : TgmCustomChannelManager;     // pointer to an external channel manager;
    FOldTransform   : TgmSelectionTransformation;  // a copy to the old transformation of a selection
    FNewTransform   : TgmSelectionTransformation;  // a copy to the new transformation of a selection

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for getting an external selection transformation object
    FGetSelectionTransformationFunc : TgmGetExternalSelectionTransformationFunc;
  public
    constructor Create(const ATransformMode: TgmTransformMode;
      AChannelManager: TgmCustomChannelManager;
      AOldTransform, ANewTransform: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmExitSelectionTransformModeCommand }

  TgmExitSelectionTransformModeCommand = class(TgmCustomCommand)
  protected
    function GetCommandName(const AExitChoices: TgmExitTransformModeChoices): string;
  end;

  { TgmExitSelectionTransformFromLayerCommand }

  TgmExitSelectionTransformFromLayerCommand = class(TgmExitSelectionTransformModeCommand)
  private
    FChannelManager           : TgmCustomChannelManager;     // pointer to an external channel manager
    FChannelSet               : TgmChannelSet;
    FLayerList                : TgmLayerList;                // pointer to an external layer list
    FLayerIndex               : Integer;
    FTransformMode            : TgmTransformMode;
    FSelectionBeforeTransform : TgmSelection;                // copy to the old selection
    FTransformation           : TgmSelectionTransformation;  // copy to the transformation before apply it the selection
    FExitChoices              : TgmExitTransformModeChoices;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const ATransformMode: TgmTransformMode;
      const AExitChoices: TgmExitTransformModeChoices;
      ASelectionBeforeTransform: TgmSelection;
      ATransformation: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmExitSelectionTransformFromLayerMaskCommand }

  TgmExitSelectionTransformFromLayerMaskCommand = class(TgmExitSelectionTransformModeCommand)
  private
    FChannelManager           : TgmCustomChannelManager;     // pointer to an external channel manager
    FLayerList                : TgmLayerList;                // pointer to an external layer list
    FLayerIndex               : Integer;
    FTransformMode            : TgmTransformMode;
    FSelectionBeforeTransform : TgmSelection;                // copy to the old selection
    FTransformation           : TgmSelectionTransformation;  // copy to the transformation before apply it the selection
    FExitChoices              : TgmExitTransformModeChoices;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; const ALayerIndex: Integer;
      const ATransformMode: TgmTransformMode;
      const AExitChoices: TgmExitTransformModeChoices;
      ASelectionBeforeTransform: TgmSelection;
      ATransformation: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmExitSelectionTransformFormAlphaChannelCommand }

  TgmExitSelectionTransformFormAlphaChannelCommand = class(TgmExitSelectionTransformModeCommand)
  private
    FChannelManager           : TgmCustomChannelManager;     // pointer to an external channel manager
    FChannelIndex             : Integer;
    FTransformMode            : TgmTransformMode;
    FSelectionBeforeTransform : TgmSelection;                // copy to the old selection
    FTransformation           : TgmSelectionTransformation;  // copy to the transformation before apply it the selection
    FExitChoices              : TgmExitTransformModeChoices;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const AChannelIndex: Integer; const ATransformMode: TgmTransformMode;
      const AExitChoices: TgmExitTransformModeChoices;
      ASelectionBeforeTransform: TgmSelection;
      ATransformation: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmExitSelectionTransformFromQuickMaskChannelCommand }

  TgmExitSelectionTransformFromQuickMaskChannelCommand = class(TgmExitSelectionTransformModeCommand)
  private
    FChannelManager           : TgmCustomChannelManager;     // pointer to an external channel manager
    FTransformMode            : TgmTransformMode;
    FSelectionBeforeTransform : TgmSelection;                // copy to the old selection
    FTransformation           : TgmSelectionTransformation;  // copy to the transformation before apply it the selection
    FExitChoices              : TgmExitTransformModeChoices;

    // methods for getting an external selection
    FGetExternalSelectionFunc : TgmGetExternalSelectionFunc;

    // methods for entering/exiting to/from selection transform mode externally
    FEnterSelectionTransformModeFunc : TgmEnterSelectionTransformModeFunc;
    FExitSelectionTransformModeProc  : TgmExitSelectionTransformModeProc;
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      const ATransformMode: TgmTransformMode;
      const AExitChoices: TgmExitTransformModeChoices;
      ASelectionBeforeTransform: TgmSelection;
      ATransformation: TgmSelectionTransformation;
      AGetSelectionFunc: TgmGetExternalSelectionFunc;
      AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
      AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;


const
  RECT_MARQUEE_COMMAND_ICON_RES_NAME          = 'RECTMARQUEECOMMANDICONMASK';
  SINGLE_ROW_MARQUEE_COMMAND_ICON_RES_NAME    = 'SINGLEROWMARQUEECOMMANDMASK';
  SINGLE_COLUMN_MARQUEE_COMMAND_ICON_RES_NAME = 'SINGLECOLUMNMARQUEECOMMANDMASK';
  ROUND_RECT_MARQUEE_COMMAND_ICON_RES_NAME    = 'ROUNDRECTMARQUEECOMMANDMASK';
  ELLIPTICAL_MARQUEE_COMMAND_ICON_RES_NAME    = 'ELLIPTICALMARQUEECOMMANDMASK';
  POLYGONAL_MARQUEE_COMMAND_ICON_RES_NAME     = 'POLYGONALMARQUEECOMMANDMASK';
  REGULAR_POLY_MARQUEE_COMMAND_ICON_RES_NAME  = 'REGULARPOLYMARQUEECOMMANDMASK';
  LASSO_MARQUEE_COMMAND_ICON_RES_NAME         = 'LASSOMARQUEECOMMANDMASK';
  MAGIC_WAND_MARQUEE_ICON_RES_NAME            = 'MAGICWANDMARQUEECOMMANDMASK';
  MAGNETIC_LASSO_MARQUEE_ICON_RES_NAME        = 'MAGNETICLASSOMARQUEECOMMANDMASK';


implementation

uses
  gmChannels,
  gmImageProcessFuncs;
  

{$R gmSelectionCommandIcons.res}

const
  CREATE_SELECTION_IF_NONE      : Boolean = True;
  DONT_CREATE_SELECTION_IF_NONE : Boolean = False;

  CREATE_SELECTION_TRANSFORMATION_IF_NONE      : Boolean = True;
  DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE : Boolean = False;


procedure GetSelectionStatus(ASelection: TgmSelection;
  var ARec: TgmSelectionStatus);
begin
  if Assigned(ASelection) then
  begin
    ARec.CornerStretched     := ASelection.IsCornerStretched;
    ARec.FlippedHorizontally := ASelection.IsHorizFlipped;
    ARec.FlippedVertically   := ASelection.IsVertFlipped;
    ARec.ForeAlphaChanged    := ASelection.IsForeAlphaChanged;
    ARec.Transformed         := ASelection.IsTransformed;
    ARec.Translated          := ASelection.IsTranslated;
  end;
end;

procedure SetSelectionStatus(ASelection: TgmSelection;
  const ARec: TgmSelectionStatus);
begin
  if Assigned(ASelection) then
  begin
    ASelection.IsCornerStretched  := ARec.CornerStretched;
    ASelection.IsHorizFlipped     := ARec.FlippedHorizontally;
    ASelection.IsVertFlipped      := ARec.FlippedVertically;
    ASelection.IsForeAlphaChanged := ARec.ForeAlphaChanged;
    ASelection.IsTransformed      := ARec.Transformed;
    ASelection.IsTranslated       := ARec.Translated;
  end;
end;

{ TgmCancelSelectionFromAlphaChannelCommand }

constructor TgmCancelSelectionFromAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; const AAlphaChannelIndex: Integer;
  ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Deselect');

  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCancelSelectionFromAlphaChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCancelSelectionFromAlphaChannelCommand.Execute;
var
  LAlphaChannel : TgmAlphaChannel;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // restore background and delete the selection
  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  CopyBitmap32(LAlphaChannel.ChannelLayer.Bitmap, FOriginalSelection.SourceBitmap);
  LAlphaChannel.UpdateChannelThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCancelSelectionFromAlphaChannelCommand.Rollback;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

{ TgmCancelSelectionFromLayerCommand }

constructor TgmCancelSelectionFromLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Deselect');

  FChannelManager := AChannelManager;
  FChannelSet     := AChannelManager.SelectedColorChannels;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCancelSelectionFromLayerCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCancelSelectionFromLayerCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    // restore background and delete the selection
    LLayer.LayerBitmap.Assign(FOriginalSelection.SourceBitmap);
    LLayer.UpdateLayerThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCancelSelectionFromLayerCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
    LLayer.UpdateLayerThumbnail();
  end;
end;

{ TgmCancelSelectionFromLayerMaskCommand }

constructor TgmCancelSelectionFromLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create('Deselect');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCancelSelectionFromLayerMaskCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCancelSelectionFromLayerMaskCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Execute(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Execute(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // restore background and delete the selection
  LLayer.MaskBitmap.Assign(FOriginalSelection.SourceBitmap);
  LLayer.UpdateMaskThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCancelSelectionFromLayerMaskCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Rollback(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCancelSelectionFromLayerMaskCommand.Rollback(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

{ TgmCancelSelectionFromQuickMaskChannelCommand }

constructor TgmCancelSelectionFromQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Deselect');

  FChannelManager := AChannelManager;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCancelSelectionFromQuickMaskChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCancelSelectionFromQuickMaskChannelCommand.Execute;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // restore background and delete the selection
  with FChannelManager.QuickMaskChannel do
  begin
    CopyBitmap32(ChannelLayer.Bitmap, FOriginalSelection.SourceBitmap);
    UpdateChannelThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCancelSelectionFromQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCancelSelectionFromQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

{ TgmCommitSelectionOnAlphaChannelCommand }

constructor TgmCommitSelectionOnAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; const AAlphaChannelIndex: Integer;
  ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Commit Selection');

  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCommitSelectionOnAlphaChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCommitSelectionOnAlphaChannelCommand.Execute;
begin
  inherited;
  
  // we just need to delete the selection
  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCommitSelectionOnAlphaChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCommitSelectionOnAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);
end;

{ TgmCommitSelectionOnLayerCommand }

constructor TgmCommitSelectionOnLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Commit Selection');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCommitSelectionOnLayerCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCommitSelectionOnLayerCommand.Execute;
begin
  inherited;
  
  // we just need to delete the selection
  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCommitSelectionOnLayerCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);
end;

{ TgmCommitSelectionOnLayerMaskCommand }

constructor TgmCommitSelectionOnLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmCommitSelectionOnLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create('Commit Selection');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCommitSelectionOnLayerMaskCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCommitSelectionOnLayerMaskCommand.Execute;
begin
  inherited;
  
  // we just need to delete the selection
  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCommitSelectionOnLayerMaskCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);
end;

{ TgmCommitSelectionOnQuickMaskChannelCommand }

constructor TgmCommitSelectionOnQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Commit Selection');

  FChannelManager := AChannelManager;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmCommitSelectionOnQuickMaskChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmCommitSelectionOnQuickMaskChannelCommand.Execute;
begin
  inherited;
  
  // we just need to delete the selection
  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCommitSelectionOnQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCommitSelectionOnQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);
end;

{ TgmCopyPixelsCommand }

constructor TgmCopyPixelsCommand.Create(
  ASelection, AClipboardSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
  AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCopyPixelsCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCopyPixelsCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCopyPixelsCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AGetClipboardSelectionFunc) then
  begin
    raise Exception.Create('TgmCopyPixelsCommand.Create(): parameter AGetClipboardSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteClipboardSelectionProc) then
  begin
    raise Exception.Create('TgmCopyPixelsCommand.Create(): parameter ADeleteClipboardSelectionProc is nil.');
  end;

  inherited Create('Copy Pixels');

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(ASelection);

  FOldClipboardSelection := nil;
  if Assigned(AClipboardSelection) then
  begin
    FOldClipboardSelection := TgmSelection.Create();
    FOldClipboardSelection.AssignAllSelectionData(AClipboardSelection);
  end;

  FGetExternalSelectionFunc     := AGetSelectionFunc;
  FDeleteExternalSelectionProc  := ADeleteSelectionProc;
  FGetClipboardSelectionFunc    := AGetClipboardSelectionFunc;
  FDeleteClipboardSelectionProc := ADeleteClipboardSelectionProc;
end;

destructor TgmCopyPixelsCommand.Destroy;
begin
  FOldSelection.Free();

  if Assigned(FOldClipboardSelection) then
  begin
    FOldClipboardSelection.Free();
  end;

  inherited;
end;

procedure TgmCopyPixelsCommand.Execute;
var
  LClipboardSelection : TgmSelection;
begin
  inherited;

  // copy selection ...

  // The FGetClipboardSelectionFunc() should create and return a selection
  // pointer. If the external clipboard selection is already existed,
  // return it, directly.
  LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
  
  // saving the target selection to clipboard selection
  LClipboardSelection.AssignAllSelectionData(FOldSelection);
end;

procedure TgmCopyPixelsCommand.Rollback;
var
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldClipboardSelection) then
  begin
    // recovery cropped selection, if any
    LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
    LClipboardSelection.AssignAllSelectionData(FOldClipboardSelection);
  end
  else
  begin
    FDeleteClipboardSelectionProc();
  end;
end;

{ TgmCutPixelsFromAlphaChannelCommand }

constructor TgmCutPixelsFromAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AAlphaChannelIndex: Integer;
  ASelection, AClipboardSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
  AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AGetClipboardSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter AGetClipboardSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteClipboardSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Create(): parameter ADeleteClipboardSelectionProc is nil.');
  end;

  inherited Create('Cut Pixels');

  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(ASelection);

  FOldClipboardSelection := nil;
  if Assigned(AClipboardSelection) then
  begin
    FOldClipboardSelection := TgmSelection.Create();
    FOldClipboardSelection.AssignAllSelectionData(AClipboardSelection);
  end;

  FGetExternalSelectionFunc     := AGetSelectionFunc;
  FDeleteExternalSelectionProc  := ADeleteSelectionProc;
  FGetClipboardSelectionFunc    := AGetClipboardSelectionFunc;
  FDeleteClipboardSelectionProc := ADeleteClipboardSelectionProc;
end;

destructor TgmCutPixelsFromAlphaChannelCommand.Destroy;
begin
  FOldSelection.Free();

  if Assigned(FOldClipboardSelection) then
  begin
    FOldClipboardSelection.Free();
  end;

  inherited;
end;

procedure TgmCutPixelsFromAlphaChannelCommand.Execute;
var
  LAlphaChannel       : TgmAlphaChannel;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // cut selection ...

  // The FGetClipboardSelectionFunc() should create and return a selection
  // pointer. If the external clipboard selection is already existed,
  // return it, directly.
  LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
  LClipboardSelection.AssignAllSelectionData(FOldSelection);  // saving the target selection to clipboard selection

  // restore background and delete the selection
  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  CopyBitmap32(LAlphaChannel.ChannelLayer.Bitmap, FOldSelection.Background);
  LAlphaChannel.UpdateChannelThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCutPixelsFromAlphaChannelCommand.Rollback;
var
  LAlphaChannel       : TgmAlphaChannel;
  LSelection          : TgmSelection;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldClipboardSelection) then
  begin
    // recovery cropped selection, if any
    LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
    LClipboardSelection.AssignAllSelectionData(FOldClipboardSelection);
  end
  else
  begin
    FDeleteClipboardSelectionProc();
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOldSelection);

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

{ TgmCutPixelsFromLayerCommand }

constructor TgmCutPixelsFromLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  ASelection, AClipboardSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
  AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AGetClipboardSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter AGetClipboardSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteClipboardSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerCommand.Create(): parameter ADeleteClipboardSelectionProc is nil.');
  end;

  inherited Create('Cut Pixels');

  FChannelManager := AChannelManager;
  FChannelSet     := AChannelManager.SelectedColorChannels;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(ASelection);

  FOldClipboardSelection := nil;
  if Assigned(AClipboardSelection) then
  begin
    FOldClipboardSelection := TgmSelection.Create();
    FOldClipboardSelection.AssignAllSelectionData(AClipboardSelection);
  end;

  FGetExternalSelectionFunc     := AGetSelectionFunc;
  FDeleteExternalSelectionProc  := ADeleteSelectionProc;
  FGetClipboardSelectionFunc    := AGetClipboardSelectionFunc;
  FDeleteClipboardSelectionProc := ADeleteClipboardSelectionProc;
end;

destructor TgmCutPixelsFromLayerCommand.Destroy;
begin
  FOldSelection.Free();

  if Assigned(FOldClipboardSelection) then
  begin
    FOldClipboardSelection.Free();
  end;

  inherited;
end;

procedure TgmCutPixelsFromLayerCommand.Execute;
var
  LLayer              : TgmCustomLayer;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // cut selection ...

  // The FGetClipboardSelectionFunc() should create and return a selection
  // pointer. If the external clipboard selection is already existed,
  // return it, directly.
  LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
  LClipboardSelection.AssignAllSelectionData(FOldSelection);  // saving the target selection to clipboard selection
  
  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    // restore background and delete the selection
    LLayer.LayerBitmap.Assign(FOldSelection.Background);
    LLayer.UpdateLayerThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCutPixelsFromLayerCommand.Rollback;
var
  LLayer              : TgmCustomLayer;
  LSelection          : TgmSelection;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldClipboardSelection) then
  begin
    // recovery cropped selection, if any
    LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
    LClipboardSelection.AssignAllSelectionData(FOldClipboardSelection);
  end
  else
  begin
    FDeleteClipboardSelectionProc();
  end;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOldSelection);

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
    LLayer.UpdateLayerThumbnail();
  end;
end;

{ TgmCutPixelsFromLayerMaskCommand }

constructor TgmCutPixelsFromLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  ASelection, AClipboardSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
  AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AGetClipboardSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter AGetClipboardSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteClipboardSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): parameter ADeleteClipboardSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create('Cut Pixels');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(ASelection);

  FOldClipboardSelection := nil;
  if Assigned(AClipboardSelection) then
  begin
    FOldClipboardSelection := TgmSelection.Create();
    FOldClipboardSelection.AssignAllSelectionData(AClipboardSelection);
  end;

  FGetExternalSelectionFunc     := AGetSelectionFunc;
  FDeleteExternalSelectionProc  := ADeleteSelectionProc;
  FGetClipboardSelectionFunc    := AGetClipboardSelectionFunc;
  FDeleteClipboardSelectionProc := ADeleteClipboardSelectionProc;
end;

destructor TgmCutPixelsFromLayerMaskCommand.Destroy;
begin
  FOldSelection.Free();

  if Assigned(FOldClipboardSelection) then
  begin
    FOldClipboardSelection.Free();
  end;

  inherited;
end;

procedure TgmCutPixelsFromLayerMaskCommand.Execute;
var
  LLayer              : TgmCustomLayer;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Execute(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Execute(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // cut selection ...

  // The FGetClipboardSelectionFunc() should create and return a selection
  // pointer. If the external clipboard selection is already existed,
  // return it, directly.
  LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
  LClipboardSelection.AssignAllSelectionData(FOldSelection);  // saving the target selection to clipboard selection

  // restore background and delete the selection
  LLayer.MaskBitmap.Assign(FOldSelection.Background);
  LLayer.UpdateMaskThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCutPixelsFromLayerMaskCommand.Rollback;
var
  LLayer              : TgmCustomLayer;
  LSelection          : TgmSelection;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldClipboardSelection) then
  begin
    // recovery cropped selection, if any
    LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
    LClipboardSelection.AssignAllSelectionData(FOldClipboardSelection);
  end
  else
  begin
    FDeleteClipboardSelectionProc();
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Rollback(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmCutPixelsFromLayerMaskCommand.Rollback(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOldSelection);

  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

{ TgmCutPixelsFromQuickMaskChannelCommand }

constructor TgmCutPixelsFromQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ASelection, AClipboardSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc;
  AGetClipboardSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteClipboardSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  if not Assigned(AGetClipboardSelectionFunc) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter AGetClipboardSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteClipboardSelectionProc) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Create(): parameter ADeleteClipboardSelectionProc is nil.');
  end;

  inherited Create('Cut Pixels');

  FChannelManager := AChannelManager;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(ASelection);

  FOldClipboardSelection := nil;
  if Assigned(AClipboardSelection) then
  begin
    FOldClipboardSelection := TgmSelection.Create();
    FOldClipboardSelection.AssignAllSelectionData(AClipboardSelection);
  end;

  FGetExternalSelectionFunc     := AGetSelectionFunc;
  FDeleteExternalSelectionProc  := ADeleteSelectionProc;
  FGetClipboardSelectionFunc    := AGetClipboardSelectionFunc;
  FDeleteClipboardSelectionProc := ADeleteClipboardSelectionProc;
end;

destructor TgmCutPixelsFromQuickMaskChannelCommand.Destroy;
begin
  FOldSelection.Free();

  if Assigned(FOldClipboardSelection) then
  begin
    FOldClipboardSelection.Free();
  end;

  inherited;
end;

procedure TgmCutPixelsFromQuickMaskChannelCommand.Execute;
var
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // cut selection ...

  // The FGetClipboardSelectionFunc() should create and return a selection
  // pointer. If the external clipboard selection is already existed,
  // return it, directly.
  LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
  LClipboardSelection.AssignAllSelectionData(FOldSelection);  // saving the target selection to clipboard selection

  // restore background and delete the selection
  with FChannelManager.QuickMaskChannel do
  begin
    CopyBitmap32(ChannelLayer.Bitmap, FOldSelection.Background);
    UpdateChannelThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmCutPixelsFromQuickMaskChannelCommand.Rollback;
var
  LSelection          : TgmSelection;
  LClipboardSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldClipboardSelection) then
  begin
    // recovery cropped selection, if any
    LClipboardSelection := FGetClipboardSelectionFunc(CREATE_SELECTION_IF_NONE);
    LClipboardSelection.AssignAllSelectionData(FOldClipboardSelection);
  end
  else
  begin
    FDeleteClipboardSelectionProc();
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmCutPixelsFromQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOldSelection);

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

{ TgmDeleteSelectionFromAlphaChannelCommand }

constructor TgmDeleteSelectionFromAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; const AAlphaChannelIndex: Integer;
  ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Delete Selection');

  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmDeleteSelectionFromAlphaChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmDeleteSelectionFromAlphaChannelCommand.Execute;
var
  LAlphaChannel : TgmAlphaChannel;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // restore background and delete the selection
  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  CopyBitmap32(LAlphaChannel.ChannelLayer.Bitmap, FOriginalSelection.Background);
  LAlphaChannel.UpdateChannelThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmDeleteSelectionFromAlphaChannelCommand.Rollback;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of the range.');
  end;

  // switch to current alpha channel
  if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

{ TgmDeleteSelectionFromLayerCommand }

constructor TgmDeleteSelectionFromLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Delete Selection');

  FChannelManager := AChannelManager;
  FChannelSet     := AChannelManager.SelectedColorChannels;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmDeleteSelectionFromLayerCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmDeleteSelectionFromLayerCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    // restore background and delete the selection
    LLayer.LayerBitmap.Assign(FOriginalSelection.Background);
    LLayer.UpdateLayerThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmDeleteSelectionFromLayerCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SimplySelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
    LLayer.UpdateLayerThumbnail();
  end;
end;

{ TgmDeleteSelectionFromLayerMaskCommand }

constructor TgmDeleteSelectionFromLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter ASelection is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create('Delete Selection');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmDeleteSelectionFromLayerMaskCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmDeleteSelectionFromLayerMaskCommand.Execute;
var
  LLayer : TgmCustomLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Execute(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Execute(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // restore background and delete the selection
  LLayer.MaskBitmap.Assign(FOriginalSelection.Background);
  LLayer.UpdateMaskThumbnail();

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmDeleteSelectionFromLayerMaskCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Rollback(): FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmDeleteSelectionFromLayerMaskCommand.Rollback(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

{ TgmDeleteSelectionFromQuickMaskChannelCommand }

constructor TgmDeleteSelectionFromQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Delete Selection');

  FChannelManager := AChannelManager;

  FOriginalSelection := TgmSelection.Create();
  FOriginalSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmDeleteSelectionFromQuickMaskChannelCommand.Destroy;
begin
  FOriginalSelection.Free();
  inherited;
end;

procedure TgmDeleteSelectionFromQuickMaskChannelCommand.Execute;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Execute(): the Quick Mask channel is unvailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // restore background and delete the selection
  with FChannelManager.QuickMaskChannel do
  begin
    CopyBitmap32(ChannelLayer.Bitmap, FOriginalSelection.Background);
    UpdateChannelThumbnail();
  end;

  if Assigned(FDeleteExternalSelectionProc) then
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmDeleteSelectionFromQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmDeleteSelectionFromQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FOriginalSelection);

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

{ TgmEnterSelectionTransformModeForAlphaChannelCommand }

constructor TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; const AChannelIndex: Integer;
  const ATransformMode: TgmTransformMode; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AChannelIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter AChannelIndex is out of range.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  inherited Create('Transform');

  FChannelManager := AChannelManager;
  FChannelIndex   := AChannelIndex;
  FTransformMode  := ATransformMode;

  FSelection := TgmSelection.Create();
  FSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmEnterSelectionTransformModeForAlphaChannelCommand.Destroy;
begin
  FSelection.Free();
  inherited;
end;

procedure TgmEnterSelectionTransformModeForAlphaChannelCommand.Execute;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelComman.Execute(): FChannelIndex is out of range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Execute(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Execute(): cannot enter to selection transform mode.');
  end;
end;

procedure TgmEnterSelectionTransformModeForAlphaChannelCommand.Rollback;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelComman.Rollback(): FChannelIndex is out of range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  // pass DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE to this function will
  // only return the external selection transform object, we should already
  // in selection transform mode
  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForAlphaChannelCommand.Rollback(): we are not in selection transform mode.');
  end;

  LSelectionTransformation.CancelTransform();
  FExitSelectionTransformModeProc();
end;

{ TgmEnterSelectionTransformModeForLayerCommand }

constructor TgmEnterSelectionTransformModeForLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; const ATransformMode: TgmTransformMode;
  ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  inherited Create('Transform');

  FChannelManager := AChannelManager;  
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FTransformMode  := ATransformMode;

  FSelection := TgmSelection.Create();
  FSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmEnterSelectionTransformModeForLayerCommand.Destroy;
begin
  FSelection.Free();
  inherited;
end;

procedure TgmEnterSelectionTransformModeForLayerCommand.Execute;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  // switch to the correct layer and color channels
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Execute(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Execute(): cannot enter to selection transform mode.');
  end;
end;

procedure TgmEnterSelectionTransformModeForLayerCommand.Rollback;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  // switch to the correct layer and color channels
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  // pass DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE to this function will
  // only return the external selection transform object, we should already
  // in selection transform mode
  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerCommand.Rollback(): we are not in selection transform mode.');
  end;

  LSelectionTransformation.CancelTransform();
  FExitSelectionTransformModeProc();
end;

{ TgmEnterSelectionTransformModeForLayerMaskCommand }

constructor TgmEnterSelectionTransformModeForLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; const ATransformMode: TgmTransformMode;
  ASelection: TgmSelection; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): the layer mask is unavailable.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  inherited Create('Transform');

  FChannelManager := AChannelManager;  
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FTransformMode  := ATransformMode;

  FSelection := TgmSelection.Create();
  FSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmEnterSelectionTransformModeForLayerMaskCommand.Destroy;
begin
  FSelection.Free();
  inherited;
end;

procedure TgmEnterSelectionTransformModeForLayerMaskCommand.Execute;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Execute(): The mask of the layer is unavailable.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Execute(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Execute(): cannot enter to selection transform mode.');
  end;
end;

procedure TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback(): The mask of the layer is unavailable.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback(): the external selection is not existed');
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  // pass DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE to this function will
  // only return the external selection transform object, we should already
  // in selection transform mode
  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForLayerMaskCommand.Rollback(): we are not in selection transform mode.');
  end;

  LSelectionTransformation.CancelTransform();
  FExitSelectionTransformModeProc();
end;

{ TgmEnterSelectionTransformModeForQuickMaskChannelCommand }

constructor TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const ATransformMode: TgmTransformMode; ASelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(ASelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): parameter ASelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  inherited Create('Transform');

  FChannelManager := AChannelManager;
  FTransformMode  := ATransformMode;

  FSelection := TgmSelection.Create();
  FSelection.AssignAllSelectionData(ASelection);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Destroy;
begin
  FSelection.Free();
  inherited;
end;

procedure TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Execute;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Execute(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Execute(): cannot enter to selection transform mode.');
  end;
end;

procedure TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Rollback;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelection);

  // pass DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE to this function will
  // only return the external selection transform object, we should already
  // in selection transform mode
  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmEnterSelectionTransformModeForQuickMaskChannelCommand.Rollback(): we are not in selection transform mode.');
  end;

  LSelectionTransformation.CancelTransform();
  FExitSelectionTransformModeProc();
end;

{ TgmExitSelectionTransformFromLayerCommand }

constructor TgmExitSelectionTransformFromLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  const ATransformMode: TgmTransformMode;
  const AExitChoices: TgmExitTransformModeChoices;
  ASelectionBeforeTransform: TgmSelection;
  ATransformation: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(ASelectionBeforeTransform) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter ASelectionBeforeTransform is nil.');
  end;

  if not Assigned(ATransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter ATransformation is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  LCommandName := GetCommandName(AExitChoices);
  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FChannelSet     := AChannelManager.SelectedColorChannels;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FTransformMode  := ATransformMode;
  FExitChoices    := AExitChoices;

  FSelectionBeforeTransform := TgmSelection.Create();
  FSelectionBeforeTransform.AssignAllSelectionData(ASelectionBeforeTransform);

  FTransformation := TgmSelectionTransformation.Create(ASelectionBeforeTransform);
  FTransformation.AssignTransformData(ATransformation);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmExitSelectionTransformFromLayerCommand.Destroy;
begin
  FSelectionBeforeTransform.Free();
  FTransformation.Free();
  
  inherited;
end;

procedure TgmExitSelectionTransformFromLayerCommand.Execute;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Execute(): the FLayerIndex is out of the range.');
  end;

  // switch to the correct layer and color channels
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Execute(): the external selection is not existed');
  end;

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Execute(): the external selection transformation is unavailable.');
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);

    case FExitChoices of
      etmcApplyTransform:
        begin
          AcceptTransform();
        end;

      etmcCancelTransform:
        begin
          CancelTransform();
          
          LLayer := FLayerList.Layers[FLayerIndex];
          if LLayer.PixelFeature = lpfNormalPixelized then
          begin
            ShowTransformedSelection(LLayer.LayerBitmap, FChannelSet);
            LLayer.UpdateLayerThumbnail();
          end;
        end;
    end;
  end;

  FExitSelectionTransformModeProc();
end;

procedure TgmExitSelectionTransformFromLayerCommand.Rollback;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];

  // switch to the correct layer and color channels
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelectionBeforeTransform);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerCommand.Rollback(): cannot enter to selection transform mode.');
  end;

  LSelectionTransformation.AssignTransformData(FTransformation);

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ExecuteTransform();

    if LLayer.PixelFeature = lpfNormalPixelized then
    begin
      ShowTransformedSelection(LLayer.LayerBitmap, FChannelSet);
      LLayer.UpdateLayerThumbnail();
    end;
  end;   
end;

{ TgmExitSelectionTransformFromLayerMaskCommand }

constructor TgmExitSelectionTransformFromLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  const ALayerIndex: Integer; const ATransformMode: TgmTransformMode;
  const AExitChoices: TgmExitTransformModeChoices;
  ASelectionBeforeTransform: TgmSelection;
  ATransformation: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
var
  LCommandName : string;
  LLayer       : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsLayerEnabled then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): the mask of the target layer is not available.');
  end;

  if not Assigned(ASelectionBeforeTransform) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter ASelectionBeforeTransform is nil.');
  end;

  if not Assigned(ATransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter ATransformation is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  LCommandName := GetCommandName(AExitChoices);
  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FTransformMode  := ATransformMode;
  FExitChoices    := AExitChoices;

  FSelectionBeforeTransform := TgmSelection.Create();
  FSelectionBeforeTransform.AssignAllSelectionData(ASelectionBeforeTransform);

  FTransformation := TgmSelectionTransformation.Create(ASelectionBeforeTransform);
  FTransformation.AssignTransformData(ATransformation);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmExitSelectionTransformFromLayerMaskCommand.Destroy;
begin
  FSelectionBeforeTransform.Free();
  FTransformation.Free();
  
  inherited;
end;

procedure TgmExitSelectionTransformFromLayerMaskCommand.Execute;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Execute(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Execute(): the mask of the target layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Execute(): the external selection is not existed');
  end;

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Execute(): the external selection transformation is unavailable.');
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    
    case FExitChoices of
      etmcApplyTransform:
        begin
          AcceptTransform();
        end;

      etmcCancelTransform:
        begin
          CancelTransform();
          
          LLayer := FLayerList.Layers[FLayerIndex];
          ShowTransformedSelection(LLayer.MaskBitmap, [csGrayscale]);
          LLayer.UpdateMaskThumbnail();
        end;
    end;
  end;

  FExitSelectionTransformModeProc();
end;

procedure TgmExitSelectionTransformFromLayerMaskCommand.Rollback;
var
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Rollback(): the mask of the target layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelectionBeforeTransform);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnLayerMaskCommand.Rollback(): cannot enter to selection transform mode.');
  end;

  LSelectionTransformation.AssignTransformData(FTransformation);

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ExecuteTransform();
    ShowTransformedSelection(LLayer.MaskBitmap, [csGrayscale]);
    LLayer.UpdateMaskThumbnail();
  end;
end;

{ TgmExitSelectionTransformFromQuickMaskChannelCommand }

constructor TgmExitSelectionTransformFromQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const ATransformMode: TgmTransformMode;
  const AExitChoices: TgmExitTransformModeChoices;
  ASelectionBeforeTransform: TgmSelection;
  ATransformation: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): the quick mask channel is unavailable.');
  end;

  if not Assigned(ASelectionBeforeTransform) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter ASelectionBeforeTransform is nil.');
  end;

  if not Assigned(ATransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter ATransformation is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  LCommandName := GetCommandName(AExitChoices);
  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FTransformMode  := ATransformMode;
  FExitChoices    := AExitChoices;

  FSelectionBeforeTransform := TgmSelection.Create();
  FSelectionBeforeTransform.AssignAllSelectionData(ASelectionBeforeTransform);

  FTransformation := TgmSelectionTransformation.Create(ASelectionBeforeTransform);
  FTransformation.AssignTransformData(ATransformation);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmExitSelectionTransformFromQuickMaskChannelCommand.Destroy;
begin
  FSelectionBeforeTransform.Free();
  FTransformation.Free();
  
  inherited;
end;

procedure TgmExitSelectionTransformFromQuickMaskChannelCommand.Execute;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Execute(): the quick mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Execute(): the external selection is not existed');
  end;

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Execute(): the external selection transformation is unavailable.');
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    
    case FExitChoices of
      etmcApplyTransform:
        begin
          AcceptTransform();
        end;

      etmcCancelTransform:
        begin
          CancelTransform();
          
          ShowTransformedSelection(
            FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
            [csGrayscale]);
            
          FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
        end;
    end;
  end;

  FExitSelectionTransformModeProc();
end;

procedure TgmExitSelectionTransformFromQuickMaskChannelCommand.Rollback;
var
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Rollback(): the quick mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelectionBeforeTransform);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnQuickMaskChannelCommand.Rollback(): cannot enter to selection transform mode.');
  end;

  LSelectionTransformation.AssignTransformData(FTransformation);

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ExecuteTransform();

    ShowTransformedSelection(
      FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap,
      [csGrayscale]);
      
    FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
  end;
end;

{ TgmExitSelectionTransformModeCommand }

function TgmExitSelectionTransformModeCommand.GetCommandName(
  const AExitChoices: TgmExitTransformModeChoices): string;
var
  LCommandName : string;
begin
  LCommandName := '';

  case AExitChoices of
    etmcApplyTransform:
      begin
        LCommandName := 'Apply Transform';
      end;

    etmcCancelTransform:
      begin
        LCommandName := 'Cancel Transform';
      end;
  end;

  Result := LCommandName;
end;

{ TgmNewSelectionOnAlphaChannelCommand }

constructor TgmNewSelectionOnAlphaChannelCommand.Create(
  const ACommandName: string; AChannelManager: TgmCustomChannelManager;
  AOldSelection, ANewSelection: TgmSelection; const AAlphaChannelIndex: Integer;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create(ACommandName);

  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := nil;
  if Assigned(ANewSelection) then
  begin
    FNewSelection := TgmSelection.Create();
    FNewSelection.AssignAllSelectionData(ANewSelection);
  end;

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmNewSelectionOnAlphaChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  if Assigned(FNewSelection) then
  begin
    FNewSelection.Free();
  end;
  
  inherited;
end;

procedure TgmNewSelectionOnAlphaChannelCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FNewSelection) then
  begin
    if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
    begin
      raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of the range.');
    end;

    // switch to current alpha channel
    if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
    begin
      FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FNewSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmNewSelectionOnAlphaChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldSelection) then
  begin
    if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
    begin
      raise Exception.Create('TgmNewSelectionOnAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of the range.');
    end;

    // switch to current alpha channel
    if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
    begin
      FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmExitSelectionTransformFormAlphaChannelCommand }

constructor TgmExitSelectionTransformFormAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  const AChannelIndex: Integer;
  const ATransformMode: TgmTransformMode;
  const AExitChoices: TgmExitTransformModeChoices;
  ASelectionBeforeTransform: TgmSelection;
  ATransformation: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AEnterSelectionTransformModeFunc: TgmEnterSelectionTransformModeFunc;
  AExitSelectionTransformModeProc: TgmExitSelectionTransformModeProc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AChannelIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter AChannelIndex is out of range.');
  end;

  if not Assigned(ASelectionBeforeTransform) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter ASelectionBeforeTransform is nil.');
  end;

  if not Assigned(ATransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter ATransformation is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AEnterSelectionTransformModeFunc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter AEnterSelectionTransformModeFunc is nil.');
  end;

  if not Assigned(AExitSelectionTransformModeProc) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Create(): parameter AExitSelectionTransformModeProc is nil.');
  end;

  LCommandName := GetCommandName(AExitChoices);
  inherited Create(LCommandName);

  FChannelManager := AChannelManager;
  FChannelIndex   := AChannelIndex;
  FTransformMode  := ATransformMode;
  FExitChoices    := AExitChoices;

  FSelectionBeforeTransform := TgmSelection.Create();
  FSelectionBeforeTransform.AssignAllSelectionData(ASelectionBeforeTransform);

  FTransformation := TgmSelectionTransformation.Create(ASelectionBeforeTransform);
  FTransformation.AssignTransformData(ATransformation);

  FGetExternalSelectionFunc        := AGetSelectionFunc;
  FEnterSelectionTransformModeFunc := AEnterSelectionTransformModeFunc;
  FExitSelectionTransformModeProc  := AExitSelectionTransformModeProc;
end;

destructor TgmExitSelectionTransformFormAlphaChannelCommand.Destroy;
begin
  FSelectionBeforeTransform.Free();
  FTransformation.Free();
  
  inherited;
end;

procedure TgmExitSelectionTransformFormAlphaChannelCommand.Execute;
var
  LChannel                 : TgmAlphaChannel;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Execute(): the FChannelIndex is out of the range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Execute(): the external selection is not existed');
  end;

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, DONT_CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Execute(): the external selection transformation is unavailable.');
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    
    case FExitChoices of
      etmcApplyTransform:
        begin
          AcceptTransform();
        end;

      etmcCancelTransform:
        begin
          CancelTransform();
          
          LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);
          ShowTransformedSelection(LChannel.ChannelLayer.Bitmap, [csGrayscale]);
          LChannel.UpdateChannelThumbnail();
        end;
    end;
  end;

  FExitSelectionTransformModeProc();
end;

procedure TgmExitSelectionTransformFormAlphaChannelCommand.Rollback;
var
  LChannel                 : TgmAlphaChannel;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Rollback(): the FChannelIndex is out of the range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Rollback(): the external selection is not existed');
  end;

  LSelection.AssignAllSelectionData(FSelectionBeforeTransform);

  LSelectionTransformation := FEnterSelectionTransformModeFunc(
    FTransformMode, CREATE_SELECTION_TRANSFORMATION_IF_NONE);

  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmApplySelectionTransformOnAlphaChannelCommand.Rollback(): cannot enter to selection transform mode.');
  end;

  LSelectionTransformation.AssignTransformData(FTransformation);

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);
  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ExecuteTransform();
    ShowTransformedSelection(LChannel.ChannelLayer.Bitmap, [csGrayscale]);
    LChannel.UpdateChannelThumbnail();
  end;
end;

{ TgmNewSelectionOnLayerCommand }

constructor TgmNewSelectionOnLayerCommand.Create( const ACommandName: string;
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  AOldSelection, ANewSelection: TgmSelection; const ALayerIndex: Integer;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create(ACommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := nil;
  if Assigned(ANewSelection) then
  begin
    FNewSelection := TgmSelection.Create();
    FNewSelection.AssignAllSelectionData(ANewSelection);
  end;
  
  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmNewSelectionOnLayerCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  if Assigned(FNewSelection) then
  begin
    FNewSelection.Free();
  end;
  
  inherited;
end;

procedure TgmNewSelectionOnLayerCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FNewSelection) then
  begin
    if (FChannelManager.CurrentChannelType <> ctColorChannel) or
       (FLayerList.SelectedIndex <> FLayerIndex) then
    begin
      FLayerList.SimplySelectLayer(FLayerIndex);
      FChannelManager.SelectColorChannel(0, True);
    end;
    
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FNewSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmNewSelectionOnLayerCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldSelection) then
  begin
    if (FChannelManager.CurrentChannelType <> ctColorChannel) or
       (FLayerList.SelectedIndex <> FLayerIndex) then
    begin
      FLayerList.SimplySelectLayer(FLayerIndex);
      FChannelManager.SelectColorChannel(0, True);
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmNewSelectionOnLayerMaskCommand }

constructor TgmNewSelectionOnLayerMaskCommand.Create(
  const ACommandName: string; AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; AOldSelection, ANewSelection: TgmSelection;
  const ALayerIndex: Integer; AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer: TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmNewSelectionOnLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create(ACommandName);

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := nil;
  if Assigned(ANewSelection) then
  begin
    FNewSelection := TgmSelection.Create();
    FNewSelection.AssignAllSelectionData(ANewSelection);
  end;

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmNewSelectionOnLayerMaskCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  if Assigned(FNewSelection) then
  begin
    FNewSelection.Free();
  end;
  
  inherited;
end;

procedure TgmNewSelectionOnLayerMaskCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FNewSelection) then
  begin
    // channel switching ...
    if FLayerList.SelectedIndex <> FLayerIndex then
    begin
      FLayerList.SelectLayer(FLayerIndex);
    end;

    if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
    begin
      FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
    end;

    if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
    begin
      FChannelManager.SelectLayerMaskChannel();
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FNewSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmNewSelectionOnLayerMaskCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldSelection) then
  begin
    // channel switching ...
    if FLayerList.SelectedIndex <> FLayerIndex then
    begin
      FLayerList.SelectLayer(FLayerIndex);
    end;

    if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
    begin
      FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
    end;

    if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
    begin
      FChannelManager.SelectLayerMaskChannel();
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmNewSelectionOnQuickMaskChannelCommand }

constructor TgmNewSelectionOnQuickMaskChannelCommand.Create(
  const ACommandName: string; AChannelManager: TgmCustomChannelManager;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewSelectionOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmNewSelectionOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmNewSelectionOnQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create(ACommandName);

  FChannelManager := AChannelManager;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := nil;
  if Assigned(ANewSelection) then
  begin
    FNewSelection := TgmSelection.Create();
    FNewSelection.AssignAllSelectionData(ANewSelection);
  end;

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmNewSelectionOnQuickMaskChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;

  if Assigned(FNewSelection) then
  begin
    FNewSelection.Free();
  end;
  
  inherited;
end;

procedure TgmNewSelectionOnQuickMaskChannelCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FNewSelection) then
  begin
    if not Assigned(FChannelManager.QuickMaskChannel) then
    begin
      raise Exception.Create('TgmNewSelectionOnQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
    end;

    // switch to current alpha channel
    if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
    begin
      FChannelManager.SelectQuickMaskChannel();
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FNewSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

procedure TgmNewSelectionOnQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if Assigned(FOldSelection) then
  begin
    if not Assigned(FChannelManager.QuickMaskChannel) then
    begin
      raise Exception.Create('TgmNewSelectionOnQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
    end;

    // switch to current alpha channel
    if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
    begin
      FChannelManager.SelectQuickMaskChannel();
    end;

    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmPastePixelsOnAlphaChannelCommand }

constructor TgmPastePixelsOnAlphaChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager; const AChannelIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AChannelIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Create(): parameter AChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Paste Pixels');

  FChannelManager := AChannelManager;
  FChannelIndex   := AChannelIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmPastePixelsOnAlphaChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;
  
  FNewSelection.Free();

  inherited;
end;

procedure TgmPastePixelsOnAlphaChannelCommand.Execute;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Execute(): FChannelIndex is out of range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FNewSelection);

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

procedure TgmPastePixelsOnAlphaChannelCommand.Rollback;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnAlphaChannelCommand.Rollback(): FChannelIndex is out of range.');
  end;

  // channel switching ...
  if FChannelManager.AlphaChannelList.SelectedIndex <> FChannelIndex then
  begin
    FChannelManager.SelectAlphaChannel(FChannelIndex, False);
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection  := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;

  // We are not going to render the old selection on the alpha channel to
  // recover the pixels to the state that before the pasting, because it is
  // much time-consuming. We can just recover it with the background
  // information in the selection that after the pasting.
  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);
  CopyBitmap32(LAlphaChannel.ChannelLayer.Bitmap, FNewSelection.SourceBitmap);
  LAlphaChannel.UpdateChannelThumbnail();
end;

{ TgmPastePixelsOnColorChannelsCommand }

constructor TgmPastePixelsOnColorChannelsCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Paste Pixels');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;
  FChannelSet     := AChannelManager.SelectedColorChannels;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmPastePixelsOnColorChannelsCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;
  
  FNewSelection.Free();

  inherited;
end;

procedure TgmPastePixelsOnColorChannelsCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Execute(): FLayerIndex is out of range.');
  end;

  // switching to the correct layer and color channel, if necessary,
  // before rendering the selection on the layer
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FNewSelection);

  LLayer := FLayerList.Layers[FLayerIndex];

  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
    LLayer.UpdateLayerThumbnail();
  end;
end;

procedure TgmPastePixelsOnColorChannelsCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnColorChannelsCommand.Rollback(): FLayerIndex is out of range.');
  end;

  // switching to the correct layer and color channel, if necessary,
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection  := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;

  // We cannot recover the layer by just rendering the old selection on it, 
  // because we don't know which color channels it was before the pasting.
  // But we can recover the layer with the background information in the
  // selection that after the pasting.
  LLayer := FLayerList.Layers[FLayerIndex];

  if LLayer.PixelFeature = lpfNormalPixelized then
  begin
    LLayer.LayerBitmap.Assign(FNewSelection.SourceBitmap);
    LLayer.UpdateLayerThumbnail();
  end;
end;

{ TgmPastePixelsOnLayerMaskCommand }

constructor TgmPastePixelsOnLayerMaskCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): the layer mask has not been enabled.');
  end;

  if LLayer.LayerProcessStage <> lpsMask then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Create(): the layer is not working on mask.');
  end;

  inherited Create('Paste Pixels');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmPastePixelsOnLayerMaskCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;
  
  FNewSelection.Free();

  inherited;
end;

procedure TgmPastePixelsOnLayerMaskCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Execute(): The mask of the layer is not available.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FNewSelection);

  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

procedure TgmPastePixelsOnLayerMaskCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerMaskCommand.Rollback(): The mask of the layer is not available.');
  end;

  // switching to the correct layer and color channel, if necessary,
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection  := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;

  // We are not going to render the old selection on the mask to recover the
  // mask to the state that before the pasting, because it is much
  // time-consuming. We can just recover it with the background information
  // in the selection that after the pasting.
  LLayer.MaskBitmap.Assign(FNewSelection.SourceBitmap);
  LLayer.UpdateMaskThumbnail();
end;

{ TgmPastePixelsOnNewLayerCommand }

constructor TgmPastePixelsOnNewLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList;
  AOldSelection, ANewSelection: TgmSelection;
  ANewLayer: TgmCustomLayer;
  const AOldLayerIndex, ANewLayerIndex: Integer;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(ANewLayer) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter ANewLayer is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(AOldLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter AOldLayerIndex is out of range.');
  end;

  if not ALayerList.IsValidIndex(ANewLayerIndex) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter ANewLayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmPastePixelsOnLayerCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Paste Pixels');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FNewLayer       := ANewLayer.GetCopy();
  FNewLayerName   := ANewLayer.LayerName;
  FOldLayerIndex  := AOldLayerIndex;
  FNewLayerIndex  := ANewLayerIndex;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmPastePixelsOnNewLayerCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;
  
  FNewSelection.Free();
  FNewLayer.Free();

  inherited;
end;

procedure TgmPastePixelsOnNewLayerCommand.Execute;
var
  LLayer          : TgmCustomLayer;
  LLayerTypeCount : Integer;
  LSelection      : TgmSelection;
begin
  inherited;

  // recreate the pasted layer
  LLayer           := FNewLayer.GetCopy();
  LLayer.LayerName := FNewLayerName;

  // the following SimplyInsert() of the layer list would not make effects
  // on the layer counter, so we have to set the counter by ourselves
  LLayerTypeCount := FLayerList.GetLayerCounter(LLayer.ClassName) + 1;
  FLayerList.SetLayerCounter(LLayer.ClassName, LLayerTypeCount);

  // insert it to the layer list
  FLayerList.SimplyInsert(FNewLayerIndex, LLayer);
  FLayerList.SelectLayer(FNewLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
  FChannelManager.AlphaChannelList.HideAllChannels();

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FNewSelection);

  // we don't have to render the new selection on the new layer,
  // because the new layer that we have copied contains all the pixels we need

  LLayer.UpdateLayerThumbnail();
end;

procedure TgmPastePixelsOnNewLayerCommand.Rollback;
var
  LLayer          : TgmCustomLayer;
  LLayerTypeCount : Integer;
  LSelection      : TgmSelection;
begin
  inherited;

  // active the layer the before pasting the seletion and
  // remove the layer that the selection pasted on
  FLayerList.SelectLayer(FOldLayerIndex);
  FChannelManager.SelectColorChannel(0, True);
  FChannelManager.AlphaChannelList.HideAllChannels();
  FLayerList.SimplyDeleteLayer(FNewLayerIndex);

  LLayer := FLayerList.Layers[FOldLayerIndex];
  
  // the SimplyDeleteLayer() would not make effects on the layer counter,
  // so we have to set the counter by ourselves
  LLayerTypeCount := FLayerList.GetLayerCounter(LLayer.ClassName) - 1;
  FLayerList.SetLayerCounter(LLayer.ClassName, LLayerTypeCount);

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);

    // here, we don't have to render the new selection on the layer,
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;
end;

{ TgmPastePixelsOnQuickMaskChannelCommand }

constructor TgmPastePixelsOnQuickMaskChannelCommand.Create(
  AChannelManager: TgmCustomChannelManager;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  ADeleteSelectionProc: TgmDeleteExternalSelectionProc);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(ADeleteSelectionProc) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Create(): parameter ADeleteSelectionProc is nil.');
  end;

  inherited Create('Paste Pixels');

  FChannelManager := AChannelManager;

  FOldSelection := nil;
  if Assigned(AOldSelection) then
  begin
    FOldSelection := TgmSelection.Create();
    FOldSelection.AssignAllSelectionData(AOldSelection);
  end;

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  FGetExternalSelectionFunc    := AGetSelectionFunc;
  FDeleteExternalSelectionProc := ADeleteSelectionProc;
end;

destructor TgmPastePixelsOnQuickMaskChannelCommand.Destroy;
begin
  if Assigned(FOldSelection) then
  begin
    FOldSelection.Free();
  end;
  
  FNewSelection.Free();

  inherited;
end;

procedure TgmPastePixelsOnQuickMaskChannelCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  // The FGetExternalSelectionFunc() should create and return a selection
  // pointer. If the external selection is already existed, return it, directly.
  LSelection := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
  LSelection.AssignAllSelectionData(FNewSelection);

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

procedure TgmPastePixelsOnQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmPastePixelsOnQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // channel switching ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  if Assigned(FOldSelection) then
  begin
    // The FGetExternalSelectionFunc() should create and return a selection
    // pointer. If the external selection is already existed, return it, directly.
    LSelection  := FGetExternalSelectionFunc(CREATE_SELECTION_IF_NONE);
    LSelection.AssignAllSelectionData(FOldSelection);
  end
  else
  begin
    FDeleteExternalSelectionProc();
  end;

  // We are not going to render the old selection on the alpha channel to
  // recover the pixels to the state that before the pasting, because it is
  // much time-consuming. We can just recover it with the background
  // information in the selection that after the pasting.
  with FChannelManager.QuickMaskChannel do
  begin
    CopyBitmap32(ChannelLayer.Bitmap, FNewSelection.SourceBitmap);
    UpdateChannelThumbnail();
  end;
end;

{ TgmSelectionAdjustmentCommand }

function TgmSelectionAdjustmentCommand.GetCommandNameByAdjustmentType(
  const AType: TgmSelectionAdjustmentType): string;
begin
  Result := '';

  case AType of
    satTranslate:
      begin
        Result := 'Move Selection';
      end;
      
    satStretchCorner:
      begin
        Result := 'Resize Selection';
      end;

    satInverse:
      begin
        Result := 'Select Inverse';
      end;

    satFeather:
      begin
        Result := 'Feather';
      end;

    satNudgeOutline:
      begin
        Result := 'Nudge Outline'
      end;

    satFlipHorizontally:
      begin
        Result := 'Flip Horizontal';
      end;

    satFlipVertically:
      begin
        Result := 'Flip Vertical';
      end;
  end;
end;

{ TgmSelectionAdjustmentOnAlphaChannelCommand }

constructor TgmSelectionAdjustmentOnAlphaChannelCommand.Create(
  const AAdjustmentType: TgmSelectionAdjustmentType;
  AChannelManager: TgmCustomChannelManager;
  const AAlphaChannelIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AOldSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): parameter AOldSelection is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LCommandName := GetCommandNameByAdjustmentType(AAdjustmentType);

  inherited Create(LCommandName);

  FAdjustmentType    := AAdjustmentType;
  FChannelManager    := AChannelManager;
  FAlphaChannelIndex := AAlphaChannelIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(AOldSelection);

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  // methods for getting an external selection
  FGetExternalSelectionFunc := AGetSelectionFunc;
end;

destructor TgmSelectionAdjustmentOnAlphaChannelCommand.Destroy;
begin
  FOldSelection.Free();
  FNewSelection.Free();
  
  inherited;
end;

procedure TgmSelectionAdjustmentOnAlphaChannelCommand.Execute;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Execute(): the external selection is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of range.');
  end;

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);

  // switch to current alpha channel ...
  if (FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex) or
     (FChannelManager.CurrentChannelType <> ctAlphaChannel) then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  LSelection.AssignAllSelectionData(FNewSelection);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

procedure TgmSelectionAdjustmentOnAlphaChannelCommand.Rollback;
var
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Rollback(): the external selection is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of range.');
  end;

  LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);

  // switch to current alpha channel ...
  if (FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex) or
     (FChannelManager.CurrentChannelType <> ctAlphaChannel) then
  begin
    FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
  end;

  LSelection.AssignAllSelectionData(FOldSelection);
  LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
  LAlphaChannel.UpdateChannelThumbnail();
end;

{ TgmSelectionAdjustmentOnLayerCommand }

constructor TgmSelectionAdjustmentOnLayerCommand.Create(
  const AAdjustmentType: TgmSelectionAdjustmentType;
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AOldSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter AOldSelection is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LCommandName := GetCommandNameByAdjustmentType(AAdjustmentType);

  inherited Create(LCommandName);

  FAdjustmentType := AAdjustmentType;
  FChannelManager := AChannelManager;
  FChannelSet     := AChannelManager.SelectedColorChannels;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(AOldSelection);

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  // methods for getting an external selection
  FGetExternalSelectionFunc := AGetSelectionFunc;
end;

destructor TgmSelectionAdjustmentOnLayerCommand.Destroy;
begin
  FOldSelection.Free();
  FNewSelection.Free();
  
  inherited;
end;

procedure TgmSelectionAdjustmentOnLayerCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Execute(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Execute(): FLayerIndex is out of range.');
  end;

  // We have to switch back to the correct layer and color channel,
  // otherwise, the selection rendering on layer is incorrect.
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LLayer := FLayerList.Layers[FLayerIndex];

  LSelection.AssignAllSelectionData(FNewSelection);
  LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
  LLayer.UpdateLayerThumbnail();
end;

procedure TgmSelectionAdjustmentOnLayerCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Rollback(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Rollback(): FLayerIndex is out of range.');
  end;

  // We have to switch back to the correct layer and color channel,
  // otherwise, the selection rendering on layer is incorrect.
  if (FChannelManager.CurrentChannelType <> ctColorChannel) or
     (FLayerList.SelectedIndex <> FLayerIndex) then
  begin
    FLayerList.SelectLayer(FLayerIndex);
    FChannelManager.SelectColorChannel(0, True);
  end;

  LLayer := FLayerList.Layers[FLayerIndex];

  LSelection.AssignAllSelectionData(FOldSelection);
  LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
  LLayer.UpdateLayerThumbnail();
end;

{ TgmSelectionAdjustmentOnLayerMaskCommand }

constructor TgmSelectionAdjustmentOnLayerMaskCommand.Create(
  const AAdjustmentType: TgmSelectionAdjustmentType;
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  LCommandName : string;
  LLayer       : TgmCustomLayer;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the mask of the layer is not enabled.');
  end;

  if not Assigned(AOldSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter AOldSelection is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LCommandName := GetCommandNameByAdjustmentType(AAdjustmentType);

  inherited Create(LCommandName);

  FAdjustmentType := AAdjustmentType;
  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ALayerIndex;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(AOldSelection);

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  // methods for getting an external selection
  FGetExternalSelectionFunc := AGetSelectionFunc;
end;

destructor TgmSelectionAdjustmentOnLayerMaskCommand.Destroy;
begin
  FOldSelection.Free();
  FNewSelection.Free();
  
  inherited;
end;

procedure TgmSelectionAdjustmentOnLayerMaskCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the mask of the layer is not enabled.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection.AssignAllSelectionData(FNewSelection);
  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

procedure TgmSelectionAdjustmentOnLayerMaskCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the mask of the layer is not enabled.');
  end;

  // channel switching ...
  if FLayerList.SelectedIndex <> FLayerIndex then
  begin
    FLayerList.SelectLayer(FLayerIndex);
  end;

  if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
  begin
    FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
  end;

  if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
  begin
    FChannelManager.SelectLayerMaskChannel();
  end;

  LSelection.AssignAllSelectionData(FOldSelection);
  LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
  LLayer.UpdateMaskThumbnail();
end;

{ TgmSelectionAdjustmentOnQuickMaskChannelCommand }

constructor TgmSelectionAdjustmentOnQuickMaskChannelCommand.Create(
  const AAdjustmentType: TgmSelectionAdjustmentType;
  AChannelManager: TgmCustomChannelManager;
  AOldSelection, ANewSelection: TgmSelection;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  LCommandName : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AOldSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): parameter AOldSelection is nil.');
  end;

  if not Assigned(ANewSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): parameter ANewSelection is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LCommandName := GetCommandNameByAdjustmentType(AAdjustmentType);

  inherited Create(LCommandName);

  FAdjustmentType := AAdjustmentType;
  FChannelManager := AChannelManager;

  FOldSelection := TgmSelection.Create();
  FOldSelection.AssignAllSelectionData(AOldSelection);

  FNewSelection := TgmSelection.Create();
  FNewSelection.AssignAllSelectionData(ANewSelection);

  // methods for getting an external selection
  FGetExternalSelectionFunc := AGetSelectionFunc;
end;

destructor TgmSelectionAdjustmentOnQuickMaskChannelCommand.Destroy;
begin
  FOldSelection.Free();
  FNewSelection.Free();
  
  inherited;
end;

procedure TgmSelectionAdjustmentOnQuickMaskChannelCommand.Execute;
var
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Execute(): the external selection is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.AssignAllSelectionData(FNewSelection);
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

procedure TgmSelectionAdjustmentOnQuickMaskChannelCommand.Rollback;
var
  LSelection : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Rollback(): the external selection is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  // switch to Quick Mask channel ...
  if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
  begin
    FChannelManager.SelectQuickMaskChannel();
  end;

  with FChannelManager.QuickMaskChannel do
  begin
    LSelection.AssignAllSelectionData(FOldSelection);
    LSelection.ShowSelection(ChannelLayer.Bitmap, [csGrayscale]);
    UpdateChannelThumbnail();
  end;
end;

{ TgmSelectionPixelProcessOnAlphaChannelCommand }

constructor TgmSelectionPixelProcessOnAlphaChannelCommand.Create(
  const ACommandName: string;
  AChannelManager: TgmCustomChannelManager;
  const AAlphaChannelIndex: Integer;
  const AUndoBmp, ARedoBmp: TBitmap32;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  i             : Integer;
  LSelection    : TgmSelection;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Create(): parameter AAlphaChannelIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FChannelManager           := AChannelManager;
  FAlphaChannelIndex        := AAlphaChannelIndex;
  FGetExternalSelectionFunc := AGetSelectionFunc;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Create: the external selection is not existed.');
  end;

  GetSelectionStatus(LSelection, FSelectionStatus);

  // save the source and background of the selection to disk for saving memory
  LRandomString          := IntToStr( GetTickCount() );
  FSourceMapFileName     := COMMAND_DATA_DIR + '\SelectionSource' + LRandomString;
  FBackgroundMapFileName := COMMAND_DATA_DIR + '\SelectionBack' + LRandomString;
  FBackgroundWidth       := LSelection.Background.Width;
  FBackgroundHeight      := LSelection.Background.Height;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      // saving the source map of the selection
      LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);
      LByteMap.ReadFrom(LSelection.SourceBitmap, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FSourceMapFileName);

      LDataStream.Clear();

      // saving the background map of the selection
      LByteMap.ReadFrom(LSelection.Background, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FBackgroundMapFileName);
    finally
      LByteMap.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmSelectionPixelProcessOnAlphaChannelCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FSourceMapFileName) then
  begin
    DeleteFile(PChar(FSourceMapFileName));
  end;

  if FileExists(FBackgroundMapFileName) then
  begin
    DeleteFile(PChar(FBackgroundMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmSelectionPixelProcessOnAlphaChannelCommand.Execute;
var
  i             : Integer;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Execute(): the external selection is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Execute(): FAlphaChannelIndex is out of range.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FRedoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FRedoMapWidth, FRedoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
        LAlphaChannel.UpdateChannelThumbnail();

        // switch to current alpha channel
        if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
        begin
          FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmSelectionPixelProcessOnAlphaChannelCommand.Rollback;
var
  i             : Integer;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LAlphaChannel : TgmAlphaChannel;
  LSelection    : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Rollback(): the external selection is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FAlphaChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnAlphaChannelCommand.Rollback(): FAlphaChannelIndex is out of range.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FUndoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FUndoMapWidth, FUndoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        LAlphaChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FAlphaChannelIndex]);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(LAlphaChannel.ChannelLayer.Bitmap, [csGrayscale]);
        LAlphaChannel.UpdateChannelThumbnail();

        // switch to current alpha channel
        if FChannelManager.AlphaChannelList.SelectedIndex <> FAlphaChannelIndex then
        begin
          FChannelManager.SelectAlphaChannel(FAlphaChannelIndex, False);
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmSelectionPixelProcessOnLayerCommand }

constructor TgmSelectionPixelProcessOnLayerCommand.Create(
  const ACommandName: string;
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList;
  const ACurrentLayerIndex: Integer;
  const AUndoBmp, ARedoBmp: TBitmap32;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  LLayer        : TgmCustomLayer;
  LSelection    : TgmSelection;
  LDataStream   : TMemoryStream;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ACurrentLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create(): parameter ACurrentLayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LLayer := ALayerList.Layers[ACurrentLayerIndex];
  if LLayer.PixelFeature <> lpfNormalPixelized then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create(): target layer is not normal-pixelized.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FChannelManager           := AChannelManager;
  FLayerList                := ALayerList;
  FLayerIndex               := ACurrentLayerIndex;
  FChannelSet               := AChannelManager.SelectedColorChannels;
  FGetExternalSelectionFunc := AGetSelectionFunc;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Create: the external selection is not existed.');
  end;

  GetSelectionStatus(LSelection, FSelectionStatus);

  // save the source and background of the selection to disk for saving memory
  LRandomString          := IntToStr( GetTickCount() );
  FSourceBmpFileName     := COMMAND_DATA_DIR + '\SelectionSource' + LRandomString;
  FBackgroundBmpFileName := COMMAND_DATA_DIR + '\SelectionBack' + LRandomString;

  LDataStream := TMemoryStream.Create();
  try
    LSelection.SourceBitmap.SaveToStream(LDataStream);
    LDataStream.Position := 0;
    LDataStream.SaveToFile(FSourceBmpFileName);

    LSelection.Background.SaveToStream(LDataStream);
    LDataStream.Position := 0;
    LDataStream.SaveToFile(FBackgroundBmpFileName);
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmSelectionPixelProcessOnLayerCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FSourceBmpFileName) then
  begin
    DeleteFile(PChar(FSourceBmpFileName));
  end;

  if FileExists(FBackgroundBmpFileName) then
  begin
    DeleteFile(PChar(FBackgroundBmpFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmSelectionPixelProcessOnLayerCommand.Execute;
var
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Execute(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature <> lpfNormalPixelized then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Execute(): target layer is not normal-pixelized.');
  end;

  if FileExists(FRedoBmpFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // We have to switch back to the correct layer and color channel,
      // otherwise, the selection rendering on layer is incorrect.
      if (FChannelManager.CurrentChannelType <> ctColorChannel) or
         (FLayerList.SelectedIndex <> FLayerIndex) then
      begin
        FLayerList.SelectLayer(FLayerIndex);
        FChannelManager.SelectColorChannel(0, True);
      end;

      LDataStream.LoadFromFile(FRedoBmpFileName);
      LDataStream.Position := 0;

      LSelection.CutOriginal.LoadFromStream(LDataStream);
      LSelection.GetForeground(); // update the foregound of the selection

      // update background of the selection
      LDataStream.LoadFromFile(FSourceBmpFileName);
      LDataStream.Position := 0;
      LSelection.SourceBitmap.LoadFromStream(LDataStream);

      LDataStream.LoadFromFile(FBackgroundBmpFileName);
      LDataStream.Position := 0;
      LSelection.Background.LoadFromStream(LDataStream);

      // we have to restore the status of the selection, before showing the selection
      SetSelectionStatus(LSelection, FSelectionStatus);
      LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
      LLayer.UpdateLayerThumbnail();
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmSelectionPixelProcessOnLayerCommand.Rollback;
var
  LDataStream : TMemoryStream;
  LLayer      : TgmCustomLayer;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Rollback(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if LLayer.PixelFeature <> lpfNormalPixelized then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerCommand.Rollback(): target layer is not normal-pixelized.');
  end;

  if FileExists(FUndoBmpFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // We have to switch back to the correct layer and color channel,
      // otherwise, the selection rendering on layer is incorrect.
      if (FChannelManager.CurrentChannelType <> ctColorChannel) or
         (FLayerList.SelectedIndex <> FLayerIndex) then
      begin
        FLayerList.SelectLayer(FLayerIndex);
        FChannelManager.SelectColorChannel(0, True);
      end;

      LDataStream.LoadFromFile(FUndoBmpFileName);
      LDataStream.Position := 0;

      LSelection.CutOriginal.LoadFromStream(LDataStream);
      LSelection.GetForeground();  // update foregound of the selection

      // update background of the selection
      LDataStream.LoadFromFile(FSourceBmpFileName);
      LDataStream.Position := 0;
      LSelection.SourceBitmap.LoadFromStream(LDataStream);

      LDataStream.LoadFromFile(FBackgroundBmpFileName);
      LDataStream.Position := 0;
      LSelection.Background.LoadFromStream(LDataStream);

      // we have to restore the status of the selection, before showing the selection
      SetSelectionStatus(LSelection, FSelectionStatus);
      LSelection.ShowSelection(LLayer.LayerBitmap, FChannelSet);
      LLayer.UpdateLayerThumbnail();
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmSelectionPixelProcessOnLayerMaskCommand }

constructor TgmSelectionPixelProcessOnLayerMaskCommand.Create(
  const ACommandName: string;
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList;
  const ALayerIndex: Integer;
  const AUndoBmp, ARedoBmp: TBitmap32;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  i             : Integer;
  LLayer        : TgmCustomLayer;
  LSelection    : TgmSelection;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create(): parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create(): parameter ALayerIndex is out of range.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsLayerEnabled then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create(): the mask of the target layer is not available.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FChannelManager           := AChannelManager;
  FLayerList                := ALayerList;
  FLayerIndex               := ALayerIndex;
  FGetExternalSelectionFunc := AGetSelectionFunc;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Create: the external selection is not existed.');
  end;

  GetSelectionStatus(LSelection, FSelectionStatus);

  // save the source and background of the selection to disk for saving memory
  LRandomString          := IntToStr( GetTickCount() );
  FSourceMapFileName     := COMMAND_DATA_DIR + '\SelectionSource' + LRandomString;
  FBackgroundMapFileName := COMMAND_DATA_DIR + '\SelectionBack' + LRandomString;
  FBackgroundWidth       := LSelection.Background.Width;
  FBackgroundHeight      := LSelection.Background.Height;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      // saving the source map of the selection
      LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);
      LByteMap.ReadFrom(LSelection.SourceBitmap, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FSourceMapFileName);

      LDataStream.Clear();

      // saving the background map of the selection
      LByteMap.ReadFrom(LSelection.Background, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FBackgroundMapFileName);
    finally
      LByteMap.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmSelectionPixelProcessOnLayerMaskCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FSourceMapFileName) then
  begin
    DeleteFile(PChar(FSourceMapFileName));
  end;

  if FileExists(FBackgroundMapFileName) then
  begin
    DeleteFile(PChar(FBackgroundMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end;

procedure TgmSelectionPixelProcessOnLayerMaskCommand.Execute;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LLayer      : TgmCustomLayer;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Execute(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Execute(): the mask of the target layer is not available.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FRedoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FRedoMapWidth, FRedoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
        LLayer.UpdateMaskThumbnail();

        // channel switching ...
        if FLayerList.SelectedIndex <> FLayerIndex then
        begin
          FLayerList.SelectLayer(FLayerIndex);
        end;

        if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
        begin
          FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
        end;

        if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
        begin
          FChannelManager.SelectLayerMaskChannel();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmSelectionPixelProcessOnLayerMaskCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LLayer      : TgmCustomLayer;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Rollback(): the external selection is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnLayerMaskCommand.Rollback(): the mask of the target layer is not available.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FUndoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FUndoMapWidth, FUndoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(LLayer.MaskBitmap, [csGrayscale]);
        LLayer.UpdateMaskThumbnail();

        // channel switching ...
        if FLayerList.SelectedIndex <> FLayerIndex then
        begin
          FLayerList.SelectLayer(FLayerIndex);
        end;

        if FLayerList.SelectedLayer.LayerProcessStage <> lpsMask then
        begin
          FLayerList.SelectedLayer.LayerProcessStage := lpsMask;
        end;

        if FChannelManager.CurrentChannelType <> ctLayerMaskChannel then
        begin
          FChannelManager.SelectLayerMaskChannel();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmSelectionPixelProcessOnQuickMaskChannelCommand }

constructor TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(
  const ACommandName: string; AChannelManager: TgmCustomChannelManager;
  const AUndoBmp, ARedoBmp: TBitmap32;
  AGetSelectionFunc: TgmGetExternalSelectionFunc);
var
  i             : Integer;
  LSelection    : TgmSelection;
  LDataStream   : TMemoryStream;
  LByteMap      : TByteMap;
  LByteBits     : PByte;
  LRandomString : string;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(): parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(): the Quick Mask channel is unavailable.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create(): parameter AGetSelectionFunc is nil.');
  end;

  inherited Create(ACommandName, AUndoBmp, ARedoBmp);

  FChannelManager           := AChannelManager;
  FGetExternalSelectionFunc := AGetSelectionFunc;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Create: the external selection is not existed.');
  end;

  GetSelectionStatus(LSelection, FSelectionStatus);

  // save the source and background of the selection to disk for saving memory
  LRandomString          := IntToStr( GetTickCount() );
  FSourceMapFileName     := COMMAND_DATA_DIR + '\SelectionSource' + LRandomString;
  FBackgroundMapFileName := COMMAND_DATA_DIR + '\SelectionBack' + LRandomString;
  FBackgroundWidth       := LSelection.Background.Width;
  FBackgroundHeight      := LSelection.Background.Height;

  LDataStream := TMemoryStream.Create();
  try
    LByteMap := TByteMap.Create();
    try
      // saving the source map of the selection
      LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);
      LByteMap.ReadFrom(LSelection.SourceBitmap, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FSourceMapFileName);

      LDataStream.Clear();

      // saving the background map of the selection
      LByteMap.ReadFrom(LSelection.Background, ctUniformRGB);

{$WARN UNSAFE_CODE OFF}
      // save byte map to a stream
      LByteBits := @LByteMap.Bits[0];
      for i := 0 to (LByteMap.Height - 1) do
      begin
        LDataStream.Write(LByteBits^, LByteMap.Width);
        Inc(LByteBits, LByteMap.Width);
      end;
{$WARN UNSAFE_CODE ON}

      LDataStream.Position := 0;
      LDataStream.SaveToFile(FBackgroundMapFileName);
    finally
      LByteMap.Free();
    end;
  finally
    LDataStream.Clear();
    LDataStream.Free();
  end;
end;

destructor TgmSelectionPixelProcessOnQuickMaskChannelCommand.Destroy;
begin
{$WARN UNSAFE_TYPE OFF}
  if FileExists(FSourceMapFileName) then
  begin
    DeleteFile(PChar(FSourceMapFileName));
  end;

  if FileExists(FBackgroundMapFileName) then
  begin
    DeleteFile(PChar(FBackgroundMapFileName));
  end;
{$WARN UNSAFE_TYPE ON}

  inherited;
end; 

procedure TgmSelectionPixelProcessOnQuickMaskChannelCommand.Execute;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Execute(): the external selection is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Execute(): the Quick Mask channel is unavailable.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FRedoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FRedoMapWidth, FRedoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, [csGrayscale]);
        FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();

        // switch to Quick Mask channel
        if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
        begin
          FChannelManager.SelectQuickMaskChannel();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

procedure TgmSelectionPixelProcessOnQuickMaskChannelCommand.Rollback;
var
  i           : Integer;
  LDataStream : TMemoryStream;
  LByteMap    : TByteMap;
  LByteBits   : PByte;
  LSelection  : TgmSelection;
begin
  inherited;

  // The FGetExternalSelectionFunc() should return a selection pointer.
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Rollback(): the external selection is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionPixelProcessOnQuickMaskChannelCommand.Rollback(): the Quick Mask channel is unavailable.');
  end;

  if FileExists(FRedoMapFileName) then
  begin
    LDataStream := TMemoryStream.Create();
    try
      // read in Byte Map data for cut original map of the selection
      LByteMap := TByteMap.Create();
      try
        LDataStream.LoadFromFile(FUndoMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FUndoMapWidth, FUndoMapHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.CutOriginal, ctUniformRGB);
        LSelection.GetForeground(); // update the foregound of the selection

        // update background of the selection
        LDataStream.Clear();
        LDataStream.LoadFromFile(FSourceMapFileName);
        LDataStream.Position := 0;
        LByteMap.SetSize(FBackgroundWidth, FBackgroundHeight);

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.SourceBitmap, ctUniformRGB);

        LDataStream.Clear();
        LDataStream.LoadFromFile(FBackgroundMapFileName);
        LDataStream.Position := 0;

{$WARN UNSAFE_CODE OFF}
        LByteBits := @LByteMap.Bits[0];
        for i := 0 to (LByteMap.Height - 1) do
        begin
          LDataStream.Read(LByteBits^, LByteMap.Width);
          Inc(LByteBits, LByteMap.Width);
        end;
{$WARN UNSAFE_CODE ON}

        LByteMap.WriteTo(LSelection.Background, ctUniformRGB);

        // we have to restore the status of the selection, before showing the selection
        SetSelectionStatus(LSelection, FSelectionStatus);
        LSelection.ShowSelection(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, [csGrayscale]);
        FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();

        // switch to current Quick Mask channel
        if FChannelManager.CurrentChannelType <> ctQuickMaskChannel then
        begin
          FChannelManager.SelectQuickMaskChannel();
        end;
      finally
        LByteMap.Free();
      end;
    finally
      LDataStream.Clear();
      LDataStream.Free();
    end;
  end;
end;

{ TgmSelectionTransformCommand }

function TgmSelectionTransformCommand.GetCommandName(
  const ATransformMode: TgmTransformMode): string;
var
  LCommandName : string;
begin
  LCommandName := '';

  case ATransformMode of
    tmDistort:
      begin
        LCommandName := 'Distort Transform';
      end;

    tmRotate:
      begin
        LCommandName := 'Rotate Transform';
      end;

    tmScale:
      begin
        LCommandName := 'Scale Transform';
      end;

    tmTranslate:
      begin
        LCommandName := 'Translate Transform';
      end;
  end;

  Result := LCommandName;
end;

{ TgmSelectionTransformOnAlphaChannelCommand }

constructor TgmSelectionTransformOnAlphaChannelCommand.Create(
  const ATransformMode: TgmTransformMode;
  AChannelManager: TgmCustomChannelManager; const AChannelIndex: Integer;
  AOldTransform, ANewTransform: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);
var
  LCommandName : string;
  LSelection   : TgmSelection;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter AChannelManager is nil.');
  end;

  if not AChannelManager.AlphaChannelList.IsValidIndex(AChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter AChannelIndex is out of the range.');
  end;

  if not Assigned(AOldTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter AOldTransform is nil.');
  end;

  if not Assigned(ANewTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter ANewTransform is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AGetSelectionTransformationFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the parameter AGetSelectionTransformationFunc is nil.');
  end;

  LCommandName := GetCommandName(ATransformMode);
  inherited Create(LCommandName);

  FChannelManager                 := AChannelManager;
  FChannelIndex                   := AChannelIndex;
  FGetExternalSelectionFunc       := AGetSelectionFunc;
  FGetSelectionTransformationFunc := AGetSelectionTransformationFunc;
  
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Create(): the external selection is not existed.');
  end;

  FOldTransform := TgmSelectionTransformation.Create(LSelection);
  FOldTransform.AssignTransformData(AOldTransform);

  FNewTransform := TgmSelectionTransformation.Create(LSelection);
  FNewTransform.AssignTransformData(ANewTransform);
end;

destructor TgmSelectionTransformOnAlphaChannelCommand.Destroy;
begin
  FOldTransform.Free();
  FNewTransform.Free();

  inherited;
end;

procedure TgmSelectionTransformOnAlphaChannelCommand.Execute;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LChannel                 : TgmAlphaChannel;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Execute(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Execute(): the external selection transformation is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Execute(): the FChannelIndex is out of the range.');
  end;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FNewTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(LChannel.ChannelLayer.Bitmap, [csGrayscale]);
    LChannel.UpdateChannelThumbnail();
  end;
end;

procedure TgmSelectionTransformOnAlphaChannelCommand.Rollback;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LChannel                 : TgmAlphaChannel;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Rollback(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Rollback(): the external selection transformation is not existed.');
  end;

  if not FChannelManager.AlphaChannelList.IsValidIndex(FChannelIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnAlphaChannelCommand.Rollback(): the FChannelIndex is out of the range.');
  end;

  LChannel := TgmAlphaChannel(FChannelManager.AlphaChannelList.Channels[FChannelIndex]);

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FOldTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(LChannel.ChannelLayer.Bitmap, [csGrayscale]);
    LChannel.UpdateChannelThumbnail();
  end;
end;

{ TgmSelectionTransformOnLayerCommand }

constructor TgmSelectionTransformOnLayerCommand.Create(
  const ATransformMode: TgmTransformMode;
  AChannelManager: TgmCustomChannelManager;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldTransform, ANewTransform: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);
var
  LCommandName : string;
  LSelection   : TgmSelection;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter AChannelManager is nil.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter ALayerIndex is out of the range.');
  end;

  if not Assigned(AOldTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter AOldTransform is nil.');
  end;

  if not Assigned(ANewTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter ANewTransform is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AGetSelectionTransformationFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the parameter AGetSelectionTransformationFunc is nil.');
  end;

  LCommandName := GetCommandName(ATransformMode);
  inherited Create(LCommandName);

  FChannelSet                     := AChannelManager.SelectedColorChannels;
  FLayerList                      := ALayerList;
  FLayerIndex                     := ALayerIndex;
  FGetExternalSelectionFunc       := AGetSelectionFunc;
  FGetSelectionTransformationFunc := AGetSelectionTransformationFunc;
  
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Create(): the external selection is not existed.');
  end;

  FOldTransform := TgmSelectionTransformation.Create(LSelection);
  FOldTransform.AssignTransformData(AOldTransform);

  FNewTransform := TgmSelectionTransformation.Create(LSelection);
  FNewTransform.AssignTransformData(ANewTransform);
end;

destructor TgmSelectionTransformOnLayerCommand.Destroy;
begin
  FOldTransform.Free();
  FNewTransform.Free();

  inherited;
end;

procedure TgmSelectionTransformOnLayerCommand.Execute;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Execute(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Execute(): the external selection transformation is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Execute(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FNewTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();

    if LLayer.PixelFeature = lpfNormalPixelized then
    begin
      ShowTransformedSelection(LLayer.LayerBitmap, FChannelSet);
      LLayer.UpdateLayerThumbnail();
    end;
  end;
end;

procedure TgmSelectionTransformOnLayerCommand.Rollback;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Rollback(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Rollback(): the external selection transformation is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FOldTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();

    if LLayer.PixelFeature = lpfNormalPixelized then
    begin
      ShowTransformedSelection(LLayer.LayerBitmap, FChannelSet);
      LLayer.UpdateLayerThumbnail();
    end;
  end;
end;

{ TgmSelectionTransformOnLayerMaskCommand }

constructor TgmSelectionTransformOnLayerMaskCommand.Create(
  const ATransformMode: TgmTransformMode;
  ALayerList: TgmLayerList; const ALayerIndex: Integer;
  AOldTransform, ANewTransform: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);
var
  LCommandName : string;
  LLayer       : TgmCustomLayer;
  LSelection   : TgmSelection;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter ALayerList is nil.');
  end;

  if not ALayerList.IsValidIndex(ALayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter ALayerIndex is out of the range.');
  end;

  LLayer := ALayerList.Layers[ALayerIndex];
  if not LLayer.IsLayerEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the mask of the target layer is not available.');
  end;

  if not Assigned(AOldTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter AOldTransform is nil.');
  end;

  if not Assigned(ANewTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter ANewTransform is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AGetSelectionTransformationFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the parameter AGetSelectionTransformationFunc is nil.');
  end;

  LCommandName := GetCommandName(ATransformMode);
  inherited Create(LCommandName);

  FLayerList                      := ALayerList;
  FLayerIndex                     := ALayerIndex;
  FGetExternalSelectionFunc       := AGetSelectionFunc;
  FGetSelectionTransformationFunc := AGetSelectionTransformationFunc;
  
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Create(): the external selection is not existed.');
  end;

  FOldTransform := TgmSelectionTransformation.Create(LSelection);
  FOldTransform.AssignTransformData(AOldTransform);

  FNewTransform := TgmSelectionTransformation.Create(LSelection);
  FNewTransform.AssignTransformData(ANewTransform);
end;

destructor TgmSelectionTransformOnLayerMaskCommand.Destroy;
begin
  FOldTransform.Free();
  FNewTransform.Free();

  inherited;
end;

procedure TgmSelectionTransformOnLayerMaskCommand.Execute;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the external selection transformation is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Execute(): the mask of the target layer is not available.');
  end;

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FNewTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(LLayer.MaskBitmap, [csGrayscale]);
    LLayer.UpdateMaskThumbnail();
  end;
end;

procedure TgmSelectionTransformOnLayerMaskCommand.Rollback;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LLayer                   : TgmCustomLayer;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the external selection transformation is not existed.');
  end;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the FLayerIndex is out of the range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not LLayer.IsMaskEnabled then
  begin
    raise Exception.Create('TgmSelectionTransformOnLayerMaskCommand.Rollback(): the mask of the target layer is not available.');
  end;

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FOldTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(LLayer.MaskBitmap, [csGrayscale]);
    LLayer.UpdateMaskThumbnail();
  end;
end;

{ TgmSelectionTransformOnQuickMaskChannelCommand }

constructor TgmSelectionTransformOnQuickMaskChannelCommand.Create(
  const ATransformMode: TgmTransformMode;
  AChannelManager: TgmCustomChannelManager;
  AOldTransform, ANewTransform: TgmSelectionTransformation;
  AGetSelectionFunc: TgmGetExternalSelectionFunc;
  AGetSelectionTransformationFunc: TgmGetExternalSelectionTransformationFunc);
var
  LCommandName : string;
  LSelection   : TgmSelection;
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the parameter AChannelManager is nil.');
  end;

  if not Assigned(AChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the quick mask channel is unavailable.');
  end;

  if not Assigned(AOldTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the parameter AOldTransform is nil.');
  end;

  if not Assigned(ANewTransform) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the parameter ANewTransform is nil.');
  end;

  if not Assigned(AGetSelectionFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the parameter AGetSelectionFunc is nil.');
  end;

  if not Assigned(AGetSelectionTransformationFunc) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the parameter AGetSelectionTransformationFunc is nil.');
  end;

  LCommandName := GetCommandName(ATransformMode);
  inherited Create(LCommandName);

  FChannelManager                 := AChannelManager;
  FGetExternalSelectionFunc       := AGetSelectionFunc;
  FGetSelectionTransformationFunc := AGetSelectionTransformationFunc;
  
  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Create(): the external selection is not existed.');
  end;

  FOldTransform := TgmSelectionTransformation.Create(LSelection);
  FOldTransform.AssignTransformData(AOldTransform);

  FNewTransform := TgmSelectionTransformation.Create(LSelection);
  FNewTransform.AssignTransformData(ANewTransform);
end;

destructor TgmSelectionTransformOnQuickMaskChannelCommand.Destroy;
begin
  FOldTransform.Free();
  FNewTransform.Free();

  inherited;
end;

procedure TgmSelectionTransformOnQuickMaskChannelCommand.Execute;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Execute(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Execute(): the external selection transformation is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Execute(): the quick mask channel is unavailable.');
  end;

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FNewTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, [csGrayscale]);
    FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
  end;
end;

procedure TgmSelectionTransformOnQuickMaskChannelCommand.Rollback;
var
  i                        : Integer;
  LVertexArray             : array [0..3] of TPoint;
  LSelection               : TgmSelection;
  LSelectionTransformation : TgmSelectionTransformation;
begin
  inherited;

  LSelection := FGetExternalSelectionFunc(DONT_CREATE_SELECTION_IF_NONE);
  if not Assigned(LSelection) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Rollback(): the external selection is not existed.');
  end;

  LSelectionTransformation := FGetSelectionTransformationFunc();
  if not Assigned(LSelectionTransformation) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Rollback(): the external selection transformation is not existed.');
  end;

  if not Assigned(FChannelManager.QuickMaskChannel) then
  begin
    raise Exception.Create('TgmSelectionTransformOnQuickMaskChannelCommand.Rollback(): the quick mask channel is unavailable.');
  end;

  for i := 0 to 3 do
  begin
    LVertexArray[i] := FOldTransform.Vertices[i];
  end;

  with LSelectionTransformation do
  begin
    ConnectSelection(LSelection);
    ChangeVertices(LVertexArray);
    ExecuteTransform();
    ShowTransformedSelection(FChannelManager.QuickMaskChannel.ChannelLayer.Bitmap, [csGrayscale]);
    FChannelManager.QuickMaskChannel.UpdateChannelThumbnail();
  end;
end;

end.
