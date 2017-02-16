unit gmRichTextLayerCommands;

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

// Update Date: 2016/11/23

interface

uses
{ Delphi }
  Windows, SysUtils, Classes,
{ GraphicsMagicLib }
  gmChannelManager,
  gmHistoryCommands,
  gmLayers,
  gmRichTextLayer;

type
  { TgmNewTextOnNewRichTextLayerCommand }

  TgmNewTextOnNewRichTextLayerCommand = class(TgmCustomCommand)
  private
    FChannelManager : TgmCustomChannelManager;  // pointer to an external channel manager
    FLayerList      : TgmLayerList;             // pointer to an external layer list
    FLayerIndex     : Integer;
    FLayerName      : string;
    FTextLayer      : TgmRichTextLayer;         // a copy to the newly created rich-text layer
  public
    constructor Create(AChannelManager: TgmCustomChannelManager;
      ALayerList: TgmLayerList; ATextLayer: TgmRichTextLayer;
      const ATextLayerIndex: Integer);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmEditTextOnExistedRichTextLayerCommand }

  TgmEditTextOnExistedRichTextLayerCommand = class(TgmCustomCommand)
  private
    FLayerList     : TgmLayerList;  // pointer to an external layer list
    FLayerIndex    : Integer;
    FOldTextStream : TMemoryStream;
    FNewTextStream : TMemoryStream;
  public
    constructor Create(ALayerList: TgmLayerList; const ATextLayerIndex: Integer;
      AOldTextStream, ANewTextStream: TMemoryStream);

    destructor Destroy; override;

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmTranslateTextRegionCommand }

  TgmTranslateTextRegionCommand = class(TgmCustomCommand)
  private
    FLayerList        : TgmLayerList;  // pointer to an external layer list
    FLayerIndex       : Integer;
    FTranslatedVector : TPoint;
  public
    constructor Create(ALayerList: TgmLayerList;
      const ATextLayerIndex: Integer; const ATranslatedVector: TPoint);

    procedure Execute; override;
    procedure Rollback; override;
  end;

  { TgmScaleTextRegionCommand }

  TgmScaleTextRegionCommand = class(TgmCustomCommand)
  private
    FLayerList      : TgmLayerList;  // pointer to an external layer list
    FLayerIndex     : Integer;
    FOldTopLeft     : TPoint;
    FOldBottomRight : TPoint;
    FNewTopLeft     : TPoint;
    FNewBottomRight : TPoint;
  public
    constructor Create(ALayerList: TgmLayerList;
      const ATextLayerIndex: Integer;
      const AOldTopLeft, AOldBottomRight: TPoint;
      const ANewTopLeft, ANewBottomRight: TPoint);

    procedure Execute; override;
    procedure Rollback; override;
  end;


const
  TYPE_TOOL_ICON_RES_NAME   = 'TYPETOOLICONMASK';

implementation

{$R gmRichTextCommandIcons.res}


{ TgmEditTextOnExistedRichTextLayerCommand }

constructor TgmEditTextOnExistedRichTextLayerCommand.Create(
  ALayerList: TgmLayerList; const ATextLayerIndex: Integer;
  AOldTextStream, ANewTextStream: TMemoryStream);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(ATextLayerIndex) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Create(): parameter ATextLayerIndex is out of range.');
  end;

  LLayer := TgmRichTextLayer(ALayerList.Layers[ATextLayerIndex]);
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Create(): the layer at ATextLayerIndex in the layer list is not a rich-text layer.');
  end;

  inherited Create('Edit Type Layer');
  
  FLayerList     := ALayerList;
  FLayerIndex    := ATextLayerIndex;
  FOldTextStream := nil;
  FNewTextStream := nil;

  if Assigned(AOldTextStream) and (AOldTextStream.Size > 0) then
  begin
    try
      AOldTextStream.Position := 0;
      FOldTextStream          := TMemoryStream.Create();

      FOldTextStream.LoadFromStream(AOldTextStream);
      FOldTextStream.Position := 0;
    finally
      AOldTextStream.Position := 0;
    end;
  end;

  if Assigned(ANewTextStream) and (ANewTextStream.Size > 0) then
  begin
    try
      ANewTextStream.Position := 0;
      FNewTextStream          := TMemoryStream.Create();
      
      FNewTextStream.LoadFromStream(ANewTextStream);
      FNewTextStream.Position := 0;
    finally
      ANewTextStream.Position := 0;
    end;
  end;

  ChangeCommandIconByResourceName(TYPE_TOOL_ICON_RES_NAME);
end;

destructor TgmEditTextOnExistedRichTextLayerCommand.Destroy;
begin
  if Assigned(FOldTextStream) then
  begin
    FOldTextStream.Free();
  end;

  if Assigned(FNewTextStream) then
  begin
    FNewTextStream.Free();
  end;

  inherited;
end;

procedure TgmEditTextOnExistedRichTextLayerCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := TgmRichTextLayer(FLayerList.Layers[FLayerIndex]);
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Execute(): the layer at FLayerIndex in the layer list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);
  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Execute(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  LTextLayer.RichTextStream.Clear();

  if Assigned(FNewTextStream) then
  begin
    FNewTextStream.Position := 0;

    LTextLayer.RichTextStream.LoadFromStream(FNewTextStream);
    LTextLayer.RichTextStream.Position := 0;
  end;

  LTextLayer.UpdateContentToAssociatedTextEditor();
  LTextLayer.DrawTextOnLayer();

  if not LTextLayer.IsRenamed then
  begin
    LTextLayer.LayerName := LTextLayer.AssociatedTextEditor.Lines[0];
  end;
end;

procedure TgmEditTextOnExistedRichTextLayerCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := TgmRichTextLayer(FLayerList.Layers[FLayerIndex]);
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Rollback(): the layer at FLayerIndex in the layer list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);
  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmEditTextOnExistedRichTextLayerCommand.Rollback(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  LTextLayer.RichTextStream.Clear();
  if Assigned(FOldTextStream) then
  begin
    FOldTextStream.Position := 0;

    LTextLayer.RichTextStream.LoadFromStream(FOldTextStream);
    LTextLayer.RichTextStream.Position := 0;
  end;
  
  LTextLayer.UpdateContentToAssociatedTextEditor();
  LTextLayer.DrawTextOnLayer();

  if not LTextLayer.IsRenamed then
  begin
    LTextLayer.LayerName := LTextLayer.AssociatedTextEditor.Lines[0];
  end;
end;

{ TgmNewTextOnNewRichTextLayerCommand }

constructor TgmNewTextOnNewRichTextLayerCommand.Create(
  AChannelManager: TgmCustomChannelManager; ALayerList: TgmLayerList;
  ATextLayer: TgmRichTextLayer; const ATextLayerIndex: Integer);
begin
  if not Assigned(AChannelManager) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Create(): parameter AChannelManager is a nil pointer.');
  end;

  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Create(): parameter ALayerList is a nil pointer.');
  end;

  if not Assigned(ATextLayer) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Create(): parameter ATextLayer is a nil pointer.');
  end;

  if not ALayerList.IsValidIndex(ATextLayerIndex) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Create(): parameter ATextLayerIndex is out of range.');
  end;

  inherited Create('Type Tool');

  FChannelManager := AChannelManager;
  FLayerList      := ALayerList;
  FLayerIndex     := ATextLayerIndex;
  FLayerName      := ATextLayer.LayerName;
  FTextLayer      := TgmRichTextLayer( ATextLayer.GetCopy() );

  ChangeCommandIconByResourceName(TYPE_TOOL_ICON_RES_NAME);
end;

destructor TgmNewTextOnNewRichTextLayerCommand.Destroy;
begin
  FTextLayer.Free();
  inherited;
end;

procedure TgmNewTextOnNewRichTextLayerCommand.Execute;
var
  LDuplicatedLayer : TgmRichTextLayer;
  LLayerCounter    : Integer;
begin
  inherited;

  LDuplicatedLayer           := TgmRichTextLayer( FTextLayer.GetCopy() );
  LDuplicatedLayer.LayerName := Self.FLayerName;

  LLayerCounter := FLayerList.GetLayerCounter(TgmRichTextLayer);

  FLayerList.SimplyInsert(FLayerIndex, LDuplicatedLayer);
  FLayerList.SimplySelectLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmRichTextLayer, LLayerCounter + 1);
  FChannelManager.SelectColorChannel(0, True);
end;

procedure TgmNewTextOnNewRichTextLayerCommand.Rollback;
var
  LLayer        : TgmCustomLayer;
  LSelectIndex  : Integer;
  LLayerCounter : Integer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Rollback(): the FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmNewTextOnNewRichTextLayerCommand.Rollback(): the layer at FLayerIndex is not a rich-text layer.');
  end;

  LLayerCounter := FLayerList.GetLayerCounter(TgmRichTextLayer);

  FLayerList.SimplyDeleteLayer(FLayerIndex);
  FLayerList.SetLayerCounter(TgmRichTextLayer, LLayerCounter - 1);

  LSelectIndex := FLayerIndex - 1;
  if LSelectIndex < 0 then
  begin
    LSelectIndex := 0;
  end;
  
  FLayerList.SimplySelectLayer(LSelectIndex);
  FChannelManager.SelectColorChannel(0, True);
end;

{ TgmScaleTextRegionCommand }

constructor TgmScaleTextRegionCommand.Create(ALayerList: TgmLayerList;
  const ATextLayerIndex: Integer;
  const AOldTopLeft, AOldBottomRight: TPoint;
  const ANewTopLeft, ANewBottomRight: TPoint);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Create(): parameter ALayerList is a nil');
  end;

  if not ALayerList.IsValidIndex(ATextLayerIndex) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Create(): parameter ATextLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ATextLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Create(): the layer at ATextLayerIndex is not a rich-text layer.');
  end;

  inherited Create('Scale Text Region');
  
  FLayerList      := ALayerList;
  FLayerIndex     := ATextLayerIndex;
  FOldTopLeft     := AOldTopLeft;
  FOldBottomRight := AOldBottomRight;
  FNewTopLeft     := ANewTopLeft;
  FNewBottomRight := ANewBottomRight;
end;

procedure TgmScaleTextRegionCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Execute(): the layer at FLayerIndex in the list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);

  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Execute(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  with LTextLayer do
  begin
    BorderStart := FNewTopLeft;
    BorderEnd   := FNewBottomRight;
    
    LTextLayer.RichTextStream.Position := 0;
    AssociatedTextEditor.Lines.LoadFromStream(LTextLayer.RichTextStream);
    DrawTextOnLayer();
  end;
end;

procedure TgmScaleTextRegionCommand.Rollback;
var
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Rollback(): the layer at FLayerIndex in the list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);

  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmScaleTextRegionCommand.Rollback(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  with LTextLayer do
  begin
    BorderStart := FOldTopLeft;
    BorderEnd   := FOldBottomRight;
    
    LTextLayer.RichTextStream.Position := 0;
    AssociatedTextEditor.Lines.LoadFromStream(LTextLayer.RichTextStream);
    DrawTextOnLayer();
  end;
end;

{ TgmTranslateTextRegionCommand }

constructor TgmTranslateTextRegionCommand.Create(ALayerList: TgmLayerList;
  const ATextLayerIndex: Integer; const ATranslatedVector: TPoint);
var
  LLayer : TgmCustomLayer;
begin
  if not Assigned(ALayerList) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Create(): parameter ALayerList is a nil');
  end;

  if not ALayerList.IsValidIndex(ATextLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Create(): parameter ATextLayerIndex is out of range.');
  end;

  LLayer := ALayerList.Layers[ATextLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Create(): the layer at ATextLayerIndex is not a rich-text layer.');
  end;

  inherited Create('Move Text Region');
  
  FLayerList        := ALayerList;
  FLayerIndex       := ATextLayerIndex;
  FTranslatedVector := ATranslatedVector;

  ChangeCommandIconByResourceName(gmHistoryCommands.MOVE_COMMAND_ICON_RES_NAME);
end;

procedure TgmTranslateTextRegionCommand.Execute;
var
  LLayer     : TgmCustomLayer;
  LTextLayer : TgmRichTextLayer;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Execute(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Execute(): the layer at FLayerIndex in the list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);

  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Execute(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  with LTextLayer do
  begin
    LTextLayer.RichTextStream.Position := 0;
    AssociatedTextEditor.Lines.LoadFromStream(LTextLayer.RichTextStream);

    Translate(FTranslatedVector);
    DrawTextOnLayer();
  end;
end;

procedure TgmTranslateTextRegionCommand.Rollback;
var
  LLayer            : TgmCustomLayer;
  LTextLayer        : TgmRichTextLayer;
  LTranslatedVector : TPoint;
begin
  inherited;

  if not FLayerList.IsValidIndex(FLayerIndex) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Rollback(): FLayerIndex is out of range.');
  end;

  LLayer := FLayerList.Layers[FLayerIndex];
  if not (LLayer is TgmRichTextLayer) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Rollback(): the layer at FLayerIndex in the list is not a rich-text layer.');
  end;

  LTextLayer := TgmRichTextLayer(LLayer);

  if not Assigned(LTextLayer.AssociatedTextEditor) then
  begin
    raise Exception.Create('TgmTranslateTextRegionCommand.Rollback(): the rich-layer at FLayerIndex in the layer list has no associated text editor.');
  end;

  LTranslatedVector.X := -FTranslatedVector.X;
  LTranslatedVector.Y := -FTranslatedVector.Y;

  with LTextLayer do
  begin
    LTextLayer.RichTextStream.Position := 0;
    AssociatedTextEditor.Lines.LoadFromStream(LTextLayer.RichTextStream);

    Translate(LTranslatedVector);
    DrawTextOnLayer();
  end;
end;

end.
