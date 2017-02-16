unit gmGradient_rwPegtopNew;

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
 * The Initial Developer of this unit are
 *
 * x2nie - Fathony Luthfillah  <x2nie@yahoo.com>
 *
 * -----------------------------------------------------------------------------
 * Original unit is PegtopColorGradients.pas, PegtopColorGradientLists.pas
 * PegtopCommon Library therefore they are
 * developed by Jens Gruschel (GRU)
 * Copyright:  (c) 2005 Jens Gruschel
 * Website:    http://www.pegtop.net/delphi
 *
 *
 * -----------------------------------------------------------------------------
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
 *
 * =============================================================================
 * Pegtop Delpi Component Library - license.txt :


Pegtop Delphi Component Library

Copyright 2004, 2005 by Jens Gruschel
For more information visit http://www.pegtop.net/delphi
Original date of publication: 01 Nov 2004
Latest change:                15 Aug 2005

________________________________________________________________________________


LICENSE

________________________________________________________________________________


1. This notice may not be removed from or altered in any source distribution.

  "Author" herein refers to Jens Gruschel (the creator of this library).
  "Software" refers to all files included with this library.

________________________________________________________________________________


2. This software contains (at least) following source code files:

   PegtopAlarmSchedules.pas
   PegtopBroadcasts.pas
   PegtopChunkFiles.pas
   PegtopColorControls.pas
   PegtopColorDialogs.pas
   PegtopColorFilters.pas
   PegtopColorGradientBars.pas
   PegtopColorGradientControls.pas
   PegtopColorGradientDialogs.pas
   PegtopColorGradientEditors.pas
   PegtopColorGradientFileDialogs.pas
   PegtopColorGradientListBoxes.pas
   PegtopColorGradientLists.pas
   PegtopColorGradients.pas
   PegtopColorServices.pas
   PegtopColorUtils.pas
   PegtopComboBoxes.pas
   PegtopCommonReg.pas
   PegtopControlEdits.pas
   PegtopCPUUtils.pas
   PegtopCursors.pas
   PegtopDesktopMagnifiers.pas
   PegtopFileDialogs.pas
   PegtopFileVersionInfo.pas
   PegtopFireButtons.pas
   PegtopFormMagnets.pas
   PegtopGraphicFileDialogs.pas
   PegtopHashes.pas
   PegtopLinks.pas
   PegtopMessageReceivers.pas
   PegtopMethodHashes.pas
   PegtopMutexSections.pas
   PegtopNetworkTimeSockets.pas
   PegtopNumEdits.pas
   PegtopNumForms.pas
   PegtopPasswordEdits.pas
   PegtopPriorityQueues.pas
   PegtopProgressBars.pas
   PegtopQualityLabels.pas
   PegtopRadioGroups.pas
   PegtopResampleUtils.pas
   PegtopScrollers.pas
   PegtopSharedMemory.pas
   PegtopSystemImages.pas
   PegtopSysUtils.pa
   PegtopThemes.pas
   PegtopThumbnails.pas
   PegtopTimeUtils.pas
   PegtopTrackBars.pas
   PegtopWindowHooks.pa
   
   
________________________________________________________________________________


3. This software is distributed as a freeware. You are free to use it as part of
   your application for any purpose including freeware, commercial and shareware
   applications, provided some credit is given to the author.

   The origin of this software must not be misrepresented; you must not claim
   your authorship. All redistributions must retain the original copyright
   notice and web site addresses.

   Commercial redistribution of the library source is allowed only with an
   explicit written permission from the author.

   This software is provided 'as-is', without warranty of any kind, either
   expressed or implied. In no event shall the author be held liable for any
   damages arising from the use of this software.

________________________________________________________________________________


Jens Gruschel

www.pegtop.net/delphi

 * ***** END LICENSE BLOCK ***** *)
 
interface

uses
{ Standard }
  Types, Classes, SysUtils, Graphics,
{ Externals }
  PegtopChunkFiles, PegtopColorGradients,
{ GraphicsMagic }
  gmFileFormatList, gmGradient;

type
  TxgrPegtopNewReader = class(TgmConverter)
  private
  public
    //constructor Create; override;
    class function WantThis(const AStream: TStream): Boolean; override;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure ParseItem(gmItem: TgmGradientItem; ptItem: TPegtopCustomColorGradient);
  end;

const
  //GradientCollectionChunkId: TPegtopChunkId = 'XGRC';
  GradientCollectionDWord  = $43524758; //XGRC
  {GradientChunkId:           TPegtopChunkId = 'XGRD';
  GradientDataChunkId:       TPegtopChunkId = 'GDAT';
  ColorChunkId:              TPegtopChunkId = 'CDEF';
  OpacityChunkId:            TPegtopChunkId = 'ODEF';
  ColorDataChunkId:          TPegtopChunkId = 'CDAT';
  OpacityDataChunkId:        TPegtopChunkId = 'ODAT';
  ColorNoiseChunkId:         TPegtopChunkId = 'CNOI';
  OpacityNoiseChunkId:       TPegtopChunkId = 'ONOI';
  ColorNoiseDataChunkId:     TPegtopChunkId = 'CNDT';
  OpacityNoiseDataChunkId:   TPegtopChunkId = 'ONDT';
  KeysChunkId:               TPegtopChunkId = 'KEYS';
  ColorKeyChunkId:           TPegtopChunkId = 'CKEY';
  OpacityKeyChunkId:         TPegtopChunkId = 'OKEY';

  GradientCollectionDataChunkId: TPegtopChunkId = 'COLD';
  GradientsChunkId:              TPegtopChunkId = 'GRDS';
  }
  
implementation

uses
  PegtopColorGradientLists;

{ TxgrPegtopNewReader }

procedure TxgrPegtopNewReader.LoadFromStream(const AStream: TStream;
  const ACollection: TCollection);
var
  LPGC : TPegtopColorGradientCollection;
  LPGI : TPegtopColorGradientItem;
  LPGO : TPegtopCustomColorGradient;
  i    : Integer;
begin
  LPGC := TPegtopColorGradientCollection.Create(nil,nil);
  LPGC.LoadFromStream(AStream);
  
  for i := 0 to (LPGC.Count - 1) do
  begin
    LPGI := LPGC[i];
    LPGO := LPGI.Gradient;
    
    ParseItem(TgmGradientCollection(ACollection).Add,LPGO);
  end;

  LPGC.Free;
end;

procedure TxgrPegtopNewReader.ParseItem(gmItem: TgmGradientItem;
  ptItem: TPegtopCustomColorGradient);
var
  i,j     : Integer;
  LgmStop : TgmGradientStopItem;
begin
  gmItem.Clear;
  gmItem.DisplayName := ptItem.Name;
  for i := 0 to ptItem.Color.Keys.Count -1 do
  begin
    LgmStop := gmItem.RGBGradient.Add;
    LgmStop.LocationScale := ptItem.Color.Keys[i].Position * 0.001;
    LgmStop.Value := ptItem.Color.Keys[i].Color;
  end;

  for i := 0 to ptItem.Opacity.Keys.Count -1 do
  begin
    LgmStop := gmItem.AlphaGradient.Add;
    j := ptItem.Opacity.Keys[i].Opacity;
    //j := round(ptItem.Opacity.Keys[i].Opacity * 0.01 * 255);
    LgmStop.Value := j-1;
    LgmStop.LocationScale := ptItem.Opacity.Keys[i].Position * 0.001;
  end;
end;

class function TxgrPegtopNewReader.WantThis(const AStream: TStream): Boolean;
var
  LFileHeader : TgmGradientFileHeader;
begin
  AStream.Read( LFileHeader, SizeOf(LFileHeader) ); 

  Result := (LFileHeader.FileID = GradientCollectionDWord) ;  //XGRC
end;

initialization
  TgmGradientCollection.RegisterConverterReader('XGR', 'Pegtop gradient files', 0, TxgrPegtopNewReader);


end.
