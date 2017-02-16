//old version of gradient file

unit gmGradient_rwUnversioned;

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

// Update Date: 2017/01/23

interface

uses
{ Standard }
  Classes, SysUtils,
{ GraphicsMagic }
  gmFileFormatList, gmGradient;

const
  GRD_UNVERSIONED_ID = $474D470E; // old unversioned graphics magic gradient

  // for load in old version of GraphicsMagic's gradient files
  OLD_GRADIENT_ID   = 'GMGradientFile';
  OLD_GRADIENT_MODE = 'Average';

type

  TgmOldReader = class(TgmConverter)
  public
    //class function FileSignature : DWORD; override;
    class function WantThis(const AStream: TStream): Boolean; override;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure LoadItemFromStream(const AStream: TStream; const AGradient: TgmGradientItem); reintroduce;
  end;

  // for load in old version of GraphicsMagic's gradient files
  TgmOldGradientFileHeader = record
    Info          : string[20];
    GradientCount : Integer;
    GradientMode  : string[20];
  end;

  TgmOldGradientInfoHeader = record
    Name       : ShortString;
    ColorCount : Integer;
  end;


implementation

uses
 Graphics, GR32;


{ TgmOldReader }

procedure TgmOldReader.LoadFromStream(const AStream: TStream;
  const ACollection: TCollection);
var
  i              : Integer;
  LOldFileHeader : TgmOldGradientFileHeader;
  LGradients     : TgmGradientCollection;
begin
  LGradients := TgmGradientCollection(ACollection);

  // read in file header of old version of gradient file
  AStream.Read( LOldFileHeader, SizeOf(TgmOldGradientFileHeader) );

  if not ((LOldFileHeader.Info = OLD_GRADIENT_ID) and
          (LOldFileHeader.GradientMode = OLD_GRADIENT_MODE)) then
  begin
    raise Exception.Create('Cannot open because the file is not supported by GraphicsMagic.');
  end;
  
  for i := 1 to LOldFileHeader.GradientCount do
    LoadItemFromStream(AStream, LGradients.Add);
end; { LoadGradientsOldVersion }


procedure TgmOldReader.LoadItemFromStream(const AStream: TStream;
  const AGradient: TgmGradientItem);
var
  LGradientInfo : TgmOldGradientInfoHeader;
  LStop         : TgmGradientStopItem;
  LColorArray   : array of TColor;
  j             : Integer;
begin
  AStream.Read( LGradientInfo, SizeOf(TgmOldGradientInfoHeader) );

  AGradient.RGBGradient.Clear;

  if LGradientInfo.ColorCount > 0 then
  begin
    AGradient.DisplayName := LGradientInfo.Name;

    //load colors from stream
    SetLength(LColorArray,LGradientInfo.ColorCount);
    AStream.Read( LColorArray[0], SizeOf(TColor) * LGradientInfo.ColorCount );

    //actual create items
    for j := 0 to Length(LColorArray) -1 do
    begin
      LStop               := AGradient.RGBGradient.Add;
      LStop.LocationScale := (j)/ (Length(LColorArray)-1);
      LStop.Value         := {Color32}(LColorArray[j]);
    end;
  end;
end;

class function TgmOldReader.WantThis(const AStream: TStream): Boolean;
var
  LFileHeader: TgmGradientFileHeader;
begin
  AStream.Read(LFileHeader,SizeOf(LFileHeader)); 
  Result := LFileHeader.FileID = GRD_UNVERSIONED_ID; //only check the MagicWord.
end;

initialization
  TgmGradientCollection.RegisterConverterReader('GRD','OldFirst Version',0, TgmOldReader);

end.
