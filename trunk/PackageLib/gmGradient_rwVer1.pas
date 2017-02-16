unit gmGradient_rwVer1;

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
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
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


type

  TgmGrdVer1_Reader = class(TgmConverter)
  public
    class function WantThis(const AStream: TStream): Boolean; override;
    procedure LoadFromStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure LoadItemFromStream(const AStream: TStream; const AGradient: TgmGradientItem); reintroduce;
  end;

  TgmGrdVer1_Writer = class(TgmConverter)
  protected
  public
    procedure SaveToStream(const AStream: TStream; const ACollection: TCollection); override;
    procedure SaveItemToStream(const AStream: TStream; const AGradient: TgmGradientItem); reintroduce;
  published
  end;

  TgmGradientInfoHeader = record
    Name           : ShortString;  // name of the gradients
    ColorStopCount : Cardinal;     // how many primary colors are in the file
    AlphaStopCount : Cardinal;     // how many primary alpha values are in the file
  end;


implementation

uses
  Graphics,GR32;
  
{ TgmGrdVer1_Reader }

procedure TgmGrdVer1_Reader.LoadFromStream(const AStream: TStream;
  const ACollection: TCollection);
var
  i           : Integer;
  LFileHeader : TgmGradientFileHeader;
  LGradients  : TgmGradientCollection;
begin
  LGradients := TgmGradientCollection(ACollection);

  // read in file header of old version of gradient file
  AStream.Read( LFileHeader, SizeOf(TgmGradientFileHeader) );

  if not ((LFileHeader.FileID = GRADIENT_FILE_ID) and
         (LFileHeader.FileVersion.AsLongWord = GRADIENT_FILE_VERSION)) then
  begin
    raise Exception.Create('Cannot open because the file is not supported by GraphicsMagic.');
  end;
  
  for i := 1 to LFileHeader.GradientCount do
  begin
    LoadItemFromStream(AStream, LGradients.Add);
  end;
end;

procedure TgmGrdVer1_Reader.LoadItemFromStream(const AStream: TStream;
  const AGradient: TgmGradientItem);
var
  LInfoHeader : TgmGradientInfoHeader;
  LStop       : TgmGradientStopItem;
  LColor      : TColor;
  LAlphaValue : Byte;
  LScale,LMid : Single;
  LMaxIndex   : Integer;
  i           : Integer;
begin
  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    // read in the info header
    AStream.Read( LInfoHeader, SizeOf(TgmGradientInfoHeader) );
    AGradient.DisplayName := LInfoHeader.Name;

    // read in data of color stops
    AGradient.RGBGradient.Clear;
    LMaxIndex := LInfoHeader.ColorStopCount - 1;

    for i := 0 to LMaxIndex do
    begin
      LStop := AGradient.RGBGradient.Add;

      AStream.Read(LColor, 4);  // read in color
      AStream.Read(LScale, 4);  // read in color location scale
      AStream.Read(LMid,   4);  // read in mid point scale

      LStop.Value         := LColor;
      LStop.LocationScale := LScale;
      LStop.MidPoint      := LMid;
    end;

    // read in data of alpha stops
    AGradient.AlphaGradient.Clear;
    LMaxIndex := LInfoHeader.AlphaStopCount - 1;

    for i := 0 to LMaxIndex do
    begin
      LStop := AGradient.AlphaGradient.Add;

      AStream.Read(LAlphaValue, 1);  // read in alpha value
      AStream.Read(LScale, 4);       // read in color location scale
      AStream.Read(LMid, 4);         // read in mid point scale

      LStop.Value         := (LAlphaValue shl 16) or (LAlphaValue shl 8) or LAlphaValue;
      LStop.LocationScale := LScale;
      LStop.MidPoint      := LMid;
    end;
  end;
end;

class function TgmGrdVer1_Reader.WantThis(const AStream: TStream): Boolean;
var
  LFileHeader: TgmGradientFileHeader;
begin
  //accept if signature is valid and Cardinal(version) = 1.
  AStream.Read(LFileHeader,SizeOf(LFileHeader)); 
  Result := (LFileHeader.FileID = GRADIENT_FILE_ID) and (LFileHeader.FileVersion.AsLongWord = 1);
end;

{ TgmGrdVer1_Writer }

procedure TgmGrdVer1_Writer.SaveItemToStream(const AStream: TStream;
  const AGradient: TgmGradientItem);
var
  LInfoHeader  : TgmGradientInfoHeader;
  LColor       : TColor;
  LScale       : Single;
  LAlphaValue  : Byte;
  i, LMaxIndex : Integer;
begin
  if Assigned(AStream) then
  begin
    // fill in info header...
    LInfoHeader.Name           := AGradient.DisplayName;
    LInfoHeader.ColorStopCount := AGradient.RGBGradient.Count;
    LInfoHeader.AlphaStopCount := AGradient.AlphaGradient.Count;

    // write in the data of info header
    AStream.Write( LInfoHeader, SizeOf(TgmGradientInfoHeader) );

    // Write in color stop data...
    
    if AGradient.RGBGradient.Count > 0 then
    begin
      LMaxIndex := AGradient.RGBGradient.Count - 1;

      for i := 0 to LMaxIndex do
      begin
        LColor := AGradient.RGBGradient[i].Value; //display color
        AStream.Write(LColor, SizeOf(TColor));

        LScale := AGradient.RGBGradient[i].LocationScale;
        AStream.Write(LScale, 4);

        LScale := AGradient.RGBGradient[i].MidPoint;
        AStream.Write(LScale, 4);
      end;
    end;

    // Write in alpha stop data...

    if AGradient.AlphaGradient.Count > 0 then
    begin
      LMaxIndex := AGradient.AlphaGradient.Count - 1;

      for i := 0 to LMaxIndex do
      begin
        LAlphaValue := AGradient.AlphaGradient[i].ByteValue;
        AStream.Write(LAlphaValue, 1);

        LScale := AGradient.AlphaGradient[i].LocationScale;
        AStream.Write(LScale, 4);

        LScale := AGradient.AlphaGradient[i].MidPoint;
        AStream.Write(LScale, 4);
      end;
    end;
  end;
end;

procedure TgmGrdVer1_Writer.SaveToStream(const AStream: TStream;
  const ACollection: TCollection );
var
  LFileHeader : TgmGradientFileHeader;
  LGradient   : TgmGradientItem;
  i           : Integer;
  LGradients  : TgmGradientCollection;
begin
  LGradients := TgmGradientCollection(ACollection);
  
  if Assigned(AStream) then
  begin
    // fill in file header
    LFileHeader.FileID                 := GRADIENT_FILE_ID;
    LFileHeader.FileVersion.AsLongWord := GRADIENT_FILE_VERSION;
    LFileHeader.GradientCount          := LGradients.Count;

    // write in file header
    AStream.Write( LFileHeader, SizeOf(TgmGradientFileHeader) );

    // write in data of color gradients
    for i := 0 to (LGradients.Count - 1) do
    begin
      LGradient := LGradients.Items[i];
      SaveItemToStream(AStream, LGradient);
    end;
  end;
end; 

initialization
  TgmGradientCollection.RegisterConverterReader('GRD', 'Graphics Magic Ver 1', 0, TgmGrdVer1_Reader);
  TgmGradientCollection.RegisterConverterWriter('GRD', 'Graphics Magic Ver 1', 0, TgmGrdVer1_Writer);

end.
