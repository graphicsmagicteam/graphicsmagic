{ The GraphicsMagic -- an image manipulation program
  CopyRight(C) 2001-, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit gmGUIFuncs;

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

{$WARN UNSAFE_CODE OFF}

interface

uses
{ Standard }
  Windows, SysUtils, StdCtrls, ExtCtrls, Forms, Dialogs, Controls, Types,
  Classes, Graphics, ComCtrls,
{ Graphics32 }
  GR32, GR32_Image,
{ GraphicsMagic Lib }
  gmTypes;

{ procedure }

  procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage32); overload;
  procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage); overload;

  procedure ScaleImage32(const ASourceBmp: TBitmap32; AImage: TImage32;
    const AWidth, AHeight: Integer);

  procedure DrawRichTextOnBitmap(const ABitmap: TBitmap32; const ARect: TRect;
    ARichEdit: TRichEdit);

{ Functions }

  function GetScreenPixelsPerInch: Integer;

  function CheckIfEditEmpty(AEdit: TEdit): Boolean;
  
  // change cursor according to a given degrees
  function GetCursorByDegree(const ADeg: Extended): TCursor;

  // change cursor according to various handles
  function SetCursorByHandle(const ADrawingHandle: TgmDrawingHandle): TCursor;

  function GetBitmapTopLeftInImage32(const AImageControl: TCustomImage32): TPoint;

  function CheckInputNumberValid(ASender: TObject; var AKey: Word;
    AShift: TShiftState): Boolean;

  function CheckInputFloatNumberValid(ASender: TObject; var AKey: Word;
    AShift: TShiftState): Boolean;

  function CheckIfInRange(const ACheckValue, AMinValue, AMaxValue: Integer): Boolean;

  function PrintRTFToCanvas(ACanvas: TCanvas; ARichEdit: TRichEdit;
    const ARect: TRect): Longint;

  function PrintRTFToBitmap(ARichEdit: TRichEdit; ABitmap: TBitmap): Longint;

const
  SMALL_THUMBNAIL_SIZE: Integer = 32;
  LARGE_THUMBNAIL_SIZE: Integer = 64;

  { custom cursor indices }
  crPathComponentSelection = 1;
  crDirectSelection        = 2;
  crPenToolDeselected      = 3;
  crPenToolSelected        = 4;
  crAddAnchorPoint         = 5;
  crDeleteAnchorPoint      = 6;
  crMovePath               = 7;
  crConvertPoint           = 8;
  crClosePath              = 9;
  crHandLoosen             = 10;
  crHandGrip               = 11;
  crPaintBucket            = 12;
  crEyedropper             = 13;
  crPenToolLastEnd         = 14;
  crAddCornerPoint         = 15;
  crCloneStamp             = 16;
  crCrossAdd               = 17;
  crCrossSub               = 18;
  crCrossIntersect         = 19;
  crCrossInterSub          = 20;
  crPolygonSelection       = 21;
  crPolygonAdd             = 23;
  crPolygonSub             = 24;
  crPolygonIntersect       = 26;
  crPolygonInterSub        = 27;
  crLassoSelection         = 28;
  crLassoAdd               = 29;
  crLassoSub               = 30;
  crLassoIntersect         = 31;
  crLassoInterSub          = 32;
  crMagicWand              = 33;
  crMagicWandAdd           = 34;
  crMagicWandSub           = 35;
  crMagicWandIntersect     = 36;
  crMagicWandInterSub      = 37;
  crMoveSelection          = 38;
  crCrop                   = 39;
  crMeasure                = 40;
  crMeasureMove            = 41;
  crMeasureAngle           = 42;
  crMagicEraser            = 43;

  // for transformation -- rotation
  crRotatePointer1         = 44;
  crRotatePointer2         = 45;
  crRotatePointer3         = 46;
  crRotatePointer4         = 47;
  crRotatePointer5         = 48;
  crRotatePointer6         = 49;
  crRotatePointer7         = 50;
  crRotatePointer8         = 51;

  crBlackPicker            = 52;
  crGrayPicker             = 53;
  crWhitePicker            = 54;
  crMagneticLasso          = 55;

implementation

uses
  RichEdit,              // TFormatRange
  gmImageProcessFuncs;   // GetScaledDimension()

{$R GMToolsCursors.res}
{$R GMToolsCursors2.res}
{$R GMToolsCursors3.res}

var
  GMaskRichEdit: TRichEdit;

procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage32);
begin
  if (APanel.Width  >= AImage.Width) and
     (APanel.Height >= AImage.Height) then
  begin
    AImage.Left := APanel.Width  div 2 - AImage.Width  div 2;
    AImage.Top  := APanel.Height div 2 - AImage.Height div 2;
  end;
end;

procedure CenterImageInPanel(const APanel: TPanel; const AImage: TImage);
begin
  if (APanel.Width  >= AImage.Width) and
     (APanel.Height >= AImage.Height) then
  begin
    AImage.Left := APanel.Width  div 2 - AImage.Width  div 2;
    AImage.Top  := APanel.Height div 2 - AImage.Height div 2;
  end;
end;

procedure ScaleImage32(const ASourceBmp: TBitmap32; AImage: TImage32;
  const AWidth, AHeight: Integer);
var
  sw, sh: Integer;
begin
  if AImage.AutoSize then
  begin
    AImage.AutoSize := False;
  end;

  if AImage.ScaleMode <> smStretch then
  begin
    AImage.ScaleMode := smStretch;
  end;

  GetScaledDimension(ASourceBmp.Width, ASourceBmp.Height, AWidth, AHeight, sw, sh);

  AImage.Width  := sw;
  AImage.Height := sh;

  AImage.Bitmap.Assign(ASourceBmp);
end;

procedure DrawRichTextOnBitmap(const ABitmap: TBitmap32; const ARect: TRect;
  ARichEdit: TRichEdit);
var
  MaskBitmap      : TBitmap32;
  RichTextStream  : TMemoryStream;
  i, j            : Integer;
  DestBit, MaskBit: PColor32Array;
  mr, mg, mb      : Byte;
  na              : Cardinal;
  AColor          : TColor32;
begin
{$RANGECHECKS OFF}

  MaskBitmap := TBitmap32.Create;
  try
    MaskBitmap.DrawMode := dmBlend;
    MaskBitmap.SetSize(ABitmap.Width, ABitmap.Height);
    MaskBitmap.Clear(clWhite32);

    RichTextStream := TMemoryStream.Create;
    try
      ARichEdit.Lines.SaveToStream(RichTextStream);
      RichTextStream.Position := 0;

      with GMaskRichEdit do
      begin
        GMaskRichEdit.Parent := ARichEdit.Parent;
        Lines.LoadFromStream(RichTextStream);
        SelectAll;
        SelAttributes.Color := clBlack;
        SelLength := 0;
      end;
    finally
      RichTextStream.Free;
    end;

    PrintRTFToCanvas(ABitmap.Canvas, ARichEdit, ARect);
    PrintRTFToCanvas(MaskBitmap.Canvas, GMaskRichEdit, ARect);

    for j := 0 to ABitmap.Height - 1 do
    begin
      if (j < ARect.Top) or (j > ARect.Bottom) then
      begin
        Continue;
      end;

      DestBit := ABitmap.ScanLine[j];
      MaskBit := MaskBitmap.ScanLine[j];

      for i := 0 to ABitmap.Width - 1 do
      begin
        if (i < ARect.Left) or (i > ARect.Right) then
        begin
          Continue;
        end;

        mr := MaskBit[i] shr 16 and $FF;
        mg := MaskBit[i] shr  8 and $FF;
        mb := MaskBit[i]        and $FF;
        na := 255 - (mr + mg + mb) div 3;

        AColor     := DestBit[i] and $00FFFFFF;
        DestBit[i] := (na shl 24) or AColor;
      end;
    end;
  finally
    MaskBitmap.Free;
  end;

{$RANGECHECKS ON}
end;

function GetScreenPixelsPerInch: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function CheckIfEditEmpty(AEdit: TEdit): Boolean;
begin
  if AEdit.Text = '' then
  begin
    Result := True;
    MessageDlg('Empty value is a invalid value', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := False;
  end;
end;

// change cursor according to a given degrees
function GetCursorByDegree(const ADeg: Extended): TCursor;
begin
  Result := crDefault;
  
  if ADeg < 0 then
  begin
    if ADeg >= (-15.0) then
    begin
      Result := crRotatePointer1;
    end
    else
    if ( ADeg > (-75.0) ) and ( ADeg < (-15.0) ) then
    begin
      Result := crRotatePointer2;
    end
    else
    if ( ADeg >= (-105.0) ) and ( ADeg <= (-75.0) ) then
    begin
      Result := crRotatePointer3;
    end
    else
    if ( ADeg > (-165.0) ) and ( ADeg < (-105.0) ) then
    begin
      Result := crRotatePointer4;
    end
    else
    if ( ( ADeg >= (-180.0) ) and ( ADeg <= (-165.0) ) ) then
    begin
      Result := crRotatePointer5;
    end;
  end
  else if ADeg >= 0 then
  begin
    if ADeg <= 15.0 then
    begin
      Result := crRotatePointer1;
    end
    else
    if (ADeg < 75.0) and (ADeg > 15.0) then
    begin
      Result := crRotatePointer8;
    end
    else
    if (ADeg <= 105.0) and (ADeg >= 75.0) then
    begin
      Result := crRotatePointer7;
    end
    else
    if (ADeg < 165.0) and (ADeg > 105.0) then
    begin
      Result := crRotatePointer6;
    end
    else
    if (ADeg <= 180.0) and (ADeg >= 165.0) then
    begin
      Result := crRotatePointer5;
    end;
  end;
end;

// change cursor according to various handles
function SetCursorByHandle(const ADrawingHandle: TgmDrawingHandle): TCursor;
begin
  Result := crDefault;

  if (ADrawingHandle = dhAxAy) or (ADrawingHandle = dhBxBy) then
  begin
    Result := crSizeNWSE;
  end
  else
  if (ADrawingHandle = dhAxBy) or (ADrawingHandle = dhBxAy) then
  begin
    Result := crSizeNESW;
  end
  else
  if (ADrawingHandle = dhTopHalfAxBx) or (ADrawingHandle = dhBottomHalfAxBx) then
  begin
    Result := crSizeNS;
  end
  else
  if (ADrawingHandle = dhLeftHalfAyBy) or (ADrawingHandle = dhRightHalfAyBy) then
  begin
    Result := crSizeWE;
  end
  else
  if (ADrawingHandle in [dhLineStart, dhLineEnd, dhCurvePoint1, dhCurvePoint2, dhPolygonPoint]) then
  begin
    Result := crSizeAll;
  end
  else
  if (ADrawingHandle = dhNone) then
  begin
    Result := crDefault;
  end;
end;

function GetBitmapTopLeftInImage32(const AImageControl: TCustomImage32): TPoint;
var
  LRect: TRect;
begin
  // get the location of the bitmap in Image32
  LRect := AImageControl.GetBitmapRect;       
  
  // convert the upper left coordinate from image32 coordinate to bitmap coordinate
  Result := AImageControl.ControlToBitmap( Point(LRect.Left, LRect.Top) );
end;

function CheckInputNumberValid(ASender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
begin
  if ( not (AKey in [Ord('0')..Ord('9')]) ) and
     ( not (AKey in [VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3,
                     VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7,
                     VK_NUMPAD8, VK_NUMPAD9, VK_RETURN,  VK_BACK,
                     VK_TAB,     VK_LEFT,    VK_RIGHT,   VK_UP,
                     VK_DOWN,    VK_DELETE,  VK_NUMLOCK]) ) then
  begin
    Result := False;
    MessageDlg('Please input a number!', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := True;
  end;
end;

function CheckInputFloatNumberValid(ASender: TObject; var AKey: Word;
  AShift: TShiftState): Boolean;
begin
  // ASCII code for decimal point is #190
  if ( not (AKey in [Ord('0')..Ord('9'), Ord(#190)]) ) and
     ( not (AKey in [VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3,
                     VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7,
                     VK_NUMPAD8, VK_NUMPAD9, VK_RETURN,  VK_BACK,
                     VK_TAB,     VK_LEFT,    VK_RIGHT,   VK_UP,
                     VK_DOWN,    VK_DELETE,  VK_NUMLOCK]) ) then
  begin
    Result := False;
    MessageDlg('Please input a number!', mtInformation, [mbOK], 0);
  end
  else
  begin
    Result := True;
  end;
end;

function CheckIfInRange(const ACheckValue, AMinValue, AMaxValue: Integer): Boolean;
begin
  if (ACheckValue >= AMinValue) and (ACheckValue <= AMaxValue) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
    
    MessageDlg(Format('The value should be in from %d to %d!',
               [AMinValue, AMaxValue]), mtInformation, [mbOK], 0);
  end
end;

//------------------------------------------------------------------------------

{ The following routines (PrintRTFToCanvas and PrintRTFToBitmap) are found from:
  http://members.chello.be/ws36637/printpreview.html

  Original author: Serhiy Perevoznyk ( serge_perevoznyk@hotmail.com )

  The Rich Edit control (we are talking about standard Windows control,
  not a Delphi component) contains built-in printing features that can be used
  to send formatted text to the printer or to paint it's content onto any canvas
  with minimal effort from the programmer.

  Of course, the standard Delphi TRichEdit component incapsulates this feature.
  We can use this posibility to make a fast print preview with a scaling or
  drawing Rich Text on any Delphi control.

  Drawing from a Rich Edit control to any canvas involves the use of the
  standard Rich Edit control message EM_FORMATRANGE.

  The lParam parameter for this message is a pointer to the TFormatRange record.
  This record have to be filled before sending the message to the RichEdit.

  The TFORMATRANGE record contains information that a rich edit control uses to
  format its output for a particular device, where

  hdc -- Device to render to.
  hdcTarget -- Target device to format for.

  rc -- Area to render to. Units are measured in twips. Twips are
        screen-independent units to ensure that the proportion of screen
        elements are the same on all display systems. A twip is defined as
        being 1/1440 of an inch.
        
  rcPage -- Entire area of rendering device. Units are measured in twips.
  chrg -- TCHARRANGE record that specifies the range of text to format.

  This record usually is used with the EM_EXGETSEL and EM_EXSETSEL messages and
  includes two fields: cpMin and cpMax.

  cpMin is a character position index immediately preceding the first character
  in the range.
  
  cpMax is a character position immediately following the last character in the
  range.
}

function PrintRTFToCanvas(ACanvas: TCanvas; ARichEdit: TRichEdit;
  const ARect: TRect): Longint;
var
  LRange: TFormatRange;
begin
  FillChar( LRange, SizeOf(TFormatRange), 0 );

  // rendering to the same DC we are measuring
  LRange.hdc       := ACanvas.handle;
  LRange.hdcTarget := ACanvas.Handle;

  // set up the page
  LRange.rc.left   := ARect.Left   * 1440 div Screen.PixelsPerInch;
  LRange.rc.top    := ARect.Top    * 1440 div Screen.PixelsPerInch;
  LRange.rc.right  := ARect.Right  * 1440 div Screen.PixelsPerInch;
  LRange.rc.Bottom := ARect.Bottom * 1440 div Screen.PixelsPerInch;

  // default the range of text to print as the entire document
  LRange.chrg.cpMax := -1;
  LRange.chrg.cpMin := 0;

  // format the text
  Result := SendMessage( ARichEdit.Handle, EM_FORMATRANGE, 1, Longint(@LRange) );

  // free cached information
  SendMessage(ARichEdit.handle, EM_FORMATRANGE, 0, 0);
end;

function PrintRTFToBitmap(ARichEdit: TRichEdit; ABitmap: TBitmap): Longint;
var
  LRange: TFormatRange;
begin
  FillChar(LRange, SizeOf(TFormatRange), 0);

  // rendering to the same DC we are measuring
  LRange.hdc       := ABitmap.Canvas.handle;
  LRange.hdcTarget := ABitmap.Canvas.Handle;

  // set up the page
  LRange.rc.left   := 0;
  LRange.rc.top    := 0;
  LRange.rc.right  := ABitmap.Width * 1440 div Screen.PixelsPerInch;
  LRange.rc.Bottom := ABitmap.Height * 1440 div Screen.PixelsPerInch;

  // default the range of text to print as the entire document
  LRange.chrg.cpMax := -1;
  LRange.chrg.cpMin := 0;

  // format the text
  Result := SendMessage(ARichEdit.Handle, EM_FORMATRANGE, 1, Longint(@LRange));

  // free cached information
  SendMessage(ARichEdit.handle, EM_FORMATRANGE, 0, 0);
end;

procedure GlobalInitialize;
begin
  // loading custom cursors from resources
  Screen.Cursors[crPathComponentSelection] := LoadCursor(hInstance, 'PATHCOMPONENTSELECTIONTOOL');
  Screen.Cursors[crDirectSelection]        := LoadCursor(hInstance, 'DIRECTSELECTIONTOOL');
  Screen.Cursors[crPenToolDeselected]      := LoadCursor(hInstance, 'PENTOOLDESELECTED');
  Screen.Cursors[crPenToolSelected]        := LoadCursor(hInstance, 'PENTOOL');
  Screen.Cursors[crAddAnchorPoint]         := LoadCursor(hInstance, 'ADDANCHORPOINT');
  Screen.Cursors[crDeleteAnchorPoint]      := LoadCursor(hInstance, 'DELETEANCHORPOINT');
  Screen.Cursors[crMovePath]               := LoadCursor(hInstance, 'MOVEPATH');
  Screen.Cursors[crConvertPoint]           := LoadCursor(hInstance, 'CONVERTPOINT');
  Screen.Cursors[crClosePath]              := LoadCursor(hInstance, 'CLOSEPATH');
  Screen.Cursors[crPenToolLastEnd]         := LoadCursor(hInstance, 'PENTOOLLASTEND');
  Screen.Cursors[crAddCornerPoint]         := LoadCursor(hInstance, 'ADDCORNERPOINT');
  Screen.Cursors[crPaintBucket]            := LoadCursor(hInstance, 'PAINTBUCKET');
  Screen.Cursors[crEyedropper]             := LoadCursor(hInstance, 'EYEDROPPER');
  Screen.Cursors[crHandLoosen]             := LoadCursor(hInstance, 'HANDLOOSEN');
  Screen.Cursors[crHandGrip]               := LoadCursor(hInstance, 'HANDGRIP');
  Screen.Cursors[crCloneStamp]             := LoadCursor(hInstance, 'CLONESTAMP');
  Screen.Cursors[crCrossAdd]               := LoadCursor(hInstance, 'CROSSADD');
  Screen.Cursors[crCrossSub]               := LoadCursor(hInstance, 'CROSSSUB');
  Screen.Cursors[crCrossIntersect]         := LoadCursor(hInstance, 'CROSSINTERSECT');
  Screen.Cursors[crCrossInterSub]          := LoadCursor(hInstance, 'CROSSINTERSUB');
  Screen.Cursors[crPolygonSelection]       := LoadCursor(hInstance, 'POLYGONSELECTION');
  Screen.Cursors[crPolygonAdd]             := LoadCursor(hInstance, 'POLYGONADD');
  Screen.Cursors[crPolygonSub]             := LoadCursor(hInstance, 'POLYGONSUB');
  Screen.Cursors[crPolygonIntersect]       := LoadCursor(hInstance, 'POLYGONINTERSECT');
  Screen.Cursors[crPolygonInterSub]        := LoadCursor(hInstance, 'POLYGONINTERSUB');
  Screen.Cursors[crLassoSelection]         := LoadCursor(hInstance, 'LASSOSELECTION');
  Screen.Cursors[crLassoAdd]               := LoadCursor(hInstance, 'LASSOADD');
  Screen.Cursors[crLassoSub]               := LoadCursor(hInstance, 'LASSOSUB');
  Screen.Cursors[crLassoIntersect]         := LoadCursor(hInstance, 'LASSOINTERSECT');
  Screen.Cursors[crLassoInterSub]          := LoadCursor(hInstance, 'LASSOINTERSUB');
  Screen.Cursors[crMagicWand]              := LoadCursor(hInstance, 'MAGICWAND');
  Screen.Cursors[crMagicWandAdd]           := LoadCursor(hInstance, 'MAGICWANDADD');
  Screen.Cursors[crMagicWandSub]           := LoadCursor(hInstance, 'MAGICWANDSUB');
  Screen.Cursors[crMagicWandIntersect]     := LoadCursor(hInstance, 'MAGICWANDINTERSECT');
  Screen.Cursors[crMagicWandInterSub]      := LoadCursor(hInstance, 'MAGICWANDINTERSUB');
  Screen.Cursors[crMoveSelection]          := LoadCursor(hInstance, 'MOVESELECTION');
  Screen.Cursors[crCrop]                   := LoadCursor(hInstance, 'CROP');
  Screen.Cursors[crMeasure]                := LoadCursor(hInstance, 'MEASURE');
  Screen.Cursors[crMeasureMove]            := LoadCursor(hInstance, 'MEASUREMOVE');
  Screen.Cursors[crMeasureAngle]           := LoadCursor(hInstance, 'MEASUREANGLE');
  Screen.Cursors[crMagicEraser]            := LoadCursor(hInstance, 'MAGICERASER');

  // for transformation -- rotation
  Screen.Cursors[crRotatePointer1]         := LoadCursor(hInstance, 'ROTATEPOINTER1');
  Screen.Cursors[crRotatePointer2]         := LoadCursor(hInstance, 'ROTATEPOINTER2');
  Screen.Cursors[crRotatePointer3]         := LoadCursor(hInstance, 'ROTATEPOINTER3');
  Screen.Cursors[crRotatePointer4]         := LoadCursor(hInstance, 'ROTATEPOINTER4');
  Screen.Cursors[crRotatePointer5]         := LoadCursor(hInstance, 'ROTATEPOINTER5');
  Screen.Cursors[crRotatePointer6]         := LoadCursor(hInstance, 'ROTATEPOINTER6');
  Screen.Cursors[crRotatePointer7]         := LoadCursor(hInstance, 'ROTATEPOINTER7');
  Screen.Cursors[crRotatePointer8]         := LoadCursor(hInstance, 'ROTATEPOINTER8');

  Screen.Cursors[crBlackPicker]            := LoadCursor(hInstance, 'BLACKPICKER');
  Screen.Cursors[crGrayPicker]             := LoadCursor(hInstance, 'GRAYPICKER');
  Screen.Cursors[crWhitePicker]            := LoadCursor(hInstance, 'WHITEPICKER');

  Screen.Cursors[crMagneticLasso]          := LoadCursor(hInstance, 'MAGNETICLASSO');

  GMaskRichEdit            := TRichEdit.Create(nil);
  GMaskRichEdit.Parent     := nil;
  GMaskRichEdit.ScrollBars := ssVertical;
  GMaskRichEdit.Visible    := False;
end;

procedure GlobalFinalize;
begin
  FreeAndNil(GMaskRichEdit);
end; 

initialization
  GlobalInitialize;

finalization


end.
