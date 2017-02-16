object frmLicence: TfrmLicence
  Left = 193
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Licence'
  ClientHeight = 272
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object mmLicence: TMemo
    Left = 0
    Top = 0
    Width = 342
    Height = 217
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'GraphicsMagic is a free software; you can redistribute it and/or' +
        ' '
      'modify it under the terms of the GNU General Public Licence '
      'as published by the Free Software Foundation; either version 2 '
      'of the Licence, or (at your option) any later version.'
      ''
      
        'GraphicsMagic is distributed in the hope that it will be useful,' +
        ' '
      'but WITHOUT ANY WARRANTY; without even the implied '
      'warranty of MERCHANTABILITY or FITNESS FOR A '
      'PARTICULAR PURPOSE.  See the GNU General Public '
      'Licence for more details.'
      ''
      'You should have received a copy of the GNU General Public '
      'Licence along with GraphicsMagic; if not, write to the Free '
      'Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, '
      'MA 02111-1307, USA.')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 120
    Top = 232
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
