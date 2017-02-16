object frmCredits: TfrmCredits
  Left = 193
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Credits'
  ClientHeight = 390
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 341
    Height = 337
    ActivePage = tbshtSpecialThanks
    Align = alTop
    TabOrder = 0
    object tbshtSpecialThanks: TTabSheet
      Caption = 'Special Thanks'
      object lstbxSpecialThanks: TListBox
        Left = 0
        Top = 0
        Width = 333
        Height = 309
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'Anders Melander'
          'Andre Felix Miertschink'
          'Babak Sateli'
          'Christian-W. Budde'
          'cybershaman'
          'Daniel Lopes'
          'Earl F. Glynn'
          'Fancesco Savastano'
          'Gerd Platl'
          'GGCat (nickname)'
          'Hao Zhu'
          'Jan Verhoeven'
          'Jean Yves Queinec'
          'Jens Gruschel'
          'Kambiz R. Khojasteh'
          'Mattias Andersson (Graphics32 Team)'
          'Michael Hansen (Graphics32 Team)'
          'Nils Haeck ( SimDesign B.V. at http://www.simdesign.nl )'
          'Rustam Kafarov'
          'Wolfgang Krug'
          'Xiong Wei'
          'Zoltan Komaromy')
        Sorted = True
        TabOrder = 0
      end
    end
    object tbshtWrittenBy: TTabSheet
      Caption = 'Written by'
      ImageIndex = 1
      object lstbxWrittenBy: TListBox
        Left = 0
        Top = 0
        Width = 333
        Height = 309
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'Ma Xiaoguang'
          'Ma Xiaoming'
          'Gerd Platl'
          'Tommi Prami'
          'x2nie - Fathony Luthfillah')
        TabOrder = 0
      end
    end
  end
  object btbtnOK: TBitBtn
    Left = 128
    Top = 352
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
