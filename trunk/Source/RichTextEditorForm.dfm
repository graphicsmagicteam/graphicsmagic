object frmRichTextEditor: TfrmRichTextEditor
  Left = 192
  Top = 132
  Width = 341
  Height = 212
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'GraphicsMagic Text Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object rchedtRichTextEditor: TRichEdit
    Left = 0
    Top = 0
    Width = 333
    Height = 149
    Align = alClient
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = rchedtRichTextEditorChange
    OnMouseUp = rchedtRichTextEditorMouseUp
  end
  object stsbrTextInfo: TStatusBar
    Left = 0
    Top = 149
    Width = 333
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
end
