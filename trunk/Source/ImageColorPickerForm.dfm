object frmImageColorPicker: TfrmImageColorPicker
  Left = 188
  Top = 131
  Width = 386
  Height = 402
  Caption = 'Image Color Picker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgvwPickArea: TImgView32
    Left = 0
    Top = 0
    Width = 378
    Height = 358
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 0
    OnMouseDown = imgvwPickAreaMouseDown
    OnMouseMove = imgvwPickAreaMouseMove
  end
end
