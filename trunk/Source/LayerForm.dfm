object frmLayers: TfrmLayers
  Left = 193
  Top = 135
  Width = 236
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Layers'
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 236
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tlbrLayerTools: TToolBar
    Left = 0
    Top = 202
    Width = 228
    Height = 24
    Cursor = crHandPoint
    Align = alBottom
    AutoSize = True
    Caption = 'tlbrLayerTools'
    Flat = True
    Images = dmMain.imglstTools
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tlbtnSeparator1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'tlbtnSeparator1'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tlbtnSpecialLayers: TToolButton
      Left = 8
      Top = 0
      Action = dmMain.actnSpecialLayers
    end
    object tlbrAddMask: TToolButton
      Left = 31
      Top = 0
      Action = dmMain.actnAddMask
    end
    object tlbtnNewLayer: TToolButton
      Left = 54
      Top = 0
      Action = dmMain.actnNewLayer
    end
    object tlbtnDeleteLayer: TToolButton
      Left = 77
      Top = 0
      Action = dmMain.actnDeleteLayer
    end
  end
  object tlbrBlendModes: TToolBar
    Left = 0
    Top = 0
    Width = 228
    Height = 23
    AutoSize = True
    ButtonHeight = 21
    Caption = 'tlbrBlendModes'
    Ctl3D = False
    Flat = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object tlbnSeparator2: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'tlbnSeparator2'
      Style = tbsSeparator
    end
    object cmbbxBlendModes: TComboBox
      Left = 8
      Top = 0
      Width = 145
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 40
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxBlendModesChange
    end
  end
  object tlbrLayerOpacity: TToolBar
    Left = 0
    Top = 23
    Width = 228
    Height = 21
    AutoSize = True
    ButtonHeight = 19
    Caption = 'tlbrLayerOpacity'
    Ctl3D = False
    Flat = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object ggbrLayerOpacity: TGaugeBar
      Left = 8
      Top = 0
      Width = 145
      Height = 19
      Cursor = crHandPoint
      Constraints.MinWidth = 120
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrLayerOpacityChange
      OnMouseUp = ggbrLayerOpacityMouseUp
    end
    object edtLayerOpacity: TEdit
      Left = 153
      Top = 0
      Width = 40
      Height = 19
      TabOrder = 1
    end
    object lblLayerOpacity: TLabel
      Left = 193
      Top = 0
      Width = 24
      Height = 19
      AutoSize = False
      Caption = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object tlbrLayerLockTools: TToolBar
    Left = 0
    Top = 44
    Width = 228
    Height = 15
    AutoSize = True
    ButtonHeight = 13
    Caption = 'tlbrLayerLockTools'
    Flat = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object ToolButton5: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object lblLockOption: TLabel
      Left = 8
      Top = 0
      Width = 27
      Height = 13
      Caption = 'Lock:'
    end
    object chckbxLockTransparency: TCheckBox
      Left = 35
      Top = 0
      Width = 15
      Height = 13
      Cursor = crHandPoint
      Action = dmMain.actnLayerLockTransparency
      TabOrder = 0
    end
    object imgLockTransparency: TImage
      Left = 50
      Top = 0
      Width = 16
      Height = 16
      Hint = 'Lock transparent pixels'
      AutoSize = True
      Picture.Data = {
        07544269746D6170F6000000424DF60000000000000076000000280000001000
        0000100000000100040000000000800000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF008888888888888888800000000000000880FFFFFF7777770880FFFFFF7777
        770880FFFFFF7777770880FFFFFF7777770880FFFFFF7777770880FFFFFF7777
        770880777777FFFFFF0880777777FFFFFF0880777777FFFFFF0880777777FFFF
        FF0880777777FFFFFF0880777777FFFFFF088000000000000008888888888888
        8888}
    end
  end
end
