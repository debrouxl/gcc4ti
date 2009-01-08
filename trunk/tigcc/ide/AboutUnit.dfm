object AboutForm: TAboutForm
  Left = 136
  Top = 90
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About TIGCC'
  ClientHeight = 228
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FramePanel: TPanel
    Left = 8
    Top = 8
    Width = 329
    Height = 182
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 20
      Top = 20
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010002002020100000000000E80200002600000010101000
        00000000280100000E0300002800000020000000400000000100040000000000
        8002000000000000000000000000000000000000000000000000800000800000
        00808000800000008000800080800000C0C0C000808080000000FF0000FF0000
        00FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000000000000000
        000000000000330000000000000000000000000000003B338888888888888888
        00000000000003BB3777777777777777800000000330003BB300777007770077
        8000000003B3003BB38807F8807F880780000000003B333BBB3F777FF777FF77
        80000000003BBBBFBB37777777777777800000000003BBBBFBB3777007770077
        80000000000033BBBFBB37F8807F88078000000000000033BBFBB37FF777FF77
        800000000000007F3BBFBB3777777777800000000000007F73BBFBB307770077
        800000000000007F7F3BBFBB307F8807800000000000007F77F3BBFBB377FF77
        800000000000007F77773BBFBB377777800000000000007F770073BBFBB30077
        800000000000007F7F88073BBFBB3807800000000000007F77FF7773BBFBB377
        800000000000007F777777773BBFBB37800000000000007F77FFFFFFF3BBFBB3
        800000000000007F78E8EEE0E83BBFBB330000000000007F78E77EE08E83BBFB
        BB3300000000007F7800800800803BBFBBBB30000000007F78EE8E80EE77E3BB
        FBBBB3000000007F78EEE8E0EEE8E3BBB333B3000000007F78EEEEE0EEE8EE3B
        B3003B300000007F788888888888883BB30003300000007F7777777777777773
        BB30000000000007FFFFFFFFFFFFFFFF33B30000000000007777777777777777
        0033000000000000000000000000000000000000FFFFFFFFF30000FFF000007F
        F800003F9C00003F8C00003FC000003FC000003FE000003FF000003FFC00003F
        FC00003FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003FFC00003F
        FC00003FFC00003FFC00003FFC00000FFC000007FC000003FC000003FC000031
        FC000039FC00001FFE00000FFF0000CFFFFFFFFF280000001000000020000000
        0100040000000000C00000000000000000000000000000000000000000000000
        000080000080000000808000800000008000800080800000C0C0C00080808000
        0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0003308888
        888800000373777777778000333B3878878780003B3B3F7FF7F7800003BBB378
        878780000033BB3FF7F78000000F3BB377778000000F73BB37878000000F7F3B
        B3F78000000F7773BB378000000F7FFF3BB38000000F886083BB3300000F8808
        083BBB30000F8E80E83B3373000F78888883B3330000FFFFFFFF3300900F0000
        80070000000700000007000080070000C0070000E0070000E0070000E0070000
        E0070000E0070000E0030000E0010000E0000000E0000000F0030000}
      Stretch = True
      IsControl = True
    end
    object ProductNameLabel: TLabel
      Left = 64
      Top = 16
      Width = 125
      Height = 15
      Caption = 'TIGCC C and ASM SDK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      IsControl = True
    end
    object VersionLabel: TLabel
      Left = 64
      Top = 31
      Width = 3
      Height = 14
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      IsControl = True
    end
    object Label2: TLabel
      Left = 16
      Top = 80
      Width = 125
      Height = 14
      Caption = 'Compiler modifications by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label3: TLabel
      Left = 16
      Top = 64
      Width = 82
      Height = 14
      Caption = 'Original linker by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label4: TLabel
      Left = 16
      Top = 112
      Width = 146
      Height = 14
      Caption = 'Library and documentation by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label5: TLabel
      Left = 16
      Top = 144
      Width = 33
      Height = 14
      Caption = 'IDE by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label6: TLabel
      Left = 167
      Top = 64
      Width = 24
      Height = 14
      Caption = ' and '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label1: TLabel
      Left = 16
      Top = 160
      Width = 146
      Height = 14
      Caption = 'Documentation conversion by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object CopyrightLabel: TLabel
      Left = 64
      Top = 44
      Width = 209
      Height = 15
      Caption = 'Copyright © 1999-2006 The TIGCC Team'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label8: TLabel
      Left = 206
      Top = 80
      Width = 108
      Height = 14
      Caption = ', Sebastian, and Kevin'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label9: TLabel
      Left = 16
      Top = 96
      Width = 47
      Height = 14
      Caption = 'Linker by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label10: TLabel
      Left = 16
      Top = 128
      Width = 109
      Height = 14
      Caption = 'A68k modifications by '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label7: TLabel
      Left = 152
      Top = 96
      Width = 24
      Height = 14
      Caption = ' and '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object URLLabel1: TURLLabel
      Left = 97
      Top = 64
      Width = 70
      Height = 14
      AutoSize = False
      Caption = 'Xavier Vassor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 0
      URL = 'mailto:XVassor@mail.dotcom.fr'
      ShowWindow = swShowMaximized
    end
    object URLLabel2: TURLLabel
      Left = 191
      Top = 64
      Width = 64
      Height = 14
      AutoSize = False
      Caption = 'Niklas Brunlid'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 1
      URL = 'mailto:Niklas@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel3: TURLLabel
      Left = 141
      Top = 80
      Width = 65
      Height = 14
      AutoSize = False
      Caption = 'Jean Canazzi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 2
      URL = 'mailto:Jean@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel4: TURLLabel
      Left = 162
      Top = 112
      Width = 54
      Height = 14
      AutoSize = False
      Caption = 'Zeljko Juric'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 3
      URL = 'mailto:Zeljko@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel5: TURLLabel
      Left = 49
      Top = 144
      Width = 89
      Height = 14
      AutoSize = False
      Caption = 'Sebastian Reichelt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 4
      URL = 'mailto:Sebastian@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel6: TURLLabel
      Left = 162
      Top = 160
      Width = 68
      Height = 14
      AutoSize = False
      Caption = 'Philipp Winkler'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 5
      URL = 'mailto:p.winkler@atn.nu'
      ShowWindow = swShow
    end
    object URLLabel7: TURLLabel
      Left = 63
      Top = 96
      Width = 89
      Height = 14
      AutoSize = False
      Caption = 'Sebastian Reichelt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 6
      URL = 'mailto:Sebastian@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel8: TURLLabel
      Left = 125
      Top = 128
      Width = 59
      Height = 14
      AutoSize = False
      Caption = 'Kevin Kofler'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 7
      URL = 'mailto:Kevin@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
    object URLLabel9: TURLLabel
      Left = 176
      Top = 96
      Width = 59
      Height = 14
      AutoSize = False
      Caption = 'Kevin Kofler'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 8
      URL = 'mailto:Kevin@tigcc.ticalc.org'
      ShowWindow = swShowMaximized
    end
  end
  object OKButton: TButton
    Left = 128
    Top = 197
    Width = 89
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end