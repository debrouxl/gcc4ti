object StartupForm: TStartupForm
  Left = 168
  Top = 86
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Welcome'
  ClientHeight = 265
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = DirectoryBoxChange
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 273
    Height = 25
    AutoSize = False
    Caption = 
      'Please double-click on the folder you installed or unzipped TI-G' +
      'CC in.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 44
    Width = 196
    Height = 13
    Caption = 'If you did not download TI-GCC yet, click '
  end
  object DownloadLabel: TURLLabel
    Left = 212
    Top = 44
    Width = 21
    Height = 13
    Cursor = crHandPoint
    Caption = 'here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    URL = 'http://www.ticalc.org/pub/win/asm/tigcc.zip'
    ShowWindow = swShowMaximized
  end
  object Label3: TLabel
    Left = 233
    Top = 44
    Width = 3
    Height = 13
    Caption = '.'
  end
  object OKButton: TButton
    Left = 56
    Top = 224
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 168
    Top = 224
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object DirectoryBox: TDirectoryListBox
    Left = 16
    Top = 64
    Width = 273
    Height = 113
    ItemHeight = 16
    TabOrder = 2
    OnChange = DirectoryBoxChange
  end
  object DriveBox: TDriveComboBox
    Left = 16
    Top = 184
    Width = 273
    Height = 19
    DirList = DirectoryBox
    TabOrder = 3
    TextCase = tcUpperCase
  end
end