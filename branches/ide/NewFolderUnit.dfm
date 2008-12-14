object NewFolderForm: TNewFolderForm
  Left = 166
  Top = 129
  ActiveControl = InputBox
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Folder'
  ClientHeight = 118
  ClientWidth = 265
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
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 63
    Height = 13
    Caption = '&Folder Name:'
    FocusControl = InputBox
  end
  object Bevel1: TBevel
    Left = 8
    Top = 56
    Width = 249
    Height = 2
    Shape = bsTopLine
  end
  object InputBox: TEdit
    Left = 88
    Top = 16
    Width = 161
    Height = 21
    TabOrder = 0
    Text = 'TI-GCC'
    OnChange = InputBoxChange
  end
  object ButtonOK: TButton
    Left = 40
    Top = 76
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 144
    Top = 76
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end