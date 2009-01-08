object StyleSelectionForm: TStyleSelectionForm
  Left = 227
  Top = 141
  ActiveControl = CheckBox1
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Style'
  ClientHeight = 135
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 88
    Width = 161
    Height = 2
    Shape = bsTopLine
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 81
    Height = 17
    Caption = 'Custom &Style'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 32
    Top = 40
    Width = 49
    Height = 17
    Caption = '&Bold'
    Enabled = False
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 32
    Top = 60
    Width = 49
    Height = 17
    Caption = '&Italic'
    Enabled = False
    TabOrder = 2
  end
  object CheckBox4: TCheckBox
    Left = 96
    Top = 40
    Width = 65
    Height = 17
    Caption = '&Underline'
    Enabled = False
    TabOrder = 3
  end
  object CheckBox5: TCheckBox
    Left = 96
    Top = 60
    Width = 65
    Height = 17
    Caption = 'S&trike Out'
    Enabled = False
    TabOrder = 4
  end
  object Button1: TButton
    Left = 16
    Top = 100
    Width = 67
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object Button2: TButton
    Left = 94
    Top = 100
    Width = 67
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
