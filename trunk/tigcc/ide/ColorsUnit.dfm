object MultipleColorsForm: TMultipleColorsForm
  Left = 188
  Top = 102
  ActiveControl = ColorBox
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Colors'
  ClientHeight = 167
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = UpdateButtons
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 120
    Width = 249
    Height = 2
    Shape = bsTopLine
  end
  object ColorBox: TListBox
    Left = 16
    Top = 16
    Width = 153
    Height = 89
    Style = lbOwnerDrawFixed
    ItemHeight = 15
    TabOrder = 0
    OnDblClick = EditColor
    OnDrawItem = ColorBoxDrawItem
    OnKeyDown = ColorBoxKeyDown
  end
  object Button1: TButton
    Left = 176
    Top = 16
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 176
    Top = 48
    Width = 73
    Height = 25
    Caption = '&Remove'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 176
    Top = 80
    Width = 73
    Height = 25
    Caption = '&Edit...'
    TabOrder = 3
    OnClick = EditColor
  end
  object Button4: TButton
    Left = 40
    Top = 132
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button5: TButton
    Left = 144
    Top = 132
    Width = 81
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 232
  end
end
