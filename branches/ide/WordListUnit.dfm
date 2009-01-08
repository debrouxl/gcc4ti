object WordListForm: TWordListForm
  Left = 209
  Top = 124
  ActiveControl = ListStrings
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Word List'
  ClientHeight = 230
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 16
    Top = 132
    Width = 193
    Height = 2
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 180
    Width = 209
    Height = 2
    Shape = bsTopLine
  end
  object Button1: TButton
    Left = 24
    Top = 144
    Width = 81
    Height = 25
    Caption = 'Co&lor...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 144
    Width = 81
    Height = 25
    Caption = '&Style...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 192
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button4: TButton
    Left = 120
    Top = 192
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object ListStrings: TMemo
    Left = 16
    Top = 16
    Width = 193
    Height = 81
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object CaseSensitiveBox: TCheckBox
    Left = 120
    Top = 104
    Width = 89
    Height = 17
    Caption = 'C&ase Sensitive'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CaseSensitiveBoxClick
  end
  object ColorDlg: TColorDialog
    Options = [cdAnyColor]
    Left = 192
  end
end
