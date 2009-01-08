object CustomStyleForm: TCustomStyleForm
  Left = 190
  Top = 113
  ActiveControl = BeginEdit
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Custom Style'
  ClientHeight = 234
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 16
    Top = 140
    Width = 193
    Height = 2
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 24
    Top = 19
    Width = 50
    Height = 13
    Caption = '&Beginning:'
    FocusControl = BeginEdit
  end
  object Label2: TLabel
    Left = 24
    Top = 43
    Width = 36
    Height = 13
    Caption = '&Ending:'
    FocusControl = EndEdit
  end
  object Label3: TLabel
    Left = 24
    Top = 67
    Width = 94
    Height = 13
    Caption = '&Ignore Ending After:'
    FocusControl = IgnoreEdit
  end
  object Bevel2: TBevel
    Left = 8
    Top = 186
    Width = 209
    Height = 2
    Shape = bsTopLine
  end
  object Button1: TButton
    Left = 24
    Top = 152
    Width = 81
    Height = 25
    Caption = 'Co&lor...'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 152
    Width = 81
    Height = 25
    Caption = '&Style...'
    TabOrder = 5
    OnClick = Button2Click
  end
  object BeginEdit: TEdit
    Left = 80
    Top = 16
    Width = 121
    Height = 21
    MaxLength = 20
    TabOrder = 0
    OnChange = BeginEditChange
  end
  object EndEdit: TEdit
    Left = 80
    Top = 40
    Width = 121
    Height = 21
    MaxLength = 20
    TabOrder = 1
    OnChange = EndEditChange
  end
  object IgnoreEdit: TEdit
    Left = 124
    Top = 64
    Width = 77
    Height = 21
    MaxLength = 1
    TabOrder = 2
    OnChange = IgnoreEditChange
  end
  object Button3: TButton
    Left = 24
    Top = 200
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object Button4: TButton
    Left = 120
    Top = 200
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object SwitchableCheckBox: TCheckBox
    Left = 24
    Top = 88
    Width = 73
    Height = 17
    Caption = 'S&witchable'
    TabOrder = 3
    OnClick = SwitchableCheckBoxClick
  end
  object LineStartOnlyCheckBox: TCheckBox
    Left = 24
    Top = 112
    Width = 177
    Height = 17
    Caption = 'Only &at beginning of line'
    TabOrder = 4
    OnClick = LineStartOnlyCheckBoxClick
  end
  object ColorDlg: TColorDialog
    Options = [cdAnyColor]
    Left = 192
  end
end
