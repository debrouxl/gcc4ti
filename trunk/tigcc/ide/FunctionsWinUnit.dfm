object FunctionsForm: TFunctionsForm
  Left = 190
  Top = 137
  ActiveControl = FuncList
  BorderStyle = bsDialog
  Caption = 'Functions'
  ClientHeight = 161
  ClientWidth = 313
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
  object FuncList: TListBox
    Left = 16
    Top = 16
    Width = 177
    Height = 129
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnClick = FuncListClick
    OnDblClick = FuncListDblClick
    OnMouseDown = FuncListMouseDown
  end
  object PrototypeButton: TButton
    Left = 208
    Top = 24
    Width = 89
    Height = 25
    Caption = '&Prototype'
    ModalResult = 6
    TabOrder = 1
  end
  object ImplementationButton: TButton
    Left = 208
    Top = 64
    Width = 89
    Height = 25
    Caption = '&Implementation'
    Default = True
    ModalResult = 7
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 208
    Top = 112
    Width = 89
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
