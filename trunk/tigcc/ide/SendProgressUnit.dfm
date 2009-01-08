object SendProgressForm: TSendProgressForm
  Left = 159
  Top = 155
  ActiveControl = CancelButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Sending...'
  ClientHeight = 95
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameLabel: TLabel
    Left = 16
    Top = 16
    Width = 3
    Height = 13
  end
  object ProgressBar: TProgressBar
    Left = 16
    Top = 32
    Width = 313
    Height = 16
    Max = 0
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 136
    Top = 60
    Width = 73
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Default = True
    TabOrder = 1
    OnClick = CancelButtonClick
  end
end
