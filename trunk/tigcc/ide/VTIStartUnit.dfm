object VTIStartForm: TVTIStartForm
  Left = 211
  Top = 165
  ActiveControl = CancelButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Starting TiEmu...'
  ClientHeight = 86
  ClientWidth = 241
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 209
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Please wait while TiEmu is being started...'
  end
  object CancelButton: TButton
    Left = 80
    Top = 48
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object FindTimer: TTimer
    Interval = 100
    OnTimer = FindTimerTimer
  end
end
