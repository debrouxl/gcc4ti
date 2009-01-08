object SyntaxProgressForm: TSyntaxProgressForm
  Left = 138
  Top = 202
  Cursor = crAppStart
  ActiveControl = CancelButton
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 51
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 51
    Align = alClient
    BevelWidth = 2
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 99
      Height = 13
      Caption = 'Highlighting Syntax...'
    end
    object ProgressBar: TProgressBar
      Left = 8
      Top = 24
      Width = 283
      Height = 16
      Min = 0
      Max = 0
      Step = 1
      TabOrder = 1
    end
    object CancelButton: TButton
      Left = 296
      Top = 16
      Width = 57
      Height = 25
      Cancel = True
      Caption = '&Stop'
      Default = True
      TabOrder = 0
      OnClick = CancelButtonClick
    end
  end
end