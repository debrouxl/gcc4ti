object BatchEdit: TBatchEdit
  Left = 297
  Top = 210
  AutoSize = True
  BorderStyle = bsToolWindow
  BorderWidth = 6
  Caption = 'Batch'
  ClientHeight = 257
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object WorkDirLbl: TLabel
    Left = 0
    Top = 0
    Width = 174
    Height = 14
    Caption = 'Work directory (containing headers)'
  end
  object HsfDirLbl: TLabel
    Left = 0
    Top = 48
    Width = 196
    Height = 14
    Caption = 'Root directory containing subdir with hsf'
  end
  object WorkDirEdit: TEdit
    Left = 24
    Top = 16
    Width = 313
    Height = 22
    TabOrder = 0
  end
  object WorkDirBtn: TButton
    Left = 339
    Top = 16
    Width = 22
    Height = 22
    Caption = '...'
    TabOrder = 1
    OnClick = WorkDirBtnClick
  end
  object HsfDirEdit: TEdit
    Left = 24
    Top = 64
    Width = 313
    Height = 22
    TabOrder = 2
  end
  object HsfDirBtn: TButton
    Left = 339
    Top = 64
    Width = 22
    Height = 22
    Caption = '...'
    TabOrder = 3
    OnClick = HsfDirBtnClick
  end
  object GoBtn: TButton
    Left = 286
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 4
    OnClick = GoBtnClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 96
    Width = 361
    Height = 129
    TabOrder = 5
  end
  object EnumCheck: TCheckBox
    Left = 8
    Top = 236
    Width = 217
    Height = 17
    Caption = 'Generate informations for enumerations'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
end
