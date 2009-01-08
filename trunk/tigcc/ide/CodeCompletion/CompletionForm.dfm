object CompForm: TCompForm
  Left = 236
  Top = 269
  Width = 328
  Height = 116
  Caption = 'Code Completion'
  Color = clBtnFace
  Constraints.MinHeight = 96
  Constraints.MinWidth = 128
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object CompList: TListBox
    Left = 0
    Top = 0
    Width = 320
    Height = 63
    Style = lbVirtual
    Align = alClient
    BorderStyle = bsNone
    ItemHeight = 16
    TabOrder = 0
    OnDblClick = CompListDblClick
    OnDrawItem = CompListDrawItem
    OnKeyDown = CompListKeyDown
    OnKeyPress = CompListKeyPress
    OnKeyUp = CompListKeyUp
  end
  object CompStatus: TStatusBar
    Left = 0
    Top = 63
    Width = 320
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
