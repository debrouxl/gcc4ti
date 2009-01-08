object TemplateForm: TTemplateForm
  Left = 396
  Top = 228
  BorderStyle = bsNone
  BorderWidth = 1
  Caption = 'Code Templates'
  ClientHeight = 126
  ClientWidth = 318
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object TmpltList: TListBox
    Left = 0
    Top = 0
    Width = 318
    Height = 126
    Style = lbVirtual
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 16
    ParentFont = False
    TabOrder = 0
    OnData = TmpltListData
    OnDblClick = TmpltListDblClick
    OnKeyDown = TmpltListKeyDown
  end
end
