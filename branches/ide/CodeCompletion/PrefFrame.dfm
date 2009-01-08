object CodingExt: TCodingExt
  Left = 0
  Top = 0
  Width = 297
  Height = 245
  TabOrder = 0
  object TemplateBox: TGroupBox
    Left = 0
    Top = 0
    Width = 297
    Height = 200
    Align = alClient
    BiDiMode = bdRightToLeft
    Caption = 'Code te&mplates'
    ParentBiDiMode = False
    TabOrder = 0
    object IdLbl: TLabel
      Left = 8
      Top = 100
      Width = 40
      Height = 13
      Caption = '&Identifier'
    end
    object CodeLbl: TLabel
      Left = 8
      Top = 120
      Width = 25
      Height = 13
      Caption = 'Cod&e'
    end
    object TmpltList: TListBox
      Left = 8
      Top = 16
      Width = 281
      Height = 73
      Style = lbVirtual
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = TmpltListClick
      OnData = TmpltListData
    end
    object IdEdit: TEdit
      Left = 56
      Top = 96
      Width = 233
      Height = 21
      TabOrder = 1
    end
    object CodeMemo: TMemo
      Left = 56
      Top = 120
      Width = 233
      Height = 57
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      WantTabs = True
    end
    object ClearBtn: TButton
      Left = 120
      Top = 180
      Width = 89
      Height = 17
      Caption = 'C&lear selection'
      TabOrder = 3
      OnClick = ClearBtnClick
    end
    object ApplyBtn: TButton
      Left = 216
      Top = 180
      Width = 73
      Height = 17
      Caption = '&Apply'
      TabOrder = 4
      OnClick = ApplyBtnClick
    end
  end
  object CompBox: TGroupBox
    Left = 0
    Top = 200
    Width = 297
    Height = 45
    Align = alBottom
    BiDiMode = bdRightToLeft
    Caption = 'Code completion'
    ParentBiDiMode = False
    TabOrder = 1
    object CompEditor: TButton
      Left = 8
      Top = 16
      Width = 281
      Height = 25
      Caption = 'Open t&he completion editor'
      TabOrder = 0
      OnClick = CompEditorClick
    end
  end
end
