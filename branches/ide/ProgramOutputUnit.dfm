object ProgramOutputForm: TProgramOutputForm
  Left = 76
  Top = 28
  Width = 513
  Height = 419
  BorderIcons = [biSystemMenu]
  Caption = 'Program Output'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 194
    Width = 505
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
  end
  object OutputMemo: TMemoComponent
    Left = 0
    Top = 17
    Width = 505
    Height = 177
    ScrollBars = ssBoth
    Bitmapped = False
    ReadOnly = True
    AlwaysShowCaret = False
    LeftMargin = 2
    TopMargin = 0
    TabSize = 2
    Align = alTop
    Color = clWindow
    Constraints.MinHeight = 64
    Constraints.MinWidth = 64
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 0
  end
  object ErrorMemo: TMemoComponent
    Left = 0
    Top = 214
    Width = 505
    Height = 178
    ScrollBars = ssBoth
    Bitmapped = False
    ReadOnly = True
    AlwaysShowCaret = False
    LeftMargin = 2
    TopMargin = 0
    TabSize = 2
    Align = alClient
    Color = clWindow
    Constraints.MinHeight = 64
    Constraints.MinWidth = 64
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 17
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = 'Output:'
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 0
    Top = 197
    Width = 505
    Height = 17
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = 'Errors:'
    TabOrder = 3
  end
end