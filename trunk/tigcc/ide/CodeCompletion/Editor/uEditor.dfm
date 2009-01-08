object CEditorForm: TCEditorForm
  Left = 606
  Top = 118
  AutoSize = True
  BorderStyle = bsToolWindow
  BorderWidth = 2
  Caption = 'Completion Editor'
  ClientHeight = 288
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object MenuPnl: TPanel
    Left = 0
    Top = 0
    Width = 441
    Height = 26
    BevelInner = bvLowered
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object NewBtn: TSpeedButton
      Left = 2
      Top = 2
      Width = 103
      Height = 22
      Caption = 'New'
      Flat = True
      OnClick = NewBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 222
      Top = 2
      Width = 103
      Height = 22
      Caption = 'Save'
      Flat = True
      OnClick = SaveBtnClick
    end
    object OpenBtn: TSpeedButton
      Left = 112
      Top = 2
      Width = 103
      Height = 22
      Caption = 'Open'
      Flat = True
      OnClick = OpenBtnClick
    end
    object ToolsBtn: TSpeedButton
      Left = 336
      Top = 2
      Width = 103
      Height = 22
      Caption = 'Tools'
      Flat = True
      OnClick = ToolsBtnClick
    end
  end
  object MainPageControl: TPageControl
    Left = 0
    Top = 32
    Width = 441
    Height = 256
    ActivePage = InfoTab
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 1
    object InfoTab: TTabSheet
      Caption = 'General Information'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      object UNameLbl: TLabel
        Left = 4
        Top = 4
        Width = 321
        Height = 14
        Caption = 
          'Source file name (without path, but with extension, like "tigccl' +
          'ib.h")'
      end
      object IncludeLbl: TLabel
        Left = 4
        Top = 52
        Width = 219
        Height = 14
        Caption = 'Included files (list files included in the source)'
      end
      object UNameEdit: TEdit
        Left = 32
        Top = 24
        Width = 393
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = SetModified
      end
      object IncludeMemo: TMemo
        Left = 32
        Top = 72
        Width = 393
        Height = 147
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
        OnChange = SetModified
      end
    end
    object ItemsTab: TTabSheet
      Caption = 'Completion items'
      ImageIndex = 1
      object ItemsLbl: TLabel
        Left = 4
        Top = 4
        Width = 41
        Height = 14
        Caption = 'Items list'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object ItemsList: TListBox
        Left = 0
        Top = 20
        Width = 169
        Height = 180
        Style = lbVirtual
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 16
        MultiSelect = True
        ParentFont = False
        TabOrder = 0
        OnClick = ItemsListClick
        OnData = ItemsListData
      end
      object ItemBox: TGroupBox
        Left = 172
        Top = 4
        Width = 261
        Height = 224
        Caption = 'Selected items properties'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsUnderline]
        ParentFont = False
        TabOrder = 1
        object PreviewBox: TPaintBox
          Left = 5
          Top = 112
          Width = 251
          Height = 81
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnPaint = PreviewBoxPaint
        end
        object ItemEditor: TValueListEditor
          Left = 4
          Top = 16
          Width = 253
          Height = 95
          BorderStyle = bsNone
          DefaultColWidth = 96
          DisplayOptions = [doAutoColResize, doKeyColFixed]
          FixedCols = 1
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          Strings.Strings = (
            '=')
          TabOrder = 0
          OnStringsChange = ItemEditorStringsChange
          ColWidths = (
            96
            155)
          RowHeights = (
            18)
        end
        object ApplyBtn: TButton
          Left = 165
          Top = 195
          Width = 92
          Height = 25
          Caption = 'Apply'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = ApplyBtnClick
        end
      end
      object DelBtn: TButton
        Left = 0
        Top = 202
        Width = 169
        Height = 25
        Caption = 'Clear selection'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = DelBtnClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'lst'
    Filter = 'Completion informations (*.ccf)|*.ccf|All files (*.*)|*.*'
    Left = 288
    Top = 32
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'lst'
    Filter = 'Completion informations (*.ccf)|*.ccf|All files (*.*)|*.*'
    Left = 320
    Top = 32
  end
  object ToolsMenu: TPopupMenu
    Left = 352
    Top = 32
    object ParseHeader: TMenuItem
      Caption = 'Parse header (*.h)'
      OnClick = ParseHeaderClick
    end
    object ParseHSF: TMenuItem
      Caption = 'Parse HSF files'
      OnClick = ParseHSFClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Batch1: TMenuItem
      Caption = 'Batch processing'
      OnClick = Batch1Click
    end
  end
  object OpenHeaderDialog: TOpenDialog
    DefaultExt = 'lst'
    Filter = 'C Header (*.h)|*.h|Tous (*.*)|*.*'
    Left = 384
    Top = 32
  end
end
