object ToolsForm: TToolsForm
  Left = 125
  Top = 116
  ActiveControl = ToolsList
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Configure Tools'
  ClientHeight = 209
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    425
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsList: TListView
    Left = 16
    Top = 16
    Width = 305
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Title'
        Width = 100
      end
      item
        Caption = 'Command Line'
        Width = 250
      end
      item
        Caption = 'Working Directory'
        Width = 200
      end
      item
        Caption = 'Window State'
        Width = 100
      end>
    ColumnClick = False
    DragMode = dmAutomatic
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = EditButtonClick
    OnDragDrop = ToolsListDragDrop
    OnDragOver = ToolsListDragOver
    OnKeyDown = ToolsListKeyDown
    OnSelectItem = ToolsListSelectItem
  end
  object AddButton: TButton
    Left = 336
    Top = 16
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add...'
    TabOrder = 1
    OnClick = AddButtonClick
  end
  object EditButton: TButton
    Left = 336
    Top = 48
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Edit...'
    Enabled = False
    TabOrder = 2
    OnClick = EditButtonClick
  end
  object RemoveButton: TButton
    Left = 336
    Top = 80
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    Enabled = False
    TabOrder = 3
    OnClick = RemoveButtonClick
  end
  object OKButton: TButton
    Left = 336
    Top = 136
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 336
    Top = 168
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
