object ToolPropertiesForm: TToolPropertiesForm
  Left = 131
  Top = 102
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Tool Properties'
  ClientHeight = 205
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    377
    205)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 83
    Top = 19
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = '&Title:'
  end
  object Label2: TLabel
    Left = 33
    Top = 51
    Width = 73
    Height = 13
    Alignment = taRightJustify
    Caption = 'Command &Line:'
  end
  object Label3: TLabel
    Left = 18
    Top = 83
    Width = 88
    Height = 13
    Alignment = taRightJustify
    Caption = '&Working Directory:'
  end
  object Label4: TLabel
    Left = 36
    Top = 115
    Width = 70
    Height = 13
    Alignment = taRightJustify
    Caption = 'Window &State:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 152
    Width = 361
    Height = 2
    Shape = bsTopLine
  end
  object TitleEdit: TEdit
    Left = 112
    Top = 16
    Width = 137
    Height = 21
    TabOrder = 0
    OnChange = EditChange
  end
  object CommandLineEdit: TEdit
    Left = 112
    Top = 48
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = EditChange
  end
  object WorkingDirEdit: TEdit
    Left = 112
    Top = 80
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object OKButton: TButton
    Left = 88
    Top = 168
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 208
    Top = 168
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object WindowStateEdit: TComboBox
    Left = 112
    Top = 112
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Normal'
      'Maximized'
      'Minimized')
  end
  object CommandLineBrowseButton: TButton
    Left = 304
    Top = 48
    Width = 59
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&Browse...'
    TabOrder = 6
    OnClick = CommandLineBrowseButtonClick
  end
  object WorkingDirBrowseButton: TButton
    Left = 304
    Top = 80
    Width = 59
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'B&rowse...'
    TabOrder = 7
    OnClick = WorkingDirBrowseButtonClick
  end
  object BrowseDialog: TOpenDialog
    Filter = 'Programs (*.exe)|*.exe|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Browse'
  end
end
