object SourceFileForm: TSourceFileForm
  Left = 56
  Top = 42
  ActiveControl = EditorToolBar
  AutoScroll = False
  Caption = 'GCC4TI IDE Editor'
  ClientHeight = 311
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBarBevel: TBevel
    Left = 0
    Top = 28
    Width = 482
    Height = 1
    Align = alTop
    Shape = bsSpacer
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 292
    Width = 482
    Height = 19
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Constraints.MaxHeight = 19
    Constraints.MinHeight = 19
    Panels = <
      item
        Alignment = taRightJustify
        Text = '1'
        Width = 30
      end
      item
        Alignment = taRightJustify
        Text = '1'
        Width = 30
      end
      item
        Bevel = pbNone
        Width = 1
      end
      item
        Text = '0 Characters'
        Width = 93
      end
      item
        Bevel = pbNone
        Width = 1
      end
      item
        Width = 200
      end>
    UseSystemFont = False
  end
  object EditorToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 482
    Height = 28
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 27
    Caption = 'Editor Toolbar'
    DisabledImages = MainForm.ToolBarImagesDisabled
    DragKind = dkDock
    EdgeBorders = [ebLeft, ebTop]
    Flat = True
    Images = MainForm.ToolBarImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    object ToolButton3: TToolButton
      Left = 0
      Top = 0
      Action = ActionFileSave
    end
    object ToolButton2: TToolButton
      Left = 27
      Top = 0
      Action = ActionFileAddToProject
    end
    object ToolButton4: TToolButton
      Left = 54
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton23: TToolButton
      Left = 62
      Top = 0
      Action = ActionFilePrintQuickly
    end
    object ToolButton24: TToolButton
      Left = 89
      Top = 0
      Width = 8
      Caption = 'ToolButton24'
      ImageIndex = 15
      Style = tbsSeparator
    end
    object ToolButton6: TToolButton
      Left = 97
      Top = 0
      Action = ActionEditDelete
    end
    object ToolButton7: TToolButton
      Left = 124
      Top = 0
      Action = ActionEditCut
    end
    object ToolButton8: TToolButton
      Left = 151
      Top = 0
      Action = ActionEditCopy
    end
    object ToolButton9: TToolButton
      Left = 178
      Top = 0
      Action = ActionEditPaste
    end
    object ToolButton10: TToolButton
      Left = 205
      Top = 0
      Width = 8
      Caption = 'ToolButton10'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 213
      Top = 0
      Action = ActionEditUndo
    end
    object ToolButton19: TToolButton
      Left = 240
      Top = 0
      Action = ActionEditRedo
    end
    object ToolButton11: TToolButton
      Left = 267
      Top = 0
      Width = 8
      Caption = 'ToolButton11'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton20: TToolButton
      Left = 275
      Top = 0
      Action = ActionFindFind
    end
    object ToolButton21: TToolButton
      Left = 302
      Top = 0
      Action = ActionFindReplace
    end
    object ToolButton25: TToolButton
      Left = 329
      Top = 0
      Action = ActionFindFunctions
      DropdownMenu = FunctionPopup
      Style = tbsDropDown
    end
    object ToolButton22: TToolButton
      Left = 369
      Top = 0
      Width = 8
      Caption = 'ToolButton22'
      ImageIndex = 16
      Style = tbsSeparator
    end
    object ToolButton13: TToolButton
      Left = 377
      Top = 0
      Action = ActionFileCompile
    end
    object ToolButton1: TToolButton
      Left = 404
      Top = 0
      Width = 4
      Caption = 'ToolButton1'
      ImageIndex = 10
      Style = tbsDivider
    end
  end
  object Actions: TActionList
    Images = MainForm.ToolBarImages
    Left = 32
    Top = 32
    object ActionFindFind: TAction
      Category = 'Find'
      Caption = '&Find...'
      Hint = 'Find Text|Find the specified text in the current source file'
      ImageIndex = 13
      ShortCut = 16454
      OnExecute = FindFind
    end
    object ActionFindReplace: TAction
      Category = 'Find'
      Caption = '&Replace...'
      Hint = 
        'Replace Text|Find the specified text in the current source file ' +
        'and replace it with something else'
      ImageIndex = 14
      ShortCut = 16466
      OnExecute = FindReplace
    end
    object ActionFindFunctions: TAction
      Category = 'Find'
      Caption = 'F&unctions...'
      Hint = 'Functions|Display a list of all functions in the current file'
      ImageIndex = 18
      OnExecute = FindFunctions
    end
    object ActionFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Hint = 'Save|Save the file'
      ImageIndex = 2
      OnExecute = FileSave
    end
    object ActionFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &As...'
      Hint = 'Save As|Save the file with another name, or in another folder'
      OnExecute = FileSaveAs
    end
    object ActionFindOpenFile: TAction
      Category = 'Find'
      Caption = '&Open File at Cursor'
      Hint = 'Open File at Cursor|Open the file the cursor is on'
      ShortCut = 16397
      OnExecute = FindOpenFile
    end
    object ActionFileCompile: TAction
      Category = 'File'
      Caption = 'C&ompile'
      Hint = 'Compile|Compile the file'
      ImageIndex = 9
      OnExecute = FileCompile
    end
    object ActionFileAddToProject: TAction
      Category = 'File'
      Caption = 'A&dd to Project'
      Hint = 'Add to Project|Add this file to the current project'
      ImageIndex = 8
      OnExecute = FileAddToProject
    end
    object ActionFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Hint = 'Print File|Print this file'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = FilePrint
    end
    object ActionFilePrintQuickly: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print File|Print this file'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = FilePrintQuickly
    end
    object ActionFileClose: TAction
      Category = 'File'
      Caption = '&Close'
      Hint = 'Close|Close this window'
      ShortCut = 32856
      OnExecute = ActionFileCloseExecute
    end
    object ActionEditUndo: TAction
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      Hint = 'Undo|Undo the last operation in the editor'
      ImageIndex = 16
      ShortCut = 32776
      OnExecute = EditUndo
    end
    object ActionEditRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      Enabled = False
      Hint = 'Redo|Redo the last undone operation in the editor'
      ImageIndex = 17
      ShortCut = 40968
      OnExecute = EditRedo
    end
    object ActionEditDelete: TAction
      Category = 'Edit'
      Caption = '&Clear'
      Enabled = False
      Hint = 'Clear|Delete the selected text in the editor'
      ImageIndex = 4
      OnExecute = EditDelete
    end
    object ActionEditCut: TAction
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      Hint = 'Cut|Copy the selected text into the clipboard and delete it'
      ImageIndex = 5
      OnExecute = EditCut
    end
    object ActionEditCopy: TAction
      Category = 'Edit'
      Caption = 'C&opy'
      Enabled = False
      Hint = 
        'Copy|Copy the selected text into the clipboard without deleting ' +
        'it'
      ImageIndex = 6
      OnExecute = EditCopy
    end
    object ActionEditPaste: TAction
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Paste text from the clipboard'
      ImageIndex = 7
      OnExecute = EditPaste
    end
    object ActionEditSelectAll: TAction
      Category = 'Edit'
      Caption = '&Select All'
      Hint = 'Select All|Select the whole text in the editor'
      ShortCut = 16449
      OnExecute = EditSelectAll
    end
    object ActionEditIncreaseIndent: TAction
      Category = 'Edit'
      Caption = '&Increase Indent'
      Hint = 
        'Increase Indent|Insert tabs so that the text is moved to the rig' +
        'ht'
      ImageIndex = 19
      ShortCut = 16457
      OnExecute = EditIncreaseIndent
    end
    object ActionEditDecreaseIndent: TAction
      Category = 'Edit'
      Caption = '&Decrease Indent'
      Hint = 
        'Decrease Indent|Remove tabs and spaces so that the text is moved' +
        ' to the left'
      ImageIndex = 20
      ShortCut = 16452
      OnExecute = EditDecreaseIndent
    end
  end
  object MainMenu: TMainMenu
    Images = MainForm.ToolBarImages
    Top = 32
    object File1: TMenuItem
      Caption = '&File'
      Hint = 
        'File|Contains operations for creating, opening, and saving files' +
        '.'
      object Save1: TMenuItem
        Action = ActionFileSave
      end
      object SaveAs1: TMenuItem
        Action = ActionFileSaveAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object AddtoProject1: TMenuItem
        Action = ActionFileAddToProject
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Compile1: TMenuItem
        Action = ActionFileCompile
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Action = ActionFilePrint
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = ActionFileClose
        ShortCut = 16499
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      Hint = 'Edit|Contains operations to edit the current file.'
      object Undo1: TMenuItem
        Action = ActionEditUndo
      end
      object Redo1: TMenuItem
        Action = ActionEditRedo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Delete1: TMenuItem
        Action = ActionEditDelete
      end
      object Cut1: TMenuItem
        Action = ActionEditCut
      end
      object Copy1: TMenuItem
        Action = ActionEditCopy
      end
      object Paste1: TMenuItem
        Action = ActionEditPaste
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Action = ActionEditSelectAll
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object IncreaseIndent1: TMenuItem
        Action = ActionEditIncreaseIndent
      end
      object DecreaseIndent1: TMenuItem
        Action = ActionEditDecreaseIndent
      end
    end
    object Find1: TMenuItem
      Caption = 'F&ind'
      Hint = '&Find|Contains operations to find code you are looking for.'
      object Find2: TMenuItem
        Action = ActionFindFind
      end
      object Replace1: TMenuItem
        Action = ActionFindReplace
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Functions1: TMenuItem
        Action = ActionFindFunctions
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object OpenFileAtCursor1: TMenuItem
        Action = ActionFindOpenFile
      end
    end
  end
  object EditorPopup: TPopupMenu
    Top = 64
    object OpenFileatCursor2: TMenuItem
      Action = ActionFindOpenFile
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Undo2: TMenuItem
      Action = ActionEditUndo
    end
    object Redo2: TMenuItem
      Action = ActionEditRedo
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object Clear1: TMenuItem
      Action = ActionEditDelete
    end
    object Cut2: TMenuItem
      Action = ActionEditCut
    end
    object Copy2: TMenuItem
      Action = ActionEditCopy
    end
    object Paste2: TMenuItem
      Action = ActionEditPaste
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object SelectAll2: TMenuItem
      Action = ActionEditSelectAll
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object IncreaseIndent2: TMenuItem
      Action = ActionEditIncreaseIndent
    end
    object DecreaseIndent2: TMenuItem
      Action = ActionEditDecreaseIndent
    end
  end
  object FindDlg: TFindDialog
    OnClose = FindDlgClose
    OnFind = FindDlgFind
    Left = 64
    Top = 32
  end
  object ReplaceDlg: TReplaceDialog
    OnClose = FindDlgClose
    OnFind = FindDlgFind
    OnReplace = ReplaceDlgReplace
    Left = 64
    Top = 64
  end
  object FunctionPopup: TPopupMenu
    OnPopup = FunctionPopupPopup
    Top = 96
    object NoFunctionsItem: TMenuItem
      Caption = '(No Functions)'
      Enabled = False
    end
  end
end
