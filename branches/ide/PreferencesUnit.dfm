object PreferencesForm: TPreferencesForm
  Left = 165
  Top = 61
  ActiveControl = OKButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 365
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    321
    365)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 144
    Top = 332
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 232
    Top = 332
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageController: TPageControl
    Left = 8
    Top = 8
    Width = 305
    Height = 317
    ActivePage = GeneralSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    OnChange = PageControllerChange
    object GeneralSheet: TTabSheet
      Caption = '&General'
      object StopCompilationBox: TCheckBox
        Left = 16
        Top = 21
        Width = 201
        Height = 17
        Caption = '&Stop compilation at first file with errors'
        TabOrder = 0
      end
      object FlatButtonsCheckBox: TCheckBox
        Left = 16
        Top = 199
        Width = 73
        Height = 17
        Caption = '&Flat buttons'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object MenuBitmapsCheckBox: TCheckBox
        Left = 16
        Top = 216
        Width = 89
        Height = 17
        Caption = 'Men&u bitmaps'
        Checked = True
        State = cbChecked
        TabOrder = 11
      end
      object AutoSaveCheckBox: TCheckBox
        Left = 16
        Top = 139
        Width = 105
        Height = 17
        Caption = '&Auto-save project before compiling'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object OpenFolderBox: TCheckBox
        Left = 16
        Top = 54
        Width = 241
        Height = 17
        Caption = 'Display &message after successful compilation'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object DeleteObjectFilesBox: TCheckBox
        Left = 16
        Top = 88
        Width = 217
        Height = 17
        Caption = 'Delete o&bject files after successful linking'
        TabOrder = 4
      end
      object DeleteErrorsCheckBox: TCheckBox
        Left = 16
        Top = 174
        Width = 137
        Height = 17
        Caption = 'Delete o&verwritten errors'
        Checked = True
        State = cbChecked
        TabOrder = 9
      end
      object SplitFilesCheckBox: TCheckBox
        Left = 16
        Top = 105
        Width = 233
        Height = 17
        Caption = 'S&plit C source files for error position extraction'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object DeleteAssemblyFilesBox: TCheckBox
        Left = 16
        Top = 71
        Width = 233
        Height = 17
        Caption = 'Delete assembly files after successful &linking'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object AllowImplicitCheckBox: TCheckBox
        Left = 16
        Top = 122
        Width = 193
        Height = 17
        Caption = 'Allow &implicit declaration of functions'
        TabOrder = 6
      end
      object AutoNewsCheckBox: TCheckBox
        Left = 16
        Top = 157
        Width = 169
        Height = 17
        Caption = 'Download &headlines on startup'
        TabOrder = 8
      end
      object JumpToErrorBox: TCheckBox
        Left = 16
        Top = 37
        Width = 161
        Height = 17
        Caption = 'Automatically &jump to first error'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object TransferSheet: TTabSheet
      Caption = '&Transfer'
      ImageIndex = 3
      DesignSize = (
        297
        289)
      object TargetBox: TGroupBox
        Left = 16
        Top = 16
        Width = 265
        Height = 253
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Target'
        TabOrder = 0
        OnClick = TargetBoxClick
        DesignSize = (
          265
          253)
        object VTIBox: TRadioButton
          Left = 16
          Top = 82
          Width = 65
          Height = 17
          Hint = '89/92/92+ HW1/2, known bugs but faster'
          Caption = '&Virtual TI'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = TargetBoxClick
        end
        object TIEmuBox: TRadioButton
          Left = 16
          Top = 38
          Width = 65
          Height = 17
          Hint = 'all models, many more features but larger and slower'
          Caption = 'TiE&mu'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          TabStop = True
          OnClick = TargetBoxClick
        end
        object RealCalcBox: TRadioButton
          Left = 16
          Top = 126
          Width = 97
          Height = 17
          Caption = 'R&eal Calculator'
          TabOrder = 7
          OnClick = TargetBoxClick
        end
        object PortBox: TGroupBox
          Left = 35
          Top = 145
          Width = 78
          Height = 93
          Caption = 'Link &Port'
          Enabled = False
          TabOrder = 8
          object PortCOM1Box: TRadioButton
            Left = 10
            Top = 20
            Width = 49
            Height = 17
            Caption = 'COM&1'
            Enabled = False
            TabOrder = 0
          end
          object PortCOM2Box: TRadioButton
            Left = 10
            Top = 36
            Width = 49
            Height = 17
            Caption = 'COM&2'
            Enabled = False
            TabOrder = 1
          end
          object PortCOM3Box: TRadioButton
            Left = 10
            Top = 52
            Width = 49
            Height = 17
            Caption = 'COM&3'
            Enabled = False
            TabOrder = 2
          end
          object PortCOM4Box: TRadioButton
            Left = 10
            Top = 68
            Width = 49
            Height = 17
            Caption = 'COM&4'
            Enabled = False
            TabOrder = 3
          end
        end
        object CableBox: TGroupBox
          Left = 123
          Top = 145
          Width = 126
          Height = 93
          Caption = 'C&able Type'
          Enabled = False
          TabOrder = 9
          object CableBlackBox: TRadioButton
            Left = 10
            Top = 28
            Width = 103
            Height = 17
            Caption = 'Blac&k Link Cable'
            Enabled = False
            TabOrder = 0
          end
          object CableGrayBox: TRadioButton
            Left = 10
            Top = 52
            Width = 103
            Height = 17
            Caption = 'Gray &Link Cable'
            Enabled = False
            TabOrder = 1
          end
        end
        object NoneBox: TRadioButton
          Left = 16
          Top = 20
          Width = 49
          Height = 17
          Caption = '&None'
          TabOrder = 0
          OnClick = TargetBoxClick
        end
        object VTIPathEdit: TEdit
          Left = 35
          Top = 100
          Width = 158
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
        end
        object TIEmuPathEdit: TEdit
          Left = 35
          Top = 56
          Width = 158
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object VTIPathBrowseButton: TButton
          Left = 196
          Top = 100
          Width = 53
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '&Browse...'
          TabOrder = 6
          OnClick = VTIPathBrowseButtonClick
        end
        object TIEmuPathBrowseButton: TButton
          Left = 196
          Top = 56
          Width = 53
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '&Browse...'
          TabOrder = 3
          OnClick = TIEmuPathBrowseButtonClick
        end
      end
    end
    object EditorSheet: TTabSheet
      Caption = 'Edito&r'
      ImageIndex = 1
      DesignSize = (
        297
        289)
      object Label2: TLabel
        Left = 16
        Top = 17
        Width = 55
        Height = 13
        Caption = 'C Tab &Size:'
        FocusControl = CTabSizeEdit
      end
      object Label3: TLabel
        Left = 16
        Top = 97
        Width = 54
        Height = 13
        Caption = 'Editor Font:'
      end
      object Label4: TLabel
        Left = 120
        Top = 17
        Width = 71
        Height = 13
        Caption = '&ASM Tab Size:'
        FocusControl = ASMTabSizeEdit
      end
      object CTabSizeEdit: TEdit
        Left = 76
        Top = 15
        Width = 29
        Height = 21
        TabOrder = 0
        Text = '2'
      end
      object ColorCheckBox: TCheckBox
        Left = 16
        Top = 45
        Width = 141
        Height = 17
        Caption = 'Specify &background color'
        TabOrder = 2
        OnClick = ColorCheckBoxClick
      end
      object ChangeColorButton: TButton
        Left = 16
        Top = 65
        Width = 78
        Height = 25
        Caption = 'C&hange...'
        Enabled = False
        TabOrder = 3
        OnClick = ChangeColorButtonClick
      end
      object ChangeFontButton: TButton
        Left = 16
        Top = 141
        Width = 81
        Height = 25
        Caption = 'Cha&nge...'
        TabOrder = 5
        OnClick = ChangeFontButtonClick
      end
      object FontLabel: TPanel
        Left = 16
        Top = 111
        Width = 265
        Height = 27
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Caption = 'FixedSys'
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Fixedsys'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object ASMTabSizeEdit: TEdit
        Left = 196
        Top = 15
        Width = 29
        Height = 21
        TabOrder = 1
        Text = '8'
      end
      object OnFlyCheckBox: TCheckBox
        Left = 16
        Top = 170
        Width = 153
        Height = 17
        Caption = 'On-the-&fly syntax highlighting'
        TabOrder = 6
      end
      object AutoBlockCheckBox: TCheckBox
        Left = 16
        Top = 202
        Width = 145
        Height = 17
        Caption = 'A&utomatic '#39'{...}'#39' blocks in C'
        Checked = True
        State = cbChecked
        TabOrder = 8
      end
      object DragDropEditCheckBox: TCheckBox
        Left = 16
        Top = 186
        Width = 121
        Height = 17
        Caption = 'Drag and dro&p editing'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object RemoveTrailingSpcCheckBox: TCheckBox
        Left = 16
        Top = 218
        Width = 177
        Height = 17
        Caption = 'R&emove trailing spaces from lines'
        TabOrder = 9
      end
    end
    object SyntaxHighlightingSheet: TTabSheet
      Caption = 'S&yntax Highlighting'
      ImageIndex = 2
      object Label5: TLabel
        Left = 16
        Top = 16
        Width = 56
        Height = 13
        Caption = 'Settings &for:'
        FocusControl = LanguageSelectionBox
      end
      object LanguageSelectionBox: TComboBox
        Left = 16
        Top = 32
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = LanguageSelectionBoxChange
        Items.Strings = (
          'C Files'
          'GNU ASM Files')
      end
      object SyntaxEnabledBox: TCheckBox
        Left = 144
        Top = 34
        Width = 65
        Height = 17
        Caption = 'Ena&bled'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = SyntaxEnabledBoxClick
      end
      object Button1: TButton
        Left = 16
        Top = 64
        Width = 125
        Height = 25
        Caption = '&Number Color...'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 156
        Top = 64
        Width = 125
        Height = 25
        Caption = 'N&umber Style...'
        TabOrder = 3
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 16
        Top = 96
        Width = 125
        Height = 25
        Caption = '&Symbol Color...'
        TabOrder = 4
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 156
        Top = 96
        Width = 125
        Height = 25
        Caption = 'Sy&mbol Style...'
        TabOrder = 5
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 16
        Top = 128
        Width = 125
        Height = 25
        Caption = '&Parenthesis Colors...'
        TabOrder = 6
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 156
        Top = 128
        Width = 125
        Height = 25
        Caption = 'P&arenthesis Style...'
        TabOrder = 7
        OnClick = Button6Click
      end
      object ObjectTree: TTreeView
        Left = 16
        Top = 160
        Width = 193
        Height = 69
        Indent = 19
        RightClickSelect = True
        ShowRoot = False
        TabOrder = 8
        OnChange = ObjectTreeChange
        OnDblClick = EditItem
        OnEdited = ObjectTreeEdited
        OnEditing = ObjectTreeEditing
        OnKeyDown = ObjectTreeKeyDown
        Items.Data = {
          01000000250000000000000000000000FFFFFFFFFFFFFFFF0000000002000000
          0C486967686C69676874696E67260000000000000000000000FFFFFFFFFFFFFF
          FF00000000000000000D437573746F6D205374796C6573230000000000000000
          000000FFFFFFFFFFFFFFFF00000000000000000A576F7264204C69737473}
      end
      object Button7: TButton
        Left = 216
        Top = 160
        Width = 65
        Height = 21
        Caption = 'Ne&w Style'
        TabOrder = 9
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 216
        Top = 184
        Width = 65
        Height = 21
        Caption = 'New &List'
        TabOrder = 10
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 216
        Top = 208
        Width = 65
        Height = 21
        Caption = 'Ed&it...'
        TabOrder = 11
        OnClick = EditItem
      end
      object ResetButton: TButton
        Left = 216
        Top = 32
        Width = 65
        Height = 21
        Caption = 'R&eset'
        TabOrder = 12
        OnClick = ResetButtonClick
      end
    end
  end
  object ColorDlg: TColorDialog
    Options = [cdAnyColor]
    Left = 224
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdFixedPitchOnly]
    Left = 256
  end
  object OKTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = OKTimerTimer
    Left = 192
  end
  object BrowseDlg: TOpenDialog
    Filter = 'Programs (*.exe)|*.exe|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Browse'
    Left = 288
  end
end
