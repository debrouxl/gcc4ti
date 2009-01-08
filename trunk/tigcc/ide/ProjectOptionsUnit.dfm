object ProjectOptionsForm: TProjectOptionsForm
  Left = 587
  Top = 357
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Project Options'
  ClientHeight = 289
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    361
    289)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 184
    Top = 256
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 272
    Top = 256
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
    Width = 345
    Height = 241
    ActivePage = GeneralSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object GeneralSheet: TTabSheet
      Caption = 'Ge&neral'
      DesignSize = (
        337
        213)
      object TargetGroupBox: TGroupBox
        Left = 16
        Top = 4
        Width = 305
        Height = 197
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Target'
        TabOrder = 0
        DesignSize = (
          305
          197)
        object PackVarEditLabel: TLabel
          Left = 50
          Top = 116
          Width = 109
          Height = 13
          Caption = 'On-calc &variable name:'
          Enabled = False
          FocusControl = PackVarEdit
        end
        object DataVarEditLabel: TLabel
          Left = 50
          Top = 60
          Width = 109
          Height = 13
          Caption = 'On-calc var&iable name:'
          Enabled = False
          FocusControl = DataVarEdit
        end
        object DataVarCopyLabel: TLabel
          Left = 50
          Top = 80
          Width = 60
          Height = 13
          Caption = 'Create copy:'
          Enabled = False
        end
        object ExecutableRadioButton: TRadioButton
          Left = 16
          Top = 20
          Width = 137
          Height = 17
          Caption = '&Regular Program (*.??z)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = TargetRadioButtonClick
        end
        object ArchiveRadioButton: TRadioButton
          Left = 16
          Top = 172
          Width = 129
          Height = 17
          Caption = '&Function Archive (*.a)'
          TabOrder = 8
          OnClick = TargetRadioButtonClick
        end
        object PackCheckBox: TCheckBox
          Left = 32
          Top = 96
          Width = 145
          Height = 17
          Caption = 'Compre&ss program (*.??y)'
          TabOrder = 4
          OnClick = PackCheckBoxClick
        end
        object PackVarEdit: TEdit
          Left = 168
          Top = 112
          Width = 121
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 5
          OnChange = VarEditChange
        end
        object DataVarCheckBox: TCheckBox
          Left = 32
          Top = 40
          Width = 185
          Height = 17
          Caption = 'External &data variable (*-data.??y)'
          TabOrder = 1
          OnClick = DataVarCheckBoxClick
        end
        object DataVarEdit: TEdit
          Left = 168
          Top = 56
          Width = 121
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 2
          OnChange = VarEditChange
        end
        object DataVarCopyPanel: TPanel
          Left = 112
          Top = 81
          Width = 178
          Height = 15
          BevelOuter = bvNone
          Enabled = False
          FullRepaint = False
          TabOrder = 3
          object DataVarCopyNeverRadioButton: TRadioButton
            Left = 2
            Top = 0
            Width = 46
            Height = 15
            Caption = 'n&ever'
            Enabled = False
            TabOrder = 0
          end
          object DataVarCopyIfArchivedRadioButton: TRadioButton
            Left = 53
            Top = 0
            Width = 68
            Height = 15
            Caption = 'if arc&hived'
            Checked = True
            Enabled = False
            TabOrder = 1
            TabStop = True
          end
          object DataVarCopyAlwaysRadioButton: TRadioButton
            Left = 127
            Top = 0
            Width = 51
            Height = 15
            Caption = '&always'
            Enabled = False
            TabOrder = 2
          end
        end
        object FargoRadioButton: TRadioButton
          Left = 16
          Top = 154
          Width = 201
          Height = 17
          Caption = 'Far&go Program (*.92p)  (experimental)'
          TabOrder = 7
          Visible = False
          OnClick = TargetRadioButtonClick
        end
        object FlashOSRadioButton: TRadioButton
          Left = 16
          Top = 136
          Width = 209
          Height = 17
          Caption = 'Flash Operating S&ystem  (experimental)'
          TabOrder = 6
          Visible = False
          OnClick = TargetRadioButtonClick
        end
      end
    end
    object CompilationSheet: TTabSheet
      Caption = 'Co&mpilation'
      ImageIndex = 1
      DesignSize = (
        337
        213)
      object DebugInfoCheckBox: TCheckBox
        Left = 16
        Top = 149
        Width = 153
        Height = 17
        Caption = 'Generate &debug information'
        TabOrder = 0
      end
      object SwitchesGroupBox: TGroupBox
        Left = 16
        Top = 16
        Width = 305
        Height = 121
        Anchors = [akLeft, akTop, akRight]
        Caption = 'S&witches'
        TabOrder = 1
        DesignSize = (
          305
          121)
        object GCCSwitchesEditLabel: TLabel
          Left = 16
          Top = 28
          Width = 71
          Height = 13
          Caption = '&GCC Switches:'
          FocusControl = GCCSwitchesEdit
        end
        object AsSwitchesEditLabel: TLabel
          Left = 16
          Top = 57
          Width = 61
          Height = 13
          Caption = 'A&s Switches:'
          FocusControl = AsSwitchesEdit
        end
        object AsmSwitchesEditLabel: TLabel
          Left = 16
          Top = 86
          Width = 74
          Height = 13
          Caption = '&A68k Switches:'
          FocusControl = AsmSwitchesEdit
        end
        object GCCSwitchesEdit: TEdit
          Left = 104
          Top = 24
          Width = 185
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object AsSwitchesEdit: TEdit
          Left = 104
          Top = 53
          Width = 185
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object AsmSwitchesEdit: TEdit
          Left = 104
          Top = 82
          Width = 185
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
      end
      object ProgramOptionsButton: TButton
        Left = 216
        Top = 176
        Width = 105
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'P&rogram Options...'
        TabOrder = 2
        OnClick = ProgramOptionsButtonClick
      end
    end
    object LinkingSheet: TTabSheet
      Caption = '&Linking'
      ImageIndex = 2
      DesignSize = (
        337
        213)
      object StdLibCheckBox: TCheckBox
        Left = 16
        Top = 151
        Width = 193
        Height = 17
        Caption = 'Lin&k against standard library (tigcc.a)'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object OptimizationGroupBox: TGroupBox
        Left = 16
        Top = 10
        Width = 305
        Height = 133
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Optimization'
        TabOrder = 0
        object OptimizeNOPsCheckBox: TCheckBox
          Left = 13
          Top = 20
          Width = 49
          Height = 17
          Caption = 'NOP&s'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object OptimizeReturnsCheckBox: TCheckBox
          Left = 13
          Top = 40
          Width = 113
          Height = 17
          Caption = '&Return Sequences'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object OptimizeBranchesCheckBox: TCheckBox
          Left = 13
          Top = 60
          Width = 65
          Height = 17
          Caption = '&Branches'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object OptimizeMovesCheckBox: TCheckBox
          Left = 133
          Top = 20
          Width = 161
          Height = 17
          Caption = 'Move/Load/Pus&h Instructions'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object OptimizeTestsCheckBox: TCheckBox
          Left = 133
          Top = 40
          Width = 145
          Height = 17
          Caption = '&Test/Compare Instructions'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
        object OptimizeCalculationsCheckBox: TCheckBox
          Left = 133
          Top = 60
          Width = 129
          Height = 17
          Caption = 'C&alculation Instructions'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object RemoveUnusedSectionsCheckBox: TCheckBox
          Left = 21
          Top = 86
          Width = 145
          Height = 17
          Caption = 'Remove &unused sections'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object CutUnusedRangesCheckBox: TCheckBox
          Left = 173
          Top = 86
          Width = 113
          Height = 17
          Caption = 'Cut unuse&d ranges'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object ReorderSectionsCheckBox: TCheckBox
          Left = 21
          Top = 106
          Width = 105
          Height = 17
          Caption = 'R&eorder sections'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object MergeConstantsCheckBox: TCheckBox
          Left = 173
          Top = 106
          Width = 105
          Height = 17
          Caption = 'Mer&ge Constants'
          Checked = True
          State = cbChecked
          TabOrder = 9
        end
      end
      object OutputBinCheckBox: TCheckBox
        Left = 16
        Top = 185
        Width = 273
        Height = 17
        Caption = 'Output &variable image without wrapper (binary mode)'
        TabOrder = 3
      end
      object InitBSSCheckBox: TCheckBox
        Left = 16
        Top = 168
        Width = 121
        Height = 17
        Caption = '&Initialize BSS section'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
    object PostBuildSheet: TTabSheet
      Caption = '&Post-Build'
      ImageIndex = 3
      DesignSize = (
        337
        213)
      object ProcessFileGroupBox: TGroupBox
        Left = 16
        Top = 16
        Width = 305
        Height = 73
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Po&st-Build Processing'
        TabOrder = 0
        DesignSize = (
          305
          73)
        object ProcessFileEditLabel: TLabel
          Left = 16
          Top = 25
          Width = 83
          Height = 13
          Caption = 'Call &after building:'
          FocusControl = ProcessFileEdit
        end
        object ProcessFileEdit: TEdit
          Left = 16
          Top = 42
          Width = 273
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object BrowseButton: TButton
          Left = 216
          Top = 17
          Width = 73
          Height = 23
          Anchors = [akTop, akRight]
          Caption = '&Browse...'
          TabOrder = 1
          OnClick = BrowseButtonClick
        end
      end
      object ExecutionGroupBox: TGroupBox
        Left = 16
        Top = 104
        Width = 305
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Execution'
        TabOrder = 1
        DesignSize = (
          305
          57)
        object CommandLineEditLabel: TLabel
          Left = 16
          Top = 24
          Width = 56
          Height = 13
          Caption = 'Pa&rameters:'
          FocusControl = CommandLineEdit
        end
        object CommandLineEdit: TEdit
          Left = 80
          Top = 20
          Width = 209
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
    end
  end
  object BrowseDialog: TOpenDialog
    Filter = 
      'Executables (*.exe;*.com)|*.exe;*.com|Batch Files (*.bat)|*.bat|' +
      'All Executables (*.exe;*.com;*.bat)|*.exe;*.com;*.bat|All Files ' +
      '(*.*)|*.*'
    FilterIndex = 3
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Browse'
  end
end
