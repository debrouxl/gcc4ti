object ProgramOptionsForm: TProgramOptionsForm
  Left = 319
  Top = 299
  ActiveControl = PageController
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Program Options'
  ClientHeight = 337
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    297
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object PageController: TPageControl
    Left = 8
    Top = 8
    Width = 281
    Height = 289
    ActivePage = WelcomeSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object WelcomeSheet: TTabSheet
      Caption = 'Welcome'
      ImageIndex = 5
      DesignSize = (
        273
        261)
      object Label1: TLabel
        Left = 8
        Top = 32
        Width = 257
        Height = 41
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Welcome to the Program Options dialog. All of the selections you' +
          ' make here are sent to TIGCC as preprocessor macros.'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 8
        Top = 80
        Width = 257
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Moving the mouse over a specific option displays the macro name ' +
          'for that option. You can look up this macro in the documentation' +
          ' by right-clicking on the item. Always do this before using some' +
          'thing with an exclamation mark.'
        WordWrap = True
      end
    end
    object CalculatorSheet: TTabSheet
      Caption = 'Calculator'
      DesignSize = (
        273
        261)
      object Label2: TLabel
        Left = 8
        Top = 10
        Width = 257
        Height = 28
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Please select which calculator(s) the program will support:'
        WordWrap = True
      end
      object Label5: TLabel
        Left = 8
        Top = 96
        Width = 257
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'If you write a program for more than one calculator, consider th' +
          'e differences in screen size.'
        WordWrap = True
      end
      object Label12: TLabel
        Left = 8
        Top = 136
        Width = 257
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Optimizing calculator pseudo-constants can decrease program size' +
          ' and increase performance, but it implies that programs cannot b' +
          'e transferred between calculators of different type.'
        WordWrap = True
      end
      object TI89CheckBox: TCheckBox
        Left = 16
        Top = 40
        Width = 73
        Height = 17
        Hint = 'USE_TI89'
        Caption = 'TI-&89'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CalcCheckBoxClick
        OnMouseDown = ItemMouseDown
      end
      object TI92PlusCheckBox: TCheckBox
        Left = 16
        Top = 58
        Width = 73
        Height = 17
        Hint = 'USE_TI92PLUS'
        Caption = 'TI-&92 Plus'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CalcCheckBoxClick
        OnMouseDown = ItemMouseDown
      end
      object V200CheckBox: TCheckBox
        Left = 16
        Top = 76
        Width = 73
        Height = 17
        Hint = 'USE_V200'
        Caption = '&V200'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CalcCheckBoxClick
        OnMouseDown = ItemMouseDown
      end
      object OptimizeCalcConstsCheckBox: TCheckBox
        Left = 16
        Top = 196
        Width = 169
        Height = 17
        Hint = 'OPTIMIZE_CALC_CONSTS'
        Caption = 'Optimize Calculator Constants'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnMouseDown = ItemMouseDown
      end
    end
    object OSSheet: TTabSheet
      Caption = 'Operating System'
      ImageIndex = 1
      DesignSize = (
        273
        261)
      object Label3: TLabel
        Left = 8
        Top = 16
        Width = 257
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'You also have to select the operating system it will run under.'
        WordWrap = True
      end
      object NoStubRadioButton: TRadioButton
        Left = 16
        Top = 48
        Width = 233
        Height = 17
        Caption = '&Any/no kernel'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = ShellRadioButtonClick
        OnMouseDown = ItemMouseDown
      end
      object DoorsRadioButton: TRadioButton
        Left = 16
        Top = 68
        Width = 233
        Height = 17
        Hint = 'USE_KERNEL'
        Caption = 'Recent &Doors-compatible kernels'
        TabOrder = 1
        OnClick = ShellRadioButtonClick
        OnMouseDown = ItemMouseDown
      end
      object PreOSRadioButton: TRadioButton
        Left = 16
        Top = 88
        Width = 233
        Height = 17
        Hint = 'USE_PREOS_COMPRESSED_TABLES'
        Caption = '&PreOS with compressed reloc table support'
        TabOrder = 2
        OnClick = ShellRadioButtonClick
        OnMouseDown = ItemMouseDown
      end
      object MinAMSCheckBox: TCheckBox
        Left = 16
        Top = 136
        Width = 129
        Height = 17
        Hint = 'MIN_AMS'
        Caption = '&Minimum AMS version:'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = MinAMSCheckBoxClick
        OnMouseDown = ItemMouseDown
      end
      object MinAMSEdit: TEdit
        Left = 148
        Top = 134
        Width = 31
        Height = 21
        Hint = 'MIN_AMS'
        TabOrder = 4
        Text = '1.00'
        OnMouseDown = ItemMouseDown
      end
      object UnofficialOSSupportCheckBox: TCheckBox
        Left = 16
        Top = 176
        Width = 217
        Height = 17
        Hint = 'UNOFFICIAL_OS_SUPPORT'
        Caption = 'Force unofficial operating system support'
        TabOrder = 5
        OnMouseDown = ItemMouseDown
      end
    end
    object RelocFormatSheet: TTabSheet
      Caption = 'Reloc Format'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 8
        Top = 8
        Width = 249
        Height = 113
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          Left = 0
          Top = 0
          Width = 249
          Height = 17
          AutoSize = False
          Caption = 'Choose the relocation format that suits your needs:'
          WordWrap = True
        end
        object RelocAMSRadioButton: TRadioButton
          Left = 8
          Top = 16
          Width = 233
          Height = 16
          Caption = 'AMS (large calls, no stub)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object RelocKernelRadioButton: TRadioButton
          Left = 8
          Top = 32
          Width = 233
          Height = 16
          Hint = 'KERNEL_FORMAT_RELOCS'
          Caption = 'Kernel (medium calls, medium stub)'
          TabOrder = 1
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object RelocCompressedRadioButton: TRadioButton
          Left = 8
          Top = 48
          Width = 233
          Height = 16
          Hint = 'COMPRESSED_FORMAT_RELOCS'
          Caption = 'Compressed (smallest calls, large stub)'
          TabOrder = 2
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object RelocFLineJumpsCheckBox: TCheckBox
          Left = 8
          Top = 80
          Width = 233
          Height = 17
          Hint = 'USE_FLINE_JUMPS'
          Caption = 'F-Line (very small calls, no stub, but slow)'
          TabOrder = 3
          OnClick = RelocFLineJumpsCheckBoxClick
          OnMouseDown = ItemMouseDown
        end
        object RelocFLineJumps4ByteCheckBox: TCheckBox
          Left = 24
          Top = 96
          Width = 217
          Height = 17
          Hint = 'USE_4_BYTE_FLINE_JUMPS'
          Caption = '4-Byte F-Line (even smaller)  (!)'
          Enabled = False
          TabOrder = 4
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object RelocMlinkRadioButton: TRadioButton
          Left = 8
          Top = 64
          Width = 233
          Height = 16
          Hint = 'MLINK_FORMAT_RELOCS'
          Caption = 'Mlink (small calls, medium stub)'
          TabOrder = 5
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
      end
      object Panel3: TPanel
        Left = 8
        Top = 122
        Width = 249
        Height = 111
        BevelOuter = bvNone
        TabOrder = 1
        object Label15: TLabel
          Left = 0
          Top = 0
          Width = 249
          Height = 17
          AutoSize = False
          Caption = 'Choose the ROM call format that suits your needs:'
          WordWrap = True
        end
        object ROMCallDirectRadioButton: TRadioButton
          Left = 8
          Top = 16
          Width = 233
          Height = 16
          Caption = 'Direct (large calls, no stub)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object ROMCallKernelRadioButton: TRadioButton
          Left = 8
          Top = 32
          Width = 233
          Height = 16
          Hint = 'KERNEL_FORMAT_ROM_CALLS'
          Caption = 'Kernel (medium calls, medium stub)'
          TabOrder = 1
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object ROMCallCompressedRadioButton: TRadioButton
          Left = 8
          Top = 48
          Width = 233
          Height = 16
          Hint = 'COMPRESSED_FORMAT_ROM_CALLS'
          Caption = 'Compressed (smallest calls, large stub)'
          TabOrder = 2
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object ROMCallFLineRadioButton: TRadioButton
          Left = 8
          Top = 80
          Width = 233
          Height = 16
          Hint = 'USE_FLINE_ROM_CALLS'
          Caption = 'F-Line (very small calls, no stub, but slow)'
          TabOrder = 3
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
        object ROMCallOptimizedCheckBox: TCheckBox
          Left = 8
          Top = 96
          Width = 233
          Height = 17
          Hint = 'OPTIMIZE_ROM_CALLS'
          Caption = 'Optimized (medium calls, very small stub)  (!)'
          TabOrder = 4
          OnMouseDown = ItemMouseDown
        end
        object ROMCallMlinkRadioButton: TRadioButton
          Left = 8
          Top = 64
          Width = 233
          Height = 16
          Hint = 'MLINK_FORMAT_ROM_CALLS'
          Caption = 'Mlink (small calls, medium stub)'
          TabOrder = 5
          OnClick = RelocRadioButtonClick
          OnMouseDown = ItemMouseDown
        end
      end
      object InternalFLineEmulatorCheckBox: TCheckBox
        Left = 8
        Top = 238
        Width = 249
        Height = 17
        Hint = 'USE_INTERNAL_FLINE_EMULATOR'
        Caption = 'Use internal F-Line emulator'
        Enabled = False
        TabOrder = 2
        OnMouseDown = ItemMouseDown
      end
    end
    object BSSFormatSheet: TTabSheet
      Caption = 'BSS/Data Format'
      ImageIndex = 3
      object Panel1: TPanel
        Left = 8
        Top = 8
        Width = 249
        Height = 97
        BevelOuter = bvNone
        TabOrder = 0
        object Label6: TLabel
          Left = 0
          Top = 0
          Width = 249
          Height = 27
          AutoSize = False
          Caption = 
            'Choose the relocation format for BSS references (i.e. references' +
            ' to uninitialized global variables):'
          WordWrap = True
        end
        object BSSKernelRadioButton: TRadioButton
          Left = 8
          Top = 48
          Width = 233
          Height = 16
          Hint = 'KERNEL_FORMAT_BSS'
          Caption = 'Kernel (medium calls, medium stub)'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnMouseDown = ItemMouseDown
        end
        object BSSCompressedRadioButton: TRadioButton
          Left = 8
          Top = 64
          Width = 233
          Height = 16
          Hint = 'COMPRESSED_FORMAT_BSS'
          Caption = 'Compressed (smallest calls, large stub)'
          TabOrder = 2
          OnMouseDown = ItemMouseDown
        end
        object BSSMergeRadioButton: TRadioButton
          Left = 8
          Top = 32
          Width = 233
          Height = 16
          Hint = 'MERGE_BSS'
          Caption = 'Merge BSS section with data section'
          TabOrder = 0
          OnMouseDown = ItemMouseDown
        end
        object BSSMlinkRadioButton: TRadioButton
          Left = 8
          Top = 80
          Width = 233
          Height = 16
          Hint = 'MLINK_FORMAT_BSS'
          Caption = 'Mlink (small calls, medium stub)'
          TabOrder = 3
          OnMouseDown = ItemMouseDown
        end
      end
      object Panel4: TPanel
        Left = 8
        Top = 120
        Width = 249
        Height = 81
        BevelOuter = bvNone
        TabOrder = 1
        object Label8: TLabel
          Left = 0
          Top = 0
          Width = 249
          Height = 27
          AutoSize = False
          Caption = 
            'Choose the relocation format for references to a data variable, ' +
            'if it exists:'
          WordWrap = True
        end
        object DataVarKernelRadioButton: TRadioButton
          Left = 8
          Top = 32
          Width = 233
          Height = 16
          Hint = 'KERNEL_FORMAT_DATA_VAR'
          Caption = 'Kernel (medium calls, medium stub)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnMouseDown = ItemMouseDown
        end
        object DataVarCompressedRadioButton: TRadioButton
          Left = 8
          Top = 48
          Width = 233
          Height = 16
          Hint = 'COMPRESSED_FORMAT_DATA_VAR'
          Caption = 'Compressed (smallest calls, large stub)'
          TabOrder = 1
          OnMouseDown = ItemMouseDown
        end
        object DataVarMlinkRadioButton: TRadioButton
          Left = 8
          Top = 64
          Width = 233
          Height = 16
          Hint = 'MLINK_FORMAT_DATA_VAR'
          Caption = 'Mlink (small calls, medium stub)'
          TabOrder = 2
          OnMouseDown = ItemMouseDown
        end
      end
    end
    object HomeScreenSheet: TTabSheet
      Caption = 'Home Screen'
      ImageIndex = 4
      DesignSize = (
        273
        261)
      object Label11: TLabel
        Left = 8
        Top = 100
        Width = 257
        Height = 45
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Note that you have to push a value to the expression stack if yo' +
          'u use a custom return value, unless you throw an error.'
        WordWrap = True
      end
      object Label10: TLabel
        Left = 8
        Top = 48
        Width = 65
        Height = 13
        Caption = 'Return Value:'
      end
      object Label7: TLabel
        Left = 8
        Top = 8
        Width = 257
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'The following options affect the program when it is run from the' +
          ' Home Screen.'
        WordWrap = True
      end
      object LCDSaveCheckBox: TCheckBox
        Left = 16
        Top = 184
        Width = 153
        Height = 17
        Hint = 'SAVE_SCREEN'
        Caption = 'Save/restore &LCD contents'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnMouseDown = ItemMouseDown
      end
      object EnableErrorReturnCheckBox: TCheckBox
        Left = 16
        Top = 152
        Width = 169
        Height = 17
        Hint = 'ENABLE_ERROR_RETURN'
        Caption = 'Enable returning &errors to AMS'
        TabOrder = 1
        OnMouseDown = ItemMouseDown
      end
      object ReturnValueRadioButton: TRadioButton
        Left = 16
        Top = 81
        Width = 89
        Height = 17
        Hint = 'RETURN_VALUE'
        Caption = 'Custom &value'
        TabOrder = 2
        OnMouseDown = ItemMouseDown
      end
      object ReturnDoneRadioButton: TRadioButton
        Left = 16
        Top = 64
        Width = 89
        Height = 17
        Caption = #39#39'&Done'#39#39
        Checked = True
        TabOrder = 3
        TabStop = True
        OnMouseDown = ItemMouseDown
      end
    end
  end
  object CloseButton: TButton
    Left = 208
    Top = 304
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
