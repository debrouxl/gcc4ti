object OpenFileStatusForm: TOpenFileStatusForm
  Left = 177
  Top = 166
  ActiveControl = FileAnimate
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Opening File...'
  ClientHeight = 99
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object FileAnimate: TAnimate
    Left = 104
    Top = 44
    Width = 16
    Height = 16
    Active = True
    Center = False
    CommonAVI = aviFindComputer
    StopFrame = 8
  end
  object FileNameLabel: TPanel
    Left = 16
    Top = 12
    Width = 225
    Height = 23
    BevelInner = bvRaised
    BevelOuter = bvLowered
    FullRepaint = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
end
