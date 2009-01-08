object NewsForm: TNewsForm
  Left = 168
  Top = 143
  ActiveControl = VisitButton
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'TIGCC News'
  ClientHeight = 193
  ClientWidth = 321
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 329
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 243
    Top = 17
    Width = 3
    Height = 13
    Anchors = [akTop, akRight]
    Caption = ':'
  end
  object NewsBox: TScrollBox
    Left = 16
    Top = 48
    Width = 289
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWindow
    ParentColor = False
    TabOrder = 3
    object RetreiveLabel: TLabel
      Left = 0
      Top = 0
      Width = 194
      Height = 13
      Caption = 'Downloading news from tigcc.ticalc.org...'
    end
  end
  object VisitButton: TButton
    Left = 120
    Top = 152
    Width = 81
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&Visit'
    ModalResult = 1
    TabOrder = 4
  end
  object CloseButton: TButton
    Left = 216
    Top = 152
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    ModalResult = 2
    TabOrder = 5
  end
  object ProxyCheckBox: TCheckBox
    Left = 16
    Top = 16
    Width = 86
    Height = 17
    Caption = '&Proxy Support:'
    TabOrder = 0
    OnClick = ProxyCheckBoxClick
  end
  object ProxyNameEdit: TEdit
    Left = 106
    Top = 14
    Width = 135
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 1
  end
  object ProxyPortEdit: TEdit
    Left = 248
    Top = 14
    Width = 57
    Height = 21
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 2
  end
  object RefreshButton: TButton
    Left = 24
    Top = 152
    Width = 81
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Refresh'
    Default = True
    TabOrder = 6
    OnClick = RefreshButtonClick
  end
  object NewsClient: TClientSocket
    Active = False
    ClientType = ctNonBlocking
    Host = 'tigcc.ticalc.org'
    Port = 80
    OnConnect = NewsClientConnect
    OnRead = NewsClientRead
    OnError = NewsClientError
  end
end