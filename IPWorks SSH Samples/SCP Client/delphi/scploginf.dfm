object FormScplogin: TFormScplogin
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'SCP Login'
  ClientHeight = 216
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 344
    Height = 192
    Caption = 'Server Information'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 56
      Height = 13
      Caption = 'Host Name:'
    end
    object Label2: TLabel
      Left = 16
      Top = 58
      Width = 26
      Height = 13
      Caption = 'User:'
    end
    object Label3: TLabel
      Left = 16
      Top = 92
      Width = 101
      Height = 13
      Caption = 'Authentication Type:'
    end
    object Label4: TLabel
      Left = 16
      Top = 126
      Width = 39
      Height = 13
      Caption = 'Key file:'
    end
    object lbPassword: TLabel
      Left = 16
      Top = 160
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object tbServer: TEdit
      Left = 88
      Top = 22
      Width = 248
      Height = 21
      TabOrder = 0
    end
    object tbUser: TEdit
      Left = 88
      Top = 56
      Width = 248
      Height = 21
      TabOrder = 1
      Text = 'Anonymous'
    end
    object cbAuthType: TComboBox
      Left = 128
      Top = 90
      Width = 208
      Height = 21
      TabOrder = 2
      Text = 'Password'
      OnChange = cbAuthTypeChange
      Items.Strings = (
        'Password'
        'Public key authentication (PEM)'
        'Public key authentication (PFX)')
    end
    object tbFilePath: TEdit
      Left = 88
      Top = 124
      Width = 160
      Height = 21
      Enabled = False
      TabOrder = 3
    end
    object tbPassword: TEdit
      Left = 88
      Top = 158
      Width = 248
      Height = 21
      PasswordChar = '*'
      TabOrder = 5
    end
    object btnBrowse: TButton
      Left = 256
      Top = 124
      Width = 80
      Height = 20
      Caption = 'Browse...'
      TabOrder = 4
      OnClick = btnBrowseClick
    end
  end
  object cmdLogin: TButton
    Left = 360
    Top = 16
    Width = 80
    Height = 20
    Caption = 'Login'
    ModalResult = 1
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 360
    Top = 48
    Width = 80
    Height = 20
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
