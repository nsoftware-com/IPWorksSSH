object FormSshell: TFormSshell
  Left = 192
  Top = 107
  Width = 451
  Height = 512
  Caption = 'SShell Demo Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    443
    485)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 374
    Height = 26
    Caption = 
      'This demo shows how to use the SShell component. After connectin' +
      'g type any commands into the text area to send commands to the s' +
      'erver.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 224
    Top = 88
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label4: TLabel
    Left = 224
    Top = 64
    Width = 25
    Height = 13
    Caption = 'User:'
  end
  object Label5: TLabel
    Left = 8
    Top = 64
    Width = 59
    Height = 13
    Caption = 'SSH Server:'
  end
  object txtPassword: TEdit
    Left = 288
    Top = 88
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object txtUser: TEdit
    Left = 288
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object txtSSHServer: TEdit
    Left = 72
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object btnConnect: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = '&Connect'
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object txtOutput: TMemo
    Left = 8
    Top = 136
    Width = 425
    Height = 338
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    OnKeyPress = txtOutputKeyPress
  end
  object iphSShell1: TiphSShell
    OnSSHServerAuthentication = iphSShell1SSHServerAuthentication
    OnStderr = iphSShell1Stderr
    OnStdout = iphSShell1Stdout
    Left = 152
    Top = 96
  end
end


