object FormSexec: TFormSexec
  Left = 192
  Top = 107
  Width = 451
  Height = 550
  Caption = 'SExec Demo Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    443
    523)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 406
    Height = 26
    Caption = 
      'This demo shows how to use the SExec component to remotely and s' +
      'ecurely execute commands on a server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 75
    Height = 13
    Caption = 'SSH Command:'
  end
  object Label3: TLabel
    Left = 256
    Top = 88
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label4: TLabel
    Left = 280
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
  object Label6: TLabel
    Left = 8
    Top = 160
    Width = 51
    Height = 13
    Caption = 'Response:'
  end
  object txtCommand: TEdit
    Left = 8
    Top = 120
    Width = 329
    Height = 21
    TabOrder = 3
    Text = 'ls -la'
  end
  object txtPassword: TEdit
    Left = 312
    Top = 88
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object txtUser: TEdit
    Left = 312
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
  object btnExecute: TButton
    Left = 360
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 4
    OnClick = btnExecuteClick
  end
  object txtResponse: TMemo
    Left = 8
    Top = 176
    Width = 425
    Height = 337
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object iphSExec1: TiphSExec
    OnSSHServerAuthentication = iphSExec1SSHServerAuthentication
    OnStderr = iphSExec1Stderr
    OnStdout = iphSExec1Stdout
    Left = 208
    Top = 64
  end
end


