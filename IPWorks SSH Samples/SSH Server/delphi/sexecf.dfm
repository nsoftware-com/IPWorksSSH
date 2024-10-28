object FormSexec: TFormSexec
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'SExec Demo Application'
  ClientHeight = 518
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    445
    518)
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
    Top = 67
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label4: TLabel
    Left = 256
    Top = 43
    Width = 25
    Height = 13
    Caption = 'User:'
  end
  object Label5: TLabel
    Left = 8
    Top = 43
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
  object Label7: TLabel
    Left = 8
    Top = 67
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object txtCommand: TEdit
    Left = 8
    Top = 120
    Width = 346
    Height = 21
    TabOrder = 4
    Text = 'ls -la'
  end
  object txtPassword: TEdit
    Left = 311
    Top = 64
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    Text = 'test'
  end
  object txtUser: TEdit
    Left = 311
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'test'
  end
  object txtSSHServer: TEdit
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object btnExecute: TButton
    Left = 360
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 5
    OnClick = btnExecuteClick
  end
  object txtResponse: TMemo
    Left = 8
    Top = 176
    Width = 427
    Height = 339
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object txtPort: TEdit
    Left = 72
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '2222'
  end
  object iphSExec1: TiphSExec
    OnSSHServerAuthentication = iphSExec1SSHServerAuthentication
    OnStderr = iphSExec1Stderr
    OnStdout = iphSExec1Stdout
    Left = 208
    Top = 64
  end
end
