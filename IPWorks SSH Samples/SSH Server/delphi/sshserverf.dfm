object FormSshserver: TFormSshserver
  Left = 529
  Top = 278
  BorderStyle = bsDialog
  Caption = 'Sshserver Demo'
  ClientHeight = 505
  ClientWidth = 676
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 51
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 658
    Height = 33
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'This demo shows how to set up an SSH server on your computer.  Y' +
        'ou need to specify the port you want the server to listen on, an' +
        'd '
      'then START the server.')
    TabOrder = 0
  end
  object textPort: TEdit
    Left = 48
    Top = 48
    Width = 113
    Height = 21
    TabOrder = 1
    Text = '2222'
  end
  object buttonStart: TButton
    Left = 561
    Top = 47
    Width = 105
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = buttonStartClick
  end
  object memoResponse: TMemo
    Left = 8
    Top = 78
    Width = 658
    Height = 420
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object iphSSHServer1: TiphSSHServer
    OnConnected = iphSSHServer1Connected
    OnDisconnected = iphSSHServer1Disconnected
    OnSSHChannelClosed = iphSSHServer1SSHChannelClosed
    OnSSHChannelDataIn = iphSSHServer1SSHChannelDataIn
    OnSSHChannelOpened = iphSSHServer1SSHChannelOpened
    OnSSHChannelOpenRequest = iphSSHServer1SSHChannelOpenRequest
    OnSSHChannelRequest = iphSSHServer1SSHChannelRequest
    OnSSHChannelRequested = iphSSHServer1SSHChannelRequested
    OnSSHServiceRequest = iphSSHServer1SSHServiceRequest
    OnSSHStatus = iphSSHServer1SSHStatus
    OnSSHUserAuthRequest = iphSSHServer1SSHUserAuthRequest
    Left = 288
    Top = 32
  end
end


