object FormTunnelConfig: TFormTunnelConfig
  Left = 0
  Top = 0
  Caption = 'Tunnel Configuration'
  ClientHeight = 319
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 66
    Height = 13
    Caption = 'Tunnel Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 163
    Height = 13
    Caption = 'Local Port (incoming connections):'
  end
  object Label3: TLabel
    Left = 8
    Top = 81
    Width = 48
    Height = 13
    Caption = 'SSH Host:'
  end
  object Label4: TLabel
    Left = 8
    Top = 108
    Width = 46
    Height = 13
    Caption = 'SSH Port:'
  end
  object Label5: TLabel
    Left = 8
    Top = 135
    Width = 48
    Height = 13
    Caption = 'SSH User:'
  end
  object Label6: TLabel
    Left = 8
    Top = 162
    Width = 72
    Height = 13
    Caption = 'SSH Password:'
  end
  object Label7: TLabel
    Left = 8
    Top = 205
    Width = 66
    Height = 13
    Caption = 'Remote Host:'
  end
  object Label8: TLabel
    Left = 8
    Top = 232
    Width = 64
    Height = 13
    Caption = 'Remote Port:'
  end
  object txtTunnelName: TEdit
    Left = 88
    Top = 8
    Width = 164
    Height = 21
    TabOrder = 0
    Text = 'MyTunnel'
  end
  object txtLocalPort: TEdit
    Left = 208
    Top = 35
    Width = 44
    Height = 21
    TabOrder = 1
    Text = '777'
  end
  object txtSSHHost: TEdit
    Left = 88
    Top = 78
    Width = 164
    Height = 21
    TabOrder = 2
  end
  object txtSSHPort: TEdit
    Left = 208
    Top = 105
    Width = 44
    Height = 21
    TabOrder = 3
    Text = '22'
  end
  object txtSSHUser: TEdit
    Left = 88
    Top = 132
    Width = 164
    Height = 21
    TabOrder = 4
  end
  object txtSSHPassword: TEdit
    Left = 88
    Top = 159
    Width = 164
    Height = 21
    TabOrder = 5
  end
  object txtRemoteHost: TEdit
    Left = 88
    Top = 202
    Width = 164
    Height = 21
    TabOrder = 6
  end
  object txtRemotePort: TEdit
    Left = 208
    Top = 229
    Width = 44
    Height = 21
    TabOrder = 7
  end
  object btnOk: TButton
    Left = 24
    Top = 283
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 147
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
end
