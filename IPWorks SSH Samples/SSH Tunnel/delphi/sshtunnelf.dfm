object FormSSHTunnel: TFormSSHTunnel
  Left = 0
  Top = 0
  Caption = 'SSHTunnel Demo'
  ClientHeight = 300
  ClientWidth = 635
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
    Top = 31
    Width = 607
    Height = 26
    Caption = 
      'This demo shows how to use the SSHTunnel component to accept inc' +
      'oming plain text connections and tunnel them to a secure SSH hos' +
      't. To use the demo, create a "New" tunnel, or "Edit" an existing' +
      ' tunnel below and click "Start".'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object TabControl1: TTabControl
    Left = 8
    Top = 72
    Width = 521
    Height = 220
    TabOrder = 0
    Tabs.Strings = (
      'Tunnels'
      'Log')
    TabIndex = 0
    OnChange = TabControl1Change
    object Log1: TMemo
      Left = 3
      Top = 24
      Width = 510
      Height = 193
      TabOrder = 1
      Visible = False
    end
    object sgTunnels: TStringGrid
      Left = 3
      Top = 24
      Width = 510
      Height = 193
      ColCount = 6
      DefaultColWidth = 83
      RowCount = 1
      FixedRows = 0
      TabOrder = 0
      OnClick = sgTunnelsClick
    end
  end
  object btnNew: TButton
    Left = 535
    Top = 96
    Width = 92
    Height = 25
    Caption = 'New'
    TabOrder = 1
    OnClick = btnNewClick
  end
  object btnEdit: TButton
    Left = 535
    Top = 127
    Width = 92
    Height = 25
    Caption = 'Edit'
    TabOrder = 2
    OnClick = btnEditClick
  end
  object btnRemove: TButton
    Left = 535
    Top = 158
    Width = 92
    Height = 25
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 635
    Height = 25
    ButtonHeight = 21
    ButtonWidth = 43
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 4
    object tbtnStart: TToolButton
      Left = 0
      Top = 0
      Caption = 'Start'
      ImageIndex = 0
      OnClick = tbtnStartClick
    end
    object tbtnRestart: TToolButton
      Left = 43
      Top = 0
      Caption = 'Restart'
      Enabled = False
      ImageIndex = 1
      OnClick = tbtnRestartClick
    end
    object tbtnStop: TToolButton
      Left = 86
      Top = 0
      Caption = 'Stop'
      Enabled = False
      ImageIndex = 2
      OnClick = tbtnStopClick
    end
  end
  object newtunnel: TiphSSHTunnel
    OnConnected = newtunnelConnected
    OnDisconnected = newtunnelDisconnected
    OnError = newtunnelError
    OnSSHServerAuthentication = newtunnelSSHServerAuthentication
    OnSSHStatus = newtunnelSSHStatus
    Left = 576
    Top = 216
  end
end


