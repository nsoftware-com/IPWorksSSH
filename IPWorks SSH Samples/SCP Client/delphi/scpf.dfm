object FormScp: TFormScp
  Left = 0
  Top = 0
  Anchors = [akLeft, akRight, akBottom]
  BorderStyle = bsSingle
  Caption = 'SCP Demo'
  ClientHeight = 460
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 9
    Width = 593
    Height = 26
    Caption = 
      'This demo shows how to use the SCP component to securely copy fi' +
      'les to and from a remote server. The SExec component is used here' +
      ' to list the specified remote directory.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 46
    Width = 248
    Height = 288
    Caption = 'Local Host'
    TabOrder = 0
    object cboLocalPath: TComboBox
      Left = 8
      Top = 16
      Width = 232
      Height = 21
      TabOrder = 0
    end
    object lvwLocalDir: TListView
      Left = 8
      Top = 40
      Width = 232
      Height = 240
      Columns = <
        item
          Caption = 'File Name'
          MaxWidth = 160
          MinWidth = 160
          Width = 160
        end
        item
          Caption = 'Size'
          MaxWidth = 65
          MinWidth = 65
          Width = 65
        end>
      TabOrder = 1
      ViewStyle = vsReport
      OnDblClick = lvwLocalDirDblClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 295
    Top = 46
    Width = 298
    Height = 288
    Caption = 'Remote Host'
    TabOrder = 1
    object tbRemotePath: TEdit
      Left = 6
      Top = 17
      Width = 232
      Height = 21
      TabOrder = 0
      Text = '~/'
    end
    object btnList: TButton
      Left = 244
      Top = 14
      Width = 47
      Height = 23
      Caption = 'List'
      TabOrder = 1
      OnClick = btnListClick
    end
    object lvwRemoteDir: TListView
      Left = 7
      Top = 40
      Width = 284
      Height = 240
      Columns = <
        item
          Caption = 'File Name'
          MaxWidth = 280
          MinWidth = 280
          Width = 280
        end>
      TabOrder = 2
      ViewStyle = vsReport
    end
  end
  object cmdUpload: TButton
    Left = 261
    Top = 144
    Width = 24
    Height = 24
    Caption = '->'
    TabOrder = 2
    OnClick = cmdUploadClick
  end
  object cmdDownload: TButton
    Left = 261
    Top = 176
    Width = 24
    Height = 24
    Caption = '<-'
    TabOrder = 3
    OnClick = cmdDownloadClick
  end
  object cmdAbortTransfer: TButton
    Left = 88
    Top = 421
    Width = 80
    Height = 24
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = cmdAbortTransferClick
  end
  object cmdExit: TButton
    Left = 176
    Top = 421
    Width = 80
    Height = 24
    Caption = 'Exit'
    TabOrder = 7
    OnClick = cmdExitClick
  end
  object cmdConnect: TButton
    Left = 8
    Top = 421
    Width = 72
    Height = 24
    Caption = 'Connect'
    TabOrder = 5
    OnClick = cmdConnectClick
  end
  object tbStatus: TMemo
    Left = 8
    Top = 343
    Width = 585
    Height = 72
    Lines.Strings = (
      'tbStatus')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
    OnChange = tbStatusChange
  end
  object iphSExec1: TiphSExec
    OnSSHServerAuthentication = iphSExec1SSHServerAuthentication
    OnStderr = iphSExec1Stderr
    OnStdout = iphSExec1Stdout
    Left = 272
    Top = 416
  end
  object iphSCP1: TiphSCP
    OnSSHServerAuthentication = iphSCP1SSHServerAuthentication
    OnSSHStatus = iphSCP1SSHStatus
    OnTransfer = iphSCP1Transfer
    Left = 336
    Top = 416
  end
end


