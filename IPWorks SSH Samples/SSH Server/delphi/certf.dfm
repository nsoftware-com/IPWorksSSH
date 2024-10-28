object FormCert: TFormCert
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Certificate Selector'
  ClientHeight = 410
  ClientWidth = 656
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
    Top = 8
    Width = 103
    Height = 13
    Caption = 'Certificate Store:  MY'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 101
    Height = 13
    Caption = 'Available Certificates'
  end
  object Label3: TLabel
    Left = 8
    Top = 199
    Width = 73
    Height = 13
    Caption = 'Certificate Info'
  end
  object buttonOK: TButton
    Left = 568
    Top = 37
    Width = 75
    Height = 21
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = buttonOKClick
  end
  object listCerts: TListBox
    Left = 10
    Top = 64
    Width = 633
    Height = 129
    ItemHeight = 13
    TabOrder = 1
    OnClick = listCertsClick
  end
  object memoCertInfo: TMemo
    Left = 8
    Top = 218
    Width = 635
    Height = 181
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object iphCertMgr1: TiphCertMgr
    CertStore = 'MY'
    OnCertList = iphCertMgr1CertList
    Left = 312
    Top = 24
  end
end
