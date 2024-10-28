unit signcsrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iphcore, iphtypes, iphcertmgr;

type
  TFormSigncsr = class(TForm)
    tCSR: TMemo;
    tSignedCSR: TMemo;
    bSign: TButton;
    bOK: TButton;
    bCancel: TButton;
    Label1: TLabel;
    cbIssuer: TComboBox;
    Label3: TLabel;
    Label2: TLabel;
    tSerialNumber: TEdit;
    Label4: TLabel;
    certMgr: TiphCertMgr;
    procedure certMgrCertList(Sender: TObject; CertEncoded: string; CertEncodedB: TBytes;
      const CertSubject, CertIssuer, CertSerialNumber: String;
      HasPrivateKey: Boolean);
    procedure bSignClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure tSerialNumberChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSigncsr: TFormSigncsr;

implementation

{$R *.dfm}

procedure TFormSigncsr.certMgrCertList(Sender: TObject; CertEncoded: string; CertEncodedB: TBytes;
  const CertSubject, CertIssuer, CertSerialNumber: String;
  HasPrivateKey: Boolean);
begin
  if HasPrivateKey then cbIssuer.Items.Add(CertSubject);
end;

procedure TFormSigncsr.bSignClick(Sender: TObject);
begin
  certMgr.CertSubject := cbIssuer.Text;
  tSignedCSR.Text :=   certMgr.signcsr(TEncoding.Default.GetBytes(tCSR.Text), StrToInt(tSerialNumber.Text))
end;

procedure TFormSigncsr.bOKClick(Sender: TObject);
begin
        Close();
end;

procedure TFormSigncsr.bCancelClick(Sender: TObject);
begin
        Close();
end;

procedure TFormSigncsr.tSerialNumberChange(Sender: TObject);
var MyNumber: integer;
begin
    try
        MyNumber := StrToInt(tSerialNumber.Text);
    except on E:Exception do begin
        ShowMessage('Please enter only numeric data for the serial number.');
        tSerialNumber.Text := '';
        end;
    end;
end;

procedure TFormSigncsr.FormActivate(Sender: TObject);
begin
  tSerialNumber.Text := IntToStr(StrToInt(tSerialNumber.Text) + 1);
  cbIssuer.items.Clear;
  certMgr.ListStoreCertificates;
  cbIssuer.ItemIndex := 0;
end;

end.
