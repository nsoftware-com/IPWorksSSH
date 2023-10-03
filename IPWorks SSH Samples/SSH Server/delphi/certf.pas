unit certf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iphcore, iphtypes, iphcertmgr, StdCtrls;

type
  TFormCert = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    buttonOK: TButton;
    listCerts: TListBox;
    memoCertInfo: TMemo;
    Label3: TLabel;
    iphCertMgr1: TiphCertMgr;
    procedure FormCreate(Sender: TObject);
    procedure ShowCertDetail();
    procedure buttonOKClick(Sender: TObject);
    procedure iphCertMgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, CertSerialNumber: string;
      HasPrivateKey: Boolean);
    procedure listCertsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    certSubject : string;
  end;

var
  FormCert: TFormCert;

implementation

{$R *.dfm}

procedure TFormCert.buttonOKClick(Sender: TObject);
begin
    ShowCertDetail();
end;

procedure TFormCert.FormCreate(Sender: TObject);
begin
	iphCertMgr1.CertStore := 'MY';
	iphCertMgr1.ListStoreCertificates();
	if listCerts.Items.Count > 0 then
	begin
		listCerts.ItemIndex := 0;
		ShowCertDetail();
	end
  else
  begin
    buttonOK.Enabled :=false;
  end;
end;

procedure TFormCert.iphCertMgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, CertSerialNumber: string;
  HasPrivateKey: Boolean);
begin
	listCerts.AddItem(CertSubject, NIL);
end;

procedure TFormCert.listCertsClick(Sender: TObject);
begin
  ShowCertDetail();
end;

procedure TFormCert.ShowCertDetail;
  var
  index : Integer;
  str : String;
begin
	index :=  listCerts.ItemIndex;
	if (not (index = -1)) then
	begin
    str := listCerts.Items.Strings[index];
		iphCertMgr1.CertSubject := AnsiString(str);
		memoCertInfo.Text :='';

		memoCertInfo.Text := 'Issuer: ' + iphCertMgr1.CertIssuer + #13#10;
		memoCertInfo.Text := memoCertInfo.Text + 'Subject: ' + iphCertMgr1.CertSubject  + #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Version: ' + iphCertMgr1.CertVersion  + #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Serial Number: ' + iphCertMgr1.CertSerialNumber +  #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Signature Algorithm: ' + iphCertMgr1.CertSignatureAlgorithm +   #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Effective Date: ' + iphCertMgr1.CertEffectiveDate +  #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Expiration Date: ' + iphCertMgr1.CertExpirationDate +  #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Public Key Algorithm: ' + iphCertMgr1.CertPublicKeyAlgorithm +  #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Public Key Length: ' + IntToStr(iphCertMgr1.CertPublicKeyLength)  + #10 + #13  ;
		memoCertInfo.Text := memoCertInfo.Text + 'Public Key: ' + iphCertMgr1.CertPublicKey +  #10 + #13  ;

		certSubject := str;
	end;
end;
end.
