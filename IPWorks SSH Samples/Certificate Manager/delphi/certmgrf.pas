(*
 * IPWorks SSH 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SSH in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksssh
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit certmgrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, iphcore, iphtypes, iphcertmgr, createcertf, signcsrf,
  generatecsrf, importcsrf;

type
  TFormCertmgr = class(TForm)
    GroupBox1: TGroupBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Certificates1: TMenuItem;
    CAFunctions1: TMenuItem;
    DeleteCertificate1: TMenuItem;
    N1: TMenuItem;
    ImportCertificate1: TMenuItem;
    ExportCertificate1: TMenuItem;
    N2: TMenuItem;
    Refresh1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    CreateSignCertificate1: TMenuItem;
    N5: TMenuItem;
    GenerateCSR1: TMenuItem;
    ImportSignedCSR1: TMenuItem;
    SignCSR1: TMenuItem;
    GroupBox2: TGroupBox;
    lvCertInfo: TListView;
    pgCertsAndKeys: TPageControl;
    tabUserStores: TTabSheet;
    tabMachineStores: TTabSheet;
    tabPFXStores: TTabSheet;
    tabKeys: TTabSheet;
    tvUserStores: TTreeView;
    Label1: TLabel;
    Label2: TLabel;
    certMgr: TiphCertMgr;
    tvMachineStores: TTreeView;
    Label4: TLabel;
    Label3: TLabel;
    lvUserCerts: TListView;
    lvMachineCerts: TListView;
    spStatus: TStatusBar;
    lvKeys: TListView;
    lvPFXCerts: TListView;
    Label5: TLabel;
    tPFXFile: TEdit;
    btnBrowsePFX: TButton;
    btnLoadPFX: TButton;
    dlgOpen: TOpenDialog;
    A1: TMenuItem;
    CreateKey1: TMenuItem;
    lPFXPassword: TLabel;
    procedure certMgrStoreList(Sender: TObject; const CertStore: String);
    procedure pgCertsAndKeysChange(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvUserStoresChange(Sender: TObject; Node: TTreeNode);
    procedure certMgrCertList(Sender: TObject; CertEncoded: string; CertEncodedB: TBytes;
      const CertSubject, CertIssuer, CertSerialNumber: String;
      HasPrivateKey: Boolean);
    procedure tvMachineStoresChange(Sender: TObject; Node: TTreeNode);
    procedure lvUserCertsClick(Sender: TObject);
    procedure lvMachineCertsClick(Sender: TObject);
    procedure certMgrKeyList(Sender: TObject; const KeyContainer: String;
      KeyType: Integer; const AlgId: String; KeyLen: Integer);
    procedure btnBrowsePFXClick(Sender: TObject);
    procedure btnLoadPFXClick(Sender: TObject);
    procedure lvPFXCertsClick(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure CreateSelfSignedCertificate1Click(Sender: TObject);
    procedure CreateSignCertificate1Click(Sender: TObject);
    procedure SignCSR1Click(Sender: TObject);
    procedure CreateKey1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ExportCertificate1Click(Sender: TObject);
    procedure GenerateCSR1Click(Sender: TObject);
    procedure DeleteCertificate1Click(Sender: TObject);
    procedure ImportCertificate1Click(Sender: TObject);
    procedure ImportSignedCSR1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ShowCertInfo;
    function SelectCert: boolean;
  public
    { Public declarations }
  end;

var
  FormCertmgr: TFormCertmgr;
  FormCreatecert: TFormCreateCert;
  FormGeneratecsr: TFormGenerateCSR;
  FormImportcsr: TFormImportCSR;
  FormSigncsr: TFormSignCSR;

implementation

{$R *.dfm}

procedure TFormCertmgr.certMgrStoreList(Sender: TObject;
  const CertStore: String);
begin
  if pgCertsAndKeys.ActivePage = tabUserStores then
    tvUserStores.Items.AddChild(tvUserStores.Items[0], CertStore);
  if pgCertsAndKeys.ActivePage = tabMachineStores then
    tvMachineStores.Items.AddChild(tvMachineStores.Items[0], CertStore);
end;

procedure TFormCertmgr.pgCertsAndKeysChange(Sender: TObject);
begin
  lvCertInfo.Items.Clear();
  lvCertInfo.Enabled := true;
  if pgCertsAndKeys.ActivePage = tabUserStores then begin
    tvUserStores.Items[0].DeleteChildren;
    lvUserCerts.Items.Clear();
    certMgr.ListCertificateStores;
    tvUserStores.Items[0].Expand(true);
  end;

  if pgCertsAndKeys.ActivePage = tabMachineStores then begin
    tvMachineStores.Items[0].DeleteChildren;
    lvMachineCerts.Items.Clear();
    certMgr.ListMachineStores;
    tvMachineStores.Items[0].Expand(true);
  end;

  if pgCertsAndKeys.ActivePage = tabKeys then begin
    lvCertInfo.Enabled := false;
    lvKeys.Items.Clear();
    certMgr.CertStoreType := cstUser;
    certMgr.ListKeys;
  end;
end;

procedure TFormCertmgr.Refresh1Click(Sender: TObject);
begin
  pgCertsAndKeysChange(Sender);
end;

procedure TFormCertmgr.FormActivate(Sender: TObject);
begin
  pgCertsAndKeysChange(Sender);
end;

procedure TFormCertmgr.certMgrCertList(Sender: TObject; CertEncoded: string; CertEncodedB: TBytes;
  const CertSubject, CertIssuer, CertSerialNumber: String;
  HasPrivateKey: Boolean);
var
  listView: TListView;
  listItem: TListItem;
begin

  if pgCertsAndKeys.ActivePage = tabUserStores then listView :=  lvUserCerts;
  if pgCertsAndKeys.ActivePage = tabMachineStores then listView := lvMachineCerts;
  if pgCertsAndKeys.ActivePage = tabPFXStores then listView:= lvPFXCerts;
  
  listItem := listView.Items.Add;
  listItem.Caption := CertSubject;
  listItem.SubItems.Add(certMgr.CertStore);
  listItem.SubItems.Add(CertEncoded);

end;

procedure TFormCertmgr.tvUserStoresChange(Sender: TObject; Node: TTreeNode);
begin
  lvUserCerts.Items.Clear();
  if tvUserStores.Selected.Parent = nil then exit;
  certMgr.CertStoreType := cstUser;
  certMgr.CertStore := tvUserStores.Selected.Text;
  certMgr.ListStoreCertificates;
  if lvUserCerts.Items.Count> 0 then lvUserCerts.Items.Item[0].Selected := true;
end;

procedure TFormCertmgr.tvMachineStoresChange(Sender: TObject; Node: TTreeNode);
begin
  lvMachineCerts.Items.Clear();
  if tvMachineStores.Selected.Parent = nil then exit;
  certMgr.CertStoreType := cstMachine;
  certMgr.CertStore := tvMachineStores.Selected.Text;
  certMgr.ListStoreCertificates;
  if lvMachineCerts.Items.Count> 0 then lvMachineCerts.Items.Item[0].Selected := true;
end;

procedure TFormCertmgr.lvUserCertsClick(Sender: TObject);
begin
  if lvUserCerts.SelCount > 0 then begin
          certMgr.CertStoreType := cstUser;
          certMgr.CertStore := lvUserCerts.Selected.SubItems[0];
          certMgr.CertSubject := lvUserCerts.Selected.Caption;
          ShowCertInfo;
  end;
end;

procedure TFormCertmgr.lvMachineCertsClick(Sender: TObject);
begin
  if lvMachineCerts.SelCount > 0 then begin
        certMgr.CertStoreType := cstMachine;
        certMgr.CertStore := lvMachineCerts.Selected.SubItems[0];
        certMgr.CertSubject := lvUserCerts.Selected.Caption;
        ShowCertInfo;
        end;
end;

procedure TFormCertmgr.ShowCertInfo;
var
  listItem: TListItem;
begin
  spStatus.SimpleText := '';
  lvCertInfo.Items.Clear();

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Subject';
	listItem.SubItems.Add(certMgr.CertSubject);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Issuer';
	listItem.SubItems.Add(certMgr.CertIssuer);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Version';
	listItem.SubItems.Add(certMgr.CertVersion);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Serial Number';
	listItem.SubItems.Add(certMgr.CertSerialNumber);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Signature Algorithm';
	listItem.SubItems.Add(certMgr.CertSignatureAlgorithm);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Effective Date';
	listItem.SubItems.Add(certMgr.CertEffectiveDate);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Expiration Date';
	listItem.SubItems.Add(certMgr.CertExpirationDate);
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key Algorithm';
	listItem.SubItems.Add(certMgr.CertPublicKeyAlgorithm);
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key Length';
	listItem.SubItems.Add(IntToStr(certMgr.CertPublicKeyLength));
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key';
	listItem.SubItems.Add(certMgr.CertPublicKey);

  if certMgr.CertPrivateKeyAvailable then
    spStatus.SimpleText := '* You have a private key that is associated with this certificate.';

end;

procedure TFormCertmgr.certMgrKeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
var
  listItem: TListItem;
begin
  listItem := lvKeys.Items.Add;
  listItem.Caption := KeyContainer;
  case KeyType of
    1: listItem.SubItems.Add('Signature');
    2: listItem.SubItems.Add('Key Exchange');
    else listItem.SubItems.Add('Unknown: ' + IntToStr(KeyType));
  end;
  listItem.SubItems.Add(IntToStr(KeyLen));
  listItem.SubItems.Add(AlgId);
end;

procedure TFormCertmgr.btnBrowsePFXClick(Sender: TObject);
begin
  if dlgOpen.Execute then tPFXFile.Text := dlgOpen.FileName;
end;

procedure TFormCertmgr.btnLoadPFXClick(Sender: TObject);
begin
  if tPFXFile.Text = '' then btnBrowsePFXClick(Sender);
  lvPFXCerts.Items.Clear();
  certMgr.CertStoreType := cstPFXFile;
  lPFXPassword.Caption := InputBox('PFX File Password', 'Please enter the PFX file password:', '');
  certMgr.CertStorePassword := lPFXPassword.Caption;
  certMgr.CertStore := tPFXFile.Text;
  certMgr.ListStoreCertificates;
end;

procedure TFormCertmgr.lvPFXCertsClick(Sender: TObject);
begin
  if lvPFXCerts.SelCount > 0 then begin
        certMgr.CertSubject := lvPFXCerts.Selected.Caption;
        ShowCertInfo;
        end;
end;

procedure TFormCertmgr.A1Click(Sender: TObject);
begin
  ShowMessage('IPWorks Certificate Manager');
end;

procedure TFormCertmgr.CreateSelfSignedCertificate1Click(Sender: TObject);
begin
  if FormCreatecert = nil then FormCreatecert := TFormCreatecert.Create(FormCertmgr);
  FormCreatecert.rbSelfSigned.Checked := true;
  FormCreatecert.Show;
  pgCertsAndKeys.ActivePage := tabUserStores
end;

procedure TFormCertmgr.CreateSignCertificate1Click(Sender: TObject);
begin
  if FormCreatecert = nil then FormCreatecert := TFormCreatecert.Create(FormCertmgr);
  FormCreatecert.rbSigned.Checked := true;
  FormCreatecert.Show;
  pgCertsAndKeys.ActivePage := tabUserStores
end;

procedure TFormCertmgr.SignCSR1Click(Sender: TObject);
begin
  if FormSigncsr = nil then FormSigncsr := TFormSigncsr.Create(FormCertmgr);
  FormSigncsr.Show;
end;

procedure TFormCertmgr.CreateKey1Click(Sender: TObject);
var
  keyName: string;
begin
  keyName := InputBox('Create Key', 'Please enter key name:', '');
  if keyName <> '' then certMgr.CreateKey(keyName);
  pgCertsAndKeys.ActivePage := tabKeys;
  pgCertsAndKeysChange(Nil);
end;

procedure TFormCertmgr.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormCertmgr.ExportCertificate1Click(Sender: TObject);
var
  password: string;
begin

  if not SelectCert then exit;
  if not dlgOpen.Execute then exit;

  password := InputBox('Export Certificate', 'Please enter a password for the PFX file:', '');
  if password = '' then begin
    ShowMessage('Can''t export certificate without a password');
    exit;
  end;

  certMgr.ExportCertificate(dlgOpen.FileName, password);
  ShowMessage('Certificate exported to: ' + dlgOpen.FileName);
end;

procedure TFormCertmgr.GenerateCSR1Click(Sender: TObject);
begin
  if FormGeneratecsr = nil then FormGeneratecsr := TFormGeneratecsr.Create(FormCertmgr);
  FormGeneratecsr.Show;
end;

function TFormCertmgr.SelectCert: boolean;
begin
  SelectCert := false;
  certMgr.Reset;
  
  if pgCertsAndKeys.ActivePage = tabUserStores then begin
    certMgr.CertStoreType := cstUser;
    if lvUserCerts.SelCount > 0 then begin
      certMgr.CertStore := tvUserStores.Selected.Text;
      certMgr.CertSubject := lvUserCerts.Selected.Caption;
    end;
  end;

  if pgCertsAndKeys.ActivePage = tabMachineStores then begin
    certMgr.CertStoreType := cstMachine;
    if lvMachineCerts.SelCount > 0 then begin
      certMgr.CertStore := tvMachineStores.Selected.Text;
      certMgr.CertSubject := lvUserCerts.Selected.Caption;
    end;
  end;

  if certMgr.CertSubject = '' then begin
    ShowMessage('Please select a certificate in a User or Machine store first.');
    exit;
  end;

  SelectCert := true;
end;

procedure TFormCertmgr.DeleteCertificate1Click(Sender: TObject);
begin
  if not SelectCert then exit;

  if MessageDlg('Are you sure you want to delete the certificate with subject: ' +
                 certMgr.CertSubject + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    certMgr.DeleteCertificate;
  end;

  if pgCertsAndKeys.ActivePage = tabUserStores then tvUserStoresChange(Sender, nil);
  if pgCertsAndKeys.ActivePage = tabMachineStores then tvMachineStoresChange(Sender, nil);
end;

procedure TFormCertmgr.ImportCertificate1Click(Sender: TObject);
begin

  if (pgCertsAndKeys.ActivePage <> tabPFXStores) or (lvPFXCerts.Items.Count = 0)  or (lvPFXCerts.Selected = nil) then begin
    ShowMessage('Please select a certificate in a PFX file first.');
    pgCertsAndKeys.ActivePage := tabPFXStores;
    exit;
  end;

  //this imports into the 'MY' store, but any other store would work as well
  certMgr.CertStoreType := cstUser;
  certMgr.CertStore := 'MY';
  certMgr.ImportCertificate(tPFXFile.Text, lPFXPassword.Caption, lvPFXCerts.Selected.Caption);
  pgCertsAndKeys.ActivePage := tabUserStores;
  Refresh1Click(nil);
  
end;

procedure TFormCertmgr.ImportSignedCSR1Click(Sender: TObject);
begin
  if FormImportcsr = nil then FormImportcsr := TFormImportcsr.Create(FormCertmgr);
  FormImportcsr.Show;
  pgCertsAndKeys.ActivePage := tabUserStores;
  Refresh1Click(nil);
end;

procedure TFormCertmgr.FormCreate(Sender: TObject);
begin
  tvUserStores.Items.AddFirst(nil,'User Certificate Stores');
  tvMachineStores.Items.AddFirst(nil,'Machine Certificate Stores');
end;

end.


