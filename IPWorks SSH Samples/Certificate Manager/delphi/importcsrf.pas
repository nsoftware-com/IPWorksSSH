unit importcsrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iphcore, iphtypes, iphcertmgr;

type
  TFormImportcsr = class(TForm)
    tCSR: TMemo;
    bOK: TButton;
    bCancel: TButton;
    Label4: TLabel;
    Label2: TLabel;
    cbKey: TComboBox;
    certMgr: TiphCertMgr;
    procedure certMgrKeyList(Sender: TObject; const KeyContainer: String;
      KeyType: Integer; const AlgId: String; KeyLen: Integer);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormImportcsr: TFormImportcsr;

implementation

{$R *.dfm}

procedure TFormImportcsr.certMgrKeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
begin
  cbKey.Items.Add(KeyContainer);
end;

procedure TFormImportcsr.bOKClick(Sender: TObject);
begin
  //this imports into the 'MY' store, but any other store would work as well
  certMgr.CertStoreType := cstUser;
  certMgr.CertStore := 'MY';
  certMgr.ImportSignedCSR(TEncoding.Default.GetBytes(tCSR.Text), cbKey.Text);
  Close();
end;

procedure TFormImportcsr.bCancelClick(Sender: TObject);
begin
        Close();
end;

procedure TFormImportcsr.FormActivate(Sender: TObject);
begin
  cbKey.Items.Clear;
  certMgr.ListKeys;
  cbKey.ItemIndex := 0;
end;

end.
