unit generatecsrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, iphcore, iphtypes, iphcertmgr;

type
  TFormGeneratecsr = class(TForm)
    tCSR: TMemo;
    bSign: TButton;
    bOK: TButton;
    Label4: TLabel;
    tSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbKey: TComboBox;
    certMgr: TiphCertMgr;
    procedure bSignClick(Sender: TObject);
    procedure certMgrKeyList(Sender: TObject; const KeyContainer: String;
      KeyType: Integer; const AlgId: String; KeyLen: Integer);
    procedure bOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGeneratecsr: TFormGeneratecsr;

implementation

{$R *.dfm}

procedure TFormGeneratecsr.bSignClick(Sender: TObject);
begin

  if Copy(tSubject.Text, 0, 3) <> 'CN=' then begin
    ShowMessage('Certificate subject must start with "CN="');
    exit;
  end;

  tCSR.Text := certMgr.generatecsr(tSubject.Text, cbKey.Text);

end;

procedure TFormGeneratecsr.certMgrKeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
begin
  cbKey.Items.Add(KeyContainer);
end;

procedure TFormGeneratecsr.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormGeneratecsr.FormActivate(Sender: TObject);
begin
  cbKey.Items.Clear;
  certMgr.ListKeys;
  cbKey.ItemIndex := 0;
end;

end.
