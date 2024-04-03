unit sexecf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iphcore, iphtypes, iphsexec, StdCtrls;

type
  TFormSexec = class(TForm)
    iphSExec1: TiphSExec;
    Label1: TLabel;
    txtCommand: TEdit;
    txtPassword: TEdit;
    txtUser: TEdit;
    txtSSHServer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnExecute: TButton;
    txtResponse: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    txtPort: TEdit;
    procedure btnExecuteClick(Sender: TObject);
    procedure iphSExec1SSHServerAuthentication(Sender: TObject; HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm,
      CertSubject, CertIssuer, Status: string; var Accept: Boolean);
    procedure iphSExec1Stdout(Sender: TObject; Text: string;
      TextB: TBytes);
    procedure iphSExec1Stderr(Sender: TObject; Text: string;
      TextB: TBytes);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSexec: TFormSexec;

implementation

{$R *.dfm}


procedure TFormSexec.iphSExec1SSHServerAuthentication(Sender: TObject;
  HostKey: string; HostKeyB: TBytes; const Fingerprint,
  KeyAlgorithm, CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFormSexec.iphSExec1Stderr(Sender: TObject; Text: string;
  TextB: TBytes);
begin
        txtResponse.Lines.Add(StringReplace(Text,#10,#13#10,[rfReplaceAll]));
        txtResponse.SelStart := Length(txtResponse.Text);
        SendMessage(txtResponse.Handle,EM_SCROLLCARET,0,0);
end;

procedure TFormSexec.iphSExec1Stdout(Sender: TObject; Text: string;
  TextB: TBytes);
begin
        txtResponse.Lines.Add(StringReplace(Text,#10,#13#10,[rfReplaceAll]));
        txtResponse.SelStart := Length(txtResponse.Text);
        SendMessage(txtResponse.Handle,EM_SCROLLCARET,0,0);
end;

procedure TFormSexec.btnExecuteClick(Sender: TObject);
var
  serverPort : Integer;
begin
  try
        serverPort := StrToInt(txtPort.Text);
        Screen.Cursor := crHourGlass;
        if not iphSexec1.Connected then
        begin
          iphSexec1.SSHUser := txtUser.Text;
          iphSexec1.SSHPassword := txtPassword.Text;
          iphSexec1.SSHLogon(txtSSHServer.Text, serverPort);
        end;
        iphSexec1.Execute(txtCommand.Text);
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
end;



end.
