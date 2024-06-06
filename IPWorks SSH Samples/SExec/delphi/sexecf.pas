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
    procedure iphSExec1SSHServerAuthentication(Sender: TObject;
      HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
      Status: string; var Accept: Boolean);
    procedure iphSExec1Stdout(Sender: TObject; Text: String);
    procedure btnExecuteClick(Sender: TObject);
    procedure iphSExec1Stderr(Sender: TObject; Text: String);
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
  HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
        Accept := True;
end;

procedure TFormSexec.iphSExec1Stdout(Sender: TObject; Text: String);
begin
        txtResponse.Lines.Add(StringReplace(Text,#10,#13#10,[rfReplaceAll]));
        txtResponse.SelStart := Length(txtResponse.Text);
        SendMessage(txtResponse.Handle,EM_SCROLLCARET,0,0);
end;

procedure TFormSexec.iphSExec1Stderr(Sender: TObject; Text: String);
begin
        txtResponse.Lines.Add(StringReplace(Text,#10,#13#10,[rfReplaceAll]));
        txtResponse.SelStart := Length(txtResponse.Text);
        SendMessage(txtResponse.Handle,EM_SCROLLCARET,0,0);        
end;

procedure TFormSexec.btnExecuteClick(Sender: TObject);
begin
  try
        Screen.Cursor := crHourGlass;
		iphSExec1.Timeout := 60;
        iphSexec1.SSHUser := txtUser.Text;
        iphSexec1.SSHPassword := txtPassword.Text;
        iphSexec1.SSHLogon(txtSSHServer.Text,22);
        iphSexec1.Execute(txtCommand.Text);
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
end;



end.

