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
unit sshellf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iphcore, iphtypes, iphsexec, StdCtrls, iphsshell;

type
  TFormSshell = class(TForm)
    Label1: TLabel;
    txtPassword: TEdit;
    txtUser: TEdit;
    txtSSHServer: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnConnect: TButton;
    txtOutput: TMemo;
    iphSShell1: TiphSShell;
    procedure btnConnectClick(Sender: TObject);
    procedure iphSShell1SSHServerAuthentication(Sender: TObject;
      HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
      Status: string; var Accept: Boolean);
    procedure iphSShell1Stdout(Sender: TObject; Text: String);
    procedure iphSShell1Stderr(Sender: TObject; Text: String);
    procedure txtOutputKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSshell: TFormSshell;

implementation

{$R *.dfm}

procedure TFormSshell.btnConnectClick(Sender: TObject);
begin

  if not(iphSshell1.Connected) then begin
  try
        Screen.Cursor := crHourGlass;
        iphSshell1.SSHUser := txtUser.Text;
        iphSshell1.SSHPassword := txtPassword.Text;
        iphSshell1.SSHLogon(txtSSHServer.Text,22);
        btnConnect.Caption := 'Disconnect';
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
  end

  else begin
        iphSshell1.SSHLogoff;
        btnConnect.Caption := 'Connect';
  end;

end;

procedure TFormSshell.iphSShell1SSHServerAuthentication(Sender: TObject;
  HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
        Accept := True;
end;

procedure TFormSshell.iphSShell1Stdout(Sender: TObject; Text: String);
begin
        txtOutput.Text := txtOutput.Text + Text;
        txtOutput.SelStart := Length(txtOutput.Text);
        SendMessage(txtOutput.Handle,EM_SCROLLCARET,0,0);
end;

procedure TFormSshell.iphSShell1Stderr(Sender: TObject; Text: String);
begin
        txtOutput.Text := txtOutput.Text + Text;
        txtOutput.SelStart := Length(txtOutput.Text);
        SendMessage(txtOutput.Handle,EM_SCROLLCARET,0,0);
end;

procedure TFormSshell.txtOutputKeyPress(Sender: TObject; var Key: Char);
begin
     iphSshell1.SendStdinText(Key);
end;

end.

