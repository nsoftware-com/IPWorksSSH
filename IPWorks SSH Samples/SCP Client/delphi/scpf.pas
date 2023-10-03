(*
 * IPWorks SSH 2022 Delphi Edition - Sample Project
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
unit scpf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, IOUtils, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls, iphsexec, iphcore, iphtypes, iphscp;

type
  TFormScp = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    cboLocalPath: TComboBox;
    tbRemotePath: TEdit;
    btnList: TButton;
    cmdUpload: TButton;
    cmdDownload: TButton;
    lvwLocalDir: TListView;
    lvwRemoteDir: TListView;
    cmdAbortTransfer: TButton;
    cmdExit: TButton;
    cmdConnect: TButton;
    iphSExec1: TiphSExec;
    iphSCP1: TiphSCP;
    tbStatus: TMemo;
    procedure cmdConnectClick(Sender: TObject);
    procedure cmdAbortTransferClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure iphSCP1SSHServerAuthentication(Sender: TObject;
     HostKey: string; HostKeyB: TArray<System.Byte>; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
     Status: string; var Accept: Boolean);
    procedure iphSCP1SSHStatus(Sender: TObject; const Message: string);
    procedure iphSExec1SSHServerAuthentication(Sender: TObject;
     HostKey: string; HostKeyB: TArray<System.Byte>; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
     Status: string; var Accept: Boolean);
    procedure cmdDownloadClick(Sender: TObject);
    procedure cmdUploadClick(Sender: TObject);
    procedure lvwLocalDirDblClick(Sender: TObject);
    procedure btnListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbStatusChange(Sender: TObject);
    procedure iphSCP1Transfer(Sender: TObject; Direction: Integer;
      const LocalFile, RemoteFile, RemotePath: string; BytesTransferred: Int64;
      PercentDone: Integer; Text: string; TextB: TArray<System.Byte>);
    procedure iphSExec1Stdout(Sender: TObject; Text: string;
      TextB: TArray<System.Byte>);
    procedure iphSExec1Stderr(Sender: TObject; Text: string;
      TextB: TArray<System.Byte>);
  private
    state: Integer;
    procedure Update_ComboBox(strItem: string);
    procedure Remote_Dir_Refresh;
    procedure Local_Dir_Refresh;
  public
    { Public declarations }
  end;

var
  FormScp: TFormScp;

implementation

uses
  scploginf;

{$R *.dfm}

procedure TFormScp.btnListClick(Sender: TObject);
begin
  Remote_Dir_Refresh();
end;

procedure TFormScp.cmdAbortTransferClick(Sender: TObject);
begin
  try
    iphSCP1.Interrupt;
  except on E: EiphSCP do
    ShowMessage(E.Message);
  end;
end;

procedure TFormScp.cmdConnectClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
   if cmdConnect.Caption = 'Connect' then
   with TFormScplogin.Create(self) do
   begin
      if ShowModal = mrOK then
      begin
         iphSCP1.SSHUser := tbUser.Text;
         iphSExec1.SSHUser := tbUser.Text;

         if cbAuthType.Text = 'Password' then
         begin
             iphSExec1.SSHAuthMode := TiphsexecSSHAuthModes.amPassword;
             iphSCP1.SSHAuthMode := TiphscpSSHAuthModes.amPassword;

             iphSExec1.SSHPassword := tbPassword.Text;
             iphSCP1.SSHPassword := tbPassword.Text;
         end
         else
         begin
           if cbAuthType.Text = 'Public key authentication (PEM)' then
           begin
             iphSExec1.SSHCertStoreType := TiphsexecSSHCertStoreTypes.cstPEMKeyFile;
             iphSExec1.SSHCertStore := tbFilePath.Text;
             iphSExec1.SSHCertStorePassword := tbPassword.Text;
             iphSexec1.SSHCertSubject := '*';
             iphSCP1.SSHCertStoreType := TiphscpSSHCertStoreTypes.cstPEMKeyFile;
             iphSCP1.SSHCertStore := tbFilePath.Text;
             iphSCP1.SSHCertStorePassword := tbPassword.Text;
             iphSCP1.SSHCertSubject := '*';
           end
           else
           begin
             iphSExec1.SSHCertStoreType := TiphsexecSSHCertStoreTypes.cstPFXFile;
             iphSExec1.SSHCertStore := tbFilePath.Text;
             iphSExec1.SSHCertStorePassword := tbPassword.Text;
             iphSexec1.SSHCertSubject := '*';
             iphSCP1.SSHCertStoreType := TiphscpSSHCertStoreTypes.cstPFXFile;
             iphSCP1.SSHCertStore := tbFilePath.Text;
             iphSCP1.SSHCertStorePassword := tbPassword.Text;
             iphSCP1.SSHCertSubject := '*';
           end;
         end;

         cmdConnect.Caption := 'Disconnect';
         try
           iphSExec1.SSHLogon(tbServer.Text, 22);
           iphSCP1.SSHLogon(tbServer.Text, 22);
           Remote_Dir_Refresh();
         except on E: EiphSCP do
           ShowMessage(E.Message);
         end;
      end;
      Free;
   end
   else
   begin
      iphSCP1.SSHLogoff;
      iphSExec1.SSHLogoff;
	  lvwRemoteDir.Items.Clear;
      cmdConnect.Caption := 'Connect';
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormScp.cmdDownloadClick(Sender: TObject);
  var filename: string;
begin
  if lvwRemoteDir.SelCount > 0 then
  begin
    filename := lvwRemoteDir.Selected.Caption;
    try
      iphSCP1.LocalFile := GetCurrentDir + filename;

      if AnsiLastChar(tbRemotePath.Text) <> '/' then
      begin
        iphSCP1.RemoteFile := tbRemotePath.Text + '/' + filename;
      end
      else
      begin
        iphSCP1.RemoteFile := tbRemotePath.Text + filename;
      end;

      iphscp1.Download;
    except on E: EiphSCP do
      ShowMessage(E.Message);
    end;
    iphSCP1.RemoteFile := '';
    Local_Dir_Refresh;
  end;
end;

procedure TFormScp.cmdExitClick(Sender: TObject);
begin
  FormScp.Close;
end;

procedure TFormScp.cmdUploadClick(Sender: TObject);
  var filename: string;
begin
  if lvwLocalDir.SelCount > 0 then
  begin
    filename := lvwLocalDir.Selected.Caption;
    try
      iphSCP1.LocalFile := filename;

      if AnsiLastChar(tbRemotePath.Text) <> '/' then
      begin
        iphSCP1.RemoteFile := tbRemotePath.Text + '/' + filename;
      end
      else
      begin
        iphSCP1.RemoteFile := tbRemotePath.Text + filename;
      end;

      iphscp1.Upload;
    except on E: EiphSCP do
      ShowMessage(E.Message);
    end;
    iphSCP1.RemoteFile := '';
    Remote_Dir_Refresh;
  end;
end;

procedure TFormScp.FormShow(Sender: TObject);
begin
  tbStatus.Text := '';
  SetCurrentDir('c:\');
  Local_Dir_Refresh;
end;

procedure TFormScp.iphSCP1SSHServerAuthentication(Sender: TObject;
  HostKey: string; HostKeyB: TArray<System.Byte>; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFormScp.iphSCP1SSHStatus(Sender: TObject; const Message: string);
begin
  tbStatus.Text := tbStatus.Text + Message + #13#10;
end;

procedure TFormScp.iphSCP1Transfer(Sender: TObject; Direction: Integer;
  const LocalFile, RemoteFile, RemotePath: string; BytesTransferred: Int64;
  PercentDone: Integer; Text: string; TextB: TArray<System.Byte>);
begin
  if Direction = 0 then
  begin
    tbStatus.Text := tbStatus.Text + IntToStr(PercentDone) + '% uploaded' + #13#10;
  end
  else
  begin
    tbStatus.Text := tbStatus.Text + IntToStr(PercentDone) + '% downloaded' + #13#10;
  end;
end;

procedure TFormScp.iphSExec1SSHServerAuthentication(Sender: TObject;
  HostKey: string; HostKeyB: TArray<System.Byte>; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
  Accept := True;
end;



procedure TFormScp.iphSExec1Stderr(Sender: TObject; Text: string;
  TextB: TArray<System.Byte>);
begin
ShowMessage('Error: ' + Text);
end;

procedure TFormScp.iphSExec1Stdout(Sender: TObject; Text: string;
  TextB: TArray<System.Byte>);
  var list: TStrings;
      temp: string;
      i: Integer;
      item: TListItem;
begin
  list := TStringList.Create;
  try
    ExtractStrings([#13], [], PChar(Text), list);
    for i := 0 to list.Count - 1 do
    begin
      temp := list[i];
      if temp <> '' then
      begin
        if AnsiLastChar(temp) = '/' then
        begin
          item := lvwRemoteDir.Items.Add;
          Delete(temp, length(temp), 1);
          item.Caption := '<dir> ' + temp;
        end
        else
        begin
          item := lvwRemoteDir.Items.Add;
          item.Caption := temp;
        end;
      end
    end;
  finally
    list.Free;
  end;

end;

procedure TFormScp.lvwLocalDirDblClick(Sender: TObject);
  var dir: string;
begin
  dir := lvwLocalDir.Selected.Caption;

  if DirectoryExists(dir) then
  begin
    SetCurrentDir(dir);
    lvwLocalDir.Items.Clear;
    Local_Dir_Refresh;
  end;
end;

procedure TFormScp.Update_ComboBox(strItem: string);
begin
  cboLocalPath.Items.Delete(cboLocalPath.Items.IndexOf(strItem));
  cboLocalPath.Items.Insert(0, strItem);
  cboLocalPath.Text := cboLocalPath.Items[0];
end;

procedure TFormScp.Remote_Dir_Refresh;
begin
  lvwRemoteDir.Items.Clear;
  iphSExec1.Execute('ls -p ' + tbRemotePath.Text);
end;

procedure TFormScp.tbStatusChange(Sender: TObject);
begin
  tbStatus.Perform(EM_LineScroll, 0, tbStatus.Lines.Count);
end;

procedure TFormScp.Local_Dir_Refresh;
  var SearchRec: TSearchRec;
      intlvwCount: Integer;
begin
  Screen.Cursor := crAppStart;
  lvwLocalDir.Items.Clear;

  if FindFirst('*', faAnyFile, SearchRec) = 0 then
  repeat
    intlvwCount := lvwLocalDir.Items.Count;
    lvwLocalDir.Items.Add;

    if (SearchRec.Attr and faDirectory) <> 0 then begin
      lvwLocalDir.Items[intlvwCount].Caption := SearchRec.Name;
      lvwLocalDir.Items[intlvwCount].SubItems.Add('<dir>');
    end
    else
    begin
      lvwLocalDir.Items[intlvwCount].Caption := SearchRec.Name;
      lvwLocalDir.Items[intlvwCount].SubItems.Add(IntToStr(SearchRec.Size));
    end;
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
  Screen.Cursor := crDefault;
end;

end.

