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
unit sshtunnelf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ComCtrls, StdCtrls, ToolWin, TunnelConfigf, iphcore, iphtypes,
  iphsshtunnel, Generics.Collections;

type
  TFormSSHTunnel = class(TForm)
    TabControl1: TTabControl;
    sgTunnels: TStringGrid;
    Log1: TMemo;
    btnNew: TButton;
    btnEdit: TButton;
    btnRemove: TButton;
    ToolBar1: TToolBar;
    tbtnStart: TToolButton;
    tbtnRestart: TToolButton;
    tbtnStop: TToolButton;
    Label1: TLabel;
    newtunnel: TiphSSHTunnel;
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNewClick(Sender: TObject);

    procedure newtunnelDisconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure newtunnelSSHServerAuthentication(Sender: TObject;
      HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
      Status: string; var Accept: Boolean);
    procedure newtunnelSSHStatus(Sender: TObject; const Message: string);
    procedure newtunnelError(Sender: TObject; ConnectionId, ErrorCode: Integer;
      const Description: string);
    procedure newtunnelConnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure btnEditClick(Sender: TObject);
    procedure sgTunnelsClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure tbtnStartClick(Sender: TObject);
    procedure tbtnStopClick(Sender: TObject);
    procedure tbtnRestartClick(Sender: TObject);

  private
    tunnels: TList<TiphSSHTunnel>;
    selIndex: Integer;
    connected: TConnectedEvent;
    disconnected: TDisconnectedEvent;
    error: TErrorEvent;
    sshserverauth: TSSHServerAuthenticationEvent;
    sshstatus: TSSHStatusEvent;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSSHTunnel: TFormSSHTunnel;

implementation

{$R *.dfm}

procedure TFormSSHTunnel.btnNewClick(Sender: TObject);
begin
  with TFormTunnelConfig.Create(self) do
    try
      ShowModal;
      if ModalResult = mrOk then
      begin
        newtunnel := newtunnel.Create(nil);
        newtunnel.OnConnected := connected;
        newtunnel.OnDisconnected := disconnected;
        newtunnel.OnError := error;
        newtunnel.OnSSHServerAuthentication := sshserverauth;
        newtunnel.OnSSHStatus := sshstatus;

        newtunnel.LocalPort := StrToInt(txtLocalPort.Text);
        newtunnel.SSHHost := txtSSHHost.Text;
        newtunnel.SSHPort := StrToInt(txtSSHPort.Text);
        newtunnel.SSHUser := txtSSHUser.Text;
        newtunnel.SSHPassword := txtSSHPassword.Text;
        newtunnel.SSHForwardHost := txtRemoteHost.Text;
        newtunnel.SSHForwardPort := StrToInt(txtRemotePort.Text);
        tunnels.Add(newtunnel);

        //UI Update
        sgTunnels.RowCount := sgTunnels.RowCount + 1;
        sgTunnels.Cells[0, sgTunnels.RowCount - 1] := txtTunnelName.Text;
        sgTunnels.Cells[1, sgTunnels.RowCount - 1] := txtLocalPort.Text;
        sgTunnels.Cells[2, sgTunnels.RowCount - 1] := txtSSHHost.Text;
        sgTunnels.Cells[3, sgTunnels.RowCount - 1] := txtSSHPort.Text;
        sgTunnels.Cells[4, sgTunnels.RowCount - 1] := txtRemoteHost.Text;
        sgTunnels.Cells[5, sgTunnels.RowCount - 1] := txtRemotePort.Text;
      end;
    finally
      Free;
    end;

end;

procedure TFormSSHTunnel.btnRemoveClick(Sender: TObject);
var
i: Integer;
begin
  if selIndex > 0 then
  try
    tunnels[selIndex - 1].StopListening();
    tunnels.Delete(selIndex -1);

    for i := selIndex to sgTunnels.RowCount - 1 do
    begin
      sgTunnels.Cells[0, selIndex] := sgTunnels.Cells[0, selIndex + 1];
      sgTunnels.Cells[1, selIndex] := sgTunnels.Cells[1, selIndex + 1];
      sgTunnels.Cells[2, selIndex] := sgTunnels.Cells[2, selIndex + 1];
      sgTunnels.Cells[3, selIndex] := sgTunnels.Cells[3, selIndex + 1];
      sgTunnels.Cells[4, selIndex] := sgTunnels.Cells[4, selIndex + 1];
      sgTunnels.Cells[5, selIndex] := sgTunnels.Cells[5, selIndex + 1];
    end;
    sgTunnels.RowCount := sgTunnels.RowCount - 1;
  finally

  end;
end;

procedure TFormSSHTunnel.btnEditClick(Sender: TObject);
begin
  if selIndex > 0 then
    with TFormTunnelConfig.Create(self) do
    try
      txtTunnelName.Text := sgTunnels.Cells[0, selIndex];
      txtLocalPort.Text := sgTunnels.Cells[1, selIndex];
      txtSSHHost.Text := sgTunnels.Cells[2, selIndex];
      txtSSHPort.Text := sgTunnels.Cells[3, selIndex];
      txtRemoteHost.Text := sgTunnels.Cells[4, selIndex];
      txtRemotePort.Text := sgTunnels.Cells[5, selIndex];
      ShowModal;
      if ModalResult = mrOk then
      begin
        newtunnel := newtunnel.Create(nil);
        newtunnel.OnConnected := connected;
        newtunnel.OnDisconnected := disconnected;
        newtunnel.OnError := error;
        newtunnel.OnSSHServerAuthentication := sshserverauth;
        newtunnel.OnSSHStatus := sshstatus;

        newtunnel.LocalPort := StrToInt(txtLocalPort.Text);
        newtunnel.SSHHost := txtSSHHost.Text;
        newtunnel.SSHPort := StrToInt(txtSSHPort.Text);
        newtunnel.SSHUser := txtSSHUser.Text;
        newtunnel.SSHPassword := txtSSHPassword.Text;
        newtunnel.SSHForwardHost := txtRemoteHost.Text;
        newtunnel.SSHForwardPort := StrToInt(txtRemotePort.Text);

        tunnels.Delete(selIndex - 1);
        tunnels.Insert(selIndex - 1, newtunnel);

        //UI Update
        sgTunnels.Cells[0, selIndex] := txtTunnelName.Text;
        sgTunnels.Cells[1, selIndex] := txtLocalPort.Text;
        sgTunnels.Cells[2, selIndex] := txtSSHHost.Text;
        sgTunnels.Cells[3, selIndex] := txtSSHPort.Text;
        sgTunnels.Cells[4, selIndex] := txtRemoteHost.Text;
        sgTunnels.Cells[5, selIndex] := txtRemotePort.Text;
      end;
    finally
      Free;
    end;
end;

procedure TFormSSHTunnel.FormCreate(Sender: TObject);
begin
  sgTunnels.Cells[0,0] := 'Name';
  sgTunnels.Cells[1,0] := 'Local Port';
  sgTunnels.Cells[2,0] := 'SSH Host';
  sgTunnels.Cells[3,0] := 'SSH Port';
  sgTunnels.Cells[4,0] := 'Remote Host';
  sgTunnels.Cells[5,0] := 'Remote Port';

  connected := newtunnel.OnConnected;
  disconnected := newtunnel.OnDisconnected;
  error := newtunnel.OnError;
  sshserverauth := newtunnel.OnSSHServerAuthentication;
  sshstatus := newtunnel.OnSSHStatus;

  tunnels := TList<TiphSSHTunnel>.Create();
  selIndex := 0;
end;

procedure TFormSSHTunnel.newtunnelConnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  Log1.Lines.Add('Connection ' + IntToStr(ConnectionId) + ' Connected: ' + Description);
end;

procedure TFormSSHTunnel.newtunnelDisconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  Log1.Lines.Add('Connection ' + IntToStr(ConnectionId) + ' Disconnection: ' + Description);
end;

procedure TFormSSHTunnel.newtunnelError(Sender: TObject; ConnectionId,
  ErrorCode: Integer; const Description: string);
begin
  Log1.Lines.Add('Connection ' + IntToStr(ConnectionId) + ' Error: ' + IntToStr(ErrorCode) + ', ' + Description);
end;

procedure TFormSSHTunnel.newtunnelSSHServerAuthentication(Sender: TObject;
  HostKey: string; HostKeyB: TBytes; const Fingerprint, KeyAlgorithm, CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
  Accept := true;
  Log1.Lines.Add('SSHServerAuthentication: ' + Fingerprint);
end;

procedure TFormSSHTunnel.newtunnelSSHStatus(Sender: TObject; const Message: string);
begin
  Log1.Lines.Add('SSHStatus: ' + Message);
end;


procedure TFormSSHTunnel.sgTunnelsClick(Sender: TObject);
begin
  selIndex := sgTunnels.Row;
end;

procedure TFormSSHTunnel.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex = 0 then
  begin
    sgTunnels.Visible := true;
    Log1.Visible := false;
  end
  else
  begin
    sgTunnels.Visible := false;
    Log1.Visible := true;
  end;
end;

procedure TFormSSHTunnel.tbtnRestartClick(Sender: TObject);
var
i: Integer;
begin
  tbtnStart.Enabled := false;
  tbtnStop.Enabled := false;
  tbtnRestart.Enabled := false;

  Log1.Lines.Add('Stopping..');
  for i := 0 to tunnels.Count - 1 do
    tunnels[i].StopListening();
  Log1.Lines.Add('Stopped.');
  Log1.Lines.Add('Starting..');
  for i := 0 to tunnels.Count - 1 do
    tunnels[i].StartListening();

  tbtnStart.Enabled := false;
  tbtnStop.Enabled := true;
  tbtnRestart.Enabled := true;
  Log1.Lines.Add('Started.');
end;

procedure TFormSSHTunnel.tbtnStartClick(Sender: TObject);
var
i: Integer;
begin
  tbtnStart.Enabled := false;
  tbtnStop.Enabled := false;
  tbtnRestart.Enabled := false;

  Log1.Lines.Add('Starting..');
  for i := 0 to tunnels.Count - 1 do
    tunnels[i].StartListening();

  tbtnStart.Enabled := false;
  tbtnStop.Enabled := true;
  tbtnRestart.Enabled := true;
  Log1.Lines.Add('Started.');
end;

procedure TFormSSHTunnel.tbtnStopClick(Sender: TObject);
var
i: Integer;
begin
  tbtnStart.Enabled := false;
  tbtnStop.Enabled := false;
  tbtnRestart.Enabled := false;

  Log1.Lines.Add('Stopping..');
  for i := 0 to tunnels.Count - 1 do
    tunnels[i].StopListening();

  tbtnStart.Enabled := true;
  tbtnStop.Enabled := false;
  tbtnRestart.Enabled := false;
  Log1.Lines.Add('Stopped.');
end;

end.

