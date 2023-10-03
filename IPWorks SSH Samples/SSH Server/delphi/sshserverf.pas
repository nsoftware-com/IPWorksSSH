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
unit sshserverf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, iphcore, iphtypes, StdCtrls, iphsshserver;

type
  TFormSshserver = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    textPort: TEdit;
    buttonStart: TButton;
    memoResponse: TMemo;
    iphSSHServer1: TiphSSHServer;
    procedure buttonStartClick(Sender: TObject);
    procedure iphSSHServer1Connected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure iphSSHServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure iphSSHServer1SSHChannelOpened(Sender: TObject;
      ConnectionId, ChannelId: Integer);
    procedure iphSSHServer1SSHServiceRequest(Sender: TObject;
      ConnectionId: Integer; const Service: string; var Accept: Boolean);
    procedure iphSSHServer1SSHStatus(Sender: TObject; ConnectionId: Integer;
      const Message: string);
    procedure FormCreate(Sender: TObject);
    procedure iphSSHServer1SSHChannelClosed(Sender: TObject; ConnectionId,
      ChannelId: Integer);
    procedure iphSSHServer1SSHChannelOpenRequest(Sender: TObject; ConnectionId,
      ChannelId: Integer; const Service: string; Parameters: string;
      var Accept: Boolean);
    procedure iphSSHServer1SSHChannelDataIn(Sender: TObject; ConnectionId,
      ChannelId: Integer; Data: string; DataB: TArray<System.Byte>);
    procedure iphSSHServer1SSHChannelRequest(Sender: TObject; ConnectionId,
      ChannelId: Integer; const RequestType: string; Packet: string;
      PacketB: TArray<System.Byte>; var Success: Boolean);
    procedure iphSSHServer1SSHUserAuthRequest(Sender: TObject;
      ConnectionId: Integer; const User, Service, AuthMethod, AuthParam: string;
      var Accept, PartialSuccess: Boolean; var AvailableMethods: string;
      const KeyAlgorithm: string);
    procedure iphSSHServer1SSHChannelRequested(Sender: TObject; ConnectionId,
      ChannelId: Integer; const RequestType: string; Packet: string;
      PacketB: TArray<System.Byte>);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSshserver: TFormSshserver;

implementation

uses
  certf, sexecf;
{$R *.dfm}

procedure TFormSshserver.buttonStartClick(Sender: TObject);
begin
  if not iphSSHServer1.Listening then
    with TFormCert.Create(self) do
    begin
      if ShowModal = mrOK then
      begin
        iphSSHServer1.SSHCertSubject  := certSubject;
        iphSSHServer1.LocalPort := StrToInt(textPort.Text);
        iphSSHServer1.DefaultTimeout := 10;
        iphSSHServer1.Listening := true;
        buttonStart.Caption := 'Stop';
        memoResponse.Lines.Add('Starting server.');
      end;
    end
  else
    begin
		  iphSSHServer1.Listening := false;
		  buttonStart.Caption := 'Start';
		  memoResponse.Lines.Add('Stopping server.');
    end;
end;

procedure TFormSshserver.FormCreate(Sender: TObject);
begin
  with TFormSexec.Create(self) do
  begin
    Show();
  end;
end;

procedure TFormSshserver.iphSSHServer1Connected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
	if not StatusCode = 0 then
	begin
		memoResponse.Lines.Add('Failure to connect. Reason : ' + Description + '.');
	end
	else
	begin
	   memoResponse.Lines.Add('Client ' + iphSSHServer1.SSHConnectionRemoteHost[ConnectionId] + ' has connected.');
	end;
end;

procedure TFormSshserver.iphSSHServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
	memoResponse.Lines.Add('Client ' + IntToStr(ConnectionId) + ' disconnected.');
end;

procedure TFormSshserver.iphSSHServer1SSHChannelRequest(Sender: TObject;
  ConnectionId, ChannelId: Integer; const RequestType: string; Packet: string;
  PacketB: TArray<System.Byte>; var Success: Boolean);
begin
	memoResponse.Lines.Add('OnSSHChannelRequest Event: ' + IntToStr(ChannelId) + ' (Request type ' + RequestType + ')');
	Success := true;
end;



procedure TFormSshserver.iphSSHServer1SSHChannelRequested(Sender: TObject;
  ConnectionId, ChannelId: Integer; const RequestType: string; Packet: string;
  PacketB: TArray<System.Byte>);
var
    sshParam  : string;
    data :  AnsiString;
    packetSend :  TBytes;
begin
	memoResponse.Lines.Add('OnSSHChannelRequested Event: ' + IntToStr(ChannelId) + ' (Request type ' + RequestType + ')');
	if CompareStr(RequestType, 'exec') = 0 then
	begin
	  if CompareStr(Packet, '') = 0  then
	  begin
		  sshParam := '';
	  end
    else
	  begin
		  sshParam := iphSSHServer1.GetSSHParam(PacketB, 'command');
	  end;
	  data :=  'You sent the command: ' +  sshParam;

    iphSSHServer1.DataToSendB[ChannelId] := TEncoding.Default.GetBytes(data);

	  packetSend := iphSSHServer1.SetSSHParam(packetSend, 's', 'exit-status');
	  packetSend := iphSSHServer1.SetSSHParam(packetSend, 'f', 'false');
	  packetSend := iphSSHServer1.SetSSHParam(packetSend, 'i', '0');

	  iphSSHServer1.SendSSHPacket(ChannelId, 98, packetSend);
	  iphSSHServer1.CloseChannel(ChannelId);
	end;
end;

procedure TFormSshserver.iphSSHServer1SSHServiceRequest(Sender: TObject;
  ConnectionId: Integer; const Service: string; var Accept: Boolean);
begin
	memoResponse.Lines.Add('OnSSHServiceRequest Event: ' + IntToStr(ConnectionId) + ' (' + Service + ')');
	Accept := true;
end;

procedure TFormSshserver.iphSSHServer1SSHStatus(Sender: TObject; ConnectionId: Integer;
  const Message: string);
begin
	memoResponse.Lines.Add('OnSSHStatus Event: ' + Message );
end;

procedure TFormSshserver.iphSSHServer1SSHUserAuthRequest(Sender: TObject;
  ConnectionId: Integer; const User, Service, AuthMethod, AuthParam: string;
  var Accept, PartialSuccess: Boolean; var AvailableMethods: string;
  const KeyAlgorithm: string);
begin
	memoResponse.Lines.Add('OnSSHUserAuthRequest Event: ' + AuthMethod + ' (' + AuthParam + ')');
	Accept := true;
end;

procedure TFormSshserver.iphSSHServer1SSHChannelClosed(Sender: TObject;
  ConnectionId, ChannelId: Integer);
begin
	memoResponse.Lines.Add('OnSSHChannelClosed Event: ' + IntToStr(ChannelId) + '');
end;

procedure TFormSshserver.iphSSHServer1SSHChannelDataIn(Sender: TObject;
  ConnectionId, ChannelId: Integer; Data: string; DataB: TArray<System.Byte>);
begin
  iphSSHServer1.DataToSendB[ChannelId] := DataB;
	memoResponse.Lines.Add('Echoing '' + Data + '' to client ' + IntToStr(ChannelId) + '.');
end;

procedure TFormSshserver.iphSSHServer1SSHChannelOpened(Sender: TObject;
  ConnectionId, ChannelId: Integer);
begin
	memoResponse.Lines.Add('OnSSHChannelOpened Event: ' + IntToStr(ChannelId));
end;


procedure TFormSshserver.iphSSHServer1SSHChannelOpenRequest(Sender: TObject;
  ConnectionId, ChannelId: Integer; const Service: string; Parameters: string;
  var Accept: Boolean);
begin
   memoResponse.Lines.Add('OnSSHChannelOpenRequest Event: ' + IntToStr(ChannelId) + ' (' + Service + ')');
   Accept := true;
end;

end.

