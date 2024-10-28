unit TunnelConfigf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormTunnelConfig = class(TForm)
    Label1: TLabel;
    txtTunnelName: TEdit;
    txtLocalPort: TEdit;
    Label2: TLabel;
    txtSSHHost: TEdit;
    Label3: TLabel;
    txtSSHPort: TEdit;
    txtSSHUser: TEdit;
    txtSSHPassword: TEdit;
    txtRemoteHost: TEdit;
    txtRemotePort: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTunnelConfig: TFormTunnelConfig;

implementation

{$R *.dfm}

end.
