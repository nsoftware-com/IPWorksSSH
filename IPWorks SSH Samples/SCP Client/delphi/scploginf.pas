unit scploginf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TFormScplogin = class(TForm)
    GroupBox1: TGroupBox;
    cmdLogin: TButton;
    cmdCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbPassword: TLabel;
    tbServer: TEdit;
    tbUser: TEdit;
    cbAuthType: TComboBox;
    tbFilePath: TEdit;
    tbPassword: TEdit;
    btnBrowse: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure cbAuthTypeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormScplogin: TFormScplogin;

implementation

{$R *.dfm}

procedure TFormScplogin.btnBrowseClick(Sender: TObject);
  var ofd: TOpenDialog;
begin
  ofd := TOpenDialog.Create(Self);
  ofd.InitialDir := '..\..';
  if ofd.Execute
    then tbFilePath.Text := ofd.FileName
    else ShowMessage('Open file was cancelled');

  ofd.Free;
end;

procedure TFormScplogin.cbAuthTypeChange(Sender: TObject);
  var enable: Boolean;
begin
  if cbAuthType.Text = 'Password' then
  begin
    enable := false;
    lbPassword.Caption := 'Password:';
  end;

  if cbAuthType.Text = 'Public key authentication (PEM)' then
  begin
    enable := true;
    lbPassword.Caption := 'Priv key password:';
  end;

  if cbAuthType.Text = 'Public key authentication (PFX)' then
  begin
    enable := true;
    lbPassword.Caption := 'Priv key password:';
  end;

  tbFilePath.Enabled := enable;
  btnBrowse.Enabled := enable;
end;

end.
