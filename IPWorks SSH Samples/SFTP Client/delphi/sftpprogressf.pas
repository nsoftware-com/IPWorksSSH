unit sftpprogressf;

interface

uses
{$IFDEF LINUX}
  QForms, QStdCtrls, Classes, QControls, Controls,
  StdCtrls, ComCtrls;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, iphcore, iphtypes, iphsftpclient;
{$ENDIF}

type
  TFormSftpprogress = class(TForm)
    ButtonCancel: TButton;
    //ProgressBar1: TProgressBar;
    LabelUpDown: TLabel;
    ProgressBar1: TProgressBar;
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   FormSftpprogress: TFormSftpprogress;

implementation

uses sftpclientf;

{$R *.dfm}

procedure TFormSftpprogress.ButtonCancelClick(Sender: TObject);
begin
   try
      FormSftpclient.SFTP1.Interrupt;
   Except on E: EIPWorksSSH do
      FormSftpclient.UpdateNotes(E.Message);
   end;
end;

end.
