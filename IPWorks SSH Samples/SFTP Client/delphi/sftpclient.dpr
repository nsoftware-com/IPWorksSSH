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

program sftpclient;

uses
  Forms,
  sftpdialogf in 'sftpdialogf.pas'   {FormSftpdialogf},
  sftploginf in 'sftploginf.pas'   {FormSftploginf},
  sftpprogressf in 'sftpprogressf.pas'   {FormSftpprogressf},
  sftpclientf in 'sftpclientf.pas' {FormSftpclient};

begin
  Application.Initialize;

  Application.CreateForm(TFormSftpclient, FormSftpclient);
  Application.CreateForm(TFormSftpdialog, FormSftpdialog);

  Application.CreateForm(TFormSftplogin, FormSftplogin);

  Application.CreateForm(TFormSftpprogress, FormSftpprogress);

  Application.Run;
end.


         
