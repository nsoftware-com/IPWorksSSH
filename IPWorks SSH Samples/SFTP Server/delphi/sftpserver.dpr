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

program sftpserver;

uses
  Forms,
  gencertdlg in 'gencertdlg.pas'   {FormGencertdlg},
  sftpserverf in 'sftpserverf.pas' {FormSftpserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormSftpserver, FormSftpserver);
  Application.CreateForm(TFormGencertdl, FormGencertdl);

  Application.Run;
end.


         
