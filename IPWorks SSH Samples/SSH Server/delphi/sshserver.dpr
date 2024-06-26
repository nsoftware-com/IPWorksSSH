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

program sshserver;

uses
  Forms,
  certf in 'certf.pas'   {FormCertf},
  sexecf in 'sexecf.pas'   {FormSexecf},
  sshserverf in 'sshserverf.pas' {FormSshserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormSshserver, FormSshserver);
  Application.CreateForm(TFormCert, FormCert);

  Application.CreateForm(TFormSexec, FormSexec);

  Application.Run;
end.


         
