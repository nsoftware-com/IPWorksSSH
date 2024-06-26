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

program scp;

uses
  Forms,
  scploginf in 'scploginf.pas'   {FormScploginf},
  scpf in 'scpf.pas' {FormScp};

begin
  Application.Initialize;

  Application.CreateForm(TFormScp, FormScp);
  Application.CreateForm(TFormScplogin, FormScplogin);

  Application.Run;
end.


         
