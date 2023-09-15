/*
 * IPWorks SSH 2022 Java Edition - Sample Project
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
 */

import java.io.*;
import java.io.IOException;
import java.lang.*;

import ipworksssh.*;

public class sshplex extends ConsoleDemo{

    public static void main(String[] args) {
      Sshplex sshplex1 = new Sshplex();

      System.out.println("************************************************************************************");
      System.out.println("* This demo shows how to use SSHPlex to perform various operations over a single   *");
      System.out.println("* SSH connection. This demo uses many hardcoded values to better illustrate the    *");
      System.out.println("* use of the component in a simple manner. Please review the source code before    *");
      System.out.println("* running this demo.                                                               *");
      System.out.println("************************************************************************************\r\n");

      try {
        // Implement event Listeners
        sshplex1.addSshplexEventListener(new ipworksssh.DefaultSshplexEventListener() {
          public void dirList(SshplexDirListEvent e) {
            System.out.println(e.fileName);
          }

          public void disconnected(SshplexDisconnectedEvent e) {
            System.out.println("Goodbye");
          }

          public void downloadComplete(SshplexDownloadCompleteEvent e) {
            System.out.println("Downloaded: " + e.remoteFile);
          }

          public void error(SshplexErrorEvent e) {
            if (e.errorCode == 1040) return; // don't report errors for manual cancellations
            System.out.println("=====================\nError encountered\nError code: " + e.errorCode + "\nError description: " + e.description + "=====================");
          }

          public void executeComplete(SshplexExecuteCompleteEvent e) {
            System.out.println("Execute complete");
          }

          public void listDirectoryComplete(SshplexListDirectoryCompleteEvent e) {
            if (e.errorCode == 0) {
              System.out.println("Directory listing of path " + e.remotePath + " complete");
            } else if (e.errorCode == 1040) {
              System.out.println("ListDirectory operation cancelled for RemotePath: " + e.remotePath + "	OperationId: " + e.operationId);
            }
          }

          public void SSHServerAuthentication(SshplexSSHServerAuthenticationEvent e) {
            if (e.accept) {
              return;
            }
            System.out.println("Server provided the certificate which has following fingerprint: " + e.fingerprint);
            char answer = ask("Would you like to continue anyways");
            if (answer == 'y') {
              e.accept = true;
            } else {
              System.out.println("exited.");
              System.exit(0);
            }
            return;
          }

          public void stderr(SshplexStderrEvent e) {
            System.out.println(new String(e.text));
          }

          public void stdout(SshplexStdoutEvent e) {
            System.out.println("\n" + new String(e.text));
          }

          public void uploadComplete(SshplexUploadCompleteEvent e) {
            System.out.println("Uploaded: " + e.localFile);
          }

        });

        logon(sshplex1);
        char userResponse;
        String opId = "";

        // SExec Operation - Executing commands
        System.out.print("\n\nThe SEXec ChannelType executes commands on the SSH server.");
        System.out.print("The Execute method executes the command and the StdOut and StdErr events hold the response.");
        System.out.print("The exit code of the command is present in the ExitStatus parameter of the ExecuteComplete event.");
        System.out.println("The demo will now execute the \"ls\" command.");
        PromptToContinue();

        sshplex1.setChannelType(Sshplex.cstSExec);
        opId = sshplex1.execute("ls"); // Begin execution. Output is fired through StdOut, then the ExecuteComplete fires.
        WaitForOpsToFinish(sshplex1, opId);  // Wait for execute operation to complete

        //SFTP Operation - File transfer over SFTP (Upload and Download)

        sshplex1.setChannelType(Sshplex.cstSftp); // set ChannelType to SFTP
        sshplex1.setOverwrite(true);
        System.out.print("\n\nThe SFTP ChannelType is used to transfer files using SFTP.");
        System.out.println("Specify the local and remote files that will be transferred.");
        System.out.println("Current remote path: " + sshplex1.getRemotePath());
        userResponse = ask("Do you want to change directory", "?");
        if (Character.toLowerCase(userResponse) == 'y') {
          sshplex1.setRemotePath(prompt("Input remote path"));
        }
        prompt("Press enter to list the current directory", ":");
        System.out.println("Listing directory...");
        opId = sshplex1.listDirectory();
        WaitForOpsToFinish(sshplex1, opId);

        userResponse = ask("Do you want to upload a file", "?");
        if (Character.toLowerCase(userResponse) == 'y') {
          sshplex1.setLocalFile(prompt("Local file (absolute path)"));
          sshplex1.setRemoteFile(prompt("Remote filename"));
          System.out.println("Uploading...");
          opId = sshplex1.upload();
          WaitForOpsToFinish(sshplex1, opId);
          System.out.println("Upload Complete");
        }

        userResponse = ask("Do you want to download a file", "?");
        if (Character.toLowerCase(userResponse) == 'y') {
          sshplex1.setLocalFile(prompt("Local file (absolute path)"));
          sshplex1.setRemoteFile(prompt("Remote filename"));
          System.out.println("Downloading...");
          opId = sshplex1.download();
          WaitForOpsToFinish(sshplex1, opId);
          System.out.println("Download Complete");
        }


        // Uncomment the lines below to explore more channel options and cancelling operations
        
        // SShell Operation - Executing commands using an interactive shell

//      sshplex1.setChannelType(Sshplex.cstSShell);
//      System.out.print("\n\nThe SShell ChannelType is used to send commands over an interactive shell.");
//      System.out.print("The Execute method executes the command and the StdOut and StdErr events hold the response.");
//      System.out.println("The demo will execute the \"ls\" command.");
//
//      String shellPrompt = prompt("Input the remote host shell prompt (for instance: \"~]$\")");
//
//      sshplex1.config("ShellPrompt="+shellPrompt); // The ShellPrompt config tells the component to wait until the specified value is returned by the server
//      opId = sshplex1.execute("ls"); // Begin execution. Output is fired through StdOut, then the ExecuteComplete fires.
//      WaitForOpsToFinish(sshplex1, opId);  // Wait for execute operation to complete

        // SCP Operation - File transfer over SCP (Upload and Download)

//		sshplex1.setChannelType(Sshplex.cstScp);
//
//		sshplex1.setOverwrite(true);
//		System.out.print("\n\nThe SCP ChannelType is used to transfer files using SCP.");
//		System.out.println("Specify the local and remote files that will be transferred.");
//
//		userResponse = ask("Do you want to upload a file", "?");
//		if (Character.toLowerCase(userResponse) == 'y') {
//			sshplex1.setLocalFile(prompt("Local file (absolute path)"));
//			sshplex1.setRemotePath(prompt("Remote Path (press enter for current dir)",":", ""));
//			sshplex1.setRemoteFile(prompt("Remote filename"));
//			opId = sshplex1.upload();
//			WaitForOpsToFinish(sshplex1,opId);
//          System.out.println("Upload Complete");
//		}
//
//		userResponse = ask("Want to download a file", "?");
//		if (Character.toLowerCase(userResponse) == 'y') {
//			sshplex1.setRemotePath(prompt("Remote Path (press enter for current dir)", ":", ""));
//			sshplex1.setRemoteFile(prompt("Remote filename"));
//			sshplex1.setLocalFile(prompt("Local file (absolute path)"));
//			opId = sshplex1.download();
//			WaitForOpsToFinish(sshplex1,opId);
//          System.out.println("Download Complete");
//		}


        // Cancelling Operations

//        System.out.println("\nOnce an operation has been started it may later be cancelled using the CancelOperation method");
//        PromptToContinue();
//        // Start 2 listDirectories, then cancel 1
//        sshplex1.setChannelType(Sshplex.cstSftp);
//        String opId1 = sshplex1.listDirectory();
//        String opId2 = sshplex1.listDirectory();
//        System.out.println("ListDirectory operations in progress...");
//        sshplex1.cancelOperation(opId1);
//        WaitForOpsToFinish(sshplex1, opId2); // wait for still running listDirectory operation to complete
//        System.out.println("Scroll up to see the output from the two listDirectory operations. One was cancelled and one was completed\n");

      } catch (IPWorksSSHException e) {
        System.out.println("IPWorksSSHException: code=" + e.getCode() + "; description=\"" + e.getMessage() + "\"");
      } catch (Exception e) {
        System.out.println("Exception: message=\"" + e.getMessage() + "\"");
        e.printStackTrace();
      }
      System.out.println("Demo complete.");
      System.exit(0);
    }


  private static void logon(Sshplex sshplex) throws IPWorksSSHException {
    // Logon to a SSH host by setting the following properties and then calling SSHLogon
    String host = prompt("Host", ":");
    int port = Integer.valueOf(prompt("Port", ":", "22")).intValue();
    sshplex.setSSHUser(prompt("User", ":"));
    sshplex.setSSHPassword(prompt("Password", ":"));
    sshplex.SSHLogon(host,port);
  }

  static void PromptToContinue() {
    prompt("Press enter to continue...");
  }

  static void WaitForOpsToFinish(Sshplex sshplex, String opId) throws IPWorksSSHException {
    while (sshplex.getOperations().containsKey(opId)) {
      sshplex.doEvents();
    }
  }
}




class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksSSHException) {
      System.out.print(" (" + ((IPWorksSSHException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



