/*
 * IPWorks SSH 2024 Java Edition - Sample Project
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
import java.util.*;
import java.lang.*;

import ipworksssh.*;

public class sftpclient extends ConsoleDemo{
	public static class myFTP extends SFTPClient {
	
		private static final long serialVersionUID = 1L;
		public long transtime;
		public long transbytes;

		public myFTP() {
			super();
			try {
				addSFTPClientEventListener(new ipworksssh.SFTPClientEventListener() {
					public void dirList(ipworksssh.SFTPClientDirListEvent e) {
						System.out.println(e.dirEntry);
					}
					public void connected(ipworksssh.SFTPClientConnectedEvent e) {
					}
					public void connectionStatus(ipworksssh.SFTPClientConnectionStatusEvent e) {
					}
					public void disconnected(ipworksssh.SFTPClientDisconnectedEvent e) {
					}
					public void endTransfer(ipworksssh.SFTPClientEndTransferEvent e) {
						long endtime;
						endtime = System.currentTimeMillis();
						transtime = endtime - sftp1.transtime;
					}
					public void error(ipworksssh.SFTPClientErrorEvent e) {
						System.out.println("Error "+e.errorCode+": "+e.description);
					}
					
					public void SSHCustomAuth(ipworksssh.SFTPClientSSHCustomAuthEvent e){}
					
					public void SSHKeyboardInteractive(ipworksssh.SFTPClientSSHKeyboardInteractiveEvent e) {
					}					
					public void SSHServerAuthentication(ipworksssh.SFTPClientSSHServerAuthenticationEvent e) {
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
					public void SSHStatus(ipworksssh.SFTPClientSSHStatusEvent e) {
					}
					public void startTransfer(ipworksssh.SFTPClientStartTransferEvent e) {
						transtime = System.currentTimeMillis();
					}
					public void transfer(ipworksssh.SFTPClientTransferEvent e) {
						transbytes = e.bytesTransferred;
					}
					@Override
					public void log(SFTPClientLogEvent arg0) {
					}
				});
			} catch (TooManyListenersException e) {
				e.printStackTrace();
			}
		}
	}
	private static void logon(myFTP ftp) throws IPWorksSSHException {
		String host = prompt("Host", ":");
			int port = Integer.valueOf(prompt("Port", ":", "22")).intValue();
			ftp.setSSHUser(prompt("User", ":"));
			ftp.setSSHPassword(prompt("Password", ":"));

			ftp.SSHLogon(host,port);
	}
	private static myFTP sftp1 = null;

	public static void main(String[] args) {
		sftp1 = new myFTP();
		String command;
		String[] argument;
		String pathname;

		try {
			logon(sftp1);
			
			System.out.println("?        bye     help     put     rmdir");
			System.out.println("append   cd      ls       pwd     mkdir");
			System.out.println("quit     get     open     rm");
			
			while (true) {
				sftp1.setRemoteFile("");
				command = prompt("sftp", ">");
				argument = command.split("\\s");	// argument[0] is a command name
				if (argument.length == 0) {
					;	// do nothing
				} else	if (argument[0].equalsIgnoreCase("?") || argument[0].equalsIgnoreCase("help")) {
					System.out.println("?        bye     help     put     rmdir");
					System.out.println("append   cd      ls       pwd     mkdir");
					System.out.println("quit     get     open     rm");
				} else if (argument[0].equalsIgnoreCase("append")) {	// append localfile remotefile
					if(argument.length <3)
					{
						System.out.println("usage: append localFile remoteFile");
						continue;
					}
					sftp1.setLocalFile(argument[1]);
					sftp1.setRemoteFile(argument[2]);
					sftp1.append();
				} else if (argument[0].equalsIgnoreCase("bye") || argument[0].equalsIgnoreCase("quit")) {
					sftp1.SSHLogoff();
					System.out.println("Goodbye.");
					System.exit(0);
				} else if (argument[0].equalsIgnoreCase("cd")) {	// cd remotepath
					if(argument.length <2)
					{
						System.out.println("usage: cd remotePath");
						continue;
					}
					sftp1.changeRemotePath(argument[1]);
				} else if (argument[0].equalsIgnoreCase("get")) {	// get remotefile
					if(argument.length <2)
					{
						System.out.println("usage: get remoteFile");
						continue;
					}					
					sftp1.setRemoteFile(argument[1]);
					sftp1.setLocalFile(argument[1]);
					sftp1.download();
					System.out.println(sftp1.transbytes
							+" bytes sent in "+((float)sftp1.transtime/1000.)
							+" seconds. ("+((float)sftp1.transbytes)/((float)sftp1.transtime)
							+" KBps)");
				} else if (argument[0].equalsIgnoreCase("ls")) {	// ls [dir]
					if (argument.length > 1) {
						pathname = sftp1.getRemotePath();
						sftp1.changeRemotePath(argument[1]);
						sftp1.listDirectory();
						sftp1.changeRemotePath(pathname);
					} else {
						sftp1.listDirectory();
					}
				} else if (argument[0].equalsIgnoreCase("mkdir")) {	// mkdir dir
					if(argument.length <2)
					{
						System.out.println("usage: mkdir directory");
						continue;
					}
					sftp1.makeDirectory(argument[1]);
				} else if (argument[0].equalsIgnoreCase("mv")) {	// mv remotefile1 remotefile2
					if(argument.length <3)
					{
						System.out.println("usage: mv remoteFile newRemoteFile");
						continue;
					}					
					sftp1.setRemoteFile(argument[1]);
					sftp1.renameFile(argument[2]);
				} else if (argument[0].equalsIgnoreCase("open")) {	// open				
					sftp1.SSHLogoff();
					logon(sftp1);
				} else if (argument[0].equalsIgnoreCase("put")) {	// put localfile
					if(argument.length <2)
					{
						System.out.println("usage: put localFile");
						continue;
					}						
					java.io.File localFile = new java.io.File(argument[1]);
					sftp1.setRemoteFile(localFile.getName());
					sftp1.setLocalFile(argument[1]);
					sftp1.upload();
					System.out.println(sftp1.transbytes
							+" bytes sent in "+((float)sftp1.transtime/1000.)
							+" seconds. ("+((float)sftp1.transbytes)/((float)sftp1.transtime)
							+" KBps)");
				} else if (argument[0].equalsIgnoreCase("pwd")) {
					System.out.println(sftp1.getRemotePath());
				} else if (argument[0].equalsIgnoreCase("rm")) {	// rm file
					if(argument.length <2)
					{
						System.out.println("usage: rmdir file");
						continue;
					}	
					sftp1.deleteFile(argument[1]);
				} else if (argument[0].equalsIgnoreCase("rmdir")) {	// rmdir dir
					if(argument.length <2)
					{
						System.out.println("usage: rmdir directory");
						continue;
					}	
					sftp1.removeDirectory(argument[1]);
				} else if (argument[0].length() == 0) {
					;	// Do nothing
				} else {
					System.out.println("Bad command / Not implemented in demo.");
				}
			}
		} catch (IPWorksSSHException e) {
			System.out.println("IPWorksSSHException: code="+e.getCode()+"; description=\""+e.getMessage()+"\"");
		} catch (Exception e) {
                  	System.out.println("Exception: message=\""+e.getMessage()+"\"");
			e.printStackTrace();
		}
                System.out.println("exited.");
                System.exit(0);
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



