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

import ipworksssh.*;

public class sexec extends ConsoleDemo{

	private static class mySExec extends SExec {
		
		private static final long serialVersionUID = 1L;

		public mySExec() {
			super();
			try {
				addSExecEventListener(new ipworksssh.SExecEventListener() {
					public void stdout(ipworksssh.SExecStdoutEvent e) {
						System.out.print(new String(e.text));
					}
					public void connected(ipworksssh.SExecConnectedEvent e) {
					}
					public void connectionStatus(ipworksssh.SExecConnectionStatusEvent e) {
					}
					public void disconnected(ipworksssh.SExecDisconnectedEvent e) {
					}
					public void error(ipworksssh.SExecErrorEvent e) {
					}
					public void SSHCustomAuth(ipworksssh.SExecSSHCustomAuthEvent e){}
					public void SSHKeyboardInteractive(ipworksssh.SExecSSHKeyboardInteractiveEvent e) {
					}					
					public void SSHServerAuthentication(ipworksssh.SExecSSHServerAuthenticationEvent e) {
						e.accept = true;
					}
					public void SSHStatus(ipworksssh.SExecSSHStatusEvent e) {
					}
					public void stderr(ipworksssh.SExecStderrEvent e) {
					}
					@Override
					public void log(SExecLogEvent arg0) {
					}
				});
			} catch (java.util.TooManyListenersException e) {
				e.printStackTrace();
			}
		}
	}

	public static void main(String[] args) {
		mySExec sexec1 = new mySExec();
		String host = "";
		int port = 22;
		String user = "";
		String password = "";

		try {
			host = prompt("Host", ":");
			port = Integer.valueOf(prompt("Port number", ":", "22")).intValue();
			user = prompt("User", ":");
			password = prompt("Password", ":");
			
      System.out.println("Connecting to "+host+"; port="+port);
			sexec1.setTimeout(60);
			sexec1.setSSHUser(user);
			sexec1.setSSHPassword(password);
			sexec1.SSHLogon(host, port);

			String commandEscape = "Q";
			System.out.println("******************************************************");
			System.out.println("Entering the SExec command loop.");
			System.out.println("Type \""+commandEscape+"\" to exit.");
			System.out.println("******************************************************");
			// command loop
			for (String s = prompt("Command", ":"); !s.equalsIgnoreCase(commandEscape); s=prompt("Command", ":")) {
				sexec1.execute(s);
			}
			System.out.println("exited.");
			if (sexec1.isConnected()) {
				sexec1.SSHLogoff();
			}
		} catch (IPWorksSSHException ipwe) {
                	System.out.println("code="+ipwe.getCode()+"; message=\""+ipwe.getMessage()+"\"");
			ipwe.printStackTrace();
		}
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



