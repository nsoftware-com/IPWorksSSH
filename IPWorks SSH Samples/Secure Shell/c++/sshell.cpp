/*
 * IPWorks SSH 2022 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include "../../include/ipworksssh.h"

const int  MAX_LINE_LEN = 80;

class SShellDemo : public SShell {
public:

  virtual int FireSSHServerAuthentication(SShellSSHServerAuthenticationEventParams *e) {
    printf("Server provided a key identified by this SHA256 hash: %s\n", Config("SSHFingerprintSHA256"));
    printf("Accept? [y/n] ");
    if (getchar() == 'y') {
      e->Accept = true;
    }
    return 0;
  }

  virtual int FireStdout(SShellStdoutEventParams* e) {
    printf("%s", e->Text);
    return 0;
  }

  virtual int FireStderr(SShellStderrEventParams* e) {
    printf("Error: %s", e->Text);
    return 0;
  }
};

int exit(int code, char* msg) {
  if (code) {
    printf("[%i] %s\n", code, msg);
    getchar();
  }
  return code;
}

int main(int argc, char* argv[]) {
  SShellDemo sShellDemo;
  if (argc != 5) {
    printf("usage: sshell host port user password\n\n");
    printf("  host         the host name or address\n");
    printf("  port         the host port\n");
    printf("  user         the username\n");
    printf("  password     the password\n");
    printf("\n\nExample: sshell 192.168.1.2 22 myusername mypassword\n\n");
  }
  else {
    // Parse arguments
    char* host = argv[1];
    int   port = atoi(argv[2]);
    char* user = argv[3];
    char* pass = argv[4];
    
    // Configure SShell
    sShellDemo.Config("ShellPrompt=]$");     // Change this to match your shell
    sShellDemo.Config("TerminalModes=53=0"); // Turn off Echo. For more modes see: https://datatracker.ietf.org/doc/html/rfc4250#section-4.5.2
    sShellDemo.SetSSHAuthMode(AM_PASSWORD);
    sShellDemo.SetSSHHost(host);
    sShellDemo.SetSSHPort(port);
    sShellDemo.SetSSHUser(user);
    sShellDemo.SetSSHPassword(pass);

    // Logon
    sShellDemo.SSHLogon(sShellDemo.GetSSHHost(), sShellDemo.GetSSHPort());

    // Loop sending commands, waiting for replies, and checking for errors
    char cmd[MAX_LINE_LEN];
    while (!sShellDemo.GetLastErrorCode()) {
      // Get the command
      fgets(cmd, MAX_LINE_LEN, stdin); 

      // Check if the program should exit or send the command
      if (strcmp(cmd, "quit\n") == 0 || strcmp(cmd, "exit\n") == 0) {
        return 0;
      }
      else {
        // Send the command
        sShellDemo.SendCommand(cmd);
      }
    }
    
  }
  return exit(sShellDemo.GetLastErrorCode(), sShellDemo.GetLastError());
}

