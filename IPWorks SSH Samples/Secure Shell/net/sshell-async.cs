/*
 * IPWorks SSH 2022 .NET Edition - Sample Project
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
 * 
 */

using System.Collections.Generic;
﻿using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorksSSH;

class sshellDemo
{
  private static Sshell sshell = new nsoftware.async.IPWorksSSH.Sshell();

  static async Task Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: sshell /s hostserver /u user /p password\n");
      Console.WriteLine("  hostserver   the host ssh server to connect to");
      Console.WriteLine("  user         the username to use for authentication");
      Console.WriteLine("  password     the password to use for authentication");
      Console.WriteLine("\nExample: sshell /s 192.168.1.2 /u myusername /p mypassword\n");
    }
    else
    {
      sshell.OnSSHServerAuthentication += sshell_OnSSHServerAuthentication;
      sshell.OnStderr += sshell_OnStderr;
      sshell.OnStdout += sshell_OnStdout;

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshell.SSHAuthMode = SshellSSHAuthModes.amPassword;
        sshell.SSHHost = myArgs["s"];
        sshell.SSHUser = myArgs["u"];
        sshell.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        await sshell.SSHLogon(sshell.SSHHost, 22);

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            await sshell.SSHLogoff();
            break;
          }
          else
          {
            await sshell.Execute(command);
          }
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  #region "Events"

  private static void sshell_OnSSHServerAuthentication(object sender, SshellSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshell_OnStderr(object sender, SshellStderrEventArgs e)
  {
    Console.WriteLine("Error: " + e.Text);
  }

  private static void sshell_OnStdout(object sender, SshellStdoutEventArgs e)
  {
    Console.Write(e.Text);
  }

  #endregion
}


class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}