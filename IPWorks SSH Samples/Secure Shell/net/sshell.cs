/*
 * IPWorks SSH 2024 .NET Edition - Sample Project
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

﻿using System;
using nsoftware.IPWorksSSH;

class sshellDemo
{
  private static SShell sshell = new nsoftware.IPWorksSSH.SShell();

  static void Main(string[] args)
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
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshell.SSHAuthMode = SShellSSHAuthModes.amPassword;
        sshell.SSHHost = myArgs["s"];
        sshell.SSHUser = myArgs["u"];
        sshell.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        sshell.SSHLogon(sshell.SSHHost, 22);

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            sshell.SSHLogoff();
            break;
          }
          else
          {
            sshell.Execute(command);
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

  private static void sshell_OnSSHServerAuthentication(object sender, SShellSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshell_OnStderr(object sender, SShellStderrEventArgs e)
  {
    Console.WriteLine("Error: " + e.Text);
  }

  private static void sshell_OnStdout(object sender, SShellStdoutEventArgs e)
  {
    Console.Write(e.Text);
  }

  #endregion
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}