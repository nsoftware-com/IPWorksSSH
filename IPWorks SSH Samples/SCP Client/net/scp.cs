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

class scpDemo
{
  private static SCP scp = new nsoftware.IPWorksSSH.SCP();
  private static SExec sexec = new nsoftware.IPWorksSSH.SExec();

  static void Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: scp /s hostserver /u user /p password\n");
      Console.WriteLine("  hostserver   the host ssh server to connect to");
      Console.WriteLine("  user         the username to use for authentication");
      Console.WriteLine("  password     the password to use for authentication");
      Console.WriteLine("\nExample: scp /s 192.168.1.2 /u myusername /p mypassword\n");
    }
    else
    {
      scp.OnSSHServerAuthentication += scp_OnSSHServerAuthentication;
      scp.OnStartTransfer += scp_OnStartTransfer;
      scp.OnEndTransfer += scp_OnEndTransfer;
      scp.OnTransfer += scp_OnTransfer;
      scp.OnDisconnected += scp_OnDisconnected;

      sexec.OnSSHServerAuthentication += sexec_OnSSHServerAuthentication;
      sexec.OnStderr += sexec_OnStderr;
      sexec.OnStdout += sexec_OnStdout;

      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        scp.SSHAuthMode = SCPSSHAuthModes.amPassword;
        sexec.SSHAuthMode = SExecSSHAuthModes.amPassword;

        scp.SSHHost = myArgs["s"];
        sexec.SSHHost = myArgs["s"];

        scp.SSHUser = myArgs["u"];
        sexec.SSHUser = myArgs["u"];

        scp.SSHPassword = myArgs["p"];
        sexec.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        scp.SSHLogon(scp.SSHHost, 22);
        sexec.SSHLogon(sexec.SSHHost, 22);

        Console.WriteLine("Type \"?\" for a list of commands.");
        string command;
        string[] arguments;
        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("?     ls     put     get     exit");
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            scp.SSHLogoff();
            sexec.SSHLogoff();
            break;
          }
          else if (arguments[0] == "ls")
          {
            if (arguments.Length > 1)
            {
              sexec.Execute("ls " + arguments[1]);
            }
            else
            {
              sexec.Execute("ls");
            }
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              scp.LocalFile = arguments[1];
              scp.RemoteFile = arguments[2];
              scp.Upload();
            }
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              scp.LocalFile = arguments[1];
              scp.RemoteFile = arguments[1];
              scp.Download();
            }
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          scp.RemoteFile = "";
          Console.Write("scp> ");
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  #region "Events"

  private static void scp_OnSSHServerAuthentication(object sender, SCPSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void scp_OnStartTransfer(object sender, SCPStartTransferEventArgs e)
  {
    if (e.Direction == 0) // Client
    {
      Console.WriteLine("Uploading " + e.LocalFile + " to " + e.RemoteFile);
    }
    else // Server
    {
      Console.WriteLine("Downloading " + e.LocalFile + " from " + e.RemoteFile);
    }
  }

  private static void scp_OnEndTransfer(object sender, SCPEndTransferEventArgs e)
  {
    if (e.Direction == 0) // Client
    {
      Console.WriteLine("Uploaded " + e.LocalFile + " to " + e.RemoteFile);
    }
    else // Server
    {
      Console.WriteLine("Downloaded " + e.LocalFile + " from " + e.RemoteFile);
    }
  }

  private static void scp_OnTransfer(object sender, SCPTransferEventArgs e)
  {
    if (e.Direction == 0) // Client
    {
      Console.WriteLine(e.PercentDone + "% Uploaded");
    }
    else // Server
    {
      Console.WriteLine(e.PercentDone + "% Downloaded");
    }
  }

  private static void scp_OnDisconnected(object sender, SCPDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
  }

  private static void sexec_OnSSHServerAuthentication(object sender, SExecSSHServerAuthenticationEventArgs e)
  {
    // If this is reached, then the server certificate has already been accepted.
    e.Accept = true;
  }

  private static void sexec_OnStderr(object sender, SExecStderrEventArgs e)
  {
    Console.WriteLine("Error: " + e.Text);
  }

  private static void sexec_OnStdout(object sender, SExecStdoutEventArgs e)
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
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
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