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

class scpDemo
{
  private static Scp scp = new nsoftware.async.IPWorksSSH.Scp();
  private static Sexec sexec = new nsoftware.async.IPWorksSSH.Sexec();

  static async Task Main(string[] args)
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
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        scp.SSHAuthMode = ScpSSHAuthModes.amPassword;
        sexec.SSHAuthMode = SexecSSHAuthModes.amPassword;

        scp.SSHHost = myArgs["s"];
        sexec.SSHHost = myArgs["s"];

        scp.SSHUser = myArgs["u"];
        sexec.SSHUser = myArgs["u"];

        scp.SSHPassword = myArgs["p"];
        sexec.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        await scp.SSHLogon(scp.SSHHost, 22);
        await sexec.SSHLogon(sexec.SSHHost, 22);

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
            await scp.SSHLogoff();
            await sexec.SSHLogoff();
            break;
          }
          else if (arguments[0] == "ls")
          {
            if (arguments.Length > 1)
            {
              await sexec.Execute("ls " + arguments[1]);
            }
            else
            {
              await sexec.Execute("ls");
            }
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              scp.LocalFile = arguments[1];
              scp.RemoteFile = arguments[2];
              await scp.Upload();
            }
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              scp.LocalFile = arguments[1];
              scp.RemoteFile = arguments[1];
              await scp.Download();
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

  private static void scp_OnSSHServerAuthentication(object sender, ScpSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void scp_OnStartTransfer(object sender, ScpStartTransferEventArgs e)
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

  private static void scp_OnEndTransfer(object sender, ScpEndTransferEventArgs e)
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

  private static void scp_OnTransfer(object sender, ScpTransferEventArgs e)
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

  private static void scp_OnDisconnected(object sender, ScpDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
  }

  private static void sexec_OnSSHServerAuthentication(object sender, SexecSSHServerAuthenticationEventArgs e)
  {
    // If this is reached, then the server certificate has already been accepted.
    e.Accept = true;
  }

  private static void sexec_OnStderr(object sender, SexecStderrEventArgs e)
  {
    Console.WriteLine("Error: " + e.Text);
  }

  private static void sexec_OnStdout(object sender, SexecStdoutEventArgs e)
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