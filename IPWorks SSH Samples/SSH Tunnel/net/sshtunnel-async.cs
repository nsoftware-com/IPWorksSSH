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

class sshtunnelDemo
{
  private static Sshtunnel sshtunnel = new nsoftware.async.IPWorksSSH.Sshtunnel();

  static async Task Main(string[] args)
  {
    if (args.Length < 12)
    {
      Console.WriteLine("usage: sshtunnel /port port /s hostserver /u user /p password /f forwardhost /fp forwardport\n");
      Console.WriteLine("  port            the local port for incoming connections");
      Console.WriteLine("  hostserver      the host ssh server to tunnel incoming connections through");
      Console.WriteLine("  user            the username to use for authentication");
      Console.WriteLine("  password        the password to use for authentication");
      Console.WriteLine("  forwardhost     the remote host to forward connections to");
      Console.WriteLine("  forwardport     the remote port to forward connections to");
      Console.WriteLine("\nExample: sshtunnel /port 777 /s 192.168.1.2 /u myusername /p mypassword /f www.microsoft.com /fp 80\n");
    }
    else
    {
      sshtunnel.OnSSHStatus += sshtunnel_OnSSHStatus;
      sshtunnel.OnSSHServerAuthentication += sshtunnel_OnSSHServerAuthentication;
      sshtunnel.OnError += sshtunnel_OnError;
      sshtunnel.OnConnected += sshtunnel_OnConnected;
      sshtunnel.OnDisconnected += sshtunnel_OnDisconnected;

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshtunnel.LocalPort = int.Parse(myArgs["port"]);
        sshtunnel.SSHAuthMode = SshtunnelSSHAuthModes.amPassword;
        sshtunnel.SSHHost = myArgs["s"];
        sshtunnel.SSHPort = 22;
        sshtunnel.SSHUser = myArgs["u"];
        sshtunnel.SSHPassword = myArgs["p"];

        sshtunnel.SSHForwardHost = myArgs["f"];
        sshtunnel.SSHForwardPort = int.Parse(myArgs["fp"]);

        Console.WriteLine("Starting SSH tunnel...");
        await sshtunnel.StartListening();
        Console.WriteLine("SSH tunnel started. Listening on local port " + sshtunnel.LocalPort + ".");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            await sshtunnel.Shutdown();
            break;
          }
          else
          {
            await sshtunnel.DoEvents();
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

  private static void sshtunnel_OnSSHStatus(object sender, SshtunnelSSHStatusEventArgs e)
  {
    Console.WriteLine(e.Message);
  }

  private static void sshtunnel_OnSSHServerAuthentication(object sender, SshtunnelSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshtunnel_OnError(object sender, SshtunnelErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshtunnel_OnConnected(object sender, SshtunnelConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshtunnel_OnDisconnected(object sender, SshtunnelDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
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