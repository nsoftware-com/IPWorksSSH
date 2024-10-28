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

class sshtunnelDemo
{
  private static SSHTunnel sshtunnel = new nsoftware.IPWorksSSH.SSHTunnel();

  static void Main(string[] args)
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
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshtunnel.LocalPort = int.Parse(myArgs["port"]);
        sshtunnel.SSHAuthMode = SSHTunnelSSHAuthModes.amPassword;
        sshtunnel.SSHHost = myArgs["s"];
        sshtunnel.SSHPort = 22;
        sshtunnel.SSHUser = myArgs["u"];
        sshtunnel.SSHPassword = myArgs["p"];

        sshtunnel.SSHForwardHost = myArgs["f"];
        sshtunnel.SSHForwardPort = int.Parse(myArgs["fp"]);

        Console.WriteLine("Starting SSH tunnel...");
        sshtunnel.StartListening();
        Console.WriteLine("SSH tunnel started. Listening on local port " + sshtunnel.LocalPort + ".");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            sshtunnel.Shutdown();
            break;
          }
          else
          {
            sshtunnel.DoEvents();
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

  private static void sshtunnel_OnSSHStatus(object sender, SSHTunnelSSHStatusEventArgs e)
  {
    Console.WriteLine(e.Message);
  }

  private static void sshtunnel_OnSSHServerAuthentication(object sender, SSHTunnelSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshtunnel_OnError(object sender, SSHTunnelErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshtunnel_OnConnected(object sender, SSHTunnelConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshtunnel_OnDisconnected(object sender, SSHTunnelDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
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