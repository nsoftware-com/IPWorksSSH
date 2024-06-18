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

class sshreversetunnelDemo
{
  private static SSHReverseTunnel sshreversetunnel = new nsoftware.IPWorksSSH.SSHReverseTunnel();

  static void Main(string[] args)
  {
    if (args.Length < 12)
    {
      Console.WriteLine("usage: sshreversetunnel /s hostserver /u user /p password /fp forwardport /r remotehost /rp remoteport\n");
      Console.WriteLine("  hostserver       the host ssh server to tunnel incoming connections through");
      Console.WriteLine("  user             the username to use for authentication");
      Console.WriteLine("  password         the password to use for authentication");
      Console.WriteLine("  forwardport      the remote port on the ssh server to request forwarding from");
      Console.WriteLine("  remotehost       the remote host to forward connections to");
      Console.WriteLine("  remoteport       the remote port to forward connections to");
      Console.WriteLine("\nExample: sshreversetunnel /s 192.168.1.2 /u myusername /p mypassword /fp 7777 /r www.microsoft.com /rp 80\n");
    }
    else
    {
      sshreversetunnel.OnSSHServerAuthentication += sshreversetunnel_OnSSHServerAuthentication;
      sshreversetunnel.OnError += sshreversetunnel_OnError;
      sshreversetunnel.OnConnected += sshreversetunnel_OnConnected;
      sshreversetunnel.OnDisconnected += sshreversetunnel_OnDisconnected;
      sshreversetunnel.OnSSHChannelOpenRequest += sshreversetunnel_OnSSHChannelOpenRequest;
      sshreversetunnel.OnSSHChannelOpened += sshreversetunnel_OnSSHChannelOpened;
      sshreversetunnel.OnSSHChannelClosed += sshreversetunnel_OnSSHChannelClosed;

      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
        int forwardPort = int.Parse(myArgs["fp"]);

        sshreversetunnel.SSHAuthMode = SSHReverseTunnelSSHAuthModes.amPassword;
        sshreversetunnel.SSHHost = myArgs["s"];
        sshreversetunnel.SSHUser = myArgs["u"];
        sshreversetunnel.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        sshreversetunnel.SSHLogon(sshreversetunnel.SSHHost, 22);

        Console.WriteLine("Requesting forwarding for port " + forwardPort);
        sshreversetunnel.RequestForwarding("", forwardPort, myArgs["r"], int.Parse(myArgs["rp"]));
        Console.WriteLine("Forwarding request granted.");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            sshreversetunnel.SSHLogoff();
            break;
          }
          else
          {
            sshreversetunnel.DoEvents();
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

  private static void sshreversetunnel_OnSSHServerAuthentication(object sender, SSHReverseTunnelSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshreversetunnel_OnError(object sender, SSHReverseTunnelErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshreversetunnel_OnConnected(object sender, SSHReverseTunnelConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshreversetunnel_OnDisconnected(object sender, SSHReverseTunnelDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
  }

  private static void sshreversetunnel_OnSSHChannelOpenRequest(object sender, SSHReverseTunnelSSHChannelOpenRequestEventArgs e)
  {
    Console.WriteLine("Connection from " + e.OriginAddress + ":" + e.OriginPort + " OK");
  }

  private static void sshreversetunnel_OnSSHChannelOpened(object sender, SSHReverseTunnelSSHChannelOpenedEventArgs e)
  {
    Console.WriteLine("SSH Channel Opened - " + e.ChannelId);
  }

  private static void sshreversetunnel_OnSSHChannelClosed(object sender, SSHReverseTunnelSSHChannelClosedEventArgs e)
  {
    Console.WriteLine("SSH Channel Closed - " + e.ChannelId);
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