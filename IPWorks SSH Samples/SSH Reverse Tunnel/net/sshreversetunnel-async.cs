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

class sshreversetunnelDemo
{
  private static Sshreversetunnel sshreversetunnel = new nsoftware.async.IPWorksSSH.Sshreversetunnel();

  static async Task Main(string[] args)
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
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
        int forwardPort = int.Parse(myArgs["fp"]);

        sshreversetunnel.SSHAuthMode = SshreversetunnelSSHAuthModes.amPassword;
        sshreversetunnel.SSHHost = myArgs["s"];
        sshreversetunnel.SSHUser = myArgs["u"];
        sshreversetunnel.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        await sshreversetunnel.SSHLogon(sshreversetunnel.SSHHost, 22);

        Console.WriteLine("Requesting forwarding for port " + forwardPort);
        await sshreversetunnel.RequestForwarding("", forwardPort, myArgs["r"], int.Parse(myArgs["rp"]));
        Console.WriteLine("Forwarding request granted.");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (true)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            await sshreversetunnel.SSHLogoff();
            break;
          }
          else
          {
            await sshreversetunnel.DoEvents();
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

  private static void sshreversetunnel_OnSSHServerAuthentication(object sender, SshreversetunnelSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshreversetunnel_OnError(object sender, SshreversetunnelErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshreversetunnel_OnConnected(object sender, SshreversetunnelConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshreversetunnel_OnDisconnected(object sender, SshreversetunnelDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
  }

  private static void sshreversetunnel_OnSSHChannelOpenRequest(object sender, SshreversetunnelSSHChannelOpenRequestEventArgs e)
  {
    Console.WriteLine("Connection from " + e.OriginAddress + ":" + e.OriginPort + " OK");
  }

  private static void sshreversetunnel_OnSSHChannelOpened(object sender, SshreversetunnelSSHChannelOpenedEventArgs e)
  {
    Console.WriteLine("SSH Channel Opened - " + e.ChannelId);
  }

  private static void sshreversetunnel_OnSSHChannelClosed(object sender, SshreversetunnelSSHChannelClosedEventArgs e)
  {
    Console.WriteLine("SSH Channel Closed - " + e.ChannelId);
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