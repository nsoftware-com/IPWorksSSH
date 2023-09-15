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

class sshclientDemo
{
  private static Sshclient sshclient = new nsoftware.async.IPWorksSSH.Sshclient();

  static async Task Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: sshclient /s hostserver /u user /p password\n");
      Console.WriteLine("  hostserver   the host ssh server to connect to");
      Console.WriteLine("  user         the username to use for authentication");
      Console.WriteLine("  password     the password to use for authentication");
      Console.WriteLine("\nExample: sshclient /s 192.168.1.2 /u myusername /p mypassword\n");
    }
    else
    {
      sshclient.OnSSHStatus += sshclient_OnSSHStatus;
      sshclient.OnSSHServerAuthentication += sshclient_OnSSHServerAuthentication;
      sshclient.OnSSHChannelRequest += sshclient_OnSSHChannelRequest;
      sshclient.OnSSHChannelData += sshclient_OnSSHChannelData;
      sshclient.OnError += sshclient_OnError;
      sshclient.OnConnected += sshclient_OnConnected;
      sshclient.OnDisconnected += sshclient_OnDisconnected;

      try
      {
        Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshclient.SSHAuthMode = SshclientSSHAuthModes.amPassword;
        sshclient.SSHHost = myArgs["s"];
        sshclient.SSHUser = myArgs["u"];
        sshclient.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        await sshclient.SSHLogon(sshclient.SSHHost, 22);

        string channelId = await sshclient.OpenChannel("session");
        await sshclient.OpenTerminal(channelId, "vt100", 80, 24, false, "\0");
        await sshclient.StartService(channelId, "shell", "");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (sshclient.Channels[channelId].ReadyToSend)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            await sshclient.CloseChannel(channelId);
            await sshclient.SSHLogoff();
            break;
          }
          else
          {
            await sshclient.SendText(channelId, command);
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

  private static void sshclient_OnSSHStatus(object sender, SshclientSSHStatusEventArgs e)
  {
    Console.WriteLine(e.Message);
  }

  private static void sshclient_OnSSHServerAuthentication(object sender, SshclientSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshclient_OnSSHChannelRequest(object sender, SshclientSSHChannelRequestEventArgs e)
  {
    Console.WriteLine(e.ChannelId + ", " + e.RequestType);
  }

  private static void sshclient_OnSSHChannelData(object sender, SshclientSSHChannelDataEventArgs e)
  {
    Console.WriteLine(e.ChannelData);
  }

  private static void sshclient_OnError(object sender, SshclientErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshclient_OnConnected(object sender, SshclientConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshclient_OnDisconnected(object sender, SshclientDisconnectedEventArgs e)
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