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

class sshclientDemo
{
  private static SSHClient sshclient = new nsoftware.IPWorksSSH.SSHClient();

  static void Main(string[] args)
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
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshclient.SSHAuthMode = SSHClientSSHAuthModes.amPassword;
        sshclient.SSHHost = myArgs["s"];
        sshclient.SSHUser = myArgs["u"];
        sshclient.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        sshclient.SSHLogon(sshclient.SSHHost, 22);

        string channelId = sshclient.OpenChannel("session");
        sshclient.OpenTerminal(channelId, "vt100", 80, 24, false, "\0");
        sshclient.StartService(channelId, "shell", "");

        Console.WriteLine("Type \"quit\" to exit the application.");
        string command;
        while (sshclient.Channels[channelId].ReadyToSend)
        {
          command = Console.ReadLine();

          if (command == "quit" || command == "exit")
          {
            sshclient.CloseChannel(channelId);
            sshclient.SSHLogoff();
            break;
          }
          else
          {
            sshclient.SendText(channelId, command);
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

  private static void sshclient_OnSSHStatus(object sender, SSHClientSSHStatusEventArgs e)
  {
    Console.WriteLine(e.Message);
  }

  private static void sshclient_OnSSHServerAuthentication(object sender, SSHClientSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshclient_OnSSHChannelRequest(object sender, SSHClientSSHChannelRequestEventArgs e)
  {
    Console.WriteLine(e.ChannelId + ", " + e.RequestType);
  }

  private static void sshclient_OnSSHChannelData(object sender, SSHClientSSHChannelDataEventArgs e)
  {
    Console.WriteLine(e.ChannelData);
  }

  private static void sshclient_OnError(object sender, SSHClientErrorEventArgs e)
  {
    Console.WriteLine("Error: " + e.ErrorCode + ", " + e.Description);
  }

  private static void sshclient_OnConnected(object sender, SSHClientConnectedEventArgs e)
  {
    Console.WriteLine("Connected with status code " + e.StatusCode + " and description " + e.Description);
  }

  private static void sshclient_OnDisconnected(object sender, SSHClientDisconnectedEventArgs e)
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