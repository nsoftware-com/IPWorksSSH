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

class sshserverDemo
{
  private static Sshserver sshserver = new nsoftware.async.IPWorksSSH.Sshserver();

  static async Task Main(string[] args)
  {
    sshserver.OnConnectionRequest += sshserver_OnConnectionRequest;
    sshserver.OnSSHUserAuthRequest += sshserver_OnSSHUserAuthRequest;
    sshserver.OnLog += sshserver_OnLog;
    sshserver.OnConnected += sshserver_OnConnected;
    sshserver.OnDisconnected += sshserver_OnDisconnected;
    sshserver.OnError += sshserver_OnError;
    sshserver.OnSSHChannelOpened += sshserver_OnSSHChannelOpened;
    sshserver.OnSSHStatus += sshserver_OnSSHStatus;
    sshserver.OnSSHChannelOpenRequest += sshserver_OnSSHChannelOpenRequest;
    sshserver.OnSSHServiceRequest += sshserver_OnSSHServiceRequest;
    sshserver.OnSSHChannelClosed += sshserver_OnSSHChannelClosed;
    sshserver.OnSSHChannelRequest += sshserver_OnSSHChannelRequest;
    sshserver.OnSSHChannelRequested += sshserver_OnSSHChannelRequested;

    try
    {
      Console.WriteLine("usage: sshserver [/port port]\n");
      Console.WriteLine("  port         the port to listen on (optional, default 22)");
      Console.WriteLine("\nExample: sshserver /port 1234\n");

      Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
      int port = myArgs.ContainsKey("port") ? int.Parse(myArgs["port"]) : 22;

      // Change this to the path to the server's SSH certificate store (e.g. a .pfx or .pem file).
      const string CERT_STORE = "MY";

      // Change this to the certificate store password.
      const string CERT_PASS = "";

      // Set up the SSH server.
      sshserver.SSHCert = new Certificate(CertStoreTypes.cstAuto, CERT_STORE, CERT_PASS, "*");
      sshserver.LocalPort = port;
      await sshserver.StartListening();
      Console.WriteLine("SSH server started. Listening on port " + sshserver.LocalPort + ".");
      Console.WriteLine("Note: For the purposes of this demo, you can authenticate with any user and any password.");

      Console.WriteLine("Type \"?\" for a list of commands.");
      Console.Write("sshserver> ");
      string command;
      string[] arguments;
      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  users                        list all currently connected users");
          Console.WriteLine("  disconnect <id>              disconnect client by id");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0] == "quit" || arguments[0] == "exit")
        {
          await sshserver.Shutdown();
          Console.WriteLine("SSH server stopped.");
          break;
        }
        else if (arguments[0] == "users")
        {
          foreach (SSHConnection conn in sshserver.Connections.Values)
          {
            Console.WriteLine(conn.ConnectionId);
          }
        }
        else if (arguments[0] == "disconnect")
        {
          if (arguments.Length > 1) await sshserver.Disconnect(arguments[1]);
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("sshserver> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine("Error: " + ex.Message);
    }
  }

  private static void Log(string msg)
  {
    Console.WriteLine(msg);
  }

  private static void Log(string connID, string msg)
  {
    Log("[" + connID + "]: " + msg);
  }

  #region "Events"

  private static void sshserver_OnConnectionRequest(object sender, SshserverConnectionRequestEventArgs e)
  {
    Log(e.Address + ":" + e.Port.ToString() + " is attempting to connect.");
  }

  private static void sshserver_OnSSHUserAuthRequest(object sender, SshserverSSHUserAuthRequestEventArgs e)
  {
    // Here is where you would check that the "user" and "password" arguments match e.User and e.AuthParam respectively.
    // For the purposes of this demo, all users are accepted.
    e.Accept = true;
    Log(e.User + " has successfully authenticated.");
    return;
  }

  private static void sshserver_OnLog(object sender, SshserverLogEventArgs e)
  {
    Log(e.ConnectionId, e.Message);
  }

  private static void sshserver_OnConnected(object sender, SshserverConnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Connected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void sshserver_OnDisconnected(object sender, SshserverDisconnectedEventArgs e)
  {
    Log(e.ConnectionId, "Now Disconnected - " + e.Description + " (" + e.StatusCode.ToString() + ")");
  }

  private static void sshserver_OnError(object sender, SshserverErrorEventArgs e)
  {
    Log(e.ConnectionId, "Error - " + e.Description + " (" + e.ErrorCode.ToString() + ")");
  }

  private static void sshserver_OnSSHChannelOpened(object sender, SshserverSSHChannelOpenedEventArgs e)
  {
    Log(e.ConnectionId, "SSH Channel Opened - " + e.ChannelId);
  }

  private static void sshserver_OnSSHStatus(object sender, SshserverSSHStatusEventArgs e)
  {
    Log(e.ConnectionId, e.Message);
  }

  private static void sshserver_OnSSHChannelOpenRequest(object sender, SshserverSSHChannelOpenRequestEventArgs e)
  {
    Log(e.ConnectionId, "SSH Channel Open Request - " + e.ChannelId + " (" + e.Service + ")");
    e.Accept = true;
  }

  private static void sshserver_OnSSHServiceRequest(object sender, SshserverSSHServiceRequestEventArgs e)
  {
    Log(e.ConnectionId, "SSH Service Request - " + e.Service);
    e.Accept = true;
  }

  private static void sshserver_OnSSHChannelClosed(object sender, SshserverSSHChannelClosedEventArgs e)
  {
    Log(e.ConnectionId, "SSH Channel Closed - " + e.ChannelId);
  }

  private static void sshserver_OnSSHChannelRequest(object sender, SshserverSSHChannelRequestEventArgs e)
  {
    Log(e.ConnectionId, "SSH Channel Request - " + e.ChannelId + " (Request type " + e.RequestType + ")");
    e.Success = true;
  }

  private static void sshserver_OnSSHChannelRequested(object sender, SshserverSSHChannelRequestedEventArgs e)
  {
    Log(e.ConnectionId, "SSH Channel Requested - " + e.ChannelId + " (Request type " + e.RequestType + ")");
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