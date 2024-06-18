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

class sshplexDemo
{
  private static SSHPlex sshplex = new nsoftware.IPWorksSSH.SSHPlex();

  static void Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: sshplex /s hostserver /u user /p password\n");
      Console.WriteLine("  hostserver   the host server to connect to");
      Console.WriteLine("  user         the username to use for authentication");
      Console.WriteLine("  password     the password to use for authentication");
      Console.WriteLine("\nExample: sshplex /s 192.168.1.2 /u myusername /p mypassword\n");
    }
    else
    {
      sshplex.OnSSHServerAuthentication += sshplex_OnSSHServerAuthentication;
      sshplex.OnDirList += sshplex_OnDirList;
      sshplex.OnDisconnected += sshplex_OnDisconnected;
      sshplex.OnStderr += sshplex_OnStderr;
      sshplex.OnStdout += sshplex_OnStdout;
      sshplex.OnDownloadComplete += sshplex_OnDownloadComplete;
      sshplex.OnUploadComplete += sshplex_OnUploadComplete;
      sshplex.OnExecuteComplete += sshplex_OnExecuteComplete;
      sshplex.OnListDirectoryComplete += sshplex_OnListDirectoryComplete;

      try
      {
        System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

        sshplex.SSHAuthMode = SSHPlexSSHAuthModes.amPassword;
        sshplex.SSHHost = myArgs["s"];
        sshplex.SSHUser = myArgs["u"];
        sshplex.SSHPassword = myArgs["p"];

        // Default port for SSH is 22.
        sshplex.SSHLogon(sshplex.SSHHost, 22);

        string opId = "";

        // Note: The use of each protocol in this demo is limited. For more functionality, see the demos for each component individually.

        // SExec Operation - Executing commands.
        sshplex.ChannelType = SSHPlexChannelTypes.cstSExec;
        Console.WriteLine("\n\nThe SExec ChannelType executes commands on the SSH server.");
        Console.WriteLine("The Execute method executes the command and the Stdout and Stderr events hold the response.");
        Console.WriteLine("The exit code of the command is present in the ExitStatus parameter of the ExecuteComplete event.");
        Console.WriteLine("The demo will now execute the \"ls\" command.");
        System.Threading.Thread.Sleep(3000);

        opId = sshplex.Execute("ls"); // Begin execution. Output is fired through Stdout, then the ExecuteComplete event fires.
        WaitForOpsToFinish(opId); // Wait for execute operation to complete.

        // SFTP Operation - File transfer over SFTP.
        sshplex.ChannelType = SSHPlexChannelTypes.cstSftp;
        sshplex.Overwrite = true;
        Console.WriteLine("\n\nThe SFTP ChannelType is used to transfer files using SFTP.");

        Console.WriteLine("Type \"?\" for a list of commands.");
        string command;
        string[] arguments;
        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("?       cd      pwd");
            Console.WriteLine("get     put     exit");
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            sshplex.SSHLogoff();
            break;
          }
          else if (arguments[0] == "cd")
          {
            if (arguments.Length > 1) sshplex.ChangeRemotePath(arguments[1]);
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              sshplex.RemoteFile = arguments[1];
              sshplex.LocalFile = arguments[1];
              sshplex.Download();
              WaitForOpsToFinish(opId);
              Console.WriteLine("File downloaded");
            }
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              sshplex.LocalFile = arguments[1];
              sshplex.RemoteFile = arguments[2];
              sshplex.Upload();
              WaitForOpsToFinish(opId);
              Console.WriteLine("File uploaded");
            }
          }
          else if (arguments[0] == "pwd")
          {
            Console.WriteLine(sshplex.QueryRemotePath());
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Invalid command.");
          } // End of command checking.

          sshplex.RemoteFile = "";
          Console.Write("sftp> ");
        }

        // Uncomment the lines below to explore more channel options and cancelling operations.

        // SShell Operation - Executing commands using an interactive shell.
        /*sshplex.ChannelType = SSHPlexChannelTypes.cstSShell;
        Console.WriteLine("\n\nThe SShell ChannelType is used to send commands over an interactive shell.");
        Console.WriteLine("The Execute method executes the command and the Stdout and Stderr events hold the response.");
        Console.WriteLine("The demo will execute the \"ls\" command.");
        System.Threading.Thread.Sleep(3000);

        opId = sshplex.Execute("ls"); // Begin execution. Output is fired through Stdout, then the ExecuteComplete event fires.
        System.Threading.Thread.Sleep(1000); // This slightly delays the main thread, allowing Stdout to fire in time.
        WaitForOpsToFinish(opId); // Wait for execute operation to complete.

        // SCP Operation - File transfer over SCP.
        /*sshplex.ChannelType = SSHPlexChannelTypes.cstScp;
        sshplex.Overwrite = true;
        Console.WriteLine("\n\nThe SCP ChannelType is used to transfer files using SCP.");

        Console.WriteLine("Type \"?\" for a list of commands.");
        string command;
        string[] arguments;
        while (true)
        {
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("?     put     get     exit");
          }
          else if (arguments[0] == "quit" || arguments[0] == "exit")
          {
            sshplex.SSHLogoff();
            break;
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              sshplex.LocalFile = arguments[1];
              sshplex.RemoteFile = arguments[2];
              sshplex.Upload();
              WaitForOpsToFinish(opId);
              Console.WriteLine("File uploaded");
            }
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              sshplex.LocalFile = arguments[1];
              sshplex.RemoteFile = arguments[1];
              sshplex.Download();
              WaitForOpsToFinish(opId);
              Console.WriteLine("File downloaded");
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

          sshplex.RemoteFile = "";
          Console.Write("scp> ");
        }*/

        // Cancelling Operations.
        /*Console.WriteLine("\nOnce an operation has been started it may later be cancelled using the CancelOperation method.");
        // Start 2 ListDirectory operations, then cancel 1.
        sshplex.ChannelType = SSHPlexChannelTypes.cstSftp;

        string opId1 = sshplex.ListDirectory();
        string opId2 = sshplex.ListDirectory();
        Console.WriteLine("ListDirectory operations in progress...");
        sshplex.CancelOperation(opId1);
        WaitForOpsToFinish(opId2); // Wait for still running ListDirectory operation to complete.

        Console.WriteLine("Scroll up to see the output from the two ListDirectory operations. One was cancelled and one was completed.\n");*/
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
    }
  }

  private static void WaitForOpsToFinish(string opId)
  {
    while (sshplex.Operations.ContainsKey(opId))
    {
      sshplex.DoEvents();
    }
  }

  #region "Events"

  private static void sshplex_OnSSHServerAuthentication(object sender, SSHPlexSSHServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void sshplex_OnDirList(object sender, SSHPlexDirListEventArgs e)
  {
    Console.WriteLine(e.DirEntry);
  }

  private static void sshplex_OnDisconnected(object sender, SSHPlexDisconnectedEventArgs e)
  {
    Console.WriteLine("Disconnected with status code " + e.StatusCode + " and description " + e.Description);
    Environment.Exit(0);
  }

  private static void sshplex_OnStderr(object sender, SSHPlexStderrEventArgs e)
  {
    Console.WriteLine("Error: " + e.Text);
  }

  private static void sshplex_OnStdout(object sender, SSHPlexStdoutEventArgs e)
  {
    Console.WriteLine(e.Text);
  }

  private static void sshplex_OnDownloadComplete(object sender, SSHPlexDownloadCompleteEventArgs e)
  {
    Console.WriteLine("Downloaded: " + e.RemoteFile);
  }

  private static void sshplex_OnUploadComplete(object sender, SSHPlexUploadCompleteEventArgs e)
  {
    Console.WriteLine("Uploaded: " + e.LocalFile);
  }

  private static void sshplex_OnExecuteComplete(object sender, SSHPlexExecuteCompleteEventArgs e)
  {
    Console.WriteLine("Execute complete");
  }

  private static void sshplex_OnListDirectoryComplete(object sender, SSHPlexListDirectoryCompleteEventArgs e)
  {
    if (e.ErrorCode == 0)
    {
      Console.WriteLine("Directory listing of path " + e.RemotePath + " complete\n");
    }
    else
    {
      Console.WriteLine("ListDirectory operation cancelled for RemotePath: " + e.RemotePath + "  OperationId: " + e.OperationId);
    }
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