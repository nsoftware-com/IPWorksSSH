# 
# IPWorks SSH 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks SSH in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworksssh
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworksssh import *

input = sys.hexversion<0x03000000 and raw_input or input

def fireError(e):
  print("Error %i: %s\n" %(e.code, e.message))

def fireSSHServerAuthentication(e):
  e.accept = True

def fireDirList(e):
  print(e.dir_entry)

def ensureArg(argument, prompt, index):
  if len(argument) <= index:
    while len(argument) <= index:
      argument.append(None)
    argument[index] = input(prompt)

def logon(ftp, host):
  try:
    print("Port (22):"),
    port = input()
    if port == '':
      port = 22
    else:
      port = int(port)

    print("User:"),
    ftp.ssh_user = input()

    print("Password:"),
    ftp.ssh_password = input()

    ftp.ssh_logon(host,port)
  except IOError as e:
    print("IOException: %s" % e)

global transtime
global transbytes


try:
  sftp1 = SFTP()
  global transbytes
  global transtime

  sftp1.on_error = fireError
  sftp1.on_ssh_server_authentication = fireSSHServerAuthentication
  sftp1.on_dir_list = fireDirList

  ensureArg(sys.argv, "Host: ", 1)
  ensureArg(sys.argv, "Port(22): ", 2)
  ensureArg(sys.argv, "User: ", 3)
  ensureArg(sys.argv, "Password: ", 4)

  host = sys.argv[1]
  if sys.argv[2] == "":
    port = 22
  else:
    port = int(sys.argv[2])
  sftp1.ssh_user = sys.argv[3]
  sftp1.ssh_password = sys.argv[4]
  sftp1.ssh_logon(host, port)

  while (True):
    sftp1.remote_file = ""
    print("sftp>"),
    command = input()
    argument = str.split(command)    # argument[0] is a command name
    argument[0] = argument[0].lower()
    if len(argument) == 0:
      None                # do nothing
    elif argument[0] == "?" or argument[0] == "help":
      print("?        bye     help     put     rmdir")
      print("append   cd      ls       pwd     mkdir")
      print("quit     get     open     rm      close")
    elif argument[0] == "append":    # append localfile remotefile
      ensureArg(argument, "Local File: ", 1)
      ensureArg(argument, "Remote File: ", 2)
      sftp1.local_file = argument[1]
      sftp1.remote_file = argument[2]
      sftp1.append()
    elif argument[0] == "bye" or argument[0] == "quit":
      sftp1.ssh_logoff()
      print("Goodbye.")
      sys.exit(0)
    elif argument[0] == "cd":    # cd remotepath
      ensureArg(argument, "Remote Path: ", 1)
      sftp1.remote_path = argument[1]
      print("CWD " + sftp1.remote_path)
    elif argument[0] == "close":
      sftp1.ssh_logoff()
      print("Logging off")
    elif argument[0] == "get":    # get remotefile localfile
      ensureArg(argument, "Remote File: ", 1)
      ensureArg(argument, "Local File: ", 2)
      sftp1.remote_file = argument[1]
      sftp1.local_file = argument[2]
      sftp1.download()
      print(argument[1] + " downloaded.")
    elif argument[0] == "ls": # ls [dir]
      if len(argument) > 1:
        pathname = sftp1.remote_path
        sftp1.remote_path = argument[1]
        sftp1.list_directory()
        sftp1.remote_path = pathname
      else:
        sftp1.list_directory()
    elif argument[0] == "mkdir":    # mkdir dir
      ensureArg(argument, "Directory: ", 1)
      sftp1.make_directory(argument[1])
    elif argument[0] == "mv":    # mv remotefile1 remotefile2
      ensureArg(argument, "Source file: ", 1)
      ensureArg(argument, "Destination File: ", 2)
      sftp1.remote_file = argument[1]
      sftp1.rename_file(argument[2])
    elif argument[0] == "open":    # open host
      ensureArg(argument, "Host: ", 1)
      sftp1.ssh_logoff()
      logon(sftp1, argument[1])
    elif argument[0] == "put":    # put localfile remotefile
      ensureArg(argument, "Local File: ", 1)
      ensureArg(argument, "Remote File: ", 2)
      sftp1.local_file = argument[1]
      sftp1.remote_file = argument[2]
      sftp1.upload()
      print(argument[1] + " uploaded.")
    elif argument[0] == "pwd":
      print(sftp1.remote_path)
    elif argument[0] == "rm":    # rm file
      ensureArg(argument, "File: ", 1)
      sftp1.delete_file(argument[1])
    elif argument[0] == "rmdir":    # rmdir dir
      ensureArg(argument, "Directory: ", 1)
      sftp1.remove_directory(argument[1])
    elif len(argument[0]) == 0:
      None
    else:
      print("Bad command / Not implemented in demo.")
except IPWorksSSHError as e:
  print("IPWorksSSHError: \"" + e.message + "\"")
except Exception as e:
  print("Exception: %s" % e)

print("exited.")
sys.exit(0)


