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
  print("Error: [" + e.code + "] " + e.description)

def fireStdout(e):
  print(bytes.decode(e.text))

def fireSSHServerAuthentication(e):
  e.accept = True

def fireDirList(e):
  print(e.dir_entry)

def ensureArg(argument, prompt, index):
  if len(argument) <= index:
    while len(argument) <= index:
      argument.append(None)
    argument[index] = input(prompt)

def logon(scp, sexec, host):
  try:
    print("Port (22):"),
    port = input()
    if port == '':
      port = 22
    else:
      port = int(port)

    print("User:"),
    user = input()

    print("Password:"),
    password = input()

    scp.ssh_user = user
    scp.ssh_password = password
    scp.ssh_logon(host, port)

    sexec.ssh_user = user
    sexec.ssh_password = password
    sexec.ssh_logon(host, port)
  except IOError as e:
    print("IOExeption: %s" % e)

global transtime
global transbytes

try:
  sexec1 = SExec()
  scp1 = SCP()
  global transbytes
  global transtime

  scp1.on_error = fireError
  scp1.on_ssh_server_authentication = fireSSHServerAuthentication
  sexec1.on_ssh_server_authentication = fireSSHServerAuthentication
  scp1.on_dir_list = fireDirList
  sexec1.on_stdout = fireStdout

  ensureArg(sys.argv, "Host: ", 1)
  ensureArg(sys.argv, "Port (22): ", 2)
  ensureArg(sys.argv, "User: ", 3)
  ensureArg(sys.argv, "Password: ", 4)
  host = sys.argv[1]
  if sys.argv[2] == "": port = 22
  else: port = int(sys.argv[2])

  scp1.ssh_user = sys.argv[3]
  scp1.ssh_password = sys.argv[4]
  scp1.ssh_logon(host, port)

  sexec1.ssh_user = sys.argv[3]
  sexec1.ssh_password = sys.argv[4]
  sexec1.ssh_logon(host, port)

  print("Available commands are as follows:")
  print("?        bye     help     put     cd")
  print("ls       pwd     quit     get    ")
  print("open     close")

  while (True):
    scp1.remote_file = ""
    print("scp>"),
    command = input()
    argument = str.split(command)    # argument[0] is a command name
    argument[0] = argument[0].lower()
    if len(argument) == 0:
      None                # do nothing
    elif argument[0] == "?" or argument[0] == "help":
      print("?        bye     help     put     cd")
      print("ls       pwd     quit     get    ")
      print("open     close")
    elif argument[0] == "bye" or argument[0] == "quit":
      scp1.ssh_logoff()
      print("Goodbye.")
      break
    elif argument[0] == "cd":    # cd remotepath
      ensureArg(argument, "Remote Path: ", 1)
      scp1.remote_path = argument[1]
      print("CWD " + scp1.remote_path)
    elif argument[0] == "close":
      scp1.ssh_logoff()
      sexec1.ssh_logoff()
      print("Logging off")
    elif argument[0] == "get":    # get remotefile localfile
      ensureArg(argument, "Remote File: ", 1)
      ensureArg(argument, "Local File: ", 2)
      scp1.remote_file = argument[1]
      scp1.local_file = argument[2]
      scp1.download()
      print(argument[1] + " downloaded.")
    elif argument[0] == "ls": # ls [dir]
      if len(argument) > 1:
        sexec1.execute("ls -1p " + argument[1])
      else:
        sexec1.execute("ls -1p " + scp1.remote_path)
    elif argument[0] == "open":    # open host
      ensureArg(argument, "Host: ", 1)
      if scp1.connected: scp1.ssh_logoff()
      if sexec1.connected: sexec1.ssh_logoff()
      logon(scp1, sexec1, argument[1])
    elif argument[0] == "put":    # put localfile remotefile
      ensureArg(argument, "Local File: ", 1)
      ensureArg(argument, "Remote File: ", 2)
      scp1.local_file = argument[1]
      scp1.remote_file = argument[2]
      scp1.upload()
      print(argument[1] + " uploaded.")
    elif argument[0] == "pwd":
      print(scp1.remote_path)
      None
    else:
      print("Bad command / Not implemented in demo.")
except IPWorksSSHError as e:
  print("IPWorksSSHError: \"" + e.message + "\"")
except Exception as e:
  print("Exception: %s" % e)

sys.exit(0)


