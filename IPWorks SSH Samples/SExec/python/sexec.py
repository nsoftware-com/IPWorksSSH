# 
# IPWorks SSH 2024 Python Edition - Sample Project
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

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)


def fireStdErr(e):
  print("StdErr: " + bytes.decode(e.text))

def fireStdOut(e):
  print("StdOut: " + bytes.decode(e.text))

def fireSSHStatus(e):
  print("Status: " + e.message)

def fireSSHServerAuth(e):
  print("\nServer provided the following fingerprint:\n%s\n"%e.fingerprint)
  cntue = input("Would you like to continue [y/n]: ")
  if cntue == "Y" or cntue == "y":
      e.accept = True

def newHost():
  host = input("Host: ")
  sexec.ssh_host = host
  port = input("Port (22): ")
  if port == '':
    port = 22
  else:
    port = int(port)
  sexec.ssh_port = port
  user = input("User: ")
  sexec.ssh_user = user
  password = input("Password: ")
  sexec.ssh_password = password
  sexec.ssh_logon(host, port)

sexec = SExec()
sexec.on_ssh_server_authentication = fireSSHServerAuth
sexec.on_ssh_status = fireSSHStatus
sexec.on_stdout = fireStdOut
sexec.on_stderr = fireStdErr

try:
    newHost()
    command = input("Command: ")
    sexec.execute(command)

    while True:
      cntu = input("Would you like to run another command? [y/n]: ")
      if cntu == "n" or cntu == "N":
        print("Goodbye!")
        sexec.ssh_logoff()
        break
      else:
        diffHost = input("Would you like to use a different server? [y/n]: ")
        if diffHost == "n" or diffHost == "N":
          command = input("Command: ")
          sexec.execute(command)
        else:
          sexec.ssh_logoff()
          newHost()
          command = input("Command: ")
          sexec.execute(command)

except IPWorksSSHError as e:
    print( "Error %s"%e.message)
except KeyboardInterrupt:
    print( "Exiting..." )



