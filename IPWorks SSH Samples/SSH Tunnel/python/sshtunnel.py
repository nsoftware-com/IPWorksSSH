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
  print("StdErr: " + e.text)

def fireStdOut(e):
  print("StdOut: " + e.text)

def fireSSHStatus(e):
  print("Status: " + e.message)

def fireSSHServerAuth(e):
  print("\nServer provided the following fingerprint:\n%s\n"%e.fingerprint)
  cntue = input("Would you like to continue [y/n]: ")
  if cntue == "Y" or cntue == "y":
      e.accept = True

def newHost():
  host = input("SSH Host: ")
  sshtunnel.ssh_host = host
  port = input("SSH Port [22]: ")
  if (port == ""):
    port = 22
  sshtunnel.ssh_port = port
  user = input("SSH User: ")
  sshtunnel.ssh_user = user
  password = input("SSH Password: ")
  sshtunnel.ssh_password = password
  localport = input("Local Port: ")
  if (localport != ""):
    sshtunnel.local_port = int(localport)
  forwardhost = input("Forward host: ")
  sshtunnel.ssh_forward_host = forwardhost
  forwardport = input("Forward port: ")
  sshtunnel.ssh_forward_port = int(forwardport)

sshtunnel = SSHTunnel()
sshtunnel.on_ssh_server_authentication = fireSSHServerAuth
sshtunnel.on_ssh_status = fireSSHStatus
sshtunnel.on_stdout = fireStdOut
sshtunnel.on_stderr = fireStdErr

try:
    newHost()
    print("Starting SSH Tunnel ...")
    sshtunnel.start_listening()
    print('Listening on local port ' + str(sshtunnel.get_local_port()) + "...")
    print("Press Ctrl+C to quit...")
    try:
      while True:
        sshtunnel.do_events()
    except KeyboardInterrupt:
      print("SSHTunnel shutting down. Goodbye!")
      sshtunnel.shutdown()

except IPWorksSSHError as e:
  print("Error: " + e.message)
except KeyboardInterrupt:
  print("Exiting...")



