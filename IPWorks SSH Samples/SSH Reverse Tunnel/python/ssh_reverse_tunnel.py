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

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

def fireConnected(e):
    print("Connected to server")

def fireError(e):
  print("StdErr: %s" % e.description)

def fireLog(e):
  print(": %s" % e.message)

def fireSSHStatus(e):
  print("Status: %s" % e.message)

def fireSSHServerAuth(e):
  print("\nServer provided the following fingerprint:\n%s\n"%e.fingerprint)
  cntue = input("Would you like to continue [y/n]: ")
  if cntue == "Y" or cntue == "y":
      e.accept = True

def newHost():
  host = input("Enter Host: ")
  sshreversetunnel.ssh_host = host
  port = input("Enter Port: ")
  sshreversetunnel.ssh_port = int(port)
  user = input("Enter User: ")
  sshreversetunnel.ssh_user = user
  password = input("Password: ")
  sshreversetunnel.ssh_password = password

sshreversetunnel = SSHReverseTunnel()
sshreversetunnel.on_connected = fireConnected
sshreversetunnel.on_ssh_server_authentication = fireSSHServerAuth
sshreversetunnel.on_ssh_status = fireSSHStatus
sshreversetunnel.on_log = fireLog
sshreversetunnel.on_error = fireError

try:
    newHost()
    print("Requesting forwarding for port " + str(sshreversetunnel.ssh_port))
    sshreversetunnel.ssh_logon(sshreversetunnel.ssh_host, sshreversetunnel.ssh_port)
    while True:
      cntu = input("Would you like to continue? [y/n]: ")
      if cntu == "n" or cntu == "N":
        print("Disconnecting...")
        sshreversetunnel.ssh_logoff()
        break
      else:
        forwardhost = input("Enter forward host: ")
        forwardport = input("Enter forward port: ")
        if (forwardhost == "" and forwardport == ""):
            sshreversetunnel.request_forwarding("0.0.0.0", 7777, "www.nsoftware.com", 80)
        else:
            sshreversetunnel.request_forwarding("", sshreversetunnel.ssh_port, forwardhost, int(forwardport))
        sshreversetunnel.do_events()
except IPWorksSSHError as e:
    print( "Error %s"%e.message)
except KeyboardInterrupt:
    print( "Exiting..." )

