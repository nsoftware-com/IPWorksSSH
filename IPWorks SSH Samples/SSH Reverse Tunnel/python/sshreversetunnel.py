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

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)



def fireConnected(e):
    print("Connected to server")

def fireError(e):
  print("StdErr: %s" % e.description)

def fireLog(e):
  print("Log: %s" % e.message)

def fireSSHServerAuth(e):
  print("\nServer provided the following fingerprint:\n%s\n"%e.fingerprint)
  cntue = input("Would you like to continue [y/n]: ")
  if cntue == "Y" or cntue == "y":
      e.accept = True

def setupHost():
  host = input("SSH Host: ")
  sshreversetunnel.ssh_host = host
  port = input("SSH Port [22]: ")
  if (port == ""):
      port = 22
  sshreversetunnel.ssh_port = int(port)
  user = input("SSH User: ")
  sshreversetunnel.ssh_user = user
  password = input("SSH Password: ")
  sshreversetunnel.ssh_password = password

sshreversetunnel = SSHReverseTunnel()
sshreversetunnel.on_connected = fireConnected
sshreversetunnel.on_ssh_server_authentication = fireSSHServerAuth
sshreversetunnel.on_log = fireLog
sshreversetunnel.on_error = fireError

try:
    setupHost()
    sshreversetunnel.ssh_logon(sshreversetunnel.ssh_host, sshreversetunnel.ssh_port)

    sshforwardport = input("Forward port [4444]: ")
    if (sshforwardport == ""):
        sshforwardport = "4444"
    remotehost = input("Remote host [www.nsoftware.com]: ")
    if (remotehost == ""):
        remotehost = "www.nsoftware.com"
    remoteport = input("Remote port [443]: ")
    if (remoteport == ""):
        remoteport = "443"

    print("Requesting forwarding for port " + sshforwardport + "; forwarding to: " + remotehost + ":" + remoteport)
    sshreversetunnel.request_forwarding("0.0.0.0", int(sshforwardport), remotehost, int(remoteport))
    print("Forwarding request successful.")
    print("Press Ctrl-C to quit...")
    try:
      while True:
        sshreversetunnel.do_events()
    except KeyboardInterrupt:
      print("Exiting...")
      sshreversetunnel.ssh_logoff()
except IPWorksSSHError as e:
    print( "Error %s"%e.message)
except KeyboardInterrupt:
    print( "Exiting..." )


