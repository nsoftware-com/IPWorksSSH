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


def fireSSHServerAuthentication(e):
  print("Server provided a key identified by this SHA256 hash: " + sshell1.config("SSHFingerprintSHA256"))
  if (input("Accept? [y/n] ") == "y"):
    e.accept = True
  else:
    e.accept = False

def fireStdout(e):
  print(str(e.text, encoding='utf-8'), end="")

def fireStderr(e):
  print(str(e.text, encoding='utf-8'), end="")

# Get user info
# These could be passed directly. Example:
# py secure-shell.py myserver 22 myuser mypass
ensureArg(sys.argv, "Host: ", 1)
ensureArg(sys.argv, "Port: ", 2)
ensureArg(sys.argv, "User: ", 3)
ensureArg(sys.argv, "Password: ", 4)

# Create instance
sshell1 = SShell()

# Add basic events
sshell1.on_ssh_server_authentication = fireSSHServerAuthentication
sshell1.on_stdout = fireStdout
sshell1.on_stderr = fireStderr

# Configure properties
sshell1.config("ShellPrompt=]$")     # Change this to match you prompt
sshell1.config("TerminalModes=53=0") # Disables echo
sshell1.set_ssh_auth_mode(2) # AM_PASSWORD
sshell1.set_ssh_host(sys.argv[1])
sshell1.set_ssh_port(int(sys.argv[2]))
sshell1.set_ssh_user(sys.argv[3])
sshell1.set_ssh_password(sys.argv[4])

# Logon
sshell1.ssh_logon(sshell1.get_ssh_host(), sshell1.get_ssh_port())

# Loop accepting commands, displaying replies, and checking for errors
while(True):
  try:
    command = input()
    if (command == "exit" or command == "quit"):
      sys.exit(0)
    else:
      sshell1.send_command(command)
  except IPWorksSSHError as e:
    print("IPWorksSSHError: " + e.message)
  except Exception as e:
    print("Exception: " + e)

