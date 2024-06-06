/*
 * IPWorks SSH 2024 JavaScript Edition - Sample Project
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
 */
 
const readline = require("readline");
const ipworksssh = require("@nsoftware/ipworksssh");

if(!ipworksssh) {
  console.error("Cannot find ipworksssh.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  const scp = new ipworksssh.scp();
  const sexec = new ipworksssh.sexec();
  let stdoutFired = false;

  scp.setOverwrite(true);

  // Set up SCP events.
  scp.on("StartTransfer",  (e) => {
    if (e.direction === 0) {
      console.log(`Uploading  ${e.localFile} to ${e.remoteFile}`);
    } else {
      console.log(`Downloading  ${e.localFile} from ${e.remoteFile}`);
    }
  })
  .on("EndTransfer", (e) => {
    if (e.direction === 0) {
      console.log(`Uploaded ${e.localFile} to  ${e.remoteFile}`);
    } else {
      console.log(`Downloaded ${e.localFile} from  ${e.remoteFile}`);
    }
  })
  .on("Transfer", (e) => {
    if (e.direction === 0) {
      console.log(`${e.percentDone} % Uploaded`);
    } else {
      console.log(`${e.percentDone} % Downloaded`);
    }
  })
  .on("SSHStatus", (e) => (console.log(e.message)))
  .on("SSHServerAuthentication", (e) => (e.accept = true));

  // Set up SExec events.
  sexec.on("Stdout", (e) => {
    console.log(e.text.toString());
    stdoutFired = true;
  })
  .on("Stderr", (e) => (console.log(`Error: ${e.text.toString()}`)))
  .on("SSHServerAuthentication", (e) => (e.accept = true));

  printInfo();

  prompt("host", "Host", ":", "");

  rl.on("line", async function (line) {
    switch (lastPrompt) {
      case "host": {
        if (line === "") {
          prompt("host", "Host", ":", "");
        } else {
          scp.setSSHHost(line);
          sexec.setSSHHost(line);
          prompt("port", "Port", ":", "22");
        }
        break;
      }
      case "port": {
        scp.setSSHPort(Number(line === "" ? lastDefault : line));
        sexec.setSSHPort(Number(line === "" ? lastDefault : line));
        prompt("user", "User", ":", "");
        break;
      }
      case "user": {
        scp.setSSHUser(line);
        sexec.setSSHUser(line);
        prompt("password", "Password", ":", "");
        break;
      }
      case "password": {
        scp.setSSHPassword(line);
        sexec.setSSHPassword(line);

        try {
          await scp.SSHLogon(scp.getSSHHost(), scp.getSSHPort());
          try {
            await sexec.SSHLogon(sexec.getSSHHost(), sexec.getSSHPort());
            console.log("");
            console.log("The valid commands for this demo are as follows:");
            console.log("?     exit     help     ls [dir]");
            console.log("put [localFile] [remoteFile]");
            console.log("get [remoteFile] [localFile]");
            console.log("");

            scp.setRemoteFile("");
            scpPrompt();
          } catch (ex) {
            console.log(`Error: ${ex}`);
            process.exit();
          }
        } catch (ex) {
          console.log(`Error: ${ex}`);
          process.exit();
        }

        break;
      }
      case "command": {
        if (line === "") {
          scp.setRemoteFile("");
          scpPrompt();
          break;
        }

        const args = line.split(/\s/);
        switch (args[0].toLowerCase()) {
          case "?":
          case "help": {
            console.log("?     exit     help     ls [dir]");
            console.log("put [localFile] [remoteFile]");
            console.log("get [remoteFile] [localFile]");
            break;
          }
          case "ls": {
            await sexec.execute(args.length === 2 ? `ls ${args[1]}` : "ls");

            while (!stdoutFired) {
              await sexec.doEvents();
            }
            stdoutFired = false;
            scp.setRemoteFile("");
            scpPrompt();
            break;
          }
          case "put": {
            if (args.length === 3) {
              scp.setLocalFile(args[1]);
              scp.setRemoteFile(args[2]);
              await scp.upload();
            } else {
              console.log("usage: put [localFile] [remoteFile]");
            }
            scp.setRemoteFile("");
            scpPrompt();
            break;
          }
          case "get": {
            if (args.length === 3) {
              scp.setRemoteFile(args[1]);
              scp.setLocalFile(args[2]);
              await scp.download();
            } else {
              console.log("usage: get [remoteFile] [localFile]");
            }
            scp.setRemoteFile("");
            scpPrompt();
            break;
          }
          case "exit": {
            await scp.SSHLogoff();
            await sexec.SSHLogoff();
            console.log("Goodbye.");
            process.exit();
            break;
          }
          case "": {
            break;
          }
          default:
            {
              console.log("Bad command / Not implemented in demo.");
            }

            await sexec.doEvents();
            await scp.doEvents();
            scp.setRemoteFile("");
            scpPrompt();
        }
      }
    }
  });
}

function scpPrompt() {
  prompt("command", "scp", ">", "");
}

function printInfo() {
  console.log("This demo shows how to use the SCP component to securely copy files to and from a ");
  console.log("remote server. The SExec component is used here to list the specified remote directory.");
  console.log();
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
