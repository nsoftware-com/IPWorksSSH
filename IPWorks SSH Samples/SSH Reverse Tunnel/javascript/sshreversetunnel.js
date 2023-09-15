/*
 * IPWorks SSH 2022 JavaScript Edition - Sample Project
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
  var sshreversetunnel = new ipworksssh.sshreversetunnel();

  if (process.argv.length == 2) {
    // print help/usage
    console.log(
      "Usage:\n\tnode .\\sshreversetunnel.js <user@sshserver.com> <sshpassword> <sshforwardport> [remotehost] [remoteport]"
    );
    process.exit();
  }

  sshreversetunnel.on("SSHServerAuthentication", function (e) {
    e.accept = true;
  });
  sshreversetunnel.on("Connected", function (e) {
    console.log("Connected to " + sshreversetunnel.getSSHHost() + ":" + sshreversetunnel.getSSHPort());
  });
  sshreversetunnel.on("Disconnected", function (e) {
    console.log("Disconnected");
  });
  sshreversetunnel.on("SSHChannelOpenRequest", function (e) {
    console.log("Connection from " + e.originAddress + ":" + e.originPort + " OK");
  });
  sshreversetunnel.on("SSHChannelOpened", function (e) {
    console.log("Channel[" + e.channelId + "] Opened");
  });
  sshreversetunnel.on("SSHChannelClosed", function (e) {
    console.log("Channel[" + e.channelId + "] Closed");
  });
  sshreversetunnel.on("Error", function (e) {
    console.log("Error: " + e.description);
  });

  sshreversetunnel.setLocalPort(parseInt(process.argv[2]));
  var sshinfo = process.argv[2].split("@");

  sshreversetunnel.setSSHHost(sshinfo[1]);
  sshreversetunnel.setSSHPort(22);
  sshreversetunnel.setSSHUser(sshinfo[0]);
  sshreversetunnel.setSSHPassword(process.argv[3]);
  console.log("Requesting forwarding for port " + parseInt(process.argv[4]));
  try {
    await sshreversetunnel.SSHLogon(sshinfo[1], 22);
    if (process.argv.length == 7) {
      await sshreversetunnel.requestForwarding(
        "",
        parseInt(process.argv[4]),
        process.argv[5],
        parseInt(process.argv[6])
      );
    } else {
      await sshreversetunnel.requestForwarding("", parseInt(process.argv[4]), "", 0);
    }
    console.log("Forwarding request successful.");
    while (true) {
      await sshreversetunnel.doEvents();
    }
  } catch (e) {
    if (e.code == 1098) {
      console.log("Forwarding request denied.");
    } else {
      if (e) {
        console.log(e.message);
      }
    }
  }
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
