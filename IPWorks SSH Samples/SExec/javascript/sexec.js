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
  const sexec = new ipworksssh.sexec();

  sexec.on("Stdout", (e)=> (console.log(e.text.toString())))
  .on("Stderr", (e) => (console.log("Error: " + e.text.toString())))
  .on("SSHServerAuthentication", (e) => (e.accept = true));
  
  prompt("host", "Host", ":", "");

  rl.on("line", async function(line) {
    switch (lastPrompt) {
      case "host": {
          if (line === "")
              prompt("host", "Host", ":", "");
          else {
              sexec.setSSHHost(line);
              prompt("port", "Port", ":", "22");
          }
          break;
      }
      case "port": {
          sexec.setSSHPort(Number(line === "" ? lastDefault : line));
          prompt("user", "User", ":", "");
          break;
      }
      case "user": {
          sexec.setSSHUser(line);
          prompt("password", "Password", ":", "");
          break;
      }
      case "password": {
          sexec.setSSHPassword(line);

          console.log(`Connecting to ${sexec.getSSHHost()}; port=${sexec.getSSHPort()}`);
          sexec.setTimeout(60);
          try {
            await sexec.SSHLogon(sexec.getSSHHost(), sexec.getSSHPort());
          } catch (ex) {
            console.log(ex);
            process.exit();
          }

          console.log("******************************************************");
          console.log("Entering the Sexec command loop.");
          console.log("Type Q to exit.");
          console.log("******************************************************");
          prompt("command", "Command", ":", "");
          break;
      }
      case "command": {
        if (line === "Q" || line === "q") {
            console.log("Exited.");
            if (sexec.isConnected())
              await sexec.SSHLogoff();
            process.exit();
        } else {
          try {
            await sexec.execute(line);
          } catch (ex) {
            console.log(ex);
          }
          prompt("command", "Command", ":", "");
        }
      }
    }
  });
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
