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
  const sshell = new ipworksssh.sshell();

  if (process.argv.length === 2) {
    // print help/usage
    console.log("Usage:\n\tnode .\\sshell.js <user@sshserver.com> <sshpassword> [port]");
    process.exit();
  }
  sshell.on("SSHServerAuthentication", function (e) {
    e.accept = true;
  });
  // usage examples
  // ex: node ./sshell.js user@sshhost.domain.com password
  // ex: node ./sshell.js user@10.0.1.21 password

  const user_and_hostname = process.argv[2].split("@");
  const user = user_and_hostname[0];
  const hostname = user_and_hostname[1];
  const password = process.argv[3];
  let port;
  if (!process.argv[4]) {
    port = 22;
  } else {
    port = process.argv[4];
  }

  sshell.setSSHUser(user);
  sshell.setSSHPassword(password);
  
  console.log(`Connecting to host "${hostname}"; port: ${port} \r\n`);

  try {
    await sshell.SSHLogon(hostname, Number(port));
  } catch (e) {
    console.log("Error connecting to SSH Server.");
    process.exit();
  }

  sshell.on("Stdout", (e) => {
    process.stdout.write(String(e.text));
  });

  rl.on("line", async function (line) {
    if (line === "bye" || line === "quit") {
      await sshell.SSHLogoff();
      process.exit();
    }
    await sshell.execute(line);
  });
}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
