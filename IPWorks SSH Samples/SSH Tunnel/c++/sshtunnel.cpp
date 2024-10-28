/*
 * IPWorks SSH 2024 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksssh.h"

#define LINE_LEN 80

class MySSHTunnel: public SSHTunnel
{
public:

	MySSHTunnel()
	{
	}

	virtual int FireSSHServerAuthentication(SSHTunnelSSHServerAuthenticationEventParams *e)
	{
		if (e->Accept) return 0;
		printf("\nServer provided the following fingerprint:\n %s\n",
		       e->Fingerprint);
		printf("Would you like to continue? [y/n] ");
		char command[LINE_LEN];
		fgets(command,LINE_LEN,stdin);
		command[strlen(command)-1] = '\0';
		if (!strcmp(command, "y")) e->Accept = true;
		else exit(0);
		return 0;
	}

	virtual int FireSSHStatus(SSHTunnelSSHStatusEventParams *e)
	{
		printf( "%s\n", e->Message );
		return 0;
	}

	virtual int FireError(SSHTunnelErrorEventParams *e )
	{
		printf("Error %i: %s", e->ErrorCode, e->Description);
		return 0;
	}


};

int main(int argc, char **argv)
{

	MySSHTunnel tunnel;         // tunnel object
	char command[LINE_LEN];     // user's command
	char *argument;             // arguments to the user's command
	int ret_code=0;

	printf ("SSH Host: " );
	fgets(command,LINE_LEN,stdin);
	command[strlen(command)-1] = '\0';
	tunnel.SetSSHHost( command );
	
	printf("SSH Port: ");
	fgets(command, LINE_LEN, stdin);
	command[strlen(command) - 1] = '\0';
	tunnel.SetSSHPort(atoi(command));

	printf ("User: " );
	fgets(command,LINE_LEN,stdin);
	command[strlen(command)-1] = '\0';
	tunnel.SetSSHUser( command );

	printf("Password: ");
	fgets(command,LINE_LEN,stdin);
	command[strlen(command)-1] = '\0';
	tunnel.SetSSHPassword( command );


	printf("Forward Host: ");
	fgets(command, LINE_LEN, stdin);
	command[strlen(command) - 1] = '\0';
	tunnel.SetSSHForwardHost(command);


	printf("Forward Port: ");
	fgets(command, LINE_LEN, stdin);
	command[strlen(command) - 1] = '\0';
	tunnel.SetSSHForwardPort(atoi(command));

	printf("Starting SSH Tunnel ...\n");
	ret_code = tunnel.StartListening();

	if (ret_code) goto done;

	printf("Listening on localport %d ...\n", tunnel.GetLocalPort());
	printf("Press Ctrl+C to quit\n");

	while (1)
	{
		ret_code = tunnel.DoEvents();
		if (ret_code) goto done;
	}

done:
	if (ret_code)     // Got an error.  The user is done.
	{
		printf( "\nError: %d", ret_code );
		if (tunnel.GetLastError())
		{
			printf( " \"%s\"\n", tunnel.GetLastError() );
		}
	}
	exit(ret_code);
	return 0;
}




