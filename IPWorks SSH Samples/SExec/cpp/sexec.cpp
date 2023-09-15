/*
 * IPWorks SSH 2022 C++ Edition - Sample Project
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
#define LINE_LEN 100

class MySexec : public SExec
{
public:
	int FireStderr(SExecStderrEventParams *e)
	{
		fwrite(e->Text, 1, e->lenText, stderr);
		return 0;
	}
	int FireStdout(SExecStdoutEventParams *e)
	{
		fwrite(e->Text, 1, e->lenText, stdout);
		return 0;
	}

	int FireSSHServerAuthentication(SExecSSHServerAuthenticationEventParams *e)
	{
		e->Accept = true;
		return 0;
	}
};

int main(int argc, char **argv)
{
	int port; 

	if (argc < 4)
	{
		fprintf(stderr, "usage: %s <host> <port> <user> <password> <command>\n", argv[0]);
		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		exit(1);
	}

	MySexec sexec;

	sscanf(argv[2], "%d", &port);
	sexec.SetSSHPort(port);
	
	int ret_code = sexec.SetTimeout(60);
	if (ret_code) goto done;

	ret_code = sexec.SetSSHUser(argv[3]);
	if (ret_code) goto done;

	ret_code = sexec.SetSSHPassword(argv[4]);
	if (ret_code) goto done;

	ret_code = sexec.SSHLogon(argv[1], sexec.GetSSHPort());
	if (ret_code) goto done;

	ret_code = sexec.Execute(argv[5]);
	if (ret_code) goto done;

done:
	if (ret_code)
	{
		fprintf(stderr, "error: %d", ret_code);
		if (sexec.GetLastError())
			fprintf(stderr, " (%s)", sexec.GetLastError());
		fprintf(stderr, "\nexiting...\n");
		exit(1);
	}
	fprintf(stderr, "\npress <return> to continue...\n");
	getchar();
	return 0;
}


