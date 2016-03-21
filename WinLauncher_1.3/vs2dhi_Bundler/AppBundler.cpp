#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <direct.h>
#include <shellapi.h>

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
	char home[MAX_PATH], home1[MAX_PATH], parameter[MAX_PATH+100], thecwd[MAX_PATH];
	char *exe;

	// Retrieve the full path and filename for the executable file
	GetModuleFileName(NULL, home, MAX_PATH);

	// Search for the last file separator (back slash) and replace it 
	// with a null character. Do this twice. This yields the home directory
	*(strrchr(home, '\\')) = '\0';
	*(strrchr(home, '\\')) = '\0';

	// Put quotes around home
	sprintf(home1, "\"%s\"", home);

	if (strncmp(lpCmdLine, "-d", 2) == 0)
	{
		exe = "..\\jre\\bin\\java.exe";
	}
	else
	{
		exe = "..\\jre\\bin\\javaw.exe";
	}
	strcpy(parameter, " -cp vs2di.jar;vs2dhiHelp.jar;jh.jar vs2.vs2App -heat -w ");
	strcat(parameter, home1);
	_getcwd(thecwd,MAX_PATH);
	if (ShellExecute(NULL,NULL,exe,parameter,thecwd,SW_SHOWNORMAL)<=(HINSTANCE)32)
	{
		MessageBox(NULL,
			"Could not start the application",
			"Please reinstall the application",MB_OK);
		return 2; 
	}

	return 0;
}




