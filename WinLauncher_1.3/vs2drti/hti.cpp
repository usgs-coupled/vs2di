// htpost.cpp : Defines the entry point for the application.
//
// Spawns a java app using the -jar jarfile [args...] 
// (ie javaw.exe -jar phast.jar)
//
// Before starting the app adds ..\lib\Win32 to the path
//
// Assumes that a Java Runtime Environment ( > 1.2) is properly installed
//
// Determines the location of javaw.exe by reading the registry
//    HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\CurrentVersion = N.N
//    HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\N.N\JavaHome
//    where N.N is the current installed version (ie 1.4)
//
// Expects the following layout:
//    .\bin
//    .\bin\${EXE_NAME}
//    .\lib
//    .\lib\${IDS_JARFILE}
//    .\lib\Win32
//    .\lib\Win32/*.dll
//
// ie
//    .\bin
//    .\bin\htpost.exe
//    .\lib
//    .\lib\hti.jar
//    .\lib\jhdf5.jar
//    .\lib\jhdfobj.jar
//    .\lib\phast.jar
//    .\lib\htJni.dll
//    .\lib\trimesh.dll
//    Note: hti.jar contains MANIFEST/Main-Class="ht.htpost"
//    and MANIFEST/Class-Path="jhdf.jar jhdf5.jar jhdfobj.jar"
#include "stdafx.h"
#include "hti.h"

// UNICODE => std::wstring   ANSI => std::string
typedef std::basic_string<TCHAR> tstring;

// COMMENT: {6/28/2007 4:30:52 PM}#ifndef JAVA_ARGS
// COMMENT: {6/28/2007 4:30:52 PM}#define JAVA_ARGS       { TEXT("ht.htApp") }
// COMMENT: {6/28/2007 4:30:52 PM}#endif
// COMMENT: {6/28/2007 4:30:52 PM}
// COMMENT: {6/28/2007 4:30:52 PM}#ifndef APP_CLASSPATH
// COMMENT: {6/28/2007 4:30:52 PM}#define APP_CLASSPATH   { TEXT("..\\lib\\hti.jar"), TEXT("..\\lib\\jh.jar") , TEXT("..\\lib\\htiHelp.jar") }
// COMMENT: {6/28/2007 4:30:52 PM}#endif

// Global Variables:
static TCHAR *java_args[] = JAVA_ARGS;
static TCHAR *app_classpath[] = APP_CLASSPATH;

tstring g_err_msg;

// Foward declarations of functions included in this code module:
BOOL AddPath(tstring& extpath);
BOOL GetExtPath(tstring& app_path, tstring& extpath);
BOOL CreateCommandLine(HINSTANCE hInstance, LPSTR lpCmdLine, tstring& cmdline);
BOOL GetArgs(LPSTR lpCmdLine, tstring& args);
BOOL GetJavaExe(HINSTANCE hInstance, tstring& java_exe);
BOOL GetClassPath(HINSTANCE hInstance, tstring& class_path);
BOOL GetAppPath(HINSTANCE hInstance, tstring& app_path);
BOOL RunCommandLine(tstring& cmdline);
int LoadString(HINSTANCE hInstance, UINT uID, tstring& strT);


int APIENTRY _tWinMain(HINSTANCE hInstance,
					   HINSTANCE hPrevInstance,
					   LPSTR     lpCmdLine,
					   int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	tstring cmdline;

	// ie "C:\Program Files\Java\jre1.6.0_01\bin\javaw.exe"
	//     -jar "C:\Program Files\USGS\hti\lib\hti.jar" "ht.htPostProcessor"
	if (!::CreateCommandLine(hInstance, lpCmdLine, cmdline))
	{
		::OutputDebugString(TEXT("CreateCommandLine failed\n"));
		return EXIT_FAILURE;
	}

	tstring app_path;
	if (!::GetAppPath(hInstance, app_path))
	{
		::OutputDebugString(TEXT("GetAppPath failed\n"));
		return EXIT_FAILURE;
	}

	tstring extpath;
	if (!::GetExtPath(app_path, extpath))
	{
		::OutputDebugString(TEXT("GetExtPath failed\n"));
		return EXIT_FAILURE;
	}

	if (!::AddPath(extpath))
	{
		::OutputDebugString(TEXT("AddPath failed\n"));
		return EXIT_FAILURE;
	}

	if (!::RunCommandLine(cmdline))
	{
		::OutputDebugString(TEXT("RunCommandLine failed\n"));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

BOOL GetExtPath(tstring& app_path, tstring& extpath)
{
	BOOL bOk = TRUE;	
	extpath = app_path + tstring(TEXT("..\\lib"));
	return bOk;
}

BOOL GetAppPath(HINSTANCE hInstance, tstring& app_path)
{
	TCHAR drive[_MAX_DRIVE];
	TCHAR dir[_MAX_DIR];
	TCHAR fname[_MAX_FNAME];
	TCHAR ext[_MAX_EXT];
	TCHAR szBuff[_MAX_PATH];
	TCHAR szOutput[_MAX_PATH];

	// Get exe path
	if (!::GetModuleFileName(hInstance, szBuff, _MAX_PATH))
	{
		::OutputDebugString(TEXT("GetModuleFileName failed"));
		return FALSE;

	}	
	_tsplitpath_s(szBuff, drive, _MAX_DRIVE, dir, _MAX_DIR, fname, _MAX_FNAME, ext, _MAX_EXT);
	_tmakepath_s(szOutput, _MAX_PATH, drive, dir, NULL, NULL);

	app_path = szOutput;
	return TRUE;
}


BOOL AddPath(tstring& extpath)
{
	BOOL bOk;
	
	// add extpath to Path and set
	if (extpath.size() > 0) {
		// get Path
		tstring path;
		DWORD dwBuf = ::GetEnvironmentVariable(TEXT("Path"), NULL, 0);
		///path.reserve(dwBuf + 1);
		///path.resize(dwBuf);
		TCHAR* buffer = new TCHAR[dwBuf + 1];
		dwBuf = ::GetEnvironmentVariable(TEXT("Path"), buffer, dwBuf);
		path = buffer;

		// modify Path
		tstring newpath = extpath + tstring(TEXT(";")) + path;

		// set Path
		bOk = ::SetEnvironmentVariable(TEXT("Path"), newpath.c_str());
		if (!bOk)
		{
			::OutputDebugString(TEXT("SetEnvironmentVariable failed: "));
			::OutputDebugString(TEXT("Path:\n"));
			::OutputDebugString(newpath.c_str());
			::OutputDebugString(TEXT("\n"));
			return FALSE;
		}
	}
	return bOk;
}

BOOL RunCommandLine(tstring& cmdline)
{
	// create child process
    STARTUPINFO startupInfo;
    ::memset(&startupInfo, 0, sizeof(STARTUPINFO));
    startupInfo.cb = sizeof(STARTUPINFO);

	TCHAR* cmd = new TCHAR[cmdline.size() + 1];
	::_tcscpy_s(cmd, cmdline.size() + 1, cmdline.c_str());

    PROCESS_INFORMATION processInfo;
    ::memset(&processInfo, 0, sizeof(PROCESS_INFORMATION));

	BOOL bOk = ::CreateProcess(
		NULL,                    // name of executable module
		cmd,                     // command line string
		NULL,                    // SD
		NULL,                    // SD
		FALSE,                   // handle inheritance option
		NORMAL_PRIORITY_CLASS,   // creation flags
		NULL,                    // new environment block
		NULL,                    // current directory name
		&startupInfo,            // startup information
		&processInfo             // process information
	);

	if (!bOk)
	{
		::OutputDebugString(TEXT("CreateProcess failed:\n"));
		::OutputDebugString(cmd);
		::OutputDebugString(TEXT("\n"));
	}
	delete[] cmd;
	return bOk;
}

int LoadString(HINSTANCE hInstance, UINT uID, tstring& strT)
{
	const size_t MAX_STRING_LENGTH = 65535;
	///strT.reserve(_MAX_PATH + 1);
	///strT.resize(_MAX_PATH);
	//size_t length = 65535 + 1;
	TCHAR str[MAX_STRING_LENGTH];
	int nChars = ::LoadString(hInstance, uID, str, MAX_STRING_LENGTH);
	strT = str;

	//strT.resize(nChars);
	//nChars = ::LoadString(hInstance, uID, strT.begin(), _MAX_PATH);
	return nChars;
}

BOOL CreateCommandLine(HINSTANCE hInstance, LPSTR lpCmdLine, tstring& cmdline)
{
	tstring args;
	tstring java_exe;
	tstring class_path;

	if (!::GetArgs(lpCmdLine, args))
	{
		::OutputDebugString(TEXT("GetArgs failed\n"));
		return FALSE;
	}

	if (!::GetJavaExe(hInstance, java_exe))
	{
		::OutputDebugString(TEXT("GetJavaExe failed"));
		::LoadString(hInstance, IDS_NOJRE, g_err_msg);
		::MessageBox(NULL, g_err_msg.c_str(), TEXT("Error"), MB_OK | MB_ICONERROR);
		return FALSE;
	}

	if (!::GetClassPath(hInstance, class_path))
	{
		::OutputDebugString(TEXT("GetClassPath failed\n"));
		return FALSE;
	}

	cmdline = tstring(TEXT("\"")) + java_exe + tstring(TEXT("\""));
	cmdline += tstring(TEXT(" -cp "));
	cmdline += tstring(TEXT("\"")) + class_path + tstring(TEXT("\""));

    const int NUM_ARGS = (sizeof(java_args) / sizeof(TCHAR *));
	for (int i = 0; i < NUM_ARGS; ++i) {
		cmdline += tstring(TEXT(" \"")) + java_args[i] + tstring(TEXT("\""));
	}
	cmdline += tstring(TEXT(" ")) + args;

	return TRUE;
}

BOOL GetClassPath(HINSTANCE hInstance, tstring& jar_path)
{
    const int NUM_APP_CLASSPATH = (sizeof(app_classpath) / sizeof(char *));
	tstring app_path;
	if (!::GetAppPath(hInstance, app_path))
	{
		::OutputDebugString(TEXT("GetClassPath failed\n"));
		return EXIT_FAILURE;
	}
	/*
	tstring sJARFILE;
	if (!::LoadString(hInstance, IDS_JARFILE, sJARFILE))
	{
		::OutputDebugString(TEXT("LoadString failed for IDS_JARFILE\n"));
		return FALSE;
	}
	*/
	for (int i = 0; i < NUM_APP_CLASSPATH; ++i) {
		jar_path += app_path + app_classpath[i] + TEXT(";");
	}
	jar_path.resize(jar_path.size() - 1); // remove trailing path separator
	return TRUE;
}

// COMMENT: {6/28/2007 3:39:03 PM}static jboolean
// COMMENT: {6/28/2007 3:39:03 PM}AddApplicationOptions()
// COMMENT: {6/28/2007 3:39:03 PM}{
// COMMENT: {6/28/2007 3:39:03 PM}    const int NUM_APP_CLASSPATH = (sizeof(app_classpath) / sizeof(char *));
// COMMENT: {6/28/2007 3:39:03 PM}    char *s, *envcp, *appcp, *apphome;
// COMMENT: {6/28/2007 3:39:03 PM}    char home[MAXPATHLEN]; /* application home */
// COMMENT: {6/28/2007 3:39:03 PM}    char separator[] = { PATH_SEPARATOR, '\0' };
// COMMENT: {6/28/2007 3:39:03 PM}    int size, i;
// COMMENT: {6/28/2007 3:39:03 PM}    int strlenHome;
// COMMENT: {6/28/2007 3:39:03 PM}
// COMMENT: {6/28/2007 3:39:03 PM}    s = getenv("CLASSPATH");
// COMMENT: {6/28/2007 3:39:03 PM}    if (s) {
// COMMENT: {6/28/2007 3:39:03 PM}	/* 40 for -Denv.class.path= */
// COMMENT: {6/28/2007 3:39:03 PM}	envcp = (char *)MemAlloc(strlen(s) + 40);
// COMMENT: {6/28/2007 3:39:03 PM}	sprintf(envcp, "-Denv.class.path=%s", s);
// COMMENT: {6/28/2007 3:39:03 PM}	AddOption(envcp, NULL);
// COMMENT: {6/28/2007 3:39:03 PM}    }
// COMMENT: {6/28/2007 3:39:03 PM}
// COMMENT: {6/28/2007 3:39:03 PM}    if (!GetApplicationHome(home, sizeof(home))) {
// COMMENT: {6/28/2007 3:39:03 PM}	ReportErrorMessage("Can't determine application home", JNI_TRUE);
// COMMENT: {6/28/2007 3:39:03 PM}	return JNI_FALSE;
// COMMENT: {6/28/2007 3:39:03 PM}    }
// COMMENT: {6/28/2007 3:39:03 PM}
// COMMENT: {6/28/2007 3:39:03 PM}    /* 40 for '-Dapplication.home=' */
// COMMENT: {6/28/2007 3:39:03 PM}    apphome = (char *)MemAlloc(strlen(home) + 40);
// COMMENT: {6/28/2007 3:39:03 PM}    sprintf(apphome, "-Dapplication.home=%s", home);
// COMMENT: {6/28/2007 3:39:03 PM}    AddOption(apphome, NULL);
// COMMENT: {6/28/2007 3:39:03 PM}
// COMMENT: {6/28/2007 3:39:03 PM}    /* How big is the application's classpath? */
// COMMENT: {6/28/2007 3:39:03 PM}    size = 40;                                 /* 40: "-Djava.class.path=" */
// COMMENT: {6/28/2007 3:39:03 PM}    strlenHome = (int)strlen(home);
// COMMENT: {6/28/2007 3:39:03 PM}    for (i = 0; i < NUM_APP_CLASSPATH; i++) {
// COMMENT: {6/28/2007 3:39:03 PM}	size += strlenHome + (int)strlen(app_classpath[i]) + 1; /* 1: separator */
// COMMENT: {6/28/2007 3:39:03 PM}    }
// COMMENT: {6/28/2007 3:39:03 PM}    appcp = (char *)MemAlloc(size + 1);
// COMMENT: {6/28/2007 3:39:03 PM}    strcpy(appcp, "-Djava.class.path=");
// COMMENT: {6/28/2007 3:39:03 PM}    for (i = 0; i < NUM_APP_CLASSPATH; i++) {
// COMMENT: {6/28/2007 3:39:03 PM}	strcat(appcp, home);			/* c:\program files\myapp */
// COMMENT: {6/28/2007 3:39:03 PM}	strcat(appcp, app_classpath[i]);	/* \lib\myapp.jar	  */
// COMMENT: {6/28/2007 3:39:03 PM}	strcat(appcp, separator);		/* ;			  */
// COMMENT: {6/28/2007 3:39:03 PM}    }
// COMMENT: {6/28/2007 3:39:03 PM}    appcp[strlen(appcp)-1] = '\0';  /* remove trailing path separator */
// COMMENT: {6/28/2007 3:39:03 PM}    AddOption(appcp, NULL);
// COMMENT: {6/28/2007 3:39:03 PM}    return JNI_TRUE;
// COMMENT: {6/28/2007 3:39:03 PM}}

BOOL GetJavaExe(HINSTANCE hInstance, tstring& java_exe)
{
	LONG lRet;
    HKEY hKey;
	DWORD dwBufLen;
	tstring value;
	tstring key;

	//  open HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment
	tstring sJRE_KEY;
	if (!::LoadString(hInstance, IDS_JRE_KEY, sJRE_KEY))
	{
		::OutputDebugString(TEXT("LoadString failed for IDS_JRE_KEY\n"));
		return FALSE;
	}
	lRet = ::RegOpenKeyEx(HKEY_LOCAL_MACHINE, sJRE_KEY.c_str(), 0, KEY_QUERY_VALUE, &hKey);
	if (lRet != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegOpenKeyEx failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(sJRE_KEY.c_str());
		::OutputDebugString(TEXT("\n"));
		return FALSE;
	}

	// query HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\CurrentVersion
	tstring sCUR_VER;
	if (!::LoadString(hInstance, IDS_CUR_VER, sCUR_VER))
	{
		::OutputDebugString(TEXT("LoadString failed for IDS_CUR_VER\n"));
		return FALSE;
	}

	dwBufLen = 0;
	lRet = ::RegQueryValueEx(hKey, sCUR_VER.c_str(), NULL, NULL, NULL, &dwBufLen);
	if (lRet == ERROR_SUCCESS)
	{
		//value.reserve(dwBufLen/sizeof(TCHAR));
		//value.resize(dwBufLen/sizeof(TCHAR) - 1);
		//lRet = ::RegQueryValueEx(hKey, sCUR_VER.c_str(), NULL, NULL, (LPBYTE)value.begin(), &dwBufLen);
		TCHAR* current = new TCHAR[dwBufLen/sizeof(TCHAR)];
		lRet = ::RegQueryValueEx(hKey, sCUR_VER.c_str(), NULL, NULL, (LPBYTE)current, &dwBufLen);
		value = current;
	}
	if (lRet != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegQueryValueEx failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(sJRE_KEY.c_str());
		::OutputDebugString(TEXT("\\"));
		::OutputDebugString(sCUR_VER.c_str());
		::OutputDebugString(TEXT("\n"));
		::RegCloseKey(hKey);
		return FALSE;
	}

	//  close HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment
	if (::RegCloseKey(hKey) != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegCloseKey failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(sJRE_KEY.c_str());
		::OutputDebugString(TEXT("\n"));
		// continue anyway
	}


	// open HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\<CurrentVersion>
	key = sJRE_KEY.c_str();
	key += TEXT("\\");
	key += value.c_str();
	lRet = ::RegOpenKeyEx(HKEY_LOCAL_MACHINE, key.c_str(), 0, KEY_QUERY_VALUE, &hKey);
	if (lRet != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegOpenKeyEx failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(key.c_str());
		::OutputDebugString(TEXT("\n"));
		return FALSE;
	}


	// query HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\<CurrentVersion>\JavaHome
	tstring sJAVAHOME;
	if (!::LoadString(hInstance, IDS_JAVAHOME, sJAVAHOME))
	{
		::OutputDebugString(TEXT("LoadString failed for IDS_JAVAHOME"));
		return FALSE;
	}
	lRet = ::RegQueryValueEx(hKey, sJAVAHOME.c_str(), NULL, NULL, NULL, &dwBufLen);
	if (lRet == ERROR_SUCCESS)
	{
		///java_exe.reserve(dwBufLen/sizeof(TCHAR));
		///java_exe.resize(dwBufLen/sizeof(TCHAR) - 1);
		///lRet = ::RegQueryValueEx(hKey, sJAVAHOME.c_str(), NULL, NULL, (LPBYTE)java_exe.begin(), &dwBufLen);
		TCHAR* buffer = new TCHAR[dwBufLen];
		lRet = ::RegQueryValueEx(hKey, sJAVAHOME.c_str(), NULL, NULL, (LPBYTE)buffer, &dwBufLen);
		java_exe = buffer;
	}
	if (lRet != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegQueryValueEx failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(sJRE_KEY.c_str());
		::OutputDebugString(TEXT("\\"));
		::OutputDebugString(sJAVAHOME.c_str());
		::OutputDebugString(TEXT("\n"));

		::RegCloseKey(hKey);
		return FALSE;
	}


	// open HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment\<CurrentVersion>
	if (::RegCloseKey(hKey) != ERROR_SUCCESS)
	{
		::OutputDebugString(TEXT("RegCloseKey failed for HKEY_LOCAL_MACHINE\\"));
		::OutputDebugString(key.c_str());
	}

	// append \bin\javaw.exe
	// ie C:\Program Files\Java\j2re1.4.1\bin\javaw.exe
	tstring sAPPEND;
	if (!::LoadString(hInstance, IDS_APPEND, sAPPEND))
	{
		::OutputDebugString(TEXT("LoadString failed for IDS_APPEND\n"));
		return FALSE;
	}
	java_exe += sAPPEND;

	return TRUE;
}

BOOL GetArgs(LPSTR lpCmdLine, tstring& args)
{
#if defined(UNICODE)
	int nChars = ::lstrlenA(lpCmdLine);
	if (nChars > 1)
	{
		// convert lpCmdLine from ansi to unicode
		args.reserve(nChars + 1);
		args.resize(nChars);
		TCHAR* buffer = new TCHAR[nChars+1];
		if (!::MultiByteToWideChar(CP_ACP, 0, lpCmdLine, -1, buffer, nChars + 1))
		{
			delete[] buffer;
			::OutputDebugString(TEXT("MultiByteToWideChar failed\n"));
			return FALSE;
		}
		args = buffer;
		delete[] buffer;
	}
	std::string test = lpCmdLine;
	assert(test.length() == args.length());
#else
	args = lpCmdLine;
#endif
	return TRUE;
}