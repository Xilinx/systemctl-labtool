/************************************************************************************************************
 * (c) Copyright 2012 - 2022 Xilinx, Inc. All rights reserved.
 * (c) Copyright 2022 - 2023 Advanced Micro Devices, Inc. All rights reserved.
 **************************************************************************************************************/
/*
 * Parts of this code comes from tclMain.c and tkMain.c
 *
 * Copyright (c) 1988-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 2000 Ajuba Solutions.
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#ifdef _WIN32
#include <Windows.h>
#if defined(_MSC_VER)
#  pragma warning(disable:4996) /* 'strcpy': This function or variable may be unsafe */
#endif

#define snprintf _snprintf
#else
#include <pthread.h>
#include <unistd.h>
#endif
#include <time.h>
#include <signal.h>
#include <sys/stat.h>

#include <tcl.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#ifndef RDI_VERSION
#define RDI_VERSION "0000.0"
#endif

static const char programName[] = "System Debugger (XSDB)";
static Tcl_ThreadId main_threadid;
static int loadIni = 1;

static time_t last_abort;

#if defined(_WIN32)
#define isatty WinIsTty
static int WinIsTty(int fd) {
    HANDLE handle;

    /*
     * For now, under Windows, we assume we are not running as a console mode
     * app, so we need to use the GUI console.  In order to enable this, we
     * always claim to be running on a tty.  This probably isn't the right
     * way to do it.
     */

    handle = GetStdHandle(STD_INPUT_HANDLE + fd);
    if ((handle == INVALID_HANDLE_VALUE) || (handle == 0)
	     || (GetFileType(handle) == FILE_TYPE_UNKNOWN)) {
	/*
	 * If it's a bad or closed handle, then it's been connected
	 * to a wish console window.
	 */

	return 1;
    } else if (GetFileType(handle) == FILE_TYPE_CHAR) {
	/*
	 * A character file handle is a tty by definition.
	 */

	return 1;
    } else {
	return 0;
    }
}
#endif

/*
 * Forward declarations for functions defined later in this file.
 */

typedef enum {
    PROMPT_NONE,		/* Print no prompt */
    PROMPT_START,		/* Print prompt for command start */
    PROMPT_CONTINUE		/* Print prompt for command continuation */
} PromptType;

#define DEFAULT_PRIMARY_PROMPT "% "

static void		Prompt(Tcl_Interp *interp, PromptType *promptPtr);
static void		StdinProc(ClientData clientData, int mask);

typedef enum {
    ComMsgMgr_MSGTYPE_STATUS,
    ComMsgMgr_MSGTYPE_ERROR,
    ComMsgMgr_MSGTYPE_TRACE,
    ComMsgMgr_MSGTYPE_INFO
} ComMsgMgr_MSGTYPE;

static void ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE type, const char *fmt, ...)
{
    FILE *log_file = stdout;
    va_list ap;

    if (type == ComMsgMgr_MSGTYPE_ERROR) {
        log_file = stderr;
    }
    va_start(ap, fmt);
    vfprintf(log_file, fmt, ap);
    fflush(log_file);
    va_end(ap);
}

static const char* time_stamp = __TIME__;
static const char* date_stamp = __DATE__;

static void display_banner()
{
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"****** %s v%s\n", programName, RDI_VERSION);
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  **** Build date : %s-%s\n", date_stamp, time_stamp);
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"    ** Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"    ** Copyright 2022-%s Advanced Micro Devices, Inc. All Rights Reserved.\n\n", date_stamp + 7);
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\n");
}

static void display_help()
{
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"Usage: xsdb [options] [tclscript] [tclargs]\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"Options:\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  -interactive\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\tEnter interactive mode after -eval or running script.\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  -help\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\tDisplay this help message.\n");
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  -no-ini\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\tDo not load xsdb.ini\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  -quiet\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\tStart XSDB in silent mode\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"  -eval tclcommand\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\tExecute <tclcommand> then exit\n") ;
    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_STATUS,"\n") ;
}

static void Tcl_XSDB_Init(Tcl_Interp *interp)
{
    char *xilvitis = getenv("XILINX_VITIS");
    char *xilsdk = getenv("XILINX_SDK");
    char xilenv[PATH_MAX];
    char *rdiroot = getenv("HDI_APPROOT");
    char *myvivado = getenv("MYVIVADO");

    xilenv[0] = '\0';

    /* Look for HDI_APPROOT first. See CR-1090203 */
    if (rdiroot != NULL && *rdiroot != '\0') {
	strncpy(xilenv, rdiroot, sizeof xilenv);
	xilenv[sizeof xilenv - 1] = '\0';
    }

    if (xilenv[0] == '\0' && xilvitis != NULL && *xilvitis != '\0') {
	strncpy(xilenv, xilvitis, sizeof xilenv);
	xilenv[sizeof xilenv - 1] = '\0';
    }

    if (xilenv[0] == '\0' && xilsdk != NULL && *xilsdk != '\0') {
	strncpy(xilenv, xilsdk, sizeof xilenv);
	xilenv[sizeof xilenv - 1] = '\0';
    }

    if (xilenv[0] != '\0') {
	char tcllibpath[PATH_MAX];
	strncpy(tcllibpath, xilenv, sizeof tcllibpath);
	tcllibpath[sizeof tcllibpath - 1] = '\0';
	strncat(tcllibpath, "/tps/tcl/tcl8.5", sizeof tcllibpath - 1);
	Tcl_SetVar(interp, "auto_path", tcllibpath, TCL_GLOBAL_ONLY | TCL_LIST_ELEMENT | TCL_APPEND_VALUE);

	/* If MYVIVADO is defined, use it to find XSDB package. Otherwise, fallback to XILINX_SDK/RDI_BASEROOT */
	tcllibpath[0] = '\0';
	if (myvivado != NULL && *myvivado != '\0') {
	    struct stat file_info;
	    strncpy(tcllibpath, myvivado, sizeof tcllibpath);
	    tcllibpath[sizeof tcllibpath - 1] = '\0';
	    strncat(tcllibpath, "/scripts/xsdb", sizeof tcllibpath - 1);
	    if (stat(tcllibpath, &file_info) != 0 || (file_info.st_mode & S_IFMT) != S_IFDIR) {
		tcllibpath[0] = '\0';
	        ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_INFO, "Cannot find scripts/xsdb in $MYVIVADO: %s\n", myvivado);
            } else {
	        ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_INFO, "Got $MYVIVADO: %s\n", myvivado);
            }
	}
	if (tcllibpath[0] == '\0') {
	    strncpy(tcllibpath, xilenv, sizeof tcllibpath);
	    tcllibpath[sizeof tcllibpath - 1] = '\0';
	    strncat(tcllibpath, "/scripts/xsdb", sizeof tcllibpath - 1);
	}
	Tcl_SetVar(interp, "auto_path", tcllibpath, TCL_GLOBAL_ONLY | TCL_LIST_ELEMENT | TCL_APPEND_VALUE);
    }
}

static void signal_handler(int sig) {
    time_t now = time(NULL);
    if (now - last_abort < 2) {
	fprintf(stderr, "exit due to repeated abort signals\n");
	Tcl_Exit(1);
    }
    last_abort = now;
}

#if defined(_WIN32)
static BOOL CtrlHandler(DWORD ctrl) {
    switch(ctrl) {
    case CTRL_C_EVENT:
    case CTRL_CLOSE_EVENT:
    case CTRL_BREAK_EVENT:
    case CTRL_SHUTDOWN_EVENT:
	signal_handler(0);
	return TRUE;
    }
    return FALSE;
}
#else
static void signal_thread_handler(ClientData args) {
    /* Set sigmask before starting any threads so it is inherited */
    sigset_t unblocksigs;
    struct sigaction sa;
    int err;

    sa.sa_handler = signal_handler;
    if (sigemptyset(&sa.sa_mask) < 0) goto error;
    sa.sa_flags = SA_RESTART;
    if (sigaction(SIGINT, &sa, 0) < 0) goto error;
    if (sigaction(SIGTERM, &sa, 0) < 0) goto error;

    if (sigemptyset(&unblocksigs) < 0) goto error;
    if (sigaddset(&unblocksigs, SIGINT) < 0) goto error;
    if (sigaddset(&unblocksigs, SIGTERM) < 0) goto error;
    if ((err = pthread_sigmask(SIG_UNBLOCK, &unblocksigs, NULL)) != 0)
	goto error2;

    for (;;) {
        if (pause() < 0 && errno != EINTR)
            break;
    }
    return;

error:
    err = errno;
error2:
    fprintf(stderr, "signal thread error %d\n", err);
    exit(1);
}

sigset_t old_blocksigs;

static void restore_sigmask(void)
{
    pthread_sigmask(SIG_SETMASK, &old_blocksigs, NULL);
}
#endif

static int xsdb_abort_check(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc,
    Tcl_Obj *const objv[])
{
    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    if (last_abort != 0) {
	Tcl_SetResult(interp, "abort", TCL_STATIC);
        return TCL_ERROR;
    }
    return TCL_OK;
}

static int xsdb_abort_clear(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc,
    Tcl_Obj *const objv[])
{
    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    last_abort = 0;
    return TCL_OK;
}

static int xsdb_version(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc,
    Tcl_Obj *const objv[])
{
    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    Tcl_ResetResult(interp);
    Tcl_SetResult(interp, RDI_VERSION, TCL_STATIC);
    return TCL_OK;
}

int main(int argc, char **argv)
{
    Tcl_Interp *interp;
    Tcl_Obj *argvPtr;
    int ind;
    int tty = isatty(0) ? 1 : 0;
    int quiet = 0;
    const char *eval = NULL;
    Tcl_Channel inChannel, outChannel;
    Tcl_DString appName;
    Tcl_Obj *script = NULL;
    const char *scriptName = NULL;
    const char *encodingName = NULL;
    Tcl_Obj *commandPtr = NULL;
    PromptType prompt = PROMPT_START;
    int exitCode = 0;
    int interactive = 0;
    char *home;

    /* Remember main thread id. */
    main_threadid = Tcl_GetCurrentThread();

    /* Parse arguments */
    for (ind = 1; ind < argc; ind++) {
        char * s = argv[ind];
        if (strcmp(s, "-interactive") == 0) {
            interactive = 1;
        } else if (strcmp(s, "-help") == 0 || strcmp(s, "-h") == 0) {
            display_help();
            exit(0);
        } else if (strcmp(s, "-quiet") == 0 || strcmp(s, "-q") == 0) {
            quiet = 1;
        } else if (ind + 1 < argc && (strcmp(s, "-eval") == 0 || strcmp(s, "-e") == 0)) {
            eval = argv[++ind];
            tty = 0;
        } else if (ind + 2 < argc && strcmp(argv[ind], "-encoding") == 0 && argv[ind+2][0] != '-') {
            encodingName = argv[ind + 1];
            ind += 2;
            break;
        } else if (strcmp(s, "-no-ini") == 0 || strcmp(s, "-n") == 0) {
            loadIni = 0;
        } else {
            if (*s != '-') break;
            fprintf(stderr, "error: illegal option '%s'\n", s);
            display_help();
            Tcl_Exit(1);
        }
    }

    if (ind < argc && argv[ind][0] != '-') {
	scriptName = argv[ind];
	ind++;
	tty = 0;
    }

    tty |= interactive;
    if (tty) {
#if defined(_WIN32)
	SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
#else
	/* Block signal to all threads by default.  Signals will be unblocked for
	 * a specific thread below. */
	sigset_t blocksigs;

	sigemptyset(&blocksigs);
	sigaddset(&blocksigs, SIGINT);
	sigaddset(&blocksigs, SIGTERM);
	pthread_sigmask(SIG_BLOCK, &blocksigs, &old_blocksigs);
	pthread_atfork(NULL, NULL, restore_sigmask);
#endif

	if (!quiet) {
            display_banner();
	}
    }

    Tcl_FindExecutable(argv[0]);

    interp = Tcl_CreateInterp();
    if (Tcl_Init(interp) != TCL_OK) {
	Tcl_AddErrorInfo(interp, "");
	ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_ERROR, "Initialization Failed:\n%s",
			  Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
	exit(1);
    }

    /*
     * Ensure that we are getting the matching version of Tcl. This is really
     * only an issue when Tk is loaded dynamically.
     */

    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
	abort();
    }

    Tcl_Preserve((ClientData) interp);

#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    if (scriptName == NULL) {
	Tcl_ExternalToUtfDString(NULL, argv[0], -1, &appName);
    } else {
	Tcl_ExternalToUtfDString(NULL, scriptName, -1, &appName);
	script = Tcl_NewStringObj(Tcl_DStringValue(&appName), -1);
	Tcl_IncrRefCount(script);
    }
    Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&appName), TCL_GLOBAL_ONLY);
    Tcl_DStringFree(&appName);

    Tcl_SetVar2Ex(interp, "argc", NULL, Tcl_NewIntObj(argc - ind), TCL_GLOBAL_ONLY);

    argvPtr = Tcl_NewListObj(0, NULL);
    while (ind < argc) {
	Tcl_DString ds;
	Tcl_ExternalToUtfDString(NULL, argv[ind++], -1, &ds);
	Tcl_ListObjAppendElement(NULL, argvPtr, Tcl_NewStringObj(
		Tcl_DStringValue(&ds), Tcl_DStringLength(&ds)));
	Tcl_DStringFree(&ds);
    }
    Tcl_SetVar2Ex(interp, "argv", NULL, argvPtr, TCL_GLOBAL_ONLY);

    /*
     * Set the "tcl_interactive" variable.
     */

    Tcl_SetVar(interp, "tcl_interactive", tty ? "1" : "0", TCL_GLOBAL_ONLY);

    if (tty) {
#if !defined(_WIN32)
	Tcl_ThreadId signal_thread;
	if (Tcl_CreateThread(&signal_thread, signal_thread_handler, (ClientData) NULL,
			     TCL_THREAD_STACK_DEFAULT, TCL_THREAD_NOFLAGS) != TCL_OK) {
	    Tcl_AddErrorInfo(interp, "");
	    ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_ERROR,"Cannot Create Signal Thread:\n%s",
			      Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
	    Tcl_DeleteInterp(interp);
	    Tcl_Exit(1);
	    return -1;
	}
#endif
	Tcl_CreateObjCommand(interp, "::xsdb::abort_check", xsdb_abort_check, NULL, NULL);
	Tcl_CreateObjCommand(interp, "::xsdb::abort_clear", xsdb_abort_clear, NULL, NULL);
    }
    Tcl_CreateObjCommand(interp, "::xsdb::get_version", xsdb_version, NULL, NULL);

    Tcl_XSDB_Init(interp);

    if (Tcl_PkgRequire(interp, "xsdb", NULL, 0) == NULL) {
	Tcl_AddErrorInfo(interp, "");
	ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_ERROR,"package require xsdb FAILED:\n%s",
			  Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
	exitCode = 1;
	goto done;
    }

    if (Tcl_Import(interp, NULL, "::xsdb::*", TCL_LEAVE_ERR_MSG) != TCL_OK) {
	Tcl_AddErrorInfo(interp, "");
	ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_ERROR,"namespace import ::xsdb::* FAILED:\n%s",
			  Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
	exitCode = 1;
	goto done;
    }

#if defined(_WIN32)
    home = getenv("USERPROFILE");
#else
    home = getenv("HOME");
#endif

    if( (home != NULL) && (*home != '\0') ){
        FILE *file;
        char xilenv[PATH_MAX];
        strncpy(xilenv, home, sizeof xilenv);
        xilenv[sizeof xilenv - 1] = '\0';
        strncat(xilenv, "/.xsdbrc", sizeof xilenv - 1);
        if ((file = fopen(xilenv, "r")) != NULL) {
            fclose(file);
            if (Tcl_EvalFile(interp, xilenv) == TCL_ERROR) {
                ComMsgMgr_SendMsg(ComMsgMgr_MSGTYPE_ERROR,"Source xsdbrc script :%s FAILED\n", xilenv);
            }
        }
    }
    Tcl_SetVar(interp, "tcl_rcFileName", "xsdb.ini", TCL_GLOBAL_ONLY);

    /*
     * Invoke the script specified on the command line, if any. Must fetch it
     * again, as the appInitProc might have reset it.
     */

    if (script != NULL) {
	int code;
	if (eval != NULL) {
	    fprintf(stderr, "warning: -e option ignored when startup script is given\n");
	}
	code = Tcl_FSEvalFileEx(interp, script, encodingName);
	if (code != TCL_OK) {
	    Tcl_Channel errChannel = Tcl_GetStdChannel(TCL_STDERR);
	    if (errChannel) {
		Tcl_Obj *options = Tcl_GetReturnOptions(interp, code);
		Tcl_Obj *keyPtr, *valuePtr;

		keyPtr = Tcl_NewStringObj("-errorinfo", -1);
		Tcl_IncrRefCount(keyPtr);
		Tcl_DictObjGet(NULL, options, keyPtr, &valuePtr);
		Tcl_DecrRefCount(keyPtr);

		if (valuePtr) {
		    Tcl_WriteObj(errChannel, valuePtr);
		}
		Tcl_WriteChars(errChannel, "\n", 1);
	    }
	    exitCode = 1;
	}
	if (!tty) goto done;
    }
    if (eval != NULL) {
	int code;
	Tcl_DString ds;
	Tcl_ExternalToUtfDString(NULL, eval, -1, &ds);
	code = Tcl_EvalEx(interp, Tcl_DStringValue(&ds), Tcl_DStringLength(&ds), 0);
	Tcl_DStringFree(&ds);
	if (code != TCL_OK) {
	    Tcl_Channel errChannel = Tcl_GetStdChannel(TCL_STDERR);
	    if (errChannel) {
		Tcl_Obj *options = Tcl_GetReturnOptions(interp, code);
		Tcl_Obj *keyPtr, *valuePtr;

		keyPtr = Tcl_NewStringObj("-errorinfo", -1);
		Tcl_IncrRefCount(keyPtr);
		Tcl_DictObjGet(NULL, options, keyPtr, &valuePtr);
		Tcl_DecrRefCount(keyPtr);

		if (valuePtr) {
		    Tcl_WriteObj(errChannel, valuePtr);
		}
		Tcl_WriteChars(errChannel, "\n", 1);
	    }
	    exitCode = 1;
	}
	if (!tty) goto done;
    }

    if (loadIni) {
	/*
	 * Evaluate the .rc file, if one has been specified.
	 */
        Tcl_SourceRCFile(interp);
    }

    /*
     * Process commands from stdin until there is an end-of-file. Note that we
     * need to fetch the standard channels again after every eval, since they
     * might have been changed.
     */

    commandPtr = Tcl_NewObj();
    Tcl_IncrRefCount(commandPtr);

    /*
     * Get a new value for tty if anyone writes to ::tcl_interactive
     */

    Tcl_LinkVar(interp, "tcl_interactive", (char *) &tty, TCL_LINK_BOOLEAN);
    inChannel = Tcl_GetStdChannel(TCL_STDIN);
    while ((inChannel != (Tcl_Channel) NULL) && !Tcl_InterpDeleted(interp)) {
	char *cmd;
	int code;
	int length;

	{
	    if (tty) {
		Prompt(interp, &prompt);
		if (Tcl_InterpDeleted(interp)) {
		    break;
		}
		if (Tcl_LimitExceeded(interp)) {
		    break;
		}
		inChannel = Tcl_GetStdChannel(TCL_STDIN);
		if (inChannel == (Tcl_Channel) NULL) {
		    break;
		}
		Tcl_SetChannelOption(interp, inChannel, "-blocking", "0");
		Tcl_CreateChannelHandler(inChannel, TCL_READABLE, StdinProc, NULL);
		Tcl_DoOneEvent(TCL_ALL_EVENTS);
	    }
	    if (Tcl_IsShared(commandPtr)) {
		Tcl_DecrRefCount(commandPtr);
		commandPtr = Tcl_DuplicateObj(commandPtr);
		Tcl_IncrRefCount(commandPtr);
	    }
	    length = Tcl_GetsObj(inChannel, commandPtr);
	    if (tty) {
#if defined(_WIN32)
		if (length < 0 && Tcl_Eof(inChannel)) {
		    /* Windows seems to report EOF when ctrl-c is pressed, retry read to
		     * make sure if it is really EOF */
		    length = Tcl_GetsObj(inChannel, commandPtr);
		}
#endif
		Tcl_DeleteChannelHandler(inChannel, StdinProc, NULL);
		Tcl_SetChannelOption(interp, inChannel, "-blocking", "1");
	    }
	    if (length < 0) {
		if (Tcl_InputBlocked(inChannel)) {
		    /*
		     * This can only happen if stdin has been set to
		     * non-blocking.  In that case, cycle back and try again.
		     * This sets up a tight polling loop (since we have no
		     * event loop running). If this causes bad CPU hogging,
		     * we might try toggling the blocking on stdin instead.
		     */

		    continue;
		}

		/*
		 * Either EOF, or an error on stdin; we're done
		 */

		break;
	    }

	    /*
	     * Add the newline removed by Tcl_GetsObj back to the string.
	     * Have to add it back before testing completeness, because
	     * it can make a difference.  [Bug 1775878].
	     */

	    if (Tcl_IsShared(commandPtr)) {
		Tcl_DecrRefCount(commandPtr);
		commandPtr = Tcl_DuplicateObj(commandPtr);
		Tcl_IncrRefCount(commandPtr);
	    }
	    Tcl_AppendToObj(commandPtr, "\n", 1);
	    cmd = Tcl_GetStringFromObj(commandPtr, &length);
	    if (!Tcl_CommandComplete(cmd)) {
		prompt = PROMPT_CONTINUE;
		continue;
	    }

	    last_abort = 0;
	    prompt = PROMPT_START;
	    /*
	     * The final newline is syntactically redundant, and causes
	     * some error messages troubles deeper in, so lop it back off.
	     */
	    Tcl_GetStringFromObj(commandPtr, &length);
	    Tcl_SetObjLength(commandPtr, --length);
	    code = Tcl_RecordAndEvalObj(interp, commandPtr, TCL_EVAL_GLOBAL);
	    inChannel = Tcl_GetStdChannel(TCL_STDIN);
	    Tcl_DecrRefCount(commandPtr);
	    commandPtr = Tcl_NewObj();
	    Tcl_IncrRefCount(commandPtr);
	    if (code != TCL_OK) {
		Tcl_Channel errChannel = Tcl_GetStdChannel(TCL_STDERR);
		if (errChannel) {
		    Tcl_WriteObj(errChannel, Tcl_GetObjResult(interp));
		    Tcl_WriteChars(errChannel, "\n", 1);
		}
	    } else if (tty) {
		Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);
		Tcl_IncrRefCount(resultPtr);
		Tcl_GetStringFromObj(resultPtr, &length);
		outChannel = Tcl_GetStdChannel(TCL_STDOUT);
		if ((length > 0) && outChannel) {
		    Tcl_WriteObj(outChannel, resultPtr);
		    Tcl_WriteChars(outChannel, "\n", 1);
		}
		Tcl_DecrRefCount(resultPtr);
	    }
	}
    }

  done:
    if (commandPtr != NULL) {
	Tcl_DecrRefCount(commandPtr);
    }

    /*
     * Rather than calling exit, invoke the "exit" command so that users can
     * replace "exit" with some other command to do additional cleanup on
     * exit. The Tcl_EvalObjEx call should never return.
     */

    if (!Tcl_InterpDeleted(interp)) {
	if (!Tcl_LimitExceeded(interp)) {
	    Tcl_Obj *cmd = Tcl_ObjPrintf("exit %d", exitCode);
	    Tcl_IncrRefCount(cmd);
	    Tcl_EvalObjEx(interp, cmd, TCL_EVAL_GLOBAL);
	    Tcl_DecrRefCount(cmd);
	}

	/*
	 * If Tcl_EvalObjEx returns, trying to eval [exit], something unusual
	 * is happening. Maybe interp has been deleted; maybe [exit] was
	 * redefined, maybe we've blown up because of an exceeded limit. We
	 * still want to cleanup and exit.
	 */

	if (!Tcl_InterpDeleted(interp)) {
	    Tcl_DeleteInterp(interp);
	}
    }
    if (script != NULL) {
	Tcl_DecrRefCount(script);
    }

    /*
     * If we get here, the master interp has been deleted. Allow its
     * destruction with the last matching Tcl_Release.
     */

    Tcl_Release((ClientData) interp);
    Tcl_Exit(exitCode);
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This function is invoked by the event dispatcher whenever standard
 *	input becomes readable. It grabs the next line of input characters,
 *	adds them to a command being assembled, and executes the command if
 *	it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(
    ClientData clientData,	/* Not used. */
    int mask)			/* Not used. */
{
    /* Do nothing - calling this function will exit the call to Tcl_DoOneEvent */
}

/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script to issue the
 *	prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(
    Tcl_Interp *interp,		/* Interpreter to use for prompting. */
    PromptType *promptPtr)	/* Points to type of prompt to print. Filled
				 * with PROMPT_NONE after a prompt is
				 * printed. */
{
    Tcl_Obj *promptCmdPtr;
    int code;
    Tcl_Channel outChannel, errChannel;

    if (*promptPtr == PROMPT_NONE) {
	return;
    }

    promptCmdPtr = Tcl_GetVar2Ex(interp,
	    ((*promptPtr == PROMPT_CONTINUE) ? "tcl_prompt2" : "tcl_prompt1"),
	    NULL, TCL_GLOBAL_ONLY);

    if (Tcl_InterpDeleted(interp)) {
	return;
    }
    if (promptCmdPtr == NULL) {
    defaultPrompt:
	outChannel = Tcl_GetStdChannel(TCL_STDOUT);
	if ((*promptPtr == PROMPT_START)
		&& (outChannel != (Tcl_Channel) NULL)) {
	    Tcl_WriteChars(outChannel, DEFAULT_PRIMARY_PROMPT,
		    strlen(DEFAULT_PRIMARY_PROMPT));
	}
    } else {
	code = Tcl_EvalObjEx(interp, promptCmdPtr, TCL_EVAL_GLOBAL);
	if (code != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		    "\n    (script that generates prompt)");
	    errChannel = Tcl_GetStdChannel(TCL_STDERR);
	    if (errChannel != (Tcl_Channel) NULL) {
		Tcl_WriteObj(errChannel, Tcl_GetObjResult(interp));
		Tcl_WriteChars(errChannel, "\n", 1);
	    }
	    goto defaultPrompt;
	}
    }

    outChannel = Tcl_GetStdChannel(TCL_STDOUT);
    if (outChannel != (Tcl_Channel) NULL) {
	Tcl_Flush(outChannel);
    }
    *promptPtr = PROMPT_NONE;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
