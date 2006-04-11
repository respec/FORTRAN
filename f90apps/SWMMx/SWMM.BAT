@ECHO OFF
CLS
GOTO START
REM  ******************** \SWMM_430\SWMM.BAT *********************
REM  *       Author: D.W. Disney, Senior Member Technical Staff  *
REM  *               Computer Sciences Corporation               *
REM  *         Date: Thursday, 12 May 1994.  Time: 12:00:18.     *
REM  *  Environment: Microway 486 B2T/50, DOS 5.01               *
REM  * Installation: Computer Sciences Corporation               *
REM  *               U.S. Environmental Protection Agency        *
REM  *               Athens ERL - Athens, Georgia                *
REM  *     Function: To execute SWMM model for a given input     *
REM  *               data file set.  Can be executed in batch or *
REM  *               interactive mode.                           *
REM  *     Calling                                               *
REM  *     Sequence: SWMM                                        *
REM  *        Input: Command line: None                          *
REM  *                      Other: SWMM run time prompts         *
REM  *************************************************************
:START
REM
IF EXIST D:\SWMM\XSWMMX.EXE GOTO CHKFILES
ECHO #
ECHO # ----------------------------------------------------------------------
ECHO #
ECHO ##  ERROR:  BAD OR MISSING XSWMMX.EXE FILE
ECHO #
ECHO #           The SWMM executable task image file XSWMMX.EXE is
ECHO #           not located in the current default DOS sub-directory.
ECHO #
ECHO #           Install and/or copy this file and all of its support
ECHO #           files to the current sub-directory or call CEAM for
ECHO #           assistance.  Refer to file READ.ME for phone number.
ECHO #
ECHO #   Press the "<Enter>" key to return to the DOS command prompt.
PAUSE >NUL
GOTO STOP
:CHKFILES
IF "%1"=="" GOTO RUNSWMM
IF EXIST %1 GOTO RUNSWMM
ECHO.
ECHO #
ECHO # ----------------------------------------------------------------------
ECHO #
ECHO ##  ERROR:  SPECIFIED FILE NAME NOT FOUND
ECHO #           FILE NAME ENTERED WAS %1
ECHO #
ECHO #           The file name %1, as entered on the command line,
ECHO #           does not exist on the specified or default (current)
ECHO #           drive/sub-directory
ECHO #
CD
ECHO #
ECHO #           Use %0 command again with different input file name.
ECHO #
ECHO #   Press the "<Enter>" key to return to the DOS command prompt.
PAUSE >NUL
GOTO STOP
REM
:RUNSWMM
TYPE D:\SWMM\SWMM.SCR
PAUSE >NUL
CLS
ECHO #
ECHO #
ECHO ##  Loading SWMM task image file...
REM
REM SET \RMFORT.ERR=\SWMM\RMFORT.ERR
SET LFDIR=D:\SWMM
D:\SWMM\XSWMMX %1 %2
REM
REM SET \RMFORT.ERR=
SET LFDIR=
ECHO #
ECHO #
ECHO ##  SWMM program execution complete.
ECHO #
ECHO #   Press the "<Enter>" key to continue.
PAUSE >NUL
REM
:STOP
REM
