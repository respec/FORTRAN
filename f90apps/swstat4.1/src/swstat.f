C
C
C
      PROGRAM      SWSTAT
C
C     + + + PURPOSE + + +
C     SWSTAT is an interactive program designed to assist the user with
C     the various tasks involved in statistical analyses of daily time
C     series data.  These analyses include duration, frequency, basic
C     statistics and annual n-day high or low events.  All analyses
C     assume input time series are on a WDM file.  The programs ANNIE
C     and IOWDM can be used to put data and manage data in a WDM file.
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP, SCLU, RESP, IND, IGR, WDMSFL, WDMFG,
     &             RETCOD,BLDOPT
      CHARACTER*8  PTHNAM, LGNAM
      CHARACTER*11 APNAM
      CHARACTER*64 FNAME,VERSN
C
C     + + + EXTERNALS + + +
      EXTERNAL     ANINIZ, ANPRGT, ANCLOS, WDFLCL, PROSTT
      EXTERNAL     QRESP, PRNTXT, PRWFLE, ZANSET
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (//, ' *** ERROR',
     $         /, ' *** ERROR',
     $         /, ' *** ERROR  Problem closing wdm data file',
     $         /, ' *** ERROR          Return code =', I6,
     $         /, ' *** ERROR',
     $         /, ' *** ERROR' )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize user environment
C     version info for unix what
C     names of application, log file, and message file
      INCLUDE 'fstmes.inc'
C
C     open message file and log file
      CALL ANINIZ (MESSFL,FNAME,LGNAM)
C
C     set application name
      CALL ZANSET (APNAM)
C
C     message file screen cluster used in this routine
      SCLU  = 149
      WDMSFL= 0
C
C     check if graphics available (value of 1 returned if yes)
      IGR = 0
      IND= 16
      CALL ANPRGT (IND,
     O             IGR)
C
 10   CONTINUE
C       do opening screen
 20     CONTINUE
C         loop back here if user tries to do analysis w/o opening WDM
          WDMFG= 0
C         get user's menu choice (1-File, 2-Analyze, 3-What, 4-Query, 5-Return)
          SGRP = 2
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
          IF (RESP.EQ.2 .AND. WDMSFL.EQ.0) THEN
C           can't do analysis without first opening a WDM file
            SGRP = 3
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            WDMFG = 1
          END IF
        IF (WDMFG.EQ.1) GO TO 20
C
        GO TO (100,200,300,400,500),RESP
C
 100    CONTINUE
C         file management
          BLDOPT = 0
          PTHNAM = ' '
          CALL PRWFLE (MESSFL, BLDOPT, PTHNAM,
     M                 WDMSFL)
          GO TO 900
C
 200    CONTINUE
C         statistics
          CALL PROSTT (MESSFL,WDMSFL,IGR,APNAM)
          GO TO 900
C
 300    CONTINUE
C         give info. on what this program's about
          SGRP = 4
          CALL PRNTXT (MESSFL,SCLU,SGRP)
          GO TO 900
C
 400    CONTINUE
C         query for support
          SGRP = 5
          CALL PRNTXT (MESSFL,SCLU,SGRP)
          GO TO 900
C
 500    CONTINUE
C         done, do nothing
          GO TO 900
C
 900    CONTINUE
C
      IF (RESP .NE. 5) GO TO 10
C
C     close wdm file if it is still open
      RETCOD = 0
      IF (WDMSFL .GT. 0) THEN
        CALL WDFLCL ( WDMSFL, RETCOD )
        WDMSFL = 0
      END IF
C
C     close message, scratch and log files
      CALL ANCLOS (MESSFL)
C
      IF (RETCOD .NE. 0) THEN
C       problem closing wdm file
        WRITE(99,2000) RETCOD
        WRITE (*,2000) RETCOD
      END IF
C
      STOP
      END
C
C
C
      SUBROUTINE   PROSTT
     I                    (MESSFL,WDMFL,IGR,APNAM)
C
C     + + + PURPOSE + + +
C     This routine calls all the statistical analysis routines.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,WDMFL,IGR
      CHARACTER*11  APNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number for WDM file
C              It will be 9999 if WDM file not open.
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     APNAM  - program name and version
C
C     + + + PARAMETERS + + +
      INTEGER    DSNBMX
      PARAMETER (DSNBMX = 300)
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER    OPT,SCLU,SGRP,FE,DSNCNT,DSNBUF(DSNBMX),I0,I1
      CHARACTER*8  PTHNAM(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP,  GPOPEN, GPCLOS
      EXTERNAL   PRBAST, PROFDR, TSCMPR, PRA193
      EXTERNAL   NDHILO, PKNTAU, PRODHY
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C     message file clusters used
      SCLU = 150
C     start with empty data set buffer
      DSNCNT = 0
      I0 = 0
      CALL ZIPI (DSNBMX, I0, DSNBUF)
C
      IF (IGR.EQ.1) THEN
C       open GKS and error file
        CALL GPOPEN (FE)
      END IF
C
 10   CONTINUE
C       Opt: 1-RETURN,2-Basic,3-Duration,4-Compare,5-Frequency,
C            6-N-day,7-Trend,8-Hydrograph
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,OPT)
C
        GO TO (50,100,200,300,400,500,600,700) OPT
C
 50     CONTINUE
C         Return to calling routine
          GO TO 900
C
 100    CONTINUE
C         basic stats for timeseries dataset
          CALL PRBAST (MESSFL,WDMFL,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 200    CONTINUE
C         flow duration analysis
          CALL PROFDR (MESSFL,WDMFL,IGR,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 300    CONTINUE
C         compare 2 timeseries
          PTHNAM(1) = 'S'
          CALL TSCMPR (MESSFL,WDMFL,IGR,I1,PTHNAM,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 400    CONTINUE
C         regular Log Pearson Type III
          CALL PRA193 (MESSFL,WDMFL,IGR,APNAM,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 500    CONTINUE
C         N-day high and low annual statistics
          CALL NDHILO (MESSFL,WDMFL,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 600    CONTINUE
C         Kendall Tau test for trends
          CALL PKNTAU (MESSFL,WDMFL,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 700    CONTINUE
C         Duration hydrograph
          CALL PRODHY (MESSFL, WDMFL, IGR,
     M                 DSNCNT,DSNBMX,DSNBUF)
          GO TO 900
C
 900    CONTINUE
      IF (OPT.NE.1) GO TO 10
C
      IF (IGR.EQ.1) THEN
C       close GKS
Ckmf    gpclos closes unit 99 and then calls gclks.  gclks 
Ckmf    (as well as later code in this program) may still
Ckmf    want to write to 99.
Ckmf    CALL GPCLOS (FE)
        CALL GCLKS
      END IF
C
      RETURN
      END
