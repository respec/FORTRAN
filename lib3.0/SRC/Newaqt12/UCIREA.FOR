C
C
C
      SUBROUTINE   UCIREA
     I                   (MESSFL,UCIFL,PTHNAM,WDMSFL,
     M                    DPRAQF,EMFG,FILES,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Read a UCI file from disk and perform preliminary checks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,UCIFL,RETCOD,DPRAQF,EMFG,WDMSFL,FILES(15)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     UCIFL  - Fortran unit number for UCI file
C     RETCOD - return code, -1 - not a HSPF UCI file, -2 problem with parms
C     DPRAQF - Subjective value for deep aquifer recharge (hspexp)
C     PTHNAM - character string of path of options selected to get here
C     EMFG   - english/metric units flag (1-eng,2-metric)
C     WDMSFL - wdm file unit number
C     FILES  - array of hspf file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,SCLU,LPTH,I1,I0,ECOUNT
      REAL         DEPSUM
      CHARACTER*1  CHSTR1(20)
      CHARACTER*48 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    PRNTXT,INTERP,HDMEST
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,GETDEP,FILSET
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G)'
      ELSE
C       no pathname
        WNDNAM= 'Get (G)'
      END IF
      CALL ZWNSET (WNDNAM)
C
C     reading uci file message
      SCLU = 201
      SGRP = 51
      CALL HDMEST (I0,MESSFL,SCLU,SGRP)
C
C     set up uci in memory and associated files
      CALL FILSET (MESSFL,UCIFL,WDMSFL,
     M             FILES,
     O             ECOUNT,RETCOD)
C
      IF (ECOUNT.EQ.0 .AND. RETCOD.EQ.0) THEN
C       a clean run data set was found
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G)'
        ELSE
C         no pathname
          WNDNAM= 'Get (G)'
        END IF
        CALL ZWNSET (WNDNAM)
C       preprocessing uci file message
        SCLU = 201
        SGRP = 58
        CALL HDMEST (I0,MESSFL,SCLU,SGRP)
C
C       interpret it but dont make instruction files
        CALL INTERP (SCLU,I0,
     M               FILES,
     O               EMFG,RETCOD)
C
        IF (RETCOD.EQ.0) THEN
C         check DEEPFR values in perlnd for hspexp subjective paramater
          CALL GETDEP (DEPSUM)
          IF (DEPSUM.GT.0.0) THEN
C           there is deep aquifer recharge
            DPRAQF= 1
          END IF
        END IF
      END IF
C
      IF (RETCOD .NE. 0 .OR. ECOUNT .NE. 0) THEN
C       let user know there was a problem
        SCLU= 55
        SGRP= 16
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G) Problem'
        ELSE
C         no pathname
          WNDNAM= 'Get (G) Problem'
        END IF
        CALL ZWNSET (WNDNAM)
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   AQVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     newaqt library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
