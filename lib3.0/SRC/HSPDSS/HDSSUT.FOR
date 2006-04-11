C
C
C
      SUBROUTINE   CHKDSS
     I                    (MESSU,MSGFL,
     M                     ECOUNT)
C
C     + + + PURPOSE + + +
C     Checks each open dss file for correct version
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,MSGFL,ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     ECOUNT - count of errors
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,IRET,I,I4
      CHARACTER*4 CITEM,CFVERS
C
C     + + + EXTERNALS + + +
      EXTERNAL ZINQIR,OMSTC,OMSTI,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CITEM/'FVER'/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 209
      I4=     4
C
      DO 10 I= 1, MAXDSS
        IF (DSSOPN(I) .NE. 0) THEN
C         file is open - check version
          CALL ZINQIR
     M                (IFLTAB(1,I),
     I                 CITEM,
     O                 CFVERS,IRET)
C
          IF (CFVERS(1:1) .NE. '6') THEN
C           error - incorrect DSS version number
            CALL OMSTI (I)
            CALL OMSTC (I4,CFVERS)
            SGRP= 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIM2CH
     I                    (SDATE,
     O                     CDATE,CTIME,RETCOD)
C
C     + + + PURPOSE + + +
C     converts an HSPF-style date to a DSS-style character
C     date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      SDATE(5),RETCOD
      CHARACTER*4  CTIME
      CHARACTER*20 CDATE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LDATE(5),I,N
      CHARACTER*1 BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER M2IHM
C
C     + + + EXTERNALS + + +
      EXTERNAL COPYI,ZIPC,YMDDAT,M2IHM
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I= 5
      CALL COPYI (I,SDATE,
     O            LDATE)
C
      I= 20
      CALL ZIPC (I,BLNK,
     O           CDATE)
C
      I= 4
      CALL YMDDAT (LDATE(1),LDATE(2),LDATE(3),I,
     O             CDATE,N,RETCOD)
C
      IF (RETCOD .EQ. 0) THEN
C       no error in year/month/day of date
        N= 60*(LDATE(4)- 1)+ LDATE(5)
        I= M2IHM (N,
     O            CTIME)
        IF (I .LT. 0) THEN
C         error in hour/minute
          RETCOD= I
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE CH2TIM
     I                  (CDATE,CTIME,
     O                   HDATE,RETCOD)
C
C     + + + PURPOSE + + +
C     converts a DSS-style character date to an HSPF-style
C     date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      HDATE(5),RETCOD
      CHARACTER*4  CTIME
      CHARACTER*20 CDATE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER LDATE(5),I
C
C     + + + FUNCTIONS + + +
      INTEGER IHM2M
C
C     + + + EXTERNALS + + +
      EXTERNAL COPYI,DATYMD,IHM2M
C
C     + + + END SPECIFICATIONS + + +
C
C
      CALL DATYMD (CDATE,
     O             LDATE(1),LDATE(2),LDATE(3),RETCOD)
C
      IF (RETCOD .EQ. 0) THEN
C       no error in cdate
        I= IHM2M (CTIME)
        IF (I .GT. 0) THEN
C         ctime is valid - note that ctime of '0000' is not valid for
C         HSPF, and cannot be created by tim2ch or addcdt
          LDATE(4)= (I-1)/60+ 1
          LDATE(5)= I- 60*(LDATE(4)- 1)
C
          I= 5
          CALL COPYI (I,LDATE,
     O                HDATE)
        ELSE
C         error in ctime
          RETCOD= I
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDCDT
     I                    (DELT,NVALS,
     M                     CDATE,CTIME)
C
C     + + + PURPOSE + + +
C     Increments a DSS-style character date and time by a given number
C     of minutes.  Increment can be forward or backward (positive or
C     negative).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      DELT,NVALS
      CHARACTER*4  CTIME
      CHARACTER*20 CDATE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SJUL,SMIN,EJUL,EMIN,I,I0
      CHARACTER*1 BLANK
C
C     + + + FUNCTIONS + + +
      INTEGER IHM2M,INCTIM,M2IHM
C
C     + + + EXTERNALS + + +
      EXTERNAL DATJUL,IHM2M,INCTIM,ZIPC,JULDAT,M2IHM
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0/0/
      DATA BLANK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
C     convert to julian
      CALL DATJUL (CDATE,
     O             SJUL,I)
      SMIN= IHM2M (CTIME)
C
C     increment julian
      I= INCTIM (DELT,I0,NVALS,SJUL,SMIN,
     O                EJUL,EMIN)
C
C     convert back to character
      I= 20
      CALL ZIPC (I,BLANK,
     O           CDATE)
      I= 4
      CALL JULDAT (EJUL,I,
     O             CDATE,I)
      I= M2IHM (EMIN,
     O          CTIME)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSSCLO
C
C     + + + PURPOSE + + +
C     Closes all open DSS files.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
C
C     + + + EXTERNALS + + +
      EXTERNAL ZCLOSE
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, MAXDSS
        IF (DSSOPN(I) .NE. 0) THEN
C         file must be closed properly
          CALL ZCLOSE (IFLTAB(1,I))
        END IF
 10   CONTINUE
C
      RETURN
      END
