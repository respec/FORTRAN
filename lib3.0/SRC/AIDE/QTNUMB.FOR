C     qtnumb.f 2.1 9/4/91
C
C
C
      SUBROUTINE   QRSPUI
     I                    (UMESFL,SCLU,SGRP,IMIN,IMAX,IDEF,
     M                     IVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses are supplied by the user to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are integer
C     numbers and the valid responses are checked against a minimum and
C     maximum that are supplied by the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,IMIN,IMAX,IDEF,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     IMIN   - user defined minimum value
C     IMAX   - user defined maximum value
C     IDEF   - user defined default value
C     IVAL   - users response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      REAL        RDUM(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRESPI
C
C     + + + END SPECFICATIONS + + +
C
      I1      = 1
      IDUM(1) = 0
      IDUM1(1)= 1
      RDUM(1) = 0.0
      DDUM(1) = 0.0
      BLNK(1) = ' '
C
C     set min/max/def to user defined values
      CALL QSCSET (I1,I1,I1,I1,I1,IMIN,IMAX,IDEF,
     I             RDUM,RDUM,RDUM,DDUM,DDUM,DDUM,
     I             IDUM,IDUM,IDUM,IDUM1,BLNK)
C
      CALL QRESPI (UMESFL,SCLU,SGRP,
     M             IVAL)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPI
     I                   (UMESFL,SCLU,SGRP,
     M                    IVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses in the message file are used to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are integer
C     numbers and the valid responses are checked against a minimum and
C     maximum that are on the message file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     IVAL   - users response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,NODAT,RETCOD,IRET,SGLCHR,LFLEN
      CHARACTER*1 NONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, CRINTX, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CRINTX, STRFND, WMSGTP, FLSETI
      EXTERNAL    ZEDT0M, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR= 0
      I4    = 4
      NODAT = -999
C
 20   CONTINUE
C       get needed info from WDM file
        CALL WMSGTP (UMESFL,SCLU,SGRP,
     O               I,RETCOD)
C       set field value if needed
        CALL FLSETI (IDEF(1),FLEN(1),
     M               IVAL,
     O               ZMNTX1(SCOL(1),FLIN(1)))
        CALL ZEDT0M (SGLCHR,
     O               IRET)
C       go back on oops
      IF (IRET.EQ.-1) GO TO 20
C
      IF (RETCOD.EQ.0) THEN
C       all ok
        I= SCOL(1)
        J= SCOL(1)+ FLEN(1)- 1
        CALL ZLJUST (ZMNTXT(FLIN(1))(I:J))
        LFLEN = LENSTR(FLEN(1),ZMNTX1(I,FLIN(1)))
        IF (LFLEN.GT.0) THEN
C         string to process
          IF (STRFND(FLEN(1),ZMNTX1(I,FLIN(1)),I4,NONE).EQ.0) THEN
C           convert value in text to variable
            IVAL= CRINTX(LFLEN,
     I                   ZMNTX1(I,FLIN(1)))
          ELSE
C           current value is 'none', set to -999
            IVAL= NODAT
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPUR
     I                    (UMESFL,SCLU,SGRP,RMIN,RMAX,RDEF,
     M                     RVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses are supplied by the user to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are integer
C     numbers and the valid responses are checked against a minimum and
C     maximum that are supplied by the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP
      REAL      RMIN,RMAX,RDEF,RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     RMIN   - user defined minimum value
C     RMAX   - user defined maximum value
C     RDEF   - user defined default value
C     RVAL   - users response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRESPR
C
C     + + + END SPECFICATIONS + + +
C
      I1      = 1
      IDUM1(1)= 1
      IDUM(1) = 0
      DDUM(1) = 0.0
      BLNK(1) = ' '
C
C     set min/max/def to user defined values
      CALL QSCSET (I1,I1,I1,I1,I1,IDUM,IDUM,IDUM,
     I             RMIN,RMAX,RDEF,DDUM,DDUM,DDUM,
     I             IDUM,IDUM,IDUM,IDUM1,BLNK)
C
      CALL QRESPR (UMESFL,SCLU,SGRP,
     M             RVAL)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPR
     I                    (UMESFL,SCLU,SGRP,
     M                     RVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses in the message file are used to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are decimal
C     numbers and the valid responses are checked against a minimum and
C     maximum that are on the message file.
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP
      REAL      RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing question
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     RVAL   - value of users response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,IRET,RETCOD,SGLCHR,LFLEN
      REAL        NODAT
      CHARACTER*1 NONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, STRFND, CHRDEC, WMSGTP, FLSETR
      EXTERNAL    ZEDT0M, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR= 0
      I4    = 4
      NODAT = -999.
C
 20   CONTINUE
C       get needed info from WDM file
        CALL WMSGTP (UMESFL,SCLU,SGRP,
     O               I,RETCOD)
C       set field value if needed
        CALL FLSETR (RDEF(1),FLEN(1),
     M               RVAL,
     O               ZMNTX1(SCOL(1),FLIN(1)))
        CALL ZEDT0M (SGLCHR,
     O               IRET)
C       go back up on oops
      IF (IRET.EQ.-1) GO TO 20
      IF (RETCOD.EQ.0) THEN
C       all ok
        I= SCOL(1)
        J= SCOL(1)+ FLEN(1)- 1
        CALL ZLJUST (ZMNTXT(FLIN(1))(I:J))
        LFLEN = LENSTR(FLEN(1),ZMNTX1(I,FLIN(1)))
        IF (LFLEN.GT.0) THEN
C         string to process
          IF (STRFND(FLEN(1),ZMNTX1(I,FLIN(1)),I4,NONE).EQ.0) THEN
C           convert value in text to variable
            RVAL= CHRDEC(LFLEN,
     I                   ZMNTX1(I,FLIN(1)))
          ELSE
C           current value is 'none', set to -999.
            RVAL= NODAT
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPUD
     I                    (UMESFL,SCLU,SGRP,DMIN,DMAX,DDEF,
     M                     DVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses are supplied by the user to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are integer
C     numbers and the valid responses are checked against a minimum and
C     maximum that are supplied by the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL,SCLU,SGRP
      DOUBLE PRECISION DMIN,DMAX,DDEF,DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number for WDM file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     DMIN   - user defined minimum value
C     DMAX   - user defined maximum value
C     DDEF   - user defined default value
C     DVAL   - users response
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      REAL        RDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRESPD
C
C     + + + END SPECFICATIONS + + +
C
      I1      = 1
      IDUM1(1)= 1
      IDUM(1) = 0
      RDUM(1) = 0.0
      BLNK(1) = ' '
C
C     set min/max/def to user defined values
      CALL QSCSET (I1,I1,I1,I1,I1,IDUM,IDUM,IDUM,
     I             RDUM,RDUM,RDUM,DMIN,DMAX,DDEF,
     I             IDUM,IDUM,IDUM,IDUM1,BLNK)
C
      CALL QRESPD (UMESFL,SCLU,SGRP,
     M             DVAL)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPD
     I                   (UMESFL,SCLU,SGRP,
     M                    DVAL)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for a response with a question from
C     the message file.  Valid responses in the message file are used to
C     check the response.  The user is prompted until an acceptable
C     answer is received.  The responses for this routine are double
C     precision numbers and the valid responses are checked against a
C     minimum and maximum that are on the message file.
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          UMESFL,SCLU,SGRP
      DOUBLE PRECISION DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - message file containing question
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     DVAL   - value of users response
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I4,IRET,RETCOD,SGLCHR,LFLEN
      DOUBLE PRECISION NODAT
      CHARACTER*1 NONE(4)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND
      DOUBLE PRECISION CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, STRFND, CHRDPR, WMSGTP, FLSETD
      EXTERNAL    ZEDT0M, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NONE/'n','o','n','e'/
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR= 0
      I4    = 4
      NODAT = -999.
C
 20   CONTINUE
C       get needed info from WDM file
        CALL WMSGTP (UMESFL,SCLU,SGRP,
     O               I,RETCOD)
C       set field value if needed
        CALL FLSETD (DDEF(1),FLEN(1),
     M               DVAL,
     O               ZMNTX1(SCOL(1),FLIN(1)))
        CALL ZEDT0M (SGLCHR,
     O               IRET)
C       back up on oops
      IF (IRET.EQ.-1) GO TO 20
      IF (RETCOD.EQ.0) THEN
C       all ok
        I= SCOL(1)
        J= SCOL(1)+ FLEN(1)- 1
        CALL ZLJUST (ZMNTXT(FLIN(1))(I:J))
        LFLEN = LENSTR(FLEN(1),ZMNTX1(I,FLIN(1)))
        IF (LFLEN.GT.0) THEN
C         string to process
          IF (STRFND(FLEN(1),ZMNTX1(I,FLIN(1)),I4,NONE).EQ.0) THEN
C           convert value in text to variable
            DVAL= CHRDPR(LFLEN,
     I                   ZMNTX1(I,FLIN(1)))
          ELSE
C           current value is 'none', set to -999.
            DVAL= NODAT
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRESPO
     I                   (MESSFL,SCLU,SGRP,OPCNT,OPLEN,MXSEL,MNSEL,
     M                    OPVAL)
C
C     + + + PURPOSE + + +
C     Perform option type field only data screen.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,OPCNT,OPLEN,
     1          MXSEL(OPCNT),MNSEL(OPCNT),OPVAL(OPLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number on message file
C     OPCNT  - number of sets of options on screen
C     OPLEN  - total number of options which may be selected from screen
C     MXSEL  - maximum number of options which may be selected per set
C     MNSEL  - minimum number of options which may be selected per set
C     OPVAL  - array of order numbers, within sets, of selected fields
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,RETCOD,IRET,SGLCHR
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTP, QSETOP, ZEDT0M, QGETOP
C
C     + + + END SPECIFICATIONS + + +
C
      SGLCHR = 0
C
 10   CONTINUE
C       get needed info from WDM file
        CALL WMSGTP (MESSFL,SCLU,SGRP,
     O               I,RETCOD)
        CALL QSETOP (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C       do 1-d data screen editing
        CALL ZEDT0M (SGLCHR,
     O               IRET)
C       go back on oops
      IF (IRET.EQ.-1) GO TO 10
C
      CALL QGETOP (OPLEN,
     O             OPVAL)
C
      RETURN
      END
