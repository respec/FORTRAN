C
C
C
      SUBROUTINE   QSPUIA
     I                   (MESSFL,SCLU,SGRP,IMIN,IMAX,IDEF,NSIZ,ICHK,
     M                    NVAL,
     O                    IVALS)
C
C     Get a set of integer numbers from the user from the terminal.
C     The min, max, and default values are set by the calling code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,IMIN,IMAX,IDEF,NSIZ,ICHK,NVAL
      INTEGER   IVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     IMIN   - minimum acceptable value
C     IMAX   - maximum acceptable value
C     IDEF   - default value
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     IVALS  - array of the values
C
C     + + + LOCAL VARAIBLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      REAL        RDUM(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRSPIA
C
C     + + + END SPECIFICATIONS + + +
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
      CALL QRSPIA (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M             NVAL,
     O             IVALS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPIA
     I                   (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M                    NVAL,
     O                    IVALS)
C
C     Get a set of integer numbers from the user from the terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,NSIZ,ICHK,NVAL
      INTEGER   IVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     IVALS  - array of the values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,LSCLU,LSGRP,CNT,INUM,IROW,NROW,RESP,DONFG,
     1            NCHK,IRET,IVAL(2,10),CVAL(3,10),PRVFLG,INTFLG
      REAL        RVAL(10)
      CHARACTER*1 TBUFF(80,10)
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCMDON, QRESP, PMXTXI, ZIPI, ZSTCMA, ZGTRET
      EXTERNAL    QRESCN, ZMNSST
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      LSCLU= 1
      DONFG= 0
C
      IF (NVAL.GT.0) THEN
C       exact number of values needed is provided
        NCHK = NVAL
      ELSE
C       get any number, up to max allowed
        NCHK = NSIZ
      END IF
C
      CALL ZIPI (NSIZ,I0,IVALS)
C
      INUM= 2
      IROW= 1
 10   CONTINUE
C       loop until all values entered or user wants out
C       allow previous and interrupt commands
        I= 4
        IF (ZCMDON(I).EQ.0) THEN
C         make 'prev' available
          J= 1
          CALL ZSTCMA (I,J)
          PRVFLG= 0
        ELSE
C         'Prev' already available, set flag to not turn it off
          PRVFLG= 1
        END IF
        I= 16
        IF (ZCMDON(I).EQ.0) THEN
C         make 'Intrpt' available
          J= 1
          CALL ZSTCMA (I,J)
          INTFLG= 0
        ELSE
C         'Intrpt' already available, set flag to not turn it off
          INTFLG= 1
        END IF
        IF (IROW+9.LE.NCHK) THEN
C         room for 10 more rows of values
          NROW= 10
        ELSE
C         running into end of allowed data
          NROW= NCHK- IROW+ 1
        END IF
        DO 20 I= 1,NROW
          IVAL(1,I)= IROW+I-1
          IVAL(2,I)= IVALS(IROW+I-1)
 20     CONTINUE
C
        CALL QRESCN (MESSFL,SCLU,SGRP,INUM,I1,I1,NROW,I1,
     M               IVAL,RVAL,CVAL,TBUFF)
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants Next screen
          IF (IROW+NROW-1.EQ.NCHK) THEN
C           at end of data, clean exit
            I= 0
 30         CONTINUE
C             accept any valid values from last screen
              I= I+ 1
              IF (IVAL(2,I).GT.-999) THEN
C               good enough
                IVALS(IROW+I-1)= IVAL(2,I)
              END IF
            IF (I.LT.NROW .AND. IVAL(2,I).GT.-999) GO TO 30
            IF (I.EQ.NROW .AND. IVAL(2,NROW).GT.-999) THEN
C             all values on screen are good
              CNT = IROW+ NROW- 1
            ELSE
C             only first I-1 values on screen are good
              CNT = IROW+ I- 2
            END IF
            DONFG= 1
          ELSE
C           do next set of values
            DO 35 I= 1,NROW
              IVALS(IROW+I-1)= IVAL(2,I)
 35         CONTINUE
            IROW= IROW+ 10
          END IF
        ELSE IF (IRET.EQ.2) THEN
C         user wants previous screen
          IF (IROW.GT.1) THEN
C           room to back up
            IF (IROW-NROW.LT.1) THEN
C             dont back up past first row
              IROW= 1
            ELSE
C             back up NROW rows
              IROW= IROW- NROW
            END IF
          ELSE
C           user backing out of input
            DONFG= 1
            CNT  = 0
          END IF
        ELSE IF (IRET.EQ.7) THEN
C         user interrupting out of input
          DONFG= 1
          I= 0
 50       CONTINUE
C           accept any valid values from last screen
            I= I+ 1
            IF (IVAL(2,I).GT.-999) THEN
C             good enough
              IVALS(IROW+I-1)= IVAL(2,I)
            END IF
          IF (I.LT.NROW .AND. IVAL(2,I).GT.-999) GO TO 50
          IF (I.EQ.NROW .AND. IVAL(2,NROW).GT.-999) THEN
C           all values on screen are good
            CNT = IROW+ NROW- 1
          ELSE
C           only first I-1 values on screen are good
            CNT = IROW+ I- 2
          END IF
        END IF
C
        IF (DONFG.EQ.1) THEN
C         exiting
          IF (PRVFLG.EQ.0) THEN
C           turn off previous command
            I= 4
            CALL ZSTCMA (I,I0)
          END IF
          IF (INTFLG.EQ.0) THEN
C           turn off interrupt command
            I= 16
            CALL ZSTCMA (I,I0)
          END IF
          IF ((CNT.GT.0 .OR. IRET.EQ.1) .AND. ICHK.EQ.1) THEN
C           check number of values
            IF (NVAL.GT.0) THEN
C             check number input against number desired
              LSGRP= 78
              I= 2
              IVAL(1,1)= CNT
              IVAL(2,1)= NVAL
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I,IVAL)
            ELSE
C             just show total valid values entered
              LSGRP= 79
              IVAL(1,1)= CNT
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I1,IVAL)
            END IF
            CALL ZMNSST
            LSGRP= 83
            CALL QRESP (MESSFL,LSCLU,LSGRP,RESP)
            IF (RESP.EQ.2) THEN
C             dont exit just yet
              DONFG= 0
            END IF
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      NVAL = CNT
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSPURA
     I                   (MESSFL,SCLU,SGRP,RMIN,RMAX,RDEF,NSIZ,ICHK,
     M                    NVAL,
     O                    RVALS)
C
C     Get a set of real numbers from the user from the terminal.
C     The min, max, and default values are set by the calling code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,NSIZ,ICHK,NVAL
      REAL      RMIN,RMAX,RDEF,RVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     RMIN   - minimum acceptable value
C     RMAX   - maximum acceptable value
C     RDEF   - default value
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     RVALS  - array of the values
C
C     + + + LOCAL VARAIBLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRSPRA
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
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
      CALL QRSPRA (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M             NVAL,
     O             RVALS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPRA
     I                   (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M                    NVAL,
     O                    RVALS)
C
C     Get a set of real numbers from the user from the terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,NSIZ,ICHK,NVAL
      REAL      RVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     RVALS  - array of the values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,LSCLU,LSGRP,CNT,IROW,NROW,RESP,DONFG,
     1            NCHK,IRET,IVAL(10),CVAL(3,10),PRVFLG,INTFLG
      REAL        RVAL(10),R0
      CHARACTER*1 TBUFF(80,10)
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCMDON, QRESP, PMXTXI, ZIPR, ZSTCMA, ZGTRET
      EXTERNAL    QRESCN, ZMNSST
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      R0 = 0.0
      LSCLU= 1
      DONFG= 0
C
      IF (NVAL.GT.0) THEN
C       exact number of values needed is provided
        NCHK = NVAL
      ELSE
C       get any number, up to max allowed
        NCHK = NSIZ
      END IF
C
      CALL ZIPR (NSIZ,R0,RVALS)
C
      IROW= 1
 10   CONTINUE
C       loop until all values entered or user wants out
C       allow previous and interrupt commands
        I= 4
        IF (ZCMDON(I).EQ.0) THEN
C         make 'prev' available
          J= 1
          CALL ZSTCMA (I,J)
          PRVFLG= 0
        ELSE
C         'Prev' already available, set flag to not turn it off
          PRVFLG= 1
        END IF
        I= 16
        IF (ZCMDON(I).EQ.0) THEN
C         make 'Intrpt' available
          J= 1
          CALL ZSTCMA (I,J)
          INTFLG= 0
        ELSE
C         'Intrpt' already available, set flag to not turn it off
          INTFLG= 1
        END IF
        IF (IROW+9.LE.NCHK) THEN
C         room for 10 more rows of values
          NROW= 10
        ELSE
C         running into end of allowed data
          NROW= NCHK- IROW+ 1
        END IF
        DO 20 I= 1,NROW
          IVAL(I)= IROW+I-1
          RVAL(I)= RVALS(IROW+I-1)
 20     CONTINUE
C
        CALL QRESCN (MESSFL,SCLU,SGRP,I1,I1,I1,NROW,I1,
     M               IVAL,RVAL,CVAL,TBUFF)
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants Next screen
          IF (IROW+NROW-1.EQ.NCHK) THEN
C           at end of data, clean exit
            I= 0
 30         CONTINUE
C             accept any valid values from last screen
              I= I+ 1
              IF (RVAL(I).GT.-999.) THEN
C               good enough
                RVALS(IROW+I-1)= RVAL(I)
              END IF
            IF (I.LT.NROW .AND. RVAL(I).GT.-999.) GO TO 30
            IF (I.EQ.NROW .AND. RVAL(NROW).GT.-999.) THEN
C             all values on screen are good
              CNT = IROW+ NROW- 1
            ELSE
C             only first I-1 values on screen are good
              CNT = IROW+ I- 2
            END IF
            DONFG= 1
          ELSE
C           do next set of values
            DO 35 I= 1,NROW
              RVALS(IROW+I-1)= RVAL(I)
 35         CONTINUE
            IROW= IROW+ 10
          END IF
        ELSE IF (IRET.EQ.2) THEN
C         user wants previous screen
          IF (IROW.GT.1) THEN
C           room to back up
            IF (IROW-NROW.LT.1) THEN
C             dont back up past first row
              IROW= 1
            ELSE
C             back up NROW rows
              IROW= IROW- NROW
            END IF
          ELSE
C           user backing out of input
            DONFG= 1
            CNT  = 0
          END IF
        ELSE IF (IRET.EQ.7) THEN
C         user interrupting out of input
          DONFG= 1
          I= 0
 50       CONTINUE
C           accept any valid values from last screen
            I= I+ 1
            IF (RVAL(I).GT.-999.) THEN
C             good enough
              RVALS(IROW+I-1)= RVAL(I)
            END IF
          IF (I.LT.NROW .AND. RVAL(I).GT.-999.) GO TO 50
          IF (I.EQ.NROW .AND. RVAL(NROW).GT.-999.) THEN
C           all values on screen are good
            CNT = IROW+ NROW- 1
          ELSE
C           only first I-1 values on screen are good
            CNT = IROW+ I- 2
          END IF
        END IF
C
        IF (DONFG.EQ.1) THEN
C         exiting
          IF (PRVFLG.EQ.0) THEN
C           turn off previous
            I= 4
            CALL ZSTCMA (I,I0)
          END IF
          IF (INTFLG.EQ.0) THEN
C           turn off interrupt
            I= 16
            CALL ZSTCMA (I,I0)
          END IF
          IF ((CNT.GT.0 .OR. IRET.EQ.1) .AND. ICHK.EQ.1) THEN
C           check number of values
            IF (NVAL.GT.0) THEN
C             check number input against number desired
              LSGRP= 78
              I= 2
              IVAL(1)= CNT
              IVAL(2)= NVAL
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I,IVAL)
            ELSE
C             just show total valid values entered
              LSGRP= 79
              IVAL(1)= CNT
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I1,IVAL)
            END IF
            CALL ZMNSST
            LSGRP= 83
            CALL QRESP (MESSFL,LSCLU,LSGRP,RESP)
            IF (RESP.EQ.2) THEN
C             dont exit just yet
              DONFG= 0
            END IF
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      NVAL = CNT
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   QSPUDA
     I                   (MESSFL,SCLU,SGRP,DMIN,DMAX,DDEF,NSIZ,ICHK,
     M                    NVAL,
     O                    DVALS)
C
C     Get a set of real numbers from the user from the terminal.
C     The min, max, and default values are set by the calling code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,NSIZ,ICHK,NVAL
      DOUBLE PRECISION  DMIN,DMAX,DDEF,DVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     DMIN   - minimum acceptable value
C     DMAX   - maximum acceptable value
C     DDEF   - default value
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     DVALS  - array of the values
C
C     + + + LOCAL VARAIBLES + + +
      INTEGER     I1,IDUM(1),IDUM1(1)
      REAL        RDUM(1)
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSET, QRSPDA
C
C     + + + END SPECIFICATIONS + + +
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
      CALL QRSPDA (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M             NVAL,
     O             DVALS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   QRSPDA
     I                   (MESSFL,SCLU,SGRP,NSIZ,ICHK,
     M                    NVAL,
     O                    DVALS)
C
C     Get a set of real numbers from the user from the terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,NSIZ,ICHK,NVAL
      DOUBLE PRECISION  DVALS(NSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number in the message file
C     SGRP   - group number in cluster
C     NSIZ   - size of buffer
C     ICHK   - flag indicating whether or not to check number of input values
C              0 - dont check, 1 - check, get user to confirm
C     NVAL   - number of values to get, if -999 then count entries
C     DVALS  - array of the values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,LSCLU,LSGRP,CNT,IROW,NROW,RESP,DONFG,
     1            NCHK,IRET,IVAL(10),CVAL(3,10),PRVFLG,INTFLG
      REAL        RVAL(10)
      DOUBLE PRECISION DVAL(10),D0
      CHARACTER*1 TBUFF(80,10)
C
C     + + + FUNCTIONS + + +
      INTEGER     ZCMDON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZCMDON, QRESP, PMXTXI, ZIPD, ZSTCMA, ZGTRET
      EXTERNAL    QRESCD, ZMNSST
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      D0 = 0.0
      LSCLU= 1
      DONFG= 0
C
      IF (NVAL.GT.0) THEN
C       exact number of values needed is provided
        NCHK = NVAL
      ELSE
C       get any number, up to max allowed
        NCHK = NSIZ
      END IF
C
      CALL ZIPD (NSIZ,D0,DVALS)
C
      IROW= 1
 10   CONTINUE
C       loop until all values entered or user wants out
C       allow previous and interrupt commands
        I= 4
        IF (ZCMDON(I).EQ.0) THEN
C         make 'prev' available
          J= 1
          CALL ZSTCMA (I,J)
          PRVFLG= 0
        ELSE
C         'Prev' already available, set flag to not turn it off
          PRVFLG= 1
        END IF
        I= 16
        IF (ZCMDON(I).EQ.0) THEN
C         make 'Intrpt' available
          J= 1
          CALL ZSTCMA (I,J)
          INTFLG= 0
        ELSE
C         'Intrpt' already available, set flag to not turn it off
          INTFLG= 1
        END IF
        IF (IROW+9.LE.NCHK) THEN
C         room for 10 more rows of values
          NROW= 10
        ELSE
C         running into end of allowed data
          NROW= NCHK- IROW+ 1
        END IF
        DO 20 I= 1,NROW
          IVAL(I)= IROW+I-1
          DVAL(I)= DVALS(IROW+I-1)
 20     CONTINUE
C
        CALL QRESCD (MESSFL,SCLU,SGRP,I1,I1,I1,I1,NROW,I1,
     M               IVAL,RVAL,DVAL,CVAL,TBUFF)
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants Next screen
          IF (IROW+NROW-1.EQ.NCHK) THEN
C           at end of data, clean exit
            I= 0
 30         CONTINUE
C             accept any valid values from last screen
              I= I+ 1
              IF (DVAL(I).GT.-999.) THEN
C               good enough
                DVALS(IROW+I-1)= DVAL(I)
              END IF
            IF (I.LT.NROW .AND. DVAL(I).GT.-999.) GO TO 30
            IF (I.EQ.NROW .AND. DVAL(NROW).GT.-999.) THEN
C             all values on screen are good
              CNT = IROW+ NROW- 1
            ELSE
C             only first I-1 values on screen are good
              CNT = IROW+ I- 2
            END IF
            DONFG= 1
          ELSE
C           do next set of values
            DO 35 I= 1,NROW
              DVALS(IROW+I-1)= DVAL(I)
 35         CONTINUE
            IROW= IROW+ 10
          END IF
        ELSE IF (IRET.EQ.2) THEN
C         user wants previous screen
          IF (IROW.GT.1) THEN
C           room to back up
            IF (IROW-NROW.LT.1) THEN
C             dont back up past first row
              IROW= 1
            ELSE
C             back up NROW rows
              IROW= IROW- NROW
            END IF
          ELSE
C           user backing out of input
            DONFG= 1
            CNT  = 0
          END IF
        ELSE IF (IRET.EQ.7) THEN
C         user interrupting out of input
          DONFG= 1
          I= 0
 50       CONTINUE
C           accept any valid values from last screen
            I= I+ 1
            IF (DVAL(I).GT.-999.) THEN
C             good enough
              DVALS(IROW+I-1)= DVAL(I)
            END IF
          IF (I.LT.NROW .AND. DVAL(I).GT.-999.) GO TO 50
          IF (I.EQ.NROW .AND. DVAL(NROW).GT.-999.) THEN
C           all values on screen are good
            CNT = IROW+ NROW- 1
          ELSE
C           only first I-1 values on screen are good
            CNT = IROW+ I- 2
          END IF
        END IF
C
        IF (DONFG.EQ.1) THEN
C         exiting
          IF (INTFLG.EQ.0) THEN
C           turn off previous
            I= 4
            CALL ZSTCMA (I,I0)
          END IF
          IF (PRVFLG.EQ.0) THEN
C           turn off interrupt
            I= 16
            CALL ZSTCMA (I,I0)
          END IF
          IF ((CNT.GT.0 .OR. IRET.EQ.1) .AND. ICHK.EQ.1) THEN
C           check number of values
            IF (NVAL.GT.0) THEN
C             check number input against number desired
              LSGRP= 78
              I= 2
              IVAL(1)= CNT
              IVAL(2)= NVAL
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I,IVAL)
            ELSE
C             just show total valid values entered
              LSGRP= 79
              IVAL(1)= CNT
              CALL PMXTXI (MESSFL,LSCLU,LSGRP,I1,I1,-I1,I1,IVAL)
            END IF
            CALL ZMNSST
            LSGRP= 83
            CALL QRESP (MESSFL,LSCLU,LSGRP,RESP)
            IF (RESP.EQ.2) THEN
C             dont exit just yet
              DONFG= 0
            END IF
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      NVAL = CNT
C
      RETURN
      END
