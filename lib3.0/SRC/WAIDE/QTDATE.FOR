C
C
C
      SUBROUTINE   DATES
     I                   (DELT,FLAG,DLEN,
     M                    STRT,STOP,
     O                    NVAL,TBUF)
C
C     + + + PURPOSE + + +
C     Based on the value of FLAG, this routine gets a start and/or
C     end date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DELT,FLAG,DLEN,NVAL
      INTEGER     STRT(DLEN),STOP(DLEN)
      CHARACTER*1 TBUF(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - time step, in minutes (optional)
C     FLAG   - retrieval flag
C              0 - date
C              1 - begin date
C              2 - end date
C              3 - begin and end date
C     DLEN   - length of date string
C              1 - year only
C              2 - year, month
C              3 - year, month, day
C              4 - year, month, day, hour
C              5 - year, month, day, hour, minute
C              6 - year, month, day, hour, minute, second
C     STRT   - starting date
C     STOP   - ending date
C     NVAL   - number of time steps (optional, requires 0<DELT<=1440)
C     TBUF   - character string containing the requested dates,
C              in the format ( 2( A4, 5( 1X, A2 ), 3X ) )
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP1,SGRP2,INUM,RNUM,CNUM,
     1            IVAL(12),CVAL(1,3,1),ERR(12),ERR1,ERR2,I,IZERO
      REAL        RVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   CKDATE, DATCHK, INITM, NUMPTS, PRNTXT, QRESPM, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 28
C
      SGRP1 = 13 + FLAG
      IF (FLAG.EQ.3) THEN
        INUM = 12
      ELSE
        INUM = DLEN
      END IF
      RNUM = 1
      CNUM = 1
      I    = 12
      IZERO = 0
      CALL ZIPI (I,IZERO,ERR)
C
C     initialize date arrays
      CALL INITM (INUM,RNUM,CNUM,IVAL,RVAL,CVAL)
C
      IF (STRT(1).GT.0.AND.STRT(1).LT.2021) THEN
C       fill IVAL with user supplied defaults
        DO 90 I =1, DLEN
          IVAL(I) = STRT(I)
          IF(FLAG.EQ.3) IVAL(I+6) = STOP(I)
 90     CONTINUE
      ELSE
C       set default hours and minutes
        IF(DELT.GE.1440.OR.DELT.LE.0) THEN
C         daily time step
          IVAL(4) = 0
          IVAL(5) = 0
          IVAL(6) = 0
          IVAL(10) = 0
          IVAL(11) = 0
          IVAL(12) = 0
        ELSE
C         time step less than daily
          IF (IVAL(4)+STRT(5).EQ.0) THEN
            IVAL(4) = DELT / 60
            IVAL(5) = DELT - IVAL(4) * DELT
            IVAL(6) = 0
          END IF
          IF (IVAL(10)+IVAL(11).EQ.0) THEN
            IVAL(10) = 24
            IVAL(11) = 0
            IVAL(12) = 0
          END IF
        END IF
      END IF
C
 100  CONTINUE
        CALL QRESPM (MESSFL,SCLU,SGRP1,INUM,RNUM,CNUM,
     M               IVAL,RVAL,CVAL,TBUF)
C
C       check validity of dates
        ERR1 = 0
        ERR2 = 0
        CALL DATCHK(IVAL,ERR)
        IF(FLAG.EQ.3) THEN
          CALL DATCHK(IVAL(7),ERR(7))
          CALL CKDATE(IVAL(1),IVAL(7),ERR2)
        END IF
        DO 120 I = 1, DLEN
          ERR1 = ERR1 + ERR(I) + ERR(I+6)
 120    CONTINUE
C
        IF (ERR1.NE.0.OR.ERR2.EQ.1) THEN
C
C         error in dates, reenter
          SGRP2 = 17
          CALL PRNTXT(MESSFL,SCLU,SGRP2)
        END IF
      IF (ERR1.NE.0.OR.ERR2.EQ.1) GO TO 100
C
      IF (DELT.GT.0.AND.DELT.LE.1440.AND.FLAG.EQ.3) THEN
C       determine number of time steps between dates
        CALL NUMPTS(IVAL(1),IVAL(7),DELT,NVAL)
      END IF
C
C     fill start and stop arrays
      DO 150 I = 1, DLEN
        STRT(I) = IVAL(I)
        IF (FLAG.EQ.3) STOP(I) = IVAL(I+6)
 150  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATESQ
     I                    (DLEN,
     M                     STRT,STOP)
C
C     + + + PURPOSE + + +
C     This routine is Jack's and just calls Kate's DATES routine
C     after a flag is set.  It helps Jack meet his monthly
C     routine quota.
C     It gets begin and end dates of length DLEN from the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DLEN
      INTEGER   STRT(DLEN),STOP(DLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DLEN   - length of dates
C     STRT   - start date
C     STOP   - end date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DELT,FLAG,NVAL
      CHARACTER*1 TBUF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL   DATES
C
C     + + + END SPECIFICATIONS + + +
C
      DELT= 0
      FLAG= 3
      CALL DATES (DELT,FLAG,DLEN,
     M            STRT,STOP,
     O            NVAL,TBUF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATEST
     I                    (DLEN,
     M                     STRT)
C
C     + + + PURPOSE + + +
C     This routine gets a date of length DLEN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DLEN
      INTEGER     STRT(DLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DLEN   - length of date
C     STRT   - start date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DELT,FLAG,NVAL,STOP(6)
      CHARACTER*1 TBUF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL   DATES
C
C     + + + END SPECIFICATIONS + + +
C
      DELT= 0
      FLAG= 0
      CALL DATES (DELT,FLAG,DLEN,
     M            STRT,STOP,
     O            NVAL,TBUF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTTSTU
     I                   (WNDNAM,
     M                    TSSTEP,TUNITS)
C
C     + + + PURPOSE + + +
C     This routine asks the user to enter a time step and time units.
C     The input values are checked for a valid combination.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TSSTEP,TUNITS
      CHARACTER WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSSTEP - time step of data in TUNITS units
C              input as default
C              outputs users answer
C     TUNITS - time units, input default
C              1 - seconds       4 - day
C              2 - minutes       5 - month
C              3 - hours         6 - year
C     WNDNAM - window name
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,TS,TU(1,3),OTFLG,GOOD,LENI,LENR,LENC
      REAL        DUMR(1)
      CHARACTER*1 TBUFF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL    QRESPM, CKTSTU, ZWNSET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SGRP, LENI, LENR, LENC, DUMR, OTFLG
     #    /   18,    1,    1,    1,  0.0,     1 /
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 28
C
C     set users defaults if available
      IF (TUNITS.GT.0 .AND. TUNITS.LT.7) THEN
C       valid time unit
        TU(1,1) = TUNITS
      ELSE
C       invalid time unit, set to default
        TU(1,1) = -999
      END IF
      IF (TSSTEP.GT.0 .AND. TSSTEP.LT.3601) THEN
C       assume good time step
        TS = TSSTEP
      ELSE
C       invalid time step, set to default
        TS = -999
      END IF
 100  CONTINUE
C       get time step and time units
        CALL ZWNSET (WNDNAM)
        CALL QRESPM (MESSFL,SCLU,SGRP,LENI,LENR,LENC,
     M               TS,DUMR,TU,TBUFF)
C       check that combination of time step and unit is valid
        CALL CKTSTU (TS,TU(1,1),OTFLG,WNDNAM,
     O               GOOD)
      IF (GOOD.NE.0) GO TO 100
C
      TUNITS = TU(1,1)
      TSSTEP = TS
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKTSTU
     I                   (TSSTEP,TUNITS,OTFLG,WNDNAM,
     O                    RETC)
C
C     + + + PURPOSE + + +
C     This routine checks time step and units for compatility.
C     If they are not compatible and the output flag is 1,
C     the allowable time steps for the given time units will
C     be output to the terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TSSTEP,TUNITS,OTFLG,RETC
      CHARACTER WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TSSTEP - time step, in TUNITS units
C     TUNITS - time units
C              1 - seconds      4 - days
C              2 - minutes      5 - months
C              3 - hours        6 - years
C     OTFLG  - output flag for output of error message
C              0 - don't print error message
C              1 - print error message
C     RETC   - return code
C              0 - compatible time step and time units
C              1 - incompatible time step and time units
C              2 - invalid time units, timestep not checked
C     WNDNAM - window name
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I0,I1,IN,N,NTS(6),STS(6),TS(55),SCLU,SGRP
      CHARACTER*30 LWNDNM
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, PMXTXI, ZWNSET
C
C     + + + DATA INITIALIZATIONS + + +
C                 sc  mn  hr  dy  mo  yr
      DATA  NTS / 24, 19,  8,  1,  2,  1 /
      DATA  STS / 32, 13,  5,  4,  2,  1 /
      DATA   TS
     #    / 1,
     #      1, 12,
     #      1,
     #      1, 24, 12, 2, 3, 6, 4, 8,
     #      1, 5, 15, 30, 60, 1440, 10, 2, 3, 4, 6, 10, 12, 20,
     #         120, 180, 240, 480, 720,
     #      30, 10, 1, 5, 2, 3, 4, 6, 12, 15, 20, 60, 90, 120, 180,
     #         240, 300, 360, 600, 720, 900, 1200, 1800, 3600      /
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 28
      I0  = 0
      I1  = 1
C
      RETC = 1
      IF (TUNITS.LT.1 .OR. TUNITS.GT.6) THEN
C       invalid time units
        RETC = 2
        SGRP = 26
        N = 1
        LWNDNM= WNDNAM(1:ZLNTXT(WNDNAM))//' Problem'
        CALL ZWNSET (LWNDNM)
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I0,I0,N,TUNITS)
      ELSE
C       valid time units, check compatibility
        J = STS(TUNITS)
        N = NTS(TUNITS)
        I = J - 1
        IN = I + N
 100    CONTINUE
          I = I + 1
          IF (TSSTEP .EQ. TS(I)) RETC = 0
        IF (RETC.EQ.1 .AND. I.LT.IN) GO TO 100
        IF (RETC.EQ.1 .AND. OTFLG.EQ.1) THEN
C         write out acceptable time steps
          SGRP = 19 + TUNITS - 1
          LWNDNM= WNDNAM(1:ZLNTXT(WNDNAM))//' Problem'
          CALL ZWNSET (LWNDNM)
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I0,I0,N,TS(I1))
        END IF
      END IF
C
      RETURN
      END
