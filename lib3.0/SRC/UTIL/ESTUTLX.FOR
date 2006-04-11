C
C
C
      INTEGER   FUNCTION   DAYMON
     I                           (YR,MON)
C
C     + + + PURPOSE + + +
C     Return the number of days in the given month for the given
C     year, with leap year taken into account.  For an invalid
C     month, -1 is returned.  For an invalid year and a valid month,
C     the correct number of days is returned, with February = 28.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MON,YR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year, valid range is 1 - 2080
C     MON    - month, valid range is 1 - 12
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I4,I100,I400,NDAMON(12)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     + + + END SPECIFICATIONS + + +
C
      I4 = 4
      I100 = 100
      I400 = 400
      IF (MON.EQ.2) THEN
        IF (YR .LE. 0  .OR.  YR .GT. 2080) THEN
C         invalid year
          DAYMON = 28
        ELSE IF (MOD(YR,I100).EQ.0) THEN
C         check whether this is a leap year on a century boundary
          IF (MOD(YR,I400).EQ.0) THEN
C           on a 400 year boundary
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        ELSE
          IF (MOD(YR,I4).EQ.0) THEN
C           leap year
            DAYMON = 29
          ELSE
            DAYMON = 28
          END IF
        END IF
C
      ELSE IF (MON.GE.1 .AND. MON.LE.12) THEN
C       no problem
        DAYMON = NDAMON(MON)
      ELSE
C       invalid month
        DAYMON = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMADD
     I                    (DATE1,TCODE,TSTEP,NVALS,
     O                     DATE2)
C
C     + + + PURPOSE + + +
C     Add NVALS time steps to first date to compute second date.
C     The first date is assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),TCODE,TSTEP,DATE2(6)
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - starting date
C     TCODE  - time units
C              1 - second          5 - month
C              2 - minute          6 - year
C              3 - hour            7 - century
C              4 - day
C     TSTEP  - time step in TCODE units
C     NVALS  - number of time steps to be added
C     DATE2  - new date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 CARRY,TIMTMP(6),I4ZRO,I4TMP,DPM
      INTEGER   I,STPOS,DONFG,IYR,IMO
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO= 0
      DO 5 I= 1,6
        TIMTMP(I)= DATE1(I)
 5    CONTINUE
C
C     figure out how much time to add and where to start
      CARRY= NVALS* TSTEP
      STPOS= TCODE
      IF (STPOS.EQ.7) THEN
C       the time units are centuries, convert to years
        STPOS= 6
        CARRY= CARRY* 100
      END IF
C
C     add the time, not changing insig. parts
      GO TO (10,20,30,40,50,60), STPOS
C
 10   CONTINUE
C       seconds
        TIMTMP(6)= TIMTMP(6)+ CARRY
        CARRY    = TIMTMP(6)/ 60
        TIMTMP(6)= TIMTMP(6)- (CARRY*60)
 20   CONTINUE
C       minutes
        TIMTMP(5)= TIMTMP(5)+ CARRY
        CARRY    = TIMTMP(5)/ 60
        TIMTMP(5)= TIMTMP(5)- (CARRY*60)
 30   CONTINUE
C       hours
        TIMTMP(4)= TIMTMP(4)+ CARRY
        CARRY    = TIMTMP(4)/ 24
        TIMTMP(4)= TIMTMP(4)- (CARRY*24)
        IF (TIMTMP(4).EQ.I4ZRO.AND.TIMTMP(5).EQ.I4ZRO.AND.
     1      TIMTMP(6).EQ.I4ZRO) THEN
C         this is the day boundry problem
          TIMTMP(4)= 24
          CARRY = CARRY- 1
        END IF
 40   CONTINUE
C       days
        TIMTMP(3)= TIMTMP(3)+ CARRY
        DONFG= 0
 45     CONTINUE
          IYR= TIMTMP(1)
          IMO= TIMTMP(2)
          DPM= DAYMON(IYR,IMO)
          IF (TIMTMP(3).GT.DPM) THEN
C           add another month
            TIMTMP(3)= TIMTMP(3)- DPM
            TIMTMP(2)= TIMTMP(2)+ 1
            I4TMP= 12
            IF (TIMTMP(2).GT.I4TMP) THEN
              TIMTMP(2)= 1
              TIMTMP(1)= TIMTMP(1)+ 1
            END IF
          ELSE IF (TIMTMP(3).LE.0) THEN
C           subtract another month
            TIMTMP(2)= TIMTMP(2)- 1
            I4TMP= 0
            IF (TIMTMP(2).EQ.I4TMP) THEN
              TIMTMP(1)= TIMTMP(1)- 1
              TIMTMP(2)= 12
            END IF
            IYR= TIMTMP(1)
            IMO= TIMTMP(2)
            TIMTMP(3)= TIMTMP(3)- DAYMON(IYR,IMO)
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 45
C       month and year updated here, so
        CARRY= 0
 50   CONTINUE
C       months
        TIMTMP(2)= TIMTMP(2)+ CARRY
        CARRY    = (TIMTMP(2)-1)/ 12
        TIMTMP(2)= TIMTMP(2)- (CARRY*12)
 60   CONTINUE
C       years
        TIMTMP(1)= TIMTMP(1)+ CARRY
C     end computed go to
      IF (TCODE.GE.5) THEN
C       check days/month
        IYR= TIMTMP(1)
        IMO= TIMTMP(2)
        I4TMP= DAYMON(IYR,IMO)
        IF (I4TMP.LT.TIMTMP(3)) TIMTMP(3)= I4TMP
        IF (DAYMON(DATE1(1),DATE1(2)).EQ.DATE1(3)) TIMTMP(3)= I4TMP
      END IF
C
      DO 100 I= 1,6
        DATE2(I)= TIMTMP(I)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMDFX
     I                    (DATE1,DATE2,
     O                     NVALS,TCODE,TSTEP)
C
C     + + + PURPOSE + + +
C     Calculate the number of values between two dates, including
C     units and time step.  First tries at one year time step, then
C     TCODE is decreased by one and tried again until the exact
C     time difference is determined.  Dates are assumed to be valid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1 - first (starting) date
C     DATE2 - second (ending) date
C     NVALS  - number of values at the output TSTEP and TCODE
C     TCODE  - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     TSTEP  - time step in TCODE units
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG,TIMTMP(6)
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMADD, TIMCHK, TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      TCODE= 6
      TSTEP= 1
      DONFG= 0
C
 10   CONTINUE
        CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP,
     O               NVALS)
        CALL TIMADD (DATE1,TCODE,TSTEP,NVALS,
     O               TIMTMP)
        IF (TIMCHK(DATE2,TIMTMP).EQ.0) THEN
C         we have call exact units, etc
          DONFG= 1
        ELSE
C         try again with shorter units
          TCODE= TCODE- 1
          IF (TCODE.EQ.0) WRITE (*,*) 'BAD TIMDFX',DATE1,DATE2
        END IF
C
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
