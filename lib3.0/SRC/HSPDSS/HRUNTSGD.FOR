C
C
C
      SUBROUTINE GETDSS
     I                  (DELT,WIDTH,FSTCAL)
C
C     + + + PURPOSE + + +
C     get values from dss file, transform them and fill a row on
C     the inpad
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,WIDTH,FSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,DTRAN,I80,RETCOD,SGRP,NGETS,NPTS,I,NVALS,I4,
     #             I20,I64,J,I1,PLACE,FSTFG,WID,MINS,JULS,I0,IOFFST,
     #             SYEAR,SMONTH,LNREM,LAGGFG
      REAL         CURVAL(1)
      CHARACTER*1  BLANK
      CHARACTER*4  CURTIM
      CHARACTER*8  CUNITS,CDTYPE
      CHARACTER*20 CURDAT
      CHARACTER*80 CPATH
C
C     + + + FUNCTIONS + + +
      INTEGER     INT,MIN0,DYPMON,VYDELT,IHM2M
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT,MIN0
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC,TIM2CH,OMSTI,OMSTC,OMSG,DSSGET,LTRAN,ADDCDT
      EXTERNAL    CH2TIM,IHM2M,ZOFSET,DATJUL,DATYMD,DYPMON,VYDELT
      EXTERNAL    OMSTD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I4,I20,I64,I80/0,1,4,20,64,80/
      DATA BLANK/' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (16A4)
 2300 FORMAT (' DSS PLACE, PAD(PLACE):  ',I10,E12.4)
 3000 FORMAT (/,' ENTERING GETDSS')
 3010 FORMAT (/,' LEAVING GETDSS',
     #          '   NGETS=',I6,';  NVALS=',I6,';  NPTS=',I6)
 3020 FORMAT (3X,'INPAD DELTA TIME=',I6,3X, 'DSS DELTAT TIME=',I6,
     #        3X,'WIDTH=',I12,/,3X,
     #        'INPAD START TIME (MIN)=',I10,/,3X,
     #        'INPAD START TIME (YR,MO,DY,ETC)=',6I6)
 3030 FORMAT (3X,'VOPADR=',I6,3X,'OFFSET=',I6,3X,'STKIND=',I6,/,
     #        3X,'STTRAN=',I6,3X,'INMODE=',I6,/,3X,'A=',E18.6,6X,
     #        'B=',E18.6)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 239
C
      IF (TESTFG .GE. 1) THEN
        WRITE (MESSU,3000)
        WRITE (MESSU,3020) DELT,DELTAT,WIDTH,INPSTR,INPDAT
        WRITE (MESSU,3030) VOPADR(1),OFFSET(1), STKIND(1),
     #                     STTRAN(1),INMODE(1), A(1),B(1)
      END IF
C
C     find transformation code
      IF ( (STTRAN(1) .EQ. 1) .OR. (STTRAN(1) .EQ. 3) ) THEN
C       same/aver
        DTRAN= 0
      ELSE IF ( (STTRAN(1) .EQ. 2) .OR. (STTRAN(1) .EQ. 4) ) THEN
C       sum/div
        DTRAN= 1
      ELSE IF (STTRAN(1) .EQ. 5) THEN
C       max
        DTRAN= 3
      ELSE IF (STTRAN(1) .EQ. 6) THEN
C       min
        DTRAN= 2
      ELSE
C       convert intp/last to same/aver
        DTRAN= 0
      END IF
C
C     convert pathname to character variable
      CALL ZIPC (I80,BLANK,
     O           CPATH)
      WRITE (CPATH,2000) (PVAR(I),I=1,16)
C
C     initialize inpad time to start time for inpad
      CALL TIM2CH (INPDAT,
     O             CURDAT,CURTIM,RETCOD)
C
      IF (RETCOD .NE. 0) THEN
C       error - converting time
        CALL OMSTI (INPDAT(1))
        CALL OMSTI (INPDAT(2))
        CALL OMSTI (INPDAT(3))
        CALL OMSTI (INPDAT(4))
        CALL OMSTI (INPDAT(5))
        CALL OMSTI (INPDAT(6))
        CALL OMSTI (RETCOD)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
      ELSE
C       process
        IF (FSTCAL .EQ. 1) THEN
C         adjust starting date, and number of values
C
          IF (STKIND(1) .NE. 1) THEN
C           first value not used
            J= MIN0(DELT,DELTAT)
            CALL ADDCDT (J,I1,
     M                   CURDAT,CURTIM)
          END IF
C
C         find offset in intervals, if any
          IF (DELT .EQ. DELTAT) THEN
C           no agg/disagg
            NREM= 0
          ELSE IF (DELT .LT. DELTAT) THEN
C           disaggregate
C
C           convert start time to julian
            MINS= IHM2M (CURTIM)
            CALL DATJUL (CURDAT,
     O                   JULS,I)
            CALL ZOFSET (JULS,MINS,DELTAT,I0,
     O                   IOFFST)
            AGGRFG= IOFFST/DELT- 1
C           find number of values to disaggregate
C
C           get starting month and year
            CALL DATYMD (CURDAT,
     O                   SYEAR,SMONTH,I,I)
C
C           compute  initial nrem
            IF (DELTAT .LE. 11000) THEN
C             constant deltat
              NREM= DELTAT/DELT
            ELSE IF (DELTAT .LE. 50000) THEN
C             monthly
              NREM= (1440 * DYPMON (SYEAR,SMONTH)) / DELT
            ELSE
C             yearly
              NREM= VYDELT (SYEAR) / DELT
            END IF
C           account for offset
            IF (AGGRFG .GE. 0) THEN
C             offset was nonzero
              NREM= NREM- AGGRFG
            ELSE
C             offset was zero
              AGGRFG= NREM- 1
              NREM= 1
            END IF
C
          ELSE
C           aggregate
            IF (STKIND(1) .EQ. 1) THEN
C             point target, first value is single
              AGGRFG= 1
              NREM= 1
            ELSE
C             mean target, first value is aggregated normally
              AGGRFG= 0
              NREM= 0
            END IF
          END IF
        END IF
C
C       find number of values and starting offset in pad
        IF ( (FSTCAL .EQ. 1) .AND. (STKIND(1) .EQ. 1) ) THEN
          NPTS= 0
          WID= WIDTH
        ELSE
          NPTS= 1
          WID= WIDTH- 1
        END IF
C
C       calculate number of calls to dssget
        NGETS= INT ((WID- 1)/500)+ 1
C
C       call dssget and place data on pad - repeat ngets times
        DO 60 I= 1, NGETS
C
C         number of values each call
          IF (I .EQ. NGETS) THEN
            NVALS= WID- (NGETS- 1)*500
          ELSE
            NVALS= 500
          END IF
C
          IF (NVALS .GT. 0) THEN
C           get values from dss file
            LNREM= NREM
            LAGGFG= AGGRFG
            CALL DSSGET (FILE,CPATH,DELT,DELTAT,CURDAT,CURTIM,
     I                   GAPVAL,NVALS,DTRAN,
     M                   LNREM,LAGGFG,
     O                   TBUFF,CUNITS,CDTYPE,RETCOD)
C
            FSTFG= 1
            IF (RETCOD .GT. 10) THEN
C             error - program bug
              CALL OMSTI (NVALS)
              CALL OMSTI (FILE)
              CALL OMSTC (I4,CURTIM)
              CALL OMSTC (I20,CURDAT)
              CALL OMSTC (I64,CPATH)
              CALL OMSTI (RETCOD)
              SGRP= 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   KNT)
            ELSE IF (RETCOD .NE. 0) THEN
C             warn user about missing values
              CALL OMSTI (NVALS)
              CALL OMSTI (FILE)
              CALL OMSTC (I4,CURTIM)
              CALL OMSTC (I20,CURDAT)
              CALL OMSTC (I64,CPATH)
              CALL OMSTI (RETCOD)
              SGRP= 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   KNT)
            END IF
            NREM= LNREM
            AGGRFG= LAGGFG
C
C           transform and place data values on inpad
            DO 40 J= 1, NVALS
C
              IF (FSTFG .EQ. 0) THEN
C               first data pt not available
                IF (J .EQ. 1) THEN
C                 this is the first data pt
                  IF ((STKIND(1) .EQ. 1) .OR. (STKIND(1) .EQ. 3)) THEN
C                   point time series, use second pt
                    CURVAL(1)= TBUFF(1)
                  ELSE
C                   mean time series, use undefined value
                    CURVAL(1)= -1.0E+30
                  END IF
                ELSE
C                 all other data pts
                  CURVAL(1)= TBUFF(J - 1)
                END IF
              ELSE
C               first data pt was available
                CURVAL(1)= TBUFF(J)
              END IF
C
              IF ( (LTRNFG .EQ. 1) .AND. (CURVAL(1) .GT. -1.E29) ) THEN
C               a linear transformation is needed
                CALL LTRAN (I1,A,B,
     M                      CURVAL)
              END IF
              NPTS= NPTS + 1
              PLACE= VOPADR(1)+ NPTS
              IF (PLACE .GT. SCRSIZ) THEN
C               error - pad overflow
                CALL OMSTD (INPDAT)
                CALL OMSTI (VOPADR(1))
                CALL OMSTI (NPTS)
                CALL OMSTI (SCRSIZ)
                CALL OMSTI (PLACE)
                SGRP= 4
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     KNT)
              END IF
              IF (INMODE(1) .EQ. 1) THEN
C               add values to current contents of row
                PAD(PLACE)= PAD(PLACE) + CURVAL(1)
                IF (TESTFG .GE. 2) THEN
                  WRITE (MESSU,2300) PLACE,PAD(PLACE)
                END IF
              ELSE
C               replace current contents of row with values
                PAD(PLACE)= CURVAL(1)
                IF (TESTFG .GE. 2) THEN
                  WRITE (MESSU,2300) PLACE,PAD(PLACE)
                END IF
              END IF
 40         CONTINUE
C
C           increment time to start of next set of values
            IF ( (STKIND(1) .EQ. 1) .AND. (FSTCAL .EQ. 1) .AND.
     #           (I .EQ. 1) .AND. (DELT .GT. DELTAT) ) THEN
C             correct start date for point time series
              CALL ADDCDT (DELTAT,I1,
     M                     CURDAT,CURTIM)
              NVALS= NVALS- 1
            ELSE IF (FSTFG .EQ. 0) THEN
C             start date already moved up by one delt
              NVALS= NVALS- 1
            END IF
            CALL ADDCDT (DELT,NVALS,
     M                   CURDAT,CURTIM)
C
          END IF
C
 60     CONTINUE
C
C       convert ending time to HSPF-style date and update instruction
        CALL CH2TIM (CURDAT,CURTIM,
     O               INPDAT,RETCOD)
      END IF
C
      IF (TESTFG .GE. 1) THEN
        WRITE (MESSU,3010) NGETS,NVALS,NPTS
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSSGET
     I                    (FILE,CPATH,DELT,DELTAT,SDATE,STIME,
     I                     GAPVAL,NVALS,DTRAN,
     M                     NREM,AGGRFG,
     O                     RVAL,CUNITS,CDTYPE,RETCOD)
C
C     + + + PURPOSE + + +
C     get timeseries data from a DSS data record
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FILE,DELT,DELTAT,NVALS,DTRAN,NREM,AGGRFG,RETCOD
      REAL         GAPVAL,RVAL(NVALS)
      CHARACTER*4  STIME
      CHARACTER*8  CUNITS,CDTYPE
      CHARACTER*20 SDATE
      CHARACTER*80 CPATH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILE   - DSS file index number assigned in FILES block
C     CPATH  - pathname for data record
C     DELT   - time step in minutes
C     DELTAT - time interval of data
C     SDATE  - starting date in DSS character format
C     STIME  - starting time in minutes
C     GAPVAL - value to fill missing data
C     NVALS  - number of values to retrieve
C     DTRAN  - transformation code
C              0 - ave (aggregate) or same (disaggregate)
C              1 - sum (aggregate) or div (disaggregate)
C              2 - min
C              3 - max
C     NREM   - number of values remaining to disaggregate, (or -1 for first
C              value single); or aggregate flag: 0 - first value normal,
C              1 - first value single
C     AGGRFG - on disaggregate, number of values disaggregated so far
C     RVAL   - array to contain values
C     CUNITS - unit attribute of data
C     CDTYPE - data type
C     RETCOD - return code
C
C     + + + PARAMETERS + + +
      INTEGER      MXARRY
      PARAMETER   (MXARRY = 1440)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I4,I20,I,RPOS,IRATIO,RET,TPOS,MISSFG,SPAN,NGET,SYEAR,
     $             SMONTH,YEAR,MONTH,N,VDELT
      REAL         TMPVAL(MXARRY),RATIO
      CHARACTER*4  LCTIME
      CHARACTER*20 LCDATE
C
C     + + + FUNCTIONS + + +
      REAL         FLOAT,MIN,MAX
      INTEGER      INT,MAX0,DYPMON,VYDELT
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT,INT,MAX0,MIN,MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL     COPYC,ZRRTS,ADDCDT,DATYMD,DYPMON,VYDELT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I4,I20/4,20/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
C     make local copy of date and time
      CALL COPYC (I20,SDATE,
     O            LCDATE)
      CALL COPYC (I4,STIME,
     O            LCTIME)
C
      IF (DELT .EQ. DELTAT) THEN
C       no transformation needed
        N= NVALS
        CALL ZRRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,
     M              N,
     O              RVAL,CUNITS,CDTYPE,I,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         fill in missing values
          DO 10 I= 1, NVALS
            IF ( (RVAL(I) .EQ. -901.0) .OR.
     #           (RVAL(I) .EQ. -902.0) ) THEN
C             fill missing value
              RVAL(I)= GAPVAL
            END IF
 10       CONTINUE
        END IF
      ELSE IF (DELT .GT. DELTAT) THEN
C       aggregate
C
C       compute ratio in real and integer forms
        RATIO= FLOAT (DELT) / FLOAT (DELTAT)
        IRATIO= INT (RATIO+ 0.5)
C
C       begin loop to read and aggregate
        RPOS= 1
 20     CONTINUE
C
          IF (NREM .EQ. 1) THEN
C           read first value of point series not aggregated
            N= 1
          ELSE
C           read enough data to make one delt
            N= IRATIO
          END IF
          CALL ZRRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,
     M                N,
     O                TMPVAL,CUNITS,CDTYPE,I,RET)
          RETCOD= MAX0 (RETCOD,RET)
C
          IF (NREM .EQ. 1) THEN
C           first value of point time series is not aggregated
            RVAL(RPOS)= TMPVAL(1)
          ELSE
C           aggregate values
C
C           set initial value
            IF (DTRAN .EQ. 2) THEN
C             min
              RVAL(RPOS)= 1.0E30
            ELSE IF (DTRAN .EQ. 3) THEN
C             max
              RVAL(RPOS)= -1.0E30
            ELSE
C             aver or sum
              RVAL(RPOS)= 0.0
            END IF
            MISSFG= 0
C
C           loop on values read
            DO 30 TPOS= 1, IRATIO
C
              IF ( ( GAPVAL .LT. 0.0) .AND. (RET .NE. 0) ) THEN
C               check for missing values
                IF ( (TMPVAL(TPOS) .EQ. -901.0) .OR.
     #               (TMPVAL(TPOS) .EQ. -902.0) ) THEN
C                 missing value found
                  MISSFG= 1
                END IF
              END IF
C             aggregate single value
              IF (DTRAN .EQ. 0) THEN
C               aver
                RVAL(RPOS)= RVAL(RPOS)+ TMPVAL(TPOS)/RATIO
              ELSE IF (DTRAN .EQ. 1) THEN
C               sum
                RVAL(RPOS)= RVAL(RPOS)+ TMPVAL(TPOS)
              ELSE IF (DTRAN .EQ. 2) THEN
C               min
                RVAL(RPOS)= MIN (RVAL(RPOS),TMPVAL(TPOS))
              ELSE IF (DTRAN .EQ. 2) THEN
C               max
                RVAL(RPOS)= MAX (RVAL(RPOS),TMPVAL(TPOS))
              END IF
 30         CONTINUE
            IF (MISSFG .EQ. 1) THEN
C             aggregated value is missing
              RVAL(RPOS)= GAPVAL
            END IF
          END IF
C
C         update for next call
          RPOS= RPOS+ 1
          IF (NREM .EQ. 1) THEN
C           read first value of point series not aggregated
            N= 1
            NREM= 0
          ELSE
C           read enough data to make one delt
            N= IRATIO
          END IF
          CALL ADDCDT (DELTAT,N,
     M                 LCDATE,LCTIME)
C
C         end of do-until loop
        IF (RPOS .LE. NVALS) GO TO 20
C
      ELSE
C       disaggregate
C
C       compute offset of date,time from beginning of data interval
C
C       find starting year and month
        CALL DATYMD (LCDATE,
     O               SYEAR,SMONTH,I,I)
C
C       compute ratio if constant (weekly or shorter interval)
        IF (DELTAT .LE. 11000) THEN
C         compute ratio in real and integer forms
          RATIO= FLOAT (DELTAT) / FLOAT (DELT)
          IRATIO= INT (RATIO + 0.5)
        ELSE IF (DELTAT .LE. 50000) THEN
C         monthly initial ratio
          VDELT= 1440 * DYPMON (SYEAR,SMONTH)
          RATIO= FLOAT (VDELT) / FLOAT (DELT)
        ELSE
C         yearly initial ratio
          VDELT= VYDELT (SYEAR)
          RATIO= FLOAT (VDELT) / FLOAT (DELT)
        END IF
C
C       compute number of values to cover or exceed span including
C       offset
        SPAN= DELT*(NVALS+ AGGRFG)
        IF (DELTAT .LE. 11000) THEN
C         compute from constant data interval
          NGET= (SPAN- 1)/DELTAT+ 1
        ELSE
C         count off enough monthly or yearly values
          NGET= 0
          YEAR= SYEAR
          MONTH= SMONTH
 40       CONTINUE
            NGET= NGET+ 1
            IF (DELTAT .LE. 50000) THEN
C             count off one month
              SPAN= SPAN- (1440 * DYPMON (YEAR,MONTH))
              MONTH= MONTH+ 1
              IF (MONTH .GT. 12) THEN
                MONTH= 1
                YEAR= YEAR+ 1
              END IF
            ELSE
C             yearly - deltat is 365 days worth of minutes
              SPAN= SPAN- VYDELT (YEAR)
              YEAR= YEAR+ 1
            END IF
C
C         done when span is zero or negative
          IF (SPAN .GT. 0) GO TO 40
C
        END IF
C
C       get data
        N= NGET
        CALL ZRRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,
     M              N,
     O              TMPVAL,CUNITS,CDTYPE,I,RETCOD)
C
C       disaggregate values
C
        YEAR= SYEAR
        MONTH= SMONTH
        RPOS= 1
        TPOS= 1
C
C       loop on values read
 50     CONTINUE
C
          MISSFG= 0
          IF (RETCOD .NE. 0) THEN
C           check current value for missing
            IF ( (TMPVAL(TPOS) .EQ. -901.0) .OR.
     #           (TMPVAL(TPOS) .EQ. -902.0) ) THEN
C             set missing flag
              MISSFG= 1
            END IF
          END IF
C
C         disaggregate a single value
          IF (MISSFG .EQ. 1) THEN
C           missing value, so fill with gapval regardless of
C           transform functional
            RVAL(RPOS)= GAPVAL
          ELSE IF (DTRAN .EQ. 1) THEN
C           div
            RVAL(RPOS)= TMPVAL(TPOS)/RATIO
          ELSE
C           same
            RVAL(RPOS)= TMPVAL(TPOS)
          END IF
C
C         update disaggregation information
          NREM= NREM- 1
          AGGRFG= AGGRFG+ 1
          IF (NREM .LE. 0) THEN
C           done disaggregating a value
            TPOS= TPOS+ 1
            AGGRFG= 0
C
C           reinitialize nrem
            IF (DELTAT .LE. 11000) THEN
C             use constant ratio
              NREM= IRATIO
            ELSE IF (DELTAT .LE. 50000) THEN
C             monthly variable deltat
              MONTH= MONTH+ 1
              IF (MONTH .GT. 12) THEN
                MONTH= 1
                YEAR= YEAR+ 1
              END IF
              VDELT= 1440 * DYPMON (YEAR,MONTH)
              NREM= VDELT / DELT
              RATIO= FLOAT (VDELT) / FLOAT (DELT)
            ELSE
C             yearly variable deltat
              YEAR= YEAR+ 1
              VDELT= VYDELT (YEAR)
              NREM= VDELT / DELT
              RATIO= FLOAT (VDELT) / FLOAT (DELT)
            END IF
          END IF
C
C         update tpos
          RPOS= RPOS+ 1
C
        IF (RPOS .LE. NVALS) GO TO 50
C
      END IF
C
      RETURN
      END
