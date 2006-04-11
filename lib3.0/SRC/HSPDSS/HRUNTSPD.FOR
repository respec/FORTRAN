C
C
C
      SUBROUTINE PUTDSS
     I                  (DELT,WIDTH,FSTCAL)
C
C     + + + PURPOSE + + +
C     Put values to dss file from the inpad after transforming them as
C     necessary.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,WIDTH,FSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
C     FSTCAL - flag indicating first time span
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,DTRAN,RETCOD,SGRP,MINS,JULS,I,I0,IOFFST,SYEAR,
     #             SMONTH,NPUTS,NPTS,NVALS,J,PLACE,I1,I8,I80,WID
      CHARACTER*1  BLANK
      CHARACTER*4  CURTIM
      CHARACTER*8  CDTYPE,CUNITS
      CHARACTER*20 CURDAT
      CHARACTER*80 CPATH
C
C     + + + FUNCTIONS + + +
      INTEGER      IHM2M,DYPMON,VYDELT
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZIPC,TIM2CH,OMSTI,OMSG,IHM2M,DATJUL,ZOFSET,DATYMD
      EXTERNAL     DYPMON,VYDELT,LTRAN,DSSPUT,ADDCDT,CH2TIM,OMSTD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I8,I80/0,1,8,80/
      DATA BLANK/' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (16A4)
 3000 FORMAT (/,' ENTERING PUTDSS')
 3010 FORMAT (/,' LEAVING PUTDSS',
     $          '   NPUTS=',I6,';  NVALS=',I6,';  NPTS=',I6)
 3020 FORMAT (3X,'INPAD DELTA TIME=',I6,3X, 'DSS DELTAT TIME=',I6,
     $        3X,'WIDTH=',I12,/,3X,
     $        'INPAD START TIME (MIN)=',I10,/,3X,
     $        'INPAD START TIME (YR,MO,DY,ETC)=',6I6)
 3030 FORMAT (3X,'VOPADR=',I6,3X,'OFFSET=',I6,3X,'STKIND=',I6,/,
     $        3X,'STTRAN=',I6,3X,'INMODE=',I6,/,3X,'A=',E18.6,6X,
     $        'B=',E18.6)
 3040 FORMAT (' INPDAT:',I4,5I3,' CURDAT-',A20,'- CURTIM-',A8,
     $        '- RETCOD',I4)
 3050 FORMAT (' CALLING DSSPUT',/,'FILE=',I2,' CUNITS-',A8,'- CDTYPE-',
     $         A8,'- DELT=',I5,' DELTAT=',I8,' NVALS=',I8,' DTRAN=',I2,
     $         /,'NREM=',I8,' AGGVAL=',G12.6,' CURDAT-',A20,
     $        '- CURTIM-',A4,'-',/,'CPATH-',A80,'-')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 240
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
      WRITE (CPATH,2000) (PVAR(I),I= 1, 16)
C
C     convert data type to character variable
      CALL ZIPC (I8,BLANK,
     O           CDTYPE)
      WRITE (CDTYPE,2000) (CTYPI(I),I= 1, 2)
C
C     create dummy units string
      CALL ZIPC (I8,BLANK,
     O           CUNITS)
C
C     initialize inpad time to start time for inpad
      CALL TIM2CH (INPDAT,
     O             CURDAT,CURTIM,RETCOD)
      IF (TESTFG .GE. 1) THEN
        WRITE (MESSU,3040) INPDAT,CURDAT,CURTIM,RETCOD
      END IF
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
C         compute offset of date,time from beginning of data interval
C
          IF (STKIND(1) .NE. 1) THEN
C           target is mean - must add one deltat to starting time
            CALL ADDCDT (DELTAT,I1,
     M                   CURDAT,CURTIM)
          END IF
C
C         convert start time to julian
          MINS= IHM2M (CURTIM)
          CALL DATJUL (CURDAT,
     O                JULS,I)
C
C         find offset in intervals, if any
          IF (DELT .EQ. DELTAT) THEN
C           no agg/disagg
            NREM= 1
          ELSE IF (DELT .LT. DELTAT) THEN
C           aggregate
            CALL ZOFSET (JULS,MINS,DELTAT,I0,
     O                   IOFFST)
            IF (IOFFST .GT. 0) THEN
C             first value is not full value
              NREM= IOFFST/DELT
            ELSE
C             find number of values to aggregate
C
C             get starting month and year
              CALL DATYMD (CURDAT,
     O                     SYEAR,SMONTH,I,I)
C
C             compute initial nrem
              IF (DELTAT .LE. 11000) THEN
C               constant deltat
                NREM= DELTAT/DELT
              ELSE IF (DELTAT .LE. 50000) THEN
C               monthly
                NREM= (1440 * DYPMON (SYEAR,SMONTH)) / DELT
              ELSE
C               yearly
                NREM= VYDELT (SYEAR) / DELT
              END IF
            END IF
          ELSE
C           disaggregate
            IF (STKIND(1) .EQ. 1) THEN
C             for point, first value is not disaggregated, but is single
              NREM= 1
            ELSE
C             first value is fully disaggregated, zero value is flag
              NREM= 0
            END IF
          END IF
C
C         no previous incomplete value
          AGGRFG= 0
C
        END IF
C
C       find number of values and starting offset in pad
        IF ( (FSTCAL .EQ. 1) .AND. (STKIND(1) .EQ. 1) ) THEN
C         point target - first value is used
          NPTS= 0
          WID= WIDTH
        ELSE
C         first value not used
          NPTS= 1
          WID= WIDTH- 1
        END IF
C
C       calculate number of calls to dssget
        NPUTS= INT ((WID-1)/500)+ 1
C
C       call dssput - repeat nputs times
        DO 60 I= 1, NPUTS
C
C         number of values each call
          IF (I .EQ. NPUTS) THEN
            NVALS= WID- (NPUTS- 1)*500
          ELSE
            NVALS= 500
          END IF
C
          IF (NVALS .GT. 0) THEN
C
C           get values from inpad and transform
            DO 40 J= 1, NVALS
C
C             find place in pad
              NPTS= NPTS+ 1
              PLACE= VOPADR(1)+ NPTS
              IF (PLACE .GT. SCRSIZ) THEN
C               error - pad overflow
                CALL OMSTD (INPDAT)
                CALL OMSTI (VOPADR(1))
                CALL OMSTI (NPTS)
                CALL OMSTI (SCRSIZ)
                CALL OMSTI (PLACE)
                SGRP= 2
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     KNT)
              END IF
C
C             fetch value
              IF (IPAD(PLACE) .EQ. -999) THEN
C               value has not been defined
                TBUFF(J)= -1.0E30
              ELSE
C               value ok
                TBUFF(J)= PAD(PLACE)
              END IF
C
C             transform if necessary
              IF ( (LTRNFG .EQ. 1) .AND. (TBUFF(J) .GT. -1.E29) ) THEN
C               a linear transformation is needed
                CALL LTRAN (I1,A,B,
     M                      TBUFF(J))
              END IF
C
 40         CONTINUE
C
C
            IF (TESTFG .GE. 1) THEN
              WRITE (MESSU,3050) FILE,CUNITS,CDTYPE,DELT,DELTAT,NVALS,
     #                           DTRAN,NREM,PVAR(20),CURDAT,CURTIM,
     #                           CPATH
            END IF
C
C           put values from dss file
            CALL DSSPUT (FILE,CPATH,CUNITS,CDTYPE,DELT,DELTAT,CURDAT,
     I                   CURTIM,NVALS,DTRAN,TBUFF,
     M                   NREM,PVAR(20),AGGRFG,
     O                   RETCOD)
C
C           increment time to start of next set of values
            IF ( (DELT .GT. DELTAT) .AND.
     #           (STKIND(1)*I*FSTCAL .EQ. 1) ) THEN
C             back up for first interval of point time series one delt,
C             and correct starting time as already done for mean ts
              NVALS= NVALS- 1
              CALL ADDCDT (DELTAT,I1,
     M                     CURDAT,CURTIM)
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
        WRITE (MESSU,3010) NPUTS,NVALS,NPTS
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSSPUT
     I                    (FILE,CPATH,CUNITS,CDTYPE,DELT,DELTAT,SDATE,
     I                     STIME,NVALS,DTRAN,RVAL,
     M                     NREM,AGGVAL,AGGRFG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     get timeseries data from a DSS data record
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FILE,DELT,DELTAT,NVALS,DTRAN,NREM,AGGRFG,RETCOD
      REAL         RVAL(NVALS),AGGVAL
      CHARACTER*4  STIME
      CHARACTER*8  CUNITS,CDTYPE
      CHARACTER*20 SDATE
      CHARACTER*80 CPATH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILE   - DSS file index number assigned in FILES block
C     CPATH  - pathname for data record
C     CUNITS - unit string for data record
C     CDTYPE - data type for data record: PER-AVER, PER-CUM, INST-VAL
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
C     RVAL   - array to contain values
C     NREM   - number of values remaining to aggregate, or flag to
C            - disaggregate first value if DELT > DELTAT
C     AGGVAL - current aggregated value
C     AGGRFG - number of values aggregated so far
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
      INTEGER      I0,RPOS,TPOS,RET,IRATIO,SYEAR,SMONTH,I,VDELT,YEAR,
     #             MONTH,I4,I20,N
      REAL         TMPVAL(MXARRY),RATIO
      CHARACTER*4  LCTIME
      CHARACTER*20 LCDATE
C
C     + + + FUNCTIONS + + +
      INTEGER      DYPMON,VYDELT
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT,INT,MAX0,MAX,MIN
C
C     + + + EXTERNALS + + +
      EXTERNAL     COPYC,ZSRTS,ADDCDT,DATYMD,DYPMON,VYDELT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I4,I20/0,4,20/
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
C       quick put
        CALL ZSRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,NVALS,RVAL,
     I              CUNITS,CDTYPE,I0,
     O              RETCOD)
      ELSE IF (DELT .GT. DELTAT) THEN
C       disaggregate
C
C       compute ratio in real and integer forms
        RATIO= FLOAT (DELT) / FLOAT (DELTAT)
        IRATIO= INT (RATIO+ 0.5)
C
C       begin loop to disaggregate and write
        DO 20 RPOS= 1, NVALS
C
C         disaggregate single value
C
          IF ( (RPOS .EQ. 1) .AND. (NREM .EQ. 1) ) THEN
C           first value is not disaggregated for point, reset flag
            N= 1
            NREM= 0
          ELSE
C           disaggregate
            N= IRATIO
          END IF
C
          DO 10 TPOS= 1, N
            IF (DTRAN .EQ. 0) THEN
C             same
              TMPVAL(TPOS)= RVAL(RPOS)
            ELSE IF (DTRAN .EQ. 1) THEN
C             div
              TMPVAL(TPOS)= RVAL(RPOS)/RATIO
            END IF
 10       CONTINUE
C
C         write values
          CALL ZSRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,N,TMPVAL,
     I                CUNITS,CDTYPE,I0,
     O                RET)
          RETCOD= MAX0 (RET,RETCOD)
C
C         update for next call
          CALL ADDCDT (DELTAT,N,
     M                 LCDATE,LCTIME)
C
 20     CONTINUE
C
      ELSE
C       aggregate
C
C       back up starting date to beginning of aggregation period
        IF (AGGRFG .GT. 0) THEN
          I= -AGGRFG
          CALL ADDCDT (DELT,I,
     M                 LCDATE,LCTIME)
        END IF
C
C       find starting year and month
        CALL DATYMD (LCDATE,
     O               SYEAR,SMONTH,I,I)
C
C       compute ratio, and integer form if constant
        IF (DELTAT .LE. 11000) THEN
C         compute ratio in real and integer forms
          RATIO= FLOAT (DELTAT) / FLOAT (DELT)
          IRATIO= INT (RATIO + 0.5)
        ELSE IF (DELTAT .LE. 50000) THEN
C         monthly initial ratio
          VDELT= 1440* DYPMON (SYEAR,SMONTH)
          RATIO= FLOAT (VDELT) / FLOAT (DELT)
        ELSE
C         yearly initial ratio
          VDELT= VYDELT (SYEAR)
          RATIO= FLOAT (VDELT) / FLOAT (DELT)
        END IF
C
C       aggregate values
C
        YEAR= SYEAR
        MONTH= SMONTH
        TPOS= 1
        RPOS= 1
        TMPVAL(1)= AGGVAL
C
C       begin loop on values
 30     CONTINUE
C
C         aggregate single value
          IF (DTRAN .EQ. 0) THEN
C           aver
            TMPVAL(TPOS)= TMPVAL(TPOS)+ RVAL(RPOS)/RATIO
          ELSE IF (DTRAN .EQ. 1) THEN
C           sum
            TMPVAL(TPOS)= TMPVAL(TPOS)+ RVAL(RPOS)
          ELSE IF (DTRAN .EQ. 2) THEN
C           max
            TMPVAL(TPOS)= MAX (RVAL(RPOS),TMPVAL(TPOS))
          ELSE IF (DTRAN .EQ. 3) THEN
C           min
            TMPVAL(TPOS)= MIN (RVAL(RPOS),TMPVAL(TPOS))
          END IF
C
C         update aggregation information
          NREM= NREM- 1
          AGGRFG= AGGRFG+ 1
          IF (NREM .LE. 0) THEN
C           done aggregating a value
            TPOS= TPOS+ 1
            AGGRFG= 0
C
C           initialize next value
            IF (DTRAN .EQ. 3) THEN
C             min
              TMPVAL(TPOS)= 1.0E30
            ELSE IF (DTRAN .EQ. 2) THEN
C             max
              TMPVAL(TPOS)= -1.0E30
            ELSE
C             sum or aver
              TMPVAL(TPOS)= 0.0
            END IF
C
C           reinitialize nrem
            IF (DELTAT .LE. 11000) THEN
C             constant deltat
              NREM= IRATIO
            ELSE IF (DELTAT .LE. 50000) THEN
C             monthly variable deltat
              MONTH= MONTH+ 1
              IF (MONTH .GT. 12) THEN
C               new year
                MONTH= 1
                YEAR= YEAR+ 1
              END IF
              VDELT= 1440* DYPMON (YEAR,MONTH)
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
C         update rpos
          RPOS= RPOS+ 1
C
C       end loop
        IF (RPOS .LE. NVALS) GO TO 30
C
C       update aggval
        AGGVAL= TMPVAL(TPOS)
C
        IF (AGGRFG .EQ. 0) THEN
C         last value is still empty, and doesn't need to be written
          TPOS= TPOS- 1
        END IF
C
C       write values
        CALL ZSRTS (IFLTAB(1,FILE),CPATH,LCDATE,LCTIME,TPOS,TMPVAL,
     I              CUNITS,CDTYPE,I0,
     O              RETCOD)
C       
      END IF
C
      RETURN
      END
