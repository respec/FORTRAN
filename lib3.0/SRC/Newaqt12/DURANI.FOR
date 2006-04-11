C
C
C
      SUBROUTINE   DURANI
     I                   (MESSFL,WDMFL,
     I                    CSCENM,CLOCNM,CCONNM,
     M                    INITFG,DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     Control the calculation of flow-duration statistics,
C     flow-duration tables and flow-duration plots.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,WDMFL,DSNCNT,DSNBMX,DSNBUF(DSNBMX),INITFG
      CHARACTER*8  CSCENM,CLOCNM,CCONNM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C     INITFG - initialize duration analysis flag
C     CSCENM - scenario name
C     CLOCNM - location name
C     CCONNM - constituent name
C
C     + + + PARAMETERS + + +
      INCLUDE   'pmxdur.inc'
      INCLUDE   'pbfmax.inc'
C
C     + + + SAVES + + +
      INTEGER   NCINT,NDURA,NCRIT,PRFG,LCOUT,PUNIT(1),DURAT(MXDUR),
     1          LCGTLT,OPVAL(2)
      SAVE      NCINT,NDURA,NCRIT,PRFG,LCOUT,PUNIT,DURAT,LCGTLT,OPVAL
      CHARACTER*80 DURNAM
      CHARACTER*64 FLNAME(1)
      SAVE         DURNAM,FLNAME
      REAL      RCLINT(MXLEV),RLCLEV(MXLC,MXLEV),RLOHI(2)
      SAVE      RCLINT,RLCLEV,RLOHI
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      SCLU,SGRP,RESP,I1,I4,I0,INUM,IVAL(3),IRET,CNUM,IM1,
     1             CLEN(1),OPCNT,OPLEN,MXSEL(1),MNSEL(1),CVAL(1),PROBFG,
     2             RNUM,IVAL2(2,MXDUR),NROW,IVAL1(1,MXLEV),IRET2,PREVFG,
     4             ACT,SSDATE(2),SEDATE(2),SDATE(6),EDATE(6),TU,TS,
     5             NUM,LCTSTO(MXLC),ANALDN,IDUM,J,N,L,I,DTRAN,I2,DONFG,
     6             NPTS
      REAL         R0,
     2             FRQNW(MXDUR),SNW(MXDUR),SQNW(MXDUR),
     3             FRQPOS(MXDUR,MXLEV),SPOS(MXDUR,MXLEV),
     4             SQPOS(MXDUR,MXLEV),FRQNEG(MXDUR,MXLEV),
     5             SNEG(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),
     6             MNW(MXDUR),MPOS(MXDUR,MXLEV),MNEG(MXDUR,MXLEV),
     7             PTNW(MXDUR),PTPOS(MXDUR,MXLEV),PTNEG(MXDUR,MXLEV),
     8             PT1NW(MXDUR),PT1POS(MXDUR,MXLEV),
     9             PT1NEG(MXDUR,MXLEV),FREVNW(MXDUR),
     1             FREVPS(MXDUR,MXLEV),FREVNG(MXDUR,MXLEV),
     2             DELT,MAX,MINIM,MEAN,SUMSQ,CR,CLOG,C
      CHARACTER*1  CTXT1(80),HEAD1(78)
      CHARACTER*8  PTHNAM(1),CDSID
      CHARACTER*78 CHEAD
C
C     + + + INTRINSICS + + +
      INTRINSIC    ALOG10, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, SGDATE, ZSTCMA, Q1INIT, Q1EDIT, QSETI, QGETI
      EXTERNAL     QSETCT, QGETCT, CVARAR, QSETOP, QGETOP, QSETR, QGETR
      EXTERNAL     Q2INIT, Q2SETI, Q2GETI, Q2EDIT, Q2SETR, Q2GETR, ZIPR
      EXTERNAL     PRNTXT, DARSLT, CARVAR, DTACT, DTGET, DAANAL, TIMDIF
      EXTERNAL     QSETFN, QGETF, PMXCNW, ZBLDWR, ZMNSST, QGTCOB,QSTCOB
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I2 = 2
      I1 = 1
      I4 = 4
      IM1= -1
      R0 = 0.0
C
      ANALDN= 0
      IF (INITFG.EQ.1) THEN
C       yes, do initializations
        NCINT = 1
        NDURA = 1
        NCRIT = 0
        DURNAM= '<none>'
        PRFG  = 7
        LCOUT = 3
        PUNIT(1)  = -99
        FLNAME(1) = 'DURANL.OUT'
        CALL ZIPR (MXLEV,R0,RCLINT)
        DO 5 I = 1,MXDUR
C         initialize duration time intervals
          DURAT(I) = I
 5      CONTINUE
        I = MXLEV*MXLC
        CALL ZIPR (I,R0,RLCLEV)
        OPVAL(1) = 1
        OPVAL(2) = 0
        RLOHI(1) = 1.0
        RLOHI(2) = 10000.0
        LCGTLT   = 1
        INITFG   = 0
      END IF
C
C     message file cluster used
      SCLU= 159
C
 10   CONTINUE
C       do header line including scen,loc,cons
        CHEAD = ' Select a Duration Option for ' //
     1           CSCENM // ' ' // CCONNM // ' at ' // CLOCNM //':'
        I = 78
        CALL CVARAR (I,CHEAD,I,HEAD1)
        CALL ZBLDWR (I,HEAD1,I1,IM1,DONFG)
        CALL ZMNSST
C       do main duration menu
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (50,100,200,300,400,500,600,700,800,900), RESP
C
 50     CONTINUE
C         initialize a new duration analysis
          NCINT = 1
          NDURA = 1
          NCRIT = 0
          DURNAM= '<none>'
          PRFG  = 7
          LCOUT = 3
          PUNIT(1)  = -99
          FLNAME(1) = 'DURANL.OUT'
          CALL ZIPR (MXLEV,R0,RCLINT)
          DO 7 I = 1,MXDUR
C           initialize duration time intervals
            DURAT(I) = I
 7        CONTINUE
          I = MXLEV*MXLC
          CALL ZIPR (I,R0,RLCLEV)
          OPVAL(1) = 1
          OPVAL(2) = 0
          RLOHI(1) = 1.0
          RLOHI(2) = 10000.0
          LCGTLT   = 1
          GO TO 999
C
 100    CONTINUE
C         modify general parameters
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 10
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set the title field
          CNUM   = 1
          CLEN(1)= 40
          CALL CVARAR (CLEN(1),DURNAM,CLEN(1),CTXT1)
          CALL QSETCT (CNUM,CLEN,CLEN(1),CTXT1)
C         set these values to # of class ints, time ints, crit levels
          INUM= 3
          IVAL(1) = NCINT
          IVAL(2) = NDURA
          IVAL(3) = NCRIT
          CALL QSETI (INUM,IVAL)
          CVAL(1) = LCGTLT
          CALL QSTCOB (I1,I2,CVAL)
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, read specs
            CALL QGETCT (CNUM,CLEN,CLEN,CTXT1)
            CALL CARVAR (CLEN(1),CTXT1,CLEN(1),DURNAM)
            CALL QGETI (INUM,IVAL)
            NCINT = IVAL(1)
            NDURA = IVAL(2)
            NCRIT = IVAL(3)
            CALL QGTCOB (I1,I2,CVAL)
            LCGTLT = CVAL(1)
C           set flag to indicate analysis has not been done
            ANALDN= 0
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 999
C
 200    CONTINUE
C         modify output options
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 20
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set the output file name
          CALL QSETFN (I1,FLNAME)
C         set these values to output levels
          INUM= 2
          IVAL(1) = PRFG
          IVAL(2) = LCOUT
          CALL QSETI (INUM,IVAL)
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, read specs
            CALL QGETF (I1,PUNIT)
            CALL QGETI (INUM,IVAL)
            PRFG = IVAL(1)
            LCOUT= IVAL(2)
C           set flag to indicate analysis has not been done
            ANALDN= 0
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 999
C
 300    CONTINUE
C         specify dates to use
          SCLU = 64
          PTHNAM(1) = 'AD'
          CALL SGDATE (MESSFL,SCLU,PTHNAM)
          SCLU = 159
C         set flag to indicate analysis has not been done
          ANALDN= 0
          GO TO 999
C
 400    CONTINUE
C         class intervals
C         make previous command available
          CALL ZSTCMA (I4,I1)
 410      CONTINUE
            PREVFG= 0
            SGRP  = 40
            CALL Q1INIT (MESSFL,SCLU,SGRP)
C           set option fields for user or standard class intervals
            OPCNT = 1
            OPLEN = 2
            MXSEL(1) = 1
            MNSEL(1) = 1
            CALL QSETOP (OPCNT,OPLEN,MXSEL,MNSEL,OPVAL)
C           set the values for upper and lower class limits
            CALL QSETR (I2,RLOHI)
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue, read specs
              CALL QGETOP (OPLEN,OPVAL)
              IF (OPVAL(2).EQ.1 .OR. OPVAL(1).EQ.2) THEN
C               wants standard class limits, get min/max
                CALL QGETR (I2,RLOHI)
                IF (NCINT.LE.1) THEN
C                 only one threshold, take low value
                  RCLINT(1) = RLOHI(1)
                ELSE
C                 set up class intervals
                  CR = (RLOHI(1)/RLOHI(2))**(1.0/(NCINT-1.0))
                  RCLINT(1) = RLOHI(1)
                  RCLINT(NCINT) = RLOHI(2)
                  J = NCINT-1
                  DO 440 N = 1,J
                    I = NCINT - N
                    RCLINT(I) = RCLINT(I+1)*CR
 440              CONTINUE
C                 round off class intervals
                  DO 450 I = 1,NCINT
                    C = RCLINT(I)
                    CLOG = ALOG10(C) + 0.001
                    IF (CLOG.LT.0.0) CLOG = CLOG - 1
                    L = INT(CLOG)
                    L = L - 1
                    C = (C/(10.0**L)) + 0.5
                    RCLINT(I) = (INT(C))*(10.0**L)
 450              CONTINUE
                END IF
              ELSE
 455            CONTINUE
C                 return here for problem on data entry
                  PROBFG = 0
                  SGRP= 41
                  CALL Q2INIT (MESSFL,SCLU,SGRP)
C                 set the values for user specified class limits
                  INUM= 1
                  DO 460 I = 1,NCINT
                    IVAL1(1,I) = I
 460              CONTINUE
                  CALL Q2SETI (INUM,NCINT,IVAL1)
                  CALL Q2SETR (INUM,NCINT,RCLINT)
                  CALL Q2EDIT (NCINT,IRET2)
                  IF (IRET2.EQ.1) THEN
C                   user wants to continue, read specs
                    CALL Q2GETI (INUM,NCINT,IVAL1)
                    CALL Q2GETR (INUM,NCINT,RCLINT)
                    PREVFG = 0
                    IF (NCINT.GT.1) THEN
C                     need check to make sure in ascending order
                      DO 470 I = 2,NCINT
                        IF (RCLINT(I-1).GT.RCLINT(I)) THEN
C                         greater than next, problem
                          PROBFG = 1
                        END IF
 470                  CONTINUE
                      IF (PROBFG.EQ.1) THEN
C                       tell user about problem
                        SGRP = 42
                        CALL PRNTXT (MESSFL,SCLU,SGRP)
                      END IF
                    END IF
                  ELSE IF (IRET2.EQ.2) THEN
C                   user wants previous screen
                    PREVFG = 1
                  END IF
                IF (PROBFG.EQ.1) GO TO 455
              END IF
C             set flag to indicate analysis has not been done
              ANALDN= 0
            END IF
          IF (PREVFG.EQ.1) GO TO 410
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 999
C
 500    CONTINUE
C         duration time intervals
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 50
          CALL Q2INIT (MESSFL,SCLU,SGRP)
C         set the values for duration time intervals
          INUM= 2
          NROW= NDURA
          DO 510 I = 1,NROW
            IVAL2(1,I) = I
            IVAL2(2,I) = DURAT(I)
 510      CONTINUE
          CALL Q2SETI (INUM,NROW,IVAL2)
          CALL Q2EDIT (NROW,IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, read specs
            CALL Q2GETI (INUM,NROW,IVAL2)
            DO 520 I = 1,NROW
              DURAT(I) = IVAL2(2,I)
 520        CONTINUE
C           set flag to indicate analysis has not been done
            ANALDN= 0
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 999
C
 600    CONTINUE
C         critical levels
          IF (NCRIT.EQ.0) THEN
C           tell user no critical levels specified
            SGRP= 69
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           make previous command available
            CALL ZSTCMA (I4,I1)
            SGRP= 59 + NCRIT
            CALL Q2INIT (MESSFL,SCLU,SGRP)
C           set the values for critical levels for each duration
            INUM= 1
            DO 610 I = 1,NDURA
              IVAL1(1,I) = DURAT(I)
 610        CONTINUE
            RNUM = 5
            CALL Q2SETI (INUM,NDURA,IVAL1)
            CALL Q2SETR (RNUM,NDURA,RLCLEV)
            CALL Q2EDIT (NDURA,IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue, read specs
              CALL Q2GETI (INUM,NDURA,IVAL1)
              CALL Q2GETR (RNUM,NDURA,RLCLEV)
C             set flag to indicate analysis has not been done
              ANALDN= 0
            END IF
C           turn off previous command
            CALL ZSTCMA (I4,I0)
          END IF
          GO TO 999
C
 700    CONTINUE
C         analyze
C         get dates for current active date set
          CALL DTACT (I)
          CALL DTGET (I,
     O                ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O                TS,DTRAN)
C         do duration analysis
C         write message for doing analysis
          SGRP = 70
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,IDUM)
C
C         how many points of data to set
          WRITE(99,*) 'du start:',SDATE,TU,TS
          WRITE(99,*) '     end:',EDATE
          CALL TIMDIF (SDATE,EDATE,TU,TS,
     O                 NPTS)
          WRITE(99,*) '    npts:',NPTS
C
          IF (NPTS.LE.0) THEN
C           no points, tell user
            SGRP= 47
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           we have some points, continue
            IF (NPTS+NPTS+1 .GT. BUFMAX) THEN
C             oops,will blow out buffer
              WRITE(99,*) 'too many points'
              SGRP= 46
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            ELSE
C             okay to do analysis
              CALL DAANAL(DURNAM,MXLEV,NCINT,RCLINT,
     I                    MXDUR,NDURA,DURAT,
     I                    MXLC,NCRIT,RLCLEV,
     I                    SDATE,EDATE,SSDATE,SEDATE,TU,TS,DTRAN,
     I                    WDMFL,DSNBUF(1),
     I                    PRFG,PUNIT(1),LCOUT,LCGTLT,
     O                    NUM,FRQNW,SNW,SQNW,
     O                    FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     O                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O                    DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO)
C             set flag to indicate analysis is done
              ANALDN = 1
            END IF
          END IF
C
          GO TO 999
C
 800    CONTINUE
C         view results
          IF (ANALDN.EQ.0) THEN
C           cannot view results until analysis is done
            SGRP = 71
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           proceed with results viewing
C           get dates for current active date set
            CALL DTACT (I)
            CALL DTGET (I,
     O                  ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O                  TS,DTRAN)
            CALL DARSLT (MESSFL,SCLU,NCINT,NDURA,NCRIT,DURNAM,
     I                   MXLEV,MXDUR,MXLC,
     I                   RCLINT,DURAT,SDATE,EDATE,SSDATE,SEDATE,
     I                   NUM,FRQNW,SNW,SQNW,
     I                   FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     I                   MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     I                   PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     I                   DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO,TU,TS,DTRAN)
          END IF
          GO TO 999
C
 900    CONTINUE
C         all done
          GO TO 999
C
 999    CONTINUE
C
      IF (RESP.NE.10) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   DARSLT
     I                   (MESSFL,SCLU,NCINT,NDURA,NCRIT,DURNAM,
     I                    MXLEV,MXDUR,MXLC,
     I                    RCLINT,DURAT,SDATE,EDATE,SSDATE,SEDATE,
     I                    NUM,FRQNW,SNW,SQNW,
     I                    FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     I                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     I                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     I                    DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO,TU,TS,DTRAN)
C
C     + + + PURPOSE + + +
C     Control the results of duration analysis, output to screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,NCINT,NDURA,NCRIT,DURAT(10),
     1          SDATE(6),EDATE(6),SSDATE(2),SEDATE(2),NUM,
     2          MXLEV,MXDUR,MXLC,LCTSTO(MXLC),TU,TS,DTRAN
      REAL      RCLINT(35),
     1          FRQNW(MXDUR),SNW(MXDUR),SQNW(MXDUR),
     2          FRQPOS(MXDUR,MXLEV),SPOS(MXDUR,MXLEV),
     3          SQPOS(MXDUR,MXLEV),FRQNEG(MXDUR,MXLEV),
     4          SNEG(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),
     5          MNW(MXDUR),MPOS(MXDUR,MXLEV),MNEG(MXDUR,MXLEV),
     6          PTNW(MXDUR),PTPOS(MXDUR,MXLEV),PTNEG(MXDUR,MXLEV),
     7          PT1NW(MXDUR),PT1POS(MXDUR,MXLEV),
     8          PT1NEG(MXDUR,MXLEV),FREVNW(MXDUR),
     9          FREVPS(MXDUR,MXLEV),FREVNG(MXDUR,MXLEV),
     1          DELT,MAX,MINIM,MEAN,SUMSQ
      CHARACTER*80 DURNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - screen cluster number
C     NCINT  - number of class intervals
C     NDURA  - number of duration intervals
C     NCRIT  - number of critical levels
C     DURNAM - name of this duration analysis
C     RCLINT - array of class intervals
C     DURAT  - array of duration intervals
C     SDATE  - starting date array
C     EDATE  - ending date array
C     SSDATE - season start date
C     SEDATE - season ending date
C     MXLEV  - maximum number of levels
C     MXDUR  - maximum number of durations
C     MXLC   - maximum number of lethal conc curves
C     TU     - time units for analysis
C     TS     - time step for analysis
C     DTRAN  - transformation function
C     NUM    -
C     FRQNW  -
C     SNW    -
C     SQNW   -
C     FRQPOS -
C     SPOS   -
C     SQPOS  -
C     FRQNEG -
C     SNEG   -
C     SQNEG  -
C     MNW    -
C     MPOS   -
C     MNEG   -
C     PTNW   -
C     PTPOS  -
C     PTNEG  -
C     PT1NW  -
C     PT1POS -
C     PT1NEG -
C     FREVNW -
C     FREVPS -
C     FREVNG -
C     DELT   -
C     MAX    -
C     MINIM  -
C     MEAN   -
C     SUMSQ  -
C     LCTSTO -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     RESP,SGRP,ONUM,OTYP(26),IVAL(17),CLEN(3),MAXLIN,
     1            I1,I0,TMPDUR,I4,IM1,LINCNT,I,OLEN,DONFG
      REAL        RVAL(8),RTMP,DUM
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CVAL(96),OBUFF1(80)
      CHARACTER*8 CTUNIT,CTRAN
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (OBUFF1,OBUFF)
      CHARACTER*80  OBUFF
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT,FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP,CVARAR,PMXTXM,DURTAB,DECPRC,PRNTXT,PMXCNW
      EXTERNAL   ZBLDWR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(13X,I4,10X,G10.4)
C
C     + + + END SPECIFICATIONS + + +
C
      IM1 = -1
      I0  = 0
      I1  = 1
      I4  = 4
      OLEN= 78
      TMPDUR = NDURA
      IF (TMPDUR.GT.5) THEN
C       can only fit 5 durations at this point
        TMPDUR = 5
      END IF
C
 10   CONTINUE
C       do duration results menu
        SGRP= 80
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,150,200,300,400,500,600,700,800,900), RESP
C
 100    CONTINUE
C         summary table
          SGRP = 81
          ONUM = 26
          OTYP(1) = 4
          OTYP(2) = 1
          OTYP(3) = 1
          OTYP(4) = 1
          OTYP(5) = 1
          OTYP(6) = 1
          OTYP(7) = 1
          OTYP(8) = 1
          OTYP(9) = 1
          OTYP(10)= 1
          OTYP(11)= 1
          OTYP(12)= 1
          OTYP(13)= 1
          OTYP(14)= 1
          OTYP(15)= 1
          OTYP(16)= 4
          OTYP(17)= 1
          OTYP(18)= 4
          OTYP(19)= 2
          OTYP(20)= 2
          OTYP(21)= 2
          OTYP(22)= 1
          OTYP(23)= 2
          OTYP(24)= 2
          OTYP(25)= 2
          OTYP(26)= 2
          IVAL(1) = SDATE(1)
          IVAL(2) = SDATE(2)
          IVAL(3) = SDATE(3)
          IVAL(4) = SDATE(4)
          IVAL(5) = SDATE(5)
          IVAL(6) = EDATE(1)
          IVAL(7) = EDATE(2)
          IVAL(8) = EDATE(3)
          IVAL(9) = EDATE(4)
          IVAL(10)= EDATE(5)
          IVAL(11)= SSDATE(1)
          IVAL(12)= SSDATE(2)
          IVAL(13)= SEDATE(1)
          IVAL(14)= SEDATE(2)
          IVAL(15)= TS
          IVAL(16)= INT(NUM)
          RVAL(1) = SPOS(1,1)
          RVAL(2) = SNW(1)
          RVAL(3) = SPOS(1,1)* DELT/ 1440
          RVAL(4) = MAX
C         adjust sig digits for mean
          RTMP = MEAN
          CALL DECPRC (I4,I1,RTMP)
          RVAL(5) = RTMP
          RVAL(6) = MINIM
C         adjust sig digits for std dev
          RTMP = SUMSQ
          CALL DECPRC (I4,I1,RTMP)
          RVAL(7) = RTMP
C         duration analysis name
          CLEN(1) = 80
          CALL CVARAR(CLEN(1),DURNAM,CLEN(1),CVAL)
C         time units
          IF (TU.EQ.1) THEN
            CTUNIT = 'seconds '
          ELSE IF (TU.EQ.2) THEN
            CTUNIT = 'minutes '
          ELSE IF (TU.EQ.3) THEN
            CTUNIT = 'hours   '
          ELSE IF (TU.EQ.4) THEN
            CTUNIT = 'days    '
          ELSE IF (TU.EQ.5) THEN
            CTUNIT = 'months  '
          ELSE IF (TU.EQ.6) THEN
            CTUNIT = 'years   '
          END IF
          CLEN(2) = 8
          CALL CVARAR(CLEN(2),CTUNIT,CLEN(2),CVAL(81))
C         transformation function
          IF (DTRAN.EQ.0) THEN
            CTRAN = 'average'
          ELSE IF (DTRAN.EQ.1) THEN
            CTRAN = 'sum    '
          ELSE IF (DTRAN.EQ.2) THEN
            CTRAN = 'max    '
          ELSE IF (DTRAN.EQ.3) THEN
            CTRAN = 'min    '
          END IF
          CLEN(3) = 8
          CALL CVARAR(CLEN(3),CTRAN,CLEN(3),CVAL(89))
          MAXLIN  = 10
          CALL PMXTXM (MESSFL,SCLU,SGRP,MAXLIN,I1,ONUM,OTYP,I0,
     I                 IVAL,RVAL,DVAL,CLEN,CVAL)
          GO TO 999
C
 150    CONTINUE
C         critical level table
          IF (NCRIT.EQ.0) THEN
C           no critical levels specified, tell user
            SGRP = 94
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           build table headers
            SGRP = 93
            CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,IM1,LINCNT)
C           begin critical level table
            DO 155 I = 1,NCRIT
              DUM= LCTSTO(I)/FLOAT(NUM)
              WRITE (OBUFF,2000) I,DUM
              CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
 155        CONTINUE
            OBUFF = ' '
            CALL ZBLDWR (OLEN,OBUFF1,I0,I0,DONFG)
          END IF
          GO TO 999
C
 200    CONTINUE
C         fraction of time relative to total
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 PTPOS,PTNEG,PTNW)
          GO TO 999
C
 300    CONTINUE
C         fraction of time relative to time in each level
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 PT1POS,PT1NEG,PT1NW)
          GO TO 999
C
 400    CONTINUE
C         time spent at each level
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 SPOS,SNEG,SNW)
          GO TO 999
C
 500    CONTINUE
C         number of excursions at each level
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 FRQPOS,FRQNEG,FRQNW)
          GO TO 999
C
 600    CONTINUE
C         average duration of excursions at each level
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 MPOS,MNEG,MNW)
          GO TO 999
C
 700    CONTINUE
C         standard deviation of excursions at each level
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 SQPOS,SQNEG,SQNW)
          GO TO 999
C
 800    CONTINUE
C         fraction of excursions with duration 'n'
          CALL DURTAB (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,TMPDUR,
     I                 DURAT,RCLINT,
     I                 FREVPS,FREVNG,FREVNW)
          GO TO 999
C
 900    CONTINUE
C         all done
          GO TO 999
C
 999    CONTINUE
C
      IF (RESP.NE.10) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURTAB
     I                   (MESSFL,SCLU,RESP,MXDUR,MXLEV,NCINT,
     I                    NDURA,DURAT,RCLINT,RPOS,RNEG,RNW)
C
C     + + + PURPOSE + + +
C     build duration analysis output table on screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,MXDUR,SCLU,RESP,NCINT,NDURA,DURAT(MXDUR),
     1          MXLEV
      REAL      RCLINT(MXLEV),RPOS(MXDUR,MXLEV),RNEG(MXDUR,MXLEV),
     1          RNW(MXDUR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     RESP   - response from menu, which table is this?
C     MXLEV  - maximum number of levels
C     MXDUR  - maximum number of durations
C     NCINT  - number of class intervals
C     NDURA  - number of durations
C     DURAT  - array of duration intervals
C     RCLINT - array of class intervals
C     RPOS   - array of positive excursion data for table
C     RNEG   - array of negative excursion data for table
C     RNW    - array of no water data for table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,I,I0,IM1,I1,OLEN,LINCNT,DONFG,J,K
      CHARACTER*1  OBUFF1(80)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (OBUFF1,OBUFF)
      CHARACTER*80  OBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     PMXCNW,ZBLDWR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (8X,10I10)
 2010 FORMAT (1F8.1,10F10.4)
 2020 FORMAT (8X,10F10.4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      IM1  = -1
      I1   = 1
      OLEN = 78
C
C     build table headers
      SGRP = RESP + 79
      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,IM1,LINCNT)
C     write line for durations
      WRITE (OBUFF,2000) (DURAT(I),I=1,NDURA)
      CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
C     write line for levels
      OBUFF = ' Levels'
      CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
C     begin positive table
      DO 10 I = 1,NCINT
        K = I+1
        WRITE (OBUFF,2010) RCLINT(I),(RPOS(J,K),J=1,NDURA)
        CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
 10   CONTINUE
C
      SGRP = 91
      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,IM1,IM1,LINCNT)
C     write line for durations
      WRITE (OBUFF,2000) (DURAT(I),I=1,NDURA)
      CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
C     write line for levels
      OBUFF = ' Levels'
      CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
C     begin negative table
      DO 20 I = 1,NCINT
        K = I+1
        WRITE (OBUFF,2010) RCLINT(I),(RNEG(J,K),J=1,NDURA)
        CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
 20   CONTINUE
C
      SGRP = 92
      CALL PMXCNW (MESSFL,SCLU,SGRP,I1,IM1,IM1,LINCNT)
C     write line for durations
      WRITE (OBUFF,2000) (DURAT(I),I=1,NDURA)
      CALL ZBLDWR (OLEN,OBUFF1,I0,IM1,DONFG)
C     begin no water table
      WRITE (OBUFF,2020) (RNW(J),J=1,NDURA)
      CALL ZBLDWR (OLEN,OBUFF1,I0,I0,DONFG)
C
      RETURN
      END
