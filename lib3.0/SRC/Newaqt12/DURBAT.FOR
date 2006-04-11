C
C
C
      SUBROUTINE   DAANAL
     I                   (DURNAM,
     I                    DMXLEV,NCINT,RCLINT,
     I                    DMXDUR,NDURA,DURAT,
     I                    DMXLC,NCRIT,RLCLEV,
     I                    SDATE,EDATE,SSDATE,SEDATE,TU,TS,DTRAN,
     I                    WDMFL,DSN,
     I                    PRFG,PUNIT,LCOUT,LCGTLT,
     O                    NUM,FRQNW,SNW,SQNW,
     O                    FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     O                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O                    DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO)
C
C     + + + PURPOSE + + +
C     do duration analysis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  DMXLEV,NCINT,DMXDUR,NDURA,DURAT(DMXDUR),DMXLC,NCRIT,
     1         SDATE(6),EDATE(6),SSDATE(2),SEDATE(2),TU,TS,DTRAN,
     2         WDMFL,DSN,PRFG,PUNIT,LCOUT,NUM,LCTSTO(DMXLC),LCGTLT
      REAL     RCLINT(DMXLEV),RLCLEV(DMXLC,DMXLEV),
     $         FRQNW(DMXDUR),SNW(DMXDUR),SQNW(DMXDUR),
     $         FRQPOS(DMXDUR,DMXLEV),SPOS(DMXDUR,DMXLEV),
     $         SQPOS(DMXDUR,DMXLEV),FRQNEG(DMXDUR,DMXLEV),
     $         SNEG(DMXDUR,DMXLEV),SQNEG(DMXDUR,DMXLEV),
     $         MNW(DMXDUR),MPOS(DMXDUR,DMXLEV),MNEG(DMXDUR,DMXLEV),
     $         PTNW(DMXDUR),PTPOS(DMXDUR,DMXLEV),PTNEG(DMXDUR,DMXLEV),
     $         PT1NW(DMXDUR),PT1POS(DMXDUR,DMXLEV),
     $         PT1NEG(DMXDUR,DMXLEV),FREVNW(DMXDUR),
     $         FREVPS(DMXDUR,DMXLEV),FREVNG(DMXDUR,DMXLEV),
     $         DELT,MAX,MINIM,MEAN,SUMSQ
      CHARACTER*80 DURNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DURNAM -
C     DMXLEV -
C     NCINT  -
C     RCLINT -
C     DMXDUR -
C     NDURA  -
C     DURAT  -
C     DMXLC  -
C     NCRIT  -
C     RLCLEV -
C     SDATE  -
C     EDATE  -
C     SSDATE -
C     SEDATE -
C     TU     -
C     TS     -
C     DTRAN  -
C     WDMFL  -
C     DSN    -
C     PRFG   -
C     PUNIT  -
C     LCOUT  -
C     LCGTLT -
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
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  QFLAG,NPTS,JTMP,RETCOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAANWV
      EXTERNAL    TSBWDS,TSBGET,TSBTIM,TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIMDIF (SDATE,EDATE,TU,TS,
     O             NPTS)
C     set time unit parameters for analysis
      QFLAG = 30
      CALL TSBTIM (TU,TS,DTRAN,QFLAG)
C     get data
      JTMP= NPTS+ 1
      CALL TSBWDS (WDMFL,DSN)
      CALL TSBGET (SDATE,NPTS,
     O             YX(JTMP),RETCOD)
      IF (RETCOD .LT. 0) THEN
C       could not retrieve data
        write(99,*) 'problem retrieving data from data set ',dsn
        RETCOD = 1
      ELSE
C       got data okay, continue
        CALL DAANWV (DURNAM,
     I               DMXLEV,NCINT,RCLINT,
     I               DMXDUR,NDURA,DURAT,
     I               DMXLC,NCRIT,RLCLEV,
     I               SDATE,EDATE,SSDATE,SEDATE,TU,TS,NPTS,
     I               PRFG,PUNIT,LCOUT,LCGTLT,
     O               NUM,FRQNW,SNW,SQNW,
     O               FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     O               MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O               PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O               DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DAANWV
     I                   (DURNAM,
     I                    DMXLEV,NCINT,RCLINT,
     I                    DMXDUR,NDURA,DURAT,
     I                    DMXLC,NCRIT,RLCLEV,
     I                    SDATE,EDATE,SSDATE,SEDATE,TU,TS,NPTS,
     I                    PRFG,PUNIT,LCOUT,LCGTLT,
     O                    NUM,FRQNW,SNW,SQNW,
     O                    FRQPOS,SPOS,SQPOS,FRQNEG,SNEG,SQNEG,
     O                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O                    DELT,MAX,MINIM,MEAN,SUMSQ,LCTSTO)
C
C     + + + PURPOSE + + +
C     do duration analysis, with data values set in yx buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  DMXLEV,NCINT,DMXDUR,NDURA,DURAT(DMXDUR),DMXLC,NCRIT,
     1         SDATE(6),EDATE(6),SSDATE(2),SEDATE(2),TU,TS,NPTS,
     2         PRFG,PUNIT,LCOUT,NUM,LCTSTO(DMXLC),LCGTLT
      REAL     RCLINT(DMXLEV),RLCLEV(DMXLC,DMXLEV),
     $         FRQNW(DMXDUR),SNW(DMXDUR),SQNW(DMXDUR),
     $         FRQPOS(DMXDUR,DMXLEV),SPOS(DMXDUR,DMXLEV),
     $         SQPOS(DMXDUR,DMXLEV),FRQNEG(DMXDUR,DMXLEV),
     $         SNEG(DMXDUR,DMXLEV),SQNEG(DMXDUR,DMXLEV),
     $         MNW(DMXDUR),MPOS(DMXDUR,DMXLEV),MNEG(DMXDUR,DMXLEV),
     $         PTNW(DMXDUR),PTPOS(DMXDUR,DMXLEV),PTNEG(DMXDUR,DMXLEV),
     $         PT1NW(DMXDUR),PT1POS(DMXDUR,DMXLEV),
     $         PT1NEG(DMXDUR,DMXLEV),FREVNW(DMXDUR),
     $         FREVPS(DMXDUR,DMXLEV),FREVNG(DMXDUR,DMXLEV),
     $         DELT,MAX,MINIM,MEAN,SUMSQ
      CHARACTER*80 DURNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DURNAM -
C     DMXLEV -
C     NCINT  -
C     RCLINT -
C     DMXDUR -
C     NDURA  -
C     DURAT  -
C     DMXLC  -
C     NCRIT  -
C     RLCLEV -
C     SDATE  -
C     EDATE  -
C     NPTS   -
C     SSDATE -
C     SEDATE -
C     TU     -
C     TS     -
C     PRFG   -
C     PUNIT  -
C     LCOUT  -
C     LCGTLT -
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
C     + + + PARAMETERS + + +
      INCLUDE 'pmxdur.inc'
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  STAFG,FINFG,TITLE(10),DURANO,I1,M,
     $         DATIM(5),DATIMS(5),DATIME(5),
     $         SESFG,SESONS(5),SESONE(5),TMPTIM(6),TIMNEW(6),
     $         CURPOS,CURNEG,LASPOS,LASNEG,LGTNW1(1),
     $         LGTPOS(MXLEV),LGTNEG(MXLEV),
     $         LCLEV(MXDUR,MXLC),LCTIML(MXDUR,MXLC),
     $         LCTSCT(MXDUR,MXLC),I,J,JTMP,IMINS
      REAL     VALUE,SUM,LEVEL(MXLEV),LCCONC(MXDUR,MXLC)
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL    DURCLC
      EXTERNAL    TSBWDS,TSBGET,TSBTIM,TIMDIF,HDATIN,TIMADD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(10A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     make extra buckets for out of range data
      M= NCINT+ 2
      LEVEL(1)= -1.0E30
      LEVEL(M)= +1.0E30
      DO 5 I= 1,NCINT
        LEVEL(I+1)= RCLINT(I)
 5    CONTINUE
C
C     set up needed arrays
      DO 20 I= 1,MXDUR
        DO 10 J= 1,MXLC
          LCCONC(I,J)= RLCLEV(J,I)
 10     CONTINUE
 20   CONTINUE
C     convert title
      READ(DURNAM,1000) TITLE
C
C     dummy values
      DURANO= 0
C
C     find delt in minutes
      I = 2
      J = 1
      CALL TIMDIF (SDATE,EDATE,I,J,
     O             IMINS)
      DELT = FLOAT(IMINS)/FLOAT(NPTS)
C
C     set dates
      DO 40 I= 1,5
        DATIMS(I)= SDATE(I)
        DATIME(I)= EDATE(I)
        DATIM(I) = SDATE(I)
 40   CONTINUE
C     convert dates to internal formats
      CALL HDATIN(DATIMS(1),DATIMS(2),DATIMS(3),DATIMS(4),DATIMS(5))
      CALL HDATIN(DATIME(1),DATIME(2),DATIME(3),DATIME(4),DATIME(5))
      CALL HDATIN(DATIM(1), DATIM(2), DATIM(3), DATIM(4), DATIMS(5))
      SESONS(2)= SSDATE(1)
      SESONS(3)= SSDATE(2)
      SESONS(4)= 0
      SESONS(5)= 0
      CALL HDATIN(SESONS(1),SESONS(2),SESONS(3),SESONS(4),SESONS(5))
      SESONE(2)= SEDATE(1)
      SESONE(3)= SEDATE(2)
      SESONE(4)= 0
      SESONE(5)= 0
      CALL HDATIN(SESONE(1),SESONE(2),SESONE(3),SESONE(4),SESONE(5))
C
C     loop to do analysis
      STAFG= 1
      FINFG= 0
      DO 100 I= 1,NPTS
        IF (I .EQ. NPTS) THEN
C         last time thru
          FINFG= 1
        END IF
        JTMP = NPTS+ I
        VALUE= YX(JTMP)
        CALL DURCLC (MXLEV,M,NCINT,MXDUR,NDURA,MXLC,NCRIT,
     I               STAFG,FINFG,LCOUT,PUNIT,PRFG,
     I               TITLE,DELT,DURANO,DATIM,
     I               VALUE,DURAT,LEVEL,LCLEV,LCCONC,LCGTLT,
     M               DATIMS,DATIME,SESFG,SESONS,SESONE,
     M               CURPOS,CURNEG,LASPOS,LASNEG,
     M               LGTNW1,LGTPOS,LGTNEG,
     M               NUM,SUM,SUMSQ,MAX,MINIM,
     M               FRQNW,SNW,SQNW,
     M               FRQPOS,SPOS,SQPOS,
     M               FRQNEG,SNEG,SQNEG,
     M               LCTIML,LCTSCT,LCTSTO,
     O               MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O               PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O               MEAN)
C       need to update current date/time
        TMPTIM(1) = DATIM(1)
        TMPTIM(2) = DATIM(2)
        TMPTIM(3) = DATIM(3)
        TMPTIM(4) = DATIM(4)
        TMPTIM(5) = DATIM(5)
        TMPTIM(6) = 0
        CALL TIMADD (TMPTIM,TU,TS,I1,TIMNEW)
        DATIM(1) = TIMNEW(1)
        DATIM(2) = TIMNEW(2)
        DATIM(3) = TIMNEW(3)
        DATIM(4) = TIMNEW(4)
        DATIM(5) = TIMNEW(5)
C
        STAFG= 0
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DAANST
     I                   (NPTS,RVALS)
C
C     + + + PURPOSE + + +
C     set data values for duration analysis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  NPTS
      REAL     RVALS(NPTS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPTS   -
C     RVALS  -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,JTMP
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NPTS
        JTMP= NPTS + I
        YX(JTMP) = RVALS(I)
 10   CONTINUE
C
      RETURN
      END
