C
C
C
      SUBROUTINE   MAPSEG
     I                   (MESSFL,SCLU,WDMPFL,DPLDSN)
C
C     + + + PURPOSE + + +
C     display line segments on a map in specified colors
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMPFL,DPLDSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number
C     WDMPFL - unit number of wdm boundary file (with dlgs)
C     DPLDSN - dataset number to display
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ICLR,IFIL,ISTY,ACTSET,ACT,LSDATE(6),LEDATE(6),
     1            LSSDAT(2),LSEDAT(2),DTTU,DTTS,DTAG,NVALS,CURDAT(6),
     2            QFLAG,J,I1,RETCOD,CLR,NEWDAT(6),I6,MOVIE,SGRP,
     3            LEN,ERRCOD,I0,DONFG,LINCNT,INIT,I4
      INTEGER*2   INT2
      REAL        RVAL(1),PERDON,WID,R0,PREWID(MXSEG)
      CHARACTER*1 DSTRNG(21),BLNK(1),STRIN1(78)
      CHARACTER*8 CDSID
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL  GEMADR,DTACT,DTGET,TIMDIF,TSBTIM,TSBWDS,TSBGET,TIMADD
      EXTERNAL  COPYI,PMXCNW,DATLST,ZBLDWR,C1INT,PAUMEN,CHRCHR,DECCHR
      EXTERNAL  ZIPC,PRNTXT,GSLWSC,ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CURANI.GT.0) THEN
C       an animation set is current, continue
        I0 = 0
        I1 = 1
        I4 = 4
        I6 = 6
        ICLR = 0
        IFIL = 0
        ISTY = 0
C       look through all map segment specs to see if we want movie
        MOVIE = 0
        DO 2 I = 1,NSEG(CURANI)
          IF (DSN(CURANI,I).NE.0) THEN
C           assume we want movie
            MOVIE = 1
          END IF
 2      CONTINUE
        CALL DTACT (ACTSET)
        IF (ACTSET.LT.1) THEN
C         no active date set, cant do movie
          MOVIE = 0
        END IF
        IF (MOVIE.EQ.0) THEN
C         dont want movie, just draw segments once
          DO 3 I = 1,NSEG(CURANI)
            CLR = CLR1(CURANI,I)
C           set line width scale factor
            CALL GSLWSC (WID1(CURANI,I))
            CALL GEMADR (WDMPFL,DPLDSN,ATMAJ(CURANI,I),ATMIN(CURANI,I),
     1                   CLR,ICLR,IFIL,ISTY)
 3        CONTINUE
        ELSE
C         show time! get current time set specs
          CALL DTGET (ACTSET,
     O                ACT,CDSID,LSDATE,LEDATE,LSSDAT,LSEDAT,
     O                DTTU,DTTS,DTAG)
          CALL TIMDIF (LSDATE,LEDATE,DTTU,DTTS,
     O                 NVALS)
          CALL COPYI (I6,LSDATE,CURDAT)
          PERDON = 100.0/FLOAT(NVALS)
          LEN = 78
          BLNK(1) = ' '
          CALL ZIPC (LEN,BLNK(1),STRIN1)
          IF (NVALS.GT.0) THEN
C           set time unit parameters
            QFLAG = 31
            CALL TSBTIM (DTTU,DTTS,DTAG,QFLAG)
            SGRP = 69
C           display current date in window, percent complete
            CALL PMXCNW (MESSFL,SCLU,SGRP,I6,I1,I1,LINCNT)
            CALL DATLST (CURDAT,
     O                   DSTRNG,LEN,ERRCOD)
            CALL CHRCHR (LEN,DSTRNG,STRIN1(3))
            CALL DECCHR (PERDON,I4,I1,LEN,STRIN1(45))
            LEN = 78
            CALL ZBLDWR (LEN,STRIN1,I0,I1,DONFG)
C
            R0= 0.0
            CALL ZIPR (NSEG(CURANI),R0,PREWID)
            J = 0
            INIT = -1
 5          CONTINUE
              J = J + 1
C             loop through each time interval
              IF (INIT.EQ.1) THEN
C               need to initialize screen again
                SGRP = 69
                CALL PMXCNW (MESSFL,SCLU,SGRP,I6,I1,I1,LINCNT)
                INIT = 0
              END IF
              PERDON = 100.0*FLOAT(J)/FLOAT(NVALS)
              LEN = 78
              CALL ZIPC (LEN,BLNK(1),STRIN1)
              CALL DATLST (CURDAT,
     O                     DSTRNG,LEN,ERRCOD)
              CALL CHRCHR (LEN,DSTRNG,STRIN1(3))
              CALL DECCHR (PERDON,I4,I1,LEN,STRIN1(45))
              LEN = 78
              CALL ZBLDWR (LEN,STRIN1,INIT,I1,DONFG)
              IF (INIT.EQ.0) THEN
C               overwrite last record from now on
                INIT = -1
              END IF
              DO 10 I = 1,NSEG(CURANI)
C               for each segment, get data at this time
                IF (ATMAJ(CURANI,I).NE.0 .OR. ATMIN(CURANI,I).NE.0) THEN
C                 want to draw something for this set of specs
                  IF (DSN(CURANI,I).EQ.0) THEN
C                   no data set number specified, use first spec
                    CLR = CLR1(CURANI,I)
                    WID = WID1(CURANI,I)
                  ELSE
                    CALL TSBWDS (FILUNT,DSN(CURANI,I))
                    CALL TSBGET (CURDAT,I1,
     O                           RVAL(1),RETCOD)
                    IF (RETCOD.NE.0) THEN
C                     problem getting value
                      CLR = 0
                      WRITE (99,*) 'problem getting value in mapseg'
                    ELSE
C                     now that we have this value, which class is it in
                      IF (RVAL(1).GT.RMAX(CURANI,I)) THEN
C                       class 3
                        CLR = CLR3(CURANI,I)
                        WID = WID3(CURANI,I)
                      ELSE IF (RVAL(1).GE.RMIN(CURANI,I) .AND.
     1                    RVAL(1).LE.RMAX(CURANI,I)) THEN
C                       class 2
                        CLR = CLR2(CURANI,I)
                        WID = WID2(CURANI,I)
                      ELSE
C                       class 1
                        CLR = CLR1(CURANI,I)
                        WID = WID1(CURANI,I)
                      END IF
                    END IF
                  END IF
                  IF (PREWID(I).GT.WID) THEN
C                   previous width was greater than this, blank it
                    CALL GSLWSC (PREWID(I))
                    CALL GEMADR (WDMPFL,DPLDSN,ATMAJ(CURANI,I),
     1                           ATMIN(CURANI,I),I0,ICLR,IFIL,ISTY)
                  END IF
C                 set line width scale factor
                  CALL GSLWSC (WID)
                  PREWID(I) = WID
C                 draw each segment at this time
                  CALL GEMADR (WDMPFL,DPLDSN,ATMAJ(CURANI,I),
     1                         ATMIN(CURANI,I),CLR,ICLR,IFIL,ISTY)
                END IF
 10           CONTINUE
              IF (INT2.NE.-2) THEN
C               didnt just single step, check for pause key
                CALL C1INT(INT2)
              END IF
              IF (INT2.EQ.32 .OR. INT2.EQ.-2) THEN
C               rewrite current date
                LEN = 78
                CALL ZBLDWR (LEN,STRIN1,INIT,INIT,DONFG)
C               write blank line
                CALL ZBLDWR (I1,BLNK,I0,INIT,DONFG)
C               do pause menu
                CALL PAUMEN (MESSFL,SCLU,
     M                       INT2)
                INIT = 1
              ELSE IF (INT2.NE.-1) THEN
C               other character (besides pause or single step), ignore
                write (99,*) 'other character ',INT2
                INT2 = -1
              END IF
C             get date for next time step
              CALL TIMADD (CURDAT,DTTU,DTTS,I1,
     O                     NEWDAT)
              CALL COPYI (I6,NEWDAT,CURDAT)
            IF (J.LT.NVALS .AND. (INT2.EQ.-1 .OR. INT2.EQ.-2)) GO TO 5
          END IF
        END IF
C       set line width scale factor back to original
        RVAL(1) = 1.0
        CALL GSLWSC (RVAL(1))
      ELSE
C       no current set of map animation specifications
        SGRP = 72
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PAUMEN
     I                   (MESSFL,SCLU,
     M                    INT2)
C
C     + + + PURPOSE + + +
C     display map animation pause menu
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MESSFL,SCLU
      INTEGER*2  INT2
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number
C     INT2   - return code from pause check
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SGRP,RESP
C
C     + + + EXTERNALS + + +
      EXTERNAL  QRESP,ZMNSST
C
C     + + + END SPECIFICATIONS + + +
C
C     keep date on screen before menu
      CALL ZMNSST
      SGRP = 70
      RESP = 1
      CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
      IF (RESP.EQ.1) THEN
C       user wanted to pause but now resume
        INT2 = -1
      ELSE IF (RESP.EQ.2) THEN
C       user wants to single step
        INT2 = -2
      ELSE IF (RESP.EQ.3) THEN
C       user wants to quit
        INT2 = 0
      END IF
C
      RETURN
      END
