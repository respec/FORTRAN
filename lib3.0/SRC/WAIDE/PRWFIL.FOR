C
C
C
      SUBROUTINE   PRWFIL
     I                    (MESSFL,PTHNAM,
     M                     WDMSFL)
C
C     + + + PURPOSE + + +
C     Open, close, build, or summarize a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL
      CHARACTER*8 PTHNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      BLDOPT
C
C     + + + EXTERNALS + + +
      EXTERNAL     PRWFLE
C
C     set flag so that the build option is on
      BLDOPT = 1
C
      CALL PRWFLE (MESSFL, BLDOPT, PTHNAM,
     M             WDMSFL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWFLE
     I                    (MESSFL, BLDOPT, PTHNAM,
     M                     WDMSFL)
C
C     + + + PURPOSE + + +
C     Open, close, build (optional), or summarize a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,BLDOPT
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     BLDOPT - flag to provide the BUILD option to the user
C              1 - yes,  0 - no
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I2,I64,SCLU,SGRP,RESP,RETCOD,IVAL,VARTYP(2),
     1             CLEN(1)
      REAL         RVAL
      DOUBLE PRECISION  DVAL
      CHARACTER*64 WDNAME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WDNAM1,WDNAME)
      CHARACTER*1  WDNAM1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP, PRWOPN, PRNTXT, WDFLCL, PRWMSU, PMXTXA
      EXTERNAL     PMXTXM, ZWNSOP
C
C     + + + END SPECIFICATIONS + + +
C
      RVAL = 0.0
      DVAL = 0.0D+00
      I0 = 0
      I1 = 1
      I2 = 2
      I64 = 64
      SCLU= 25
C
 10   CONTINUE
C       File menu:  1-Open, 2-Close, 3-Build, 4-Summarize, 5-Return
        CALL ZWNSOP (I1,PTHNAM)
        IF (BLDOPT .EQ. 1) THEN
C         include BUILD as option for user
          SGRP= 1
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        ELSE
C         don't give user the BUILD option
          SGRP= 33
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C         add 1 to RESPs after BUILD
          IF (RESP .GE. 3) RESP = RESP + 1
        END IF
C
        GO TO (100,200,300,400,500), RESP
C
 100    CONTINUE
C         open a WDM file
          IF (WDMSFL.EQ.0) THEN
C           nothing currently open, ok to open
            CALL PRWOPN (MESSFL,SCLU,I0,
     M                   WDMSFL)
          ELSE
C           already a WDM file open, can't open another
            INQUIRE (UNIT=WDMSFL,NAME=WDNAME)
            SGRP= 7
            CLEN(1)= 64
            CALL PMXTXA (MESSFL,SCLU,SGRP,I1,I1,I0,I1,CLEN,WDNAM1)
          END IF
          GO TO 900
C
 200    CONTINUE
C         close a WDM file
          IF (WDMSFL.GT.0) THEN
C           there is a WDM file open--close it; get name first
            INQUIRE (UNIT=WDMSFL,NAME=WDNAME)
            CALL WDFLCL (WDMSFL,
     O                   RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             print message to user that WDM file has been closed
              SGRP   = 13
              CLEN(1)= 64
              CALL PMXTXA (MESSFL,SCLU,SGRP,I1,I1,I0,I1,CLEN,WDNAM1)
              WDMSFL = 0
            ELSE
C             unable to close WDM file
              IVAL = RETCOD
C             1st var. to include in msg. is type character, 2nd is integer
              VARTYP(1) = 4
              VARTYP(2) = 1
              SGRP = 14
              CALL PMXTXM (MESSFL,SCLU,SGRP,I1,I1,I2,VARTYP,I0,IVAL,
     &                     RVAL,DVAL,I64,WDNAM1)
            END IF
          ELSE
C           no WDM file opened to close
            IF (BLDOPT .EQ. 1) THEN
C             tell user can user Open or Build option to open a WDM file
              SGRP = 8
            ELSE
C             tell user can use Open option to open a WDM file
              SGRP = 15
            END IF
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 900
C
 300    CONTINUE
C         build a new WDM file
          IF (WDMSFL.EQ.0) THEN
C           no WDM file open, ok to build one
            I= 2
            CALL PRWOPN (MESSFL,SCLU,I,
     M                   WDMSFL)
Ckmf        IF (WDMSFL.GT.0) THEN
Ckmf          WDM file successfully built
Ckmf          SGRP= 11
Ckmf          CALL PRNTXT (MESSFL,SCLU,SGRP)
Ckmf        END IF
          ELSE
C           WDM file already open, can't build one
            INQUIRE (UNIT=WDMSFL,NAME=WDNAME)
            SGRP= 9
            CLEN(1)= 64
            CALL PMXTXA (MESSFL,SCLU,SGRP,I1,I1,I0,I1,CLEN,WDNAM1)
          END IF
          GO TO 900
C
 400    CONTINUE
C         summarize WDM file contents
          IF (WDMSFL.GT.0) THEN
C           file open to summarize
            CALL PRWMSU (MESSFL,WDMSFL,SCLU)
          ELSE
C           no file open to summarize
            IF (BLDOPT .EQ. 1) THEN
C             tell user need to Open or Build a WDM file first
              SGRP = 10
            ELSE
C             tell user need to Open a WDM file first
              SGRP = 16
            END IF
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 900
C
 500    CONTINUE
C         return to opening screen
          GO TO 900
C
 900    CONTINUE
C
      IF (RESP.NE.5) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWOPN
     I                   (MESSFL,SCLU,NEWFG,
     M                    WDMSFL)
C
C     + + + PURPOSE + + +
C     Interactively open an existing or build a new WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,NEWFG,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     NEWFG  - new file flag,
C              0 - opening existing file, 2 - creating a new file
C     WDMSFL - Fortran unit number of WDM file opened
C              0 - WDM file not opened
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,SGRP,FILUN(1),IRET,IVAL(1),I4
      CHARACTER*1  STRIN1(4)
      CHARACTER*4  WDID
      CHARACTER*8  GLOID
C
C     + + + EXTERNALS + + +
      EXTERNAL     Q1INIT, Q1EDIT, ZSTCMA, QGETF, WIDADD, CVARAR, GSTGLV
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I4 = 4
C
C     allow 'Prev'
      I= 4
      CALL ZSTCMA (I,I1)
      IF (NEWFG.EQ.0) THEN
        SGRP= 3
      ELSE
        SGRP= 2
      END IF
C     init screen
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C     have user enter WDM file name
      CALL Q1EDIT (IRET)
C     turn off 'Prev'
      I= 4
      CALL ZSTCMA (I,I0)
      IF (IRET.EQ.1) THEN
C       get the unit number of the WDM file opened
        CALL QGETF (I1,
     O              FILUN)
        WDMSFL= FILUN(1)
C       initialize wdm file id in common
        WDID = 'WDM1'
        CALL WIDADD (WDMSFL,I1,WDID)
C       set global variable for wdm file ids
        GLOID  = 'WDID    '
        IVAL(1)= 0
        CALL CVARAR (I4,WDID,I4,STRIN1(1))
        CALL GSTGLV (GLOID,I4,I4,STRIN1,I1,IVAL)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMSU
     I                    (MESSFL,WDMSFL,SCLU)
C
C     + + + PURPOSE + + +
C     Summarize the contents of a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster number on message file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,RREC,RIND,DSNMIN,DSNMAX,DIRMIN,DIRMAX,IDSTYP,
     1             I,I0,I1,I12,IM1,J,K,L,COUNT,MAXLN,LINCNT,DSCTYP,IFLG,
     2             IVAL(1)
      CHARACTER*1  WNAMEX(78),BK(1),DSNTYP(12,10)
      CHARACTER*64 FNAME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (WNAMEX,WNAME)
      CHARACTER*78 WNAME
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, LENSTR, WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, LENSTR, WDCKDT, GETTXT
      EXTERNAL  PMXTXI, CHRCHR, INTCHR, ZIPC, PMXCNW, ZBLDWR
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (16A4)
C
C     + + + END SPECIFICATIONS + + +
C
      BK(1)= ' '
      I0   = 0
      I1   = 1
      I12  = 12
      IM1  = -1
      MAXLN= 50
C
C     get valid dataset types
      SGRP= 31
      I   = 108
      CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O             DSNTYP)
      SGRP= 32
      I   = 12
      CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O             DSNTYP(1,10))
      I= 120
      DSCTYP= I/12
C
C     general summary information
      SGRP = 21
      CALL PMXCNW (MESSFL,SCLU,SGRP,MAXLN,I1,IM1,
     O             LINCNT)
C     blank line
      CALL ZBLDWR (I1,BK,I0,IM1,
     O             LINCNT)
C     print header for name of file
      SGRP= 22
      I   = 13
      CALL GETTXT (MESSFL,SCLU,SGRP,I,
     O             WNAMEX)
C
C     get general directory record
      RREC = 1
      RIND = WDRCGO(WDMSFL,RREC)
C     is name of file available?
      IF (WIBUFF(PFNAME,RIND).NE.0) THEN
C       get name of WDM file, as it was created
        WRITE (FNAME,1000) (WIBUFF(I,RIND),I=PFNAME,PFNAME+15)
        WNAME(14:24)= 'created as '
        WNAME(25:78)= FNAME(1:54)
        L= 78
        CALL ZBLDWR (L,WNAMEX,I0,IM1,
     O               LINCNT)
      END IF
C
C     get name of WDM file, as it is stored, with inquire statement
      INQUIRE (UNIT=WDMSFL,NAME=FNAME)
      WNAME(14:24)= 'stored as  '
      WNAME(25:78)= FNAME(1:54)
      L= 78
      CALL ZBLDWR (L,WNAMEX,I0,IM1,
     O             LINCNT)
C     print size of file
      SGRP= 23
      CALL PMXTXI(MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,I1,WIBUFF(PMXREC,RIND))
C     write out a blank line
      CALL ZBLDWR (I1,BK,I0,IM1,
     O             LINCNT)
C     min and maximum dataset number
C     look for directory records used
      DIRMIN= 0
      DO 20 I= PDIRPT,PDIRPT+399
        IF (WIBUFF(I,RIND).GT.0) THEN
C         this rec is used, current max
          DIRMAX= WIBUFF(I,RIND)
          DSNMAX= (I-PDIRPT)* 500
          IF (DIRMIN.EQ.0) THEN
C           also min
            DIRMIN= DIRMAX
            DSNMIN= DSNMAX
          END IF
        END IF
 20   CONTINUE
      IF (DIRMIN.GT.0) THEN
C       datasets available, get actual min dsn
        RIND= WDRCGO(WDMSFL,DIRMIN)
        IFLG= 0
        I   = 4
30      CONTINUE
          I= I+ 1
          IF (WIBUFF(I,RIND).GT.0) THEN
C           data set found, is it type we're looking for
            J= DSNMIN+ I- 4
            IDSTYP= WDCKDT (WDMSFL,J)
            IF (IDSTYP.GT.0) THEN
C             valid type, are we using it
              IF (LENSTR(I12,DSNTYP(1,IDSTYP)).GT.0) THEN
C               we are using this type, this is min
                IFLG= 1
              END IF
            END IF
          END IF
        IF (IFLG.EQ.0 .AND. I.LT.504) GO TO 30
        IF (IFLG.GT.0) THEN
C         min found for type in use
          DSNMIN= DSNMIN+ I- 4
        ELSE
C         type in use not found on min directory pointer record
          DSNMIN= 0
        END IF
        SGRP= 26
        IVAL(1) = DSNMIN
        CALL PMXTXI (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,I1,IVAL)
C       get actual max dsn
        RIND= WDRCGO(WDMSFL,DIRMAX)
        IFLG= 0
        I   = 505
40      CONTINUE
          I= I- 1
          IF (WIBUFF(I,RIND).GT.0) THEN
C           data set found, is it type we're looking for
            J= DSNMAX+ I- 4
            IDSTYP= WDCKDT (WDMSFL,J)
            IF (IDSTYP.GT.0) THEN
C             valid type, are we using it
              IF (LENSTR(I12,DSNTYP(1,IDSTYP)).GT.0) THEN
C               we are using this type, this is max
                IFLG= 1
              END IF
            END IF
          END IF
        IF (IFLG.EQ.0 .AND. I.GT.5) GO TO 40
        IF (IFLG.GT.0) THEN
C         max found for type in use
          DSNMAX= DSNMAX+ I- 4
        ELSE
C         type in use not found on max directory pointer record
          DSNMAX= 0
        END IF
        SGRP= 27
        IVAL(1) = DSNMAX
        CALL PMXTXI (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,I1,IVAL)
      END IF
C     write out a blank line
      CALL ZBLDWR (I1,BK,I0,IM1,
     O             LINCNT)
C     print count of each dataset type
      K= 0
      COUNT= 0
      RREC = 1
      RIND = WDRCGO(WDMSFL,RREC)
      DO 10 I= 1,DSCTYP
        J= PTSNUM+ (I-1)* 2
        IF (WIBUFF(J,RIND).GT.0 .AND. LENSTR(I12,DSNTYP(1,I)).GT.0) THEN
          L= 45
          CALL ZIPC (L,BK(1),WNAMEX)
          IF (K.EQ.0) THEN
C           first one, print general heading
            SGRP= 24
            CALL PMXCNW (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,
     O                   LINCNT)
            SGRP= 28
            CALL PMXCNW (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,
     O                   LINCNT)
          END IF
          K= K+ 1
          L= 12
          CALL CHRCHR (L,DSNTYP(1,I),WNAMEX(6))
          L= 6
          CALL INTCHR (WIBUFF(J,RIND),L,I0,
     M                 L,WNAMEX(24))
          COUNT= COUNT+ WIBUFF(J,RIND)
          L= 35
          CALL ZBLDWR (L,WNAMEX,I0,IM1,
     O                 LINCNT)
        END IF
 10   CONTINUE
      IF (K.EQ.0) THEN
C       no datasets present
        SGRP= 25
        CALL PMXCNW (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,
     O               LINCNT)
      ELSE
C       write out total
        SGRP= 28
        CALL PMXCNW (MESSFL,SCLU,SGRP,MAXLN,IM1,IM1,
     O               LINCNT)
        I= 40
        CALL ZIPC (I,BK(1),WNAMEX)
        I= 20
        SGRP= 29
        CALL GETTXT (MESSFL,SCLU,SGRP,I,WNAMEX)
        I= 6
        CALL INTCHR (COUNT,I,I0,
     M               L,WNAMEX(24))
        I= 45
        CALL ZBLDWR (I,WNAMEX,I0,IM1,
     O               LINCNT)
      END IF
C     write the whole screen out
      CALL ZBLDWR (I1,BK,I0,I0,
     O             LINCNT)
C
      RETURN
      END
