      SUBROUTINE   HSPF_TREE_INIT
     I                          (FILNAM,
     O                           RETCOD)

      ! dummy arguments
      CHARACTER*64 FILNAM
      INTEGER      RETCOD

      ! local variables
      INTEGER      FILES(15),I,I0,I1,MESSFL,USRFL,MESSU,
     $             RCL,SCLU,SGRP
      LOGICAL      LFLAG
      CHARACTER*64 HMSNAM
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRMT

      ! externals
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPFXX, HDMESI
      EXTERNAL     ZIPI, DSSCLO, UCIINP, HDMESC, HDMEST, WDBOPN, SCCLAL

      ! end specifications

      MESSFL= 9
      I0    = 0
      I1    = 1

      ! avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)

      OPEN(UNIT=99,FILE='ERROR.FIL')

      ! initialize wdm file common block
      CALL WDBFIN

      ! open message file
      INCLUDE 'FHSMES.INC'
      CALL WDBOPN (MESSFL,HMSNAM,I1,
     O             RETCOD)
      IF (RETCOD.NE.0) THEN
        ! problem with opening message file, prompt for other file name
        WRITE (*,*) 'HSPTSTR:Problem:',RETCOD,MESSFL,I,' with ',HMSNAM
      ELSE
        ! do opening message
        CALL SCCLAL
        SCLU = 201
        SGRP = 50
        CALL HDMESC (MESSFL,SCLU,SGRP,FILNAM)
        ! open input file
        USRFL= 7
        ACC  = 'SEQUENTIAL'
        FRMT = 'FORMATTED'
        RCL  = 0
        STAT = 'OLD'
        CALL FILOPN (ACC,FRMT,RCL,STAT,USRFL,FILNAM,
     O               RETCOD)
        IF (RETCOD .EQ. 0) THEN
          ! input file opened, process files block in input file
          ! initialize files to closed
          I = 14
          CALL ZIPI (I,I0,FILES)
          FILES(15) = MESSFL
          CALL FILBLK (USRFL,
     M                 FILES,
     O                 RETCOD)
          ! back to beginning of input file
          REWIND(USRFL)

          IF (RETCOD .EQ. 0) THEN
            ! file block processed without error - beginning message
            CALL SCCLAL
            SGRP = 51
            CALL HDMEST (MESSFL,SCLU,SGRP)
            ! read users uci file
            MESSU  = FILES(1)
            CALL UCIINP (USRFL,MESSFL,MESSU)
            ! close users input file
            CLOSE (UNIT=USRFL)
            ! proceed to interp model
            CALL HSPFXX (FILES,
     O                   RETCOD)
            ! simulation complete
            IF (RETCOD .EQ. 0) THEN
              SGRP = 52
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 1) THEN
              ! runfg=0 in global block - must stop
              SGRP = 53
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 2) THEN
              ! errors in input file -  must stop
              SGRP = 54
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 3) THEN
              ! no run keyword found in input file
              SGRP = 55
              CALL HDMEST (MESSFL,SCLU,SGRP)
            END IF
          ELSE
            ! error in files block
            SGRP = 56
            CALL HDMESI (MESSFL,SCLU,SGRP,RETCOD)
          END IF
        ELSE
          ! error opening uci file
          SGRP = 57
          CALL HDMESI (MESSFL,SCLU,SGRP,RETCOD)
        END IF

        ! close any dss files
        CALL DSSCLO
      END IF

      END SUBROUTINE HSPF_TREE_INIT

      SUBROUTINE HSPFXX
     I                   (FILES,
     O                    RETCOD)

      ! dummy arguemnts
      INTEGER   FILES(15),RETCOD

      ! argument defintions
      ! FILES  - unit numbers of files from files block in uci or application
      ! RETCOD - return code - 0:run completed
      !                      1:interp only
      !                      2:errors in interp
      !                      3:no run keyword found

      ! local variables
      INTEGER      I0,I1,SCLU,SGRP,MESSU,MSGFL,INITFG,CLEN,CONT,KCNT,
     $             ECOUNT,KWDDIM(1),KWDTYP(1),MKFILS
      CHARACTER*1  CHSTR1(20),KWDLIB(12)

      ! equivalences
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR

      ! externals
      EXTERNAL  PMXTFT, KEYUCI, DMPKEY, INTERP, WMSGTT
      EXTERNAL  INIKEY, HDMEST

      ! input formats
 1000 FORMAT (12A1,2I4)

      ! end specifciations

      I1= 1
      I0= 0
      ! no errors yet
      ECOUNT= 0

      MESSU= FILES(1)
      MSGFL= FILES(15)

      SCLU= 201

      ! title block to output file
      SGRP= 1
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)

      ! start of job message
      SGRP= 2
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)

      ! first pre-process input for a data set message
      SGRP= 3
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)

      ! get major keywords (RUN)
      SGRP= 21
      INITFG= 1
      CLEN= 20
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) KWDLIB,KWDDIM,KWDTYP

      ! look for a fresh set of keywords
      CALL INIKEY

      ! look for major keywords
      CLEN= 4
      CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
      CALL DMPKEY

      IF (ECOUNT .EQ. 0) THEN
        ! a clean run data set was found, interpret it
        SGRP= 58
        CALL HDMEST (MSGFL,SCLU,SGRP)
        MKFILS= 1
        CALL INTERP (SCLU,MKFILS,
     M               FILES,
     O               RETCOD)

      ELSE
        ! no run keyword found
        RETCOD= 3
      END IF

      RETURN

      END SUBROUTINE HSPFXX


      SUBROUTINE HSPF_TREE_BUILD(OPT,OUTLEV)

      ! build tree

      USE TREE

      INTEGER      OPT,OUTLEV

      INTEGER      RWFG,LEN,KEY,REC(50),MXKY,ACNT
      CHARACTER*24 SNAM,TNAM

      RWFG= 0
      LEN = 50

      MXKY= 2
      KEY = 1
      ACNT= 0
      DO WHILE (KEY <= MXKY)
        CALL WORKIO (RWFG,LEN,KEY,
     M               REC,MXKY)
        CALL BLDNAM (OPT,REC(1),
     M               SNAM)
        CALL BLDNAM (OPT,REC(27),
     M               TNAM)
        IF (SNAM(1:1) .NE. ' ' .AND. TNAM(1:1) .NE. ' ') THEN
          IF (OUTLEV .GT. 5) THEN
            WRITE(99,*) 'BUILDH:',KEY,' S:',SNAM,' T:',TNAM
          END IF
          CALL BRAN_ADD(SNAM)
          CALL BRAN_ADD(TNAM)
          CALL CONN_ADD(SNAM,TNAM)
          ACNT = ACNT + 1
        END IF
        KEY = KEY+ 1
      END DO

      END SUBROUTINE HSPF_TREE_BUILD


      SUBROUTINE   BLDNAM
     I                   (OPT,TREC,
     O                    NAM)

      INTEGER      OPT,TREC(8)
      CHARACTER*24 NAM

      EXTERNAL     OPSTUF
      INTEGER      OPSTUF

      INTEGER      ID,I,FG,IT
      CHARACTER*6  TYP

2000  FORMAT(A4,A2)
2010  FORMAT(A6,I4,1X,A4,A2,1X,A4,A2)
2020  FORMAT(A6,2(1X,A4,A2))

      WRITE(TYP,2000) TREC(1),TREC(2)
      IF (TYP(4:6) .EQ. '   ') THEN
        ID = TREC(3)
        FG = 0
      ELSE
        ID = OPSTUF(TREC(3),3)
        IT = OPSTUF(TREC(3),4)
        FG = 1
      END IF
      IF (OPT .EQ. 1) THEN
        WRITE(NAM,2010) TYP,ID,(TREC(I),I=5,8)
      ELSE IF (OPT .EQ. 2) THEN
        IF (FG .EQ. 0) THEN
          WRITE(NAM,2010) TYP,ID,TREC(7),TREC(8)
        ELSE
          WRITE(NAM,2010) TYP,ID
        END IF
      ELSE IF (OPT .EQ. 3) THEN
        IF (FG .EQ. 0) THEN
          ! skip wdm, dss, etc.
          NAM = ' '
        ELSE
          WRITE(NAM,2010) TYP,ID
        END IF
      ELSE IF (OPT .EQ. 4) THEN
        IF (FG .EQ. 0 .OR. (IT.GE.5 .AND.IT.LT.7)) THEN
          ! skip wdm, dss, or utils
          NAM = ' '
        ELSE
          WRITE(NAM,2010) TYP,ID
        END IF
      ELSE IF (OPT .EQ. 5) THEN
        IF (FG .EQ. 0) THEN
          WRITE(NAM,2020) TYP,TREC(7),TREC(8)
        ELSE
          WRITE(NAM,2020) TYP,(TREC(I),I=5,8)
        END IF
      END IF

      END SUBROUTINE BLDNAM


      INTEGER FUNCTION OPSTUF(POS,TYP)
      INTEGER POS,TYP
      INCLUDE 'CRIN1.INC'
      OPSTUF = OPNTAB(TYP,POS)
      END FUNCTION OPSTUF
