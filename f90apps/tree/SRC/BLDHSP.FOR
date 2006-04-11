      SUBROUTINE   INIHSP
     I                   (FILNAM,DBG,
     O                    WDMSFL,RETCOD)

      ! dummy arguments
      CHARACTER*80 FILNAM
      INTEGER      DBG,WDMSFL,RETCOD

      ! local variables
      INTEGER      FILES(15),I,I0,I1,MESSFL,USRFL,MESSU,RCL
      LOGICAL      LFLAG
      CHARACTER*64 HMSNAM
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRMT

      ! externals
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPFXX
      EXTERNAL     ZIPI, DSSCLO, UCIINP, WDBOPN

      ! end specifications

      MESSFL= 9
      I0    = 0
      I1    = 1

      ! avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)

      ! initialize wdm file common block
      CALL WDBFIN

      ! open message file
      INCLUDE 'FHSMES.INC'
      WRITE(*,*) 'INIHSP:HMSNAM "',HMSNAM(1:LEN_TRIM(HMSNAM)),'"'
      CALL WDBOPN (MESSFL,HMSNAM,I1,
     O             RETCOD)
      WRITE(*,*) 'INIHSP:opened MESSFL',RETCOD
      IF (RETCOD.NE.0) THEN
        ! problem with opening message file
        WRITE (*,*) 'INIHSP:Problem:',RETCOD,MESSFL,I,' with ',HMSNAM
      ELSE
        ! open input file
        USRFL= 7
        ACC  = 'SEQUENTIAL'
        FRMT = 'FORMATTED'
        RCL  = 0
        STAT = 'OLD'
        CALL FILOPN (ACC,FRMT,RCL,STAT,USRFL,FILNAM,
     O               RETCOD)
        WRITE(*,*) 'INIHSP:opened USRFL',RETCOD
        IF (RETCOD .EQ. 0) THEN
          ! input file opened, process files block in input file
          ! initialize files to closed
          I = 14
          CALL ZIPI (I,I0,FILES)
          FILES(15) = MESSFL
          WRITE(*,*) 'INIHSP:call FILBLK with',MESSFL,USRFL
          CALL FILBLK (USRFL,
     M                 FILES,
     O                 RETCOD)
          ! back to beginning of input file
          REWIND(USRFL)

          IF (RETCOD .EQ. 0) THEN
            ! file block processed without error - beginning message
            WDMSFL = FILES(11)
            ! read users uci file
            MESSU  = FILES(1)
            CALL UCIINP (USRFL,MESSFL,MESSU)
            WRITE(*,*) 'INIHSP:back from UCIINP'
            ! close users input file
            CLOSE (UNIT=USRFL)
            ! proceed to interp model
            CALL HSPFXX (FILES,
     O                   RETCOD)
            ! simulation complete
            IF (RETCOD .NE. 0) THEN
              WRITE(*,*) 'INIHSP:back from HSPFXX with',RETCOD
            END IF
          ELSE
            ! error in files block
            WRITE(*,*) 'INIHSP:back from FILBLK with',RETCOD
          END IF
        ELSE
          ! error opening uci file
          WRITE(*,*) 'INIHSP:back from FILOPN with',RETCOD
        END IF

        ! close any dss files
        CALL DSSCLO
      END IF

      END SUBROUTINE INIHSP

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
      INTEGER      I0,I1,SCLU,SGRP,MSGFL,INITFG,CLEN,CONT,KCNT,
     $             ECOUNT,KWDDIM(1),KWDTYP(1),MKFILS
      CHARACTER*1  CHSTR1(20),KWDLIB(12)

      ! equivalences
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR

      ! externals
      EXTERNAL  KEYUCI, DMPKEY, INTERP, WMSGTT, INIKEY

      ! input formats
 1000 FORMAT (12A1,2I4)

      ! end specifciations

      I1= 1
      I0= 0
      ! no errors yet
      ECOUNT= 0

      MSGFL= FILES(15)

      SCLU= 201

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


      SUBROUTINE BLDHSP(OPT,DBG)

      ! build tree

      USE TREE

      INTEGER      OPT,DBG

      INTEGER      RWFG,LEN,KEY,REC(50),MXKY,ACNT
      REAL         MFACT
      CHARACTER*24 SNAM,TNAM,CNAM
      CHARACTER*6  STYP,TTYP

      RWFG= 0
      LEN = 50

      MXKY= 2
      KEY = 1
      ACNT= 0
      DO WHILE (KEY <= MXKY)
        CALL WORKIO (RWFG,LEN,KEY,
     M               REC,MXKY)
        CALL BLDNAM (OPT,REC(1),
     M               SNAM,STYP)
        CALL BLDNAM (OPT,REC(27),
     M               TNAM,TTYP)
        IF (SNAM(1:1) .NE. ' ' .AND. TNAM(1:1) .NE. ' ') THEN
          IF (DBG .GT. 5) THEN
            WRITE(99,*) 'BLDHSP:',KEY,' S:',SNAM,' T:',TNAM
          END IF
          CALL BRAN_ADD(SNAM,BTYPE=STYP)
          CALL FILT_ADD(STYP)
          CALL BRAN_ADD(TNAM,BTYPE=TTYP)
          CALL FILT_ADD(TTYP)
          MFACT= TRANSFER(REC(21),MFACT)
          WRITE(CNAM,*) MFACT
          CALL CONN_ADD(SNAM,TNAM,CNAM)
          ACNT = ACNT + 1
        END IF
        KEY = KEY+ 1
      END DO

      END SUBROUTINE BLDHSP


      SUBROUTINE   BLDNAM
     I                   (OPT,TREC,
     O                    NAM,TYP)

      INTEGER      OPT,TREC(8)
      CHARACTER*24 NAM
      CHARACTER*6  TYP

      EXTERNAL     OPSTUF
      INTEGER      OPSTUF

      INTEGER      ID,I,FG,PBLK

2000  FORMAT(A4,A2)
2010  FORMAT(A6,I4,1X,A4,A2,1X,A4,A2)
2020  FORMAT(A6,2(1X,A4,A2))

      WRITE(TYP,2000) TREC(1),TREC(2)
      IF (TYP(5:6) .EQ. '  ' .AND. TYP(4:4) .NE. 'Y') THEN
        ID = TREC(3)
        FG = 0
      ELSE
        ID = OPSTUF(TREC(3),3)
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
          WRITE(NAM,2020) TYP,TREC(7),TREC(8)
        ELSE
          WRITE(NAM,2020) TYP,(TREC(I),I=5,8)
        END IF
      END IF
      ! get rid of extra blanks
      PBLK = 0
      I    = 0
      DO
        I = I+ 1
        IF (LEN_TRIM(NAM(I:24)) == 0) THEN
          EXIT
        ELSE IF (NAM(I:I) == ' ') THEN
          IF (PBLK == 0) THEN
            PBLK = I
          END IF
        ELSE IF (PBLK > 0) THEN
          NAM(PBLK+1:24) = NAM(I:24)
          I    = PBLK+ 1
          PBLK = 0
        END IF
      END DO

      END SUBROUTINE BLDNAM


      INTEGER FUNCTION OPSTUF(POS,TYP)
      INTEGER POS,TYP
      INCLUDE 'CRIN1.INC'

      IF (POS > 0 .AND. POS <= MAXOPN) THEN
        IF (TYP > 0 .AND. TYP <= 20) THEN
          OPSTUF = OPNTAB(TYP,POS)
        ELSE
          WRITE(99,*) 'HSPTSTR:OPSTUF:bad typ:',TYP,POS
          OPSTUF = 0
        END IF
      ELSE
        WRITE(99,*) 'HSPTSTR:OPSTUF:bad pos:',POS,TYP
        OPSTUF = 0
      END IF

      END FUNCTION OPSTUF
