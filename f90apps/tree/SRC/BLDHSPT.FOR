      MODULE BLDHSPT
        PUBLIC  INIHSP,BLDHSP
        PRIVATE

        TYPE HOPS
          CHARACTER*6 :: TYP
          INTEGER     :: CNT
          INTEGER     :: SIZ
        END TYPE HOPS
        TYPE (HOPS),SAVE :: HOP_INFO(0:10)

        INTEGER,SAVE  :: LWDMFL

      CONTAINS

        SUBROUTINE   INIHSP
     I                     (FILNAM,DBG,
     O                      WDMSFL,RETCOD)

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
        EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK
        EXTERNAL     ZIPI, UCIINP, WDBOPN

        ! end specifications

        MESSFL= 9
        I0    = 0
        I1    = 1

        ! avoid some lahey math errors
        LFLAG = .TRUE.
        CALL INVALOP (LFLAG)
        CALL UNDFL (LFLAG)

        HOP_INFO%CNT = 0
        HOP_INFO%TYP = ' '
        ! initialize wdm file common block
        CALL WDBFIN

        ! open message file
        INCLUDE 'FHSMES.INC'
        !WRITE(*,*) 'BLDHSPT:INIHSP: "',HMSNAM(1:LEN_TRIM(HMSNAM)),'"'
        CALL WDBOPN (MESSFL,HMSNAM,I1,
     O               RETCOD)
        !WRITE(*,*) 'BLDHSPT:INIHSP:opened MESSFL',RETCOD
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
     O                 RETCOD)
          !WRITE(*,*) 'BLDHSPT:INIHSP:opened USRFL',RETCOD
          IF (RETCOD .EQ. 0) THEN
            ! input file opened, process files block in input file
            ! initialize files to closed
            I = 14
            CALL ZIPI (I,I0,FILES)
            FILES(15) = MESSFL
            !WRITE(*,*) 'BLDHSPT:INIHSP:call FILBLK with',MESSFL,USRFL
            CALL FILBLK (USRFL,
     M                   FILES,
     O                   RETCOD)
            ! back to beginning of input file
            REWIND(USRFL)

            IF (RETCOD .EQ. 0) THEN
              ! file block processed without error - beginning message
              WDMSFL = FILES(11)
              LWDMFL = WDMSFL
              ! read users uci file
              MESSU  = FILES(1)
              CALL UCIINP (USRFL,MESSFL,MESSU)
              !WRITE(*,*) 'BLDHSPT:INIHSP:back from UCIINP'
              ! close users input file
              CLOSE (UNIT=USRFL)
              ! proceed to interp model
              !WRITE(*,*) 'BLDHSPT:INIHSP:about to call HSPFXX'
              CALL HSPFXX (FILES,
     O                     RETCOD)
              ! simulation complete
              !WRITE(*,*) 'BLDHSPT:INIHSP:back from HSPFXX with',RETCOD
            ELSE
              ! error in files block
              WRITE(*,*) 'BLDHSPT:INIHSP:back from FILBLK with',RETCOD
            END IF
          ELSE
            ! error opening uci file
            WRITE(*,*) 'BLDHSPT:INIHSP:back from FILOPN with',RETCOD
          END IF
        END IF

        END SUBROUTINE INIHSP


        SUBROUTINE   HSPFXX
     I                     (FILES,
     O                      RETCOD)

        ! dummy arguemnts
        INTEGER   FILES(15),RETCOD

        ! argument defintions
        ! FILES  - unit numbers of files from files block in uci
        ! RETCOD - return code - 0:run completed
        !                      1:interp only
        !                      2:errors in interp
        !                      3:no run keyword found

        ! local variables
        INTEGER      I0,I1,SCLU,SGRP,MSGFL,INITFG,CLEN,CONT,KCNT,
     $               ECOUNT,KWDDIM(1),KWDTYP(1),MKFILS
        CHARACTER*1  CHSTR1(20),KWDLIB(12)

        ! equivalences
        EQUIVALENCE (CHSTR1,CHSTR)
        CHARACTER*20 CHSTR

        ! externals
        EXTERNAL  KEYUCI, DMPKEY, INTERP, WMSGTT, INIKEY

        ! input formats
 1000   FORMAT (12A1,2I4)

        ! end specifciations

        I1    = 1
        I0    = 0
        ! no errors yet
        ECOUNT= 0

        MSGFL = FILES(15)

        SCLU  = 201

        ! get major keywords (RUN)
        !WRITE(*,*) 'BLDHSPT:HSPFXX get RUN keyword',MSGFL
        SGRP  = 21
        INITFG= 1
        CLEN  = 20
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1000) KWDLIB,KWDDIM,KWDTYP

        !WRITE(*,*) 'BLDHSPT:HSPFXX got RUN keyword'
        ! look for a fresh set of keywords
        CALL INIKEY

        ! look for major keywords
        CLEN= 4
        CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M               ECOUNT,
     O               KCNT)
        !WRITE(*,*) 'BLDHSPT:HSPFXX found other keywords',KCNT,ECOUNT

        IF (ECOUNT .EQ. 0) THEN
          ! a clean run data set was found, interpret it
          MKFILS= 1
          CALL INTERP (SCLU,MKFILS,
     M                 FILES,
     O                 RETCOD)
          !WRITE(*,*) 'BLDHSPT:HSPFXX done INTERP',RETCOD
        ELSE
          ! no run keyword found
          RETCOD= 3
        END IF

        END SUBROUTINE HSPFXX


        SUBROUTINE   BLDHSP(OPT,DBG)

        ! build tree
        USE TREE, ONLY : BRAN_ADD,CONN_ADD,MXPARM

        INTEGER      OPT,DBG

        INTEGER      RWFG,LEN,KEY,REC(50),MXKY
        INTEGER      SOSV(MXPARM),TOSV(MXPARM)
        REAL         MFACT
        CHARACTER*24 SNAM,TNAM,CNAM
        CHARACTER*6  STYP,TTYP

        RWFG= 0
        LEN = 50

        MXKY= 1
        KEY = 0
        DO WHILE (KEY < MXKY)
          KEY = KEY+ 1
          CALL WORKIO (RWFG,LEN,KEY,
     M                 REC,MXKY)
          CALL BLDNAM (OPT,REC(1),
     M                 SNAM,STYP,SOSV)
          CALL BLDNAM (OPT,REC(27),
     M                 TNAM,TTYP,TOSV)
          IF (SNAM(1:1) .NE. ' ' .AND. TNAM(1:1) .NE. ' ') THEN
            CALL BRAN_ADD(SNAM,BTYPE=STYP,BPARM=SOSV)
            CALL BRAN_ADD(TNAM,BTYPE=TTYP,BPARM=TOSV)
            MFACT= TRANSFER(REC(21),MFACT)
            WRITE(CNAM,*) MFACT
            CALL CONN_ADD(SNAM,TNAM,CNAM)
            IF (DBG .GT. 5) THEN
              WRITE(99,*) 'BLDHSP:',KEY,' S:',SNAM,' T:',TNAM,' C:',CNAM
            END IF
          END IF
        END DO

        END SUBROUTINE BLDHSP


        SUBROUTINE   BLDNAM
     I                     (OPT,TREC,
     O                      NAM,TYP,OSV)

        USE TREE, ONLY : FILT_ADD,FILT_MOD,FILT_ADD_PARM,MXPARM,MXDOPT

        INTEGER      OPT,TREC(8),OSV(MXPARM)
        CHARACTER*24 NAM
        CHARACTER*6  TYP

        INTEGER      ID,I,PBLK,OMCODE,OPTNO,SAIND,SALEN,RETC,TDSFRC
        INTEGER      DOPT(MXDOPT),DCUR
        CHARACTER*1  CTMP1(8)
        EQUIVALENCE (CTMP1,CTMP)
        CHARACTER*8  CTMP

1000    FORMAT(2A4)
2000    FORMAT(A4,A2)
2010    FORMAT(A6,I4,1X,A4,A2,1X,A4,A2)
2020    FORMAT(A6,2(1X,A4,A2))

        WRITE(TYP,2000) TREC(1),TREC(2)

        IF (TYP(1:3) .NE. 'WDM') THEN
          CALL OPSTUF(TREC(3),                          ! opn - sav info
     O                OMCODE,OPTNO,OSV)
          ID     = OPTNO
        ELSE
          OMCODE = 0
          ID     = TREC(3)                               ! wdm - sav dsn
          OSV    = 0
          OSV(1) = ID
          SAIND  = 17
          SALEN  = 1
          CALL WDBSGI(LWDMFL,ID,SAIND,SALEN,OSV(2),RETC)
          SAIND  = 33
          CALL WDBSGI(LWDMFL,ID,SAIND,SALEN,OSV(3),RETC)
          OSV(4) = TREC(7)
          OSV(5) = TREC(8)
          CALL WTFNDT(LWDMFL,ID,SALEN,TDSFRC,OSV(6),OSV(12),RETC)
          SALEN = 8
          SAIND = 288
          CALL WDBSGC(LWDMFL,ID,SAIND,SALEN,CTMP1,RETC)
          READ(CTMP,1000) OSV(18),OSV(19)
          SAIND = 289
          CALL WDBSGC(LWDMFL,ID,SAIND,SALEN,CTMP1,RETC)
          READ(CTMP,1000) OSV(20),OSV(21)
          SAIND = 290
          CALL WDBSGC(LWDMFL,ID,SAIND,SALEN,CTMP1,RETC)
          READ(CTMP,1000) OSV(22),OSV(22)
        END IF
        IF (HOP_INFO(OMCODE)%CNT == 0) THEN
          DOPT   = 0
          HOP_INFO(OMCODE)%TYP = TYP
          CALL FILT_ADD(TYP)
          IF (OMCODE == 0) THEN                             ! wdm
            DOPT(2) = 1                                        ! list
            DOPT(3) = 1                                        ! plot
            DOPT(4) = 1                                        ! parms
            DCUR    = 4
            CALL FILT_ADD_PARM(TYP,'DSN',      1,1,1)
            CALL FILT_ADD_PARM(TYP,'SDATE',    6,6,1)
            CALL FILT_ADD_PARM(TYP,'EDATE',   12,6,1)
            CALL FILT_ADD_PARM(TYP,'SCENARIO',18,2,0)
            CALL FILT_ADD_PARM(TYP,'CONSTITU',20,2,0)
            CALL FILT_ADD_PARM(TYP,'LOCATION',22,2,0)
            CALL FILT_ADD_PARM(TYP,'TCODE',    2,1,1)
            CALL FILT_ADD_PARM(TYP,'TSTEP',    3,1,1)
            CALL FILT_ADD_PARM(TYP,'TSTYP',    4,2,0)
          ELSE IF (OMCODE == 1) THEN                         ! perlnd
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 4
            CALL FILT_ADD_PARM(TYP,'LSID',   37,5,0)
            CALL FILT_ADD_PARM(TYP,'VCSFG', 300,1,1)
            CALL FILT_ADD_PARM(TYP,'VLEFG', 305,1,1)
            CALL FILT_ADD_PARM(TYP,'LZETP', 438,1,2)
            CALL FILT_ADD_PARM(TYP,'LZSN',  308,1,2)
            CALL FILT_ADD_PARM(TYP,'INFILT',309,1,2)
            CALL FILT_ADD_PARM(TYP,'AGWRC', 313,1,2)
            CALL FILT_ADD_PARM(TYP,'DEEPFR',318,1,2)
            CALL FILT_ADD_PARM(TYP,'BASETP',319,1,2)
            CALL FILT_ADD_PARM(TYP,'INTFW', 436,1,2)
            CALL FILT_ADD_PARM(TYP,'IRC',   437,1,2)
            CALL FILT_ADD_PARM(TYP,'KVARY', 312,1,2)
            CALL FILT_ADD_PARM(TYP,'AGWETP',320,1,2)
            CALL FILT_ADD_PARM(TYP,'CEPSC', 433,1,2)
            CALL FILT_ADD_PARM(TYP,'UZSN',  434,1,2)
          ELSE IF (OMCODE == 2) THEN                         ! implnd
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 4
            CALL FILT_ADD_PARM(TYP,'LSID',   37,5,0)
          ELSE IF (OMCODE == 3) THEN                         ! rchres
            DOPT(1) = 1                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'RCHID',  37,5,0)
          ELSE IF (OMCODE == 4) THEN                         ! copy
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'NPT'  ,   2,1,1)
            CALL FILT_ADD_PARM(TYP,'NMN'  ,   3,1,1)
          ELSE IF (OMCODE == 5) THEN                         ! pltgen
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'TITLE',  30,6,0)
            CALL FILT_ADD_PARM(TYP,'NCURV',   6,1,1)
          ELSE IF (OMCODE == 6) THEN                         ! disply
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'TITLE',   1,6,0)
          ELSE IF (OMCODE == 7) THEN                         ! duranl
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'TITLE',   1,6,0)
          ELSE IF (OMCODE == 8) THEN                         ! gener
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'OPCODE', 30,1,0)
          ELSE IF (OMCODE == 9) THEN                         ! mutsin
            DOPT(1) = 2                                        ! conn
            DOPT(4) = 1                                        ! parms
            DCUR    = 1
            CALL FILT_ADD_PARM(TYP,'NPT'  ,   2,1,1)
            CALL FILT_ADD_PARM(TYP,'NMN'  ,   3,1,1)
          END IF
          CALL FILT_MOD(TYP,DOPT=DOPT,DCUR=DCUR)
        END IF
        HOP_INFO(OMCODE)%CNT = HOP_INFO(OMCODE)%CNT+ 1

        IF (OPT .EQ. 1) THEN
          WRITE(NAM,2010) TYP,ID,(TREC(I),I=5,8)
        ELSE IF (OPT .EQ. 2) THEN
          IF (OMCODE .EQ. 0) THEN                            ! wdm
            WRITE(NAM,2010) TYP,ID,TREC(7),TREC(8)
          ELSE                                               ! opn
            WRITE(NAM,2010) TYP,ID
          END IF
        ELSE IF (OPT .EQ. 3) THEN
          IF (OMCODE .EQ. 0) THEN
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


        SUBROUTINE   OPSTUF(POS,
     O                      OMCODE,OPTNO,OSVF)
        INTEGER POS,OMCODE,OPTNO,OSVF(500)
        INCLUDE 'CRIN1.INC'

        IF (POS > 0 .AND. POS <= MAXOPN) THEN
          OMCODE = OPNTAB(4,POS)
          OPTNO  = OPNTAB(3,POS)
          CALL GETOSV(OPNTAB(7,POS),OPNTAB(7,POS),500,
     O                OSVF)                          ! first part of osv
        ELSE
          WRITE(99,*) 'BLDHSP:OPSTUF:bad pos:',POS
          OMCODE = 0
          OPTNO  = 0
          OSVF   = 0
        END IF

        END SUBROUTINE OPSTUF

      END MODULE BLDHSPT
