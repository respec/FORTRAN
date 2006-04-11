      MODULE TREE
        PUBLIC BRAN_ADD,BRAN_DEL,BRAN_GET_PARM
        PUBLIC CONN_ADD
        PUBLIC FILT_ADD,FILT_ADD_PARM,FILT_LIS,FILT_MOD
        PUBLIC DISP_LIS
        PUBLIC TREE_INI,TREE_SET,TREE_DMP,TREE_SUM,TREE_ROOT
        PUBLIC MXLEV,MXCON,MXDOPT,MXPARM
        PRIVATE

        INTEGER    MXLEV,MXCON,MXDOPT,MXPARM,MXPCNT
        PARAMETER (MXLEV=50,MXCON=20,MXDOPT=5,MXPARM=500,MXPCNT=10)

        TYPE CONN
          CHARACTER(LEN=24)       :: NAME      ! of connector
          TYPE (BRAN), POINTER    :: BUPT      ! up branch pointer
          TYPE (BRAN), POINTER    :: BDPT      ! down branch pointer
          TYPE (CONN), POINTER    :: CNXU      ! next connector up side
          TYPE (CONN), POINTER    :: CNXD      ! prev connector dn side
        END TYPE CONN

        TYPE BRAN
          CHARACTER(LEN=24)       :: NAME         ! of branch
          CHARACTER(LEN=80)       :: DESC         ! description
          CHARACTER(LEN=16)       :: UNTS
          CHARACTER(LEN=16)       :: TYPE
          INTEGER                 :: PARM(MXPARM)
          TYPE (BRAN), POINTER    :: BNXT         ! next branch
          TYPE (BRAN), POINTER    :: BPRV         ! prev branch
          TYPE (CONN), POINTER    :: CUPT         ! first up connector
          INTEGER                 :: UPCT         ! count of up connectors
          TYPE (CONN), POINTER    :: CDPT         ! first down connector
          INTEGER                 :: DNCT         ! count of down connnectors
        END TYPE BRAN
        TYPE (BRAN),TARGET,SAVE   :: BRAN_FIRST ! instance of first branch

        TYPE DISP
          TYPE(BRAN), POINTER     :: BP
          INTEGER                 :: LEV
          INTEGER                 :: POS
          INTEGER                 :: CNT
          INTEGER                 :: CONID(MXCON)
          CHARACTER(LEN=24)       :: PCONAM
          TYPE(DISP), POINTER     :: NXT
        END TYPE DISP
        TYPE (DISP),TARGET,SAVE   :: DISP_FIRST

        TYPE FILT
          CHARACTER(LEN=24)       :: NAME
          INTEGER                 :: CNT
          INTEGER                 :: DOPT(MXDOPT)
          INTEGER                 :: DCUR
          LOGICAL                 :: UFLG
          INTEGER                 :: PCNT
          CHARACTER(LEN=16)       :: PNAM(MXPCNT)
          INTEGER                 :: PPOS(MXPCNT)
          INTEGER                 :: PSIZ(MXPCNT)
          INTEGER                 :: PTYP(MXPCNT)
          TYPE(FILT), POINTER     :: NXT
        END TYPE FILT
        TYPE (FILT),TARGET,SAVE   :: FILT_FIRST

        INTEGER,SAVE  :: BCNT = 0          ! branch count
        INTEGER,SAVE  :: CCNT = 0          ! connection count
        INTEGER,SAVE  :: FCNT = 0          ! filter count
        INTEGER,SAVE  :: DBG  = 0          ! debug option, more with higher

      CONTAINS

        SUBROUTINE TREE_SET (NDBG)
          INTEGER, INTENT(IN), OPTIONAL  :: NDBG

          IF (PRESENT(NDBG)) DBG = NDBG

        END SUBROUTINE TREE_SET

        SUBROUTINE TREE_INI

          INTEGER                   :: LBCNT,LCUCNT,LCDCNT,LFCNT,I
          TYPE (BRAN),   POINTER    :: BP,TBP
          TYPE (CONN),   POINTER    :: CP,TCP
          TYPE (FILT),   POINTER    :: FP,TFP

          LBCNT = 0
          LCUCNT= 0
          LCDCNT= 0
          IF (ASSOCIATED(BRAN_FIRST%BNXT)) THEN
            BP => BRAN_FIRST%BNXT
            DO
              IF (ASSOCIATED(BP%CUPT)) THEN
                CP => BP%CUPT
                I  = 0
                DO
                  I      =  I+ 1
                  TCP    => CP%CNXU
                  LCUCNT =  LCUCNT + 1
                  IF (DBG>4) THEN
                    IF (I > 1) THEN
                      WRITE(99,*) 'TREE_INI:deCPU ',REPEAT(' ',26),
     $                            LCUCNT,POINTER(CP),I,' "',CP%NAME,'"'
                    ELSE
                      WRITE(99,*) 'TREE_INI:deCPU "',BP%NAME,'"',
     $                            LCUCNT,POINTER(CP),I,' "',CP%NAME,'"'
                    END IF
                  END IF
                  IF (ASSOCIATED(CP)) THEN
                    DEALLOCATE(CP)
                  END IF
                  CP => TCP
                  IF (.NOT.ASSOCIATED(CP)) THEN
                    IF (I .NE. BP%UPCT) THEN
                      WRITE(99,*) 'TREE_INI:up count mismatch ',BP%NAME
                    END IF
                    EXIT
                  END IF
                END DO
              END IF

              TBP => BP
              BP  => BP%BNXT
              LBCNT = LBCNT+ 1
              IF (DBG>6) THEN
                WRITE(99,*) 'TREE_INI:de BP "',TBP%NAME,'"',
     $                      LBCNT,POINTER(TBP)
              END IF
              IF (ASSOCIATED(TBP)) THEN
                DEALLOCATE(TBP)
              ELSE
                WRITE(99,*) 'TREE_INI:no pointer'
              END IF
              IF (.NOT.ASSOCIATED(BP)) THEN
                EXIT
              END IF
            END DO

            NULLIFY(BRAN_FIRST%BNXT)

          ELSE
            IF (DBG>3) THEN
              WRITE(99,*)'TREE_INI:cnt ',BCNT,LBCNT,CCNT,LCUCNT,LCDCNT
            END IF
            CCNT= 0
            BCNT= 0
          END IF

          IF (ASSOCIATED(FILT_FIRST%NXT)) THEN
            LFCNT = 0
            FP  => FILT_FIRST%NXT
            DO WHILE (ASSOCIATED(FP))
              TFP => FP%NXT
              DEALLOCATE(FP)
              LFCNT = LFCNT+ 1
              FP  => TFP
            END DO
            NULLIFY (FILT_FIRST%NXT)
            IF (LFCNT .NE. FCNT) THEN
              WRITE(99,*) 'TREE_INI:filt count wrong:',FCNT,LFCNT
            END IF
            FCNT = 0
          END IF

        END SUBROUTINE TREE_INI

        SUBROUTINE TREE_DMP

          TYPE (BRAN),       POINTER    :: BP
          TYPE (CONN),       POINTER    :: CP
          TYPE (FILT),       POINTER    :: FP

          IF (ASSOCIATED(BRAN_FIRST%BNXT)) THEN
            WRITE(99,*) 'TREE_DMP:branches'
            BP => BRAN_FIRST%BNXT
            DO
              WRITE(99,*) BP%NAME,BP%UPCT,BP%DNCT,POINTER(BP)
              IF (BP%UPCT > 0) THEN                   ! up connectors
                CP => BP%CUPT
                DO I = 1, BP%UPCT
                  WRITE(99,*) '    up',I,POINTER(CP)
                  IF (I < BP%UPCT) THEN
                    CP => CP%CNXU
                  END IF
                END DO
              END IF
              IF (BP%DNCT > 0) THEN                   ! down connectors
                CP => BP%CDPT
                DO I = 1, BP%DNCT
                  WRITE(99,*) '    dn',I,POINTER(CP)
                  IF (I < BP%DNCT) THEN
                    CP => CP%CNXD
                  END IF
                END DO
              END IF
              IF (ASSOCIATED(BP%BNXT)) THEN
                BP => BP%BNXT
              ELSE
                WRITE(99,*) 'TREE_DMP:done branches'
                EXIT
              END IF
            END DO
          ELSE
            WRITE(99,*) 'TREE_DMP:no branches to dump'
          END IF

          IF (ASSOCIATED(FILT_FIRST%NXT)) THEN
            WRITE(99,*) 'TREE_DMP:filters'
            FP => FILT_FIRST%NXT
            DO
              WRITE(99,*) FP%NAME,FP%CNT,FP%UFLG,POINTER(FP),FP%DOPT
              FP => FP%NXT
              IF (.NOT. ASSOCIATED(FP)) THEN
                WRITE(99,*) 'TREE_DMP:done filters'
                EXIT
              END IF
            END DO
          ELSE
            WRITE(99,*) 'TREE_DMP:no filters to dump'
          END IF

        END SUBROUTINE TREE_DMP

        SUBROUTINE TREE_ROOT (NAME)
          CHARACTER(LEN=24),INTENT(OUT)        :: NAME   ! of first root

          INTEGER                              :: UPCT,DNCT,IND
          CHARACTER(LEN=24)                    :: DNAM

          IF (ASSOCIATED(BRAN_FIRST%BNXT)) THEN
            NAME = BRAN_FIRST%BNXT%NAME
            DO
              CALL BRAN_GET(NAME,UPCT,DNCT)
              IF (DBG > 9) WRITE(99,*) 'TREE_ROOT: ',NAME,UPCT,DNCT
              IF (DNCT > 0) THEN
                IF (DNCT > 1) WRITE(99,*) 'TREE_ROOT:too many dn',DNCT
                IND = 1
                CALL CONN_GET(NAME,IND,'D',DNAM)
                NAME = DNAM
              ELSE
                WRITE(99,*) 'TREE_ROOT is ',NAME
                EXIT
              END IF
            END DO
          ELSE
            NAME = 'Unknown'
          END IF

        END SUBROUTINE TREE_ROOT

        SUBROUTINE TREE_SUM (LEVUSE,LEVCNT,LEVPOSMX,BNAME,BDIR)
          INTEGER, INTENT(OUT)                 :: LEVUSE
          INTEGER, INTENT(OUT)                 :: LEVCNT(*)
          INTEGER, INTENT(OUT)                 :: LEVPOSMX(*)
          CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: BNAME
          CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: BDIR

          CHARACTER(LEN=1)                     :: LDIR
          CHARACTER(LEN=24)                    :: LNAM
          TYPE (BRAN), POINTER                 :: LBP
          TYPE (DISP), POINTER                 :: LDP, LNDP
          INTEGER                              :: LEV,CDP,I

          IF (PRESENT(BNAME)) THEN         ! use name if present
            WRITE(99,*) 'TREE_SUM:look for ',BNAME
            LBP => BRAN_POS(BNAME)
          ELSE                             ! default to first name in list
            WRITE(99,*) 'TREE_SUM:look for ',BRAN_FIRST%BNXT%NAME
            LBP => BRAN_FIRST%BNXT
          END IF
          LNAM = LBP%NAME

          IF (PRESENT(BDIR)) THEN          ! use direction if present
            IF (BDIR == 'D') THEN
              LDIR = 'D'
            ELSE
              LDIR = 'U'
            END IF
          ELSE                             ! default to up
            LDIR = 'U'
          END IF

          IF (ASSOCIATED(DISP_FIRST%NXT)) THEN     ! get rid of prev display
            LDP => DISP_FIRST%NXT
            DO
              LNDP => LDP%NXT
              DEALLOCATE(LDP)
              IF (ASSOCIATED(LNDP)) THEN
                LDP => LNDP
              ELSE
                EXIT
              END IF
            END DO
            NULLIFY(DISP_FIRST%NXT)
          END IF

          LEVCNT(1:MXLEV)  = 0
          LEVPOSMX(1:MXLEV)= 0
          IF (ASSOCIATED(LBP)) THEN        ! calc levels
            CDP      =  0
            LEV      =  1
            LEVUSE   =  1
            LDP      => DISP_FIRST
            CALL TREE_LEV (LBP%NAME,LDIR,
     M                     LEV,LEVUSE,LEVCNT,LEVPOSMX,CDP,LDP)
          ELSE                         ! bad or no branch
            LEVUSE = 0
          END IF

          IF (DBG>5) THEN
            I = SUM(LEVCNT(1:LEVUSE))
            WRITE(99,*) 'TREE_SUM:disp ',LDIR,' ',LNAM,LEVUSE,I
            IF (I > 0) THEN
              WRITE(99,*) '             cnts',LEVCNT(1:LEVUSE)
              WRITE(99,*) '             maxs',LEVPOSMX(1:LEVUSE)
            END IF
          END IF

        END SUBROUTINE TREE_SUM

        RECURSIVE SUBROUTINE TREE_LEV (NAME,DIR,
     M                                 LEV,LEVUSE,LEVCNT,LEVPOSMX,
     M                                 CDP,LDP)
          CHARACTER(LEN=24), INTENT(IN)  :: NAME
          CHARACTER(LEN=1),  INTENT(IN)  :: DIR
          INTEGER                        :: LEV
          INTEGER                        :: LEVUSE
          INTEGER                        :: LEVCNT(*)
          INTEGER                        :: LEVPOSMX(*)
          INTEGER                        :: CDP
          TYPE(DISP), POINTER            :: LDP

          INTEGER                        :: UPCT,DNCT,FLEN,LPOS
          LOGICAL                        :: USE_IT
          LOGICAL, SAVE                  :: UPFLG
          TYPE (FILT), POINTER           :: FP

          TYPE TDPT
            CHARACTER(LEN=24)            :: CNAM
            CHARACTER(LEN=24)            :: BNAM
            INTEGER                      :: CNT
            INTEGER                      :: UCNT
            INTEGER                      :: POS
            INTEGER                      :: IND
            TYPE (DISP), POINTER         :: TLDP
            TYPE (TDPT), POINTER         :: NEXT
            TYPE (TDPT), POINTER         :: PREV
          END TYPE TDPT
          TYPE (TDPT),TARGET             :: TDPT_FIRST
          TYPE (TDPT),POINTER,SAVE       :: TDPT_NOW

          USE_IT = .TRUE.
          IF (ASSOCIATED(FILT_FIRST%NXT)) THEN
            FP     => FILT_FIRST%NXT
            FLEN   =  SCAN(NAME,' ')
            DO WHILE (ASSOCIATED(FP))
              IF (FP%NAME(1:FLEN) == NAME(1:FLEN)) THEN
                USE_IT = FP%UFLG
                EXIT
              ELSE
                FP => FP%NXT
              END IF
            END DO
          END IF

          !WRITE(99,*) 'TREE_LEV:',NAME,USE_IT,LEV

          IF (USE_IT) THEN
            IF (LEV > LEVUSE) THEN
              LEVUSE = LEV
            END IF

            CALL BRAN_GET(NAME,UPCT,DNCT)

            IF (LEV < MXLEV) THEN
              CDP           =  CDP + 1                      ! count of disps
              ALLOCATE(LDP%NXT)
              LDP           => LDP%NXT
              LDP%BP        => BRAN_POS(NAME)
              LDP%LEV       =  LEV
              LEVCNT(LEV)   =  LEVCNT(LEV) + 1
              LEVPOSMX(LEV) =  LEVPOSMX(LEV)+ 1
              IF (DBG>7) THEN
                WRITE(99,*)'TREE_LEV:pos:',LEV,LEVCNT(LEV),LEVPOSMX(LEV)
              END IF
              LDP%POS       =  LEVPOSMX(LEV)
              IF (LEV > 1) THEN
                TDPT_NOW%UCNT = TDPT_NOW%UCNT+ 1
                LDP%PCONAM    = TDPT_NOW%CNAM
                IF (DBG>7) THEN
                  WRITE(99,*) 'TDPT_NOW new one',LEV,POINTER(TDPT_NOW)
                END IF
                ALLOCATE(TDPT_NOW%NEXT)
                TDPT_NOW%NEXT%PREV => TDPT_NOW
                TDPT_NOW           => TDPT_NOW%NEXT
              ELSE
                IF (DBG>7) THEN
                  WRITE(99,*) 'TDPT_NOW point to first'
                END IF
                LDP%PCONAM =  'Root'
                TDPT_NOW      => TDPT_FIRST
              END IF
              IF (DBG>7) THEN
                WRITE(99,2002) LEV,POINTER(TDPT_NOW),POINTER(LDP)
 2002           FORMAT('TDPT_NOW at ',I4,2I12)
              END IF
              TDPT_NOW%TLDP => LDP
              TDPT_NOW%BNAM =  NAME
              TDPT_NOW%POS  =  0
              TDPT_NOW%UCNT =  0
              IF (DIR == 'U') THEN
                TDPT_NOW%CNT= UPCT
              ELSE
                TDPT_NOW%CNT= DNCT
              END IF
              IF (LEV > 1) THEN
                TDPT_NOW%PREV%IND = CDP
              END IF

              DO WHILE (TDPT_NOW%POS < TDPT_NOW%CNT)
                TDPT_NOW%POS = TDPT_NOW%POS+ 1
                IF (TDPT_NOW%POS <= MXLEV) THEN
                  CALL CONN_GET(NAME,TDPT_NOW%POS,DIR,
     O                          TDPT_NOW%BNAM,TDPT_NOW%CNAM)

                  IF (DBG>7) THEN
                    WRITE(99,2000) NAME,TDPT_NOW%BNAM,TDPT_NOW%CNAM,
     #                             TDPT_NOW%POS,LEV
 2000               FORMAT ('TREE_LEV:conn',3(' "',A24,'"'),I4,I4)
                  END IF

                  LEV   = LEV+ 1
                  UPFLG = .TRUE.
                  CALL TREE_LEV (TDPT_NOW%BNAM,
     M                           DIR,LEV,LEVUSE,
     M                           LEVCNT,LEVPOSMX,CDP,LDP)

                  IF (DBG>7) THEN
                    WRITE(99,2005) TDPT_NOW%CNAM,LEV,TDPT_NOW%POS,
     #                             TDPT_NOW%IND,TDPT_NOW%UCNT,
     #                             POINTER(TDPT_NOW),
     #                             POINTER(TDPT_NOW%TLDP)
 2005               FORMAT('TREE_LEV:save "',A24,'"',4I4,2I12)
                  END IF

                  LPOS = TDPT_NOW%UCNT
                  IF (LPOS > 0) THEN
                    TDPT_NOW%TLDP%CONID(LPOS) = TDPT_NOW%IND
                  END IF
                  TDPT_NOW%TLDP%CNT         = TDPT_NOW%UCNT
                ELSE
                  WRITE(99,*) 'TREE_LEV:too many connections'
                  WRITE(*,*)  'TREE_LEV:too many connections'
                  STOP
                END IF

              END DO
              IF (DBG>6) THEN
                WRITE(99,2010) NAME,LEV,TDPT_NOW%CNT,
     #                         LEVUSE,LEVCNT(1:LEVUSE)
 2010           FORMAT('TREE_LEV:fnd  "',A24,'" lv:',I4,' nx',I4,
     #                 ' mx:',I4,' ct:',(20I4))
              END IF
              IF (LEV < MXLEV .AND. UPFLG) THEN
                LEVPOSMX(LEV+1:MXLEV) = LEVPOSMX(LEV+1:MXLEV) + 1
              END IF
              IF (LEV > 1) THEN
                IF (DBG>7) THEN
                  WRITE(99,*) 'TDPT_NOW dn',LEV,POINTER(TDPT_NOW)
                END IF
                TDPT_NOW => TDPT_NOW%PREV
                DEALLOCATE(TDPT_NOW%NEXT)
              ELSE
                NULLIFY(TDPT_FIRST%NEXT)
                IF (DBG>7) THEN
                  WRITE(99,*) 'TDPT_NOW bt',LEV,POINTER(TDPT_FIRST)
                END IF
              END IF

              UPFLG = .FALSE.

            ELSE
              IF (DBG>2) WRITE(99,*) 'TREE_LEV:too many levels'
            END IF
          ELSE
            !WRITE(*,*) 'TREE_LEV:skip ',LEV,' ',NAME
          END IF

          LEV   = LEV - 1

        END SUBROUTINE TREE_LEV

        FUNCTION BRAN_POS (BNAME) RESULT (BP)
          CHARACTER(LEN=*), INTENT(IN) :: BNAME
          TYPE (BRAN),      POINTER    :: BP

          BP => BRAN_FIRST
          DO
            IF (BNAME == BP%NAME) THEN
              IF (DBG>7) THEN
                WRITE(99,*)'BRAN_POS:fnd  "',BNAME,'"',POINTER(BP)
              END IF
              EXIT
            ELSE IF (.NOT. ASSOCIATED(BP%BNXT)) THEN
              ALLOCATE (BP%BNXT)
              BCNT= BCNT+ 1
              BP%BNXT%BPRV => BP
              BP => BP%BNXT
              IF (DBG>3) THEN
                WRITE(99,*) 'BRAN_POS:allc',BCNT,POINTER(BP),
     $                      ' "',BNAME,'"'
              END IF
              BP%NAME = BNAME
              BP%DESC = ' '
              BP%UNTS = ' '
              BP%TYPE = ' '
              EXIT
            ELSE
              BP => BP%BNXT
            END IF
          END DO

        END FUNCTION BRAN_POS

        SUBROUTINE BRAN_ADD (BNAME,BDESC,BUNTS,BTYPE,BPARM)
          CHARACTER(LEN=*), INTENT(IN)           :: BNAME
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: BDESC
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: BUNTS, BTYPE
          INTEGER         , INTENT(IN), OPTIONAL :: BPARM(MXPARM)

          TYPE (BRAN),       POINTER    :: BP

          BP => BRAN_POS(BNAME)
          BP%NAME = BNAME
          IF (PRESENT(BDESC)) THEN
            BP%DESC = BDESC
          END IF
          IF (PRESENT(BUNTS)) THEN
            BP%UNTS = BUNTS
          END IF
          IF (PRESENT(BTYPE)) THEN
            BP%TYPE = BTYPE
          END IF
          IF (PRESENT(BPARM)) THEN
            BP%PARM = BPARM
          END IF

        END SUBROUTINE BRAN_ADD

        SUBROUTINE BRAN_LIS (FLG,NAME)
          INTEGER                       :: FLG
          CHARACTER(LEN=*)              :: NAME

          TYPE (BRAN),SAVE,  POINTER    :: BP

          IF (FLG == -1) THEN
            BP  => BRAN_FIRST%BNXT
            FLG =  0
          END IF

          IF (ASSOCIATED(BP)) THEN
            NAME =  BP%NAME
            BP   => BP%BNXT
          ELSE
            NAME = ' '
            FLG  = 1
          END IF

        END SUBROUTINE BRAN_LIS

        SUBROUTINE BRAN_GET (NAME,UPCT,DNCT,DESC,UNTS,TYPE)
          CHARACTER(LEN=24), INTENT(IN)            :: NAME
          INTEGER,           INTENT(OUT), OPTIONAL :: UPCT, DNCT
          CHARACTER(LEN=80), INTENT(OUT), OPTIONAL :: DESC
          CHARACTER(LEN=16), INTENT(OUT), OPTIONAL :: UNTS, TYPE

          TYPE (BRAN),       POINTER     :: BP

          IF (DBG>6) WRITE(99,*) 'BRAN_GET:gpos "',NAME,'"'
          BP => BRAN_POS(NAME)
          IF (DBG>5) THEN
            WRITE(99,*) 'BRAN_GET:detl "',BP%NAME,'"',BP%UPCT,BP%DNCT
          END IF
          IF (PRESENT(UPCT)) THEN
            UPCT = BP%UPCT
          END IF
          IF (PRESENT(DNCT)) THEN
            DNCT = BP%DNCT
          END IF
          IF (PRESENT(DESC)) THEN
            DESC= BP%DESC
          END IF
          IF (PRESENT(UNTS)) THEN
            UNTS= BP%UNTS
          END IF
          IF (PRESENT(TYPE)) THEN
            TYPE= BP%TYPE
          END IF

        END SUBROUTINE BRAN_GET

        SUBROUTINE BRAN_GET_PARM (NAME,IND,MXSIZ,PNAM,PSIZ,PVAL,PTYP)
          CHARACTER(LEN=24), INTENT(IN)  :: NAME
          INTEGER,           INTENT(IN)  :: IND
          INTEGER,           INTENT(IN)  :: MXSIZ
          CHARACTER(LEN=16), INTENT(OUT) :: PNAM
          INTEGER,           INTENT(OUT) :: PSIZ
          INTEGER,           INTENT(OUT) :: PVAL(MXSIZ)
          INTEGER,           INTENT(OUT) :: PTYP

          INTEGER                  :: LPOS,LEND
          CHARACTER(LEN=16)        :: BTYPE
          TYPE (BRAN), POINTER     :: BP
          TYPE (FILT), POINTER     :: FP

          PNAM  = ' '
          PVAL  = 0
          PTYP  = -1
          PSIZ  = 0

          BP    => BRAN_POS(NAME)
          BTYPE =  BP%TYPE
          IF (LEN_TRIM(BTYPE) > 0) THEN
            FP  => FILT_POS(BTYPE)
            IF (IND <= FP%PCNT) THEN
              PNAM = FP%PNAM(IND)
              PTYP = FP%PTYP(IND)
              LPOS = FP%PPOS(IND)
              PSIZ = FP%PSIZ(IND)
              IF (PSIZ > MXSIZ) THEN
                PSIZ = MXSIZ
              END IF
              LEND = LPOS+ PSIZ- 1
              PVAL(1:PSIZ) = BP%PARM(LPOS:LEND)
            END IF
          END IF
        END SUBROUTINE BRAN_GET_PARM

        SUBROUTINE BRAN_DEL (BNAME)
          CHARACTER(LEN=24), INTENT(IN)   :: BNAME

          INTEGER               :: I, J
          TYPE (BRAN), POINTER  :: BP, TBP
          TYPE (CONN), POINTER  :: NCP, PCP, UCP, DCP, TCP, OCP

          BP => BRAN_POS(BNAME)
          IF (ASSOCIATED(BP)) THEN
            IF (DBG > 5) THEN
              WRITE(99,*) 'BRAN_DEL:d&u ',BNAME,' ',BP%DNCT,BP%UPCT
            END IF

            IF (BP%DNCT == 0) THEN
              !nothing below - all above now terminate
              IF (DBG > 7) WRITE(99,*) 'BRAN_DEL:no below'
              ! need to delete connections down to this branch
              NCP => BP%CUPT
              DO I = 1, BP%UPCT
                ! loop thru all connections
                IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:del up ',I,BP%UPCT
                PCP => NCP
                !  dealloc branch pointer above this connection
                TBP => PCP%BUPT
                TCP => TBP%CDPT
                J = 0
                DO
                  J = J+ 1
                  IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:del bp ',J
                  IF (POINTER(TCP) == POINTER(PCP)) THEN   ! not 90
                    IF (DBG > 8) THEN
                      WRITE(99,*) 'BRAN_DEL:del bp match',PCP%BUPT%DNCT
                    END IF
                    IF (J == PCP%BUPT%DNCT) THEN
                      IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:nullify',J
                      IF (J == 1) THEN
                        NULLIFY (TBP%CDPT)
                      ELSE
                        NULLIFY (OCP%CNXD)
                      END IF
                    ELSE
                      IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:skip',J
                      IF (J == 1) THEN
                        TBP%CDPT => TCP%CNXD
                      ELSE
                        OCP%CNXD => TCP%CNXD
                      END IF
                    END IF
                    EXIT
                  ELSE
                    WRITE(99,*) 'BRAN_DEL:try',POINTER(TCP%CNXD)
                    OCP => TCP
                    TCP => TCP%CNXD
                  END IF
                END DO
                PCP%BUPT%DNCT = PCP%BUPT%DNCT - 1
                IF (DBG > 6) WRITE(99,*) 'BRAN_DEL:dcon ',POINTER(PCP)
                DEALLOCATE(PCP)
                CCNT = CCNT - 1
              END DO
            ELSE IF (BP%UPCT == 0) THEN
              !nothing above - all below now terminate
              IF (DBG > 7) WRITE(99,*) 'BRAN_DEL:no above'
              NCP => BP%CDPT
              DO I = 1, BP%DNCT
                ! loop thru all connections
                IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:del dn ',I,BP%DNCT
                PCP => NCP
                !  dealloc branch pointer below this connection
                TBP => PCP%BDPT
                TCP => TBP%CUPT
                J = 0
                DO
                  J = J+ 1
                  IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:del bp ',J
                  IF (POINTER(TCP) == POINTER(PCP)) THEN   ! not 90
                    IF (DBG > 8) THEN
                      WRITE(99,*) 'BRAN_DEL:del bp match',PCP%BDPT%UPCT
                    END IF
                    IF (J == PCP%BDPT%UPCT) THEN
                      IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:nullify',J
                      IF (J == 1) THEN
                        NULLIFY (TBP%CUPT)
                      ELSE
                        NULLIFY (OCP%CNXU)
                      END IF
                    ELSE
                      IF (DBG > 8) WRITE(99,*) 'BRAN_DEL:skip',J
                      IF (J == 1) THEN
                        TBP%CUPT => TCP%CNXU
                      ELSE
                        OCP%CNXU => TCP%CNXU
                      END IF
                    END IF
                    EXIT
                  ELSE
                    WRITE(99,*) 'BRAN_DEL:try',POINTER(TCP%CNXU)
                    OCP => TCP
                    TCP => TCP%CNXU
                  END IF
                END DO
                PCP%BDPT%UPCT = PCP%BDPT%UPCT - 1
                IF (DBG > 6) WRITE(99,*) 'BRAN_DEL:dcon ',POINTER(PCP)
                DEALLOCATE(PCP)
                CCNT = CCNT - 1
              END DO
            ELSE
              !fix up and down connections
              DCP => BP%CDPT
              DO I = 1, BP%DNCT
                UCP => BP%CUPT
                DO J = 1, BP%UPCT
                  IF (DBG > 8) THEN
                    WRITE(99,*) 'BRAN_DEL',I,J,POINTER(UCP),POINTER(DCP)
                    WRITE(99,*) '  UU:',UCP%BUPT%NAME,POINTER(UCP%BUPT)
                    WRITE(99,*) '  UD:',UCP%BDPT%NAME,POINTER(UCP%BDPT)
                    WRITE(99,*) '  DU:',DCP%BUPT%NAME,POINTER(DCP%BUPT)
                    WRITE(99,*) '  DD:',DCP%BDPT%NAME,POINTER(DCP%BDPT)
                  END IF
                  ! maybe not quite right -
                  UCP%BDPT => DCP%BDPT
                  WRITE(99,*) 'BRAN_DEL:NUD:',UCP%BDPT%NAME,
     $                      POINTER(UCP%BDPT),UCP%BDPT%UPCT
                  DCP%BUPT => UCP%BUPT
                  WRITE(99,*) 'BRAN_DEL:NDU:',DCP%BUPT%NAME,
     $                      POINTER(DCP%BUPT),UCP%BUPT%DNCT
                  ! on to the BNXT pointer
                  UCP => UCP%CNXU
                END DO
                PCP => DCP
                DCP => DCP%CNXD
                IF (DBG > 6) THEN
                  WRITE(99,*) 'BRAN_DEL:remove connection ',POINTER(PCP)
                END IF
                DEALLOCATE(PCP)
                CCNT = CCNT - 1
              END DO
            END IF

            IF (ASSOCIATED(BP%BPRV)) THEN
              IF (ASSOCIATED(BP%BNXT)) THEN
                IF (DBG > 5) THEN
                  WRITE(99,*) 'BRAN_DEL:nptr "',BP%BPRV%NAME,'"  "',
     $                                          BP%BNXT%NAME,'"'
                END IF
                BP%BNXT%BPRV => BP%BPRV
                BP%BPRV%BNXT => BP%BNXT
              ELSE
                IF (DBG > 5) THEN
                  WRITE(99,*)'BRAN_DEL:deal BPRV ptr: ',BP%BPRV%NAME
                END IF
                DEALLOCATE(BP%BPRV%BNXT)
                BCNT = BCNT - 1
              END IF
            ELSE
              IF (DBG > 5) THEN
                WRITE(99,*)'BRAN_DEL:deal BNXT ptr: ',BP%BNXT%NAME
              END IF
              DEALLOCATE(BP%BNXT%BPRV)
              BCNT = BCNT - 1
            END IF

            IF (DBG > 4) THEN
              WRITE(99,*) 'BRAN_DEL:doit "',BP%NAME,'"',POINTER(BP)
            END IF
            DEALLOCATE (BP)
            BCNT = BCNT - 1
          ELSE
            WRITE(99,*) 'BRAN_DEL:unknown branch: ',BNAME
          END IF

        END SUBROUTINE BRAN_DEL


        SUBROUTINE CONN_ADD (UNAME,DNAME,CNAME)
          CHARACTER(LEN=24), INTENT(IN)           :: UNAME,DNAME
          CHARACTER(LEN=24), INTENT(IN), OPTIONAL :: CNAME

          TYPE (BRAN),  POINTER    :: BUP, BDP
          TYPE (CONN),  POINTER    :: NCP, PCP
          INTEGER                  :: CNT
          LOGICAL                  :: SAMFG

          BUP => BRAN_POS(UNAME)
          BDP => BRAN_POS(DNAME)

          SAMFG = .FALSE.
          CNT = 0
          NCP => BUP%CDPT
          DO
            IF (.NOT. ASSOCIATED(NCP)) THEN
              ALLOCATE (NCP)
              CCNT = CCNT+ 1
              IF (DBG>3) THEN
                WRITE(99,*)'CONN_ADD:allc',CCNT,POINTER(NCP),' "',UNAME,
     $                     '"  "',DNAME,'"'
              END IF
              IF (PRESENT(CNAME)) THEN
                NCP%NAME = CNAME
              END IF
              BUP%DNCT = BUP%DNCT+ 1
              IF (CNT == 0) THEN
                BUP%CDPT => NCP
                IF (DBG>8) WRITE(99,*)'CONN_ADD:UP str to dn'
              ELSE
                PCP%CNXD => NCP
                IF (DBG>8) WRITE(99,*) 'CONN_ADD:UP num',BUP%DNCT
              END IF
              IF (DBG>6) THEN
                WRITE(99,*)'CONN_ADD:UP frm "',BUP%NAME,'"',BUP%DNCT
              END IF
              NCP%BDPT => BDP
              NCP%BUPT => BUP
              EXIT
            ELSE
              IF (DBG>7) THEN
                WRITE(99,*)'CONN_ADD:Uskp "',NCP%BUPT%NAME,
     $                                '"  "',NCP%BDPT%NAME,'"'
              END IF
              IF (NCP%BUPT%NAME .EQ. UNAME .AND.
     $            NCP%BDPT%NAME .EQ. DNAME) THEN
                IF (DBG>6) THEN
                  WRITE(99,*) 'CONN_ADD:same "',UNAME,'"  "',DNAME,'"'
                END IF
                SAMFG = .TRUE.
                EXIT
              END IF
              PCP => NCP
              NCP => NCP%CNXD
              CNT = CNT+ 1
            END IF
          END DO

          IF (.NOT. SAMFG) THEN
            BDP%UPCT = BDP%UPCT+ 1
            IF (.NOT. ASSOCIATED(BDP%CUPT)) THEN
              BDP%CUPT => NCP
              IF (DBG>8) WRITE(99,*)'CONN_ADD:DN str to up'
            ELSE
              PCP => BDP%CUPT
              DO
                IF (.NOT. ASSOCIATED(PCP%CNXU)) THEN
                  PCP%CNXU => NCP
                  IF (DBG>8) WRITE(99,*) 'CONN_ADD:DN num',BDP%UPCT
                  EXIT
                ELSE
                  PCP => PCP%CNXU
                END IF
              END DO
            END IF
            IF (DBG>6) THEN
              WRITE(99,*)'CONN_ADD:DN frm "',BDP%NAME,'"',BDP%UPCT
            END IF
          END IF

        END SUBROUTINE CONN_ADD

        SUBROUTINE CONN_GET (BNAME,IND,DIR,XNAME,CNAME)
          CHARACTER(LEN=24), INTENT(IN)            :: BNAME
          INTEGER,           INTENT(IN)            :: IND
          CHARACTER(LEN=1),  INTENT(IN)            :: DIR
          CHARACTER(LEN=24), INTENT(OUT)           :: XNAME
          CHARACTER(LEN=24), INTENT(OUT), OPTIONAL :: CNAME

          INTEGER              :: CNT
          TYPE (BRAN), POINTER :: BP
          TYPE (CONN), POINTER :: CP

          BP  => BRAN_POS(BNAME)
          IF (DIR == 'U') THEN
            CP => BP%CUPT
          ELSE
            CP => BP%CDPT
          END IF

          CNT = 0
          DO
            CNT= CNT+ 1
            IF (CNT == IND) THEN
              IF (DIR == 'U') THEN
                XNAME = CP%BUPT%NAME
              ELSE
                XNAME = CP%BDPT%NAME
              END IF
              IF (PRESENT(CNAME)) THEN
                CNAME = CP%NAME
              END IF
              IF (DBG>5) THEN
                WRITE(99,2000) BNAME,XNAME,CP%NAME,IND,POINTER(CP)
2000            FORMAT('CONN_GET:fnd  ',3('"',A24,'" '),2I12)
              END IF
              EXIT
            ELSE IF (DIR == 'U') THEN
              CP => CP%CNXU
            ELSE
              CP => CP%CNXD
            END IF
          END DO

        END SUBROUTINE CONN_GET


        FUNCTION FILT_POS (NAME) RESULT (FP)
          CHARACTER(LEN=*), INTENT(IN) :: NAME
          TYPE (FILT),      POINTER    :: FP

          FP => FILT_FIRST
          DO
            IF (NAME == FP%NAME) THEN
              IF (DBG>7) THEN
                WRITE(99,*)'FILT_POS:fnd  "',NAME,'"',POINTER(FP)
              END IF
              EXIT
            ELSE IF (.NOT. ASSOCIATED(FP%NXT)) THEN
              ALLOCATE (FP%NXT)
              FCNT= FCNT+ 1
              FP => FP%NXT
              IF (DBG>3) THEN
                WRITE(99,*) 'FILT_POS:allc',FCNT,POINTER(FP),
     $                      ' "',NAME,'"'
              END IF
              FP%NAME = NAME
              FP%CNT  = 0
              FP%DOPT = 0
              FP%UFLG = .TRUE.
              EXIT
            ELSE
              FP => FP%NXT
            END IF
          END DO

        END FUNCTION FILT_POS

        SUBROUTINE FILT_ADD (NAME)
          CHARACTER(LEN=*), INTENT(IN)           :: NAME

          TYPE (FILT),       POINTER    :: FP

          FP => FILT_POS(NAME)
          FP%CNT  = FP%CNT + 1

        END SUBROUTINE FILT_ADD

        SUBROUTINE FILT_ADD_PARM (NAME,PNAM,PPOS,PSIZ,PTYP)
          CHARACTER(LEN=*), INTENT(IN)  :: NAME
          CHARACTER(LEN=*), INTENT(IN)  :: PNAM
          INTEGER,          INTENT(IN)  :: PPOS
          INTEGER,          INTENT(IN)  :: PSIZ
          INTEGER,          INTENT(IN)  :: PTYP

          INTEGER                       :: IND
          TYPE (FILT),       POINTER    :: FP

          FP => FILT_POS(NAME)

          IF (FP%PCNT < MXPCNT) THEN
            FP%PCNT = FP%PCNT+ 1
            IND     = FP%PCNT

            FP%PNAM(IND)= PNAM
            FP%PPOS(IND)= PPOS
            FP%PSIZ(IND)= PSIZ
            FP%PTYP(IND)= PTYP
          ELSE
            WRITE(99,*) 'FILT_ADD_PARM:too many parms: ',NAME,PNAM
          END IF

        END SUBROUTINE FILT_ADD_PARM

        SUBROUTINE FILT_MOD (NAME,UFLG,DOPT,DCUR)
          CHARACTER(LEN=*), INTENT(IN)           :: NAME
          LOGICAL,          INTENT(IN), OPTIONAL :: UFLG
          INTEGER,          INTENT(IN), OPTIONAL :: DOPT(MXDOPT)
          INTEGER,          INTENT(IN), OPTIONAL :: DCUR

          TYPE (FILT),       POINTER    :: FP

          FP => FILT_POS(NAME)

          IF (PRESENT(UFLG)) THEN
            FP%UFLG = UFLG
          END IF

          IF (PRESENT(DOPT)) THEN
            FP%DOPT = DOPT
          END IF

          IF (PRESENT(DCUR)) THEN
            FP%DCUR = DCUR
          END IF

        END SUBROUTINE FILT_MOD


        SUBROUTINE FILT_LIS (FLG,
     O                       NAME,UFLG,CNT,DOPT,DCUR)

          INTEGER                       :: FLG
          CHARACTER(LEN=24),INTENT(OUT) :: NAME
          LOGICAL,          INTENT(OUT) :: UFLG
          INTEGER,          INTENT(OUT) :: CNT
          INTEGER,          INTENT(OUT) :: DOPT(MXDOPT)
          INTEGER,          INTENT(OUT) :: DCUR

          TYPE (FILT),SAVE,  POINTER    :: FP

          IF (FLG == -1) THEN
            FP  => FILT_FIRST%NXT
            FLG =  0
          END IF

          IF (ASSOCIATED(FP)) THEN
            NAME =  FP%NAME
            CNT  =  FP%CNT
            DOPT =  FP%DOPT
            DCUR =  FP%DCUR
            UFLG =  FP%UFLG
            FP   => FP%NXT
          ELSE
            NAME = ' '
            FLG  = 1
          END IF

        END SUBROUTINE FILT_LIS


        SUBROUTINE DISP_LIS (FLG,
     O                       NAME,DESC,LEV,POS,
     O                       CNT,CONID,PCONAM,TYPE)

          INTEGER                       :: FLG
          CHARACTER(LEN=24),INTENT(OUT) :: NAME
          CHARACTER(LEN=80),INTENT(OUT) :: DESC
          INTEGER,          INTENT(OUT) :: LEV,POS,CNT,CONID(MXCON)
          CHARACTER(LEN=24),INTENT(OUT) :: PCONAM
          CHARACTER(LEN=16),INTENT(OUT) :: TYPE

          TYPE (DISP), POINTER, SAVE    :: DP

          IF (FLG == 1) THEN
            DP => DISP_FIRST
            FLG = 0
          END IF

          DP => DP%NXT
          IF (ASSOCIATED(DP)) THEN
            NAME  = DP%BP%NAME
            DESC  = DP%BP%DESC
            TYPE  = DP%BP%TYPE
            LEV   = DP%LEV
            POS   = DP%POS
            CNT   = DP%CNT
            CONID = DP%CONID
            PCONAM= DP%PCONAM
          ELSE
            FLG = -1
          END IF
        END SUBROUTINE DISP_LIS

      END MODULE TREE
