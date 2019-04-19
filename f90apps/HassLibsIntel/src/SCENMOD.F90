      MODULE SCENMOD

        USE KERNEL32
        USE HIOOSV, ONLY : GTOSVI, GTOSVR
        
        PUBLIC M_SIMSCN, M_ACTSCN, M_COPSCN, M_REPUCI
        PUBLIC M_DELSCN, M_UCISAV, M_GLOBLK, M_DELUCI
        PUBLIC M_GTNXKW, M_XTABLE, M_XBLOCK, M_PUTUCI, M_XTINFO
        PUBLIC M_WMSGTW, M_WMSGTH, M_XBLOCKEX, M_XTABLEEX
        PUBLIC M_SETDBG, M_SPIPH, UPDATESTATUS, UPDATESTATUSX
        PUBLIC M_HGETI, M_HGETC, M_HPUTC, M_GTINS, M_PBMPAR
        PUBLIC M_DELBMP, M_MSGUNIT, M_NEWDSN, M_NEWFIL
        PUBLIC M_FILSTA, M_SET_DRIVER, SYNC_TIME
        PUBLIC M_GETSTRING
        PRIVATE

        TYPE FILTYP
          CHARACTER*256 NAM
          INTEGER      FUN
        END TYPE FILTYP

        TYPE (FILTYP),  SAVE    :: MSG,UCI,WDM(4),ECH
        TYPE(T_OVERLAPPED),SAVE :: IOVL  
        INTEGER,        SAVE    :: FILES(15)
        INTEGER,        SAVE    :: I0    = 0
        INTEGER(DWORD), SAVE    :: I0DW = 0
        INTEGER,        SAVE    :: I1    = 1
        INTEGER,        SAVE    :: UNIT_FLG
        INTEGER,        SAVE    :: ECOUNT
        INTEGER,        SAVE    :: DBGLEV = 0
        INTEGER(HANDLE),SAVE    :: HPIN = 0
        INTEGER(HANDLE),SAVE    :: HPOUT = 0
        INTEGER,        SAVE    :: MOD_INTEG_FLAG = 0
        REAL,           SAVE    :: MOD_INTEG_TIMESTEP = 0  !days
        CHARACTER*256,  SAVE    :: MOD_INTEG_FILENAME = ""
        LOGICAL,        SAVE    :: DBGPAU = .FALSE.

      CONTAINS

        SUBROUTINE M_SPIPH (HIN,HOUT)
          INTEGER,         INTENT(IN) :: HIN, HOUT
          CHARACTER*20 S

          HPIN = HIN
          HPOUT = HOUT
          WRITE(S,2000) HIN,HOUT
2000      FORMAT ("In M_SPIPH",2I5)
          CALL UPDATESTATUSX(7,20,S)

        END SUBROUTINE M_SPIPH

		SUBROUTINE UPDATESTATUSA (IOPT,A1,I1,A2)
          INTEGER,         INTENT(IN) :: IOPT,I1
          CHARACTER(LEN=*),INTENT(IN) :: A1,A2
		
          INTEGER                     :: ILEN
		  CHARACTER*1024              :: ATXT
		  
2000      FORMAT(1X,A,I3,1X,A)
		  
		  WRITE(ATXT,2000,ERR=10) TRIM(A1),I1,TRIM(A2)
          ILEN = LEN_TRIM(ATXT)
		  
          CALL UPDATESTATUSX(IOPT,ILEN,ATXT)
          IF (DBGLEV > 0) THEN
            WRITE(99,*) ATXT
            IF (DBGPAU) READ(*,*)
          END IF
		  RETURN
 		  
10        CONTINUE
          ! message about internal write problem goes here
 
        END SUBROUTINE UPDATESTATUSA

        SUBROUTINE UPDATESTATUSD (IOPT,ATXT)
          INTEGER,         INTENT(IN) :: IOPT 
          CHARACTER(LEN=*),INTENT(IN) :: ATXT
		
          INTEGER                     :: ILEN
		  
          ILEN = LEN_TRIM(ATXT)
		  
          CALL UPDATESTATUSX(IOPT,ILEN,ATXT)
          IF (DBGLEV > 0) THEN
            WRITE(99,*) ATXT
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE UPDATESTATUSD
		
        SUBROUTINE UPDATESTATUSX (IOPT,ILEN,ATXT)
          INTEGER,         INTENT(IN) :: IOPT,ILEN
          CHARACTER(LEN=*),INTENT(IN) :: ATXT

          INTEGER*1  JTXT(256)
          INTEGER    I, J

          !text to I*1 array
          JTXT= 32
          I   = 1
          DO WHILE (I.LE.ILEN .AND. I.LT.256)
            JTXT(I)= ICHAR(ATXT(I:I))
            I      = I+ 1
          END DO
          JTXT(I)= 0
          J= UPDATESTATUS(IOPT,JTXT)

        END SUBROUTINE UPDATESTATUSX

        INTEGER FUNCTION UPDATESTATUS(I,J)
          INTEGER,       INTENT(IN) :: I
          INTEGER*1  J(*)

          INTEGER    P, K, L, lr, la, lm 

          CHARACTER*255 T,O,X
          character   S(257)
          character   M(200)

          type (T_OVERLAPPED) :: lpOverlapped

          UPDATESTATUS = 0 !assume ok

          !WRITE(*,*) "UPDATESTATUS:" , HPOUT, HPIN, MSG%FUN

          IF (I .GT. 0) THEN
            IF (HPOUT .GT. 0) THEN
              P = 0
              T = ""
              DO
                P = P+ 1
                IF (J(P) .EQ. 0) THEN !got it all 
                  P = LEN_TRIM(T)
                  IF (I .EQ. 5) THEN
                    WRITE(X,'(I2.1)') J(1)
                    P = LEN_TRIM(X)
                    O = "(PROGRESS " // X(1:P) // ")"
                    !WRITE(*,*) "Percent:",X(1:P)," ",O
                  ELSE 
                    IF (I .LT. 10) THEN
                      WRITE(X,'("(MSG",I1)') I
                    ELSE IF (I .EQ. 10) THEN
                      X = "(MSG10"
                    ELSE IF (I .EQ. 99) THEN
                      X = "(MSG99"
                    END IF
                    O = X(1:LEN_TRIM(X))//" "//T(1:P)//")"
                  END IF
                  P = LEN_TRIM(O)
                  IF (DBGLEV>0) THEN
                    WRITE(*,*) P,":",O(1:P)
                    IF (DBGPAU) READ(*,*)
                  END IF
                  IF (I.NE.0) THEN
                    P = P+ 1
                    O(P:P)= CHAR(0)
                    K = 0
                    L = WriteFile(HPOUT, LOC(O), P, LOC(K), lpOverlapped)
                    IF (DBGLEV>0) THEN
                      WRITE(*,*) "Write:",HPOUT,P,L,K
                      IF (DBGPAU) READ(*,*)
                    END IF
                  END IF
                  EXIT 
                ELSE
                  T(P:P) =  ACHAR(J(P))
                END IF
              END DO     
            END IF
          ELSE !just looking for Pause or Cancel
            IF (HPIN .GT. 0) THEN ! check for user messages
              L = PeekNamedPipe(HPIN,0,0,lr,la,lm)
              IF (L .NE. 0 .AND. LA .GT. 0) THEN ! a message
                L = ReadFile(HPIN, LOC(M), la, lr, 0)
                IF (DBGLEV>0) THEN
                  WRITE(*,*) "A message!",HPIN,L,LA,LR,M(1)
                  IF (DBGPAU) READ(*,*)
                END IF
                IF (M(1) .EQ. 'P' ) THEN ! pause
                  LA = 0
                  DO WHILE (LA.EQ.0)
                    L=PeekNamedPipe(HPIN,0,0,lr,la,lm)
                  END DO
                END IF
                IF (M(1) .EQ. 'C') THEN ! cancel
                  UPDATESTATUS = 1  
                END IF
              END IF
            END IF
          END IF

        END FUNCTION UPDATESTATUS

        SUBROUTINE M_SETDBG (LEV)
          INTEGER,         INTENT(IN) :: LEV

          IF (LEV < 0) THEN
            DBGPAU = .TRUE.
          ELSE
            DBGPAU = .FALSE.
          END IF
          DBGLEV = ABS(LEV)
          !WRITE(*,*) 'M_SETDBG:',LEV
          IF (DBGPAU) READ(*,*)

        END SUBROUTINE M_SETDBG

        SUBROUTINE M_ACTSCN (MKFILS,CSCEN,WDMFL,MSGFL,RETCOD)

          INTEGER,         INTENT(IN)  :: MKFILS
          CHARACTER(LEN=*),INTENT(IN)  :: CSCEN
          INTEGER,         INTENT(IN)  :: WDMFL(4),MSGFL
          INTEGER,         INTENT(OUT) :: RETCOD

          INTEGER                      :: IOPT,TMKFIL,L,I,LEN,I7
          INTEGER                      :: LOCLFG,TRET,LUCIU
          LOGICAL                      :: LFLAG,EXISTS
          CHARACTER(LEN=1024)          :: M
          CHARACTER(LEN=1024)          :: S
          CHARACTER(LEN=1024)          :: LSCEN

2000      FORMAT(1X,A,I3,1X,A)
2010      FORMAT(1X,A,I3,1X,L)

          TMKFIL = MKFILS

          I7 = 7   !debug
          WRITE(S,*) 'M_ACTSCN:entry:',CSCEN,WDMFL(1),MSGFL
          CALL UPDATESTATUSD(I7,S)

          ! avoid some lahey math errors
          LFLAG = .TRUE.
          !CALL INVALOP (LFLAG)
          !CALL UNDFL (LFLAG)
          ! CALL OVEFL (LFLAG)    dangerous to avoid these
          ! CALL DVCHK (LFLAG)

          RETCOD = 0
          DO L = 1,4
            IF (WDMFL(L).GT.0) THEN
              WDM(L)%FUN = WDMFL(L)
              INQUIRE(UNIT=WDMFL(L),NAME=WDM(L)%NAM)
              CALL UPDATESTATUSA(I7,'M_ACTSCN:WDMFL:',WDM(L)%FUN,TRIM(WDM(L)%NAM))
            END IF
          END DO

          MSG%FUN    = MSGFL
          INQUIRE(UNIT=MSGFL,NAME=MSG%NAM)
          CALL UPDATESTATUSA(I7,'M_ACTSCN:MSGFL:',MSG%FUN,TRIM(MSG%NAM))
 
          UCI%FUN= 10
          LSCEN = CSCEN
          CALL UPDATESTATUSA(I7,'M_ACTSCN:UCINAM:',UCI%FUN,TRIM(LSCEN))
          UCI%NAM= TRIM(LSCEN) // '.UCI'
          CALL UPDATESTATUSA(I7,'M_ACTSCN:UCIFL:',UCI%FUN,TRIM(UCI%NAM))
          INQUIRE(FILE=UCI%NAM,EXIST=EXISTS)
          WRITE(S,2010) 'M_ACTSCN:UCI EXISTS:',UCI%FUN,EXISTS
          CALL UPDATESTATUSD(I7,S)

          OPEN(UNIT=UCI%FUN,FILE=UCI%NAM,STATUS='OLD',ERR=10)

          WRITE(S,*) 'M_ACTSCN:about to call FILSET'
          CALL UPDATESTATUSD(I7,S)

          IF (WDM(1)%FUN.GT.0) THEN
            !coming from local call, not remote
            LOCLFG = 1
          ELSE
            LOCLFG = 0
          END IF

          FILES= I0
          FILES(15) = MSG%FUN
          FILES(11) = WDM(1)%FUN
          FILES(12) = WDM(2)%FUN
          FILES(13) = WDM(3)%FUN
          FILES(14) = WDM(4)%FUN

          IF (TMKFIL.EQ.-3) THEN
            !special case, dont open files
            LUCIU = -1 * UCI%FUN
          ELSE
            LUCIU = UCI%FUN
          END IF

          I = 0
 20       CONTINUE
            I = I+ 1
            CALL FILSET (MSG%FUN,LUCIU,WDM(1)%FUN,FILES,ECOUNT,RETCOD)
            !keep trying-file from prev process may not have closed
          IF (RETCOD .NE. 0 .AND. I .LT. 1000000) GO TO 20

          WDM(1)%FUN = FILES(11)
          WDM(2)%FUN = FILES(12)
          WDM(3)%FUN = FILES(13)
          WDM(4)%FUN = FILES(14)

          WRITE(S,*) 'M_ACTSCN:back from FILSET',RETCOD
          CALL UPDATESTATUSD(I7,S)

          DO I = 1,4
            IF (WDM(I)%FUN .GT. 0) THEN
              INQUIRE(UNIT=WDM(I)%FUN,NAME=WDM(I)%NAM,ERR=10)
              CALL UPDATESTATUSA(J,'M_ACTSCN:WDMFL:',WDM(I)%FUN,TRIM(WDM(I)%NAM))
            END IF
          END DO        

          ECH%FUN    = FILES(1)
          INQUIRE(UNIT=ECH%FUN,NAME=ECH%NAM,ERR=10) 
          CALL UPDATESTATUSA(I7,'M_ACTSCN:ECHFL:',ECH%FUN,TRIM(ECH%NAM))

          IF (RETCOD .EQ. 0) THEN
            IOPT  = 10
            IF (TMKFIL.LT.-1) THEN
              CALL HDMES3(IOPT,'HSPF')
            ELSE IF (TMKFIL.GT.-1) THEN
              CALL HDMES3(IOPT,'GenScnActivate HSPF')
            END IF

            IF (TMKFIL.NE.-1) THEN
              IF (TMKFIL.LT.0) THEN
                TMKFIL = 0
              END IF
              WRITE(S,*) 'M_ACTSCN:abt 2call INTERP',I0,FILES
              CALL UPDATESTATUSD(I7,S)
 
              M= "before calling INTERP"
              CALL M_FILSTA (M)

              CALL INTERP (I0,TMKFIL,FILES,UNIT_FLG,RETCOD)

              WRITE(S,*) 'M_ACTSCN:back from INTERP',RETCOD
              CALL UPDATESTATUSD(I7,S)
            ELSE
              WRITE(S,*) 'M_ACTSCN:skip INTERP'
              CALL UPDATESTATUSD(I7,S)
            END IF
  
            M= "before File Closing in M_ACTSCN"
            CALL M_FILSTA (M)

            IOPT  = 99
            CALL HDMES3(IOPT,' ')   ! hide status

          END IF

          CLOSE (UNIT=UCI%FUN)   ! close users input file
          CALL FILCLO            ! close all files except wdm files
          CALL FILTSF            ! close and reopen any feq tsf files

          M= "before WDM closing in M_ACTSCN"
          CALL M_FILSTA (M)

          IF (LOCLFG.EQ.0) THEN
            !coming from remote call
            DO 30 I = 1,4
              IF (WDM(I)%FUN .GT. 0) THEN
                !CLOSE(WDM(I)%FUN)
                WRITE(M,*) 'about to close ',WDM(I)%FUN
                CALL M_FILSTA (M)
                CALL WDFLCL(WDM(I)%FUN,TRET)
                WDM(I)%FUN = 0
                WDM(I)%NAM = ""
                M= "after one WDM closing in M_ACTSCN"
                CALL M_FILSTA (M)
              END IF
 30         CONTINUE
          END IF

          M= "at end of File Closing in M_ACTSCN"
          CALL M_FILSTA (M)

          I = 7   !debug
          WRITE(S,*) 'M_ACTSCN:exit ',RETCOD
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV > 0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          RETURN

10        CONTINUE

            I = 7   !debug
            WRITE(S,*) 'M_ACTSCN:got to ERR 10'
            L = LEN_TRIM(S)
            CALL UPDATESTATUSX(I,L,S)
            IF (DBGLEV>0) THEN
              WRITE(99,*) S
              IF (DBGPAU) READ(*,*)
            END IF

            RETCOD = -1
            RETURN

        END SUBROUTINE M_ACTSCN

        SUBROUTINE   M_SIMSCN (RETCOD)

          INTEGER, INTENT(OUT)  :: RETCOD

          INTEGER       L,I,I7,LOCLFG,TRET
          CHARACTER*240 S
          CHARACTER(LEN=80)            :: M
          LOGICAL       OPEN

2000      FORMAT(1X,A,I3,1X,A)

          I7 = 7   !debug
          WRITE(S,*) 'M_SIMSCN:entry:',TRIM(ECH%NAM)
          CALL UPDATESTATUSD(I,S)

          M= "before FILSET in M_SIMSCN"
          CALL M_FILSTA (M)

          WRITE(S,*) 'M_SIMSCN:about to call FILSET',MSG%FUN,UCI%FUN,WDM(1)%FUN,WDM(2)%FUN,WDM(3)%FUN,WDM(4)%FUN
          CALL UPDATESTATUSD(I,S)

          OPEN(UNIT=UCI%FUN,FILE=UCI%NAM,STATUS='OLD',ERR=10)

          IF (WDM(1)%FUN.GT.0) THEN
            !coming from local call, not remote
            LOCLFG = 1
          ELSE
            LOCLFG = 0
          END IF

          FILES= 0
          FILES(15) = MSG%FUN
          FILES(11) = WDM(1)%FUN
          FILES(12) = WDM(2)%FUN
          FILES(13) = WDM(3)%FUN
          FILES(14) = WDM(4)%FUN

          I = 0
 20       CONTINUE
            I= I+ 1
            CALL FILSET (MSG%FUN,UCI%FUN,WDM(1)%FUN,FILES,ECOUNT,RETCOD)
            !keep trying-file from prev process may not have closed
          IF (I .LT. 1000000 .AND. RETCOD.NE.0) GO TO 20

          WDM(1)%FUN = FILES(11)
          WDM(2)%FUN = FILES(12)
          WDM(3)%FUN = FILES(13)
          WDM(4)%FUN = FILES(14)

          M= "at end of FilSet in M_SIMSCN"
          CALL M_FILSTA (M)

          WRITE(S,*) 'M_SIMSCN:about to call HSPF',FILES
          CALL UPDATESTATUSD(I,S)

          ! proceed to run model
          CALL HSPF (FILES,RETCOD)
          ! simulation complete

          M= "before WDM closing in M_SIMSCN"
          CALL M_FILSTA (M)

          IF (LOCLFG.EQ.0) THEN  !calling from remote
            DO 30 I = 1,4
              IF (WDM(I)%FUN .GT. 0) THEN
                WRITE(M,*) 'about to close ',WDM(I)%FUN
                CALL M_FILSTA (M)

                CALL WDFLCL(WDM(I)%FUN,TRET)
                IF (TRET.NE.0) THEN
                  WRITE(S,*) 'M_SIMSCN:CLOSED WDM ',WDM(I)%FUN,TRET
                  L = LEN_TRIM(S)
                  CALL UPDATESTATUSX(I,L,S)
                END IF

                WDM(I)%FUN = 0
                WDM(I)%NAM = ""
                M= "after one WDM closing in M_SIMSCN"
                CALL M_FILSTA (M)
              END IF
 30         CONTINUE
          END IF
          CLOSE (UNIT=UCI%FUN)   ! close users input file

          WRITE(S,*) 'M_SIMSCN:back from HSPF with',RETCOD
          CALL UPDATESTATUSD(I,S)

          M= "at end of File Closing in M_SIMSCN"
          CALL M_FILSTA (M)

          RETURN

10        CONTINUE

            WRITE(S,*) 'M_ACTSCN:got to ERR 10'
            CALL UPDATESTATUSD(I,S)

            RETCOD = -1
            RETURN

        END SUBROUTINE M_SIMSCN

        SUBROUTINE M_GLOBLK (SDATE,EDATE,OUTLEV,SPOUT,RUNFG,EMFG,IHMFG,INFO)

          INTEGER, INTENT(OUT)    :: SDATE(5),EDATE(5),OUTLEV
          INTEGER, INTENT(OUT)    :: SPOUT,RUNFG,EMFG,IHMFG,INFO(80)

          INTEGER                 :: SEDAT(10),RUNMIN,RESMFG,L
          INTEGER                 :: SDATIM(5),EDATIM(5)
          CHARACTER*80            :: RNINFO

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_GLOBLK:entry',MSG%FUN,ECOUNT,I0
            IF (DBGPAU) READ(*,*)
          END IF
          CALL GLOBLK (I0,MSG%FUN,I0,ECOUNT,SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,SPOUT,RESMFG,RUNFG,EMFG,RNINFO,IHMFG)
          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_GLOBLK:back ',ECOUNT,OUTLEV,RUNMIN
            IF (DBGPAU) READ(*,*)
          END IF
          DO L = 1,5
            SDATE(L) = SEDAT(L)
            EDATE(L) = SEDAT(L+5)
          END DO
          DO L = 1,80
            INFO(L) = ICHAR(RNINFO(L:L))
          END DO

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_GLOBLK:exit'
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_GLOBLK

        !newaqt:scnutl
        SUBROUTINE M_GTNXKW (INIT,ID,IKWD,KWDFG,CONTFG,RETID)
          INTEGER, INTENT(IN)  :: INIT,ID
          INTEGER, INTENT(OUT) :: IKWD(12),KWDFG,CONTFG,RETID

          INTEGER              :: L
          CHARACTER*12         :: CKWD

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_GTNXKW:entry',INIT,ID
            IF (DBGPAU) READ(*,*)
          END IF

          IF (MSG%FUN > 0) THEN
            CALL GTNXKW (MSG%FUN,INIT,ID,CKWD,KWDFG,CONTFG,RETID)
          ELSE
            WRITE(*,*) 'SCENMOD:M_GTNXKW:no message file open'
            CKWD  = ''
            KWDFG = 0
            CONTFG= 0
            RETID = 0
          END IF
          DO L = 1, 12
            IKWD(L) = ICHAR(CKWD(L:L))
          END DO

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_GTNXKW:exit ',KWDFG,CONTFG,RETID
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_GTNXKW

        !newaqt:scnutl
        SUBROUTINE M_XBLOCK (BLKNO,INIT,RETKEY,IBUFF,RETCOD)
          INTEGER, INTENT(IN)  :: BLKNO,INIT
          INTEGER, INTENT(OUT) :: IBUFF(80),RETCOD,RETKEY

          INTEGER              :: L
          CHARACTER*80         :: CBUFF

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XBLOCK:entry',BLKNO,INIT
            IF (DBGPAU) READ(*,*)
          END IF

          CALL XBLOCK (BLKNO,INIT,RETKEY,CBUFF,RETCOD)
          DO L = 1, 80
            IBUFF(L) = ICHAR(CBUFF(L:L))
          END DO
          !WRITE(*,*) 'SCENMOD:M_XBLOCK: ',RETCOD,CBUFF

        END SUBROUTINE M_XBLOCK

        !newaqt:scnutl
        SUBROUTINE M_XBLOCKEX (BLKNO,INIT,RETKEY,IBUFF,RECTYP,RETCOD)
          INTEGER, INTENT(IN)    :: BLKNO,INIT
          INTEGER, INTENT(INOUT) :: RETKEY
          INTEGER, INTENT(OUT)   :: IBUFF(80),RETCOD,RECTYP

          INTEGER              :: L
          CHARACTER*80         :: CBUFF

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XBLOCKEX:entry',BLKNO,INIT
            IF (DBGPAU) READ(*,*)
          END IF

          CALL XBLOCKEX (BLKNO,INIT,RETKEY,CBUFF,RECTYP,RETCOD)
          DO L = 1, 80
            IBUFF(L) = ICHAR(CBUFF(L:L))
          END DO
          !WRITE(*,*) 'SCENMOD:M_XBLOCKEX: ',RETCOD,CBUFF

        END SUBROUTINE M_XBLOCKEX

        !newaqt:scnutl
        SUBROUTINE M_XTABLE (OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,RETKEY,IBUFF,RETCOD)

          INTEGER, INTENT(IN)  :: OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR
          INTEGER, INTENT(OUT) :: RETKEY,IBUFF(80),RETCOD

          INTEGER              :: L
          CHARACTER*80         :: CBUFF

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XTABLE:entry',OMCODE,TABNO,UUNITS,INIT
            IF (DBGPAU) READ(*,*)
          END IF

          IF (MSG%FUN > 0) THEN
            CALL XTABLE (MSG%FUN,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,RETKEY,CBUFF,RETCOD)
          ELSE
            WRITE(*,*) 'SCENMOD:M_XTABLE:no message file open'
            CBUFF = ''
            RETKEY= 0
            RETCOD= -999
          END IF
          DO L = 1, 80
            IBUFF(L) = ICHAR(CBUFF(L:L))
          END DO

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XTABLE:exit ',RETCOD,TRIM(CBUFF)
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_XTABLE

        !newaqt:scnutl
        SUBROUTINE M_XTABLEEX (OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,RETKEY,IBUFF,RECTYP,RETCOD)

          INTEGER, INTENT(IN)    :: OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR
          INTEGER, INTENT(INOUT) :: RETKEY
          INTEGER, INTENT(OUT)   :: RECTYP,IBUFF(80),RETCOD

          INTEGER              :: L
          CHARACTER*80         :: CBUFF

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XTABLEEX:entry',OMCODE,TABNO,UUNITS,INIT
            IF (DBGPAU) READ(*,*)
          END IF

          IF (MSG%FUN > 0) THEN
            CALL XTABLEEX (MSG%FUN,OMCODE,TABNO,UUNITS,INIT,ADDFG,OCCUR,RETKEY,CBUFF,RECTYP,RETCOD)
          ELSE
            WRITE(*,*) 'SCENMOD:M_XTABLE:no message file open'
            CBUFF = ''
            RETKEY= 0
            RECTYP= 0
            RETCOD= -999
          END IF
          DO L = 1, 80
            IBUFF(L) = ICHAR(CBUFF(L:L))
          END DO

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_XTABLEEX:exit ',RETCOD,TRIM(CBUFF)
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_XTABLEEX

        !newaqt:scnutl
        SUBROUTINE M_XTINFO (OMCODE,TNUM,UUNITS,CHKEST,LNFLDS,LSCOL,LFLEN,ILFTYP,LAPOS,LIMIN,LIMAX,LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,IHDRBUF,IFDNAM,ISECT,IREPT,RETCOD)

        INTEGER,INTENT(IN)  :: OMCODE,TNUM,UUNITS,CHKEST
        INTEGER,INTENT(OUT) :: LNFLDS,LSCOL(30),LFLEN(30),LAPOS(30),LIMIN(30),LIMAX(30),LIDEF(30),LNMHDR,RETCOD,ISECT,IREPT
        REAL,   INTENT(OUT) :: LRMIN(30),LRMAX(30),LRDEF(30)
        INTEGER,INTENT(OUT) :: ILFTYP(30),IHDRBUF(78,10),IFDNAM(12,30)

        CHARACTER(LEN=1)    :: LFTYP(30),HDRBUF(78,10),LPNAME(12,30)
        CHARACTER(LEN=8)    :: LTMP
        INTEGER             :: I,J,SCLU,SGRP

        IF (DBGLEV > 0) THEN
          WRITE(*,*) 'M_XTINFO:entry',OMCODE,TNUM,UUNITS
          IF (DBGPAU) READ(*,*)
        END IF

        CALL XCLUGP (MSG%FUN,OMCODE,TNUM,UUNITS,CHKEST,SCLU,SGRP,ISECT,IREPT,RETCOD)
        IF (DBGLEV > 0) THEN
          WRITE(*,*) 'M_XTINFO:after XCLUGP',RETCOD,SCLU,SGRP
          IF (DBGPAU) READ(*,*)
        END IF
        IF (SGRP.EQ.0 .AND. RETCOD.EQ.0) THEN
          RETCOD = 2
        END IF
        IF (RETCOD.EQ.0) THEN
          CALL WMSGTX (MSG%FUN,SCLU,SGRP,LNFLDS,LSCOL,LFLEN,LFTYP,LAPOS,LIMIN,LIMAX,LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,HDRBUF,RETCOD)
          LPNAME = ''
          CALL WMSGPN (MSG%FUN,SCLU,SGRP,LPNAME)
          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_XTINFO:after WMSGTX',RETCOD,LNFLDS,LNMHDR
            IF (DBGPAU) READ(*,*)
          END IF
        END IF

        DO I = 1,30
          ILFTYP(I)= ICHAR(LFTYP(I))
          DO J = 1,12
            IFDNAM(J,I) = ICHAR(LPNAME(J,I))
          END DO
        END DO
        DO I = 1,10
          DO J = 1,78
            IHDRBUF(J,I) = ICHAR(HDRBUF(J,I))
          END DO
        END DO

        IF (DBGLEV > 0) THEN
          WRITE(*,*)     'M_XTINFO:exit ',RETCOD
          IF (DBGLEV > 1) THEN
            WRITE(*,*)   '         NFLDS',LNFLDS
            DO I = 1,LNFLDS
              WRITE(LTMP,'(8A1)') (LPNAME(J,I),J=1,12)
              WRITE(*,*) '      ',LTMP,' ',LFTYP(I),LSCOL(I),LFLEN(I)
            END DO
            IF (DBGLEV > 2) THEN
              DO I = LNFLDS+1,30
                WRITE(LTMP,'(8A1)') (LPNAME(J,I),J=1,12)
                WRITE(*,*) '      ',LTMP,' ',LFTYP(I),LSCOL(I),LFLEN(I)
              END DO
            END IF
          END IF
          IF (DBGPAU) READ(*,*)
        END IF

      END SUBROUTINE M_XTINFO

      !adwdm:ztwdmf:wmsgtw
      SUBROUTINE M_WMSGTW (ID,IWINAM)
        INTEGER,INTENT(IN)  :: ID
        INTEGER,INTENT(OUT) :: IWINAM(*)

        CHARACTER(LEN=48)   :: WINAM
          
        CALL WMSGTW (ID,WINAM)
        DO I = 1,48
          IWINAM(I) = ICHAR(WINAM(I:I))
        END DO
      END SUBROUTINE M_WMSGTW

      !adwdm:ztwdmf:wmsgth
      SUBROUTINE M_WMSGTH (GHELP,FHELP)
        INTEGER,INTENT(OUT) :: GHELP,FHELP(64)
          
        CALL WMSGTH (GHELP,FHELP)
      END SUBROUTINE M_WMSGTH

        !newaqt:scnutl
        SUBROUTINE M_DELSCN (CSCEN)
          CHARACTER(LEN=*) :: CSCEN
          CHARACTER*8      :: LSCEN

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_DELSCN:entry:',TRIM(CSCEN)
            IF (DBGPAU) READ(*,*)
          END IF

          LSCEN = CSCEN
          CALL DELSCN(MSG%FUN,LSCEN)

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_DELSCN:exit'
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_DELSCN

        !newaqt:scnutl
        SUBROUTINE M_COPSCN (DSNID, RELABS, CASCN, CNSCN)
          INTEGER,          INTENT(IN) :: DSNID,RELABS
          CHARACTER(LEN=*), INTENT(IN) :: CASCN
          CHARACTER(LEN=*), INTENT(IN) :: CNSCN

          CHARACTER(LEN=8) :: LASCN
          CHARACTER(LEN=8) :: LNSCN

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_COPSCN:entry:',TRIM(CASCN)
            WRITE(*,*) '               ',TRIM(CNSCN),DSNID,RELABS
            IF (DBGPAU) READ(*,*)
          END IF

          LASCN = CASCN
          LNSCN = CNSCN

          CALL COPSCN(LASCN,LNSCN,DSNID,RELABS)

          UCI%NAM= CNSCN // '.UCI'

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_COPSCN:exit'
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_COPSCN

        !newaqt:scnutl
        SUBROUTINE M_NEWFIL (CASCN, CNSCN)
          CHARACTER(LEN=*), INTENT(IN) :: CASCN
          CHARACTER(LEN=*), INTENT(IN) :: CNSCN

          CHARACTER(LEN=8) :: LASCN
          CHARACTER(LEN=8) :: LNSCN

          LASCN = CASCN
          LNSCN = CNSCN

          CALL NEWFIL(LASCN,LNSCN)

          UCI%NAM= CNSCN // '.UCI'

        END SUBROUTINE M_NEWFIL

        !newaqt:scnutl
        SUBROUTINE M_NEWDSN (WDMID, ODSN, NDSN)
          INTEGER,          INTENT(IN) :: WDMID, ODSN, NDSN

          CALL NEWDSN (3,WDMID,ODSN,NDSN)

        END SUBROUTINE M_NEWDSN

        !newaqt:scnutl
        SUBROUTINE M_UCISAV

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_UCISAV:entry:',TRIM(UCI%NAM),UCI%FUN
            IF (DBGPAU) READ(*,*)
          END IF

          OPEN(UNIT=UCI%FUN,FILE=UCI%NAM)    ! open uci file

          CALL UCISAV(UCI%FUN)

          CLOSE (UNIT=UCI%FUN)      ! close uci file

          CALL TSDSCL(MSG%FUN,1,1)

          IF (DBGLEV > 0) THEN
            WRITE(*,*) 'M_UCISAV:exit'
            IF (DBGPAU) READ(*,*)
          END IF

        END SUBROUTINE M_UCISAV

        !hspf:hiouci
        SUBROUTINE M_REPUCI (KEY, CBUFF)
          INTEGER,          INTENT(IN) :: KEY
          CHARACTER(LEN=*), INTENT(IN) :: CBUFF

          CHARACTER(LEN=80) :: CBUFF80

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_REPUCI:entry',KEY,TRIM(CBUFF)
            IF (DBGPAU) READ(*,*)
          END IF

          CBUFF80 = CBUFF
          CALL REPUCI(KEY,CBUFF80)

        END SUBROUTINE M_REPUCI

        !hspf:hiouci
        SUBROUTINE M_DELUCI (KEY)
          INTEGER,          INTENT(IN) :: KEY

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_DELUCI:entry',KEY
            IF (DBGPAU) READ(*,*)
          END IF

          CALL DELUCI(KEY)

        END SUBROUTINE M_DELUCI

        !hspf:hiouci
        SUBROUTINE M_PUTUCI (KEY, TYPE, CBUFF)
          INTEGER,          INTENT(IN) :: KEY,TYPE
          CHARACTER(LEN=*), INTENT(IN) :: CBUFF

          CHARACTER(LEN=80) :: CBUFF80

          IF (DBGLEV > 1) THEN
            WRITE(*,*) 'M_PUTUCI:entry',KEY,TYPE,TRIM(CBUFF)
            IF (DBGPAU) READ(*,*)
          END IF

          CBUFF80 = CBUFF

          CALL PUTUCI(CBUFF80,TYPE,KEY)

        END SUBROUTINE M_PUTUCI

        !newaqt:hget
        SUBROUTINE M_HGETI (ITMNAM,IDNO,IVAL)

          CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM
          INTEGER,         INTENT(IN)  :: IDNO
          INTEGER,         INTENT(OUT) :: IVAL

          IF (DBGLEV>0) THEN
            WRITE(*,*) 'M_HGETI',ITMNAM,IDNO
          END IF

          CALL HGETI(ITMNAM,IDNO,IVAL)

        END SUBROUTINE M_HGETI

        !newaqt:hget
        SUBROUTINE M_HPUTC (ITMNAM,IDNO,CTXT)

          CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM,CTXT
          INTEGER,         INTENT(IN)  :: IDNO

          CALL HPUTC(ITMNAM,IDNO,CTXT)

        END SUBROUTINE M_HPUTC

        !newaqt:hget
        SUBROUTINE M_HGETC (ITMNAM,IDNO,ICTXT)

          CHARACTER(LEN=*),INTENT(IN)  :: ITMNAM
          INTEGER,         INTENT(IN)  :: IDNO
          INTEGER,         INTENT(OUT) :: ICTXT(*)

          INTEGER              :: L
          CHARACTER*80         :: CTXT

          IF (DBGLEV>0) THEN
            WRITE(*,*) 'M_HGETC',ITMNAM,IDNO
          END IF

          CALL HGETC(ITMNAM,IDNO,CTXT)

          DO L = 1, 80
            ICTXT(L) = ICHAR(CTXT(L:L))
          END DO

        END SUBROUTINE M_HGETC

        !newaqt:ucibat
        SUBROUTINE M_GTINS (INIT,IDNO,RORB,ICTXT,RAREA)

          INTEGER,         INTENT(IN)  :: INIT,IDNO,RORB
          INTEGER,         INTENT(OUT) :: ICTXT(*)
          REAL,            INTENT(OUT) :: RAREA

          INTEGER              :: L
          CHARACTER*10         :: CTXT

          IF (DBGLEV>0) THEN
            WRITE(*,*) 'M_GTINS',IDNO,RORB
          END IF

          CALL GTINS(INIT,IDNO,RORB,CTXT,RAREA)

          DO L = 1, 10
            ICTXT(L) = ICHAR(CTXT(L:L))
          END DO

        END SUBROUTINE M_GTINS

        !newaqt:ucibat
        SUBROUTINE M_PBMPAR (CIN,INID,RAREA,COUT,OUTID,RETCOD)

          INTEGER,         INTENT(IN)  :: INID,OUTID
          CHARACTER(LEN=*),INTENT(IN)  :: CIN,COUT
          REAL,            INTENT(IN)  :: RAREA
          INTEGER,         INTENT(OUT) :: RETCOD

          IF (DBGLEV>0) THEN
            WRITE(*,*) 'M_PBMPAR',CIN,INID,RAREA,COUT,OUTID
          END IF

          CALL PBMPAR (CIN,INID,RAREA,COUT,OUTID,RETCOD)

        END SUBROUTINE M_PBMPAR

        !newaqt:ucibat
        SUBROUTINE M_DELBMP (BMPID)

          INTEGER,         INTENT(IN)  :: BMPID

          IF (DBGLEV>0) THEN
            WRITE(*,*) 'M_DELBMP',BMPID
          END IF

          CALL DELBMP (BMPID)

        END SUBROUTINE M_DELBMP

        SUBROUTINE M_MSGUNIT (MSGFL)

          INTEGER,         INTENT(IN)  :: MSGFL

          MSG%FUN = MSGFL

        END SUBROUTINE M_MSGUNIT

        SUBROUTINE M_FILSTA (MSG)

          CHARACTER(LEN=*),INTENT(IN)  :: MSG
  
          INTEGER                      :: I,J,K,L
          CHARACTER(LEN=128)           :: F,S,A,M,W
          CHARACTER(LEN=8)             :: C,D
          LOGICAL                      :: O 

          !write(*,*) 'FILSTA:' // TRIM(MSG)

          J = 7
          S = 'File Status at ' // TRIM(MSG)
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(J,L,S)

          DO 10 I = 1, 200
            IF (I.NE.5 .AND. I.NE.6) THEN
              INQUIRE(I,OPENED=O)
            ELSE !avoid opening nasty dos box!
              O = .FALSE.
            END IF
            IF (O) THEN
              INQUIRE(I,NAME=F,ACCESS=A,FORM=M,RECL=K,READWRITE=W)
              WRITE(C,'(I3)') I
              WRITE(D,'(I5)') K
              S = '  unit ' // TRIM(C) // ' file ' // TRIM(F) // ' is open, access:' // TRIM(A) // ' form:' // TRIM(M) // ' r/w:' // TRIM(W) // ' recl:' // TRIM(D)
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(J,L,S)
            END IF 
 10       CONTINUE

        END SUBROUTINE M_FILSTA

        SUBROUTINE M_SET_DRIVER (INTEG_FLAG,INTEG_TIMESTEP, INTEG_FILENAME)

          INTEGER,         INTENT(IN)    :: INTEG_FLAG
          REAL,            INTENT(IN)    :: INTEG_TIMESTEP
          CHARACTER*256,   INTENT(IN)    :: INTEG_FILENAME

          MOD_INTEG_FLAG = INTEG_FLAG
          MOD_INTEG_TIMESTEP = INTEG_TIMESTEP
          MOD_INTEG_FILENAME = INTEG_FILENAME
          !write(99,*) 'in m_set_driver ', INTEG_FLAG,INTEG_timestep

        END SUBROUTINE M_SET_DRIVER

        SUBROUTINE SYNC_TIME (DELT,OPST,OPND,OPNTAB,EXUPFG,EXTWID)

          INTEGER,         INTENT(IN)  :: DELT,OPST,OPND
          INTEGER,         INTENT(IN)  :: OPNTAB(20,OPND)
          INTEGER,         INTENT(OUT) :: EXUPFG,EXTWID

          INTEGER                      :: BINADD,BINU,LFIL,CLEN1,CLEN2
          INTEGER                      :: VARADD(4),RECORD,OFFSET,CLEN3
          INTEGER                      :: VLEN(4),IV,NVLIB,MXSUB(4)
          INTEGER                      :: ADDR,VARTYP(4),IVAL(4,1)
          REAL                         :: RVAL(4,12)
          CHARACTER(LEN=128)           :: BINNAM,CSTR
          CHARACTER(LEN=256)           :: VSTR(4)
          CHARACTER(LEN=512)           :: UPDSTR
          CHARACTER(LEN=6)             :: VARNAM(4)

          DATA NVLIB/4/
          DATA VARNAM/'LZS   ','VLEFG ','LZETP ','LZETPM'/
          DATA VARADD/ 864,     366,     881,     626    /
          DATA VARTYP/ 3,       2,       3,       3      /
          DATA MXSUB / 1,       1,       1,       12     /

          EXUPFG = MOD_INTEG_FLAG
          EXTWID = MOD_INTEG_TIMESTEP*1440.0/DELT
          !write(99,*) 'in sync_time ', delt,exupfg,extwid

          LFIL = 11
          OPEN (LFIL, FILE=MOD_INTEG_FILENAME, ERR=999)

          !look through all operations for simulation modules
          DO 10 I= OPST, OPND
            IF (OPNTAB(4,I) .LE. 3) THEN
              !found a perlnd, implnd, or rchres

              !we can assume BINU fits within first record
              !second arg is address of binu(1)
              IF (OPNTAB(4,I) .LE. 2) THEN
                !perlnd or implnd
                BINADD = 49
              ELSE
                !rchres
                BINADD = 48
              END IF
              CALL GTOSVI (OPNTAB(7,I),BINADD,BINU)
              INQUIRE (BINU,NAME=BINNAM,ERR=8)

              !report this file name to the driver
              WRITE (CSTR,"(A4,A2,1X,I4,1X)") OPNTAB(1,I),OPNTAB(2,I),OPNTAB(3,I)
              CLEN2 = LEN_TRIM(BINNAM)
              CLEN1 = LEN_TRIM(CSTR)
              IF (OPNTAB(4,I) .EQ. 1) THEN
                !perlnd
                DO 5 IV= 1, NVLIB
                  DO 3 IX= 1, MXSUB(IV)
                    ADDR= VARADD(IV)+ IX- 1
                    RECORD = INT (ADDR / 500)
                    OFFSET = ADDR- 500*RECORD 
                    RECORD = RECORD+ OPNTAB(7,I)
                    IF (VARTYP(IV) .EQ. 2) THEN
                      CALL GTOSVI (RECORD,OFFSET,IVAL(IV,IX))
                    ELSE IF (VARTYP(IV) .EQ. 3) THEN
                      CALL GTOSVR (RECORD,OFFSET,RVAL(IV,IX))
                    END IF
 3                CONTINUE
                  IF (VARTYP(IV) .EQ. 2) THEN
                    WRITE (VSTR(IV),"(A6,1X,(20I10))") VARNAM(IV),(IVAL(IV,IX),IX=1,MXSUB(IV))
                  ELSE IF (VARTYP(IV) .EQ. 3) THEN
                    WRITE (VSTR(IV),"(A6,1X,20(1PE12.4,1X))") VARNAM(IV),(RVAL(IV,IX),IX=1,MXSUB(IV))
                  END IF
                  VLEN(IV)= LEN_TRIM (VSTR(IV))+ 1
                  VSTR(IV)(VLEN(IV):VLEN(IV))= ' '
 5              CONTINUE
                WRITE (UPDSTR,*) '(HSPF BINOUT_ID ',(CSTR(J:J),J=1,CLEN1),' ',(BINNAM(J:J),J=1,CLEN2),' '
                CLEN3= LEN_TRIM (UPDSTR)+ 1
                DO 7 IV = 1, NVLIB
                  DO 6 J = 1, VLEN(IV)
                    CLEN3= CLEN3+ 1
                    UPDSTR(CLEN3:CLEN3)= VSTR(IV)(J:J)
 6                CONTINUE
 7              CONTINUE
                CLEN3= CLEN3+ 1
                UPDSTR(CLEN3:CLEN3)= ')'
                WRITE (LFIL,"(A512)") UPDSTR
              ELSE
                WRITE (LFIL,*) '(HSPF BINOUT_ID ',(CSTR(J:J),J=1,CLEN1),' ',(BINNAM(J:J),J=1,CLEN2),')'
              END IF

              GOTO 9
  8           CONTINUE
                WRITE (99,*) 'COULD NOT FIND FILE FOR BINU ',BINU
  9           CONTINUE

            END IF
 10       CONTINUE

          CLOSE(LFIL)

 999      CONTINUE

        END SUBROUTINE SYNC_TIME

        SUBROUTINE M_GETSTRING (S)

          !INTEGER    PeekNamedPipe, ReadFile

          CHARACTER(LEN=*),INTENT(OUT)  :: S
          INTEGER(LPDWORD)  L, lr, la, lm, I
          INTEGER(LPVOID) SPTR
          INTEGER(DWORD)  LX

          S = ''
          DO WHILE (LEN_TRIM(S) .EQ. 0)
            !L = PeekNamedPipe(Carg(HPIN),Carg(0),carg(0),lr,la,lm)
            L = PeekNamedPipe(HPIN,NULL,NULL,lr,la,lm)
            IF (L .NE. 0 .AND. LA .GT. 0) THEN ! a message
              !L = ReadFile(CARG(HPIN), M, CARG(la), lr, Carg(0))
              SPTR = LOC(S)
              L = ReadFile(HPIN, SPTR, lX, LR, IOVL)
                !WRITE(*,*) 'm_getstring',LR,HPIN
              !WRITE(99,*) 'm_getstring',LR,HPIN
              !WRITE(*,*) 'm_getstring',trim(s)
            ELSE
              call sleep(10)
            END IF
          END DO
          S = ADJUSTL(S)
          S = TRIM(S)

        END SUBROUTINE M_GETSTRING

      END MODULE SCENMOD
