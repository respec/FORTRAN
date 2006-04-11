      MODULE SCENMOD

        PUBLIC M_SIMSCN, M_ACTSCN
        PUBLIC M_SPIPH, UPDATESTATUS, UPDATESTATUSX
        PUBLIC M_FILSTA, SYNC_TIME
        PUBLIC M_GETSTRING

        PRIVATE

        TYPE FILTYP
          CHARACTER*64 NAM
          INTEGER      FUN
        END TYPE FILTYP

        TYPE (FILTYP),SAVE    :: MSG,UCI,WDM(4),ECH
        INTEGER,      SAVE    :: FILES(15)
        INTEGER,      SAVE    :: I0    = 0
        INTEGER,      SAVE    :: I1    = 1
        INTEGER,      SAVE    :: UNIT_FLG
        INTEGER,      SAVE    :: ECOUNT
        INTEGER,      SAVE    :: DBGLEV = 0
        INTEGER,      SAVE    :: HPIN = 0
        INTEGER,      SAVE    :: HPOUT = 0
        INTEGER,      SAVE    :: MOD_INTEG_FLAG = 0
        REAL,         SAVE    :: MOD_INTEG_TIMESTEP = 0  !days
        CHARACTER*256,SAVE    :: MOD_INTEG_FILENAME = ""
        LOGICAL,      SAVE    :: DBGPAU = .FALSE.

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

          INTEGER    WriteFile, PeekNamedPipe, ReadFile
          DLL_IMPORT WriteFile, PeekNamedPipe, ReadFile

          INTEGER,       INTENT(IN) :: I
          INTEGER*1  J(*)

          INTEGER    K, L, lr, la, lm

          CHARACTER*255 T,O,X
          INTEGER*2  P, S(257)
          INTEGER*1  M(200)

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
                    DO K = 1,P
                      S(K) = ICHAR(O(K:K))
                    END DO
                    P = P+ 1
                    S(P) = 0
                    P = P* 2
                    L=WriteFile(Carg(HPOUT),S,Carg(P),K,Carg(0))
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
              L = PeekNamedPipe(Carg(HPIN),Carg(0),carg(0),lr,la,lm)
              IF (L .NE. 0 .AND. LA .GT. 0) THEN ! a message
                L = ReadFile(CARG(HPIN), M, CARG(la), lr, Carg(0))
                IF (DBGLEV>0) THEN
                  WRITE(*,*) "A message!",HPIN,L,LA,LR,M(1)
                  IF (DBGPAU) READ(*,*)
                END IF
                IF (M(1) .EQ. ICHAR('P')) THEN ! pause
                  LA = 0
                  DO WHILE (LA.EQ.0)
                    L=PeekNamedPipe(Carg(HPIN),Carg(0),carg(0),lr,la,lm)
                  END DO
                END IF
                IF (M(1) .EQ. ICHAR('C')) THEN ! cancel
                  UPDATESTATUS = 1  
                END IF
              END IF
            END IF
          END IF

        END FUNCTION UPDATESTATUS

        SUBROUTINE M_ACTSCN (MKFILS,CSCEN,WDMFL,MSGFL,RETCOD)

          INTEGER,         INTENT(IN)  :: MKFILS
          CHARACTER(LEN=*),INTENT(IN)  :: CSCEN
          INTEGER,         INTENT(IN)  :: WDMFL(4),MSGFL
          INTEGER,         INTENT(OUT) :: RETCOD

          INTEGER                      :: IOPT,TMKFIL,L,I,LEN
          INTEGER                      :: LOCLFG,TRET,LUCIU
          LOGICAL                      :: LFLAG
          CHARACTER(LEN=80)            :: M
          CHARACTER(LEN=240)           :: S

2000      FORMAT(1X,A,I3,1X,A)

          TMKFIL = MKFILS

          I = 7   !debug
          WRITE(S,*) 'M_ACTSCN:entry:',CSCEN,WDMFL(1),MSGFL
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV > 0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          ! avoid some lahey math errors
          LFLAG = .TRUE.
          CALL INVALOP (LFLAG)
          CALL UNDFL (LFLAG)

          RETCOD = 0
          DO L = 1,4
            IF (WDMFL(L).GT.0) THEN
              WDM(L)%FUN = WDMFL(L)
              INQUIRE(UNIT=WDMFL(L),NAME=WDM(L)%NAM)
              I = 7   !debug
          WRITE(S,2000) 'M_ACTSCN:WDMFL:',WDM(L)%FUN,TRIM(WDM(L)%NAM)
              LEN = LEN_TRIM(S)
              CALL UPDATESTATUSX(I,LEN,S)
              IF (DBGLEV>0) THEN
                WRITE(99,*) S
                IF (DBGPAU) READ(*,*)
              END IF
            END IF
          END DO

          MSG%FUN    = MSGFL
          INQUIRE(UNIT=MSGFL,NAME=MSG%NAM)
          I = 7   !debug
          WRITE(S,2000) 'M_ACTSCN:MSGFL:',MSG%FUN,TRIM(MSG%NAM)
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV>0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          UCI%FUN= 10
          UCI%NAM= CSCEN // '.UCI'

          I = 7   !debug
          WRITE(S,2000) 'M_ACTSCN:UCIFL:',UCI%FUN,TRIM(UCI%NAM)
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV>0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          OPEN(UNIT=UCI%FUN,FILE=UCI%NAM,STATUS='OLD',ERR=10)

          I = 7   !debug
          WRITE(S,*) 'M_ACTSCN:about to call FILSET'
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)

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
C           special case, dont open files
            LUCIU = -1 * UCI%FUN
          ELSE
            LUCIU = UCI%FUN
          END IF

          I = 0
 20       CONTINUE
            I = I+ 1
            CALL FILSET (MSG%FUN,LUCIU,WDM(1)%FUN,
     M                   FILES,
     O                   ECOUNT,RETCOD)
            !keep trying-file from prev process may not have closed
          IF (RETCOD .NE. 0 .AND. I .LT. 1000000) GO TO 20

          WDM(1)%FUN = FILES(11)
          WDM(2)%FUN = FILES(12)
          WDM(3)%FUN = FILES(13)
          WDM(4)%FUN = FILES(14)

          I = 7   !debug
          WRITE(S,*) 'M_ACTSCN:back from FILSET',RETCOD
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV>0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          DO I = 1,4
            IF (WDM(I)%FUN .GT. 0) THEN
              INQUIRE(UNIT=WDM(I)%FUN,NAME=WDM(I)%NAM,ERR=10)
              J = 7   !debug
        WRITE(S,2000) 'M_ACTSCN:WDMFL:',WDM(I)%FUN,TRIM(WDM(I)%NAM)
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(J,L,S)
            END IF
          END DO        

          ECH%FUN    = FILES(1)
          INQUIRE(UNIT=ECH%FUN,NAME=ECH%NAM,ERR=10)
          I = 7   !debug
          WRITE(S,2000) 'M_ACTSCN:ECHFL:',ECH%FUN,TRIM(ECH%NAM)
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV>0) THEN
            WRITE(99,*) S
            IF (DBGPAU) READ(*,*)
          END IF

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
              I = 7   !debug
              WRITE(S,*) 'M_ACTSCN:abt 2call INTERP',I0,FILES
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(I,L,S)
              IF (DBGLEV>1) THEN
                WRITE(99,*) S
                IF (DBGPAU) READ(*,*)
              END IF

              M= "before calling INTERP"
              CALL M_FILSTA (M)

              CALL INTERP (I0,TMKFIL,
     M                     FILES,
     O                     UNIT_FLG,RETCOD)

              I = 7   !debug
              WRITE(S,*) 'M_ACTSCN:back from INTERP',RETCOD
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(I,L,S)
            ELSE
              I = 7   !debug
              WRITE(S,*) 'M_ACTSCN:skip INTERP'
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(I,L,S)
            END IF

            IF (DBGLEV>0) THEN
              WRITE(99,*) S
              IF (DBGPAU) READ(*,*)
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

          INTEGER       L,I,LOCLFG,TRET
          CHARACTER*240 S
          CHARACTER(LEN=80)            :: M
          LOGICAL       OPEN

2000      FORMAT(1X,A,I3,1X,A)

          I = 7   !debug
          WRITE(S,*) 'M_SIMSCN:entry:',TRIM(ECH%NAM)
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV > 0) THEN
            WRITE(*,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          M= "before FILSET in M_SIMSCN"
          CALL M_FILSTA (M)
          I = 7   !debug
          WRITE(S,*) 'M_SIMSCN:about to call FILSET',MSG%FUN,UCI%FUN,
     1                WDM(1)%FUN,WDM(2)%FUN,WDM(3)%FUN,WDM(4)%FUN
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)

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
            CALL FILSET (MSG%FUN,UCI%FUN,WDM(1)%FUN,
     M                   FILES,
     O                   ECOUNT,RETCOD)
            !keep trying-file from prev process may not have closed
          IF (I .LT. 1000000 .AND. RETCOD.NE.0) GO TO 20

          WDM(1)%FUN = FILES(11)
          WDM(2)%FUN = FILES(12)
          WDM(3)%FUN = FILES(13)
          WDM(4)%FUN = FILES(14)

          M= "at end of FilSet in M_SIMSCN"
          CALL M_FILSTA (M)

          I = 7   !debug
          WRITE(S,*) 'M_SIMSCN:about to call HSPF',FILES
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV > 0) THEN
            WRITE(*,*) S
            IF (DBGPAU) READ(*,*)
          END IF

          ! proceed to run model
          CALL HSPF (FILES,
     O               RETCOD)
          ! simulation complete

          I = 7   !debug
          WRITE(S,*) 'M_SIMSCN:back from HSPF with',RETCOD
          L = LEN_TRIM(S)
          CALL UPDATESTATUSX(I,L,S)
          IF (DBGLEV > 0) THEN
            WRITE(*,*) S
            IF (DBGPAU) READ(*,*)
          END IF

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

          M= "at end of File Closing in M_SIMSCN"
          CALL M_FILSTA (M)

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

        END SUBROUTINE M_SIMSCN


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
              S = '  unit ' // TRIM(C) //
     1            ' file ' // TRIM(F) // 
     1            ' is open, access:' // TRIM(A) // 
     1            ' form:' // TRIM(M) //
     1            ' r/w:' // TRIM(W) //
     1            ' recl:' // TRIM(D)            
              L = LEN_TRIM(S)
              CALL UPDATESTATUSX(J,L,S)
            END IF 
 10       CONTINUE

        END SUBROUTINE M_FILSTA

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
          DATA VARADD/ 863,     366,     880,     625    /
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
              CALL GTOSVI (OPNTAB(7,I),BINADD,
     1                     BINU)
              INQUIRE (BINU,NAME=BINNAM,ERR=8)

              !report this file name to the driver
              WRITE (CSTR,"(A4,A2,1X,I4,1X)") OPNTAB(1,I),OPNTAB(2,I),
     1                                        OPNTAB(3,I)
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
                      CALL GTOSVI (RECORD,OFFSET,
     O                             IVAL(IV,IX))
                    ELSE IF (VARTYP(IV) .EQ. 3) THEN
                      CALL GTOSVR (RECORD,OFFSET,
     O                             RVAL(IV,IX))
                    END IF
 3                CONTINUE
                  IF (VARTYP(IV) .EQ. 2) THEN
                    WRITE (VSTR(IV),"(A6,1X,20I10))")
     1                     VARNAM(IV),(IVAL(IV,IX),IX=1,MXSUB(IV))
                  ELSE IF (VARTYP(IV) .EQ. 3) THEN
                    WRITE (VSTR(IV),"(A6,1X,20(1PE12.4,1X))")
     1                     VARNAM(IV),(RVAL(IV,IX),IX=1,MXSUB(IV))
                  END IF
                  VLEN(IV)= LEN_TRIM (VSTR(IV))+ 1
                  VSTR(IV)(VLEN(IV):VLEN(IV))= ' '
 5              CONTINUE
                WRITE (UPDSTR,*) '(HSPF BINOUT_ID ',
     1                            (CSTR(J:J),J=1,CLEN1),' ',
     1                            (BINNAM(J:J),J=1,CLEN2),' '
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
                WRITE (LFIL,*) '(HSPF BINOUT_ID ',
     1                          (CSTR(J:J),J=1,CLEN1),' ',
     1                          (BINNAM(J:J),J=1,CLEN2),')'
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

          INTEGER    PeekNamedPipe, ReadFile
          DLL_IMPORT PeekNamedPipe, ReadFile, Sleep

          CHARACTER(LEN=*),INTENT(OUT)  :: S
          INTEGER    L, lr, la, lm, I
          INTEGER*1  M(200)

          S = ''
          DO WHILE (LEN_TRIM(S) .EQ. 0)
            L = PeekNamedPipe(Carg(HPIN),Carg(0),carg(0),lr,la,lm)
            IF (L .NE. 0 .AND. LA .GT. 0) THEN ! a message
              L = ReadFile(CARG(HPIN), M, CARG(la), lr, Carg(0))
              !WRITE(*,*) 'm_getstring',LR,HPIN
              !WRITE(99,*) 'm_getstring',LR,HPIN
              DO I = 1,LR-1
                S(I:I) = CHAR(M(I))
              END DO
              !WRITE(*,*) 'm_getstring',trim(s)
            ELSE
              l = 10
              call sleep(val(l))
            END IF
          END DO
          S = ADJUSTL(S)
          S = TRIM(S)

        END SUBROUTINE M_GETSTRING


      END MODULE SCENMOD
