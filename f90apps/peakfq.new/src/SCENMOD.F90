      MODULE SCENMOD

        USE KERNEL32
        
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
      End Module Scenmod