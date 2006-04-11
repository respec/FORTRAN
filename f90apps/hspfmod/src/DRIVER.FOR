C
C
C
      PROGRAM   DRIVER
C
C     + + + PURPOSE + + +
C     play with linked lists of uci files
C
C     + + + MODULES + + +
      USE  UCI
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      :: FOUND
      INTEGER      :: I, J, F, L, R, D, UCIFL, DBG
      CHARACTER*80 :: CMDLIN
      CHARACTER*12 :: K
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GETCL(CMDLIN)
      IF (LEN_TRIM(CMDLIN) > 0) THEN
        READ(CMDLIN,*) DBG
        WRITE(*,*) '---- DEBUG LEVEL',DBG,' ----'
        CALL UCI_PUT_DBG(DBG)
      END IF
      CALL UCI_NEW
      UCIFL= 20
      OPEN(UNIT=UCIFL,FILE='HSPF.UCI',STATUS='OLD')
      WRITE(*,*) '---- READ THE FILE ----'
      CALL UCI_REA(UCIFL)
      WRITE(*,*) '---- PRINT THE LIST ----'
      CALL UCI_PRT(FMT=0)
      I= 15
      J= I
      CALL UCI_PRV(I,FOUND)
      WRITE(*,*) '---- PREV OF',J,' IS',I,' ----',FOUND
      WRITE(*,*) '---- ADD A KEY ----'
      F= 1
      L= 91
      J= 100
      K= 'RUN'
      D= 1
      CALL UCI_REP(F,TYP=J)
      CALL UCI_KEY_PUT (K,D,J,F,L,R)
      WRITE(*,*) '---- PRINT THE KEYS ----',R
      CALL UCI_KEY_PRT
      WRITE(*,*) '---- GET THE KEYS ----'
      CALL UCI_KEY_GET (J,D,TEXT=K,SREC=F,EREC=L)
      WRITE(*,*) '---- GOT KEYS:',K,F,L
      WRITE(*,*) '---- PRINT THE LIST ----'
      CALL UCI_PRT
      WRITE(*,*) '---- CLOSE THE FILE ----'
      CALL UCI_CLO
C
      STOP
      END PROGRAM DRIVER
