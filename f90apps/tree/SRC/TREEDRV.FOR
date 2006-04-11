      PROGRAM TREEDRV

      USE TREE

      INTEGER    MXPSIZ
      PARAMETER (MXPSIZ=20)

      CHARACTER(LEN=80)  :: CMDLIN,DESC,TMP,FNAM
      CHARACTER(LEN=24)  :: NAME,PCONAM,ONAM
      CHARACTER(LEN=16)  :: TYPE,PNAM
      CHARACTER(LEN=1)   :: BDIR,COPT
      INTEGER            :: OPT,RETCOD,LEV,OFL,IFL,DBG,FLG,POS,CNT,BOPT
      INTEGER            :: I,CONID(MXCON),WDMSFL,IN,OU,SK,IND
      INTEGER            :: VAL(MXPSIZ),PTYP,SIZ
      INTEGER            :: LEVUSE,LEVCNT(MXLEV),LEVPOSMX(MXLEV)
      REAL               :: RVAL(MXPSIZ)

1000  FORMAT(A1,1X,A1,1X,A24)
2000  FORMAT(1X,A24,' lmx:',I4,' tct:',I4,' cnt:',20I4)
2010  FORMAT(25X,2(9X),' vps:',20I4)
2020  FORMAT(A,A24,A,' lev:',I4,' ind:',I4,' pos:',I4,' cnt:',I4,
     #       1X,A24,A16,A80)
2030  FORMAT(24X,  A,' con:',20I4)
2040  FORMAT(20A4)
2050  FORMAT(24X,  A,' prm: ',A16,5X,' val: ',A)
2060  FORMAT(24X,  A,' prm: ',A16,5X,' val: ',20I4)
2070  FORMAT(24X,  A,' prm: ',A16,5X,' val: ',20F8.3)

      ! get input file name - this is a system-specific routine
      CMDLIN= 'HSPF.UCI'
      CALL GETCL(CMDLIN)

      CALL CMDPARSE (.TRUE.,CMDLIN,
     O               FNAM)

      LEVMIN= 6
      DBG   = 2
      CALL CMDPARSE (.FALSE.,CMDLIN,
     O               TMP)
      IF (LEN_TRIM(TMP) > 0) THEN
        READ(TMP,*) LEVMIN
        CALL CMDPARSE (.FALSE.,CMDLIN,
     O                 TMP)
        IF (LEN_TRIM(TMP) > 0) THEN
          READ(TMP,*) DBG
        END IF
      END IF

      WRITE(*,*) 'TREE_DRV:"',FNAM(1:LEN_TRIM(FNAM)),'"',LEVMIN,DBG

      BOPT= 0
      CALL TREE_BLD (BOPT,FNAM,DBG,
     O               WDMSFL,RETCOD)

      OFL = 98
      OPEN (UNIT=OFL,FILE='TREEDRV.OUT')

      IFL = 97
      OPEN(UNIT=IFL,FILE='TREEDRV.OPT',STATUS='OLD')
      DO
        WRITE(OFL,*)  '*********************************'
        IF (DBG>5) THEN
          WRITE(99,*)   '*********************************'
        END IF
        ONAM = NAME
        READ (IFL,1000,END=20) COPT,BDIR,NAME
        IF (LEN_TRIM(NAME) == 0) THEN
          NAME = ONAM
        END IF

        IF (COPT == 'B') THEN
          READ(BDIR,'(I1)') OPT
          WRITE(99,*) 'TREE_DRV:rbld:',OPT
          WRITE(*,*)  'TREE_DRV:rbld:',OPT
          CALL TREE_BLD (OPT,FNAM,DBG,
     O                   WDMSFL,RETCOD)
        ELSE IF (COPT == 'R') THEN
          CALL TREE_ROOT(NAME)
          WRITE(99,*) 'TREE_DRV:root:"',NAME(1:LEN_TRIM(NAME)),'"'
          WRITE(*,*)  'TREE_DRV:root:"',NAME(1:LEN_TRIM(NAME)),'"'
        ELSE IF (COPT == 'L') THEN
          WRITE(99,*) 'TREE_DRV:list:"',NAME(1:LEN_TRIM(NAME)),'"'
          WRITE(*,*)  'TREE_DRV:list:"',NAME(1:LEN_TRIM(NAME)),'"'
          CALL TREE_SUM(LEVUSE,LEVCNT,LEVPOSMX,NAME,BDIR)
          IF (LEVUSE >= LEVMIN) THEN
            WRITE(OFL,2000) NAME,LEVUSE,
     #                      SUM(LEVCNT(1:LEVUSE)),LEVCNT(1:LEVUSE)
            WRITE(OFL,2010) LEVPOSMX(1:LEVUSE)
            LEV = 0
            FLG = 1
            IND = 0
            DO WHILE (FLG >= 0)
              IND = IND+ 1
              CALL DISP_LIS (FLG,
     O                       NAME,DESC,LEV,POS,CNT,CONID,PCONAM,TYPE)
              IF (FLG == 0) THEN
                IN = ((LEV-1)*2) + 1
                OU = (LEVUSE*2)- IN
                WRITE(OFL,2020) REPEAT(' ',IN),NAME,REPEAT(' ',OU),
     #                          LEV,IND,POS,CNT,PCONAM,TYPE,DESC
                SK = LEVUSE*2
                IF (CNT > 0 .AND. DBG > 6) THEN
                  WRITE(OFL,2030) REPEAT(' ',SK),(CONID(I),I=1,CNT)
                END IF
                I = 0
                DO
                  I = I+ 1
                  CALL BRAN_GET_PARM(NAME,I,MXPSIZ,PNAM,SIZ,VAL,PTYP)
                  IF (PTYP == -1) THEN
                    EXIT
                  ELSE IF (PTYP == 0) THEN
                    WRITE(TMP,2040) VAL(1:SIZ)
                    SIZ = LEN_TRIM(TMP)
                    WRITE(OFL,2050) REPEAT(' ',SK),PNAM,TMP(1:SIZ)
                  ELSE IF (PTYP == 1) THEN
                    WRITE(OFL,2060) REPEAT(' ',SK),PNAM,VAL(1:SIZ)
                  ELSE IF (PTYP == 2) THEN
                    RVAL(1:SIZ) = TRANSFER(VAL(1:SIZ),RVAL)
                    WRITE(OFL,2070) REPEAT(' ',SK),PNAM,RVAL(1:SIZ)
                  END IF
                END DO
              END IF
            END DO
          END IF
        ELSE IF (COPT == 'D') THEN
          WRITE(99,*)  '***** BEFORE DELETE BRANCH:UP'
          WRITE(OFL,*) '***** BEFORE DELETE BRANCH:UP'
          CALL TREE_DMP
          WRITE(99,*)  '***** DELETE ',NAME
          WRITE(OFL,*) '***** DELETE ',NAME
          CALL BRAN_DEL(NAME)
          WRITE(99,*)  '***** AFTER DELETE BRANCH:DOWN'
          WRITE(OFL,*) '***** AFTER DELETE BRANCH:DOWN'
          CALL TREE_DMP
        END IF
      END DO
 20   CONTINUE

      CALL TREE_INI

      WRITE(99,*) 'TREE_DRV:end'

      END PROGRAM TREEDRV


      SUBROUTINE CMDPARSE (RST,INP,OUT)

      LOGICAL,          INTENT(IN)  :: RST
      CHARACTER(LEN=80),INTENT(IN)  :: INP
      CHARACTER(LEN=80),INTENT(OUT) :: OUT

      INTEGER                       :: POS
      INTEGER, SAVE                 :: STR = 1

      IF (RST) THEN
        STR = 1
      END IF

      POS = SCAN(INP(STR:80),' ')
      OUT = INP(STR:STR+POS-2)
      STR = STR + POS
      WRITE(*,*) 'CMDPARSE:"',OUT(1:LEN_TRIM(OUT)),'"',STR,
     #                ' "',INP(1:LEN_TRIM(INP)),'"'

      RETURN
      END
