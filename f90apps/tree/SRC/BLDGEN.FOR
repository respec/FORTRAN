      SUBROUTINE TREE_BLD (OPT,FNAM,DBG,
     O                     WDMSFL,RETCOD)

      USE TREE
      USE BLDHSPT

      INTEGER,     INTENT(IN)  :: OPT
      CHARACTER*80,INTENT(IN)  :: FNAM
      INTEGER,     INTENT(IN)  :: DBG
      INTEGER,     INTENT(OUT) :: WDMSFL,RETCOD

      CHARACTER*3, SAVE        :: FEXT
      INTEGER                  :: LOPT,POS
      LOGICAL                  :: OPNFG

      INQUIRE(UNIT=99,OPENED=OPNFG)
      IF (.NOT.OPNFG) THEN
        OPEN (UNIT=99,FILE='TREE.DBG',RECL=256)
      END IF

      CALL TREE_SET(NDBG=DBG)
      CALL TREE_INI

      IF (OPT == 0) THEN
        POS  = SCAN(FNAM,'.')
        FEXT = FNAM(POS+1:POS+3)
        CALL QUPCAS(3,FEXT)
C       WRITE(*,*)'BLDGEN:TREE_BLD:"',FEXT,'" frm "',
C    #    FNAM(1:LEN_TRIM(FNAM)),'"'
        IF (FEXT == 'UCI') THEN
C         WRITE(*,*) 'BLDGEN:TREE_BLD:call INIHSP "',
C           FNAM(1:LEN_TRIM(FNAM)),'"'
          CALL INIHSP(FNAM,DBG,
     O                WDMSFL,RETCOD)
C         WRITE(*,*) 'BLDGEN:TREE_BLD:WDMSFL,RETCOD',WDMSFL,RETCOD
          LOPT = 2
        ELSE IF (FEXT == 'INP') THEN
          CALL INIINP(DBG)
        END IF
      ELSE
        LOPT = OPT
      END IF

      IF (FEXT == 'UCI') THEN
        CALL BLDHSP(LOPT,DBG)
      END IF

      END SUBROUTINE TREE_BLD
