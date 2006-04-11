      SUBROUTINE   BUILDB (DBG)

        USE TREE

        INTEGER             :: DBG

        INTEGER             :: POS
        CHARACTER(LEN=80)   :: LINE,DESC
        CHARACTER(LEN=24)   :: MNAM,DNAM
        CHARACTER(LEN=16)   :: DUNT,DTYP

        CALL TREE_SET(NDBG=DBG)

        IF (DBG>4) WRITE(99,*) 'BUILDB  :entr'

        OPEN(UNIT=10,FILE='METHOD.INP',STATUS='OLD')
        DO
          READ(10,'(A16)',END=10) MNAM
          READ(10,'(A80)',END=10) DESC
          CALL BRAN_ADD(MNAM,DESC)
          IF (DBG>5) THEN
            WRITE(99,*) 'BUILDB  :meth "',MNAM,'"  "',DESC(1:20),'...'
          END IF
          READ(10,'(A80)',END=10) LINE
        END DO
 10     CONTINUE
        CLOSE(UNIT=10)
        IF (DBG>4) WRITE(99,*) 'BUILDB  :done METHOD'

        OPEN(UNIT=10,FILE='DATA.INP',STATUS='OLD')
        DO
          READ(10,'(A16)',END=15) DNAM
          READ(10,'(A80)',END=15) DESC
          READ(10,'(A16)',END=15) DUNT
          READ(10,'(A16)',END=15) DTYP
          CALL BRAN_ADD(DNAM,DESC,DUNT,DTYP)
          IF (DBG>5) THEN
            WRITE(99,*) 'BUILDB  :data "',DNAM,'"  "',DESC(1:20),'...'
          END IF
          READ(10,'(A80)',END=15) LINE
        END DO
 15     CONTINUE
        CLOSE(UNIT=10)
        IF (DBG>4) WRITE(99,*) 'BUILDB  :done DATA'

        OPEN(UNIT=10,FILE='TREE.INP',STATUS='OLD')
        DO
          READ(10,'(A80)',END=20) LINE
          POS = INDEX(LINE,'method')
          IF (POS > 0) THEN
            MNAM = ADJUSTL(LINE(POS+7:POS+22))
            ! WRITE(99,*) 'BUILDB  :TREE:method: ',MNAM
          ELSE
            POS = INDEX(LINE,'input')
            IF (POS > 0) THEN
              DNAM = ADJUSTL(LINE(POS+6:POS+21))
              IF (DBG>6) THEN
                WRITE(99,*) 'BUILDB  :intr "',MNAM,'"  "',DNAM,'"'
              END IF
              CALL CONN_ADD(DNAM,MNAM)
            ELSE
              POS = INDEX(LINE,'output')
              IF (POS > 0) THEN
                DNAM = ADJUSTL(LINE(POS+7:POS+22))
                IF (DBG>6) THEN
                  WRITE(99,*) 'BUILDB  :outr "',MNAM,'"  "',DNAM,'"'
                END IF
                CALL CONN_ADD(MNAM,DNAM)
              END IF
            END IF
          END IF
        END DO
 20     CONTINUE
        IF (DBG>4) WRITE(99,*) 'BUILDB :end of TREE.INP'
        CLOSE(UNIT=10)
	
      END SUBROUTINE BUILDB
