C
C
C
      PROGRAM   WDMDSN
C
C     + + + PURPOSE + + +
C     driver to find all wdm dataset numbers in use
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      WDMSFL,DSN,RONFLG,RETCOD,DCNT,DBUFF(16),LDCNT,I
      CHARACTER*64 WDNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBOPN, WDDSNX
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A64)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (16I5)
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(*,*) 'Enter name of WDM file: '
      READ (*,1000) WDNAME
C
      WDMSFL= 30
      RONFLG= 0
      CALL WDBOPN (WDMSFL,WDNAME,RONFLG,
     O             RETCOD)
C
      IF (RETCOD .NE. 0) THEN
C       problem opening specified wdm file
        WRITE(*,*) 'Problem opening WDM file:',RETCOD
      ELSE
C       look for datasets
        LDCNT= 0
        DCNT = 0
        DSN  = 1
 10     CONTINUE
          CALL WDDSNX(WDMSFL,
     M                DSN)
          IF (DSN .GT. 0) THEN
C           another one found
            DCNT = DCNT+ 1
            LDCNT= LDCNT+ 1
            DBUFF(LDCNT)= DSN
            IF (LDCNT.EQ.16) THEN
C             dump output buffer
              WRITE(*,2000) DBUFF
              LDCNT= 0
            END IF
            DSN  = DSN+ 1
          ELSE
C           all done
            IF (LDCNT .GT. 0) THEN
C             final buffer
              WRITE(*,2000) (DBUFF(I),I=1,LDCNT)
            END IF
            WRITE(*,*) 'count:',DCNT
          END IF
        IF (DSN .GT. 0) GO TO 10
      END IF
C
      STOP
      END
