C
C
C
      PROGRAM   WDATTRF
C
C     + + + PURPOSE + + +
C     Fix attribute SEADBG and SEADND conflict
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      WDMSFL,RDOFLG,RETCOD,ATMSFL,DSAIND,L,SATYP,SALEN
      CHARACTER*64 WDNAME
      CHARACTER*1  BUFF(16)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBOPN, WDATFX, WDFLCL, WDSAGY, WTGPSU
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(A64)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT('Problem with attribute 443: ',16A1,2I5,/,
     1       'Obtain a revised Attribute WDM file')
C
C     + + + END SPECIFICATIONS + + +
C
C     open error logging file
      OPEN(UNIT=99,FILE='error.fil')
C
 10   CONTINUE
        WRITE(*,*) 'Enter name of WDM file containing attributes: '
        READ (*,1000) WDNAME
        IF (LEN_TRIM(WDNAME).EQ.0) THEN
          WRITE(*,*) '  Sorry, you must specify an attribute WDM file'
          STOP
        END IF
        ATMSFL= 90
        RDOFLG= 1
        CALL WDBOPN (ATMSFL,WDNAME,RDOFLG,
     O               RETCOD)
        IF (RETCOD.NE.0) THEN
          WRITE(*,*) 'Error opening file, RETCOD ',RETCOD
        ELSE
C         check for of fixed 443
          DSAIND= 443
          CALL WDSAGY (ATMSFL,DSAIND,
     O                 BUFF,L,SATYP,SALEN,L,L)
          IF (SATYP.NE.3) THEN
            WRITE(*,2000) BUFF,SATYP,SALEN
            STOP
          END IF
        END IF
      IF (RETCOD.NE.0) GOTO 10
C
      WRITE(*,*) 'Enter name of WDM file: '
      READ (*,1000) WDNAME
C     open the WDM file
      WDMSFL= 91
      RDOFLG= 0
      CALL WDBOPN (WDMSFL,WDNAME,RDOFLG,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       open ok
        CALL WDATFX(WDMSFL,99)
C       see how timeseries pointers are doing
        CALL WTGPSU(WDMSFL,99)
C       this will check bad pointer problem
        CALL WDFLCL(WDMSFL)
      ELSE
        WRITE(*,*) 'Error opening WDM file ',RETCOD
      END IF
C
      STOP
      END 