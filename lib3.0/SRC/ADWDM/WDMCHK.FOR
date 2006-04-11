C
C
C
      SUBROUTINE   WDMCHK
     I                    (WDMSFL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     check WDM pointer chains
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD    
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - Return code - 0 OK, -13 pointer count does not match available data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RREC,RIND,NDSN,PFDSN,TYP,CNT,TCNT,RETC,ODSN,DSN,ATFG
      LOGICAL      OPEN
      CHARACTER*64 FNAME
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(I5)
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDDSCK
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      TYP    = 0

      INQUIRE(UNIT=99,OPENED=OPEN)
      IF (OPEN .EQV. .FALSE.) THEN
        RETURN
      END IF

C     WRITE(99,2010) 'WDMCHK,PTSNUM:', WDMSFL,PTSNUM
 10   CONTINUE
C       loop looking by type
        TYP  = TYP+ 1
C       WRITE(99,*) '  TYPE:',TYP
C       bring file definition record into memory
        RREC = 1
        RIND = WDRCGO(WDMSFL,RREC)
C       calculate pointers within file definition record
        PFDSN= PTSNUM+ (TYP-1)* 2+ 1
        NDSN = WIBUFF(PFDSN,RIND)
        TCNT = WIBUFF(PTSNUM+ (TYP-1)* 2,RIND)
C
        IF (NDSN .EQ. 0) THEN
C         WRITE(99,*) '    NO DATASETS'
        ELSE
C         WRITE(99,*) '    LOOKING FOR ',TCNT,' DATASETS'
          ODSN= 0
          CNT = 0
 20       CONTINUE
C           loop to check datasets
            CALL WDDSCK (WDMSFL,NDSN,RREC,RETC)
            RIND= WDRCGO(WDMSFL,RREC)
            IF (NDSN .NE. WIBUFF(2,RIND)) THEN
              IF (WIBUFF(1,RIND) .EQ. ODSN) THEN
C               good chain
C               WRITE(99,*) '    GOOD POINTERS FOR ',NDSN
                CNT = CNT + 1
                ODSN= NDSN
                NDSN= WIBUFF(2,RIND)
              ELSE
                RETCOD = -13
                WRITE(99,*) 'WDMCHK:dsn ',WIBUFF(2,RIND),
     #                      ' does not point back to',NDSN
              END IF
            ELSE
              RETCOD = -13
              WRITE(99,*) 'WDMCHK:dsn ',NDSN,' points to itself' 
            END IF
          IF (CNT.LT.TCNT .AND. NDSN.GT.0 .AND. RETCOD.EQ.0) GO TO 20

          IF (CNT .NE. TCNT) THEN
C           big problem
            RETCOD = -13
            WRITE(99,*) 'WDMCHK:count for type ',TYP,' does not match ',
     #                   CNT,TCNT
            WRITE(99,*) '****************'
            WRITE(99,*) ' BE SURE YOU HAVE A BACKUP OF YOUR WDM FILE!!!'
            WRITE(99,*) ' '
            WRITE(99,*) ' run WDIMEX with the following records piped '
            WRITE(99,*) '   to STDIN'
            WRITE(99,*) '   (delete 2nd rec if your wdm has attributes)'
            WRITE(99,*) '   (may need to change the file name)'
            WRITE(99,*) '   (be sure you have space for temp.exp)'
            WRITE(99,*) ' rename your wdm file to a dummy name'
            WRITE(99,*) ' next run WDIMEX to import "temp.exp" to a new'
            WRITE(99,*) '   file created with the name of your old file'
            WRITE(99,*) '****************'
            INQUIRE(UNIT=WDMSFL,NAME=FNAME)
            WRITE(99,*) FNAME
            WRITE(99,*) 'c:\lib3.0\hspfmsg.wdm'
            WRITE(99,*) 'e'
            WRITE(99,*) 'temp.exp'
            WRITE(99,*) ' '
            WRITE(99,*) 's'
C           loop by dsn
            DSN = 0
            ATFG= 0
 30         CONTINUE
              DSN= DSN+ 1
              CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
              IF (RETC .EQ. 0) THEN
C               valid dsn
                WRITE(99,2000) DSN
                IF (ATFG.EQ.0) THEN
                  RIND= WDRCGO(WDMSFL,DSN)
                  IF (WIBUFF(6,RIND).LE.1) THEN 
C                   first timeseries
                    WRITE(99,*) 'A'
                    ATFG=1
                  END IF
                END IF
              END IF
            IF (DSN .LT. 32000) GO TO 30
            WRITE(99,*) '0'
            WRITE(99,*) 'r'
          END IF
        END IF
      IF (TYP .LT. 9 .AND. RETCOD .EQ. 0) GO TO 10
C
      IF (RETCOD .NE. 0) THEN
        WRITE(*,*) 'WDMCHK:dsn ptr problem, see ERROR.FIL for details'
      END IF       
C
      RETURN
      END 
C
C
C
      SUBROUTINE WDATFX
     I                 (WDMSFL,OFL)
C
C     + + + PURPOSE + + +
C     fix attribute 443/444 problem
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,OFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     OFL    - Fortran unit number of output report file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DREC,DSN,RREC,RETC,DIND,PSAVAL,
     #          PSA,SAMAX,I,PSIND,DSAIND,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO,WDDSCK,WDRCUP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C     get directory record
      DREC= 1
      DIND= WDRCGO(WDMSFL,DREC)
      IF (WIBUFF(112,DIND) .NE. 0) THEN
        WRITE(OFL,*) 'WDATFX:attribute 443/444 problem already fixed'
      ELSE
        WRITE(OFL,*) 'WDATFX:begin fixing attribute 443/444 problem'
        DSN = 0
 10     CONTINUE
C         loop by dsn
          DSN = DSN+ 1
          CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
          IF (RETC .EQ. 0) THEN
C           valid dsn, check attributes
            DIND= WDRCGO(WDMSFL,RREC)
            PSA  = WIBUFF(10,DIND)
            SAMAX= WIBUFF(PSA,DIND)
            IF (SAMAX.GT.0) THEN
C             some attributes to check
              I= 0
 30           CONTINUE
                I     = I+ 1
                PSIND = PSA+ (I*2)
                DSAIND= WIBUFF(PSIND,DIND)
                IF (DSAIND.EQ.443 .OR. DSAIND.EQ.444) THEN
C                 might be a problem
                  PSAVAL= WIBUFF(PSIND+1,DIND)
                  J= WIBUFF(PSAVAL,DIND)
                  WRITE(OFL,*) '  checking',DSN,DSAIND,J
                  IF (J.GE.1 .AND. J.LE.31) THEN
C                   we have a problem, fix it, new index #
                    WIBUFF(PSIND,DIND) = DSAIND+ 3
                    CALL WDRCUP(WDMSFL,DIND)
                  END IF
                END IF
              IF (I.LT.SAMAX) GO TO 30
            END IF
          END IF
        IF (DSN .LT. 32000) GO TO 10
C       update directory to show fix made
        DREC= 1
        DIND= WDRCGO(WDMSFL,DREC)
        WIBUFF(112,DIND) = 1
        CALL WDRCUP(WDMSFL,DIND)
        WRITE(OFL,*) '  done attribute update'
      END IF
C
      RETURN
      END
