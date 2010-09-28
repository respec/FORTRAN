      SUBROUTINE QFCLOS (IFL,I)
        INTEGER, INTENT(IN) :: IFL,I

        IF (I >= 0) THEN
          CLOSE(IFL,ERR=10)
        END IF
 10     CONTINUE

      END SUBROUTINE QFCLOS
C
C
C
      SUBROUTINE   FILSET
     I                   (MESSFL,UCIFL,WDMSFL,
     M                    FILES,
     O                    ECOUNT,RETCOD)
C
C     + + + PURPOSE + + +
C     set up uci and associated files for activating a scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,UCIFL,RETCOD,WDMSFL,FILES(15),ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     UCIFL  - Fortran unit number for UCI file, neg to not open files
C     RETCOD - return code, -1 - not a HSPF UCI file, -2 problem with parms
C     WDMSFL - wdm file unit number
C     FILES  - array of hspf file unit numbers
C     ECOUNT - count of errors from files block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SGRP,SCLU,I1,I0,MSGFL,INITFG,CLEN,
     1             KWDDIM(1),KWDTYP(1),CONT,KCNT
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
      CHARACTER*4  CTEMP
      LOGICAL      IOPEN
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    QFCLOS,WMSGTT,KEYUCI,DMPKEY
      EXTERNAL    FILBLK,UCIINP,WCH2UD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     if any files are already open, close them
      DO 10 I = 1,10
C       don't do for wdm file
        IF (FILES(I).NE.0) THEN
C         already open, close and set to zero
          INQUIRE (UNIT=FILES(I),OPENED=IOPEN)
          IF (IOPEN) THEN
            CLOSE (UNIT=FILES(I))
          END IF
          FILES(I) = 0
        END IF
 10   CONTINUE
C     use hspf msg file
      FILES(15)= MESSFL
C     process files block in input file
      CALL FILBLK (UCIFL,
     M             FILES,
     O             RETCOD)
      WRITE (99,*) 'FILBLK RETCOD',RETCOD
C     back to beginning of input file
      REWIND(ABS(UCIFL))
      IF (UCIFL.LT.0) THEN
C       dont do files block
        RETCOD = 0
      END IF
C
      ECOUNT = 0
      IF (RETCOD .EQ. 0) THEN
C       file block processed without error
C       read users uci file into memory
        MSGFL = FILES(1)
        CALL UCIINP (ABS(UCIFL),MESSFL,MSGFL)
C       close UCI file
        I = 0
        CALL QFCLOS (ABS(UCIFL),I)
C       get major keywords (RUN)
        SCLU  = 201
        SGRP  = 21
        INITFG= 1
        CLEN  = 20
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1000)  KWDLIB,KWDDIM,KWDTYP
C       look for major keywords
        CLEN  = 4
        CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M               ECOUNT,
     O               KCNT)
        CALL DMPKEY
      END IF
C
      RETURN
      END