C
C
C
      SUBROUTINE FILTAB (FNAME,NFREC,IPR,LFTREC)
C
C     + + + PURPOSE + + +
C     fill in common blocks with data from spec FTF file
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64 FNAME
      INTEGER      NFREC, IPR, LFTREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FNAME  - FTF file name
C     NFREC  - number of full records
C     IPR    - items per record
C     LFTREC - items in last record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ARSIZE.PRM'
      INCLUDE 'FTABLE.COM'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J, F
C
C     + + + END SPECIFICATIONS + + +
C
      F= 101
      OPEN(UNIT=F,FILE=FNAME,FORM='UNFORMATTED',ERR=90)
      I = 0
      DO J = 1, NFREC
        READ(F) FTAB(I:I+IPR-1)
        I = I + IPR
      END DO
      READ(F) FTAB(I:I+LFTREC-1)
      CLOSE(UNIT=F)
C
 90   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE TABTYP
     I                 (ADDR,
     O                  TYP)
C
C     + + + PURPOSE + + +
C     determine type of table which starts at addr
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ADDR,TYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADDR  - address where table starts
C     TABTYP - type of table
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ARSIZE.PRM'
      INCLUDE 'FTABLE.COM'
C
C     + + + END SPECIFICATIONS + + +
C
      TYP = ITAB(ADDR+2)
C
      RETURN
      END
