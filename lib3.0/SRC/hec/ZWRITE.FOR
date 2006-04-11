C     SUBROUTINE ZWRIT6 ( IFLTAB, CPATH, NPATH, IUHEAD, NUHEAD,         H
C    * IDATA, NDATA, IPLAN, LFOUND)                                     H
      SUBROUTINE ZWRITE ( IFLTAB, CPATH, NPATH, IUHEAD, NUHEAD,         MLu
     * IDATA, NDATA, IPLAN, LFOUND)                                     MLu
C
C     Replaced by ZWRITX
C     This stores short integer words.  For long words, call ZWRITX
C
C     IPLAN = 0   Always write
C     IPLAN = 1   Only write if new record
C     IPLAN = 2   Only write if old record
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      COMMON /WORDS/ IWORD(10)
C
C     INTEGER*6 IFLTAB(*), IIHEAD(2)                                    H
      INTEGER*4 IFLTAB(*), IIHEAD(2)                                    ML
C     INTEGER IFLTAB(*), IIHEAD(2)                                      u
      INTEGER IUHEAD(*), IDATA(*)
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
C
C
C
      IIHEAD(1) = NUHEAD
      IIHEAD(2) = NDATA
C
      ND = ((NDATA  - 1) / IWORD(8)) + 1
      NH = ((NUHEAD - 1) / IWORD(8)) + 1
C
      JTYPE = ITYPE
C
      CALL ZWRITX ( IFLTAB, CPATH, NPATH, IIHEAD, 2, ICHEAD, 0,
     * IUHEAD, NH, IDATA, ND, JTYPE, IPLAN, ISTAT, LFOUND)
C
      RETURN
      END
