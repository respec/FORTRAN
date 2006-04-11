      SUBROUTINE ZREADX (IFLTAB, CPATH, IIHEAD, KIHEAD, NIHEAD,
     * ICHEAD, KCHEAD, NCHEAD, IUHEAD, KUHEAD, NUHEAD, IDATA,
     * KDATA, NDATA, IPLAN, LFOUND)
C
C     Main routine for retrieving data (ZRDBUF may also be used)
C
C     Written by Bill Charley at HEC, 1989
C
C     INTEGER*6 IFLTAB(*), IIHEAD(*), ICHEAD(*), IUHEAD(*), IDATA(*)    H
C     INTEGER IFLTAB(*), IIHEAD(*), ICHEAD(*), IUHEAD(*), IDATA(*)      u
      INTEGER*4 IFLTAB(*), IIHEAD(*), ICHEAD(*), IUHEAD(*), IDATA(*)    ML
      CHARACTER CPATH*(*)
      LOGICAL LFOUND, LEND
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
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH,        D
     * KIHEAD, KCHEAD, KUHEAD, KDATA, IPLAN                             D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZREADX',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A,/,T10,'KIHEAD:',I5,',  KCHEAD:',I5,     D
     * ',  KUHEAD:',I5,',  KDATA:',I5,',  IPLAN:',I4)                   D
C
C
C     IF (IFLTAB(1).NE.6) THEN                                          H
C     NUHEAD = KUHEAD                                                   H
C     NDATA = KDATA                                                     H
C     CALL CHRLNB (CPATH, NPATH)                                        H
C     CALL ZREAD4 ( IFLTAB, CPATH, NPATH, IUHEAD, NUHEAD,               H
C    * IDATA, NDATA, IPLAN, LFOUND)                                     H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Get the info block, and read the first portion of the header
      IFLTAB(KRBNPA) = -1
      CALL ZRDBUF (IFLTAB, CPATH, IIHEAD, 0, N, IDATA, 0, J,
     * LEND, IPLAN, LFOUND)
C
      IF (LFOUND) THEN
C
C     Get get any internal header area
      JSIZE = INFO(NPPWRD+KINIHE)
      NIHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KIHEAD)
      IF (JSIZE.GT.0)
     * CALL ZGTREC (IFLTAB, IIHEAD, JSIZE, INFO(NPPWRD+KIAIHE), .FALSE.)
C
C     Get get the compression header area
      JSIZE = INFO(NPPWRD+KINCHE)
      NCHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KCHEAD)
      IF (JSIZE.GT.0)
     * CALL ZGTREC (IFLTAB, ICHEAD, JSIZE, INFO(NPPWRD+KIACHE), .FALSE.)
C
C     Get get any user header area
      JSIZE = INFO(NPPWRD+KINUHE)
      NUHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KUHEAD)
      IF (JSIZE.GT.0)
     * CALL ZGTREC (IFLTAB, IUHEAD, JSIZE, INFO(NPPWRD+KIAUHE), .FALSE.)
C
C     Get the data
      JSIZE = INFO(NPPWRD+KINDAT)
      NDATA = JSIZE
      JSIZE = MIN0 (JSIZE, KDATA)
      IF (JSIZE.GT.0)
     * CALL ZGTREC (IFLTAB, IDATA, JSIZE, INFO(NPPWRD+KIADAT), .FALSE.)
C
      ENDIF
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820) NIHEAD, NCHEAD, NUHEAD,      D
     * NDATA                                                            D
 820  FORMAT (T6,'-----DSS---Debug: Exit  ZREADX',/,T10,                D
     * 'NIHEAD:',I5,',  NCHEAD:',I5,',  NUHEAD:',I5,',  NDATA:',I5)     D
C
      RETURN
C
      END
