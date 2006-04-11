      SUBROUTINE ZRECIN (IFLTAB, IOUT, ILEVEL, CPATH, IBUFF, KBUFF,
     * LFOUND)
C
C     Print out record information
C     Written by Bill Charley at HEC, 1990.
C
C     Example output:
C
C  Record Found:
C  /MUSKINGUM/ZANF5/PRECIP-INC/01JAN1970/12HOUR/OBS/
C Regular-interval time series; Tag: T14; Precision: 2; Password Applied
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   8;  Number of Data:   60;  Space Allocated:  20
C  Compressed to 43%
C  Compression Method:  3;  Delta + Repeat
C  Precision:  -2;  Base:  0.000;  Size: 2
C
C  Record Found:
C  /MUSKINGUM/ZANF5/FLOW/01JAN1990/1HOUR/OBS/
C Regular-interval time series; Tag: T14; Precision: 2; Password Applied
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   6;  Number of Data:  744;  Space Allocated: 1440
C  Data qualilty flags set
C
C
C     INTEGER*6 IFLTAB(*), IBUFF(*)                                     H
C     INTEGER IFLTAB(*), IBUFF(*)                                       u
      INTEGER*4 IFLTAB(*), IBUFF(*)                                     ML
      CHARACTER CPATH*(*), CSCRAT*80
      LOGICAL LFOUND
      LOGICAL LBASE, LRPEAT, LDELTA, LSIGDT
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH         D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZRECIN',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A)                                        D
C
C
C     IF (IFLTAB(1).NE.6) THEN                                          H
C     WRITE (MUNIT, *)'ZRECIN:  DSS file is not version 6!'             H
C     RETURN                                                            H
C     ENDIF                                                             H
C
      CALL ZRDINF (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
      IF (ISTAT.EQ.0) THEN
      LFOUND = .TRUE.
      ELSE
      LFOUND = .FALSE.
      ENDIF
C
      IF (LFOUND) THEN
C
      WRITE (IOUT,100) CPATH
 100  FORMAT (' Record Found:',/,1X,A)
C
      IF (ILEVEL.GE.2) THEN
C
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA = INFO(NPPWRD+KINDAT)
C
      ITYPE = INFO(NPPWRD+KITYPE)
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
C
C Regular-interval time series; Tag: T14; Precision: 2; Password Applied
      IF (ITYPE.EQ.100) THEN
      WRITE (CSCRAT, 110) CTAG(1:NTAGC)
 110  FORMAT (' Regular-interval time series;  Tag: ',A)
      ELSE IF (ITYPE.EQ.110) THEN
      WRITE (CSCRAT, 120) CTAG(1:NTAGC)
 120  FORMAT (' Irregular-interval time series;  Tag: ',A)
      ELSE IF (ITYPE.EQ.200) THEN
      WRITE (CSCRAT, 130) CTAG(1:NTAGC)
 130  FORMAT (' Paired Data;  Tag: ',A)
      ELSE IF (ITYPE.EQ.300) THEN
      WRITE (CSCRAT, 140) CTAG(1:NTAGC)
 140  FORMAT (' Text Data;  Tag: ',A)
      ELSE
      WRITE (CSCRAT, 150) CTAG(1:NTAGC)
 150  FORMAT (' Undefined data type;  Tag: ',A)
      ENDIF
C
      IF ((INFO(NPPWRD+KIPREC).GT.0).AND.(INFO(NPPWRD+KIPREC).LE.8))THEN
      CALL CHRLNB (CSCRAT, N)
      WRITE (CSCRAT(N+1:), 160) INFO(NPPWRD+KIPREC)
 160  FORMAT (';  Precision:',I2)
      ENDIF
C
      IF (INFO(NPPWRD+KIPASS).GT.0) THEN
      CALL CHRLNB (CSCRAT, N)
C!!!! WRITE (CSCRAT(N+1:), 170)
 170  FORMAT (';  Password Applied')
      ENDIF
C
      WRITE (IOUT, 180) CSCRAT
 180  FORMAT (A)
C
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CSCRAT(1:7), 1)
      CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CSCRAT(11:15), 1)
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CSCRAT(21:26), 1)
      WRITE (IOUT,220) CSCRAT(1:7), CSCRAT(11:15), CSCRAT(21:26)
 220  FORMAT (' Last Written on ',A,',  at ',A,'  by Program:  ',A)
C
      WRITE (CSCRAT, 240) INFO(NPPWRD+KIVER), INFO(NPPWRD+KILNDA)
 240  FORMAT ('Version:',I4,';  Number of data:',I5)
      IF (INFO(NPPWRD+KILNDA).NE.INFO(NPPWRD+KINDAT)) THEN
      WRITE (CSCRAT(36:), 260) INFO(NPPWRD+KINDAT)
 260  FORMAT (';  Space Allocated:',I5)
      ENDIF
      WRITE (IOUT, 300) CSCRAT
 300  FORMAT (1X,A)
C
      IF (INFO(NPPWRD+KIQUAL).GT.0) THEN
      WRITE (IOUT, 320)
 320  FORMAT (' Data flags set.')
C
      ELSE IF (INFO(NPPWRD+KICOMP).GT.0) THEN
      PREC = ( (REAL(INFO(NPPWRD+KINDAT)) + REAL(NCHEAD)) /
     * REAL(INFO(NPPWRD+KILNDA)) ) * 100.
      WRITE (IOUT, 400) PREC
 400  FORMAT (' Compressed to',F5.1,'%')
C
      IMETH = INFO(NPPWRD+KICOMP)
      CSCRAT = ' '
      IF (IMETH.EQ.1) CSCRAT = 'Repeat'
      IF (IMETH.EQ.2) CSCRAT = 'Delta'
      IF (IMETH.EQ.3) CSCRAT = 'Repeat + Delta'
      IF (IMETH.EQ.4) CSCRAT = 'Significant Digits'
      IF (IMETH.EQ.5) CSCRAT = 'Repeat + Significant Digits'
      WRITE (IOUT,420) IMETH, CSCRAT(1:30)
 420  FORMAT (' Compression Method:',I3,';  ',A)
C
      JSIZE = MIN0 (NCHEAD, KBUFF)
      IF (JSIZE.GT.0)
     * CALL ZGTREC (IFLTAB, IBUFF, JSIZE, INFO(NPPWRD+KIACHE), .FALSE.)
C
      CALL DHINFO (IBUFF, JSIZE, IMETH, N, BASE, LBASE, NELMS, ISIZE,
     * IDCPRE, LRPEAT, LDELTA, LSIGDT, IST)
C
      IF (LDELTA) THEN
      WRITE (IOUT, 440) IDCPRE, ISIZE, BASE, LBASE
 440  FORMAT(' Precision:',I3,';  Element Size:',I2,';  Base:',F12.2,
     * ';  User set base:',L2)
      ENDIF
C
C
      ENDIF
      ENDIF
C
      ELSE
C
      WRITE (IOUT,500) CPATH
 500  FORMAT (' Record Does Not Exist: ',/,1X,A,/)
C
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820)                              D
 820  FORMAT (T6,'-----DSS---Debug: Exit  ZRECIN')                      D
      RETURN
C
      END
