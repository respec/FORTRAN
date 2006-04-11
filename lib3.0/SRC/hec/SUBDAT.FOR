      SUBROUTINE SUBDAT ( CLINE )
C
C     SUBSTITUTE THE DATE/TIME IN CLINE WHERE LOWER CASE APPEAR
C
C     ddd - Julian day of current year  023
C     dd  - Day of current month  03
C     d   - Day of week as digit 1=Sunday  to  7=Saturday
C     www - Day of week - SUN, MON, TUE
C     ww  - Week number of current year 02   01 starts first Sunday
C     w   - Day of week as a letter S=Sunday,  M T W H F A=Saturday
C     mmm - Month of year FEB
C     mm  - Month number of year 04
C     yyyy- Year 1987
C     yy  - Year 87
C     y   - Year 7 for 1987
C     hh  - Hour   13
C     nn  - Minute  45
C     ss  - Second  28
C
C
      PARAMETER ( MX = 14 )
C
      CHARACTER CLINE*(*), CT(MX)*4, CS(MX)*4, CDAT*9, CTIM*8, CD(7)*3
      CHARACTER CDAY(7)*1
      INTEGER IT(MX)
      INTEGER*4 JUL, JUL001, JULWK, ITS                                 MLlg
C
      DATA CT/'ddd ','dd  ','www ','ww  ','mmm ','mm  ',
     1        'yyyy','yy  ','hh  ','nn  ','ss  ',
     2        'd'   ,'w'   ,'y'/
C
      DATA CD    /'SUN','MON','TUE','WED','THU','FRI','SAT'/
      DATA CDAY / 'S',  'M',  'T',  'W',  'H',  'F',  'A' /
C
      DATA IT/3,2,3,2,3,2,4,2,2,2,2,1,1,1/
C
CC    WRITE(3,*) CLINE
C
      CALL CDATE ( CDAT )
      CALL CTIME ( CTIM )
      CALL CURTIM ( JUL, MIN )
      CALL DATIME ( IY, JDAY, ITS )
      ID = JLIYMD ( JUL, IYR, IMON, IDAY )
      IDOW = IDAYWK ( JUL )
      JUL001 = JUL - JDAY + 1
      JDOW = IDAYWK ( JUL001 )
      JULWK = JUL001
      IF(JDOW .GT. 1) JULWK = JUL001 + (8-JDOW)
      IWK = (JUL-JULWK) /7  + 1
C
      WRITE(CS(1),10) JDAY
      CS(2) = CDAT(1:2)//'  '
      CS(3) = CD(IDOW)//' '
      WRITE(CS(4),11) IWK
      CS(5) = CDAT(4:6)//' '
      WRITE(CS(6),11) IMON
      CS(7) = '19'//CDAT(8:9)
      CS(8) = CDAT(8:9)//'  '
      CS(9) = CTIM(1:2)//'  '
      CS(10)= CTIM(4:5)//'  '
      CS(11)= CTIM(7:8)//'  '
      WRITE(CS(12),12) IDOW
      CS(13)= CDAY(IDOW)
      CS(14)= CDAT(9:9)
C
CC    WRITE(3,*) CT
CC    WRITE(3,*) CS
      DO 20 I = 1, MX
      K = IT(I)
      J = INDEX( CLINE, CT(I)(1:K) )
CC    WRITE(3,*) I,K,J
      IF ( J.GT.0 ) THEN
      CLINE(J:J+K-1) = CS(I)(1:K)
      ENDIF
   20 CONTINUE
C
   10 FORMAT(I3.3,' ')
   11 FORMAT(I2.2,'  ')
   12 FORMAT(I1,'   ')
CC    WRITE(3,*) CLINE
      RETURN
      END
