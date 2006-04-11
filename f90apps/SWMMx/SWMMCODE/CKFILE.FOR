      SUBROUTINE CKFILE(IFILE)
C=======================================================================
C     Open a formatted output file.
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER FNAME*60,BLANK*60,FMT*60,FORM*60
      DATA BLANK/' '/
      DATA FMT/'('' Enter output filename for storm event data: '')'/
      IF(JKP(51).EQ.0) THEN
                       FORM = FMT
100                    WRITE(*,FORM)
                       READ(*,99) FNAME
                       IF(FNAME.EQ.BLANK) GO TO 100
                       JKP(51) = 1
                       OPEN(IFILE,FILE=FNAME,STATUS='UNKNOWN')
                       ENDIF
99    FORMAT(A)
      RETURN
      END
