      SUBROUTINE ZRDPAT (IUNIT, IPOS, INUMB, CTAG, CPATH, NPATH, LEND)
C
C    Read Pathnames according to reference numbers from the catalog file
C
C     Written by Bill Charley at HEC, 1990.
C
      CHARACTER CPATH*(*), CLINE*140, CFORMT*40, CTAG*(*)
      INTEGER*4 IPOS, INUMB                                             ML
      LOGICAL LEND, LTAG
C
      SAVE LTAG, CFORMT
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      COMMON /ZDSSC1/ CPDATE, CPPROG
      CHARACTER CPDATE*7, CPPROG*6
C
C
C
      LEND = .FALSE.
      NLEN = LEN(CPATH)
      IF (NLEN.GT.80) THEN
      CPATH = ' '
      NLEN = 80
      ENDIF
C
      IF (IPOS.EQ.0) THEN
      REWIND IUNIT
C     Begin reading through file, looking for a pathname
      DO 80 I=1,20
      READ (IUNIT, 20, END=910) CLINE
 20   FORMAT (A)
C     Does this line contain a pathname ?
      IPLOC = INDEX (CLINE, '  /')
C
      IF (IPLOC.GT.0) THEN
C     Yes - Make a format to read it and other pathnames in the file
      IPLOC = IPLOC + 2
      IF (CLINE(6:6).EQ.' ') THEN
      LTAG = .FALSE.
      CPDATE = ' '
      CPPROG = ' '
      WRITE (CFORMT, 40) IPLOC
 40   FORMAT ('(I5,T',I2.2,',A)')
      ELSE
      LTAG = .TRUE.
      WRITE (CFORMT, 60) IPLOC
 60   FORMAT ('(I6,2X,A8,2X,A6,2X,A7,T',I2.2,',A)')
      ENDIF
C
C     Now read the reference number, tag, and pathname
      IF (LTAG) THEN
      READ (CLINE,CFORMT,ERR=920,IOSTAT=JERR) IPOS, CTAG, CPPROG,
     * CPDATE, CPATH(1:NLEN)
      ELSE
      READ (CLINE, CFORMT, ERR=920, IOSTAT=JERR) IPOS, CPATH(1:NLEN)
      CTAG = ' '
      ENDIF
      IF (IPOS.LT.INUMB) GO TO 100
      GO TO 800
      ENDIF
C
 80   CONTINUE
      GO TO 910
      ENDIF
C
C
 100  CONTINUE
      IF (LTAG) THEN
      READ (IUNIT, CFORMT, END=900, ERR=920, IOSTAT=JERR) IPOS, CTAG,
     * CPPROG, CPDATE, CPATH(1:NLEN)
      ELSE
      READ (IUNIT, CFORMT, END=900, ERR=920, IOSTAT=JERR) IPOS,
     * CPATH(1:NLEN)
      CTAG = ' '
      ENDIF
      IF (IPOS.LT.INUMB) GO TO 100
C
C
 800  CONTINUE
      INUMB = IPOS
      CALL CHRLNB (CPATH(1:NLEN), NPATH)
C
 820  CONTINUE
      RETURN
C
C
C
C     We have reached the end of the catalog,
C     Set IPOS as a flag, and save the number of the last pathname
 900  CONTINUE
      INUMB = IPOS
      LEND = .TRUE.
      GO TO 820
C
 910  CONTINUE
      WRITE (MUNIT,911)
 911  FORMAT (/' -----DSS--- ZRDPAT:  Error - Unable to Recognize',
     *' the Catalog File ',/)
      GO TO 900
C
 920  CONTINUE
      WRITE (MUNIT,921) JERR, CFORMT
 921  FORMAT (/' -----DSS--- ZRDPAT:  Error during Read from'
     *' the Catalog File ',/' Error:',I5,',  Format: ',A,/)
      GO TO 900
C
      END
