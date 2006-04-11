      SUBROUTINE ZRDCAT (IUNIT, LALL, JUNIT, CTAGS, NTAGS, CPATH, NPATH,
     * NFOUND)
C
C     Read Pathnames from the catalog file
C     Use the tag to search for the pathname or read all pathnames
C     (if LALL is true).
C     If JUNIT is greater than zero, write to that units instead or
C     returning paths to the calling program.
C     ZTAGPA is an alternative routine (and may be prefered!)
C
C     Written by Bill Charley at HEC, 1990.
C
      CHARACTER CPATH(*)*(*), CTAGS(*)*(*), CLINE*140, CFORMT*16, CT*8
      CHARACTER CP*80
      INTEGER NPATH(*)
      LOGICAL LALL, LFIRST
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      DATA LFIRST /.TRUE./
C
C
      NFOUND = 0
      IF ((LFIRST).OR.(.NOT.LALL).OR.(NPATH(1).LE.0)) THEN
C
      IF (LALL) THEN
      LFIRST = .FALSE.
      ELSE
      DO 10 I=1,NTAGS
      NPATH(I) = 0
 10   CONTINUE
      ENDIF
C
      REWIND IUNIT
C     If we are writing pathname out to a file, clear that file
      IF (JUNIT.GT.0) THEN
      REWIND JUNIT
      WRITE (JUNIT, *, ERR=950, IOSTAT=JERR) ' '
      REWIND JUNIT
      ENDIF
C
C     Begin reading through file, looking for a pathname
      DO 60 I=1,20
      READ (IUNIT, 20, END=930) CLINE
 20   FORMAT (A)
C     Does this line contain a pathname ?
      IPLOC = INDEX (CLINE(1:60), '  /')
      IF (IPLOC.GT.0) THEN
C     Yes - Make a format to read it and other pathnames in the file
      IPLOC = IPLOC + 2
      WRITE (CFORMT, 40) IPLOC
 40   FORMAT ('(8X,A8,T',I2.2,'A)')
C     Now read the tag, and pathname
      READ (CLINE, CFORMT, ERR=940, END=900, IOSTAT=JERR) CT, CP
      GO TO 120
      ENDIF
 60   CONTINUE
      GO TO 930
C
      ENDIF
C
C
 100  CONTINUE
      READ (IUNIT,CFORMT,END=900,ERR=940,IOSTAT=JERR) CT, CP
C
 120  CONTINUE
C
      IF (LALL) THEN
      CPATH(1) = CP
      CALL CHRLNB (CPATH(1), NPATH(1))
      CTAGS(1) = CT
      NFOUND = 1
      GO TO 800
      ENDIF
C
      DO 140 I=1,NTAGS
      IF (NPATH(I).EQ.0) THEN
      IF (CT.EQ.CTAGS(I)) THEN
      NFOUND = NFOUND + 1
      IF (JUNIT.GT.0) THEN
      WRITE (JUNIT,130) NFOUND, CT, CP
 130  FORMAT (I6,2X,A,4X,A)
      NPATH(I) = 80
      ELSE
      CPATH(I) = CP
      CALL CHRLNB (CP, NPATH(I))
      ENDIF
      IF (NFOUND.GE.NTAGS) GO TO 800
      GO TO 100
      ENDIF
      ENDIF
 140  CONTINUE
      GO TO 100
C
C
 800  CONTINUE
      RETURN
C
C
C     Reached end of catalog file.
 900  CONTINUE
      IF (LALL) LFIRST = .TRUE.
      GO TO 800
C
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931)
 931  FORMAT (/' -----DSS--- ZRDCAT:  Error - Unable to Recognize',
     *' the Catalog File ',/)
      GO TO 800
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) JERR
 941  FORMAT (/' -----DSS--- ZRDCAT:  Error during Read from'
     *' the Catalog File ',/' Error:',I5,/)
      GO TO 800
C
 950  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,951) JUNIT, JERR
 951  FORMAT (/' -----DSS--- ZRDCAT:  Error during Write, Unit:',I4,
     * /,' Error:',I5,/)
      GO TO 800
C
      END
