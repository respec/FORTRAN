      SUBROUTINE ZCAOUT (IFLTAB, ICUNIT, ICDUNT, LCDCAT, NORECS)
C
C     Write out the sorted file to the catalog file, and the condensed
C     catalog file, if set.
C
C     Written by Troy Nicolini at HEC, 1989.
C
C
      CHARACTER CPART(6)*32, CDASH*32, CFORMT*10
      CHARACTER CLINE*140, CLINEL*140, CSTR*140, CPATH*80, CTAG*8
      INTEGER  IBPART(6), IEPART(6), ILPART(6), ILPRTL(6)
      LOGICAL LWRITE, LCDCAT, LBLANK(6), LMISS
      INTEGER*4 NORECS, JULS, JULEL, JULSL, INTL, INUMB                 ML
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
C
CADD C.ZDSSCA                                                           H
      INCLUDE 'zdssca.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
C
C
      DATA CDASH /'- - - - - - - - - - - - - - - - '/
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20)                                D
 20   FORMAT (T6, '-----DSS---Debug:  Enter ZCAOUT')                    D
C
C     Initializations.
      DO 40 I=1,6
      ILPRTL(I) = 1
      CPART(I) = ' '
 40   CONTINUE
      LWRITE = .FALSE.
      LMISS = .FALSE.
      JULS  = -2
      JULSL = -3
      JULEL = -4
      CSTR = ' '
C
      MTOTAL = MTOTAL + 1
      WRITE (CFORMT, 60) MTOTAL
 60   FORMAT ('(T',I3.3,',I6)')                                         MLu
C60   FORMAT ('(T',I3.3,',A )')                                         H
      REWIND (ISUNIT(2))
C
C     Read each record from the sorted file
      DO 300 ILOOP=1,NORECS
C
C     Read the record number of the sorted file
      READ (ISUNIT(2), CFORMT, END=800) INUMB                           MLu
C     Read the catalog line corresponging to that number
      READ (ISUNIT(3), REC=INUMB) CSTR(1:128)                           MLu
C
C     Read the sorted catalog
C     READ (ISUNIT(2), CFORMT, END=800) CSTR                            H
C
      IF (.NOT.LEXTND) THEN
C
C     Short version of catalog
      CALL CHRLNB (CSTR(1:90), NSTR)
      WRITE (ICUNIT,100,ERR=900) ILOOP, CSTR(1:NSTR)
 100  FORMAT (I6,2X,A)
      IF (LMAP) WRITE (MAPUNT,120,ERR=900) CSTR(9:NSTR)
 120  FORMAT (A)
C
      ELSE
C
C     Extended Version of Catalog
      CALL CHRLNB (CSTR, NSTR)
      WRITE (ICUNIT,100,ERR=900) ILOOP, CSTR(1:NSTR)
      IF (LMAP) WRITE (MAPUNT,120,ERR=900) CSTR(49:NSTR)
      ENDIF
C
C
C     Is a compressed catalog being produced?
      IF (.NOT.LCDCAT) GO TO 300
C
      DO 200 N=1,6
      LBLANK(N) = .TRUE.
 200  CONTINUE
C
      CLINE = ' '
      CTAG = CSTR (1:8)
C
      IF (.NOT.LEXTND) THEN
      CPATH = CSTR (11:)
      ELSE
      CPATH = CSTR (49:)
      ENDIF
C
C     Insert the tag at the start of the line.
      IEND = MAXPRT(7)
      CLINE(1:IEND) = CTAG
C
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
C
      DO 220 N=1,5
      MPART = IORDER(N)
C
      IBEGIN = IEND + 3
      IEND   = IBEGIN + MAXPRT(MPART) - 1
C
C     Test for missing part.
      IF (ILPART(MPART).EQ.0) THEN
      CLINE(IBEGIN:IBEGIN + 5) = '(null)'
C
C     Test for equality of parts.
      ELSE IF  ((CPATH(IBPART(MPART):IEPART(MPART)).EQ.
     * CPART(MPART)(1:ILPRTL(MPART))).AND.(LBLANK(MPART))) THEN
      CLINE(IBEGIN:IEND-1) = CDASH
C
C     Otherwise write part to CLINE and update the lagging variables.
      ELSE
      CLINE(IBEGIN:IEND) = CPATH(IBPART(MPART):IEPART(MPART))
      ILPRTL(MPART) = ILPART(MPART)
      CPART(MPART) = CPATH(IBPART(MPART):IEPART(MPART))
      J = IORDER(N+1)
      LBLANK(J) = .FALSE.
      ENDIF
C
 220  CONTINUE
C
C
C     Set up to test whether the date part of the current line is
C     in sequence with date part of the lagging line. This is only
C     necessary if parts ABCFE  of the current line are the same as
C     parts ABCFE  of the lagging line.  (I.e. LBLANK  is true.)
C
      IF (LBLANK(4)) THEN
      IF ((ILPART(4).LT.7).OR.(ILPART(5).LT.4)) THEN
      JULS = -10
      GO TO 240
      ENDIF
C
C     Get the julian date of this pathname
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULS,IERR)
      IF (IERR.NE.0) GO TO 240
      JULEL = JULSL
      IST = 1
C
C     Get the julian date for the next block
      CALL ZGINTL (INTL, CPATH(IBPART(5):IEPART(5)), NVALS, IST)
      IF (IST.EQ.0) THEN
C     Regular interval
      CALL ZBEGDT (JULEL, INTL, IYR, IMON, IDAY, IBLOCK)
      ELSE IF (IST.EQ.1) THEN
C     Irregular interval
      CALL ZIRBEG (IFLTAB, JULEL, CPATH(IBPART(5):IEPART(5)), IYR,
     * IMON, IDAY, IBLOCK, IDUM1, IDUM2)
      ELSE
C     Not time series
      JULEL = -5
      GO TO 240
      ENDIF
C     Increment the date to the next block
      CALL ZINCBK (IBLOCK, JULEL, IYR, IMON, IDAY)
      ENDIF
C
C     End of date set up block.
C
 240  CONTINUE
      IBEGIN = IEND + 3
      IEND  = IBEGIN + MAXPRT(4) - 1
C
C     Test for missing part.
      IF (ILPART(4).EQ.0) THEN
      CLINE(IBEGIN:IBEGIN + 5) ='(null)'
      LWRITE = .TRUE.
C
C     Test for equality of previous parts and whether dates are in
C     sequence.  Hyphenate current date onto lagging line.
C     Update lagging variable JULSL.
      ELSE IF (LBLANK(4) .AND. (JULS.EQ.JULEL)) THEN
      CLINEL(IENDL + 1:IENDL + 3) = ' - '
      CLINEL(IENDL+ 4:IENDL+ 4+ MAXPRT(4)) = CPATH(IBPART(4):IEPART(4))
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
C
      ELSE IF (LBLANK(4) .AND. (JULS.NE.JULEL) .AND. (LCCDAT)) THEN
      CLINEL(IENDL + 1:IENDL + 3) = ' - '
      CLINEL(IENDL+ 4:IENDL+ 6+ MAXPRT(4)) = CPATH(IBPART(4):IEPART(4))
     * // ' *'
      LMISS = .TRUE.
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
C
C     Otherwise, write the current date to CLINE and
C     update the lagging variables.
      ELSE
      CLINE(IBEGIN:IEND) = CPATH(IBPART(4):IEPART(4))
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
      ILPRTL(4) = ILPART(4)
      CPART(4) = CPATH(IBPART(4):IEPART(4))
      LWRITE = .TRUE.
      ENDIF
C
C
      IF (LWRITE) THEN
      IF (ILOOP.GT.1) WRITE (ICDUNT,260,ERR=900) CLINEL
 260  FORMAT (1X,A)
      CLINEL = CLINE
      IENDL = IBEGIN + ILPART(4) - 1
      LWRITE = .FALSE.
      ENDIF
C
C
 300  CONTINUE
C
      IF (LCDCAT) THEN
      IF (LWRITE) THEN
      WRITE (ICDUNT,260,ERR=900) CLINE
      ELSE
      WRITE (ICDUNT,260,ERR=900) CLINEL
      ENDIF
      IF (LMISS) WRITE (ICDUNT, 310, ERR=900)
 310  FORMAT ('   *  Record time span has missing periods.')
      ENDIF
C
C
 800  CONTINUE
      LCCDAT = .FALSE.
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)                               D
 820  FORMAT (T6,'-----DSS---Debug:  EXIT ZCAOUT')                      D
      RETURN
C
C
C     Error during write.
 900  CONTINUE
      WRITE (MUNIT, 901)
 901  FORMAT (/' **** ERROR - ZCAT:   Error during write to',
     * ' the catalog file ****',/,' Unable to complete catalog ')
      GO TO 800
C
C
      END
