      SUBROUTINE ZSELCA (CPATH, NPATH, CCTAG, CCPROG, CCDATE, CCTIME,
     * CDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH, LCDCAT,
     * LERR)
C
C     Selective Catalog.  Used by ZCAT, which is called by ZCATLG
C     If the catalog is to be sorted, or pathnames selectively chosen,
C     this subroutine Unforms them, then either writes them to a scratch
C     file for sorting, or to the catalog file (if not to be sorted)
C
C     Written by Bill Charley at HEC, 1988.
C
      CHARACTER CCTAG*(*), CCPROG*(*), CCDATE*(*), CCTIME*(*)
      CHARACTER CDTYPE*(*), CPATH*(*)
      CHARACTER CPATH2*80, CPATH3*80, CTPATH*80, CLINE*128
      LOGICAL LSELCA, LMATCH, LCDCAT, LERR
      LOGICAL LTIMES
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      INTEGER*4 JUL, INTL, JNPATH, JP                                   ML
C
C
C
CADD C.ZDSSCA                                                           H
      INCLUDE 'zdssca.h'                                                MLu
C
CADD C.ZDSSCC                                                           H
      INCLUDE 'zdsscc.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) NPATH, CPATH(1:NPATH)          D
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZSELCA,  NPATH:',I5,/,       D
     * T13,'Path: ',A)                                                  D
      LERR = .FALSE.
C
      IF (LSORT.OR.LSELCA.OR.LTWCAT) THEN
C     Unform the Pathname
      CALL ZUPATH (CPATH(1:NPATH), IBPART, IEPART, ILPART, ISTAT)
      DO 30 I=1,6
      IF (IEPART(I).LT.IBPART(I)) IEPART(I) = IBPART(I)
 30   CONTINUE
C
C     If a condensed catalog is being generated, get the maximum
C     lengths of each part.
      IF (LCDCAT) THEN
      DO 40 I=1,6
      IF (MAXPRT(I).LT.ILPART(I) ) MAXPRT(I) = ILPART(I)
 40   CONTINUE
      CALL CHRLNB (CCTAG(1:8), ILAST)
      IF (MAXPRT(7).LT.ILAST) MAXPRT(7) = ILAST
      ENDIF
      ENDIF
C
C     If the selective catalog feature is used, check if the
C     proper parts match (or don't, as the case may be)
C
      IF (LSELCA) THEN
      CALL ZMATCA(CPATH, IBPART, IEPART, ILPART, CCDATE, CCPROG, LMATCH)
      IF (.NOT.LMATCH) GO TO 800
      ELSE
      LMATCH = .TRUE.
      ENDIF
C
C
C     Check to see if the data is time series.  If so,
C     use a julian date for the D part, and a time in
C     minutes for the E part (or similar number for Irregular TS)
C
      IF (LSORT) THEN
      LTIMES = .FALSE.
      IF (ILPART(4).EQ.9) THEN
      JST = 1
      CALL ZGINTL (INTL, CPATH(IBPART(5):IEPART(5)), NV, JST)
      IF ((JST.EQ.0).OR.(JST.EQ.1)) THEN
      LTIMES = .TRUE.
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)), JUL, IERR)
      IF (IERR.NE.0) LTIMES = .FALSE.
      ENDIF
      ENDIF
      ENDIF
C
C
C     Now write the pathname, pathname parts, and info to scratch file
      NOPTHS = NOPTHS + 1
C
      CTPATH = CPATH
      IF (LTIMES.AND.LSORT) THEN
      WRITE (CTPATH(IBPART(4):IEPART(4)), 100) JUL
      IBPART(5) = IBPART(5) - 1
      IEPART(5) = IEPART(5) + 1
C     Be sure INTL is positive (irregular int t.s. is negative)
      INTL = INTL + 100
      WRITE (CTPATH(IBPART(5):IEPART(5)), 100) INTL
 100  FORMAT (I6.6)
      ENDIF
C
C
C     Write the pathname parts for sorting, and the direct
C     access file record number
      IF (LSORT) THEN
C     Write pathname parts for sorting
      DO 120 I=1,6
      CLINE(IBPMAX(I):IEPMAX(I)) =
     * CTPATH(IBPART(IORDER(I)):IEPART(IORDER(I)))
 120  CONTINUE
      WRITE (ISUNIT(1), 140, ERR=900) CLINE(1:MTOTAL), NOPTHS           MLu
 140  FORMAT (A,I6)                                                     MLu
      ENDIF
C
C     Extended Form of Catalog
      IF (LEXTND) THEN
C
      IF (LSORT) THEN
C     Write the catalog line to the temporary direct access file
      WRITE (CLINE, 200, ERR=900)                                       MLu
     * CCTAG, CCPROG, CCDATE, CCTIME, CDTYPE, IRVERS, NDATA,            MLu
     * CPATH(1:NPATH)                                                   MLu
 200  FORMAT (A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)        MLu
      WRITE (ISUNIT(3), REC=NOPTHS, ERR=900) CLINE(1:128)               MLu
C
C     Write the catalog line to sort input file
C     WRITE (ISUNIT(1), 210, ERR=900) CLINE(1:MTOTAL),                  H
C    * CCTAG, CCPROG, CCDATE, CCTIME, CDTYPE, IRVERS, NDATA,            H
C    * CPATH(1:NPATH)                                                   H
C210  FORMAT (A, A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)     H
C
      ELSE
C     Write info and Pathname directly to the catalog
      WRITE (JCUNIT,220,ERR=900) NOPTHS, CCTAG, CCPROG,
     * CCDATE, CCTIME, CDTYPE, IRVERS, NDATA, CPATH(1:NPATH)
 220  FORMAT (I6, 2X, A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)
      IF (LMAP) WRITE (MAPUNT,240,ERR=900) CPATH(1:NPATH)
      ENDIF
 240  FORMAT (A)
C
      ELSE
C
C     Short Form of Catalog
C
      IF (LSORT) THEN
C     Write pathname parts and pathname for sorting
      WRITE (CLINE, 260, ERR=900) CCTAG, CPATH(1:NPATH)                 MLu
 260  FORMAT (A8,2X,A)                                                  MLu
      WRITE (ISUNIT(3), REC=NOPTHS, ERR=900) CLINE(1:128)               MLu
C
C     WRITE (ISUNIT(1), 270, ERR=900) CLINE(1:MTOTAL), CCTAG,           H
C    * CPATH(1:NPATH)                                                   H
C270  FORMAT (A, A8, 2X, A)                                             H
C
C     No sort, write directly to the catalog
      ELSE
C
C     If the calling program has a time window set (LTWCAT),
C     keep only one pathname for which have all
C     but the date part different
      IF (LTWCAT) THEN
      J = 1
      CALL ZGINTL (INTL, CPATH(IBPART(5):IEPART(5)), NV, J)
      IF ((J.NE.0).AND.(J.NE.1))  GO TO 300
C
      CPATH2 = ' '
      WRITE (CPATH2(1:2), '(I2)') NPATH
      CPATH2(3:) = CPATH(1:IBPART(4)-1) // CPATH(IBPART(5):NPATH)
C
      IF (NTWPAT.GT.0) THEN
      DO 280 I=1,NTWPAT
      READ (UNIT=68,REC=I) CPATH3
      READ (CPATH3(1:2), '(I2)') N
      IF (N.EQ.NPATH) THEN
      IF (CPATH2.EQ.CPATH3) GO TO 800
      ENDIF
 280  CONTINUE
      ENDIF
      NTWPAT = NTWPAT + 1
      WRITE (UNIT=68,REC=NTWPAT) CPATH2
      ENDIF
C
C
 300  CONTINUE
      JP = NOPTHS
C     Use JNPATH as counter if from subroutine ZRDCAT
      IF (JNUNIT.NE.0) JP = JNPATH
      WRITE (JCUNIT, 320, ERR=900) JP, CCTAG, CPATH(1:NPATH)
 320  FORMAT (I6,2X,A,2X,A)
      IF (LMAP) WRITE (MAPUNT, 340, ERR=900) CPATH(1:NPATH)
 340  FORMAT (A)
      ENDIF
C
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)                               D
 820  FORMAT (T8,'-----DSS---Debug:  Exit ZSELCA')                      D
      RETURN
C
C
C     Error during write
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/' **** ZCATLG - Error During Catalog Write ****',/,
     * ' Unable to Create Catalog',/)
      LERR = .TRUE.
      GO TO 800
C
      END
