      SUBROUTINE ZGTDTS (IFLTAB, CA, NA, CB, NB, CC, NC, CF, NF,        MLu
     * JULS, ISTIME, JULE, IETIME, INTL, IHEAD, NHEAD, IOFSET,          MLu
     * VALUES, NVALS, CUNITS, CTYPE, ISTAT)                             MLu
C     SUBROUTINE ZGTDT6 (IFLTAB, CA, NA, CB, NB, CC, NC, CF, NF,        H
C    * JULS, ISTIME, JULE, IETIME, INTL, IHEAD, NHEAD, IOFSET,          H
C    * VALUES, NVALS, CUNITS, CTYPE, ISTAT)                             H
C
C     Replaced by ZRRTS
C
C     INTEGER*6 IFLTAB(*), IQUAL, IUHEAD                                H
      INTEGER*4 IFLTAB(*), IQUAL, IUHEAD                                ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 JUL, JULS, JULE, INTL, IOFSET                           ML
      INTEGER IHEAD(*)
      REAL VALUES(*)
      CHARACTER CA*(*), CB*(*), CC*(*), CF*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CPATH*80, CE*12, CDATE*9, CTIME*4
      LOGICAL LQREAD
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      CE = ' '
      I = 2
      CALL ZGINTL (INTL, CE, N, I)
      IF  (I.NE.0) GO TO 900
C
      ND = 0
      NE = 12
      CPATH = ' '
      CALL ZFPN (CA, NA, CB, NB, CC, NC, ' ', ND, CE, NE,
     * CF, NF, CPATH, NPATH)
C
      CALL DATCLN (JULS, ISTIME, JUL, IT)
      CALL JULDAT (JUL, 104, CDATE, N)
      N = M2IHM (IT, CTIME)
C
      N = NOPERS (INTL,0,JULS,ISTIME,JULE,IETIME) + 1
      IF (N.GT.NVALS) N = NVALS
      NVALS = N
      IF (NVALS.LE.0) GO TO 910
C
C
      CALL ZRRTSX ( IFLTAB, CPATH(1:NPATH), CDATE, CTIME, NVALS,
     * VALUES, IQUAL, .FALSE., LQREAD, CUNITS, CTYPE, IUHEAD, 0,
     * JHEAD, IOFSET, JCOMP, ISTAT)
C
C     Fix up header
      IF ((ISTAT.LE.4).AND.(NHEAD.GE.20)) THEN
      IHEAD(20) = IOFSET
C     CALL ZADDHD (CUNITS, CTYPE, JULS, INTL, IOFSET, VALUES,           H
C    * NVALS, IHEAD, JSTAT)                                             H
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      WRITE (MUNIT, 901) INTL
 901  FORMAT (/,' -----DSS---ZGTDTS;  ERROR:  Unrecognizable Time',
     * ' Interval',/' Interval Specified:',I6,/)
      ISTAT = 12
      GO TO 800
C
 910  CONTINUE
      WRITE (MUNIT, 911) NVALS
 911  FORMAT (/,' -----DSS---ZGTDTS;  ERROR:  Illegal Time Window',
     * ' Given',/' Number of Values:',I6,/)
      WRITE (MUNIT, 912) CDATE, CTIME, JULS
 912  FORMAT (' Starting Date: ',A,',  Time: ',A,I8)
      CALL DATCLN (JULE, IETIME, JUL, IT)
      CALL JULDAT (JUL, 104, CDATE, N)
      N = M2IHM (IT, CTIME)
      WRITE (MUNIT, 913) CDATE, CTIME, JULE
 913  FORMAT (' Ending Date: ',A,',  Time: ',A,I8,/)
      ISTAT = 15
      GO TO 800
C
      END
