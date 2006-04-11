      SUBROUTINE ZRETAG (IFLTAB, CPATH, NPATH, CTAG, LFOUND)
C
C     Retags a record in a DSS file with tag CTAG.
C     (similar to a rename)
C
C     Written by Bill Charley at HEC, 1989.
C
C
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*(*), CTAG*(*), CT*8
      LOGICAL LFOUND
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20) IFLTAB(KUNIT)                D
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZRETAG;  Unit:',I4)          D
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZRETAG',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C     Lock file for multiple User Access
      CALL ZMULTU ( IFLTAB, .TRUE.)
C
      CT = CTAG
C
C     Fin the pahtname bin block location
      CALL ZCHECK ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C
C     Change the tag in the pathname bin
      NPPWRD = (NPATH-1)/NCPW + 1
      CALL CHRHOL (CT, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      I = IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, IPNBIN, I, IPBADD, .FALSE.)
C
C     Get the record information area
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL ZGTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
C
C     Change the tag in the record information block
      CALL CHRHOL (CT, 1, NTAGC, INFO(NPPWRD+KITAG), 1)
      CALL ZPTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Write informative message
C
      IF (MLEVEL.GE.4) WRITE (MUNIT,100) CT, CPATH(1:NPATH)
 100  FORMAT (1X,'----DSS--ZRETAG:  ',A,2X,A)
C
      ELSE
C
C     That record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,160) CPATH(1:NPATH)
 160  FORMAT (' ----DSS--ZRETAG; Record Not Found: ',A)
C
      ENDIF
C
C     Release Multiple User Acess and dump buffer
      CALL ZMULTU ( IFLTAB, .FALSE.)
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT,220)                              D
 220  FORMAT (T8,'-----DSS---Debug:  Exit  ZRETAG')                     D
C
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL ZERROR (IFLTAB, 11, 'ZRETAG', NP, IADD, CPATH, NPATH, ' ',NP)
C
      END
