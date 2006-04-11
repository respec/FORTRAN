C     SUBROUTINE ZDELE6 ( IFLTAB, CPATH, NPATH, LFOUND)                 H
      SUBROUTINE ZDELET ( IFLTAB, CPATH, NPATH, LFOUND)                 MLu
C
C     Deletes records from a DSS file by flagging a status
C     cell in both the pathname bin and the data area.  The
C     data is not really deleted after this call and may be
C     recovered by ZRECOV.  The data is physically removed
C     on a squeeze by DSSUTL
C
C     Written by Bill Charley at HEC, 1988.
C
C
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*(*), CTPATH*80
      LOGICAL LFOUND
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
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
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZDELET;  Unit:',I4)          D
C
C     Lock file for multiple User Access (unless call from rename)
      IF (IRENAM.EQ.0) CALL ZMULTU ( IFLTAB, .TRUE.)
C
C
C     Fin the pahtname bin block location
      CALL ZCHECK ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C
C     Set the status flag in the pathname bin to delete
      IF (IRENAM.EQ.2) THEN
      IPNBIN(JPNBIN+KBSTAT) = 3
      ELSE
      IPNBIN(JPNBIN+KBSTAT) = 2
      ENDIF
      I = IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, IPNBIN, I, IPBADD, .FALSE.)
C
C     Get the record information area
      NPPWRD = (NPATH-1)/NCPW + 1
      NPMWRD = (NPATH-1)/NCMW + 1
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL ZGTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
      CALL HOL2CH ( INFO(KIPATH), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).NE.CTPATH(1:NPATH)) GO TO 900
C
C     Set record information block status flag
      IF (IRENAM.EQ.2) THEN
      INFO(KISTAT) = 3
      ELSE
      INFO(KISTAT) = 2
      ENDIF
      CALL ZPTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Erase last pathname checked from IFLTAB
      IFLTAB(KLPATL) = 1
C
      IF (IRENAM.LE.1) THEN
C     Increment dead space (will not include unused node space)
      IFLTAB(KNRECS) = IFLTAB(KNRECS) - 1
      IFLTAB(KDEAD) = IFLTAB(KDEAD) + NINFO + NPPWRD + NHEAD + NDATA
      IF (IRENAM.EQ.0) THEN
      IADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
C     Write informative message
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,100)
     * IFLTAB(KUNIT), CPATH(1:NPATH)
 100  FORMAT (1X,'-----DSS---ZDELETE Unit',I4,':  ',A)
C
C     IF WE DELETED THE LAST RECORD, PRINT A WARNING MESSAGE
      IF ((MLEVEL.GE.2).AND.(IFLTAB(KNRECS).LE.0)) WRITE (MUNIT, 120)
     * IFLTAB(KUNIT)
 120  FORMAT (1X,'-----DSS---ZDELETE Unit',I4,':  WARNING:',
     * '  DSS File Now Empty.')
C
      ENDIF
      ENDIF
C
      ELSE
C
C     That record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,160) IFLTAB(KUNIT), CPATH(1:NPATH)
 160  FORMAT (' -----DSS---ZDELETE; Record Not Found, Unit:',I4,/,
     * ' Pathname: ',A)
C
      ENDIF
C
C     Release Multiple User Acess
      IF (IRENAM.EQ.0) CALL ZMULTU ( IFLTAB, .FALSE.)
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT,220)                              D
 220  FORMAT (T8,'-----DSS---Debug:  Exit  ZDELET')                     D
C
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL ZERROR (IFLTAB, 11, 'ZDELET', 0, IADD, CPATH, NPATH, CTPATH,
     * NP)
C
      END
