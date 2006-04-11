      SUBROUTINE ZUNDEL ( IFLTAB, CPATH, NPATH, ISTAT)
C
C     Undeletes records in a DSS previously deleted by ZDELET.
C     ZDELET sets a flag in the pathname bin and data area
C     indicating that the record has been deleted.  The
C     data is not really deleted until a squeez by DSSUTL.
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
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20)                              D
 20   FORMAT (T8,'-----DSS---Debug: Enter ZUNDEL')                      D
C
C     IF (IFLTAB(1).NE.6) THEN                                          H
C     WRITE (MUNIT, *)'ZUNDEL:  DSS file is not version 6!'             H
C     WRITE (MUNIT, *)'Can only undelete with version 6 files'          H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C
      ISTAT = 0
      LUNDEL = .TRUE.
C
C
C     Should all records in the file be undeleted??
      IF (CPATH(1:NPATH).EQ.'ALL') THEN
      CALL ZUDALL (IFLTAB, 0)
      ELSE
C
C     Just undelete a single  record
C
C     Lock file for multiple User Access
      CALL ZMULTU ( IFLTAB, .TRUE.)
C
C     Find the pathname bin block location
      IFLTAB(KLPATL) = -1
      CALL ZCHECK ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C
C     Make sure the status flag is set to delete
      IF (IPNBIN(JPNBIN+KBSTAT).NE.2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT,80) IFLTAB(KUNIT), CPATH(1:NPATH)
 80   FORMAT (' -----DSS***ZUNDEL Unit',I4,':  Record Already Exists;',/
     * ' Pathname: ',A)
      ISTAT = 2
      GO TO 200
      ENDIF
C
C     Reset the status flag in the pathname bin to OK
      IPNBIN(JPNBIN+KBSTAT) = 1
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
C     Reset record information block status flag
      INFO(KISTAT) = 1
      CALL ZPTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Decrement dead space
      IFLTAB(KNRECS) = IFLTAB(KNRECS) + 1
      IFLTAB(KDEAD) = IFLTAB(KDEAD) - (NINFO + NPPWRD + NHEAD + NDATA)
      IADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
C     Write informative message
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,100) IFLTAB(KUNIT), CPATH(1:NPATH)
 100  FORMAT (1X,'-----DSS---ZUNDEL Unit',I4,':  ',A)
C
C
      ELSE
C
C     That record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,160) IFLTAB(KUNIT), CPATH(1:NPATH)
 160  FORMAT (' -----DSS***ZUNDEL Unit',I4,':  Unable to Find Record;',/
     * ' Pathname: ',A)
      ISTAT = 1
C
      ENDIF
C
      ENDIF
C
 200  CONTINUE
C     Release Multiple User Acess
      CALL ZMULTU ( IFLTAB, .FALSE.)
      LUNDEL = .FALSE.
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE ( MUNIT,820)                              D
 820  FORMAT (T8,'-----DSS---Debug:  Exit ZUNDEL')                      D
C
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL ZERROR (IFLTAB, 11, 'ZUNDEL', NP, IADD, CPATH, NPATH,
     * CTPATH, NP)
C
      END
