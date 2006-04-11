C     SUBROUTINE ZCHEC6 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)   H
      SUBROUTINE ZCHECK ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)   MLu
C
C     Basic subroutine to find location of pathname record in
C     DSS file.  Returns the length of the header and data
C     areas.  The location of the record is passed back to ZWRITE
C     and ZREAD via commons.
C
C     Written by Bill Charley at HEC, 1988.
C
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      CHARACTER CPATH*(*)
      CHARACTER CLPATH*90
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      LOGICAL LFOUND
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), NPATH, CPATH    D
 20   FORMAT (T8,'-----DSS---Debug:  Enter ZCHECK;  Unit:',I4,          D
     * ',  NPATH:',I4,/,T4,'Pathname: ',A)                              D
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZCHECK',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
      IF (LBWRIT) GO TO 900
C
C     Initialization
      LFOUND = .FALSE.
      NDATA = 0
      NHEAD = 0
      NPMWRD = (NPATH - 1)/NCMW + 1
      NPPWRD = (NPATH - 1)/NCPW + 1
C
C     IPLOOP counts how many paths have this hash code - for
C     information only (effiency of the file)
      IPLOOP = 0
C
C     Force all characters in the pathname to be upper case
      DO 25 I=1,NPATH
      ICH = ICHAR(CPATH(I:I))
      IF (ICH.LT.32) THEN
      CPATH(I:I) = '?'
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)
     * 'Illegal Pathname Character at ',I,ICH
      ELSE IF (ICH.GT.96) THEN
      IF (ICH.LT.123) CPATH(I:I) = CHAR(ICH-32)
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)
     * 'Lowercase Pathname Character at ',I,ICH
      ENDIF
 25   CONTINUE
C
C     Save the pathname
      CKPATH = CPATH(1:NPATH)
C
C
C     Check to see if the last pathname accessed is the same one -
C     Are they the same length?
      IF (NPATH.EQ.IFLTAB(KLPATL)) THEN
C
C     Yes - Compare Pathnames
      CALL HOL2CH ( IFLTAB(KLPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
      LINTAB = .FALSE.
      IF (IFLTAB(KINTAB).EQ.1) LINTAB = .TRUE.
C     Was the Found flag set to Found
      IF (IFLTAB(KLPFOU).EQ.1) THEN
      IF (MLEVEL.GE.12) WRITE (MUNIT,30) 'Found'                        D
 30   FORMAT (T10,'-*- Record Previously Checked: ',A,'  -*-')          D
      ELSE
      IF (MLEVEL.GE.12) WRITE (MUNIT,30) 'NOT Found'                    D
C     On a write, force the file to be read so the correct
C     hash code and table flag is set.
      IF (LWRITE) GO TO 40
      GO TO 100
      ENDIF
C
C     If another record from a different unit has been accessed in
C     the mean time, reload this pathname bin
      IPBADD = IFLTAB(KPADD)
      JPNBIN  = IFLTAB(KPJBIN)
      NSIZE = IFLTAB(KBNSIZ)
      CALL ZGTREC (IFLTAB, IPNBIN, NSIZE, IPBADD, .FALSE.)
C
C     Now double check to ensure the file has not changed
      IF ((IPNBIN(JPNBIN).EQ.0).OR.((IPNBIN(JPNBIN).EQ.2.)
     * .AND.(.NOT.LUNDEL))) GO TO 40
      IF (NPATH.NE.IPNBIN(JPNBIN+KBNPAT)) GO TO 40
C
      LFOUND = .TRUE.
C     Report back the header and data length
      NHEAD = IFLTAB(KPNHEA)
      NDATA = IFLTAB(KPNDAT)
      GO TO 100
      ENDIF
      ENDIF
C
C
C
C     No - Check the normal way
C
 40   CONTINUE
      LINTAB = .FALSE.
C
C     Decode the Pathname
      MHASH = IFLTAB(KHASH)
      CALL ZHASH ( CKPATH, NPATH, MHASH, IHASH)
C
C     If a Hash table is used, read the address for this hash code
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IADD = NPERM + IHASH
      CALL ZGTREC (IFLTAB, IPBADD, 1, IADD, .FALSE.)
      IF (MLEVEL.GE.10) WRITE (MUNIT,45) IPBADD                         D
 45   FORMAT (T12,'Table address:',I8)                                  D
C     Does a pathname bin exist for this hash code?  (Exit if no)
      IF (IPBADD.EQ.0) GO TO 90
C
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
C
C     If no Hash table is used (type 2), read the bin directly
      IPBADD = ((IHASH - 1) * IFLTAB(KBNSIZ)) + IFLTAB(KAFBIN)
C
      ELSE
C     Should not get here
      WRITE (MUNIT,*)' IFLTAB CORRUPT IN ZCHECK, KTABLE DOES NOT MATCH'
      CALL ZABORT (IFLTAB, 100, 'ZCHECK', 0, IFLTAB(KTABLE), ' ')
      ENDIF
C
C     This Hash Code is in the table
      LINTAB = .TRUE.
 60   CONTINUE
C
C     Read the pathname bin
      ISIZE = IFLTAB(KBNSIZ)
      CALL ZGTREC (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
      JPNBIN = 1
C
C     Loop through the pathname bin, looking for this pathname
 80   CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 90
C
C     Yes - See if it is the correct one.
      IPLOOP = IPLOOP + 1
      NBWPAT = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(LUNDEL.AND.(IPNBIN(JPNBIN).EQ.2)))
     * THEN
C
C     Check the length of the pathname
      IF (NPATH.EQ.IPNBIN(JPNBIN+KBNPAT)) THEN
C     Same length - check the pathnames
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
C     Same Pathnames - record found.  Report NHEAD and NDATA
      LFOUND = .TRUE.
      NHEAD = IPNBIN(JPNBIN+KBNHEA+NBWPAT)
      NDATA = IPNBIN(JPNBIN+KBNDAT+NBWPAT)
C     Save this pathname's name and location
      IFLTAB(KPADD) = IPBADD
      IFLTAB(KPJBIN) = JPNBIN
      IFLTAB(KAINFO) = IPNBIN(JPNBIN+KBAINF+NBWPAT)
      IFLTAB(KDTYPE) = IPNBIN(JPNBIN+KBTYPE+NBWPAT)
      IFLTAB(KPNHEA) = NHEAD
      IFLTAB(KPNDAT) = NDATA
      IFLTAB(KLPFOU) = 1
      IFLTAB(KINTAB) = 1
      IFLTAB(KLPATL) = NPATH
      CALL CH2HOL ( CKPATH, IFLTAB(KLPATH), NPMWRD)
C
      GO TO 100
      ENDIF
      ENDIF
C
C     Is this the same record, and it was deleted earlier??
      ELSE IF ((IPNBIN(JPNBIN).EQ.2).AND.(LWRITE)) THEN
C     Check the length of the pathname
      IF (NPATH.EQ.IPNBIN(JPNBIN+KBNPAT)) THEN
C     Same length - check the pathnames
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
C     Yes - re-mark record as unaccessible!
      IPNBIN(JPNBIN) = 4
      CALL ZPTREC (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
      CALL ZGTREC (IFLTAB, INFO, 2, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      IF ((INFO(KIFLAG).EQ.NPFLAG).AND.(INFO(KISTAT).EQ.2)) THEN
      INFO(KISTAT) = 4
      CALL ZPTREC (IFLTAB, INFO, 2, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      ENDIF
      ENDIF
      ENDIF
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more paths in this bin (bin full) - go to next path bin
      N = IFLTAB(KBNSIZ) - 1
      IPBADD = IPNBIN(N)
      GO TO 60
      ENDIF
C     Check next path in block (unless no more)
      JPNBIN = JPNBIN + NBWPAT + NLBIN
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 90
      GO TO 80
C
C
C     Record Not Found - remember Pathname
 90   CONTINUE
      IFLTAB(KLPFOU) = 0
      IF (LINTAB) THEN
      IFLTAB(KINTAB) = 1
      ELSE
      IFLTAB(KINTAB) = 0
      ENDIF
      IFLTAB(KLPATL) = NPATH
      CALL CH2HOL ( CKPATH, IFLTAB(KLPATH), NPMWRD)
C
 100  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,120) IFLTAB(KUNIT), LFOUND, LINTAB D
 120  FORMAT (T8,'-----DSS---Debug:  Exit ZCHECK',/,T10,'Unit:',I4,     D
     * '  Found: ',L1,'  In table: ',L1)                                D
C
      RETURN
C
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/,' -----DSS---ZCHECK:  **** ERROR;  Uncompleted',
     * ' Buffered Write ****',/,' (All Buffered Writes MUST be',
     * ' Completed prior to any other DSS call.)',/)
      CALL ZABORT (IFLTAB, 60, 'ZCHECK', 0, IFLTAB(KUNIT), ' ')
C
      END
