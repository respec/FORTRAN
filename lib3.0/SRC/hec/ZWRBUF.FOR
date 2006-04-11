      SUBROUTINE ZWRBUF (IFLTAB, CPATH, IHEAD, NHEAD, NTHEAD,
     * IDATA, NDATA, NTDATA, LEND)
C
C     Bufferd write to the DSS file
C
C     Written by Bill Charley at HEC, 1989.
C
C     INTEGER*6 IFLTAB(*), IDATA(*), IHEAD(*), NADD                     H
      INTEGER*4 IFLTAB(*), IDATA(*), IHEAD(*), NADD                     ML
C     INTEGER IFLTAB(*), IDATA(*), IHEAD(*)                             u
      CHARACTER CTPATH*90, CPATH*(*)
      LOGICAL LNEW, LEND, LDONE, LFOUND
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH,        D
     * NHEAD, NTHEAD, NDATA, NTDATA, LEND                               D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZWRBUF',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A,/,                                      D
     * T10,'NHEAD:',I7,';  NTHEAD:',I7,/,                               D
     * T10,'NDATA:',I7,';  NTDATA:',I7,/,T10,'LEND: ',l1)               D
C
      CALL CHRLNB (CPATH, NPATH)
      NPPWRD = (NPATH-1)/NCPW + 1
      NPMWRD = (NPATH-1)/NCMW + 1
C
C     IF (IFLTAB(1).NE.6) THEN                                          H
C     CALL ZWRIT4 ( IFLTAB, CPATH, NPATH, IHEAD, NHEAD,                 H
C    * IDATA, NDATA, IPLAN, LFOUND)                                     H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C
C     Is this the first call for this record?
      LWRITE = .TRUE.
      LNEW = .TRUE.
      IF (IFLTAB(KWBNPA).EQ.NPATH) THEN
      CALL HOL2CH (IFLTAB(KWBPAT), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).EQ.CTPATH(1:NPATH)) LNEW = .FALSE.
      ENDIF
C
C
      IF (LNEW) THEN
C
C     Initialize the number of header and data stored to zero
      NHSTOR = 0
      NDSTOR = 0
C
C     Set Multiple User Access and read the permanent section
      CALL ZMULTU ( IFLTAB, .TRUE.)
C
C     Check if record exists
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZWRITE:  Check PATH'              D
      CALL ZCHECK ( IFLTAB, CPATH, NPATH, JHEAD, JDATA, LFOUND)
      LBWRIT = .TRUE.
C
C     If the pathname was not found by ZCHECK write new pointers
C
      IF (.NOT.LFOUND) THEN
C
      IF (NTDATA.LT.0) THEN
      ND = NDATA
      ELSE
      ND = NTDATA
      ENDIF
C
      CALL ZNWRIT (IFLTAB, CPATH, NPATH, 0, 0, NTHEAD, ND)
C
      ELSE
C
      IF (LPROTC) GO TO 920
C     If we are re-writing an old record, and the size is
C     unknown, we must write it at the end of the file.
C     Set the size a little larger than the current to force this
      IF (NTDATA.LT.0) THEN
      CALL ZRDINF (IFLTAB, CPATH, NH, ND, ISTAT)
      JHEAD = INFO(NPPWRD+KINUHE) + INFO(NPPWRD+KINCHE) +
     * INFO(NPPWRD+KINUHE)
      ND = ND + JHEAD + 2
      ELSE
      ND = NTDATA
      ENDIF
C
      CALL ZOWRIT (IFLTAB, CPATH, NPATH, 0, 0, NTHEAD, ND)
C
      IF (NTDATA.LT.0) THEN
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) - JHEAD - JDATA + NTHEAD
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NDATA
      INFO(NPPWRD+KINDAT) = NDATA
      ENDIF
C
C
      ENDIF
C
      IF (NTDATA.LT.0) THEN
      IFLTAB(KWBSIZ) = 2
      ELSE
      IFLTAB(KWBSIZ) = 1
      ENDIF
      IFLTAB(KWBNPA) = NPATH
      IFLTAB(KWBHAD) = INFO(NPPWRD+KIAUHE)
      IFLTAB(KWBDAD) = INFO(NPPWRD+KIADAT)
      CALL CH2HOL (CPATH, IFLTAB(KWBPAT), NPMWRD)
C
      ENDIF
C
C
C     Now store header and data arrays
      IF (NHEAD.GT.0) THEN
      NSIZE = NHEAD + IFLTAB(KWBHAD) - INFO(NPPWRD+KIAUHE)
C     Is this header size greater than that allocated?
      IF (NSIZE.GT.INFO(NPPWRD+KINUHE)) GO TO 900
      CALL ZPTREC (IFLTAB, IHEAD, NHEAD, IFLTAB(KWBHAD), .FALSE.)
      IFLTAB(KWBHAD) = IFLTAB(KWBHAD) + NHEAD
      ENDIF
C
      IF (NDATA.GT.0) THEN
      NSIZE = NDATA + IFLTAB(KWBDAD) - INFO(NPPWRD+KIADAT)
C     Is this data size greater than that allocated?
      IF (IFLTAB(KWBSIZ).EQ.1) THEN
      IF (NSIZE.GT.INFO(NPPWRD+KINDAT)) GO TO 910
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NSIZE
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NSIZE
      INFO(NPPWRD+KINDAT) = NSIZE
      INFO(NPPWRD+KILNDA) = NSIZE
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NDATA
      ENDIF
      CALL ZPTREC (IFLTAB, IDATA, NDATA, IFLTAB(KWBDAD), .FALSE.)
      IFLTAB(KWBDAD) = IFLTAB(KWBDAD) + NDATA
      ENDIF
C
C     Are we done yet?
      IF (NTDATA.LT.0) THEN
      LDONE = LEND
      ELSE
      NHSTOR = NHSTOR + NHEAD
      NDSTOR = NDSTOR + NDATA
      IF ((NHSTOR.GE.NTHEAD).AND.(NDSTOR.GE.NTDATA)) THEN
      LDONE = .TRUE.
      ELSE
      LDONE = .FALSE.
      ENDIF
      ENDIF
C
C
      IF (LDONE) THEN
      I = IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, IPNBIN, I, IPBADD, .FALSE.)
      NSIZE = NPPWRD + NINFO
      CALL ZPTREC (IFLTAB, INFO, NSIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
      IFLTAB(KWBNPA) = 0
C
C     Dump the buffers and unlock the file
      CALL ZMULTU (IFLTAB, .FALSE.)
      LBWRIT = .FALSE.
      LWRITE = .FALSE.
C
      IF (MLEVEL.GE.3) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT,510) CPATH(1:NPATH)
 510  FORMAT(' --ZWRITE: ',A)
      ELSE
      WRITE (MUNIT,520)IFLTAB(KUNIT), INFO(NPPWRD+KIVER), CPATH(1:NPATH)
 520  FORMAT(' -----DSS---ZWRITE Unit',I4,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
      ENDIF
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 820)                             D
 820  FORMAT (T6,'-----DSS---Debug: Exit ZWRBUF')                       D
      LPROTC = .FALSE.
      RETURN
C
 900  CONTINUE
      WRITE (MUNIT,901) CPATH(1:NPATH), INFO(NPPWRD+KINUHE), NSIZE
 901  FORMAT (/,' -----DSS***  ZWRBUF;  Error:  Insufficient Header',
     * ' Space Allocated -----',/,
     * ' The total amount of header space allocated is less than',/,
     * ' the amount attempting to store',/,' Pathname: ',A,/,
     * ' Allocated Size:',I8,';  Current Size:',I8,/)
      CALL ZABORT (IFLTAB, 320, 'ZWRBUF', 0, NADD, ' ')
C
 910  CONTINUE
      WRITE (MUNIT,911) CPATH(1:NPATH), INFO(NPPWRD+KINDAT), NSIZE
 911  FORMAT (/,' -----DSS***  ZWRBUF;  Error:  Insufficient Data',
     * ' Space Allocated -----',/,
     * ' The total amount of data space allocated is less than',/,
     * ' the amount attempting to store',/,' Pathname: ',A,/,
     * ' Allocated Size:',I8,';  Current Size:',I8,/)
      CALL ZABORT (IFLTAB, 330, 'ZWRBUF', 0, NADD, ' ')
C
 920  CONTINUE
      IF (MLEVEL.GE.2) WRITE (MUNIT, 921) CPATH(1:NPATH)
 921  FORMAT (' -----DSS---ZWRITE;  Record Already Exists',/,
     * ' IPLAN set to write for new records only (no data written)',/,
     * ' Pathname: ',A)
      GO TO 800
C
      END
