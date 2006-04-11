C     SUBROUTINE ZCOFI6 (IFTOLD, IFTNEW, IARRAY, KARRAY, IBUFF, KBUFF,  H
C    * LRCDEL, LRETAG)                                                  H
      SUBROUTINE ZCOFIL (IFTOLD, IFTNEW, IARRAY, KARRAY, IBUFF, KBUFF,  MLu
     * LRCDEL, LRETAG)                                                  MLu
C
C
C     Z-Copy-File.
C     Copies all records in file opened with IFTNEW into
C     file opened with IFTOLD.
C     Records that have been deleted (but have not yet been
C     squeezed out) may be restored by setting LRCDEL to true.
C     New tags will be generated if LRETAG is set to true.
C
C     ZCOFIL copies via a brute-force approach;  Thus it
C     will copy all vaild data from a damaged file
C     (restoring it as best as possible).
C
C     Written by Bill Charley at HEC, 1988.
C
C     INTEGER*6 IFTNEW(*), IFTOLD(*), IBUFF(KBUFF), INFOCO(30)          H
      INTEGER*4 IFTNEW(*), IFTOLD(*), IBUFF(KBUFF), INFOCO(30)          ML
C     INTEGER IFTNEW(*), IFTOLD(*), IBUFF(KBUFF), INFOCO(30)            u
C     INTEGER*6 IADD, IIHADD, ICHADD, IUHADD, IDADD, IARRAY(KARRAY)     H
      INTEGER*4 IADD, IIHADD, ICHADD, IUHADD, IDADD, IARRAY(KARRAY)     ML
C     INTEGER IARRAY(KARRAY)                                            u
      INTEGER*4 IREC, NRECS                                             ML
      LOGICAL LRCDEL, LRETAG, LFOUND, LPROT2
      CHARACTER CPATH*85
C     CHARACTER CV*4  !TEMPORARY - REMOVE JUNE 1991 *********           H
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
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
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFTOLD(KUNIT)                  D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZCOFIL;  Unit:',I4)          D
C
C     Make Sure that we have enough space to work with
      IF ((KARRAY.LT.NBSIZE).OR.(KBUFF.LT.NBSIZE)) GO TO 900
C
C     Make sure that we are copying the same version
      IF ((IFTOLD(1).NE.6).OR.(IFTNEW(1).NE.6)) GO TO 910
      LPROT2 = LPROTC
      LCOFIL = .TRUE.
C
C     Are we in a read only state?
      IF (IFTNEW(KREADO).EQ.1) GO TO 920
C
C     Print any squeeze status message
C     IF (LSQSTA) CALL CHRWT (MUNIT, CHAR(13)//CHAR(10)//'  0% Complete'HM
C    * // CHAR(13), 16)                                                 HM
C     IF (LSQSTA) CALL CHRWT (MUNIT, '  0% Complete' // CHAR(13), 14)   u
C
C     Get the number of physical records in file (not data recs)
      IADD = IFTOLD(KFSIZE) - 1
      CALL ZGETRW (IADD, NRECS, IW)
C
      DO 200 IREC=1,NRECS
C
C     Should the status (% complete) be printed
      IF (LSQSTA) THEN
C     N = MOD (IREC,10)                                                 Hu
C     N = MOD (IREC,40)                                                 M
      IF (N.EQ.0) THEN
      IP = ((IREC-1)*100) / NRECS
      WRITE (CPATH(1:13), 25) IP
 25   FORMAT (I3,'% Complete')
      CALL CHRWT (MUNIT, CPATH(1:13)//CHAR(13), 14)
      ENDIF
      ENDIF
C
C     Read physical record IREC from file
      CALL ZGETAD (IADD, IREC, 1)
      CALL ZGTREC (IFTOLD, IARRAY, NBSIZE, IADD, .FALSE.)
C
C     Did we hit end of file?
      IF (IADD.LT.0) THEN
      WRITE (MUNIT, 28) IREC
 28   FORMAT (//, ' **** Caution:  Reached End of File at Physical',
     * ' Record:',I8,/,' **** Probable Incomplete File Copy',/)
      GO TO 800
      ENDIF
C
C     Search through this record, looking for pathname flags
C
      DO 200 IWRD=1,NBSIZE
C
      IF (IARRAY(IWRD).EQ.NPFLAG) THEN
C
C     Found a flag - Get the first three words of the information
C     block to see if this indeed is the start of a data record
      CALL ZGETAD (IADD, IREC, IWRD)
      CALL ZGTREC ( IFTOLD, INFO, 3, IADD, .FALSE.)
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,30)IADD, INFO(KISTAT), INFO(KINPAT)D
 30   FORMAT (T5,'Found a INFO Flag at address',I13,/,                  D
     * T5,'Status:',I5,';  Pathname Length:',I5)                        D
C     Is this an undelete of a renamed or deleted record?
      IF ((INFO(KISTAT).EQ.2).AND.(LRCDEL)) INFO(KISTAT) = 1
C     Check for a valid status flag (1)
      IF (INFO(KISTAT).EQ.1) THEN
C     Check for a valid pathname length
      NPATH = INFO(KINPAT)
      IF ((NPATH.GT.0).AND.(NPATH.LE.80)) THEN
C
C     Passed, therefore a valid record - Get the full information block
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1)/NCMW + 1
      CALL ZGTREC ( IFTOLD, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Get the pathname
      CPATH = ' '
      CALL HOLCHR (INFO(KIPATH), 1, NPATH, CPATH, 1)
C
C
C     If this data is time-series, and we need to use the compression
C     method of the new file, call ZRRTSX, then ZSRTSX via ZCOREC
      IF ((INFO(NPPWRD+KITYPE).EQ.100).AND.(LTSCMP).AND.
     * (INFO(NPPWRD+KIQUAL).EQ.0)) THEN
C
      IF  ((IFTNEW(KCOMPN).GT.0).OR.(INFO(NPPWRD+KICOMP).GT.0)) THEN
      IF (LRETAG) THEN
      IST = -5
      ELSE
      IST = 0
      ENDIF
      CALL ZCOREC (IFTOLD, IFTNEW, CPATH(1:NPATH), CPATH(1:NPATH),
     * IBUFF, KBUFF, IARRAY, KARRAY, IST)
      LPROTC = LPROT2
C     Re-read the record that we just destroyed
      CALL ZGETAD (IADD, IREC, 1)
      CALL ZGTREC (IFTOLD, IARRAY, NBSIZE, IADD, .FALSE.)
      GO TO 200
      ENDIF
C
      ENDIF
C
C
C     Save pertainent info from the info block
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
      IIHADD = INFO(NPPWRD+KIAIHE)
      ICHADD = INFO(NPPWRD+KIACHE)
      IUHADD = INFO(NPPWRD+KIAUHE)
      IDADD  = INFO(NPPWRD+KIADAT)
      ITYPE  = INFO(NPPWRD+KITYPE)
      NLDATA = INFO(NPPWRD+KILNDA)
      IF (LRETAG) THEN
      CTAG = ' '
      ELSE
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
      ENDIF
C
      NLEN = KIQUAL - KILNDA + 1
      DO 40 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFOCO(I) = INFO(J)
 40   CONTINUE
      DO 45 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      IPASS(I) = INFO(J)
 45   CONTINUE
C
C     Temporary for old dss versions
C     Remove June, 1991 *****
C     IF (IPASS(1).NE.0) THEN                                           H
C     CALL ZINQIR (IFTOLD, 'FVER', CV, I)                               H
C     I = ICHAR(CV(3:3))                                                H
C     IF (I.LT.69) THEN                                                 H
C     IPASS(1) = 0                                                      H
C     IPASS(2) = 0                                                      H
C     ENDIF                                                             H
C     ENDIF                                                             H
**********************************
C
      IF ((IDADD.LT.100).OR.(IDADD.GE.IFTOLD(KFSIZE))) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 50) IDADD, CPATH(1:NPATH)
 50   FORMAT(' -----DSS*** ZCOFIL;  Error: Data Address out of Range',/
     * ' Unable to Copy Record.  Data Address:',I12,/,' Pathname: ',A)
      GO TO 200
      ENDIF
C
C     Write the data record to the DSS file w/ IFTNEW
C
C     Get multiple user access
      LWRITE = .FALSE.
      CALL ZMULTU ( IFTNEW, .TRUE.)
C
C     Check if new record exists
      CALL ZCHECK (IFTNEW, CPATH(1:NPATH), NPATH, JHEAD, JDATA, LFOUND)
C
C
C     If the pathname was not found by ZCHECK write new pointers
C
      IF (.NOT.LFOUND) THEN
      CALL ZNWRIT (IFTNEW, CPATH(1:NPATH), NPATH, NIHEAD, NCHEAD,
     * NUHEAD, NDATA)
      ELSE
      IF (LPROT2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 60) CPATH(1:NPATH)
 60   FORMAT (' -----DSS---ZCOFIL:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
      CALL ZMULTU ( IFTNEW, .FALSE.)
      GO TO 200
      ENDIF
      CALL ZOWRIT (IFTNEW, CPATH(1:NPATH), NPATH, NIHEAD, NCHEAD,
     * NUHEAD, NDATA)
      ENDIF
C
C
C     Update the information block to contain what the old one had
C     But don't copy the tag
C
      NLEN = KITIME - KILNDA + 1
      DO 80 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFO(J) = INFOCO(I)
 80   CONTINUE
C
      NLEN = KIQUAL - KILNDA + 1
      IBEG = KITYPE - KILNDA + 1
      DO 90 I=IBEG,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFO(J) = INFOCO(I)
 90   CONTINUE
      DO 100 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      INFO(J) = IPASS(I)
 100  CONTINUE
C
      ISIZE = NPPWRD + NINFO
      CALL ZPTREC (IFTNEW, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
C
C     Now copy the headers and the data array
C
C     Internal header
      IF (NIHEAD.GT.0) THEN
      N = MIN0(NIHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIAIHE)
 120  CONTINUE
      CALL ZGTREC (IFTOLD, IBUFF, N, IIHADD, .FALSE.)
      CALL ZPTREC (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NIHEAD) THEN
      IIHADD = IIHADD + N
      IADD = IADD + N
      N = NIHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 120
      ENDIF
      ENDIF
C
C     Compression header
      IF (NCHEAD.GT.0) THEN
      N = MIN0(NCHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIACHE)
 140  CONTINUE
      CALL ZGTREC (IFTOLD, IBUFF, N, ICHADD, .FALSE.)
      CALL ZPTREC (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NCHEAD) THEN
      ICHADD = ICHADD + N
      IADD = IADD + N
      N = NCHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 140
      ENDIF
      ENDIF
C
C     User Header
      IF (NUHEAD.GT.0) THEN
      N = MIN0(NUHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIAUHE)
 160  CONTINUE
      CALL ZGTREC (IFTOLD, IBUFF, N, IUHADD, .FALSE.)
      CALL ZPTREC (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NUHEAD) THEN
      IUHADD = IUHADD + N
      IADD = IADD + N
      N = NUHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 160
      ENDIF
      ENDIF
C
C     Data Array
      IF (NDATA.GT.0) THEN
      N = MIN0(NDATA,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIADAT)
 180  CONTINUE
      CALL ZGTREC (IFTOLD, IBUFF, N, IDADD, .FALSE.)
      CALL ZPTREC (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NDATA) THEN
      IDADD = IDADD + N
      IADD = IADD + N
      N = NDATA - NTOT
      N = MIN0(N,KBUFF)
      GO TO 180
      ENDIF
      ENDIF
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,190) CPATH(1:NPATH)
 190  FORMAT (' -----DSS--- ZCOFIL;  Record: ',A)
C
C     Release multiple user access
      CALL ZMULTU ( IFTNEW, .FALSE.)
      LWRITE = .FALSE.
C
      ENDIF
      ENDIF
      ENDIF
C
 200  CONTINUE
C
C     Now, compute new tag-hash code blocks
      CALL ZCATFI (IFTNEW, LFOUND, LFOUND, LFOUND, .TRUE., LFOUND)
C
C     IF (LSQSTA) CALL CHRWT (MUNIT, '100' // CHAR(13), 4)              H
C     IF (LSQSTA) CALL CHRWT (MUNIT, '100' // CHAR(13) // CHAR(10), 5)  Mu
C
 800  CONTINUE
      LSQSTA = .FALSE.
      LTSCMP = .FALSE.
      LPROTC = .FALSE.
      LCOFIL = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)                               D
 820  FORMAT (T6,'-----DSS---Debug:  Exit ZCOFIL')                      D
C
      RETURN
C
 900  CONTINUE
      WRITE (MUNIT, 901) KARRAY, KBUFF, NBSIZE
 901  FORMAT (/,' ----- DSS --- ZCOFIL:  ERROR;  Buffer size(s) not',
     * ' Large enough to copy file',/,' Sizes provided:',2I8,
     * ';   Size Required:',I5,/)
      GO TO 800
C
 910  CONTINUE
      WRITE (MUNIT, 911) IFTOLD(1), IFTNEW(1)
 911  FORMAT (/,' ----- DSS --- ZCOFIL:  ERROR;  DSS File(s) not',
     * ' Version 6.',/,' Versions:',2I8,/)
      GO TO 800
C
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 921)
 921  FORMAT (/' -----DSS---ZCOFIL:  ERROR;  File has Read Access Only')
      GO TO 800
C
      END
