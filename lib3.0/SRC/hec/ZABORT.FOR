      SUBROUTINE ZABORT (IFLTAB, IERR, CSUB, JERR, IADD, CCOMET)
C
C     Records abort messages
C     On Harris records errors at the end of file SYST*DSS6-ERR
C
C     Written by Bill Charley at HEC, 1989
C
C     Error Codes:
C        10 - Pointer or address array incorrect
C        11 - Pointer or address array incorrect (Wrong Pathname)
C        20 - Illegal Unit number
C        30 - Error on Physical Read
C        40 - Error on Physical Write
C        41 - Disk Space Exceeded
C        50 - Corrupt IFLTAB array (Keys don't match)
C        60 - Incomplete Buffered write
C        70 - DSS File Not Opened
C       100 - Illegal KTABLE vaiable (IFLTAB Corrupt)
C       110 - Illegal Number of Characters per Machine word Set
C       120 - Time-series header length too small
C       130 - Excess write on read only file errors
C       200 - Unable to make shared assignment
C       210 - Unable to lock file
C       220 - Unalbe to unlock file
C       300 - Uncompatiable Versions
C       310 - Data Compression Used in old code
C       320 - Insufficient Header Space in ZWRBUF
C       330 - Insufficient Data Space in ZWRBUF
C
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
C     INTEGER IQ(3), IA(3)                                              H
      CHARACTER CSUB*(*), CCOMET*(*)
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C     INTEGER ILINE(44), IB(6)                                          H
C     CHARACTER CLINE*132, CA*8, CPRO*18                                H
C     EQUIVALENCE (ILINE, CLINE), (IA,CA)                               H
C
C
C
      WRITE (MUNIT,20) IERR, CSUB
 20   FORMAT (' Error Code',I5,',  occuring in subroutine ',A)
      CALL CHRLNB (CKPATH, N)
      IF (N.EQ.0) N = 1
      WRITE (MUNIT, 30) CKPATH(1:N)
 30   FORMAT (' Last Pathname Accessed: ',A)
C
C
C     Assign file.  If it does not exist, don't record message
C
C     IW=0                                                              H
C     CALL ATOTA (8H0000SYST,IQ,8)                                      H
C     CALL ATOTA (8HDSS6-ERR,IA,8)                                      H
C40   CALL ASSIGG(143,IQ,IA,0,IRR)                                      H
C     IF (IRR.EQ.0) THEN                                                H
C     OK GO ON
C     ELSE IF (IRR.EQ.10) THEN                                          H
C     HERE FOR BUSY FILE
C     WAIT 24 TICKS = .2 SEC
C     CALL WAIT(24,0,KLM)                                               H
C     IW=IW+1                                                           H
C     IF IT TAKES MORE THAN 4 SEC GIVE UP
C     IF(IW.GE.20) GO TO 100                                            H
C     GO TO 40                                                          H
C     ELSE                                                              H
C     HERE FOR SOME OTHER ERROR--  FILE NOT PRESENT,ETC
C     GO TO 100                                                         H
C     ENDIF                                                             H
C
C     Get pertaint information
C     CLINE = ' '                                                       H
C     CALL WHEN (CLINE, CLINE(9:))                                      H
C     IUNIT = IFLTAB(KUNIT)                                             H
C     CALL GETNAM (IUNIT, CLINE(19:), ISTAT)                            H
C     CLINE(38:) = CSUB                                                 H
C     WRITE (CLINE(45:),'(I4,I9,I10)') IERR, JERR, IADD                 H
C
C     Get the name of the Program
C     CALL PCALOC ('P/NAME ',IPLOC)                                     H
C     DO 60 I=1,4                                                       H
C60   CALL GETPCA (IPLOC+I-1, IB(I))                                    H
C     CALL TATOA (IB(3),IA,8)                                           H
C     CPRO(1:9)=CA(1:8)//'*'                                            H
C     CALL TATOA(IB,IA,8)                                               H
C     CPRO(10:17)=CA(1:8)                                               H
C
C     CLINE(70:) = CPRO                                                 H
C     CLINE(90:) = CCOMET                                               H
C
C     DO A FAST OPEN (IE NO DISK ACCESS TO UPDATE LW DATE)
C
C     CALL FOPEN (143,ISTAT)                                            H
C     CALL WIND (143)                                                   H
C     CALL GIOPLW (143,'02,ILINE,44,ISTAT)                              H
C     CALL GIOPSW (143, '14, ISTAT)                                     H
C
C
C100  CONTINUE                                                          H
      CALL ABORT
C
      END
