      SUBROUTINE ZCO6T5 (IFTAB6, IFTAB5, BUFF1, KBUFF1, BUFF2, KBUFF2)
C
C     Copy a version 6 file to version 5.
C
C     Written by Bill Charley at HEC, 1989.
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
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
C     INTEGER*6 IFTAB6(*), IADD, NADD                                   H
      INTEGER*4 IFTAB6(*), IADD, NADD                                   ML
C     INTEGER IFTAB6(*)                                                 u
      INTEGER IFTAB5(*)
      REAL BUFF1(KBUFF1), BUFF2(KBUFF2)
      CHARACTER CPATH*85
      CHARACTER*10 CCPROG, CCDATE, CCTIME
      LOGICAL LFOUND
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFTAB6(KUNIT)                  D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZCO6T5;  Unit:',I4)          D
C
C     Read the permanent section of the file
      IADD = 1
      CALL ZGTREC (IFTAB6, IFTAB6(KPERM), NPERM, IADD, .FALSE.)
C
C     Get the first pathname bin address
      IF (IFTAB6(KTABLE).EQ.1) THEN
      NBIN = IFTAB6(KBNBLK)
      ELSE IF (IFTAB6(KTABLE).EQ.2) THEN
      NBIN = IFTAB6(KHASH)
      ELSE
C     We should never get here
      GO TO 900
      ENDIF
C
C     Get the address of the first bin
      IADD = IFTAB6(KAFBIN)
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
      LSBUFF(1) = .FALSE.
      NBNSIZ = IFTAB6(KBNSIZ)
      CALL ZGTREC (IFTAB6, IPNBIN, NBNSIZ, IADD, .FALSE.)
C     This next line keeps that area in memory while we read elsewhere
      LSBUFF(1) = .TRUE.
      JPNBIN = 1
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      NBMWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCMW) + 1
C
C     Record status good? (not deleted or renamed?)
      IF (IPNBIN(JPNBIN).EQ.1) THEN
C
C     Get pathname and pathname length
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NBMWPA)
C
      IDTYPE = IPNBIN(JPNBIN+KBTYPE+NBPWPA)
      NADD = IPNBIN(JPNBIN+NBPWPA+KBAINF)
      CALL ZGTREC (IFTAB6, INFO, NINFO+NBPWPA, NADD, .FALSE.)
C
      IRVERS = INFO(NBPWPA+KIVER)
      CALL ZSET5 ('RVERS', ' ', IRVERS)
      CALL HOLCHR (INFO(NBPWPA+KIPROG), 1, NPROGC, CCPROG, 1)
      CALL ZSET5 ('PROG', CCPROG, I)
      CALL HOLCHR (INFO(NBPWPA+KIDATE), 1, NDATEC, CCDATE, 1)
      CALL ZSET5 ('RDATE', CCDATE, I)
      CALL HOLCHR (INFO(NBPWPA+KITIME), 1, NTIMEC, CCTIME, 1)
      CALL ZSET5 ('RTIME', CCTIME, I)
C
C     Now copy the record
      CALL ZWR6T5 (IFTAB6, IFTAB5, CPATH, NPATH, IDTYPE,
     * BUFF1, KBUFF1, BUFF2, KBUFF2, LFOUND)
C
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
      JPNBIN = JPNBIN + NBPWPA + NLBIN
C     Is that pointer too large?
      IF (JPNBIN.GT.(IFTAB6(KBNSIZ)-2)) GO TO 200
C     Go back and look for next pathnme within this bin
      GO TO 100
C
C
 200  CONTINUE
C     At this point, there are no more pathnames in the current bin
C     (or it is full and extends into another block).
C     Read the next pathname bin
      NBIN = NBIN - 1
C     Any more bins in this block?
      IF (NBIN.LE.0) THEN
C     No - Get pointer to next bin block (section).
C     Get location of pointers in bin
      I = IFTAB6(KBNSIZ)
C     Any more bins in the DSS file? (Exit to 300 if no more).
      IF (IPNBIN(I).EQ.0) GO TO 300
      IADD = IPNBIN(I)
      NBIN = IFTAB6(KBNBLK)
      ELSE
C     More bins available within this block - get next one.
      IADD = IADD + IFTAB6(KBNSIZ)
      ENDIF
C
C     Go back up and read next bin
      GO TO 40
C
C
C     No more bins or pathnames left.  All done
 300  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,320)                               D
 320  FORMAT (T6,'-----DSS---Debug:  Exit  ZCO6T5')                     D
      RETURN
C
C
 900  CONTINUE
      CALL ZERROR (IFTAB6, 100, 'ZCO6T5', 0, IFTAB6(KTABLE), ' ', 0,
     * ' ', 0)
C
      END
