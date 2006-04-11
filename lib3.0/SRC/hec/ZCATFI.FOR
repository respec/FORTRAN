      SUBROUTINE ZCATFI (IFLTAB, LEXTND, LSELCA, LCDCAT, LTONLY, LERR)
C
C     Generate a catalog from a DSS file.
C     This may either be an extended version or a short one.
C     The extended one includes the last written date and time,
C     the record version number, and the program that
C     last wrote that record.
C
C     The extended version takes substantially longer than the short.
C
C     Written by Bill Charley at HEC, 1988.
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
C
CADD C.ZDSSCM                                                           H
      INCLUDE 'zdsscm.h'                                                MLu
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
C     INTEGER*6 IFLTAB(*), IADD, NADD                                   H
      INTEGER*4 IFLTAB(*), IADD, NADD, JNPATH, NRECS                    ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*85, CCPROG*10, CCDATE*10, CCTIME*10, CCTAG*10
      CHARACTER CDTYPE*3
      LOGICAL LMATCH, LEXTND, LSELCA, LCDCAT, LTONLY, LERR
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)                  D
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZCATFI;  Unit:',I4)          D
      LTWCAT = .FALSE.
C
C     Read the permanent section of the file and lock the file
      CALL ZMULTU (IFLTAB, .TRUE.)
      NRECS = IFLTAB(KNRECS)
      CCTAG = ' '
      JNPATH = 0
C
C     Set up the tag-hash code block
      CALL ZTAGFI (IFLTAB, -1, NRECS, IZERO, IZERO)
C
C     Unlock the file
      CALL ZMULTU (IFLTAB, .FALSE.)
C
C     Get the first pathname bin address
      IF (IFLTAB(KTABLE).EQ.1) THEN
      NBIN = IFLTAB(KBNBLK)
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
      NBIN = IFLTAB(KHASH)
      ELSE
C     We should never get here
      GO TO 900
      ENDIF
C
C     Print any catalog status message
C     IF (LCATST) CALL CHRWT (MUNIT, CHAR(13)//CHAR(10)//'  0% Complete'HM
C    * // ',      0 Records'// CHAR(13), 32)                            HM
C     IF (LCATST) CALL CHRWT (MUNIT, '  0% Complete'                    u
C    * // ',      0 Records'// CHAR(13), 30)                            u
C
C
C     Get the address of the first bin
      IADD = IFLTAB(KAFBIN)
      JJBUFF = 1
      JJREC = -2
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
C     Release the previous record
      IF (JCREC(JJBUFF).EQ.JJREC) LSBUFF(JJBUFF) = .FALSE.
      NBNSIZ = IFLTAB(KBNSIZ)
      CALL ZGTREC (IFLTAB, IPNBIN, NBNSIZ, IADD, .TRUE.)
C     This next lines keeps that area in memory while we read elsewhere
      JJBUFF = JBUFF
      JJREC = JCREC(JBUFF)
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
      JNPATH = JNPATH + 1
C
C     Yes.  Save this tag in the tag-hash code block
      IF (JNPATH.LE.NRECS) THEN
      IF (JNPATH.LT.NRECS) THEN
      IFUN = 0
      ELSE
      IFUN = 1
      ENDIF
      CALL ZTAGFI (IFLTAB, IFUN, NRECS, IPNBIN(JPNBIN+KBTAG+NBPWPA),
     * IPNBIN(JPNBIN+KBHASH+NBPWPA))
      ENDIF
C
C     Should the status (% complete) be printed
      IF (LCATST) THEN
      N = MOD (JNPATH,10)
      IF (N.EQ.0) THEN
      IP = ((JNPATH-1)*100) / NRECS
      WRITE (CPATH(1:29), 25) IP, JNPATH
 25   FORMAT (I3,'% Complete;',I7,' Records')
      CALL CHRWT (MUNIT, CPATH(1:29)//CHAR(13), 30)
      ENDIF
      ENDIF
C
C
C
C     If this is a tag-hash code only call, bypass writing pathname
      IF (.NOT.LTONLY) THEN
C
C     Is a short or extended version asked for
      IF (.NOT.LEXTND) THEN
C
C     Get pathname and pathname length
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NBMWPA)
C
C     Pick up the number of data and header words,
C     the data type, and the catalog tag identifier.
      NHEAD = IPNBIN(JPNBIN+KBNHEA+NBPWPA)
      NDATA = IPNBIN(JPNBIN+KBNDAT+NBPWPA)
      IDTYPE = IPNBIN(JPNBIN+KBTYPE+NBPWPA)
      CALL HOLCHR (IPNBIN(JPNBIN+KBTAG+NBPWPA), 1, NTAGC, CCTAG, 1)
C
      CALL ZSELCA (CPATH, NPATH, CCTAG(1:NTAGC), CCPROG, CCDATE, CCTIME,
     * CDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH,
     * LCDCAT, LERR)
      IF (LERR) GO TO 800
C
      ELSE
C
C     Extended Catalog
C
C     Get Information Block
      NADD = IPNBIN(JPNBIN+NBPWPA+KBAINF)
      CALL ZGTREC (IFLTAB, INFO, NINFO+NBPWPA, NADD, .FALSE.)
C
C     Extract information from this array
      IRSTAT = INFO(KISTAT)
      IF (IRSTAT.GT.1) GO TO 180
      IF (INFO(KIFLAG).NE.NPFLAG) GO TO 180
C     Get pathname and pathname length
      NPATH = INFO(KINPAT)
      CALL HOL2CH (INFO(KIPATH), CPATH, NBMWPA)
C
      NHEAD = INFO(NBPWPA+KINUHE)
      NDATA = INFO(NBPWPA+KILNDA)
      IDTYPE = INFO(NBPWPA+KITYPE)
      IRVERS = INFO(NBPWPA+KIVER)
      CALL HOLCHR (INFO(NBPWPA+KIPROG), 1, NPROGC, CCPROG, 1)
      CALL HOLCHR (INFO(NBPWPA+KIDATE), 1, NDATEC, CCDATE, 1)
      CALL HOLCHR (INFO(NBPWPA+KITIME), 1, NTIMEC, CCTIME, 1)
      CALL HOLCHR (INFO(NBPWPA+KITAG), 1, NTAGC, CCTAG, 1)
C
      IF (IDTYPE.EQ.100) THEN
      CDTYPE = 'RTS'
      ELSE IF (IDTYPE.EQ.110) THEN
      CDTYPE = 'ITS'
      ELSE IF (IDTYPE.EQ.200) THEN
      CDTYPE = 'PD '
      ELSE IF (IDTYPE.EQ.300) THEN
      CDTYPE = 'TXT'
      ELSE
      CDTYPE = 'UND'
      ENDIF
      CALL ZSELCA (CPATH, NPATH, CCTAG(1:NTAGC), CCPROG, CCDATE, CCTIME,
     * CDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH,
     * LCDCAT, LERR)
      IF (LERR) GO TO 800
C
      ENDIF
      ENDIF
C
C     Have we reached the number of records in the file?
      IF (JNPATH.GE.NRECS) GO TO 800
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
 180  CONTINUE
      JPNBIN = JPNBIN + NBPWPA + NLBIN
C     Is that pointer too large?
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
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
      I = IFLTAB(KBNSIZ)
C     Any more bins in the DSS file? (Exit to 800 if no more).
      IF (IPNBIN(I).EQ.0) GO TO 800
      IADD = IPNBIN(I)
      NBIN = IFLTAB(KBNBLK)
      ELSE
C     More bins available within this block - get next one.
      IADD = IADD + IFLTAB(KBNSIZ)
      ENDIF
C
C     Go back up and read next bin
      GO TO 40
C
C
C     No more bins or pathnames left.  All done
 800  CONTINUE
      IF (LCATST) THEN
      WRITE (CPATH(1:29), 25) 100, JNPATH
      CALL CHRWT (MUNIT, CPATH(1:29)//CHAR(13), 30)
C     CALL CHRWT (MUNIT, CHAR(10), 1)                                   Mu
      LCATST = .FALSE.
      ENDIF
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)                               D
 820  FORMAT (T6,'-----DSS---Debug:  Exit  ZCATFI')                     D
      RETURN
C
C
 900  CONTINUE
      CALL ZERROR (IFLTAB, 100, 'ZCATFI', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
C
      END
