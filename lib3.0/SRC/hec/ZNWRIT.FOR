      SUBROUTINE ZNWRIT (IFLTAB, CPATH, NPATH, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
C
C     Main routine for writing a new record to a DSS file
C     Not User callable (use ZWRITX)
C
C     Written by Bill Charley, HEC, June 1989
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSCZ                                                           H
      INCLUDE 'zdsscz.h'                                                MLu
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
C     INTEGER*6 IFLTAB(*), NADD                                         H
      INTEGER*4 IFLTAB(*), NADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      CHARACTER CTPATH*90, CPATH*(*)
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), CPATH(1:NPATH), D
     * NIHEAD, NCHEAD, NUHEAD, NDATA                                    D
 20   FORMAT (/,T10,'-----DSS---Debug: Enter ZNWRIT,  Unit:',I4,/,      D
     * /,T12,'Pathname: ',A,/,T12,'NIHEAD:',I5,', NCHEAD:',I5,          D
     * ', NUHEAD:',I5,', NDATA:',I5)                                    D
C
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CTPATH = CPATH
C
C
C     Clear Last Pathname not found in ZCHECK
      IFLTAB(KLPATL) = -1
C     Update effiency information variables
      IF (IPLOOP.EQ.0) THEN
      IFLTAB(KHUSED) = IFLTAB(KHUSED) + 1
      IF (IFLTAB(KTABLE).EQ.2) IFLTAB(KBINS) = IFLTAB(KBINS) + 1
      ENDIF
      IF (IPLOOP.GT.IFLTAB(KMAXPH)) THEN
      IFLTAB(KMAXPH) = IPLOOP
      IFLTAB(KMAXHC) = IHASH
      ENDIF
C
C     Get maximum part lengths for file
      IF (CPATH(1:1).EQ.'/') THEN
      CALL ZUPATH (CPATH(1:NPATH), IBPART, IEPART, ILPART, J)
      IF (J.EQ.0) THEN
      DO 40 I=1,6
      CALL GETHOL (IFLTAB(KMXPRT), I, IMXPRT)
      IF (ILPART(I).GT.IMXPRT) THEN
      IMXPRT = ILPART(I)
      CALL PUTHOL (IFLTAB(KMXPRT), I, IMXPRT)
      ENDIF
 40   CONTINUE
      ENDIF
      ENDIF
C
C
C     Write new pointers
C
C     If this pathname's hash code was not in the hash table
C     create a new bin, and save that bin's address in the table
      IF (.NOT.LINTAB) THEN
C
C     Write a new pathname bin
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Write new path bin'      D
      CALL ZNWBIN (IFLTAB)
C
C     Write address of new pathname bin to hash location in table
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Save bin add in index'   D
      NADD = NPERM + IHASH
      CALL ZPTREC (IFLTAB, IPBADD, 1, NADD, .TRUE.)
C
      ELSE
C
C     A hash code already there (or file structure 2).  Make
C     sure that there is space in the bin for this pathname.
      I = JPNBIN + NPPWRD + NLBIN + 2
      IF (I.GT.IFLTAB(KBNSIZ)) THEN
C     No more space in this bin - create a new bin
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Add another path bin'    D
      IPNBIN(JPNBIN) = -1
      CALL ZNWBIN (IFLTAB)
      ENDIF
      ENDIF
C
C     Save information about this record in the pathname bin
      IPNBIN(JPNBIN+KBSTAT) = 1
      IPNBIN(JPNBIN+KBNPAT) = NPATH
      CALL CH2HOL (CTPATH, IPNBIN(JPNBIN+KBPATH), NPMWRD)
C     Point to new data area, which will start at end of file
      IPNBIN(JPNBIN+NPPWRD+KBAINF) = IFLTAB(KFSIZE)
      IPNBIN(JPNBIN+NPPWRD+KBNHEA) = NUHEAD
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NDATA
      IF (NLDATA.GT.0) THEN
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NLDATA
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NDATA
      ENDIF
      IPNBIN(JPNBIN+NPPWRD+KBHASH) = IHASH
C
      IPNBIN(JPNBIN+NPPWRD+KBTAG+1) = 0
      IF (IRENAM.EQ.0) THEN
      IFLTAB(KNRECS) = IFLTAB(KNRECS) + 1
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = ITYPE
C     Generate a tag
      IF (ICHAR(CTAG(1:1)).LT.32) CTAG = ' '
      IF (CTAG(1:1).EQ.' ') CALL ZGETAG (IFLTAB, CPATH(1:NPATH), CTAG)
C     Be sure tag has no non-printable characters in it!
      DO 60 I=1, NTAGC
      IF (ICHAR(CTAG(I:I)).LT.32) CTAG(I:I) = ' '
 60   CONTINUE
      CALL CHRHOL (CTAG, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = IRNTYP
      CALL CHRHOL (CRNTAG, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      ENDIF
C
C     Now store this bin with the new information
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Store path bin'          D
      I = INT(IFLTAB(KBNSIZ))
      CALL ZPTREC (IFLTAB, IPNBIN, I, IPBADD, .TRUE.)
C
C     Now create the information block
C
C     Update the file size to include this new block
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NINFO + NPPWRD
C     Store the pathname
      INFO(KIFLAG) = NPFLAG
      INFO(KISTAT) = 1
      INFO(KINPAT) = NPATH
      CALL CH2HOL (CTPATH, INFO(KIPATH), NPMWRD)
C
      IF (IRENAM.EQ.0) THEN
C     Store the header array location and length
      INFO(NPPWRD+KIAIHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINIHE) = NIHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NIHEAD
      INFO(NPPWRD+KIACHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINCHE) = NCHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NCHEAD
      INFO(NPPWRD+KIAUHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINUHE) = NUHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NUHEAD
C     Store the data array location and length
      INFO(NPPWRD+KIADAT) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINDAT) = NDATA
      IF (NLDATA.GT.0) THEN
      INFO(NPPWRD+KILNDA) = NLDATA
      ELSE
      INFO(NPPWRD+KILNDA) = NDATA
      ENDIF
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NDATA
C     Store the data type, version number, date, time, and tag
      INFO(NPPWRD+KITYPE) = ITYPE
C     Compression flag (set to 0 for no compression)
      INFO(NPPWRD+KICOMP) = ICOMP
C     Alternative flag, usually used for data quality
      INFO(NPPWRD+KIQUAL) = IQUAL
      INFO(NPPWRD+KIVER) = IBVER
      CALL CHRHOL (CPROG, 1, NPROGC, INFO(NPPWRD+KIPROG), 1)
      CALL CHRHOL (CDATE, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
      INFO(NPPWRD+KITAG+NTAG-1) = 0
      CALL CHRHOL (CTAG, 1, NTAGC, INFO(NPPWRD+KITAG), 1)
      DO 80 I=1,NPASS
      INFO(NPPWRD+KIPASS+I-1) = 0
 80   CONTINUE
      INFO(NPPWRD+KIPREC) = IPREC
      INFO(NPPWRD+KIRES) = 0
C
      ENDIF
C
C     Store the information block
      ISIZE = NPPWRD + NINFO
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Store info block'        D
      CALL ZPTREC (IFLTAB, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .TRUE.)
C
      IF (IFLTAB(KFILEW).EQ.1) THEN
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KLWDAT), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, IFLTAB(KLWTIM), 1)
      IFLTAB(KFILEW) = 0
      ENDIF
C
      NADD = 1
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Update root'             D
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
 800  CONTINUE
C     Reset flags
      NLDATA = -1
      ITYPE = 0
      ICOMP = 0
      IQUAL = 0
      CTAG = ' '
      IRENAM = 0
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)                               D
 820  FORMAT (T8,'-----DSS---Debug: EXIT ZNWRIT')                       D
      RETURN
C
      END
