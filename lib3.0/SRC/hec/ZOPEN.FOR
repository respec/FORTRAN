      SUBROUTINE ZOPEN ( IFLTAB, CFNAME, ISTAT)                         MLu
C     SUBROUTINE ZOPEN6 ( IFLTAB, CFNAME, ISTAT)                        H
C
C
C     Open DSS file called CFNAME
C     The file will be generated if not existing
C     Initalizes DSS software, and sets up DSS file
C     structure in CFNAME
C
C     DSS File Structure Level 6:  Written by Bill Charley and
C                                  Art Pabst, HEC 1988
C
C
C     INTEGER*6 IFLTAB(*), NADD, IFSIZE, ISIZE                          H
      INTEGER*4 IFLTAB(*), NADD, IFSIZE, ISIZE                          ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 IREC, JREC                                              ML
C
      CHARACTER CFNAME*(*), CTEMP*20
      CHARACTER CNAME*64
C     CHARACTER CPRGN*17   !!!TEMP FOR HARRIS tracking                  H
C
      LOGICAL LNEW, LEXIST, LBTEST
      LOGICAL LOP                                                       HL
C     SAVE LFIRST
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
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSBZ                                                           H
      INCLUDE 'zdssbz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
      DATA IHANDL /0/
C
C
C
      IF (MLEVEL.GE.10) WRITE ( MUNIT, 10) CFNAME                       D
 10   FORMAT (/,T3,'-----DSS---Debug:  Enter ZOPEN',/,                  D
     * T8,'File Name Provided: ',A)                                     D
C
C     If this is the first access to a DSS subroutine, initialize
      IF (LFIRST) CALL ZINIT
      LFIRST = .FALSE.
C
C     Add any file name extension and determine if the file exists.
      CALL ZFNAME (CFNAME, CNAME, ILEN, LEXIST)
C
      IF (ILEN.EQ.0) THEN
      WRITE (MUNIT,20)
 20   FORMAT (/,T3,'-----DSS*** ZOPEN ERROR:  No File Name Specified!'/)
      ISTAT = -10
      GO TO 910
      ENDIF
C
      ISTAT = 0
      IFLTAB(KREADO) = 0
      LNEW = .NOT. LEXIST
C
      IF (MLEVEL.GE.10) WRITE ( MUNIT, 40) LEXIST, CNAME(1:ILEN)        D
 40   FORMAT (/,T3,'-----DSS---Debug:  ZOPEN;  Exist: ',L1,             D
     * ';  File: ',A)                                                   D
C
C
      CALL WHEN (CDATE,CTIME)
C
C     Check if the unit is defined by a ZSET call
 50   CONTINUE
      IF (LDUNIT) THEN
      IUNIT = IDUNIT
      LDUNIT = .FALSE.
      ELSE
      NUNIT = NUNIT + 1
      IF (NUNIT.GT.200) NUNIT=200
      IUNIT = NUNIT
      ENDIF
C
C     See if this unit number is already being used
C     (where FORTRAN I/O is used only).
      INQUIRE (UNIT=IUNIT, OPENED=LOP)                                  HL
      IF (LOP) THEN                                                     HL
      IF (IUNIT.LT.200) GO TO 50                                        HL
      ENDIF                                                             HL
C
      IF ((IUNIT.LE.0).OR.(IUNIT.GT.200)) THEN
      WRITE ( MUNIT, 60) IUNIT
 60   FORMAT (//,' ***** ILLEGAL UNIT NUMBER FOR DSS FILE:',I9)
      CALL ZABORT (IFLTAB, 20, 'ZOPEN ', IUNIT, IFLTAB(1),
     * 'Illegal Unit')
      ENDIF
C
C
C     On the Harris, make a shared file assignment
C
C     CALL ZASSIG ( IUNIT, CNAME(1:ILEN), IMULT, LNEW)                  H
C
C     IF (IMULT.LT.0) THEN                                              H
C     IF (MLEVEL.GE.1) WRITE ( MUNIT, 80) CNAME                         H
C80   FORMAT (' -----DSS*** ZOPEN:  Error - Unable to Open File: ',A/)  H
C     ISTAT = -1                                                        H
C     GO TO 800                                                         H
C     ENDIF                                                             H
C
C     Set this file type for multiple user access
C     IF IFLTAB(KMULT) = 1, MULTIPLE USER ACCESS
C     IF IFLTAB(KMULT) = 2, NO MULTIPLE USER ACCESS
C
      IF (LEXCL) THEN
      IFLTAB(KMULT) = 2
      IFLTAB(KEXCL) = 1
      ELSE
      IFLTAB(KMULT) = 1
      IFLTAB(KEXCL) = 0
      ENDIF
      IFLTAB(KMULT) = 2                                                 ML
C
C     For Lahey, be sure the file ends in an even record length!
      IF (LEXIST) CALL ZLAHEY (CNAME, IUNIT)                            L
C
C     Open the file
C     CALL GIOPSW (IUNIT, '13, JSTAT)                                   H
      OPEN (UNIT=IUNIT,ACCESS='DIRECT',RECL=NRECL,FILE=CNAME,           PL
     * IOSTAT=ISTAT)                                                    PL
C
C     If new create it
C     IMODE = 66                                                        M
C     IF (LNEW) THEN                                                    M
C     CALL CREAF (CNAME(1:ILEN)//CHAR(0), 0, IHANDL, ISTAT)             M
C     CALL CLOSF (IHANDL, ISTAT)                                        M
C     ELSE                                                              M
C     See if the file is in a read only mode or not
C     CALL FILEN (CNAME(1:ILEN)//CHAR(0), 0, 'F', CTEMP, IFSIZE,        M
C    * CTEMP, CTEMP, IATT, ISTAT)                                       M
C     IF (LBTEST(IATT,0)) THEN                                          M
C     IMODE = 64                                                        M
C     IFLTAB(KREADO) = 1                                                M
C     ENDIF                                                             M
C     ENDIF                                                             M
C
C     Open in DOS with Deny None and read/write for Network
C     CALL OPENF ( CNAME(1:ILEN)//CHAR(0), IMODE, IHANDL, ISTAT)        M
C
C     Check that the file can be locked (locus=2)
C     CALL EXTERR (IEX, ICL, IAC, ILOCUS)                               M
C     IF (ILOCUS.EQ.2) IFLTAB(KMULT) = 1                                M
C     IF (MLEVEL.GE.10) WRITE (MUNIT,101) ILOCUS, IFLTAB(KMULT)         MD
C101  FORMAT (T7,'Locus:',I5,',  Multiple user access:',I4)             MD
C
C     CNAME(ILEN+1:ILEN+1) = CHAR(0)                                    u
C     CALL OPENF ( CNAME(1:ILEN+1), 10, IHANDL, ISTAT)                  u
C     IF (ISTAT.NE.0) THEN                                              u
C     CALL OPENF ( CNAME(1:ILEN+1), 0, IHANDL, ISTAT)                   u
C     IF (ISTAT.EQ.0) IFLTAB(KREADO) = 1                                u
C     ENDIF                                                             u
C     CNAME(ILEN+1:ILEN+1) = ' '                                        u
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 100) ISTAT, IUNIT, LNEW, NRECL,   D
     * IFLTAB(KMULT), IFLTAB(KEXCL), CNAME(1:ILEN)                      D
 100  FORMAT (T7,'Open status:',I5,',  Unit:',I4,',  New:',L2,          D
     * ',  Record Length:',I5,' Bytes',/,T7,'Multiple User Access:',I4, D
     * ',  Exclusive Access:',I4,/,T7,'Name: ',A)                       D
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, *)' Handle: ',IHANDL              D
C
      IF (ISTAT.NE.0) THEN                                              MLu
C     On the Harris, test bit 17 to see if we opened ok
C     IF (LBTEST(JSTAT,17)) THEN                                        H
C     If an error occured during giop open, try re-opening with
C     a FROTRAN open.  This should give the user an understandable
C     I/O status.
C     OPEN (UNIT=IUNIT,ACCESS='DIRECT',RECL=NRECL,IOSTAT=ISTAT)         H
      IF (ISTAT.EQ.0) GO TO 140
      WRITE ( MUNIT, 120) CNAME, ISTAT
 120  FORMAT (/,' -----DSS*** ZOPEN:  Error - Unable to Open File: ',A,/
     * '    Open Status:',I8,/)
      GO TO 800
      ENDIF
C
 140  CONTINUE
      CALL GETNAM ( IUNIT, CNAME, IERR)                                 HL
      CALL CHRLNB (CNAME, ILEN)                                         HL
C
C     Determine if we have write access to the file
      IF (LREADO) IFLTAB(KREADO) = 1
C     CALL LFINFO (IUNIT, IFCB, IFLAGS, IASTAT, IST)                    H
C     IF (.NOT.LBTEST(IASTAT,0)) IFLTAB(KREADO) = 1                     H
C
C
C     Save File structure verion in file table
      IFLTAB(KNV) = 6
      IFLTAB(KUNIT) = IUNIT
C     IFLTAB(KHANDL) = IHANDL                                           Mu
      IFLTAB(KHANDL) = IUNIT                                            HL
      NFILES = NFILES + 1
      IFLTAB(KFILES) = NFILES
C     CALL CH2HOL (CNAME, IFLTAB(KNAME), 16)                            Mlgsm
C     CALL CH2HOL (CNAME, IFLTAB(KNAME), 8)                             c
      IFLTAB(KFILEW) = 1
C
C     Set KEY elements in IFLTAB (to insure that it
C     does not become corrupt)
C
      IFLTAB(KEY1) = NKEY
      IFLTAB(KEY2) = NKEY
      IFLTAB(KEY3) = NKEY
C
C
C     Initialize buffer parameters
C
      IFLTAB(KLOCK) = 0
      IFLTAB(KWLOCK) = 0
      CALL ZBDUMP (IFLTAB, 1)
C
C
C     Get permanent section of file (if it exists)
C
      IF (.NOT.LNEW) THEN
      NADD = 1
      CALL ZGTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
C
C     Was this file not closed with write lock on last time?
      IF (IFLTAB(KFSIZE).EQ.-1357) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 150) CNAME(1:ILEN)
 150  FORMAT (/,' *****DSS***ZOPEN:  File was not closed during a',
     * ' Locked Access',/,' The file size is unknown;  Name: ',A,/,
     * ' --- FILE ACCESS IS READ ONLY ---',/,
     * ' Squeeze the file with DSSUTL to correct this condition.')
C     Set in a read only state (so we can at least squeeze the file)
      IFLTAB(KREADO) = 1
C     Get the best estimate of the file size.
C  !!!!!!!!FIX ME !!!!!!!!!!
      IFSIZE = 0
C     CALL FILEN (CNAME(1:ILEN)//CHAR(0), 0, 'F', CTEMP, IFSIZE,        M
C    * CTEMP, CTEMP, IDUM, ISTAT)                                       M
C     IFSIZE = ((IFSIZE - 1)/ 4) + 1                                    M
C     CALL GIOPLW ( IUNIT, '23, I, '37777777, IS)                       H
C     CALL GETE (I)                                                     H
C     IFSIZE = I                                                        H
C     IFSIZE = IFSIZE * 56                                              H
      IFSIZE = IFSIZE + 1                                               MLu
      IF (IFSIZE.LT.100) IFSIZE = 9999999
      IFLTAB(KFSIZE) = IFSIZE
      ENDIF
C
C     If file has zero size, last open not complete (or not DSS file)
      IF (IFLTAB(KFSIZE).LE.0) LNEW = .TRUE.
      ENDIF
C
C     Check if this is a DSS file
      CALL HOLCHR (IFLTAB(KDSS), 1, NDSSC, CTEMP, 1)
      IF (MLEVEL.GE.11) WRITE (MUNIT, 155) CTEMP(1:NDSSC), CDSS(1:NDSSC)
 155  FORMAT (T11,'File Identifier Read -',A,'-  Current identifier -',
     * A,'-')
      IF (LNEW) CTEMP = ' '
      IF (CTEMP(1:NDSSC).EQ.CDSS(1:NDSSC)) THEN
C     Yes it is a DSS file -
C     see if it is the current software version
      CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
      IF(MLEVEL.GE.11)WRITE(MUNIT,156)CTEMP(1:NVERSC), CVERS(1:NVERSC)
 156  FORMAT (T11,'File Version Read -',A,'-  Current Version -',A,'-')
C
      IF (CTEMP(1:2).EQ.CVERS(1:2)) THEN
C     Yes it is
      IF (MLEVEL.GE.1) WRITE (MUNIT,160) CNAME(1:ILEN), IUNIT,
     * CTEMP(1:NVERSC)
 160  FORMAT (T5,'-----DSS---ZOPEN:  Existing File Opened,  File: ',A,/,
     * T24,'Unit:',I4,';  DSS Version: ',A)
      IF ((IFLTAB(KREADO).EQ.1).AND.(MLEVEL.GE.2)) WRITE (MUNIT, 180)
 180  FORMAT (T24, '(Read Access Only)')
      IF ((MLEVEL.GE.2).AND.((LSTABL).OR.(LSZSET))) WRITE (MUNIT, 190)
 190  FORMAT (T5,'-----DSS--- Parameters Set for Existing File Ignored')
C
      NADD = IFLTAB(KFSIZE) - 1
      CALL ZGETRW (NADD, IREC, IWRD)
      IFLTAB(KMXREC) = IREC
C
      ELSE
      WRITE ( MUNIT, 200) CTEMP(1:NVERSC), CVERS(1:NVERSC)
      ISTAT = -3
      GO TO 900
      ENDIF
C
      ELSE
C
C     Check for older software (incompatiable) versions
      IF (.NOT.LNEW) THEN
      CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
      IF (CTEMP(2:2).EQ.'-') THEN
      WRITE ( MUNIT, 200) CTEMP(1:NVERSC), CVERS(1:NVERSC)
 200  FORMAT (//,' ***** DSS: Incompatiable DSS Software Versions *****'
     * ,/,' The File Specified is Version ',A,/
     * ' This Software is Version ',A,/
     * ' These Versions are Incompatiable - Please Update',
     * ' Your DSS File, or this Program',//)
      ISTAT = -3
      GO TO 900
      ENDIF
      ENDIF
C
C
C     New file - Be sure we are not read only
      IF (LREADO) THEN
      WRITE (MUNIT, 210) CNAME(1:ILEN)
 210  FORMAT (/,' ----DSS---ZOPEN;  ERROR:  Read Access Only Given',
     * ' for New File',/,' Unable to open file, Name: ',A)
      ISTAT = -2
      GO TO 900
      ENDIF
C
C     Create permanent section
C
      K = KPERM + NPERM - 1
      DO 220 I=KPERM,K
      IFLTAB(I) = 0
 220  CONTINUE
C
C     Set 'ZDSS' at the first location in the file
      CALL CHRHOL (CDSS, 1, NDSSC, IFLTAB(KDSS), 1)
      IFLTAB(KNRECS) = 0
      IFLTAB(KSEQNO) = 0
      IFLTAB(KDEAD) = 0
      IFLTAB(KMXREC) = 0
      CALL CHRHOL (CVERS, 1, NVERSC, IFLTAB(KVERS), 1)
C
C     If the file is new because it is being squeezed, keep
C     the old file date.
      IF (LFDATE) THEN
      CALL CHRHOL (CFDATE, 1, NDATEC, IFLTAB(KCREAT), 1)
      LFDATE = .FALSE.
      ELSE
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KCREAT), 1)
      ENDIF
C
C     Save the last written date and time in the perm section
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KLWDAT), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, IFLTAB(KLWTIM), 1)
C
C     Set the file size, and structure (via ZSET or default)
      CALL ZFSIZE (IFLTAB)
C
C     If this structre uses a hash table, set aside space for it
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IFLTAB(KBNREM) = IFLTAB(KBNBLK)
      ISIZE = INT(IFLTAB(KHASH)) + NPERM
      IFLTAB(KAFBIN) = 1
      IFLTAB(KAFBIN) = IFLTAB(KAFBIN) + ISIZE
C     Set aside space for the first set of pathname bins
C     Keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
C     E.g., starting at words 1 or 225 or 449 or 673 on the Harris,
C     word 1 on the PC, or where the bin size equals the NBSIZE
      CALL ZGETRW (IFLTAB(KAFBIN), IREC, IWRD)
C     I = (IWRD-2) / INT(IFLTAB(KBNSIZ))                                Hc
C     JWRD = ((I + 1) * INT(IFLTAB(KBNSIZ))) + 1                        Hc
C     ISIZE = JWRD - IWRD                                               Hc
C     IFLTAB(KAFBIN) = IFLTAB(KAFBIN) + ISIZE                           Hc
      IF (IWRD.NE.1) THEN                                               MLmlgs
      JREC = IREC + 1                                                   MLmlgs
      CALL ZGETAD (IFLTAB(KAFBIN), JREC, 1)                             MLmlgs
      ENDIF                                                             MLmlgs
      IFLTAB(KANBIN) = IFLTAB(KAFBIN)
      IFLTAB(KFSIZE) = IFLTAB(KAFBIN)
      ISIZE = IFLTAB(KBNBLK) * IFLTAB(KBNSIZ)
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
C
      ELSE
C
C     This structrue does not use a hash table.  The has points
C     directly to the first pathname bin for that has code.
C     Set aside space equal to the hash size time the bin size
      IFLTAB(KBNREM) = 0
      IFLTAB(KAFBIN) = NPERM + 1
C     Keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
C     E.g., starting at words 1 or 225 or 449 or 673 on the Harris,
C     word 1 on the PC, or where the bin size equals the NBSIZE
      CALL ZGETRW (IFLTAB(KAFBIN), IREC, IWRD)
C     I = (IWRD-2) / INT(IFLTAB(KBNSIZ))                                Hc
C     JWRD = ((I + 1) * INT(IFLTAB(KBNSIZ))) + 1                        Hc
C     ISIZE = JWRD - IWRD                                               Hc
C     IFLTAB(KAFBIN) = IFLTAB(KAFBIN) + ISIZE                           Hc
      IF (IWRD.NE.1) THEN                                               MLmlgs
      JREC = IREC + 1                                                   MLmlgs
      CALL ZGETAD (IFLTAB(KAFBIN), JREC, 1)                             MLmlgs
      ENDIF                                                             MLmlgs
      IFLTAB(KANBIN) = IFLTAB(KAFBIN)
      ISIZE = (IFLTAB(KHASH) - 1) * IFLTAB(KBNSIZ)
      IFLTAB(KANBIN) = IFLTAB(KANBIN) + ISIZE
      IFLTAB(KFSIZE) = IFLTAB(KAFBIN)
      ISIZE = IFLTAB(KHASH) * IFLTAB(KBNSIZ)
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
      ENDIF
C
C     Iniitalize efficiency variables
      IFLTAB(KBINS)  = 0
      IFLTAB(KHUSED) = 0
      IFLTAB(KBOVER) = 0
      IFLTAB(KMAXPH) = 0
C
      IFLTAB(KCOMPI) = 0
      IFLTAB(KCOMPN) = 0
C
      IFLTAB(KITSIN) = 50
      IFLTAB(KITSDA) = 100
      IFLTAB(KITSMO) = 100
      IFLTAB(KITSYE) = 100
      IFLTAB(KITSDE) = 100
      IFLTAB(KITSCE) = 100
C
C     Write permanent section to the file with zero file size
C     This prevents reads for ZPTREC, and protects the file in
C     case an abort occurs before the open is complete
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = 0
      IFLTAB(KBSADD) = 0
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C     Dump the buffer to disk
      N = JBUFF
      CALL ZBDUMP (IFLTAB, 1)
      JBUFF = N
      JCREC(JBUFF) = 1
      LSBUFF(JBUFF) = .TRUE.
      IFLTAB(KFSIZE) = IFSIZE
C
      IF (IFLTAB(KTABLE).EQ.1) THEN
C     Write a zerored Hash Table
      NADD = NADD + NPERM
      JSIZE = -(IFLTAB(KHASH))
      CALL ZPTREC (IFLTAB, IZERO,  JSIZE, NADD, .FALSE.)
C
C     Write the first pathname bin block
      ISIZE = IFLTAB(KBNBLK) * IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, ISIZE, -1, IFLTAB(KAFBIN), .FALSE.)
C
      ELSE
C     This is a structure 2 type file -
C     Write all pathanme bins to the file (takes a while!)
      ISIZE = IFLTAB(KHASH) * IFLTAB(KBNSIZ)
      CALL ZPTREC (IFLTAB, ISIZE, -1, IFLTAB(KAFBIN), .FALSE.)
      ENDIF
C
      IFLTAB(KFILEW) = 0
C
C     Re-write permanent section to the file with correct file size
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
C
C
      CALL ZBDUMP (IFLTAB, 1)
C
      IF (MLEVEL.GE.1) WRITE (MUNIT,240) CNAME(1:ILEN), IUNIT,
     * CVERS(1:NVERSC)
 240  FORMAT (T5,'-----DSS---ZOPEN:  New File Opened,  File: ',A,/,
     * T24,'Unit:',I4,';  DSS Version: ',A)
C
      ENDIF
C
 800  CONTINUE
C
C     If this file has been set in a write lock mode,
C     read in physical records 1 and 2, then lock them.
C
      IF (LWLOCK) THEN
C     Be sure that we have no more than two records locked at this time
C     (from other file(s))
      J = 0
      DO 820 I=1,MXBUFF
      IF (LOCKBF(I)) J = J + 1
 820  CONTINUE
C
      IF (J.LE.2) THEN
C     Write to the file a flag indicating Write Lock on,
C     in case there is an abort, we will know the file size is unknown!!
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = -1357
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
      CALL ZBDUMP (IFLTAB, 1)
      IFLTAB(KFSIZE) = IFSIZE
C     Now lock records 1 and 2
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      JREC = 2
      CALL ZGETAD (NADD, JREC, 1)
      CALL ZGTREC (IFLTAB, INFO, 1, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      IFLTAB(KWLOCK) = 1
      ENDIF
C
      ENDIF
C
C
      LSZSET = .FALSE.
      LSTABL = .FALSE.
      LEXCL = .FALSE.
      LWLOCK = .FALSE.
      LREADO = .FALSE.
      NSIZE = 0
      IHSIZE = 0
      IFLTAB(KCOMP) = 0
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CSIZE = ' '
C
C *********** Beta Test Information **************
C     CALL ASSIGX (147, 'SYS6*BETAINFO', I)
C     IF (I.NE.0) THEN
C     CALL WAITS (0.5)
C     CALL ASSIGX (147, 'SYS6*BETAINFO', I)
C     ENDIF
C     IF (I.EQ.0) THEN
C     OPEN (UNIT=147,IOSTAT=J)
C     IF (J.EQ.0) THEN
C     CALL WIND (147)
C     IF (CTEMP(1:1).EQ.'6') THEN
C     CTEMP = 'Old'
C     ELSE
C     CTEMP = 'New'
C     ENDIF
C     IF (IFLTAB(KEXCL).EQ.1)  CTEMP(6:6) = 'E'
C     IF (IFLTAB(KWLOCK).EQ.1) CTEMP(7:7) = 'W'
C     IF (IFLTAB(KREADO).EQ.1) CTEMP(8:8) = 'R'
C     CALL PRGNAM (CPRGN)
C     WRITE (147,840) CNAME(1:17), CTEMP(1:8), IFLTAB(KTABLE),
C    * IFLTAB(KHASH), CDATE, CTIME, CVERS(1:4), CPRGN
C840  FORMAT (A,1X,A,I4,I6,2X,A,1X,A,2X,A,2X,A)
C     ENDIF
C     CLOSE (UNIT=147)
C     ENDIF
C ************************************************
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 850) ISTAT                        D
 850  FORMAT (T3,'-----DSS---Debug; Exit ZOPEN;  Status:',I4)           D
C
      RETURN
C
  900 CONTINUE
C     Error occured - Close the file
      CLOSE (UNIT=IUNIT)                                                L
C     CALL CLOSF (IHANDL, ISTAT)                                        Mu
C     CALL GIOPSW (IUNIT, '14, ISTAT)                                   H
 910  CONTINUE
      IF (MLEVEL.GE.10) WRITE (MUNIT, 850) ISTAT                        D
      RETURN
      END
