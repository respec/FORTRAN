      SUBROUTINE ZSRTSX ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * JQUAL, LQUAL, CUNITS, CTYPE, IUHEAD, NUHEAD, IPLAN,
     * JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
C     Z-Store Regular interval Time-Series data
C     Data is stored according to the
C     time window set from CDATE/CTIME and NVALS.
C
C     Input:
C        IFLTAB:  Working DSS array used in ZOPEN call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CPATH:   Pathname of the data to be stored.  The "D" part
C                 is ignored.  CPATH sould be declared as
C                 CHARACTER*80.
C        CDATE:   Beginning date of the time window.  This may be
C                 a standard military style date (e.g. 01MAR74).
C        CTIME:   Beginning time of the time window.  This must be
C                 a standard 24 hour clock time (e.g. 1630).  Any
C                 time offset is implied by this date and time.
C                 CTIME should be declared as CHARACTER*4.
C        NVALS:   The number of data values to store.  This number
C                 defines the end of the time window.
C        VALUES:  The data to be stored.
C        CUNITS:  Character string containing the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string containing the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C        IPLAN:   A flag indicating if existing data should be written
C                 over or not:
C                 IPLAN = 0  Always replace data.
C                 IPLAN = 1  Only replace missing data.
C                 IPLAN = 4  Do not allow a missing input data to
C                            replace a valid data piece.
C     Output:
C        ISTAT:   Integer status parameter, indicating the
C                 successfullness of the data storage.
C                 ISTAT = 0  All ok.
C                 ISTAT = 4  All missing data flags - no data stored
C                            unless IPLAN set to 2.
C                 ISTAT > 9  Illegal call to ZSRTS
C
C     Written by Bill Charley at HEC, 1987.
C
C
C     DIMENSION STATEMENTS
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CDATE*(*), CTIME*(*)
C     INTEGER*6 IFLTAB(*), IUHEAD(*), IADD, JQUAL(*)                    H
C     INTEGER IFLTAB(*), IUHEAD(*), JQUAL(*)                            u
      INTEGER*4 IFLTAB(*), IUHEAD(*), JQUAL(*)                          ML
      INTEGER*4 JULS, JULE, INTL, IYMDJL, JULSD, JUL, IOFSET, JULAST    ML
      INTEGER*4 IFUNIT, IADD, NELMS                                     ML
      INTEGER IPLAN, ISTAT, NVALS
      LOGICAL LQUAL, LQREAD, LREADD, LUPRTS, LBASEV, LBASE, LDHIGH
      LOGICAL LHIGH, LPROT2
C
      REAL VALUES(*)
C
C     Local Dimensions
      CHARACTER CTPATH*88
      CHARACTER CPART(6)*32, CTSPAT*80
      CHARACTER CDATE1*12, CDATE2*12, CTIME1*4, CTIME2*4, CSCRAT*20
      INTEGER NPART(6)
      LOGICAL LFOUND, LERR
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
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
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSDC                                                           H
      INCLUDE 'zdssdc.h'                                                MLu
C
C
C
      NSTART = 1
      ISTAT = 0
      LPROT2 = LPROTC
C
      CALL ZINQIR ( IFLTAB, 'UNIT',   CDATE1, IFUNIT)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN                                             D
      WRITE (MUNIT,20) IFUNIT, IPLAN                                    D
 20   FORMAT (T10,'----- Enter ZSRTS for unit',I3,' -----',/,           D
     *        T10,'      IPLAN: ',I4)                                   D
      ENDIF                                                             D
C
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     CALL ZSRTS ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,          H
C    * CUNITS, CTYPE, IPLAN, ISTAT)                                     H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZSRTSX', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C     Are we in a read only state?
      IF (IFLTAB(KREADO).EQ.1) GO TO 987
C
C     Unform the pathname
      DO 30 I=1,6
      CPART(I) = ' '
 30   CONTINUE
      CALL CHRLNB (CPATH, NPATH)
      IF ((NPATH.GT.80).OR.(NPATH.LE.1)) GO TO 900
      CALL ZUFPN (CPART(1), NPART(1), CPART(2), NPART(2),
     * CPART(3), NPART(3), CPART(4), NPART(4), CPART(5), NPART(5),
     * CPART(6), NPART(6), CPATH, NPATH, IERR)
      IF (IERR.NE.0) GO TO 900
C
      IF (MLEVEL.GE.7) THEN                                             D
      WRITE (MUNIT, 40) CPATH(1:NPATH), NPATH                           D
 40   FORMAT (' Pathname: ',A,/,' Length:',I3)                          D
      IF (MLEVEL.GE.8) THEN                                             D
      CSCRAT = 'ABCDEF'                                                 D
      WRITE (MUNIT, 50) (CSCRAT(I:I), NPART(I), CPART(I),I=1,6)         D
 50   FORMAT (T10, 'Lengths and contents of the pathname parts:',       D
     * 6(/,T14,'Part ',A,' Length: ',I2,'   Contents: ',A))             D
      WRITE (MUNIT,55) JCOMP, LDHIGH, NPREC, LBASEV, BASEV              D
 55   FORMAT (T5,'Compression:',I3,', Size:',L2,', Precision:',I3,      D
     * ', Base:',L2,2X,F8.2)                                            D
      ENDIF                                                             D
      ENDIF                                                             D
C
C     If the compression flag is set to zero (use default compression),
C     see if the file has this data type to be compressed
      BASE = BASEV
      LBASE = LBASEV
      LHIGH = LDHIGH
      NPRE = NPREC
      IF (JCOMP.EQ.0) THEN
      IF (IFLTAB(KCOMPN).GT.0) THEN
      CALL ZGETCI (IFLTAB, CPART, IFCOMP, BASE, LBASE, LHIGH, NPRE)
      IF (MLEVEL.GE.8) THEN                                             D
      WRITE (MUNIT,255) IFCOMP, LHIGH, NPRE, LBASE, BASE                D
 255  FORMAT (T5,'File Compression:',I3,', Size:',L2,', Precision:',I3, D
     * ', Base:',L2,2X,F8.2)                                            D
      ENDIF                                                             D
      ELSE
      IFCOMP = 0
      ENDIF
      ENDIF
C
C     Determine the time interval, in minutes
      I = 1
      CALL ZGINTL (INTL, CPART(5), NUMDAT, I)
      IF (I.LT.0) GO TO 910
C
C     Check that a positive number of data was requested
      IF (NVALS.LE.0) GO TO 920
C
C     Get time window
      CALL DATJUL ( CDATE, JULS, IERR)
      IF (IERR.NE.0) GO TO 930
      ISTIME = IHM2M (CTIME)
      IF (ISTIME.EQ.0) THEN
      JULS = JULS - 1
      ISTIME = 1440
      ENDIF
C
C     Check for an illegal starting time
      IF ((ISTIME.LT.0).OR.(ISTIME.GT.1440)) GO TO 940
C     If the time is not on the standard boundaries, adjust it
C     and determine the time offset, in minutes.
      CALL ZOFSET (JULS,ISTIME,INTL,1,IOFSET)
C     Compute the ending time from the number of values
      I = INCTIM (INTL, 0, NVALS-1, JULS, ISTIME, JULE, IETIME)
C
      IF (MLEVEL.GE.7) THEN                                             D
      WRITE (MUNIT,80)INTL,NVALS                                        D
 80   FORMAT (T10,'Time Window set.  Interval:',I6,'  Number of',       D
     * ' data values:',I7)                                              D
      CALL JULDAT (JULS, 1, CDATE1, NDATE1)                             D
      CALL JULDAT (JULE, 1, CDATE2, NDATE2)                             D
      I = M2IHM (ISTIME, CTIME1)                                        D
      I = M2IHM (IETIME, CTIME2)                                        D
      WRITE (MUNIT,90) CDATE1(1:NDATE1), CTIME1, JULS, ISTIME,          D
     * CDATE2(1:NDATE2), CTIME2, JULE, IETIME, IOFSET                   D
 90   FORMAT (T10,'Starting date and time:  ',A,2X,A,'  (',I7,I5,')',/, D
     *        T10,'Ending   date and time:  ',A,2X,A,'  (',I7,I5,')',/, D
     *        T10,'Input time offset:',I7)                              D
      WRITE (MUNIT,95) LTOL, TOL                                        D
 95   FORMAT (T10,'Tolerance Set: ',L1,',   Tolerance:',F12.9)          D
      ENDIF                                                             D
C
C     Comupute a flag to send to ZMOVBK based upon IPLAN
      IFLAG = 1
      IF (IPLAN.EQ.1) THEN
      IFLAG = -1
      ELSE IF (IPLAN.EQ.4) THEN
      IFLAG = -4
      ENDIF
C
C
C     Obtain the date of the first block
      CALL ZBEGDT (JULS, INTL, IYR, IMON, IDAY, IBLOCK)
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
C     Get the date of the last block
      CALL ZBEGDT (JULE, INTL, JYR, JMON, JDAY, IBLOCK)
      JULAST = IYMDJL (JYR, JMON, JDAY)
C
C     Lock the file for multiple user access
      CALL ZMULTU ( IFLTAB, .TRUE.)
C
C     Loop, reading data blocks, changing the D (date)
C     part of the pathname each time
C
      CDATE1 = ' '
 100  CONTINUE
C     Get the new D (Date) part
      CALL YMDDAT (IYR, IMON, IDAY, 104, CDATE1, NDATE1, IERR)
      IF (IERR.NE.0) GO TO 950
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
      CTSPAT = ' '
      CALL ZPATH (CPART(1), CPART(2), CPART(3), CDATE1(1:NDATE1),
     * CPART(5), CPART(6), CTSPAT, NTSPAT)
C
C
C     Get the time of the next data block
      CALL ZINCBK (IBLOCK, JUL, IYR, IMON, IDAY)
C
      IF (INTL.EQ.10080) THEN
C     Weekly data.  Set so that the data is always on
C     Saturday at 2400 hours.
      NDAY = IDAYWK (JUL)
      JUL = JUL - NDAY + 1
      NDAY = IDAYWK (JULSD)
      JULSD = JULSD - NDAY + 1
      ENDIF
C
C
C     See if we need to read this record (does the time window
C     completely span this record, and IPLAN = 0)
      IF ((JULS.LT.JULSD).AND.(JULE.GT.JUL).AND.(IPLAN.EQ.0).AND.
     * (.NOT.LTOL)) THEN
      LREADD = .FALSE.
      IF (MLEVEL.GE.8) WRITE (MUNIT,210)                                D
 210  FORMAT(T10,'Bypass Read because time window spans entire block')  D
      ELSE
      LREADD = .TRUE.
      ENDIF
C
C     Check to see if the data record already exists.
      LWRITE = .TRUE.
      CALL ZCHECK (IFLTAB, CTSPAT, NTSPAT, JHEAD, JDATA, LFOUND)
      IF (MLEVEL.GE.8) THEN                                             D
      WRITE (MUNIT,220) LFOUND, CTSPAT                                  D
 220  FORMAT (T10,'After ZCHECK, Record found:',L2,/,T10,'Pathname: ',A)D
      IF (LFOUND) WRITE (MUNIT,230) JDATA, JHEAD                        D
 230  FORMAT (T10,'Number of data:',I5,'  Header length:',I4)           D
      ENDIF                                                             D
C
      IF (LFOUND) THEN
C
      IF (LPROT2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 235) CTSPAT(1:NTSPAT)
 235  FORMAT (' -----DSS---ZSRTSX:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
      GO TO 400
      ENDIF
C
C     Get Information Block
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL ZGTREC (IFLTAB, INFO, NINFO+NPPWRD, IADD, .TRUE.)
C     Double Check that this is the correct pathname
      IF (NTSPAT.NE.INFO(KINPAT)) GO TO 960
      CALL HOL2CH (INFO(KIPATH), CTPATH, NPMWRD)
      IF (CTSPAT(1:NTSPAT).NE.CTPATH(1:NTSPAT)) GO TO 960
C     Is the record compressed?
      IRCOMP = INFO(NPPWRD+KICOMP)
C     Does the record use data quality flags?
      IF (INFO(NPPWRD+KIQUAL).GT.0) THEN
      LQREAD = .TRUE.
      ELSE
      LQREAD = .FALSE.
      ENDIF
      ELSE
      IRCOMP = 0
      LREADD = .FALSE.
      LQREAD = .FALSE.
      ENDIF
C
C     Determine the compression scheme to use (if any)
      IF (JCOMP.EQ.0) THEN
C     The compression scheme of the data read overides the file default
      IF (IRCOMP.EQ.0) THEN
      IACOMP = IFCOMP
      ELSE
      IACOMP = IRCOMP
      ENDIF
      ELSE
      IACOMP = JCOMP
      ENDIF
C
C     Quality and data compression are mutually exclusive;
C     If LQUAL is true, turn off data compression
      IF (LQUAL.OR.LQREAD) IACOMP = 0
C
C     Be sure we have enough buffer space to use data compression
C     (can't use data compression with 1 min data!)
      IF ((IACOMP.NE.0).AND.(NUMDAT.GT.744)) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 238) CTSPAT(1:NTSPAT)
 238  FORMAT (' -----DSS---ZSRTSX:  Unable to compress data with',
     * ' this time interval;',/,' Pathname: ',A)
      IACOMP = 0
      ENDIF
C
C     Determine if we can use the subroutine ZUPRTS to store
C     the data.  This subroutine is faster than the alternative
C     and is meant to store a small amount of data in an
C     already existing recrod:
      LUPRTS = .TRUE.
C     Can't use if the record does not exist
      IF (.NOT.LFOUND) LUPRTS = .FALSE.
C     Can't use if data compression is on
      IF (IRCOMP.GT.0) LUPRTS = .FALSE.
      IF (IACOMP.GT.0) LUPRTS = .FALSE.
C     Can't use if we need to rearange data
      IF (LQUAL.NEQV.LQREAD) LUPRTS = .FALSE.
C     Can't use if we need to change the user header
      IF (JHEAD.NE.NUHEAD) THEN
      IF (NUHEAD.NE.-1) LUPRTS = .FALSE.
      ENDIF
C
C     Do we need to read data currently on the disk?
      IF ((LREADD).AND.(.NOT.LUPRTS)) THEN
C     If compression is on, and we are not replacing the entire
C     block, read the record and uncompress the data set
      IF (IRCOMP.GT.0) THEN
C     Read the compression header
      NDCH = INFO(NPPWRD+KINCHE)
      CALL ZGTREC (IFLTAB, IDCH, NDCH, INFO(NPPWRD+KIACHE), .TRUE.)
C     Read the compressed data
      NCDA = INFO(NPPWRD+KINDAT)
      CALL ZGTREC (IFLTAB, BUFF(745), NCDA, INFO(NPPWRD+KIADAT),
     * .FALSE.)
C     Uncompress the data
      NELMS = -1
      NDCH = NDCH * 2
      CALL DUREAL (BUFF(1), 745, 1, NELMS, BUFF(745), NCDA, 745, IDCH,
     * NDCH, IST)
      IF (IST.NE.0) GO TO 980
C     NEED TO GET OTHER COMPRESSION INFORMATION HERE !!!!!!!!!!!!!!!!!!
C     (EG BASE NPREC, ETC!!!)
      ELSE
C     Read the data for ZMOVBK
      NDA = INFO(NPPWRD+KINDAT)
      CALL ZGTREC (IFLTAB, BUFF, NDA, INFO(NPPWRD+KIADAT), .FALSE.)
      ENDIF
      ENDIF
C
C
C     Compute the logical amount of data to be stored
      NDATA = NOPERS (INTL, 0, JULSD, 0, JUL, 0)
      NLDATA = NDATA
      NTDATA = NDATA
      IF ((LQUAL).OR.(LQREAD)) NTDATA = NDATA * 2
C
C
C     If no data was read, fill BUFF with missing data flag (-901.0)
 240  CONTINUE
      IF (.NOT.LFOUND) THEN
      IF (LQUAL) THEN
      DO 250 I=1,NTDATA,2
      BUFF(I) = -901.0
      BUFF(I+1) = 0.0
 250  CONTINUE
      ELSE
      DO 260 I=1,NDATA
      BUFF(I) = -901.0
 260  CONTINUE
      ENDIF
      ELSE
C     If quality flags are to be added to a block without them,
C     expand the block for them
      IF ((LQUAL).AND.(.NOT.LQREAD)) THEN
      DO 261 I=NDATA,1,-1
      ILBUFF(I*2) = 0
      ILBUFF(I*2-1) = ILBUFF(I)
 261  CONTINUE
      ENDIF
      ENDIF
C
C
C     Move data values from VALUES into BUFF
      IF (.NOT.LUPRTS) THEN
      CALL ZMOVBK (VALUES, JQUAL, LQUAL, LQREAD, NSTART, NVALS, JULS,
     * ISTIME, INTL, BUFF, ILBUFF, NDATA, JULSD, JSTAT, IFLAG)
      IF (JSTAT.NE.0) THEN
      ISTAT = JSTAT
      IF (ISTAT.GE.10) GO TO 970
      ENDIF
      ENDIF
C
C     Compress the data, if a compression scheme is used
      IF (IACOMP.GT.0) THEN
C
      IF (IACOMP.GT.5) GO TO 980
      IF ((IACOMP.EQ.2).OR.(IACOMP.EQ.3)) THEN
      IF ((NPRE.LT.-6).OR.(NPRE.GT.6)) GO TO 985
      IF (LHIGH) THEN
      NBYTES = 2
      ELSE
      NBYTES = 0
      ENDIF
      ENDIF
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,265)IACOMP, LHIGH, NPRE, LBASE, BASED
 265  FORMAT (T5,'Compression Set, Scheme:',I3,', Size:',L2,            D
     * ',  Precision:',I3,', Base:',L2,2X,F8.2)                         D
      NELMS = NTDATA
      CALL DCREAL (BUFF, NELMS, IACOMP, BASE, LBASE, NBYTES, NPRE,
     * BUFF(745), NRB, 745, IDCH, NCHEAD, KDCH, IST)
      IF (MLEVEL.GE.9) WRITE (MUNIT, 266) NRB, NCHEAD, IST              D
 266  FORMAT (T5,'Compression;  NRB:',I6,'  NCHEAD:',I6,', STATUS:',I6) D
C
C     If a combination compression scheme was unsuccessful,
C     try compressing with just the repeat scheme.
      IF (IST.NE.0) THEN
      IF ((IACOMP.EQ.3).OR.(IACOMP.EQ.5)) THEN
      IACOMP = 1
      NELMS = NTDATA
      CALL DCREAL (BUFF, NELMS, IACOMP, BASE, LBASE, NBYTES, NPRE,
     * BUFF(745), NRB, 745, IDCH, NCHEAD, KDCH, IST)
      IF (MLEVEL.GE.9) WRITE (MUNIT, 266) NRB, NCHEAD, IST              D
      ENDIF
      ENDIF
C
      N = NRB + NCHEAD
      IF ((IST.NE.0).OR.(NRB.LE.0).OR.(N.GE.NTDATA)) THEN
      IACOMP = 0
      NCHEAD = 0
      IF (IST.EQ.0) IST = -1
      IF (MLEVEL.GE.2) WRITE (MUNIT,267) IST, CTSPAT(1:NTSPAT)
 267  FORMAT (' -----DSS---ZSRTSX;  WARNING:  Unable to Compress',
     * ' Data.  Status:',I6,/,' Pathname: ',A)
      ELSE
      NTDATA = NRB
      ENDIF
      ELSE
      NCHEAD = 0
      ENDIF
C
C     Add the Header Information
      NIHEAD = 5
      INTBUF(1) = IOFSET
      CSCRAT = CUNITS
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(2),  1)
      CSCRAT = CTYPE
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(4), 1)
C
C     Set the compression value and quality flag to be used in the write
      ICOMP = IACOMP
      IQUAL = 0
      IF ((LQUAL).OR.(LQREAD)) IQUAL = 1
C
C     If we are updating, and NUHEAD is -1, do not write over the
C     the existing user header
      IF (NUHEAD.GE.0) THEN
      NUH = NUHEAD
      ELSE
      NUH = JHEAD
      ENDIF
C
C     Now reserve space in the DSS file
      ITYPE = 100
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      IF (LFOUND) THEN
C     If we are updating data in a record, we don't need to rewrite
C     the entire record (i.e., not changing the number of data)
      IF (.NOT.LUPRTS) THEN
      CALL ZOWRIT (IFLTAB, CTSPAT, NTSPAT, NIHEAD, NCHEAD, NUH,
     * NTDATA)
      ENDIF
      ELSE
C     Write a new record to the DSS file
      CALL ZNWRIT (IFLTAB, CTSPAT, NTSPAT, NIHEAD, NCHEAD, NUH,
     * NTDATA)
      ENDIF
C
      ITYPE = 0
C
C     Now store the header arrays
      CALL ZPTREC (IFLTAB, INTBUF, NIHEAD, INFO(NPPWRD+KIAIHE), .FALSE.)
      IF (NCHEAD.GT.0) CALL ZPTREC (IFLTAB, IDCH, NCHEAD,
     * INFO(NPPWRD+KIACHE), .FALSE.)
      IF (NUHEAD.GT.0) CALL ZPTREC (IFLTAB, IUHEAD, NUHEAD,
     * INFO(NPPWRD+KIAUHE), .FALSE.)
C
C     Now store the data array
C
      IF (LUPRTS) THEN
C     We are only updating a portion of the record (already in the file)
C     Only store the data passed to us
      CALL ZUPRTS (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NDATA, NVALS, VALUES, JQUAL, LQUAL, IPLAN, LERR)
      IF (LERR) GO TO 990
      ELSE
      IF (IACOMP.LE.0) THEN
      IPOS = 1
      ELSE
      IPOS = 745
      ENDIF
      CALL ZPTREC (IFLTAB, BUFF(IPOS), NTDATA, INFO(NPPWRD+KIADAT),
     * .FALSE.)
      ENDIF
C
C     Are we done storing the data?
 290  CONTINUE
      LWRITE = .FALSE.
      IF (MLEVEL.GE.3) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT,300) CTSPAT(1:NTSPAT)
 300  FORMAT(' --ZWRITE: ',A)
      ELSE
      WRITE (MUNIT,320)IFLTAB(KUNIT),INFO(NPPWRD+KIVER),CTSPAT(1:NTSPAT)
 320  FORMAT(' -----DSS---ZWRITE Unit',I4,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
 400  CONTINUE
      IF (NSTART.GT.NVALS) GO TO 800
      IF (JUL.GT.JULAST) GO TO 800
C
C     Clear some space in the buffer arrays
      CALL ZBDUMP (IFLTAB, 2)
C
C     Need to store more data, loop back to 100
      GO TO 100
C
C
C     Done.  Exit ZSRTS.
 800  CONTINUE
C     Unlock the file and dump all buffers
      CALL ZMULTU (IFLTAB, .FALSE.)
      LTOL = .FALSE.
C
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT                   D
 820  FORMAT(T10,'----- Exit ZSRTS, Number of data values ',            D
     * 'stored:',I7,',  Status:',I4,/)                                  D
C
      RETURN
C
C
C     --- ERROR STATEMENTS ---
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) NPATH, CPATH
 901  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - ILLEGAL PATHNAME',
     * ' OR PATHAME LENGTH',/,' Length: ',I5,/,' Pathname: ',A,/)
      ISTAT = 24
      GO TO 990
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPART(5), INTL, CPATH
 911  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - NON-STANDARD TIME',
     * ' INTERVAL',/,' Interval: ',A,2X,I8,/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) NVALS, CPATH
 921  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - NUMBER OF VALUES',
     * ' TO STORE IS LESS THAN 1',/,' NVALS: ',I8,/,' Pathname: ',A,/)
      ISTAT = 11
      GO TO 990
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CDATE, JULS, CPATH
 931  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - ILLEGAL STARTING',
     * ' DATE SPECIFIED',/,' Date: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) CTIME, ISTIME, CPATH
 941  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - ILLEGAL STARTING',
     * ' TIME SPECIFIED',/,' Time: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 950  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,951) CDATE1, IYR, IMON, IDAY, CPATH
 951  FORMAT(/,' *****DSS*** ZSRTS:  FATAL ERROR  - UNABLE TO GENERATE',
     * ' BLOCK DATE',/,' Date: ',A,3X,3I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 960  CONTINUE
      NP = INFO(KINPAT)
      CALL ZERROR (IFLTAB, 11, 'ZSRTSX', 0, IADD, CTSPAT, NTSPAT,
     * CTPATH, NP)
C
 970  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,971) ISTAT, CPATH
 971  FORMAT (/,' *****DSS*** ZSRTS:  FATAL ERROR  - UNABLE TO ',
     * ' STORE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      GO TO 990
C
 980  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 981) IACOMP
 981  FORMAT (/,' *** ERROR:  ZSRTSX;  Illegal Data compression scheme',
     * /,' Setting:',I6,';  Min Allowed: 0,  Max: 5')
      ISTAT = 51
      GO TO 990
C
 985  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 986) NPRE
 986  FORMAT (/,' *** ERROR:  ZSRTSX;  Illegal Data Compression',
     * ' Precision Value'/,' Value:',I6,';  Min Allowed: -6,  Max: 6')
      ISTAT = 52
      GO TO 990
C
 987  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 988) CPATH
 988  FORMAT (' -----DSS---ZSRTSX:  ERROR;  File has Read Access Only',
     * /,' Pathname: ',A)
      ISTAT = 30
      GO TO 990
C
C
 990  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,991) ISTAT                          D
 991  FORMAT(T10,'----- Exit ZSRTS, Error return; Status:',I4)          D
      GO TO 800
C
      END
