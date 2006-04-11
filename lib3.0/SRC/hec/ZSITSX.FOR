      SUBROUTINE ZSITSX (IFLTAB, CPATH, ITIMES, VALUES, NVALUE, IBDATE,
     * JQUAL, LSQUAL, CUNITS, CTYPE, IUHEAD, NUHEAD, INFLAG, ISTAT)
C
C     Z - Store - IRregular - Time - Series  data into DSS database
C     (Extended version)
C
C     Written by Bill Charley
C
C
C     --- Arguments ---
C     Input:
C         IFLTAB -  File table array used in ZOPEN CALL (DIM to 1200)
C         CPATH  - Pathname
C         VALUES - The data values to be stored by the subroutine.
C         ITIMES - An array containing the minutes of each data value,
C                  from the base date, IBDATE
C         NVALS -  Number if date/value pairs to store.
C         IBDATE -  Base date - real julian date of starting time; add
C                  ITIMES(I) to this to get the date for (I).
C         CUNITS - Units of data, CHARACTER*8
C         CTYPE -  Type of data, CHARACTER*8
C         INFLAG - Flag to indicate whether to replace or merge new data
C                  with old data. Replace for editing/changing data,
C                  merge for adding data.
C                  = 0  FOR MERGE
C                  = 1  FOR REPLACE
C
C         ISTAT -  Status parameter on operation
C                  = 0 IF OK
C                  = 4 IF NO DATA FOUND
C                  .GE. 10 IF A 'FATAL' ERROR OCCURED
C
C
C     INTEGER*6 IFLTAB(*), JQUAL(*), ITIMES(*), IUHEAD(*)               H
      INTEGER*4 IFLTAB(*), JQUAL(*), ITIMES(*), IUHEAD(*)               ML
C     INTEGER IFLTAB(*), JQUAL(*), ITIMES(*), IUHEAD(*)                 u
      REAL VALUES(*)
C
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C     INTEGER*6 JTIME, I6, I1440                                        H
      INTEGER*4 JTIME, I6, I1440                                        ML
      INTEGER*4 JULS, JULE, IYMDJL, JUL, IBDATE, IDATE, JULSB, JULEB    ML
      INTEGER*4 NQUAL                                                   ML
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*), CDATE1*9, CDATE2*9
      CHARACTER CPATH1*80
      CHARACTER CTIME1*4, CTIME2*4, CDATE*9, CSCRAT*20
C
C
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
      LOGICAL LF, LCLEAR, LQUAL, LSQUAL
C
C
      ISTAT = 0
      IPOS = 1
      I1440 = 1440
      NVALS = NVALUE
C
C
C     Get beginning and ending dates from DATE array
      CALL DATCLL (IBDATE, ITIMES(1), JULS, ISTIME)
      CALL DATCLL (IBDATE, ITIMES(NVALS), JULE, IETIME)
C
C
      IF (MLEVEL.GE.9) THEN                                             D
      CALL JULDAT (JULS, 114, CDATE1, N)                                D
      CALL JULDAT (JULE, 114, CDATE2, N)                                D
      WRITE (MUNIT, 20) JULS, ISTIME, CDATE1, JULE, IETIME, CDATE2,     D
     * CPATH                                                            D
 20   FORMAT(T10,'----- ENTERING ZSITS-----',                           D
     */,T5,'Starting date and time: ',3X,2I8,2X,A,                      D
     */,T5,'Ending date and time:   ',3X,2I8,2X,A,                      D
     */,T5,'Pathname: ',A)                                              D
      WRITE (MUNIT,30) KLBUFF,NVALS,INFLAG                              D
 30   FORMAT(T5,'BUFF DIM:',I5,'  NUMBER OF DATA:',I6,',  INFLAG:',I4)  D
      J = NVALS                                                         D
      IF (J.GT.25) J = 25                                               D
      DO 40 I=1,J                                                       D
      WRITE (MUNIT,35) I, ITIMES(I), VALUES(I)                          D
 35   FORMAT (' Element',I4,',  Time:',I8,',  Value:',F10.1)            D
 40   CONTINUE                                                          D
      ENDIF                                                             D
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     CALL CHRLNB (CPATH, NPATH)                                        H
C     N = NIBUFF                                                        H
C     BDATE = IBDATE                                                    H
C     CALL ZMIN2R (ITIMES, ITIMES, 0.0, NVALS)                          H
C     CALL ZPIRTS (IFLTAB, CPATH, NPATH, BUFF, KLBUFF, INTBUF,          H
C    * N, ITIMES, VALUES, NVALS, BDATE, CUNITS, CTYPE,                  H
C    * INFLAG, ISTAT)                                                   H
C     CALL ZR2MIN (ITIMES, ITIMES, 0, NVALS)                            H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZSITSX', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C     Check that the dates are in order
      IF (NVALS.NE.1) THEN
      DO 70 J=1,NVALS-1
      K = J + 1
      IF (ITIMES(K).LE.ITIMES(J)) THEN
      WRITE (MUNIT,60) CPATH, ITIMES(J), ITIMES(K)
 60   FORMAT(' -----DSS*** ZSITS:  Error - Times are not Ascending:',
     * /,' Pathname: ',A,/,' Relative Times: ',2I10)
C
C     Figure out dates/times to write out in a nice fashion
      CALL DATCLL (IBDATE, ITIMES(J), IDATE, ITIME)
      CALL JULDAT (IDATE, 104, CDATE1, ND)
      ND = M2IHM (ITIME, CTIME1)
C
      CALL DATCLL (IBDATE, ITIMES(K), IDATE, ITIME)
      CALL JULDAT (IDATE, 104, CDATE2, ND)
      ND = M2IHM (ITIME, CTIME2)
      WRITE (MUNIT,65) J, VALUES(J), ITIMES(J), CDATE1, CTIME1,
     * K, VALUES(K), ITIMES(K), CDATE2, CTIME2
 65   FORMAT (2(' Element',I4,', Value:',F8.3,' Time offset',I6,
     * ',  has a date of ',A,', and time ',A,/))
      ISTAT = 30
      GO TO 800
      ENDIF
 70   CONTINUE
      ENDIF
C
C
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
C
      IF (ISTAT.EQ.0) CALL ZIRBEG (IFLTAB, JULS,
     * CPATH(IBPART(5):IEPART(5)), IYR, IMON, IDAY, IBLOCK, MINBLK,
     * INCBLK)
C
      IF ((IBLOCK.LE.0).OR.(ISTAT.NE.0)) GO TO 910
C
C
C     Get starting date of first block
      JUL = IYMDJL (IYR, IMON, IDAY)
      CPATH1 = CPATH(1:IBPART(4)-1) // '01JAN1900/' //
     * CPATH(IBPART(5):IEPART(6)+1)
      CALL CHRLNB (CPATH1, NPATH)
C
C
C     Major loop through time blocks
 100  CONTINUE
C
      JULSB = JUL
      LCLEAR = .FALSE.
C     Form the pathname
      CALL JULDAT (JUL, 104, CDATE, N)
      CPATH1(IBPART(4):IBPART(4)+8) = CDATE
C
C     Get starting date of subsequent blocks
      CALL ZINCBK (IBLOCK, JUL, IYR, IMON, IDAY)
C
C
C     Get starting and ending dates of existing data block
      JULEB = JUL - 1
C
C
C     If data is to be replaced and times encompass entire
C     block--don't read, just replace block
      IF (INFLAG.EQ.1) THEN
      IF ((JULS.LT.JULSB).AND.(JULE.GE.JUL)) GO TO 500
      END IF
C
C
      CALL ZREADX (IFLTAB, CPATH1, INTBUF, NIBUFF, NIHEAD,
     * ICHEAD, 0, J, IUHEAD, 0, N, BUFF, KLBUFF, NDA, 2, LF)
C
      IF (.NOT.LF) GO TO 500
C
C     Check that the buffer size is large enough.
      IF (NDA.GT.KLBUFF) GO TO 920
C
C     Found data
C
      IBSIZE = INTBUF(1)
      NPAIRS = INTBUF(2)
C     IF (IBSIZE.LE.0)OR (NPAIRS.LE.0) THEN ???
      CALL ZINQIR (IFLTAB, 'QUAL', CSCRAT, NQUAL)
      IF (NQUAL.EQ.1) THEN
      LQUAL = .TRUE.
      ELSE
      LQUAL = .FALSE.
      ENDIF
C
      IF ((LSQUAL).AND.(.NOT.LQUAL)) THEN
      DO 195 I=NPAIRS,1,-1
      ILBUFF(I*3) = 0
      ILBUFF(I*3-1) = ILBUFF(I*2)
      ILBUFF(I*3-2) = ILBUFF(I*2-1)
 195  CONTINUE
      LQUAL = .TRUE.
      ENDIF
C
      IF (LQUAL) THEN
      IMULT = 3
      ELSE
      IMULT = 2
      ENDIF
C
C
C     ----- REPLACE DATA -----
C
      IF (INFLAG.EQ.1) THEN
C
C     There are 6 possible cases for replacing data:
C
C    Block read          Block to be stored
C
C                                                     ____
C                                                    |    |
C                                             ____   | 6  |
C                                     ____   |    |  |    |
C                                    |    |  | 5  |  |____|
C    ______  ........................|....|..|....|............
C   |      |                         |    |  |    |
C   |      |                  ____   |    |  |____|
C   |      |                 |    |  | 4  |
C   | Read |                 | 3  |  |    |
C   |      |          ____   |    |  |    |
C   |      |         |    |  |____|  |    |
C   |______| ........|....|..........|....|....................
C             ____   | 2  |          |    |
C            |    |  |    |          |____|
C            | 1  |  |    |
C            |    |  |____|
C            |____|
C
C
C
C
C     Locate dates in block read
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,202) NPAIRS,IBSIZE                   D
202   FORMAT(' RECORD READ; NPAIRS, IBSIZE = ',2I8)                     D
C
C     Check to see if data starts beford this block
      IF (IPOS.GT.1) THEN
      IBEG = 1
      GO TO 250
      ENDIF
C
C
C     Is this a case #1?
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
C     ###### CASE 1 ######
C     Is the current time > than the last value read?
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) THEN
C     Data occurs after last piece of data read
C     Add data to the end of BUFF
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 215'                D
 215  CONTINUE
C
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
C
C     Check to see if first or last data to be removed (-902.)
      IF (VALUES(IPOS).EQ.-902.0) THEN
      IF ((IPOS.EQ.1).OR.(IPOS.EQ.NVALS)) GO TO 220
      ENDIF
C
      NPAIRS = NPAIRS + 1
      N = ((NPAIRS-1)*IMULT) + 1
      IF ((N+1).GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) THEN
      IF (LSQUAL) THEN
      ILBUFF(N+2) = JQUAL(IPOS)
      ELSE
      ILBUFF(N+2) = 0
      ENDIF
      ENDIF
C
 220  IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
      GO TO 215
      ENDIF
C     ######  ######
C
C
C     Locate beginning position of dates() in BUFF
      DO 210 J=1,NPAIRS
      IBEG = J
      N = ((J-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).LE.0) GO TO 250
 210  CONTINUE
C
C     Data beings prior to or in the middle of BUFF
C     Case 2, 3, or 4
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 250'                D
 250  CONTINUE
C     Is the last value to be stored .ge. the last value read?
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(NVALS), JULSB, JTIME ).GE.0) THEN
C
C     Times encompass entire block read (Case 4)
      IF (IBEG.EQ.1) GO TO 500
C
C
C     Data is to be replaced in later part of
C     block read  through the end of BUFF)
C
C     ###### CASE 2 ######
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 290'                       D
C     Insert new data into BUFF
C     Is the first value just a flag for the end of the time window?
      IF ((IPOS.EQ.1).AND.(VALUES(IPOS).EQ.-902.0)) IPOS = IPOS + 1
      JPOS = IPOS
      DO 290 I=JPOS,NVALS
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
      NPAIRS = IBEG
      N = ((IBEG-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) THEN
      IF (LSQUAL) THEN
      ILBUFF(N+2) = JQUAL(IPOS)
      ELSE
      ILBUFF(N+2) = 0
      ENDIF
      ENDIF
      IBEG = IBEG + 1
      IPOS = IPOS + 1
 290  CONTINUE
C
C     Is the last value just a flag for the end of the time window?
      IF (VALUES(NVALS).EQ.-902.0) NPAIRS = NPAIRS - 1
      GO TO 600
C
C     ######  ######
C
      ELSE
C     Data occurs in the middle of BUFF (i.e., does
C     not go beyond the end of BUFF)
C
C     Obtain the location (in BUFF) of the last piece of data
      DO 300 J=IBEG,NPAIRS
      IEND = J
      N = ((J-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(NVALS), JULSB, JTIME).LT.0) GO TO 310
 300  CONTINUE
C
C     IMPOSSIBLE?????
C
C
C     ##### CASE 3, 5, 6 ######
 310  CONTINUE
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 310'                D
C
C     Increase or decrease space in BUFF to allow for new data
      J = NVALS - IPOS + 1
      K = IEND - IBEG
      NDOWN = J - K
      IF ((IPOS.EQ.1).AND.(VALUES(IPOS).EQ.-902.0)) NDOWN = NDOWN - 1
      IF ((VALUES(NVALS).EQ.-902.0).AND.(NVALS.GT.0)) NDOWN = NDOWN - 1
      IF (NDOWN.LT.0) LCLEAR = .TRUE.
      NPAIRS = NPAIRS + NDOWN
      IF (NPAIRS*IMULT.GT.KLBUFF) GO TO 900
      CALL ZIRDOW (IBEG, NPAIRS, NDOWN, IMULT, BUFF)
C
C     Insert new data into BUFF
      IEND = IEND + NDOWN - 1
C     Should we delete the first data value (set to -902.)?
      IF ((IPOS.EQ.1).AND.(VALUES(IPOS).EQ.-902.0)) IPOS = IPOS + 1
      IF ((IEND.EQ.NVALS).AND.(VALUES(NVALS).EQ.-902.0)) IEND = IEND - 1
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 320'                       D
      DO 320 K=IBEG,IEND
      N = ((K-1)*IMULT) + 1
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) THEN
      IF (LSQUAL) THEN
      ILBUFF(N+2) = JQUAL(IPOS)
      ELSE
      ILBUFF(N+2) = 0
      ENDIF
      ENDIF
      IPOS = IPOS + 1
 320  CONTINUE
C
      GO TO 600
C
C     #####  ######
C
      ENDIF
C
C
      ELSE
C
C
C     ----- MERGE DATA -----
C
C
      IBEG = 1
C
C     If we are only adding data to end, speed up by not searching BUFF
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) GO TO 410
C
C     Loop through block read, replacing and inserting data
 400  CONTINUE
C
C     If date is GREATER than current BUFF date:
C     Loop back and compare next BUFF date)
      N = ((IBEG-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) THEN
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
      IBEG = IBEG + 1
      IF (IBEG.GT.NPAIRS) GO TO 410
      GO TO 400
      ENDIF
C
C     If date is LESS than current BUFF date:
C     Move dates and values in BUFF down one (after this spot)
C     to allow for the new piece of data to be added here
      N = ((IBEG-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).LT.0) THEN
      NPAIRS = NPAIRS + 1
      IF (NPAIRS*IMULT.GT.KLBUFF) GO TO 900
      CALL ZIRDOW (IBEG, NPAIRS, 1, IMULT, BUFF)
      ENDIF
C
C     Insert new date and value here (or replace if equal)
      N = ((IBEG-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) THEN
      IF (LSQUAL) THEN
      ILBUFF(N+2) = JQUAL(IPOS)
      ELSE
      ILBUFF(N+2) = 0
      ENDIF
      ENDIF
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
C
      GO TO 400
C
C
C     The remainder of the data occurs at the end of the block
C     (or into the next block)
 410  CONTINUE
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
C
      NPAIRS = NPAIRS + 1
      N = ((NPAIRS-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) THEN
      IF (LSQUAL) THEN
      ILBUFF(N+2) = JQUAL(IPOS)
      ELSE
      ILBUFF(N+2) = 0
      ENDIF
      ENDIF
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
      GO TO 410
C
C
      ENDIF
C
C
C
C
C     Block does not exist (or data to be replaced fully encompasses
C     block just read) -- create entirely new block to write
C     ###### CASE 4 ######
 500  CONTINUE
C
      LQUAL = LSQUAL
      IF (LQUAL) THEN
      IMULT = 3
      ELSE
      IMULT = 2
      ENDIF
      LCLEAR = .TRUE.
C     Check to see if first or last data to be removed (-902.)
      IF ((IPOS.EQ.1).AND.(VALUES(IPOS).EQ.-902.0)) IPOS = IPOS + 1
      IF ((VALUES(NVALS).EQ.-902.).AND.(NVALS.GT.0)) NVALS = NVALS - 1
      IF (IPOS.GT.NVALS) GO TO 800
C
C     Look for ending position of data
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 510'                       D
      DO 510 I=1,NVALS
C
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) THEN
      NPAIRS = I - 1
      GO TO 600
      ENDIF
C
      N = ((I-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      ILBUFF(N) = ITIMES(IPOS) - (I6 * I1440)
      BUFF(N+1) = VALUES(IPOS)
      IF (LQUAL) ILBUFF(N+2) = JQUAL(IPOS)
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) THEN
      NPAIRS = I
      GO TO 600
      ENDIF
 510  CONTINUE
C
      NPAIRS = NVALS
C
 600  CONTINUE
C
C     Is there any data to this block?
      IF (NPAIRS.LE.0) GO TO 620
C
C     Determine block size of this record
      JBSIZE = IBSIZE
      IF (NPAIRS.LE.MINBLK) THEN
      IBSIZE = MINBLK
      ELSE
      K = (NPAIRS - MINBLK) / INCBLK
      IBSIZE = MINBLK + ((K+1) * INCBLK)
      ENDIF
      IF (JBSIZE.NE.IBSIZE) LCLEAR = .TRUE.
C
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,540)NPAIRS,IBSIZE                    D
 540  FORMAT(T5,'NUMBER OF PAIRS THIS RECORD = ',I5,' BLOCK SIZE =',I5) D
C
C     Clear out 'null data' area (between NPAIRS and IBSIZE)
      IF (LCLEAR) THEN
      NDA = IBSIZE * IMULT
      K = (NPAIRS * IMULT) + 1
      DO 550 I=K,NDA
      ILBUFF(I) = 0
 550  CONTINUE
      ENDIF
C
C
C     Move header information into arguments
      INTBUF(1) = IBSIZE
      INTBUF(2) = NPAIRS
      INTBUF(3) = JULSB
C     Relative time of last data value
      INTBUF(4) = ILBUFF((NPAIRS*IMULT)-1)
      INTBUF(5) = 0
      CSCRAT = CUNITS
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(6),  1)
      CSCRAT = CTYPE
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(8), 1)
      NIHEAD = 9
C
C
C     Write data to DSS
      ITYPE = 110
      CALL ZSET ('TYPE', ' ', ITYPE)
      NLDATA = NPAIRS
      IF (LQUAL) CALL ZSET ('QUAL', 'ON', 1)
      NDA = IBSIZE * IMULT
      CALL ZWRITX (IFLTAB, CPATH1, NPATH, INTBUF, NIHEAD, 0, 0,
     * IUHEAD, NUHEAD, BUFF, NDA, 110, 0, IST, LF)
C *********************************************************************
C
C
 620  CONTINUE
C     Have we reached the end time yet?
C     IF (IPOS.GT.NVALS) GO TO 800
      IF (JULE.LT.JUL) GO TO 800
C
C     Go read next record
      GO TO 100
C
C
C
C     Finished
 800  CONTINUE
      IF ((NVALS.EQ.0).AND.(ISTAT.EQ.0)) ISTAT = 4
      IF (MLEVEL.GE.9) WRITE (MUNIT, 820) NVALS, ISTAT                  D
 820  FORMAT (T5,'----- Exiting ZSITS; Values Written:',I5,             D
     * ',  Status:',I4)                                                 D
      RETURN
C
C
C     Error conditions
C     Not enough buffer space to write record to DSS
 900  CONTINUE
      WRITE (MUNIT,901) CPATH1, KLBUFF
 901  FORMAT(/,' ***** ERROR - ZSITS;  Buffer size not large enough',
     * ' to store this amount of data *****',/,' Pathname: ',A,/,
     * ' Buffer Size:',I6,/)
      ISTAT = 21
      GO TO 800
C
 910  CONTINUE
      WRITE (MUNIT,911) CPATH
 911  FORMAT (' -----DSS*** ZSITS:  Error - Unable to ',
     *' Recognize Pathname as Irregular Time-Series',/,
     *' Pathname: ',A)
      ISTAT = 24
      GO TO 800
C
 920  CONTINUE
      WRITE (MUNIT,921) CPATH1, NDA, KLBUFF
 921  FORMAT (/,' ***** ERROR - ZSITS;  The Buffer Array is not',
     *' Large Enough to Read Record',/,
     *' Record: ',A,/' Buffer Size Required:',I6,'  Size Provided',I6/)
      ISTAT = 21
      GO TO 800
C
C
      END
