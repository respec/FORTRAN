      SUBROUTINE ZRDBUF (IFLTAB, CPATH, IHBUFF, KHBUFF, NHBUFF,
     * IDBUFF, KDBUFF, NDBUFF, LEND, IPLAN, LFOUND)
C
C     Bufferd read from the DSS file
C     Main subroutine for reading data from the DSS file
C
C     Written by Bill Charley at HEC, 1989.
C
C     INTEGER*6 IFLTAB(*), IDBUFF(*), IHBUFF(*), NADD                   H
C     INTEGER IFLTAB(*), IDBUFF(*), IHBUFF(*)                           u
      INTEGER*4 IFLTAB(*), IDBUFF(*), IHBUFF(*), NADD                   ML
      CHARACTER CTPATH*90, CPATH*(*)
      LOGICAL LNEW, LEND, LFOUND
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
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH         D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZRDBUF',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A)                                        D
C
      CALL CHRLNB (CPATH, NPATH)
      NPPWRD = (NPATH-1)/NCPW + 1
      NPMWRD = (NPATH-1)/NCMW + 1
C
C     IF (IFLTAB(1).NE.6) THEN                                          H
C     NHBUFF = KHBUFF                                                   H
C     NDBUFF = KDBUFF                                                   H
C     LEND = .TRUE.                                                     H
C     CALL ZREAD4 ( IFLTAB, CPATH, NPATH, IHBUFF, NHBUFF,               H
C    * IDBUFF, NDBUFF, IPLAN, LFOUND)                                   H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Is this the first call for this record?
      LNEW = .TRUE.
      LREAD = .TRUE.
      IF (IFLTAB(KRBNPA).EQ.NPATH) THEN
      CALL HOL2CH (IFLTAB(KRBPAT), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).EQ.CTPATH(1:NPATH)) THEN
      IF (NPATH.EQ.INFO(KINPAT)) THEN
      CALL HOL2CH (INFO(KIPATH), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).EQ.CTPATH(1:NPATH)) LNEW = .FALSE.
      ENDIF
      ENDIF
      ENDIF
C
      IF (LNEW) THEN
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'--ZRDBUF:  New Record'          D
C
C     Yes.  Read the information block
C
      CALL ZRDINF ( IFLTAB, CPATH, JHEAD, JDATA, ISTAT)
      NDBUFF = JDATA
      NHBUFF = JHEAD
      IF (ISTAT.EQ.0) THEN
      LFOUND = .TRUE.
      ELSE
      LFOUND = .FALSE.
      ENDIF
C
C     If the record does not exist, write message then return
      IF (.NOT.LFOUND) THEN
      NHBUFF = 0
      NDBUFF = 0
      IF ((IPLAN.NE.2).OR.(MLEVEL.GT.4)) THEN
      IF (MLEVEL.GE.2) WRITE ( MUNIT,40) IFLTAB(KUNIT), CPATH(1:NPATH)
 40   FORMAT (' -----DSS--- ZREAD:  Cannot Find Record Specified;',
     * '  Unit:',I3,/,' Pathname: ',A)
      ENDIF
      GO TO 800
      ENDIF
C
C     Don't attempt to read more than what is stored
      IF (KDBUFF.LT.NDBUFF) NDBUFF = KDBUFF
      IF (KHBUFF.LT.NHBUFF) NHBUFF = KHBUFF
C
      IFLTAB(KRBNPA) = NPATH
      CALL CH2HOL (CPATH, IFLTAB(KRBPAT), NPMWRD)
C
C     Read the Header array
      NADD = INFO(NPPWRD+KIAUHE)
      IF (NHBUFF.GT.0) CALL ZGTREC ( IFLTAB, IHBUFF, NHBUFF, NADD,
     * .FALSE.)
C
      IFLTAB(KRBHBE) = NADD + NHBUFF
      IFLTAB(KRBHEN) = NADD + JHEAD - 1
C
C     Read the data array
      NADD = INFO(NPPWRD+KIADAT)
      IF (NDBUFF.GT.0) CALL ZGTREC ( IFLTAB, IDBUFF, NDBUFF, NADD,
     * .FALSE.)
C
      IFLTAB(KRBDBE) = NADD + NDBUFF
      IFLTAB(KRBDEN) = NADD + JDATA - 1
C
C     Write the message that the data was read
      IF (((MLEVEL.GE.4).AND.(IPLAN.NE.2)).OR.(MLEVEL.GT.5)) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT, 60) CPATH(1:NPATH)
 60   FORMAT(' --ZREAD:  ',A)
      ELSE
      WRITE ( MUNIT, 80) IFLTAB(KUNIT), INFO(NPPWRD+KIVER),
     * CPATH(1:NPATH)
 80   FORMAT(' -----DSS--- ZREAD Unit',I4,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
      ELSE
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,*)'--ZRDBUF:  Second Read'         D
C
C     Read the header block
      NHBUFF = IFLTAB(KRBHEN) - IFLTAB(KRBHBE) + 1
      IF (NHBUFF.GT.0) THEN
      NADD = IFLTAB(KRBHBE)
      IF (KHBUFF.LT.NHBUFF) NHBUFF = KHBUFF
      CALL ZGTREC ( IFLTAB, IHBUFF, NHBUFF, NADD, .FALSE.)
      IFLTAB(KRBHBE) = NADD + NHBUFF
      ENDIF
C
C     Read the data block
      NDBUFF = IFLTAB(KRBDEN) - IFLTAB(KRBDBE) + 1
      IF (NDBUFF.GT.0) THEN
      NADD = IFLTAB(KRBDBE)
      IF (KDBUFF.LT.NDBUFF) NDBUFF = KDBUFF
      CALL ZGTREC ( IFLTAB, IDBUFF, NDBUFF, NADD, .FALSE.)
      IFLTAB(KRBDBE) = NADD + NDBUFF
      ENDIF
C
      LFOUND = .TRUE.
C
      ENDIF
C
C
C     Has all the data been read?
      IF ((IFLTAB(KRBDBE).GT.IFLTAB(KRBDEN)).AND.
     *  (IFLTAB(KRBHBE).GT.IFLTAB(KRBHEN))) THEN
      LEND = .TRUE.
      IFLTAB(KRBNPA) = 0
      ELSE
      LEND = .FALSE.
      ENDIF
C
 800  CONTINUE
      LREAD = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) LFOUND                       D
 820  FORMAT (T6,'-----DSS--Debug: EXIT ZRDBUF',/,T14,                  D
     * 'FOUND = ',L1)                                                   D
C
      RETURN
      END
