      SUBROUTINE RAIN
C=======================================================================
C     Created June 1988 by Bob Dickinson (R.E.D.)
C     Last updated December, 1990 by R.E.D.
C     WCH, March 1993.  Add warning message for multiple stations.
C     WCH, August 1993. Add option to process new EarthInfo ASCII files
C       and to place 15-min. data on interface file at 15-min intervals,
C       not in the form of hourly totals.
C     WCH, 9/7/93.  Add option to bypass all statistical calculations
C       if IDECID = 0.
C     WCH, 11/12/93. Fix THISO-HISTO problem for IFORM = 3 and add
C       option for user-defined input for hours to range from 1-24 as
C       well as 0-23.
C     WCH, 4/22/94. Set units for depth for IFORM <= 1.
C     WCH, 4/25/94. Add to logic for recovery of 15-min. data.
C     WCH, 4/26/94. Provide input for optional treatment of accumulated
C       rainfall (NCDC code = A).
C     WCH, 4/26/94. Correct minor error for print of inches missing
C       data.
C     WCH, 4/26/94. Print indication of special rainfall codes in event
C       summary.
C     WCH, 5/25/94. Reset MFLAG parameter at end of each day during
C       event definition calcs.  Also fix KUNIT definition.
C     WCH, 10/15/94. Fix check for old HIST = new THISTO.  Prevented
C       use of multiple gages except for IFORM >= 9.
C     WCH, 2/27/95.  Typo in Format 1026.
C     WCH, 8/1/95.  Allow alphanumeric station IDs for AES precipitation
C       files (IFORM = 5 or 13).  Change involves converting integer
C       station IDs to character after input for other data.  This
C       change affects the Runoff and Statistics Blocks that read
C       the rainfall interface file.  Also,
C       read AES ID from AES data as A7 instead of I8 (Sub. GTRAIN).
C     WCH, 7/23/96.  Change heading for interevent time hours.  Open
C       NSCRAT1 as unformatted file if DOS name entered on @-line,
C       thus avoiding query of user for IFILE=1.  Add header for this
C       ASCII file.
C     WCH, 7/25/96.  Allow over-writing of existing rainfall interface
C       file.
C     WCH (Bruce LaZerte), 10/2/96.  Correct formats 2023, 2024.
C     WCH, 12/3/96.  Initialize some variables that change during run
C       in statements, not DATA statements.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'PRECIP.INC'
C=======================================================================
      INTEGER JK5(2),JSUM(2),IPRN(5,5),Y1(2),M1(2),D1(2),KSUM(2)
      INTEGER KEY(LSTORM,3),JK1(3),JK2(3),DURAT,MIT,DURLS,JK4(2),COND
C#### WCH, 8/1/95.
C####      INTEGER DRY,WET,PRCON,STORM,MSTRM,DSTRM,YSTRM,HSTRM,JSTA(10)
      INTEGER DRY,WET,PRCON,STORM,MSTRM,DSTRM,YSTRM,HSTRM
C
CIM INCREASE HYETOGRAPHS   ~~~~~~~~~~~~~~~~~~~
      REAL INTEN,STAT(5,8),TSUM(2),XOUT(500),WANE(MAXRG),WAX(MAXRG)
cim      REAL INTEN,STAT(5,8),TSUM(2),XOUT(500),WANE(10),WAX(10)
cim  ~~~~~~~~~~~~~~~~~~
cim  rainfall totals
      REAL SUMRAIN(MAXRG)
      CHARACTER  TITL(6)*10,NABRK(4)*6,VALUE(4)*6,VALMM(4)*6
      CHARACTER*60 NEWFIL
C#### WCH, 4/26/94.
      CHARACTER* 1 BLANK
C#### WCH, 8/1/95.  CHANGE STATION NAMES TO CHARACTER INTERNALLY.
CIM INCREASE HYETOGRAPHS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CHARACTER*8 JSTA(MAXRG)
cim      CHARACTER*8 JSTA(10)
cim ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C=======================================================================
      DATA BLANK/' '/
      DATA JK1/1,3,0/,JK2/1,1,0/,JK4/1,0/,JK5/2,0/,DRY/2/
      DATA WET/1/
      DATA VALUE/' Hours',' in/hr','Inches',' Hours'/
      DATA VALMM/' Hours',' mm/hr','Millim',' Hours'/
      DATA NABRK/'Month ','Year  ','Storm ','Avg yr'/
      DATA TITL/'Duration  ','Intensity ','Volume    ',
     1          'Delta     ','Years     ','Months    '/
C####      DATA JSUM/0,0/,XINT/0/,MHDATA/0/,MSDATA/0/,DURLS/0/
C####      DATA COND/2/,NDRY/0/,NDRHR/0/,VOLUM/0/,DURAT/0/,STORM/0/
C####      DATA NOYRS/0/,LOC11/0/
C=======================================================================
C#### WCH, 12/3/96.  INITIALIZE MOST VARIABLES HERE, NOT IN DATA STMTS.
C=======================================================================
      JK4(1)  = 1
      JK4(2)  = 0
      JSUM(1) = 0
      JSUM(2) = 0
      XINT    = 0.
      MHDATA  = 0
      MSDATA  = 0
      DURLS   = 0
      COND    = 2
      NDRY    = 0
      NDRHR   = 0
      VOLUM   = 0.
      DURAT   = 0
      STORM   = 0
      NOYRS   = 0
      LOC11   = 0
C#### WCH, 12/3/96.  ADD INITIALIZATION OF IGO.
      IGO     = 0
C
      MIT     = 1
      KUNIT   = 0
      METRIC  = 0
C#### WCH, 8/1/95.
      LENREC  = 0
C#### WCH, 4/26/94.  INITIALIZE NEW VARIABLE ACODE.
      DO 5 I = 1,366
      DO 5 J = 1,4
    5 ACODE(I,J) = BLANK
      INCNT  = INCNT + 1
      IOUTCT = IOUTCT + 1
      IO     = JIN(INCNT)
      JO     = JOUT(IOUTCT)
      ITEMP  = NSCRAT(2)
C#### WCH, 8/93.
      HIST   = 0.0
CCC      THISTO = 60.0
C=======================================================================
C     Open Rainfall file(s).
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='FORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='FORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C#### WCH, 7/23/96.  ALLOW USER TO ENTER NSCRAT1 FILE NAME WITH @-LINE.
      IF(JKP(51).EQ.1.AND.NSCRAT(1).GT.0.AND.FFNAME(51).NE.' ')
     +OPEN(NSCRAT(1),FILE=FFNAME(51),FORM='FORMATTED',STATUS='UNKNOWN')
C
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).NE.'SCRT2.UF') O
     +PEN(NSCRAT(2),FILE=FFNAME(52),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).EQ.'SCRT2.UF')
     +             OPEN(NSCRAT(2),FORM='UNFORMATTED',STATUS='SCRATCH')
      REWIND IO
C=======================================================================
C#### WCH, 8/1/95.  CHECK TO BE SURE INPUT FILE CONTAINS SOME DATA.
C     STRANGE ERRORS INVOLVING SYSTEM HANG-UPS MAY RESULT OTHERWISE.
C
C     CAUTION TO PROGRAMMERS.  OPTION "FLEN=" MAY BE SPECIFIC TO LAHEY
C     FORTRAN.
C=======================================================================
c     INQUIRE (UNIT=IO,FLEN=LENREC)
c     IF(LENREC.LE.0) THEN
c          WRITE (N6,965) IO
c          WRITE (*,965) IO
c          STOP
c          ENDIF
C=======================================================================
      WRITE(N6,1000)
      WRITE(*,1000)
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      WRITE(N6,68)          TITLE(1),TITLE(2)
C#######################################################################
C#### WCH, 4/26/94.
C     INSERT OPTIONAL DATA GROUP B0 FOR NEW PARAMETER KODEA.
C     KODEA   = 0, DON'T INCLUDE NCDC CUMULATIVE VALUES IN RAINFALL
C                  TIME SERIES.  HISTORICAL SWMM METHOD (DEFAULT).
C             = 1, AVERAGE CUMULATIVE VALUES (NCDC CODE = "A") OVER
C                  PRECEDING TIME PERIOD.
C             = 2, TREAT CUMULATIVE VALUE AS INSTANTANEOUS VALUE AT
C                  INDICATED TIME.
C     OPTIONS 1,2 ONLY FOR IFORM = 0,1,4,6, >=9.
C     KODEPRT = 0, DON'T PRINT INDICATION OF SPECIAL CODES FOR ALL
C                  DATES, ONLY FOR DATES OF EVENTS (DEFAULT).
C             = 1, PRINT INDICATOR IN EVENT SUMMARY FOR ANY DATE ON
C                  WHICH SPECIAL CODES ARE PRESENT.
C#### WCH, 7/25/96.  ALLOW OVERWRITING OF EXISTING INTERFACE FILE.
C     KOVER   = 0, ADD NEW RAINGAGE DATA TO EXISTING INTERFACE FILE.
C              =1, OVERWRITE EXISTING RAINFALL INTERFACE FILE, CREATING
C                  FILE CONTAINING ONLY THE DATA FOR CURRENT GAGE.
C=======================================================================
      KOVER  = 0
      KODEA  = 0
      KODEPR = 0
C=======================================================================
C >>>>>>>>>>>>> READ OPTIONAL DATA GROUP B0 <<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
C#### WCH, 7/25/96.  ADD KOVER
      IF(CC.EQ.'B0') READ(N5,*,ERR=888) CC,KODEA,KODEPR,KOVER
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP B1 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
C#### WCH, 8/1/95.
C     READ ALPHANUMERIC ISTA IF IFORM = 5 OR 13.
C     READ INTEGER IISTA OTHERWISE, AND CONVERT TO CHARACTER ISTA.
C     AFFECTS APPROXIMATELY NEXT 20 LINES.
C=======================================================================
      READ(N5,*,ERR=888) CC,IFORM
      IF(CC.NE.'B1') THEN
           CC = 'B1'
           GO TO 888
           ELSE
           BACKSPACE N5
           ENDIF
      IF(IFORM.NE.5.AND.IFORM.NE.13) READ(N5,*,ERR=888) CC,IFORM,
     1     IISTA,IDECID,JYBEG,JYEND,IYEAR,ISUM
      IF(IFORM.EQ.5.OR.IFORM.EQ.13)  READ(N5,*,ERR=888) CC,IFORM,
     1     ISTA,IDECID,JYBEG,JYEND,IYEAR,ISUM
      IF(IDECID.GT.0) THEN
                      BACKSPACE N5
                      IF(IFORM.NE.5.AND.IFORM.NE.13)
     +                   READ(N5,*,ERR=888) CC,IFORM,IISTA,IDECID,JYBEG,
     +                      JYEND,IYEAR,ISUM,MIT,NPTS,IFILE,A,NOSTAT
                      IF(IFORM.EQ.5.OR.IFORM.EQ.13)
     +                   READ(N5,*,ERR=888) CC,IFORM,ISTA,IDECID,JYBEG,
     +                      JYEND,IYEAR,ISUM,MIT,NPTS,IFILE,A,NOSTAT
                      ENDIF
      IF(IDECID.EQ.2) THEN
                      JO   = 0
                      NSTA = 1
                      ENDIF
      IYBEG(1) = JYBEG/10000
      IF((JYBEG.NE.0).AND.(IYBEG(1).LT.100)) THEN
      JYBEG = JYBEG - IYBEG(1)*10000
      IYBEG(1) = IYBEG(1)+1900
      JYBEG = JYBEG + IYBEG(1)*10000
      ENDIF
      IYBEG(2) = (JYBEG - IYBEG(1)*10000)/100
      IYEND(1) = JYEND/10000
      IF((JYEND.NE.0).AND.(IYEND(1).LT.100)) THEN
      JYEND = JYEND - IYEND(1)*10000
      IYEND(1) = IYEND(1)+1900
      JYEND = JYEND + IYEND(1)*10000
      ENDIF
      IYEND(2) = (JYEND - IYEND(1)*10000)/100
      IYEND(3) = JYEND - IYEND(1)*10000 - IYEND(2)*100
      IYBEG(3) = JYBEG - IYBEG(1)*10000 - IYBEG(2)*100
      IF(IYBEG(2).EQ.0) IYBEG(2) =  1
      IF(IYBEG(3).EQ.0) IYBEG(3) =  1
      IF(IYEND(2).EQ.0) IYEND(2) = 12
      IF(IYEND(3).EQ.0) IYEND(3) = 31
C=======================================================================
C#### WCH, 8/1/95.  CONVERT INTEGER IISTA TO CHARACTER ISTA.
C     AMAZING (TO WCH) CONVERSION METHOD IS WRITE TO "INTERNAL FILES".
C     SEE LAHEY LANGUAGE REFERENCE, SECTION 9.2.1
C=======================================================================
      IF(IFORM.NE.5.AND.IFORM.NE.13) WRITE(ISTA,'(I8)') IISTA
C=======================================================================
C#### WCH, 8/1/95.  REMEMBER, FROM HERE ON, ISTA AND JSTA() ARE
C     CHARACTER VARIABLES.
C=======================================================================
      IF(IDECID.GT.0) WRITE(N6,1021) ISTA,IYBEG,IYEND,MIT,NPTS,IFORM,
     +                ISUM,IYEAR,IFILE,IDECID,A,NOSTAT
      IF(IDECID.EQ.0) WRITE(N6,1022) ISTA,IYBEG,IYEND,IFORM,
     +                                    ISUM,IYEAR,IDECID
C#### WCH, 4/26/94.
      WRITE(N6,1025) KODEA
      WRITE(N6,1026) KODEPR
C#### WCH, 7/25/96.
      WRITE(N6,1028) KOVER
C#### WCH, 8/1/95. ADD NOTE FOR IFORM=13.
      IF(IFORM.EQ.13) WRITE(N6,1027)
C=======================================================================
C     READ THE NUMBER OF STATIONS ON JO FILE
C          IF THE FILE IS EMPTY STATEMENT 123 WILL BE CALLED
C=======================================================================
      IF(IDECID.LE.1) THEN
                      IF(JO.EQ.0) CALL ERROR(111)
                      MSTA   = 0
                      READ(JO,END=123,ERR=123) NSTA,MRAIN,
     +                                         (JSTA(I),I=1,NSTA)
C#######################################################################
C#### WCH, 7/25/96.  ALLOW USER TO OVERWRITE EXISTING FILE WITH NEW
C     GAGE DATA.
C#######################################################################
                      IF(KOVER.EQ.1) THEN
                           WRITE (N6,2121) JO,NSTA
                           GOTO 123
                           ENDIF
                      MSTA   = NSTA
CIM INCREASE HYETOGRAPHS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      IF(NSTA.EQ.MAXRG) CALL ERROR(113)
                      DO J = 1, MAXRG
                      SUMRAIN(J) = 0.0
                      ENDDO
cim                      IF(NSTA.EQ.10) CALL ERROR(113)
cim ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                      NSTA =  NSTA + 1
                      WRITE(N6,2115)  NSTA
C#######################################################################
C  WCH, 3/3/93  WRITE CAUTION MESSAGE WHEN NSTA > 1
C#######################################################################
                      IF(NSTA.GT.1) WRITE(N6,2116) JO, JO
                      IF(NSTA.GT.1) WRITE(*,2116)  JO, JO
                      JSTA(NSTA)  =   ISTA
                      MRAIN       =   1000000
                      WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
C=======================================================================
C                     Read and save JO on NSCRAT(2) scratch file.
C=======================================================================
                      IF(ITEMP.EQ.0) CALL ERROR(114)
                      REWIND ITEMP
                      DO 2110 I = 1,1000000
                      READ(JO,END=2112) JULDAY,RHOUR,
     +                                  THISTO,(WANE(J),J=1,MSTA)
C#### WCH, 8/93. CHECK FOR COMPATIBILITY OF TIME INTERVALS.
C#### WCH, 10/25/94.  DO THIS ONLY FOR IFORM >= 9.  CHECK OCCURS
C     LATER FOR IFORM = 3 AND IN SUB. GTRAIN FOR OTHER IFORM VALUES.
C####                      IF(I.EQ.1) THEN
C
C#### WCH, 8/1/95. SKIP THIS CHECK FOR NEW IFORM=13.
                      IF(IFORM.EQ.13) GO TO 2108
C
                      IF(I.EQ.1.AND.IFORM.GE.9) THEN
                         IF(IFORM.EQ.9.OR.IFORM.EQ.11) THEN
                            IF(ABS(THISTO-900.).LT.0.1) GO TO 2108
                            HIST = 900.0
                            GO TO 2106
                            ENDIF
                         IF(IFORM.EQ.10.OR.IFORM.EQ.12) THEN
                            IF(ABS(THISTO-3600.).GT.0.1) GO TO 2108
                            HIST = 3600.0
                            ENDIF
 2106                    WRITE(N6,9110) HIST,THISTO
                         WRITE(*,9110)  HIST,THISTO
                         STOP
                         ENDIF
 2108                 WRITE(ITEMP)      JULDAY,RHOUR,
     +                                  THISTO,(WANE(J),J=1,MSTA)
 2110                 CONTINUE
 2112                 CONTINUE
                      REWIND JO
                      REWIND ITEMP
                      MRAIN = 1000000
                      WRITE(JO) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
C#### WCH, 8/93.
                      HIST = THISTO
                      GO TO 124
C=======================================================================
C      Here, if single rain gage.
C=======================================================================
  123                                NSTA = 1
                      WRITE(N6,2115) NSTA
                      WRITE(N6,2120) NSTA,ISTA
                      JSTA(1) = ISTA
                      REWIND JO
                      MRAIN = 1000000
                      WRITE (JO) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
  124                 CONTINUE
                      ENDIF
C=======================================================================
C     Check file existence of NSCRAT(1).
C=======================================================================
      IF(IFILE.EQ.1) THEN
                     NOUT  = NSCRAT(1)
                     CALL CKFILE(NOUT)
                     ENDIF
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP B2 <<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(IFORM.EQ.3) THEN
                     READ(N5,*,ERR=888) CC,THISTO,METRIC,KUNIT,FIRMAT,
     +                         CONV,F1,F2,F3,F4,F5,F6,F7
C#### WCH, 11/12/93
                     IF(CONV.LT.0.0) THEN
                          IHH = 1
                          CONV = -CONV
                          ELSE
                          IHH = 0
                          ENDIF
                     WRITE(N6,1030) FIRMAT,METRIC,KUNIT,CONV,
     +                              THISTO,F1,F2,F3,F4,F5,F6,F7
C#### WCH, 11/12/93
                     IF(IHH.EQ.0) WRITE (N6,1035)
                     IF(IHH.EQ.1) WRITE (N6,1036)
                     THISTO = THISTO*60.0
C#### WCH, 8/93
                     IF(HIST.GT.0.0.AND.ABS(HIST-THISTO).GT.0.1) THEN
                        WRITE(N6,9110) THISTO,HIST
                        WRITE(*,9110)  THISTO,HIST
                        STOP
                        ENDIF
C#### WCH, 11/12/93
                     HIST = THISTO
C=======================================================================
C >>>>>>>>>>>>>> READ DATA GROUP B3 <<<<<<<<<<<<<<<<<<<<
C=======================================================================
                     READ(N5,*,ERR=888) CC
                     BACKSPACE N5
                     NUVAL = 1
                     IF(CC.EQ.'B3') READ(N5,*,ERR=888) CC,NUVAL
                     ENDIF
C=======================================================================
C#### WCH, 8/1/95.  ADD IFORM=13.
      IF(IFORM.EQ.5.OR.IFORM.EQ.13) METRIC = 1
                                    METRIC = METRIC + 1
C#### WCH, 8/93.
CCC      THISTO = THISTO * 60.0
      NEND = 24
      IF(CONV.EQ.0.0)   CONV = 1.0
      XCONV  = CONV
C#### WCH, 4/22/94.  SET UNITS FOR DEPTH FOR IFORM <= 1.
C#### WCH, 5/25/94.  ALSO FOR IFORM = 4 OR 6.
      IF(IFORM.LE.1.OR.IFORM.EQ.4.OR.IFORM.EQ.6) KUNIT = 1
C#######################################################################
C     WCH, 8/93.  Add option for recent (1993) EarthInfo ASCII hourly
C       and 15-min precipitation files.
C     Values of IFORM:
C                Unprocessed  Processed
C     15-min.        9            11
C     Hourly        10            12
C#######################################################################
C#### WCH, 8/1/95.  ADD CHECK FOR IFORM = 13.
      IF(IFORM.GE.9.AND.IFORM.LT.13) THEN
         IF(IFORM.EQ.9.OR.IFORM.EQ.10) THEN
            M3 = NSCRAT(3)
            IF(M3.EQ.0) CALL ERROR(104)
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).NE.'SCRT3.UF')
     +      OPEN(M3,FILE=FFNAME(53),FORM='FORMATTED',STATUS='UNKNOWN')
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +      OPEN(M3,FORM='FORMATTED',STATUS='SCRATCH')
            ENDIF
         IF(IFORM.EQ.9.OR.IFORM.EQ.11) THEN
            M4 = NSCRAT(4)
            IF(M4.EQ.0) CALL ERROR(155)
            OPEN(M4,FORM='UNFORMATTED',STATUS='SCRATCH')
            NEND = 96
            ENDIF
         CALL G9RAIN(0,IGO,ILOST,M3,M4)
         KUNIT = 1
         ENDIF
C=======================================================================
      IF(IFORM.EQ.3) CALL G3RAIN(0,IGO,ILOST)
      IF(IFORM.LT.9.AND.IFORM.NE.3) CALL GTRAIN(0,IGO,ILOST,M4)
C#### WCH, 8/1/95.
      IF(IFORM.EQ.13) CALL GTRAIN(0,IGO,ILOST,M4)
C#######################################################################
C     WCH, 8/93.  FOR 15-MIN. DATA, NEED TO USE NSCRAT(4) FOR TEMPORARY
C       STORAGE OF ONE YEAR'S DATA.
C=======================================================================
C#### WCH, 8/1/95
      IF((IFORM.LT.9.OR.IFORM.EQ.13).AND.ABS(HIST-900.0).LT.0.1) THEN
            M4 = NSCRAT(4)
            IF(M4.EQ.0) CALL ERROR(155)
            OPEN(M4,FORM='UNFORMATTED',STATUS='SCRATCH')
            NEND = 96
            ENDIF
      THISTO = HIST
      IF(NEND.EQ.96) WRITE(N6,2027)
C=======================================================================
C     END OF YEAR OR END OF FILE-COMPUTE SYNOPTIC DATA
C     FIND NUMBER OF DAYS IN THIS YEAR
C     SAVE LAST STORM NUMBER AND PRESENT FILE NUMBER
C=======================================================================
C      write to 6 to get carriage control
    7 WRITE(6,911) NEWYR
      CONV  = XCONV
      IF(IFORM.NE.3.AND.IFORM.LT.9) CALL GTRAIN(1,IGO,ILOST,M4)
      IF(IFORM.EQ.3) CALL G3RAIN(1,IGO,ILOST)
C#### WCH, 8/1/95.
      IF(IFORM.EQ.13) CALL GTRAIN(1,IGO,ILOST,M4)
      IF(IFORM.GE.9.AND.IFORM.LT.13) CALL G9RAIN(1,IGO,ILOST,M3,M4)
C
      IF(ILOST.EQ.0) GO TO 7
C#### WCH, 8/93
      IF(NEND.EQ.96) REWIND M4
      NDAYR = KDATE(0,1,NEWYR+1)-KDATE(0,1,NEWYR)
      MFLAG = 0
      NOLDS = STORM
      NDRSA = NDRY
      NOYRS = NOYRS + 1
                                     CONV = 0.01
      IF(IFORM.EQ.3.AND.METRIC.EQ.1) CONV = 0.001
      IF(IFORM.EQ.3.AND.METRIC.EQ.2) CONV = 0.01
C#### WCH, 8/1/95.
      IF(IFORM.EQ.5.OR.IFORM.EQ.13)  CONV = 0.1
C=======================================================================
C     Write interface file for NWS precipitation data.
C     Must use this IF-loop for 15-min. data, except for user-defined.
C=======================================================================
C#### WCH, 8/1/95.  ADD CHECK FOR IFORM=13.
      IF(IFORM.LT.3.OR.IFORM.EQ.4.OR.(IFORM.GE.6.AND.IFORM.LT.13)) THEN
           IREAD   = 0
           DO 70 J = 1,NDAYR
           JULDAY  = NEWYR*1000 + J
C#######################################################################
C     WCH, 8/93.  SEVERAL CHANGES FOR 15-MIN RAINFALL
C#######################################################################
           M4READ  = 0
           DO 71 K = 1,NEND
           KK = K
           IF(NEND.EQ.96) KK = (K-1)/4 + 1
           IF(HOUR(J,KK).LE.0) GO TO 71
           IF(NEND.EQ.24) THEN
              REIN  = FLOAT(HOUR(J,KK))*CONV
              RHOUR = FLOAT(KK-1)*3600.0
              WANE(NSTA) = REIN
              ELSE
C=======================================================================
C     Here, for 15-min. data, compute hour and time and read yearly
C       data from file M4.
C=======================================================================
              RHOUR = FLOAT(K-1)*THISTO
              REIN       = 0.0
              WANE(NSTA) = 0.0
              IF(M4READ.EQ.0) THEN
 3900              READ(M4,END=3940,ERR=950,IOSTAT=IOS)
     1                         JLDAY,SHOUR,THIST,REIN
C#### WCH, 4/25/94.  SPEED UP 15-MIN RAINFALL RECOVERY?
                   IF(JLDAY.EQ.JULDAY.AND.ABS(RHOUR-SHOUR).LT.0.1)
     1                                               GO TO 3910
                   IF(JLDAY.LT.JULDAY) GO TO 3900
                   IF(JLDAY.EQ.JULDAY.AND.(RHOUR-SHOUR).GT.0.1)
     1                                               GO TO 3900
                   IF(JLDAY.GT.JULDAY.OR.(JLDAY.EQ.JULDAY.AND.
     1                    (SHOUR-RHOUR).GT.0.1)) THEN
                      REIN = 0.0
                      BACKSPACE M4
                      ENDIF
C#### WCH, 4/25/94.  ADD STATMENT 3910.
 3910              WANE(NSTA) = REIN
                   ELSE
                   GO TO 3950
                   ENDIF
              ENDIF
           GO TO 3950
C=======================================================================
C     If reach end of file on M4, set M4READ=1, don't read any more and
C       end this year's rain data at end of day.
C=======================================================================
 3940      M4READ = 1
C=======================================================================
 3950      IF(JO.GT.0)    THEN
              IF(MSTA.GT.0)  THEN
 4000            IF(IREAD.EQ.0) READ(ITEMP,END=4010) JDAY,QHOUR,
     +                                    THIS,(WAX(I),I=1,MSTA)
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.EQ.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = REIN
                    WRITE(JO)  JULDAY,RHOUR,THISTO,(WAX(I),I=1,NSTA)
                    GO TO 71
                    ENDIF
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.LT.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 4000
                    ENDIF
                 IF(JDAY.GE.JULDAY) THEN
                    IREAD     = 1
                    DO 4020 I = 1,MSTA
 4020               WANE(I)   = 0.0
                    WRITE(JO) JULDAY,RHOUR,THISTO,(WANE(I),I=1,NSTA)
                    GO TO 71
                    ENDIF
                 IF(JDAY.LE.JULDAY) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 4000
                    ENDIF
 4010            JDAY  = 9999999
                 QHOUR = 0.0
                 IREAD = 1
                 GO TO 4000
C=======================================================================
C     Here, just one precip. station (MSTA = 0).
C=======================================================================
                 ELSE
                 WRITE(JO) JULDAY,RHOUR,THISTO,REIN
                 ENDIF
C=======================================================================
C     ENDIF for JO > 0
C=======================================================================
              ENDIF
   71      CONTINUE
           IF(M4READ.EQ.1) GO TO 701
   70      CONTINUE
  701      CONTINUE
           ENDIF
C=======================================================================
C     User defined rainfall.
C=======================================================================
      IF(IFORM.EQ.3) THEN
           IREAD   = 0
c     write(*,*) NSTORM
c     read(*,*) junk
           DO 80 J = 1,NSTORM
           JULDAY  = NEWYR*1000 + RDAY(J)
           REIN    = FLOAT(RRAIN(J))*CONV
           IF(KUNIT.EQ.1)  REIN = REIN*3600.0/THISTO
           RHOUR                = RTIME(J)*3600.0
           WANE(NSTA)           = REIN
           sumrain(nsta) = sumrain(nsta) + rein*THISTO/3600.0
           IF (RDAY(J).LT.999990) THEN
           LDAY1 = JULDAY
           HLAST1 = RHOUR
           ENDIF
C=======================================================================
C    NOTE, PRECIP. ON INTERFACE FILE IS VALUE OF REIN.
C    PRECIP. USED FOR STATISTICS IS VALUE OF HOUR().
C=======================================================================
           IF(JO.GT.0)    THEN
              IF(MSTA.GT.0)  THEN
 5000            IF(IREAD.EQ.0) THEN
                     READ(ITEMP,END=5010) JDAY,QHOUR,
     +                                     THIS,(WAX(I),I=1,MSTA)
                     LDAY2 = JDAY
                     HLAST2 = QHOUR
                 ENDIF
c          write(*,*) j,nstorm
c          write(*,*) jday,qhour,this,thisto,'-'
c          write(*,*) julday,rhour
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.EQ.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = REIN
                    WRITE(JO)  JULDAY,RHOUR,THISTO,(WAX(I),I=1,NSTA)
                     do i = 1, msta
                     sumrain(i) = sumrain(i) + wax(i)*THISTO/3600.0
                     enddo
                    GO TO 80
                    ENDIF
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.LT.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 5000
                    ENDIF
                 IF(JDAY.GE.JULDAY) THEN
                    IREAD     = 1
                    DO 5020 I = 1,MSTA
 5020               WANE(I)   = 0.0
                    IF (RDAY(J).LT.999990)
     +              WRITE(JO) JULDAY,RHOUR,THISTO,(WANE(I),I=1,NSTA)
                    GO TO 80
                    ENDIF
                 IF(JDAY.LT.JULDAY) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 5000
                    ENDIF
 5010            JDAY  = 9999999
                 QHOUR = 0.0
                 IREAD = 1
                 GO TO 5000
                 ELSE
                    IF (RDAY(J).LT.999990)
     1            WRITE(JO) JULDAY,RHOUR,THISTO,REIN
                 ENDIF
              ENDIF
   80      CONTINUE
           ENDIF
C write last day and total rain for IFORM=3 only
	IF(IFORM.EQ.3) THEN
           WRITE(N6,8000) LDAY1,HLAST1/60.0/60.0
           IF (MSTA.GT.0) WRITE(N6,8010) LDAY2,HLAST2/60.0/60.0
           WRITE(N6,8020)
           DO I =1, NSTA
           WRITE(N6,8030) I,SUMRAIN(I)
           SUMRAIN(I) = 0.0
           ENDDO
	ENDIF
C=======================================================================
C     AES Canadian rainfall.
C=======================================================================
C#### WCH, 8/1/95
      IF(IFORM.EQ.5.OR.IFORM.EQ.13) THEN
           IREAD   = 0
           DO 85 J = 1,NDAYR
           JULDAY  = NEWYR*1000 + J
           DO 86 K = 1,24
           IF(HOUR(J,K).LE.0) GO TO 86
           REIN  = FLOAT(HOUR(J,K))*CONV
           KK    = K - 1
           RHOUR = FLOAT(KK)*3600.0
           WANE(NSTA)           = REIN
C=======================================================================
           IF(JO.GT.0)    THEN
              IF(MSTA.GT.0)  THEN
 6000            IF(IREAD.EQ.0) READ(ITEMP,END=6010) JDAY,QHOUR,
     +                                  THIS,(WAX(I),I=1,MSTA)
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.EQ.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = REIN
                    WRITE(JO)  JULDAY,RHOUR,THISTO,(WAX(I),I=1,NSTA)
                    GO TO 86
                    ENDIF
                 IF(JDAY.EQ.JULDAY.AND.QHOUR.LT.RHOUR) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 6000
                    ENDIF
                 IF(JDAY.GE.JULDAY) THEN
                    IREAD     = 1
                    DO 6020 I = 1,MSTA
 6020               WANE(I)   = 0.0
                    WRITE(JO) JULDAY,RHOUR,THISTO,(WANE(I),I=1,NSTA)
                    GO TO 86
                    ENDIF
                 IF(JDAY.LT.JULDAY) THEN
                    IREAD     = 0
                    WAX(NSTA) = 0.0
                    WRITE(JO) JDAY,QHOUR,THIS,(WAX(I),I=1,NSTA)
                    GO TO 6000
                    ENDIF
 6010            JDAY  = 9999999
                 QHOUR = 0.0
                 IREAD = 1
                 GO TO 6000
                 ELSE
                 WRITE(JO) JULDAY,RHOUR,THISTO,REIN
                 ENDIF
              ENDIF
86         CONTINUE
85         CONTINUE
           ENDIF
C=======================================================================
C=======================================================================
C     Write yearly summary.
C=======================================================================
C=======================================================================
      IF(IYEAR.EQ.1) THEN
                     IF(IFORM.EQ.3) THEN
                                    WRITE(N6,975) TITLE(1),TITLE(2)
C#### WCH, 8/1/95.
                                    ELSE IF(IFORM.EQ.5.OR.IFORM.EQ.13)
     +                                  THEN
                                        WRITE(N6,971) TITLE(1),TITLE(2)
                                    ELSE
                                    WRITE(N6,970) TITLE(1),TITLE(2)
                                    ENDIF
                     KW = 0
                     II = 0
                     DO 81  J = 1,NDAYR
                     DO 82  K = 1,24
                     IF(HOUR(J,K).LE.0) GO TO 82
                     KW = KW + 1
                     II = II + 1
                     IPRN(KW,1) = HOUR(J,25)
                     IPRN(KW,2) = HOUR(J,26)
                     IPRN(KW,3) = HOUR(J,27)
                     IPRN(KW,4) = K - 1
                     IPRN(KW,5) = HOUR(J,K)
                     IF(MOD(II,220).EQ.0) THEN
                        IF(IFORM.EQ.3) WRITE(N6,975) TITLE(1),TITLE(2)
C#### WCH, 8/1/95.
                        IF(IFORM.EQ.5.OR.IFORM.EQ.13) WRITE(N6,971)
     +                                            TITLE(1),TITLE(2)
                        IF(IFORM.NE.3.AND.IFORM.NE.5.AND.IFORM.NE.13)
     +                              WRITE(N6,970) TITLE(1),TITLE(2)
                        ENDIF
                     IF(MOD(KW,4).EQ.0) THEN
                            WRITE(N6,980) ((IPRN(KK,JJ),JJ=1,5),KK=1,4)
                            KW = 0
                            ENDIF
82                   CONTINUE
81                   CONTINUE
                     IF(KW.GT.0) WRITE(N6,980)
     +                          ((IPRN(KK,JJ),JJ=1,5),KK=1,KW)
                     ENDIF
C#######################################################################
C     WCH, 9/7/93.
C     Skip synoptic analysis if IDECID = 0.
C=======================================================================
      IF(IDECID.EQ.0) THEN
           NEWYR = NEWYR + 1
           GO TO 115
           ENDIF
C=======================================================================
C     End of year printouts.
C=======================================================================
      IF(ISUM.EQ.1.AND.METRIC.EQ.1) WRITE(N6,2030) TITLE(1),TITLE(2)
      IF(ISUM.EQ.1.AND.METRIC.EQ.2) WRITE(N6,2035) TITLE(1),TITLE(2)
C=======================================================================
C     PRESENT CONDITION IS DETERMINED IF ANY PRECIPITATION IS
C     RECORDED FOR THIS HOUR
C     PREVIOUS CONDITION AND PRESENT CONDITION DETERMINE CURRENT STATUS
C     PRCON  AND  COND  CAN BE EITHER 1 OR 2.  THUS  NEXT
C     WHICH DEFINES THE CURRENT STATUS CAN BE EITHER 1,2,3, OR 4
C     AS INDICATED IN THE FOLLOWING TABLE
C
C                          PREVIOUS CONDITION
C                             WET       DRY
C                          (COND=1)  (COND=2)
C     PRESENT       WET
C     CONDITION  (PRCON=1)     1         3
C
C                   DRY        2         4
C                (PRCON=2)
C
C     IN CODE, DRY = 2, WET = 1 (INTEGER VARIABLES).
C=======================================================================
C     NOTE, ALL STATISTICAL AND SUMMARY CALCULATIONS ARE DONE USING
C       HOURLY DATA.  WHEN RAW DATA ARE AT 15-MIN. INTERVALS, HOURLY
C       VALUES REPRESENT THE SUM FOR THE HOUR.
C=======================================================================
      II       = 0
      TIMDAY   = 0.0
      DO 100 J = 1,NDAYR
      JULDAY   = NEWYR*1000 + J
      CALL DATED
C#### WCH, 4/26/94.  JXP = INDICATOR VARIABLE FOR PRINT-OUTS.
      JXP = 0
      DO 90  K = 1,24
      IF(MFLAG.GE.1)     GO TO 46
      IF(HOUR(J,K).GE.0) GO TO 48
      IF(IFORM.EQ.2) THEN
                     IF(HOUR(J,K).EQ.-1) MHDATA = MHDATA + 1
                     IF(HOUR(J,K).EQ.-2) MSDATA = MSDATA + 1
                     GO TO 90
                     ENDIF
C#### WCH, 8/1/95.
      IF(IFORM.EQ.5.OR.IFORM.EQ.13) THEN
                     IF(HOUR(J,K).EQ.-1) MHDATA = MHDATA + 1
                     IF(HOUR(J,K).EQ.-2) MSDATA = MSDATA + 1
                     GO TO 90
                     ENDIF
C#### WCH, 4/25/94.  CAN HAVE -1 AND -2 FLAGS FOR IFORM = 1 ALSO.
C     CHANGE IF-STMT TO ALLOW CHECK FOR THESE CASES ALSO.
C####  46  IF(IFORM.EQ.0.OR.IFORM.EQ.4.OR.IFORM.GE.6) THEN
  46  IF(IFORM.LE.1.OR.IFORM.EQ.4.OR.IFORM.GE.6) THEN
                     IF(MFLAG.EQ.0) THEN
                         IF(HOUR(J,K).EQ.-1) THEN
                                             MHDATA = MHDATA + 1
                                             ELSE
                                             MSDATA = MSDATA + 1
                                             ENDIF
                         ENDIF
                     IF(MSDATA.GT.0) MSDATA = MSDATA + 1
                     IF(MHDATA.GT.0) MHDATA = MHDATA + 1
                     MFLAG = MFLAG +1
                     IF(HOUR(J,K).LE.-1.AND.MFLAG.GT.1) MFLAG = 0
                     GO TO 90
                     ENDIF
  48                       PRCON = 2
      IF(HOUR(J,K) .GT. 0) PRCON = 1
      NEXT = (COND-1)*2+PRCON
C=======================================================================
C     Continue storm (NEXT=1)
C=======================================================================
      IF(NEXT.EQ.1) THEN
                    DURAT = DURAT + 1 + NDRHR
                    NDRHR = 0
                    IF(HOUR(J,K).GT.0.AND.KUNIT.EQ.0) VOLUM =
     +                 VOLUM + FLOAT(HOUR(J,K))*CONV*THISTO/3600.0
                    IF(HOUR(J,K).GT.0.AND.KUNIT.EQ.1) VOLUM =
     +                 VOLUM + FLOAT(HOUR(J,K))*CONV
                    IF(HOUR(J,K).GT.XINT) XINT = HOUR(J,K)
                    ENDIF
C=======================================================================
C     DRY PERIOD (NEXT=2)-CHECK IF NOL OF CONSECUTIVE DRY HOURS
C     IS SUFFICIENT TO BE CONSIDERED AS END OF STORM
C=======================================================================
      IF(NEXT.EQ.2) THEN
                    NDRHR = NDRHR + 1
                    IF(NDRHR.LT.MIT) GO TO 90
                    STORM = STORM+1
                    IF(STORM.EQ.1) DURLS = DURAT
                    DELTA = 0.5*(DURAT+DURLS)+FLOAT(NDRY)
C=======================================================================
C                   CHECK IF DELTA IS TO BE IGNORED
C=======================================================================
                    IF(STORM.EQ.1)                 GO TO 61
                    IF(MHDATA.GT.0.OR.MSDATA.GT.0) GO TO 61
                    GO TO 62
   61               DELTA = 0.0
                    DH0   = 1.0E10
   62               CONTINUE
                    INTEN = VOLUM/FLOAT(DURAT)
                    LOC11 = LOC11 + 1
                    IF(LOC11.GT.LSTORM) THEN
                                        WRITE(N6,9101) LSTORM
                                        GO TO 101
                                        ENDIF
                    KEY(LOC11,1) = MSTRM
                    KEY(LOC11,2) = YSTRM
                    KEY(LOC11,3) = LOC11
                    X1(LOC11,1)  = DURAT
                    X1(LOC11,2)  = INTEN
                    X1(LOC11,3)  = FLOAT(JDAY)
                    X2(LOC11,1)  = VOLUM
                    X2(LOC11,2)  = DELTA
                    X2(LOC11,3)  = FLOAT(JDAY)
                    XDUR         = DURAT
                    DH0          = DH0+DELTA-0.5*DURAT
                    ZINT         = XINT*CONV
C=======================================================================
C                   Create an ASCII interface file with storm data.
C=======================================================================
                    IF(IFILE.EQ.1) THEN
C#### WCH, 7/23/96.  ADD HEADER TO ASCII FILE.
                        IF(STORM.EQ.1) THEN
                             IF(METRIC.EQ.1) WRITE(NOUT,2023) MIT
                             IF(METRIC.EQ.2) WRITE(NOUT,2024) MIT
                             ENDIF
                        DH1 = DH0
                        IF(DH0.GE.1E06) DH1 = 0.0
                        WRITE(NOUT,2025) ISTA,STORM,MSTRM,DSTRM,YSTRM,
     *                  HSTRM,XDUR,VOLUM,INTEN,ZINT,DH1,MHDATA,MSDATA
                        ENDIF
C=======================================================================
C#### WCH, 4/26/94.  HERE, INCLUDE PRINT OF SPECIAL CODE INDICATOR
                    IXY = JDATE(DSTRM,MSTRM,YSTRM)
                    IF(ISUM.EQ.1)  THEN
                       JXP = 1
                       IF(DH0.LT.1E06) THEN
                          WRITE(N6,2020) STORM,MSTRM,DSTRM,YSTRM,HSTRM,
     * XDUR,VOLUM,INTEN,ZINT,DH0,MHDATA,MSDATA,(ACODE(IXY,JXY),JXY=1,4)
                          ELSE
                          WRITE(N6,2021) STORM,MSTRM,DSTRM,YSTRM,HSTRM,
     * XDUR,VOLUM,INTEN,ZINT,MHDATA,MSDATA,(ACODE(IXY,JXY),JXY=1,4)
                          ENDIF
                       II = II + 1
                       IF(MOD(II,50).EQ.0) THEN
                        IF(METRIC.EQ.1) WRITE(N6,2030) TITLE(1),TITLE(2)
                        IF(METRIC.EQ.2) WRITE(N6,2035) TITLE(1),TITLE(2)
                        ENDIF
                       ENDIF
C=======================================================================
                    JSUM(1) = JSUM(1) + MHDATA
                    JSUM(2) = JSUM(2) + MSDATA
                    SUM(2)  = SUM(2)  + VOLUM
                    XINT    = 0.0
                    MHDATA  = 0
                    MSDATA  = 0
                    DH0     = -0.5*DURAT
                    NDRY    = NDRHR
                    NDRHR   = 0
                    COND    = DRY
                    DURLS   = DURAT
                    ENDIF
C=======================================================================
C     Beginning of storm  (NEXT=3)
C=======================================================================
      IF(NEXT.EQ.3) THEN
                    MSTRM = HOUR(J,25)
                    DSTRM = HOUR(J,26)
                    YSTRM = HOUR(J,27)
                    JDAY  = JULDAY
                    HSTRM = K - 1
                    DURAT = 1
                    VOLUM = 0.0
                    IF(HOUR(J,K).GT.0.AND.KUNIT.EQ.0) VOLUM =
     +                 VOLUM + FLOAT(HOUR(J,K))*CONV*THISTO/3600.0
                    IF(HOUR(J,K).GT.0.AND.KUNIT.EQ.1) VOLUM =
     +                 VOLUM + FLOAT(HOUR(J,K))*CONV
                    IF(HOUR(J,K).GT.XINT) XINT = HOUR(J,K)
                    NDRHR = 0
                    COND  = WET
                    ENDIF
C=======================================================================
C     Continuation of dry weather (NEXT=4).
C=======================================================================
      IF(NEXT.EQ.4) NDRY = NDRY+1
   90 CONTINUE
C#######################################################################
C#### WCH, 4/26/94.  PRINT SPECIAL CODES INDICATOR IF DESIRED.
C=======================================================================
      IF(NEXT.NE.2.AND.KODEPR.EQ.1.AND.JXP.EQ.0) THEN
          IF(ACODE(J,1).NE.BLANK)
     1       WRITE(N6,2022) MONTH,NDAY,NEWYR,(ACODE(J,JXY),JXY=1,4)
          JXP = 0
          ENDIF
C#### WCH, 5/25/94.  RESET MFLAG AT END OF EACH DAY TO AVOID CARRY-OVER.
      MFLAG = 0
  100 CONTINUE
C=======================================================================
C     Update for next year.
C     Revert to saved storm and record number if less than 2 storms.
C=======================================================================
      NEWYR = NEWYR+1
      IF(STORM-NOLDS.LT.2) THEN
                           NDRY  = NDRSA
                           STORM = NOLDS
                           ENDIF
      RE = FLOAT(JSUM(1))/(FLOAT(NDAYR)*24.0)*SUM(2)
C#### WCH, 4/26/94.  DIVIDE SUM(1) BY 100 TO GET INCHES.
      IF(ISUM.EQ.1.AND.METRIC.EQ.1) WRITE(N6,990) SUM(2),JSUM(1),
     1   JSUM(2),RE,SUM(1)/100.
      IF(ISUM.EQ.1.AND.METRIC.EQ.2) WRITE(N6,990) SUM(2),JSUM(1),
     1   JSUM(2),RE,SUM(1)
      DO 110  K = 1,2
      TSUM(K)   = SUM(K)  + TSUM(K)
      KSUM(K)   = JSUM(K) + KSUM(K)
      JSUM(K)   = 0
  110 SUM(K)    = 0
C=======================================================================
C     Continue reading/or/start writing.
C=======================================================================
C#### WCH, 9/7/93
  115 IF(IYEND(1).GT.0.0.AND.NEWYR.GT.IYEND(1)) GO TO 49
      IF(IGO.EQ.0) GO TO 7
C=======================================================================
C     If IFORM = 4 or IFORM = 6 a new file may be opened.
C=======================================================================
   49 CONTINUE
      IF(IFORM.EQ.4.OR.IFORM.EQ.6) THEN
                    READ(N5,*,ERR=888) CC
                    IF(CC.EQ.'@') THEN
                                  BACKSPACE N5
                                  READ(N5,*,ERR=888) CC,IO,NEWFIL
                                  IGO = 0
                                  CLOSE(IO)
                                  OPEN(IO,FILE=NEWFIL,FORM='FORMATTED',
     +                                                STATUS='UNKNOWN')
                                  REWIND IO
C#### WCH, 8/93
                                  CALL GTRAIN(0,IGO,ILOST,M4)
                                  IF(IGO.EQ.0) GO TO 7
                                  ELSE
C#### WCH, 8/93
                                  WRITE (N6,9120)
                                  WRITE (*,9120)
                                  BACKSPACE N5
                                  ENDIF
                     ENDIF
C#######################################################################
C     WCH, 9/7/03.
C     End of Rain Block if IDECID = 0.
C=======================================================================
      IF(IDECID.EQ.0) GO TO 600
C=======================================================================
C     Write rainfall summary.
C=======================================================================
      WRITE(N6,2065)
      IF(METRIC.EQ.1) WRITE(N6,995) ISTA,KSUM(1),KSUM(2),TSUM(2),NOYRS
      IF(METRIC.EQ.2) WRITE(N6,996) ISTA,KSUM(1),KSUM(2),TSUM(2),NOYRS
C=======================================================================
C     End of storm event calculations.  Start printing statistics.
C=======================================================================
  101 CONTINUE
      IF(NOSTAT.GT.0) THEN
      DO 400 ICOL = 4,1,-1
      IF(ICOL.LE.2) WRITE(N6,2065)
      IF (ICOL .EQ. 1) CALL SHELL(KEY,LOC11,LSTORM,3,JK1,JK2)
      IF(NOSTAT.GT.0) WRITE(N6,2070) NABRK(ICOL)
      IF(ICOL.LE.2) KEYV = KEY(1,ICOL)
      IF(ICOL.EQ.1) KEYX = KEY(1,2)
      IF(ICOL.EQ.2) KEYX = KEY(1,1)
      IC       = 0
      DO 300 J = 1,LOC11
      IF(J.EQ.1)     GO TO 260
      IF(J.EQ.LOC11) GO TO 240
C=======================================================================
C     THERE IS NO KEY CHANGE IN PASSES 3 AND 4.
C     CHECK FOR CHANGE IN KEY
C=======================================================================
      IF (ICOL .GT. 2) GO TO 280
      IF (KEY(J,ICOL) .EQ. KEYV) GO TO 280
  240 IC = IC+1
      IF(ICOL.EQ.1.AND.NOYRS.EQ.1) STAT(5,1) = STAT(5,1) + 1.0
      DO 250 I = 1,5
      IF(ICOL.EQ.4) STAT(I,1) = FLOAT(NOYRS)
      IF(STAT(I,1).LE.0.0) STAT(I,1) = 1.0
                    STAT(I,5) = STAT(I,2)/STAT(I,1)
      IF(STAT(I,1).GT.1.0) THEN
                  STAT(I,6) = SQRT(ABS((STAT(I,6)-
     *            STAT(I,2)**2/STAT(I,1))/(STAT(I,1)-1.0)))
                  STAT(I,6) = STAT(I,6)/STAT(I,5)
                  ELSE
                  STAT(I,6) = 1.0E20
                  ENDIF
  250 CONTINUE
      IF(ICOL.LE.2) WRITE(N6,2130) KEYV
      IF(ICOL.NE.3) JJ = 5
      IF(ICOL.EQ.3) JJ = 4
      IF(ICOL.EQ.1) JJ = 4
      DO 255 I = 1,JJ
      II       = I
      IF(ICOL.EQ.2.AND.II.EQ.JJ) II = 6
      IF(STAT(I,6).LT.1.0E05) THEN
                   WRITE(N6,2080) TITL(II),(STAT(I,M),M=1,6)
                   ELSE
                   WRITE(N6,2081) TITL(II),(STAT(I,M),M=1,5)
                   ENDIF
  255 CONTINUE
      IF(ICOL.LE.2) KEYV = KEY(J,ICOL)
      IF(J.EQ.LOC11) GO TO 300
  260 DO 270 I  = 1,5
      STAT(I,1) = 0.0
      STAT(I,2) = 0.0
      STAT(I,3) =  1.0E20
      STAT(I,4) = -1.0E20
  270 STAT(I,6) = 0.0
      DATAX     = 0.0
C=======================================================================
C     THE THIRD COLUMN OF ARRAY KEY STORES THE ORIGINAL
C     SEQUENCE NUMBER OF THE STORM INFORMATION BEFORE
C     THE SWITCH AROUND IN SHELL SORT.
C=======================================================================
C     STAT(I,1) = NUMBER
C     STAT(I,2) = TOTAL
C     STAT(I,3) = MINIMUM
C     STAT(I,4) = MAXIMUM
C     STAT(I,5) = AVERAGE
C     STAT(I,6) = VARIANCE
C=======================================================================
  280 DO 290 I = 1,5
      JJ       = KEY(J,3)
      IF(I.LE.2) DATA = X1(JJ,I)
      IF(I.EQ.3) DATA = X2(JJ,1)
      IF(I.EQ.4) DATA = X2(JJ,2)
      IF(I.EQ.5) DATA = X2(JJ,1)
      IF(DATA.EQ.0.0) GO TO 290
      STAT(I,2) = STAT(I,2) + DATA
      STAT(I,6) = STAT(I,6) + DATA*DATA
      IF(I.NE.5) THEN
                 STAT(I,1) = STAT(I,1)+1.0
                 IF(DATA.LT.STAT(I,3)) STAT(I,3) = DATA
                 IF(DATA.GT.STAT(I,4)) STAT(I,4) = DATA
                 GO TO 290
                 ENDIF
      IF(ICOL.EQ.1.AND.KEY(J,2).EQ.KEYX) GO TO 290
      IF(ICOL.EQ.2.AND.KEY(J,1).EQ.KEYX) GO TO 290
      STAT(I,1) = STAT(I,1) + 1.0
C=======================================================================
C     Break in year occured or break in month occured.
C=======================================================================
      IF(ICOL.EQ.1) KEYX = KEY(J,2)
      IF(ICOL.EQ.2) KEYX = KEY(J,1)
      IF(STAT(5,2)-DATAX.LE.0.0) GO TO 290
      IF(STAT(5,2)-DATAX.LT.STAT(5,3)) STAT(5,3) = STAT(5,2)-DATAX
      IF(STAT(5,2)-DATAX.GT.STAT(5,4)) STAT(5,4) = STAT(5,2)-DATAX
      STAT(5,6) = STAT(5,6) + ( STAT(5,2)-DATAX)**2
      DATAX     = STAT(5,2)
  290 CONTINUE
  300 CONTINUE
  400 CONTINUE
      ENDIF
C=======================================================================
C     Compute and print recurrance interval.
C     JK - Shell sort key.
C=======================================================================
      IF(NOSTAT.GT.0) THEN
                      DO 500 IVAR = 1,4
                      IF(IVAR.EQ.1) THEN
                                    MYTEST = NOSTAT/1000
                                    NOSTAT = NOSTAT - MYTEST*1000
                                    IF(MYTEST.EQ.0) GO TO 500
                                    ENDIF
                      IF(IVAR.EQ.2) THEN
                                    MYTEST = NOSTAT/100
                                    NOSTAT = NOSTAT - MYTEST*100
                                    IF(MYTEST.EQ.0) GO TO 500
                                    ENDIF
                      IF(IVAR.EQ.3) THEN
                                    MYTEST = NOSTAT/10
                                    NOSTAT = NOSTAT - MYTEST*10
                                    IF(MYTEST.EQ.0) GO TO 500
                                    ENDIF
                      IF(IVAR.EQ.4.AND.NOSTAT.EQ.0) GO TO 500
                      IF(IVAR.LE.2) JK4(1) = IVAR
                      IF(IVAR.GT.2) JK4(1) = IVAR - 2
                      IF(IVAR.LE.2)CALL SHELR(X1,LOC11,LSTORM,3,
     +                                        JK4,JK5)
                      IF(IVAR.GT.2)CALL SHELR(X2,LOC11,LSTORM,3,
     +                                       JK4,JK5)
                      IF(NPTS.EQ.0)     NPTS = LOC11
                      IF(NPTS.GT.LOC11) NPTS = LOC11
                      IF(NPTS.GT.500)   NPTS = 500
                      DO 440 I= 1,NPTS
  440                 XOUT(I) = (FLOAT(NOYRS)+1.0-2.0*A)/(FLOAT(I)-A)
                      IF(METRIC.EQ.1) WRITE(N6,2170) TITL(IVAR),
     +                                   VALUE(IVAR),VALUE(IVAR)
                      IF(METRIC.EQ.2) WRITE(N6,2170) TITL(IVAR),
     +                                   VALMM(IVAR),VALMM(IVAR)
                      MPTS     = NPTS/2
                      TIMDAY   = 0.0
                      DO 450 I = 1,MPTS
                      I2       = I + MPTS
                      IF(IVAR.LE.2) JULDAY   = IFIX(X1(I,3))
                      IF(IVAR.GT.2) JULDAY   = IFIX(X2(I,3))
                      CALL DATED
                      M1(1)    = MONTH
                      D1(1)    = NDAY
                      Y1(1)    = NYEAR
                      IF(IVAR.LE.2) JULDAY   = IFIX(X1(I2,3))
                      IF(IVAR.GT.2) JULDAY   = IFIX(X2(I2,3))
                      CALL DATED
                      M1(2)    = MONTH
                      D1(2)    = NDAY
                      Y1(2)    = NYEAR
                      IV       = IVAR - 2
                      IF(IVAR.GT.2) WRITE(N6,2150) I,X2(I,IV),XOUT(I),
     +                             M1(1),D1(1),Y1(1),I2,X2(I2,IV),
     +                             XOUT(I2),M1(2),D1(2),Y1(2)
  450                 IF(IVAR.LE.2) WRITE(N6,2150) I,X1(I,IVAR),XOUT(I),
     +                             M1(1),D1(1),Y1(1),I2,X1(I2,IVAR),
     +                             XOUT(I2),M1(2),D1(2),Y1(2)
  500                 CONTINUE
                      ENDIF
C#### WCH, 9/7/93.
  600 WRITE(*,1010)
      WRITE(N6,1010)
      CLOSE (JIN(INCNT))
      RETURN
C=======================================================================
  888 CALL IERROR
C#######################################################################
C     WCH, 10/11/93.  ADDITIONAL ERROR MESSAGE FOR READING SCRATCH
C       FILE M4 WITH 15-MIN. DATA.
C=======================================================================
C#### WCH, 8/4/95.  ALTER IOSTAT FOR LAHEY.
  950 WRITE(N6,9500) M4,JULDAY,JLDAY,SHOUR,THIST,REIN,MOD(IOS,256)
      WRITE(*,9501) M4,MOD(IOS,256)
      STOP
C=======================================================================
   68 FORMAT(/,10X,A80,/,10X,A80)
  911 FORMAT('+ Reading rainfall from year : ',I4)
C#### WCH, 8/1/95.  FILE ERROR MESSAGE.
  965 FORMAT(' ==> ERROR!  JIN DATA INPUT FILE ON UNIT',I3,' IS EMPTY.'
     1,/,' BE SURE YOU HAVE UNIT PROPERLY DEFINED ON @-LINE.',/,
     2   ' RUN STOPPED AT BEGINNING OF RAIN BLOCK.')
  970 FORMAT(//,10X,
     +' ******************************************',/,10X,
     +' * Rainfall from Nat. Weather Serv. file  *',/,10X,
     +' * in units of hundredths of an inch      *',/,10X,
     +' ******************************************',//,
     * 10X,A80,/,10X,A80,//,1X,4(2X,'Mo/Dy/Year Hr/ Rn'),/,
     *                 1X,4(2X,'---------- ------'))
  971 FORMAT(//,10X,
     +' ******************************************',/,10X,
     +' * Rainfall from Weather Service tape in  *',/,10X,
     +' * units of tenths of a millimeter        *',/,10X,
     +' ******************************************',//,
     * 10X,A80,/,10X,A80,//,1X,4(2X,'Mo/Dy/Year Hr/ Rn'),/,
     *                 1X,4(2X,'---------- ------'))
  975 FORMAT(//,10X,
     +' ########################################################',/,10X,
     +' # Rainfall from user time series in units of           #',/,10X,
     +' # thousandths of an inch or hundredths of a millimeter #',/,10X,
     +' # (as defined by the user with parameter METRIC)       #',/,10X,
     +' ########################################################',//,
     *10X,A80,/,10X,A80,//,1X,4(2X,'Mo/Dy/Year Hr/ Rn'),/,
     *                1X,4(2X,'---------- ------'))
  980 FORMAT(' ',4(1X,2(I2,'/'),I4,I3,'/',I4))
  990 FORMAT(' Total',19X,F8.2,20X,I6,I8,/,
     *' Estimated missing rainfall              ',12X,F6.2,F8.2)
C#### WCH, 8/1/95.  995 AND 996: CHANGE ISTA FORMAT TO A8 FROM I8.
  995 FORMAT(/,
     *'      ########################################',/,
     *'      #         Precipitation Summary        #',/,
     *'      ########################################',//,
     *'      Rainfall summary for station    ',A8,/,
     *'      Total missing hours             ',2X,I6,'  hours',/,
     *'      Total hours of meter malfunction',2X,I6,'  hours',/,
     *'      Total rainfall                  ',F8.2,'  inches',/,
     *'      Total number of years           ',2X,I6,'  years',/)
  996 FORMAT(/,
     *'      ****************************************',/,
     *'      *         Precipitation Summary        *',/,
     *'      ****************************************',//,
     *'      Rainfall summary for station    ',A8,/,
     *'      Total missing hours             ',2X,I6,'  hours',/,
     *'      Total hours of meter malfunction',2X,I6,'  hours',/,
     *'      Total rainfall                  ',F8.2,'  millimeters',/,
     *'      Total number of years           ',2X,I6,'  years',/)
 1000 FORMAT(/,' ***************************************************',/,
     1        ' * Entry made to the Rain Block                    *',/,
     2        ' * Created by the University of Florida - 1988     *',/,
     3        ' * Updated by Oregon State University, Dec. 1996   *',/,
     4        ' ***************************************************',//)
 1010 FORMAT(/,' ===> Rain Block ended normally.')
C#### WCH, 8/1/95.  CHANGE ISTA TO CHARACTER.
 1021 FORMAT(1X,//,
     1'      ########################################',/,
     1'      #  Precipitation Block Input Commands  #',/,
     1'      ########################################',//,
     1 5X,'Station, ISTA.........................',A10,//,
     2 5X,'Beginning date, IYBEG (Yr/Mo/Dy)......',
     2     2X,I4,'/',I2,'/',I2,//,
     3 5X,'Ending date, IYEND (Yr/Mo/Dy).........',
     3     2X,I4,'/',I2,'/',I2,//,
     4 5X,'Minimum interevent time, MIT..........',I6,//,
     6 5X,'Number of ranked storms, NPTS.........',I6,//,
     6 5X,'NWS format, IFORM (See text)..........',I6,//,
     6 5X,'Print storm summary, ISUM (O-No 1-Yes)',I6,//,
     6 5X,'Print all rainfall, IYEAR (O-No 1-Yes)',I6,//,
     6 5X,'Save storm event data on NSCRAT(1)....',I6,/,
     7 5X,'(IFILE =0 -Do not save, =1 -Save data)',//,
     8 5X,'IDECID 0 - Create interface file',/,
     9 5X,'       1 - Create file and analyze',/,
     1 5X,'       2 - Synoptic analysis..........',I6,//,
     1 5X,'Plotting position parameter, A........',F6.2,//,
     1 5X,'Storm event statistics, NOSTAT........',I6,//)
 1022 FORMAT(1X,//,
     1'      ########################################',/,
     1'      #  Precipitation Block Input Commands  #',/,
     1'      ########################################',//,
     1 5X,'Station, ISTA.........................',A10,/,
     2 5X,'Beginning date, IYBEG (Yr/Mo/Dy)......',
     2     2X,I4,'/',I2,'/',I2,//,
     3 5X,'Ending date, IYEND (Yr/Mo/Dy).........',
     3     2X,I4,'/',I2,'/',I2,//,
     6 5X,'NWS format, IFORM (see text)..........',I6,/,
     6 5X,'Print storm summary, ISUM (O-NO 1-YES)',I6,/,
     6 5X,'Print all rainfall, IYEAR (O-NO 1-YES)',I6,/,
     8 5X,'IDECID 0 - Create interface file',/,
     9 5X,'       1 - Create file and analyze',/,
     1 5X,'       2 - Synoptic analysis..........',I6)
C#### WCH, 4/26/94.
 1025 FORMAT(/,
     1 5X,'KODEA (from optional group B0)........',I6,/,
     2 5X,' = 0, Do not include NCDC cumulative values.',/,
     3 5X,' = 1, Average NCDC cumulative values.',/,
     4 5X,' = 2, Use NCDC cumulative value as inst. rain.')
C#### WCH, 2/27/95.  Typo: accumulated, not accumuated
 1026 FORMAT(/,
     1 5X,'KODEPR (from optional group B0).......',I6,/,
     2 5X,' Print NCDC special codes in event summary:',/,
     3 5X,' = 0, only on days with events.',/,
     4 5X,' = 1, on all days with codes present.',/,
     5 5X,' Codes: A = accumulated value, I = incomplete value,',/,
     6 5X,'        M = missing value,     O = other code present')
C#### WCH, 8/1/95.  ADDITIONAL INFO FOR IFORM=13.
 1027 FORMAT(
     1 5X,' For current Canadian AES data, I = unadjusted')
C#### WCH, 7/25/96.
 1028 FORMAT(/,
     1 5X,'KOVER (from optional group B0)........',I6,/,
     2 5X,' = 0, Do not over-write existing rainfall interface file.',/,
     3 5X,' = 1, Over-write existing file with single new gage data.')
 1030 FORMAT(1X,//,5X,
     +' #############################',/,5X,
     +' # User defined input format #',/,5X,
     +' #############################',//,
     + 5X,' User format (FIRMAT)..................',A60,/,
     + 5X,' I/O units (METRIC) 0 = U.S. customary.',/,
     + 5X,'                    1 = Metric units...',I6,/,
     + 5X,' Units of rainfall (KUNIT).............',I6,/,
     + 5X,' 0 -> Intensity; 1 --> Volume..........',/,
     + 5X,' Conversion factor (CONV)..............',F6.2,/,
     + 5X,' Rainfall interval in minutes (THISTO).',F6.2,/,
     + 5X,' Field position for STATION NUMBER, F1.',I6,/,
     + 5X,' Field position for YEAR,    F2........',I6,/,
     + 5X,' Field position for MONTH,   F3........',I6,/,
     + 5X,' Field position for DAY,     F4........',I6,/,
     + 5X,' Field position for HOUR,    F5........',I6,/,
     + 5X,' Field position for MINUTE,  F6........',I6,/,
     + 5X,' Field position for RAINFALL,F7........',I6)
C#### WCH, 11/12/93
 1035 FORMAT (5X,' Hour values to be in range 0 - 23 (CONV > 0)',/)
 1036 FORMAT (5X,' Hour values to be in range 1 - 24 (CONV < 0)',/)
C#### WCH, 4/26/94.  FORMATS 2020,2021,2022,2025
 2020 FORMAT(I5,1X,2(I2,'/'),I4,I3,F7.0,3(1X,F6.2),1X,F6.0,I5,I8,
     1 5X,4A1)
 2021 FORMAT(I5,1X,2(I2,'/'),I4,I3,F7.0,3(1X,F6.2),1X,
     +      ' Undef',I5,I8,5X,4A1)
 2022 FORMAT(4X,2(I2,'/'),I4,58X,4A1)
C#### WCH, 7/23/96.  ADD HEADER FOR ASCII FILE WITH EVENT DATA.
C#### WCH (B. LAZERTE), 10/2/96.  CHANGE FORMAT FOR MIT TO I7, NOT F7.2.
 2023 FORMAT(' Min. Interevent Time =',I7,' hrs.',/,
     1 T34,'Dura-          Avg    Max  Inter-  Hours   Hours',/,
     1'          Event   Date   Start   tion  Volume Inten  Inten  event
     2  Missing Meter Special',/,
     3'  Station   No. Mo Da Yr  Hour   hours inches in/hr  in/hr  hours
     4  Data    Stuck   Codes')
 2024 FORMAT(' Min. Interevent Time =',I7,' hrs.',/,
     1 T34,'Dura-          Avg    Max  Inter-  Hours   Hours',/,
     1'          Event   Date   Start   tion  Volume Inten  Inten  event
     2  Missing Meter Special',/,
     3'  Station   No. Mo Da Yr  Hour   hours   mm   mm/hr  mm/hr  hours
     4  Data    Stuck   Codes')
C#### WCH, 8/1/95.  CHANGE 2025 FIRST FIELD TO A8 FROM I6.
 2025 FORMAT(1X,A8,1X,I5,1X,2(I2,1X),I2,I5,F7.0,3(1X,F6.2),
     *       1X,F6.0,I5,I8,5X,4A1)
C#### WCH, 8/93
 2027 FORMAT(//,' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',/,
     1          ' Note, 15-min. data are being processed, but hourly',/,
     2          ' print-out, summaries, and statistics are based on ',/,
     3          ' hourly totals only.  Data placed on interface file',/,
     4          ' are at correct 15-min. intervals.                 ',/,
     5          ' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
C#### WCH, 7/23/96.  CHANGE HEADING FOR INTEREVENT TIME.
 2030 FORMAT(1H1,/,
     +' ##########################################################',/,
     *' #  S  T  O  R  M    E  V  E  N  T    S  U  M  M  A  R  Y #',/,
     +' ##########################################################',//,
     +1X,A80,/,1X,A80,//,
     223X,'Dur-          Ave    Max  Inter- Hours   Hours'/,
     41X, 'Storm        Start   ',
     3    'ation Volume Inten  Inten  event  Missing Meter Special'/,
     51X,' No    Date   Hour   hours inches in/hr  in/hr  hours  Data
     5 Stuck   Codes',/,1X,' --    ----   ----   ----- ------ -----  ---
     6--  -----  ----    ----- -------')
 2035 FORMAT(1H1,/,
     +' ##########################################################',/,
     *' #  S  T  O  R  M    E  V  E  N  T    S  U  M  M  A  R  Y #',/,
     +' ##########################################################',//,
     +1X,A80,/,1X,A80,//,
     223X,'Dur-          Ave    Max  Inter- Hours   Hours'/,
     41X, 'Storm        Start   ',
     3    'ation Volume Inten  Inten  event  Missing Meter Special'/,
     51X,' No    Date   Hour   hours millim mm/hr  mm/hr  hours  Data
     5 Stuck   Codes',/,1X,' --    ----   ----   ----- ------ -----  ---
     6--  -----  ----    ----- -------')
 2065 FORMAT(1H1)
 2070 FORMAT (1X,//,5X,'Rainfall Statistics by ',A6,'(for period of',
     .' record)'/,
     .18X,'Number',5X,'Total',3X,'Minimum',3X,'Maximum',3X,'Average',
     .3X,'Coef-Var',/,
     .18X,'------',5X,'-----',3X,'-------',3X,'-------',3X,'-------',
     .3X,'--------')
 2080 FORMAT(8X,A10,F6.0,1X,F9.2,4(1X,F9.3))
 2081 FORMAT(8X,A10,F6.0,1X,F9.2,3(1X,F9.3),' Undefined')
 2115 FORMAT(//,
     +' ********************************************************',/,
     +' *  Precipitation output created using the Rain block   *',/,
     +' *  Number of precipitation stations...',I9,'        *',/,
     +' ********************************************************',/)
 2116 FORMAT(/,' $$$ CAUTION! Multiple precip. stations based on non-emp
     *ty file on unit ',I3,/,' If this is not correct, delete file on un
     *it ',I3,' before Rain Block run.',/,' Otherwise may lead to incorr
     *ect Rain Block interface file.'/)
C#### WCH, 8/1/95.  CHANGE STATION IDs TO CHARACTER.
 2120 FORMAT(' Location Station Number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',A13,/))
C#### WCH, 7/25/96.  MESSAGE ABOUT OVER-WRITING JOUT.
 2121 FORMAT(/,' $$$ CAUTION! About to over-write non-empty rainfall int
     1erface file',/,
     2' on unit',I3,' possibly containing data from',I3,' gage(s).',/,
     3' This is in accordance with parameter KOVER = 1.')
 2130 FORMAT(1X,I6)
 2150 FORMAT(' ',2(I5,2F8.3,3X,2(I2,'/'),I4,5X))
 2170 FORMAT(//,1H1,10X,'Return Period(years) for ',A10,///,1X,
     .2('     ',8X,2X,'Return                  '),/,1X,
     .2(' Rank',2X,A6,2X,'Period   Mo/Dy/Yr       '),/,1X,
     .2(' ----',2X,'------',2X,'------   --------       '))
 2220 FORMAT(6X,21I5,I9,T2,I5)
 2240 FORMAT(1X,I4,12(1X,I3))
 9101 FORMAT(/,' ===> Error !! # of storms greater than: ',I9,/)
cim   cim 11/97 write last days in file
 8000 FORMAT(/,' Last day and hour for this gage          = '
     a,I10,F10.3)
 8010 FORMAT(  ' Last day and hour in input transfer file = '
     a,I10,F10.3)
 8020 FORMAT('    Total Rainfall For Each Gage : ')
 8030 FORMAT(/,5X,I10,F10.3)
C#### WCH, 8/93.
 9110 FORMAT(/,' ===> ERROR. Time interval for new data,',F7.1,/,
     1 ' sec. does not',/,'     agree with time interval,',F7.1,
     2 ' sec. of data on unit JOUT with which',/,
     3 '     new data are to be combined.  Run stopped.')
 9120 FORMAT(/,' WARNING. Additional data are expected for IFORM = 4 or
     16 but no @-line is',/,' found in input stream to identify data for
     2 future years.',/,' Rain Block will stop processing data.')
C#### WCH, 10/11/93.
C#### WCH, 8/4/95.  9500 AND 9501, CHANGE RMFORT TO LAHEY.
 9500 FORMAT(/,' ERROR READING SCRATCH FILE ON UNIT',I3,' CONTAINING',/,
     1' YEARLY 15-MIN. PRECIP. TIME SERIES.',/,' SHOULD BE AT APPROX. JU
     2LIAN DAY',I6,'. PARAMETERS FROM SCRATCH FILE ARE:',/,
     3' JULIAN DAY =',I6,/,' TIME OF DAY =',F7.1,' SECONDS',/,
     4' THISTO =',F7.1,' SECONDS',/,' PRECIP =',F7.3,' IN/HR',/,
     5' LAHEY ERROR NO. =',I5,/,' RUN STOPPED FROM RAIN BLOCK.')
 9501 FORMAT(' ERROR READING SCRATCH FILE ON UNIT',I3,' CONTAINING',/,
     1' YEARLY 15-MIN. PRECIP. TIME SERIES.',/,' SEE OUTPUT FILE FOR DET
     2AILED MESSAGE.',/,
     3' LAHEY ERROR NO. =',I5,/,' RUN STOPPED FROM RAIN BLOCK.')
C=======================================================================
      END
