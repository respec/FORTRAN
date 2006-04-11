      SUBROUTINE G9RAIN(IDO,IGO,ILOST,M3,M4)
C	RAIN BLOCK
C	CALLED BY RAIN NEAR LINES 436 AND 468
C=======================================================================
C   Routine to convert EarthInfo ASCII output files of hourly and 
C     15-minute precipitation files into ASCII file readable by 
C     list-directed read statement.  
C   Also, perform all calculations for reading of processed data for
C     each year (IDO = 1).
C   Written by Wayne Huber, August 1993
C   WCH, 4/12/94.  Fix logic for check of stop date.
C   WCH, 4/25/94.  Put intensity, not depth, on 15-min. scratch file M4,
C     provide for rounding and correct sum when using average, 
C     initialize output array for processed rain file.  
C   WCH, 4/26/94.  Include provision for listing of special codes in
C     rainfall data in event summary.
C   WCH, 5/25/94.  Check for negative last increment when averaging
C     rainfall.
C   WCH, 6/5/95.  Correction for handling of accumulated rain under old
C     option.  
C   WCH, 8/1/95.  Adapt subroutine to presence of character station ID.
C     Here, retain an integer ID for all computations.  
C   WCH, 8/4/95.  Alter IOSTAT for Lahey.
C   WCH, 8/4/95.  For Lahey, re-write ASCII data file using zeros 
C     instead of periods.  Lahey won't accept a period (DOT) as a zero!
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PRECIP.INC'
C=======================================================================
      PARAMETER (NJJJ=600,NKKK=800)
C=======================================================================
C  Caution.  If change NJJJ parameter, may have to change 9000 and 9010
C    Format statements to read enough character data.
C=======================================================================
C#### WCH, 8/4/95.  ADD CHARACTER ZERO.
      CHARACTER STATE*2,CUNIT*2,CHH(NJJJ)*1,CLYD(NKKK)*1,DASH*1,CCA*1,
     1 CCDB*1,CCE*1,CCI*1,CCMB*1,CCMS*1,DOT*1,COMMA*1,QUOTE*1,
     2 CCDS*1,CODE(96)*1,PCODE(4,7)*1,QUEST*1,CCT*1,BLANK*1,SLASH*1,
     3 ZERO*1
      DIMENSION YRAIN(96),LDATEP(4)
      DATA DASH/'-'/,CCA/'A'/,CCE/'E'/,CCI/'I'/,CCMB/'M'/,CCMS/'m'/,
     1 CCDS/'d'/,DOT/'.'/,COMMA/','/,QUOTE/''''/,CCDB/'D'/,QUEST/'?'/,
     2 CCT/'T'/,BLANK/' '/,SLASH/1H//,ZERO/'0'/
C=======================================================================
C  IFORM =  9: Use "raw" EarthInfo ASCII file for 15-min. data.
C  IFORM = 10: Use "raw" EarthInfo ASCII file for hourly data.
C  IFORM = 11: Use processed EarthInfo ASCII file for 15-min. data.
C  IFORM = 12: Use processed EarthInfo ASCII file for hourly data.
C  "Raw" data file must be converted for form suitable for list-
C    directed input, i.e., place character data in quotes.
C=======================================================================
C#### WCH, 8/1/95.  Recover integer station ID for use in this program.
C     NOTE: Change all ISTA values to KSTA in remainder of program!
C=======================================================================
      READ(ISTA,'(I8)') KSTA
C=======================================================================
C  If IDO = 0, first search for station number and correct dates.
C=======================================================================
      IF(IDO.EQ.1) GO TO 400
C=======================================================================
      REWIND IO
      IPPP = 0
   20 IF(IFORM.EQ.9.OR.IFORM.EQ.10) THEN
C=======================================================================
C  Check for field width of station number.
C=======================================================================
        LREAD = 0
        IF(KSTA/1000.LT.1) LREAD = 1
C=======================================================================
        IF(LREAD.EQ.0) READ(IO,9000,END=1000,ERR=1200) STATE,NSTA,INDIC,
     +           CUNIT,NEWMON,NEWDAY,NEWYR
        IF(LREAD.EQ.1) READ(IO,9010,END=1000,ERR=1200) STATE,NSTA,INDIC,
     +           CUNIT,NEWMON,NEWDAY,NEWYR
        ELSE
        READ(IO,*,END=1000,IOSTAT=IOS,ERR=1300) STATE,NSTA,INDIC,
     +           CUNIT,NEWMON,NEWDAY,NEWYR
        ENDIF
      IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 8/1/95.  THIS COMPARISON MAY NOT WORK.  CHECK IN FUTURE.
      IF(NSTA.NE.KSTA) THEN
                WRITE(N6,9510) NSTA,KSTA,STATE
                WRITE(*,9510) NSTA,KSTA,STATE
                STOP
                ENDIF
C=======================================================================
C   Here if station numbers match.
C   Now, find correct starting date.
C=======================================================================
      IF(IPPP.EQ.0) WRITE(*,9100) IO,NSTA
      IPPP = 1
      IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1)) GO TO 20
      IF(NEWMON.LT.IYBEG(2).AND.NEWYR.EQ.IYBEG(1)) GO TO 20
      IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3).
     +     AND.NEWYR.EQ.IYBEG(1)) GO TO 20
C=======================================================================
C  Here, have matched station and dates.
C=======================================================================
      IF(IFORM.EQ.9.OR.IFORM.EQ.11) THEN
         THISTO = 900.0
         NEND   = 97
         NJR    = NJJJ
         ELSE
         THISTO = 3600.0
         NEND   = 25
         NJR    = 175
         ENDIF
      HIST = THISTO
      IF(IFORM.GE.11) RETURN      
C=======================================================================
C  Now, must convert comma-delimited EarthInfo format containing non-
C    numerical values into ASCII form readable by list-directed format.
C  Store file on NSCRAT(3) = M3.
C=======================================================================
      REWIND M3
      REWIND IO
C=======================================================================
C  Note, will intentionally read beyond end of record in order to read
C    all characters on the line.  
C  There is apparently no error when reading beyond end of record.  
C  Don't need to backspace.
C=======================================================================
   25 DO 30 I = 1,NJR
   30 CHH(I) = COMMA
C#### WCH, 4/25/94.  ADD INITIALIZATION OF OUTPUT ARRAY ALSO.
      DO 35 I = 1,NKKK
   35 CLYD(I) = BLANK
      IF(LREAD.EQ.0) READ(IO,9000,END=300,ERR=50) STATE,NSTA,INDIC,
     +  CUNIT,NEWMON,NEWDAY,NEWYR,(CHH(I),I=1,NJR)
      IF(LREAD.EQ.1) READ(IO,9010,END=300,ERR=50) STATE,NSTA,INDIC,
     +  CUNIT,NEWMON,NEWDAY,NEWYR,(CHH(I),I=1,NJR)
      IF(NEWYR.LT.100) NEWYR = NEWYR + 1900
C=======================================================================
C  Parse characters beyond column 24 based on comma separation.  
C  Every data field is separated by a field that may or may not contain
C    an indicator variable.
C  There are up to 96 data fields for quarter-hourly values, and up to
C    24 data fields for hourly values -- plus a daily total for each.
C=======================================================================
C  Now, must parse fields.
C  J = SUBSCRIPT FOR INPUT ARRAY
C  K = SUBSCRIPT FOR OUTPUT ARRAY
C=======================================================================
   50 J = 1
      K = 1
      KOUNT = 0
   60 KOUNT = KOUNT + 1
C=======================================================================
C  Check for too many characters in line being read or line being 
C    written.
C=======================================================================
      IF(J.GE.NJR) THEN
        WRITE(N6,9520) NJR,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        WRITE(*,9520)  NJR,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        CLOSE (M3)
        STOP
        ENDIF
      IF(K.GE.NKKK) THEN
        WRITE(N6,9530) M3,NKKK,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        WRITE(*,9530)  M3,NKKK,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        CLOSE (M3)
        STOP
        ENDIF
C=======================================================================
      IF(CHH(J).EQ.DASH) THEN 
C#### WCH, 8/4/95.  USE ZERO INSTEAD OF DOT.
C####        CLYD(K) = DOT
        CLYD(K) = ZERO
        CLYD(K+1) = COMMA
        IF(KOUNT.EQ.NEND) THEN
           GO TO 200
           ELSE
           K = K + 2
           J = J + 4
           GO TO 100
           ENDIF
        ENDIF
      IF(CHH(J).EQ.DOT) THEN
C#### WCH, 8/4/95.  USE ZERO INSTEAD OF DOT.
C####        CLYD(K) = DOT
        CLYD(K) = ZERO
        CLYD(K+1) = COMMA
        IF(KOUNT.EQ.NEND) THEN
           GO TO 200
           ELSE
           K = K + 2
           J = J + 2
           GO TO 100
           ENDIF
        ENDIF
C=======================================================================
C  Here, should be reading a number of field-width 4.
C=======================================================================
      DO 70  L = 1,4
      LL = L - 1
   70 CLYD(K+LL) = CHH(J+LL)
C=======================================================================
C  There are occasional field-widths of 5. Check.
C=======================================================================
      IF(CHH(J+4).NE.COMMA) THEN
         CLYD(K+4) = CHH(J+4)
         CLYD(K+5) = COMMA
         J = J + 6
         K = K + 6
         ELSE
         CLYD(K+4) = COMMA
         K = K + 5
         J = J + 5
         ENDIF
      IF(KOUNT.EQ.NEND) THEN
C=======================================================================
C  If last entry (daily total), then ready to write record.
C=======================================================================
        KLAST = K - 1
        GO TO 200
        ENDIF
C=======================================================================
C  Here, should be between numerical fields.
C  If lucky, there is no entry (next character is a comma).
C  Otherwise, insert intermediate character between quotes.
C=======================================================================
  100 IF(CHH(J).EQ.COMMA) THEN
        CLYD(K) = COMMA
        K = K + 1
        J = J + 1
        GO TO 60
        ELSE
        CLYD(K) = QUOTE
        CLYD(K+1) = CHH(J)
        CLYD(K+2) = QUOTE
        CLYD(K+3) = COMMA
        K = K + 4
        J = J + 2
        GO TO 60
        ENDIF
C=======================================================================
C  Here, have completed reading and parsing one line.
C  Now, write onto unit M3 = NSCRAT(3).        
C=======================================================================
  200 IF(KLAST.GT.NKKK) THEN
        WRITE(N6,9530) M3,NKKK,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        WRITE(*,9530)  M3,NKKK,STATE,NSTA,NEWMON,NEWDAY,NEWYR
        CLOSE (M3)
        STOP
        ENDIF
      WRITE (*,9120) NEWMON,NEWYR
      CLYD(KLAST) = SLASH
      WRITE(M3,9250) QUOTE,STATE,QUOTE,COMMA,NSTA,COMMA,INDIC,
     1 COMMA,QUOTE,CUNIT,QUOTE,COMMA,NEWMON,COMMA,NEWDAY,COMMA,NEWYR,
     2 COMMA,(CLYD(I),I=1,KLAST)
       GO TO 25
C=======================================================================
C  Here, have processed last record on input file.  
C  Reset file to beginning of desired time period.
C=======================================================================
  300 IF(IFORM.LT.11) IUSE = M3
      IF(IFORM.GE.11) IUSE = IO
      WRITE(*,9110) IUSE
      REWIND IUSE
CXXX  320 READ(IUSE,*,END=1000,IOSTAT=IOS,ERR=1300) STATE,NSTA,INDIC,
  320 READ(IUSE,*,END=1000) STATE,NSTA,INDIC,
     +           CUNIT,NEWMON,NEWDAY,NEWYR
C#### WCH, 8/1/95.  THIS COMPARISON MAY NOT WORK. CHECK IN FUTURE.
      IF(NSTA.NE.KSTA) THEN
                WRITE(N6,9510) NSTA,KSTA,STATE
                WRITE(*,9510) NSTA,KSTA,STATE
                STOP
                ENDIF
C=======================================================================
C   Here if station numbers match.
C   Now, find correct starting date.
C=======================================================================
      IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1)) GO TO 320
      IF(NEWMON.LT.IYBEG(2).AND.NEWYR.EQ.IYBEG(1)) GO TO 320
      IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3).
     +     AND.NEWYR.EQ.IYBEG(1)) GO TO 320
C=======================================================================
C  Here, have matched station and dates.  
C  Ready to read data for each year (IDO = 1).
C=======================================================================
      BACKSPACE IUSE
      RETURN
C=======================================================================
C  End of initial pass through G9RAIN (IDO = 0).  
C=======================================================================
C  Read processed EarthInfo precipitation file for each year (IDO = 1).
C  If working with 15-min data, must store annual data on a scratch 
C    file, unit M4 = NSCRAT(4).  Combine with data from other gages
C    and write final interface file in Subroutine RAIN.  
C=======================================================================
  400 IF(IFORM.LT.11) IO = M3
C####  WCH, 4/26/94.  INITIALIZE NEW VARIABLE ACODE.
      DO 405 I = 1,366
      DO 405 J = 1,4
  405 ACODE(I,J) = BLANK
      IF(IFORM.EQ.9.OR.IFORM.EQ.11) THEN
C=======================================================================
C  15-min. data
C=======================================================================
         NEND = 96
         IDIVD = 4
         THIS = 900.
         REWIND M4
         ELSE
C=======================================================================
C  Hourly data
C=======================================================================
         NEND = 24
         IDIVD = 1
         THIS = 3600.
         ENDIF
      ILOST = 1
      LLLPRT = 0
      CALL SETIA(HOUR,366,27,0)
      IDAST = KDATE(0,1,NEWYR)
      JPRT = 0
  420 DO 425 I = 1,NEND
  425 CODE(I) = BLANK
CXXX
CXXX      READ(IO,*,END=700,IOSTAT=IOS,ERR=1300) STATE,NSTA,INDIC,
      READ(IO,*,END=700) STATE,NSTA,INDIC,
     + CUNIT,NEWMON,NEWDAY,NWYR,(YRAIN(I),CODE(I),I=1,NEND),DAYTOT
      IF(IYEND(1).NE.0) THEN
C#### WCH, 4/12/94.  FIXED FLAWED LOGIC FOR CHECK OF STOP DATES.
         IF(NWYR.GT.IYEND(1)) GO TO 700
         IF(NWYR.EQ.IYEND(1).AND.NEWMON.GT.IYEND(2)) GO TO 700
         IF(NWYR.EQ.IYEND(1).AND.NEWMON.EQ.IYEND(2).AND.
     +      NEWDAY.GT.IYEND(3)) GO TO 700
C####         IF(.NOT.(NWYR.LE.IYEND(1).AND.NEWMON.LE.IYEND(2).AND.
C####     +      NEWDAY.LE.IYEND(3))) GO TO 700
         ENDIF
      IF(NWYR.GT.NEWYR) GO TO 600
      IDAY = KDATE(NEWDAY,NEWMON,NWYR) - IDAST
      JULDAY = NWYR*1000 + IDAY
      TOT = 0.0
      LPRT = 0
      IF(JPRT.EQ.0) THEN
         DO 440 J = 1,7
         DO 440 I = 1,4
  440    PCODE(I,J) = DASH
         ENDIF
      IMARK = 1
C=======================================================================
      DO 500 I = 1,NEND
C#### WCH, 4/26/94.  MAKE INDICATION OF SPECIAL CHARACTERS.
      CALL RAINCD(CODE(I),IDAY)
      TOT = TOT + YRAIN(I)
      IHR = (I-1)/IDIVD + 1
      IF(NEND.EQ.96) RHOUR = FLOAT(I-1)*THIS
      IF(CODE(I).EQ.BLANK) THEN
        HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IFIX(YRAIN(I)*100.)
C#### WCH, 4/25/94.  NEED TO PUT INTENSITY ON M4, NOT DEPTH.
        IF(YRAIN(I).GT.0.0.AND.NEND.EQ.96) WRITE (M4) JULDAY,RHOUR,
     +     THIS,YRAIN(I)/0.25
        ELSE
        IF(LPRT.EQ.0) JPRT = JPRT + 1
        IF(CODE(I).EQ.CCA) THEN
          PCODE(JPRT,1) = CCA
          LPRT = 1
C=======================================================================
C  Here, have an accumulated total.  Spread over previous intervals
C    until reaching beginning of day or previous non-nul code. 
C=======================================================================
C#### WCH, 4/25/94.  SEVERAL SMALL CHANGES IN THIS SECTION.
C#### WCH, 4/26/94.  INSERT NEW CODE FOR KODEA INDICATOR.
          IF(KODEA.EQ.1) THEN       
             AVG = YRAIN(I)/FLOAT(I-IMARK+1)
             IVGS = 0
C#### WCH, 5/25/94.  NEED TO CHECK FOR NEGATIVE LAST INCREMENT.
C     SUBSTITUTE JRAIN FOR IFIX(AVG... ) IN DO 450 LOOP BELOW.
             JRAIN = IFIX(AVG*100.0+0.4)
             IF((I-IMARK)*JRAIN.GT.IFIX(YRAIN(I)*100.)) JRAIN = JRAIN-1
             IF(JRAIN.LT.0) JRAIN = 0
C
             DO 450 J = IMARK,I
             IIHR = (J-1)/IDIVD + 1
             IF(NEND.EQ.96.AND.AVG.GT.0.0) THEN
                RHOUR = FLOAT(J)*THIS
                WRITE(M4) JULDAY,RHOUR,THIS,AVG/0.25
                ENDIF
             IF(J.LT.I) THEN
                IVGS = IVGS + JRAIN
                HOUR(IDAY,IIHR) = HOUR(IDAY,IIHR) + JRAIN
                ELSE
                HOUR(IDAY,IIHR) = HOUR(IDAY,IIHR) + (IFIX(YRAIN(I)*100.)
     1           - IVGS)
                ENDIF
  450        CONTINUE
C
             IMARK = I + 1
             GO TO 500
C=======================================================================
C   If KODEA = 2, lump accumulated rain at this time.
C=======================================================================
             ELSE IF(KODEA.EQ.2) THEN
                HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IFIX(YRAIN(I)*100.)
                IF(YRAIN(I).GT.0.0.AND.NEND.EQ.96) WRITE (M4) JULDAY,
     +             RHOUR,THIS,YRAIN(I)/0.25
C=======================================================================
C   If KODEA = 0 (only remaining option), indicate as in past.
C=======================================================================
             ELSE 
             HOUR(IDAY,IHR) = -2
C#### WCH, 6/5/95.  CHANGE TO YRAIN(I)*100.
C####             SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
             SUM(1) = SUM(1) + YRAIN(I)*100.
             ENDIF
C
          ENDIF
        IF(CODE(I).EQ.CCMB.OR.CODE(I).EQ.CCMS) THEN
C=======================================================================
C  Here, have missing data.  Fill in HOUR array with -1.
C=======================================================================
          DO 460 J = IMARK,I
          IIHR = (J-1)/IDIVD + 1
  460     HOUR(IDAY,IIHR) = -1
          IMARK = I + 1
          PCODE(JPRT,2) = CCMB
          LPRT = 1
          GO TO 500
          ENDIF
        IF(CODE(I).EQ.CCDB.OR.CODE(I).EQ.CCDS) THEN
C=======================================================================
C  Here, have deleted data.  Fill in array HOUR with -1.
C=======================================================================
           DO 470 J = IMARK,I
           IIHR = (J-1)/IDIVD + 1
  470      HOUR(IDAY,IIHR) = -1
           IMARK = I + 1
           LPRT = 1
           PCODE(JPRT,3) = CCDB
           GO TO 500
           ENDIF
C=======================================================================
C  Here, if have incomplete data.  Fill in array HOUR with -1.
C=======================================================================
         IF(CODE(I).EQ.CCI) THEN
           DO 480 J = IMARK,I
           IIHR = (J-1)/IDIVD + 1
  480      HOUR(IDAY,IIHR) = -1
           IMARK = I + 1
           LPRT = 1
           PCODE(JPRT,4) = CCI
           GO TO 500
           ENDIF
        IF(CODE(I).EQ.CCE) THEN
C=======================================================================
C  Here, have estimated data.  Use estimated value.
C=======================================================================
           HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IFIX(YRAIN(I)*100.)
C#### WCH, 4/25/94.  NEED TO PUT INTENSITY ON M4, NOT DEPTH.
           IF(NEND.EQ.96.AND.YRAIN(I).GT.0.0) WRITE(M4) JULDAY,RHOUR,
     +        THIS,YRAIN(I)/0.25
           IMARK = I + 1
           LPRT = 1
           PCODE(JPRT,5) = CCE
           GO TO 500
           ENDIF
C=======================================================================
C  Here if have strange character.  Use precip. value and indicate 
C    in print-out.
C=======================================================================
        HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IFIX(YRAIN(I)*100.)
C#### WCH, 4/25/94.  NEED TO PUT INTENSITY ON M4, NOT DEPTH.
        IF(NEND.EQ.96.AND.YRAIN(I).GT.0.0) WRITE(M4) JULDAY,RHOUR,
     +     THIS,YRAIN(I)/0.25
        IMARK = I + 1
        LPRT = 1
        PCODE(JPRT,6) = QUEST  
      ENDIF
  500 CONTINUE
C=======================================================================
C  Check cumulative total for the day.
C=======================================================================
      IF(ABS(DAYTOT-TOT).GT.0.005) THEN
         PCODE(JPRT,7) = CCT
         LPRT = 1      
         ENDIF
      HOUR(IDAY,25) = NEWMON
      HOUR(IDAY,26) = NEWDAY
      HOUR(IDAY,27) = NWYR
      IF(LPRT.EQ.0) GO TO 420
C=======================================================================
C  Here, print out dates for various codes encountered in data.
C=======================================================================
        LDATEP(JPRT) = NEWMON*10000 + NEWDAY*100 + NWYR
        IF(LLLPRT.EQ.0.AND.NEWYR.EQ.IYBEG(1)) THEN
           WRITE(N6,9740)
           LLLPRT = 1
           ENDIF
        IF(JPRT.EQ.4) THEN
           WRITE(N6,9750) (LDATEP(I),(PCODE(I,K),K=1,7),I=1,4)
           JPRT = 0
           ENDIF
      GO TO 420
C=======================================================================
C  Here, have reached end of year or end of requested data.
C  Print out remaining error codes if needed.
C=======================================================================
  600 BACKSPACE IO
      IGO = 0
      GO TO 720
  700 IGO = 1 
  720 IF(JPRT.GT.0) WRITE(N6,9750) (LDATEP(I),(PCODE(I,K),K=1,7),
     1   I=1,JPRT)
      RETURN
C=======================================================================
C  Here if can't find desired date on the file.
C=======================================================================
 1000 WRITE(N6,9550) NSTA,STATE,NEWMON,NEWDAY,NEWYR
      WRITE(*,9550) NSTA,STATE,NEWMON,NEWDAY,NEWYR
      STOP
C=======================================================================
C  Here, likely trying to read EarthInfo file with wrong format.
C=======================================================================
 1200 WRITE(N6,9600) KSTA
      WRITE(*,9600)  KSTA
      STOP
C=======================================================================
C  Here, error while reading processed EarthInfo file.  Hard to guess
C    what's wrong.
C=======================================================================
 1300 IF(IDO.EQ.1) NEWYR = NWYR
C#### WCH, 8/4/95.  ALTER IOSTAT FOR LAHEY.
      WRITE(N6,9610) IO,KSTA,MOD(IOS,256),IDO,NEWMON,NEWDAY,NEWYR,
     +  YRAIN(NEND),CODE(NEND),DAYTOT
      WRITE(*,9610)  IO,KSTA,MOD(IOS,256),IDO,NEWMON,NEWDAY,NEWYR,
     +  YRAIN(NEND),CODE(NEND),DAYTOT
      STOP
C=======================================================================
 9000 FORMAT (A2,1X,I4,1X,I1,1X,A2,1X,I2,1X,I2,3X,I2,1X,600A1)
 9010 FORMAT (A2,1X,I3,1X,I1,1X,A2,1X,I2,1X,I2,3X,I2,1X,600A1)
 9100 FORMAT (/,' Searching unit',I3,' for correct dates for station',
     1 I5,//)
 9110 FORMAT(/,' Correct station, dates and format of input file.',/,
     1' Rewind unit',I3,' to continue precipitation data processing.',/)
 9120 FORMAT ('+Converting to proper ASCII format for month',I3,
     1 ' of year',I5)
 9250 FORMAT (A1,A2,2A1,I4,A1,I1,2A1,A2,2A1,2(I2,A1),(I4,A1),700A1)
 9510 FORMAT(/,' ERROR. Station number,',I5,' on EarthInfo ASCII file',
     1 /,' does not correspond to requested number,',I5,' for state ',
     2 A2,'.',/,' Check JIN data file and try again.  Run stopped.')
 9520 FORMAT(/,' ERROR.  Line in input file has more than',I4,
     1' characters.',/,' Shorten line and input EarthInfo ASCII file aga
     2in.',/,' Line is for: State= ',A2,', Station=',I5,' MO/DA/YR = ',
     3 I2,'/',I2,'/',I4,/,' Run stopped.')
 9530 FORMAT(/,' ERROR.  Line being written to scratch file',I3,
     1 ' has more than',I4,' characters.',/,
     2 ' Shorten line and input EarthInfo ASCII file again.',/,
     3 ' Line is for: State= ',A2,', Station=',I5,' MO/DA/YR = ',
     4 I2,'/',I2,'/',I4,/,' Run stopped.')
 9550 FORMAT(/,' ERROR. Run about to be stopped because cannot match req
     1uested dates (or station) with',/,
     2 ' EarthInfo ASCII file dates (or station) for:',/,
     3 ' File Station No.=',I5,', File State= ',
     4 A2,'. Last mo/da/yr on file= ',I2,'/',I2,'/',I4)
 9600 FORMAT(/,' ERROR in first try reading EarthInfo input file on unit
     1',I3,/,' Likely caused by requested station value,',I3,
     2 ', that is not 3 or 4 digits long. --',/,
     3 ' Requested value must match value on file in length.',/,
     4 ' Run stopped.')
 9610 FORMAT (/,' ERROR while reading processed EarthInfo ASCII file on 
     1unit',I3,/,' for requested station',I5,/,
     2 ' IOSTAT=',I5,' IDO=',I2,' NEWMON=',I3,' NEWDAY=',I3,' NEWYR=',
     3 I3,/,' Last three values read from record (rain, code, total)='
     4 ,F5.2,1X,A1,F5.2,/,' Review data.  Run stopped.')
 9740 FORMAT (//,' DATES (MO/DA/YR) OF FLAGS IN PRECIPITATION DATA FROM 
     1PROCESSED EARTHINFO ASCII FILE:',/,
     2 ' COLUMN  FLAG  MEANING',/,
     3 '   1       A   ACCUMULATED VALUE(S) AVERAGED OVER PRECEDING HOUR
     4S',/,
     5 '   2       M   MISSING DATA',/,
     6 '   3       D   DELETED DATA',/,
     7 '   4       I   INCOMPLETE DATA',/,
     8 '   5       E   ESTIMATED DATA',/,
     9 '   6       ?   UNKNOWN FLAG',/,
     1 '   7       T   DAILY TOTAL ON FILE DOES NOT MATCH CALCULATED TOT
     2AL',/)
 9750 FORMAT (4(I9,1X,'(',7A1,')'))
      END
