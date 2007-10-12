C
C
C
      SUBROUTINE   TMAXIS
     I                    (XLEN,YMLEN,ALEN,SIZEL,NCRV,TSTEP,
     I                     TUNITS,SDATIM,EDATIM,DTYPE,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine selects which time axis routine to call based
C     on the time step and number of values to plot.  It calls
C     routines for annual, monthly, daily, or minute data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NCRV,SDATIM(6),EDATIM(6)
      INTEGER   TSTEP(NCRV),TUNITS(NCRV),DTYPE(NCRV)
      REAL      XLEN,YMLEN,SIZEL,BOTOM, ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YMLEN  - length of main Y-axis, in world coordinates
C              auxilary axis plus small space between, in world
C              coordinates
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C     SIZEL  - height of lettering, in world coordinates
C     NCRV   - number of curves
C     TSTEP  - time step in TUNITS units
C     TUNITS - time units code
C              1 - seconds     5 - months
C              2 - minutes     6 - years
C              3 - hours       7 - centuries
C              4 - days
C     SDATIM - start date (yr,mo,dy,hr,mn,sc)
C     EDATIM - end date (yr,mo,dy,hr,mn,sc)
C     DTYPE  - type of time series (1-point, 2-mean)
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,DT(6),TUMIN,TSMIN,NPTS,K,NDAYS,L5,L1,TMP,FLG1,FLG2,
     $          TSTYPE
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMDIF, YRAXIS, MOAXIS, DYAXIS, MIAXIS, GSCHUP
      EXTERNAL   CMPTIM
C
C     + + + END SPECIFICATIONS + + +
C
      L5   = 5
      L1   = 1
      CALL GSCHUP (0.0,1.0)
C     determine if any time series are point
      TSTYPE = 1
      DO 2 I = 1,NCRV
        IF (DTYPE(I) .EQ. 2) TSTYPE = 2
 2    CONTINUE
C     determine minimum time units
      TUMIN = TUNITS(1)
      TSMIN = TSTEP(1)
      IF (NCRV .GT. 1) THEN
        DO 4 K = 2,NCRV
          CALL CMPTIM (TUMIN,TSMIN, TUNITS(K),TSTEP(K), FLG1,FLG2)
          IF (FLG2 .EQ. 2) THEN
C           found shorter time interval
            TUMIN = TUNITS(K)
            TSMIN = TSTEP(K)
          END IF
 4      CONTINUE
      END IF
C
      IF (TUMIN.EQ.2 .AND. TSMIN.EQ.1440) THEN
C       daily time step
        TUMIN= 4
        TSMIN= 1
      END IF
      IF (TUMIN.EQ.3) THEN
C       convert hour to minutes
        TUMIN= 2
        TSMIN= 60* TSMIN
      END IF
C
      IF (TUMIN.EQ.2) THEN
C       check case of several months with short time interval
        CALL TIMDIF (SDATIM,EDATIM,L5,L1,TMP)
        IF (TMP.GE.3) THEN
          TUMIN= 4
          TSMIN= 1
        END IF
      END IF
C
      DO 10 I = 1,6
        DT(I) = SDATIM(I)
 10   CONTINUE
C
      CALL TIMDIF (SDATIM,EDATIM,4,L1,NDAYS)
      IF (SDATIM(3).EQ.1 .AND. SDATIM(4).EQ.24) NDAYS= NDAYS+ 1
C
      IF (TUMIN .EQ. 5) THEN
C       NPTS must be number of months not n-month time steps
        CALL TIMDIF (SDATIM,EDATIM,TUMIN,L1,NPTS)
      ELSE
        CALL TIMDIF (SDATIM,EDATIM,TUMIN,TSMIN,NPTS)
      END IF
C
      IF (TUMIN.GT.5) THEN
C       annual data
        CALL YRAXIS (XLEN,YMLEN,NPTS,SIZEL,DT(1),ALEN,TSTYPE,BOTOM)
      ELSE IF (TUMIN.GT.4) THEN
C       monthly data
        CALL MOAXIS (XLEN,YMLEN,NPTS,SIZEL,ALEN,DT(1),DT(2),NDAYS,
     &               TSTYPE,BOTOM)
      ELSE IF (TUMIN.GT.3) THEN
C       daily data
        CALL DYAXIS (XLEN,YMLEN,NDAYS,SIZEL,ALEN,DT(1),DT(2),DT(3),
     &               BOTOM)
      ELSE
C       hourly or min data
        CALL MIAXIS (XLEN,YMLEN,TSMIN,NPTS,SIZEL,SDATIM,ALEN,BOTOM)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MIAXIS
     I                    (XLEN,YLEN,TSMIN,NPTS,SIZEL,SDATIM,ALEN,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     for time steps less than one day
C     This routine costructs a horizontal time-axis using
C     hour/minutes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPTS,SDATIM(6),TSMIN
      REAL      XLEN,SIZEL,YLEN,BOTOM,ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     TSMIN  - timestep, in minutes
C     NPTS   - number of intervals to plot
C     SIZEL  - height of lettering, in world coordinates
C     SDATIM - start date (yr,mo,dy,hr,mn,sc)
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MPIN,I,UB,IOPT,DT(6),INTV,J,INTH,CUMM,DD
      INTEGER      JUST, OLEN, LEN, TBFLG, IHV, DCHK(6)
      INTEGER      I2, PASS2, MYRFLG
      REAL         XP,XPOS,YPOS,SPACE,FRACT,XINC,DPIN,X(2),Y(2),
     1             INPD, YPOSMX
      CHARACTER*4  CHOUR
      CHARACTER*18 CSTR
      CHARACTER*1  C1(4)
      CHARACTER*4 MON(12)
      CHARACTER*1 M(12)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
      REAL   ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT, REAL, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DATNXT, DTSTRG, TMTICS, ANGTX, ANGTXA, GPL, INTCHR
      EXTERNAL   ICHOUR, DAYMON, HVXLIN, ANGLEN
C
C     + + + DATA INITIALIZATIONS + + + 
      DATA MON/'JAN ','FEB ','MAR ','APR ','MAY ','JUNE',
     #         'JULY','AUG ','SEPT','OCT ','NOV ','DEC '/
      DATA M/'J','F','M','A','M','J','J','A','S','O','N','D'/
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
      DO 10 I= 1,6
        DT(I)= SDATIM(I)
 10   CONTINUE
C
      MPIN= INT(REAL(TSMIN*NPTS)/XLEN)
C     minutes per inch
      DPIN= REAL(MPIN)/1440.0
C     days per inch
      XINC= XLEN/REAL(NPTS)
      CUMM= 0
C
C     draw bottom and top lines of axes
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
C     check spacing
      DCHK(1) = 1999
      DCHK(2) = 9
      DCHK(3) = 30
      DCHK(4) = 0
      DCHK(5) = 0
      DCHK(6) = 0
      IOPT = 2
C     compute inches per day
      INPD = 1.0/DPIN
      CALL DTSTRG (DCHK,IOPT,INPD,SIZEL,LEN,CSTR)
C
      IF (LEN .LE. 10) THEN
C       can't fit y/m/d under 1 day, so just put day and
C       label month/year for each month if space,
C       usually more than 1 day per inch
        IF (DPIN .LT. 3) THEN
C         print every day
          DD= 1
        ELSE IF (DPIN .LT. 6) THEN
C         print every other day
          DD= 2
        ELSE IF (DPIN .LT. 15) THEN
          DD= 5
        ELSE IF (DPIN .LT. 30) THEN
          DD= 10
        ELSE
          DD= 99
        END IF
C
        MYRFLG = 0
        PASS2 = 0
        YPOSMX = -5.0
        DO 20 I=1,NPTS
C         increment time
          UB = 1
          CALL DATNXT(TSMIN,UB,DT)
          XP = REAL(I)*XINC
          CUMM = CUMM + 1
          IF ((DT(4).EQ.24.AND.DT(5).EQ.0.AND.
     &               DT(3).EQ.DAYMON(DT(1),DT(2))) .OR. I.EQ.NPTS) THEN
C           label with month and year
            SPACE = REAL(CUMM)*XINC
            IOPT = 1
            CALL DTSTRG (DT,IOPT,SPACE,SIZEL,LEN,CSTR)
            IF (SPACE .GT. 9.0*SIZEL) THEN
C             enough space for printing the month/year
              IOPT = 1
              CALL DTSTRG (DT,IOPT,SPACE,SIZEL,LEN,CSTR)
              XPOS = XP - 0.5*SPACE -0.5*ANGLEN(LEN,CSTR)
              YPOS = -3.5*SIZEL
              IHV = 1
              CALL ANGTX (XPOS,YPOS,CSTR,IHV)
              MYRFLG = 1
            ELSE IF (SPACE .GT. 4.0*SIZEL) THEN
C             put abbreviated month
              LEN = 4
              XPOS = XP - 0.5*SPACE -0.5*ANGLEN(LEN,MON(DT(2)))
              YPOS = -3.5*SIZEL
              IHV = 1
              CALL ANGTX (XPOS,YPOS,MON(DT(2)),IHV)
            ELSE IF (SPACE .GT. SIZEL) THEN
C             just put month character
              LEN = 1
              XPOS = XP - 0.5*SPACE -0.5*ANGLEN(LEN,M(DT(2)))
              YPOS = -3.5*SIZEL
              IHV = 1
              CALL ANGTX (XPOS,YPOS,M(DT(2)),IHV)
            END IF
C
            IF (DT(2) .EQ. 12 .OR. I .EQ. NPTS) THEN
C             write year if needed       
              IF (MYRFLG.EQ.0 .AND. PASS2.EQ.1) THEN
C               did not use year month on one line
                LEN = 4
                JUST = 1
                IF (365.0/DPIN .LT. XP) THEN
C                 there is a full year on the plot
                  CALL INTCHR (DT(1),LEN,JUST,OLEN,C1)
                  XPOS = XP - 0.5*365.0/DPIN -0.5*ANGLEN(LEN,C1)    
                  YPOS = -5.0*SIZEL
                  IHV = 1
                  CALL ANGTXA (XPOS,YPOS,LEN,C1,IHV)
                  YPOSMX = -6.5
                ELSE IF (3.5*SIZEL .LT. XP) THEN
C                 room for partial year
                  CALL INTCHR (DT(1),LEN,JUST,OLEN,C1)
                  XPOS = XP - 0.5*XP -0.5*ANGLEN(LEN,C1)     
                  YPOS = -5.0*SIZEL
                  IHV = 1
                  CALL ANGTXA (XPOS,YPOS,LEN,C1,IHV)
                  YPOSMX = -6.5
C               ELSE
C                 no room for year               
                END IF
              END IF
            END IF
C
            IF (I .GT. 1 .AND. I .LT. NPTS) THEN
C             put month mark if not beginning or end
              X(1)= XP
              Y(1)= -2.0 * SIZEL
              X(2)= XP
              Y(2)= -4.0 * SIZEL
              CALL GPL (I2,X,Y)
            END IF
            CUMM = 0
            PASS2 = 1
            FRACT= 0.75
            CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
          END IF
C
          IF (DT(4).EQ.12.AND.DT(5).EQ.0.AND.DD.EQ.1) THEN
C           print day between tics
            XPOS = XP - SIZEL
            IF (DT(3).LT.10) XPOS = XP - 0.5*SIZEL
            YPOS= -1.5*SIZEL
            JUST= 1
            LEN = 4
            CALL INTCHR (DT(3),LEN,JUST,OLEN,C1)
            IHV = 1
            CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
          ELSE IF (DT(4).EQ.24.AND.DT(5).EQ.0 .AND.  
     $             MOD(DT(3),DD).EQ.0) THEN
            FRACT = 0.5
            CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
            IF (DD .GT. 1) THEN
C             print day on tics
              JUST= 1
              LEN = 4
              CALL INTCHR (DT(3),LEN,JUST,OLEN,C1)
              XPOS= XP- 0.5*ANGLEN(OLEN,C1)
              YPOS= -1.5* SIZEL
              IHV = 1
              CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
            END IF
          END IF
C
 20     CONTINUE
C
C       write title on axis.
        XPOS= XLEN/2.0 - SIZEL*6.0
        YPOS= YPOSMX*SIZEL
        IHV = 1
        CALL ANGTX (XPOS,YPOS,'TIME, IN DAYS',IHV)
C
      ELSE
C       can fit y/m/d under each day, usually
C       when 1 day is greater then 1 inches.
C       compute frequency of minute tics if any
        INTV = 999
        INTH = 1
        IF (MPIN .GT. 720) THEN
          INTH = 12
        ELSE IF (MPIN .GT. 360) THEN
          INTH = 6
        ELSE IF (MPIN .GT. 180) THEN
          INTH = 4
        ELSE IF (MPIN .GT.90) THEN
          INTH = 2
        ELSE IF (MPIN .GT. 40) THEN
          INTH = 1
        ELSE IF (MPIN .GT. 30) THEN
C         use minute tics
          INTV = 30
        ELSE IF (MPIN .GT. 20) THEN
          INTV = 15
        ELSE IF (MPIN .GT. 10) THEN
          INTV = 10
        ELSE IF (MPIN .GT. 5) THEN
          INTV = 5
        ELSE IF (MPIN .GT. 3) THEN
          INTV = 2
        ELSE
          INTV = 1
        END IF
C
C       put hour label at first tic
        XP = 0.0
        IF (DT(5).EQ.0 .AND. MOD(DT(4),INTH).EQ.0) THEN
C         print hour.
          FRACT = 0.75
          CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
          XPOS = XP - 1.6*SIZEL
          YPOS= -2.0*SIZEL
          CALL ICHOUR (DT(4)*100, CHOUR)
          IHV = 1
          CALL ANGTX (XPOS,YPOS,CHOUR,IHV)
        ELSE IF (MOD(DT(5),INTV) .EQ. 0 .AND. INTV .NE. 999) THEN
C         print minute
          FRACT = 0.5
          CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
          XPOS = XP -1.6*SIZEL
          YPOS= -2.0*SIZEL
          J = 100*DT(4) + DT(5)
          CALL ICHOUR (J, CHOUR)
          IHV = 1
          CALL ANGTX (XPOS,YPOS,CHOUR,IHV)
        END IF
C
        DO 40 I=1,NPTS
C         increment time.
          UB = 1
          CALL DATNXT (TSMIN,UB,DT)
          XP = REAL(I)*XINC
          CUMM = CUMM + 1
          IF ((DT(4).EQ.24.AND.DT(5).EQ.0) .OR. (I.EQ.NPTS)) THEN
C           label the day
            SPACE = REAL(CUMM)*XINC
            IOPT = 2
            CALL DTSTRG (DT,IOPT,SPACE,SIZEL,LEN,CSTR)
            IF (SPACE .GT. REAL(LEN)*SIZEL) THEN
C             enough space for printing the month,day,year
              XPOS = XP - 0.5*SPACE -0.5*ANGLEN(LEN,CSTR)
              YPOS = -4.0*SIZEL
              IHV = 1
              CALL ANGTX (XPOS,YPOS,CSTR,IHV)
            END IF
            IF (I .GT. 1 .AND. I .LT. NPTS) THEN
C             put day mark if not beginning or end
              X(1) = XP
              Y(1) = -2.5 * SIZEL
              X(2) = XP
              Y(2) = -4.5 * SIZEL
              CALL GPL (I2,X,Y)
            END IF
            CUMM = 0
          END IF
          IF (DT(5).EQ.0 .AND. MOD(DT(4),INTH).EQ.0) THEN
C           print hour.
            FRACT = 0.75
            CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
            XPOS = XP - 1.6*SIZEL
            YPOS= -2.0*SIZEL
            CALL ICHOUR (DT(4)*100, CHOUR)
            IHV = 1
            CALL ANGTX (XPOS,YPOS,CHOUR,IHV)
          ELSE IF (MOD(DT(5),INTV) .EQ. 0 .AND. INTV .NE. 999) THEN
C           print minute
            FRACT = 0.5
            CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
            XPOS = XP -1.6*SIZEL
            YPOS= -2.0*SIZEL
            J = 100*DT(4) + DT(5)
            CALL ICHOUR (J, CHOUR)
            IHV = 1
            CALL ANGTX (XPOS,YPOS,CHOUR,IHV)
          END IF
C
 40     CONTINUE
C
C       write title on axis.
        XPOS = XLEN/2.0 - 6.0*SIZEL
        YPOS = -6.5*SIZEL
        IHV = 1
        CALL ANGTX (XPOS,YPOS,'TIME, IN HOURS',IHV)
      END IF
C
      BOTOM = YPOS
C
      RETURN
      END
C
C
C
      SUBROUTINE   DYAXIS
     I                    (XLEN,YLEN,NPTS,SIZEL,ALEN,
     M                     LY,LM,LD,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws the time axis using days, months and
C     years.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPTS,LD,LM,LY
      REAL XLEN,SIZEL,YLEN,BOTOM,ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or length of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     NPTS   - number of days
C     SIZEL  - height of lettering, in world coordinates
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C     LY     - year
C     LM     - month
C     LD     - day
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CN,I,CUMM,JUST,LEN,OLEN,TBFLG,IHV,DPM,ILY,ILM
      INTEGER     I2, LLEN(12), SLEN(12), I1, I4, YCHK, I12,ILD
      REAL        INPMO,INPDY,XP,XPOS,YPOS,SPACE,FRACT,X(2),Y(2)
      REAL        CLEN,INPYR,YPOSDY,YPOSMO,YPOSYR,AVAIL,NEED
      CHARACTER*9 MONTH(12)
      CHARACTER*4 MON(12)
      CHARACTER*1 M(12), C1(4)
      LOGICAL     PRTDY, PRTMO
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
      REAL      ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, MOD, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   TMTICS,DAYMON,HVXLIN, GPL, ANGTX, ANGTXA, GSCHH, INTCHR
      EXTERNAL   ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MONTH/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     #           'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     #           'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA LLEN/7,8,5,5,3,4,4,6,9,7,8,8/
      DATA MON/'JAN ','FEB ','MAR ','APR ','MAY ','JUNE',
     #         'JULY','AUG ','SEPT','OCT ','NOV ','DEC '/
      DATA SLEN/3,3,3,3,3,4,4,3,4,3,3,3/
      DATA M/'J','F','M','A','M','J','J','A','S','O','N','D'/
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I2= 2
      I4= 4
      I12=12
      ILY = LY
      ILM = LM
      ILD = LD
C
C     inches per day
      INPDY= XLEN/REAL(NPTS)
C     inches per month
      INPMO= 30.0*INPDY
C     inches per year
      INPYR = 365.0*INPDY
C     offset if days numbered on axis, and flag for tics
      PRTDY= .TRUE.
C     CN is frequency of days on axis label
      IF (INPMO .GT. 100.0*SIZEL) THEN
        CN   = 1
      ELSE IF (INPMO .GT. 50.0*SIZEL) THEN
        CN = 2
      ELSE IF (INPMO .GT. 24*SIZEL) THEN
        CN= 5
      ELSE IF (INPMO .GT. 10.0*SIZEL) THEN
        CN = 10
      ELSE IF (INPMO .GT. 7.0*SIZEL) THEN
        CN = 15
      ELSE
C       no days will be printed
        CN = 32
        PRTDY = .FALSE.
      END IF
C
C     set year labels every 1, 2, 5, 10, 20, ...years
      JUST= 0
      LEN = 4
      CALL INTCHR (LY,LEN,JUST,OLEN,C1)
      YCHK= INT(1.25*ANGLEN(I4,C1)/INPYR)+ 1
      IF (YCHK.EQ.4 .OR. YCHK.EQ.3) YCHK = 5
      IF (YCHK .GT. 5) YCHK = ((YCHK-1)/10)*10 + 10
C
C     set conditions and positions for axis labels
      IF (INPYR .GT. 1.1*ANGLEN(I12,M)) THEN
        PRTMO = .TRUE.
      ELSE
        PRTMO = .FALSE.
      END IF
      IF (PRTDY) THEN
        YPOSDY = -1.25*SIZEL
        YPOSMO = -2.75*SIZEL
        YPOSYR = -4.75*SIZEL
      ELSE IF (PRTMO) THEN
        YPOSDY = 0.0
        YPOSMO = -1.5*SIZEL
        YPOSYR = -3.5*SIZEL
      ELSE
        YPOSDY = 0.0
        YPOSMO = 0.0
        YPOSYR = -2.0*SIZEL
      END IF
C
C     draw axes lines across bottom and top
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
      LD  = LD- 1
      CUMM= 0
      DO 20 I= 1,NPTS
        LD= LD+ 1
        IF (LD.GT.DAYMON(LY,LM)) THEN
          LM= LM+ 1
          LD= 1
          IF (LM.GT.12) THEN
            LY= LY+ 1
            LM= 1
          END IF
        END IF
        XP= REAL(I)*INPDY
C
        IF (PRTMO) THEN
C         enough space to put more than just the year
          IF (LD.EQ.DAYMON(LY,LM)) THEN
C           print end-of-month marker
            FRACT= 0.75
            CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
          END IF
          IF (INPMO.GE.5.0*SIZEL) THEN
            IF (LD.EQ.15) THEN
C             print month
              IF ((LY.NE.ILY.OR.LM.NE.ILM) .AND. NPTS-I.GE.13) THEN
C               not first or last month so not special case
C               note NPTS-I is number of days after the 15th that
C               are left in the last month remaining in month, 
C               15+"13" = 28 in case last month is February 
                IF (INPMO.GE.10.0*SIZEL) THEN
C                 print month and spell out
                  XPOS= XP- 0.5*ANGLEN(LLEN(LM),MONTH(LM))
                  YPOS= YPOSMO              
                  IHV = 1
                  CALL ANGTX (XPOS,YPOS,MONTH(LM),IHV)
                ELSE
C                 use abbreviation
                  XPOS= XP- 0.5*ANGLEN(SLEN(LM),MON(LM))
                  YPOS= YPOSMO                
                  IHV = 1
                  CALL ANGTX (XPOS,YPOS,MON(LM),IHV)
                END IF
              END IF
            END IF
C
            IF (LD.EQ.DAYMON(LY,LM) .AND. LM.EQ.ILM. AND.
     $          LY.EQ.ILY) THEN
C             first month, check for space
              AVAIL = INPDY*REAL(DAYMON(LY,LM)-ILD+1)
              IF (INPMO .GE. 10.0*SIZEL) THEN
                NEED = ANGLEN(LLEN(LM),MONTH(LM))
              ELSE
                NEED = ANGLEN(SLEN(LM),MON(LM))
              END IF
              IF (NEED .LT. 1.1*AVAIL) THEN
C               print the month
                XPOS = XP - 0.5*AVAIL - 0.5*NEED
                YPOS = YPOSMO
                IHV = 1
                IF (INPMO .GE. 10.0*SIZEL) THEN
                  CALL ANGTX(XPOS,YPOS,MONTH(LM),IHV)
                ELSE
                  CALL ANGTX(XPOS,YPOS,MON(LM),IHV)
                END IF
              END IF
            END IF
C
            IF (I.EQ.NPTS .AND. LD.LT.28) THEN
C             last month with less than 28 days to plot
C             check for space
              AVAIL = INPDY*REAL(LD)              
              IF (INPMO .GE. 10.0*SIZEL) THEN
                NEED = ANGLEN(LLEN(LM),MONTH(LM))
              ELSE
                NEED = ANGLEN(SLEN(LM),MON(LM))
              END IF
              IF (NEED .LT. 1.1*AVAIL) THEN
C               print the month
                XPOS = XP - 0.5*AVAIL - 0.5*NEED
                YPOS = YPOSMO
                IHV = 1
                IF (INPMO .GE. 10.0*SIZEL) THEN
                  CALL ANGTX(XPOS,YPOS,MONTH(LM),IHV)
                ELSE
                  CALL ANGTX(XPOS,YPOS,MON(LM),IHV)
                END IF
              END IF
            END IF
C
            IF (PRTDY) THEN
              DPM = DAYMON(LY,LM)
              IF (MOD(LD,CN).EQ.0 .OR. LD.EQ.DPM) THEN
C               print every CN-TH day and last day of month
                XPOS= XP- 0.6* SIZEL
                IF (LD.LT.10) XPOS= XP- 0.3* SIZEL
                IF ((DPM-LD)*INPDY.GT.2.5*SIZEL .OR. LD.EQ.DPM) THEN
C                 enough space for day numbers
                  YPOS= YPOSDY        
                  CALL GSCHH (0.8 * SIZEL)
                  JUST= 1
                  LEN = 4
                  CALL INTCHR (LD,LEN,JUST,OLEN,C1)
                  IHV = 1
                  CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
                  CALL GSCHH (SIZEL)
                  FRACT= 0.50
                  CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
                END IF
              END IF
            END IF
          ELSE
C           for real small plots just label with first letter of the month.
            IF (LD.EQ.15) THEN
C             print first letter of the month
              CLEN = ANGLEN(I1,M(3))
              IF (CLEN*1.2 .LT. INPMO .OR. MOD(LM,2) .EQ. 0) THEN
C               enough room for month character
                XPOS= XP- 0.5*ANGLEN(I1,M(LM))
                YPOS= YPOSMO                
                IHV = 1
                CALL ANGTX (XPOS,YPOS,M(LM),IHV)
              END IF
            END IF
          END IF
        END IF
C
        CUMM= CUMM+ 1
        IF ((LM.EQ.12 .AND. LD.EQ.31) .OR. (I.EQ.NPTS)) THEN
          IF (MOD(LY,YCHK) .EQ. 0) THEN    
C           label the year
            LEN = 4
            JUST = 1
            CALL INTCHR (LY,LEN,JUST,OLEN,C1)          
            IF (REAL(CUMM)*INPDY.GT.1.25*ANGLEN(I4,C1)
     $          .OR. CUMM.GE.365) THEN
C             write year if full year or partial year
C             with enough space 
              SPACE = REAL(CUMM)*INPDY
              XPOS= XP- 0.5* SPACE- 0.5*ANGLEN(I4,C1)
              YPOS= YPOSYR               
              IF (LY .LT. 2100) THEN
C               not "generic" year so plot
                IHV = 1
                CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
              END IF
            END IF
          END IF
          IF (I .GT. 1 .AND. I .LT. NPTS) THEN
C           put year mark if not beginning or end
            IF (PRTMO) THEN
C             case where months are labeled, put mark between
C             years outside the plot
              Y(1)= -0.5 * SIZEL
              Y(2)= YPOSYR + 0.5 * SIZEL
            ELSE
C             just years labeled, put tics inside
              Y(1) = 0.0
              IF (MOD(LY,YCHK) .EQ. 0) THEN
                Y(2) = 0.75*SIZEL
              ELSE
                Y(2) = 0.5*SIZEL
              END IF
            END IF
            X(1)= XP
            X(2)= XP
            CALL GPL (I2,X,Y)
          END IF
          CUMM= 0
        END IF
 20   CONTINUE
C
      BOTOM= YPOSYR 
C
      RETURN
      END
C
C
C
      SUBROUTINE   MOAXIS
     I                   (XLEN,YLEN,NPTS,SIZEL,ALEN,
     M                    LY,LM,NDAYS,TSTYPE,
     O                    BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws a time axis using months and years.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL XLEN,SIZEL,YLEN,BOTOM,ALEN
      INTEGER   NPTS,LM,LY,NDAYS,TSTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     NPTS   - number of months
C     SIZEL  - height of lettering, in world coordinates
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C     LY     - year
C     LM     - month
C     NDAYS  - total number of days for axis
C     TSTYPE - time series type (2-point, 1-mean)
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,CUM,ID,CUMM,JUST,OLEN,TBFLG, IHV
      INTEGER   I2, LLEN(12), SLEN(12), I1, I4, ICHK
      REAL      XINC,XP,XPOS,YPOS,INPMO,SPACE,FRACT,X(2),Y(2)
      CHARACTER*9 MONTH(12)
      CHARACTER*4 MON(12)
      CHARACTER*1 M(12),C1(4)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
      REAL      ANGLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, INT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   TMTICS, DAYMON, HVXLIN, GPL, ANGTX, ANGTXA, INTCHR
      EXTERNAL   ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MONTH/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     #           'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     #           'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA LLEN/7,8,5,5,3,4,4,6,9,7,8,8/
      DATA MON/'JAN ','FEB ','MAR ','APR ','MAY ','JUNE',
     #            'JULY','AUG ','SEPT','OCT ','NOV ','DEC '/
      DATA SLEN/3,3,3,3,3,4,4,3,4,3,3,3/
      DATA M/'J','F','M','A','M','J','J','A','S','O','N','D'/
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1 
      I2= 2
      I4= 4
C
C     set year labels every 1, 2, 5, 10, 20, ...years
      XINC= XLEN/(REAL(NPTS)/12.0)
      JUST= 0
      CALL INTCHR (LY,I4,JUST,OLEN,C1)
      ICHK= INT(1.25*ANGLEN(I4,C1)/XINC)+ 1
      IF (ICHK.EQ.4 .OR. ICHK.EQ.3) ICHK = 5
      IF (ICHK .GT. 5) ICHK = ((ICHK-1)/10)*10 + 10
C
      XINC = XLEN/REAL(NDAYS)
C     XINC = inches per day on plot
      CUM  = 0
      INPMO= XINC* 30.0
C
C     draw lines at bottom and top
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
      LM  = LM- 1
      XP  = 0.0
      CUMM= 0
      DO 20 I= 1,NPTS
        LM= LM+ 1
        IF (LM.GT.12) THEN
          LY= LY+ 1
          LM= 1
        END IF
        ID= DAYMON(LY,LM)
C
        IF (INPMO.GE.10.0*SIZEL) THEN
C         print month and spell out
          XPOS= XP+ 0.5*REAL(ID)*XINC
          IF (TSTYPE .EQ. 1) XPOS = XPOS-0.5*ANGLEN(LLEN(LM),MONTH(LM))
          YPOS= -2.0* SIZEL
          IHV = 1
          CALL ANGTX (XPOS,YPOS,MONTH(LM),IHV)
        ELSE IF (INPMO.GE.5.0*SIZEL) THEN
C         print month, space for 4 letters
          XPOS= XP+ 0.5* REAL(ID)* XINC 
          IF (TSTYPE .EQ. 1) XPOS = XPOS - 0.5*ANGLEN(SLEN(LM),MON(LM))
          YPOS= -2.0* SIZEL
          IHV = 1
          CALL ANGTX (XPOS,YPOS,MON(LM),IHV)
        ELSE IF (INPMO .GE. SIZEL) THEN
C         for real small plots just label with first letter of the month.
C         print first letter of the month
          XPOS= XP+ 0.5* REAL(ID)* XINC
          IF (TSTYPE .EQ. 1) XPOS = XPOS - 0.5*ANGLEN(I1,M(LM))
          YPOS= -2.0* SIZEL
          IHV = 1
          CALL ANGTX (XPOS,YPOS,M(LM),IHV)
C       ELSE
C         months won't be shown since not enough space
        END IF
C
        CUM  = CUM+ DAYMON(LY,LM)
        XP   = REAL(CUM)* XINC
        IF (INPMO .GE. SIZEL .AND. LM.EQ.12) THEN
          FRACT= 0.75
        ELSE
          FRACT = 0.5
        END IF
        IF (INPMO .GE. SIZEL .OR. LM.EQ.12) THEN
          CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
        END IF
C
        CUMM = CUMM+ ID
        IF (LM.EQ.12 .OR. I.EQ.NPTS) THEN
C         label the year
          IF (INPMO .GE. SIZEL) THEN
C           add year when months were shown
            SPACE = REAL(CUMM)* XINC
            IF (SPACE.GT.6.0*SIZEL) THEN
C             enough space for printing the year
              XPOS= XP- 0.5* SPACE- 1.8* SIZEL
              YPOS= -4.0* SIZEL
              JUST= 0
              CALL INTCHR (LY,I4,JUST,OLEN,C1)
              IF (LY .LT. 2100) THEN
C               not "generic" year so plot
                IHV = 1
                CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
              END IF
            END IF
            IF (I.LT.NPTS) THEN
C             put year mark if not beginning or end
              X(1)= XP
              IF (TSTYPE .EQ. 1) THEN
                Y(1)= -1.0* SIZEL
              ELSE
                Y(1)= -2.5*SIZEL
              END IF
              X(2)= XP
              Y(2)= -4.0* SIZEL
              CALL GPL (I2,X,Y)
            END IF
            CUMM= 0
          ELSE
C           no months, just label years
            SPACE = REAL(CUMM)* XINC
            IF (MOD(LY,ICHK).EQ.0) THEN
C             print year
              IF (ICHK .LT. 5) THEN
                FRACT = 0.5
              ELSE
                FRACT= 0.75
              END IF
              CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
C
              CALL INTCHR (LY,I4,JUST,OLEN,C1)
              IF (TSTYPE .EQ. 1) THEN
C               center in space
                XPOS = XP - 0.5*SPACE - 0.5*ANGLEN(I4,C1)
              ELSE
C               center on point
                XPOS = XP - 0.5*ANGLEN(I4,C1)
              END IF
              YPOS  = -2.0* SIZEL
              IHV = 1
              IF (XPOS .GT. 0.0) THEN
C               check on first year
                CALL ANGTXA (XPOS,YPOS,OLEN,C1,IHV)
              END IF
            ELSE
              FRACT= 0.5
              CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
            END IF
            CUMM= 0
          END IF    
        END IF
 20   CONTINUE
C
      BOTOM= -4.0* SIZEL
C
      RETURN
      END
C
C
C
      SUBROUTINE   YRAXIS
     I                    (XLEN,YLEN,NPTS,SIZEL,YR,ALEN,TSTYPE,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws time axis for annual data using years.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPTS,YR,TSTYPE
      REAL      XLEN,SIZEL,FRACT,YLEN,BOTOM,ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between,
C              in world coordinates
C     NPTS   - number of points
C     SIZEL  - height of lettering, in world coordinates
C     YR     - starting year
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C     TSTYPE - type of time series (2-point, 1-mean)
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IYR,ICHK,JUST,LEN,OLEN,TBFLG, IHV,L4
      REAL        XINC,YP,XP
      CHARACTER*1 C1(4)
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, INT, MOD
C
C     + + + FUNCTIONS + + +
      REAL        ANGLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL    TMTICS, INTCHR, HVXLIN, ANGTXA, ANGLEN
C
C     + + + END SPECIFICATIONS + + +
C
      L4= 4
      XINC= XLEN/REAL(NPTS)
C     set year labels every 1, 2, 5, 10, 20, ...years
      JUST= 0
      LEN = 4
      CALL INTCHR (YR,LEN,JUST,OLEN,C1)
      ICHK= INT(1.25*ANGLEN(L4,C1)/XINC)+ 1
      IF (ICHK.EQ.4 .OR. ICHK.EQ.3) ICHK = 5
      IF (ICHK .GT. 5) ICHK = ((ICHK-1)/10)*10 + 10
C
C     draw axes lines at bottom and top
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
C     year
      IYR= YR- 1
C
      DO 20 I= 1,NPTS
        IYR= IYR+ 1
        XP = REAL(I)* XINC
        YP = 0.0
        IF (MOD(IYR,ICHK).EQ.0) THEN
C         print year
          IF (ICHK .LT. 5) THEN
            FRACT = 0.5
          ELSE
            FRACT= 0.75
          END IF
          CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
C
          CALL INTCHR (IYR,LEN,JUST,OLEN,C1)
          IF (TSTYPE .EQ. 1) THEN
C           center in space
            XP = XP - 0.5* XINC - 0.5*ANGLEN(L4,C1)
          ELSE
C           center on point
            XP = XP - 0.5*ANGLEN(L4,C1)
          END IF
          YP  = -2.0* SIZEL
          IHV = 1
          CALL ANGTXA (XP,YP,OLEN,C1,IHV)
        ELSE
          FRACT= 0.5
          CALL TMTICS (XP,YLEN,SIZEL,FRACT,ALEN)
        END IF
 20   CONTINUE
C
      BOTOM= -2.0* SIZEL
C
      RETURN
      END
C
C
C
      SUBROUTINE   TMTICS
     I                    (XP,YLEN,SIZEL,FRACT,ALEN)
C
C     + + + PURPOSE + + +
C     This routine places tic marks for time on botton and top
C     of main plot and on auxilary plot if used.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL      XP,YLEN,SIZEL,FRACT,ALEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XP     - horizontal location on plot, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between,
C              in world coordinates
C     SIZEL  - height of lettering, in world coordinates
C     FRACT  - tic size, as a fraction of SIZEL
C     ALEN   - auxilary plot axis length
C              0 - no auxilary plot
C              1 - auxilary plot
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2
      REAL      TSIZ,BASE,XPT(2),YPT(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GPL
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
      TSIZ  = SIZEL* FRACT
      XPT(1)= XP
      YPT(1)= 0.0
      XPT(2)= XP
      YPT(2)= TSIZ
      CALL GPL (I2,XPT,YPT)
C     top line main plot
      YPT(1)= YLEN- TSIZ
      YPT(2)= YLEN
      CALL GPL (I2,XPT,YPT)
C     tics for auxiliary plot
      IF (ALEN .GT. 0.0001) THEN
        BASE  = YLEN+ 1.5* SIZEL
C       bottom of aux plot
        YPT(1)= BASE
        YPT(2)= BASE+ 0.6* TSIZ
        CALL GPL (I2,XPT,YPT)
C       top of aux plot
        YPT(1)= BASE+ ALEN- 0.6* TSIZ
        YPT(2)= BASE+ ALEN
        CALL GPL (I2,XPT,YPT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LGAXIS
     I                    (XLEN,YLEN,LBAXIS,SIZEL,MINY,MAXY,LFTRT,AUX)
C
C     + + + PURPOSE + + +
C     This routine draws a logrithmic Y-axis on the left or
C     right side of the plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MAXY,MINY,LFTRT
      REAL        XLEN,YLEN,SIZEL,AUX
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or length of main Y-axis and
C              auxilary axis plus small space between,
C              in world coordinates
C     LBAXIS - character string for axis label.
C     SIZEL  - height of lettering, in world coordinates
C     MINY   - log base 10 for minimum value for axis
C     MAXY   - log base 10 for maximum value for axis
C     LFTRT  - axis indicator
C              1 - left axis
C              2 - right axis
C     AUX    - location of label if < 0.0 for alignment with auxilary
C              plot
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,LEN,OLEN,SIGDIG,DECPLA,IFLG,IDUM, IHV,ENOTE
      INTEGER     I2, JUST
      REAL        XP,YDELT,YP,YVALUE,XPMIN,YPP,XS,XPT(2),YPT(2),
     1            M1,P1,ZERO
      CHARACTER*1 STR(14)
C
C     + + + FUNCTION + + +
      REAL      ANGLEN
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ALOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL    DECCHX, CLNSFT, LABYAX, GPL, ANGTXA, GSCHUP, HVYLIN
      EXTERNAL   INTCHR, ANGLEN, GSCHH, ADCOMA, LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
      M1    = -1.0
      P1    = 1.0
      ZERO  = 0.0
      I2    = 2
      DECPLA= 5
      SIGDIG= 4
      LEN   = 14
      JUST = 1
C
C     draw axis line
      CALL HVYLIN (XLEN,YLEN,LFTRT)
C
C     draw axis tics
      IF (LFTRT.EQ.2) THEN
        XP= XLEN
        XS= -SIZEL
      ELSE
        XS= SIZEL
        XP= 0.0
      END IF
      YP   = 0.0
      YDELT= YLEN/REAL(MAXY-MINY)
      DO 20 I = (MINY+1),MAXY
C       put tics along y-axis
        IF (0.12*YDELT .GT. SIZEL) THEN
C         put additional tics
          DO 15 J = 2,9
            YPP   = YP+ ALOG10(REAL(J))*YDELT
            XPT(1)= XP
            YPT(1)= YPP
            XPT(2)= XP+ 0.3* XS
            YPT(2)= YPP
            CALL GPL (I2,XPT,YPT)
 15       CONTINUE
        END IF
        YP    = YP+ YDELT
        XPT(1)= XP
        YPT(1)= YP
        XPT(2)= XP+ 0.5* XS
        YPT(2)= YP
        CALL GPL (I2,XPT,YPT)
 20   CONTINUE
C
C     label log Y-axis
      YP   = 0.0
      XPMIN= -SIZEL
      IFLG = 1
C
      ENOTE = 0
      IF (MINY.LT.3 .OR. MAXY.GT.5) ENOTE = 1
      DO 30 I= MINY,MAXY
        IF (ENOTE .EQ. 0) THEN
C         reasonable size numbers
          YVALUE= 10.0**I
          CALL DECCHX (YVALUE,LEN,SIGDIG,DECPLA,STR)
          CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
          IF (YVALUE .GE. 1000.0) THEN
            CALL ADCOMA (LEN,STR)
            OLEN = LENSTR (LEN,STR)
          END IF
          OLEN = LENSTR (LEN,STR)
          XP = -ANGLEN(OLEN,STR) - 0.8*SIZEL
          IF (XP.LT.XPMIN) XPMIN= XP
          IF (LFTRT.EQ.2) XP= XLEN+ 0.8*SIZEL
          YPP= YP- 0.5* SIZEL
          IHV = 1
          CALL ANGTXA (XP,YPP,OLEN,STR,IHV)
        ELSE
C         big numbers
          STR(1) = '1'
          STR(2) = '0'
          LEN = 3
          CALL INTCHR (I, LEN, JUST, OLEN, STR(3))
          IF (I.EQ.0 .OR. I.EQ.1) OLEN = 0
          LEN = 2
          IF (I.EQ.0) LEN = 1
          XP = -ANGLEN(LEN,STR) - 0.8*SIZEL       
          IF (OLEN .GT. 0) THEN
C           add space for exponent
            XP = XP - 0.7*ANGLEN(OLEN,STR(3))
          END IF
          IF (XP .LT. XPMIN) XPMIN = XP
          IF (LFTRT .EQ. 2) XP = XLEN + 0.8*SIZEL
          YPP = YP - 0.5*SIZEL
          IHV = 1
          CALL ANGTXA (XP, YPP, LEN, STR, IHV)
          IF (OLEN .GT. 0) THEN
C           write exponent
            XP = XP + ANGLEN(LEN,STR(1))
            YPP = YPP + 0.65*SIZEL
            CALL GSCHH (0.7*SIZEL)
            CALL ANGTXA (XP, YPP, OLEN, STR(3), IHV)
            CALL GSCHH (SIZEL)
          END IF
        END IF
        YP = YP+ YDELT
 30   CONTINUE
C
C     set to write up side
      CALL GSCHUP (M1,ZERO)
C     title axis
      CALL LABYAX (LBAXIS,SIZEL,AUX,XPMIN,LFTRT,XLEN,YLEN)
C     back to horiz write
      CALL GSCHUP (ZERO,P1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ARAXIS
     I                    (XLEN,YLEN,LBAXIS,SIZEL,YMIN,YMAX,LFTRT,AUX,
     I                     TICS)
C
C      + + + PURPOSE + + +
C     This routine draws an arithmetic Y-axis on the left or
C     right hand side.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LFTRT, TICS
      REAL        YMIN,YMAX,SIZEL,YLEN,XLEN,AUX
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between,
C              in world coordinates
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering, in world coordinates
C     YMIN   - array of minimum values
C     YMAX   - array of maximum values
C     LFTRT  - axis indicator
C              1 - left axis
C              2 - right axis
C     AUX    - location of label if < 0.0 for alignment with auxilary
C              plot
C     TICS   - number of tics on the axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LEN,OLEN,DECPLA,SIGDIG,IDUM,IFLG,MXDP,TP1,IHV,TM1
      INTEGER     I2
      REAL        XP,YDELT,YP,YVALUE,XPMIN,XS,YINC
      REAL        XPT(2),YPT(2),M1,P1,ZERO,YPP
      CHARACTER*1 STR(14),BLNK
C
C     + + + FUNCTION + + +
      REAL     ANGLEN
      INTEGER  LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHX,CLNSFT,LABYAX, ZIPC, GPL, ANGTXA, GSCHUP, HVYLIN
      EXTERNAL   ANGLEN, ADCOMA, LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
      I2    = 2
      M1    = -1.0
      P1    = 1.0
      ZERO  = 0.0
      BLNK  = ' '
      DECPLA= 5
      SIGDIG= 4
      LEN   = 14
C
C     draw axis line(s)
      CALL HVYLIN (XLEN,YLEN,LFTRT)
C
C     adjustment for prime bug
      IF (ABS(YMIN).LT.1.0E-8) YMIN= 0.0
C
C     draw axis tics
      IF (LFTRT.EQ.2) THEN
        XP= XLEN
        XS= -SIZEL
      ELSE
        XS= SIZEL
        XP= 0.0
      END IF
      YP= 0.0
C     number of tics on Y-axis
      TP1 = TICS + 1
      TM1 = TICS - 1
      YDELT= YLEN/REAL(TICS)
      DO 20 I = 1,TM1
        YP    = YP + YDELT
        XPT(1)= XP
        YPT(1)= YP
        XPT(2)= XP+ 0.5* XS
        YPT(2)= YP
        CALL GPL (I2,XPT,YPT)
 20   CONTINUE
C
C     label  Y-axis
      YP    = 0.0
      YVALUE= YMIN
      YINC  = (YMAX-YMIN)/REAL(TICS)
C     determine decimal places
      MXDP= 0
      IFLG= 1
      DO 25 I= 1,TP1
        CALL DECCHX (YVALUE,LEN,SIGDIG,DECPLA,STR)
        CALL CLNSFT (LEN, IFLG,STR,OLEN,IDUM)
        IF (IDUM.GT.MXDP) MXDP= IDUM
        YVALUE= YVALUE+ YINC
 25   CONTINUE
C
      DECPLA= MXDP
      YVALUE= YMIN
      IF (DECPLA.GT.0) IFLG= 0
      XPMIN = -SIZEL
      CALL ZIPC (LEN,BLNK,STR)
      DO 30 I= 1,TP1
        IF (I.EQ.1 .AND. ABS(YVALUE).LT.1.0E-6) THEN
          OLEN  = 1
          STR(1)= '0'
        ELSE
          CALL DECCHX (YVALUE,LEN,SIGDIG,DECPLA,STR)
          CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
          IF (YVALUE .GE. 1000.0) THEN
            CALL ADCOMA (LEN,STR)
            OLEN = LENSTR (LEN,STR)
          END IF
        END IF
        XP = -ANGLEN(OLEN,STR) - 0.8*SIZEL
        IF (XP.LT.XPMIN) XPMIN = XP
        IF (LFTRT.EQ.2) XP= XLEN+ 0.8* SIZEL
        YPP= YP- 0.5* SIZEL
        IHV = 1
        CALL ANGTXA (XP,YPP,OLEN,STR,IHV)
        YP = YP+ YDELT
        YVALUE= YVALUE+ YINC
 30   CONTINUE
C
C     write up the side
      CALL GSCHUP (M1,ZERO)
C     title axis
      CALL LABYAX (LBAXIS,SIZEL,AUX,XPMIN,LFTRT,XLEN,YLEN)
C     back to horiz write
      CALL GSCHUP (ZERO,P1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   LXAXIS
     I                    (XLEN,YLEN,LBAXIS,SIZEL,MINX,MAXX,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws a logrithmetic X-axis on top and bottom
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MAXX,MINX
      REAL        XLEN,SIZEL,YLEN,BOTOM
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between,
C              in world coordinates
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering, in world coordinates
C     MINX   - minimum value for X-axis
C     MAXX   - maximum value for X-axis
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,LEN,OLEN,DECPLA,SIGDIG,IFLG,IDUM,K,SIGN,TBFLG,IHV
      INTEGER     I2, ENOTE, JUST
      REAL        XP,XDELT,YP,XVALUE,XPP,XPT(3),YPT(3),ZERO,ONE,YPP,TLEN
      CHARACTER*1 STR(14)
C
C     + + + FUNCTION + + +
      REAL      ANGLEN
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ALOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHX, CLNSFT, LABXAX, GPL, ANGTXA, GSCHUP, HVXLIN
      EXTERNAL   INTCHR, ANGLEN, GSCHH, ADCOMA, LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
      I2    = 2
      ZERO  = 0.0
      ONE   = 1.0
      DECPLA= 5
      SIGDIG= 4
      LEN   = 14
      JUST = 1
C
C     set character up vector
      CALL GSCHUP (ZERO,ONE)
C
      XDELT = XLEN/REAL(MAXX-MINX)
C
C     draw axis lines
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
      DO 25 K= 1,2
        XP  = 0.0
        IF (K.EQ.1) THEN
C         draw axis tics on bottom
          YP  = 0.0
          SIGN= 1
        ELSE
C         draw axis tics on top
          YP  = YLEN
          SIGN= -1
        END IF
        DO 20 I= (MINX+1),MAXX
          IF (0.12*XDELT .GT. SIZEL) THEN
C           additional tics
            DO 15 J = 2,9
              XPP   = XP+ ALOG10(REAL(J))* XDELT
              XPT(1)= XPP
              YPT(1)= YP
              XPT(2)= XPP
              YPT(2)= YP+ 0.3* (SIGN*SIZEL)
              CALL GPL (I2,XPT,YPT)
 15         CONTINUE
          END IF
          XP    = XP+ XDELT
          XPT(1)= XP
          YPT(1)= YP
          XPT(2)= XP
          YPT(2)= YP+ 0.5* (SIGN*SIZEL)
          CALL GPL (I2,XPT,YPT)
 20     CONTINUE
 25   CONTINUE
C
C     label log X-axis on bottom.
      YP  = -2.0* SIZEL
      IFLG= 1
      ENOTE = 0
      IF (MINX.LT.3 .OR. MAXX.GT.5) ENOTE = 1
      DO 30 I=MINX,MAXX
        IF (ENOTE .EQ. 0) THEN
        XVALUE= 10.0**I
C         reasonable size numbers
          CALL DECCHX (XVALUE,LEN,SIGDIG,DECPLA,STR)
          CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
          IF (XVALUE .GE. 1000.0) THEN
            CALL ADCOMA (LEN,STR)
            OLEN = LENSTR (LEN,STR)
          END IF
          XP= REAL(I-MINX)*XDELT -0.5*ANGLEN(OLEN,STR)
          IHV = 1
          CALL ANGTXA (XP,YP,OLEN,STR,IHV)
        ELSE
C         big numbers
          STR(1) = '1'
          STR(2) = '0'
          LEN = 3
          CALL INTCHR (I, LEN, JUST, OLEN, STR(3))
          IF (I.EQ.0 .OR. I.EQ.1) OLEN = 0
          LEN = 2
          IF (I.EQ.0) LEN = 1
          IF (OLEN.GT.0) THEN
            TLEN= ANGLEN(OLEN,STR(3))
          ELSE
            TLEN= 0.0
          END IF
          XP = REAL(I-MINX)*XDELT -0.5*ANGLEN(LEN,STR) - 0.35*TLEN
          IHV = 1
          CALL ANGTXA (XP, YP, LEN, STR, IHV)
          IF (OLEN .GT. 0) THEN
C           write exponent
            XP = XP + ANGLEN(LEN,STR)
            YPP = YP + 0.65*SIZEL
            CALL GSCHH (0.7*SIZEL)
            CALL ANGTXA (XP, YPP, OLEN, STR(3), IHV)
            CALL GSCHH (SIZEL)
          END IF
        END IF
 30   CONTINUE
C
C     title axis on bottom.
      CALL LABXAX (LBAXIS,SIZEL,XLEN,BOTOM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   AXAXIS
     I                    (XLEN,YLEN,LBAXIS,SIZEL,XMIN,XMAX,TICS,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws an arithmetic X-axis.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TICS
      REAL        SIZEL,XLEN,XMIN,XMAX,YLEN,BOTOM
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering, in world coordinates
C     XMIN   - minimum value for X-axis
C     XMAX   - maximum value for X-axis
C     TICS   - number of tics on the axis
C     BOTOM  - location of botom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,K,LEN,OLEN,SIGDIG,DECPLA,IDUM,IFLG,MXDP,
     1            SIGN,TBFLG,TP1, IHV, TM1, ISPACE
      INTEGER     I2
      REAL        XP,XDELT,YP,XVALUE,XINC,XPT(2),YPT(2),ZERO,ONE,
     $            XLGE, SPACE
      CHARACTER*1 STR(14),BLNK
C
C     + + + FUNCTION + + +
      REAL      ANGLEN
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, REAL, INT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECCHX,CLNSFT,LABXAX, ZIPC, GPL, ANGTXA, GSCHUP, HVXLIN
      EXTERNAL   ANGLEN, ADCOMA, LENSTR
C
C     + + + END SPECIFICATIONS + + +
C
      ZERO  = 0.0
      ONE   = 1.0
      I2    = 2
      BLNK  = ' '
      DECPLA= 5
      SIGDIG= 4
      LEN   = 14
C
C     set character up vector
      CALL GSCHUP (ZERO,ONE)
C     adjustment for prime bug
      IF (ABS(XMIN).LT.1.0E-8) XMIN= 0.0
C
C     draw axis lines
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C
      TP1 = TICS + 1
      TM1 = TICS - 1
      XDELT= XLEN/REAL(TICS)
      DO 25 K= 1,2
        XP  = 0.0
        IF (K.EQ.1) THEN
C         draw axis tics on bottom
          YP  = 0.0
          SIGN= 1
        ELSE
C         draw axis tics on top
          YP  = YLEN
          SIGN= -1
        END IF
C       number of tics on X-axis
        DO 20 I = 1,TM1
          XP    = XP+ XDELT
          XPT(1)= XP
          YPT(1)= YP
          XPT(2)= XP
          YPT(2)= YP+ 0.5* (SIGN*SIZEL)
          CALL GPL (I2,XPT,YPT)
 20     CONTINUE
 25   CONTINUE
C
C     label X-axis on bottom
      YP    = -2.0*SIZEL
      XP    = 0.0
      XVALUE= XMIN
      XINC  = (XMAX-XMIN)/REAL(TICS)
C     determine decimal places
      MXDP= 0
      IFLG= 1
      DO 30 I= 1,TP1
        CALL DECCHX (XVALUE,LEN,SIGDIG,DECPLA,STR)
        CALL CLNSFT (LEN, IFLG,STR,OLEN,IDUM)
        IF (IDUM .GT. MXDP) MXDP= IDUM
        XVALUE= XVALUE+ XINC
 30   CONTINUE
      DECPLA= MXDP
      XVALUE= XMIN
      IF (DECPLA.GT.0) IFLG = 0
C
C     determine spacing 
      XLGE = XMAX
      IF (XMIN .GT. XLGE) XLGE = XMIN
      CALL DECCHX (XLGE,LEN,SIGDIG,DECPLA,STR)
      CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
      IF (XLGE .GE. 1000.0) THEN
        CALL ADCOMA (LEN,STR)
        OLEN = LENSTR (LEN,STR)
      END IF
C     get length
      SPACE = ANGLEN(OLEN,STR)
      ISPACE = INT((SPACE+SIZEL)/XDELT) + 1
C
      DO 40 I= 1,TP1
        IF (MOD(I+1,ISPACE) .EQ. 0) THEN
          CALL ZIPC (LEN,BLNK,STR)
          IF (I.EQ.1 .AND. ABS(XVALUE).LT.1.0E-6) THEN
            STR(1)= '0'
            OLEN  = 1
          ELSE
            CALL DECCHX (XVALUE,LEN,SIGDIG,DECPLA,STR)
            CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
            IF (XVALUE .GE. 1000.0) THEN
              CALL ADCOMA (LEN,STR)
              OLEN = LENSTR (LEN,STR)
            END IF
          END IF
          XP= REAL(I-1)* XDELT- 0.5*ANGLEN(OLEN,STR)
          IHV = 1
          CALL ANGTXA (XP,YP,OLEN,STR,IHV)
        END IF
        XVALUE= XVALUE+ XINC
 40   CONTINUE
C
C     title axis on bottom.
      CALL LABXAX (LBAXIS, SIZEL, XLEN, BOTOM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   RHTBDR
     I                    (TRANSF,TICS,XLEN,YLEN,SIZEL,MINX,MAXX)
C
C     + + + PURPOSE + + +
C     This routine draws a right border if a right axis is not needed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TRANSF, TICS, MINX, MAXX
      REAL      XLEN, YLEN, SIZEL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRANSF - type of axis
C              1 - arithmetic
C              2 - logrithmic
C     TICS   - number of tics for axis
C     XLEN   - length of X-axis, in world coordinates
C     YLEN   - length of y-axis or lenght of main Y-axis and
C              auxilary axis plus small space between, in world
C              coordinates
C     SIZEL  - height of lettering, in world coordinates
C     MINX   - minimum value for X-axis
C     MAXX   - maximum value for X-axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,LRFLG,TM1
      INTEGER   I2
      REAL      XP,YP,YDELT,YPP,XPT(3),YPT(3)
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL, ALOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL    GPL, HVYLIN
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
C     draw right line of axis
      LRFLG= 2
      CALL HVYLIN (XLEN,YLEN,LRFLG)
C
      IF (TRANSF.EQ.2) THEN
C       logarithmic tics drawn on right
        YDELT = YLEN/REAL(MAXX-MINX)
        XP    = XLEN
        YP    = 0.0
        DO 30 I= (MINX+1),MAXX
          IF (0.12*YDELT .GT. SIZEL) THEN
C           put additional tics
            DO 15 J= 2,9
              YPP   = YP+ ALOG10(REAL(J))* YDELT
              XPT(1)= XP
              YPT(1)= YPP
              XPT(2)= XP- 0.3* SIZEL
              YPT(2)= YPP
              CALL GPL (I2,XPT,YPT)
 15         CONTINUE
          END IF
          YP    = YP+ YDELT
          XPT(1)= XP
          YPT(1)= YP
          XPT(2)= XP- 0.5* SIZEL
          YPT(2)= YP
          CALL GPL (I2,XPT,YPT)
 30     CONTINUE
      ELSE IF (TICS .GT. 0) THEN
C       draw tics on the right for arithmetic scale
        TM1 = TICS - 1
        YDELT = YLEN/REAL(TICS)
        XP    = XLEN
        YP    = 0.0
        DO 40 I= 1,TM1
          YP    = YP+ YDELT
          XPT(1)= XP
          YPT(1)= YP
          XPT(2)= XP- 0.5* SIZEL
          YPT(2)= YP
          CALL GPL (I2,XPT,YPT)
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PBAXIS
     I                    (XLEN,YLEN,LBAXIS,SIZEL,STDEV,PORR,
     O                     BOTOM)
C
C     + + + PURPOSE + + +
C     This routine draws a normal probability axis.
C
C     + + + + DUMMY ARGUMENTS + + +
      INTEGER   PORR
      REAL        XLEN,SIZEL,STDEV,YLEN
      CHARACTER*1 LBAXIS(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN   - length of x axis in world coordinates
C     YLEN   - length of y axis in world coordinates
C     LBAXIS - character string for axis label
C     SIZEL  - height of lettering in world coordinates.
C     STDEV  - number of standard deviations to use for bounds on axis
C     PORR   - flag for type of labeling
C              1- percent probability  (exceedence)
C              2- recurrence interval  (exceedence)
C              3- probability as a fraction  (exceedence)
C              4- percent probability  (non-exceedence)
C              5- recurrence interval  (non-exceedence)
C              6- probability as a fraction  (non-exceedence)
C     BOTOM  - location of bottom line of label in world
C              coordinates below axis
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IFLG,LEN,SIGDIG,DECPLA,I,OLEN,IDUM,J,TBFLG,IMIN,
     #            IMAX, IHV
      INTEGER     I2
      REAL        PCT(19),XP(19),DSCAL,BOTOM,XLP,YLP,X,YP(4),XPT(2),
     #            XNEED, XUSED, RTP(19)
      CHARACTER*1 STR(5)
C
C     + + + FUNCTIONS + + +
      REAL   GAUSEX, ANGLEN
C
C     + + + INTRINSICS + + +
C
C     + + + EXTERNALS + + +
      EXTERNAL   HVXLIN, GAUSEX, DECCHX, CLNSFT, GPL, ANGTXA, LABXAX
      EXTERNAL   ANGLEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA PCT / 99.9, 99.8, 99.5,99.,98.,95.,90.,80.,70.,50.,30.,20.,
     #             10., 5., 2., 1.,.5,.2, .1 /
      DATA RTP / 1.001,1.002,1.005,1.01,1.02,1.05,1.1,1.25,1.5,2.0,
     $             3.0,5.0,10.0,20.0,50.0,100.0,200.0,500.0,1000.0/
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
C     draw bottom and top lines
      TBFLG= 3
      CALL HVXLIN (XLEN,YLEN,TBFLG)
C     YP(1..2) for bottom tic
C     YP(3..4) for top tic
      YP(1)= 0.0
      YP(2)= SIZEL* 0.5
      YP(3)= YLEN
      YP(4)= YLEN- 0.5* SIZEL
      DSCAL= XLEN/(2.0*STDEV)
C
C     draw tics
      IMIN = 20
      DO 10 I= 1,19
        IF (PORR .LE. 3) THEN
C         scale 99 - 1
          J = I
        ELSE
C       scale 1 - 99
          J = 20 - I
        END IF
        IF (PORR.EQ.2 .OR. PORR.EQ.5) THEN   
          X = 1.0/RTP(J)
        ELSE
          X = PCT(J)/100.0
        END IF
        XP(J) = DSCAL*(GAUSEX(X) + STDEV)
        IF (XP(J) .GT. 0.0 .AND. XP(J) .LT. XLEN) THEN
          IF (I .LT. IMIN) IMIN = I
          XPT(1)= XP(J)
          XPT(2)= XP(J)
          CALL GPL (I2,XPT,YP(1))
          CALL GPL (I2,XPT,YP(3))
        END IF
 10   CONTINUE
      IMAX = 19+1 - IMIN
C
C     label axis
      IFLG  = 1
      LEN   = 5
      YLP= -1.5* SIZEL
      XUSED = -0.5*SIZEL
      DO 30 I=IMIN,IMAX
        IF (PORR .LE. 3) THEN
C         scale 99 - 1
          J = I
        ELSE
C         scale 1 - 99
          J = 20 - I
        END IF
        IF (PORR .EQ. 2 .OR. PORR .EQ. 5) THEN
C         recurrence interval
          X = RTP(J)   
          IF (X .GT. 1.9) THEN
            SIGDIG = 2
            DECPLA = 0
          ELSE IF (X .GT. 1.009) THEN
            SIGDIG = 3
            DECPLA = 2
          ELSE
            SIGDIG = 4
            DECPLA = 3
          END IF
        ELSE IF (PORR .EQ. 3 .OR. PORR .EQ. 6) THEN
C         probability as a fraction
          X = 0.01*PCT(J)
          IF (X .GT. 0.991) THEN
            SIGDIG = 3
            DECPLA = 3
          ELSE IF (X .GT. 0.009) THEN
            SIGDIG = 2
            DECPLA = 2
          ELSE
            SIGDIG = 2
            DECPLA = 3
          END IF
        ELSE 
C         probability as percent
          X = PCT(J)
          IF (X .GT. 99.1) THEN
            SIGDIG = 3 
            DECPLA = 1
          ELSE IF (X .GT. 0.99) THEN 
            SIGDIG = 2
            DECPLA = 0
          ELSE
            SIGDIG = 3
            DECPLA = 1
          END IF
        END IF
        CALL DECCHX (X,LEN,SIGDIG,DECPLA,STR)
        CALL CLNSFT (LEN,IFLG,STR,OLEN,IDUM)
        XNEED = ANGLEN(OLEN,STR)
        XLP= XP(I) - 0.5*XNEED
        IHV = 1
        IF (XLP .GT. (XUSED+0.5*SIZEL)) THEN
C         enough space so label the tic
          CALL ANGTXA (XLP,YLP,OLEN,STR,IHV)
          XUSED = XP(I) + 0.5*XNEED
        END IF
 30   CONTINUE
C
C     title axis
      BOTOM= -4.0* SIZEL
      CALL LABXAX (LBAXIS,SIZEL,XLEN,BOTOM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HVXLIN
     I                    (XLEN,YLEN,TBFLG)
C
C     + + + PURPOSE + + +
C     routine prints heavy lines for top and bottom x-axis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TBFLG
      REAL      XLEN,YLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN  - length of x-axis
C     YLEN  - length of y-axis
C     TBFLG - top/bottom flag, 1- bottom line, 2- top line, 3- both lines
C                              4- middle line
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2
      REAL      YP(2),XP(2)
C     REAL      FAT,ONE
C
C     + + + EXTERNALS + + +
C     EXTERNAL   GSLWSC
      EXTERNAL   GPL
C
C     + + + DATA INITIALIZATIONS + + +
C     DATA FAT,ONE/2.0,1.0/
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
C     Note: to set line width to 50% greater, CALL GSLWSC (FAT)
C
C     set x coordinates
      XP(1)= 0.0
      XP(2)= XLEN
      IF (TBFLG.EQ.1 .OR. TBFLG.EQ.3) THEN
C       draw bottom line
        YP(1)= 0.0
        YP(2)= 0.0
        CALL GPL (I2,XP,YP)
      END IF
      IF (TBFLG.EQ.2 .OR. TBFLG.EQ.3) THEN
C       draw top line
        YP(1)= YLEN
        YP(2)= YLEN
        CALL GPL (I2,XP,YP)
      END IF
      IF (TBFLG.EQ.4) THEN
C       draw middle line
        YP(1)= YLEN/2.0
        YP(2)= YLEN/2.0
        CALL GPL (I2,XP,YP)
      END IF
C
C     Note: to reset line width, CALL GSLWSC (ONE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HVYLIN
     I                    (XLEN,YLEN,LRFLG)
C
C     + + + PURPOSE + + +
C     routine prints heavy lines for right and left y-axis
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LRFLG
      REAL      XLEN,YLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XLEN  - length of x-axis
C     YLEN  - length of y-axis
C     LRFLG - left/right flag, 1- left line, 2- right line, 3- both lines
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2
      REAL      YP(2),XP(2)
C     REAL      FAT,ONE
C
C     + + + EXTERNALS + + +
C     EXTERNAL   GSLWSC
      EXTERNAL   GPL
C
C     + + + DATA INITIALIZATIONS + + +
C     DATA FAT,ONE/2.0,1.0/
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C
C     Note: to set line width to 50% greater, CALL GSLWSC (FAT)
C
C     set y coordinates
      YP(1)= 0.0
      YP(2)= YLEN
      IF (LRFLG.EQ.1 .OR. LRFLG.EQ.3) THEN
C       draw left line
        XP(1)= 0.0
        XP(2)= 0.0
        CALL GPL (I2,XP,YP)
      END IF
      IF (LRFLG.EQ.2 .OR. LRFLG.EQ.3) THEN
C       draw right line
        XP(1)= XLEN
        XP(2)= XLEN
        CALL GPL (I2,XP,YP)
      END IF
C
C     Note, to reset line width, CALL GSLWSC (ONE)
C
      RETURN
      END
