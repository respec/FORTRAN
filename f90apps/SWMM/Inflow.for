      SUBROUTINE INFLOW
C	EXTRAN BLOCK
C     CALLED BY TRANSX NEAR LINE 497
C=======================================================================
C     THIS SUBROUTINE SELECTS THE EXTRAN INPUT HYDROGRAPH
C     ORDINATE FROM OFF-LINE FILES AND/OR USER INPUT (K3 LINES).
C=======================================================================
C     WCH, 6/16/94.  CORRECT USER INPUT OPTION FOR POSSIBILITY OF
C       MULTIPLE VALUES FOR SAME JUNCTION (P. 30 OF EXTRAN MANUAL).
C     WCH (SACHA HENCHMAN, METCALF AND EDDY, BOSTON), 10/17/95.
C       REMOVE ABOVE FIX BY REMOVING DO 4010 LOOP, TO AVOID ADDING 50%
C       MORE INFLOW WHEN USING BOTH INTERFACE FILE INPUT AND K3 INPUT.
C       KEEP TRACK SEPARATELY OF INFLOWS FROM INTERFACE FILE (QINN)
C       AND INFLOWS FROM K3 LINES (QINNK3) TO AVOID DOUBLE COUNTING.
C       DO NOT ALLOW MULTIPLE INFLOWS FOR SAME JUNCTION ON K3 LINES.
C     WCH, 10/17/95.  ALLOW MIS-MATCH OF EXTRAN AND INTERFACE FILE
C       JUNCTIONS.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'MOBFF.INC'
      DIMENSION LSWIT(NIE),NSWIT(NIE),MSWIT(NIE),
     +                                ORTIME(NVORF),NORPLC(NVORF)
      CHARACTER*10 NSWIT,MSWIT
C=======================================================================
C     Define statement function for linear interpolation.
C=======================================================================
      QLINTP(Q1,Q2,T1,T2,T) = Q1 + (Q2-Q1)*(T-T1)/(T2-T1)
C=======================================================================
C     Initialize new QINN, QINNK3 and QIN arrays.
C=======================================================================
      DO 40 J   = 1,NJ
cim  change for variable base flows
cim   QIN(J)    = QINST(J)
      QIN(J)    = QINST(J)*BFFMONTH(1,IWHICH(J))
      QINN(J,1) = QINN(J,2)
      QINN(J,2) = 0.0
C#### WCH, 10/17/95.  ADD NEW QINNK3
      QINNK3(J,1) = QINNK3(J,2)
      QINNK3(J,2) = 0.0
   40 CONTINUE
C=======================================================================
C     Calculate new depth and pipe length for an orifice with a
C               time history.
C=======================================================================
      IF(NVOR.GT.0) THEN
                    DO 9000 M = 1,NVOR
                    N         = LORIF(M)
                    JJ        = NORPLC(M)
                    IF(JJ.EQ.0) JJ = 1
                    DO 8990 J      = JJ,NVOTIM
                    IF(TIME/3600.0.GE.VORIF(M,J,1)) GO TO 8991
 8990               CONTINUE
                    GO TO 9000
 8991               CONTINUE
                    IF(ORTIME(M).GT.TIME/3600.0) GO TO 9000
                    ORTIME(M) = VORIF(M,J,1)
                    NORPLC(M) = J + 1
                    ZZ        = 0.96*DEEP(N)
cim begin OOOO
CIM  adjustment for CIRCULAR orifices
                    if (NKLASS(N).EQ.51.OR.NKLASS(N).EQ.52) THEN
                    DEEPOLD   = DEEP(N)
                    DEEP(N)   = SQRT(4.0*VORIF(M,J,3)/3.14159)
                    WIDE(N)   = DEEP(N)
                    AFULL(N)  = VORIF(M,J,3)
                    RFULL(N)  = DEEP(N)/4.0
CIM CHANGE INVERT
CIM  NOTE  ZU + DEEPOLD = OLD CROWN ELEV.
CIM        ZU + DEEPOLD - DEEP = new invert elev.
                    IF (IOINV(M).EQ.1) then
                    ZU(N) = ZU(N)+DEEPOLD-DEEP(N)
                    ZD(N) = ZD(N)+DEEPOLD-DEEP(N)
                    ENDIF
                    ELSE
CIM   ADJUST ONLY DEPTH FOR RECTANGULAR ORIFICE, WIDTH REMAINS UNCHANGED
                    DEEPOLD   = DEEP(N)
                    DEEP(N)   = VORIF(M,J,3)/WIDE(N)
                    AFULL(N)  = VORIF(M,J,3)
                    RFULL(N)  = AFULL(N)/(2.0*DEEP(N)+2.0*WIDE(N))
CIM CHANGE INVERT
CIM  NOTE  ZU + DEEPOLD = OLD CROWN ELEV.
CIM        ZU + DEEPOLD - DEEP = new invert elev.
                    IF (IOINV(M).EQ.1) then
                    ZU(N) = ZU(N)+DEEPOLD-DEEP(N)
                    ZD(N) = ZD(N)+DEEPOLD-DEEP(N)
                    ENDIF
                    END IF
CIM END   OOOOOOO
                                    DDD = DELT
C BAC START   !!!!!!!
C                    IF(NEQUAL.GT.1) DDD = FLOAT(NEQUAL)
C BAC END   !!!!!!!
                    CLEN      = 2.0*DDD*SQRT(GRVT*DEEP(N))
                    LEN(N)    = AMAX1(200.0,CLEN)
                    CMANN     = CMET(9,METRIC)
                    ROUGH(N)  = CMANN*RFULL(N)**.66667/
     +                         (VORIF(M,J,2)*SQRT(LEN(N)*2.0*GRVT))
                    DO 700 J = 1,NJ
                    IF(JCE.EQ.0.AND.NJUNC(N,1).EQ.JUN(J))  GO TO 720
                    IF(JCE.EQ.1.AND.KJUNC(N,1).EQ.AJUN(J)) GO TO 720
  700               CONTINUE
CIM START  OOOOOOOOOOOO
CIM MODIFY TO INCLUDE RECTANGULAR BOTTOM OUTLET ORIFICE
CIM ALSO MODIFY PRINT STATEMENT TO INCLUDE MODIFIED WIDTH
  720               IF((NKLASS(N).EQ.52).OR.(NKLASS(N).EQ.54)) THEN
                       ZU(N)      = ZU(N) - 0.96*DEEP(N) + ZZ
                       Z(J)       = ZU(N)
                       ZD(N)      = ZU(N) - 0.01/CMET(1,METRIC)
                       IF(JCE.EQ.0) WRITE(N6,6010) M,NCOND(N),DEEP(N),
     +                              WIDE(N),LEN(N),ROUGH(N),ZU(N),
     +                              ZD(N),TIME/3600.0
                       IF(JCE.EQ.1) WRITE(N6,6011) M,ACOND(N),DEEP(N),
     +                              WIDE(N),LEN(N),ROUGH(N),ZU(N),
     +                              ZD(N),TIME/3600.0
                       ELSE
                       IF(JCE.EQ.0) WRITE(N6,6015) M,NCOND(N),DEEP(N),
     +                               WIDE(N),LEN(N),ROUGH(N),TIME/3600.0
                       IF(JCE.EQ.1) WRITE(N6,6016) M,ACOND(N),DEEP(N),
     +                               WIDE(N),LEN(N),ROUGH(N),TIME/3600.0
                       ENDIF
CIM    CORRECTION    ROUGH NEEDS TO BE MODIFIED (SEE INDAT3)
      ROUGH(N)  = GRVT*ROUGH(N)**2/CMET(9,METRIC)**2
 9000               CONTINUE
                    ENDIF
CIM END  OOOOOO
C=======================================================================
C     Conditional time for first time step.
C=======================================================================
      IF(MTIME.EQ.1) THEN
                     JDAY  = IDATEZ
                     TMDAY = TZERO
                     TEND  = TZERO
                     IF(METRIC.EQ.1) THEN
                                     QQCONV = QCONV
                                     ELSE
                                     QQCONV = QCONV/CMET(8,METRIC)
                                     ENDIF
                     ENDIF
C=======================================================================
C     Interface file hydrograph ordinates are interpolated.
C     Note that in most cases, there is no interface file ordinate
C     at time zero.  Hydrograph ordinate at time zero is implicitly
C     assumed to equal zero.
C=======================================================================
      IF(LOCATS.EQ.0) THEN
      NEWQTAPE = 1
      ELSE
cim      IF(LOCATS.GT.0) THEN
 1000 IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GT.TMDAY)) THEN
              DO 4523 I  = 1,LOCATS
              QTAPE(I,1) = QTAPE(I,2)
 4523         CONTINUE
              IF(NQUAL.LE.0) THEN
                      READ(LAST,END=205) JDAY,TMDAY,
     +                DELTA,(QTAPE(I,2),I=1,LOCATS)
              ELSE
                      READ(LAST,END=205) JDAY,TMDAY,
     +                DELTA,(QTAPE(I,2),(PDUM,J=1,NQUAL),I=1,LOCATS)
              ENDIF
              TREF = TIME/3600.0
              CALL NTIME(JDAY,TMDAY,TFILE)
              IF(TFILE.LT.0.0) GO TO 1000
              TFILE      = TFILE/3600.0
              DO 1400 I  = 1,LOCATS
 1400         QTAPE(I,2) = QTAPE(I,2)*QQCONV
C ### RHF 12/20/96 SET FLAG IF INFLOWS CHANGE
C   NEWQTAPE IS RETURNED AS 1 IF STEADY, 0 IF NOT STEADY
      IF (ISOLSKIP .EQ. 1) THEN
      NEWQTAPE = 1
      DO 1450 I = 1,LOCATS
      DELTFLOW = ABS(QTAPE(I,1)-QTAPE(I,2))
      IF (DELTFLOW.GT.TOLCS2) NEWQTAPE = 0
c     write(n6,*) 'inflow0',tstart,deltflow,tolcs2,newqtape
 1450 CONTINUE
      ENDIF
C ---
              ENDIF
C=======================================================================
C     Interpolate flow values.
C=======================================================================
      THR        = TIME/3600.0 - TREF
      DO 1500 L  = 1,LOCATS
      KSWIT      = NLOC(L)
C#### WCH, 10/17/95.  CHECK FOR ZERO NLOC (NO EXTRAN JUNCTION).
      IF(KSWIT.GT.0) THEN
           IF(JCE.EQ.0) LSWIT(L)   =  JUN(KSWIT)
           IF(JCE.EQ.1) MSWIT(L)   = AJUN(KSWIT)
           ELSE
           IF(JCE.EQ.0) LSWIT(L)   = 0
           IF(JCE.EQ.1) MSWIT(L)   = KAN(L)
           ENDIF
 1500 CONTINUE
      IF(MOD(MTIME,INTER).EQ.0) THEN
             IF(METRIC.EQ.1) WRITE(N6,5000) TIME/3600.0
             IF(METRIC.EQ.2) WRITE(N6,5001) TIME/3600.0
             IF(JCE.EQ.0) WRITE(N6,5830)(LSWIT(L),QTAPE(L,2),L=1,LOCATS)
             IF(JCE.EQ.1) WRITE(N6,5833)
     1	   (ADJUSTR(MSWIT(L)),QTAPE(L,2),L=1,LOCATS)
             ENDIF
C=======================================================================
      DO 4570 L = 1,LOCATS
      J         = NLOC(L)
C#### WCH, 10/17/95.  CHECK FOR NO NLOC = 0 (NO EXTRAN JUNCTION).
      IF(J.EQ.0) GO TO 4570
      QQ1       = QTAPE(L,1)
      QQ2       = QTAPE(L,2)
      IF(TFILE.EQ.0.0) THEN
                       QINN(J,2) = QQ2
                       QIN(J)    = QIN(J) + QQ1/2.0 + QQ2/2.0
                       ELSE
                       QEND      = QLINTP(QQ1,QQ2,0.0,TFILE,THR)
                       QINN(J,2) = QEND
                       QIN(J)    = QIN(J) + QINN(J,1)/2.0 + QEND/2.0
                       ENDIF
 4570 CONTINUE
      ENDIF
C=======================================================================
C     Line hydrograph input values are interpolated.
C=======================================================================
      IF(NJSW.EQ.0) THEN
      NEWQCARD = 1
      ELSE
cim      IF(NJSW.GT.0) THEN
C=======================================================================
C     New input data required.
C=======================================================================
      IF(MTIME.EQ.1) THEN
               READ(N5,*,ERR=888,END=889) CC
               IF(CC.NE.'K3') THEN
                              WRITE(N6,9100) TIME/3600.0
                              STOP
                              ENDIF
               BACKSPACE N5
               READ(N5,*,ERR=888,END=889)CC,TSTART,(QCARD(L,2),L=1,NJSW)
               WRITE(N6,4999)
               DO 2500 L     = 1,NJSW
               KSWIT         = JSW(L)
C#### WCH, 10/17/95.  FROM SUB. INDAT3, JSW() CANNOT BE ZERO.  THIS
C     CODE NOT NEEDED.
C####               IF(KSWIT.EQ.0) THEN
C####                              WRITE(*,*) L,KSWIT,NJSW
C####                              PAUSE
C####                              ENDIF
C#### WCH (M&E, BOSTON), 10/17/95.  USE NEW QINNK3() AND DON'T
C     ADD QINST HERE.
C####               QINN(KSWIT,1) = QCARD(L,2) + QINST(KSWIT)
               QINNK3(KSWIT,1) = QCARD(L,2)
               IF(JCE.EQ.0) LSWIT(L)  = JUN(KSWIT)
 2500          IF(JCE.EQ.1) NSWIT(L)  = AJUN(KSWIT)
               IF(METRIC.EQ.1) WRITE(N6,5831) TSTART
               IF(METRIC.EQ.2) WRITE(N6,5832) TSTART
               IF(JCE.EQ.0) WRITE(N6,5830)(LSWIT(L),QCARD(L,2),L=1,NJSW)
               IF(JCE.EQ.1) WRITE(N6,5833)
     1		 (ADJUSTR(NSWIT(L)),QCARD(L,2),L=1,NJSW)
               TSTART = 3600.0*TSTART
               TEND   = TSTART
C ### RHF 12/20/96 SET FLAG IF INFLOWS CHANGE
C   NEWQCARD IS RETURNED AS 1 IF STEADY, 0 IF NOT STEADY
      IF (ISOLSKIP .EQ. 1) THEN
      NEWQCARD = 1
      DO 3100 I = 1,NJSW
      DELTFLOW = ABS(QCARD(I,1)-QCARD(I,2))
      IF (DELTFLOW.GT.TOLCS2) NEWQCARD = 0
c     write(n6,*) 'inflow1',tstart,deltflow,tolcs2,newqcard
 3100 continue
      ENDIF
C ---
               ENDIF
C======================================================================
 6666 CONTINUE
      IF(TIME-TEND.GT.0.0) THEN
               TSTART     = TEND
               DO 300 L   = 1,NJSW
  300          QCARD(L,1) = QCARD(L,2)
               READ(N5,*,END=889,ERR=888,IOSTAT=IREADE) CC
	         IF(IREADE.NE.0) GO TO 889
c this next line shouldn't be needed
c	         IF(EOF(N5)) GO TO 889  KAI DIDN'T LIKE THIS 
               IF(CC.NE.'K3') THEN
                              WRITE(N6,9100) TIME/3600.0
                              STOP
                              ENDIF
               BACKSPACE N5
               READ(N5,*,END=889,ERR=888,IOSTAT=IREADE) 
     1		 CC,TEND,(QCARD(L,2),L=1,NJSW)
	         IF(IREADE.NE.0) GO TO 889
c this next line shouldn't be needed
c	         IF(EOF(N5)) GO TO 889
               WRITE(N6,5999)
               DO 3000 L              = 1,NJSW
               KSWIT                  = JSW(L)
               IF(JCE.EQ.0) LSWIT(L)  = JUN(KSWIT)
 3000          IF(JCE.EQ.1) NSWIT(L)  = AJUN(KSWIT)
               IF(METRIC.EQ.1) WRITE(N6,5831) TEND
               IF(METRIC.EQ.2) WRITE(N6,5832) TEND
               IF(JCE.EQ.0) WRITE(N6,5830)(LSWIT(L),QCARD(L,2),L=1,NJSW)
               IF(JCE.EQ.1) WRITE(N6,5833)
     1		              (ADJUSTR(NSWIT(L)),QCARD(L,2),L=1,NJSW)
               WRITE(N6,5999)
               TEND = 3600.0*TEND
C ### RHF 12/20/96 SET FLAG IF INFLOWS CHANGE
C   NEWQCARD IS RETURNED AS 1 IF STEADY, 0 IF NOT STEADY
      IF (ISOLSKIP .EQ. 1) THEN
      NEWQCARD = 1
      DO 3101 I = 1,NJSW
      DELTFLOW = ABS(QCARD(I,1)-QCARD(I,2))
      IF (DELTFLOW.GT.TOLCS2) NEWQCARD = 0
c     write(n6,*) 'inflow2',tstart,deltflow,tolcs2,newqcard
 3101 continue
      ENDIF
C ---
               GO TO 6666
               ENDIF
C=======================================================================
C     No new input data required.
C=======================================================================
         DO 4000 L = 1,NJSW
         J         = JSW(L)
         QQ1       = QCARD(L,1)
         QQ2       = QCARD(L,2)
         T         = TIME/3600.0
         T1        = TSTART/3600.0
         T2        = TEND/3600.0
         QQ        = QLINTP(QQ1,QQ2,T1,T2,T)
         QINNK3(J,2) = QQ
C#### WCH, 6/16/94.  NEED TO ADD QINN(J,1)/2 IN SEPARATE LOOP IN ORDER
C    NOT TO ADD IT TWICE IN EVENT OF DUPLICATE INPUT NODES.
C#### 4000    QIN(J)    = QIN(J) + QINN(J,1)/2.0 + QQ/2.0
C#### WCH (M&E, BOSTON), 10/17/95.  KEEP K3 CONTRIBUTION SEPARATE
C     USING NEW VARIABLE QINNK3(J, ).
C     ALSO, DO NOT ALLOW MULTIPLE INPUTS FOR SAME JUNCTION IN K3 LINES.
C
 4000    QIN(J)    = QIN(J) + QQ/2.0 + QINNK3(J,1)/2.0
C
C#### WCH (M&E, BOSTON), 10/17/95.  REMOVE THIS LOOP BECAUSE HALF OF
C    INFLOW IS ALREADY ADDED IN DO 4570 AND DO 4000, AND QINN(J,1)
C    STARTS OUT WITH OLD INSTANT. INPUT DUE TO DO 40 LOOP.  SO, DO NOT
C    ALLOW MULTIPLE INPUTS FOR SAME JUNCTION IN K3 LINES.
C####         DO 4010 J = 1,NJ
C#### 4010    QIN(J)    = QIN(J) + QINN(J,1)/2.0
         ENDIF
C=======================================================================
      RETURN
  888 CALL IERROR
	STOP
  889	WRITE(N6,9200) TIME/3600.0
	TEND = 999999999.9
      DO L=1,NJSW
	QCARD(L,1)=0.0
      QCARD(L,2)=0.0
	ENDDO
	RETURN
C=======================================================================
  205 DO 2000 L  = 1,LOCATS
      QTAPE(L,1) = 0.0
2000  QTAPE(L,2) = 0.0
      WRITE(N6,9200) TIME/3600.0
      JDAY       = 9999999
      TMDAY      = 0.0
      RETURN
C=======================================================================
 4980 FORMAT(/,' TZERO is later in time than last record on tape from
     1interface file.')
 4999 FORMAT(/,1X,40(2H# ))
 5000 FORMAT(/,' ===> System inflows (file) at ',F8.2,' hours',
     *' (  Junction / Inflow, cfs)',/)
 5001 FORMAT(/,' ===> System inflows (file) at ',F8.2,' hours',
     *' (  Junction / Inflow, cu m/s)',/)
 5830 FORMAT(6(1X,I10,'/',1PE9.2))
 5833 FORMAT(6(1X,A10,'/',1PE9.2))
 5831 FORMAT(' ===> System inflows (data group K3) at ',F8.2,
     +          ' hours ( Junction / Inflow,cfs )',/)
 5832 FORMAT(' ===> System inflows (data group K3) at ',F8.2,
     +          ' hours ( Junction / Inflow,cu m/s )',/)
 5999 FORMAT(1X,40(2H# ))
CIM START OOOOOOOOO
 6010 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I9,/,
     .       '       CONDUIT NUMBER..........................',I10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4,/,
     .       '       SIMULATION TIME IN HOURS.............',F12.4)
 6011 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I9,/,
     .       '       CONDUIT NANE............................',A10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4,/,
     .       '       SIMULATION TIME IN HOURS.............',F12.4)
 6015 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I7,/,
     .       '       INTERNAL CONDUIT NUMBER.................',I10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       SIMULATION TIME IN HOURS.............',F12.4)
 6016 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NAME............................',A10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       SIMULATION TIME IN HOURS.............',F12.4)
CIM END     OOOOOOOOO
 9100 FORMAT(/,' ===> Error !! K3 data input ended at time: ',F12.5,
     +        ' hours.')
 9200 FORMAT(/,' ===> Warning !! End of input file reached at time = ',
     +         F6.1,' hours.',/,
     +'                 Simulation continues with zero inflow.',/)
C=======================================================================
      END
