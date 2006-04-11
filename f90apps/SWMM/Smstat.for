      SUBROUTINE SMSTAT
C     RUNOFF BLOCK
C     CALLED FROM HYDRO NEAR LINE 661
C=======================================================================
C     ROUTINE TO PRINT SUMMARY STATISTICS FOR CHANNEL/PIPES.
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93.
C     FIX SUBSCRIPT CALCULATION, RED, 12/31/93.
C     FIX TIME COMPUTATION, RED, 1/25/94.
C     CHECK FOR ZERO MAXIMA LEADING TO EXPONENTIATION ERRORS, 
C       WCH, 5/25/94.
C     ADD CALCS FOR WIER/ORIFICE OUTFLOW, WCH, 10/19/95.
C=======================================================================
C     SPECIFICATION STATEMENTS
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'RUNSTAT.INC'
C=======================================================================
C     INITIALIZATION
C=======================================================================
      IF(NOG.EQ.999) THEN
           NOGG = NSAVE
           ELSE
           NOGG = NOG+NSAVE
           ENDIF
      CALL WSTAT
      IULDAY  = JULDAY
      STIMDAY = TIMDAY
      DO 3002 J = 1,NOGG
C#### MAXTIM(J) = MAXTIM(J)/3600.0
      IF((J/42*42).NE.J.AND.J.NE.1) GO TO 3000
      WRITE(N6,4001)
      WRITE(N6,4004)
      IF(METRIC.EQ.1) WRITE(N6,4002)
      IF(METRIC.EQ.2) WRITE(N6,4003)
 3000 CONTINUE
C=======================================================================
C     I=NGUT(J)  CHANGE ORDER OF OUTPUT TO ORDER OF INPUT WITH INLETS 
C                AT BOTTOM  CIM  8/93
C=======================================================================
C#### RED (WCH), 12/31/93.  SHOULD BE I = NGUT(J).
C####      I = J
      I = NGUT(J)
Cred  Move this division from below do 3002 - 1/25/94
      MAXTIM(I) = MAXTIM(I)/3600.0
C=======================================================================
C     COMPUTE FULL FLOW AND VELOCITY
C     ALSO COMPUTE VELOCITY AND FLOW AT MAXIMUM DEPTH
C=======================================================================
C     TRAPEZOIDAL CHANNEL
C=======================================================================
      IF(NPG(I).EQ.1.OR.NPG(I).EQ.5) THEN
         DEPM = MAXDEP(I)
         DEPD = DFULL(I)
         IF(SURLEN(I).GT.0.0) DEPM = DEPD
         AXF = 0.5*(GS1(I)+GS2(I))*DEPD**2+GWIDTH(I)*DEPD
         WPF = SQRT(GS1(I)**2+1.)*DEPD+SQRT(GS2(I)**2+1.)*DEPD+GWIDTH(I)
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
         GQP = AXF*GCON(I)*(AXF/WPF)**0.6666667
         IF(WTYPE(I).EQ.0.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**1.5
         IF(WTYPE(I).EQ.1.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**2.5
         IF(WTYPE(I).EQ.2.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(2.0*32.2*(DEPD-WELEV(I)))**0.5
         IF(WTYPE(I).EQ.-1) GQD = GQP
         IF(WTYPE(I).GT.-1) GQD = AMIN1(GQD,GQP)
         GVD = GQD/AXF
C####         GVD = GCON(I)*(AXF/WPF)**0.6666667
C####         GQD = GVD*AXF
C#### WCH, 5/25/94.  CHANGE FORM OF SQUARE TO ALLOW DEPM = 0.
C####         AXM = 0.5*(GS1(I)+GS2(I))*DEPM**2+GWIDTH(I)*DEPM
         AXM = 0.5*(GS1(I)+GS2(I))*DEPM*DEPM+GWIDTH(I)*DEPM
         WPM = SQRT(GS1(I)**2+1.)*DEPM+SQRT(GS2(I)**2+1.)*DEPM+GWIDTH(I)
C#### WCH, 10/19/95.  ADD SAME IF STMT AS FOR OTHER SHAPES.
C#### WCH, 5/25/94.  CHECK FOR ZERO AXM OR WPM.
         IF(AXM.LE.0.0.OR.WPM.LE.0.0) THEN
              VM = 0.0
              QM = 0.0
              ELSE
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
              IF(WTYPE(I).EQ.-1) QM = AXM*GCON(I)*(AXM/WPM)**0.6666667
              IF(WTYPE(I).EQ.0.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**1.5
              IF(WTYPE(I).EQ.1.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**2.5
              IF(WTYPE(I).EQ.2.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(2.0*32.2*(DEPM-WELEV(I)))**0.5
              VM = QM/AXM
              ENDIF
C####         QM      = VM * AXM
         ENDIF
C=======================================================================
C     CIRCULAR PIPE
C=======================================================================
      IF(NPG(I).EQ.2.OR.NPG(I).EQ.6) THEN
         DEPM = GWIDTH(I)/2.*(1.-COS(MAXDEP(I)))
         DEPD = GWIDTH(I)
         IF(SURLEN(I).GT.0.0) DEPM = DEPD
         GVD = GCON(I)*(DEPD/4.)**0.66666667
         GQD = GVD*3.14159*DEPD*DEPD/4.
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
         AXF = 3.14159*DEPD*DEPD/4.
         GQP = AXF*GCON(I)*(DEPD/4.)**0.66666667
         IF(WTYPE(I).EQ.0.AND.DEPD.GT.WELEV(I)) GQD = 
     1        WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**1.5
         IF(WTYPE(I).EQ.1.AND.DEPD.GT.WELEV(I)) GQD = 
     1        WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**2.5
         IF(WTYPE(I).EQ.2.AND.DEPD.GT.WELEV(I)) GQD = 
     1        WDIS(I)*SPILL(I)*(2.0*32.2*(DEPD-WELEV(I)))**0.5
         IF(WTYPE(I).EQ.-1) GQD = GQP
         IF(WTYPE(I).GT.-1) GQD = AMIN1(GQD,GQP)
         GVD = GQD/AXF
C
         D0  = MAXDEP(I)
         SIN2D0  = 0.5*SIN(2.0*D0)
         AXM     = GWIDTH(I)**2*(D0-SIN2D0)/4.0
         WPM     = GWIDTH(I)*D0
C#### WCH, 5/25/94.  CHECK FOR ZERO AXM OR WPM.
         IF(AXM.LE.0.0.OR.WPM.LE.0.0) THEN
              VM = 0.0
              QM = 0.0
              ELSE
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
              IF(WTYPE(I).EQ.-1) QM = AXM*GCON(I)*(AXM/WPM)**0.6666667
              IF(WTYPE(I).EQ.0.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**1.5
              IF(WTYPE(I).EQ.1.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**2.5
              IF(WTYPE(I).EQ.2.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(2.0*32.2*(DEPM-WELEV(I)))**0.5
              VM = QM/AXM
              ENDIF
C####         QM      = VM * AXM
         ENDIF
C=======================================================================
C     DUMMY GUTTER ==> PRINT ONLY MAX COMPUTED FLOW
C=======================================================================
      IF(NPG(I).EQ.3) THEN
         JULDAY = MAXJUL(I)
         TIMDAY = MAXTIM(I) 
         CALL DATED
         IF(METRIC.EQ.2) MAXDEP(I) = MAXDEP(I)/35.315
         IF(JCE.EQ.0) WRITE (N6,4005) NAMEG(I),MAXDEP(I),MONTH,
     +                                NDAY,NYEAR,MAXTIM(I)
         IF(JCE.EQ.1) WRITE (N6,4006) KAMEG(I),MAXDEP(I),MONTH,
     +                                NDAY,NYEAR,MAXTIM(I)
         GO TO 3002
         ENDIF
C=======================================================================
C     PARABOLIC CHANNEL
C=======================================================================
      IF(NPG(I).EQ.4.OR.NPG(I).EQ.7) THEN
         DEPD = DFULL(I)
         DEPM = MAXDEP(I)
         IF(SURLEN(I).GT.0.0) DEPM = DEPD
         A2 = GWIDTH(I)**4.0/(64.0*DEPD**2.0)
         X  = GWIDTH(I)/2.0
         WPF = 8.0*DEPD/GWIDTH(I)**2*(X*SQRT(A2+X**2) + 
     +      A2*LOG(X+SQRT(A2+X**2)) - 
     +      A2*LOG(SQRT(A2)))       
         AXF  =  0.66666667*GWIDTH(I)*DEPD
         RADF = AXF/WPF
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
         GQP = AXF*GCON(I)*RADF**0.6666667
         IF(WTYPE(I).EQ.0.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**1.5
         IF(WTYPE(I).EQ.1.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(DEPD-WELEV(I))**2.5
         IF(WTYPE(I).EQ.2.AND.DEPD.GT.WELEV(I)) GQD = 
     1             WDIS(I)*SPILL(I)*(2.0*32.2*(DEPD-WELEV(I)))**0.5
         IF(WTYPE(I).EQ.-1) GQD = GQP
         IF(WTYPE(I).GT.-1) GQD = AMIN1(GQD,GQP)
         GVD = GQD/AXF
C####         GQD  = GCON(I)*AXF*RADF**0.666666667
         D0   = DEPM
         WIDTH = GWIDTH(I)*SQRT(D0/DFULL(I))
         X     = WIDTH/2.0
         A2    = GWIDTH(I)**4/(64.0*DFULL(I)**2)
C#### WCH, 5/25/94.  CHANGE X**2 TO X*X TO AVOID EXPONENTIATION ERRORS.
         WPM   = 8.0*DFULL(I)/GWIDTH(I)**2*(X*SQRT(A2+X*X) + 
     +           A2*LOG(X+SQRT(A2+X*X)) - 
     +           A2*LOG(SQRT(A2)))       
         AXM   = 0.66666667*WIDTH*D0
C#### WCH, 5/25/94.  CHECK FOR ZERO AXM OR WPM.
         IF(AXM.LE.0.0.OR.WPM.LE.0.0) THEN
              VM = 0.0
              QM = 0.0
              ELSE
C#### WCH, 10/19/95.  ACCOUNT FOR WEIR/ORIFICE OUTFLOW.
              IF(WTYPE(I).EQ.-1) QM = AXM*GCON(I)*(AXM/WPM)**0.6666667
              IF(WTYPE(I).EQ.0.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**1.5
              IF(WTYPE(I).EQ.1.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(DEPM-WELEV(I))**2.5
              IF(WTYPE(I).EQ.2.AND.DEPM.GT.WELEV(I)) QM = 
     1             WDIS(I)*SPILL(I)*(2.0*32.2*(DEPM-WELEV(I)))**0.5
              VM = QM/AXM
              ENDIF
C####         QM      = VM * AXM
         ENDIF
C=======================================================================
C     COMPUTE FLOW AND DEPTH RATIOS
C=======================================================================
      FR = QM/GQD
      DR = DEPM/DEPD
C=======================================================================
C     CONVERT SURCHARGE VOLUME TO AC-FT
C=======================================================================
      MAXSUR(I) = MAXSUR(I)/43560.
      SURLEN(I) = SURLEN(I)/3600.
C=======================================================================
C     PRINT STATISTICS
C=======================================================================
      JULDAY = MAXJUL(I)
      TIMDAY = MAXTIM(I) 
      CALL DATED
      IF(METRIC.EQ.2) THEN
           GQD  = GQD/35.315
           GVD  = GVD/3.2808
           DEPD = DEPD/3.2808
           MAXQIN(I) = MAXQIN(I)/35.315
           QM   = QM/35.315
           DEPM = DEPM/3.2808
           VM   = VM/3.2808
           MAXSUR(I) = MAXSUR(I)*43560.0/35.315
           ENDIF
      IF(JCE.EQ.0) WRITE(N6,4009) NAMEG(I),GQD,GVD,DEPD,MAXQIN(I),
     1 QM,DEPM,VM,MONTH,NDAY,NYEAR,MAXTIM(I),SURLEN(I),MAXSUR(I),FR,DR
      IF(JCE.EQ.1) WRITE(N6,4010) KAMEG(I),GQD,GVD,DEPD,MAXQIN(I),
     1 QM,DEPM,VM,MONTH,NDAY,NYEAR,MAXTIM(I),SURLEN(I),MAXSUR(I),FR,DR
 3002 CONTINUE
      WRITE(N6,4012) J-1
      WRITE(N6,5554)
 4041 JULDAY = IULDAY
      TIMDAY = STIMDAY
      RETURN
C=======================================================================
 4001 FORMAT(//,40X,'SUMMARY STATISTICS FOR CHANNEL/PIPES'/
     1       ' ',36X,48('='))
 4004 FORMAT(/,2X,
     1'                                       MAXIMUM   MAXIMUM   ',
     2'MAXIMUM  MAXIMUM    TIME        LENGTH     MAXIMUM    ',
     3'RATIO OF  RATIO OF',/,2X,
     4'           FULL     FULL      FULL    COMPUTED  COMPUTED  ',
     5'COMPUTED COMPUTED     OF           OF      SURCHARGE   ',
     6'MAX. TO  MAX. DEPTH',/,2X,
     7'  CHANNEL  FLOW   VELOCITY    DEPTH    INFLOW   OUTFLOW    ',
     8'DEPTH   VELOCITY OCCURRENCE    SURCHARGE   VOLUME     ',
     9'FULL      TO FULL')
 4002 FORMAT (2X,
     *'   NUMBER  (CFS)    (FPS)     (FT)      (CFS)    (CFS)     ',
     1'(FT)    (FPS)    DAY   HR.      (HOUR)     (AC-FT)    ',
     2'FLOW       DEPTH',/,2X,
     3' -------- -------------------------   --------------------',
     4'-----------------------------   ---------------------- ',
     5'-------------------')
 4003 FORMAT (2X,
     *'   NUMBER  (CMS)    (M/S)      (M)      (CMS)    (CMS)     ',
     1' (M)    (M/S)    DAY   HR.      (HOUR)     (CU-M)     ',
     2'FLOW       DEPTH',/,2X,
     3' -------- -------------------------   --------------------',
     4'-----------------------------   ---------------------- ',
     5'-------------------')
C 4004 FORMAT(/,37X,3(3X,'MAXIMUM'),6X,'TIME',7X,'LENGTH',5X,'MAXIMUM',1X
C     2,2(4X,'RATIO OF')/' ',6X,3(4X,' FULL '),1X,3(2X,'COMPUTED'),6X,
C     3 'OF',10X,'OF',6X,'SURCHARGE',4X,'MAX. TO',3X,'MAX. DEPTH'/
C     4 ' ',' CHANNEL',3X,'FLOW',4X,'VELOCITY',4X,'DEPTH',4X,'INFLOW',4X,
C     5 'OUTFLOW',4X,'DEPTH',3X,'OCCURRENCE',2X,'SURCHARGE',5X,'VOLUME',
C     6 6X,'FULL  ',4X,' TO FULL '/' ',1X,'NUMBER',4X,'(CFS)',5X,'(FPS)',
C     7 5X,'(FT)',6X,'(CFS)',5X,'(CFS)',5X,'(FT)',4X,'DAY   HR.',5X,
C     8 '(HOUR)',5X,'(AC-FT)',6X,'FLOW',8X,'DEPTH'/' ',1X,6('-'),2(3X,
C     9 6('-')),2('-'),3X,6('-'),3X,39('-'),2(3X,9('-')),3X,8('-'),3X,
C     9 10('-')/)
 4005 FORMAT (1X,I10,25X,F11.2,27X,I3,'/',I2,'/',I4,F6.2)
 4006 FORMAT (1X,A10,25X,F11.2,27X,I3,'/',I2,'/',I4,F6.2)
 4009 FORMAT(1X,I10,F9.2,F7.2,F9.2,F11.2,F10.2,F9.2,F8.2,I3,'/',I2,'/',
     1 I4,F6.2,F11.2,E14.5,F8.2,F10.2)
 4010 FORMAT(1X,A10,F9.2,F7.2,F9.2,F11.2,F10.2,F9.2,F8.2,I3,'/',I2,'/',
     1 I4,F6.2,F11.2,E14.5,F8.2,F10.2)
 4012 FORMAT(/,46X,'TOTAL NUMBER OF CHANNELS/PIPES = ',I4)
 5554 FORMAT(/,'  *** NOTE ***  THE MAXIMUM FLOWS AND DEPTHS ARE CALCULA
     2TED AT THE END OF THE TIME INTERVAL ')
      END
