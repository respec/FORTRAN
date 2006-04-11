C
C
C
      SUBROUTINE BIODEG(
     I  K,  
     O  DKBIO)
C    
C     + + + PURPOSE + + + 
C     An approximation to the time dependent solution of equations of
C     App. 3 of Soulas, 1982.  Uses method of Carnahan, et al., 1969.
C     Modification date: 8/24/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INCLUDE 'PPARM.INC'
      INTEGER*4 K,N
      REAL*8    DKBIO(3,NCMPTS)
C
C     + + + ARGUMENT DEFINITIONS + + + 
C
C
C     K        Chemical number for PRZM
C     NUMINC   Number of timesteps desired
C     H        Timestep size                (days)
C     KSM      Saturation constants for respective
C              populations  (mg/g water)
C     KCM
C     KC
C     MKS
C     KR 
C     KLDM     Death rates for respective populations       (1/day)
C     KLDC
C     KLDS
C     KLDR
C     USM      Maximum specific growth rate for population  (1/day)
C     UCM
C     MUC
C     US
C     UR
C     CM(I)    Mineralizable carbon                (mg/g moist soil)
C     Q(I)     Average carbon content of the populations (dimensionless)
C     Y(1,k,I) Xm:   Metabolizing Microbial population(mg/g moist soil)
C     Y(2,k,I) Xc:   Co-Metabolising
C     Y(3,k,I) Xs:   Sensitive
C     Y(4,k,I) Xr:   Non-Sensitive
C     Y(5,k,I) ST:   Pesticide concentration in the moist soil  (mg/g)
C     Y(6,k,I) CW:Carbon concentration in the soil solution (mg/g water)
C     KL1      Second-order death rate for XS        (1/(mg/g)*1/day)
C     KL2      Dissociation constant of enzyme-substrate complex (1/day)
C     KXX      Interim term
C     YSM      True growth yield of the population (mg(dry wt.)/mg ??)
C     YCM
C     YC
C     YS
C     YR
C     SWATR    Weight of soil solution (aqueous phase)     (g)
C     P        weight of dry soil (solid phase)            (g)
C     KD(K,I)  Distribution coefficient            (g water/g dry soil)
C     KE       Average Enzyme content of XC              (dimensionless)
C     KIN      Inhibition constant                       (mg/g dry soil)
C     KSK      Carbon solubilization constant
C     W(I)     Water content of the soil (HSOIL/P)      (g water/g soil)
C     WD(I)    Water conversion
C     WS(I)    Water equilvalent
C     AM       Maintenence coefficient of the Xi population
C     AC
C     AS
C     AR 
C     Y(N,K,I) Vector of Solution
C     FF(N,I)  Vector of derivatives
C     ICOUNT   Local counter for NUMINC
C     PESTR(K,I)     Total pesticide in each compartment
C     THETN(I) Total water content in each compartment
C
C     +  +  + PARAMETERS +  +  +
C
      PARAMETER    (N = 6)
C
C     +  +  + COMMON BLOCKS +  +  +
C
      INCLUDE 'CBIO.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
C
C     + + + LOCAL VARIABLES + + + 
C
      REAL*8       H, KXX, SWATR(NCMPTS), ATERM, QTERM
      REAL*8       P(NCMPTS), DBD(NCMPTS) 
      REAL*8       W(NCMPTS), WD(NCMPTS), WS(NCMPTS), RELTST, COMVOL 
      REAL*8       FF(N,NCMPTS), EE
      REAL*8       OLDY1(NCMPTS), OLDY2(NCMPTS), OLDY3(NCMPTS)
      REAL*8       OLDY4(NCMPTS), OLDY5(NCMPTS), OLDY6(NCMPTS)
      INTEGER*4    ICOUNT, NUMINC, I, J, L, RCALC, IDNINT
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC DBLE,IDNINT
C
C     + + + EXTERNALS + + +
C
      EXTERNAL RCALC,RELTST
C
C     + + + END SPECIFCATIONS + + + 
C     
C     Calculate time step constant
      L = 0
C     Calculate compartment variables
      DO 89 I = 1,NCOM2
        DBD(I)  = 0.0
        P(I)    = 0.0
        W(I)    = 0.0
        WD(I)   = 0.0
        WS(I)   = 0.0
        OLDY1(I) = 0.0
        OLDY2(I) = 0.0
        OLDY3(I) = 0.0
        OLDY4(I) = 0.0
        OLDY5(I) = 0.0
        OLDY6(I) = 0.0
        DO 88 J = 1,N
          FF(J,I) = 0.0
88      CONTINUE
89    CONTINUE
      DO 99 I = 1,NCOM2
        COMVOL = DBLE(DELX(I))
        DBD(I) = DBLE(BD(I))
        P(I)   = DBD(I) * COMVOL
        SWATR(I)  = DBLE(THETN(I)) * COMVOL
        W(I)   = SWATR(I) / P(I)
        WD(I)  = (1. + W(I)) / DBLE(KD(K,I) + W(I))
        WS(I)  = W(I) / (1. + W(I))
C       calculate pesticide concentration in moist soil (St)
C       PESTR is in g pesticide / cm**2 
C       Y(5....) is in mg pesticide / g moist soil
        Y(5,K,I) = (PESTR(K,I) * 1000.) / (DBD(I) * DELX(I))
        Y(6,K,I) = C12(K,I) / SWATR(I)
C       Save old y values
        OLDY1(I) = Y(1,K,I)
        OLDY2(I) = Y(2,K,I)
        OLDY3(I) = Y(3,K,I)
        OLDY4(I) = Y(4,K,I)
        OLDY5(I) = Y(5,K,I)
        OLDY6(I) = Y(6,K,I)
 99   CONTINUE
C
      H = 0.10D0
      NUMINC = IDNINT(1.0D0/H)
C     Begin main loop here
      DO 100 I = 1,NCOM2
C       set timestep loop counter
        ICOUNT = 0
 120    ICOUNT = ICOUNT + 1
 130    L =  RCALC(H, FF, Y, K, I, NCOM2)
C       if RCALC is Zero, integration is done
        IF (L .NE. 1) GO TO 200
C
C     Calculate dXm/dt
        FF(1,I) = ((USM * WD(I) * Y(5,K,I) * (Y(1,K,I) / 
     +             (KSM + WD(I) * Y(5,K,I))) +
     +             (UCM * Y(6,K,I) * (Y(1,K,I) / KCM)) -
     +             (KLDM * Y(1,K,I))))
C       Calculate dXc/dt
        KXX = KC * (1. + ((Y(4,K,I) + Y(3,K,I)) / KIN))
        FF(2,I) = ((MUC * Y(6,K,I) * Y(2,K,I)/KXX) - (KLDC * Y(2,K,I)))
C       
C       Calculate dXs/dt
        FF(3,I) = ((US * Y(6,K,I) * Y(3,K,I) / MKS) -
     +           (KL1 * WD(I) * Y(5,K,I) * Y(3,K,I))-(KLDS * Y(3,K,I)))
C       
C       Calculate dXr
        FF(4,I) = ((UR * Y(6,K,I) * Y(4,K,I) / KR) - (KLDR * Y(4,K,I)))
C       
C       Calculate dSt/dt
        EE = ((KE/WS(I)) * Y(2,K,I))
        FF(5,I) = (-1./YSM * (1/WS(I)) * USM * Y(5,K,I) * 
     +         (Y(1,K,I)/(KSM + WD(I) * Y(5,K,I)))
     +         - KL2 * EE * Y(5,K,I) / (KCM + WD(I) * Y(5,K,I)))          
C       
C       Calculate dCw/dt
        QTERM = Q(I) * (KLDM*Y(1,K,I) + KLDC*Y(2,K,I) + KLDS*Y(3,K,I)  
     +                    + KL1*WD(I)*Y(5,K,I)*Y(3,K,I) + 
     +                      KLDR*Y(4,K,I)) / WS(I)
        ATERM = - (AM * Y(1,K,I) / WS(I)) - (AC * Y(2,K,I) / WS(I))
     +          - (AS * Y(3,K,I) / WS(I)) - (AR * Y(4,K,I) / WS(I)) 
        FF(6,I) = (KSK * (CM(I)/WS(I) + QTERM - Y(6,K,I)) 
     +     - (1/YCM) * (1/WS(I)) * UCM * Y(6,K,I) * (Y(1,K,I)/KCM)
     +     - (1/YC)  * (1/WS(I)) * MUC  * Y(6,K,I) 
     +               * (Y(2,K,I)/(KC*(1+((Y(4,K,I)+Y(3,K,I))/KIN))))
     +     - (1/YS)  * (1/WS(I)) * US  * Y(6,K,I) * (Y(3,K,I)/MKS)              
     +     - (1/YR)  * (1/WS(I)) * UR  * Y(6,K,I) * (Y(4,K,I)/KR)              
     +        + ATERM )
C
        GO TO 130
C       End of integration 
 200    CONTINUE     
C       Check for one day
        IF (ICOUNT .LE. NUMINC) GO TO 120
        C12(K,I) = Y(6,K,I) * SWATR(I)
C     End of compartment loop is 100
 100  CONTINUE
C     Pesticide rate calculations go here
      DO 301 I = 1,NCOM2
        IF (OLDY5(I).LE.0.0) THEN
          DKBIO(K,I) = 0.0
        ELSE
          DKBIO(K,I) = Y(5,K,I)/OLDY5(I)
          DKBIO(K,I) = 1.0D0 - RELTST(DKBIO(K,I))
        ENDIF
 301  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRZEXM (CHMNUM)
C
C     + + + PURPOSE + + +
C
C     To create output file for EXAMS model
C     Modification date: 2/13/92
C     Further modified by PV, at AQUA TERRA Consultants 9/93
C     to output the chemicals considered for simulation
C
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER*4 CHMNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     CHMNUM  - number of current chemical being simulated
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'EXAM.INC'
      INCLUDE 'CONSTP.INC'
C
C     + + + LOCAL VARIABLES + + +
C
C      INTEGER*4    LENACT,I,K,J,ILEN
      INTEGER*4    CNAP,K,J
      REAL         SLTNHA
C      REAL         EXTAPP,TRFLX1,TEFLUX
      CHARACTER*2  YREXTN
      CHARACTER*6  LOC
      CHARACTER*19 CROP
      CHARACTER*75 CHEM
      CHARACTER*20 SOIL
C
C     + + + OUTPUT FORMATS + + +
C
18    FORMAT(1X,'!')
19    FORMAT(1X,'!',1X,'FILENAME: ',A72)
20    FORMAT(1X,F8.2,2X,'19',I2,2X,I2)
21    FORMAT(1X,'!',1X,'MLRA: ',A6,2X,'SOIL: ',A20,2X,'CROP: ',A19)
26    FORMAT(1X,'!',1X,'CHEM: ',A75)
22    FORMAT(1X,'!',1X,'App Date: ',2(I2,'/'),I2,3X,
     *       'App Rate:',3F8.3,1X,'kg/ha')
23    FORMAT(1X,'!!')
25    FORMAT(1X,'!','Record 1',/,
     *       1X,'!','  FLD 1 - Area of Field (ha)',/,
     *       1X,'!','  FLD 2 - Year of Simulation',/,
     *       1X,'!','  FLD 3 - Number of Pesticide Applications',/,
     *       1X,'!','Record 2 - # Number of Pesticide Applications',/,
     *       1X,'!','  FLD 1 - Month',/,
     *       1X,'!','  FLD 2 - Day',/,
     *       1X,'!','  FLD 3 - Application Rate (kg/ha)',/,
     *       1X,'!','Record (2+#Apps) - End of File',/,
     *       1X,'!','  FLD 1 - Month',/,
     *       1X,'!','  FLD 2 - Day',/,
     *       1X,'!','  FLD 3 - Runoff Depth (cm/day)',/,
     *       1X,'!','  FLD 4 - Runoff Pesticide Flux up to NCHEM',
     + ' ((g/cm2)/day)',/,
     *       1X,'!','  FLD 5 - Erosion Soil Loss ((tonnes/ha)/day)',/,
     *       1X,'!','  FLD 6 - Erosion Pesticide Flux up to NCHEM',
     + ' ((g/cm2)/day)'/,
     *       1X,'!','  FLD 7 - Precipitation (cm)')
30    FORMAT(1X,I2,1X,I2,2X,9(E10.4,1X))
33    FORMAT(1X,I2,1X,I2,2X,3F8.3)
C
C     + + + END SPECIFICATIONS + + +
C
        CNAP=0
C       create exams load file names & file header
        IF (IY .NE. IYOLD) THEN
          WRITE(LOC,'(6A1)')TITLE(32:37)
          WRITE(CROP,'(19A1)')HTITLE(44:62)
          WRITE(CHEM,'(3A20)')(PSTNAM(K),K=1,NCHEM)
          WRITE(SOIL,'(20A1)')STITLE(35:54)
            IF (IYOLD .NE. -9) THEN
             CLOSE(56)
            ENDIF
          IYOLD=IY
          WRITE (YREXTN,'(I2)') IY
          OPEN(56,FILE='PRZM2EXA.D'//YREXTN
     +           ,STATUS='UNKNOWN',ERR=999)
C         write out Header 
          WRITE(56,19) 'PRZM2EXA.D'//YREXTN
          WRITE(56,21) LOC,SOIL,CROP
          WRITE(56,26) CHEM
C
          DO 5 K=1,NAPS
            WRITE(56,22)APMEX(K),APDEX(K),IAPYR(K),
     *       (TAPP(J,K)*1E5,J=1,CHMNUM)
  5       CONTINUE
C
          WRITE(56,18)
          WRITE(56,25)
          WRITE(56,23)
          DO 35 K=1,NAPS
             IF(IAPYR(K).EQ.IY)THEN
                CNAP=CNAP+1
                CNAP2=CNAP2+1
             ENDIF
 35       CONTINUE
          WRITE(56,20)AFIELD,IY,CNAP
          DO 36 K=1,CNAP
             WRITE(56,33)APMEX(K+OFFST),APDEX(K+OFFST),
     *       (TAPP(J,K)*1E5,J=1,CHMNUM)
 36       CONTINUE
        OFFST=CNAP2
        ENDIF
C       write out EXAMS loadings data
        SLTNHA=SEDL/AFIELD
        IF(RUNOF.GT.R0MIN)
     *      WRITE(56,30)MONTH,DOM,RUNOF,(ROFLUX(J),J=1,CHMNUM),SLTNHA,
     *                  (ERFLUX(J),J=1,CHMNUM),PRECIP
C
999   CONTINUE
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   RCALC(
     I                 H, FF, Y, K, I, NCOM2)
C
C     +  +  + PURPOSE +  +  +
C     4th order Runge-Kutta method of integrating biodegradation
C     differential equation.  After Carnahan et al.,1969
C     in Applied Numerical Methods. 
C     Modification date: 2/11/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER*4 N,K,I,NCOM2
      PARAMETER (N=6)
      REAL*8    H,FF(N,NCOM2),Y(N,3,NCOM2)
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     SAVEY(N) -  is initial value of Y(J,K,I) 
C     PHI(N)   -  is initial value of FF(J,I)
C     H        -  timestep size
C     NCOM2    -  number of compartments 
C     Y(J,K,I) -  vector of solution
C     FF(J,I)  -  vector of derivatives
C     INTFLG   -  flag for sucessful integration 
C     K        -  chemical number being simulated
C
C     +  +  + PARAMETERS +  +  +
C 
C      PARAMETER (N=6)
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC DABS
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER*4 J,M
      REAL*8    SAVEY(N), PHI(N), KK1(N), KK2(N), KK3(N), KK4(N)
C
C     +  +  + DATA INTIALIZATIONS +  +  +
C
      DATA M / 0 /
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      M = M + 1
C     Pass 1 - use initial values
      IF (M .EQ.1) THEN
        DO 11 J = 1, N
C         Store original values of Y1 to Y6
          SAVEY(J) = Y(J,K,I)
   11   CONTINUE
        RCALC = 1
        RETURN 
C     Pass 2 - store initial, increment input
      ELSE IF (M .EQ. 2) THEN
        DO 22 J = 1, N 
C         KK1 equals hf(y)
          KK1(J) = H * FF(J,I)
          PHI(J) = KK1(J)
C         to get f(y+KK1/2) we need to send (y+KK1/2)
          Y(J,K,I) = SAVEY(J) + (KK1(J) / 2.0D0)
   22   CONTINUE
        RCALC = 1
        RETURN
C     Pass 3 - store next k term, calculate y3
      ELSE IF (M .EQ. 3) THEN
        DO 33 J = 1, N
C         KK2 = hf(y+KK1/2)
          KK2(J) = H * FF(J,I)
          PHI(J) = PHI(J) + (2.0D0 * KK2(J)) 
          Y(J,K,I) = SAVEY(J) + (KK2(J) / 2.0D0)
   33   CONTINUE
        RCALC = 1
        RETURN
C     Pass 4 - store next k term, calculate y4
      ELSE IF (M .EQ. 4) THEN
        DO 44 J = 1, N
C         KK3 = hf(y+KK2/2)
          KK3(J) = H * FF(J,I)
          PHI(J) = PHI(J) + (2.0D0 * KK3(J)) 
          Y(J,K,I) = SAVEY(J) + KK3(J)
   44   CONTINUE
        RCALC = 1
        RETURN
C     Pass 5 - complete the integration
      ELSE IF (M .EQ. 5) THEN
        DO 55 J = 1, N                  
C         KK4 = hf(y+KK3)
          KK4(J) = H * FF(J,I)
          PHI(J) = PHI(J) + KK4(J)
          Y(J,K,I) = SAVEY(J) + (PHI(J) / 6.0)
   55   CONTINUE
        M = 0
        RCALC = 0
        RETURN
C
      ENDIF  
C
      RETURN
      END
C
      SUBROUTINE   INIT
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'CONSTP.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          R2PREC,D2PREC,TI
      REAL             R1,R2,R3,R4,  TR
      DOUBLE PRECISION D1,D2,D3,D4
C
C     + + + FUNCTIONS + + +
      REAL             RNOP
      DOUBLE PRECISION DNOP

C     + + + INTRINSICS + + +
      INTRINSIC  INT,LOG10,DBLE
C
C     + + + EQUIVALENCE STATEMENTS + + +
      EQUIVALENCE (TR,TI)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' UNKNOWN DOUBLE PRECISION FORMAT, using default double pre
     .cision values!')
 2010 FORMAT(' UNKNOWN MACHINE TYPE, using default precision values!')
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     No OPperation, used to keep Ryan/McFarland optimization honest
      RNOP(R1) = R1
      DNOP(D1) = D1
C
C     + + + END SPECIFICATIONS + + +
C
C     Calculate machine dependent numeric constants
C
C     Determine the number of decimal digits of REAL precision number and
C     the smallest REAL greater than 1.0.
C     First find the number of significant binary digits, then convert
C     it to the number of significant decimal digits.  Any machine used
C     today is going to have more than 7 binary digits of precision 
C     (actually, we're cheating, because 1 is added to R2PREC after
C     it is tested.  This usually results in 7 decimal digits of
C     precision, which is usually the case, whereas strictly
C     speaking only 6 decimal digits are guaranteed, and 6 is
C     usually the result if R2PREC is initialized to 6).
      R2PREC = 7
      R1 = 1.0
      R2 = 0.0078125
 100  CONTINUE
      R3 = R1 + R2
C     this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C     the precision of an 80 bit floating point register is computed
C     (instead of a 4-byte real) when R/M optimization is turned on.
C     You can comment out the call to NOP if you observe that by
C     doing so DECCHR doesn't provide extra digits of precision.
      R4 = RNOP(R3)
      IF (R1 .LT. R3) THEN
         RP1MIN = R3
         R2PREC = R2PREC + 1
         R2 = R2 / 2.0
         GO TO 100
      END IF
C
      RPREC = INT(LOG10(2.0**R2PREC))
C
C     Determine the number of decimal digits of the typical DOUBLE precision
C     number and the smallest DOUBLE greater than 1.0D0.  
      D2PREC = 7
      D1 = 1.0D0
      D2 = 0.0078125D0
 200  CONTINUE
      D3 = D1 + D2
C     this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C     the precision of an 80 bit floating point register is computed
C     (instead of a 4-byte real) when R/M optimization is turned on.
C     You can comment out the call to NOP if you observe that by
C     doing so DECCHR doesn't provide extra digits of precision.
      D4 = DNOP(D3)
      IF (D1 .LT. D3) THEN
         DP1MIN = D3
         D2PREC = D2PREC + 1
         D2 = D2 / 2.0D0
         GO TO 200
      END IF
C
      DPREC = INT(LOG10(2.0D0**D2PREC))
C
      TR = 1.0
      IF (TI .EQ. 1065353216) THEN
C       this should be the case for the Sun or Ryan/McFarland
        R1   = 1.0E-19
C        R0MIN = 1.17549435E-19 * R1
        R0MIN = 1.1754945E-19 * R1
        R1   = 1.0E+19
        R0MAX = 3.40282347E+19 * R1
        D1   = 1.0D-28
        D0MIN = (2.22507385850720219D-28 * D1**10)
        D1   = 1.0D+28
        D0MAX = (1.7976931348623157D0 * D1**11)
      ELSE IF (TI .EQ. 16512) THEN
C       this should be the case for the VAX
        R1   = 1.0E-20
        R0MIN = 2.9387359E-19 * R1
        R1   = 1.0E+18
        R0MAX = 1.7014117E+20 * R1
        IF (DPREC .EQ. 17) THEN
C          this should be the case for the default /NOG_FLOAT compiler option
           D1   = 1.0D-20
           D0MIN = 2.938735877055719D-19 * D1
           D1   = 1.0E+19
           D0MAX = 1.7014118346046923D+19 * D1
        ELSE IF (DPREC .EQ. 16) THEN
C          this should be the case for the /G_FLOAT option
           D3 = 1.0D-21
           D4 = 1.0D-20
           D0MIN = (5.562684646268008D-20)*(D4**5)*(D3**9)
           D3 = 1.0D+21
           D4 = 1.0D+20
           D0MAX = (8.988465674311578D+21) * (D3**6) * (D4**8) 
        ELSE 
C          assume D_FLOAT real*8 type
           WRITE(*,2000)
           D1   = 1.0D-20
           D0MIN = 2.938735877055719D-19 * D1
           D1   = 1.0E+19
           D0MAX = 1.7014118346046923D+19 * D1
        END IF
      ELSE
        WRITE(*,2010)
        R1    = 1.0E-19
        R0MIN = 1.17549435E-19 * R1
        R1    = 1.0E+18
        R0MAX = 1.7014117E+20 * R1
        D1    = 1.0D-20
        D0MIN = 2.938735877055719D-19 * D1
        D1    = 1.0E+19
        D0MAX = 1.7014118346046923D+19 * D1
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE PDSTRB 
     I                  (APPAMT,CHEM,APPDEP)
C
C
C     This routine distributes a chemical application down to
C     an input depth
C      
C     + + + ARGUMENT DEFINITIONS + + +
C
C     CHEM   - chemical id number (1-3)
C     APPAMT - total amount of chemical to distribute in the soil
C     APPDEP - depth to which chemical should be applied;
C
C     + + + ARGUMENTS + + +
      INTEGER  CHEM
      REAL     APPAMT,APPDEP
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,CMPT
      REAL     DEP
C
C     + + + END SPECIFICATIONS + + +
C
      CMPT= 0
      DEP= 0.0
 10   CONTINUE
        CMPT= CMPT + 1
        DEP= DEP + DELX(CMPT)
      IF (DEP .LT. APPDEP) GO TO 10
C
      DO 20 I= 1, CMPT
        SOILAP(CHEM,I)= APPAMT*(DELX(I)/DEP)
        PESTR(CHEM,I)= PESTR(CHEM,I) + SOILAP(CHEM,I)/(DELX(I)*THETO(I))
        SPESTR(CHEM,I)= PESTR(CHEM,I)*THETO(I)/(THETO(I) + 
     *                  KD(CHEM,I)*BD(I) + (THETAS(I) - THETO(I))*
     *                  KH(CHEM,I))
 20   CONTINUE
C
      RETURN
      END
      SUBROUTINE   OUTTAB
C
C     + + + PURPOSE + + +
C
C     Accumulates and outputs daily, monthly, and
C     annual summaries for water and pesticide fluxes.
C     Written by SBC, AQUA TERRA Consultants
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXYRS.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'TABLE.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
C
      REAL        TRAIN(MXYRS),TSURF(MXYRS),TIFLO(MXYRS),TBFLO(MXYRS),
     1            TTFLO(MXYRS),TEVPO(MXYRS),TROPST(MXYRS,3),
     2            TSEDI(MXYRS),TERPST(MXYRS,3),TLPST(MXYRS,3),
     3            TBPST(MXYRS,3),SUMSURF,SUMIFLO,SUMBFLO,SUMTFLO,SUMTSED
      INTEGER     MN,IIY,KK,II,GAP,LOOP,K,KYEAR,KEND,KSTRT
      CHARACTER*4 WATR,MNTH,DAY
C
C     + + + INTRINSICS + + +
C
      INTRINSIC   INT
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA MNTH/'MNTH'/,DAY/' DAY'/,WATR/'WATR'/
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT ('IF SIMULATING MORE THAN 25 YEARS WITH MONTHLY OUTPUT
     1         OPTION, TABLE WILL END AT 25 YEARS UNLESS USER CHANGES
     2         ARRAY BOUNDS IN OUTTAB')
2005  FORMAT ('ERROR!!  NO WATER IN SYSTEM')
2010  FORMAT (T27,'PRZM-2 WATER AND PESTICIDE SUMMARY')
2020  FORMAT (/,T10,'YEAR:',I4,T27,'JAN',T37,'FEB',T47,'MAR',T57,'APR',
     1        T67,'MAY',T77,'JUN')
2030  FORMAT (/,/,T10,'YEAR:',I4,T27,'JUL',T37,'AUG',T47,'SEP',T57,'OCT'
     1        ,T67,'NOV',T77,'DEC')
2040  FORMAT (/,'RAINFALL (cm)',T20,6F10.3)
2050  FORMAT ('RUNOFF (cm)',/,2X,'SURFACE',T20,6F10.3,/,2X,'INTERFLOW',
     1        T20,6F10.3,/,2X,'BASEFLOW',T20,6F10.3,/,2X,'TOTAL',T20,
     2        6F10.3) 
2060  FORMAT ('EVAPORATION (cm)',/,2X,'TOTAL',T20,6F10.3)
2070  FORMAT ('EROSION (tonnes)',T20,6F10.4,/)
2080  FORMAT ('PESTICIDE LOSSES (kg/ha)')  
2089  FORMAT (1X,A20)
2090  FORMAT (1X,A20)
2091  FORMAT (2X,'SURFACE RUNOFF',T20,6F10.4)
2092  FORMAT (2X,'INTERFLOW',T20,6F10.4)
2093  FORMAT (2X,'BASEFLOW',T20,6F10.4)
2094  FORMAT (2X,'ERODED',T20,6F10.4)
2100  FORMAT (2X,'TOTAL RAINFALL',T20,6F10.3)
2200  FORMAT (2X,'TOTAL SURFACE',T20,6F10.3)
2300  FORMAT (2X,'TOTAL INTERFLOW',T20,6F10.3)
2400  FORMAT (2X,'TOTAL BASEFLOW',T20,6F10.3)
2500  FORMAT (2X,'TOTAL FLOW',T20,6F10.3)
2600  FORMAT (2X,'TOTAL ET',T20,6F10.3)
2700  FORMAT (2X,'TOTAL EROSION',T20,6F10.3)
2800  FORMAT (/,/,'ANNUAL SUMMARY:',/,10X,'YEAR:',T20,6I10)
2801  FORMAT ('ANNUAL HYDROLOGIC SUMS (WATER IN CM; EROSION IN TONNES)') 
2802  FORMAT (/,'ANNUAL PESTICIDE LOSSES (kg/ha)')
2803  FORMAT (/,/,'HYDROLOGY SIMULATION TOTALS:',T30,2X,'TOTAL (cm)',
     1        T50,'% OF TOTAL',/)
2804  FORMAT ('SURFACE RUNOFF',T30,F10.3,T50,F10.3)  
2805  FORMAT ('INTERFLOW',T30,F10.3,T50,F10.3)  
2806  FORMAT ('BASEFLOW',T30,F10.3,T50,F10.3)  
2807  FORMAT ('TOTAL OUTFLOW',T30,F10.3)  
3000  FORMAT (/,'% OF TOTAL RUNOFF')
3100  FORMAT (2X,'% SURFACE  ',T20,6F10.3)
3200  FORMAT (2X,'% INTERFLOW',T20,6F10.3) 
3300  FORMAT (2X,'% BASE FLOW',T20,6F10.3) 
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITEM1 .EQ. WATR .AND. (STEP1 .EQ. MNTH .OR. STEP1 .EQ. DAY)) 
     1  THEN 
       OPEN(19,FILE='PTAB.OUT')
C
      WRITE (19,2010)       
      II=0
      DO 10 IIY=STARTYR,ENDYEAR
        II=II+1
        WRITE (19,2020)IIY
        WRITE (19,2040) (RAIN(MN,II),MN=1,6) 
        WRITE (19,2050) (SURF(MN,II),MN=1,6),(IFLO(MN,II),MN=1,6),
     1                  (BFLO(MN,II),MN=1,6),(TFLO(MN,II),MN=1,6)
        WRITE (19,2060) (EVPO(MN,II), MN=1,6)
        WRITE (19,2070) (SEDI(MN,II), MN=1,6)
        WRITE (19,2080)
        DO 20 KK=1,NCHEM
          WRITE (19,2090) PSTNAM(KK)
          WRITE (19,2091) (ROPST(MN,II,KK),MN=1,6)
          WRITE (19,2092) (LPST(MN,II,KK),MN=1,6)
          WRITE (19,2093) (BPST(MN,II,KK),MN=1,6)
          WRITE (19,2094) (ERPST(MN,II,KK),MN=1,6)
20      CONTINUE
        WRITE (19,2030)IIY
        WRITE (19,2040) (RAIN(MN,II),MN=7,12) 
        WRITE (19,2050) (SURF(MN,II),MN=7,12),(IFLO(MN,II),MN=7,12),
     1                  (BFLO(MN,II),MN=7,12),(TFLO(MN,II),MN=7,12)
        WRITE (19,2060) (EVPO(MN,II), MN=7,12)
        WRITE (19,2070) (SEDI(MN,II), MN=7,12)
        WRITE (19,2080)
        DO 30 KK=1,NCHEM
          WRITE (19,2090) PSTNAM(KK)
          WRITE (19,2091) (ROPST(MN,II,KK),MN=7,12)
          WRITE (19,2092) (LPST(MN,II,KK),MN=7,12)
          WRITE (19,2093) (BPST(MN,II,KK),MN=7,12)
          WRITE (19,2094) (ERPST(MN,II,KK),MN=7,12)
30      CONTINUE
        TROPST(II,1) = 0.0
        TROPST(II,2) = 0.0
        TROPST(II,3) = 0.0
        TLPST(II,1) = 0.0
        TLPST(II,2) = 0.0
        TLPST(II,3) = 0.0
        TBPST(II,1) = 0.0
        TBPST(II,2) = 0.0
        TBPST(II,3) = 0.0
        TERPST(II,1) = 0.0
        TERPST(II,2) = 0.0
        TERPST(II,3) = 0.0
        TRAIN(II)=0.0
        TSURF(II)=0.0 
        TIFLO(II)=0.0
        TBFLO(II)=0.0 
        TEVPO(II)=0.0
        TSEDI(II)=0.0
        TTFLO(II)=0.0
          DO 200 KK=1,NCHEM
            DO 300 MN=1,12
             TROPST(II,KK) = TROPST(II,KK) + ROPST(MN,II,KK)
             TLPST(II,KK) = TLPST(II,KK) + LPST(MN,II,KK)
             TBPST(II,KK) = TBPST(II,KK) + BPST(MN,II,KK)
             TERPST(II,KK) = TERPST(II,KK) + ERPST(MN,II,KK)
300         CONTINUE
200       CONTINUE
        DO 100 MN=1,12       
          TRAIN(II)=TRAIN(II)+RAIN(MN,II)
          TSURF(II)=TSURF(II)+SURF(MN,II) 
          TIFLO(II)=TIFLO(II)+IFLO(MN,II) 
          TBFLO(II)=TBFLO(II)+BFLO(MN,II) 
          TEVPO(II)=TEVPO(II)+EVPO(MN,II) 
          TSEDI(II)=TSEDI(II)+SEDI(MN,II) 
          TTFLO(II)=TTFLO(II)+TFLO(MN,II) 
100     CONTINUE
        IF (TTFLO(II).LE.0.0) THEN
          WRITE (19,2005)
          STOP
        ENDIF

10    CONTINUE
      GAP=(ENDYEAR-STARTYR)+1
      IF (GAP.GT.25) WRITE (19,2000)
      SUMSURF=0.0
      SUMIFLO=0.0
      SUMBFLO=0.0
      SUMTFLO=0.0
      SUMTSED=0.0
      DO 500 II=1,GAP
        SUMSURF=SUMSURF+TSURF(II)
        SUMIFLO=SUMIFLO+TIFLO(II)
        SUMBFLO=SUMBFLO+TBFLO(II)
        SUMTFLO=SUMTFLO+TTFLO(II)
        SUMTSED=SUMTSED+TSEDI(II)
500   CONTINUE
      LOOP = INT(GAP/6)+1
      DO 60 K=1,LOOP
        KYEAR = STARTYR+((K-1)*6)
        KSTRT = ((K-1)*6)+1
        IF (K.EQ.LOOP) THEN
          KEND = (ENDYEAR-KYEAR)+1
        ELSE
          KEND = 6
        ENDIF
        WRITE (19,2800) (II,II=KYEAR,KYEAR+(KEND-1))
        WRITE (19,2801)
        WRITE (19,2100) (TRAIN(II),II=KSTRT,KEND)
        WRITE (19,2200) (TSURF(II),II=KSTRT,KEND)
        WRITE (19,2300) (TIFLO(II),II=KSTRT,KEND)
        WRITE (19,2400) (TBFLO(II),II=KSTRT,KEND)
        WRITE (19,2500) (TTFLO(II),II=KSTRT,KEND)
        WRITE (19,2600) (TEVPO(II),II=KSTRT,KEND)
        WRITE (19,2700) (TSEDI(II),II=KSTRT,KEND)
        WRITE (19,3000)
        WRITE (19,3100) (TSURF(II)/TTFLO(II)*100.,II=KSTRT,KEND)
        WRITE (19,3200) (TIFLO(II)/TTFLO(II)*100.,II=KSTRT,KEND)
        WRITE (19,3300) (TBFLO(II)/TTFLO(II)*100.,II=KSTRT,KEND)
        WRITE (19,2802)
        DO 50 KK=1,NCHEM
          WRITE (19,2089) PSTNAM(KK)
          WRITE (19,2200) (TROPST(II,KK),II=1,KEND)
          WRITE (19,2300) (TLPST(II,KK),II=1,KEND)
          WRITE (19,2400) (TBPST(II,KK),II=1,KEND)
          WRITE (19,2700) (TERPST(II,KK),II=1,KEND)
50      CONTINUE
60    CONTINUE
      WRITE (19,2803)
      WRITE (19,2804)  SUMSURF,(SUMSURF/SUMTFLO)*100.
      WRITE (19,2805)  SUMIFLO,(SUMIFLO/SUMTFLO)*100.
      WRITE (19,2806)  SUMBFLO,(SUMBFLO/SUMTFLO)*100. 
      WRITE (19,2807)  SUMTFLO 
C
      ENDIF
      RETURN
      END
C
      BLOCK DATA PRZBLK
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'CMISC.INC'
C
      DATA DAYCNT / 0 /
C
      END

