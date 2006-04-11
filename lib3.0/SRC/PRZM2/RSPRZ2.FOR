C
C
C
      SUBROUTINE TRDIA1 (A,B,C,X,F,N,LPRZOT,MODID)
C
C     + + + PURPOSE + + +
C
C     Solves a system of equations with a tridiagonal coefficient matrix
C     Modofication date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +           
C
      INCLUDE 'PPARM.INC'
      INTEGER     N,LPRZOT
      REAL        A(NCMPTS),B(NCMPTS),C(NCMPTS),F(NCMPTS),X(NCMPTS)
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +          
C
C     LPRZOT....Fortran unit number for output file
C     MODID...character string for identification of output file LPRZOT
C
C     + + + PARAMETERS + + +
C
C      INCLUDE 'PPARM.INC'
C
C     + + + LOCAL VARIABLES + + +         
C
      INTEGER   I,NM1
      REAL      U(NCMPTS),Y(NCMPTS),L(NCMPTS),DLIMIT
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
C
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C-----Data intializations             
C
      DATA U,Y,L / NCMPTS*0.0, NCMPTS*0.0, NCMPTS*0.0 /
      DATA DLIMIT /1.0E-10/
C
C-----End specifications              
C
      MESAGE = 'TRDIA1'
      CALL SUBIN(MESAGE)
C
C-----Factor matrix into upper and lower halves
C
      U(1)=B(1)
      DO 10 I=2,N
        IF(ABS(U(I-1)).LT.1.0E-5) GO TO 40
        L(I)=A(I)/U(I-1)
        U(I)=B(I)-L(I)*C(I-1)
10    CONTINUE
C
C-----Solve LUX=F
C
      Y(1)=F(1)
      DO 20 I=2,N
         Y(I)=F(I)-L(I)*Y(I-1)
20    CONTINUE
C
      IF(ABS(U(N)).LT.1.0E-5) GO TO 40
         X(N)=Y(N)/U(N)
      IF (X(N).LE.DLIMIT) X(N) = 0.0
         NM1=N-1
         DO 30 I=1,NM1
           X(N-I)=(Y(N-I)-C(N-I)*X(N+1-I))/U(N-I)
           IF (X(N-I).LE.DLIMIT) X(N-I) = 0.0
30       CONTINUE
         GO TO 800
40    CONTINUE
      WRITE(LPRZOT,2000) (MODID,I=1,11)
C
C-----FORMAT statements
C
2000  FORMAT (1X,A3,1X,110(1H*),/,1X,A3,1X,110(1H*),/,1X,A3,/,1X,A3,50X,
     1        'E R R O R',/,1X,A3,/,1X,A3,10X,'TRIDIAGONAL MATRIX IN ',
     2        'SUBROUTINE TRDIA COULD NOT BE SOLVED FOR THIS DAY. ',
     3        'PRZM WILL',/,1X,A3,10X,' USE VALUES FOR THE LAST TIME ',
     4        'STEP AND CONTINUE ON. YOU MAY WANT TO STOP AND CHECK',/,
     5        1X,A3,10X,'BOTH THE INPUT SEQUENCE AND THE CODE IN ',
     6        'SLPEST AND TRDIA',/,1X,A3,/,1X,A3,/,1X,A3,1X,110(1H*))
C
 800  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE PESTAP (K)
C
C     + + + PURPOSE + + +
C     Computes amount and location of pesticide application
C     (foliage, soil surface, or soil layer)
C     Modification date: 2/18/92 JAM
C     Further modified by PV @ AQUA TERRA Consultants 9/93 to hardwire
C     the calculation of pesticide depth in runoff to 1 cm 
C     
C     + + + DUMMY ARGUMENTS + + +                 
C
      INTEGER   K
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     K  -  chemical being simulated (1-3)
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXNSZ.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +                 
C
      REAL         DEP,APPAMT
      CHARACTER*80 MESAGE
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
C
      INTRINSIC REAL,DBLE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,EXPCHK,SUBOUT,PDSTRB,PZDSPL
C
C     + + + END SPECIFICATIONS + + +                   
C
      MESAGE = 'PESTAP'
      CALL SUBIN(MESAGE)
C
 125  FORMAT(' Surface compartment > 1 cm: Pesticide Distributed',
     *       ' to 1st compartment depth')
C
      IF (FAM .EQ. 1) THEN
C
C       Soil pesticide application
        PLNTAP(K)= 0.
C
C       the pesticide is incorporated beyond the surface compartment,
        IF (DEPI(K,NAPPC) .GT. DELX(1)) THEN
C
C         if the incorporation depth is less than 1 cm, distribute the
C         pesticide to 1 cm depth
C         call subroutine PDSTRB
          IF (DEPI(K,NAPPC) .LT. 1.0) THEN
            DEP = 1.0
            CALL PDSTRB(TAPP(K,NAPPC),K,DEP)
          ELSE
C
C         if the incorporation depth is more than or equal to 1 cm, 
C         distribute the pesticide down to the depth of incorporation
            CALL PDSTRB(TAPP(K,NAPPC),K,DEPI(K,NAPPC))
          END IF
        ELSE
C
C         Surface application of the pesticide.
C         Hard-wiring the pesticide distribution to 1 cm depth when-
C         ever it is surface applied.  Distribute the pesticide in 
C         either the first compartment thickness or 1 cm.
          IF (DELX(1) .GE. 1.0) THEN
C
C           surface compartment is greater than 1 cm. A warning will 
C           will be issued to the user.  Pesticide will be distributed
C           in the first compartment.
            PESTR(K,1) = PESTR(K,1)+ TAPP(K,NAPPC)/(DELX(1)*THETO(1))
            IF (DELX(1) .GT. 1.0) THEN
              WRITE(MESAGE,125)
              CALL PZDSPL(FECHO,MESAGE)
            END IF
            SPESTR(K,1)= PESTR(K,1)*THETO(1)/(THETO(1)+KD(K,1)*BD(1)
     1                 +(THETAS(1)-THETO(1))*KH(K,1))
            SOILAP(K,1)= TAPP(K,NAPPC)
          ELSE
C
C           the surface compartment is less than 1 cm, however
C           the pesticide will be distributed in the 1 cm depth
C           call subroutine PDSTRB
            DEP = 1.0
            CALL PDSTRB(TAPP(K,NAPPC),K,DEP)
          END IF            
        END IF
C
      ELSEIF (FAM .EQ. 2) THEN
C
C       Linear foliar pesticide application
C       message is printed to the user that the pesticide is 
C       distributed within the first compartment or 1 cm, 
C       whichever is greater. 
        IF (DELX(1) .GE. 1.0) THEN
          PLNTAP(K)    = COVER * TAPP(K,NAPPC)
          SOILAP(K,1)  = (1.0- COVER) * TAPP(K,NAPPC)
          PESTR(K,1)   = PESTR(K,1)+ SOILAP(K,1)/(DELX(1)*THETO(1))
          IF (DELX(1) .GT. 1.0) THEN
            WRITE(MESAGE,125)
            CALL PZDSPL(FECHO,MESAGE)
          END IF
          SPESTR(K,1)  = PESTR(K,1)*THETO(1)/(THETO(1)+KD(K,1)*BD(1)
     1                   +(THETAS(1)-THETO(1))*KH(K,1))
        ELSE
          DEP = 1.0           
          PLNTAP(K)    = COVER * TAPP(K,NAPPC)
          APPAMT = (1.0-COVER) * TAPP(K,NAPPC)
          CALL PDSTRB(APPAMT,K,DEP)
        END IF            
C
        FOLPST(K)    = FOLPST(K)+ PLNTAP(K)
        IFSCND(NCROP)= 0
      ELSE
C
C       Exponential foliar pesticide application
C       message is printed to the user that the pesticide is distributed
C       within the first compartment or 1 cm, whichever is greater
C
        IF (DELX(1) .GE. 1.0) THEN
          PLNTAP(K)    = (1.0-REAL(EXPCHK(DBLE(-FILTRA*WEIGHT)))) *
     1                   TAPP(K,NAPPC)
          SOILAP(K,1)  = TAPP(K,NAPPC)- PLNTAP(K)
          IF (DELX(1) .GT. 1.0) THEN
            WRITE(MESAGE,125)
            CALL PZDSPL(FECHO,MESAGE)
          END IF
          PESTR(K,1)   = PESTR(K,1)+ SOILAP(K,1)/(DELX(1)*THETO(1))
          SPESTR(K,1)  = PESTR(K,1)*THETO(1)/(THETO(1)+KD(K,1)*BD(1)
     1                   +(THETAS(1)-THETO(1))*KH(K,1))
C
        ELSE
C
C         the surface compartment is less than 1 cm, however
C         the pesticide will be distributed in the 1 cm depth
          DEP = 1.0
          PLNTAP(K)    = (1.0-REAL(EXPCHK(DBLE(-FILTRA*WEIGHT)))) *
     1                   TAPP(K,NAPPC)
          APPAMT  = TAPP(K,NAPPC)- PLNTAP(K)
          CALL PDSTRB(APPAMT,K,DEP)
        END IF            
        FOLPST(K)    = FOLPST(K)+ PLNTAP(K)
        IFSCND(NCROP)= 0
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE PLPEST (K)
C
C     + + + PURPOSE + + +
C
C     Determines amount of pesticide which disappears
C     from plant surface by first order decay.  The variable PLDKRT
C     is a pseudo first order decay rate which may include processes
C     of volatilization, oxidation, photolysis, etc.
C     also determines pesticide washed off during rainfall events.
C     Modification date: 2/18/92
C
C     + + + DUMMY ARGUMENTS + + +                 
C
      INTEGER   K
C
C     + + + ARGUMENTS DEFINITIONS + + +
C
C     K   -  pesticide index number
C
C     + + + PARAMETERS + + + 
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CHYDR.INC'
C
C     + + + LOCAL VARIABLES + + +           
C
      REAL         TERM,TERM1,TERM2,FPWLOS,ALIMIT
      CHARACTER*80 MESAGE
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
C      
      INTRINSIC REAL,DBLE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,EXPCHK,SUBOUT
C
C     + + + DATA STATEMENTS + + +
C
      DATA ALIMIT /1.0E-30/
C
C     + + + END SPECIFICATIONS + + +                 
C
      MESAGE = 'PLPEST'
      CALL SUBIN(MESAGE)
C
      IF (IFSCND(NCROP) .GE. 4) THEN
C
C     Signal to bypass this routine
C
      ELSE IF (IFSCND(NCROP) .EQ. 0 .OR. IFSCND(NCROP) .EQ. 3) THEN
C
C       Before harvest or
C       after harvest under surface-residue condition
C
        TERM   = REAL(EXPCHK(DBLE(-(FEXTRC(K)*PRECIP))))
        IF (THRUFL .LE. 0.0) TERM= 1.0
        TERM1    = REAL(EXPCHK(DBLE(-PLDKRT(K)*DELT)))
        TERM2    = REAL(EXPCHK(DBLE(-PLVKRT(K)*DELT)))
        FOLP0(K) = FOLPST(K)
        FPWLOS   = FOLP0(K)*(1.0-TERM)
        FPDLOS(K)= FOLP0(K)*(1.0-TERM1)
        FPVLOS(K)= FOLP0(K)*(1.0-TERM2)
        FOLPST(K)= FOLP0(K) -FPDLOS(K) -FPWLOS -FPVLOS(K)
C
C       Check for underflow
C
        IF(FOLPST(K).LE.ALIMIT) FOLPST(K)=0.0
        WTERM(K) = 0.0
        IF (THRUFL .GT. 0.0) WTERM(K) = FPWLOS
      ELSE IF (IFSCND(NCROP) .EQ. 1) THEN
C
C       After harvest and assign surface applied condition
C
        PESTR(K,1) = PESTR(K,1) + FOLPST(K)/(DELX(1)*THETO(1))
        SPESTR(K,1)= PESTR(K,1)*THETO(1)/(THETO(1)+KD(K,1)*BD(1)
     1              +(THETAS(1)-THETO(1))*KH(K,1))
        SOILAP(K,1)= FOLPST(K)
        IFSCND(NCROP) = 4
        FPVLOS(K)  = 0.0
        FPDLOS(K)  = 0.0
        FOLPST(K)  = 0.0
        FOLP0(K)   = 0.0
        WTERM(K)   = 0.0
      ELSE IF (IFSCND(NCROP) .EQ. 2) THEN
C
C       After harvest remode all pesticide from canopy
C
        FMRMVL(K) = FOLPST(K)
        IFSCND(NCROP) = 4
        FPVLOS(K) = 0.0
        FPDLOS(K) = 0.0
        FOLPST(K) = 0.0
        FOLP0(K)  = 0.0
        WTERM(K)  = 0.0
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE CANOPY(
     I  UTEMP,UWIND,ZRH,ZCH,URH,
     O  TOTR,CRC)
C
C     + + + PURPOSE + + +
C
C     To calculates the overall vertical transport resistance
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +                   
C
      REAL       UTEMP(2),UWIND(2),ZRH,ZCH,URH,TOTR,CRC(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     UTEMP  - ???
C     UWIND  - ???
C     ZRH    - ???
C     ZCH    - canopy height (cm)
C     URH    - ???
C     TOTR   - ???
C     CRC    - canopy resistance cm/day
C
C     + + + LOCAL VARIABLES + + +
C
      REAL         GRADW,GRADT,HTEMP,RINUM,FAIH,FAIM,SAIM,USTAR,
     1             LOGD,LOGZ0,D,Z0,DIFFCH,HGT,TOT,PAI,DIFK
      INTEGER      LAYERS,I,N
      CHARACTER*80 MESAGE
      REAL         LOGCHK, LNCHK
      REAL*8       EXPCHK, SQRCHK
C
C     +  +  + INTRINSICS +  +  + 
C      
      INTRINSIC ALOG,DBLE,ATAN,REAL
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,SQRCHK,LNCHK,LOGCHK,EXPCHK,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +             
C
      DATA PAI/3.1415926/
C
C     + + + END SPECIFICATIONS + + +        
C
      MESAGE = 'CANOPY'
      CALL SUBIN(MESAGE)
C
      GRADT=(UTEMP(2)-UTEMP(1))/ZRH
      GRADW=(UWIND(2)-UWIND(1))/ZRH
      HTEMP=(UTEMP(1)+UTEMP(2))*0.5
C
C     Computes Richardson number
C
      RINUM=(9.8/(HTEMP+273.16))*GRADT/(GRADW**2)*86400.0**2
C
C     Computes atmospheric stability correction factors
C
      IF (RINUM .LT. -0.003) THEN
        FAIH=(1.0-16.0*RINUM)**(-0.5)
        FAIM= REAL(SQRCHK(DBLE(FAIH)))
      ELSEIF (RINUM .GT. 0.003) THEN
        FAIH=1.0+5.2*RINUM
        FAIM=FAIH
      ELSE
        FAIH=1.0
        FAIM=1.0
      ENDIF
C
      SAIM=PAI/2.0-1.0+FAIM+ALOG(8.0)+3.0*LNCHK(FAIM)
     1     -2.0*LNCHK(FAIM+1.0)
     2     -LNCHK(1+FAIM**2)-2.0*ATAN(FAIM)
C
C     Computes zero displacement height (D)
C
      LOGD=0.9793*LOGCHK(ZCH)-0.1536
      D=10.0**LOGD
C
C     Computes roughness length, (Z0)
C
      LOGZ0=0.997*LOGCHK(ZCH)-0.883
      Z0=10.0**LOGZ0
C
C     Computes frictional velocity (USTAR)
C
      IF (Z0 .LE. 0.0) THEN
        USTAR=0.40*URH/(LNCHK(ZRH-D)-SAIM)
      ELSE
        USTAR=0.40*URH/(LNCHK((ZRH-D)/Z0)-SAIM)
      ENDIF
C
      DIFFCH=USTAR*0.4*(ZCH-D)/FAIH
      LAYERS = 500
      HGT = ZCH/LAYERS
      TOT = 0.0
      TOTR = 0.0
      N = 1
      DO 10 I = 1, LAYERS-1
        DIFK = DIFFCH *REAL(EXPCHK(DBLE(4.0*(HGT*(I-1)/
     1                                       ZCH-1.0))))
        TOT = TOT + HGT/DIFK
        IF (I .EQ. 249+(N-1)*250) THEN
C
C         Divide 100.0 to convert the unit of resistance
C         from day/m to day/cm
C
          CRC(N)= TOT/100.0
          TOTR  = TOTR +CRC(N)
          TOT   = 0.0
          N     = N +1
        ENDIF
10    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE SLPST0(
     I  LPRZOT, MODID, K, DKBIO)
C
C     + + + PURPOSE + + +
C
C     Sets up the coefficient matrix for the solution
C     of the soil pesticide transport equation. It then calls an equa-
C     tion solver for the tridiagonal matrix and sets up pesticide
C     flux terms using the new concentrations.
C     Modification date: 2/18/92 JAM
C     Further modified at AQUA TERRA Consultants to hard code the
C     pesticide extraction depth to 1 cm. 9/93
C
C     + + + DUMMY ARGUMENTS + + +              
C
      INTEGER     K,LPRZOT
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     LPRZOT - Fortran unit number for output file LPRZOT
C     MODID  - character string for output file identification
C     K      - chemical number being simulated (1-3)
C     DKBIO  - array containing rate of biodegradation
C     PRDPTH - runoff pesticide extraction depth
C     PFRAC  - pesticide to be distributed in the remaining depth
C              fraction
C     CMPT   - number of compartments which make up the pesticide
C              runoff extraction depth
C
C     + + + PARAMETERS + + +   
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
C
C     + + + LOCAL VARIABLES + + +            
C
      INTEGER      I,J,KLIN,CMPT
      REAL         THAIR(NCMPTS),DGAIR(NCMPTS)
      REAL         VTERM,DDLN,PFRAC,PRDPTH
      REAL*8       DKBIO(3,NCMPTS)
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  + 
C      
      EXTERNAL SUBIN,PSTLNK,TRDIAG,SUBOUT
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'SLPST0'
      CALL SUBIN(MESAGE)
C
      DO 10 J=1,NCOM2
        THAIR(J)=THETAS(J)-THETN(J)
        IF (THAIR(J) .LT. 0.0) THAIR(J) = 0.0
C       DGAIR now includes correction for air to bulk volume
        DGAIR(J)=(THAIR(J)**(10./3)/THETAS(J)**2)*DAIR * THAIR(J)
10    CONTINUE
      CALL PSTLNK(K,1)
C
C     Calculate number of compartments which make up runoff
C     pesticide extraction depth, defined by variable PRDPTH.
C
      PRDPTH = 1.0
      KLIN = 0
      DDLN = 0.0
15    CONTINUE
        KLIN = KLIN + 1
        DDLN = DDLN + DELX(KLIN)
      IF (DDLN .LT. PRDPTH) GO TO 15
      CMPT = KLIN
      PFRAC = PRDPTH - (DDLN-DELX(KLIN))
C
C     Set up coefficients for surface layer
C
      A(1)= 0.0
      B(1)= ((DISP(K,1)*THETN(1)+KH(K,1)*DGAIR(1))/(DELX(1)*
     1       DELX(1))
     1       +VEL(1)*THETN(1)/DELX(1)
     2       +(DWRATE(K,1)*THETN(1))+(DSRATE(K,1)*KD(K,1)*BD(1))
     3       +(DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))
     4       +(DGRATE(K,1)*THAIR(1)*KH(K,1))
     5       +ELTERM(K)) *DELT
     6       +THETN(1) + KD(K,1)*BD(1) + THAIR(1)*KH(K,1)
     7       +CONDUC*KH(K,1)*DELT/DELX(1)
     8       +(RTR(1,1)+RTR(2,1)+RTR(3,1))*THETN(1)*DELT
C
C     Add runoff term based on the number of compartments which
C     make up 1 cm depth.
C
      IF (CMPT .EQ. 1) THEN
        B(1) = B(1) + (RUNOF/DELX(1))
      ELSE
        B(1) = B(1) + (RUNOF/PRDPTH)  
      ENDIF
C
      C(1)= -(DISP(K,2)*THETN(2)+KH(K,1)*DGAIR(2))*DELT/(DELX(1)*
     1       0.5*(DELX(1)+DELX(2)))
      F(1)= (THETO(1)+KD(K,1)*BD(1)+(THETAS(1)-THETO(1))*OKH(K,1))
     1      *SPESTR(K,1)+WTERM(K)*DELT/DELX(1)
     2      +(SRC(1,1)+SRC(2,1)+SRC(3,1))*DELT
C
C     Calculate coefficient of non-boundary soil layers
C
      DO 20 I=2,NCOM2M
        CALL PSTLNK(K,I)
        A(I)= (-(DISP(K,I-1)*THETN(I-1)+KH(K,I-1)*DGAIR(I-1))
     1          /(DELX(I)*0.5*(DELX(I-1)+DELX(I)))
     2          -VEL(I-1)*THETN(I-1)/DELX(I)) *DELT
        B(I)= ((DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))
     1          /(DELX(I)*0.5*(DELX(I-1)+DELX(I)))
     2        + (DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))
     3          /(DELX(I)*0.5*(DELX(I)+DELX(I+1)))
     4          +VEL(I)*THETN(I)/DELX(I)
     5          +(DWRATE(K,I)*THETN(I))+(DSRATE(K,I)*KD(K,I)*BD(I))
     6          +(DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))
     +          + (DGRATE(K,I)*THAIR(I)*KH(K,I))
     7          +GAMMA(I)*ET(I)*THETN(I)/SW(I)) *DELT
     8          +THETN(I)+KD(K,I)*BD(I)+THAIR(I)*KH(K,I)
     9          +(RTR(1,I)+RTR(2,I)+RTR(3,I))*THETN(I)*DELT
C
C       Add runoff term if current compartment number is less than or
C       equal to the number of compartments which make up 1 cm depth.
C
        IF (I .LT. CMPT) THEN
          B(I) = B(I) + (RUNOF/PRDPTH) 
        ELSE  
          IF (I .EQ. CMPT) B(I) = B(I) + (RUNOF/DELX(I))*(PFRAC/PRDPTH)
        ENDIF
C
        C(I)= -(DISP(K,I+1)*THETN(I+1)+KH(K,I+1)*DGAIR(I+1))
     1         *DELT/(DELX(I)*0.5*(DELX(I)+DELX(I+1)))
        F(I)= (THETO(I)+KD(K,I)*BD(I)+(THETAS(I)-THETO(I))*OKH(K,I))
     1        *SPESTR(K,I)+(SRC(1,I)+SRC(2,I)+SRC(3,I))*DELT
C
20    CONTINUE
C
C     Calculate coefficients of bottom layer
C
      CALL PSTLNK(K,NCOM2)
C
      VTERM   = VEL(NCOM2) * THETN(NCOM2) / DELX(NCOM2)
C
      A(NCOM2)=(-(DISP(K,NCOM2M)*THETN(NCOM2M)
     1         +KH(K,NCOM2M)*DGAIR(NCOM2M))
     2         /(DELX(NCOM2)*0.5*(DELX(NCOM2M)+DELX(NCOM2)))
     3         -VEL(NCOM2M)*THETN(NCOM2M)/DELX(NCOM2))*DELT
      B(NCOM2)= ((DISP(K,NCOM2)*THETN(NCOM2)
     1         +KH(K,NCOM2)*DGAIR(NCOM2))/(DELX(NCOM2)*DELX(NCOM2))
     2         + VTERM
     3         +(DWRATE(K,NCOM2)*THETN(NCOM2))
     4         +(DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))
     5         +(DSRATE(K,NCOM2)*KD(K,NCOM2)*BD(NCOM2))
     6         +DGRATE(K,NCOM2)*THAIR(NCOM2)*KH(K,NCOM2))*DELT
     7         +THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)
     8         +THAIR(NCOM2)*KH(K,NCOM2)+(RTR(1,NCOM2)
     9         +RTR(2,NCOM2)+RTR(3,NCOM2))*THETN(NCOM2)*DELT
C
      C(NCOM2)= 0.0
      F(NCOM2)= (THETO(NCOM2)+KD(K,NCOM2)*BD(NCOM2)+(THETAS(NCOM2)-
     1           THETO(NCOM2))*OKH(K,NCOM2))*SPESTR(K,NCOM2)
     2          +(SRC(1,NCOM2)+SRC(2,NCOM2)+SRC(3,NCOM2))*DELT
C
C     Call equation solver
C
      CALL TRDIAG (A,B,C,X,F,NCOM2,LPRZOT,MODID)
C
C     Calculate pesticide fluxes
C
      PVFLUX(K,1)=-CONDUC*X(1)*KH(K,1)
      UPFLUX(K,1)=0.0
      DFFLUX(K,1)=DISP(K,1)/(0.5*(DELX(1)+DELX(2)))*X(1)*THETN(1)
     1           -DISP(K,2)/(0.5*(DELX(1)+DELX(2)))*X(2)*THETN(2)
      ADFLUX(K,1)=VEL(1)*X(1)*THETN(1)
      LTFLUX(K,1)=0.0
      DKFLUX(K,1)=DELX(1)*X(1)*(DWRATE(K,1)*THETN(1)+DSRATE(K,1)
     1            *BD(1)*KD(K,1)+DGRATE(K,1)*THAIR(1)*KH(K,1)
     2            +DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))
      WOFLUX(K)   =WTERM(K)
      IF (CMPT .EQ. 1) THEN
        RFFLUX(K,1) =RUNOF*X(1)
      ELSE
        RFFLUX(K,1) =RUNOF*X(1)*(DELX(1)/PRDPTH)
      ENDIF
      ERFLUX(K)   =ELTERM(K)*DELX(1)*X(1)
      TRFLUX(K,1) =(RTR(1,1)+RTR(2,1)+RTR(3,1))*X(1)*THETN(1)*DELX(1)
      SRCFLX(K,1) =(SRC(1,1)+SRC(2,1)+SRC(3,1))*DELX(1)
      DO 30 I=2,NCOM2M
        RFFLUX(K,I) = 0.0
        IF (I .LT. CMPT) THEN
          RFFLUX(K,I)=RUNOF*X(I)*(DELX(I)/PRDPTH)
        ELSE
          IF (I .EQ. CMPT) RFFLUX(K,I)=RUNOF*X(I)*(PFRAC/PRDPTH)
        ENDIF
        PVFLUX(K,I)=DGAIR(I)*KH(K,I)/(0.5*(DELX(I)+DELX(I+1)))*X(I)-
     1         DGAIR(I+1)*KH(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*X(I+1)
        DFFLUX(K,I)=DISP(K,I)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I)*X(I)-
     1         DISP(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I+1)*X(I+1)
        ADFLUX(K,I)=VEL(I)*X(I)*THETN(I)
        LTFLUX(K,I)=OUTFLO(I)*X(I)
        DKFLUX(K,I)=DELX(I)*X(I)*(DWRATE(K,I)*THETN(I)+DSRATE(K,I)
     1             *BD(I)*KD(K,I)+DGRATE(K,I)*THAIR(I)*KH(K,I)
     2             +DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))
        UPFLUX(K,I)=GAMMA(I)*ET(I)*X(I)
        TRFLUX(K,I)=(RTR(1,I)+RTR(2,I)+RTR(3,I))*X(I)*THETN(I)*DELX(I)
        SRCFLX(K,I)=(SRC(1,I)+SRC(2,I)+SRC(3,I))*DELX(I)
30    CONTINUE
C
      RZFLUX(K)= DISP(K,NCOMRZ)/(0.5*(DELX(NCOMRZ)+DELX(NCOMRZ+1)))
     1  *THETN(NCOMRZ)*X(NCOMRZ)-DISP(K,NCOMRZ+1)/(0.5*(DELX(NCOMRZ+1)
     2  +DELX(NCOMRZ)))*THETN(NCOMRZ+1)*X(NCOMRZ+1)
     3  +(VEL(NCOMRZ)*X(NCOMRZ)*THETN(NCOMRZ))
      RFFLUX(K,NCOM2)=0.
      DFFLUX(K,NCOM2)=0.
      PVFLUX(K,NCOM2)=0.
      UPFLUX(K,NCOM2)=0.
      ADFLUX(K,NCOM2) = VTERM * DELX(NCOM2) * X(NCOM2)
      LTFLUX(K,NCOM2)=OUTFLO(NCOM2)*X(NCOM2)
      DKFLUX(K,NCOM2)=DELX(NCOM2)*X(NCOM2)*(DWRATE(K,NCOM2)
     1            *THETN(NCOM2)+DSRATE(K,NCOM2)*BD(NCOM2)*KD(K,NCOM2)
     2            +DGRATE(K,NCOM2)*THAIR(NCOM2)*KH(K,NCOM2)
     3            +DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))
      TRFLUX(K,NCOM2)= (RTR(1,NCOM2)+RTR(2,NCOM2)+RTR(3,NCOM2))
     1                 *X(NCOM2)*THETN(NCOM2)*DELX(NCOM2)
      SRCFLX(K,NCOM2)= (SRC(1,NCOM2)+SRC(2,NCOM2)+SRC(3,NCOM2))
     1                 *DELX(NCOM2)
C
C     Calculate core flux values.
C     Multiply internal units of GR/CM**2 by 10**5 so output
C     is expressed in units of KG/HA (as in the input).
C
      DCOFLX(K) = DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
     1          - DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
     2          + (VEL(NCOM2)*X(NCOM2)*THETN(NCOM2))
      DCOFLX(K) = DCOFLX(K) * 1.0E5
      MCOFLX(K) = MCOFLX(K) + DCOFLX(K)
      YCOFLX(K) = YCOFLX(K) + DCOFLX(K)
C
C     Accumulate fluxes from soil layers for output
C
      ROFLUX(K)= 0.0
      SUPFLX(K)= 0.0
      SDKFLX(K)= 0.0
      TTRFLX(K)= 0.0
      TSRCFX(K)= 0.0
      LATFLX(K)=0.0
      DO 40 I=1,NCOM2
         ROFLUX(K)= ROFLUX(K)+RFFLUX(K,I)
         SDKFLX(K)= SDKFLX(K)+DKFLUX(K,I)
         SUPFLX(K)= SUPFLX(K)+UPFLUX(K,I)
         TTRFLX(K)= TTRFLX(K)+TRFLUX(K,I)
         TSRCFX(K)= TSRCFX(K)+SRCFLX(K,I)
         LATFLX(K)= LATFLX(K)+LTFLUX(K,I)
40    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE SLPST1 (LPRZOT,MODID,K,DKBIO)
C
C     + + + PURPOSE + + +
C
C     Sets up the coefficient matrix for the solution
C     of the soil pesticide transport equation. It then calls an equa-
C     tion solver for the tridiagonal matrix and sets up pesticide
C     flux terms using the new concentrations
C     Modification date: 2/18/92 JAM
C     Further modified at AQUA TERRA Consultants to hard code the
C     pesticide extraction depth to 1 cm. 9/93
C
C     + + + DUMMY ARGUMENTS + + +             
C
      INTEGER     LPRZOT,K
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     LPRZOT - Fortran unit number for output file LPRZOT
C     MODID  - character string for output file identification
C     K      - chemical number being simulated (1-3)
C     DKBIO  - biodegradation rate
C     PRDPTH - runoff pesticide extraction depth
C     PFRAC  - pesticide to be distributed in the remaining depth
C              fraction
C     CMPT   - number of compartments which make up the pesticide
C              runoff extraction depth
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CACCUM.INC'
C
C     + + + LOCAL VARIABLES + + +   
C
      INTEGER      I,KK,KLIN,CMPT
      REAL         THAIR(NCMPTS),DGAIR(NCMPTS)
      REAL         DDLN,PFRAC,PRDPTH
      REAL*8       DKBIO(3,NCMPTS)
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + + 
C      
      EXTERNAL SUBIN,PSTLNK,TRDIAG,SUBOUT
C
C     + + + END SPECIFICATIONS + + +     
C
      MESAGE = 'SLPST1'
      CALL SUBIN(MESAGE)
C
      DO 10 KK=1,NCOM2
        THAIR(KK)=THETAS(KK)-THETN(KK)
        IF (THAIR(KK).LT.0.0) THAIR(KK) = 0.0
        DGAIR(KK)=(THAIR(KK)**(10./3)/THETAS(KK)**2)*DAIR * THAIR(KK)
10    CONTINUE
C
      CALL PSTLNK(K,1)
C
C     Calculate number of compartments which make up runoff
C     pesticide extraction depth, defined by variable PRDPTH.
C
      PRDPTH = 1.0
      KLIN = 0
      DDLN = 0.0
15    CONTINUE
        KLIN = KLIN + 1
        DDLN = DDLN + DELX(KLIN)
      IF (DDLN .LT. PRDPTH) GO TO 15
      CMPT = KLIN
      PFRAC = PRDPTH - (DDLN-DELX(KLIN))
C
C     Set up coefficients for surface layer
C
      A(1)= 0.0
      B(1)= ((DISP(K,1)*THETN(1)+KH(K,1)*DGAIR(1))/(DELX(1)*
     1       DELX(1))
     2       +(DWRATE(K,1)*THETN(1))+(DSRATE(K,1)*KD(K,1)*BD(1))
     3       +(DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))
     4       +(DGRATE(K,1)*THAIR(1)*KH(K,1))
     5       +ELTERM(K)) *DELT
     6       +THETN(1) + KD(K,1)*BD(1) + THAIR(1)*KH(K,1)
     7       +CONDUC*KH(K,1)*DELT/DELX(1)
     8       +(RTR(1,1)+RTR(2,1)+RTR(3,1))*THETN(1)*DELT
C
      IF (CMPT .EQ. 1) THEN
        B(1) = B(1) + (RUNOF/DELX(1))
      ELSE
        B(1) = B(1) + (RUNOF/PRDPTH)  
      ENDIF
C
      C(1)= -(DISP(K,2)*THETN(2)+KH(K,1)*DGAIR(2))*DELT/(DELX(1)*
     1       0.5*(DELX(1)+DELX(2)))
      F(1)= (THETO(1)+KD(K,1)*BD(1)+(THETAS(1)-THETO(1))*OKH(K,1))
     1      *SPTEMP(K,1)+WTERM(K)*DELT/DELX(1)
     2      +(SRC(1,1)+SRC(2,1)+SRC(3,1))*DELT
C
C     Calculate coefficient of non-boundary soil layers
C
      DO 20 I=2,NCOM2M
        CALL PSTLNK(K,I)
        A(I)= (-(DISP(K,I-1)*THETN(I-1)+KH(K,I-1)*DGAIR(I-1))
     1          /(DELX(I)*0.5*(DELX(I-1)+DELX(I)))
     2          )*DELT
        B(I)= ((DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))
     1          /(DELX(I)*0.5*(DELX(I-1)+DELX(I)))
     1        + (DISP(K,I)*THETN(I)+KH(K,I)*DGAIR(I))
     1          /(DELX(I)*0.5*(DELX(I)+DELX(I+1)))
     2          +(DWRATE(K,I)*THETN(I))+(DSRATE(K,I)*KD(K,I)*BD(I))
     3          +(DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))
     1          +(DGRATE(K,I)*THAIR(I)*KH(K,I))
     4          +GAMMA(I)*ET(I)*THETN(I)/SW(I)) *DELT
     5          +THETN(I)+KD(K,I)*BD(I)+THAIR(I)*KH(K,I)
     6          +(RTR(1,I)+RTR(2,I)+RTR(3,I))*THETN(I)*DELT
C
C       Add runoff term if current compartment number is less than or
C       equal to the number of compartments which make up 1 cm depth.
C
        IF (I .LT. CMPT) THEN
          B(I) = B(I) + (RUNOF/PRDPTH) 
        ELSE  
          IF (I .EQ. CMPT) B(I) = B(I) + (RUNOF/DELX(I))*(PFRAC/PRDPTH)
        ENDIF
C
        C(I)= -(DISP(K,I+1)*THETN(I+1)+KH(K,I+1)*DGAIR(I+1))
     1         *DELT/(DELX(I)*0.5*(DELX(I)+DELX(I+1)))
        F(I)= (THETO(I)+KD(K,I)*BD(I)+(THETAS(I)-THETO(I))*OKH(K,I))
     1        *SPTEMP(K,I)
     2        +(SRC(1,I)+SRC(2,I)+SRC(3,I))*DELT
20    CONTINUE
C
C     Calculate coefficients of bottom layer
C
      CALL PSTLNK(K,NCOM2)
      A(NCOM2)=(-(DISP(K,NCOM2M)*THETN(NCOM2M)+KH(K,NCOM2M)
     1         *DGAIR(NCOM2M))/(DELX(NCOM2)*0.5*
     2         (DELX(NCOM2M)+DELX(NCOM2))))*DELT
      B(NCOM2)= ((DISP(K,NCOM2)*THETN(NCOM2)+KH(K,NCOM2)*DGAIR(NCOM2))
     1         /(DELX(NCOM2)*DELX(NCOM2))
     2         +(DWRATE(K,NCOM2)*THETN(NCOM2))
     3         +(DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))
     1         +(DSRATE(K,NCOM2)
     4         *KD(K,NCOM2)*BD(NCOM2))+DGRATE(K,NCOM2)*THAIR(NCOM2)
     5         *KH(K,NCOM2))*DELT+THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)
     6         +THAIR(NCOM2)*KH(K,NCOM2)+(RTR(1,NCOM2)+RTR(2,NCOM2)
     7         +RTR(3,NCOM2))*THETN(NCOM2)*DELT
      C(NCOM2)= 0.0
      F(NCOM2)= (THETO(NCOM2)+KD(K,NCOM2)*BD(NCOM2)+(THETAS(NCOM2)-
     1          THETO(NCOM2))*KH(K,NCOM2))*SPTEMP(K,NCOM2)
     2          +(SRC(1,NCOM2)+SRC(2,NCOM2)+SRC(3,NCOM2))*DELT
C
C     call solve equation
C
      CALL TRDIAG (A,B,C,X,F,NCOM2,LPRZOT,MODID)
C
C     Calculate pesticide fluxes
C
      PVFLUX(K,1)= -CONDUC*X(1)*KH(K,1)
      UPFLUX(K,1)= 0.0
      DFFLUX(K,1)= DISP(K,1)/(0.5*(DELX(1)+DELX(2)))*X(1)*THETN(1)
     1            -DISP(K,2)/(0.5*(DELX(1)+DELX(2)))*X(2)*THETN(2)
      DKFLUX(K,1)= DELX(1)*X(1)*(DWRATE(K,1)*THETN(1)+DSRATE(K,1)
     1            *BD(1)*KD(K,1)+DGRATE(K,1)*THAIR(1)*KH(K,1)
     2            +DKBIO(K,1)*(THETN(1)+KD(K,1)*BD(1)))
      WOFLUX(K)  = WTERM(K)
      IF (CMPT .EQ. 1) THEN
        RFFLUX(K,1) =RUNOF*X(1)
      ELSE
        RFFLUX(K,1) =RUNOF*X(1)*(DELX(1)/PRDPTH)
      ENDIF
      ERFLUX(K)  = ELTERM(K)*DELX(1)*X(1)
      TRFLUX(K,1)= (RTR(1,1)+RTR(2,1)+RTR(3,1))*X(1)*THETN(1)*DELX(1)
      SRCFLX(K,1)= (SRC(1,1)+SRC(2,1)+SRC(3,1))*DELX(1)
C
      DO 30 I=2,NCOM2M
        RFFLUX(K,I) = 0.0
        IF (I .LT. CMPT) THEN
          RFFLUX(K,I)=RUNOF*X(I)*(DELX(I)/PRDPTH)
        ELSE
          IF (I .EQ. CMPT) RFFLUX(K,I)=RUNOF*X(I)*(PFRAC/PRDPTH)
        ENDIF
        PVFLUX(K,I)=DGAIR(I)*KH(K,I)/(0.5*(DELX(I)+DELX(I+1)))*X(I)-
     1         DGAIR(I+1)*KH(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*X(I+1)
        DFFLUX(K,I)=DISP(K,I)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I)*X(I)-
     1         DISP(K,I+1)/(0.5*(DELX(I)+DELX(I+1)))*THETN(I+1)*X(I+1)
        DKFLUX(K,I)=DELX(I)*X(I)*(DWRATE(K,I)*THETN(I)+DSRATE(K,I)
     1            *BD(I)*KD(K,I)+DGRATE(K,I)*THAIR(I)*KH(K,I)
     2            +DKBIO(K,I)*(THETN(I)+KD(K,I)*BD(I)))
        UPFLUX(K,I)=GAMMA(I)*ET(I)*X(I)
        TRFLUX(K,I)=(RTR(1,I)+RTR(2,I)+RTR(3,I))*X(I)*THETN(I)*DELX(I)
        SRCFLX(K,I)=(SRC(1,I)+SRC(2,I)+SRC(3,I))*DELX(I)
30    CONTINUE
C
      RZFLUX(K)=DISP(K,NCOMRZ)/(0.5*(DELX(NCOMRZ)+DELX(NCOMRZ+1)))*
     1        THETN(NCOMRZ)*X(NCOMRZ)
     1  -DISP(K,NCOMRZ+1)/(0.5*(DELX(NCOMRZ+1)+DELX(NCOMRZ)))*
     1  THETN(NCOMRZ+1)*X(NCOMRZ+1)
     2  +(VEL(NCOMRZ)*X(NCOMRZ)*THETN(NCOMRZ))
      RFFLUX(K,NCOM2)=0.
      DFFLUX(K,NCOM2)=0.
      PVFLUX(K,NCOM2)=0.
      UPFLUX(K,NCOM2)=0.
      DKFLUX(K,NCOM2)=DELX(NCOM2)*X(NCOM2)*(DWRATE(K,NCOM2)*THETN(NCOM2)
     1           +DSRATE(K,NCOM2)*BD(NCOM2)*KD(K,NCOM2)
     2           +DGRATE(K,NCOM2)*THAIR(NCOM2)*KH(K,NCOM2)
     3           +DKBIO(K,NCOM2)*(THETN(NCOM2)+KD(K,NCOM2)*BD(NCOM2)))
      TRFLUX(K,NCOM2)=(RTR(1,NCOM2)+RTR(2,NCOM2)+RTR(3,NCOM2))
     1                *X(NCOM2)*THETN(NCOM2)*DELX(NCOM2)
      SRCFLX(K,NCOM2)=(SRC(1,NCOM2)+SRC(2,NCOM2)+SRC(3,NCOM2))
     1                *DELX(NCOM2)
C
C     Calculate core flux values.
C     Multiply internal units of GR/CM**2 by 10**5 so output
C     is expressed in units of KG/HA (as in the input).
C
      DCOFLX(K) = DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
     1          - DISP(K,NCOM2)/DELX(NCOM2)*THETN(NCOM2)*X(NCOM2)
     2          + (VEL(NCOM2)*X(NCOM2)*THETN(NCOM2))
      DCOFLX(K) = DCOFLX(K) * 1.0E5
      MCOFLX(K) = MCOFLX(K) + DCOFLX(K)
      YCOFLX(K) = YCOFLX(K) + DCOFLX(K)
C
C     Accumulate fluxes from soil layers for output
C
      ROFLUX(K)= 0.0
      SUPFLX(K)= 0.0
      SDKFLX(K)= 0.0
      TTRFLX(K)= 0.0
      TSRCFX(K)= 0.0
C
      DO 40 I=1,NCOM2
         ROFLUX(K)= ROFLUX(K)+RFFLUX(K,I)
         SDKFLX(K)= SDKFLX(K)+DKFLUX(K,I)
         SUPFLX(K)= SUPFLX(K)+UPFLUX(K,I)
         TTRFLX(K)= TTRFLX(K)+TRFLUX(K,I)
         TSRCFX(K)= TSRCFX(K)+SRCFLX(K,I)
40    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE MOC(K)
C
C     + + + PURPOSE + + +
C
C     This subroutine solves the advection component of the soil
C     pesticide transport process.  SLPST1 is called next and uses
C     the advected concentrations to calculate the transport of
C     pesticide due to all other processes.  This algorithm is based
C     on one presented in Khaleel and Reddell (1986) in Groundwater
C     modified for use under variably saturated conditions, with
C     retardation and decay. To acomodate these changes the algorithm
C     operates on total pesticide mass in each call rather than conc.
C
C     This subroutine was added by Y.Meeks 4/87 to minimize numerical
C     dispersion in the solution of the transport equation.  To include
C     this subroutine, velocity calculations were deleted from SLPEST,
C     a call MOC1 toggle was added to the main program, and
C     initializations were added near the end of the subroutine
C     INITL.  See specific subroutines for more details.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER  K
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     K   - chemical number being simulated (1-3)
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + + 
C
      REAL         CHANGE(NCMPTS),SUMC(NCMPTS),COUNT(NCMPTS),ZIN(250),
     1             TEND,TLEFT,MASS(NCMPTS),RVEL(NCMPTS),PTEMP(NCMPTS),
     2             NET(NCMPTS),DDLN,ZTOT,CTOT,ZCTOT,ZOLD,ZOLD1
      INTEGER      J,I,M,LL,NCELL,IOUT,NEW,ILIN,RTIOM1,IDEL,ICHECK,
     1             IFACE,FIRST,ISTART,IEND,MM,M3,NUMSUM
      CHARACTER*80 MESAGE
      INTEGER      IERROR
      LOGICAL      FATAL
      REAL         CCSUM
C
C     +  +  + INTRINSICS +  +  + 
C      
      INTRINSIC ABS,AMIN1,AMAX1,MIN0
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,ERRCHK,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
C
 2090 FORMAT('NPI[',I4,'] + NEW[',I4,'] is greater than NPII[',I4,
     1       '] in subroutine MOC')
C
C     +  +  + END SPECIFICATIONS +  +  + 
C
      MESAGE = 'MOC'
      CALL SUBIN(MESAGE)
C
C       Convert dissolved concentration to total mass in each cell
C
        DO 10  J=1,NCOM2
          PESTR(K,J)=SPESTR(K,J)*(THETO(J)+KD(K,J)*BD(J)+(THETAS(J)-
     1             THETO(J))*OKH(K,J))/THETO(J)
          MASS(J)=PESTR(K,J)*DELX(J)*THETO(J)
10      CONTINUE
C
C       Calculate the change in mass in each cell and increment each
C       point in the cell by the correct amount
C
        DO 20 J=1,NCOM2
          CHANGE(J)= MASS(J)-MASSO(K,J)
20      CONTINUE
C
C       Find out how many points are currently in each cell
C
        DO 30 J=1,NCOM2
          COUNT(J)=0.
          SUMC(J)=0.
30      CONTINUE
C
        DO 40 M=1,NPI(K)
          ILIN = 0
          DDLN = 0.0
33        ILIN = ILIN + 1
            DDLN = DDLN + DELX(ILIN)
            IF (Z(K,M) .GT. DDLN) GO TO 33
            NCELL = ILIN
          COUNT (NCELL) = COUNT(NCELL) + 1.0
40      CONTINUE
C
C       Increment the mass at each point. A positive change is added
C       equally to all points in the cell, and a negative vhange is
C       weighted by the actual mass at the point to avoid negative
C       mass values.
C
        DO 50 M=1,NPI(K)
          ILIN = 0
          DDLN = 0.0
43        ILIN = ILIN + 1
            DDLN = DDLN + DELX(ILIN)
            IF (Z(K,M) .GT. DDLN) GO TO 43
            NCELL = ILIN
          IF (ABS(COUNT(NCELL)-0.0).LT.1.0E-5)  COUNT(NCELL)=1.0
          IF (CHANGE(NCELL).GE.0.0) THEN
            CC(K,M) = CC(K,M)  + CHANGE(NCELL)/COUNT(NCELL)
          ELSE
            IF (MASSO(K,NCELL).LE.0) GO TO 50
            CC(K,M) = CC(K,M)+(CHANGE(NCELL))*(CC(K,M)/MASSO(K,NCELL))
          END IF
50      CONTINUE
C
C
C       Each point is advected to a new location according to the
C       local velocity. This is done in several steps for clarity.
C
C       Convert to retarded velocity
C
        DO 60 J=1,NCOM2
          RVEL(J)=VEL(J)/(1.0+KD(K,J)*BD(J)/THETO(J))
          IF(RVEL(J).GE.30.0) MOCFLG=1
60      CONTINUE
C
C       Move the points to their new locations.  If a point moves out
C       of one cell it will begin to move at the new local velocity.
C       if the velocity in a cell is zero, points do not move there.
C
        DO 70 M=1,NPI(K)
          ILIN = 0
          DDLN = 0.0
63        ILIN = ILIN + 1
            DDLN = DDLN + DELX(ILIN)
            IF (Z(K,M) .GT. DDLN) GO TO 63
            NCELL = ILIN
          IF(ABS(RVEL(NCELL)-0.0).LT.1.0E-5) GO TO 70
          TEND  = (ZC(NCELL) + DELX(NCELL)/2 - Z(K,M))/RVEL(NCELL)
          IF (TEND .GT. DELT) THEN
            ZOLD = Z(K,M)
            Z(K,M) = Z(K,M) + DELT*RVEL(NCELL)
            DO 75 IFACE=1,ICROSS(K)
              IF (Z(K,M).GE.TOP(K,IFACE).AND.ZOLD.LT.TOP(K,IFACE))
     1          PCOUNT(K,IFACE) = PCOUNT(K,IFACE) + 1
75          CONTINUE
          ELSE
            ZOLD = Z(K,M)
            Z(K,M) = ZC(NCELL) + DELX(NCELL)/2
            DO 76 IFACE=1,ICROSS(K)
              IF (Z(K,M).GE.TOP(K,IFACE).AND.ZOLD.LT.TOP(K,IFACE))
     1          PCOUNT(K,IFACE) = PCOUNT(K,IFACE) + 1
76          CONTINUE
            TLEFT = DELT - TEND
            DO 80 LL=1,40
              IF (TLEFT.GT.0.0) THEN
               IF ((NCELL+LL).GT.NCOM2) THEN
                 RVEL(NCELL+LL)=RVEL(NCOM2)
                 DELX(NCELL+LL)=DELX(NCOM2)
               ENDIF
               ZOLD1= Z(K,M)
               Z(K,M)=Z(K,M)+AMIN1(DELX(NCELL+LL),RVEL(NCELL+LL)*TLEFT)
               DO 85 IFACE=1,ICROSS(K)
C
C   The following if statements and the local variable ZOLD1 above
C   were introduced by ssh at aqua terra so that PCOUNT is incremented
C   for only the last point consolidation boundary that is crossed in
C   one time step
C
                 IF (IFACE.EQ.1) THEN
                   IF (Z(K,M).GE.TOP(K,IFACE).AND.ZOLD1.LT.TOP(K,IFACE))
     1               PCOUNT(K,IFACE) = PCOUNT(K,IFACE) + 1
                 ELSE
                   IF (Z(K,M).GE.TOP(K,IFACE).AND.ZOLD1.LT.
     1             TOP(K,IFACE))THEN
                     PCOUNT(K,IFACE) = PCOUNT(K,IFACE) + 1
                     IF (ZOLD.LT.TOP(K,IFACE-1))
     1                 PCOUNT(K,IFACE-1) = PCOUNT(K,IFACE-1) - 1
                   ENDIF
                 ENDIF     
85             CONTINUE
               IF (ABS(RVEL(NCELL+LL)-0.0).LT.1.0E-5) THEN
                 TLEFT=0.0
               ELSE
                 TLEFT=AMAX1(TLEFT-DELX(NCELL+LL)/RVEL(NCELL+LL),0.0)
               END IF
              ELSE
                GO TO 70
              END IF
80          CONTINUE
          END IF
70      CONTINUE
C
C    The array indices are readjusted to keep track of points which
C    flow into and out of the soil column.  The advective flux out of
C    column is calculated as the sum of the mass carried by the
C    points which move out of the bottom.
C
        ADFLUX(K,NCOM2)=0.0
        CCSUM = 0.0
        IOUT=NPI(K) + 1
        DO 90 M=1,NPI(K)
          IF(Z(K,M).GT.CORED) THEN
C
C           IOUT is the lowest valued index no. of points below the core
C
            IOUT = MIN0(M,IOUT)
            CCSUM = CCSUM + CC(K,M)
          END IF
90      CONTINUE
        CC(K,IOUT) = CCSUM
C
C       Following code skips calculation of adflux if SAFTMOD is on but
C       VADOFT is not ???
C
        ADFLUX(K,NCOM2) = CCSUM / DELT
        NPI(K) = IOUT - 1
C
        NEW =  0
        DDLN = 0
        ILIN = 1
C
C    The following if statements and the local variable NUMSUM were
C    added by ssh at aqua terra, 1-89, so that only two new points
C    are placed in layers less than or equal to 2 cm thick.  Also, the
C    maximum number of new points was changed from 50 to 150 (see the
C    DO 100 statement) to prevent mass balance errors.
C
        IF(DELX(ILIN).LE.2.0) THEN
          NUM=2
          NUMSUM=2
        ELSE
          NUM=4
          NUMSUM=4
       ENDIF
        DO 100 I=1,150
C         IF (I-1 .EQ. ILIN*NUM) ILIN = ILIN + 1
          IF (I-1 .EQ. NUMSUM) THEN
            ILIN = ILIN + 1
            IF(DELX(ILIN).LE.2.0) THEN
              NUM=2
              NUMSUM=NUMSUM+NUM
            ELSE
              NUM=4
              NUMSUM=NUMSUM+NUM
            ENDIF
          ENDIF
          DDLN = DDLN + DELX(ILIN)/NUM
          IF ((Z(K,1)-DDLN).GT.0.0) THEN
C           ZIN(I) = Z(K,1)-DDLN
            ZIN(I) = DDLN
            NEW=NEW + 1
          ELSE
            GO TO 110
          END IF
100     CONTINUE
110     CONTINUE
C
        IF ((NPI(K)+NEW) .GT. NPII) THEN
          IERROR = 2040
          WRITE(MESAGE,2090) NPI(K), NEW, NPII
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        ENDIF
C
        DO 120 M=NPI(K),1,-1
          Z(K,M+NEW) = Z(K,M)
          CC(K,M+NEW) = CC(K,M)
120     CONTINUE
C
C       The mass to assign to incoming points is based on the
C       concentration of incoming water.
C
        IDEL = 0
        IF (NEW.GT.0) THEN
          DO 130 M=1,NEW
            Z(K,M) = ZIN(M)
            CC(K,M)  = CNCPND*ZIN(1)/NEW
130       CONTINUE
          NPI(K) = NPI(K) + NEW
C
C         Consolidate points if necassary
C
          DO 190 M=1,ICROSS(K)
230         CONTINUE
              IF (PCOUNT(K,M) .GE. RATIO(K,M)) THEN
                IDEL = IDEL + RATIO(K,M) - 1
              DO 195 MM=1, NPI(K)
C
C               Find index of the first point under the interface
C
                IF (Z(K,MM) .GE. TOP(K,M)) THEN
                  FIRST = MM
                  GO TO 200
                ENDIF
195           CONTINUE
200         CONTINUE
C
C           Average points that need to be consolidated
C           distance is mass averaged, mass is summed
C
            ISTART = FIRST + PCOUNT(K,M) - RATIO(K,M)
            IEND   = FIRST + PCOUNT(K,M) - 1
            ZTOT   = 0.
            CTOT   = 0.
            ZCTOT  = 0.
            DO 210 M3=ISTART, IEND
              ZCTOT = ZCTOT + Z(K,M3)*CC(K,M3)
              CTOT  = CTOT + CC(K,M3)
              ZTOT  = ZTOT + Z(K,M3)
210         CONTINUE
            IF (CTOT.LE.1.0E-30) THEN
              Z(K,ISTART)= ZTOT/(IEND - ISTART + 1.)
            ELSE
              Z(K,ISTART) = ZCTOT/CTOT
            END IF
            CC(K,ISTART) = CTOT
C
C           Shift Z and CC arrays
C
            RTIOM1 = RATIO(K,M) - 1
            DO 220 M3=ISTART+1, NPI(K)-RTIOM1
              Z(K,M3) = Z(K,M3+RTIOM1)
              CC(K,M3) = CC(K,M3+RTIOM1)
220         CONTINUE
            PCOUNT(K,M) = PCOUNT(K,M) - RATIO(K,M)
            ICHECK = 1
          ELSE
            ICHECK = 0
C
C    The following line was added by ssh at aqua terra, 1-89, so
C    that the point consolidation counter is reset to zero after
C    each time step.
C
            PCOUNT(K,M) = 0
          ENDIF
          IF (ICHECK .EQ. 1) GO TO 230
190       CONTINUE
        END IF
        NPI(K) = NPI(K) - IDEL
C
C       Calculate the total mass and convert to dissolved concentration
C       in each cell.
C
        DO 140 M=1,NPI(K)
          ILIN = 0
          DDLN = 0.0
133       ILIN = ILIN + 1
            DDLN = DDLN + DELX(ILIN)
            IF (Z(K,M) .GT. DDLN) GO TO 133
            NCELL = ILIN
          SUMC (NCELL) =  SUMC(NCELL) + CC(K,M)
140     CONTINUE
C
        DO 150 J=1,NCOM2
          MASS(J)=SUMC(J)
          IF (ABS(THETO(J)-0.0).LT.1.0E-5) THEN
            PTEMP(J)=PESTR(K,J)
          ELSE
            PTEMP(J)=MASS(J)/THETO(J)/DELX(J)
            SPTEMP(K,J)=PTEMP(J)*THETO(J)/(THETO(J)+KD(K,J)*BD(J)+
     1                (THETAS(J)-THETO(J))*OKH(K,J))
          END IF
150     CONTINUE
C
C    Calculate the flux advected out of interior cells.  This is the
C    net flux in the next cell down (difference before and after the
C    advection step) minus the flux out of that lower cell.  This
C    difference gives the flux into the underlying cell which is
C    equal to the advective flux out of the cell of interest.
C
        DO 160 J=1,NCOM2
          NET(J)=(PESTR(K,J)-PTEMP(J))*DELX(J)*THETO(J)/DELT
160     CONTINUE
C
        DO 170 J=NCOM2-1,1,-1
          ADFLUX(K,J)=ADFLUX(K,J+1)-NET(J+1)
170     CONTINUE
C
C
C      Store the calculated concentrations
C
       DO 180 J=1,NCOM2
         MASSO(K,J)=MASS(J)
180    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE TRDIAG (A,B,C,X,F,N,LPRZOT,MODID)
C
C     + + + PURPOSE + + +
C
C     Solves a system of equations with a tridiagonal coefficient matrix
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +            
C
      INCLUDE 'PPARM.INC'
      REAL*8      A(NCMPTS),B(NCMPTS),C(NCMPTS),F(NCMPTS),
     1            X(NCMPTS)
      INTEGER     N,LPRZOT
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     A      - 1ST DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     B      - 2ND DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     C      - 3RD DIAGONAL VECTOR OF THE TRIDIAGONAL MATRIX
C     F      - RIGHT HAND SIDE VECTOR
C     X      - WORKING ARRRAY (SCRATCH ARRAY)
C     N      - TOTAL NUMBER OF COMPARTMENTS
C     LPRZOT - FILE OUTPUT UNIT NUMBER
C     MODID  - MODEL ID
C
C     + + + PARAMETERS + + +                
C
C      INCLUDE 'PPARM.INC'
C
C     + + + LOCAL VARIABLES + + +             
C
      REAL*8       U(NCMPTS),Y(NCMPTS),L(NCMPTS)
      INTEGER      I,NM1,IERROR
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     + + + FUNCTIONS + + +
C
      REAL*8       RELTST
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC ABS
C
C     + + + EXTERNALS + + + 
C
      EXTERNAL SUBIN,ERRCHK,SUBOUT,RELTST
C
C     + + + DATA INITIALIZATIONS + + +                
C
      DATA U,Y,L / NCMPTS*0.D0, NCMPTS*0.D0, NCMPTS*0.D0 /
C
C     + + + OUTPUT FORMATS + + +
C
2000  FORMAT (1X,A3,1X,110(1H*),/,1X,A3,1X,110(1H*),/,1X,A3,/,1X,A3,50X,
     1        'E R R O R',/,1X,A3,/,1X,A3,10X,'TRIDIAGONAL MATRIX IN ',
     2        'SUBROUTINE TRDIAG COULD NOT BE SOLVED ON THIS DAY. PRZM',
     3        ' WILL',/,1X,A3,10X,' USE VALUES FOR THE LAST TIME STEP ',
     4        'AND CONTINUE ON. YOU MAY WANT TO STOP AND CHECK',/,1X,A3,
     5       10X,'BOTH THE INPUT SEQUENCE AND THE CODE IN SLPST0, SLPS',
     6        'T1, OR TRDIA1',/,1X,A3,1X,A3,/,1X,A3,/,1X,A3,1X,110(1H*))
 2100 FORMAT('Solution for tridiagonal matrix not found, previous ',
     1       'days values used')
C
C     + + + END SPECIFICATIONS + + +            
C
      MESAGE = 'TRDIAG'
      CALL SUBIN(MESAGE)
C
C     Factor matrix into upper and lower halves
C
      U(1)=B(1)
      DO 10 I=2,N
        IF(ABS(U(I-1)).LT.1.0E-5) GO TO 40
        L(I)=A(I)/U(I-1)
        U(I)=B(I)-L(I)*C(I-1)
10    CONTINUE
C
C     Solve LUX=F
C
      Y(1)=F(1)
      DO 20 I=2,N
         Y(I)=F(I)-L(I)*Y(I-1)
         Y(I)=RELTST(Y(I))
20    CONTINUE
      IF(ABS(U(N)).LT.1.0E-5) GO TO 40
         X(N)=Y(N)/U(N)
         X(N)=RELTST(X(N))
         NM1=N-1
         DO 30 I=1,NM1
            X(N-I)=(Y(N-I)-C(N-I)*X(N+1-I))/U(N-I)
            X(N-I)=RELTST(X(N-I))
30       CONTINUE
         GO TO 800
40    CONTINUE
      IERROR = 2050
      WRITE(MESAGE,2100)
      FATAL  = .FALSE.
      CALL ERRCHK(IERROR,MESAGE,FATAL)
      WRITE(LPRZOT,2000) (MODID,I=1,11)
C
 800  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE MASBAL (APDEP,K,IPRZM)
C
C     + + + PURPOSE + + +
C
C     Calculates mass balance error terms for both hydrology
C     and pesticide transport.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +         
C
      INTEGER   K, IPRZM
      REAL      APDEP
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXZON.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CGLBPZ.INC'
      INCLUDE 'CPTAP.INC'
      INCLUDE 'CSPTIC.INC'
C
C     + + + LOCAL VAIRIABLES + + +              
C
      INTEGER      I
      REAL         XP(NCMPTS),XPB(NCMPTS),TERM1,TERM2
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + INTRINSICS + + +
C
      INTRINSIC FLOAT
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'MASBAL'
      CALL SUBIN(MESAGE)
C
        WINPZ(IPRZM)  = 0.0
        WDSTPZ(IPRZM) = 0.0
        DO 5 I = 1,K
          PDKPZ(IPRZM,K)  = 0.0
          PINPZ(IPRZM,K)  = 0.0
          PDSTPZ(IPRZM,K) = 0.0
          PTRPZ(IPRZM,K)  = 0.0
          PSRCFX(IPRZM,K) = 0.0
5       CONTINUE
        IF (K .GT. 1) GO TO 15
C
C       First pest, need a water balance
C
        WBAL= 0.0
        WBAL= PRECIP+SNOWFL+APDEP-RUNOF-AINF(NCOM2+1)+OSNOW-SNOW
     1        +CINTB-CINT-CEVAP
C
C       Store intermediate results for global mass balance
C
        WINPZ(IPRZM) = WBAL + AINF(NCOM2+1) + WINPZ(IPRZM)
C
        DO 10 I=1,NCOM2
          WBAL= WBAL+LINF(I)-ET(I)-OUTFLO(I)+(THETO(I)-THETN(I))*DELX(I)
C
C         store intermediate results for global mass balance
C
          WINPZ(IPRZM) = WINPZ(IPRZM)-ET(I)-OUTFLO(I)
          WDSTPZ(IPRZM) = WDSTPZ(IPRZM) + (THETN(I)-THETO(I))*DELX(I)
10      CONTINUE
        CWBAL= CWBAL+WBAL
15    CONTINUE
C
C     Pest balance
C
      PBAL(K)= 0.0
      PBAL(K)= FOLP0(K)-FOLPST(K)-FPDLOS(K)-FPVLOS(K)
      PBAL(K)= PBAL(K)-ROFLUX(K)-ERFLUX(K)-ADFLUX(K,NCOM2)
     1         -SDKFLX(K)-SUPFLX(K)
C**** ADD LATERAL OUTFLOW PESTICIDE FLUX ************************
     +         -LATFLX(K)
      PINPZ(IPRZM,K) = PINPZ(IPRZM,K) + WTERM(K) -
     1               ROFLUX(K) - ERFLUX(K) - SUPFLX(K) +
     2               PVFLUX(K,1)
C
      DO 20 I=1,NCOM2
        XP(I)= X(I)*(THETN(I)+KD(K,I)*BD(I)+(THETAS(I)-THETN(I))
     1         *KH(K,I))
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Date: Wednesday, 17 June 1992.  Time: 10:41:10.
        TERM1=PESTR(K,I)*THETO(I)
        TERM2=TERM1-XP(I)
        XPB(I)=TERM2*DELX(I)/FLOAT(DELT)+SRCFLX(K,I)-TRFLUX(K,I)
C        XPB(I)=(PESTR(K,I)*THETO(I)-XP(I))*DELX(I)/DELT+SRCFLX(K,I)
C     1         -TRFLUX(K,I)
        PBAL(K)=PBAL(K)+XPB(I)
C
        PDSTPZ(IPRZM,K) = PDSTPZ(IPRZM,K)-TERM2*DELX(I)/DELT
C        PDSTPZ(IPRZM,K) = PDSTPZ(IPRZM,K)+(XP(I)- PESTR(K,I)*THETO(I))*
C     1                  DELX(I)/DELT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PTRPZ(IPRZM,K) = PTRPZ(IPRZM,K) + TRFLUX(K,I)
        PSRCFX(IPRZM,K) = PSRCFX(IPRZM,K) + SRCFLX(K,I)
        PINPZ(IPRZM,K) = PINPZ(IPRZM,K) + SRCFLX(K,I)
20    CONTINUE
      PDKPZ(IPRZM,K) = PDKPZ(IPRZM,K) + SDKFLX(K)
      PBAL(K) =PBAL(K) + PVFLUX(K,1)
      CPBAL(K)=CPBAL(K)+PBAL(K)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE PSTLNK(K,J)
C
C     + + + PURPOSE + + +
C
C     Provides linkage for transformation and source terms for
C     parent/daughter relationships
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      INTEGER   K,J
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     K   - chemical number
C     J   - daughter number
C
C     + + + PARAMETERS + + +            
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CPEST.INC'
      INCLUDE 'CHYDR.INC'
C
C     + + + LOCAL VARIABLES + + +            
C
      INTEGER  KK
C
C     + + + END SPECIFICATIONS + + +
C
      RTR(1,J) = 0.0
      DO 10 KK = 1, NCHEM
        SRC(KK,J) = 0.0
10    CONTINUE
      IF (K.NE.1) GO TO 15
        RTR(2,J) = DKRT12(J)
        RTR(3,J) = DKRT13(J)
        GO TO 20
15    CONTINUE
      IF (K.NE.2) GO TO 16
        RTR(2,J) = 0.0
        RTR(3,J) = DKRT23(J)
        SRC(1,J) = DKRT12(J) * SPESTR(1,J) * THETN(J)
        GO TO 20
16    CONTINUE
      RTR(2,J) = 0.0
      RTR(3,J) = 0.0
      SRC(1,J) = DKRT13(J) * SPESTR(1,J) * THETN(J)
      SRC(2,J) = DKRT23(J) * SPESTR(2,J) * THETN(J)
C
20    CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRAP
     I                   (FECHO)
C
C     + + + PURPOSE + + +
C     Computes amount and location of nitrogen application.
C     Further modified by PV @ AQUA TERRA Consultants 9/93 to hardwire
C     the calculation of pesticide depth in runoff to 1 cm.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FECHO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FECHO  - Fortran unit number for echo file
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CNITR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CHYDR.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,CMPT
      REAL         DEP,APPDEP
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
      INTRINSIC REAL,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL SUBIN,SUBOUT,PZDSPL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' Surface compartment > 1 cm: Nitrogen Distributed',
     $       ' to 1st compartment depth')
C
C     + + + END SPECIFICATIONS + + +                   
C
      MESAGE = 'NITRAP'
      CALL SUBIN(MESAGE)
C
      IF (DEPI(1,NAPPC) .GT. DELX(1)) THEN
C       the nitrogen is incorporated beyond the surface compartment
        IF (DEPI(1,NAPPC) .LT. 1.0) THEN
C         distribute the nitrogen to 1 cm depth
          APPDEP = 1.0
        ELSE
C         distribute the nitrogen down to the depth of incorporation
          APPDEP = DEPI(1,NAPPC)
        END IF
      ELSE
C       Hard-wiring the nitrogen distribution to 1 cm depth when-
C       ever it is surface applied.  Distribute the nitrogen in
C       either the first compartment thickness or 1 cm.
        IF (DELX(1) .GE. 1.0) THEN
C         nitrogen will be distributed through the first compartment
          APPDEP = 0.0
          IF (DELX(1) .GT. 1.0) THEN
C           warn user about distributing further than specified in input
            WRITE(MESAGE,2000)
            CALL PZDSPL(FECHO,MESAGE)
          END IF
        ELSE
C         nitrogen will be distributed in the 1 cm depth
          APPDEP = 1.0
        END IF
      END IF
C
C     determine how many compartments to distribute nitrogen in
      CMPT= 0
      DEP = 0.0
 10   CONTINUE
        CMPT= CMPT + 1
        DEP = DEP + DELX(CMPT)
      IF (DEP .LT. APPDEP) GO TO 10
C
      DO 20 I = 1,CMPT
C       add nitrogen application to appropriate compartments
        SOILAP(1,I) = TAPP(1,NAPPC)/CMPT
        SOILAP(2,I) = TAPP(2,NAPPC)/CMPT
        SOILAP(3,I) = TAPP(3,NAPPC)/CMPT
        NIT(1,I) = NIT(1,I) + SOILAP(3,I) * (1.0-NAPFRC(NAPPC))
        NIT(2,I) = NIT(2,I) + SOILAP(1,I)
        NIT(4,I) = NIT(4,I) + SOILAP(2,I)
        NIT(7,I) = NIT(7,I) + SOILAP(3,I) * NAPFRC(NAPPC)
 20   CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITBAL
     I                   (APDEP,IPRZM)
C
C     + + + PURPOSE + + +
C
C     Calculates mass balance error terms for both hydrology
C     and nitrogen transport.
C     Modification date: 9/26/95 PRH
C
C     + + + DUMMY ARGUMENTS + + +         
      INTEGER   IPRZM
      REAL      APDEP
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CNITR.INC'
      INCLUDE 'CSPTIC.INC'
C
C     + + + LOCAL VAIRIABLES + + +              
      INTEGER      I
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL SUBIN,SUBOUT
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'NITBAL'
      CALL SUBIN(MESAGE)
C
C     first do water balance
      WBAL= 0.0
      WBAL= PRECIP+SNOWFL+APDEP-RUNOF-AINF(NCOM2+1)+OSNOW-SNOW
     1      +CINTB-CINT-CEVAP
C
      DO 10 I=1,NCOM2
        WBAL= WBAL+LINF(I)-ET(I)-OUTFLO(I)+(THETO(I)-THETN(I))*DELX(I)
10    CONTINUE
      CWBAL= CWBAL+WBAL
C
C     nitrogen balance, start with change in storage
      PBAL(1) = TONIT0 - TOTNIT
C     add input fluxes
      DO 20 I = 1,3
C       three constituent depositions and septic inflows
        PBAL(1) = PBAL(1) + NIADDR(I) + NIADWT(I)
 20   CONTINUE
      DO 30 I = 1,NCOM2
C       three constituent soil applications
        PBAL(1) = PBAL(1) + SOILAP(1,I) + SOILAP(2,I) + SOILAP(3,I)
C       three constituent septic effluent inflows
        PBAL(1) = PBAL(1) + AMMINF(I) + NITINF(I) + ORGINF(I)
        IF (FIXNFG.EQ.1) THEN
C         include nitrogen fixation
          PBAL(1) = PBAL(1) + NCFX12(I,1)
        END IF
C       subtract lateral outflow from each compartment
        PBAL(1) = PBAL(1) - NCFX3(I,1) - NCFX5(I,1) -
     $            NCFX14(I,1) - NCFX16(I,1)
C       denitrification and volatilization
        PBAL(1) = PBAL(1) - NCFX6(I,1) - NCFX18(I,1)
 30   CONTINUE
C     subtract core outflow
      PBAL(1) = PBAL(1) - NCFX2(NCOM2,1) - NCFX4(NCOM2,1) -
     $          NCFX13(NCOM2,1) - NCFX15(NCOM2,1)
C
      CPBAL(1) = CPBAL(1) + PBAL(1)
C
C     update total nitrogen storage
      TONIT0 = TOTNIT
C
      CALL SUBOUT
C
      RETURN
      END
