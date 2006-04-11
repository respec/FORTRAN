      SUBROUTINE INDAT1
C=======================================================================
C     This Subroutine reads and prints control,
C     Conduit and Junction data.
C=======================================================================
C     THIS PROGRAM LAST MODIFIED BY R.E.D., 4/92.
C     MODIFICATION IS TO ALLOW ONLY ONE SET OF C3,34 LINES TO BE
C       USED FOR IRREGULAR CHANNELS IF DESIRED.
C     ADD INITIAL DATE ON LINE B1, WCH, 4/11/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'FLODAT.INC'
      CHARACTER GTYPE(2)*20,CTYPE(8)*10,KDUM(8)*10,IREAD*4
      DATA GTYPE/'  POSITIVE FLAP GATE','  NEGATIVE FLAP GATE'/
      DATA CTYPE/' CIRCULAR ',' RECTANGLE',' HORSESHOE',
     +           ' EGG-SHAPE',' BASKET   ',' TRAPEZOID',
     +           ' POWER FNC',' NATURAL  '/
      DATA IREAD/'CARD'/
C=======================================================================
C                  E X E C U T I O N
C=======================================================================
      NSTOP  = 0
      ISOL   = 0
      JSLOT  = 0
      KSUPER = 0
      NCSAVE = NSCRAT(4)
      IF(NCSAVE.GT.0) REWIND NCSAVE
C=======================================================================
C     READ THE TWO A1 TITLE LINES
C=======================================================================
      READ(N5,*,ERR=888) CC,ALPHA1
      READ(N5,*,ERR=888) CC,ALPHA2
      WRITE(N6,2999)
      WRITE(N6,5060) ALPHA1,ALPHA2
C=======================================================================
C     READ SOLUTION PARAMETERS ON OPTIONAL DATA GROUP B0
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B0') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,ISOL,KSUPER
                     ELSE
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     READ SOLUTION PARAMETERS ON OPTIONAL DATA GROUP BB
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BB') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,JELEV,JDOWN
                     IF(JELEV.LT.0.OR.JELEV.GT.1) JELEV = 0
                     IF(JDOWN.LT.0.OR.JDOWN.GT.2) JDOWN = 0
                     ELSE
                     JELEV = 0
                     JDOWN = 0
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     GENERAL CONTROL PARAMETERS ON DATA GROUPS B1, B2 AND B3
C=======================================================================
C#### WCH, 4/11/94.  ADD OPTIONAL INITIAL DATE TO LINE B1 INPUT.
      IDATZ = 0
      READ(N5,*,ERR=88) CC,NTCYC,DELT,TZERO,NSTART,INTER,JNTER,JREDO,
     1 IDATZ
      GO TO 90
C#######################################################################
C#### WCH, 4/11/94.  ASSUME ERROR IS DUE TO NEW PARAMETER.  READ AGAIN
C     WITHOUT IDATZ SINCE IDATZ IS STRICTLY OPTIONAL. 
C=======================================================================
   88 BACKSPACE N5
      BACKSPACE N5
      READ(N5,*,ERR=888) CC,NTCYC,DELT,TZERO,NSTART,INTER,JNTER,JREDO
C
   90 READ(N5,*,ERR=888) CC,METRIC,NEQUAL,AMEN,ITMAX,SURTOL
      READ(N5,*,ERR=888) CC,NHPRT,NQPRT,NPLT,LPLT,NJSW
C=======================================================================
C     METRIC = 0 --> U.S. CUSTOMARY UNITS
C     METRIC = 1 --> METRIC UNITS
C                    NOTE: METRIC = METRIC + 1  SOON IN FORTRAN CODE.
C                    NOTE: EXTRAN USES CONSISTENT UNITS (U.S. OR METRIC)
C                      INTERNALLY IN CODE DURING COMPUTATIONS.
C     JSLOT  = 0 --> SUM OF JUNCTION FLOW IS ZERO
C     JSLOT  = 1 --> PREISSMAN SLOT
C=======================================================================
      IF(SURTOL.LT.0.0) THEN
                        ISOL   = 1
                        SURTOL = ABS(SURTOL)
                        ENDIF
      IF(ISOL.GE.2)  JSLOT = 1
      METRIC               = METRIC + 1
      RDELT                = DELT
      DELT2                = DELT/2.0
      GRVT                 = 32.2
      IF(METRIC.EQ.2) GRVT = 9.806
      IF(AMEN.EQ.0.0.AND.METRIC.EQ.1) AMEN = 12.566
      IF(AMEN.EQ.0.0.AND.METRIC.EQ.2) AMEN =  1.22
      WRITE(N6,5100)  NTCYC
      WRITE(N6,5120)  DELT,DELT*FLOAT(NTCYC)/3600.0
      IF(NEQUAL.EQ.0) WRITE(N6,5121) NEQUAL
      IF(NEQUAL.GE.1) WRITE(N6,5122) NEQUAL
      IF(METRIC.EQ.1) WRITE(N6,5123) METRIC-1
      IF(METRIC.EQ.2) WRITE(N6,5124) METRIC-1
      IF(NSTART.LE.0) NSTART   = 1
      WRITE(N6,5140)  NSTART,INTER,FLOAT(INTER)*DELT/60.0,JNTER,
     +                FLOAT(JNTER)*DELT/60.0,JREDO
      WRITE(N6,5160)  TZERO
C#### WCH, 4/11/94
      IF(IDATZ.EQ.0) THEN
           IDATZ = 880101
           WRITE(N6,5162) IDATZ
           ELSE
           WRITE(N6,5164) IDATZ
           ENDIF
      WRITE(N6,5170)  ITMAX,SURTOL
      IF(METRIC.EQ.1) WRITE(N6,5175) AMEN
      IF(METRIC.EQ.2) WRITE(N6,5176) AMEN
      IF(ISOL.EQ.0)   WRITE(N6,5177)
      IF(ISOL.EQ.1)   WRITE(N6,5178)
      IF(ISOL.EQ.2)   WRITE(N6,5179)
      IF(KSUPER.EQ.0) WRITE(N6,5183)
      IF(KSUPER.EQ.1) WRITE(N6,5184)
      TZERO  =  3600.0*TZERO
      ITMAX  = IABS(ITMAX)
      IF(NEQUAL.EQ.1) NEQUAL = IFIX(DELT)
      WRITE(N6,5180) NJSW
      IF(JELEV.GT.0) WRITE(N6,5185)
C=======================================================================
C     Read Junction numbers for print and plot data.
C=======================================================================
      IF(NHPRT.GT.0.AND.JCE.EQ.0) THEN
                     READ(N5,*,ERR=888) CC,(JPRT(I),I=1,NHPRT)
                     WRITE(N6,5200)  NHPRT,(JPRT(I),I=1,NHPRT)
                     ENDIF
      IF(NHPRT.GT.0.AND.JCE.EQ.1) THEN
                     READ(N5,*,ERR=888) CC,(AOUT(I,1),I=1,NHPRT)
                     WRITE(N6,5201)  NHPRT,(AOUT(I,1),I=1,NHPRT)
                     ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR DETAILED PRINTOUT
C=======================================================================
      IF(NQPRT.GT.0.AND.JCE.EQ.0) THEN
                     READ(N5,*,ERR=888) CC,(CPRT(I),I=1,NQPRT)
                     WRITE(N6,5220)  NQPRT,(CPRT(I),I=1,NQPRT)
                     ENDIF
      IF(NQPRT.GT.0.AND.JCE.EQ.1) THEN
                     READ(N5,*,ERR=888) CC,(AOUT(I,2),I=1,NQPRT)
                     WRITE(N6,5221)  NQPRT,(AOUT(I,2),I=1,NQPRT)
                     ENDIF
C=======================================================================
C     READ JUNCTION NUMBERS FOR PLOTTING
C=======================================================================
      IF(NPLT.GT.0.AND.JCE.EQ.0) THEN
                    READ(N5,*,ERR=888) CC,(JPLT(N),N=1,NPLT)
                    WRITE(N6,5240)     NPLT,(JPLT(N),N=1,NPLT)
                    ENDIF
      IF(NPLT.GT.0.AND.JCE.EQ.1) THEN
                    READ(N5,*,ERR=888) CC,(AOUT(N,3),N=1,NPLT)
                    WRITE(N6,5241)     NPLT,(AOUT(N,3),N=1,NPLT)
                    ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR PLOTTING
C=======================================================================
      IF(LPLT.GT.0.AND.JCE.EQ.0) THEN
                    READ(N5,*,ERR=888) CC,(KPLT(N),N=1,LPLT)
                    WRITE(N6,5260)     LPLT,(KPLT(N),N=1,LPLT)
                    ENDIF
      IF(LPLT.GT.0.AND.JCE.EQ.1) THEN
                    READ(N5,*,ERR=888) CC,(AOUT(N,4),N=1,LPLT)
                    WRITE(N6,5261)     LPLT,(AOUT(N,4),N=1,LPLT)
                    ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR PLOTTING WATER SURFACE SLOPE
C     DATA WILL BE ON DATA GROUP B8 IF PRESENT.
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B8') THEN
                     BACKSPACE N5
                     IF(JCE.EQ.0) THEN
                     READ(N5,*,ERR=888) CC,NSURF,(JSURF(N),N=1,NSURF)
                     WRITE(N6,5265)        NSURF,(JSURF(N),N=1,NSURF)
                     ENDIF
                     IF(JCE.EQ.1) THEN
                     READ(N5,*,ERR=888) CC,NSURF,(AOUT(N,5),N=1,NSURF)
                     WRITE(N6,5266)        NSURF,(AOUT(N,5),N=1,NSURF)
                     ENDIF
                     ELSE
                     NSURF = 0
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     Read conduit data on data group C1.
C=======================================================================
      WRITE(*,6000)
      NATUR    = 0
      DO 260 N = 1,NEE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'C1') GO TO 280
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NCOND(N),(NJUNC(N,K),K=1,2),
     +                  QO(N),NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                  ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,ACOND(N),(KJUNC(N,K),K=1,2),
     +                  QO(N),NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                  ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N)
      IF(ROUGH(N).LE.0.0) ROUGH(N) = 0.014
C=======================================================================
C     IF LEN(N) IS NEGATIVE A FLAP GATE IS BEING MODELED
C        THIS FLAP GATE ONLY ALLOWS FLOW IN THE POSITIVE DIRECTION
C=======================================================================
      IF(LEN(N).LT.0) THEN
                      INGATE(N) = 1
                      LEN(N)    = ABS(LEN(N))
                      ENDIF
C=======================================================================
C     IF NKLASS(N) IS NEGATIVE A FLAP GATE IS BEING MODELED
C        THIS FLAP GATE ONLY ALLOWS FLOW IN THE NEGATIVE DIRECTION
C=======================================================================
      IF(NKLASS(N).LT.0) THEN
                         INGATE(N) = 2
                         NKLASS(N) = IABS(NKLASS(N))
                         ENDIF
C=======================================================================
C     NKLASS=1 CIRCULAR PIPE
C     NKLASS=2 RECTANGULAR PIPE
C     NKLASS=3 HORSESHOE PIPE
C     NKLASS=4 EGGSHAPED PIPE
C     NKLASS=5 BASKETHANDLE PIPE
C     NKLASS=6 TRAPEZOIDAL CHANNEL
C     NKLASS=7 PARABOLIC CROSS-SECTION OR POWER FUNCTION CROSS SECTION
C     NKLASS=8 IRREGULAR CROSS-SECTION
C     NKLASS=9  ORIFICES (SEE BELOW)
C     NKLASS=10 ORIFICES (SEE BELOW)
C=======================================================================
      KLASS = NKLASS(N)
      IF(KLASS.EQ.1.OR.KLASS.GE.9) THEN
               RFULL(N)  = DEEP(N)/4.0
               AFULL(N)  = (3.1415926/4.0)*DEEP(N)**2
               WIDE(N)   = DEEP(N)
               ENDIF
      IF(KLASS.EQ.2) THEN
               RFULL(N) = (WIDE(N)*DEEP(N))/(2.*WIDE(N)+2.0*DEEP(N))
               AFULL(N) =  WIDE(N)*DEEP(N)
               ENDIF
      IF(KLASS.EQ.3) THEN
               RFULL(N) = 0.25381  * DEEP(N)
               ENDIF
      IF(KLASS.EQ.4) THEN
               RFULL(N) = 0.19311 * DEEP(N)
               ENDIF
      IF(KLASS.EQ.5) THEN
               RFULL(N) = 0.28800*DEEP(N)
               ENDIF
      IF(KLASS.EQ.6) THEN
               AFULL(N)=DEEP(N)*(WIDE(N)+DEEP(N)/2.*(STHETA(N)+SPHI(N)))
               IF(WIDE(N).LE.0.0) WIDE(N) = 0.01
               RFULL(N)=AFULL(N)/(WIDE(N)+DEEP(N)*
     +                  (SQRT(1.0+STHETA(N)**2)+SQRT(1.0+SPHI(N)**2)))
               ENDIF
      IF(KLASS.EQ.7.AND.STHETA(N).EQ.0.0) THEN
               AFULL(N) = 0.666666667*WIDE(N)*DEEP(N)
               X      = WIDE(N)/2.0
               X1     = WIDE(N)**4/(64.0*DEEP(N)**2)
               WETPER = 8.0*DEEP(N)/WIDE(N)**2*(X*SQRT(X1+X**2)
     +                             +  X1*LOG(X+SQRT(X1+X**2)) -
     +                                X1*LOG(SQRT(X1)))
               RFULL(N) = AFULL(N)/WETPER
               ENDIF
      IF(KLASS.EQ.7.AND.STHETA(N).GT.0.0) THEN
               AFULL(N) = WIDE(N)*DEEP(N)*(1.0 - 1.0/(STHETA(N)+1.0))
               X      = WIDE(N)/2.0
               X1     = WIDE(N)**4/(64.0*DEEP(N)**2)
               WETPER = 8.0*DEEP(N)/WIDE(N)**2*(X*SQRT(X1+X**2)
     +                             +  X1*LOG(X+SQRT(X1+X**2)) -
     +                                X1*LOG(SQRT(X1)))
               RFULL(N) = AFULL(N)/WETPER
               ENDIF
  260 CONTINUE
  280 NC  = N-1
      NTC = NC
C=======================================================================
C     PRINT OUT INFORMATION ON FLAP GATES (IF ANY EXIST)
C=======================================================================
      NFLAP     = 0
      DO 9000 N = 1,NC
      IF(INGATE(N).GT.0) THEN
                         NFLAP = NFLAP + 1
                         IF(NFLAP.EQ.1) WRITE(N6,9010)
                         ND = INGATE(N)
                         IF(JCE.EQ.0) WRITE(N6,9020) NCOND(N),GTYPE(ND)
                         IF(JCE.EQ.1) WRITE(N6,9021) ACOND(N),GTYPE(ND)
                         ENDIF
 9000 CONTINUE
C=======================================================================
C     READ THE CS DATA GROUP TO DETERMINE IF THE NATURAL CROSS
C          SECTION INFORMATION WILL BE SAVED ON NSCRAT(4) OR
C          READ FROM NSCRAT(4) BY THE PROGRAM
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'CS') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,IREAD
                     ELSE
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS FROM THE SCRATCH FILE.
C=======================================================================
      IF(IREAD.EQ.'READ') THEN
               IF(NCSAVE.EQ.0) THEN
                               WRITE(N6,8999)
                               STOP
                               ENDIF
               READ(NCSAVE,ERR=8888) NATUR,NC
               READ(NCSAVE,ERR=8888) (NQC(N),N=1,NC)
               READ(NCSAVE,ERR=8888) (NUMQ(N),N=1,NC)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,1,J),J=1,26),N=1,NATUR)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,2,J),J=1,26),N=1,NATUR)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,3,J),J=1,26),N=1,NATUR)
               DO 275 N = 1,NC
               IF(NKLASS(N).LT.7) GO TO 275
               READ(NCSAVE,ERR=8888) SLOPE,AFULL(N),DEEP(N),WIDE(N),
     +                               LEN(N),ROUGH(N),RFULL(N),NKLASS(N)
  275          CONTINUE
               ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS, IF ANY.
C=======================================================================
      IF(IREAD.EQ.'SAVE'.OR.IREAD.EQ.'CARD') THEN
      IIPRNT = 0
      DO 285 N = 1,NC
      IF(NKLASS(N).EQ.8) THEN
            IF(IIPRNT.EQ.0) WRITE(N6,5355)
            IIPRNT = 1
            SLOPE = SPHI(N)
            NATUR = NATUR + 1
            KCOND = 0
            KSTOP = 0
            IF(STHETA(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 STHETA(N) = ABS(STHETA(N))
                                 ENDIF
            CALL GETCUR(N,STHETA(N),SLOPE,METRIC,0,KCOND,AFULL(N),
     +                  DEEP(N),WIDE(N),LEN(N),ROUGH(N),RFULL(N),
     +                  NSTOP,NCOND(N),ACOND(N),KSTOP)
            ENDIF
C=======================================================================
C     CALCULATE DATA FOR POWER FUNCTION CROSS SECTIONS
C=======================================================================
      IF(NKLASS(N).EQ.7) THEN
            KCOND    = 1
            NATUR    = NATUR + 1
            SLOPE    = 0.0
            KSTOP    = 0
            IF(STHETA(N).EQ.0.0) STHETA(N) = 2.0
            IF(STHETA(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 STHETA(N) = ABS(STHETA(N))
                                 ENDIF
            CALL GETCUR(N,STHETA(N),SLOPE,METRIC,0,KCOND,AFULL(N),
     +                  DEEP(N),WIDE(N),LEN(N),ROUGH(N),RFULL(N),
     +                  NSTOP,NCOND(N),ACOND(N),KSTOP)
            NKLASS(N) = 8
            ENDIF
  285 CONTINUE
      ENDIF
C=======================================================================
C     NOW SAVE THE DATA FOR IRREGULAR CHANNELS ON THE SCRATCH FILE.
C=======================================================================
      IF(IREAD.EQ.'SAVE') THEN
               IF(NCSAVE.EQ.0) THEN
                               WRITE(N6,8999)
                               STOP
                               ENDIF
               WRITE(NCSAVE,ERR=8888) NATUR,NC
               WRITE(NCSAVE,ERR=8888) (NQC(N),N=1,NC)
               WRITE(NCSAVE,ERR=8888) (NUMQ(N),N=1,NC)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,1,J),J=1,26),N=1,NATUR)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,2,J),J=1,26),N=1,NATUR)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,3,J),J=1,26),N=1,NATUR)
               DO 295 N = 1,NC
               IF(NKLASS(N).LT.7) GO TO 295
               WRITE(NCSAVE,ERR=8888) SLOPE,AFULL(N),DEEP(N),WIDE(N),
     +                                LEN(N),ROUGH(N),RFULL(N),NKLASS(N)
  295          CONTINUE
               ENDIF
C=======================================================================
C     PRINT CONDUIT DATA
C=======================================================================
      DO 300 N = 1,NC
      KLASS    = NKLASS(N)
      IF(N.EQ.1.OR.MOD(N,50).EQ.0) THEN
                             WRITE(N6,2999)
                             WRITE(N6,5060) ALPHA1,ALPHA2
                             IF(METRIC.EQ.1) WRITE(N6,5300)
                             IF(METRIC.EQ.2) WRITE(N6,5301)
                             ENDIF
      IF(JCE.EQ.0) THEN
      IF(ZU(N).EQ.0.0.AND.ZD(N).EQ.0.0) THEN
        IF(NKLASS(N).EQ.6) WRITE(N6,5320) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5321) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2)
                 ELSE
        IF(NKLASS(N).EQ.6) WRITE(N6,5322) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),ZU(N),ZD(N),
     +                     STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5323) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),ZU(N),ZD(N)
                ENDIF
      ENDIF
      IF(JCE.EQ.1) THEN
      IF(ZU(N).EQ.0.0.AND.ZD(N).EQ.0.0) THEN
        IF(NKLASS(N).EQ.6) WRITE(N6,5420) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5421) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2)
                 ELSE
        IF(NKLASS(N).EQ.6) WRITE(N6,5422) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),ZU(N),ZD(N),
     +                     STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5423) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),ZU(N),ZD(N)
                ENDIF
      ENDIF
  300 CONTINUE
C=======================================================================
C     CHECK FOR VIOLATION OF WAVE TRAVEL/CONDUIT LENGTH RATIO
C=======================================================================
      WRITE(N6,9030)
      VOLOLD   = 0.0
      VOLNEW   = 0.0
      DO 320 N = 1,NC
      VOLOLD   = VOLOLD + AFULL(N)*LEN(N)
      IF(NKLASS(N).GT.5) THEN
                         CALL HYDRAD(N,NKLASS(N),DEEP(N),RMID,AMID,BMID)
                         AB = AMID/BMID
                         IF(AB.LE.0.0) AB = 0.01
                         ELSE
                         AB = DEEP(N)
                         ENDIF
      RATIO    = SQRT(AB*GRVT)*DELT/LEN(N)
      IF(RATIO.GT.1.0.AND.JCE.EQ.0) WRITE(N6,5335) NCOND(N),RATIO
      IF(RATIO.GT.1.0.AND.JCE.EQ.1) WRITE(N6,5336) ACOND(N),RATIO
      RATIO    = SQRT(AB*GRVT)*FLOAT(NEQUAL)/LEN(N)
      IF(RATIO.GT.1.0.AND.NEQUAL.GE.1) THEN
                      CLEN     = FLOAT(NEQUAL)*SQRT(AB*GRVT)
                      ROUGH(N) = ROUGH(N)*SQRT(LEN(N)/CLEN)
                      LEN(N)   = CLEN
                      RATIO    = SQRT(AB*GRVT)*DELT/LEN(N)
                      IF(JCE.EQ.0) WRITE(N6,5337) NCOND(N),CLEN,
     +                                            ROUGH(N),RATIO
                      IF(JCE.EQ.1) WRITE(N6,5338) ACOND(N),CLEN,
     +                                            ROUGH(N),RATIO
                      VOLNEW   = VOLNEW + AFULL(N)*LEN(N)
                      ELSE
                      VOLNEW   = VOLNEW + AFULL(N)*LEN(N)
                      ENDIF
  320 CONTINUE
      IF(NEQUAL.GE.1.AND.METRIC.EQ.1) WRITE(N6,5339)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.GE.1.AND.METRIC.EQ.2) WRITE(N6,5340)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.0.AND.METRIC.EQ.1) WRITE(N6,5341)  VOLOLD
      IF(NEQUAL.EQ.0.AND.METRIC.EQ.2) WRITE(N6,5342)  VOLOLD
C=======================================================================
C     FIND JUNCTION DATA TO READ FROM GROUP D1.
C     THE FOLLOWING SEQUENCE IS REQUIRED IF NATURAL CHANNEL INFORMATION
C     WAS ENTERED IN THE DATA INPUT STREAM.  R.E.D., 4/92
C=======================================================================
      DO 370 J=1,1000
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'D1'.OR.CC.EQ.'C6') THEN
                                   BACKSPACE N5
                                   GO TO 375
                                   ENDIF
  370 CONTINUE
  375 CONTINUE
C=======================================================================
C     READ JUNCTION DATA ON DATA GROUP D1
C=======================================================================
      WRITE(*,6050)
      DO 380 J = 1,NEE
  340 READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'D1'.AND.J.GT.1) GO TO 400
      IF(J.EQ.1. AND.(CC.EQ.'C3'.OR.CC.EQ.'X1'.OR.
     *                CC.EQ.'C4'.OR.CC.EQ.'GR')) THEN
                                READ(N5,*,ERR=888) CC
                                GO TO 340
                                ENDIF
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,JUN(J),GRELEV(J),
     +                                Z(J),QINST(J),Y(J)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,AJUN(J),GRELEV(J),
     +                                Z(J),QINST(J),Y(J)
      ZCROWN(J) = Z(J)
C=======================================================================
C     Set up junction connectivity array from pipe data.
C=======================================================================
      LOC      = 0
      IF(NC.GT.0) THEN
      DO 360 N = 1,NC
      DO 360 K = 1,2
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J)) THEN
                                            LOC          = LOC+1
                                            NCHAN(J,LOC) = N
                                            ENDIF
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J))THEN
                                            LOC          = LOC+1
                                            NCHAN(J,LOC) = N
                                            ENDIF
  360 CONTINUE
      IF(LOC.EQ.0) THEN
                   IF(JCE.EQ.0) WRITE(N6,5350)  JUN(J)
                   IF(JCE.EQ.1) WRITE(N6,5351) AJUN(J)
                   JSKIP(J) = 1
                   ENDIF
      ELSE
C=======================================================================
C     ZCROWN IS EQUAL TO THE GROUND ELEVATION FOR JUNCTIONS WITH
C            NO CONNECTING CONDUITS.
C=======================================================================
      ZCROWN(J) = GRELEV(J) - Z(J)
      IF(ZCROWN(J).LE.0.0) THEN
                           IF(JCE.EQ.0) WRITE(N6,8200)  JUN(J)
                           IF(JCE.EQ.1) WRITE(N6,8201) AJUN(J)
                           NSTOP = NSTOP + 1
                           ENDIF
      ENDIF
  380 CONTINUE
  400 NJ = J-1
C=======================================================================
C     CONVERT CONDUIT CONNECTIVITY NUMBERS TO INTERNAL SYSTEM
C     ASSIGN POSITIVE DOWNSTREAM FLOW CONVENTION
C=======================================================================
      IMOVE    = 0
      IF(NC.GT.0) THEN
      DO 600 N = 1,NC
      DO 540 K = 1,2
      DO 500 J = 1,NJ
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 520
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 520
  500 CONTINUE
      IF(JCE.EQ.0) WRITE(N6,5390) NJUNC(N,K),NCOND(N)
      IF(JCE.EQ.1) WRITE(N6,5391) KJUNC(N,K),ACOND(N)
      NSTOP      = NSTOP + 1
  520 NJUNC(N,K) = J
  540 CONTINUE
      NL    = NJUNC(N,1)
      NH    = NJUNC(N,2)
      IF(JELEV.EQ.0) THEN
                     ZU(N) = Z(NL) + ZU(N)
                     ZD(N) = Z(NH) + ZD(N)
                     ELSE
                     IF(ZU(N).EQ.0.0) ZU(N) = Z(NL) + ZU(N)
                     IF(ZD(N).EQ.0.0) ZD(N) = Z(NH) + ZD(N)
                     ENDIF
      IF(ZU(N)-ZD(N).LT.0.0) THEN
                             IMOVE      = IMOVE + 1
                             IF(JCE.EQ.1) THEN
                                          KDUM(1)    = KJUNC(N,1)
                                          KJUNC(N,1) = KJUNC(N,2)
                                          KJUNC(N,2) = KDUM(1)
                                          ENDIF
                             TEMP       = ZU(N)
                             ZU(N)      = ZD(N)
                             ZD(N)      = TEMP
                             QO(N)      = -QO(N)
                             NJUNC(N,1) = NH
                             NJUNC(N,2) = NL
                             NL         = NJUNC(N,1)
                             NH         = NJUNC(N,2)
                             IF(IMOVE.EQ.1) WRITE(N6,6200)
                             IF(JCE.EQ.0) WRITE(N6,6205) IMOVE,NCOND(N)
                             IF(JCE.EQ.1) WRITE(N6,6206) IMOVE,ACOND(N)
                             ENDIF
      IF((ZU(N) + DEEP(N)).GT.ZCROWN(NL))  ZCROWN(NL) = ZU(N)+DEEP(N)
      IF((ZD(N) + DEEP(N)).GT.ZCROWN(NH))  ZCROWN(NH) = ZD(N)+DEEP(N)
      IF(ZCROWN(NL).GT.GRELEV(NL)+0.001) THEN
                    IF(JCE.EQ.0) WRITE(N6,5395) NCOND(N),JUN(NL)
                    IF(JCE.EQ.1) WRITE(N6,5396) ACOND(N),AJUN(NL)
                    ZCROWN(NL) = GRELEV(NL) - 0.01
                    NSTOP      = NSTOP + 1
                    ENDIF
      IF(ZCROWN(NH).GT.GRELEV(NH)+0.001) THEN
                    IF(JCE.EQ.0) WRITE(N6,5395) NCOND(N),JUN(NH)
                    IF(JCE.EQ.1) WRITE(N6,5396) ACOND(N),AJUN(NH)
                    ZCROWN(NH) = GRELEV(NH) - 0.01
                    NSTOP      = NSTOP + 1
                    ENDIF
  600 CONTINUE
      ENDIF
C=======================================================================
C     Print junction data.
C=======================================================================
      DO 460 J = 1,NJ
      MPT      = 0
      NZP      = 0
      DO 420 I = 1,8
      K1       = NCHAN(J,I)
      IF(K1.EQ.0)            GO TO 440
      IF(JCE.EQ.0) IDUM(I) = NCOND(K1)
      IF(JCE.EQ.1) KDUM(I) = ACOND(K1)
      MPT                  = MPT + 1
C=======================================================================
C     Check for all conduits above the Junction invert.
C=======================================================================
                           JJ = 1
      IF(NJUNC(K1,1).EQ.J) JJ = 1
      IF(NJUNC(K1,2).EQ.J) JJ = 2
      IF(JJ.EQ.1.AND.ZU(K1).GT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.2.AND.ZD(K1).GT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.1.AND.ZU(K1).LT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.2.AND.ZD(K1).LT.Z(J)) NZP = NZP + 1
  420 CONTINUE
  440 CONTINUE
      IF(J.EQ.1.OR.MOD(J,50).EQ.0) THEN
                                   WRITE(N6,2999)
                                   WRITE(N6,5060) ALPHA1,ALPHA2
                                   IF(METRIC.EQ.1) WRITE(N6,5360)
                                   IF(METRIC.EQ.2) WRITE(N6,5361)
                                   ENDIF
      IF(MPT.GT.0) THEN
      IF(JCE.EQ.0) WRITE(N6,5380) J,JUN(J),GRELEV(J),ZCROWN(J),
     +                            Z(J),QINST(J),Y(J),(IDUM(K),K=1,MPT)
      IF(JCE.EQ.1) WRITE(N6,5381) J,AJUN(J),GRELEV(J),ZCROWN(J),
     +                            Z(J),QINST(J),Y(J),(KDUM(K),K=1,MPT)
      IF(NZP.EQ.MPT.AND.JCE.EQ.0) WRITE(N6,5382)  JUN(J)
      IF(NZP.EQ.MPT.AND.JCE.EQ.1) WRITE(N6,5383) AJUN(J)
      ELSE
      IF(JCE.EQ.0) WRITE(N6,5380) J,JUN(J),GRELEV(J),
     +                            ZCROWN(J),Z(J),QINST(J),Y(J)
      IF(JCE.EQ.1) WRITE(N6,5381) J,AJUN(J),GRELEV(J),
     +                            ZCROWN(J),Z(J),QINST(J),Y(J)
      ENDIF
  460 CONTINUE
  480 CONTINUE
C=======================================================================
C     Check for high pipe and print a warning at the upstream end.
C=======================================================================
      DO 495 N = 1,NC
      J        = NJUNC(N,1)
      IF(ZU(N).EQ.Z(J)) GO TO 495
      ITEST     = 0
      JTEST     = 0
      DO 490 KK = 1,8
      NKK       = NCHAN(J,KK)
      IF(NKK.EQ.N) GO TO 490
      IF(NKK.EQ.0.OR.NKK.GT.NC) GO TO 494
                            JJ = 1
      IF(NJUNC(NKK,1).EQ.J) JJ = 1
      IF(NJUNC(NKK,2).EQ.J) JJ = 2
      JTEST = JTEST + 1
      IF(JJ.EQ.1.AND.ZU(N).LE.ZU(NKK) + DEEP(NKK)) GO TO 490
      IF(JJ.EQ.2.AND.ZU(N).LE.ZD(NKK) + DEEP(NKK)) GO TO 490
      ITEST = ITEST + 1
  490 CONTINUE
  494 CONTINUE
      IF(ITEST.EQ.JTEST.AND.JCE.EQ.0) WRITE(N6,5392) NCOND(N),JUN(J)
      IF(ITEST.EQ.JTEST.AND.JCE.EQ.1) WRITE(N6,5393) ACOND(N),AJUN(J)
  495 CONTINUE
C=======================================================================
C     Check for high pipe and print a warning at the downstream end.
C=======================================================================
      DO 595 N = 1,NC
      J        = NJUNC(N,2)
      IF(ZD(N).EQ.Z(J)) GO TO 595
      ITEST     = 0
      JTEST     = 0
      DO 590 KK = 1,8
      NKK       = NCHAN(J,KK)
      IF(NKK.EQ.N) GO TO 590
      IF(NKK.EQ.0.OR.NKK.GT.NC) GO TO 594
                            JJ = 1
      IF(NJUNC(NKK,1).EQ.J) JJ = 1
      IF(NJUNC(NKK,2).EQ.J) JJ = 2
      JTEST = JTEST + 1
      IF(JJ.EQ.1.AND.ZD(N).LE.ZU(NKK) + DEEP(NKK)) GO TO 590
      IF(JJ.EQ.2.AND.ZD(N).LE.ZD(NKK) + DEEP(NKK)) GO TO 590
      ITEST = ITEST + 1
  590 CONTINUE
  594 CONTINUE
      IF(ITEST.EQ.JTEST.AND.JCE.EQ.0) WRITE(N6,5392) NCOND(N),JUN(J)
      IF(ITEST.EQ.JTEST.AND.JCE.EQ.1) WRITE(N6,5393) ACOND(N),AJUN(J)
  595 CONTINUE
C=======================================================================
      WRITE(*,6100)
      RETURN
 888  CALL IERROR
 8888 WRITE(N6,8887)
      STOP
C=======================================================================
 2999 FORMAT(/,
     1       '1',40(2H--)/' ','ENVIRONMENTAL PROTECTION AGENCY',13X,40H*
     2***   EXTENDED TRANSPORT PROGRAM   ****,8X,'WATER RESOURCES DIVISI
     3ON',/,' ','WASHINGTON, D.C.            ',16X,4H****,32X,4H****,8X,
     4'CAMP DRESSER & MCKEE INC.',/,' ','                ',28X,4H****,
     56X,'   ANALYSIS MODULE  ',6X,4H****,8X,'ANNANDALE, VIRGINIA')
 5060 FORMAT(/,5X,A80,/,5X,A80,/)
 5100 FORMAT(/,' Control information for simulation',/,
     +       ' ----------------------------------',//,
     +       ' Integration cycles.................',I8,/)
 5120 FORMAT(' Length of integration step is......',F8.2,
     +       ' seconds',/,
     +       ' Simulation length..................',F8.2,' hours',/)
 5121 FORMAT(' Do not create equiv. pipes(NEQUAL).',I8,/)
 5122 FORMAT(' Create equivalent conduits based ',/,
     +       ' on the COURANT condition...........',I8,/)
 5123 FORMAT(' Use U.S. customary units for I/O...',I8,/)
 5124 FORMAT(' Use metric units for I/O...........',I8,/)
 5140 FORMAT(' Printing starts in cycle...........',I8,//,
     +       ' Intermediate printout intervals of.',I8,' cycles',/,
     +       ' Intermediate printout intervals of.',F8.2,' minutes',//,
     +       ' Summary printout intervals of......',I8,' cycles',/,
     +       ' Summary printout time interval of..',F8.2,' minutes',//,
     +       ' Hot start file parameter (REDO)....',I8,/)
 5160 FORMAT(' Initial time.......................',F8.2,' hours')
C#### WCH, 4/11/94.
 5162 FORMAT(' Initial date (default).............',I8,' (yr/mo/day)')
 5164 FORMAT(' Initial date.......................',I8,' (yr/mo/day)') 
 5170 FORMAT(/,' Iteration variables: ITMAX.........',I8,/,
     1         '                      SURTOL........',F8.4,/)
 5175 FORMAT(' Default surface area of junctions....',F8.2,
     +       ' square feet.',/)
 5176 FORMAT(' Default surface area of junctions..',F6.2,
     +       ' square meters.',/)
 5177 FORMAT(' EXTRAN VERSION 3.3 SOLUTION. (ISOL = 0).',/,
     +       ' Sum of Junction flow is zero during surcharge.',/)
 5178 FORMAT(' SEMI-IMPLICIT EXTRAN SOLUTION. (ISOL = 1).',/,
     +       ' NEWTON-RAPHSON SURCHARGE ITERATION.',/)
 5179 FORMAT(' ITERATIVE EXTRAN SOLUTION. (ISOL = 2).',/,
     +       ' NEWTON-RAPHSON SURCHARGE ITERATION.',/)
 5180 FORMAT(' NJSW INPUT HYDROGRAPH JUNCTIONS....',I6)
 5183 FORMAT(' NORMAL FLOW OPTION WHEN THE WATER  ',/,
     +       ' SURFACE SLOPE IS LESS THAN THE     ',/,
     +       ' GROUND SURFACE SLOPE (KSUPER=0)....',/)
 5184 FORMAT(' NORMAL FLOW OPTION WHEN THE FROUDE ',/,
     +       ' # IS GREATER THAN 1.0 (KSUPER=1)...',/)
 5185 FORMAT(/,' Note: ZU and ZD are elevations not offsets on C1 data l
     +ines.  You are using the BB optional data group.',/)
 5200 FORMAT(/,' Printed output for the following',I3,
     +         ' Junctions',//,(5X,10I10))
 5201 FORMAT(/,' Printed output for the following',I3,
     +         ' Junctions',//,(5X,10A10))
 5220 FORMAT(/,' Printed output for the following',I3,
     +         ' Conduits',//,(5X,10I10))
 5221 FORMAT(/,' Printed output for the following',I3,
     +         ' Conduits',//,(5X,10A10))
 5240 FORMAT (/,' Water surface elevations will be plotted',
     +    ' for the following ',I3,' Junctions',//,(5X,10I10))
 5241 FORMAT (/,' Water surface elevations will be plotted',
     +    ' for the following ',I3,' Junctions',//,(5X,10A10))
 5260 FORMAT(/,' Flow rate will be plotted for the following ',
     +             I3,' Conduits',//,(5X,10I10))
 5261 FORMAT(/,' Flow rate will be plotted for the following ',
     +             I3,' Conduits',//,(5X,10A10))
 5265 FORMAT(/,' THE WATER SURFACE SLOPE WILL BE PLOTTED FOR THE ',
     +         'FOLLOWING ',I3,' CONDUITS',//,(5X,10I10))
 5266 FORMAT(/,' THE WATER SURFACE SLOPE WILL BE PLOTTED FOR THE ',
     +         'FOLLOWING ',I3,' CONDUITS',//,(5X,10A10))
 5300 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                     Conduit Data                  *',/,
     +' *****************************************************',//,
     1'  INP  CONDUIT    LENGTH   CONDUIT     AREA    MANNING MAX WIDTH
     1    DEPTH       JUNCTIONS      INVERT HEIGHT     TRAPEZOID',/,
     2'  NUM   NUMBER     (FT)      CLASS   (SQ FT)     COEF.   (FT)
     2     (FT)      AT THE ENDS    ABOVE JUNCTIONS   SIDE SLOPES',/,
     3' ----   ------   -------  --------   -------   ------- ---------
     3    -----   -------   ------- ---------------   ----- -----')
 5301 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                     Conduit Data                  *',/,
     +' *****************************************************',//,
     1'  INP  CONDUIT    LENGTH   CONDUIT     AREA    MANNING MAX WIDTH
     1    DEPTH       JUNCTIONS      INVERT HEIGHT     TRAPEZOID',/,
     2'  NUM   NUMBER     (M)       CLASS    (SQ M)     COEF.   (M)
     2     (M)       AT THE ENDS    ABOVE JUNCTIONS   SIDE SLOPES',/,
     3' ----   ------   -------  --------   -------   ------- ---------
     3    -----   -------   ------- ---------------   ----- -----')
 5320 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2I10,16X,2F7.2)
 5321 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2I10)
 5322 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2I10,2F8.2,2F7.2)
 5323 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2I10,2F8.2)
 5335 FORMAT(' ===> WARNING !!! (C*DELT/LEN) IN CONDUIT',
     .       I10,' IS',F5.1,' AT FULL DEPTH.')
 5336 FORMAT(' ===> WARNING !!! (C*DELT/LEN) IN CONDUIT ',
     .       A10,' IS',F5.1,' AT FULL DEPTH.')
 5337 FORMAT(' ===> THE NEW EQUIVALENT LENGTH/ROUGHNESS/RATIO FOR COND',
     +'UIT ',I10,' IS ',F10.1,' / ',F10.7,' / ',F5.1)
 5338 FORMAT(' ===> THE NEW EQUIVALENT LENGTH/ROUGHNESS/RATIO FOR COND',
     +'UIT ',A10,' IS ',F10.1,' / ',F10.7,' / ',F5.1)
 5339 FORMAT(/,
     +' ****************************************',/,
     +' *  Equivalent Conduit Volume Analysis  *',/,
     +' ****************************************',//,
     +' Input full depth volume............',1PE14.4,' cubic feet',/,
     +' New full depth volume..............',1PE14.4,' cubic feet',/,
     +' New volume / Old volume ratio......',0PF14.4)
 5340 FORMAT(/,
     +' ****************************************',/,
     +' *  Equivalent Conduit Volume Analysis  *',/,
     +' ****************************************',//,
     +' Input full depth volume............',1PE14.4,' cubic meters',/,
     +' New full depth volume..............',1PE14.4,' cubic meters',/,
     +' New volume / Old volume ratio......',0PF14.4)
 5341 FORMAT(/,
     +' ********************',/,
     +' *  Conduit Volume  *',/,
     +' ********************',//,
     +' Input full depth volume............',1PE14.4,' cubic feet')
 5342 FORMAT(/,
     +' ********************',/,
     +' *  Conduit Volume  *',/,
     +' ********************',//,
     +' Input full depth volume............',1PE14.4,' cubic meters')
 5350 FORMAT(/,'===> WARNING !!! JUNCTION',I10,
     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT.')
 5351 FORMAT(/,'===> WARNING !!! JUNCTION',A10,
     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT.')
 5355 FORMAT(//,'$$$ FOR NATURAL CHANNELS WITH MULTIPLE DEPRESSIONS OR V
     *ARIABLE MANNINGS N,',/,'    FLOW IS SUM OF FLOWS IN DEPRESSIONS AN
     *D/OR VARIABLE-N SEGMENTS.',/,'    THIS IS EQUIVALENT TO SUMMING CO
     *NVEYANCES ACROSS THE CHANNEL. $$$'//)
 5360 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                  Junction Data                    *',/,
     +' *****************************************************',//,
     1' INP  JUNCTION    GROUND    CROWN     INVERT     QINST   INITIAL
     +    CONNECTING CONDUITS',/,
     3' NUM    NUMBER     ELEV.     ELEV.     ELEV.       CFS DEPTH(FT)'
     3,/,' ---    ------   -------   -------    ------   ------- -------
     3--     -------------------')
 5361 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                  Junction Data                    *',/,
     +' *****************************************************',//,
     1' INP  JUNCTION    GROUND    CROWN     INVERT     QINST   INITIAL
     +  CONNECTING CONDUITS',/,
     3' NUM    NUMBER     ELEV.     ELEV.     ELEV.       CMS  DEPTH(M)'
     3,/,' ---    ------   -------   -------    ------   ------- -------
     3--   -------------------')
 5380 FORMAT(I4,I10,F10.2,4F10.2,3X,8(I7,1X))
 5381 FORMAT(I4,1X,A10,F9.2,4F10.2,3X,5(A10,1X),/,67X,3(A10,1X))
 5382 FORMAT(' ===> Warning all conduits connecting to Junction ',I9,
     +       ' lie above the Junction invert.')
 5383 FORMAT(' ===> Warning all conduits connecting to Junction ',A9,
     +       ' lie above the Junction invert.')
 5390 FORMAT(/,' ===> ERROR !!! JUNCTION',I9,' ON CONDUIT',I9,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5391 FORMAT(/,' ===> ERROR !!! JUNCTION',A10,' ON CONDUIT ',A10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5392 FORMAT(' ===> WARNING !!!  THE INVERT OF ',
     *'CONDUIT ',I9,' LIES ABOVE THE CROWN OF ALL CONDUITS ',
     *' AT JUNCTION ',I9)
 5393 FORMAT(' ===> WARNING !!!  THE INVERT OF ',
     *'CONDUIT ',A10,' LIES ABOVE THE CROWN OF ALL CONDUITS ',
     *' AT JUNCTION ',A10)
 5395 FORMAT(/,' ===> ERROR !!!  CONDUIT',I9,' HAS CAUSED ZCROWN OF',
     1       ' JUNCTION',I9,' TO LIE ABOVE THE SPECIFIED GROUND ELEV.')
 5396 FORMAT(/,' ===> ERROR !!!  CONDUIT ',A10,' HAS CAUSED ZCROWN ',
     1   'OF JUNCTION ',A10,' TO LIE ABOVE THE SPECIFIED GROUND ELEV.')
 5420 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,
     +                                 2X,2A9,16X,2F7.2)
 5421 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,2A9)
 5422 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,2A9,
     +                                           2F8.2,2F7.2)
 5423 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,2A9,2F8.2)
 6000 FORMAT(/,' Reading conduit data.')
 6050 FORMAT(/,' Reading junction data.')
 6100 FORMAT(/,' Reading remaining simulation data.')
 6200 FORMAT(/,' ===> Warning !! The upstream and downstream junctions f
     +or the following conduits',/,
     + '                 have been reversed to correspond to the positiv
     +e flow and decreasing',/
     +,'                 slope EXTRAN convention.  A negative flow in th
     +e output thus means ',/,
     + '                 the flow was from your original upstream juncti
     +on to your original',/,
     + '                 downstream junction.  Any initial flow has been
     + multiplied by -1.',/)
 6205 FORMAT(13X,I5,'.  Conduit #...',I10,'  has been changed.')
 6206 FORMAT(13X,I5,'.  Conduit #...',A10,'  has been changed.')
 8200 FORMAT(' ===> ERROR !!! THE GROUND ELEVATION OF JUNCTION ',I9,
     *           ' LIES ABOVE THE JUNCTION INVERT ELEVATION.')
 8201 FORMAT(' ===> ERROR !!! THE GROUND ELEVATION OF JUNCTION ',A10,
     *           ' LIES ABOVE THE JUNCTION INVERT ELEVATION.')
 8887 FORMAT(/,' ERROR !!  READING OR WRITING NATURAL CHANNEL',
     +         ' INFORMATION ON NSCRAT(4).')
 8999 FORMAT(/,' ERROR !!  THE NATURAL CHANNEL INFORMATION WAS',
     +         ' NOT PRESENT ON THE NSCRAT(4) FILE.')
 9010 FORMAT(/,
     +' ************************************************************',/,
     +' *               Flap Gate Conduit Information              *',/,
     +' *               -----------------------------              *',/,
     +' * POSITIVE FLAP GATE - FLOW ONLY ALLOWED FROM THE UPSTREAM *',/,
     +' *                      TO THE DOWNSTREAM JUNCTION          *',/,
     +' * NEGATIVE FLAP GATE - FLOW ONLY ALLOWED FROM THE          *',/,
     +' *                      DOWNSTREAM TO THE UPSTREAM JUNCTION *',/,
     +' ************************************************************',//
     +,'    Conduit   Type of Flap Gate',/,
     + '    -------   -----------------')
 9020 FORMAT(1X,I10,A20)
 9021 FORMAT(1X,A10,A20)
 9030 FORMAT(/)
C=======================================================================
      END
