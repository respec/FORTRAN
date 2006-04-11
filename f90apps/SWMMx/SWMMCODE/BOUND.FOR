      SUBROUTINE BOUND(YDEP,YDEPT,QP,T,DT)
C=======================================================================
C     THIS SUBROUTINE COMPUTES THE LINK FLOW 'QP(LINK)' FOR
C     EACH EXTERNAL AND INTERNAL NODE TO NODE TRANSFER
C     RED, 11/29/93.  MAKE SURE FLOW IN OUTFALL PIPE TO CLOSED GATES IS
C       ZERO TO AVOID BAD CONTINUITY ERRORS.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'VOLDAT.INC'
      DIMENSION YDEP(NEE),YDEPT(NEE),QP(NEE)
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCUR AT EACH REACH ITERATION
C=======================================================================
      IF(IT.GT.0.AND.ISOL.LT.1) GO TO 200
C=======================================================================
C     ASSIGN SURFACE AREA TO STORAGE JUNCTIONS
C     NEW COMPUTATION OF AREA FOR IRREGULAR STORAGE ELEMENTS
C=======================================================================
      IF(NSTORE.GT.0) THEN
                      DO 114 I = 1,NSTORE
                      J        = JSTORE(I)
                      IF(ASTORE(I).GE.0.0) THEN
                          AS(J)      = AS(J) + ASTORE(I)
                          ELSE IF(NUMV(I).LT.0) THEN
                          IF(YDEP(J).EQ.0.0) YDEP(J) = FUDGE
                          AS(J) = AS(J) +
     +                            VCURVE(I,1,1)*YDEP(J)**VCURVE(I,2,1)
                          ELSE
                          NTOX       = NUMV(I)
                          RESELV     = YDEP(J)
                          DO 1131 IX = 2,NTOX
                          IF(RESELV.LT.VCURVE(I,2,IX)) GO TO 1132
 1131                     CONTINUE
 1132                     DELTA = (RESELV-VCURVE(I,2,IX-1))/
     +                            (VCURVE(I,2,IX)-VCURVE(I,2,IX-1))
                          AS(J) = AS(J)+DELTA*(VCURVE(I,1,IX) -
     +                            VCURVE(I,1,IX-1))+VCURVE(I,1,IX-1)
                          ENDIF
  114                CONTINUE
                     ENDIF
C=======================================================================
C     COMPUTE DISCHARGE OVER TRANSVERSE AND SIDEFLOW WEIRS
C=======================================================================
  200 IF(NWEIR.GT.0) THEN
         DO 560 I = 1,NWEIR
         WK       = COEF(I)
         POWER    = 1.5
         V2       = 0.0
         LINK     = LWEIR(I)
         DIR      = +1.0
         J1       = NJUNC(LINK,1)
         J2       = NJUNC(LINK,2)
         Y1       = YDEP(J1)  + Z(J1)
         CREST    = YCREST(I) + Z(J1)
         IF(J2.LE.0) THEN
                     ICASE     = JTIDES(J1)
                     HTIDE(J1) = HTIDES(ICASE,T)
                     Y2        = AMAX1(HTIDE(J1),CREST)
                     HEADW     = Y1-CREST
                     IF(HEADW) 480,480,305
                     ENDIF
         Y2    = YDEP(J2) + Z(J2)
         HEADW = AMAX1(Y1,Y2) - CREST
         IF(HEADW) 480,480,280
C=======================================================================
C     CHECK FOR BACKFLOW
C=======================================================================
  280    IF(Y1-Y2.LT.0.0) THEN
                          DIR = -1.0
                          Y1  = Y2
                          Y2  = YDEP(J1) + Z(J1)
                          J1  = J2
                          J2  = NJUNC(LINK,1)
                          ENDIF
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C=======================================================================
  305 IF(IT.GT.0.AND.ISOL.LE.1) THEN
                  IF(JCHECK(J1).EQ.IND(2)) GO TO 320
                  IF(J2.LE.0)              GO TO 560
                  IF(JCHECK(J2).EQ.IND(1)) GO TO 560
                  ENDIF
  320 IF(Y1.GT.YTOP(I)+Z(J1)) GO TO 440
      IF(DIR) 380,340,340
  340 IF(KWEIR(I)-3) 380,360,360
C=======================================================================
C     WK IS A FUNCTION OF APPROACH VELOCITY FOR SIDEFLOW WEIRS
C=======================================================================
  360 WK    = COEF(I)
      V2    = 0.0
      POWER = 1.66667
C=======================================================================
C     WEIR DISCHARGE
C=======================================================================
  380 GVT   = 2.0*GRVT
      QWEIR = WK*WLEN(I)*((HEADW+V2/GVT)**POWER-(V2/GVT)**POWER)
      KW    = KWEIR(I)
      GO TO (420,400,420,400),KW
C=======================================================================
C     APPLY ARMCO TIDE GATE CORRECTION
C     (ARMCO WATER CONTROL GATES CATALOG)
C=======================================================================
  400 IF(HTIDE(J1).GE.(YDEP(J1)+Z(J1))) GO TO 480
      VEL1  = COEF(I)*HEADW**(POWER-1.0)
      HLOSS = (4./GRVT)*VEL1**2*EXP(-1.15*VEL1/SQRT(YTOP(I)-YCREST(I)))
      HEADW = HEADW - HLOSS
      IF(HEADW.LE.0)                 GO TO 480
      IF((CREST+HEADW).LE.HTIDE(J1)) GO TO 480
      QWEIR = COEF(I)*WLEN(I)*HEADW**POWER
C=======================================================================
C     SUBMERGED WEIR COMPUTATIONS, DFK, 8/74
C=======================================================================
  420 RATIO = (Y2-CREST)/(Y1-CREST)
      IF((Y2-CREST).LE.0) GO TO 500
      IF(RATIO.LE.0.30)   GO TO 421
      IF(RATIO.LE.0.75)   GO TO 422
      IF(RATIO.LE.0.85)   GO TO 423
      IF(RATIO.LE.0.95)   GO TO 424
      CONST = 0.4-0.3*(RATIO-0.95)/0.05
      GO TO 430
  421 CONST = 1.0
      GO TO 430
  422 CONST = 1.0-0.1*(RATIO-0.3)/0.45
      GO TO 430
  423 CONST = 0.9-0.1*(RATIO-0.75)/0.1
      GO TO 430
  424 CONST = 0.8-0.4*(RATIO-0.85)/0.1
  430 QWEIR = CONST*COEF(I)*WLEN(I)*(Y1-CREST)**1.5
      GO TO 500
C=======================================================================
C     OUTFLOW IN SURCHARGED CONDITION
C=======================================================================
  440 IF(Y1-Y2) 480,480,460
  460 HEADW = Y1 - AMAX1(Y2,CREST)
      IF(COEFS(I).GT.0.0) GO TO 470
      A(LINK)   = (YTOP(I)-YCREST(I))*WLEN(I) + 1.0E-10
                      DDD = DT
      IF(NEQUAL.GE.1) DDD = FLOAT(NEQUAL)
      LEN(LINK) = AMAX1(200.,2.0*DDD*
     +            (GRVT*(4.0*A(LINK)/3.14159)**0.5)**0.5)
      COEFS(I)  = ABS(QP(LINK))/(A(LINK)*SQRT(2.0*GRVT*HEADW))
      IF(JCE.EQ.0) WRITE(N6,9999) NCOND(LINK),COEFS(I),A(LINK),
     +                            LEN(LINK),TIME/3600.0
      IF(JCE.EQ.1) WRITE(N6,9998) ACOND(LINK),COEFS(I),A(LINK),
     +                            LEN(LINK),TIME/3600.0
  470 QWEIR = COEFS(I)*A(LINK)*SQRT(2.0*GRVT*HEADW)
      DQDH  = COEFS(I)*A(LINK)*GRVT/SQRT(2.0*GRVT*HEADW)
      IF(ISOL.LE.1) THEN
                    SUMAL(J1) = SUMAL(J1) + DQDH
                    SUMAL(J2) = SUMAL(J2) + DQDH
                    ENDIF
      GO TO 500
  480 QWEIR    = 0.0
  500 IF(ISOL.LE.1) QP(LINK) = DIR*QWEIR
      IF(ISOL.GE.2) QP(LINK) = 0.50*DIR*QWEIR + 0.50*DIR*QP(LINK)
  560 CONTINUE
      ENDIF
C=======================================================================
C     COMPUTE PUMP DISCHARGES
C     NOTE -- ONLY ONE INFLUENT PIPE CAN BE CONNECTED TO A PUMP NODE
C=======================================================================
      IF(NPUMP.GT.0) THEN
      DO 900  I = 1,NPUMP
      LINK      = LPUMP(I)
      J1        = NJUNC(LINK,1)
      J2        = NJUNC(LINK,2)
      IPIP      = IPTYP(I)
      GO TO (710,880,610),IPIP
C=======================================================================
C     PUMP CURVE FORMULATION
C=======================================================================
  610 IF(ISOL.GE.2) GO TO 620
      IF(IT) 620,620,615
  615 IF(Y(J1).GT.0.96*(ZCROWN(J1)-Z(J1))) GO TO 620
      IF(Y(J2).LE.0.96*(ZCROWN(J2)-Z(J2))) GO TO 900
  620 JSKIP(J1) = 0
      QOUT      = 0.0
      DH        = (YDEP(J2)+Z(J2)) - (YDEP(J1)+Z(J1))
C=======================================================================
C     DETERMINE PUMP OPERATION
C=======================================================================
      IF(IPOPR(I)) 630,630,625
C=======================================================================
C     PUMP CURRENTLY ON
C=======================================================================
  625 IF(YDEP(J1).GT.POFF(I)) GO TO 640
      IPOPR(I) = -1
      GO TO 700
C=======================================================================
C     PUMP CURRENTLY OFF
C=======================================================================
  630 IF(YDEP(J1).LT.PON(I)) GO TO 700
      IPOPR(I) = 1
C=======================================================================
C     COMPUTE PUMP FLOW FROM LINEARIZED PUMP CURVE
C=======================================================================
  640 H1 = VRATE(I,1)
      H2 = VRATE(I,2)
      H3 = VRATE(I,3)
      Q1 = PRATE(I,1)
      Q2 = PRATE(I,2)
      Q3 = PRATE(I,3)
C#######################################################################
C CAUTION. CHECK IN INDAT2 TO BE SURE H1 NE H2 NE H3 TO AVOID ZERO DIVIDE
C#######################################################################
                     QOUT = AMAX1(Q1,Q2 + (H2-DH)*(Q2-Q1)/(H1-H2))
      IF(QOUT.GT.Q2) QOUT = AMIN1(Q3,Q2 + (H2-DH)*(Q3-Q2)/(H2-H3))
                     QOUT = 0.5*(QO(LINK) + QOUT)
C=======================================================================
C     LIMIT PUMP Q IF JUNCTION PUMPED DRY
C=======================================================================
      QINJ     = QIN(J1)
      DO 650 K = 1,8
      N        = NCHAN(J1,K)
      IF(N.EQ.LINK) GO TO 660
                           DQ = 0.5*(QP(N) + QO(N))
      IF(NJUNC(N,1).EQ.J1) DQ = -DQ
      QINJ                    = QINJ + DQ
  650 CONTINUE
  660 IF(AS(J1).LE.0.0) GO TO 700
      IF(Y(J1)+(QINJ-0.5*(QOUT+QO(LINK)))*DT/AS(J1).GT.0.0) GO TO 700
  680                       QOUT = QINJ
  700 IF(QOUT.LE.0.0)       QOUT = 0.0
      QP(LINK) = QOUT
      GO TO 900
C=======================================================================
C     OFF-LINE PUMP STATION WITH WET WELL; IPTYP
C     COMPUTE INFLOW TO WET WELL FOR GATES OPEN CONDITION
C=======================================================================
  710 N    =  NCHAN(J1,1)
      QINJ = QP(N)
      JUP  = NJUNC(N,1)
      IF(JUP.NE.J1) GO TO 711
      JUP  = NJUNC(N,2)
      QINJ = -QP(N)
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURING
C=======================================================================
  711 IF(IT.GT.0.AND.ISOL.LE.1) THEN
                  IF(Y(JUP).GT.0.96*(ZCROWN(JUP)-Z(JUP))) GO TO 715
                  IF(Y(J1).LE.0.96*(ZCROWN(J1)-Z(J1)))    GO TO 900
                  ENDIF
  715 IF(QINJ.LT.0.0) QINJ = 0.0
      CALL DEPTHX(N,NKLASS(N),QP(N),YCRIT,YNORM)
C=======================================================================
C     SET CRITICAL DEPTH AT WET WELL FOR OFF-LINE PUMP
C=======================================================================
      YDEPT(J1) = AMIN1(YCRIT,YNORM)
      IF(ISOL.LE.1) VWELL(I)  = VWELL(I) + QINJ*DELT2
      IF(ISOL.GE.2) VWELL(I)  = VWBEG(I) + QINJ*DT
C=======================================================================
C     SET OFF-LINE PUMP RATE
C=======================================================================
      QOUT = 0.0
      IF(VWELL(I)) 800,800,740
  740 QOUT = PRATE(I,1)
      IF(VWELL(I)-VRATE(I,1)) 800,760,760
  760 QOUT = PRATE(I,2)
      IF(VWELL(I)-VRATE(I,2)) 800,780,780
  780 QOUT = PRATE(I,3)
C=======================================================================
C     COMPUTE NEW WET WELL VOLUME
C=======================================================================
  800 IF(ISOL.LE.1) VNEW =  VWELL(I) - QOUT*DELT2
      IF(ISOL.GE.2) VNEW =  VWBEG(I) - QOUT*DT
C=======================================================================
C     CHECK FOR DRY WELL
C=======================================================================
      IF(VNEW.LE.0.0) THEN
                      IF(ISOL.LE.1) QOUT  = VWELL(I)/DELT2
                      IF(ISOL.GE.2) QOUT  = VWELL(I)/DT
                      VWELL(I) = 0.0
                      QP(LINK) = QOUT
                      JPFUL(I) = 1
                      GO TO 900
                      ENDIF
C=======================================================================
C     CHECK FOR FLOODED WELL
C=======================================================================
      IF(VRATE(I,3)-VNEW.LE.0.0) THEN
               IF(ISOL.LE.1) DIFF = (VNEW-VRATE(I,3))/DELT2
               IF(ISOL.GE.2) DIFF = (VNEW-VRATE(I,3))/DT
               VWELL(I) = VRATE(I,3)
               QOUT     = PRATE(I,3)
               QP(LINK) = QOUT
               N        = NCHAN(J1,1)
C=======================================================================
C              THROTTLE PUMP STATION INFLOW
C=======================================================================
               QP(N)      = QP(N) - SIGN(DIFF,QP(N))
               SUMQ(JUP)  = SUMQ(JUP)  + 0.5*DIFF
               SUMQS(JUP) = SUMQS(JUP) + DIFF
               IF(ISOL.GE.2) AT(N) = QP(N)
               GO TO 900
               ENDIF
C=======================================================================
C     NORMAL WET WELL CONDITION
C=======================================================================
      VWELL(I) = VNEW
      QP(LINK) = QOUT
      GO TO 900
C=======================================================================
C     SET PUMP RATE FOR IN-LINE LIFT PUMP
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C=======================================================================
  880 IF(ISOL.GE.2) GO TO 875
      IF(IT) 875,875,872
  872 IF(Y(J1).LE.0.96*(ZCROWN(J1)-Z(J1))) GO TO 900
  875 JSKIP(J1) = 0
      IF(YDEP(J1)-FUDGE) 882,882,885
  882 QINJ     = 0.0
      DO 883 K = 1,8
      N        = NCHAN(J1,K)
      IF(N.GT.NC) GO TO 884
      IF(NJUNC(N,2).EQ.J1) QINJ = QINJ + QP(N)
  883 CONTINUE
  884                        QOUT = QINJ
      IF(QOUT.GE.PRATE(I,1)) QOUT = PRATE(I,1)
      GO TO 888
C=======================================================================
C     SET PUMP RATE
C=======================================================================
  885                            QOUT = PRATE(I,1)
      IF(YDEP(J1).GT.VRATE(I,1)) QOUT = PRATE(I,2)
      IF(YDEP(J1).GT.VRATE(I,2)) QOUT = PRATE(I,3)
  888 IF(QOUT.GT.PRATE(I,3))     QOUT = PRATE(I,3)
      IF(ISOL.LE.1) QP(LINK) =     QOUT
      IF(ISOL.GE.2) QP(LINK) = 0.5*QOUT + 0.5*QP(LINK)
  900 CONTINUE
      ENDIF
C=======================================================================
C     SET DEPTH AT FREE OUTFALL * TIDAL NODES (ONE PIPE/NODE)
C=======================================================================
      IF(NFREE.GT.0) THEN
               DO 960 I = 1,NFREE
               J        = JFREE(I)
               ICASE    = JTIDES(J)
               HTIDE(I) = HTIDES(ICASE,T)
               N        = NCHAN(J,1)
               LINK     = NCHAN(J,2)
               QP(LINK) = QP(N)
               IF(JSKIP(J).EQ.2) THEN
                                 YDEPT(J) = 1.0
                                 GO TO 960
                                 ENDIF
C=======================================================================
C              CHECK FOR OUTFALL PIPE ON AN ADVERSE SLOPE
C=======================================================================
               IF(NJUNC(N,1).EQ.J) QP(LINK) = -QP(LINK)
               CALL DEPTHX(N,NKLASS(N),QP(N),YCRIT,YNORM)
               IF(JDOWN.EQ.0) YDEPT(J) = AMIN1(YCRIT,YNORM)
               IF(JDOWN.EQ.1) YDEPT(J) = YCRIT
               IF(JDOWN.EQ.2) YDEPT(J) = YNORM
C=======================================================================
C              CHECK FOR FULL PIPE OR SURCHARGE
C=======================================================================
               IF(YDEPT(J).GT.DEEP(N)) YDEPT(J) = DEEP(N)
C=======================================================================
C              CHECK FOR TIDAL INFLUENCE
C=======================================================================
               IF(NTIDE(ICASE).GT.1) THEN
               IF((YDEPT(J)+Z(J)).LT.HTIDE(I)) YDEPT(J) = HTIDE(I)-Z(J)
               ELSE
               IF((YDEPT(J)+Z(J)).LT.Z(J))     YDEPT(J) = Z(J)
               ENDIF
C=======================================================================
C              STAGE HISTORY IS SET TO INPUT DEPTH
C=======================================================================
               IF(NTIDE(ICASE).EQ.5)           YDEPT(J) = HTIDE(I)-Z(J)
  960          CONTINUE
               ENDIF
C=======================================================================
C     SET DEPTH AT TIDE GATE OR CLOSE GATE
C=======================================================================
      IF(NGATE.GT.0) THEN
               DO 1060 I = 1,NGATE
               J         = JGATE(I)
               ICASE     = JTIDES(J)
               HTIDE(I)  = HTIDES(ICASE,T)
               N         = NCHAN(J,1)
               LINK      = NCHAN(J,2)
               QP(LINK)  = QP(N)
C=======================================================================
C              CHECK FOR OUTFALL PIPE ON AN ADVERSE SLOPE
C=======================================================================
               JUP       = 1
               IF(NJUNC(N,2).EQ.J) GO TO 1010
               QP(LINK) = -QP(LINK)
               JUP      = 2
 1010          IF(H(N,JUP)-HTIDE(I)) 1020,1020,1030
C=======================================================================
C              GATE CLOSED
C#### RED, 11/29/93.
C              Define the link flow as well as the outfall pipe to have
C              zero flow.  Otherwise may have horrendous continuity 
C              errors.  Add QP(N) = 0.0.
C=======================================================================
 1020          YDEPT(J) = H(N,JUP)-Z(J)
               QP(LINK) = 0.0
               QP(N)    = 0.0
               IF(YDEPT(J).LT.0.0) YDEPT(J) = 0.0
               GO TO 1060
C=======================================================================
C              GATE OPEN
C=======================================================================
 1030          CALL DEPTHX(N,NKLASS(N),QP(N),YCRIT,YNORM)
               YDEPT(J) = AMIN1(YCRIT,YNORM)
C=======================================================================
C              CHECK FOR FULL PIPE OR SURCHARGE
C=======================================================================
               IF(YDEPT(J).GT.DEEP(N)) THEN
                           IF(JSLOT.EQ.0) YDEPT(J) = DEEP(N)
                           ENDIF
C=======================================================================
C              CHECK FOR TIDE ELEVATION
C=======================================================================
               IF(NTIDE(ICASE).GT.1) THEN
               IF((YDEPT(J)+Z(J)).LT.HTIDE(I)) YDEPT(J) = HTIDE(I)-Z(J)
               ELSE
               IF((YDEPT(J)+Z(J)).LT.Z(J))     YDEPT(J) = Z(J)
               ENDIF
C=======================================================================
C              STAGE HISTORY IS SET TO INPUT DEPTH
C=======================================================================
               IF(NTIDE(ICASE).EQ.5)           YDEPT(J) = HTIDE(I)-Z(J)
 1060          CONTINUE
               ENDIF
      RETURN
C=======================================================================
 9998 FORMAT(' SURCHARGE COEFFICIENT OF WEIR ',A10,' = ',F10.5,
     +       ' X-SEC AREA = ',F10.5,/,
     +       ' EQUIVALENT PIPE LENGTH =       ',F10.5,
     +       ' AT TIME (HOURS) = ',F10.2)
 9999 FORMAT(' SURCHARGE COEFFICIENT OF WEIR ',I9,' = ',F10.5,
     +       ' X-SEC AREA = ',F10.5,/,
     +       ' EQUIVALENT PIPE LENGTH =       ',F10.5,
     +       ' AT TIME (HOURS) = ',F10.2)
C=======================================================================
      END
