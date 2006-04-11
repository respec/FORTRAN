      SUBROUTINE OUTPUT
C=======================================================================
C     Subroutine prints output and controls the printer/plot routines.
C     WCH, 11/29/93.  Correct metric depth/diameter units to meters.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'LAB.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'HYFLOW.INC'
C=======================================================================
      DIMENSION FMAX(NPO),FMEAN(NPO),FMIN(NPO),YY(201,2)
      DIMENSION GMAX(NPO),GMEAN(NPO),GMIN(NPO),NPT(2),X(201,2),
     +          PSF(NPO,2)
      CHARACTER TP*1,VER1*10,VER2*10,VER3*10,VER4*10,
     +               YER1*10,YER2*10,YER3*10,YER4*10,BMJ*10
      CHARACTER ASTER*1,XID*1,POUND*1,DOLLAR*1
      DATA VER1/' CONDUIT  '/,VER2/' FLOW IN  '/,VER3/'   CFS    ' /,
     +     VER4/'  CU M/S  '/
      DATA YER1/' JUNCTION '/,YER2/'WATER SURF'/,YER3/' ELEV(FT) ' /,
     +     YER4/' ELEV(M)  '/
      DATA TP/' '/,ASTER/'*'/,POUND/'#'/,DOLLAR/'$'/
C=======================================================================
C     Write Junction inflows.
C=======================================================================
      WRITE(N6,5002)
      WRITE(N6,5004)
      IF(METRIC.EQ.1) WRITE(N6,5007)
      IF(METRIC.EQ.2) WRITE(N6,4995)
      SUMQIN  = 0.0
      DO 19 J = 1,NJ
      IF(QQI(J).LE.0.0) GO TO 19
      IF(JCE.EQ.0) WRITE(N6,5003)  JUN(J),QQI(J)
      IF(JCE.EQ.1) WRITE(N6,5013) AJUN(J),QQI(J)
      SUMQIN    = SUMQIN  + QQI(J)
  19  CONTINUE
      DO 20 J = 1,NJ
      IF(QOU(J).GE.0.0) GO TO 20
      IF(JCE.EQ.0) WRITE(N6,5003)  JUN(J),-QOU(J)
      IF(JCE.EQ.1) WRITE(N6,5013) AJUN(J),-QOU(J)
      SUMQIN    = SUMQIN  - QOU(J)
  20  CONTINUE
C=======================================================================
C     Write Junction outflows.
C=======================================================================
      IF(METRIC.EQ.1) WRITE(N6,5005)
      IF(METRIC.EQ.2) WRITE(N6,4998)
      SUMOUT   = 0.0
      DO 119 J = 1,NJ
      IF(QOU(J).LE.0.0) GO TO 119
      IF(JCE.EQ.0) WRITE(N6,5003)  JUN(J),QOU(J)
      IF(JCE.EQ.1) WRITE(N6,5013) AJUN(J),QOU(J)
      SUMOUT   = SUMOUT + QOU(J)
  119 CONTINUE
C=======================================================================
C     PRINT CONTINUITY SUMMARY
C=======================================================================
      IF(METRIC.EQ.1) WRITE(N6,5001) VINIT,SUMQIN,VINIT+SUMQIN
      IF(METRIC.EQ.2) WRITE(N6,4999) VINIT,SUMQIN,VINIT+SUMQIN
      IF(METRIC.EQ.1) WRITE(N6,5008) SUMOUT
      IF(METRIC.EQ.2) WRITE(N6,4997) SUMOUT
      IF(METRIC.EQ.1) WRITE(N6,5009) VLEFT,VLEFT+SUMOUT
      IF(METRIC.EQ.2) WRITE(N6,4996) VLEFT,VLEFT+SUMOUT
      SUME   = VINIT + SUMQIN
      PCTERR = (SUME - SUMOUT - VLEFT) / SUME * 100.0
      WRITE(N6,5006) PCTERR
      NOUT     = NSCRAT(1)
C=======================================================================
C     PRINT H.G.L. AND WATER DEPTH AT NODES
C=======================================================================
      IF(NHPRT.GT.0) THEN
      DO 100 I = 1,NHPRT
      FMEAN(I) = 0.0
      FMAX(I)  = 0.0
      FMIN(I)  = 1.0E30
      GMEAN(I) = 0.0
      GMAX(I)  = 0.0
      GMIN(I)  = 1.0E30
      MJPRT    = JPRT(I)
      JPRT(I)  = JUN(MJPRT)
  100 PRGEL(I) = GRELEV(MJPRT)
      DO 125 I = 1,NHPRT,5
      REWIND NOUT
      READ(NOUT,7000) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT,7000) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7000) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7000) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7010) (BMJ,J=1,NSURF)
      ENDIF
      IF(METRIC.EQ.1) WRITE(N6,5020)
      IF(METRIC.EQ.2) WRITE(N6,5030)
      WRITE(N6,5000)  ALPHA1,ALPHA2
      IT = I + 4
      IF(IT.GT.NHPRT) IT = NHPRT
      IF(JCE.EQ.0) WRITE(N6,5040) (TP,JPRT(L),L=I,IT)
      IF(JCE.EQ.1) WRITE(N6,5041) (TP,AOUT(L,1),L=I,IT)
      WRITE(N6,5060) (TP,PRGEL(L),L=I,IT)
      WRITE(N6,5061) (TP,L=I,IT)
      WRITE(N6,5062) (TP,L=I,IT)
      LT       = MIN0(I+4,NHPRT)
      IF(LTIME.GT.0) THEN
      DO 120 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),
     +                                        PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT,7020) TIME,MINUTE,JSEC,
     +                                (PSF(J,1),PSF(J,2),J=1,NSURF)
      DO 110 K = I,LT
      FMEAN(K) = FMEAN(K) + PRTH(K)
      GMEAN(K) = GMEAN(K) + PRTY(K)
      IF(PRTH(K).GT.FMAX(K)) FMAX(K) = PRTH(K)
      IF(PRTY(K).GT.GMAX(K)) GMAX(K) = PRTY(K)
      IF(PRTH(K).LT.FMIN(K)) FMIN(K) = PRTH(K)
      IF(PRTY(K).LT.GMIN(K)) GMIN(K) = PRTY(K)
 110  CONTINUE
      LTIMEH   = IFIX(TIME/3600.0)
      WRITE(N6,5080) LTIMEH,MINUTE,JSEC,(PRTH(K),PRTY(K),K=I,LT)
  120 CONTINUE
      DENOM = FLOAT(LTIME)
      ELSE
      DENOM = 1.0
      ENDIF
      WRITE(N6,5243) (FMEAN(K)/DENOM,GMEAN(K)/DENOM,K=I,LT)
      WRITE(N6,5245) (FMAX(K),GMAX(K),K=I,LT)
      WRITE(N6,5250) (FMIN(K),GMIN(K),K=I,LT)
      WRITE(N6,6666)
  125 CONTINUE
      ENDIF
C=======================================================================
C     COMPUTE AND PRINT SUMMARY STATISTICS FOR JUNCTIONS
C=======================================================================
      IBAD     = 1
      JBAD     = 1
      YMAX     = 0.0
      IXPLAN   = 0
      DO 700 J = 1,NJ
      IF(J.EQ.1.OR.(J/39*39).EQ.J) GO TO 701
      GO TO 702
  701 WRITE(N6,750)
      WRITE(N6,5000) ALPHA1,ALPHA2
      IF(METRIC.EQ.1) WRITE(N6,751)
      IF(METRIC.EQ.2) WRITE(N6,749)
C=======================================================================
C     COMPUTE FEET MAXIMUM DEPTH IS BELOW GROUND ELEVATION
C=======================================================================
  702                  FTBLG = GRELEV(J)-(DEPMAX(J)+Z(J))
C=======================================================================
C PRINT SYMBOL IF INFLOW OCCURS AT SURFACE FLOODING LOCATION
C=======================================================================
      XID = TP
      IF(FTBLG.LE.0.0) THEN
                       FTBLG = 0.0
                       DO 703 I = 1,LOCATS
                       IF(J.EQ.NLOC(I)) XID = ASTER
  703                  CONTINUE
                       DO 704 I = 1,NJSW
                       IF(J.EQ.JSW(I).AND.XID.EQ.TP) XID = POUND
                       IF(J.EQ.JSW(I).AND.XID.EQ.ASTER) XID = DOLLAR
  704                  CONTINUE
                       IF(XID.NE.TP) IXPLAN = 1
                       ENDIF
C=======================================================================
C     COMPUTE FEET OF SURCHARGE AT MAXIMUM DEPTH
C=======================================================================
                        SURMAX = DEPMAX(J)+Z(J)-ZCROWN(J)
      IF(SURMAX.LE.0.0) SURMAX = 0.0
C=======================================================================
C     PRINT JUNCTION STATISTICS
C=======================================================================
      SURLEN(J) = SURLEN(J)/60.0
      FLDLEN(J) = FLDLEN(J)/60.0
      YTOT(J)   = YTOT(J)/FLOAT(NTCYC)
      YDEV(J)   = 100.0*YDEV(J)/FLOAT(NTCYC)
      YTOT(J)   = YTOT(J)   + Z(J)
      DEPMAX(J) = DEPMAX(J) + Z(J)
      IF(YDEV(J).GT.YMAX) THEN
                          JBAD = J
                          YMAX = YDEV(J)
                          ENDIF
      IF(JCE.EQ.0) WRITE(N6,752) JUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
      IF(JCE.EQ.1) WRITE(N6,762) AJUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
  700 CONTINUE
      IF(IXPLAN.NE.0) WRITE(N6,770)
      IF(IXPLAN.NE.0) WRITE(N6,771)
C=======================================================================
C     PRINT FLOWS * VELOCITIES IN PIPES
C=======================================================================
      IF(NQPRT.GT.0) THEN
      AMULT    = FLOAT(JNTER)
      DO 140 I = 1,NQPRT
      L        = CPRT(I)
      FMEAN(I) = 0.0
      FMAX(I)  = 0.0
      FMIN(I)  = 1.0E30
      GMEAN(I) = 0.0
      GMAX(I)  = 0.0
      GMIN(I)  = 1.0E30
  140 CPRT(I)  = NCOND(L)
      DO 160 I = 1,NQPRT,5
      REWIND NOUT
      READ(NOUT,7000) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT,7000) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7000) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7000) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7010) (BMJ,J=1,NSURF)
      ENDIF
      IF(METRIC.EQ.1) WRITE(N6,5100)
      IF(METRIC.EQ.2) WRITE(N6,5101)
      WRITE(N6,5000) ALPHA1,ALPHA2
                      IT = I + 4
      IF(IT.GT.NQPRT) IT = NQPRT
      IF(JCE.EQ.0) WRITE(N6,5120) (TP,CPRT(L),L=I,IT)
      IF(JCE.EQ.1) WRITE(N6,5130) (TP,AOUT(L,2),L=I,IT)
      WRITE(N6,5121) (TP,L=I,IT)
      WRITE(N6,5122) (TP,L=I,IT)
      LT       = MIN0(I+4,NQPRT)
      IF(LTIME.GT.0) THEN
      DO 165 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                                                 J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT,7020) TIME,MINUTE,JSEC,
     +                                (PSF(J,1),PSF(J,2),J=1,NSURF)
      DO 170 K = I,LT
      FMEAN(K) = FMEAN(K) + PRTQ(K)
      GMEAN(K) = GMEAN(K) + PRTV(K)
      IF(PRTQ(K).GT.FMAX(K)) FMAX(K) = PRTQ(K)
      IF(PRTV(K).GT.GMAX(K)) GMAX(K) = PRTV(K)
      IF(PRTQ(K).LT.FMIN(K)) FMIN(K) = PRTQ(K)
      IF(PRTV(K).LT.GMIN(K)) GMIN(K) = PRTV(K)
 170  CONTINUE
      LTIMEH   = IFIX(TIME/3600.0)
      IF(METRIC.EQ.1) WRITE(N6,5140) LTIMEH,MINUTE,JSEC,
     +                               (PRTQ(K),PRTV(K),K=I,LT)
      IF(METRIC.EQ.2) WRITE(N6,5141) LTIMEH,MINUTE,JSEC,
     +                               (PRTQ(K),PRTV(K),K=I,LT)
  165 CONTINUE
      DENOM = FLOAT(LTIME)
      ELSE
      DENOM = 1.0
      ENDIF
      IF(METRIC.EQ.1) THEN
                WRITE(N6,5142) (FMEAN(K)/DENOM,GMEAN(K)/DENOM,K=I,LT)
                WRITE(N6,5145) (FMAX(K),GMAX(K),K=I,LT)
                WRITE(N6,5150) (FMIN(K),GMIN(K),K=I,LT)
                WRITE(N6,5144) (FMEAN(K)*DELT*AMULT,K=I,LT)
                ELSE
                WRITE(N6,5143) (FMEAN(K)/DENOM,GMEAN(K)/DENOM,K=I,LT)
                WRITE(N6,5146) (FMAX(K),GMAX(K),K=I,LT)
                WRITE(N6,5151) (FMIN(K),GMIN(K),K=I,LT)
                WRITE(N6,5144) (FMEAN(K)*DELT*AMULT,K=I,LT)
                ENDIF
      WRITE(N6,6666)
  160 CONTINUE
      ENDIF
C=======================================================================
C     Compute and print summary statistics for conduits.
C=======================================================================
      DO 900 N = 1,NTL
      IF(N.EQ.1.OR.MOD(N,40).EQ.0) THEN
                                   WRITE(N6,800)
                                   WRITE(N6,5000) ALPHA1,ALPHA2
                                   IF(METRIC.EQ.1) WRITE(N6,801)
                                   IF(METRIC.EQ.2) WRITE(N6,799)
                                   ENDIF
C=======================================================================
C     COMPUTE DESIGN VELOCITY AND FLOW IN CONDUIT
C     COMPUTE RATIO OF MAX TO DESIGN FLOW IN CONDUIT
C=======================================================================
      IF(N.LE.NTC) THEN
                             QRATIO = 0.0
         IF(QFULL(N).GT.0.0) QRATIO = QMAXX(N)/QFULL(N)
C=======================================================================
C        COMPUTE MAX WATER DEPTH ABOVE CONDUIT INVERT AT BOTH ENDS
C=======================================================================
         SLOPE     = (ZU(N)-ZD(N))/LEN(N)
         VDSGN     = SQRT(GRVT*SLOPE/ROUGH(N))*RFULL(N)**0.6666667
         DMAXNL    = PMAX(N,1) - ZU(N)
         DMAXNH    = PMAX(N,2) - ZD(N)
         SUPLEN(N) = SUPLEN(N)/60.0
         IF(METRIC.EQ.1) VHGHT = DEEP(N)*12.0
C#### WCH, 11/29/93.  CORRECT METRIC DIAMETER/DEPTH OUTPUT TO METERS.
         IF(METRIC.EQ.2) VHGHT = DEEP(N)
         ELSE
         SLOPE     = 1.0E20
         ENDIF
C=======================================================================
C     Print conduit statistics.
C=======================================================================
      IF(SLOPE.LT.1.0E10) THEN
               IF(JCE.EQ.0.OR.N.GT.NC) WRITE(N6,802)
     2                      NCOND(N),QFULL(N),VDSGN,VHGHT,
     2                      QMAXX(N),IQHR(N),IQMIN(N),VMAXX(N),
     2                      IVHR(N),IVMIN(N),QRATIO,DMAXNL,DMAXNH,
     2                      SUPLEN(N),SLOPE
               IF(JCE.EQ.1.AND.N.LE.NC) WRITE(N6,812)
     2                      ACOND(N),QFULL(N),VDSGN,VHGHT,
     2                      QMAXX(N),IQHR(N),IQMIN(N),VMAXX(N),
     2                      IVHR(N),IVMIN(N),QRATIO,DMAXNL,DMAXNH,
     2                      SUPLEN(N),SLOPE
               ELSE
               IF(JCE.EQ.0) WRITE(N6,803) NCOND(N),QMAXX(N),
     +                                    IQHR(N),IQMIN(N)
               IF(JCE.EQ.1) WRITE(N6,813) ACOND(N),QMAXX(N),
     +                                    IQHR(N),IQMIN(N)
               ENDIF
  900 CONTINUE
C=======================================================================
C     Print conduit link conditions.
C=======================================================================
      QMAX     = 0.0
      DO 950 N = 1,NTL
      IF(N.EQ.1.OR.MOD(N,40).EQ.0) THEN
                                   IF(METRIC.EQ.1) WRITE(N6,940)
                                   IF(METRIC.EQ.2) WRITE(N6,941)
                                   ENDIF
      QMEAN    = QTOT(N)/FLOAT(NTCYC)
      QDEV(N)  = 100.0*QDEV(N)/FLOAT(NTCYC)
      QTOT(N)  = QTOT(N)*RDELT
      IF(N.LE.NTC) THEN
      IF(QDEV(N).GT.QMAX) THEN
                          IBAD = N
                          QMAX = QDEV(N)
                          ENDIF
      IF(JCE.EQ.0) WRITE(N6,955) NCOND(N),CTIME(N,1)/60.0,
     +                           CTIME(N,2)/60.0,CTIME(N,3)/60.0,
     +                           CTIME(N,4)/60.0,QMEAN,QDEV(N),QTOT(N),
     +                           HMAX(N),AMAX(N)
      IF(JCE.EQ.1) WRITE(N6,965) ACOND(N),CTIME(N,1)/60.0,
     +                           CTIME(N,2)/60.0,CTIME(N,3)/60.0,
     +                           CTIME(N,4)/60.0,QMEAN,QDEV(N),QTOT(N),
     +                           HMAX(N),AMAX(N)
      ELSE
      IF(JCE.EQ.0) WRITE(N6,975) NCOND(N),QMEAN,QTOT(N)
      IF(JCE.EQ.1) WRITE(N6,985) ACOND(N),QMEAN,QTOT(N)
      ENDIF
  950 CONTINUE
      WRITE(N6,9400)
      IF(JCE.EQ.0) WRITE(N6,9410) NCOND(IBAD),QDEV(IBAD),
     +                                       JUN(JBAD),YDEV(JBAD)
      IF(JCE.EQ.1) WRITE(N6,9420) ACOND(IBAD),QDEV(IBAD),
     +                                       AJUN(JBAD),YDEV(JBAD)
C=======================================================================
C     Printer plot package.
C=======================================================================
      HORIZ(1) = '  CLOCK TIME IN HOURS. '
      HORIZ(2) = '                       '
      HTITLE(1)= 'PLOT OF JUNCTION ELEVATION  '
      HTITLE(2)= '                            '
C=======================================================================
C     Plot Junction water surface elevations.
C=======================================================================
      IF(NPLT.GT.0) THEN
                VERT1 = YER1
                VERT2 = YER2
                IF(METRIC.EQ.1) VERT3 = YER3
                IF(METRIC.EQ.2) VERT3 = YER4
                NPT(1)   = NPTOT
                NPT(2)   = 0
                DO 200 N = 1,NPLT
                J        = JPLT(N)
                NJUN     = JUN(J)
                CALL CURVE(TPLT,YPLT(1,N),NPT,1,NJUN,AJUN(J))
                IF(METRIC.EQ.1) WRITE(N6,2000) Z(J),ZCROWN(J),GRELEV(J)
  200           IF(METRIC.EQ.2) WRITE(N6,2001) Z(J),ZCROWN(J),GRELEV(J)
                ENDIF
C=======================================================================
C     Plot conduit flows.
C=======================================================================
      IF(LPLT.GT.0) THEN
                    HTITLE(1) = ' PLOT OF CONDUIT FLOW'
                    HTITLE(2)= '                      '
                    VERT1 = VER1
                    VERT2 = VER2
                    IF(METRIC.EQ.1) VERT3 = VER3
                    IF(METRIC.EQ.2) VERT3 = VER4
                    NPT(1)   = NPTOT
                    NPT(2)   = 0
                    DO 280 N = 1,LPLT
                    L        = KPLT(N)
                    NKON     = NCOND(L)
  280               CALL CURVE(TPLT,QPLT(1,N),NPT,1,NKON,ACOND(L))
                    ENDIF
C=======================================================================
C     Plot water surface slopes.
C=======================================================================
      IF(NSURF.GT.0.AND.LTIME.GT.0) THEN
      MP    = (LTIME+200)/200
      REWIND NOUT
      READ(NOUT,7000) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT,7000) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7000) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7000) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT,7010) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT,7010) (BMJ,J=1,NSURF)
      ENDIF
      K         = 0
      DO 6000 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT,7020) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                                                 J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT,7020) TIME,MINUTE,JSEC,
     +                                (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(MOD(L,MP).EQ.0) THEN
                         K         = K + 1
                         TPLT(K)   = TIME/3600.0
                         X(K,1)    = TIME/3600.0
                         X(K,2)    = TIME/3600.0
                         DO 6010 N = 1,NSURF
                         J         = JSURF(N)
                         N2        = 2*N
                         N1        = N2 - 1
                         YY(K,1)   = PSF(N,1)
                         YY(K,2)   = PSF(N,2)
 6010                    QPLT(K,N) = (PSF(N,2)-PSF(N,1))/LEN(J)
                         ENDIF
 6000 CONTINUE
      HTITLE(1) = 'PLOT OF JUNCTION ELEVATIONS'
      HTITLE(2) = 'AT EACH END OF THE CONDUIT '
      VERT1     = YER1
      VERT2     = YER2
      IF(METRIC.EQ.1) VERT3 = YER3
      IF(METRIC.EQ.2) VERT3 = YER4
      DO 380 N  = 1,NSURF
      L         = JSURF(N)
      NKON      = NCOND(L)
      NPT(1)    = K
      NPT(2)    = K
      CALL CURVE(X,YY,NPT,2,NKON,ACOND(L))
      WRITE(N6,9380)
  380 CONTINUE
      ENDIF
      RETURN
C=======================================================================
  749 FORMAT(' ',
     +'                     UPPERMOST    MEAN             MAXIMUM     TI
     +ME   Meters of   Meters Max.      LENGTH    LENGTH     MAXIMUM',/,
     +'            GROUND  PIPE CROWN  JUNCTION  JUNCTION JUNCTION     O
     +F     SURCHARGE    DEPTH IS         OF        OF      JUNCTION',/,
     +' JUNCTION ELEVATION  ELEVATION ELEVATION   AVERAGE   ELEV.   OCCU
     +RENCE   AT MAX    BELOW GROUND   SURCHARGE  FLOODING      AREA',/,
     6'   NUMBER     (M)        (M)       (M)    % CHANGE     (M)   HR.
     +MIN.  ELEVATION    ELEVATION      (MIN)      (MIN)    (SQ.MET)',/,
     +' -------- --------- ----------  -------- --------- --------  ----
     +-----  ---------   ------------  ---------  --------  --------')
  750 FORMAT(/,1H1,/,
     +' ************************************************************',/,
     +' *   J U N C T I O N   S U M M A R Y   S T A T I S T I C S  *',/,
     +' ************************************************************',/)
  751 FORMAT(' ',
     +'                     UPPERMOST    MEAN             MAXIMUM     TI
     +ME     FEET OF     FEET MAX.      LENGTH    LENGTH     MAXIMUM',/,
     +'            GROUND  PIPE CROWN  JUNCTION  JUNCTION JUNCTION     O
     +F     SURCHARGE    DEPTH IS         OF        OF      JUNCTION',/,
     +' JUNCTION ELEVATION  ELEVATION ELEVATION   AVERAGE   ELEV.   OCCU
     +RENCE   AT MAX    BELOW GROUND   SURCHARGE  FLOODING      AREA',/,
     6'   NUMBER    (FT)       (FT)       (FT)   % CHANGE   (FT)    HR.
     +MIN.  ELEVATION    ELEVATION      (MIN)      (MIN)     (SQ.FT)',/,
     +' -------- --------- ----------  -------- --------- --------  ----
     +-----  ---------   ------------  ---------  --------  --------')
  752 FORMAT(1X,I8,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
  762 FORMAT(1X,A8,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
  770 FORMAT(/,'WARNING.  INFLOW TO EXTRAN OCCURED AT JUNCTION THAT EXPE
     *RIENCED SURFACE FLOODING.',/,'IF THIS INFLOW OCCURED SIMULTANEOUSL
     *Y WITH FLOODING, WATER WILL NOT ENTER EXTRAN',/,'AND IS LOST FROM
     *SIMULATION EXCEPT FOR CONTINUITY CHECK.',/,'USER MAY WISH TO CONSI
     *DER EFFECTS ON UPSTREAM CHANNELS/PIPES NOT MODELED IN EXTRAN.')
  771 FORMAT('ASTERISK (*) => INFLOW AT JUNCTION WITH INFLOW FROM SWMM I
     *NTERFACE FILE.',/,'POUND (#)    => INFLOW AT JUNCTION WITH INFLOW
     *FROM K3 GROUP.',/,'DOLLAR ($)   => INFLOW AT JUNCTION WITH INFLOWS
     * FROM BOTH SOURCES.',/,'NOTE THAT CONSTANT JUNCTION INFLOWS CAN EN
     *HANCE FLOODING.')
C#### WCH, 11/29/93.  CHANGE DIAMETER/DEPTH PRINT TO METERS.
  799 FORMAT(/,
     +1X,'                             CONDUIT   MAXIMUM     TIME
     +MAXIMUM     TIME     RATIO OF   MAXIMUM DEPTH ABOVE  LENGTH CONDUI
     +T',/,
     +1X,'          DESIGN    DESIGN  VERTICAL  COMPUTED      OF      CO
     +MPUTED      OF       MAX. TO   INV. AT CONDUIT ENDS  OF NORM  SLOP
     +E',/,
     +1X,'CONDUIT     FLOW   VELOCITY   DEPTH     FLOW   OCCURRENCE    V
     +ELOCITY  OCCURRENCE   DESIGN  UPSTREAM   DOWNSTREAM    FLOW',/,
     +1X,' NUMBER    (CMS)     (M/S)      (M)    (CMS)    HR.  MIN.
     +(MPS)    HR.  MIN.    FLOW       (M)        (M)       (MIN)   (M/M
     +)',/,
     +1X,' ------  -------  --------  --------   -------  ----------   -
     +------  ----------   -------  --------    ---------   -----  -----
     +-')
  800 FORMAT(/,1H1,/,
     +' ***********************************************************',/,
     +' *   C O N D U I T   S U M M A R Y   S T A T I S T I C S   *',/,
     +' ***********************************************************',/)
  801 FORMAT(/,
     +1X,'                             CONDUIT   MAXIMUM     TIME
     +MAXIMUM     TIME     RATIO OF   MAXIMUM DEPTH ABOVE  LENGTH CONDUI
     +T',/,
     +1X,'          DESIGN    DESIGN  VERTICAL  COMPUTED      OF      CO
     +MPUTED      OF       MAX. TO   INV. AT CONDUIT ENDS  OF NORM  SLOP
     +E',/,
     +1X,'CONDUIT     FLOW   VELOCITY   DEPTH     FLOW   OCCURRENCE    V
     +ELOCITY  OCCURRENCE   DESIGN  UPSTREAM   DOWNSTREAM    FLOW',/,
     +1X,' NUMBER    (CFS)     (FPS)     (IN)    (CFS)    HR.  MIN.
     +(FPS)    HR.  MIN.    FLOW      (FT)       (FT)       (MIN) (FT/FT
     +)',/,
     +1X,' ------  -------  --------  --------   -------  ----------   -
     +------  ----------   -------  --------    ---------   -----  -----
     +-')
  802 FORMAT(' ',I7,1PE9.2,2X,0PF8.2,1X,F9.3,1X,1PE9.2,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
  803 FORMAT(' ',I7,1X,'   UNDEF',2(2X,'   UNDEF'),1X,1PE9.2,I6,I6)
  813 FORMAT(' ',A10,' UNDEF',2(2X,'   UNDEF'),1X,1PE9.2,I6,I6)
  812 FORMAT(' ',A7,1PE9.2,2X,0PF8.2,1X,F9.3,1X,1PE9.2,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
  940 FORMAT(/,1H1,/,10X,
     +' **************************************************',/,10X,
     +' * SUBCRITICAL AND CRITICAL FLOW ASSUMPTIONS FROM *',/,10X,
     +' * SUBROUTINE HEAD.  SEE FIGURE 5-4 IN THE EXTRAN *',/,10X,
     +' *       MANUAL FOR FURTHER INFORMATION.          *',/,10X,
     *' **************************************************',//,
     +'               LENGTH     LENGTH       LENGTH       LENGTH ',/,
     +'                 OF         OF        OF UPSTR. OF DOWNSTR.
     +  MEAN                   TOTAL     MAXIMUM     MAXIMUM',/,
     +'    CONDUIT     DRY     SUBCRITICAL    CRITICAL    CRITICAL
     +  FLOW     AVERAGE        FLOW   HYDRAULIC  CROSS SECT',/,
     +'     NUMBER   FLOW(MIN)  FLOW(MIN)    FLOW(MIN)   FLOW(MIN)
     + (CFS)    % CHANGE    CUBIC FT  RADIUS(FT)   AREA(FT2)',/,
     +'    -------  ---------- -----------   ---------   ---------    --
     +------   ---------   ---------  ---------   ----------')
  941 FORMAT(/,1H1,/,10X,
     +' **************************************************',/,10X,
     +' * SUBCRITICAL AND CRITICAL FLOW ASSUMPTIONS FROM *',/,10X,
     +' * SUBROUTINE HEAD.  SEE FIGURE 5-4 IN THE EXTRAN *',/,10X,
     +' *       MANUAL FOR FURTHER INFORMATION.          *',/,10X,
     *' **************************************************',//,
     +'               LENGTH     LENGTH       LENGTH       LENGTH ',/,
     +'                 OF         OF        OF UPSTR. OF DOWNSTR.
     +  MEAN                   TOTAL     MAXIMUM     MAXIMUM',/,
     +'    CONDUIT     DRY     SUBCRITICAL    CRITICAL    CRITICAL
     +  FLOW     AVERAGE        FLOW   HYDRAULIC  CROSS SECT',/,
     +'     NUMBER   FLOW(MIN)  FLOW(MIN)    FLOW(MIN)   FLOW(MIN)
     + (CMS)    % CHANGE   CUBIC MET RADIUS(MET)  AREA(SQ.M)',/,
     +'    -------  ---------- -----------   ---------   ---------    --
     +------   ---------   ---------  ---------   ----------')
  955 FORMAT(1X,I10,5F12.2,F12.4,1PE12.4,0PF12.4,F12.4)
  965 FORMAT(1X,A10,5F12.2,F12.4,1PE12.4,0PF12.4,F12.4)
  975 FORMAT(1X,I10,4('   UNDEFINED'),F12.2,12X,1PE12.4)
  985 FORMAT(1X,A10,4('   UNDEFINED'),F12.2,12X,1PE12.4)
 2000 FORMAT(/,20X,'INVERT ELEV -',F8.2,' FEET',/,20X,
     .             ' CROWN ELEV -',F8.2,' FEET',/,20X,
     .             'GROUND ELEV -',F8.2,' FEET')
 2001 FORMAT(/,20X,'INVERT ELEV -',F8.2,' METERS',/,20X,
     .             ' CROWN ELEV -',F8.2,' METERS',/,20X,
     .             'GROUND ELEV -',F8.2,' METERS')
 4995 FORMAT(4X,'JUNCTION',2X,' INFLOW, CU M',/,
     +       4X,'--------',2X,'-------------')
 4997 FORMAT(' *******************************************************',
     +     /,' * TOTAL SYSTEM OUTFLOW         = ',1PE14.4,' CU M  *')
 4996 FORMAT(' * VOLUME LEFT IN SYSTEM        = ',1PE14.4,' CU M  *',/,
     +       ' * OUTFLOW + FINAL VOLUME       = ',1PE14.4,' CU M  *',/,
     +       ' *******************************************************')
 4998 FORMAT(/,4X,'JUNCTION',2X,'OUTFLOW, CU M',/,
     +       4X,'--------',2X,'-------------')
 4999 FORMAT(/,
     1    ' *******************************************************',/,
     1       ' * INITIAL SYSTEM VOLUME        = ',1PE14.4,' CU M  *',/,
     1       ' * TOTAL SYSTEM INFLOW VOLUME   = ',1PE14.4,' CU M  *',/,
     2       ' * INFLOW + INITIAL VOLUME      = ',1PE14.4,' CU M  *')
 5000 FORMAT(/,10X,A80,/,10X,A80,/)
 5001 FORMAT(/,
     1    ' *******************************************************',/,
     1       ' * INITIAL SYSTEM VOLUME        = ',1PE14.4,' CU FT *',/,
     1       ' * TOTAL SYSTEM INFLOW VOLUME   = ',1PE14.4,' CU FT *',/,
     2       ' * INFLOW + INITIAL VOLUME      = ',1PE14.4,' CU FT *')
 5002 FORMAT(/,1H1,/,
     +' ***************************************************',/,
     +' * EXTRAN CONTINUITY BALANCE AT THE LAST TIME STEP *',/,
     +' ***************************************************',/)
 5003 FORMAT(3X,I9,1PE14.4)
 5013 FORMAT(2X,A10,1PE14.4)
 5004 FORMAT(
     +' ************************************************',/,
     +' * JUNCTION INFLOW, OUTFLOW OR STREET FLOODING *',/,
     +' ************************************************',/)
 5005 FORMAT(/,4X,'JUNCTION',2X,'OUTFLOW, FT3',/,
     +       4X,'--------',2X,'------------')
 5006 FORMAT(' * ERROR IN CONTINUITY, PERCENT = ',F14.2,'       *',/,
     +    ' *******************************************************')
 5007 FORMAT(4X,'JUNCTION',2X,' INFLOW, FT3',/,
     +       4X,'--------',2X,'------------')
 5008 FORMAT(
     +   ' *******************************************************',/,
     +       ' * TOTAL SYSTEM OUTFLOW         = ',1PE14.4,' CU FT *')
 5009 FORMAT(' * VOLUME LEFT IN SYSTEM        = ',1PE14.4,' CU FT *',/,
     +       ' * OUTFLOW + FINAL VOLUME       = ',1PE14.4,' CU FT *',/,
     +       ' *******************************************************')
 5020 FORMAT(/,1H1,/,
     +' #########################################################',/,
     +' #  T i m e  H i s t o r y  o f  t h e  H. G. L. ( Feet) #',/,
     +' #########################################################',/)
 5030 FORMAT(/,1H1,/,
     +' ##########################################################',/,
     +' #  T i m e  H i s t o r y  o f  t h e  H. G. L. (meters) #',/,
     +' ##########################################################',/)
 5040 FORMAT('          ',5(A1,' Junction:',I10))
 5041 FORMAT('          ',5(A1,' Junction:',A10))
 5060 FORMAT('    Time  ',5(A1,'   Ground:',F10.2))
 5061 FORMAT('  Hr:Mn:Sc',5(A1,' Elevation     Depth'))
 5062 FORMAT('  --------',5(A1,' ---------     -----'),/)
 5080 FORMAT (' ',I3,':',I2,':',I2,5(F11.2,F10.2))
 5100 FORMAT(/,1H1,/,
     +' #############################################',/,
     +' #     Time History of Flow and Velocity     #',/,
     +' #    Q(cfs), Vel(ft/s), Total(cubic feet)   #',/,
     +' #############################################',/)
 5101 FORMAT(/,1H1,/,
     +' #############################################',/,
     +' #     Time History of Flow and Velocity     #',/,
     +' #    Q(cms), Vel(m/s), Total(cubic meters)  #',/,
     +' #############################################',/)
 5120 FORMAT(/,'    Time  ',5(A1,' Conduit:',I10))
 5121 FORMAT('  Hr:Mn:Sc',5(A1,'     Flow    Veloc.'))
 5122 FORMAT('  --------',5(A1,'     ----    ------'))
 5130 FORMAT(/,'   Time   ',5(A1,'  Conduit:',A9))
 5140 FORMAT(1H ,I3,':',I2,':',I2,5(F10.2,F10.2))
 5141 FORMAT(1H ,I3,':',I2,':',I2,5(F10.4,F10.4))
 5142 FORMAT('     Mean ',5(F10.2,F10.2))
 5143 FORMAT('     Mean ',5(F10.4,F10.4))
 5144 FORMAT('    Total ',5(1PE10.3,10X))
 5145 FORMAT('  Maximum ',5(F10.2,F10.2))
 5146 FORMAT('  Maximum ',5(F10.4,F10.4))
 5150 FORMAT('  Minimum ',5(F10.2,F10.2))
 5151 FORMAT('  Minimum ',5(F10.4,F10.4))
 5243 FORMAT('     Mean ',5(F11.2,F10.2))
 5245 FORMAT('  Maximum ',5(F11.2,F10.2))
 5250 FORMAT('  Minimum ',5(F11.2,F10.2))
 6666 FORMAT(/)
 7000 FORMAT(200(I10,1X))
 7010 FORMAT(200(A10,1X))
 7020 FORMAT(E12.5,2I7,200(E12.5,1X))
 9380 FORMAT(/,20X,'Upstream ==> Asterisk  Downstream ===> Plus')
 9400 FORMAT(/,
     +' ************************************************************',/,
     +' * AVERAGE % CHANGE IN JUNCTION OR CONDUIT IS DEFINED AS:   *',/,
     +' * CONDUIT  % CHANGE ==> 100.0 ( Q(n+1) - Q(n) ) / Qfull    *',/,
     +' * JUNCTION % CHANGE ==> 100.0 ( Y(n+1) - Y(n) ) / Yfull    *',/,
     +' ************************************************************')
 9410 FORMAT(/,
     +'  The Conduit with the largest average change... ',I10,
     +' had',F10.3,' percent',/,
     +' The Junction with the largest average change... ',I10,
     +' had',F10.3,' percent',/)
 9420 FORMAT(/,
     +'  The Conduit with the largest average change... ',A10,
     +' had',F10.3,' percent',/,
     +' The Junction with the largest average change... ',A10,
     +' had',F10.3,' percent',/)
C=======================================================================
      END
