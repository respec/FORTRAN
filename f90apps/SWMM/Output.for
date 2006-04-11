      SUBROUTINE OUTPUT
C	EXTRAN BLOCK
C	CALLED BY EXTRAN NEAR LINE 207
C=======================================================================
C     Subroutine prints output and controls the printer/plot routines.
C     WCH, 11/29/93.  Correct metric depth/diameter units to meters.
C     WCH, 8/4/95.  Don't print junction outflows < 0.001 cf or cm.
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
      INCLUDE 'FCHAN.INC'
CIM 1/99 CHANGE FOR BA OUTPUT CONTROLS    - OUTPUT CONTROLS
      INCLUDE 'BALINES.INC'
	INCLUDE 'TRANAID.INC'
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
      IF(JCE.EQ.0) THEN
	    WRITE(N6,5003)  JUN(J),QQI(J)
	ELSE
          WRITE(N6,5013) AJUN(J),QQI(J)
	ENDIF
      SUMQIN    = SUMQIN  + QQI(J)
  19  CONTINUE
      DO 20 J = 1,NJ
      IF(QOU(J).GE.0.0) GO TO 20
      IF(JCE.EQ.0) THEN
	    WRITE(N6,5003)  JUN(J),-QOU(J)
	ELSE
          WRITE(N6,5013) AJUN(J),-QOU(J)
	ENDIF
      SUMQIN    = SUMQIN  - QOU(J)
  20  CONTINUE
C=======================================================================
C     Write Junction outflows.
C=======================================================================
      IF(METRIC.EQ.1) WRITE(N6,5005)
      IF(METRIC.EQ.2) WRITE(N6,4998)
      SUMOUT   = 0.0
      DO 119 J = 1,NJ
C#### WCH, 8/4/95.  DO NOT PRINT OUTFLOWS < 0.001 CU FT OR CU M.
C     THAT IS, INCREASE FROM CHECK FOR .LE.0 TO .LE.0.0001.
      IF(QOU(J).LE.0.0001) GO TO 119
cim try leaving 0.0
cim      IF(QOU(J).LE.0.0) GO TO 119
      IF(JCE.EQ.0) THEN
	    WRITE(N6,5003)  JUN(J),QOU(J)
	ELSE
          WRITE(N6,5013) AJUN(J),QOU(J)
	ENDIF
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
CIM TEST OF ALTERNATIVE CALCULATION OF FINAL VOLUME
C   COMPUTED BY TRACKING TOTAL VOLUME IN EACH JUNCTION
	VLEFT2 = 0.0
	DO  N   = 1,NTC
	VLEFT2 = VLEFT2 + VOL(N)
	ENDDO
	PCTERR2 = (SUME - SUMOUT - VLEFT2) / SUME * 100.0
      WRITE(N6,7000) VLEFT2,PCTERR2
cim write continuity error to console
      WRITE(*,6006) PCTERR
cim intercon  call fintercon to output results
      call fintercon
cim end
C
C******** PRINT FULL FLOW SUMMARY IF REQUIRED
C
      DO IWFA=1,NTL
       IF(IFULL(IWFA).GT.0) GO TO 47
      ENDDO
       GO TO 49
   47    WRITE(N6,5047)
      DO 48 IWF=IWFA,NTL
         IF(IFULL(IWF).GT.0) THEN
         IF (JCE.EQ.0) THEN
               WRITE(N6,5048) NCOND(IWF),NFIRST(IWF),
     *                        FHOUR(IWF),NLAST(IWF),FLHOUR(IWF)
         ELSE
               WRITE(N6,5049) ACOND(IWF),NFIRST(IWF),
     *                        FHOUR(IWF),NLAST(IWF),FLHOUR(IWF)
         ENDIF
         ENDIF
   48 CONTINUE
      WRITE(N6,5050)
   49 CONTINUE
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
cim change nout to unformatted sequential
      READ(NOUT) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (BMJ,J=1,NSURF)
      ENDIF
      IF(METRIC.EQ.1) WRITE(N6,5020)
      IF(METRIC.EQ.2) WRITE(N6,5030)
      WRITE(N6,5000)  ALPHA1,ALPHA2
      IT = I + 4
      IF(IT.GT.NHPRT) IT = NHPRT
      IF(JCE.EQ.0) THEN
	    WRITE(N6,5040) (TP,JPRT(L),L=I,IT)
	ELSE
          WRITE(N6,5041) (TP,AOUT(L,1),L=I,IT)
	ENDIF
      WRITE(N6,5060) (TP,PRGEL(L),L=I,IT)
      WRITE(N6,5061) (TP,L=I,IT)
      WRITE(N6,5062) (TP,L=I,IT)
      LT       = MIN0(I+4,NHPRT)
      IF(LTIME.GT.0) THEN
      DO 120 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),
     +                                        PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT) TIME,MINUTE,JSEC,
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
CIM 1/99  Skip printing of page headers if JHEAD = 1
CIM print first header line
      WRITE(N6,750)
      WRITE(N6,5000) ALPHA1,ALPHA2
      IF(METRIC.EQ.1) WRITE(N6,751)
      IF(METRIC.EQ.2) WRITE(N6,749)
      DO 700 J = 1,NJ
      IF(JHEAD.EQ.0.AND.MOD(J,40).EQ.0) THEN
      WRITE(N6,750)
      WRITE(N6,5000) ALPHA1,ALPHA2
      IF(METRIC.EQ.1) WRITE(N6,751)
      IF(METRIC.EQ.2) WRITE(N6,749)
      END IF
C=======================================================================
C     COMPUTE FEET MAXIMUM DEPTH IS BELOW GROUND ELEVATION
C=======================================================================
                    FTBLG = GRELEV(J)-(DEPMAX(J)+Z(J))
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
CIM CHANGE TO PRINT 10 CHARACTERS AND DIGITS
	IF (JP10.EQ.0) THEN
CIM THESE ARE ORIGINAL FORMATS
      IF(JCE.EQ.0) THEN
	    WRITE(N6,752) JUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
	ELSE
          WRITE(N6,762) AJUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
	ENDIF
	ELSE
CIM THESE ARE MODIFIED FORMATS
      IF(JCE.EQ.0) THEN
	    WRITE(N6,6752) JUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
	ELSE
          WRITE(N6,6762) AJUN(J),GRELEV(J),ZCROWN(J),
     1                   YTOT(J),YDEV(J),DEPMAX(J),IDHR(J),IDMIN(J),
     1                   SURMAX,FTBLG,XID,SURLEN(J),FLDLEN(J),ASMAXX(J)
	ENDIF
	ENDIF
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
      READ(NOUT) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (BMJ,J=1,NSURF)
      ENDIF
      IF(METRIC.EQ.1) WRITE(N6,5100)
      IF(METRIC.EQ.2) WRITE(N6,5101)
      WRITE(N6,5000) ALPHA1,ALPHA2
                      IT = I + 4
      IF(IT.GT.NQPRT) IT = NQPRT
      IF(JCE.EQ.0) THEN
	    WRITE(N6,5120) (TP,CPRT(L),L=I,IT)
	ELSE
          WRITE(N6,5130) (TP,AOUT(L,2),L=I,IT)
	ENDIF
      WRITE(N6,5121) (TP,L=I,IT)
      WRITE(N6,5122) (TP,L=I,IT)
      LT       = MIN0(I+4,NQPRT)
      IF(LTIME.GT.0) THEN
      DO 165 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                                                 J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT) TIME,MINUTE,JSEC,
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
CIM 1/99 eliminate header lines if JHEAD =1
CIM write first header line
                                   WRITE(N6,800)
                                   WRITE(N6,5000) ALPHA1,ALPHA2
CIM 1/99 MODIFY HEADER TO GO WITH EXPANDED FORMATS
      IF(JP10.EQ.0) THEN
CIM THESE ARE ORIGINAL FORMATS
                                    IF(METRIC.EQ.1) THEN 
								                 WRITE(N6,801)
	                              ELSE
                                                   WRITE(N6,799)
	                              ENDIF
	ELSE
CIM MODIFIED FORMATS
                                    IF(METRIC.EQ.1) THEN 
								                 WRITE(N6,6801)
	                              ELSE
                                                   WRITE(N6,6799)
	                              ENDIF
	ENDIF
      DO 900 N = 1,NTL
      IF(JHEAD.EQ.0.AND.MOD(N,40).EQ.0) THEN
                                   WRITE(N6,800)
                                   WRITE(N6,5000) ALPHA1,ALPHA2
	IF(JP10.EQ.0) THEN
CIM THESE ARE ORIGINAL HEADER FORMATS
                                   IF(METRIC.EQ.1) THEN 
								                 WRITE(N6,801)
	                             ELSE
                                                   WRITE(N6,799)
	                             ENDIF
	ELSE
CIM MODIFIED FORMATS
                                   IF(METRIC.EQ.1) THEN 
								                 WRITE(N6,6801)
	                             ELSE
                                                   WRITE(N6,6799)
	                             ENDIF
	ENDIF
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
CIM 1/99 WRITE ALL 10 DIGITS AND CHARACTERS
	IF(JP10.EQ.0) THEN
CIM WRITE ORIGINAL FORMAT STATEMENTS
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
	ELSE
CIM WRITE MODIFIED FORMAT STATEMENTS
      IF(SLOPE.LT.1.0E10) THEN
               IF(JCE.EQ.0.OR.N.GT.NC) WRITE(N6,6802)
     2                      NCOND(N),QFULL(N),VDSGN,VHGHT,
     2                      QMAXX(N),IQHR(N),IQMIN(N),VMAXX(N),
     2                      IVHR(N),IVMIN(N),QRATIO,DMAXNL,DMAXNH,
     2                      SUPLEN(N),SLOPE
               IF(JCE.EQ.1.AND.N.LE.NC) WRITE(N6,6812)
     2                      ACOND(N),QFULL(N),VDSGN,VHGHT,
     2                      QMAXX(N),IQHR(N),IQMIN(N),VMAXX(N),
     2                      IVHR(N),IVMIN(N),QRATIO,DMAXNL,DMAXNH,
     2                      SUPLEN(N),SLOPE
               ELSE
               IF(JCE.EQ.0) WRITE(N6,6803) NCOND(N),QMAXX(N),
     +                                    IQHR(N),IQMIN(N)
               IF(JCE.EQ.1) WRITE(N6,6813) ACOND(N),QMAXX(N),
     +                                    IQHR(N),IQMIN(N)
               ENDIF
	ENDIF
  900 CONTINUE
C=======================================================================
C     Print conduit link conditions.
C=======================================================================
      QMAX     = 0.0
CIM 1/99 ELIMINATE INTERMEDIATE HEADER LINES IF JHEAD = 1
CIM WRITE FIRST HEADER LINE HERE
                                   IF(METRIC.EQ.1) WRITE(N6,940)
                                   IF(METRIC.EQ.2) WRITE(N6,941)
      DO 950 N = 1,NTL
      IF(JHEAD.EQ.0.AND.MOD(N,40).EQ.0) THEN
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
      IF(JCE.EQ.0) THEN
	             WRITE(N6,955) NCOND(N),CTIME(N,1)/60.0,
     +                           CTIME(N,2)/60.0,CTIME(N,3)/60.0,
     +                           CTIME(N,4)/60.0,QMEAN,QDEV(N),QTOT(N),
     +                           HMAX(N),AMAX(N)
	ELSE
                   WRITE(N6,965) ACOND(N),CTIME(N,1)/60.0,
     +                           CTIME(N,2)/60.0,CTIME(N,3)/60.0,
     +                           CTIME(N,4)/60.0,QMEAN,QDEV(N),QTOT(N),
     +                           HMAX(N),AMAX(N)
	ENDIF
      ELSE
      IF(JCE.EQ.0) THEN
	    WRITE(N6,975) NCOND(N),QMEAN,QTOT(N)
	ELSE
          WRITE(N6,985) ACOND(N),QMEAN,QTOT(N)
	ENDIF
      ENDIF
  950 CONTINUE
      WRITE(N6,9400)
      IF(JCE.EQ.0) THEN
	             WRITE(N6,9410) NCOND(IBAD),QDEV(IBAD),
     +                                       JUN(JBAD),YDEV(JBAD)
	ELSE
                   WRITE(N6,9420) ACOND(IBAD),QDEV(IBAD),
     +                                       AJUN(JBAD),YDEV(JBAD)
	ENDIF
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
      READ(NOUT) JCE,NHPRT,NQPRT,NSURF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE NOUT FILE
C=======================================================================
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) READ(NOUT) (N1,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (N1,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (N1,J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) READ(NOUT) (BMJ,J=1,NHPRT)
      IF(NQPRT.GT.0) READ(NOUT) (BMJ,J=1,NQPRT)
      IF(NSURF.GT.0) READ(NOUT) (BMJ,J=1,NSURF)
      ENDIF
      K         = 0
      DO 6000 L = 1,LTIME
      IF(JPRINT.EQ.1) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                    K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT),
     +                              (PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                     K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                                                 K=1,NHPRT)
      IF(JPRINT.EQ.4) READ(NOUT) TIME,MINUTE,JSEC,(PRTY(K),PRTH(K),
     +                   K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                   J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) READ(NOUT) TIME,MINUTE,JSEC,(PRTQ(J),PRTV(J),
     +                                                 J=1,NQPRT)
      IF(JPRINT.EQ.7) READ(NOUT) TIME,MINUTE,JSEC,
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
cim 10/98  change to avoid strings that wrap around which
c          is not handled well by all compilers
  749 FORMAT(3X,
     +'                     UPPERMOST    MEAN             MAXIMUM ',
     +'    TIME   METERS OF   METERS MAX.      LENGTH    LENGTH   ',
     +'  MAXIMUM',/,2X,
     +'            GROUND  PIPE CROWN  JUNCTION  JUNCTION JUNCTION',
     +'     OF     SURCHARGE    DEPTH IS         OF        OF     ',
     +' JUNCTION',/,2X,
     +' JUNCTION ELEVATION  ELEVATION ELEVATION   AVERAGE   ELEV. ',
     +'  OCCURENCE   AT MAX    BELOW GROUND   SURCHARGE  FLOODING ',
     +'     AREA',/,2X,
     6'   NUMBER     (M)        (M)       (M)    % CHANGE     (M) ',
     +'  HR. MIN.  ELEVATION    ELEVATION      (MIN)      (MIN)   ',
     +' (SQ.MET)',/,2X,
     +' -------- --------- ----------  -------- --------- --------',
     +'  ---------  ---------   ------------  ---------  -------- ',
     +' --------')
  750 FORMAT(/,1H1,/,
     +' ************************************************************',/,
     +' *   J U N C T I O N   S U M M A R Y   S T A T I S T I C S  *',/,
     +' ************************************************************',/)
  751 FORMAT(3X,
     +'                     UPPERMOST    MEAN             MAXIMUM ',
     +'    TIME     FEET OF     FEET MAX.      LENGTH    LENGTH   ',
     +'  MAXIMUM',/,2X,
     +'            GROUND  PIPE CROWN  JUNCTION  JUNCTION JUNCTION',
     +'     OF     SURCHARGE    DEPTH IS         OF        OF     ',
     +' JUNCTION',/,2X,
     +' JUNCTION ELEVATION  ELEVATION ELEVATION   AVERAGE   ELEV. ',
     +'  OCCURENCE   AT MAX    BELOW GROUND   SURCHARGE  FLOODING ',
     +'     AREA',/,2X,
     6'   NUMBER    (FT)       (FT)       (FT)   % CHANGE   (FT)  ',
     +'  HR. MIN.  ELEVATION    ELEVATION      (MIN)      (MIN)   ',
     +'  (SQ.FT)',/,2X,
     +' -------- --------- ----------  -------- --------- --------',
     +'  ---------  ---------   ------------  ---------  -------- ',
     +' --------')
cim modify format to include 10 characters  
  752 FORMAT(1X,I8,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
  762 FORMAT(1X,A8,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
CIM WRITE ALL 10 CHARACTERS AND DIGITS
 6752 FORMAT(1X,I10,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
 6762 FORMAT(1X,A10,1X,F9.2,2X,F9.2,F10.2,1X,F9.4,F9.2,I6,I5,3X,F8.2,
     2       4X,F10.2,A1,1X,F9.1,2X,F8.1,1PE11.3)
  770 FORMAT(/,5X,'WARNING.  INFLOW TO EXTRAN OCCURED AT JUNCTION THAT',
     *' EXPERIENCED SURFACE FLOODING.',/,
     *5X,'IF THIS INFLOW OCCURED SIMULTANEOUSLY WITH FLOODING, WATER W',
     *'ILL NOT ENTER EXTRAN',/,
     *5X,'AND IS LOST FROM SIMULATION EXCEPT FOR CONTINUITY CHECK.',/,
     *5X,'USER MAY WISH TO CONSIDER EFFECTS ON UPSTREAM CHANNELS/PIPES',
     *' NOT MODELED IN EXTRAN.')
  771 FORMAT(/,5X,'ASTERISK (*) => INFLOW AT JUNCTION WITH INFLOW FROM',
     *' SWMM INTERFACE FILE.',/,
     *5X,'POUND (#)    => INFLOW AT JUNCTION WITH INFLOW FROM K3 GROUP',
     *'.',/,
     *5X,'DOLLAR ($)   => INFLOW AT JUNCTION WITH INFLOWS FROM BOTH SO',
     *'URCES.',//,
     *5X,'NOTE THAT CONSTANT JUNCTION INFLOWS CAN ENHANCE FLOODING.')
  800 FORMAT(/,1H1,/,
     +' ***********************************************************',/,
     +' *   C O N D U I T   S U M M A R Y   S T A T I S T I C S   *',/,
     +' ***********************************************************',/)
C#### WCH, 11/29/93.  CHANGE DIAMETER/DEPTH PRINT TO METERS.
  799 FORMAT(/,
     +1X,'                             CONDUIT   MAXIMUM     TIME  ',
     +'     MAXIMUM    TIME      RATIO OF   MAXIMUM DEPTH ABOVE ',
     +' LENGTH CONDUIT',/,
     +1X,'          DESIGN    DESIGN  VERTICAL  COMPUTED      OF   ',
     +'   COMPUTED      OF       MAX. TO   INV. AT CONDUIT ENDS ',
     +' OF NORM  SLOPE',/,
     +1X,'CONDUIT     FLOW   VELOCITY   DEPTH     FLOW   OCCURRENCE',
     +'    VELOCITY  OCCURRENCE   DESIGN  UPSTREAM   DOWNSTREAM ',
     +'   FLOW',/,
     +1X,' NUMBER    (CMS)     (M/S)      (M)    (CMS)    HR.  MIN.',
     +'     (MPS)    HR.  MIN.    FLOW       (M)        (M)     ',
     +'  (MIN)   (M/M)',/,
     +1X,' ------  -------  --------  --------   -------  ---------',
     +'-   -------  ----------   -------  --------    --------- ',
     +'  -----  ------')
  801 FORMAT(/,
     +1X,'                             CONDUIT   MAXIMUM     TIME   ',
     +'    MAXIMUM    TIME      RATIO OF   MAXIMUM DEPTH ABOVE ',
     +' LENGTH CONDUIT',/,
     +1X,'          DESIGN    DESIGN  VERTICAL  COMPUTED      OF    ',
     +'  COMPUTED      OF       MAX. TO   INV. AT CONDUIT ENDS ',
     +' OF NORM  SLOPE',/,
     +1X,'CONDUIT     FLOW   VELOCITY   DEPTH     FLOW   OCCURRENCE ',
     +'   VELOCITY  OCCURRENCE   DESIGN  UPSTREAM   DOWNSTREAM ',
     +'   FLOW',/,
     +1X,' NUMBER    (CFS)     (FPS)     (IN)    (CFS)    HR.  MIN. ',
     +'    (FPS)    HR.  MIN.    FLOW      (FT)       (FT)     ',
     +'  (MIN) (FT/FT)',/,
     +1X,' ------  -------  --------  --------   -------  ----------',
     +'   -------  ----------   -------  --------    --------- ',
     +'  -----  ------')
 6801	FORMAT(/,
     1'                                   CONDUIT   MAXIMUM  ',
     2'     TIME      MAXIMUM      TIME   RATIO OF  MAXIMUM ',
     3'DEPTH ABOVE    LENGTH  CONDUIT',/,
     4'               DESIGN    DESIGN   VERTICAL   COMPUTED ',
     5'      OF       COMPUTED      OF     MAX. TO  INV. AT ',
     6'CONDUIT ENDS  OF NORM   SLOPE ',/,
     7'   CONDUIT      FLOW    VELOCITY     DEPTH     FLOW   ',
     8'  OCCURRENCE   VELOCITY  OCCURRENCE DESIGN  UPSTREAM ',
     9'  DOWNSTREAM    FLOW          ',/,
     1'   NUMBER       (CFS)     (F/S)      (IN)     (CFS)   ',
     2'   HR.  MIN.    (F/S)    HR.  MIN.    FLOW      (FT) ',
     3'      (FT)      (MIN)  (FT/FT)',/,
     4' ----------  ---------- -------- --------- -----------',
     5' ----- ----- --------- ----- ----- -------- -------- ',
     6'----------- --------- -------- ')
 6799	FORMAT(/,
     1'                                   CONDUIT   MAXIMUM  ',
     2'     TIME      MAXIMUM      TIME   RATIO OF  MAXIMUM ',
     3'DEPTH ABOVE    LENGTH  CONDUIT',/,
     4'               DESIGN    DESIGN   VERTICAL   COMPUTED ',
     5'      OF       COMPUTED      OF     MAX. TO  INV. AT ',
     6'CONDUIT ENDS  OF NORM   SLOPE ',/,
     7'   CONDUIT      FLOW    VELOCITY     DEPTH     FLOW   ',
     8'  OCCURRENCE   VELOCITY  OCCURRENCE DESIGN  UPSTREAM ',
     9'  DOWNSTREAM    FLOW          ',/,
     1'   NUMBER       (CMS)     (M/S)       (M)     (CMS)   ',
     2'   HR.  MIN.    (M/S)    HR.  MIN.    FLOW      (M)  ',
     3'      (M)       (MIN)   (M/M) ',/,
     4' ----------  ---------- -------- --------- -----------',
     5' ----- ----- --------- ----- ----- -------- -------- ',
     6'----------- --------- -------- ')
cim original formats
  802 FORMAT(' ',I7,1PE9.2,2X,0PF8.2,1X,F9.3,1X,1PE9.2,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
  803 FORMAT(' ',I7,1X,'   UNDEF',2(2X,'   UNDEF'),1X,1PE9.2,I6,I6)
  813 FORMAT(' ',A10,' UNDEF',2(2X,'   UNDEF'),1X,1PE9.2,I6,I6)
CWCH, 9/27/99.  Increase conduit ID field to 8 for now (to A8 from A7)
  812 FORMAT(' ',A8,1PE9.2,2X,0PF8.2,1X,F9.3,1X,1PE9.2,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
cim revised formats
 6802 FORMAT(1X,I10,1X,1PE11.4,1X,0PF8.2,1X,F9.3,1X,1PE11.4,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
 6803 FORMAT(1X,I10,7X,'UNDEF',4X,'UNDEF',5X,'UNDEF',1X,1PE11.4,I6,I6)
 6813 FORMAT(1X,A10,7X,'UNDEF',4X,'UNDEF',5X,'UNDEF',1X,1PE11.4,I6,I6)
 6812 FORMAT(1X,A10,1X,1PE11.4,1X,0PF8.2,1X,F9.3,1X,1PE11.4,I6,I6,2X,
     +             0PF8.2,I6,I6,2(1X,F8.2),4X,F8.2,3X,F7.1,2X,F7.5)
cim
  940 FORMAT(/,1H1,/,10X,
     +' **************************************************',/,10X,
     +' * SUBCRITICAL AND CRITICAL FLOW ASSUMPTIONS FROM *',/,10X,
     +' * SUBROUTINE HEAD.  SEE FIGURE 5-4 IN THE EXTRAN *',/,10X,
     +' *       MANUAL FOR FURTHER INFORMATION.          *',/,10X,
     *' **************************************************',//,
     +'               LENGTH     LENGTH       LENGTH       LENGTH ',/,
     +'                 OF         OF        OF UPSTR. OF DOWNSTR.  ',
     +'      MEAN                   TOTAL     MAXIMUM     MAXIMUM',/,
     +'    CONDUIT     DRY     SUBCRITICAL    CRITICAL   CRITICAL   ',
     +'      FLOW     AVERAGE        FLOW   HYDRAULIC  CROSS SECT',/,
     +'     NUMBER   FLOW(MIN)  FLOW(MIN)    FLOW(MIN)   FLOW(MIN)  ',
     +'     (CFS)    % CHANGE    CUBIC FT  RADIUS(FT)   AREA(FT2)',/,
     +'    -------  ---------- -----------   ---------   ---------  ',
     +'  --------   ---------   ---------  ---------   ----------')
  941 FORMAT(/,1H1,/,10X,
     +' **************************************************',/,10X,
     +' * SUBCRITICAL AND CRITICAL FLOW ASSUMPTIONS FROM *',/,10X,
     +' * SUBROUTINE HEAD.  SEE FIGURE 5-4 IN THE EXTRAN *',/,10X,
     +' *       MANUAL FOR FURTHER INFORMATION.          *',/,10X,
     *' **************************************************',//,
     +'               LENGTH     LENGTH       LENGTH       LENGTH ',/,
     +'                 OF         OF        OF UPSTR. OF DOWNSTR.  ',
     +'      MEAN                   TOTAL     MAXIMUM     MAXIMUM',/,
     +'    CONDUIT     DRY     SUBCRITICAL    CRITICAL    CRITICAL  ',
     +'      FLOW     AVERAGE        FLOW   HYDRAULIC  CROSS SECT',/,
     +'     NUMBER   FLOW(MIN)  FLOW(MIN)    FLOW(MIN)   FLOW(MIN)  ',
     +'     (CMS)    % CHANGE   CUBIC MET RADIUS(MET)  AREA(SQ.M)',/,
     +'    -------  ---------- -----------   ---------   ---------  ',
     +'  --------   ---------   ---------  ---------   ----------')
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
 5003 FORMAT(2X,I10,1PE14.4)
 5013 FORMAT(2X,A10,1PE14.4)
 5004 FORMAT(
     +' ************************************************',/,
     +' * JUNCTION INFLOW, OUTFLOW OR STREET FLOODING *',/,
     +' ************************************************',/)
 5005 FORMAT(/,4X,'JUNCTION',2X,'OUTFLOW, FT3',/,
     +       4X,'--------',2X,'------------')
 5006 FORMAT(' * ERROR IN CONTINUITY, PERCENT = ',F14.2,'       *',/,
     +    ' *******************************************************')
 7000 FORMAT(//,
     1' TEST WRITE OF ALTERNATIVE CONTINUITY ERROR CALCULATION',/,
     1' VOLUME LEFT IN SYSTEM       = ',1PE14.4,' CU. FT.',/,
	1' ERROR IN CONTINUITY PERCENT = ',F14.2)
 6006 FORMAT(' ===> Continuity error (percent): ',F10.2)
  311 FORMAT(' ===> Elapsed Time  (minutes)   : ',F24.3)
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
 5047 FORMAT(//,16(2H- ),' SUMMARY OF FULL FLOW CHANNEL WARNINGS ',
     *21(2H- ),//,
     *'   OPEN CHANNEL   TIME STEP OF FIRST   TIME OF FIRST',
     *'   TIME STEP OF LAST   TIME OF LAST',/,
     *'      NUMBER          OCCURRENCE        OCCURRENCE  ',
     *'      OCCURRENCE        OCCURRENCE',/,
     *42X,'(HOURS)',28X,'(HOURS)',/,
     *'   ------------   ------------------   -------------',
     *'   -----------------   ------------')
 5048 FORMAT(1X,I10,13X,I5,13X,F6.2,13X,I5,11X,F6.2)
 5049 FORMAT(1X,A10,13X,I5,13X,F6.2,13X,I5,11X,F6.2)
 5050 FORMAT(/,
     A3X,'THE PROGRAM USES FULL DEPTH CHANNEL CHARACTERISTICS',
     A' TO COMPUTE FLOW THROUGH THE TRAPEZOIDAL, IRREGULAR, OR ',/,
     B3X,'PARABOLIC/POWER FUNCTION CONDUIT WHEN THE COMPUTED DEPTHS ',
     C'EXCEED MAXIMUM DEPTH.  THIS WILL AFFECT THE MAXIMUM',/,
     D3X,'COMPUTED HEAD AND FLOWS IN THE MODEL.  IT IS HIGHLY ',
     E'RECOMMENDED THAT THE MODELED CROSS SECTIONS BE EXTENDED',/,
     F3X,'TO ELIMINATE THESE FULL FLOW CHANNEL WARNINGS')
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
 5130 FORMAT(/,'   Time   ',5(A1,' Conduit:',A10))
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
cim 7000 FORMAT(200(I10,1X))
cim 7010 FORMAT(200(A10,1X))
cim 7020 FORMAT(E12.5,2I7,200(E12.5,1X))
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
