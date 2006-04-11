      SUBROUTINE TRANSX
C=======================================================================
C     Subroutine TRANSX of the Sewer model calls subroutines
C     XROUTE, YROUTE AND ZROUTE for the solution of the link
C     momentum equation and the nodal continuity equation.
C     Modified 7/14/93 by WCH based on CDM memo from Chuck Moore
C       to correct for equality check for surface flooding.
C     Slight format change for NSCRAT1 error message, WCH, 11/5/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'VOLDAT.INC'
      DIMENSION QOUT(NEE),VTEMP(NEE),BADTIM(NEE),BADMAX(NEE),PSF(NPO,2)
      LOGICAL FAIL
C=======================================================================
C     Define Statement function for linear interpolation.
C=======================================================================
      QLINTP(Q1,Q2,T2,T) = Q1 + (Q2-Q1)*T/T2
C=======================================================================
C     Initialization.
C=======================================================================
      ICYC       = 0
      LTIME      = 0
      NPTOT      = 0
      MTIME      = 0
      NERROR     = 0
      ITMXXX     = 0
      ITTOTL     = 0
      JTIME      = 0
      NUSE       = 4
      SUMERR     = 0.0
      NOUT       = NSCRAT(1)
      NREDO      = NSCRAT(2)
      XDELT      = RDELT
      YOOMIN     = RDELT
      YOOMAX     = 0.0
      T2         = RDELT/3600.0
      FMAX       = 3.0
      FMIN       = 1.0
      FACT       = 3.0
      AFACT      = 0.0
      ITURN      = 0
C=======================================================================
C     Calculate JPRINT for output arrays.
C=======================================================================
      IF(NHPRT.GT.0.AND.NQPRT.GT.0.AND.NSURF.GT.0) JPRINT = 1
      IF(NHPRT.GT.0.AND.NQPRT.GT.0.AND.NSURF.EQ.0) JPRINT = 2
      IF(NHPRT.GT.0.AND.NQPRT.EQ.0.AND.NSURF.EQ.0) JPRINT = 3
      IF(NHPRT.GT.0.AND.NQPRT.EQ.0.AND.NSURF.GT.0) JPRINT = 4
      IF(NHPRT.EQ.0.AND.NQPRT.GT.0.AND.NSURF.GT.0) JPRINT = 5
      IF(NHPRT.EQ.0.AND.NQPRT.GT.0.AND.NSURF.EQ.0) JPRINT = 6
      IF(NHPRT.EQ.0.AND.NQPRT.GT.0.AND.NSURF.GT.0) JPRINT = 7
C=======================================================================
C     Save Junction and conduit names or numbers on NOUT.
C=======================================================================
      REWIND NOUT
      WRITE(NOUT,7000) JCE,NHPRT,NQPRT,NSURF
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) WRITE(NOUT,7000) (JUN(JPRT(J)),J=1,NHPRT)
      IF(NQPRT.GT.0) WRITE(NOUT,7000) (NCOND(CPRT(J)),J=1,NQPRT)
      IF(NSURF.GT.0) WRITE(NOUT,7000) (NCOND(JSURF(J)),J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) WRITE(NOUT,7010) (AJUN(JPRT(J)),J=1,NHPRT)
      IF(NQPRT.GT.0) WRITE(NOUT,7010) (ACOND(CPRT(J)),J=1,NQPRT)
      IF(NSURF.GT.0) WRITE(NOUT,7010) (ACOND(JSURF(J)),J=1,NSURF)
      ENDIF
C=======================================================================
C     Read hot-start file NREDO.
C=======================================================================
      IF(NREDO.GT.0) THEN
         REWIND NREDO
         IF(JREDO.EQ.1.OR.JREDO.EQ.3) THEN
             WRITE(N6,2100)
             READ(NREDO,ERR=8888,END=8888) MTL,NJJ,NPP
             READ(NREDO,ERR=8888) (Q(N),N=1,MTL),(QO(N),N=1,MTL)
             READ(NREDO,ERR=8888) (V(N),N=1,MTL),(VT(N),N=1,MTL)
             READ(NREDO,ERR=8888) (A(N),N=1,MTL),(AT(N),N=1,MTL)
             READ(NREDO,ERR=8888) (ASFULL(N),N=1,NJJ)
             READ(NREDO,ERR=8888) (Y(N),N=1,NJJ),(YT(N),N=1,NJJ)
             READ(NREDO,ERR=8888) (VWELL(N),N=1,NPP),(JPFUL(N),N=1,NPP)
             GO TO 666
             ENDIF
         ENDIF
C=======================================================================
C     Calculate initial volume in conduits(VINIT), use average of
C     YNORM's at Junctions unless initial Junction depths are specified.
C=======================================================================
      DO 10 J  = 1,NJ
      QQI(J)   = 0.0
      IF(Y(J).GT.0.01) GO TO 10
      DO 20 JJ = 1,8
      IF(NCHAN(J,JJ).EQ.0) GO TO 11
      N       = NCHAN(J,JJ)
      IF(QO(N).NE.0.0) THEN
                       CALL DEPTHX(N,NKLASS(N),QO(N),YC,YNORM)
                       Y(J)    = Y(J) + YNORM
                       ENDIF
   20 CONTINUE
   11 Y(J) = Y(J)/FLOAT(JJ-1)
   10 CONTINUE
C=======================================================================
C     Initialize AT,VT,Q,Y arrays
C     calculate initial system volume (VINIT).
C=======================================================================
  666 DO 25 N   = 1,NTC
      BADTIM(N) = 0.0
      BADMAX(N) = 0.0
      AOVERB(N) = DEEP(N)
      SLOPE     = (ZU(N)-ZD(N))/LEN(N)
      IF(SLOPE.EQ.0.0) SLOPE = 0.01/LEN(N)
      VDSGN     = SQRT(GRVT*SLOPE/ROUGH(N))*RFULL(N)**0.6666667
      QFULL(N)  = AFULL(N)*VDSGN
      NL        = NJUNC(N,1)
      NH        = NJUNC(N,2)
      H(N,1)    = Y(NL) + Z(NL)
      H(N,2)    = Y(NH) + Z(NH)
      IF(JREDO.EQ.0.OR.JREDO.EQ.2) THEN
                       CALL HEAD(N,NL,NH,H(N,1),H(N,2),QO(N),
     +                             A(N),V(N),HRAD,ANH,ANL,RNL,RNH,
     +                             IDOIT,LINK(N))
                                   VT(N)     = V(N)
                                   AT(N)     = A(N)
                                   Q(N)      = QO(N)
                                   ELSE
                       CALL HEAD(N,NL,NH,H(N,1),H(N,2),QT(N),
     +                             AT(N),VT(N),HRAD,ANH,ANL,RNL,RNH,
     +                             IDOIT,LINK(N))
                                   ENDIF
      YO(N)     = HRAD
  25  VINIT     = VINIT + (ANL+ANH)*LEN(N)/2.0
C 25  IF(NKLASS(N).LE.8) VINIT = VINIT + (ANL+ANH)*LEN(N)/2.0
C=======================================================================
C     Write initial information.
C=======================================================================
      WRITE(N6,1598) TIME/3600.0
      WRITE(N6,1501)
      IF(JCE.EQ.0) WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
      IF(JCE.EQ.1) WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
      WRITE(N6,1503)
      IF(JCE.EQ.0) WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
      IF(JCE.EQ.1) WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
      WRITE(N6,1506)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),V(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),V(N),N=1,NTC)
      WRITE(N6,1507)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),A(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),A(N),N=1,NTC)
      WRITE(N6,1508)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),YO(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),YO(N),N=1,NTC)
      WRITE(N6,1509)
      IF(JCE.EQ.0) WRITE(N6,1517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
C=======================================================================
C     Calculate QFULL for each orifice.
C=======================================================================
       IF(NORIF.GT.0) THEN
                      DO 35 I  = 1,NORIF
                      N        = NC + I
                      QFULL(N) = AORIF(I)*SQRT(2.0*GRVT*DEEP(N))
     +                                                 *CORIF(I)
   35                 CONTINUE
                      ENDIF
C=======================================================================
C     Calculate YCROWN for each Junction.
C=======================================================================
      DO 30 J   = 1,NJ
      IF(ZCROWN(J).EQ.Z(J)) ZCROWN(J) = Z(J) + 1.0
                                   YCROWN(J) = ZCROWN(J)-Z(J)
      IF(GRELEV(J)-ZCROWN(J).GT.0) YCROWN(J) = 0.96*(ZCROWN(J)-Z(J))
   30 YT(J)     = Y(J)
C=======================================================================
C     New Computation of Volume for Irregular Storage Elements.
C=======================================================================
      DO 70 I = 1,NSTORE
      J       = JSTORE(I)
      IF(ASTORE(I).GE.0.0) THEN
                         VINIT  = VINIT + Y(J)*ASTORE(I)
                         ELSE IF(NUMV(I).LT.0) THEN
                         IF(Y(J).EQ.0.0) Y(J) = 0.0001
                         VINIT = VINIT +
     +                           VCURVE(I,1,1)*Y(J)**VCURVE(I,2,1)
                         ELSE
                         NTOX       = NUMV(I)
                         RESELV     = Y(J)
                         VINIT      = VINIT + RESELV*AMEN
                         DO 1891 IX = 1,NTOX
 1891                    IF(RESELV.LT.VCURVE(I,2,IX)) GO TO 1892
 1892                    DELTA = (RESELV-VCURVE(I,2,IX-1))/
     +                           (VCURVE(I,2,IX)-VCURVE(I,2,IX-1))
                         VINIT = VINIT+DELTA*(VCURVE(I,3,IX) -
     +                           VCURVE(I,3,IX-1))+VCURVE(I,3,IX-1)
                         ENDIF
   70 CONTINUE
C=======================================================================
C     Write first line of output interface file.
C=======================================================================
      IF(NEXT.GT.0) THEN
                    BELT = 0.0
                    WRITE(NEXT) JULDAY,TIMDAY,BELT,(QOUT(N),N=1,MJSW)
                    ENDIF
C=======================================================================
C     Major Program loop through time.
C=======================================================================
      WRITE(*,23)
      WRITE(*,24)   NJ,NTL,NTCYC
      MP          = (NTCYC+200)/200 + 1
      DO 750 MCY  = 1,NTCYC,MP
      NPTOT       = NPTOT + 1
      IF(MTIME+MP.GT.NTCYC) MP = NTCYC - MTIME
      DO 640 MCYY = 1,MP
C=======================================================================
C     Calculate and update time.
C=======================================================================
      LIT         = 0
      MTIME       = MTIME + 1
      ICYC        = ICYC  + 1
      TIMEX       = TIME
      JLAST       = JULDAY
      TLAST       = TIMDAY
      TIME        = TIME + DELT
      TIME2       = TIME - DELT/2.0
      CALL STIME(DELT)
      CALL DATED
C=======================================================================
C     Surcharge parameters.
C=======================================================================
      ERROR = 0.0
      TOL   = 1.0
      IT    = 0
      NSUR  = 1
      CALL INFLOW
C=======================================================================
C     Save beginning time step values.
C=======================================================================
      MSURGE    = 0
      DO 9000 J = 1,NJ
      IF(ISOL.GE.2) THEN
                    YT(J)    = Y(J)
                    VTEMP(J) = VWELL(J)
                    ENDIF
 9000 IF(Y(J).GT.YCROWN(J)) MSURGE = MSURGE + 1
      MNORM     = 0
      YDELT     = XDELT/FACT
      XDELT     = RDELT
      DO 9010 N = 1,NTC
      NL        = NJUNC(N,1)
      NH        = NJUNC(N,2)
      IF(ISOL.GE.2) QT(N)  = Q(N)
      IF(NKLASS(N).GE.5.AND.NKLASS(N).LT.9) THEN
                            DIFF   = AOVERB(N)
                            ELSE
                            DIFF1 = H(N,1) - ZU(N)
                            DIFF2 = H(N,2) - ZD(N)
                            DIFF  = AMAX1(DIFF1,DIFF2)
                            ENDIF
      VEL       = ABS(V(N))
      IF(VEL.GT.0.0)  THEN
                      IF(DIFF.GT.0.0) VEL = VEL + SQRT(GRVT*DIFF)
                                      VEL = LEN(N)/VEL
                      IF(ISOL.GE.2) THEN
                          BADTIM(N) = BADTIM(N) + VEL*FLOAT(NUSE)
                          IF(YDELT.GE.VEL) BADMAX(N) = BADMAX(N) + RDELT
                          IF(VEL.LT.XDELT) XDELT = VEL
                          ELSE
                          IF(RDELT.GE.VEL) BADMAX(N) = BADMAX(N)+RDELT
                                           BADTIM(N) = BADTIM(N)+VEL
                          ENDIF
                      ELSE
                      IF(ISOL.LE.1) BADTIM(N)=BADTIM(N)+RDELT
                      IF(ISOL.GE.2) BADTIM(N)=BADTIM(N)+VEL*FLOAT(NUSE)
                      ENDIF
 9010 IF(ICHECK(N).EQ.IND(2)) MNORM = MNORM + 1
                       FACT  = FMAX
      IF(MSURGE.GE.1)  FACT  = FMIN
                      XDELT  = XDELT*FACT
      IF(MTIME.GT.1)  THEN
      IF(JCE.EQ.0) WRITE(*,22) MTIME,ITTOTL,MNORM,MSURGE,NCOND(IBAD),
     +                         QMAX,JUN(JBAD),YMAX
      IF(JCE.EQ.1) WRITE(*,21) MTIME,ITTOTL,MNORM,MSURGE,ACOND(IBAD),
     +                         QMAX,AJUN(JBAD),YMAX
      ELSE
      IF(XDELT.EQ.RDELT) XDELT = RDELT/4.0
      IF(JCE.EQ.0)  WRITE(*,22) MTIME,ITTOTL,MNORM,MSURGE
      IF(JCE.EQ.1)  WRITE(*,21) MTIME,ITTOTL,MNORM,MSURGE
      ENDIF
C=======================================================================
C     Call modified EULER solution's.
C=======================================================================
      IF(ISOL.EQ.0) THEN
                    CALL XROUTE
                    LIT = IT + 2
                    ENDIF
      IF(ISOL.EQ.1) THEN
                    CALL YROUTE
                    LIT = IT + 2
                    ENDIF
C=======================================================================
C      Call the Iterative solution.  Determine the time step size based
C      on the shortest conduit and the previous maximum velocity.
C=======================================================================
      IF(ISOL.GE.2) THEN
                    FAIL      = .FALSE.
                    IPICK     = 0
                    N1        = 1
                    CDELT     = 0.0
                    IF(XDELT.EQ.0.0) THEN
                                     NUSE   = 4
                                     ELSE
                                     NUSE   = IFIX(RDELT/XDELT)
                                     IF(NUSE.LE.0) NUSE = 1
                                     ENDIF
 8999               CONTINUE
                    DELT      = RDELT/FLOAT(NUSE)
                    DELT2     = DELT/2.0
                    IF(DELT.LT.YOOMIN) YOOMIN = DELT
                    IF(DELT.GT.YOOMAX) YOOMAX = DELT
                    TIME      = TIMEX
                    JULDAY    = JLAST
                    TIMDAY    = TLAST
                    DO 9040 N = N1,NUSE
                    IT        = 1
                    TIME2     = TIME + DELT/2.0
                    TIME      = TIME + DELT
                    CALL STIME(DELT)
                    CALL DATED
                    DO 9045 J = 1,NJ
                    QQ1       = QINN(J,1)
                    QQ2       = QINN(J,2)
                    T         = FLOAT(N)*DELT/3600.0
                    T1        = FLOAT(N-1)*DELT/3600.0
                    QIN(J)    = QLINTP(QQ1,QQ2,T2,T1)/2.0 +
     +                          QLINTP(QQ1,QQ2,T2,T)/2.0  + QINST(J)
 9045               CONTINUE
                    CALL ZROUTE(FAIL)
                    IF(FAIL.AND.IPICK.LE.1) THEN
                                DO 9055 J = 1,NJ
                                VWELL(J)  = VTEMP(J)
 9055                           Y(J)      = YT(J)
                                DO 9056 M = 1,NTC
 9056                           Q(M)      = QT(M)
                                IPICK     = IPICK + 1
                                LIT       = LIT   + IT
                                ITURN     = ITURN + 1
                                N1        = 2*(N-1)+ 1
                                IF(N.EQ.1) N1 = 1
                                NUSE      = NUSE*2
                                FACT      = FACT/2.0
                                GO TO 8999
                                ENDIF
                    LIT       = LIT   + IT
                    AFACT     = AFACT + FACT
                    CDELT     = CDELT + DELT
                    JTIME     = JTIME + 1
                    TIMEX     = TIME
                    JLAST     = JULDAY
                    TLAST     = TIMDAY
                    IF(ABS(ERROR).GT.SUMERR) SUMERR=ABS(ERROR)
                    DO 9050 J = 1,NJ
                    IF(QIN(J).LT.0.0) QOU(J) = QOU(J) - QIN(J)*DELT
                    IF(QIN(J).GE.0.0) QQI(J) = QQI(J) + QIN(J)*DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
                    IF(Y(J).GE.GRELEV(J)-Z(J)-0.000001) 
     *                  QOU(J) = QOU(J)  +  SUMQ(J)*DELT
 9050               IF(QIN(J).GE.0.0) SUMQIN = SUMQIN + QIN(J)*DELT
                    I         = 0
                    DO 9060 M = NTC+1,NTL
                    J         = NJUNC(M,1)
                    IF(NJUNC(M,2).LE.0) THEN
                                  QOU(J)  = QOU(J) + Q(M)*DELT
                                  I       = I+1
                                  QOUT(I) = Q(M)
                                  ENDIF
 9060               CONTINUE
C=======================================================================
C     Check for maximum flow and velocity in conduits.
C=======================================================================
                    DO 9070 M = 1,NTL
                    IF(M.LE.NTC.AND.ICHECK(M).EQ.IND(2))
     +                              SUPLEN(M) = SUPLEN(M) + DELT
                    IF(ABS(Q(M)).GT.ABS(QMAXX(M))) THEN
                                        QMAXX(M) = Q(M)
                                        IQHR(M)  = IFIX(TIME/3600.0)
                                        IQMIN(M) = MINUTE
                                        ENDIF
                    IF(MTIME.GT.1.AND.ABS(V(M)).GT.ABS(VMAXX(M))) THEN
                                        VMAXX(M) = V(M)
                                        IVHR(M)  = IFIX(TIME/3600.0)
                                        IVMIN(M) = MINUTE
                                        ENDIF
                   IF(M.LE.NTC) THEN
                   IF(H(M,1).GT.PMAX(M,1)) PMAX(M,1) = H(M,1)
                   IF(H(M,2).GT.PMAX(M,2)) PMAX(M,2) = H(M,2)
                   IF(LINK(M).EQ.'DR1') CTIME(M,1)   = CTIME(M,1) + DELT
                   IF(LINK(M).EQ.'SUB') CTIME(M,2)   = CTIME(M,2) + DELT
                   IF(LINK(M).EQ.'CR1') CTIME(M,3)   = CTIME(M,3) + DELT
                   IF(LINK(M).EQ.'CR2') CTIME(M,4)   = CTIME(M,4) + DELT
                   ENDIF
 9070              CONTINUE
C=======================================================================
C     Check for surcharge and maximum depth at junctions.
C=======================================================================
           DO 9080 J = 1,NJ
           IF((Z(J)+Y(J)).GT.ZCROWN(J)) SURLEN(J) = SURLEN(J)+DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
           IF((Z(J)+Y(J)).GE.GRELEV(J)-0.000001) 
     *                                  FLDLEN(J) = FLDLEN(J)+DELT
           IF(AS(J).GT.ASMAXX(J))       ASMAXX(J) = AS(J)
           IF(Y(J).GT.DEPMAX(J)) THEN
                                 DEPMAX(J) = Y(J)
                                 IDHR(J)   = IFIX(TIME/3600.0)
                                 IDMIN(J)  = MINUTE
                                 ENDIF
 9080      CONTINUE
 9040      CONTINUE
                    DELT  = RDELT
                    DELT2 = DELT/2.0
                    ENDIF
C=======================================================================
C     Surcharge summary information.
C=======================================================================
      IF(IT+1.GT.ITMXXX) ITMXXX = IT + 1
                         ITTOTL = ITTOTL + LIT
                         IF(ABS(ERROR).GT.SUMERR) SUMERR = ABS(ERROR)
C=======================================================================
C     Calculate total depth and total flow for junctions and conduits .
C=======================================================================
      YMAX      = 0.0
      JBAD      = 1
      DO 9500 J = 1,NJ
      YDIFF     = (Y(J) - YO(J))/YCROWN(J)
      IF(ABS(YDIFF).GT.ABS(YMAX)) THEN
                                  YMAX = YDIFF
                                  JBAD = J
                                  ENDIF
      YDEV(J)   = YDEV(J) + ABS(YDIFF)
 9500 YTOT(J)   = YTOT(J) + Y(J)
      QMAX      = 0.0
      IBAD      = 1
      DO 9600 N = 1,NTL
      IF(N.LE.NTC) THEN
      QDIFF     = (Q(N) - QO(N))/QFULL(N)
      IF(ABS(QDIFF).GT.ABS(QMAX)) THEN
                                  QMAX = QDIFF
                                  IBAD = N
                                  ENDIF
      ENDIF
      QDEV(N)   = QDEV(N) + ABS(QDIFF)
 9600 QTOT(N)   = QTOT(N) + Q(N)
C=======================================================================
C     Compute continuity parameters.
C=======================================================================
      IF(ISOL.LE.1) THEN
            DO 950 J = 1,NJ
            IF(QIN(J).LT.0.0) QOU(J) = QOU(J) - QIN(J)*DELT
            IF(QIN(J).GE.0.0) QQI(J) = QQI(J) + QIN(J)*DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
            IF(ABS(Y(J)-(GRELEV(J)-Z(J))).LE.0.000001) 
     *                                 QOU(J) = QOU(J) + SUMQS(J)*DELT
 950        IF(QIN(J).GE.0.0)          SUMQIN = SUMQIN + QIN(J)*DELT
            I        = 0
            DO 960 N = NTC+1,NTL
            J        = NJUNC(N,1)
            IF(NJUNC(N,2).LE.0) THEN
                                QOU(J)  = QOU(J) + Q(N)*DELT
                                I       = I+1
                                QOUT(I) = Q(N)
                                ENDIF
  960       CONTINUE
            ENDIF
C=======================================================================
C     Write output interface file.
C=======================================================================
      IF(NEXT.GT.0) WRITE(NEXT) JULDAY,TIMDAY,DELT,(QOUT(N),N=1,MJSW)
C=======================================================================
C     Check for normal flow, conduit maximum flow and velocity.
C=======================================================================
      IF(ISOL.LE.1) THEN
      DO 902 N = 1,NTL
      IF(N.LE.NTC.AND.ICHECK(N).EQ.IND(2)) SUPLEN(N) = SUPLEN(N) + DELT
      IF(ABS(Q(N)).GT.ABS(QMAXX(N))) THEN
                                     QMAXX(N) = Q(N)
                                     IQHR(N)  = IFIX(TIME/3600.0)
                                     IQMIN(N) = MINUTE
                                     ENDIF
      IF(MTIME.GT.1.AND.ABS(V(N)).GT.ABS(VMAXX(N))) THEN
                                     VMAXX(N) = V(N)
                                     IVHR(N)  = IFIX(TIME/3600.0)
                                     IVMIN(N) = MINUTE
                                     ENDIF
      IF(N.LE.NTC) THEN
                   IF(H(N,1).GT.PMAX(N,1)) PMAX(N,1) = H(N,1)
                   IF(H(N,2).GT.PMAX(N,2)) PMAX(N,2) = H(N,2)
                   IF(LINK(N).EQ.'DR1') CTIME(N,1) = CTIME(N,1) + DELT
                   IF(LINK(N).EQ.'SUB') CTIME(N,2) = CTIME(N,2) + DELT
                   IF(LINK(N).EQ.'CR1') CTIME(N,3) = CTIME(N,3) + DELT
                   IF(LINK(N).EQ.'CR2') CTIME(N,4) = CTIME(N,4) + DELT
                   ENDIF
  902 CONTINUE
      ENDIF
C=======================================================================
C     Check for surcharge and maximum depth at junctions.
C=======================================================================
      IF(ISOL.LE.1) THEN
      DO 906 J = 1,NJ
      IF((Z(J)+Y(J)).GT.ZCROWN(J)) SURLEN(J) = SURLEN(J)+DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
      IF((Z(J)+Y(J)).GE.GRELEV(J)-0.000001) 
     *                             FLDLEN(J) = FLDLEN(J)+DELT
      IF(AS(J).GT.ASMAXX(J))       ASMAXX(J) = AS(J)
      IF(Y(J).GT.DEPMAX(J)) THEN
                            DEPMAX(J) = Y(J)
                            IDHR(J)   = IFIX(TIME/3600.0)
                            IDMIN(J)  = MINUTE
                            ENDIF
  906 CONTINUE
      ENDIF
C=======================================================================
C     Check intermediate printout requirements.
C=======================================================================
      IF(MOD(ICYC,INTER).EQ.0.AND.ICYC.GE.NSTART) THEN
         TMIN   = FLOAT(MINUTE) + FLOAT(JSEC)/60.0
         KHR    = IFIX(TIME/3600.0)
         IF(IT.LE.0.OR.ISOL.GE.2) THEN
                     WRITE(N6,1499) ICYC,KHR,TMIN
                     ELSE
                     WRITE(N6,1500) ICYC,KHR,TMIN,ERROR,IT
                     ENDIF
         WRITE(N6,1501)
         IF(JCE.EQ.0) WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
         IF(JCE.EQ.1) WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
         WRITE(N6,1503)
         IF(JCE.EQ.0) WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
         IF(JCE.EQ.1) WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
         DO 570 J = 1,NJ
C###### WCH 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT
C######
         IF(ABS(Y(J)-(GRELEV(J)-Z(J))).LE.0.000001) THEN
         IF(JCE.EQ.0) THEN
         IF(METRIC.EQ.1) WRITE(N6,514) JUN(J),QOU(J),SUMQS(J),TIME/3600.
         IF(METRIC.EQ.2) WRITE(N6,515) JUN(J),QOU(J),SUMQS(J),TIME/3600.
         ENDIF
         IF(JCE.EQ.1) THEN
         IF(METRIC.EQ.1)WRITE(N6,524) AJUN(J),QOU(J),SUMQS(J),TIME/3600.
         IF(METRIC.EQ.2)WRITE(N6,525) AJUN(J),QOU(J),SUMQS(J),TIME/3600.
         ENDIF
         ENDIF
  570    CONTINUE
         ENDIF
C#######################################################################
C     Store HGL, flow and velocity for printout in Subroutine Output.
C#######################################################################
      IF(MOD(MTIME,JNTER).EQ.0) THEN
      LTIME         = LTIME + 1
      IF(NHPRT.GT.0) THEN
                     DO 600    I   = 1,NHPRT
                     J             = JPRT(I)
                     PRTY(I)       = Y(J)
  600                PRTH(I)       = Y(J) + Z(J)
                     ENDIF
C#######################################################################
      IF(NQPRT.GT.0) THEN
                     DO 620 I      = 1,NQPRT
                     L             = CPRT(I)
                     PRTQ(I)       = Q(L)
  620                PRTV(I)       = V(L)
                     ENDIF
C#######################################################################
C     Save water surface slope.
C#######################################################################
      IF(NSURF.GT.0) THEN
                     DO 630 I      = 1,NSURF
                     L             = JSURF(I)
                     PSF(I,1)      = H(L,1)
  630                PSF(I,2)      = H(L,2)
                     ENDIF
C#######################################################################
C     Write NOUT scratch file every JNTER times.
C#######################################################################
C#### WCH, 11/5/93.  ADD IOSTAT TO OUTPUT.
      IF(JPRINT.EQ.1) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +            JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PRTQ(J),
     +             PRTV(J),J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +  JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +                   JSEC,(PRTY(K),PRTH(K),K=1,NHPRT)
      IF(JPRINT.EQ.4) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     + JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     + JSEC,(PRTQ(J),PRTV(J),J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +                           JSEC,(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.7) WRITE(NOUT,7020,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +                         JSEC,(PSF(J,1),PSF(J,2),J=1,NSURF)
      ENDIF
C#######################################################################
  640 CONTINUE
C=======================================================================
C     Store HGL*flow for printer plot routine, save every MP'th step.
C=======================================================================
      TPLT(NPTOT) = TIME/3600.0
      IF(NPLT.GT.0) THEN
                    DO 700     N       = 1,NPLT
                    J                  = JPLT(N)
  700               YPLT(NPTOT,N)      = Y(J) + Z(J)
                    ENDIF
      IF(LPLT.GT.0) THEN
                    DO 725      N      = 1,LPLT
                    L                  = KPLT(N)
  725               QPLT(NPTOT,N)      =  Q(L)
                    ENDIF
  750 CONTINUE
C=======================================================================
C     Write the final model condition heads and flows.
C=======================================================================
      WRITE(N6,1599) TIME/3600.0
      WRITE(N6,1501)
      IF(JCE.EQ.0) WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
      IF(JCE.EQ.1) WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
      WRITE(N6,1503)
      IF(JCE.EQ.0) WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
      IF(JCE.EQ.1) WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
C=======================================================================
C     Compute water volume left in storage.
C=======================================================================
      IF(NSTORE.GT.0) THEN
                      DO 800 I = 1,NSTORE
                      J        = JSTORE(I)
C=======================================================================
C     Irregular storage elements.
C=======================================================================
      IF(ASTORE(I).GE.0.0) THEN
                      VLEFT  = VLEFT + Y(J)*ASTORE(I)
                      ELSE IF(NUMV(I).LT.0) THEN
                      IF(Y(J).EQ.0.0) Y(J) = 0.0001
                      VLEFT  = VLEFT + VCURVE(I,1,1)*Y(J)**VCURVE(I,2,1)
                      ELSE
                      NTOX   = NUMV(I)
                      RESELV = Y(J)
                      VLEFT  = VLEFT + RESELV*AMEN
                      DO 891 IX = 1,NTOX
                      IF(RESELV.LT.VCURVE(I,2,IX)) GO TO 892
  891                 CONTINUE
  892                 DELTA = (RESELV-VCURVE(I,2,IX-1))/
     +                        (VCURVE(I,2,IX)-VCURVE(I,2,IX-1))
                      VLEFT = VLEFT+DELTA*(VCURVE(I,3,IX) -
     +                        VCURVE(I,3,IX-1))+VCURVE(I,3,IX-1)
                      ENDIF
  800                 CONTINUE
                      ENDIF
C=======================================================================
C     Save ending information on file NREDO.
C=======================================================================
      IF(NREDO.GT.0.AND.JREDO.GE.2) THEN
               REWIND NREDO
               WRITE(N6,2200)
               WRITE(NREDO,ERR=8889) NTL,NJ,NPUMP
               WRITE(NREDO,ERR=8889) (Q(N),N=1,NTL),(QO(N),N=1,NTL)
               WRITE(NREDO,ERR=8889) (V(N),N=1,NTL),(VT(N),N=1,NTL)
               WRITE(NREDO,ERR=8889) (A(N),N=1,NTL),(AT(N),N=1,NTL)
               WRITE(NREDO,ERR=8889) (ASFULL(N),N=1,NJ)
               WRITE(NREDO,ERR=8889) (Y(N),N=1,NJ),(YT(N),N=1,NJ)
               WRITE(NREDO,ERR=8889) (VWELL(N),N=1,NPUMP),
     +                               (JPFUL(N),N=1,NPUMP)
               ENDIF
C=======================================================================
C     Volume remaining in conduit with tide gate not included in VLEFT.
C=======================================================================
      DO 840 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
C     IF(NKLASS(N).GE.9) GO TO 840
      H(N,1)  = Y(NL) + Z(NL)
      H(N,2)  = Y(NH) + Z(NH)
      CALL HEAD(N,NL,NH,H(N,1),H(N,2),Q(N),A(N),V(N),HRAD,ANH,ANL,
     +                                      RNL,RNH,IDOIT,LINK(N))
      ASFULL(N) = HRAD
      QO(N)     = A(N)*LEN(N)
      IF(NGATE.GT.0) THEN
                     DO 845 I = 1,NGATE
                     IF(JGATE(I).EQ.NH.OR.JGATE(I).EQ.NL) GO TO 840
  845                CONTINUE
                     ENDIF
      VLEFT     = VLEFT + 0.5*(ANH + ANL)*LEN(N)
  840 CONTINUE
C=======================================================================
C     Write further final condition information.
C=======================================================================
      WRITE(N6,1506)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),V(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),V(N),N=1,NTC)
      WRITE(N6,1507)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),A(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),A(N),N=1,NTC)
      WRITE(N6,1527)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),QO(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),QO(N),N=1,NTC)
      WRITE(N6,1508)
      IF(JCE.EQ.0) WRITE(N6,1514) (NCOND(N),ASFULL(N),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1516) (ACOND(N),ASFULL(N),N=1,NTC)
      WRITE(N6,1509)
      IF(JCE.EQ.0) WRITE(N6,1517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
C=======================================================================
C     Call print and plot subroutine (OUTPUT).
C=======================================================================
      IF(ISOL.LE.1) THEN
      ITT    = ITTOTL - 2*NTCYC
      DENOM  = FLOAT(NTCYC)
      AVGIT  = FLOAT(ITTOTL)/DENOM
      IF(METRIC.EQ.1) WRITE(N6,1600)ITMXXX,ITTOTL,AVGIT,ITT,SUMERR,NTCYC
      IF(METRIC.EQ.2) WRITE(N6,1605)ITMXXX,ITTOTL,AVGIT,ITT,SUMERR,NTCYC
      WRITE(N6,1620)
      IF(JCE.EQ.0) WRITE(N6,1621) (NCOND(N),BADMAX(N)/60.0,N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1622) (ACOND(N),BADMAX(N)/60.0,N=1,NTC)
      WRITE(N6,1625)
      IF(JCE.EQ.0) WRITE(N6,1621) (NCOND(N),BADTIM(N)/DENOM,N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1622) (ACOND(N),BADTIM(N)/DENOM,N=1,NTC)
      ELSE
      DENOM   = FLOAT(NTCYC)
      WENOM   = FLOAT(JTIME)
      AVGIT   = FLOAT(ITTOTL)/WENOM
      ARAT    = WENOM/DENOM
      AVGSIZ  = DENOM*RDELT/WENOM
      AFACT   = AFACT/WENOM
      WRITE(N6,1606) ITMXXX,ITTOTL,SUMERR,JTIME,ARAT,AVGIT,
     +               AVGSIZ,YOOMIN,YOOMAX,ITURN,AFACT
      WRITE(N6,1620)
      IF(JCE.EQ.0) WRITE(N6,1621) (NCOND(N),BADMAX(N)/60.0,N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1622) (ACOND(N),BADMAX(N)/60.0,N=1,NTC)
      WRITE(N6,1625)
      IF(JCE.EQ.0) WRITE(N6,1621) (NCOND(N),BADTIM(N)/WENOM,N=1,NTC)
      IF(JCE.EQ.1) WRITE(N6,1622) (ACOND(N),BADTIM(N)/WENOM,N=1,NTC)
      ENDIF
      RETURN
C=======================================================================
C#### WCH, 11/5/93.  ALSO WRITE TO THE SCREEN AND ADD IOSTAT TO 8905.
 8888 WRITE(N6,8885)
      WRITE(*,8885)
      STOP
 8889 WRITE(N6,8886)
      WRITE(*,8886)
      STOP
 8900 WRITE(N6,8905) IOS
      WRITE(*,8905)  IOS
      STOP
C=======================================================================
   21 FORMAT('+',6X,I5,9X,3I9,1X,2(A8,F7.3))
   22 FORMAT('+',6X,I5,9X,3I9,1X,2(I8,F7.3))
   23 FORMAT(/,
     +' ************************************************************',/,
     +' *  NRM ==>  # OF CONDUITS USING NORMAL FLOW EQUATION       *',/,
     +' *   SJ ==>  # OF SURCHARGED JUNCTIONS.                     *',/,
     +' * COND ==>  # CONDUIT WITH THE GREATEST FLOW CHANGE        *',/,
     +' *             ( Q(n+1) - Q(n) ) / Qfull = QMAX             *',/,
     +' * JUNC ==>  # JUNCTION WITH THE GREATEST DEPTH CHANGE      *',/,
     +' *             ( Y(n+1) - Y(n) ) / Yfull = YMAX             *',/,
     +' ************************************************************',/)
   24 FORMAT(
     +' TOTAL # OF JUNCTIONS ==> ',I5,
     +' TOTAL # OF CONDUITS  ==> ',I5,//,
     +' BEGINNING LOOP THRU ',I6,' TIME STEPS',/,
     + ' TIME STEP #.  # OF ITERATIONS. # OF NRM  # OF SJ',
     +'    COND   QMAX    JUNC   YMAX',/)
  514 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I9,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
  515 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I9,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
  524 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A9,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
  525 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A9,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
 1499 FORMAT(/,1X,'CYCLE ',I5,6X,'TIME ',I4,' HRS - ',F5.2,' MIN')
 1500 FORMAT(/,1X,'CYCLE ',I5,6X,'TIME ',I4,' HRS - ',F5.2,
     *' MIN  FLOW DIFFERENTIAL IN SURCHARGED AREA = ',F6.2,
     *'CFS  ITERATIONS REQUIRED = ',I2,/)
 1501 FORMAT(/,' JUNCTION / DEPTH  / ELEVATION ',
     +         '   ===>  "*" JUNCTION IS SURCHARGED.')
 1502 FORMAT(3(I10,'/',F7.2,A1,'/',F9.2))
 1503 FORMAT(/,'   CONDUIT/       FLOW',
     +         '   ===> "*" CONDUIT USES THE NORMAL FLOW OPTION.')
 1505 FORMAT(4(I10,'/',F11.2,A1))
 1506 FORMAT(/,'   CONDUIT/   VELOCITY')
 1507 FORMAT(/,'   CONDUIT/ CROSS SECTIONAL AREA')
 1508 FORMAT(/,'   CONDUIT/ HYDRAULIC RADIUS')
 1509 FORMAT(/,'   CONDUIT/ UPSTREAM/ DOWNSTREAM ELEVATION')
 1512 FORMAT(3(1X,A9,'/',F7.2,A1,'/',F9.2))
 1514 FORMAT(4(I10,'/',F11.2,1X))
 1515 FORMAT(4(1X,A9,'/',F11.2,A1))
 1516 FORMAT(4(1X,A9,'/',F11.2,1X))
 1517 FORMAT(3(I10,'/',F9.2,'/',F9.2))
 1518 FORMAT(3(1X,A9,'/',F9.2,'/',F9.2))
 1527 FORMAT(/,'   CONDUIT/  FINAL VOLUME ')
 1598 FORMAT(/,
     +' ***********************************',/,
     +' *    INITIAL MODEL CONDITION      *',/,
     +' * INITIAL TIME = ',F9.2,' HOURS  *',/,
     +' ***********************************')
 1599 FORMAT(/,
     +' *********************************',/,
     +' *     FINAL MODEL CONDITION     *',/,
     +' *  FINAL TIME = ',F9.2,' HOURS *',/,
     +' *********************************')
 1600 FORMAT(///,
     +' ###################################################',/,
     +' #           Surcharge Iteration Summary           #',/,
     +' ###################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
     +' Total number of iterations in the simulation....',I9,/,
     +' Average number of iterations per time step......',F9.2,/,
     +' Surcharge iterations during the simulation......',I9,/,
     +' Maximum surcharge flow error during simulation..',1PE9.2,' cfs',
     +/,' Total number of time steps during simulation....',I9,/)
 1605 FORMAT(///,
     +' ###################################################',/,
     +' #           Surcharge Iteration Summary           #',/,
     +' ###################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
     +' Total number of iterations in the simulation....',I9,/,
     +' Average number of iterations per time step......',F9.2,/,
     +' Surcharge iterations during the simulation......',I9,/,
     +' Maximum surcharge flow error during simulation..',1PE9.2,' cms',
     +/,' Total number of time steps during simulation....',I9,/)
 1606 FORMAT(///,
     +' ####################################################',/,
     +' #             Extran Iteration Summary             #',/,
     +' ####################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
     +' Total number of iterations in the simulation....',I9,/,
     +' Maximum continuity error during simulation......',F9.4,
     +' fraction',/,
     +' Total number of time steps during simulation....',I9,/,
     +' Ratio of actual # of time steps / NTCYC.........',F9.1,/,
     +' Average number of iterations per time step......',F9.2,/,
     +' Average  time step size(seconds)................',F9.2,/,
     +' Smallest time step size(seconds)................',F9.1,/,
     +' Largest  time step size(seconds)................',F9.1,/,
     +' Number of times ITMAX exceeded..................',I9,/,
     +' Average Courant Factor Tf.......................',F9.2)
 1620 FORMAT(/,1H1,/,
     +' ***************************************************',/,
     +' *         CONDUIT COURANT CONDITION SUMMARY       *',/,
     +' * TIME IN MINUTES DELT > COURANT TIME STEP        *',/,
     +' ***************************************************',/,
     +' * SEE BELOW FOR EXPLANATION OF COURANT TIME STEP. *',/,
     +' ***************************************************',//,
     +'  CONDUIT #  TIME(MN)  CONDUIT #  TIME(MN)  CONDUIT #  TIME(MN)
     +CONDUIT #  TIME(MN)',/,
     +'  ---------  --------  ---------  --------  ---------  --------
     +---------  --------')
 1621 FORMAT(4(1X,I10,F10.2))
 1622 FORMAT(4(1X,A10,F10.2))
 1625 FORMAT(/,1H1,/,
     +' ************************************************',/,
     +' *         CONDUIT COURANT CONDITION SUMMARY    *',/,
     +' ************************************************',/,
     +' * COURANT   =            CONDUIT LENGTH        *',/,
     +' * TIME STEP = -------------------------------- *',/,
     +' *             VELOCITY + SQRT(GRVT*AREA/WIDTH) *',/,
     +' ************************************************',/,
     +' * AVERAGE COURANT CONDITION TIME STEP(SECONDS) *',/,
     +' ************************************************',//,
     +'  CONDUIT # TIME(SEC)  CONDUIT # TIME(SEC)  CONDUIT # TIME(SEC)
     +CONDUIT # TIME(SEC)',/,
     +'  ---------  --------  ---------  --------  ---------  --------
     +---------  --------')
 2100 FORMAT(/,
     +' *****************************************************',/,
     +' *        READING HOT START FILE ON NSCRAT(2)        *',/,
     +' * SEE INITIAL MODEL CONDITIONS FOR HOT START VALUES *',/,
     +' *****************************************************',/)
 2200 FORMAT(/,
     +' ***************************************************',/,
     +' *      WRITING HOT START FILE ON NSCRAT(2)        *',/,
     +' * SEE FINAL MODEL CONDITIONS FOR HOT START VALUES *',/,
     +' ***************************************************',/)
 7000 FORMAT(200(I10,1X))
 7010 FORMAT(200(A10,1X))
 7020 FORMAT(E12.5,2I7,200(E12.5,1X))
 8885 FORMAT(/,' ===> Error !! Reading the Hot-start file.',/,
     +         '               Usual cause - empty file.',/)
 8886 FORMAT(/,' ===> Error !! Writing the Hot-start file.',/,
     +'               Usual cause - Unformatted record length problem',
     +/,'               use the /R DOS option to extend the unformatted
     +record length.',/)
 8905 FORMAT(/,' ===> Error !! Writing NSCRAT(1) file.',/,
     1 ' Fortran error no. =',I5,'. A common cause is that you',/,
     2 ' have run out of space on your hard disk needed by this',/,
     3 ' voluminous scratch file.  Run stopped from Extran.',/)
C=======================================================================
      END
