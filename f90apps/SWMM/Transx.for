      SUBROUTINE TRANSX
C	EXTRAN BLOCK
C=======================================================================
C     Subroutine TRANSX of the Extran model calls subroutines
C     XROUTE, YROUTE AND ZROUTE for the solution of the link
C     momentum equation and the nodal continuity equation.
C     Modified 7/14/93 by WCH based on CDM memo from Chuck Moore
C       to correct for equality check for surface flooding.
C     Slight format change for NSCRAT1 error message, WCH, 11/5/93.
C     Correct DO 892 loop to avoid possible negative subscript
C       when dealing with variable area junctions, WCH (RED), 12/7/94.
C     Correct power function calculation for volume and change minimum
C       area assumptions for irregular storage, WCH, 12/8/94.
C     Correction to above correction.  Include VINIT and VLEFT in
C       summations since these variables sum over all pipes and
C       junctions.  WCH (C. Moore), 6/5/95.
C     Alter IOSTAT for Lahey. WCH, 8/4/95.
C     Correct inflow calculation for ISOL = 2 following changes to
C       Sub. INFLOW of 10/17/95. WCH, 7/25/96.
C     Put in error check for zero QFULL.  WCH, 1/22/97.
C     Significant changes by CDM (C.I.Moore) for SWMM 4.4, 96-97.
C     FIX TRANSX not to save maximums before NSTART.  This allows
C       spurious startup values from being saved for the conduit and
C       junction summary output. CIM, 7/15/97.
C     CHANGE FOR MULTIPLE OUTPUT PERIODS, CHANGE CHECK FOR NSTART TO
C     A CHECK AGAINST IBESTART(IBE) AND IBEEND(IBE).  UPDATE IBE AS
C     NECESSARY.  cim 5/98
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
      INCLUDE 'FCHAN.INC'
cim TRANAID START
CIM include common block for tranaid transfer file
      INCLUDE 'TRANAID.INC'
c new include for detailed ASCII output
      INCLUDE 'ASCOUT1.INC'
cim common for variable base flow
      INCLUDE 'MOBFF.INC'
CIM 1/99 CHANGE FOR BA OUTPUT CONTROLS    - OUTPUT CONTROLS
      INCLUDE 'BALINES.INC'
CIM 6/99 EXTRAN TO WASP LINK
	INCLUDE 'WASP.INC'
      character*20 ifile
      DOUBLE PRECISION RAYTIME
CIM  TRANAID END
CIM  NEW COMMON FOR WRITING OF RESULTS TO ASCII FILE
      INCLUDE 'CWRITE.INC'
CIM
CIM  NEW COMMON FOR WRITING INTERMEDIATE CONTINUITY CALCS
      INCLUDE 'INTCON.INC'
CIM END
CIM  MULTIPLE PRINTOUT PERIODS  5/98
      INCLUDE 'BE.INC'
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
cim speed  initialize stuff for head routine
cim  make fudge smaller
cim      FUDGE = 0.0001
      FUDGE = 0.00001
cim speed
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
cim change write to nout to a sequential unformatted write
      WRITE(NOUT) JCE,NHPRT,NQPRT,NSURF
      IF(JCE.EQ.0) THEN
      IF(NHPRT.GT.0) WRITE(NOUT) (JUN(JPRT(J)),J=1,NHPRT)
      IF(NQPRT.GT.0) WRITE(NOUT) (NCOND(CPRT(J)),J=1,NQPRT)
      IF(NSURF.GT.0) WRITE(NOUT) (NCOND(JSURF(J)),J=1,NSURF)
      ELSE
      IF(NHPRT.GT.0) WRITE(NOUT) (AJUN(JPRT(J)),J=1,NHPRT)
      IF(NQPRT.GT.0) WRITE(NOUT) (ACOND(CPRT(J)),J=1,NQPRT)
      IF(NSURF.GT.0) WRITE(NOUT) (ACOND(JSURF(J)),J=1,NSURF)
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
C     Calculate initial volume in conduits (VINIT).  Use average of
C     YNORM's at Junctions unless initial Junction depths are specified.
C=======================================================================
      DO 10 J  = 1,NJ
      QQI(J)   = 0.0
cim initialized qou(j) 5/97
      QOU(J)   = 0.0
      IF(Y(J).GT.0.01) GO TO 10
      DO 20 JJ = 1,NCHN
      IF(NCHAN(J,JJ).EQ.0) GO TO 11
      N       = NCHAN(J,JJ)
      IF(QO(N).NE.0.0) THEN
                       CALL DEPTHX(N,NKLASS(N),QO(N),YC,YNORM)
                       Y(J)    = Y(J) + YNORM
                       ENDIF
   20 CONTINUE
C#### WCH, 12/8/94.  CAN THERE BE A JUNCTION WITH NO CONNECTING PIPES?
C     CHECK, JUST IN CASE TO AVOID ZERO DIVIDE.
   11 IF(JJ.GT.1) Y(J) = Y(J)/FLOAT(JJ-1)
   10 CONTINUE
C=======================================================================
C     Initialize AT,VT,Q,Y arrays
C     calculate initial system volume (VINIT).
C=======================================================================
CIM START  OOOOOOOO
CIM     DETERMINE INITIAL GATED GATE OPENING INITIAL DEPTH Y
CIM     USE LARGE DELTA T TO SET INITIAL CONDITIONS  (11.6 DAYS)
  666 CALL OGATES(999999.9,Y,V)
CIM END  OOOOOOOO
      DO 25 N   = 1,NTC
      IFULL(N)=0
      NFIRST(N)=0
      FHOUR(N)=0.0
      NLAST(N)=0
      FLHOUR(N)=0.0
      BADTIM(N) = 0.0
      BADMAX(N) = 0.0
CIM TRANAID START
      SUMQ(N) = 0.0
CIM TRANAID END
      AOVERB(N) = DEEP(N)
      SLOPE     = (ZU(N)-ZD(N))/LEN(N)
      IF(SLOPE.EQ.0.0) SLOPE = 0.01/LEN(N)
      VDSGN     = SQRT(GRVT*SLOPE/ROUGH(N))*RFULL(N)**0.6666667
      QFULL(N)  = AFULL(N)*VDSGN
C#######################################################################
C#### WCH, 1/22/97.  CHECK FOR ZERO QFULL.
C=======================================================================
      IF(QFULL(N).LE.0.0) THEN
           IF(JCE.EQ.0) THEN
                        WRITE(N6,9400) NCOND(N),N
           ELSE
                        WRITE(N6,9401) ACOND(N),N
           ENDIF
           WRITE(N6,9402) QFULL(N),VDSGN,RFULL(N),AFULL(N),SLOPE
           ENDIF
      NL        = NJUNC(N,1)
      NH        = NJUNC(N,2)
      H(N,1)    = AMAX1(Y(NL) + Z(NL),ZU(N))
      H(N,2)    = AMAX1(Y(NH) + Z(NH),ZD(N))
      IF(JREDO.EQ.0.OR.JREDO.EQ.2) THEN
                       CALL NHEAD(N,NL,NH,H(N,1),H(N,2),QO(N),
     +                             A(N),V(N),HRAD,ANH,ANL,RNL,RNH,
     +                             IDOIT,LINK(N),AS)
                                   VT(N)     = V(N)
                                   AT(N)     = A(N)
                                   Q(N)      = QO(N)
                                   ELSE
                       CALL NHEAD(N,NL,NH,H(N,1),H(N,2),QT(N),
     +                             AT(N),VT(N),HRAD,ANH,ANL,RNL,RNH,
     +                             IDOIT,LINK(N),AS)
                                   ENDIF
CIM TRANAID START
CIM   HYDRAULIC RADIUS SAVED IN r ARRAY IN SUBROUTINE VOLUME
CIM      YO(N)     = HRAD
CIM TRANAID END
C=======================================================================
C     Note, initial volume will never be zero because FUDGE = 0.0001 is
C     used in Sub. HEAD instead of zeroes for cross sectional areas
C     (and other cross section parameters).  Thus, initial volume will
C     approximately equal sum of pipe lengths x FUDGE, plus corrections
C     for Courant violations plus new equivalent pipes, e.g., for
C     orifices.  I.e., in equation below, ANL = ANH = 0.0001 even if
C     zero flows and depths are specified as initial conditions.
C=======================================================================
CIM TRANAID START
CIM VOLUME CALCULATION MOVED TO SUBROUTINE VOLUME
CIM   25  VINIT     = VINIT + (ANL+ANH)*LEN(N)/2.0
CIM
  25  CONTINUE
c  compute initial total volume and volume for each junction
      CALL VOLUME(VINIT,.TRUE.)
cim INTERCON
CIM  call IINTERCON to initialized intermediate continuity output data
      VNOW = VINIT
      call IINTERCON(.true.)
CIM INTERCON END
CIM 5/99 CALL WASPINIT TO INITIAL EXTRAN TO WASP HYRDODYNAMICS INTERFACE FILE
      CALL WASPINIT
c.......................................................................
c       Open hydraulic output file, and initialize variables
c.......................................................................
	if (idump .gt. 0) then
	ihyd = ihydwr - 1
	ihydwr = nint(dthyd/delt)
cim test write
c        write(n6,*) idump,ihyd,ihydwr,dthyd,hydstr
	write(*,6789)
6789    format(' Enter hydraulic output file name: ',$)
	read(*,'(a20)') ifile
	open(20, file=ifile, status='unknown', form='unformatted')
	write(20) nfree,(ntl-nfree),nj,(qinst(j),j=1,nj),
     &            (jfree(j),j=1,nfree)
	write(20) (len(n),n=1,ntl)
	write(20) ((njunc(n,i),i=1,2),n=1,ntl)
	write(20) (jun(j),j=1,nj)
	write(20) (ncond(n),n=1,ntl)
	write(20) (sqrt(rough(n)/grvt)*cmet(9,metric), n=1,ntl)
		if (hydstr .le. time/3600.) then
cim note time is not double precision in this program, convert to raytime which is
                raytime=time/3600.
		write(20) raytime
		write(20) (q(n),n=1,ntl)
		write(20) (a(n),n=1,ntl)
		write(20) (r(n),n=1,ntl)
		write(20) (vol(j),j=1,nj)
		write(20) (qin(j),j=1,nj)
		write(20) ((y(j)+z(j)),j=1,nj)
		ihyd = 0
		end if
	end if
CIM      VOLUME CALCULATION NOW IN SUBROUTINE VOLUME
C 25  IF(NKLASS(N).LE.8) VINIT = VINIT + (ANL+ANH)*LEN(N)/2.0
CIM TRANAID END
C=======================================================================
C     Write initial information.
C=======================================================================
      WRITE(N6,1598) TIME/3600.0
      WRITE(N6,1501)
cim 1/99 write all 10 digits and characters
CIM old write statements for JP10 = 0
      IF(JP10.EQ.0) THEN
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
          ELSE
                   WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
          ENDIF
      WRITE(N6,1503)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
          ELSE
                   WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
          ENDIF
      WRITE(N6,1506)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),V(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),V(N),N=1,NTC)
          ENDIF
      WRITE(N6,1507)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),A(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),A(N),N=1,NTC)
          ENDIF
      WRITE(N6,1508)
CIM  TRANAID START
CIM  NOTE HYDRAULIC RADIUS IS NOW SAVED IN R ARRAY IN SUBROUTINE VOLUME
CIM  CHANGE YO TO R IN FOLLOWING TWO LINES
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),R(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),R(N),N=1,NTC)
          ENDIF
CIM  TRANAID END
      WRITE(N6,1509)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
          ELSE
                   WRITE(N6,1518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
          ENDIF
      ELSE
C     FOLLOWING WRITE ALL 10 DIGITS AND CHARACTERS
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
          ELSE
                   WRITE(N6,61512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
          ENDIF
      WRITE(N6,1503)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
          ELSE
                   WRITE(N6,61515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
          ENDIF
      WRITE(N6,1506)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),V(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),V(N),N=1,NTC)
          ENDIF
      WRITE(N6,1507)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),A(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),A(N),N=1,NTC)
          ENDIF
      WRITE(N6,1508)
CIM  TRANAID START
CIM  NOTE HYDRAULIC RADIUS IS NOW SAVED IN R ARRAY IN SUBROUTINE VOLUME
CIM  CHANGE YO TO R IN FOLLOWING TWO LINES
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),R(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),R(N),N=1,NTC)
          ENDIF
CIM  TRANAID END
      WRITE(N6,1509)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
          ELSE
                   WRITE(N6,61518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
          ENDIF
      ENDIF
C
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
CIM TRANAID START
CIM VOLUME CALC FOR IRREGULAR STORAGE ELEMENTS MOVED TO VOLUME SUBROUTINE
CIM VINIT CALCS COMMENTED OUT OF NEXT LOOP
C=======================================================================
C     New Computation of Volume for Irregular Storage Elements.
C=======================================================================
cim   DO 70 I = 1,NSTORE
cim   J       = JSTORE(I)
cim   IF(ASTORE(I).GE.0.0) THEN
CIM                         VINIT  = VINIT + Y(J)*ASTORE(I)
cim                      ELSE IF(NUMV(I).LT.0) THEN
cim                         IF(Y(J).EQ.0.0) Y(J) = 0.0001
C=======================================================================
C#### WCH, 12/8/94.  ERROR HERE.  MUST >>INTEGRATE<< AREA POWER
C     FUNCTION TO GET VOLUME!  ALSO, LET MINIMUM AREA = AMEN.
C=======================================================================
C####                         VINIT = VINIT +
C####     +                           VCURVE(I,1,1)*Y(J)**VCURVE(I,2,1)
C=======================================================================
C#### WCH (C.MOORE), 6/5/95.  CORRECTION TO CORRECTION.  MUST INCLUDE
C     VINIT IN SUMMATION SINCE VINIT IS GRAND TOTAL FOR ALL
C     PIPES/JUNCTIONS.
C=======================================================================
C####                            VINIT = Y(J)*AMEN
CIM                            VINIT = VINIT + Y(J)*AMEN
CIM                            VINIT = VINIT + VCURVE(I,1,1) *
CIM     +         Y(J)**(VCURVE(I,2,1)+1)/(VCURVE(I,2,1)+1)
cim                      ELSE
cim                      NTOX       = NUMV(I)
cim                      RESELV     = Y(J)
C#### WCH, 12/8/94.  MIN. AREA = AMEN NOT USED IN SUB. BOUND, SO
C     DON'T INCLUDE IT HERE.  BUT ADD CHECK IN SUB. INDAT2 TO ENSURE
C     THAT MIN. AREA NOT = ZERO.   ALSO, START WITH SUBSCRIPT IX = 2
C     TO ENSURE THAT IX-1 NOT = 0.
C####                         VINIT      = VINIT + RESELV*AMEN
cim                      DO 1891 IX = 2,NTOX
cim 1891                    IF(RESELV.LT.VCURVE(I,2,IX)) GO TO 1892
cim 1892                    DELTA = (RESELV-VCURVE(I,2,IX-1))/
cim     +                           (VCURVE(I,2,IX)-VCURVE(I,2,IX-1))
CIM                         VINIT = VINIT+DELTA*(VCURVE(I,3,IX) -
CIM     +                           VCURVE(I,3,IX-1))+VCURVE(I,3,IX-1)
cim                         ENDIF
cim   70 CONTINUE
CIM TRANAID END
C=======================================================================
C     Write first line of output interface file.
C=======================================================================
      IF(NEXT.GT.0) THEN
                    BELT = 0.0
                    WRITE(NEXT) JULDAY,TIMDAY,BELT,(QOUT(N),N=1,MJSW)
                    ENDIF
C
C here call ASCOUT to output head information
C
      IF (IASCII.NE.0) CALL ASCOUT(1)
C
C=======================================================================
C     Major Program loop through time.
C=======================================================================
C ### RHF 10/16/96
      NTIMCS = 0
      QMINCS = 999999.999
      QMOUCS = 999999.999
C   NEWQIN IS 1 IF STEADY, 0 IF NOT STEADY
      NEWQIN = 0
      NOROUT = 0
      CONSTINFLOW = .FALSE.
      SMALLOUTFLOW = .FALSE.
      EQUALINOUT = .FALSE.
      CONSTINFLOWO = .FALSE.
      SMALLOUTFLOWO = .FALSE.
      EQUALINOUTO = .FALSE.
      IBAD = 1
      JBAD = 1
      WRITE(*,23)
      IF (ISOLSKIP.EQ.1)  WRITE(*,923)
      WRITE(*,924)
      WRITE(*,24)   NJ,NTL,NTCYC
      IF (ISOLSKIP.NE.1) THEN
            WRITE(*,27)
            ELSE
            WRITE(*,28)
            ENDIF
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
C  CHECK IF IBE NEEDS UPDATED
   39 IF (ICYC.LT.IBEEND(IBE)) GO TO 40
      IBE = IBE + 1
      GO TO 39
   40 CONTINUE
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
      IF (ISOLSKIP.EQ.1) THEN
C   NEWQTAPE IS RETURNED AS 1 IF STEADY, 0 IF NOT STEADY
C   NEWQCARD IS RETURNED AS 1 IF STEADY, 0 IF NOT STEADY
      NOROUTO = NOROUT
      IF (NEWQTAPE.EQ.0.or.NEWQCARD.EQ.0) THEN
      NEWQIN = 0
      NOROUT = 0
      CONSTINFLOWO = CONSTINFLOW
      CONSTINFLOW = .FALSE.
      IF (NOROUTO.EQ.1) CALL SSSTATUS(1)
      ELSE
      NEWQIN = 1
      ENDIF
      ENDIF
c     write(n6,*) 'transx1',NEWQTAPE,NEWQCARD,Newqin
C=======================================================================
C     Save beginning time step values.
C=======================================================================
      MSURGE    = 0
cim move if statment out of the loop
      DO 9000 J = 1,NJ
cim      IF(ISOL.GE.2) THEN
cim                    YT(J)    = Y(J)
cim                    VTEMP(J) = VWELL(J)
cim                    ENDIF
 9000 IF(Y(J).GT.YCROWN(J)) MSURGE = MSURGE + 1
      IF(ISOL.GE.2) THEN
      DO 9001 J = 1,NJ
                    YT(J)    = Y(J)
                    VTEMP(J) = VWELL(J)
 9001 CONTINUE
      ENDIF
      MNORM     = 0
      YDELT     = XDELT/FACT
      XDELT     = RDELT
      DO 9010 N = 1,NTC
      NL        = NJUNC(N,1)
      NH        = NJUNC(N,2)
      IF(ISOL.GE.2) QT(N)  = Q(N)
CIM START <><><><><><>
CIM      THIS IS FOR TRAPEZOID, IRREGULAR(PARABOLIC) OPEN CHANNELS
CIM      IF(NKLASS(N).GE.5.AND.NKLASS(N).LT.9) THEN
         IF(NKLASS(N).EQ.22.OR.NKLASS(N).EQ.23) THEN
CIM END  <><><><><><><>
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
                      IF(ISOL.LE.1) THEN
                        BADTIM(N)=BADTIM(N)+RDELT
                        else
                        BADTIM(N)=BADTIM(N)+VEL*FLOAT(NUSE)
                      ENDIF
                      ENDIF
 9010 IF(ICHECK(N).EQ.IND(2)) MNORM = MNORM + 1
                       FACT  = FMAX
      IF(MSURGE.GE.1)  FACT  = FMIN
                      XDELT  = XDELT*FACT
cim clean if statements here and add additional parameters
cim      IF(MTIME.GT.1)  THEN
      IF(JCE.EQ.0) THEN
                   IF (ISOLSKIP.NE.1) THEN
                   WRITE(6,22) MTIME,ITTOTL,MNORM,MSURGE,NCOND(IBAD),
     +                         QMAX,JUN(JBAD),YMAX
                   ELSE
                   WRITE(6,22) MTIME,ITTOTL,MNORM,MSURGE,NCOND(IBAD),
     +                         QMAX,JUN(JBAD),YMAX,NOROUT,NEWQIN,NTIMCS
                   ENDIF
      ELSE
                   IF (ISOLSKIP.NE.1) THEN
                   WRITE(6,21) MTIME,ITTOTL,MNORM,MSURGE,ACOND(IBAD),
     +                         QMAX,AJUN(JBAD),YMAX
                   ELSE
                   WRITE(6,21) MTIME,ITTOTL,MNORM,MSURGE,ACOND(IBAD),
     +                         QMAX,AJUN(JBAD),YMAX,NOROUT,NEWQIN,NTIMCS
                   ENDIF
      endif
cim   read(*,*) intemp
cim      ELSE
cim      IF(XDELT.EQ.RDELT) XDELT = RDELT/4.0
cim      IF(JCE.EQ.0)  WRITE(*,22) MTIME,ITTOTL,MNORM,MSURGE
cim      IF(JCE.EQ.1)  WRITE(*,21) MTIME,ITTOTL,MNORM,MSURGE
cim      ENDIF
C=======================================================================
C     Call modified EULER solution's.
C=======================================================================
CIM SPEEDY USE CASE STATEMENTS HERE (COULD BE REPLACED BY COMPUTED GOTO)
      SELECT CASE (ISOL)
cim   IF(ISOL.EQ.0) THEN
      CASE(0)
         IF (NOROUT.EQ.0) THEN
                    CALL XROUTE
                    LIT = IT + 2
                    ntimcs = ntimcs + 1
         ENDIF
cim                    ENDIF
cim      IF(ISOL.EQ.1) THEN
      CASE(1)
         IF (NOROUT.EQ.0) THEN
                    CALL YROUTE
                    LIT = IT + 2
                    ntimcs = ntimcs + 1
         ENDIF
cim                    ENDIF
C=======================================================================
C      Call the iterative solution.  Determine the time step size based
C      on the shortest conduit and the previous maximum velocity.
C=======================================================================
cim      IF(ISOL.GE.2) THEN
      CASE(2)
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
C=======================================================================
C     Interpolate for inflows at this time.
C=======================================================================
                    DO 9045 J = 1,NJ
                    QQ1       = QINN(J,1)
                    QQ2       = QINN(J,2)
                    T         = FLOAT(N)*DELT/3600.0
                    T1        = FLOAT(N-1)*DELT/3600.0
                    QIN(J)    = QLINTP(QQ1,QQ2,T2,T1)/2.0 +
CIM  change to implement monthly base flow factors
CIM     +                          QLINTP(QQ1,QQ2,T2,T)/2.0  + QINST(J)
     +                          QLINTP(QQ1,QQ2,T2,T)/2.0  +
     +                          QINST(J)*BFFMONTH(1,IWHICH(J))
C#######################################################################
C#### WCH, 7/25/96.  FROM CHANGES TO SUB. INFLOW OF 10/17/95, K3-LINE
C     INPUT IN SUB. INFLOW NOW KEPT SEPARATE FROM INTERFACE FILE
C     INPUT, IN ARRAY QINNK3.  NEED TO INTERPOLATE ON THAT ARRAY HERE
C     NOW AS WELL.
C=======================================================================
                    QQ1       = QINNK3(J,1)
                    QQ2       = QINNK3(J,2)
                    QIN(J)    = QLINTP(QQ1,QQ2,T2,T1)/2.0 +
     +                          QLINTP(QQ1,QQ2,T2,T)/2.0  + QIN(J)
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
c inflow and outflow
                    DO 9050 J = 1,NJ
                    IF(QIN(J).LT.0.0) QOU(J) = QOU(J) - QIN(J)*DELT
                    IF(QIN(J).GE.0.0) QQI(J) = QQI(J) + QIN(J)*DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
CIM 5/99 USE SURELEV IN PLACE OF GRELEV
CIM I THINK THIS IS REJECTED INFLOW IF SURCHARGED
                    IF(Y(J).GE.SURELEV(J)-Z(J)-0.000001) THEN
                       QOU(J) = QOU(J)  +  SUMQ(J)*DELT
					 VOL(J) = VOL(J)  -  SUMQ(J)*DELT
	ENDIF
cim intercont
cim sumqin is reset and computed in OUTPUT
cim 9050               IF(QIN(J).GE.0.0) SUMQIN = SUMQIN + QIN(J)*DELT
 9050 CONTINUE
                    I         = 0
c these are boundary conduit flows
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
CIM add check for nstart
cim change to call to IPRNTIT (see end of this file)
cim       IF (ICYC.GE.NSTART) THEN
      IF (IPRNTIT(ICYC).EQ.1) THEN
CIM
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
CIM SPEED can do away with these four ifs  link is now integer not string
                   CTIME(M,link(m))   = CTIME(M,link(m)) + DELT
cim                   IF(LINK(M).EQ.'DR1') CTIME(M,1)   = CTIME(M,1) + DELT
cim                   IF(LINK(M).EQ.'SUB') CTIME(M,2)   = CTIME(M,2) + DELT
cim                   IF(LINK(M).EQ.'CR1') CTIME(M,3)   = CTIME(M,3) + DELT
cim                   IF(LINK(M).EQ.'CR2') CTIME(M,4)   = CTIME(M,4) + DELT
                                ENDIF
 9070              CONTINUE
C=======================================================================
C     Check for surcharge and maximum depth at junctions.
C=======================================================================
                   DO 9080 J = 1,NJ
                   IF((Z(J)+Y(J)).GT.ZCROWN(J)) SURLEN(J)=SURLEN(J)+DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
C     HERE USE SURELEV IN PLACE OF GRELEV
                   IF((Z(J)+Y(J)).GE.SURELEV(J)-0.000001)
     *                                  FLDLEN(J) = FLDLEN(J)+DELT
                   IF(AS(J).GT.ASMAXX(J))       ASMAXX(J) = AS(J)
                   IF(Y(J).GT.DEPMAX(J)) THEN
                                 DEPMAX(J) = Y(J)
                                 IDHR(J)   = IFIX(TIME/3600.0)
                                 IDMIN(J)  = MINUTE
                                 ENDIF
 9080              CONTINUE
cim check for NSTART
      ENDIF
CIM
 9040              CONTINUE
                   DELT  = RDELT
                   DELT2 = DELT/2.0
cim eliminated by case statements        ENDIF
C=======================================================================
C     End of segment for ISOL = 2.
C=======================================================================
      END SELECT
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
C ### RHF 12/23/96
      QMAX = 0.0
      IF (NOROUT.EQ.1) GO TO 9610
C ---
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
C #### RHF 8/14/96
 9610  CONTINUE
C ----
      IF(ISOL.LE.1) THEN
C ### RHF 12/20/96
      QOUTCS = 0.
      QINCS = 0.
C ---
C    COMPUTE MODEL INFLOWS AND OUTFLOWS
C   FIRST DO INFLOW AND OUTFLOWS
            DO 950 J = 1,NJ
            IF(QIN(J).LT.0.0) QOU(J) = QOU(J) - QIN(J)*DELT
            IF(QIN(J).GE.0.0) QQI(J) = QQI(J) + QIN(J)*DELT
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
C  5.99
C     HERE USE SURELEV IN PLACE OF GRELEV
C  THIS IS OVERFLOW?
           IF(ABS(Y(J)-(SURELEV(J)-Z(J))).LE.0.000001) THEN
		        QOU(J) = QOU(J) + SUMQS(J)*DELT
*				VOL(J) = VOL(J) - SUMQS(J)*DELT
	ENDIF
C ### RHF 12/20/96
c     IF (NOROUT .EQ. 1) GO TO 950
            IF(QIN(J).LT.0.0) QOUTCS = QOUTCS - QIN(J)
            IF(QIN(J).GE.0.0) QINCS = QINCS + QIN(J)
C     HERE USE SURELEV IN PLACE OF GRELEV
            IF(ABS(Y(J)-(SURELEV(J)-Z(J))).LE.0.000001)
     *                                 QOUTCS = QOUTCS + SUMQS(J)
C ---
CIM INTERCONT
CIM  SUMQIN now computed in output.for
cim 950        IF(QIN(J).GE.0.0)          SUMQIN = SUMQIN + QIN(J)*DELT
  950 continue
            I        = 0
C  THESE ARE BOUNDARY CONDUITS
            DO 960 N = NTC+1,NTL
            J        = NJUNC(N,1)
            IF(NJUNC(N,2).LE.0) THEN
                                QOU(J)  = QOU(J) + Q(N)*DELT
                                I       = I+1
                                QOUT(I) = Q(N)
C ### RHF 12/20/96
      QOUTCS = QOUTCS + Q(N)
C ---
                                ENDIF
  960       CONTINUE
C ### RHF 12/20/96 CHECK TOTAL OUTFLOW TO  DETERMINE IF ROUTING SHOULD
C     BE STOPPED
      IF (ISOLSKIP .NE. 1) GO TO 965
cim change this if statement from rhf's original code
C   NEWQIN IS 1 IF STEADY, 0 IF NOT STEADY
cim   IF (NOROUT.EQ.0 .AND. NEWQIN.EQ.0) THEN
cim      WRITE(N6,*) 'QQS',QINCS,QOUTCS
c     IF (NOROUT.EQ.0) THEN
      NOROUTO = NOROUT
      QMINCS = AMIN1(QMINCS,QINCS)
      QMOUCS = AMIN1(QMOUCS,QOUTCS)
      NOROUT = 1
      IF (QOUTCS.GE.QLOWCS) THEN
c  this is case where outflow is greater than minimum
      NOROUT = 0
      SMALLOUTFLOWO = SMALLOUTFLOW
      SMALLOUTFLOW = .FALSE.
      ELSE
      SMALLOUTFLOWO = SMALLOUTFLOW
      SMALLOUTFLOW = .TRUE.
      ENDIF
      IF (NEWQIN.EQ.0) then
c  this is case where inflow at any location changed
      NOROUT = 0
      CONSTINFLOWO = CONSTINFLOW
      CONSTINFLOW = .FALSE.
      ELSE
      CONSTINFLOWO = CONSTINFLOW
      CONSTINFLOW = .TRUE.
      end if
c     IF ((QOUTCS.LT.QLOWCS).AND.(NEWQIN.EQ.1)) THEN
cim      IF (QOUTCS .GT. QLOWCS) GO TO 965
cim      RAT = 1.0
cim      IF (QINCS .GT. 0.01) RAT = QOUTCS / QINCS
cim      IF (RAT .GT. TOLCS1 .OR. RAT .LT. TOLCS2) GO TO 965
      DELTFLOW = ABS(QOUTCS-QINCS)
c      IF (DELTFLOW.LT.ABS(TOLCS1*QINCS)) NOROUT = 1
      IF (DELTFLOW.GT.ABS(TOLCS1*QINCS)) then
c   this is case where outflow differs from inflow
      NOROUT = 0
      EQUALINOUTO = EQUALINOUT
      EQUALINOUT = .FALSE.
      ELSE
      EQUALINOUTO = EQUALINOUT
      EQUALINOUT = .TRUE.
      END IF
c     WRITE(N6,*) 'TRANSX2',QOUTCS, QINCS, DELTFLOW, NOROUT
cim      NOROUT = 1
c     ENDIF
c     IF (NOROUT.EQ.1) Then
      IF ((NOROUTO.EQ.1).AND.(NOROUT.EQ.0)) THEN
C  PROGRAM GOES TO NONSTEADY STATE CONDITIONS
      CALL SSSTATUS(1)
      ENDIF
      IF ((NOROUTO.EQ.0).AND.(NOROUT.EQ.1)) THEN
C  PROGRAM GOES TO NONSTEADY STATE CONDITIONS
      CALL SSSTATUS(4)
C  Reset everything to initial conditions here
      IF(NREDO.GT.0 .AND. KREDO.GT.0) THEN
        REWIND NREDO
        READ(NREDO,ERR=8888,END=8888) MTL,NJJ,NPP
        READ(NREDO,ERR=8888) (Q(N),N=1,MTL),(QO(N),N=1,MTL)
        READ(NREDO,ERR=8888) (V(N),N=1,MTL),(VT(N),N=1,MTL)
        READ(NREDO,ERR=8888) (A(N),N=1,MTL),(AT(N),N=1,MTL)
        READ(NREDO,ERR=8888) (ASFULL(N),N=1,NJJ)
        READ(NREDO,ERR=8888) (Y(N),N=1,NJJ),(YT(N),N=1,NJJ)
        READ(NREDO,ERR=8888) (VWELL(N),N=1,NPP),(JPFUL(N),N=1,NPP)
        DO 962 N   = 1,NTC
        NL        = NJUNC(N,1)
        NH        = NJUNC(N,2)
        H(N,1)    = AMAX1(Y(NL) + Z(NL),ZU(N))
        H(N,2)    = AMAX1(Y(NH) + Z(NH),ZD(N))
 962    CONTINUE
        ENDIF
      ENDIF
      IF (NOROUT.EQ.0) THEN
      DO 963 J = 1, NJ
      YO(J) = Y(J)
  963 CONTINUE
      DO 967 N = 1, NTL
cim this should be N not J
cim   QO(J) = Q(J)
      QO(N) = Q(N)
cim  I think that this next line is needed but will leave in for now
      QOUT(N) = Q(N)
 967  CONTINUE
      ENDIF
 965  CONTINUE
C ---
            ENDIF
C=======================================================================
C     Write output interface file.
C=======================================================================
      IF(NEXT.GT.0) WRITE(NEXT) JULDAY,TIMDAY,DELT,(QOUT(N),N=1,MJSW)
CIM=====================================================================
C     SAVE AND WRITE FLOWS TO ASCII FILE
CIM=====================================================================
      if (noflow.gt.0) CALL WRTFLOW
cim =====================================================================
C     SAVE INTERMEDIATE CONTINUITY RESULTS
CIM=====================================================================
      CALL INTERCON(.false.)
C=======================================================================
C     Check for normal flow, conduit maximum flow and velocity.
C=======================================================================
cim add check for NSTART
cim change check for NSTART to function IPRNTIT (see end of this file)
cim   IF(ISOL.LE.1.AND.ICYC.GE.NSTART) THEN
      IF(ISOL.LE.1.AND.IPRNTIT(ICYC).EQ.1) THEN
C####      IF(ISOL.LE.1) THEN
      DO 902 N = 1,NTL
      IF(N.LE.NTC.AND.ICHECK(N).EQ.IND(2)) SUPLEN(N) = SUPLEN(N) + DELT
C #### RHF 8/14/96
      IF (NOROUT.EQ.1) GO TO 901
C ----
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
C #### RHF 8/14/96
  901 CONTINUE
C ----
      IF(N.LE.NTC) THEN
                   IF(H(N,1).GT.PMAX(N,1)) PMAX(N,1) = H(N,1)
                   IF(H(N,2).GT.PMAX(N,2)) PMAX(N,2) = H(N,2)
cim speed can do away with these four ifs
                  CTIME(N,link(n))   = CTIME(n,link(n)) + DELT
cim                   IF(LINK(N).EQ.'DR1') CTIME(N,1) = CTIME(N,1) + DELT
cim                   IF(LINK(N).EQ.'SUB') CTIME(N,2) = CTIME(N,2) + DELT
cim                   IF(LINK(N).EQ.'CR1') CTIME(N,3) = CTIME(N,3) + DELT
cim                   IF(LINK(N).EQ.'CR2') CTIME(N,4) = CTIME(N,4) + DELT
                   ENDIF
  902 CONTINUE
      ENDIF
C=======================================================================
C     Check for surcharge and maximum depth at junctions.
C=======================================================================
cim include check for nstart
cim change from check to nstart to function IPRNTIT
c      IF(ISOL.LE.1.AND.ICYC.GE.NSTART) THEN
      IF(ISOL.LE.1.AND.IPRNTIT(ICYC).EQ.1) THEN
C####      IF(ISOL.LE.1) THEN
      DO 906 J = 1,NJ
      IF((Z(J)+Y(J)).GT.ZCROWN(J)) SURLEN(J) = SURLEN(J)+DELT
C ### RHF 12/23/96
      IF (NOROUT.EQ.1) GO TO 906
C ---
C###### WCH, 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93.
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT.
C######
C     HERE USE SURELEV IN PLACE OF GRELEV
      IF((Z(J)+Y(J)).GE.SURELEV(J)-0.000001)
     *                             FLDLEN(J) = FLDLEN(J)+DELT
      IF(AS(J).GT.ASMAXX(J))       ASMAXX(J) = AS(J)
      IF(Y(J).GT.DEPMAX(J)) THEN
                            DEPMAX(J) = Y(J)
                            IDHR(J)   = IFIX(TIME/3600.0)
                            IDMIN(J)  = MINUTE
                            ENDIF
  906 CONTINUE
      ENDIF
CIM  CALL WASPCOMP TO COMPUTE AVERAGE FLOWS
CIM  AND WRITE RESULTS AS NECESSARY  5/99
      CALL WASPCOMP
CIM
CIM   TRANAID START
c.......................................................................
c       Update and write out hydraulics data
c.......................................................................
	if (idump .gt. 0) then

		if (time/3600 .ge. hydstr) then
c.....  Add flows to sum-flow array
		ihyd = ihyd + 1
		do n=1,ntl
                qsum(n) = qsum(n) + 0.5*(q(n)+qo(n))
                qisum(n) = qisum(n) + qin(n)
                end do
cim test write
c                write(n6,*) ihyd,ihydwr
c.....  Check to see if it's time to write out information
			if (ihyd .eq. ihydwr) then
			IF (LVCALC) call volume(vsys,LVCALC)
cim note time is not double precision in this program, convert to raytime which is
                        raytime = time/3600.
			write(20) raytime
			write(20) (qsum(n)/float(ihydwr),n=1,ntl)
			write(20) (a(n),n=1,ntl)
			write(20) (r(n),n=1,ntl)
			write(20) (vol(j),j=1,nj)
                        write(20) (qisum(n)/float(ihydwr),n=1,nj)
			write(20) ((y(j)+z(j)),j=1,nj)
			do n=1,ntl
			qsum(n) = 0.0
                        qisum(n) = 0.0
			end do
			ihyd = 0
			end if
		end if
	end if
CIM  TRANAID END
C=======================================================================
C     Check intermediate printout requirements.
C=======================================================================
C
CIM change check for NSTART to FUNCTION IPRNTIT
cim      IF(MOD(ICYC,INTER).EQ.0.AND.ICYC.GE.NSTART) THEN
      IF(MOD(ICYC,INTER).EQ.0.AND.IPRNTIT(ICYC).EQ.1) THEN
C  here call ASCOUT
      IF (IASCII.NE.0) THEN
       CALL ASCOUT(2)
      ELSE
         TMIN   = FLOAT(MINUTE) + FLOAT(JSEC)/60.0
         KHR    = IFIX(TIME/3600.0)
         IF(IT.LE.0.OR.ISOL.GE.2) THEN
                     WRITE(N6,1499) ICYC,KHR,TMIN
                     ELSE
                     WRITE(N6,1500) ICYC,KHR,TMIN,ERROR,IT
                     ENDIF
         WRITE(N6,1501)
CIM 1/99 Write 10 digits and characters
CIM THESE ARE ORIGINAL FORMATS
         IF (JP10.EQ.0) THEN
            IF(JCE.EQ.0) THEN
                      WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
            ELSE
                      WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
            ENDIF
         WRITE(N6,1503)
             IF(JCE.EQ.0) THEN
                      WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
             ELSE
                      WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
             ENDIF
         ELSE
CIM THIS ARE REVISED FORMATS
            IF(JCE.EQ.0) THEN
                      WRITE(N6,61502) (JUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
            ELSE
                      WRITE(N6,61512) (AJUN(J),Y(J),JCHECK(J),
     +                                Y(J)+Z(J),J=1,NJ)
            ENDIF
         WRITE(N6,1503)
             IF(JCE.EQ.0) THEN
                      WRITE(N6,61505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
             ELSE
                      WRITE(N6,61515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
             ENDIF
         ENDIF
         DO 570 J = 1,NJ
C###### WCH 7/14/93, BASED ON CDM MEMO FROM CHUCK MOORE, 6/25/93
C       ALTER CHECK FOR EQUALITY OF DEPTH TO GROUND - INVERT
C######
C     HERE USE SURELEV IN PLACE OF GRELEV
         IF(ABS(Y(J)-(SURELEV(J)-Z(J))).LE.0.000001) THEN
cim 1/99 option to print 10 characters
         IF(JP10.EQ.0) THEN
CIM THESE ARE ORIGINAL FORMATS
         IF(JCE.EQ.0) THEN
         IF(METRIC.EQ.1)WRITE(N6,514) JUN(J),QOU(J),SUMQS(J),TIME/3600.
         IF(METRIC.EQ.2)WRITE(N6,515) JUN(J),QOU(J),SUMQS(J),TIME/3600.
         ELSE
         IF(METRIC.EQ.1)WRITE(N6,524)AJUN(J),QOU(J),SUMQS(J),TIME/3600.
         IF(METRIC.EQ.2)WRITE(N6,525)AJUN(J),QOU(J),SUMQS(J),TIME/3600.
         ENDIF
         ELSE
CIM THESE ARE REVISED FORMATS
         IF(JCE.EQ.0) THEN
         IF(METRIC.EQ.1)WRITE(N6,6514)JUN(J),QOU(J),SUMQS(J),TIME/3600.
         IF(METRIC.EQ.2)WRITE(N6,6515)JUN(J),QOU(J),SUMQS(J),TIME/3600.
         ELSE
        IF(METRIC.EQ.1)WRITE(N6,6524)AJUN(J),QOU(J),SUMQS(J),TIME/3600.
        IF(METRIC.EQ.2)WRITE(N6,6525)AJUN(J),QOU(J),SUMQS(J),TIME/3600.
         ENDIF
         ENDIF
         ENDIF
  570    CONTINUE
         ENDIF
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
cim change nout to sequential unformatted
      IF(JPRINT.EQ.1) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +            JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PRTQ(J),
     +             PRTV(J),J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.2) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +  JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.3) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +                   JSEC,(PRTY(K),PRTH(K),K=1,NHPRT)
      IF(JPRINT.EQ.4) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     + JSEC,(PRTY(K),PRTH(K),K=1,NHPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.5) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     + JSEC,(PRTQ(J),PRTV(J),J=1,NQPRT),(PSF(J,1),PSF(J,2),J=1,NSURF)
      IF(JPRINT.EQ.6) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
     +                           JSEC,(PRTQ(J),PRTV(J),J=1,NQPRT)
      IF(JPRINT.EQ.7) WRITE(NOUT,ERR=8900,IOSTAT=IOS) TIME,MINUTE,
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
      IF (ICONTER.LT. 2147483647) call intercon(.true.)
C=======================================================================
C     Write the final model condition heads and flows.
C=======================================================================
      WRITE(N6,1599) TIME/3600.0
      WRITE(N6,5815) JULDAY,NYEAR,MONTH,NDAY,
     +               TIMDAY/3600.
      WRITE(N6,1501)
cim 1/99 write all 10 digits and characters
CIM THESE ARE ORIGINAL FORMATS
      IF(JP10.EQ.0) THEN
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
          ELSE
                   WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
          ENDIF
      WRITE(N6,1503)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
          ELSE
                   WRITE(N6,1515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
          ENDIF
      ELSE
CIM THESE ARE REVISED FORMATS
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1502) (JUN(J),Y(J),JCHECK(J),
     +                                 Y(J)+Z(J),J=1,NJ)
          ELSE
                   WRITE(N6,1512) (AJUN(J),Y(J),JCHECK(J),
     +                                  Y(J)+Z(J),J=1,NJ)
          ENDIF
      WRITE(N6,1503)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61505) (NCOND(N),Q(N),ICHECK(N),N=1,NTL)
          ELSE
                   WRITE(N6,61515) (ACOND(N),Q(N),ICHECK(N),N=1,NTL)
          ENDIF
      ENDIF
C=======================================================================
C     Compute water volume left in storage.
CIM   TRANAID START
CIM  NOTE THIS IS MOVED TO VOLUME SUBROUTINE
      CALL VOLUME(VLEFT,LVCALC)
CIM TRANAID END
C=======================================================================
C   call ASCOUT for last time
      IF (IASCII.NE.0) CALL ASCOUT(3)
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
C     THIS IS MOVED TO VOLUME.FOR CALLED ABOVE
C     LEAVE LOOP HERE IN CASE NEED UPDATES TO A,V, LINK, h ETC
C=======================================================================
      DO 840 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
C     IF(NKLASS(N).GE.9) GO TO 840
        H(N,1)    = AMAX1(Y(NL) + Z(NL),ZU(N))
        H(N,2)    = AMAX1(Y(NH) + Z(NH),ZD(N))
      CALL NHEAD(N,NL,NH,H(N,1),H(N,2),Q(N),A(N),V(N),HRAD,ANH,ANL,
     +                                      RNL,RNH,IDOIT,LINK(N),AS)
CIM TRANAID START
CIM  Hydraulic radius is saved in R() array in VOLUME
CIM      ASFULL(N) = HRAD
CIM TRANAID END
      QO(N)     = A(N)*LEN(N)
      IF(NGATE.GT.0) THEN
                     DO 845 I = 1,NGATE
                     IF(JGATE(I).EQ.NH.OR.JGATE(I).EQ.NL) GO TO 840
  845                CONTINUE
                     ENDIF
CIM  TRANAID START
CIM  volume calcs now in subroutine VOLUME
CIM      VLEFT     = VLEFT + 0.5*(ANH + ANL)*LEN(N)
CIM  TRANAID END
  840 CONTINUE
C=======================================================================
C     Write further final condition information.
C=======================================================================
CIM 1/99 WRITE ALL 10 DIGITS AND CHARACTERS
      IF (JP10.EQ.0) THEN
CIM THESE ARE ORIGINAL FORMATS
      WRITE(N6,1506)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),V(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),V(N),N=1,NTC)
          ENDIF
      WRITE(N6,1507)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),A(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),A(N),N=1,NTC)
          ENDIF
      WRITE(N6,1527)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),QO(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),QO(N),N=1,NTC)
          ENDIF
      WRITE(N6,1508)
CIM TRANAID START
CIM NOTE THAT R ARRAY NOW HAS HYDRAULIC RADIUS SET IN VOLUME.
CIM CHANGE ASFULL TO R IN FOLLOWING LINES
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1514) (NCOND(N),R(N),N=1,NTC)
          ELSE
                   WRITE(N6,1516) (ACOND(N),R(N),N=1,NTC)
          ENDIF
CIM TRANAID END
      WRITE(N6,1509)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,1517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
          ELSE
                   WRITE(N6,1518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
          ENDIF
      ELSE
CIM THESE ARE REVISED WRITE STATEMENTS
      WRITE(N6,1506)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),V(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),V(N),N=1,NTC)
          ENDIF
      WRITE(N6,1507)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),A(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),A(N),N=1,NTC)
          ENDIF
      WRITE(N6,1527)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),QO(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),QO(N),N=1,NTC)
          ENDIF
      WRITE(N6,1508)
CIM TRANAID START
CIM NOTE THAT R ARRAY NOW HAS HYDRAULIC RADIUS SET IN VOLUME.
CIM CHANGE ASFULL TO R IN FOLLOWING LINES
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61514) (NCOND(N),R(N),N=1,NTC)
          ELSE
                   WRITE(N6,61516) (ACOND(N),R(N),N=1,NTC)
          ENDIF
CIM TRANAID END
      WRITE(N6,1509)
          IF(JCE.EQ.0) THEN
                   WRITE(N6,61517) (NCOND(N),H(N,1),H(N,2),N=1,NTC)
          ELSE
                   WRITE(N6,61518) (ACOND(N),H(N,1),H(N,2),N=1,NTC)
          ENDIF
      ENDIF
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
      IF(JCE.EQ.0) THEN
	   WRITE(N6,1621) (NCOND(N),BADMAX(N)/60.0,N=1,NTC)
	ELSE
         WRITE(N6,1622) (ACOND(N),BADMAX(N)/60.0,N=1,NTC)
	ENDIF
      WRITE(N6,1625)
      IF(JCE.EQ.0) THEN
	   WRITE(N6,1621) (NCOND(N),BADTIM(N)/DENOM,N=1,NTC)
	ELSE
         WRITE(N6,1622) (ACOND(N),BADTIM(N)/DENOM,N=1,NTC)
	ENDIF
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
      IF(JCE.EQ.0) THEN
	    WRITE(N6,1621) (NCOND(N),BADMAX(N)/60.0,N=1,NTC)
	ELSE
          WRITE(N6,1622) (ACOND(N),BADMAX(N)/60.0,N=1,NTC)
	ENDIF
      WRITE(N6,1625)
      IF(JCE.EQ.0) THEN
	    WRITE(N6,1621) (NCOND(N),BADTIM(N)/WENOM,N=1,NTC)
	ELSE
          WRITE(N6,1622) (ACOND(N),BADTIM(N)/WENOM,N=1,NTC)
	ENDIF
      ENDIF
C ### RHF 10/16/96
      IF (ISOLSKIP.EQ.1) write(n6,8700)NTIMCS,QMINCS,QMOUCS
 8700 FORMAT(/,' NUMBER OF DYNAMIC ROUTING TIME STEPS (NTIMCS):',I11,/,
     +' MINIMUM TOTAL INFLOW  = ',f10.3,'CFS',/,
     +' MINIMUM TOTAL OUTFLOW = ',f10.3,'CFS',/)
C ---
      RETURN
C=======================================================================
C#### WCH, 11/5/93.  ALSO WRITE TO THE SCREEN AND ADD IOSTAT TO 8905.
 8888 WRITE(N6,8885)
      WRITE(*,8885)
      STOP
 8889 WRITE(N6,8886)
      WRITE(*,8886)
      STOP
C#### WCH, 8/4/95.  ALTER IOSTAT FOR LAHEY.
 8900 WRITE(N6,8905) MOD(IOS,256)
      WRITE(*,8905)  MOD(IOS,256)
      STOP
C=======================================================================
cim change format to avoid stars for very long runs  4/30/97
cim   21 FORMAT('+',6X,I5,9X,3I9,1X,2(A8,F7.3))
cim   22 FORMAT('+',6X,I5,9X,3I9,1X,2(I8,F7.3))
   21 FORMAT('+',I10,I10,2I4,2(1X,A10,F7.3),2I2,I10)
   22 FORMAT('+',I10,I10,2I4,2(1X,I10,F7.3),2I2,I10)


   23 FORMAT(/
     +' ************************************************************',/,
     +' * STEP ==>  Time step                                      *',/,
     +' * ITER ==>  # of iterations (including surcharge)          *',/,
     +' *  NRM ==>  # of conduits using normal flow equation       *',/,
     +' *   SJ ==>  # of surcharged junctions.                     *',/,
     +' * COND ==>  # conduit with the greatest flow change        *',/,
     +' *             ( Q(n+1) - Q(n) ) / Qfull = QMAX             *',/,
     +' * JUNC ==>  # junction with the greatest depth change      *',/,
     +' *             ( Y(n+1) - Y(n) ) / Yfull = YMAX             *')
 923  FORMAT(
     +' *   I1 ==>  = 1 when steady state conditions (ISOL 4)      *',/,
     +' *   I2 ==>  = 1 when inflows are steady                    *',/,
     +' * CALL ==>  Number of calls to routing subroutines         *')
 924  FORMAT(
     +' ************************************************************',/)
   24 FORMAT(
     +' TOTAL # OF JUNCTIONS ==> ',I5,/
     +' TOTAL # OF CONDUITS  ==> ',I5,//,
     +' BEGINNING LOOP THRU ',I11,' TIME STEPS',/)
   27 FORMAT(
     +6X,'STEP',6X,'ITER',1X,'NRM',2X,'SJ',7X,'COND',3X,'QMAX',7X,
     +'JUNC',3X,'YMAX',/)
   28 FORMAT(
     +6X,'STEP',6X,'ITER',1X,'NRM',2X,'SJ',7X,'COND',3X,'QMAX',7X,
     +'JUNC',3X,'YMAX','I1I2',6X,'CALL',/)
  514 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I9,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
  515 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I9,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
  524 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A9,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
  525 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A9,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
cim 1/99  print all 10 characters
 6514 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I10,1X,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
 6515 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',I10,1X,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
 6524 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A10,1X,1PE10.2,
     +         ' CU.FT.  FLOOD FLOW =  ',0PF10.1,' CFS AT HOUR ',F6.2)
 6525 FORMAT(/,' CUMULATIVE OVERFLOW VOLUME FROM NODE ',A10,1X,1PE10.2,
     +         ' CU.MET.  FLOOD FLOW =  ',0PF10.1,' CMS AT HOUR ',F6.2)
cim  change to avoid stars on long simulations
cim 1499 FORMAT(/,1X,'CYCLE ',I5,6X,'TIME ',I4,' HRS - ',F5.2,' MIN')
cim 1500 FORMAT(/,1X,'CYCLE ',I5,6X,'TIME ',I4,' HRS - ',F5.2,
 1499 FORMAT(/,1X,'CYCLE ',I10,1X,'TIME ',I4,' HRS - ',F5.2,' MIN')
 1500 FORMAT(/,1X,'CYCLE ',I10,1X,'TIME ',I4,' HRS - ',F5.2,
     *' MIN  FLOW DIFFERENTIAL IN SURCHARGED AREA = ',F6.2,
     *'CFS  ITERATIONS REQUIRED = ',I3,/)
 1501 FORMAT(/,' JUNCTION / DEPTH  / ELEVATION ',
     +         '   ===>  "*" JUNCTION IS SURCHARGED.')
 1502 FORMAT(3(I10,'/',F7.2,A1,'/',F9.2))
61502 FORMAT(3(1X,I10,'/',F7.2,A1,'/',F9.2))

 1503 FORMAT(/,'   CONDUIT/       FLOW',
     +         '   ===> "*" CONDUIT USES THE NORMAL FLOW OPTION.')
 1505 FORMAT(4(I10,'/',F11.2,A1))
61505 FORMAT(4(1X,I10,'/',F11.2,A1))
 1506 FORMAT(/,'   CONDUIT/   VELOCITY')
 1507 FORMAT(/,'   CONDUIT/ CROSS SECTIONAL AREA')
 1508 FORMAT(/,'   CONDUIT/ HYDRAULIC RADIUS')
 1509 FORMAT(/,'   CONDUIT/ UPSTREAM/ DOWNSTREAM ELEVATION')
 1512 FORMAT(3(1X,A9,'/',F7.2,A1,'/',F9.2))
61512 FORMAT(3(1X,A10,'/',F7.2,A1,'/',F9.2))
 1514 FORMAT(4(I10,'/',F11.2,1X))
61514 FORMAT(4(1X,I10,'/',F11.2,1X))
 1515 FORMAT(4(1X,A9,'/',F11.2,A1))
61515 FORMAT(4(1X,A10,'/',F11.2,A1))
 1516 FORMAT(4(1X,A9,'/',F11.2,1X))
61516 FORMAT(4(1X,A10,'/',F11.2,1X))
 1517 FORMAT(3(I10,'/',F9.2,'/',F9.2))
61517 FORMAT(3(1X,I10,'/',F9.2,'/',F9.2))
 1518 FORMAT(3(1X,A9,'/',F9.2,'/',F9.2))
61518 FORMAT(3(1X,A10,'/',F9.2,'/',F9.2))
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
cim print final date and time
C#### WCH, 7/25/96.
 5815 FORMAT(/,' >>> ENDING DATE AND TIME OF EXTRAN RUN ARE:',/,
     2' JULIAN DATE:',I8,/,
     3' YR/MO/DA:   ',I4,'/',I2,'/',I2,/,
     4' TIME OF DAY:',F7.3,' HRS')
c
 1600 FORMAT(///,
     +' ###################################################',/,
     +' #           Surcharge Iteration Summary           #',/,
     +' ###################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
cim change format to avoid stars for very long runs  4/30/97
     +' Total number of iterations in the simulation..',I11,/,
     +' Average number of iterations per time step......',F9.2,/,
     +' Surcharge iterations during the simulation......',I9,/,
     +' Maximum surcharge flow error during simulation..',1PE9.2,' cfs',
cim change format to avoid stars for very long runs  4/30/97
     +/,' Total number of time steps during simulation..',I11,/)
 1605 FORMAT(///,
     +' ###################################################',/,
     +' #           Surcharge Iteration Summary           #',/,
     +' ###################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
cim change format to avoid stars for very long runs  4/30/97
     +' Total number of iterations in the simulation..',I11,/,
     +' Average number of iterations per time step......',F9.2,/,
     +' Surcharge iterations during the simulation......',I9,/,
     +' Maximum surcharge flow error during simulation..',1PE9.2,' cms',
cim change format to avoid stars for very long runs  4/30/97
     +/,' Total number of time steps during simulation..',I11,/)
 1606 FORMAT(///,
     +' ####################################################',/,
     +' #             Extran Iteration Summary             #',/,
     +' ####################################################',//,
     +' Maximum number of iterations in a time step.....',I9,/,
cim change format to avoid stars for very long runs  4/30/97
     +' Total number of iterations in the simulation..',I11,/,
     +' Maximum continuity error during simulation......',F9.4,
     +' fraction',/,
cim change format to avoid stars for very long runs  4/30/97
     +' Total number of time steps during simulation..',I11,/,
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
cim 7000 FORMAT(200(I10,1X))
cim 7010 FORMAT(200(A10,1X))
cim 7020 FORMAT(E12.5,2I7,200(E12.5,1X))
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
C#### WCH, 1/22/97.
 9400 FORMAT(/,' WARNING! FOR CONDUIT ',I10,', SEQUENCE NO.',I4,/,
     1 ' QFULL .LE. 0.  DIVIDE BY ZERO ERROR LIKELY IN FUTURE.')
 9401 FORMAT(/,' WARNING! FOR CONDUIT ',A10,', SEQUENCE NO.',I4,/,
     1 ' QFULL .LE. 0.  DIVIDE BY ZERO ERROR LIKELY IN FUTURE.')
 9402 FORMAT(' QFULL=',F9.3,' VEL=',F9.3,' RFULL=',F9.3,/,
     1 ' AFULL=',F9.3,' SLOPE=',F9.4)
C=======================================================================
      END


      FUNCTION IPRNTIT(ICYC)
C   RETURNS 1 IF IN RANGE OF STARTING AND ENDING OF CURRENT PRINT PERIOD
      INCLUDE 'BE.INC'
      IF ((ICYC.GE.IBESTART(IBE)).AND.(ICYC.LE.IBEEND(IBE))) THEN
      IPRNTIT = 1
      ELSE
      IPRNTIT = 0
      ENDIF
      RETURN
      END

