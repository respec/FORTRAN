      SUBROUTINE INDAT2
C	EXTRAN BLOCK
C     CALLED BY EXTRAN NEAR LINE 199
C=======================================================================
C     This subroutine reads data groups E1-I2
C     except for hydrograph lines in 'INFLOW'.
C     IT ALSO PERFORMS SOME INITIALIZATION.   ALL NODE-CONDUIT LINKAGES
C     ARE SET UP AND CONVERTED TO THE INTERNAL NUMBER SYSTEM.
C     Additional error message placed by WCH, 8/28/92.
C     Add error check for minimum values of stage-area data, and correct
C       power function volume calculation, WCH, 12/8/94.
C     Add option for printing of detailed storage junction input data,
C       WCH, 12/8/94.
C     Alter E1 data error check to avoid conflict between Sub. ERROR and
C       variable ERROR, WCH, 1/23/95.
C     Correct check for weir/pump outfall junctions, allowing these to
C       be used, RED, 3/12/95.
C     Above correction also for outfalls with gates! WCH (RED), 3/16/95.
C     Increase allowable number of connecting channels to a junction,
C       WCH, 8/4/95.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'VOLDAT.INC'
CIM BE contains JELEV
      INCLUDE 'BE.INC'
cim pp 5/1/97
CIM      DIMENSION ISWTCH(4)
CIM 4/30/97 Change to make it less painfull to change program dimensions.
cim change dimensions of ONAME, WNAME,PNAME,OUTF, and OUTG to the parameter
cim values defined in TAPES.INC
      CHARACTER JTYPE(3)*10,ONAME(NEO)*8,WNAME(NEW)*8,
     +            PNAME(NEP)*8,OUTF(NTG)*8,OUTG(NTG)*8,BMJ*10
      CHARACTER TMPSTR*8
CIM START OOOOOOOOOOOO
      LOGICAL CIRCULAR
      CHARACTER*10 KCNODE
      DIMENSION DEEPO(NEO),WIDEO(NEO)
cim   newklass dimensioned for four basic orifice types: circle side,
cim   circle bottom, rectangle side, rectangle bottom
      DIMENSION NEWKLASS(4)
CIM END  OOOOOOOOOOOO
      DATA JTYPE/'  CONSTANT','  VARIABLE',' POWER FNC'/
C=======================================================================
CIM initialize string variables
CIM This will work as long as NEO, NEP, NEW, and NTG are less than 1000.
cim
CIM initialize ONAME with dimension of NEO
      DO I = 1,NEO
      IF (I.LT.10) THEN
      WRITE(TMPSTR,'(I1)') I
      ONAME(I) = 'ORF   #'//TMPSTR
      ELSE
      IF (I.LT.100) THEN
      WRITE(TMPSTR,'(I2)') I
      ONAME(I) = 'ORF  #'//TMPSTR
      ELSE
      WRITE(TMPSTR,'(I3)') I
      ONAME(I) = 'ORF #'//TMPSTR
      ENDIF
      ENDIF
      ENDDO
CIM initialize WNAME with dimension of NEW
      DO I = 1,NEW
      IF (I.LT.10) THEN
      WRITE(TMPSTR,'(I1)') I
      WNAME(I) = 'WEIR  #'//TMPSTR
      ELSE
      IF (I.LT.100) THEN
      WRITE(TMPSTR,'(I2)') I
      WNAME(I) = 'WEIR #'//TMPSTR
      ELSE
      WRITE(TMPSTR,'(I3)') I
      WNAME(I) = 'WEIR#'//TMPSTR
      ENDIF
      ENDIF
      ENDDO
CIM initialize PNAME with dimension of NEP
      DO I = 1,NEP
      IF (I.LT.10) THEN
      WRITE(TMPSTR,'(I1)') I
      PNAME(I) = 'PUMP  #'//TMPSTR
      ELSE
      IF (I.LT.100) THEN
      WRITE(TMPSTR,'(I2)') I
      PNAME(I) = 'PUMP #'//TMPSTR
      ELSE
      WRITE(TMPSTR,'(I3)') I
      PNAME(I) = 'PUMP#'//TMPSTR
      ENDIF
      ENDIF
      ENDDO
CIM initialize OUTF with dimension of NTG
      DO I = 1,NTG
      IF (I.LT.10) THEN
      WRITE(TMPSTR,'(I1)') I
      OUTF(I) = 'FREE  #'//TMPSTR
      ELSE
      IF (I.LT.100) THEN
      WRITE(TMPSTR,'(I2)') I
      OUTF(I) = 'FREE #'//TMPSTR
      ELSE
      WRITE(TMPSTR,'(I3)') I
      OUTF(I) = 'FREE#'//TMPSTR
      ENDIF
      ENDIF
      ENDDO
CIM initialize OUTG with dimension of NTG
      DO I = 1,NTG
      IF (I.LT.10) THEN
      WRITE(TMPSTR,'(I1)') I
      OUTG(I) = 'GATE  #'//TMPSTR
      ELSE
      IF (I.LT.100) THEN
      WRITE(TMPSTR,'(I2)') I
      OUTG(I) = 'GATE #'//TMPSTR
      ELSE
      WRITE(TMPSTR,'(I3)') I
      OUTG(I) = 'GATE#'//TMPSTR
      ENDIF
      ENDIF
      ENDDO
CIM END
C=======================================================================
C#### WCH, 12/8/94.  READ STRICTLY OPTIONAL E0 LINE TO CONTROL PRINTING
C     OF VARIABLE STORAGE JUNCTION INPUT DATA.
C     NVSPR =  0 - DON'T ECHO DETAILED STORAGE JUNCTION INPUT DATA.
C     NVSPR NE 0 - PRINT DETAILED DATA.
C=======================================================================
      NVSPR = 0
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'E0') READ(N5,*,ERR=888) CC,NVSPR
C=======================================================================
C     READ STORAGE JUNCTION DATA ON DATA GROUP E1
C=======================================================================
cim move this up to here
          IF(METRIC.EQ.1) QCOEF = 43560.0
          IF(METRIC.EQ.2) QCOEF = 10000.0
cim
      DO 640 I = 1,NVSE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'E1') GO TO 645
C=======================================================================
C#### WCH, 12/8/94.  PRINT STORAGE JUNCTION HEADER FOR DETAILED PRINT.
C=======================================================================
      IF(NVSPR.NE.0) THEN
           WRITE(N6,2999)
           WRITE(N6,5060) ALPHA1,ALPHA2
           WRITE(N6,5397)
           ENDIF
C=======================================================================
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,JSTORE(I),ZTOP(I),
     +                                ASTORE(I),NUMST
      ELSE
                   READ(N5,*,ERR=888) CC,KSTORE(I),ZTOP(I),
     +                                ASTORE(I),NUMST
      ENDIF
C=======================================================================
C     READ VARIABLE STORAGE JUNCTION DATA
C#### WCH, 1/23/95.  WRITE OUT ERROR 119 HERE.  DON'T CALL SUB. ERROR.
C=======================================================================
      IF(ASTORE(I).LT.0.0.AND.NUMST.GT.0) THEN
C####          IF(NUMST.GT.NVST) CALL ERROR(119)
          IF(NUMST.GT.NVST) THEN
               WRITE(N6,9119)
               STOP ' PROGRAM STOPPED BEFORE E2 LINE DATA IN EXTRAN'
               ENDIF
C
          NUMV(I) = NUMST
          READ(N5,*,ERR=888) CC
C#### WCH, 12/7/94.  IF USING GOTO888, HELPFUL TO SET CC = 'E2'.
C####          IF(CC.NE.'E2') GO TO 888
          IF(CC.NE.'E2') THEN
               CC = 'E2'
               GO TO 888
               ENDIF
C
          BACKSPACE N5
          READ(N5,*,ERR=888) CC,(VCURVE(I,1,J),VCURVE(I,2,J),J=1,NUMST)
CIM If JELEV = 3 or 4 then convert elevation to depth here.
	    IF (JELEV.GE.3) THEN
CIM FIND CORRESPONDING JUNCTION
          IF (JCE.EQ.0) THEN
          DO JUNCIM = 1, NJ
           IF(JSTORE(I).EQ.JUN(JUNCIM))  GO TO 498
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ELSE
	    DO JUNCIM = 1,NJ
          IF(KSTORE(I).EQ.AJUN(JUNCIM)) GO TO 498
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ENDIF
  498 CONTINUE
		DO J = 1 , NUMST
	    VCURVE(I,2,J) = VCURVE(I,2,J) - Z(JUNCIM)
	    ENDDO
	    ENDIF
CIM
          DO 252 JES = 2,NUMST
C#### WCH, 12/8/94.  PRINT JUNCTION ID WITH WARNING.
          IF(VCURVE(I,2,JES-1).GT.VCURVE(I,2,JES)) THEN
               IF(JCE.EQ.0) THEN
                  WRITE(N6,998) JSTORE(I),
     1                    VCURVE(I,2,JES-1),VCURVE(I,2,JES)
               ELSE
                  WRITE(N6,999) KSTORE(I),
     1                    VCURVE(I,2,JES-1),VCURVE(I,2,JES)
               ENDIF
          ENDIF
 252      CONTINUE
C=======================================================================
C#### WCH, 12/8/94.  CHECK TO BE SURE MIN. DEPTH = 0 AND MIN. AREA > 0.
C=======================================================================
          IF(VCURVE(I,2,1).GT.0.0.OR.VCURVE(I,1,1).LE.0.0) THEN
               IF(JCE.EQ.0) THEN
                 WRITE(N6,990) JSTORE(I)
               ELSE
                 WRITE(N6,991) KSTORE(I)
               ENDIF
               IF(VCURVE(I,2,1).GT.0.0) THEN
                    WRITE(N6,992) VCURVE(I,2,1)
                    VCURVE(I,2,1) = 0.0
                    ENDIF
               IF(VCURVE(I,1,1).LE.0.0) THEN
                    WRITE(N6,993) VCURVE(I,1,1)
cim this correction to avoid mixing units
cim amen is in square feet
cim area read in as acres or hectares and coverted below.  Should still be acres or hectares here.
                    VCURVE(I,1,1) = AMEN/QCOEF
cim
                    ENDIF
               ENDIF
C=======================================================================
C         INTEGRATE VOLUME CURVE FROM AREA/STAGE DATA
C=======================================================================
          VCURVE(I,3,1)         = 0.0
cim          IF(METRIC.EQ.1) QCOEF = 43560.0
cim          IF(METRIC.EQ.2) QCOEF = 10000.0
          VCURVE(I,1,1) = VCURVE(I,1,1)*QCOEF
          DO 251      J = 2,NUMST
          VCURVE(I,1,J) = VCURVE(I,1,J)*QCOEF
  251     VCURVE(I,3,J) = VCURVE(I,3,J-1)+
     +                    (VCURVE(I,2,J)-VCURVE(I,2,J-1))*
     +                    (VCURVE(I,1,J)+VCURVE(I,1,J-1))/2.0
C=======================================================================
C#### WCH, 12/8/94.  OPTIONAL PRINT-OUT OF VARIABLE AREA DATA.
C=======================================================================
          IF(NVSPR.NE.0) THEN
               IF(JCE.EQ.0) THEN
                 WRITE(N6,890) JSTORE(I)
               ELSE
                 WRITE(N6,891) KSTORE(I)
               ENDIF
               IF(METRIC.EQ.1) WRITE(N6,892)
               IF(METRIC.EQ.2) WRITE(N6,893)
               WRITE(N6,894) (K,VCURVE(I,2,K),VCURVE(I,1,K)/QCOEF,
     1                   VCURVE(I,3,K)/1000.0,K=1,NUMST)
               ENDIF
C=======================================================================
          ENDIF
C=======================================================================
C     READ VARIABLE STORAGE JUNCTION DATA DEFINED AS A POWER FUNCTION
C     VCURVE(I,1,1) = COEFFICIENT
C     VCURVE(I,2,1) = POWER
C=======================================================================
      IF(NUMST.LT.0) THEN
                      NUMV(I)   =   NUMST
                      ASTORE(I) = -1234.0
                      READ(N5,*,ERR=888) CC
                      IF(CC.NE.'E2') GO TO 888
                      BACKSPACE N5
                      READ(N5,*,ERR=888) CC,VCURVE(I,1,1),VCURVE(I,2,1)
C=======================================================================
C#### WCH, 12/8/94.  OPTIONAL PRINT-OUT OF INPUT DATA.
C=======================================================================
                      IF(NVSPR.NE.0) THEN
                           IF(JCE.EQ.0) THEN
                             WRITE(N6,895) JSTORE(I),
     1                       VCURVE(I,1,1),VCURVE(I,2,1)
                           ELSE
                             WRITE(N6,896) KSTORE(I),
     1                       VCURVE(I,1,1),VCURVE(I,2,1)
                           ENDIF
                      ENDIF
C
                      ENDIF
  640 CONTINUE
  645 NSTORE = I-1
C=======================================================================
C     WRITE THE PAGE TITLES
C=======================================================================
      IF(NSTORE.GT.0) THEN
                      WRITE(N6,2999)
                      WRITE(N6,5060) ALPHA1,ALPHA2
                      WRITE(N6,5398)
      IF(METRIC.EQ.1) WRITE(N6,5495)
      IF(METRIC.EQ.2) WRITE(N6,5496)
C=======================================================================
C     CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
      DO 646 I = 1,NSTORE
      DO 648 J = 1,NJ
      IF(JCE.EQ.0.AND.JSTORE(I).EQ.JUN(J))  GO TO 650
      IF(JCE.EQ.1.AND.KSTORE(I).EQ.AJUN(J)) GO TO 650
  648 CONTINUE
      IF(JCE.EQ.0) THEN
       WRITE(N6,5494) JSTORE(I)
      ELSE
       WRITE(N6,5484) KSTORE(I)
      ENDIF
      NSTOP     = NSTOP + 1
  650 JSTORE(I) = J
      JSKIP(J)  = 0
cim  zcrown is used to define when things go into surcharge
cim  I don't think that it is ever used to reduce surface area
cim  for constant surface area storage nodes.  ZCROWN is set
cim  to the crown elevation of the highest pipe in INDAT1.  If
cim  ZTOP is less than ZCROWN defined previously, don't let it
cim  reduce it below the highest pipe to avoid going to surcharge
cim  calculations too soon.
cim      ZCROWN(J) = ZTOP(I)
      IF (ZTOP(I).GE.ZCROWN(J)) THEN
         ZCROWN(J) = ZTOP(I)
      ELSE
         IF(JCE.EQ.0) THEN
            WRITE(N6,8005) JSTORE(I),ZTOP(I),ZCROWN(J)
            ELSE
            WRITE(N6,8006) KSTORE(I),ZTOP(I),ZCROWN(J)
            ENDIF
         ZTOP(I) = ZCROWN(J)
      ENDIF
      JJ        = NUMV(I)
      IF(ZCROWN(J).GT.GRELEV(J)) THEN
	   WRITE(N6,7005) 
	       GRELEV(J) = ZCROWN(J) + 0.1
	IF (GRELEV(J).GT.SURELEV(J)) THEN
	   WRITE(N6,7010)
	       SURELEV(J) = GRELEV(J)
	ENDIF
	ENDIF
      IF(ASTORE(I).GT.0) THEN
                         CF  = ASTORE(I)*(ZTOP(I)-Z(J))
                         AF  = ASTORE(I)
                         JJJ = 1
                         ENDIF
      IF(ASTORE(I).LT.0.AND.JJ.GT.0) THEN
                                     CF  = VCURVE(I,3,JJ)
                                     AF  = VCURVE(I,1,JJ)
                                     JJJ = 2
                                     ENDIF
      IF(ASTORE(I).LT.0.AND.JJ.LT.0) THEN
                                     AF  = VCURVE(I,1,1) *
     +                                (ZTOP(I)-Z(J))**VCURVE(I,2,1)
C#### WCH, 12/8/94.  MUST >>INTEGRATE<< POWER FUNCTION TO GET VOLUME!
C     ALSO, USE MINIMUM AREA = AMEN.
C####                                     CF  = AF*(ZTOP(I) - Z(J))
                                     CF = AMEN*(ZTOP(I)-Z(J))
                                     CF = CF + VCURVE(I,1,1) *
     +             (ZTOP(I)-Z(J))**(VCURVE(I,2,1)+1)/(VCURVE(I,2,1)+1)
                                     JJJ = 3
                                     ENDIF
      LSTORE  = JSTORE(I)
      IF(JCE.EQ.0) THEN
       WRITE(N6,5399)  JUN(LSTORE),JTYPE(JJJ),AF,CF,ZTOP(I)
      ELSE
       WRITE(N6,5499) AJUN(LSTORE),JTYPE(JJJ),AF,CF,ZTOP(I)
      ENDIF
  646 CONTINUE
      NTL = NTL + NSTORE
      ENDIF
C=======================================================================
C     INITIALIZE NTC AND NTL
C=======================================================================
      NTC  = NC
      NTL  = NC
      NVOR = 0
CIM START   OOOOOOOOOO
      NOGATES = 0
CIM END  OOOOOOOOOOO
C=======================================================================
C     Read orifice data on data group F1.
C=======================================================================
cim start  <><><><><><><><><><>
C     NKLASS(N) = 1, NEWKLASS(1) = 51 - SIDE OUTLET CIRCULAR ORIFICE
C     NKLASS(N) = 2, NEWKLASS(2) = 52 - BOTTOM OUTLET (SUMP) CIRCULAR ORIFICE
CIM START OOOOOOOOO
CIM   NKLASS(N) = 3, NEWKLASS(3) = 53 - SIDE OUTLET RECTANGULAR ORIFICE
CIM   NKLASS(N) = 4, NEWKLASS(4) = 54 - BOTTOM OUTLET (SUMP) RECTANGULAR ORIFICE
CIM  END  OOOOOOOO
      NEWKLASS(1) = 51
      NEWKLASS(2) = 52
CIM START  OOOOOOOOO
      NEWKLASS(3) = 53
      NEWKLASS(4) = 54
CIM END  OOOOOOOOO
cim end      <><><><><><><><>
C=======================================================================
C=======================================================================
      DO 690 I = 1,NEO
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'F1') GO TO 695
      N = NTC+I
      IF(JCE.EQ.0) THEN
          READ(N5,*,ERR=888) CC,(NJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N)
      ELSE
          READ(N5,*,ERR=888) CC,(KJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N)
      ENDIF
CIM START  OOOOOOOOO
C  MODIFICATION TO READ DEEPO AND WIDEO FOR RECTANGULAR ORIFICE
      CIRCULAR = .TRUE.
      IF ((NKLASS(N).EQ.3) .OR.
     .   (NKLASS(N).EQ.4) .OR.
     .   (NKLASS(N).EQ.-3) .OR.
     .   (NKLASS(N).EQ.-4) .OR.
     .   (NKLASS(N).EQ.13) .OR.
     .   (NKLASS(N).EQ.14) .OR.
     .   (NKLASS(N).EQ.23) .OR.
     .   (NKLASS(N).EQ.24)) THEN
      CIRCULAR = .FALSE.
      BACKSPACE N5
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,(NJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N),
     A                       DEEPO(I),WIDEO(I)
      ELSE
                   READ(N5,*,ERR=888) CC,(KJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N),
     A                       DEEPO(I),WIDEO(I)
      ENDIF
	IF (AORIF(I).EQ.0.0) AORIF(I) = DEEPO(I)*WIDEO(I)
      ENDIF
CIM change this next one to key on CORIF
CIM   IF CORIF < 0 then gate closes from the bottom meaning that the
CIM   ZU and ZP change as gate opening changes.
      IF (CORIF(I).LT.0.0) then
      CORIF(I) = - CORIF(I)
      IOINV(I) = 1
      ELSE
      IOINV(I) = 0
      ENDIF
CIM END  OOOOOOOOOO
CIM CHANGE FOR JELEV = 4.  ZU is elevation not depth
	IF (JELEV.EQ.4) THEN
CIM FIND UPSTREAM OR FROM JUNCTION INVERT ELEVATION 
          IF (JCE.EQ.0) THEN
          DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 678
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ELSE
	    DO JUNCIM = 1,NJ
          IF(KJUNC(N,1).EQ.AJUN(JUNCIM)) GO TO 678
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ENDIF
  678 CONTINUE
	ZU(N) = ZU(N) - Z(JUNCIM)
	ENDIF
CIM INSERT CHECK FOR NEGATIVE ZU
	IF (ZU(N).LT.0.0) THEN
	WRITE(N6,5442) I
	NSTOP = NSTOP + 1
	ENDIF
C=======================================================================
C     READ F2 DATA GROUP FOR ORIFICES WITH A TIME HISTORY
C=======================================================================
      IF(NKLASS(N).LT.0) THEN
                         NVOR      = NVOR + 1
                         NKLASS(N) = IABS(NKLASS(N))
                         READ(N5,*,ERR=888) CC,NTIME,(VORIF(NVOR,J,1),
     +                     VORIF(NVOR,J,2),VORIF(NVOR,J,3),J=1,NTIME)
                         ENDIF
CIM START  OOOOOOOO
CIM=======================================================================
CIM      READ F3 DATA GROUP FOR ORIFICES WITH TIMED CLOSURE GATE CONTROL
CIM=======================================================================
      IF((NKLASS(N).GT.10).AND.NKLASS(N).LT.20) THEN
            NOGATES=NOGATES+1
            NKLASS(N) = NKLASS(N)-10
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,ICNODE(I),OOPEN(I),OCLOSE(I),
     .                                OCAREA(I),ORATE(I),IDIR(I),
     .                                IOPRNT(I)
      ELSE
                   READ(N5,*,ERR=888) CC,KCNODE,OOPEN(I),OCLOSE(I),
     .                                OCAREA(I),ORATE(I),IDIR(I),
     .                                IOPRNT(I)
      ENDIF
CIM   IF JELEV IS 4 THEN CONVERT OOPEN AND OCLOSE FROM ELEVATIONS TO DEPTHS HERE
      IF (JELEV.EQ.4) THEN
CIM FIND CONTROL JUNCTION INVERT ELEVATION 
          IF (JCE.EQ.0) THEN
          DO JUNCIM = 1, NJ
           IF(ICNODE(I).EQ.JUN(JUNCIM))  GO TO 679
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ELSE
	    DO JUNCIM = 1,NJ
          IF(KCNODE.EQ.AJUN(JUNCIM)) GO TO 679
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ENDIF
  679 CONTINUE
	OOPEN(I) = OOPEN(I) - Z(JUNCIM)
	OCLOSE(I) = OCLOSE(I) - Z(JUNCIM)
	ENDIF
CIM ADD A CHECK FOR NEGATIVE OOPEN AND OOCLOSE
	IF ((OOPEN(I).LT.0.0) .OR. (OCLOSE(I).LT.0.0)) THEN
	WRITE(N6,5443) I
	NSTOP = NSTOP + 1
	ENDIF
CIM  ECHO PRINT OF TIMED CLOSURE GATED CONTROL HERE
      IF(JCE.EQ.0) THEN
                   WRITE(N6,660) I,ICNODE(I),OOPEN(I),OCLOSE(I),
     .                           OCAREA(I),ORATE(I),IDIR(I),IOPRNT(I)
      ELSE
                   WRITE(N6,661) I,KCNODE,OOPEN(I),OCLOSE(I),
     .                           OCAREA(I),ORATE(I),IDIR(I),IOPRNT(I)
      ENDIF
      DO 680 J = 1,NJ
      IF(JCE.EQ.0.AND.ICNODE(I).EQ.JUN(J))  GO TO 681
      IF(JCE.EQ.1.AND.KCNODE.EQ.AJUN(J)) GO TO 681
  680 CONTINUE
      IF(JCE.EQ.0) THEN
       WRITE(N6,5445) ICNODE(I)
      ELSE
       WRITE(N6,5446) KCNODE
      ENDIF
      NSTOP      = NSTOP+1
CIM   NOTE THAT TIMED CLOSURE GATED CONTROL ORIFICES HAVE NEGATIVE ICNODE
  681 ICNODE(I) = -J
      END IF
CIM=======================================================================
CIM      READ F4 DATA GROUP FOR ORIFICES WITH HEAD DEPENDENT GATE CONTROL
CIM=======================================================================
      IF (NKLASS(N).GT.20) THEN
      NOGATES=NOGATES+1
            NKLASS(N) = NKLASS(N)-20
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,ICNODE(I),OOPEN(I),OCLOSE(I),
     .                                OCAREA(I),ORATE(I),IDIR(I),
     .                                IOPRNT(I)
      ELSE
                   READ(N5,*,ERR=888) CC,KCNODE,OOPEN(I),OCLOSE(I),
     .                                OCAREA(I),ORATE(I),IDIR(I),
     .                                IOPRNT(I)
      ENDIF
CIM   IF JELEV IS 4 THEN CONVERT OOPEN AND OCLOSE FROM ELEVATIONS TO DEPTHS HERE
      IF (JELEV.EQ.4) THEN
CIM FIND CONTROL JUNCTION INVERT ELEVATION 
          IF (JCE.EQ.0) THEN
          DO JUNCIM = 1, NJ
           IF(ICNODE(I).EQ.JUN(JUNCIM))  GO TO 684
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ELSE
	    DO JUNCIM = 1,NJ
          IF(KCNODE.EQ.AJUN(JUNCIM)) GO TO 684
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ENDIF
  684 CONTINUE
	OOPEN(I) = OOPEN(I) - Z(JUNCIM)
	OCLOSE(I) = OCLOSE(I) - Z(JUNCIM)
	ENDIF
CIM ADD A CHECK FOR NEGATIVE OOPEN AND OOCLOSE
	IF ((OOPEN(I).LT.0.0) .OR. (OCLOSE(I).LT.0.0)) THEN
	WRITE(N6,5443) I
	NSTOP = NSTOP + 1
	ENDIF
      IF(OOPEN(I).EQ.OCLOSE(I)) THEN
      WRITE(N5,*) ' ERROR - OPEN AND CLOSE DEPTHS CAN NOT BE EQUAL ',
     .'FOR HEAD DEPENDENT GATES (F4 CARD)'
      NSTOP = NSTOP + 1
      END IF
CIM ECHO PRINT OF INPUT DATA FOR HEAD DEPENDENT GATED CONTROL HERE
      IF(JCE.EQ.0) THEN
                   WRITE(N6,662) I,ICNODE(I),OOPEN(I),OCLOSE(I),
     .                           OCAREA(I),ORATE(I),IDIR(I),IOPRNT(I)
      ELSE
                   WRITE(N6,663) I,KCNODE,OOPEN(I),OCLOSE(I),
     .                           OCAREA(I),ORATE(I),IDIR(I),IOPRNT(I)
      ENDIF
      DO 685 J = 1,NJ
      IF(JCE.EQ.0.AND.ICNODE(I).EQ.JUN(J))  GO TO 686
      IF(JCE.EQ.1.AND.KCNODE.EQ.AJUN(J)) GO TO 686
  685 CONTINUE
      IF(JCE.EQ.0) THEN
       WRITE(N6,5445) ICNODE(I)
      ELSE
       WRITE(N6,5446) KCNODE
      ENDIF
      NSTOP      = NSTOP+1
  686 ICNODE(I) = J
      END IF
       IF (OCAREA(I).LT.0.00001) OCAREA(I)=0.00001
       IF (AORIF(I).LT.0.00001) AORIF(I)=0.00001
CIM CONVERT RATE TO DELTA AREA PER UNIT TIME (SQ.FT./SECOND)
      IF (ORATE(I).GT.0.0) THEN
          ORATE(I)=ABS(AORIF(I)-OCAREA(I))/(ORATE(I)*60.0*60.0)
        ELSE
          ORATE(I)=999999999.99
       ENDIF
CIM   CHANGE IDIR TO MATCH DEFINITION OF INGATE
      IF (IDIR(I).EQ.0) ITEMP=0
      IF (IDIR(I).LT.0) ITEMP=2
      IF (IDIR(I).GT.0) ITEMP=1
      IDIR(I)=ITEMP
CIM END      OOOOOOOOOO
  690 CONTINUE
  695 NORIF = I-1
CIM START     OOOOOOOOO
CIM   OPEN FILE FOR TEST PRINTOUT
cim      IF (NOGATES.GT.0) OPEN(UNIT=7,FILE='TEST.OUT',STATUS='UNKNOWN')
CIM END  OOOOOOOOOOOO
      NTC   = NTC + NORIF
      NTL   = NTL + NORIF
      IF(NORIF.GT.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,5420)
      IF(METRIC.EQ.2) WRITE(N6,5421)
      DO 780 I = 1,NORIF
      N        = NTC - NORIF + I
CIM START  OOOOOOOOOO
      CIRCULAR = ((NKLASS(N).EQ.1) .OR.
     .   (NKLASS(N).EQ.2) .OR.
     .   (NKLASS(N).EQ.-1) .OR.
     .   (NKLASS(N).EQ.-2) .OR.
     .   (NKLASS(N).EQ.11) .OR.
     .   (NKLASS(N).EQ.12) .OR.
     .   (NKLASS(N).EQ.21) .OR.
     .   (NKLASS(N).EQ.22))
      IF (CIRCULAR) then
      IF(JCE.EQ.0) THEN
                   WRITE(N6,5440) (NJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N)
      ELSE
                   WRITE(N6,5441) (KJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N)
      ENDIF
      ELSE
      AORIF(I) = WIDEO(I)*DEEPO(I)
      IF(JCE.EQ.0) THEN
                   WRITE(N6,5440) (NJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N),
     A                             DEEPO(I),WIDEO(I)
      ELSE
                   WRITE(N6,5441) (KJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N),
     A                             DEEPO(I),WIDEO(I)
      ENDIF
      ENDIF
CIM END  OOOOOOOOOOOO
C=======================================================================
C     Convert to internal number system.
C=======================================================================
      LORIF(I) = N
      NCOND(N) = N + 90000
      IF(JCE.EQ.1) ACOND(N) = ONAME(I)
CIM START  OOOOOOOOOO
      IF (CIRCULAR) THEN
      DEEP(N)  = SQRT(4.0*AORIF(I)/3.14159)
      WIDE(N)  = DEEP(N)
      AFULL(N) = AORIF(I)
      RFULL(N) = DEEP(N)/4.0
      ELSE
      DEEP(N)  = DEEPO(I)
      WIDE(N)  = WIDEO(I)
      AFULL(N) = AORIF(I)
      RFULL(N) = AFULL(N)/(2.0*DEEPO(I)+2.0*WIDEO(I))
      ENDIF
CIM END      OOOOOOOOOOO
                      DDD = DELT
C BAC START !!!!!!!
C NOTE CHANGE IN DEFINITION OF NEQUAL
C      IF(NEQUAL.GT.1) DDD = FLOAT(NEQUAL)
C BAC END  !!!!!!!
      CLEN     = 2.0*DDD*SQRT(GRVT*DEEP(N))
      LEN(N)   = AMAX1(200.,CLEN)
      CMANN    = CMET(9,METRIC)
      ROUGH(N) = CMANN*RFULL(N)**.66667/(CORIF(I)*SQRT(LEN(N)*2.0*GRVT))
CIM START  <><><><><>
cim            Revise nklass to internal number here  FIRST CHECK FOR VALID
CIM            RANGE
      IF (NKLASS(N).LT.1.OR.NKLASS(N).GT.4) THEN
      IF(JCE.EQ.0) THEN
        WRITE(N6,7000) (NJUNC(N,K),K=1,2),NKLASS(N)
      ELSE
       WRITE(N6,7001) (KJUNC(N,K),K=1,2),NKLASS(N)
      ENDIF
      STOP 'ORIFICE TYPE NOT VALID'
      END IF
      NKLASS(N)= NEWKLASS(NKLASS(N))
CIM END <><><><><><>
CIM START OOOOOOOO
CIM    BOTTOM OUTLET CONVERSION
      IF((NKLASS(N).EQ.52).OR.(NKLASS(N).EQ.54)) THEN
            ZU(N) = -0.96*DEEP(N)
CIM     INVERT ELEVATIONS FOR BOTTOM OUTLET WON'T CHANGE
            IOINV(I) = 0
      ENDIF
CIM END     OOOOOOOOO
      DO 770 K = 1,2
      DO 700 J = 1,NJ
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 720
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 720
  700 CONTINUE
      IF(JCE.EQ.0) THEN
         WRITE(N6,5450) NJUNC(N,K)
      ELSE
         WRITE(N6,5451) KJUNC(N,K)
      ENDIF
      NSTOP      = NSTOP+1
  720 NJUNC(N,K) = J
      IF(JSKIP(J).EQ.1) THEN
                        JSKIP(J)   = 0
                        IF(ZCROWN(J).LT.DEEP(N)) ZCROWN(J) = DEEP(N)
                        ENDIF
C=======================================================================
C     SET ZU(N) FOR BOTTOM OUTLET
C     SET ZU(N) AND ZD(N) ELEVATIONS
C     LOWER Z(J) AT UPSTREAM END BY -0.96*DEEP(N)
C=======================================================================
      IF(K.EQ.1) THEN
                 ZU(N) = ZU(N) + Z(J)
                 ZD(N) = ZU(N) - 0.01/CMET(1,METRIC)
CIM START  OOOOOOO
CIM       BOTTOM OUTLET CONVERSION
                 IF((NKLASS(N).EQ.52).OR.(NKLASS(N).EQ.54)) THEN
CIM END          OOOOOOOO
                              IF(Y(J).GT.0.0) Y(J) = Y(J) + 0.96*DEEP(N)
                              Z(J) = ZU(N)
                         IF(JCE.EQ.0) THEN
                          WRITE(N6,8011)  JUN(J),Z(J)
                         ELSE
                          WRITE(N6,8012) AJUN(J),Z(J)
                         ENDIF
                              ENDIF
                 ENDIF
C=======================================================================
C     CHECK GROUND ELEVATION
C=======================================================================
      IF(ZU(N)+DEEP(N).GE.GRELEV(J)) THEN
                                     IF(JCE.EQ.0) THEN
                                      WRITE(N6,5455) JUN(J)
                                     ELSE
                                      WRITE(N6,5456)AJUN(J)
                                     ENDIF
                                     NSTOP = NSTOP+1
                                     ENDIF
      IF(ZD(N)+DEEP(N).GE.GRELEV(J)) THEN
                                     IF(JCE.EQ.0) THEN
                                      WRITE(N6,5455) JUN(J)
                                     ELSE
                                      WRITE(N6,5456)AJUN(J)
                                     ENDIF
                                     NSTOP = NSTOP+1
                                     ENDIF
  730 CONTINUE
C#### WCH, 8/4/95.  INCREASE NUMBER OF CONNECTING CHANNELS.
      DO 740 KK = 1,NCHN
      IF(NCHAN(J,KK)) 760,760,740
  740 CONTINUE
      IF (NCHAN(J,KK).NE.0) THEN
                     IF (JCE.EQ.0) THEN
                       WRITE(n6,8330) JUN(J)
                       ELSE
                       WRITE(n6,8331) AJUN(J)
                       ENDIF
                     NSTOP = NSTOP + 1
      ENDIF
  760 NCHAN(J,KK) = N
  770 CONTINUE
C=======================================================================
C     CHECK GRAVITY FLOW DIRECTION
C=======================================================================
      IF(ZU(N).LT.ZD(N)) THEN
                         J2    = NJUNC(N,2)
                         IF(JCE.EQ.0) THEN
                          WRITE(N6,5458)  JUN(J2)
                         ELSE
                          WRITE(N6,5459) AJUN(J2)
                         ENDIF
                         NSTOP = NSTOP+1
                         ENDIF
  780 CONTINUE
      DO 790 I = 1,NORIF
      N        = LORIF(I)
CIM START OOOOOO
      CIRCULAR = ((NKLASS(N).EQ.51).OR.(NKLASS(N).EQ.52))
      IF (CIRCULAR) THEN
      IF(JCE.EQ.0) THEN
        WRITE(N6,6010) I,NCOND(N),DEEP(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
      ELSE
        WRITE(N6,6011) I,ACOND(N),DEEP(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
      ENDIF
      ELSE
      IF(JCE.EQ.0) THEN
        WRITE(N6,6012) I,NCOND(N),DEEP(N),WIDE(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
      ELSE
        WRITE(N6,6013) I,ACOND(N),DEEP(N),WIDE(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
      ENDIF
      END IF
cim change ioninv(N) to IOINV(I)
      IF (IOINV(I).EQ.1) WRITE(N6,6014)
CIM END OOOOOOO
  790 CONTINUE
      ENDIF
C=======================================================================
C     READ WEIR DATA ON DATA GROUP G1.
C     THIS ROUTINE HAS BEEN MODIFIED TO TRANSFER WEIR DISCHARGES FROM
C     NODE TO NODE RATHER THAN FROM NODE TO CONDUIT.
C=======================================================================
      DO 820 I = 1,NEW
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'G1') GO TO 840
      N = NTC+I
C  BAC START  WWWWWWWWWWWWWWW
C  READ NEW INPUTS FOR ADDITIONAL WEIR OPTIONS
CIM
CIM     KWEIR       : Type of weir.
CIM                 = 1 Transverse horizontal weir (exponent = 3/2).
CIM                 = 2 Transverse horizontal weir with tide gate.
CIM                 = 3 Side flow horizontal weir (exponent = 5/3).
CIM                 = 4 Side flow horizontal weir with tide gate.
CIM                 = 5 V-notch or triangular (exponent = 5/2).
CIM                 = 6 V-notch or triangular with tide gate.
CIM                 = 7 Trapezoidal (compound exponent)
CIM                 = 8 Trapezoidal with tide gate.
C  SINCE THE LINE GROUP AND THE NUMBER OF INPUT FIELDS ON THE LINE
C  GROUP THAT FOLLOWS THE WEIR LINES VARIES, READS MUST BE MADE IN
C  TWO PARTS.
C  IWRPRT IS USED FOR TELLING THE PROGRAM WHICH WRITE STATEMENT TO USE
C  FOR THE INPUT ECHO OF THE WEIRS.
      ISUBEQ(I)=0
      ENDCON(I)=0.0
      IWRPRT(I)=0
      COEF2(I)=0.0
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,(NJUNC(N,K),K=1,2),KWEIR(I),
     +                             YCREST(I),YTOP(I),WLEN(I),COEF(I),
     +                             ISUBEQ(I),ENDCON(I),THETAV(I),
     +                             COEF2(I)
      ELSE
                   READ(N5,*,ERR=888) CC,(KJUNC(N,K),K=1,2),KWEIR(I),
     +                             YCREST(I),YTOP(I),WLEN(I),COEF(I),
     +                             ISUBEQ(I),ENDCON(I),THETAV(I),
     +                             COEF2(I)
      ENDIF
      IF(ENDCON(I)+THETAV(I)+COEF2(I).EQ.0.0) IWRPRT(I)=1
CIM   IF JELEV IS 4 THEN CONVERT OOPEN AND OCLOSE FROM ELEVATIONS TO DEPTHS HERE
      IF (JELEV.EQ.4) THEN
CIM FIND UPSTREAM JUNCTION INVERT ELEVATION 
          IF (JCE.EQ.0) THEN
          DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 819
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ELSE
	    DO JUNCIM = 1,NJ
          IF(KJUNC(N,1).EQ.AJUN(JUNCIM)) GO TO 819
	    ENDDO
CIM SAVE CHECK FOR NO MATCH FOR LATER
	    ENDIF
  819 CONTINUE
	YCREST(I) = YCREST(I) - Z(JUNCIM)
	YTOP(I) = YTOP(I) - Z(JUNCIM)
	ENDIF
CIM ADD A CHECK FOR NEGATIVE YTOP OR YCREST
	IF ((YCREST(I).LT.0.0) .OR. (YTOP(I).LT.0.0)) THEN
	WRITE(N6,5444) I
	NSTOP = NSTOP + 1
	ENDIF
C  BAC END  WWWWWWWWWWW
  820 CONTINUE
  840 NWEIR = I-1
      IF(NWEIR.GT.0) THEN
                     IF(METRIC.EQ.1) WRITE(N6,5480)
                     IF(METRIC.EQ.2) WRITE(N6,5481)
                     DO 1020 I = 1,NWEIR
                     N1        = NTC+I
                     LWEIR(I)  = N1
                     IF(JCE.EQ.0) THEN
                       NCOND(N1) = 90000 + N1
                     ELSE
                       ACOND(N1) = WNAME(I)
                     ENDIF
                     COEFS(I)  = 0.0
C  BAC START  WWWWWWW
      IF(JCE.EQ.0) THEN
               IF(IWRPRT(I).EQ.0) THEN
                                  WRITE(N6,5487)
     +                            (NJUNC(N1,K),K=1,2),
     +                            NCOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I)
               ELSE
                                  WRITE(N6,5485)
     +                            (NJUNC(N1,K),K=1,2),
     +                            NCOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I),ISUBEQ(I),
     +                            ENDCON(I),THETAV(I),COEF2(I)
               ENDIF
      ELSE
               IF(IWRPRT(I).EQ.0) THEN
                                  WRITE(N6,5488)
     +                            (KJUNC(N1,K),K=1,2),
     +                            ACOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I)
               ELSE
                                  WRITE(N6,5486)
     +                            (KJUNC(N1,K),K=1,2),
     +                            ACOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I),ISUBEQ(I),
     +                            ENDCON(I),THETAV(I),COEF2(I)
               ENDIF
      ENDIF
C  BAC END  WWWWWW
                     DO 875 K = 1,2
                     IF(JCE.EQ.0.AND.NJUNC(N1,K).EQ.0)   GO TO 875
                     IF(JCE.EQ.1.AND.KJUNC(N1,K).EQ.' ') GO TO 875
                     DO 870 J = 1,NJ
                     IF(JCE.EQ.0.AND.NJUNC(N1,K).EQ.JUN(J))  GO TO 871
                     IF(JCE.EQ.1.AND.KJUNC(N1,K).EQ.AJUN(J)) GO TO 871
  870                CONTINUE
                     IF(JCE.EQ.0) THEN
                       WRITE(N6,5490) NJUNC(N1,K)
                     ELSE
                       WRITE(N6,5491) KJUNC(N1,K)
                     ENDIF
                     NSTOP       = NSTOP+1
  871                NJUNC(N1,K) = J
cim   check for downstream node being higher than weir elevation
                     IF (K.EQ.2) THEN
                     WELEV = YCREST(I) + Z(NJUNC(N1,1))
                     IF (Z(NJUNC(N1,2)).GE.WELEV) THEN
                     IF (JCE.EQ.0) THEN
                         WRITE(N6,8332) I,JUN(NJUNC(N1,1)),
     a                   JUN(NJUNC(N1,2)),Z(NJUNC(N1,2)),WELEV
                     ELSE
                         WRITE(N6,8333) I,AJUN(NJUNC(N1,1)),
     a                   AJUN(NJUNC(N1,2)),Z(NJUNC(N1,2)),WELEV
                     ENDIF
 8332 FORMAT(' ERROR = Weir Number ',I10,' FROM NODE ',I10,
     1' TO NODE ',I10,/,
     2' Invert elevation of downstream node = ',F10.3,
     3' is greater than elevation of weir crest = ',F10.3,'.',/,
     4' This can cause erroneous results.  Set downstream node invert',
     5' elevation to be less than weir crest and adjust ZP on',
     6' connecting pipes.')
 8333 FORMAT(' ERROR = Weir Number ',I10,' FROM NODE ',A10,
     1' TO NODE ',A10,/,
     2' Invert elevation of downstream node = ',F10.3,
     3' is greater than elevation of weir crest = ',F10.3,'.',/,
     4' This can cause erroneous results.  Set downstream node invert',
     5' elevation to be less than weir crest and adjust ZP on',
     6' connecting pipes.')
                     NSTOP = NSTOP + 1
                     ENDIF
                     ENDIF
                     DO 873 KK   = 1,NCHN
                     IF(NCHAN(J,KK)) 874,874,873
  873                CONTINUE
cim write a warning
                     WRITE(*,*) J,' ',AJUN(J),KK
                     IF (NCHAN(J,KK).GE.0) THEN
                     IF (JCE.EQ.0) THEN
                       WRITE(n6,8330) JUN(J)
                       ELSE
                       WRITE(n6,8331) AJUN(J)
                       ENDIF
                     NSTOP = NSTOP + 1
                     ENDIF
  874                NCHAN(J,KK) = N1
  875                CONTINUE
 1020                CONTINUE
                     NTL = NTL + NWEIR
                     ENDIF
C=======================================================================
C     READ PUMP DATA ON DATA GROUP H1
C
C     IPTYP = 1   Off-line pump with wet well (program will
C                 set pump junction invert to -100).
C                 Off-line pump operates on wet well volume.
C                 Note:  Only one pipe can be connected to a
C                        type 1 pump node.
C
C     IPTYP = 2   In-line lift pump.  In-line pump operates on
C                 head at pumped junction.
C
C     IPTYP = 3   Three-point head-discharge pump curve.  Pump
C                 rate varies linearly based on head difference
C                 between discharge and pumped junctions.
C
C     IPTYP = 4   Variable-speed in line pump.  Pump rate varies
C                 linearly based on depth in pumped junction.
C
C     IPTYP = 5   Constant speed lift station type pump.  Pump rate
C                 is entered for individual pumps.  Pumps stay on 
C                 until depth drops to POFF level.  When pumps come
C                 on the rate increases linearly from zero to PRATE 
C                 over PONDELAY seconds.  Implemented by C. Moore of
C                 CDM  6/99.
C=======================================================================
CIM CHANGE TO PRINT IF NUMBER OF PUMPS EXCEED NEP 6/97
      DO 1060 I = 1,NEP+1
C
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'H1') GO TO 1080
C print error message
      IF(I.GT.NEP) THEN
      WRITE(N6,8010) NEP
      STOP 'TOO MANY PUMPS, SEE ERROR MESSAGE IN OUTPUT FILE'
      ENDIF
CIM PP 5/1/97 various changes for reading optional NPRATE parameter
      READ(N5,*,ERR=888) CC,IPTYP(I)
      NPRATE(I) = 3
C     INITIALIZE PUMP STATUS TO OFF
C     USE FOR TYPE 3 and 5
      DO K=1,MAXPRA
	IPOPR(K,I) = -1
	TIMEON(K,I) = 0.0
	ENDDO
      BACKSPACE N5
      N = NTL + I
      IF (IPRATE.EQ.0) THEN
       SELECT CASE (IPTYP(I))
       CASE (1)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I)
        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I)
        ENDIF
       CASE (2)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     1                (VRATE(I,K),K=1,2)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 1040
	    ENDDO
 1040      DO K = 1,2 
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     1                (VRATE(I,K),K=1,2)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 1041
	    ENDDO
 1041     DO K=1,2
            VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	    ENDDO
	  ENDIF 
        ENDIF
cim  ADD A CHECK FOR NEGATIVE VRATES
      IF((VRATE(I,1).LT.0.0).OR.(VRATE(I,2).LT.0.0)) THEN
	WRITE(N6,5447) I
	NSTOP = NSTOP + 1
	ENDIF
       CASE (3)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I),PON(I),POFF(I)
CIM  CONVERT PON AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 1042
	    ENDDO
 1042      PON(I) = PON(I) - Z(JUNCIM)
           POFF(I) = POFF(I) - Z(JUNCIM)
	  ENDIF 

        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I),PON(I),POFF(I)
CIM  CONVERT PON AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 1043
	    ENDDO
 1043      PON(I) = PON(I) - Z(JUNCIM)
           POFF(I) = POFF(I) - Z(JUNCIM)
	  ENDIF 
        ENDIF
CIM ADD A CHECK FOR NEGATIVE PON OR POFF
        IF((PON(I).LT.0.0).OR.(POFF(I).LT.0.0)) THEN
	   WRITE(N6,5448) I
			NSTOP = NSTOP + 1
	  ENDIF
       CASE (4)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),PON(I),POFF(I)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 1044
	    ENDDO
 1044      DO K = 1,3
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),PON(I),POFF(I)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 1045
	    ENDDO
 1045     DO K=1,3
            VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	    ENDDO
	  ENDIF 
        ENDIF
cim  ADD A CHECK FOR NEGATIVE VRATES
      DO K=1,3
      IF(VRATE(I,K).LT.0.0) THEN
	WRITE(N6,5447) I
	NSTOP = NSTOP + 1
	ENDIF
	ENDDO
c     New type 5 pump
       CASE (5)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),POFF(I),PONDELAY(I)
CIM  CONVERT VRATE AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 1046
	    ENDDO
 1046      POFF(I) = POFF(I) - Z(JUNCIM)
           DO K = 1,3
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 

        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),POFF(I),PONDELAY(I)
CIM  CONVERT VRATE AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 1047
	    ENDDO
 1047      POFF(I) = POFF(I) - Z(JUNCIM)
           DO K = 1,3
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ENDIF
CIM ADD A CHECK FOR NEGATIVE VRATES OR POFF
        IF(POFF(I).LT.0.0) THEN
	   WRITE(N6,5448) I
			NSTOP = NSTOP + 1
	  ENDIF
	DO K=1,3
	   IF (VRATE(I,K).LT.0.0) THEN
	   WRITE(N6,5447) I
	ENDIF
	ENDDO
	ENDSELECT
        ELSE
C this repeats above code when IPRATE is used
        SELECT CASE (IPTYP(I))
        CASE (1)
         IF(JCE.EQ.0) THEN
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)),
     4                VWELL(I)
         ELSE
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)),
     3                VWELL(I)
         ENDIF
        CASE (2)
        IF(JCE.EQ.0)  THEN
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)-1)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 8040
	    ENDDO
 8040      DO K=1,NPRATE(I)-1
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ELSE
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)-1)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 8041
	    ENDDO
 8041     DO K=1,NPRATE(I)-1
            VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	    ENDDO
	  ENDIF 
        ENDIF
cim  ADD A CHECK FOR NEGATIVE VRATES
      DO K = 1,NPRATE(I) - 1
      IF (VRATE(I,K).LT.0.0) THEN
	WRITE(N6,5447) I
	NSTOP = NSTOP + 1
	ENDIF
	ENDDO
        CASE(3)
        IF(JCE.EQ.0)  THEN
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)),
     4                VWELL(I),PON(I),POFF(I)
CIM  CONVERT PON AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 8042
	    ENDDO
 8042      PON(I) = PON(I) - Z(JUNCIM)
           POFF(I) = POFF(I) - Z(JUNCIM)
	  ENDIF 
        ELSE
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)),
     4                VWELL(I),PON(I),POFF(I)
CIM  CONVERT PON AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 8043
	    ENDDO
 8043      PON(I) = PON(I) - Z(JUNCIM)
           POFF(I) = POFF(I) - Z(JUNCIM)
	  ENDIF 
        ENDIF
CIM ADD A CHECK FOR NEGATIVE PON OR POFF
        IF((PON(I).LT.0.0).OR.(POFF(I).LT.0.0)) THEN
	   WRITE(N6,5448) I
			NSTOP = NSTOP + 1
	  ENDIF
        CASE(4)
        IF(JCE.EQ.0) THEN
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     3                (VRATE(I,K),K=1,NPRATE(I)),
	4                PON(I),POFF(I)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 8044
	    ENDDO
 8044      DO K = 1,NPRATE(I)
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ELSE
                      READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,NPRATE(I)),
     2                (VRATE(I,K),K=1,NPRATE(I)),PON(I),POFF(I)
CIM CONVERT VRATE FROM ELEVATION TO DEPTH FOR JELEV = 4
        IF (JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 8045
	    ENDDO
 8045     DO K=1,NPRATE(I)
            VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	    ENDDO
	  ENDIF 
        ENDIF
cim  ADD A CHECK FOR NEGATIVE VRATES
      DO K=1,NPRATE(I)
      IF(VRATE(I,K).LT.0.0) THEN
	WRITE(N6,5447) I
	NSTOP = NSTOP + 1
	ENDIF
	ENDDO
c     New type 5 pump
       CASE (5)
        IF(JCE.EQ.0) THEN
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),POFF(I),PONDELAY(I)
CIM  CONVERT VRATE AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(NJUNC(N,1).EQ.JUN(JUNCIM))  GO TO 8046
	    ENDDO
 8046      POFF(I) = POFF(I) - Z(JUNCIM)
           DO K = 1,NPRATE(I)
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 

        ELSE
                  READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),NPRATE(I),
     2                (PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),POFF(I),PONDELAY(I)
CIM  CONVERT VRATE AND POFF TO DEPTHS IF JELEV = 4
                  IF(JELEV.EQ.4) THEN
	    DO JUNCIM = 1, NJ
           IF(KJUNC(N,1).EQ.AJUN(JUNCIM))  GO TO 8047
	    ENDDO
 8047      POFF(I) = POFF(I) - Z(JUNCIM)
           DO K = 1,NPRATE(I)
           VRATE(I,K) = VRATE(I,K) - Z(JUNCIM)
	     ENDDO
	  ENDIF 
        ENDIF
CIM ADD A CHECK FOR NEGATIVE VRATES OR POFF
        IF(POFF(I).LT.0.0) THEN
	   WRITE(N6,5448) I
			NSTOP = NSTOP + 1
	  ENDIF
	DO K=1,NPRATE(I)
	   IF (VRATE(I,K).LT.0.0) THEN
	   WRITE(N6,5447) I
	ENDIF
	ENDDO
        END SELECT
        ENDIF
CIM      ISWTCH(IPTYP(I)) = IPTYP(I)
 1060 CONTINUE
 1080 NPUMP = I - 1
C=======================================================================
C     PRINT PUMP NODES
C=======================================================================
      IF(NPUMP.GT.0) THEN
cim pp 5/1/97  change format of pump printout
cim Simply print one pump at a time in order that they were input.
cim Delete all lines associated with original input
      WRITE(N6,5560)
      DO I = 1,NPUMP
      N         = NTL+I
      IF(JCE.EQ.0) THEN
        WRITE(N6,5562) I,(NJUNC(N,K),K=1,2)
        ELSE
        WRITE(N6,5564) I,(KJUNC(N,K),K=1,2)
      ENDIF
      SELECT CASE (IPTYP(I))
      CASE (1)
C=======================================================================
C     PRINT OFF-LINE PUMP DATA IPTYP = 1
C=======================================================================
       IF(METRIC.EQ.1) THEN
          WRITE(N6,5566) VWELL(I)
          WRITE(N6,5568)
          ELSE
          WRITE(N6,5570) VWELL(I)
          WRITE(N6,5572)
        ENDIF
        WRITE(N6,5574) 0.0,VRATE(I,1),PRATE(I,1)
        WRITE(N6,5574) (VRATE(I,K-1),VRATE(I,K),PRATE(I,K),
     1                   K=2,NPRATE(I))
CIM CHECK THAT VRATES ARE IN INCREASING ORDER.
        DO K=2,NPRATE(I)
           IF (VRATE(I,K).LT.VRATE(I,K-1)) THEN
             WRITE(N6,9010)
             NSTOP = NSTOP+1
           ENDIF
        ENDDO
      CASE (2)
C=======================================================================
C     PRINT IN-LINE PUMP DATA IPTYP = 2
C=======================================================================
        WRITE(N6,5576)
        IF(METRIC.EQ.1) THEN
          WRITE(N6,5578)
          ELSE
          WRITE(N6,5580)
        ENDIF
        WRITE(N6,5574) 0.0,VRATE(I,1),PRATE(I,1)
        WRITE(N6,5574) (VRATE(I,K-1),VRATE(I,K),PRATE(I,K),
     1                  K=2,NPRATE(I)-1)
        WRITE(N6,5582) VRATE(I,NPRATE(I)-1),PRATE(I,NPRATE(I))
CIM CHECK THAT VRATES ARE IN INCREASING ORDER.
        DO K=2,NPRATE(I)-1
           IF (VRATE(I,K).LT.VRATE(I,K-1)) THEN
             WRITE(N6,9010)
             NSTOP = NSTOP+1
           ENDIF
        ENDDO
      CASE (3)
C=======================================================================
C     PRINT PUMP CURVE DATA IPTYP = 3
C=======================================================================
        IF(METRIC.EQ.1) THEN
          WRITE(N6,5584) PON(I),POFF(I)
          WRITE(N6,5586)
          ELSE
          WRITE(N6,5588) PON(I),POFF(I)
          WRITE(N6,5590)
        ENDIF
        WRITE(N6,5592) (VRATE(I,K),PRATE(I,K),K=1,NPRATE(I))
        WRITE(N6,5600)
C#######################################################################
C CHECK FOR FLAT PUMP CURVE.  CANNOT HAVE ADJACENT VRATES EQUAL
C THIS AVOIDS ZERO DIVIDE IN SUBROUTINE BOUND WCH, 8/28/92
C#######################################################################
CIM ALSO CHECK THAT VRATES ARE IN DECREASING ORDER
        DO K=1,NPRATE(I)-1
           IF(VRATE(I,K).EQ.VRATE(I,K+1)) THEN
             WRITE(N6,5575)
             NSTOP = NSTOP+1
           ENDIF
           IF(VRATE(I,K).LT.VRATE(I,K+1)) THEN
             WRITE(N6,9020)
             NSTOP = NSTOP+1
           ENDIF
       ENDDO
      CASE (4)
C=======================================================================
C     PRINT VARIABLE SPEED PUMP CURVE DATA IPTYP = 4
C=======================================================================
        WRITE(N6,5594)
        IF(METRIC.EQ.1) THEN
          WRITE(N6,5596)
          ELSE
          WRITE(N6,5598)
        ENDIF
        WRITE(N6,5592) (VRATE(I,K),PRATE(I,K),K=1,NPRATE(I))
        WRITE(N6,5600)
        IF(METRIC.EQ.1) THEN
          WRITE(N6,5602) PRATE(I,1),VRATE(I,1)
          WRITE(N6,5604) PRATE(I,NPRATE(I)),VRATE(I,NPRATE(I))
          ELSE
          WRITE(N6,5606) VRATE(I,1),PRATE(I,1)
          WRITE(N6,5608) PRATE(I,NPRATE(I)),VRATE(I,NPRATE(I))
        ENDIF
C#######################################################################
C CHECK FOR FLAT PUMP CURVE.  CANNOT HAVE ADJACENT VRATES EQUAL
C THIS AVOIDS ZERO DIVIDE IN SUBROUTINE BOUND WCH, 8/28/92
C#######################################################################
C CHECK THAT VRATES INCREASE
        DO K=1,NPRATE(I)-1
           IF(VRATE(I,K).EQ.VRATE(I,K+1)) THEN
             WRITE(N6,5575)
             NSTOP = NSTOP+1
           ENDIF
           IF(VRATE(I,K).GT.VRATE(I,K+1)) THEN
           WRITE(N6,9010)
           NSTOP = NSTOP+1
           ENDIF
        ENDDO
c initialize to off
	IPOPR(1,I)=-1
	IF ((PON(I).EQ.0.0).AND.(POFF(I).EQ.0.0)) THEN
	PON(I) = -0.1
	POFF(I) = -0.2
	ELSE
	WRITE(N6,8400) PON(I),POFF(I)
	ENDIF
	CASE (5)
C=======================================================================
C     PRINT CONSTANT SPEED LIFT STATION TYPE PUMP DATA IPTYP = 5
C=======================================================================
	WRITE(N6,8100)
	DO K=1,NPRATE(I)
	IF (METRIC.EQ.1) THEN
      WRITE(N6,8102) I,PRATE(I,K),'CFS',VRATE(I,K),'FT'
	ELSE 
	WRITE(N6,8102) I,PRATE(I,K),'M3S',VRATE(I,K),'M '
	END IF
	enddo
	IF (METRIC.EQ.1) THEN	
	WRITE(N6,8104) POFF(I),'FT',PONDELAY(I)
	ELSE
	WRITE(N6,8104) POFF(I),'M ',PONDELAY(I)
	ENDIF
      CASE DEFAULT
        WRITE(N6,5576)
        NSTOP = NSTOP+1
      END SELECT
      ENDDO
C=======================================================================
C                   CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                    DO 1240 I = 1,NPUMP
                    N         = NTL+I
                    LPUMP(I)  = N
                    NCOND(N)  = N + 90000
                    IF(JCE.EQ.1) ACOND(N)  = PNAME(I)
                    DO 1220 K = 1,2
                    DO 1180 J = 1,NJ
                    IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 1190
                    IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 1190
 1180               CONTINUE
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5610) NJUNC(N,K)
                    ELSE
                      WRITE(N6,5611) KJUNC(N,K)
                    ENDIF
                    NSTOP      = NSTOP+1
 1190               NJUNC(N,K) = J
                    DO 1195 KK = 1,NCHN
                    IF(NCHAN(J,KK)) 1200,1200,1195
 1195               CONTINUE
      IF (NCHAN(J,KK).NE.0) THEN
                     IF (JCE.EQ.0) THEN
                       WRITE(n6,8330) JUN(J)
                       ELSE
                       WRITE(n6,8331) AJUN(J)
                       ENDIF
                     NSTOP = NSTOP + 1
      ENDIF
 1200               NCHAN(J,KK) = N
                    IF(IPTYP(I).GE.2) GO TO 1220
                    IF(KK.LE.2)       GO TO 1220
                    IF(K.EQ.2)        GO TO 1220
                    IF(JCE.EQ.0) THEN
                     WRITE(N6,5615)  JUN(J)
                    ELSE
                     WRITE(N6,5614) AJUN(J)
                    ENDIF
                    NSTOP = NSTOP + 1
 1220               CONTINUE
C=======================================================================
C                   SET INFLOW INDEX FOR PUMP NODE
C=======================================================================
                    JP  = NJUNC(N,1)
                    IF(IPTYP(I).GE.2) GO TO 1235
                    JSKIP(JP) = 1
                    Z(JP)     = -100.0
 1235               CONTINUE
                    JPFUL(I)  = 1
 1240               CONTINUE
                    NTL       = NTL + NPUMP
                    ENDIF
CIM OK HAVE READ WEIRS ORIFICES AND PUMPS, NOW CHECK IF ALL NODES
CIM ARE CONNECTED TO AT LEAST ONE PIPE AND QUIT IF ANY AREN'T
      INCON = 0
      DO J=1,NJ
      IF (NCHAN(J,1).EQ.0) THEN
        INCON = INCON + 1
        JSKIP(J) = 1
        IF (JCE.EQ.0) THEN
          WRITE(N6,5350) JUN(J)
        ELSE
          WRITE(N6,5351) AJUN(J)
        ENDIF
      ENDIF
      ENDDO
      IF (INCON.GT.0) THEN
      WRITE(N6,5352) INCON
      WRITE(*,5352) INCON
      STOP
      ENDIF
CIM END
C=======================================================================
C     READ DATA FOR OUTFALLS WITHOUT TIDE GATES ON DATA GROUP I1
C=======================================================================
      NFREE     = 0
cimtide  JTIDE is not initialized.  Do so here just to be certain.
cimtide
      do i=1,nee
      jtides(nee) = 0
      enddo
cimtide
cim print error message
      DO 1280 I = 1,NTG+1
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'I1') GO TO 1300
C print error message
      IF(I.GT.NTG) THEN
      WRITE(N6,8015) NTG
      STOP 'TOO MANY FREE OUTFALLS SEE ERROR MESSAGE IN OUTPUT FILE'
      ENDIF
      IF(JCE.EQ.0) THEN
           READ(N5,*,ERR=888) CC,JFREE(I),NBCF(I)
      ELSE
           READ(N5,*,ERR=888) CC,KFREE(I),NBCF(I)
      ENDIF
      NFREE = NFREE + 1
      IF(NPUMP.GT.0) THEN
               DO 1340 K = 1,NPUMP
               N         = LPUMP(K)
               JDN       = NJUNC(N,2)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,2)
               IF(JCE.EQ.0.AND.JUN(JDN).EQ.JFREE(I))  JSKIP(JDN) = 2
               IF(JCE.EQ.1.AND.AJUN(JDN).EQ.KFREE(I)) JSKIP(JDN) = 2
C              IF(JSKIP(JDN).EQ.2)   ZCROWN(JDN) = 2.0 + Z(JDN)
 1340          CONTINUE
               ENDIF
C=======================================================================
C     CHECK FOR WEIR OUTFALLS WITHOUT TIDE GATES
C=======================================================================
      IF(NWEIR.GT.0) THEN
               DO 1350 K = 1,NWEIR
               N         = LWEIR(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
C#### RED, 3/12/95.  Compare JUN(J) with JFREE, not J with JFREE.
C###               IF(JCE.EQ.0.AND.J.EQ.JFREE(I))   THEN
               IF(JCE.EQ.0.AND.JUN(J).EQ.JFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1350
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1350
                                                ENDIF
 1350          CONTINUE
               ENDIF
C=======================================================================
C     CHECK FOR PUMP OUTFALLS.
C=======================================================================
      IF(NPUMP.GT.0) THEN
               DO 1375 K = 1,NPUMP
               N         = LPUMP(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
C#### RED, 3/12/95.  Compare JUN(J) with JFREE, not J with JFREE.
C###               IF(JCE.EQ.0.AND.J.EQ.JFREE(I))   THEN
               IF(JCE.EQ.0.AND.JUN(J).EQ.JFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1375
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1375
                                                ENDIF
 1375          CONTINUE
               ENDIF
 1280 CONTINUE
 1300 CONTINUE
C=======================================================================
C     PRINT OUTFLOW NODES
C=======================================================================
      IF(NFREE.GT.0) THEN
                     WRITE(N6,5616)
                     IF(JCE.EQ.0) THEN
                        WRITE(N6,5620) (JFREE(I),NBCF(I),I=1,NFREE)
                     ELSE
                        WRITE(N6,5621) (KFREE(I),NBCF(I),I=1,NFREE)
                     ENDIF
C=======================================================================
C                    CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                     DO 1390 I = 1,NFREE
                     DO 1360 J = 1,NJ
                     IF(JCE.EQ.0.AND.JFREE(I).EQ.JUN(J))  GO TO 1380
                     IF(JCE.EQ.1.AND.KFREE(I).EQ.AJUN(J)) GO TO 1380
 1360                CONTINUE
                     IF(JCE.EQ.0) THEN
                       WRITE(N6,5630) JFREE(I)
                     ELSE
                       WRITE(N6,5631) KFREE(I)
                     ENDIF
                     NSTOP      = NSTOP + 1
 1380                JFREE(I)   = J
C=======================================================================
C                   CHECK FREE BC'S FOR MULTIPLE INPUT CONDUITS
C=======================================================================
                     IF(NCHAN(J,2).GT.0) THEN
                              IF(JCE.EQ.0) THEN
                               WRITE(N6,1853)  JUN(J)
                              ELSE
                               WRITE(N6,1854) AJUN(J)
                              ENDIF
                              NSTOP     = NSTOP + 1
                              ENDIF
                     JTIDES(J)  = NBCF(I)
                     N          = NTL + I
                     NJUNC(N,1) = J
                     NJUNC(N,2) = 0
                     IF(JCE.EQ.1) KJUNC(N,1) = AJUN(J)
                     IF(JCE.EQ.1) KJUNC(N,2) = 'BOUNDARY  '
                     NCHAN(J,2) = N
                     NCOND(N)   = N + 90000
                     IF(JCE.EQ.1) ACOND(N)   = OUTF(I)
                     IF(JSKIP(J).EQ.0) JSKIP(J)   = 1
 1390                CONTINUE
                     NTL        = NTL + NFREE
                     ENDIF
C=======================================================================
C     READ DATA FOR OUTFALLS WITH TIDE GATES ON DATA GROUP I2
C=======================================================================
      NGATE     = 0
cim print error message
      DO 1420 I = 1,NTG+1
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'I2') GO TO 1440
C print error message
      IF(I.GT.NTG) THEN
      WRITE(N6,8020) NTG
      STOP 'TOO MANY OUTFALLS WITH TIDE GATES - SEE ERROR MESSAGE IN OUT
     +PUT FILE'
      ENDIF
      IF(JCE.EQ.0) THEN
       READ(N5,*,ERR=888) CC,JGATE(I),NBCG(I)
      ELSE
       READ(N5,*,ERR=888) CC,KGATE(I),NBCG(I)
      ENDIF
      NGATE = NGATE + 1
C=======================================================================
C     CHECK FOR WEIR OUTFALLS WITH TIDE GATES
C=======================================================================
      IF(NWEIR.GT.0) THEN
               DO 1450 K = 1,NWEIR
               N         = LWEIR(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
C#### WCH (RED), 3/16/95. Compare JUN(J) with JGATE, not J with JGATE.
C####               IF(JCE.EQ.0.AND.J.EQ.JGATE(I))   THEN
               IF(JCE.EQ.0.AND.JUN(J).EQ.JGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1450
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1450
                                                ENDIF
 1450          CONTINUE
               ENDIF
C=======================================================================
C     CHECK FOR PUMP OUTFALLS WITH TIDE GATES
C=======================================================================
      IF(NPUMP.GT.0) THEN
               DO 1475 K = 1,NPUMP
               N         = LPUMP(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
C#### WCH (RED), 3/16/95. Compare JUN(J) with JGATE, not J with JGATE.
C####               IF(JCE.EQ.0.AND.J.EQ.JGATE(I))   THEN
               IF(JCE.EQ.0.AND.JUN(J).EQ.JGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1475
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1475
                                                ENDIF
 1475          CONTINUE
               ENDIF
 1420 CONTINUE
 1440 CONTINUE
C=======================================================================
C     PRINT TIDE GATE NODES
C=======================================================================
      IF(NGATE.GT.0) THEN
                     WRITE(N6,5656)
                     IF(JCE.EQ.0) THEN
                       WRITE(N6,5660) (JGATE(I),NBCG(I),I=1,NGATE)
                     ELSE
                       WRITE(N6,5661) (KGATE(I),NBCG(I),I=1,NGATE)
                     ENDIF
C=======================================================================
C                    CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                     DO 1510 I = 1,NGATE
                     DO 1480 J = 1,NJ
                     IF(JCE.EQ.0.AND.JGATE(I).EQ.JUN(J))  GO TO 1500
                     IF(JCE.EQ.1.AND.KGATE(I).EQ.AJUN(J)) GO TO 1500
 1480                CONTINUE
                     IF(JCE.EQ.0) THEN
                      WRITE(N6,5662) JGATE(I)
                     ELSE
                      WRITE(N6,5663) KGATE(I)
                     ENDIF
                     NSTOP      = NSTOP + 1
 1500                JGATE(I)   = J
C=======================================================================
C                   CHECK TIDAL BC'S FOR MULTIPLE INPUT CONDUITS
C=======================================================================
                     IF(NCHAN(J,2).GT.0) THEN
                              IF(JCE.EQ.0) THEN
                               WRITE(N6,1853)  JUN(J)
                              ELSE
                               WRITE(N6,1854) AJUN(J)
                              ENDIF
                              NSTOP     = NSTOP + 1
                              ENDIF
                     JTIDES(J)  = NBCG(I)
                     N          = NTL + I
                     NJUNC(N,1) = J
                     NJUNC(N,2) = 0
                     IF(JCE.EQ.1) KJUNC(N,1) = AJUN(J)
                     IF(JCE.EQ.1) KJUNC(N,2) = 'BOUNDARY  '
                     NCHAN(J,2) = N
                     NCOND(N)   = N + 90000
                     IF(JCE.EQ.1) ACOND(N)   = OUTG(I)
                     JSKIP(J)   = 1
 1510                CONTINUE
                     NTL        = NTL + NGATE
                     ENDIF
C=======================================================================
C     PRINT WEIR OUTFALL NODES
C=======================================================================
      IF(NWEIR.GT.0) THEN
                     ILOOP      = 0
                     DO 1470 K  = 1,NWEIR
                     N          = LWEIR(K)
                     J          = NJUNC(N,1)
                     JJ         = NJUNC(N,2)
                     IF(JJ.LE.0) THEN
                          ILOOP = ILOOP + 1
                          IF(ILOOP.EQ.1) WRITE(N6,6660)
                          IF(JCE.EQ.0) THEN
                            WRITE(N6,6665)  JUN(J),JTIDES(J)
                          ELSE
                            WRITE(N6,6670) AJUN(J),JTIDES(J)
                          ENDIF
                          ENDIF
 1470                CONTINUE
                     ENDIF
C=======================================================================
C     PRINT PUMP OUTFALL NODES
C=======================================================================
      IF(NPUMP.GT.0) THEN
                     ILOOP      = 0
                     DO 1570 K  = 1,NPUMP
                     N          = LPUMP(K)
                     J          = NJUNC(N,1)
                     JJ         = NJUNC(N,2)
                     IF(JJ.LE.0) THEN
                        ILOOP = ILOOP + 1
                        IF(ILOOP.EQ.1) WRITE(N6,6680)
                        IF(JCE.EQ.0) THEn
                          WRITE(N6,6665)  JUN(J),JTIDES(J)
                        ELSE
                          WRITE(N6,6670) AJUN(J),JTIDES(J)
                        ENDIF
                        ENDIF
 1570                CONTINUE
                     ENDIF
C=======================================================================
C     INTERNAL CONNECTIVITY INFORMATION
C=======================================================================
      WRITE(N6,2999)
      WRITE(N6,5060) ALPHA1,ALPHA2
      WRITE(N6,5665)
      IF(JCE.EQ.0) THEN
        WRITE(N6,5670)
      ELSE
        WRITE(N6,5671)
      ENDIF
      N1        = NC+1
      DO 1525 N = N1,NTL
      J1        = NJUNC(N,1)
      J2        = NJUNC(N,2)
      IF(J2.GT.0.AND.JCE.EQ.0) J2 = JUN(J2)
      IF(JCE.EQ.0) THEN
        WRITE(N6,5675) NCOND(N),JUN(J1),J2
      ELSE
        WRITE(N6,5685) ACOND(N),KJUNC(N,1),KJUNC(N,2)
      ENDIF
 1525 CONTINUE
      IF(NJ.GT.NEE) THEN
                    WRITE(N6,5676) NEE
                    NSTOP = NSTOP+1
                    ENDIF
      IF(NTL.GT.NEE) THEN
                    WRITE(N6,5677) NEE
                    NSTOP = NSTOP+1
                    ENDIF
      RETURN
 888  CALL IERROR
C=======================================================================
C#### WCH, 12/8/94.  NEW 890 - 999.
  890 FORMAT(/' DETAILED DATA FOR VARIABLE AREA/TABULAR INPUT JUNCTION',
     1 1X,I10,/,' POINT    DEPTH       AREA     VOLUME')
  891 FORMAT(/' DETAILED DATA FOR VARIABLE AREA/TABULAR INPUT JUNCTION',
     1 1X,A10,/,' POINT    DEPTH       AREA     VOLUME')
  892 FORMAT(' NUMBER      FT         AC    1000-FT3',/,
     1        ' -------------------------------------')
  893 FORMAT(' NUMBER       M         HA     1000-M3',/,
     1        ' -------------------------------------')
  894 FORMAT(I6,F10.3,2E11.4)
  895 FORMAT(/' INPUT DATA FOR VARIABLE AREA/POWER FUNCTION JUNCTION',
     1 1X,I10,/,' COEFFICIENT =',F10.3,/,' EXPONENT    =',F10.3)
  896 FORMAT(/' INPUT DATA FOR VARIABLE AREA/POWER FUNCTION JUNCTION',
     1 1X,A10,/, ' COEFFICIENT =',F10.3,/,' EXPONENT    =',F10.3)
  990 FORMAT(/,' !! WARNING FOR VARIABLE AREA STORAGE JUNCTION NO. '
     1,I10)
  991 FORMAT(/,' !! WARNING FOR VARIABLE AREA STORAGE JUNCTION NO. '
     1,A10)
  992 FORMAT(' LOWEST DEPTH = ',F9.4,'. MUST BE = 0.  VALUE SET TO 0.')
  993 FORMAT( ' LOWEST AREA = ',F9.4,'. MUST BE > 0.  VALUE SET TO AMEN
     1(LINE B2).')
  998 FORMAT(/,' WARNING!  FOR JUNCTION ',I10,' AREA DECREASES BETWEEN',
     1' STAGES',F10.3,' AND ',F10.3)
  999 FORMAT(/,' WARNING!  FOR JUNCTION ',A10,' AREA DECREASES BETWEEN',
     1' STAGES',F10.3,' AND ',F10.3)
 1853 FORMAT(/,' ===> ERROR !!  OUTFALL JUNCTION ',I10,' HAS TWO OR',/,
     +         '                MORE CONNECTING CONDUITS.')
 1854 FORMAT(/,' ===> ERROR !!  OUTFALL JUNCTION ',A10,' HAS TWO OR',/,
     +         '                MORE CONNECTING CONDUITS.')
2999  FORMAT(/,
     1       '1',40(2H--)/' ','ENVIRONMENTAL PROTECTION AGENCY',13X,40H*
     2***   EXTENDED TRANSPORT PROGRAM   ****,8X,'WATER RESOURCES DIVISI
     3ON',/,' ','WASHINGTON, D.C.            ',16X,4H****,32X,4H****,8X,
     4'CAMP DRESSER & MCKEE INC.',/,' ','                ',28X,4H****,
     56X,'   ANALYSIS MODULE  ',6X,4H****,8X,'ANNANDALE, VIRGINIA')
 5060 FORMAT(/,5X,A80,/,5X,A80,/)
 5397 FORMAT(//,
     +' ******************************************************',/,
     +' *          DETAILED STORAGE JUNCTION DATA            *',/,
     +' ******************************************************',/)
 5398 FORMAT(//,
     +' ******************************************************',/,
     +' *          STORAGE JUNCTION DATA SUMMARY             *',/,
     +' ******************************************************',/)
 5399 FORMAT(7X,I10,A10,2F18.2,F11.3)
 5499 FORMAT(7X,A10,A10,2F18.2,F11.3)
CIM START  OOOOOOO
 5420 FORMAT(//,
     +' *********************************************',/,
     +' *              ORIFICE DATA                 *',/,
     +' *********************************************',//,
     *'       FROM         TO                AREA      DISCHARGE   HEIGH
     +T ABOVE         RECTANGULAR ORIFICE',/,
     *'   JUNCTION   JUNCTION      TYPE     (FT2)    COEFFICIENT  JUNCTI
     +ON (FT)         DEPTH          WIDTH',/,
     *'   --------   --------      ----     -----    -----------  ------
     +-------        ------          -----')
 5421 FORMAT(//,
     +' *********************************************',/,
     +' *              ORIFICE DATA                 *',/,
     +' *********************************************',//,
     *'       FROM         TO                AREA      DISCHARGE   HEIGH
     +T ABOVE         RECTANGULAR ORIFICE',/,
     *'   JUNCTION   JUNCTION      TYPE    (MET2)    COEFFICIENT  JUNCTI
     +ON  (M)         DEPTH          WIDTH',/,
     *'   --------   --------      ----     -----    -----------  ------
     +-------        ------          -----')
 5440 FORMAT(1X,3I10,F10.2,F15.3,3F15.3)
 5441 FORMAT(1X,2A10,I10,F10.2,F15.3,3F15.3)
 5442 FORMAT(' ERROR ** ORIFICE ZP HEIGHT ABOVE UPSTREAM',
     a' NODE INVERT IS LESS THAN ZERO.'
     a,/,'          ORIFICE NUMBER = ', I10)
 5443	FORMAT(' ERROR ** OOPEN OR OCLOSE IS LESS THAN ZERO',
     a' FOR ORIFICE NUMBER ',I10)
 5444 FORMAT(' ERROR ** YTOP OR YCREST IS LESS THAN ZERO FOR WEIR',
     A' NUMBER ',I10)
 5445 FORMAT(/,' ====> ERROR !!!  GATED ORIFICE CONTROL JUNCTION ',I10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5446 FORMAT(/,' ====> ERROR !!!  GATED ORIFICE CONTROL JUNCTION ',A10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5447 FORMAT(' ERROR ** VRATE IS LESS THAN ZERO FOR PUMP NUMBER ',I10)
 5448 FORMAT(' ERROR ** PON OR POFF IS LESS THAN ZERO FOR PUMP',
     A' NUMBER ',I10)
 5449 FORMAT(' ERROR ** IPTYP EQUALS ',I4,' WHICH IS NOT A VALID',
     A' PUMP TYPE')
CIM END     OOOOOOOO
 5450 FORMAT(/,' ====> ERROR !!!  ORIFICE JUNCTION ',I10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5451 FORMAT(/,' ====> ERROR !!!  ORIFICE JUNCTION ',A10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5455 FORMAT(/,' ====> ERROR !!! ORIFICE TOP LIES ABOVE GROUND ELEVATION
     . AT JUNCTION ',I10)
 5456 FORMAT(/,' ====> ERROR !!! ORIFICE TOP LIES ABOVE GROUND ELEVATION
     . AT JUNCTION ',A10)
 5458 FORMAT(/,' ====> ERROR !!! ORIFICE OUTLET AT JUNCTION ',I10,
     1' IS HIGHER THAN INLET')
 5459 FORMAT(/,' ====> ERROR !!! ORIFICE OUTLET AT JUNCTION ',A10,
     1' IS HIGHER THAN INLET')
C  BAC START WWWWWWW
 5480 FORMAT(//,
     +' *********************************************',/,
     +' *                 WEIR DATA                 *',/,
     +' *********************************************',//,
     *'     FROM       TO        LINK               CREST       WEIR   '
     *,'   WEIR        DISCHARGE   SUBMERGENCE  NUMBER OF END  V-NOTCH '
     *,'ANGLE  SECOND DISCHARGE',/,
     *'   JUNCTION  JUNCTION    NUMBER      TYPE  HEIGHT(FT)   TOP(FT) '
     *,' LENGTH(FT)   COEFFICIENT   EQUATION    CONTRACTIONS   OR SIDE '
     *,'SLOPE    COEFFICIENT',/,
     *'   --------  --------    ------      ----  ----------   ------- '
     *,' ----------   -----------  -----------  -------------  --------'
     *,'-----  ----------------')
C    *'       FROM        TO      LINK                 CREST      WEIR
C    *      WEIR     DISCHARGE',/,
C    *'   JUNCTION  JUNCTION    NUMBER      TYPE  HEIGHT(FT)   TOP(FT)
C    *LENGTH(FT)   COEFFICIENT',/,
C    *'   --------  --------    ------      ----  ----------   -------
C    *----------   -----------')
 5481 FORMAT(//,
     +' *********************************************',/,
     +' *                 WEIR DATA                 *',/,
     +' *********************************************',//,
     *'     FROM       TO        LINK                CREST       WEIR  '
     *,'    WEIR       DISCHARGE   SUBMERGENCE  NUMBER OF END  V-NOTCH '
     * ,'ANGLE  SECOND DISCHARGE',/,
     *'   JUNCTION  JUNCTION    NUMBER      TYPE   HEIGHT(M)    TOP(M) '
     *,'  LENGTH(M)   COEFFICIENT   EQUATION    CONTRACTIONS   OR SIDE '
     *,'SLOPE    COEFFICIENT',/,
     *'   --------  --------    ------      ----   ---------    ------ '
     *,'  ---------   -----------  -----------  -------------  --------'
     *,'-----  ----------------')
C    *'       FROM        TO      LINK                 CREST      WEIR
C    *      WEIR     DISCHARGE',/,
C    *'   JUNCTION  JUNCTION    NUMBER      TYPE  HEIGHT (M)   TOP (M)
C    *LENGTH (M)   COEFFICIENT',/,
C    *'   --------  --------    ------      ----  ----------   -------
C    *----------   -----------')
 5485 FORMAT(1X,3I10,I10,F12.2,F10.2,F12.2,F14.4,7X,I1,10X,F5.1,7X,
     +F13.4,F17.2)
 5486 FORMAT(1X,3A10,I10,F12.2,F10.2,F12.2,F14.4,7X,I1,10X,F5.1,7X,
     +F13.4,F17.2)
C  BAC END   WWWWW
 5487 FORMAT(1X,4I10,F12.2,F10.2,F12.2,F14.4)
 5488 FORMAT(1X,3A10,I10,F12.2,F10.2,F12.2,F14.4)
 5490 FORMAT(/,' ====> ERROR !!! WEIR JUNCTION',I10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5491 FORMAT(/,' ====> ERROR !!! WEIR JUNCTION ',A10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5484 FORMAT(/,' ====> ERROR !!! STORAGE JUNCTION ',A10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5494 FORMAT(/,' ====> ERROR !!! STORAGE JUNCTION ',I10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5495 FORMAT(
     +'                                MAXIMUM OR          PEAK OR     '
     +,'   CROWN  ',/,
     +' STORAGE JUNCTION  JUNCTION  CONSTANT SURFACE   CONSTANT VOLUME '
     +,' ELEVATION',/,
     +'   NUMBER OR NAME      TYPE     AREA (FT2)        (CUBIC FEET)  '
     +,'    (FT)  ',/,
     +'   --------------  --------  ----------------   --------------- '
     +,' ---------')
 5496 FORMAT(
     +'                                MAXIMUM OR          PEAK OR     '
     +,'   CROWN  ',/,
     +' STORAGE JUNCTION  JUNCTION  CONSTANT SURFACE   CONSTANT VOLUME '
     +,' ELEVATION',/,
     +'   NUMBER OR NAME      TYPE     AREA  (M2)        (CUBIC MET.)  '
     +,'     (M)  ',/,
     +'   --------------  --------  ----------------   --------------- '
     +,' ---------')
CIM PP  NEW PUMP OUTPUT 5/1/97
 5560 FORMAT(//,
     +' *********************************************',/,
     +' *                 PUMP DATA                 *',/,
     +' *********************************************',//)
 5562 FORMAT(5X,'PUMP NUMBER ',I3,' PUMPED JUNCTION = ',I10,
     1'    RECEIVING JUNCTION = ',I10)
 5564 FORMAT(5X,'PUMP NUMBER ',I3,' PUMPED JUNCTION = ',A10,
     1'    RECEIVING JUNCTION = ',A10)
 5566 FORMAT(/,5X,'OFF-LINE PUMP (IPTYP = 1)   INITIAL STORAGE',
     1' VOLUME = ',F10.3,' CU. FT.')
 5570 FORMAT(/,5X,'OFF-LINE PUMP (IPTYP = 1)   INITIAL STORAGE',
     1' VOLUME = ',F10.3,' CU. M.')
 5568 FORMAT(/,'          WET WELL VOLUMES',/,
     1'          GREATER THAN         AND LESS            PUMPING',/,
     2'           OR EQUAL              THAN                RATE',/,
     3'            (FT^3)              (FT^3)              (CFS)')
 5576 FORMAT(/,5X,'IN-LINE PUMP (IPTYP = 2)')
 5578 FORMAT(/,'          PUMPED JUNCTION DEPTHS',/,
     1'          GREATER THAN         AND LESS            PUMPING',/,
     2'           OR EQUAL              THAN                RATE',/,
     3'             (FT)                (FT)               (CFS)')
 5574 FORMAT(3F20.3)
 5582 FORMAT(F20.3,20X,F20.3)
 5584 FORMAT(/,5X,'PUMP CURVE DATA (IPTYP = 3)   PUMP ON AT JUNCTION',
     1' DEPTH = ',F10.3,' FEET.   PUMP OFF AT JUNCTION DEPTH = ',
     2F10.3,' FEET.')
 5586 FORMAT(/,
     1'                                                   PUMPING',/,
     2'          JUNCTION HEAD DIFFERENCE                   RATE',/,
     3'                   (FEET)                           (CFS)')
 5592 FORMAT(F25.3,F30.3)
 5594 FORMAT(/,5X,'VARIABLE SPEED PUMP CURVE DATA (IPTYP = 4)')
 5596 FORMAT('                      PUMPED',/,
     1'                     JUNCTION                      PUMPING',/,
     2'                      DEPTH                          RATE',/,
     3'                       (FT)                         (CFS)')
 5600 FORMAT('          Pumping rate is linearly interpolated from ',
     1'the above values.')
 5602 FORMAT('          Pumping rate equals ',F10.3,' cfs for depths ',
     1'less than ',F10.3,' feet.')
 5604 FORMAT('          Pumping rate equals ',F10.3,' cfs for depths ',
     1'greater than ',F10.3,' feet.')
 5572 FORMAT(/,'         WET WELL VOLUMES',/,
     1'          GREATER THAN         AND LESS            PUMPING',/,
     2'           OR EQUAL              THAN                RATE',/,
     3'             (M^3)               (M^3)              (M3/S)')
 5580 FORMAT(/,'          PUMPED JUNCTION DEPTHS',/,
     1'          GREATER THAN         AND LESS            PUMPING',/,
     2'           OR EQUAL              THAN                RATE',/,
     3'             (M)                 (M)                (M3/S)')
 5588 FORMAT(/,5X,'PUMP CURVE DATA (IPTYP = 3)   PUMP ON AT JUNCTION',
     1' DEPTH = ',F10.3,' METERS.   PUMP OFF AT JUNCTION DEPTH = ',
     2F10.3,' METERS.')
 5590 FORMAT(/,
     1'                                                   PUMPING',/,
     2'          JUNCTION HEAD DIFFERENCE                   RATE',/,
     3'                  (METERS)                          (M3/S)')
 5598 FORMAT(/,'                      PUMPED',/,
     1'                     JUNCTION                      PUMPING',/,
     2'                      DEPTH                          RATE',/,
     3'                        (M)                         (M3/S)')
 5606 FORMAT('          Pumping rate equals ',F10.3,' M3/S for depths ',
     1'less than ',F10.3,' meters.')
 5608 FORMAT('          Pumping rate equals ',F10.3,' M3/S for depths ',
     1'greater than ',F10.3,' meters.')
 5575 FORMAT(/,' ====> ERROR !!! CANNOT HAVE EQUAL VRATE (HEAD) VALUES',
     *' ON PUMP CURVE (TO AVOID ZERO DIVIDE UPON INTERPOLATION).',/,
     *'       MUST PROVIDE AT LEAST MINIMAL HEAD DIFFERENCE.')
 8100 FORMAT(/,5X,'LIFT STATION TYPE PUMP DATA (IPTYP = 5)',/)
 8102 FORMAT(10X,'PUMP #',I2,' HAS A CAPACITY OF ',F10.3,A3,
     1' AND STARTS AT A DEPTH OF ',F10.3,A2)
 8104 FORMAT(/,10X,'ALL PUMPS TURN OFF WHEN DEPTH EQUALS ',F10.3,A2,/,
     110X,'PUMPS DELAY',F10.3,' SECONDS TO INCREASE FROM ZERO FLOW',
     2' TO CAPACITY')
CIM
 9010 FORMAT(\,' ===> ERROR !!! VRATES MUST BE INPUT IN INCREASING ',
     a'ORDER FOR IPTYP 1, 2, AND 4 PUMPS.')
 9020 FORMAT(\,' ===> ERROR !!! VRATES MUST BE INPUT IN DECREASING ',
     a'ORDER FOR IPTYP 3 PUMPS.')
CIM
 5610 FORMAT(/,' ====> ERROR !!! PUMP JUNCTION ',I10,
     +     ' IS NOT CONTAINED IN THE JUNCTION DATA')
 5611 FORMAT(/,' ====> ERROR !!! PUMP JUNCTION ',A10,
     +     ' IS NOT CONTAINED IN THE JUNCTION DATA')
 5615 FORMAT(/,' ====> ERROR !!!  MORE THAN ONE PIPE IS INFLUENT TO OFF-
     .LINE PUMP JUNCTION ',I10)
 5614 FORMAT(/,' ====> ERROR !!!  MORE THAN ONE PIPE IS INFLUENT TO OFF-
     .LINE PUMP JUNCTION ',A10)
 8010 FORMAT ('ERROR ==> The number of pumps exceeds the maximum',
     +' allowed by the program dimensions (NEP = ',I10,')',/,10X,
     +'Change NEP in TAPES.INC and recompile')
 5616 FORMAT(//,
     +' **************************************************',/,
     +' *          FREE OUTFALL DATA (DATA GROUP I1)     *',/,
     +' *         BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' **************************************************',/)
 5620 FORMAT(' OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5621 FORMAT(' OUTFALL AT JUNCTION....',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5630 FORMAT(/,' ====> ERROR !!!  FREE OUTFALL JUNCTION ',I10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5631 FORMAT(/,' ====> ERROR !!!  FREE OUTFALL JUNCTION ',A10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 8015 FORMAT ('ERROR ==> The number of free outfall exceeds the ',
     +'maximum number',
     +' allowed by the program dimensions (NTG = ',I10,')',/,10X,
     +'Change NTG in TAPES.INC and recompile')
 5656 FORMAT(//,
     +' ***********************************************',/,
     +' *    TIDE GATE OUTFALL DATA (DATA GROUP I2)   *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' ***********************************************',/)
 5660 FORMAT(' OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5661 FORMAT(' OUTFALL AT JUNCTION... ',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5662 FORMAT(/,' ====> ERROR !!!  TIDE GATE JUNCTION ',I10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5663 FORMAT(/,' ====> ERROR !!!  TIDE GATE JUNCTION ',A10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 8020 FORMAT ('ERROR ==> The number of outfall with tide gates ',
     +'exceeds the maximum number',
     +' allowed by the program dimensions (NTG = ',I10,')',/,10X,
     +'Change NTG in TAPES.INC and recompile')
 5665 FORMAT(//,
     +' **************************************************',/,
     +' *        INTERNAL CONNECTIVITY INFORMATION       *',/,
     +' **************************************************',/)
 5670 FORMAT('          CONDUIT     JUNCTION     JUNCTION',/,
     +       '          -------     --------     --------')
 5671 FORMAT('     CONDUIT      JUNCTION     JUNCTION',/,
     +       '     -------      --------     --------')
 5675 FORMAT(4X,I11,2I13)
 5676 FORMAT(/,' ====> ERROR !!! TOTAL NUMBER OF JUNCTIONS(INCLUDING WEI
     1RS) EXCEED PROGRAM DIMENSIONS, NEE =',I4)
 5677 FORMAT(/,' ====> ERROR !!! TOTAL NUMBER OF LINKS EXCEEDS PROGRAM D
     1IMENSIONS, NEE =',I4)
 5685 FORMAT(5X,A10,3X,A10,3X,A10)
CIM START OOOOOOOOOO
 6010 FORMAT(/,' ====> EQUIVALENT CIRCULAR PIPE INFORMATION ',
     .       'FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NUMBER..........................',I10,/,
     .       '       PIPE DIAMETER........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.5,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6011 FORMAT(/,' ====> EQUIVALENT CIRCULAR PIPE INFORMATION ',
     .       'FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NAME........................... ',A10,/,
     .       '       PIPE DIAMETER........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.5,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6012 FORMAT(/,' ====> EQUIVALENT RECTANGULAR PIPE INFORMATION',
     .       ' FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NUMBER..........................',I10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.5,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6013 FORMAT(/,' ====> EQUIVALENT RECTANGULAR PIPE INFORMATION ',
     .               'FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NAME........................... ',A10,/,
     .       '       PIPE DEPTH...........................',F12.2,/,
     .       '       PIPE WIDTH...........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.5,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6014 FORMAT(/,'       INVERT CHANGES AS GATE OPENS AND CLOSES')
  660 FORMAT(/,' ====> TIME CLOSURE GATE DATA FOR ORIFICE # ',I7,/,
     .       '       CONTROL JUNCTION NAME............... ',I10,/,
     .       '       DEPTH AT WHICH GATE OPENS............',F12.2,/,
     .       '       DEPTH AT WHICH GATE CLOSES...........',F12.2,/,
     .       '       CLOSED AREA..........................',F12.2,/,
     .       '       MINIMUM CLOSURE TIME (HOURS).........',F12.5,/,
     .       '       DIRECTION CONTROL....................',I12,/,
     .       '       PRINT OPTION.........................',I12)
  661 FORMAT(/,' ====> TIME CLOSURE GATE DATA FOR ORIFICE # ',I7,/,
     .       '       CONTROL JUNCTION NAME............... ',A10,/,
     .       '       DEPTH AT WHICH GATE OPENS............',F12.2,/,
     .       '       DEPTH AT WHICH GATE CLOSES...........',F12.2,/,
     .       '       CLOSED AREA..........................',F12.2,/,
     .       '       MINIMUM CLOSURE TIME (HOURS).........',F12.5,/,
     .       '       DIRECTION CONTROL....................',I12,/,
     .       '       PRINT OPTION.........................',I12)
  662 FORMAT(/,' ====> HEAD DEPENDENT CLOSURE GATE DATA FOR ORIFICE # '
     .       ,I7,/,
     .       '       CONTROL JUNCTION NAME............... ',I10,/,
     .       '       DEPTH AT WHICH GATE OPENS............',F12.2,/,
     .       '       DEPTH AT WHICH GATE CLOSES...........',F12.2,/,
     .       '       CLOSED AREA..........................',F12.2,/,
     .       '       MINIMUM CLOSURE TIME (HOURS).........',F12.5,/,
     .       '       DIRECTION CONTROL....................',I12,/,
     .       '       PRINT OPTION.........................',I12)
  663 FORMAT(/,' ====> HEAD DEPENDENT CLOSURE GATE DATA FOR ORIFICE # '
     .       ,I7,/,
     .       '       CONTROL JUNCTION NAME............... ',A10,/,
     .       '       DEPTH AT WHICH GATE OPENS............',F12.2,/,
     .       '       DEPTH AT WHICH GATE CLOSES...........',F12.2,/,
     .       '       CLOSED AREA..........................',F12.2,/,
     .       '       MINIMUM CLOSURE TIME (HOURS).........',F12.5,/,
     .       '       DIRECTION CONTROL....................',I12,/,
     .       '       PRINT OPTION.........................',I12)
CIM END   OOOOOOOOOO
CIM START <><><><><><><>
 5350 FORMAT(/,' ===> JUNCTION ',I10,
     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT, PUMP STATION, ',
     2         'WEIR, OR ORIFICE.')
 5351 FORMAT(/,' ===> JUNCTION ',A10,
     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT, PUMP STATION, ',
     2         'WEIR, OR ORIFICE.')
 5352 FORMAT(/,' ERROR  ',I5,' JUNCTIONS ARE NOT CONNECTED ',/,
     1         ' RUN IS STOPPED')
 7000 FORMAT(' ERROR - ORIFICE TYPE NOT VALID FOR ORIFICE ',
     A'NJUNC1 = ',I10,' NJUNC2 = ',I10,' NKLASS = ',I10)
 7001 FORMAT(' ERROR - ORIFICE TYPE NOT VALID FOR ORIFICE ',
     A'KJUNC1 = ',A10,' KJUNC2 = ',A10,' NKLASS = ',I10)
CIM END     <><><><><><>
 6660 FORMAT(//,
     +' ***********************************************',/,
     +' *               WEIR OUTFALL DATA             *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' ***********************************************',/)
 6665 FORMAT(' WEIR OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 6670 FORMAT(' WEIR OUTFALL AT JUNCTION... ',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 6680 FORMAT(//,
     +' ***********************************************',/,
     +' *               PUMP OUTFALL DATA             *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP H1    *'/,
     +' ***********************************************',/)
C#### WCH, 1/23/95.
 7005 FORMAT(15X,'WARNING - ZCROWN IS GREATER THAN GROUND ELEVATION.',
     1'  GROUND ELEVATION IS SET TO ZCROWN + 0.1')
 7010 FORMAT(15X,'WARNING - MAXIMUM SURCHARGE ELEVATION IS LESS THAN',
     1' GROUND ELEVATION.  SURCHARGE ELEVATION IS SET EQUAL TO THE',
     2' GROUND ELEVATION')
 9119 FORMAT(/,' ERROR! ===> NUMST on line E1 is greater than allowed by
     1 NVST on the parameter statement on TAPES.INC')
 8005 FORMAT(/,' WARNING ===> At storage junction ',I10,' the user ',
     +'input ZTOP equals ',F10.3,/,14X,' which is less than the top ',
     +'of the highest pipe entering the junction (',f10.3,')',/,14X,
     +' ZTOP is set equal to highest crown elevation')
 8006 FORMAT(/,' WARNING ===> At storage junction ',A10,' the user ',
     +'input ZTOP equals ',F10.3,/,14X,' which is less than the top ',
     +'of the highest pipe entering the junction (',f10.3,')',/,14X,
     +' ZTOP is set equal to highest crown elevation')
 8011 FORMAT(5X,'WARNING - INVERT OF JUNCTION ID ',I10,' WAS LOWERED',
     a' TO EQUAL ',F10.3,' TO ALLOW FOR BOTTOM OUTLET ORIFICE')
 8012 FORMAT(5X,'WARNING - INVERT OF JUNCTION ID ',A10,' WAS LOWERED',
     a' TO EQUAL ',F10.3,' TO ALLOW FOR BOTTOM OUTLET ORIFICE')
8330  FORMAT('ERROR - TOO MANY CONDUITS ENTERING JUNCTION ',I10,/,
     +'Change connectivity or increase NCHN in TAPES.INC and ',
     +'recompile program')
8331  FORMAT('ERROR - TOO MANY CONDUITS ENTERING JUNCTION ',A10,/,
     +'Change connectivity or increase NCHN in TAPES.INC and ',
     +'recompile program')
8400	FORMAT(10X,'PUMP TURNS ON WHEN DEPTH EQUALS  ',F10.3,'FEET',/,
     1       10X,'PUMP TURNS OFF WHEN DEPTH EQUALS ',F10.3,'FEET')
C=======================================================================
      END
