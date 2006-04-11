      SUBROUTINE NHEAD(N,NL,NH,HEAD1,HEAD2,QP,AREA,VEL,HRAD,
     +                          ANH,ANL,RNL,RNH,IDOIT,KINK,AS1)
C     EXTRAN BLOCK
C	CALLED BY _ROUTE ROUTINES AND ELSEWHERE
C=======================================================================
C     This subroutine performs the following functions:
C       1 - Converts NODAL depths to pipe depths.
C       2 - Assigns conduit surface areas to upstream and downstream
C           junctions depending on depth and flow conditions. Note that
C           a surface area is not assigned to orifice or weir links.
C       3 - Computes average conduit hydraulic parameters AREA, VEL, HRAD
C           used to compute flow in _ROUTE routines.
C
C     WCH, 11/16/93.  CORRECT MID-CHANNEL WIDTH VALUE FOR NORMAL
C       SUBCRITICAL FLOW CASE.
C     RED, 12/31/93.  CORRECTION FOR INTEGER IDOIT VALUE.
C     WCH, 12/7/94.  ADD RED COMMENTS, SET FASNH = 1.0 ALWAYS, AND
C       MAKE SEVERAL CODE CHANGES TO CORRESPOND TO RED CODE.
C     WRM, 07/01/1997 CREATED NHEAD FROM HEAD BY ADDING ARGUMENT AS1
C       AND CHANGING ALL OCCURANCES OF AS->AS1. REDUCTIONS INTO AS1
C       ARE NO LONGER IN COMMON AND CAN THUS BE PERFORMED IN PARALLEL.
C
C   Revised by Chuck Moore (CDM Annandale to improve computational
C   efficiency.  Gives results identical to original program.
C   Revised by C. Moore and Brett Cunningham to improve consistency
C   in calculations.  Major revision to improve calculation of
C   representative or average pipe characteristics to provide
C   consistent flow calculations in _ROUTE subroutines.
C
C   Definition of terms.
C     Downstream end of pipe is the end with the lowest pipe invert
C     elevation.
C
C                                       UPSTREAM  DOWNSTREAM
C
C     Junction                             NL        NH
C     Junction invert elevation            Z(NL)     Z(NH)
C     Conduit invert elevation             ZU(N)     ZD(N)
C     Flow elevation                       HEAD1     HEAD2
C     Flow depth in conduit                YNL       YNH
C
C     IDOIT = 0, Means a dry pipe and no flow calculation.
C     Note, HEAD1 = Z(NL) + Y(NL) in calling subroutines, and ZU not
C     necessarily = Z(NL).
C     Also, HEAD2 = Z(NH) + Y(NH) in calling subroutines, and ZD not
C     necessarily = Z(NH).
C
C     Assigns pipes to one of four conditions:
C     SUB - Depths of flow at both the upstream and downstream ends
C           is are greater than the critical and normal flow depths.
C           Use depths at upstream and downstream junctions for depths
C           at upstream and downstream ends of conduit.  Assigns 1/2 of
C           conduit surface area to upstream and downstream junctions.
C     CR1 - Reverse flow and depth at upstream junction is less than
C           critical flow depth.  Flow must pass through critical at
C           upstream end.
C           Use critical flow depth at upstream end, junction depth
C           at downstream end.  Flow must pass through critical at
C           downstream end.  Apply conduit surface area to downstream
C           junction.
C     CR2 - Positive flow and depth at downstream junction is less than
C           critical or normal depth.  Assign minimum of normal or
C           critical depth to downstream end of conduit and junction
C           elevation at the upstream end.  Flow must pass through
C           critical or normal depth at downstream end.  Assign conduit
C           surface area to upstream junction.
C     DR1 - If pipe is completely dry, set all parameters to small
C           values and return.
C           If the depth in the downstream end is such that there is
C           water in the pipe, set depth at downstream end to junction
C           depth.  Set depth at upstream end to small value.  Estimate
C           actual surface area in pipe and assign to downstream junction.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'OUT.INC'
      DIMENSION AS1(NEE)
CIMspeed  don't have character if statements
CIM  make KINK integer
cim  make appropriate changes to array link in other parts of program
cim  add variables DR1, SUB, CR1, and CR2 to new common in OUT.INC
cim  initialize in parameter statement
cim      CHARACTER*3 KINK
C=======================================================================
      YNL   =  HEAD1 - ZU(N)
cim limit ynl and ynh to crown of pipe and to be greater than zero
      YNL   = AMIN1(YNL,DEEP(N) + 2.0 * fudge)
      YNL   = AMAX1(YNL,fudge)
      YNH   =  HEAD2 - ZD(N)
      YNH   = AMIN1(YNH,DEEP(N) + 2.0 * fudge)
      YNH   = AMAX1(YNH,fudge)
      FASNH = 1.0
      IDOIT = 1
CIM SPEED MOVE FUDGE INITIALIZATION TO TRANSX      FUDGE = 0.0001
CIM fudge now equals 0.00001
CIM REDO THESE IF STATEMENTS
      IF(YNL.GT.FUDGE.AND.YNH.GT.FUDGE) THEN
C=======================================================================
C     Computations for YNL > 0.0, YNH > 0.0   start
C     Elevation in both upstream and downstream junctions is
C     greater than the pipe invert
C=======================================================================
C              FLOW = QP < 0.0  ==> ADVERSE FLOW
C=======================================================================
C :(    Eliminate checks between pipe invert elevation and invert
C :(    elevation of adjacent node.  By definition, the elevation of
C :(    all pipes must be greater than or equal to the junction invert.
C :(    Only sump type orifices can have inverts less than upstream
C :(    conduit.  Note that there is special code below for sump orifice,
C :(    this orifice code may need to be modified to account for the
C :(    commenting out of these lines.
C :(      The conduit is SUB if the invert elevation of
C :(      the upstream junction is less than the invert elevation
C :(      of the conduit, and subcritical or critical otherwise.
C         SUB if upstream junction depth is greater than the
C         critical conduit depth.
C         CR1 if upstream junction depth is less than the
C         critical conduit depth.
C=======================================================================
           IF(QP.LT.0.0) THEN
                    IF(ZU(N)-Z(NL).LE.FUDGE) THEN
                         KINK = SUB
                         ELSE
                     CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
C####                     IF(YC-YNL.LE.FUDGE) KINK = SUB
C####                     IF(YC-YNL.GT.FUDGE) KINK = CR1
C#### WCH (RED), 12/7/94.  CHANGE TO FORM USED BY RED.
                     IF(YC+ZU(N)-HEAD1.LE.FUDGE) KINK = SUB
                     IF(YC+ZU(N)-HEAD1.GT.FUDGE) KINK = CR1
                         ENDIF
C                ENDIF  (change end if to else below)
C=======================================================================
C               FLOW = QP > 0.0   ==> POSITIVE FLOW
C=======================================================================
C         CR2 if the downstream junction depth is less than the
C         minimum of critical or normal conduit depth.
C         SUB if the downstream junction depth is greater than the
C         maximum of critical or normal conduit depth.
C         If the downstream depth is between normal and critical,
C         calculate the value of weighting variable FASNH.
C=======================================================================
CIM change to else           IF(QP.GE.0.0) THEN
          ELSE
c   ZD should not be less than Z (maybe orifices)
                    IF(ZD(N)-Z(NH).LE.FUDGE) THEN
                     KINK = SUB
                     ELSE
                     CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                     Y2 = AMIN1(YC,YNORM)
C$$$$$  Brett Cunningham  change FUDGE to -FUDGE below
                     IF(Y2-YNH.GT.FUDGE) THEN
                          KINK = CR2
C$$$$$  Brett Cunningham  change FUDGE to -FUDGE below?????
                          ELSE IF(YNH-AMAX1(YC,YNORM).GT.FUDGE) THEN
                             KINK = SUB
                          ELSE
C=======================================================================
C     Variable FASNH accounts for case in which downstream depth, YNH,
C     is between critical and normal depth of entering pipe.  Idea is
C     if YNH ~ Ycrit then FASNH ~ small and don't use much pipe area
C     for this junction since entering flow is very near critical.  If
C     YNH ~ Ynorm, then FASNH ~ 1.0 and use most of pipe area for this
C     junction since entering flow is near normal.  However, must
C     check for QP = 0 and YNH = 0 ==> Ycrit = Ynorm = YNH = 0.  In
C     this case, assume downstream flow will be close to critical
C     and set FASNH = 0.
C=======================================================================
C#### WCH, 12/7/94.  CHECK FOR ZERO DENOMINATOR.
                          IF(ABS(YNORM-YC).GT.FUDGE) THEN
                             FASNH = (YNH-Y2)/ABS(YNORM-YC)
                             ELSE
                             FASNH = 0.0
                             ENDIF
                          ENDIF
                         ENDIF
                ENDIF
cimspeedy           ENDIF
cimspeedy       End of Computations for YNL > 0.0, YNH > 0.0
cim
      else
      IF(YNL.LE.FUDGE.AND.YNH.LE.FUDGE) THEN
C=======================================================================
C     DRY PIPE-> depth in upstream and downstream junctions is less than
C     the pipe invert elevation.
C
C=======================================================================
cim don't think that this should be done but not too sure
cim take out for now 6/97
c           HEAD1     = HEAD2
           YMID      = FUDGE
           KINK      = DR1
           ANH       = FUDGE
           ANL       = FUDGE
           RNL       = FUDGE
           BMID      = FUDGE
C#### WCH (RED), 12/7/94.  Add AMID and RMID.
           AMID      = FUDGE
           RMID      = FUDGE
           AREA      = FUDGE
           VEL       = 0.0
           QO(N)     = 0.0
           HRAD      = FUDGE
C#### RED (WCH), 12/31/93.  CORRECT INTEGER IDOIT TO BE ZERO.
C####    IDOIT     = FUDGE
           IDOIT     = 0
           AOVERB(N) = FUDGE
C#### WCH, 12/7/94.  RED CODE USES SAME CALC. FOR NKLASS = 10.
C####           IF(NKLASS(N).LE.9)  THEN
           AS1(NL) = AS1(NL) + BMID*LEN(N)/2.0
           AS1(NH) = AS1(NH) + BMID*LEN(N)/2.0
C####                ENDIF
C####           IF(NKLASS(N).EQ.10) AS1(NL) = AS1(NL) + BMID*LEN(N)/2.0
c     write(n6,*) 1,n,qp,ynl,ynh,ymid,kink
           RETURN
C  end of dry pipe calculations.
cim speedy           ENDIF
           elseif (YNH.GT.FUDGE) THEN
C=======================================================================
C     YNL LE 0, YNH.GT.0
C     YNL < 0.0, YNH > 0.0
C     Elevation at upstream junction is less than US conduit invert.
C     Elevation at downstream junction is greater than DS conduit invert.
C     If downstream HGL is less than the invert of the upstream
C     end of the conduit, the conduit has no flow, use condition DR1
C     If downstream HGL is greater than the invert of the upstream
C     end of the conduit, flow is in reverse direction, assign critical
C     depth to upstream end, use condition CR1.
C=======================================================================
CIMSPEED      IF(YNL.LE.FUDGE) THEN
           IF(HEAD2-ZU(N).LT.FUDGE) THEN
                KINK = DR1
                    ELSE IF(ZU(N).LE.Z(NL)) THEN
                       KINK = SUB
                ELSE
                CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                KINK = CR1
                ENDIF
cimspeed           ENDIF
C=======================================================================
C     YNH LE 0, YNL GT 0
C     YNL > 0.0, YNH < 0.0
C     Here elevation in downstream node is less than DS pipe invert and
C     elevation in upstream node is greater than US pipe invert.
C     Always minimum of critical or normal downstream, CR2
C     RED added the following code..
C     If upstream HGL is less than the invert of the downstream
C     end of conduit + fudge, conduit is dry (RED code) (very flat pipe)
C     1 - This should never ever happen.  If the upstream pipe invert end
C         by definition is always greater than or equal to the elevation of
C         the downstream invert, then if the elevation is greater than the
C         upstream invert it must alway be greater than the invert of the
C         downstrea invert.
C     2-  The way that the program assigns YNL to fudge in DR1 part of the
C         code is not correct even if this could occur.
C         Comment these lines out.
C=======================================================================
      else
cimspeed      IF(YNH.LE.FUDGE) THEN
C#### WCH (RED), 12/7/94.  ADD ADDITIONAL IF INVOLVING HEAD1 AND GOTO2000.
C :*         IF(HEAD1-ZD(N).LT.FUDGE) THEN
C :*            KINK = DR1
                    IF(ZD(N).LE.Z(NH)) THEN
                    KINK = SUB
                ELSE
            CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
            Y2 = AMIN1(YC,YNORM)
            KINK = CR2
                ENDIF
         GO TO 2000
         ENDIF
         ENDIF
C=======================================================================
C     SPECIAL CONDITION FOR SUMP ORIFICES
C=======================================================================
C#### WCH, 12/7/94.  THIS IF-GROUP HAS BEEN COMMENTED OUT IN OSU/EPA
C     CODE. UN-COMMENT (USE FORTRAN LINES) TO CORRESPOND TO RED CODE.
C     WCH: AGREE WITH THESE LINES.  ELSE IF DOWNSTREAM INVERT, ZD, <
C     INVERT OF DOWNSTREAM JUNCTION, Z(NH) OR Z(J) BELOW, THEN
C     EQUIVALENT ORIFICE CONDUIT WILL ALWAYS "SEE" A DOWNSTREAM DEPTH
C     OF AT LEAST THE POSITIVE DIFFERENCE, Z(NH) - ZD WHEN IN FACT THE
C     DEPTH SHOULD BE ZERO.  THESE STATEMENTS ALSO KEEP INITIAL VOLUME
C     IN ORIFICE PIPE SET AT ZERO VALUE (OR AS CLOSE AS POSSIBLE USING
C     FUDGE VALUE FOR CROSS SECTIONAL AREA).
CIM START  OOOOOOOOOOO
CIM   MODIFY TO INCLUDE RECTANGULAR BOTTOM OUTLET ORIFICE
C=======================================================================
      IF(((NKLASS(N).EQ.52).OR.(NKLASS(N).EQ.54))
CIM END OOOOOOOOOOO
     A    .AND.YNL.LE.0.96*DEEP(N)) THEN
              J   = NJUNC(N,2)
              YNH = HEAD2 - Z(J)
              IF(YNH.LE.FUDGE) THEN
                               CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                               Y2 = AMIN1(YC,YNORM)
                               KINK  = CR2
                               ELSE
                               KINK  = SUB
                               ENDIF
              ENDIF
C=======================================================================
C     'NORMAL SITUATION' HALF SURFACE AREA AT EACH END
C=======================================================================
C#### WCH, 12/7/94.  NOTE, RED CODE HAS SPECIAL SITUATION FOR OUTFALL
C     CONDUITS.  OSU/EPA CODE DOES NOT HAVE VARIABLE HTAS.
C
C      IF(HTAS(NL).GT.0.AND.HTAS(NH).GT.0) THEN
C                                    IF(KINK.EQ.CRT1) KINK = SUBN
C                                    IF(KINK.EQ.CRT2) KINK = SUBN
C                                    ENDIF
C=======================================================================
C#### WCH (RED), 12/7/94.  CREATE STMT 2000.
C
 2000 CONTINUE
CIM SPEEDY IF(KINK.EQ.SUB) THEN
       SELECT CASE (KINK)
       CASE (SUB)
CIM SPEEDY
change ----------------------------------------------------------------
CHANGE DEFAULT CASE IS TO USE MIDDLE VALUE
C
change           YMID = 0.5*(YNL+YNH)
      FACTOR = 0.5
CHANGE  This is correction for YMID, do only if called for by IM2
      IF (IM2.EQ.1) THEN
CHANGE  IF Q is positive and HGL slope is greater than bottom slope
       IF ((QP.GT.0.0).AND.((HEAD1-HEAD2)-(ZU(N)-ZD(N)).GT.0.0)) THEN
CHANGE YNORM GT YC INDICATES MILD BOTTOM SLOPE - This is M2 condition....
        IF(YNORM.GT.YC) THEN
          FACTOR = RM2INTER(YNL,YNH,YNORM,YC,NKLASS(N),DEEP(N))
        ELSEIF  (YNORM.LT.YC) THEN
CHANGE THIS IS FOR STEEP BOTTOM SLOPE S2 drawdown condition
        FACTOR = RS2INTER(YNL,YC,YNORM,YC,NKLASS(N),DEEP(N))
        ELSE
CHANGE ALSO TRAP FOR CRITICAL BOTTOM SLOPE (YNORM=YC) to avoid zero divide
          FACTOR = 0.5
        ENDIF
       ENDIF
      ENDIF
          YMID = YNH + (YNL-YNH)*FACTOR
CHANGE
change ----------------------------------------------------------------
           IF(YMID.LT.FUDGE) YMID = FUDGE
           CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
C#### WCH, 11/16/93.  SERIOUS ERROR.  CHANGE "WIDTH" TO BMID!!
C####         CALL HYDRAD(N,NKLASS(N),YMID,HRAD,AREA,WIDTH)
           CALL HYDRAD(N,NKLASS(N),YMID,HRAD,AREA,BMID)
           CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
C#### WCH, 12/7/94.  RED CODE DOES NOT DISTIGUISH BETWEEN KLASS 10
C     AND OTHERS.
C
C####           IF(NKLASS(N).LE.9) THEN
           AS1(NL) = AS1(NL)+0.25*(BNL+BMID)*LEN(N)
           AS1(NH) = AS1(NH)+0.25*(BMID+BNH)*LEN(N)*FASNH
C####                ENDIF
C####           IF(NKLASS(N).EQ.10)AS1(NL) = AS1(NL) + BMID*LEN(N)
C=======================================================================
C#### WCH, 12/7/94.  RED CODE OMITS FOLLOWING LINES UP TO RETURN AND
C     USES COMPLEX WEIGHTING CALCULATION AT END.  DON'T USE
C     RED CODE UNTIL BETTER UNDERSTOOD BY WCH.  THUS, LEAVE THESE LINES.
C=======================================================================
change ----------------------------------------------------------------
change           AREA      = 0.50*(ANL+ANH)
change           HRAD      = 0.50*(RNL+RNH)
change           WIDTH     = 0.50*(BNL+BNH)
          WIDTH = BMID
change
change ----------------------------------------------------------------
           HRAD      = AMAX1(HRAD,FUDGE)
           WIDTH     = AMAX1(WIDTH,FUDGE)
           VEL       = QP/AREA
           AOVERB(N) = AREA/WIDTH
c     write(n6,*) 2,n,qp,ynl,ynh,ymid,kink
           RETURN
cim speedy           ENDIF
C=======================================================================
C     CRITICAL SECTION UPSTREAM AND SURFACE AREA DOWNSTREAM
C=======================================================================
cim speedy      IF(KINK.EQ.CR1) THEN
        CASE (CR1)
cim speedy
           HEAD1 = YC + ZU(N)
           YNL   = YC
change ----------------------------------------------------------------
c
CHANGE DEFAULT CASE IS TO USE MIDDLE VALUE
C
change           YMID = 0.5*(YNL+YNH)
      FACTOR = 0.5
CHANGE  This is correction for YMID, do only if called for by IM2
      IF (IM2.EQ.1) THEN
CHANGE  IF Q is positive and HGL slope is greater than bottom slope
       IF ((QP.GT.0.0).AND.((HEAD1-HEAD2)-(ZU(N)-ZD(N)).GT.0.0)) THEN
CHANGE YNORM GT YC INDICATES MILD BOTTOM SLOPE - This is M2 condition....
        IF(YNORM.GT.YC) THEN
          FACTOR = RM2INTER(YNL,YNH,YNORM,YC,NKLASS(N),DEEP(N))
        ELSEIF  (YNORM.LT.YC) THEN
CHANGE THIS IS FOR STEEP BOTTOM SLOPE S2 drawdown condition
        FACTOR = RS2INTER(YNL,YC,YNORM,YC,NKLASS(N),DEEP(N))
        ELSE
CHANGE ALSO TRAP FOR CRITICAL BOTTOM SLOPE (YNORM=YC) to avoid zero divide
          FACTOR = 0.5
        ENDIF
       ENDIF
      ENDIF
          YMID = YNH + (YNL-YNH)*FACTOR
CHANGE
change ----------------------------------------------------------------
           IF(YMID.LE.FUDGE) YMID = FUDGE
           CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
           CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
           CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
C#### WCH, 12/7/94.  RED USES COEF OF 0.5, NOT 0.25.
C####           AS1(NH) = AS1(NH) + 0.25*(BMID+BNH)*LEN(N)
           AS1(NH) = AS1(NH) + 0.5*(BMID+BNH)*LEN(N)
cim speedy           ENDIF
C=======================================================================
C     CRITICAL SECTION DOWNSTREAM AND SURFACE AREA UPSTREAM
C=======================================================================
cim speedy      IF(KINK.EQ.CR2) THEN
       CASE (CR2)
cim speedy
           YNH   = Y2
           HEAD2 = YNH + ZD(N)
change ----------------------------------------------------------------
CHANGE DEFAULT CASE IS TO USE MIDDLE VALUE
C
change           YMID = 0.5*(YNL+YNH)
      FACTOR = 0.5
CHANGE  This is correction for YMID, do only if called for by IM2
      IF (IM2.EQ.1) THEN
CHANGE  IF Q is positive and HGL slope is greater than bottom slope
       IF ((QP.GT.0.0).AND.((HEAD1-HEAD2)-(ZU(N)-ZD(N)).GT.0.0)) THEN
CHANGE YNORM GT YC INDICATES MILD BOTTOM SLOPE - This is M2 condition....
        IF(YNORM.GT.YC) THEN
          FACTOR = RM2INTER(YNL,YNH,YNORM,YC,NKLASS(N),DEEP(N))
        ELSEIF  (YNORM.LT.YC) THEN
CHANGE THIS IS FOR STEEP BOTTOM SLOPE S2 drawdown condition
        FACTOR = RS2INTER(YNL,YC,YNORM,YC,NKLASS(N),DEEP(N))
        ELSE
CHANGE ALSO TRAP FOR CRITICAL BOTTOM SLOPE (YNORM=YC) to avoid zero divide
          FACTOR = 0.5
        ENDIF
       ENDIF
      ENDIF
          YMID = YNH + (YNL-YNH)*FACTOR
CHANGE
change ----------------------------------------------------------------
           IF(YMID.LE.FUDGE) YMID = FUDGE
           CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
           CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
           CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
           AS1(NL) = AS1(NL) + 0.25*(BNL+BMID)*LEN(N)
cim speedy           ENDIF
C=======================================================================
C     DRY UPSTREAM AND SURFACE AREA DOWNSTREAM
C=======================================================================
cim speedy      IF(KINK.EQ.DR1) THEN
          CASE (DR1)
cim speedy
CIM BAC This next statement seems to be a total screw up,
CIM  No reason to change upstream head to downstream head!!
CIM           HEAD1 = HEAD2
           YNL   = FUDGE
C#### WCH, 12/7/94.  RED CODE DIFFERS CONSIDERABLY.
C     BELOW, COMMENT OUT OLD LINES AND INCLUDE COMMENT FOR EACH
C     NEW RED LINE.  BASICALLY, DON'T USE MID (YMID, ETC.) VALUES.
C####           YMID  = HEAD2-0.5*(ZU(N)+ZD(N))
C####           IF(YMID.LT.FUDGE) YMID = FUDGE
CIM if YNL = fudge simply set others to fudge and get on with it.?
           CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
cim              RNL = FUDGE
cim              ANL = FUDGE
cim              BNL = FUDGE
CIM
C####           CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
           CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
C####           AREA      = 0.25*(ANL+2.0*AMID+ANH)
C####           HRAD      = 0.5*(RMID+RNH)
C#### WCH, 12/7/94.  NEW AREA =   AND HRAD =.
cim note that if ANL is FUDGE then AREA will be FUDGE
           AREA      = SQRT(ANL*ANH)
           AREA      = AMAX1(AREA,FUDGE)
cim set HRAD to that of downstream end?
           HRAD      = RNH
           HRAD      = AMAX1(HRAD,FUDGE)
C
           VEL       = 0.0
           QO(N)     = 0.0
           IDOIT     = 0
           AOVERB(N) = FUDGE
C#### WCH, 12/7/94.  RED INCLUDES CALC. FOR HGL NOT IN OSU/EPA CODE. OMIT.
C####         HGL(N,1)  = YNL + ZU(N)
C####         HGL(N,2)  = YNH + ZD(N)
C#### OLD OSU/EPA CODE:
C####           AS1(NH) = AS1(NH) + 0.25*(BMID+BNH)*LEN(N)
C####           IF(ZU(N)-Z(NL).LT.FUDGE) AS1(NL) =
C####     +                            AS1(NL) + 0.25*(BNL+BMID)*LEN(N)
C#### NEW RED CODE.
cim seems like this overestimates area, entire length won't be flooded to
cim width of downstream end.
           AS1(NH)    = AS1(NH) + BNH*LEN(N)
cim  seems like this next line double counts conduit area. Again ZU(N)
cim  should not be less than Z(NL)
           IF(ZU(N)-Z(NL).LT.FUDGE) AS1(NL) = AS1(NL) + BNL*LEN(N)
C#### OSU/EPA CODE DOES NOT INCLUDE LINK(N)
C####           LINK(N)   = KINK
C
c     write(n6,*) 3,n,qp,ynl,ynh,ymid,kink
           RETURN
cim speedy           ENDIF
      END SELECT
cim speedy
C=======================================================================
C     COMPUTE CROSS-SECTION AREA, VELOCITY * HYDRAULIC RADIUS
C=======================================================================
C#### WCH, 12/7/94.  HERE, RED CODE USES COMPLEX WEIGHTING TO CALCULATE
C     AREA, HRAD AND WIDTH.  DO NOT USE UNTIL BETTER UNDERSTOOD BY WCH.
C     SO THIS CODE REMAINS UNCHAGED.
C=======================================================================
change ----------------------------------------------------------------
c      AREA  = 0.25*(ANL+2.0*AMID+ANH)
c      HRAD  = 0.25*(RNL+2.0*RMID+RNH)
c      WIDTH = 0.25*(BNL+2.0*BMID+BNH)
      AREA = AMID
      HRAD = RMID
      WIDTH = BMID
change
change ----------------------------------------------------------------
      HRAD  = AMAX1(HRAD,FUDGE)
      WIDTH = AMAX1(WIDTH,FUDGE)
      IF(AREA.LE.FUDGE) THEN
           VEL       = 0.0
           QO(N)     = 0.0
           IDOIT     = 0
           AREA      = FUDGE
           AOVERB(N) = FUDGE
           ELSE
           VEL       = QP/AREA
           AOVERB(N) = AREA/WIDTH
           IF(VEL.GT.50.0)  VEL =  50.0
           IF(VEL.LT.-50.0) VEL = -50.0
           ENDIF
c     write(n6,*) 4,n,qp,ynl,ynh,ymid,kink
      RETURN
      END
