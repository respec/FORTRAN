      SUBROUTINE HEAD(N,NL,NH,HEAD1,HEAD2,QP,AREA,VEL,HRAD,
     +                          ANH,ANL,RNL,RNH,IDOIT,KINK)
C=======================================================================
C     THIS SUBROUTINE CONVERTS NODAL DEPTHS TO PIPE DEPTHS
C     IT ALSO ASSIGNS SURFACE AREAS TO THE PROPER NODES
C     SURFACE AREA IS NOT ASSIGNED TO ORIFICE OR WEIR LINKS
C     WCH, 11/16/93.  CORRECT MID-CHANNEL WIDTH VALUE FOR NORMAL
C       SUBCRITICAL FLOW CASE.
C     RED, 12/31/93.  CORRECTION FOR INTEGER IDOIT VALUE.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'OUT.INC'
      CHARACTER*3 KINK
C=======================================================================
      YNL   = HEAD1 - ZU(N)
      YNH   = HEAD2 - ZD(N)
      FASNH = 1.0
      IDOIT = 1
      FUDGE = 0.0001
C=======================================================================
C     DRY PIPE-> NO SURFACE AREA FOR ENDS WITH NEGATIVE Y
C=======================================================================
      IF(YNL.LE.FUDGE.AND.YNH.LE.FUDGE) THEN
         HEAD1     = HEAD2
         YMID      = FUDGE
         KINK      = 'DR1'
         ANH       = FUDGE
         ANL       = FUDGE
         RNL       = FUDGE
         BMID      = FUDGE
         AREA      = FUDGE
         VEL       = 0.0
         QO(N)     = 0.0
         HRAD      = FUDGE
C#### RED (WCH), 12/31/93.  CORRECT INTEGER IDOIT TO BE ZERO.
C####    IDOIT     = FUDGE
         IDOIT     = 0
         AOVERB(N) = FUDGE
         IF(NKLASS(N).LE.9)  THEN
                             AS(NL) = AS(NL) + BMID*LEN(N)/2.0
                             AS(NH) = AS(NH) + BMID*LEN(N)/2.0
                             ENDIF
         IF(NKLASS(N).EQ.10) AS(NL) = AS(NL) + BMID*LEN(N)/2.0
         RETURN
         ENDIF
C=======================================================================
C     YNL LE 0, YNH..GT.0 (CRIT OR NORM UPSTRM OR STORAGE DWNSTRM)
C=======================================================================
      IF(YNL.LE.FUDGE) THEN
             IF(HEAD2-ZU(N).LT.FUDGE) THEN
                      KINK = 'DR1'
                      ELSE IF(ZU(N).LE.Z(NL)) THEN
                           KINK = 'SUB'
                           ELSE
                           CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                           KINK = 'CR1'
                           ENDIF
                      ENDIF
C=======================================================================
C     YNH LE 0, YNL GT 0, CRITICAL OR NORM DOWNSTREAM
C=======================================================================
      IF(YNH.LE.FUDGE) THEN
                     IF(ZD(N).LE.Z(NH)) THEN
                              KINK = 'SUB'
                              ELSE
                              CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                              Y2 = AMIN1(YC,YNORM)
                              KINK = 'CR2'
                              ENDIF
                     ENDIF
C=======================================================================
      IF(YNL.GT.FUDGE.AND.YNH.GT.FUDGE) THEN
C=======================================================================
C                                   QP < 0  ADVERSE FLOW
C=======================================================================
      IF(QP.LT.0.0) THEN
                    IF(ZU(N)-Z(NL).LE.FUDGE) THEN
                       KINK = 'SUB'
                       ELSE
                       CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
                       IF(YC-YNL.LE.FUDGE) KINK = 'SUB'
                       IF(YC-YNL.GT.FUDGE) KINK = 'CR1'
                       ENDIF
                    ENDIF
C=======================================================================
C                      POSITIVE FLOW
C=======================================================================
      IF(QP.GE.0.0) THEN
      IF(ZD(N)-Z(NH).LE.FUDGE) THEN
               KINK = 'SUB'
               ELSE
               CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
               Y2 = AMIN1(YC,YNORM)
               IF(Y2-YNH.GT.FUDGE) THEN
                     KINK = 'CR2'
                     ELSE IF(YNH-AMAX1(YC,YNORM).GT.FUDGE) THEN
                          KINK = 'SUB'
                          ELSE
                          FASNH = (YNH-Y2)/ABS(YNORM-YC)
                          ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     SPECIAL CONDITION FOR SUMP ORIFICES
C=======================================================================
C      IF(NKLASS(N).EQ.10.AND.YNL.LE.0.96*DEEP(N)) THEN
C              J   = NJUNC(N,2)
C              YNH = HEAD2 - Z(J)
C              IF(YNH.LE.FUDGE) THEN
C                               CALL DEPTHX(N,NKLASS(N),QP,YC,YNORM)
C                               Y2 = AMIN1(YC,YNORM)
C                               KINK  = 'CR2'
C                               ELSE
C                               KINK  = 'SUB'
C                               ENDIF
C              ENDIF
C=======================================================================
C     'NORMAL SITUATION' HALF SURFACE AREA AT EACH END
C=======================================================================
      IF(KINK.EQ.'SUB') THEN
                           YMID = 0.5*(YNL+YNH)
         IF(YMID.LT.FUDGE) YMID = FUDGE
         CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
C#### WCH, 11/16/93.  SERIOUS ERROR.  CHANGE "WIDTH" TO BMID!!
C####         CALL HYDRAD(N,NKLASS(N),YMID,HRAD,AREA,WIDTH)
         CALL HYDRAD(N,NKLASS(N),YMID,HRAD,AREA,BMID)
         CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
         IF(NKLASS(N).LE.9) THEN
                            AS(NL) = AS(NL)+0.25*(BNL+BMID)*LEN(N)
                            AS(NH) = AS(NH)+0.25*(BMID+BNH)*LEN(N)*FASNH
                            ENDIF
         IF(NKLASS(N).EQ.10)AS(NL) = AS(NL) + BMID*LEN(N)
         AREA      = 0.50*(ANL+ANH)
         HRAD      = 0.50*(RNL+RNH)
         WIDTH     = 0.50*(BNL+BNH)
         HRAD      = AMAX1(HRAD,FUDGE)
         WIDTH     = AMAX1(WIDTH,FUDGE)
         VEL       = QP/AREA
         AOVERB(N) = AREA/WIDTH
         RETURN
         ENDIF
C=======================================================================
C     CRITICAL SECTION DOWNSTREAM SURFACE AREA UPSTREAM
C=======================================================================
      IF(KINK.EQ.'CR2') THEN
         YNH   = Y2
         HEAD2 = YNH + ZD(N)
         YMID  = 0.5*(YNL+YNH)
         IF(YMID.LE.FUDGE) YMID = FUDGE
         CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
         CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
         CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
         AS(NL) = AS(NL) + 0.25*(BNL+BMID)*LEN(N)
         ENDIF
C=======================================================================
C     CRITICAL SECTION UPSTREAM SURFACE AREA DOWNSTREAM
C=======================================================================
      IF(KINK.EQ.'CR1') THEN
         HEAD1 = YC + ZU(N)
         YNL   = YC
         YMID  = 0.5*(YNL+YNH)
         IF(YMID.LE.FUDGE) YMID = FUDGE
         CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
         CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
         CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
         AS(NH) = AS(NH) + 0.25*(BMID+BNH)*LEN(N)
         ENDIF
C=======================================================================
C     DRY UPSTREAM SURFACE AREA DOWNSTREAM
C=======================================================================
      IF(KINK.EQ.'DR1') THEN
         HEAD1 = HEAD2
         YNL   = FUDGE
         YMID  = HEAD2-0.5*(ZU(N)+ZD(N))
         IF(YMID.LT.FUDGE) YMID = FUDGE
         CALL HYDRAD(N,NKLASS(N),YNL,RNL,ANL,BNL)
         CALL HYDRAD(N,NKLASS(N),YMID,RMID,AMID,BMID)
         CALL HYDRAD(N,NKLASS(N),YNH,RNH,ANH,BNH)
         AREA      = 0.25*(ANL+2.0*AMID+ANH)
         HRAD      = 0.5*(RMID+RNH)
         VEL       = 0.0
         QO(N)     = 0.0
         IDOIT     = 0
         AOVERB(N) = FUDGE
         AS(NH) = AS(NH) + 0.25*(BMID+BNH)*LEN(N)
         IF(ZU(N)-Z(NL).LT.FUDGE) AS(NL) =
     +                            AS(NL) + 0.25*(BNL+BMID)*LEN(N)
         RETURN
         ENDIF
C=======================================================================
C     COMPUTE CROSS-SECTION AREA, VELOCITY * HYDRAULIC RADIUS
C=======================================================================
      AREA  = 0.25*(ANL+2.0*AMID+ANH)
      HRAD  = 0.25*(RNL+2.0*RMID+RNH)
      WIDTH = 0.25*(BNL+2.0*BMID+BNH)
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
      RETURN
      END
