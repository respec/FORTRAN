      SUBROUTINE DEPTHX(N,KLASS,QPO,YC,YNORM)
C	EXTRAN BLOCK
C=======================================================================
C     THIS SUBROUTINE FINDS THE CRITICAL DEPTH
C     AND THE NORMAL DEPTH CORRESPONDING TO THE FLOW QPO
CIM Numerous changes to clean up "if" statements in code and to
CIM incorporate new pipe types
      INCLUDE 'TAPES.INC'
C=======================================================================
C=======================================================================
C    NOTE : QC IS CALCULATED FROM FROUDE # = 1
C           IF QC GE QP THEN THE PIPE OR CHANNEL IS SURCHARGED
C=======================================================================
      QP     = ABS(QPO)
      YC     = 0.0
      YNORM  = 0.0
      IF(QP.LE.0.0) RETURN
C=======================================================================
C     SPECIFY NTYPE FOR CIRCULAR ORIFICES- SAME AS CIRCULAR CONDUITS
C=======================================================================
                     NTYPE = KLASS
CIM START
      IF(NTYPE.GE.51) NTYPE = 1
CIM=======================================================================
CIM     SPECIFY NTYPE FOR RECTANGULAR ORIFICES- SAME AS RECTANGULAR CONDUITS
CIM=======================================================================
      IF(NTYPE.GE.53) NTYPE = 21
      Select Case (NTYPE)
      CASE (1:20)
      CALL DEPTHXCL(N,NTYPE,QP,YC,YNORM)
      case (21)
      CALL DEPTHX21(N,QP,YC,YNORM)
      CASE (22)
      CALL DEPTHX22(N,QP,YC,YNORM)
      CASE (24)
      CALL DEPTHX24(N,QP,YC,YNORM)
      CASE (25)
      CALL DEPTHX25(N,QP,YC,YNORM)
      CASE (0)
cim for some reason was calling for wiers ???
      CASE DEFAULT
      WRITE(n6,*) 'ERROR - INCORRECT NTYPE IN DEPTHX'
      WRITE(N6,*) '        NTYPE = ',NTYPE
      STOP 'ERROR - INCORRECT NTYPE IN DEPTHX'
      END SELECT
      RETURN
      END


      SUBROUTINE DEPTHXCL(N,NTYPE,QP,YC,YNORM)
CIM
CIM  close conduits that use ANORM,TWNORM arrays
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
      include 'BD.INC'
      INCLUDE 'PIPESED.INC'
C=======================================================================
C     CALCULATE YC AND YNORM FOR CLOSED CONDUITS  NTYPE .le.20
C     SEARCH AREA * WIDTH TABLES FOR PROPER LOCATION
C=======================================================================
C
CIM  CHECK FOR NON-ZERO SEDIMENT DEPTH
CIM
      IF (SEDEPTH(N).NE.0.0) THEN
      CALL DXCLD(N,NTYPE,QP,YC,YNORM)
      RETURN
      ENDIF
      QCO      = 0.0
      DO 300 I = 2,26
C=======================================================================
C     USE NON-DIMENSIONAL CURVES
C=======================================================================
                     AREA  = AFULL(N) * ANORM(I,NTYPE)
                     WIDTH = WIDE(N)  * TWNORM(I,NTYPE)
      QC = AREA*SQRT(GRVT*AREA/WIDTH)
      IF(QC-QP.GE.0.0) THEN
                       DELTA = (QP-QCO)/(QC-QCO)
                       YC    = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                       GO TO 400
                       ELSE
                       QCO = QC
                       ENDIF
  300 CONTINUE
      YC = DEEP(N)
C=========================================================================
C     SEARCH AREA * RADIUS TABLES FOR PROPER LOCATION
C=========================================================================
 400  QNORMO  = 0.0
      DO 600 I= 2,26
CIM        USE NON-DIMENSIONAL CURVES
                     AREA = AFULL(N)*ANORM(I,NTYPE)
                     HRAD = RFULL(N)*HRNORM(I,NTYPE)
C=========================================================================
C     QNORM IS BASED ON MANNING'S FORMULA
C=========================================================================
      QNORM               = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*
     +                      ROUGH(N)))*AREA*HRAD**0.6667
      IF(QNORM-QP.GE.0.0) THEN
                          DELTA = (QP-QNORMO)/(QNORM-QNORMO)
                          YNORM = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  600 CONTINUE
      YNORM = DEEP(N)
      RETURN
      END


      SUBROUTINE DXCLD(N,NTYPE,QP,YC,YNORM)
CIM
CIM  close conduits that use ANORM,TWNORM arrays
CIM  that have a non zero sediment depth
CIM
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
      include 'BD.INC'
      INCLUDE 'PIPESED.INC'
C=======================================================================
C     CALCULATE YC AND YNORM FOR CLOSED CONDUITS  NTYPE .le.20
C     SEARCH AREA * WIDTH TABLES FOR PROPER LOCATION
C=======================================================================
      QCO      = 0.0
      FDEPTH = SEDEPTH(N)/(SEDEPTH(N)+DEEP(N))
C  FIRST POINT TO CHECK NEEDS TO BE FIRST DEPTH GREATER THAN
C  SEDIMENT DEPTH
      IZERO  = 1 + IFIX(FDEPTH*25.0) + 1
      DO 300 I = IZERO,26
C=======================================================================
C     USE NON-DIMENSIONAL CURVES
C=======================================================================
       AREA = (AFULL(N) + SEDAREA(N)) * ANORM(I,NTYPE)
       AREA = AMAX1((AREA-SEDAREA(N)),0.0)
       WIDTH = WIDE(N)  * TWNORM(I,NTYPE)
       QC = AREA*SQRT(GRVT*AREA/WIDTH)
       IF(QC-QP.GE.0.0) THEN
                DELTA = (QP-QCO)/(QC-QCO)
                YC    = 0.04*(FLOAT(I-2)+DELTA)*(DEEP(N)+SEDEPTH(N)) -
     *                  SEDEPTH(N)
                YC    = AMAX1(YC,0.0)
C      IF (N.EQ.8) WRITE(N6,*) N,QP,FDEPTH,IZERO,AREA,WIDTH,QC,YC
                GO TO 400
               ELSE
                QCO = QC
               ENDIF
  300 CONTINUE
      YC = DEEP(N)
C=========================================================================
C     SEARCH AREA * RADIUS TABLES FOR PROPER LOCATION
C=========================================================================
 400  QNORMO  = 0.0
      FDEPTH = SEDEPTH(N)/(SEDEPTH(N)+DEEP(N))
C  FIRST POINT TO CHECK NEEDS TO BE FIRST DEPTH GREATER THAN
C  SEDIMENT DEPTH
      IZERO  = 1 + IFIX(FDEPTH*25.0) + 1
      DO 600 I= IZERO,26
CIM        USE NON-DIMENSIONAL CURVES
         AREA = (AFULL(N)+SEDAREA(N))*ANORM(I,NTYPE)
         HRAD = (RFULL(N)+SEDRAD(N))*HRNORM(I,NTYPE)
         PERIM = AREA/HRAD
CIM  ADJUST FOR SEDIMENT
         AREA = AMAX1((AREA - SEDAREA(N)),0.0)
         PERIM = AMAX1((PERIM - SEDPERI(N)),0.0)
         HRAD = AREA/PERIM
C
C=========================================================================
C     QNORM IS BASED ON MANNING'S FORMULA
C=========================================================================
      QNORM      = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*
     +             ROUGH(N)))*AREA*HRAD**0.6667
      IF(QNORM-QP.GE.0.0) THEN
                 DELTA = (QP-QNORMO)/(QNORM-QNORMO)
                 YNORM = 0.04*(FLOAT(I-2)+DELTA)*(DEEP(N)+SEDEPTH(N)) -
     +                   SEDEPTH(N)
                 YNORM = AMAX1(YNORM,0.0)
C      IF(N.EQ.8) WRITE(N6,*) FDEPTH,IZERO,AREA,HRAD,PERIM,QNORM,YNORM
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  600 CONTINUE
      YNORM = DEEP(N)
      RETURN
      END



      SUBROUTINE DEPTHX21(N,QP,YC,YNORM)
CIM
CIM  RECTANGULAR
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
C=======================================================================
C     CALCULATE YC AND YNORM FOR
C     RECTANGULAR CHANNELS
C=======================================================================
      QCO      = 0.0
      DO 300 I = 2,26
CIM   RECTANGULAR SECTIONS
                     AREA = AFULL(N)*(I-1)/25.0
                     WIDTH = WIDE(N)
      QC = AREA*SQRT(GRVT*AREA/WIDTH)
      IF(QC-QP.GE.0.0) THEN
                       DELTA = (QP-QCO)/(QC-QCO)
                       YC    = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                       GO TO 400
                       ELSE
                       QCO = QC
                       ENDIF
  300 CONTINUE
      YC = DEEP(N)
  400 QNORMO  = 0.0
      DO 600 I= 2,26
CIM RECTANGULAR CONDUITS
                     AREA = AFULL(N)*(I-1)/25.0
                     HRAD = AREA/(WIDE(N)+2.0*(I-1)/25.0*DEEP(N))
C=========================================================================
C     QNORM IS BASED ON MANNING'S FORMULA
C=========================================================================
      QNORM               = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*
     +                      ROUGH(N)))*AREA*HRAD**0.6667
      IF(QNORM-QP.GE.0.0) THEN
                          DELTA = (QP-QNORMO)/(QNORM-QNORMO)
                          YNORM = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  600 CONTINUE
      YNORM = DEEP(N)
      RETURN
      END

      SUBROUTINE DEPTHX22(N,QP,YC,YNORM)
CIM
CIM  TRAPEZOIDS
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
C=========================================================================
C     YC FOR TRAPEZOIDAL CHANNELS
C=========================================================================
      QCO      = 0.0
      DO 660 I = 2,26
      YI       = 0.04*FLOAT(I-1)*DEEP(N)
      WIDTH    = YI*(STHETA(N)+SPHI(N))+WIDE(N)
      AREA     = 0.5*YI*(WIDTH+WIDE(N))
      QC       = AREA*SQRT(GRVT*AREA/WIDTH)
      IF(QC-QP.GE.0.0) THEN
                       DELTA = (QP-QCO)/(QC-QCO)
                       YC    = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                       GO TO 665
                       ELSE
                       QCO = QC
                       ENDIF
  660 CONTINUE
      YC = DEEP(N)
C=========================================================================
C     YNORM FOR TRAPEZOIDAL CHANNELS
C=========================================================================
  665 QNORMO   = 0.0
      SROOTS   = SQRT(1.+STHETA(N)**2)+SQRT(1.+SPHI(N)**2)
      DO 680 I = 2,26
      YI       = 0.04*FLOAT(I-1)*DEEP(N)
      AREA     = YI*(WIDE(N)+YI/2.*(STHETA(N)+SPHI(N)))
      HRAD     = AREA/(WIDE(N)+YI*SROOTS)
      QNORM    = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*ROUGH(N)))
     1           *AREA*HRAD**0.666666667
      IF(QNORM-QP.GE.0.0) THEN
                          DELTA  = (QP-QNORMO)/(QNORM-QNORMO)
                          YNORM  = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  680 CONTINUE
      YNORM = DEEP(N)
      RETURN
      end

      SUBROUTINE DEPTHX24(N,QP,YC,YNORM)
CIM
CIM  IRREGULAR SECTIONS
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
      include 'flodat.inc'
C=======================================================================
C     CALCULATE YC AND YNORM FOR
C     IRREGULAR CHANNELS
C     SEARCH AREA * WIDTH TABLES FOR PROPER LOCATION
C=======================================================================
      M    = NQC(N)
      QCO      = 0.0
      DO 300 I = 2,26
C=======================================================================
C     USE IRREGULAR CHANNEL DATA IF REQUIRED.
C=======================================================================
CIM   THIS IS FOR IRREGULAR, POWER FUNCTION, AND PARABOLIC
                     AREA  = AFULL(N) * QCURVE(M,2,I)
                     WIDTH = WIDE(N)  * QCURVE(M,3,I)
      QC = AREA*SQRT(GRVT*AREA/WIDTH)
      IF(QC-QP.GE.0.0) THEN
                       DELTA = (QP-QCO)/(QC-QCO)
                       YC    = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                       GO TO 400
                       ELSE
                       QCO = QC
                       ENDIF
  300 CONTINUE
      YC = DEEP(N)
C=========================================================================
C     SEARCH AREA * RADIUS TABLES FOR PROPER LOCATION
C=========================================================================
 400  QNORMO  = 0.0
      DO 600 I= 2,26
CIM  POWER FUNCTION, PARABOLIC, AND IRREGULAR
                     AREA = AFULL(N)*QCURVE(M,2,I)
                     HRAD = RFULL(N)*QCURVE(M,1,I)
C=========================================================================
C     QNORM IS BASED ON MANNING'S FORMULA
C=========================================================================
      QNORM               = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*
     +                      ROUGH(N)))*AREA*HRAD**0.6667
      IF(QNORM-QP.GE.0.0) THEN
                          DELTA = (QP-QNORMO)/(QNORM-QNORMO)
                          YNORM = 0.04*(FLOAT(I-2)+DELTA)*DEEP(N)
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  600 CONTINUE
      YNORM = DEEP(N)
      RETURN
      END

      SUBROUTINE DEPTHX25(N,QP,YC,YNORM)
CIM
CIM  BRIDGE SECTION
      INCLUDE 'TAPES.INC'
      include 'CONTR.inc'
      include 'pipe.inc'
      include 'bridges.inc'
C=======================================================================
C     CALCULATE YC AND YNORM FOR BRIDGE SECTION
C     SEARCH AREA * WIDTH TABLES FOR PROPER LOCATION
C=======================================================================
      QCO      = 0.0
      M     = NBRGE(N)
      DELTD = MAXBPNT-1.0
      DO 300 I = 2,MAXBPNT
                     AREA  = BRAREA(M,I)
                     WIDTH = BRAREA(M,I)
      QC = AREA*SQRT(GRVT*AREA/WIDTH)
      IF(QC-QP.GE.0.0) THEN
                       DELTA = (QP-QCO)/(QC-QCO)
                       YC    = DELTD*(FLOAT(I-2)+DELTA)*DEEP(N)
                       GO TO 400
                       ELSE
                       QCO = QC
                       ENDIF
  300 CONTINUE
      YC = DEEP(N)
C=========================================================================
C     SEARCH AREA * RADIUS TABLES FOR PROPER LOCATION
C=========================================================================
 400  QNORMO  = 0.0
      DO 600 I= 2,MAXBPNT
                     AREA = BRAREA(M,I)
                     HRAD = BRHYD(M,I)
C=========================================================================
C     QNORM IS BASED ON MANNING'S FORMULA
C=========================================================================
      QNORM               = SQRT(GRVT*(ZU(N)-ZD(N))/(LEN(N)*
     +                      ROUGH(N)))*AREA*HRAD**0.6667
      IF(QNORM-QP.GE.0.0) THEN
                          DELTA = (QP-QNORMO)/(QNORM-QNORMO)
                          YNORM = DELTD*(FLOAT(I-2)+DELTA)*DEEP(N)
                          RETURN
                          ELSE
                          QNORMO = QNORM
                          ENDIF
  600 CONTINUE
      YNORM = DEEP(N)
      RETURN
      END
