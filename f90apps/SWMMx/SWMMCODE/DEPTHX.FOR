      SUBROUTINE DEPTHX(N,KLASS,QPO,YC,YNORM)
C=======================================================================
C     THIS SUBROUTINE FINDS THE CRITICAL DEPTH
C     AND THE NORMAL DEPTH CORRESPONDING TO THE FLOW QPO
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'FLODAT.INC'
C=======================================================================
C    NOTE : QC IS CALCULATED FROM FROUDE # = 1
C           IF QC GE QP THEN THE PIPE OR CHANNEL IS SURCHARGED
C=======================================================================
      QP     = ABS(QPO)
      YC     = 0.0
      YNORM  = 0.0
      IF(QP.LE.0.0) RETURN
C=======================================================================
C     SPECIFY NTYPE FOR ORIFICES- SAME AS CIRCULAR CONDUITS
C=======================================================================
                     NTYPE = KLASS
      IF(NTYPE.GE.9) NTYPE = 1
C=======================================================================
C     CALCULATE YC AND YNORM FOR
C     NON-TRAPEZOIDAL AND NON-PARABOLIC CHANNELS
C     SEARCH AREA * WIDTH TABLES FOR PROPER LOCATION
C=======================================================================
      IF(NTYPE.LE.5.OR.NTYPE.EQ.8) THEN
      QCO      = 0.0
      DO 300 I = 2,26
C=======================================================================
C     USE IRREGULAR CHANNEL DATA IF REQUIRED.
C     OTHERWISE USE ORIGINAL NON-DIMENSIONAL CURVES
C=======================================================================
      IF(NTYPE.LT.8) THEN
                     AREA  = AFULL(N) * ANORM(I,NTYPE)
                     WIDTH = WIDE(N)  * TWNORM(I,NTYPE)
                     ELSE
                     M     = NQC(N)
                     AREA  = AFULL(N) * QCURVE(M,2,I)
                     WIDTH = WIDE(N)  * QCURVE(M,3,I)
                     ENDIF
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
      IF(NTYPE.LT.8) THEN
                     AREA = AFULL(N)*ANORM(I,NTYPE)
                     HRAD = RFULL(N)*HRNORM(I,NTYPE)
                     ELSE
                     M    = NQC(N)
                     AREA = AFULL(N)*QCURVE(M,2,I)
                     HRAD = RFULL(N)*QCURVE(M,1,I)
                     ENDIF
C=========================================================================
C     SPECIAL CONSIDERATION FOR NTYPE = 2 --> RECTANGULAR CHANNELS
C=========================================================================
      IF(NTYPE.EQ.2) HRAD = AREA/(WIDE(N) + 2.0*(I-1)/25.*DEEP(N))
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
      ENDIF
C=========================================================================
C     YC FOR TRAPEZOIDAL CHANNELS
C=========================================================================
      IF(NTYPE.EQ.6) THEN
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
      ENDIF
      END
