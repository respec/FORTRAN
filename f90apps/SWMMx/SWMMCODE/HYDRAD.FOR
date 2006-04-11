      SUBROUTINE HYDRAD(N,KLASS,DEPTH,HRAD,AREA,WIDTH)
C=======================================================================
C     THIS SUBROUTINE COMPUTES THE HYDRAULIC RADIUS,
C     SURFACE WIDTH, AND CROSS-SECTIONAL AREA FOR CONDUIT 'N'
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'FLODAT.INC'
      DATA NUMERR/0/
C=======================================================================
C     CHECK FOR NEGATIVE DEPTH
C=======================================================================
      IF(DEPTH.LT.0.0) THEN
                       IF(NUMERR.LE.99) THEN
                       IF(JCE.EQ.0) WRITE(N6,5000) NCOND(N),DEPTH
                       IF(JCE.EQ.1) WRITE(N6,5001) ACOND(N),DEPTH
                       ENDIF
                       DEPTH  = FUDGE
                       NUMERR = NUMERR + 1
                       IF(NUMERR.EQ.100) WRITE(N6,900)
                       ENDIF
C=======================================================================
C     SPECIFY NTYPE FOR ORIFICES - SAME AS CIRCULAR CONDUITS
C=======================================================================
      NTYPE = KLASS
      IF(NTYPE.GE.9) NTYPE = 1
C=======================================================================
C     RECTANGULAR SECTION (SPECIAL CASE)
C=======================================================================
      IF(NTYPE.EQ.2) THEN
                     WIDTH  = WIDE(N)
                     FDEP   = DEPTH/DEEP(N)
                     IF(DEPTH.GT.DEEP(N)) DEPTH = DEEP(N)
                     AREA   = WIDTH*DEPTH
                     WETPER = WIDTH + 2.0*DEPTH
                     IF(FDEP.GE.1.0)  WETPER = 2.0*WIDTH + 2.0*DEPTH
                     IF(FDEP.GE.0.90.AND.FDEP.LT.1.0)
     +                       WETPER = WETPER + (FDEP-0.90)*WIDTH*10.0
                     HRAD  = AREA/WETPER
                     HRAD  = AMAX1(HRAD,FUDGE)
                     AREA  = AMAX1(AREA,FUDGE)
                     IF(JSLOT.GT.0.AND.FDEP.GT.1.0) THEN
                                        SLOT = 0.005*WIDE(N)
                     IF(FDEP.LT.1.25) THEN
                                        ALF   = WIDE(N)
                                        BETA  = (SLOT - ALF)/0.0625
                                        FF    = FDEP  - 1.0
                                        WIDTH = ALF + BETA*FF*FF
                                        ELSE
                                        WIDTH = SLOT
                                        ENDIF
                     AREA  = AFULL(N) + WIDTH*(FDEP-1.0)*DEEP(N)
                     ENDIF
                     RETURN
                     ENDIF
C=======================================================================
C      TRAPEZOIDAL SECTION (SPECIAL CASE)
C=======================================================================
      IF(NTYPE.EQ.6) THEN
                     DEPTT = DEPTH
                     FDEP  = DEPTH-DEEP(N)
                     IF(FDEP.GT.0.0) THEN
                                DEPTT = DEEP(N)
                                IF(NUMERR.LE.99) THEN
                                   IF(JCE.EQ.0) WRITE(N6,5010) NCOND(N)
                                   IF(JCE.EQ.1) WRITE(N6,5011) ACOND(N)
                                   ENDIF
                                NUMERR = NUMERR + 1
                                IF(NUMERR.EQ.100) WRITE(N6,900)
                                ENDIF
                     WIDTH  = WIDE(N)+DEPTT*(STHETA(N)+SPHI(N))
                     AREA   = DEPTT*(WIDE(N)+(DEPTT/2.)*
     1                        (STHETA(N)+SPHI(N)))
                     WETPER = WIDE(N)+DEPTT*(SQRT(1.0+STHETA(N)**2) +
     1                        SQRT(1.+SPHI(N)**2))
                     IF(WETPER.LE.0.0) WETPER = FUDGE
                     HRAD   = AREA/WETPER
                     HRAD   = AMAX1(HRAD,FUDGE)
                     AREA   = AMAX1(AREA,FUDGE)
                     RETURN
                     ENDIF
C=======================================================================
C=======================================================================
C     FOLLOWING SECTION FOR NTYPE = 1, 3, 4, 5 AND 8.
C=======================================================================
C=======================================================================
C     INTERPOLATE TABLE OF PROPERTIES
C     USE IRREGULAR CHANNEL DATA IF REQUIRED.
C     OTHERWISE USE ORIGINAL NON-DIMENSIONAL CURVES
C=======================================================================
      FDEPTH = DEPTH/DEEP(N)
C=======================================================================
C     CONDUIT IS NOT FULL.  DEPTH/DEEP(N) LE 1.0
C=======================================================================
      IF(FDEPTH - 1.0.LE.0.0) THEN
      I     = 1 + IFIX(FDEPTH*25.0)
      IF(I.GE.26) I = 25
      DELTA = (FDEPTH - 0.04*FLOAT(I-1))*25.0
C=======================================================================
C     NATURAL CROSS SECTION OR POWER FUNCTION CHANNELS ARE NTYPE = 8.
C=======================================================================
      IF(NTYPE.LT.8) THEN
                     WIDTH = WIDE(N)*(TWNORM(I,NTYPE) +
     1                       (TWNORM(I+1,NTYPE)-TWNORM(I,NTYPE))*DELTA)
                     AREA  = AFULL(N)*(ANORM(I,NTYPE) +
     1                      (ANORM(I+1,NTYPE)-ANORM(I,NTYPE))*DELTA)
                     HRAD  = RFULL(N)*(HRNORM(I,NTYPE) +
     1                      (HRNORM(I+1,NTYPE)-HRNORM(I,NTYPE))*DELTA)
                     ELSE
                     M     = NQC(N)
                     WIDTH = WIDE(N)*(QCURVE(M,3,I) +
     +                       (QCURVE(M,3,I+1)-QCURVE(M,3,I))*DELTA)
                     AREA  = AFULL(N)*(QCURVE(M,2,I) +
     +                       (QCURVE(M,2,I+1)-QCURVE(M,2,I))*DELTA)
                     HRAD  = RFULL(N)*(QCURVE(M,1,I) +
     +                       (QCURVE(M,1,I+1)-QCURVE(M,1,I))*DELTA)
                     ENDIF
      HRAD  = AMAX1(HRAD,FUDGE)
      AREA  = AMAX1(AREA,FUDGE)
      RETURN
      ELSE
C=======================================================================
C     CONDUIT IS FULL DEPTH/DEEP(N) GT 1.0
C=======================================================================
C     PRINT WARNING MESSAGE FOR NATURAL CROSS SECTIONS OR POWER FUNCTION
C=======================================================================
      IF(NTYPE.EQ.8) THEN
                     DEPTT = DEEP(N)
                     IF(NUMERR.LE.99) THEN
                        IF(JCE.EQ.0) WRITE(N6,5010) NCOND(N)
                        IF(JCE.EQ.1) WRITE(N6,5011) ACOND(N)
                        ENDIF
                     NUMERR = NUMERR + 1
                     IF(NUMERR.EQ.100) WRITE(N6,900)
                     ENDIF
C=======================================================================
C     PREISSMAN SLOT FOR CONDUITS WITH NTYPE = 1, 3 OR 4.
C     VALID ONLY IS JSLOT = 1, OR ITMAX WAS NEGATIVE.
C=======================================================================
                                    SLOT  = 0.005*DEEP(N)
      IF(JSLOT.GT.0.AND.NTYPE.LE.5) THEN
                                    FF    = FDEPTH - 1.0
                     IF(FDEPTH.LT.1.25) THEN
                                        ALF   = TWNORM(26,NTYPE)*WIDE(N)
                                        BETA  = (SLOT  - ALF)/0.0625
                                        WIDTH = ALF + BETA*FF*FF
                                        ELSE
                                        WIDTH = SLOT
                                        ENDIF
                     ENDIF
      IF(JSLOT.EQ.0.OR.NTYPE.EQ.8) WIDTH = 0.0
      AREA  = AFULL(N) + WIDTH*(FDEPTH-1.0)*DEEP(N)
      HRAD  = RFULL(N)
      RETURN
      ENDIF
C=======================================================================
  900 FORMAT(/,' ====> ERRORS IN HYDRAD EXCEED 100.  ERROR PRINTOUT STOP
     +S BUT SIMULATION CONTINUES.',/)
 5000 FORMAT(/,' ====> NEGATIVE DEPTH ENTERED TO HYDRAD, COND.',
     +       I9,1PE16.4)
 5001 FORMAT(/,' ====> NEGATIVE DEPTH ENTERED TO HYDRAD, COND.',
     +       A10,1PE16.4)
 5010 FORMAT(/,' ====> WARNING!  OPEN CHANNEL ',I9,' IS FULL. ',
     +        'USING FULL FLOW PARAMETERS.')
 5011 FORMAT(/,' ====> WARNING!  OPEN CHANNEL ',A10,' IS FULL. ',
     +        'USING FULL FLOW PARAMETERS.')
C=======================================================================
      END
