      SUBROUTINE HYDRAD(N,KLASS,DEPTH,HRAD,AREA,WIDTH)
C     EXTRAN BLOCK
C     CALLED BY INDAT1,_ROUTE, and HEAD
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
      INCLUDE 'PIPESED.INC'
      INCLUDE 'FCHAN.INC'
      DATA NUMERR/0/
C=======================================================================
C     CHECK FOR NEGATIVE DEPTH
C=======================================================================
      IF(DEPTH.LT.0.0) THEN
C$OMP CRITICAL
                       IF(NUMERR.LE.99) THEN
                       IF(JCE.EQ.0) WRITE(N6,5000) NCOND(N),DEPTH
                       IF(JCE.EQ.1) WRITE(N6,5001) ACOND(N),DEPTH
                       ENDIF
                       DEPTH  = FUDGE
                       NUMERR = NUMERR + 1
                       IF(NUMERR.EQ.100) WRITE(N6,900)
C$OMP END CRITICAL
                       ENDIF
C=======================================================================
C     SPECIFY NTYPE FOR CIRCULAR ORIFICES - SAME AS CIRCULAR CONDUITS
C=======================================================================
      NTYPE = KLASS
CIM  START OOOOOOOOOOOO
      IF(NTYPE.GE.51) NTYPE = 1
C=======================================================================
C     SPECIFY NTYPE FOR RECTANGULAR ORIFICES - SAME AS RECTANGULAR CONDUITS
C=======================================================================
      IF(NTYPE.GE.53) NTYPE = 21
CIM END  OOOOOOOOOOOO
C=======================================================================
C     RECTANGULAR SECTION (SPECIAL CASE)
C=======================================================================
cim  start <><><><><><>
      IF(NTYPE.EQ.21) THEN
                     WIDTH  = WIDE(N)
CIM CHANGE ORDER OF IF STATEMENTS
CIM     note, this next code changes value of DEPTH passed through
CIM     function call for types 21 only.  Problem??
CIM     Changed to RDEPTH cim 1/96
                     RDEPTH = DEPTH
                     FDEP   = RDEPTH/DEEP(N)
                     IF(RDEPTH.GT.DEEP(N)) RDEPTH = DEEP(N)
                     FDEP2 = RDEPTH/DEEP(N)
cim  by making this change, the results change slightly.  Differences are noted
cim  in the time of peak flows and peak depths in summary routines.   The peak
cim  depths and velocities are not changed. In following line, I've allowed
cim  DEPTH to be modified to have this version replicate other versions of
cim the program.
                     depth = rdepth
cim end <><><><><>
                     AREA   = WIDTH*RDEPTH
                     WETPER = WIDTH + 2.0*RDEPTH
CIM   CHANGE CODE TO ELIMINATE TWO IFs.  FDEP2 CANNOT EXCEED 1.0 (see above)
CIM                  IF(FDEP.GE.1.0)  WETPER = 2.0*WIDTH + 2.0*RDEPTH
CIM                  IF(FDEP.GE.0.90.AND.FDEP.LT.1.0)
                     IF(FDEP2.GE.0.90)
     +                       WETPER = WETPER + (FDEP2-0.90)*WIDTH*10.0
CIM   END CHANGE
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
CIM  START <><><><>
      IF(NTYPE.EQ.22) THEN
CIM  END   <><><><>
                     DEPTT = DEPTH
                     FDEP  = DEPTH-DEEP(N)
                     IF(FDEP.GT.0.0) THEN
                                DEPTT = DEEP(N)
C ******* KEEP SUMMARY OF FULL CHANNELS FOR SUMMARY PRINT OUT
         IFULL(N)=IFULL(N)+1
         IF(NFIRST(N).EQ.0) THEN
            NFIRST(N)=ICYC
            FHOUR(N)=TIME/3600.
         ENDIF
         NLAST(N)=ICYC
         FLHOUR(N)=TIME/3600.
C *****  WRITE STATEMENT NO LONGER USED -- MOVED TO SUMMARY IN OUTPUT
C                                IF(NUMERR.LE.99) THEN
C                                   IF(JCE.EQ.0) WRITE(N6,5010) NCOND(N)
C                                   IF(JCE.EQ.1) WRITE(N6,5011) ACOND(N)
C                                   ENDIF
C                                NUMERR = NUMERR + 1
C                                IF(NUMERR.EQ.100) WRITE(N6,900)
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
C       BRIDGES
C=======================================================================
      IF(NTYPE.EQ.25) THEN
      CALL HBRIDGE(N,DEPTH,HRAD,AREA,WIDTH)
      RETURN
      ENDIF
C=======================================================================
C=======================================================================
C     FOLLOWING SECTION FOR NTYPE = 1, 3, 4, 5 AND 8
Cim           revised types         1  2  3  4 24 and
cim           new types             5  6  7
cim  note that 24 includes parabolic, power function, and irregular
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
CIM
CIM  FOR NOW MAINTAIN TWO SECTIONS FOR PIPES WITH AND WITHOUT
CIM  SEDIMENT,  MAY ELIMINATE IF LATER
CIM  FIRST COMES ORIGINAL CODE WITHOUT SEDIMENT ADJUSTMENT
CIM
      IF (SEDEPTH(N).EQ.0.0) THEN
      I     = 1 + IFIX(FDEPTH*25.0)
      IF(I.GE.26) I = 25
      DELTA = (FDEPTH - 0.04*FLOAT(I-1))*25.0
C=======================================================================
C     NATURAL CROSS SECTION OR POWER FUNCTION CHANNELS ARE NTYPE =24.
C=======================================================================
cim this next statement is valid for closed conduits using revised
cim numbering scheme.  Use normalized curves.
      IF(NTYPE.LE.20) THEN
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
CIM THIS IS SECTION WITH SEDIMENT
      ELSE
      FDEPTH = (DEPTH+SEDEPTH(N))/(DEEP(N)+SEDEPTH(N))
      I     = 1 + IFIX(FDEPTH*25.0)
      IF(I.GE.26) I = 25
      DELTA = (FDEPTH - 0.04*FLOAT(I-1))*25.0
C=======================================================================
C     NATURAL CROSS SECTION OR POWER FUNCTION CHANNELS ARE NTYPE =24.
C=======================================================================
cim this next statement is valid for closed conduits using revised
cim numbering scheme.  Use normalized curves.
      IF(NTYPE.LE.20) THEN
                     WIDTH = WIDE(N)*(TWNORM(I,NTYPE) +
     1                       (TWNORM(I+1,NTYPE)-TWNORM(I,NTYPE))*DELTA)
                     AREA  = (AFULL(N)+SEDAREA(N))*(ANORM(I,NTYPE) +
     1                      (ANORM(I+1,NTYPE)-ANORM(I,NTYPE))*DELTA)
                     HRAD  = (RFULL(N)+SEDRAD(N))*(HRNORM(I,NTYPE) +
     1                      (HRNORM(I+1,NTYPE)-HRNORM(I,NTYPE))*DELTA)
                     PERIM = AREA/HRAD
                     PERIM = AMAX1((PERIM-SEDPERI(N)),0.0)
                     AREA = AMAX1((AREA-SEDAREA(N)),0.0)
                     HRAD = AREA/PERIM
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
      ENDIF
C
C
C
      ELSE
C=======================================================================
C     CONDUIT IS FULL DEPTH/DEEP(N) GT 1.0
C=======================================================================
C     PRINT WARNING MESSAGE FOR NATURAL CROSS SECTIONS OR POWER FUNCTION
C=======================================================================
      IF(NTYPE.EQ.24) THEN
                     DEPTT = DEEP(N)
C ******* KEEP SUMMARY OF FULL CHANNELS FOR SUMMARY PRINT OUT
         IFULL(N)=IFULL(N)+1
         IF(NFIRST(N).EQ.0) THEN
            NFIRST(N)=ICYC
            FHOUR(N)=TIME/3600.
         ENDIF
         NLAST(N)=ICYC
         FLHOUR(N)=TIME/3600.
C *****  WRITE STATEMENT NO LONGER USED -- MOVED TO SUMMARY IN OUTPUT
c                     IF(NUMERR.LE.99) THEN
c                        IF(JCE.EQ.0) WRITE(N6,5010) NCOND(N)
c                        IF(JCE.EQ.1) WRITE(N6,5011) ACOND(N)
c                        ENDIF
c                     NUMERR = NUMERR + 1
c                     IF(NUMERR.EQ.100) WRITE(N6,900)
                     ENDIF
C=======================================================================
CIM
C     PREISSMAN SLOT FOR CONDUITS WITH NTYPE <=20 (closed conduits)
C     VALID ONLY IS JSLOT = 1, OR ITMAX WAS NEGATIVE.
C=======================================================================
                                    SLOT  = 0.005*DEEP(N)
      IF(JSLOT.GT.0.AND.NTYPE.LE.20) THEN
                                    FF    = FDEPTH - 1.0
                     IF(FDEPTH.LT.1.25) THEN
                                        ALF   = TWNORM(26,NTYPE)*WIDE(N)
                                        BETA  = (SLOT  - ALF)/0.0625
                                        WIDTH = ALF + BETA*FF*FF
                                        ELSE
                                        WIDTH = SLOT
                                        ENDIF
                    ELSE
                       WIDTH = 0.0
                    END IF
      AREA  = AFULL(N) + WIDTH*(FDEPTH-1.0)*DEEP(N)
      HRAD  = RFULL(N)
      RETURN
      ENDIF
C=======================================================================
  900 FORMAT(/,' ====> ERRORS IN HYDRAD EXCEED 100.  ERROR PRINTOUT STOP
     +S BUT SIMULATION CONTINUES.',/)
 5000 FORMAT(/,' ====> NEGATIVE DEPTH ENTERED TO HYDRAD, COND.',
     +       I10,1PE16.4)
 5001 FORMAT(/,' ====> NEGATIVE DEPTH ENTERED TO HYDRAD, COND.',
     +       A10,1PE16.4)
 5010 FORMAT(/,' ====> WARNING!  OPEN CHANNEL ',I10,' IS FULL. ',
     +        'USING FULL FLOW PARAMETERS.')
 5011 FORMAT(/,' ====> WARNING!  OPEN CHANNEL ',A10,' IS FULL. ',
     +        'USING FULL FLOW PARAMETERS.')
C=======================================================================
      END
