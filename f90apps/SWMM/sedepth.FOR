      SUBROUTINE SEDEPTH1(N)
      INCLUDE 'TAPES.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'PIPESED.INC'
C	EXTRAN BLOCK
C  FOR CIRCULAR PIPE TYPES
c  This "should" work for all types using ANORM, TWNORM, and RNORM arrays.
c  Compute parameters for depth equal to sediment depth.
c  Following code was taken from HYDRAD.
C      C. MOORE CDM
C=======================================================================
C     INTERPOLATE TABLE OF PROPERTIES
C     USE IRREGULAR CHANNEL DATA IF REQUIRED.
C     OTHERWISE USE ORIGINAL NON-DIMENSIONAL CURVES
C=======================================================================
      FDEPTH = SEDEPTH(N)/DEEP(N)
C=======================================================================
C     CONDUIT IS NOT FULL.  DEPTH/DEEP(N) LE 1.0
C=======================================================================
      IF(FDEPTH - 1.0.LE.0.0) THEN
      I     = 1 + IFIX(FDEPTH*25.0)
      ELSE
c write message and quit if sediment depth is greater than DEEP
      IF (JCE.EQ.0) THEN
      WRITE(N6,6000) N,NCOND(N),SEDEPTH(N),DEEP(N)
 6000 FORMAT(' ERROR - SPECIFIED SEDIMENT DEPTH IS GREATER THAN PIPE',
     A' DEPTH FOR CONDUIT ',I6,I10,/,20X,
     B' SEDIMENT DEPTH = ',F10.3,' FEET',' PIPE DEPTH = ',F10.3,'FEET')
      ELSE
      WRITE(N6,6010) N,ACOND(N),SEDEPTH(N),DEEP(N)
 6010 FORMAT(' ERROR - SPECIFIED SEDIMENT DEPTH IS GREATER THAN PIPE',
     A' DEPTH FOR CONDUIT ',I6,A10,/,20X,
     B' SEDIMENT DEPTH = ',F10.3,' FEET',' PIPE DEPTH = ',F10.3,'FEET')
      ENDIF
      STOP 'PIPE SEDIMENT DEPTH EXCEEDS PIPE DEPTH - SEE OUTPUT'
      ENDIF
C      write(n6,*) 1,N,SEDEPTH(N),DEEP(N),FDEPTH,AFULL(N),WPER,RFULL(N)
      IF(I.GE.26) I = 25
      DELTA = (FDEPTH - 0.04*FLOAT(I-1))*25.0
C  THIS IS TOP WIDTH OF SEDIMENT
                     WIDTH = WIDE(N)*(TWNORM(I,1) +
     1                       (TWNORM(I+1,1)-TWNORM(I,1))*DELTA)
C  THIS IS CROSS-SECTIONAL AREA OF SEDIMENT
                     AREA  = AFULL(N)*(ANORM(I,1) +
     1                      (ANORM(I+1,1)-ANORM(I,1))*DELTA)
                     SEDAREA(N) = AREA
C  THIS IS HRAD OF SEDIMENT
                     HRAD  = RFULL(N)*(HRNORM(I,1) +
     1                      (HRNORM(I+1,1)-HRNORM(I,1))*DELTA)
C  THIS IS WETTED PERIMETER OF SEDIMENT/PIPE CONTACT
                     PERIM = AREA/HRAD
C  THIS IS WETTED PERIMETER OF FULL PIPE SECTION
                     PFULL = AFULL(N)/RFULL(N)
C      WRITE(N6,*) WIDTH, AREA, HRAD, PERIM, PFULL
C  WETTED PERIMETER AJUSTMENT IS PERIM MINUS TOP WIDTH
                     SEDPERI(N) = PERIM - WIDTH
C  ADJUST AFULL BY SUBRACTING AREA OF SEDIMENT
      AFULL(N)  = AFULL(N) - SEDAREA(N)
C  COMPUTE FULL WETTED PERIMETER AFTER AJUSTING FOR
C  SEDIMENT
      WPER = PFULL - SEDPERI(N)
C  THIS IS NOW HYDRAULIC RADIUS OF OPEN PIPE AREA
C  AFTER ADJUSTMENT FOR SEDIMENT
      RF = AFULL(N)/WPER
C  ADJUSTMENT FOR FULL FLOW "CLEAN" PIPE HYDRUALIC RADIUS AND
C  FULL FLOW HYDRAULIC RADIUS ADJUSTED FOR SEDIMENT
      SEDRAD(N) = RFULL(N) - RF
      RFULL(N)  = RF
C  NOTE WIDTH IS NOT ADJUSTED
C  ADJUST DEEP
      DEEP(N) = DEEP(N) - SEDEPTH(N)
C      write(n6,*) 2,N,AFULL(N),WPER,RFULL(N),SEDAREA(N),SEDPERI(N),
C     +            SEDRAD(N)
c  ADJUST Zps of pipe for sediment depth
      ZU(N) = ZU(N) + SEDEPTH(N)
      ZD(N) = ZD(N) + SEDEPTH(N)
      RETURN
      END
