      SUBROUTINE CURVE(X,Y,NPT,NCV,NPLOT,ALPLOT)
C     GRAPH BLOCK
C=======================================================================
C     THIS IS A MAJOR SUBROUTINE IN GRAPH BLOCK.
C     SCALING IS FIRST ACCOMPLISHED, THEN 'DRAWING' OF CURVES.
C     SUBROUTINE ARGUMENTS ARE:
C     X   = ARRAY OF X VALUES FOR EACH OF NCV CURVES PER GRAPH.
C     Y   = ARRAY OF Y VALUES FOR EACH OF NCV CURVES PER GRAPH.
C     NPT = NUMBER OF X,Y PAIRS FOR EACH CURVE.
C     NCV = NUMBER OF CURVES TO BE OVERPRINTED ON EACH GRAPH.
C
C     X AND Y ARRAYS MUST BE OF LENGTH 201 PER CURVE, BUT ACCORDING
C        TO USUAL FORTRAN RULES, THERE MAY BE MORE THAN 2 CURVES.
C
C     THIS SUBROUTINE IS CALLED FROM SUBROUTINES:
C                               GRAPH  -- EXECUTIVE BLOCK
C                               HCURVE -- RUNOFF BLOCK
C                               OUTPUT -- EXTRAN BLOCK
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'LAB.INC'
      DIMENSION X(201,2),Y(201,2),NPT(2),DUMX(4),DUMY(4)
      CHARACTER*10 ALPLOT
C=======================================================================
C     SET UP X AND Y SCALES
C     INITIALIZE DIMENSIONED VARIABLES TO ZERO.
C=======================================================================
      DATA DUMX/4*0.0/
      DATA DUMY/4*0.0/
C=======================================================================
      DO I=1,NCV
      IF (NPT(I).GT.199) THEN
      WRITE(N6,7001) NPT(I)
 7001 FORMAT(' ATTEMPT TO CALL PLOTTING ROUTINES FOR MORE THAN 199',/,
     a' POINTS.  ONLY FIRST 199 OF THE ',I5,' POINTS ARE PLOTTED.')
      NPT(I) = 199
      END IF
      ENDDO
      XMAX = -1.0E30
      XMIN =  1.0E30
      YMAX = -1.0E30
      YMIN =  1.0E30
      DO 10 K = 1,NCV
      N       = NPT(K)
      DO 10 J = 1,N
      IF( X(J,K).GT.XMAX ) XMAX = X(J,K)
      IF( X(J,K).LT.XMIN ) XMIN = X(J,K)
      IF( Y(J,K).GT.YMAX ) YMAX = Y(J,K)
      IF( Y(J,K).LT.YMIN ) YMIN = Y(J,K)
   10 CONTINUE
      KOKAY   = 0
      DUMX(1) = XMIN
      DUMX(2) = XMAX
      CALL SCALE(DUMX,10.0,2,1,NPLOT,KOKAY,ALPLOT)
      IF(KOKAY.EQ.1) RETURN
C=======================================================================
      KOKAY   = 0
      DUMY(1) = YMIN
      DUMY(2) = YMAX
      CALL SCALE(DUMY,5.0,2,1,NPLOT,KOKAY,ALPLOT)
      IF(KOKAY.EQ.1) RETURN
C=======================================================================
      DO 20 K  = 1, NCV
      N        = NPT(K)
      X(N+1,K) = DUMX(3)
      X(N+2,K) = DUMX(4)
      Y(N+1,K) = DUMY(3)
      Y(N+2,K) = DUMY(4)
   20 CONTINUE
C=======================================================================
C     FORM X LABELS AND FACTORS
C=======================================================================
      XMIN      = DUMX(3)
      DELTX     = DUMX(4)
      XLAB(1)   = XMIN
      DO 30 I   = 1,10
   30 XLAB(I+1) = XLAB(I)+DELTX
      XSCAL     = 100./(XLAB(11)-XMIN)
C=======================================================================
C     FORM Y LABELS AND FACTORS
C=======================================================================
      YMIN      = DUMY(3)
      DELTY     = DUMY(4)
      YLAB(6)   = YMIN
      DO 40 I   = 1,5
   40 YLAB(6-I) = YLAB(7-I)+DELTY
      YSCAL     = 50.0/(YLAB(1)-YMIN)
C=======================================================================
C     INITIALIZE PLOT OUTLINE
C=======================================================================
      NCD = 100
      CALL PPLOT(0,0,NCD,NPLOT,ALPLOT)
      K = 1
C=======================================================================
C     DRAW IN EACH CURVE, JOINING XO-YO AND XT-YT
C=======================================================================
      DO 80 L = 1,NCV
      IF(NPT(L).GT.0) THEN
                      XO      = XSCAL*(X(1,L)-XMIN)
                      YO      = YSCAL*(Y(1,L)-YMIN)
                      NPOINT  = NPT(L)
                      DO 50 N = 2,NPOINT
                      XT = XSCAL*(X(N,L) - XMIN)
                      YT = YSCAL*(Y(N,L) - YMIN)
                      CALL PINE(XO,YO,XT,YT,K,NPLOT,ALPLOT)
                      XO = XT
                      YO = YT
   50                 CONTINUE
                      ENDIF
      K = K + 1
   80 CONTINUE
C=======================================================================
C     OUTPUT FINAL PLOT
C=======================================================================
      NC = 99
      CALL PPLOT(0,0,NC,NPLOT,ALPLOT)
      RETURN
      END
