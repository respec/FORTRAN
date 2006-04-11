      SUBROUTINE POINTS(J,NGRAF,NOMOS)
C=======================================================================
C     THIS SUBROUTINE IS CALLED BY THE STATS BLOCK.
C     IT IS UTILIZED TO GENERATE (X,Y) PAIRS OF DATA POINTS
C     PRIOR TO CALLING THE CURVE SUBROUTINE.  THESE POINTS ARE
C     PLOTTED ON EITHER THE RETURN PERIOD OR FREQUENCY GRAPH.
C     UPDATED 7/21/93 BY WCH TO INCLUDE PARAMETER A IN RETURN
C       PERIOD CALCULATION.
C=======================================================================
      INCLUDE 'STCOM.INC'
C=======================================================================
C     CALCULATE THE INCREMENT (INC) TO BE USED IN TAKING VALUES FROM
C     THE SORTED SERIES FOR ASSIGNMENT TO AN (X,Y) PAIR ON THE GRAPH.
C=======================================================================
      INC =     NEVNTS/200
      IF(INC*200.NE.NEVNTS) INC = INC + 1
C=======================================================================
C     THE FIRST POINT ASSIGNED CORRESPONDS TO THE FIRST (LARGEST) VALUE
C     IN THE SORTED SERIES.  THE LAST POINT ASSIGNED CORRESPONDS TO AN
C     INTEGER MULTIPLE (PLUS ONE) OF THE INCREMENT.
C=======================================================================
      LAST = NEVNTS
C=======================================================================
C     CALCULATE THE NUMBER OF POINTS ON THE GRAPH.
C=======================================================================
      NPT(1) = LAST/INC
      LL     = 0
      IF(LAST.EQ.NPT(1)*INC) GO TO 50
      NPT(1) = NPT(1) + 1
      LL     = 1
50    CONTINUE
C=======================================================================
C     ASSIGN ORDINATE VALUES
C=======================================================================
      DO 100 I   = 1,LAST,INC
      ILAST      = (LAST + 1 - I)/INC + LL
      Y(ILAST,1) = PARAM(I,J)
100   CONTINUE
C=======================================================================
C     ASSIGN ABSCISSA VALUES FOR EITHER RETURN PERIOD OR FREQUENCY
C     RETURN PERIOD ANALYSIS(THEN)
C     FREQUENCY ANALYSIS(ELSE)
C=======================================================================
      IF(NGRAF.NE.2) THEN
C#### WCH, 7/21/93.  COMPUTE RETURN PERIOD USING PARAMETER A. 
                     THETOP = FLOAT(NOMOS+1)-2.0*A
                     DO 200 I = 1,LAST,INC
                     IN       = (LAST+1-I)/INC + LL
C#### WCH, 7/21/93
                     X(IN,1) = THETOP/(FLOAT(I)-A)
                     X(IN,1) = ALOG10(X(IN,1))
200                  CONTINUE
                     ELSE
                     DO 400 I    = 1,LAST,INC
                     ILAST       = (LAST+1-I)/INC + LL
                     X(ILAST,1) = (1.0-FLOAT(I-1)/FLOAT(NEVNTS))*100.0
400                  CONTINUE
                     ENDIF
      RETURN
      END
