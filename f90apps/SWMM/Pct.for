      FUNCTION PCT(X,IP,PGR,PSIZE,M,NOE,KOE,N)
C	TRANSPORT BLOCK
C     CALLED BY QUAL NEAR LINES 217, 222, 238
C=======================================================================
C     FUNCTION TO PERFORM LINEAR INTERPOLATION BETWEEN VALUES OF
C     PARTICLE SIZE - PERCENT GREATER TABLE.
C     CALLED FROM SUB. QUAL.
C     PROGRAMMED BY E.FOUFOULA AND W. HUBER, SEPT. 1981
C     WCH, 1/13/95.  LIMIT NO. ERROR MESSAGES; CORRECT ERROR TRAP.
C=======================================================================
      INCLUDE 'TAPES.INC'
      DIMENSION PGR(5),PSIZE(5)
      CHARACTER KOE*10
C=======================================================================
C#### WCH, 1/13/95.  LIMIT NUMBER OF ERROR MESSAGES.
C=======================================================================
      IF(N.EQ.1) THEN
           NUMMEX = 50
           NUMMES = 1           
           ENDIF
C
      IF(X.GT.PSIZE(5).OR.X.LT.0.0) GO TO 100
      DO 50 I = 2,5
      II      = I
      IF (X.LE.PSIZE(I)) GO TO 55
   50 CONTINUE
   55 PCT =PGR(II-1)-(PGR(II-1)-PGR(II))/
     1          (PSIZE(II)-PSIZE(II-1))*(X-PSIZE(II-1))
      RETURN
C=======================================================================
C#### WCH, 1/13/95.  SCREWY IF-STMT.  ELIMINATE.  ALSO, LIMIT NO. 
C     MESSAGES.  
C####  100 IF(X.LT.1.05*PSIZE(5).AND.X.GT.PSIZE(5)) THEN
C=======================================================================
  100 IF(X.LT.0.0)      PCT = 100.0
      IF(X.GT.PSIZE(5)) PCT = 0.0
C####                                     ENDIF
      IF(NUMMES.LE.NUMMEX) THEN
           IF(NUMMES.EQ.1) WRITE(N6,899) NUMMEX
           IF(JCE.EQ.0) WRITE(N6,900) NUMMES,NOE,M,IP,N,X
           IF(JCE.EQ.1) WRITE(N6,901) NUMMES,KOE,M,IP,N,X
           NUMMES = NUMMES + 1 
           ENDIF
      RETURN
C=======================================================================
  899 FORMAT(/,' NOTE!  A MAXIMUM OF',I3,' WARNING MESSAGES WILL BE PRIN
     1TED FROM FUNCTION PCT.')
  900 FORMAT  (/,' WARNING ! NO.',I3,' FROM FUNCTION PCT.  INTERPOLATION
     1 VARIABLE, X (MM), OUT OF RANGE. EXT. ELE. NO.=',I10,' INT. ELE.
     2NO.=',I8,/,' POLLUT. NO.=',I2,' TIME STEP=',I5,' X=',F9.5,
     3 ' AS DEFAULT, SET PCT = APPROPRIATE END VALUE (0 OR 100).')
  901 FORMAT  (/,' WARNING ! NO.',I3,' FROM FUNCTION PCT.  INTERPOLATION
     1 VARIABLE, X (MM), OUT OF RANGE. EXT. ELE. NO.=',A10,' INT. ELE. 
     2NO.=',I8,/,' POLLUT. NO.= ',I2,' TIME STEP= ',I5,' X=',F9.5,
     3 ' AS DEFAULT, SET PCT = APPROPRIATE END VALUE (0 OR 100).')
C=======================================================================
      END
