      FUNCTION PCT(X,IP,PGR,PSIZE,M,NOE,KOE,N)
C=======================================================================
C     FUNCTION TO PERFORM LINEAR INTERPOLATION BETWEEN VALUES OF
C     PARTICLE SIZE - PERCENT GREATER TABLE.
C     CALLED FROM SUB. QUAL.
C     PROGRAMMED BY E.FOUFOULA AND W. HUBER, SEPT. 1981
C=======================================================================
      INCLUDE 'TAPES.INC'
      DIMENSION PGR(5),PSIZE(5)
      CHARACTER KOE*10
C
      IF(X.GT.PSIZE(5).OR.X.LT.0.0) GO TO 100
      DO 50 I = 2,5
      II      = I
      IF (X.LE.PSIZE(I)) GO TO 55
   50 CONTINUE
   55 PCT =PGR(II-1)-(PGR(II-1)-PGR(II))/
     1          (PSIZE(II)-PSIZE(II-1))*(X-PSIZE(II-1))
      RETURN
  100 IF(X.LT.1.05*PSIZE(5).AND.X.GT.PSIZE(5)) THEN
                                     IF(X.LT..0)       PCT = 100.0
                                     IF(X.GT.PSIZE(5)) PCT = 0.0
                                     ENDIF
      IF(JCE.EQ.0) WRITE(N6,900) NOE,M,IP,N,X
      IF(JCE.EQ.1) WRITE(N6,901) KOE,M,IP,N,X
      RETURN
C=======================================================================
  900 FORMAT(/,' ===> WARNING !! FROM FUNCTION PCT.  INTERPOLATION  VARI
     1ABLE, X (MM), OUT OF RANGE. EXT. ELE. NO.=',I8,' INT. ELE. NO.=',
     2I8,/,' POLLUT. NO.=',I2,' TIME STEP=',I5,' X=',F9.5,
     3 ' AS DEFAULT, SET PCT = APPROPRIATE END VALUE (0 OR 100).')
  901 FORMAT(/,' ===> WARNING !! FROM FUNCTION PCT.  INTERPOLATION  VARI
     1ABLE, X (MM), OUT OF RANGE. EXT. ELE. NO.= ',A10,' INT. ELE. NO.='
     2,I8,/,' POLLUT. NO.= ',I2,' TIME STEP= ',I5,' X=',F9.5,
     3 ' AS DEFAULT, SET PCT = APPROPRIATE END VALUE (0 OR 100).')
C=======================================================================
      END
