      SUBROUTINE RDIIREAD(JFLAG,N)
C=======================================================================
C     ROUTINE TO READ INFILTRATION/INFLOW DATA.
C     WRITTEN BY CHUCK MOORE, CDM, 8/93
C     EDITED FOR STYLE BY WCH, 8/93
C     CHANGE 4 OCCURENCES OF IFLAG TO JFLAG AND ADD METRIC VARIABLE
C       BY INCLUDING DETAIL.INC, RED, 11/29/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'RDII.INC'
C#### RED, 11/29/93.
      INCLUDE 'DETAIL.INC'
C=======================================================================
C  THIS IS FIRST CALL TO READ GLOBAL DATA F3 LINE
C======================================================================= 
      IF(JFLAG.EQ.0) THEN
           TSTEP  = 0.0
           TSTEP2 = 0.0
C=======================================================================
C>>>>>>>>>>>>>>> READ F3 LINE <<<<<<<<<<<<<<<
C=======================================================================
           READ(N5,*,ERR=888) CC
           BACKSPACE(N5)
           IF(CC.EQ.'F3') THEN
                READ(N5,*,ERR=888) CC,IIRDII,TSTEP,TSTEP2
                IRATIOS = NINT(TSTEP/TSTEP2+0.001)
                IF(AMOD(TSTEP,TSTEP2).NE.0.0) THEN 
                      IF(IRATIOS.LT.1) IRATIOS = 1
                      TSTEP2 = TSTEP/FLOAT(IRATIOS)
                      WRITE(N6,6000) TSTEP2
                      ENDIF
                ENDIF
           RETURN
           ENDIF 
C=======================================================================
C  THIS IS SECOND CALL TO READ GLOBAL DATA
C ======================================================================
      IF(JFLAG.EQ.1) THEN
           DO 50 J=1,NTK
           NRDHYET(J) = 0
           DO 50 I = 1,3
           RDIIT(J,I)=0.0
   50      RDIIK(J,I)=0.0
           NRDII = 0
C=======================================================================
C>>>>>>>>>>>>>>> READ F4 LINE <<<<<<<<<<<<<<<
C=======================================================================
  100      READ(N5,*,ERR=888) CC
           BACKSPACE(N5)
           IF(CC.EQ.'F4') THEN
                IF(TSTEP.EQ.0.0.OR.TSTEP2.EQ.0.0) THEN 
                     WRITE(N6,*) ' ERROR: F4 CARD FOUND BUT TSTEP',
     +                 ' SPECIFIED ON F3 CARD EQUALS 0.0'
                     ENDIF
                NRDII = NRDII + 1
                IF(NRDII.GT.NTK) THEN
                     WRITE(N6,*) ' ERROR - ATTEMPT TO READ MORE THAN ',
     +                  NTK,' F4 DATA LINES'
                     STOP
                     ENDIF
                READ(N5,*,ERR=888) CC,NRDHYET(NRDII),
     1            (RDIIT(NRDII,I),RDIIK(NRDII,I),
     2            DSTORE(NRDII,I),STORAGE(NRDII,I),DREC(NRDII,I),I=1,3)
                NNTK = NRDII
                GO TO 100
                ENDIF 
                RETURN
           ENDIF
C=======================================================================
C  THIS IS THE THIRD CALL TO READ SUBBASIN DATA
C=======================================================================
      IF(JFLAG.EQ.2) THEN
C=======================================================================
C>>>>>>>>>>>>>>> READ H5 LINE <<<<<<<<<<<<<<<
C=======================================================================
           READ(N5,*,ERR=888) CC
           BACKSPACE N5
           IF(CC.EQ.'H5') THEN
                READ(N5,*,ERR=888) CC,SEWAREA(N),
     +                            (RDIIR(N,I),I=1,3),ICURVE(N)
C=======================================================================
C  CONVERT FROM METRIC, HA TO AC.
C=======================================================================
                IF(METRIC.EQ.2) SEWAREA(N) = SEWAREA(N)*2.471
                IF(SEWAREA(N).EQ.0.0) THEN 
                     ICURVE(N) = 0
                     DO 200 I = 1,3
  200                RDIIR(N,I) = 0.0
                     ENDIF 
                ENDIF
           ENDIF 
C=======================================================================
      RETURN
C=======================================================================
  888 CALL IERROR
C=======================================================================
 6000 FORMAT(' WARNING - INPUT TSTEP2 NOT EQUAL TO INTEGER FRACTION ',
     1'OF RAINFALL TIME STEP AND WAS ADJUSTED TO THE NEAREST INTEGER',
     2' FRACTION'/'  NEW TSTEP2 = ',F10.5)
      END
