      SUBROUTINE RDIIREAD(JFLAG,N)
C     RUNOFF BLOCK
C     CALLED BY RHYDRO1, CATCH
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
CIM 12 VALUES AND INTIALIZE ALL VALUES           
           DO 50 JJ = 1,12
           DO 50 I = 1,3
           RDIIT(J,I,JJ)=0.0
           RDIIK(J,I,JJ)=0.0
           DSTORE(J,I,JJ) = 0.0
           STORAGE(J,I,JJ) = 0.0
           DREC(J,I,JJ) = 0.0
  50       CONTINUE
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
     1            (RDIIT(NRDII,I,1),RDIIK(NRDII,I,1),
     2            DSTORE(NRDII,I,1),STORAGE(NRDII,I,1),
     3            DREC(NRDII,I,1),I=1,3)
                NNTK = NRDII
cim monthly input of values
                IF (NRDHYET(NRDII).LT.0) THEN
                NRDHYET(NRDII)=-NRDHYET(NRDII)
                DO 101 JJ=2,12
 101            READ(N5,*,ERR=888) CC,
     1            (RDIIT(NRDII,I,JJ),RDIIK(NRDII,I,JJ),
     2            DSTORE(NRDII,I,JJ),STORAGE(NRDII,I,JJ),
     3            DREC(NRDII,I,JJ),I=1,3)
                ELSE                
                DO 102 I=1,3
                do 102 jj=2,12
                RDIIT(NRDII,I,JJ) = RDIIT(NRDII,I,1)
                RDIIK(NRDII,I,JJ) = RDIIK(NRDII,I,1)
                DSTORE(NRDII,I,JJ) = DSTORE(NRDII,I,1)
                STORAGE(NRDII,I,JJ) = STORAGE(NRDII,I,1)
                DREC(NRDII,I,JJ) = DREC(NRDII,I,1)
  102           CONTINUE
                ENDIF
CIM MONTHLY 
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
     +                            (RDIIR(N,I,1),I=1,3),ICURVE(N)
C=======================================================================
C  CONVERT FROM METRIC, HA TO AC.
C=======================================================================
                IF(METRIC.EQ.2) SEWAREA(N) = SEWAREA(N)*2.471
                IF(SEWAREA(N).EQ.0.0) THEN 
                     ICURVE(N) = 0
                     DO 200 I = 1,3
  200                RDIIR(N,I,1) = 0.0
                     ENDIF 
                IF (SEWAREA(N).LT.0.0) THEN 
                SEWAREA(N) = -SEWAREA(N)
                DO 201 JJ=2,12
  201           READ(N5,*,ERR=888) CC,(RDIIR(N,I,JJ),I=1,3)
                ELSE
                DO 202 JJ=2,12
                DO 202 I=1,3
  202           RDIIR(N,I,JJ) = RDIIR(N,I,1)
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