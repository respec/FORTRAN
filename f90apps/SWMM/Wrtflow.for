      SUBROUTINE WRTFLOW
C	EXTRAN BLOCK
C	CALLED BY TRANSX NEAR LINE 951
c  stores flows to sumflw array
c  writes average flow for previous time step to file when required
cim   3/97
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'CWRITE.INC'
      LOGICAL NONZERO
CWRMb save values between subroutine calls
      SAVE IMONTH,INDAY,INYEAR,PTIMDAY,IJULDAY
CWRMe
      IOFLOW = IOFLOW + 1
      DO 100 N=1,NOFLOW+NOFDUP
      L = FLOWOUT(N)
 100  SUMFLW(N) = SUMFLW(N) + Q(L)
cim      write(nfout,*) (q(flowout(n)),n=1,noflow)
CIM
cim  WRITE AND RESET IF DESIRED
cim
      IF (IOFLOW.GE.IFINTER) THEN
c here take absolute values if desired,
      IF (B9ABS) THEN
	DO N = 1,NOFLOW+NOFDUP
	SUMFLW = ABS(SUMFLW)
	ENDDO
	ENDIF
C
      NONZERO = .FALSE.
      DO 150 N=1,NOFLOW
      SUMFLW(N) = SUMFLW(N)/IOFLOW
      IF (SUMFLW(N).LT.FLOWMIN) THEN
      SUMFLW(N) = 0.0
      ELSE
      NONZERO = .TRUE.
      ENDIF
 150  CONTINUE
      IF (NONZERO) THEN
      DO N=1,NOFDUP
      NN = N+NOFLOW
      SUMFLW(NN) = SUMFLW(NN)/IOFLOW
      IF (SUMFLW(NN).LT.FLOWMIN) SUMFLW(NN) = 0.0
      L = FLOWREF(N)
      IF (SUMFLW(L).LT.FLOWMIN) SUMFLW(NN) = 0.0
      ENDDO
      IF (.NOT.WRITING) THEN
c write a zero flow record -FLWSTEP before this time.
       IF(IFLBIN.EQ.0) THEN
       WRITE(NFOUT,7000) IMONTH,INDAY,INYEAR,PTIMDAY/60.0/60.0,
     +                   DRYFLWSTEP,(0.0,N=1,NOFLOW+NOFDUP)
      ELSE

       IF (IJULDAY.NE.0) THEN
       WRITE(NFOUT) IJULDAY,PTIMDAY,DRYFLWSTEP,(0.0,N=1,NOFLOW+NOFDUP)
       ELSE
c      here if start of file, try to compute ijulday and ptimday
       ijulday = julday
       ptimday = timday - flwstep
       if (ptimday.LT.0) THEN
       PTIMDAY = 86400+PTIMDAY
       IJULDAY = IJULDAY - 1
       ICHYEAR = IJULDAY/1000
       ICHDAY  = IJULDAY - 1000*ICHYEAR
       IF (ICHDAY.LE.0) THEN
       ICHYEAR = ICHYEAR - 1
       ICHDAY = 365-ICHDAY
       IF((ICHYEAR/4)*4-ICHYEAR.EQ.0) ICHDAY = 366 - ICHDAY
       IJULDAY = ICHDAY + ICHYEAR*1000
	 ENDIF
       ENDIF
       WRITE(NFOUT) IJULDAY,PTIMDAY,DRYFLWSTEP,(0.0,N=1,NOFLOW+NOFDUP)
      ENDIF

      ENDIF
      DRYFLWSTEP = 0.0
      WRITING = .TRUE.
      ENDIF
      IF(IFLBIN.EQ.0) THEN
       WRITE(NFOUT,7000) MONTH,NDAY,NYEAR,TIMDAY/60.0/60.0,FLWSTEP,
     +                               (SUMFLW(N),N=1,NOFLOW+NOFDUP)
      ELSE
       WRITE(NFOUT) JULDAY,TIMDAY,FLWSTEP,(SUMFLW(N),N=1,NOFLOW+NOFDUP)
      ENDIF
      ELSE IF (WRITING) THEN
C  Write zero flow record at end of positive flow period
      IF(IFLBIN.EQ.0) THEN
       WRITE(NFOUT,7000) MONTH,NDAY,NYEAR,TIMDAY/60.0/60.0,FLWSTEP,
     +                               (0.0,N=1,NOFLOW+NOFDUP)
      ELSE
       WRITE(NFOUT) JULDAY,TIMDAY,FLWSTEP,(0.0,N=1,NOFLOW+NOFDUP)
      ENDIF
      WRITING = .FALSE.
      DRYFLWSTEP = 0.0
      IMONTH=MONTH
      INDAY=NDAY
      INYEAR=NYEAR
      PTIMDAY=TIMDAY
      IJULDAY=JULDAY
      ELSE
C  Save previous time step values
      DRYFLWSTEP = DRYFLWSTEP + FLWSTEP
      IMONTH=MONTH
      INDAY=NDAY
      INYEAR=NYEAR
      PTIMDAY=TIMDAY
      IJULDAY=JULDAY
      ENDIF
      DO 500 N=1,NOFLOW+NOFDUP
 500  SUMFLW(N) = 0.0
      IOFLOW = 0
      ENDIF
cim
      RETURN
 7000 FORMAT(I3,I3,I5,F7.3,F7.0,1PE14.4,200E14.4)
      END
