	PROGRAM TESTEMA
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C	READS INPUT DATA FILE CONTAINING FREEFORM INPUT:
C	YEAR, QLOW, QHIGH
C
C     FITS PEARSON TYPE III DISTRIBUTION TO CENSORED DATA USING 
C     AN EM ALGORITHM AND METHOD OF MOMENTS ESTIMATORS
C
C     FITS BINOMIAL-CENSORED DATA
C
C     AUTHOR....TIM COHN
C     DATE......JULY 26, 1998
C     MODIFIED..NOVEMBER 11, 1998 (TAC)
C
	
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (NSIZE=10000)
	DIMENSION TL(NSIZE),TU(NSIZE),QL(NSIZE),QU(NSIZE),
     1       P_FIT(3),IYEAR(NSIZE),PR(15),XMOM(3)
	CHARACTER*256 FNAME
	
      LOGICAL      LFLAG

        COMMON /PERF01/ITERS,IFAIL,DISF

cprh	DATA PR/.0001,.0002,.0005,.001,.002,.005,.01,.02,.05,.10,.20,.5,
cprh     1          .8,.9,.95,.98,.99,.995,.998,.999,.9995,.9998,.9999/
	DATA PR/.005,.01,.05,.10,.20,.3333,.5,.5708,
     1          .8,.9,.96,.98,.99,.995,.998/

C     avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)
      
1	CONTINUE     
	WRITE(*,*) 'C************************************************'
	WRITE(*,*) 'C'
	WRITE(*,*) 'C            TESTEMA VERSION 11/13/98'
	WRITE(*,*) 'C'
	WRITE(*,*) ' '
	
	WRITE(*,*) ' ENTER INPUT FILENAME'
	WRITE(*,*) ' FREE FORMAT: YEAR, NO. REPEATS, QLOW, QHIGH'
	READ(*,'(A256)') FNAME
	
	OPEN(UNIT=11,FILE=FNAME,STATUS='OLD')
	
	    NOBS = 0
	    IFIRST = 0
	DO 10 I=1,NSIZE
	  READ(11,*,END=99) IYEAR(I),NCT,QL(I),QU(I)
	    IF( NCT .EQ. 0 ) IFIRST = 1
	    IF( IFIRST .EQ. 0 ) THEN
	      WRITE(*,'(I8,I8,2F10.1)') IYEAR(I),NCT,QL(I),QU(I)
	    ENDIF
	  DO 10 J=1,NCT
	    NOBS = NOBS + 1
	  TL(NOBS) = LOG(MAX(QL(I),1.D-99))
	  TU(NOBS) = LOG(MAX(QU(I),1.D-99))
10	CONTINUE

99	CONTINUE
	CLOSE(11)
	NO = NOBS

C
	WRITE(*,*) ' '
        WRITE(*,*) ' ENTER THE REGIONAL SKEW AND WEIGHT (2F)'
        WRITE(*,*) ' A VALUE OF ZERO FOR WEIGHT IMPLIES USE ONLY'
        WRITE(*,*) ' AT-SITE SKEW;  A VALUE OF 0.0 MEANS USE ONLY'
        WRITE(*,*) ' AT-SITE SKEW;  A VALUE OF 1 MEANS USE ONLY'
        WRITE(*,*) ' REGIONAL SKEW'

        READ(*,*) RSKEW,RWEIGHT
	  WRITE(*,*) ' '
	  WRITE(*,*) ' REGIONAL SKEWNESS:  ',RSKEW
	  WRITE(*,*) ' WEIGHT ON REG SKEW: ',RWEIGHT
	  WRITE(*,*) ' '
	
        CALL P3_MOM_BIN(RSKEW,RWEIGHT,NO,TL,TU,P_FIT)
	
	  WRITE(*,*) ' TOTAL RECORD LENGTH:',NO
	  WRITE(*,*) ' '
	IF(IFAIL .NE. 0) THEN
	  WRITE(*,*) ' FAILURE TO CONVERGE'
	  STOP
	ENDIF
	  WRITE(*,*) ' EMA ITERATIONS:      ',ITERS
	  WRITE(*,*) ' CONVERGENCE CRIT D2: ',DISF
	  WRITE(*,*) ' '
	  
	XMOM(1) = P_FIT(1)+P_FIT(2)*P_FIT(3)
	XMOM(2) = P_FIT(2)*P_FIT(3)**2
	XMOM(3) = DSIGN(1.D0,P_FIT(3))*2.D0/SQRT(P_FIT(2))
        
        WRITE(*,'(/,3X,''PARMS'',T12,1P,3D15.6)') (P_FIT(K),K=1,3)
        WRITE(*,'(/,3X,''MOMENTS'',T12,1P,3D15.6,//)') (XMOM(K),K=1,3)

	WRITE(*,'(5X,''P[<Q]'',T21,''T'',T33,''LOG-Q'',T46,''Q'')')
	WRITE(*,'(51(''-''))')
	DO 20 I=1,15
	 T  = 1.D0/(1.D0-PR(I))
         QL99        =   FP_G3_ICDF(PR(I),P_FIT) 
        WRITE(*,'(3X,0P,F8.5,F12.4,4X,F12.8,F12.1)') PR(I),T,
     1              QL99,EXP(QL99)
20	CONTINUE
	READ(*,*)
	GOTO 1
	
	END
	
