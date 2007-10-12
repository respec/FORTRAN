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
     1          P_FIT(3),IYEAR(NSIZE),PR(23),CMOMS(3),
     2          THL(1),THU(1),NOBS(1),EPS(1)
	CHARACTER*256 FNAME
	
      LOGICAL      LFLAG

        COMMON /PERF01/ITERS,IFAIL,DISF

	DATA PR/.0001,.0002,.0005,.001,.002,.005,.01,.02,.05,.10,.20,.5,
     1          .8,.9,.95,.98,.99,.995,.998,.999,.9995,.9998,.9999/

C     avoid some lahey math errors
      LFLAG = .TRUE.
      CALL INVALOP (LFLAG)
      CALL UNDFL (LFLAG)
      CALL OVEFL (LFLAG)
      
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
	
      NO = 0
      IFIRST = 0
	DO 15 I=1,NSIZE
	  READ(11,*,END=99) IYEAR(I),NCT,QL(I),QU(I)
	  IF( NCT .EQ. 0 ) IFIRST = 1
	  IF( IFIRST .EQ. 0 ) THEN
	    WRITE(*,'(I8,I8,2F10.1)') IYEAR(I),NCT,QL(I),QU(I)
	  ENDIF
	  DO 10 J=1,NCT
	    NO = NO + 1
          TL(NO) = LOG(MAX(QL(I),1.D-99))
          TU(NO) = LOG(MAX(QU(I),1.D-99))
 10     CONTINUE
 15   CONTINUE

 99   CONTINUE
	CLOSE(11)
cprh	NO = NOBS

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

        CALL P2M(P_FIT,CMOMS)
	
	  WRITE(*,*) ' TOTAL RECORD LENGTH:',NO
	  WRITE(*,*) ' '
	IF(IFAIL .NE. 0) THEN
	  WRITE(*,*) ' FAILURE TO CONVERGE'
	  STOP
	ENDIF
	  WRITE(*,*) ' EMA ITERATIONS:      ',ITERS
	  WRITE(*,*) ' CONVERGENCE CRIT D2: ',DISF
	  WRITE(*,*) ' '
	  
cprh	XMOM(1) = P_FIT(1)+P_FIT(2)*P_FIT(3)
cprh	XMOM(2) = P_FIT(2)*P_FIT(3)**2
cprh	XMOM(3) = DSIGN(1.D0,P_FIT(3))*2.D0/SQRT(P_FIT(2))
      THL(1) =-9.0D99
      THU(1) = 9.0D99
      NOBS(1)= 44
      NT = 1
      REGMSE = 9000.
      NEPS = 1
      EPS(1) = 0.95
        
        WRITE(*,'(/,3X,''PARMS'',T14,1P,3D15.6)') (P_FIT(K),K=1,3)
cprh        WRITE(*,'(/,3X,''MOMENTS'',T14,1P,3D15.6,//)') (XMOM(K),K=1,3)
        WRITE(*,'(/,3X,''C MOMENTS'',T14,1P,3D15.6,//)')(CMOMS(K),K=1,3)

	WRITE(*,'(3X,''P[<Q]'',T19,''T'',T31,''LOG-Q'',T43,''Q'','//
     1                      'T55,''CI LOW'',T67,''CI HIGH'')')
CPRH 2000 FORMAT(3X,'P[<Q]',T19,'T',T31,'LOG-Q',T43,'Q',
CPRH     1                  T55,'CI LOW',T67'CI HIGH'')')

	WRITE(*,'(78(''-''))')
CTMP	DO 20 I=1,23
       write(*,*) 'calling var_p_r_ci' 
       write(*,*) '  nt ',nt
       write(*,*) '  thl ',thl
       write(*,*) '  thu ',thu
       write(*,*) '  nobs ',nobs
       write(*,*) '  p_fit ',p_fit
       write(*,*) '  pr ',pr(17)
       write(*,*) '  regmse ',regmse     
        CALL VAR_P_R_CI(NT,THL,THU,NOBS,P_FIT,PR(17),REGMSE,
     O                  QL99,SYP,SSYP,CVYPSYP)
       write(*,*) 'calling ci_ema_m03'
        CALL CI_EMA_M03(YP,SYP,SSYP,CVYPSYP,NEPS,EPS,CIL,CIH)

ctmp        T  = 1.D0/(1.D0-PR(I))
cprh         QL99        =   FP_G3_ICDF(PR(I),P_FIT) 
        WRITE(*,'(3X,0P,F8.5,F12.4,4X,F12.8,3F12.1)') PR(I),T,
     1              QL99,EXP(QL99),EXP(CIL),EXP(CIH)
CTMP 20	CONTINUE
	READ(*,*)
	GOTO 1
	
	END
	
