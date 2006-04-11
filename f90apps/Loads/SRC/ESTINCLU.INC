C*** REVISION 92.07 7/28/92
C===================================================================
C
C       COMMON BLOCK AREAS FOR THE LOADS PROGRAM
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 16, 1988
C		MODIFIED........JULY 28, 1992 (TAC & WGB)
C
C===================================================================
C
C       DEFINITIONS OF COMMON BLOCKS
C
C         BLK1_C            DATA CORRESPONDING TO THE CALIBRATION
C                           DATA SET
C         BLK1_E            DATA CORRESPONDING TO THE ESTIMATION
C                           DATA SET
C         BLK1_R            DATA CORRESPONDING TO THE REGRESSION
C
C===================================================================
C
C       DEFINITIONS OF VARIABLES
C
C         NC        I*4     NUMBER VARIABLES IN XC USED TO CALIBRATE
C                             MODEL
C         NP        I*4     NUMBER OF PARAMETERS (NP=1 ==> ONLY
C                             A CONSTANT IN THE MODEL)
C         X_C(*,NP)  R*8     EXPLANATORY VARIABLES FOR CALIBRATION
C                             XC(1) = 1 ==> A CONSTANT IN MODEL
C         BETA(NP)  R*8     THE COLUMN VECTOR OF ESTIMATED PARAMETERS
C                             B(NP+1) = STND. DEV. RESIDS.
C         NE        I*4     NUMBER OBS. IN XE
C         X_E(*,NP)  R*8     REGRESSORS USED TO ESTIMATE
C
C===================================================================

        IMPLICIT DOUBLE PRECISION (A-H,L-M,O-Z)
        PARAMETER(NSIZE=5000,NPRM=24,NCON=500,N_E=30000,N_EX=1000)

        CHARACTER*100 FNAME
        CHARACTER*20  NAME,DNAME,VNAME

        COMMON /BLK1_I/
     1    NC,NE,NP,NRECS,INDEX,IOFF,NDV,IYC,NPVARS,NCENS,IEASY

C===================================================================
C
C       COMMON BLOCKS RELATED TO CALIBRATION OF MODEL
C
        COMMON /BLK1_C/
     1    IHEAD_C(NSIZE,5)
	 
	 	DIMENSION 
     1	  IY_C(NSIZE),IM_C(NSIZE),ID_C(NSIZE),IT_C(NSIZE)

		EQUIVALENCE
     1				(IHEAD_C(1,1),ISTAT_C),
     2				(IHEAD_C(1,2),IY_C  ),
     3				(IHEAD_C(1,3),IM_C  ),
     4				(IHEAD_C(1,4),ID_C   ),
     5				(IHEAD_C(1,5),IT_C   )

        COMMON /BLK2_C/
     1    FLOW_C(NSIZE),LOGQ_C(NSIZE),D_TIME_C(NSIZE),LQCENT,DTCENT

        COMMON /BLK3_C/
     1    DATA_C(NSIZE),THRESH_C(NSIZE),CENTER(NPRM)

        COMMON /BLK4_C/
     1    X_C(NSIZE,NPRM),Y_C(NSIZE),YT_C(NSIZE)

        COMMON /BLK5_C/
     1    NAME(0:NPRM),DNAME(NCON),VNAME(200)

        COMMON /BLK6_C/
     1    IPVARS(NPRM),XDATA(NSIZE,NPRM),THRESH(NSIZE,NPRM)

        COMMON /BLK7_C/
     1    C_Y_C(NSIZE),C_YT_C(NSIZE)

C===================================================================
C
C       COMMON BLOCKS RELATED TO REGRESSION
C
        COMMON /BLK1_R/
     1    BETA(NPRM),XTX_1(NPRM,NPRM),RES(NSIZE),PRED(NSIZE)

        COMMON /BLK2_R/
     1    XVAR(200),IVAR(200)

        COMMON /BLK3_R/
     1    STDDEV(NPRM),P_V(NPRM)

        COMMON /BLK4_R/
     1    Bz(NPRM),STDz(NPRM),Pz(NPRM),Rz(NSIZE),PRz(NSIZE)

C===================================================================
C
C       COMMON BLOCKS RELATED TO ESTIMATION
C
        COMMON /BLK1_E/IHEAD_E(N_E,5)

	 	DIMENSION 
     1	  IY_E(N_E),IM_E(N_E),ID_E(N_E)
	 
		EQUIVALENCE
     1				(IHEAD_E(1,1),ISTAT_E),
     2				(IHEAD_E(1,2),IY_E   ),
     3				(IHEAD_E(1,3),IM_E   ),
     4				(IHEAD_E(1,4),ID_E   )

        COMMON /BLK2_E/
     1    FLOW_E(N_E),D_TIME_E(N_E)

        COMMON /BLK3_E/
     1    X_E(N_EX,NPRM)

        COMMON /BLK4_E/
     1    Y_E(N_EX),MVUE(N_EX),V(N_EX)
