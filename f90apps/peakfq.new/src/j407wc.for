C
C
C
      SUBROUTINE   WCFAGB
     #                    (PKQ,PKLOG,WRCPP,SYSPP,NPK,IRC,EMAOPT)
C
C     + + + PURPOSE + + +
C     WRC FLOOD FREQ ANALYSIS BULL 17 GUIDELINES EXECUTIVE RTNE.
C
C     FLOOD FREQUENCY CALCULATIONS  AS DESCRIBED IN U.S. WATER RESOURCES
C     COUNCIL HYDROLOGY COMMITTEE BULLETIN 17 (1976), GUIDELINES FOR DETER-
C     MINING FLOOD FLOW FREQUENCY, AND REVISED AS BULL 17-A (6/77).
C                                             AND BULL. 17-B, 1981.
C
C     ALTHOUGH THESE CALCULATIONS ARE BELIEVED TO CONFORM TO THE WRC GUIDE-
C     LINES, THE USER  IS RESPONSIBLE FOR ALL INTERPRETATIONS AND APPLI-
C     CATIONS OF THE RESULTS.
C
C     SEE FORMAT NO 1 FOR CURRENT VERSION NUMBER AND DATE.
C     WCFAGB VERSION 2.0 5/1/77 BY W.KIRBY, U.S.GEOLOGICAL SURVEY,
C      NATIONAL CTR STOP 430, RESTON, VA 22092.  (703) 860-6947.
C     VERSION 2.0 INCORPORATES REVISIONS TO WRC BULL 17 THROUGH 4/4/77.
C                AND PUBLISHED IN BULL 17-A (6/77).
C     REV 5/79 VER 2.2 - MINOR CHANGES IN -DLO,-FCA,--CSA,-CM0 FOR HAND-
C                 LING NONSTANDARD CONDITIONS.  NO CHANGE IN STANDARD
C                 WRC CALCULATIONS.
C     REV 7/20/79 VER 2.2 - INCREASED PRECISION IN COMPUTING SUMS AND
C            SUMH IN -ASP, -DHH, -DLO.
C
C     REV 10/18/79 VER 2.3 - FIX ROUND-OFF IN --CWS.  USE STUTX FOR EXPEC-
C           TED PROB IN --FCX.
C
C     REV 1/81 VER 2.4 BULL 17-B. -- NEW WEIGHTED SKEW USING STD ERRORS
C           OF GENERALIZED AND STATION SKEWS.  (WCFCWS, FCA)
C      -- THREE-POINT FIT PEARSON TYPE III M, S, G AFTER COND PROB ADJ
C      -- COMPUTE WEIGHTED SKEW AFTER COND PROB ADJ, NOT BEFORE.
C      -- AUTOMATIC HIGH OUTLIER TEST AND REVISED LOW OUTLIER TEST,
C           USING GRUBBS-BECK NORMAL OUTLIER TEST.  (WCFDHH, DLO, OUTKGB.)
C
C     REV 4/28/81 WK - WCFDHH,DLO - USE NUMBER OF PEAKS ABOVE FLOOD BASE
C         IN OUTLIER TESTS.  (NOT NSYS/HISTPD)
C          WCFDHH - PRINT HI-OUT TEST CRIT  IF SUPERSEDED BY MIN HIST PK.
C
C     REV 11/5/81 WK - WCFFCA CHECK *ABS-VALUE* OF WRCSKW IN TABLE RANGE
C
C     REV 12/83 - K.FLYNN - REVISIONS FOR PRIME - WCFCM0
C
C     REV 9/88 - AML - for conforming to OSW coding conventions
C
C     USER INTERFACE TO WRC CALCULATIONS --
C     ASSIGN APPROPRIATE VALUES TO GENSKU...HISTPD. IN COMMON WCFCM1.
C     PLACE FLOWS  IN PKQ VECTOR, HISTORIC EVENTS FIRST..
C     CALL WCFAGB (PKQ ... IRC).   (WCFAGB CALLS ALL OTHER  ROUTINES USED.)
C     UPON RETURN, IF IRC .LT. 3, THE RESULTS WILL BE IN COMMONS WCFCM1
C     AND WCFCM2 AND IN VECTORS IN ARG LIST.  OTHERWISE, IF IRC .EQ. 3,
C     THE CALCULATIONS  COULD NOT BE COMPLETED FOR REASONS EXPLAINED IN
C     NUMBERED MESSAGES.  THE CONTENTS OF THE COMMONS AND VECTORS OTHER
C     THAN PKQ WILL BE MEANINGLESS.   THE PKQ VECTOR IS NOT ALTERED BY
C     WCFAGB AND THE PKLOG ELEMENTS ARE IN THE SAME ORDER AS THE PKQS.
C     A SAMPLE MAIN PROGRAM, WCFZSD, IS INCLUDED IN THIS DISTRIBUTION.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPK, IRC, EMAOPT
      REAL      PKQ(NPK), PKLOG(NPK), WRCPP(NPK), SYSPP(NPK)
C
C     + + + ARGUMENT DEFINITION + + +
C     PKQ    - peak flow input vector--first historic, then systematic
C     PKLOG  - peak flow LOG10 work-output vector
C     WRCPP  - WRC prob plot positions systematic and historic PKS.
C     SYSPP  - prob plot positions systematic record peaks
C     NPK    - number of observed peaks--first historic, then systematic
C     IRC    - return code--0, 1, 2, or 3:
C              0 - no error
C              3 - error, calculation aborted
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NSYS1, ISYS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WCFASP, WCFDHH, WCFCSA, WCFAPI, WCFFCA,
     #          WCFFCX, WCFEPP, GBTESTX
C
C     + + + END SPECIFICATIONS + + +
C
      IRC=0
      IF(MSL.GE.3)WRITE(MSG,001)
    1 FORMAT(4X,'WCF001J-FLOOD FREQUENCY, BULLETIN  17-B.',
     $          ' VER 2.6P (12/19/83)',
     $     /,5X,'      -PRELIMINARY MACHINE COMPUTATIONS.  USER IS RE-',
     $     /,5X,'      -SPONSIBLE FOR ASSESSMENT AND INTERPRETATION.  ')
C
      CALL WCFAPI (PKQ,PKLOG,WRCPP,SYSPP,NPK,IRC)
      IF(IRC.GE.3)GOTO95
      ISYS=NHIST+1
      NSYS1=NPK-NHIST
C
      CALL WCFASP (PKLOG(ISYS),SYSPP(ISYS),NSYS1,IRC,EMAOPT)
      IF(IRC.GE.3)GOTO95
C
      IF(WRCASK.GT.EPS2)GOTO50
      IF(WRCASK.GE.EPS1)GOTO30
C
      CALL GBTESTX (PKLOG(ISYS),NSYS1,EMAOPT,
     O              IER)
Ckmf  IF(NLWOUT.GT.0 .AND. IRC.LT.3)CALL WCFCSA (4H17B1, IRC)
      IF(NLWOUT.GT.0 .AND. IRC.LT.3)CALL WCFCSA ('17B1', IRC)
      IF(IRC.GE.3)GOTO95
      CALL WCFDHH (PKLOG,NPK, IRC,EMAOPT)
Ckmf  IF(NHISTN+NHIOUT.GT.0 .AND. IRC.LT.3) CALL WCFCSA (4H17B2,IRC)
      IF(NHISTN+NHIOUT.GT.0 .AND. IRC.LT.3) CALL WCFCSA ('17B2',IRC)
      IF(IRC.GE.3)GOTO95
      GOTO70
C
   30 CONTINUE
      CALL GBTESTX (PKLOG(ISYS),NSYS1,EMAOPT,
     O              IER)
      IF(IRC.GE.3)GOTO95
      CALL WCFDHH (PKLOG,NPK,IRC,EMAOPT)
      IF(IRC.GE.3)GOTO95
Ckmf  IF(NHISTN+NHIOUT+NLWOUT.GT.0) CALL WCFCSA (4H17B3,IRC)
      IF(NHISTN+NHIOUT+NLWOUT.GT.0) CALL WCFCSA ('17B3',IRC)
      IF(IRC.GE.3)GOTO95
      GOTO70
C
   50 CALL WCFDHH (PKLOG,NPK,IRC,EMAOPT)
Ckmf  IF(NHIOUT+NHISTN.GT.0 .AND. IRC.LT.3) CALL WCFCSA (4H17B4,IRC)
      IF(NHIOUT+NHISTN.GT.0 .AND. IRC.LT.3) CALL WCFCSA ('17B4',IRC)
      IF(IRC.GE.3)GOTO95
      CALL GBTESTX (PKLOG(ISYS),NSYS1,EMAOPT,
     O              IER)
Ckmf  IF(NLWOUT.GT.0 .AND. IRC.LT.3) CALL WCFCSA (4H17B5,IRC)
      IF(NLWOUT.GT.0 .AND. IRC.LT.3) CALL WCFCSA ('17B5',IRC)
      IF(IRC.GE.3)GOTO95
C
   70 CONTINUE
C
C     IF(NOPPOS.NE.1) CALL WCFEPP (WRCPP, 4H17B ,NSYS+NHIST)
C     CALL WCFFCA (WRCFC,4H17B,IRC,EMAOPT)
      IF(NOPPOS.NE.1) CALL WCFEPP (WRCPP, '17B ', NSYS+NHIST)
      CALL WCFFCA (WRCFC, '17B ', IRC,EMAOPT)
      IF(IRC.GE.3)GOTO95
C
      CALL WCFFCX (IRC)
      IF(IRC.GE.3)GOTO95
C
      IF(MSL.GE.3 .OR. (IRC.GE.2.AND.MSL.GE.2) ) WRITE(MSG,2)IRC
002   FORMAT(43H    WCF002J-CALCS COMPLETED.  RETURN CODE = ,I3)
      RETURN
C
   95 IF(MSL.GT.0)WRITE(MSG,003)IRC
003   FORMAT(43H ***WCF003E-CALCS ABORTED.    RETURN CODE = ,I3)
      RETURN
      END
C
C
C
      SUBROUTINE   WCFAPI
     #                    (PKQ,PKLOG,WRCPP,SYSPP,NPK,IER)
C
C      + + + PURPOSE + + +
C     ANALYZE PARAMETERS AND INITIALIZE
C     (WRC Bulletin-17 Flood Frequency Analysis)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPK, IER
      REAL      PKQ(NPK), PKLOG(NPK), WRCPP(NPK), SYSPP(NPK)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PKQ    - peak flow input vector--historic first, then systematic
C     PKLOG  - peak flow LOG10 work-output vector
C     WRCPP  - WRC prob plot positions observed PKS
C     SYSPP  - prob plot positions systematic record PKS
C     NPK    - number of observed peaks
C     IER    - error return code 0, 1, 2, or 3:
C              0 - no error
C              3 - error, calculation aborted
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C      + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
Cprh  replace FC with explicit variable names in cwcf2.inc
Cprh      REAL  FC(31,5)
Cprh      COMMON/ WCFCM2 /FC
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, I
      REAL      BIGQ, T, FINIT(30)
C
C     + + + INTRINSICS + + +
      INTRINSIC  SIGN, ALOG10, MAX0, FLOAT, INT
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (FINIT(1),WRCBAS)
C
C     + + + END SPECIFICATIONS + + +
C
Cmeb  BIGQ = 10.**BIGLOG        meb02/92
      BIGQ = 10.**INT(BIGLOG)
Cprh      DO10J=1,5
Cprh      DO10I=1,31
Cprh   10 FC(I,J)=SIGN(BIGLOG,FLOAT(I)-15.5)
Cprh  init common variables in WCFCM2 (include file cwcf2.inc)
      DO 10 I=1,MXINT
        WRCFC(I) = SIGN(BIGLOG,FLOAT(I)-15.5)
        EPFC(I)  = SIGN(BIGLOG,FLOAT(I)-15.5)
        CLIML(I) = SIGN(BIGLOG,FLOAT(I)-15.5)
        CLIMU(I) = SIGN(BIGLOG,FLOAT(I)-15.5)
        SYSRFC(I)= SIGN(BIGLOG,FLOAT(I)-15.5)
 10   CONTINUE
      DO20I=1,30
   20 FINIT(I)=BIGQ
C
      IF(MSL.GE.4) WRITE(MSG,101) GENSKU,IGSOPT,RMSEGS,GAGEB,
     $        QLWOUT,QHIOUT,NHIST,HISTPD
  101 FORMAT(4X,'WCF101L-INPUT PARAMS- GENSKU OPT STD-ERR GAGEB  ',
     $  'QLWOUT  QHIOUT  NHIST HISTPD'/
     $  26X,F6.3,1X,I2,1X,F6.3,1X,F6.1,1X,F7.1,1X,F9.1,1X,I4,1X,F7.1)
C
      IF(NPK.GT.NHIST.AND.NHIST.GE.0)GOTO30
      IER=3
      IF(MSL.GT.0)WRITE(MSG,102)NPK,NHIST
102   FORMAT(44H ***WCF102E-INVALID PEAK COUNTS. NPK,NHIST = ,2I10)
      RETURN
   30 CONTINUE
C
      IF(MSL.LT.4)GOTO40
      IF(NOTRAN.NE.1)WRITE(MSG,103)NPK,PKQ
      IF(NOTRAN.EQ.1)WRITE(MSG,104)NPK,PKLOG
103   FORMAT(50H    WCF103L-INPUT PEAKS,HISTORIC FIRST. TOTAL NO = ,I6/
     $         (14X,5F12.1))
104   FORMAT(50H    WCF104L-INPUT LOG PEAKS,HIST FIRST. TOTAL NO = ,I6/
     $         (9X,10F7.3))
   40 CONTINUE
C
      IF(GENSKU.GE.GSMIN.AND.GENSKU.LE.GSMAX)GOTO50
      IER=MAX0(IER,1)
      IF(MSL.GE.2)WRITE(MSG,107)GENSKU,GSMIN,GSMAX
107   FORMAT(49H   *WCF107I-ACCEPTED GEN SKEW OUTSIDE MAP LIMITS.,3F8.3)
   50 CONTINUE
C
      IF(NOPPOS.EQ.1)GOTO70
      WRCPP(1)=BIGQ
      IF(NOSYS.EQ.1)GOTO70
      J=NHIST+1
      DO60I=1,J
   60 SYSPP(I)=-1.
      SYSPP(J)=BIGQ
   70 CONTINUE
C
      NMISS=0
      IF(NOTRAN.EQ.1)GOTO110
      DO90I=1,NPK
      T=PKQ(I)
      IF(T.LE.0.)GOTO80
      PKLOG(I)=ALOG10(T)
      GOTO90
   80 IF(T.LT.0.)GOTO85
      PKLOG(I)=-(BIGLOG+.001)
      GOTO90
   85 NMISS=NMISS+1
      IF(I.LE.NHIST)IER=13
      IF(T.GT.-BIGLOG-1.)T=-777777.
      PKLOG(I)=T
      IF(NOPPOS.EQ.1)GOTO90
      J=NPK+1-NMISS
      WRCPP(J)=T
      IF(NOSYS.NE.1)SYSPP(I)=T
   90 CONTINUE
C
C     wording of following 2 messages changed by AML 8/93 after
C     exchange of edoc's between Raymond Slade and Bill Kirby 
      IF(NMISS.EQ.0)GOTO100
      IER=MAX0(IER,2)
      IF(MSL.GT.0)WRITE(MSG,109)NMISS
109   FORMAT(  '  **WCF109W-PEAKS WITH MINUS-FLAGGED DISCHARGES ',
     $           'WERE BYPASSED.  ',I6)
      IF(IER.LE.3)GOTO100
      IER=3
      IF(MSL.GT.0)WRITE(MSG,111)
111   FORMAT(  ' ***WCF111E-HISTORIC PEAK HAD MINUS-FLAGGED ',         
     $         ' DISCHARGE')    
  100 CONTINUE
  110 CONTINUE
C
      NSYS=NPK-NHIST-NMISS
      IF(NMISS.GT.0 .AND. MSL.GT.0)WRITE(MSG,113)NSYS
Ckmf  revised format because of problem with Lahey compiler
C 113 FORMAT(56H  **WCF113W-NUMBER OF SYSTEMATIC PEAKS HAS BEEN REDUCED
C    $       ,10HTO NSYS =  ,I4)
  113 FORMAT( '  **WCF113W-NUMBER OF SYSTEMATIC PEAKS HAS BEEN ',
     $        'REDUCED TO NSYS = ',I4)
      IF(NSYS.GT.0)GOTO130
      IER=3
      IF(MSL.GT.0)WRITE(MSG,117)NSYS,NPK,NHIST,NMISS
117   FORMAT(42H ***WCF117E-NO DATA IN SYSTEMATIC RECORD. ,
     $        25H NSYS,NPK,NHIST,NMISS=    / 30X,4I10)
      GOTO140
  130 IF(NSYS.GE.10)GOTO140
      IER=MAX0(IER,2)
      IF(MSL.GT.0)WRITE(MSG,118)NSYS
118   FORMAT( '  **WCF118W-SYSTEMATIC RECORD SHORTER THAN 17B SPEC.',I6)
  140 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   WCFASP
     #                    (SYSLOG,SYSPP,NSYS1,IER,EMAOPT)
C
C     + + + PURPOSE + + +
C     ANALYZE SYSTEMATIC PEAKS
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     REV 7/20/79 WK TO MAKE ADDENDS TO SUMS DOUBLE PRECISION
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NSYS1,IER,EMAOPT
      REAL SYSLOG(NSYS1),SYSPP(NSYS1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SYSLOG - systematic peak logarithms
C     SYSPP  - prob plot positions systematic record PKS
C     NSYS1  - number of systematic peaks
C     IER    - error return code
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NAGB, I
      REAL      T, SYSTAT(8), WSTATS(8)
      DOUBLE PRECISION Z
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (WSTATS(1),WRCBAS),(SYSTAT(1),SYSBAS)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL   WCFCSA, WCFEPP, WCFFCA
C
C     + + + END SPECIFICATIONS + + +
C
      NLWOUT=0
      NHIOUT=0
      NHISTN=0
      HISTPN=NSYS
      SYSMAX=-BIGLOG
      SYSMIN=+BIGLOG
      NAGB=0
      DO10I=1,3
      SUMH(I)=0D0
   10 SUMS(I)=0D0
C
      WRCBAS=-BIGLOG
      IF(GAGEB.GT.0.)WRCBAS=ALOG10(GAGEB)
      SYSBAS=WRCBAS
      DO30I=1,NSYS1
      T=SYSLOG(I)
      IF(T.LE.WRCBAS)GOTO30
      NAGB=NAGB+1
      IF(T.LT.SYSMIN)SYSMIN=T
      IF(T.GT.SYSMAX)SYSMAX=T
      Z = T
      SUMS(1)=SUMS(1)+Z
      SUMS(2)=SUMS(2)+Z**2
      SUMS(3)=SUMS(3)+Z**3
   30 CONTINUE
      NBGB=NSYS-NAGB
C
      IF(MSL.LT.2)GOTO40
      IF (EMAOPT .EQ. 0) THEN
        T=10.**WRCBAS
        IF(NBGB.GT.0)WRITE(MSG,133)NBGB,T
        IF(NBGB.LE.0)WRITE(MSG,134)T
133     FORMAT(56H    WCF133I-SYSTEMATIC PEAKS BELOW GAGE BASE WERE NOTED.
     $         ,I10,1X,F8.1)
134     FORMAT(53H    WCF134I-NO SYSTEMATIC PEAKS WERE BELOW GAGE BASE.,
     $         14X,F8.1)
      END IF
   40 CONTINUE
C
Ckmf  CALL WCFCSA (4HSYS , IER )
      CALL WCFCSA ( 'SYS ', IER )
      IF(IER.GE.3)  RETURN
C
      IF(NOSYS.EQ.1) RETURN
Ckmf  IF(NOPPOS.NE.1) CALL WCFEPP (SYSPP, 4HSYS ,NSYS)
Ckmf  CALL WCFFCA (SYSRFC,4HSYS,IER,EMAOPT)
      IF(NOPPOS.NE.1) CALL WCFEPP (SYSPP, 'SYS ', NSYS)
      CALL WCFFCA (SYSRFC, 'SYS ',IER,EMAOPT)
C     STATISTICS ARE REPORTED BY WCFFCA IF REQUIRED.
      DO 50 I=1,8
   50 SYSTAT(I)=WSTATS(I)
      RETURN
      END
C
C
C
      SUBROUTINE   WCFCSA
     #                    (LABEL,IER)
C
C     + + + PURPOSE + + +
C     CALCULATE STATISTICS OF ABOVE-BASE PEAKS
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     INCLUDING WRC HISTORIC ADJ, WHEN HIST INFO EXISTS.
C     NOTE -- THESE FORMULAS ARE OK EVEN IF NHIOUT.GT.0 AND HISTPN.EQ.NSYS.
C      (I.E., EVEN IF WCFDHH HAS DETECTED HIGH OUTLIERS BUT NO HISTORIC
C      INFO HAS BEEN GIVEN.)  IN THIS CASE, HISTWT = 1.
C
C     REV 5/79 WK TO SIMPLIFY FN CALC AND TO TRAP NEGATIVE VARIANCES.
C
C     + + + HISTORY + + +
C     kmf - nov 9, 2000 - changed label from Hollerith to character
C
C     + + + DUMMY ARGUMENTS + + +
Ckmf  INTEGER   LABEL, IER
      INTEGER   IER
      CHARACTER*4 LABEL
C
C     + + + ARGUMENTS DEFINITIONS + + +
C     LABEL  - input identification label printed in error message,
C              indicates point from which routine was called (used
C              only in error messages)
C     IER    - error return code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      FN, Z, V
      DOUBLE PRECISION S(3),DBLAAV
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT, SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      FN =NSYS-NHIOUT-NBGB-NLWOUT
      IF (FN.LT.3.)GOTO97
      Z=NHISTN+NHIOUT
c       write(*,*) 'WCFCSA: NHISTN,NHIOUT,NSYS',NHISTN,NHIOUT,NSYS
      HISTWT=(HISTPN-Z)/FLOAT(NSYS-NHIOUT)
      FN = Z + HISTWT*FN
      DO50I=1,3
   50 S(I)=SUMS(I)*HISTWT + SUMH(I)
C
c       write(*,*) 'WCFCSA: FN,HISTPN',FN,HISTPN
      WRCPAB=FN/HISTPN
      DBLAAV=S(1)/FN
      WRCAAV=DBLAAV
      V=(S(2)-S(1)**2/FN)/(FN-1.)
c       write(*,*)'WCFCSA: V',V
cprh      IF(V.GE.0.) GO TO 60
      IF(V.GT.0.) GO TO 60
      IER=3
      IF(MSL.GT.0) WRITE(MSG, 143) LABEL, V
143   FORMAT(38H ***WCF143E-NEGATIVE VARIANCE OF LOGS   ,1A4,F10.5)
      RETURN
   60 CONTINUE
      WRCASD=SQRT(V)
c       write(*,*)'WCFCSA: 60, WRCASD,DBLAAV,S',WRCASD,DBLAAV,S
      WRCASK=(S(3)-3.*DBLAAV*S(2) + 2.*DBLAAV**2*S(1))*FN
     $         / ((FN-1.)*(FN-2.)*WRCASD**3)
c       write(*,*)'WCFCSA: WRCASK',WRCASK
      RETURN
C
   97 IER=3
      IF(MSL.GT.0)WRITE(MSG,141) LABEL,
     $                      FN,NSYS,NBGB,NLWOUT,NHIOUT,NHISTN,HISTPN
141   FORMAT(48H ***WCF141E-SAMPLE SIZE TOO SMALL TO CALC STATS.  ,1A6,
     $         F8.1/11X,  37HNSYS,NBGB,NLWOUT,NHIOUT,NHISTN,HISTPN  /
     $         8X,5I6, F10.1)
      RETURN
      END
C
C
C
      SUBROUTINE   WCFCWS
     #                    (IRC)
C
C     + + + PURPOSE + + +
C     CALCULATE WRC WEIGHTED SKEW
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     REV 10/79 WK - REDEFINE 'ROUND' TO HANDLE NEGATIVE X.
C     REV 1/81 WK - BULL 17-B WEIGHTED SKEW CALC BASED ON MEAN SQUARE ERROR
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IRC
C
C     + + + ARGUMENTS DEFINITIONS + + +
C     IRC    - return code, 0, 1, 2, or 3:
C              0 - no error
C              3 - error, calculation aborted
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL   ERL, AG, A, S, VSTA, VGS, T, HOLD, X
C
C     + + + FUNCTIONS + + +
      REAL   ROUND
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10, SIGN, MAX0, AINT, ABS
C
C     + + + END SPECIFICATIONS + + +
C
      ROUND(X) =  SIGN( AINT(.5+ABS(X)/WSKLAT)*WSKLAT, X )
C
      ERL = NSYS
      ERL = ERL + HRECWS*(HISTPN-ERL)
C
C  MSE OF STATION SKEW
      AG = ABS(WRCSKW)
      A = -0.33 + 0.08*AG
      IF(AG.GT.0.90) A = -0.52 + 0.30*AG
      S = 0.94 - 0.26*AG
      IF(AG.GT.1.50) S = 0.55
      VSTA = 10.**(A - S*(ALOG10(ERL)-1.))
C     assign to common variable for printout later
      ASMSEG = VSTA
C
C  MSE OF GEN SKEW
      VGS = RMSEGS**2
      IF(RMSEGS.LE.0.) VGS = RMSDGS**2
C
C  WEIGHT
      GSKWGT = VSTA/(VSTA+VGS)
C
      T = WRCSKW
      WRCSKW = WRCSKW + GSKWGT*(GENSKU-WRCSKW)
      IF(WSKLAT.GT.0.) WRCSKW = ROUND(WRCSKW)
      IF(IGSOPT.EQ.0)RETURN
C
      IRC=MAX0(IRC,1)
      HOLD = WRCSKW
      IF(IGSOPT.GT.0)GOTO10
      GSKWGT=0.
      WRCSKW = T
      GOTO20
   10 GSKWGT=1.
      WRCSKW=GENSKU
   20 CONTINUE
      IF(WSKLAT.GT.0.) WRCSKW = ROUND(WRCSKW)
C
      IF(MSL.GE.2) WRITE(MSG,151) HOLD, WRCSKW, IGSOPT
151   FORMAT( '   *WCF151I-17B WEIGHTED SKEW REPLACED BY USER OPTION.',
     $          2F10.3,I4)
      RETURN
      END
C
C
C
      SUBROUTINE   WCFDHH
     #                    (PKLOG,NDIM,IER,EMAOPT)
C
C     + + + PURPOSE + + +
C     DETECT HISTORIC PEAKS AND HIGH OUTLIERS
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     REV 7/20/79 WK TO MAKE ADDENDS TO SUMS DOUBLE PRECISION
C     REV 1/81 WK - BULL 17-B REVISED OUTLIER TEST
C     REV 4/28/81 WK - TO USE NUMBER OF PEAKS ABOVE FLOOD BASE IN HI-OUT
C       TEST.  ALSO PRINT HI-OUT TEST BEFORE SUPERSEDING WITH MIN HIST PEAK
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDIM, IER, EMAOPT
      REAL      PKLOG(NDIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PKLOG  - peak flow LOG10 work-input vector
C     NDIM   - size of PKLOG array
C     IER    - error return code
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ISYS,J
      REAL      HPMIN,T,U
      DOUBLE PRECISION Z
C
C     + + + FUNCTIONS + + +
      REAL   OUTKGB
C
C     + + + INTRINSICS + + +
      INTRINSIC  MAX0, ALOG10, AMIN1, FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL OUTKGB
C
C     + + + END SPECIFICATIONS + + +
C
C  PRELIM CHECK NO. 153 IS NO LONGER NEEDED WITH NEW BULL 17-B
C    HIGH OUTLIER TEST.  SEE MSG 164 AND 167 FOR BAD HISTORIC INFO.
C
C
C  NOTE HIST PKS
      HPMIN = BIGLOG
      IF(NHIST.LE.0)GOTO40
      NHISTN=NHIST
      DO 30 I=1,NHISTN
      T=PKLOG(I)
      IF(T.LT.HPMIN) HPMIN = T
      Z = T
      SUMH(1)=SUMH(1)+Z
      SUMH(2)=SUMH(2)+Z**2
   30 SUMH(3)=SUMH(3)+Z**3
   40 CONTINUE
C
C  SET HIGH-OUTLIER BASE
      WRCHOT = WRCAAV + WRCASD*OUTKGB(SIGHOT, NSYS-NBGB-NLWOUT )
      IF(QHIOUT.LE.0.)GOTO60
      IF(MSL.LT.2) GO TO 44
      IF (EMAOPT .EQ. 0) THEN
        T = 10.**WRCHOT
        WRITE(MSG,161) QHIOUT, T
161     FORMAT('   *WCF161I-USER HIGH OUTLIER CRITERION REPLACES 17B. ',
     $     F10.1,1X,F10.1 )
      END IF
   44 IER = MAX0(1, IER)
      T=ALOG10(QHIOUT)
      IF(T.LE.HPMIN) GO TO 50
      IER=MAX0(IER,2)
      IF (EMAOPT .EQ. 0) THEN
        U = 10.**HPMIN
        IF(MSL.GT.0)WRITE(MSG,157)QHIOUT,U
157     FORMAT(45H  **WCF157W-USER HIGH-OUTLIER CRIT LOWERED TO,
     $         13H MIN HIST PK.,  2(1X,F9.1))
      END IF
      T = HPMIN
   50 WRCHHB=T
      GO TO 66
   60 CONTINUE
      WRCHHB = WRCHOT
      IF(WRCHHB.LE.HPMIN) GO TO 66
      IF(MSL.LT.2) GO TO 65
      IF (EMAOPT .EQ. 0) THEN
        T = 10.**WRCHOT
        WRITE(MSG,156) T
156     FORMAT(4X,'WCF156I-17B HI-OUTLIER TEST SUPERSEDED BY ',
     $            'MIN HIST PK',1X,F9.1)
      END IF
   65 WRCHHB = HPMIN
   66 CONTINUE
      IF(WRCHHB.GT.WRCBAS)GOTO70
      IER=3
      IF(MSL.LE.0)RETURN
      IF (EMAOPT .EQ. 0) THEN
        U=10.**WRCHHB
        T=10.**WRCBAS
        WRITE(MSG,159)U,T
159     FORMAT(39H ***WCF159E-HIGH-OUT/HIST-PK BASE BELOW ,
     $          19H LOW-OUT/GAGE BASE.,  2(1X,F9.1))
      END IF
      RETURN
   70 CONTINUE
C
C  DETECT HIGH OUTLIERS
      IF(WRCHHB.GT.SYSMAX)GOTO100
      ISYS=NHISTN+1
      DO90I=ISYS,NDIM
      T=PKLOG(I)
      IF(T.LT.WRCHHB)GOTO90
      NHIOUT=NHIOUT+1
      Z = T
      DO 85 J=1,3
      SUMS(J)=SUMS(J)-Z
      SUMH(J)=SUMH(J)+Z
   85 Z = Z*T
   90 CONTINUE
      IF(NHIOUT.GT.0)GOTO100
      IER=3
      WRITE(MSG,197) SYSMAX, WRCHHB
  197 FORMAT(/' ***WCF004*-INTERNAL PROGRAM LOGIC ERROR DHH-197 -- ',
     $     2F10.5)
  100 CONTINUE
C
C  REPORT NO HIGH-HIST
      IF(NHIOUT+NHISTN.GT.0)GOTO110
      IF(MSL.LT.2) GO TO 16301
      IF (EMAOPT .EQ. 0) THEN
        U = 10.**WRCHHB
        WRITE(MSG,163) U
      END IF
16301 CONTINUE
  163 FORMAT('    WCF163I-NO HIGH OUTLIERS OR HISTORIC PEAKS ',
     $       'EXCEEDED HHBASE.  ',F10.1)
      IF(HISTPD.LE.0.) RETURN
      IER=MAX0(IER,2)
      IF(MSL.LE.0) RETURN
      WRITE(MSG,164) HISTPD
164   FORMAT('  **WCF164W-HISTORIC PERIOD IGNORED.  ',F6.1 )
      RETURN
C
C  REPORT HIGH-OUT-HIST COUNTS
  110 CONTINUE
      IF(HISTPD.LE.0) GO TO 210
      HISTPN = HISTPD
      IF (EMAOPT .EQ. 0) THEN
        T=10.**WRCHHB
        IF(MSL.GE.2)WRITE(MSG,165)NHIOUT,NHISTN,T
165     FORMAT(44H    WCF165I-HIGH OUTLIERS AND HISTORIC PEAKS,
     $         14H ABOVE HHBASE., 2(1X,I2),1X,F10.1)
      END IF
      IF(HISTPN.GT.FLOAT(NHISTN+NSYS) .OR. EMAOPT.EQ.1)GOTO120
      IER=3
      IF(MSL.GT.0 .AND. EMAOPT.EQ.0) WRITE(MSG,167) HISTPN,NSYS,NHISTN
167   FORMAT(54H ***WCF167E-HIST PERIOD NO LONGER THAN SYS + HIST PKS.,
     $        F9.1,2I5)
      RETURN
  120 T=AMIN1(300., 5.*FLOAT(NSYS))
      IF(HISTPN.LE.T)GOTO130
      IER=MAX0(IER,1)
      IF(MSL.GE.2)WRITE(MSG,169)HISTPN,T
169   FORMAT(48H   *WCF169I-ACCEPTED HISTORIC PERIOD GTR THAN T.,2F9.1)
  130 I=NHISTN+NHIOUT
      IF(10*I.LE.NSYS .OR. I.LE.1) GO TO 140
      IER=MAX0(IER,2)
      IF(MSL.GT.0 .AND. EMAOPT.EQ.0)WRITE(MSG,171)I,NSYS
171   FORMAT(42H  **WCF171W-NUMBER HI-OUT/HIST PKS EXCEEDS,
     $       18H 10PCT OF SYS PKS. ,  2I7)
  140 CONTINUE
      RETURN
  210 CONTINUE
      IF(NHISTN.LE.0) GO TO 215
      IER = 3
      WRITE(MSG,1601) NHIST, NHISTN, HISTPD
1601  FORMAT(/' ***WCF004*-INTERNAL PROGRAM LOGIC ERROR DHH-1601. ',2I6,
     $         F7.1/)
  215 CONTINUE
      IF(MSL.LT.2) RETURN
      IF (EMAOPT .EQ. 0) THEN
        T = 10.**WRCHHB
        WRITE(MSG,162) NHIOUT, T
162     FORMAT('    WCF162I-SYSTEMATIC PEAKS EXCEEDED HIGH-',
     $      'OUTLIER CRITERION. ',I3,1X,F9.1)
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE   WCFDLO
     #                    (SYSLOG,NSYS1,IER,EMAOPT)
C
C     + + + PURPOSE + + +
C     DETECT LOW OUTLIERS
C     REV 5/79 WK - MOVED WRC CHECK FOR TOO MANY BELOW BASE FROM FCA
C     REV 7/20/79 WK TO MAKE ADDENDS TO SUMS DOUBLE PRECISION
C     REV 1/81 WK - BULL 17-B REVISED OUTLIER TEST
C     REV 4/28/81 WK - TO USE NUMBER OF PEAKS ABOVE GAGE BASE IN LOW-OUT
C         TEST.  USES HIST WT .
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NSYS1,IER,EMAOPT
      REAL      SYSLOG(NSYS1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SYSLOG - systematic peak logarithms (input)
C     NSYS1  - number of systematic peak logarithms
C     IER    - error return code
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NBB,I,NBBMAX
      REAL      ERL,T,TEST
      DOUBLE PRECISION Z
C
C     + + + FUNCTIONS + + +
      REAL   OUTKGB
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10, MAX0, FLOAT, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL  OUTKGB
C
C     + + + END SPECIFICATIONS + + +
C
      ERL = NSYS-NBGB
      T = HISTPN - HISTWT*NBGB
      ERL = ERL + HRECWO*(T-ERL)
      T = OUTKGB(SIGLOT, INT(ERL+.5))
      WRCLOW=WRCAAV-WRCASD*T
      TEST=WRCLOW
      IF(QLWOUT.LE.0.)GOTO10
      TEST=ALOG10(QLWOUT)
      IER=MAX0(IER,1)
      IF(MSL.LT.2)GOTO10
      T=10.**WRCLOW
      WRITE(MSG,191)QLWOUT,T
191   FORMAT(  '   *WCF191I-USER LOW-OUTLIER CRITERION SUPERSEDES 17B.',
     $       4X,F8.1,1X,F8.1)
   10 CONTINUE
C
      IF(TEST.LT.WRCHHB)GOTO20
      IER=3
      IF(MSL.LE.0)     RETURN
      IF (EMAOPT .EQ. 0) THEN
        WRCHHB=10.**WRCHHB
        TEST=10.**TEST
        WRITE(MSG,193)TEST,WRCHHB
193     FORMAT(52H ***WCF193E-LOW-OUTLIER CRITERION EXCEEDS HIGH-HIST.,
     $             2F11.1)
      END IF
      RETURN
C
   20 NLWOUT=0
      IF(TEST.GE.SYSMIN)GOTO30
      IF(MSL.LT.2)  GO TO 25
      TEST=10.**TEST
      WRITE(MSG,195)TEST
195   FORMAT(48H    WCF195I-NO LOW OUTLIERS WERE DETECTED BELOW ,
     $       10HCRITERION.,9X,F8.1)
   25 IF(NBGB.GT.0) GO TO 65
      RETURN
C
   30 WRCBAS=TEST
      DO 40 I=1,NSYS1
      T=SYSLOG(I)
      IF(T.GT.WRCBAS.OR.T.LE.SYSBAS)GOTO40
      Z = T
      SUMS(1)=SUMS(1)-Z
      SUMS(2)=SUMS(2)-Z**2
      SUMS(3)=SUMS(3)-Z**3
      NLWOUT=NLWOUT+1
   40 CONTINUE
      IF(NLWOUT.GT.0)GOTO50
      IER=3
      IF(MSL.GT.0)WRITE(MSG,197)SYSMIN,WRCBAS,SYSBAS
197   FORMAT(/' ***WCF004*-INTERNAL PROGRAM LOGIC ERROR DLO-197. ',
     $       3F10.5/)
      RETURN
C
   50 IF(MSL.LT.2)  GO TO 60
      IF (EMAOPT .EQ. 0) THEN
        T=10.**WRCBAS
        WRITE(MSG,198)NLWOUT,T
198     FORMAT(46H    WCF198I-LOW OUTLIERS BELOW FLOOD BASE WERE,
     $            9H DROPPED.,  I8,4X,F8.1)
      END IF
   60 CONTINUE
C
C  CHECK FOR TOO MANY BELOW-BASE PEAKS
   65 CONTINUE
      NBB = NBGB+NLWOUT
      NBBMAX = 0.25*FLOAT(NSYS)
      IF(NBB.LE.NBBMAX) GO TO 70
      IER=MAX0(IER,2)
      IF(MSL.LE.0) GO TO 70
      IF (EMAOPT .EQ. 0) THEN
        T = 10.**WRCBAS
        WRITE(MSG,199)NBB,T,NBBMAX
199     FORMAT(  '  **WCF199W-NUMBER OF PEAKS BELOW FLOOD BASE',
     $           ' EXCEEDS 17B SPEC.', 1X,I3,1X,F8.1,1X,I3)
      END IF
   70 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   WCFEPP
     #                    (PPV, LABEL, NDIM)
C
C     + + + PURPOSE + + +
C     EMPIRICAL PROBABILITY PLOTTING POSITIONS,  INCL WRC HIST ADJ AS APPRO
C
C     + + + HISTORY + + +
C     kmf - nov 9, 2000 - changed label from Hollerith to character
C
C     + + + DUMMY ARGUMENTS + + +
C     INTEGER   LABEL,NDIM
      INTEGER   NDIM
      REAL      PPV(NDIM)
      CHARACTER*4 LABEL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PPV    - output vector for empirical plotting probabilities
C     LABEL  - identification label for printing message
C     NDIM   - dimension of PPV
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NPK,NZ,I
      REAL       FAC,A
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,MIN0
C
C     + + + END SPECIFICATIONS + + +
C
      NPK=NSYS+NHISTN
      NZ=NHIOUT+NHISTN
      FAC=1./(HISTPN+1.-2.*WEIBA)
      IF(NZ.LE.0)NZ=NPK
      DO20I=1,NZ
   20 PPV(I)=(FLOAT(I)-WEIBA)*FAC
      IF(NZ.GE.NPK) GO TO 70
      A=(HISTWT-1.)*(FLOAT(NZ)+.5)+WEIBA
      NZ=NZ+1
      DO40I=NZ,NPK
   40 PPV(I) = (FLOAT(I)*HISTWT - A)*FAC
   70 IF(MSL.LT.3)GOTO90
      NZ=MIN0(10,NPK)
      WRITE(MSG,203)LABEL,(PPV(I),I=1,NZ)
  203 FORMAT(48H    WCF203J-PLOTTING POSITIONS OF TOP TEN PEAKS. ,
     $         A6/(9X,10F7.4))
   90 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   WCFFCA
     #                    (FCQ,LABEL,IER,EMAOPT)
C
C     + + + PURPOSE + + +
C     FREQUENCY CURVE PEARSON TYPE III ORDINATES,
C     INCL COND PROB ADJST (WRC APPX 4) IF REQUIRED.
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     ALSO INCLUDES WEIGHTED-SKEW CALC IF LABEL.EQ.'WRC'.  (1/81- B-17-B.)
C
C     REV 5/79 WK - MOVED WRC CHECK FOR TOO MANY BELOW BASE TO DLO
C            AFTER COND PROB ADJ.
C     REV 1/81 WK - BULL 17-B -- THREE-POINT FIT PEARSON TYPE III
C      -- COMPUTE WEIGHTED SKEW AFTER COND PROB ADJ (NOT BEFORE) WHEN
C            LABEL.EQ.'WRC'.  (BUT NOT IN SYST-REC CALCS.)
C
C     REV 11/5/81 WK - WCFFCA CHECK *ABS-VALUE* OF WRCSKW IN TABLE RANGE
C
C     + + + HISTORY + + +
C     kmf - Nov 9, 2000, converted LABEL and LCWRC Hollerith
C                        to character
C
C     + + + DUMMY ARGUMENTS + + +
Ckmf  INTEGER LABEL,IER
      INTEGER   IER,EMAOPT
      REAL FCQ(*)
      CHARACTER*4  LABEL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FCQ    - output vector of frequency curve ordinates
C     LABEL  - identifier printed in error message and used to
C              identify systematic-records vs WRC calculations
C     IER    - error return code
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
C
C     + + + LOCAL VARIABLES + + +
Ckmf  INTEGER   I, LCWRC
      INTEGER   I
      REAL      PBB, TAB(3), T
      CHARACTER*4 LCWRC
C
C     + + + FUNCTIONS + + +
      REAL   HARTRG, HARTK
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS,AMAX1
C
C     + + + EXTERNAL + + +
      EXTERNAL HARTIV, HARTRG, HARTK, WCFCWS
C
Ckmf  DATA  LCWRC / 4H17B /
      DATA  LCWRC / '17B ' /
C
C     + + + END SPECIFICATIONS + + +
C
      IF(ABS(WRCASK).LE.9.) GO TO 20
      IER=3
      IF(MSL.GT.0) WRITE(MSG,215) LABEL, WRCASK
215   FORMAT(' ***WCF215E-SKEW OUT OF TABLE RANGE.  ',1A4,3F9.3)
      RETURN
   20 CALL HARTIV(WRCASK, MXINT, TXPROB,
     O            WORK)
c       write(99,2000) wrcask
c 2000  format('WCFFCA: After 20 HARTIV, WRCASK ',F8.5,' WORK:')
c       write(99,2010) work
c 2010  format(8(1X,F8.5))
C
C  CONDITIONAL PROB ADJUSTMT.
      PBB = 1. - WRCPAB
      IF(WRCPAB.GT.0.9999)GOTO40
Cprh  use CPAMP1,CPAMP2,CPAMP3 as declared in include file cwcf0.inc
Cprh  instead of CPAMP(3) array originally declared in local common here
Cprh      IF(WRCPAB.LE.CPAMP(1)) GO TO 30
Cprh      DO 25 I=1,3
Cprh   25 TAB(I) = WRCAAV + WRCASD*HARTK(1.-CPAMP(I)/WRCPAB, WORK)
      IF(WRCPAB.LE.CPAMP1) GO TO 30
c       write(99,2015) WRCPAB,CPAMP1,CPAMP2,CPAMP3
 2015  format('WCFFCA: Before TAB calcs, WRCPAB, CPAMP1-3',4(1x,f8.5))
      TAB(1) = WRCAAV + WRCASD*HARTK(1.-CPAMP1/WRCPAB, WRCASK)
      TAB(2) = WRCAAV + WRCASD*HARTK(1.-CPAMP2/WRCPAB, WRCASK)
      TAB(3) = WRCAAV + WRCASD*HARTK(1.-CPAMP3/WRCPAB, WRCASK)
c       write(99,2020) TAB(1),TAB(2),TAB(3)
 2020  format('WCFFCA: TAB 1-3 ',3(1X,F8.5))
      WRCSKW = HARTRG((TAB(3)-TAB(2))/(TAB(2)-TAB(1)))
      IF(WRCSKW.LE.9.) GO TO 27
      IER = 3
      IF(MSL.GT.0) WRITE(MSG,215)LABEL,WRCASK,WRCSKW
      RETURN
   27 CALL HARTIV(WRCSKW, MXINT, TXPROB,
     O            WORK)
c       write(99,2030) wrcskw
c 2030  format('WCFFCA: After 27 HARTIV, WRCSKW ',F8.5,' WORK:')
c       write(99,2010) work
Cprh  see note above regarding replacing CPAMP(3) array
Cprh      WRCUSD = (TAB(3)-TAB(2))/(HARTK(1.-CPAMP(3),WORK)
Cprh     $         -HARTK(1.-CPAMP(2),WORK))
Cprh      WRCUAV = TAB(1) - WRCUSD*HARTK(1.-CPAMP(1),WORK)
      WRCUSD = (TAB(3)-TAB(2))/(HARTK(1.-CPAMP3,WRCSKW)
     $         -HARTK(1.-CPAMP2,WRCSKW))
      WRCUAV = TAB(1) - WRCUSD*HARTK(1.-CPAMP1,WRCSKW)
c       write(99,2040) wrcusd,wrcuav
 2040  format('WCFFCA: WRCUSD ',F8.5,' WRCUAV ',F8.5)
      GO TO 45
C
   30 CONTINUE
      IER=3
      IF(MSL.LE.0) RETURN
      IF (EMAOPT .EQ. 0) THEN
        WRITE(MSG,213)LABEL,PBB
213     FORMAT(46H ***WCF213E-COND PROB ADJUST FAILED--EXCESSIVE,
     $      1X,1A4, 18H PROB BELOW BASE. ,F8.4)
      END IF
      RETURN
C
   40 CONTINUE
      WRCUAV=WRCAAV
      WRCUSD=WRCASD
      WRCSKW = WRCASK
C
C  WEIGHTED SKEW
  45  CONTINUE
      IF(LABEL.NE.LCWRC) GO TO 50
      T = WRCSKW
      CALL WCFCWS(IER)
      IF(ABS(WRCSKW-T).LE.0.) GO TO 50
      IF(ABS(WRCSKW).LE.9) GO TO 48
      IER = 3
      IF(MSL.GT.0) WRITE(MSG,215) LABEL,WRCASK,WRCSKW,GENSKU
      RETURN
   48 CALL HARTIV(WRCSKW, MXINT, TXPROB,
     O            WORK)
c       write(99,2050) wrcskw
c 2050  format('WCFFCA: After 48 HARTIV, WRCSKW ',F8.5,' WORK:')
c       write(99,2010) work
C
   50 IF(MSL.GE.4)WRITE(MSG,217)LABEL,WRCPAB,WRCUAV,WRCUSD,WRCSKW,
     $                                       WRCAAV,WRCASD,WRCASK
217   FORMAT('    WCF217L-FREQUENCY CURVE PARAMS --',
     $          1X,1A4,4F8.4/(50X,3F8.4))
Cprh  change to parameter MXINT, added CONTINUE to loop
Cprh      DO 55 I=1,31
      DO 55 I=1,MXINT
        WORK(I) = AMAX1(WRCBAS, WRCUAV+WORK(I)*WRCUSD)
   55 CONTINUE
      DO 60 I=INDX1,INDX2
Cprh      IF(TXPROB(I).GT.WRCPAB) GO TO 60
        IF(TXPROB(I).LE.WRCPAB) THEN
          FCQ(I) = WORK(I)
C         *** removed so don't get break in plot, ie don't substitute
C         *** the flood base under this condition        
C         IF(ABS(TXPROB(I)-WRCPAB) .LE. 0.) FCQ(I) = WRCBAS
C         ***
        END IF
   60 CONTINUE
C
Cprh  replace RETURN with IF...END IF block
Cprh      IF(MSL.LT.3)  RETURN
      IF(MSL.GE.3) THEN
Cprh    added continue to 70 loop
        DO 70 I=1,3
          TAB(I) = 10.**WORK(INDXPT(I))
   70   CONTINUE
        WRITE(MSG,219)LABEL,TAB
219     FORMAT(33H    WCF219J-FREQ CURVE ORDINATES ,1A4,4X,
     $          38H  2-YR (.50) 10-YR (.10) 100-YR (.01)   /39X,3F12.1)
      END IF

      RETURN
      END
C
C
C
      SUBROUTINE   WCFFCX
     #                   ( IER )
C
C     + + + PURPOSE + + +
C     FREQ CURVE EXTENSIONS -- CONFIDENCE LIMS AND EXPECTED PROBS
C     (WRC Bulletin-17 Flood Frequency Analysis)
C     REV 10/79 WK - TO USE STUTX FOR T-QUANTILES IN EXPECTED PROB.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IER
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IER    - error return code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,IIK,N233,NDF
      REAL A,ABT,C,EP,ERL,EXPFAC,RAD,SDA,SIGMA,T,TAB(3),TAB2(3),Z,ZC2
C
C     + + + FUNCTIONS + + +
      REAL HARTK, GAUSCF, GAUSAB, STUTX
C
C     + + + INTRINSICS + + +
      INTRINSIC MAX0,SQRT,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  HARTK, GAUSCF, GAUSAB, STUTX
C
C     + + + END SPECIFICATIONS + + +
C
      ERL = NSYS
      ERL = ERL + HRECWX*(HISTPN-ERL)
C
C  EXPECTED PROBABILITY ADJUSTMENT, APPROX FOR SMALL SKEW
      IF(NOEPFC.EQ.1)GOTO70
      EXPFAC = SQRT(1. + 1./ERL)
      NDF = ERL - 1.
      N233 = 0
      DO60IIK=1,NINDX
      I=INDXPT(IIK)
c      write(*,*) 'WCFFCX: 60 Loop-I,TXPROB,WRCPAB',I,TXPROB(I),WRCPAB
      IF(TXPROB(I).GT.WRCPAB) GO TO  60
      T = STUTX(1.-TXPROB(I), NDF)
      IF(ABS(T).LE.1E4) GO TO 30
      IER=3
      IF(MSL.GT.0) WRITE(*,237) NDF,TXPROB(I), T
      IF(MSL.GT.0) WRITE(MSG,237) NDF,TXPROB(I), T
237   FORMAT(/' ***WCF004*-INTERNAL PROGRAM LOGIC ERROR-STUTX. ',
     $        I6,2F10.5)
      RETURN
c      write(*,*) 'WCFFCX: B4 GAUSCF-30'
   30 EP=GAUSCF(T*EXPFAC)
c      write(*,*) 'WCFFCX: B4 HARTK, I,EP,WRCSKW',I,EP,WRCSKW
c       write(99,*) 'WCFFCX: B4 HARTK, I,EP,WRCSKW',I,EP,WRCSKW,' WORK:'
c       write(99,2010) work
c 2010  format(8(1X,F8.5))
      Z = HARTK(EP, WRCSKW)* wrcusd + wrcuav
c       write(*,*) 'WCFFCX: after HARTK, Z(',I,') is ',Z
      IF( Z.LT. BIGLOG) GO TO 50
      Z = BIGLOG
      N233 = N233 + 1
      IF(N233.GT.1) GO TO 50
      IER = MAX0(2, IER)
      EP=1.-EP
      IF(MSL.GT.0) WRITE(*,233) EP, TXPROB(I)
      IF(MSL.GT.0) WRITE(MSG,233) EP, TXPROB(I)
233   FORMAT(51H  **WCF233W-EXPECTED PROB OUT OF RANGE AT TAB PROB.,
     $        2F10.5)
   50 IF(Z.LT.-BIGLOG) Z=-BIGLOG
      EPFC(I)  =  Z
   60 CONTINUE
      IF(MSL.LT.3)GOTO70
      DO65I=1,3
   65 TAB(I)=10.**EPFC(INDXPT(I))
      WRITE(*,238)TAB
      WRITE(MSG,238)TAB
238   FORMAT( '    WCF238J-FREQ CURVE 17B-EXPECT-PROB.',3F12.1)
   70 CONTINUE
C
c      write(*,*) 'WCFFCX: after 70, NOCLIM=',NOCLIM
C  95 PCT CONFIDENCE LIMITS APPROX FOR SMALL SKEW
      IF(NOCLIM.EQ.1)GOTO90
      ZC2=GAUSAB(CLSIZE)**2
      A=1.-ZC2/(2.*(ERL-1.))
      C=1.-A
      ABT = A*ZC2/ERL
      SDA=WRCUSD/A
      SIGMA=WRCUSD
      IF(ABS(SIGMA).LE.0.)SIGMA=1.
Cprh      DO80IIK=1,NINDX
      DO 80 I = INDX1,INDX2
Cprh      I=INDXPT(IIK)
c      write(*,*) 'WCFFCX: 80 Loop-I,TXPROB,WRCPAB',I,TXPROB(I),WRCPAB
        IF(TXPROB(I).GT.WRCPAB) GO TO  80
        Z = (WRCFC(I)-WRCUAV)/SIGMA
        RAD = SQRT(C*Z**2+ABT)
        CLIML(I) = WRCUAV + SDA*(Z-RAD)
        CLIMU(I) = WRCUAV + SDA*(Z+RAD)
c      write(*,*) 'WCFFCX: 80 Loop-CLIML,CLIMU',CLIML(I),CLIMU(I)
   80 CONTINUE
      IF(MSL.LT.3)GOTO90
      DO85I=1,3
      TAB(I)= 10.**CLIML(INDXPT(I))
   85 TAB2(I)=10.**CLIMU(INDXPT(I))
      Z=100.*CLSIZE
      WRITE(*,239) Z, TAB2, TAB
      WRITE(MSG,239) Z, TAB2, TAB
239   FORMAT( '    WCF239J-FREQ CURVE CONF LIMS B17B', F5.1,F9.1,2F12.1,
     $      /,39X, 3F12.1 )
   90 CONTINUE
      RETURN
      END
C
C
C
      BLOCKDATA
C
C     + + + PURPOSE + + +
C     INITIALIZE COMMON WCFCM0
C     REV 7/77 WK TO  ENLARGE WCFCM0 FOR USE BY J407-X.
C     REV 5/79 WK - SET IWXMOD TO 2 IF ANY WRC PARAMETERS ARE CHANGED.
C     REV 11/03 PRH, AQUA TERRA Consultants, for batch version of PEAKFQ
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
C     WCFBDI is set here, but not used anywhere else
Cprh      COMMON/ WCFBDI  /WCFBDI
Cprh      INTEGER WCFBDI(1)
C
      INCLUDE 'cwcf0.inc'
C
C     + + + DATA INITIALIZATIONS + + +
Cprh      DATA    WCFBDI   /0/
      DATA MSG, MSL, NOPPOS, NOTRAN, NOSYS, NOEPFC, NOCLIM
     $   /   6,   2,     0,     0,      0,      0,       0            /
C
Cmeb: BIGLOG changed from 38 to 37 on Data General AViiON
Caml: and further changed to 29 for 5/94 compiler
      DATA  GSMIN, GSMAX, WEIBA, CLSIZE, INDX1, INDX2, WSKLAT, BIGLOG
     $   /-.4001, .8001,     0.,    .95,    5,    29,      0.,    29.  /
C
c      DATA NINDX/14/, INDXPT/16,21,26,5,6,10,11,12,   17,20,23,25,27,28,
c     *                        17*0 /
Cprh: expanded index array to include 1.5 yr (.6667) and 2.33 yr (.4292) floods
      DATA NINDX/15/, INDXPT/17,22,27,5,6,10,11,12,14,18,21,24,26,28,29,
     *                        17*0 /

c      DATA  TXPROB /
c     $ 0.9999, 0.9995, 0.9990, 0.9980, 0.9950, 0.9900, 0.9800, 0.9750,
c     $ 0.9600, 0.9500, 0.9000, 0.8000, 0.7000, 0.6000, 0.5704, 0.5000, 
c     $ 0.4296, 0.4000, 0.3000, 0.2000, 0.1000, 0.0500, 0.0400, 0.0250,
c     $ 0.0200, 0.0100, 0.0050, 0.0020, 0.0010, 0.0005, 0.0001/
Cprh: added 1.5 yr (.6667) flood to probability array
      DATA  TXPROB /
     $ 0.9999, 0.9995, 0.9990, 0.9980, 0.9950, 0.9900, 0.9800, 0.9750,
     $ 0.9600, 0.9500, 0.9000, 0.8000, 0.7000, 0.6667, 0.6000, 0.5704, 
     $ 0.5000, 0.4292, 0.4000, 0.3000, 0.2000, 0.1000, 0.0500, 0.0400,
     $ 0.0250, 0.0200, 0.0100, 0.0050, 0.0020, 0.0010, 0.0005, 0.0001/
C
      DATA EPS1,EPS2, HRECWO,HRECWX,HRECWS, RMSDGS, CPAMP1,CPAMP2,CPAMP3
     $  /  -0.4,+0.4,  1.0,   0.0,   1.0,  0.55,    0.50,  0.10,   0.01/
      DATA SIGLOT, SIGHOT / 0.10, 0.10/
C
      DATA      WCXAUX  / 2*0. /, IWXMOD / 0 /
C
C     + + + END SPECIFICATIONS + + +
C
      END
C
C
C
      REAL   FUNCTION   WCFGSM
     #                         ( FLATA, FLONGA )
C
C     + + + PURPOSE + + +
C     WRC BULL. 17-A GENERALIZED SKEW MAP.C
C     FOR LATITUDE AND LONGITUDE IN DEGREES AND FRACTIONS OF A DEGREE.
C     (E.G., 45 DEG, 30 MIN  =  45.5 DEG.)
C     POINTS OUTSIDE U.S. ARE ASSIGNED LARGE NEGATIVE VALUE.
C
C     DATA STATEMENT USES 2 CARDS PER MERIDIAN OF LONGITUDE, STARTING AT
C     67 DEG AND ENDING AT 125 DEG WEST LONGITUDE.  SKEW VALUES ARE  READ
C     FROM SOUTH (25 DEG) TO NORTH (50 DEG)  ALONG EACH MERIDIAN.
C     SKEW VALUES TABULATED ARE IN UNITS OF 0.01 SKEW UNIT.  POINTS
C     OUTSIDE CONTINENTAL U.S. ARE SET TO -222.
C     ALASKA AND HAWAII ARE NOT TABULATED BY LAT-LONG BUT ARE HANDLED
C     CORRECTLY BY PROGRAMMED TESTS.   ALSO PUERTO RICO.
C
C     REV 8/6/79 WK TO IMPROVE THE REPRESENTATION OF THE GEN SKEW RIDGE
C             AT GEN SKEW VALUE OF 0.2 RUNNING THROUGH EAST CALIFORNIA
C             AND WEST OREGON AND WASHINGTON.
C
C     REV 8/9/79 WK REVISED VARIOUS MAP SKEWS.
C
C     REV 11/5/81 WK -- ADD PUERTO RICO GEN SKEW = 0.0  (12/17/86-WK)
C
C     REV 4/86 WK TO CIRCUMVENT PRIME F77 REV19.4.4 COMPILER BUG(S)
C       RELATING TO STATEMENT FUNCTIONS.
C
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  FLATA, FLONGA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FLATA  - Latitude North, degrees and fraction
C     FLONGA - Longitude West, degrees and fraction
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2 ISK(26,59)
      INTEGER  I, J, LAT, LONG
      REAL     FLAT, FLONG, GSK, S, WX, WY
      REAL     FLATMN, FLATMX, FLONMN, FLONMX
      LOGICAL LLCELL, LLC
C
C     + + + SAVE VARIABLES + + +
      INTEGER*2 ISK1A(26,5),ISK1B(26,4),ISK2A(26,5),ISK2B(26,4),
     1          ISK3A(26,5),ISK3B(26,4),ISK4A(26,5),ISK4B(26,4),
     2          ISK5A(26,5),ISK5B(26,4),ISK6A(26,5),ISK6B(26,4),
     3          ISK7A(26,5)
      SAVE      ISK1A,ISK1B,ISK2A,ISK2B,
     1          ISK3A,ISK3B,ISK4A,ISK4B,
     2          ISK5A,ISK5B,ISK6A,ISK6B,
     3          ISK7A
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (ISK1A(1,1),ISK(1, 1)),(ISK1B(1,1),ISK(1, 6)),
     1            (ISK2A(1,1),ISK(1,10)),(ISK2B(1,1),ISK(1,15)),
     2            (ISK3A(1,1),ISK(1,19)),(ISK3B(1,1),ISK(1,24)),
     3            (ISK4A(1,1),ISK(1,28)),(ISK4B(1,1),ISK(1,33)),
     4            (ISK5A(1,1),ISK(1,37)),(ISK5B(1,1),ISK(1,42)),
     5            (ISK6A(1,1),ISK(1,46)),(ISK6B(1,1),ISK(1,51)),
     6            (ISK7A(1,1),ISK(1,55))
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ISK1A /
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,  55,  38,  12,  -8, -15,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,  50,  33,  10,  -8, -15,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,  70,  70,  70,  47,  34,  10,  -8, -20,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,  70,  70,  60,  44,  31,   5, -10, -20,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,  70,  70,  57,  40,  28,   0, -10,-222,-222,-222/
      DATA ISK1B /
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,  70,  70,  70,  48,  35,  18,  -2,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222,  70,  70,  60,  42,  30,   8,  -8,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,  70,  70,  70,  48,  36,  20,   0, -12,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,  45,  70,  70,
     $   70,  70,  70,  56,  40,  28,  10,  -5, -16,-222,-222,-222,-222/
      DATA ISK2A /
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,  20,  40,  70,  70,
     $   70,  70,  65,  44,  32,  18,   0, -10,-222,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,  17,  38,  60,  70,
     $   70,  70,  50,  33,  25,   5,  -8, -17,-222,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222,-222,  -3,  12,  32,  54,  68,
     $   65,  52,  38,  28,  10,  -5, -15,-222,-222,-222,-222,-222,-222,
     $ -222,-222,-222,-222,-222,-222,-222, -10, -10,   2,  30,  50,  60,
     $   55,  42,  30,  12,  -2, -13, -24,-222,-222,-222,-222,-222,-222,
     $  -10, -10, -10, -10, -10, -10, -10, -10, -10,  -7,  25,  45,  51,
     $   40,  28,  12,  -1, -12, -24, -34,-222,-222,-222,-222,-222,-222/
      DATA ISK2B /
     $   -3,  -7, -10, -10, -10, -10, -10, -10, -10, -10,  15,  40,  40,
     $   30,  15,   0, -10, -22, -33,-222,-222,-222,-222,-222,-222,-222,
     $    0,  -2,  -5,  -6, -10, -10, -10, -10, -10, -10,  10,  32,  29,
     $   18,   0, -10, -20, -30, -40, -40,-222,-222,-222,-222,-222,-222,
     $ -222,   0,   0,   0,  -4,  -7, -10, -10, -10, -10,   5,  18,  15,
     $    0, -10, -20, -30, -40, -40, -40, -40, -33,-222,-222,-222,-222,
     $ -222,-222,-222,-222,   5,   0,  -3,  -8,  -8,  -3,   5,   8,   0,
     $   -9, -19, -30, -40, -40, -40, -40, -40, -25, -10,-222,-222,-222/
      DATA ISK3A /
     $ -222,-222,-222,-222,  17,  13,   8,   3,   2,   2,   5,   2,  -7,
     $  -15, -28, -40, -40, -40, -40, -40, -33, -20, -10,-222,-222,-222,
     $ -222,-222,-222,-222,  20,  20,  20,  17,  12,   7,   5,  -2, -14,
     $  -24, -37, -40, -40, -40, -40, -40, -28, -10, -10,-222,-222,-222,
     $ -222,-222,-222,-222,-222,  20,  20,  20,  16,   8,   0, -12, -19,
     $  -32, -40, -40, -40, -40, -40, -38, -23, -10, -10, -10,-222,-222,
     $ -222,-222,-222,-222,-222,  20,  20,  20,  11,   3,  -8, -17, -27,
     $  -38, -40, -40, -40, -40, -40, -35, -22, -10, -10, -10, -10, -10,
     $ -222,-222,-222,-222,  20,  20,  19,  12,   3,  -5, -14, -24, -30,
     $  -38, -40, -40, -40, -40, -40, -36, -23, -10, -10, -10, -10, -10/
      DATA ISK3B /
     $ -222,-222,-222,-222,  18,  13,  12,   2,  -6, -15, -23, -30, -32,
     $  -38, -40, -40, -40, -40, -40, -38, -27, -14, -10, -10, -10, -10,
     $ -222,-222,-222,-222,   9,   4,   2,  -9, -19, -24, -30, -30, -30,
     $  -35, -39, -40, -40, -40, -40, -40, -30, -20, -10, -10, -10, -10,
     $ -222,-222,-222,-222,   0,  -7, -13, -20, -25, -30, -30, -30, -30,
     $  -34, -38, -40, -40, -40, -40, -40, -36, -26, -13, -10, -10, -10,
     $ -222,-222,-222,-222, -10, -15, -21, -27, -30, -30, -30, -30, -30,
     $  -31, -35, -38, -40, -40, -40, -40, -40, -32, -22, -15, -10, -10/
      DATA ISK4A /
     $ -222,-222,-222,-222, -18, -24, -29, -30, -30, -30, -30, -30, -30,
     $  -30, -31, -34, -37, -40, -40, -40, -40, -39, -27, -22, -17, -10,
     $ -222,-222,-222, -23, -26, -30, -30, -30, -29, -29, -28, -28, -28,
     $  -28, -29, -30, -32, -38, -40, -40, -40, -40, -33, -28, -20, -15,
     $ -222,-222,-222, -29, -30, -29, -29, -28, -27, -26, -25, -25, -24,
     $  -24, -24, -25, -25, -30, -38, -40, -40, -40, -40, -34, -28, -20,
     $  -30, -30, -30, -29, -28, -28, -26, -25, -24, -23, -23, -22, -22,
     $  -20, -19, -17, -15, -14, -22, -40, -40, -40, -40, -40, -32, -27,
     $  -30, -29, -28, -28, -27, -25, -24, -23, -22, -21, -20, -19, -18,
     $  -15, -13,  -9,  -2,  30,   2, -25, -40, -40, -40, -40, -37, -32/
      DATA ISK4B /
     $ -222, -27, -27, -26, -24, -23, -22, -21, -20, -19, -18, -16, -14,
     $  -13,  -9,  -5,  35,  60,  45,  -5, -30, -40, -40, -40, -40, -35,
     $ -222, -25, -24, -23, -22, -21, -20, -18, -17, -16, -15, -13, -13,
     $  -10,  -7,  -2,  60,  60,  60,  10, -20, -40, -40, -40, -40, -40,
     $ -222,-222,-222, -21, -20, -18, -16, -15, -14, -14, -12, -11, -10,
     $  -10,  -6,  -1,  60,  60,  60,  23, -11, -33, -40, -40, -40, -40,
     $ -222,-222,-222,-222, -13, -13, -12, -12, -11, -11, -10, -10, -10,
     $  -10,  -7,  -2,  50,  60,  60,  32,  -6, -27, -40, -40, -40, -40/
      DATA ISK5A /
     $ -222,-222,-222,-222,   0,  -4,  -5,  -7,  -8,  -9, -10, -10, -10,
     $  -10,  -8,  -4,  20,  60,  60,  30,  -5, -24, -40, -40, -40, -40,
     $ -222,-222,-222,-222,  -9,  -5,   0,   0,  -4,  -6,  -8, -10, -10,
     $  -10, -10,  -8,  -2,  40,  50,  12,  -3, -18, -40, -40, -40, -40,
     $ -222,-222,-222,-222, -15, -13,  -8,   0,   0,  -3,  -5,  -8, -10,
     $  -10, -12, -13,  -8,  -1,   8,   0,  -1, -10, -38, -40, -40, -40,
     $ -222,-222,-222,-222,-222, -20, -18, -12,  -5,   0,  -3,  -6,  -8,
     $  -10, -23, -30, -25,  -8,   0,   0,   0,  -7, -27, -40, -40, -40,
     $ -222,-222,-222,-222,-222,-222, -20, -20, -14,  -5,   0,  -5,  -8,
     $  -12, -30, -30, -30, -15,  -4,   0,  10,   0, -17, -38, -40, -40/
      DATA ISK5B /
     $ -222,-222,-222,-222,-222,-222, -20, -20, -20, -12,   0,  -4,  -7,
     $  -14, -30, -30, -30, -30,  -8,   3,  12,  11,  -5, -26, -40, -40,
     $ -222,-222,-222,-222,-222,-222, -20, -20, -20, -17,  -7,   0,  -7,
     $  -12, -26, -30, -30, -30, -20,  -2,  15,  17,   8, -14, -30, -40,
     $ -222,-222,-222,-222,-222,-222, -20, -20, -20, -17,  -10,   0, -4,
     $   -9, -22, -30, -30, -30, -30, -13,  12,  21,  17,   2, -18, -35,
     $ -222,-222,-222,-222,-222,-222, -13, -20, -20, -17, -11,  -4,  -3,
     $   -8, -18, -27, -30, -30, -30, -30,  -5,  19,  24,  20,   0, -25/
      DATA ISK6A /
     $ -222,-222,-222,-222,-222,-222,   8,  -5, -11, -13, -10,  -4,   0,
     $   -4, -13, -20, -28, -30, -30, -30, -25,   5,  28,  38,  28, -10,
     $ -222,-222,-222,-222,-222,-222,  18,   9,  -3,  -8,  -7,  -3,   0,
     $    0,  -8, -14, -23, -28, -30, -30, -30, -20,  18,  60,  60,  20,
     $ -222,-222,-222,-222,-222,-222,   0,  20,   8,  -1,  -3,   0,   0,
     $    0,   0,  -9, -17, -23, -29, -30, -30, -30, -20,  30,  60,  60,
     $ -222,-222,-222,-222,-222,-222,-222,   3,  20,   8,   0,   0,   0,
     $    0,   0,  -2, -10, -17, -23, -30, -30, -30, -30, -20,  30,  60,
     $ -222,-222,-222,-222,-222,-222,-222, -28,   5,  20,  10,   0,   0,
     $    0,   0,   0,  -3, -11, -16, -23, -28, -30, -30, -30, -10,  45/
      DATA ISK6B /
     $ -222,-222,-222,-222,-222,-222,-222, -30, -22,  10,  20,   8,   0,
     $    0,   0,   0,   0,  -3,  -8, -15, -20, -28, -30, -30, -30, -10,
     $ -222,-222,-222,-222,-222,-222,-222, -30, -30, -17,   7,  20,  10,
     $    2,   0,   0,   0,   0,   0,  -6, -12, -17, -25, -30, -30, -30,
     $ -222,-222,-222,-222,-222,-222,-222,-222, -30, -30, -15,   5,  20,
     $   12,   4,   0,   0,   0,   0,   0,  -2,  -5, -12, -18, -22, -30,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222, -30, -30, -17,   2,
     $   20,  20,   8,   1,   0,   0,   0,   0,   0,   0,  -3,  -7, -10/
      DATA ISK7A /
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222, -30, -30, -30, -23,
     $   -6,  10,  20,  17,   8,   0,   0,   0,   0,   0,   0,   0,   0,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222, -30, -30, -30,
     $  -30, -15,   0,  15,  20,  15,  10,   1,   0,   0,   0,   0,   0,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222, -30, -30,
     $  -30, -30, -30, -18, -10,  15,  20,  20,  15,   3,   0,   0,   0,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $  -30, -30, -30, -30, -30, -30, -18,  -5,  15,  20,  20,   5,   0,
     $ -222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,-222,
     $ -222,-222, -30, -30, -30, -30, -30, -30, -30, -20,   0,   0,  20/
C
C     + + + END SPECIFICATIONS + + +
C
      LLCELL(FLATMN,FLATMX,FLONMN,FLONMX) = FLONG.GE.FLONMN
     $  .AND. FLONG.LT.FLONMX .AND. FLAT.GE.FLATMN .AND. FLAT.LT.FLATMX
C
Caml  exponent changed from 37 to 29 for 5/94 compiler
      WCFGSM = -1E29
      FLAT = FLATA
      FLONG = FLONGA
C
      LLC = LLCELL(25., 50.01,  67.,  125.01 )
      IF(LLC) GO TO 100
C
C  CHECK ALASKA, HAWAII, PUERTO RICO
C
      LLC = LLCELL(54., 61.,129., 140.)
      IF(LLC) WCFGSM = 0.33
C
      LLC = LLCELL(58., 72.,140., 170.)
      IF(LLC) WCFGSM = 0.70
C
      LLC = LLCELL(50., 58.,150., 360.)
      IF(LLC) WCFGSM = 0.70
C
      LLC = LLCELL(18., 30.,154., 162.)
      IF(LLC) WCFGSM = -0.05
C
      LLC = LLCELL(17., 19., 63.,  68.)
      IF(LLC) WCFGSM = 0.0
C
      RETURN
C
C  CONTINENTAL U.S. SKEW MAP
  100 CONTINUE
      LAT = FLAT
      LONG = FLONG
      WY = FLAT - LAT
      WX = FLONG - LONG
      GSK = 0.
      DO 150 I =1, 2
      WY = 1. - WY
      DO 150 J = 1, 2
      WX = 1. - WX
      S = ISK( LAT-24+I-1, LONG-66+J-1 )
      IF(S.GT.-220.) GO TO 150
      IF(WX.LT.0.05) WX = 0.
      IF( WY.LT.0.05) WY = 0.
Caml  37 changed to 29 fro 5/94 compiler
      S = -1E29
  150 GSK = GSK + 0.01*S*WX*WY
      IF(GSK.GT.-1E29) WCFGSM = GSK
      RETURN
      END
C
C
C
      SUBROUTINE   GBTESTX
     I                    (SYSLOG,NSYS1,EMAOPT,
     O                     IER)
C
C     + + + PURPOSE + + +
C     wrapper for call to call low outlier tests, 
C     both traditional B17B and Multiple Grubbs-Beck test in EMA code
C
C     EMAThresh contains Threshold specs and EMA data arrays
      USE EMAThresh
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NSYS1,IER,EMAOPT
      REAL    SYSLOG(NSYS1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SYSLOG - systematic peak logarithms (input)
C     NSYS1  - number of systematic peak logarithms
C     IER    - error return code
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'

      integer ns,nlow,nzero
      double precision  gbcrit,gbthresh,pvaluew,qs
      character*4 gbtype

C     used by Tim's EMA code
      common /tacg01/gbcrit,gbthresh,pvaluew(10000),qs(10000),
     1               ns,nlow,nzero,gbtype
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
      REAL    LGBCRIT
      DOUBLE PRECISION MISSNG,GBTHRSH,REGSKEW,REGMSE,Z
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MISSNG /1.0D-99/
C
C     + + + EXTERNALS + + +
      EXTERNAL GBTEST, WCFDLO
C
C     + + + INTRINSICS + + +
      INTRINSIC LOG10, MAX
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('    EMA003I-LOW OUTLIERS WERE DETECTED USING ',
     $       'MULTIPLE GRUBBS-BECK TEST',I8,4X,F8.1,/
     $       '      THE FOLLOWING PEAKS (WITH CORRESPONDING P-VALUES)',
     $       ' WERE DROPPED:')
 2010 FORMAT(8X,F8.1,4X,'(',F6.4,')')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LOTYPE .EQ. 'MGBT') THEN
C       perform Multiple Grubbs-Beck LO test
        IF (EMAOPT.EQ.0) THEN
C         but only perform it here for B17B method (EMA has it built into code)
          GBTYPE = 'MGBT'
          GBTHRSH = LOG10(MAX(MISSNG,GAGEB))
          REGSKEW= GENSKU
          IF (IGSOPT.EQ.1) THEN
C           Generalized skew, set to very small
            REGMSE = 0.0
          ELSE IF (IGSOPT.EQ.-1) THEN
C           Station skew, ignore regional skew
            REGMSE = 1.0D10
          ELSE
C           Weighted, set to root mean square
            REGMSE = RMSEGS**2
          END IF
 
          CALL GBTEST(NOBS,QL,QU,TL,TU,DTYPE,GBTHRSH,
     M                QL,QU,TL,TU)

          LGBCRIT = REAL(gbcrit)
C         report Multiple GB LO messges
          IF (NLOW .GT. 0) THEN
            WRITE(MSG,2000) nlow-nzero,10**LGBCRIT
            DO 10 I = 1,NLOW
              IF (qs(I).GT.1.0D-99) THEN
                WRITE(MSG,2010)qs(I),pvaluew(I)
              END IF
 10         CONTINUE
          END IF
        END IF

C       update B17 computational variables        
        NLWOUT = NLOW - NBGB
        DO 20 I=1,NSYS1
          IF (SYSLOG(I).LT.LGBCRIT .AND. SYSLOG(I).GT.SYSBAS) THEN
            Z = SYSLOG(I)
            SUMS(1)=SUMS(1)-Z
            SUMS(2)=SUMS(2)-Z**2
            SUMS(3)=SUMS(3)-Z**3
          END IF
 20     CONTINUE

      ELSE
C       perform traditional B17B LO test
        CALL WCFDLO (SYSLOG,NSYS1,IER,EMAOPT)
C       low outlier info needs to be set here for later storage/use
C       (GBTEST sets these in call for multiple GB)
        gbcrit = WRCBAS
        nlow = NBGB + NLWOUT
        nzero= NBGB
c        nzero= 0
c        DO 10 I = 1, NSYS1
c          IF (SYSLOG(I) .LT. 1.0E-6) THEN
c            nzero = nzero + 1
c          END IF
c 10     CONTINUE
      END IF

C
      RETURN
      END