      SUBROUTINE STCOST
C	STORAGE TREATMENT COST
C	CALLED BY STRT AT LINE 616
C=======================================================================
C     SUBROUTINE TO PRINT COST CALCULATION DATA.
C=======================================================================
C     WCH, 12/5/94.  MOVE PRINT-OUTS TO LEFT ON PAGE. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'S1.INC'
      DIMENSION COST(2),TCOST(2),VAR(2)
C=======================================================================
  100 WRITE(N6,130)
      WRITE(N6,150)
C
  200 TCOST(1) = 0.0
      TCOST(2) = 0.0
C
  300 DO 450 I = 1,NU
      DO 350 IC = 1,2
      COST(IC)  = 0.0
      VAR(IC)   = 0.0
      IF(KPC(I,IC).LE.0) GO TO 330
      IF(KPC(I,IC).EQ.1) VAR(IC) = QMAX(I)
      IF(KPC(I,IC).EQ.2) VAR(IC) = QMAXS(I)
      IF(KPC(I,IC).EQ.3) VAR(IC) = VMAX(I)
      IF(KPC(I,IC).EQ.4) VAR(IC) = VMAXS(I)
      IF(IC.EQ.1) COST(IC) = CCC(I,1)*VAR(IC)**CCC(I,2)
      IF(IC.EQ.2) COST(IC) = CCC(I,3)*VAR(IC)**CCC(I,4)
  330 IF(IC.EQ.2) COST(IC) = COST(IC)+CCC(I,5)*DOP(I)
      TCOST(IC)            = TCOST(IC)+COST(IC)
  350 CONTINUE
C
  400 WRITE(N6,430) I,CCC(I,1),CCC(I,2),KPC(I,1),VAR(1),COST(1),
     1      CCC(I,3),CCC(I,4),CCC(I,5),KPC(I,2),VAR(2),DOP(I),COST(2)
  450 CONTINUE
C
  500 WRITE(N6,550) TCOST
      IF(METRIC.EQ.1) WRITE(N6,570)
      IF(METRIC.EQ.2) WRITE(N6,590)
C=======================================================================
C#### WCH, 12/5/94.  MOVE OUTPUT TO LEFT ON PAGE.  CHANGE 10X TO 1X ETC.
C#### AMOUNTS TO SUBTRACTING 9 FROM MOST INITIAL X SPACINGS.
  130 FORMAT(//,10X,'$$ COST CALCULATIONS $$',/)
  150 FORMAT(7X,12('-'),' INITIAL CAPITAL COST ',11('-'),2X,16('-'),
     1' OPERATION AND MAINTENANCE COSTS ',15('-'),/, 1X,'UNIT  ---EQ. CO
     2EFFS.---  ---COST VAR.--    COST,$    -----EQUATION COEFFS.-----
     3---COST VAR.--   OPER.     COST,$',/, 2X,'NO.      A        B
     4TYPE*   VALUE',19X,'D        F        H     TYPE*   VALUE    HOURS
     5',/, 1X,'----  -------- --------  ----- --------  ----------  ----
     6---- -------- --------  ----- -------- --------  ----------')
  430 FORMAT( 2X,I2,3X,E8.3,1X,E8.3,4X,I1,3X,E8.3,2X,E10.4,2X,E8.3,
     11X,E8.3,1X,E8.3,4X,I1,3X,E8.3,1X,E8.3,2X,E10.4)
  550 FORMAT(/, 1X,'TOTAL COST FOR ALL UNITS',17X,E10.4,55X,E10.4,/)
  570 FORMAT(9X,'* 1 = MAXIMUM ALLOWABLE INFLOW, CFS',/,11X,'2 = MAXIMUM
     1 OBSERVED INFLOW, CFS',/,11X,'3 = MAXIMUM ALLOWABLE STORAGE, CU FT
     2',/,11X,'4 = MAXIMUM OBSERVED STORAGE, CU FT')
  590 FORMAT(9X,'* 1 = MAXIMUM ALLOWABLE INFLOW ,CU M/SEC',/,11X,'2 = MA
     1XIMUM OBSERVED INFLOW, CU M/SEC',/,11X,'3 = MAXIMUM ALLOWABLE STOR
     2AGE, CU M',/,11X,'4 = MAXIMUM OBSERVED STORAGE, CU M')
C=======================================================================
      RETURN
      END
