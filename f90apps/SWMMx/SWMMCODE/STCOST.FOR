      SUBROUTINE STCOST
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
  130 FORMAT(1H1,/////,10X,'COST CALCULATIONS',/////)
  150 FORMAT(16X,12('-'),' INITIAL CAPITAL COST ',11('-'),2X,16('-'),
     1' OPERATION AND MAINTENANCE COSTS ',15('-'),/,10X,'UNIT  ---EQ. CO
     2EFFS.---  ---COST VAR.--    COST,$    -----EQUATION COEFFS.-----
     3---COST VAR.--   OPER.     COST,$',/,11X,'NO.      A        B
     4TYPE*   VALUE',19X,'D        F        H     TYPE*   VALUE    HOURS
     5',/,10X,'----  -------- --------  ----- --------  ----------  ----
     6---- -------- --------  ----- -------- --------  ----------')
  430 FORMAT(11X,I2,3X,E8.3,1X,E8.3,4X,I1,3X,E8.3,2X,E10.4,2X,E8.3,
     11X,E8.3,1X,E8.3,4X,I1,3X,E8.3,1X,E8.3,2X,E10.4)
  550 FORMAT(/,10X,'TOTAL COST FOR ALL UNITS',17X,E10.4,55X,E10.4,//)
  570 FORMAT(18X,'* 1 = MAXIMUM ALLOWABLE INFLOW,CFS',/,20X,'2 = MAXIMUM
     1 OBSERVED INFLOW,CFS',/,20X,'3 = MAXIMUM ALLOWABLE STORAGE,CU FT',
     2/,20X,'4 = MAXIMUM OBSERVED STORAGE,CU FT')
  590 FORMAT(18X,'* 1 = MAXIMUM ALLOWABLE INFLOW,CU M/SEC',/,20X,'2 = MA
     1XIMUM OBSERVED INFLOW,CU M/SEC',/,20X,'3 = MAXIMUM ALLOWABLE STORA
     2GE,CU M',/,20X,'4 = MAXIMUM OBSERVED STORAGE,CU M')
C=======================================================================
      RETURN
      END
