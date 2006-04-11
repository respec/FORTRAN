      SUBROUTINE RSPLIT
C=======================================================================
C     INPUT AND INITIALIZATION ROUTINE FOR TABULAR FLOW SPLIT OPTION
C      IN THE TRANSPORT BLOCK, TYPE 26 FLOW DIVIDER.
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     SLIGHTLY REVISED FOR STYLE AND INSTALLED BY WCH, 8/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'FSPLIT.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      CHARACTER*40 INSTRING
C=======================================================================
C     CHECK
C ======================================================================
      IF(KINOU.GE.NTSP) THEN 
         WRITE(N6,6000) KINOU,NTSP
         STOP
         ENDIF
C=======================================================================
C     LOOP THROUGH KINOU ELEMENTS
C=======================================================================
      DO 500 IS =1,KINOU
C=======================================================================
C     FIND INTERNAL NUMBER OF THIS FLOW SPLIT ELEMENT.
C=======================================================================
      DO 10 I = 1,NE
      M       = I
      IF(KINOUT(I).EQ.IS) GO TO 20 
   10 CONTINUE
   20 CONTINUE
C=======================================================================
C   >>>>>>>>>>>> READ DATA GROUP G6 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,INSTRING
      IF(CC.NE.'G6') THEN 
         WRITE(N6,6050) CC
         STOP
         ENDIF
C=======================================================================
C   >>>>>>>>>>>> READ DATA GROUP G7 <<<<<<<<<<<<
C=======================================================================
      NSPLIT(IS) = 0
      DO 30 MM = 1,NTSPTS
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'G7') GO TO 35
      NSPLIT(IS) = NSPLIT(IS)+1
      READ(N5,*,ERR=888) CC, SPLITIN(IS,MM),SPLITOUT(IS,MM)
   30 CONTINUE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF (CC.EQ.'G7') THEN
         WRITE(N6,4075) NTSPTS
         STOP
         ENDIF
   35 CONTINUE
      WRITE(N6,1010)
      IF(JCE.EQ.0) WRITE(N6,1030) NOE(M)
      IF(JCE.EQ.1) WRITE(N6,1031) KOE(M)
      WRITE(N6,1032) INSTRING
      IF(METRIC.EQ.1) WRITE(N6,1110)
      IF(METRIC.GE.2) WRITE(N6,1115) 
      DO 40 MM=1,NSPLIT(IS)
      WRITE(N6,1120) SPLITIN(IS,MM),SPLITOUT(IS,MM),
     1               SPLITIN(IS,MM)-SPLITOUT(IS,MM)
      IF ((MM.EQ.1).AND.
     1       ((SPLITIN(IS,MM)+SPLITOUT(IS,MM)).NE.0.0)) THEN 
         WRITE(N6,1130)
         STOP
         ENDIF
      IF (SPLITOUT(IS,MM).GT.SPLITIN(IS,MM)) THEN
         WRITE(N6,1140)
         STOP
         ENDIF
      IF(MM.GT.1) THEN
         IF(SPLITIN(IS,MM).LT.SPLITIN(IS,MM-1)) THEN
            WRITE(N6,1150) 
            STOP
            ENDIF
         ENDIF
   40 CONTINUE
      IF(JCE.EQ.0) WRITE(N6,1165) GEOM3(M)
      IF(JCE.EQ.1) WRITE(N6,1166) KGEOM(M)
      IF(METRIC.EQ.1) GO TO 500
C=======================================================================
C     CONVERT FROM METRIC TO U.S. CUSTOMARY UNITS.
C=======================================================================
      DO 200 MM       = 1,NSPLIT(IS)
      SPLITIN(IS,MM)  = SPLITIN(IS,MM)*35.315
      SPLITOUT(IS,MM) = SPLITOUT(IS,MM)*35.315
  200 CONTINUE
  500 CONTINUE
      RETURN
  888 CALL IERROR
C=======================================================================
 1010 FORMAT(1H1,/,10X,'TABULAR FLOW SPLIT INPUT DATA',/)
 1030 FORMAT(10X,34('*'),/,6X,'TYPE 26 TABULAR FLOW SPLIT ELEMENT # '
     1,I10,/,10X,34('*'),/)
 1031 FORMAT(10X,23('*'),/,6X,'TYPE 26 TABULAR FLOW SPLIT ELEMENT # '
     1,A10,/,10X,23('*'),/)
 1032 FORMAT(10X,'DESCRIPTION FROM G6 LINE - ',1A40)
 1110 FORMAT(/,10X,'TABULAR INFLOW-OUTFLOW RELATIONSHIPS',//,10X,
     1'INFLOW CFS   UNDIVERTED OUTFLOW CFS   DIVERTED OUTFLOW  CFS ',/,
     210X,'----------   ----------------------   ----------------------'
     3)
 1115 FORMAT(/,10X,'TABULAR INFLOW-OUTFLOW RELATIONSHIPS',//,10X,
     1'INFLOW CMS   UNDIVERTED OUTFLOW CMS   DIVERTED OUTFLOW  CMS ',
     2'-----------  ----------------------   ----------------------')
 1120 FORMAT(10X,F11.3,F15.3,F28.3)
 1130 FORMAT(/,' ERROR - FIRST POINT IN CURVE MUST HAVE ZERO INFLOW '
     1,'AND ZERO UNDIVERTED FLOW.',/,' RUN STOPPED.')
 1140 FORMAT(/, ' ERROR - DIVERTED FLOW CANNOT EXCEED INFLOW.',/,
     1 ' RUN STOPPED.')
 1150 FORMAT(/,'ERROR - INFLOWS MUST INCREASE.  RUN STOPPED.')
 1165 FORMAT(/,5X,'UNDIVERTED FLOW FROM FLOW DIVIDER IS TO',
     1' ELEMENT NO.',F8.0)
 1166 FORMAT(/,5X,'UNDIVERTED FLOW FROM FLOW DIVIDER IS TO',
     1' ELEMENT NO.',A10)
 4075 FORMAT(/,' ERROR - ATTEMPT TO READ MORE THAN ',I3,' G7 INPUTS. RUN
     1 STOPPED.')
 6000 FORMAT(/,' ERROR - NUMBER OF INPUT TYPE 26 TABULAR FLOW SPLITS = '
     1,I4,' THIS EXCEEDS MAXIMUM OF ',I4,' DEFINED BY NTSP'
     2,' IN PROGRAM PARAMETER STATEMENTS',/,' RUN STOPPED.')
 6050 FORMAT(/' ERROR. LOOKING FOR CARD TYPE G6 BUT FOUND ',A2,/,
     1' RUN STOPPED.') 
C=======================================================================
      END
