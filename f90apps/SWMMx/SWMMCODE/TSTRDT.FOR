      SUBROUTINE TSTRDT
C=======================================================================
C     INPUT AND INITIALIZATION ROUTINE FOR INTERNAL STORAGE IN THE
C       TRANSPORT BLOCK.
C     MODIFIED FROM ROUTINE IN THE STORAGE/TREATMENT BLOCK BY
C       S.J.NIX, SEPTEMBER 1981.  (REVISED, JANUARY 1983, NOV. 1989)
C     UPDATED NOVEMBER 1992 BY WCH TO CORRECT SUBSCRIPTS 
C       FOR A1,A2,D0
C     UPDATED 8/93 BY WCH (CHUCK MOORE, CDM) FOR LINE ID ERROR CHECKS.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
      DIMENSION A1(NTSE),A2(NTSE)
      EQUIVALENCE (A1,B1),(A2,B2)
C=======================================================================
C     LOOP THRU KSTOR ELEMENTS
C=======================================================================
      DO 1270 IS = 1,KSTOR
C#######################################################################
C WCH 11/3/92
C NEW INITIALIZATION
C#######################################################################
      A11 = 0.0
      A21 = 0.0
      D01 = 0.0
C=======================================================================
C     FIND INTERNAL NUMBER OF THIS STORAGE ELEMENT.
C=======================================================================
      DO 50 I = 1,NE
      M       = I
      IF(KSTORE(I).EQ.IS) GO TO 100
   50 CONTINUE
C=======================================================================
C>>>>>>>>  READ DATA GROUP G1  <<<<<<<<
C=======================================================================
  100 READ(N5,*,ERR=888) CC,LOUT(IS)
C=======================================================================
C>>>>>>>>  READ DATA GROUP G2  <<<<<<<<
C=======================================================================
      MINT      = 0
      STOR      = 0.0
      DO 230 MM = 1,17
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'G2') GO TO 240
      IF(LOUT(IS).EQ.0) THEN
                        READ(N5,*,ERR=888) CC,TSDEP(IS,MM),
     +                      TSAREA(IS,MM),TSTORE(IS,MM),TSQOU(IS,MM)
                        ELSE
                        READ(N5,*,ERR=888) CC,TSDEP(IS,MM),
     +                           TSAREA(IS,MM),TSTORE(IS,MM)
                        ENDIF
      IF(TSDEP(IS,1)+TSTORE(IS,1).GT.0.0) THEN
                                  IF(JCE.EQ.0) WRITE(N6,220) NOE(M)
                                  IF(JCE.EQ.1) WRITE(N6,221) KOE(M)
                                  STOP
                                  ENDIF
      IF(MM.GT.1.AND.TSDEP(IS,MM)+TSAREA(IS,MM).LE.0.0) GO TO 240
      MINT = MINT+1
      STOR = STOR+TSTORE(IS,MM)
  230 CONTINUE
  240 MINTS(IS) = MINT
      IF(STOR.GT.0.0) GO TO 260
      DO 250 MM     = 2,MINT
  250 TSTORE(IS,MM) = TSTORE(IS,MM-1) + (TSDEP(IS,MM) -
     +            TSDEP(IS,MM-1))*(TSAREA(IS,MM)+TSAREA(IS,MM-1))/2.0
  260  CONTINUE
C=======================================================================
      IF(LOUT(IS).LE.0) GO TO 600
      IF(LOUT(IS).GE.3) GO TO 500
C=======================================================================
C>>>>>>>>  READ DATA GROUP G3  <<<<<<<<
C=======================================================================
      LT           = LOUT(IS)
      DO 410    MM = 1,MINT
  410 TSQOU(IS,MM) = 0.0
C##################################################
C WCH, 11/3/92
C NECESSARY TO SAVE ONLY SECOND A1,A2,D0 PARAMETERS
C##################################################
      IF(JCE.EQ.0.AND.LT.EQ.2) READ(N5,*,ERR=888) CC,A11,D01,
     +                         A21,A1(IS),D0(IS),A2(IS),GEOM3(M)
      IF(JCE.EQ.1.AND.LT.EQ.2) READ(N5,*,ERR=888) CC,A11,D01,
     +                         A21,A1(IS),D0(IS),A2(IS),KGEOM(M)
      IF(LT.EQ.1) READ(N5,*,ERR=888) CC,A11,D01,A21
C#### WCH (CDM), 8/93.  ERROR CHECKS ON LINE ID.
      IF(CC.NE.'G3') THEN
         WRITE(N6,4040) CC
         STOP
         ENDIF
      DO 420 I  = 1,LT
      DO 420 MM = 1,MINT
C#### WCH, 11/3/92
         IF(I.EQ.1) THEN
      IF(TSDEP(IS,MM)-D0(I).GT.0.0001) TSQOU(IS,MM)=TSQOU(IS,MM)+A11*
     1                                     (TSDEP(IS,MM)-D01)**A21
         ENDIF
         IF(I.EQ.2) THEN
      IF(TSDEP(IS,MM)-D0(I).GT.0.0001) TSQOU(IS,MM)=TSQOU(IS,MM)+A1(IS)*
     1                                    (TSDEP(IS,MM)-D0(IS))**A2(IS)
         ENDIF
  420 CONTINUE
      GO TO 600
C=======================================================================
C>>>>>>>>  READ DATA GROUP G4  <<<<<<<<
C=======================================================================
  500 READ(N5,*,ERR=888) CC,TDSTAR(IS,1),TDSTAR(IS,2),TQPUMP(IS,1),
     1                      TQPUMP(IS,2),TDSTOP(IS)
C#### WCH (CDM), 8/93.  ERROR CHECKS ON LINE ID.
      IF(CC.NE.'G4') THEN
         WRITE(N6,4041) CC
         STOP
         ENDIF
      IF(TDSTAR(IS,2).LT.TDSTAR(IS,1)) THEN
                         IF(JCE.EQ.0) WRITE(N6,510) NOE(M)
                         IF(JCE.EQ.1) WRITE(N6,511) KOE(M)
                         STOP
                         ENDIF
      IF(TDSTOP(IS).GT.TDSTAR(IS,1))   THEN
                         IF(JCE.EQ.0) WRITE(N6,520) NOE(M)
                         IF(JCE.EQ.1) WRITE(N6,521) KOE(M)
                         STOP
                         ENDIF
C=======================================================================
C>>>>>>>>  READ DATA GROUP G5  <<<<<<<<
C=======================================================================
  600 IF(NPOLL.GT.0) READ(N5,*,ERR=888) CC,STORL(IS),
     +                                  (PTC0(IS,IP),IP=1,NPOLL)
      IF(NPOLL.EQ.0) READ(N5,*,ERR=888) CC,STORL(IS)
C#### WCH (CDM), 8/93.  ERROR CHECKS ON LINE ID.
      IF(CC.NE.'G5') THEN
         WRITE(N6,4042) CC
         STOP
         ENDIF
      WRITE(N6,1010)
      IF(JCE.EQ.0) WRITE(N6,1030) NOE(M)
      IF(JCE.EQ.1) WRITE(N6,1031) KOE(M)
      IF(METRIC.EQ.1) WRITE(N6,1110) LOUT(IS)
      IF(METRIC.GE.2) WRITE(N6,1115) LOUT(IS)
      DO 1120 MM = 1,MINT
 1120 WRITE(N6,1130) TSDEP(IS,MM),TSAREA(IS,MM),
     +               TSTORE(IS,MM),TSQOU(IS,MM)
C=======================================================================
      IF(LOUT(IS).LE.0) GO TO 1200
      IF(LOUT(IS).GE.3) GO TO 1170
      WRITE(N6,1140)
      DO 1150 I = 1,LT
C### WCH, 11/3/92
         IF(I.EQ.1) WRITE(N6,1160) A11,D01,A21
         IF(I.EQ.2) WRITE(N6,1160) A1(IS),D0(IS),A2(IS)
 1150 CONTINUE
      IF(JCE.EQ.0.AND.LOUT(IS).EQ.2) WRITE(N6,1165) GEOM3(M)
      IF(JCE.EQ.1.AND.LOUT(IS).EQ.2) WRITE(N6,1166) KGEOM(M)
      GO TO 1200
 1170 WRITE(N6,1180)
      IF(METRIC.EQ.1) WRITE(N6,1190) TDSTAR(IS,1),TDSTAR(IS,2),
     1                   TQPUMP(IS,1),TQPUMP(IS,2),TDSTOP(IS)
      IF(METRIC.EQ.2) WRITE(N6,1195) TDSTAR(IS,1),TDSTAR(IS,2),
     1                   TQPUMP(IS,1),TQPUMP(IS,2),TDSTOP(IS)
 1200 IF(LOUT(IS).LE.0) WRITE(N6,1205)
      IF(METRIC.EQ.1)   WRITE(N6,1210) IS,STORL(IS)
      IF(METRIC.EQ.2)   WRITE(N6,1220) IS,STORL(IS)
      IF(NPOLL.LE.0)    GO TO 1260
      WRITE(N6,1230)
      DO 1240 IP = 1,NPOLL
 1240 WRITE(N6,1250) PNAME(IP),PUNIT(IP),PTC0(IS,IP)
 1260 IF(METRIC.EQ.1) GO TO 1270
C=======================================================================
C     CONVERT FROM METRIC TO U.S. CUSTOMARY UNITS.
C=======================================================================
      DO 1262 MM    = 1,MINT
      TSDEP(IS,MM)  = TSDEP(IS,MM)*3.2808
      TSAREA(IS,MM) = TSAREA(IS,MM)*10.764
      TSTORE(IS,MM) = TSTORE(IS,MM)*35.315
 1262 TSQOU(IS,MM)  = TSQOU(IS,MM)*35.315
      STORL(IS)     = STORL(IS)*35.315
      IF(LOUT(IS).LT.3) GO TO 1264
      TDSTOP(IS)    = TDSTOP(IS)*3.2808
      DO 1263 I     = 1,2
      TDSTAR(IS,I)  = TDSTAR(IS,I)*3.2808
 1263 TQPUMP(IS,I)  = TQPUMP(IS,I)*35.315
      GO TO 1270
C####################
C WCH, 11/3/92
C####################
 1264 IF(LOUT(IS).NE.2) GO TO 1270
      D0(IS)     = D0(IS)*3.2808
      A1(IS)     = A1(IS)*3.2808**(3.0-A2(IS))
 1270 CONTINUE
      RETURN
  888 CALL IERROR
C=======================================================================
  220 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G2 : THE FIRST ENTRIES FOR
     1DEPTH AND VOLUME FOR STORAGE ELEMENT ',I8,/,32X,'ARE NON-ZERO. SIM
     2ULATION TERMINATED.')
  221 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G2 : THE FIRST ENTRIES FOR
     1DEPTH AND VOLUME FOR STORAGE ELEMENT ',A10,/,32X,'ARE NON-ZERO. SI
     2MULATION TERMINATED.')
  510 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G4 : IN STORAGE ELEMENT ',
     1I8,' THE DEPTH AT WHICH THE SECOND PUMPING RATE BEGINS IS LESS',/,
     232X,'THAN THE DEPTH AT WHICH THE FIRST PUMPING RATE BEGINS. SIMULA
     3TION TERMINATED.')
  511 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G4 : IN STORAGE ELEMENT ',
     1A10,' THE DEPTH AT WHICH THE SECOND PUMPING RATE BEGINS IS LESS',
     2/,32X,'THAN THE DEPTH AT WHICH THE FIRST PUMPING RATE BEGINS. SIMU
     3LATION TERMINATED.')
  520 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G4 : IN STORAGE ELEMENT ',
     1I8,' THE DEPTH AT WHICH ALL PUMPING STOPS IS GREATER THAN',/,32X,
     2'THE DEPTH AT WHICH PUMPING BEGINS. SIMULATION TERMINATED.')
  521 FORMAT(/,' ===> ERROR !!! IN DATA GROUP G4 : IN STORAGE ELEMENT ',
     1A10,' THE DEPTH AT WHICH ALL PUMPING STOPS IS GREATER THAN',/,32X,
     2'THE DEPTH AT WHICH PUMPING BEGINS. SIMULATION TERMINATED.')
 1010 FORMAT(1H1,/,10X,'STORAGE ELEMENT INPUT DATA',//)
 1030 FORMAT(10X,23('*'),/,10X,'STORAGE ELEMENT # ',I5,/,10X,23('*'),/)
 1031 FORMAT(10X,23('*'),/,10X,'STORAGE ELEMENT # ',A10,/,10X,23('*'),/)
 1110 FORMAT(/,10X,'DEPTH-AREA-STORAGE-OUTFLOW RELATIONSHIPS( LOUT = ',
     1I1,') :',//,5X,'DEPTH,FT.   SURFACE AREA,SQ.FT.   STORAGE,CU.FT.
     2     OUTFLOW,CFS',/,5X,
     3'---------   -------------------   --------------      -----------
     4')
 1115 FORMAT(/,10X,'DEPTH-AREA-STORAGE-OUTFLOW RELATIONSHIPS( LOUT = ',
     1I1,') :',//,5X,'DEPTH, M.   SURFACE AREA,SQ. M.   STORAGE,CU. M.
     2     OUTFLOW,CMS',/,5X,
     3'---------   -------------------   --------------      -----------
     4')
 1130 FORMAT(6X,F6.2,8X,F13.1,7X,F13.1,10X,F8.2)
 1140 FORMAT(/,10X,'+ GOVERNED BY POWER FUNCTIONS :',/)
 1160 FORMAT(15X,'OUTFLOW = ',F8.2,'*(DEPTH - ',F6.2,')** ',F5.3)
 1165 FORMAT(/,5X,'FLOW FROM SECOND OUTLET IS TO ELEMENT NO.',F8.0)
 1166 FORMAT(/,5X,'FLOW FROM SECOND OUTLET IS TO ELEMENT NO.',A10)
 1180 FORMAT(/,10X,'+ GOVERNED BY PUMPING',/)
 1190 FORMAT(10X,'PUMPED OUTFLOW:',//,15X,'DEPTH AT WHICH FIRST PUMPING
     1RATE BEGINS,FT. : ',F8.2,/,15X,'DEPTH AT WHICH SECOND PUMPING RATE
     2 BEGINS,FT.: ',F8.2,/,15X,'FIRST PUMPING RATE,CFS
     3      : ',F8.2,/,15X,'SECOND PUMPING RATE,CFS
     4 : ',F8.2,/,15X,'DEPTH AT WHICH ALL PUMPING STOPS,FT.         : ',
     5F8.2)
 1195 FORMAT(15X,'PUMPED OUTFLOW:',//,15X,'DEPTH AT WHICH FIRST PUMPING
     1RATE BEGINS,M.  : ',F8.2,/,15X,'DEPTH AT WHICH SECOND PUMPING RATE
     2 BEGINS,M. : ',F8.2,/,15X,'FIRST PUMPING RATE,CU. M/SEC
     3      : ',F8.2,/,15X,'SECOND PUMPING RATE,CU. M/SEC
     4 : ',F8.2,/,15X,'DEPTH AT WHICH ALL PUMPING STOPS,M.          : ',
     5F8.2)
 1205 FORMAT (/,10X,'+ GOVERNED BY OUTFLOWS FROM DATA GROUP G2')
 1210 FORMAT(/,10X,'INITIAL CONDITIONS FOR JUNCTION INPUT #:',I5,/,
     1         15X,'VOLUME,CU.FT.     : ',1PE13.4)
 1220 FORMAT(/,10X,'INITIAL CONDITIONS FOR JUNCTION INPUT #:',I5,/,
     1         15X,'VOLUME,CU.M.      : ',1PE13.4)
 1230 FORMAT(15X,'POLLUTANT CONCENTRATIONS     :')
 1250 FORMAT(20X,A8,',',A8,7X,': ',5X,E8.3)
C#### WCH (CDM), 8/93.
 4040 FORMAT(' ERROR - Looking for G3 group but found ',A2)
 4041 FORMAT(' ERROR - Looking for G4 group but found ',A2)
 4042 FORMAT(' ERROR - Looking for initial conditions on G5 group but ',
     1'found ',A2)
C=======================================================================
      END
