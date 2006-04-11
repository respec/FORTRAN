      SUBROUTINE FILTH
C=======================================================================
C     ROUTINE TO COMPUTE AVERAGE DAILY DRY-WEATHER FLOW QUANTITY
C     AND QUALITY ON THE BASIS OF DIRECT MEASUREMENTS AND
C     DEMOGRAPHIC INFORMATION.
C     NOTE: QUALITY COMPUTATIONS ARE AVAILABLE ONLY FOR:
C           POLLUTANT 1 = BOD-5
C           POLLUTANT 2 = SUSPENDED SOLIDS
C           POLLUTANT 3 = TOTAL COLIFORM
C           IF A FOURTH POLLUTANT IS SIMULATED, IT IS NOT AFFECTED BY
C           SUBROUTINE FILTH.
C
C     UPDATED BY W.C.H., SEPTEMBER 1981.
C     UPDATED BY R.E.D., APRIL, 1988.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      CHARACTER KNPUT*10
C=======================================================================
C     READ DAILY AND HOURLY CORRECTION FACTORS FOR SEWAGE
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP L1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,DVDWF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP L2,L3 <<<<<<<<<<<<
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     READ (N5,*,ERR=888) CC,DVBOD
                     READ (N5,*,ERR=888) CC,DVSS
                     ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP M1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,HVDWF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS M2,M3,M4 <<<<<<<<<<<<
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     READ (N5,*,ERR=888) CC,HVBOD
                     READ (N5,*,ERR=888) CC,HVSS
                     READ (N5,*,ERR=888) CC,HVCOLI
                     ENDIF
C=======================================================================
C     ASSIGN DEFAULT VALUES.
C=======================================================================
      DO 4950 I = 1,7
      IF(DVDWF(I).LE.0.0) DVDWF(I) = 1.0
      IF(DVBOD(I).LE.0.0) DVBOD(I) = 1.0
      IF(DVSS(I).LE.0.0) DVSS(I)   = 1.0
 4950 CONTINUE
      DO 4960 I = 1,24
      IF(HVDWF(I).LE.0.0) HVDWF(I)   = 1.0
      IF(HVBOD(I).LE.0.0) HVBOD(I)   = 1.0
      IF(HVSS(I).LE.0.0) HVSS(I)     = 1.0
      IF(HVCOLI(I).LE.0.0) HVCOLI(I) = 1.0
 4960 CONTINUE
C=======================================================================
C     READ TOTAL NUMBER OF SUBAREAS, TYPE OF FLOW AND QUALITY DATA
C     AVAILABLE, NUMBER OF PROCESS FLOWS, TIME SIMULATION BEGINS( DAY
C     OF WEEK)         , CURRENT VALUE OF THE CONSUMER PRICE INDEX,CPI,
C     CURRENT VALUE OF THE COMPOSITE CONSTRUCTION COST INDEX, CCCI.
C     POPULA IS THE TOTAL POP IN ALL AREAS IN THOUSANDS
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP N1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,KTNUM,KASE,NPF,KDAY,CPI,CCCI,POPULA
      IF(KASE.LE.0) KASE   = 1
      IF(KDAY.LE.0) KDAY   = 1
      IF(KDAY.GT.7) KDAY   = 7
      IF(CPI.LE.0.0) CPI   = 109.5
      IF(CCCI.LE.0.0) CCCI = 103.0
      IF(METRIC.EQ.1) THEN
                      CMET5 = 1.0
                      CMET6 = 1.0
                      ELSE
C=======================================================================
C                     CMET5 = CU M TO GAL.
C                     CMET6 = KG TO LB.
C=======================================================================
                      CMET5 = 264.2
                      CMET6 = 2.2046
                      ENDIF
      WRITE (N6,601)
      WRITE (N6,5030) KTNUM,KASE,NPF,KDAY,CPI,CCCI,POPULA
C=======================================================================
C     COMPUTE TOTAL INFILTRATION IN STUDY AREA.
C     INCLUDE BASE FLOW INPUT AT MANHOLES.
C=======================================================================
      AINFIL    = 0.0
      DO 415 KK = 1,NE
      QMANHO    = 0.0
      IF(NTYPE(KK).EQ.19) QMANHO = DIST(KK)
  415 AINFIL                     = AINFIL + QINFIL(KK) + QMANHO
      IF(KASE.NE.1) ADWF=0.0
C=======================================================================
C     IF KASE = 1 THE AVERAGE FLOW AND CHARACTERISTICS ARE KNOWN FOR THE
C                 ENTIRE AREA BUT NOT FOR THE INDIVIDUAL SUBAREAS
C                 (EG SEWAGE PLANT DATA)
C     IF KASE = 2 NO SUCH DATA ARE AVAILABLE
C
C     ADWF ETC. ARE THE AVE DAILY VALUES OF DRY WEATHER FLOW IN CFS,
C     BOD IN MG/L, SUSP SOLIDS IN MG/L , RESPECTIVELY
C     (FOR EXAMPLE, FROM A TREATMENT PLANT SERVING THE ENTIRE STUDY AREA
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP O1 <<<<<<<<<<<<
C=======================================================================
      IF (KASE.GT.1) GO TO 412
      READ (N5,*,ERR=888) CC,ADWF,ABOD,ASUSO,ACOLI
      IF(METRIC.EQ.1) WRITE (N6,5050) ADWF
      IF(METRIC.EQ.2) WRITE (N6,5051) ADWF
      WRITE (N6,5060) ABOD,ASUSO,ACOLI
      ADWF = ADWF*CMET(8,METRIC)
C=======================================================================
C     ACOLI IS THE VALUE OF TOTAL COLIFORMS IN MPN/100ML.  READ TOTAL
C     AREA TO PLANT, INDUSTRIAL AREA, COMMERCIAL, RESIDENTIAL HIGH
C     INCOME, RESIDENTIAL AVE INCOME, RESIDENTIAL LOW INCOME, RESI-
C     DENTIAL WITH GARBAGE GRINDERS, PARKS AND OPEN AREA -
C     ALL IN ACRES (OR HECTARES).
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP O2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,TOTA,TINA,TCA,TRHA,TRAA,TRLA,TRGGA,TPOA
      IF(METRIC.EQ.1) WRITE (N6,5070)
      IF(METRIC.EQ.2) WRITE (N6,5071)
      WRITE (N6,5080) TOTA,TINA,TCA,TRHA,TRAA,TRLA,TRGGA,TPOA
      TOTA  = TOTA/CMET(2,METRIC)
      TINA  = TINA/CMET(2,METRIC)
      TCA   = TCA/CMET(2,METRIC)
      TRHA  = TRHA/CMET(2,METRIC)
      TRAA  = TRAA/CMET(2,METRIC)
      TRLA  = TRLA/CMET(2,METRIC)
      TRGGA = TRGGA/CMET(2,METRIC)
      TPOA  = TPOA/CMET(2,METRIC)
C=======================================================================
C     COMPUTE TOTAL BOD AND SUSPENDED SOLIDS IN POUNDS PER DAY
C     COMPUTE TOTAL COLIFORMS IN MPN/DAY
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     TOTBOD = (ADWF/1.547)*ABOD*8.34
                     TOTSS  = (ADWF/1.547)*ASUSO*8.34
                     TCOLI  = ACOLI*ADWF*2.447E+7
                     ENDIF
C=======================================================================
C     MAKE DATA CORRECTION FOR INFILTRATION
C=======================================================================
                       C1DWF = ADWF- AINFIL
      IF(C1DWF.LT.0.0) C1DWF = 0.0
C=======================================================================
C     MAKE DATA CORRECTION FOR PROCESS FLOWS
C     FIRST INITIALIZE SUMATION QUANTITIES
C=======================================================================
      SUMQPF = 0.0
      SUMBOD = 0.0
      SUMSS  = 0.0
C=======================================================================
C     READ PROCESS FLOW CHARACTERISTICS AND LOCATIONS
C                      NPF IS NUMBER OF PROCESS FLOWS
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP P1 <<<<<<<<<<<<
C=======================================================================
      IF(NPF.GT.0) THEN
      IF(METRIC.EQ.1) WRITE (N6,5090)
      IF(METRIC.EQ.2) WRITE (N6,5091)
      DO 500 JJ = 1,NPF
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,INPUT,QPF,BODPF,SUSPF
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KNPUT,QPF,BODPF,SUSPF
      IF(JCE.EQ.0) WRITE(N6,5100) INPUT,QPF,BODPF,SUSPF
      IF(JCE.EQ.1) WRITE(N6,5101) KNPUT,QPF,BODPF,SUSPF
      QPF    = QPF*CMET(8,METRIC)
      SUMQPF = SUMQPF + QPF
      IF(NPOLL.GT.0) THEN
                     SUMBOD = SUMBOD +(QPF/1.547)*BODPF*8.34
                     SUMSS  = SUMSS + (QPF/1.547)*SUSPF*8.34
                     ENDIF
  500 CONTINUE
      ENDIF
C
      C2DWF = C1DWF-SUMQPF
      IF(C2DWF.LE.0.0) GO TO 412
      C1BOD = TOTBOD-SUMBOD
      C1SS  = TOTSS-SUMSS
C=======================================================================
C     MAKE FINAL CORRECTIONS TO ALLOW FOR INCOME VARIATIONS, COMMERCIAL
C     USE, GARBAGE GRINDERS, AND POPULATION
C     COMPUTE RESIDENTIAL AND COMMERCIAL AREA CONTRIBUTING TO PLANT
      TDWFA=TOTA-TINA-TPOA
C     COMPUTE WEIGHTED DWFA BASED ON EXPECTED VARIATIONS IN SEWAGE
C     STRENGTH.
      WTDWFA= 0.9*TCA+1.2*TRHA+1.0*TRAA+0.8*TRLA+1.3*TRGGA
C     COMPUTE CORRECTION FACTOR TO WEIGHT SEWAGE STRENGTH - NOTE BOD AND
C     SS ARE AFFECTED EQUALLY
C=======================================================================
      TDTR = TDWFA + TRGGA
      IF(TDTR.LE.0.0) CF = 1.0
      IF(TDTR.GT.0.0) CF = WTDWFA/TDWFA
C=======================================================================
C     MODIFY MEASURED STRENGTHS TO PERMIT WEIGHTING IN SUBAREA COMPS
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     C2BOD = C1BOD/CF
                     C2SS  = C1SS/CF
                     IF(C2BOD.LE.0.0) C2BOD = 0.0
                     IF(C2SS.LE.0.0) C2SS   = 0.0
C=======================================================================
C     A1COLI IS THE AVERAGE NUMBER OF COLI PER CAPITA PER DAY.
C     COMPUTE AVERAGE CORRECTED AND WEIGHTED DWF CHARACTERISTICS IN
C     LBS/DAY/CFS.
C=======================================================================
                     A1BOD  = C2BOD/C2DWF
                     A1SS   = C2SS/C2DWF
                     A1COLI = TCOLI/(POPULA*1000.)
                     GO TO 414
                     ENDIF
C=======================================================================
C     KASE 2 FOLLOWS
C
C     IN THE EVENT MEASURED FLOW CHARACTERISTICS ARE NOT AVAILABLE ASSUME
C     FOR A1BOD,  A1SS, AND A1COLI
C     A1BOD ASSUMES 85 GAL/CAP/DAY AND 0.20LBS/CAP/DAY FOR AVE INCOME
C     A1SS ASSUMES 85 GAL/CAP/DAY AND 0.22 LBS/CAP/DAY FOR AVE INCOME
C     A1COLI,MPN/DAY/CAP, ASSUMES 200 BILLION COLIFORMS PER CAPITA PER DAY
C=======================================================================
  412 IF(NPOLL.GT.0) THEN
                     A1BOD  = 1300.0
                     A1SS   = 1420.0
                     A1COLI = 2.00E+11
                     ENDIF
C=======================================================================
C     ASSUME 100 GAL/CAP/DAY INCLUDING INFILTRATION
C=======================================================================
      ADWF  = POPULA*1000.*1.547*100./1000000.
  414 X1BOD = A1BOD*0.18551
      X1SS  = A1SS*0.18551
      XDWF  = ADWF/CMET(8,METRIC)
      Y1BOD = A1BOD/CMET6*CMET(8,METRIC)
      Y1SS  = A1SS/CMET6*CMET(8,METRIC)
      IF(METRIC.EQ.1) WRITE (N6,5200) XDWF
      IF(METRIC.EQ.2) WRITE (N6,5201) XDWF
      IF(NPOLL.GT.0.AND.METRIC.EQ.1) WRITE (N6,5210) Y1BOD,X1BOD,Y1SS,
     1                               X1SS,A1COLI,ACOLI
      IF(NPOLL.GT.0.AND.METRIC.EQ.2) WRITE (N6,5211) Y1BOD,X1BOD,Y1SS,
     1                               X1SS,A1COLI,ACOLI
      SMMDWF = 0.00
      SMMBOD = 0.00
      SMMSS  = 0.00
      TOTPOP = 0.0
      SMMQQ  = 0.0
      WRITE (N6,5250)
      WRITE (N6,5255)
      IF(METRIC.EQ.1) WRITE (N6,5260)
      IF(METRIC.EQ.2) WRITE (N6,5261)
      WRITE (N6,5270)
      QQ     = 0.0
      QQDWF  = 0.0
      SMTDWF = 0.0
C=======================================================================
C     SUBAREA INPUT AND CALCULATIONS FOLLOW.
C
C     FLOW CALCULATIONS ARE PERFORMED FIRST.
C=======================================================================
      NTEMP    = NSCRAT(1)
      IF(NTEMP.EQ.0) CALL ERROR(50)
      REWIND NTEMP
      DO 300 I = 1,KTNUM
      DWF      = 0.0
C=======================================================================
C     COMPUTE DWF FOR EACH SUBAREA
C     SUBAREAS IN A CITY ARE CHOSEN PRIMARILY ON LAND USE CRITERIA
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP Q1 <<<<<<<<<<<<
C=======================================================================
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,KNUM,INPUT,KLAND,METHOD,
     +             KUNIT,MSUBT,SAGPF,SABPF,SASPF,WATER,PRICE,SEWAGE,
     +             ASUB,POPDEN,DWLNGS,FAMILY,VALUE,PCGG,XINCOM
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KNUM,KNPUT,KLAND,METHOD,
     +             KUNIT,MSUBT,SAGPF,SABPF,SASPF,WATER,PRICE,SEWAGE,
     +             ASUB,POPDEN,DWLNGS,FAMILY,VALUE,PCGG,XINCOM
      IF(KLAND.LE.0)   KLAND = 5
      IF(METHOD.LE.0) METHOD = 2
C=======================================================================
C     DATA CHECK AND ASSUMPTIONS NECESSARY TO OVERCOME MISSING DATA
C     IF HOUSE VALUATION(VALUE) IS UNDEFINED, ASSUME $20,000 HOMES
C=======================================================================
      IF(VALUE.LE.0) VALUE = 20.0
C=======================================================================
C     Print input image.
C=======================================================================
      IF(JCE.EQ.0) WRITE(N6,5300) KNUM,INPUT,KLAND,METHOD,KUNIT,MSUBT,
     +             WATER,PRICE,SEWAGE,ASUB,POPDEN,DWLNGS,FAMILY,VALUE,
     +             PCGG,XINCOM,SAGPF,SABPF,SASPF
      IF(JCE.EQ.1) WRITE(N6,5301) KNUM,KNPUT,KLAND,METHOD,KUNIT,MSUBT,
     +             WATER,PRICE,SEWAGE,ASUB,POPDEN,DWLNGS,FAMILY,VALUE,
     +             PCGG,XINCOM,SAGPF,SABPF,SASPF
C=======================================================================
C     Make necessary METRIC conversions.
C=======================================================================
      IF(KUNIT.EQ.0) WATER = WATER*CMET5
      IF(KUNIT.EQ.1) WATER = WATER*CMET(8,METRIC)
      PRICE  = PRICE/CMET(8,METRIC)
      SEWAGE = SEWAGE*CMET(8,METRIC)
      ASUB   = ASUB*CMET(2,METRIC)
      POPDEN = POPDEN/CMET(2,METRIC)
      SAGPF  = SAGPF*CMET(8,METRIC)
C=======================================================================
C     CHECK ON WHETHER POPULATION DENSITY OR THE NUMBER OF DWELLINGS
C     FOR EACH SUBAREA ARE INCLUDED AS INPUT DATA
C     CORRECT VALUE TO 1960 DOLLARS USING DEPARTMENT OF INTERIOR
C     COMPOSITE CONSTRUCTION COST INDEX(1960 VALUE OF CCCI=103.0)
C=======================================================================
      IF(CCCI.LE.0.0) CCCI = 103.0
      VALUE = VALUE*103.0/CCCI
      IF(DWLNGS.GT.0.0) GO TO 130
      IF(POPDEN.GT.0.0) GO TO 115
C=======================================================================
C     IF BOTH POPULATION DENSITY AND NUMBER OF DWELLINGS PER SUBAREA
C     ARE NOT TABULATED, ASSUME 10 DWELLING UNITS PER ACRE
C=======================================================================
      DWLNGS = 10.0*ASUB
      GO TO 130
C=======================================================================
C     IF POPULATION DENSITY BUT NOT NUMBER PER HOUSEHOLD(FAMILY) IS
C     TABULATED, ASSUME FAMILY=3.0
C=======================================================================
  115 IF(FAMILY.LE.0.0) FAMILY = 3.0
      DWLNGS = POPDEN*ASUB/FAMILY
C=======================================================================
C     METHOD USED TO ESTIMATE D W F WITH MEASURED SEWAGE OR WATER FLOWS
C=======================================================================
  130 IF(SEWAGE.GT.0.0) GO TO 136
      IF(WATER.LE.0.0) GO TO 100
C=======================================================================
C     CHECK UNITS OF WATER DATA AND USE APPROPRIATE CONVERSION FACTOR
C=======================================================================
      IF(KUNIT.LE.0) GO TO 135
      DWF = WATER/(30.4*24.*3600.)*1000.0
      GO TO 200
  135 DWF = WATER/(30.4*24.*3600.)*0.134*1000.
      GO TO 200
  136 DWF = SEWAGE
      GO TO 200
C=======================================================================
C     GO TO APPROPRIATE DWF ESTIMATE DEPENDING ON TYPE OF LAND USE
C     TYPE 1 SINGLE FAMILY RESIDENTIAL
C     TYPE 2 MULTI-FAMILY RESIDENTIAL
C     TYPE 3 COMMERCIAL
C     TYPE 4 INDUSTRIAL
C     TYPE 5 UNDEVELOPED OR PARKLANDS
C=======================================================================
  100 GO TO(10,20,200,200,200),KLAND
C=======================================================================
C     METHOD TO ESTIMATE DWF IN SINGLE FAMILY RESIDENTIAL SUBAREAS
C     CHECK ON WHETHER PRICE OF WATER IS INCLUDED AS INPUT DATA
C     THIS DETERMINES WHICH OF LINAWEAVER'S EQUATION TO USE
C     LINAWEAVER'S EQUATIONS ALSO DEPEND ON WHETHER WATER IS METERED
C     ASSUME FLAT RATE PRICING IF METERING IS NOT INDICATED IN DATA
C=======================================================================
   10 IF(METHOD.NE.1)  GO TO 20
      IF(PRICE.GT.0.0) GO TO 13
C=======================================================================
C     EQUATION 1 LINAWEAVER
C=======================================================================
      DWF = (178.+3.28*VALUE)*DWLNGS*0.134/(24.*3600.)
      GO TO 200
C=======================================================================
C     EQUATION 2 METERED WITH PUBLIC SEWER LINAWEAVER AND HOWE
C     CORRECT PRICE TO 1965 DOLLARS USING THE CONSUMER PRICE INDEX
C     (1965 CPI=109.0)
C=======================================================================
   13 IF(CPI.LE.0.0) CPI = 109.9
      PRICE = PRICE*109.9/CPI
      DWF   = (206.+3.47*VALUE-1.30*PRICE)*DWLNGS*0.134/(24.*3600.)
      GO TO 200
C=======================================================================
C     METHOD TO ESTIMATE DWF IN MULTI-FAMILY RESIDENTIAL SUBAREAS
C     CHECK ON WHETHER VALUE AND FAMILY ARE INCLUDED AS INPUT DATA
C=======================================================================
   20 IF(FAMILY.GT.0.0) GO TO 22
      FAMILY = 3.0
C=======================================================================
C     EQUATION 3 FLAT RATE AND APARTMENTS WITH PUBLIC SEWER L. AND H.
C=======================================================================
   22 DWF = (28.9+4.39*VALUE+33.6*FAMILY)*DWLNGS*0.134/(24.*3600.)
C=======================================================================
C     DWF MODEL REQUIRES WATER OR SEWAGE INPUTS
C     TO ESTIMATE DWF FOR COMMERCIAL OR INDUSTRIAL SUBAREAS
C=======================================================================
  200 DWF    = DWF    + SAGPF
      POP    = ASUB   * POPDEN
      TOTPOP = TOTPOP + POP
      IF(ADWF.LE.0.0) QQF = 0.0
      IF(ADWF.GT.0.0) QQF = AINFIL/ADWF
                       QQ = QQF*DWF
      IF(KLAND.NE.4)   QQ = QQF*DWF
      IF(KLAND.EQ.4)   QQ = 0.0
      QQDWF = DWF + QQ
      IF(DWF.EQ.0.0) THEN
                     DWF = 0.0001
                     WRITE(N6,6999)
                     ENDIF
      IF(NPOLL.LT.1) GO TO 6005
C=======================================================================
C     COMPUTE DWF QUALITY FOR EACH SUBAREA.  DAILY QUALITY AVERAGES
C     ARE CONVERTED TO SUBAREA QUALITY RATES (LBS/SEC).
C=======================================================================
      C1DT = DT/(24.0*60.0*60.0)
      IF(XINCOM.LE.0.0) XINCOM = VALUE/2.5
      IF(KLAND.LE.2) GO TO 421
      IF(KLAND.EQ.3.OR.KLAND.EQ.4) GO TO 422
C=======================================================================
C     OPEN AND PARK AREAS.
C=======================================================================
      DWBOD = DWF*A1BOD*C1DT
      DWSS  = DWF*A1SS*C1DT
      GO TO 25
C=======================================================================
C     COMMERCIAL AND INDUSTRIAL AREAS.
C=======================================================================
  422 DWBOD = DWF*0.9*A1BOD*C1DT
      DWSS  = DWF*0.9*A1SS*C1DT
      GO TO 25
C=======================================================================
C     COMPUTE RESIDENTIAL STRENGTHS ON THE BASIS OF
C     INCOME AND GARBAGE GRINDERS.
C=======================================================================
  421 DWBOD = DWF*A1BOD*C1DT
      DWSS  = DWF*A1SS*C1DT
      IF(XINCOM.GT.15.0) DWBOD = 1.2*DWBOD
      IF(XINCOM.GT.15.0)  DWSS = 1.2*DWSS
      IF(XINCOM.LT.7.) DWBOD   = 0.8*DWBOD
      IF(XINCOM.LT.7.) DWSS    = 0.8*DWSS
      DWBOD = DWBOD+(0.3*PCGG*DWBOD)/100.0
      DWSS  =  DWSS+(0.3*PCGG*DWSS)/100.0
C=======================================================================
C     ADD PROCESS FLOWS FOR ALL LANDUSES.
C=======================================================================
   25 PRBOD  = (SAGPF/1.547)*SABPF*8.34*C1DT
      PRSS   = (SAGPF/1.547)*SASPF*8.34*C1DT
      DWBOD  = DWBOD+PRBOD
      DWSS   = DWSS+PRSS
      DW1BOD = DWBOD*60./DT
      DW1SS  = DWSS*60./DT
 6005 XDWF   = DWF/CMET(8,METRIC)
      XQQ    = QQ/CMET(8,METRIC)
      XQQDWF = QQDWF/CMET(8,METRIC)
      XW1BOD = DW1BOD/CMET6
      XW1SS  = DW1SS/CMET6
      MS     = MSUBT
      IF(NPOLL.GT.0) WRITE(NTEMP) KNUM,MS,XDWF,XQQ,XQQDWF,XW1BOD,XW1SS
      IF(NPOLL.EQ.0) WRITE(NTEMP) KNUM,MS,XDWF,XQQ,XQQDWF
C=======================================================================
C     COMPUTE TOTAL QUANTITIES IN SYSTEM
C=======================================================================
      SMMDWF = SMMDWF + DWF
      SMMQQ  = SMMQQ  + QQ
      SMTDWF = SMMQQ+SMMDWF
      XMMDWF = SMMDWF/CMET(8,METRIC)
      XMMQQ  = SMMQQ/CMET(8,METRIC)
      XMTDWF = SMTDWF/CMET(8,METRIC)
      JNPUT  = NIN(INPUT,KNPUT)
      IF(NPOLL.LT.1) GO TO 4028
      SMMBOD = SMMBOD + DWBOD
      SMMSS  = SMMSS + DWSS
      IF(SMTDWF.LE.0.0) DWCOLI = 0.0
      IF(SMTDWF.GT.0.0) DWCOLI = A1COLI*TOTPOP/(SMTDWF*2.447E+7)
      D2COLI = A1COLI*POP/(60.*24.*60.)
C=======================================================================
C     D2COLI  IS THE TOTAL DWF COLI FROM THE ASUB IN MPN/SEC
C=======================================================================
      WDWF(JNPUT,1)  = (DWBOD/DT) + WDWF(JNPUT,1)
      WDWF(JNPUT,2)  = (DWSS/DT)  + WDWF(JNPUT,2)
      WDWF(JNPUT,3)  = D2COLI     + WDWF(JNPUT,3)
      IF(SMTDWF.GT.1.0E-15) THEN
         BODCON = (SMMBOD*1000000.)/(SMTDWF*DT*7.48*8.34)
         SSCONC = (SMMSS*1000000.)/(SMTDWF*DT*7.48*8.34)
         ENDIF
      XMMBOD = SMMBOD/CMET6
      XMMSS  = SMMSS/CMET6
      WRITE(NTEMP) XMMDWF,XMMQQ,XMTDWF,XMMBOD,XMMSS,TOTPOP,BODCON,
     1             SSCONC,DWCOLI
 4028 CONTINUE
      IF(NPOLL.EQ.0) WRITE(NTEMP) XMMDWF,XMMQQ,XMTDWF
  800 CONTINUE
      QDWF(JNPUT) = QDWF(JNPUT) + DWF
  300 CONTINUE
C=======================================================================
      WRITE(N6,5265)
      IF(NPOLL.LT.1) WRITE(N6,5271)
      IF(NPOLL.GT.0) WRITE(N6,5272)
      IF(NPOLL.EQ.0.AND.METRIC.EQ.1) WRITE(N6,5273)
      IF(NPOLL.EQ.0.AND.METRIC.EQ.2) WRITE(N6,5274)
      IF(NPOLL.GT.0.AND.METRIC.EQ.1) WRITE(N6,5275)
      IF(NPOLL.GT.0.AND.METRIC.EQ.2) WRITE(N6,5276)
      REWIND NTEMP
C=======================================================================
      DO 400 I = 1,KTNUM
      IF(NPOLL.EQ.0) THEN
                     READ(NTEMP)    KNUM,MS,XDWF,XQQ,XQQDWF
                     WRITE(N6,5350) KNUM,XDWF,XQQ,XQQDWF
                     READ(NTEMP)    XMMDWF,XMMQQ,XMTDWF
                     IF(MS.GT.0)    WRITE(N6,5370) XMMDWF,XMMQQ,XMTDWF
                     ELSE
                     READ(NTEMP)    KNUM,MS,XDWF,XQQ,XQQDWF,XW1BOD,XW1SS
                     WRITE(N6,5350) KNUM,XDWF,XQQ,XQQDWF
                     READ(NTEMP) XMMDWF,XMMQQ,XMTDWF,
     +                    XMMBOD,XMMSS,TOTPOP,BODCON,SSCONC,DWCOLI
                     IF(MS.GT.0) WRITE(N6,5370) XMMDWF,XMMQQ,XMTDWF,
     +                    XMMBOD,XMMSS,TOTPOP,BODCON,SSCONC,DWCOLI
                     ENDIF
  400 CONTINUE
C=======================================================================
C     END OF SUBAREA COMPUTATIONS.
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     BODCON = (SMMBOD*1000000.)/(SMTDWF*DT*7.48*8.34)
                     SSCONC = (SMMSS*1000000.)/(SMTDWF*DT*7.48*8.34)
                     XMMBOD = SMMBOD/CMET6
                     XMMSS  = SMMSS/CMET6
                     WRITE (N6,5400) XMMDWF,XMMQQ,XMTDWF,XMMBOD,XMMSS,
     +                               TOTPOP,BODCON,SSCONC,DWCOLI
                     ELSE
                     WRITE (N6,5400) XMMDWF,XMMQQ,XMTDWF
                     ENDIF
      IF(KASE.NE.1.OR.ADWF.EQ.0) THEN
                      CF2 = 1.0
                      ELSE
                      CF2  = ADWF/SMTDWF
                      XDWF = ADWF/CMET(8,METRIC)
                      IF(METRIC.EQ.1) WRITE(N6,5410) XDWF,XMTDWF,CF2
                      IF(METRIC.EQ.2) WRITE(N6,5411) XDWF,XMTDWF,CF2
                      ENDIF
C=======================================================================
C     CORRECTION FACTOR (CF2) APPLIED TO THE DWF (QUANTITY AND QUALITY)
C     WHEN ADWF IS MEASURED (KASE=1)
C
C     ALSO, CONVERT INPUT LOADS TO CFS * CONCENTRATION.
C
C     16028.3 = 453600 MG/LB / 28.3 L/CU FT.
C=======================================================================
  431 DO 432 I = 1,NE
      IF(NPOLL.GT.0) THEN
                     WDWF(I,1) = WDWF(I,1)*CF2*16028.3
                     WDWF(I,2) = WDWF(I,2)*CF2*16028.3
                     WDWF(I,3) = WDWF(I,3)*CF2/28.3
                     ENDIF
  432 QDWF(I)   = QDWF(I)*CF2
C=======================================================================
C     DAILY AND HOURLY CORRECTION FACTORS APPLIED WITHIN TRANSPORT MODEL
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     WRITE(N6,609)
                     WRITE(N6,610) (I,DVDWF(I),DVBOD(I),DVSS(I),I=1,7)
                     WRITE(N6,611)
                     WRITE(N6,612) (I,HVDWF(I),HVBOD(I),HVSS(I),
     1                              HVCOLI(I),I=1,24)
                     ELSE
                     WRITE(N6,6009)
                     WRITE(N6,6100)(I,DVDWF(I),I=1,7)
                     WRITE(N6,611)
                     WRITE(N6,6120)(I,HVDWF(I),I=1,24)
                     ENDIF
      RETURN
  888 CALL IERROR
C=======================================================================
  601 FORMAT(//,1H1,/,
     1' ***********************************************************',/,
     2' * PREDICTION OF DRY-WEATHER FLOW QUANTITY (AND QUALITY IF *',/,
     3' * SIMULATED).  OUTPUT FROM SUBROUTINE FILTH QUALITY, IF   *',/,
     4' * SIMULATED MUST CONSIST OF THE FOLLOWING PARAMETERS:     *',/,
     5' ***********************************************************',//,
     69X,'QUALITY PARAMETER 1 IS BOD-5             MG/L',/,
     79X,'QUALITY PARAMETER 2 IS SUSPENDED SOLIDS  MG/L',/,
     89X,'QUALITY PARAMETER 3 IS TOTAL COLIFORM    MPN/L',/,
     59X,'QUALITY PARAMETER 4 IS USER DEFINED           ',/,
     69X,'AND UNAFFECTED BY SUBROUTINE FILTH.')
 5030 FORMAT(//,
     1' ***************************************************',/,
     1' * OVERALL INFORMATION.  INPUT FROM DATA GROUP N1: *',/,
     1' ***************************************************',//,
     1 ' NUMBER OF SUBAREAS (KTNUM)........................',I9,/,
     2 ' AREA QUALITY DATA USED? 1=YES,2=NO (KASE).........',I9,/,
     3 ' NO. OF PROCESS FLOWS IN STUDY AREA (NPF)..........',I9,/,
     4 ' DAY OF WEEK OF BEGINNING OF SIMULATION (KDAY).....',I9,/,
     5 ' CONSUMER PRICE INDEX (CPI)........................',F9.1,/,
     6 ' COMPOSITE CONSTRUCTION COST INDEX (CCCI)..........',F9.1,/,
     7 ' TOTAL STUDY AREA POPULATION, THOUSANDS (POPULA)...',F9.3)
 5050 FORMAT(//,
     1' *****************************',/,
     2' * AVERAGE STUDY AREA DATA.  *',/,
     3' * INPUT FROM DATA GROUP O1. *',/
     4' *****************************',//,
     5  ' AVE. DWF (ADWF).............',F14.3,' CFS.')
 5051 FORMAT(//,
     1' *****************************',/,
     2' * AVERAGE STUDY AREA DATA.  *',/,
     3' * INPUT FROM DATA GROUP O1. *',/
     4' *****************************',//,
     5       ' AVE. DWF (ADWF).............',F14.3,' CUBIC M/SEC.')
 5060 FORMAT(' AVE. BOD-5 (ABOD)...........',F14.3,' MG/L.',/,
     2       ' AVE. S.S. (ASUSO)...........',F14.3,' MG/L.',/,
     3       ' AVE. TOT. COLIF. (ACOLI)....',1PE14.7,' MPN/100 ML.')
 5070 FORMAT(//,
     1' ************************************************',/,
     2' * CONTRIBUTING AREAS WITHIN STUDY AREA.  INPUT *',/,
     3' * FROM DATA GROUP O2.  ALL AREAS IN ACRES.     *',/
     4' ************************************************',/)
 5071 FORMAT(//,
     1' ************************************************',/,
     2' * CONTRIBUTING AREAS WITHIN STUDY AREA.  INPUT *',/,
     3' * FROM DATA GROUP O2.  ALL AREAS IN HECTARES.  *',/
     4' ************************************************',/)
 5080 FORMAT(
     1 ' TOTAL AREA OF MEASURED QUALITY VALUES (TOTA)........',F10.3,/,
     2 ' CONTRIBUTING INDUSTRIAL AREA (TINA).................',F10.3,/,
     3 ' CONTRIBUTING COMMERCIAL AREA (TCA)..................',F10.3,/,
     4 ' CONTRIBUTING HIGH-INCOME RESIDENTIAL AREA (TRHA)....',F10.3,/,
     5 ' CONTRIBUTING AVE.-INCOME RESIDENTIAL AREA (TRAA)....',F10.3,/,
     6 ' CONTRIBUTING LOW-INCOME RESIDENTIAL AREA (TRLA).....',F10.3,/,
     7 ' AREA WITH GARBAGE GRINDERS (TRGGA)..................',F10.3,/,
     8 ' PARK AND OPEN AREA (TPOA)...........................',F10.3)
 5090 FORMAT(//,
     1' ********************************************************',/,
     2' * PROCESS FLOW INFORMATION.  INPUT FROM DATA GROUP P1. *',/,
     3' ********************************************************',//,
     4'     INPUT  INFLOW   AVERAGE   AVERAGE',/,
     5'      ELE.            BOD-5    SUS. S.',/,
     6'       NO.   (CFS)   (MG/L)    (MG/L)',/,
     7'      ----   -----   ------    ------')
 5091 FORMAT(//,
     1' ********************************************************',/,
     2' * PROCESS FLOW INFORMATION.  INPUT FROM DATA GROUP P1. *',/,
     3' ********************************************************',//,
     4'     INPUT  INFLOW   AVERAGE   AVERAGE',/,
     5'      ELE.            BOD-5    SUS. S.',/,
     6'       NO.  (CM/S)   (MG/L)    (MG/L)',/,
     7'      ----   -----   ------    ------')
 5100 FORMAT(1X,I9,F8.3,F11.3,F10.3)
 5101 FORMAT(1X,A9,F8.3,F11.3,F10.3)
 5200 FORMAT(//,' *********************************************',/,
     2' * CORRECTED AND WEIGHTED DRY-WEATHER FLOW   *',/,
     3' * CHARACTERISTICS FOR THE TOTAL STUDY AREA: *',/,
     4' *********************************************',/,
     5' FLOW',9X,'= ',F10.2,' CFS.')
 5201 FORMAT(//,' *********************************************',/,
     2' * CORRECTED AND WEIGHTED DRY-WEATHER FLOW   *',/,
     3' * CHARACTERISTICS FOR THE TOTAL STUDY AREA: *',/,
     4' *********************************************',/,
     5' FLOW',9X,'= ',F10.2,' CUBIC M/SEC.')
 5210 FORMAT (' BOD-5',8X,'= ',F10.2,' LB/DAY-CFS     = ',F10.3,' MG/L.'
     1,/,' SUSP. SOLIDS = ',F10.2,' LB/DAY-CFS     = ',F10.3,' MG/L.',/,
     2' TOT. COLIF.  = ',1PE10.4,' MPN/DAY-CAPITA = ',
     3  1PE10.4,' MPN/100 ML')
 5211 FORMAT (' BOD-5',8X,'= ',F10.2,' KG/DAY-CMS     = ',F10.3,' MG/L.'
     1,/,' SUSP. SOLIDS = ',F10.2,' KG/DAY-CMS     = ',F10.3,' MG/L.',/,
     2' TOT. COLIF.  = ',1PE10.4,' MPN/DAY-CAPITA = ',
     3  1PE10.4,' MPN/100 ML')
 5250 FORMAT (///,
     1 '**********************************************************',/,
     1 '*    SUBAREA DATA  ECHO OF INPUT LINE Q1.  REFER TO      *',/,
     3 '*    USER''S MANUAL FOR MEANING OF THE INPUT PARAMETERS.  *',/,
     4 '**********************************************************',//)
 5255 FORMAT(' >>> PRINT OF SOME DATA INPUT VALUES MAY BE ALTERED BY DEF
     +AULT VALUES. <<<')
 5260 FORMAT(' >>> NOTE: U.S. CUSTOMARY UNITS BEING USED.
     1             <<<')
 5261 FORMAT(' >>> NOTE: METRIC UNITS BEING USED.
     1             <<<')
 5265 FORMAT(//,
     +10X,' ***********************************',/,
     +10X,' *  OUTPUT OF SUBAREA CALCULATIONS *',/,
     +10X,' ***********************************')
 5270 FORMAT(//,' KNUM INPUT KLAND METHOD KUNIT MSUBT WATER PRICE SEWAGE
     +  ASUB  POPDEN DWLNGS FAMILY VALUE  PCGG XINCOM  SAGPF SABPF SASPF
     +',/,' ---- ----- ----- ------ ----- ----- ----- ----- ------  ----
     +  ------ ------ ------ -----  ---- ------  ----- ----- -----')
 5271 FORMAT(//,   '      KNUM       DWF  +  INFIL  = TOTDWF')
 5272 FORMAT(//,'      KNUM       DWF  +  INFIL  = TOTDWF     BOD-5   SU
     +S.SOL TOTAL-POP   BODCONC    SSCONC   TOTCOLI')
 5273 FORMAT('                 CFS       CFS       CFS',/,
     +       '            ----       ---     -----    ------')
 5274 FORMAT('                CM/S      CM/S      CM/S',/,
     +       '            ----       ---     -----    ------')
 5275 FORMAT('                 CFS       CFS       CFS    LB/MIN    LB/M
     +IN   PERSONS     MG/L      MG/L  MPN/100ML',/,'      ----       --
     +-     -----    ------    ------    ------   -------   -------    -
     +-----  --------')
 5276 FORMAT('                CM/S      CM/S      CM/S    KG/MIN    KG/M
     +IN   PERSONS     MG/L      MG/L  MPN/100ML',/,'      ----       --
     +-     -----    ------    ------    ------   -------   -------    -
     +-----  --------')
 5300 FORMAT(I4,I5,I6,I7,2I6,F8.1,F6.1,F7.1,F6.1,F8.1,2F7.1,
     1       2F6.1,F7.1,F7.2,2F6.1)
 5301 FORMAT(I4,1X,A4,I6,I7,2I6,F8.1,F6.1,F7.1,F6.1,F8.1,2F7.1,
     1       2F6.1,F7.1,F7.2,2F6.1)
 5350 FORMAT(3X,I7,5F10.3)
 5370 FORMAT(' SUBTOTAL ',8F10.3,1PE10.2)
 5400 FORMAT('    TOTAL ',8F10.3,1PE10.2)
 5410 FORMAT (///,' COMPARISON OF MEASURED (ADWF) AND CALCULATED (SMTDWF
     1) TOTAL SEWAGE FLOW:',/,'  ADWF =',F10.3,' CFS. SMTDWF =',1PE10.3,
     2' CFS.',//,' DRY-WEATHER FLOW QUANTITY (AND QUALITY LOAD RATE)',/,
     3' IS MULTIPLIED BY A CORRECTION FACTOR, ADWF/SMTDWF = ',
     41PE10.3,' AT EACH INLET.',//)
 5411 FORMAT (///,' COMPARISON OF MEASURED (ADWF) AND CALCULATED (SMTDWF
     1)',/,' TOTAL SEWAGE FLOW:  ADWF =',F10.3,' CMS. SMTDWF =',1PE10.3,
     2' CFS.',//,' DRY-WEATHER FLOW QUANTITY (AND QUALITY LOAD RATE)',
     3' IS MULTIPLIED',/,' BY A CORRECTION FACTOR, ADWF/SMTDWF = ',
     41PE10.3,' AT EACH INLET.',//)
  609 FORMAT('1',T20,'DAILY AND HOURLY CORRECTION FACTORS FOR SEWAGE',
     1' DATA',//,T16,'DAY',T30,'DVDWF',T40,'DVBOD',T50,'DVSS',T60,
     2'DVCOLI',/,T16,'---',T30,'-----',T40,'-----',T50,'----',
     3 T60,'------')
 6009 FORMAT('1',T5,'DAILY AND HOURLY CORRECTION FACTORS FOR SEWAGE',
     1' DATA',//,T16,'DAY',T30,'DVDWF',/,T16,'---',T30,'-----')
  610 FORMAT(T15,I3,7X,3F10.3)
 6100 FORMAT(T15,I3,7X,F10.3)
  611 FORMAT(/,T15,'HOUR',/,T15,'----')
  612 FORMAT(T15,I3,7X,4F10.3)
 6120 FORMAT(T15,I3,7X,F10.3)
 6999 FORMAT(' ===> WARNING !! DWF FOR WAS 0.0.  SET TO 0.0001 BY',
     +       ' SUBROUTINE FILTH.')
C=======================================================================
      END
