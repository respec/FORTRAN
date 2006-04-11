      SUBROUTINE PRINTQ
C=======================================================================
C     ROUTINE TO PRINT INFLOW AND OUTFLOW HYDROGRAPHS AND
C     AND POLLUTOGRAPHS.  ALSO COMPUTED ARE LOADS AND FIRST TWO MOMENTS.
C     FIRST PROGRAMMED BY KELLY NEAD, AUG. 1980.
C     UPDATED BY W.C.H., SEPT. 1981.
C     UPDATED OCTOBER, 1988
C     UPDATED NOVEMBER 1992 BY WCH TO CORRECT METRIC UNITS 
C       FOR MAX/MIN FLOW
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      DIMENSION TQUALC(10),TQUALS(10),CMEAN(10),STNDEV(10),
     1          TQUALF(10),PP(4),PP1(10),FMAX(10),FMIN(10)
      INTEGER YOO
      CHARACTER*8 FFMT,EFMT,POUT(6),TOUT(20)
      CHARACTER*8 X8,BLNK,UTOT(10),UNTN(5),FMOUT(16)
C=======================================================================
      DATA FFMT /'1PG10.3,'/
      DATA POUT/'(4X,I2,''','/'',I2,''/',''',I2,4X,','I2,1X,I4',
     +          ',I6,    ','1X)     '/
      DATA EFMT /'1PE10.3,'/
      DATA BLNK /'        '/,X8/'10X,    '/
      DATA UNTN /'    KG  ','TOT QUAN',' Q*C*DT ','CUBIC-FT',' CUBIC-M'/
      DATA TOUT/'('' FLOW ','WTD MEAN','S.......',''', 7X,  ',
     +          '('' FLOW ','WTD STD ','DEVS....',''', 7X,  ',
     +          '('' TOTAL',' LOADS..','........',''', 7X,  ',
     +          '('' MAXIM','UM VALUE','........',''', 7X,  ',
     +          '('' MINIM','UM VALUE','........',''', 7X,  '/
C=======================================================================
C     CFACT1 CONVERTS FROM CFS * MG/L * DT TO KG.
C     CFACT2 CONVERTS FROM CFS * CONC * DT TO 'TOTAL QUANTITY'
C=======================================================================
      CFACT1 = 28.316/1.E6
      CFACT2 = 28.316
      II     = 0
    5 II     = II+1
      IF(JPRINT.EQ.2) II = 2
      IF(II.EQ.3.AND.JPRINT.EQ.1) GO TO 600
C=======================================================================
C     II = 1 READING INFLOWS
C     II = 2 READING OUTFLOWS
C=======================================================================
      NTX = NSCRAT(II)
      REWIND NTX
      DO 7 I = 1,NPOLL
    7 PP(I)  = 0.0
      IF(NNYN.LT.NTH) NYN(NNYN+1) = 0
      IF(NNPE.LT.NTH) NPE(NNPE+1) = 0
      NVAL = 7
      NFLW = 6
      NOUT = 6 + NPOLL
      YOO  = NPOLL + 1
      XNDT = FLOAT(NDT)
C=======================================================================
C     CONVERSION FROM CU M TO CU FT.
C=======================================================================
                      CMET3 = 1.0
      IF(METRIC.EQ.2) CMET3 = 35.31
C=======================================================================
C     INITIALIZE VARIABLES FOR UNITS OF TOTAL LOADS
C=======================================================================
      DO 10 I     = 1,10
      UTOT(I)     = BLNK
   10 CONTINUE
C=======================================================================
C     BUILD CHARACTER STRING FOR UNITS OF TOTAL LOADS
C=======================================================================
      IF(METRIC.EQ.1) THEN
                      UTOT(1)    = UNTN(4)
                      UTOT(NFLW) = UNTN(4)
                      ELSE
                      UTOT(1)    = UNTN(5)
                      UTOT(NFLW) = UNTN(5)
                      ENDIF
      M       = 1
      DO 14 I = 1,NPOLL
      M       = M + 1
      IF(NDIM(I).EQ.0) THEN
                       UTOT(M)   = UNTN(1)
                       UTOT(M+5) = UNTN(1)
                       ENDIF
      IF(NDIM(I).EQ.1) THEN
                       UTOT(M)   = UNTN(2)
                       UTOT(M+5) = UNTN(2)
                       ENDIF
      IF(NDIM(I).EQ.2) THEN
                       UTOT(M)   = UNTN(3)
                       UTOT(M+5) = UNTN(3)
                       ENDIF
   14 CONTINUE
C=======================================================================
C     INITIALIZE DYNAMIC FORMAT STATEMENT ARRAYS
C=======================================================================
      DO 4 K   = 1,16
      FMOUT(K) = X8
    4 CONTINUE
C=======================================================================
C     BUILD FORMAT STATEMENT FOR EXTERNAL ELEMENTS
C=======================================================================
      FMOUT(16) = POUT(6)
      DO 24 I   = 1,NPOLL
      IF(NDIM(I).EQ.1) THEN
                       FMOUT(I+6)  = EFMT
                       FMOUT(I+11) = EFMT
                       ELSE
                       FMOUT(I+6)  = FFMT
                       FMOUT(I+11) = FFMT
                       ENDIF
   24 CONTINUE
C=======================================================================
C     MAIN LOOP
C=======================================================================
      NFLG   = 0
      IWHERE = 1
      IF(II.EQ.2) NNYN = NNPE
      DO 100 JJ = 1,NNYN,2
      REWIND NTX
      DO 3 K    = 1,5
   3  FMOUT(K)  = POUT(K)
      FMOUT(6)  = FFMT
      FMOUT(11) = FFMT
      JULDAY    = IDATEZ
      TIMDAY    = TZERO
C=======================================================================
C     INITIALIZE VARIABLES FOR STATISTICAL COMPUTATIONS
C=======================================================================
      DO 8 N    = 1,10
      TQUALC(N) = 0.0
      TQUALF(N) = 0.0
      TQUALS(N) = 0.0
      CMEAN(N)  = 0.0
      STNDEV(N) = 0.0
      FMAX(N)   = 0.0
      FMIN(N)   = 1.0E20
    8 CONTINUE
C=======================================================================
C     PRINT HEADINGS
C=======================================================================
      IF(II.EQ.1) THEN
                  WRITE(N6,300)
                  WRITE(N6,900) TITLE(1),TITLE(2)
                  IF(JJ.LT.NNYN) THEN
                     IF(JCE.EQ.0) WRITE(N6,302)  NYN(JJ),NYN(JJ+1)
                     IF(JCE.EQ.1) WRITE(N6,1302) KYN(JJ),KYN(JJ+1)
                     WRITE(N6,303) (PNAME(K),K=1,4),(PNAME(K),K=1,4)
                     IF(METRIC.EQ.1) WRITE(N6,305)
     +                         (PUNIT(K),K=1,4),(PUNIT(K),K=1,4)
                     IF(METRIC.EQ.2) WRITE(N6,405)
     +                         (PUNIT(K),K=1,4),(PUNIT(K),K=1,4)
                     ELSE
                     IF(JCE.EQ.0) WRITE(N6,602)  NYN(JJ)
                     IF(JCE.EQ.1) WRITE(N6,1602) KYN(JJ)
                     WRITE(N6,603) (PNAME(K),K=1,4)
                     IF(METRIC.EQ.1) WRITE(N6,605) (PUNIT(K),K=1,4)
                     IF(METRIC.EQ.2) WRITE(N6,705) (PUNIT(K),K=1,4)
                     ENDIF
                  ELSE
                  WRITE(N6,301)
                  WRITE(N6,900) TITLE(1),TITLE(2)
                  IF(JJ.LT.NNYN) THEN
                    IF(JCE.EQ.0) WRITE(N6,302)  NPE(JJ),NPE(JJ+1)
                    IF(JCE.EQ.1) WRITE(N6,1302) KPE(JJ),KPE(JJ+1)
                    WRITE(N6,304) (PNAME(K),K=1,4),(PNAME(K),K=1,4)
                    IF(METRIC.EQ.1) WRITE(N6,305)
     +                        (PUNIT(K),K=1,4),(PUNIT(K),K=1,4)
                    IF(METRIC.EQ.2) WRITE(N6,405)
     +                        (PUNIT(K),K=1,4),(PUNIT(K),K=1,4)
                    ELSE
                    IF(JCE.EQ.0) WRITE(N6,602)  NPE(JJ)
                    IF(JCE.EQ.1) WRITE(N6,1602) KPE(JJ)
                    WRITE(N6,604) (PNAME(K),K=1,4)
                    IF(METRIC.EQ.1) WRITE(N6,605) (PUNIT(K),K=1,4)
                    IF(METRIC.EQ.2) WRITE(N6,705) (PUNIT(K),K=1,4)
                    ENDIF
                  ENDIF
C=======================================================================
C     INNER LOOP
C=======================================================================
      KPR      = 0
      DO 99 KK = 1,NDT
      KPR      = KPR+1
      IF(KPR.GT.INTPRT) KPR = 1
C=======================================================================
C     INCREMENT DATE AND TIME
C=======================================================================
      CALL STIME(DT)
      CALL DATED
      IUPTO  = 0
C=======================================================================
C     READ UP TO DESIRED EXT. ELEMENT
C=======================================================================
      IF(IWHERE.GT.1) THEN
   30                 IUPTO = IUPTO + 1
                      READ(NTX) QQ,(PP(I),I=1,NPOLL)
                      IF(IUPTO.LT.IWHERE-1) GO TO 30
                      ENDIF
      IF(JJ.EQ.NNYN) GO TO 40
      IF(NFLG.EQ.1)  GO TO 40
C=======================================================================
C     READ DESIRED EXTERNAL ELEMENT INFORMATION
C=======================================================================
      READ(NTX) QQ,(PP(I),I=1,NPOLL)
C=======================================================================
C     SUMS FOR LATER STATISTICAL COMPUTATIONS
C=======================================================================
      TQUALF(1) = TQUALF(1) + QQ
      TQUALS(1) = TQUALS(1) + QQ**2
      IF(QQ.GT.FMAX(1)) FMAX(1) = QQ
      IF(QQ.LT.FMIN(1)) FMIN(1) = QQ
      DO 32 L   = 1,NPOLL
      IF(PP(L).GT.FMAX(L+1)) FMAX(L+1) = PP(L)
      IF(PP(L).LT.FMIN(L+1)) FMIN(L+1) = PP(L)
      TQUALF(L+1) = TQUALF(L+1) + PP(L)     * QQ
      TQUALS(L+1) = TQUALS(L+1) + PP(L)** 2 * QQ
   32 CONTINUE
C=======================================================================
C     READ DESIRED EX. ELEMENT INFO FOR 2ND PRINTED PER/PAGE
C=======================================================================
      READ(NTX,END=98) QQ1,(PP1(I),I=1,NPOLL)
C=======================================================================
C     SUMS FOR LATER STATISTICAL COMPUTATIONS
C=======================================================================
      TQUALF(NFLW) = TQUALF(NFLW) + QQ1
      TQUALS(NFLW) = TQUALS(NFLW) + QQ1**2
      IF(QQ1.GT.FMAX(NFLW)) FMAX(NFLW) = QQ1
      IF(QQ1.LT.FMIN(NFLW)) FMIN(NFLW) = QQ1
      INDX      = 0
      DO 34 I   = NVAL,NOUT
      INDX      = INDX+1
      IF(PP1(INDX).GT.FMAX(INDX+NFLW)) FMAX(INDX+NFLW) = PP1(INDX)
      IF(PP1(INDX).LT.FMIN(INDX+NFLW)) FMIN(INDX+NFLW) = PP1(INDX)
      TQUALF(I) = TQUALF(I) + PP1(INDX)    * QQ1
      TQUALS(I) = TQUALS(I) + PP1(INDX)**2 * QQ1
   34 CONTINUE
      QQ  = QQ/CMET3
      QQ1 = QQ1/CMET3
      IF(KPR.EQ.INTPRT) WRITE(N6,FMOUT) MONTH,NDAY,NYEAR,JHR,
     +                  MINUTE,KK,QQ,(PP(I),I=1,NPOLL),
     +                            QQ1,(PP1(I),I=1,NPOLL)
      GO TO 45
C=======================================================================
C     READ DESIRED EX. ELEMENT INFO WHEN ONLY ONE IS PRINTED PER/PAGE.
C=======================================================================
   40 READ(NTX,END=98) QQ,(PP(I),I=1,NPOLL)
C=======================================================================
C     SUMS FOR LATER STATISTICAL COMPUTATIONS
C=======================================================================
      TQUALF(1) = TQUALF(1) + QQ
      TQUALS(1) = TQUALS(1) + QQ**2
      IF(QQ.GT.FMAX(1)) FMAX(1) = QQ
      IF(QQ.LT.FMIN(1)) FMIN(1) = QQ
      DO 41 L   = 1,NPOLL
      IF(PP(L).GT.FMAX(L+1)) FMAX(L+1) = PP(L)
      IF(PP(L).LT.FMIN(L+1)) FMIN(L+1) = PP(L)
      TQUALF(L+1) = TQUALF(L+1) + PP(L)    * QQ
      TQUALS(L+1) = TQUALS(L+1) + PP(L)**2 * QQ
   41 CONTINUE
C=======================================================================
      QQ = QQ/CMET3
      IF(KPR.EQ.INTPRT) WRITE(N6,FMOUT) MONTH,NDAY,NYEAR,JHR,
     +                  MINUTE,KK,QQ,(PP(I),I=1,NPOLL)
C=======================================================================
C     READ REMAINING EX. ELE. INFO FOR CURRENT TIME STEP
C=======================================================================
   45 IWEAR2  = (NNYN - (JJ+1))
      DO 46 K = 1,IWEAR2
   46 READ(NTX,END=98) QQ,(PP(I),I=1,NPOLL)
   99 CONTINUE
C=======================================================================
C     COMPUTATIONS FOR STATISTICAL INFORMATION
C=======================================================================
   98 IF (JJ+2.GE.NNYN) NFLG = 1
      IWHERE       = IWHERE + 2
      TQUALC(1)    = TQUALF(1)    * DT
      TQUALC(NFLW) = TQUALF(NFLW) * DT
      DO 50 I      = 1,NPOLL
      IF(NDIM(I).EQ.0) TQUALC(I+1) = DT * TQUALF(I+1) * CFACT1
      IF(NDIM(I).EQ.1) TQUALC(I+1) = DT * TQUALF(I+1) * CFACT2
      IF(NDIM(I).EQ.2) TQUALC(I+1) = DT * TQUALF(I+1)
   50 CONTINUE
C=======================================================================
      CMEAN(1)  = TQUALF(1)  /  XNDT
      ARG       = (TQUALS(1) - (XNDT * CMEAN(1) ** 2)) / (XNDT - 1.0)
                     STNDEV(1) = 0.0
      IF(ARG.GT.0.0) STNDEV(1) = SQRT(ARG)
      DO 56 I   = 2,NPOLL+1
      IF(TQUALF(1).LE.0.0) GO TO 56
      CMEAN(I)  = TQUALF(I)/TQUALF(1)
      ARG       = TQUALS(I)/TQUALF(1) - CMEAN(I)**2
      STNDEV(I) = 0.0
      IF(ARG.GT.0.0) STNDEV(I) = SQRT(ARG)
   56 CONTINUE
      IF(NNYN.EQ.JJ) GO TO 101
      INDX    = 0
      DO 51 I = NVAL,NOUT
      INDX    = INDX + 1
      IF(NDIM(INDX).EQ.0) TQUALC(I) = DT * TQUALF(I) * CFACT1
      IF(NDIM(INDX).EQ.1) TQUALC(I) = DT * TQUALF(I) * CFACT2
      IF(NDIM(INDX).EQ.2) TQUALC(I) = DT * TQUALF(I)
   51 CONTINUE
C=======================================================================
      CMEAN(NFLW)  =  TQUALF(NFLW) / XNDT
      ARG          = (TQUALS(NFLW) - (XNDT*CMEAN(NFLW)**2))/(XNDT-1.0)
                     STNDEV(NFLW) = 0.0
      IF(ARG.GT.0.0) STNDEV(NFLW) = SQRT(ARG)
      DO 57 I   = NVAL,NOUT
      IF(TQUALF(NFLW).LE.0.0) GO TO 57
      CMEAN(I)  = TQUALF(I) / TQUALF(NFLW)
      ARG       = TQUALS(I) / TQUALF(NFLW) - CMEAN(I)**2
      STNDEV(I) = 0.0
      IF(ARG.GT.0.0) STNDEV(I) = SQRT(ARG)
   57 CONTINUE
C=======================================================================
C     PRINT STATISTICAL INFO FOR 2 EXTERNAL ELEMENTS ON A PAGE
C=======================================================================
      CMEAN(1)     = CMEAN(1)/CMET3
      CMEAN(NFLW)  = CMEAN(6)/CMET3
      STNDEV(1)    = STNDEV(1)/CMET3
      STNDEV(NFLW) = STNDEV(NFLW)/CMET3
      TQUALC(1)    = TQUALC(1)/CMET3
      TQUALC(NFLW) = TQUALC(NFLW)/CMET3
C###### WCH, 11/92
      FMAX(1)      = FMAX(1)/CMET3
      FMAX(NFLW)   = FMAX(NFLW)/CMET3
      FMIN(1)      = FMIN(1)/CMET3
      FMIN(NFLW)   = FMIN(NFLW)/CMET3
C######
      FMOUT(1)     = TOUT(1)
      FMOUT(2)     = TOUT(2)
      FMOUT(3)     = TOUT(3)
      FMOUT(4)     = TOUT(4)
      FMOUT(5)     = BLNK
      WRITE(N6,505)
      WRITE(N6,FMOUT) (CMEAN(I),I=1,YOO),(CMEAN(I),I=6,NOUT)
      FMOUT(1) = TOUT(5)
      FMOUT(2) = TOUT(6)
      FMOUT(3) = TOUT(7)
      FMOUT(4) = TOUT(8)
      WRITE(N6,FMOUT) (STNDEV(I),I=1,YOO),(STNDEV(I),I=6,NOUT)
      FMOUT(1) = TOUT(13)
      FMOUT(2) = TOUT(14)
      FMOUT(3) = TOUT(15)
      FMOUT(4) = TOUT(16)
      WRITE(N6,FMOUT) (FMAX(I),I=1,YOO),(FMAX(I),I=6,NOUT)
      FMOUT(1) = TOUT(17)
      FMOUT(2) = TOUT(18)
      FMOUT(3) = TOUT(19)
      FMOUT(4) = TOUT(20)
      WRITE(N6,FMOUT) (FMIN(I),I=1,YOO),(FMIN(I),I=6,NOUT)
      FMOUT(1)  = TOUT(9)
      FMOUT(2)  = TOUT(10)
      FMOUT(3)  = TOUT(11)
      FMOUT(4)  = TOUT(12)
      FMOUT(6)  = EFMT
      FMOUT(11) = EFMT
      WRITE(N6,FMOUT) (TQUALC(I),I=1,YOO),(TQUALC(I),I=6,NOUT)
      WRITE(N6,410)   (UTOT(I),I=1,NOUT)
      GO TO 100
C=======================================================================
C     PRINT STATISTICAL INFO FOR ONE EX. ELEMENT ON A PAGE
C=======================================================================
  101 CMEAN(1)  = CMEAN(1)/CMET3
      STNDEV(1) = STNDEV(1)/CMET3
      TQUALC(1) = TQUALC(1)/CMET3
C#### WCH, 11/92
      FMAX(1)   = FMAX(1)/CMET3
      FMIN(1)   = FMIN(1)/CMET3
C####
      FMOUT(1)  = TOUT(1)
      FMOUT(2)  = TOUT(2)
      FMOUT(3)  = TOUT(3)
      FMOUT(4)  = TOUT(4)
      FMOUT(5)  = BLNK
      WRITE(N6,805)
      WRITE(N6,FMOUT) (CMEAN(I),I=1,YOO)
      FMOUT(1) = TOUT(5)
      FMOUT(2) = TOUT(6)
      FMOUT(3) = TOUT(7)
      FMOUT(4) = TOUT(8)
      WRITE(N6,FMOUT) (STNDEV(I),I=1,YOO)
      FMOUT(1) = TOUT(13)
      FMOUT(2) = TOUT(14)
      FMOUT(3) = TOUT(15)
      FMOUT(4) = TOUT(16)
      WRITE(N6,FMOUT) (FMAX(I),I=1,YOO)
      FMOUT(1) = TOUT(17)
      FMOUT(2) = TOUT(18)
      FMOUT(3) = TOUT(19)
      FMOUT(4) = TOUT(20)
      WRITE(N6,FMOUT) (FMIN(I),I=1,YOO)
      FMOUT(1) = TOUT(9)
      FMOUT(2) = TOUT(10)
      FMOUT(3) = TOUT(11)
      FMOUT(4) = TOUT(12)
      FMOUT(6) = EFMT
      WRITE(N6,FMOUT) (TQUALC(I),I=1,YOO)
      WRITE(N6,410)   (UTOT(I),I=1,YOO)
  100 CONTINUE
      IF (II.EQ.1) GO TO 5
  600 RETURN
C=======================================================================
 300  FORMAT(/,1H1,/,10X,
     +' *******************************************************',/,10X,
     +' *      SELECTED INLET HYDROGRAPHS AND POLLUTOGRAPHS   *',/,10X,
     +' *******************************************************')
 301  FORMAT(/,1H1,/,10X,
     +' *******************************************************',/,10X,
     +' *     SELECTED OUTFLOW HYDROGRAPHS AND POLLUTOGRAPHS  *',/,10X,
     +' *******************************************************')
 302  FORMAT(/,31X,'>>>>>> EXTERNAL ELEMENT NUMBER ',I8,' <<<<<<',T85,
     1'>>>>>> EXTERNAL ELEMENT NUMBER ',I8,' <<<<<<')
1302  FORMAT(/,31X,'>>>>>> EXTERNAL ELEMENT NUMBER ',A8,' <<<<<<',T85,
     1'>>>>>> EXTERNAL ELEMENT NUMBER ',A8,' <<<<<<')
 303  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',4X,'INFLOW ',4(2X,A8),
     1 3X,'INFLOW ',4(2X,A8))
 304  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',3X,'OUTFLOW ',4(2X,A8),
     1 2X,'OUTFLOW ',4(2X,A8))
 305  FORMAT(5X,'MO/DA/YR',4X,'HR:MIN',3X,'STEP',5X,'CFS   ',4(2X,A8),
     +       4X,'CFS   ',4(2X,A8),/,
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------   -------  --------  --------  --------  --------')
 405  FORMAT(5X,'MO/DA/YR',4X,'HR:MIN',3X,'STEP','  CU M/SEC ',
     +       4(2X,A8),' CU M/SEC ',4(2X,A8),/,
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------   -------  --------  --------  --------  --------')
 410  FORMAT(29X,10(2X,A8))
 505  FORMAT(
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------   -------  --------  --------  --------  --------')
 602  FORMAT(/,21X,'>>>>>>>> EXTERNAL ELEMENT NUMBER ',I9,' <<<<<<<<')
1602  FORMAT(/,21X,'>>>>>>>> EXTERNAL ELEMENT NUMBER ',A10,' <<<<<<<')
 603  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',4X,'INFLOW ',4(2X,A8))
 604  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',3X,'OUTFLOW ',4(2X,A8))
 605  FORMAT(5X,'MO/DA/YR',4X,'HR:MIN',3X,'STEP',5X,'CFS   ',4(2X,A8),/,
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------')
 705  FORMAT(5X,'MO/DA/YR',4X,'HR:MIN',3X,'STEP','  CU M/SEC ',
     +       4(2X,A8),/,
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------')
 805  FORMAT(
     +' -------------  -------  ----  --------  --------  --------  ----
     +----  --------')
 900  FORMAT(/,11X,A80,/,11X,A80)
C=======================================================================
      END
