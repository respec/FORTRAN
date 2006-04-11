      SUBROUTINE PRINTQ
C	TRANSPORT BLOCK
C	CALLED BY TRANS NEAR LINE 891
C=======================================================================
C     ROUTINE TO PRINT INFLOW AND OUTFLOW HYDROGRAPHS AND
C     AND POLLUTOGRAPHS.  ALSO COMPUTED ARE LOADS AND FIRST TWO MOMENTS.
C     FIRST PROGRAMMED BY KELLY NEAD, AUG. 1980.
C     UPDATED BY W.C.H., SEPT. 1981.
C     UPDATED OCTOBER, 1988
C     UPDATED NOVEMBER 1992 BY WCH TO CORRECT METRIC UNITS
C       FOR MAX/MIN FLOW
C     WCH, 10/14/94.  CORRECT IF-STMT LOGIC FOR MINOR NON-ERROR
C       (BECAUSE CAUGHT IN LATER DO 100 LOOP) FOR CASE JPRINT = 1
C       (PRINT ONLY INFLOWS) AND II = 2 (SECOND PASS TO PRINT OUTFLOWS).
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
CIMT CHANGE THESE DIMENSIONS FROM 10 TO MQUAL+1, ELIMINATE PP1
      DIMENSION TQUALC(MQUAL+1),TQUALS(MQUAL+1),CMEAN(MQUAL+1),
     1          STNDEV(MQUAL+1),TQUALF(MQUAL+1),PP(MQUAL),
     2          FMAX(MQUAL+1),FMIN(MQUAL+1)
      INTEGER YOO
CIMX  ELIMINATE TWO PER PAGE FORMAT
      CHARACTER*8 FFMT,EFMT,POUT(6),TOUT(20)
      CHARACTER*8 X8,BLNK,UTOT(MQUAL+1),UNTN(5),FMOUT(7+MQUAL)
C=======================================================================
      DATA FFMT /'1PG10.3,'/
      DATA POUT/'(4X,I2,''','/'',I2,''/',''',I4,2X,','I2,1X,I4',
     +          ',I6,4X, ',')       '/
      DATA EFMT /'1PE10.3,'/
      DATA BLNK /'        '/,X8/'10X,    '/
      DATA UNTN /'    KG  ','TOT QUAN',' Q*C*DT ','CUBIC-FT',' CUBIC-M'/
      DATA TOUT/'('' FLOW ','WTD MEAN','S.......',''',11X,  ',
     +          '('' FLOW ','WTD STD ','DEVS....',''',11X,  ',
     +          '('' TOTAL',' LOADS..','........',''', 8X,  ',
     +          '('' MAXIM','UM VALUE','........',''',11X,  ',
     +          '('' MINIM','UM VALUE','........',''',11X,  '/
C=======================================================================
C     CFACT1 CONVERTS FROM CFS * MG/L * DT TO KG.
C     CFACT2 CONVERTS FROM CFS * CONC * DT TO 'TOTAL QUANTITY'
C=======================================================================
      CFACT1 = 28.316/1.E6
      CFACT2 = 28.316
      II     = 0
    5 II     = II+1
      IF(JPRINT.EQ.2) II = 2
C#### WCH, 10/14/94.  SHOULD BE II.GE.2, NOT II.EQ.3
      IF(II.GE.2.AND.JPRINT.EQ.1) GO TO 600
C=======================================================================
C     II = 1 READING INFLOWS
C     II = 2 READING OUTFLOWS
C=======================================================================
      NTX = NSCRAT(II)
      REWIND NTX
      DO 7 I = 1,NPOLL
    7 PP(I)  = 0.0
      IF(NNYN.LT.NTOA) NYN(NNYN+1) = 0
      IF(NNPE.LT.NTOA) NPE(NNPE+1) = 0
CIMX      NVAL = 7
CIMX      NFLW = 6
CIMX      NOUT = 6 + NPOLL
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
CIM CHANGE 10 TO MQUAL+1
      DO 10 I     = 1,MQUAL+1
      UTOT(I)     = BLNK
   10 CONTINUE
C=======================================================================
C     BUILD CHARACTER STRING FOR UNITS OF TOTAL LOADS
C=======================================================================
      IF(METRIC.EQ.1) THEN
                      UTOT(1)    = UNTN(4)
CIMX                      UTOT(NFLW) = UNTN(4)
                      ELSE
                      UTOT(1)    = UNTN(5)
CIMX                      UTOT(NFLW) = UNTN(5)
                      ENDIF
      M       = 1
      DO 14 I = 1,NPOLL
      M       = M + 1
      IF(NDIM(I).EQ.0) THEN
                       UTOT(M)   = UNTN(1)
CIMX                       UTOT(M+5) = UNTN(1)
                       ENDIF
      IF(NDIM(I).EQ.1) THEN
                       UTOT(M)   = UNTN(2)
CIMX                       UTOT(M+5) = UNTN(2)
                       ENDIF
      IF(NDIM(I).EQ.2) THEN
                       UTOT(M)   = UNTN(3)
CIMX                       UTOT(M+5) = UNTN(3)
                       ENDIF
   14 CONTINUE
C=======================================================================
C     INITIALIZE DYNAMIC FORMAT STATEMENT ARRAYS
C=======================================================================
      DO 4 K   = 1,MQUAL+7
      FMOUT(K) = X8
    4 CONTINUE
C=======================================================================
C     BUILD FORMAT STATEMENT FOR EXTERNAL ELEMENTS
C=======================================================================
      FMOUT(NPOLL+7) = POUT(6)
      DO 24 I   = 1,NPOLL
      IF(NDIM(I).EQ.1) THEN
                       FMOUT(I+6)  = EFMT
cimx                       FMOUT(I+11) = EFMT
                       ELSE
                       FMOUT(I+6)  = FFMT
cimx                       FMOUT(I+11) = FFMT
                       ENDIF
   24 CONTINUE
C=======================================================================
C     MAIN LOOP
C=======================================================================
      NFLG   = 0
      IWHERE = 1
      IF(II.EQ.2) NNYN = NNPE
CIMX      DO 100 JJ = 1,NNYN,2
      DO 100 JJ = 1,NNYN
      REWIND NTX
      DO 3 K    = 1,5
   3  FMOUT(K)  = POUT(K)
      FMOUT(6)  = FFMT
cimx      FMOUT(11) = FFMT
      JULDAY    = IDATEZ
      TIMDAY    = TZERO
C=======================================================================
C     INITIALIZE VARIABLES FOR STATISTICAL COMPUTATIONS
C=======================================================================
CIMT  Change from 10 to MQUAL+1
      DO 8 N    = 1,MQUAL+1
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
C=======================================================================
C     HERE, PRINT INFLOW DATA.
C=======================================================================
                  WRITE(N6,300)
                  WRITE(N6,900) TITLE(1),TITLE(2)
CIMX                  IF(JJ.LT.NNYN) THEN
CIMX                     IF(JCE.EQ.0) WRITE(N6,302)  NYN(JJ),NYN(JJ+1)
CIMX                     IF(JCE.EQ.1) WRITE(N6,1302) KYN(JJ),KYN(JJ+1)
CIMXChange 4 to NPOLL next few lines
CIMX                     WRITE(N6,303) (PNAME(K),K=1,NPOLL),
CIMX     1                             (PNAME(K),K=1,NPOLL)
CIMX                     IF(METRIC.EQ.1) WRITE(N6,305)
CIMX     1                         (PUNIT(K),K=1,npoll),
CIMX     2                         (PUNIT(K),K=1,npoll)
CIMX                     IF(METRIC.EQ.2) WRITE(N6,405)
CIMX     +                         (PUNIT(K),K=1,4),(PUNIT(K),K=1,NPOLL)
CIMX                     ELSE
                     IF(JCE.EQ.0) WRITE(N6,602)  NYN(JJ)
                     IF(JCE.EQ.1) WRITE(N6,1602) KYN(JJ)
CIMT  change 4 to npoll  next 3 lines
                     WRITE(N6,603) (PNAME(K),K=1,NPOLL)
                     IF(METRIC.EQ.1) WRITE(N6,605) (PUNIT(K),K=1,NPOLL)
                     IF(METRIC.EQ.2) WRITE(N6,705) (PUNIT(K),K=1,NPOLL)
                     write(n6,805) ('  --------',k=1,npoll+1)
CIMX                     ENDIF
                  ELSE
C=======================================================================
C     HERE, PRINT OUTFLOW DATA.  NOTE, NNYN = NNPE.
C=======================================================================
                  WRITE(N6,301)
                  WRITE(N6,900) TITLE(1),TITLE(2)
CIMX                  IF(JJ.LT.NNYN) THEN
CIMX                    IF(JCE.EQ.0) WRITE(N6,302)  NPE(JJ),NPE(JJ+1)
CIMX                    IF(JCE.EQ.1) WRITE(N6,1302) KPE(JJ),KPE(JJ+1)
CIMXChange 4 to NPOLL NEXT FEW LINES
CIMX                    WRITE(N6,304) (PNAME(K),K=1,NPOLL),
CIMX     1                            (PNAME(K),K=1,NPOLL)
CIMX                    IF(METRIC.EQ.1) WRITE(N6,305)
CIMX     +                        (PUNIT(K),K=1,NPOLL),
CIMX     1                        (PUNIT(K),K=1,NPOLL)
CIMX                    IF(METRIC.EQ.2) WRITE(N6,405)
CIMX     +                        (PUNIT(K),K=1,NPOLL),
CIMX     1                        (PUNIT(K),K=1,NPOLL)
CIMX                    ELSE
                    IF(JCE.EQ.0) WRITE(N6,602)  NPE(JJ)
                    IF(JCE.EQ.1) WRITE(N6,1602) KPE(JJ)
Change 4 to Npoll next few lines
                    WRITE(N6,604) (PNAME(K),K=1,NPOLL)
                    IF(METRIC.EQ.1) WRITE(N6,605) (PUNIT(K),K=1,NPOLL)
                    IF(METRIC.EQ.2) WRITE(N6,705) (PUNIT(K),K=1,NPOLL)
                     write(n6,805) ('  --------',k=1,npoll+1)
CIMX                    ENDIF
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
CIMX ALWAYS GO TO 40
CIMX      IF(JJ.EQ.NNYN) GO TO 40
CIMX      IF(NFLG.EQ.1)  GO TO 40
CIMXC=======================================================================
CIMXC     READ DESIRED EXTERNAL ELEMENT INFORMATION
CIMXC=======================================================================
CIMX      READ(NTX) QQ,(PP(I),I=1,NPOLL)
CIMXC=======================================================================
CIMXC     SUMS FOR LATER STATISTICAL COMPUTATIONS
CIMXC=======================================================================
CIMX      TQUALF(1) = TQUALF(1) + QQ
CIMX      TQUALS(1) = TQUALS(1) + QQ**2
CIMX      IF(QQ.GT.FMAX(1)) FMAX(1) = QQ
CIMX      IF(QQ.LT.FMIN(1)) FMIN(1) = QQ
CIMX      DO 32 L   = 1,NPOLL
CIMX      IF(PP(L).GT.FMAX(L+1)) FMAX(L+1) = PP(L)
CIMX      IF(PP(L).LT.FMIN(L+1)) FMIN(L+1) = PP(L)
CIMX      TQUALF(L+1) = TQUALF(L+1) + PP(L)     * QQ
CIMX      TQUALS(L+1) = TQUALS(L+1) + PP(L)** 2 * QQ
CIMX   32 CONTINUE
CIMXC=======================================================================
CIMXC     READ DESIRED EX. ELEMENT INFO FOR 2ND PRINTED PER/PAGE
CIMXC=======================================================================
CIMX      READ(NTX,END=98) QQ1,(PP1(I),I=1,NPOLL)
CIMXC=======================================================================
CIMXC     SUMS FOR LATER STATISTICAL COMPUTATIONS
CIMXC=======================================================================
CIMX      TQUALF(NFLW) = TQUALF(NFLW) + QQ1
CIMX      TQUALS(NFLW) = TQUALS(NFLW) + QQ1**2
CIMX      IF(QQ1.GT.FMAX(NFLW)) FMAX(NFLW) = QQ1
CIMX      IF(QQ1.LT.FMIN(NFLW)) FMIN(NFLW) = QQ1
CIMX      INDX      = 0
CIMX      DO 34 I   = NVAL,NOUT
CIMX      INDX      = INDX+1
CIMX      IF(PP1(INDX).GT.FMAX(INDX+NFLW)) FMAX(INDX+NFLW) = PP1(INDX)
CIMX      IF(PP1(INDX).LT.FMIN(INDX+NFLW)) FMIN(INDX+NFLW) = PP1(INDX)
CIMX      TQUALF(I) = TQUALF(I) + PP1(INDX)    * QQ1
CIMX      TQUALS(I) = TQUALS(I) + PP1(INDX)**2 * QQ1
CIMX   34 CONTINUE
CIMX      QQ  = QQ/CMET3
CIMX      QQ1 = QQ1/CMET3
CIMX      IF(KPR.EQ.INTPRT) WRITE(N6,FMOUT) MONTH,NDAY,NYEAR,JHR,
CIMX     +                  MINUTE,KK,QQ,(PP(I),I=1,NPOLL),
CIMX     +                            QQ1,(PP1(I),I=1,NPOLL)
CIMX      GO TO 45
CIMXC=======================================================================
CIMXC     READ DESIRED EX. ELEMENT INFO WHEN ONLY ONE IS PRINTED PER/PAGE.
CIMXC=======================================================================
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
CIMX   45 IWEAR2  = (NNYN - (JJ+1))
   45 IWEAR2  = (NNYN - JJ)
      DO 46 K = 1,IWEAR2
   46 READ(NTX,END=98) QQ,(PP(I),I=1,NPOLL)
   99 CONTINUE
C=======================================================================
C     COMPUTATIONS FOR STATISTICAL INFORMATION
C=======================================================================
CIMX   98 IF (JJ+2.GE.NNYN) NFLG = 1
   98 IF (JJ+1.GE.NNYN) NFLG = 1
CIMX      IWHERE       = IWHERE + 2
      IWHERE       = IWHERE + 1
      TQUALC(1)    = TQUALF(1)    * DT
CIMX      TQUALC(NFLW) = TQUALF(NFLW) * DT
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
CIMX      IF(NNYN.EQ.JJ) GO TO 101
CIMX      INDX    = 0
CIMX      DO 51 I = NVAL,NOUT
CIMX      INDX    = INDX + 1
CIMX      IF(NDIM(INDX).EQ.0) TQUALC(I) = DT * TQUALF(I) * CFACT1
CIMX      IF(NDIM(INDX).EQ.1) TQUALC(I) = DT * TQUALF(I) * CFACT2
CIMX      IF(NDIM(INDX).EQ.2) TQUALC(I) = DT * TQUALF(I)
CIMX   51 CONTINUE
CIMXC=======================================================================
CIMX      CMEAN(NFLW)  =  TQUALF(NFLW) / XNDT
CIMX      ARG          = (TQUALS(NFLW) - (XNDT*CMEAN(NFLW)**2))/(XNDT-1.0)
CIMX                     STNDEV(NFLW) = 0.0
CIMX      IF(ARG.GT.0.0) STNDEV(NFLW) = SQRT(ARG)
CIMX      DO 57 I   = NVAL,NOUT
CIMX      IF(TQUALF(NFLW).LE.0.0) GO TO 57
CIMX      CMEAN(I)  = TQUALF(I) / TQUALF(NFLW)
CIMX      ARG       = TQUALS(I) / TQUALF(NFLW) - CMEAN(I)**2
CIMX      STNDEV(I) = 0.0
CIMX      IF(ARG.GT.0.0) STNDEV(I) = SQRT(ARG)
CIMX   57 CONTINUE
CIMXC=======================================================================
CIMXC     PRINT STATISTICAL INFO FOR 2 EXTERNAL ELEMENTS ON A PAGE
CIMXC=======================================================================
CIMX      CMEAN(1)     = CMEAN(1)/CMET3
CIMX      CMEAN(NFLW)  = CMEAN(6)/CMET3
CIMX      STNDEV(1)    = STNDEV(1)/CMET3
CIMX      STNDEV(NFLW) = STNDEV(NFLW)/CMET3
CIMX      TQUALC(1)    = TQUALC(1)/CMET3
CIMX      TQUALC(NFLW) = TQUALC(NFLW)/CMET3
CIMXC###### WCH, 11/92
CIMX      FMAX(1)      = FMAX(1)/CMET3
CIMX      FMAX(NFLW)   = FMAX(NFLW)/CMET3
CIMX      FMIN(1)      = FMIN(1)/CMET3
CIMX      FMIN(NFLW)   = FMIN(NFLW)/CMET3
CIMXC######
CIMX      FMOUT(1)     = TOUT(1)
CIMX      FMOUT(2)     = TOUT(2)
CIMX      FMOUT(3)     = TOUT(3)
CIMX      FMOUT(4)     = TOUT(4)
CIMX      FMOUT(5)     = BLNK
CIMX      WRITE(N6,505)
CIMX      WRITE(N6,FMOUT) (CMEAN(I),I=1,YOO),(CMEAN(I),I=6,NOUT)
CIMX      FMOUT(1) = TOUT(5)
CIMX      FMOUT(2) = TOUT(6)
CIMX      FMOUT(3) = TOUT(7)
CIMX      FMOUT(4) = TOUT(8)
CIMX      WRITE(N6,FMOUT) (STNDEV(I),I=1,YOO),(STNDEV(I),I=6,NOUT)
CIMX      FMOUT(1) = TOUT(13)
CIMX      FMOUT(2) = TOUT(14)
CIMX      FMOUT(3) = TOUT(15)
CIMX      FMOUT(4) = TOUT(16)
CIMX      WRITE(N6,FMOUT) (FMAX(I),I=1,YOO),(FMAX(I),I=6,NOUT)
CIMX      FMOUT(1) = TOUT(17)
CIMX      FMOUT(2) = TOUT(18)
CIMX      FMOUT(3) = TOUT(19)
CIMX      FMOUT(4) = TOUT(20)
CIMX      WRITE(N6,FMOUT) (FMIN(I),I=1,YOO),(FMIN(I),I=6,NOUT)
CIMX      FMOUT(1)  = TOUT(9)
CIMX      FMOUT(2)  = TOUT(10)
CIMX      FMOUT(3)  = TOUT(11)
CIMX      FMOUT(4)  = TOUT(12)
CIMX      FMOUT(6)  = EFMT
CIMX      FMOUT(11) = EFMT
CIMX      WRITE(N6,FMOUT) (TQUALC(I),I=1,YOO),(TQUALC(I),I=6,NOUT)
CIMX      WRITE(N6,410)   (UTOT(I),I=1,NOUT)
CIMX      GO TO 100
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
CIMX
      WRITE(N6,805) (' -------- ',I=1,NPOLL+1)
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
CIMT CHANGE 4( to 99( in next few lines
CIMX 303  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',4X,'INFLOW ',99(2X,A8),
CIMX     1 3X,'INFLOW ',99(2X,A8))
CIMX 304  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',3X,'OUTFLOW ',99(2X,A8),
CIMX     1 2X,'OUTFLOW ',99(2X,A8))
CIMX 305  FORMAT(5X,'MO/DA/YEAR',2X,'HR:MIN',3X,'STEP',5X,'CFS   ',
CIMX     1      99(2X,A8),
CIMX     +       4X,'CFS   ',99(2X,A8),/,
CIMX     +' -------------  -------  ----  --------  --------  --------  ----
CIMX     +----  --------   -------  --------  --------  --------  --------')
CIMX 405  FORMAT(5X,'MO/DA/YEAR',2X,'HR:MIN',3X,'STEP','  CU M/SEC ',
CIMX     +       99(2X,A8),' CU M/SEC ',99(2X,A8),/,
CIMX     +' -------------  -------  ----  --------  --------  --------  ----
CIMX     +----  --------   -------  --------  --------  --------  --------')
 410  FORMAT(29X,99(2X,A8))
CIMX 505  FORMAT(
CIMX     +' -------------  -------  ----  --------  --------  --------  ----
CIMX     +----  --------   -------  --------  --------  --------  --------')
 602  FORMAT(/,21X,'>>>>>>>> EXTERNAL ELEMENT NUMBER ',I10,' <<<<<<<')
1602  FORMAT(/,21X,'>>>>>>>> EXTERNAL ELEMENT NUMBER ',A10,' <<<<<<<')
 603  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',4X,'INFLOW ',99(2X,A8))
 604  FORMAT(/7X,'DATE',7X,'TIME',4X,'TIME',3X,'OUTFLOW ',99(2X,A8))
 605  FORMAT(5X,'MO/DA/YEAR',2X,'HR:MIN',3X,'STEP',5X,'CFS   ',
     1       99(2X,A8))
CIMX     +/,
CIMX     +' -------------  -------  ----  --------  --------  --------  ----
CIMX     +----  --------')
 805  FORMAT('  -------------  -------  ----',99(A10))
 705  FORMAT(5X,'MO/DA/YEAR',2X,'HR:MIN',3X,'STEP','  CU M/SEC ',
     +       99(2X,A8))
CIMX     +' -------------  -------  ----  --------  --------  --------  ----
CIMX     +----  --------')
 900  FORMAT(/,11X,A80,/,11X,A80)
C=======================================================================
      END
