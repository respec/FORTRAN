      SUBROUTINE INTRAN
C=======================================================================
C     University of Florida transport model.
C     This large subroutine performs input of Transport data.
C     Updated May 1992 to use Subroutine GETCUR.
C     Updated June 1993 by RED to read natural channel data correctly.
C     Modified by WCH (CDM - Chuck Moore), 8/93 for new Type 26 flow
C     divider based on a tabular flow split.  
C     Fix metric conversion and eliminate one JSTOP, 9/23/93. WCH (RED).
C     Add additional parameter for WASP linkage, James L. Martin, 
C       AScI Corp., 10/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'NEWTR.INC'
      INCLUDE 'FSPLIT.INC'
C=======================================================================
      DIMENSION QI(NET),QO(NET),QO1(NET),QO2(NET),NDUM(4)
      CHARACTER PNONE*8,PNDUM(4)*8,PUDUM(4)*8,BLANK*8,KNEED*10,
     +          BMJ*10,KSURF(30)*8
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      DATA BLANK/'        '/
      DATA PNONE/'  NONE  '/
C=======================================================================
C     Initialization.
C=======================================================================
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
      LAST   = JIN(INCNT)
      NEXT   = JOUT(IOUTCT)
      SOURCE = 'TRANSPORT BLOCK'
      KPASS  = 0
      KDAY   = 1
C#### WCH (JLM), 10/93
      ISUMRY = 0
      IF(NOQ.EQ.0) THEN
                   WRITE(N6,10)
                   WRITE(*,10)
                   ELSE
                   WRITE(N6,11)
                   WRITE(*,11)
                   ENDIF
C=======================================================================
C     Open all scratch files for the Transport Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') O
     +PEN(NSCRAT(1),FILE=FFNAME(51),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).NE.'SCRT2.UF') O
     +PEN(NSCRAT(2),FILE=FFNAME(52),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).NE.'SCRT3.UF') O
     +PEN(NSCRAT(3),FILE=FFNAME(53),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).NE.'SCRT4.UF') O
     +PEN(NSCRAT(4),FILE=FFNAME(54),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).NE.'SCRT5.UF') O
     +PEN(NSCRAT(5),FILE=FFNAME(55),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).NE.'SCRT6.UF') O
     +PEN(NSCRAT(6),FILE=FFNAME(56),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).NE.'SCRT7.UF') O
     +PEN(NSCRAT(7),FILE=FFNAME(57),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF')
     +             OPEN(NSCRAT(1),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).EQ.'SCRT2.UF')
     +             OPEN(NSCRAT(2),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +             OPEN(NSCRAT(3),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).EQ.'SCRT4.UF')
     +             OPEN(NSCRAT(4),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).EQ.'SCRT5.UF')
     +             OPEN(NSCRAT(5),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).EQ.'SCRT6.UF')
     +             OPEN(NSCRAT(6),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).EQ.'SCRT7.UF')
     +             OPEN(NSCRAT(7),FORM='UNFORMATTED',STATUS='SCRATCH')
C#######################################################################
C     WCH, 10/93.  CAUTION!  NSCRAT(8) WILL BE OPENED IN SUBROUTINE
C       LINK IF WASP LINKAGE IS USED.
C=======================================================================
C     READ IN DATA FOR EACH SEWER SYSTEM
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      WRITE(N6,907)         TITLE(1),TITLE(2)
C=======================================================================
C>>>>>>>>>>>> READ OPTIONAL PARAMETERS ON DATA GROUP B0 <<<<<<<<<<<<
C=======================================================================
      ISLOPE = 0
      ITRAP  = 0
      IFLIP  = 0
      INFLEW = 0
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B0') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,ISLOPE,ITRAP,IFLIP,INFLEW
                     IF(ISLOPE.LT.0) ISLOPE = 0
                     IF(ITRAP.LT.0)  ITRAP  = 0
                     IF(IFLIP.LT.0)  IFLIP  = 0
                     IF(INFLEW.LT.0) INFLEW = 0
                     IF(ISLOPE.GT.1) ISLOPE = 0
                     IF(ITRAP.GT.1)  ITRAP  = 0
                     IF(IFLIP.GT.1)  IFLIP  = 0
                     IF(INFLEW.GT.1) INFLEW = 0
                     ELSE
                     BACKSPACE N5
                     ENDIF
      WRITE(N6,908)  ISLOPE,ITRAP,IFLIP,INFLEW
C=======================================================================
C>>>>>>>>>>>> READ EXECUTION DATA ON DATA GROUP B1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NDT,NINPUT,NNYN,NNPE,NOUTS,NPRINT,NPOLL,
     1                   NITER,JDATEZ,METRIC,INTPRT
C=======================================================================
C             NDT    = NUMBER OF TIME STEPS
C             NINPUT = NUMBER OF INPUT ELEMENTS
C             NNYN   = NUMBER OF ELEMENTS FOR WHICH INFLOW
C                      PRINTOUT IS DESIRED
C             NNPE   = NUMBER OF ELEMENTS FOR WHICH OUTFLOW
C                      PRINTOUT IS DESIRED
C             INTPRT = NUMBER OF TIME STEPS BETWEEN INFLOW AND
C                       OUTFLOW PRINTS.
C             NOUTS  = NUMBER OF ELEMENTS FOR WHICH OUTFLOW
C                      HYDROGRAPHS AND POLLUTOGRAPHS ARE TO BE
C                      PROVIDED ON TAPE FOR INTERFACING
C             NPRINT = 0 SOME INTERNAL ERROR MESSAGES SUPPRESSED
C                      1 PRINT ALL INTERNAL ERROR MESSAGES
C             NPOLL  = NUMBER OF POLLUTANTS TO BE ROUTED
C             NITER  = NUMBER OF ITERATIONS IN ROUTING SCHEME
C             JDATEZ = INITIAL DATE, YR-MO-DAY
C             METRIC = 0 USE U.S. CUSTOMARY UNITS FOR I/O
C                      1 USE METRIC UNITS FOR I/O
C                      NOTE: METRIC = METRIC+1 LATER IN CODE.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
C=======================================================================
C
C             DT     = LENGTH OF TIME STEP IN SECONDS
C             EPSIL  = CONVERGENCE CRITERION IN ROUTING
C             DWDAYS = NUMBER OF DRY WEATHER DAYS PRIOR TO STORM
C             TZERO  = STARTING TIME OF DAY OF STORM, DECIMAL HOUR
C             GNU    = KINEMATIC VISCOCITY OF WATER.
C             TRIBA  = CATCHMENT AREA
C=======================================================================
C     ASSIGN DEFAULT VALUES
C=======================================================================
      IF(NITER.LE.0)    NITER = 4
      IF(EPSIL.LE.0.0)  EPSIL = 0.0001
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B3 <<<<<<<<<<<<
C=======================================================================
C
      READ (N5,*,ERR=888) CC,NCNTRL,NINFIL,NFILTH,NDESN
C=======================================================================
C             NCNTRL = 0  INTERFACE FILE ACCESSED (CARD INPUT OPTIONAL)
C                      1 INPUT FROM CARDS ONLY
C             NINFIL = 1 IF INFILTRATION ROUTINE IS TO BE CALLED
C                      0 IF INFITRATION ROUTINE IS NOT TO BE CALLED
C             NFILTH = 1 IF DRY WEATHER FLOW ROUTINE IS TO BE CALLED
C                      0 IF DRY WEATHER FLOW ROUTINE IS NOT TO BE CALLED
C             NDESN  = 1 IF DESIGN ROUTINE IS TO BE CALLED
C                      0 IF DESIGN ROUTINE NOT CALLED
C=======================================================================
C     PRINT INPUT VARIABLES
C=======================================================================
      WRITE(N6,950) NDT,NINPUT,NNYN,NNPE,INTPRT,NOUTS,NPOLL,NITER
      IF((NOUTS.GT.0.AND.NEXT.LE.0).OR.(NOUTS.LE.0.AND.NEXT.GT.0))
     1                    WRITE (N6,3005) NOUTS,NEXT
      WRITE (N6,951) NPRINT,NCNTRL,NINFIL,NFILTH,NDESN
      IF(METRIC.EQ.0) WRITE (N6,3010)
      IF(METRIC.EQ.1) WRITE (N6,3015)
C=======================================================================
C     ESTABLISH VARIOUS METRIC CONVERSION FACTORS.
C     MULTIPLY FIRST UNITS TO OBTAIN SECOND UNITS.
C=======================================================================
      METRIC = METRIC + 1
      IF(METRIC.EQ.1) THEN
                      CMET1 = 1.0
                      CMET2 = 1.0
                      CMET3 = 1.0
                      CMET4 = 1.0
                      CMET5 = 1.0
                      ELSE
C             M TO FT.
                      CMET1 = 3.281
C             SQ M TO SQ FT.
                      CMET2 = 10.764
C             CU M TO CU FT.
                      CMET3 = 35.31
C             SQ CM TO SQ FT.
                      CMET4 = 0.001076
C             HA TO AC.
                      CMET5 = 2.471
                      ENDIF
      IF(GNU.LE.0.0.AND.METRIC.EQ.1) GNU = 0.00001
      IF(GNU.LE.0.0.AND.METRIC.EQ.2) GNU = 0.01
      IF(METRIC.EQ.1) WRITE (N6,952) DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
      IF(METRIC.EQ.2) WRITE (N6,953) DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
C=======================================================================
C     READ DATA DESCRIBING USER SUPPLIED CONDUITS, IF ANY.
C=======================================================================
C     READ DATA GROUP C1
C=======================================================================
      READ (N5,*,ERR=888) CC,NKLASS,KPRINT
      NKLAS  = NKLASS + 16
      IF(NKLASS.GT.2) CALL ERROR(118)
      IF(NKLASS.LE.0)  GO TO 41
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(NAME(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(NN(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D3 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(MM(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D4 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(ALFMAX(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D5 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(PSIMAX(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D6 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(AFACT(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D7 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(RFACT(I),I=17,NKLAS)
C=======================================================================
C     READ PARAMENTERS FOR DEPTH-AREA RELATIONSHIPS.
C=======================================================================
      DO 18 I   = 17,NKLAS
      KDEPTH(I) = 2
      NNN       = NN(I)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D8 <<<<<<<<<<<<
C=======================================================================
      DO 19 K = 1,NNN,8
      NN1     = K + 7
      IF(NN1.GT.NNN) NN1 = NNN
      READ(N5,*,ERR=888) CC,(DNORM(I,J),J=K,NN1)
   19 CONTINUE
      NNN = NNN+1
      IF(NNN.LE.50) THEN
                    DO 17 J    = NNN,51
   17               DNORM(I,J) = 0.0
                    ENDIF
   18 CONTINUE
C=======================================================================
C     READ PARAMETERS AND PERFORM INITIALIZING CALCULATIONS ON Q-A CURVE
C=======================================================================
      DO 40 I  = 17,NKLAS
      KLASS(I) = 2
C=======================================================================
C     CONDUIT WITH TABULAR Q-A RELATIONSHIP
C=======================================================================
      MMM = MM(I)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D9 <<<<<<<<<<<<
C=======================================================================
      DO 21 K = 1,MMM,8
      MM1     = K + 7
      IF(MM1.GT.MMM) MM1 = MMM
      READ(N5,*,ERR=888) CC,(QNORM(I,J),J=K,MM1)
   21 CONTINUE
      DALPHA     = 1.0/FLOAT(MMM-1)
      ANORM(1,1) = 0.0
      DO 33 J    = 2,MMM
   33 ANORM(I,J) = ANORM(I,J-1)+DALPHA
      MMM        = MMM+1
      IF (MMM.LE.50) THEN
                     DO 39 J = MMM,51
                     ANORM(I,J) = 0.0
   39                QNORM(I,J)=0.0
                     ENDIF
   40 CONTINUE
   41 CONTINUE
C=======================================================================
C     WRITE DATA DESCRIBING DIFFERENT TYPES OF SEWER ELEMENTS(IF DESIRED)
C=======================================================================
      IF(KPRINT.EQ.1) THEN
                WRITE(N6,970)
                DO 43 I = 1,18
                MMM     = MM(I)
                IF(MM(I).LT.NN(I)) MMM = NN(I)
   43           WRITE(N6,971) I,NAME(I),ALFMAX(I),PSIMAX(I),AFACT(I),
     1                        RFACT(I),KDEPTH(I),KLASS(I),
     2                 (J,ANORM(I,J),QNORM(I,J),DNORM(I,J),J=1,MMM)
                WRITE(N6,972)
                WRITE(N6,973)  (I,KDEPTH(I),KLASS(I),NAME(I),I=19,25)
                ENDIF
C=======================================================================
C     SET DEFAULT POLLUTANT NAMES AND UNITS.
C=======================================================================
      GNU        = GNU*CMET4
      DO 3090 K  = 1,4
      CPINF(K)   = 0.0
      PNAME(K) = PNONE
 3090 PUNIT(K) = BLANK
C=======================================================================
C     BYPASS TAPE OPERATIONS IF DESIRED
C=======================================================================
      IF(NCNTRL.GT.0.AND.LAST.GT.0) WRITE(N6,5080)
      IF(NCNTRL.GT.0) GO TO 57
      IF(LAST.LE.0) WRITE (N6,3095) LAST
C=======================================================================
C     INTERFACING MECHANISM FOR QUANTITY AND QUALITY OF RUNOFF
C     READ HEADING INFORMATION ON INPUT FILE.
C=======================================================================
      KPASS = 1
      CALL INFACE(1,LAST)
C=======================================================================
C     TEST FOR ADEQUATE ARRAY SPACE FOR FILE INPUT.
C=======================================================================
      IF(LOCATS.GT.NTH) THEN
                        WRITE (N6,4007)
                        STOP
                        ENDIF
  57  IF(KPASS.EQ.0) THEN
                     NYEAR = JDATEZ/10000
                     NDAY  = JDATEZ - NYEAR*10000
                     MONTH = NDAY/100
                     NDAY  = NDAY - MONTH*100
                     IF(NDAY.LE.0) NDAY   = 2
                     IF(MONTH.LE.0) MONTH = 8
                     IF(NYEAR.LE.0) NYEAR = 41
                     JULDAY  = 1000*NYEAR + JDATE(NDAY,MONTH,NYEAR)
                     IDATEZ  = JULDAY
                     TZERO   = TZERO*3600.0
                     ELSE
                     JULDAY  = IDATEZ
                     ENDIF
      TIMDAY = TZERO
      CALL DATED
      WRITE(N6,966) NYEAR+1900,MONTH,NDAY,JHR,MINUTE,JSEC
C=======================================================================
C     Read in element data and initialize variables.
C=======================================================================
      I = 1
      KSTOR = 0
C#### WCH (CDM), 8/93.
      KINOU = 0
      NSTOR = NTSE
C=======================================================================
C     SET INITIAL DEFAULT VALUES AND RATIOS.
C     USER MUST SUPPLY ANY DEFAULT VALUES FOR PHYSICAL PARAMETERS.
C=======================================================================
      K         = 0
      NTD       = 0
      DO 4010 I = 1,6
      DEF(I)    = 0.0
 4010 RATIO(I)  = 1.0
      JSTOP     = 0
      WRITE(*,5050)
      DO 4070 I = 1,2*NET
      K         = K + 1
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'E1') THEN
                     IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NOE(K),
     +                      (NUE(K,J),J=1,3),NTIPE,(VAR(J),J=1,5),
     +                                           BARREL(K),VAR(6)
                     IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KOE(K),
     +                      (KUE(K,J),J=1,3),NTIPE,(VAR(J),J=1,5),
     +                                       BARREL(K),VAR(6),KGEOM(K)
                     ELSE
                     GO TO 4080
                     ENDIF
C=======================================================================
C     SET NEW DEFAULT VALUES(-2) AND NEW RATIOS(-1)
C=======================================================================
      IF(JCE.EQ.0)       THEN
      IF(NOE(K).EQ.-2)   THEN
                         IF(NTIPE.GT.0) NTD = NTIPE
                         DO 4015 L = 1,6
                         IF(VAR(L).GT.0.0) DEF(L) = VAR(L)
 4015                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
      IF(NOE(K).EQ.-1)   THEN
                         DO 4025 L = 1,6
                         IF(VAR(L).GT.0.0) RATIO(L) = VAR(L)
 4025                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
                         ENDIF
      IF(JCE.EQ.1)       THEN
      IF(KOE(K).EQ.'-2') THEN
                         IF(NTIPE.GT.0) NTD = NTIPE
                         DO 8015 L = 1,6
                         IF(VAR(L).GT.0.0) DEF(L) = VAR(L)
 8015                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
      IF(KOE(K).EQ.'-1') THEN
                         DO 8025 L = 1,6
                         IF(VAR(L).GT.0.0) RATIO(L) = VAR(L)
 8025                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
                         ENDIF
C=======================================================================
C     ASSIGN DEFAULT VALUES AND MULTIPLY BY RATIOS.
C=======================================================================
      IF(NTIPE.EQ.0) NTIPE     = NTD
      IF(NTIPE.LT.0) THEN
                     NTIPE     = IABS(NTIPE)
                     NODSGN(K) = 1
                     ENDIF
      NTYPE(K) = NTIPE
      IF(NTIPE.GT.16.AND.BARREL(K).LT.1.0) BARREL(K) = 1.0
      IF(NTIPE.LT.14.AND.BARREL(K).LE.0.0) BARREL(K) = 1.0
      SCF(K)   = GNO
      IF(VAR(1).LT.0.0) THEN
                        VAR(1)  = ABS(VAR(1))
                        SCF(K)  = YES
                        ENDIF
      DO 4045 L = 1,6
      IF(VAR(L).LE.0.0) VAR(L) = DEF(L)
                        VAR(L) = VAR(L)*RATIO(L)
 4045 CONTINUE
C=======================================================================
C     CONVERT TO FT-SEC UNITS. CONVERT BACK LATER FOR PRINTING ONLY.
C=======================================================================
      NT    = NTYPE(K)
      CMETT = 1.0
      IF(METRIC.EQ.2.AND.NTYPE(K).LE.18) CMETT = CMET1
C#### WCH (RED), 9/23/93. 
      IF(METRIC.EQ.2.AND.NTYPE(K).EQ.23) CMETT = CMET3
      DIST(K)  = VAR(1)*CMETT
      GEOM1(K) = VAR(2)*CMETT
      SLOPE(K) = VAR(3)
      ROUGH(K) = VAR(4)
      GEOM2(K) = VAR(5)*CMETT
      IF(NTYPE(K).NE.13) GEOM3(K) = VAR(6)*CMETT
      IF(NTYPE(K).EQ.13.AND.ITRAP.EQ.0) GEOM3(K) = VAR(6)
      IF(NTYPE(K).EQ.13.AND.ITRAP.EQ.1) GEOM3(K) = 1.0/VAR(6)
      IF(NT.EQ.19.OR.NT.EQ.20.OR.NT.EQ.23) DIST(K) = DIST(K)*CMET3
      IF(NT.EQ.20.OR.NT.EQ.21)            GEOM1(K) = GEOM1(K)*CMET3
      IF(NT.EQ.23) THEN
                   GEOM1(K) = GEOM1(K)*CMET1
                   SLOPE(K) = SLOPE(K)*CMET3
                   ROUGH(K) = ROUGH(K)*CMET1
                   GEOM2(K) = GEOM2(K)*CMET1
                   ENDIF
      KSTORE(K) = 0
      IF(NTYPE(K).EQ.22) THEN
                         KSTOR     = KSTOR + 1
                         KSTORE(K) = KSTOR
                         ENDIF
C#### WCH (CDM), 8/93.  NEW VARIABLES FOR TYPE 26 FLOW DIVIDER.
      KINOUT(K) = 0
      IF(NTYPE(K).EQ.26) THEN
                         KINOU      = KINOU + 1
                         KINOUT(K) = KINOU
                         ENDIF
      QFULL(K) = 0.0
      QMAX(K)  = 0.0
      AFULL(K) = 0.0
      IF(NTYPE(K).LE.18.AND.SLOPE(K).LE.0.0) THEN
                        JSTOP = 1
                        IF(JCE.EQ.0) WRITE(N6,9500) NOE(K)
                        IF(JCE.EQ.1) WRITE(N6,9501) KOE(K)
                        ENDIF
      IF(NTYPE(K).LE.15.AND.ROUGH(K).LE.0.0) THEN
                        JSTOP = 1
                        IF(JCE.EQ.0) WRITE(N6,9510) NOE(K)
                        IF(JCE.EQ.1) WRITE(N6,9511) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.21.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.21.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).GE.23.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).GE.23.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
C=======================================================================
C     CHECK FOR VALID ELEMENT NUMBER.
C=======================================================================
      IF(K.NE.1) THEN
                 KM1        = K-1
                 DO 4065 KL = 1,KM1
                 IF(JCE.EQ.0.AND.NOE(K).NE.NOE(KL)) GO TO 4065
                 IF(JCE.EQ.1.AND.KOE(K).NE.KOE(KL)) GO TO 4065
                 IF(JCE.EQ.0) WRITE(N6,4064) NOE(K),K,NOE(KL),KL
                 IF(JCE.EQ.1) WRITE(N6,8064) KOE(K),K,KOE(KL),KL
                 JSTOP = 1
                 GO TO 4066
 4065            CONTINUE
                 ENDIF
 4066 IF(JCE.EQ.0) THEN
                   NNEED     = NOE(K)
                   NINNUM(K) = NNEED
                   ELSE
                   KNEED     = KOE(K)
                   KINNUM(K) = KNEED
                   ENDIF
 4070 CONTINUE
 4080 NE = K - 1
      WRITE (N6,967) NE
      IF(JSTOP.GT.0) THEN
                     WRITE(N6,4082)
                     STOP
                     ENDIF
      IF(NE.GT.NET) THEN
                    WRITE(N6,968) NE
                    STOP
                    ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS, IF ANY.
C=======================================================================
      NATUR    = 0
      DO 285 N = 1,NE
      IF(NTYPE(N).EQ.16) THEN
            NATUR = NATUR + 1
            KCOND = 0
            IF(ISLOPE.EQ.0) SLAP = SLOPE(N)*0.01
            IF(ISLOPE.EQ.1) SLAP = SLOPE(N)
            KSTOP = 0
            IF(BARREL(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 BARREL(N) = ABS(BARREL(N))
                                 ENDIF
CC$$$$5/3/92
            CALL GETCUR(N,BARREL(N),SLAP,METRIC,1,KCOND,AFULL(N),
     +                  GEOM1(N),GEOM2(N),DIST(N),ROUGH(N),
     +                  RFULL,JSTOP,NOE(N),KOE(N),KSTOP)
            QCURVE(NATUR,1,26) = RFULL
            BARREL(N)          = 1.0
            ENDIF
      IF(NTYPE(N).EQ.15.OR.NTYPE(N).EQ.14) THEN
            KCOND = 1
            NATUR = NATUR + 1
            IF(ISLOPE.EQ.0) SLAP = SLOPE(N)*0.01
            IF(ISLOPE.EQ.1) SLAP = SLOPE(N)
            KSTOP = 0
            IF(BARREL(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 BARREL(N) = ABS(BARREL(N))
                                 ENDIF
            IF(NTYPE(N).EQ.14) GEOM3(N) = 2.0
CC$$$$5/3/92
            CALL GETCUR(N,GEOM3(N),SLAP,METRIC,1,KCOND,AFULL(N),
     +                  GEOM1(N),GEOM2(N),DIST(N),ROUGH(N),
     +                  RFULL,JSTOP,NOE(N),KOE(N),KSTOP)
            QCURVE(NATUR,1,26) = RFULL
            ENDIF
  285 CONTINUE
C#######################################################################
C RED, 6/2/93. FIX TO READ REVISED NATURAL CHANNEL LINES IN GETCUR.
C     Find special conduit data to read from groups F1, G1 or H1.
C     The following sequence is required if natural channel information
C       was entered in the data input stream.
C#######################################################################
      DO 9700 J = 1,10000
      READ(N5,*,ERR=888) CC
C#### WCH (CDM), 8/93.  CHECK FOR G6 LINE ALSO.
C#### WCH, 5/10/94.  ADD ALL OTHER POSSIBLE IDs: H2,I2,J2,K1,L1.
      IF(CC.EQ.'F1'.OR.CC.EQ.'G1'.OR.CC.EQ.'H1'.OR.
     +   CC.EQ.'I1'.OR.CC.EQ.'J1'.OR.CC.EQ.'$'.OR.CC.EQ.'G6'.OR.
     +   CC.EQ.'H2'.OR.CC.EQ.'I2'.OR.CC.EQ.'J2'.OR.
     +   CC.EQ.'K1'.OR.CC.EQ.'L1'             ) THEN
                                                BACKSPACE N5
                                                GO TO 9750
                                                ENDIF
 9700 CONTINUE
 9750 CONTINUE
C#######################################################################
C=======================================================================
C     INPUT INFORMATION FOR QUALITY PARAMETERS.
C=======================================================================
      KSPG = 0
      IF(NPOLL.LE.0) GO TO 4200
      WRITE (N6,4090)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP F1 <<<<<<<<<<<<
C=======================================================================
      DO 4140 K = 1,NPOLL
      READ(N5,*,ERR=888) CC,KPOL,PNDUM(K),PUDUM(K),NDUM(K),DECAY(K),
     1                   SPG(K),(PSIZE(J,K),PGR(J,K),J=2,5),PSDWF(K)
C=======================================================================
C     HERE, USER INPUT DEFINES POLLUTANT.
C=======================================================================
      IF(KPOL.EQ.0) THEN
                    IPOLX(K)    = 0
                    ELSE
                    IPOLX(K)    = KPOL
                    PNDUM(K)    = PNAME(KPOL)
                    PUDUM(K)    = PUNIT(KPOL)
                    NDUM(K)     = NDIM(KPOL)
                    ENDIF
      PSIZE(1,K) = 0.0
      PGR(1,K)   = 100.0
      WRITE (N6,4130)  K,PNDUM(K),PUDUM(K),NDUM(K),KPOL,DECAY(K),SPG(K)
      IF(SPG(K).GT.1.0) KSPG = KSPG+1
      DECAY(K)               = DECAY(K)/86400.0
 4140 CONTINUE
      DO 4141 K = 1,NPOLL
      PNAME(K)  = PNDUM(K)
      PUNIT(K)  = PUDUM(K)
      NDIM(K)   = NDUM(K)
 4141 CONTINUE
C
      IF(KSPG.EQ.0) WRITE (N6,4145)
      IF(KSPG.EQ.0) GO TO 4170
      WRITE (N6,4150)
C
      DO 4160 K = 1,NPOLL
      IF(SPG(K).LE.1.0) GO TO 4160
      WRITE (N6,4155) K,PSDWF(K),(PSIZE(L,K),PGR(L,K),L=1,5)
      DO 4156 J = 2,4
      IF(PSIZE(J,K).LE.PSIZE(5,K)) GO TO 4156
      PSIZE(5,K) = PSIZE(J,K)
 4156 CONTINUE
      IF(NDIM(K).GT.0) THEN
                       WRITE (N6,4157) K,NDIM(K),K
                       SPG(K) = 1.0
                       KSPG = KSPG-1
                       ENDIF
 4160 CONTINUE
 4170 CONTINUE
 4200 CONTINUE
C=======================================================================
C     Sequence element data in model.
C=======================================================================
      IF(NPOLL.GT.0) WRITE (N6,915)
      CALL SLOP
C=======================================================================
C     Calculate constants and flow parameters for each element.
C=======================================================================
      CALL FIRST(MI,0)
C=======================================================================
C     Write description of run and sewer system.
C=======================================================================
      KARIGE = 1
      IF(METRIC.EQ.1) WRITE(N6,916) KARIGE
      IF(METRIC.EQ.2) WRITE(N6,919) KARIGE
      DO 4250 I = 1,NE
      NT        = NTYPE(I)
      SSL       = SLOPE(I)
      RGH       = ROUGH(I)
      IF(NT.LE.18) THEN
                   VAR(1) = DIST(I)/CMET1
                   VAR(2) = GEOM1(I)/CMET1
                   VAR(3) = GEOM2(I)/CMET1
                   IF(NT.NE.13) VAR(4) = GEOM3(I)/CMET1
                   IF(NT.EQ.13.AND.ITRAP.EQ.0) VAR(4) = GEOM3(I)
                   IF(NT.EQ.13.AND.ITRAP.EQ.1) VAR(4) = 1.0/GEOM3(I)
                   ELSE
                   CMETT = 1.0
                   IF(NT.EQ.19.OR.NT.EQ.20.OR.NT.EQ.23) CMETT = CMET3
                   VAR(1) = DIST(I)/CMETT
                   VAR(4) = GEOM3(I)
                   CMETT = 1.0
                   IF(NT.EQ.20.OR.NT.EQ.21) CMETT = CMET3
                   VAR(2) = GEOM1(I)/CMETT
                   IF(NT.NE.23) THEN
                                VAR(3) = GEOM2(I)
                                ELSE
                                VAR(2) = GEOM1(I)/CMET1
                                SSL    = SLOPE(I)/CMET3
                                RGH    = ROUGH(I)/CMET1
                                VAR(3) = GEOM2(I)/CMET1
                                ENDIF
                   ENDIF
      VAR(5) = AFULL(I)/CMET2
      VAR(6) = QFULL(I)/CMET3
      QMM    = QMAX(I)/CMET3
      IF(NT.LT.19.AND.JCE.EQ.0) WRITE(N6,920) NOE(I),NT,NAME(NT),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                                  (VAR(J),J=5,6),QMM,SCF(I)
      IF(NT.LT.19.AND.JCE.EQ.1) WRITE(N6,940) KOE(I),NT,NAME(NT),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                                  (VAR(J),J=5,6),QMM,SCF(I)
      IF(NT.GE.19.AND.JCE.EQ.0) WRITE(N6,921) NOE(I),NT,NAME(NT),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I)
      IF(NT.GE.19.AND.JCE.EQ.1) WRITE(N6,941) KOE(I),NT,NAME(NT),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                   VAR(5),KGEOM(I)
C=======================================================================
C     CONVERT CONST. INFLOW AND CONCENTRATIONS TO LOADS,
C     IN UNITS OF CONCENTRATION * CFS.
C=======================================================================
      IF(NT.EQ.19) THEN
                   GEOM1(I) = GEOM1(I)*DIST(I)
                   SLOPE(I) = SLOPE(I)*DIST(I)
                   ROUGH(I) = ROUGH(I)*DIST(I)
                   GEOM2(I) = GEOM2(I)*DIST(I)
                   ENDIF
      IF(NT.EQ.14.OR.NT.EQ.15) NTYPE(I) = 16
 4250 CONTINUE
C=======================================================================
C     Input data for storage elements.
C=======================================================================
      IF(KSTOR.GT.NSTOR) THEN
                         WRITE(N6,4252) KSTOR,NSTOR
                         STOP
                         ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS G1,G2,G3,G4,G5 <<<<<<<<<<<<
C=======================================================================
      IF(KSTOR.GT.0) CALL TSTRDT
C#######################################################################
C     WCH, 8/93.  NEW FLOW DIVIDER, TYPE 26.  CHUCK MOORE, CDM
C
C>>>>>>>>>>>> READ DATA GROUPS G6,G7  TABULAR FLOW SPLITER <<<<<<<<<<<<
C=======================================================================
      IF(KINOU.GT.0) CALL RSPLIT
C=======================================================================
C     INITIALIZATION
C=======================================================================
      DO 4255 I  = 1,4
 4255 CPPP(I)    = 0.0
      DO 50 K    = 1,NE+1
      IOLD(K)    = 1
      QINFIL(K)  = 0.0
      QDWF(K)    = 0.0
      RNOFF(K)   = 0.0
      DO 5006 I  = 1,2
      DO 5006 II = 1,2
      A(K,II,I)  = 0.0
      Q(K,II,I)  = 0.0
 5006 CONTINUE
      IF(NPOLL.GT.0) THEN
                     DO 5010 L = 1,NPOLL
                     WDWF(K,L) = 0.0
                     SCOUR(K,L)= 0.0
                     DS(K,L)   = PSIZE(5,L)
                     DB(K,L)   = PSIZE(1,L)
                     PLUTO(L,K)= 0.0
                     DO 5008 I = 1,2
                     CPOL1(K,I,L) = 0.0
                     CPOL2(K,I,L) = 0.0
 5008                CONTINUE
                     IF(K.LE.NTH) THEN
                                  PE1(L,K)=0.0
                                  PE2(L,K)=0.0
                                  PF1(L,K)=0.0
                                  PF2(L,K)=0.0
                                  ENDIF
 5010                CONTINUE
                     ENDIF
      IF(K.LE.NTH) THEN
                   QE1(K)=0.0
                   QE2(K)=0.0
                   QF1(K)=0.0
                   QF2(K)=0.0
                   ENDIF
   50 CONTINUE
      QO(NE+1)    = 0.0
      QI(NE+1)    = 0.0
      NTYPE(NE+1) = 19
      RNOFF(NINPUT+1) = 0.0
      IF(NPOLL.GT.0) THEN
                     DO 51 I = 1,NPOLL
   51                CPOL2(NE+1,2,I) = 0.0
                     ENDIF
      DO 52 I  = 1,7
      DVDWF(I) = 1.0
      DVBOD(I) = 1.0
   52 DVSS(I)  = 1.0
      DO 53 I  = 1,24
      HVDWF(I) = 1.0
      HVBOD(I) = 1.0
   53 HVSS(I)  = 1.0
C=======================================================================
C     CHECK FOR COMPATIBILITY OF INTERFACE FILE LOCATIONS WITH
C     TRANSPORT ELEMENT NUMBERS.
C=======================================================================
      IF(NCNTRL.EQ.0) THEN
                      DO 4270 J = 1,LOCATS
                      DO 4265 I = 1,NE
                      IF(JCE.EQ.0.AND.NLOC(J).EQ.NOE(I)) GO TO 4270
                      IF(JCE.EQ.1.AND.KAN(J).EQ.KOE(I))  GO TO 4270
 4265                 CONTINUE
C#### WCH (RED), 9/93.  RED SUGGESTS ELIMINATING THIS JSTOP.
CCC                      JSTOP = 1
                      IF(JCE.EQ.0) WRITE(N6,4268) NLOC(J)
                      IF(JCE.EQ.1) WRITE(N6,4368) KAN(J)
C=======================================================================
C     CHANGE TO NEGATIVE NO. AND IGNORE INPUT FROM FILE.
C=======================================================================
                      NLOC(J) = -NLOC(J)
                      KAN(J)  = ' '
 4270                 CONTINUE
                      ENDIF
C=======================================================================
C     READ ELEMENT NUMBERS OF LOCATIONS TO BE PLACED ON INTERFACE FILE.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP H1 <<<<<<<<<<<<
C=======================================================================
      IF(NOUTS.GT.0) THEN
                     IF(JCE.EQ.0) THEN
                          READ(N5,*,ERR=888) CC,(JN(I),I=1,NOUTS)
                          WRITE(N6,917)         (JN(I),I=1,NOUTS)
                          ELSE
                          READ(N5,*,ERR=888) CC,(KJN(I),I=1,NOUTS)
                          WRITE(N6,918)         (KJN(I),I=1,NOUTS)
                          ENDIF
                     ENDIF
C#######################################################################
C     READ INPUT DATA FOR WASP LINKAGE, IF PRESENT.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS H2 AND H3 (OPTIONAL) <<<<<<<<<<<
C=======================================================================
      CALL LINK(0)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP I1 <<<<<<<<<<<<
C=======================================================================
      IF(NINPUT.GT.0) THEN
                      IF(JCE.EQ.0) THEN
                           READ(N5,*,ERR=888) CC,(KORDER(I),I=1,NINPUT)
                           WRITE(N6,4290)        (KORDER(I),I=1,NINPUT)
                           ELSE
                           READ(N5,*,ERR=888) CC,(BORDER(I),I=1,NINPUT)
                           WRITE(N6,4291)        (BORDER(I),I=1,NINPUT)
                           ENDIF
                      ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP I2 (IF PRESENT) <<<<<<<<<<<<
C=======================================================================
       READ(N5,*,ERR=888) CC
       IF(CC.EQ.'I2') THEN
                      BACKSPACE N5
                      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NSURF,
     +                                     (JSURF(N),N=1,NSURF)
                      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,NSURF,
     +                                     (KSURF(N),N=1,NSURF)
                      IF(JCE.EQ.0) WRITE(N6,1975) (JSURF(K),K=1,NSURF)
                      IF(JCE.EQ.1) WRITE(N6,1976) (KSURF(K),K=1,NSURF)
                      IF(NSCRAT(7).EQ.0) CALL ERROR(112)
                      ELSE
                      NSURF    = 0
                      BACKSPACE N5
                      ENDIF
C=======================================================================
C    READ IN ELEMENT NUMBERS FOR WHICH INPUT POLLUTOGRAPHS AND
C     HYDROGRAPHS TO BE STORED AT ALL TIME STEPS.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP J1 <<<<<<<<<<<<
C=======================================================================
      IF(NNYN.GT.0) THEN
                    IF(JCE.EQ.0) THEN
                         READ(N5,*,ERR=888) CC,(NYN(I),I=1,NNYN)
                         WRITE(N6,4370)        (NYN(I),I=1,NNYN)
                         ELSE
                         READ(N5,*,ERR=888) CC,(KYN(I),I=1,NNYN)
                         WRITE(N6,4371)        (KYN(I),I=1,NNYN)
                         ENDIF
                    ENDIF
C=======================================================================
C     READ IN ELEMENT NUMBERS FOR WHICH OUTPUT POLLUTOGRAPHS AND
C     HYDROGRAPHS TO BE STORED AT ALL TIME STEPS.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP J2 <<<<<<<<<<<<
C=======================================================================
      IF(NNPE.GT.0) THEN
                    IF(JCE.EQ.0) THEN
                         READ(N5,*,ERR=888) CC,(NPE(I),I=1,NNPE)
                         WRITE(N6,4380)        (NPE(I),I=1,NNPE)
                         ELSE
                         READ(N5,*,ERR=888) CC,(KPE(I),I=1,NNPE)
                         WRITE(N6,4381)        (KPE(I),I=1,NNPE)
                         ENDIF
                    ENDIF
C=======================================================================
C     PERFORM SCRATCH TAPE OPERATIONS
C     STORE PRINT INFORMATION ON SCRATCH TAPES
C=======================================================================
      JPRINT = 0
      IF(NNYN.GT.0.AND.NNPE.LE.0) JPRINT = 1
      IF(NNYN.LE.0.AND.NNPE.GT.0) JPRINT = 2
      IF(NNYN.GT.0.AND.NNPE.GT.0) JPRINT = 3
      IF(JPRINT.EQ.0) GO TO 4400
C
      DO 62 III = 1,2
      IF(III.EQ.1.AND.NNYN.EQ.0) GO TO 62
      IF(III.EQ.2.AND.NNPE.EQ.0) GO TO 62
      NTX       = NSCRAT(III)
      IF(NTX.LE.0) WRITE (N6,4390) III,NTX,NTX
      REWIND NTX
      IF(III.GT.1) GO TO 61
C=======================================================================
C     Must resequence NYN array to match computation order.
C=======================================================================
      N         = 0
      DO 4394 I = 1,NE
      M         = JR(I)
      DO 4392 J = 1,NNYN
      IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 4392
      IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 4392
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA  = NYN(N)
                   NYN(N) = NYN(J)
                   NYN(J) = LYDIA
                   ELSE
                   BMJ    = KYN(N)
                   KYN(N) = KYN(J)
                   KYN(J) = BMJ
                   ENDIF
      GO TO 4393
 4392 CONTINUE
 4393 IF(N.GE.NNYN) GO TO 62
 4394 CONTINUE
C=======================================================================
C     Must resequence NPE array to match computation order.
C=======================================================================
   61 N = 0
      DO 4398 I = 1,NE
      M         = JR(I)
      DO 4396 J = 1,NNPE
      IF(JCE.EQ.0.AND.NOE(M).NE.NPE(J)) GO TO 4396
      IF(JCE.EQ.1.AND.KOE(M).NE.KPE(J)) GO TO 4396
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA  = NPE(N)
                   NPE(N) = NPE(J)
                   NPE(J) = LYDIA
                   ELSE
                   BMJ    = KPE(N)
                   KPE(N) = KPE(J)
                   KPE(J) = BMJ
                   ENDIF
      GO TO 4397
 4396 CONTINUE
 4397 IF(N.GE.NNPE) GO TO 62
 4398 CONTINUE
   62 CONTINUE
 4400 CONTINUE
C=======================================================================
C     Must resequence JSURF array to match computation order.
C=======================================================================
      IF(NSURF.GT.0) THEN
      N         = 0
      DO 5398 I = 1,NE
      M         = JR(I)
      DO 5396 J = 1,NSURF
      IF(JCE.EQ.0.AND.NOE(M).NE.JSURF(J)) GO TO 5396
      IF(JCE.EQ.1.AND.KOE(M).NE.KSURF(J)) GO TO 5396
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA    = JSURF(N)
                   JSURF(N) = JSURF(J)
                   JSURF(J) = LYDIA
                   ELSE
                   JSURF(J) = M
                   ENDIF
      GO TO 5397
 5396 CONTINUE
 5397 IF(N.GE.NSURF) GO TO 5562
 5398 CONTINUE
 5562 CONTINUE
      ENDIF
C=======================================================================
C     Check for input errors in NYN, NPE or JSURF arrays.
C=======================================================================
      IF(NNYN.GT.0) THEN
              DO 5400 J = 1,NNYN
              DO 5402 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 5402
              IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 5402
              GO TO 5400
 5402         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9560) NYN(J)
              IF(JCE.EQ.1) WRITE(N6,9561) KYN(J)
 5400         CONTINUE
              ENDIF
      IF(NNPE.GT.0) THEN
              DO 5405 J = 1,NNPE
              DO 5407 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.NPE(J)) GO TO 5407
              IF(JCE.EQ.1.AND.KOE(M).NE.KPE(J)) GO TO 5407
              GO TO 5405
 5407         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9570) NPE(J)
              IF(JCE.EQ.1) WRITE(N6,9571) KPE(J)
 5405         CONTINUE
              ENDIF
      IF(NSURF.GT.0) THEN
              DO 5410 J = 1,NSURF
              DO 5412 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.JSURF(J)) GO TO 5412
              IF(JCE.EQ.1.AND.KOE(M).NE.KSURF(J)) GO TO 5412
              GO TO 5410
 5412         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9580) JSURF(J)
              IF(JCE.EQ.1) WRITE(N6,9581) KSURF(J)
 5410         CONTINUE
              ENDIF
C=======================================================================
C     Stop if input errors have been found.
C=======================================================================
      IF(JSTOP.GT.0)  THEN
                      WRITE(N6,4082)
                      STOP
                      ENDIF
C=======================================================================
C     DETERMINE AVERAGE DAILY DWF AND INFILTRATION
C     DATA ARE READ IN FROM BOTH OF THESE SUBROUTINES.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS K1 AND K2 <<<<<<<<<<<<
C=======================================================================
      IF(NINFIL.EQ.1) THEN
                      WRITE(*,5055)
                      CALL INFIL
                      ENDIF
C=======================================================================
C>>>>>>>>>>> READ DATA GROUPS L1,L2,L3,M1,M2,M3,M4,N1,O1,O2,P1,Q1 <<<<<<
C=======================================================================
      IF(NFILTH.EQ.1) THEN
                      WRITE(*,5060)
                      CALL FILTH
                      ENDIF
C=======================================================================
C     Initialize dry-weather flow (base flow) deposition.
C=======================================================================
      N = 0
      IF(KSPG.GT.0.AND.DWDAYS.GT.0.0) CALL DWLOAD
C=======================================================================
C     INITIALIZE DWF (BASE FLOW) FLOWS, AREAS AND CONCENTRATIONS.
C=======================================================================
      WRITE(*,5070)
      CALL INITAL
C=======================================================================
C     Generate output interface file.
C=======================================================================
      IF(NEXT.LE.0.OR.NOUTS.LE.0) RETURN
C=======================================================================
C     Here, Transport is first block run.
C=======================================================================
      IF(LAST.EQ.0.AND.NCNTRL.GT.0) THEN
                                     TITLE(3) = TITLE(1)
                                     TITLE(4) = TITLE(2)
                                     ENDIF
       REWIND NEXT
       WRITE(NEXT) NOUTS,NPOLL
       IF(JCE.EQ.0) WRITE(NEXT) (JN(I),I=1,NOUTS)
       IF(JCE.EQ.1) WRITE(NEXT) (KJN(I),I=1,NOUTS)
       QQCONV = QCONV
       QCONV  = CMET(8,METRIC)
       SOURCE = 'TRANSPORT BLOCK'
       CALL INFACE(2,NEXT)
       QCONV  = QQCONV
       RETURN
  888  CALL IERROR
C=======================================================================
   10 FORMAT(/,1X,
     1'#####################################################',/,1X,
     2'# Entry made to the Transport Block, last updated   #',/,1X,
     3'# by Oregon State University, March 1994.           #',/,1X,
     4'#####################################################',/,1X,
     5'# "The sewer is the conscience of the city."        #',/,1X,
     6'#                                 Victor Hugo (1862)#',/,1X,
     7'#####################################################',/)
   11 FORMAT(/,1X,
     1'#####################################################',/,1X,
     2'# Entry made to the Transport Block, last updated   #',/,1X,
     3'# by Oregon State University, March 1994.           #',/,1X,
     4'#####################################################',/)
  907 FORMAT (' ',10X,A80,/,11X,A80)
  908 FORMAT(/,
     1' ************************************************',/,
     2' *  OPTIONAL INPUT PARAMETRS FROM DATA LINE B0  *',/,
     2' *  ------------------------------------------  **',/,
     2' *    The default values are used if line B0    *',/,
     2' *    is not part of the TRANSPORT input data.  *',/,
     2' *    Defaults:  ISLOPE=0  ITRAP=0   IFLIP=0    *',/,
     3' ************************************************',//,
     4' Use ft/100 ft or m/100 m for conduit slope...(ISLOPE=0)..',I5,/,
     4' Use ft/ft or m/m for input of conduit slope..(ISLOPE=1)..',/,
     6' Vertical/horizontal trapezoid side slope.....(ITRAP=0)...',I5,/,
     6' Horizontal/vertical trapezoid side slope.....(ITRAP=1)...',/,
     7' Default input of flow/pollutants on line R1..(IFLIP=0)...',I5,/,
     7' Input flow/pollutants on one R1 line.........(IFLIP=1)...',/,
     8' Include only hydrograph input(s) on printout.(INFLEW=0)..',I5,/,
     8' Include all input(s) on input printout.......(INFLEW=1)..',/)
  915 FORMAT(1H1)
  916 FORMAT(I1,/,
     1' *****************************************************',/,
     1' *           TRANSPORT ELEMENT PARAMETERS            *',/,
     1' *                                                   *',/,
     *' * CAUTION: COLUMN HEADINGS ARE FOR CONDUITS.  REFER *',/,
     2' * TO USERS MANUAL FOR MEANING FOR NON-CONDUITS.     *',/,
     1' *****************************************************',//,
     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1  GEO
     3M2  GEOM3  NUMBER   AFULL     QFULL     QMAX   SUPER-CRITICAL',/,
     4'   ELE. TY                 (FT/FT)   (FT)   ROUGHNESS   (FT)   (F
     4T)   (FT)    OF    (SQ.FT)    (CFS)    (CFS)   FLOW WHEN LESS',/,
     5'   NUM. PE TYPE NAME
     5          BARRELS                              THAN 95% FULL?',/,
     6'   ---- -- -----------     -------  ------  --------  ------  ---
     6--  ----- -------  -------   ------  -------  --------------')
  919 FORMAT(I1,/,
     1' *****************************************************',/,
     1' *           TRANSPORT ELEMENT PARAMETERS            *',/,
     1' *                                                   *',/,
     *' * CAUTION: COLUMN HEADINGS ARE FOR CONDUITS.  REFER *',/,
     2' * TO USERS MANUAL FOR MEANING FOR NON-CONDUITS.     *',/,
     1' *****************************************************',//,
     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1  GEO
     3M2  GEOM3  NUMBER   AFULL     QFULL     QMAX   SUPER-CRITICAL',/,
     4'   ELE. TY                   (M/M)    (M)   ROUGHNESS    (M)    (
     4M)    (M)    OF     (SQ.M)    (CMS)    (CMS)   FLOW WHEN LESS',/,
     5'   NUM. PE TYPE NAME
     5          BARRELS                              THAN 95% FULL?',/,
     6'   ---- -- -----------     -------  ------  --------  ------  ---
     6--  ----- -------  -------   ------  -------  --------------')
  917 FORMAT(//,
     1' ********************************************************',/,
     2' * HYDROGRAPHS AND POLLUTOGRAPHS PROVIDED TO SUBSEQUENT *',/,
     3' * BLOCKS FOR THE FOLLOWING ELEMENTS ON DATA GROUP H1:  *',/,
     4' ********************************************************',//,
     5  (10I9))
  918 FORMAT(//,
     1' ********************************************************',/,
     2' * HYDROGRAPHS AND POLLUTOGRAPHS PROVIDED TO SUBSEQUENT *',/,
     3' * BLOCKS FOR THE FOLLOWING ELEMENTS ON DATA GROUP H1:  *',/,
     4' ********************************************************',//,
     5  (10(1X,A10)))
  920 FORMAT(1X,I6,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
  921 FORMAT(1X,I6,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
  940 FORMAT(1X,A6,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
  941 FORMAT(1X,A6,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1X,A9,F9.2,4X,A4)
  968 FORMAT(//,' ====> TOO MANY ELEMENTS READ IN IN TRANSPORT BLOCK',
     1          '(',I4,').  PROGRAM STOPPED IN SUB TRANS.')
  950 FORMAT(//,' NUMBER OF TIME STEPS (NDT)........................',
     1 T52,I5,/,' NO. OF ELEMENTS FOR CARD HYDROGRAPH INPUT (NINPUT)',
     2 T52,I5,/,' NO. OF ELEMENTS FOR INPUT HYDROGRAPH PRINT (NNYN).',
     3 T52,I5,/,' NO. OF ELEMENTS FOR OUTPUT HYDROGRAPH PRINT (NNPE)',
     4 T52,I5,/,' NO. TIME STEPS BETWEEN I/O PRINTS (INTPRT)........',
     5 T52,I5,/,' NO. OF ELEMENTS FOR INTERFACE TRANSFER (NOUTS)....',
     6 T52,I5,/,' NO. OF POLLUTANTS SIMULATED (NPOLL)...............',
     7 T52,I5,/,' NO. OF ITERATIONS FOR FLOW ROUTING (NITER)........',
     8 T52,I5)
  951 FORMAT(//,' FOR THE FOLLOWING PARAMETERS, 1=YES, 0=NO:',
     1/,'    PRINT INTERNAL ERROR MESSAGES (NPRINT).........',I3,
     2/,' HYDROGRAPH INPUT FROM CARDS ONLY (NCNTRL).........',I3,
     3/,' CALL INFILTRATION ROUTINE (NINFIL)................',I3,
     4/,' CALL DRY-WEATHER FLOW ROUTINE (NFILTH)............',I3,
     5/,' USE HYDRAULIC DESIGN ROUTINE (NDESN)..............',I3)
  952 FORMAT(//,' SIZE OF TIME STEP (DT, SECONDS)...................',
     1 F8.1,/,' ALLOWABLE ROUTING CONVERGENCE ERROR (EPSIL).......',
     2 F8.6,/,' NO. DRY DAYS PRIOR TO SIMULATION (DWDAYS).........',
     2 F8.2,/,' STARTING TIME OF DAY IN HOURS.....................',
C#### WCH, 1/4/94.  FIX TYPO IN VISCOSITY
     3 F8.2,/,' KINEMATIC VISCOSITY (GNU, SQ FT/SEC)..............',
     4 1PE8.2,/,' TOTAL CATCHMENT AREA (TRIBA, ACRES)...............',
     5 0PF8.2)
  953 FORMAT(//,' SIZE OF TIME STEP (DT, SECONDS)...................',
     1 F8.1,/,' ALLOWABLE ROUTING CONVERGENCE ERROR (EPSIL).......',
     2 F8.6,/,' NO. DRY DAYS PRIOR TO SIMULATION (DWDAYS).........',
     2 F8.2,/,' STARTING TIME OF DAY IN HOURS.....................',
     3 F8.2,/,' KINEMATIC VISCOSITY (GNU, SQ CM/SEC)..............',
     4 1PE8.2,/,' TOTAL CATCHMENT AREA (TRIBA, HECTARES)............',
     5 0PF8.2)
  966 FORMAT(//,' STARTING DATE OF SIMULATION (YR/MO/DAY) = ',
     1            I5,'/',I2,'/',I2,/,
     2          ' STARTING TIME OF DAY HR:MN:SEC          = ',
     3            I5,'/',I2,'/',I2)
  967 FORMAT (//,1H1,'THERE WERE ',I4,' ELEMENTS, NE, INPUT',
     1 ' INTO TRANSPORT BLOCK FROM DATA GROUP E1.')
  970 FORMAT(/,'NTYPE  DESCRIPTION         ALFMAX  PSIMAX  AFACT   RFACT
     +  KDEPTH  KLASS  INDEX  ANORM   QNORM    DNORM',/,
     +'-----  -----------         ------  ------  -----   -----  ------
     +  -----  -----  -----   -----    -----')
  971 FORMAT(I5,2X,A20,F7.4,3F8.4,I5,I7,I8,F8.3,2F9.5,/,(70X,I8,F8.3,
     1 2F9.5))
  972 FORMAT('  NON-CONDUITS',//,T5,'NTYPE  KDEPTH  KLASS  DESCRIPTION')
  973 FORMAT(1X,I6,I7,I8,4X,A20)
 1975 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP I2    *',/,
     +         '     ***********************',//,
     1          ' CHANNEL/DEPTH PRINT DATA GROUP.......',10I7,
     2          19(/,31X,10I7))
 1976 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP I2    *',/,
     +         '     ***********************',//,
     1         ' CHANNEL/DEPTH PRINT DATA GROUP.......',10(A6,1X),
     2          19(/,31X,10(A6,1X)))
 3005 FORMAT(/,' ===> WARNING !! FROM SUB TRANS. NOUTS=',I4,' AND JOUT =
     1',I4,'.  BOTH MUST BE GT 0 TO GENERATE INTERFACE FILE.',/,
     2 ' SIMULATION WILL CONTINUE, BUT NO NEW INTERFACE FILE WILL BE GEN
     3ERATED.')
 3010 FORMAT(/,' USE U.S. CUSTOMARY UNITS FOR INPUT/OUTPUT(METRIC=0).')
 3015 FORMAT(/,' USE METRIC UNITS FOR MOST INPUT/OUTPUT(METRIC=1).',/,
     1' UNITS OF FEET AND SECONDS STILL USED FOR MOST INTERNAL COMPUTATI
     2ONS.')
 3095 FORMAT(' ===> WARNING  !! FROM SUB. TRANS.  INPUT FILE UNIT NUMBER
     1(JIN) = ',I9,/,1X,'PROGRAM WILL SOON TRY TO READ FROM THIS FILE',
     2' AND AN ERROR MESSAGE MAY APPEAR.')
 4007 FORMAT(/,' ===>  ERROR IN SUB TRANS.  NO. OF INPUT LOCATIONS FROM
     1 INTERFACE FILE =',I9,' IS GT NTH ALLOWABLE FOR TRANSPORT INPUT.',
     2/,' PROGRAM STOPPED.')
 4064 FORMAT (' ====> ERROR IN SUB. TRANS.  ELE. NO.',I4,'(INPUT SEQUENC
     1E',I4,') IS SAME AS ELE. NO.',I4,' (INPUT SEQUENCE',I4,').',/,
     2 ' THIS IS NOT ALLOWED.  PROGRAM WILL STOP BELOW.')
 8064 FORMAT (' ====> ERROR IN SUB. TRANS.  ELE. NO.',A9,'(INPUT SEQUENC
     1E',I4,') IS SAME AS ELE. NO.',A9,' (INPUT SEQUENCE',I4,').',/,
     2 ' THIS IS NOT ALLOWED.  PROGRAM WILL STOP BELOW.')
 4082 FORMAT (///,' <> <> <> <> <> PROGRAM STOPPED IN SUBROUTINE TRANS D
     1UE TO ELEMENT NUMBER ERROR(S) GIVEN ABOVE. <> <> <> <> <>')
 4090 FORMAT(///,
     110X,' *****************************************************',/,
     210X,' *   QUALITY PARAMETERS TO BE SIMULATED IN TRANSPORT *',/,
     310X,' *****************************************************',//,
     1T27,'TYPE OF',T37,'INTERFACE FILE    DECAY COEF',T66,'SPECIFIC',/,
     3 ' NO.  NAME      UNITS      UNITS   POSITION (IF ANY)   (1/DAY)
     4  GRAVITY',/,' ---  ----      -----      -----   -----------------
     5   -------    -------')
 4130 FORMAT (1X,I2,3X,A8,2X,A8,I6,I13,F18.3,F12.3)
 4145 FORMAT (//,
     110X,' *******************************************',/,
     210X,' *     ALL SPECIFIC GRAVITIES ARE LE 1.0.  *',/,
     310X,' *     NO SIMULATION OF SCOUR-DEPOSITION.  *',/,
     410X,' *******************************************',//)
 4150 FORMAT (//,
     15X,' **********************************************',/,
     25X,' * PARTICLE SIZE DISTRIBUTIONS FOR QUALITY    *',/,
     35X,' * CONSTITUENTS WITH SPECIFIC GRAVITIES > 1.0 *',/,
     45X,' **********************************************',//,
     25X,' POLL.    MAX DWF     SIZE     PERCENT',/,
     35X,'  NO.    SIZE (MM)    (MM)     GREATER',/,
     45X,' ----    ---------    ----     -------')
 4155 FORMAT(5X,I4,2F12.5,F10.3,/,4(21X,F12.5,F10.3,/))
 4157 FORMAT(/,' ===> WARNING !! FROM SUB TRANS.  NDIM(',I1,') = ',I2,
     1 ', GT 0.  CONCENTRATION UNITS MUST BE MG/L FOR USE OF SCOUR/DEPOS
     2ITION ROUTINES.',/,' SPECIFIC GRAVITY, SPG, HAS BEEN SET = 1.0 FOR
     3 POLLUTANT NO.',I2,' TO PERMIT CONTINUATION OF RUN.')
 4252 FORMAT (//,' >>>>> ERROR IN SUB. TRANS.  NUMBER OF STORAGE UNITS='
     1,I3,' WHICH EXCEEDS ALLOWABLE TOTAL = ',I3,' <<<<<',/,
     2 ' >>>>> PROGRAM STOPPED <<<<<')
 4268 FORMAT (/,' ===> ERROR !!! FROM SUB. TRANS.  NO ELEMENT NUMBER MAT
     1CH FOUND FOR INPUT LOCATION ',I7,' ON INTERFACE FILE.  SIMULATION
     2CONTINUES.')
 4368 FORMAT (/,' ===> WARNING !!! FROM SUB. TRANS.  NO ELEMENT NUMBER M
     1ATCH FOUND FOR INPUT LOCATION ',A10,' ON INTERFACE FILE.  SIMULATI
     2ON CONTINUES.')
 4290 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT OF HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE READ *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP I1:   *',/,
     4' *********************************************************',//,
     5  (10I9))
 4291 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT OF HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE READ *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP I1:   *',/,
     4' *********************************************************',//,
     5  (10(1X,A10)))
 4370 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J1:   *',/,
     4' *********************************************************',//,
     5  (10I9))
 4371 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J1:   *',/,
     4' *********************************************************',//,
     5  (10(1X,A10)))
 4380 FORMAT(//,
     1' **********************************************************',/,
     2' * OUTPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J2:    *',/,
     4' **********************************************************',//,
     5  (10I9))
 4381 FORMAT(//,
     1' **********************************************************',/,
     2' * OUTPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J2:    *',/,
     4' **********************************************************',//,
     5  (10(1X,A10)))
 4390 FORMAT(/,' ===> ERROR !!! IN SUB TRANS. NSCRAT(',I1,') = ',
     1 I4,'.  MUST BE GT 0 FOR PRINT OF INPUT AND OUTPUT HYDROGRAPHS.',
     2 /,' A SYSTEM ERROR MAY SOON BE EXPECTED AS PROGRAM TRIES TO WRITE
     3 ON UNIT',I4)
 5050 FORMAT(/,' Reading element data.')
 5055 FORMAT(/,' Reading infiltration data.')
 5060 FORMAT(/,' Reading water quality data.')
 5070 FORMAT(/,' Calculating initial conditions.')
 5080 FORMAT(/,' ===> WARNING !! NCNTRL GT 0 BUT INPUT INTERFACE FILE IS
     + DEFINED.',/)
 9500 FORMAT(/,' ===> ERROR !! ZERO SLOPE FOR ELEMENT # ',I10)
 9501 FORMAT(/,' ===> ERROR !! ZERO SLOPE FOR ELEMENT # ',A10)
 9510 FORMAT(/,' ===> ERROR !! ZERO ROUGHNESS FOR ELEMENT # ',I10)
 9511 FORMAT(/,' ===> ERROR !! ZERO ROUGHNESS FOR ELEMENT # ',A10)
 9520 FORMAT(/,' ===> ERROR !! NO GEOM3 ELEMENT IS SPECIFIED FOR ELEMENT
     + # ',I10)
 9521 FORMAT(/,' ===> ERROR !! NO GEOM3 ELEMENT IS SPECIFIED FOR ELEMENT
     + # ',A10)
 9560 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP J1.')
 9561 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP J1.')
 9570 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP J2.')
 9571 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP J2.')
 9580 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP I2.')
 9581 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP I2.')
C=======================================================================
      END
