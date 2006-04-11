      SUBROUTINE QHYDRO
C=======================================================================
C     QHYDRO was created NOVEMBER, 1981 BY R.DICKINSON
C     last updated December, 1990.
C     Updated 4/93 by WCH to correct land-use input and print-out 
C       from groups L1 and L2 and slight format modifications.
C     WCH, 9/3/93. Add quality input for infiltration/inflow.
C     WCH (Warren Chrusciel), 9/28/93.  Fix metric conversion for QFACT3.
C     WCH, 9/29/93.  Add warning and code for too many J2 lines.
C     WCH, 11/15/93.  Change loop for IEND to NQSS + 2 from NRQ + 2.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'NEW89.INC'
      INCLUDE 'GRWTR.INC'
C#### WCH, 9/93
      INCLUDE 'RDII.INC'
      DIMENSION XINJ2(6),RJLAND(6),DJLAND(6),
     1          XINJ3(10),DNQS(10),RNQS(10),EROS(5),REROS(5),
     2          DEROS(5),SUBQL(12),DSUBQL(12),RSUBQL(12)
      CHARACTER JDLINK(2)*16,IDASH*4,ISTAR*4,ISPACE*4,KKN*10,
     +          JDBET(3)*20,JDFDB(3)*16,JDTBC(5)*16,JDTWC(3)*20
      DATA IDASH/'----'/,ISTAR/'****'/,ISPACE/'    '/
      DATA JDLINK/' NO SNOW LINKAGE','BUILDUP FOR SNOW'/
      DATA JDBET/'   POWER LINEAR(0) ',' EXPONENTIAL(1)    ',
     +           ' MICHAELIS MENTEN(2)'/
      DATA JDFDB/'CHAN. LENGTH(0) ','        AREA(1) ',
     +           '    CONSTANT(2) '/
C#### WCH, 9/9/93. REMOVE ONE BLANK FROM BEFORE "RATG" AND COSMETIC.
      DATA JDTWC/'  POWER EXPONEN.(0) ',' RATG CURVE NO UL(1)',
     2           '  RATING CURVE UL(2)'/
      DATA JDTBC/' FRACT. BLDUP(0)',' POWER-LINEAR(1)',
     2           '  EXPONENTIAL(2)',' MICH. MENTEN(3)',
     4           '   NO BUILDUP(4)'/
C=======================================================================
      DO 50     I = 1,NRQ
      NDIM(I)     = 0.0
      DO 50     J = 1,NLU
      KEY(J,I)    = 0.0
      KALC(J,I)   = 0.0
      KWASH(J,I)  = 0.0
      TEMPLD(J,I) = 0.0
      KACGUT(J,I) = 0.0
      WASHPO(J,I) = 0.0
      CONCRN(J,I) = 0.0
      RCOEF(J,I)  = 0.0
      RCOEFX(J,I) = 0.0
 50   CBFACT(J,I) = 0.0
C=======================================================================
C>>>>>>>>>>>>>>>>>>> READ DATA GROUP JJ <<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'JJ') THEN
                     READ(N5,*,ERR=888) CC,IMUL
                     IF(IMUL.LT.0) CALL ERROR(143)
                     ELSE
                     IMUL = 0
                     ENDIF
C=======================================================================
C>>>>>>>>>>>>>>>>>>> READ DATA GROUP J1 <<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NQS,JLAND,IROS,IROSAD,DRYDAY,CBVOL,
     1                   DRYBSN,RAINIT,REFFDD,KLNBGN,KLNEND
      IF(CC.NE.'J1') CALL ERROR(140)
      IF(IMUL.EQ.0) N1 = 1
      IF(IMUL.GT.0) N1 = JLAND
C=======================================================================
C     Add an erosion constituent if IROS = 1.
C=======================================================================
      IF(IROS.GT.0) THEN 
                    NQS        =  NQS + 1
                    PNAME(NQS) = KEROS(1)
                    PUNIT(NQS) = KEROS(2)
                    NDIM(NQS)  = 0
                    NQS = NQS - 1
                    ENDIF
      NQSS = NQS
      IF(KLNEND.LE.0)   KLNEND = 367
      IF(DRYBSN.LE.0.0) THEN
                        WRITE(N6,1623) DRYBSN
                        DRYBSN = 1.0
                        ENDIF
C=======================================================================
C     Write the quality input data.
C=======================================================================
      IF(IPRN(7).EQ.0) THEN
        IF(METRIC.EQ.1) WRITE(N6,1180) NQS,JLAND,CBVOL
        IF(METRIC.EQ.2) WRITE(N6,1190) NQS,JLAND,CBVOL
        IF(IROS.EQ.0) WRITE(N6,1590) IROS
        IF(IROS.EQ.1) WRITE(N6,1600) IROS
        IF(IROS.EQ.1) WRITE(N6,1610) IROSAD
        IF(METRIC.EQ.1.AND.IROS.EQ.1) WRITE(N6,1615) RAINIT
        IF(METRIC.EQ.2.AND.IROS.EQ.1) WRITE(N6,1616) RAINIT
                                      WRITE(N6,1620) DRYDAY
        WRITE(N6,1625) DRYBSN
        WRITE(N6,1630) REFFDD
        WRITE(N6,1635) KLNBGN,KLNEND
        ENDIF
      CBVOL  = CBVOL  * CMET(8,METRIC)
      RAINIT = RAINIT / CMET(5,METRIC)
C=======================================================================
C     Set the default limits and default ratio for land use.
C=======================================================================
      DO 1045 J = 1,6
      DJLAND(J) = 0.0
 1045 RJLAND(J) = 1.0
      DJLAND(1) = 1.0E35
C=======================================================================
C     Read in JLAND land use data groups.
C=======================================================================
      IF(JLAND.LE.0)   CALL ERROR(1)
      IF(JLAND.GT.NLU) CALL ERROR(2)
      J          = 0
      DO 1070 JJ = 1,500
      J          = J + 1
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>>> READ DATA GROUP J2 <<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,LNAME(J),METHOD(J),JACGUT(J),
     1                      (XINJ2(K),K=1,6)
      IF(CC.NE.'J2') CALL ERROR(141)
C=======================================================================
C     SET THE NEW DEFAULT VALUES -- METHOD(J) = -2
C=======================================================================
      IF( METHOD(J).EQ.-2 ) THEN
                            DO 1055 K = 1,6
 1055                       IF(XINJ2(K).GT.0.0) DJLAND(K)=XINJ2(K)
                            J = J - 1
                            GO TO 1070
                            ENDIF
C=======================================================================
C     SET THE NEW RATIO  -- METHOD(J) = - 1
C=======================================================================
      IF(METHOD(J).EQ.-1) THEN
                          DO 1060 K = 1,6
 1060                     IF(XINJ2(K).GT.0.0) RJLAND(K)=XINJ2(K)
                          J = J - 1
                          GO TO 1070
                          ENDIF
C=======================================================================
C     MULTIPLY BY THE RATIOS AND ASSIGN DEFAULT VALUES
C=======================================================================
      DO 1067 K = 1,6
      IF(XINJ2(K).LE.0.0) XINJ2(K) = DJLAND(K)
 1067                     XINJ2(K) = XINJ2(K) * RJLAND(K)
      DDLIM(J)  = XINJ2(1)
      DDPOW(J)  = XINJ2(2)
      DDFACT(J) = XINJ2(3)
      CLFREQ(J) = XINJ2(4)
      AVSWP(J)  = XINJ2(5)
      DSLCL(J)  = XINJ2(6)
      IF(METHOD(J).LT.0.OR.METHOD(J).GT.2)    CALL ERROR(8)
      IF(JACGUT(J).LT.0.OR.JACGUT(J).GT.2)    CALL ERROR(9)
C#######################################################################
C     WCH, 9/29/93.  ADD CODE FOR MORE J2 LINES THAN VALUE OF JLAND.
C=======================================================================
      IF(J.EQ.JLAND.AND.JLAND.EQ.NLU) GO TO 1071
      IF(J.LT.JLAND) GO TO 1070
C=======================================================================
C     READ NEXT LINE TO SEE IF IT IS ANOTHER LAND USE LINE.
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'J2') THEN
           JLAND = JLAND + 1
           WRITE(N6,6018) J,JLAND
           IF(JLAND.GT.NLU) CALL ERROR(2)
           IF(IMUL.GT.0) N1 = JLAND
           GO TO 1070
           ELSE
           GO TO 1071
           ENDIF
 1070 CONTINUE
 1071 CONTINUE
C=======================================================================
C     SET THE DEFAULT VALUES AND DEFAULT RATIOS
C     FOR THE BUILDUP PARAMETERS AND NQS PARAMETERS
C=======================================================================
      DO 1072 I = 1,10
      DNQS(I)   = 0.0
1072  RNQS(I)   = 1.0
C=======================================================================
C     Read in the NQS quality constituents.
C=======================================================================
      IF(NQS.LE.0)  CALL ERROR(3)
      IF(NQS.GT.10) CALL ERROR(4)
      IF(NQS.EQ.10.AND.IROS.EQ.1) CALL ERROR(5)
      K          = 0
      DO 1075 KK = 1,500
      K          = K + 1
      DO 1090  J = 1,N1
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>>> READ DATA GROUP J3 <<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,PNAME(K),PUNIT(K),NDIM(K),KALC(J,K),
     1                      KWASH(J,K),KACGUT(J,K),LINKUP(J,K),
     2                      (XINJ3(L),L=1,10)
      IF(CC.NE.'J3') CALL ERROR(142)
C=======================================================================
C     SET THE NEW DEFAULT VALUES -- KALC(J,K) = -2
C=======================================================================
         IF(KALC(J,K).EQ.-2) THEN
                             DO 1076 I = 1,10
 1076                        IF(XINJ3(I).GT.0.0) DNQS(I) = XINJ3(I)
                             K         = K - 1
                             GO TO 1075
                             ENDIF
C=======================================================================
C     Set the new ratio  -- KALC(J,K) = -1
C=======================================================================
       IF(KALC(J,K).EQ.-1) THEN
                           DO 1078 I = 1,10
 1078                      IF(XINJ3(I).GT.0.0) RNQS(I) = XINJ3(I)
                           K         = K - 1
                           GO TO 1075
                           ENDIF
1079  DO 1080 I      = 1,10
      IF(XINJ3(I).LT.0.0) XINJ3(I) = DNQS(I)
1080  XINJ3(I)       = XINJ3(I)    * RNQS(I)
      QFACT(J,1,K)   = XINJ3(1)
      QFACT(J,2,K)   = XINJ3(2)
      QFACT(J,3,K)   = XINJ3(3)
      QFACT(J,4,K)   = XINJ3(4)
      QFACT(J,5,K)   = XINJ3(5)
      WASHPO(J,K)    = XINJ3(6)
C#### WCH, 4/13/93.  CHECK FOR EXTREME VALUE OF WASHPO.
        IF(WASHPO(J,K).GT.25.) WRITE(N6,6017) J,K,WASHPO(J,K)
      RCOEF(J,K)     = XINJ3(7)
      CBFACT(J,K)    = XINJ3(8)
      CONCRN(J,K)    = XINJ3(9)
      REFF(J,K)      = XINJ3(10)
1090  CONTINUE
      IF(K.EQ.NQS) GO TO 1175
1075  CONTINUE
1175  CONTINUE
      IF(RAINIT.LT.0.AND.IROS.EQ.1)   CALL ERROR(7)
C=======================================================================
C     Write the land use data.
C=======================================================================
      IF(IPRN(7).EQ.0) WRITE(N6,1639)
      IF(IPRN(7).EQ.0) WRITE(N6,1640)
      DO 1645 J = 1,JLAND
      JET       = METHOD(J) + 1
      JAM       = JACGUT(J) + 1
      IF(IPRN(7).EQ.0) WRITE(N6,1641)
     +                 LNAME(J),JDBET(JET),JDFDB(JAM),DDLIM(J),DDPOW(J),
     +                 DDFACT(J),CLFREQ(J),AVSWP(J),DSLCL(J)
C=======================================================================
C     Make the units conversion from metric input to U.S. Standard units
C     for DDLIM and DDFACT.
C=======================================================================
                      CLB = 1.0
      IF(METRIC.EQ.2) CLB = 1000.0/453.7
      IF(JACGUT(J).EQ.0) DDLIM(J) = DDLIM(J) *
     *                              CLB/CMET(1,METRIC)
      IF(JACGUT(J).EQ.1) DDLIM(J) = DDLIM(J) * CLB/CMET(2,METRIC)
      IF(JACGUT(J).EQ.2) DDLIM(J) = DDLIM(J) * CLB
      IF(METHOD(J).LE.0) THEN
                         IF(JACGUT(J).EQ.0) DDFACT(J) = DDFACT(J) *
     *                                           CLB/CMET(1,METRIC)
                         IF(JACGUT(J).EQ.1) DDFACT(J) = DDFACT(J) *
     *                                           CLB/CMET(2,METRIC)
                         IF(JACGUT(J).EQ.2) DDFACT(J) = DDFACT(J)*CLB
                         ENDIF
 1645 CONTINUE
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>> READ DATA GROUP J4 <<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      DO 1647 I = 1,10
      DO 1647 J = 1,10
 1647 F1(I,J)   = 0.0
 1650 READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'J4') THEN
                     READ(N5,*,ERR=888) CC,KTO,KFROM,FIN1
                     IF(KTO.GT.0) F1(KTO,KFROM) = FIN1
                     GO TO 1650
                     ENDIF
C=======================================================================
C     Write the constituent data.
C=======================================================================
      IF(IPRN(7).EQ.0) THEN
         WRITE(N6,1660)
         DO 2000 K = 1,NQS,5
         DO 1990 J = 1,N1
         KK        = K + 4
         IF(NQS.LT.KK)  KK = NQS
         WRITE(N6,2001) (PNAME(I),I=K,KK)
         WRITE(N6,2002) (ISPACE,I=K,KK)
         WRITE(N6,2003) (PUNIT(I),I=K,KK)
         WRITE(N6,2004) (NDIM(I),I=K,KK)
         WRITE(N6,2006) (KALC(J,I),I=K,KK)
         WRITE(N6,2205) (JDTBC(KALC(J,I)+1),I=K,KK)
         WRITE(N6,2007) (KWASH(J,I),I=K,KK)
         WRITE(N6,2206) (JDTWC(KWASH(J,I)+1),I=K,KK)
         WRITE(N6,2008) (KACGUT(J,I),I=K,KK)
C#### WCH 4/12/93.  SHOULD PRINT JDFDB, NOT JDTBC.
         WRITE(N6,2207) (JDFDB(KACGUT(J,I)+1),I=K,KK)
         WRITE(N6,2009) (LINKUP(J,I),I=K,KK)
         WRITE(N6,2208) (JDLINK(LINKUP(J,I)+1),I=K,KK)
         WRITE(N6,2209) (QFACT(J,1,I),I=K,KK)
         WRITE(N6,2500) (QFACT(J,2,I),I=K,KK)
         WRITE(N6,2501) (QFACT(J,3,I),I=K,KK)
         WRITE(N6,2502) (QFACT(J,4,I),I=K,KK)
         WRITE(N6,2503) (QFACT(J,5,I),I=K,KK)
         WRITE(N6,2504) (WASHPO(J,I),I=K,KK)
         WRITE(N6,2505) (RCOEF(J,I),I=K,KK)
         WRITE(N6,2506) (CBFACT(J,I),I=K,KK)
         WRITE(N6,2507) (CONCRN(J,I),I=K,KK)
         WRITE(N6,2508) (REFF(J,I),I=K,KK)
         WRITE(N6,2509) (J,I=K,KK)
1990     CONTINUE
2000     CONTINUE
         ENDIF
C=======================================================================
C     Make the units conversion from Metric input to U.S. Customary units
C=======================================================================
      IF(METRIC.EQ.2) THEN
      DO 2700 K = 1,NQS
      DO 2690 J = 1,N1
      IF(KWASH(J,K).EQ.0) RCOEF(J,K) = RCOEF(J,K)*  25.4 ** WASHPO(J,K)
      IF(KWASH(J,K).GT.0) RCOEF(J,K) = RCOEF(J,K)*35.315 ** WASHPO(J,K)
C
      IF(KALC(J,K).EQ.0.OR.KALC(J,K).EQ.4) GO TO 2700
C
      IF(KACGUT(J,K).EQ.0) QFACT(J,1,K) = QFACT(J,1,K) * 2.205 / 32.808
      IF(KACGUT(J,K).EQ.1) QFACT(J,1,K) = QFACT(J,1,K) * 2.205 / 2.4710
      IF(KACGUT(J,K).EQ.2) QFACT(J,1,K) = QFACT(J,1,K) * 2.205
C
      IF(KALC(J,K).EQ.1) THEN
      IF(KACGUT(J,K).EQ.0) QFACT(J,3,K) = QFACT(J,3,K) * 2.205 / 32.808
C#### WCH (W. CHRUSCIEL), 9/28/93.  DIVIDE BY 2.471, NOT 32.808
      IF(KACGUT(J,K).EQ.1) QFACT(J,3,K) = QFACT(J,3,K) * 2.205 / 2.4710
      IF(KACGUT(J,K).EQ.2) QFACT(J,3,K) = QFACT(J,3,K) * 2.205
      ENDIF
 2690 CONTINUE
 2700 CONTINUE
      ENDIF
C#######################################################################
      DO 3070 I = 1,NQS
      DO 3070 J = 1,NQS
      IF(F1(I,J).NE.0.0) GO TO 3075
3070  CONTINUE
      GO TO 3099
3075  CONTINUE
C=======================================================================
C     Write the fractional constituent data.
C=======================================================================
      IF(IPRN(7).EQ.0) THEN
      WRITE(N6,3076)
      WRITE(N6,3077)
      WRITE(N6,3078) (PNAME(JJ),JJ=1,NQS)
      WRITE(N6,3094) (IDASH,ISTAR,KK=1,NQS)
C
      DO 3080 I = 1,NQS
      DO 3085 JTEST = 1,NQS
      IF(F1(I,JTEST).NE.0.0) GO TO 3086
3085  CONTINUE
      GO TO 3080
3086  WRITE(N6,3090) (ISTAR,KK=1,NQS)
      WRITE(N6,3091) (ISTAR,KK=1,NQS)
      WRITE(N6,3092) (ISTAR,KK=1,NQS)
      WRITE(N6,3093) PNAME(I),(F1(I,JJ),JJ=1,NQS)
      WRITE(N6,3094) (IDASH,ISTAR,KK=1,NQS)
3080  CONTINUE
      ENDIF
C=======================================================================
C     Read groundwater quality data if present on data group J5.
C=======================================================================
3099  CONTINUE
      IF(NOGWSC.GT.0) THEN
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>> READ DATA LINE J5 <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
                READ(N5,*,ERR=888) CC
                BACKSPACE N5
                IF(CC.EQ.'J5')THEN
                              READ(N5,*,ERR=888) CC,(CGWQ(K),K=1,NQS)
                              WRITE(N6,3010) (PNAME(K),
     +                                        CGWQ(K),PUNIT(K),K=1,NQS)
                              ENDIF
                ENDIF
C=======================================================================
C     Read infiltration/inflow quality data if present on data group J6.
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>> READ DATA LINE J6 <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
                IF(CC.EQ.'J6')THEN
                              READ(N5,*,ERR=888) CC,(CONCII(K),K=1,NQS)
                              WRITE(N6,3015) (PNAME(K),
     +                                      CONCII(K),PUNIT(K),K=1,NQS)
                              ENDIF
C=======================================================================
C     NWPGE is a line counter for the printout
C             of erosion and subcatchment data.
C=======================================================================
      NWPGE = 0
      IF(IROS.GT.0) THEN
C=======================================================================
C     Erosion input
C=======================================================================
C     Set the default values and default ratios for erosion.
C=======================================================================
      DO 4000 I = 1,5
      DEROS(I)  = 0.0
4000  REROS(I)  = 1.0
C=======================================================================
C     Print the erosion title data group depending on metric.
C=======================================================================
      IF(METRIC.EQ.1.AND.IPRN(7).EQ.0) WRITE(N6,4050)
      IF(METRIC.EQ.2.AND.IPRN(7).EQ.0) WRITE(N6,4051)
      NWPGE       = 13
      DO 5000  II = 1,1000
      NWPGE       = NWPGE + 1
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>> READ DATA GROUP K1 <<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'K1') THEN
                     IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,
     +                                    N,(EROS(K),K=1,5)
                     IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,
     +                                    KKN,(EROS(K),K=1,5)
                     ELSE
                     GO TO 5005
                     ENDIF
C=======================================================================
C     Set the default values for erosion.
C=======================================================================
      IF(N.NE.-2) GO TO 4065
      DO 4064 J = 1,5
      IF(EROS(J).GT.0.0) DEROS(J) = EROS(J)
4064  CONTINUE
      GO TO 5000
C=======================================================================
C     Set the new ratios for erosion
C=======================================================================
4065  IF(N.NE.-1) GO TO 4067
      DO 4066 J = 1,5
      IF(EROS(J).GT.0.0) REROS(J) = EROS(J)
4066  CONTINUE
      GO TO 5000
4067  DO 4080 J = 1,NOW
      IF(JCE.EQ.0.AND.N.NE.NAMEW(J)) GO TO 4080
      IF(JCE.EQ.1.AND.KKN.NE.KAMEW(J)) GO TO 4080
      K = J
      GO TO 4090
4080  CONTINUE
C=======================================================================
C     No match was found call ERROR subroutine.
C=======================================================================
      CALL ERROR(11)
C=======================================================================
C     Multiply by the ratios and assign default values.
C=======================================================================
4090  DO 4085 I = 1,5
      IF(EROS(I).LE.0.0) EROS(I) = DEROS(I)
4085  EROS(I)   = EROS(I) * REROS(I)
C=======================================================================
      ERODAR  = EROS(1)
      ERLEN   = EROS(2)
      SOILF   = EROS(3)
      CROPMF  = EROS(4)
      CONTPF  = EROS(5)
      ERLEN   = ERLEN  * CMET(1,METRIC)
      ERODAR  = ERODAR * CMET(2,METRIC)
C=======================================================================
C     The new values of ERLEN and ERODAR are used internally in
C     the runoff block.  The input values are saved in EROS(1)
C     and EROS(2) and will be printed on the output sheet.
C=======================================================================
C     Compute the slope length gradient ratio.
C=======================================================================
      SLGR = SQRT(ERLEN) * (0.0076 + 0.53 * WSLOPE(K) +
     1                             7.6 * WSLOPE(K)**2 )
C=======================================================================
C     COMPUTE THE PARTIAL USLE ( MINUS THE RAINFALL FACTOR)
C     UNITS ARE IN MILLIGRAMS
C     THE NUMBER 9.072E08 = 2000.0 * 453.6E03
C     THE UNITS ARE LBS/TON * MG/LBS.
C=======================================================================
      CNSTNT(K) = SLGR * SOILF * CROPMF * CONTPF * ERODAR * 9.072E08
C=======================================================================
      IF(IPRN(7).EQ.0) THEN
      IF(METRIC.EQ.1.AND.NWPGE.EQ.55) WRITE(N6,4050)
      IF(METRIC.EQ.2.AND.NWPGE.EQ.55) WRITE(N6,4051)
      IF(NWPGE.EQ.55) NWPGE = 13
      WRITE(N6,4070) N,EROS(1),EROS(2),SOILF,CROPMF,CONTPF,
     .           SLGR,CNSTNT(K)
      ENDIF
5000  CONTINUE
5005  IF(IPRN(7).EQ.0) WRITE(N6,4075)
C=======================================================================
C     Since IROS = 1 , add a new quality constituent.
C=======================================================================
      NQSS = NQS
      NQS  = NQS + 1
      ENDIF
C=======================================================================
C     Read the subcatchment quality data.
C=======================================================================
C     Set the default values and default ratios.
C=======================================================================
      DO 5020 I = 1,12
      DSUBQL(I) = 0.0
5020  RSUBQL(I) = 1.0
      DO 5030 I = 1,10
5030  RNQS(I)   = 0.0
      M         = 0
      SMCTCH    = 0.0
      GUTSM     = 0.0
C#### WCH, 11/15/93
      IEND      = NQSS + 2
C=======================================================================
      DO 6010 JJ = 1,600
      M          = M + 1
      IF(M.GT.NOW) GO TO 6020
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>>> Read data group L1 <<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,N,KL,(SUBQL(K),K=1,IEND)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KKN,KL,(SUBQL(K),K=1,IEND)
C=======================================================================
C     Alter ratios
C=======================================================================
      IF(N.NE.-1) GO TO 5070
      DO 5065 I = 1,IEND
5065  IF(SUBQL(I).NE.0.0) RSUBQL(I) = SUBQL(I)
      M = M - 1
      GO TO 6010
C=======================================================================
C     Alter the default values
C=======================================================================
5070  IF(N.NE.-2) GO TO 5080
      DO 5075 I = 1,IEND
5075  DSUBQL(I) = SUBQL(I)
      M = M - 1
      GO TO 6010
5080  DO 6000 K = 1,NOW
      IF(JCE.EQ.0.AND.N.NE.NAMEW(K))   GO TO 6000
      IF(JCE.EQ.1.AND.KKN.NE.KAMEW(K)) GO TO 6000
      KK = K
      GO TO 6005
6000  CONTINUE
      IF(JCE.EQ.0) WRITE(N6,6006)   N
      IF(JCE.EQ.1) WRITE(N6,6016) KKN
6005  DO 6007 I   = 1,12
      IF(SUBQL(I).LE.0.0) SUBQL(I) = DSUBQL(I)
6007  SUBQL(I)      = SUBQL(I) * RSUBQL(I)
C##### WCH, 4/19/93.  DON'T ALLOW ZERO FOR KL.
      IF(KL.LE.0.OR.KL.GT.NLU)  KL = 1
      KLAND(KK)     = KL
      BASINS(KK)    = SUBQL(1)
      GQLEN(KK)     = SUBQL(2)
C=======================================================================
C>>>>>>>>>>>>>>>>>>>>>>>>>> Read data group L2 <<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(IMUL.EQ.0) PLAND(KL,KK) = 1.0
      IF(IMUL.GT.0) READ(N5,*,ERR=888) CC,(PLAND(I,KK),I=1,N1)
      TOLAND     = 0.0
      DO 6008  I = 1,JLAND
      IF(IMUL.EQ.0.AND.I.NE.KL) PLAND(I,KK) = 0.0
      TOLAND     = TOLAND + PLAND(I,KK)
6008  CONTINUE
C##### WCH, 4/12/93.  RELAX CONSTRAINT ON SUM = 1.00000000000000000000.
      IF(ABS(TOLAND-1.0).GT.0.001) THEN
                        WRITE(N6,6300) KK
                        CALL ERROR(154)
                        ENDIF
C=======================================================================
      DO 6009  I    = 1,N1
C#### WCH, 11/15/93
      DO 6009  J    = 1,NQSS
C### WCH, 4/12/93.  REMOVE MULTIPLICATION BY PLAND.  DONE IN SUB QINT. 
6009  PSHED(I,J,KK) = SUBQL(J+2)
6010  CONTINUE
6020  CONTINUE
      IF(IMUL.EQ.0.AND.JLAND.GT.1) THEN
                                   DO 7100  K   = 1,NQS
                                   DO 7100  J   = 1,JLAND
                                   KALC(J,K)    = KALC(1,K)
                                   KWASH(J,K)   = KWASH(1,K)
                                   KACGUT(J,K)  = KACGUT(1,K)
                                   LINKUP(J,K)  = LINKUP(1,K)
                                   QFACT(J,1,K) = QFACT(1,1,K)
                                   QFACT(J,2,K) = QFACT(1,2,K)
                                   QFACT(J,3,K) = QFACT(1,3,K)
                                   QFACT(J,4,K) = QFACT(1,4,K)
                                   QFACT(J,5,K) = QFACT(1,5,K)
                                   WASHPO(J,K)  = WASHPO(1,K)
                                   RCOEF(J,K)   = RCOEF(1,K)
                                   CBFACT(J,K)  = CBFACT(1,K)
                                   CONCRN(J,K)  = CONCRN(1,K)
                                   REFF(J,K)    = REFF(1,K)
7100                               CONTINUE
                                   ENDIF
C=======================================================================
C     Do not write a new page heading if there is less than 55 lines
C                                 printed on the erosion output page.
C=======================================================================
      DO 6200 N = 1,NOW
      NWPGE     = NWPGE + 1
      IF(N.GT.1)       GO TO 6240
      IF(NWPGE.EQ.55)  GO TO 6100
      IF(IPRN(7).EQ.0) WRITE(N6,6029)
      GO TO 6040
C=======================================================================
C     Write a new page heading.
C=======================================================================
6100  IF(IPRN(7).EQ.0) WRITE(N6,6101)
6040  IF(IPRN(7).EQ.0) WRITE(N6,6102) (ISPACE,LL=1,NQSS)
      IF(IPRN(7).EQ.0) WRITE(N6,6103) (ISPACE,LL=1,NQSS)
C### WCH, 4/12/93.  MODIFY PRINT-OUT TO GIVE UNITS.
      IF(METRIC.EQ.1.AND.IPRN(7).EQ.0) WRITE(N6,6104) (ISPACE,LL=1,NQSS)
      IF(METRIC.EQ.2.AND.IPRN(7).EQ.0) WRITE(N6,6114) (ISPACE,LL=1,NQSS)
C=======================================================================
C     Write the descriptor line for GUTTER length depending
C                                    on the value of METRIC.
C=======================================================================
      IF(IPRN(7).EQ.0) THEN
             IF(METRIC.EQ.1) WRITE(N6,6105) (PNAME(LL),LL=1,NQSS)
             IF(METRIC.EQ.2) WRITE(N6,6106) (PNAME(LL),LL=1,NQSS)
             WRITE(N6,6109) (ISPACE,JJ=1,NQSS)
             ENDIF
      NWPGE = 10
6240  KOUT  = KLAND(N)
      IF(JCE.EQ.0.AND.IPRN(7).EQ.0) WRITE(N6,6250)
     +       N,NAMEW(N),LNAME(KOUT),KLAND(N),GQLEN(N),BASINS(N),
     +                                  (PSHED(1,K,N),K=1,NQSS)
      IF(JCE.EQ.1.AND.IPRN(7).EQ.0) WRITE(N6,6251)
     +       N,KAMEW(N),LNAME(KOUT),KLAND(N),GQLEN(N),BASINS(N),
     +                                  (PSHED(1,K,N),K=1,NQSS)
C=======================================================================
C     Sum the GUTTER lengths and the number of catchbasins for printout.
C=======================================================================
      SMCTCH    = SMCTCH   + BASINS(N)
      GUTSM     = GUTSM    + GQLEN(N)
      GQLEN(N)  = GQLEN(N) * CMET(10,METRIC)
      DO 6190 J = 1,N1
      DO 6190 K = 1,NQSS
C##### WCH, 4/12/93.  CONVERT TO TOTAL LOAD.
      IF(METRIC.EQ.1)
     * RNQS(K) = RNQS(K)+PSHED(J,K,N)*WAREA(N)/43560.*PLAND(J,N)
      IF(METRIC.EQ.2)
     * RNQS(K) = RNQS(K)+PSHED(J,K,N)*WAREA(N)/(43560.*2.471)*PLAND(J,N)
6190  CONTINUE
6200  CONTINUE
C=======================================================================
C     Write the gutter length and catchbasin sums.
C=======================================================================
C### WCH, 4/12/93. ALTER PRINT-OUT FOR METRIC.
      IF(METRIC.EQ.1.AND.IPRN(7).EQ.0) WRITE(N6,6270) 
     * GUTSM,SMCTCH,(RNQS(I),I=1,NQSS)
      IF(METRIC.EQ.2.AND.IPRN(7).EQ.0) WRITE(N6,6271) 
     * GUTSM,SMCTCH,(RNQS(I),I=1,NQSS)
C#######################################################################
C  WCH, 4/13/93.  PRINT OUT LAND USE FRACTIONS FROM GROUP L2
C=======================================================================
C     Do not write a new page heading if there are fewer than 45 lines
C                         printed on the last subcatchment output page.
C=======================================================================
      IF(IPRN(7).EQ.0.AND.IMUL.GT.0) THEN
           IF(NWPGE.GT.45) NWPGE = 55
           DO 6400 N = 1,NOW
           NWPGE     = NWPGE + 1
           IF(N.GT.1.AND.NWPGE.LE.55)  GO TO 6340
C=======================================================================
C     Write a new page heading.
C=======================================================================
           IF(NWPGE.LT.45) WRITE(N6,6500)
           IF(NWPGE.GE.45) WRITE(N6,6501)
           WRITE(N6,6502)
           WRITE(N6,6503) (LL,LL=1,N1)
           WRITE(N6,6504) (LNAME(LL),LL=1,N1)
           NWPGE = 10
6340       KOUT  = KLAND(N)
           IF(JCE.EQ.0) WRITE(N6,6505) N,NAMEW(N),(PLAND(LL,N),LL=1,N1)
           IF(JCE.EQ.1) WRITE(N6,6506) N,KAMEW(N),(PLAND(LL,N),LL=1,N1)
6400       CONTINUE
C     END LOOP FOR L2 FRACTIONS PRINT
           ENDIF
C=======================================================================
C     INITIALIZE WATERSHED POLLUTION LOADS.....
C=======================================================================
      CALL QINT
      RETURN
  888 CALL IERROR
C=======================================================================
1180  FORMAT(/,1H1,/,
     1' ###################################################',/,
     1' #              Quality Simulation                 #',/,
     1' ###################################################',/,
     1' #      General Quality Control Data Groups        #',/,
     1' ###################################################',//,
     22X,'Description',28X,'Variable',7X,'Value',/,2X,
     3'-----------',28X,'--------',7X,'-----',//,
     42X,'Number of quality constituents.....    NQS.......',I10,
     5//,2X,'Number of land uses................  JLAND.......',
     6I10,//,2X,'Standard catchbasin volume.........  CBVOL.......',
     7F10.2,' cubic feet',/)
1190  FORMAT(/,1H1,/,
     1' ###################################################',/,
     1' #              Quality Simulation                 #',/,
     1' ###################################################',/,
     1' #      General Quality Control Data Groups        #',/,
     1' ###################################################',//,
     22X,'Description',28X,'Variable',7X,'Value',/,2X,
     3'-----------',28X,'--------',7X,'-----',//,
     42X,'Number of quality constituents.....    NQS.......',I10,
     5//,2X,'Number of land uses................  JLAND.......',
     6I10,//,2X,'Standard catchbasin volume.........  CBVOL.......',
     7F10.2,' cubic meters',/)
1590  FORMAT(2X,'Erosion is not simulated.........    IROS........',
     1       I10,/)
1600  FORMAT(2X,'Erosion is simulated using the ',/,
     1       2X,'Universal soil loss equation.......  IROS........',
     2       I10,/)
1610  FORMAT(2X,'Erosion added to constituent....... IROSAD.......',
     1  I10,/)
1615  FORMAT(2X,'HIGHEST AVERAGE 30 MINUTE RAINFALL',/,2X,
     1           'INTENSITY DURING STORM OR YEAR..... RAINIT.......',
     1 F10.3,' IN/HR',/)
1616  FORMAT(2X,'HIGHEST AVERAGE 30 MINUTE RAINFALL',/,2X,
     1           'INTENSITY DURING STORM OR YEAR..... RAINIT.......',
     1 F10.3,' MM/HR',/)
1620  FORMAT(2X,'DRY DAYS PRIOR TO START OF STORM... DRYDAY.......',
     1  F10.2,' DAYS',/)
1623  FORMAT(/,' ====> ERROR !! DRYBSN HAS A',
     1      'VALUE OF ',F10.3,' AND HAS BEEN CHANGED TO 1.0',/)
1625  FORMAT(2X,'DRY DAYS REQUIRED TO RECHARGE',/,2X,
     1           'CATCHBASIN CONCENTRATION TO  ',/,2X,
     2           'INITIAL VALUES..................... DRYBSN.......',
     3 F10.2,' DAYS',/)
1630  FORMAT(2X,'DUST AND DIRT',/,
     1 2X,'STREET SWEEPING EFFICIENCY......... REFFDD.......',
     2 F10.3,/)
1635  FORMAT(2X,'DAY OF YEAR ON WHICH STREET ',/,2X,
     1           'SWEEPING BEGINS.................... KLNBGN.......',
     2 I10,//,2X,'DAY OF YEAR ON WHICH STREET',/,2X,
     3           'SWEEPING ENDS...................... KLNEND.......',
     4 I10,/)
1639  FORMAT(/,
     1' ###########################################',/,
     1' #     Land use data on data group J2      #',/,
     1' ###########################################',/)
 1640 FORMAT(1X,'                                                 ',
     1'            LIMITING                      CLEANING  AVAIL.',
     2'    DAYS SINCE',/,
     3'                                                              ',
     4'BUILDUP    BUILDUP  BUILDUP   INTERVAL  FACTOR       LAST',/,
     5'LAND USE   BUILDUP EQUATION TYPE   FUNCTIONAL DEPENDENCE OF',
     6'   QUANTITY   POWER    COEFF.    IN DAYS   FRACTION   SWEEPING',
     7/,'(LNAME)            (METHOD)        BUILDUP PARAMETER(JACGUT)',
     8'  (DDLIM)    (DDPOW)  (DDFACT)  (CLFREQ)  (AVSWP)     (DSLCL)',/,
     9'--------   ---------------------   -------------------------',
     1'  --------   -------  --------  --------  -------    ---------')
 1641 FORMAT(1X,A8,1X,A20,5X,A16,8X,1PE11.3,5(0PF10.3))
 1660 FORMAT(//,1H1,/
     1' ##############################################',/,
     1' #     Constituent data on data group J3      #',/,
     1' ##############################################',/)
2001  FORMAT(/,26X,5(6X,A8,6X))
2002  FORMAT(26X,5(5X,A1,'--------',6X))
2003  FORMAT(1X,'Constituent units........',5(6X,A8,6X))
2004  FORMAT(1X,'Type of units............',5(8X,I4,8X) )
2006  FORMAT(1X,'KALC.....................',5(8X,I4,8X))
2007  FORMAT(1X,'KWASH....................',5(8X,I4,8X))
2008  FORMAT(1X,'KACGUT...................',5(8X,I4,8X))
2009  FORMAT(1X,'LINKUP...................',5(8X,I4,8X))
C#### WCH 4/13/93.  WCH MONKEYED WITH THESE FORMATS.
2205  FORMAT(1X,'Type of buildup calcul...',5(2X,A16,2X))
2206  FORMAT(1X,'Type of washoff calcul...',5A20)
2207  FORMAT(1X,'Dependence of buildup....',5(2X,A16,2X))
2208  FORMAT(1X,'Linkage to snowmelt......',5(2X,A16,2X))
2209  FORMAT(1X,'Buildup param 1 (QFACT1).',5(4X,F12.3,4X))
2500  FORMAT(1X,'Buildup param 2 (QFACT2).',5(4X,F12.3,4X))
2501  FORMAT(1X,'Buildup param 3 (QFACT3).',5(4X,F12.3,4X))
2502  FORMAT(1X,'Buildup param 4 (QFACT4).',5(4X,F12.3,4X))
2503  FORMAT(1X,'Buildup param 5 (QFACT5).',5(4X,F12.3,4X))
2504  FORMAT(1X,'Washoff power (WASHPO)...',5(4X,F12.3,4X))
2505  FORMAT(1X,'Washoff coef. (RCOEF)....',5(4X,F12.3,4X))
2506  FORMAT(1X,'Init catchb conc (CBFACT)',5(4X,F12.3,4X))
2507  FORMAT(1X,'Precip. conc. (CONCRN)...',5(4X,F12.3,4X))
2508  FORMAT(1X,'Street sweep effic (REFF)',5(4X,F12.3,4X))
2509  FORMAT(1X,'Land use number..........',5(4X,I12,4X))
3010  FORMAT(/,
     +       ' *************************************************',/,
     +       ' * Constant Groundwater Quality Concentration(s) *',/,
     +       ' *************************************************',//,
     +       10(2X,A10,' has a concentration of..',F10.4,' ',A10,/))
3015  FORMAT(/,
     +       ' *************************************************',/,
     +       ' * Constant Infil/Inflw Quality Concentration(s) *',/,
     +       ' *************************************************',//,
     +       10(2X,A10,' has a concentration of..',F10.4,' ',A10,/))
3076  FORMAT(/,1H1,/,
     1' *******************************************',/,
     2' * Fractions for contributions from other  *',/,
     2' *     constituents on data group J4       *',/,
     1' *******************************************',//)
3077  FORMAT(15X,' CONSTITUENT FROM WHICH THE FRACTION IS COMPUTED',
     1  /,15X,' -----------------------------------------------')
3078  FORMAT(/,15X,'*',10(A8,1X,'*'),/)
3090  FORMAT(1X,'Fraction will *',10(9X,A1))
3091  FORMAT(1X,' be added to  *',10(9X,A1))
3092  FORMAT(1X,'  Constituent *',10(9X,A1))
3093  FORMAT(4X,A8,3X,'*',10(1X,F7.3,1X,'*'))
3094  FORMAT(15X,'*',10(A4,'-----',A1))
4050  FORMAT(/,1H1,T20,'*******************************',/,
     1           T20,'*      EROSION INPUT DATA     *',/,
     2           T20,'*        DATA GROUP K1        *',/,
     3           T20,'*******************************',//,
     4           T11,'SUBCAT',T20,'EROSION',T30,'  FLOW',T45,'  SOIL',
     5           T60,'CROPPING',T75,'CONTROL',T90,' SLOPE',T105,
     6               'PARTIAL',/,
     7           T10, 'NUMBER',T20,'AREA-ACRES',T30,' LENGTH-FT',T45,
     8               ' FACTOR',T60,' FACTOR',T75,' FACTOR',T90,
     9               'LENGTH',T105,' USLE *',/,T90,'GRAD-RATIO',/,
     1            T10,2('--------',2X),6('----------',5X))
4051  FORMAT(/,1H1,T20,'*******************************',/,
     1           T20,'*      EROSION INPUT DATA     *',/,
     2           T20,'*        DATA GROUP K1        *',/,
     3           T20,'*******************************',//,
     4           T11,'SUBCAT',T20,'EROSION',T30,'  FLOW',T45,'  SOIL',
     5           T60,'CROPPING',T75,'CONTROL',T90,' SLOPE',T105,
     6               'PARTIAL',/,
     7           T10, 'NUMBER',T20,' AREA-HA  ',T30,' LENGTH-M',T45,
     8               ' FACTOR',T60,' FACTOR',T75,' FACTOR',T90,
     9               'LENGTH',T105,' USLE *',/,T90,'GRAD-RATIO',/,
     1            T10,2('--------',2X),6('----------',5X))
4070  FORMAT(9X,I6,3X,2F10.1,5X,4(F10.3,5X),1PE10.2)
4075  FORMAT(/,' ===> PARTIAL USLE = ERODAR * SOILF * CROPMF * CONTPF *
     1SLGR * (9.072E8 MG/TON)')
6006  FORMAT(' ===> ERROR !!  NO MATCH WAS FOUND FOR',
     1       ' SUBCATCHMENT ',I10,' ON DATA GROUP L1',/,
     2           '                      WITH ANY SUBCATCHMENT ',
     3       'NUMBERS ON DATA GROUP H1.')
6016  FORMAT(' ===> ERROR !!  NO MATCH WAS FOUND FOR',
     1       ' SUBCATCHMENT ',A10,' ON DATA GROUP L1',/,
     2           '                      WITH ANY SUBCATCHMENT ',
     3       'NUMBERS ON DATA GROUP H1.')
C#### WCH, 4/13/93
6017  FORMAT(' $$$$$ WARNING!!!  FOR LAND USE',I2,' AND POLLUTANT',I3,
     1 ' WASHPO =',F8.2,/,' VALUES GREATER THAN APPROX. 25 LIKELY TO CAU
     2SE NUMERICAL PROBLEMS.',/,' E.G., EXTREMELY LARGE OR SMALL WASHOFF 
     3 VALUES.')
C#### WCH, 9/29/93.
 6018 FORMAT (' $$$$ WARNING! JLAND VALUE ON LINE J1 =',I2,' AND IS LESS
     1 THAN NUMBER OF J2 LINES.',/,' JLAND INCREASED TO',I3,' AND CONTIN
     2UE READING THE J2 LINES IN HOPES OF INPUTTING ALL LAND USE DATA.')
6029  FORMAT(//,
     1' *****************************************************',/,
     1' *     Subcatchment surface quality on data group L1 *',/,
     1' *****************************************************',/)
6101  FORMAT(/,1H1,/,
     1' *****************************************************',/,
     1' *     Subcatchment surface quality on data group L1 *',/,
     1' *****************************************************',/)
C#### WCH, 4/12/93.  ALTER FORMATS SLIGHTLY FOR METRIC LOAD UNITS
C                    AND WIDER SUBCAT. NO. FIELD. 
6102  FORMAT(//,1X,27X,'    Total   Number',10(A1,'  Input '),/)
6103  FORMAT(1X,22X,'Land',1X,'   Gutter     of  ',10(A1,' Loading'))
6104  FORMAT(1X,11X,'   Land     Use    Length   Catch-',
     .       10(A1,' load/ac'))
6114  FORMAT(1X,11X,'   Land     Use    Length   Catch-',
     .       10(A1,' load/ha'))
6105  FORMAT(1X,9X,'No.  Usage    No.   10**2ft   Basins  ',
     .       10(1X,A8))
6106  FORMAT(1X,9X,'No.  Usage    No.      Km     Basins  ',
     .       10(1X,A8))
6109  FORMAT(6X,'------',' --------','  ----','   --------',
     .          ' --------',10(A1,' -------'))
6250  FORMAT(1X,I4,I7,2X,A8,1X,I2,4X,2F9.2,10(1X,1PE8.1))
6251  FORMAT(1X,I4,1X,A7,1X,A8,1X,I2,4X,2F9.2,10(1X,1PE8.1))
6270  FORMAT(1X,'Totals (Loads in lb or other)',F8.2,F9.2,10(1X,1PE8.1))
6271  FORMAT(1X,'Totals (Loads in kg or other)',F8.2,F9.2,10(1X,1PE8.1))
6300  FORMAT(' ===> Problem with subcatchment # ',I6,' L2 line.')
6500  FORMAT(//,
     1' ********************************************************',/,
     1' *   Subcatchment land use fractions on data group L2   *',/,
     1' ********************************************************',/)
6501  FORMAT(/,1H1,/,
     1' ********************************************************',/,
     1' *   Subcatchment land use fractions on data group L2   *',/,
     1' ********************************************************',/)
6502  FORMAT(14X,'FRACTION FOR LAND USE NUMBER:')
6503  FORMAT(5X,'CATCHMENT',I6,4I10)
6504  FORMAT(1X,'NO.     NAME',5(2X,A8))
6505  FORMAT(I4,1X,I8,5F10.3)
6506  FORMAT(I4,1X,A8,5F10.3)
8000  FORMAT(1H1)
C=======================================================================
      END
