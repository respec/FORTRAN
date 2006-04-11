      SUBROUTINE ERROR(K)
C=======================================================================
C     Subroutine Error last updated December, 1990 by Bob Dickinson.
C=======================================================================
      INCLUDE 'TAPES.INC'
C=======================================================================
      WRITE(*,9700)
      WRITE(N6,9500)
      IF(K.EQ.1) THEN
                 WRITE(*,1000)
                 WRITE(N6,1000)
                 STOP
                 ENDIF
      IF(K.EQ.2) THEN
                 WRITE(*,1002) NLU
                 WRITE(N6,1002) NLU
                 STOP
                 ENDIF
      IF(K.EQ.3) THEN
                 WRITE(*,1003)
                 WRITE(N6,1003)
                 STOP
                 ENDIF
      IF(K.EQ.4) THEN
                 WRITE(*,1004)
                 WRITE(N6,1004)
                 STOP
                 ENDIF
      IF(K.EQ.5) THEN
                 WRITE(*,1005)
                 WRITE(N6,1005)
                 STOP
                 ENDIF
      IF(K.EQ.6) THEN
                 WRITE(*,1006)
                 WRITE(N6,1006)
                 STOP
                 ENDIF
      IF(K.EQ.7) THEN
                 WRITE(*,1007)
                 WRITE(N6,1007)
                 STOP
                 ENDIF
      IF(K.EQ.8) THEN
                 WRITE(*,1008)
                 WRITE(N6,1008)
                 STOP
                 ENDIF
      IF(K.EQ.9) THEN
                 WRITE(*,1009)
                 WRITE(N6,1009)
                 STOP
                 ENDIF
      IF(K.EQ.11) THEN
                  WRITE(*,1011)
                  WRITE(N6,1011)
                  STOP
                  ENDIF
      IF(K.EQ.12) THEN
                  WRITE(*,70)
                  WRITE(N6,70)
                  STOP
                  ENDIF
      IF(K.EQ.13) THEN
                  WRITE(*,740) NG
                  WRITE(N6,740) NG
                  STOP
                  ENDIF
      IF(K.EQ.14) THEN
                  WRITE(*,878) NGW
                  WRITE(N6,878) NGW
                  STOP
                  ENDIF
      IF(K.EQ.15) THEN
                  WRITE(*,2080)
                  WRITE(N6,2080)
                  STOP
                  ENDIF
      IF(K.EQ.16) THEN
                  WRITE(*,2100)
                  WRITE(N6,2100)
                  STOP
                  ENDIF
      IF(K.EQ.17) THEN
                  WRITE(*,2120) NW
                  WRITE(N6,2120) NW
                  STOP
                  ENDIF
      IF(K.EQ.18) THEN
                  WRITE(*,907)
                  WRITE(N6,907)
                  STOP
                  ENDIF
      IF(K.EQ.19) THEN
                  WRITE(*,910)
                  WRITE(N6,910)
                  STOP
                  ENDIF
      IF(K.EQ.20) THEN
                  WRITE(*,920)
                  WRITE(N6,920)
                  STOP
                  ENDIF
      IF(K.EQ.100) THEN
                   WRITE(*,9100)
                   WRITE(N6,9100)
                   STOP
                   ENDIF
      IF(K.EQ.101) THEN
                   WRITE(*,9101)
                   WRITE(N6,9101)
                   STOP
                   ENDIF
      IF(K.EQ.102) THEN
                   WRITE(*,9102)
                   WRITE(N6,9102)
                   STOP
                   ENDIF
      IF(K.EQ.103) THEN
                   WRITE(*,9103)
                   WRITE(N6,9103)
                   STOP
                   ENDIF
      IF(K.EQ.104) THEN
                   WRITE(*,9104)
                   WRITE(N6,9104)
                   STOP
                   ENDIF
      IF(K.EQ.111) THEN
                   WRITE(*,9111)
                   WRITE(N6,9111)
                   STOP
                   ENDIF
      IF(K.EQ.112) THEN
                   WRITE(*,9112)
                   WRITE(N6,9112)
                   STOP
                   ENDIF
      IF(K.EQ.113) THEN
                   WRITE(*,9113)
                   WRITE(N6,9113)
                   STOP
                   ENDIF
      IF(K.EQ.114) THEN
                   WRITE(*,9114)
                   WRITE(N6,9114)
                   STOP
                   ENDIF
      IF(K.EQ.115) THEN
                   WRITE(*,9115)
                   WRITE(N6,9115)
                   STOP
                   ENDIF
      IF(K.EQ.116) THEN
                   WRITE(*,9116)
                   WRITE(N6,9116)
                   STOP
                   ENDIF
      IF(K.EQ.117) THEN
                   WRITE(*,9117)
                   WRITE(N6,9117)
                   STOP
                   ENDIF
      IF(K.EQ.118) THEN
                   WRITE(*,9118)
                   WRITE(N6,9118)
                   STOP
                   ENDIF
      IF(K.EQ.119) THEN
                   WRITE(*,9119)
                   WRITE(N6,9119)
                   STOP
                   ENDIF
      IF(K.EQ.130) THEN
                   WRITE(*,9130)
                   WRITE(N6,9130)
                   STOP
                   ENDIF
      IF(K.EQ.131) THEN
                   WRITE(*,9131)
                   WRITE(N6,9131)
                   STOP
                   ENDIF
      IF(K.EQ.140) THEN
                   WRITE(*,9140)
                   WRITE(N6,9140)
                   STOP
                   ENDIF
      IF(K.EQ.141) THEN
                   WRITE(*,9141)
                   WRITE(N6,9141)
                   STOP
                   ENDIF
      IF(K.EQ.142) THEN
                   WRITE(*,9142)
                   WRITE(N6,9142)
                   STOP
                   ENDIF
      IF(K.EQ.143) THEN
                   WRITE(*,9143)
                   WRITE(N6,9143)
                   STOP
                   ENDIF
      IF(K.EQ.150) THEN
                   WRITE(*,9150)
                   WRITE(N6,9150)
                   STOP
                   ENDIF
      IF(K.EQ.151) THEN
                   WRITE(*,9151)
                   WRITE(N6,9151)
                   STOP
                   ENDIF
      IF(K.EQ.152) THEN
                   WRITE(*,9152)
                   WRITE(N6,9152)
                   STOP
                   ENDIF
      IF(K.EQ.153) THEN
                   WRITE(*,9153)
                   WRITE(N6,9153)
                   STOP
                   ENDIF
      IF(K.EQ.154) THEN
                   WRITE(*,9154)
                   WRITE(N6,9154)
                   STOP
                   ENDIF
C#### WCH, 8/93.
      IF(K.EQ.155) THEN
                   WRITE(*,9155)
                   WRITE(N6,9155)
                   STOP
                   ENDIF
      IF(K.EQ.156) THEN
                   WRITE(*,9156)
                   WRITE(N6,9156)
                   STOP
                   ENDIF
      IF(K.EQ.157) THEN
                   WRITE(*,9157)
                   WRITE(N6,9157)
                   STOP
                   ENDIF
      IF(K.EQ.200) THEN
                   WRITE(*,9600)
                   WRITE(N6,9600)
                   STOP
                   ENDIF
C=======================================================================
  70  FORMAT(/,' ===> Error !!  Program stopped.  NSCRAT(2)',
     1' should not be equal to zero.')
 740  FORMAT(/,' ===> Error !!  The total number of assigned',
     1'channel/pipes/inlets, including dummies, ',/,
     2'                 exceeds the common storage block,',I5)
 878  FORMAT(/,' ===> Error !!  THE NUMBER OF SUBCATCHMENTS CALLING ',
     1' THE GROUNDWATER ROUTINE IS > ',I5)
 907  FORMAT(' Run stopped due to Error in groundwater input data.')
 910  FORMAT(' VARIABLE THISTO USED WHEN RAINFALL KYTPE = 0; STOP')
 920  FORMAT(/,' ===> Error !!  PROGRAM STOPPED.  NSCRAT(4)',
     1' SHOULD NOT BE EQUAL TO ZERO WHEN IPRN(2) = 0.')
1000  FORMAT(///,' JLAND CANNOT BE LESS THAN 1 - Check your input.')
1002  FORMAT(///,' JLAND CANNOT BE GREATER THAN NLU =',I3,
     1  ' - Check your input.')
1003  FORMAT(///,' NQS CANNOT BE LESS THAN 1 - Check your input.')
1004  FORMAT(///,' NQS CANNOT BE GREATER THAN 10 - Check your input.')
1005  FORMAT(///,' NQS CANNOT BE EQUAL TO 10 IF IROS EQUALS 1 -
     1  Check your input.')
1006  FORMAT(///,' DRYDAY IS LESS THAN ZERO AND YOU WANT TO RUN
     1 CONTINUOUS SWMM - ICRAIN EQUALS 1')
1007  FORMAT(///,' RAINIT IS LESS THAN ZERO AND IROS EQUALS 1 -
     1  CHANGE YOUR NUMBERS')
1008  FORMAT(///,' YOU HAVE AN INVALID BUILDUP EQUATION TYPE FOR
     1 LAND USE - Check your input')
1009  FORMAT(///,' YOU HAVE AN INVALID NUMBER FOR JACGUT - Check your
     1 input.')
1011  FORMAT(///,' SUBCATCHMENT NUMBER DOES NOT MATCH THE NAMES',
     1           ' IN ARRAY NAMEW - You were reading line K1')
2080  FORMAT('0$$$$$$$$$$ SIMULATION STOPPED DUE TO Error(S)',
     1'IN SNOW MELT INPUT DATA GIVEN ABOVE.')
2100  FORMAT(//,T10,'* * * * *  SIMULATION  STOPPED  * * * * *',//,
     1T10,' ISNOW =',I2,' THIS VARIABLE MUST BE SET',
     2' IN ACCORDANCE WITH',//,T10,'The runoff block line data',
     3' input write up.')
2120  FORMAT(/,' ===> Error !! SIMULATION STOPPED BECAUSE THE ',
     1' NUMBER OF SUBCATCHMENTS IS GREATER THAN THE ALLOWABLE',I5)
9100  FORMAT(/,' ===> JIN interface file is not defined.  It should',
     +               ' be greater than zero.')
9101  FORMAT(/,' ===> JOUT interface file is not defined.  It should',
     1               ' be greater than zero.')
9102  FORMAT(/,' ===> NUMX is not defined and extract option is used.',
     1               ' NUMX should be greater than zero.')
9103  FORMAT(/,' ===> NSCRAT(1) is not defined in the COMBINE Block.')
C#### WCH, 8/93.
9104  FORMAT(/,' ===> NSCRAT(3) is not defined in the RAIN Block,',/,
     1         '      and IFORM = 4 or 9 or 10.')
9111  FORMAT(/,' ===> IDECID LE 1 and output interface file is not',
     1         ' defined.')
9112  FORMAT(/,' ===> File NSCRAT(7) must be defined for conduit depth',
     1         ' printout.')
9113  FORMAT(/,' ===> You already have ten rainfall stations on your',/,
     1         '      JOUT file.')
9114  FORMAT(/,' ===> NSCRAT(2) is not defined in the RAIN Block.',/,
     1         '      The Rain Block is adding new information to',/,
     1         '      an existing interface file and needs file',/,
     1         '      NSCRAT(2) to hold the old rainfall time series.')
9115  FORMAT(/,' ===> NRGAG greater than rainfall stations on precipitat
     1ion file.')
9116  FORMAT(/,' ===> Some of your rainfall values are less than THISTO
     1units apart.')
9117  FORMAT(/,' ===> A JIN, JOUT, OR NSCRAT FILE uses file numbers 5, 6
     1 or 55.  This is not allowed.')
9118  FORMAT(/,' ===> NKLASS is greater than 2 on data group C1.')
9119  FORMAT(/,' ===> NUMST on line E1 is greater than allowed by NVST o
     1n the parameter statement on TAPES.INC')
9130  FORMAT(/,' ===> Input interface file is le zero in Stats.')
9131  FORMAT(/,' ===> Output interface file is le zero in Stats.')
9140  FORMAT(/,' ===> No J1 line was found in Runoff input file.')
9141  FORMAT(/,' ===> No J2 line was found in Runoff input file.')
9142  FORMAT(/,' ===> No J3 line was found in Runoff input file.')
9143  FORMAT(/,' ===> IMUL less than zero on line JJ.')
9150  FORMAT(/,' ===> IVAP = 3 allows only 600 values of evaporation.')
9151  FORMAT(/,' ===> Your starting year for evaporation <   0.')
9152  FORMAT(/,' ===> Your starting year for evaporation > 100.')
9153  FORMAT(/,' ===> Index problem with evaporation in WSHED.')
9154  FORMAT(/,' ===> Land fractions do not total to 1.0 in QHYDRO.')
9155  FORMAT(/,' ===> ERROR. NSCRAT(4) = 0 in Rain Block and 15-min.data
     1 are being processed.',/,'     Run stopped.')
9156  FORMAT(/,' ===> ERROR. New data in Rain Block are for 15-min. inte
     1rvals,',/,'     but data to be combined on unit JOUT are not.  Run
     2 stopped.')
9157  FORMAT(/,' ===> ERROR. New data in Rain Block are for hourly inter
     1vals,',/,'     but data to be combined on unit JOUT are not.  Run
     2stopped.')
9500  FORMAT(/,' ===> A @#$!@#@ error in your input was found,',/,
     1       '       the simulation will end for the following reason.')
9600  FORMAT(/,' ===> A @#$!@#@ error in your input was found,',/,
     1        '      the simulation ended for the above reason.')
9700  FORMAT(' ################################################',/,
     1       ' #  A fatal ERROR was found in your data input. #',/,
     2       ' ################################################',/)
C=======================================================================
      STOP
      END
