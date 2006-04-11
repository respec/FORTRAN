      SUBROUTINE COMB2(ICOMB)
C=======================================================================
C     COMB2 does the analysis for ICOMB option 4.
C     Read interface file and produce a readable ASCII version.
C     Updated to include additional header information on ASCII
C       output file.  WCH, 11/19/93.
C     Add option to print concentrations instead of loads on ASCII file.
C       WCH, 11/23/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'COMB.INC'
C#### WCH, 11/23/93
      CHARACTER NEWFIL*60,LOAD*8,FLOW*8,BLANK*8,FIRMT*4,FIR*4
      DIMENSION FIRMT(7),FIR(10)
      DATA LOAD/'  Load  '/,FLOW/'  Flow  '/,BLANK/'        '/
      DATA FIRMT/'(34X',',220','(I8,','3X,0','00(3','X,A8','))) '/
      DATA FIR/'01(3','02(3','03(3','04(3','05(3','06(3','07(3','08(3',
     1  '09(3','10(3'/
C=======================================================================
C     Write a formatted interface file (ICOMB=4).
C=======================================================================
      IF(ICOMB.EQ.4.AND.LAST.GT.0) THEN
               XTIM1 = 0.0
C=======================================================================
C     Read input interface file header information.
C=======================================================================
               CALL INFACE(1,LAST)
               INQUIRE(NEXT,NAME=NEWFIL)
               CLOSE(NEXT)
               OPEN(NEXT,FILE=NEWFIL,FORM='FORMATTED',STATUS='UNKNOWN')
C=======================================================================
C#### WCH, 11/19/93.  Print additional header info onto output file.
C                     Copy statements from Subroutine INFACE.FOR.
C=======================================================================
           WRITE(NEXT,1)  TITLE(1),TITLE(2)
           WRITE(NEXT,2)  TITLE(3),TITLE(4)
           WRITE(NEXT,3)  SOURCE
           WRITE(NEXT,4)  IDATEZ,TZERO
           WRITE(NEXT,5)  LOCATS,NQUAL,TRIBA,JCE
C=======================================================================
C     Sequence of location numbers and pollutant information.
C=======================================================================
           IF(JCE.EQ.0) THEN
                WRITE(NEXT,6) (NLOC(I),I=1,LOCATS)
                ELSE
                WRITE(NEXT,66) (KAN(I),I=1,LOCATS)
                ENDIF
           IF(NQUAL.GT.0) THEN
                WRITE(NEXT,7) (J,PNAME(J),PUNIT(J),NDIM(J),J=1,NQUAL)
                ENDIF
           WRITE(NEXT,8) QCONV
C#######################################################################
C#### WCH, 11/23/93.  SEVERAL OUTPUT FORMAT CHANGES.
                             KEND = LOCATS
               IF(NUMX.GT.0) KEND = NUMX
               IPTEST = KEND*(1+NQUAL)
               IF(IPTEST.GT.220) WRITE(N6,153) IPTEST
               IF(NQUAL.GT.0.AND.IPOLLU.EQ.0) WRITE(NEXT,157)
               WRITE(NEXT,154) NQUAL,KEND
               IF(NQUAL.EQ.0) THEN
                    IF(NUMX.GT.0.AND.JCE.EQ.0) WRITE(NEXT,155)
     +                              (NODEX(J),J=1,NUMX)
                    IF(NUMX.EQ.0.AND.JCE.EQ.0) WRITE(NEXT,155)
     +                              (NLOC(J),J=1,LOCATS)
                    IF(NUMX.GT.0.AND.JCE.EQ.1) WRITE(NEXT,156)
     +                              (KODEX(J),J=1,NUMX)
                    IF(NUMX.EQ.0.AND.JCE.EQ.1) WRITE(NEXT,156)
     +                              (KAN(J),J=1,LOCATS)
                    ELSE
                    IF(JCE.EQ.0) FIRMT(5) = FIR(NQUAL)
                    IF(NUMX.GT.0.AND.JCE.EQ.0) WRITE(NEXT,FIRMT)
     +                          (NODEX(J),(BLANK,I=1,NQUAL),J=1,NUMX)
                    IF(NUMX.EQ.0.AND.JCE.EQ.0) WRITE(NEXT,FIRMT)
     +                          (NLOC(J),(BLANK,I=1,NQUAL),J=1,LOCATS)
                    IF(NUMX.GT.0.AND.JCE.EQ.1) WRITE(NEXT,156)
     +                          (KODEX(J),(BLANK,I=1,NQUAL),J=1,NUMX)
                    IF(NUMX.EQ.0.AND.JCE.EQ.1) WRITE(NEXT,156)
     +                          (KAN(J),(BLANK,I=1,NQUAL),J=1,LOCATS)
                    ENDIF
               IF(NQUAL.LE.0) THEN
                  WRITE(NEXT,960)
                  ELSE
                  WRITE(NEXT,961) (FLOW,(PNAME(J),J=1,NQUAL),I=1,KEND)
                  IF(IPOLLU.EQ.0) WRITE(NEXT,962) (BLANK,
     1               (LOAD,J=1,NQUAL),I=1,KEND)
                  IF(IPOLLU.EQ.1) WRITE(NEXT,962) (BLANK,
     1               (PUNIT(J),J=1,NQUAL),I=1,KEND)
                  ENDIF
 100           IF(NQUAL.LE.0) READ(LAST,END=200) JDAY,TMDAY,
     +                        DELT,(QO1(I),I=1,LOCATS)
               IF(NQUAL.GT.0) READ(LAST,END=200) JDAY,TMDAY,
     +                  DELT,(QO1(I),(POLL1(J,I),J=1,NQUAL),I=1,LOCATS)
C=======================================================================
C     Extract only selected nodes.
C=======================================================================
C#### WCH, 11/23/93.  IN LOOPS BELOW, INCLUDE OPTION TO PRINT
C                     QUALITY CONCENTRATIONS INSTEAD OF LOADS.
C=======================================================================
              DO 150 J = 1,LOCATS
              IF(NUMX.GT.0) THEN
                  DO 160 K = 1,NUMX
                  IF(JCE.EQ.0.AND.NODEX(K).EQ.NLOC(J)) THEN
                     QO2(K) = QO1(J)
                     IF(NQUAL.GT.0) THEN
                        DO 170 KK   = 1,NQUAL
                        POLL2(KK,K) = POLL1(KK,J)
                        IF(IPOLLU.EQ.1.AND.QO2(K).GT.0.0) 
     1                      POLL2(KK,K) = POLL2(KK,K)/QO2(K)
  170                   CONTINUE
                        ENDIF
                     ENDIF
                  IF(JCE.EQ.1.AND.KODEX(K).EQ.KAN(J)) THEN
                     QO2(K) = QO1(J)
                     IF(NQUAL.GT.0) THEN
                        DO 175 KK   = 1,NQUAL
                        POLL2(KK,K) = POLL1(KK,J)
                        IF(IPOLLU.EQ.1.AND.QO2(K).GT.0.0) 
     1                      POLL2(KK,K) = POLL2(KK,K)/QO2(K)
  175                   CONTINUE      
                        ENDIF
                     ENDIF
  160             CONTINUE
                  ELSE
                  QO2(J) = QO1(J)
                  IF(NQUAL.GT.0) THEN
                     DO 180 KK   = 1,NQUAL
                     POLL2(KK,J) = POLL1(KK,J)
                     IF(IPOLLU.EQ.1.AND.QO2(J).GT.0.0) 
     1                  POLL2(KK,J) = POLL2(KK,J)/QO2(J)
  180                CONTINUE      
                     ENDIF
                  ENDIF
  150         CONTINUE
C=======================================================================
C     Write output file information.
C=======================================================================
                            XTIM1 = XTIM1 + DELT/3600.0
               IF(NQUAL.EQ.0) WRITE(NEXT,205) JDAY,TMDAY,
     +                        XTIM1,(QO2(I),I=1,KEND)
               IF(NQUAL.GT.0) WRITE(NEXT,205) JDAY,TMDAY,
     +            XTIM1,(QO2(I),(POLL2(J,I),J=1,NQUAL),I=1,KEND)
               GO TO 100
 200           CONTINUE
               RETURN
               ENDIF
C=======================================================================
    1 FORMAT(
     1' ###########################################',/,
     2' # Header information from interface file: #',/,
     3' ###########################################',//,
     4 ' Title from first computational block:',/,1X,A80,/,1X,A80)
    2 FORMAT(/,' Title from immediately preceding computational block:',
     1 /,1X,A80,/,1X,A80)
    3 FORMAT(/,
     1 ' Name of preceding block:................',A20)
    4 FORMAT(
     1 ' Initial Julian date (IDATEZ)......................',I8,/,
     2 ' Initial time of day in seconds (TZERO)............',F8.1)
    5 FORMAT(' No. transfered input locations....................',I8,/,
     1       ' No. transfered pollutants.........................',I8,/,
     2       ' Size of total catchment area (acres)..............',
     2 F10.2,
     3     /,' ID numbers (JCE=0) or alphanumeric (JCE=1)........',I8)
    6 FORMAT(/,' #################################################',/,
     1' # ID numbers of interface inlet locations:      # ',/,
     2' #################################################',/,(8I9))
    7 FORMAT(/,' #########################################',/,
     1' # Quality parameters on interface file: #',/,
     2' #########################################',//,
     3 ' No. Name      Units     Type of units',/,
     4 ' --- ----      -----     -------------',/,
     5 (1X,I2,2X,A8,2X,A8,I7))
    8 FORMAT (/,' Conversion factor to cfs for flow units',/,
     1           ' on interface file.  Multiply by: ',F11.5)
   66 FORMAT(/,' #################################################',/,
     1' # ID numbers of interface inlet locations:      # ',/,
     2' #################################################',/,
     3(8(1X,A10)))
C#### WCH, 11/19/93
  153 FORMAT(/,' WARNING!  No. of columns of flows/pollutants to be plac
     1ed on ASCII file > 220.',/,' No. columns = ',I3,' = no. locations 
     2* (1 + no. pollutants).',/,' Convenient tabular order may be lost 
     3due to wrap-around.')
  154 FORMAT(/,' ASCII Interface File from the COMBINE Block.',/,
     1 ' Data columns are:',/,
     2 ' 1. Julian day',/,
     3 ' 2. Time of day in seconds',/,
     4 ' 3. Elapsed time in hours since start of simulation',/,
     5 ' 4. Flow (cfs or cms) and ',I2,' pollutants repeated for the fol
     6lowing',I4,' nodes:')
  155 FORMAT(34X,220(1X,I8,2X))
  156 FORMAT(34X,220(3X,A8))
  157 FORMAT(/,' NOTE: Pollutant loads are listed with units of flow x c
     1oncentration,',/,' e.g., cfs x mg/l or cms x mg/l')
  205 FORMAT(I8,1PG11.4,G15.6,220(G11.4))
  960 FORMAT('  Julian   Time of   Elapsed Time       Flow(s)...',/,
     1       '     Day  Day (sec)     (hours)')
  961 FORMAT('  Julian   Time of   Elapsed Time',220(3X,A8))
  962 FORMAT('     Day  Day (sec)     (hours)  ',220(3X,A8))
C=======================================================================
      END
