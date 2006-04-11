      SUBROUTINE COMBIN
C	COMBINE BLOCK
C=======================================================================
C     Rewritten   June 1988 by R. Dickinson
C     Updated      May 1989 by R.E.D.
C     Updated November 1990 by Laura Terrell, CDM
C     Updated December 1990 by R.E.D.
C     Updated   August 1992 by WCH and May 1993 by RED
C     Add error message, WCH, 8/93.
C     Make combine and collate options compatible with alphanumeric 
C       labels and minor format changes, WCH, 11/19/93.
C     Add option for printing concentrations instead of loads on 
C       ASCII file, WCH, 11/23/93.
C     Create readable ASCII version of rainfall interface file, WCH, 
C       7/25/96.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'COMB.INC'
C=======================================================================
      CHARACTER KODEOT*10
      INTEGER   SECOND,FIRST,ILAG
C#######################################################################
C REMOVE DOUBLE PRECISION FROM DELT, DELT1, DELT2,   WCH, 8/28/92
C     DOUBLE PRECISION DELT, DELT1,DELT2,DIFF,TDIFF
C AND FROM DIFF, TDIFF,  RED 5/28/93
C     DOUBLE PRECISION DIFF,TDIFF
C#######################################################################
      LOGICAL*1 OK1,OK2
C=======================================================================
C     This subroutine has six main objectives:
C
C     ICOMB      => IF = 0, COLLATE OPTION
C                   IF = 1, COMBINE OPTION
C                   IF = 2, EXTRACT (AND OPTIONALLY RENUMBER)
C                           NODES FROM A SINGLE INPUT FILE.
C                   IF = 3, READ FILE HEADER
C                   IF = 4, CREATE ASCII FILE FROM BINARY INTERFACE FILE
C                   IF = 5, CALCULATE THE SIMPLE STATISTICS (SUMS) OF
C                           AN INTERFACE FILE
C                   IF = 6, CALCULATE THE SIMPLE STATISTICS (SUMS) OF
C                           A RAIN BLOCK INTERFACE FILE
C#### WCH, 7/25/96. 
C                   IF = 7, SAME AS 6, PLUS CREATE ASCII FILE OF 
C                           RAINFALL INTERFACE FILE.  
C=======================================================================
C     Define statement function for linear interpolation.
C=======================================================================
CIM ITS NOT USED
CIM      QLINTP(Q1,Q2,T1,T2,T) = Q1 + (Q2-Q1)*(T-T1)/(T2-T1)
C=======================================================================
      WRITE(N6,10)
      WRITE(*,10)
C=======================================================================
C     Initialization.
C=======================================================================
      INCNT      = INCNT  + 1
      IOUTCT     = IOUTCT + 1
      LAST       = JIN(INCNT)
      NEXT       = JOUT(IOUTCT)
      SECOND     = NSCRAT(1)
      IF(LAST.LE.0) CALL ERROR(100)
C=======================================================================
C     Open files for the Combine Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C#### WCH, 7/25/96.  CHECK FOR POSSIBLE FILE FORM MIS-MATCH, BECAUSE
C     JOUT MIGHT BE USED FOR FORMATTED FILE.
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +   FFNAME(25+IOUTCT).EQ.'JIN.UF')) THEN
            OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
            ELSE      
            CALL FILECK(JOUT(IOUTCT),FFNAME(25+IOUTCT),'UNFORMATTED')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +   FFNAME(25+IOUTCT).NE.'JIN.UF')
     +   OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +   STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
            ENDIF
      IF(NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') OPEN(NSCRAT(1),
     +             FILE=FFNAME(51),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF') OPEN(NSCRAT(1),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
C=======================================================================
C     Data initialization.
C=======================================================================
C#### WCH, 11/23/93
      IPOLLU     = 0
CIMT  Change upper range from 10 to MQUAL
      DO 8 J     = 1,MQUAL
      NDIM(J)    = 0
      NDIM2(J)   = 0
      PNAM1(J)   = ' '
      PNAM2(J)   = ' '
      PUNIT(J)   = ' '
      PUNIT2(J)  = ' '
      PUNIT1(J)  = ' '
      NDIM1(J)   = 0
      DO 8 I     = 1,NIE
      CPOLL(J,I) = 0.0
      POLL1(J,I) = 0.0
      POLL2(J,I) = 0.0
      POLL3(J,I) = 0.0
      POLD1(J,I) = 0.0
      POLD2(J,I) = 0.0
      IF(J.GT.1) GO TO 8
      QO1(I)     = 0.0
      QO2(I)     = 0.0
      QO3(I)     = 0.0
      QQO(I)     = 0.0
      QOLD1(I)   = 0.0
      QOLD2(I)   = 0.0
      INPOS1(I)  = 0
      INPOS2(I)  = 0
      JCOMB(I)   = 0
    8 CONTINUE
C=======================================================================
C>>>>>READ DATA GROUP A1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,ICOMB
      WRITE(N6,71) ICOMB
      IF(NEXT.LE.0.AND.ICOMB.LT.2) CALL ERROR(101)
C#### WCH, 7/25/96.
C####      IF(NEXT.LE.0.AND.ICOMB.EQ.4) CALL ERROR(101)
      IF(NEXT.LE.0.AND.(ICOMB.EQ.4.OR.ICOMB.EQ.7)) CALL ERROR(101)
C=======================================================================
      IF(ICOMB.EQ.3.OR.ICOMB.GT.4) THEN
                                   CALL COMB1(ICOMB)
                                   WRITE(N6,9000)
                                   WRITE(*,9000)
                                   RETURN
                                   ENDIF
C=======================================================================
C     End of Combine Block if ICOMB = 3, 5, 6 or 7. 
C=======================================================================
C     Read remaining Combine Block input.
C=======================================================================
C>>>>>READ DATA GROUP B1<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(3)
      READ(N5,*,ERR=888) CC,TITLE(4)
      WRITE(N6,660)         TITLE(3),TITLE(4)
C=======================================================================
C>>>>>READ DATA GROUP B2<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
C     NODEOT => Node number for output.
C     KODEOT => Node name   for output.
C#### WCH, 11/23/93.
C     NPOLL  => Number of pollutants to be placed on new file.
C     If NPOLL < 0, then for ICOMB = 4, print concentrations on ASCII
C       file, not loads.
C=======================================================================
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,NODEOT,NPOLL
C#### WCH, 11/23/93.
                   IF(NPOLL.LT.0) THEN
                      IPOLLU = 1
                      NPOLL = -NPOLL
                      ENDIF
C#### WCH, 11/19/93.  OUTPUT DATA SET = JOUT, NOT JIN
                   WRITE(N6,410)         NODEOT,JOUT(IOUTCT),NPOLL
                   ELSE
C#### WCH, 8/2/93. ADD ADDITIONAL ERROR MESSAGE.  GO TO 887.
                   READ(N5,*,ERR=887) CC,KODEOT,NPOLL
C#### WCH, 11/23/93.
                   IF(NPOLL.LT.0) THEN
                      IPOLLU = 1
                      NPOLL = -NPOLL
                      ENDIF
                   WRITE(N6,411)         KODEOT,JOUT(IOUTCT),NPOLL
                   ENDIF
	IF (NPOLL.GT.MQUAL) THEN
	WRITE(N6,*) 'ERROR *** NPOLL IS GREATER THAN MAXIMUM ALLOWED.'
	STOP 'ERROR *** NPOLL IS GREATER THAN MAXIMUM ALLOWED.'
	ENDIF
C#### WCH, 11/23/93
      IF(ICOMB.EQ.4.AND.NPOLL.GT.0) THEN
           IF(IPOLLU.EQ.0) WRITE(N6,415)
           IF(IPOLLU.EQ.1) WRITE(N6,416)
           ENDIF
C=======================================================================
C>>>>>READ DATA GROUP B3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     READ(N5,*,ERR=888) CC,(NPOS1(J),NPOS2(J),J=1,NPOLL)
                     WRITE(N6,420)       (J,NPOS1(J),NPOS2(J),J=1,NPOLL)
                     ENDIF
C=======================================================================
C>>>>>READ DATA GROUP C1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NUMX,NUMR
      WRITE(N6,430)         NUMX,NUMR
C=======================================================================
C>>>>>READ DATA GROUP C2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(NUMX.GT.0) THEN
              IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,(NODEX(I),I=1,NUMX)
C#### WCH, 8/2/93. ADD ADDITIONAL ERROR MESSAGE.  GO TO 887.
              IF(JCE.EQ.1) READ(N5,*,ERR=887) CC,(KODEX(I),I=1,NUMX)
              WRITE(N6,440)
              IF(JCE.EQ.0) WRITE(N6,450) (NODEX(I),I=1,NUMX)
              IF(JCE.EQ.1) WRITE(N6,451) (KODEX(I),I=1,NUMX)
              ENDIF
C=======================================================================
C>>>>>READ DATA GROUP C3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(NUMR.GT.0) THEN
              IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,(NODER(I),I=1,NUMR)
C#### WCH, 8/2/93. ADD ADDITIONAL ERROR MESSAGE.  GO TO 887.
              IF(JCE.EQ.1) READ(N5,*,ERR=887) CC,(KODER(I),I=1,NUMR)
              WRITE(N6,445)
              IF(JCE.EQ.0) WRITE(N6,450) (NODER(I),I=1,NUMR)
              IF(JCE.EQ.1) WRITE(N6,451) (KODER(I),I=1,NUMR)
              ENDIF
C=======================================================================
C     Extract and renumber an interface file (ICOMB=2).
C=======================================================================
      IF(ICOMB.EQ.2.AND.NUMX.EQ.0) CALL ERROR(102)
      IF(ICOMB.EQ.2) THEN
               CALL INFACE(1,LAST)
C=======================================================================
C     Transfer information to file NEXT.
C=======================================================================
               REWIND NEXT
               WRITE(NEXT) NUMX,NQUAL
               IF(JCE.EQ.0.AND.NUMR.GT.0) WRITE(NEXT)(NODER(I),I=1,NUMX)
               IF(JCE.EQ.0.AND.NUMR.EQ.0) WRITE(NEXT)(NODEX(I),I=1,NUMX)
               IF(JCE.EQ.1.AND.NUMR.GT.0) WRITE(NEXT)(KODER(I),I=1,NUMX)
               IF(JCE.EQ.1.AND.NUMR.EQ.0) WRITE(NEXT)(KODEX(I),I=1,NUMX)
               SOURCE = 'COMBINE BLOCK'
               CALL INFACE(2,NEXT)
C=======================================================================
C     Read input interface file information.
C=======================================================================
  50           IF(NQUAL.LE.0) READ(LAST,END=90) JDAY,TMDAY,
     +                        DELT,(QO1(I),I=1,LOCATS)
               IF(NQUAL.GT.0) READ(LAST,END=90) JDAY,TMDAY,
     +                  DELT,(QO1(I),(POLL1(J,I),J=1,NQUAL),I=1,LOCATS)
                   IF(JDAY.EQ.99999) JDAY = 9999999
                   JYEAR = JDAY/1000
                   IF (JYEAR.LT.100) THEN
                   JDAY = JDAY - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JDAY = JDAY + JYEAR*1000
                   ENDIF
C=======================================================================
C     Extract only selected nodes.
C=======================================================================
               DO 55 J = 1,LOCATS
               DO 60 K = 1,NUMX
               IF(JCE.EQ.0.AND.NODEX(K).EQ.NLOC(J)) THEN
                                           QO2(K) = QO1(J)
                                           IF(NQUAL.GT.0) THEN
                                           DO 70 KK    = 1,NQUAL
   70                                      POLL2(KK,K) = POLL1(KK,J)
                                           ENDIF
                                           ENDIF
               IF(JCE.EQ.1.AND.KODEX(K).EQ.KAN(J)) THEN
                                           QO2(K) = QO1(J)
                                           IF(NQUAL.GT.0) THEN
                                           DO 75 KK    = 1,NQUAL
   75                                      POLL2(KK,K) = POLL1(KK,J)
                                           ENDIF
                                           ENDIF
   60          CONTINUE
   55          CONTINUE
C=======================================================================
C     Write output file information.
C=======================================================================
               IF(NQUAL.EQ.0) WRITE(NEXT) JDAY,TMDAY,
     +                        DELT,(QO2(I),I=1,NUMX)
               IF(NQUAL.GT.0) WRITE(NEXT) JDAY,TMDAY,
     +            DELT,(QO2(I),(POLL2(J,I),J=1,NQUAL),I=1,NUMX)
               GO TO 50
  90           WRITE(N6,9000)
               WRITE(*,9000)
               RETURN
               ENDIF
C=======================================================================
C     End of ICOMB = 2 option.
C=======================================================================
      IF(ICOMB.EQ.4) THEN
                     CALL COMB2(ICOMB)
                     WRITE(N6,9000)
                     WRITE(*,9000)
                     RETURN
                     ENDIF
C=======================================================================
C     End of ICOMB = 4 option.
C=======================================================================
C     COLLATE (ICOMB=0) and COMBINE (ICOMB=1) options are left.
C     The pollutant names and units are saved from the first
C     data-set and written on the output data-set.
C=======================================================================
C     LOCAT1 = Total number of inlets on first input data set.
C     LOCAT2 = Total number of inlets on second input data set.
C     NQ1    = Total number of water quality constituents on 1st data set
C     NQ2    = Total number of water quality constituents on 2nd data set
C=======================================================================
C     Read file headers from file # 2.
C=======================================================================
      IF(SECOND.LE.0) CALL ERROR(103)
      WRITE(N6,9005)
      CALL INFACE(1,SECOND)
      LOCAT2   = LOCATS
      NQ2      = NQUAL
      DO 210 I = 1,LOCAT2
C#### WCH, 11/19/93.  FIX FOR ALPHA LABELS.
      IF(JCE.EQ.0) THEN
           JUNC2(I) = NLOC(I)
           ELSE
           KUNC2(I) = KAN(I)
           ENDIF
      IF(NQ2.GT.0) THEN
                   DO 220 J  = 1,NQ2
                   PNAM2(J)  = PNAME(J)
                   PUNIT2(J) = PUNIT(J)
  220              NDIM2(J)  = NDIM(J)
                   ENDIF
  210 CONTINUE
C=======================================================================
C     Add the tributary areas together in variable TRIBBA.
C=======================================================================
      TRIBBA = 0.0
      TRIBBA = TRIBBA + TRIBA
C=======================================================================
C     Read file headers from file # 1.
C=======================================================================
      IF(LAST.LE.0) CALL ERROR(103)
      WRITE(N6,9010)
      CALL INFACE(1,LAST)
      LOCAT1   = LOCATS
      NQ1      = NQUAL
      DO 230 I = 1,LOCAT1
C#### WCH, 11/19/93.  FIX FOR ALPHA LABELS.
      IF(JCE.EQ.0) THEN
           JUNC1(I) = NLOC(I)
           ELSE
           KUNC1(I) = KAN(I)
           ENDIF
      IF(NQ1.GT.0) THEN
                   DO 240 J  = 1,NQ1
                   PNAM1(J)  = PNAME(J)
                   PUNIT1(J) = PUNIT(J)
  240              NDIM1(J)  = NDIM(J)
                   ENDIF
  230 CONTINUE
      TRIBA  = TRIBBA + TRIBA
C=======================================================================
C     Determine the names of the quality constituents on the new
C     interface file.  The names will be duplicates of the names on
C     the first input file except for the case in which
C     NPOS1(KPOLL) equals zero, in which case the names will be copied
C     from the second input file.  
C=======================================================================
      IF(NPOLL.GT.0) THEN
                     DO 470 KPOLL = 1,NPOLL
                     IF(NPOS1(KPOLL).LE.0) GO TO 460
                     K1           = NPOS1(KPOLL)
                     PNAME(KPOLL) = PNAM1(K1)
                     PUNIT(KPOLL) = PUNIT1(K1)
                     NDIM(KPOLL)  = NDIM1(K1)
                     GO TO 470
  460                K2 = NPOS2(KPOLL)
C=======================================================================
C     Error if both NPOS1 and NPOS2 = 0.
C=======================================================================
                     IF(K2.LE.0) GO TO 2200
                     PNAME(KPOLL)   = PNAM2(K2)
                     PUNIT(KPOLL)   = PUNIT2(K2)
                     NDIM(KPOLL)    = NDIM2(K2)
  470                CONTINUE
                     ENDIF
C=======================================================================
C     Read the first line of the two interface files.
C=======================================================================
      IF(NQ1.EQ.0) THEN
                   READ(LAST) JULDAY,TIMDAY,DELT1,(QO1(I),I=1,LOCAT1)
                   IF(JULDAY.EQ.99999) JULDAY = 9999999
                   JYEAR = JULDAY/1000
                   IF (JYEAR.LT.100) THEN
                   JULDAY = JULDAY - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JULDAY = JULDAY + JYEAR*1000
                   ENDIF
                   ELSE
                   READ(LAST) JULDAY,TIMDAY,DELT1,
     +                        (QO1(I),(POLL1(J,I),J=1,NQ1),I=1,LOCAT1)
                   IF(JULDAY.EQ.99999) JULDAY = 9999999
                   JYEAR = JULDAY/1000
                   IF (JYEAR.LT.100) THEN
                   JULDAY = JULDAY - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JULDAY = JULDAY + JYEAR*1000
                   ENDIF
                   ENDIF
      IF(NQ2.EQ.0) THEN
                   READ(SECOND) JDAY2,TMDAY2,DELT2,(QO2(I),I=1,LOCAT2)
                   IF(JDAY.EQ.99999) JDAY = 9999999
                   JYEAR = JDAY2/1000
                   IF (JYEAR.LT.100) THEN
                   JDAY2 = JDAY2 - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JDAY2 = JDAY2 + JYEAR*1000
                   ENDIF
                   ELSE
                   READ(SECOND) JDAY2,TMDAY2,DELT2,
     +                          (QO2(I),(POLL2(J,I),J=1,NQ2),I=1,LOCAT2)
                   IF(JDAY2.EQ.99999) JDAY2 = 9999999
                   JYEAR = JDAY2/1000
                   IF (JYEAR.LT.100) THEN
                   JDAY2 = JDAY2 - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JDAY2 = JDAY2 + JYEAR*1000
                   ENDIF
                   ENDIF
C=======================================================================
C     Determine TZERO and IDATEZ for the new interface file.
C=======================================================================
      CALL NTIME(JDAY2,TMDAY2,DIFF)
      IF(DIFF.GE.0.0) THEN
                      TZERO  = TIMDAY
                      FIRST  = 1
                      IDATEZ = JULDAY
                      ENDIF
      IF(DIFF.LT.0.0) THEN
                      TZERO  = TMDAY2
                      FIRST  = 2
                      IDATEZ = JDAY2
                      ENDIF
C=======================================================================
C     If ICOMB equals 0 and the collate option is requested, it is
C     necessary to interleave the file locations from input files
C     one and two.  If the two files include identical location
C     numbers in their inlet arrays, the flow and quality constituent
C     values (loads = flow x concentration) will be added together for 
C     those locations.  If the inlet number appears in only one file, 
C     the flow and quality constituent values will be transfered 
C     intact to the new interface file.
C=======================================================================
C    The new interface file's location numbers will be arranged in the
C    following order:  First --> locations (inlets) in both input files
C                      Second -> locations in file one not 
C                                already transfered
C                      Third --> locations in file two not
C                                already transfered
C========================================================================
C     NPOSIT    = INLET position on the new interface file.
C     INPOS1    = INLET position on input file 1 corresponding
C                 to NPOSIT position on the new interface file.
C     INPOS2    = INLET position on input file 2 corresponding
C                 to NPOSIT position on the new interface file.
C     JUNC1     = INLET array from input file 1.
C     JUNC2     = INLET array from input file 2.
C     NLOC      = INLET array for the new interface file.
C     LOCATS    = Number of inlets on both files 1 and 2.
C=======================================================================
C     Find the INLET locations that are on both interface files.
C=======================================================================
      NPOSIT         = 0
      DO 575 I       = 1,LOCAT1
      DO 550 N       = 1,LOCAT2
C#### WCH, 11/19/93
      IF(JCE.EQ.0.AND.JUNC1(I).NE.JUNC2(N)) GO TO 550
      IF(JCE.EQ.1.AND.KUNC1(I).NE.KUNC2(N)) GO TO 550
      NPOSIT         = NPOSIT + 1
      INPOS1(NPOSIT) = I
      INPOS2(NPOSIT) = N
C#### WCH, 11/19/93
      IF(JCE.EQ.0) NLOC(NPOSIT)   = JUNC1(I)
      IF(JCE.EQ.1) KAN(NPOSIT)    = KUNC1(I)
      GO TO 575
  550 CONTINUE
  575 CONTINUE
C=======================================================================
C     Find the INLET locations that are only on interface file # 1.
C=======================================================================
      LOCATS         = NPOSIT
      DO 625  I      = 1,LOCAT1
      DO 600  N      = 1,LOCATS
C#### WCH, 11/19/93
      IF(JCE.EQ.0.AND.NLOC(N).EQ.JUNC1(I)) GO TO 625
      IF(JCE.EQ.1.AND.KAN(N).EQ.KUNC1(I))  GO TO 625
  600 CONTINUE
      NPOSIT         = NPOSIT + 1
C#### WCH, 11/19/93
      IF(JCE.EQ.0) NLOC(NPOSIT) = JUNC1(I)
      IF(JCE.EQ.1) KAN(NPOSIT)  = KUNC1(I)
      INPOS1(NPOSIT) = I
      INPOS2(NPOSIT) = 0
  625 CONTINUE
C=======================================================================
C     Find the INLET locations that are only on interface file # 2.
C=======================================================================
      LOCATS         = NPOSIT
      DO 675 I       = 1,LOCAT2
      DO 650 N       = 1,LOCATS
C#### WCH, 11/19/93
      IF(JCE.EQ.0.AND.NLOC(N).EQ.JUNC2(I)) GO TO 675
      IF(JCE.EQ.1.AND.KAN(N).EQ.KUNC2(I))  GO TO 675
  650 CONTINUE
      NPOSIT         = NPOSIT + 1
C#### WCH, 11/19/93
      IF(JCE.EQ.0) NLOC(NPOSIT) = JUNC2(I)
      IF(JCE.EQ.1) KAN(NPOSIT)  = KUNC2(I)
      INPOS1(NPOSIT) = 0
      INPOS2(NPOSIT) = I
  675 CONTINUE
      LOCATS         = NPOSIT
C=======================================================================
C     Now create another tester array - JCOMB(200)
C         If JCOMB = 1 then both files one and two have an inlet
C                      corresponding to NPOSIT on the new interface file
C         If JCOMB = 2 then only the first file has an inlet
C                      corresponding to NPOSIT on the new interface file
C         If JCOMB = 3 then only the second file has an inlet
C                      corresponding to NPOSIT on the new interface file
C=======================================================================
      DO 700 NPOSIT = 1,LOCATS
      IF(INPOS1(NPOSIT).GT.0.AND.INPOS2(NPOSIT).GT.0) JCOMB(NPOSIT) = 1
      IF(INPOS1(NPOSIT).GT.0.AND.INPOS2(NPOSIT).LE.0) JCOMB(NPOSIT) = 2
      IF(INPOS1(NPOSIT).LE.0.AND.INPOS2(NPOSIT).GT.0) JCOMB(NPOSIT) = 3
  700 CONTINUE
C=======================================================================
C     The next statements (and the later summation) are essentially
C     the only difference between the Combine option and the Collate
C     option.  The Combine and Collate options have the same input
C     requirements except that for Combine, the sumation sequence
C     sums all the flows and loads from all input locations (inlets)
C     and outputs these summations at location NODEOT.
C=======================================================================
      IF(ICOMB.EQ.1) THEN
                     KPOSIT  = 1
C=======================================================================
C     Offer a default output location ID.
C=======================================================================
                     IF(NODEOT.LE.0)   NODEOT =  12345
                     IF(KODEOT.EQ.' ') KODEOT = '12345'
                     IF(JCE.EQ.0) NLOC(1) = NODEOT
                     IF(JCE.EQ.1)  KAN(1) = KODEOT
                     LOCATS  = 1
                     ENDIF
C=======================================================================
C     Write the header information on the new interface file.
C=======================================================================
      IF(NEXT.GT.0) THEN
                    REWIND NEXT
                    WRITE(NEXT) LOCATS,NPOLL
                    IF(JCE.EQ.0) WRITE(NEXT) (NLOC(I),I=1,LOCATS)
                    IF(JCE.EQ.1) WRITE(NEXT)  (KAN(I),I=1,LOCATS)
                    SOURCE = 'COMBINE BLOCK'
                    WRITE(N6,9015)
                    CALL INFACE(2,NEXT)
                    CALL INFACE(1,NEXT)
                    ENDIF
C=======================================================================
C     DO-loop for reading all input data.
C=======================================================================
      XTIM1      = 0.0
      XTIM2      = 0.0
      DELT1      = 0.0
      DELT2      = 0.0
      OK1        = .FALSE.
      OK2        = .FALSE.
C##### WCH, 5/28/93.  CHANGE DABS TO ABS
      DIFF       = ABS(DIFF)
      ILAG       = 1
      J1LAG      = 0
      J2LAG      = 0
      IF(FIRST.EQ.1)  OK1 = .TRUE.
      IF(FIRST.EQ.2)  OK2 = .TRUE.
      IF(DIFF.EQ.0.0) OK2 = .TRUE.
C=======================================================================
C     Initialize arrays CPOLL and QQO for each time step.
C=======================================================================
      DO 999 KDT = 1,1000000
      DO 750   K = 1,LOCATS
      DO 740   L = 1,NPOLL
  740 CPOLL(L,K) = 0.0
  750 QQO(K)     = 0.0
C=======================================================================
C     Read the two input files.
C=======================================================================
      IF(FIRST.EQ.2.AND.XTIM2.GE.DIFF) OK1 = .TRUE.
      IF(FIRST.EQ.1.AND.XTIM1.GE.DIFF) OK2 = .TRUE.
C=======================================================================
      IF(JULDAY.NE.9999999.AND.OK1) THEN
      IF(ILAG.NE.2) THEN
                   IF(NQ1.EQ.0) READ(LAST,END=1000) JULDAY,TIMDAY,DELT1,
     +                                 (QO1(I),I=1,LOCAT1)
                   IF(NQ1.GT.0) READ(LAST,END=1000) JULDAY,TIMDAY,DELT1,
     +                         (QO1(I),(POLL1(J,I),J=1,NQ1),I=1,LOCAT1)
                   IF(JULDAY.EQ.99999) JULDAY = 9999999
                   JYEAR = JULDAY/1000
                   IF (JYEAR.LT.100) THEN
                   JULDAY = JULDAY - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JULDAY = JULDAY + JYEAR*1000
                   ENDIF
                   ELSE
                   DO 744 I   = 1,LOCAT1
                   QO1(I)     = QO3(I)
                   DO 743 J   = 1,NQ1
                   POLL1(J,I) = POLL3(J,I)
  743              CONTINUE
  744              CONTINUE
                   ENDIF
      DELT = DELT1
      ENDIF
C=======================================================================
      IF(JDAY2.NE.9999999.AND.OK2) THEN
      IF(ILAG.NE.3) THEN
                   IF(NQ2.EQ.0) READ(SECOND,END=1010) JDAY2,TMDAY2,
     +                                    DELT2,(QO2(I),I=1,LOCAT2)
                   IF(NQ2.GT.0) READ(SECOND,END=1010) JDAY2,TMDAY2,
     +               DELT2,(QO2(I),(POLL2(J,I),J=1,NQ2),I=1,LOCAT2)
                   IF(JDAY2.EQ.99999) JDAY2 = 9999999
                   JYEAR = JDAY2/1000
                   IF (JYEAR.LT.100) THEN
                   JDAY2 = JDAY2 - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JDAY2 = JDAY2 + JYEAR*1000
                   ENDIF
                   ELSE
                   DO 746 I   = 1,LOCAT2
                   QO2(I)     = QO3(I)
                   DO 745 J   = 1,NQ2
                   POLL2(J,I) = POLL3(J,I)
  745              CONTINUE
  746              CONTINUE
                   ENDIF
      DELT = DELT2
      ENDIF
C=======================================================================
C     The end of both input files has been found.  Branch to 1020.
C=======================================================================
      IF(JULDAY.EQ.9999999.AND.JDAY2.EQ.9999999) GO TO 1020
C=======================================================================
C     Calculate the time difference in seconds between two data sets.
C=======================================================================
      CALL NTIME(JDAY2,TMDAY2,DIFF)
C=======================================================================
C                      Calculate the variable ILAG.
C
C     ILAG = 1 ==> The difference in time between files 2 & 1 = 0.
C                  No changes from the original COMBINE code.
C     ILAG = 2 ==> The difference in time between files 2 & 1 is < 0.
C                  This means that file 2's time is before file 1's
C                  time.  File 1's values for flow and pollutants will
C                  be set to zero.  File 1's values will be temporarily
C                  stored in QO3 and POLL3.
C     ILAG = 3 ==> The difference in time between FILE 2 & 1 is > 0.
C                  This means that File 1's time is before file 2's
C                  time.  File 2's values for flow and pollutants will
C                  be set to zero.  File 2's values will be temporarily
C                  stored in QO3 and POLL3.
C
C=======================================================================
      IF(DIFF.EQ.0.0) THEN
                      ILAG        = 1
                      J1LAG       = 0
                      J2LAG       = 0
                      DO 7400 I   = 1,LOCAT1
                      QOLD1(I)    = QO1(I)
                      DO 7410 J   = 1,NQ1
7410                  POLD1(J,I)  = POLL1(J,I)
7400                  CONTINUE
                      DO 7420 I   = 1,LOCAT2
                      QOLD2(I)    = QO2(I)
                      DO 7430 J   = 1,NQ2
7430                  POLD2(J,I)  = POLL2(J,I)
7420                  CONTINUE
                      ENDIF
      IF(DIFF.LT.0.0) THEN
           ILAG       = 2
           J2LAG      = 0
           J1LAG      = J1LAG + 1
C##### WCH, 5/28/93.  CHANGE DABS TO ABS
           TDIFF      = ABS(DIFF)
           IF(J1LAG.EQ.1) TDIF1(I) = TDIFF + DELT2
           SLOPE      = (TDIF1(I)-TDIFF)/TDIF1(I)
           DO 748 I   = 1,LOCAT1
           QO3(I)     = QO1(I)
           QO1(I)     = (QO1(I)-QOLD1(I))*SLOPE + QOLD1(I)
           IF(QO1(I).LT.0.0) QO1(I) = 0.0
           DO 747 J   = 1,NQ1
           POLL3(J,I) = POLL1(J,I)
           POLL1(J,I) = (POLL1(J,I)-POLD1(J,I))*SLOPE + POLD1(J,I)
           IF(POLL1(J,I).LT.0.0) POLL1(J,I) = 0.0
  747      CONTINUE
  748      CONTINUE
           DELT1      = DELT1 - DELT2
           DELT       = DELT2
           ENDIF
      IF(DIFF.GT.0.0) THEN
           ILAG       = 3
           J1LAG      = 0
           J2LAG      = J2LAG + 1
           IF(J2LAG.EQ.1) TDIF2(I) = DIFF + DELT1
           SLOPE      = (TDIF2(I)-DIFF)/TDIF2(I)
           DO 751 I   = 1,LOCAT2
           QO3(I)     = QO2(I)
           QO2(I)     = (QO2(I)-QOLD2(I))*SLOPE + QOLD2(I)
           IF(QO2(I).LT.0.0) QO2(I) = 0.0
           DO 749 J   = 1,NQ2
           POLL3(J,I) = POLL2(J,I)
           POLL2(J,I) = (POLL2(J,I)-POLD2(J,I))*SLOPE + POLD2(J,I)
           IF(POLL2(J,I).LT.0.0) POLL2(J,I) = 0.0
  749      CONTINUE
  751      CONTINUE
           DELT2      = DELT2 - DELT1
           DELT       = DELT1
           ENDIF
C=======================================================================
      XTIM1 = XTIM1 + DELT1
      XTIM2 = XTIM2 + DELT2
C=======================================================================
C     Combine all the input locations into one output location.
C=======================================================================
      IF(ICOMB.EQ.1) THEN
                     DO 940 KK1  = 1,LOCAT1
                     QQO(KPOSIT) = QQO(KPOSIT) + QO1(KK1)
                     IF(NPOLL.GT.0) THEN
                              DO 945  MM       = 1,NPOLL
                              K1               = NPOS1( MM )
  945                         CPOLL(MM,KPOSIT) = CPOLL(MM,KPOSIT)
     +                                            + POLL1(K1,KK1)
                              ENDIF
  940                CONTINUE
                     DO 950 KK2  = 1,LOCAT2
                     QQO(KPOSIT) = QQO(KPOSIT) + QO2(KK2)
                     IF(NPOLL.GT.0) THEN
                              DO 955  MM       = 1,NPOLL
                              K2               = NPOS2( MM )
  955                         CPOLL(MM,KPOSIT) = CPOLL(MM,KPOSIT)
     +                                            + POLL2(K2,KK2)
                              ENDIF
  950                CONTINUE
                     ENDIF
C=======================================================================
C     Collate (interleave) all the input locations together.
C=======================================================================
      IF(ICOMB.EQ.0) THEN
                     DO 960 NPOSIT = 1,LOCATS
                     KK1         = INPOS1(NPOSIT)
                     KK2         = INPOS2(NPOSIT)
                     IF(JCOMB(NPOSIT).EQ.1) QQO(NPOSIT) =
     +                               QQO(NPOSIT) + QO1(KK1) + QO2(KK2)
                     IF(JCOMB(NPOSIT).EQ.2) QQO(NPOSIT) =
     +                                          QQO(NPOSIT) + QO1(KK1)
                     IF(JCOMB(NPOSIT).EQ.3) QQO(NPOSIT) =
     +                                          QQO(NPOSIT) + QO2(KK2)
                     IF(NPOLL.GT.0) THEN
                        DO 920 MM        = 1,NPOLL
                        K1               = NPOS1( MM )
                        K2               = NPOS2( MM )
                        IF(JCOMB(NPOSIT).EQ.1) CPOLL(MM,NPOSIT) =
     +                  CPOLL(MM,NPOSIT) + POLL1(K1,KK1) + POLL2(K2,KK2)
                        IF(JCOMB(NPOSIT).EQ.2) CPOLL(MM,NPOSIT) =
     +                                  CPOLL(MM,NPOSIT) + POLL1(K1,KK1)
                        IF(JCOMB(NPOSIT).EQ.3) CPOLL(MM,NPOSIT) =
     +                                  CPOLL(MM,NPOSIT) + POLL2(K2,KK2)
  920                   CONTINUE
                        ENDIF
  960                   CONTINUE
                     ENDIF
C=======================================================================
C     Write the actual interface data line.
C=======================================================================
  990 IF(NPOLL.EQ.0) WRITE(NEXT) JULDAY,TIMDAY,DELT,(QQO(K),K=1,LOCATS)
      IF(NPOLL.GT.0) THEN
      IF(ILAG.EQ.2)  THEN
                     WRITE(NEXT) JDAY2,TMDAY2,DELT2,(QQO(K),
     +                      (CPOLL(M,K),M=1,NPOLL),K=1,LOCATS)
                     ELSE
                     WRITE(NEXT) JULDAY,TIMDAY,DELT1,(QQO(K),
     +                      (CPOLL(M,K),M=1,NPOLL),K=1,LOCATS)
                     ENDIF
                     ENDIF
      GO TO 999
C=======================================================================
C     End of the first file is reached.
C=======================================================================
 1000 CONTINUE
      JULDAY    = 9999999
      TIMDAY    =   0.0
      DO 1050 I = 1,LOCAT1
      QO1(I)    = 0.0
      QOLD1(I)  = 0.0
      IF(NQ1.GT.0) THEN
                   DO 1060 J  = 1,NQ1
                   POLL1(J,I) = 0.0
 1060              POLD1(J,I) = 0.0
                   ENDIF
 1050 CONTINUE
      GO TO 999
C=======================================================================
C     End of the second file is reached.
C=======================================================================
 1010 CONTINUE
      JDAY2     = 9999999
      TMDAY2    =   0.0
      DO 1100 I = 1,LOCAT2
      QO2(I)    = 0.0
      QOLD2(I)  = 0.0
      IF(NQ2.GT.0) THEN
                   DO 1110 J  = 1,NQ2
                   POLL2(J,I) = 0.0
 1110              POLD2(J,I) = 0.0
                   ENDIF
 1100 CONTINUE
  999 CONTINUE
 1020 CONTINUE
      WRITE(N6,9000)
      WRITE(*,9000)
      RETURN
C#### WCH, 11/22/93
 2200 WRITE(N6,2210) KPOLL
      STOP
C#### WCH, 8/2/93.  ADD ADDITIONAL ERROR MESSAGE.
  887 WRITE (*,9500) CC
      WRITE(N6,9500) CC
  888 CALL IERROR
C=======================================================================
  10  FORMAT(/,
     +' ################################################',/,
     +' # Entry made to Combine Block. Last updated at #',/,
     +' # OSU, July 1996.                              #',/,
     +' ################################################',/)
C#### WCH, 7/25/96.  CHANGE FORMAT 71 SLIGHTLY
  71  FORMAT(/,' ICOMB...................................',I5,/,
     +         ' = 0 ==> Collate option',/,
     +         '   1 ==> Combine option',/,
     +         '   2 ==> Extract option',/,
     +         '   3 ==> File reader option',/,
     +         '   4 ==> ASCII file creation',/,
     +         '   5 ==> Interface file summation option',/,
     +         '   6 ==> Rain Block interface file summation option',/,
     +         '   7 ==> Rain Block interface file summation and ASCII f
     +ile creation on JOUT',/)
  410 FORMAT(/,' Output node number is............',I10,//,
     1         ' Output data-set unit number is...',I10,//,
     2         ' Number of quality constituents...',I10,/)
  411 FORMAT(/,' Output node name is..............',A10,//,
     1         ' Output data-set unit number is...',I10,//,
     2         ' Number of quality constituents...',I10,/)
C#### WCH, 11/23/93
  415 FORMAT(' Output ASCII file will give loads (flow x concentration) 
     1for pollutant units.')
  416 FORMAT(' Output ASCII file will give concentrations for pollutant 
     1units (NPOLL < 0).')
  420 FORMAT(/,' Water quality constituent........',I3,/,
     +         ' Position on input file one.......',I3,/,
     +         ' Position on input file two.......',I3)
  430 FORMAT(/,' Locations to be extracted (NUMX).',I3,//,
     +         ' Locations to be renumbered (NUMR)',I3)
  440 FORMAT(/,
     +' #########################################',/,
     +' # The following nodes will be extracted #',/,
     +' #     from the input file(s).           #',/,
     +' #########################################',/)
  445 FORMAT(/,
     +' ##################################################',/,
     +' # The following numbers are the new node numbers #',/,
     +' # assigned to the extracted/renumbered nodes.    #',/,
     +' ##################################################',/)
  450 FORMAT(1X,10I10)
  451 FORMAT(1X,10A10)
  455 FORMAT(///,' Ending time in seconds for file 1 --->',F15.2,
     +        /, ' Ending time in seconds for file 2 --->',F15.2)
  660 FORMAT(1H1,4X,A80,/,5X,A80)
C#### WCH, 11/22/93
 2210 FORMAT(//,' Error! NPOS1 and NPOS2 (data group B3) are both zero f
     +or pollutant number',I2,/,' At least one must be non-zero.  Run st
     +opped from the Combine Block.')
 9000 FORMAT(/,' ===> Combine Block ended normally.')
 9005 FORMAT(/,
     +' **************************************************',/,
     +' *  Reading information from the NSCRAT(1) file.  *',/,
     +' **************************************************')
 9010 FORMAT(/,
     +' ********************************************',/,
     +' *  Reading information from the JIN file.  *',/,
     +' ********************************************')
 9015 FORMAT(/,
     +' *******************************************',/,
     +' *  Writing information on the JOUT file.  *',/,
     +' *******************************************')
C#### WCH, 8/2/93.
 9500 FORMAT (/' $$$ YOU ARE ABOUT TO RECEIVE AN ERROR MESSAGE WHILE TRY
     1ING TO READ LINE ',A2,/,'    IT IS POSSIBLE THIS IS CAUSED BY NOT 
     2USING ALPHANUMERIC VALUES (ENCLOSED',/,'     IN QUOTES) WHEN'
     3 ,' NODE NUMBERS ARE REQUESTED AS INPUT.  FOR EXAMPLE,'/
     4'     A ZERO INPUT FOR ALPHANUMERIC IS JUST TWO QUOTES',4H ''.)
      END
