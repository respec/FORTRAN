      SUBROUTINE RHYDRO2
C=======================================================================
C     SECOND GENERAL INPUT SUBROUTINE OF THE RUNOFF BLOCK
C     RHYDRO2 LAST UPDATED MAY, 1989
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'NEW89.INC'
C=======================================================================
C>>>>>>>> READ DATA GROUP M1-M2 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NPRNT,INTERV
      IF(NPRNT.GT.0) THEN
               READ(N5,*,ERR=888) CC,NDET,(JSTART(I),JSTOP(I),I=1,NDET)
               WRITE(N6,1955) NPRNT,INTERV,(JSTART(I),JSTOP(I),I=1,NDET)
               ENDIF
      IF(NPRNT.LT.1) GO TO 2020
      DO 3000 L = 1,NDET
      IF(JSTART(L).EQ.0) GO TO 3000
      M1        = JSTART(L)/10000
      M3        = JSTART(L) - M1*10000
      M2        = M3/100
      M3        = M3 - M2*100
      JSTART(L) = 1000*M1 + JDATE(M3,M2,M1)
      M1        = JSTOP(L)/10000
      M3        = JSTOP(L) - M1*10000
      M2        = M3/100
      M3        = M3 - M2*100
      JSTOP(L)  = 1000*M1 + JDATE(M3,M2,M1)
 3000 CONTINUE
C=======================================================================
C>>>>>>>> READ DATA GROUP M3 <<<<<<<<
C=======================================================================
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,(IPRNT(K),K=1,NPRNT)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,(KPRNT(K),K=1,NPRNT)
C=======================================================================
C     Write data group M3.
C=======================================================================
      IF(JCE.EQ.0) WRITE(N6,1965) (IPRNT(K),K=1,NPRNT)
      IF(JCE.EQ.1) WRITE(N6,1966) (KPRNT(K),K=1,NPRNT)
C=======================================================================
C>>>>>>>> READ DATA GROUP M4 - IF PRESENT <<<<<<<<
C=======================================================================
       READ(N5,*,ERR=888) CC
       IF(CC.EQ.'M4') THEN
                      BACKSPACE N5
                      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,MDEEP,
     +                                     (IDEEP(N),N=1,MDEEP)
                      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,MDEEP,
     +                                     (KDEEP(N),N=1,MDEEP)
                      IF(JCE.EQ.0) WRITE(N6,1975) (IDEEP(K),K=1,MDEEP)
                      IF(JCE.EQ.1) WRITE(N6,1976) (KDEEP(K),K=1,MDEEP)
                      IF(NSCRAT(7).EQ.0) CALL ERROR(112)
                      ELSE
                      MDEEP    = 0
                      BACKSPACE N5
                      ENDIF
C=======================================================================
C     Convert IPRNT to internal numbers.
C=======================================================================
 1969 NN        = NOG + NSAVE
      DO 2000 N = 1,NPRNT
      DO 1970 J = 1,NN
C=======================================================================
C     POSITIVE NUMBER MEANS PRINT INFLOW TO CHANNEL OR INLET.
C     Negative number means print outflow from channel.
C=======================================================================
      IF(JCE.EQ.0.AND.IABS(IPRNT(N)).EQ.NAMEG(J)) GO TO 1990
      IF(JCE.EQ.1.AND.KPRNT(N).EQ.KAMEG(J))       GO TO 1990
 1970 CONTINUE
      IF(JCE.EQ.0) WRITE(N6,1980) IPRNT(N),N
      IF(JCE.EQ.1) WRITE(N6,1981) KPRNT(N),N
      GO TO 2000
 1990 IF(JCE.EQ.0) THEN
             IF(J.GT.NOG) IPRNT(N) = J
             IF(J.LE.NOG) IPRNT(N) = IPRNT(N)/IABS(IPRNT(N))*J
             ELSE
             IPRNT(N) = J
             ENDIF
 2000 CONTINUE
      WRITE(N6,2010)
 2020 CONTINUE
C=======================================================================
C     Convert IDEEP to internal numbers.
C=======================================================================
      IF(MDEEP.GT.0) THEN
               DO 2500 N = 1,MDEEP
               DO 2510 J = 1,NN
               IF(JCE.EQ.0.AND.IDEEP(N).EQ.NAMEG(J)) GO TO 2600
               IF(JCE.EQ.1.AND.KDEEP(N).EQ.KAMEG(J)) GO TO 2600
 2510          CONTINUE
               IF(JCE.EQ.0) WRITE(N6,2610) IDEEP(N),N
               IF(JCE.EQ.1) WRITE(N6,2611) KDEEP(N),N
               GO TO 2500
 2600          IDEEP(N) = J
 2500          CONTINUE
               ENDIF
C=======================================================================
C     Setup output files.
C=======================================================================
      NOUT      = JOUT(IOUTCT)
      QCONV     = CMET(8,METRIC)
      NQUAL     = NQS
      LOCATS    = NSAVE
      IF(NOUT.GE.1) THEN
                    REWIND NOUT
                    WRITE(NOUT) NSAVE,NQS
                    IF(JCE.EQ.0) WRITE(NOUT) (ISAVE(I),I=1,NSAVE)
                    IF(JCE.EQ.1) WRITE(NOUT) (KSAVE(I),I=1,NSAVE)
                    CALL INFACE(2,NOUT)
                    ENDIF
      IF(NOGG.EQ.0) NOG = 999
      DO 2040 J = 1,NG
 2040 NGUT(J)   = 0
      RETURN
  888 CALL IERROR
C=======================================================================
 1955 FORMAT(/,
     +'     ***********************',/,
     +'     *    DATA GROUP M1    *',/,
     +'     ***********************',//,
     .         /,' TOTAL NUMBER OF PRINTED GUTTERS/INLETS...NPRNT..',
     1      I9,/,' NUMBER OF TIME STEPS BETWEEN PRINTINGS..INTERV..',
     2      I9,/,' STARTING AND STOPPING PRINTOUT DATES............',
     3             2I9,/,4(49X,2I9,/))
 1965 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP M3    *',/,
     +         '     ***********************',//,
     1          ' CHANNEL/INLET PRINT DATA GROUPS......',10I7,
     2          19(/,31X,10I7))
 1966 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP M3    *',/,
     +         '     ***********************',//,
     1         ' CHANNEL/INLET PRINT DATA GROUPS......',10(A6,1X),
     2          19(/,31X,10(A6,1X)))
 1975 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP M4    *',/,
     +         '     ***********************',//,
     1          ' CHANNEL/DEPTH PRINT DATA GROUPS......',10I7,
     2          19(/,31X,10I7))
 1976 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP M4    *',/,
     +         '     ***********************',//,
     1         ' CHANNEL/DEPTH PRINT DATA GROUPS......',10(A6,1X),
     2          19(/,31X,10(A6,1X)))
 1980 FORMAT(/,' ===>  ERROR !! CANNOT MATCH IPRNT(N),',I5,' FOR N = ',
     1       I5,' FOR ANY J FROM 1 TO NO. OF INLETS + CHANNELS.')
 1981 FORMAT(/,' ===>  ERROR !! CANNOT MATCH IPRNT(N), ',A10,
     1' FOR N = ',I5,' FOR ANY J FROM 1 TO NO. OF INLETS + CHANNELS.')
 2010 FORMAT(1H )
 2610 FORMAT(/,' ===>  ERROR !! CANNOT MATCH IDEEP(N),',I5,' FOR N = ',
     1       I5,' FOR ANY J FROM 1 TO NO. OF INLETS + CHANNELS.')
 2611 FORMAT(/,' ===>  ERROR !! CANNOT MATCH IPRNT(N), ',A10,
     1' FOR N = ',I5,' FOR ANY J FROM 1 TO NO. OF INLETS + CHANNELS.')
C=======================================================================
      END
