      SUBROUTINE WSTAT
C=======================================================================
C     SUBROUTINE TO PRINT SUMMARY STATS FOR WATERSHEDS
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93.
C     METRIC CORRECTION FOR RAINFALL PRINTOUT, WCH, 11/15/93.
Cred  Change NGTO(I) to NAMEG(J) in write statements - 1/25/94
Cred  Change KGTO(I) to KAMEG(J) in write statements - 1/25/94
Cred  Add loop 9000 to find channel number - 1/25/94
C=======================================================================
C    note : programmed only for ft/pound/sec system 
C           some work on convertion factors required 
C           if metrics used   
C=======================================================================
C     SPECIFICATION STATEMENTS
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'RUNSTAT.INC'
C=======================================================================
      REAL LOSPER,PEAKPER,PEAKIMP,PEAKTOT
C=======================================================================
C     TOTAL RAINFALL IN INCHES
C=======================================================================
      DO 10 J=1,10
  10  RTOT(J)=RTOT(J)/3600.
C=======================================================================
CJES SUBCATCHMENT SUMMARY TABLE
C=======================================================================
      ICOUNT   = 0 
      DO 500 I = 1, NOW
      IF (WAREA(I).NE.0.0) THEN 
c.....
Cred  Change NGTO(I) to NAMEG(J) in write statements - 1/25/94
Cred  Add loop 9000 to find channel number - 1/25/94
c.....
      nogg                = nog + nsave
      if(nog.eq.999) nogg = nsave
      DO 9000 JJ = 1,nogg
      DO 9000 KK = 1,NCP
      IF(NWTOG(KK,JJ).EQ.I) J = JJ
 9000 CONTINUE
C=======================================================================
C     RUNOFF IN INCHES
C=======================================================================
C     WRITE(N6,*) I,WAREA(I)
C     WRITE(N6,*) (WAR(J,I),J=1,4)
C     WRITE(N6,*) (SUBQPEAK(J,I),J=1,6)
C     WRITE(N6,*) (SUBDEP(J,I),J=1,4)
C     WRITE(N6,*) (RTOT(J),J=1,10)
C
           AREATOT = 0.
           AREAIMP = 0.
           AREAPER = 0.
           FRACIMP = 0.
           RUNPER  = 0.
           LOSPER  = 0.
           PEAKPER = 0.
           RUNIMP  = 0.
           PEAKIMP = 0.
           RUNTOT  = 0.
           PEAKTOT = 0.
           UNITRF  = 0.
           ICOUNT  = ICOUNT + 1 
           IF(ICOUNT.GE.2)  GO TO 460
           IF(ICOUNT.EQ.42) ICOUNT = 0 
C=======================================================================
CJES   SUBCATCHMENT SUMMARY TABLE HEADER 
C=======================================================================
           WRITE(N6,1000)
           IF(METRIC.EQ.1) WRITE(N6,1001)
           IF(METRIC.EQ.2) WRITE(N6,1002)
 460       CONTINUE
C=======================================================================
CJES    FINDING STATS ON PERVIOUS FRACTION 
C=======================================================================
           IF(WAR(2,I).LE.0.0) GO TO 465
           RUNPER  =  SUBDEP(2,I)/WAR(2,I)*12.0
C#### WCH, 11/15/93
           LOSPER  =  RTOT(NHYET(I))/CMET(5,METRIC) - RUNPER
           PEAKPER =  SUBQPEAK(2,I)
C=======================================================================
CJES    FINDING STATS ON IMPERVIOUS FRACTION 
CJES    COMBINING TWO IMPERVIOUS SHEET PLANES (K=1 AND 3)
C=======================================================================
  465      AREAIMP = WAR(1,I) + WAR(3,I) + WAR(4,I)
           IF(AREAIMP.LE.0.0) GO TO 470
           RUNIMP = (SUBDEP(1,I) + SUBDEP(3,I) +  SUBDEP(4,I))
     1        /AREAIMP*12.0
           AREAIMP = AREAIMP / 43560.
           PEAKIMP = SUBQPEAK(5,I)
C=======================================================================
CJES  FINDING STATS ON SUBCATCHMENT 
C=======================================================================
  470      AREATOT = WAR(1,I)+WAR(2,I)+WAR(3,I)+WAR(4,I)
           RUNTOT  = (SUBDEP(1,I)+SUBDEP(2,I)+SUBDEP(3,I)+SUBDEP(4,I))
     1        /AREATOT*12.0
           AREATOT = AREATOT/43560.0
           FRACIMP = AREAIMP / AREATOT * 100.0
           UNITRF  = SUBQPEAK(6,I) /  AREATOT
           PEAKTOT = SUBQPEAK(6,I)
C
           NGAG = NHYET(I)
C#### WCH, 11/15/93.  NOTE, RAIN NEVER CONVERTED FROM METRIC.
           RTOTNG = RTOT(NGAG)
           IF(METRIC.EQ.2) THEN
                AREATOT = AREATOT/2.471
C####                RTOTNG  = RTOTNG*25.4
                RUNPER  = RUNPER*25.4
                LOSPER  = LOSPER*25.4
                PEAKPER = PEAKPER/35.315
                RUNIMP  = RUNIMP*25.4
                PEAKIMP = PEAKIMP/35.315
                RUNTOT  = RUNTOT*25.4
                PEAKTOT = PEAKTOT/35.315
                UNITRF  = UNITRF*25.4
                ENDIF
           IF(JCE.EQ.0) WRITE(N6,1100) NAMEW(I),NAMEG(J),AREATOT,
     1                  FRACIMP,RTOTNG,RUNPER,LOSPER,PEAKPER,
     2                  RUNIMP,PEAKIMP,RUNTOT,PEAKTOT,UNITRF
           IF(JCE.EQ.1) WRITE(N6,1101) KAMEW(I),KAMEG(J),AREATOT,
     1                  FRACIMP,RTOTNG,RUNPER,LOSPER,PEAKPER,
     2                  RUNIMP,PEAKIMP,RUNTOT,PEAKTOT,UNITRF
           ELSE
           IF(JCE.EQ.0) WRITE(N6,1100) NAMEW(I),NAMEG(J),WAREA(I)
           IF(JCE.EQ.1) WRITE(N6,1101) KAMEW(I),KAMEG(J),WAREA(I)
           ENDIF
 500  CONTINUE 
      WRITE(N6,1200)
      RETURN
 1000 FORMAT('1'//35X,'SUMMARY STATISTICS FOR SUBCATCHMENTS',/35X,
     !36('=')//51X,'PERVIOUS AREA     IMPERVIOUS AREA    ',
     !' TOTAL SUBCATCHMENT AREA ',/
     !48X,'-------------------  -----------------  ',
     !'-------------------------',/
     !30X,'        TOTAL     TOTAL',
     !9X,'PEAK              PEAK               PEAK     PEAK',
     !/'             GUTTER                 SIMULATED   RUNOFF TOTAL ',
     !'RUNOFF   RUNOFF   RUNOFF      RUNOFF  RUNOFF   UNIT ',/
     !' SUBCATCH-  OR INLET  AREA  PERCENT  RAINFALL   DEPTH LOSSES ',
     !' RATE     DEPTH    RATE        DEPTH   RATE    RUNOFF')
 1001 FORMAT (
     !' MENT NO.     NO.     (AC)   IMPER.   (IN)      (IN)   ',      
     !'(IN)   (CFS)     (IN)   (CFS)        (IN)   (CFS)   (IN/HR)',/
     ! 116('-'))
 1002 FORMAT (
     !' MENT NO.     NO.     (HA)   IMPER.   (MM)      (MM)   ',      
     !'(MM)   (CMS)     (MM)   (CMS)        (MM)   (CMS)   (MM/HR)',/
     ! 116('-'))
 1100  FORMAT(I9,I9,F8.2,F8.1,F8.2,F10.3,F8.3,F8.2,F9.3,F9.2,
     !  F10.3,F9.2,F9.3)
 1101  FORMAT(1X,A8,1X,A8,F8.2,F8.1,F8.2,F10.3,F8.3,F8.2,F9.3,F9.2,
     !  F10.3,F9.2,F9.3)
 1200 FORMAT(/10X,' *** NOTE *** IMPERVIOUS AREA STATISTICS AGGREGATE',
     ! ' IMPERVIOUS AREAS WITH AND WITHOUT DEPRESSION STORAGE',/)
      END
