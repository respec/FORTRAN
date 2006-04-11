      SUBROUTINE RDIISHED
C     RUNOFF BLOCK
C     CALLED BY HYDRO NEAR LINE 353
C=======================================================================
C    RDIISHED CREATED AUGUST 1993 BY C. MOORE, CDM
C    PERFORMS RDII CALCULATIONS FOR BASINS WITH E5 CARDS
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'RDII.INC'
C=======================================================================
   10 CALL NTIME(JRDDAY(2),RDTIM(2),DEL2)
CCC      write(6,*) 1,JRDDAY(2),RDTIM(2),DEL2,JULDAY,TIMDAY
      IF(DEL2.LE.0.0) THEN
           CALL READNEXT
           GO TO 10
           ENDIF
      CALL NTIME(JRDDAY(1),RDTIM(1),DEL1)
CCC      write(6,*) 2,JRDDAY(1),RDTIM(1),DEL1,JULDAY,TIMDAY
      RRATIO = AMAX1(-DEL1/(DEL2-DEL1),0.0)
      DO 100 J = 1,NOW
      IF(ICURVE(J).EQ.0.OR.SEWAREA(J).EQ.0.0) GO TO 100
      JJ    = ICURVE(J)
      FFLOW = 0.0
      DO 50 I = 1,3
      FFFLOW = RDFLOW(JJ,I,1)+RRATIO*(RDFLOW(JJ,I,2)-RDFLOW(JJ,I,1))
C=======================================================================
C     RDFLOW = IN/HR
C     RDFLOW*SEWAREA = AC-IN/HR = CFS
C=======================================================================
      FFFLOW = FFFLOW*SEWAREA(J)*RDIIR(J,I,MONTH)
      FFLOW  = FFLOW+FFFLOW
      CNTRDII(I) = CNTRDII(I) + FFFLOW*DMEAN
   50 CONTINUE
C=======================================================================
C     MUST SAVE I/I FLOW SEPARATELY IF WANT TO ISOLATE QUALITY
C       CONTRIBUTIONS.
C=======================================================================
      FLOWII(J) = FFLOW
  100 CONTINUE
      END
