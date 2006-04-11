      SUBROUTINE INTIDEF
C	EXTRAN BLOCK
C	CREATE BY C.MOORE OF CDM  1998
C	READ TIDAL HISTORY INFORMATION FROM EXTERNAL FILE
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIDEFILE.INC'
      INCLUDE 'CONTR.INC'
      include 'tide.inc'
      include 'bnd.inc'
      include 'junc.inc'
      include 'stimer.inc'
C  FIRST CHECK THAT NSCRAT(5) WAS INITIALIZED CORRECTLY
      IF (NSCRAT(5).EQ.0) THEN
      WRITE(N6,*) 'ERROR - NSCRAT(5) MUST BE NON-ZERO TO USE OPTION',
     A' TO READ TIDES FROM ASCII FILE'
      STOP 'NSCRAT(5) IS ZERO'
      ELSE
      NTDN = NSCRAT(5)
      ENDIF
      IF (FFNAME(55).EQ.'SCRT5.UF') THEN
      WRITE(N6,*) 'ERROR - YOU MUST SPECIFY THE NAME FOR NSCRAT(5)',
     A' TO READ TIDES FROM ASCII FILE'
      STOP 'NO NAME GIVEN FOR NSCRAT(5)'
      ENDIF
C

C     NOW READ FIRST 3 LINES OF INPUT FILE
C
      READ(NTDN,*) NTIDS, NDUP
      IF (NTIDS.GT.MAXTIDES) THEN
      WRITE(N6,*) 'ERROR - NUMBER OF TIDES IN FILE (',NTIDS,
     1') EXCEEDS MAXIMUM NUMBER ALLOWED (',MAXTIDES,')'
      STOP 'ERROR - NUMBER OF TIDES IN FILE EXCEEDS MAXIMUM ALLOWED'
      END IF
      IF (NDUP.GT.MAXDUP) THEN
      WRITE(N6,*) 'ERROR - NUMBER OF OUTFALLS THAT USE DUPLICATE ',
     1'TIDAL HISTORY (',NDUP,') EXCEEDS MAXIMUM ALLOWED (',MAXDUP,')'
      STOP 'ERROR - NUMBER OF DUPLICATE TIDES EXCEEDS MAXIMUM ALLOWED'
      END IF
C  PROCESSING IF NOT ALPHANUMERIC
      IF (JCE.EQ.0) THEN
      READ(NTDN,*) (JTIDS(J),J=1,NTIDS)
      WRITE(N6,*) 'TIDAL TIME HISTORIES ARE READ FROM ASCII FILE',
     A' (NSCRAT(5)) FOR THE FOLLOWING ',NTIDS,' BOUNDARY JUNCTIONS'
      WRITE(N6,7000) (JTIDS(J),J=1,NTIDS)
      IF (NDUP.GT.0) THEN
      DO J=1,NDUP
      READ(NTDN,*) (JDUP(J,K),K=1,2)
      ENDDO
      WRITE(N6,*) 'THE FOLLOWING ',NDUP,' JUNCTIONS USE DUPLICATE ',
     1'TIDAL HISTORIES'
      WRITE(N6,*) ' BOUNDARY  TIDE'
      WRITE(N6,*) '   NODE   HISTORY'
      DO J=1,NDUP
      WRITE(N6,7000) (JDUP(J,K),K=1,2)
      DO K=1,NTIDS
      IF (JTIDS(K).EQ.JDUP(J,2)) THEN
      JDUP(J,2) = -K
      GO TO 20
      ENDIF
      ENDDO
      WRITE(N6,*) 'ERROR - TIME HISTORY ASSOCIATED WITH OUTFALL ',
     1JDUP(J,1),' WAS NOT FOUND (',JDUP(J,2),')'
      STOP 'ERROR - DUPLICATE TIME HISTORY NOT FOUND'
  20  CONTINUE
      ENDDO
      ENDIF
C  CORRELATE JTIDS WITH BOUNDARY ELEMENTS AND CHECK THAT ALL ARE INCLUDED
C   LOOP THROUGH ALL JUNCTIONS, IF MATCH SET JTIDE = 10000 + NUMBER OF TIDE
C   HISTORY
      DO K = 1,NTIDS
      DO J = 1,NJ
      IF (JTIDS(K).EQ.JUN(J)) THEN
      JTIDES(J) = 10000 + K
      GO TO 40
      ENDIF
      enddo
      WRITE(N6,*) 'WARNING - JUNCTION ASSOCIATED WITH TIDE HISTORY ',
     1JTIDS(K),' WAS NOT FOUND'
   40 CONTINUE
      ENDDO
      DO K = 1,NDUP
      DO J = 1,NJ
      IF (JDUP(K,1).EQ.JUN(J)) THEN
      JTIDES(J) = 10000-JDUP(K,2)
      GO TO 50
      ENDIF
      ENDDO
      WRITE(N6,*) 'WARNING - JUNCTION ASSOCIATED WITH DUPLICATE',
     1' TIDE HISTORY ',JDUP(K,1),' WAS NOT FOUND'
   50 CONTINUE
      ENDDO
      ELSE
C  PROCESSING IF ALPHANUMERIC  just a copy of above code but using
c  alphanumeric variables
      READ(NTDN,*) (ATIDS(J),J=1,NTIDS)
      WRITE(N6,*) 'TIDAL TIME HISTORIES ARE READ FROM ASCII FILE',
     A' (NSCRAT(5)) FOR THE FOLLOWING ',NTIDS,' BOUNDARY JUNCTIONS'
      WRITE(N6,7010) (ATIDS(J),J=1,NTIDS)
      IF (NDUP.GT.0) THEN
      DO J=1,NDUP
      READ(NTDN,*) (ADUP(J,K),K=1,2)
      ENDDO
      WRITE(N6,*) 'THE FOLLOWING ',NDUP,' JUNCTIONS USE DUPLICATE ',
     1'TIDAL HISTORIES'
      WRITE(N6,*) ' BOUNDARY  TIDE'
      WRITE(N6,*) '   NODE   HISTORY'
      DO J=1,NDUP
      WRITE(N6,7010) (ADUP(J,K),K=1,2)
      DO K=1,NTIDS
      IF (ATIDS(K).EQ.ADUP(J,2)) THEN
      JDUP(J,2) = -K
      GO TO 120
      ENDIF
      ENDDO
      WRITE(N6,*) 'ERROR - TIME HISTORY ASSOCIATED WITH OUTFALL ',
     1ADUP(J,1),' WAS NOT FOUND (',ADUP(J,2),')'
      STOP 'ERROR - DUPLICATE TIME HISTORY NOT FOUND'
 120  CONTINUE
      ENDDO
      ENDIF
C  CORRELATE ATIDS WITH BOUNDARY ELEMENTS AND CHECK THAT ALL ARE INCLUDED
C   LOOP THROUGH ALL JUNCTIONS, IF MATCH SET JTIDE = 10000 + NUMBER OF TIDE
C   HISTORY
      DO K = 1,NTIDS
      DO J = 1,NJ
      IF (ATIDS(K).EQ.AJUN(J)) THEN
      JTIDES(J) = 10000 + K
      GO TO 140
      ENDIF
      enddo
      WRITE(N6,*) 'WARNING - JUNCTION ASSOCIATED WITH TIDE HISTORY ',
     1ATIDS(K),' WAS NOT FOUND'
  140 CONTINUE
      ENDDO
      DO K = 1,NDUP
      DO J = 1,NJ
      IF (ADUP(K,1).EQ.AJUN(J)) THEN
      JTIDES(J) = 10000-JDUP(K,2)
      GO TO 150
      ENDIF
      ENDDO
      WRITE(N6,*) 'WARNING - JUNCTION ASSOCIATED WITH DUPLICATE',
     1' TIDE HISTORY ',ADUP(K,1),' WAS NOT FOUND'
  150 CONTINUE
      ENDDO
      ENDIF
C  somehow check that all boundary assigned to type 6 have a JTIDE greater
C  than 10000
c  do free first
      do i=1,nfree
      ijunc = jfree(i)
cim      write(n6,*) m2s2,i,ijunc,ajun(ijunc),jtides(ijunc)
      if (jtides(ijunc).eq.m2s2) then
      write(n6,*) 'Error - Free outfall junction assigned to type 6',
     a' was not found in file'
      if (jce.eq.0) then
      write(n6,*) '        for outfall junction ',jun(ijunc)
      else
      write(n6,*) '        for outfall junction ',ajun(ijunc)
      endif
      stop 'Free outfall not found in ASCII file'
      endif
      enddo
c  do gated now
      do i=1,ngate
      ijunc = jgate(i)
cim      write(n6,*) m2s2,i,ijunc,ajun(ijunc),jtides(ijunc)
      if (jtides(ijunc).eq.m2s2) then
      write(n6,*) 'Error - Gates outfall junction assigned to type 6',
     a' was not found in file'
      if (jce.eq.0) then
      write(n6,*) '        for outfall junction ',jun(ijunc)
      else
      write(n6,*) '        for outfall junction ',ajun(ijunc)
      endif
      stop 'Gated outfall type 6 not found in ASCII file'
      endif
      enddo
c
c now read first line in input file
c
      read(NTDN,*,end=9000) ityear,itmonth,itday,thour,
     1(tideel(j,2),j=1,ntids)
c      if (ityear.gt.99) ityear=ityear-1900
      if (ityear.lt.100) ityear=ityear+1900
      tjulday = 1000*ITYEAR + JDATE(ITDAY,ITMONTH,ITYEAR)
      tiddate(2) = tjulday + thour/24.0
      write(n6,8000) tiddate(2),(tideel(j,2),j=1,ntids)
 8000 format(/,'New tide data - ',500f12.3)
c read next line in input file
      call newtide(dble(0.0))
      RETURN
 9000 write(n6,*) 'ERROR - No tide date in input file'
      stop 'No tide date in input file'
 7000 FORMAT(10I10)
 7010 format(10a10)
      END

      SUBROUTINE newtide(sdate)
c read new tide data
c repeats until #1 and #2 straddle sdate (simulation date)
      include 'tidefile.inc'
      include 'tapes.inc'
      DOUBLE PRECISION SDATE
  10  continue
c first shift 1 to 2
      tiddate(1) = tiddate(2)
      do j=1,ntids
      tideel(j,1) = tideel(j,2)
      enddo
c read next data
      read(NTDN,*,end=9000) ityear,itmonth,itday,thour,
     1(tideel(j,2),j=1,ntids)
cim      write(*,*) ityear,itmonth,itday,thour,
cim     1(tideel(j,2),j=1,ntids)
CIM      write(n6,*) ityear,itmonth,itday,thour,
CIM     1(tideel(j,2),j=1,ntids)
c      if (ityear.gt.99) ityear=ityear-1900
       if (ityear.lt.100) ityear=ityear+1900
      tjulday = 1000.0*ITYEAR + JDATE(ITDAY,ITMONTH,ITYEAR)
CIM      WRITE(N6,"(2F20.10)") THOUR,TJULDAY
      THOUR = THOUR/24.0
CIM      WRITE(N6,"(2F20.10)") THOUR,TJULDAY
      tiddate(2) = tjulday + thour
CIM      WRITE(N6,"(F20.10)") TIDDATE(2)
      if (tiddate(2).le.tiddate(1)) then
      WRITE(N6,*)
      write(n6,*) 'Tide dates in ASCII file do not increase ',
     a'at following date and time : ', ityear,itmonth,itday,thour
      stop 'tide dates in ascii file do not increase'
      endif
      if (tiddate(2).le.sdate) go to 10
      write(n6,7000) tiddate(2),(tideel(j,2),j=1,ntids)
 7000 format(/,'New tide data - ',500f12.3)
      return
c end if input file, tide remains constant
c  set date #2 to a large number
 9000 tiddate(2) = 99365.0
      WRITE(N6,*)
      write(n6,*) 'WARNING - End of file reached in tide file.',
     1'Tide will remain constant for duration of simulation'
      return
      end

      FUNCTION TIDEFILE(ICASE)
CIM  CALL TO INTERPOLATE TIDE DATA
CIM  ICASE SHOULD BE THE POSITION IN THE ASCII FILE + 10000
CIM
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIDEFILE.INC'
      DOUBLE PRECISION TIMENOW
      TIMENOW = JULDAY+TIMDAY/24.0/60.0/60.0
      ICSE = ICASE-10000
C READ NEW TIDE LINE IF NECESSARY
      DO WHILE (TIMENOW.GE.TIDDATE(2))
      CALL NEWTIDE(TIMENOW)
      ENDDO
C
C NOW INTERPOLATE
      Temp = HLINTP(TIDEEL(ICSE,1),TIDEEL(ICSE,2),
     1                 TIDDATE(1),TIDDATE(2),TIMENOW)
c     write(n6,*) icse,TIDEEL(ICSE,1),TIDEEL(ICSE,2),
c    1                 TIDDATE(1),TIDDATE(2),TIMENOW,temp
      TIDEFILE = temp
      RETURN
      END

      FUNCTION HLINTP(H1,H2,T1,T2,T)
      DOUBLE PRECISION T1,T2,T,RATIO,maxratio,minratio
      minratio = 0.0
      maxratio = 1.0
      RATIO = (T-T1)/(T2-T1)
      RATIO = DMAX1(RATIO,minratio)
      RATIO = DMIN1(RATIO,maxratio)
      HLINTP = H1 + (H2-H1)*RATIO
      RETURN
      END
