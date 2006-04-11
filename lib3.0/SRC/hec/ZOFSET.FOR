      SUBROUTINE ZOFSET (JUL, ITIME, INTL, IFLAG, IOFS)
C
C     Determine the time offset for regular interval
C     time-series data.
C
C     Written by Bill Charley, HEC.
C
C     INTEGER*6 NTIME                                                   H
      INTEGER*4 JUL, INTL, IOFS, JOFS, I, J, NTIME, JL, IYMDJL          ML
C
C
      IF (INTL.GT.1440) GO TO 20
C
C     Compute time offset (as JOFS)
      I = INTL + ITIME
      J = I / INTL
      JOFS = I - (J*INTL)
      IF (IFLAG.LE.1) IOFS = JOFS
C     Compute only?
      IF (IFLAG.EQ.0) RETURN
C
C     Update time to standard
      NTIME = ITIME
      IF (JOFS.NE.0) NTIME = NTIME + INTL - JOFS
      CALL DATCLL (JUL, NTIME, JUL, ITIME)
      IF (IFLAG.NE.2) RETURN
C
C     Adjust according to offset read in
      IF (IOFS.NE.0) THEN
      NTIME = NTIME - INTL + IOFS
      CALL DATCLL (JUL, NTIME, JUL, ITIME)
      ENDIF
      RETURN
C
 20   CONTINUE
      NTIME = ITIME
      CALL DATCLL (JUL, NTIME, JL, ITIM)
      IF (INTL.EQ.10080) GO TO 60
      I = JL + 1
      I = JLIYMD (I,IY,IM,ID)
      IF (INTL.GT.50000) GO TO 40
C
C     CHECK TO SEE IF ON INTERVAL
      IF (ITIM.NE.1440) GO TO 30
      IF (ID.NE.1) GO TO 30
 25   JOFS = 0
 26   CONTINUE
      IF (IFLAG.LE.1) IOFS = JOFS
      IF (IFLAG.EQ.0) RETURN
      JUL = JL
      ITIME = ITIM
      IF ((IFLAG.EQ.2).AND.(IOFS.NE.0)) THEN
C     DECREMENT THE DATE/TIME ONE PERIOD, THEN ADD OFFSET
      I = INCTIM (INTL, 0, -1, JUL, ITIME, JL, ITIM)
      NTIME = ITIM + IOFS
      CALL DATCLL (JL, NTIME, JUL, ITIME)
      ENDIF
      RETURN
C
C     MONTHLY DATA, OFF INTERVAL
 30   CONTINUE
      I = JLIYMD (JL,IY,IM,ID)
      J = ID - 1
      I = J * 1440
      JOFS = I + ITIM
      IF (ID.GE.28) JOFS = 0
      JL = IYMDJL(IY,IM,1)
      I = INCTIM(INTL,0,1,JL,0,JL,ITIM)
      GO TO 26
C
C     YEARLY DATA
 40   CONTINUE
      IF (ITIM.NE.1440) GO TO 50
      IF ((ID.NE.1).OR.(IM.NE.1)) GO TO 50
      GO TO 25
C
C     YEARLY DATA, OFF INTERVAL
 50   CONTINUE
      I = JLIYMD(JL,IY,IM,ID)
      I = IYMDJL(IY,1,1)
      JOFS = ((JL - I) * 1440) + ITIM
      IF ((IM.EQ.12).AND.(ID.GE.30)) JOFS = 0
      I = INCTIM(INTL,0,1,I,0,JL,ITIM)
      GO TO 26
C
C     WEEKLY DATA
 60   CONTINUE
      NDAY = IDAYWK(JL)
      IF ((NDAY.NE.7).OR.(ITIM.NE.1440)) GO TO 65
      GO TO 25
C
C     WEEKLY DATA, OFF INTERVAL
 65   CONTINUE
      JOFS = ((NDAY - 1) * 1440) + ITIM
      IF (JOFS.NE.0) THEN
      NTIME = ITIM + INTL - JOFS
      CALL DATCLL (JL, NTIME, JL, ITIM)
      ENDIF
      GO TO 26
C
      END
