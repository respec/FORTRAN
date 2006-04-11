      SUBROUTINE CURTIM ( JUL, MIN)
C
C     PROVIDES CURRENT HEC JULIAN DATE AND
C     TIME IN MINS PAST MIDNIGHT WHEN CALLED
C
      CHARACTER CDATE*8, CTIME*8, CHR*4                                 PICAL
C     INTEGER*4 JUL, IYMDJL                                             M
      INTEGER*4 JUL                                                     L
C     INTEGER*2 IHR,IMIN,ISEC,I100SC                                    M
C     INTEGER*2 IYR,IMON,IDAY                                           M
C
C     CALL DATIME ( IYR, IDAY, ITSEC)                                   H
C     JAN1 = IYMDJL ( IYR, 1, 1)                                        H
C     JUL = JAN1 + IDAY - 1                                             H
C     MIN = ITSEC/600                                                   H
C
      CALL WHEN ( CDATE, CTIME)                                         PICAL
      CHR(1:2) = CTIME(1:2)                                             PICAL
      CHR(3:4) = CTIME(4:5)                                             PICAL
      CALL DATJUL (CDATE, JUL, I)                                       PICAL
      MIN = IHM2M (CHR)                                                 PICAL
C
C     CALL GETTIM(IHR,IMIN,ISEC,I100SC)                                 M
C     JHR = IHR                                                         M
C     JMIN = IMIN                                                       M
C     MIN = JHR*60 + JMIN                                               M
C     CALL GETDAT(IYR,IMON,IDAY)                                        M
C     JYR = IYR                                                         M
C     JMON = IMON                                                       M
C     JDAY = IDAY                                                       M
C     JUL = IYMDJL(JYR,JMON,JDAY)                                       M
C
C     CALL SYSTIM (JUL, ISEC)                                           lg
C     MIN = ISEC / 60                                                   lg
C
      RETURN
      END
