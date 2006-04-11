      SUBROUTINE DATED
C#######################################################################
C     FIND THE YEAR, MONTH, DAY, HOUR, MINUTE AND SECOND CORRESPONDING
C     TO THE EXISTING JULDAY AND TIMDAY
C
C     JULDAY = JULIAN DAY
C     TIMDAY = TIME OF DAY IN SECONDS
C     CORRECTION BY WCH, 4/93 TO FIX CONVERSION FROM TIMDAY IN 
C       SECONDS TO HR/MIN/SEC.
C#######################################################################
      INCLUDE 'STIMER.INC'
C=======================================================================
      NYEAR = JULDAY/1000
      JEWEL = JULDAY - 1000*NYEAR
      IFEB  = 28
      IF((NYEAR/4)*4-NYEAR.EQ.0) IFEB = 29
      IF(JEWEL.LE.31)      THEN
                           MONTH = 1
                           NDAY  = JEWEL
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.31+IFEB) THEN
                           MONTH = 2
                           NDAY  = JEWEL - 31
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.62+IFEB) THEN
                           MONTH = 3
                           NDAY  = JEWEL - 31 - IFEB
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.92+IFEB) THEN
                           MONTH = 4
                           NDAY  = JEWEL - 62 - IFEB
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.123+IFEB) THEN
                            MONTH = 5
                            NDAY  = JEWEL - 92 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.153+IFEB) THEN
                            MONTH = 6
                            NDAY  = JEWEL - 123 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.184+IFEB) THEN
                            MONTH = 7
                            NDAY  = JEWEL - 153 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.215+IFEB) THEN
                            MONTH = 8
                            NDAY  = JEWEL - 184 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.245+IFEB) THEN
                            MONTH = 9
                            NDAY  = JEWEL - 215 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.276+IFEB) THEN
                            MONTH = 10
                            NDAY  = JEWEL - 245 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.306+IFEB) THEN
                            MONTH = 11
                            NDAY  = JEWEL - 276 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.337+IFEB) THEN
                            MONTH = 12
                            NDAY  = JEWEL - 306 - IFEB
                            ENDIF
C#### WCH, 4/20/93. CHANGE FRACTION ADDED TO 0.0001 HR.  MUST BE < 1 SEC.
  100 JHR     = IFIX(TIMDAY/3600.0 + 0.0001)
      JSEC    = IFIX(TIMDAY - FLOAT(JHR)*3600.0 + 0.01)
      MINUTE  = JSEC / 60
      JSEC    = JSEC - 60*MINUTE
      RETURN
      END
