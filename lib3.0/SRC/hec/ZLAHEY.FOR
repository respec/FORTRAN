      SUBROUTINE ZLAHEY (CNAME, IUNIT)
C
      CHARACTER CNAME*(*), CH*4
      INTEGER*4 IVALUE, IADD, ITEMP                                     L
C
      OPEN (UNIT=IUNIT, ACCESS='DIRECT', RECL=4, FILE=CNAME,            L
     * STATUS='OLD', IOSTAT=ISTAT)                                      L
C
C     Check that this is a DSS file
      READ (UNIT=IUNIT, REC=1, ERR=800, IOSTAT=ISTAT) IVALUE            L
      CALL HOLCHR (IVALUE, 1, 4, CH, 1)                                 L
      IF (CH.NE.'ZDSS') GO TO 800                                       L
C
C     Check that this is a version 6 DSS file
      READ (UNIT=IUNIT, REC=5, ERR=800, IOSTAT=ISTAT) IVALUE            L
      CALL HOLCHR (IVALUE, 1, 4, CH, 1)                                 L
      IF (CH(1:2).NE.'6-') GO TO 800                                    L
C
C     Get the current file size.
      READ (UNIT=IUNIT, REC=6, ERR=800, IOSTAT=ISTAT) IADD              L
C     Compute the location of the last word in the file
      ITEMP = (IADD - 1) / 127                                          L
      IADD  = (ITEMP + 1) * 128                                         L
C
C     See if this word exists
      READ (UNIT=IUNIT, REC=IADD, IOSTAT=ISTAT) IVALUE                  L
C     If it does not (ISTAT set to -1), write it (as a zero value)
      IF (ISTAT.LT.0) THEN                                              L
      IVALUE = 0                                                        L
      WRITE (UNIT=IUNIT, REC=IADD) IVALUE                               L
      ENDIF                                                             L
C
 800  CONTINUE
C     Now close the file
      CLOSE (UNIT=IUNIT)                                                L
      RETURN
      END
