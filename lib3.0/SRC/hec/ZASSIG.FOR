      SUBROUTINE ZASSIG ( IUNIT, CNAME, IMULT, LNEW)
C
C     **** A HARRIS MACHINE SPECIFIC ROUTINE *****
C     Makes a shared assignment for multiple user access
C     Will create a new file, if the file does not exist
C
C     Written by Bill Charley at HEC, 1984.
C
C     CHARACTER CNAME*(*)                                               H
C     LOGICAL LNEW                                                      H
C
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
C     LNEW = .FALSE.                                                    H
C     Test to see if the file exists, and is direct access
C     IF (LEXCL) THEN                                                   H
C     CALL ASSIGX ( IUNIT, CNAME, IERR)                                 H
C     ELSE                                                              H
C     CALL ASSIGS ( IUNIT, CNAME, IERR)                                 H
C     IF (IERR.EQ.23) THEN                                              H
C     CALL WAITS (5.0)                                                  H
C     CALL ASSIGS (IUNIT, CNAME, IERR)                                  H
C     ENDIF                                                             H
C     ENDIF                                                             H
C
C     IF (IERR.EQ.0) THEN                                               H
C     IMULT = 0                                                         H
C
C     ELSE IF (IERR.EQ.1) THEN                                          H
C
C     File does not exist - create it
C
C     CALL CCREAT ( CNAME, 400, 0, -2, JERR)                            H
C     LNEW = .TRUE.                                                     H
C     IF (JERR.NE.0) THEN                                               H
C     IF (MLEVEL.GT.0) WRITE ( MUNIT,40) CNAME, JERR                    H
C40   FORMAT (/,T5,'***WARNING:   -----DSS--ZOPEN; Unable to ',         H
C    * 'Create File ',A,/,T5,'Error =',I5)                              H
C     IMULT = -1                                                        H
C     GO TO 200                                                         H
C     ENDIF                                                             H
C
C     IF (MLEVEL.GE.1) WRITE (MUNIT,60) CNAME                           H
C60   FORMAT (T5,'-----DSS---ZOPEN; Created DSS File: ',A)              H
C
C     CALL CRETYP ( CNAME, '0000164, 0, JERR)                           H
C
C     IF (LEXCL) THEN                                                   H
C     CALL ASSIGX ( IUNIT, CNAME, JERR)                                 H
C     IF (JERR.NE.0) THEN                                               H
C     IF (MLEVEL.GT.0) WRITE ( MUNIT,70) CNAME, JERR                    H
C70   FORMAT (/,1X,'***ERROR: -----DSS--ZOPEN; Unable to ',             H
C    * 'make Exclusive Assignment',/' File: ',A,';  Error: ',I5)        H
C     IMULT = -2                                                        H
C     GO TO 200                                                         H
C     ENDIF                                                             H
C     ELSE                                                              H
C     CALL ASSIGS ( IUNIT, CNAME, JERR)                                 H
C     IF (JERR.NE.0) THEN                                               H
C     IF (MLEVEL.GT.0) WRITE ( MUNIT,80) CNAME, JERR                    H
C80   FORMAT (/,1X,'***ERROR: -----DSS--ZOPEN; Unable to ',             H
C    * 'make Shared Assignment',/' File: ',A,';  Error: ',I5)           H
C     IMULT = -2                                                        H
C     GO TO 200                                                         H
C     ENDIF                                                             H
C     ENDIF                                                             H
C
C     IMULT = 0                                                         H
C
C     ELSE                                                              H
C
C     If this is a batch job, and we are unable to connect,
C     delay for 4 minutes or until the file is available
C     CALL PTYPE (ITYPE)                                                H
C     IF ((ITYPE.GT.0).AND.((IERR.EQ.23).OR.(IERR.EQ.10))) THEN         H
C     IF (MLEVEL.GE.2) WRITE (MUNIT, 90)                                H
C90   FORMAT (' ZOPEN:  Delay for Assignment...')                       H
C     DO 100 I=1,48                                                     H
C     CALL WAITS (5.0)                                                  H
C     IF (LEXCL) THEN                                                   H
C     CALL ASSIGX ( IUNIT, CNAME, JERR)                                 H
C     ELSE                                                              H
C     CALL ASSIGS ( IUNIT, CNAME, JERR)                                 H
C     ENDIF                                                             H
C     IF (JERR.EQ.0) THEN                                               H
C     IMULT = 0                                                         H
C     GO TO 200                                                         H
C     ENDIF                                                             H
C100  CONTINUE                                                          H
C     ENDIF                                                             H
C
C     IF (MLEVEL.GT.0) WRITE ( MUNIT, 80) CNAME, IERR                   H
C     IMULT = -2                                                        H
C     GO TO 200                                                         H
C     ENDIF                                                             H
C
C
C200  CONTINUE                                                          H
      RETURN
      END
