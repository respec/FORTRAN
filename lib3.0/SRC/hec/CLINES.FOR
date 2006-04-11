      SUBROUTINE CLINES (NLINES)
C
C       INTEGER   NLINES, ISTAT, NSTR                                   H
C       CHARACTER CSTR*80                                               H
C
C       returns the number of lines on the terminal screen
C         (1) checks the LNS register
C         (2) uses the STM register to determine the terminal type,
C             which is searched for in the COED terminal definition
C             file.
C         (3) if 1 and 2 fail, the number of lines is returned from
C             the Harris GEN file.
C
C         Alaric Clinton                                1 JULY 1988
C
C       * check the #LNS register *
C       CALL GSTRRG ('LNS', CSTR, NSTR, ISTAT)                          H
C       IF ((ISTAT .NE. 0) .OR. (NSTR .LE. 0)) THEN                     H
C         CALL CLINED (NLINES)                                          H
C       ELSE                                                            H
C         NLINES = INTGR (CSTR, 1, NSTR, ISTAT)                         H
C         IF ((NLINES .LT. 1) .OR. (ISTAT .NE. 0)) CALL CLINED (NLINES) H
C       ENDIF                                                           H
C     RETURN                                                            H
C
C     ROUTINE FOR NON-HARRIS SITES
C
C     NLINES = 24                                                       PAClg
      NLINES = 25                                                       ML
      RETURN                                                            PMACLlg
      END
