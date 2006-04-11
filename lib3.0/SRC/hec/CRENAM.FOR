      SUBROUTINE CRENAM (COLDNM,CNEWNM,IERR)
C
C
C     INTEGER NAME1(6),NAME2(6)                                         H
      CHARACTER COLDNM*(*),CNEWNM*(*)
      CHARACTER*64 CNMOLD, CNMNEW                                       MLlg
C
      CNMOLD = COLDNM                                                   MLlg
      CNMNEW = CNEWNM                                                   MLlg
C
C
C     DO 10 I = 1,6                                                     H
C       NAME1(I) = 3H                                                   H
C       NAME2(I) = 3H                                                   H
C10   CONTINUE                                                          H
C
C     ILEN1 = 18                                                        H
C     ILEN2 = 18                                                        H
C     J1 = LEN(COLDNM)                                                  H
C     J2 = LEN(CNEWNM)                                                  H
C     IF (J1 .LT. ILEN1) ILEN1 = J1                                     H
C     IF (J2 .LT. ILEN2) ILEN2 = J2                                     H
C
C     CALL CHRHOL(COLDNM,1,ILEN1,NAME1,1)                               H
C     CALL CHRHOL(CNEWNM,1,ILEN2,NAME2,1)                               H
C
C     CALL RENAME(NAME1,NAME2,IERR)                                     H
C
      CALL CHRLNB(CNMOLD,J1)                                            MLlg
      CALL CHRLNB(CNMNEW,J2)                                            MLlg
      CNMOLD(J1+1:J1+1) = CHAR(0)                                       MLlg
      CNMNEW(J2+1:J2+1) = CHAR(0)                                       MLlg
C
      CALL RNAMF (CNMOLD, CNMNEW, IERR)                                 MLlg
C
 
C
      RETURN
      END
