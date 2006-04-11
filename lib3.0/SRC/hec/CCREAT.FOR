      SUBROUTINE CCREAT (CNAME,IGRAN,IPACK,ITYPE,IERR)
C
C
      CHARACTER CNAME*(*)
      LOGICAL L                                                         L
C     CHARACTER CNAM*20                                                 Mlg
C     INTEGER NAME(6)                                                   H
C
C
C     DO 10 I = 1,6                                                     H
C     NAME(I) = 3H                                                      H
C10   CONTINUE                                                          H
C
C     ILEN = 18                                                         H
C     J = LEN(CNAME)                                                    H
C     IF (J .LT. ILEN) ILEN = J                                         H
C     CALL CHRHOL(CNAME,1,ILEN,NAME,1)                                  H
C     CALL CREATE(NAME,IGRAN,IPACK,ITYPE,IERR)                          H
C
C     CNAM = CNAME                                                      Mlg
C     J = NINDXR(CNAM,' ')                                              Mlg
C     CALL CREAF(CNAM(1:J)//CHAR(0),0,IH,IERR)                          Mlg
C     CALL CLOSF(IH,IERR)                                               Mlg
C
      DO 20 I=50,90                                                     L
      INQUIRE (UNIT=I,OPENED=L)                                         L
      IF (.NOT.L) THEN                                                  L
      OPEN (UNIT=I,FILE=CNAME,IOSTAT=IERR)                              L
      CLOSE (UNIT=I)                                                    L
      RETURN                                                            L
      ENDIF                                                             L
 20   CONTINUE                                                          L
C
      RETURN
      END
