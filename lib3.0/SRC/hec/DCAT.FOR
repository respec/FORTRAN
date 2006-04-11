      SUBROUTINE DCAT ( EAX, AH, AL )
C
      INTEGER AH, AL, EAX
C
C     AL = MOD(EAX,256)
C     AH = EAX/256
C
      AL = IAND(EAX,255)
      AH = IAND(ISHFT(EAX,-8),255)
C
      RETURN
      END
