C     dummy for dll's where no screen stuff available
C
      SUBROUTINE   GETKEY
     O                    (GROUP,CODE)
      INTEGER   GROUP,CODE
      END
C
      SUBROUTINE   SCPRBN
     I                    (LEN,RMFLG,CRFLG,STR)
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(255)
      END
C
      SUBROUTINE   C1IN
     O                 (I2CHAR)
      INTEGER*2   I2CHAR
      END
C
      SUBROUTINE   C1INT
     O                  (I2CHAR)
      INTEGER*2 I2CHAR
      END
C
      SUBROUTINE   OSDIRL
     I                   (WRKDIR,IFNAME,
     O                    UMFNMS,UMNFIL)
      CHARACTER*64 WRKDIR,IFNAME,UMFNMS(*)
      INTEGER      UMNFIL
      END
