C     dummy for dll's where no screen stuff available
C
      SUBROUTINE   QCHR
     O                   (CHR,ICHR)
      INTEGER     ICHR
      CHARACTER*1 CHR
      END
C
      SUBROUTINE   SCCUHM
     O                    (CROW,CCOL)
      INTEGER   CROW,CCOL
      END
C
      SUBROUTINE   SCPRST
     I                     (LEN,STR)
      INTEGER     LEN
      CHARACTER*1 STR(256)
      END
C
      SUBROUTINE   SCPRBF
     I                    (LEN,RMFLG,CRFLG,STR)
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(*)
      END
C
      SUBROUTINE   COLSET
     I                    (FOR,BAC)
      INTEGER    FOR,BAC
      END
C
      SUBROUTINE   COLGET
     O                    (FOR,BAC)
      INTEGER    FOR,BAC
      END
C
      SUBROUTINE   ZBEEP
      END
C
      SUBROUTINE   ZFMTWR
     I                    (STRING)
      CHARACTER STRING*(*)
      END
C
      SUBROUTINE   ZWRSCR
     I                    (STRING,LINE,COLUMN)
      INTEGER   LINE,COLUMN
      CHARACTER STRING*(*)
      END
