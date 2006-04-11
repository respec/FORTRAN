C
C
C
      SUBROUTINE   GETOSV
     I                  (KEYST,KEYND,MAXOSV,
     O                   OSVI)
C
C     + + + PURPOSE + + +
C     Move an osv from osvfl into memory
C
C     + + + MODULES + + +
      USE  OSV
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   KEYND,KEYST,MAXOSV,OSVI(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + END SPECIFICATIONS + + +
C
      CALL OSV_GET (KEYST,KEYND,MAXOSV,
     O              OSVI)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOSV
     I                   (KEYST,KEYND,MAXOSV,OSVI)
C
C     + + + PURPOSE + + +
C     Move an osv from memory to osvfl
C
C     + + + MODULES + + +
      USE  OSV
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,MAXOSV,OSVI(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + END SPECIFICATIONS + + +
C
      CALL OSV_PUT (KEYST,KEYND,MAXOSV,OSVI)
C
      RETURN
      END
