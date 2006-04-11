C
C     CTERIF.INC 2.1 9/4/91
C
      COMMON /TERI/ TERIFL, TEMPFL, LOGFL
      INTEGER       TERIFL, TEMPFL, LOGFL
C
      SAVE   /TERI/
C
C     + + + DEFINITIONS + + +
C     TERIFL - Fortran unit number for input from terminal or log file
C     TEMPFL - Same as initial TERIFL to use after TERIFL changed to
C     LOGFL  - Fortran unit number for the log file
C              LOGIFL for input from log file
C
