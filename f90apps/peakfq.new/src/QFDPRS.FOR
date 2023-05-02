C
C
C
      SUBROUTINE   QFDPRS
     I                   (ISTR,
     O                    WRKDIR,IFNAME)
C
C     + + + PURPOSE + + +
C     Parse the file name and/or directory from an input string.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) ISTR,WRKDIR,IFNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR   - input string being parsed
C     WRKDIR - working directory
C     IFNAME - file name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,ILEN,IPRM
      CHARACTER DIRCHR
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
C     init file name and working directory
      IFNAME = ' '
      WRKDIR = ' '
C
C     assume pc, use \
      DIRCHR= CHAR(92)
C
      ILEN = ZLNTXT(ISTR)
      IF (ILEN .GT. 0) THEN
        N = ILEN + 1
 20     CONTINUE        
          N = N - 1
        IF (ISTR(N:N) .NE. DIRCHR .AND. N .GT. 1) GOTO 20
        IF (ISTR(N:N) .EQ. DIRCHR) THEN
C         directory indicator found, set working directory and file name
          WRKDIR = ISTR(1:N)
          IFNAME = ISTR(N+1:)
        ELSE
C         no directory indicator found, just set file name
          IFNAME = ISTR(1:ILEN)
        END IF
      END IF
C
      RETURN
      END
