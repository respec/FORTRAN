      SUBROUTINE CLINE
C=======================================================================
C   Option for command line input of file names.
C   Written by Chuck Moore of CDM, 8/93.
C=======================================================================
C   This subroutine uses RMFortran-specific functions ARGC and ARGV to
C     return number of arguments on command line (including .EXE file)
C     and character values of the arguments, respectively.
C   For Lahey Fortran, must include Function ARGC to accomplish same
C     task.
C=======================================================================
C     WCH, 10/2/96.  Declare FPOS as integer to avoid Watcom compiler
C       errors.  (Bruce LaZerte)
C=======================================================================
      INCLUDE 'CLNAME.INC'
      CHARACTER*50 ARGV,FNAME
      CHARACTER*1 FIRSTCH
C#### WCH (B. LAZERTE), 10/2/96.  ALSO DECLARE FPOS AS INTEGER.
      INTEGER*2 I,ARGC,IFIL,FPOS
C
      INAMES=0
      IFIL = ARGC( )
      NNAMES=0
      IF (IFIL.EQ.1) RETURN
C=======================================================================
C   Note that program name is first name recognized; hence, ARGC always
C     > 0.
C=======================================================================
      DO 100 I= 2,IFIL
      FNAME=ARGV(I-1)
      FIRSTCH=FNAME
C=======================================================================
C   Allow for RM Fortran switch options to be included on command line.
C=======================================================================
      IF (FIRSTCH.EQ.'/')   GO TO 100
       NNAMES=NNAMES+1
       CNAMES(NNAMES) = FNAME
100    CONTINUE
      RETURN
      END
C
C=======================================================================
C
C      PROGRAM TEST
C      INTEGER NUML,ARGC
C      CHARACTER*50 FNAME,ARGV
C      NUML = ARGC()
C      WRITE(6,*) NUML
C      DO 10 J=1,NUML
C      FNAME=ARGV(J)
C  10  WRITE(6,*) '-',FNAME,'-'
C      STOP
C      END

      INTEGER FUNCTION ARGC()
C
C      FUNCTION TO ACT LIKE RMFORTRAN ARGC FUNCTION
C        RETURNS THE NUMBER OF ARGUMENTS IN COMMAND LINE
C        THE PROGRAM NAME COUNTS AS ONE ARGUMENT  CIM 11/92
      CHARACTER*132 INLINE
      LOGICAL INSTRING
C        THIS NEXT LINE IS THE LAHEY PROCEDURE TO GET COMMAND LINE STRING
      CALL GETCL(INLINE)
C     NBLANK IS LAHEY PROCEDURE TO RETURN INDEX OF LAST NON-ZERO CHARACTER IN STRING
c  len_trim is alternative
      ITEMP=NBLANK(INLINE)
c     ITEMP=LEN_TRIM(INLINE)
      NCOUNT = 1
      INSTRING=.FALSE.
      DO 10 J=1,ITEMP
      IF ((INLINE(J:J).EQ.' ') .AND. (INSTRING)) INSTRING=.FALSE.
      IF ((INLINE(J:J).NE.' ') .AND. (.NOT. INSTRING)) THEN
      INSTRING=.TRUE.
      NCOUNT=NCOUNT+1
      END IF
   10 CONTINUE
      ARGC=NCOUNT
      RETURN
      END

      CHARACTER*(*) FUNCTION ARGV(INDEX)
C     PROGRAM TO ACT LIKE RMFORTRAN ARGV
C     RETURNS THE STRING IN COMMAND LINE SPECIFIED BY INDEX
C     INDEX =1 RETURNS THE FIRST ONE, INDEX 2 RETURNS THE SECOND
C     A BLANK LINE IS RETURNED IF INDEX EXCEEDS NUMBER OF STRINGS
C     STRINGS ARE DELIMITED BY BLANKS ON COMMAND LINE    CIM 11/92
C
C        THIS NEXT LINE IS THE LAHEY PROCEDURE TO GET COMMAND LINE STRING
      CHARACTER*132 INLINE
      INTEGER ARGC
      LOGICAL INSTRING
      CALL GETCL(INLINE)
C     NBLANK IS LAHEY PROCEDURE TO RETURN INDEX OF LAST NON-ZERO CHARACTER IN STRING
       ITEMP=NBLANK(INLINE)
C     ITEMP=LEN_TRIM(INLINE)
      NUML = ARGC()
      IF (INDEX.GT.(NUML-1)) THEN
      ARGV=' '
      RETURN
      END IF
      NCOUNT=0
      INSTRING=.FALSE.
      DO 10 J=1,ITEMP
      IF ((INLINE(J:J).EQ.' ') .AND. (INSTRING)) THEN
       INSTRING=.FALSE.
       IF (NCOUNT.EQ.INDEX) GO TO 20
      END IF
      IF ((INLINE(J:J).NE.' ') .AND. (.NOT. INSTRING)) THEN
      INSTRING=.TRUE.
      ISTART=J
      NCOUNT=NCOUNT+1
      END IF
   10 CONTINUE
   20 CONTINUE
      IEND=J-1
      ARGV=INLINE(ISTART:IEND)
      END
