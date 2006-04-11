      SUBROUTINE CLINEDVS
C      USE DFLIB
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
C   Modified for Digital Visual Fortran
C   CIM 6/97
C=======================================================================
      INCLUDE 'CLNAME.INC'
      CHARACTER*50 ARGV,FNAME
      CHARACTER*1 FIRSTCH
C#### WCH (B. LAZERTE), 10/2/96.  ALSO DECLARE FPOS AS INTEGER.
      INTEGER*2 I,ARGC,IFIL,FPOS
C
      INAMES=0
      IFIL = NARGS()
      NNAMES=0
      IF (IFIL.EQ.1) RETURN
C=======================================================================
C   Note that program name is first name recognized; hence, ARGC always
C     > 0.
C=======================================================================
      DO 100 I= 1,IFIL-1
        CALL GETARG (I,FNAME)
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
