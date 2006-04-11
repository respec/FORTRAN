      SUBROUTINE FILECK(IO,FNAME,DEFORM)
C      ADD FOLLOWING LINE FOR DIGITAL VISUAL FORTRAN
C      USE DFPORT
C=======================================================================
C     Subroutine to check form (formatted vs. unformatted) of existing
C     file to avoid run-stopping error when desired form does not
C     match existing form.
C     This problem does not occur with an empty file, only one 
C     previously used that contains data in the wrong form.  
C     At program pause, user will have option to allow deletion of
C     existing file (FNAME), and continue run.
C     Note: Subroutine SYSTEM may be Lahey-specific, in which case 
C     do not use this subroutine or references to it, and just suffer
C     through resulting error message.
C     Wayne Huber, 7/25/96.
C     Modified 7/97 to check if file exists  C. Moore CDM 6/25/97
C     Add EX to logical, INQUIRE, and add check to return if file
C     doesn't exist.
C=======================================================================
C     IO    = unit number of file to be checked.
C     FNAME = DOS name of file
C     DEFORM = desired form for file (FORMATTED or UNFORMATTED)
C=======================================================================
      CHARACTER*60 FNAME
      CHARACTER*11 DEFORM,ACFORM
      LOGICAL OPEND,EX
C=======================================================================
C     If no file is specified (IO = 0), do nothing.
C=======================================================================
      IF(IO.LE.0) RETURN
C=======================================================================
C     Otherwise, check form of file FNAME to see if is OK to open.
C     OK if form about to be used matches existing form.  
C     Otherwise, print message and ask user if it is OK to delete
C     the offending file.
C=======================================================================
      INQUIRE(IO,FORM=ACFORM,OPENED=OPEND,exist=EX)
      IF (.NOT.EX) RETURN
      IF(ACFORM.EQ.DEFORM) THEN
           IF(OPEND) REWIND IO
           RETURN
           ELSE
CIM ELIMINATE WRITE AND PAUSE
c           WRITE(*,9000) IO,FNAME,DEFORM,IO
c           PAUSE
C=======================================================================
C     Prepare a delete command by concatenating DOS delete command
C     with offending file name.
C     Caution.  Subroutine SYSTEM allows a DOS command to be executed.
C     This may be Lahey-specific.
C=======================================================================
c the following line is lahey
           CALL SYSTEM('del '//FNAME)
c  the following line is digital visual fortran
c           IOEER = SYSTEM('del '//FNAME)
           ENDIF
      RETURN
C=======================================================================
c 9000 FORMAT(/,' Program is about to stop with error message for unit',
c     1 I3,/,' about ''form'' of file: ',A60,/,
c     2' File momentarily should be of form: ',A11,', but is not.',/,
c     3' At prompt, press <enter> to delete above file and then',/,
c     4' over-write it, or press <control>-C or <control>-break',/,
c     5' to end run and not delete the file.  In this latter case,',/,
c     6' provide new file name on @-line for unit',I3,' and run again.')
      END


      


