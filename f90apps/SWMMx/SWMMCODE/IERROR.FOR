      SUBROUTINE IERROR
C=======================================================================
C     Tell the user of an input data error.
C     WCH, 10/5/93.  Add option to read correctly when near end of user
C       input data file.
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER JCODE*120
      BACKSPACE N5
C#### wch, 10/5/93.  ADD END=30.
      READ(N5,10,END=30) JCODE
   10 FORMAT(A120)
      WRITE(N6,20) CC,JCODE
      WRITE(*,20) CC,JCODE
C#### WCH, 10/5/93.  ALTER BOB'S FAMOUS ERROR MESSAGE SLIGHTLY.
   20 FORMAT(/,' You had an error in your input data.',/,' Did you inclu
     1de enough values or zeroes for all parameters on an input line??'
     2,/,' The program attempted to read line --> ',A2,' and this is an 
     3echo of what it found:',/,1X,A120)
      WRITE(N6,21)
      WRITE(*,21)
   21 FORMAT(/,' ===> Run ended due to input error(s) in either the ',
     +       /,'      above line or the previous line.')
      STOP
C#### WCH, 10/5/93.  ADD OPTION WHEN WITHIN 120 CHARACTERS OF END OF DATA.
   30 WRITE(N6,31) CC,JCODE
      WRITE(*,31)  CC,JCODE
   31 FORMAT (/,' You had an error in your input data.',/,' Did you incl
     1ude enough values or zeroes for all parameters on an input line??'
     2,/,' You are near the end of your input data file.',/,' The progra
     3m attempted to read line --> ',A2,' and this is an echo of what it
     4 found:',/,1X,A120,/,' (This may include an echo of more than one 
     5line.)')
      WRITE(N6,21)
      WRITE(*,21)
      STOP
      END
