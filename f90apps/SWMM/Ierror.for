      SUBROUTINE IERROR
C     MANY BLOCKS
C=======================================================================
C     Tell the user of an input data error.
C     WCH, 10/5/93.  Add option to read correctly when near end of user
C       input data file.
C     WCH, 10/17/95 and 8/15/96.  Add to error message slightly.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER JCODE*120
      BACKSPACE N5
C#### WCH, 10/5/93.  ADD END=30.
      READ(N5,10,END=30) JCODE
   10 FORMAT(A120)
      WRITE(N6,20) CC,JCODE
      WRITE(*,20) CC,JCODE
C#### WCH, 10/5/93 and 8/15/96.  ALTER BOB'S FAMOUS ERROR MESSAGE SLIGHTLY.
   20 FORMAT(/,' You had an error in your input data.',/,' Did you inclu
     1de enough values or zeroes for all parameters on this or',/,
     2' an earlier input line?'
     2,/,' Do you have a missing asterisk on a comment line?'
     3,/,' The program attempted to read line --> ',A2,' and this is an 
     3echo of what it found:',/,1X,A120)
      WRITE(N6,21)
      WRITE(*,21)
C#### WCH, 10/17/95 and 8/15/96.  Add to message.
   21 FORMAT(/,' ==> Run ended due to input error(s) in either the ',
     +       /,'     above line or some previous line.',/,
     2         '     Please check output file for likely additional erro
     3r or warning messages.')
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