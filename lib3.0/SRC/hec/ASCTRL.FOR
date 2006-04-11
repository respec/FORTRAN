      SUBROUTINE ASCTRL (IUNIT, CFUN, IARG1, IARG2)
C
C     ANSI Screen Control - Erases screen, moves cursor,
C     changes attributes, etc. for terminals meeting ANSI standards.
C
      CHARACTER CFUN*(*)
      CHARACTER CACT(19)*2, CSEQ(19)*3, CSTR*9, CTERM*9
C
C
C     Move Cursor
      DATA CACT(1)  /'MC'/, CSEQ(1)  /'[X '/
C     Delete Character(s)
      DATA CACT(2)  /'DC'/, CSEQ(2)  /'[P '/
C     Cursor Right
      DATA CACT(3)  /'CR'/, CSEQ(3)  /'[C '/
C     Cursor Left
      DATA CACT(4)  /'CL'/, CSEQ(4)  /'[D '/
C     Cursor Up
      DATA CACT(5)  /'CU'/, CSEQ(5)  /'[A '/
C     Cursor Down
      DATA CACT(6)  /'CD'/, CSEQ(6)  /'[B '/
C     Delete Line(s)
      DATA CACT(7)  /'DL'/, CSEQ(7)  /'[M '/
C     Insert Line(s)
      DATA CACT(8)  /'IL'/, CSEQ(8)  /'[L '/
C     Insert Character Mode
      DATA CACT(9)  /'IC'/, CSEQ(9)  /'[4h'/
C     Type Over Mode
      DATA CACT(10) /'TO'/, CSEQ(10) /'[4l'/
C     Clear Screen
      DATA CACT(11) /'CS'/, CSEQ(11) /'[2J'/
C     Erase Line
      DATA CACT(12) /'EL'/, CSEQ(12) /'[2K'/
C     Keypad Application Mode
      DATA CACT(13) /'KA'/, CSEQ(13) /'=  '/
C     Keypad Numeric Mode
      DATA CACT(14) /'KN'/, CSEQ(14) /'>  '/
C     Bold
      DATA CACT(15) /'BO'/, CSEQ(15) /'[1m'/
C     Blinking
      DATA CACT(16) /'BL'/, CSEQ(16) /'[5m'/
C     Underline
      DATA CACT(17) /'UL'/, CSEQ(17) /'[4m'/
C     Reverse Video
      DATA CACT(18) /'RV'/, CSEQ(18) /'[7m'/
C     Normal Attributes
      DATA CACT(19) /'NO'/, CSEQ(19) /'[0m'/
C
C
C     Is the command function long enough?
      J = LEN (CFUN)
      IF (J.LT.2) RETURN
C
C     Determine which function this is
      DO 10 I=1,19
      IF (CFUN.EQ.CACT(I)) THEN
      IACT = I
      GO TO 20
      ENDIF
 10   CONTINUE
C     Function not recognized - return
      RETURN
C
 20   CONTINUE
C
C     Is this the cursor move function? (special function)
      IF (IACT.EQ.1) THEN
      WRITE (CSTR,30) IARG1, IARG2
 30   FORMAT ('[',I3,';',I3,'H')
      GO TO 100
C
C     Does this function use an argument?
      ELSE IF (IACT.LE.8) THEN
      IF (IARG1.GT.1) THEN
      CSTR = CSEQ(IACT)(1:1)//'   '//CSEQ(IACT)(2:2)
      WRITE (CSTR(2:4),'(I3)') IARG1
      GO TO 100
      ENDIF
      ENDIF
C
C     No Arguments - send out sequence
      CALL CHRLNB (CSEQ(IACT), NCHR)
      CALL CHRWT (IUNIT, CHAR(27)//CSEQ(IACT), NCHR+1)
      RETURN
C
C     Arguements used - remove blanks then send out sequence
 100  CONTINUE
      CALL REMBLK (CSTR, CTERM, NCHR)
      CALL CHRWT (IUNIT, CHAR(27)//CTERM, NCHR+1)
      RETURN
C
      END
