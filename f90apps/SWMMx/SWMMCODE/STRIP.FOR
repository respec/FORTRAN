      SUBROUTINE STRIP(N55)
C=======================================================================
C     THIS SUBROUTINE STRIPS COMMENT LINES FROM THE SWMM INPUT FILE
C     Note to Vax Programmers: An output format error will occur
C                              unless special provisions are made by the
C                              VAX operator.  An alternative is to change
C                              all occurrences of 230 to 120 in the
C                              STRIP subroutine.
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER ICODE*1,JCODE*1,ISTAR*1,KCODE*230,DOLL*1,BLANK*1,LEND*10
      DATA ISTAR/'*'/,DOLL/'$'/,BLANK/' '/,LEND/'''  ''ENDPR'/
C=======================================================================
      WRITE(*,999)
      DO 100 I = 1,1000000
      READ(N55,1,END=101,ERR=101) ICODE,JCODE,KCODE
      IF(ICODE.EQ.ISTAR) GO TO 100
      IF(ICODE.EQ.DOLL) THEN
                        WRITE(N5,2) ICODE,JCODE,KCODE
                        ELSE IF(ICODE.EQ.BLANK) THEN
                             WRITE(N5,5) ICODE,JCODE,KCODE
                             ELSE
                             WRITE(N5,4) ICODE,JCODE,KCODE
                             ENDIF
100   CONTINUE
101   WRITE(N5,6) LEND
      CLOSE (N55)
      REWIND N5
1     FORMAT(2A1,A230)
2     FORMAT('''',A1,'''',1X,A1,A230)
3     FORMAT(A230)
4     FORMAT('''',2A1,''' ',A230)
5     FORMAT(2A1,A230)
6     FORMAT(A10)
999   FORMAT(/,' Reading the input file and deleting comment lines.',/,
     +       ' This process is much faster when a RAM drive is used.',/)
      RETURN
      END
