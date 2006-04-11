      SUBROUTINE PPLOT(IX,IY,K,NPLOT,ALPLOT)
C=======================================================================
C     THIS PART OF GRAPHING ROUTINES PRINTS THE 51 BY 101 ARRAY, A,
C               OF POINTS.
C     IT ALSO ASSIGNS SYMBOLS TO THE ARRAY AND PRINTS TITLES, LABELS.
C     THIS SUBROUTINE LAST MODIFIED MARCH, 1988 BY RED.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PLTARY.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'LAB.INC'
C=======================================================================
      CHARACTER ALPLOT*10,SYM(9)*1
      DATA SYM/'*','+','''','X','.','2',' ','I','-'/
C=======================================================================
C     PLACE PROPER SYMBOL IN ARRAY LOCATION.
C=======================================================================
      IF(K-99) 10,20,240
   10 A(51-IY,IX+1) = SYM(K)
      RETURN
C=======================================================================
C     PRINT TOTAL ARRAY, A, PLUS LABELS AND TITLES.
C=======================================================================
   20 CONTINUE
      I  = 0
      J2 = 1
      WRITE(N6,31)
      DO 180 II = 1,6
      I         = I+1
      IF(YLAB(1).LE.100000.0) THEN
                              WRITE(N6,90) YLAB(II),(A(I,J),J=1,101)
                              ELSE IF(YLAB(1).GT.100000.0) THEN
                              WRITE(N6,100) YLAB(II),(A(I,J),J=1,101)
                              ELSE
                              WRITE(N6,95) YLAB(II),(A(I,J),J=1,101)
                              ENDIF
      IF(II.EQ.6) GO TO 190
C=======================================================================
      DO 170 JJ = J2,9
      I         = I+1
      IF(I.EQ.24) THEN
                  WRITE(N6,120) VERT1,(A(I,J),J=1,101)
                  ENDIF
      IF(I.EQ.26) THEN
                  WRITE(N6,120) VERT2,(A(I,J),J=1,101)
                  ENDIF
      IF(I.EQ.28) THEN
                  WRITE(N6,120) VERT3,(A(I,J),J=1,101)
                  ENDIF
      IF(I.NE.24.AND.I.NE.26.AND.I.NE.28) WRITE(N6,160) (A(I,J),J=1,101)
  170 CONTINUE
      J2 = 1
  180 CONTINUE
  190 CONTINUE
      WRITE(N6,200) XLAB
      IF(JCE.EQ.0) WRITE(N6,210) NPLOT,HORIZ(1),HORIZ(2)
      IF(JCE.EQ.1) WRITE(N6,211) ALPLOT,HORIZ(1),HORIZ(2)
      WRITE(N6,30) HTITLE
      RETURN
C=======================================================================
C     INITIALIZE PLOT OUTLINE.
C=======================================================================
  240 DO 260 I = 1,50
      DO 250 J = 1,101
  250 A(I,J)   = SYM(7)
      A(I,1)   = SYM(8)
      A(I,101) = SYM(8)
  260 CONTINUE
      DO 270 J = 1,101
      A(1,J)   = SYM(9)
  270 A(51,J)  = SYM(9)
      DO 280 I = 1,101,10
      A(1,I)   = SYM(8)
  280 A(51,I)  = SYM(8)
      DO 290 I = 11,41,10
      A(I,1)   = SYM(9)
      A(I,101) = SYM(9)
  290 CONTINUE
C=======================================================================
   30 FORMAT(/,18X,A30,1X,A30)
   31 FORMAT(/,'1')
   90 FORMAT(' ',F14.3,1X,101A1)
   95 FORMAT(' ',F14.6,1X,101A1)
  100 FORMAT(' ',1PE14.2,1X,101A1)
  120 FORMAT(1X,A10,5X,101A1)
  160 FORMAT(16X,101A1)
  200 FORMAT(F18.1,10F10.1)
  210 FORMAT(/,18X,'LOCATION NO. : ',I9,1X,A30,2X,A30)
  211 FORMAT(/,17X,'LOCATION NO. : ',A10,1X,A30,2X,A30)
C=======================================================================
      RETURN
      END
