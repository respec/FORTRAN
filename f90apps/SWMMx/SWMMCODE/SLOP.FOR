      SUBROUTINE SLOP
C=======================================================================
C     ROUTINE TO SEQUENCE ELEMENTS FOR COMPUTATION.
C     THIS VERSION OF SLOP USES VECTOR 'NIN' TO POINT TO INTERNAL NUMBER
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      CHARACTER BMJ*10
C=======================================================================
C     ZERO OUT ARRAYS
C=======================================================================
      NEP1    = NE + 1
      BMJ     = ' '
      DO 10 I = 1,NE
      IOLD(I) = 0
   10 JR(I)   = 0
C=======================================================================
C     COMPUTE INTERNAL UPSTREAM ELEMENT NUMBERS.
C=======================================================================
      DO 15 N = 1,NE
      DO 15 J = 1,3
C=======================================================================
C     IF THERE IS NO UPSTREAM ELEMENT, ASSIGN ARTIFICIAL VALUE.
C     OR, UPSTREAM ELEMENT NUMBERS NOW FOUND FROM POINTING VECTOR, 'NIN'.
C=======================================================================
      IF(JCE.EQ.0) THEN
                   IF(NUE(N,J).LE.0)   THEN
                                       INUE(N,J) = NEP1
                                       ELSE
                                       L         = NUE(N,J)
                                       INUE(N,J) = NIN(L,BMJ)
                                       ENDIF
                   ELSE
                   IF(KUE(N,J).EQ.' ') THEN
                                       INUE(N,J) = NEP1
                                       ELSE
                                       BMJ       = KUE(N,J)
                                       INUE(N,J) = NIN(L,BMJ)
                                       ENDIF
                   ENDIF
   15 CONTINUE
C=======================================================================
C     SEQUENCE ELEMENTS FOR COMPUTATION.
C     FLOW MAY BE ROUTED IN ELEMENT I IF IT HAS BEEN ROUTED IN ALL
C     UPSTREAM ELEMENTS.
C=======================================================================
      DO 30 N = 1,NE
      I       = 1
   17 IF(IOLD(I)) 20,20,18
   18 I       = I+1
      IF (I-NE) 17,17,30
   20 DO 25 J = 1,3
      IF(JCE.EQ.0.AND.NUE(I,J).LE.0)   GO TO 25
      IF(JCE.EQ.1.AND.KUE(I,J).EQ.' ') GO TO 25
   22 L       = INUE(I,J)
      IF (IOLD(L)) 18,18,25
   25 CONTINUE
      IOLD(I) = 1
      JR(N)   = I
   30 CONTINUE
C=======================================================================
      WRITE(N6,901) NEP1
      WRITE(N6,905)
      DO 50 I = 1,NE
      L       = JR(I)
      NT      = NTYPE(I)
      IF(JCE.EQ.1) WRITE(N6,916) KOE(I),I,NT,NAME(NT),
     +             (KUE(I,J),J=1,3),I,KOE(L),L,(INUE(L,J),J=1,3)
   50 IF(JCE.EQ.0) WRITE(N6,906) NOE(I),I,NT,NAME(NT),
     +             (NUE(I,J),J=1,3),I,NOE(L),L,(INUE(L,J),J=1,3)
C=======================================================================
      DO 100 I  = 1,NE
      L         = JR(I)
      NORDER(I) = 0
      DO 200 J  = 1,NE
      M         = JR(J)
      DO 200 K  = 1,3
      IF(JCE.EQ.0.AND.NUE(M,K).EQ.NOE(L)) GO TO 100
      IF(JCE.EQ.1.AND.KUE(M,K).EQ.KOE(L)) GO TO 100
  200 CONTINUE
      NORDER(I) = 1
  100 CONTINUE
      RETURN
C=======================================================================
  901 FORMAT(1H1,/,
     1' ****************************************************',/,
     2' *     ELEMENT LINKAGES AND COMPUTATION SEQUENCE    *',/,
     3' ****************************************************',/,
     4' * NOTE: ELEMENT # 0 IS GIVEN AN INTERNAL # OF ',I4,' *',/,
     5' ****************************************************',/)
  905 FORMAT(' EXTERNAL INTERNAL                       UPSTREAM ELEMENTS
     1',7X,'ORDER OF COMPUTATIONS AT EACH TIME STEP (PROCEEDING DOWNSTRE
     2AM)',/,' ELEMENT  ELEMENT',26X,'(EXTERNAL NOS.)',7X,'COMPUTATION
     3EXTERNAL INTERNAL  INTERNAL UPSTREAM',/,' NUMBER   NUMBER',
     4'   TYPE  DISCRIPTION',9X,'1     2     3',10X,
     5'SEQUENCE    NUMBER   NUMBER   ELEMENT  NUMBERS',
     5/,' ------   ------   ----  -----------',7X,'----------------',
     59X,'--------    ------   ------   ----------------')
  906 FORMAT(I6,I9,I7,3X,A16,3I6,6X,I11,I10,I9,1X,3I6)
  916 FORMAT(1X,A9,I5,I7,3X,A16,3A8,I10,1X,A10,I9,1X,3I6)
C=======================================================================
      END
