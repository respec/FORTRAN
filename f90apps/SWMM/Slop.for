      SUBROUTINE SLOP
C     TRANSPORT BLOCK
C     CALLED BY INTRAN NEAR LINE 793
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
      LOGICAL SCREWED,ISSPLIT
C ARRAY USED TO CHECK HOW MANY TIMES ELEMENTS APPEAR IN NUK KUE ARRAYS
      DIMENSION NDOWN(NET)
C=======================================================================
C     ZERO OUT ARRAYS
C=======================================================================
       SCREWED = .FALSE.
      NEP1    = NE + 1
      BMJ     = ' '
      DO 10 I = 1,NE
	INUE(I,1) = 0
	INUE(I,2) = 0
	INUE(I,3) = 0
      IOLD(I) = 0
	NDOWN(I) = 0
	NORDER(I) = 0
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
c      IF(JCE.EQ.0.AND.NUE(I,J).LE.0)   GO TO 25
c      IF(JCE.EQ.1.AND.KUE(I,J).EQ.' ') GO TO 25
      L       = INUE(I,J)
      IF(L.EQ.NEP1) GO TO 25
      IF (IOLD(L)) 18,18,25
   25 CONTINUE
      IOLD(I) = 1
      JR(N)   = I
   30 CONTINUE
C  SOMEHOW CHECK FOR A LOOP IN THE NETWORK
C     
      DO 350 N=1,NE
	L = JR(N)
	IF (L.EQ.0) THEN
       SCREWED = .TRUE.
	WRITE(N6,6080) N
	ENDIF
	I = 0
  310	I = I + 1
	M = JR(I)
	IF (M.EQ.L) GO TO 350
	DO 320 J = 1,3
	IF (INUE(M,J).EQ.L) THEN
       SCREWED = .TRUE.
	IF (JCE.EQ.0) THEN
	WRITE(N6,7010) NOE(M)
	ELSE
	WRITE(N6,7020) KOE(M)
	ENDIF 
	ENDIF
  320 CONTINUE
      GO TO 310
  350 CONTINUE
C=======================================================================
      WRITE(N6,901) NEP1
      WRITE(N6,905)
      DO 50 I = 1,NE
      L       = JR(I)
      NTPE      = NTYPE(I)
	IF (L.NE.0) THEN
      IF(JCE.EQ.0) THEN
	WRITE(N6,906) NOE(I),I,NTPE,NAME(NTPE),
     +             (NUE(I,J),J=1,3),I,NOE(L),L,(INUE(L,J),J=1,3)
	ELSE
      WRITE(N6,916) KOE(I),I,NTPE,NAME(NTPE),
     +             (KUE(I,J),J=1,3),I,KOE(L),L,(INUE(L,J),J=1,3)
	ENDIF
	ELSE
      IF(JCE.EQ.0) THEN
	WRITE(N6,906) NOE(I),I,NTPE,NAME(NTPE),
     +             (NUE(I,J),J=1,3),I,0,0,0,0,0
	ELSE
      WRITE(N6,916) KOE(I),I,NTPE,NAME(NTPE),
     +             (KUE(I,J),J=1,3),I,' ',0,0,0,0
	ENDIF
	ENDIF
   50 CONTINUE
C=======================================================================
      DO 100 I  = 1,NE
      L         = JR(I)
      NORDER(I) = 0
      DO 200 J  = 1,NE
      M         = JR(J)
      DO 200 K  = 1,3
c      IF(JCE.EQ.0.AND.NUE(M,K).EQ.NOE(L)) GO TO 100
c      IF(JCE.EQ.1.AND.KUE(M,K).EQ.KOE(L)) GO TO 100
	IF(INUE(M,K).EQ.L) GO TO 100
  200 CONTINUE
      NORDER(I) = 1
  100 CONTINUE
CIM   ADD A CHECK FOR ELEMENTS THAT APPEAR MORE THAN ONCE IN NUE KUE
CIM   ARRAYS.  SHOULD HAPPEN ONLY OF NTPE OF ELEMENT IS 21, 23, 24, 26
      DO 115 N = 1,NE
      DO 115 J = 1,3
	NDOWN(INUE(N,J)) = NDOWN(INUE(N,J))+1
  115 CONTINUE
      SCREWED = .FALSE.
	DO 120 N = 1,NE
	   IF (NDOWN(N).EQ.0) THEN
c     NORDER(N) = 1
	   ELSE
	     IF(NDOWN(N).EQ.1) GO TO 120
	     IF(NDOWN(N).EQ.2) THEN
	     ISSPLIT = .FALSE.
	      IF(NTYPE(n).EQ.21) ISSPLIT = .TRUE.
CWCH, 9/27/99. STORAGE UNIT IS ALSO A FLOW SPLITTER.
            IF(NTYPE(n).EQ.22) ISSPLIT = .TRUE.
	      IF(NTYPE(n).EQ.23) ISSPLIT = .TRUE.
	      IF(NTYPE(n).EQ.24) ISSPLIT = .TRUE.
	      IF(NTYPE(n).EQ.26) ISSPLIT = .TRUE.
	      IF(.NOT.ISSPLIT) THEN
             SCREWED = .TRUE.
	       IF(JCE.EQ.0) THEN
	         WRITE(N6,6050) NOE(n),NDOWN(n)
             ELSE
	         WRITE(N6,6060) KOE(n),NDOWN(n)
             ENDIF
	       ENDIF
	      ELSE
CIM HERE IF NDOWN IS LARGER THAN 2
           SCREWED = .TRUE.
	       IF(JCE.EQ.0) THEN
	         WRITE(N6,6030) NOE(n),NDOWN(n)
             ELSE
	         WRITE(N6,6040) KOE(n),NDOWN(n)
             ENDIF
	     ENDIF
         ENDIF
  120 CONTINUE
CIM   ADD TABLE THAT LIST ALL DOWNSTREAM ELEMENTS
CIM   NORDER = 1
      WRITE(N6,6000)
      DO I = 1,NE
	IF (NORDER(I).EQ.1) THEN
	M = JR(I)
	IF (JCE.EQ.0) THEN
	      WRITE(N6,6010) M,NOE(M)
	ELSE
		  WRITE(N6,6020) M,KOE(M)
	ENDIF
	ENDIF
	ENDDO
c	do N = 1,ne
c	WRITE(n6,*) N,JR(N),NDOWN(N)
c	enddo
CIM   ONE MORE CHECK TO SEE
       IF (SCREWED) STOP ' ERROR IN MODEL CONNECTIVITY '
      RETURN
C=======================================================================
CIM REVISE FORMATS TO PRINT 10 CHARACTER AND DIGIT IDS
  901 FORMAT(1H1,/,
     1' ****************************************************',/,
     2' *     ELEMENT LINKAGES AND COMPUTATION SEQUENCE    *',/,
     3' ****************************************************',/,
     4' * NOTE: ELEMENT # 0 IS GIVEN AN INTERNAL # OF ',I4,' *',/,
     5' ****************************************************',/)
  905 FORMAT(3X,
     1' EXTERNAL INTERNAL',35X,'UPSTREAM ELEMENTS',3X,
     1'ORDER OF COMPUTATIONS AT EACH TIME STEP (PROCEEDING DOWNSTREAM)'
     2,/,3X,' ELEMENT  ELEMENT',36X,'(EXTERNAL NOS.)',7X,'COMPUTATION',
     3'  EXTERNAL INTERNAL  INTERNAL UPSTREAM',/,3X,' NUMBER   NUMBER',
     4'   TYPE  DISCRIPTION',12X,' 1         2         3',7X,
     5'SEQUENCE    NUMBER   NUMBER   ELEMENT  NUMBERS',/,3X,
     5' ------   ------   ----  -----------',7X,
     5'----------------------------',
     56X,'--------    ------   ------   ----------------')
  906 FORMAT(1X,I10,3X,I4,I7,3X,A16,3I10,I11,3X,I10,I9,1X,3I6)
  916 FORMAT(1X,A10,3X,I4,I7,3X,A16,3A10,I11,3X,A10,I9,1X,3I6)
 6000	FORMAT(1H1,/,
     1' ****************************************************',/,
     2' *     DOWNSTREAM ELEMENTS (MODEL OUTLETS)          *',/,
     3' ****************************************************',/)
 6010	FORMAT(10X,I10,I10)
 6020 FORMAT(10X,I10,A10)
 6030 FORMAT('  ERROR - ELEMENT ',I10,' APPEARS ',I3,
     1' TIMES AS AN UPSTREAM ELEMENT')
 6040 FORMAT('  ERROR - ELEMENT ',A10,' APPEARS ',I3,
     1' TIMES AS AN UPSTREAM ELEMENT')
 6050 FORMAT('  ERROR - ELEMENT ',I10,' APPEARS ',I3,
     1' TIMES AS AN UPSTREAM ELEMENT',
     2' AND IS NOT A FLOW SPLIT ELEMENT')
 6060 FORMAT('  ERROR - ELEMENT ',A10,' APPEARS ',I3,
     1' TIMES AS AN UPSTREAM ELEMENT',
     2' AND IS NOT A FLOW SPLIT ELEMENT')
 6080 FORMAT('  ERROR - COMPUTATION ORDER FOR A ELEMENT IS ZERO',/
     1,10X,'THIS MEANS THAT A CORRECT COMPUTATIONAL ORDER WAS NOT FOUND'
     2,/,10X,'CHECK INPUT STREAM FOR LOOPS OR OTHER PROBLEMS',/,
     310X,'COMPUTATIONAL ORDER OF ELEMENT = ',I10)
 7010 FORMAT('  ERROR - ELEMENT ',I10,' FOUND UPSTREAM FROM ITSELF',
     1' AT ELEMENT ',I10,/,10X,'CHECK COMPUTATIONAL ORDER')
 7020 FORMAT('  ERROR - ELEMENT ',A10,' FOUND UPSTREAM FROM ITSELF',
     1' AT ELEMENT ',A10,/,10X,'CHECK COMPUTATIONAL ORDER')
     2  
C=======================================================================
      END
