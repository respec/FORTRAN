      SUBROUTINE MVBITS(IW,IB,NB,JW,JB)
C
C     ****************************************************************
C     *                                                              *
C     *   SUBROUTINE: MVBITS  (MOVE BITS)                            *
C     *   AUTHOR: DANIEL EDWARDS                                     *
C     *   DATE: 7 JULY 1986                                          *
C     *                                                              *
C     *   AS A MIL-STD-1753 STANDARD EXTENSION TO THE ANSI 77 STAN-  *
C     *   DARD, THE BIT MOVE SUBROUTINE MOVES NB BITS FROM POSITIONS *
C     *   IB THROUGH IB+NB-1 OF ARGUMENT IW TO POSITIONS JB THROUGH  *
C     *   JB+NB-1 OF ARGUMENT JW.  THE PORTION OF ARGUMENT JW NOT    *
C     *   AFFECTED BY THE MOVEMENT OF BITS REMAINS UNCHANGED.  ALL   *
C     *   ARGUMENTS ARE INTEGER EXPRESSIONS, EXCEPT JW, WHICH MUST   *
C     *   BE A VARIABLE OR ARRAY ELEMENT.  ARGUMENTS IW AND JW ARE   *
C     *   PERMITTED TO BE THE SAME NUMERIC STORAGE UNIT.  THE VALUES *
C     *   OF IB+NB AND JB+NB MUST BE LESS THAN OR EQUAL TO THE       *
C     *   LENGTHS OF IW AND JW RESPECTIVELY.                         *
C     *                                                              *
C     *   IN SUMMARY:                                                *
C     *      IW ===> WORD WHICH CONTAINS THE BITS TO BE COPIED OR    *
C     *              MOVED (IW REMAINS UNCHANGED).                   *
C     *      IB ===> LOCATION OF BEGINNING BIT IN IW TO BE MOVED.    *
C     *      NB ===> NUMBER OF BITS TO MOVE.                         *
C     *      JW ===> WORD WHICH WILL RECEIVE THE MOVED BITS FROM IW  *
C     *              (ALL OTHER BITS REMAIN UNCHANGED).              *
C     *      JB ===> LOCATION OF BEGINNING BIT IN JW WHERE THE BITS  *
C     *              WILL BE RECIVED.                                *
C     *                                                              *
C     ****************************************************************
C
C     CLEAR IMASK BY SETTING ALL BITS EQUAL TO 0.
C
      IMASK = 0
C
C     CREATE WINDOW IN MASK CORRESPONDING TO IB THROUGH IB+NB-1
C     BY SETTING THOSE BITS TO 1.
C
      DO 10 KINDEX = 1, NB
         ILOC = IB + KINDEX - 1
         IMASK = IBSET( IMASK, ILOC )
 10   CONTINUE
C
C     PUT BITS TO BE MOVED INTO THE WINDOW OF IMASK BY PERFORMING
C     A LOGICAL 'AND' BETWEEN IMASK AND IW.
C
      IMASK = IAND( IMASK, IW )
C
C     SHIFT WINDOW OF IMASK CONTAINING IMAGE SO THAT IT CORRESPONDS
C     TO JB THROUGH JB+NB-1 IN JW.
C
      IMASK = ISHFT( IMASK, (JB-IB) )
C
C     BLANK OUT SOME SPACE WITH 0 BITS IN JW CORRESPONDING TO
C     JB THROUGH JB+NB-1.
C
      DO 20 KINDEX = 1, NB
         ILOC = JB + KINDEX - 1
         JW = IBCLR( JW, ILOC )
 20   CONTINUE
C
C     COMBINE IMASK AND JW SO THE INFORMATION IN IMASK SLIDES INTO
C     THE SPACE IN JW WITHOUT TOUCHING THE REST OF JW.
C
      JW = IOR( JW, IMASK )
C
      RETURN
      END
