      FUNCTION HTIDES(I,T)
C=======================================================================
C     CALCULATES TIDAL HEIGHT BASED ON SEVEN TIDAL COEFFICEINTS
C=======================================================================
C CORRECTION BY WCH, 8/28/92
C#######################################################################
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'CONTR.INC'
C=======================================================================
C WCH MODIFICATION, 11/1/91: DOUBLE PRECISION TLAG,WW
C=======================================================================
      DOUBLE PRECISION TLAG,WW
      DATA NERR1/0/,NERR2/0/
C=======================================================================
      WW  =  W(I)/3600.0
      THR =  T/3600.0
C=======================================================================
C     HTIDE IS A FUNCTION OF SINE AND COSINE TERMS FOR NTIDE(I) LE 4
C     HTIDE IS AN INTERPOLATED FUNCTION OF TIME IF NTIDE(I) EQ 5
C=======================================================================
      IF(NTIDE(I).LE.4) THEN
                        TLAG     = T - PHLAGS(I)
                        HTIDE(I) = A1(I)                  +
     +                             A2(I)*DSIN(WW*TLAG)     +
     +                             A3(I)*DSIN(2.0*WW*TLAG) +
     +                             A4(I)*DSIN(3.0*WW*TLAG) +
     +                             A5(I)*DCOS(WW*TLAG)     +
     +                             A6(I)*DCOS(2.0*WW*TLAG) +
     +                             A7(I)*DCOS(3.0*WW*TLAG)
                        ELSE
                        IF(THR.LT.STIDE(I,1,1)) THEN
                                          NERR2    = NERR2 + 1
                                          IF(NERR2.EQ.1)
     +                                             WRITE(N6,2001) I
C#######################################################################
C CORRECT MIDDLE SUBSCRIPT OF STIDE FROM 1 TO 2, 8/28/92
C CORRECTION NOTED BY V. ADDERLEY, CH2M-HILL
C IN GENERAL, MIDDLE INDEX=1 ==> TIME
C             MIDDLE INDEX=2 ==> CORRESPONDING STAGE
C#######################################################################
                                          HTIDE(I) = STIDE(I,2,1)
                                          GO TO 1001
                                          ENDIF
                        JJ        = NUMTID(I)
                        IF(THR.GT.STIDE(I,1,JJ)) THEN
                                          HTIDE(I) = STIDE(I,2,JJ)
                                          NERR1    = NERR1 + 1
                                          IF(NERR1.EQ.1)
     +                                             WRITE(N6,2002) I
                                          GO TO 1001
                                          ENDIF
                        DO 1000 J = 1,JJ-1
                        J1        = J + 1
                        IF(THR.GE.STIDE(I,1,J).
     +                           AND.THR.LE.STIDE(I,1,J1)) THEN
                                 DIFF     = STIDE(I,2,J1) - STIDE(I,2,J)
                                 SX       = STIDE(I,1,J1) - STIDE(I,1,J)
                                 HTIDE(I) = STIDE(I,2,J) +
     +                                      DIFF*(THR-STIDE(I,1,J))/SX
                                 GO TO 1001
                                 ENDIF
 1000                   CONTINUE
                        HTIDE(I) = STIDE(I,2,JJ)
 1001                   CONTINUE
                        ENDIF
      HTIDES = HTIDE(I)
      RETURN
C=======================================================================
 2001 FORMAT(/,' WARNING !! SIMULATION STARTS BEFORE TIME HISTORY',/,
     +         ' STAGE BEGINS FOR TIDAL BOUNDARY CONDITION ',I6,/,
     +         ' PROGRAM DEFAULTS TO THE FIRST STAGE VALUE.')
 2002 FORMAT(/,' WARNING !! SIMULATION CONTINUES AFTER THE TIME',/,
     +         ' HISTORY STAGE ENDS FOR TIDAL BOUNDARY CONDITION ',I6,/,
     +         ' PROGRAM DEFAULTS TO THE LAST STAGE VALUE.')
C=======================================================================
      END
