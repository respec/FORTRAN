CIM CHANGE I TO ICASE TO MAKE IT CLEARER TO FOLLOW
      FUNCTION HTIDES(ICASE,T)
C	EXTRAN BLOCK
C     CALLED BY BOUND NEAR LINES 89, 554, 600
C=======================================================================
C     CALCULATES TIDAL HEIGHT BASED ON SEVEN TIDAL COEFFICEINTS
C=======================================================================
C CORRECTION BY WCH, 8/28/92
CIMTIDE
CIM  this subroutine dangerously uses htide array in incorrect way.
cim  HTIDE is dimensioned to number of extran elements.
cim  in this subrountine htide(icase) is used???
cim  change htide to chtide in this routine, is passed as 
cim  result of htides function to calling routine  3/97
CIM  change HTIDE(ICASE) to CHTIDE
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
cimtide  use new function tidefile to compute tide elevation from
cimtide  file input and return
cimtide
      if (icase.gt.10000) then
      htides = tidefile(icase)
      return
      endif
cimtide
      WW  =  W(ICASE)/3600.0
      THR =  T/3600.0
C=======================================================================
C     HTIDE IS A FUNCTION OF SINE AND COSINE TERMS FOR NTIDE(ICASE) LE 4
C     HTIDE IS AN INTERPOLATED FUNCTION OF TIME IF NTIDE(ICASE) EQ 5
C=======================================================================
      IF(NTIDE(ICASE).LE.4) THEN
                        TLAG     = T - PHLAGS(ICASE)
cim change htide to chtide here
                        CHTIDE = A1(ICASE)                  +
     +                             A2(ICASE)*DSIN(WW*TLAG)     +
     +                             A3(ICASE)*DSIN(2.0*WW*TLAG) +
     +                             A4(ICASE)*DSIN(3.0*WW*TLAG) +
     +                             A5(ICASE)*DCOS(WW*TLAG)     +
     +                             A6(ICASE)*DCOS(2.0*WW*TLAG) +
     +                             A7(ICASE)*DCOS(3.0*WW*TLAG)
                        ELSE
                        IF(THR.LT.STIDE(ICASE,1,1)) THEN
                                          NERR2    = NERR2 + 1
                                          IF(NERR2.EQ.1)
     +                                             WRITE(N6,2001) ICASE
C#######################################################################
C CORRECT MIDDLE SUBSCRIPT OF STIDE FROM 1 TO 2, 8/28/92
C CORRECTION NOTED BY V. ADDERLEY, CH2M-HILL
C IN GENERAL, MIDDLE INDEX=1 ==> TIME
C             MIDDLE INDEX=2 ==> CORRESPONDING STAGE
C#######################################################################
cim change htide(icase) to chtide in next line
                                        CHTIDE = STIDE(ICASE,2,1)
                                          GO TO 1001
                                          ENDIF
                        JJ        = NUMTID(ICASE)
                        IF(THR.GT.STIDE(ICASE,1,JJ)) THEN
cim change htide(icase) to chtide in next line
                                       CHTIDE = STIDE(ICASE,2,JJ)
                                          NERR1    = NERR1 + 1
                                          IF(NERR1.EQ.1)
     +                                             WRITE(N6,2002) ICASE
                                          GO TO 1001
                                          ENDIF
                        DO 1000 J = 1,JJ-1
                        J1        = J + 1
                        IF(THR.GE.STIDE(ICASE,1,J).
     +                       AND.THR.LE.STIDE(ICASE,1,J1)) THEN
                             DIFF = STIDE(ICASE,2,J1) - STIDE(ICASE,2,J)
                             SX   = STIDE(ICASE,1,J1) - STIDE(ICASE,1,J)
cim change htide(icase) to chtide in next line
                                 CHTIDE = STIDE(ICASE,2,J) +
     +                                    DIFF*(THR-STIDE(ICASE,1,J))/SX
                                 GO TO 1001
                                 ENDIF
 1000                   CONTINUE
cim change htide(icase) to chtide in next line
                        CHTIDE = STIDE(ICASE,2,JJ)
 1001                   CONTINUE
                        ENDIF
cim change htide(icase) to chtide in next line
      HTIDES = CHTIDE
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