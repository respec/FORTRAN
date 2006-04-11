      SUBROUTINE ASCOUT(IENTER)
C	EXTRAN BLOCK
      INCLUDE 'TAPES.INC'
      INCLUDE 'ASCOUT1.INC'
C=======================================================================
C     OUTPUT SWMM EXTRAN RESULTS TO DETAILED ASCII INTERFACE FILE 
C
C     CREATED 9/11/97 by Charles I. Moore
C                        Camp Dresser & McKee
C                        Annandale, Va.
C
C     This initial version writes files in DETAILED ASCII format 
C
C=======================================================================
C  IF IENTER = 1 then write the header information
C     IENTER = 2 then write the result record for this time step
C     IENTER = 3 check that last result is complete and pad if necessary
C
      SELECT CASE (IENTER)
      CASE (1)
      ICOUNT = 20
               CALL ASCHEADA
               CALL ASCRESA
      CASE (2)
               CALL ASCRESA
      CASE (3) 
               CALL ASCRESF
      END SELECT
      RETURN 
      END

