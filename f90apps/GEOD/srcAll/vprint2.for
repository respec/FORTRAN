*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vprint2.f,v 1.1 1998/07/07 20:10:39 grogers Exp $
*  vprint2.f
*
      SUBROUTINE vPRINT2 (LU, NCONV, VRSION, GHT)
*
* Purpose: This subroutine prints out the actual transformation results
***********************************************************************
*
*  $Log: vprint2.f,v $
*  Revision 1.1  1998/07/07 20:10:39  grogers
*  PR#0, initial add of vertcon_lib
*
*
* This subroutine prints out the actual transformation results using
* a free format - the same as the input file format.  This is used
* for type 2 format.
      REAL VRSION, GHT
      INTEGER LU, NCONV
      CHARACTER*80 CARD
      CHARACTER*96 B96 
      COMMON /vCURNT/ B96
      EQUIVALENCE(CARD,B96)
* Write header record to identify source of correction and value
      IF (NCONV .EQ. 1) THEN
        WRITE (LU, 10) VRSION
   10   FORMAT ('VERTCON  Version', F4.1)
      ENDIF
* In this format, the variable CARD contains the image of the input
* card.  This is written to the output file instead of using the
* latitude, longitude, and name variables.  The correction
* overwrites whatever is in columns 33-40
*      write(6,'(a80)') CARD
      WRITE (LU,100) CARD(1:32), GHT, CARD(41:80)
  100 FORMAT (A32, F7.3, 1x, A40)
      RETURN
      END
