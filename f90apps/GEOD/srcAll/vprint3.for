*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vprint3.f,v 1.2 1998/07/08 22:14:00 grogers Exp $
*  vprint3.f
*
      SUBROUTINE vPRINT3 (LU, NCONV, VRSION, GHT)
*
* Purpose: This subroutine prints out the actual transformed results
********************************************************************
*
*  $Log: vprint3.f,v $
*  Revision 1.2  1998/07/08 22:14:00  grogers
*  PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
*
c Revision 1.1  1998/07/07  20:10:40  grogers
c PR#0, initial add of vertcon_lib
c
*
* This subroutine prints out the actual transformed results using
* NGS Blue Book File format  (same as the input file format)
* This is used for type 3 format.
      LOGICAL TONGVD
      REAL VRSION, GHT, GHTX
      INTEGER LU, NCONV
      CHARACTER*2  CODE,METER
      CHARACTER*80 CARD
      CHARACTER*96 B96 
      COMMON /vCURNT/ B96
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, TONGVD, NSPACE
      EQUIVALENCE(CARD,B96)
      DATA METER/'MT'/
* Write header record to identify source of correction and value
      IF (NCONV .EQ. 1) THEN
        WRITE (LU, 10) VRSION
   10   FORMAT ('VERTCON  Version', F4.1)
      ENDIF
* In this format, the variable CARD contains the image of the input
* card.  This is written to the output file instead of using the
* latitude, longitude, and name variables.  The elevation is
* overwritten by the corrected value
*
      READ(B96,'(49x,A2,F10.5)',IOSTAT=IOS,ERR=101) CODE,HT
          GHTX=GHT
       IF(CODE.NE.METER) GHTX=DBLE(GHT)/0.3048D0
      IF(TONGVD) THEN
        HT=HT+GHTX
      ELSE
        HT=HT-GHTX
      ENDIF
       HT=vROUND(HT)
*
      WRITE (LU,100,IOSTAT=IOS,ERR=102) B96(1:51), HT, B96(62:80)
  100 FORMAT (A51, F10.5, A19)
      RETURN
  101 WRITE(LU,*) 'NGS Blue Book format elevation decode error -'
       GO TO 103
  102 WRITE(LU,*) 'NGS Blue Book format elevation encode error -'
  103  WRITE(LU,'(A80)') CARD
       STOP 
      END
