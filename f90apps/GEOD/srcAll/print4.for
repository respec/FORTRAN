*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/print4.f,v 1.2 1998/07/08 22:13:58 grogers Exp $
*  print4.f
*
      SUBROUTINE PRINT4 (LU, NCONV, VRSION, GHT)
*
* Purpose: Prints out the actual transformed results
****************************************************
*
*  $Log: print4.f,v $
*  Revision 1.2  1998/07/08 22:13:58  grogers
*  PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
*
c Revision 1.1  1998/07/07  20:10:26  grogers
c PR#0, initial add of vertcon_lib
c
*
* Prints out the actual transformed results using
* NGS Internal Benchmark File format  (same as the input file format)
* This is used for type 4 format.
*
      LOGICAL TONGVD
      REAL VRSION, GHT
      INTEGER LU, NCONV
      CHARACTER*2 NAVD,NGVD,CODE
      CHARACTER*80 CARD
      CHARACTER*96 B96 
      COMMON /vCURNT/ B96
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, TONGVD, NSPACE
       SAVE NAVD,NGVD
      EQUIVALENCE(CARD,B96)
      DATA NAVD/'88'/,NGVD/'29'/
* Write header record to identify source of correction and value
      IF (NCONV .EQ. 1) THEN
        WRITE (LU, 10) VRSION
   10   FORMAT ('VERTCON  Version', F4.1)
      ENDIF
* In this format, the variable CARD contains the image of the input
* card.  This is written to the output file instead of using the
* latitude, longitude, and name variables.  The approximate
* elevation is overwritten by the corrected walue when applicable
*
*     WRITE(NOUT,'(A96,f5.2)') B96,GHT 
      READ(B96,'(64x,F10.5,20x,A2)',IOSTAT=IOS,ERR=101) HT,CODE
      IF(TONGVD) THEN
        IF((CODE.EQ.'  ').OR.(CODE.EQ.NGVD)) THEN
         B96(95:96)=NAVD
         HT=HT+GHT
        ENDIF
       ELSE
        IF(CODE.EQ.NAVD) THEN
         B96(95:96)=NGVD
         HT=HT-GHT
        ENDIF
      ENDIF
       HT=vROUND(HT)
*
      WRITE (LU,100,IOSTAT=IOS,ERR=102) B96(1:64), HT, B96(75:96)
  100 FORMAT (A64, F10.5, A22)
      RETURN
  101 WRITE(LU,*) 'NGS Internal BM format elevation decode error'
       GO TO 103
  102 WRITE(LU,*) 'NGS Internal BM format elevation encode error'
  103  WRITE(LU,'(A80)') CARD
       STOP 
      END
