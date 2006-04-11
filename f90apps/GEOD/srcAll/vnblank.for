*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vnblank.f,v 1.1 1998/07/07 20:10:38 grogers Exp $
*  vnblank.f
*
      SUBROUTINE vNBLANK (A, IFLAG, NBLK)
*
*  Purpose:  Return position of last non-blank in string (IFLAG = 2)
*** or position of first non-blank in string (IFLAG = 1)
********************************************************************
*
*  $Log: vnblank.f,v $
*  Revision 1.1  1998/07/07 20:10:38  grogers
*  PR#0, initial add of vertcon_lib
*
*
      INTEGER IFLAG, NBLK, LENG, IBLK
      CHARACTER*(*) A
      LENG = LEN(A)
      IF (IFLAG .EQ. 2) THEN
        DO 1 IBLK = LENG, 1, -1
          IF ( A(IBLK:IBLK) .NE. ' ' ) THEN
            NBLK = IBLK
            RETURN
          ENDIF
    1   CONTINUE
      ELSEIF (IFLAG .EQ. 1) THEN
        DO 2 IBLK = 1, LENG, +1
          IF ( A(IBLK:IBLK) .NE. ' ' ) THEN
            NBLK = IBLK
            RETURN
          ENDIF
    2   CONTINUE
      ENDIF
* String contains all blanks
      NBLK = 0
      RETURN
      END
