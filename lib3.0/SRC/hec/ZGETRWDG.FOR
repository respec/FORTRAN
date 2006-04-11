      SUBROUTINE ZGETRW (IADD, IREC, IWRD)
C
C     Get record IREC, word IWRD from the word address IADD
C
C     Written by Bill Charley at HEC, 1988.
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
C     INTEGER*6 IADD, ITEMP1, ITEMP2, ITEMP3                            H
      INTEGER*4 IADD, ITEMP1, ITEMP2, ITEMP3, IREC                      ML
C
C
C     Must to arithmetic in large integer words!
      ITEMP3 = NBSIZE                                                   HML
      ITEMP1 = ((IADD - 1) / ITEMP3)                                    HML
      IREC = INT(ITEMP1) + 1                                            H
C     IREC = INT4(ITEMP1) + 1                                           ML
      ITEMP2 = IADD - (ITEMP1 * ITEMP3)                                 HML
      IWRD = INT(ITEMP2)                                                HML
C
      IREC = ((IADD - 1) / NBSIZE) + 1                                  u
      IWRD = IADD - ((IREC - 1) * NBSIZE)                               u
C
      RETURN
      END
