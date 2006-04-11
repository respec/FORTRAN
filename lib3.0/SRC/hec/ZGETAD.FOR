      SUBROUTINE ZGETAD (IADD, IREC, IWRD)
C
C     Get address IADD from recrod IREC, word IWRD
C
C     Written by Bill Charley at HEC, 1989.
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
C     INTEGER*6 IADD, ITEMP1, ITEMP2, ITEMP3                            H
      INTEGER*4 IADD, ITEMP1, ITEMP2, ITEMP3, IREC                      ML
C
C
C     IADD = ((IREC - 1) * NBSIZE) + IWRD                               u
C
C     Must do arithmetic in large integer words!
      ITEMP1 = NBSIZE                                                   HML
      ITEMP2 = IREC                                                     HML
      ITEMP3 = IWRD                                                     HML
      IADD = ((ITEMP2 - 1) * ITEMP1) + IWRD                             HML
C
      RETURN
      END
