      SUBROUTINE CHRLNB (CLINE,ILAST)                                   MLlg
C
C     FINDS THE LAST NON-BLANK CHARACTER IN CHARACTER ARRAY CLINE
C
      CHARACTER CLINE*(*)                                               MLlg
C
C     ILEN = LEN(CLINE)                                                 M
C     ILAST = NINDXC ( CLINE, ILEN, 1, ' ')                             M
C
      ILAST = NINDXR (CLINE, ' ')                                       Llg
C
      RETURN                                                            MLlg
      END                                                               MLlg
