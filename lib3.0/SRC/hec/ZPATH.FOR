      SUBROUTINE ZPATH (CA, CB, CC, CD, CE, CF, CPATH, NPATH)
C
C     ZPATH forms a pathname from the input pathname parts.
C     Pathnames longer than 80 characters are truncated before
C     being returned but always contain the necessary seven forward
C     slashes
C
C     CA, CB, CC, CD, CE, CF - Input character strings
C     CPATH -     Pathname character string returned
C     NPATH -     Length of pathname string, also used as an indexer
C
C     Written by John Miwa at HEC, 1988.
C
C
C     Declare variables
         CHARACTER CA*(*),CB*(*),CC*(*),CD*(*),CE*(*),CF*(*)
         CHARACTER CPART(6)*32, CPATH*(*)
         INTEGER IBEG(6), IEND(6)
C
C
C
C     Fill pathname string with blanks
      CPATH = ' '
C
C     Get the beginning and ending positions of each pathname part
      CALL CHRFLB (CA, IBEG(1), IEND(1))
      CALL CHRFLB (CB, IBEG(2), IEND(2))
      CALL CHRFLB (CC, IBEG(3), IEND(3))
      CALL CHRFLB (CD, IBEG(4), IEND(4))
      CALL CHRFLB (CE, IBEG(5), IEND(5))
      CALL CHRFLB (CF, IBEG(6), IEND(6))
C
C     Fill dummy character array
      CPART(1) = CA
      CPART(2) = CB
      CPART(3) = CC
      CPART(4) = CD
      CPART(5) = CE
      CPART(6) = CF
C
C
      NPATH = 1
      DO 40 I=1,6
C
C     Calculate string lengths
      IF (IEND(I).EQ.0) THEN
      ILEN = 0
      ELSE
      ILEN = IEND(I) - IBEG(I) + 1
      END IF
C
C     Place Slash in path
      CPATH (NPATH:NPATH) = '/'
C
      IF ((NPATH+ILEN).LT.80) THEN
C
C     Add pathname part to pathname
      IF (ILEN.NE.0) CPATH(NPATH+1:) = CPART(I)(IBEG(I):IEND(I))
C
C     Compute new pathname length
      NPATH = NPATH + ILEN + 1
C
      ELSE
C
C     Add last part of pathname
      JLEN = LEN(CPATH)
      IF (JLEN.GT.80) JLEN = 80
      CPATH(NPATH+1:JLEN) = CPART(I)(IBEG(I):)
C     Add ending slashmarks
      DO 20 J=1,7-I
      CPATH(JLEN:JLEN) = '/'
      JLEN = JLEN - 1
 20   CONTINUE
      NPATH = 80
      GO TO 60
      END IF
C
 40   CONTINUE
C
C     Add final slashmark
      CPATH(NPATH:NPATH)= '/'
C
 60   CONTINUE
      RETURN
C
      END
