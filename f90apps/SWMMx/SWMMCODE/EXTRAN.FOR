      SUBROUTINE EXTRAN
C=======================================================================
C        EXTENDED TRANSPORT MODEL LAST UPDATED NOVEMBER, 1990
C                             BY
C                 CAMP DRESSER AND MCKEE, INC.
C                      LARRY A. ROESNER
C                     ROBERT P. SHUBINSKI
C                       JOHN A. ALDRICH
C
C                             AND
C                     UNIVERSITY OF FLORIDA
C                       ROBERT E. DICKINSON
C                        WAYNE C. HUBER
C=======================================================================
C     ADD NO-QUOTE OPTION, WCH, 11/29/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
C=======================================================================
C     Set up transfer files.
C=======================================================================
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
      LAST   = JIN(INCNT)
      NEXT   = JOUT(IOUTCT)
      WRITE(*,100)
      WRITE(N6,100)
C#### WCH, 11/39/93.  ADD NO-QUOTE OPTION TO EXTRAN.
      IF(NOQ.EQ.0) THEN
           WRITE(*,102)
           WRITE(N6,102)
           ELSE
           WRITE(*,101)
           WRITE(N6,101)
           ENDIF
C=======================================================================
C     Open all scratch files for the Extran Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') OPEN(NSCRAT(1),
     +             FILE=FFNAME(51),FORM='FORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(2).GT.0.AND.FFNAME(52).NE.'SCRT2.UF') OPEN(NSCRAT(2),
     +             FILE=FFNAME(52),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(3).GT.0.AND.FFNAME(53).NE.'SCRT3.UF') OPEN(NSCRAT(3),
     +             FILE=FFNAME(53),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(4).GT.0.AND.FFNAME(54).NE.'SCRT4.UF') OPEN(NSCRAT(4),
     +             FILE=FFNAME(54),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(5).GT.0.AND.FFNAME(55).NE.'SCRT5.UF') OPEN(NSCRAT(5),
     +             FILE=FFNAME(55),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(6).GT.0.AND.FFNAME(56).NE.'SCRT6.UF') OPEN(NSCRAT(6),
     +             FILE=FFNAME(56),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(7).GT.0.AND.FFNAME(57).NE.'SCRT7.UF') OPEN(NSCRAT(7),
     +             FILE=FFNAME(57),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF') OPEN(NSCRAT(1),
     +             FORM='FORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(2).GT.0.AND.FFNAME(52).EQ.'SCRT2.UF') OPEN(NSCRAT(2),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(3).GT.0.AND.FFNAME(53).EQ.'SCRT3.UF') OPEN(NSCRAT(3),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(4).GT.0.AND.FFNAME(54).EQ.'SCRT4.UF') OPEN(NSCRAT(4),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(5).GT.0.AND.FFNAME(55).EQ.'SCRT5.UF') OPEN(NSCRAT(5),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(6).GT.0.AND.FFNAME(56).EQ.'SCRT6.UF') OPEN(NSCRAT(6),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(NSCRAT(7).GT.0.AND.FFNAME(57).EQ.'SCRT7.UF') OPEN(NSCRAT(7),
     +             FORM='UNFORMATTED',STATUS='SCRATCH')
C=======================================================================
C     INITIALIZATION
C=======================================================================
      DO 5    N    = 1,NEE
      ICHECK(N)    = IND(1)
      JCHECK(N)    = IND(1)
      PMAX(N,1)    = 0.0
      PMAX(N,2)    = 0.0
      JSKIP(N)     = 0
      INGATE(N)    = 0
      YTOT(N)      = 0.0
      QTOT(N)      = 0.0
      YDEV(N)      = 0.0
      QDEV(N)      = 0.0
      Y(N)         = 0.0
      YT(N)        = 0.0
      YO(N)        = 0.0
      A(N)         = 0.0
      AT(N)        = 0.0
      V(N)         = 0.0
      VT(N)        = 0.0
      Q(N)         = 0.0
      QO(N)        = 0.0
      QT(N)        = 0.0
      AS(N)        = 0.0
      ASFULL(N)    = 0.0
      WIDE(N)      = 0.0
      AMAX(N)      = 0.0
      HMAX(N)      = 0.0
      AFULL(N)     = 0.0
      RFULL(N)     = 0.0
      H(N,1)       = 0.0
      H(N,2)       = 0.0
      ZD(N)        = 0.0
      ZU(N)        = 0.0
      DO 5    M    = 1,8
    5 NCHAN(N,M)   = 0
      DO 6    M    = 1,NVORF
      DO 6    N    = 1,NVOTIM
      VORIF(M,N,1) = 999999.0
      VORIF(M,N,2) = 0.0
      VORIF(M,N,3) = 0.0
    6 CONTINUE
      DO 900 N   = 1,NTL
      VMAXX(N)   = 0.0
      QMAXX(N)   = 0.0
      IVHR(N)    = 0
      IVMIN(N)   = 0
      IQHR(N)    = 0
      IQMIN(N)   = 0
  900 CONTINUE
      SUMQIN     = 0.0
      VLEFT      = 0.0
      VINIT      = 0.0
      DO 950 J   = 1,NJ
      JCHECK(J)  = IND(1)
      SUMQ(J)    = 0.0
      SUMQS(J)   = 0.0
      SUMAL(J)   = 0.0
      SURLEN(J)  = 0.0
      DEPMAX(J)  = 0.0
      QOU(J)     = 0.0
      FLDLEN(J)  = 0.0
      ASMAXX(J)  = 0.0
      IDHR(J)    = 0
      IDMIN(J)   = 0
  950 CONTINUE
C=======================================================================
C     Read input data.
C=======================================================================
      CALL INDAT1
      CALL INDAT2
      CALL INDAT3
C=======================================================================
C     Call TRANSX the driver routine for modified Euler solution.
C=======================================================================
      TIME  = TZERO
      CALL TRANSX
      WRITE(*,1610)
      CALL OUTPUT
      WRITE(*,150)
      WRITE(N6,150)
      RETURN
C=======================================================================
C#### WCH, 11/29/93.
  100 FORMAT(/,
     +' *********************************************************',/,
     +' * Entry made to the EXTENDED TRANSPORT MODEL (EXTRAN)   *',/,
     1' * developed 1973 by Camp, Dresser and McKee (CDM) with  *',/,
     1' * modifications 1977-1991 by the University of Florida. *',/,
     1' *                                                       *',/,
     1' * Most recent update: April 1994 by CDM, Oregon         *',/,
     1' * State University, and XP Software, Inc.               *')
  101 FORMAT(
     1' *********************************************************',/)
  102 FORMAT(
     1' *                                                       *',/,
     1' * "Smooth runs the water where the brook is deep."      *',/,
     1' *                   Shakespeare, Henry VI, II, III, 1   *',/,
     1' *********************************************************',/)
  150 FORMAT(/,' ===> Extended Transport model simulation',
     1         ' ended normally.')
 1610 FORMAT(/,' Calling output and graph subroutines.')
C=======================================================================
      END
