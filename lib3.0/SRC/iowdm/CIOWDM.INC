C
C     time-series arrays, sizes, and pointers
      INTEGER   MXVAL
      PARAMETER ( MXVAL=3660 )
C
      COMMON /CIOWDM/ DATES, IPT, IPTN, KTREC, KTERR, INTFG,
     >                TSSTEP, TUNITS, DTOVWR, QFLAG, VAL, VALQ
      INTEGER   DATES(6,3), IPT, IPTN, KTREC, KTERR, INTFG,
     >          TSSTEP, TUNITS, DTOVWR, QFLAG, VALQ(MXVAL)
      REAL      VAL(MXVAL)
