C
C       Include file CMPLR.PAR (Compiler and hardware specific
C                               parameters).
C
C------- The following are used in the program supplied functions.
C
      REAL*8 REALMX, REALMN, EXPMN, EXPMX, EXNMX
      INTEGER*4 MAXINT
      LOGICAL PCASCI, WINDOW, NONPC
      INTEGER*2 MAXREC
C
C------- Specify maximum and minimum allowable real numbers.
C
      PARAMETER (REALMX = 1.0D30, REALMN = 1.0D-30)
C
C------- Maximum allowable integer and record length.
C
      PARAMETER (MAXINT = 2147483647)
      PARAMETER (MAXREC = 512)
C
C------- Set limits for exponential function.
C
C     EXP(EXNMX) = 0.0
C     EXP(EXPMN) = 1.0
C     EXP(EXPMX) > REALMX, overflow condition
C
      PARAMETER (EXNMX = -53.0, EXPMN = REALMN, EXPMX = 53.0)
C
C------- Set WINDOW to TRUE if window ouput is desired (it should).
C
      PARAMETER (WINDOW = .TRUE.)
C
C------- Set PCASCI to TRUE if PC-compatible is being used.
C
      PARAMETER (PCASCI = .TRUE.)
C
C------- Set NONPC to TRUE if NON-PC computer is used.  If PC-compatible
C     computer is being used, PCASCI should equal TRUE and NONPC should
C     equal FALSE. If terminal (non-PC) is being used (i.e. PRIME, VAX)
C     both WINDOW and NONPC should equal TRUE for window output
C
      PARAMETER (NONPC = .FALSE.)
C
