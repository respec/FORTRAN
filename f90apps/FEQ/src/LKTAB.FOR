C
C
C
      SUBROUTINE   LKTAB
     I                  (ADRS, ARGA, SKLT,
     O                   FUNC, NTAB, PDV)
 
C     + + + PURPOSE + + +
C     Do function table look up for all but cross-section tables
C     and two-d tables.
 
C     + + + DUMMY ARGUMENTS + + +
      INTEGER ADRS, NTAB, SKLT
      REAL ARGA, FUNC, PDV
 
C     + + +DUMMY ARGUMENT DEFINITIONS + + +
C     ADRS   - address of the function table in FTAB/ITAB
C     ARGA   - argument for finding values in a function table
C     SKLT   - flag for skipping the lower bound test on table lookup
C     FUNC   - value of function found at ARGA
C     NTAB   - table number
C     PDV    - partial derivative value
 
C     + + + COMMON BLOCKS + + +
      INCLUDE 'arsize.prm'
      INCLUDE 'ftable.com'
      INCLUDE 'xscom.com'
 
C     + + + SAVED VALUES + + +
      INTEGER STEP(5)
      SAVE STEP
 
C     + + + LOCAL VARIABLES + + +
      INTEGER HA, INC, LA, LSTA, TYPE
      REAL A2, A3, ADIFF, AONE, ARG, ARG2, AZERO, DF, FACT, FONE, FPONE,
     A     FPZERO, FZERO, H, P, PH, SHIFT
 
C     + + + INTRINSICS + + +
      INTRINSIC EXP, LOG
 
C     + + + DATA INITIALIZATIONS + + +
      DATA STEP/0,2,3,3,3/
 
C     + + + OUTPUT FORMATS + + +
 50   FORMAT(/,'*BUG:XXX* Invalid TYPE=',I5,' in LKTAB for TABLE#=',I5)
 2000 FORMAT('0','*ERR:70* ARGUMENT BELOW RANGE IN LKTAB',
     A       /,1X,' TABLE NUMBER   = ',I4,
     B       /,1X,' TIME           = ',F14.0,
     C       /,1X,' ARGUMENT       = ',F10.4)
 2010 FORMAT('0','*ERR:71* ARGUMENT ABOVE RANGE IN LKTAB',
     A       /,1X,' TABLE NUMBER   = ',I4,
     B       /,1X,' TIME           = ',F14.0,
     C       /,1X,' ARGUMENT       = ',F10.4)
 2020 FORMAT('0','SUBROUTINE LKTAB RETURNS :',/,1X,
     A           ' NTAB =  ',I5,' ARG =  ',E12.5,' FUNC =  ',E12.5,
     B           ' PDV =  ',E12.5)
C***********************************************************************
C     HA = HIGH ADDRESS
C     LA = LOW ADDRESS
C     ITAB(ADRS+1) = TABLE NUMBER
C     LSTA = LAST ADDRESS
 
      HA = ITAB(ADRS)
      LA = ADRS + 5
      LSTA = ITAB(ADRS+3)
      TYPE = ITAB(ADRS+2)
      NTAB = ITAB(ADRS+1)
      SHIFT = FTAB(ADRS+4)
C      ARG = ARGA + SHIFT
      ARG = ARGA
      IF(TYPE.GT.4) THEN
        INC = 3
      ELSE
        INC = STEP(TYPE)
      ENDIF
 
      IF(TYPE.EQ.5) THEN
        ARG2 = ARG
        ARG = LOG(ARG)
      ENDIF
 
C     INITIALIZE
 
      ADIFF = ARG - FTAB(LSTA)
      IF(ADIFF) 100,300,200
 
C     ARGUMENT PASSED LESS THAN AT PREVIOUS ACCESS TO TABLE
 
 100  IF( LSTA.GE.LA ) GOTO 110
      IF(SKLT.EQ.0) GOTO 105
 
C     SKIP LOWER BOUND TEST
 
      LSTA = LSTA + INC
      ARG = FTAB(LSTA)
      GOTO 300
 
 105  WRITE(STDOUT,2000) ITAB(ADRS+1), TIME, ARG
      EFLAG = 1
      STOP 'Abnormal stop: errors found.'
C      RETURN
 
 110  IF( ARG.GE.FTAB(LSTA) ) GOTO 300
      LSTA = LSTA - INC
      GOTO 100
 
C     ARGUMENT PASSED GREATER THAN AT PREVIOUS ACCESS TO TABLE
 
 200  IF( LSTA.LT.HA ) GOTO 210
      WRITE(STDOUT,2010) ITAB(ADRS+1), TIME, ARG
      EFLAG = 1
      LSTA = HA - INC
      ARG = FTAB(HA)
      GOTO 300
 210  IF( ARG.LE.FTAB(LSTA+INC) ) GOTO 300
      LSTA = LSTA + INC
      GOTO 200
 
 300  CONTINUE
 
      GOTO (1,2,3,4,5,1,1,1,1,1,11), TYPE
 
 1    CONTINUE
        WRITE(STDOUT,50) TYPE, NTAB
        STOP 'Abnormal stop: errors found.'
 
 2    CONTINUE
 
C     SINGLE FUNCTION - LINEAR INTERPOLATION
 
C       FETCH VALUES FROM FTAB
 
        AZERO = FTAB(LSTA)
        FZERO = FTAB(LSTA+1)
        AONE = FTAB(LSTA+2)
        FONE = FTAB(LSTA+3)
 
C       INTERPOLATE
 
        FACT = (ARG - AZERO)/(AONE - AZERO)
        FUNC = FZERO + FACT*(FONE - FZERO)
        PDV  = (FONE - FZERO)/(AONE - AZERO)
        GOTO 1000
 
C     Dual value- trapezoidal rule interpolation
 
 3    CONTINUE
 
C       FETCH VALUES
        AZERO = FTAB(LSTA)
        FZERO = FTAB(LSTA+1)
        FPZERO = FTAB(LSTA+2)
        AONE = FTAB(LSTA+3)
        FONE = FTAB(LSTA+4)
        FPONE = FTAB(LSTA+5)
        PH = ARG - AZERO
        H = AONE - AZERO
        P = PH/H
        PDV = FPZERO + P*(FPONE- FPZERO)
        FUNC = FZERO + 0.5*PH*(FPZERO+ PDV)
        GOTO 1000
 
C     HERMITE CUBIC  INTERPOLATION
 
 4    CONTINUE
        AZERO = FTAB(LSTA)
        FZERO = FTAB(LSTA+1)
        FPZERO = FTAB(LSTA+2)
        AONE = FTAB(LSTA+3)
        FONE = FTAB(LSTA+4)
        FPONE = FTAB(LSTA+5)
        PH = ARG - AZERO
        H = AONE - AZERO
        P = PH/H
        DF = FONE - FZERO
        A2 = 3.*DF - H*(FPZERO + FPZERO + FPONE)
        A3 = H*(FPZERO+FPONE) - 2.*DF
        PDV = FPZERO + P*(A2 + A2 + 3.*A3*P)/H
        FUNC = FZERO + PH*FPZERO + P*P*(A2 + P*A3)
        GOTO 1000
 
 
C     Log-log linear
 
 5    CONTINUE
 
C       Special treatment needed if in the first panel of the
C       breakpoint sequence.  We will extrapolate from the second
C       panel because the zero argument point is included implicitly.
 
        IF(LSTA.EQ.LA) THEN
          LSTA = LA + 3
        ENDIF
        AZERO = FTAB(LSTA)
        FZERO = FTAB(LSTA+1)
        FPZERO = FTAB(LSTA+2)
        PH = ARG - AZERO
        FUNC = EXP(FZERO + PH*FPZERO)
        PDV = FUNC*FPZERO/ARG2
        GOTO 1000
 
C     TIME SERIES WITH TWO VALUES PER TIME POINT
 
 11   CONTINUE
        AZERO = FTAB(LSTA)
        FZERO = FTAB(LSTA+1)
        FPZERO = FTAB(LSTA+2)
        AONE = FTAB(LSTA+3)
        FONE = FTAB(LSTA+4)
        FPONE = FTAB(LSTA+5)
        PH = ARG- AZERO
        H = AONE - AZERO
        P = PH/H
 
        FUNC = FZERO + P*(FONE - FZERO)
        PDV = FPZERO + P*(FPONE - FPZERO)
        GOTO 1000
 
 1000 CONTINUE
 
C     RESET POINTER TO LAST ADDRESS
 
      ITAB(ADRS+3) = LSTA
 
      IF(OUTPUT.GT.4) WRITE(STDOUT,2020) ITAB(ADRS+1), ARG, FUNC, PDV
 
      RETURN
 
      END
C
