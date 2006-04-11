      LOGICAL FUNCTION BTEST(IW,IBIT)                                   HL
C
      IX=IBITS(IW,IBIT,1)                                               HL
      IF(IX.EQ.0) THEN                                                  HL
      BTEST=.FALSE.                                                     HL
      ELSE                                                              HL
      BTEST=.TRUE.                                                      HL
      ENDIF                                                             HL
      RETURN                                                            HL
C
      END                                                               HL
